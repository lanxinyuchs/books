/*
 * RBS static memory 
 *
 * Copyright 2012 Ericsson AB
 * Andrey Panteleev <andrey.xx.panteleev@ericsson.com>
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

/*
 * TODO list:
 * -- rewrite
 */

#include <linux/module.h>

#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/net.h>
#include <linux/slab.h>
#include <asm/uaccess.h>
#include <linux/skbuff.h>
#include <linux/types.h>
#include <net/sock.h>

#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-fn.h>
#include <linux/rbs/rbs-smem.h>

#define SMEM_RGNNAMESIZE	RBS_SMEM_RGNNAMESIZE

#ifndef CONFIG_RBSSMEM_PERSIST_SIZE
#define CONFIG_RBSSMEM_PERSIST_SIZE 0x100000
#endif

#ifndef CONFIG_RBSSMEM_PERSIST_ADDR
#define CONFIG_RBSSMEM_PERSIST_ADDR 0x20000000
#endif

struct pmem_region {
	struct list_head node;
	phys_addr_t base;
	phys_addr_t size;
};

#define PMEM_MAGIC	0x534d454d

struct pmem_rgn_head {
	unsigned int magic;
	int free;
	char name[SMEM_RGNNAMESIZE];
	phys_addr_t base;
	phys_addr_t size;
};

#define PMEM_NRGN (PAGE_SIZE / sizeof(struct pmem_rgn_head)) 

struct pmem_block {
	struct list_head free;
	struct list_head alloc;

	struct pmem_rgn_head *head;
	phys_addr_t pmem_base;
	phys_addr_t pmem_size;
};

struct smem_region {
	struct list_head node;
	void *base;
	char name[SMEM_RGNNAMESIZE];
	phys_addr_t size;
};

struct smem_block {
	struct list_head alloc;
};

struct rbs_smem {
	struct smem_block smem;
	struct pmem_block pmem;
	struct mutex lock;
};

static struct rbs_smem *rbs_smem = NULL;

static struct pmem_region *pmem_alloc(struct pmem_block *pmem, 
		struct pmem_region *frgn, phys_addr_t base, phys_addr_t size)
{
	struct pmem_region *nrgn;

	/* sanity check */
	if (base < frgn->base || 
	    base + size > frgn->base + frgn->size)
		return NULL;

	nrgn = kzalloc(sizeof(*nrgn), GFP_KERNEL);
	if (!nrgn)
		return NULL;

	/* starts in the middle of the free region? */
	if (frgn->base != base) {
		struct pmem_region *efrgn;

		efrgn = kzalloc(sizeof(*efrgn), GFP_KERNEL);
		if (!efrgn) {
			kfree(nrgn);
			return NULL;
		}

		efrgn->size = base - frgn->base;
		INIT_LIST_HEAD(&efrgn->node);

		frgn->base += efrgn->size;
		frgn->size -= efrgn->size;

		list_add(&efrgn->node, &frgn->node);
	}

	nrgn->base = base;
	nrgn->size = size;
	INIT_LIST_HEAD(&nrgn->node);

	list_add_tail(&nrgn->node, &pmem->alloc);

	frgn->base += size;
	frgn->size -= size;
	if (!frgn->size) {
		list_del(&frgn->node);
		kfree(frgn);
	}

	return nrgn;
}

static struct pmem_region *pmem_alloc_new(struct pmem_block *pmem,
		phys_addr_t size)
{
	struct pmem_region *rgn;
	struct pmem_region *frgn = NULL;

	list_for_each_entry(frgn, &pmem->free, node) {
		if (frgn->size >= size) {
			rgn = frgn;
			break;
		}
	}
	if (!frgn)
		return NULL;

	return pmem_alloc(pmem, frgn, frgn->base, size);
}

static struct pmem_region *pmem_search_free(struct pmem_block *pmem,
		phys_addr_t base)
{
	struct pmem_region *rgn;

	list_for_each_entry(rgn, &pmem->free, node) {
		if (base >= rgn->base && base < rgn->base + rgn->size)
			return rgn;
	}
	
	return NULL;
}

static struct pmem_region *pmem_search_alloc(struct pmem_block *pmem,
		phys_addr_t base)
{
	struct pmem_region *rgn;

	list_for_each_entry(rgn, &pmem->alloc, node) {
		if (base >= rgn->base && base < rgn->base + rgn->size)
			return rgn;
	}
	
	return NULL;
}

#if 0
static void pmem_free(struct pmem_block *pmem, struct pmem_region *rgn)
{
	struct pmem_region *crgn;
	struct pmem_region *nrgn = NULL;
	struct pmem_region *prgn = NULL;
	int merged = 0;

	list_del_init(&rgn->node);

	/* find previous and next free regions */
	list_for_each_entry(crgn, &pmem->free, node) {
		if (crgn->base > rgn->base) {
			nrgn = crgn;
			break;
		}
		prgn = crgn;
	}

	crgn = rgn;

	/* can merge with the previous? */
	if (prgn && (prgn->base + prgn->size == crgn->base)) {
		prgn->size += crgn->size;
		crgn = prgn;
		merged = 1;
	}

	/* can merge with the next? */
	if (nrgn && (crgn->base + crgn->size == nrgn->base)) {
		nrgn->size += crgn->size;
		nrgn->base = crgn->base;
		if (merged) {
			list_del(&crgn->node);
			kfree(crgn);
		}
		merged = 1;
	}

	/* insert if we are unable to merge */
	if (!merged) {
		if (prgn)
			list_add_tail(&rgn->node, &prgn->node);
		else if (nrgn)
			list_add(&rgn->node, &nrgn->node);
		else
			list_add_tail(&rgn->node, &pmem->free);
	} else
		kfree(rgn);
}
#endif

static int rbs_smem_alloc_persist(struct pmem_block *pmem, const char *name,
		phys_addr_t size, phys_addr_t *addr, int flags)
{
	int i, idx = -1, fidx= -1;
	int ret = -ENOMEM;
	struct pmem_region *rgn;

	if (!pmem)
		return -EFAULT;
	if (strlen(name) >= SMEM_RGNNAMESIZE || !size)
		return -EINVAL;
	if (size & ~PAGE_MASK || !addr)
		return -EINVAL;
	/* do we have persistent memory at all? */
	if (!pmem->head)
		return -ENOMEM;

	for (i = 0; i < PMEM_NRGN; i++) {
		if (!pmem->head[i].free && !strcmp(name, pmem->head[i].name) &&
		    size == pmem->head[i].size) {
			idx = i;
			break;
		}

		if (pmem->head[i].free && fidx == -1) {
			fidx = i;
		}
	}
	if (idx == -1 && fidx == -1) 
		return -ENOMEM;

	if (idx != -1) {
		rgn = pmem_search_alloc(pmem, pmem->head[idx].base);
		if (!rgn) {
			pmem->head[idx].free = 1;
			pr_err("allocated region not found\n");
			return -ENOMEM;
		}

		ret = 1; /* recovered */
		*addr = rgn->base;
		goto allocated;
	}

	/* allocate new region */
	rgn = pmem_alloc_new(pmem, size);
	if (!rgn)
		return -ENOMEM;

	strcpy(pmem->head[fidx].name, name);
	pmem->head[fidx].base = rgn->base;
	pmem->head[fidx].size = rgn->size;
	pmem->head[fidx].free = 0;

	if (flags & RBS_SMEM_ZERO)
		memset(((void *) rbs_smem->pmem.head) + 
				(rgn->base - rbs_smem->pmem.pmem_base),
				0, rgn->size);

	ret = 0;
	*addr = rgn->base;

allocated:

	return ret;
}

static int rbs_smem_alloc(struct smem_block *smem, const char *name,
		phys_addr_t size, phys_addr_t *addr, int flags)
{
	int ret = -ENOMEM;
	struct smem_region *rgn;
	struct smem_region *argn = NULL;

	if (!smem)
		return -EFAULT;
	if (strlen(name) >= SMEM_RGNNAMESIZE || !size)
		return -EINVAL;
	if (size & ~PAGE_MASK || !addr)
		return -EINVAL;

	list_for_each_entry(rgn, &smem->alloc, node) {
		if (!strcmp(name, rgn->name) && rgn->size == size) {
			argn = rgn;
			break;
		}
	}
	if (argn) {
		ret = 1;
		*addr = virt_to_phys(argn->base);
		goto allocated;
	}

	argn = kmalloc(sizeof(*argn), GFP_KERNEL);
	if (!argn)
		return -ENOMEM;
	argn->base = kmalloc(size, GFP_KERNEL);
	if (!argn->base) {
		kfree(argn);
		return -ENOMEM;
	}
	INIT_LIST_HEAD(&argn->node);
	strcpy(argn->name, name);
	argn->size = size;

	list_add_tail(&argn->node, &smem->alloc);

	ret = 0;
	if (flags & RBS_SMEM_ZERO)
		memset(argn->base, 0, size);
	*addr = virt_to_phys(argn->base); 

allocated:

	return ret;
}

int rbs_smem_alloc_kernel(const char *name, phys_addr_t size,
		void **addr, int flags)
{
	int ret;
	phys_addr_t base;

	if (!rbs_smem)
		return -EFAULT;
	if (!addr)
		return -EINVAL;

	mutex_lock(&rbs_smem->lock);
	
	if (flags & RBS_SMEM_PERSIST) {
		ret = rbs_smem_alloc_persist(&rbs_smem->pmem, name, size,
				&base, flags);
		if (ret < 0)
			goto out;
		*addr = ((void *) rbs_smem->pmem.head) + 
				(base - rbs_smem->pmem.pmem_base);
		goto out;
	}

	ret = rbs_smem_alloc(&rbs_smem->smem, name, size, &base, flags);
	if (ret < 0)
		goto out;
	*addr = (void *) phys_to_virt(base);

out:
	mutex_unlock(&rbs_smem->lock);
	return ret;		
}
EXPORT_SYMBOL(rbs_smem_alloc_kernel);

static int rbs_smem_persist_recover(struct pmem_block *pmem)
{
	int i;
	struct pmem_rgn_head *head;
	struct pmem_region *rgn;

	for (i = 0; i < PMEM_NRGN; i++) {
		if (pmem->head[i].magic != PMEM_MAGIC)
			return -1;
	}

	for (i = 0; i < PMEM_NRGN; i++) {
		head = &pmem->head[i];
		if (head->free)
			continue;

		if (head->base < pmem->pmem_base ||
		    head->base + head->size > pmem->pmem_base + pmem->pmem_size ||
		    !head->size || (head->size & ~PAGE_MASK)) {
			head->free = 1;
			continue;
		}
		
		rgn = pmem_search_free(pmem, head->base);
		if (!rgn) {
			head->free = 1;
			continue;
		}

		if (!pmem_alloc(pmem, rgn, head->base, head->size)) {
			head->free = 1;
		}
	}

	return 0;
}

static void rbs_smem_persist_clear(struct pmem_block *pmem)
{
	int i;

	for (i = 0; i < PMEM_NRGN; i++) {
		pmem->head[i].magic = PMEM_MAGIC;
		pmem->head[i].free = 1;
	}
}

static void rbs_smem_init_persist(struct pmem_block *pmem)
{
	if (!pmem->head)
		return;

	if (!rbs_smem_persist_recover(pmem))
		return;

	rbs_smem_persist_clear(pmem);
}

static int __init rbs_smem_init(void)
{
	rbs_smem = kzalloc(sizeof(*rbs_smem), GFP_KERNEL);
	if (!rbs_smem) {
		pr_err("%s: unable to allocated memory\n", __func__);
		return -ENOMEM;
	}

	mutex_init(&rbs_smem->lock);
	
	INIT_LIST_HEAD(&rbs_smem->smem.alloc);
	INIT_LIST_HEAD(&rbs_smem->pmem.alloc);
	INIT_LIST_HEAD(&rbs_smem->pmem.free);
	rbs_smem->pmem.head = NULL;
	rbs_smem->pmem.pmem_base = 0;

#ifdef CONFIG_RBSSMEM_HAS_PERSISTENT
	do {
		struct pmem_region *rgn;
		phys_addr_t size = CONFIG_RBSSMEM_PERSIST_SIZE;

		size = (size >> PAGE_SHIFT) << PAGE_SHIFT;
		if (!size)
			break;
		
		rgn = kmalloc(sizeof(*rgn), GFP_KERNEL);
		if (!rgn)
			break;

		rbs_smem->pmem.pmem_base = CONFIG_RBSSMEM_PERSIST_ADDR;
		rbs_smem->pmem.pmem_size = size;

		INIT_LIST_HEAD(&rgn->node);
		rgn->base = rbs_smem->pmem.pmem_base + PAGE_SIZE;
		rgn->size = size - PAGE_SIZE;

		rbs_smem->pmem.head = ioremap_prot(rbs_smem->pmem.pmem_base,
				size, _PAGE_WRITETHRU | _PAGE_COHERENT);
		if (!rbs_smem->pmem.head) {
			kfree(rgn);
			pr_err("%s: unable to remap region\n", __func__);
			break;
		}

		list_add_tail(&rgn->node, &rbs_smem->pmem.free);

	} while (0);
#endif /* CONFIG_RBSSMEM_HAS_PERSISTENT */

	rbs_smem_init_persist(&rbs_smem->pmem);

	return 0;
}
core_initcall(rbs_smem_init);

static int rbs_smem_sock_ioctl_alloc(void __user *arg)
{
	struct rbs_smem_alloc cmd;
	int ret;

	if (copy_from_user(&cmd, arg, sizeof(cmd)))
		return -EFAULT;
	if (cmd.size & ~PAGE_MASK)
		return -EINVAL;
	if (!rbs_smem)
		return -EINVAL;

	mutex_lock(&rbs_smem->lock);
	if (cmd.flags & RBS_SMEM_PERSIST)
		ret = rbs_smem_alloc_persist(&rbs_smem->pmem, cmd.name,
				cmd.size, (phys_addr_t *) &cmd.base, 
				cmd.flags);
	else
		ret = rbs_smem_alloc(&rbs_smem->smem, cmd.name, cmd.size,
				(phys_addr_t *) &cmd.base, cmd.flags);
	mutex_unlock(&rbs_smem->lock);

	if (ret < 0)
		return ret;

	cmd.recovered = ret;

	if (copy_to_user(arg, &cmd, sizeof(cmd))) 
		return -EFAULT;

	return 0;
}

static int rbs_smem_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
		unsigned long arg)
{
	int ret = 0;

	switch (cmd) {
	case RBS_SMEMIOC_ALLOC:
		ret = rbs_smem_sock_ioctl_alloc((void __user *) arg);
		break;
	default:
		ret = -ENOSYS;
		break;
	}

	return ret;
}

static int rbs_smem_sock_mmap(struct rbs_sock *rsk,
		struct vm_area_struct *vma)
{
        long length = vma->vm_end - vma->vm_start;

	return remap_pfn_range(vma, vma->vm_start,
			vma->vm_pgoff, length,
			vma->vm_page_prot);
}

static struct rbs_proto_ops rbs_smem_proto_ops = {
	.ioctl = rbs_smem_sock_ioctl,
	.mmap = rbs_smem_sock_mmap,
};

static ssize_t rbs_smem_sysfs_list(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr,
		char *buf)
{
	int i;
	int offset = 0;
	struct pmem_block *pmem;
	struct smem_region *rgn;

	if (!rbs_smem)
		return -EFAULT;

	pmem = &rbs_smem->pmem;
	
	if (pmem->head) {
		mutex_lock(&rbs_smem->lock);
		for (i = 0; (i < PMEM_NRGN); i++) {
			if (pmem->head[i].free)
				continue;
			offset += sprintf(buf + offset,
					"* 0x%.8x-0x%.8x %8u %s\n",
					(unsigned int) pmem->head[i].base,
					(unsigned int) (pmem->head[i].base + 
						pmem->head[i].size - 1),
					(unsigned int) pmem->head[i].size,
					pmem->head[i].name);
		}
		mutex_unlock(&rbs_smem->lock);
	}

	mutex_lock(&rbs_smem->lock);
	list_for_each_entry(rgn, &rbs_smem->smem.alloc, node) {
		offset += sprintf(buf + offset,
				"  0x%.8x-0x%.8x %8u %s\n",
				(unsigned int) virt_to_phys(rgn->base),
				(unsigned int) (virt_to_phys(rgn->base) +
					rgn->size - 1),
				(unsigned int) rgn->size,
				rgn->name);
	}
	mutex_unlock(&rbs_smem->lock);

	return offset;
}

static struct rbs_fn_attribute rbs_smem_attrs[] = {
	_RBS_FN_ATTR(list, 0444, rbs_smem_sysfs_list, NULL),
	_RBS_FN_ATTR_NULL,
};

static struct rbs_fn rbs_smem_fn = {
	.name = "rbs-smem",
	.owner = THIS_MODULE,
	.attrs = rbs_smem_attrs,
};

static int __init rbs_smem_lateinit(void)
{
	int ret;

	ret = rbs_proto_register(RBS_PROTO_SMEM, &rbs_smem_proto_ops,
			rbs_smem);
	if (ret) {
		pr_err("%s: unable to register protocol\n", __func__);
		return ret;
	}

	return rbs_fn_add(&rbs_smem_fn);
}
late_initcall(rbs_smem_lateinit);


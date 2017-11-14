/*
 * CPM1 - Support functions
 *
 * Copyright 2013 Ericsson AB
 *
 * This program is free software; you can redistribute	it and/or modify it
 * under  the terms of	the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
  *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <linux/vmalloc.h>
#include <linux/wait.h>
#include <linux/interrupt.h>
#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-fn.h>
#include <linux/semaphore.h>
#include <linux/irqdomain.h>
#include <linux/of.h>
#include <linux/of_irq.h>
#include <asm/cacheflush.h>
#include <asm/page.h>



struct io_area {
	unsigned long	start;
	unsigned long	end;
};

struct reg_xfer {
	unsigned long	instr;
	unsigned long	value;
};

struct mdc_xfer {
	unsigned long	phy;
	unsigned long	reg;
	unsigned short	val;
	unsigned short	c45;
};

#define RBS_PCIE_SUP_WAITINT	 _IOW('j', 0, int)
#define RBS_PCIE_SUP_REGXFER	 _IOWR('j', 1, struct reg_xfer)
#define RBS_MDIO_READ			 _IOWR('j', 2, struct mdc_xfer)
#define RBS_MDIO_WRITE			 _IOWR('j', 3, struct mdc_xfer)

#define ALLOWED_RANGE(start_page,end_page)			  \
	{(unsigned long)start_page, (unsigned long)end_page},


static void sup_vma_open(struct vm_area_struct *vma);
static void sup_vma_close(struct vm_area_struct *vma);
static int sup_vma_fault(struct vm_area_struct *vma, struct vm_fault *vmf);
static int pcie_sup_sock_mmap(struct rbs_sock *rsk, struct vm_area_struct *vma);
static int pcie_sup_sock_ioctl(struct rbs_sock *, unsigned int, unsigned long);
static int reg_xfer(void __user *arg);


static struct io_area  allowed_pages[] = {
	ALLOWED_RANGE(0x00000000, 0x00040000)
	ALLOWED_RANGE(0x02000000, 0x02001008)
	ALLOWED_RANGE(0x02010000, 0x02010089)
	ALLOWED_RANGE(0x02010090, 0x02010095)
	ALLOWED_RANGE(0x02010096, 0x020100A2)
	ALLOWED_RANGE(0x02010100, 0x02010168)
	ALLOWED_RANGE(0x02010180, 0x020101C0)
	ALLOWED_RANGE(0x02012100, 0x02012200)
	ALLOWED_RANGE(0x02013000, 0x02014000)
	ALLOWED_RANGE(0x02020000, 0x02020050)
	ALLOWED_RANGE(0x02020100, 0x02020128)
	ALLOWED_RANGE(0x02020130, 0x02020138)
	ALLOWED_RANGE(0x02020140, 0x02020147)
	ALLOWED_RANGE(0x020FFFF0, 0x02100000)
	ALLOWED_RANGE(0x03000000, 0x03040000)
	ALLOWED_RANGE(0x03080000, 0x030C0000)
	ALLOWED_RANGE(0x03100000, 0x03180000)
	ALLOWED_RANGE(0, 0)
};


static struct rbs_fn sup_fn = {
	.name = "map",
	.owner = THIS_MODULE,
};

static struct rbs_proto_ops pcie_sup_proto_ops = {
	.mmap = pcie_sup_sock_mmap,
	.ioctl = pcie_sup_sock_ioctl,
};

static const struct vm_operations_struct sup_vm_ops = {
	.open  = sup_vma_open,
	.close = sup_vma_close,
	.fault = sup_vma_fault,
};


static int check_range(unsigned long page, unsigned long size)
{
	struct io_area *p;
	unsigned long aligned_size = (size + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
	unsigned long num_pages = aligned_size >> PAGE_SHIFT;

	for (p = allowed_pages; p->end; p++)
		if ((page >= p->start) && ((page + num_pages) <= p->end))
			return 0;

	return -1;
}

static void sup_vma_open(struct vm_area_struct *vma)
{
	if (!try_module_get(THIS_MODULE))
		pr_err("PCIe_sup: try_module_get() failed\n");
}

static void sup_vma_close(struct vm_area_struct *vma)
{
	module_put(THIS_MODULE);
}

static int sup_vma_fault(struct vm_area_struct *vma, struct vm_fault *vmf)
{
	pr_err("sup_vma_fault: %d\n",78);
	return VM_FAULT_SIGBUS;
}

static int pcie_sup_sock_mmap(struct rbs_sock *rsk, struct vm_area_struct *vma)
{
	unsigned long size = vma->vm_end - vma->vm_start;
	int ret;

	(void)rsk;

	if (check_range(vma->vm_pgoff, size) < 0)
		return -EINVAL;

	vma->vm_flags |= VM_IO | VM_DONTDUMP | VM_DONTCOPY;
	vma->vm_page_prot = pgprot_noncached(vma->vm_page_prot);
	vma->vm_ops = &sup_vm_ops;

	ret = remap_pfn_range(vma, vma->vm_start, vma->vm_pgoff, size,
							vma->vm_page_prot);
	if (ret == 0)
		sup_vma_open(vma);
	else
		pr_err("remap_pfn_range failed: %d\n",ret);

	return ret;
}

static int reg_xfer(void __user *arg)
{
	struct reg_xfer xfr;
	unsigned int *p;

	if (copy_from_user(&xfr, arg, sizeof(xfr)))
		return -EFAULT;

	p = (unsigned int*)__vmalloc(32, GFP_KERNEL, PAGE_KERNEL_EXEC);
	if (p == NULL)
		return -ENOMEM;

	p[0] = xfr.instr;
	p[1] = 0xe12fff1e;

	flush_icache_range((unsigned long)p,(unsigned long)p + 8);
	clean_dcache_area((void*)p,8);
	xfr.value = ((unsigned int (*)(unsigned int))p)(xfr.value);
	vfree(p);

	return copy_to_user(arg, &xfr, sizeof(xfr));
}

extern int
acp_mdio_write(unsigned long address, unsigned long offset,
	       unsigned short value, int clause45);
static int mdc_write(void __user *arg)
{
	struct mdc_xfer xfr;
	if (copy_from_user(&xfr, arg, sizeof(xfr)))
		return -EFAULT;

	return acp_mdio_write(xfr.phy, xfr.reg, xfr.val, (int)xfr.c45);
}

extern int
acp_mdio_read(	unsigned long address, unsigned long offset,
				unsigned short *value, int clause45);
static int mdc_read(void __user *arg)
{
	struct mdc_xfer xfr;
	int ret;

	if (copy_from_user(&xfr, arg, sizeof(xfr)))
		return -EFAULT;

	ret = acp_mdio_read(xfr.phy, xfr.reg, &xfr.val, (int)xfr.c45);
	if (ret == 0)
		ret = copy_to_user(arg, &xfr, sizeof(xfr));

	return ret;
}

static int pcie_sup_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
								unsigned long arg)
{
	unsigned long tmo = 0;
	int ret;

	switch (cmd) {
	case RBS_PCIE_SUP_REGXFER:
		return reg_xfer((void __user *)arg);

	case RBS_MDIO_READ:
		return mdc_read((void __user *)arg);

	case RBS_MDIO_WRITE:
		return mdc_write((void __user *)arg);

	default:
		return -EOPNOTSUPP;
	}

}

int __init pcie_sup_init(void)
{
	int ret;

	ret = rbs_proto_register(RBS_PROTO_IOMAP, &pcie_sup_proto_ops, NULL);
	if (ret) {
		pr_err("%s: unable to register RBS_PROTO_IOMAP, %d\n",__func__, ret);
		return -1;
	}

	ret = rbs_fn_add(&sup_fn);
	if (ret) {
		pr_err("%s: unable to register SUP_FN, %d\n", __func__, ret);
		goto fail;
	}

	return 0;

fail:

	rbs_proto_unregister(RBS_PROTO_IOMAP, &pcie_sup_proto_ops);
	return ret;
}

void __exit pcie_sup_exit(void)
{
	rbs_fn_remove(&sup_fn);
	rbs_proto_unregister(RBS_PROTO_IOMAP, &pcie_sup_proto_ops);
}

module_init(pcie_sup_init);
module_exit(pcie_sup_exit);

MODULE_LICENSE("GPL");


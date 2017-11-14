/*
 * CPM2 - Support functions
 *
 * Copyright 2016 Ericsson AB
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
#include <linux/string.h>
#include <linux/of.h>
#include <linux/of_irq.h>
#include <linux/of_mdio.h>
#include <asm/cacheflush.h>
#include <asm/page.h>

#define ACCESS_NOK		0
#define	ACCESS_OK		1
#define NO_IRQ			-1

#define PCIE_START		0
#define PCIE_END		2

#define eq(_s1, _s2)		(strcmp(_s1, _s2) == 0)
#define dummy_dev(_db)		(_db - &dtb_ranges[PCIE_START] + 1)

#define for_each_page_range(_p) \
	for (_p = allowed_pages; p->end; p++)

#define for_each_dtb_range(_p) \
	for (_p = dtb_ranges; p->ap_ix >= 0; p++)

struct mdc_xfer {
	unsigned int	phy;
	unsigned int	reg;
	unsigned short	val;
	unsigned short	c45;
};

#define RBS_PCIE_SUP_WAITINT	 _IOW('j', 0, int)
#define RBS_MDIO_READ		 _IOWR('j', 2, struct mdc_xfer)
#define RBS_MDIO_WRITE		 _IOWR('j', 3, struct mdc_xfer)

#define ALLOWED_RANGE(start_page, end_page, ok)	\
	{(uint32_t) start_page,			\
	 (uint32_t) end_page,			\
	 (uint32_t) ok},

#define DTB_ENTRY(ap_ix, path)	\
	{(int32_t) ap_ix,		\
	 path},

struct dtb_entry {
	int32_t		ap_ix;
	const char	path[32];
};

struct io_area {
	uint32_t	start;
	uint32_t	end;
	uint32_t	ok;
};

static struct dtb_entry dtb_ranges[] = {
	DTB_ENTRY(81, "/soc/pcie@c000000000")
	DTB_ENTRY(82, "/soc/pcie@c800000000")
	DTB_ENTRY(83, "/soc/pcie@d000000000")
	DTB_ENTRY(28, "/soc/gpdma@8004120000")
	DTB_ENTRY(29, "/soc/gpdma@8004130000")
	DTB_ENTRY(36, "/soc/trng@8031060000")
	DTB_ENTRY(42, "/soc/amba/uart@8080000000")
	DTB_ENTRY(43, "/soc/amba/uart@8080010000")
	DTB_ENTRY(44, "/soc/amba/uart@8080020000")
	DTB_ENTRY(45, "/soc/amba/uart@8080030000")
	DTB_ENTRY(46, "/soc/i2c@8080080000")
	DTB_ENTRY(47, "/soc/i2c@8080090000")
	DTB_ENTRY(48, "/soc/i2c@80800a0000")
	DTB_ENTRY(49, "/soc/i2c@80800b0000")
	DTB_ENTRY(50, "/soc/amba/ssp@8080100000")
	DTB_ENTRY(51, "/soc/amba/gpio@8080180000")
	DTB_ENTRY(52, "/soc/amba/gpio@8080190000")
	DTB_ENTRY(53, "/soc/mdio@8080200000")
	DTB_ENTRY(54, "/soc/mtc@8080210000")
	DTB_ENTRY(55, "/soc/amba/timer@8080220000")
	DTB_ENTRY(57, "/soc/ethernet@8080240000")
	DTB_ENTRY(61, "/soc/usb@9000000000")
	DTB_ENTRY(65, "/soc/sata@a000000000")
	DTB_ENTRY(66, "/soc/sata@a000010000")
	DTB_ENTRY(-1, "")
};

static struct io_area allowed_pages[] = {
	ALLOWED_RANGE(0x00000000, 0x00040000, ACCESS_OK)
	ALLOWED_RANGE(0x08000000, 0x08001000, ACCESS_OK)
	ALLOWED_RANGE(0x08001000, 0x08001400, ACCESS_OK)
	ALLOWED_RANGE(0x08002000, 0x08002400, ACCESS_OK)
	ALLOWED_RANGE(0x08002400, 0x08002800, ACCESS_OK)
	ALLOWED_RANGE(0x08002800, 0x08002c00, ACCESS_OK)
	ALLOWED_RANGE(0x08002c00, 0x08003000, ACCESS_OK)
	ALLOWED_RANGE(0x08003000, 0x08003400, ACCESS_OK)
	ALLOWED_RANGE(0x08003400, 0x08003800, ACCESS_OK)
	ALLOWED_RANGE(0x08003800, 0x08003c00, ACCESS_OK)
	ALLOWED_RANGE(0x08003c00, 0x08004000, ACCESS_OK)
	ALLOWED_RANGE(0x08004000, 0x08004010, ACCESS_OK)
	ALLOWED_RANGE(0x08004010, 0x08004020, ACCESS_OK)
	ALLOWED_RANGE(0x08004020, 0x08004030, ACCESS_OK)
	ALLOWED_RANGE(0x08004030, 0x08004040, ACCESS_OK)
	ALLOWED_RANGE(0x08004040, 0x08004050, ACCESS_OK)
	ALLOWED_RANGE(0x08004050, 0x08004060, ACCESS_OK)
	ALLOWED_RANGE(0x08004060, 0x08004070, ACCESS_OK)
	ALLOWED_RANGE(0x08004070, 0x08004080, ACCESS_OK)
	ALLOWED_RANGE(0x08004080, 0x08004090, ACCESS_OK)
	ALLOWED_RANGE(0x08004090, 0x080040a0, ACCESS_OK)
	ALLOWED_RANGE(0x080040a0, 0x080040b0, ACCESS_OK)
	ALLOWED_RANGE(0x080040b0, 0x080040c0, ACCESS_OK)
	ALLOWED_RANGE(0x080040c0, 0x080040d0, ACCESS_OK)
	ALLOWED_RANGE(0x080040d0, 0x080040e0, ACCESS_OK)
	ALLOWED_RANGE(0x080040e0, 0x080040f0, ACCESS_OK)
	ALLOWED_RANGE(0x080040f0, 0x08004100, ACCESS_OK)
	ALLOWED_RANGE(0x08004100, 0x08004110, ACCESS_OK)
	ALLOWED_RANGE(0x08004120, 0x08004130, ACCESS_NOK)
	ALLOWED_RANGE(0x08004130, 0x08004140, ACCESS_NOK)
	ALLOWED_RANGE(0x08004140, 0x08004140, ACCESS_OK)
	ALLOWED_RANGE(0x08008000, 0x08010000, ACCESS_OK)
	ALLOWED_RANGE(0x08010000, 0x08014000, ACCESS_OK)
	ALLOWED_RANGE(0x08030000, 0x08031000, ACCESS_OK)
	ALLOWED_RANGE(0x08031000, 0x08031040, ACCESS_OK)
	ALLOWED_RANGE(0x08031040, 0x08031060, ACCESS_OK)
	ALLOWED_RANGE(0x08031060, 0x08031080, ACCESS_NOK)
	ALLOWED_RANGE(0x08031080, 0x080310a0, ACCESS_OK)
	ALLOWED_RANGE(0x08032000, 0x08032800, ACCESS_OK)
	ALLOWED_RANGE(0x08032800, 0x08032900, ACCESS_OK)
	ALLOWED_RANGE(0x08032900, 0x08032910, ACCESS_OK)
	ALLOWED_RANGE(0x08032910, 0x08032920, ACCESS_OK)
	/* Peripheral I/O range starts below */
	ALLOWED_RANGE(0x08080000, 0x08080010, ACCESS_NOK)
	ALLOWED_RANGE(0x08080010, 0x08080020, ACCESS_NOK)
	ALLOWED_RANGE(0x08080020, 0x08080030, ACCESS_NOK)
	ALLOWED_RANGE(0x08080030, 0x08080040, ACCESS_NOK)
	ALLOWED_RANGE(0x08080080, 0x08080090, ACCESS_NOK)
	ALLOWED_RANGE(0x08080090, 0x080800a0, ACCESS_NOK)
	ALLOWED_RANGE(0x080800a0, 0x080800b0, ACCESS_NOK)
	ALLOWED_RANGE(0x080800b0, 0x080800c0, ACCESS_NOK)
	ALLOWED_RANGE(0x08080100, 0x08080110, ACCESS_NOK)
	ALLOWED_RANGE(0x08080180, 0x08080190, ACCESS_NOK)
	ALLOWED_RANGE(0x08080190, 0x080801a0, ACCESS_NOK)
	ALLOWED_RANGE(0x08080200, 0x08080210, ACCESS_NOK)
	ALLOWED_RANGE(0x08080210, 0x08080220, ACCESS_NOK)
	ALLOWED_RANGE(0x08080220, 0x08080230, ACCESS_NOK)
	ALLOWED_RANGE(0x08080230, 0x08080240, ACCESS_OK)
	ALLOWED_RANGE(0x08080240, 0x08080250, ACCESS_NOK)
	ALLOWED_RANGE(0x08080250, 0x08080260, ACCESS_OK)
	ALLOWED_RANGE(0x08080260, 0x08080280, ACCESS_OK)
	ALLOWED_RANGE(0x08080400, 0x08080600, ACCESS_OK)
	/* USB I/O range starts below */
	ALLOWED_RANGE(0x09000000, 0x09000100, ACCESS_NOK)
	ALLOWED_RANGE(0x09000200, 0x09000210, ACCESS_OK)
	ALLOWED_RANGE(0x09000300, 0x09000400, ACCESS_OK)
	ALLOWED_RANGE(0x09000400, 0x09000600, ACCESS_OK)
	/* PCI I/O range starts below */
	ALLOWED_RANGE(0x0a000000, 0x0a000010, ACCESS_NOK)
	ALLOWED_RANGE(0x0a000010, 0x0a000020, ACCESS_NOK)
	ALLOWED_RANGE(0x0a000020, 0x0a000030, ACCESS_OK)
	ALLOWED_RANGE(0x0a000030, 0x0a000040, ACCESS_OK)
	ALLOWED_RANGE(0x0a000100, 0x0a000110, ACCESS_OK)
	ALLOWED_RANGE(0x0a000110, 0x0a000120, ACCESS_OK)
	ALLOWED_RANGE(0x0a000300, 0x0a000400, ACCESS_OK)
	ALLOWED_RANGE(0x0a000400, 0x0a000600, ACCESS_OK)
	ALLOWED_RANGE(0x0a002000, 0x0a003000, ACCESS_OK)
	ALLOWED_RANGE(0x0a003000, 0x0a003010, ACCESS_OK)
	ALLOWED_RANGE(0x0a004000, 0x0a005000, ACCESS_OK)
	ALLOWED_RANGE(0x0a005000, 0x0a005010, ACCESS_OK)
	ALLOWED_RANGE(0x0a006000, 0x0a007000, ACCESS_OK)
	ALLOWED_RANGE(0x0a007000, 0x0a007010, ACCESS_OK)
	ALLOWED_RANGE(0x0b000000, 0x0b040000, ACCESS_NOK)
	ALLOWED_RANGE(0x0b800000, 0x0b840000, ACCESS_NOK)
	ALLOWED_RANGE(0x0c000000, 0x0c400000, ACCESS_NOK)
	ALLOWED_RANGE(0x0c800000, 0x0cc00000, ACCESS_NOK)
	ALLOWED_RANGE(0x0d000000, 0x0d400000, ACCESS_NOK)
	ALLOWED_RANGE(0, 0, 0)
};

static struct mii_bus *axm_mdio_get_bus(void)
{
	struct device_node *np;
	static struct mii_bus *mdio_bus = NULL;

	if (mdio_bus == NULL) {
		np = of_find_compatible_node(NULL, NULL, "lsi,axm-mdio");
		if (np)
			mdio_bus = of_mdio_find_bus(np);
	}
	return mdio_bus;
}

static int axm_mdio_write(unsigned int address, unsigned int offset,
		           unsigned short value, int clause45)
{
	struct mii_bus *bus = axm_mdio_get_bus();

	if (!bus)
		return -ENODEV;

	if (clause45)
		offset |= MII_ADDR_C45;
	return bus->write(bus, address, offset, value);
}

static int axm_mdio_read(unsigned int address, unsigned int offset,
                          unsigned short *value, int clause45)
{
	struct mii_bus *bus = axm_mdio_get_bus();

	if (!bus)
		return -ENODEV;

	if (clause45)
		offset |= MII_ADDR_C45;

	*value = bus->read(bus, address, offset);
	return 0;
}

static int mdc_write(void __user *arg)
{
	struct mdc_xfer xfr;
	if (copy_from_user(&xfr, arg, sizeof(xfr)))
		return -EFAULT;

	return axm_mdio_write(xfr.phy, xfr.reg, xfr.val, (int)xfr.c45);
}

static int mdc_read(void __user *arg)
{
	struct mdc_xfer xfr;
	int ret;

	if (copy_from_user(&xfr, arg, sizeof(xfr)))
		return -EFAULT;

	ret = axm_mdio_read(xfr.phy, xfr.reg, &xfr.val, (int)xfr.c45);
	if (ret == 0)
		ret = copy_to_user(arg, &xfr, sizeof(xfr));

	return ret;
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
	pr_err("sup_vma_fault: %d\n", 78);
	return VM_FAULT_SIGBUS;
}


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

	for_each_page_range(p)
		if ((p->ok == ACCESS_OK) &&
		    ((page >= p->start) && ((page + num_pages) <= p->end)))
			return 0;

	return -1;
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

static int pcie_sup_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
			       unsigned long arg)
{

	switch (cmd) {

	case RBS_MDIO_READ:
		return mdc_read((void __user *)arg);

	case RBS_MDIO_WRITE:
		return mdc_write((void __user *)arg);

	default:
		return -EOPNOTSUPP;
	}

}

static void __init set_state_from_dtb(struct dtb_entry *p)
{
	struct device_node *np;
	const char *status;
	struct io_area *ap = &allowed_pages[p->ap_ix];

	np = of_find_node_by_path(p->path);
	if (np) {
		if (of_property_read_string(np, "status", &status) == 0)
			ap->ok = eq(status, "okay");
		of_node_put(np);
	}
}

static void __init adjust_allowed_ranges(void)
{
	struct dtb_entry *p;

	for_each_dtb_range(p)
		set_state_from_dtb(p);
}

static struct rbs_fn sup_fn = {
	.name = "map",
	.owner = THIS_MODULE,
};

static struct rbs_proto_ops pcie_sup_proto_ops = {
	.mmap = pcie_sup_sock_mmap,
	.ioctl = pcie_sup_sock_ioctl,
};

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

	adjust_allowed_ranges();

	if (axm_mdio_get_bus() == NULL)
		pr_info("%s: could not obtain reference to LMT PHY mdio\n",
			__func__);
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


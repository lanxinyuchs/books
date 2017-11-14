/*
 * ICM1 - ICM1 Driver
 *
 * Copyright 2013 Ericsson AB
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

/**
 * TODO:
 * -- logging
 */

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/io.h>
#include <linux/pci.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/platform_device.h>
#include <linux/of_platform.h>
#include <linux/slab.h>
#include <linux/gpio.h>
#include <linux/file.h>
#include <linux/vmalloc.h>
#include <linux/fs.h>
#include <linux/spi/spi.h>
#include <linux/amba/pl022.h>
#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-fn.h>
#include <linux/rbs/rbs-icmctrl.h>

#ifdef CONFIG_AXXIA_PEI
#include <linux/axxia-pei.h>
#endif

#include <asm/io.h>
#include <asm/irq.h>
#include <asm/uaccess.h>

#include "icm1.h"

/* According to 155 18-CAH 109 2241/1 Uen */
#define PCI_COBRA_VENDOR	0x1a25
#define PCI_COBRA_DEVICE	0x0001
#define PCI_TAIPAN_VENDOR	0x1a25
#define PCI_TAIPAN_DEVICE	0x0001

/* No change from above according to IWD delta for Katla, 155 18-30/CAH 109 2496/1 Uen */
#define PCI_KATLA_VENDOR	0x1a25
#define PCI_KATLA_DEVICE	0x0001

#define ICM1_PCI_DOMAIN		0
#define ICM1_PCI_BUSNR		0

#define ICM1_SPI_BUSNUM		0

#define ICM1_GPIO_BASE		16

#define ICM1_PIN_CONFIG_N	(ICM1_GPIO_BASE + 0)
#define ICM1_PIN_CONF_DONE	(ICM1_GPIO_BASE + 1)
#define ICM1_PIN_STATUS_N	(ICM1_GPIO_BASE + 2)
#define ICM1_PIN_CVP_CONFDONE	(ICM1_GPIO_BASE + 5)
#define ICM1_PIN_INIT_DONE	(ICM1_GPIO_BASE + 6)
#define ICM1_PIN_PCIE_RESET_N	(ICM1_GPIO_BASE + 3)
#define ICM1_PIN_LOG_RESET_N	(ICM1_GPIO_BASE + 4)

#define ICM1_PS_TRFBUFSIZ	(1024 * 1024) /* also max. image size */
#define ICM1_CVP_TRFBUFSIZ	(1024 * 1024)

struct icm1 {
	struct pci_dev *pdev;
	void __iomem *cvp_regs;

	int state;
	int configuring;
	struct spi_device *sdev;
	struct spi_master *smaster;

	struct mutex lock;
	atomic_t map_ref;
};

struct icm1_sock {
	int r_config;
	struct icm1 *icm;
};

static struct icm1 *icm1;

static const unsigned char bit_rev_tbl256[] = 
{
	0x00, 0x80, 0x40, 0xC0, 0x20, 0xA0, 0x60, 0xE0,
	0x10, 0x90, 0x50, 0xD0, 0x30, 0xB0, 0x70, 0xF0,
	0x08, 0x88, 0x48, 0xC8, 0x28, 0xA8, 0x68, 0xE8,
	0x18, 0x98, 0x58, 0xD8, 0x38, 0xB8, 0x78, 0xF8,
	0x04, 0x84, 0x44, 0xC4, 0x24, 0xA4, 0x64, 0xE4,
	0x14, 0x94, 0x54, 0xD4, 0x34, 0xB4, 0x74, 0xF4,
	0x0C, 0x8C, 0x4C, 0xCC, 0x2C, 0xAC, 0x6C, 0xEC,
	0x1C, 0x9C, 0x5C, 0xDC, 0x3C, 0xBC, 0x7C, 0xFC,
	0x02, 0x82, 0x42, 0xC2, 0x22, 0xA2, 0x62, 0xE2,
	0x12, 0x92, 0x52, 0xD2, 0x32, 0xB2, 0x72, 0xF2,
	0x0A, 0x8A, 0x4A, 0xCA, 0x2A, 0xAA, 0x6A, 0xEA,
	0x1A, 0x9A, 0x5A, 0xDA, 0x3A, 0xBA, 0x7A, 0xFA,
	0x06, 0x86, 0x46, 0xC6, 0x26, 0xA6, 0x66, 0xE6,
	0x16, 0x96, 0x56, 0xD6, 0x36, 0xB6, 0x76, 0xF6,
	0x0E, 0x8E, 0x4E, 0xCE, 0x2E, 0xAE, 0x6E, 0xEE,
	0x1E, 0x9E, 0x5E, 0xDE, 0x3E, 0xBE, 0x7E, 0xFE,
	0x01, 0x81, 0x41, 0xC1, 0x21, 0xA1, 0x61, 0xE1,
	0x11, 0x91, 0x51, 0xD1, 0x31, 0xB1, 0x71, 0xF1,
	0x09, 0x89, 0x49, 0xC9, 0x29, 0xA9, 0x69, 0xE9,
	0x19, 0x99, 0x59, 0xD9, 0x39, 0xB9, 0x79, 0xF9,
	0x05, 0x85, 0x45, 0xC5, 0x25, 0xA5, 0x65, 0xE5,
	0x15, 0x95, 0x55, 0xD5, 0x35, 0xB5, 0x75, 0xF5,
	0x0D, 0x8D, 0x4D, 0xCD, 0x2D, 0xAD, 0x6D, 0xED,
	0x1D, 0x9D, 0x5D, 0xDD, 0x3D, 0xBD, 0x7D, 0xFD,
	0x03, 0x83, 0x43, 0xC3, 0x23, 0xA3, 0x63, 0xE3,
	0x13, 0x93, 0x53, 0xD3, 0x33, 0xB3, 0x73, 0xF3,
	0x0B, 0x8B, 0x4B, 0xCB, 0x2B, 0xAB, 0x6B, 0xEB,
	0x1B, 0x9B, 0x5B, 0xDB, 0x3B, 0xBB, 0x7B, 0xFB,
	0x07, 0x87, 0x47, 0xC7, 0x27, 0xA7, 0x67, 0xE7,
	0x17, 0x97, 0x57, 0xD7, 0x37, 0xB7, 0x77, 0xF7,
	0x0F, 0x8F, 0x4F, 0xCF, 0x2F, 0xAF, 0x6F, 0xEF,
	0x1F, 0x9F, 0x5F, 0xDF, 0x3F, 0xBF, 0x7F, 0xFF
};

static int icm1_wait_pin(int pin, int lev, int tmo)
{
	int val;
	unsigned long end;

	end = jiffies + msecs_to_jiffies(tmo);

	do {
		val = gpio_get_value_cansleep(pin);
		if (val == lev)
			return 0;
	} while (time_before(jiffies, end));

	return -1;
}

static void icm1_get_state_pins(struct icm1 *icm1)
{
	int val;

	if (!(icm1->state & RBS_ICMCTRL_CTRLPINS))
		return;

	val = gpio_get_value_cansleep(ICM1_PIN_CONF_DONE);
	if (!val)
		icm1->state &= ~(RBS_ICMCTRL_PERIPHERAL|RBS_ICMCTRL_FABRIC);
	else
		icm1->state |= RBS_ICMCTRL_PERIPHERAL;

	val = gpio_get_value_cansleep(ICM1_PIN_CVP_CONFDONE);
	if (!val)
		icm1->state &= ~RBS_ICMCTRL_FABRIC;
	else
		icm1->state |= RBS_ICMCTRL_FABRIC;

	val = gpio_get_value_cansleep(ICM1_PIN_INIT_DONE);
	if (!val)
		icm1->state &= ~RBS_ICMCTRL_USER;
	else
		icm1->state |= RBS_ICMCTRL_USER;
}

static int icm1_pci_stop(struct icm1 *icm1)
{
	if (icm1->state & (RBS_ICMCTRL_PCIPRFAIL | RBS_ICMCTRL_PCIREADY)) {
		if (atomic_read(&icm1->map_ref))
			return -EBUSY;

		if (icm1->pdev)
			pci_stop_and_remove_bus_device(icm1->pdev);
		icm1->pdev = NULL;
		icm1->state &= ~RBS_ICMCTRL_PCIPRFAIL;
	}

	return 0;
}

static int icm1_pci_rescan(struct icm1 *icm1)
{
	struct pci_bus *bus;

	mutex_lock(&icm1->lock);
	if (icm1->configuring) {
		mutex_unlock(&icm1->lock);
		return -EBUSY;
	}

	if (icm1_pci_stop(icm1)) {
		mutex_unlock(&icm1->lock);
		return -EBUSY;
	}

#ifdef CONFIG_AXXIA_PEI
	axxia_pcie_reset();
#endif

	bus = pci_find_bus(ICM1_PCI_DOMAIN, ICM1_PCI_BUSNR);
	if (!bus) {
		pr_err("ICM1: PCI bus (%u %u) not found\n", ICM1_PCI_DOMAIN,
				ICM1_PCI_BUSNR);
		mutex_unlock(&icm1->lock);
		return -EIO;
	}

	pci_rescan_bus(bus);

	mutex_unlock(&icm1->lock);
	return 0;
}

static int icm1_ctrl_sock_init(struct rbs_sock *rsk, void *init_param)
{
	struct icm1_sock *sock;

	sock = kmalloc(sizeof(*sock), GFP_KERNEL);
	if (!sock)
		return -ENOMEM;
	sock->r_config = 0;
	sock->icm = init_param;

	rsk->proto_data = sock;
	return 0;
}

static void icm1_ctrl_sock_done(struct rbs_sock *rsk)
{
	struct icm1_sock *sock = rsk->proto_data;

	if (sock->r_config) {
		mutex_lock(&sock->icm->lock);
		sock->icm->configuring = 0;
		mutex_unlock(&sock->icm->lock);
	}

	kfree(sock);
	rsk->proto_data = NULL;
}

static int icm1_load_ps(struct icm1 *icm1, struct file *file,
		unsigned char *buf, int bsiz)
{
	loff_t off;
	int len, val, ret = 0;
	int i;
	struct spi_transfer t;
	struct spi_message m;

	pr_info("ICM1: Starting PS load...\n");

	/* 
	 * preload image
	 *
	 * This limits PS image size, but we cannot hold SPI bus locked
	 * for undefined amount of time and there is a chance that
	 * image is located on the device that is behind the same SPI bus.
	 * Better solution would for user-space to load image to memory
	 * and give it to us (TODO)
	 */
	off = 0;
	while ((len = kernel_read(file, off, buf + off, bsiz - off)) > 0) {
		off += len;
		if ((int) off >= bsiz) {
			pr_err("ICM1: PS image is too big\n");
			ret = -EMSGSIZE;
			goto abort;
		}

		if (signal_pending(current)) {
			ret = -EINTR;
			goto abort;
		}
	}

	if (len) {
		pr_err("ICM1: file read failed offset:%d, %d\n",
				(int) off, len);
		ret = len;
		goto abort;
	}

	len = (int) off;

	/* change bit order  */
	for (i = 0; i < len; i++)
		buf[i] = bit_rev_tbl256[buf[i]];

	/* put device in reset */
	gpio_set_value_cansleep(ICM1_PIN_LOG_RESET_N, 0);
	gpio_set_value_cansleep(ICM1_PIN_PCIE_RESET_N, 0);

	/* clear configuration */
	gpio_set_value_cansleep(ICM1_PIN_CONFIG_N, 0);
	if (icm1_wait_pin(ICM1_PIN_STATUS_N, 0, 1000)) {
		pr_err("ICM1: nSTATUS is not asserted\n");
		ret = -EIO;
		goto abort;
	}
	if (icm1_wait_pin(ICM1_PIN_CONF_DONE, 0, 1000)) {
		pr_err("ICM1: CONF_DONE is not de-asserted\n");
		ret = -EIO;
		goto abort;
	}

	/* lock SPI bus */
	if ((ret = spi_bus_lock(icm1->smaster))) {
		pr_err("ICM1: unable to lock SPI bus, %d\n", ret);
		goto abort;
	}

	/* start configuraiton (CONFIG pin) */
	gpio_set_value_cansleep(ICM1_PIN_CONFIG_N, 1);
	if (icm1_wait_pin(ICM1_PIN_STATUS_N, 1, 1000)) {
		pr_err("ICM1: nSTATUS is not de-asserted\n");
		ret = -EIO;
		goto abort_spi_unlock;
	}
	if (icm1_wait_pin(ICM1_PIN_CONF_DONE, 0, 1000)) {
		pr_err("ICM1: CONF_DONE is not de-asserted\n");
		ret = -EIO;
		goto abort_spi_unlock;
	}
	udelay(3); /* should be at least 2us */

	/* transfer image */
	memset(&t, 0, sizeof(t));
	t.tx_buf = buf;
	t.len = len;
	spi_message_init(&m);
	spi_message_add_tail(&t, &m);
	ret = spi_sync_locked(icm1->sdev, &m);
	if (ret) {
		pr_err("ICM1: unable to transfer image, %d\n", ret);
		goto abort_spi_unlock;
	}

	/* check if configuraiton CRC error occured */
	val = gpio_get_value_cansleep(ICM1_PIN_STATUS_N);
	if (!val) {
		pr_err("ICM1: nSTATUS is asserted (CRC error)\n");
		ret = -EIO;
		goto abort_spi_unlock;
	}

	/* dummy write (make DCLK toggle some more) */
	len = 0;
	t.tx_buf = &len;
	t.len = sizeof(len);
	spi_message_init(&m);
	spi_message_add_tail(&t, &m);
	(void) spi_sync_locked(icm1->sdev, &m);

	/* unlock SPI bus */
	spi_bus_unlock(icm1->smaster);

	/* wait for configuration to be completed */
	if (icm1_wait_pin(ICM1_PIN_CONF_DONE, 1, 1000)) {
		pr_err("ICM1: CONF_DONE is not asserted (nSTATUS=%d)\n",
				gpio_get_value_cansleep(ICM1_PIN_STATUS_N));
		ret = -EIO;
		goto abort;
	}

	/* release resets */
	gpio_set_value_cansleep(ICM1_PIN_PCIE_RESET_N, 1);
	gpio_set_value_cansleep(ICM1_PIN_LOG_RESET_N, 1);

	pr_info("ICM1: PS load successful\n");

	/* it doesn't seem to be possible to
	 * see when initialization is complete when
	 * loading only IO-ring, give it sometime */
	msleep(200);
	return 0;

abort_spi_unlock:
	spi_bus_unlock(icm1->smaster);

abort:
	return ret;
}

static int icm1_load_cvp(struct icm1 *icm1, struct file *file,
		unsigned char *buf, int bsiz)
{
	loff_t off;
	struct cvp_dev *cvp;
	int len, ret = 0;

	gpio_set_value_cansleep(ICM1_PIN_LOG_RESET_N, 0);
	udelay(3);

	cvp = icm1_cvp_init(icm1->pdev, icm1->cvp_regs);
	if (IS_ERR(cvp))
		return -EIO;

	off = 0;
	while ((len = kernel_read(file, off, buf, bsiz)) > 0) {
		off += len;

		ret = icm1_cvp_send(cvp, buf, len);
		if (ret) {
			pr_err("ICM1: icm1_cvp_send() failed, %d\n", ret);
			goto abort;
		}

		if (signal_pending(current)) {
			ret = -EINTR;
			goto abort;
		}

	       if (icm1_cvp_check_status(cvp)) {
		       pr_err("ICM1: CVP CRC error\n");
		       ret = -EIO;
		       goto abort;
	       }
	}

	if (len) {
		pr_err("ICM1: file read failed off:%d, %d\n",
				(int) off, len);
		ret = len;
		goto abort;
	}

	icm1_cvp_finish(cvp, 0);

	gpio_set_value_cansleep(ICM1_PIN_LOG_RESET_N, 1);
	udelay(3);

	if (icm1_wait_pin(ICM1_PIN_CVP_CONFDONE, 1, 1000)) {
		pr_err("ICM1: CVP_CONFDONE is not asserted\n");
		ret = -EIO;
	}

	return ret;

abort:
	icm1_cvp_finish(cvp, 1);
	return ret;
}

static int icm1_ioctl_load(struct icm1 *icm1, struct icm1_sock *sock,
		void __user *arg)
{
	struct rbs_icmctrl_ioc_load ioc;
	struct file *file;
	unsigned char *buf;
	int bsiz, ret;

	if (copy_from_user(&ioc, arg, sizeof(ioc)))
		return -EFAULT;

	mutex_lock(&icm1->lock);
	if (icm1->configuring) {
		mutex_unlock(&icm1->lock);
		return -EBUSY;
	}

	icm1_get_state_pins(icm1);

	if (!(icm1->state & RBS_ICMCTRL_CTRLPINS) ||
	    (ioc.mode & !(icm1->state & RBS_ICMCTRL_PCIREADY)) ||
	    (!ioc.mode & !(icm1->state & RBS_ICMCTRL_SPI))) {
		mutex_unlock(&icm1->lock);
		return -EINVAL;
	}
	if (!ioc.mode && icm1_pci_stop(icm1)) {
		mutex_unlock(&icm1->lock);
		return -EBUSY;
	}

	file = fget(ioc.fd);
	if (!file) {
		mutex_unlock(&icm1->lock);
		return -EBADF;
	}

	if (!ioc.mode)
		bsiz = ICM1_PS_TRFBUFSIZ;
	else
		bsiz = ICM1_CVP_TRFBUFSIZ;

	buf = vmalloc(bsiz);
	if (!buf) {
		mutex_unlock(&icm1->lock);
		return -ENOMEM;
	}

	icm1->configuring = 1;
	sock->r_config = 1;
	mutex_unlock(&icm1->lock);

	if (!ioc.mode)
		ret = icm1_load_ps(icm1, file, buf, bsiz);
	else
		ret = icm1_load_cvp(icm1, file, buf, bsiz);

	mutex_lock(&icm1->lock);
	icm1->configuring = 0;
	sock->r_config = 0;
	mutex_unlock(&icm1->lock);

	vfree(buf);
	fput(file);
	return ret;
}

static int icm1_ctrl_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
		unsigned long arg)
{
	struct icm1_sock *sock = rsk->proto_data;
	struct icm1 *icm1 = sock->icm;
	int ret = 0;

	switch (cmd) {
	case RBS_ICMCTRLIOC_LOAD:
		ret = icm1_ioctl_load(icm1, sock, (void __user *) arg);
		break;
	case RBS_ICMCTRLIOC_PCISCAN:
		ret = icm1_pci_rescan(icm1);
		break;
	case RBS_ICMCTRLIOC_GETSTATE:
	{
		int state;

		mutex_lock(&icm1->lock);
		icm1_get_state_pins(icm1);
		state = icm1->state;
		mutex_unlock(&icm1->lock);

		ret = copy_to_user((void __user *) arg, &state,
				sizeof(state));
		break;
	}
	default:
		ret = -ENOSYS;
		break;
	}

	return ret;
}

static void icm1_vma_open(struct vm_area_struct *vma)
{
	struct icm1 *icm1 = vma->vm_private_data;

	if (!try_module_get(THIS_MODULE))
		pr_err("ICM1: try_module_get() failed\n");

	atomic_inc(&icm1->map_ref);
}

static void icm1_vma_close(struct vm_area_struct *vma)
{
	struct icm1 *icm1 = vma->vm_private_data;

	atomic_dec(&icm1->map_ref);
	module_put(THIS_MODULE);
}

static int icm1_vma_fault(struct vm_area_struct *vma, struct vm_fault *vmf)
{
	return VM_FAULT_SIGBUS;
}

static const struct vm_operations_struct icm1_vm_ops = {
	.open = icm1_vma_open,
	.close = icm1_vma_close,
	.fault = icm1_vma_fault,
};

static int icm1_ctrl_sock_mmap(struct rbs_sock *rsk,
		struct vm_area_struct *vma)
{
	int ret;
	phys_addr_t addr;
	struct icm1_sock *sock = rsk->proto_data;
	struct icm1 *icm1 = sock->icm;
        phys_addr_t size = vma->vm_end - vma->vm_start;
	phys_addr_t offset = vma->vm_pgoff << PAGE_SHIFT;

	mutex_lock(&icm1->lock);
	if (icm1->configuring) {
		mutex_unlock(&icm1->lock);
		return -EBUSY;
	}
	icm1_get_state_pins(icm1);
	if (!(icm1->state & RBS_ICMCTRL_PCIREADY) ||
	    !(icm1->state & RBS_ICMCTRL_USER)) {
		mutex_unlock(&icm1->lock);
		return -EINVAL;
	}

	if (offset + size > pci_resource_len(icm1->pdev, 0)) {
		mutex_unlock(&icm1->lock);
		return -EINVAL;
	}

	vma->vm_private_data = icm1;
	vma->vm_ops = &icm1_vm_ops;

	vma->vm_flags |= VM_IO | VM_DONTDUMP | VM_DONTCOPY;
	vma->vm_page_prot = pgprot_noncached(vma->vm_page_prot);

	addr = pci_resource_start(icm1->pdev, 0) + offset;

	ret = remap_pfn_range(vma, vma->vm_start,
			addr >> PAGE_SHIFT, size,
			vma->vm_page_prot);
	if (!ret)
		icm1_vma_open(vma);

	mutex_unlock(&icm1->lock);
	return ret;
}

static struct rbs_proto_ops icm1_ctrl_proto_ops = {
	.init = icm1_ctrl_sock_init,
	.done = icm1_ctrl_sock_done,
	.ioctl = icm1_ctrl_sock_ioctl,
#if defined(CONFIG_COMPAT)
	.compat_ioctl = icm1_ctrl_sock_ioctl,
#endif
	.mmap = icm1_ctrl_sock_mmap,
};

static int icm1_pci_probe(struct pci_dev *pdev,
		const struct pci_device_id *ent)
{
	int ret;

	icm1->pdev = pdev;

	ret = pci_enable_device(icm1->pdev);
	if (ret) {
		pr_err("ICM1: unable to enable device, %d\n", ret);
		icm1->state |= RBS_ICMCTRL_PCIPRFAIL;
		return ret;
	}

	icm1->cvp_regs = pci_iomap(pdev, 0, 0x10000);
	if (!icm1->cvp_regs) {
		pr_err("ICM1: failed to map MEM region\n");
		icm1->state |= RBS_ICMCTRL_PCIPRFAIL;
		return -EIO;
	}

	icm1->state |= RBS_ICMCTRL_PCIREADY;

	return 0;
}

static void icm1_pci_remove(struct pci_dev *pdev)
{
	if (icm1->cvp_regs)
		iounmap(icm1->cvp_regs);
	icm1->pdev = NULL;
	icm1->cvp_regs = NULL;
	icm1->state &= ~RBS_ICMCTRL_PCIREADY;
}

static int icm1_spi_probe(struct spi_device *spi)
{
	pr_info("ICM1: %s\n", __func__);

	if (icm1 && spi) {
		/* If on dusX3 this device is defined in the dtb */
		spi->mode = SPI_MODE_0;

		icm1->smaster = spi_master_get(spi->master);
		if (!icm1->smaster) {
			pr_err("ICM1: Unable to hold icm1 spi bus master\n");

			return -EIO;
		}
		icm1->sdev = spi;
		icm1->state |= RBS_ICMCTRL_SPI;

		pr_info("ICM1: state %x\n", icm1->state);

		return 0;
	}

	return -ENODEV;
}

static int icm1_spi_remove(struct spi_device *spi)
{
	pr_info("ICM1 : %s\n", __func__);

	icm1->state &= ~RBS_ICMCTRL_SPI;
	icm1->sdev = NULL;
	spi_master_put(icm1->smaster);
	icm1->smaster = NULL;

	return 0;
}

static struct gpio icm1_gpios[] = {
	{
		.gpio = ICM1_PIN_CONFIG_N,
		.flags = GPIOF_OUT_INIT_HIGH,
		.label = "icm1_config_n",
	},
	{
		.gpio = ICM1_PIN_CONF_DONE,
		.flags = GPIOF_IN,
		.label = "icm1_conf_done",
	},
	{
		.gpio = ICM1_PIN_STATUS_N,
		.flags = GPIOF_IN,
		.label = "icm1_status_n",
	},
	{
		.gpio = ICM1_PIN_CVP_CONFDONE,
		.flags = GPIOF_IN,
		.label = "icm1_cvp_confdone",
	},
	{
		.gpio = ICM1_PIN_INIT_DONE,
		.flags = GPIOF_IN,
		.label = "icm1_cvp_init_done",
	},
	{
		.gpio = ICM1_PIN_PCIE_RESET_N,
		.flags = GPIOF_OUT_INIT_HIGH,
		.label = "icm1_pcie_reset_n",
	},
	{
		.gpio = ICM1_PIN_LOG_RESET_N,
		.flags = GPIOF_OUT_INIT_HIGH,
		.label = "icm1_log_reset_n",
	},
};

static struct spi_driver icm1_spi_driver = {
	.driver = {
		.name =            "icm1_ps",
		.owner =           THIS_MODULE,
	},
	.probe =                   icm1_spi_probe,
	.remove =                  icm1_spi_remove,
};

static DEFINE_PCI_DEVICE_TABLE(icm1_pci_tbl) = {
	{PCI_DEVICE(PCI_COBRA_VENDOR, PCI_COBRA_DEVICE)},
	{PCI_DEVICE(PCI_TAIPAN_VENDOR, PCI_TAIPAN_DEVICE)},
	{PCI_DEVICE(PCI_KATLA_VENDOR, PCI_KATLA_DEVICE)},
	{0,},
};

static struct pci_driver icm1_pci_driver = {
	.name 		= "icm1",
	.id_table 	= icm1_pci_tbl,
	.probe 		= icm1_pci_probe,
	.remove 	= icm1_pci_remove,
};

static struct rbs_fn icm_fn = {
	.name = "icm",
	.owner = THIS_MODULE,
};

static __init int icm1_top_init(void)
{
	int ret;

	icm1 = kzalloc(sizeof(*icm1), GFP_KERNEL);
	if (!icm1) {
		pr_err("%s: out of memory\n", __func__);
		return -ENOMEM;
	}

	mutex_init(&icm1->lock);
	atomic_set(&icm1->map_ref, 0);

	ret = pci_register_driver(&icm1_pci_driver);
	if (ret < 0) {
		pr_err("%s: unable to register PCIe driver, %d\n",
				__func__, ret);
		kfree(icm1);
		return -EIO; /* XXX */
	}

	/* get GPIO pins */
	ret = gpio_request_array(icm1_gpios, ARRAY_SIZE(icm1_gpios));
	if (ret) {
		pr_err("%s: unable to request GPIOs\n", __func__);
		goto pci_cleanup;
	}
	icm1->state |= RBS_ICMCTRL_CTRLPINS;

	ret = spi_register_driver(&icm1_spi_driver);
	if (ret < 0) {
		pr_err("%s: unable to register icm1_spi_driver\n", __func__);
		goto gpio_cleanup;
	}
	/* fetch current FPGA mode */
	icm1_get_state_pins(icm1);

	/* register API */
	ret = rbs_proto_register(RBS_PROTO_ICMCTRL, &icm1_ctrl_proto_ops, icm1);
	if (ret) {
		pr_err("%s: unable to register RBS_PROTO_ICMCTRL, %d\n",
				__func__, ret);
		goto spi_cleanup;
	}

	ret = rbs_fn_add(&icm_fn);
	if (ret) {
		pr_err("%s: unable to register FN, %d\n", __func__, ret);
		goto fn_cleanup;
	}

	pr_info("ICM1: state %x\n", icm1->state);
	return 0;

fn_cleanup:
	rbs_proto_unregister(RBS_PROTO_ICMCTRL, &icm1_ctrl_proto_ops);

spi_cleanup:
	spi_unregister_driver(&icm1_spi_driver);

gpio_cleanup:
	gpio_free_array(icm1_gpios, ARRAY_SIZE(icm1_gpios));

pci_cleanup:
	pci_unregister_driver(&icm1_pci_driver);

	kfree(icm1);
	return -EFAULT;
}

static void __exit icm1_top_exit(void)
{
	rbs_fn_remove(&icm_fn);

	rbs_proto_unregister(RBS_PROTO_ICMCTRL, &icm1_ctrl_proto_ops);

	spi_unregister_driver(&icm1_spi_driver);

	pci_unregister_driver(&icm1_pci_driver);

	gpio_free_array(icm1_gpios, ARRAY_SIZE(icm1_gpios));

	kfree(icm1);
}

module_init(icm1_top_init);
module_exit(icm1_top_exit);
MODULE_LICENSE("GPL");

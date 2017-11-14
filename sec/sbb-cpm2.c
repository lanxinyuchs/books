/*
 * Copyright (C) 2014 Ericsson AB
 *
 * sbb.c
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
 * FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
 * EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
 * OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND,
 * EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
 * IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
 * NECESSARY SERVICING, REPAIR OR CORRECTION.
 *
 * IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL
 * ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE
 * THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
 * GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE
 * OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA
 * OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES
 * OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
 * EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGES.
 *
 * You should have received a copy of the GNU General Public License version 2
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 */

#include <linux/init.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <asm/io.h>
#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/device.h>
#include <linux/of.h>
#include <linux/rbs/rbs-rni.h>
#include <asm/uaccess.h>
#include <linux/delay.h>

MODULE_LICENSE("GPL");

#include "linux/rbs/rbs-sbb.h"
#if defined(CONFIG_COMPAT)
#include "sbb-cpm2-compat.h"
#endif

/* measurements of MAX_IMG_POLL with different data lengths suggests this
   formula: MAX_IMG_POLL = 6 + (data_len - 0x40000) / 0x58000.
   instead just use double of the MAX_IMG_POLL needed for the largest
   size that was possible to kmalloc on an idle system (~3MB --> 14ms). */
#define MAX_SDO_POLL			8  /* 8 ms */
#define MAX_IMG_POLL			28  /* 28 ms */
#define MAX_ZEROIZE_POLL		8  /* 8 ms */

#define IMG_MIN_OVERHEAD              372
#define IMG_MAGIC                     0x53424221

#define SBB_NUM_PARAM_REGS		60

#define SBB_PHYSICAL_ADDRESS		0x8032910000ULL
#define SBB_PHYSICAL_SIZE		0x10000

#define SBB_REG_SEMAPHORE		0x0800
#define SBB_REG_FUNCTION_CONTROL	0x0804
#define SBB_REG_STATUS			0x0810
#define SBB_REG_DEBUG			0x081C

/* The base address of the SBB parameter registers is 0x0820 but the command
   used int this file uses 0x838 as base register for their parameters so I
   will use this as base address in the definition to avoid having to pad the
   parameter structure. */
#define SBB_REG_PARAMS			0x0838
#define SBB_REG_INTERRUPT_STATUS	0x0e04
#define SBB_REG_CLEAR_MEMORY	        0x408C

#define SBB_ISR_ERROR_KEY_EXPIRED	(1<<26)
#define SBB_ISR_ERROR_KEY_TYPE		(1<<25)
#define SBB_ISR_ERROR_SIG_GEN_TIMEOUT	(1<<24)
#define SBB_ISR_ERROR_NO_KAK		(1<<23)
#define SBB_ISR_ERROR_HASH_LENGTH	(1<<22)
#define SBB_ISR_ERROR_KEY_LENGTH	(1<<21)
#define SBB_ISR_ERROR_CRYPTO_MODE	(1<<20)
#define SBB_ISR_ERROR_CRYPTO_CFG	(1<<19)
#define SBB_ISR_ERROR_KEY_ID		(1<<18)
#define SBB_ISR_SELF_TEST_FAILED	(1<<17)
#define SBB_ISR_AUTH_DONE		(1<<16)
#define SBB_ISR_ERROR_ADDRESS		(1<<15)
#define SBB_ISR_ERROR_VERSION		(1<<14)
#define SBB_ISR_ERROR_INVALID_SIG	(1<<12)
#define SBB_ISR_ERROR_SDO_AUTH_FAILED	(1<<11)
#define SBB_ISR_ERROR_DATA		(1<<10)
#define SBB_ISR_ERROR_PAD		(1<<9)
#define SBB_ISR_ERROR_LENGTH		(1<<8)
#define SBB_ISR_ERROR_ID		(1<<7)
#define SBB_ISR_ERROR_SECBOOT_OFF	(1<<5)
#define SBB_ISR_ERROR_ROOTKEY		(1<<4)
#define SBB_ISR_ERROR_SEC_BOOT_KEK	(1<<3)
#define SBB_ISR_ERROR_HW		(1<<2)
#define SBB_ISR_INFO_VER_UPD_NEEDED	(1<<1)
#define SBB_ISR_DONE			(1<<0)

#define SBB_ISR_SDO_SET_ERR_BITS (SBB_ISR_ERROR_ADDRESS |		\
				  SBB_ISR_ERROR_LENGTH |		\
				  SBB_ISR_ERROR_ROOTKEY)
#define SBB_ISR_SDO_GET_ERR_BITS (SBB_ISR_ERROR_ADDRESS |		\
				  SBB_ISR_ERROR_ROOTKEY)
#define SBB_ISR_SDO_GET_FLT_BITS (SBB_ISR_ERROR_SDO_AUTH_FAILED |	\
				  SBB_ISR_ERROR_DATA |			\
				  SBB_ISR_ERROR_PAD |			\
				  SBB_ISR_ERROR_LENGTH |		\
				  SBB_ISR_ERROR_ID)
#define SBB_ISR_SDO_GET_BITS (SBB_ISR_SDO_GET_ERR_BITS | 		\
			      SBB_ISR_SDO_GET_FLT_BITS)
#define SBB_ISR_IMG_VERIFY_ERR_BITS (SBB_ISR_ERROR_KEY_EXPIRED |	\
				     SBB_ISR_ERROR_KEY_TYPE |		\
				     SBB_ISR_ERROR_NO_KAK |		\
				     SBB_ISR_ERROR_KEY_LENGTH |		\
				     SBB_ISR_ERROR_SECBOOT_OFF |	\
				     SBB_ISR_ERROR_SEC_BOOT_KEK)
#define SBB_ISR_IMG_VERIFY_FLT_BITS (SBB_ISR_ERROR_ADDRESS |		\
				     SBB_ISR_ERROR_VERSION |		\
				     SBB_ISR_ERROR_INVALID_SIG |	\
				     SBB_ISR_ERROR_DATA |		\
				     SBB_ISR_ERROR_PAD |		\
				     SBB_ISR_ERROR_LENGTH |		\
				     SBB_ISR_ERROR_ID)
#define SBB_ISR_IMG_VERIFY_BITS (SBB_ISR_IMG_VERIFY_ERR_BITS |		\
				 SBB_ISR_IMG_VERIFY_FLT_BITS)

#define SBB_DBG_STAGE			0xffff

#define SBB_FUNC_NOP			0
#define SBB_FUNC_IMG_VERIF		1
#define SBB_FUNC_IMG_SIGN		2
#define SBB_FUNC_SET_SDO		3
#define SBB_FUNC_GET_SDO		4
#define SBB_FUNC_AUTH_REQ		5
#define SBB_FUNC_AUTH_RSP		6
#define SBB_FUNC_AUTH_LOAD		7
#define SBB_FUNC_AUTH_CLOSE		8
#define SBB_FUNC_KEY_VALID		9
#define SBB_FUNC_GET_TRNG		10
#define SBB_FUNC_IMG_VERIF_DR		11
#define SBB_FUNC_GET_SDO_DR		12
#define SBB_FUNC_SELF_TEST		13
#define SBB_FUNC_REMOVE_SDO		14
#define SBB_FUNC_ENCRYPT		15
#define SBB_FUNC_DECRYPT		16
#define SBB_FUNC_HASH			17
#define SBB_FUNC_SIG_GEN		18
#define SBB_FUNC_SIG_VERIF		19
#define SBB_FUNC_LOAD_KEY		20
#define SBB_FUNC_INIT			0xffff /* Can't be called */

#define SBB_SDO_DATA_ALIGN_BYTES	16

#define ECM_PHYSICAL_ADDRESS		0x8002c04000ULL
#define ECM_PHYSICAL_SIZE		0x124
#define ECM_REG_CTRL			0x00
#define ECM_REG_STAT			0x04
#define ECM_REG_ADDR			0x14
#define ECM_REG_RDAT			0x40

#define ECM_CTRL_R_SIZE			0xff000000
#define ECM_CTRL_R_SIZE_SHIFT(x)	((x & 0xff) << 24)
#define ECM_CTRL_W_SIZE			0x00ff0000
#define ECM_CTRL_W_SIZE_SHIFT(x)	((x & 0xff) << 16)
#define ECM_CTRL_CLR_R_DATA		(1<<15)
#define ECM_CTRL_PWR_DOWN		(1<<7)
#define ECM_CTRL_PROG_W_DATA		(1<<2)
#define ECM_CTRL_CLR_W_DATA		(1<<1)
#define ECM_CTRL_READ_EFUSE		(1<<0)

#define ECM_ADDR_MARG_READ		(1<<31)
#define ECM_ADDR_RW_ADDR		0x1fff

#define ECM_STAT_INTERESING_BITS	0x1fbfc000 /* Added new info status bits, not sure if needed */
#define ECM_STAT_LOCAL_R_DONE		(1<<29)

#define CLM_ZEROIZE_REQ                 1

#ifndef CONFIG_RBS_SBB_DRIVER_MAJOR_NUMBER
  #define CONFIG_RBS_SBB_DRIVER_MAJOR_NUMBER 0 /* Dynamic allocation */
#endif
#ifndef CONFIG_RBS_SBB_DRIVER_MINOR_NUMBER
  #define CONFIG_RBS_SBB_DRIVER_MINOR_NUMBER 0 /* First minor number */
#endif
#define SBB_MINOR_OFFSET		0
#define DRIVER_NUM_DEVICES		1
#define DRIVER_NAME			"rbs-sbb"


typedef union ioc_params {
#if defined(CONFIG_COMPAT)
	struct sbb_compat_sdo_set compat_sdo_set;
	struct sbb_compat_sdo_get compat_sdo_get;
	struct sbb_compat_img_verify compat_img_verify;
#endif
	sbb_ioc_sdo_set_t   sdo_set;
	sbb_ioc_sdo_get_t   sdo_get;
	sbb_ioc_efuse_get_t efuse_get;
	sbb_ioc_img_verify_t img_verify;
} ioc_params_t;

/* SDO object header is written by HW in big endian */
struct sbb_sdo_object_header {
	u32 id;
	u32 length;
	u8  pad_length;
	u8  reserved;
	u16 object_info;
	u8  iv[16];
};

struct sbb_sdo_parameters {
	u32 src_address_hi;
	u32 src_address_lo;
	u32 dst_address_hi;
	u32 dst_address_lo;
	u32 pad1;
	u32 data_length;
	u32 owner_info_hi;
	u32 owner_info_lo;
	u32 pad2;
	u16 object_info;
	u16 pad3;
	u32 srk_index_hi;
	u32 srk_index_lo;
	u8  pad4[24];
	u8  iv[SDO_IV_LENGTH];
};

struct sbb_img_verify_parameters {
	u32 src_address_hi;
	u32 src_address_lo;
	u32 dst_address_hi;
	u32 dst_address_lo;
	u64 pad[2];
	u64 version; /* output */
};

struct sbb_device {
	dev_t                  driver;
	int                    major_number;
	int                    minor_number;
	struct class           *class_p;
	struct semaphore       semaphore;
	void __iomem           *sbb_base_address;
	void __iomem           *ecm_base_address;
	struct cdev            cdev;
	struct file_operations fopts;
	u32                    max_sdo_poll;
};

struct sbb_common_sdo_par {
	uint32_t data_length;
	uint32_t sdo_length;
	uint32_t owner_info_hi;
	uint32_t owner_info_lo;
	uint16_t object_info;
	uint16_t srk_index;
	uint8_t  iv[SDO_IV_LENGTH];
};

struct sbb_img_verify_par {
	uint32_t img_length;
	uint32_t failed; /* output, 0 = verify OK, 1 = verify NOK */
};

static struct sbb_device *sbb_dev_p;

static inline int verify_mem_access(unsigned int dir,
				    void *ptr,
				    unsigned long len)
{
	if (dir & _IOC_READ)
		return !access_ok(VERIFY_WRITE, (void __user *)ptr, len);
	else
		return !access_ok(VERIFY_READ, (void __user *)ptr, len);
}

static inline int sbb_clean_up(int rc, void *ptr1, void *ptr2)
{
	if (ptr1 != NULL)
		kfree(ptr1);

	if (ptr2 != NULL)
		kfree(ptr2);

	return rc;
}

static inline int sdo_get_object_size(unsigned int data_size)
{
	int pad_size =
		SBB_SDO_DATA_ALIGN_BYTES - (data_size%SBB_SDO_DATA_ALIGN_BYTES);
	if (pad_size == SBB_SDO_DATA_ALIGN_BYTES)
		pad_size = 0;
	return SDO_MIN_OVERHEAD + data_size + pad_size;
}

static inline u32 sdo_get_data_size(void *sdo_p)
{
	struct sbb_sdo_object_header *head_p = \
		(struct sbb_sdo_object_header *)sdo_p;
	return ntohl(head_p->length) - head_p->pad_length;
}

static inline u32 sbb_read32(u32 offset)
{
	return readl(sbb_dev_p->sbb_base_address + offset);
}

static inline void sbb_write32(u32 offset, u32 value)
{
	writel(value, sbb_dev_p->sbb_base_address + offset);
}

static inline u64 sbb_read64(u32 offset)
{
	return readq(sbb_dev_p->sbb_base_address + offset);
}

static inline void sbb_write64(u32 offset, u64 value)
{
	writeq(value, sbb_dev_p->sbb_base_address + offset);
}

void sbb_print_isr(u32 isr, const char *function)
{
	if (function != NULL)
		pr_err("SBB Error in function %s\n", function);
	if (isr & SBB_ISR_ERROR_KEY_EXPIRED)
		pr_err("SBB ISR 26 - ERROR_KEY_EXPIRED\n");
	if (isr & SBB_ISR_ERROR_KEY_TYPE)
		pr_err("SBB ISR 25 - ERROR_KEY_TYPE\n");
	if (isr & SBB_ISR_ERROR_SIG_GEN_TIMEOUT)
		pr_err("SBB ISR 24 - ERROR_SIG_GEN_TIMEOUT\n");
	if (isr & SBB_ISR_ERROR_NO_KAK)
		pr_err("SBB ISR 23 - ERROR_NO_KAK\n");
	if (isr & SBB_ISR_ERROR_HASH_LENGTH)
		pr_err("SBB ISR 22 - ERROR_HASH_LENGTH\n");
	if (isr & SBB_ISR_ERROR_KEY_LENGTH)
		pr_err("SBB ISR 21 - ERROR_KEY_LENGHT\n");
	if (isr & SBB_ISR_ERROR_CRYPTO_MODE)
		pr_err("SBB ISR 20 - ERROR_CRYPTO_MODE\n");
	if (isr & SBB_ISR_ERROR_CRYPTO_CFG)
		pr_err("SBB ISR 19 - ERROR_CRYPTO_CFG\n");
	if (isr & SBB_ISR_ERROR_KEY_ID)
		pr_err("SBB ISR 18 - ERROR_KEY_ID\n");
	if (isr & SBB_ISR_SELF_TEST_FAILED)
		pr_err("SBB ISR 17 - SELF_TEST_FAILED\n");
	if (isr & SBB_ISR_AUTH_DONE)
		pr_info("SBB ISR 16 - AUTH_DONE\n");
	if (isr & SBB_ISR_ERROR_ADDRESS)
		pr_err("SBB ISR 15 - ERROR_ADDRESS\n");
	if (isr & SBB_ISR_ERROR_VERSION)
		pr_err("SBB ISR 14 - ERROR_VERSION\n");
	if (isr & SBB_ISR_ERROR_INVALID_SIG)
		pr_err("SBB ISR 12 - ERROR_INVALID_SIG\n");
	if (isr & SBB_ISR_ERROR_SDO_AUTH_FAILED)
		pr_err("SBB ISR 11 - ERROR_SDO_AUTH_FAILED\n");
	if (isr & SBB_ISR_ERROR_DATA)
		pr_err("SBB ISR 10 - ERROR_DATA\n");
	if (isr & SBB_ISR_ERROR_PAD)
		pr_err("SBB ISR 9 - ERROR_PAD\n");
	if (isr & SBB_ISR_ERROR_LENGTH)
		pr_err("SBB ISR 8 - ERROR_LENGTH\n");
	if (isr & SBB_ISR_ERROR_ID)
		pr_err("SBB ISR 7 - ERROR_ID\n");
	if (isr & SBB_ISR_ERROR_SECBOOT_OFF)
		pr_err("SBB ISR 5 - ERROR_SECBOOT_OFF\n");
	if (isr & SBB_ISR_ERROR_ROOTKEY)
		pr_err("SBB ISR 4 - ERROR_ROOTKEY\n");
	if (isr & SBB_ISR_ERROR_SEC_BOOT_KEK)
		pr_err("SBB ISR 3 - ERROR_SEC_BOOT_KEK\n");
	if (isr & SBB_ISR_ERROR_HW)
		pr_err("SBB ISR 2 - ERROR_HW\n");
	if (isr & SBB_ISR_INFO_VER_UPD_NEEDED)
		pr_err("SBB ISR 1 - INFO_VER_UPD_NEEDED\n");
	if (isr & SBB_ISR_DONE)
		pr_info("SBB ISR 0 - SBB_ISR_DONE\n");
}

static int sbb_get_lock(void)
{
	u32 lock = 0;

	mb();
	lock = sbb_read32(SBB_REG_SEMAPHORE);
	if (lock != 0)
		return 0;
	pr_err("SBB Could not get lock\n");
	return -1;
}

static void sbb_unlock(void)
{
	sbb_write32(SBB_REG_SEMAPHORE, 0);
	mb();
}

static void sbb_write_parameters(void *params_p, unsigned int length)
{
	u32 offset = SBB_REG_PARAMS;
	u32 end = SBB_REG_PARAMS + length;
	u64 *data_p = (u64 *)params_p;

	while (offset < end) {
		sbb_write64(offset, *data_p);
		data_p++;
		offset += 8;
	}
	mb();
}

static void sbb_read_parameters(void *params_p, unsigned int length)
{
	u32 offset = SBB_REG_PARAMS;
	u32 end = SBB_REG_PARAMS + length;
	u32 *data_p = (u32 *)params_p;

	mb();
	while (offset < end) {
		*data_p = sbb_read32(offset);
		data_p++;
		offset += 4;
	}
}

static void sbb_clear_isr(void)
{
	sbb_write32(SBB_REG_INTERRUPT_STATUS, ~0);
	mb();
}

static int sbb_execute_command(u32 command)
{
	sbb_write32(SBB_REG_FUNCTION_CONTROL, command);
	mb();

	return 0;
}

static int sbb_wait_done(u32 *isr_p, unsigned int max_poll)
{
	unsigned int i;
	u64 stat;
	const unsigned long timeout_ns = 1000 * 1000;
	ktime_t wait;

	mb();
	for (i = 0; i < max_poll; i++) {
		*isr_p = sbb_read32(SBB_REG_INTERRUPT_STATUS);
		if (*isr_p & SBB_ISR_DONE)
			return 0;

		wait = ns_to_ktime(timeout_ns);
		set_current_state(TASK_UNINTERRUPTIBLE);
		(void) schedule_hrtimeout(&wait, HRTIMER_MODE_REL);
	}
	/* It took too long time */
	stat = sbb_read64(SBB_REG_STATUS);
	panic("SBB Did not get command done, "
	      "isr 0x%08x stat high 0x%08x stat low 0x%08x\n",
	      *isr_p, ((u32)(stat >> 32)), ((u32)(stat & 0xffffffff)));

	return -EFAULT;
}

static int sbb_do_command(u32 cmd, void *src, void *dst,
			  struct sbb_common_sdo_par *par, const char *caller)
{
	u32 isr = 0, isr_mask;
	int err;
	struct sbb_sdo_parameters sbb_params = {0};
	phys_addr_t src_phys_addr, dst_phys_addr;

	src_phys_addr = virt_to_phys(src);
	sbb_params.src_address_hi = (u32)(src_phys_addr>>32);
	sbb_params.src_address_lo = (u32)(src_phys_addr);

	dst_phys_addr = virt_to_phys(dst);
	sbb_params.dst_address_hi = (u32)(dst_phys_addr>>32);
	sbb_params.dst_address_lo = (u32)(dst_phys_addr);

	sbb_params.owner_info_hi = par->owner_info_hi;
	sbb_params.owner_info_lo = par->owner_info_lo;

	if (cmd == SBB_FUNC_SET_SDO) {
		int i;

		sbb_params.data_length = par->data_length;

		sbb_params.object_info = par->object_info;
		sbb_params.srk_index_hi = 0; /* Hard coded for now */
		sbb_params.srk_index_lo = 0; /* Hard coded for now */

		for (i = 0; i < SDO_IV_LENGTH; i++)
			sbb_params.iv[i] = par->iv[i];

		isr_mask = SBB_ISR_SDO_SET_ERR_BITS;

	} else if (cmd == SBB_FUNC_GET_SDO) {
		isr_mask = SBB_ISR_SDO_GET_BITS;
	} else {
		return -EFAULT;
	}

	sbb_clear_isr();
	sbb_write_parameters(&sbb_params, sizeof(sbb_params));

	if ((err = sbb_execute_command(cmd)))
		return sbb_clean_up(err, src, dst);
	if ((err = sbb_wait_done(&isr, sbb_dev_p->max_sdo_poll)))
		return sbb_clean_up(err, src, dst);
	if (isr & isr_mask) {
		/* error or faulty sdo */
		sbb_print_isr(isr, caller);
		if (isr & SBB_ISR_SDO_GET_ERR_BITS)
			return sbb_clean_up(-EFAULT, src, dst);
		else
			return sbb_clean_up(-EINVAL, src, dst);
	}

	return 0;
}

static int sbb_zeroize(void)
{
	u32 isr = 0;

	sbb_clear_isr();
	sbb_write32(SBB_REG_CLEAR_MEMORY, CLM_ZEROIZE_REQ);
	return sbb_wait_done(&isr, MAX_ZEROIZE_POLL);
}

static int sbb_mem_for_sdo_set(struct sbb_common_sdo_par *par, void __user *dst,
			       void **sdo, void **data)
{
	int sdo_length;

	/* Check data length and access to return buffer */
	if (par->data_length > SDO_MAX_LENGTH ||
	    par->data_length == 0) {
		pr_err("SBB invalid data length (%u)\n",
		       par->data_length);
		return -EINVAL;
	}
	sdo_length = sdo_get_object_size(par->data_length);

	if (verify_mem_access(_IOC_READ, dst, sdo_length)) {
		pr_err("SBB could not access out buffert at %p\n", dst);
		return -EFAULT;
	}

	if (!data || !sdo)
		return -EFAULT;

	/* Setup internal buffers */
	*data = kmalloc(par->data_length, GFP_DMA | GFP_KERNEL);
	if (*data == NULL) {
		pr_err("SBB could not alloc mem\n");
		return -ENOMEM;
	}
	*sdo = kmalloc(sdo_length, GFP_DMA | GFP_KERNEL);
	if (*sdo == NULL) {
		pr_err("SBB could not alloc mem\n");
		return sbb_clean_up(-ENOMEM, *data, NULL);
	}

	return sdo_length;
}

static int sbb_mem_for_sdo_get(struct sbb_common_sdo_par *par, void __user *dst,
			       void **sdo, void **data)
{
	/* Check sdo length and access to return buffer */
	if (par->sdo_length > SDO_MAX_LENGTH ||
	    par->sdo_length <= SDO_MIN_OVERHEAD) {
		pr_err("SBB invalid sdo length (%u)\n",
		       par->sdo_length);
		return -EINVAL;
	}

	if (verify_mem_access(_IOC_READ, dst, par->sdo_length)) {
		pr_err("SBB could not access out buffert at %p\n", dst);
		return -EFAULT;
	}

	if (!data || !sdo)
		return -EFAULT;

	/* Setup internal buffers */
	*data = kmalloc(par->sdo_length - SDO_MIN_OVERHEAD,
			 GFP_KERNEL);
	if (*data == NULL) {
		pr_err("SBB could not alloc mem\n");
		return -ENOMEM;
	}
	*sdo = kmalloc(par->sdo_length, GFP_DMA | GFP_KERNEL);
	if (*sdo == NULL) {
		pr_err("SBB could not alloc mem\n");
		return sbb_clean_up(-ENOMEM, *data, NULL);
	}
	return 0;
}

static int sdo_do_set_body(void __user *src, void __user *dst,
			   struct sbb_common_sdo_par *par)
{
	unsigned long left;
	void *data_p = NULL;
	void *sdo_p = NULL;
	int sdo_length;

	if ((sdo_length = (sbb_mem_for_sdo_set(par, dst, &sdo_p, &data_p))) < 0)
		return sdo_length;

	/* Get data. This one does access_ok on the address */
	left = copy_from_user(data_p, src, par->data_length);
	if (left != 0) {
		pr_err("SBB could not get data (%lu left)\n", left);
		return sbb_clean_up(-EFAULT, data_p, sdo_p);
	}

	if (sbb_do_command(SBB_FUNC_SET_SDO, data_p, sdo_p, par, __func__))
		return -EFAULT;

	/* Return data. We have already done access_ok on the address */
	left = __copy_to_user(dst, sdo_p, sdo_length);
	if (left != 0) {
		pr_err("SBB could not write sdo data (%lu left)\n", left);
		return sbb_clean_up(-EFAULT, data_p, sdo_p);
	}
	/* Return the length */
	par->sdo_length = (uint32_t)sdo_length;

	if (sbb_zeroize())
		return -EFAULT;

	return sbb_clean_up(0, data_p, sdo_p);
}

static int sdo_do_get_body(void __user *src, void __user *dst,
			   struct sbb_common_sdo_par *par)
{
	unsigned long left;
	void *data_p = NULL;
	void *sdo_p = NULL;
	u32 data_length;
	struct sbb_sdo_parameters sbb_params;
	int ret;

	if ((ret = (sbb_mem_for_sdo_get(par, dst, &sdo_p, &data_p))) < 0)
		return ret;

	/* Get data. This one does access_ok on the address */
	left = copy_from_user(sdo_p, src, par->sdo_length);
	if (left != 0) {
		pr_err("SBB could not get sdo (%lu left)\n", left);
		return sbb_clean_up(-EFAULT, data_p, sdo_p);
	}

	if (sbb_do_command(SBB_FUNC_GET_SDO, sdo_p, data_p, par, __func__))
		return -EFAULT;

	/* Return data. We have already done access_ok on the address */
	data_length = sdo_get_data_size(sdo_p);
	left = __copy_to_user(dst, data_p, data_length);
	if (left != 0) {
		pr_err("SBB could not copy sdo data (%lu left)\n", left);
		return sbb_clean_up(-EFAULT, data_p, sdo_p);
	}

	/* Return the length and object_info, get object_info from sbb params */
	sbb_read_parameters(&sbb_params, sizeof(sbb_params));
	par->object_info = sbb_params.object_info;
	par->data_length = data_length;

	if (sbb_zeroize())
		return -EFAULT;

	return sbb_clean_up(0, data_p, sdo_p);
}

static int img_do_verify_body(void __user *img, struct sbb_img_verify_par *par)
{
	unsigned long left;
	void *img_p;
	struct sbb_img_verify_parameters sbb_params = {0};
	phys_addr_t img_phys_addr;
	u32 isr;
	int err;

	/* Default value */
	par->failed = 1;

	/* Check img length */
	if (par->img_length > IMG_MAX_LENGTH ||
	    par->img_length <= IMG_MIN_OVERHEAD) {
		pr_err("SBB invalid image size (%u)\n",
		       par->img_length);
		return -EINVAL;
	}

	/* Setup internal buffers */
	img_p = kmalloc(par->img_length,
			(GFP_DMA | GFP_KERNEL));
	if (img_p == NULL) {
		pr_err("SBB could not alloc mem\n");
		return -ENOMEM;
	}

	/* Get data. This one does access_ok on the address */
	left = copy_from_user(img_p, img, par->img_length);
	if (left != 0) {
		pr_err("SBB could not get image (%lu left)\n", left);
		return sbb_clean_up(-EFAULT, img_p, NULL);
	}

	if (*((u32 *)img_p) != htonl(IMG_MAGIC)) {
		pr_err("SBB not a scure boot image (magic 0x%08x)\n",
			ntohl(*((u32 *)img_p)));
		return sbb_clean_up(-EINVAL, img_p, NULL);
	}

	img_phys_addr = virt_to_phys(img_p);
	sbb_params.src_address_hi = (u32)(img_phys_addr>>32);
	sbb_params.src_address_lo = (u32)(img_phys_addr);
	sbb_params.dst_address_hi = sbb_params.src_address_hi;
	sbb_params.dst_address_lo = sbb_params.src_address_lo;

	sbb_clear_isr();
	sbb_write_parameters(&sbb_params, sizeof(sbb_params));
	if ((err = sbb_execute_command(SBB_FUNC_IMG_VERIF))) {
		return sbb_clean_up(err, img_p, NULL);
	}
	if ((err = sbb_wait_done(&isr, MAX_IMG_POLL))) {
		return sbb_clean_up(err, img_p, NULL);
	}

	if (isr & SBB_ISR_IMG_VERIFY_BITS) {
		sbb_print_isr(isr, __func__);
		if (isr & SBB_ISR_IMG_VERIFY_ERR_BITS) {
			return sbb_clean_up(-EFAULT, img_p, NULL);
		}
		return sbb_clean_up(-EINVAL, img_p, NULL);
	}

	par->failed = 0;
	return sbb_clean_up(0, img_p, NULL);
}

static int sdo_do_set(ioc_params_t *params_p)
{
	sbb_ioc_sdo_set_t *par = &params_p->sdo_set;
	struct sbb_common_sdo_par pp;
	int ret;

	memcpy(&pp, &par->data_length, sizeof(pp));
	/* Default value */
	pp.sdo_length = 0;

	ret = sdo_do_set_body((void __user *) par->src_p,
			      (void __user *) par->dst_p, &pp);

	par->sdo_length = pp.sdo_length;

	return ret;
}

static int sdo_do_get(ioc_params_t *params_p)
{
	sbb_ioc_sdo_get_t *par = &params_p->sdo_get;
	struct sbb_common_sdo_par pp;
	int ret;

	pp.sdo_length = par->sdo_length;
	pp.owner_info_hi = par->owner_info_hi;
	pp.owner_info_lo = par->owner_info_lo;

	/* Default value */
	pp.data_length = 0;

	ret = sdo_do_get_body((void __user *) par->src_p,
			      (void __user *) par->dst_p, &pp);

	if (!ret) {
		par->object_info = pp.object_info;
		par->data_length = pp.data_length;
	}

	return ret;
}

static int img_do_verify(ioc_params_t *params_p)
{
	sbb_ioc_img_verify_t *par = &params_p->img_verify;
	struct sbb_img_verify_par pp;
	int ret;

	pp.img_length = par->img_length;
	pp.failed = par->failed;

	ret = img_do_verify_body((void __user *)par->img_p, &pp);

	par->failed = pp.failed;

	return ret;
}

static inline u32 ecm_read32(u32 offset)
{
	return readl(sbb_dev_p->ecm_base_address + offset);
}

static inline void ecm_write32(u32 offset, u32 value)
{
	writel(value, sbb_dev_p->ecm_base_address + offset);
}

static int read_ecm(u32 bit_address, u32 num_bits, u32 *out_p)
{
	u32 value;
	int i;
#if !defined(CONFIG_RBS_SIM_ECM_READ_DONE_BUG)
	int tmo = 50;
#endif

	/* Check if valid bit size */
	if ((num_bits != 4) && (num_bits != 128) && (num_bits != 256)) {
		pr_err("SBB ECM Read Error: Invalid size %d\n", num_bits);
		return -EINVAL;
	}
	/* HW expects one less */
	num_bits--;

	/* Verify that ReadEFuse is 0 (bit 0 of the control register). */
	value = ecm_read32(ECM_REG_CTRL);
	if ((value & (ECM_CTRL_READ_EFUSE | ECM_CTRL_PROG_W_DATA)) != 0) {
		pr_err("SBB ECM Read Error: ReadEFuse and/or ProgWriteData is not 0\n");
		return -EBUSY;
	}

	/* Set ClearReadData and PwrDown. Clear PadEnb. */
	ecm_write32(ECM_REG_CTRL, (ECM_CTRL_CLR_R_DATA | ECM_CTRL_PWR_DOWN));

	/* Set ReadAddr 12:0 to the starting bit address. */
	value = ecm_read32(ECM_REG_ADDR);
	value &= ~ECM_ADDR_RW_ADDR;
	value |= (bit_address & ECM_ADDR_RW_ADDR);
	ecm_write32(ECM_REG_ADDR, value);

	/* Clear PwrDown and PadEnb. */
	ecm_write32(ECM_REG_CTRL, 0);

	/* Wait at least 800 ns. */
	udelay(1);

	/* Clear the status bits. */
	ecm_write32(ECM_REG_STAT, ECM_STAT_INTERESING_BITS);

	/* Update ReadDataSize (number of bits to read).
	   Clear PwrDown and PadEnb. Set ReadEFuse. */
	value = ecm_read32(ECM_REG_CTRL);
	value &= ~ECM_CTRL_R_SIZE;
	value |= ECM_CTRL_R_SIZE_SHIFT(num_bits);
	value &= ~ECM_CTRL_PWR_DOWN;
	value |= ECM_CTRL_READ_EFUSE;
	ecm_write32(ECM_REG_CTRL, value);

#if !defined(CONFIG_RBS_SIM_ECM_READ_DONE_BUG)
	/* Wait for LocalReadDone to be set. */
	while (((ecm_read32(ECM_REG_STAT) & ECM_STAT_LOCAL_R_DONE) == 0) &&
	       (!tmo)) {
		tmo--;
		udelay(1);
	}

	if ((ecm_read32(ECM_REG_STAT) & ECM_STAT_LOCAL_R_DONE) == 0) {
		pr_err("SBB: ECM Read error, local read done not asserted\n");
		return -EFAULT;
	}
#endif

	/* Set PwrDown and clear PadEnb. */
	ecm_write32(ECM_REG_CTRL, ECM_CTRL_PWR_DOWN);

	/* Clear LocalReadDone.	*/
	ecm_write32(ECM_REG_STAT, ECM_STAT_LOCAL_R_DONE);

	/* Check the status. */
	value = (ecm_read32(ECM_REG_STAT) & ECM_STAT_INTERESING_BITS);
	if (value != 0) {
		pr_err("SBB ECM Read Error: Status is 0x%08x\n", value);
		return -EFAULT;
	}
	/* Read Values.	*/
	for (i = 0; i <= (num_bits / 32); i++) {
		value = ecm_read32(ECM_REG_RDAT + (4*i));
		if (num_bits == 3)
			*out_p++ = value;
		else
			*out_p++ = swab32(value);
	}

	return 0;
}

static int get_efuses(ioc_params_t *params_p)
{
	int rc;

	memset(&params_p->efuse_get.fuses[0], 0,
	       sizeof(params_p->efuse_get.fuses));
	rc = read_ecm(params_p->efuse_get.address,
			   params_p->efuse_get.length,
			   &params_p->efuse_get.fuses[0]);
	return rc;
}

static int do_sbb_fn(int (*fn)(ioc_params_t *params_p),
		     ioc_params_t *params_p)
{
	int rc;

	/* See comment in sbb_execute_command() */
	do {
		if (sbb_get_lock()) {
			return -EFAULT;
		}
		rc = fn(params_p);
		if (rc == -EAGAIN) {
			pr_notice("SBB Trying to get lock again\n");
		}
	} while (rc == -EAGAIN);
	sbb_unlock();
	return rc;
}

static int sbb_open(struct inode *inode, struct file *filp)
{
	if (! try_module_get(THIS_MODULE))
		return -ENODEV;
	return 0;
}

static int sbb_release(struct inode *inode, struct file *filp)
{
	module_put(THIS_MODULE);
	return 0;
}

static int sbb_get_ioc_params(unsigned int cmd, void __user *arg,
			      ioc_params_t *params_p)
{
	unsigned long len, left;

	switch (cmd) {
	case SBB_IOC_GET_EFUSES:
		len = sizeof(sbb_ioc_efuse_get_t);
		break;
	case SBB_IOC_SET_SDO:
		len = sizeof(sbb_ioc_sdo_set_t);
		break;
	case SBB_IOC_GET_SDO:
		len = sizeof(sbb_ioc_sdo_get_t);
		break;
	case SBB_IOC_VERIFY_IMG:
		len = sizeof(sbb_ioc_img_verify_t);
		break;
	default:
		return -ENOTTY;
		break;
	}
	/* This one does access_ok on the address */
	left = copy_from_user(params_p, arg, len);
	if (left != 0) {
		pr_err("SBB could not get parmeters (%lu left)\n", left);
		return -EFAULT;
	}
	return 0;
}

static int sbb_set_ioc_params(unsigned int cmd, void __user *arg,
			      ioc_params_t *params_p)
{
	unsigned long len, left;

	switch (cmd) {
	case SBB_IOC_GET_EFUSES:
		len = sizeof(sbb_ioc_efuse_get_t);
		break;
	case SBB_IOC_SET_SDO:
		len = sizeof(sbb_ioc_sdo_set_t);
		break;
	case SBB_IOC_GET_SDO:
		len = sizeof(sbb_ioc_sdo_get_t);
		break;
	case SBB_IOC_VERIFY_IMG:
		len = sizeof(sbb_ioc_img_verify_t);
		break;
	default:
		return -ENOTTY;
		break;
	}
	/* This one does access_ok on the address */
	left = copy_to_user(arg, params_p, len);
	if (left != 0) {
		pr_err("SBB could not set parmeters (%lu left)\n",
		       left);
		return -EFAULT;
	}
	return 0;
}

static long sbb_ioctl(struct file *filp, unsigned int cmd, unsigned long arg)
{
	int rc;
	ioc_params_t params = {{0}};
	void __user *argp = (void __user *) arg;

	if ((rc = sbb_get_ioc_params(cmd, argp, &params)))
		return rc;

	if (down_interruptible(&sbb_dev_p->semaphore))
		return -ERESTARTSYS;

	rni_ctrl_set_wfc();

	switch (cmd) {
	case SBB_IOC_GET_EFUSES:
		rc = get_efuses(&params);
		break;
	case SBB_IOC_SET_SDO:
		rc = do_sbb_fn(sdo_do_set, &params);
		break;
	case SBB_IOC_GET_SDO:
		rc = do_sbb_fn(sdo_do_get, &params);
		break;
	case SBB_IOC_VERIFY_IMG:
		rc = do_sbb_fn(img_do_verify, &params);
		break;
	default:
		rc = -ENOTTY; /* Already checked */
		break;
	}

	if (rc)
		sbb_set_ioc_params(cmd, argp, &params);
	else
		rc = sbb_set_ioc_params(cmd, argp, &params);

	rni_ctrl_clear_wfc();
	up(&sbb_dev_p->semaphore);
	return rc;
}

#if defined(CONFIG_COMPAT)
static int sdo_do_compat_set(ioc_params_t *params_p)
{
	struct sbb_compat_sdo_set *par = &params_p->compat_sdo_set;
	struct sbb_common_sdo_par pp;
	int ret;

	memcpy(&pp, &par->data_length, sizeof(pp));
	/* Default value */
	pp.sdo_length = 0;

	ret = sdo_do_set_body(compat_ptr(par->src_p),
			      compat_ptr(par->dst_p), &pp);

	par->sdo_length = pp.sdo_length;

	return ret;
}

static int sdo_do_compat_get(ioc_params_t *params_p)
{
	struct sbb_compat_sdo_get *par = &params_p->compat_sdo_get;
	struct sbb_common_sdo_par pp;
	int ret;

	pp.sdo_length = par->sdo_length;
	pp.owner_info_hi = par->owner_info_hi;
	pp.owner_info_lo = par->owner_info_lo;
	pp.object_info = par->object_info;

	/* Default value */
	pp.data_length = 0;

	ret = sdo_do_get_body(compat_ptr(par->src_p),
			      compat_ptr(par->dst_p), &pp);

	if (!ret) {
		par->object_info = pp.object_info;
		par->data_length = pp.data_length;
	}

	return ret;
}

static int img_do_compat_verify(ioc_params_t *params_p)
{
	struct sbb_compat_img_verify *par = &params_p->compat_img_verify;
	struct sbb_img_verify_par pp;
	int ret;

	pp.img_length = par->img_length;
	pp.failed = par->failed;

	ret = img_do_verify_body(compat_ptr(par->img_p), &pp);

	par->failed = pp.failed;

	return ret;
}

static int sbb_get_compat_params(unsigned int cmd, void __user *arg,
				 ioc_params_t *params_p)
{
	unsigned long len, left;

	switch (cmd) {
	case SBB_IOC_GET_EFUSES:
		len = sizeof(sbb_ioc_efuse_get_t);
		break;
	case SBB_IOC_SET_SDO:
		len = sizeof(struct sbb_compat_sdo_set);
		break;
	case SBB_IOC_GET_SDO:
		len = sizeof(struct sbb_compat_sdo_get);
		break;
	case SBB_IOC_VERIFY_IMG:
		len = sizeof(struct sbb_compat_img_verify);
		break;
	default:
		return -ENOTTY;
		break;
	}
	/* This one does access_ok on the address */
	left = copy_from_user(params_p, arg, len);
	if (left != 0) {
		pr_err("SBB could not get parmeters (%lu left)\n", left);
		return -EFAULT;
	}
	return 0;
}

static int sbb_set_compat_params(unsigned int cmd, void __user *arg,
				 ioc_params_t *params_p)
{
	unsigned long len, left;

	switch (cmd) {
	case SBB_IOC_GET_EFUSES:
		len = sizeof(sbb_ioc_efuse_get_t);
		break;
	case SBB_IOC_SET_SDO:
		len = sizeof(struct sbb_compat_sdo_set);
		break;
	case SBB_IOC_GET_SDO:
		len = sizeof(struct sbb_compat_sdo_get);
		break;
	case SBB_IOC_VERIFY_IMG:
		len = sizeof(struct sbb_compat_img_verify);
		break;
	default:
		return -ENOTTY;
		break;
	}
	/* This one does access_ok on the address */
	left = copy_to_user(arg, params_p, len);
	if (left != 0) {
		pr_err("SBB could not set parmeters (%lu left)\n",
		       left);
		return -EFAULT;
	}
	return 0;
}

static long sbb_compat_ioctl(struct file *filp, unsigned int cmd,
			     unsigned long arg)
{
	int rc;
	ioc_params_t params = {{0}};
	void __user *argp = (void __user *) arg;

	if ((rc = sbb_get_compat_params(cmd, argp, &params)))
		return rc;

	if (down_interruptible(&sbb_dev_p->semaphore))
		return -ERESTARTSYS;

	rni_ctrl_set_wfc();

	switch (cmd) {
	case SBB_IOC_GET_EFUSES:
		rc = get_efuses(&params);
		break;
	case SBB_IOC_SET_SDO:
		rc = do_sbb_fn(sdo_do_compat_set, &params);
		break;
	case SBB_IOC_GET_SDO:
		rc = do_sbb_fn(sdo_do_compat_get, &params);
		break;
	case SBB_IOC_VERIFY_IMG:
		rc = do_sbb_fn(img_do_compat_verify, &params);
		break;
	default:
		rc = -ENOTTY; /* Already checked */
		break;
	}

	if (rc)
		sbb_set_compat_params(cmd, argp, &params);
	else
		rc = sbb_set_compat_params(cmd, argp, &params);

	rni_ctrl_clear_wfc();
	up(&sbb_dev_p->semaphore);
	return rc;
}
#endif

static int sbb_driver_init(void)
{
	int rc;
	dev_t dev;
	struct device_node *np;
	u32 freq, cycles, cycles_ms;

	/* Setup the dvice structures */
	sbb_dev_p = kzalloc(sizeof(*sbb_dev_p), GFP_KERNEL);
	if (sbb_dev_p == NULL) {
		pr_err("SBB Could not create SBB dev\n");
		return -ENOMEM;
	}
	sbb_dev_p->major_number = CONFIG_RBS_SBB_DRIVER_MAJOR_NUMBER;
	sbb_dev_p->minor_number = CONFIG_RBS_SBB_DRIVER_MINOR_NUMBER;
	sbb_dev_p->sbb_base_address = NULL;
	sbb_dev_p->ecm_base_address = NULL;
	sbb_dev_p->fopts.owner = THIS_MODULE;
	sbb_dev_p->fopts.unlocked_ioctl = sbb_ioctl;
#if defined(CONFIG_COMPAT)
	sbb_dev_p->fopts.compat_ioctl = sbb_compat_ioctl;
#endif
	sbb_dev_p->fopts.open = sbb_open;
	sbb_dev_p->fopts.release = sbb_release;

	/* Set/get driver (major) number and allocate device (minor)
	   numbers for it */
	if (sbb_dev_p->major_number) {
		sbb_dev_p->driver = MKDEV(sbb_dev_p->major_number,
					  sbb_dev_p->minor_number);
		rc = register_chrdev_region(sbb_dev_p->driver,
					    DRIVER_NUM_DEVICES,
					    DRIVER_NAME);
	} else {
		rc = alloc_chrdev_region(&sbb_dev_p->driver,
					 sbb_dev_p->minor_number,
					 DRIVER_NUM_DEVICES, DRIVER_NAME);
	}
	if (rc < 0) {
		pr_err("SBB Could not register driver\n");
		goto error_free_sbb;
	}
	sbb_dev_p->major_number = MAJOR(sbb_dev_p->driver);
	sbb_dev_p->minor_number = MINOR(sbb_dev_p->driver);

	/* Create a device class */
	sbb_dev_p->class_p = class_create(THIS_MODULE, "rbs_sdo_class");
	if (sbb_dev_p->class_p == NULL) {
		pr_err("SBB Could not create class\n");
		rc = -1;
		goto error_unreg_dev;
	}

	/* Setup the semaphore as a mutex */
	sema_init(&sbb_dev_p->semaphore, 1);

	/* Get IO address */
	sbb_dev_p->sbb_base_address = ioremap_nocache(SBB_PHYSICAL_ADDRESS,
						      SBB_PHYSICAL_SIZE);
	sbb_dev_p->ecm_base_address = ioremap_nocache(ECM_PHYSICAL_ADDRESS,
						      ECM_PHYSICAL_SIZE);
	if (sbb_dev_p->sbb_base_address == NULL ||
	    sbb_dev_p->ecm_base_address == NULL) {
		pr_err("SBB Could not map pysical address\n");
		rc = -1;
		goto error_unmap_io;
	}

	/* Add as char device */
	cdev_init(&sbb_dev_p->cdev, &sbb_dev_p->fopts);
	sbb_dev_p->cdev.owner = THIS_MODULE;
	sbb_dev_p->cdev.ops   = &sbb_dev_p->fopts;
	dev = MKDEV(sbb_dev_p->major_number,
		    sbb_dev_p->minor_number + SBB_MINOR_OFFSET);
	rc = cdev_add(&sbb_dev_p->cdev, dev, 1);
	if (rc < 0) {
		pr_err("SBB Could not create SBB device\n");
		goto error_unmap_io;
	}
	/* And create a sbb device node for it */
	if (device_create(sbb_dev_p->class_p, NULL, dev, NULL, "sbb") == NULL) {
		pr_err("SBB Could not create sbb node\n");
		rc = -1;
		goto error_del_cdev;
	}
	/* calculate max delay for function completion
	 * DS just states - time depends on data size, but not how.
	 * re-use algorithm used in uboot:
	 *
	 * Max cycles required by the SBB
	 * 2 * 10^6 + (image_size_in_bits / 128 * 25) + C clock cycles.
	 * Where C is less than 10^6.
	 */
	np = of_find_node_by_path("/clocks/cpu");
	if (!np || of_property_read_u32(np, "frequency", &freq)) {
		sbb_dev_p->max_sdo_poll = MAX_SDO_POLL;
	} else {
		cycles = 3000000ULL + ((SDO_MAX_LENGTH/16) * 25);
		cycles_ms = freq/1000;
		sbb_dev_p->max_sdo_poll = (cycles + cycles_ms)/cycles_ms;
		sbb_dev_p->max_sdo_poll *= 2;
	}
	return 0;

error_del_cdev:
	cdev_del(&sbb_dev_p->cdev);
error_unmap_io:
	if (sbb_dev_p->sbb_base_address != NULL)
		iounmap(sbb_dev_p->sbb_base_address);

	if (sbb_dev_p->ecm_base_address != NULL)
		iounmap(sbb_dev_p->ecm_base_address);

	class_destroy(sbb_dev_p->class_p);
error_unreg_dev:
	unregister_chrdev_region(sbb_dev_p->driver, DRIVER_NUM_DEVICES);
error_free_sbb:
	kfree(sbb_dev_p);
	return rc;
}

static void sbb_driver_exit(void)
{
	/* Remove char device, unregister driver */
	cdev_del(&sbb_dev_p->cdev);
	iounmap(sbb_dev_p->sbb_base_address);
	iounmap(sbb_dev_p->ecm_base_address);
	unregister_chrdev_region(sbb_dev_p->driver, DRIVER_NUM_DEVICES);
	class_destroy(sbb_dev_p->class_p);
	kfree(sbb_dev_p);
}

module_init(sbb_driver_init);
module_exit(sbb_driver_exit);


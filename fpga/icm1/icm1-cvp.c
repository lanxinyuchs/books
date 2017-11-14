/*
 * CVP - ICM1 Driver
 *
 * Copyright 2013 Ericsson AB
 * Andrey Panteleev <andrey.xx.panteleev@ericsson.com>
 *
 * Based on 
 * altera_cvp.c -- driver for configuring Altera FPGAs via CvP
 *
 * Written by: Andres Cassinelli <acassine@altera.com>
 *             Altera Corporation
 *
 * Copyright (C) 2012 Altera Corporation. All Rights Reserved.
 *
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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/device.h> /* dev_err(), etc. */
#include <linux/pci.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/slab.h>  /* kmalloc */
#include <linux/vmalloc.h>
#include <linux/delay.h>

#include "icm1.h"

#define NUM_VSEC_REGS		17 /* number of VSEC registers for CvP */
#define BYTES_IN_REG		4 /* number of bytes in each VSEC register */

#define ERR_CHK_INTERVAL	1024  /* only check for CRC errors every this
					 many 32-bit words */

#define MAX_WAIT_MS             500 /* Time (ms) to wait for a bit value
				       before timeout */
#define BIT_POLL_INTERVAL_MS    20  /* Poll interval (ms) to check for
				       bit value */
#define OFFSET_VSEC		0x200 /* byte offset of VSEC register block
					 for CvP */
#define OFFSET_CVP_STATUS	0x1E /* byte offsets of registers within VSEC */
#define OFFSET_CVP_MODE_CTRL	0x20
#define OFFSET_CVP_NUMCLKS	0X21
#define OFFSET_CVP_DATA		0x28
#define OFFSET_CVP_PROG_CTRL	0x2C
#define OFFSET_UNC_IE_STATUS	0x34

#define MASK_DATA_ENCRYPTED	0x01 /* bit 0 of CVP_STATUS */
#define MASK_DATA_COMPRESSED	0x02 /* bit 1 of CVP_STATUS */
#define MASK_CVP_CONFIG_READY	0x04 /* bit 2 of CVP_STATUS */
#define MASK_CVP_CONFIG_ERROR	0x08 /* bit 3 of CVP_STATUS */
#define MASK_CVP_EN		0x10 /* bit 4 of CVP_STATUS */
#define MASK_USER_MODE		0x20 /* bit 5 of CVP_STATUS */
#define MASK_PLD_CLK_IN_USE	0x01 /* bit 8 of CVP_STATUS (bit 0 of byte
					@ CVP_STATUS+1) */
#define MASK_CVP_MODE		0x01 /* bit 0 of CVP_MODE_CTRL */
#define MASK_HIP_CLK_SEL	0X02 /* bit 1 of CVP_MODE_CTRL */
#define MASK_CVP_CONFIG		0x01 /* bit 0 of CVP_PROG_CTRL */
#define MASK_START_XFER		0x02 /* bit 1 of CVP_PROG_CTRL */
#define MASK_CVP_CFG_ERR_LATCH	0x20 /* bit 5 of UNC_IE_STATUS */

/* CvP bits */
enum {  
	DATA_ENCRYPTED = 0,
	DATA_COMPRESSED,
	CVP_CONFIG_READY,
	CVP_CONFIG_ERROR,
	CVP_EN,
	USER_MODE,
	PLD_CLK_IN_USE,
	CVP_MODE,
	HIP_CLK_SEL,
	CVP_CONFIG,
	START_XFER,
	CVP_CFG_ERR_LATCH
};

struct cvp_dev {
	struct pci_dev *pdev;
	void __iomem *wr_addr;

	u8 remain[3];
	char remain_size;
};

static int altera_cvp_get_offset_and_mask(int bit, int *byte_offset, u8 *mask)
{
	switch (bit) {
	case DATA_ENCRYPTED:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_STATUS;
		*mask = MASK_DATA_ENCRYPTED;
		break;
	case DATA_COMPRESSED:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_STATUS;
		*mask = MASK_DATA_COMPRESSED;
		break;
	case CVP_CONFIG_READY:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_STATUS;
		*mask = MASK_CVP_CONFIG_READY;
		break;
	case CVP_CONFIG_ERROR:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_STATUS;
		*mask = MASK_CVP_CONFIG_ERROR;
		break;
	case CVP_EN:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_STATUS;
		*mask = MASK_CVP_EN;
		break;
	case USER_MODE:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_STATUS;
		*mask =  MASK_USER_MODE;
		break;
	case PLD_CLK_IN_USE:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_STATUS + 1;
		*mask = MASK_PLD_CLK_IN_USE;
		break;
	case CVP_MODE:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_MODE_CTRL;
		*mask = MASK_CVP_MODE;
		break;
	case HIP_CLK_SEL:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_MODE_CTRL;
		*mask = MASK_HIP_CLK_SEL;
		break;
	case CVP_CONFIG:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_PROG_CTRL;
		*mask = MASK_CVP_CONFIG;
		break;
	case START_XFER:
		*byte_offset = OFFSET_VSEC + OFFSET_CVP_PROG_CTRL;
		*mask = MASK_START_XFER;
		break;
	case CVP_CFG_ERR_LATCH:
		*byte_offset = OFFSET_VSEC + OFFSET_UNC_IE_STATUS;
		*mask = MASK_CVP_CFG_ERR_LATCH;
		break;
	default:
		return -EINVAL;
	}
	return 0;
}

static int altera_cvp_read_bit(struct cvp_dev *cvp, int bit, u8 *value)
{
	int byte_offset;
	u8 byte_val, byte_mask;
	if (altera_cvp_get_offset_and_mask(bit, &byte_offset, &byte_mask))
		return -EINVAL;
	if (pci_read_config_byte(cvp->pdev, byte_offset, &byte_val))
		return -EAGAIN;
	*value = (byte_val & byte_mask) ? 1 : 0;
	return 0;
}

static int altera_cvp_write_bit(struct cvp_dev *cvp, int bit, u8 value)
{
	int byte_offset;
	u8 byte_val, byte_mask;

	switch (bit) {
	case CVP_MODE:
	case HIP_CLK_SEL:
	case CVP_CONFIG:
	case START_XFER:
	case CVP_CFG_ERR_LATCH:
		altera_cvp_get_offset_and_mask(bit, &byte_offset, &byte_mask);
		pci_read_config_byte(cvp->pdev, byte_offset, &byte_val);
		byte_val = value ?
			(byte_val | byte_mask) : (byte_val & ~byte_mask);
		pci_write_config_byte(cvp->pdev, byte_offset, byte_val);
		return 0;
	default:
		return -EINVAL; /* only the bits above are writeable */
	}
} 

static int altera_cvp_set_num_clks(struct cvp_dev *cvp, int num_clks)
{
	if (num_clks < 1 || num_clks > 64)
		return -EINVAL;
	if (num_clks == 64)
		num_clks = 0x00;
	return (pci_write_config_byte(cvp->pdev,
				OFFSET_VSEC + OFFSET_CVP_NUMCLKS,
				num_clks));
}

#define NUM_REG_WRITES 244
#define DUMMY_VALUE 0x00000000
/**
 * altera_cvp_switch_clk() - switch between CvP clock and internal clock
 *
 * Issues dummy memory writes to the PCIe HIP, allowing the Control Block to
 * switch between the HIP's CvP clock and the internal clock.
 */
static int altera_cvp_switch_clk(struct cvp_dev *cvp)
{
	int i;
	altera_cvp_set_num_clks(cvp, 1);
	for (i = 0; i < NUM_REG_WRITES; i++) {
		iowrite32(DUMMY_VALUE, cvp->wr_addr);
	}
	return 0;
}

static int altera_cvp_set_data_type(struct cvp_dev *cvp)
{
	int error, num_clks;
	u8 compr, encr;

	if ((error = altera_cvp_read_bit(cvp, DATA_COMPRESSED, &compr)) ||
	    (error = altera_cvp_read_bit(cvp, DATA_ENCRYPTED, &encr)))
		return error;

	if (compr)
		num_clks = 8;
	else if (encr)
		num_clks = 4;
	else
		num_clks = 1;

	return (altera_cvp_set_num_clks(cvp, num_clks));
}

static int altera_cvp_send_data(struct cvp_dev *cvp, u32 *data,
		unsigned long num_words)
{
#ifdef DEBUG
	u8 bit_val;
	unsigned int i;
	for (i = 0; i < num_words; i++) {
		iowrite32(data[i], cvp->wr_addr);
		if (i + 1 % ERR_CHK_INTERVAL == 0) {
			altera_cvp_read_bit(cvp, CVP_CONFIG_ERROR, &bit_val);
			if (bit_val) {
				dev_err(&cvp->pdev->dev,
						"CB detected a CRC error "
						"between words %d and %d\n",
						i + 1 - ERR_CHK_INTERVAL,
						i + 1);
				return -EAGAIN;
			}
		}
	}
	dev_info(&cvp->pdev->dev, "A total of %ld 32-bit words were "
			"sent to the FPGA\n", num_words);
#else
	iowrite32_rep(cvp->wr_addr, data, num_words);
#endif /* DEBUG */
	return 0;
}

/* Polls the requested bit until it has the specified value (or until timeout) */
/* Returns 0 once the bit has that value, error code on timeout */
static int altera_cvp_wait_for_bit(struct cvp_dev *cvp, int bit, u8 value)
{
	unsigned long poll_interval_jiffies = msecs_to_jiffies(BIT_POLL_INTERVAL_MS);
	unsigned long wait_ms = 0;
	u8 bit_val;

	altera_cvp_read_bit(cvp, bit, &bit_val);
	while (bit_val != value) {
		if (wait_ms > MAX_WAIT_MS) {
			dev_err(&cvp->pdev->dev, "Timed out while "
				"polling bit %d\n", bit);
			return -EAGAIN;
		}
		wait_ms += jiffies_to_msecs(poll_interval_jiffies -
					    schedule_timeout_interruptible(poll_interval_jiffies));
		altera_cvp_read_bit(cvp, bit, &bit_val);
	}
	dev_info(&cvp->pdev->dev, "Bit %d wait time %lu ms\n",
		 bit, wait_ms);

	return 0;
}

static int altera_cvp_setup(struct cvp_dev *cvp)
{
	dev_info(&cvp->pdev->dev, "Starting CvP...\n");

	msleep(10);
	altera_cvp_write_bit(cvp, HIP_CLK_SEL, 1);
	msleep(10);
	altera_cvp_write_bit(cvp, CVP_MODE, 1);
	altera_cvp_switch_clk(cvp); /* allow CB to sense if system reset
				       is issued */
	altera_cvp_write_bit(cvp, CVP_CONFIG, 1); /* request CB to begin 
						     CvP transfer */
	/* wait until CB is ready */
	if (altera_cvp_wait_for_bit(cvp, CVP_CONFIG_READY, 1))
		return -EAGAIN;

	altera_cvp_switch_clk(cvp);
	altera_cvp_write_bit(cvp, START_XFER, 1);
	altera_cvp_set_data_type(cvp);
	return 0; /* success */
}

static int altera_cvp_teardown(struct cvp_dev *cvp, int abort)
{
	u8 bit_val;

	if (abort)
		cvp->remain_size = 0;

	/* if necessary, flush remainder buffer */
	if (cvp->remain_size > 0) {
		u32 last_word = 0;
		memcpy(&last_word, cvp->remain, cvp->remain_size);
		altera_cvp_send_data(cvp, &last_word, 1);
	}

	altera_cvp_write_bit(cvp, START_XFER, 0);
	/* request CB to end CvP transfer */
	altera_cvp_write_bit(cvp, CVP_CONFIG, 0);
	altera_cvp_switch_clk(cvp);


	altera_cvp_write_bit(cvp, CVP_MODE, 0);
	msleep(10);
	altera_cvp_write_bit(cvp, HIP_CLK_SEL, 0);
	msleep(10);
	
	if (abort)
		return 0;

	/* wait until CB is ready */
	if (altera_cvp_wait_for_bit(cvp, CVP_CONFIG_READY, 0))
		return -EAGAIN;

	altera_cvp_read_bit(cvp, CVP_CFG_ERR_LATCH, &bit_val);
	if (bit_val) {
		dev_err(&cvp->pdev->dev, "Configuration error detected, "
				"CvP has failed\n");
		/* clear error bit */
		altera_cvp_write_bit(cvp, CVP_CFG_ERR_LATCH, 1); 
		return -EFAULT;
	}

	/* wait for application layer to be ready */
	altera_cvp_wait_for_bit(cvp, PLD_CLK_IN_USE, 1);
	if (altera_cvp_wait_for_bit(cvp, USER_MODE, 1))
		return -EAGAIN;
	dev_info(&cvp->pdev->dev, "CvP successful, application "
			"layer ready\n");
	return 0; /* success */
}

struct cvp_dev *icm1_cvp_init(struct pci_dev *pdev, void __iomem *wr_addr)
{
	struct cvp_dev *cvp;
	int ret;

	cvp = kmalloc(sizeof(*cvp), GFP_KERNEL);
	if (!cvp)
		return ERR_PTR(-ENOMEM);

	cvp->pdev = pdev;
	cvp->wr_addr = wr_addr;
	cvp->remain_size = 0;

	ret = altera_cvp_setup(cvp);
	if (ret) {
		dev_err(&cvp->pdev->dev, "Unable to setup CvP\n");
		return ERR_PTR(ret);
	}

	return cvp;
}

int icm1_cvp_send(struct cvp_dev *cvp, char *buf, int len)
{
	char *rbuf;
	int rlen, ret;
	int realloc = 0;

	rlen = len + cvp->remain_size;

	if (cvp->remain_size) {
		realloc = 1;
		rbuf = vmalloc(len + cvp->remain_size);
		if (!rbuf)
			return -ENOMEM;
		memcpy(rbuf, cvp->remain, cvp->remain_size);
		memcpy(rbuf + cvp->remain_size, buf, len);
	} else
		rbuf = buf;

	cvp->remain_size = rlen % 4;

	if (cvp->remain_size)
		memcpy(cvp->remain, rbuf + (rlen - cvp->remain_size),
				cvp->remain_size);

	ret = altera_cvp_send_data(cvp, (u32 *) rbuf, rlen / 4);
	if (ret) {
		dev_err(&cvp->pdev->dev, "Unable to send image\n");
	}

	if (realloc)
		vfree(rbuf);
	return ret;
}

int icm1_cvp_check_status(struct cvp_dev *cvp)
{
	u8 bit_val = 1;

	altera_cvp_read_bit(cvp, CVP_CONFIG_ERROR, &bit_val);

	return bit_val ? 1 : 0;
}

int icm1_cvp_finish(struct cvp_dev *cvp, int abort)
{
	int ret;

	ret = altera_cvp_teardown(cvp, abort);

	kfree(cvp);
	return ret;
}



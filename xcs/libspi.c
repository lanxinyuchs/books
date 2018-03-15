/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <pthread.h>

#include <itc.h>
#include <getopt.h>
#include <uio_helper.h>
#include <semaphore.h>

#include "libspi.h"

#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_libspi
#include "tpt_create.h"
#include "tpt.h"
/*
 * SPI clock rate (CTRL RATE).
 * SPI clock rate = 245.72 MHz / (2* (CTRL_RATE +1))
 */
#define CTRL_RATE_MASK     0x3fff0000
#define CTRL_RATE_POS      16

/*
 * SPI Clock idle (CTRL CLK_IDLE).
 * Half Clock Cycles Idle Time First and Last bit and in Bidir Shift Phase
 * 0 One half cycle
 * 1 Two half cycle
*/
#define CTRL_CLK_IDLE_MASK 0x00001000
#define CTRL_CLK_IDLE_POS  12

/*
 * SPI Slave Select (CTRL SS).
 * This bit sets the Slave Select signal if the CTRL_SS_MODE is set to
 * manual.
 * 0 Slave Select is inactive (low)
 * 1 Slave Select is active (high)
 */
#define CTRL_SS_MASK       0x00000200
#define CTRL_SS_POS        9

/*
 * Slave Select bidir mode (CTRL SS_BIDIR).
 * 0 Normal mode. Slave select active during whole SPI transfer
 * 1 Switch mode. Slave select inactivates at direction switch.
 */
#define CTRL_SS_BIDIR_MASK 0x0000100
#define CTRL_SS_BIDIR_POS  8

/*
 * Slave Select mode (CTRL SS_MODE).
 * 0 No automatic Slave Select generation
 * 1 Automatic Slave Select generation
 */
#define CTRL_SS_MODE_MASK  0x0000080
#define CTRL_SS_MODE_POS   7

/*
 * SPI Enable (CTRL EN).
 * 0 SPI communication is disabled
 * 1 SPI communication is enabled
 */
#define CTRL_EN_MASK       0x0000020
#define CTRL_EN_POS        5

/*
 * LSB First Enable (CTRL LSBFE).
 * 0 Send each 32 bits word with MSB bit first
 * 1 Send each 32 bits word with LSB bit first
 */
#define CTRL_LSBFE_MASK    0x0000010
#define CTRL_LSBFE_POS     4

/*
 * SPI Clock Phase bit (CTRL CPHA).
 * 0 Sampling on leading edges
 * 1 Sampling on trailing edges
 */
#define CTRL_CPHA_MASK     0x00000008
#define CTRL_CPHA_POS      3

/*
 * SPI Clock polarity bit (CTRL CPOL).
 * 0 In idle state spi_m_clk is low
 * 1 In idle state spi_m_clk is high
 */
#define CTRL_CPOL_MASK     0x00000004
#define CTRL_CPOL_POS      2

/*
 * SPI command mode (CTRL MODE).
 * 00 Write
 * 01 Read
 * 10 Bidir
 * 11 Duplex
 */
#define CTRL_MODE_MASK     0x00000003
#define CTRL_MODE_POS      0

/*
 * Select which SPI port to use (HUB PORT).
 * Range is 0 to 11.
 */
#define HUB_PORT_MAX      2
#define HUB_PORT_MASK     0x000000C0
#define HUB_PORT_POS      6

/*
 * Selects if 3 or 4 wire communication to slave (HUB 4W).
 * 0 = 3-wire SPI.
 * 1 = 4-wire SPI.
 */
#define HUB_4W_MASK     0x00000004
#define HUB_4W_POS      2

/*
 * Selects if spi clock should be inverted (HUB CLK_INV).
 * 0 = SPI_CLK is unchanged.
 * 1 = Invert  SPI_CLK.
 */
#define HUB_CLK_INV_MASK     0x00000002
#define HUB_CLK_INV_POS      1

/*
 * Selects if slave select should be inverted (HUB SS_HIGH).
 * 0 = Slave Select is active low
 * 1 = Slave Select is active high
 */
#define HUB_SS_HIGH_MASK     0x00000001
#define HUB_SS_HIGH_POS      0

/* SPI_LENGTH, bit positions in register. */
#define LENGTH_DIR_POS       16 /* SPI length until direction shift    */
/* (0 - 255). Only used in bidir mode. */
/* Number of bits  is DIR+1            */
#define LENGTH_VAL_POS       0  /* SPI transfer length (0 - 255).      */
/* Number of bits is VAL+1             */

/* CMD */
#define CMD_RESET            0x2   /* Reset SPI controller. */
#define CMD_STRB             0x1   /* Start transfer. */

/* SPI_STATUS */
#define STATUS_ACT           0x80 /* An SPI command is processed.      */
#define STATUS_RX_OR         0x40 /* RX Overrun flag.                  */
#define STATUS_RX_AVAIL      0x20 /* Data is available in the RX fifo. */
#define STATUS_RX_FULL       0x10 /* RX fifo is full.                  */
#define STATUS_TX_UR         0x08 /* TX underrun flag.                 */
#define STATUS_TX_FULL       0x04 /* TX fifo is full.                  */
#define STATUS_TX_EMPTY      0x02 /* TX fifo is empty.                 */
#define STATUS_DONE          0x01 /* An SPI command is done.           */

#define LENGTH_VAL_MAX       256  /* Maximum number of bits in one burst. */

/* Pointers to SPI registers */
struct spi_reg {
	uint32_t ctrl;        /* 0x00 */
	uint32_t length;      /* 0x04 */
	uint32_t tx;          /* 0x08 */
	uint32_t rx;          /* 0x0C */
	uint32_t cmd;         /* 0x10 */
	uint32_t status;      /* 0x14 */
	uint32_t status_trap;  /* 0x18 */
	uint32_t status_mask;  /* 0x1C */
	uint32_t status_force; /* 0x20 */
	uint32_t status_debug; /* 0x24 */
	uint32_t status_trig;  /* 0x28 */
	uint32_t space[9];    /* 0x2C-0x4C */
	uint32_t hubSs;       /* 0x50 */
	uint32_t hub[16];     /* 0x54-0xC8 */
};

struct spi_dev {
	uint32_t unit;
	bool         in_use;
	uint32_t     ctrl_reg; /* Units copy of CTRL register*/
	struct spi_master *master;
};

struct spi_master {
	struct {
		/* Transmitting unit.  */
		uint32_t unit;
		/* Indicates if error occurred. */
		uint32_t err;
		struct {
			/* Number of elements in vector.*/
			uint32_t count;
			/* Pointer to array of buffers and buffer lengths. */
			const struct spi_buffer_des *vector;
			/* current index in vector array.*/
			uint32_t index;
			/* Pointer to transmitted byte in
			   vector[index].base_addr.*/
			uint8_t *buffer;
			/* Number of bytes left to process in
			   vector[index].base_addr.*/
			uint32_t length;

		} iov;

		struct {
			/* Number of bits in this write operation   */
			uint32_t num_bits;
			/* Number of bits left to write*/
			uint32_t num_bits_left;
			struct {
				uint32_t count;
				const struct spi_buffer_des *vector;
				uint32_t index;

			} iov;
		} wr;

		struct {
			/* Number of bits in this read operation   */
			uint32_t num_bits_left;
			/* Number of bits left to read*/
			uint32_t num_aligned_bits_left;
			/* Number of bits to read from fifo*/
			uint32_t num_bits_from_fifo;
			struct {
				struct spi_buffer_des *vector;
				uint32_t count;
				uint32_t index;
				uint32_t *buffer;
				uint32_t length;

			} iov;
		} rd;
	} pnd_tr; /*pending transfer operation*/

	pthread_mutex_t mutex;
	pthread_cond_t cond;

	volatile struct spi_reg *spi_reg;
	struct spi_dev dev[SPI_SLAVES_PER_DEV];

};

static bool spi_handle_transfer(struct spi_dev *dev);
static bool spi_handle_transfer_done(struct spi_dev *dev);
void spi_get_aligned_rd_bits(struct spi_master *master);
void check_result(const char *format, int err_code);

#ifdef SPI_DEBUG
#define DEBUG_TRACE(handle, func) debug_trace(handle, func)
static void debug_trace(struct spi_dev *handle, const char *func)
{
	struct spi_master *master = NULL;
	master = handle->master;
	TPT_INFO(STR("spi_block(%s): register (value): ctrl (0x%x) "
	             "length (0x%x), tx (0x%x), rx (0x%x), "
	             "cmd (0x%x),status(0x%x), status_trap (0x%x), status_mask (0x%x), hubSs(0x%x), hub(0x%x)",
	             func, master->spi_reg->ctrl, master->spi_reg->length, master->spi_reg->tx,
	             master->spi_reg->rx,
	             master->spi_reg->cmd, master->spi_reg->status, master->spi_reg->status_trap,
	             master->spi_reg->status_mask,
	             master->spi_reg->hubSs, master->spi_reg->hub[handle->unit]));

}
#else /* SPI_DEBUG */
#define DEBUG_TRACE(handle, func) ((void) 0)
#endif /* SPI_DEBUG */

void check_result(const char *format, int err_code)
{
	if(err_code != 0) {
		TPT_ERROR(STR("%s failed with error %d", format, err_code));
		abort();
	}
}

/**
 * open a spi slave device.
 */
struct spi_dev *spi_open(struct spi_master *handle, uint32_t unit)
{
	struct spi_master *master = NULL;
	struct spi_dev  *device = NULL;

	if(handle == NULL) {
		return NULL;
	}

	if (unit >= SPI_SLAVES_PER_DEV) {
		return NULL;
	}
	master = handle;
	device = &master->dev[unit];

	if (device->in_use) {
		return NULL;
	}

	device->unit = unit;

	device->in_use = true;

	device->ctrl_reg = CTRL_RATE_MASK | CTRL_SS_MODE_MASK |
	                   CTRL_EN_MASK;
	device->master = master;

	master->spi_reg->hub[unit] = 0;

	return device;
}

/**
 * close a spi slave device.
 */
uint32_t spi_close(struct spi_dev *handle)
{
	struct spi_dev  *dev = NULL;

	if(handle == NULL) {
		return SPI_EINVALID_PARA;
	}

	dev = handle;

	if (!dev->in_use) {
		return SPI_EWRONG_STATE;
	}
	dev->in_use = false;

	return SPI_SUCCESS;
}
/**
 * get the configuration of  a spi slave device.
 */
uint32_t spi_get_conf(struct spi_dev *handle, int32_t *tagList)
{
	struct spi_master  *master = NULL;
	struct spi_dev  *dev = NULL;

	if((handle == NULL) || (tagList == NULL)) {
		return SPI_EINVALID_PARA;
	}

	dev = handle;
	master = dev->master;

	if (!dev->in_use) {
		return SPI_EWRONG_STATE;
	}

	while (*tagList != SPI_TAGC_TAGEND) {
		switch (*tagList) {
		case SPI_TAGC_BITRATE:
			tagList[1] = (int32_t)
			             (SPI_CLK_IN_HZ / (2 *
			                               (((dev->ctrl_reg & CTRL_RATE_MASK)
			                                 >> CTRL_RATE_POS) + 1)));
			break;
		case SPI_TAGC_RX_DELAY:
			tagList[1] = (int32_t)
			             ((dev->ctrl_reg & CTRL_CLK_IDLE_MASK) ?
			              SPI_RX_DELAY_ONE_CYCLE :
			              SPI_RX_DELAY_HALF_CYCLE);
			break;
		case SPI_TAGC_AUTO_SS:
			tagList[1] = (int32_t)
			             ((dev->ctrl_reg & CTRL_SS_MODE_MASK) ?
			              SPI_AUTO_SS_ON : SPI_AUTO_SS_OFF);
			break;
		case SPI_TAGC_SEND_FIRST:
			tagList[1] = (int32_t)
			             ((dev->ctrl_reg & CTRL_LSBFE_MASK) ?
			              SPI_SEND_FIRST_LSB : SPI_SEND_FIRST_MSB);
			break;
		case SPI_TAGC_PHASE:
			tagList[1] = (int32_t)
			             ((dev->ctrl_reg & CTRL_CPHA_MASK) ?
			              SPI_PHASE_TRAILING : SPI_PHASE_LEADING);
			break;
		case SPI_TAGC_POLARITY:
			tagList[1] = (int32_t)
			             ((dev->ctrl_reg & CTRL_CPOL_MASK) ?
			              SPI_POLARITY_HIGH : SPI_POLARITY_LOW);
			break;
		case SPI_TAGC_INVERT_CLK:
			tagList[1] = (int32_t)
			             ((master->spi_reg->hub[dev->unit] & HUB_CLK_INV_MASK) ?
			              SPI_INVERT_CLK_ON : SPI_INVERT_CLK_OFF);
			break;
		case SPI_TAGC_SS:
			tagList[1] = (int32_t)
			             ((dev->ctrl_reg & CTRL_SS_MASK) ?
			              SPI_SS_HIGH : SPI_SS_LOW);
			break;
		case SPI_TAGC_SS_BIDIR:
			tagList[1] = (int32_t)
			             ((dev->ctrl_reg & CTRL_SS_BIDIR_MASK) ?
			              SPI_SS_BIDIR_SWITCH : SPI_SS_BIDIR_NORMAL);
			break;
		case SPI_TAGC_PORT:
			tagList[1] = (int32_t)
			             ((master->spi_reg->hub[dev->unit] &
			               HUB_PORT_MASK) >> HUB_PORT_POS);
			break;
		case SPI_TAGC_WIRE:
			tagList[1] = (int32_t)
			             ((master->spi_reg->hub[dev->unit] & HUB_4W_MASK) ?
			              SPI_WIRE_4 : SPI_WIRE_3);
			break;
		case SPI_TAGC_SS_POL:
			tagList[1] = (int32_t)
			             ((master->spi_reg->hub[dev->unit] &
			               HUB_SS_HIGH_MASK) ? SPI_SS_POL_HIGH : SPI_SS_POL_LOW);
			break;
		default:
			return SPI_EINVALID_PARA;
		}
		tagList += 2;
	}

	return SPI_SUCCESS;
}
/**
 * set the configuration of  a spi slave device.
 */
uint32_t spi_set_conf(struct spi_dev *handle, int32_t *tagList)
{
	struct spi_dev  *dev = NULL;
	struct spi_master  *master = NULL;

	if((handle == NULL) || (tagList == NULL)) {
		return SPI_EINVALID_PARA;
	}

	dev = handle;
	master = dev->master;
	if (!dev->in_use) {
		return SPI_EWRONG_STATE;
	}

	while (*tagList != SPI_TAGC_TAGEND) {
		switch (*tagList) {
		case SPI_TAGC_BITRATE: {
			uint32_t ctrl_rate, bitrate_in_Hz;

			ctrl_rate = SPI_CLK_IN_HZ / (2 * tagList[1]) - 1;
			bitrate_in_Hz = SPI_CLK_IN_HZ / (2 * (ctrl_rate + 1));

			if (bitrate_in_Hz > (uint32_t) tagList[1]) {
				ctrl_rate++;
			}

			if (ctrl_rate > CTRL_RATE_MASK) {
				ctrl_rate = CTRL_RATE_MASK;
			}

			dev->ctrl_reg = (dev->ctrl_reg & ~CTRL_RATE_MASK) |
			                (ctrl_rate << CTRL_RATE_POS);
			break;
		}
		case SPI_TAGC_RX_DELAY:
			if ((spi_rx_delay) tagList[1] == SPI_RX_DELAY_HALF_CYCLE) {
				dev->ctrl_reg &= ~CTRL_CLK_IDLE_MASK;
			} else if ((spi_rx_delay) tagList[1] ==
			           SPI_RX_DELAY_ONE_CYCLE) {
				dev->ctrl_reg |= CTRL_CLK_IDLE_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_AUTO_SS:
			if ((spi_auto_slave_select) tagList[1] == SPI_AUTO_SS_OFF) {
				dev->ctrl_reg &= ~CTRL_SS_MODE_MASK;
			} else if ((spi_auto_slave_select) tagList[1] == SPI_AUTO_SS_ON) {
				dev->ctrl_reg |= CTRL_SS_MODE_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_SEND_FIRST:
			if ((spi_send_first) tagList[1] == SPI_SEND_FIRST_MSB) {
				dev->ctrl_reg &= ~CTRL_LSBFE_MASK;
			} else if ((spi_send_first) tagList[1] ==
			           SPI_SEND_FIRST_LSB) {
				dev->ctrl_reg |= CTRL_LSBFE_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_PHASE:
			if ((spi_phase) tagList[1] == SPI_PHASE_LEADING) {
				dev->ctrl_reg &= ~CTRL_CPHA_MASK;
			} else if ((spi_phase) tagList[1] == SPI_PHASE_TRAILING) {
				dev->ctrl_reg |= CTRL_CPHA_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_POLARITY:
			if ((spi_polarity) tagList[1] == SPI_POLARITY_LOW) {
				dev->ctrl_reg &= ~CTRL_CPOL_MASK;
			} else if ((spi_polarity) tagList[1] == SPI_POLARITY_HIGH) {
				dev->ctrl_reg |= CTRL_CPOL_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_INVERT_CLK:
			if ((spi_invert_clock) tagList[1] == SPI_INVERT_CLK_OFF) {
				master->spi_reg->hub[dev->unit] &= ~HUB_CLK_INV_MASK;
			} else if ((spi_invert_clock) tagList[1] ==
			           SPI_INVERT_CLK_ON) {
				master->spi_reg->hub[dev->unit] |= HUB_CLK_INV_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_SS:
			if ((spi_slave_select) tagList[1] == SPI_SS_LOW) {
				dev->ctrl_reg &= ~CTRL_SS_MASK;
			} else if ((spi_slave_select) tagList[1] == SPI_SS_HIGH) {
				dev->ctrl_reg |= CTRL_SS_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_SS_BIDIR:
			if ((spi_slave_select_bidir) tagList[1] == SPI_SS_BIDIR_NORMAL) {
				dev->ctrl_reg &= ~CTRL_SS_BIDIR_MASK;
			} else if ((spi_slave_select_bidir) tagList[1] ==
			           SPI_SS_BIDIR_SWITCH) {
				dev->ctrl_reg |= CTRL_SS_BIDIR_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_PORT:
			if (tagList[1] > HUB_PORT_MAX) {
				return SPI_EINVALID_PARA;
			} else {
				master->spi_reg->hub[dev->unit] =
				        (master->spi_reg->hub[dev->unit] & ~HUB_PORT_MASK) |
				        (tagList[1] << HUB_PORT_POS);
			}
			break;
		case SPI_TAGC_WIRE:
			if ((spi_wire) tagList[1] == SPI_WIRE_3) {
				master->spi_reg->hub[dev->unit] &= ~HUB_4W_MASK;
			} else if ((spi_wire) tagList[1] == SPI_WIRE_4) {
				master->spi_reg->hub[dev->unit] |= HUB_4W_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		case SPI_TAGC_SS_POL:
			if ((spi_slave_select_pol) tagList[1] == SPI_SS_POL_LOW) {
				master->spi_reg->hub[dev->unit] &= ~HUB_SS_HIGH_MASK;
			} else if ((spi_slave_select_pol) tagList[1] == SPI_SS_POL_HIGH) {
				master->spi_reg->hub[dev->unit] |= HUB_SS_HIGH_MASK;
			} else {
				return SPI_EINVALID_PARA;
			}
			break;
		default:
			return SPI_EINVALID_PARA;
		}
		tagList += 2; /* Skip tag and it's value. */
	}

	return SPI_SUCCESS;
}
/**
 * transmit a vector of buffer data to a spi slave device.
 */
uint32_t
spi_transfer(struct spi_dev *handle, struct spi_buffer_des *vector,
             uint32_t count)
{
	uint32_t err = 0;
	struct spi_dev  *dev = NULL;
	struct spi_master  *master = NULL;
	int pthread_ret;

	if(handle == NULL) {
		return SPI_EINVALID_PARA;
	}

	dev = handle;
	master = dev->master;
	if (!dev->in_use) {
		return SPI_EWRONG_STATE;
	}

	if (count == 0) {
		return SPI_EINVALID_PARA;
	}

	/*
	 * Reset spi controller. Note that if TX_UR or RX_UR is set Spi controller
	 * need to be reset before clearing the trap register
	 */
	master->spi_reg->cmd = CMD_RESET;

	/* Clear TRAP register. */
	master->spi_reg->status_trap = 0xFFFFFFFF;

	memset(&master->pnd_tr, 0, sizeof(master->pnd_tr));

	master->pnd_tr.unit = dev->unit;
	master->pnd_tr.err = SPI_SUCCESS;
	master->pnd_tr.iov.count = count;
	master->pnd_tr.iov.vector = vector;
	/* Enable interrupts. */
	master->spi_reg->status_mask = STATUS_RX_OR | STATUS_TX_UR |
	                               STATUS_DONE;

	pthread_ret = pthread_mutex_lock(&master->mutex);
	check_result("pthread_mutex_lock", pthread_ret);

	spi_handle_transfer(dev);
	pthread_ret = pthread_cond_wait(&master->cond, &master->mutex);
	check_result("pthread_cond_wait", pthread_ret);

	pthread_ret = pthread_mutex_unlock(&master->mutex);
	check_result("pthread_mutex_unlock", pthread_ret);

	err = master->pnd_tr.err;

	/* Disable interrupts. */
	master->spi_reg->status_mask = 0x00000000;
	master->pnd_tr.err = SPI_SUCCESS;

	return err;
}
void spi_get_aligned_rd_bits(struct spi_master *master)
{
	uint32_t i;

	master->pnd_tr.rd.iov.buffer =
	        master->pnd_tr.rd.iov.vector[master->pnd_tr.rd.iov.index].
	        base_addr;


	master->pnd_tr.rd.iov.length =
	        master->pnd_tr.rd.iov.vector[master->pnd_tr.rd.iov.index].len;

	for (i = master->pnd_tr.rd.iov.index; i < master->pnd_tr.rd.iov.count;
	     i++) {
		master->pnd_tr.rd.num_aligned_bits_left +=
		        (master->pnd_tr.rd.iov.vector[i].len) << 3;

		if(master->pnd_tr.rd.num_aligned_bits_left >
		    master->pnd_tr.rd.num_bits_left) {
			master->pnd_tr.rd.num_aligned_bits_left =
			        master->pnd_tr.rd.num_bits_left;
		}
		if ((master->pnd_tr.rd.iov.vector[i].len ) & 3 ||
		    master->pnd_tr.rd.num_aligned_bits_left ==
		    master->pnd_tr.rd.num_bits_left) {
			break;
		}
	}
	master->pnd_tr.rd.num_bits_left -=
	        master->pnd_tr.rd.num_aligned_bits_left;

}
static void spi_parse_xfer_header(struct spi_master *master,
                                  struct spi_xfer *xfer)
{
	switch (xfer->mode) {
	case SPI_MODE_WRITE:
		master->pnd_tr.wr.num_bits = xfer->length_in_bits;
		master->pnd_tr.wr.iov.vector = xfer->wr_vector;
		master->pnd_tr.rd.iov.vector = NULL;
		master->pnd_tr.rd.iov.count = 0;
		master->pnd_tr.rd.num_bits_left = 0;
		break;
	case SPI_MODE_READ:
		master->pnd_tr.wr.num_bits = 0;
		master->pnd_tr.wr.iov.vector = NULL;
		master->pnd_tr.wr.iov.count = 0;
		master->pnd_tr.rd.num_bits_left = xfer->length_in_bits;
		master->pnd_tr.rd.iov.vector = xfer->rd_vector;
		break;
	case SPI_MODE_BIDIR:
		master->pnd_tr.wr.num_bits = xfer->length_bidir_in_bits;
		master->pnd_tr.rd.num_bits_left =
		        xfer->length_in_bits - xfer->length_bidir_in_bits;
		master->pnd_tr.wr.iov.vector = xfer->wr_vector;
		master->pnd_tr.rd.iov.vector = xfer->rd_vector;
		break;
	case SPI_MODE_DUPLEX:
		master->pnd_tr.wr.num_bits = xfer->length_in_bits;
		master->pnd_tr.rd.num_bits_left = xfer->length_in_bits;
		master->pnd_tr.wr.iov.vector = xfer->wr_vector;
		master->pnd_tr.rd.iov.vector = xfer->rd_vector;
		break;
	}
	if (master->pnd_tr.wr.iov.vector != NULL) {
		master->pnd_tr.wr.iov.index = 0;
		master->pnd_tr.wr.iov.count = xfer->wr_vector_count;
		master->pnd_tr.wr.num_bits_left = master->pnd_tr.wr.num_bits;
	}
	if (master->pnd_tr.rd.iov.vector != NULL) {
		master->pnd_tr.rd.iov.index = 0;
		master->pnd_tr.rd.iov.count = xfer->rd_vector_count;
		master->pnd_tr.rd.num_aligned_bits_left = 0;
		spi_get_aligned_rd_bits(master);

	}

}

/**
 * Low level transfer/receive data function.
 */
static bool
spi_handle_transfer(struct spi_dev *dev)
{
	uint32_t word_to_write = 0;
	uint32_t nof_bits_in_fifo = 0;
	struct spi_master  *master = NULL;

	master = dev->master;

	for (;;) {
		uint32_t size;
		int32_t *tagList = NULL;

		if (master->pnd_tr.iov.index == master->pnd_tr.iov.count) {
			return false;
		}

		if (master->pnd_tr.wr.num_bits_left) {
			master->pnd_tr.iov.buffer =
			        master->pnd_tr.wr.iov.vector[master->pnd_tr.wr.iov.index].base_addr;
			master->pnd_tr.iov.length =
			        master->pnd_tr.wr.iov.vector[master->pnd_tr.wr.iov.index].len;
			goto spi_handle_transfer_tx;
		}

		if (master->pnd_tr.rd.num_aligned_bits_left ) {
			goto spi_handle_transfer_tx_done;
		}
		if (!master->pnd_tr.iov.length) {
			master->pnd_tr.iov.buffer = master->pnd_tr.iov.
			                            vector[master->pnd_tr.iov.index].base_addr;
			master->pnd_tr.iov.length = master->pnd_tr.iov.
			                            vector[master->pnd_tr.iov.index].len;
		}

		tagList = (int32_t *) master->pnd_tr.iov.buffer;

		switch (*tagList) {
		case SPI_TAGC_TRANSFER: {
			struct spi_xfer *xfer = (struct spi_xfer *) &tagList[1];
			spi_parse_xfer_header(dev->master, xfer);
			/* Write to CTRL register */
			dev->ctrl_reg = (dev->ctrl_reg & ~CTRL_MODE_MASK) |
			                xfer->mode;
			master->spi_reg->ctrl = dev->ctrl_reg;
			size = sizeof(*tagList) + sizeof(struct spi_xfer);
			break;
		}
		case SPI_TAGC_SS:
			if ((spi_slave_select) tagList[1] == SPI_SS_LOW) {
				dev->ctrl_reg &= ~CTRL_SS_MASK;
			} else if ((spi_slave_select) tagList[1] == SPI_SS_HIGH) {
				dev->ctrl_reg |= CTRL_SS_MASK;
			} else {
				master->pnd_tr.err = SPI_EINVALID_PARA;
				goto spi_handle_transfer_error;
			}
			master->spi_reg->ctrl = dev->ctrl_reg;
			size = 2 * sizeof(int32_t);
			break;
		case SPI_TAGC_TAGEND:
			size = sizeof(int32_t);
			break;
		default:
			master->pnd_tr.err = SPI_EINVALID_PARA;
			goto spi_handle_transfer_error;
		}

		master->pnd_tr.iov.buffer += size;
		master->pnd_tr.iov.length -= size;

		if (!master->pnd_tr.iov.length) {
			master->pnd_tr.iov.index++;
		}
		continue;

spi_handle_transfer_tx: {
			uint32_t txLengthInBits, txLengthThisLapInBits;

			txLengthInBits = LENGTH_VAL_MAX - nof_bits_in_fifo;

			if ( txLengthInBits > master->pnd_tr.wr.num_bits_left) {
				txLengthInBits = master->pnd_tr.wr.num_bits_left;
			}

			txLengthThisLapInBits = master->pnd_tr.iov.length << 3;

			if (txLengthThisLapInBits > txLengthInBits) {
				txLengthThisLapInBits = txLengthInBits;
			}
			master->pnd_tr.wr.num_bits_left -= txLengthThisLapInBits;
			txLengthInBits -= txLengthThisLapInBits;

			if (!(nof_bits_in_fifo & (32 - 1)) &&
			    !((uintptr_t)master->pnd_tr.iov.buffer & 0x3)) {
				nof_bits_in_fifo += txLengthThisLapInBits;

				while (txLengthThisLapInBits >= 32) {
					master->spi_reg->tx =
					        *((uint32_t *)master->pnd_tr.iov.buffer);
					master->pnd_tr.iov.buffer += 4;
					master->pnd_tr.iov.length -= 4;
					txLengthThisLapInBits -= 32;
				}

				if (txLengthThisLapInBits) {
					if (!master->pnd_tr.wr.num_bits_left) {
						/* Last write operation. */
						master->spi_reg->tx =
						        *((uint32_t *)master->
						          pnd_tr.iov.buffer);
						master->pnd_tr.iov.length = 0;
					} else {
						nof_bits_in_fifo -=
						        txLengthThisLapInBits;
						goto spi_handle_transfer_not_aligned;
					}
				}
			} else {
				bool try_align_tx;

spi_handle_transfer_not_aligned:
				if (!((dev->ctrl_reg ^ SPI_MODE_DUPLEX) &
				      CTRL_MODE_MASK) || txLengthInBits ||
				    !master->pnd_tr.wr.num_bits_left) {
					/*
					 * Duplex mode, data in current iov
					 * will not suffice to fill fifo or
					 * last write operation: do not try to align tx.
					 */
					try_align_tx = false;
				} else {
					try_align_tx = true;
				}

				while (txLengthThisLapInBits) {
					/*
					  FIXME:
					  Probably here need to be changed due to the little endian system
					 */
					word_to_write = (word_to_write << 8) |
					                *master->pnd_tr.iov.buffer;
					master->pnd_tr.iov.buffer++;
					master->pnd_tr.iov.length--;
					txLengthThisLapInBits -= 8;
					nof_bits_in_fifo += 8;
					if (!(nof_bits_in_fifo & (32 - 1))) {
						master->spi_reg->tx = word_to_write;
						word_to_write = 0;
					}
					if (try_align_tx && txLengthThisLapInBits < 32 &&
					    !((uintptr_t)master->pnd_tr.iov.buffer & 0x3)) {
						/*
						 * less than 32 bits left to transfer,
						 * Source is aligned and not
						 * last fragment. Break now that we
						 * are aligned.
						 */
						master->pnd_tr.wr.num_bits_left +=
						        txLengthThisLapInBits;
						/* Make sure that last word is stored below. */
						txLengthInBits = 0;
						break;
					}
				}

				if (!txLengthInBits && (nof_bits_in_fifo & (32 - 1))) {
					/* Write last bytes to fifo. */
					master->spi_reg->tx = word_to_write;
				}
			}
			if (!master->pnd_tr.iov.length) {
				master->pnd_tr.wr.iov.index++;
				if(master->pnd_tr.wr.iov.index >=
				    master->pnd_tr.wr.iov.count &&
				    master->pnd_tr.wr.num_bits_left) {
					master->pnd_tr.err = SPI_EOTHER;
					goto spi_handle_transfer_error;
				}
			}

			if (!txLengthInBits) {
				break;
			}
		}

	} /* for (;;) */

spi_handle_transfer_tx_done:
	if (!((dev->ctrl_reg ^ SPI_MODE_DUPLEX) & CTRL_MODE_MASK)) {
		/*
		* Duplex mode, reset nof_bits_in_fifo to make common code below
		* work.
		*/
		nof_bits_in_fifo = 0;
	} else if (!((dev->ctrl_reg ^ SPI_MODE_BIDIR) & CTRL_MODE_MASK) &&
	           !nof_bits_in_fifo) {
		/* No bits left to transfer change to READ mode. */
		dev->ctrl_reg = (dev->ctrl_reg & ~CTRL_MODE_MASK) | SPI_MODE_READ;
		master->spi_reg->ctrl = dev->ctrl_reg;
	}

	master->pnd_tr.rd.num_bits_from_fifo = LENGTH_VAL_MAX - nof_bits_in_fifo;

	if (master->pnd_tr.rd.num_bits_from_fifo >= master->pnd_tr.rd.
	    num_aligned_bits_left) {
		master->pnd_tr.rd.num_bits_from_fifo =
		        master->pnd_tr.rd.num_aligned_bits_left;
	} else {
		/*
		 * Not last fragment. Align size to lower 32 bit boundary if
		 * size is greater than 31 bits.
		 */
		if (master->pnd_tr.rd.num_bits_from_fifo & ~31) {
			master->pnd_tr.rd.num_bits_from_fifo &= ~31;
		}
	}

	/* Write to SPI_LENGTH register. */
	if(nof_bits_in_fifo == 0) {
		master->spi_reg->length =  master->pnd_tr.rd.num_bits_from_fifo - 1;
	} else {
		master->spi_reg->length = (nof_bits_in_fifo - 1) << LENGTH_DIR_POS |
		                          (nof_bits_in_fifo + master->pnd_tr.rd.num_bits_from_fifo - 1);
	}

	/* Write to SPI_HUB_SS register */
	master->spi_reg->hubSs = master->pnd_tr.unit;

	if (master->pnd_tr.err != SPI_SUCCESS) {
		/*
		 * If error occurs before we issued command it indicates that
		 * unless we introduced a bug in driver that someone else is
		 * accessing or HW. The error that occurs in HS76897 is most
		 * likely due to that someone else is acessing our HW between
		 * transactions. This check is to try to catch if someone is
		 * accessing our HW after we started transaction but before we
		 * issued command.Set bit to indicate that this is premature
		 * error.
		 */
		master->pnd_tr.err |= master->pnd_tr.err * (SPI_ECODE_MASK + 1)
		                      + SPI_EOTHER;
		goto spi_handle_transfer_error;
	}

	/* Start SPI transfer */
	master->spi_reg->cmd = CMD_STRB;

	return true;

spi_handle_transfer_error:
	return false;
}

/**
 * provide a way to handle something after the transfer process.
 */
static bool
spi_handle_transfer_done(struct spi_dev *dev)
{
	struct spi_master  *master = dev->master;
	uint32_t status_reg = master->spi_reg->status_trap;

	if (!status_reg) {
		/* Should never occur. */
		master->pnd_tr.err =
		        SPI_EFATAL_MASK | SPI_ESUB_UNKNOWN_INT | SPI_EOTHER;
		goto spi_handle_transfer_done_exit;
	}
	if ((status_reg & (STATUS_RX_OR | STATUS_TX_UR))) {
		master->pnd_tr.err = SPI_EOTHER;
		if(status_reg & STATUS_RX_OR) {
			master->pnd_tr.err |= SPI_ESUB_RX_OR;
		}
		if(status_reg & STATUS_TX_UR) {
			master->pnd_tr.err |= SPI_ESUB_TX_UR;
		}
	}

	if (!master->pnd_tr.err) {
		/* Clear TRAP register. */
		master->spi_reg->status_trap = 0xFFFFFFFF;
		if (status_reg & STATUS_DONE && master->pnd_tr.rd.num_aligned_bits_left) {

			master->pnd_tr.rd.num_aligned_bits_left -=
			        master->pnd_tr.rd.num_bits_from_fifo;

			while (master->pnd_tr.rd.num_bits_from_fifo) {
				struct spi_master  *master = dev->master;
				*master->pnd_tr.rd.iov.buffer = master->spi_reg->rx;
				if (master->pnd_tr.rd.num_bits_from_fifo <= 32) {
					master->pnd_tr.rd.num_bits_from_fifo = 0;
					break;
				}
				master->pnd_tr.rd.num_bits_from_fifo -= 32;
				master->pnd_tr.rd.iov.buffer++;
				if (master->pnd_tr.rd.iov.length >
				    sizeof(uint32_t)) {
					master->pnd_tr.rd.iov.length -= sizeof(uint32_t);
				} else {
					master->pnd_tr.rd.iov.index++;
					master->pnd_tr.rd.iov.buffer =
					        master->pnd_tr.rd.iov.
					        vector[master->pnd_tr.rd.iov.index].base_addr;
					master->pnd_tr.rd.iov.length =
					        master->pnd_tr.rd.iov.
					        vector[master->pnd_tr.rd.iov.index].len;
				}
			}
			if (!master->pnd_tr.rd.num_aligned_bits_left) {
				spi_get_aligned_rd_bits(master);
			}
		}

	}

spi_handle_transfer_done_exit:
	if (master->pnd_tr.err == SPI_SUCCESS) {
		return true;
	}

	/* Disable interrupts. */
	master->spi_reg->status_mask = 0x00000000;

	/*
	 * Reset spi controller. Note that if TX_UR or RX_UR is set Spi controller
	 * need to be reset before clearing the trap register
	 */
	master->spi_reg->cmd = CMD_RESET;
	/* Clear TRAP register. */
	master->spi_reg->status_trap = 0xFFFFFFFF;

	return false;
}
/**
 * the interrupt service handler.
 */
void spi_int_handler(struct spi_master *master)
{
	uint32_t unit = master->pnd_tr.unit;
	struct spi_dev  *dev = &master->dev[unit];
	int pthread_ret;

	if (spi_handle_transfer_done(dev)) {
		if (!spi_handle_transfer(dev)) {

			pthread_ret = pthread_mutex_lock(&master->mutex);
			check_result("pthread_mutex_lock", pthread_ret);

			pthread_ret = pthread_cond_signal(&master->cond);
			check_result("pthread_cond_signal", pthread_ret);

			pthread_ret = pthread_mutex_unlock(&master->mutex);
			check_result("pthread_mutex_unlock", pthread_ret);

		}
	}
}
/**
 * initialize  a spi slave device.
 */
struct spi_master *spi_init(void)
{
	struct spi_master *master = NULL;

	master = (struct spi_master *)calloc(1, sizeof(struct spi_master));
	if(NULL == master) {
		TPT_ERROR("calloc failed.");
		return NULL;
	}
	pthread_mutex_init(&master->mutex, NULL);
	pthread_cond_init(&master->cond, NULL);

	return master;

}
uint32_t spi_init_regs(struct spi_master *master, void *addr)
{
	if(addr == NULL)
		return SPI_EINVALID_PARA;

	master->spi_reg = (struct spi_reg *)addr;

	/*
	 * Reset spi controller. Note that if TX_UR or RX_UR is set Spi controller
	 * need to be reset before clearing the trap register
	 */
	master->spi_reg->cmd = CMD_RESET;
	/* Disable interrupts. */
	master->spi_reg->status_mask = 0x00000000;
	/* Clear TRAP register. */
	master->spi_reg->status_trap = 0xFFFFFFFF;
	/* Set slave select to none.*/
	master->spi_reg->hubSs = 16;

	return SPI_SUCCESS;

}

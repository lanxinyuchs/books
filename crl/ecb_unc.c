/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>

#include "ecb_dev.h"
#include "ecb_unc.h"
#include "ecb_fcs.h"

#include "ecb.h"

#include "log.h"

/* HDLC Control octet poll/final bit. */
#define CTRL_PF      0x10

/* HDLC Flag and Escape octets. */
#define CHAR_T       0x7e
#define CHAR_E       0x7d
#define CHAR_ESC(ptr, c)	  \
	do { \
		if ((c) == CHAR_T || (c) == CHAR_E) { \
			*((ptr)++) = CHAR_E; \
			*((ptr)++) = (c) ^ 0x20; \
		} else { \
			*((ptr)++) = (c); \
		} \
	} while (0);

/* HDLC frame field sizes. */
#define HDLC_SIZE_ADDR 1
#define HDLC_SIZE_CTRL 1
#define HDLC_SIZE_FCS  2
#define HDLC_SIZE_MIN  (HDLC_SIZE_ADDR + HDLC_SIZE_CTRL + HDLC_SIZE_FCS)

struct unc_obj {
	struct ecb_ctx      *ctx;
	uint8_t              addr;
	struct ecb_unc_stat *stat;
	uint8_t             *ctrl;
	int                  info_flag;
	uint8_t             *info;
	uint32_t            *info_size;
};


static inline void unc_update_stat(void *handle, struct ecb_unc_stat *stat,
                                   struct ecb_unc_stat *local)
{
	struct ecb_dev_stat_hdlc dev_stat;

	dev_stat.n_tx        = local->n_tx;
	dev_stat.n_rx        = local->n_rx;
	dev_stat.n_tx_frames = local->n_tx_frames;
	dev_stat.n_rx_frames = local->n_rx_frames;
	dev_stat.n_rsp_tmo   = local->n_rsp_tmo;
	dev_stat.err_addr    = local->err_addr;
	dev_stat.err_ctrl    = local->err_ctrl;
	dev_stat.err_crc     = local->err_crc;
	dev_stat.err_size    = local->err_size;

	ecb_dev_stat_opt(handle, ECB_DEV_STAT_OPT_UPDATE_HDLC, &dev_stat);

	if (!stat)
		return;
	stat->n_tx        += local->n_tx;
	stat->n_rx        += local->n_rx;
	stat->n_tx_frames += local->n_tx_frames;
	stat->n_rx_frames += local->n_rx_frames;
	stat->n_rsp_tmo   += local->n_rsp_tmo;
	stat->err_addr    += local->err_addr;
	stat->err_ctrl    += local->err_ctrl;
	stat->err_crc     += local->err_crc;
	stat->err_size    += local->err_size;
}

static int unc_rx(void *obj, uint8_t *data, uint32_t size)
{
	struct unc_obj *o = (struct unc_obj*) obj;
	uint8_t        *from = data;
	uint8_t        *to = data;
	uint8_t        *frame = data;
	uint16_t        fcs = ECB_FCS_INIT;
	uint32_t        idx;

	if (o->stat) {
		o->stat->n_rx += size;
	}

	/* Unescape and calculate FCS for frame. */
	while (*from != CHAR_T) {
		if (*from != CHAR_E) {
			*to = *from++;
		} else {
			/* Invert bit 5 of the next following byte. */
			from++;
			*to = *(from++) ^ 0x20;
			size--;
		}
		fcs = ECB_FCS(fcs, *to++);
	}

	/* Exclude the CHAR_T from frame. */
	size--;

	/* If the length is bad, seek next frame. */
	if (size < HDLC_SIZE_MIN) {
		if (size > 0 && o->stat) {
			o->stat->err_size++;
		}
		return 0;
	}

	/* If the FCS is bad, seek next frame. */
	if (!o->ctx->unc.fcs_be) {
		if (fcs != ECB_FCS_GOOD) {
			if (o->stat) {
				o->stat->err_crc++;
			}
			return 0;
		}
	} else { /* For big endian FCS, calculate CRC and compare with FCS. */
		fcs = ECB_FCS_INIT;
		for (idx = 0; idx < size - HDLC_SIZE_FCS; idx++) {
			fcs = ECB_FCS(fcs, frame[idx]);
		}
		fcs = htons(fcs ^ 0xffff);
		if (memcmp(&fcs, &frame[size - HDLC_SIZE_FCS],
		           HDLC_SIZE_FCS) != 0) {
			o->stat->err_crc++;
			return 0;
		}
	}

	/* If the HDLC address is bad, seek next frame. */
	if (frame[0] != o->addr) {
		if (o->stat) {
			o->stat->err_addr++;
		}
		return 0;
	}

	if (frame[1] & CTRL_PF) {

		/* Store HDLC frame excluding FCS in object. */
		if (o->ctrl) {
			*o->ctrl = frame[1];
		}

		if (size > HDLC_SIZE_MIN && o->info_size) {
			if (size - HDLC_SIZE_MIN <= *o->info_size) {
				*o->info_size = size - HDLC_SIZE_MIN;
				o->info_flag = 1;
				memcpy(o->info, &frame[2], *o->info_size);
				o->stat->n_rx_frames++;
			} else if (o->stat) {
				o->stat->err_size++;
			}
		}

		/* Final bit set, last frame received. */
		return 1;

	} else if (o->stat) {

		/* Bad HDLC control field, seek next frame. */
		o->stat->err_ctrl++;

	}
	/* Final bit not set, seek next frame. */
	return 0;
}

static uint32_t unc_tx(void *handle, uint8_t *data,
                       uint8_t addr, uint8_t ctrl, void *info, uint32_t size)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;
	uint8_t        *from = (uint8_t*) info;
	uint8_t        *to = data;
	uint32_t        idx;
	uint16_t        fcs = ECB_FCS_INIT;

	/* Append prefix. */
	for (idx = 0; idx < ctx->unc.prefix.size; idx++) {
		*to++ = ctx->unc.prefix.data[idx];
	}

	/* Append opening flags. */
	for (idx = 0; idx < ctx->unc.n_oflags; idx++) {
		*to++ = CHAR_T;
	}

	/* Append address field. */
	fcs = ECB_FCS(fcs, addr);
	CHAR_ESC(to, addr);

	/* Append control field. */
	fcs = ECB_FCS(fcs, ctrl);
	CHAR_ESC(to, ctrl);

	/* Append information field. */
	for (idx = 0; idx < size; idx++, from++) {
		fcs = ECB_FCS(fcs, *from);
		CHAR_ESC(to, *from);
	}

	/* Append the FCS according to FCS endian option. */
	fcs ^= 0xffff;
	if (ctx->unc.fcs_be) {
		fcs = (uint16_t) (((fcs & 0xff00) >> 8) |
		                  ((fcs & 0x00ff) << 8));
	}
	CHAR_ESC(to, (uint8_t) (fcs & 0xff));
	CHAR_ESC(to, (uint8_t) (fcs >> 8));

	/* Append the terminating flag. */
	*to++ = CHAR_T;

	return (uint32_t) (to - data);
}

int ecb_unc_cmd_rsp(void *handle, struct ecb_unc_stat *stat, uint8_t addr,
                    uint8_t cmd_ctrl, void *cmd_info, uint32_t cmd_info_size,
                    uint8_t *rsp_ctrl, void *rsp_info, uint32_t *rsp_info_size,
                    struct timespec *const tmo)
{
	struct ecb_unc_stat local_stat;
	struct unc_obj  obj = {
		.ctx = (struct ecb_ctx*) handle,
		.addr = addr,
		.stat = &local_stat,
		.ctrl = rsp_ctrl,
		.info_flag = 0,
		.info = (uint8_t*) rsp_info,
		.info_size = rsp_info_size,
	};
	uint8_t         frame[ECB_DEV_MTU];
	uint32_t        size;
	int             ret;

	if (!handle) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_unc_cmd_rsp() called with NULL pointer");
		return -1;
	}

	memset(&local_stat, 0, sizeof(struct ecb_unc_stat));

	size = unc_tx(handle, frame, addr, cmd_ctrl, cmd_info, cmd_info_size);

	if (ecb_dev_begin(handle) != 0) {
		return -1;
	}

	if (ecb_dev_write(handle, frame, size) == 0) {
		local_stat.n_tx += size;
		local_stat.n_tx_frames++;
	}

	ret = ecb_dev_receive(handle, CHAR_T, unc_rx, &obj, tmo);
	if (!obj.info_flag && rsp_info_size) {
		*rsp_info_size = 0;
	}

	if (ret == 0) {
		local_stat.n_rsp_tmo++;
	}

	unc_update_stat(handle, stat, &local_stat);

	if (ecb_dev_end(handle) != 0) {
		return -1;
	}

	return ret;
}

int ecb_unc_wait(void *handle, struct ecb_unc_stat *stat, uint8_t addr,
                 uint8_t *ctrl, void *info, uint32_t *info_size,
                 struct timespec *const tmo)
{
	struct ecb_unc_stat local_stat;
	struct unc_obj  obj = {
		.ctx = (struct ecb_ctx*) handle,
		.addr = addr,
		.stat = &local_stat,
		.ctrl = ctrl,
		.info_flag = 0,
		.info = (uint8_t*) info,
		.info_size = info_size,
	};
	int             ret;

	if (!handle) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_unc_wait() called with NULL pointer");
		return -1;
	}

	memset(&local_stat, 0, sizeof(struct ecb_unc_stat));

	ret = ecb_dev_receive(handle, CHAR_T, unc_rx, &obj, tmo);
	if (!obj.info_flag && info_size) {
		*info_size = 0;
	}

	/* TODO: timeout counter should be increased here if this is used
	   by AU2 clients */

	unc_update_stat(handle, stat, &local_stat);

	return ret;
}

int ecb_unc_send(void *handle, struct ecb_unc_stat *stat, uint8_t addr,
                 uint8_t ctrl, void *info, uint32_t info_size)
{
	struct ecb_unc_stat local_stat;
	uint8_t  frame[ECB_DEV_MTU];
	uint32_t size;

	if (!handle) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_unc_send() called with NULL pointer");
		return -1;
	}

	memset(&local_stat, 0, sizeof(struct ecb_unc_stat));

	size = unc_tx(handle, frame, addr, ctrl, info, info_size);

	if (ecb_dev_write(handle, frame, size) == 0) {
		local_stat.n_tx += size;
		local_stat.n_tx_frames++;
	}

	unc_update_stat(handle, stat, &local_stat);

	return 0;
}

int ecb_unc_opt(void *handle, enum ecb_unc_opt option, const void *value)
{
	struct ecb_ctx            *ctx = (struct ecb_ctx*) handle;
	struct ecb_unc_opt_prefix *prefix;
	uint32_t                   u32;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_unc_opt() called with NULL pointer");
		return -1;
	}

	switch (option) {
	case ECB_UNC_OPT_FCS_BIG_ENDIAN:
		memcpy(&ctx->unc.fcs_be, value, sizeof(uint32_t));
		break;
	case ECB_UNC_OPT_N_OFLAGS:
		memcpy(&u32, value, sizeof(uint32_t));
		if (u32 > ECB_UNC_OPT_MAX_OFLAGS) {
			log_err(ctx->log_prefix,
			        "ecb_unc_opt() invalid number of opening flags");
			return -1;
		}
		ctx->unc.n_oflags = u32;
		break;
	case ECB_UNC_OPT_PREFIX:
		ctx->unc.prefix.size = 0;
		if (ctx->unc.prefix.data) {
			free(ctx->unc.prefix.data);
		}
		prefix = (struct ecb_unc_opt_prefix*) value;
		memcpy(&u32, &prefix->size, sizeof(uint32_t));
		if (u32 > ECB_UNC_OPT_MAX_PREFIX) {
			log_err(ctx->log_prefix,
			        "ecb_unc_opt() invalid number of "
			        "prefix characters");
			return -1;
		} else if (u32 == 0) {
			break;
		}
		ctx->unc.prefix.data = malloc(u32);
		if (!ctx->unc.prefix.data) {
			log_err(ctx->log_prefix,
			        "ecb_unc_opt() could not allocate space for "
			        "prefix characters");
			return -1;
		}
		ctx->unc.prefix.size = u32;
		memcpy(ctx->unc.prefix.data, prefix->data, u32);
		break;
	default:
		return -1;
	}

	return 0;
}

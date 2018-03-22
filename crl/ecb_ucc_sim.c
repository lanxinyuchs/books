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
#include <unistd.h>
#include <fcntl.h>

#include <ecb_dev.h>
#include <ecb_ucc.h>
#include <ecb_fcs.h>
#include "ecb_ucc_sim.h"

/* HDLC frame field sizes. */
#define HDLC_SIZE_ADDR 1
#define HDLC_SIZE_CTRL 1
#define HDLC_SIZE_FCS  2
#define HDLC_SIZE_MIN  (HDLC_SIZE_ADDR + HDLC_SIZE_CTRL + HDLC_SIZE_FCS)

/* HDLC Flag and Escape octets. */
#define CHAR_T       0x7e
#define CHAR_E       0x7d
#define CHAR_ESC(ptr, c)				\
	do {						\
		if ((c) == CHAR_T || (c) == CHAR_E) {	\
			*((ptr)++) = CHAR_E;		\
			*((ptr)++) = (c) ^ 0x20;	\
		} else {				\
			*((ptr)++) = (c);		\
		}					\
	} while (0);

/* HDLC Control octet. */
#define CTRL_UI      0x03
#define CTRL_PF      0x10

/* Time for AU4 unit to disable TX. */
#define UCC_DELAY_TX_DISABLE    2000

/* MTU for testing purposes, even larger than serial transmit buffer size
   in kernel */
#define ECB_TEST_MTU    8000

struct ucc_obj {
	uint8_t              addr;
	struct ecb_ucc_stat *stat;
	int                  cmd_info_flag;
	uint8_t             *cmd_info;
	uint32_t            *cmd_info_size;
};

static uint32_t ucc_sim_tx(uint8_t *data, uint8_t addr, uint8_t ctrl,
		       void *info, uint32_t size, ucc_sim_status behavior)
{
	uint8_t  *from = (uint8_t*) info;
	uint8_t  *to = data;
	uint32_t  idx;
	uint16_t  fcs = ECB_FCS_INIT;

	/* Append start terminator. */
	*to++ = CHAR_T;

	/* Append address field. */
	if (behavior.bit.address == 1) {
		addr++;
	}
	fcs = ECB_FCS(fcs, addr);
	CHAR_ESC(to, addr);

	/* Append control field. */
	if (behavior.bit.ctrl == 1) {
		ctrl++;
	}
	fcs = ECB_FCS(fcs, ctrl);
	CHAR_ESC(to, ctrl);

	/* Append information field. */
	for (idx = 0; idx < size; idx++, from++) {
		fcs = ECB_FCS(fcs, *from);
		CHAR_ESC(to, *from);
	}

	/* Append the FCS. */
	fcs ^= 0xffff;
	if (behavior.bit.crc == 1) {
		fcs++;
	}
	CHAR_ESC(to, (uint8_t) (fcs & 0xff));
	CHAR_ESC(to, (uint8_t) (fcs >> 8));

	/* Append the end terminator. */
	*to++ = CHAR_T;

	return (uint32_t) (to - data);
}

static uint32_t ucc_sim_tx_final(uint8_t *data, uint8_t addr)
{
	uint8_t  *to = data;
	uint16_t  fcs = ECB_FCS_INIT;

	/* Append start terminator. */
	*to++ = CHAR_T;

	/* Append address field. */
	fcs = ECB_FCS(fcs, addr);
	CHAR_ESC(to, addr);

	/* Append control field. */
	fcs = ECB_FCS(fcs, CTRL_UI | CTRL_PF);
	CHAR_ESC(to, CTRL_UI | CTRL_PF);

	/* Append information field 0. */
	fcs = ECB_FCS(fcs, 0);
	CHAR_ESC(to, 0);

	/* Append the FCS. */
	fcs ^= 0xffff;

	CHAR_ESC(to, (uint8_t) (fcs & 0xff));
	CHAR_ESC(to, (uint8_t) (fcs >> 8));

	/* Append the end terminator. */
	*to++ = CHAR_T;

	return (uint32_t) (to - data);
}

static int ucc_sim_rx(void *obj, uint8_t *data, uint32_t size)
{
	struct ucc_obj *o = (struct ucc_obj*) obj;
	uint8_t        *from = data;
	uint8_t        *to = data;
	uint8_t        *frame = data;
	uint16_t        fcs = ECB_FCS_INIT;

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
		printf("Frame dropped, bad length %u, min %u!\n", size, HDLC_SIZE_MIN);
		return 0;
	}

	/* If the FCS is bad, seek next frame. */
	if (fcs != ECB_FCS_GOOD) {
		if (o->stat) {
			o->stat->err_crc++;
		}
		printf("Frame dropped, bad crc!\n");
		return 0;
	}

	/* If the HDLC address is bad, seek next frame. */
	if (frame[0] != o->addr) {
		if (o->stat) {
			o->stat->err_addr++;
		}
		printf("Frame dropped, bad address %02x, expecting %02x!\n",
		       frame[0], o->addr);
		return 0;
	}

	if ((frame[1] == CTRL_UI) || (frame[1] == (CTRL_UI | CTRL_PF))) {

		/* Store HDLC information field in object, seek next frame. */
		if (size > HDLC_SIZE_MIN && o->cmd_info_size) {
			if (size - HDLC_SIZE_MIN <= *o->cmd_info_size) {
				*o->cmd_info_size = size - HDLC_SIZE_MIN;
				o->cmd_info_flag = 1;
				memcpy(o->cmd_info,
				       &frame[2],
				       *o->cmd_info_size);
			} else if (o->stat) {
				o->stat->err_size++;
				printf("Frame dropped, bad size %u, max %u!\n",
				       size - HDLC_SIZE_MIN, *o->cmd_info_size);
			}
		}
		if (frame[1] == (CTRL_UI | CTRL_PF)) {

			/* UI with final bit set, last frame received. */
			return 1;
		}

	} else if (o->stat) {

		/* Bad HDLC control field, seek next frame. */
		o->stat->err_ctrl++;
		printf("Frame dropped, bad ctrl field %u!\n", frame[1]);

	}

	return 0;
}

int ecb_ucc_sim_send(void *handle, uint8_t addr, void *rsp_info,
                     uint32_t rsp_info_size, ucc_sim_status behavior)
{
	uint8_t  frame[ECB_TEST_MTU],
	         frame2[ECB_DEV_MTU];
	uint32_t size, size2;

	/* First frame contains data which will be handled by a4ci server */
	size = ucc_sim_tx(frame, addr, CTRL_UI, rsp_info, rsp_info_size, behavior);
	/* Second frame contains data which will be discarded by a4ci server, but
	   server will be notified that it has received last frame */
	size2 = ucc_sim_tx_final(frame2, addr);

	if (ecb_dev_begin(handle) != 0) {
		return -1;
	}

	/* Send only if timeout behavior is not set */
	if (behavior.bit.timeout == 0) {
		ecb_dev_write(handle, frame, size);
		ecb_dev_write(handle, frame2, size2);
	}

	if (ecb_dev_end(handle) != 0) {
		return -1;
	}

	return 0;
}

int ecb_ucc_sim_receive(void *handle, uint8_t addr, void *cmd_info,
                        uint32_t *cmd_info_size, time_t tmo_sec)
{
	struct ucc_obj obj = {
		.addr = addr,
		.stat = 0,
		.cmd_info_flag = 0,
		.cmd_info = (uint8_t*) cmd_info,
		.cmd_info_size = cmd_info_size
	};
	struct timespec tmo = {
		.tv_sec = tmo_sec,
		.tv_nsec = 0
	};
	int ret;

	if (ecb_dev_begin(handle) != 0) {
		return -1;
	}

	ret = ecb_dev_receive(handle, CHAR_T, ucc_sim_rx, &obj, &tmo);
	if (!obj.cmd_info_flag && cmd_info_size) {
		*cmd_info_size = 0;
	}

	usleep(UCC_DELAY_TX_DISABLE);

	if (ecb_dev_end(handle) != 0) {
		return -1;
	}

	return ret;
}

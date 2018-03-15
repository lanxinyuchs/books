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

#include "ecb_dev.h"
#include "ecb_ucc.h"
#include "ecb_fcs.h"


/* HDLC Control octet. */
#define CTRL_UI      0x03
#define CTRL_PF      0x10

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

/* HDLC frame field sizes. */
#define HDLC_SIZE_ADDR 1
#define HDLC_SIZE_CTRL 1
#define HDLC_SIZE_FCS  2
#define HDLC_SIZE_MIN  (HDLC_SIZE_ADDR + HDLC_SIZE_CTRL + HDLC_SIZE_FCS)

/* Time for AU4 unit to disable TX. */
#define AU4_DELAY_TX_DISABLE    2000

/* HDLC address of FCU which some come with a FCS field defect. */
#define AU4_ADDR_FCU            0x2f

/* Possible FCS bit errors from FCUs due to the FCS field defect. */
#define AU4_FCU_TOGGLE_1ST      0x2000
#define AU4_FCU_TOGGLE_2ND      0x0020
#define AU4_FCU_TOGGLE_ALL      0x2020


struct ucc_obj {
	uint8_t              addr;
	struct ecb_ucc_stat *stat;
	int                  rsp_info_flag;
	uint8_t             *rsp_info;
	uint32_t            *rsp_info_size;
};


static int ucc_rx(void *obj, uint8_t *data, uint32_t size)
{
	struct ucc_obj *o = (struct ucc_obj*) obj;
	uint8_t        *from = data;
	uint8_t        *to = data;
	uint8_t        *frame = data;
	uint32_t        i;
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
		return 0;
	}

	/* If the FCS is bad, seek next frame. */
	if (fcs != ECB_FCS_GOOD) {
		if (frame[0] != AU4_ADDR_FCU) {

			if (o->stat) {
				o->stat->err_crc++;
			}
			return 0;

		} else {

			/* Calculate FCS for defect FCU PPs (HDLC address 0x2f) */
			fcs = ECB_FCS_INIT;
			for (i = 0; i < size - HDLC_SIZE_FCS; i++) {
				fcs = ECB_FCS(fcs, frame[i]);
			}
			fcs = ((fcs << 8) | (fcs >> 8)) ^ 0xffff;
			if (memcmp(&fcs, &frame[size - HDLC_SIZE_FCS], HDLC_SIZE_FCS) != 0) {
				fcs ^= AU4_FCU_TOGGLE_2ND;
				if (memcmp(&fcs, &frame[size - HDLC_SIZE_FCS], HDLC_SIZE_FCS) != 0) {
					fcs ^= AU4_FCU_TOGGLE_2ND;
					fcs ^= AU4_FCU_TOGGLE_1ST;
					if (memcmp(&fcs, &frame[size - HDLC_SIZE_FCS], HDLC_SIZE_FCS) != 0) {
						fcs ^= AU4_FCU_TOGGLE_1ST;
						fcs ^= AU4_FCU_TOGGLE_ALL;
						if (memcmp(&fcs, &frame[size - HDLC_SIZE_FCS], HDLC_SIZE_FCS) != 0) {
							fcs ^= AU4_FCU_TOGGLE_ALL;
							if (o->stat) {
								o->stat->err_crc++;
							}
							return 0;
						}
					}
				}
			}

		}
	}

	/* If the HDLC address is bad, seek next frame. */
	if (frame[0] != o->addr) {
		if (o->stat) {
			o->stat->err_addr++;
		}
		return 0;
	}

	if (frame[1] == (CTRL_UI | CTRL_PF)) {

		/* UI with final bit set, last frame received. */
		return 1;

	} else if (frame[1] == CTRL_UI) {

		/* Store HDLC information field in object, seek next frame. */
		if (size > HDLC_SIZE_MIN && o->rsp_info_size) {
			if (size - HDLC_SIZE_MIN <= *o->rsp_info_size) {
				*o->rsp_info_size = size - HDLC_SIZE_MIN;
				o->rsp_info_flag = 1;
				memcpy(o->rsp_info,
				       &frame[2],
				       *o->rsp_info_size);
			} else if (o->stat) {
				o->stat->err_size++;
			}
		}

	} else if (o->stat) {

		/* Bad HDLC control field, seek next frame. */
		o->stat->err_ctrl++;

	}

	return 0;
}

static uint32_t ucc_tx(uint8_t *data, uint8_t addr, uint8_t ctrl,
		       void *info, uint32_t size)
{
	uint8_t  *from = (uint8_t*) info;
	uint8_t  *to = data;
	uint32_t  idx;
	uint16_t  fcs = ECB_FCS_INIT;

	/* Append some start terminators. */
	*to++ = CHAR_T;
	*to++ = CHAR_T;

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

	/* Append the FCS. */
	fcs ^= 0xffff;
	CHAR_ESC(to, (uint8_t) (fcs & 0xff));
	CHAR_ESC(to, (uint8_t) (fcs >> 8));

	/* Append the end terminator. */
	*to++ = CHAR_T;

	return (uint32_t) (to - data);
}

int ecb_ucc_cmd_rsp(void *handle, struct ecb_ucc_stat *stat, uint8_t addr,
                    void *cmd_info, uint32_t cmd_info_size,
                    void *rsp_info, uint32_t *rsp_info_size,
                    struct timespec *const tmo)
{
	struct ucc_obj obj = {
		.addr = addr,
		.stat = stat,
		.rsp_info_flag = 0,
		.rsp_info = (uint8_t*) rsp_info,
		.rsp_info_size = rsp_info_size,
	};
	uint8_t        frame[ECB_DEV_MTU];
	uint32_t       size;
	int            ret;

	size = ucc_tx(frame, addr, CTRL_UI | CTRL_PF, cmd_info, cmd_info_size);

	if (ecb_dev_begin(handle) != 0) {
		return -1;
	}

	if (ecb_dev_write(handle, frame, size) == 0 && stat) {
		stat->n_tx += size;
	}

	ret = ecb_dev_receive(handle, CHAR_T, ucc_rx, &obj, tmo);
	if (!obj.rsp_info_flag && rsp_info_size) {
		*rsp_info_size = 0;
	}

	usleep(AU4_DELAY_TX_DISABLE);

	if (ecb_dev_end(handle) != 0) {
		return -1;
	}

	return ret;
}

int ecb_ucc_cmd(void *handle, struct ecb_ucc_stat *stat, uint8_t addr,
                void *cmd_info, uint32_t cmd_info_size)
{
	uint8_t  frame[ECB_DEV_MTU];
	uint32_t size;

	size = ucc_tx(frame, addr, CTRL_UI, cmd_info, cmd_info_size);

	if (ecb_dev_begin(handle) != 0) {
		return -1;
	}

	if (ecb_dev_write(handle, frame, size) == 0 && stat) {
		stat->n_tx += size;
	}

	if (ecb_dev_end(handle) != 0) {
		return -1;
	}

	return 0;
}

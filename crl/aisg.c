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

/*
 * Documents: 3GPP TS 25.462 version 12.0.0 Release 12
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/param.h>

#include <ecb_fcs.h>
#include <ecb_dev.h>
#include <ecb_unc.h>

#include <atfi.h>

#include "atfi_hdlc.h"
#include "aisg.h"

#include "log.h"

/*
*******************************************************************************
**  MACROS
*******************************************************************************
*/


#define MODEM_TXRX_TIME     3000 /* Time [us] for modem TX to RX transition */

#define AISG_MAX_INFO_SIZE    74 /* Maximum information field size */

#define AISG_FI             0x81 /* Format Identifier */
#define AISG_GI_USER_DEF    0xf0 /* Group Identifier (user defined) */

#define AISG_PI_UID            1 /* AISG Unique Identifier */
#define AISG_PI_HDLC_ADDR      2 /* AISG HDLC Address */
#define AISG_PI_MASK           3 /* AISG Bit Mask */
#define AISG_PI_DEVICE_TYPE    4 /* AISG Device Type */
#define AISG_PI_VID            6 /* AISG Vendor Identifier */
#define AISG_PI_RESET_DEVICE   7 /* AISG Reset Device */

#define PI_MASK_VID          0x1
#define PI_MASK_UID          0x2
#define PI_MASK_DEVICE_TYPE  0x4
#define PI_MASK_COMPLETE (PI_MASK_VID | PI_MASK_UID | PI_MASK_DEVICE_TYPE)

/* Device Scan */
#define DS_MIN_RSP_P_SIZE     12 /* Minimum response parameter size */
#define DS_MAX_RSP_P_SIZE     28 /* Maximum response parameter size */
#define DS_MIN_RSP_F_SIZE     16 /* Minimum response frame size */
#define DS_MAX_RSP_F_SIZE     35 /* Maximum response frame size */
#define DS_MAX_RSP_D_SIZE   2048 /* Maximum response data size */

/* Address Assignment */
#define AA_MIN_RSP_I_SIZE     11 /* Minimum response information size */
#define AA_MIN_RSP_P_SIZE      8 /* Minimum response parameter size */


/*
*******************************************************************************
**  FUNCTIONS
*******************************************************************************
*/

static int unc_wait(const char *log_prefix __attribute__((unused)),
                    void *handle, uint8_t addr,
                    uint8_t *rsp_ctrl, void *rsp_info, uint32_t *rsp_info_size,
                    struct timespec *rsp_tmo)
{
	uint8_t rsp_ctrl0;
	int     result;

	result = ecb_unc_wait(handle, NULL, addr,
	                      rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
	                      rsp_info, rsp_info_size, rsp_tmo);
	if (result == 1) {
		log_hdlc(addr, rsp_ctrl == NULL ? &rsp_ctrl0 : rsp_ctrl,
		         rsp_info, rsp_info_size, "RX(%s)", log_prefix);
	}

	return result;
}

static int unc_send(const char *log_prefix __attribute__((unused)),
                    void *handle, uint8_t addr,
                    uint8_t cmd_ctrl, void *cmd_info, uint32_t cmd_info_size)
{
	log_hdlc(addr, &cmd_ctrl, cmd_info, &cmd_info_size,
	         "TX(%s)", log_prefix);

	return ecb_unc_send(handle, NULL,
	                    addr, cmd_ctrl, cmd_info, cmd_info_size);
}

static void dump(char *prefix, const void *data, uint32_t size)
{
	uint32_t idx;
	char     text[200], *p = text;

	p += sprintf(p, "%s: ", prefix);
	for (idx = 0; idx < size; idx++) {
		p += sprintf(p, "%02x ", ((uint8_t*) data)[idx]);
	}
	log_trace2(text);
}

static void analyse_rsp_data(const char *log_prefix __attribute__((unused)),
                             uint8_t *data, uint32_t size,
                             uint16_t *result,
                             uint8_t *device_type,
                             uint8_t *rsp_uid, uint8_t *rsp_uid_size)
{
	const uint8_t good_rsp[] = {
		HDLC_ADDR_NO, HDLC_XID | HDLC_PF, AISG_FI, AISG_GI_USER_DEF
	};
	uint32_t  src, dst, n_errors, pi_mask;
	uint32_t  info_size __attribute__((unused));
	uint8_t  *para, *pv, gl, pi, pl, idx;
	uint16_t  fcs;

	/*
	 * Remove PARMRK characters.
	 *
	 * Break condition        -> 0xff 0x00 0x00 -> <>
	 * <X> bad data           -> 0xff 0x00 <X>  -> <>
	 * <0xff> good data 0xff  -> 0xff <0xff>    -> <0xff>
	 * <X> good data not 0xff -> <X>            -> <X>
	 *
	 */

	src = dst = n_errors = 0;
	while (src < size) {
		if (data[src] == 0xff) {
			src++;
			if (src >= size) {
				break;
			}
			if (data[src] == 0x00) {
				n_errors++;
				src += 2;
			} else if (data[src] == 0xff) {
				data[dst++] = data[src++];
			} else {
				break;
			}
		} else {
			data[dst++] = data[src++];
		}
	}
	size = dst;

#if 1 /* For testing purposes on XMU03 with break condition. */
	if (n_errors == 1 && size == 0) {
		*result = 2;
		return;
	}
#endif

	/* Set to collision for now, reset on a good response. */
	*result = 1;

	src = 0;
	while (src < size) {

		/* Skip any flag octets. */
		while (src < size && data[src] == 0x7e) {
			src++;
		}

		/* Unescape and calculate CRC until end of frame. */
		dst = 0;
		fcs = ECB_FCS_INIT;
		while (src < size && data[src] != 0x7e) {
			if (data[src] == 0x7d) {
				src++;
				if (src >= size) {
					return; /* End of data */
				}
				data[dst] = data[src++] ^ 0x20;
			} else {
				data[dst] = data[src++];
			}
			fcs = ECB_FCS(fcs, data[dst++]);
		}

		/* Check frame size. */
		if (dst < DS_MIN_RSP_F_SIZE || dst > DS_MAX_RSP_F_SIZE) {
			continue;
		}

		/* Check FCS. */
		if (fcs != ECB_FCS_GOOD) {
			continue;
		}

		/* HDLC frame: < ADDR(1), CTRL(1), INFO, FCS(2) > */
		info_size = dst - 4;

		/* Log received HDLC frame. */
		log_hdlc(data[0], &data[1], &data[2], &info_size,
		         "RX(%s)", log_prefix);

		/* Check first 4 bytes with a good response. */
		if (memcmp(data, good_rsp, sizeof(good_rsp)) != 0) {
			continue;
		}

		/* Check GL field (5:th byte) for valid size. */
		gl = data[4];
		if (gl < DS_MIN_RSP_P_SIZE || gl > DS_MAX_RSP_P_SIZE) {
			continue;
		}

		/* Extract Id and Device Type from parameter fields. */
		idx = 0;
		pi_mask = 0;
		para = &data[5];
		while (idx < gl) {
			pi = para[idx++];
			pl = para[idx++];
			pv = &para[idx];
			switch (pi) {
			case AISG_PI_VID:
				pi_mask |= PI_MASK_VID;
				break;
			case AISG_PI_UID:
				pi_mask |= PI_MASK_UID;
				memcpy(rsp_uid, pv,
				       MAX(pl, ATFI_MAX_UNIQUE_HW_ID_LENGTH));
				*rsp_uid_size = pl;
				break;
			case AISG_PI_DEVICE_TYPE:
				pi_mask |= PI_MASK_DEVICE_TYPE;
				*device_type = *pv;
				break;
			default:
				break;
			}
			idx += pl;
		}

		/* A complete response contains VID, UID and Device Type. */
		if (pi_mask != PI_MASK_COMPLETE) {
			continue;
		}

		*result = 0; /* Scan response received. */
		return;
	}

	return; /* End of data */
}

int aisg_device_scan(const char *log_prefix,
                     void *handle,
                     uint32_t timeout,
                     const uint8_t *cmd_uid,
                     const uint8_t cmd_uid_size,
                     const uint8_t *cmd_mask,
                     const uint8_t cmd_mask_size,
                     uint16_t *result,
                     uint8_t *device_type,
                     uint8_t *rsp_uid,
                     uint8_t *rsp_uid_size)
{
	int       idx = 0, ret = 0, res;
	uint32_t  cmd_info_size, size;
	uint8_t   cmd_info[AISG_MAX_INFO_SIZE], *data;

	/* Allocate memory for response data. */
	size = DS_MAX_RSP_D_SIZE;
	data = malloc(size);
	if (data == NULL) {
		ret = -1;
		goto err_mem;
	}

	/* Prepare command information field. */
	cmd_info[idx++] = AISG_FI;
	cmd_info[idx++] = AISG_GI_USER_DEF;
	cmd_info[idx++] = 2 + cmd_uid_size + 2 + cmd_mask_size;
	cmd_info[idx++] = AISG_PI_UID;
	cmd_info[idx++] = cmd_uid_size;
	if (cmd_uid_size > 0) {
		memcpy(&cmd_info[idx], cmd_uid, cmd_uid_size);
		idx += cmd_uid_size;
	}
	cmd_info[idx++] = AISG_PI_MASK;
	cmd_info[idx++] = cmd_mask_size;
	if (cmd_mask_size > 0) {
		memcpy(&cmd_info[idx], cmd_mask, cmd_mask_size);
		idx += cmd_mask_size;
	}
	cmd_info_size = idx;

	if (ecb_dev_begin(handle) != 0) {
		ret = -1;
		goto err_begin;
	}

	/* Send XID Device Scan command. */
	unc_send(log_prefix, handle, HDLC_ADDR_BC,
	         HDLC_XID | HDLC_PF, cmd_info, cmd_info_size);

	/* Collect response data during the specified time. */
	usleep(timeout * 1000);
	res = ecb_dev_read(handle, data, &size);
	usleep(MODEM_TXRX_TIME);
	ecb_dev_end(handle);

	dump("RX", data, size);
	log_bus_receive(data, size, "RX(%s)", log_prefix);
	if (res == -1) {
		ret = -1; /* Error occured! */
	} else if (size == 0) {
		*result = 2; /* Nothing received, time-out. */
	} else {
		/* Something was received, analyse response data. */
		analyse_rsp_data(log_prefix,
		                 data,
		                 size,
		                 result,
		                 device_type,
		                 rsp_uid,
		                 rsp_uid_size);
	}

err_begin:
	free(data);
err_mem:
	return ret;
}


int aisg_address_assignment(const char *log_prefix,
                            void *handle,
                            uint32_t timeout,
                            uint8_t addr,
                            const uint8_t *cmd_uid,
                            const uint8_t cmd_uid_size,
                            uint8_t device_type)
{
	struct timespec rsp_tmo = {
		.tv_sec = 0, .tv_nsec = timeout * 1000000
	};
	int      idx = 0, res;
	uint32_t cmd_info_size, rsp_info_size = AISG_MAX_INFO_SIZE;
	uint8_t  cmd_info[AISG_MAX_INFO_SIZE], rsp_info[AISG_MAX_INFO_SIZE];
	uint8_t  rsp_ctrl, gl, pi, pl, *pv, *para;

	/* Prepare command information field. */
	cmd_info[idx++] = AISG_FI;
	cmd_info[idx++] = AISG_GI_USER_DEF;
	cmd_info[idx++] = 3 + 3 + (cmd_uid_size > 0 ? 2 + cmd_uid_size : 0);
	cmd_info[idx++] = AISG_PI_HDLC_ADDR;
	cmd_info[idx++] = 1;
	cmd_info[idx++] = addr;
	if (cmd_uid_size > 0) {
		cmd_info[idx++] = AISG_PI_UID;
		cmd_info[idx++] = cmd_uid_size;
		memcpy(&cmd_info[idx], cmd_uid, cmd_uid_size);
		idx += cmd_uid_size;
	}
	cmd_info[idx++] = AISG_PI_DEVICE_TYPE;
	cmd_info[idx++] = 1;
	cmd_info[idx++] = device_type;
	cmd_info_size = idx;

	if (ecb_dev_begin(handle) != 0) {
		return -1;
	}

	unc_send(log_prefix, handle, HDLC_ADDR_BC,
	         HDLC_XID | HDLC_PF, cmd_info, cmd_info_size);

	res = unc_wait(log_prefix, handle, addr,
	               &rsp_ctrl, rsp_info, &rsp_info_size, &rsp_tmo);

	usleep(MODEM_TXRX_TIME);

	ecb_dev_end(handle);

	if (res == 0) {
		return -1; /* Response time-out */
	} else if (res == -1) {
		return -1; /* An error occured */
	} else if (res == 1) {

		if (rsp_ctrl != (HDLC_XID | HDLC_PF)) {
			return -1; /* Unexpected control field */
		}

		if (rsp_info_size < AA_MIN_RSP_I_SIZE) {
			return -1; /* Too small information size */
		}

		if (rsp_info[0] != AISG_FI) {
			return -1; /* Expected AISG FI */
		}

		if (rsp_info[1] != AISG_GI_USER_DEF) {
			return -1; /* Expected AISG GI */
		}

		gl = rsp_info[2];
		if (gl < AA_MIN_RSP_P_SIZE) {
			return -1; /* Too small parameter size */
		}

		/* Extract Id and Device Type from parameter fields. */
		idx = 0;
		para = &rsp_info[3];
		while (idx < gl) {
			pi = para[idx++];
			pl = para[idx++];
			pv = &para[idx];
			if (pi == AISG_PI_DEVICE_TYPE) {
				if (*pv != device_type) {
					return -1;
				} else {
					/* OK, device type exists. */
				}
			}
			idx += pl;
		}

	} else {
		return -1;
	}

	return 0;
}


int aisg_reset_device(const char *log_prefix,
                      void *handle, uint32_t timeout, uint8_t addr)
{
	uint8_t cmd_info[] = {
		AISG_FI, AISG_GI_USER_DEF, 2, AISG_PI_RESET_DEVICE, 0
	};
	struct timespec rsp_tmo = {
		.tv_sec = 0, .tv_nsec = timeout * 1000000
	};

	if (ecb_dev_begin(handle) != 0) {
		return -1;
	}

	unc_send(log_prefix, handle, addr,
	         HDLC_XID | HDLC_PF, cmd_info, sizeof(cmd_info));

	if (addr != HDLC_ADDR_BC) {
		(void) unc_wait(log_prefix, handle, addr,
		                NULL, NULL, NULL, &rsp_tmo);
		usleep(MODEM_TXRX_TIME);
	}

	ecb_dev_end(handle);

	return 0;
}

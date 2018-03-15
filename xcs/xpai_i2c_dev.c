/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <stdio.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <linux/i2c.h>
#include <linux/i2c-dev.h>
#include "common.h"
#include "log_tpt.h"
#include "xpai_hdr_i2c_if.h"

#define MAX_I2C_PORTS           2
#define XPAI_I2C_PORTS_TO_START 2

static S32 i2c_rdwr(U32 portId, U8 *buffer, U32 length,
                    struct i2c_msg *msgs, U32 nmsgs, int *err)
{
	int fd;
	char file[sizeof("/dev/i2c-4294967295")];
	struct i2c_rdwr_ioctl_data ioc = { msgs, nmsgs };

	if (portId >= MAX_I2C_PORTS) {
		TPT_ERROR(STR("I2C port%u is not valid", portId));
		return XPAI_I2C_WRONG_PORT;
	}

	if ((buffer == NULL) || (length == 0)) {
		TPT_ERROR(STR("Invalid data buffer or length(%u) is used", length));
		return XPAI_I2C_OTHER_ERROR;
	}

	if (snprintf(file, sizeof(file), "/dev/i2c-%u", portId) < 0) {
		TPT_ERROR(STR("snprintf error"));
		return XPAI_I2C_OTHER_ERROR;
	}

	if ((fd = open(file, O_RDWR)) < 0) {
		*err = errno;
		TPT_ERROR(STR("Unable to open I2C device %s with errno: %d", file, errno));
		return XPAI_I2C_OTHER_ERROR;
	}

	if (ioctl(fd, I2C_RDWR, &ioc) == -1) {
		*err = errno;
		close(fd);
		return XPAI_I2C_OTHER_ERROR;
	}

	if (close(fd) == -1) {
		*err = errno;
		TPT_ERROR(STR("Unable to close I2C device %s with errno: %d", file, errno));
		return XPAI_I2C_OTHER_ERROR;
	}

	return length;
}

/****
 *
 *      Function XPAI_I2CWritePort
 *
 *****/
S32 XPAI_I2CWritePort(U32 portId, U32 address, U8 *buffer, U32 length)
{
	S32 ret = 0;
	int err = 0;
	struct i2c_msg msg = { address, 0, length, buffer };

	ret = i2c_rdwr(portId, buffer, length, &msg, 1, &err);

	if (err) {
		TPT_ERROR(STR("I2C write failed: port %u len %u (addr=0x%x) - %s",
		              portId, length, address, strerror(err)));
	}

	return ret;
}

/****
 *
 *      Function XPAI_I2CWriteSubPort
 *
 *****/
S32 XPAI_I2CWriteSubPort(U32 portId,
                         U32 address,
                         U8 *buffer,
                         U32 length,
                         U32 subAddress)
{
	S32 ret = 0;
	int err = 0;
	uint8_t addr_buf = subAddress;
	struct i2c_msg msg[2] = {
		{ address, 0,      1, &addr_buf },
		{ address, 0, length,    buffer }
	};

	ret = i2c_rdwr(portId, buffer, length, msg, 2, &err);

	if (err) {
		TPT_ERROR(STR("I2C write failed: port %u len %u@0x%x (addr=0x%x) - %s",
		              portId, length, addr_buf, address, strerror(err)));
	}

	return ret;
}

/****
 *
 *      Function XPAI_I2CReadPort
 *
 *****/
S32 XPAI_I2CReadPort(U32 portId, U32 address, U8 *buffer, U32 length)
{
	S32 ret = 0;
	int err = 0;
	struct i2c_msg msg = { address, I2C_M_RD, length, buffer };

	ret = i2c_rdwr(portId, buffer, length, &msg, 1, &err);

	if (err) {
		TPT_ERROR(STR("I2C read failed: port %u len %u (addr=0x%x) - %s",
		              portId, length, address, strerror(err)));
	}

	return ret;
}

/****
 *
 *      Function XPAI_I2CReadSubPort
 *
 *****/
S32 XPAI_I2CReadSubPort(U32 portId,
                        U32 address,
                        U8 *buffer,
                        U32 length,
                        U32 subAddress)
{
	S32 ret = 0;
	int err = 0;
	uint8_t addr_buf = subAddress;
	struct i2c_msg msg[2] = {
		{ address,        0,      1, &addr_buf },
		{ address, I2C_M_RD, length,    buffer }
	};

	ret = i2c_rdwr(portId, buffer, length, msg, 2, &err);

	if (err) {
		TPT_ERROR(STR("I2C read failed: port %u len %u@0x%x (addr=0x%x) - %s",
		              portId, length, addr_buf, address, strerror(err)));
	}

	return ret;
}

/****
 *
 *      Function i2c_init
 *
 *****/
int32_t xpai_i2c_port_init(uint32_t portId, uint32_t client_ref)
{
	char file[sizeof("/dev/i2c-4294967295")];

	if (portId >= MAX_I2C_PORTS) {
		TPT_ERROR(STR("client %u: I2C port%u is not valid", client_ref, portId));
		return INIT_I2C_PORT_NOK;
	}

	if (snprintf(file, sizeof(file), "/dev/i2c-%u", portId) < 0) {
		TPT_ERROR(STR("client %u: snprintf error", client_ref));
		return INIT_OTHER_ERROR;
	}

	if (access(file, F_OK) == -1) {
		TPT_ERROR(STR("client %u: %s doesn't exist (errno : %d)", client_ref, file,
		              errno));
		return INIT_OTHER_ERROR;
	}

	if (access(file, R_OK | W_OK) == -1) {
		TPT_ERROR(STR("client %u: failed to read or write %s (errno : %d)", client_ref,
		              file, errno));
		return INIT_OTHER_ERROR;
	}

	return INIT_OK;
}

int32_t xpai_i2c_init(uint32_t client_ref)
{
	int i;
	int32_t res;

	for (i = 0; i < XPAI_I2C_PORTS_TO_START; i++) {
		res = xpai_i2c_port_init(i, client_ref);
		if (res != INIT_OK) {
			return res;
		}
	}

	return INIT_OK;
}

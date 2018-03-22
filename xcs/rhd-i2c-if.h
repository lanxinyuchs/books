/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef _RHD_I2C_H_
#define _RHD_I2C_H_
#include <stdint.h>
#include "rhd-msg-base.h"
#include "conn-establish.h"

#define I2C_SERVER_VERSIONS     1
#define I2C_NOF_PORTS           3
#define DAEMON_NAME             "rhd-i2cd"
#define RHD_I2C_MAILBOX         "RHD_I2C"
#define MAX_MAILBOX_NUM         32

#define RHD_I2C_STRUCTS \
	struct i2c_open_req open_req;     \
	struct i2c_open_cfm open_cfm;     \
	struct i2c_open_rej open_rej;     \
	struct i2c_write_req write_req;   \
	struct i2c_write_cfm write_cfm;   \
	struct i2c_write_rej write_rej;   \
	struct i2c_read_req read_req;     \
	struct i2c_read_cfm read_cfm;           \
	struct i2c_read_rej read_rej;           \
	struct i2c_write_sub_req write_sub_req; \
	struct i2c_write_sub_cfm write_sub_cfm; \
	struct i2c_write_sub_rej write_sub_rej; \
	struct i2c_read_sub_req read_sub_req;   \
	struct i2c_read_sub_cfm read_sub_cfm;   \
	struct i2c_read_sub_rej read_sub_rej;

enum i2c_error_code {
	I2C_ERROR_ADDRESS_NON_ACKNOWLEDGED = -100,
	I2C_ERROR_INCOMPLETE_TRANSFER = -101,
	I2C_ERROR_TIMEOUT = -102,
	I2C_ERROR_ILLEGAL_ADDRESS = -103,
	I2C_ERROR_OTHER = -104,
};

/*
  Lets make a macro for compulsory header fields ...
*/
#define RHD_I2C_MSG_HEADER                      \
	uint32_t msgno;                         \
	uint32_t procedure_ref;                 \
	uint32_t connection_ref;

/* Messages used by the connection establish mechanism.
   the message strucst are defined in conn-establish.h */
#define I2C_CONN_ESTABLISH_REQ  (RHD_I2C_MSG_BASE + 0x01)
#define I2C_CONN_ESTABLISH_CFM  (RHD_I2C_MSG_BASE + 0x02)
#define I2C_CONN_ESTABLISH_REJ  (RHD_I2C_MSG_BASE + 0x03)
#define I2C_CONN_DISCONNECT_REQ (RHD_I2C_MSG_BASE + 0x04)
#define I2C_CONN_DISCONNECT_CFM (RHD_I2C_MSG_BASE + 0x05)
#define I2C_CONN_DISCONNECT_REJ (RHD_I2C_MSG_BASE + 0x06)
#define I2C_CONN_MONITOR_FWD    (RHD_I2C_MSG_BASE + 0x07)

/* Our own messages */
#define RHD_I2C_WRITE_REQ       (RHD_I2C_MSG_BASE + 0x09)
struct i2c_write_req {
	RHD_I2C_MSG_HEADER
	uint32_t address;
	uint32_t length;
	uint8_t data[1];
};

#define RHD_I2C_WRITE_CFM       (RHD_I2C_MSG_BASE + 0x0A)
struct i2c_write_cfm {
	RHD_I2C_MSG_HEADER
	uint32_t length;
};

#define RHD_I2C_WRITE_REJ       (RHD_I2C_MSG_BASE + 0x0B)
struct i2c_write_rej {
	RHD_I2C_MSG_HEADER
	uint32_t error_code;
};

#define RHD_I2C_READ_REQ        (RHD_I2C_MSG_BASE + 0x0C)
struct i2c_read_req {
	RHD_I2C_MSG_HEADER
	uint32_t address;
	uint32_t length;

};

#define RHD_I2C_READ_CFM        (RHD_I2C_MSG_BASE + 0x0D)
struct i2c_read_cfm {
	RHD_I2C_MSG_HEADER
	uint32_t length;
	uint8_t data[1];
};

#define RHD_I2C_READ_REJ        (RHD_I2C_MSG_BASE + 0x0E)
struct i2c_read_rej {
	RHD_I2C_MSG_HEADER
	uint32_t error_code;
};

#define RHD_I2C_WRITE_SUB_REQ   (RHD_I2C_MSG_BASE + 0x0F)
struct i2c_write_sub_req {
	RHD_I2C_MSG_HEADER
	uint32_t address;
	uint32_t length;
	uint32_t sub_address;
	uint8_t sub_address_size;
	uint8_t data[1];
};

#define RHD_I2C_WRITE_SUB_CFM   (RHD_I2C_MSG_BASE + 0x10)
struct i2c_write_sub_cfm {
	RHD_I2C_MSG_HEADER
	uint32_t length;
};

#define RHD_I2C_WRITE_SUB_REJ   (RHD_I2C_MSG_BASE + 0x11)
struct i2c_write_sub_rej {
	RHD_I2C_MSG_HEADER
	uint32_t error_code;
};

#define RHD_I2C_READ_SUB_REQ    (RHD_I2C_MSG_BASE + 0x12)
struct i2c_read_sub_req {
	RHD_I2C_MSG_HEADER
	uint32_t address;
	uint32_t length;
	uint32_t sub_address;
	uint8_t sub_address_size;
};

#define RHD_I2C_READ_SUB_CFM    (RHD_I2C_MSG_BASE + 0x13)
struct i2c_read_sub_cfm {
	RHD_I2C_MSG_HEADER
	uint32_t length;
	uint8_t data[1];
};

#define RHD_I2C_READ_SUB_REJ    (RHD_I2C_MSG_BASE + 0x14)
struct i2c_read_sub_rej {
	RHD_I2C_MSG_HEADER
	uint32_t error_code;
};

#define RHD_I2C_OPEN_REQ       (RHD_I2C_MSG_BASE + 0x15)
struct i2c_open_req {
	RHD_I2C_MSG_HEADER
};

#define RHD_I2C_OPEN_CFM       (RHD_I2C_MSG_BASE + 0x16)
struct i2c_open_cfm {
	RHD_I2C_MSG_HEADER
};

#define RHD_I2C_OPEN_REJ       (RHD_I2C_MSG_BASE + 0x17)
struct i2c_open_rej {
	RHD_I2C_MSG_HEADER
};


#endif //_RHD_I2C_H_

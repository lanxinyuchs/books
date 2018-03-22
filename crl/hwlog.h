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

#ifndef HWLOG_H__
#define HWLOG_H__

#include <stdint.h>

#include "hwli.h"

#define HWLOG_MAILBOX             "HWLOG"

#define HWLOG_PROTOCOL_REVISION   1

#define HWLOG_BASE                0x2c00

#define HWLOG_REQ                 (HWLOG_BASE + 0x00)
#define HWLOG_CFM                 (HWLOG_BASE + 0x10)
#define HWLOG_REJ                 (HWLOG_BASE + 0x20)
#define HWLOG_DATA                (HWLOG_BASE + 0x30)
#define HWLOG_ACK                 (HWLOG_BASE + 0x40)

#define HWLOG_CONNECT_REQ         (HWLOG_REQ + 0x0)
#define HWLOG_CONNECT_CFM         (HWLOG_CFM + 0x0)
#define HWLOG_CONNECT_REJ         (HWLOG_REJ + 0x0)

#define HWLOG_LOGREAD_REQ         (HWLOG_REQ + 0x1)
#define HWLOG_LOGREAD_CFM         (HWLOG_CFM + 0x1)
#define HWLOG_LOGREAD_REJ         (HWLOG_REJ + 0x1)
#define HWLOG_LOGREAD_DATA        (HWLOG_DATA + 0x1)
#define HWLOG_LOGREAD_ACK         (HWLOG_ACK + 0x1)

#define HWLOG_WRITE_REQ           (HWLOG_REQ + 0x2)
#define HWLOG_WRITE_CFM           (HWLOG_CFM + 0x2)
#define HWLOG_WRITE_REJ           (HWLOG_REJ + 0x2)

#define HWLOG_LOGERASE_REQ        (HWLOG_REQ + 0x3)
#define HWLOG_LOGERASE_CFM        (HWLOG_CFM + 0x3)
#define HWLOG_LOGERASE_REJ        (HWLOG_REJ + 0x3)


/* connection established */
struct hwlog_connect_req {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	uint32_t nbr_of_supported_protocol_revisions;
	uint32_t protocol_revision;
};

struct hwlog_connect_cfm {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	uint32_t protocol_revision;
};

struct hwlog_connect_rej {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
};

/* logread */
struct hwlog_logread_req {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	uint32_t data_per_ack;            /* 0 sends an ack after last entry */
};

struct hwlog_logread_cfm {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	uint32_t len;
};

struct hwlog_logread_rej {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
};

struct hwlog_logread_data {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	int32_t  sequence_number;
	struct hwli_entry entry;
};

/* Use HWLOG_LOGREAD_ABORT as sequence_number from client to abort transfer */
#define HWLOG_LOGREAD_ABORT -1

struct hwlog_logread_ack {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	int32_t  sequence_number;
};

/* write */
struct hwlog_write_req {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	char id[4];
	uint32_t filter;
	uint32_t filter_on_msg;
	char msg[112];
};

struct hwlog_write_cfm {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
};

struct hwlog_write_rej {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
	int32_t  error_code;
};

/* logerase */
struct hwlog_logerase_req {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
};

struct hwlog_logerase_cfm {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
};

struct hwlog_logerase_rej {
	uint32_t msgno;
	uint32_t procedure_reference;
	uint32_t connection_reference;
};

#define HWLOG_SIGNAL_STRUCTS                              \
	struct hwlog_connect_req    hwlog_connect_req;        \
	struct hwlog_connect_cfm    hwlog_connect_cfm;        \
	struct hwlog_connect_rej    hwlog_connect_rej;        \
	struct hwlog_logread_req    hwlog_logread_req;        \
	struct hwlog_logread_cfm    hwlog_logread_cfm;        \
	struct hwlog_logread_rej    hwlog_logread_rej;        \
	struct hwlog_logread_data   hwlog_logread_data;       \
	struct hwlog_logread_ack    hwlog_logread_ack;        \
	struct hwlog_write_req      hwlog_write_req;          \
	struct hwlog_write_cfm      hwlog_write_cfm;          \
	struct hwlog_write_rej      hwlog_write_rej;          \
	struct hwlog_logerase_req   hwlog_logerase_req;       \
	struct hwlog_logerase_cfm   hwlog_logerase_cfm;       \
	struct hwlog_logerase_rej   hwlog_logerase_rej        \

#endif /* HWLOG_H__ */

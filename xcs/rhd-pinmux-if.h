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

#ifndef _RHD_PINMUX_H_
#define _RHD_PINMUX_H_
#include <stdint.h>
#include "rhd-msg-base.h"
#include "conn-establish.h"
#include "pinmux.h"

#define PINMUX_SERVER_VERSIONS     1
#define DAEMON_NAME                "rhd-pinmuxd"
#define RHD_PINMUX_MAILBOX         "RHD_PINMUX"
#define MAX_MAILBOX_NUM            32
#define MAX_LIST_INFO_PIN_LEN           6
#define MAX_LIST_INFO_FUNC_LEN          7
#define MAX_LIST_INFO_PULLSEL_LEN       14
#define MAX_LIST_INFO_IMPSEL_LEN        11
#define MAX_LIST_INFO_SLEW_LEN          8
#define MAX_LIST_INFO_MBOX_NAME_LEN     34
#define MAX_LIST_INFO_HANDLE_LEN        10
#define MAX_LIST_INFO_PIN_MARGIN_LEN    2
#define MAX_LIST_INFO_NEW_LINE          2

#define MAX_LIST_INFO_STR_LEN  \
        (MAX_LIST_INFO_PIN_LEN + MAX_LIST_INFO_FUNC_LEN + \
         MAX_LIST_INFO_PULLSEL_LEN + MAX_LIST_INFO_IMPSEL_LEN + \
         MAX_LIST_INFO_SLEW_LEN + MAX_LIST_INFO_MBOX_NAME_LEN + \
         MAX_LIST_INFO_HANDLE_LEN + MAX_LIST_INFO_PIN_MARGIN_LEN + \
         MAX_LIST_INFO_NEW_LINE) * 32

#define RHD_PINMUX_STRUCTS                              \
	struct pinmux_reserve_req reserve_req;          \
	struct pinmux_reserve_cfm reserve_cfm;          \
	struct pinmux_reserve_rej reserve_rej;          \
	struct pinmux_unreserve_req unreserve_req;      \
	struct pinmux_unreserve_cfm unreserve_cfm;      \
	struct pinmux_unreserve_rej unreserve_rej;      \
	struct pinmux_set_func_req set_func_req;        \
	struct pinmux_set_func_cfm set_func_cfm;        \
	struct pinmux_set_func_rej set_func_rej;        \
	struct pinmux_get_func_req get_func_req;        \
	struct pinmux_get_func_cfm get_func_cfm;        \
	struct pinmux_get_func_rej get_func_rej;        \
	struct pinmux_set_cfg_req set_cfg_req;          \
	struct pinmux_set_cfg_cfm set_cfg_cfm;          \
	struct pinmux_set_cfg_rej set_cfg_rej;          \
	struct pinmux_get_cfg_req get_cfg_req;          \
	struct pinmux_get_cfg_cfm get_cfg_cfm;          \
	struct pinmux_get_cfg_rej get_cfg_rej;          \
	struct pinmux_list_req list_req;                \
	struct pinmux_list_cfm list_cfm;                \
	struct pinmux_list_rej list_rej;                \
	struct pinmux_list_ind list_ind;                \
	struct pinmux_list_end_ind list_end_ind;		\
	struct pinmux_dump_req dump_req;                \
	struct pinmux_dump_cfm dump_cfm;                \
	struct pinmux_dump_rej dump_rej;


/*
  Lets make a macro for compulsory header fields ...
*/
#define RHD_PINMUX_MSG_HEADER                   \
	uint32_t msgno;                         \
	uint32_t procedure_ref;                 \
	uint32_t connection_ref;

/* Messages used by the connection establish mechanism.
   the message strucst are defined in conn-establish.h */
#define PINMUX_CONN_ESTABLISH_REQ  (RHD_PINMUX_MSG_BASE + 0x01)
#define PINMUX_CONN_ESTABLISH_CFM  (RHD_PINMUX_MSG_BASE + 0x02)
#define PINMUX_CONN_ESTABLISH_REJ  (RHD_PINMUX_MSG_BASE + 0x03)
#define PINMUX_CONN_DISCONNECT_REQ (RHD_PINMUX_MSG_BASE + 0x04)
#define PINMUX_CONN_DISCONNECT_CFM (RHD_PINMUX_MSG_BASE + 0x05)
#define PINMUX_CONN_DISCONNECT_REJ (RHD_PINMUX_MSG_BASE + 0x06)
#define PINMUX_CONN_MONITOR_FWD    (RHD_PINMUX_MSG_BASE + 0x07)

/* Our own messages */
#define RHD_PINMUX_RESERVE_REQ         (RHD_PINMUX_MSG_BASE + 0x09)
struct pinmux_reserve_req {
	RHD_PINMUX_MSG_HEADER
	uint32_t number_of_pins;
	uint32_t pin[1];
};

#define RHD_PINMUX_RESERVE_CFM         (RHD_PINMUX_MSG_BASE + 0x0A)
struct pinmux_reserve_cfm {
	RHD_PINMUX_MSG_HEADER
	pinmux_handle_t handle;
};

#define RHD_PINMUX_RESERVE_REJ         (RHD_PINMUX_MSG_BASE + 0x0B)
struct pinmux_reserve_rej {
	RHD_PINMUX_MSG_HEADER
	pinmux_status_t error_code;
};

#define RHD_PINMUX_UNRESERVE_REQ       (RHD_PINMUX_MSG_BASE + 0x0C)
struct pinmux_unreserve_req {
	RHD_PINMUX_MSG_HEADER
	pinmux_handle_t handle;
};

#define RHD_PINMUX_UNRESERVE_CFM       (RHD_PINMUX_MSG_BASE + 0x0D)
struct pinmux_unreserve_cfm {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_UNRESERVE_REJ      (RHD_PINMUX_MSG_BASE + 0x0E)
struct pinmux_unreserve_rej {
	RHD_PINMUX_MSG_HEADER
	pinmux_status_t error_code;
};

#define RHD_PINMUX_SET_FUNC_REQ        (RHD_PINMUX_MSG_BASE + 0x0F)
struct pinmux_set_func_req {
	RHD_PINMUX_MSG_HEADER
	pinmux_handle_t handle;
	pinmux_func_type_t func_type;
	uint32_t number_of_pins;
	uint32_t pin[1];
};

#define RHD_PINMUX_SET_FUNC_CFM        (RHD_PINMUX_MSG_BASE + 0x10)
struct pinmux_set_func_cfm {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_SET_FUNC_REJ        (RHD_PINMUX_MSG_BASE + 0x11)
struct pinmux_set_func_rej {
	RHD_PINMUX_MSG_HEADER
	pinmux_status_t error_code;
};

#define RHD_PINMUX_GET_FUNC_REQ        (RHD_PINMUX_MSG_BASE + 0x12)
struct pinmux_get_func_req {
	RHD_PINMUX_MSG_HEADER
	pinmux_handle_t handle;
	uint32_t pin;
};

#define RHD_PINMUX_GET_FUNC_CFM        (RHD_PINMUX_MSG_BASE + 0x13)
struct pinmux_get_func_cfm {
	RHD_PINMUX_MSG_HEADER
	pinmux_func_type_t func_type;
};

#define RHD_PINMUX_GET_FUNC_REJ        (RHD_PINMUX_MSG_BASE + 0x14)
struct pinmux_get_func_rej {
	RHD_PINMUX_MSG_HEADER
	pinmux_status_t error_code;
};

#define RHD_PINMUX_SET_CFG_REQ         (RHD_PINMUX_MSG_BASE + 0x15)
struct pinmux_set_cfg_req {
	RHD_PINMUX_MSG_HEADER
	pinmux_handle_t handle;
	pinmux_cfg_type_t cfg_type;
	pinmux_cfg_value_t cfg_value;
	uint32_t number_of_pins;
	uint32_t pin[1];
};

#define RHD_PINMUX_SET_CFG_CFM         (RHD_PINMUX_MSG_BASE + 0x16)
struct pinmux_set_cfg_cfm {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_SET_CFG_REJ         (RHD_PINMUX_MSG_BASE + 0x17)
struct pinmux_set_cfg_rej {
	RHD_PINMUX_MSG_HEADER
	pinmux_status_t error_code;
};

#define RHD_PINMUX_GET_CFG_REQ        (RHD_PINMUX_MSG_BASE + 0x18)
struct pinmux_get_cfg_req {
	RHD_PINMUX_MSG_HEADER
	pinmux_handle_t handle;
	uint32_t pin;
	pinmux_cfg_type_t cfg_type;
};

#define RHD_PINMUX_GET_CFG_CFM        (RHD_PINMUX_MSG_BASE + 0x19)
struct pinmux_get_cfg_cfm {
	RHD_PINMUX_MSG_HEADER
	pinmux_cfg_value_t cfg_value;
};

#define RHD_PINMUX_GET_CFG_REJ        (RHD_PINMUX_MSG_BASE + 0x1A)
struct pinmux_get_cfg_rej {
	RHD_PINMUX_MSG_HEADER
	pinmux_status_t error_code;
};

typedef enum {
	PINMUX_LIST_TYPE_INVALID = -1,
	PINMUX_LIST_TYPE_SHORT = 0,
	PINMUX_LIST_TYPE_ALL,
} pinmux_list_type_t;

#define RHD_PINMUX_LIST_REQ        (RHD_PINMUX_MSG_BASE + 0x1B)
struct pinmux_list_req {
	RHD_PINMUX_MSG_HEADER
	uint32_t type;
	uint32_t verbose_mode;
};

#define RHD_PINMUX_LIST_CFM        (RHD_PINMUX_MSG_BASE + 0x1C)
struct pinmux_list_cfm {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_LIST_REJ        (RHD_PINMUX_MSG_BASE + 0x1D)
struct pinmux_list_rej {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_LIST_IND        (RHD_PINMUX_MSG_BASE + 0x1E)
struct pinmux_list_ind {
	RHD_PINMUX_MSG_HEADER
	char str[MAX_LIST_INFO_STR_LEN];
};

#define RHD_PINMUX_LIST_END_IND    (RHD_PINMUX_MSG_BASE + 0x1F)
struct pinmux_list_end_ind {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_DUMP_REQ        (RHD_PINMUX_MSG_BASE + 0x20)
struct pinmux_dump_req {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_DUMP_CFM        (RHD_PINMUX_MSG_BASE + 0x21)
struct pinmux_dump_cfm {
	RHD_PINMUX_MSG_HEADER
};

#define RHD_PINMUX_DUMP_REJ        (RHD_PINMUX_MSG_BASE + 0x22)
struct pinmux_dump_rej {
	RHD_PINMUX_MSG_HEADER
};
#endif //_RHD_PINMUX_H_

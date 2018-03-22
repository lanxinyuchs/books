/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#ifndef _TEST_SERVER_H_
#define _TEST_SERVER_H_

#include "conn-establish-helper.h"

#define TEST_SERVER_VERSIONS  4,3,2,1

#define TEST_SERVER_NAME     "test_server"

#define ERROR_1 0x12345678

#define TEST_MSG_BASE 0x12340000
#define TEST_CONN_ESTABLISH_REQ  (TEST_MSG_BASE + 0x01)
#define TEST_CONN_ESTABLISH_CFM  (TEST_MSG_BASE + 0x02)
#define TEST_CONN_ESTABLISH_REJ  (TEST_MSG_BASE + 0x03)
#define TEST_CONN_DISCONNECT_REQ (TEST_MSG_BASE + 0x04)
#define TEST_CONN_DISCONNECT_CFM (TEST_MSG_BASE + 0x05)
#define TEST_CONN_DISCONNECT_REJ (TEST_MSG_BASE + 0x06)
#define TEST_CONN_MONITOR_FWD    (TEST_MSG_BASE + 0x07)

#define TEST_MESSAGE_REQ               (TEST_MSG_BASE + 0x11)
#define TEST_MESSAGE_CFM               (TEST_MSG_BASE + 0x12)
#define TEST_MESSAGE_REJ               (TEST_MSG_BASE + 0x13)
#define TEST_CD_SET_REQ                (TEST_MSG_BASE + 0x14)
#define TEST_CD_SET_CFM                (TEST_MSG_BASE + 0x15)
#define TEST_CD_SET_REJ                (TEST_MSG_BASE + 0x16)
#define TEST_CD_CLEAR_REQ              (TEST_MSG_BASE + 0x17)
#define TEST_CD_CLEAR_CFM              (TEST_MSG_BASE + 0x18)
#define TEST_CD_CLEAR_REJ              (TEST_MSG_BASE + 0x19)
#define TEST_RE_INIT_REQ               (TEST_MSG_BASE + 0x1a)
#define TEST_RE_INIT_CFM               (TEST_MSG_BASE + 0x1b)
#define TEST_RE_INIT_REJ               (TEST_MSG_BASE + 0x1c)
#define TEST_SET_TMO_REQ               (TEST_MSG_BASE + 0x1d)
#define TEST_SET_TMO_RSP               (TEST_MSG_BASE + 0x1e)
#define TEST_GET_CLIENT_INFO_REQ       (TEST_MSG_BASE + 0x1f)
#define TEST_GET_CLIENT_INFO_RSP       (TEST_MSG_BASE + 0x20)
#define TEST_PRINT_TEST_INFO_REQ       (TEST_MSG_BASE + 0x21)
#define TEST_PRINT_TEST_INFO_CFM       (TEST_MSG_BASE + 0x22)

/*callback testing messages*/
#define TEST_SET_CONN_REJECT_CAUSE_REQ (TEST_MSG_BASE + 0x31)
#define TEST_SET_CONN_REJECT_CAUSE_CFM (TEST_MSG_BASE + 0x32)
#define TEST_SET_MASTER_MBOX_REQ       (TEST_MSG_BASE + 0x33)
#define TEST_SET_MASTER_MBOX_CFM       (TEST_MSG_BASE + 0x34)
#define TEST_CLIENT_DISCONNECT_IND     (TEST_MSG_BASE + 0x35)
#define TEST_CLIENT_DIED_IND           (TEST_MSG_BASE + 0x36)
#define TEST_ECHO_REQ                  (TEST_MSG_BASE + 0x37)
#define TEST_ECHO_RSP                  (TEST_MSG_BASE + 0x38)


#define CONN_ESTABLISH_MESSAGES_STRUCT(name)          \
	struct conn_establish_msg_numbers  name =     \
	{                                             \
		TEST_CONN_ESTABLISH_REQ,              \
		TEST_CONN_ESTABLISH_CFM,              \
		TEST_CONN_ESTABLISH_REJ,              \
		TEST_CONN_DISCONNECT_REQ,             \
		TEST_CONN_DISCONNECT_CFM,             \
		TEST_CONN_DISCONNECT_REJ,             \
		TEST_CONN_MONITOR_FWD                 \
	}


#define TEST_MESSAGES                                                   \
	struct test_message_req  test_message_req;                      \
	struct test_message_cfm  test_message_cfm;                      \
	struct test_message_rej  test_message_rej;                      \
	struct test_cd_set_req   test_cd_set_req;                       \
	struct test_cd_set_cfm   test_cd_set_cfm;                       \
	struct test_cd_set_rej   test_cd_set_rej;                       \
	struct test_cd_clear_req test_cd_clear_req;                     \
	struct test_cd_clear_cfm test_cd_clear_cfm;                     \
	struct test_cd_clear_rej test_cd_clear_rej;                     \
	struct test_re_init_req  test_re_init_req;                      \
	struct test_re_init_cfm  test_re_init_cfm;                      \
	struct test_re_init_rej  test_re_init_rej;                      \
	struct test_set_tmo_req  test_set_tmo_req;                      \
	struct test_set_tmo_rsp  test_set_tmo_rsp;                      \
	struct test_print_test_info_req test_print_test_info_req;       \
	struct test_print_test_info_cfm test_print_test_info_cfm;       \
	struct test_get_client_info_req test_get_client_info_req;       \
	struct test_get_client_info_rsp test_get_client_info_rsp;       \
	struct test_set_master_mbox_req test_set_master_mbox_req;       \
	struct test_set_master_mbox_cfm test_set_master_mbox_cfm;       \
	struct test_set_conn_reject_cause_req test_set_conn_reject_cause_req;   \
	struct test_set_conn_reject_cause_cfm test_set_conn_reject_cause_cfm;   \
	struct test_client_died_ind     test_client_died_ind


#define TEST_MSG_HEADER         \
	uint32_t msgno;         \
	uint32_t procedure_ref; \
	uint32_t connection_ref

struct example_message_req {
	TEST_MSG_HEADER;
	uint32_t your_members;
};

struct test_message_req {
	TEST_MSG_HEADER;
	uint32_t wanted_reply_msgno;
};
struct test_message_cfm {
	TEST_MSG_HEADER;
	uint32_t client_data;
	uint32_t protocol_version;
};
struct test_message_rej {
	TEST_MSG_HEADER;
	int32_t error_code;
};
struct test_cd_set_req {
	TEST_MSG_HEADER;
	uint32_t server_ref;
	uint32_t data;
};
struct test_cd_set_cfm {
	TEST_MSG_HEADER;
};
struct test_cd_set_rej {
	TEST_MSG_HEADER;
	char text[100];
	int32_t error;
};
struct test_cd_clear_req {
	TEST_MSG_HEADER;
	uint32_t server_ref;
};
struct test_cd_clear_cfm {
	TEST_MSG_HEADER;
	uint32_t data;
};
struct test_cd_clear_rej {
	TEST_MSG_HEADER;
	char text[100];
	int32_t error;
	uint32_t data;
};

/*needs to be working without an established connection*/
struct test_re_init_req {
	uint32_t msgno;
	uint32_t max_clients;
	uint32_t use_callbacks;
};
struct test_re_init_cfm {
	uint32_t msgno;
};
struct test_re_init_rej {
	uint32_t msgno;
};

/*needs to be working without an established connection*/
struct test_set_tmo_req {
	uint32_t msgno;
	uint32_t time_out;
};
struct test_set_tmo_rsp {
	uint32_t msgno;
};

struct test_get_client_info_req {
	TEST_MSG_HEADER;
	uint32_t server_ref;
};
struct test_get_client_info_rsp {
	TEST_MSG_HEADER;
	uint32_t result;
	uint32_t client_ref;
};

/*needs to be working without an established connection*/
struct test_print_test_info_req {
	uint32_t msgno;
	char str[100];
};
struct test_print_test_info_cfm {
	uint32_t msgno;
};

struct test_set_conn_reject_cause_req {
	TEST_MSG_HEADER;
	uint32_t reject_cause;
};
struct test_set_conn_reject_cause_cfm {
	TEST_MSG_HEADER;
};
struct test_set_master_mbox_req {
	TEST_MSG_HEADER;
	itc_mbox_id_t mbox;
};
struct test_set_master_mbox_cfm {
	TEST_MSG_HEADER;
};
struct test_client_died_ind {
	uint32_t msgno;
	itc_mbox_id_t mbox;
	uint32_t client_ref;
	uint32_t server_ref;
};

#endif

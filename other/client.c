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

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <itc.h>
#include <unistd.h>
#include <time.h>
#include <arpa/inet.h>

#include "test.h"

#ifdef LOG_LTTNG
#define TRACEPOINT_DEFINE
#define TRACEPOINT_PROBE_DYNAMIC_LINKAGE
#endif
#include "log.h"

#define MAILBOX_SIZE 5


#ifdef LOG_CONSOLE
#   define PRINT(format,...) printf("Test %d:%4d: " format "\n",  \
	                                test_number,__LINE__, ##__VA_ARGS__)
#else
#   define PRINT(format,...) \
	do { \
		printf("Test %d:%4d: " format "\n", \
		       test_number,__LINE__, ##__VA_ARGS__); \
		log_info("Test %d: " format, \
		         test_number, ##__VA_ARGS__); \
	} while(0)
#endif

#ifdef LOG_CONSOLE
#	define TEST_INFO(str) \
	print_test_info(server_mbox,str,test_number,__LINE__)
#endif
#ifdef LOG_LTTNG
#   define TEST_INFO(str) \
	do { \
		log_info("\n********************************************\n" \
		         "*** Test %d: %s\n" \
		         "********************************************\n", \
		         test_number,str); \
		print_test_info(server_mbox,str,test_number,__LINE__); \
	} while(0)
#endif
#ifdef LOG_SYSLOG
#   define TEST_INFO(str) \
	do { \
		log_info("********************************************"); \
		log_info("*** Test %d: %s", test_number,str); \
		log_info("********************************************"); \
		print_test_info(server_mbox,str,test_number,__LINE__); \
	} while(0)
#endif

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	TEST_MESSAGES;
	conn_establish_req_t   conn_establish_req;
	conn_establish_cfm_t   conn_establish_cfm;
	conn_establish_rej_t   conn_establish_rej;
};

#define MY_CONN_TMO 1000
#define OK   0
#define FAIL 1

static uint32_t test_number = 0;
static uint32_t procedure_ref = 0;
static CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
static char mailbox_name[50];
static itc_mbox_id_t server_mbox;

/*Number of clients to connect to check no limitation of clients */
#define INFINITE_NUMBER 123
static uint32_t inf_server_ref[INFINITE_NUMBER];


static void print_test_info(itc_mbox_id_t mbox, char *str,
                            int test_number, int line)
{
	union itc_msg *msg = NULL;
	uint32_t filter[] = {1, TEST_PRINT_TEST_INFO_CFM};
	printf("********************************************\n");
#ifdef DEBUG
	printf("*** Test %d:%d %s \n", test_number, line, str);
#else
	printf("*** Test %d: %s \n", test_number, str);
#endif
	printf("********************************************\n");

	/* Send info about test case to server so we can synchronize
	   the output */
	msg = itc_alloc(sizeof(struct test_print_test_info_req),
	                TEST_PRINT_TEST_INFO_REQ);

	snprintf(msg->test_print_test_info_req.str,
	         sizeof(msg->test_print_test_info_req.str),
	         "Test %d %s", test_number, str);
	itc_send(&msg, mbox, ITC_MY_MBOX);
	msg = itc_receive(filter, MY_CONN_TMO, mbox);
	if(!msg) {
		PRINT("Timeout! Server did not reply within 1 second");
	}
	if(msg) itc_free(&msg);
}

#define MY_REJECT_CAUSE 99

char *reason(uint32_t err_code)
{
	switch(err_code) {
	case CONN_ESTABLISH_SUCCESS:
		return "CONN_ESTABLISH_SUCCESS";
	case CONN_ESTABLISH_REJ_TIME_OUT:
		return "CONN_ESTABLISH_REJ_TIME_OUT";
	case CONN_ESTABLISH_REJ_ALREADY_CONNECTED:
		return "CONN_ESTABLISH_REJ_ALREADY_CONNECTED";
	case CONN_ESTABLISH_REJ_UNSUPPORTED_VERSION:
		return "CONN_ESTABLISH_REJ_UNSUPPORTED_VERSION";
	case CONN_ESTABLISH_REJ_NOT_A_CLIENT:
		return "CONN_ESTABLISH_REJ_NOT_A_CLIENT";
	case CONN_ESTABLISH_REJ_INVALID_PARAMETER:
		return "CONN_ESTABLISH_REJ_INVALID_PARAMETER";
	case CONN_ESTABLISH_REJ_MAX_CLIENTS_REACHED:
		return "CONN_ESTABLISH_REJ_MAX_CLIENTS_REACHED";
	case CONN_ESTABLISH_REJ_OUT_OF_MEMORY:
		return "CONN_ESTABLISH_REJ_OUT_OF_MEMORY";
	case MY_REJECT_CAUSE:
		return "MY_REJECT_CAUSE";
	default:
		return "Unknown error";
	}
}


int send_set_client_data( itc_mbox_id_t server_mbox,
                          uint32_t real_server_ref,
                          uint32_t test_server_ref,
                          uint32_t data,
                          int expecting_error)
{
	uint32_t filter[] = {2, TEST_CD_SET_CFM, TEST_CD_SET_REJ};
	union itc_msg *msg = itc_alloc(sizeof(struct test_cd_set_req),
			                               TEST_CD_SET_REQ);
	msg->test_cd_set_req.server_ref = test_server_ref;
	msg->test_cd_set_req.data = data;
	msg->test_cd_set_req.connection_ref = real_server_ref;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(filter, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout waiting for TEST_CD_SET_CFM/_REJ");
		return FAIL;
	}
	if(msg->msgno == TEST_CD_SET_CFM) {
		itc_free(&msg);
		if(expecting_error) {
			PRINT("TEST_CD_SET_REQ unexpectedly succeded");
			return FAIL;
		}
		return OK;
	}
	if((msg->test_cd_set_rej.error == expecting_error)) {
		PRINT("Expected result : %s", msg->test_cd_set_rej.text);
		itc_free(&msg);
		return OK;
	}
	PRINT("Unexpected result : %s", msg->test_cd_set_rej.text);
	itc_free(&msg);
	return FAIL;
}
int send_clear_client_data( itc_mbox_id_t server_mbox,
                            uint32_t real_server_ref,
                            uint32_t test_server_ref,
                            uint32_t expected_data,
                            int expecting_error)
{
	uint32_t filter[] = {2, TEST_CD_CLEAR_CFM, TEST_CD_CLEAR_REJ};
	union itc_msg *msg = itc_alloc(sizeof(struct test_cd_clear_req),
			                               TEST_CD_CLEAR_REQ);
	msg->test_cd_clear_req.server_ref = test_server_ref;
	msg->test_cd_clear_req.connection_ref = real_server_ref;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(filter, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout waiting for TEST_CD_CLEAR_CFM/_REJ");
		return FAIL;
	}
	if(msg->msgno == TEST_CD_CLEAR_CFM) {
		if(expecting_error) {
			PRINT("TEST_CD_CLEAR_REQ unexpectedly succeded, "
			       "expected data was 0x%08x, received data was 0x%08x",
			       expected_data, msg->test_cd_clear_cfm.data);
			itc_free(&msg);
			return FAIL;
		}

		if(expected_data != msg->test_cd_clear_cfm.data) {
			PRINT("TEST_CD_CLEAR_REQ succeded but wrong data was received, "
			       "expected data was 0x%08x, received data was 0x%08x)",
			       expected_data, msg->test_cd_clear_cfm.data);
			itc_free(&msg);
			return FAIL;
		}
		itc_free(&msg);
		return OK;
	}
	if((msg->test_cd_clear_rej.error == expecting_error)) {
		PRINT("Expected result : %s "
		       "(expected data was 0x%08x, received data was 0x%08x)",
		       msg->test_cd_clear_rej.text,
		       expected_data, msg->test_cd_clear_rej.data);
		itc_free(&msg);
		return OK;
	}
	PRINT("Unexpected result : %s"
	       "(expected data was 0x%08x, received data was 0x%08x)",
	       msg->test_cd_clear_rej.text,
	       expected_data, msg->test_cd_clear_rej.data);
	itc_free(&msg);
	return FAIL;
}

int re_init_server(uint32_t max_clients, uint32_t use_callbacks)
{
	union itc_msg *msg;
	/* BEGIN THIS_IS_REALLY_UGLY */
	/* Calling conn_establish_server_init() more than once for
	   one server is totally forbidden. When we do this here
	   to set a new the max numbers of clients we lose all
	   connections and get a memory leak.... but this is test code.*/
	msg = itc_alloc(sizeof(struct test_re_init_req),
	                TEST_RE_INIT_REQ);
	msg->test_re_init_req.max_clients = max_clients;
	msg->test_re_init_req.use_callbacks = use_callbacks;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout! Server did not reply within 1 second");
		return FAIL;
	}

	if(msg->msgno != TEST_RE_INIT_CFM) {
		PRINT("Received unexpected msgno; "
		       "expected: 0x%08x , received 0x%08x",
		       TEST_RE_INIT_CFM, msg->msgno);
		itc_free(&msg);
		return FAIL;
	}
	/* END THIS_IS_REALLY_UGLY */
	itc_free(&msg);
	return OK;
}

/****************************************************************
 *  Test 1 : Normal operation
 ****************************************************************/
int test_1( void )
{
	union itc_msg *msg = NULL;
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t server_ref;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;

	test_number = 1;
	TEST_INFO("Starting : Normal operation. "
	          "Connection, signal sending, disconnection");
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	if(selected_version != requested_versions_1[0]) {
		PRINT("Wrong selected protocol version, "
		       "expected: 0x%08x, got: 0x%08x",
		       requested_versions_1[0], selected_version);
		return FAIL;
	}

	msg = itc_alloc(sizeof(struct test_message_req),
	                TEST_MESSAGE_REQ);
	msg->test_message_req.procedure_ref = ++procedure_ref;
	msg->test_message_req.connection_ref = server_ref;
	msg->test_message_req.wanted_reply_msgno = TEST_MESSAGE_CFM;

	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout! Server did not reply within 1 second");
		return FAIL;
	}

	if(msg->any_msg.procedure_ref != procedure_ref) {
		PRINT("Server replied with invalid procedure_ref; "
		       "expected 0x%08x, received 0x%08x",
		       procedure_ref, msg->any_msg.procedure_ref);
		itc_free(&msg);
		return FAIL;
	}
	if(msg->any_msg.connection_ref != client_ref) {
		PRINT("Server replied with invalid connection_ref; "
		       "expected 0x%08x, received 0x%08x",
		       client_ref, msg->any_msg.connection_ref);
		itc_free(&msg);
		return FAIL;
	}

	if(msg->msgno != TEST_MESSAGE_CFM) {
		PRINT("Received unexpected msgno; "
		       "expected: 0x%08x , received 0x%08x",
		       TEST_MESSAGE_CFM, msg->msgno);
		itc_free(&msg);
		return FAIL;
	}

	if(msg->test_message_cfm.protocol_version != selected_version) {
		PRINT("Server a and client have different oppinion about "
		       "selected protocol version. "
		       "server: 0x%08x , client: 0x%08x",
		       msg->test_message_cfm.protocol_version,
		       selected_version);
		itc_free(&msg);
		return FAIL;
	}

	itc_free(&msg);
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 2 : Conn establish twice - different clients
 ****************************************************************/
int test_2( void )
{
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t client_ref2 = 1235;
	uint32_t server_ref;
	uint32_t server_ref2;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t requested_versions_2[] = {2, 3};
	uint32_t selected_version;
	uint32_t selected_version2;

	test_number = 2;
	TEST_INFO("Starting : Two clients. "
	          "Connect 1, connect 2, disconnect 2, disconnect 1.");
	/*Connecting first*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	/*Connecting second*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref2,
	              sizeof(requested_versions_2) / sizeof(requested_versions_2[0]),
	              requested_versions_2,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref2,
	              &selected_version2);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	/*test of first selected version is done in Test 1*/
	if(selected_version2 != requested_versions_2[0]) {
		PRINT("Wrong selected protocol version, "
		       "expected: 0x%08x, got: 0x%08x",
		       requested_versions_2[0], selected_version2);
		return FAIL;
	}

	/*Disconnecting second */
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref2,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	/*Disconnecting first */
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 3 : Disconnection of an unknown client
 ****************************************************************/
int test_3( void )
{
	uint32_t res;
	uint32_t server_ref = ((2 ^ 32) - 42); /*Just a large number*/
	test_number = 3;
	TEST_INFO("Starting : Disconnection of an unknown client");

	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_REJ_NOT_A_CLIENT) {
		PRINT("Connection disconnect returned "
		       "unexpected return code 0x%08x", res);
		return FAIL;
	}
	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 4 : Conn establish twice - same client
 ****************************************************************/
int test_4( void )
{
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t server_ref;
	uint32_t server_ref2;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;
	uint32_t selected_version2;

	test_number = 4;
	TEST_INFO("Starting : Conn establish twice - same client.");
	/*Connecting first*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	/*Connecting second time*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref2,
	              &selected_version2);
	if(res != CONN_ESTABLISH_REJ_ALREADY_CONNECTED) {
		PRINT("Connection establish returned "
		       "unexpected result code :0x%08x", res);
		return FAIL;
	}
	/*Disconnecting first */
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	TEST_INFO("Test passed.");

	return OK;
}

/****************************************************************
 *  Test 5 : Set/clear client_data
 ****************************************************************/
int test_5( void )
{
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t server_ref;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;

	test_number = 5;
	TEST_INFO("Starting : Set/Clear client_data");
	/*Connecting first*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	PRINT("Set client data - normal");
	if(send_set_client_data(server_mbox, server_ref, server_ref,
	                        0x12345678,
	                        CONN_ESTABLISH_CLIENT_DATA_OK) != OK)
		return FAIL;
	PRINT("Set client data again - checking data already set.");
	if(send_set_client_data(server_mbox, server_ref, server_ref,
	                        0x12345678,
	                        CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET) != OK)
		return FAIL;
	PRINT("Set client data for non-exinstent client.");
	if(send_set_client_data(server_mbox, server_ref, server_ref + 1,
	                        0x12345678,
	                        CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST) != OK)
		return FAIL;

	PRINT("Clear client data - normal");
	if(send_clear_client_data(server_mbox, server_ref, server_ref,
	                          0x12345678,
	                          CONN_ESTABLISH_CLIENT_DATA_OK) != OK)
		return FAIL;
	PRINT("Clear client data again - returning NULL (represented as data = 0).");
	if(send_clear_client_data(server_mbox, server_ref, server_ref,
	                          0x00000000,
	                          CONN_ESTABLISH_CLIENT_DATA_OK) != OK)
		return FAIL;
	PRINT("Clear client data for non-exinstent client.");
	if(send_clear_client_data(server_mbox, server_ref, server_ref + 1,
	                          0x00000000,
	                          CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST) != OK)
		return FAIL;

	/*Disconnecting first */
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	TEST_INFO("Test passed.");
	return OK;
}


/****************************************************************
 *  Test 6 : Check that client_data is accompanying message when
 *           receiving ordinary message in server.
 ****************************************************************/
int test_6( void )
{
	union itc_msg *msg = NULL;
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t server_ref;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;

	test_number = 6;
	TEST_INFO("Starting : Check that client_data is accompanying message "
	          "when receiving ordinary message in server.");
	/*Connecting first*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	PRINT("Set client data");
	if(send_set_client_data(server_mbox, server_ref, server_ref,
	                        0x12121212, CONN_ESTABLISH_CLIENT_DATA_OK) != OK)
		return FAIL;


	PRINT("Checking that the message is accompanyed by client_data");
	msg = itc_alloc(sizeof(struct test_message_req),
	                TEST_MESSAGE_REQ);
	msg->test_message_req.procedure_ref = ++procedure_ref;
	msg->test_message_req.connection_ref = server_ref;
	msg->test_message_req.wanted_reply_msgno = TEST_MESSAGE_CFM;

	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout! Server did not reply within 1 second");
		return FAIL;
	}

	if(msg->msgno != TEST_MESSAGE_CFM) {
		PRINT("Received unexpected msgno; "
		       "expected: 0x%08x , received 0x%08x",
		       TEST_MESSAGE_CFM, msg->msgno);
		itc_free(&msg);
		return FAIL;
	}
	if(msg->test_message_cfm.client_data != 0x12121212) {
		PRINT("Received unexpected client_data; "
		       "expected: 0x%08x , received 0x%08x",
		       0x12121212, msg->test_message_cfm.client_data);
		itc_free(&msg);
		return FAIL;
	}
	itc_free(&msg);

	PRINT("Clear client data - normal");
	if(send_clear_client_data(server_mbox, server_ref, server_ref,
	                          0x12121212, CONN_ESTABLISH_CLIENT_DATA_OK) != OK)
		return FAIL;

	/*Disconnecting first */
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 7 : Handling of client_data when disconnecting.
 ****************************************************************/
int test_7( void )
{
	union itc_msg *msg = NULL;
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t server_ref;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;

	test_number = 7;
	TEST_INFO("Starting : Handling of client_data when disconnecting.");
	/*Connecting first*/
	PRINT("Connecting");
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	PRINT("Set client data - normal");
	if(send_set_client_data(server_mbox, server_ref, server_ref,
	                        0x78787878, CONN_ESTABLISH_CLIENT_DATA_OK) != OK)
		return FAIL;


	PRINT("Disconnecting");
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	PRINT("Receiving info about client_data in disconnect_fwd");
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout! Server did not reply within 1 second");
		return FAIL;
	}

	if(msg->msgno != TEST_CD_CLEAR_CFM) {
		PRINT("Received unexpected msgno; "
		       "expected: 0x%08x , received 0x%08x",
		       TEST_CD_CLEAR_CFM, msg->msgno);
		itc_free(&msg);
		return FAIL;
	}
	if(msg->test_cd_clear_cfm.data != 0x78787878) {
		PRINT("Received unexpected client_data; "
		       "expected: 0x%08x , received 0x%08x",
		       0x78787878, msg->test_message_cfm.client_data);
		itc_free(&msg);
		return FAIL;
	}
	itc_free(&msg);

	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 8 : Check that messages with an unknown connection_ref
 *           is discarded.
 ****************************************************************/
int test_8( void )
{
	union itc_msg *msg = NULL;

	test_number = 8;
	TEST_INFO("Starting : Check that messages with an unknown "
	          "connection_ref is discarded.");

	msg = itc_alloc(sizeof(struct test_message_req),
	                TEST_MESSAGE_REQ);
	msg->test_message_req.procedure_ref = ++procedure_ref;
	msg->test_message_req.connection_ref = 99999;
	msg->test_message_req.wanted_reply_msgno = TEST_MESSAGE_CFM;

	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(msg) {
		PRINT("Got an unexpected reply. msgno = 0x%08x", msg->msgno);
		itc_free(&msg);
		return FAIL;
	}

	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 9 : Requesting unsupported version.
 ****************************************************************/
int test_9( void )
{
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t server_ref;
	uint32_t requested_versions_3[] = {5};
	uint32_t selected_version;

	test_number = 9;
	TEST_INFO("Starting : Requesting unsupported version.");
	/*Connecting first*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_3) / sizeof(requested_versions_3[0]),
	              requested_versions_3,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_REJ_UNSUPPORTED_VERSION) {
		PRINT("Connection establish did not fail due to"
		       "CONN_ESTABLISH_REJ_UNSUPPORTED_VERSION (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 10 : Disconnecting clients in "random" order.
 ****************************************************************/
int test_10( void )
{
	int i;
	uint32_t res;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;
	int disconnect_order[] = {0, 9, 6, 2, 7, 5, 3, 1, 8, 4};

	test_number = 10;
	TEST_INFO("Starting : Disconnecting clients in \"random\" order.");

	/*default is an infinte number of clients.*/
	PRINT("Connecting %d times", 10);
	for(i = 0; i < 10; i++) {
		res = conn_establish(
		              /*input parameters*/
		              server_mbox,
		              ++procedure_ref,
		              i,
		              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
		              requested_versions_1,
		              &conn_messages,
		              MY_CONN_TMO,
		              /*returned values*/
		              &inf_server_ref[i],
		              &selected_version);
		if(res != CONN_ESTABLISH_SUCCESS) {
			PRINT("Connection establish failed for client "
			       "number %d (reason: %d %s )",
			       i, res, reason(res));
			return FAIL;
		}
	}
	PRINT("Disconnecting");
	for(i = 0; i < 10; i++) {
		res = conn_disconnect( server_mbox,
		                       ++procedure_ref,
		                       inf_server_ref[disconnect_order[i]],
		                       &conn_messages,
		                       MY_CONN_TMO );
		if(res != CONN_ESTABLISH_SUCCESS) {
			PRINT("Connection disconnect failed for client "
			       "number %d (reason: %d %s)",
			       disconnect_order[i], res, reason(res));
			return FAIL;
		}
	}

	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 11 : "Infinite" number of clients
 ****************************************************************/
int test_11( void )
{
	int i;
	uint32_t res;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;

	test_number = 11;
	TEST_INFO("Starting : \"Infinite\" number of clients.");

	/*default is an infinte number of clients.*/
	PRINT("Connecting %d times", INFINITE_NUMBER);
	for(i = 0; i < INFINITE_NUMBER; i++) {
		res = conn_establish(
		              /*input parameters*/
		              server_mbox,
		              ++procedure_ref,
		              i,
		              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
		              requested_versions_1,
		              &conn_messages,
		              MY_CONN_TMO,
		              /*returned values*/
		              &inf_server_ref[i],
		              &selected_version);
		if(res != CONN_ESTABLISH_SUCCESS) {
			PRINT("Connection establish failed for client "
			       "number %d (reason: %d %s)", i, res, reason(res));
			return FAIL;
		}
	}
	PRINT("Disconnecting");
	for(i = 0; i < INFINITE_NUMBER; i++) {
		res = conn_disconnect( server_mbox,
		                       ++procedure_ref,
		                       inf_server_ref[i],
		                       &conn_messages,
		                       MY_CONN_TMO );
		if(res != CONN_ESTABLISH_SUCCESS) {
			PRINT("Connection disconnect failed for client "
			       "number %d (reason: %d %s)", i, res, reason(res));
			return FAIL;
		}
	}

	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 12 : Connect/Disconnect time out
 ****************************************************************/
int test_12( void )
{
	union itc_msg *msg = NULL;
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t client_ref2 = 1235;
	uint32_t server_ref;
	uint32_t server_ref2;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;

	test_number = 12;
	TEST_INFO("Starting : Connect/Disconnect time out.");

	/*Introduce a delay in server that will cause time outs*/
	msg = itc_alloc(sizeof(struct test_set_tmo_req),
	                TEST_SET_TMO_REQ);
	msg->test_set_tmo_req.time_out = 500;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout! Server did not reply within 1 second");
		return FAIL;
	}

	if(msg->msgno != TEST_SET_TMO_RSP) {
		PRINT("Received unexpected msgno; "
		       "expected: 0x%08x , received 0x%08x",
		       TEST_SET_TMO_RSP, msg->msgno);
		itc_free(&msg);
		return FAIL;
	}
	itc_free(&msg);

	/* try to connect with a too short time out */
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              200,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_REJ_TIME_OUT) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}

	/* set up new connction to tear down later*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref2,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              0,
	              /*returned values*/
	              &server_ref2,
	              &selected_version);

	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		return FAIL;
	}
	/* try to disconnect with a too short time out */
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref2,
	                       &conn_messages,
	                       200 );
	if(res != CONN_ESTABLISH_REJ_TIME_OUT) {
		PRINT("Got unexpected result in time out test; "
		       "result:0x%08x)\n", res);
		return FAIL;
	}

	/*Remove delay in server*/
	msg = itc_alloc(sizeof(struct test_set_tmo_req),
	                TEST_SET_TMO_REQ);
	msg->test_set_tmo_req.time_out = 0;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg) {
		PRINT("Timeout! Server did not reply within 1 second");
		return FAIL;
	}

	sleep(1);
	while(msg) {   /*eat up all received messages missed due to too short tmo*/
		itc_free(&msg);
		msg = itc_receive(ITC_NOFILTER, 0, server_mbox);
	}

	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 13 : Limited number of clients
 ****************************************************************/
int test_13( void )
{
	int i;
	uint32_t res;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t selected_version;
	int disconnect_order[] = {0, 9, 6, 2, 7, 5, 3, 1, 8, 4};

	test_number = 13;
	TEST_INFO("Starting : Limited number of clients.");

	if(re_init_server(5, 0) != OK)
		return FAIL;

	/*default is an inte number of clients.*/
	for(i = 0; i < 10; i++) {
		res = conn_establish(
		              /*input parameters*/
		              server_mbox,
		              ++procedure_ref,
		              i,
		              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
		              requested_versions_1,
		              &conn_messages,
		              MY_CONN_TMO,
		              /*returned values*/
		              &inf_server_ref[i],
		              &selected_version);
		if(res != CONN_ESTABLISH_SUCCESS)
			break;
	}
	if((i != 5) || (res != CONN_ESTABLISH_REJ_MAX_CLIENTS_REACHED)) {
		PRINT("Expected error %d after connecting %d clients; "
		       "got error %d after connecting %d clients; ",
		       CONN_ESTABLISH_REJ_MAX_CLIENTS_REACHED, 5,
		       res, i);
		goto fail;
	}
	for(i = 0; i < 5; i++) {
		res = conn_disconnect( server_mbox,
		                       ++procedure_ref,
		                       inf_server_ref[i],
		                       &conn_messages,
		                       MY_CONN_TMO );
		if(res != CONN_ESTABLISH_SUCCESS) {
			PRINT("Connection disconnect failed for client "
			       "number %d (reason: %d %s )",
			       disconnect_order[i], res, reason(res));
			goto fail;
		}
	}
	TEST_INFO("Test passed.");
	if(re_init_server(0, 1) != OK)
		return FAIL;
	return OK;
fail:
	re_init_server(0, 1);
	return FAIL;
}

/****************************************************************
 *  Test 14 : Test of callbacks.
 ****************************************************************/
int test_14( void )
{
	union itc_msg *msg = NULL;
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t client_ref2 = 1235;
	uint32_t server_ref;
	uint32_t server_ref2;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t requested_versions_2[] = {2, 3};
	uint32_t selected_version;
	uint32_t selected_version2;

	test_number = 14;
	TEST_INFO("Starting : Test of callbacks.");

	if(re_init_server(5, 1) != OK)
		return FAIL;

	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason: %d %s )",
		       res, reason(res));
		goto fail;
	}

	/*Testing dropped msg callback*/
	msg = itc_alloc(4, TEST_ECHO_REQ);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_ECHO_RSP)) {
		PRINT("dropped_msg_cb should have sent back a reply, but it didn,t.");
		goto fail;
	}
	itc_free(&msg);

	/* Testing connect callback */
	msg = itc_alloc(sizeof(struct test_set_conn_reject_cause_req),
	                TEST_SET_CONN_REJECT_CAUSE_REQ);
	msg->test_set_conn_reject_cause_req.procedure_ref = ++procedure_ref;
	msg->test_set_conn_reject_cause_req.connection_ref = server_ref;
	msg->test_set_conn_reject_cause_req.reject_cause = MY_REJECT_CAUSE;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_SET_CONN_REJECT_CAUSE_CFM)) {
		PRINT("Did not get a TEST_SET_CONN_REJECT_CAUSE_CFM.");
		goto fail;
	}
	itc_free(&msg);
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref2,
	              sizeof(requested_versions_2) / sizeof(requested_versions_2[0]),
	              requested_versions_2,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref2,
	              &selected_version2);
	if(res == CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection unexpectedly succeded");
		goto fail;
	}
	if(res != MY_REJECT_CAUSE) {
		PRINT("Connection establish failed for wrong reason "
		       "(reason:%d %s, expected: %d %s)",
		       res, reason(res), MY_REJECT_CAUSE, reason(MY_REJECT_CAUSE));
		goto fail;
	}

	/* Testing disconnect callback */
	msg = itc_alloc(sizeof(struct test_set_master_mbox_req),
	                TEST_SET_MASTER_MBOX_REQ);
	msg->test_set_master_mbox_req.procedure_ref = ++procedure_ref;
	msg->test_set_master_mbox_req.connection_ref = server_ref;
	msg->test_set_master_mbox_req.mbox = itc_current_mbox();
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_SET_MASTER_MBOX_CFM)) {
		PRINT("Did not get a TEST_SET_MASTER_MBOX_CFM.");
		goto fail;
	}
	itc_free(&msg);

	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref2,
	              sizeof(requested_versions_2) / sizeof(requested_versions_2[0]),
	              requested_versions_2,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref2,
	              &selected_version2);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason:0x%08x)", res);
		goto fail;
	}
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref2,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason:0x%08x)", res);
		goto fail;
	}
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_CLIENT_DISCONNECT_IND)) {
		PRINT("Did not get a TEST_CLIENT_DISCONNECT_IND.");
		goto fail;
	}
	if( msg->test_client_died_ind.client_ref != client_ref2 ||
	    msg->test_client_died_ind.server_ref != server_ref2 ) {
		PRINT("Wrong client was disconnected, "
		       "client_ref was: 0x%08x exp: 0x%08x "
		       "server_ref was: 0x%08x exp: 0x%08x",
		       msg->test_client_died_ind.client_ref, client_ref2,
		       msg->test_client_died_ind.server_ref, server_ref2);
		goto fail;
	}
	itc_free(&msg);

	/* Testing client died callback */
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref2,
	              sizeof(requested_versions_2) / sizeof(requested_versions_2[0]),
	              requested_versions_2,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref2,
	              &selected_version2);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason:0x%08x)", res);
		goto fail;
	}
	msg = itc_alloc(sizeof(conn_any_msg_t), TEST_CONN_MONITOR_FWD);
	msg->any_msg.procedure_ref = 0;
	msg->any_msg.connection_ref = server_ref2;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_CLIENT_DIED_IND)) {
		PRINT("Did not get a TEST_CLIENT_DIED_IND.");
		goto fail;
	}
	if( msg->test_client_died_ind.client_ref != client_ref2 ||
	    msg->test_client_died_ind.server_ref != server_ref2 ) {
		PRINT("Wrong client was died, "
		       "client_ref was: 0x%08x exp: 0x%08x "
		       "server_ref was: 0x%08x exp: 0x%08x",
		       msg->test_client_died_ind.client_ref, client_ref2,
		       msg->test_client_died_ind.server_ref, server_ref2);
		goto fail;
	}
	itc_free(&msg);

	/*Stop sending _INDs from callbacks.*/
	msg = itc_alloc(sizeof(struct test_set_master_mbox_req),
	                TEST_SET_MASTER_MBOX_REQ);
	msg->test_set_master_mbox_req.procedure_ref = ++procedure_ref;
	msg->test_set_master_mbox_req.connection_ref = server_ref;
	msg->test_set_master_mbox_req.mbox = 0;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_SET_MASTER_MBOX_CFM)) {
		PRINT("Did not get a TEST_SET_MASTER_MBOX_CFM.");
		goto fail;
	}
	itc_free(&msg);

	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason:0x%08x)", res);
		goto fail;
	}
	TEST_INFO("Test passed.");
	if(re_init_server(0, 1) != OK)
		return FAIL;
	return OK;

fail:
	re_init_server(0, 1);
	if(msg) itc_free(&msg);
	return FAIL;
}

/****************************************************************
 *  Test 15 : get client_info
 ****************************************************************/
int test_15( void )
{
	union itc_msg *msg = NULL;
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t client_ref2 = 1235;
	uint32_t server_ref;
	uint32_t server_ref2;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t requested_versions_2[] = {2, 3};
	uint32_t selected_version;
	uint32_t selected_version2;

	test_number = 15;
	TEST_INFO("Starting : Get client info.");

	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions_1) / sizeof(requested_versions_1[0]),
	              requested_versions_1,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason:0x%08x)", res);
		return FAIL;
	}
	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref2,
	              sizeof(requested_versions_2) / sizeof(requested_versions_2[0]),
	              requested_versions_2,
	              &conn_messages,
	              MY_CONN_TMO,
	              /*returned values*/
	              &server_ref2,
	              &selected_version2);
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection establish failed (reason:0x%08x)", res);
		return FAIL;
	}

	/*Look up an existing client */
	msg = itc_alloc(sizeof(struct test_get_client_info_req),
	                TEST_GET_CLIENT_INFO_REQ);
	msg->test_get_client_info_req.procedure_ref = ++procedure_ref;
	msg->test_get_client_info_req.connection_ref = server_ref;
	msg->test_get_client_info_req.server_ref = server_ref2;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_GET_CLIENT_INFO_RSP)) {
		PRINT("Did not get a TEST_GET_CLIENT_INFO_RSP");
		if(msg) itc_free(&msg);
		return FAIL;
	}
	if(msg->test_get_client_info_rsp.result != CONN_ESTABLISH_CLIENT_INFO_OK) {
		PRINT("conn_get_client_info failed, reason %d",
		       msg->test_get_client_info_rsp.result);
		itc_free(&msg);
		return FAIL;
	}
	if(msg->test_get_client_info_rsp.client_ref != client_ref2) {
		PRINT("conn_get_client_info returned wrong client_ref, "
		       "was %d expected %d",
		       msg->test_get_client_info_rsp.client_ref, client_ref2);
		itc_free(&msg);
		return FAIL;
	}
	itc_free(&msg);

	/*Look up an non-existing client */
	msg = itc_alloc(sizeof(struct test_get_client_info_req),
	                TEST_GET_CLIENT_INFO_REQ);
	msg->test_get_client_info_req.procedure_ref = ++procedure_ref;
	msg->test_get_client_info_req.connection_ref = server_ref;
	msg->test_get_client_info_req.server_ref = server_ref2 + 1;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(ITC_NOFILTER, MY_CONN_TMO, server_mbox);
	if(!msg || (msg->msgno != TEST_GET_CLIENT_INFO_RSP)) {
		PRINT("Did not get a TEST_GET_CLIENT_INFO_RSP");
		if(msg) itc_free(&msg);
		return FAIL;
	}
	if(msg->test_get_client_info_rsp.result !=
	    CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST) {
		PRINT("conn_get_client_info returned wrong error code, "
		       "was %d expected %d",
		       msg->test_get_client_info_rsp.result,
		       CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST);
		itc_free(&msg);
		return FAIL;
	}
	itc_free(&msg);

	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason:0x%08x)", res);
		return FAIL;
	}
	res = conn_disconnect( server_mbox,
	                       ++procedure_ref,
	                       server_ref2,
	                       &conn_messages,
	                       MY_CONN_TMO );
	if(res != CONN_ESTABLISH_SUCCESS) {
		PRINT("Connection disconnect failed (reason:0x%08x)", res);
		return FAIL;
	}

	TEST_INFO("Test passed.");
	return OK;
}

/****************************************************************
 *  Test 16 : Faulty server configurations
 ****************************************************************/
int test_16( void )
{
	uint32_t res;
	conn_server_handle_t handle;
	uint32_t supported_versions[] = {1,2,3,4,5};

	struct conn_establish_msg_numbers invalid_conn_messages = {1,2,3,3,5,6,7};

	test_number = 16;
	TEST_INFO("Starting : Misc fault cases for conn_establish_server_init.");

	PRINT(" Testing invalid server handle");
	res = conn_establish_server_init(NULL, /* invalid handle  */
	                                 1,
	                                 supported_versions,
	                                 &conn_messages, 0, NULL);
	if(res==CONN_INIT_OK) {
		PRINT("conn_establish_server_init should have failed.");
		return FAIL;
	}

	PRINT(" Testing zero numbers of supported versions");
	res = conn_establish_server_init(&handle,
	                                 0, /*0 numbers of supported versions*/
	                                 supported_versions,
	                                 &conn_messages, 0, NULL);
	if(res==CONN_INIT_OK) {
		PRINT("conn_establish_server_init should have failed.");
		return FAIL;
	}

	PRINT(" Testing NULL as a supported versions array");
	res = conn_establish_server_init(&handle,
	                                 1,
	                                 NULL, /*no supported versions*/
	                                 &conn_messages, 0, NULL);
	if(res==CONN_INIT_OK) {
		PRINT("conn_establish_server_init should have failed.");
		return FAIL;
	}

	PRINT(" Testing NULL as message number array");
	res = conn_establish_server_init(&handle,
	                                 1,
	                                 supported_versions,
	                                 NULL, /* no message numbers specified */
	                                 0, NULL);
	if(res==CONN_INIT_OK) {
		PRINT("conn_establish_server_init should have failed.");
		return FAIL;
	}

	PRINT(" Testing message number array with dupplicates");
	res = conn_establish_server_init(&handle,
	                                 1,
	                                 supported_versions,
	                                 &invalid_conn_messages, /* includes duplicates*/
	                                 0, NULL);
	if(res==CONN_INIT_OK) {
		PRINT("conn_establish_server_init should have failed.");
		return FAIL;
	}

	TEST_INFO("Test passed.");
	return OK;
}


/****************************************************************
 *  Test 17 : Sending an invalid conn_establish req to server
 ****************************************************************/
int test_17( void )
{
	union itc_msg *msg = NULL;
	uint32_t client_ref  = 1234;
	test_number = 17;
	TEST_INFO("Starting : Sending an invalid conn_establish req to server.");

	uint32_t filter[] = {2, conn_messages.establish_cfm,
	                     conn_messages.establish_rej
	                    };

	PRINT("Testing REQ with zero requested versions");
	msg = itc_alloc(sizeof(conn_establish_req_t),
	                conn_messages.establish_req);
	msg->conn_establish_req.procedure_ref = ++procedure_ref;
	msg->conn_establish_req.connection_ref = client_ref;
	msg->conn_establish_req.protocol_count = htonl(0);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(filter, 10000, server_mbox);
	if(!msg) {
		PRINT("Server did not answer the connection request");
		return FAIL;
	}
	if(msg->msgno == conn_messages.establish_cfm) {
		PRINT("Server confirmed a broken request");
		itc_free(&msg);
		return FAIL;
	}
	itc_free(&msg);

	PRINT("Testing REQ with number of requested versions > "
	      "provided number of version numbers");
	msg = itc_alloc(sizeof(conn_establish_req_t),
	                conn_messages.establish_req);
	msg->conn_establish_req.procedure_ref = ++procedure_ref;
	msg->conn_establish_req.connection_ref = client_ref;
	msg->conn_establish_req.protocol_count = htonl(1);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(filter, 10000, server_mbox);
	if(!msg) {
		PRINT("Server did not answer the connection request");
		return FAIL;
	}
	if(msg->msgno == conn_messages.establish_cfm) {
		PRINT("Server confirmed a broken request");
		itc_free(&msg);
		return FAIL;
	}
	itc_free(&msg);

	TEST_INFO("Test passed.");
	return OK;
}


#if 0
/****************************************************************
 *  Test n : <Description here>
 ****************************************************************/
int test_n( void )
{
	union itc_msg *msg = NULL;
	uint32_t res;
	uint32_t client_ref  = 1234;
	uint32_t client_ref2 = 1235;
	uint32_t client_ref3 = 1236;
	uint32_t server_ref;
	uint32_t server_ref2;
	uint32_t server_ref3;
	uint32_t requested_versions_1[] = {TEST_SERVER_VERSIONS};
	uint32_t requested_versions_2[] = {2, 3};
	uint32_t requested_versions_3[] = {5};
	uint32_t selected_version;
	uint32_t selected_version2;
	test_number = n;
	TEST_INFO("Starting : <Description here>.");

	TEST_INFO("Test passed.");
	return OK;
}
#endif

void usage(char *prog_name, uint32_t max_tc)
{
	printf( "Usage : \n"
	        "%s <  test_number | a | r > ... \n"
	        "  a : run all tests \n"
	        "  r : run all tests in random order \n"
	        "  test_number : a testcase number (1..%d)\n",
	        prog_name, max_tc);
}

int test(uint32_t tc)
{
	itc_mbox_id_t my_mbox;
	union itc_msg* msg;
	uint32_t filter[] = {1,ITC_LOCATE_DEFAULT_NO};
	int res;

	my_mbox = itc_create_mailbox(mailbox_name, 0);
	if(my_mbox == ITC_NO_ID) {
		PRINT("Unable to create ITC mailbox!");
		exit(FAIL);
	}

	itc_locate_async(TEST_SERVER_NAME,NULL,ITC_MY_MBOX);
	msg = itc_receive(filter,10000,ITC_FROM_ALL);
	if(!msg) {
		PRINT("Cannot locate the server \"%s\"", TEST_SERVER_NAME);
		exit(FAIL);
	}
	server_mbox = itc_sender(msg);
	itc_free(&msg);

	switch(tc) {
	case 1:
		res = test_1();
		break;
	case 2:
		res = test_2();
		break;
	case 3:
		res = test_3();
		break;
	case 4:
		res = test_4();
		break;
	case 5:
		res = test_5();
		break;
	case 6:
		res = test_6();
		break;
	case 7:
		res = test_7();
		break;
	case 8:
		res = test_8();
		break;
	case 9:
		res = test_9();
		break;
	case 10:
		res = test_10();
		break;
	case 11:
		res = test_11();
		break;
	case 12:
		res = test_12();
		break;
	case 13:
		res = test_13();
		break;
	case 14:
		res = test_14();
		break;
	case 15:
		res = test_15();
		break;
	case 16:
		res = test_16();
		break;
	case 17:
		res = test_17();
		break;
	default:
		printf("Invalid test case: %d\n", tc);
		res = FAIL;
	}
	itc_delete_mailbox(my_mbox);
	return res;
}
void randomize(uint32_t *array, uint32_t array_size)
{
	int i, j;
	uint32_t tmp;
	srand( time(NULL) );
	for(i = array_size - 1; i > 0; i--) {
		j = rand() % (i + 1);
		tmp = array[i];
		array[i] = array[j];
		array[j] = tmp;
	}
}

#define MAX_TC 17
#define ALL    999
#define RANDOM 998

int main( int argc, char **argv )
{
	int i, arg;
	uint32_t res;
	uint32_t tc;
	char *endptr = NULL;
	uint32_t test_cases[MAX_TC];

	if(argc == 1) {
		usage(argv[0], MAX_TC);
		exit(FAIL);
	}

	if(itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		PRINT("Unable to inizalize ITC!");
		exit(FAIL);
	}
	snprintf(mailbox_name, sizeof(mailbox_name), "%s-%d",
	         argv[0], getpid());

	for(arg = 1; arg < argc; arg++) {
		if(strcmp(argv[arg], "a") == 0) {
			tc = ALL;
		} else if(strcmp(argv[arg], "r") == 0) {
			tc = RANDOM;
		} else {
			tc = strtoul(argv[arg], &endptr, 0);
			if( (*endptr != '\0') || (tc == 0) || (tc > MAX_TC) ) {
				printf("\"%s\" is an invalid parameter.\n",
				       argv[arg]);
				usage( argv[0], MAX_TC );
				exit(FAIL);
			}
		}

		switch(tc) {
		case RANDOM:
			for(i = 0; i < MAX_TC; i++)
				test_cases[i] = i + 1;
			randomize(test_cases, MAX_TC);
			for(i = 0; i < MAX_TC; i++) {
				res = test(test_cases[i]);
				if(res != OK)
					exit(FAIL);
			}
			break;

		case ALL:
			for(i = 1; i <= MAX_TC; i++) {
				res = test(i);
				if(res != OK)
					exit(FAIL);
			}
			break;
		default:
			res = test(tc);
			if(res != OK)
				exit(FAIL);
		}
	}
	test_number = 0;
	printf("\n"
	       "***********************\n"
	       "** All tests passed. **\n"
	       "***********************\n");
#ifdef LOG_LTTNG
	log_info("\n"
	       "***********************\n"
	       "** All tests passed. **\n"
	       "***********************\n");
#endif
#ifdef LOG_SYSLOG
	log_info("***********************");
	log_info("** All tests passed. **");
	log_info("***********************");
#endif
	return OK;
}

/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and dissemination to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <itc.h>
#include "signals.h"
#include "client.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_trace_test_client
#include "tpt_create.h" /* This creates the tracepoints, should only be done once for each tracepoint provider */
#include "tpt.h"

#define CLIENT_OBJ_ONE "one"
#define CLIENT_OBJ_TWO "two"

#define MAX_MAILBOX_NUM    1
static itc_mbox_id_t server_mbox = ITC_NO_ID;
static itc_mbox_id_t client_mbox = ITC_NO_ID;
static int is_initialized = 0;

union itc_msg {
	uint32_t msgno;
	struct tpt_test_one_req one_req;
	struct tpt_test_one_cfm one_cfm;
	struct tpt_test_two_req two_req;
};

static void init_itc(void)
{
	const char *mailbox_name = "tpt-client";
	const char *server_mailbox_name = "tpt-server";

	TPT_INIT();

	/* Initialize ITC */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	/* Create our mailbox. */
	client_mbox = itc_create_mailbox(mailbox_name, 0);
	if (client_mbox == ITC_NO_ID) {
		TPT_ERROR("itc_create_mailbox failed");
		return;
	}

	/* Locate server mailbox */
	server_mbox = itc_locate(server_mailbox_name);
	if (server_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Did not find server %s", server_mailbox_name));
		return;
	}

	is_initialized = 1;
}

void send_one_message(void)
{
	union itc_msg *msg = NULL;

	if(!is_initialized) {
		init_itc();
	}

	TPT_ERROR("!!! client");

	msg = itc_alloc(sizeof(union itc_msg), TPT_TEST_ONE_REQ);
	msg->one_req.int1 = 1;
	msg->one_req.int2 = 2;
	TPT_TRACE_OBJ(1, CLIENT_OBJ_ONE,
	              STR("TPT_TEST_ONE_REQ set int1 = %u int2 = %u",
	                  msg->one_req.int1, msg->one_req.int2));
	TPT_OBJ_SEND_SIG(CLIENT_OBJ_ONE, msg->msgno, server_mbox, "TPT_TEST_ONE_REQ");
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	switch (msg->msgno) {
	case TPT_TEST_ONE_CFM:
		TPT_OBJ_REC_SIG(CLIENT_OBJ_ONE, msg->msgno, "TPT_TEST_ONE_CFM");
		TPT_TRACE_OBJ(1, CLIENT_OBJ_ONE, STR("TPT_TEST_ONE_CFM returned %s",
		                                     msg->one_cfm.status == SERVER_OK ? "SERVER_OK" : "SERVER_ERROR"));
		break;
	default:
		TPT_ERROR(STR("Received Unexpected message: %x from 0x%x\n", msg->msgno,
		              itc_sender(msg)));
		break;
	}

	itc_free(&msg);
}

void send_two_message(void)
{
	union itc_msg *msg = NULL;

	if(!is_initialized) {
		init_itc();
	}

	msg = itc_alloc(sizeof(union itc_msg), TPT_TEST_TWO_REQ);
	memset(msg->two_req.an_array, 0xff, sizeof(msg->two_req.an_array));
	TPT_OBJ_DATA(CLIENT_OBJ_TWO, "TPT_TEST_TWO_REQ set array to 0xff",
	             (const char *)msg->two_req.an_array, sizeof(msg->two_req.an_array));
	TPT_OBJ_SEND_SIG(CLIENT_OBJ_TWO, msg->msgno, server_mbox, "TPT_TEST_TWO_REQ");
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
}

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
#include "server.h"
#include "server_log.h"

#define MAX_MAILBOX_NUM    1
static itc_mbox_id_t server_mbox = ITC_NO_ID;

union itc_msg {
	uint32_t msgno;
	struct tpt_test_one_req one_req;
	struct tpt_test_one_cfm one_cfm;
	struct tpt_test_two_req two_req;
};

static void read_messages(void)
{
	union itc_msg *msg;
	union itc_msg *cfm_msg;

	for (;;) {
		TPT_TRACE(1, "Waiting for message");
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {
		case TPT_TEST_ONE_REQ:
			TPT_OBJ_REC_SIG(SERVER_OBJ_ONE, msg->msgno, "TPT_TEST_ONE_REQ");
			server_handle_one((struct tpt_test_one_req *)msg);

			/* Send back reply */
			cfm_msg = itc_alloc(sizeof(union itc_msg), TPT_TEST_ONE_CFM);
			cfm_msg->one_cfm.status = SERVER_OK;
			TPT_TRACE_OBJ(1, SERVER_OBJ_ONE,
			              STR("TPT_TEST_ONE_CFM set status to %s",
			                  cfm_msg->one_cfm.status ==
			                  SERVER_OK ? "SERVER_OK" : "SERVER_ERROR"));
			TPT_OBJ_SEND_SIG(SERVER_OBJ_ONE, cfm_msg->msgno, server_mbox,
			                 "TPT_TEST_ONE_CFM");
			itc_send(&cfm_msg, itc_sender(msg), ITC_MY_MBOX);

			break;
		case TPT_TEST_TWO_REQ:
			TPT_OBJ_REC_SIG(SERVER_OBJ_TWO, msg->msgno, "TPT_TEST_TWO_REQ");
			server_handle_two((struct tpt_test_two_req *)msg);
			break;
		default:
			TPT_ERROR(STR("Received Unexpected message: %x from 0x%x\n", msg->msgno,
			              itc_sender(msg)));
			exit(0);
			break;
		}
		itc_free(&msg);
	}
}


int main(int argc, char **argv)
{
	const char *mailboxname = "tpt-server";

	(void)argc;
	(void)argv;

	TPT_INIT();

	/* Initialize ITC */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	/* Create our mailbox. */
	server_mbox = itc_create_mailbox(mailboxname, 0);
	if (server_mbox == ITC_NO_ID)
		return -1;

	TPT_INFO("Server started");

	read_messages();

	return 0;
}

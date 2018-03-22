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
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "itc.h"
#include "pinmux.h"
#include "conn-establish-helper.h"
#include "rhd-pinmux-if.h"

#define USAGE() printf("Usage:\n"                                       \
		       "pinmux [-s] [-a] [-v] [-h]\n"                   \
		       "-s  show the reserved pins information\n"       \
		       "-a  show all the pins information\n"            \
		       "-v  show extra information (handle)\n"          \
		       "-h  print usage\n");

#define CONN_ESTABLISH_TMO 1000

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_PINMUX_STRUCTS
};

static struct conn_establish_msg_numbers  conn_messages = {
	PINMUX_CONN_ESTABLISH_REQ,
	PINMUX_CONN_ESTABLISH_CFM,
	PINMUX_CONN_ESTABLISH_REJ,
	PINMUX_CONN_DISCONNECT_REQ,
	PINMUX_CONN_DISCONNECT_CFM,
	PINMUX_CONN_DISCONNECT_REJ,
	PINMUX_CONN_MONITOR_FWD
};

static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} pinmux_conn;

static uint32_t client_ref = 0x1;

static int32_t get_mbox(itc_mbox_id_t *pinmux_mbox)
{
	/* client mailbox */
	if (itc_current_mbox() == ITC_NO_ID) {
		printf("Mailbox doesn't exist.\n");
		return 1;
	}

	/* server mailbox */
	*pinmux_mbox = itc_locate(RHD_PINMUX_MAILBOX);
	if (*pinmux_mbox == ITC_NO_ID) {
		printf("pinmux server is not exist or mailbox %s "
		       "doesn't exist\n",
		       RHD_PINMUX_MAILBOX);
		return 1;
	}
	return 0;
}

static int pinmux_conn_establish(void)
{
	uint32_t requested_versions[] = {PINMUX_SERVER_VERSIONS};
	uint32_t procedure_ref = 0;

	int ret;
	/* Find client and server mailboxes */
	if (get_mbox(&pinmux_conn.server_mbox) != 0) {
		printf("Client:Cannot find mailbox\n");
		return -1;
	}

	/*Connect to the server*/
	ret = conn_establish(
	            /*input parameters*/
	            pinmux_conn.server_mbox,
	            ++procedure_ref,
	            client_ref,
	            sizeof(requested_versions) / sizeof(requested_versions[0]),
	            requested_versions,
	            &conn_messages,
	            CONN_ESTABLISH_TMO,
	            /*returned values*/
	            &pinmux_conn.server_ref,
	            &pinmux_conn.selected_version);
	if (ret != CONN_ESTABLISH_SUCCESS) {
		printf("Connection establish failed with reason:0x%x\n",
		       ret);
		return -1;
	}

	return 0;
}

static int msg_check(union itc_msg *msg)
{
	if((msg->any_msg.connection_ref != client_ref)) {
		printf("Client:Server replied with invalid ...._ref; \n"
		       "connection_ref: expected 0x%08x, received 0x%08x\n",
		       client_ref, msg->any_msg.connection_ref);
		itc_free(&msg);
		return -1;
	}
	return 0;
}

static int pinmux_list(uint32_t type, uint32_t verbose_mode)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {4, RHD_PINMUX_LIST_CFM, RHD_PINMUX_LIST_REJ,
	                       RHD_PINMUX_LIST_IND, RHD_PINMUX_LIST_END_IND};
	int ret = -1;
	if(pinmux_conn_establish()) {
		return -1;
	}

	sendmsg = itc_alloc(sizeof(struct pinmux_list_req),
	                    RHD_PINMUX_LIST_REQ);

	sendmsg->list_req.connection_ref = pinmux_conn.server_ref;
	sendmsg->list_req.type = type;
	sendmsg->list_req.verbose_mode = verbose_mode;
	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	while(1) {
		receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
		                         pinmux_conn.server_mbox);

		if(msg_check(receivemsg)) {
			return -1;
		}
		switch(receivemsg->msgno) {
		case RHD_PINMUX_LIST_CFM:
			break;
		case RHD_PINMUX_LIST_IND:
			printf(receivemsg->list_ind.str);
			break;
		case RHD_PINMUX_LIST_END_IND:
			ret = 0;
			break;
		case RHD_PINMUX_LIST_REJ:
			printf("List failed\n");
			break;
		default:
			printf("receive unexpected msg 0x%x\n",
			       receivemsg->msgno);
			break;
		}
		itc_free(&receivemsg);
		if(!ret) {
			break;
		}
	}
	return ret;
}

int main(int argc, char **argv)
{
	itc_mbox_id_t mbox_id = ITC_NO_ID;
	pinmux_list_type_t type = PINMUX_LIST_TYPE_INVALID;
	int verbose_mode = 0;
	int ret = -1;
	int i;

	/* Initialize ITC and create mailbox  */
	int itcinit_ret = itc_init(MAX_MAILBOX_NUM, ITC_MALLOC,
	                           NULL, ITC_NO_NAMESPACE, 0);
	if(itcinit_ret) {
		printf("itc_init failed (%d)", itcinit_ret);
		goto main_end;
	}

	mbox_id = itc_create_mailbox("pinmux-cmd", 0);
	if(mbox_id == ITC_NO_ID) {
		printf("Fail to create pinmux cmd mailbox.\n");
		goto main_end;
	}

	if(argc > 3) {
		printf("Arguments are invalid.\n");
		USAGE();
		goto main_end;

	}
	if(argc == 1) {
		USAGE();
		ret = 0;
		goto main_end;

	}

	for (i = 1; i < argc; i++) {
		if(!strcmp(argv[i], "-s")) {
			if(type == PINMUX_LIST_TYPE_ALL) {
				continue;
			}
			type = PINMUX_LIST_TYPE_SHORT;
		} else if(!strcmp(argv[i], "-a")) {
			type = PINMUX_LIST_TYPE_ALL;
		} else if(!strcmp(argv[i], "-v")) {
			verbose_mode = 1;
		} else if(!strcmp(argv[i], "-h")) {
			USAGE();
			ret = 0;
			goto main_end;
		} else {
			printf("Arguments are invalid.\n");
			USAGE();
			goto main_end;
		}
	}
	if(type != PINMUX_LIST_TYPE_ALL &&
	   type != PINMUX_LIST_TYPE_SHORT) {
		printf("Arguments are invalid.\n");
		USAGE();
		goto main_end;
	}
	ret = pinmux_list(type, verbose_mode);
main_end:
	if(mbox_id != ITC_NO_ID) {
		itc_delete_mailbox(mbox_id);
	}
	return ret;
}

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
#include <arpa/inet.h>

#include "example-msg.h"

#define MAILBOX_SIZE 1
#define EXAMPLE_TIME_OUT 1000 /*or whatever is resonable for this server*/

#define OK  0
#define FAIL 1

union itc_msg {
	uint32_t msgno;
	EXAMPLE_MESSAGES;
};


int main( int argc, char **argv )
{
	int i;
	union itc_msg *msg = NULL;

	char mailbox_name[50];
	itc_mbox_id_t my_mbox_id = ITC_NO_ID;
	itc_mbox_id_t server_mbox_id = ITC_NO_ID;
	uint32_t procedure_ref = 0;
	uint32_t client_ref = 1234;
	uint32_t requested_versions[] = {2, 3}; /* prefer version 2 over 3 */
	uint32_t requested_version_count =
	        (sizeof(requested_versions) / sizeof(requested_versions[0]));
	uint32_t server_ref;
	uint32_t selected_version;
	uint32_t conn_establish_filter[] = {2, EXAMPLE_CONN_ESTABLISH_CFM,
	                                    EXAMPLE_CONN_ESTABLISH_REJ
	                                   };
	uint32_t conn_disconnect_filter[] = {2, EXAMPLE_CONN_DISCONNECT_CFM,
	                                     EXAMPLE_CONN_DISCONNECT_REJ
	                                    };

	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Client:Unable to inizalize ITC!\n");
		goto fail;
	}
	snprintf(mailbox_name, sizeof(mailbox_name), "%s-%d", argv[0], getpid());
	my_mbox_id = itc_create_mailbox(mailbox_name, 0);
	if (my_mbox_id == ITC_NO_ID) {
		printf("Client:Unable to create ITC mailbox!\n");
		goto fail;
	}

	server_mbox_id = itc_locate(EXAMPLE_SERVER_NAME);
	if(server_mbox_id == ITC_NO_ID) {
		printf("Client:Cannot locate the server \"%s\"\n", EXAMPLE_SERVER_NAME);
		goto fail;
	}

	msg = itc_alloc(sizeof(conn_establish_req_t) +
	                sizeof(requested_versions) -
	                sizeof(requested_versions[0]),/* struct already have one element*/
	                EXAMPLE_CONN_ESTABLISH_REQ);
	msg->example_conn_establish_req.procedure_ref = ++procedure_ref;
	msg->example_conn_establish_req.connection_ref = client_ref;
	/*Note that we use network byte order in the connection establish
	  mechanism. You need to use htonl/ntohl when setting/reading
	  struct members.*/
	msg->example_conn_establish_req.protocol_count = htonl(
	                        requested_version_count);
	for(i = 0; i < requested_version_count; i++) {
		msg->example_conn_establish_req.protocol_versions[i] =
		        htonl(requested_versions[i]);
	}
	itc_send(&msg, server_mbox_id, ITC_MY_MBOX);
	msg = itc_receive(conn_establish_filter,
	                  EXAMPLE_TIME_OUT,
	                  server_mbox_id);
	if(!msg) {
		printf("Timeout waiting for server.\n");
		goto fail;
	}
	if(msg->any_msg.procedure_ref != procedure_ref) {
		printf("Server replied using wrong procedure_ref "
		       "(expected 0x%08x, received 0x%08x).",
		       procedure_ref, msg->any_msg.procedure_ref);
		goto fail;
	}
	if(msg->msgno == EXAMPLE_CONN_ESTABLISH_CFM) {
		selected_version =
		        ntohl(msg->example_conn_establish_cfm.selected_protocol_version);
		server_ref = msg->example_conn_establish_cfm.connection_ref;
		printf("Connected. Selected version is %d\n", selected_version);
	} else {
		printf("Connection request was rejected, reason(%d)\n",
		       ntohl(msg->example_conn_establish_rej.reason));
		printf("Requested versions: ");
		for(i = 0; i < requested_version_count; i++)
			printf("0x%08x, ", requested_versions[i]);
		printf("\nSupported versions: ");
		for(i = 0;
		    i < ntohl(msg->example_conn_establish_rej.supported_protocol_count);
		    i++)
			printf("0x%08x, ",
			       ntohl(msg->example_conn_establish_rej.supported_protocol_versions[i]));

		printf("\n");
		goto fail;
	}
	itc_free(&msg);
	msg = itc_alloc(sizeof(struct example_message_req_s),
	                EXAMPLE_MESSAGE_REQ);
	msg->example_message_req.procedure_ref = ++procedure_ref;
	msg->example_message_req.connection_ref = server_ref;
	itc_send(&msg, server_mbox_id, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, EXAMPLE_TIME_OUT, server_mbox_id);
	if(!msg) {
		printf("Client:Timeout! Server did not reply within 1 second\n");
		goto fail;
	}

	/* Here you might want to check the received procedure_ref and
	   connection_ref if you are intrested....*/
	if( (msg->any_msg.procedure_ref != procedure_ref) ||
	    (msg->any_msg.connection_ref != client_ref) ) {
		printf("Client:Server replied with invalid ...._ref; \n"
		       "procedure_ref : expected 0x%08x, received 0x%08x\n"
		       "connection_ref: expected 0x%08x, received 0x%08x\n",
		       procedure_ref, msg->any_msg.procedure_ref,
		       client_ref, msg->any_msg.connection_ref);

		goto fail;
	}

	/* Process your reply...*/
	if(msg->msgno == EXAMPLE_MESSAGE_CFM) {
		printf("Client:Got the expected reply\n");
	} else {
		printf("Client:Received unexpected msgno; "
		       "expexted: 0x%08x , received 0x%08x\n",
		       EXAMPLE_MESSAGE_CFM, msg->msgno);
		goto fail;
	}
	itc_free(&msg);

	/*sending another message...*/
	msg = itc_alloc(sizeof(struct example_message_req_s),
	                EXAMPLE_MESSAGE_REQ);
	msg->example_message_req.procedure_ref = ++procedure_ref;
	msg->example_message_req.connection_ref = server_ref;
	itc_send(&msg, server_mbox_id, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, EXAMPLE_TIME_OUT, server_mbox_id);
	if(!msg) {
		printf("Client:Timeout! Server did not reply within 1 second\n");
		goto fail;
	}

	/* Process your reply...*/
	if(msg->msgno == EXAMPLE_MESSAGE_CFM) {
		printf("Client:Got the expected reply\n");
	} else {
		printf("Client:Received unexpected msgno; "
		       "expexted: 0x%08x , received 0x%08x\n",
		       EXAMPLE_MESSAGE_CFM, msg->msgno);
		goto fail;
	}
	itc_free(&msg);

	/* When you're done talking the server then disconnect. */
	msg = itc_alloc(sizeof(conn_disconnect_req_t),
	                EXAMPLE_CONN_DISCONNECT_REQ);

	msg->example_conn_disconnect_req.procedure_ref = procedure_ref;
	msg->example_conn_disconnect_req.connection_ref = server_ref;
	itc_send(&msg, server_mbox_id, ITC_MY_MBOX);

	msg = itc_receive(conn_disconnect_filter, EXAMPLE_TIME_OUT, server_mbox_id);
	if(!msg) {
		printf("Client:Timeout! Server did not reply within 1 second\n");
		goto fail;
	}
	if(msg->msgno == EXAMPLE_CONN_DISCONNECT_CFM) {
		printf("Disconnected.\n");
	} else {
		printf("Disconnection request was rejected, reason(%d)\n",
		       ntohl(msg->example_conn_disconnect_rej.reason));
		goto fail;
	}

	printf("Client:Done.\n");

	itc_free(&msg);
	itc_delete_mailbox(my_mbox_id);
	return OK;

fail:
	if(msg)
		itc_free(&msg);

	if(my_mbox_id != ITC_NO_ID)
		itc_delete_mailbox(my_mbox_id);

	return FAIL;
}

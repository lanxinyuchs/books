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

#include "example-msg.h"
#include "example-func.h"

#define MAILBOX_SIZE 1
#define EXAMPLE_TIME_OUT 1000

#define OK  0
#define FAIL 1

union itc_msg {
	uint32_t msgno;
	EXAMPLE_MESSAGES;
};

/* If connect and disconnect should be implicit (part of other functions and
   hidden in the exposed interface) or explicit (have a separate function in
   the exposed interface) is not specified, it's up to the one systemizing
   the function interface to decide.
   In this example it's explicit. */

int example_interact_with_server(itc_mbox_id_t server_mbox_id,
                                 uint32_t server_ref,
                                 uint32_t client_ref,
                                 uint32_t procedure_ref,
                                 uint32_t argument)
{
	union itc_msg *msg = NULL;

	msg = itc_alloc(sizeof(struct example_message_req_s),
	                EXAMPLE_MESSAGE_REQ);
	msg->example_message_req.procedure_ref = procedure_ref;
	msg->example_message_req.connection_ref = server_ref;
	msg->example_message_req.your_payload = argument;
	itc_send(&msg, server_mbox_id, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, EXAMPLE_TIME_OUT, server_mbox_id);
	if(!msg) {
		printf("Client:Timeout! Server did not reply within 1 second\n");
		return FAIL;
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
		itc_free(&msg);
		return FAIL;
	}

	/* Process your reply...*/
	if(msg->msgno == EXAMPLE_MESSAGE_CFM) {
		printf("Client:Got the expected reply\n");
		itc_free(&msg);
		return OK;
	} else {
		printf("Client:Received unexpected msgno; "
		       "expexted: 0x%08x , received 0x%08x\n",
		       EXAMPLE_MESSAGE_CFM, msg->msgno);
		itc_free(&msg);
		return FAIL;
	}
}



int main( int argc, char **argv )
{
	int i;
	char mailbox_name[50];
	itc_mbox_id_t my_mbox_id = ITC_NO_ID;
	itc_mbox_id_t server_mbox_id = ITC_NO_ID;
	uint32_t procedure_ref = 0;
	uint32_t client_ref = 1234;
	uint32_t requested_versions[] = {2, 3};
	EXAMPLE_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	uint32_t server_ref;
	uint32_t selected_version;
	uint32_t res;


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
	/*You might want to use the conn_establish provided by libconn_establish_helper
	  directly or you might want to wrap it into your own function or you
	  might even want to  make it part of the function interacting with
	  the server.
	  Here we us it as is .*/
	res = conn_establish(
	              /*input parameters*/
	              server_mbox_id,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              EXAMPLE_TIME_OUT,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("Client:Connection establish failed (reason:0x%08x)\n", res);
		goto fail;
	}

	/* Now whe have an established connection, and can start sending and
	   receiving mesages.*/
	for(i = 0; i < 10; i++) {
		res = example_interact_with_server(
		              server_mbox_id,
		              server_ref,
		              client_ref, /*just to be able to verify reply from server*/
		              ++procedure_ref,
		              12345 /*message payload*/);
		if(res != OK) {
			printf("Server interaction failed.\n");
			break;
		}
	}

	/* When you're done talking the server then disconnect.
	   (If you don't then you will occupy a "client slot" for as long your
	   mailbox exists. That might be a problem if the server just allows
	   a limited number of clients. Also, it's nice of you to say goodbye
	   when you leave :-) )*/
	res = conn_disconnect( server_mbox_id,
	                       ++procedure_ref,
	                       server_ref,
	                       &conn_messages,
	                       EXAMPLE_TIME_OUT );
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("Client:Connection disconnect failed (reason:0x%08x)\n", res);
		goto fail;
	}
	printf("Client:Done.\n");

	itc_delete_mailbox(my_mbox_id);
	return OK;

fail:
	if(my_mbox_id != ITC_NO_ID)
		itc_delete_mailbox(my_mbox_id);

	return FAIL;
}

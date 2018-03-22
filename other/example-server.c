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
#include <getopt.h>
#include <itc.h>

#include "example-msg.h"
#include "example-func.h"

#define MAILBOX_SIZE 1

union itc_msg {
	uint32_t msgno;
	EXAMPLE_MESSAGES;
};

struct client_data {
	uint32_t your_client_specific_data;
};

static void destroy_cb(struct conn_client_info *ci)
{
	printf("Server: Destroy callback \n"
	       "Client with mailbox 0x%08x, server_ref 0x%08x and "
	       "client_ref 0x%08x has %s \n",
	       ci->connected_mailbox, ci->server_ref,
	       ci->client_ref,
	       ci->state == CONN_ESTABLISH_STATUS_DISCONNECTING ?
	       "actively disconnected." : "died (or forgot to disconnect.)");

	/*Handle cleanup after client has disconnected or died
	  .
	  ..
	  ...
	  ....
	  And finally free the clients pecific data if you have any.*/
	if(ci->client_data) {
		free(ci->client_data);
	}
	return;
}


static void main_loop(void *handle)
{
	union itc_msg *msg;
	union itc_msg *reply = NULL;
	struct conn_client_info client_information;
	struct client_data *client_data; /* "helper pointer" to avoid type casting
                                          * "client_information.client_data" all
                                          * the time.*/
	int res;
	/**
	 * Create our mailbox.
	 */
	itc_mbox_id_t my_mbox = itc_create_mailbox(EXAMPLE_SERVER_NAME, 0);
	if (my_mbox == ITC_NO_ID) {
		printf("Server:Unable to create ITC mailbox \"%s\"!\n", EXAMPLE_SERVER_NAME);
	}
	printf("Server:Entering main loop\n");
	for (;;) {
		msg = itc_receive(ITC_NOFILTER,
		                  ITC_NO_TMO,
		                  ITC_FROM_ALL);

		/*Handle CONN_ESTABLISH... messages (and messages from
		 unknown clients.*/
		if(!conn_check_client(handle, &msg, &client_information))
			continue;

		client_data = client_information.client_data;
		/*Here we will always have a valid message.*/
		switch(msg->msgno) {
		case EXAMPLE_MESSAGE_REQ:
			printf("Server:Received EXAMPLE_MESSAGE_REQ, "
			       "protocol version is %d\n",
			       client_information.protocol_version);

			/*Allocate client specific data if you want to use that.*/
			if(!client_data) {
				client_data = malloc(sizeof(struct client_data));
				if(client_data == NULL) {
					printf( "%s:%d - Memory allocation failed",
					        __FILE__, __LINE__);
					exit(1);
				}
				client_data->your_client_specific_data = 0xBEEF;
				res = conn_set_client_data(handle,
				                           client_information.server_ref,
				                           client_data);
				if(res)
					printf("Server:Failed to set client data, (%d)", res);
			} else {
				printf("Server:Your client data was set to \"%x\" when "
				       "previous message was received.\n",
				       client_data->your_client_specific_data);
			}
			/*Process your incomming message and optionally provide
			  a reply.*/
			reply = itc_alloc(sizeof(struct example_message_cfm_s),
			                  EXAMPLE_MESSAGE_CFM);
			break;

		default: {
			char name[64];
			itc_get_name(client_information.sender, name, sizeof(name));
			printf("Server:Received an unknown message "
			       "(msgno:0x%08x) from client (name:\"%s\", "
			       "mbox_id:0x%08x, "
			       "client_ref:0x%08x, server_ref:0x%08x)\n",
			       msg->msgno, name, client_information.sender,
			       client_information.client_ref,
			       client_information.server_ref);
			break;
		}
		}

		/*Fill out the common fields and return the reply message.*/
		if(reply) {
			reply->any_msg.procedure_ref =
			        client_information.procedure_ref;
			reply->any_msg.connection_ref =
			        client_information.client_ref;
			itc_send(&reply, client_information.sender, ITC_MY_MBOX);
		}
		itc_free(&msg);
	}
}

static void *server_init(void)
{
	conn_server_handle_t handle;
	EXAMPLE_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	uint32_t supported_versions[] = {EXAMPLE_SERVER_VERSIONS};
	int res;
	struct conn_event_callbacks cb;
	/* No special action when clients are connecting. */
	cb.client_connecting_cb    = NULL;
	/* Using the same callback for both disconnecting and dying clients. */
	cb.client_disconnecting_cb = destroy_cb;
	cb.client_died_cb          = destroy_cb;
	/* No special handling of dropped messages. */
	cb.dropped_msg_cb          = NULL;

	res = conn_establish_server_init(&handle,
	                                 sizeof(supported_versions) /
	                                 sizeof(supported_versions[0]),
	                                 supported_versions,
	                                 &conn_messages, 0 , &cb);
	if(res != CONN_INIT_OK) {
		printf("Server: conn_establish_server_init returned %d.\n", res);
		return NULL;
	}
	return handle;
}

int main( int argc, char **argv )
{
	conn_server_handle_t handle;
	printf("Server:Starting example server\n");

	if(itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Server:Unable to inizalize ITC!\n");
		return -1;
	}

	handle = server_init();
	if (handle != NULL) {
		main_loop(handle);
	} else {
		printf("Server:Failed to initialize example server.\n");
		return -1;
	}

	return 0;
}

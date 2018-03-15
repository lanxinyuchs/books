/*
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
#include <unistd.h>
#include <libgen.h>
#include <itc.h>

#include "evti.h"
#include "event_server_internal.h"


#define MBOX_STR_LEN 30
#define RECEIVE_TMO  1000


union itc_msg {
	uint32_t  msgno;
	evti_subsc_cmd_req_s    subsc_cmd_req;
	evti_subsc_cmd_cfm_s    subsc_cmd_cfm;
	evti_subsc_cmd_rej_s    subsc_cmd_rej;
	evti_subsc_cmd_ind_s    subsc_cmd_ind;
};


/******************************************************************************
 *
 * Global function:
 *      main
 *
 * Parameters:
 *      argc,  Number of arguments submitted.
 *      argv,  Array with pointers to each argument string.
 *
 * Return value:
 *      Returns 0 when succeded.
 *              -1 when failed.
 *
 * Description:
 *      Implements the "subsc" shell command.
 *      Prints all tags and the names of the subscribing processes.
 *
 *****************************************************************************/

int main(int argc, char *argv[])
{

	char my_mbox_name[MBOX_STR_LEN];
	itc_mbox_id_t my_mbox_id = ITC_NO_ID;
	itc_mbox_id_t server_mbox_id;
	union itc_msg *msg = NULL;

	char subscriber_name[60];
	uint32_t rec_filter[] = {2, EVTI_SUBSC_CMD_CFM, EVTI_SUBSC_CMD_REJ};
	uint32_t ind_filter[] = {2, EVTI_SUBSC_CMD_IND};
	int32_t status = -1;


	if (argc != 1) {
		printf("ERROR: \"%s\" takes no arguments.\n", argv[0]);
		goto clean_up;
	}

	if (itc_init(/*MAILBOX_SIZE*/ 1, ITC_MALLOC,
	                              NULL, ITC_NO_NAMESPACE, 0)) {
		printf("ERROR: Failed to initiate ITC.\n");
		goto clean_up;
	}
	snprintf(my_mbox_name, sizeof(my_mbox_name), "%s-%d",
	         basename(argv[0]), getpid());
	my_mbox_id = itc_create_mailbox(my_mbox_name, 0);
	if (my_mbox_id == ITC_NO_ID) {
		printf("ERROR: Failed to create ITC mailbox.\n");
		goto clean_up;
	}

	server_mbox_id = itc_locate(EVTI_SERVER_NAME);
	if(server_mbox_id == ITC_NO_ID) {
		printf("ERROR: Failed to locate the server \"%s\".\n",
		       EVTI_SERVER_NAME);
		goto clean_up;
	}

	msg = itc_alloc(sizeof(evti_subsc_cmd_req_s),
	                EVTI_SUBSC_CMD_REQ);
	itc_send(&msg, server_mbox_id, ITC_MY_MBOX);

	msg = itc_receive(rec_filter, RECEIVE_TMO, server_mbox_id);
	if(!msg) {
		printf("ERROR: Time out waiting for reply from server.\n");
		goto clean_up;
	}

	if(msg->msgno == EVTI_SUBSC_CMD_REJ) {
		printf("No subscribers registered yet.\n");
		status = 0;
		goto clean_up;
	}
	itc_free(&msg);

	/* Print header. */
	printf("\nRegistered subscriptions in XCS Event Server\n\n");
	printf("Tag                              : Process name              \n");
	printf("=============================================================\n");

	while(1) {
		msg = itc_receive(ind_filter, RECEIVE_TMO, server_mbox_id);
		if(!msg) {
			printf("ERROR: Time out waiting for data from server.\n");
			goto clean_up;
		}

		if(msg->subsc_cmd_ind.num_subscribers == 0) {
			itc_free(&msg);
			break;
		}

		for(int i = 0; i < msg->subsc_cmd_ind.num_subscribers; i++) {

			if(!itc_get_name(msg->subsc_cmd_ind.subscribers[i],
			                 subscriber_name,
			                 sizeof(subscriber_name))) {
				sprintf(subscriber_name, "<UNKNOWN>");
			}

			printf("%-32s : %s\n",
			       msg->subsc_cmd_ind.tag,
			       subscriber_name);
		}
		itc_free(&msg);
	}
	printf("\n");
	status = 0;
clean_up:
	if (msg)
		itc_free(&msg);
	if (my_mbox_id != ITC_NO_ID)
		itc_delete_mailbox(my_mbox_id);
	return status;
}

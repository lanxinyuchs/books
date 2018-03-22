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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <libgen.h>
#include <itc.h>
#include <getopt.h>
#include <libgen.h>
#include "evti.h"

#define MAILBOX_SIZE 1

union itc_msg {
	uint32_t msgno;
	EVTI_SubscribeIndS subscribe_ind;
	EVTI_DelivIndS deliv_ind;
	EVTI_DelivReqS deliv_req;
	EVTI_DelivCfmS deliv_cfm;
	EVTI_DelivRejS deliv_rej;
};

static void print_usage(char *arg0)
{
	printf( "Usage : %s [-h] [-d nnnn] [-r] <tag> \n"
	        " -h   --help    Display usage information.\n"
	        " -d   --delay   How long to wait (milliseconds) before answering a REQ\n"
	        " -r   --reject  Answer a REQ with a REJ (default is to answer with a CFM)\n"
	        " <tag>          The <tag> to subscribe to.\n",
	        basename(arg0));
}

int main(int argc, char **argv)
{
	itc_mbox_id_t my_mbox;
	itc_mbox_id_t server_mbox;
	char mailbox_name[50];
	union itc_msg *msg;
	union itc_msg *reply;
	int delay = 0;
	bool send_rej = false;

	int next_option = 0;
	const char *const short_options = "hrd:";
	const struct option long_options[] = {
		{ "help", 0, NULL, 'h' },
		{ "reject", 0, NULL, 'r' },
		{ "delay", required_argument, NULL, 'd' },
		{ NULL, 0, NULL, 0 },
	};

	do {
		next_option = getopt_long (argc, argv, short_options,
		                           long_options, NULL);
		switch (next_option) {
		case 'h':
			print_usage(argv[0]);
			return 0;
		case 'r':
			send_rej = true;
			break;
		case 'd':
			delay = atoi(optarg);
			break;
		case -1:
		default:
			break;

		}
	} while (next_option != -1);

	if( argc == optind ||
	    argc > optind + 1 ) {
		printf("Invalid number of parameters\n");
		print_usage(argv[0]);
		return -1;
	}

	printf("Subscribing to tag: \"%s\", delay before answer: \"%d\"\n",
	       argv[optind], delay);

	/*Set up ITC*/
	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Unable to inizalize ITC!\n");
		return -1;
	}
	snprintf(mailbox_name, sizeof(mailbox_name), "%s-%d",
	         basename(argv[0]), getpid());
	my_mbox = itc_create_mailbox(mailbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		printf("Unable to create ITC mailbox!\n");
		return -1;
	}

	/*Find the board rhd*/
	server_mbox = itc_locate(EVTI_SERVER_NAME);
	if(server_mbox == ITC_NO_ID) {
		printf("Cannot locate the server \"%s\"\n",
		       EVTI_SERVER_NAME);
		return -1;
	}


	msg = itc_alloc(sizeof(EVTI_SubscribeIndS),
	                EVTI_SUBSCRIBE_IND);
	snprintf(msg->subscribe_ind.tag, EVTI_MAX_TAG_LENGTH,
	         "%s", argv[optind]);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	while(1) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch(msg->msgno) {
		case EVTI_DELIV_IND:
			printf("EVTI_DELIV_IND: Tag:\"%s\", Data:\"%s\".\n",
			       msg->deliv_ind.tag, msg->deliv_ind.data);
			break;

		case EVTI_DELIV_REQ:
			printf("EVTI_DELIV_REQ: Tag:\"%s\", Data:\"%s\".\n",
			       msg->deliv_ind.tag, msg->deliv_ind.data);
			printf("Answering with a \"%s\" .....",
			       send_rej ? "EVTI_DELIV_REJ" : "EVTI_DELIV_CFM");

			reply = itc_alloc(sizeof(EVTI_DelivCfmS),
			                  send_rej ? EVTI_DELIV_REJ : EVTI_DELIV_CFM);
			usleep(delay * 1000);
			itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
			printf("Now.\n");
			break;

		default:
			printf("Got an unknown message (0x%08x).\n",
			       msg->msgno);
		}
		itc_free(&msg);
	}
	return 0;
}

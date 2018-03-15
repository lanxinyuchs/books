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
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>
#include <libgen.h>
#include <itc.h>
#include <getopt.h>
#include <libgen.h>
#include "evti.h"

#define MAILBOX_SIZE 1

union itc_msg {
	uint32_t msgno;
	EVTI_DistributeIndS ind;
	EVTI_DistributeReqS req;
	EVTI_DistributeCfmS cfm;
	EVTI_DistributeRejS rej;
};

static void print_usage(char *arg0)
{
	printf( "Usage : %s [-h] <-i|-r> <tag> [data] \n"
	        " -h   --help    Display usage information.\n"
	        " -i   --ind     Distribute <tag> with an \"ind\" message.\n"
	        " -r   --req     Distribute <tag> with an \"req\" message \n"
	        "                and wait for the reply.\n"
	        " <tag>          The tag to distribute.\n"
	        " [data]         Optional data to distribute along with the tag\n",
	        basename(arg0));
}

int main(int argc, char **argv)
{
	itc_mbox_id_t my_mbox;
	itc_mbox_id_t server_mbox;
	char mailbox_name[50];
	union itc_msg *msg;
	bool send_ind = false;
	bool send_req = false;
	bool has_data = false;

	int next_option = 0;
	const char *const short_options = "hir";
	const struct option long_options[] = {
		{ "help", 0, NULL, 'h' },
		{ "ind", 0, NULL, 'i' },
		{ "req", 0, NULL, 'r' },
		{ NULL, 0, NULL, 0 },
	};

	do {
		next_option = getopt_long (argc, argv, short_options,
		                           long_options, NULL);
		switch (next_option) {
		case 'h':
			print_usage(argv[0]);
			return 0;
		case 'i':
			send_ind = true;
			break;
		case 'r':
			send_req = true;
			break;
		case -1:
		default:
			break;

		}
	} while (next_option != -1);

	if( (send_ind && send_req) ||
	    (!send_ind && !send_req) ) {
		printf(" -i or -r must be specified (but not both)\n");
		print_usage(argv[0]);
		return -1;
	}

	if( argc == optind ||
	    argc > optind + 2 ) {
		printf("Invalid number of parameters\n");
		print_usage(argv[0]);
		return -1;
	}
	has_data = (argc == optind + 2);
	printf("%s, Tag: \"%s\" , Data: \"%s\"\n",
	       send_ind ? "IND" : "REQ",
	       argv[optind],
	       has_data ? argv[optind + 1] : "<NONE>");

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


	printf("HEJ %d\n", has_data ? (uint32_t)strlen(argv[optind + 1]) : 0);
	msg = itc_alloc(sizeof(EVTI_DistributeIndS) +
	                (has_data ? (uint32_t)strlen(argv[optind + 1]) : 0),
	                (send_ind ? EVTI_DISTRIBUTE_IND : EVTI_DISTRIBUTE_REQ));
	snprintf(msg->ind.tag, EVTI_MAX_TAG_LENGTH,
	         "%s", argv[optind]);
	if(has_data) {
		snprintf(msg->ind.data, strlen(argv[optind + 1]) + 1,
		         "%s", argv[optind + 1]);
	} else {
		msg->ind.data[0] = '\0';
	}
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	if(send_ind)
		return 0;

	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	switch(msg->msgno) {
	case EVTI_DISTRIBUTE_CFM:
		printf("Got a confirm.\n");
		break;
	case EVTI_DISTRIBUTE_REJ:
		printf("Got a reject.\n");
		break;
	default:
		printf("Got an unknown message (0x%08x) as a reply.\n",
		       msg->msgno);
	}
	itc_free(&msg);
	return 0;
}

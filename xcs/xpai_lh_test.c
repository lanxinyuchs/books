/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include "itc.h"
#include "xpai_xhp_if.h"
#include "common.h"

#define MAX_MAILBOX_NUM    1
#define LH_TEST_NUMBER_OF_PORTS  2
#define LH_TEST_ECP_MAX_NO_OF_BUFFS  5
static uint32_t client_ref = 1;

void print_usage(char *program);

int main(int argc, char **argv)
{
	int ret;
	uint32_t port;
	uint32_t ch;

	if (argc != 3) {
		print_usage(argv[0]);
		return 0;
	}

	port = strtoul(argv[1], NULL, 0);
	ch   = strtoul(argv[2], NULL, 0);

	if (port >= LH_TEST_NUMBER_OF_PORTS) {
		print_usage(argv[0]);
		return 0;
	}
	if (ch >= LH_TEST_ECP_MAX_NO_OF_BUFFS) {
		print_usage(argv[0]);
		return 0;
	}

	printf("test lh starting ...\n");

	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if(itc_create_mailbox("xpai-lh-test", 0) == ITC_NO_ID) {
		printf("Failed to create mailbox\n");
		return -1;
	}
	ret = xpai_lh_port_init(port, client_ref);
	if (ret) {
		printf("Lh init port %d failed, return %d\n", port, ret);
		return -1;
	}

	ret = XPAI_StartLink(port, ch);

	if (ret)
	{
		printf("xpai_startLink test failed\n");
		return -1;
	}
 	printf("successfully\n");
	return 0;
}

void print_usage(char *program) {
	printf("Run with arguments %s <port> <channel>\n", program);
	printf("<port> is less than %d, <channel> is less than %d\n",
	       LH_TEST_NUMBER_OF_PORTS, LH_TEST_ECP_MAX_NO_OF_BUFFS);
}


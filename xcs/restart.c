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
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <itc.h>
#include <unistd.h>
#include <getopt.h>
#include "libreboot.h"

#define MAILBOX_SIZE 5

void print_usage(void)
{
	printf("Usage: restart [options] [load module container no]\n"
	       "Options:\n"
	       "    -m | --managed   Managed reboot [default]\n"
	       "    -i | --immediate Reboot immediately (unmanaged)\n"
	       "    -t | --test      Reboot and perform a\n"
	       "                     'cold restart with test'\n"
	       "    -h | --help      Show this message\n"
	       "\n"
	       "Load module container no:\n"
	       "    The lmc slot to select after reboot. If not supplied, AUBOOT\n"
	       "    will be selected. If immediate is selected, this argument is\n"
	       "    ignored.\n"
	       "\n");
}

static int managed_reboot(int lmc_no, bool test)
{
	itc_mbox_id_t my_mbox;
	char mailbox_name[50];
	int ret;

	/*Set up ITC*/
	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Unable to inizalize ITC!\n");
		return -1;
	}
	snprintf(mailbox_name, sizeof(mailbox_name), "restart-%d", getpid());
	my_mbox = itc_create_mailbox(mailbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		printf("Unable to create ITC mailbox!\n");
		return -1;
	}

	if (test)
		ret = reboot_with_test_on_slot("restart", "Cold with test",
		                               lmc_no);
	else
		ret = reboot_with_slot("restart", "Managed", lmc_no);

	if (ret == LIBREBOOT_SERVER_ERROR) {
		printf("Cannot locate lmc_server!\n");
		return -1;
	} else if (ret == LIBREBOOT_LMC_INVALID) {
		printf("LMC %d is not valid!\n", lmc_no);
		return -1;
	}

	return 0;
}

int main(int argc, char *argv[])
{
	int lmc_no = -1;
	char *endptr = NULL;
	int argid;
	bool immediate = false;
	bool test = false;
	const char *const short_options = "mith";
	const struct option long_options[] = {
		{ "managed",   no_argument, NULL, 'm' },
		{ "immediate", no_argument, NULL, 'i' },
		{ "test",      no_argument, NULL, 't' },
		{ "help",      no_argument, NULL, 'h' },
		{ 0, 0, 0, 0 },
	};

	for (;;) {
		argid = getopt_long(argc, argv, short_options, long_options, NULL);

		if (argid == -1)
			break;

		switch (argid) {
		case 'm':
			break;
		case 'i':
			immediate = true;
			break;
		case 't':
			test = true;
			break;
		case 'h':
			print_usage();
			return 0;
		default:
			print_usage();
			return 1;
		}
	}

	if (immediate && test) {
		printf("Both immediate and test given, you must choose one!\n");
		return 1;
	}

	if (optind < argc) {
		lmc_no = strtol(argv[optind], &endptr, 10);
		if( (*endptr != '\0') || (lmc_no < 0) ) {
			print_usage();
			return 1;
		}
	}

	if (immediate)
		/* Will never return from this call */
		reboot_immediate("restart", "Immediate");

	return managed_reboot(lmc_no, test);
}

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
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include <inttypes.h>
#include "itc.h"
#include "nvpi3.h"
#include "nvpi3_msg.h"
#include "parcmd.h"

#define PARLIST_MAILBOX_NAME        "parlist"

union itc_msg {
	uint32_t msgno;
	NVPI3_MESSAGES;
};

static void print_usage(void)
{
	printf("NAME\n");
	printf("    parlist - Lists database groups.\n");
	printf("\n");
	printf("SYNOPSIS:\n");
	printf("    parlist [-h]\n");
	printf("\n");
	printf("DESCRIPTION\n");
	printf("    Command to list database groups.\n");
	printf("\n");
	printf("    -h prints this text and exits.\n");
}

static nvpi3_result_t list_db_groups_callback(
	void *user_data, uint32_t group_index, uint32_t num_of_groups,
	const char *name, uint32_t num_of_def,
	const struct nvpi3_db_definition def[])
{
	if (!group_index) {
		printf("Group\n");
	}
	printf("%s\n", name);
	return NVPI3_RESULT_SUCCESS;
}

int main(int argc, char *argv[])
{
	struct parcmd_conn connection;
	int ret = -1, opt;

	while ((opt = getopt(argc, argv, "h")) != -1) {
		switch (opt) {
		case 'h':
			print_usage();
			ret = 0;
			goto main_end;
		case '?':
			if (isprint(optopt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				        optopt);
			}
			goto main_end;
		default:
			if (isprint(opt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				       opt);
			}
			goto main_end;
		}
	}

	if (optind < argc) {
		printf("Unknown non-option arguments: ");
		while (optind < argc) {
			printf("%s ", argv[optind++]);
		}
		printf("\n");
		goto main_end;
	}

	ret = parcmd_conn_establish(PARLIST_MAILBOX_NAME, &connection);
	if (ret) {
		goto main_end;
	}

	if (parcmd_list_db_groups(&connection, list_db_groups_callback, NULL) !=
	    NVPI3_RESULT_SUCCESS) {
		ret = -1;
	}

	if (parcmd_conn_disconnect(&connection)) {
		ret = -1;
	}

main_end:
	exit(ret);
}

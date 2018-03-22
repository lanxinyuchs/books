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
#include <errno.h>
#include <stdbool.h>
#include <getopt.h>
#include <itc.h>
#include <unistd.h>
#include "lmc_server.h"

#define MAILBOX_SIZE       5
#define CONN_ESTABLISH_TMO 0
#define LMC_MAX_SUBFILES   16
#define LMC_MAX_LOADFILES  4

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	LMC_SERVER_MESSAGES;
};

static void print_usage(void)
{
	printf("Usage: lmlist [<number of lmc>]\n\n"
	       "     List the lms stored in lmc, if number of lmc\n"
	       "     is not given list the lms in the current lmc\n\n");
}

static char *err2str(int result)
{
	switch (result) {
	case LMC_RESULT_NOT_FOUND:
		return "No load file found";
	case LMC_TOO_MANY_SUBFILES:
		return "Too many subfiles in LMC";
	case LMC_RESULT_OTHER_ERROR:
		return "Read LMC failed";
	default:
		return "";
	}
}

int main(int argc, char **argv)
{
	int i;
	char *endptr = NULL;
	uint32_t use_current = 0;
	uint32_t lmc_index = LMC_MAX_LOADFILES;
	uint32_t nof_subfiles;
	union itc_msg *msg = NULL;

	char mailbox_name[50];
	itc_mbox_id_t my_mbox = ITC_NO_ID;
	itc_mbox_id_t server_mbox;
	uint32_t procedure_ref = 0;
	uint32_t client_ref = 1234;
	uint32_t requested_versions[] = {LMC_SERVER_VERSIONS};
	LMC_CONN_ESTABLISH_MESSAGES_STRUCT(lm_list_conn_messages);
	uint32_t server_ref;
	uint32_t selected_version;
	uint32_t res;
	int status = -1;

	if (argc > 2) {
		printf("Too many arguments\n");
		print_usage();
		goto exit;
	}
	if (argc == 1) {
		use_current = 1;
	} else if (strcmp(argv[1], "-h") == 0) {
		status = 0;
		print_usage();
		goto exit;
	} else {
		lmc_index = strtol(argv[1], &endptr, 10);
		if (*endptr != '\0') {
			printf("Invalid argument\n");
			print_usage();
			goto exit;
		}
	}

	if ((use_current == 0) && (lmc_index >= LMC_MAX_LOADFILES)) {
		printf("Wrong lmc index\n");
		print_usage();
		goto exit;
	}

	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Unable to inizalize ITC!\n");
		goto exit;
	}

	snprintf(mailbox_name, sizeof(mailbox_name), "%s-%d", argv[0], getpid());
	my_mbox = itc_create_mailbox(mailbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		printf("Unable to create ITC mailbox!\n");
		goto exit;
	}

	server_mbox = itc_locate(LMC_SERVER_NAME);
	if (server_mbox == ITC_NO_ID) {
		printf("Cannot locate the server \"%s\"\n", LMC_SERVER_NAME);
		goto exit;
	}

	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &lm_list_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if (res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection establish failed (reason:0x%x)\n", res);
		goto exit;
	}

	msg = itc_alloc(sizeof(struct lmc_get_subfile_info_req),
	                LMC_GET_SUBFILE_INFO_REQ);
	msg->get_subfile_info_req.procedure_ref = ++procedure_ref;
	msg->get_subfile_info_req.connection_ref = server_ref;
	msg->get_subfile_info_req.use_current = use_current;
	msg->get_subfile_info_req.index = lmc_index;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, 5000, server_mbox);
	if (!msg) {
		printf("Timeout! Server did not reply within 5 seconds,\n");
		goto exit;
	}

	if (msg->any_msg.procedure_ref != procedure_ref) {
		printf("Server replied with invalid procedure_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       procedure_ref, msg->any_msg.procedure_ref);
		goto exit;
	}

	if (msg->any_msg.connection_ref != client_ref) {
		printf("Server replied with invalid connection_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       client_ref, msg->any_msg.connection_ref);
		goto exit;
	}

	if ((msg->msgno != LMC_GET_SUBFILE_INFO_CFM) &&
	    (msg->msgno != LMC_GET_SUBFILE_INFO_REJ)){
		printf("Server replied with unexpected message;"
		       "message number: 0x%08x\n",
		       msg->msgno);
		goto exit;
	}

	if (msg->msgno == LMC_GET_SUBFILE_INFO_REJ) {
		printf("Get lmlist failed on LMC%d, %s\n",
		       lmc_index,
		       err2str(msg->get_subfile_info_rej.error_code));
		goto exit;
	}

	printf("   %-16s%-32s%-32s\n", "Type", "Suid/magic", "name");
	nof_subfiles = msg->get_subfile_info_cfm.counter;

	for (i = 0; i < nof_subfiles && i < LMC_MAX_SUBFILES; i++) {
		/* Print the index */
		printf("%2d ", i);
		/* Print LM type, Suid/Magic, name */
		if (msg->get_subfile_info_cfm.list[i].magic !=
		    XLF_IBOOT_WPR_MAGIC_BLOB) {
			printf("%-16s", "Applic");
			printf("%-32x\n", msg->get_subfile_info_cfm.list[i].magic);
			continue;
		}
		switch (msg->get_subfile_info_cfm.list[i].blob.info.type) {
		case XLF_BLOB_TYPE_UENVIMAGE:
			printf("%-16s", "Blob:UENVIMAGE");
			break;
		case XLF_BLOB_TYPE_DTB:
			printf("%-16s", "Blob:DTB");
			break;
		case XLF_BLOB_TYPE_ROOTFS:
			printf("%-16s", "Blob:ROOTFS");
			break;
		case XLF_BLOB_TYPE_UIMAGE:
			printf("%-16s", "Blob:UIMAGE");
			break;
		case XLF_BLOB_TYPE_ASCII_DB:
			printf("%-16s", "Blob:ASCII DB");
			break;
		default:
			printf("%-16s", "Blob");
			break;
		}
		printf("%-32s", msg->get_subfile_info_cfm.list[i].blob.info.lmid);
		printf("%-32s\n",
		       msg->get_subfile_info_cfm.list[i].blob.info.name);
	}

	itc_free(&msg);
	res = conn_disconnect(server_mbox, ++procedure_ref,
	                      server_ref, &lm_list_conn_messages,
			      CONN_ESTABLISH_TMO);
	if (res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection disconnect failed (reason:0x%x)\n", res);
		goto exit;
	}

	status = 0;

exit:
	if (msg)
		itc_free(&msg);
	if (my_mbox != ITC_NO_ID)
		itc_delete_mailbox(my_mbox);

	return status;
}

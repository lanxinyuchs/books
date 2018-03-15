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

#define MAILBOX_SIZE 5
#define CONN_ESTABLISH_TMO 0

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	LMC_SERVER_MESSAGES;
};

static void print_usage(void)
{
	printf("Usage: lmcdelete -h|<pid>\n\n"
	       "Delete the lmc with the given pid\n"
	       "-h: Print this message\n"
	       "pid:\n"
	       "     SW product identity of the LMC to delete.\n"
	       "     The pid is listed with the lmclist\n\n");
}

static char *err2str(uint32_t result)
{
	switch (result) {
	case LMC_RESULT_WRONG_STATE:
		return "Loading lmc now";
	case LMC_RESULT_ACCESS_DENIED:
		return "Try to delete current excuting lmc"
		       " or the last working auboot";
	case LMC_RESULT_INVALID_PARAM:
		return "Unexpected parameter value";
	case LMC_RESULT_OTHER_ERROR:
		return "Other error";
	default:
		return "";
	}
}

int main( int argc, char **argv )
{
	char *pid = NULL;
	union itc_msg *msg = NULL;

	char mailbox_name[50];
	itc_mbox_id_t my_mbox = ITC_NO_ID;
	itc_mbox_id_t server_mbox;
	uint32_t procedure_ref = 0;
	uint32_t client_ref = 1234;
	uint32_t requested_versions[] = {LMC_SERVER_VERSIONS};
	LMC_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	uint32_t server_ref;
	uint32_t selected_version;
	uint32_t res;
	uint32_t lmc_len;
	uint32_t max_len;
	int status = -1;

	if (argc != 2) {
		print_usage();
		goto exit;
	} else if (strcmp(argv[1], "-h") == 0) {
		print_usage();
		status = 0;
		goto exit;
	} else {
		pid = argv[1];
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
	if(server_mbox == ITC_NO_ID) {
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
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection establish failed (reason:0x%x)\n", res);
		goto exit;
	}

	lmc_len = strlen(pid);
	max_len = XLF_SUID_LEN -1;
	if (lmc_len > max_len) {
		printf("Given PID is too long(%u)", lmc_len);
		goto exit;
	}
	msg = itc_alloc(sizeof(struct lmc_load_file_delete_req) + lmc_len + 1,
	                LMC_LOAD_FILE_DELETE_REQ);
	msg->load_file_delete_req.procedure_ref = ++procedure_ref;
	msg->load_file_delete_req.connection_ref = server_ref;
	strncpy(msg->load_file_delete_req.loadmodule, pid, lmc_len + 1);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	/* First receive reply from lmc server */
	msg = itc_receive(ITC_NOFILTER, 5000, server_mbox);
	if(!msg) {
		printf("Timeout! Server did not reply within 5 seconds,\n");
		goto exit;
	}

	if(msg->any_msg.procedure_ref != procedure_ref) {
		printf("Server replied with invalid procedure_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       procedure_ref, msg->any_msg.procedure_ref);
		goto exit;
	}
	if(msg->any_msg.connection_ref != client_ref) {
		printf("Server replied with invalid connection_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       client_ref, msg->any_msg.connection_ref);
		goto exit;
	}

	if(msg->msgno == LMC_LOAD_FILE_DELETE_REJ) {
		printf("Server replied with reject message;\n"
		       "error code: %s\n",
		       err2str(msg->load_file_delete_rej.error_code));
		goto exit;
	} else if(msg->msgno != LMC_LOAD_FILE_DELETE_CFM) {
		printf("Server replied with unexpected message;"
		       "message number: 0x%08x\n",
		       msg->msgno);
		goto exit;
	}

	printf("Start deleting LMC, wait for indication\n");
	itc_free(&msg);
	/* Indication is sent from slave thread */
	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	if (msg->msgno != LMC_LOAD_FILE_DELETE_IND) {
		printf("Receive unexpected message;\n"
		       "message number: 0x%08x\n",
		       msg->msgno);
		goto exit;
	}
	if (msg->load_file_delete_ind.result != LMC_RESULT_SUCCESS) {
		printf("Delete LMC failed, %s\n",
		       err2str(msg->load_file_delete_ind.result));
		goto exit;
	}
	printf("Success: file deleted\n");
	status = 0;
exit:
	if (msg)
		itc_free(&msg);
	if (my_mbox != ITC_NO_ID)
		itc_delete_mailbox(my_mbox);

	return status;
}

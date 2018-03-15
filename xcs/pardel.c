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
#include <sys/queue.h>
#include "itc.h"
#include "nvpi3.h"
#include "nvpi3_msg.h"
#include "parcmd.h"

#define PARDEL_MAILBOX_NAME        "pardel"

union itc_msg {
	uint32_t msgno;
	NVPI3_MESSAGES;
};

struct read_ind_item {
	STAILQ_ENTRY(read_ind_item) slist;
	struct nvpi3_read_ind *ind;
};

static void print_usage(void)
{
	printf("NAME\n");
	printf("    pardel - Deletes parameters.\n");
	printf("\n");
	printf("SYNOPSIS:\n");
	printf("    pardel [-h] -g <group> -n <name> [-a]\n");
	printf("\n");
	printf("DESCRIPTION\n");
	printf("    Command to delete parameters.\n");
	printf("\n");
	printf("    -h prints this text and exits.\n");
	printf("    <group> defines which database group to delete parameters in.\n");
	printf("    <name> is defines the names of the paramaters to delete.\n");
	printf("    Regular expressions using POSIX Extended Regular Expression syntax are\n");
	printf("    allowed in name.\n");
	printf("    -a is optional and enables delete of parameters matching <name> that are\n");
	printf("    taggged deleted.\n");
}

static nvpi3_result_t collect_ind_callback(void *user_data,
                                           struct nvpi3_read_ind **ind)
{
	STAILQ_HEAD(slist_head, read_ind_item) *head = user_data;
	struct read_ind_item *item = malloc(sizeof(struct read_ind_item));
	if (item == NULL) {
		printf("malloc of size %d failed, aborting\n",
		       sizeof(struct read_ind_item));
		return NVPI3_RESULT_OTHER_ERROR;
	}

	item->ind = *ind;
	*ind = NULL;
	STAILQ_INSERT_TAIL(head, item, slist);
	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t delete_node(const struct parcmd_conn *conn,
                                  nvpi3_db_group_handle group_handle,
                                  const char *name, int *monitored)
{
	struct nvpi3_delete_node_req *req;
	union itc_msg *reply;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	uint32_t size = strlen(name) + 1;
	static const uint32_t filter[] =
		{3, NVPI3_DELETE_NODE_CFM, NVPI3_DELETE_NODE_REJ,
		 ITC_MONITOR_DEFAULT_NO};


	req = (struct nvpi3_delete_node_req *)
		itc_alloc(offsetof(struct nvpi3_delete_node_req, node_name) +
	                  size, NVPI3_DELETE_NODE_REQ);
	req->connection_ref = conn->server_ref;
	req->group_handle = group_handle;
	req->transaction_handle = NULL;
	memcpy(req->node_name, name, size);
	itc_send((union itc_msg **) &req, conn->server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, conn->server_mbox);
	switch (reply->msgno) {
	case NVPI3_DELETE_NODE_CFM:
		printf("Deleted node %s\n", name);
		break;
	case NVPI3_DELETE_NODE_REJ:
		if (reply->nvpi3_delete_node_rej.error ==
		    NVPI3_RESULT_NOT_FOUND) {
			break;
		}
		printf("Received NVPI3_DELETE_NODE_REJ (%" PRIu32
		       "), aborting\n", reply->nvpi3_delete_node_rej.error);
		result = reply->nvpi3_delete_node_rej.error;
		break;
	case ITC_MONITOR_DEFAULT_NO:
		printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
		result = NVPI3_RESULT_OTHER_ERROR;
		*monitored = 0;
		break;
	}

	itc_free(&reply);
	return result;
}

static nvpi3_result_t delete_key(const struct parcmd_conn *conn,
                                 nvpi3_db_group_handle group_handle,
                                 const char *name, nvpi3_key_type_t type,
                                 int *monitored)
{
	struct nvpi3_delete_key_req *req;
	union itc_msg *reply;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	uint32_t size = strlen(name) + 1;
	static const uint32_t filter[] =
		{3, NVPI3_DELETE_KEY_CFM, NVPI3_DELETE_KEY_REJ,
		 ITC_MONITOR_DEFAULT_NO};

	req = (struct nvpi3_delete_key_req *)
		itc_alloc(offsetof(struct nvpi3_delete_key_req, key_name) +
		          size, NVPI3_DELETE_KEY_REQ);
	req->connection_ref = conn->server_ref;
	req->group_handle = group_handle;
	req->transaction_handle = NULL;
	req->type = type;
	memcpy(req->key_name, name, size);
	itc_send((union itc_msg **) &req, conn->server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, conn->server_mbox);
	switch (reply->msgno) {
	case NVPI3_DELETE_KEY_CFM:
		printf("Deleted key %s of type %" PRIu32 "\n", name, type);
		break;
	case NVPI3_DELETE_KEY_REJ:
		if (reply->nvpi3_delete_key_rej.error ==
		    NVPI3_RESULT_NOT_FOUND) {
			break;
		}
		printf("Received NVPI3_DELETE_KEY_REJ (%" PRIu32
		       "), aborting\n", reply->nvpi3_delete_key_rej.error);
		result = reply->nvpi3_delete_key_rej.error;
		break;
	case ITC_MONITOR_DEFAULT_NO:
		printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
		result = NVPI3_RESULT_OTHER_ERROR;
		*monitored = 0;
		break;
	}

	itc_free(&reply);
	return result;
}

static nvpi3_result_t par_del(const struct parcmd_conn *conn,
                              const char *group_name, const char *pattern,
                              uint32_t flags)
{
	nvpi3_db_group_handle group_handle;
	nvpi3_result_t result, result2;
	itc_monitor_id_t monitor_id;
	int monitored;
	STAILQ_HEAD(slist_head, read_ind_item) head =
		STAILQ_HEAD_INITIALIZER(head);

	result = parcmd_open_db_group(conn, group_name, &group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		return result;
	}

	result = parcmd_read(conn, group_handle, flags, pattern,
	                     collect_ind_callback, &head);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto par_del_end;
	}

	monitor_id = itc_monitor(conn->server_mbox, NULL);
	monitored = 1;

	for (;;) {
		struct read_ind_item *item = STAILQ_FIRST(&head);
		if (item == NULL) {
			break;
		}

		if (result == NVPI3_RESULT_SUCCESS) {
			if (item->ind->type == NVPI3_READ_IND_NODE) {
				result = delete_node(conn, group_handle,
				                     (const char *) item->ind +
				                     item->ind->name_offset,
			                             &monitored);
			} else {
				result = delete_key(conn, group_handle,
				                    (const char *) item->ind +
				                    item->ind->name_offset,
				                    item->ind->u.key.type,
				                    &monitored);
			}
		}

		STAILQ_REMOVE_HEAD(&head, slist);
		itc_free((union itc_msg **) &item->ind);
		free(item);
	}

	if (monitored) {
		itc_unmonitor(monitor_id);
	}

	result2 = parcmd_close_db_group(conn, group_handle);
	if (result == NVPI3_RESULT_SUCCESS) {
		result = result2;
	}

par_del_end:
	return result;
}

int main(int argc, char *argv[])
{
	struct parcmd_conn connection;
	int ret = 0, opt;
	uint32_t flags = 0;
	const char *group_name = NULL, *pattern = NULL;

	while ((opt = getopt(argc, argv, "hg:n:a")) != -1) {
		switch (opt) {
		case 'h':
			print_usage();
			goto main_end;
		case 'g':
			group_name = optarg;
			break;
		case 'n':
			pattern = optarg;
			break;
		case 'a':
			flags = NVPI3_READ_REQ_FLAGS_ALL;
			break;
		case '?':
			if (optopt == 'g' || optopt == 'n') {
				printf("Option -%c requires an argument.\n",
				        optopt);
			} else if (isprint(optopt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				        optopt);
			}
			ret = -1;
			goto main_end;
		default:
			if (isprint(opt)) {
				printf("Unknown option `-%c'.\n", optopt);
			} else {
				printf("Unknown option character `0x%x'.\n",
				        opt);
			}
			ret = -1;
			goto main_end;
		}
	}

	if (group_name == NULL) {
		printf("Missing argument -g <group>\n");
		ret = -1;
	}

	if (pattern == NULL) {
		printf("Missing argument -n <name>\n");
		ret = -1;
	}

	if (optind < argc) {
		printf("Unknown non-option arguments: ");
		while (optind < argc) {
			printf("%s ", argv[optind++]);
		}
		printf("\n");
		ret = -1;
	}

	if (ret) {
		goto main_end;
	}

	ret = parcmd_conn_establish(PARDEL_MAILBOX_NAME, &connection);
	if (ret) {
		goto main_end;
	}

	if (par_del(&connection, group_name, pattern, flags) !=
	            NVPI3_RESULT_SUCCESS) {
		ret = -1;
	}

	if (parcmd_conn_disconnect(&connection)) {
		ret = -1;
	}

main_end:
	exit(ret);
}

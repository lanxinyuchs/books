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
#include <unistd.h>
#include <string.h>
#include <inttypes.h>
#include "itc.h"
#include "nvpi3_func.h"
#include "parcmd.h"

#define PARCMD_MAX_NO_OF_MAILBOXES 4

union itc_msg {
	uint32_t msgno;
	NVPI3_MESSAGES;
};

NVPI3_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);

static uint32_t requested_versions[] = {NVPI3_SERVER_VERSIONS};

int parcmd_conn_establish(const char *mbox_name,
                          struct parcmd_conn *conn)
{
	int ret;
	uint32_t res;
	itc_mbox_id_t mbox_id = ITC_NO_ID;

	ret = itc_init(PARCMD_MAX_NO_OF_MAILBOXES, ITC_MALLOC, NULL,
	               ITC_NO_NAMESPACE, 0);
	if (ret) {
		printf("itc_init failed (%d), aborting\n", ret);
		goto parcmd_conn_establish_err;
	}

	mbox_id = itc_create_mailbox(mbox_name, 0);
	if (mbox_id == ITC_NO_ID) {
		printf("itc_create_mailbox %s failed, aborting\n", mbox_name);
		goto parcmd_conn_establish_err;
	}

	conn->server_mbox = itc_locate(NVPI3_SERVER_NAME);
	if(conn->server_mbox == ITC_NO_ID) {
		printf("itc_locate %s failed, aborting\n", mbox_name);
		goto parcmd_conn_establish_err;
	}

	conn->conn_messages = &conn_messages;

	/* establish connection to the server using libconn_establish_helper.*/
	res = conn_establish(
	              /*input parameters*/
	              conn->server_mbox,
	              0, /* procedure_ref */
	              getpid(), /* client_ref */
	              sizeof(requested_versions)/sizeof(requested_versions[0]),
	              requested_versions,
	              conn->conn_messages,
	              PARCMD_TIMEOUT,
	              /*returned vaglues*/
	              &conn->server_ref,
	              &conn->selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("conn_establish failed (%" PRIu32 "), aborting\n", res);
		goto parcmd_conn_establish_err;
	}

	return 0;

parcmd_conn_establish_err:
	if (mbox_id != ITC_NO_ID) {
		itc_delete_mailbox(mbox_id);
	}

	return -1;
}

int parcmd_conn_disconnect(const struct parcmd_conn *conn)
{
	itc_mbox_id_t mbox_id;
	uint32_t res;

	res = conn_disconnect(conn->server_mbox,
	                      0, /* procedure_ref */
	                      conn->server_ref,
	                      &conn_messages,
	                      PARCMD_TIMEOUT );
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("conn_disconnect failed (%" PRIu32 "), aborting\n", res);
	}

	mbox_id = itc_current_mbox();
	if (mbox_id != ITC_NO_ID) {
		itc_delete_mailbox(mbox_id);
	}

	return (res == CONN_ESTABLISH_SUCCESS) ? 0 : -1;
}

nvpi3_result_t parcmd_list_db_groups(
	const struct parcmd_conn *conn,
	nvpi3_result_t (*callback)(void *user_data, uint32_t group_index,
	                           uint32_t num_of_groups, const char *name,
	                           uint32_t num_of_def,
	                           const struct nvpi3_db_definition def[]),
	void *user_data)
{
	struct nvpi3_list_db_groups_req *req;
	union itc_msg *reply;
	itc_monitor_id_t monitor_id;
	int monitored;
	uint32_t group_index;
	nvpi3_result_t result;

	static const uint32_t filter_cfm_rej[] =
		{3, NVPI3_LIST_DB_GROUPS_CFM, NVPI3_LIST_DB_GROUPS_REJ,
		 ITC_MONITOR_DEFAULT_NO};
	static const uint32_t filter_ind[] =
		{3, NVPI3_LIST_DB_GROUPS_IND, NVPI3_LIST_DB_GROUPS_END_IND,
		 ITC_MONITOR_DEFAULT_NO};

	monitor_id = itc_monitor(conn->server_mbox, NULL);
	monitored = 1;

	req = (struct nvpi3_list_db_groups_req *)
		itc_alloc(sizeof(struct nvpi3_list_db_groups_req),
		          NVPI3_LIST_DB_GROUPS_REQ);
	req->connection_ref = conn->server_ref;
	itc_send((union itc_msg **) &req, conn->server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter_cfm_rej, ITC_NO_TMO, conn->server_mbox);
	switch (reply->msgno) {
	case NVPI3_LIST_DB_GROUPS_CFM:
		break;
	case NVPI3_LIST_DB_GROUPS_REJ:
		result = reply->nvpi3_list_db_groups_rej.error;
		printf("Received NVPI3_LIST_DB_GROUPS_REJ (%" PRIu32
		       "), aborting\n", result);
		goto parcmd_list_db_groups_end;
	case ITC_MONITOR_DEFAULT_NO:
		printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
		result = NVPI3_RESULT_OTHER_ERROR;
		monitored = 0;
		goto parcmd_list_db_groups_end;
	}

	itc_free(&reply);

	for  (group_index = 0; ; group_index++) {
		reply = itc_receive(filter_ind, ITC_NO_TMO, conn->server_mbox);
		switch (reply->msgno) {
		case NVPI3_LIST_DB_GROUPS_IND:
			result = callback(user_data, group_index,
			                  reply->nvpi3_list_db_groups_ind.
			                  num_of_groups,
			                  reply->nvpi3_list_db_groups_ind.name,
			                  reply->nvpi3_list_db_groups_ind.
			                  num_of_definitions,
			                  reply->nvpi3_list_db_groups_ind.
			                  definition);

			if (result != NVPI3_RESULT_SUCCESS) {
				goto parcmd_list_db_groups_end;
			}
			break;
		case NVPI3_LIST_DB_GROUPS_END_IND:
			result = reply->nvpi3_list_db_groups_end_ind.result;
			if (result != NVPI3_RESULT_SUCCESS) {
				printf("Received NVPI3_LIST_DB_GROUPS_END_IND "
				       "with error %" PRIu32 ", aborting\n",
				       result);

			}
			goto parcmd_list_db_groups_end;
		case ITC_MONITOR_DEFAULT_NO:
			printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
			result = NVPI3_RESULT_OTHER_ERROR;
			monitored = 0;
			goto parcmd_list_db_groups_end;
		}
		itc_free(&reply);
	}

parcmd_list_db_groups_end:
	itc_free(&reply);

	if (monitored) {
		itc_unmonitor(monitor_id);
	}
	return result;
}

nvpi3_result_t parcmd_open_db_group(const struct parcmd_conn *conn,
                                    const char *name,
                                    nvpi3_db_group_handle *group_handle)
{
	struct nvpi3_open_db_group_req *req;
	union itc_msg *reply;
	itc_monitor_id_t monitor_id;
	int monitored;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	static const uint32_t filter[] =
		{3, NVPI3_OPEN_DB_GROUP_CFM, NVPI3_OPEN_DB_GROUP_REJ,
		 ITC_MONITOR_DEFAULT_NO};

	*group_handle = NULL;
	monitor_id = itc_monitor(conn->server_mbox, NULL);
	monitored = 1;

	req = (struct nvpi3_open_db_group_req *)
		itc_alloc(offsetof(struct nvpi3_open_db_group_req, name) +
		          strlen(name) + 1, NVPI3_OPEN_DB_GROUP_REQ);
	req->connection_ref = conn->server_ref;
	strcpy(req->name, name);
	itc_send((union itc_msg **) &req, conn->server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, conn->server_mbox);
	switch (reply->msgno) {
	case NVPI3_OPEN_DB_GROUP_CFM:
		*group_handle = reply->nvpi3_open_db_group_cfm.group_handle;
		break;
	case NVPI3_OPEN_DB_GROUP_REJ:
		printf("Received NVPI3_OPEN_DB_GROUP_REJ (%" PRIu32
		       "), aborting\n", reply->nvpi3_open_db_group_rej.error);
		result = reply->nvpi3_open_db_group_rej.error;
		break;

	case ITC_MONITOR_DEFAULT_NO:
		printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
		result = NVPI3_RESULT_OTHER_ERROR;
		monitored = 0;
		break;
	}

	if (monitored) {
		itc_unmonitor(monitor_id);
	}

	itc_free(&reply);
	return result;
}

nvpi3_result_t parcmd_close_db_group(const struct parcmd_conn *conn,
                                     nvpi3_db_group_handle group_handle)
{
	struct nvpi3_close_db_group_req *req;
	union itc_msg *reply;
	itc_monitor_id_t monitor_id;
	int monitored;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	static const uint32_t filter[] =
		{3, NVPI3_CLOSE_DB_GROUP_CFM, NVPI3_CLOSE_DB_GROUP_REJ,
		 ITC_MONITOR_DEFAULT_NO};

	monitor_id = itc_monitor(conn->server_mbox, NULL);
	monitored = 1;

	req = (struct nvpi3_close_db_group_req *)
		itc_alloc(sizeof(struct nvpi3_close_db_group_req),
		          NVPI3_CLOSE_DB_GROUP_REQ);
	req->connection_ref = conn->server_ref;
	req->group_handle = group_handle;
	itc_send((union itc_msg **) &req, conn->server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, conn->server_mbox);
	switch (reply->msgno) {
	case NVPI3_CLOSE_DB_GROUP_CFM:
		break;
	case NVPI3_CLOSE_DB_GROUP_REJ:
		printf("Received NVPI3_CLOSE_DB_GROUP_REJ (%" PRIu32
		       "), aborting\n", reply->nvpi3_close_db_group_rej.error);
		result = reply->nvpi3_close_db_group_rej.error;
		break;
	case ITC_MONITOR_DEFAULT_NO:
		printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
		result = NVPI3_RESULT_OTHER_ERROR;
		monitored = 0;
		break;
	}

	if (monitored) {
		itc_unmonitor(monitor_id);
	}

	itc_free(&reply);
	return result;
}

nvpi3_result_t parcmd_read(
	const struct parcmd_conn *conn, nvpi3_db_group_handle group_handle,
	uint32_t flags, const char *pattern,
	nvpi3_result_t (*callback)(void *user_data,
	                           struct nvpi3_read_ind **ind),
	void *user_data)
{
	struct nvpi3_read_req *req;
	union itc_msg *reply;
	itc_monitor_id_t monitor_id;
	int monitored;
	nvpi3_result_t result;
	static const uint32_t filter_cfm_rej[] =
		{3, NVPI3_READ_CFM, NVPI3_READ_REJ, ITC_MONITOR_DEFAULT_NO};
	static const uint32_t filter_ind[] =
		{3, NVPI3_READ_IND, NVPI3_READ_END_IND, ITC_MONITOR_DEFAULT_NO};

	monitor_id = itc_monitor(conn->server_mbox, NULL);
	monitored = 1;

	req = (struct nvpi3_read_req *)
		itc_alloc(offsetof(struct nvpi3_read_req, pattern) +
		          strlen(pattern) + 1, NVPI3_READ_REQ);
	req->connection_ref = conn->server_ref;
	req->group_handle = group_handle;
	req->flags = flags;
	strcpy(req->pattern, pattern);
	itc_send((union itc_msg **) &req, conn->server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter_cfm_rej, ITC_NO_TMO, conn->server_mbox);
	switch (reply->msgno) {
	case NVPI3_READ_CFM:
		break;
	case NVPI3_READ_REJ:
		result = reply->nvpi3_read_rej.error;
		printf("Received NVPI3_READ_REJ (%" PRIu32 "), aborting\n",
		       result);
		goto parcmd_read_end;
	case ITC_MONITOR_DEFAULT_NO:
		printf("Server %s died, aborting\n",
		       NVPI3_SERVER_NAME);
		result = NVPI3_RESULT_OTHER_ERROR;
		monitored = 0;
		goto parcmd_read_end;
	}

	itc_free(&reply);

	for (;;) {
		reply = itc_receive(filter_ind, ITC_NO_TMO,  conn->server_mbox);
		switch (reply->msgno) {
		case NVPI3_READ_IND:
			result = callback(user_data,
			                 (struct nvpi3_read_ind **) &reply);
			if (result != NVPI3_RESULT_SUCCESS) {
				goto parcmd_read_end;
			}
			break;
		case NVPI3_READ_END_IND:
			result = reply->nvpi3_read_end_ind.result;
			if (result != NVPI3_RESULT_SUCCESS) {
				printf("Received NVPI3_READ_END_IND with error "
				       "%" PRIu32 ", aborting\n", result);

			}
			goto parcmd_read_end;
		case ITC_MONITOR_DEFAULT_NO:
			printf("Server %s died, aborting\n", NVPI3_SERVER_NAME);
			result = NVPI3_RESULT_OTHER_ERROR;
			monitored = 0;
			goto parcmd_read_end;
		}
		if (reply != NULL) {
			itc_free(&reply);
		}
	}

parcmd_read_end:
	itc_free(&reply);

	if (monitored) {
		itc_unmonitor(monitor_id);
	}

	return result;
}

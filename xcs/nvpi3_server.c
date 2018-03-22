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
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <sys/queue.h>
#include <regex.h>
#include "itc.h"
#include "conn-establish-helper.h"
#include "nvpi3.h"
#include "nvpi3_cfg.h"
#include "nvpi3_srv.h"
#include "nvpi3_msg.h"
#include "nvpi3_func.h"
#include "log_tpt.h"

#define MAX_NUM_OF_MAILBOXES  4
#define DAEMON_NAME           "nvpi3_serverd"
#define EXIT_SIGNAL           0xdeadbeef

union itc_msg {
	uint32_t msgno;
	struct {
		NVPI3_MSG_HEADER;
	} header;
	NVPI3_MESSAGES;
};

struct nvpi3_transaction_msg {
	TAILQ_ENTRY(nvpi3_transaction_msg) list;
	union itc_msg *msg;
};

struct nvpi3_transaction_object {
	LIST_ENTRY(nvpi3_transaction_object) list;
	TAILQ_HEAD(transaction_msg_head, nvpi3_transaction_msg) msg_head;
};

struct client_entry {
	LIST_ENTRY(client_entry) list;
	LIST_HEAD(transaction_head, nvpi3_transaction_object) transaction_head;
};

struct get_value_callback_data {
	uint32_t procedure_ref;
	uint32_t connection_ref;
	itc_mbox_id_t sender;
};

struct enum_keys_callback_data {
	uint32_t procedure_ref;
	uint32_t connection_ref;
	itc_mbox_id_t sender;
	uint32_t flags;
	regex_t regex;
};

struct enum_db_group_callback_data {
	uint32_t procedure_ref;
	uint32_t connection_ref;
	itc_mbox_id_t sender;
};

static LIST_HEAD(client_head , client_entry) client_head =
	LIST_HEAD_INITIALIZER(client_head);

static itc_mbox_id_t server_mbox = ITC_NO_ID;


static void handle_create_db_group(union itc_msg *msg,
                                   struct conn_client_info *client_info,
                                   itc_mbox_id_t sender)
{
	nvpi3_db_group_handle group_handle;
	nvpi3_result_t result;
	int *cleared;
	struct nvpi3_create_db_group_req *req = &msg->nvpi3_create_db_group_req;

	cleared = malloc(sizeof(*cleared) * req->num_of_definitions);
	if (cleared == NULL) {
		TPT_ERROR(STR("malloc of size %d failed",
		              sizeof(*cleared) * req->num_of_definitions));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto handle_create_db_group_end;
	}

	result = nvpi3_srv_create_db_group(client_info->client_data, req->name,
	                                   req->num_of_definitions,
	                                   req->definition, cleared,
	                                   &group_handle);

handle_create_db_group_end:
	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_create_db_group_cfm *cfm =
			(struct nvpi3_create_db_group_cfm *)
			itc_alloc(sizeof(struct nvpi3_create_db_group_cfm),
			          NVPI3_CREATE_DB_GROUP_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		cfm->group_handle = group_handle;
		TPT_SEND_SIG(cfm->msgno, sender,
		             STR("NVPI3_CREATE_DB_GROUP_CFM %p",
		                 (void *)group_handle));
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_create_db_group_rej *rej =
			(struct nvpi3_create_db_group_rej *)
			itc_alloc(sizeof(struct nvpi3_create_db_group_rej),
			          NVPI3_CREATE_DB_GROUP_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_CREATE_DB_GROUP_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}

	free(cleared);
}

static void handle_destroy_db_group(union itc_msg *msg,
                                    struct conn_client_info *client_info,
                                    itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	struct nvpi3_destroy_db_group_req *req =
		&msg->nvpi3_destroy_db_group_req;

	result = nvpi3_srv_destroy_db_group(req->group_handle);

	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_destroy_db_group_cfm *cfm =
			(struct nvpi3_destroy_db_group_cfm *)
			itc_alloc(sizeof(struct nvpi3_destroy_db_group_cfm),
			          NVPI3_DESTROY_DB_GROUP_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender,
		             "NVPI3_DESTROY_DB_GROUP_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_destroy_db_group_rej *rej =
			(struct nvpi3_destroy_db_group_rej *)
			itc_alloc(sizeof(struct nvpi3_destroy_db_group_rej),
			          NVPI3_DESTROY_DB_GROUP_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_DESTROY_DB_GROUP_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void handle_open_db_group(union itc_msg *msg,
                                 struct conn_client_info *client_info,
                                 itc_mbox_id_t sender)
{
	nvpi3_db_group_handle group_handle;
	nvpi3_result_t result;
	struct nvpi3_open_db_group_req *req = &msg->nvpi3_open_db_group_req;

	result = nvpi3_srv_open_db_group(client_info->client_data, req->name,
	                                 &group_handle);

	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_open_db_group_cfm *cfm =
			(struct nvpi3_open_db_group_cfm *)
			itc_alloc(sizeof(struct nvpi3_open_db_group_cfm),
			          NVPI3_OPEN_DB_GROUP_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		cfm->group_handle = group_handle;
		TPT_SEND_SIG(cfm->msgno, sender,
		             STR("NVPI3_OPEN_DB_GROUP_CFM %p",
		                 (void *)group_handle));
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_open_db_group_rej *rej =
			(struct nvpi3_open_db_group_rej *)
			itc_alloc(sizeof(struct nvpi3_open_db_group_rej),
			          NVPI3_OPEN_DB_GROUP_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_OPEN_DB_GROUP_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void handle_close_db_group(union itc_msg *msg,
                                  struct conn_client_info *client_info,
                                  itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	struct nvpi3_close_db_group_req *req = &msg->nvpi3_close_db_group_req;

	result = nvpi3_srv_close_db_group(req->group_handle);

	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_close_db_group_cfm *cfm =
			(struct nvpi3_close_db_group_cfm *)
			itc_alloc(sizeof(struct nvpi3_close_db_group_cfm),
			          NVPI3_CLOSE_DB_GROUP_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender,
		             "NVPI3_CLOSE_DB_GROUP_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_close_db_group_rej *rej =
			(struct nvpi3_close_db_group_rej *)
			itc_alloc(sizeof(struct nvpi3_close_db_group_rej),
			          NVPI3_CLOSE_DB_GROUP_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_CLOSE_DB_GROUP_REJ %d", rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void handle_open_node(union itc_msg *msg,
                             struct conn_client_info *client_info,
                             itc_mbox_id_t sender)
{
	nvpi3_node_handle node_handle;
	nvpi3_result_t result;
	struct nvpi3_open_node_req *req = &msg->nvpi3_open_node_req;

	result = nvpi3_srv_open_node(req->group_handle, req->node_name,
	                             &node_handle);

	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_open_node_cfm *cfm = (struct nvpi3_open_node_cfm *)
			itc_alloc(sizeof(struct nvpi3_open_node_cfm),
			          NVPI3_OPEN_NODE_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		cfm->node_handle = node_handle;
		TPT_SEND_SIG(cfm->msgno, sender,
		             STR("NVPI3_OPEN_NODE_CFM %p",
		                 (void *)node_handle));
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_open_node_rej *rej =
			(struct nvpi3_open_node_rej *)
			itc_alloc(sizeof(struct nvpi3_open_node_rej),
			          NVPI3_OPEN_NODE_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_OPEN_NODE_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void handle_close_node(union itc_msg *msg,
                              struct conn_client_info *client_info,
                              itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	struct nvpi3_close_node_req *req = &msg->nvpi3_close_node_req;

	result = nvpi3_srv_close_node(req->node_handle);

	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_close_node_cfm *cfm =
			(struct nvpi3_close_node_cfm *)
			itc_alloc(sizeof(struct nvpi3_close_node_cfm),
			          NVPI3_CLOSE_NODE_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender,
		             "NVPI3_CLOSE_NODE_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_close_node_rej *rej =
			(struct nvpi3_close_node_rej *)
			itc_alloc(sizeof(struct nvpi3_close_node_rej),
			          NVPI3_CLOSE_NODE_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_CLOSE_NODE_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static nvpi3_result_t get_value_callback(void *user_data, uint32_t size,
                const union nvpi3_key_value *value)
{
	struct get_value_callback_data *ud = user_data;
	struct nvpi3_get_value_cfm *cfm = (struct nvpi3_get_value_cfm *)
		itc_alloc(offsetof(struct nvpi3_get_value_cfm, value) + size,
		          NVPI3_GET_VALUE_CFM);

	cfm->procedure_ref = ud->procedure_ref;
	cfm->connection_ref = ud->connection_ref;
	cfm->value_size = size;
	memcpy(&cfm->value, value, size);
	TPT_SEND_SIG(cfm->msgno, ud->sender,
	             STR("NVPI3_GET_VALUE_CFM value_size:%d",
	                 cfm->value_size));
	itc_send((union itc_msg **)&cfm, ud->sender, ITC_MY_MBOX);
	return NVPI3_RESULT_SUCCESS;
}

static void handle_get_value(union itc_msg *msg,
                             struct conn_client_info *client_info,
                             itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	struct get_value_callback_data callback_data;
	struct nvpi3_get_value_req *req = &msg->nvpi3_get_value_req;

	callback_data.procedure_ref = req->procedure_ref;
	callback_data.connection_ref = client_info->client_ref;
	callback_data.sender = sender;

	result = nvpi3_srv_get_value(req->node_handle, req->key_name, req->type,
	                             get_value_callback, &callback_data);
	if (result != NVPI3_RESULT_SUCCESS) {
		struct nvpi3_get_value_rej *rej =
			(struct nvpi3_get_value_rej *)
			itc_alloc(sizeof(struct nvpi3_get_value_rej),
			          NVPI3_GET_VALUE_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_GET_VALUE_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void handle_get_value_size(union itc_msg *msg,
                                  struct conn_client_info *client_info,
                                  itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	uint32_t value_size = 0;  /* Initialized to avoid coverity error. */
	struct nvpi3_get_value_size_req *req = &msg->nvpi3_get_value_size_req;

	result = nvpi3_srv_get_value_size(req->node_handle, req->key_name,
	                                  req->type, &value_size);
	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_get_value_size_cfm *cfm =
			(struct nvpi3_get_value_size_cfm *)
			itc_alloc(sizeof(struct nvpi3_get_value_size_cfm),
			          NVPI3_GET_VALUE_SIZE_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		cfm->value_size = value_size;
		TPT_SEND_SIG(cfm->msgno, sender, "NVPI3_GET_VALUE_SIZE_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_get_value_size_rej *rej =
			(struct nvpi3_get_value_size_rej *)
			itc_alloc(sizeof(struct nvpi3_get_value_size_rej),
			          NVPI3_GET_VALUE_SIZE_REJ);

		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_GET_VALUE_SIZE_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void handle_create_transaction(union itc_msg *msg,
                                      struct conn_client_info *client_info,
                                      itc_mbox_id_t sender)
{
	struct nvpi3_create_transaction_req *req =
		&msg->nvpi3_create_transaction_req;
	nvpi3_transaction_handle transaction_handle =
		malloc(sizeof(*transaction_handle));

	if (transaction_handle != NULL) {
		struct nvpi3_create_transaction_cfm *cfm;
		struct client_entry *entry = client_info->client_data;

		TAILQ_INIT(&transaction_handle->msg_head);
		LIST_INSERT_HEAD(&entry->transaction_head, transaction_handle,
		                 list);
		cfm = (struct nvpi3_create_transaction_cfm *)
			itc_alloc(sizeof(struct nvpi3_create_transaction_cfm),
			          NVPI3_CREATE_TRANSACTION_CFM);
		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		cfm->transaction_handle = transaction_handle;
		TPT_SEND_SIG(cfm->msgno, sender,
		             "NVPI3_CREATE_TRANSACTION_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_create_transaction_rej *rej;

		TPT_ERROR(STR("malloc of size %d failed",
		              sizeof(*transaction_handle)));
		rej = (struct nvpi3_create_transaction_rej *)
			itc_alloc(sizeof(struct nvpi3_create_transaction_rej),
			          NVPI3_CREATE_TRANSACTION_REJ);
		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = NVPI3_RESULT_OTHER_ERROR;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_CREATE_TRANSACTION_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void destroy_transaction(nvpi3_transaction_handle handle)
{
	struct nvpi3_transaction_msg *transaction_msg;

	while ((transaction_msg = TAILQ_FIRST(&handle->msg_head)) != NULL) {
		TAILQ_REMOVE(&handle->msg_head, transaction_msg, list);
		itc_free(&transaction_msg->msg);
		free(transaction_msg);
	}

	LIST_REMOVE(handle, list);
	free(handle);
}

static nvpi3_result_t check_transaction(struct client_entry *entry,
                                        nvpi3_transaction_handle handle)
{
	nvpi3_transaction_handle h_transaction;

	LIST_FOREACH(h_transaction, &entry->transaction_head, list) {
		if (h_transaction == handle) {
			return NVPI3_RESULT_SUCCESS;
		}
	}
	return NVPI3_RESULT_INVALID_PARAM;
}

static void handle_destroy_transaction(union itc_msg *msg,
                                       struct conn_client_info *client_info,
                                       itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	struct nvpi3_destroy_transaction_req *req =
		&msg->nvpi3_destroy_transaction_req;

	result = check_transaction((struct client_entry *)
	                           client_info->client_data,
	                           req->transaction_handle);
	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_destroy_transaction_cfm *cfm;

		destroy_transaction(req->transaction_handle);

		cfm = (struct nvpi3_destroy_transaction_cfm *)
			itc_alloc(sizeof(struct nvpi3_destroy_transaction_cfm),
			          NVPI3_DESTROY_TRANSACTION_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender,
		             "NVPI3_DESTROY_TRANSACTION_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_destroy_transaction_rej *rej =
			(struct nvpi3_destroy_transaction_rej *)
			itc_alloc(sizeof(struct nvpi3_destroy_transaction_rej),
			          NVPI3_DESTROY_TRANSACTION_REJ);
		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_DESTROY_TRANSACTION_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}


static nvpi3_result_t commit_transaction(nvpi3_transaction_handle handle)
{
	struct nvpi3_transaction_msg *transaction_msg;
	nvpi3_result_t result2;
	nvpi3_result_t result1 = NVPI3_RESULT_SUCCESS;

	while ((transaction_msg = TAILQ_FIRST(&handle->msg_head)) != NULL) {
		TAILQ_REMOVE(&handle->msg_head, transaction_msg, list);
		switch (transaction_msg->msg->msgno) {
		case NVPI3_DELETE_NODE_REQ: {
			struct nvpi3_delete_node_req *req =
				&transaction_msg->msg->nvpi3_delete_node_req;
			result2 = nvpi3_srv_delete_node(req->group_handle,
			                                req->node_name);
			if (result2 != NVPI3_RESULT_SUCCESS) {
				TPT_TRACE(1, STR("nvpi3_srv_delete_node failed "
				                 "(%d)", result2));
				if (result1 == NVPI3_RESULT_SUCCESS) {
					result1 = result2;
				}
			}
			break;
		}

		case NVPI3_SET_VALUE_REQ: {
			struct nvpi3_set_value_req *req =
				&transaction_msg->msg->nvpi3_set_value_req;
			result2 = nvpi3_srv_set_value(
				req->group_handle,
				(const char *) req + req->key_name_offset,
				req->type, req->value_size, &req->value);
			if (result2 != NVPI3_RESULT_SUCCESS) {
				TPT_TRACE(1,
				          STR("nvpi3_srv_set_value failed (%d)",
				              result2));
				if (result1 == NVPI3_RESULT_SUCCESS) {
					result1 = result2;
				}
			}
			break;
		}

		case NVPI3_DELETE_KEY_REQ: {
			struct nvpi3_delete_key_req *req =
				&transaction_msg->msg->nvpi3_delete_key_req;
			result2 = nvpi3_srv_delete_key(req->group_handle,
			                               req->key_name,
			                               req->type);
			if (result2 != NVPI3_RESULT_SUCCESS) {
				TPT_TRACE(1, STR("nvpi3_srv_delete_key failed "
				                 "(%d)", result2));
				if (result1 == NVPI3_RESULT_SUCCESS) {
					result1 = result2;
				}
			}
			break;
		}
		}

		itc_free(&transaction_msg->msg);
		free(transaction_msg);
	}

	return result1;
}

static void handle_commit_transaction(union itc_msg *msg,
                                      struct conn_client_info *client_info,
                                      itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	struct nvpi3_commit_transaction_req *req =
		&msg->nvpi3_commit_transaction_req;

	result = check_transaction((struct client_entry *)
	                           client_info->client_data,
	                           req->transaction_handle);
	if (result == NVPI3_RESULT_SUCCESS) {
		result = commit_transaction(req->transaction_handle);
	}

	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_commit_transaction_cfm *cfm;
		cfm = (struct nvpi3_commit_transaction_cfm *)
			itc_alloc(sizeof(struct nvpi3_commit_transaction_cfm),
			          NVPI3_COMMIT_TRANSACTION_CFM);

		cfm->procedure_ref = req->procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender,
		             "NVPI3_COMMIT_TRANSACTION_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_commit_transaction_rej *rej =
			(struct nvpi3_commit_transaction_rej *)
			itc_alloc(sizeof(struct nvpi3_commit_transaction_rej),
			          NVPI3_COMMIT_TRANSACTION_REJ);
		rej->procedure_ref = req->procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_COMMIT_TRANSACTION_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static nvpi3_result_t add_transaction_msg(nvpi3_transaction_handle handle,
                union itc_msg **msg)
{
	struct nvpi3_transaction_msg *transaction_msg =
		malloc(sizeof(*transaction_msg));

	if (transaction_msg != NULL) {
		transaction_msg->msg = *msg;
		*msg = NULL;
		TAILQ_INSERT_TAIL(&handle->msg_head, transaction_msg, list);
		return NVPI3_RESULT_SUCCESS;
	}
	return NVPI3_RESULT_OTHER_ERROR;
}

static void handle_delete_node(union itc_msg **msg,
                               struct conn_client_info *client_info,
                               itc_mbox_id_t sender)
{
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	struct nvpi3_delete_node_req *req = &(*msg)->nvpi3_delete_node_req;
	uint32_t procedure_ref = req->procedure_ref;

	if (req->transaction_handle != NULL) {
		result = check_transaction((struct client_entry *)
		                           client_info->client_data,
		                           req->transaction_handle);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = add_transaction_msg(req->transaction_handle,
			                             msg);
		}
	}

	if (*msg != NULL) {
		result = nvpi3_srv_delete_node(req->group_handle,
		                               req->node_name);
		itc_free(msg);
	}


	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_delete_node_cfm *cfm =
			(struct nvpi3_delete_node_cfm *)
			itc_alloc(sizeof(struct nvpi3_delete_node_cfm),
			          NVPI3_DELETE_NODE_CFM);

		cfm->procedure_ref = procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender, "NVPI3_DELETE_NODE_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_delete_node_rej *rej =
			(struct nvpi3_delete_node_rej *)
			itc_alloc(sizeof(struct nvpi3_delete_node_rej),
			          NVPI3_DELETE_NODE_REJ);

		rej->procedure_ref = procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_DELETE_NODE_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}

	if (*msg != NULL) {
		itc_free(msg);
	}
}

static void handle_set_value(union itc_msg **msg,
                             struct conn_client_info *client_info,
                             itc_mbox_id_t sender)
{
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	struct nvpi3_set_value_req *req = &(*msg)->nvpi3_set_value_req;
	uint32_t procedure_ref = req->procedure_ref;

	if (req->transaction_handle != NULL) {
		result = check_transaction((struct client_entry *)
		                           client_info->client_data,
		                           req->transaction_handle);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = add_transaction_msg(req->transaction_handle,
			                             msg);
		}
	}

	if (*msg != NULL) {
		result = nvpi3_srv_set_value(
		                 req->group_handle,
		                 (const char *) req + req->key_name_offset,
		                 req->type, req->value_size, &req->value);
		itc_free(msg);
	}


	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_set_value_cfm *cfm =
			(struct nvpi3_set_value_cfm *)
			itc_alloc(sizeof(struct nvpi3_set_value_cfm),
			          NVPI3_SET_VALUE_CFM);

		cfm->procedure_ref = procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender, "NVPI3_SET_VALUE_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_set_value_rej *rej =
			(struct nvpi3_set_value_rej *)
			itc_alloc(sizeof(struct nvpi3_set_value_rej),
			          NVPI3_SET_VALUE_REJ);

		rej->procedure_ref = procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_SET_VALUE_REJ %d", rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static void handle_delete_key(union itc_msg **msg,
                              struct conn_client_info *client_info,
                              itc_mbox_id_t sender)
{
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	struct nvpi3_delete_key_req *req = &(*msg)->nvpi3_delete_key_req;
	uint32_t procedure_ref = req->procedure_ref;

	if (req->transaction_handle != NULL) {
		result = check_transaction((struct client_entry *)
		                           client_info->client_data,
		                           req->transaction_handle);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = add_transaction_msg(req->transaction_handle,
			                             msg);
		}
	}

	if (*msg != NULL) {
		result = nvpi3_srv_delete_key(req->group_handle, req->key_name,
		                              req->type);
		itc_free(msg);
	}


	if (result == NVPI3_RESULT_SUCCESS) {
		struct nvpi3_delete_key_cfm *cfm =
			(struct nvpi3_delete_key_cfm *)
			itc_alloc(sizeof(struct nvpi3_delete_key_cfm),
			          NVPI3_DELETE_KEY_CFM);

		cfm->procedure_ref = procedure_ref;
		cfm->connection_ref = client_info->client_ref;
		TPT_SEND_SIG(cfm->msgno, sender, "NVPI3_DELETE_KEY_CFM");
		itc_send((union itc_msg **)&cfm, sender, ITC_MY_MBOX);
	} else {
		struct nvpi3_delete_key_rej *rej =
			(struct nvpi3_delete_key_rej *)
			itc_alloc(sizeof(struct nvpi3_delete_key_rej),
			          NVPI3_DELETE_KEY_REJ);

		rej->procedure_ref = procedure_ref;
		rej->connection_ref = client_info->client_ref;
		rej->error = result;
		TPT_SEND_SIG(rej->msgno, sender,
		             STR("NVPI3_DELETE_KEY_REJ %d",
		                 rej->error));
		itc_send((union itc_msg **)&rej, sender, ITC_MY_MBOX);
	}
}

static nvpi3_result_t enum_keys_callback(void *user_data,
                const char *database_name,
                const char *key_name,
                nvpi3_key_type_t type,
                const char *type_str, uint32_t size,
                const union nvpi3_key_value *value)
{
	int ret;
	uint32_t database_name_offset, key_name_offset, type_str_offset,
		ind_size;
	struct nvpi3_read_ind *ind;
	struct enum_keys_callback_data *ud = user_data;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	database_name_offset = offsetof(struct nvpi3_read_ind, u.key.value) +
	                       size;
	key_name_offset = database_name_offset + strlen(database_name) + 1;
	type_str_offset = key_name_offset + strlen(key_name) + 1;
	ind_size = type_str_offset + strlen(type_str) + 1;

	ind = (struct nvpi3_read_ind *) itc_alloc(ind_size, NVPI3_READ_IND);
	ind->procedure_ref = ud->procedure_ref;
	ind->connection_ref = ud->connection_ref;
	ind->database_name_offset = database_name_offset;
	strcpy((char *) ind + database_name_offset, database_name);
	ind->name_offset = key_name_offset;
	/*
	 * Perform a temporary concatenation of key_name and type_str for
	 * regexec operation.
	 */
	sprintf((char *) ind + key_name_offset, "%s%s", key_name, type_str);

	ret = regexec(&ud->regex, (char *) ind + key_name_offset, 0, NULL, 0);
	if (ret) {
		if(ret != REG_NOMATCH) {
			TPT_INFO(STR("regexec failed (%d)", ret));
			result = NVPI3_RESULT_OTHER_ERROR;
		}
		itc_free((union itc_msg **)&ind);
		goto enum_keys_callback_end;
	}

	*((char *) ind + type_str_offset - 1) = '\0';
	strcpy((char *) ind + type_str_offset, type_str);

	ind->type = NVPI3_READ_IND_KEY;
	ind->u.key.type_str_offset = type_str_offset;
	ind->u.key.type = type;
	ind->u.key.value_size = size;
	memcpy(&ind->u.key.value, value, size);
	TPT_SEND_SIG(ind->msgno, ud->sender,
	             STR("NVPI3_READ_IND database_name:%s, key_name:%s, "
	                 "type:%d, value_size:%d",
	                 (char *) ind + database_name_offset,
	                 (char *) ind + key_name_offset, ind->u.key.type,
	                 ind->u.key.value_size));
	itc_send((union itc_msg **)&ind, ud->sender, ITC_MY_MBOX);

enum_keys_callback_end:
	return result;
}

nvpi3_result_t enum_nodes_callback(void *user_data, const char *database_name,
                                   nvpi3_node_handle node_handle)
{
	int ret;
	uint32_t database_name_offset, node_name_offset, ind_size;
	struct nvpi3_read_ind *ind;
	const char *node_name;
	struct enum_keys_callback_data *ud = user_data;
	nvpi3_result_t result = nvpi3_srv_get_node_name(node_handle,
	                        &node_name);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto enum_nodes_callback_end;
	}

	ret = regexec(&ud->regex, node_name, 0, NULL, 0);
	if (ret) {
		if(ret != REG_NOMATCH) {
			TPT_INFO(STR("regexec failed (%d)", ret));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto enum_nodes_callback_end;
		}
		goto enum_nodes_callback1;
	}

	database_name_offset = offsetof(struct nvpi3_read_ind, u.node.strings);
	node_name_offset = database_name_offset + strlen(database_name) + 1;
	ind_size = node_name_offset + strlen(node_name) + 1;

	ind = (struct nvpi3_read_ind *) itc_alloc(ind_size, NVPI3_READ_IND);
	ind->procedure_ref = ud->procedure_ref;
	ind->connection_ref = ud->connection_ref;
	ind->database_name_offset = database_name_offset;
	strcpy((char *) ind + database_name_offset, database_name);
	ind->name_offset = node_name_offset;
	strcpy((char *) ind + node_name_offset, node_name);
	ind->type = NVPI3_READ_IND_NODE;
	TPT_SEND_SIG(ind->msgno, ud->sender,
	             STR("NVPI3_READ_IND database_name:%s, node_name:%s",
	                 (char *) ind + database_name_offset,
	                 (char *) ind + node_name_offset));
	itc_send((union itc_msg **)&ind, ud->sender, ITC_MY_MBOX);

enum_nodes_callback1:
	result = nvpi3_srv_enum_keys(node_handle,
	                             ud->flags & NVPI3_READ_REQ_FLAGS_ALL,
	                             enum_keys_callback, user_data);
enum_nodes_callback_end:
	return result;
}

static void handle_read_req(union itc_msg *msg,
                            struct conn_client_info *client_info,
                            itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	int ret;
	struct enum_keys_callback_data callback_data;
	union itc_msg *reply;
	struct nvpi3_read_req *req = &msg->nvpi3_read_req;

	callback_data.procedure_ref = req->procedure_ref;
	callback_data.connection_ref = client_info->client_ref;
	callback_data.sender = sender;
	callback_data.flags = req->flags;
	ret = regcomp(&callback_data.regex, req->pattern,
	              REG_EXTENDED | REG_NOSUB);
	if (ret) {
		TPT_INFO(STR("regcomp failed (%d)", ret));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto handle_read_req_error;
	}

	reply = itc_alloc(sizeof(struct nvpi3_read_cfm), NVPI3_READ_CFM);
	reply->nvpi3_read_cfm.procedure_ref = req->procedure_ref;
	reply->nvpi3_read_cfm.connection_ref = client_info->client_ref;
	TPT_SEND_SIG(reply->msgno, sender, "NVPI3_READ_CFM");
	itc_send(&reply, sender, ITC_MY_MBOX);

	result = nvpi3_srv_enum_nodes(
		req->group_handle, "/",
		callback_data.flags & NVPI3_READ_REQ_FLAGS_ALL,
		enum_nodes_callback, &callback_data);

	regfree(&callback_data.regex);
	reply = itc_alloc(sizeof(struct nvpi3_read_end_ind),
	                  NVPI3_READ_END_IND);
	reply->nvpi3_read_end_ind.procedure_ref = req->procedure_ref;
	reply->nvpi3_read_end_ind.connection_ref = client_info->client_ref;
	reply->nvpi3_read_end_ind.result = result;
	TPT_SEND_SIG(reply->msgno, sender,
	             STR("NVPI3_READ_END_IND %d",
	                 reply->nvpi3_read_end_ind.result));
	itc_send(&reply, sender, ITC_MY_MBOX);
	return;

handle_read_req_error:
	reply = itc_alloc(sizeof(struct nvpi3_read_rej), NVPI3_READ_REJ);
	reply->nvpi3_read_rej.procedure_ref = req->procedure_ref;
	reply->nvpi3_read_rej.connection_ref = client_info->client_ref;
	reply->nvpi3_read_rej.error = result;
	TPT_SEND_SIG(reply->msgno, sender,
	             STR("NVPI3_READ_REJ %d", reply->nvpi3_read_rej.error));
	itc_send(&reply, sender, ITC_MY_MBOX);
}

static nvpi3_result_t enum_db_group_callback(
	void *user_data, uint32_t num_of_groups,
	const char *name, uint32_t num_of_definitions,
	const struct nvpi3_db_definition definition[])
{
	struct nvpi3_list_db_groups_ind *ind;
	struct enum_db_group_callback_data *ud = user_data;

	ind = (struct nvpi3_list_db_groups_ind *)
		itc_alloc(offsetof(struct nvpi3_list_db_groups_ind,
		                   definition) +
		          num_of_definitions * sizeof(ind->definition[0]),
		          NVPI3_LIST_DB_GROUPS_IND);
	ind->procedure_ref = ud->procedure_ref;
	ind->connection_ref = ud->connection_ref;
	ind->num_of_groups = num_of_groups;
	strncpy(ind->name, name, sizeof(ind->name));
	ind->name[sizeof(ind->name)-1] = '\0';
	ind->num_of_definitions = num_of_definitions;
	memcpy(ind->definition, definition,
	       num_of_definitions * sizeof(ind->definition[0]));
	TPT_SEND_SIG(ind->msgno, ud->sender,
	             STR("NVPI3_LIST_DB_GROUPS_IND group_name:%s", ind->name));
	itc_send((union itc_msg **)&ind, ud->sender, ITC_MY_MBOX);
	return NVPI3_RESULT_SUCCESS;
}

static void handle_list_db_group_req(union itc_msg *msg,
                                     struct conn_client_info *client_info,
                                     itc_mbox_id_t sender)
{
	nvpi3_result_t result;
	struct enum_db_group_callback_data callback_data;
	union itc_msg *reply;
	struct nvpi3_list_db_groups_req *req = &msg->nvpi3_list_db_groups_req;

	callback_data.procedure_ref = req->procedure_ref;
	callback_data.connection_ref = client_info->client_ref;
	callback_data.sender = sender;

	reply = itc_alloc(sizeof(struct nvpi3_list_db_groups_cfm),
	                  NVPI3_LIST_DB_GROUPS_CFM);
	reply->nvpi3_list_db_groups_cfm.procedure_ref = req->procedure_ref;
	reply->nvpi3_list_db_groups_cfm.connection_ref =
		client_info->client_ref;
	TPT_SEND_SIG(reply->msgno, sender, "NVPI3_LIST_DB_GROUPS_CFM");
	itc_send(&reply, sender, ITC_MY_MBOX);

	result = nvpi3_srv_enum_db_groups(enum_db_group_callback,
	                                  &callback_data);

	reply = itc_alloc(sizeof(struct nvpi3_list_db_groups_end_ind),
	                  NVPI3_LIST_DB_GROUPS_END_IND);
	reply->nvpi3_list_db_groups_end_ind.procedure_ref = req->procedure_ref;
	reply->nvpi3_list_db_groups_end_ind.connection_ref =
		client_info->client_ref;
	reply->nvpi3_list_db_groups_end_ind.result = result;
	TPT_SEND_SIG(reply->msgno, sender,
	             STR("NVPI3_LIST_DB_GROUPS_END_IND %d",
	                 reply->nvpi3_list_db_groups_end_ind.result));
	itc_send(&reply, sender, ITC_MY_MBOX);
}

static void msg_loop(conn_server_handle_t conn_handle)
{
	struct conn_client_info client_info;
	itc_mbox_id_t sender;
	union itc_msg *msg;

	for (;;) {

		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		if (msg->msgno == EXIT_SIGNAL) {
			TPT_INFO(STR("%s exiting as ordered", DAEMON_NAME));
			itc_free(&msg);
			return;
		}

		/*
		 * Handles CONN_ESTABLISH... messages (and messages from unknown
		 * clients.
		 */
		if(!conn_check_client(conn_handle, &msg, &client_info))
			continue;

		sender = itc_sender(msg);

		switch (msg->msgno) {

		case NVPI3_CREATE_DB_GROUP_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_CREATE_DB_GROUP_REQ");
			handle_create_db_group(msg, &client_info, sender);
			break;

		case NVPI3_DESTROY_DB_GROUP_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_DESTROY_DB_GROUP_REQ");
			handle_destroy_db_group(msg, &client_info, sender);
			break;

		case NVPI3_OPEN_DB_GROUP_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_OPEN_DB_GROUP_REQ");
			handle_open_db_group(msg, &client_info, sender);
			break;

		case NVPI3_CLOSE_DB_GROUP_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_CLOSE_DB_GROUP_REQ");
			handle_close_db_group(msg, &client_info, sender);
			break;

		case NVPI3_OPEN_NODE_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_OPEN_REQ");
			handle_open_node(msg, &client_info, sender);
			break;

		case NVPI3_CLOSE_NODE_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_CLOSE_NODE_REQ");
			handle_close_node(msg, &client_info, sender);
			break;

		case NVPI3_GET_VALUE_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_GET_VALUE_REQ");
			handle_get_value(msg, &client_info, sender);
			break;

		case NVPI3_GET_VALUE_SIZE_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_GET_VALUE_SIZE_REQ");
			handle_get_value_size(msg, &client_info, sender);
			break;

		case NVPI3_CREATE_TRANSACTION_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_CREATE_TRANSACTION_REQ");
			handle_create_transaction(msg, &client_info, sender);
			break;

		case NVPI3_DESTROY_TRANSACTION_REQ:
			TPT_REC_SIG(msg->msgno,
			            "NVPI3_DESTROY_TRANSACTION_REQ");
			handle_destroy_transaction(msg, &client_info, sender);
			break;

		case NVPI3_COMMIT_TRANSACTION_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_COMMIT_TRANSACTION_REQ");
			handle_commit_transaction(msg, &client_info, sender);
			break;

		case NVPI3_DELETE_NODE_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_DELETE_NODE_REQ");
			handle_delete_node(&msg, &client_info, sender);
			/* The message stored or freed in handle_delete_node */
			continue;

		case NVPI3_SET_VALUE_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_SET_VALUE_REQ");
			handle_set_value(&msg, &client_info, sender);
			/* The message stored or freed in handle_set_value */
			continue;

		case NVPI3_DELETE_KEY_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_DELETE_KEY_REQ");
			handle_delete_key(&msg, &client_info, sender);
			/* The message stored or freed in handle_delete_key. */
			continue;

		case NVPI3_READ_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_READ_REQ");
			handle_read_req(msg, &client_info, sender);
			break;

		case NVPI3_LIST_DB_GROUPS_REQ:
			TPT_REC_SIG(msg->msgno, "NVPI3_LIST_DB_GROUP_REQ");
			handle_list_db_group_req(msg, &client_info, sender);
			break;

		default: {
			char name[64];
			itc_get_name(client_info.sender, name, sizeof(name));
			TPT_ERROR(STR("Received an unknown message "
			              "(msgno:0x%08x) from client "
			              "(name:\"%s\", mbox_id:0x%08x, "
			              "client_ref:0x%08x,"
			              "server_ref:0x%08x)",
			              msg->msgno, name,
			              client_info.sender,
			              client_info.client_ref,
			              client_info.server_ref));
		}

		}

		itc_free(&msg);

	}
}

static void destroy_client(struct client_entry *entry)
{
	struct client_entry *p;

	LIST_FOREACH(p, &client_head, list) {
		nvpi3_result_t result;
		nvpi3_transaction_handle transaction_handle;
		if (p != entry) {
			continue;
		}

		result = nvpi3_srv_destroy(entry);

		if (result != NVPI3_RESULT_SUCCESS &&
		    result != NVPI3_RESULT_NOT_FOUND) {
			TPT_ERROR(STR("nvpi3_srv_destroy failed (%d)", result));

		}

		while ((transaction_handle =
		        LIST_FIRST(&entry->transaction_head)) != NULL) {
			LIST_REMOVE(transaction_handle, list);
			free(transaction_handle);
		}

		LIST_REMOVE(entry, list);
		free(entry);
		return;
	}
	TPT_ERROR(STR("client %p not found", (void *)entry));
}

static uint32_t client_connect(struct conn_client_info *client_info)
{
	uint32_t result = UINT32_MAX;
	struct client_entry *entry = malloc(sizeof(*entry));

	if (entry != NULL) {
		result = conn_set_client_data(client_info->server_handle,
		                              client_info->server_ref,
		                              entry);

		if (result == CONN_ESTABLISH_CLIENT_DATA_OK) {
			LIST_INIT(&entry->transaction_head);
			LIST_INSERT_HEAD(&client_head, entry, list);
		} else {
			TPT_ERROR(STR("conn_set_client_data failed (%d)",
			              result));
			free(entry);
		}
	} else {
		TPT_ERROR(STR("malloc of size %d failed", sizeof(*entry)));

	}

	return result;
}

static void client_disconnect_or_died(struct conn_client_info *client_info)
{
	destroy_client(client_info->client_data);
}

static int conn_server_init(conn_server_handle_t *handle)
{
	NVPI3_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	int result;

	static const struct conn_event_callbacks cb = {
		/* client_connecting_cb */
		client_connect,
		/* client_disconnecting_cb */
		client_disconnect_or_died,
		/* client_died_cb */
		client_disconnect_or_died,
		/* dropped_msg_cb */
		NULL
	};

	static const uint32_t supported_versions[] = {NVPI3_SERVER_VERSIONS};

	result =
		conn_establish_server_init(handle,
		                           sizeof(supported_versions) /
		                           sizeof(supported_versions[0]),
		                           (uint32_t *) supported_versions,
		                           &conn_messages, 0,
		                           (struct conn_event_callbacks *) &cb);

	if(result != CONN_INIT_OK) {
		TPT_ERROR(STR("conn_establish_server_init failed (%d)",
		              result));
	}

	return result;
}

static int init_itc(void)
{
	int ret = itc_init(MAX_NUM_OF_MAILBOXES, ITC_MALLOC, NULL,
	                   ITC_NO_NAMESPACE, 0);
	if (ret) {
		TPT_ERROR(STR("itc_init failed (%d)", ret));
	} else {
		server_mbox = itc_create_mailbox(NVPI3_SERVER_NAME, 0);
		if (server_mbox == ITC_NO_ID) {
			TPT_ERROR(STR("itc_create_mailbox %s",
			          NVPI3_SERVER_NAME));
			ret = -1;
		}
	}

	return ret;
}

static int init(conn_server_handle_t *handle)
{
	int ret = init_itc();

	if (!ret) {
		if (conn_server_init(handle)) {
			ret = -1;
		}
	}

	return ret;
}


static void print_usage()
{
	printf("Usage: nvpi3d <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

static void exit_handler(__attribute__((__unused__)) int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send((union itc_msg **)&msg, server_mbox, ITC_MY_MBOX);
}

int main(int argc, char **argv)
{
	int daemonize = 0;
	int32_t ret = 0;

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		} else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		} else {
			print_usage();
			exit(-EINVAL);
		}
	}

	if (!daemonize || !daemon(0, 0)) {
		conn_server_handle_t conn_handle = NULL;
		/* Initialized to avoid coverity error. */
		TPT_INFO(STR("Starting %s %s",
		             daemonize ? "daemon" : "foreground process",
		             DAEMON_NAME));
		ret = init(&conn_handle);
		if (!ret) {
			/* Start processing ITC messages.
			 * No return.
			 */
			if (signal(SIGTERM, exit_handler) != SIG_ERR) {
				msg_loop(conn_handle);
			} else {
				TPT_ERROR("Failed to install signal exit "
				          "handler");
				ret = -EFAULT;
			}
		}
	} else {
		TPT_ERROR(STR("Failed to start daemon %s", DAEMON_NAME));
		ret = -EFAULT;
	}

	return ret;
}

/**
 * @copyright
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include "itc.h"
#include "nvpi3.h"
#include "nvpi3_msg.h"
#include "nvpi3_func.h"
#include "conn-establish-helper.h"
#include "log_tpt.h"

#define CONN_ESTABLISH_TMO  0  /* Infinite waiting for the answer of server */
#define NVPI3_MBOX          "NVPI3_CONN_ESTABLISH_CLIENT"



/* declarations for message sending */
union itc_msg {
	uint32_t msgno;
	NVPI3_MESSAGES;
};

static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} nvpi3_conn;


NVPI3_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);

static int initialized = 0;
static uint32_t client_ref = 0;

static pthread_mutex_t suspend_thread = PTHREAD_MUTEX_INITIALIZER;
static pthread_once_t once_control = PTHREAD_ONCE_INIT;
static pthread_mutex_t conn_complete_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t conn_complete;

static int nvpi3_init(void);
static int msg_check(union itc_msg **msg);

nvpi3_result_t nvpi3_open_db_group(const char *name,
                                   nvpi3_db_group_handle *group_handle)

{
	struct nvpi3_open_db_group_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_OPEN_DB_GROUP_CFM, NVPI3_OPEN_DB_GROUP_REJ};

	*group_handle = NULL;

	if (nvpi3_init()) {
		goto nvpi3_open_db_group_end;
	}

	req = (struct nvpi3_open_db_group_req *)
	      itc_alloc(offsetof(struct nvpi3_open_db_group_req, name) +
	                strlen(name) + 1, NVPI3_OPEN_DB_GROUP_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	strcpy(req->name, name);

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_OPEN_DB_GROUP_REQ name:%s",
	                 client_ref, req->name));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_open_db_group_end;
	}

	switch(reply->msgno) {
	case NVPI3_OPEN_DB_GROUP_CFM:
		result = NVPI3_RESULT_SUCCESS;
		*group_handle = reply->nvpi3_open_db_group_cfm.group_handle;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_OPEN_DB_GROUP_CFM"
		                " group_handle:%p",
		                client_ref, (void *)*group_handle));
		break;

	case NVPI3_OPEN_DB_GROUP_REJ:
		result = reply->nvpi3_open_db_group_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_OPEN_DB_GROUP_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_open_db_group_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}



nvpi3_result_t nvpi3_close_db_group(nvpi3_db_group_handle group_handle)
{
	struct nvpi3_close_db_group_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_CLOSE_DB_GROUP_CFM, NVPI3_CLOSE_DB_GROUP_REJ};

	if (nvpi3_init()) {
		goto nvpi3_close_db_group_end;
	}

	req = (struct nvpi3_close_db_group_req *)
	      itc_alloc(sizeof(struct nvpi3_close_db_group_req),
	                NVPI3_CLOSE_DB_GROUP_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->group_handle = group_handle;

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_CLOSE_DB_GROUP_REQ group_handle:%p",
	                 client_ref, (void *)req->group_handle));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_close_db_group_end;
	}

	switch(reply->msgno) {
	case NVPI3_CLOSE_DB_GROUP_CFM:
		result = NVPI3_RESULT_SUCCESS;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_CLOSE_DB_GROUP_CFM",
		                client_ref));
		break;

	case NVPI3_CLOSE_DB_GROUP_REJ:
		result = reply->nvpi3_close_db_group_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_CLOSE_DB_GROUP_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_close_db_group_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_open_node(nvpi3_db_group_handle group_handle,
                               const char *node_name,
                               nvpi3_node_handle *node_handle)
{
	struct nvpi3_open_node_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_OPEN_NODE_CFM, NVPI3_OPEN_NODE_REJ};

	*node_handle = NULL;

	if (nvpi3_init()) {
		goto nvpi3_open_node_end;
	}

	req = (struct nvpi3_open_node_req *)
	      itc_alloc(offsetof(struct nvpi3_open_node_req, node_name) +
	                strlen(node_name) + 1, NVPI3_OPEN_NODE_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->group_handle = group_handle;
	strcpy(req->node_name, node_name);

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_OPEN_NODE_REQ group_handle:%p,"
	                 " node_name;%s",
	                 client_ref, (void *)req->group_handle,
	                 req->node_name));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_open_node_end;
	}

	switch(reply->msgno) {
	case NVPI3_OPEN_NODE_CFM:
		result = NVPI3_RESULT_SUCCESS;
		*node_handle = reply->nvpi3_open_node_cfm.node_handle;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_OPEN_NODE_CFM"
		                " node_handle:%p",
		                client_ref, (void *)*node_handle));
		break;

	case NVPI3_OPEN_NODE_REJ:
		result = reply->nvpi3_open_node_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_OPEN_NODE_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_open_node_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_close_node(nvpi3_node_handle node_handle)
{
	struct nvpi3_close_node_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_CLOSE_NODE_CFM, NVPI3_CLOSE_NODE_REJ};

	if (nvpi3_init()) {
		goto nvpi3_close_node_end;
	}

	req = (struct nvpi3_close_node_req *)
	      itc_alloc(sizeof(struct nvpi3_close_node_req),
	                NVPI3_CLOSE_NODE_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->node_handle = node_handle;

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_CLOSE_NODE_REQ node_handle:%p",
	                 client_ref, (void *)req->node_handle));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_close_node_end;
	}

	switch(reply->msgno) {
	case NVPI3_CLOSE_NODE_CFM:
		result = NVPI3_RESULT_SUCCESS;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_CLOSE_NODE_CFM",
		                client_ref));
		break;

	case NVPI3_CLOSE_NODE_REJ:
		result = reply->nvpi3_close_node_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_CLOSE_NODE_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_close_node_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_get_value(nvpi3_node_handle node_handle,
                               const char *key_name, nvpi3_key_type_t type,
                               uint32_t *size, union nvpi3_key_value *value)
{
	struct nvpi3_get_value_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_GET_VALUE_CFM, NVPI3_GET_VALUE_REJ};

	if (nvpi3_init()) {
		goto nvpi3_get_value_end;
	}

	req = (struct nvpi3_get_value_req *)
	      itc_alloc(offsetof(struct nvpi3_get_value_req, key_name) +
	                strlen(key_name) + 1, NVPI3_GET_VALUE_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->node_handle = node_handle;
	req->type = type;
	strcpy(req->key_name, key_name);

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_GET_VALUE_REQ node_handle:%p, "
	                 "type:%d, key_name:%s",
	                 client_ref, (void *)req->node_handle,
	                 req->type, req->key_name));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_get_value_end;
	}

	switch(reply->msgno) {
	case NVPI3_GET_VALUE_CFM:
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_GET_VALUE_CFM",
		                client_ref));

		if (*size >= reply->nvpi3_get_value_cfm.value_size) {
			result = NVPI3_RESULT_SUCCESS;
			memcpy(value, &reply->nvpi3_get_value_cfm.value,
			       reply->nvpi3_get_value_cfm.value_size);
		} else {
			result = NVPI3_RESULT_BUFFER_TOO_SMALL;
			TPT_INFO(STR("Warning: Client:%d failure returned size %d bigger"
			             " than requested %d",
			             client_ref,
			             reply->nvpi3_get_value_cfm.value_size,
			             *size));
		}

		*size = reply->nvpi3_get_value_cfm.value_size;
		break;

	case NVPI3_GET_VALUE_REJ:
		result = reply->nvpi3_get_value_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_GET_VALUE_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_get_value_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_get_value_size(nvpi3_node_handle node_handle,
                                    const char *key_name,
                                    nvpi3_key_type_t type, uint32_t *size)
{
	struct nvpi3_get_value_size_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_GET_VALUE_SIZE_CFM, NVPI3_GET_VALUE_SIZE_REJ};

	*size = 0;

	if (nvpi3_init()) {
		goto nvpi3_get_value_size_end;
	}

	req = (struct nvpi3_get_value_size_req *)
	      itc_alloc(offsetof(struct nvpi3_get_value_size_req, key_name) +
	                strlen(key_name) + 1, NVPI3_GET_VALUE_SIZE_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->node_handle = node_handle;
	req->type = type;
	strcpy(req->key_name, key_name);

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_GET_VALUE_SIZE_REQ node_handle:%p, "
	                 "type:%d, key_name:%s",
	                 client_ref, (void *)req->node_handle,
	                 req->type, req->key_name));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_get_value_size_end;
	}

	switch(reply->msgno) {
	case NVPI3_GET_VALUE_SIZE_CFM:
		result = NVPI3_RESULT_SUCCESS;
		*size = (int)reply->nvpi3_get_value_size_cfm.value_size;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_GET_VALUE_SIZE_CFM size:%u",
		                client_ref, *size));
		break;

	case NVPI3_GET_VALUE_SIZE_REJ:
		result = reply->nvpi3_get_value_size_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_GET_VALUE_SIZE_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_get_value_size_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_delete_node(nvpi3_db_group_handle group_handle,
                                 nvpi3_transaction_handle transaction_handle,
                                 const char *node_name)
{
	struct nvpi3_delete_node_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_DELETE_NODE_CFM, NVPI3_DELETE_NODE_REJ};

	if (nvpi3_init()) {
		goto nvpi3_delete_node_end;
	}

	req = (struct nvpi3_delete_node_req *)
	      itc_alloc(offsetof(struct nvpi3_delete_node_req, node_name) +
	                strlen(node_name) + 1, NVPI3_DELETE_NODE_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->group_handle = group_handle;
	req->transaction_handle = transaction_handle;
	strcpy(req->node_name, node_name);

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_DELETE_NODE_REQ group_handle:%p,"
	                 " transaction_handle:%p, node_name;%s",
	                 client_ref,
	                 (void *)req->group_handle,
	                 (void *)req->transaction_handle,
	                 req->node_name));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_delete_node_end;
	}

	switch(reply->msgno) {
	case NVPI3_DELETE_NODE_CFM:
		result = NVPI3_RESULT_SUCCESS;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_DELETE_NODE_CFM",
		                client_ref));
		break;

	case NVPI3_DELETE_NODE_REJ:
		result = reply->nvpi3_delete_node_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_DELETE_NODE_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_delete_node_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_set_value(nvpi3_db_group_handle group_handle,
                               nvpi3_transaction_handle transaction_handle,
                               const char *key_name, nvpi3_key_type_t type,
                               uint32_t size,
                               const union nvpi3_key_value *value)
{
	struct nvpi3_set_value_req *req;
	uint32_t name_offset, req_size;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_SET_VALUE_CFM, NVPI3_SET_VALUE_REJ};

	if (nvpi3_init()) {
		goto nvpi3_set_value_end;
	}

	name_offset = offsetof(struct nvpi3_set_value_req, value) + size;
	req_size = name_offset + strlen(key_name) + 1;

	req = (struct nvpi3_set_value_req *)
	      itc_alloc(req_size, NVPI3_SET_VALUE_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->group_handle = group_handle;
	req->transaction_handle = transaction_handle;
	req->key_name_offset = name_offset;
	strcpy((char *) req + name_offset, key_name);
	req->type = type;
	req->value_size = size;
	memcpy(&req->value, value, size);

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_SET_VALUE_REQ group_handle:%p,"
	                 " transaction_handle:%p, key_name_offset:%u, type:%d, value_size:%u",
	                 client_ref, (void *)req->group_handle, (void *)req->transaction_handle,
	                 req->key_name_offset, req->type, req->value_size));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_set_value_end;
	}

	switch(reply->msgno) {
	case NVPI3_SET_VALUE_CFM:
		result = NVPI3_RESULT_SUCCESS;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_SET_VALUE_CFM",
		                client_ref));
		break;

	case NVPI3_SET_VALUE_REJ:
		result = reply->nvpi3_set_value_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_SET_VALUE_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_set_value_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_delete_key(nvpi3_db_group_handle group_handle,
                                nvpi3_transaction_handle transaction_handle,
                                const char *key_name, nvpi3_key_type_t type)
{
	struct nvpi3_delete_key_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_DELETE_KEY_CFM, NVPI3_DELETE_KEY_REJ};

	if (nvpi3_init()) {
		goto nvpi3_delete_key_end;
	}

	req = (struct nvpi3_delete_key_req *)
	      itc_alloc(offsetof(struct nvpi3_delete_key_req, key_name) +
	                strlen(key_name) + 1, NVPI3_DELETE_KEY_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->group_handle = group_handle;
	req->transaction_handle = transaction_handle;
	req->type = type;
	strcpy(req->key_name, key_name);

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_DELETE_KEY_REQ group_handle:%p,"
	                 " transaction_handle:%p, type:%d, key_name:%s", client_ref,
	                 (void *)req->group_handle, (void *)req->transaction_handle, req->type,
	                 req->key_name));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_delete_key_end;
	}

	switch(reply->msgno) {
	case NVPI3_DELETE_KEY_CFM:
		result = NVPI3_RESULT_SUCCESS;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_DELETE_KEY_CFM",
		                client_ref));
		break;

	case NVPI3_DELETE_KEY_REJ:
		result = reply->nvpi3_delete_key_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_DELETE_KEY_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_delete_key_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_create_transaction(
        nvpi3_transaction_handle *transaction_handle)
{
	struct nvpi3_create_transaction_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_CREATE_TRANSACTION_CFM, NVPI3_CREATE_TRANSACTION_REJ};

	*transaction_handle = NULL;

	if (nvpi3_init()) {
		goto nvpi3_create_transaction_end;
	}

	req = (struct nvpi3_create_transaction_req *)
	      itc_alloc(sizeof(struct nvpi3_create_transaction_req),
	                NVPI3_CREATE_TRANSACTION_REQ);
	req->connection_ref = nvpi3_conn.server_ref;

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_CREATE_TRANSACTION_REQ",
	                 client_ref));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_create_transaction_end;
	}

	switch(reply->msgno) {
	case NVPI3_CREATE_TRANSACTION_CFM:
		result = NVPI3_RESULT_SUCCESS;
		*transaction_handle =
		        reply->nvpi3_create_transaction_cfm.transaction_handle;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_CREATE_TRANSACTION_CFM"
		                " transaction_handle:%p",
		                client_ref,
		                (void *)*transaction_handle));
		break;

	case NVPI3_CREATE_TRANSACTION_REJ:
		result = reply->nvpi3_create_transaction_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_CREATE_TRANSACTION_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_create_transaction_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_destroy_transaction(
        nvpi3_transaction_handle transaction_handle)
{
	struct nvpi3_destroy_transaction_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] = {
		2,
		NVPI3_DESTROY_TRANSACTION_CFM,
		NVPI3_DESTROY_TRANSACTION_REJ
	};

	if (nvpi3_init()) {
		goto nvpi3_destroy_transaction_end;
	}

	req = (struct nvpi3_destroy_transaction_req *)
	      itc_alloc(sizeof(struct nvpi3_destroy_transaction_req),
	                NVPI3_DESTROY_TRANSACTION_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->transaction_handle = transaction_handle;

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_DESTROY_TRANSACTION_REQ"
	                 " transaction_handle:%p", client_ref,
	                 (void *)req->transaction_handle));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_destroy_transaction_end;
	}

	switch(reply->msgno) {
	case NVPI3_DESTROY_TRANSACTION_CFM:
		result = NVPI3_RESULT_SUCCESS;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_DESTROY_TRANSACTION_CFM",
		                client_ref));
		break;

	case NVPI3_DESTROY_TRANSACTION_REJ:
		result = reply->nvpi3_destroy_transaction_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_DESTROY_TRANSACTION_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_destroy_transaction_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

nvpi3_result_t nvpi3_commit_transaction(
        nvpi3_transaction_handle transaction_handle)
{
	struct nvpi3_commit_transaction_req *req;
	union itc_msg *reply = NULL;
	nvpi3_result_t result = NVPI3_RESULT_OTHER_ERROR;
	static const uint32_t filter[] =
		{2, NVPI3_COMMIT_TRANSACTION_CFM, NVPI3_COMMIT_TRANSACTION_REJ};

	if (nvpi3_init()) {
		goto nvpi3_commit_transaction_end;
	}

	req = (struct nvpi3_commit_transaction_req *)
	      itc_alloc(sizeof(struct nvpi3_commit_transaction_req),
	                NVPI3_COMMIT_TRANSACTION_REQ);
	req->connection_ref = nvpi3_conn.server_ref;
	req->transaction_handle = transaction_handle;

	TPT_SEND_SIG(req->msgno,
	             nvpi3_conn.server_mbox,
	             STR("Client:%d sending NVPI3_COMMIT_TRANSACTION_REQ"
	                 " transaction_handle:%p", client_ref,
	                 (void *)req->transaction_handle));

	itc_send((union itc_msg **)&req, nvpi3_conn.server_mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, nvpi3_conn.server_mbox);

	if (msg_check(&reply)) {
		goto nvpi3_commit_transaction_end;
	}

	switch(reply->msgno) {
	case NVPI3_COMMIT_TRANSACTION_CFM:
		result = NVPI3_RESULT_SUCCESS;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_COMMIT_TRANSACTION_CFM",
		                client_ref));
		break;

	case NVPI3_COMMIT_TRANSACTION_REJ:
		result = reply->nvpi3_commit_transaction_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client:%d received NVPI3_COMMIT_TRANSACTION_REJ %d",
		                client_ref, result));
		break;
	}

nvpi3_commit_transaction_end:
	if (reply != NULL) {
		itc_free(&reply);
	}

	return result;
}

static int32_t get_mbox(itc_mbox_id_t *nvpi3_mbox)
{
	client_ref = getpid();

	/* client mailbox */
	if (itc_current_mbox() == ITC_NO_ID) {
		TPT_ERROR(STR("Client:%d Mailbox doesn't exist", client_ref));
		return 1;
	}


	/* server mailbox */
	*nvpi3_mbox = itc_locate(NVPI3_SERVER_NAME);
	if (*nvpi3_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Client:%d nvpi3 server is not exist or mailbox %s "
		              "doesn't exist", client_ref, NVPI3_SERVER_NAME));
		return 1;
	}
	return 0;
}

static void *do_conn_establish(void *data)
{
	int ret;
	uint32_t requested_versions[] = {NVPI3_SERVER_VERSIONS};
	uint32_t procedure_ref = 0;
	itc_mbox_id_t mbox_id = ITC_NO_ID;

	mbox_id = itc_create_mailbox(NVPI3_MBOX, 0);
	if (mbox_id == ITC_NO_ID) {
		TPT_ERROR(STR("create_mailbox of %s failed", NVPI3_MBOX));
		goto do_conn_establish_error;
	}

	/* Find client and server mailboxes */
	if (get_mbox(&nvpi3_conn.server_mbox)) {
		goto do_conn_establish_error;
	}

	/*Connect to the server*/
	ret = conn_establish(/*input parameters*/
	              nvpi3_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) /
	              sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &nvpi3_conn.server_ref,
	              &nvpi3_conn.selected_version);
	if (ret != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Connection establish failed with reason:0x%x",
		              ret));
		goto do_conn_establish_error;
	}

	TPT_TRACE(1, STR("mailbox id for nvpi3: %u,"
	                 "server connection ref:%u,"
	                 "selected version: %u",
	                 nvpi3_conn.server_mbox,
	                 nvpi3_conn.server_ref,
	                 nvpi3_conn.selected_version));

	ret = pthread_mutex_lock(&conn_complete_lock);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d", ret));
		goto do_conn_establish_error;
	}

	initialized = 1;

	ret = pthread_cond_signal(&conn_complete);
	if (ret) {
		TPT_ERROR(STR("pthread_cond_signal return error %d", ret));
		(void) pthread_mutex_unlock(&conn_complete_lock);
		pthread_exit(NULL);
	}

	ret = pthread_mutex_unlock(&conn_complete_lock);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d", ret));
		pthread_exit(NULL);
	}

	/* Suspend thread. */
	ret = pthread_mutex_lock(&suspend_thread);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d", ret));
		pthread_exit(NULL);
	}

	TPT_ERROR("Thread was resumed unexpectedly, nvpi3 services will be"
	          " disabled");
	pthread_exit(NULL);

do_conn_establish_error:
	if (mbox_id != ITC_NO_ID) {
		itc_delete_mailbox(mbox_id);
	}

	(void) pthread_cond_signal(&conn_complete);
	pthread_exit(NULL);
}

static void init_routine(void)
{
	pthread_attr_t attr;
	pthread_t thread;
	int ret;
	pthread_condattr_t cattr;
	int locked = 0;

	ret = pthread_condattr_init(&cattr);
	if (ret) {
		TPT_ERROR(STR("pthread_condattr_init failed with error %d", ret));
		return;
	}

	ret = pthread_cond_init(&conn_complete, &cattr);
	if (ret) {
		TPT_ERROR(STR("pthread_cond_init failed with error %d", ret));
		return;
	}

	ret = pthread_attr_init(&attr);
	if (ret) {
		TPT_ERROR(STR("pthread_attr_init failed with error %d", ret));
		return;
	}

	ret = pthread_mutex_lock(&suspend_thread);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d", ret));
		return;
	}

	ret = pthread_mutex_lock(&conn_complete_lock);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d", ret));
		goto init_routine_error;
	}

	locked = 1;

	ret = pthread_create(&thread, &attr, do_conn_establish, NULL);
	if (!ret) {
		ret = pthread_cond_wait(&conn_complete, &conn_complete_lock);
		if (!ret) {
			goto init_routine_end;
		}
		TPT_ERROR(STR("pthread_cond_wait failed with error %d", ret));
	} else {
		TPT_ERROR(STR("pthread_create failed with error %d", ret));
	}

init_routine_error:
	ret = pthread_mutex_unlock(&suspend_thread);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d", ret));
	}

init_routine_end:
	if (locked) {
		ret = pthread_mutex_unlock(&conn_complete_lock);
		if (ret) {
			TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
			              ret));
		}
	}
}

static int nvpi3_init(void)
{
	pthread_once(&once_control, init_routine);

	if (!initialized) {
		TPT_ERROR(STR("Client:%d failed to initialized nvpi3", client_ref));
		return -1;
	} else {
		return 0;
	}
}

static int msg_check(union itc_msg **msg)
{
	if (((*msg)->any_msg.connection_ref != client_ref)) {
		TPT_ERROR(STR("Client:%d invalid ref: expected 0x%08x, got 0x%08x",
		              client_ref, client_ref, (*msg)->any_msg.connection_ref));
		itc_free(msg);
		return -1;
	}
	return 0;
}

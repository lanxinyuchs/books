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
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include "itc.h"
#include "nvpi3.h"
#include "nvpi3_msg.h"
#include "nvpi3_func.h"
#include "conn-establish-helper.h"
#include "log_tpt.h"

#define CONN_ESTABLISH_TMO     0  /* Infinite waiting for the answer of server */
#define NVPI3_CFG_MBOX         "NVPI3_CFG_CONN_ESTABLISH_CLIENT"
#define NVPI3_CFG_COMMIT_MBOX  "NVPI3_CFG_COMMIT_CALLBACK_"

/* declarations for message sending */
union itc_msg {
	uint32_t msgno;
	NVPI3_MESSAGES;
};

static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} nvpi3_cfg_conn;

NVPI3_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);

static int initialized = 0;
static uint32_t client_ref = 0;

static pthread_mutex_t suspend_thread = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t conn_complete_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t conn_complete;
static pthread_once_t once_control = PTHREAD_ONCE_INIT;
static pthread_mutex_t commit_created_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t commit_created;

static char client_mbox_name[32];

static int create_commit_callback(itc_mbox_id_t *mbox_id);
static int nvpi3_cfg_init(void);
static int msg_check(union itc_msg **msg);


nvpi3_result_t nvpi3_create_db_group(
	const char *name,
	uint32_t num_of_definitions,
	const struct nvpi3_db_definition definition[],
	nvpi3_db_group_handle *group_handle)
{
	int32_t ret = 0;
	struct nvpi3_create_db_group_req *req;
	union itc_msg *reply;
	itc_mbox_id_t commit_mboxid = ITC_NO_ID;
	static const uint32_t filter[] =
		{2, NVPI3_CREATE_DB_GROUP_CFM, NVPI3_CREATE_DB_GROUP_REJ};

	if (nvpi3_cfg_init()) {
		return NVPI3_RESULT_OTHER_ERROR;
	}

	if (create_commit_callback(&commit_mboxid)) {
		return NVPI3_RESULT_OTHER_ERROR;
	}
	if (!num_of_definitions) {
		TPT_ERROR(STR("Client %s(%d): number of definitions %d is "
		              "invalid", client_mbox_name, client_ref,
		              num_of_definitions));
		return  NVPI3_RESULT_INVALID_PARAM;
	}
	req = (struct nvpi3_create_db_group_req *)
	      itc_alloc(offsetof(struct nvpi3_create_db_group_req,
	                         definition) +
	                num_of_definitions *
	                sizeof(struct nvpi3_db_definition),
	                NVPI3_CREATE_DB_GROUP_REQ);

	req->connection_ref = nvpi3_cfg_conn.server_ref;

	if (strlen(name) >= sizeof(req->name)) {
		TPT_ERROR(STR("Client %s(%d): database group name length %d is "
		              "longer than %d", client_mbox_name, client_ref,
		              strlen(name), sizeof(req->name)));
		itc_free((union itc_msg **)&req);
		return NVPI3_RESULT_INVALID_PARAM;
	}

	strcpy(req->name, name);

	req->num_of_definitions =
	        num_of_definitions;
	memcpy(req->definition, definition,
	       num_of_definitions * sizeof(struct nvpi3_db_definition));

	req->commit_db_mbox_id = commit_mboxid;

	TPT_SEND_SIG(req->msgno,
	             nvpi3_cfg_conn.server_mbox,
	             STR("Client %s(%d): NVPI3_CREATE_DB_GROUP_REQ,"
	                 " number_of_definitions:%u, destination: %s(%u)",
	                 client_mbox_name, client_ref, num_of_definitions,
	                 NVPI3_SERVER_NAME, nvpi3_cfg_conn.server_mbox));

	itc_send((union itc_msg **)&req, nvpi3_cfg_conn.server_mbox,
	         ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO,
	                    nvpi3_cfg_conn.server_mbox);

	if (msg_check(&reply)) {
		return NVPI3_RESULT_OTHER_ERROR;
	}

	switch(reply->msgno) {
	case NVPI3_CREATE_DB_GROUP_CFM:
		*group_handle = reply->nvpi3_create_db_group_cfm.group_handle;
		TPT_REC_SIG(reply->msgno,
		            STR("Client %s(%d): receive "
		                "NVPI3_CREATE_DB_GROUP_CFM(handle:%p)"
		                " from %s(%u)", client_mbox_name, client_ref,
		                (void *)reply->nvpi3_create_db_group_cfm.
		                group_handle,
		                NVPI3_SERVER_NAME, nvpi3_cfg_conn.server_mbox));

		ret = NVPI3_RESULT_SUCCESS;

		break;
	case NVPI3_CREATE_DB_GROUP_REJ:
		ret = reply->nvpi3_create_db_group_rej.error;

		TPT_REC_SIG(reply->msgno,
		            STR("Client %s(%d): receive "
		                "NVPI3_CREATE_DB_GROUP_REJ(Error code: %d)"
		                " from %s(%u)", client_mbox_name, client_ref,
		                reply->nvpi3_create_db_group_rej.error,
		                NVPI3_SERVER_NAME, nvpi3_cfg_conn.server_mbox));
		break;
	default:
		TPT_ERROR(STR("Client %s(%d): Unexpected signal 0x%x is "
		              "received", client_mbox_name, client_ref,
		              reply->msgno));
		ret = NVPI3_RESULT_OTHER_ERROR;
		break;

	}

	itc_free(&reply);

	return ret;
}

nvpi3_result_t nvpi3_destroy_db_group(nvpi3_db_group_handle group_handle)
{
	int32_t ret = 0;
	struct nvpi3_destroy_db_group_req *req;
	union itc_msg *reply;
	static const uint32_t filter[] =
		{2, NVPI3_DESTROY_DB_GROUP_CFM, NVPI3_DESTROY_DB_GROUP_REJ};


	if (nvpi3_cfg_init()) {
		return NVPI3_RESULT_OTHER_ERROR;
	}

	req = (struct nvpi3_destroy_db_group_req *)
	      itc_alloc(sizeof(struct nvpi3_destroy_db_group_req),
	                NVPI3_DESTROY_DB_GROUP_REQ);

	req->group_handle = group_handle;
	req->connection_ref = nvpi3_cfg_conn.server_ref;

	TPT_SEND_SIG(req->msgno,
	             nvpi3_cfg_conn.server_mbox,
	             STR("Client %s(%d): NVPI3_DESTROY_DB_GROUP_REQ, "
	                 "group_handle:%u, destination: %s(%u)",
	                 client_mbox_name, client_ref, (uint32_t)group_handle,
	                 NVPI3_SERVER_NAME, nvpi3_cfg_conn.server_mbox));

	itc_send((union itc_msg **)&req, nvpi3_cfg_conn.server_mbox,
	         ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO,
	                    nvpi3_cfg_conn.server_mbox);

	if (msg_check(&reply)) {
		return NVPI3_RESULT_OTHER_ERROR;
	}

	switch(reply->msgno) {
	case NVPI3_DESTROY_DB_GROUP_CFM:
		TPT_REC_SIG(reply->msgno,
		            STR("Client %s(%d): receive "
		                "NVPI3_DESTROY_DB_GROUP_CFM from %s(%u)",
		                client_mbox_name, client_ref, NVPI3_SERVER_NAME,
		                nvpi3_cfg_conn.server_mbox));
		ret = NVPI3_RESULT_SUCCESS;
		break;
	case NVPI3_DESTROY_DB_GROUP_REJ:
		ret = reply->nvpi3_destroy_db_group_rej.error;
		TPT_REC_SIG(reply->msgno,
		            STR("Client %s(%d): receive "
		                "NVPI3_DESTROY_DB_GROUP_REJ(Error code: %d) "
		                "from:%s(%u)", client_mbox_name, client_ref,
		                reply->nvpi3_destroy_db_group_rej.error,
		                NVPI3_SERVER_NAME, nvpi3_cfg_conn.server_mbox));
		break;
	default:
		TPT_ERROR(STR("Client %s(%d): Unexpected signal 0x%x is "
		              "received", client_mbox_name, client_ref,
		              reply->msgno));
		ret = NVPI3_RESULT_OTHER_ERROR;
		break;

	}
	itc_free(&reply);

	return ret;
}

static void *commit_callback(void *data)
{
	union itc_msg *rec, *reply;
	struct nvpi3_commit_db_rreq commit_req;
	itc_mbox_id_t *mbox_id = (itc_mbox_id_t *) data;
	uint32_t ret;
	char mbox_name[sizeof(NVPI3_CFG_COMMIT_MBOX) +
		       sizeof("4294967295") - 1];
	static uint32_t counter = 0;

	*mbox_id = ITC_NO_ID;

	ret = pthread_mutex_lock(&commit_created_lock);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_lock failed with "
		              "error %d", client_mbox_name, client_ref, ret));
		goto commit_callback_err;
	}

	sprintf(mbox_name, NVPI3_CFG_COMMIT_MBOX "%" PRIu32, counter);
	counter++;
	*mbox_id = itc_create_mailbox(mbox_name, 0);
	if (*mbox_id == ITC_NO_ID) {
		ret = -1;
		TPT_ERROR(STR("Client %s(%d): create_mailbox of %s failed",
		              client_mbox_name, client_ref, mbox_name));
		goto commit_callback_err;
	}

	ret = pthread_cond_signal(&commit_created);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_cond_signal return "
		              "error %d", client_mbox_name, client_ref, ret));
		(void) pthread_mutex_unlock(&commit_created_lock);
		goto commit_callback_err;
	}

	ret = pthread_mutex_unlock(&commit_created_lock);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_unlock failed with "
		              "error %d", client_mbox_name, client_ref, ret));
		goto commit_callback_err;
	}

	for(;;) {
		rec = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch(rec->msgno) {
		case NVPI3_COMMIT_DB_RREQ:
			commit_req = rec->nvpi3_commit_db_rreq;
			ret = commit_req.callback(commit_req.user_data,
			                          commit_req.size,
			                          &commit_req.storage);
			reply = itc_alloc(sizeof(struct nvpi3_commit_db_rrej),
			                  NVPI3_COMMIT_DB_RREJ);
			if (!ret) {
				reply->nvpi3_commit_db_rcfm.msgno =
				        NVPI3_COMMIT_DB_RCFM;
				itc_send(&reply, itc_sender(rec),
				         ITC_MY_MBOX);

			} else {
				itc_send(&reply, itc_sender(rec),
				         ITC_MY_MBOX);
			}

			break;
		default:
			break;
		}

		itc_free(&rec);

	}

commit_callback_err:
	if (ret && *mbox_id != ITC_NO_ID) {
		itc_delete_mailbox(*mbox_id);
	}
	pthread_exit(NULL);
}


static int create_commit_callback(itc_mbox_id_t *mbox_id)
{
	pthread_attr_t attr;
	pthread_t thread;
	int locked = 0;
	int ret1;

	ret1 = pthread_attr_init(&attr);
	if (ret1) {
		TPT_ERROR(STR("Client %s(%d): pthread_attr_init failed with "
		              "error %d", client_mbox_name, client_ref, ret1));
		goto create_commit_callback_end;
	}

	ret1 = pthread_mutex_lock(&commit_created_lock);
	if (ret1) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_lock failed with "
		              "error %d", client_mbox_name, client_ref, ret1));
		goto create_commit_callback_end;
	}

	locked = 1;

	ret1 = pthread_create(&thread, &attr, commit_callback, mbox_id);
	if (!ret1) {
		ret1 = pthread_cond_wait(&commit_created, &commit_created_lock);
		if (ret1) {
			TPT_ERROR(STR("Client %s(%d): pthread_cond_wait failed "
			              "with error %d", client_mbox_name,
			              client_ref, ret1));
			goto create_commit_callback_end;
		}
	} else {
		TPT_ERROR(STR("Client %s(%d): pthread_create failed with "
		              "error %d", client_mbox_name, client_ref, ret1));
		goto create_commit_callback_end;
	}

	if (*mbox_id == ITC_NO_ID) {
		ret1 = -1;
	}

create_commit_callback_end:
	if (locked) {
		int ret2 = pthread_mutex_unlock(&commit_created_lock);
		if (ret2) {
			TPT_ERROR(STR("Client %s(%d): pthread_mutex_unlock "
			              "failed with error %d", client_mbox_name,
			              client_ref, ret2));
		}
		if (!ret1) {
			ret1 = ret2;
		}
	}
	return ret1;
}

static int32_t get_mbox(itc_mbox_id_t *nvpi3_cfg_mbox)
{
	union itc_msg *msg;
	uint32_t resp_msg[] = {1, ITC_LOCATE_DEFAULT_NO};

	/* client mailbox */
	if(itc_current_mbox() == ITC_NO_ID) {
		TPT_ERROR(STR("Client %s(%d): Mailbox of connection"
		              " establishment doesn't exist.", client_mbox_name,
		              client_ref));
		return 1;
	}

	/* server mailbox */
	TPT_TRACE(7, STR("Client %s(%d): locating %s", client_mbox_name,
	                 client_ref, NVPI3_SERVER_NAME));

	itc_locate_async(NVPI3_SERVER_NAME, NULL, ITC_MY_MBOX);

	msg = itc_receive(resp_msg, ITC_NO_TMO, ITC_FROM_ALL);

	*nvpi3_cfg_mbox = itc_sender(msg);

	TPT_TRACE(7, STR("Client %s(%d): %s found", client_mbox_name,
	                 client_ref, NVPI3_SERVER_NAME));

	itc_free(&msg);
	return 0;
}

static void *do_conn_establish(void *data)
{
	int ret;
	uint32_t requested_versions[] = {NVPI3_SERVER_VERSIONS};
	uint32_t procedure_ref = 0;
	itc_mbox_id_t mbox_id = ITC_NO_ID;

	mbox_id = itc_create_mailbox(NVPI3_CFG_MBOX, 0);
	if (mbox_id == ITC_NO_ID) {
		TPT_ERROR(STR("Client %s(%d): create connection establishment "
		              "mailbox (%s) failed", client_mbox_name,
		              client_ref, NVPI3_CFG_MBOX));
		goto do_conn_establish_error;
	}

	/* Find client and server mailboxes */
	if (get_mbox(&nvpi3_cfg_conn.server_mbox)) {
		goto do_conn_establish_error;
	}

	/*Connect to the server*/
	ret = conn_establish(/*input parameters*/
	              nvpi3_cfg_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) /
	              sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &nvpi3_cfg_conn.server_ref,
	              &nvpi3_cfg_conn.selected_version);
	if (ret != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client %s(%d): Connection establish failed with "
		              "reason:0x%x", client_mbox_name, client_ref,
		              ret));
		goto do_conn_establish_error;
	}

	TPT_TRACE(1, STR("Client %s(%d): server mailbox id for nvpi3_cfg: %u,"
	                 "server connection ref:%u,"
	                 "selected version: %u",
	                 client_mbox_name,
	                 client_ref,
	                 nvpi3_cfg_conn.server_mbox,
	                 nvpi3_cfg_conn.server_ref,
	                 nvpi3_cfg_conn.selected_version));

	ret = pthread_mutex_lock(&conn_complete_lock);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_lock failed with "
		              "error %d", client_mbox_name, client_ref, ret));
		goto do_conn_establish_error;
	}

	initialized = 1;

	ret = pthread_cond_signal(&conn_complete);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_cond_signal return "
		              "error %d", client_mbox_name, client_ref, ret));
		(void) pthread_mutex_unlock(&conn_complete_lock);
		pthread_exit(NULL);
	}

	ret = pthread_mutex_unlock(&conn_complete_lock);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_unlock failed with "
		              "error %d", client_mbox_name, client_ref, ret));
		pthread_exit(NULL);
	}

	/* Suspend thread. */
	ret = pthread_mutex_lock(&suspend_thread);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_lock failed with "
		              "error %d", client_mbox_name, client_ref, ret));
		pthread_exit(NULL);
	}

	TPT_ERROR(STR("Client %s(%d): Thread was resumed unexpectedly, "
	              "nvpi3 services will be disabled", client_mbox_name,
	              client_ref));
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
	itc_mbox_id_t main_thread_client_mbox;

	client_ref = getpid();

	main_thread_client_mbox = itc_current_mbox();
	if(main_thread_client_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Client %d: Mailbox doesn't exist.", client_ref));
		return;
	}

	itc_get_name(main_thread_client_mbox,
	             client_mbox_name,
	             sizeof(client_mbox_name));

	ret = pthread_condattr_init(&cattr);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_condattr_init failed with"
		              " error %d", client_mbox_name, client_ref, ret));
		return;
	}

	ret = pthread_cond_init(&conn_complete, &cattr);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_cond_init failed with "
		              "error %d", client_mbox_name, client_ref, ret));
		return;
	}

	ret = pthread_attr_init(&attr);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_attr_init failed with "
		              "error %d", client_mbox_name, client_ref, ret));
		return;
	}

	ret = pthread_mutex_lock(&suspend_thread);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_lock failed with"
		              " error %d", client_mbox_name, client_ref, ret));
		return;
	}

	ret = pthread_mutex_lock(&conn_complete_lock);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_lock failed with"
		              "error %d", client_mbox_name, client_ref, ret));
		goto init_routine_error;
	}

	locked = 1;

	ret = pthread_create(&thread, &attr, do_conn_establish, NULL);
	if (!ret) {
		ret = pthread_cond_wait(&conn_complete, &conn_complete_lock);
		if (!ret) {
			goto init_routine_end;
		}
		TPT_ERROR(STR("Client %s(%d): pthread_cond_wait failed with"
		              " error %d", client_mbox_name, client_ref, ret));
	} else {
		TPT_ERROR(STR("Client %s(%d): pthread_create failed with "
		              "error %d", client_mbox_name, client_ref, ret));
	}

init_routine_error:
	ret = pthread_mutex_unlock(&suspend_thread);
	if (ret) {
		TPT_ERROR(STR("Client %s(%d): pthread_mutex_unlock failed with"
		              " error %d", client_mbox_name, client_ref, ret));
	}

init_routine_end:
	if (locked) {
		ret = pthread_mutex_unlock(&conn_complete_lock);
		if (ret) {
			TPT_ERROR(STR("Client %s(%d): pthread_mutex_unlock "
			              "failed with error %d",
			              client_mbox_name, client_ref, ret));
		}
	}
}

static int nvpi3_cfg_init(void)
{
	pthread_once(&once_control, init_routine);

	if (!initialized) {
		TPT_ERROR(STR("Client %s(%d): failed to initialized nvpi3",
		              client_mbox_name, client_ref));
		return -1;
	} else {
		return 0;
	}
}

static int msg_check(union itc_msg **msg)
{
	if (((*msg)->any_msg.connection_ref != client_ref)) {
		TPT_ERROR(STR("Client%s(%d): invalid ref: expected 0x%08x,"
		              " got 0x%08x", client_mbox_name, client_ref,
		              client_ref, (*msg)->any_msg.connection_ref));
		itc_free(msg);
		return -1;
	}
	return 0;
}

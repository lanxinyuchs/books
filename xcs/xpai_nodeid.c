/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <pthread.h>
#include <ctype.h>
#include "itc.h"
#include "common.h"
#include "log_tpt.h"
#include "conn-establish-helper.h"

#include "xpai_xhl_if.h"
#include "nodeid_server.h"

#define CONN_ESTABLISH_TMO  0  /* Infinite waiting for the answer of server */
#define NODEID_CLIENT_MBOX          "NODE_ID_CLIENT"

static struct server_info nodeid_conn;
static bool initialized = false;

static uint32_t client_ref = 1;

static pthread_once_t once_control = PTHREAD_ONCE_INIT;
static pthread_cond_t conn_complete;
static pthread_mutex_t conn_complete_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t suspend_thread = PTHREAD_MUTEX_INITIALIZER;

union itc_msg {
	uint32_t msgno;
	NODEID_STRUCTS
};


static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} nodeid_conn;

static int msg_check(union itc_msg **msg)
{
	if (((*msg)->any_msg.connection_ref != client_ref)) {
		TPT_ERROR(STR("Client:%d invalid ref: expected 0x%08x, "
		              "got 0x%08x", client_ref, client_ref,
		              (*msg)->any_msg.connection_ref));
		itc_free(msg);
		return -1;
	}
	return 0;
}

static int32_t convert_err_code(uint32_t nodeid_err_code, uint32_t is_read)
{
	switch (nodeid_err_code) {
		case NODEID_RESULT_SUCCESS:
			return is_read ? XPAI_READ_NODE_ID_OK :
			                XPAI_WRITE_NODE_ID_OK;
		case NODEID_RESULT_INVALID_PARAM:
			return is_read ? XPAI_READ_NODE_ID_NOK_PARAM :
			                XPAI_WRITE_NODE_ID_NOK_PARAM;
		case NODEID_RESULT_NOT_FOUND:
			return is_read ? XPAI_READ_NODE_ID_NOK_NOT_READABLE :
			                XPAI_WRITE_NODE_ID_NOK_NOT_WRITEABLE;
		case NODEID_RESULT_RESOURCE_SHORTAGE:
			return is_read ? XPAI_READ_NODE_ID_NOK_NOT_READABLE :
			                XPAI_WRITE_NODE_ID_NOK_NOT_WRITEABLE;
		case NODEID_RESULT_ACCESS_DENIED:
			return is_read ? XPAI_READ_NODE_ID_NOK_NOT_READABLE :
			                XPAI_WRITE_NODE_ID_NOK_NOT_WRITEABLE;
		case NODEID_RESULT_OTHER_ERROR:
			return is_read ? XPAI_READ_NODE_ID_NOK_OTHER :
			                XPAI_WRITE_NODE_ID_NOK_OTHER;
		default:
			return is_read ? XPAI_READ_NODE_ID_NOK_OTHER :
			                XPAI_WRITE_NODE_ID_NOK_OTHER;
	}
}

static void *do_conn_establish(__attribute__((unused))void *data)
{
	int ret;
	uint32_t requested_versions[] = {NODEID_SERVER_VERSIONS};
	uint32_t procedure_ref = 0;
	itc_mbox_id_t mbox_id = ITC_NO_ID;
	NODEID_CONN_ESTABLISH_MSG_STRUCT(conn_messages);

	mbox_id = itc_create_mailbox(NODEID_CLIENT_MBOX, 0);
	if (mbox_id == ITC_NO_ID) {
		TPT_ERROR(STR("create_mailbox of %s failed",
		              NODEID_CLIENT_MBOX));
		goto do_conn_establish_error;
	}

	/* Find client and server mailboxes */
	if (xpai_locate_mbox(NODEID_SERVER_NAME, &nodeid_conn.server_mbox) !=
	    INIT_OK) {
		goto do_conn_establish_error;
	}

	/*Connect to the server*/
	ret = conn_establish(/*input parameters*/
	              nodeid_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) /
	              sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &nodeid_conn.server_ref,
	              &nodeid_conn.selected_version);
	if (ret != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Connection establish failed with reason:0x%x",
		              ret));
		goto do_conn_establish_error;
	}

	TPT_TRACE(1, STR("mailbox id for nodeid: %u,"
	                 "server connection ref:%u,"
	                 "selected version: %u",
	                 nodeid_conn.server_mbox,
	                 nodeid_conn.server_ref,
	                 nodeid_conn.selected_version));

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
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              ret));
		pthread_exit(NULL);
	}

	/* Suspend thread. */
	ret = pthread_mutex_lock(&suspend_thread);
	if (ret) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d", ret));
		pthread_exit(NULL);
	}

	TPT_ERROR("Thread was resumed unexpectedly, nodeid service will be"
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
		TPT_ERROR(STR("pthread_condattr_init failed with error %d",
		              ret));
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
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              ret));
	}

init_routine_end:
	if (locked) {
		ret = pthread_mutex_unlock(&conn_complete_lock);
		if (ret) {
			TPT_ERROR(STR("pthread_mutex_unlock "
			              "failed with error %d", ret));
		}
	}
}


static int nodeid_init(void)
{
	pthread_once(&once_control, init_routine);

	if (!initialized) {
		TPT_ERROR(STR("Client:%d failed to "
		              "initialized nodeid", client_ref));
		return INIT_OTHER_ERROR;
	} else {
		return INIT_OK;
	}
}

static int32_t readnodeid(uint32_t *length, uint8_t *buffer)
{
	union itc_msg *msg_p = NULL;
	int32_t result = XPAI_READ_NODE_ID_OK;

	static uint32_t rx_filter[] = {2,
	                               NODE_ID_READ_CFM,
	                               NODE_ID_READ_REJ
	                               };

	TPT_TRACE(1, STR("readnodeid (length=%p buffer=%p)",
	                 (void *)length, (void *)buffer));

	if (nodeid_init()) {
		TPT_ERROR("Connection to server is not started");
		return XPAI_READ_NODE_ID_NOK_SERVER;
	}

	/* Send request. */
	msg_p = itc_alloc(sizeof(struct nodeid_read_req), NODE_ID_READ_REQ);
	msg_p->nodeid_read_req.connection_ref = nodeid_conn.server_ref;
	TPT_SEND_SIG(msg_p->msgno, nodeid_conn.server_mbox,
	             "NODE_ID_READ_REQ");
	itc_send(&msg_p, nodeid_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg_p = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);

	if (msg_check(&msg_p)) {
		result = XPAI_READ_NODE_ID_NOK_OTHER;
		goto ret_err;
	}

	switch (msg_p->msgno) {
		case NODE_ID_READ_CFM:
			TPT_REC_SIG(msg_p->msgno, "NODE_ID_READ_CFM\n");
			if (msg_p->nodeid_read_cfm.length >
			    XPAI_WRITE_NODE_ID_MAX_LENGTH) {
				TPT_INFO(STR("Node id length %d exceeds max "
				    "length %d", msg_p->nodeid_read_cfm.length,
				    XPAI_WRITE_NODE_ID_MAX_LENGTH));
				result = XPAI_READ_NODE_ID_NOK_NOT_READABLE;
				break;
			}
			*length = msg_p->nodeid_read_cfm.length;
			memcpy(buffer, msg_p->nodeid_read_cfm.node_id, *length);
			break;

		case NODE_ID_READ_REJ:
			TPT_REC_SIG(msg_p->msgno,
			            STR("NODE_ID_READ_REJ: error code=%d",
			            msg_p->nodeid_read_rej.error_code));
			result = convert_err_code(
			                msg_p->nodeid_read_rej.error_code, 1);
			break;
	}
	itc_free(&msg_p);

ret_err:
	return result;
}

static int32_t writenodeid(uint32_t length, U8 *buffer)
{
	union itc_msg *msg_p = NULL;
	int32_t result = XPAI_WRITE_NODE_ID_OK;

	static uint32_t rx_filter[] = {2,
		                       NODE_ID_WRITE_CFM,
		                       NODE_ID_WRITE_REJ
		                       };

	TPT_TRACE(1, STR("writenodeid (length=%u buffer=%p)",
	          length, buffer));

	if (nodeid_init()) {
		TPT_ERROR("Connection to server is not started");
		return XPAI_WRITE_NODE_ID_NOK_SERVER;
	} else if ((length == 0) || (length > XPAI_WRITE_NODE_ID_MAX_LENGTH)) {
		TPT_ERROR(STR("Length error, length=%d", length));
		return XPAI_WRITE_NODE_ID_NOK_PARAM;
	}

	/* Send request. */
	msg_p = itc_alloc(offsetof(struct nodeid_write_req, node_id)
	                   + length, NODE_ID_WRITE_REQ);
	msg_p->nodeid_write_req.connection_ref = nodeid_conn.server_ref;
	msg_p->nodeid_write_req.length = length;
	memcpy(msg_p->nodeid_write_req.node_id, buffer, length);

	TPT_SEND_SIG(msg_p->msgno, nodeid_conn.server_mbox, "NODE_ID_WRITE_REQ");
	itc_send(&msg_p, nodeid_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg_p = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);

	if (msg_check(&msg_p)) {
		result = XPAI_WRITE_NODE_ID_NOK_OTHER;
		goto ret_err;
	}

	switch (msg_p->msgno) {
		case NODE_ID_WRITE_CFM:
			TPT_REC_SIG(msg_p->msgno, "NODE_ID_WRITE_CFM");
			break;

		case NODE_ID_WRITE_REJ:
			TPT_REC_SIG(msg_p->msgno,
			            STR("NODE_ID_WRITE_REJ: error code=%d",
			                msg_p->nodeid_write_rej.error_code));
			result = convert_err_code(msg_p->nodeid_write_rej.error_code, 0);
			break;
	}
	itc_free(&msg_p);

ret_err:
	return result;
}

S32 XPAI_ReadNodeId(U32 *length, U8 *buffer)
{
	return readnodeid(length, buffer);
}
S32 XPAI_WriteNodeId(U32 length, U8 *buffer)
{
	return writenodeid(length, buffer);
}

int32_t xpai_nodeid_init(void)
{
	return nodeid_init();
}

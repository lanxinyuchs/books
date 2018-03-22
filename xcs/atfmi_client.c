/**
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
#include <stdlib.h>
#include <string.h>

#include <itc.h>
#include "atfmi_internal.h"
#include "atfmi.h"
#include "atfmi.sig"

#include "conn-establish.h"
#include "conn-establish-helper.h"

/* Logging */
#include "com_ericsson_atfmi_client.h"

#define MY_CONN_TMO 1000

union itc_msg
{
	uint32_t msgno;
	ATFMI_MESSAGES;
};

typedef struct
{
	itc_mbox_id_t server_mbox_id;
	uint32_t procedure_ref;
	uint32_t client_ref;
	uint32_t requested_versions[ATFMI_NO_OF_SUPPORTED_VERSIONS];
	uint32_t requested_version_count;
	uint32_t server_ref;
	uint32_t selected_version;
} ConEstData;

/*----------------------------  Definition of Global Variables  -------------*/
/*----------------------------  Definition of Local Variables  --------------*/

atfmi_result_t atfmi_init(const char *profile)
{
	union itc_msg *msg = NULL, *recv_msg = NULL, *monitor_msg = NULL;
	itc_monitor_id_t monitor_id;
	uint32_t filter[] = { 3, ATFMI_INIT_CFM,
	                         ATFMI_INIT_REJ,
	                         ATFMI_SERVER_DOWN_IND };
	uint32_t result;
	atfmi_result_t res;
	ATFMI_CONN_ESTABLISH_MESSAGES_STRUCT(atfmi_conn_messages);
	ConEstData con_est_data =
	{.server_mbox_id = ITC_NO_ID,
	 .procedure_ref = 0,
	 .requested_versions = {ATFMI_REQ_VERSION },
	 .requested_version_count =(sizeof(con_est_data.requested_versions)
	 / sizeof(con_est_data.requested_versions[0]))
	};

	con_est_data.client_ref = itc_current_mbox();
	con_est_data.server_mbox_id = itc_locate(ATFMI_SERVER_MBOX_NAME);
	if (con_est_data.server_mbox_id == ITC_NO_ID)
	{
		TPT_ERROR(STR("Client:Cannot locate the server \"%s\"",
		            ATFMI_SERVER_MBOX_NAME));
		return ATFMI_WRONG_STATE;
	}

	result = conn_establish(con_est_data.server_mbox_id,
	                        ++(con_est_data.procedure_ref),
	                        con_est_data.client_ref,
	                        con_est_data.requested_version_count,
	                        con_est_data.requested_versions,
	                        &atfmi_conn_messages,
	                        MY_CONN_TMO,
	                        /*returned values*/
	                        &con_est_data.server_ref,
	                        &con_est_data.selected_version);

	if (result != CONN_ESTABLISH_SUCCESS)
	{
		TPT_ERROR(STR(
		       "Connection establish failed (reason:0x%08x)",
		       result));
		return ATFMI_OTHER_ERROR;
	}

	if (con_est_data.selected_version !=
	    con_est_data.requested_versions[0])
	{
		TPT_ERROR(STR(
		       "Wrong selected protocol version, "
		       "expected: 0x%08x, got: 0x%08x",
		       con_est_data.requested_versions[0],
		       con_est_data.selected_version));
		return ATFMI_OTHER_ERROR;
	}
	monitor_msg = itc_alloc(sizeof(struct atfmi_server_down_ind),
	                        ATFMI_SERVER_DOWN_IND);
	monitor_id = itc_monitor(con_est_data.server_mbox_id, &monitor_msg);

	msg = itc_alloc(sizeof(struct atfmi_init_req), ATFMI_INIT_REQ);

	msg->atfmi_init_req.procedure_ref = ++(con_est_data.procedure_ref);
	msg->atfmi_init_req.connection_ref = con_est_data.server_ref;
	strncpy(msg->atfmi_init_req.profile,
	        profile,
	        ATFMI_MAX_PROFILE_NAME_SIZE);

	itc_send(&msg, con_est_data.server_mbox_id, ITC_MY_MBOX);

	recv_msg = itc_receive(filter, ITC_NO_TMO, ITC_FROM_ALL);

	if (recv_msg->msgno != ATFMI_INIT_CFM)
	{
		switch (recv_msg->msgno) {
			case ATFMI_SERVER_DOWN_IND:
				TPT_REC_SIG((uintptr_t)recv_msg,
				            "ATFMI_SERVER_DOWN_IND");
				itc_free(&recv_msg);
				res = ATFMI_WRONG_STATE;
				goto free_disconnect;
			case ATFMI_INIT_REJ:
				TPT_REC_SIG((uintptr_t)recv_msg,
				            STR("ATFMI_INIT_REJ code %u",
				            recv_msg->atfmi_init_rej.errorCode));
				res = recv_msg->atfmi_init_rej.errorCode;
				goto free_disconnect;
			default:
				TPT_ERROR(STR(
				       "Received an unexpected message 0x%x "
				       "sender: 0x%x",
				       recv_msg->msgno, itc_sender(recv_msg)));
				res = ATFMI_UNSUPPORTED;
				goto free_disconnect;

		}
	}
	res = ATFMI_SUCCESS;

free_disconnect:
	itc_free(&recv_msg);
	itc_unmonitor(monitor_id);

	result = conn_disconnect(con_est_data.server_mbox_id,
	                         ++(con_est_data.procedure_ref),
	                         con_est_data.server_ref,
	                         &atfmi_conn_messages,
	                         MY_CONN_TMO);

	if (result != CONN_ESTABLISH_SUCCESS)
	{
		TPT_ERROR(STR(
		       "Connection disconnect failed (reason:0x%08x)",
		       result));
		return ATFMI_OTHER_ERROR;
	}

	TPT_INFO(STR("Client %s init :Done. res %u", profile, res));

	return res;
}

atfmi_result_t atfmi_shutdown(const char *profile)
{
	union itc_msg *msg = NULL, *recv_msg = NULL, *monitor_msg = NULL;
	itc_monitor_id_t monitor_id;
	uint32_t filter[] = { 3, ATFMI_SHUTDOWN_CFM,
	                         ATFMI_SHUTDOWN_REJ,
	                         ATFMI_SERVER_DOWN_IND };
	uint32_t result;
	atfmi_result_t res;
	ATFMI_CONN_ESTABLISH_MESSAGES_STRUCT(atfmi_conn_messages);
	ConEstData con_est_data =
	{ .server_mbox_id = ITC_NO_ID,
	  .procedure_ref = 0,
	  .requested_versions = {ATFMI_REQ_VERSION },
	  .requested_version_count = (sizeof(con_est_data.requested_versions) /
	                         sizeof(con_est_data.requested_versions[0]))
	};
	con_est_data.client_ref = itc_current_mbox();
	con_est_data.server_mbox_id = itc_locate(ATFMI_SERVER_MBOX_NAME);

	if (con_est_data.server_mbox_id == ITC_NO_ID)
	{
		TPT_ERROR(STR("Client:Cannot locate the server \"%s\"",
	                         ATFMI_SERVER_MBOX_NAME));
		return ATFMI_WRONG_STATE;
	}

	result = conn_establish(con_est_data.server_mbox_id,
	                        ++(con_est_data.procedure_ref),
	                        con_est_data.client_ref,
	                        con_est_data.requested_version_count,
	                        con_est_data.requested_versions,
	                        &atfmi_conn_messages,
	                        MY_CONN_TMO,
	                        /*returned values*/
	                        &con_est_data.server_ref,
	                        &con_est_data.selected_version);

	if (result != CONN_ESTABLISH_SUCCESS)
	{
		TPT_ERROR(STR(
		       "Connection establish failed (reason:0x%08x)",
		       result));
		return ATFMI_OTHER_ERROR;
	}

	if (con_est_data.selected_version !=
	                     con_est_data.requested_versions[0])
	{
		TPT_ERROR(STR(
		       "Wrong selected protocol version, "
		       "expected: 0x%08x, got: 0x%08x",
		       con_est_data.requested_versions[0],
		       con_est_data.selected_version));
		return ATFMI_OTHER_ERROR;
	}
	monitor_msg = itc_alloc(sizeof(struct atfmi_server_down_ind),
	                        ATFMI_SERVER_DOWN_IND);
	monitor_id = itc_monitor(con_est_data.server_mbox_id, &monitor_msg);

	msg = itc_alloc(sizeof(struct atfmi_shutdown_req), ATFMI_SHUTDOWN_REQ);

	msg->atfmi_shutdown_req.procedure_ref = ++(con_est_data.procedure_ref);
	msg->atfmi_shutdown_req.connection_ref = con_est_data.server_ref;
	strncpy(msg->atfmi_shutdown_req.profile,
	        profile,
	        ATFMI_MAX_PROFILE_NAME_SIZE);

	itc_send(&msg, con_est_data.server_mbox_id, ITC_MY_MBOX);

	recv_msg = itc_receive(filter, ITC_NO_TMO, ITC_FROM_ALL);

	if (recv_msg->msgno != ATFMI_SHUTDOWN_CFM)
	{
		switch (recv_msg->msgno) {
			case ATFMI_SERVER_DOWN_IND:
				TPT_REC_SIG((uintptr_t)recv_msg,
				            "ATFMI_SERVER_DOWN_IND");
				itc_free(&recv_msg);
				res = ATFMI_WRONG_STATE;
				goto free_disconnect;
			case ATFMI_SHUTDOWN_REJ:
				TPT_REC_SIG((uintptr_t)recv_msg,
				            STR("ATFMI_SHUTDOWN_REJ code %u",
				       recv_msg->atfmi_shutdown_rej.errorCode));
				res = recv_msg->atfmi_shutdown_rej.errorCode;
				goto free_disconnect;
			default:
				TPT_ERROR(STR(
				       "Received an unexpected message 0x%x",
				       recv_msg->msgno));
				res = ATFMI_UNSUPPORTED;
				goto free_disconnect;

		}
	}
	res = ATFMI_SUCCESS;

free_disconnect:
	itc_free(&recv_msg);
	itc_unmonitor(monitor_id);

	result = conn_disconnect(con_est_data.server_mbox_id,
	                         ++(con_est_data.procedure_ref),
	                         con_est_data.server_ref,
	                         &atfmi_conn_messages,
	                         MY_CONN_TMO);

	if (result != CONN_ESTABLISH_SUCCESS)
	{
		TPT_ERROR(STR(
		       "Connection disconnect failed (reason:0x%08x)",
		       result));
		return ATFMI_OTHER_ERROR;
	}

	TPT_INFO(STR("Client %s shutdown :Done. res %u", profile, res));

	return res;
}

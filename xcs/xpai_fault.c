/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
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
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>
#include <itc.h>
#include "xcbc_fault_if.h"
#include "xpai_xcbc_fault_if.h"
#include "common.h"
#include "rhd-common.h"
#include "log_tpt.h"
#include "conn-establish-helper.h"

static struct server_info fault_conn;
static bool initialized = false;
static struct conn_establish_msg_numbers fault_conn_messages = {
	XCBC_FAULT_CONN_ESTABLISH_REQ,
	XCBC_FAULT_CONN_ESTABLISH_CFM,
	XCBC_FAULT_CONN_ESTABLISH_REJ,
	XCBC_FAULT_CONN_DISCONNECT_REQ,
	XCBC_FAULT_CONN_DISCONNECT_CFM,
	XCBC_FAULT_CONN_DISCONNECT_REJ,
	XCBC_FAULT_CONN_MONITOR_FWD
};

union itc_msg {
	uint32_t msgno;
	XCBC_FAULT_STRUCTS
};
/****
 *
 *      Function XPAI_UnsubscribeFaults
 *
 *****/
U32 XPAI_UnsubscribeFaults(U32 pid)
{
	union itc_msg *out_msg;

	TPT_TRACE(1, STR("XPAI_UnsubscribeFaults(pid=0x%x)", pid));
	if (!initialized) {
		TPT_ERROR("Connection to server is not started");
		return 1;
	}
	/* Send fault ind message */
	out_msg = itc_alloc(sizeof(struct xcbc_report_fault),
		            ITC_MONITOR_DEFAULT_NO);
	out_msg->sub_faults_req.mbox = pid;
	out_msg->report_fault.connection_ref = fault_conn.server_ref;
	TPT_SEND_SIG(out_msg->msgno, fault_conn.server_mbox, "XCBC_XPAI_List_Node_Delete");
	itc_send(&out_msg, fault_conn.server_mbox, ITC_MY_MBOX);
	return 0;
}

/* FIXME: in this file, the log should be replaced with united trace solution */


/****
 *
 *      Function XPAI_Fault
 *
 *****/
U32 XPAI_Fault(U16 FaultType, U16 FaultRecoveryAction, char *FaultDescription)
{
	uint32_t result = XPAI_FAULT_OK;
	union itc_msg *out_msg;
	char *par = "";

	TPT_TRACE(1, STR("Fault (type=%d recovAct=%d descr=%s)",
	          FaultType, FaultRecoveryAction,
	          FaultDescription ? FaultDescription : ""));

	if (!initialized) {
		TPT_ERROR("Connection to server is not started");
		return XPAI_FAULT_NOT_OK;
	}

	/* Check the Fault type */
	if ((FaultType > XPAI_GENERAL_HW_ERROR) ||
	    (FaultType < XPAI_GENERAL_SW_ERROR)) {
		par = "FaultType";
		result = XPAI_FAULT_NOT_OK;
	}

	/* Check the Recovery action */
	if (FaultRecoveryAction > XPAI_ENTITY_DEGRADED) {
		par = "FaultRecoveryAction";
		result = XPAI_FAULT_NOT_OK;
	}

	/* Check the Fault description */
	if (!FaultDescription) {
		FaultDescription = "";
	}

	/* Send fault ind message */
	if (result == XPAI_FAULT_OK) {
		out_msg = itc_alloc(sizeof(struct xcbc_report_fault),
		                    XCBC_REPORT_FAULT);
		out_msg->report_fault.fault_type = FaultType;
		out_msg->report_fault.recov_act = FaultRecoveryAction;
		strncpy(out_msg->report_fault.fault_description, FaultDescription,
		        XCBC_MAX_FAULT_DESCR_LEN);
		out_msg->report_fault.fault_description[XCBC_MAX_FAULT_DESCR_LEN - 1] = '\0';
		out_msg->report_fault.connection_ref = fault_conn.server_ref;

		TPT_SEND_SIG(out_msg->msgno, fault_conn.server_mbox, "XCBC_REPORT_FAULT");
		itc_send(&out_msg, fault_conn.server_mbox, ITC_MY_MBOX);
	} else {
		TPT_ERROR(STR("Illegal parameter value '%s'. No fault reported!!!", par));
	}

	return result;
}

/****
 *
 *      Function XPAI_SubscribeFaults
 *
 *****/
U32 XPAI_SubscribeFaults(U32 pid)
{
	union itc_msg     *out_msg;
	union itc_msg     *in_msg;
	uint32_t           result = XPAI_SUBSCRIBE_FAULTS_OK;

	static uint32_t  rx_filter[] = {2,
	                                XCBC_SUBSCRIBE_FAULTS_CFM,
	                                XCBC_SUBSCRIBE_FAULTS_REJ
	                               };

	TPT_TRACE(1, STR("XPAI_SubscribeFaults(pid=0x%x)", pid));

	if (!initialized) {
		TPT_ERROR("Connection to server is not started");
		return XPAI_SUBSCRIBE_FAULTS_NOK_SERVER;
	}
	/* Send request. */
	out_msg = itc_alloc(sizeof(struct subscribe_faults_req),
	                    XCBC_SUBSCRIBE_FAULTS_REQ);
	out_msg->sub_faults_req.mbox = pid;
	out_msg->sub_faults_req.connection_ref = fault_conn.server_ref;
	TPT_SEND_SIG(out_msg->msgno, fault_conn.server_mbox, "XCBC_SUBSCRIBE_FAULTS_REQ");
	itc_send(&out_msg, fault_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (in_msg->msgno) {
	case XCBC_SUBSCRIBE_FAULTS_CFM:
		TPT_REC_SIG(in_msg->msgno, "XCBC_SUBSCRIBE_FAULTS_CFM\n");
		result = XPAI_SUBSCRIBE_FAULTS_OK;
		break;

	case XCBC_SUBSCRIBE_FAULTS_REJ:
		TPT_REC_SIG(in_msg->msgno, STR("XCBC_SUBSCRIBE_FAULTS_REJ: error code=%d",
		            in_msg->sub_faults_rej.err_code));
		result = in_msg->sub_faults_rej.err_code;
		break;

	default:
		TPT_ERROR(STR("Unexpected signal (msgno=0x%x) received from 0x%x",
		        in_msg->msgno, itc_sender(in_msg)));
		result = XPAI_SUBSCRIBE_FAULTS_NOK_OTHER;
		break;
	}
	itc_free(&in_msg);

	return result;
}

/****
 *
 *      Function XPAI_FaultClear
 *
 *****/
U32 XPAI_FaultClear(U16 faultType, U16 recoveryAction)
{
	union itc_msg  *out_msg;
	union itc_msg  *in_msg;
	U32             returnCode = XPAI_FAULT_CLEAR_OK;

	static uint32_t rx_filter[] = {2,
	                               XCBC_FAULT_CLEAR_CFM,
	                               XCBC_FAULT_CLEAR_REJ
	                              };

	TPT_TRACE(1, STR("XPAI_FaultClear(faultType=%d recoveryAction=%d)",
	          faultType, recoveryAction));

	if (!initialized) {
		TPT_ERROR("Connection to server is not started");
		return XPAI_FAULT_CLEAR_NOK_SERVER;
	}

	/* If wrong faultType. */
	if (!( (faultType == XPAI_GENERAL_SW_ERROR) ||
	       (faultType == XPAI_GENERAL_HW_ERROR) ||
	       (faultType == XPAI_FAULT_CLEAR_FAULT_TYPE_ALL) )) {
		TPT_ERROR(STR("XPAI_FaultClear() received illegal faultType: %d", faultType));
		returnCode = XPAI_FAULT_CLEAR_NOK_WRONG_PARAM;
		/* Else if wrong recoveryAction. */
	} else if ( (recoveryAction > XPAI_ENTITY_DEGRADED) &&
	            (recoveryAction != XPAI_FAULT_CLEAR_RECOVERY_ACTION_ALL) ) {
		TPT_ERROR(STR("XPAI_FaultClear() received illegal recoveryAction: %d",
		        recoveryAction));
		returnCode = XPAI_FAULT_CLEAR_NOK_WRONG_PARAM;
	} else {
		/* Send request. */
		out_msg = itc_alloc(sizeof(struct fault_clear_req),
		                    XCBC_FAULT_CLEAR_REQ);
		out_msg->clear_req.fault_type = faultType;
		out_msg->clear_req.recov_act  = recoveryAction;
		out_msg->clear_req.connection_ref = fault_conn.server_ref;
		TPT_SEND_SIG(out_msg->msgno, fault_conn.server_mbox, "XCBC_FAULT_CLEAR_REQ");
		itc_send(&out_msg, fault_conn.server_mbox, ITC_MY_MBOX);

		/* Receive answer. */
		in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
		switch (in_msg->msgno) {
		case XCBC_FAULT_CLEAR_CFM:
			TPT_REC_SIG(in_msg->msgno, "XPP_XCBC_FAULT_CLEAR_CFM");
			break;
		case XCBC_FAULT_CLEAR_REJ:
			TPT_REC_SIG(in_msg->msgno, STR("XCBC_FAULT_CLEAR_REJ: error code=%d",
			          in_msg->clear_rej.err_code));
			returnCode = in_msg->clear_rej.err_code;
			break;

		default:
			TPT_ERROR(STR("Unexpected signal (sigNo=0x%c) received from 0x%x",
			        in_msg->msgno, itc_sender(in_msg)));
			returnCode = XPAI_FAULT_CLEAR_NOK_OTHER;
			break;
		}
		itc_free(&in_msg);
	}

	return returnCode;
}

/****
 *
 *      Function fault_init
 *
 *****/
int32_t xpai_fault_init(uint32_t client_ref)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {FAULT_SERVER_VERSIONS};
	uint32_t res;

	if (initialized) {
		TPT_TRACE(1, "Connection already initialized!");
		res = INIT_OK;
		goto fault_init_end;
	}

	/*call connection establish*/
	res = xpai_locate_mbox(FAULT_SERVER_MAILBOX, &fault_conn.server_mbox);
	if (res != INIT_OK) {
		TPT_ERROR("Client:Cannot find fault server mailbox");
		goto fault_init_end;
	}

	res = conn_establish(
	              /*input parameters*/
	              fault_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &fault_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &fault_conn.server_ref,
	              &fault_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed (reason:0x%08x)",
		        res));
		res = INIT_SERVER_NOK;
		goto fault_init_end;
	}

	TPT_TRACE(1, STR("mailbox id for fault server: %u\n \
                   server connection ref:%u\n \
                   selected version: %u\n",
	          fault_conn.server_mbox,
	          fault_conn.server_ref,
	          fault_conn.selected_version));

	initialized = true;
	res = INIT_OK;

fault_init_end:
	return res;
}

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
#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include <itc.h>
#include "rhd-mmi-if.h"
#include "rhd-common.h"
#include "xdai_xdmmi_if.h"
#include "common.h"
#include "log_tpt.h"
#include "conn-establish-helper.h"

static struct server_info mmi_conn;
static bool initialized = false;

union itc_msg {
	uint32_t msgno;
	RHD_MMI_STRUCTS
};

static struct conn_establish_msg_numbers mmi_conn_messages = {
	MMI_CONN_ESTABLISH_REQ,
	MMI_CONN_ESTABLISH_CFM,
	MMI_CONN_ESTABLISH_REJ,
	MMI_CONN_DISCONNECT_REQ,
	MMI_CONN_DISCONNECT_CFM,
	MMI_CONN_DISCONNECT_REJ,
	MMI_CONN_MONITOR_FWD
};
/* Fix me: in this file, the log should be replaced with united trace solution */

/****
 *
 *      Function state_to_string
 *
 *****/
static const char *state_to_string(U32 state)
{
	static char buf[30];
	switch (state) {
	case XDAI_MAINTENANCE_STATE_DEACTIVATED:
		return "XDAI_MAINTENANCE_STATE_DEACTIVATED";
		break;
	case XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
		return "XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS";
		break;
	case XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
		return "XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC";
		break;
	case XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE:
		return "XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE";
		break;
	default:
		(void) snprintf(buf, 30, "<unknown state 0x%x>", (int)state);
		return buf;
		break;
	}
}

/****
 *
 *      Function XDAI_SetMaintenanceState
 *
 *****/
U32 XDAI_SetMaintenanceState(U32 state)
{
	union itc_msg *out_msg = NULL;
	union itc_msg *in_msg  = NULL;
	uint32_t rx_filter[] = {2, RHD_MMI_SET_STATE_CFM,
	                        RHD_MMI_SET_STATE_REJ
	                       };
	U32 result;

	TPT_TRACE(1, STR("XDAI_SetMaintenanceState (state=%s)\n",
	          state_to_string(state)));

	if (!initialized) {
		TPT_ERROR("Connection to MMI is not initialized");
		return XDAI_SET_ERROR;
	}

	/* Allocate and initiate the request signal to MMI. */

	out_msg = itc_alloc(sizeof(struct mmi_set_state_req),
	                    RHD_MMI_SET_STATE_REQ);
	out_msg->set_state_req.state = state;
	out_msg->set_state_req.connection_ref = mmi_conn.server_ref;

	TPT_SEND_SIG(out_msg->msgno, mmi_conn.server_mbox,
			STR("Set state to %s ", state_to_string(state)));
	itc_send(&out_msg, mmi_conn.server_mbox, ITC_MY_MBOX);

	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch(in_msg->msgno) {
	case RHD_MMI_SET_STATE_CFM:
		TPT_REC_SIG(in_msg->msgno, "RHD_MMI_SET_STATE_CFM:");
		result = XDAI_SET_SUCCESS;
		break;
	case RHD_MMI_SET_STATE_REJ:
		TPT_REC_SIG(in_msg->msgno, STR("RHD_MMI_SET_STATE_REJ: error 0x%x",
		          in_msg->set_state_rej.error));
		result = (in_msg->set_state_rej.error == RHD_MMI_BUTTON_PRESSED) ?
		         XDAI_BUTTON_PRESSED : XDAI_SET_ERROR;
		break;
	default:
		TPT_ERROR(STR("Received unexpected signal 0x%x", in_msg->msgno));
		result = XDAI_SET_ERROR;
		break;
	}

	itc_free(&in_msg);
	return result;
} /* end XDAI_SetMaintenenceState() */


/****
 *
 *      Function XDAI_SubscribeMaintenanceState
 *
 *****/
U32 XDAI_SubscribeMaintenanceState(PROCESS pid)
{
	union itc_msg *out_msg = NULL;
	union itc_msg *in_msg  = NULL;
	uint32_t rx_filter[] = {2, RHD_MMI_SUBSCRIBE_CFM,
	                        RHD_MMI_SUBSCRIBE_REJ
	                       };
	U32 result = 0;

	TPT_TRACE(1, STR("XDAI_SubscribeMaintenanceState(pid=0x%x)\n", pid));

	if (!initialized) {
		TPT_ERROR("Connection to MMI is not initialized");
		return XDAI_SUBSCRIBE_ERROR;
	}

	/* Allocate and initiate the request signal to MMI. */

	out_msg = itc_alloc(sizeof(struct mmi_subscribe_req),
	                    RHD_MMI_SUBSCRIBE_REQ);
	/* Only can be used to subscribe the caller itself */
	out_msg->subscribe_req.mbox = pid;
	out_msg->set_state_req.connection_ref = mmi_conn.server_ref;

	TPT_SEND_SIG(out_msg->msgno, mmi_conn.server_mbox, "RHD_MMI_SUBSCRIBE_REQ");
	itc_send(&out_msg, mmi_conn.server_mbox, ITC_MY_MBOX);

	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (in_msg->msgno) {
	case RHD_MMI_SUBSCRIBE_CFM:
		TPT_REC_SIG(in_msg->msgno, "RHD_MMI_SUBSCRIBE_CFM\n");
		result = XDAI_SUBSCRIBE_SUCCESS;
		break;

	case RHD_MMI_SUBSCRIBE_REJ:
		TPT_REC_SIG(in_msg->msgno, STR("RHD_MMI_SUBSCRIBE_REJ: error=0x%x",
		          in_msg->subscribe_rej.error));
		result = XDAI_SUBSCRIBE_ERROR;
		break;

	default:
		TPT_ERROR(STR("Received unexpected signal 0x%x", in_msg->msgno));
		break;
	}
	itc_free(&in_msg);

	return result;
} /* end XDAI_SubscribeMaintenanceState() */

/****
 *
 *      Function mmi_init
 *
 *****/
S32 xdai_mmi_init(U32 client_ref)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {MMI_SERVER_VERSIONS};
	uint32_t res;

	if (initialized) {
		TPT_TRACE(1, "Already initialized!");
		res = INIT_OK;
		goto mmi_init_end;
	}

	/*call connection establish*/
	res = xpai_locate_mbox(RHD_MMI_MAILBOX, &mmi_conn.server_mbox);
	if (res != INIT_OK) {
		TPT_ERROR("Client:Cannot find server mailbox");
		goto mmi_init_end;
	}

	res = conn_establish(
	              /*input parameters*/
	              mmi_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &mmi_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &mmi_conn.server_ref,
	              &mmi_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed \
                         (reason:0x%08x)", res));
		res = INIT_SERVER_NOK;
		goto mmi_init_end;
	}

	TPT_TRACE(1, STR("mailbox id for MMI: %u\n \
                   server connection ref:%u\n \
                   selected version: %u\n",
	          mmi_conn.server_mbox,
	          mmi_conn.server_ref,
	          mmi_conn.selected_version));
	initialized = true;
	res = INIT_OK;

mmi_init_end:
	return res;
}

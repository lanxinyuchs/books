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
#include "rhd-vii-if.h"
#include "xpai_xmmi_if.h"
#include "common.h"
#include "rhd-common.h"
#include "log_tpt.h"
#include "conn-establish-helper.h"

#define XPAI_VII_OTHER_ERROR -100

static struct server_info vii_conn;
static bool initialized = false;

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_VII_STRUCTS
};

static struct conn_establish_msg_numbers vii_conn_messages = {
	VII_CONN_ESTABLISH_REQ,
	VII_CONN_ESTABLISH_CFM,
	VII_CONN_ESTABLISH_REJ,
	VII_CONN_DISCONNECT_REQ,
	VII_CONN_DISCONNECT_CFM,
	VII_CONN_DISCONNECT_REJ,
	VII_CONN_MONITOR_FWD
};


/****
 *
 *      Function XPAI_Vii
 *
 *****/
U32 XPAI_Vii(U32 indication)
{
	U32 tmp = indication;
	union itc_msg *msg;

	TPT_TRACE(1, STR("XPAI_Vii(indication=0x%x)", indication));

	/* Detect if indication selects one of the optional LEDs */
	if (tmp & OPTIONAL_LED_MASK) { /*  LED selector is not 0 */
#ifndef SPECIAL_LED
		TPT_INFO("ABN:Fan led is not supported");
		return XPAI_VII_INVALID_REQ;
#else
		/* Verify that indication only contain
		 * a valid action for optional LED */
		/* This is all bits that are not used for special LEDs */
		if ((tmp & 0x3fff1fff) != 0)
			return XPAI_VII_INVALID_REQ;
		/* Verify if the requested command is valid for the optional LED.
		 * List and value of valid operations are defined
		 * in xpai_xmmi_if.h */
		tmp &= OPTIONAL_LED_INDICATION_MASK;
		tmp = tmp >> XPAI_VII_SHIFT_INDICATION;
		if (tmp > 2)
			return XPAI_VII_INVALID_REQ;
#endif
	} else {
		/* Verify that indication mask is one of
		 * the values defined in xpai_xmmi_if.h */
		/* These are all bits not used by red,
		 * green and yellow/blue LEDs */
		if ((tmp & 0xec00ec00) != 0)
			return XPAI_VII_INVALID_REQ;

		/* Verify that indication contains one and only one bit,
		 * otherwise report error. This verifies that no or:ed requests
		 * takes place and that no green led status indicator in
		 * xpai_xmmi_if.h is used in XPAI_Vii
		 */
		while (tmp > 1) {
			if ((tmp & 1) != 0)
				return XPAI_VII_INVALID_REQ;

			tmp >>= 1;
		}
		if (tmp == 0)
			return XPAI_VII_INVALID_REQ;
	}

	/* Check if the connection is established */
	if (!initialized) {
		TPT_ERROR("Connection to VII server is not established");
		return XPAI_VII_INVALID_REQ;
	}

	/* Send the request */
	msg = itc_alloc(sizeof(struct vii_ind_req), RHD_VII_LED_CTRL_IND);
	msg->ind_req.req = indication;
	msg->ind_req.connection_ref = vii_conn.server_ref;

	TPT_SEND_SIG(msg->msgno, vii_conn.server_mbox,
	             STR("Send RHD_VII_LED_CTRL_IND: req=%u",
	                 msg->ind_req.req));
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);
	return XPAI_VII_OK;
}

/****
 *
 *      Function XPAI_GetVii
 *
 *****/
U32 XPAI_GetVii(U32 *redLed, U32 *greenLed, U32 *yellowLed)
{
	union itc_msg *msg;
	uint32_t      rx_filter[] = {1, RHD_VII_INFO_CFM};
	U32           return_code = XPAI_GET_VII_OK;

	TPT_TRACE(1, STR("XPAI_GetVii(redLed=%p greenLed=%p yellowLed=%p)",
	             (void *)redLed, (void *)greenLed, (void *)yellowLed));

	/* Check if the connection is established */
	if (!initialized) {
		TPT_ERROR("Connection to VII server is not established");
		return XPAI_GET_VII_NOK_SERVER;
	}

	/* Send request. */
	msg = itc_alloc(sizeof(conn_any_msg_t), RHD_VII_INFO_REQ);
	msg->any_msg.connection_ref = vii_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, vii_conn.server_mbox, "Send RHD_VII_INFO_REQ");
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	switch (msg->msgno) {
	case RHD_VII_INFO_CFM:
		TPT_REC_SIG(msg->msgno,
		            STR("Receive RHD_VII_INFO_CFM: req_masks red=0x%x "
		                "blue=0x%x, green=0x%x\n",
		                msg->led_info.req_masks.red,
		                msg->led_info.req_masks.blue,
		                msg->led_info.req_masks.green));
		/* Check that the pointers are not null and
		 * asmsgn values to out params. */
		if ((redLed) && (greenLed) && (yellowLed)) {
			*redLed = msg->led_info.req_masks.red;
			*greenLed = msg->led_info.req_masks.green;
			/* To be compatible with G1 */
			if (*greenLed == VII_O_NO_POWER_LOADING_SW)
				*greenLed = XPAI_VII_NO_POWER;
			else if (*greenLed == VII_MISSING_RESOURCE_END)
				*greenLed = XPAI_VII_POWER;
			/* Info LED on XENON is blue */
			*yellowLed = msg->led_info.req_masks.blue;
		} else {
			return_code = XPAI_GET_VII_NOK_OTHER;
		}

		break;

	default:
		TPT_ERROR(STR("Unexpected msg (msgno=0x%X) received from 0x%X",
		        msg->msgno, itc_sender(msg)));
		return_code = XPAI_GET_VII_NOK_OTHER;
		break;
	}
	itc_free(&msg);

	/* Send back msgnal to release RHD_VII. */
	msg = itc_alloc(sizeof(conn_any_msg_t), RHD_VII_DONE_IND);
	msg->any_msg.connection_ref = vii_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, vii_conn.server_mbox, "Send RHD_VII_DONE_IND");
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);

	return return_code;
}

/****
 *
 *      Function XPAI_GetVii2
 *
 *****/
U32 XPAI_GetVii2(U32 led, U32 *indication)
{
	union itc_msg *msg;
	uint32_t      rx_filter[] = {2, RHD_VII2_INFO_CFM,
	                             RHD_VII2_INFO_REJ
	                            };
	U32           return_code = XPAI_GET_VII2_NOK_OTHER;

	if (!indication)
		return return_code;

	TPT_TRACE(1, STR("XPAI_GetVii2(led = 0x%x)", led));

	/* Check if the connection is established */
	if (!initialized) {
		TPT_ERROR("Connection to VII server is not established");
		return XPAI_GET_VII2_NOK_SERVER;
	}
#ifndef SPECIAL_LED
	if (led == VII_SPECIAL_LED) {
		TPT_INFO("ABN:Fan led is not supported");
		return XPAI_GET_VII2_NOK_NOT_AVAILABLE;
	}
#endif
	/* Send the request */
	msg = itc_alloc(sizeof(struct vii_ind_req), RHD_VII2_INFO_REQ);
	msg->ind_req.req = led;
	msg->ind_req.connection_ref = vii_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, vii_conn.server_mbox,
	             STR("Send RHD_VII2_INFO_REQ: req led=%u",
	                 msg->ind_req.req));
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);

	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	if (msg->msgno == RHD_VII2_INFO_CFM) {
		TPT_REC_SIG(msg->msgno,
		            STR("Indication = %u",
		            msg->info_cfm.ind));
		return_code = XPAI_GET_VII2_OK;
		*indication = msg->info_cfm.ind;
		if (led == VII_OPERATIONAL_LED) {
			if (msg->info_cfm.ind == VII_O_NO_POWER_LOADING_SW)
				*indication = XPAI_VII_NO_POWER;
			else if (msg->info_cfm.ind == VII_MISSING_RESOURCE_END)
				*indication = XPAI_VII_POWER;
		}
	} else {
		TPT_ERROR(STR("Unknown signal 0x%x from 0x%x",
		              msg->msgno, itc_sender(msg)));
		return_code = XPAI_GET_VII2_NOK_OTHER;
	}
	itc_free(&msg);

	/* Send back msgnal to release RHD_VII. */
	msg = itc_alloc(sizeof(conn_any_msg_t), RHD_VII_DONE_IND);
	msg->any_msg.connection_ref = vii_conn.server_ref;
	TPT_SEND_SIG(msg->msgno, vii_conn.server_mbox, "Send RHD_VII_DONE_IND");
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);

	return return_code;
}

/****
 *
 *      Function vii_init
 *
 *****/
S32 xpai_vii_init(U32 client_ref)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {VII_SERVER_VERSIONS};
	uint32_t res;

	if (initialized) {
		TPT_TRACE(1, "Already initialized!");
		res = INIT_OK;
		goto vii_init_end;
	}

	/*call connection establish*/
	res = xpai_locate_mbox(RHD_VII_MAILBOX, &vii_conn.server_mbox);
	if (res != INIT_OK) {
		TPT_ERROR("Client:Cannot find server mailbox");
		goto vii_init_end;
	}

	res = conn_establish(
	              /*input parameters*/
	              vii_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &vii_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &vii_conn.server_ref,
	              &vii_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed "
				"(reason:0x%08x)", res));
		res = INIT_SERVER_NOK;
		goto vii_init_end;
	}

	TPT_TRACE(1, STR("mailbox id for VII: %u\n \
                 server connection ref:%u\n \
                 selected version: %u\n",
	             vii_conn.server_mbox,
	             vii_conn.server_ref,
	             vii_conn.selected_version));
	initialized = true;
	res = INIT_OK;

vii_init_end:
	return res;
}

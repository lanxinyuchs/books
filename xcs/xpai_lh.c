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
#include <itc.h>
#include <stdio.h>
#include <rhami_lh_spec.h>
#include "common.h"
#include "xpai_xhp_if.h"
#include "log_tpt.h"
#include "libecp.h"
#include "rhd-common.h"
#include "conn-establish-helper.h"
#include "uio_helper.h"

#define XPAI_LH_NUMBER_OF_PORTS  7
#define XPAI_LH_NUMBER_OF_PORTS_TRXM 1
static uint32_t procedure_ref = 0;
static uint32_t my_client_ref[XPAI_LH_NUMBER_OF_PORTS];
static struct server_info lh_conn[XPAI_LH_NUMBER_OF_PORTS];
static int initialized[XPAI_LH_NUMBER_OF_PORTS];

union itc_msg {
	uint32_t        msgno;
	conn_any_msg_t  any_msg;
	RHAMI_LH_STRUCTS
};

static struct conn_establish_msg_numbers lh_conn_messages = {
	RHAMI_LH_CONN_ESTABLISH_REQ,
	RHAMI_LH_CONN_ESTABLISH_CFM,
	RHAMI_LH_CONN_ESTABLISH_REJ,
	RHAMI_LH_CONN_DISCONNECT_REQ,
	RHAMI_LH_CONN_DISCONNECT_CFM,
	RHAMI_LH_CONN_DISCONNECT_REJ,
	RHAMI_LH_CONN_MONITOR_FWD
};

static int msg_check(union itc_msg **msg, uint32_t portId)
{
	if ((*msg)->any_msg.procedure_ref != procedure_ref) {
		printf("Server replied with invalid procedure_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       procedure_ref, (*msg)->any_msg.procedure_ref);
		itc_free(msg);
		return -1;
	}
	if ((*msg)->any_msg.connection_ref != my_client_ref[portId]) {
		printf("Server replied with invalid connection_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       my_client_ref[portId], (*msg)->any_msg.connection_ref);
		itc_free(msg);
		return -1;
	}

	return 0;
}

static S32 get_server_mbox(itc_mbox_id_t *server_mbox, U32 port)
{
	char mailbox[sizeof(RHAMI_LH_MAILBOX) + sizeof("-0xFFFFFFFF")];

	if (snprintf(mailbox, sizeof(mailbox), "%s_%d", RHAMI_LH_MAILBOX,
	             port) < 0) {
		TPT_ERROR("snprintf error");
		return -1;
	}

	return xpai_locate_mbox(mailbox, server_mbox);
}

/******************************************************************************
 *
 * Global function:
 *      XPAI_StartLink
 *
 * Description:
 *      This function is further described in 'xpai_xhp_if.h'
 *
 * Side effects:
 *
 *****************************************************************************/
S32 XPAI_StartLink(U32 port, U32 channel)
{
	union itc_msg      *send_msg;
	union itc_msg      *receive_msg;
	S32                returnCode = XPAI_START_LINK_OK;
	static U32 rx_filter[] = {2,
	                          RHAMI_LH_STARTLINK_CFM,
	                          RHAMI_LH_STARTLINK_REJ};

	/*TRACE_OBJ_PARAM(XPAI, STR("XPAI_StartLink("
				  "port=0x%X, "
				  "channel=0x%X)",
				  port, channel));*/

	TPT_INFO(STR("Calling XPAI_StartLink, port %d, channel %d\n", port, channel));
	/* if wrong port. */
	if (port >= XPAI_LH_NUMBER_OF_PORTS)
	{
		TPT_ERROR(STR("XPAI_StartLink() received illegal port: %d", port));
		return XPAI_START_LINK_NOK_PORT;
	}

	/* if wrong buffer. */
	if (channel >= ECP_MAX_NO_OF_BUFFS)
	{
		TPT_ERROR(STR("XPAI_StartLink() received illegal buffer: %d", channel));
		return XPAI_START_LINK_NOK_CHANNEL;
	}

	if (!initialized[port]) {
		TPT_ERROR(STR("Lh port%u is not initialized", port));
		return XPAI_START_LINK_NOK_OTHER;
	}

	/* Send start link request. */
	send_msg = itc_alloc(sizeof(struct lh_startlink_req), RHAMI_LH_STARTLINK_REQ);
	send_msg->startlink_req.connection_ref = lh_conn[port].server_ref;
	send_msg->startlink_req.procedure_ref = ++procedure_ref;
	send_msg->startlink_req.port = port;
	send_msg->startlink_req.channel_id = 0; /*ecp_address is not used for xenon 1.0*/
	send_msg->startlink_req.buff_idx = channel;

	TPT_SEND_SIG(send_msg->msgno, lh_conn[port].server_mbox, "RHAMI_LH_STARTLINK_REQ");
	itc_send(&send_msg, lh_conn[port].server_mbox, ITC_MY_MBOX);

	receive_msg = itc_receive(rx_filter, ITC_NO_TMO, lh_conn[port].server_mbox);
	if (msg_check(&receive_msg, port)) {
		returnCode = -1;
		goto exit;
	}
	switch (receive_msg->msgno)
	{
	case RHAMI_LH_STARTLINK_CFM:
		TPT_REC_SIG(receive_msg->msgno, "RHAMI_LH_STARTLINK_CFM");
		break;
	case RHAMI_LH_STARTLINK_REJ:
		TPT_REC_SIG(receive_msg->msgno, "RHAMI_LH_STARTLINK_REJ");
		returnCode = XPAI_START_LINK_NOK_OTHER;
		break;
	default:
		TPT_ERROR(STR("Unexpected signal (sigNo=0x%x) received from 0x%x",
		receive_msg->msgno, itc_sender(receive_msg)));
		returnCode = XPAI_START_LINK_NOK_OTHER;
		break;
	}
exit:
	if (receive_msg) itc_free(&receive_msg);
	return returnCode;
}

int32_t xpai_lh_port_init(uint32_t portId, uint32_t client_ref)
{
	uint32_t requested_versions[] = {LH_SERVER_VERSIONS};
	uint32_t res;

	if (portId >= XPAI_LH_NUMBER_OF_PORTS) {
		TPT_ERROR(STR("Lh port%u is not valid", portId));
		res = INIT_LH_PORT_NOK;
		goto lh_init_end;
	}
	TPT_INFO(STR("initialized[%d] == %d\n", portId, initialized[portId]));
	if (initialized[portId]) {
		TPT_TRACE(1, "Already initialized!");
		res = INIT_OK;
		goto lh_init_end;
	}

	my_client_ref[portId] = client_ref;
	/* Call connection establish */
	res = get_server_mbox(&lh_conn[portId].server_mbox, portId);
	if (res != INIT_OK) {
		TPT_ERROR(STR("Client:Cannot find server mailbox for port %u", portId));
		goto lh_init_end;
	}
	res = conn_establish(
	              /*input parameters*/
	              lh_conn[portId].server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &lh_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &lh_conn[portId].server_ref,
	              &lh_conn[portId].selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed on port %u"
		              "(reason:0x%08x)", portId, res));
		res = INIT_SERVER_NOK;
		goto lh_init_end;
	}
	TPT_TRACE(1, STR("mailbox id for LH%u: %u\n \
	                 server connection ref:%u\n \
	                 selected version: %u\n",
	                 portId,
	                 lh_conn[portId].server_mbox,
	                 lh_conn[portId].server_ref,
	                 lh_conn[portId].selected_version));

	TPT_INFO(STR("initialized[%d] done!\n", portId));
	initialized[portId] = 1;
	res = INIT_OK;

lh_init_end:
	return res;
}

int32_t xpai_lh_init(uint32_t client_ref)
{
	int32_t res;
	uint32_t port;
	char devname[8];
	void *handle = NULL;
        uint32_t xpai_lh_ports;

        xpai_lh_ports = strcmp(getenv("SYS_BOARD_TYPE"), "BP") == 0 ?
                             XPAI_LH_NUMBER_OF_PORTS : XPAI_LH_NUMBER_OF_PORTS_TRXM;

	for (port = 0; port < xpai_lh_ports; port++) {
		snprintf(devname, sizeof(devname), "ecp%d", port);
		handle = uio_open(devname);
		if (handle == (UIO_HANDLE_) - 1) {
			TPT_INFO(STR("Failed to open UIO device %s\n", devname));
			continue;
		}
		uio_close(handle);
		res = xpai_lh_port_init(port, client_ref);
		if (res != INIT_OK) {
			return res;
		}
	}
	return INIT_OK;
}

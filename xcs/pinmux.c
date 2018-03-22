/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
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
#include <pthread.h>
#include <stdbool.h>
#include "itc.h"
#include "rhd-pinmux-if.h"
#include "pinmux.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_libpinmux
#include "tpt_create.h"
#include "tpt.h"

#define CONN_ESTABLISH_TMO      1000


static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} pinmux_conn;

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_PINMUX_STRUCTS
};

static struct conn_establish_msg_numbers  pinmux_conn_messages = {
	PINMUX_CONN_ESTABLISH_REQ,
	PINMUX_CONN_ESTABLISH_CFM,
	PINMUX_CONN_ESTABLISH_REJ,
	PINMUX_CONN_DISCONNECT_REQ,
	PINMUX_CONN_DISCONNECT_CFM,
	PINMUX_CONN_DISCONNECT_REJ,
	PINMUX_CONN_MONITOR_FWD
};

static int initialized = 0;
static uint32_t client_ref = 0x999;
static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

static uint32_t pinmux_init(void);

/****
 *
 *      Function get_mbox
 *
 *****/
static int32_t get_mbox(itc_mbox_id_t *pinmux_mbox)
{
	union itc_msg *msg;
	uint32_t resp_msg[] = {1, ITC_LOCATE_DEFAULT_NO};
	/* client mailbox */
	if (itc_current_mbox() == ITC_NO_ID) {
		TPT_ERROR("Mailbox doesn't exist.");
		return 1;
	}

	/* server mailbox */
	itc_locate_async(RHD_PINMUX_MAILBOX, NULL, ITC_MY_MBOX);
	msg = itc_receive(resp_msg, ITC_NO_TMO, ITC_FROM_ALL);
	*pinmux_mbox = itc_sender(msg);
	itc_free(&msg);
	TPT_TRACE(1, "Found pinmux server mailbox");

	return 0;
}

static uint32_t msg_check(union itc_msg *msg)
{
	if((msg->any_msg.connection_ref != client_ref)) {
		TPT_ERROR(STR("Client:Server replied with invalid ...._ref;"
		              "connection_ref: expected 0x%08x, received 0x%08x",
		              client_ref, msg->any_msg.connection_ref));
		itc_free(&msg);
		return 1;
	}
	return 0;
}

static void pin_str(uint32_t number_of_pins, const uint32_t pin[],
                    char *trace_str)
{
	char *tmp_str = NULL;
	int size = 0;
	tmp_str = trace_str;
	for(uint32_t i = 0; i < number_of_pins; i++) {
		size = snprintf(tmp_str, sizeof("999 "), "%d ", pin[i]);
		tmp_str = tmp_str + size;
	}
}


static pinmux_status_t trace_send_sig(union itc_msg *sendmsg)
{
	char *pinstr = NULL;
	uint32_t number_of_pins = 0;

	switch(sendmsg->msgno) {
	case RHD_PINMUX_RESERVE_REQ:
		number_of_pins = sendmsg->reserve_req.number_of_pins;
		break;
	case RHD_PINMUX_SET_FUNC_REQ:
		number_of_pins = sendmsg->set_func_req.number_of_pins;
		break;
	case RHD_PINMUX_SET_CFG_REQ:
		number_of_pins = sendmsg->set_cfg_req.number_of_pins;
		break;
	default:
		return PINMUX_STATUS_OTHER;
	}

	pinstr = calloc(number_of_pins, sizeof("999 "));
	if(pinstr == NULL) {
		TPT_ERROR(STR("calloc size %d failed",
		              number_of_pins * sizeof("999 ")));
		return PINMUX_STATUS_OTHER;
	}

	switch(sendmsg->msgno) {
	case RHD_PINMUX_RESERVE_REQ:
		pin_str(number_of_pins, sendmsg->reserve_req.pin, pinstr);

		TPT_SEND_SIG(sendmsg->msgno, pinmux_conn.server_mbox,
		             STR("RHD_PINMUX_RESERVE_REQ, number_of_pins:%u, "
		                 "pins:%s, destination: %u("RHD_PINMUX_MAILBOX")",
		                 number_of_pins, pinstr, pinmux_conn.server_mbox));

		break;
	case RHD_PINMUX_SET_FUNC_REQ:
		pin_str(number_of_pins, sendmsg->set_func_req.pin, pinstr);
		TPT_SEND_SIG(sendmsg->msgno, pinmux_conn.server_mbox,
		             STR("RHD_PINMUX_SET_FUNC_REQ, handle:0x%x,"
		                 " number_of_pins: %u, pins: %s",
		                 (unsigned int)sendmsg->set_func_req.handle,
		                 number_of_pins, pinstr));

		break;
	case RHD_PINMUX_SET_CFG_REQ:
		pin_str(number_of_pins, sendmsg->set_cfg_req.pin, pinstr);
		TPT_SEND_SIG(sendmsg->msgno, pinmux_conn.server_mbox,
		             STR("RHD_PINMUX_SET_CFG_REQ, handle:0x%x, "
		                 "number_of_pins: %u, pins: %s, "
		                 "cfg_type: %u, cfg_value: %u",
		                 (unsigned int)sendmsg->set_cfg_req.handle,
		                 number_of_pins, pinstr, sendmsg->set_cfg_req.
		                 cfg_type, sendmsg->set_cfg_req.cfg_value));
		break;
	default:
		return PINMUX_STATUS_OTHER;
	}
	free(pinstr);
	return PINMUX_STATUS_SUCCESS;
}
/****
 *
 *      Function pinmux_reserve
 *
 *****/
pinmux_status_t pinmux_reserve(uint32_t number_of_pins,
                               const uint32_t pin[],
                               pinmux_handle_t *handle)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2,
	                        RHD_PINMUX_RESERVE_CFM,
	                        RHD_PINMUX_RESERVE_REJ
	                       };
	int32_t ret = 0;

	if(pinmux_init()) {
		return PINMUX_STATUS_OTHER;
	}

	sendmsg = itc_alloc(offsetof(struct pinmux_reserve_req, pin) +
	                    number_of_pins * sizeof(uint32_t),
	                    RHD_PINMUX_RESERVE_REQ);

	sendmsg->reserve_req.connection_ref = pinmux_conn.server_ref;
	sendmsg->reserve_req.number_of_pins = number_of_pins;
	memcpy(sendmsg->reserve_req.pin, pin,
	       number_of_pins * sizeof(uint32_t));

	if(trace_send_sig(sendmsg) != PINMUX_STATUS_SUCCESS) {
		itc_free(&sendmsg);
		return PINMUX_STATUS_OTHER;
	}
	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
	                         pinmux_conn.server_mbox);

	if(msg_check(receivemsg)) {
		return PINMUX_STATUS_OTHER;
	}
	switch(receivemsg->msgno) {
	case RHD_PINMUX_RESERVE_CFM:
		*handle = receivemsg->reserve_cfm.handle;
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_RESERVE_CFM(handle: 0x%x) from: %u("
		                RHD_PINMUX_MAILBOX")",
		                (unsigned int)receivemsg->reserve_cfm.handle,
		                pinmux_conn.server_mbox));
		ret = PINMUX_STATUS_SUCCESS;
		break;
	case RHD_PINMUX_RESERVE_REJ:
		ret = receivemsg->reserve_rej.error_code;
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_RESERVE_REJ(Error code: %d) from: %u("
		                RHD_PINMUX_MAILBOX")",
		                receivemsg->reserve_rej.error_code,
		                pinmux_conn.server_mbox));
		break;

	}
	itc_free(&receivemsg);

	return ret;
}

/****
 *
 *      Function pinmux_unreserve
 *
 *****/
pinmux_status_t pinmux_unreserve(pinmux_handle_t handle)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2,
	                        RHD_PINMUX_UNRESERVE_CFM,
	                        RHD_PINMUX_UNRESERVE_REJ
	                       };
	int32_t ret = 0;

	if(pinmux_init()) {
		return PINMUX_STATUS_OTHER;
	}

	sendmsg = itc_alloc(sizeof(struct pinmux_unreserve_req),
	                    RHD_PINMUX_UNRESERVE_REQ);

	sendmsg->unreserve_req.connection_ref = pinmux_conn.server_ref;
	sendmsg->unreserve_req.handle = handle;

	TPT_SEND_SIG(sendmsg->msgno, pinmux_conn.server_mbox,
	             STR("RHD_PINMUX_UNRESERVE_REQ, handle:0x%x",
	                 (unsigned int)handle));

	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
	                         pinmux_conn.server_mbox);

	if(msg_check(receivemsg)) {
		return PINMUX_STATUS_OTHER;
	}

	switch(receivemsg->msgno) {
	case RHD_PINMUX_UNRESERVE_CFM:
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_UNRESERVE_CFM from: %u("
		                RHD_PINMUX_MAILBOX")",
		                pinmux_conn.server_mbox));

		ret = PINMUX_STATUS_SUCCESS;
		break;
	case RHD_PINMUX_UNRESERVE_REJ:
		ret = receivemsg->unreserve_rej.error_code;
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_UNRESERVE_REJ(Error code: %d) "
		                "from: %u ("RHD_PINMUX_MAILBOX")",
		                receivemsg->unreserve_rej.error_code,
		                pinmux_conn.server_mbox));
		break;

	}
	itc_free(&receivemsg);

	return ret;
}


/****
 *
 *      Function pinmux_set_func
 *
 *****/
pinmux_status_t pinmux_set_func(pinmux_handle_t handle,
                                uint32_t number_of_pins,
                                const uint32_t pin[],
                                pinmux_func_type_t func_type)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2,
	                        RHD_PINMUX_SET_FUNC_CFM,
	                        RHD_PINMUX_SET_FUNC_REJ
	                       };
	int32_t ret = 0;

	if(pinmux_init()) {
		return PINMUX_STATUS_OTHER;
	}

	sendmsg = itc_alloc(offsetof(struct pinmux_set_func_req, pin) +
	                    number_of_pins * sizeof(uint32_t),
	                    RHD_PINMUX_SET_FUNC_REQ);

	sendmsg->set_func_req.connection_ref = pinmux_conn.server_ref;
	sendmsg->set_func_req.handle = handle;
	sendmsg->set_func_req.number_of_pins = number_of_pins;
	sendmsg->set_func_req.func_type = func_type;
	memcpy(sendmsg->set_func_req.pin, pin,
	       number_of_pins * sizeof(uint32_t));

	if(trace_send_sig(sendmsg) != PINMUX_STATUS_SUCCESS) {
		itc_free(&sendmsg);
		return PINMUX_STATUS_OTHER;
	}

	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
	                         pinmux_conn.server_mbox);

	if(msg_check(receivemsg)) {
		return PINMUX_STATUS_OTHER;
	}

	switch(receivemsg->msgno) {
	case RHD_PINMUX_SET_FUNC_CFM:
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_SET_FUNC_CFM from: %u("
		                RHD_PINMUX_MAILBOX")",
		                pinmux_conn.server_mbox));
		ret = PINMUX_STATUS_SUCCESS;
		break;
	case RHD_PINMUX_SET_FUNC_REJ:
		ret = receivemsg->set_func_rej.error_code;
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_SET_FUNC_REJ(Error code: %d)"
		                " from: %u ("RHD_PINMUX_MAILBOX")",
		                receivemsg->set_func_rej.error_code,
		                pinmux_conn.server_mbox));
		break;

	}
	itc_free(&receivemsg);

	return ret;
}

/****
 *
 *      Function pinmux_get_func
 *
 *****/
pinmux_status_t pinmux_get_func(pinmux_handle_t handle,
                                uint32_t pin,
                                pinmux_func_type_t *func_type)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2,
	                        RHD_PINMUX_GET_FUNC_CFM,
	                        RHD_PINMUX_GET_FUNC_REJ
	                       };
	int32_t ret = 0;

	if(pinmux_init()) {
		return PINMUX_STATUS_OTHER;
	}

	sendmsg = itc_alloc(sizeof(struct pinmux_get_func_req),
	                    RHD_PINMUX_GET_FUNC_REQ);

	sendmsg->get_func_req.connection_ref = pinmux_conn.server_ref;
	sendmsg->get_func_req.handle = handle;
	sendmsg->get_func_req.pin = pin;

	TPT_SEND_SIG(sendmsg->msgno, pinmux_conn.server_mbox,
	             STR("RHD_PINMUX_GET_FUNC_REQ, handle:0x%x, pin: %u",
	                 (unsigned int)handle, pin));

	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
	                         pinmux_conn.server_mbox);

	if(msg_check(receivemsg)) {
		return PINMUX_STATUS_OTHER;
	}

	switch(receivemsg->msgno) {
	case RHD_PINMUX_GET_FUNC_CFM:
		*func_type = receivemsg->get_func_cfm.func_type;
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_GET_FUNC_CFM func_type: %u"
		                " from: %u ("RHD_PINMUX_MAILBOX")",
		                receivemsg->get_func_cfm.func_type,
		                pinmux_conn.server_mbox));
		ret = PINMUX_STATUS_SUCCESS;
		break;
	case RHD_PINMUX_GET_FUNC_REJ:
		ret = receivemsg->get_func_rej.error_code;
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_GET_FUNC_REJ(Error code: %d)"
		                " from: %u ("RHD_PINMUX_MAILBOX")",
		                receivemsg->get_func_rej.error_code,
		                pinmux_conn.server_mbox));
		break;

	}
	itc_free(&receivemsg);

	return ret;
}


/****
 *
 *      Function pinmux_set_cfg
 *
 *****/
pinmux_status_t pinmux_set_cfg(pinmux_handle_t handle,
                               uint32_t number_of_pins,
                               const uint32_t pin[],
                               pinmux_cfg_type_t cfg_type,
                               pinmux_cfg_value_t cfg_value)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2,
	                        RHD_PINMUX_SET_CFG_CFM,
	                        RHD_PINMUX_SET_CFG_REJ
	                       };
	int32_t ret = 0;

	if(pinmux_init()) {
		return PINMUX_STATUS_OTHER;
	}

	sendmsg = itc_alloc(offsetof(struct pinmux_set_cfg_req, pin) +
	                    number_of_pins * sizeof(uint32_t),
	                    RHD_PINMUX_SET_CFG_REQ);

	sendmsg->set_cfg_req.connection_ref = pinmux_conn.server_ref;
	sendmsg->set_cfg_req.handle = handle;
	sendmsg->set_cfg_req.number_of_pins = number_of_pins;
	sendmsg->set_cfg_req.cfg_type = cfg_type;
	sendmsg->set_cfg_req.cfg_value = cfg_value;
	memcpy(sendmsg->set_cfg_req.pin, pin,
	       number_of_pins * sizeof(uint32_t));

	if(trace_send_sig(sendmsg) != PINMUX_STATUS_SUCCESS) {
		itc_free(&sendmsg);
		return PINMUX_STATUS_OTHER;
	}

	itc_send(&sendmsg, pinmux_conn.server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO,
	                         pinmux_conn.server_mbox);

	if(msg_check(receivemsg)) {
		return PINMUX_STATUS_OTHER;
	}

	switch(receivemsg->msgno) {
	case RHD_PINMUX_SET_CFG_CFM:
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_SET_CFG_CFM from: %u ("
		                RHD_PINMUX_MAILBOX")",
		                pinmux_conn.server_mbox));
		ret = PINMUX_STATUS_SUCCESS;
		break;
	case RHD_PINMUX_SET_CFG_REJ:
		ret = receivemsg->set_cfg_rej.error_code;
		TPT_REC_SIG(receivemsg->msgno,
		            STR("RHD_PINMUX_SET_CFG_REJ(Error code: %d)"
		                " from: %u ("RHD_PINMUX_MAILBOX")",
		                receivemsg->set_cfg_rej.error_code,
		                pinmux_conn.server_mbox));
		break;

	}
	itc_free(&receivemsg);

	return ret;
}


/****
 *
 *      Function pinmux_init
 *
 *****/
static uint32_t pinmux_init(void)
{
	uint32_t requested_versions[] = {PINMUX_SERVER_VERSIONS};
	uint32_t procedure_ref = 0;
	int pthread_ret;
	uint32_t ret = 1;


	if(initialized) {
		return 0;
	}

	pthread_ret = pthread_mutex_lock(&lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		return 1;
	}

	if(initialized) {
		ret = 0;
		goto pinmux_init_end;
	}

	/* Find client and server mailboxes */
	if (get_mbox(&pinmux_conn.server_mbox) != 0) {
		TPT_ERROR("Client:Cannot find mailbox");
		goto pinmux_init_end;
	}

	/*Connect to the server*/
	ret = conn_establish(
	              /*input parameters*/
	              pinmux_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &pinmux_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &pinmux_conn.server_ref,
	              &pinmux_conn.selected_version);
	if (ret != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Connection establish failed with reason:0x%x",
		              ret));
		ret = 1;
		goto pinmux_init_end;
	}

	TPT_TRACE(1, STR("mailbox id for pinmux: %u"
	                 "server connection ref:%u"
	                 "selected version: %u",
	                 pinmux_conn.server_mbox,
	                 pinmux_conn.server_ref,
	                 pinmux_conn.selected_version));

	initialized = 1;
	ret = 0;
pinmux_init_end:
	pthread_ret = pthread_mutex_unlock(&lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
		ret = 1;

	}
	return ret;
}

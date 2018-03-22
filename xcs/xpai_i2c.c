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
#include <pthread.h>
#include <itc.h>
#include "rhd-i2c-if.h"
#include "rhd-common.h"
#include "xpai_hdr_i2c_if.h"
#include "common.h"
#include "log_tpt.h"
#include "conn-establish-helper.h"

#define MAX_I2C_PORTS           3
#ifndef __MACHINE_ZYNQMP_MIMO
#define XPAI_I2C_PORTS_TO_START 2
#else
#define XPAI_I2C_PORTS_TO_START_BP 3
#define XPAI_I2C_PORTS_TO_START_TRXM 1
#endif

static struct server_info i2c_conn[MAX_I2C_PORTS];
static int initialized[MAX_I2C_PORTS];
static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

union itc_msg {
	uint32_t msgno;
	RHD_I2C_STRUCTS
};

static struct conn_establish_msg_numbers  i2c_conn_messages = {
	I2C_CONN_ESTABLISH_REQ,
	I2C_CONN_ESTABLISH_CFM,
	I2C_CONN_ESTABLISH_REJ,
	I2C_CONN_DISCONNECT_REQ,
	I2C_CONN_DISCONNECT_CFM,
	I2C_CONN_DISCONNECT_REJ,
	I2C_CONN_MONITOR_FWD
};

/* Fix me: in this file, the log should be replaced with united trace solution */

/****
 *
 *      Function get_mbox
 *
 *****/
static int32_t get_mbox(itc_mbox_id_t *i2c_mbox, uint32_t port_id)
{
	char mailbox[sizeof(RHD_I2C_MAILBOX) + sizeof("-0xFFFFFFFF")];

	if (snprintf(mailbox, sizeof(mailbox), "%s_%d", RHD_I2C_MAILBOX,
	             port_id) < 0) {
		TPT_ERROR("snprintf error");
		return INIT_OTHER_ERROR;
	}

	return xpai_locate_mbox(mailbox, i2c_mbox);
}

static int32_t open_device(uint32_t port_id)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2, RHD_I2C_OPEN_CFM, RHD_I2C_OPEN_REJ};
	int32_t ret = 0;

	sendmsg = itc_alloc(sizeof(struct i2c_open_req),
	                    RHD_I2C_OPEN_REQ);

	sendmsg->open_req.connection_ref = i2c_conn[port_id].server_ref;
	TPT_SEND_SIG(sendmsg->msgno, i2c_conn[port_id].server_mbox, "RHD_I2C_OPEN_REQ");
	itc_send(&sendmsg, i2c_conn[port_id].server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO, i2c_conn[port_id].server_mbox);
	switch(receivemsg->msgno) {
	case RHD_I2C_OPEN_CFM:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_OPEN_CFM from: 0x%x",
		          itc_sender(receivemsg)));
		ret = 0;
		break;
	case RHD_I2C_OPEN_REJ:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_OPEN_REJ from: 0x%x",
		          itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	default:
		TPT_ERROR(STR("Unexpected msg (msgno=0x%X) received from 0x%x",
		        receivemsg->msgno, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;

	}
	itc_free(&receivemsg);

	return ret;
}
/****
 *
 *      Function XPAI_I2CWritePort
 *
 *****/
S32 XPAI_I2CWritePort(U32 portId, U32 address, U8 *buffer, U32 length)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2, RHD_I2C_WRITE_CFM, RHD_I2C_WRITE_REJ};
	int32_t ret = 0;

	if (portId >= MAX_I2C_PORTS) {
		TPT_ERROR(STR("I2C port%u is not valid", portId));
		return XPAI_I2C_WRONG_PORT;
	}

	if (!initialized[portId]) {
		TPT_ERROR(STR("I2C port%u is not initialized", portId));
		return XPAI_I2C_OTHER_ERROR;
	}

	if ((buffer == NULL) || (length == 0)) {
		TPT_ERROR(STR("Invalid data buffer or length(%u) is used", length));
		return XPAI_I2C_OTHER_ERROR;
	}

	ret = open_device(portId);
	if(ret) {
		return ret;
	}

	sendmsg = itc_alloc(offsetof(struct i2c_write_req, data) + length,
	                    RHD_I2C_WRITE_REQ);
	sendmsg->write_req.connection_ref = i2c_conn[portId].server_ref;
	sendmsg->write_req.address = address;
	sendmsg->write_req.length = length;
	memcpy(sendmsg->write_req.data, buffer, length);
	TPT_SEND_SIG(sendmsg->msgno, i2c_conn[portId].server_mbox,
			STR("RHD_I2C_WRITE_REQ, portId:%u, address:%u, length:%u",
				          portId, address, length));
	itc_send(&sendmsg, i2c_conn[portId].server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO, i2c_conn[portId].server_mbox);
	switch(receivemsg->msgno) {
	case RHD_I2C_WRITE_CFM:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_WRITE_CFM(length: %d) from: 0x%x",
		          receivemsg->write_cfm.length, itc_sender(receivemsg)));
		ret = receivemsg->write_cfm.length;
		break;
	case RHD_I2C_WRITE_REJ:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_WRITE_REJ(Error code: %d) from: 0x%x",
		          receivemsg->write_rej.error_code, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	default:
		TPT_ERROR(STR("Unexpected msg (msgno=0x%X) received from 0x%x",
		        receivemsg->msgno, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	}
	itc_free(&receivemsg);

	return ret;

}
/****
 *
 *      Function XPAI_I2CWriteSubPort
 *
 *****/
S32 XPAI_I2CWriteSubPort(U32 portId,
                         U32 address,
                         U8 *buffer,
                         U32 length,
                         U32 subAddress)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2, RHD_I2C_WRITE_SUB_CFM, RHD_I2C_WRITE_SUB_REJ};
	int32_t ret = 0;

	if (portId >= MAX_I2C_PORTS) {
		TPT_ERROR(STR("I2C port%u is not valid", portId));
		return XPAI_I2C_WRONG_PORT;
	}

	if (!initialized[portId]) {
		TPT_ERROR(STR("I2C port%u is not initialized", portId));
		return XPAI_I2C_OTHER_ERROR;
	}

	if ((buffer == NULL) || (length == 0)) {
		TPT_ERROR(STR("Invalid data buffer or length(%u) is used", length));
		return XPAI_I2C_OTHER_ERROR;
	}

	ret = open_device(portId);
	if(ret) {
		return ret;
	}

	sendmsg = itc_alloc(offsetof(struct i2c_write_sub_req, data) + length,
	                    RHD_I2C_WRITE_SUB_REQ);

	sendmsg->write_sub_req.connection_ref = i2c_conn[portId].server_ref;
	sendmsg->write_sub_req.address = address;
	sendmsg->write_sub_req.length = length;
	sendmsg->write_sub_req.sub_address = subAddress;
	sendmsg->write_sub_req.sub_address_size = 1;
	memcpy(sendmsg->write_sub_req.data, buffer, length);
	TPT_SEND_SIG(sendmsg->msgno, i2c_conn[portId].server_mbox,
	             STR("RHD_I2C_WRITE_SUB_REQ, portId:%u, address:%u, "
	             "length:%u, subAddress:%u", portId, address, length, subAddress));
	itc_send(&sendmsg, i2c_conn[portId].server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO, i2c_conn[portId].server_mbox);
	switch(receivemsg->msgno) {
	case RHD_I2C_WRITE_SUB_CFM:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_WRITE_SUB_CFM(length: %d) from: 0x%x",
		          receivemsg->write_sub_cfm.length, itc_sender(receivemsg)));
		ret = receivemsg->write_sub_cfm.length;
		break;
	case RHD_I2C_WRITE_SUB_REJ:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_WRITE_SUB_REJ(Error code: %d) from: 0x%x",
		          receivemsg->write_sub_rej.error_code, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	default:
		TPT_ERROR(STR("Unexpected msg (msgno=0x%X) received from 0x%x",
		        receivemsg->msgno, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	}
	itc_free(&receivemsg);

	return ret;

}
/****
 *
 *      Function XPAI_I2CReadPort
 *
 *****/
S32 XPAI_I2CReadPort(U32 portId, U32 address, U8 *buffer, U32 length)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2, RHD_I2C_READ_CFM, RHD_I2C_READ_REJ};
	int32_t ret = 0;

	if (portId >= MAX_I2C_PORTS) {
		TPT_ERROR(STR("I2C port%u is not valid", portId));
		return XPAI_I2C_WRONG_PORT;
	}

	if (!initialized[portId]) {
		TPT_ERROR(STR("I2C port%u is not initialized", portId));
		return XPAI_I2C_OTHER_ERROR;
	}

	if ((buffer == NULL) || (length == 0)) {
		TPT_ERROR(STR("Invalid data buffer or length(%u) is used", length));
		return XPAI_I2C_OTHER_ERROR;
	}

	ret = open_device(portId);
	if(ret) {
		return ret;
	}

	sendmsg = itc_alloc(sizeof(struct i2c_read_req),
	                    RHD_I2C_READ_REQ);

	sendmsg->read_req.connection_ref = i2c_conn[portId].server_ref;
	sendmsg->read_req.address = address;
	sendmsg->read_req.length = length;

	TPT_SEND_SIG(sendmsg->msgno, i2c_conn[portId].server_mbox,
			STR("RHD_I2C_READ_REQ, portId:%u, address:%u, length:%u",
	          portId, address, length));
	itc_send(&sendmsg, i2c_conn[portId].server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO, i2c_conn[portId].server_mbox);

	switch(receivemsg->msgno) {
	case RHD_I2C_READ_CFM:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_READ_CFM(length: %d) from: 0x%x",
		          receivemsg->read_cfm.length, itc_sender(receivemsg)));
		memcpy(buffer, receivemsg->read_cfm.data, length);
		ret = receivemsg->read_cfm.length;
		break;
	case RHD_I2C_READ_REJ:
		TPT_REC_SIG(receivemsg->msgno, STR("RHD_I2C_READ_REJ(Error code: %d) from: 0x%x",
		          receivemsg->read_rej.error_code, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	default:
		TPT_ERROR(STR("Unexpected msg (msgno=0x%X) received from 0x%x",
		        receivemsg->msgno, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	}
	itc_free(&receivemsg);

	return ret;

}

/****
 *
 *      Function XPAI_I2CReadSubPort
 *
 *****/
S32 XPAI_I2CReadSubPort(U32 portId,
                        U32 address,
                        U8 *buffer,
                        U32 length,
                        U32 subAddress)
{
	union itc_msg *sendmsg = NULL;
	union itc_msg *receivemsg = NULL;
	uint32_t rx_filter[] = {2, RHD_I2C_READ_SUB_CFM, RHD_I2C_READ_SUB_REJ};
	int32_t ret = 0;

	if (portId >= MAX_I2C_PORTS) {
		TPT_ERROR(STR("I2C port%u is not valid", portId));
		return XPAI_I2C_WRONG_PORT;
	}

	if (!initialized[portId]) {
		TPT_ERROR(STR("I2C port%u is not initialized", portId));
		return XPAI_I2C_OTHER_ERROR;
	}

	if ((buffer == NULL) || (length == 0)) {
		TPT_ERROR(STR("Invalid data buffer or length(%u) is used", length));
		return XPAI_I2C_OTHER_ERROR;
	}

	ret = open_device(portId);
	if(ret) {
		return ret;
	}

	sendmsg = itc_alloc(sizeof(struct i2c_read_sub_req),
	                    RHD_I2C_READ_SUB_REQ);

	sendmsg->read_sub_req.connection_ref = i2c_conn[portId].server_ref;
	sendmsg->read_sub_req.address = address;
	sendmsg->read_sub_req.length = length;
	sendmsg->read_sub_req.sub_address = subAddress;
	sendmsg->read_sub_req.sub_address_size = 1;

	TPT_SEND_SIG(sendmsg->msgno, i2c_conn[portId].server_mbox,
			STR("RHD_I2C_READ_SUB_REQ, portId:%u, address:%u, "
	          "length:%u, subAddress: %u",
	          portId, address, length, subAddress));
	itc_send(&sendmsg, i2c_conn[portId].server_mbox, ITC_MY_MBOX);

	receivemsg = itc_receive(rx_filter, ITC_NO_TMO, i2c_conn[portId].server_mbox);

	switch(receivemsg->msgno) {
	case RHD_I2C_READ_SUB_CFM:
		TPT_REC_SIG(receivemsg->msgno,
				STR("RHD_I2C_READ_SUB_CFM(length: %d) from: 0x%x",
		          receivemsg->read_sub_cfm.length, itc_sender(receivemsg)));
		memcpy(buffer, receivemsg->read_sub_cfm.data, length);
		ret = receivemsg->read_sub_cfm.length;
		break;
	case RHD_I2C_READ_SUB_REJ:
		TPT_REC_SIG(receivemsg->msgno,
				STR("RHD_I2C_READ_SUB_REJ(Error code: %d) from: 0x%x",
		          receivemsg->read_sub_rej.error_code, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	default:
		TPT_ERROR(STR("Unexpected msg (msgno=0x%X) received from 0x%x",
		        receivemsg->msgno, itc_sender(receivemsg)));
		ret = XPAI_I2C_OTHER_ERROR;
		break;
	}
	itc_free(&receivemsg);

	return ret;

}


/****
 *
 *      Function i2c_init
 *
 *****/
int32_t xpai_i2c_port_init(uint32_t portId, uint32_t client_ref)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {I2C_SERVER_VERSIONS};
	int      pthread_ret = 1;
	uint32_t res = INIT_OTHER_ERROR;

	if (portId >= MAX_I2C_PORTS) {
		TPT_ERROR(STR("I2C port%u is not valid", portId));
		res = INIT_I2C_PORT_NOK;
		goto i2c_init_end;
	}

	if (initialized[portId]) {
		res = INIT_OK;
		goto i2c_init_end;
	}

	pthread_ret = pthread_mutex_lock(&lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		        pthread_ret));
		goto i2c_init_end;
	}

	if(initialized[portId]) {
		res = INIT_OK;
		goto i2c_init_end;
	}

	/*call connection establish*/
	res = get_mbox(&i2c_conn[portId].server_mbox, portId);
	if (res != INIT_OK) {
		TPT_ERROR(STR("Client: Cannot find mailbox for port %u", portId));
		res = INIT_OTHER_ERROR;
		goto i2c_init_end;
	}

	res = conn_establish(
	              /*input parameters*/
	              i2c_conn[portId].server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &i2c_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &i2c_conn[portId].server_ref,
	              &i2c_conn[portId].selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed on port %u \
                         (reason:0x%08x)", portId, res));
		res = INIT_SERVER_NOK;
		goto i2c_init_end;
	}

	TPT_TRACE(1, STR("mailbox id for I2C%d: %u\n \
                   server connection ref:%u\n \
                   selected version: %u",
	          portId,
	          i2c_conn[portId].server_mbox,
	          i2c_conn[portId].server_ref,
	          i2c_conn[portId].selected_version));

	initialized[portId] = 1;
	res = INIT_OK;

i2c_init_end:
	if (pthread_ret == 0) {
		pthread_ret = pthread_mutex_unlock(&lock);
		if(pthread_ret != 0) {
			TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
			        pthread_ret));
			res = INIT_OTHER_ERROR;

		}
	}

	return res;
}

int32_t xpai_i2c_init(uint32_t client_ref)
{
	int i;
	int32_t res;
        int ports_to_start;

        ports_to_start = strcmp(getenv("SYS_BOARD_TYPE"), "BP") == 0 ?
                            XPAI_I2C_PORTS_TO_START_BP : XPAI_I2C_PORTS_TO_START_TRXM;

        for (i = 0; i < ports_to_start; i++) {
		res = xpai_i2c_port_init(i, client_ref);
		if (res  != INIT_OK) {
			return res;
		}
	}

	return INIT_OK;
}

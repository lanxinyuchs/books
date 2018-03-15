/******************************************************************************
 * Copyright (c) Ericsson AB 2014 All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and dissemination to the receivers employees shall
 * only be made on a strict need to know basis.
 */

#include <itc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hwlog.h"
#include "signals.h"

#define ERR(fmt, ...) do {                                \
	fprintf(stderr, "%s:%d %s() Error: " fmt "\n",        \
	        __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
} while (0)

#define MAILBOX_SIZE 1

union itc_msg {
	struct {
		uint32_t msgno;
		uint32_t procedure_reference;
		uint32_t connection_reference;
	};
	HWLOG_SIGNAL_STRUCTS;
};

itc_mbox_id_t init_itc(uint32_t *connection_reference)
{
	uint32_t procedure_reference_ret, connection_reference_ret;
	uint32_t protocol_revision_ret;
	uint32_t revisions[] = { 1, 0 };
	itc_mbox_id_t hwld_mbox = ITC_NO_ID;
	int ret;

	hwld_mbox = itc_locate(HWLOG_MAILBOX);
	if (hwld_mbox == ITC_NO_ID) {
		ERR("Failed to locate mailbox %s\n", HWLOG_MAILBOX);
		return ITC_NO_ID;
	}

	send_connect_req(hwld_mbox, 0xbeef, 0, 1, revisions, ITC_MY_MBOX);

	ret = get_connect_reply(hwld_mbox, &procedure_reference_ret,
	                        &connection_reference_ret,
	                        &protocol_revision_ret, ITC_NO_TMO);

	if (ret == REJECT) {
		ERR("Connection rejected");
		return ITC_NO_ID;
	}

	if (protocol_revision_ret != 1) {
		ERR("Expected protocol revision 1, got %d", protocol_revision_ret);
		return ITC_NO_ID;
	}

	if (procedure_reference_ret != 0xbeef) {
		ERR("Expected procedure_reference 0xbeef, got %#x\n",
		    procedure_reference_ret);
		return ITC_NO_ID;
	}

	*connection_reference = connection_reference_ret;
	return hwld_mbox;
}

void send_connect_req(itc_mbox_id_t dst,
                      uint32_t procedure_reference,
                      uint32_t connection_reference,
                      uint32_t nbr_of_supported_protocol_revisions,
                      uint32_t *protocol_revisions,
                      itc_mbox_id_t src)
{
	union itc_msg *msg;
	struct hwlog_connect_req *req;
	uint32_t *revs = protocol_revisions;
	uint32_t *revp;
	int cnt = 0;
	size_t struct_size;

	while (*revs++) cnt++;
	struct_size = sizeof(struct hwlog_connect_req) +
	              (cnt - 1) * sizeof(uint32_t);

	msg = itc_alloc(struct_size, HWLOG_CONNECT_REQ);
	req = &msg->hwlog_connect_req;

	msg->procedure_reference = procedure_reference;
	msg->connection_reference = connection_reference;
	req->nbr_of_supported_protocol_revisions =
	    nbr_of_supported_protocol_revisions;

	revp = &req->protocol_revision;
	for (int i = 0; i < cnt; i++)
		*revp++ = *protocol_revisions++;

	itc_send(&msg, dst, src);
}

void send_logread_req(itc_mbox_id_t dst,
                      uint32_t procedure_reference,
                      uint32_t connection_reference,
	                  uint32_t data_per_ack,
                      itc_mbox_id_t src)
{
	union itc_msg *msg = itc_alloc(sizeof(struct hwlog_logread_req),
	                               HWLOG_LOGREAD_REQ);
	struct hwlog_logread_req *req = &msg->hwlog_logread_req;

	msg->procedure_reference = procedure_reference;
	msg->connection_reference = connection_reference;
	req->data_per_ack = data_per_ack;

	itc_send(&msg, dst, src);
}

void send_write_req(itc_mbox_id_t dst,
                    uint32_t procedure_reference,
                    uint32_t connection_reference,
                    char *id,
                    uint32_t filter,
                    uint32_t filter_on_msg,
                    char *text,
                    itc_mbox_id_t src)
{
	union itc_msg *msg = itc_alloc(sizeof(struct hwlog_write_req),
	                               HWLOG_WRITE_REQ);
	struct hwlog_write_req *req = &msg->hwlog_write_req;

	msg->procedure_reference = procedure_reference;
	msg->connection_reference = connection_reference;
	strncpy(req->id, id, 4);
	req->filter = filter;
	req->filter_on_msg = filter_on_msg;
	strncpy(req->msg, text, HWLI_MESSAGE_SZ);

	itc_send(&msg, dst, src);
}

void send_logerase_req(itc_mbox_id_t dst,
                       uint32_t procedure_reference,
                       uint32_t connection_reference,
                       itc_mbox_id_t src)
{
	union itc_msg *msg = itc_alloc(sizeof(struct hwlog_logerase_req),
	                               HWLOG_LOGERASE_REQ);

	msg->procedure_reference = procedure_reference;
	msg->connection_reference = connection_reference;

	itc_send(&msg, dst, src);
}

void send_logread_ack(itc_mbox_id_t dst,
                      uint32_t procedure_reference,
                      uint32_t connection_reference,
                      int32_t sequence_number,
                      itc_mbox_id_t src)
{
	union itc_msg *msg = itc_alloc(sizeof(struct hwlog_logread_ack),
	                               HWLOG_LOGREAD_ACK);
	struct hwlog_logread_ack *ack = &msg->hwlog_logread_ack;

	msg->procedure_reference = procedure_reference;
	msg->connection_reference = connection_reference;
	ack->sequence_number = sequence_number;

	itc_send(&msg, dst, src);
}

int get_connect_reply(itc_mbox_id_t src,
                      uint32_t *procedure_reference,
                      uint32_t *connection_reference,
                      uint32_t *protocol_revision,
                      int32_t timeout)
{
	uint32_t filter[3] = { 2, HWLOG_CONNECT_CFM, HWLOG_CONNECT_REJ };
	union itc_msg *msg = itc_receive(filter, timeout, src);
	struct hwlog_connect_cfm *cfm = &msg->hwlog_connect_cfm;
	int status = REJECT;

	if (!msg)
		return TIMEOUT;

	if (msg->msgno == HWLOG_CONNECT_REJ)
		goto exit;

	*procedure_reference = msg->procedure_reference;
	*connection_reference = msg->connection_reference;
	*protocol_revision = cfm->protocol_revision;
	status = CONFIRM;

exit:
	itc_free(&msg);
	return status;
}

int get_logread_reply(itc_mbox_id_t src,
                    uint32_t *procedure_reference,
                    uint32_t *connection_reference,
                    uint32_t *len,
                    int32_t timeout)
{
	uint32_t filter[3] = { 2, HWLOG_LOGREAD_CFM, HWLOG_LOGREAD_REJ };
	union itc_msg *msg = itc_receive(filter, timeout, src);
	struct hwlog_logread_cfm *cfm = &msg->hwlog_logread_cfm;
	int status = REJECT;

	if (!msg)
		return TIMEOUT;

	if (msg->msgno == HWLOG_LOGREAD_REJ)
		goto exit;

	*procedure_reference = msg->procedure_reference;
	*connection_reference = msg->connection_reference;
	*len = cfm->len;
	status = CONFIRM;

exit:
	itc_free(&msg);
	return status;
}

int get_write_reply(itc_mbox_id_t src,
                    uint32_t *procedure_reference,
                    uint32_t *connection_reference,
                    int32_t timeout)
{
	uint32_t filter[3] = { 2, HWLOG_WRITE_CFM, HWLOG_WRITE_REJ };
	union itc_msg *msg = itc_receive(filter, timeout, src);
	int status = REJECT;

	if (!msg)
		return TIMEOUT;

	if (msg->msgno == HWLOG_WRITE_REJ)
		goto exit;

	*procedure_reference = msg->procedure_reference;
	*connection_reference = msg->connection_reference;
	status = CONFIRM;

exit:
	itc_free(&msg);
	return status;
}

int get_logerase_reply(itc_mbox_id_t src,
                       uint32_t *procedure_reference,
                       uint32_t *connection_reference,
                       int32_t timeout)
{
	uint32_t filter[3] = { 2, HWLOG_LOGERASE_CFM, HWLOG_LOGERASE_REJ };
	union itc_msg *msg = itc_receive(filter, timeout, src);
	int status = REJECT;

	if (!msg)
		return TIMEOUT;

	if (msg->msgno == HWLOG_LOGERASE_REJ)
		goto exit;

	*procedure_reference = msg->procedure_reference;
	*connection_reference = msg->connection_reference;
	status = CONFIRM;

exit:
	itc_free(&msg);
	return status;
}

int get_logread_data(itc_mbox_id_t src,
                     uint32_t *procedure_reference,
                     uint32_t *connection_reference,
                     int32_t  *sequence_number,
                     struct hwli_entry *entry,
                     int32_t timeout)
{
	uint32_t filter[2] = { 1, HWLOG_LOGREAD_DATA };
	union itc_msg *msg = itc_receive(filter, timeout, src);
	struct hwlog_logread_data *data = &msg->hwlog_logread_data;

	if (!msg)
		return TIMEOUT;

	*procedure_reference = msg->procedure_reference;
	*connection_reference = msg->connection_reference;
	*sequence_number = data->sequence_number;
	*entry = data->entry;

	itc_free(&msg);
	return CONFIRM;
}

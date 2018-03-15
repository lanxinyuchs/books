#include <itc.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/un.h>
#include <errno.h>

#include "hwlog.h"
#include "hwli.h"
#include "log.h"

union itc_msg {
	struct {
		uint32_t msgno;
		uint32_t procedure_reference;
		uint32_t connection_reference;
	};
	HWLOG_SIGNAL_STRUCTS;
};

static itc_mbox_id_t _hwld_mbox = ITC_NO_ID;
static uint32_t _connection_reference;

static int connect()
{
	union itc_msg *itc_msg, *reply;
	struct hwlog_connect_req *req;
	_hwld_mbox = itc_locate(HWLOG_MAILBOX);

	if (_hwld_mbox == ITC_NO_ID)
		return 0;

	itc_msg = itc_alloc(sizeof(struct hwlog_connect_req),
	                    HWLOG_CONNECT_REQ);

	req = &itc_msg->hwlog_connect_req;
	req->nbr_of_supported_protocol_revisions = 1;
	req->protocol_revision = HWLOG_PROTOCOL_REVISION;
	itc_send(&itc_msg, _hwld_mbox, ITC_MY_MBOX);
	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, _hwld_mbox);

	if (reply->msgno == HWLOG_CONNECT_CFM) {
		_connection_reference = reply->connection_reference;
		itc_free(&reply);
		return 1;
	}

	itc_free(&reply);
	return 0;
}

int hwli_write(char *id, uint32_t filter, int filter_on_msg, char *msg)
{
	union itc_msg *itc_msg, *reply;
	struct hwlog_write_req *req;
	int i, len;
	int status = HWLI_ERROR;

	if (strlen(id) > HWLI_ID_SZ) {
		log_info("HWLI: logid (%s) too long", id);
		goto exit;
	}

	len = strlen(msg);
	if (len < HWLI_MESSAGE_SZ) {
		for (i = 0; i < len; i++) {
			if (msg[i] < 0x20 || msg[i] > 0x7e) {
				log_info("HWLI: Text must contain only printable characters");
				goto exit;
			}
		}
	} else {
		log_info("HWLI: Error Text too long");
		goto exit;
	}


	if (_hwld_mbox == ITC_NO_ID) {
		if (!connect())
			goto exit;
	}

	itc_msg = itc_alloc(sizeof(struct hwlog_write_req),
	                    HWLOG_WRITE_REQ);

	req = &itc_msg->hwlog_write_req;
	strcpy(req->id, id);
	strcpy(req->msg, msg);
	req->filter_on_msg = filter_on_msg;
	req->filter = filter;
	req->connection_reference = _connection_reference;

	itc_send(&itc_msg, _hwld_mbox, ITC_MY_MBOX);
	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, _hwld_mbox);

	if (reply->msgno == HWLOG_WRITE_CFM) {
		status = HWLI_SUCCESS;
	} else if (reply->msgno == HWLOG_WRITE_REJ) {
		status = (int) reply->hwlog_write_rej.error_code;
	}
	itc_free(&reply);

exit:
	return status;
}

int hwli_readlog(struct hwli_entry **entries, uint32_t *size)
{
	union itc_msg *msg, *ack_msg, *reply = NULL;
	struct hwli_entry * tmp = NULL;
	int32_t len, bufsize, ack_nbr;
	int status = HWLI_ERROR;

	if (_hwld_mbox == ITC_NO_ID) {
		if (!connect())
			goto exit;
	}

	msg = itc_alloc(sizeof(struct hwlog_logread_req),
					HWLOG_LOGREAD_REQ);
	msg->connection_reference = _connection_reference;
	msg->hwlog_logread_req.data_per_ack = 0; /* Only ack last entry */
	itc_send(&msg, _hwld_mbox, ITC_MY_MBOX);
	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, _hwld_mbox);

	if (reply->msgno != HWLOG_LOGREAD_CFM)
		goto exit;

	len = reply->hwlog_logread_cfm.len;
	bufsize = len * sizeof(struct hwli_entry);
	tmp = malloc(bufsize);

	/* For completeness; if malloc() fails on Linux we are hosed */
	if (!tmp) {
		log_info("HWLI: Failed to malloc, aborting..");
		ack_msg = itc_alloc(sizeof(struct hwlog_logread_ack),
		                    HWLOG_LOGREAD_ACK);
		ack_msg->connection_reference = _connection_reference;
		ack_msg->hwlog_logread_ack.sequence_number = HWLOG_LOGREAD_ABORT;
		itc_send(&ack_msg, _hwld_mbox, ITC_MY_MBOX);
		goto exit;
	}

	for (int i = 0; i < len; i++) {
		itc_free(&reply);
		reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, _hwld_mbox);
		if (reply->msgno != HWLOG_LOGREAD_DATA)
			goto exit;

		tmp[i] = reply->hwlog_logread_data.entry;

		/* Only ack the last entry */
		ack_nbr = reply->hwlog_logread_data.sequence_number;
	};

	if (len > 0) {
		ack_msg = itc_alloc(sizeof(struct hwlog_logread_ack),
		                    HWLOG_LOGREAD_ACK);
		ack_msg->connection_reference = _connection_reference;
		ack_msg->hwlog_logread_ack.sequence_number = ack_nbr;
		itc_send(&ack_msg, _hwld_mbox, ITC_MY_MBOX);
	}

	*entries = tmp;
	*size = len;
	status = HWLI_SUCCESS;

exit:
	if (reply) itc_free(&reply);
	if (status != HWLI_SUCCESS && tmp) free(tmp);
	return status;
}

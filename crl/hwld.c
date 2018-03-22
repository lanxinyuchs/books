/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <itc.h>
#include <mtd/mtd-user.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <signal.h>

#include "hwlog.h"
#include "hwli.h"
#include "hwld_io.h"
#include "log.h"

/*
 * Log entry format in flash
 *      1            12         3          1            111
 * ! adm state ! date time ! log id ! filter value ! free text !
 *
 * Note that this implementation doesn't fill in or rely on the filter value
 * field.
 */

#define FLASH_DEVICE         "/dev/mtd"
#define DEVICE_ENV           "sys_hwlog_dev"

/* possible values for hwlog_entry.adm_state */
#define ADM_STATE_DELETED    (0x00)
#define ADM_STATE_WRITTEN    (0x3f)
#define ADM_STATE_UNDEFINED  (0x7f)
#define ADM_STATE_FREE       (0xff)

#define MAILBOX_SIZE         1

#define NBR_CONNECTIONS      20

#define ACK_TIMEOUT_MS       10000

#define DATE_SIZE            12
#define LOGID_SIZE           3
#define FREETEXT_SIZE        111

/* special logid codes */
#define NEW_HW_CONFIG        "000"
#define NEW_SW_CONFIG        "003"
#define POWER_ON             "004"
#define RESTART_COLD_W_TEST  "005"

#define EXIT_SIGNAL          0xdeadbeef

union itc_msg {
	struct {
		uint32_t msgno;
		uint32_t procedure_reference;
		uint32_t connection_reference;
	};
	HWLOG_SIGNAL_STRUCTS;
};

/* server global state */
static mtd_info_t mtd_info;
static int mtd_fd;
static hwld_io *io;

static itc_mbox_id_t _hwld_mbox = ITC_NO_ID;
static itc_mbox_id_t *connections;
static int max_connections = NBR_CONNECTIONS;
static struct hwlog_entry sw_cfg_entry;
static int sw_cfg_entry_cached;

/* index to next entry to write */
static int write_idx;

struct hwlog_entry {
	char adm_state;
	char date[DATE_SIZE];
	char logid[LOGID_SIZE];
	char filter_value;
	char freetext[FREETEXT_SIZE];
};

static void get_datetime(char *date)
{
	time_t rawtime;
	struct tm * timeinfo;

	time(&rawtime);
	timeinfo = gmtime(&rawtime);
	strftime(date, DATE_SIZE + 1, "%y%m%d%H%M%S", timeinfo);
}

static int get_free_entry(void)
{
	char hwlog_content[mtd_info.size];
	int idx = 0;

	if (lseek(mtd_fd, 0, SEEK_SET) < 0) {
		log_info("Failed lseek during hwld init procedure");
		return -1;
	}
	if (read(mtd_fd, hwlog_content, sizeof(hwlog_content)) < 0) {
		log_info("Failed to read hwlog during hwld init procedure");
		return -1;
	}

	while (idx < mtd_info.size) {
		if (hwlog_content[idx] == ADM_STATE_FREE)
			break;

		idx += sizeof(struct hwlog_entry);
	}

	if (idx >= mtd_info.size) {
		log_info("get free entry failed, log full");
		write_idx = idx;
		return -1;
	}

	write_idx = idx;
	return 0;
}

static int hwl_eraselog()
{
	if (io->eraselog(mtd_fd, mtd_info.erasesize, mtd_info.size))
		return -1;

	write_idx = 0;
	return 0;
}

static int hwl_readlog(struct hwli_entry **entries, uint32_t *size)
{
	struct hwlog_entry e;
	struct hwli_entry *ie;
	struct hwli_entry *ientries = NULL;
	int status = 0;
	int cnt = 0;
	int idx;

	*entries = NULL;
	*size = 0;

	for (idx = 0;
	     idx < mtd_info.size;
	     idx += sizeof(struct hwlog_entry)) {

		lseek(mtd_fd, idx, SEEK_SET);
		if (read(mtd_fd, &e, sizeof(e)) < 0) {
			log_info("Failed to read entry in readlog, trying next entry");
			continue;
		}

		/* Not a valid entry, skip */
		if (e.adm_state == ADM_STATE_UNDEFINED)
			continue;

		if (e.adm_state == ADM_STATE_FREE)
			goto exit;

		cnt++;
		ientries = realloc(ientries, cnt * sizeof(struct hwli_entry));
		if (ientries == NULL) {
			log_info("readlog failed to allocate memory");
			status = -1;
			goto exit;
		}

		ie = ientries + cnt - 1;
		memcpy(ie->id, e.logid, LOGID_SIZE);
		ie->id[HWLI_ID_SZ-1] = 0;
		memcpy(ie->time, e.date, DATE_SIZE);
		ie->time[HWLI_TIMEDATE_SZ-1] = 0;
		memcpy(ie->msg, e.freetext, FREETEXT_SIZE);
		ie->msg[HWLI_MESSAGE_SZ-1] = 0;
	}

exit:
	if (status == 0) {
		*entries = ientries;
		*size = cnt;
	}
	else if (ientries) {
		free(ientries);
	}

	return status;
}

static int do_filter(union itc_msg * msg)
{
	struct hwli_entry *entries = NULL;
	struct hwlog_write_req *req = &msg->hwlog_write_req;
	uint32_t len, cnt = 0;
	int special_code = 0;
	int status = 1;

	if (strncmp(req->id, NEW_HW_CONFIG, LOGID_SIZE) == 0 ||
	    strncmp(req->id, NEW_SW_CONFIG, LOGID_SIZE) == 0) {

		special_code = 1;

		if (!req->filter_on_msg) {
			log_info("Logid %.*s called without filter on message, forcing",
			         LOGID_SIZE, req->id);
			req->filter_on_msg = 1;
		}

		if (req->filter != 1) {
			log_info("Logid %.*s called with filter %d (must be 1), forcing",
			         LOGID_SIZE, req->id, req->filter);
			req->filter = 1;
		}
	}
	else if (strncmp(req->id, POWER_ON, LOGID_SIZE) == 0 ||
	         strncmp(req->id, RESTART_COLD_W_TEST, LOGID_SIZE) == 0) {

		log_info("Logid %.*s called with filter, forcing no filter",
		         LOGID_SIZE, req->id);
		req->filter = 0;
	}

	if (req->filter == 0)
		goto exit;

	if (hwl_readlog(&entries, &len) < 0)
		goto exit;

	for (uint32_t i = 0; i < len; i++) {
		if (strncmp(req->id, entries[i].id, LOGID_SIZE) == 0) {
			if (req->filter_on_msg) {
				if (strncmp(req->msg, entries[i].msg, FREETEXT_SIZE) == 0)
					cnt++;
				else if (special_code)
					/* Counter is reset if the SW/HW config
					 * is not the same as in the last entry */
					cnt = 0;
			}
			else {
				cnt++;
			}
		}
		else if (!special_code &&
		        (strncmp(entries[i].id, NEW_HW_CONFIG, LOGID_SIZE) == 0 ||
		         strncmp(entries[i].id, NEW_SW_CONFIG, LOGID_SIZE) == 0 ||
		         strncmp(entries[i].id, POWER_ON, LOGID_SIZE) == 0 ||
		         strncmp(entries[i].id, RESTART_COLD_W_TEST, LOGID_SIZE) == 0))
		{
			/*
			 * These special codes reset the filter count, but not if they
			 * are the logid:s being filtered on. These special codes are
			 * global.
			 */
			cnt = 0;
		}
	}

	if (cnt >= req->filter)
		status = 0;

exit:
	if (entries) free(entries);
	return status;
}

static int write_validate(struct hwlog_entry *ep)
{
	struct hwlog_entry ev;

	if (lseek(mtd_fd, write_idx, SEEK_SET) < 0)
		return -2;

	/*
	 * In case of seek() failure in get free_entry() we might end up on a
	 * written entry, do sanity check if position is free.
	 */
	if (read(mtd_fd, &ev, sizeof(ev)) < 0 || ev.adm_state != ADM_STATE_FREE)
		return -1;

	if (lseek(mtd_fd, write_idx, SEEK_SET) < 0)
		return -2;

	if (write(mtd_fd, ep, sizeof(*ep)) < 0)
		return -1;

	if (lseek(mtd_fd, write_idx, SEEK_SET) < 0)
		return -2;

    if (read(mtd_fd, &ev, sizeof(ev)) < 0)
		return -1;

	if (strncmp(ev.date, ep->date, DATE_SIZE) == 0 &&
		strncmp(ev.logid, ep->logid, LOGID_SIZE) == 0 &&
		strncmp(ev.freetext, ep->freetext, FREETEXT_SIZE) == 0) {

		/* Validation successful, mark entry as written */
		if (lseek(mtd_fd, write_idx, SEEK_SET) < 0)
			return -2;

		ev.adm_state = ADM_STATE_WRITTEN;
		if (write(mtd_fd, &ev, sizeof(ev)) < 0)
			return -1;

		return 0;
	}

	return -1;
}

static int write_to_flash(struct hwlog_entry *ep)
{
	while (write_idx < mtd_info.size) {
		/*
		 * We try to write the entry, and validate that it was written.
		 * The validation is confirmed in the flash entry using
		 * ADM_STATE_WRITTEN.
		 *
		 * Since we *really* don't want the write to fail, we try another
		 * entry on failure if possible.
		 */
		int ret = write_validate(ep);

		if (ret == -2) {
			/*
			 * We can't mitigate a seek() failure. Leave start_idx and hope
			 * the failure is intermittent and that next write succeeds.
			 */
			log_info("Failed seek(), failing write");
			return -1;
		}
		else if  (ret == -1) {
			/* Try the next entry */
			log_info("Failed to write entry to %#x, trying next", write_idx);
			write_idx += sizeof(struct hwlog_entry);
		}
		else {
			/* Success */
			break;
		}
	}

	if (write_idx >= mtd_info.size) {
		log_info("HWlog became full, entry not written");
		return -1;
	}

	write_idx += sizeof(struct hwlog_entry);

	return 0;
}

static int32_t hwl_write(union itc_msg *msg)
{
	struct hwlog_write_req * req = &msg->hwlog_write_req;
	struct hwlog_entry e;
	char date[DATE_SIZE+1];

	if (write_idx >= mtd_info.size) {
		log_info("HWlog full, entry not written");
		return HWLI_LOG_FULL;
	}

	if (!do_filter(msg)) {
		if (strncmp(req->id, NEW_SW_CONFIG, LOGID_SIZE) == 0)
			sw_cfg_entry_cached = 0;

		return HWLI_FILTERED;
	}

	memset(&e, ERASED_FLASH_CONTENT, sizeof(e));
	e.adm_state = ADM_STATE_UNDEFINED;
	get_datetime(date);
	strncpy(e.date, date, DATE_SIZE);
	strncpy(e.logid, req->id, LOGID_SIZE);
	e.filter_value = 0;
	strncpy(e.freetext, req->msg, FREETEXT_SIZE);

	if (strncmp(e.logid, NEW_SW_CONFIG, LOGID_SIZE) == 0) {
		/*
		 * If there are 2 consecutive updates of the SW configurations without
		 * any write in between, no write of configuration data will be done.
		 * Only the last is stored in the "cache".
		 */
		sw_cfg_entry = e;
		sw_cfg_entry_cached = 1;
	}
	else {
		if (sw_cfg_entry_cached)
		{
			if (write_to_flash(&sw_cfg_entry))
				return HWLI_ERROR;
			sw_cfg_entry_cached = 0;
		}

		if (write_to_flash(&e))
			return HWLI_ERROR;
	}

	return 0;
}

static int init_comm()
{
	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		log_err("Unable to initialize ITC!");
		return -1;
	}

	_hwld_mbox = itc_create_mailbox(HWLOG_MAILBOX, 0);
	if (_hwld_mbox == ITC_NO_ID) {
		log_err("Unable to create ITC mailbox!");
		return -1;
	}

	return 0;
}

static int is_connected(itc_mbox_id_t mbox)
{
	int i;

	for (i = 0; i < max_connections && connections[i]; i++) {
		if (connections[i] == mbox)
			return 1;
	}

	return 0;
}

static void connect(union itc_msg *msg)
{
	union itc_msg *reply;
	itc_mbox_id_t mbox = itc_sender(msg);
	struct hwlog_connect_req * req = &msg->hwlog_connect_req;
	uint32_t * revp = &req->protocol_revision;
	uint32_t revs = req->nbr_of_supported_protocol_revisions;
	int i;

	/* Sanity check the message */
	if (itc_size(msg) < sizeof(struct hwlog_connect_req)) {
		log_info("corrupt size of itc message");
		reply = itc_alloc(sizeof(struct hwlog_connect_rej),
		                  HWLOG_CONNECT_REJ);
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	/*
	 * Sanity check the number of protocols supported parameter.
	 * Note that this code has to change if the definition of
	 * struct hwlog_connect_req changes.
	 */
	if (revs > 0xfffffff ||
	    itc_size(msg) < sizeof(*req) + (revs - 1) * sizeof(uint32_t))
	{
		uint32_t revisions_in_msg = ((itc_size(msg) - sizeof(*req)) >> 2) + 1;
		if (req->nbr_of_supported_protocol_revisions > revisions_in_msg)
			revs = revisions_in_msg;
	}

	for (i = 0; i < revs; i++) {
		if (*revp++ == HWLOG_PROTOCOL_REVISION)
			break;
	}

	if (i == revs) {
		/* No matching protocol revision found */
		reply = itc_alloc(sizeof(struct hwlog_connect_rej),
		                  HWLOG_CONNECT_REJ);
		goto exit;
	}

	for (i = 0; i < max_connections && connections[i]; i++) {
		if (connections[i] == mbox)
			/* Already connected */
			goto cfm_exit;
	}

	if (max_connections == i) {
		/* No space, get more */
		itc_mbox_id_t * save = connections;
		int size = (max_connections + NBR_CONNECTIONS + 1)
		           * sizeof(itc_mbox_id_t);

		connections = realloc(connections, size);
		if (!connections) {
			log_info("Failed to allocate more connections, rejecting");
			connections = save;
			reply = itc_alloc(sizeof(struct hwlog_connect_rej),
			                  HWLOG_CONNECT_REJ);
			goto exit;
		}


		memset(((char *) connections) + max_connections * sizeof(itc_mbox_id_t),
		       0, (NBR_CONNECTIONS + 1) * sizeof(itc_mbox_id_t));
		max_connections += NBR_CONNECTIONS;
	}

	/*
	 * Not saving itc_monitor_id_t since the only way the client is
	 * disconnected is if the client thread or mailbox dies.
	 */
	itc_monitor(mbox, NULL);
	connections[i] = mbox;

cfm_exit:
	reply = itc_alloc(sizeof(struct hwlog_connect_cfm),
	                  HWLOG_CONNECT_CFM);
	reply->hwlog_connect_cfm.protocol_revision = HWLOG_PROTOCOL_REVISION;

exit:
	reply->procedure_reference = msg->procedure_reference;
	reply->connection_reference = mbox;
	itc_send(&reply, mbox, ITC_MY_MBOX);
}

static void disconnect(itc_mbox_id_t mbox)
{
	int i, j;
	itc_mbox_id_t t = ITC_NO_ID;

	for (i = 0; i < max_connections && (t = connections[i]); i++) {
		if (connections[i] == mbox) {
			for (j = i; j < max_connections && connections[j]; j++) {
				connections[j] = connections[j+1];
			}
			connections[j] = 0;
			break;
		}
	}

	if (i == max_connections || t == 0) {
		log_info("Failed to disconnect mailbox, not found!");
	}
}

static void process_write(union itc_msg *msg)
{
	itc_mbox_id_t mbox = itc_sender(msg);
	union itc_msg *reply;
	int32_t error_code;

	/* Sanity check the message */
	if (itc_size(msg) < sizeof(struct hwlog_write_req)) {
		log_info("corrupt size of itc message");
		reply = itc_alloc(sizeof(struct hwlog_write_rej),
					HWLOG_WRITE_REJ);
		reply->hwlog_write_rej.error_code = HWLI_ERROR;
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	/* Make sure the client is connected */
	if (!is_connected(msg->connection_reference)) {
		log_info("non-connected client tried to issue command");
		reply = itc_alloc(sizeof(struct hwlog_write_rej),
					HWLOG_WRITE_REJ);
		reply->procedure_reference = msg->procedure_reference;
		reply->hwlog_write_rej.error_code = HWLI_ERROR;
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	error_code = hwl_write(msg);
	if (error_code != 0) {
		reply = itc_alloc(sizeof(struct hwlog_write_rej),
					HWLOG_WRITE_REJ);
		reply->hwlog_write_rej.error_code = error_code;
	} else
		reply = itc_alloc(sizeof(struct hwlog_write_cfm),
					HWLOG_WRITE_CFM);

	reply->procedure_reference = msg->procedure_reference;
	reply->connection_reference = msg->connection_reference;
	itc_send(&reply, mbox, ITC_MY_MBOX);
}

static int get_ack(uint32_t nbr, itc_mbox_id_t client_mbox)
{
	union itc_msg *msg;
	uint32_t filter[] =
		{ 3, HWLOG_LOGREAD_ACK, HWLOG_WRITE_REQ, ITC_MONITOR_DEFAULT_NO };
	int status = 0;

	while (1) {
		msg = itc_receive(filter, ACK_TIMEOUT_MS, client_mbox);

		if (msg == NULL) {
			log_info("readlog timed out waiting for ack, aborting transfer");
			break;
		}

		if (msg->msgno == HWLOG_WRITE_REQ) {
			/* Let a write through while we wait for a readlog ack */
			process_write(msg);

			/* Don't advance the readlog for loop for the high prio write */
			itc_free(&msg);
			continue;
		}
		else if (msg->msgno == ITC_MONITOR_DEFAULT_NO) {
			log_info("readlog aborted because client died before completion");
			disconnect(client_mbox);
			break;
		}
		else if (msg->hwlog_logread_ack.sequence_number ==
		                                            HWLOG_LOGREAD_ABORT) {
			log_info("readlog aborted from client");
			break;
		}
		else if (msg->hwlog_logread_ack.sequence_number != nbr) {
			log_info("Received %#x ack sequence number, expected %#x",
			         msg->hwlog_logread_ack.sequence_number, nbr);
			break;
		}

		status = 1;
		break;
	}

	if (msg)
		itc_free(&msg);
	return status;
}

static void process_read(union itc_msg *msg)
{
	struct hwli_entry *entries = NULL;
	itc_mbox_id_t mbox = itc_sender(msg);
	union itc_msg *reply;
	uint32_t len;

	/* Sanity check the message */
	if (itc_size(msg) < sizeof(struct hwlog_logread_req)) {
		log_info("corrupt size of itc message");
		reply = itc_alloc(sizeof(struct hwlog_logread_rej),
		                  HWLOG_LOGREAD_REJ);
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	/* Make sure the client is connected */
	if (!is_connected(msg->connection_reference)) {
		log_info("non-connected client tried to issue command");
		reply = itc_alloc(sizeof(struct hwlog_logread_rej),
		                  HWLOG_LOGREAD_REJ);
		reply->procedure_reference = msg->procedure_reference;
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	if (hwl_readlog(&entries, &len) < 0) {
		reply = itc_alloc(sizeof(struct hwlog_logread_rej),
		                  HWLOG_LOGREAD_REJ);
		reply->procedure_reference = msg->procedure_reference;
		reply->connection_reference = msg->connection_reference;
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	reply = itc_alloc(sizeof(struct hwlog_logread_cfm), HWLOG_LOGREAD_CFM);
	reply->procedure_reference = msg->procedure_reference;
	reply->connection_reference = msg->connection_reference;
	reply->hwlog_logread_cfm.len = len;
	itc_send(&reply, mbox, ITC_MY_MBOX);

	for (uint32_t nbr = 1; nbr <= len; nbr++) {
		reply = itc_alloc(sizeof(struct hwlog_logread_data),
		                  HWLOG_LOGREAD_DATA);
		reply->procedure_reference = msg->procedure_reference;
		reply->connection_reference = msg->connection_reference;
		reply->hwlog_logread_data.sequence_number = nbr;
		reply->hwlog_logread_data.entry = entries[nbr-1];
		itc_send(&reply, mbox, ITC_MY_MBOX);

		/*
		 * Send an ack if we are sending the last data or if
		 * the client gave a data interval for acking and it's time to ack.
		 */
		if (nbr == len || (msg->hwlog_logread_req.data_per_ack
			    && nbr % msg->hwlog_logread_req.data_per_ack == 0)) {
			if (!get_ack(nbr, mbox))
				break;
		}
	}

	if (entries)
		free(entries);
}

static void process_erase(union itc_msg *msg)
{
	itc_mbox_id_t mbox = itc_sender(msg);
	union itc_msg *reply;

	/* Sanity check the message */
	if (itc_size(msg) < sizeof(struct hwlog_logerase_req)) {
		log_info("corrupt size of itc message");
		reply = itc_alloc(sizeof(struct hwlog_logerase_rej),
					HWLOG_LOGERASE_REJ);
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	/* Make sure the client is connected */
	if (!is_connected(msg->connection_reference)) {
		log_info("non-connected client tried to issue command");
		reply = itc_alloc(sizeof(struct hwlog_logerase_rej),
					HWLOG_LOGERASE_REJ);
		reply->procedure_reference = msg->procedure_reference;
		itc_send(&reply, mbox, ITC_MY_MBOX);
		return;
	}

	if (hwl_eraselog())
		reply = itc_alloc(sizeof(struct hwlog_logerase_rej),
					HWLOG_LOGERASE_REJ);
	else
		reply = itc_alloc(sizeof(struct hwlog_logerase_cfm),
					HWLOG_LOGERASE_CFM);

	reply->procedure_reference = msg->procedure_reference;
	reply->connection_reference = msg->connection_reference;
	itc_send(&reply, mbox, ITC_MY_MBOX);
}

static void read_messages(void)
{
	union itc_msg *msg;

	while (1) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {
			case HWLOG_CONNECT_REQ:
				connect(msg);
				break;
			case HWLOG_WRITE_REQ:
				process_write(msg);
				break;
			case HWLOG_LOGREAD_REQ:
				process_read(msg);
				break;
			case HWLOG_LOGERASE_REQ:
				process_erase(msg);
				break;
			case ITC_MONITOR_DEFAULT_NO:
				disconnect(itc_sender(msg));
				break;
			case EXIT_SIGNAL:
				log_info("hwld exiting as ordered");
				itc_free(&msg);
				return;
			default:
				log_info("Received unknown message %u", msg->msgno);
				break;
		}

		itc_free(&msg);
	}
}

void exit_handler(int sig)
{
	union itc_msg *msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	log_info("Received signal %d, terminating", sig);
	itc_send(&msg, _hwld_mbox, ITC_MY_MBOX);
}

int main(int argc, char *argv[])
{
	int sfd;
	char *dev = getenv(DEVICE_ENV);

	if (!dev) {
		log_err("The %s env variable must be set", DEVICE_ENV);
		exit(1);
	}

	connections = calloc(max_connections + 1, sizeof(itc_mbox_id_t));
	if (!connections) {
		log_err("Failed to allocate space for connections");
		exit(1);
	}

	if (strncmp(dev, FLASH_DEVICE, strlen(FLASH_DEVICE)) == 0)
		io = &hwld_io_mtd;
	else
		io = &hwld_io_file;

	mtd_fd = io->init(dev, &mtd_info);
	if (mtd_fd == -1) {
		log_err("Failed to initialize i/o device: %s", dev);
		exit(1);
	}

	/* Setup server */
	sfd = init_comm();
	if(sfd < 0)
		exit(1);
	log_info("hwld started");

	/* find and set write_idx */
	if (get_free_entry() < 0)
		log_info("get_free_entry() failed");

	if (signal(SIGTERM, exit_handler) == SIG_ERR) {
		log_err("Failed to install signal exit handler");
		exit(1);
	}

	read_messages();

	return 0;
}

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
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include <mtd/mtd-user.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include "hwlog.h"
#include "signals.h"

#define OK 0
#define FAILED -2
#define COUNT_MORE_THAN_EXPECTED -3
#define COUNT_LESS_THAN_EXPECTED -4

#define MAILBOX_SIZE 101

#define FLASH_SIZE (512 * 1024)
#define ENTRY_SIZE 128

#define ERR(fmt, ...) do {                                \
	fprintf(stderr, "%s:%d %s() Error: " fmt "\n",        \
	        __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
} while (0)

#define MSG(fmt, ...) do { printf(fmt "\n", ##__VA_ARGS__); } while (0)

union itc_msg {
	struct {
		uint32_t msgno;
		uint32_t procedure_reference;
		uint32_t connection_reference;
	};
	HWLOG_SIGNAL_STRUCTS;
};

struct result {
	struct hwli_entry *entries;
	uint32_t size;
	int return_code;
};

void read_log(int print, struct result *res)
{
	int ret = 0;
	struct hwli_entry *entries;
	uint32_t size = 0;

	ret = hwli_readlog(&entries, &size);

	if (print) {
		if (ret == HWLI_SUCCESS) {
			MSG("Number of hwlog entries %d", size);
			for (int i = 0; i < size; i++) {
				MSG("%d (%s) %s %s",
			    	i+1, entries[i].id, entries[i].time, entries[i].msg);
			}
		}
	}

	res->entries = entries;
	res->size = size;
	res->return_code = ret;
}

int hwlog_readlog_test() {
	struct result res;

	MSG("Starting hwlog_readlog_test..");
	read_log(1, &res);

	return res.return_code == HWLI_SUCCESS ? OK : FAILED;
}

int erase_log(itc_mbox_id_t mbox, uint32_t connection_reference)
{
	uint32_t procedure_reference_ret, connection_reference_ret;

	send_logerase_req(mbox, 0xbeef, connection_reference, ITC_MY_MBOX);

	if (get_logerase_reply(mbox, &procedure_reference_ret,
	                       &connection_reference_ret, ITC_NO_TMO) != CONFIRM) {
		ERR("Log erase rejected");
		return FAILED;
	}

	if (procedure_reference_ret != 0xbeef) {
		ERR("Expected connection_reference 0xbeef, got %#x\n",
		    connection_reference_ret);
		return FAILED;
	}

	return OK;
}

int hwlog_eraselog_test()
{
	uint32_t connection_reference;
	struct result res;
	itc_mbox_id_t mbox = init_itc(&connection_reference);

	MSG("Starting hwlog_eraselog_test..");

	if (mbox == ITC_NO_ID)
		return FAILED;

	if (erase_log(mbox, connection_reference) != OK)
		return FAILED;

	read_log(1, &res);

	if (res.return_code != HWLI_SUCCESS || res.size > 0) {
		ERR("Failed to erase hwlog %d", res.return_code);
		return FAILED;
	}

	return OK;
}

int hwlog_writelog_test()
{
	uint32_t connection_reference;
	itc_mbox_id_t mbox = init_itc(&connection_reference);
	struct result res;
	char *msg = "hellohello";
	char *id = "xyz";

	MSG("Starting hwlog_writelog_test..");

	if (mbox == ITC_NO_ID)
		return FAILED;

	if (erase_log(mbox, connection_reference) != OK)
		return FAILED;

	if (hwli_write(id, HWLI_NO_FILTER, HWLI_NO_FILTER_ON_MSG, msg)
		!= HWLI_SUCCESS) {
		ERR("Failed write");
		return FAILED;
	}

	MSG("Verifying write..");
	read_log(1, &res);

	return (res.return_code == HWLI_SUCCESS && res.size == 1) ? OK : FAILED;
}

static int read_compare(int expected, char *swcfg_msg)
{
	struct result res;
	int last_swcfg_idx = -1;

	read_log(0, &res);
	if (res.return_code != HWLI_SUCCESS || res.size != expected) {
		ERR("Expected %d entries, got %d", expected, res.size);
		return FAILED;
	}

	for (int i = 0; i < res.size; i++) {
		if (strncmp(res.entries[i].id, "003", HWLI_ID_SZ) == 0)
			last_swcfg_idx = i;
	}

	if (last_swcfg_idx == -1) {
		if (swcfg_msg) {
			ERR("No sw config entry found!");
			return FAILED;
		}
	}
	else {
		if (!swcfg_msg) {
			ERR("Found unexpected sw config entry!");
			return FAILED;
		}

		if (strncmp(res.entries[last_swcfg_idx].msg,
		            swcfg_msg, HWLI_MESSAGE_SZ) != 0)
		{
			ERR("sw config message was %s, expected %s",
			    res.entries[last_swcfg_idx].msg, swcfg_msg);
			return FAILED;
		}
	}

	return OK;
}

int hwlog_writelog_filter_test()
{
	char *msg1 = "hellohello";
	char *msg2 = "holahola";
	char *msg3 = "power on";
	char *msg4 = "sw_config_a";
	char *msg5 = "sw_config_b";
	char *msg6 = "sfp_info_a";
	char *msg7 = "sfp_info_b";
	char *id = "abc";
	char *hw_cfg_id = "000";
	char *sw_cfg_id = "003";
	char *special_id = "004";
	uint32_t filter = 1;
	uint32_t connection_reference;
	itc_mbox_id_t mbox = init_itc(&connection_reference);

	MSG("Starting hwlog_writelog_filter_test..");

	if (mbox == ITC_NO_ID)
		return FAILED;

	if (erase_log(mbox, connection_reference) != OK)
		return FAILED;

	if (hwli_write(id, filter, HWLI_NO_FILTER_ON_MSG, msg1) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(1, NULL))
		return FAILED;

	if (hwli_write(id, filter, HWLI_NO_FILTER_ON_MSG, msg1) != HWLI_FILTERED)
		return FAILED;

	if (read_compare(1, NULL))
		return FAILED;

	if (hwli_write(id, filter, HWLI_FILTER_ON_MSG, msg2) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(2, NULL))
		return FAILED;

	if (hwli_write(id, filter, HWLI_FILTER_ON_MSG, msg2) != HWLI_FILTERED)
		return FAILED;

	if (read_compare(2, NULL))
		return FAILED;

	/* power on cannot be called with filter, should be forced by server */
	if (hwli_write(special_id, filter, 1, msg3) != HWLI_SUCCESS)
		return FAILED;

	/* The special id should reset the filter, letting this write go through */
	if (hwli_write(id, filter, 1, msg2) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(4, NULL))
		return FAILED;

	/* Since filter is forced off, this write should also go through */
	if (hwli_write(special_id, filter, 1, msg3) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(5, NULL))
		return FAILED;

	/* Filter will be forced on for config id, this entry will be cached */
	if (hwli_write(sw_cfg_id, 0, 0, msg4) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(5, NULL))
		return FAILED;

	/* This should not cause a write */
	if (hwli_write(sw_cfg_id, 0, 0, msg4) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(5, NULL))
		return FAILED;

	/* This should update the cache */
	if (hwli_write(sw_cfg_id, 0, 0, msg5) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(5, NULL))
		return FAILED;

	/* This should cause sw cfg to be written along with this entry */
	if (hwli_write(id, 0, 0, msg2) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(7, msg5))
		return FAILED;

	/* This should go through and update the cache */
	if (hwli_write(sw_cfg_id, 0, 0, msg4) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(7, msg5))
		return FAILED;

	/* This should go through */
	/* This should cause sw cfg to be written along with this entry */
	if (hwli_write(hw_cfg_id, 1, HWLI_FILTER_ON_MSG, msg6) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(9, msg4))
		return FAILED;

	if (hwli_write(id, 0, 0, msg2) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(10, msg4))
		return FAILED;

	/* This should not go through */
	if (hwli_write(hw_cfg_id, 1, HWLI_FILTER_ON_MSG, msg6) != HWLI_FILTERED)
		return FAILED;

	if (read_compare(10, msg4))
		return FAILED;

	if (hwli_write(hw_cfg_id, 1, HWLI_FILTER_ON_MSG, msg7) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(11, msg4))
		return FAILED;

	/* This should be allowed as msg6 wasn't the last hw_cfg_id one */
	if (hwli_write(hw_cfg_id, 1, HWLI_FILTER_ON_MSG, msg6) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(12, msg4))
		return FAILED;

	/* This should be cached */
	if (hwli_write(sw_cfg_id, 0, 0, msg5) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(12, msg4))
		return FAILED;

	/* This is the same as the last written sw_cfg_id message.
	 * It should be discarded but also clear the cache.
	 */
	if (hwli_write(sw_cfg_id, 0, 0, msg4) != HWLI_FILTERED)
		return FAILED;

	if (read_compare(12, msg4))
		return FAILED;

	/* This should be allowed as msg5 wasn't the last hw_cfg_id one
	 * Previous 2 sf_cfg_id messages should have been discarded
	 */
	if (hwli_write(hw_cfg_id, 1, HWLI_FILTER_ON_MSG, msg5) != HWLI_SUCCESS)
		return FAILED;

	if (read_compare(13, msg4))
		return FAILED;

	return OK;
}

int bad_client_connect_test()
{
	int ret;
	uint32_t procedure_reference, connection_reference, protocol_revision;
	uint32_t revisions[] = { 2, 1, 0 };
	itc_mbox_id_t mbox;

	MSG("Starting bad_client_connect_test..");

	mbox = itc_locate(HWLOG_MAILBOX);
	if (mbox == ITC_NO_ID) {
		ERR("Failed to locate mailbox %s\n", HWLOG_MAILBOX);
		return FAILED;
	}

	/* Say that we have more revisions than we really do, should succeed */
	send_connect_req(mbox, 100, 101, 0xffffff, revisions, ITC_MY_MBOX);

	ret = get_connect_reply(mbox, &procedure_reference, &connection_reference,
	                        &protocol_revision, ITC_NO_TMO);

	if (ret == REJECT) {
		ERR("Connection rejected");
		return FAILED;
	}

	if (protocol_revision != 1) {
		ERR("Expected protocol revision 1, got %d", protocol_revision);
		return FAILED;
	}

	return OK;
}

static int connect(int id, itc_mbox_id_t *hwld_mbox, uint32_t *conn_ref)
{
	int ret;
	uint32_t procedure_reference, connection_reference, protocol_revision;
	uint32_t revisions[] = { 1, 0 };
	itc_mbox_id_t mbox;
	char mailbox_name[] = "hwlog_test   ";

	snprintf(mailbox_name, strlen(mailbox_name), "hwlog_test%d", id);

	if (itc_create_mailbox(mailbox_name, 0) == ITC_NO_ID) {
		ERR("Failed to create itc mailbox");
		return FAILED;
	}

	mbox = itc_locate(HWLOG_MAILBOX);
	if (mbox == ITC_NO_ID) {
		ERR("Failed to locate mailbox %s", HWLOG_MAILBOX);
		return FAILED;
	}

	send_connect_req(mbox, 0, 0, 1, revisions, ITC_MY_MBOX);

	ret = get_connect_reply(mbox, &procedure_reference, &connection_reference,
	                        &protocol_revision, ITC_NO_TMO);

	if (ret == REJECT) {
		ERR("Connection rejected");
		return FAILED;
	}

	if (protocol_revision != 1) {
		ERR("Expected protocol revision 1, got %d", protocol_revision);
		return FAILED;
	}

	*conn_ref = connection_reference;
	*hwld_mbox = mbox;
	return OK;
}

#define NUM_CLIENTS 100

void *client_thread(void *arg)
{
	int thread_id = (int) arg;
	itc_mbox_id_t hwld_mbox;
	uint32_t connection_reference;
	void * status = (void *) FAILED;
	uint32_t procedure_reference_ret, connection_reference_ret;
	char max_text[HWLI_MESSAGE_SZ];
	char id[HWLI_ID_SZ];
	int i;

	if (connect(thread_id, &hwld_mbox, &connection_reference) != OK)
		goto exit;

	for (i = 0; i < HWLI_MESSAGE_SZ-1; i++) {
		max_text[i] = (thread_id % 77) + '0';
	}
	max_text[i] = '\0';

	snprintf(id, HWLI_ID_SZ, "%d", thread_id);

	send_write_req(hwld_mbox, thread_id, connection_reference, id,
	    HWLI_NO_FILTER, HWLI_NO_FILTER_ON_MSG, max_text, ITC_MY_MBOX);

	if (get_write_reply(hwld_mbox, &procedure_reference_ret,
	                    &connection_reference_ret, ITC_NO_TMO) != CONFIRM) {
		ERR("Failed write confirm in client %d", thread_id);
		goto exit;
	}

	if (procedure_reference_ret != thread_id) {
		ERR("Expected %d procedure_reference, got %d",
		    thread_id, procedure_reference_ret);
		goto exit;
	}

	if (connection_reference_ret != connection_reference) {
		ERR("Expected %d connection_reference, got %d",
		    connection_reference, connection_reference_ret);
		goto exit;
	}

	status = (void *) OK;

exit:
	return status;
}

int many_clients_test()
{
	struct hwli_entry *entries;
	pthread_t clients[NUM_CLIENTS];
	uint32_t connection_reference;
	itc_mbox_id_t mbox = init_itc(&connection_reference);
	uint32_t size;

	MSG("Starting many_clients_test..");

	if (mbox == ITC_NO_ID)
		return FAILED;

	MSG("Erasing log..");
	if (erase_log(mbox, connection_reference) != OK)
		return FAILED;

	MSG("Spawning %d client threads..", NUM_CLIENTS);

	for (int i = 0; i < NUM_CLIENTS; i++) {
		if (pthread_create(&clients[i], NULL, client_thread, (void *) i)) {
			ERR("Failed to create client thread %d", i);
			return FAILED;
		}
	}

	for (int i = 0; i < NUM_CLIENTS; i++) {
		void * ret;
		pthread_join(clients[i], &ret);
		if (ret)
			return FAILED;
	}

	MSG("Client threads finished, comparing log..");

	if (hwli_readlog(&entries, &size) != HWLI_SUCCESS) {
		ERR("Failed to read log");
		return FAILED;
	}

	if (size != NUM_CLIENTS) {
		ERR("Expected %d entries in log, got %d", NUM_CLIENTS, size);
		return FAILED;
	}

	for (int i = 0; i < NUM_CLIENTS; i++) {
		char *txt = entries[i].msg;
		int logid = atoi(entries[i].id);
		for (int j = 0; j < HWLI_MESSAGE_SZ-1; j++) {
			if ((txt[j] - '0') != (logid % 77)) {
				ERR("Text wrong at %d, expected %d, got %c", j, i, txt[j]);
				return FAILED;
			}
		}

		if (txt[HWLI_MESSAGE_SZ-1] != '\0') {
			ERR("Text %d not null terminated", i);
			return FAILED;
		}
	}

	return OK;
}

int readlog_ack(itc_mbox_id_t mbox,
                uint32_t connection_reference,
                struct hwli_entry *entries,
                uint32_t len,
                uint32_t data_per_ack)
{
	uint32_t procedure_reference_ret, connection_reference_ret;
	uint32_t len_ret, left = len;
	int32_t sequence_number_ret;
	struct hwli_entry entry;

	MSG("Sending logread request with %d data per ack..", data_per_ack);

	send_logread_req(mbox, data_per_ack, connection_reference, data_per_ack,
	                 ITC_MY_MBOX);

	if (get_logread_reply(mbox, &procedure_reference_ret,
	        &connection_reference_ret, &len_ret, ITC_NO_TMO) == REJECT) {
		ERR("logread rejected");
		return FAILED;
	}

	if (connection_reference_ret != connection_reference) {
		ERR("Got wrong connection_reference %u, expected %u",
		    connection_reference_ret, connection_reference);
		return FAILED;
	}

	if (procedure_reference_ret != data_per_ack) {
		ERR("Got wrong procedure_reference %u, expected %u",
		    procedure_reference_ret, data_per_ack);
		return FAILED;
	}

	if (len_ret != len) {
		ERR("Got wrong len %d, expected %d", len_ret, len);
		return FAILED;
	}

	while (left > 0) {
		for (int i = 0; i < data_per_ack && left > 0; i++, left--) {
			if (get_logread_data(mbox, &procedure_reference_ret,
			                     &connection_reference_ret,
			                     &sequence_number_ret,
			                     &entry, 5000) == TIMEOUT) {
				ERR("logread timed out with %u left", left);
				return FAILED;
			}

#if 0
			if (strcmp(entry.id, entries[i].id) != 0) {
				ERR("for entry %d got id %s, expected %s\n", i,
				    entry.id, entries[i].id);
				return FAILED;
			}

			if (strcmp(entry.msg, entries[i].msg) != 0) {
				ERR("for entry %d got msg %s, expected %s\n", i,
				    entry.msg, entries[i].msg);
				return FAILED;
			}
#endif
		}

		MSG("Sending ack..");
		send_logread_ack(mbox, data_per_ack, connection_reference,
		                 sequence_number_ret, ITC_MY_MBOX);
	}

	return OK;
}

#define NUM_ENTRIES 10

int readlog_ack_test()
{
	uint32_t connection_reference;
	struct hwli_entry entries[NUM_ENTRIES];
	itc_mbox_id_t mbox = init_itc(&connection_reference);

	MSG("Starting readlog_ack_test..");

	if (mbox == ITC_NO_ID)
		return FAILED;

	MSG("Erasing log..");
	if (erase_log(mbox, connection_reference) != OK)
		return FAILED;

	MSG("Writing %d entries to log..", NUM_ENTRIES);
	for (int i = 0; i < NUM_ENTRIES; i++) {
		uint32_t dummy;
		int j;

		snprintf(entries[i].id, HWLI_ID_SZ, "%d", i);

		for (j = 0; j < HWLI_MESSAGE_SZ-1; j++)
			entries[i].msg[j] = (i % 77) + '0';
		entries[i].msg[j] = '\0';

		send_write_req(mbox, 0, connection_reference,
		               entries[i].id, HWLI_NO_FILTER, HWLI_NO_FILTER_ON_MSG,
		               entries[i].msg, ITC_MY_MBOX);

		if (get_write_reply(mbox, &dummy, &dummy, ITC_NO_TMO) != CONFIRM) {
			ERR("Failed write confirm");
			return FAILED;
		}
	}

	for (int data_per_ack = 1; data_per_ack <= NUM_ENTRIES; data_per_ack++) {
		if (readlog_ack(mbox, connection_reference, entries,
	                    NUM_ENTRIES, data_per_ack) != OK)
			return FAILED;
	}

	return OK;
}

int get_mtd_info(char *device, mtd_info_t *pinfo)
{
	int fd = open(device, O_RDWR, O_SYNC | O_NONBLOCK);
	if (fd < 0)
		return -1;

	if (ioctl(fd, MEMGETINFO, pinfo)) {
		close(fd);
		return -1;
	}

	close(fd);

	return 0;
}

#define NUM_EXTRA_ENTRIES 10

int fill_log_test()
{
	uint32_t connection_reference;
	int num_entries, max_entries;
	itc_mbox_id_t mbox = init_itc(&connection_reference);
	char id[HWLI_ID_SZ];
	char msg[HWLI_MESSAGE_SZ];
	struct hwli_entry *entries;
	size_t size;
	char *mtd_device;

	MSG("Starting fill_log_test..");

	mtd_device = getenv("sys_hwlog_dev");
	if (!mtd_device) {
		ERR("sys_hwlog_dev not set");
		return FAILED;
	}

	if (strncmp(mtd_device, "/dev/mtd", strlen("/dev/mtd")) == 0) {
		mtd_info_t mtd_info;
		if (get_mtd_info(mtd_device, &mtd_info)) {
			ERR("Failed to get mtd info of %s", mtd_device);
			return FAILED;
		}
		max_entries = mtd_info.size / ENTRY_SIZE;
	}
	else {
		max_entries = FLASH_SIZE / ENTRY_SIZE;
	}

	if (mbox == ITC_NO_ID)
		return FAILED;

	MSG("Erasing log..");
	if (erase_log(mbox, connection_reference) != OK)
		return FAILED;

	/* Write more entries than will fit in the log */
	num_entries = max_entries + NUM_EXTRA_ENTRIES;

	MSG("Writing %u entries to log, %u is max it can hold",
	    num_entries, max_entries);

	for (int i = 0; i < num_entries; i++) {
		uint32_t dummy;
		int ret;
		int j;

		strcpy(id, "abc");

		for (j = 0; j < HWLI_MESSAGE_SZ-1; j++)
			msg[j] = (i % 77) + '0';
		msg[j] = '\0';

		send_write_req(mbox, 0, connection_reference,
		               id, HWLI_NO_FILTER, HWLI_NO_FILTER_ON_MSG,
		               msg, ITC_MY_MBOX);

		ret = get_write_reply(mbox, &dummy, &dummy, ITC_NO_TMO);

		if (i < max_entries && ret != CONFIRM) {
			ERR("Failed write confirm");
			return FAILED;
		}

		if (i >= max_entries && ret != REJECT) {
			ERR("Expected rejection of excess entries");
			return FAILED;
		}
	}

	if (hwli_write("xxx", HWLI_NO_FILTER, HWLI_NO_FILTER_ON_MSG, "baba") !=
	    HWLI_LOG_FULL) {
		ERR("Expected HWLI_LOG_FULL when writing to full log");
		return FAILED;
	}

	if (hwli_readlog(&entries, &size) != HWLI_SUCCESS) {
		ERR("Failed to read log");
		return FAILED;
	}

	if (size != max_entries) {
		ERR("Read %d entries from log, expected %d", size, max_entries);
		return FAILED;
	}

	MSG("Reading back %u entries and confirming them..", max_entries);

	for (int i = 0; i < max_entries; i++) {
		char *txt = entries[i].msg;

		for (int j = 0; j < HWLI_MESSAGE_SZ-1; j++) {
			if ((txt[j] - '0') != (i % 77)) {
				ERR("Text wrong at %d, expected %d, got %c", j, i, txt[j]);
				return FAILED;
			}
		}

		if (txt[HWLI_MESSAGE_SZ-1] != '\0') {
			ERR("Text %d not null terminated", i);
			return FAILED;
		}
	}

	return OK;
}

int write_during_ack_test()
{
	uint32_t connection_reference;
	char *msgs[] = { "my message", "other message" };
	char *ids[] = { "abc", "bcd" };
	itc_mbox_id_t mbox = init_itc(&connection_reference);
	struct hwli_entry entry;
	struct hwli_entry *entries;
	uint32_t dummy;
	int32_t seq;
	size_t size;

	MSG("Starting write_during_ack_test..");

	if (mbox == ITC_NO_ID)
		return FAILED;

	MSG("Erasing log..");
	if (erase_log(mbox, connection_reference) != OK)
		return FAILED;

	MSG("Writing and confirming entries..");
	/* Write the first entry */
	send_write_req(mbox, 0, connection_reference,
	               ids[0], HWLI_NO_FILTER, HWLI_NO_FILTER_ON_MSG,
	               msgs[0], ITC_MY_MBOX);

	if (get_write_reply(mbox, &dummy, &dummy, ITC_NO_TMO) != CONFIRM) {
		ERR("Failed write confirm");
		return FAILED;
	}

	/* Ack after every entry */
	send_logread_req(mbox, 0, connection_reference, 1,
	                 ITC_MY_MBOX);

	if (get_logread_reply(mbox, &dummy,&dummy, &dummy, ITC_NO_TMO) == REJECT) {
		ERR("logread rejected");
		return FAILED;
	}

	/* Read entry, the server will expect an ack .. */
	if (get_logread_data(mbox, &dummy, &dummy, &seq,
	                     &entry, 5000) == TIMEOUT) {
		ERR("logread timed out");
		return FAILED;
	}

	/* Instead we write another entry, it should get priority */
	send_write_req(mbox, 0, connection_reference,
	               ids[1], HWLI_NO_FILTER, HWLI_NO_FILTER_ON_MSG,
	               msgs[1], ITC_MY_MBOX);

	if (get_write_reply(mbox, &dummy, &dummy, ITC_NO_TMO) != CONFIRM) {
		ERR("Failed write confirm");
		return FAILED;
	}

	/* Now ack the previously read entry */
	send_logread_ack(mbox, 0, connection_reference,
	                 seq, ITC_MY_MBOX);

	/*
	 * The read should be done, the high prio write will be available next
	 * logread. Read again and confirm.
	 */
	if (hwli_readlog(&entries, &size) != HWLI_SUCCESS) {
		ERR("Failed to read log");
		return FAILED;
	}

	if (size != 2) {
		ERR("Expected 2 entries, got %d", size);
		return FAILED;
	}

	return OK;
}

void printUsage() {

	printf("Usage: hwlog_test -tc <test case nbr>\n\n"
	       "\twhere <test case nbr> is one of:\n"
	       "\t1:  Read hwlog test\n"
	       "\t2:  Write hwlog test with no filter\n"
	       "\t3:  Write hwlog test with filter\n"
	       "\t4:  Erase hwlog test (erases log)\n"
	       "\t5:  Bad client connect test\n"
	       "\t6:  Many clients test (erases log)\n"
	       "\t7:  Readlog with ack test (erases log)\n"
	       "\t8:  Fill log test (erases log)\n"
	       "\t9:  Write during ack test (erases log)\n"
	       "\n");
}

int main(int argc, char **argv) {

	int tc = 0, result = 0;

	if (argc < 3) {
		printUsage();
		return FAILED;
	}

	if (argc >= 3) {
		if (strstr(argv[1], "-tc") != NULL) {
			tc = atoi(argv[2]);
		}
	}

	/*
	 * The application needs to initialize itc and create a mailbox for the
	 * thread before using the hwli interface.
	 */
	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		ERR("Failed to initialize itc");
		exit(1);
	}

	if (itc_create_mailbox("hwlog_test", 0) == ITC_NO_ID) {
		ERR("Failed to create itc mailbox");
		exit(1);
	}

	switch (tc) {

	case 1:
		result = hwlog_readlog_test();
		break;

	case 2:
		result = hwlog_writelog_test();
		break;

	case 3:
		result = hwlog_writelog_filter_test();
		break;

	case 4:
		result = hwlog_eraselog_test();
		break;

	case 5:
		result = bad_client_connect_test();
		break;

	case 6:
		result = many_clients_test();
		break;

	case 7:
		result = readlog_ack_test();
		break;

	case 8:
		result = fill_log_test();
		break;

	case 9:
		result = write_during_ack_test();
		break;

	default:
		printf("Wrong tc number %d\n", tc);
		printUsage();
		return FAILED;
	}

	if (result) {
		printf("\nTestcase %d FAILED \n", tc);
		return FAILED;
	} else {
		printf("\nTestcase %d PASSED \n", tc);
	}

	return OK;
}

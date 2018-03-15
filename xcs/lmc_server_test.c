/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <getopt.h>
#include <itc.h>
#include "lmc_server.h"

#define MAILBOX_SIZE 1
#define CONN_ESTABLISH_TMO 0

static uint32_t server_ref;
static uint32_t procedure_ref = 0;
static uint32_t client_ref = 1234;
static itc_mbox_id_t server_mbox;
static itc_mbox_id_t my_mbox;

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	LMC_SERVER_MESSAGES;
};

static enum {
	IDLE,      /* default value */
	WRITE,     /* flash write test */
	READ       /* flash read test */
} test = IDLE;


static void print_usage(char *arg0)
{
	printf("Usage : %s [-h] <-w> <path> <pid> \n"
	       " -h	Display usage information.\n"
	       " -w	Load LMC to flash\n"
	       " <path> path to the LMC file\n"
	       " <pid>  LMC pid\n\n", basename(arg0));
}

static uint32_t check_msg(uint32_t expected, union itc_msg *msg)
{
	uint32_t res = 1;

	if(!msg) {
		printf("Timeout! Server did not reply within 15 seconds,\n");
		goto check_failed;
	}
	if(msg->any_msg.connection_ref != client_ref) {
		printf("Server replied with invalid connection_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       client_ref, msg->any_msg.connection_ref);
		goto check_failed;
	}

	if(msg->msgno != expected) {
		printf("Server replied with unexpected message;"
		       "message number: 0x%08x\n",
		       msg->msgno);
		goto check_failed;
	}

	res = 0;
check_failed:
	return res;
}

static struct lmc_load_file_entry *get_lmc_list(uint32_t *lmc_count)
{
	uint32_t list_size;
	struct lmc_load_file_entry *lmc_list = NULL;
	union itc_msg *msg = NULL;

	msg = itc_alloc(sizeof(struct lmc_get_load_file_info_req),
	                LMC_GET_LOAD_FILE_INFO_REQ);
	msg->get_load_file_info_req.procedure_ref = ++procedure_ref;
	msg->get_load_file_info_req.connection_ref = server_ref;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, 15000, server_mbox);
	if (check_msg(LMC_GET_LOAD_FILE_INFO_CFM, msg))
		goto get_lmc_list_failed;

	*lmc_count = msg->get_load_file_info_cfm.lmc_count;
	list_size = sizeof(struct lmc_load_file_entry) * (*lmc_count);
	lmc_list = malloc(list_size);
	if (!lmc_list) {
		printf("Malloc failed for size %u", list_size);
		goto get_lmc_list_failed;
	}
	memcpy(lmc_list, msg->get_load_file_info_cfm.info, list_size);

get_lmc_list_failed:
	if (msg)
		itc_free(&msg);
	return lmc_list;
}

static uint32_t delete_lmc(char * pid)
{
	uint32_t lmc_len = strlen(pid);
	union itc_msg *msg;

	msg = itc_alloc(sizeof(struct lmc_load_file_delete_req) + lmc_len + 1,
	                LMC_LOAD_FILE_DELETE_REQ);
	strncpy(msg->load_file_delete_req.loadmodule, pid, lmc_len + 1);
	msg->load_file_delete_req.connection_ref = server_ref;
	msg->load_file_delete_req.procedure_ref = ++procedure_ref;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, 15000, server_mbox);
	if (check_msg(LMC_LOAD_FILE_DELETE_CFM, msg))
		goto delete_failed;

	itc_free(&msg);
	/* check indication */
	msg = itc_receive(ITC_NOFILTER, 15000, server_mbox);
	if (!msg) {
		printf("Timeout! Server did not reply within 15 seconds,\n");
		goto delete_failed;
	}
	if (msg->msgno != LMC_LOAD_FILE_DELETE_IND) {
		printf("Receive unexpected message 0x%08x\n", msg->msgno);
		goto delete_failed;
	}
	if (msg->load_file_delete_ind.result != LMC_RESULT_SUCCESS) {
		printf("Delete failed, return %u",
		       msg->load_file_delete_ind.result);
		goto delete_failed;
	}
	itc_free(&msg);
	return 0;

delete_failed:
	if (msg)
		itc_free(&msg);
	return 1;
}

static uint32_t load_init(char *pid, uint32_t *block_size)
{
	union itc_msg *msg;
	uint32_t lmc_len = strlen(pid);

	msg = itc_alloc(sizeof(struct lmc_load_file_init_req) + lmc_len + 1,
	                LMC_LOAD_FILE_INIT_REQ);
	strncpy(msg->load_file_init_req.loadmodule, pid, lmc_len + 1);
	msg->load_file_init_req.connection_ref = server_ref;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(ITC_NOFILTER, 15000, ITC_FROM_ALL);
	if (check_msg(LMC_LOAD_FILE_INIT_CFM, msg))
		goto load_init_failed;

	*block_size = msg->load_file_init_cfm.max_block_size;

	itc_free(&msg);
	return 0;

load_init_failed:
	printf("Load init failed\n");
	if (msg)
		itc_free(&msg);
	return 1;
}

static uint32_t load_data(uint32_t block_size, uint16_t seq_nr, char *buf)
{
	union itc_msg *msg;

	msg = itc_alloc(sizeof(struct lmc_load_file_data_req) + block_size,
	                LMC_LOAD_FILE_DATA_REQ);
	msg->load_file_data_req.lm_block_size = block_size;
	msg->load_file_data_req.lm_seq_nr = seq_nr;
	msg->load_file_data_req.connection_ref = server_ref;
	memcpy(msg->load_file_data_req.lm_block, buf, block_size);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg = itc_receive(ITC_NOFILTER, 15000, ITC_FROM_ALL);
	if (check_msg(LMC_LOAD_FILE_DATA_CFM, msg))
		goto load_data_failed;
	itc_free(&msg);
	return 0;

load_data_failed:
	printf("Load data failed\n");
	if (msg)
		itc_free(&msg);
	return 1;
}

static uint32_t load_end(uint16_t *load_res)
{
	union itc_msg *msg;

	msg = itc_alloc(sizeof(struct lmc_load_file_end_req),
	                LMC_LOAD_FILE_END_REQ);
	msg->load_file_end_req.connection_ref = server_ref;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, 15000, ITC_FROM_ALL);
	if (check_msg(LMC_LOAD_FILE_END_CFM, msg))
		goto load_end_failed;
	*load_res = msg->load_file_end_cfm.result;
	itc_free(&msg);
	return 0;

load_end_failed:
	printf("Load end failed\n");
	if (msg)
		itc_free(&msg);
	return 1;
}

static uint32_t clean_flash(char *pid, struct lmc_load_file_entry *lmc_list,
                            uint32_t lmc_count)
{

	for (int i = 0; i < lmc_count; i++) {
		if (lmc_list[i].state != LMC_STATE_VALID)
			continue;
		if (!strcmp(lmc_list[i].lmid, pid)) {
			if (delete_lmc(pid))
				return 1;
			break;
		}
	}
	return 0;

}

static uint32_t write_test(char *path, char *pid)
{
	uint32_t res = 1;
	uint32_t block_size = 0;
	uint16_t load_res = 0;
	FILE    *fp = NULL;
	int      i, n;
	uint32_t do_close = 0;
	uint32_t bytes_send = 0;
	char    *buf = NULL;
	uint32_t lmc_count = 0;
	struct lmc_load_file_entry *lmc_list = NULL;
	union itc_msg *msg = NULL;
	uint32_t load_filter[] = {1, LMC_LOAD_FILE_DATA_IND};	

	fp = fopen(path, "r");
	if (fp == NULL) {
		printf("Could not find file %s\n", path);
		goto write_test_failed;
	}
	/* get lmc list */
	lmc_list = get_lmc_list(&lmc_count);
	if (!lmc_list)
		goto write_test_failed;
	/* Check if the pid is loaded already, if so, delete it */
	if (clean_flash(pid, lmc_list, lmc_count))
		goto write_test_failed;

	/* load lmc to flash */
	if (load_init(pid, &block_size))
		goto write_test_failed;
	do_close = 1;
	/* loading */
	buf = malloc(block_size);
	if (buf == NULL) {
		printf("failed to alloc buf with block size %u\n", block_size);
		goto write_test_failed;
	}
	i = 0;
	n = fread(buf, sizeof(uint8_t), block_size, fp);
	if (n > 0) {
		bytes_send += n;
	} else if (n < 0) {
		printf("Read got negative value %d", n);
		goto write_test_failed;
	} else if (n == 0) {
		printf("Can't read LMC\n");
		goto write_test_failed;
	}

	while (load_data(block_size, i, buf) == 0) {
		msg = itc_receive(load_filter, 15000, ITC_FROM_ALL);
		if (!msg) {
			printf("Server didn't reply in 15s, load data failed\n");
			goto write_test_failed;
		}
		if (msg->load_file_data_ind.lm_seq_nr != i) {
			printf("Wrong sequence numbers, expected %d, got %d\n",
			       i, msg->load_file_data_ind.lm_seq_nr);
			goto write_test_failed;
		}
		itc_free(&msg);

		if ((i % 100) == 0)
			printf("Wrting to flash, block #%d\n", i/100);
		n = fread(buf, sizeof(uint8_t), block_size, fp);
		if (n > 0) {
			bytes_send += n;
		} else if (n < 0) {
			printf("Got negative read value %d\n", n);
			goto write_test_failed;
		} else if (n == 0) {
			break;
		}
		i++;
	}

	if (load_end(&load_res))
		goto write_test_failed;

	do_close = 0;
	if (load_res != LMC_RESULT_SUCCESS) {
		printf("Load abort\n");
		goto write_test_failed;
	}
	res = 0;
write_test_failed:
	if (msg)
		itc_free(&msg);
	if (fp)
		fclose(fp);
	if (buf)
		free(buf);
	if (lmc_list)
		free(lmc_list);
	if (do_close)
		load_end(&load_res);
	return res;
}

int main(int argc, char **argv)
{
	char mailbox[50];
	char *path = NULL;
	char *pid = NULL;
	int opt;
	uint32_t res;
	uint32_t selected_version;
	uint32_t requested_versions[] = {LMC_SERVER_VERSIONS};
	LMC_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);

	if (argc > 4 || argc < 2) {
		printf("Invalid number of parameters\n");
		print_usage(argv[0]);
		return -1;
	}
	while ((opt = getopt(argc, argv, "hw:")) != -1) {
		switch (opt) {
		case 'h':
			print_usage(argv[0]);
			return 0;
		case 'w':
			test = WRITE;
			path = optarg;
			break;
		default:
			print_usage(argv[0]);
			return -1;
		}
	}

	if (test == WRITE) {
		if (argc == 4) {
			pid = argv[3];
		} else {
			printf("Invalid number of parameters\n");
			print_usage(argv[0]);
			return -1;
		}
	}
	if ((!path) || (!pid)) {
		printf("Invalid parameters\n");
		print_usage(argv[0]);
		return -1;
	}

	/* Set up ITC */
	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Unable to inizalize ITC!\n");
		return -1;
	}
	snprintf(mailbox, sizeof(mailbox), "%s-%d",
	         basename(argv[0]), getpid());
	my_mbox = itc_create_mailbox(mailbox, 0);
	if (my_mbox == ITC_NO_ID) {
		printf("Unable to create ITC mailbox!\n");
		return -1;
	}

	/* Find lmc server */
	server_mbox = itc_locate(LMC_SERVER_NAME);
	if(server_mbox == ITC_NO_ID) {
		printf("Cannot locate the server \"%s\"\n",
		       LMC_SERVER_NAME);
		return -1;
	}

	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);
	if(res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection establish failed (reason:0x%x)\n", res);
		return -1;
	}

	switch (test) {
	case WRITE:
		if (write_test(path, pid)) {
			printf("Write test failed\n");
			goto stop_test;
		}
		break;
	default:
		printf("Unexpect test value\n");
		return -1;
	}
	printf("*** LMC server %s test passed ***\n",
	       (test == WRITE)?"write":"read");
stop_test:
	printf("Quit test");
	return EXIT_SUCCESS;
}

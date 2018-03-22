/**
 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <itc.h>
#include <string.h>

#include "nodeid_server.h"
#include "conn-establish-helper.h"

#define NODEID_CMD_MAILBOX_NUM (1 + 1)
#define NODEID_CMD_MBOX_NAME   "nodeid_cmd"
#define CONN_TMO                5000

#define NODEID_CMD_NAME_STR  "nodeid"
#define NODEID_CMD_USAGE_STR "nodeid <command>|help"
#define NODEID_CMD_DESCR_STR "Manages node id."

#define NODEID_CMD_MAX_LEN   50
static uint32_t client_ref = 6500;
static uint32_t procedure_ref = 0;

static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} nodeid_conn;

union itc_msg {
	uint32_t msgno;
	NODEID_STRUCTS
};

static int msg_check(union itc_msg **msg)
{
	if ((*msg)->any_msg.procedure_ref != procedure_ref) {
		printf("Server replied with invalid procedure_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       procedure_ref, (*msg)->any_msg.procedure_ref);
		itc_free(msg);
		return -1;
	}
	if ((*msg)->any_msg.connection_ref != client_ref) {
		printf("Server replied with invalid connection_ref; "
		       "expected 0x%08x, received 0x%08x\n",
		       client_ref, (*msg)->any_msg.connection_ref);
		itc_free(msg);
		return -1;
	}

	return 0;
}

static void usage()
{
	printf("NAME\n\r");
	printf("    %s - %s\n\r", NODEID_CMD_NAME_STR, NODEID_CMD_DESCR_STR);
	printf("\n\r");
	printf("SYNOPSIS:\n\r");
	printf("    %s\n\r", NODEID_CMD_USAGE_STR);
	printf("\n\r");
	printf("DESCRIPTION\n\r");
	printf("    This command gives the possibility to read, write or delete the node id.\n\r");
	printf("\n\r");
	printf("OPERANDS\n\r");
	printf("    <command ...>\n\r");
	printf("    -------------\n\r");
	printf("    help                       This text.\n\r");
	printf("    read                       Read node id, hexadecimal output.\n\r");
	printf("    write <byte> <byte>...     Write node id, 1-50 bytes in hexadecimal.\n\r");
	printf("    delete                     Delete node id.\n\r");
	printf("\n\r");
}

static int readnodeid(void)
{
	union itc_msg *msg_p = NULL;
	uint32_t i;
	int ret = 0;
	static uint32_t rx_filter[] = {2,
	                               NODE_ID_READ_CFM,
	                               NODE_ID_READ_REJ
	                               };

	/* Send request. */
	msg_p = itc_alloc(sizeof(struct nodeid_read_req), NODE_ID_READ_REQ);
	msg_p->nodeid_read_req.connection_ref = nodeid_conn.server_ref;
	msg_p->nodeid_read_req.procedure_ref = ++procedure_ref;

	itc_send(&msg_p, nodeid_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg_p = itc_receive(rx_filter, CONN_TMO, nodeid_conn.server_mbox);

	if (!msg_p) {
		printf("Timeout! Server did not reply within 5 seconds\n");
		ret = -1;
		goto exit;
	}

	if (msg_check(&msg_p)) {
		ret = -1;
		goto exit;
	}
	switch (msg_p->msgno) {
		case NODE_ID_READ_CFM:
			printf("Node id (bytes in hexadecimal):\n\r");
			for (i = 0; i < msg_p->nodeid_read_cfm.length; i++) {
				printf("%02x ", msg_p->nodeid_read_cfm.node_id[i]);
			}
			printf("\n\r");
			break;

		case NODE_ID_READ_REJ:
			ret = msg_p->nodeid_erase_rej.error_code;
			if (ret == NODEID_RESULT_NOT_FOUND) {
				printf("Node id does not exist.\n\r");
			} else {
				printf("Error when trying to read node id, error=%u.\n\r",
				      ret);
			}
			break;
	}

exit:
	if (msg_p)
		itc_free(&msg_p);
	return ret;
}

static int writenodeid(char** node_arg, uint32_t len)
{
	union itc_msg *msg_p = NULL;
	uint32_t i;
	uint8_t node_arr[NODEID_CMD_MAX_LEN];
	int ret = 0;
	unsigned long value;
	char *endptr;

	static uint32_t rx_filter[] = {2,
	                                NODE_ID_WRITE_CFM,
	                                NODE_ID_WRITE_REJ
	                               };

	if (len > NODEID_CMD_MAX_LEN) {
		printf("Too many bytes (%u). Type '%s help'.\n\r", len, NODEID_CMD_NAME_STR);
		goto exit;
	}

	for (i = 0; i < len; i++) {

		value = strtoul(node_arg[i], &endptr, 16);

		if ((*endptr != '\0') || (value & 0xffffff00)) {
			printf("Strtoul error or "
			       "cannot split input into array.\n"
			       "Type '%s help'.\n\r", NODEID_CMD_NAME_STR);
			ret = -1;
			goto exit;
		}

		node_arr[i] = (uint8_t)value;
	}

	/* Send request. */
	msg_p = itc_alloc(offsetof(struct nodeid_write_req, node_id)
	                 + len, NODE_ID_WRITE_REQ);
	msg_p->nodeid_write_req.connection_ref = nodeid_conn.server_ref;
	msg_p->nodeid_write_req.procedure_ref = ++procedure_ref;
	msg_p->nodeid_write_req.length = len;
	memcpy(msg_p->nodeid_write_req.node_id, node_arr, len);

	itc_send(&msg_p, nodeid_conn.server_mbox, ITC_MY_MBOX);

	/* Receive answer. */
	msg_p = itc_receive(rx_filter, CONN_TMO, nodeid_conn.server_mbox);

	if (!msg_p) {
		printf("Timeout! Server did not reply within 5 seconds\n");
		ret = -1;
		goto exit;
	}

	if (msg_check(&msg_p)) {
		ret = -1;
		goto exit;
	}
	switch (msg_p->msgno) {
		case NODE_ID_WRITE_CFM:
			break;

		case NODE_ID_WRITE_REJ:
			ret = msg_p->nodeid_write_rej.error_code;
			if (ret == NODEID_RESULT_NOT_FOUND) {
				printf("Not possible to write node id, area full.\n\r");
			} else {
				printf("Error when trying to write node id, error=%u.\n\r",
				    ret);
			}
			break;
	}
exit:
	if (msg_p)
		itc_free(&msg_p);
	return ret;
}

static int delete_node_id(void)
{
	union itc_msg *msg_p = NULL;
	int ret = 0;

	static uint32_t rx_filter[] = { 2,
	                                NODE_ID_ERASE_CFM,
	                                NODE_ID_ERASE_REJ };

	msg_p = itc_alloc(sizeof(struct nodeid_erase_req), NODE_ID_ERASE_REQ);
	msg_p->nodeid_erase_req.connection_ref = nodeid_conn.server_ref;
	msg_p->nodeid_erase_req.procedure_ref = ++procedure_ref;

	itc_send(&msg_p, nodeid_conn.server_mbox, ITC_MY_MBOX);
	msg_p = itc_receive(rx_filter, CONN_TMO, nodeid_conn.server_mbox);
	if (!msg_p) {
		printf("Timeout! Server did not reply within 5 seconds.\n");
		ret = -1;
		goto exit;
	}

	if (msg_check(&msg_p)) {
		ret = -1;
		goto exit;
	}

	switch (msg_p->msgno) {
		case NODE_ID_ERASE_CFM:
			break;

		case NODE_ID_ERASE_REJ:
			ret = msg_p->nodeid_erase_rej.error_code;
			printf("Error when trying to delete node id, "
			    "error=%u.\n\r", ret);
			break;
	}

exit:
	if (msg_p)
		itc_free(&msg_p);
        return ret;

}

int main(int argc, char *argv[])
{
	uint32_t res = 0;
	uint32_t requested_versions[] = {NODEID_SERVER_VERSIONS};
	NODEID_CONN_ESTABLISH_MSG_STRUCT(conn_messages);
	int ret = 0;
	itc_mbox_id_t my_mbox = ITC_NO_ID;
	if (argc < 2) {
		printf("Wrong number of arguments. Type '%s help'.\n\r", NODEID_CMD_NAME_STR);
		exit(1);
	}

	if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "help") == 0) {
		usage();
		return 0;
	}

	if (itc_init(NODEID_CMD_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Failed to initialize itc\n");
		return 1;
	}

	if ((my_mbox = itc_create_mailbox(NODEID_CMD_MBOX_NAME, 0)) == ITC_NO_ID) {
		printf("Failed to create itc mailbox\n");
		return 1;
	}

	nodeid_conn.server_mbox = itc_locate(NODEID_SERVER_NAME);
	if (nodeid_conn.server_mbox == ITC_NO_ID) {
		printf("Cannot locate the server \"%s\"\n", NODEID_SERVER_NAME);
		return 1;
	}

	/*Connect to the server*/
	res = conn_establish(/*input parameters*/
	                     nodeid_conn.server_mbox,
	                     ++procedure_ref,
	                     client_ref,
	                     sizeof(requested_versions) /
	                     sizeof(requested_versions[0]),
	                     requested_versions,
	                     &conn_messages,
	                     CONN_TMO,
	                     /*returned values*/
	                     &nodeid_conn.server_ref,
	                     &nodeid_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection establish failed with reason:0x%x\n", res);
		return 1;
	}

	if (strcmp(argv[1], "read") == 0) {
		ret = readnodeid();
	} else if (strcmp(argv[1], "write") == 0) {
		if (argc < 3) {
			printf("Too few bytes. Type '%s help'.\n\r", NODEID_CMD_NAME_STR);
			usage();
			return 1;
		}
		ret = writenodeid(argv + 2, argc - 2);
	} else if (strcmp(argv[1], "delete") == 0) {
		ret = delete_node_id();
	} else {
		printf("Wrong argument. Type '%s help'.\n\r", NODEID_CMD_NAME_STR);
	}

	res = conn_disconnect(nodeid_conn.server_mbox,
	                      ++procedure_ref,
	                      nodeid_conn.server_ref,
	                      &conn_messages,
	                      CONN_TMO);
	if (res != CONN_ESTABLISH_SUCCESS) {
		printf("Connection disconnect failed (reason:0x%08x)", res);
		return 1;
	}

	if (my_mbox != ITC_NO_ID)
		itc_delete_mailbox(my_mbox);

	return ret;
}

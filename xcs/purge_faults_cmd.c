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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <itc.h>
#include <getopt.h>
#include "xcbc_fault_if.h"
#include "conn-establish-helper.h"

#define MAX_MAILBOX_NUM 32
#define PURGE_CMD_MBOX  "purge-faults"
#define CONN_TMO 1000

static uint32_t client_ref = 1122; /* dummy value */
struct {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} fault_server;
static struct conn_establish_msg_numbers conn_messages = {
	XCBC_FAULT_CONN_ESTABLISH_REQ,
	XCBC_FAULT_CONN_ESTABLISH_CFM,
	XCBC_FAULT_CONN_ESTABLISH_REJ,
	XCBC_FAULT_CONN_DISCONNECT_REQ,
	XCBC_FAULT_CONN_DISCONNECT_CFM,
	XCBC_FAULT_CONN_DISCONNECT_REJ,
	XCBC_FAULT_CONN_MONITOR_FWD
};

union itc_msg {
	uint32_t       msgno;
	conn_any_msg_t any_msg;
};

/**
 * Function print_usage
 */
static void print_usage(void)
{
	printf( "purge_faults\n"
	        "       Purge all faults in the fault list.\n\n"
	        "-h\n"
	        "       Display usage information.\n\n"
	      );
}


/**
 * Function conn_init
 */
static uint32_t conn_init(void)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {FAULT_SERVER_VERSIONS};
	uint32_t res;

	res = conn_establish(
	              /*input parameters*/
	              fault_server.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_TMO,
	              /*returned values*/
	              &fault_server.server_ref,
	              &fault_server.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		printf("Client:Connection establish failed."
		       " (reason:0x%08x)\n", res);
		return res;
	}

	return 0;
}

/**
 * Function purge_cmd_init
 */
static uint32_t purge_cmd_init(void)
{
	uint32_t res = 1;
	int ret;

	memset(&fault_server, 0, sizeof(fault_server));
	/* Initialize ITC and create mailbox */
	ret = itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if(ret) {
		printf("itc_init failed %d", ret);
		return res;
	}
	if (itc_create_mailbox(PURGE_CMD_MBOX, 0) == ITC_NO_ID) {
		printf("Failed to create mailbox %s\n", PURGE_CMD_MBOX);
		return res;
	}
	/* get fault server mailbox */
	fault_server.server_mbox = itc_locate(FAULT_SERVER_MAILBOX);
	if (fault_server.server_mbox == ITC_NO_ID) {
		printf("Cannot find FAULT_SERVER_MBOX\n");
		return res;
	}

	res = conn_init();
	return res;
}

/**
 * Function handle_purge_cmd
 */
static void handle_purge_cmd(void)
{
	union itc_msg *msg;

	msg = itc_alloc(sizeof(struct xcbc_purge_faults_ind),
	                XCBC_PURGE_FAULTS_IND);
	msg->any_msg.connection_ref = fault_server.server_ref;
	itc_send(&msg, fault_server.server_mbox, ITC_MY_MBOX);

}

/**
 * Function main
 */
int main(int argc, char **argv)
{
	uint32_t res;


	if (argc >= 2) {
		print_usage();
		return 0;
	}

	res = purge_cmd_init();
	if (res)
		return EXIT_FAILURE;

	handle_purge_cmd();

	return EXIT_SUCCESS;
}

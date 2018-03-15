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

#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include "itc.h"
#include "hwli.h"
#include "xcbc_fault_if.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER     com_ericsson_xcs_bpar_check
#include "tpt_create.h"
#include "tpt.h"

#define MAX_NUM_OF_MAILBOXS    32
#define BPAR_CHECK_MBOX_NAME   "BPAR_CHECK_MBOX"
#define CONN_ESTABLISH_TMO     1000

static struct conn_establish_msg_numbers fault_conn_messages = {
	XCBC_FAULT_CONN_ESTABLISH_REQ,
	XCBC_FAULT_CONN_ESTABLISH_CFM,
	XCBC_FAULT_CONN_ESTABLISH_REJ,
	XCBC_FAULT_CONN_DISCONNECT_REQ,
	XCBC_FAULT_CONN_DISCONNECT_CFM,
	XCBC_FAULT_CONN_DISCONNECT_REJ,
	XCBC_FAULT_CONN_MONITOR_FWD
};

union itc_msg {
	uint32_t msgno;
	XCBC_FAULT_STRUCTS
};
static uint32_t server_ref;
static uint32_t selected_version;
static uint32_t initialized = 0;

static void print_usage(void)
{
	printf("This program is checking existence of mandatory board parameters.\n\n"
	       "Usage: bpar_check <options>\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n\n");
}

static int connect_fault_server(itc_mbox_id_t server_mbox)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {FAULT_SERVER_VERSIONS};
	uint32_t client_ref = 0x01;
	uint32_t res;

	if(initialized) {
		return 0;
	}

	res = conn_establish(
	              /*input parameters*/
	              server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) /
	              sizeof(requested_versions[0]),
	              requested_versions,
	              &fault_conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &server_ref,
	              &selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed"
		              " (reason:0x%08x)",
		              res));
		return -1;
	}
	initialized = 1;
	return 0;

}
static int get_mbox(itc_mbox_id_t *fault_mbox)
{
	*fault_mbox = itc_locate(FAULT_SERVER_MAILBOX);
	if (*fault_mbox == ITC_NO_ID) {
		TPT_ERROR(FAULT_SERVER_MAILBOX" does not exist");
		return -1;
	}
	return 0;
}

static int report_fault(uint16_t fault_type,
                        uint16_t fault_recovery_action,
                        char *fault_description)
{
	union itc_msg *out_msg;
	uint32_t server_mbox;

	TPT_TRACE(1, STR("Fault (type=%d recovAct=%d descr=%s)",
	                 fault_type, fault_recovery_action,
	                 fault_description ? fault_description : ""));

	if(get_mbox(&server_mbox)) {
		TPT_ERROR("report fault failed since fault server mailbox "
		          "can not be found.");
		return -1;
	}

	if(connect_fault_server(server_mbox)) {
		TPT_ERROR("report fault failed since fault server can "
		          "not be connected.");
		return -1;
	}

	out_msg = itc_alloc(sizeof(struct xcbc_report_fault),
	                    XCBC_REPORT_FAULT);
	out_msg->report_fault.fault_type = fault_type;
	out_msg->report_fault.recov_act = fault_recovery_action;
	strncpy(out_msg->report_fault.fault_description, fault_description,
	        XCBC_MAX_FAULT_DESCR_LEN);
	out_msg->report_fault.fault_description[XCBC_MAX_FAULT_DESCR_LEN - 1] =
	        '\0';
	out_msg->report_fault.connection_ref = server_ref;

	TPT_SEND_SIG(out_msg->msgno, server_mbox, "Send XCBC_REPORT_FAULT to "
	             FAULT_SERVER_MAILBOX);
	itc_send(&out_msg, server_mbox, ITC_MY_MBOX);

	return 0;
}

static int bpar_check(void)
{
	const char *mandatory_bpar[] = {
		"SYS_HW_PID",
		"SYS_HW_REV",
		"SYS_HW_NAME",
		"SYS_HW_MARKET_NAME",
		"SYS_HW_DATE",
		"SYS_HW_SERIAL"
	};

	char *lf_pid = NULL;
	int result = 0;
	char *env_value = NULL;
	int i;

	lf_pid = getenv("SYS_LF_PID");

	if(lf_pid == NULL) {
		lf_pid = " ";
	}

	for(i = 0; i < sizeof(mandatory_bpar) / sizeof(mandatory_bpar[0]);
	    i++) {
		env_value = getenv(mandatory_bpar[i]);
		if(env_value == NULL) {
			/* report fault_descr to fault_server */
			char fault_descr[XCBC_MAX_FAULT_DESCR_LEN];
			snprintf(fault_descr, sizeof(fault_descr),
			         "201;%s;Missing board parameter(s) in FLASH "
			         "memory.(e.g. %s)", lf_pid, mandatory_bpar[i]);
			fault_descr[XCBC_MAX_FAULT_DESCR_LEN - 1] = '\0';

			TPT_ERROR(fault_descr);
			result += report_fault(XCBC_GENERAL_HW_ERROR,
			             XCBC_ENTITY_RESTART,
			             fault_descr);

			/* write hwli_msg in hwlog */
			char hwli_msg[HWLI_MESSAGE_SZ];
			snprintf(hwli_msg, sizeof(hwli_msg),
			         "%s;Missing board parameter(s) in FLASH "
			         "memory.(e.g. %s)", lf_pid, mandatory_bpar[i]);
			hwli_msg[HWLI_MESSAGE_SZ - 1] = '\0';

			if(hwli_write("201", 4, 0, hwli_msg)) {
				TPT_ERROR("call hwli_write report fault "
				          "failed");
			}
		} else {
			TPT_INFO(STR("%s = %s", mandatory_bpar[i], env_value));
		}
	}
	return result;
}


static int init_itc(itc_mbox_id_t *mbox_id)
{
	int ret = itc_init(MAX_NUM_OF_MAILBOXS,
	                   ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if(ret) {
		TPT_ERROR(STR("itc_init failed %d", ret));
		return -1;
	}

	*mbox_id = itc_create_mailbox(BPAR_CHECK_MBOX_NAME, 0);
	if(*mbox_id == ITC_NO_ID) {
		TPT_ERROR(STR("failed to create %s", BPAR_CHECK_MBOX_NAME));
		return -1;
	}
	return 0;
}

static void delete_itc(itc_mbox_id_t mbox)
{
	if( mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}
}

int main(int argc, char **argv)
{

	int ret = 0;
	itc_mbox_id_t mbox = ITC_NO_ID;

	if (argc > 1) {
		if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		} else {
			print_usage();
			exit(-EINVAL);
		}
	}

	ret = init_itc(&mbox);
	if(!ret) {
		ret = bpar_check();
	}

	delete_itc(mbox);
	return ret;
}

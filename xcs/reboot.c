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
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <sys/reboot.h>

#include <uio_helper.h>
#include <llog.h>
#include "evti.h"
#include "lmc_server.h"
#include "libreboot.h"
#include "libresetmem.h"
#include "booti_restart.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_libreboot
#include <tpt_create.h>
#include <tpt.h>


#define CURRENT_LMC_ENV                 "SYS_LF_PID"

#define LMC_AUBOOT BOOTI_RESTART_LMC_AUBOOT

#define XLL_DISTRIBUTE_TMO      10000
#define CONN_ESTABLISH_TMO      0

static struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
} lmc_conn;

static uint32_t client_ref = 0x234;
static uint32_t procedure_ref = 0;

static uint32_t reboot_init(void);

union itc_msg {
	uint32_t msgno;
	EVTI_DistributeIndS ind;
	EVTI_DistributeReqS req;
	EVTI_DistributeCfmS cfm;
	EVTI_DistributeRejS rej;
	conn_any_msg_t any_msg;
	LMC_SERVER_MESSAGES;
};

/**
 * Check reply msg from LMC server
 */
static uint32_t msg_check(union itc_msg *msg)
{
	uint32_t res = 1;
	if (!msg) {
		TPT_ERROR(STR("\"%s\" did not reply within 5 seconds",
		              LMC_SERVER_NAME));
		goto check_end;
	}

	if (msg->any_msg.procedure_ref != procedure_ref) {
		TPT_ERROR(STR("\"%s\" replied with invalid procedure_ref; "
		              "expected 0x%08x, received 0x%08x",
		              LMC_SERVER_NAME,
		              procedure_ref, msg->any_msg.procedure_ref));
		goto check_end;
	}

	if (msg->any_msg.connection_ref != client_ref) {
		TPT_ERROR(STR("\"%s\" replied with invalid connection_ref; "
		              "expected 0x%08x, received 0x%08x",
		              LMC_SERVER_NAME,
		              client_ref, msg->any_msg.connection_ref));
		goto check_end;
	}

	if (msg->msgno != LMC_GET_LOAD_FILE_INFO_CFM) {
		TPT_ERROR(STR("\"%s\" replied with unexpected message; "
		              "message number: 0x%08x", LMC_SERVER_NAME,
		              msg->msgno));
		goto check_end;
	}

	res = 0;
check_end:
	return res;
}

/**
 * Distribute the restart tag
 */
static void restart_notice(char *tag, uint32_t timeout)
{
	union itc_msg *msg = NULL;
	uint32_t rx_filter[] = {2, EVTI_DISTRIBUTE_CFM, EVTI_DISTRIBUTE_REJ};
	itc_mbox_id_t server_mbox;

	server_mbox = itc_locate(EVTI_SERVER_NAME);
	if (server_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Cannot locate the server \"%s\"",
		              EVTI_SERVER_NAME));
		goto clean_up;
	}

	msg = itc_alloc(sizeof(EVTI_DistributeReqS), EVTI_DISTRIBUTE_REQ);
	snprintf(msg->ind.tag, EVTI_MAX_TAG_LENGTH, tag);
	msg->ind.data[0] = '\0';

	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(rx_filter, timeout, ITC_FROM_ALL);
	if (msg) {
		switch(msg->msgno) {
		case EVTI_DISTRIBUTE_CFM:
			TPT_INFO("Distribute confirm.");
			break;
		case EVTI_DISTRIBUTE_REJ:
			TPT_ERROR("Distribute reject.");
			break;
		default:
			TPT_ERROR(STR("Received unknown message (0x%08x).",
			              msg->msgno));
		}
	} else {
		TPT_ERROR("No application response on restart distribute.");
	}
clean_up:
	if (msg)
		itc_free(&msg);
	return;
}

/**
 * Get LMC slot number from production ID
 */
static int32_t get_lmc_from_pid(char *loadmodule)
{
	uint32_t i;
	uint32_t slot_number = LMC_AUBOOT;
	struct lmc_load_file_entry *lmc_list;
	union itc_msg *msg = NULL;
	uint32_t rx_filter[] = {1, LMC_GET_LOAD_FILE_INFO_CFM};

	if (loadmodule == NULL)
		goto glm_end;
	/*Get the list of LMCs*/
	msg = itc_alloc(sizeof(struct lmc_get_load_file_info_req),
	                LMC_GET_LOAD_FILE_INFO_REQ);
	msg->get_load_file_info_req.procedure_ref = ++procedure_ref;
	msg->get_load_file_info_req.connection_ref = lmc_conn.server_ref;
	msg->get_load_file_info_req.flags = 0;
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	msg = itc_receive(rx_filter, ITC_NO_TMO, lmc_conn.server_mbox);
	if (msg_check(msg))
		goto glm_end;

	lmc_list = msg->get_load_file_info_cfm.info;

	for (i = 0; i < msg->get_load_file_info_cfm.lmc_count; i++) {
		if (lmc_list[i].state != LMC_STATE_VALID)
			continue;
		if (!strcmp(loadmodule, lmc_list[i].lmid)) {
			slot_number = i;
			break;
		}
	}

glm_end:
	if (msg)
		itc_free(&msg);
	return slot_number;
}

/**
 * Check if LMC ID is valid
 */
static int check_lmc(uint32_t slot_number)
{
	int res = 1;
	struct lmc_load_file_entry *lmc_list;
	union itc_msg *msg = NULL;
	uint32_t rx_filter[] = {1, LMC_GET_LOAD_FILE_INFO_CFM};

	/*Get the list of LMCs*/
	msg = itc_alloc(sizeof(struct lmc_get_load_file_info_req),
	                LMC_GET_LOAD_FILE_INFO_REQ);
	msg->get_load_file_info_req.procedure_ref = ++procedure_ref;
	msg->get_load_file_info_req.connection_ref = lmc_conn.server_ref;
	msg->get_load_file_info_req.flags = 0;
	itc_send(&msg, lmc_conn.server_mbox, ITC_MY_MBOX);

	msg = itc_receive(rx_filter, ITC_NO_TMO, lmc_conn.server_mbox);
	if (msg_check(msg))
		goto clm_end;

	lmc_list = msg->get_load_file_info_cfm.info;

	if (slot_number >= msg->get_load_file_info_cfm.lmc_count)
		goto clm_end;

	if (lmc_list[slot_number].state == LMC_STATE_VALID)
		res = 0;

clm_end:
	if (msg)
		itc_free(&msg);
	return res;
}

/**
 * Managed restart
 */
int managed_reboot(char *program, char *reason, char *loadmodule,
                   int32_t slot_number, bool use_slot, uint32_t reboot_type)
{
	char *current_lmc = getenv(CURRENT_LMC_ENV);

	if (use_slot) {
		if (reboot_init())
			return LIBREBOOT_SERVER_ERROR;
		if (slot_number >= 0) {
			if (check_lmc(slot_number)) {
				TPT_ERROR(STR("LMC %d is not valid", slot_number));
				return LIBREBOOT_LMC_INVALID;
			}
		}
	}
	else {
		/* Check loadmodule */
		if ((loadmodule == NULL) || (strlen(loadmodule) == 0))
			loadmodule = current_lmc;
		if (reboot_init())
			slot_number = LMC_AUBOOT;
		else
			slot_number = get_lmc_from_pid(loadmodule);
	}

	if (reboot_type == BOOTI_RESTART_TYPE_ORDERED)
		llog_write("Ordered restart", program, 0, "Cold", 0, NULL, reason);

	/* distribute restart tag */
	restart_notice(XLL_RESTART, XLL_DISTRIBUTE_TMO);
	resetmem_restart_board(slot_number, reboot_type);

	return 0;
}

/**
 * Restart board with given name
 */
void reboot_with_pid(char *program, char *reason, char *loadmodule)
{
	(void) managed_reboot(program, reason, loadmodule, -1, false,
	                      BOOTI_RESTART_TYPE_ORDERED);
}

/**
 * Restart board with given LMC ID
 */
int reboot_with_slot(char *program, char *reason, int32_t slot_number)
{
	return managed_reboot(program, reason, NULL, slot_number, true,
	                      BOOTI_RESTART_TYPE_ORDERED);
}

/**
 * Reboot immediatly with the youngest AUBOOT
 */
void reboot_immediate(char *program, char *reason)
{
	int32_t slot_number = LMC_AUBOOT;

	llog_write("Ordered restart", program, 0, "Cold", 0, NULL, reason);
	resetmem_restart_board(slot_number, BOOTI_RESTART_TYPE_ORDERED_IMMEDIATE);
}

/**
 * Set up connection to LMC server
 */
static uint32_t reboot_init(void)
{
	uint32_t requested_versions[] = {LMC_SERVER_VERSIONS};
	LMC_CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	uint32_t res = 1;

	/*Find the LMC server*/
	lmc_conn.server_mbox = itc_locate(LMC_SERVER_NAME);
	if (lmc_conn.server_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Cannot locate the server \"%s\"",
		          LMC_SERVER_NAME));
		goto init_end;
	}

	/*Connect to the server*/
	res = conn_establish(
	              /*input parameters*/
	              lmc_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_ESTABLISH_TMO,
	              /*returned values*/
	              &lmc_conn.server_ref,
	              &lmc_conn.selected_version);
	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Connection to \"%s\" failed(reason:0x%x)",
		              LMC_SERVER_NAME, res));
	}

init_end:
	return res;
}

/**
 * Reboot crash
 * reboot on current LMC after program crash
 */
void reboot_crash(char *program, pid_t pid, int signal, char *pmd)
{
	int32_t slot_number = LMC_AUBOOT;

	llog_write("Program Crash", NULL, 0, "Cold", 0, NULL, NULL);
	resetmem_restart_board(slot_number, BOOTI_RESTART_TYPE_CRASH);
}

int reboot_with_test_on_slot(char *program, char *reason, int32_t slot_number)
{
	return managed_reboot(program, reason, NULL, slot_number, true,
	                      BOOTI_RESTART_TYPE_ORDERED_WITH_TEST);
}

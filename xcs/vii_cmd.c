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
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <itc.h>
#include "rhd-vii-if.h"
#include "rhd-common.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER  com_ericsson_xcs_rhd_vii_cmd
#include "tpt_create.h"
#include "tpt.h"

#define MAX_MAILBOX_NUM 32
#define VII_CMD_MBOX    "vii-cmd"
#define CONN_TMO 1000

static uint32_t client_ref = 1111; /* dummy value */
static struct server_info vii_conn;

union itc_msg {
	uint32_t        msgno;
	conn_any_msg_t  any_msg;
	RHD_VII_STRUCTS
};

static struct conn_establish_msg_numbers conn_messages = {
	VII_CONN_ESTABLISH_REQ,
	VII_CONN_ESTABLISH_CFM,
	VII_CONN_ESTABLISH_REJ,
	VII_CONN_DISCONNECT_REQ,
	VII_CONN_DISCONNECT_CFM,
	VII_CONN_DISCONNECT_REJ,
	VII_CONN_MONITOR_FWD
};

/**
 * Function print_usage
 */
static void print_usage(void)
{
	printf( "vii\n"
	        "       Print the current LEDs state\n\n"
	        "-h\n"
	        "       Display usage information.\n\n"
	        "[d]\n"
	        "       'vii_cmd d' gives more details.\n\n"
	      );
}

/**
 * Function conn_init
 */
static uint32_t conn_init(void)
{
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {VII_SERVER_VERSIONS};
	uint32_t res;

	res = conn_establish(
	              /*input parameters*/
	              vii_conn.server_mbox,
	              ++procedure_ref,
	              client_ref,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &conn_messages,
	              CONN_TMO,
	              /*returned values*/
	              &vii_conn.server_ref,
	              &vii_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("Client:Connection establish failed"
			      "(reason:0x%08x)\n", res));
		return res;
	}

	TPT_TRACE(1, STR("mailbox id for VII: %u "
			 "server connection ref:%u "
			 "selected version: %u\n",
	                 vii_conn.server_mbox,
	                 vii_conn.server_ref,
	                 vii_conn.selected_version));

	return 0;

}

/**
 * Function vii_cmd_init
 */
static uint32_t vii_cmd_init(void)
{
	uint32_t res = 1;

	memset(&vii_conn, 0, sizeof(vii_conn));
	/* Initialize ITC and create mailbox */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if (itc_create_mailbox(VII_CMD_MBOX, 0) == ITC_NO_ID) {
		printf("Failed to create mailbox %s\n", VII_CMD_MBOX);
		return res;
	}
	/* get VII daemon mailbox */
	vii_conn.server_mbox = itc_locate(RHD_VII_MAILBOX);
	if (vii_conn.server_mbox == ITC_NO_ID) {
		printf("Cannot find RHD_VII_MBOX\n");
		return res;
	}

	res = conn_init();
	return res;
}

/**
 * Function display_red_led
 */
static void display_red_led(uint32_t ind)
{
	printf("  Red:    \n");
	if (ind == VII_FAULT)
		printf("    VII_FAULT    (steady light)\n\r");
	else if (ind == VII_ERROR)
		printf("    VII_ERROR    (0.5 hz)\n\r");
	else
		printf("    Default      (off)\n\r");
}

/**
 * Function display_green_led
 */
static void display_green_led(uint32_t ind)
{
	uint32_t tmp_mask = 0;

	printf("  Green:  \n");

	tmp_mask = ind & (~VII_O_LOADING_SW);

	if (VII_NO_POWER == tmp_mask) {
		printf("    VII_NO_POWER"
		       "        (off)\n\r");
	} else if (VII_O_BUSY == tmp_mask) {
		(ind & VII_O_LOADING_SW) ?
		printf("    VII_O_BUSY_LOADING_SW"
		       "        (16 hz + double flash)\n\r") :
		printf("    VII_BOOTTEST_START or VII_O_BUSY"
		       "        (16 hz)\n\r");

	} else if (VII_LOADTEST_START == tmp_mask) {
		(ind & VII_O_LOADING_SW) ?
		printf("    VII_LOADTEST_START_LOADING_SW"
		       "        (2 hz + double flash)\n\r") :
		printf("    VII_LOADTEST_START"
		       "        (2 hz)\n\r");

	} else if (VII_MISSING_RESOURCE_START == tmp_mask) {
		(ind & VII_O_LOADING_SW) ?
		printf("    VII_MISSING_RESOURCE_START_LOADING_SW"
		       "        (0.5 hz + double flash)\n\r") :
		printf("    VII_MISSING_RESOURCE_START"
		       "        (0.5 hz)\n\r");
	} else {
		(ind == VII_POWER_LOADING_SW) ?
		printf("    VII_POWER_LOADING_SW"
		       "        (steady light + double flash)\n\r") :
		printf("    Default"
		       "        (steady light)\n\r");
	}
}

/**
 * Function display_blue_led
 */
static void display_blue_led(uint32_t ind)
{
	printf("  Blue: \n");
	if (VII_BOARD_LOCKED == ind) {
		printf("    VII_BOARD_LOCKED or\n");
		printf("    VII_M_FULL_MAINTENANCE_MODE"
		       "        (steady light)\n\r");
	} else if (VII_SHUTDOWN_START == ind) {
		printf("    VII_SHUTDOWN_START or\n");
		printf("    VII_M_REMOVING_TRAFFIC"
		       "        (0.5 hz)\n\r");
	} else if (VII_BOARD_BUSY_START == ind) {
		printf("    VII_BOARD_BUSY_START");
		printf("        (16 hz)\n\r");
	} else if (VII_M_ALARMS_SUPPRESSED == ind) {
		printf("    VII_M_ALARMS_SUPPRESSED");
		printf("        (16 hz)\n\r");
	} else {
		printf("    Default"
		       "        (off)\n\r");
	}
}

/**
 * Function display_yellow_led
 */
#ifdef SPECIAL_LED
static void display_yellow_led(uint32_t ind)
{
	printf("  Yellow: \n");
	if (ind == VII_OFF) {
		printf("    (off)\n\r");
	} else if (ind == VII_STEADY) {
		printf("    (steady light)\n\r");
	} else if (ind == VII_05HZ) {
		printf("    (0.5 hz)\n\r");
	} else { /* Should never happen */
		printf("    UNEXPECTED  (unknown)\n\r");
	}
}
#endif
/**
 * Function display_led_state
 */
static void display_led_state(struct leds req_masks)
{
	char* boardtype;
	printf("Indicate led states:\n\r");
	/*Fault LED*/
	display_red_led(req_masks.red);
	/*Operation LED*/
	display_green_led(req_masks.green);
	/*Maintenance LED*/
	if ((boardtype = getenv("SYS_BOARD_TYPE")) == NULL) {
		TPT_ERROR("SYS_BOARD_TYPE not found, aborting");
        }
	TPT_TRACE(1, STR("boardtype is: %s\n\r",boardtype));
	if (strcmp(boardtype ,"BP") == 0) {
		display_blue_led(req_masks.blue);
	}
	/*Special LED*/
#ifdef SPECIAL_LED
	display_yellow_led(req_masks.yellow);
#endif
}

static void display_led_request(uint32_t ind, itc_mbox_id_t mbox)
{
	char client_name[MAX_NAME_LEN];

	if (mbox != ITC_NO_ID) {
		if (itc_get_name(mbox, client_name, MAX_NAME_LEN))
			printf("Requests made by client: %s\n\r", client_name);
		else
			printf("Requests made by client: %d\n\r", mbox);
	}
	/* print the request made by the client */
	if ((ind & VII_FAULT) != 0)
		printf("  VII_FAULT\n\r");

	if ((ind & VII_ERROR) != 0)
		printf("  VII_ERROR\n\r");

	if ((ind & VII_NO_POWER) != 0)
		printf("  VII_NO_POWER\n\r");

	if ((ind & VII_BOOTTEST_START) != 0)
		printf("  VII_BOOTTEST or VII_O_BUSY\n\r");

	if ((ind & VII_LOADTEST_START) != 0)
		printf("  VII_LOADTEST\n\r");

	if ((ind & VII_MISSING_RESOURCE_START) != 0)
		printf("  VII_MISSING_RESOURCE\n\r");

	if ((ind & VII_O_LOADING_SW) != 0)
		printf("  VII_O_LOADING_SW\n\r");

	if ((ind & VII_BOARD_LOCKED) != 0)
		printf("  VII_BOARD_LOCKED or \
                        VII_M_FULL_MAINTENANCE_MODE\n\r");

	if ((ind & VII_SHUTDOWN_START) != 0)
		printf("  VII_SHUTDOWN or \
                        VII_M_REMOVING_TRAFFIC\n\r");

	if ((ind & VII_BOARD_BUSY_START) != 0)
		printf("  VII_BOARD_BUSY\n\r");

	if ((ind & VII_M_ALARMS_SUPPRESSED) != 0)
		printf("  VII_M_ALARMS_SUPPRESSED\n\r");
}
/**
 * Function display_led_stateX
 */
static void display_led_stateX(void)
{
	union itc_msg *msg;
#ifdef SPECIAL_LED
	char client_name[MAX_NAME_LEN];
#endif
	uint32_t rx_filter[] = {1, RHD_VII_LED_CMD_CFM};
	uint32_t timeout = 10000;

	msg = itc_alloc(sizeof(conn_any_msg_t), RHD_VII_LED_CMD_REQ);
	msg->any_msg.connection_ref = vii_conn.server_ref;
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);

	printf("Registered clients:\n\r");
	for (;;) {
		msg = itc_receive(rx_filter, timeout, ITC_FROM_ALL);
		if (!msg) {
			printf("No clients\n");
			return;
		}
#ifdef SPECIAL_LED
		if (msg->cmd_cfm.ind == 0) {
			if (itc_get_name(msg->cmd_cfm.mbox, client_name, MAX_NAME_LEN))
				printf("Special LED status made by client: %s\n\r",
				       client_name);
			else
				printf("Special LED status made by client: %d\n\r",
				       msg->cmd_cfm.mbox);
			itc_free(&msg);
			return;
		}
#endif
		display_led_request(msg->cmd_cfm.ind, msg->cmd_cfm.mbox);
		itc_free(&msg);
	}
}

/**
 * Function handle_vii_cmd
 */
static void handle_vii_cmd(uint32_t dflag)
{
	union itc_msg *msg;
	uint32_t rx_filter[] = {1, RHD_VII_INFO_CFM};

	msg = itc_alloc(sizeof(conn_any_msg_t), RHD_VII_INFO_REQ);
	msg->any_msg.connection_ref = vii_conn.server_ref;
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);

	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	/* Display the led status */
	display_led_state(msg->led_info.req_masks);

	itc_free(&msg);

	/* Send signal to release RHD_VII */
	msg = itc_alloc(sizeof(conn_any_msg_t), RHD_VII_DONE_IND);
	msg->any_msg.connection_ref = vii_conn.server_ref;
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);

	if (dflag == 1)
		display_led_stateX();

}

/**
 * Function main
 */
int main(int argc, char **argv)
{
	uint32_t dflag = 0;
	uint32_t res;


	if (argc > 2) {
		print_usage();
		return 0;
	}

	if (argc == 2) {
		if(strcmp(argv[1], "d")) {
			print_usage();
			return 0;
		}
		dflag = 1;
	}

	res = vii_cmd_init();
	if (res)
		return EXIT_FAILURE;

	handle_vii_cmd(dflag);

	return EXIT_SUCCESS;
}

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
#include <stdbool.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <itc.h>
#include "rhd-vii-if.h"
#include "rhd-vii-hw.h"

#include "rhd-common.h"
#include "evti.h"
#include "conn-establish-helper.h"


/*----------------------------  CONSTANTS  ----------------------------------*/
#define TRACEPOINT_PROVIDER   com_ericsson_xcs_rhd_vii
#include "tpt_create.h"
#include "tpt.h"

/* Fault LED */
#define LED_IND_FAULT             LED_STEADY
#define LED_IND_ERROR             LED_05HZ
#define LED_IND_DEFAULT_RED       LED_OFF
/* Operation LED */
#define LED_IND_NO_POWER          LED_OFF
#define LED_IND_BOOTTEST          LED_16HZ
#define LED_IND_LOADTEST          LED_2HZ
#define LED_IND_MISSING_RESOURCE  LED_05HZ
#define LED_IND_DEFAULT_GREEN     LED_STEADY
#define LED_IND_LOADING_SW_ON     LED_DF_ON
#define LED_IND_LOADING_SW_OFF    LED_DF_OFF
/* Info LED */
#define LED_IND_BOARD_LOCKED      LED_STEADY
#define LED_IND_MAINTENANCE_MODE  LED_STEADY
#define LED_IND_SHUTDOWN          LED_05HZ
#define LED_IND_REMOVING_TRAFFIC  LED_05HZ
#define LED_IND_BOARD_BUSY        LED_16HZ
#define LED_IND_ALARMS_SUPPRESSED LED_16HZ
#define LED_IND_DEFAULT_BLUE      LED_OFF

/* Init IND value */
#define LED_IND_INIT              0

#define DAEMON_NAME      "rhd-viid"
#define MAX_MAILBOX_NUM  32

#define DAEMON_NAME_LEN  sizeof(DAEMON_NAME)
#define UIO_DEV_NAME_LEN sizeof(UIO_DEV_VII)
#define EXIT_SIGNAL 0xdeadbeef

union itc_msg {
	uint32_t              msgno;
	conn_any_msg_t        any_msg;
	EVTI_DistributeIndS   distribute_ind;
	RHD_VII_STRUCTS
};

static struct conn_establish_msg_numbers vii_conn_messages = {
	VII_CONN_ESTABLISH_REQ,
	VII_CONN_ESTABLISH_CFM,
	VII_CONN_ESTABLISH_REJ,
	VII_CONN_DISCONNECT_REQ,
	VII_CONN_DISCONNECT_CFM,
	VII_CONN_DISCONNECT_REJ,
	VII_CONN_MONITOR_FWD
};

/*
 * Local Variable Definition
 */
static itc_mbox_id_t sender_mbox = ITC_NO_ID;
static itc_mbox_id_t vii_mbox = ITC_NO_ID;
static itc_mbox_id_t last_client = ITC_NO_ID;
static struct clients *client_list = NULL;
static struct leds req_masks; /* Request mask */
static	char* board_type;
/*
 * Local Function Declaration
 */
static void print_usage(void);
static char *to_str(uint32_t mask);
static int vii_init(void);
static void vii_shutdown(void);

static struct clients *find_client(itc_mbox_id_t client_mbox,
                                   struct clients *client_list);
static struct clients *register_client(itc_mbox_id_t client_mbox,
                                       struct clients **client_list);
static void remove_client(itc_mbox_id_t client_mbox,
                          struct clients **client_list);
static void display_led_statex(struct clients *client_list,
                               uint32_t client_ref);
static void handle_info_req(struct conn_client_info *client_info);
static void handle_info2_req(union itc_msg *rec_msg, uint32_t client_ref);
static void set_fault_led_state(uint32_t combined_req, struct leds *req_masks);
static void set_operational_led_state(uint32_t combined_req,
                                      struct leds *req_masks);
static void set_info_led_state(uint32_t combined_req, struct leds *req_masks);
#ifdef SPECIAL_LED
static uint32_t set_special_led_state(uint32_t req, uint32_t *ind);
#endif


static uint32_t filter_double_flash(uint32_t new_green, uint32_t old_green);
static void distribute_leds(struct leds *new);
static void rebuild_led_state(uint32_t req, struct clients *client_list);
static void register_request(itc_mbox_id_t client_mbox, uint32_t req,
                             struct clients **client_list);
static void read_messages(conn_server_handle_t handle);

/**
 * Function print_usage
 */
static void print_usage(void)
{
	printf("Usage: rhd-vii <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

/**
 * Function to_str
 * Interpret the constant to string
 */
static char *to_str(uint32_t mask)
{
	switch (mask) {
	case VII_FAULT:
		return "Fault";
	case VII_ERROR:
		return "Error";
	case VII_NO_POWER:
		return "NoPower";
	case VII_BOOTTEST_START:
		return "BootTestStart or OBusy";
	case VII_LOADTEST_START:
		return "LoadTestStart";
	case VII_MISSING_RESOURCE_START:
		return "MissingResourceStart";
	case VII_O_LOADING_SW:
		return "OLoadingSoftware";
	case VII_BOARD_LOCKED:
		return "BoardLocked or MFullMaintenanceMode";
	case VII_SHUTDOWN_START:
		return "ShutDownStart or MRemovingTraffic";
	case VII_BOARD_BUSY_START:
		return "BusyStart";
	case VII_M_ALARMS_SUPPRESSED:
		return "MAlarmasSuppressed";
	case VII_NO_FAULT:
		return "NoFault";
	case VII_POWER:
		return "Power";
	case VII_BOOTTEST_END:
		return "BootTestEnd or OBusyEnd";
	case VII_LOADTEST_END:
		return "LoadTestEnd";
	case VII_MISSING_RESOURCE_END:
		return "MissingResourceEnd";
	case VII_O_LOADING_SW_END:
		return "OLoadingSoftwareEnd";
	case VII_BOARD_UNLOCKED:
		return "BoardUnlocked or MFullMaintenenceModeEnd";
	case VII_SHUTDOWN_END:
		return "ShutDownEnd or MRemovingTrafficEnd";
	case VII_BOARD_BUSY_END:
		return "BoardBusyEnd";
	case VII_M_ALARMS_SUPPRESSED_END:
		return "MAlarmsSuppressedEnd";
	case VII_O_NO_POWER_LOADING_SW:
		/* no constant for this in xpai */
		return "ONoPowerLoadingSoftware";
	case VII_O_BUSY_LOADING_SW:
		return "OBusyLoadingSoftware";
	case VII_LOADTEST_START_LOADING_SW:
		return "OLoadtestStartLoadingSoftware";
	case VII_MISSING_RESOURCE_START_LOADING_SW:
		return "OMissingResourceStartLoadingSoftware";
	case VII_POWER_LOADING_SW:
		return "OPowerLoadingSoftware";
	case VII_OFF:
		return "Off";
	case VII_05HZ:
		return "0.5 hz";
	case VII_STEADY:
		return "Steady";
	default:
		;
	}

	return "";
}

/**
 * Function vii_mbx_init
 * Initialize vii mailbox
 */
static int vii_mbx_init(void)
{
	/* Initialize ITC */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	/* Create our mailbox. */
	vii_mbox = itc_create_mailbox(RHD_VII_MAILBOX, 0);
	if (vii_mbox == ITC_NO_ID)
		return -EFAULT;

	return 0;
}

/**
 * Function vii_init
 * Initialize vii
 */
static int vii_init(void)
{
	if (vii_mbx_init()) {
		return -EFAULT;
	}

	if (vii_hw_init()) {
		return -EFAULT;
	}

	return 0;
}

/**
 * Function vii_shutdown
 * Shut down vii
 */
static void vii_shutdown(void)
{
	itc_delete_mailbox(vii_mbox);
	vii_hw_shutdown();
}

/**
 * Function filter_double_flash
 * Filter green LED request mask due to XPAI doesn't support combined bitmask
 */
static uint32_t filter_double_flash(uint32_t new_green, uint32_t old_green)
{
	static uint32_t filtered_green = 0;

	if (new_green != old_green) {
		if ((!(old_green & VII_O_LOADING_SW)) &&
		    (new_green & VII_O_LOADING_SW)) {
			filtered_green = VII_O_LOADING_SW;
		} else if((!(new_green & VII_O_LOADING_SW)) &&
		          (old_green & VII_O_LOADING_SW)) {
			filtered_green = VII_O_LOADING_SW_END;
		} else {
			filtered_green = (new_green & ~VII_O_LOADING_SW);
		}
	}

	return filtered_green;

}

/**
 * Function create_distribute_data
 * Allocate the distribute data and return the pointer
 */
static char *create_distribute_data(uint32_t count, char *name, uint32_t value,
                                    ...)
{
	struct {
		char *name;
		uint32_t value;
	} *data_set = malloc(count * (sizeof(char *) + sizeof(uint32_t)));
	va_list args;
	uint32_t i, size = 1;
	char *p, *str = NULL;

	if (!data_set)
		goto clean_up;

	data_set[0].name = name;
	data_set[0].value = value;

	va_start(args, value);
	for(i = 1; i < count; i++) {
		data_set[i].name = va_arg(args, char *);
		data_set[i].value = va_arg(args, uint32_t);
	}
	va_end(args);

	for(i = 0; i < count; i++) {
		size += snprintf(NULL, 0, "%s=%d ", data_set[i].name,
		                 data_set[i].value);
	}

	str = p = malloc(size);
	if (!str)
		goto clean_up;

	for(i = 0; i < count; i++) {
		p += sprintf(p, "%s=%d ", data_set[i].name, data_set[i].value);
	}
	*(--p) = '\0'; /* Remove last whitespace. */

clean_up:
	if (data_set)
		free(data_set);

	return str;
}

/**
 * Function send_distribute_ind
 * Send distribute signal
 */
static void send_distribute_ind(char *tag, char *data)
{
	union itc_msg *msg;
	itc_mbox_id_t event_server;
	uint32_t resp_msg[] = {1, ITC_LOCATE_DEFAULT_NO};

	itc_locate_async(EVTI_SERVER_NAME, NULL, ITC_MY_MBOX);
	msg = itc_receive(resp_msg, ITC_NO_TMO, ITC_FROM_ALL);
	event_server = itc_sender(msg);
	itc_free(&msg);
	TPT_TRACE(1, "Found event server mailbox");
	/* Distribute the LED state change */
	msg = itc_alloc(sizeof(EVTI_DistributeIndS) + strlen(data),
	                EVTI_DISTRIBUTE_IND);
	strcpy(msg->distribute_ind.tag, tag);
	strcpy(msg->distribute_ind.data, data);

	TPT_SEND_SIG(msg->msgno, event_server,
	             STR("EVTI_DISTRIBUTE_IND: tag=\"%s\" data=\"%s\"",
	                 msg->distribute_ind.tag,
	                 msg->distribute_ind.data));
	itc_send(&msg, event_server, ITC_MY_MBOX);
}

/**
 * Function distribute_leds
 * Distribute the new standard LED state if it has changed
 * Standard LEDs: red, green, blue
 */
static void distribute_leds(struct leds *new)
{
	static struct leds old = { VII_NO_FAULT,
		       VII_BOARD_BUSY_END,
		       VII_MISSING_RESOURCE_END,
		       0
	}; /* Old req mask to spot changes */
	char            *data;
	uint32_t        filtered_green;
	uint32_t        status = 1;

	if (new->red == old.red && new->green == old.green
	    && new->blue == old.blue && new->yellow == old.yellow) {
		status = 0;
		goto exit;
	}

	filtered_green = filter_double_flash(new->green, old.green);
	data = create_distribute_data(3, "Red", new->red,
	                              "Green", filtered_green,
	                              "Blue", new->blue);
	if (!data)
		goto exit;

	/* Distribute the LED state change */
	send_distribute_ind("XMMI_ViiState", data);
	free(data);

	/* Trace LED state change */
	if (new->red != old.red) {
		data = create_distribute_data(1, "Fault", new->red);
		if (!data)
			goto exit;
		/* Distribute the LED state change */
		send_distribute_ind("XMMI_ViiState2", data);
		free(data);
		TPT_TRACE(3, STR("Red LED: %s -> %s",
		                 to_str(old.red),
		                 to_str(new->red)));
	}
	if (new->green != old.green) {
		data = create_distribute_data(1, "Operational", filtered_green);
		if (!data)
			goto exit;
		/* Distribute the LED state change */
		send_distribute_ind("XMMI_ViiState2", data);
		free(data);
		TPT_TRACE(3, STR("Green LED: %s -> %s",
		                 to_str(old.green),
		                 to_str(new->green)));
	}
	if (strcmp(board_type ,"BP") == 0) {
		if (new->blue != old.blue) {
			data = create_distribute_data(1, "Information", new->blue);
			if (!data)
				goto exit;
			/* Distribute the LED state change */
			send_distribute_ind("XMMI_ViiState2", data);
			free(data);
			TPT_TRACE(3, STR("Blue LED: %s -> %s",
		                 to_str(old.blue),
		                 to_str(new->blue)));
		}
	}
#ifdef SPECIAL_LED
	if(new->yellow != old.yellow) {
		data = create_distribute_data(1, "Special", new->yellow);
		if (!data)
			goto exit;
		/* Distribute the LED state change */
		send_distribute_ind("XMMI_ViiState2", data);
		free(data);
		TPT_TRACE(3, STR("Special: %s -> %s",
		                 to_str(old.yellow),
		                 to_str(new->yellow)));
	}
#endif
	old = *new;
	status = 0;
exit:
	if (status)
		TPT_ERROR("create distribute failed.");
	return;
}

static char *led_mode_to_str(enum led_mode led)
{
	switch (led) {
	case LED_OFF:
		return "OFF";
	case LED_STEADY:
		return "ON";
	case LED_05HZ:
		return "0.5HZ";
	case LED_2HZ:
		return "2HZ";
	case LED_16HZ:
		return "16HZ";
	case LED_DF_ON:
		return "DF_ON";
	case LED_DF_OFF:
		return "DF_OFF";
	default:
		return "INVALID";
	}
}

/**
 * Function set_special_led_state
 */
#ifdef SPECIAL_LED
static uint32_t set_special_led_state(uint32_t req, uint32_t *ind)
{
	enum led_mode yellow_led;
	uint32_t indication;

	indication = req & OPTIONAL_LED_INDICATION_MASK;

	switch (indication) {
	case VII_OFF:
		yellow_led = LED_OFF;
		break;
	case VII_05HZ:
		yellow_led = LED_05HZ;
		break;
	case VII_STEADY:
		yellow_led = LED_STEADY;
		break;
	default:
		TPT_ERROR(STR("0x%x is not a valid "
		              "special LED indication.", req));
		return 1;
	}

	*ind = indication;
	set_led(LED_SPECIAL_IND, yellow_led);
	TPT_TRACE(3, STR("Set YELLOW %s", led_mode_to_str(yellow_led)));

	return 0;
}
#endif

/**
 * Function set_fault_led_state
 */
static void  set_fault_led_state(uint32_t combined_req, struct leds *req_masks)
{
	enum led_mode red_led;

	if ((combined_req & VII_FAULT) != 0) {
		red_led = LED_IND_FAULT;
		req_masks->red = VII_FAULT;
	} else if ((combined_req & VII_ERROR) != 0) {
		red_led = LED_IND_ERROR;
		req_masks->red = VII_ERROR;
	} else {
		red_led = LED_IND_DEFAULT_RED;
		req_masks->red = VII_NO_FAULT;
	}

	set_led(LED_FAULT_IND, red_led);
	TPT_TRACE(3, STR("Set RED %s", led_mode_to_str(red_led)));
}

/**
 * Function set_operational_led_state
 */
static void set_operational_led_state(uint32_t combined_req,
                                      struct leds *req_masks)
{
	enum led_mode green_led;

	if ((combined_req & VII_NO_POWER) != 0) {
		green_led = LED_IND_NO_POWER;
		req_masks->green = VII_NO_POWER;
	} else if ((combined_req & VII_BOOTTEST_START) != 0) {
		green_led = LED_IND_BOOTTEST;
		req_masks->green = VII_BOOTTEST_START;
	} else if ((combined_req & VII_O_BUSY) != 0) {
		green_led = LED_IND_BOARD_BUSY;
		req_masks->green = VII_O_BUSY;
	} else if ((combined_req & VII_LOADTEST_START) != 0) {
		green_led = LED_IND_LOADTEST;
		req_masks->green = VII_LOADTEST_START;
	} else if ((combined_req & VII_MISSING_RESOURCE_START) != 0) {
		green_led = LED_IND_MISSING_RESOURCE;
		req_masks->green = VII_MISSING_RESOURCE_START;
	} else {
		green_led = LED_IND_DEFAULT_GREEN;
		req_masks->green = VII_MISSING_RESOURCE_END;
	}

	/* if double flash mode */
	if ((combined_req & VII_O_LOADING_SW) != 0) {
		if (green_led == LED_IND_DEFAULT_GREEN) {
			green_led = LED_IND_LOADING_SW_ON;
		} else {
			green_led = LED_IND_LOADING_SW_OFF;
		}
		req_masks->green   |= VII_O_LOADING_SW;
	}

	set_led(LED_OPER_IND, green_led);
	TPT_TRACE(3, STR("Set GREEN %s", led_mode_to_str(green_led)));
}

/**
 * Function set_info_led_state
 */
static void  set_info_led_state(uint32_t combined_req, struct leds *req_masks)
{
	enum led_mode blue_led;

	if ((combined_req & VII_BOARD_LOCKED) != 0) {
		blue_led = LED_IND_BOARD_LOCKED;
		req_masks->blue = VII_BOARD_LOCKED;
	}
	if ((combined_req & VII_M_FULL_MAINTENANCE_MODE) != 0) {
		blue_led = LED_IND_MAINTENANCE_MODE;
		req_masks->blue = VII_M_FULL_MAINTENANCE_MODE;
	} else if ((combined_req & VII_SHUTDOWN_START) != 0) {
		blue_led = LED_IND_SHUTDOWN;
		req_masks->blue = VII_SHUTDOWN_START;
	} else if ((combined_req & VII_M_REMOVING_TRAFFIC) != 0) {
		blue_led = LED_IND_REMOVING_TRAFFIC;
		req_masks->blue = VII_M_REMOVING_TRAFFIC;
	} else if ((combined_req & VII_BOARD_BUSY_START) != 0) {
		blue_led = LED_IND_BOARD_BUSY;
		req_masks->blue = VII_BOARD_BUSY_START;
	} else if ((combined_req & VII_M_ALARMS_SUPPRESSED) != 0) {
		blue_led = LED_IND_ALARMS_SUPPRESSED;
		req_masks->blue = VII_M_ALARMS_SUPPRESSED;
	} else {
		blue_led = LED_IND_DEFAULT_BLUE;
		req_masks->blue = VII_BOARD_UNLOCKED;
	}

	set_led(LED_MAINT_IND, blue_led);
	TPT_TRACE(3, STR("Set BLUE %s", led_mode_to_str(blue_led)));
}

/**
 * Function rebuild_led_state
 * Rebuild the LEDs states according to priority
 */
static void rebuild_led_state(uint32_t req, struct clients *client_list)
{
	uint32_t combined_req = 0;


	if (req & OPTIONAL_LED_MASK) { /* Optinal LED request */
#ifndef SPECIAL_LED
		return;
#else
		if (set_special_led_state(req, &combined_req))
			return; /* If failed to set LED */
		last_client = sender_mbox;
		req_masks.yellow = combined_req;
#endif
	} else {
		/* Generate the combined request */
		while (client_list != NULL) {
			combined_req |= client_list->ind_req;
			client_list = client_list->next;
		}
		/* set LEDs */
		
		set_fault_led_state(combined_req, &req_masks);
		set_operational_led_state(combined_req, &req_masks);
		if (strcmp(board_type ,"BP") == 0) {
			set_info_led_state(combined_req, &req_masks);
		}
	}
	/* Distribute and trace the state-change, if any */
	distribute_leds(&req_masks);

	return;
}

/**
 * Function find_client
 * Search for a client in the client list
 */
static struct clients *find_client(itc_mbox_id_t client_mbox,
                                   struct clients *client_list)
{
	while (client_list != NULL) {
		if (client_list->mbox == client_mbox)
			return client_list;

		client_list = client_list->next;
	}
	return NULL;
}

/**
 * Function register_client
 * Register client to the client list
 */
static struct clients *register_client(itc_mbox_id_t client_mbox,
                                       struct clients **client_list)
{
	struct clients *new_cr;

	/* Check if client is already registered */
	if (find_client(client_mbox, *client_list) != NULL) {
		TPT_INFO(STR("Warning: 0x%x already registered "
		             "as a client.", client_mbox));
		return NULL;
	}

	/* Create new element */
	new_cr = (struct clients *)malloc(sizeof(struct clients));
	if (!new_cr)
		return NULL;
	new_cr->mbox = client_mbox;
	new_cr->ind_req = 0;
	new_cr->prev = NULL;
	new_cr->next = *client_list;

	/* Check if this is first element in list */
	if (*client_list != NULL)
		(*client_list)->prev = new_cr;

	/* Insert new client element */
	*client_list = new_cr;

	return new_cr;
}

/**
 * Function register_request
 * Change the bits in the client's request
 */
static void register_request(itc_mbox_id_t client_mbox, uint32_t req,
                             struct clients **client_list)
{
	struct clients *reg_ptr;

	/* Find the right register entry*/
	reg_ptr = find_client(client_mbox, *client_list);

	if (NULL == reg_ptr) {
		TPT_TRACE(3, STR("Adding new client to client list "
		                 "(mailbox: 0x%x)", client_mbox));
		reg_ptr = register_client(client_mbox, client_list);
	}

	if (NULL == reg_ptr) {
		TPT_INFO(STR("Warning: Adding new client failed "
		             "(mailbox: 0x%x)", client_mbox));
		return;
	}

	/* Fill in the clients private led array if it is not special LED */
	if (!(req & OPTIONAL_LED_MASK)) {
		uint32_t or_mask  = req & 0xffff;
		uint32_t and_mask = ~req >> 16;
		reg_ptr->ind_req = (reg_ptr->ind_req & and_mask) | or_mask;
	}
	return;
}

/**
 * Function remove_client
 * Remove client from client list
 */
static void remove_client(itc_mbox_id_t client_mbox,
                          struct clients **client_list)
{
	struct clients *client_ptr;

	/* Find the right register entry */
	client_ptr = find_client(client_mbox, *client_list);

	if (NULL == client_ptr)
		return;

	/* Remove the element from the list.*/
	if (NULL == client_ptr->prev)
		*client_list = client_ptr->next;  /* First element */
	else
		(client_ptr->prev)->next = client_ptr->next;


	if (NULL != client_ptr->next)
		(client_ptr->next)->prev = client_ptr->prev;

	free(client_ptr);
	return;
}

/**
 * Function wait_for_vii_done
 * Wait for a release signal
 */
static void wait_for_vii_done(void)
{
	union itc_msg *msg;
	static uint32_t rx_filter[] = {1, RHD_VII_DONE_IND};

	/* Wait for release signal */
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	TPT_REC_SIG(msg->msgno, "Receive RHD_VII_DONE_IND");
	itc_free(&msg);
	return;
}

/**
 * Function handle_info_req
 * Retrieve LED status and send it back
 */
static void handle_info_req(struct conn_client_info *client_info)
{
	union itc_msg *msg;

	/* Send answer to client */
	msg = itc_alloc(sizeof(struct vii_info), RHD_VII_INFO_CFM);
	msg->led_info.req_masks = req_masks;
	msg->any_msg.connection_ref = client_info->client_ref;
	msg->any_msg.procedure_ref = client_info->procedure_ref;

	TPT_SEND_SIG(msg->msgno, sender_mbox,
	             STR("Send RHD_VII_INFO_CFM: req_masks red=%x "
	                 "blue=%x, green=%x",
	                 msg->led_info.req_masks.red,
	                 msg->led_info.req_masks.blue,
	                 msg->led_info.req_masks.green));
	itc_send(&msg, sender_mbox, ITC_MY_MBOX);

	/* Wait for release signal */
	wait_for_vii_done();
}

/**
 * Function handle_info2_req
 * Retrieve certain LED status and send it back
 */
static void handle_info2_req(union itc_msg *rec_msg, uint32_t client_ref)
{
	union itc_msg *msg = NULL;
	uint32_t ind = 0;
	uint32_t req;

	if (itc_size(rec_msg) != sizeof(struct vii_ind_req)) {
		TPT_ERROR("corrupt size of VII2 INFO REQ message.");
		msg = itc_alloc(sizeof(conn_any_msg_t),
		                RHD_VII2_INFO_REJ);
		msg->any_msg.connection_ref = client_ref;
		itc_send(&msg, sender_mbox, ITC_MY_MBOX);
		return;
	}
	req = rec_msg->ind_req.req;

	switch (req) {
	case VII_FAULT_LED:
		ind = req_masks.red;
		break;
	case VII_OPERATIONAL_LED:
		ind = req_masks.green;
		break;
	case VII_INFORMATION_LED:
		if(strcmp(board_type ,"BP") == 0) {
			ind = req_masks.blue;
		}
		break;
#ifdef SPECIAL_LED
	case VII_SPECIAL_LED:
		ind = req_masks.yellow;
		break;
#endif
	default:
		TPT_ERROR(STR("0x%x is not a valid LED ID.", req));
		msg = itc_alloc(sizeof(conn_any_msg_t), RHD_VII2_INFO_REJ);
		break;
	}

	/* Send answer to client */
	if (msg == NULL) {
		msg = itc_alloc(sizeof(struct vii2_info_cfm),
		                RHD_VII2_INFO_CFM);
		msg->info_cfm.ind = ind;
		TPT_SEND_SIG(msg->msgno, sender_mbox,
		             STR("Send RHD_VII2_INFO_CFM: ind=0x%x.",
		                 msg->info_cfm.ind));
	}

	msg->any_msg.connection_ref = client_ref;
	msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;
	itc_send(&msg, sender_mbox, ITC_MY_MBOX);

	/* Wait for release signal */
	wait_for_vii_done();
}

/**
 * Function display_led_statex
 * Used by vii cmd, print the LED ind to shell
 */
static void display_led_statex(struct clients *client_list,
                               uint32_t client_ref)
{
	union itc_msg *msg;

	while (client_list != NULL) {
		/* Skip a client if it only has optional LED request */
		if (client_list->ind_req == 0) {
			client_list = client_list->next;
			continue;
		}
		msg = itc_alloc(sizeof(struct vii_cmd_cfm),
		                RHD_VII_LED_CMD_CFM);
		msg->any_msg.connection_ref = client_ref;
		msg->cmd_cfm.ind = client_list->ind_req;
		msg->cmd_cfm.mbox = client_list->mbox;
		itc_send(&msg, sender_mbox, ITC_MY_MBOX);

		client_list = client_list->next;
	}
	if (last_client != ITC_NO_ID) {
		msg = itc_alloc(sizeof(struct vii_cmd_cfm),
		                RHD_VII_LED_CMD_CFM);
		msg->any_msg.connection_ref = client_ref;
		msg->cmd_cfm.ind = 0;
		msg->cmd_cfm.mbox = last_client;
		itc_send(&msg, sender_mbox, ITC_MY_MBOX);
	}
}

/**
 * Function read_messages
 * Start reading received messages through ITC and handle them.
 */
static void read_messages(conn_server_handle_t handle)
{
	union itc_msg *msg;
	struct conn_client_info client_info;

	/* wating for event server to start */
	uint32_t resp_msg[] = {1, ITC_LOCATE_DEFAULT_NO};

	itc_locate_async(EVTI_SERVER_NAME, NULL, ITC_MY_MBOX);
	msg = itc_receive(resp_msg, ITC_NO_TMO, ITC_FROM_ALL);
	itc_free(&msg);

	if ((board_type = getenv("SYS_BOARD_TYPE")) == NULL) {
			TPT_ERROR("SYS_BOARD_TYPE not found, aborting");
			return;
        }

	/* Initialize the LEDs */
	rebuild_led_state(LED_IND_INIT, client_list);
#ifdef SPECIAL_LED
	rebuild_led_state(LED_IND_INIT | OPTIONAL_LED_MASK, client_list);
#endif

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		if (msg->msgno == EXIT_SIGNAL) {
			vii_shutdown();
			TPT_INFO("rhd-vii exiting as ordered");
			itc_free(&msg);
			return;
		}
		/*Handle CONN_ESTABLISH... messages (and messages from
		 unknown clients.*/
		if(!conn_check_client(handle, &msg, &client_info))
			continue;

		sender_mbox = client_info.sender;

		switch (msg->msgno) {
		case RHD_VII_LED_CTRL_IND: {
			TPT_REC_SIG(msg->msgno,
			            STR("Receive RHD_VII_LED_CTRL_IND,"
			                " sender: %d.",
			                sender_mbox));
			if (itc_size(msg) == sizeof(struct vii_ind_req)) {
				register_request(sender_mbox,
				                 msg->ind_req.req,
				                 &client_list);
				rebuild_led_state(msg->ind_req.req,
				                  client_list);
			} else {
				TPT_ERROR("Corrupt size of LED CTRL IND.");
			}
			break;
		}
		case RHD_VII_INFO_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("Receive RHD_VII_INFO_REQ, sender: %d.",
			                sender_mbox));
			handle_info_req(&client_info);
			break;
		}
		case RHD_VII2_INFO_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("Receive RHD_VII2_INFO_REQ, sender: %d.",
			                sender_mbox));
			handle_info2_req(msg, client_info.client_ref);
			break;
		}
		case RHD_VII_LED_CMD_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("Receive RHD_VII_LED_CMD_REQ, sender: %d.",
			                sender_mbox));
			display_led_statex(client_list, client_info.client_ref);
			break;
		}
		default:
			TPT_ERROR(STR("Receive Unexpected message: %d.",
			              msg->msgno));
			break;
		}
		itc_free(&msg);

	}
}


/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{
	remove_client(client_info->connected_mailbox, &client_list);
	rebuild_led_state(LED_IND_INIT, client_list);
}

/**
 * Function conn_server_init
 */
static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	struct conn_event_callbacks cb = { NULL, client_disconnect,
		       client_disconnect, NULL
	};
	uint32_t supported_versions[] = {VII_SERVER_VERSIONS};

	int conn_result = conn_establish_server_init(&handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &vii_conn_messages, 0, &cb);
	if (conn_result != CONN_INIT_OK) {
		TPT_ERROR("Initalization of conn_establish mechanism failed.");
		return NULL;
	}

	return handle;
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_TRACE(3, STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, vii_mbox, ITC_MY_MBOX);
}

/**
 * Main function
 * start the rhd_vii daemon
 */
int main(int argc, char **argv)
{
	int daemonize = 0;
	int32_t ret = 0;
	void *handle;

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		} else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		} else {
			print_usage();
			exit(-EINVAL);
		}
	}


	if (rhd_try_lock(DAEMON_NAME)) {
		printf("failed to obtain lock: %s\n", DAEMON_NAME);
		ret = -EFAULT;
		return ret;
	}

	if (!daemonize || !daemon(0, 0)) {
		TPT_INFO(STR("Starting %s %s",
		             daemonize ? "daemon" : "foreground process",
		             DAEMON_NAME));

		handle = conn_server_init();

		if (!vii_init() && (handle != NULL)) {
			/* Start processing ITC messages.
			 * No return.
			 */
			if (signal(SIGTERM, exit_handler) == SIG_ERR) {
				TPT_ERROR("Failed to install "
				          "signal exit handler");
				exit(1);
			}
			read_messages(handle);

		} else {
			TPT_ERROR("Failed to intialize vii");
			ret = -EFAULT;
		}
	} else {
		TPT_ERROR(STR("Failed to start daemon %s",
		              DAEMON_NAME));
		ret = -EFAULT;
	}

	return ret;

}

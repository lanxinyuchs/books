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
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <itc.h>
#include <uio_helper.h>
#include <pthread.h>
#include "rhd-mmi-if.h"
#include "rhd-vii-if.h"
#include "rhd-common.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER       com_ericsson_xcs_rhd_mmi
#include "tpt_create.h"
#include "tpt.h"

/* button states */
#define BUTTON_STATE_IDLE         1
#define BUTTON_STATE_ST_PRESS     2
#define BUTTON_STATE_MT_PRESS     3
#define BUTTON_STATE_SECOND_PRESS 4

/* Validate request return values */
#define NEW_STATE_OK_NOT_CHANGED 2
#define NEW_STATE_OK             1
#define NEW_STATE_NOK            0

#define UIO_DEV_MMI     "mmi"
#define DAEMON_NAME     "rhd-mmid"
#define MAX_MAILBOX_NUM 32
#define MMI_INT_MBOX    "MMI-INT"
#define MMI_CLIENT_REF  4261 /*dummy value*/
#define MMI_STATE_FILE  "/var/log/mmi/state"
#define MMI_STATE_PATH  "/var/log/mmi"
#define MMI_RESET       "MMI"

#define DAEMON_NAME_LEN  sizeof(DAEMON_NAME)
#define UIO_DEV_NAME_LEN sizeof(UIO_DEV_MMI)
/* MMI register offset */
#define XENON_MMI_STATUS        (0x260)
#define XENON_MMI_STATUS_TRAP   (0x264)
#define XENON_MMI_STATUS_MASK   (0x268)
/* MMI status mask */
#define STATUS_PRESSED         1 /* button pressed */
#define STATUS_MEDIUM          2 /* button pressed more than 2s */
#define STATUS_RELEASED        4 /* button released */

#define EXIT_SIGNAL 0xdeadbeef

union itc_msg {
	uint32_t                msgno;
	conn_any_msg_t          any_msg;
	struct vii_ind_req      ind_req;
	RHD_MMI_STRUCTS
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

static struct conn_establish_msg_numbers mmi_conn_messages = {
	MMI_CONN_ESTABLISH_REQ,
	MMI_CONN_ESTABLISH_CFM,
	MMI_CONN_ESTABLISH_REJ,
	MMI_CONN_DISCONNECT_REQ,
	MMI_CONN_DISCONNECT_CFM,
	MMI_CONN_DISCONNECT_REJ,
	MMI_CONN_MONITOR_FWD
};

struct mmi_block {
	uint32_t        state;          /* current state */
	uint32_t        button_state;   /* button state machine */
	itc_mbox_id_t   subscriber;     /* registered subscriber */
	int             button_reset;   /* Non-zero if MMI button reset */
};

struct mmi_registers {
	volatile uint32_t *status_trap;  /* MMI_STATUS_TRAP */
	volatile uint32_t *status_mask;  /* MMI_STATUS_MASK */
};

typedef enum {
	MMI_PRESS_INT,     /* button pressed */
	MMI_MT_INT,        /* button pressed more than 2s */
	MMI_RELEASE_INT    /* button released */
} mmi_event_t;

/*
 * Local Variable Definition
 */
static itc_mbox_id_t sender_mbox = ITC_NO_ID;
static itc_mbox_id_t mmi_mbox = ITC_NO_ID;
static itc_mbox_id_t int_mbox = ITC_NO_ID;
static struct mmi_registers mmi_reg;
static void *mmap_base = NULL;
static UIO_HANDLE_ uio_handle = 0;
static struct mmi_block mmi_data;
static struct server_info vii_conn;

/*
 * Local Function Declaration
 */
static void mmi_int_notifier(__attribute__((unused)) void *data);
static void mmi_int_notifier_post(__attribute__((unused)) void *data);

/*----------------------------  Function Definitions  -----------------------*/
/**
 * Fuction state_to_str
 * Converts a state to a readable string
 */
static const char *state_to_str(uint32_t state)
{
	static char buf[30];
	switch (state) {
	case MMI_MAINTENANCE_STATE_DEACTIVATED:
		return "MMI_MAINTENANCE_STATE_DEACTIVATED";
		break;
	case MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
		return "MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS";
		break;
	case MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
		return "MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC";
		break;
	case MMI_MAINTENANCE_STATE_MAINTENANCE_MODE:
		return "MMI_MAINTENANCE_STATE_MAINTENANCE_MODE";
		break;
	default:
		(void) sprintf(buf, "<unknown state 0x%x>", (int)state);
		return buf;
		break;
	}
}

/**
 * Fuction button_to_str
 * Converts a button state to a readable string
 */
static const char *button_to_str(uint32_t state)
{
	static char buf[30];
	switch (state) {
	case BUTTON_STATE_IDLE:
		return "BUTTON_STATE_IDLE";
		break;
	case BUTTON_STATE_ST_PRESS:
		return "BUTTON_STATE_ST_PRESS";
		break;
	case BUTTON_STATE_MT_PRESS:
		return "BUTTON_STATE_MT_PRESS";
		break;
	case BUTTON_STATE_SECOND_PRESS:
		return "BUTTON_STATE_SECOND_PRESS";
		break;
	default:
		(void) sprintf(buf, "<unknown state 0x%x>", (int)state);
		return buf;
		break;
	}
}

/**
 * Fuction validate_state
 */
static uint32_t validate_state(uint32_t state)
{
	uint32_t res = 0;

	switch (state) {
	case MMI_MAINTENANCE_STATE_DEACTIVATED:
	case MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
	case MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
	case MMI_MAINTENANCE_STATE_MAINTENANCE_MODE:
		res = 1;
		break;
	}
	return res;
}

/**
 * Fuction set_vii
 * Send LED control signal to rhd-vii
 */
static uint32_t set_vii(uint32_t indication)
{
	union itc_msg *msg = NULL;
	uint32_t res = 1;
	uint32_t tmp = indication;

	/* check if we have connection to RHD VII */
	if (vii_conn.server_mbox == ITC_NO_ID ||
	    vii_conn.server_ref == 0) {
		TPT_ERROR("No connection set between MMI and VII.");
		return res;
	}

	/* Verify indication */
	/* This is all bits that are not used by red,
	   green and yellow/blue LEDs */
	if ((tmp & 0xfc00fc00) != 0)
		return res;
	/* Verify that indication contains one and only one bit */
	while (tmp > 1) {
		if ((tmp & 1) != 0)
			return res;
		tmp >>= 1;
	}
	if (tmp == 0)
		return res;

	/* Send the request */
	msg = itc_alloc(sizeof(struct vii_ind_req), RHD_VII_LED_CTRL_IND);
	msg->ind_req.req = indication;
	msg->any_msg.connection_ref = vii_conn.server_ref;

	TPT_SEND_SIG(msg->msgno, vii_conn.server_mbox,
	             STR("Send RHD_VII_LED_CTRL_IND: req=%u .",
	                 msg->ind_req.req));
	itc_send(&msg, vii_conn.server_mbox, ITC_MY_MBOX);
	res = 0;

	return res;

}
/**
 * Fuction change_vii
 * Change LED status according to MMI state
 */
static void change_vii(uint32_t current_state, uint32_t new_state)
{
	TPT_TRACE(3, STR("change maintenance LED %s -> %s.",
	                 state_to_str(current_state),
	                 state_to_str(new_state)));

	/* reset current state */
	switch (current_state) {
	case MMI_MAINTENANCE_STATE_DEACTIVATED:
		/* nothing to reset .i.e. out */
		break;
	case MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
		if (set_vii(VII_M_ALARMS_SUPPRESSED_END)) {
			TPT_INFO("Warning: failed to change LED "
			         "to VII_M_ALARMS_SUPPRESSED_END.");
		}
		break;
	case MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
		if (set_vii(VII_M_REMOVING_TRAFFIC_END)) {
			TPT_INFO("Warning: failed to change LED"
			         "to VII_M_REMOVING_TRAFFIC_END.");
		}
		break;
	case MMI_MAINTENANCE_STATE_MAINTENANCE_MODE:
		if (set_vii(VII_M_FULL_MAINTENANCE_MODE_END)) {
			TPT_INFO("Warning: failed to change LED "
			         "to VII_M_FULL_MAINTENANCE_MODE_END.");
		}
		break;
	default:
		TPT_INFO(STR("Warning: unknown state 0x%x "
		             "when disabling maintenance LED.",
		             current_state));
		break;
	}

	switch (new_state) {
	case MMI_MAINTENANCE_STATE_DEACTIVATED:
		/* nothing to set i.e. out */
		break;
	case MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
		if (set_vii(VII_M_ALARMS_SUPPRESSED)) {
			TPT_INFO("Warning: failed to change LED "
			         "to VII_M_ALARMS_SUPPRESSED.");
		}
		break;
	case MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
		if (set_vii(VII_M_REMOVING_TRAFFIC)) {
			TPT_INFO("Warning: failed to change LED"
			         " to VII_M_REMOVING_TRAFFIC.");
		}
		break;
	case MMI_MAINTENANCE_STATE_MAINTENANCE_MODE:
		if (set_vii(VII_M_FULL_MAINTENANCE_MODE)) {
			TPT_INFO("Warning: failed to change LED "
			         "to VII_M_FULL_MAINTENANCE_MODE.");
		}
		break;
	default:
		TPT_INFO(STR("Warning: unknown state 0x%x "
		             "when changing maintenance LED.",
		             new_state));
		break;
	}
}

/**
 * Function inform_subscriber
 */
static void inform_subscriber(struct mmi_block *mmi)
{
	union itc_msg *msg = NULL;
	if (mmi->subscriber != 0) {
		msg = itc_alloc(sizeof(struct mmi_state_ind),
		                RHD_MMI_STATE_IND);
		msg->state_ind.state = mmi->state;
		TPT_TRACE(3, STR("Send MMI_STATE_IND: state %s.",
		                 state_to_str(mmi->state)));
		itc_send(&msg, mmi->subscriber, ITC_MY_MBOX);
	} else {
		TPT_INFO("Warning: try to send MMI_MAINTENANCE_STATE_IND "
		         "with no registered receiver.");
	}
}

/**
 * Function save_to_fs
 */
static void save_to_fs(uint32_t state)
{
	FILE *fp;
	struct stat sb;


	if (access(MMI_STATE_FILE, W_OK | R_OK) == 0)
		goto write_to_file;

	if (stat(MMI_STATE_PATH, &sb) == 0) {
		if (S_ISDIR(sb.st_mode))
			goto write_to_file;

		if (remove(MMI_STATE_PATH)) {
			TPT_ERROR(STR("Cannot delete file %s",
			          MMI_STATE_PATH));
			return;
		}
	}
	if (mkdir(MMI_STATE_PATH,
	          S_IRUSR | S_IWUSR |
	          S_IRGRP | S_IROTH) == -1) {
		TPT_ERROR(STR("Failed to create %s",
		              MMI_STATE_PATH));
		return;
	}

write_to_file:
	fp = fopen(MMI_STATE_FILE, "w");
	if (!fp) {
		TPT_ERROR(STR("Failed to open %s", MMI_STATE_FILE));
		return;
	}
	TPT_TRACE(1, STR("save state %s", state_to_str(state)));
	fprintf(fp, "%u", state);
	fclose(fp);

	return;
}

/**
 * Function validate_request
 * Validate state change request according to state diagram
 */
static uint32_t validate_request(struct mmi_block *data, uint32_t new_state)
{
	uint32_t result = NEW_STATE_NOK;

	if (data->state == MMI_MAINTENANCE_STATE_DEACTIVATED) {
		switch (new_state) {
		case MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
		case MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
			TPT_INFO(STR("maintenance state change %s -> %s.",
			             state_to_str(data->state),
			             state_to_str(new_state)));
			result = NEW_STATE_OK;
			break;
		case MMI_MAINTENANCE_STATE_DEACTIVATED:
			TPT_INFO(STR("new maintenance state equals"
				     " current state (%s).",
				     state_to_str(new_state)));
			result = NEW_STATE_OK_NOT_CHANGED;
			break;
		default:
			TPT_INFO(STR("Warning: invalid maintenance "
			             "state change %s -> %s.",
			             state_to_str(data->state),
			             state_to_str(new_state)));
			result = NEW_STATE_NOK;
			break;
		}
	} else if (data->state == MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS) {
		switch (new_state) {
		case MMI_MAINTENANCE_STATE_DEACTIVATED:
			TPT_INFO(STR("maintenance state change %s -> %s.",
				     state_to_str(data->state),
				     state_to_str(new_state)));
			result = NEW_STATE_OK;
			break;
		case MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
			TPT_INFO(STR("new maintenance state equals "
				     "current state (%s).",
				     state_to_str(new_state)));
			result = NEW_STATE_OK_NOT_CHANGED;
			break;
		default:
			TPT_INFO(STR("Warning: invalid maintenance state "
			             "change %s -> %s.",
			             state_to_str(data->state),
			             state_to_str(new_state)));
			result = NEW_STATE_NOK;
			break;
		}
	} else if (data->state == MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC) {
		switch (new_state) {
		case MMI_MAINTENANCE_STATE_MAINTENANCE_MODE:
			TPT_INFO(STR("maintenance state change %s -> %s.",
				     state_to_str(data->state),
				     state_to_str(new_state)));
			result = NEW_STATE_OK;
			break;
		case MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
			TPT_INFO(STR("new maintenance state equals "
				     "current state (%s).",
				     state_to_str(new_state)));
			result = NEW_STATE_OK_NOT_CHANGED;
			break;
		default:
			TPT_INFO(STR("Warning: invalid maintenance state "
			             "change %s -> %s.",
			             state_to_str(data->state),
			             state_to_str(new_state)));
			result = NEW_STATE_NOK;
			break;
		}
	} else if (data->state == MMI_MAINTENANCE_STATE_MAINTENANCE_MODE) {
		switch (new_state) {
		case MMI_MAINTENANCE_STATE_DEACTIVATED:
			TPT_INFO(STR("maintenance state change %s -> %s.",
				     state_to_str(data->state),
				     state_to_str(new_state)));
			result = NEW_STATE_OK;
			break;
		case MMI_MAINTENANCE_STATE_MAINTENANCE_MODE:
			TPT_INFO(STR("new maintenance state equals "
				     "current state (%s).",
				     state_to_str(new_state)));
			result = NEW_STATE_OK_NOT_CHANGED;
			break;
		default:
			TPT_INFO(STR("Warning: invalid maintenance state "
			             "change %s -> %s.",
			             state_to_str(data->state),
			             state_to_str(new_state)));
			result = NEW_STATE_NOK;
			break;
		}
	}

	return result;
}

/**
 * Function handle_set_state_req
 * Updates state in mmi_block
 */
static void handle_set_state_req(struct mmi_block *data,
                                 union itc_msg *rec_msg, uint32_t client_ref)
{
	union itc_msg *msg = NULL;
	uint32_t result  = 0;

	if (itc_size(rec_msg) != sizeof(struct mmi_set_state_req)) {
		TPT_ERROR("corrupt size of MMI set state message");
		msg = itc_alloc(sizeof(struct mmi_set_state_rej),
		                RHD_MMI_SET_STATE_REJ);
		msg->set_state_rej.error = RHD_MMI_ERROR;
		msg->any_msg.connection_ref = client_ref;
		itc_send(&msg, sender_mbox, ITC_MY_MBOX);
		return;
	}

	if (data->button_state != BUTTON_STATE_IDLE) {
		TPT_INFO(STR("state change request %s -> %s rejected"
		             " since maintenance button is active.",
		             state_to_str(data->state),
		             state_to_str(rec_msg->set_state_req.state)));

		msg = itc_alloc(sizeof(struct mmi_set_state_rej),
		                RHD_MMI_SET_STATE_REJ);
		msg->set_state_rej.error = RHD_MMI_BUTTON_PRESSED;
		msg->any_msg.connection_ref = client_ref;
		msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;

		itc_send(&msg, sender_mbox, ITC_MY_MBOX);
		return;
	}

	result = validate_request(data, rec_msg->set_state_req.state);

	if (result == NEW_STATE_NOK) {
		/* send REJ */
		msg = itc_alloc(sizeof(struct mmi_set_state_rej),
		                RHD_MMI_SET_STATE_REJ);
		msg->set_state_rej.error = RHD_MMI_ERROR;
		msg->any_msg.connection_ref = client_ref;
		msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;

		itc_send(&msg, sender_mbox, ITC_MY_MBOX);
	} else { /* NEW_STATE_OK or NEW_STATE_OK_NOT_CHANGED */
		/* in XDAI_SetMaintenanceState only state deactivated
		 * is remembered in restarts */
		if (rec_msg->set_state_req.state ==
		    MMI_MAINTENANCE_STATE_DEACTIVATED)
			save_to_fs(MMI_MAINTENANCE_STATE_DEACTIVATED);

		if(result == NEW_STATE_OK) {
			/* if state was changed, update LED and state */
			change_vii(data->state, rec_msg->set_state_req.state);
			data->state = rec_msg->set_state_req.state;
		}

		/* send CFM */
		msg = itc_alloc(sizeof(struct mmi_set_state_cfm),
		                RHD_MMI_SET_STATE_CFM);
		msg->any_msg.connection_ref = client_ref;
		msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;

		itc_send(&msg, sender_mbox, ITC_MY_MBOX);
		if(result == NEW_STATE_OK) {
			/* Only inform subscriber if the state was changed */
			inform_subscriber(data);
		}
	}
	return;
}

/**
 * Function handle_subscribe_req
 * adds/replaces one subscriber to MMI and send MMI state back
 */
static void handle_subscribe_req(struct mmi_block *data,
                                 union itc_msg *rec_msg, uint32_t client_ref)
{
	union itc_msg *msg = NULL;
	char client_name[20];
	itc_mbox_id_t client_mbox = rec_msg->subscribe_req.mbox;

	if (itc_size(rec_msg) != sizeof(struct mmi_subscribe_req)) {
		TPT_ERROR("corrupt size of MMI subscribe message");
		msg = itc_alloc(sizeof(struct mmi_subscribe_rej),
		                RHD_MMI_SUBSCRIBE_REJ);
		msg->subscribe_rej.error = RHD_MMI_ERROR;
		msg->any_msg.connection_ref = client_ref;
		itc_send(&msg, sender_mbox, ITC_MY_MBOX);
		return;
	}
	/* replace subscriber or register first subscriber after button reset */
	if (data->subscriber != client_mbox) {
		if (itc_get_name(client_mbox, client_name, 20)) {
			TPT_TRACE(1, STR("register subscriber %s.",
			                 client_name));
		} else {
			TPT_TRACE(1, STR("register subscriber %d.",
			                 client_mbox));
		}
		data->subscriber = client_mbox;
	}

	msg = itc_alloc(sizeof(struct mmi_subscribe_cfm),
	                RHD_MMI_SUBSCRIBE_CFM);
	msg->any_msg.connection_ref = client_ref;
	msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;
	itc_send(&msg, sender_mbox, ITC_MY_MBOX);

	/* inform subscriber about current state */
	inform_subscriber(data);
}

/**
 * Function handle_st_release
 * If button released in 2 seconds
 */
static void handle_st_release(struct mmi_block *mmi)
{
	TPT_TRACE(3, "button released.");
	uint32_t res = 0;
	/* state check */
	if (mmi->state != MMI_MAINTENANCE_STATE_DEACTIVATED) {
		TPT_ERROR(STR("MMI state out of sync. "
		             "Release received when state = %s"
		             " and button_state = %s.",
		             state_to_str(mmi->state),
		             button_to_str(mmi->button_state)));
	}

	res = validate_request(mmi, MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS);
	if (res != NEW_STATE_OK)
		TPT_ERROR("state machine out of sync.");

	/* Vii has already been set in button press */
	mmi->button_state = BUTTON_STATE_IDLE;
	mmi->state        = MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS;
	inform_subscriber(mmi);

	save_to_fs(mmi->state);
}

/**
 * Function handle_mt_release
 * If button released in 2-7 seconds
 */
static void handle_mt_release(struct mmi_block *mmi)
{
	TPT_TRACE(3, "button released.");
	uint32_t res = 0;
	/* state check */
	if (mmi->state != MMI_MAINTENANCE_STATE_DEACTIVATED) {
		TPT_ERROR(STR("MMI state out of sync. "
		              "Release received when state = %s"
		              " and button_state = %s.",
		              state_to_str(mmi->state),
		              button_to_str(mmi->button_state)));
	}

	res = validate_request(mmi, MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC);
	if (res != NEW_STATE_OK)
		TPT_ERROR("state machine out of sync.");

	/* Vii has already been set in handle_mt_press */
	mmi->button_state = BUTTON_STATE_IDLE;
	mmi->state        = MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC;
	inform_subscriber(mmi);

	save_to_fs(mmi->state);
}

/**
 * Function handle_second_press_release
 */
static void handle_second_press_release(struct mmi_block *mmi)
{
	uint32_t res = 0;

	TPT_TRACE(3, "button second press_release .");

	if (mmi->state != MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC) {
		/* change state to DEACTIVATED */
		res = validate_request(mmi, MMI_MAINTENANCE_STATE_DEACTIVATED);
		if (res != NEW_STATE_OK)
			TPT_ERROR("state machine out of sync.");

		change_vii(mmi->state, MMI_MAINTENANCE_STATE_DEACTIVATED);
		mmi->button_state = BUTTON_STATE_IDLE;
		mmi->state        = MMI_MAINTENANCE_STATE_DEACTIVATED;
		inform_subscriber(mmi);
		save_to_fs(mmi->state);
	} else {
		mmi->button_state = BUTTON_STATE_IDLE;
	}
}

/**
 * Function handle_st_press
 */
static void handle_st_press(struct mmi_block *mmi)
{
	union itc_msg *msg;
	mmi->button_state = BUTTON_STATE_ST_PRESS;

	/* inform any subscriber but since state is not "suppress alarms"
	   before button is release we don't use inform_subscribers() */
	if (mmi->subscriber != 0) {
		msg = itc_alloc(sizeof(struct mmi_state_ind),
		                RHD_MMI_STATE_IND);
		msg->state_ind.state = MMI_BUTTON_PRESSED;
		TPT_SEND_SIG(msg->msgno, mmi->subscriber,
		             "send RHD_MMI_STATE_IND: \
		              state MMI_BUTTON_PRESSED.");
		itc_send(&msg, mmi->subscriber, ITC_MY_MBOX);
	}
	/* change LED to Fast Blink but still no real state change */
	change_vii(MMI_MAINTENANCE_STATE_DEACTIVATED,
	           MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS);
}

/**
 * Function handle_mt_press
 */
static void handle_mt_press(struct mmi_block *mmi)
{
	TPT_TRACE(3, "Change to slow blink.");
	/* check state, ignore the interrupt for 2nd press */
	if (mmi->button_state == BUTTON_STATE_SECOND_PRESS)
		return;

	if ((mmi->state != MMI_MAINTENANCE_STATE_DEACTIVATED) ||
	    (mmi->button_state != BUTTON_STATE_ST_PRESS)) {
		TPT_TRACE(3, STR("MMI state out of synce."
		                 " Press received when state = %s"
		                 " and buttonState = %s.",
		                 state_to_str(mmi->state),
		                 button_to_str(mmi->button_state)));
	}

	mmi->button_state = BUTTON_STATE_MT_PRESS;
	change_vii(MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS,
	           MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC);
}

/**
 * Function handle_button_press_ind
 */
static void handle_button_press_ind(struct mmi_block *mmi)
{
	union itc_msg *msg = NULL;

	if (mmi->state == MMI_MAINTENANCE_STATE_DEACTIVATED) {
		TPT_TRACE(3, "button pressed.");
		handle_st_press(mmi);
	} else {
		/* if state is not MMI_MAINTENANCE_STATE_DEACTIVATED ->
		    second press or reset */
		TPT_TRACE(3, "button second press or reset press.");

		mmi->button_state = BUTTON_STATE_SECOND_PRESS;

		/* assume subscriber is interested in this case
		 * also (not specefied) */
		if (mmi->subscriber != 0) {
			msg = itc_alloc(sizeof(struct mmi_state_ind),
			                RHD_MMI_STATE_IND);
			msg->state_ind.state = MMI_BUTTON_PRESSED;
			TPT_SEND_SIG(msg->msgno, mmi->subscriber,
			             "send RHD_MMI_STATE_IND: "
			             "state MMI_BUTTON_PRESSED.");
			itc_send(&msg, mmi->subscriber, ITC_MY_MBOX);
		}

		/* no Vii or timeouts if second press */
	}
}

/**
 * Function handle_button_release_ind
 */
static void handle_button_release_ind(struct mmi_block *mmi)
{
	if (mmi->button_state == BUTTON_STATE_ST_PRESS)
		handle_st_release(mmi);
	else if (mmi->button_state == BUTTON_STATE_MT_PRESS)
		handle_mt_release(mmi);
	else if (mmi->button_state == BUTTON_STATE_SECOND_PRESS)
		handle_second_press_release(mmi);
}

/**
 * Function init_mmi_data
 */
static int init_mmi_data(struct mmi_block *mmi)
{
	int result = 0;
	char *env_value;
	/* init mmi */
	mmi->state        = MMI_MAINTENANCE_STATE_DEACTIVATED;
	mmi->button_state = BUTTON_STATE_IDLE;
	mmi->subscriber   = 0;
	mmi->button_reset  = 0;

	env_value = getenv("SYS_RESET_CAUSE");

	if (env_value == NULL) {
		TPT_INFO("Missing status parameter SYS_RESET_CAUSE.");
		result = -EFAULT;
	} else if (!strcmp(env_value, MMI_RESET)){
		mmi->button_reset = 1;
	}

	return result;
}

/**
 * Function set_init_state
 */
static void set_init_state(struct mmi_block *mmi)
{
	if (mmi->button_reset == 0) /* not button reset */
		return;

	uint32_t state;
	FILE *fp;
	fp = fopen(MMI_STATE_FILE, "r");
	if (!fp) {
		TPT_ERROR(STR("reset caused by maintenance button, "
		              "but state did not survive restart, "
		              "open %s failed",
		              MMI_STATE_FILE));
		return;
	}
	if (fscanf(fp, "%u", &state) != 1) {
		TPT_ERROR("Cannot find mmi state from file");
		fclose(fp);
		return;
	}
	if (validate_state(state) == 1) {
		mmi->state = state;
		/* state machine force REMOVING_TRAFFIC to
		   MAINTENANCE_MODE when reset */
		if (mmi->state == MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC)
			mmi->state =
			        MMI_MAINTENANCE_STATE_MAINTENANCE_MODE;
		change_vii(MMI_MAINTENANCE_STATE_DEACTIVATED, mmi->state);
		TPT_INFO(STR("reset caused by maintenance button,"
		             " goto state %s.",
		             state_to_str(mmi->state)));
	} else {
		TPT_ERROR(STR("reset caused by maintenance button,"
		              "but state did not survive restart,"
		              "invalid state = %d",
		              state));
	}
	fclose(fp);

}

/**
 * Function init_regs
 * Init MMI registers.
 */
static void init_regs(void)
{
	/* store pointers to MMI registers */
	mmi_reg.status_trap = (uint32_t *) (((uintptr_t)mmap_base) +
	                                    XENON_MMI_STATUS_TRAP);
	mmi_reg.status_mask = (uint32_t *) (((uintptr_t)mmap_base) +
	                                    XENON_MMI_STATUS_MASK);
	/* Clear TRAP register (all bits). */
	*(mmi_reg.status_trap) = ~0;

	/* Set MASK register */
	*(mmi_reg.status_mask) =
	        STATUS_PRESSED | STATUS_MEDIUM | STATUS_RELEASED;

}

/***
 * Function send_press_ind
 * Send press IND to MMI
 ***/
static void send_press_ind(void)
{
	union itc_msg *msg;
	msg = itc_alloc(sizeof(struct mmi_button_event_ind),
	                RHD_MMI_BUTTON_PRESS_IND);
	msg->any_msg.connection_ref = 0;
	itc_send(&msg, mmi_mbox, ITC_MY_MBOX);
}

/***
 * Function send_mt_ind
 * Send medium time press IND to MMI
 ***/
static void send_mt_ind(void)
{
	union itc_msg *msg;
	msg = itc_alloc(sizeof(struct mmi_button_event_ind),
	                RHD_MMI_BUTTON_MT_IND);
	msg->any_msg.connection_ref = 0;
	itc_send(&msg, mmi_mbox, ITC_MY_MBOX);
}

/***
 * Function send_release_ind
 * Send release IND to MMI
 ***/
static void send_release_ind(void)
{
	union itc_msg *msg;
	msg = itc_alloc(sizeof(struct mmi_button_event_ind),
	                RHD_MMI_BUTTON_RELEASE_IND);
	msg->any_msg.connection_ref = 0;
	itc_send(&msg, mmi_mbox, ITC_MY_MBOX);
}

/***
 * Function handle_interrupt_event
 *
 ***/
static void handle_interrupt_event(mmi_event_t event)
{
	switch(event) {
	case MMI_PRESS_INT:
		send_press_ind();
		break;

	case MMI_MT_INT:
		send_mt_ind();
		break;

	case MMI_RELEASE_INT:
		send_release_ind();
		break;

	default:
		TPT_ERROR("Illegal event.");
	}

}

/**
 * Function mmi_int_notifier_post
 * The interrupt handler called only for the first time.
 */
static void mmi_int_notifier_post(__attribute__((unused)) void *data)
{
	if (int_mbox == ITC_NO_ID)
		int_mbox = itc_create_mailbox(MMI_INT_MBOX, 0);

	/* Change interrupt handler */
	if (uio_irq_set_notifier(uio_handle, mmi_int_notifier, NULL))
		TPT_ERROR("Set IRQ handler function failed.");
	mmi_int_notifier(data);
}

/**
 * Function mmi_int_notifier
 * The interrupt handler for MMI.
 */
static void mmi_int_notifier(__attribute__((unused)) void *data)
{
	uint32_t status_trap;

	status_trap = *(mmi_reg.status_trap);
	/* clear status trap */
	*(mmi_reg.status_trap) = status_trap;

	TPT_TRACE(3,
	          STR("---> hwInterrupt status_trap=0x%x.",
	              status_trap));

	if (!status_trap ||
	    (status_trap & ~(STATUS_PRESSED | STATUS_MEDIUM | STATUS_RELEASED))) {
		/* This should not happen. */
		TPT_ERROR("An unknown MMI interrupt occurred.");
		goto mmi_int_notifier_end;
	}

	if (mmi_data.button_state == BUTTON_STATE_IDLE) {
		if (status_trap & STATUS_PRESSED)
			handle_interrupt_event(MMI_PRESS_INT);
		if (status_trap & STATUS_MEDIUM)
			handle_interrupt_event(MMI_MT_INT);
		if (status_trap & STATUS_RELEASED)
			handle_interrupt_event(MMI_RELEASE_INT);
	} else {
		if (status_trap & STATUS_MEDIUM)
			handle_interrupt_event(MMI_MT_INT);
		if (status_trap & STATUS_RELEASED)
			handle_interrupt_event(MMI_RELEASE_INT);
		if (status_trap & STATUS_PRESSED)
			handle_interrupt_event(MMI_PRESS_INT);
	}

mmi_int_notifier_end:
	uio_enable_irq(uio_handle);
	return;

}

/**
 * Function mmi_init
 * Initialize thread mutex and interrupt handler
 */
static int mmi_init(void)
{
	if (init_mmi_data(&mmi_data))
		return -EFAULT;
	/* Initialize ITC */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	/* Create our mailbox. */
	mmi_mbox = itc_create_mailbox(RHD_MMI_MAILBOX, 0);
	if (mmi_mbox == ITC_NO_ID)
		return -EFAULT;

	uio_handle = uio_open(UIO_DEV_MMI);
	if (uio_handle == UIO_OPEN_FAILED) {
		TPT_ERROR("Failed to open uio");
		return -EFAULT;
	}

	mmap_base = uio_mmap(uio_handle);
	if (mmap_base == MAP_FAILED) {
		mmap_base = NULL;
		TPT_ERROR("Failed to peform UIO memory mapping");
		goto init_error;
	}

	uio_disable_irq(uio_handle);

	/**
	 * Set the notifier for the interrupts.
	 */
	if (uio_irq_set_notifier(uio_handle, mmi_int_notifier_post, NULL)) {
		TPT_ERROR("Failed to set UIO interrupt notifier");
		goto init_error;
	}
	TPT_TRACE(3, "UIO interrupt notifier set.");

	/* start interrupt handler */
	if (uio_bind_irq(uio_handle)) {
		TPT_ERROR("Unable to start UIO interrupt handler");
		goto init_error;
	}
	TPT_TRACE(3, "UIO interrupt handler started");

	uio_enable_irq(uio_handle);

	init_regs();

	return 0;

init_error:
	if (uio_handle != UIO_OPEN_FAILED) {
		uio_close(uio_handle);
	}
	return -EFAULT;
}

static int set_vii_conn(void)
{
	uint32_t res;
	uint32_t procedure_ref = 0;
	uint32_t requested_versions[] = {VII_SERVER_VERSIONS};
	union itc_msg *msg;
	uint32_t resp_msg[] = {2, EXIT_SIGNAL, ITC_LOCATE_DEFAULT_NO};

	/* Get RHD_VII mailbox */
	itc_locate_async(RHD_VII_MAILBOX, NULL, ITC_MY_MBOX);
	msg = itc_receive(resp_msg, ITC_NO_TMO, ITC_FROM_ALL);
	if (msg->msgno == EXIT_SIGNAL) {
		TPT_INFO("rhd-mmi exiting as ordered");
		itc_free(&msg);
		return 1;
	}
	vii_conn.server_mbox = itc_sender(msg);
	itc_free(&msg);
	TPT_TRACE(1, "Found vii server mailbox");

	res = conn_establish(
	              /*input parameters*/
	              vii_conn.server_mbox,
	              ++procedure_ref,
	              MMI_CLIENT_REF,
	              sizeof(requested_versions)/sizeof(requested_versions[0]),
	              requested_versions,
	              &vii_conn_messages,
	              1000,
	              /*returned values*/
	              &vii_conn.server_ref,
	              &vii_conn.selected_version);

	if (res != CONN_ESTABLISH_SUCCESS) {
		TPT_ERROR(STR("MMI:Connection establish to VII failed"
		              "(reason:0x%08x).", res));
		return -EFAULT;
	}

	return 0;
}

/**
 * Function read_messages
 * Start reading received messages through ITC and handle them.
 */
static int read_messages(conn_server_handle_t handle)
{
	int ret;
	union itc_msg *msg;
	struct conn_client_info client_info;

	ret = set_vii_conn();
	if (ret)
		return (ret > 0) ? 0 : ret;

	set_init_state(&mmi_data);
	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		/* Handle interrupt messages */
		switch (msg->msgno) {
		case RHD_MMI_BUTTON_PRESS_IND:
			TPT_REC_SIG(msg->msgno, "RHD_MMI_BUTTON_PRESS_IND.");
			handle_button_press_ind(&mmi_data);
			goto next_loop;

		case RHD_MMI_BUTTON_MT_IND:
			TPT_REC_SIG(msg->msgno, "RHD_MMI_BUTTON_MT_IND.");
			handle_mt_press(&mmi_data);
			goto next_loop;

		case RHD_MMI_BUTTON_RELEASE_IND:
			TPT_REC_SIG(msg->msgno, "RHD_MMI_BUTTON_RELEASE_IND.");
			handle_button_release_ind(&mmi_data);
			goto next_loop;
		case EXIT_SIGNAL:
			TPT_INFO("rhd-mmi exiting as ordered");
			itc_free(&msg);
			return 0;

		default:
			break;
		}

		/*Handle CONN_ESTABLISH... messages (and messages from
		 unknown clients.*/
		if(!conn_check_client(handle, &msg, &client_info))
			continue;

		sender_mbox = client_info.sender;

		switch (msg->msgno) {
		case RHD_MMI_SUBSCRIBE_REQ:
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_MMI_SUBSCRIBE_REQ: sender=0x%x.",
			                sender_mbox));
			handle_subscribe_req(&mmi_data, msg,
			                     client_info.client_ref);
			break;

		case RHD_MMI_SET_STATE_REQ:
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_MMI_SET_STATE_REQ: state=%s.",
			                state_to_str(msg->set_state_req.state)));
			handle_set_state_req(&mmi_data,
			                     msg, client_info.client_ref);
			break;

		default:
			TPT_ERROR(STR("Received unexpected "
			              "message %d.", msg->msgno));
			break;
		}
next_loop:

		itc_free(&msg);
	}
}


/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{
	if (mmi_data.subscriber == client_info->connected_mailbox) {
		TPT_INFO("Subscriber died.");
		mmi_data.subscriber = 0;
	}
}

/**
 * Function conn_server_init
 */
static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	uint32_t supported_versions[] = {MMI_SERVER_VERSIONS};
	struct conn_event_callbacks cb = { NULL, client_disconnect,
		       client_disconnect, NULL
	};
	int conn_res = conn_establish_server_init(&handle,
	                sizeof(supported_versions) /
	                sizeof(supported_versions[0]),
	                supported_versions,
	                &mmi_conn_messages, 0, &cb);
	if (conn_res != CONN_INIT_OK)
		return NULL;

	return handle;
}

/**
 * print_usage
 */
static void print_usage()
{
	printf("Usage: rhd-mmi <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, mmi_mbox, ITC_MY_MBOX);
}

/**
 * Main function
 * start the rhd_mmi daemon
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

		if (!mmi_init() && (handle != NULL)) {
			/* Start processing ITC messages.
			 * No return.
			 */
			if (signal(SIGTERM, exit_handler) == SIG_ERR) {
				TPT_ERROR("Failed to install "
				          "signal exit handler");
				exit(1);
			}
			ret = read_messages(handle);

		} else {
			TPT_INFO("Failed to intialize mmi");
			ret = -EFAULT;
		}
	} else {
		TPT_ERROR(STR("Failed to start daemon %s", DAEMON_NAME));
		ret = -EFAULT;
	}

	return ret;

}

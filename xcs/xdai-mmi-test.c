#include "common.h"
#include "xdai_xdmmi_if.h"
#include "xpai_xmmi_if.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <itc.h>
#include <stdbool.h>
#include <pthread.h>
#include <unistd.h>
#include <uio_helper.h>

#define UNUSED __attribute__((__unused__))
#define MAX_MAILBOX_NUM    32
#define MBOX "MMI_TEST"
#define MSG_TIMEOUT 15000

#define XPAI_UIO_DEV_MMI_TEST       "mmi_test"
#define MMI_STATE_FILE              "/var/log/mmi/state"
#define TEST_ENTRY_FILE             "/var/log/mmi/test"
#define TEST_ENTRY_PATH             "/var/log/mmi"

/* MMI register offset */
#define XPAI_MMI_TEST_STATUS        (0x260)
#define XPAI_MMI_TEST_STATUS_FORCE  (0x26c)

/* MMI status mask */
#define XPAI_MMI_STATUS_PRESSED         1 /* button pressed */
#define XPAI_MMI_STATUS_MEDIUM          2 /* button pressed more than 2s */
#define XPAI_MMI_STATUS_RELEASED        4 /* button released */

#define SLAVE_ANNOUNCE  0x12341001 /* dummy value for announce signal */
#define SLAVE_STATUS    0x12341002
struct slave_status_s {
	uint32_t msgno;
	uint32_t error;
};

#define XPAI_MMI_DEFAULT_STATE  XDAI_MAINTENANCE_STATE_DEACTIVATED

/* Error codes */
#define XPAI_MMI_EXIT_FAILURE   -1
#define XPAI_MMI_SUCCESS        0
#define XPAI_MMI_WRONG_STATE    1
#define XPAI_MMI_SUBSCRIBE_ERR  2
#define XPAI_MMI_SET_FAILED     3
#define XPAI_MMI_NO_IND         4
#define XPAI_MMI_LED_FAIL       5
#define XPAI_MMI_OTHER_ERROR    6

static itc_mbox_id_t server_mbox;
static itc_mbox_id_t main_mbox;
static itc_mbox_id_t sub_mbox;
static pthread_t     sub_thread;
static uint32_t      client_ref = 1642; /*dummy value for MMI*/

static void *slave(void UNUSED *ptr);
static uint32_t handle_mmi_state_ind(uint32_t state);

union itc_msg {
	uint32_t                   msgno;
	XDAI_MaintenanceStateIndS  state_ind;
	struct slave_status_s      slave_status;
};

static UIO_HANDLE_ uio_handle = 0;
static void *mmap_base = NULL;
static volatile uint32_t *reg_status_force;

struct transition_s {
	uint32_t  s_initial; /* Initial state */
	uint32_t  s_next; /* Next state*/
	bool      ind; /* We get indication */
};
#define XPAI_POS_TRANSITIONS (sizeof(pos_transitions)/sizeof(*pos_transitions))
#define XPAI_NEG_TRANSITIONS (sizeof(neg_transitions)/sizeof(*neg_transitions))

/* All existing transitions */
static struct transition_s pos_transitions[] = {
	/* All existing transitions from XDAI_MAINTENANCE_STATE_DEACTIVATED and
	 * XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS */
	{XDAI_MAINTENANCE_STATE_DEACTIVATED,      XDAI_MAINTENANCE_STATE_DEACTIVATED,      false},
	{XDAI_MAINTENANCE_STATE_DEACTIVATED,      XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  true},
	{XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  false},
	{XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  XDAI_MAINTENANCE_STATE_DEACTIVATED,      true},
	{XDAI_MAINTENANCE_STATE_DEACTIVATED,      XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, true},
	/* All existing transitions from XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC */
	{XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, false},
	{XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, true},
	/* All existing transitions from XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE */
	{XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, false},
	{XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, XDAI_MAINTENANCE_STATE_DEACTIVATED,      true},
};

/* All non-existent transitions */
static struct transition_s neg_transitions[] = {
	/* All non-existent transitions from XDAI_MAINTENANCE_STATE_DEACTIVATED */
	{XDAI_MAINTENANCE_STATE_DEACTIVATED,      XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, false},
	/* Go to supress alarm state using existing transition */
	{XDAI_MAINTENANCE_STATE_DEACTIVATED,      XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  true},
	/* All non-existent transitions from XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS */
	{XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, false},
	{XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, false},
	/* Go to removing traffic state using existing transitions */
	{XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  XDAI_MAINTENANCE_STATE_DEACTIVATED,      true},
	{XDAI_MAINTENANCE_STATE_DEACTIVATED,      XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, true},
	/* All non-existent transitions from XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC */
	{XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, XDAI_MAINTENANCE_STATE_DEACTIVATED,      false},
	{XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  false},
	/* Go to XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE using existent transition */
	{XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, true},
	/* All non-existent transitions from XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE */
	{XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS,  false},
	{XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC, false},
	/* Return back to default state */
	{XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE, XDAI_MAINTENANCE_STATE_DEACTIVATED,      true},
};

static char *state_to_str(uint32_t state)
{
	switch (state) {
	case XDAI_BUTTON_PRESSED:
		return "XDAI_BUTTON_PRESSED";

	case XDAI_MAINTENANCE_STATE_DEACTIVATED:
		return "XDAI_MAINTENANCE_STATE_DEACTIVATED";

	case XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
		return "XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS";

	case XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
		return "XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC";

	case XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE:
		return "XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE";

	default:
		return "Unknown state";
	}
	return "";
}

static char *ind_to_str(uint32_t ind)
{
	switch (ind) {
	case XPAI_VII_M_FULL_MAINTENANCE_MODE:
		return "XPAI_VII_M_FULL_MAINTENANCE_MODE";

	case XPAI_VII_M_REMOVING_TRAFFIC:
		return "XPAI_VII_M_REMOVING_TRAFFIC";

	case XPAI_VII_BOARD_BUSY_START:
		return "XPAI_VII_BOARD_BUSY_START";

	case XPAI_VII_M_ALARMS_SUPPRESSED:
		return "XPAI_VII_M_ALARMS_SUPPRESSED";

	case XPAI_VII_M_FULL_MAINTENANCE_MODE_END:
		return "XPAI_VII_M_FULL_MAINTENANCE_MODE_END";

	case XPAI_VII_M_REMOVING_TRAFFIC_END:
		return "XPAI_VII_M_REMOVING_TRAFFIC_END";

	case XPAI_VII_BOARD_BUSY_END:
		return "XPAI_VII_BOARD_BUSY_END";

	case XPAI_VII_M_ALARMS_SUPPRESSED_END:
		return "XPAI_VII_M_ALARMS_SUPPRESSED_END";

	default:
		return "Unknown indication.";
	}
	return "";
}

static uint32_t check_state_ind(uint32_t expected_state)
{
	uint32_t res = XPAI_MMI_SUCCESS;
	uint32_t state_ind_filter[] = {1, XDAI_MAINTENANCE_STATE_IND};
	union itc_msg *recv_msg;

	recv_msg = itc_receive(state_ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
	if (!recv_msg) {
		printf("Error, no XDAI_MAINTENANCE_STATE_IND received\n");
		return XPAI_MMI_NO_IND;
	}
	printf("Received MMI state indication, state: %s\n",
	       state_to_str(recv_msg->state_ind.state));
	if (recv_msg->state_ind.state != expected_state) {
		printf("Error, expected state: %s\n",
		       state_to_str(expected_state));
		res = XPAI_MMI_WRONG_STATE;
		goto the_end;
	}
	res = handle_mmi_state_ind(recv_msg->state_ind.state);

the_end:
	itc_free(&recv_msg);

	return res;
}

static uint32_t check_led_status(uint32_t led)
{
	uint32_t res, ind;
	res = XPAI_GetVii2(XPAI_VII_MAINTENANCE_LED, &ind);
	if (res != XPAI_GET_VII2_OK) {
		printf("Get LED state failed, error code %u\n", res);
		return XPAI_MMI_LED_FAIL;
	} else if (ind != led) {
		printf("LED check failed, state is %s\n", ind_to_str(ind));
		return XPAI_MMI_LED_FAIL;
	}

	return XPAI_MMI_SUCCESS;
}

static uint32_t check_fs_state(uint32_t expected_state)
{
	uint32_t state;
	FILE *fp;

	fp = fopen(MMI_STATE_FILE, "r");
	if (!fp) {
		printf("Cannot find state file\n");
		return XPAI_MMI_OTHER_ERROR;
	}
	if (fscanf(fp, "%u", &state) != 1) {
		printf("Cannot find mmi state from file\n");
		fclose(fp);
		return XPAI_MMI_WRONG_STATE;
	}
	fclose(fp);
	if (state != expected_state) {
		printf("Wrong state in file, get %s while expect %s\n",
		       state_to_str(state),
		       state_to_str(expected_state));
		return XPAI_MMI_WRONG_STATE;
	}
	return XPAI_MMI_SUCCESS;
}

static int subscribe_to_server(uint32_t expected_state)
{
	uint32_t rv;
	int      res = XPAI_MMI_SUCCESS;
	union itc_msg *recv_msg = NULL;
	uint32_t ind_filter[] = {1, XDAI_MAINTENANCE_STATE_IND};

	/* Subscribe to mmi server */
	rv = XDAI_SubscribeMaintenanceState(main_mbox);
	if (rv != XDAI_SUBSCRIBE_SUCCESS) {
		printf("%s: Subscribe to mmi server failed, return %u!\n",
		       __func__, rv);
		res = XPAI_MMI_SUBSCRIBE_ERR;
		return res;
	}

	/* Receive an indication with default state - deactivated */
	recv_msg = itc_receive(ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
	if (!recv_msg) {
		printf("We haven't received expected indication! Should have "
		       "received XDAI_MAINTENANCE_STATE_IND with state %s\n",
		       state_to_str(expected_state));
		res = XPAI_MMI_NO_IND;
		return res;
	}
	if (recv_msg->state_ind.state != expected_state) {
		printf("We should have received XDAI_MAINTENANCE_STATE_IND with"
		       " state %s but we received %s\n",
		       state_to_str(expected_state),
		       state_to_str(recv_msg->state_ind.state));
		res = XPAI_MMI_WRONG_STATE;
		itc_free(&recv_msg);
		return res;
	}
	itc_free(&recv_msg);
	return res;
}

static int check_test_file(uint32_t *entry)
{
	int res = -1;
	FILE *fp;
	struct stat sb;

	if (access(TEST_ENTRY_FILE, W_OK | R_OK) == 0)
		goto read_file;

	if (stat(TEST_ENTRY_PATH, &sb) == 0) {
		if (!S_ISDIR(sb.st_mode)) {
			if (remove(TEST_ENTRY_PATH)) {
				printf("Cannot delete file %s",
				       TEST_ENTRY_PATH);
				return res;
			}
			if (mkdir(TEST_ENTRY_PATH,
			          S_IRUSR | S_IWUSR |
			          S_IRGRP | S_IROTH) == -1) {
				printf("Failed to create %s", TEST_ENTRY_PATH);
				return res;
			}
		}
	}
	res = 0;
	return res;
read_file:
	fp = fopen(TEST_ENTRY_FILE, "r");
	if(!fp) {
		printf("Failed to open %s\n", TEST_ENTRY_FILE);
		return res;
	}
	if (fscanf(fp, "%u", entry) != 1) {
		printf("Cannot find test entry from file\n");
		fclose(fp);
		return res;
	}
	fclose(fp);
	res = 0;
	return res;
}

static int write_test_entry(uint32_t entry)
{
	int res = -1;
	FILE *fp;

	fp = fopen(TEST_ENTRY_FILE, "w");
	if (!fp) {
		printf("Failed to open %s", TEST_ENTRY_FILE);
		return res;
	}
	fprintf(fp, "%u", entry);
	fclose(fp);

	res = 0;
	return res;
}

static int create_slave(bool test_positive)
{
	union itc_msg *msg;
	int res;
	uint32_t announce_filter[] = {1, SLAVE_ANNOUNCE};

	/* Create slave to subscribe to the server */
	printf("Starting up slave\n");
	res = pthread_create(&sub_thread, NULL, slave, (void *)test_positive);

	if (res) {
		printf("%s: Failed to create thread, error: %d\n",
		       __func__, res);
		return XPAI_MMI_EXIT_FAILURE;
	}
	msg = itc_receive(announce_filter, MSG_TIMEOUT, ITC_FROM_ALL);
	if (!msg) {
		printf("%s: Timeout waiting for the thread to announce its "
		       "existence\n", __func__);
		return XPAI_MMI_EXIT_FAILURE;
	}
	sub_mbox = itc_sender(msg);
	itc_free(&msg);

	usleep(10000);

	return XPAI_MMI_SUCCESS;
}

static void *slave(void *ptr)
{
	union itc_msg *start_msg = NULL, *status_msg = NULL, *recv_msg = NULL;
	itc_mbox_id_t my_mbox;
	char my_mbox_name[50];
	uint32_t res, i;
	uint32_t ind_filter[] = {1, XDAI_MAINTENANCE_STATE_IND};

	uint32_t transitions_num = XPAI_POS_TRANSITIONS;
	struct transition_s *transitions = pos_transitions;
	bool test_positive = (bool)ptr;
	if (!test_positive) {
		transitions_num = XPAI_NEG_TRANSITIONS;
		transitions = neg_transitions;
	}

	sprintf(my_mbox_name, MBOX "_SLAVE_%08x", (uint32_t) pthread_self());
	my_mbox = itc_create_mailbox(my_mbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		sprintf("Slave unable to create ITC mailbox \"%s\"!\n",
		        my_mbox_name);
	}
	start_msg = itc_alloc(sizeof(uint32_t), SLAVE_ANNOUNCE);
	itc_send(&start_msg, main_mbox, ITC_MY_MBOX);

	status_msg = itc_alloc(sizeof(struct slave_status_s), SLAVE_STATUS);
	status_msg->slave_status.error = XPAI_MMI_SUCCESS;

	/* Subscribe to mmi server */
	res = XDAI_SubscribeMaintenanceState(my_mbox);
	if (res != XDAI_SUBSCRIBE_SUCCESS) {
		printf("%s: Subscribe to mmi server failed, return %u!\n",
		       __func__, res);
		status_msg->slave_status.error = XPAI_MMI_SUBSCRIBE_ERR;
		goto the_end;
	}

	/* Receive an indication with default state - deactivated */
	recv_msg = itc_receive(ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
	if (!recv_msg) {
		printf("We haven't received expected indication! Should have "
		       "received XDAI_MAINTENANCE_STATE_IND with state %s\n",
		       state_to_str(XPAI_MMI_DEFAULT_STATE));
		status_msg->slave_status.error = XPAI_MMI_NO_IND;
		goto the_end;
	}
	if (recv_msg->state_ind.state != XPAI_MMI_DEFAULT_STATE) {
		printf("We should have received XDAI_MAINTENANCE_STATE_IND with"
		       " state %s but we received %s\n",
		       state_to_str(XPAI_MMI_DEFAULT_STATE),
		       state_to_str(recv_msg->state_ind.state));
		status_msg->slave_status.error = XPAI_MMI_WRONG_STATE;
		goto the_end;
	}
	itc_free(&recv_msg);

	/* Traverse transitions from the transitions table */
	for (i = 0; i < transitions_num; i++) {

		printf("Current state: %s, set maintenance state %s\n",
		       state_to_str(transitions[i].s_initial),
		       state_to_str(transitions[i].s_next));
		res = XDAI_SetMaintenanceState(transitions[i].s_next);
		/* Set should never fail in positive test and should not fail
		 * in negative test if transition table contains indication */
		if (res != XDAI_SET_SUCCESS
		     && (test_positive || transitions[i].ind)) {
			printf("Set maintenance state failed, res = %u\n", res);
			status_msg->slave_status.error = XPAI_MMI_SET_FAILED;
			goto the_end;
		}
		/* Set should fail if negative test transition table entry does
		 *  not contain indication */
		else if ((res == XDAI_SET_SUCCESS)
			 && !test_positive
			 && !transitions[i].ind) {
			printf("Set maintenance state should have failed!\n");
			status_msg->slave_status.error = XPAI_MMI_OTHER_ERROR;
			goto the_end;
		}
		/* If we don't expect indication don't wait for the message */
		if (!transitions[i].ind) {
			continue;
		}
		recv_msg = itc_receive(ind_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if (!recv_msg) {
			printf("Expected indication not received!\n");
			status_msg->slave_status.error = XPAI_MMI_NO_IND;
			goto the_end;
		}
		printf("Received MMI state indication, state: %s\n",
		       state_to_str(recv_msg->state_ind.state));
		if (transitions[i].s_next != recv_msg->state_ind.state) {
			printf("Expected state different from received "
			       "state!");
			status_msg->slave_status.error = XPAI_MMI_WRONG_STATE;
			goto the_end;
		}
		res = handle_mmi_state_ind(recv_msg->state_ind.state);
		if (res) {
			status_msg->slave_status.error = res;
			goto the_end;
		}
		itc_free(&recv_msg);
	}

the_end:
	if (recv_msg) itc_free(&recv_msg);
	printf("Bye. %s exiting.\n", my_mbox_name);
	itc_send(&status_msg, main_mbox, ITC_MY_MBOX);
	itc_delete_mailbox(my_mbox);
	pthread_exit(NULL);
}

static int handle_init(void)
{
	int res;

	if(itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!\n", __func__);
		return XPAI_MMI_OTHER_ERROR;
	}

	main_mbox = itc_create_mailbox(MBOX, 0);
	if (main_mbox == ITC_NO_ID) {
		printf("%s: Cannot create ITC mailbox \"%s\"!\n",
		       __func__, MBOX);
		return XPAI_MMI_OTHER_ERROR;
	}

	server_mbox = itc_locate(RHD_MMI_MAILBOX);
	if (server_mbox == ITC_NO_ID) {
		printf("%s: Cannot find mmi server \"%s\"!\n",
		       __func__, RHD_MMI_MAILBOX);
		return XPAI_MMI_OTHER_ERROR;
	}

	res = xdai_mmi_init(client_ref);
	if (res != INIT_OK) {
		printf("MMI init failed, return %d\n", res);
		return XPAI_MMI_OTHER_ERROR;
	}

	res = xpai_vii_init(client_ref);
	if (res != INIT_OK) {
		printf("VII init failed, return %d\n", res);
		return XPAI_MMI_OTHER_ERROR;
	}

	return XPAI_MMI_SUCCESS;
}

static uint32_t handle_mmi_state_ind(uint32_t state)
{
	uint32_t res = XPAI_MMI_OTHER_ERROR;
	switch (state) {
	case XDAI_BUTTON_PRESSED:
		res = XPAI_MMI_SUCCESS;
		break;
	case XDAI_MAINTENANCE_STATE_DEACTIVATED:
		res = check_led_status(XPAI_VII_M_FULL_MAINTENANCE_MODE_END);
		break;
	case XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS:
		res = check_led_status(XPAI_VII_M_ALARMS_SUPPRESSED);
		break;
	case XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC:
		res = check_led_status(XPAI_VII_M_REMOVING_TRAFFIC);
		break;
	case XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE:
		res = check_led_status(XPAI_VII_M_FULL_MAINTENANCE_MODE);
		break;
	default:
		printf("Unknown state %u\n", state);
		break;
	}
	return res;
}

/* Test case verifies that it is possible to go through all MMI states
 * by calling XDAI_SetMaintenanceState() and checks if led states are changed
 * accordingly */
static int test1()
{
	int res;
	union itc_msg *msg = NULL;
	bool positive = true;

	res = create_slave(positive);
	if (res) {
		printf("%s: Failed to create and subscribe test thread.\n",
		       __func__);
		return XPAI_MMI_EXIT_FAILURE;
	}

	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	if (msg->msgno == SLAVE_STATUS) {
		res = msg->slave_status.error;
		pthread_join(sub_thread, NULL);
	} else {
		res = XPAI_MMI_EXIT_FAILURE;
	}

	itc_free(&msg);
	return res;
}

/* Press button short duration */
static void press_button_st()
{
	*(reg_status_force) = XPAI_MMI_STATUS_PRESSED;
}

/* Press button medium duration */
static void press_button_medium()
{
	press_button_st();
	usleep(2000000);
	*(reg_status_force) = XPAI_MMI_STATUS_MEDIUM;
}

/* Release the button */
static void release_button()
{
	*(reg_status_force) = XPAI_MMI_STATUS_RELEASED;
}

/* Press button short duration and release */
static uint32_t press_button_st_release()
{
	uint32_t rv;
	press_button_st();
	rv = check_state_ind(XDAI_BUTTON_PRESSED);
	if (rv) {
		return rv;
	}
	usleep(500000);
	release_button();
	rv = check_state_ind(XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS);
	if (rv) {
		return rv;
	}
	/* wait for mmi server to write */
	usleep(500000);
	rv = check_fs_state(XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS);
	if (rv) {
		return rv;
	}
	return XPAI_MMI_SUCCESS;
}

/* Press button short duration second time and release */
static uint32_t second_press_button_st_release()
{
	uint32_t rv;
	press_button_st();
	rv = check_state_ind(XDAI_BUTTON_PRESSED);
	if (rv) {
		return rv;
	}
	usleep(500000);
	release_button();
	rv = check_state_ind(XDAI_MAINTENANCE_STATE_DEACTIVATED);
	if (rv) {
		return rv;
	}
	/* wait for mmi server to write */
	usleep(500000);
	rv = check_fs_state(XDAI_MAINTENANCE_STATE_DEACTIVATED);
	if (rv) {
		return rv;
	}
	return XPAI_MMI_SUCCESS;
}

/* Press button medium duration and release */
static uint32_t press_button_mt_release()
{
	uint32_t rv;
	press_button_medium();
	rv = check_state_ind(XDAI_BUTTON_PRESSED);
	if (rv) {
		return rv;
	}
	usleep(1000000);
	release_button();
	rv = check_state_ind(XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC);
	if (rv) {
		return rv;
	}
	/* wait for mmi server to write */
	usleep(500000);
	rv = check_fs_state(XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC);
	if (rv) {
		return rv;
	}
	printf("Traffic removed\n");
	rv = XDAI_SetMaintenanceState(XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE);
	if (rv != XDAI_SET_SUCCESS) {
		printf("Set maintenance mode failed, error code %u\n", rv);
		return XPAI_MMI_SET_FAILED;
	}
	rv = check_state_ind(XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE);
	if (rv) {
		return rv;
	}

	return XPAI_MMI_SUCCESS;
}

/* Open uio, set map and store register pointer */
static int handle_uio_init()
{
	uio_handle = uio_open(XPAI_UIO_DEV_MMI_TEST);
	if (uio_handle == UIO_OPEN_FAILED) {
		printf("%s: Failed to open uio\n", __func__);
		return XPAI_MMI_EXIT_FAILURE;
	}

	mmap_base = uio_mmap(uio_handle);
	if (mmap_base == MAP_FAILED) {
		mmap_base = NULL;
		printf("%s: Failed to peform UIO memory mapping\n", __func__);
		return XPAI_MMI_EXIT_FAILURE;
	}

	/* Store pointer to MMI force register */
	reg_status_force = (uint32_t *) (((uintptr_t)mmap_base) +
	                                 XPAI_MMI_TEST_STATUS_FORCE);

	return XPAI_MMI_SUCCESS;
}

/* Close uio, remove uio map */
static void handle_uio_clear()
{
	if (uio_handle && uio_handle != UIO_OPEN_FAILED) {
		if (mmap_base != MAP_FAILED) {
			uio_munmap(uio_handle);
		}
		uio_close(uio_handle);
	}
}

/* Test case checks MMI indications received on button press interrupt and tests
 * if led states are changed accordingly */
static int test2()
{
	int res;
	int rv;
	uint32_t entry = 0;

	res = handle_uio_init();
	if (res) {
		printf("Uio init failed!\n");
		goto test2_end;
	}

	/* check test entry file */
	rv = check_test_file(&entry);
	if (rv < 0) {
		res = XPAI_MMI_OTHER_ERROR;
		goto test2_end;
	}

	switch(entry) {
	case 1:
		goto test_entry1;
		break;
	case 2:
		goto test_entry2;
		break;
	default:
		break;
	}
	/* Subscribe to mmi server and check initial state */
	res = subscribe_to_server(XPAI_MMI_DEFAULT_STATE);
	if (res != XPAI_MMI_SUCCESS)
		goto test2_end;

	/* Press button short and release */
	printf("Short button press and release first time\n");
	res = press_button_st_release();
	if (res) {
		printf("Short button press test failed\n");
		goto test2_end;
	}

	/* write test entry */
	entry = 1;
	rv = write_test_entry(entry);
	if (rv < 0) {
		res = XPAI_MMI_OTHER_ERROR;
		goto test2_end;
	}
	goto test2_end;

test_entry1:
	/* Restart mmi server and check the state */
	res = subscribe_to_server(XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS);
	if (res != XPAI_MMI_SUCCESS)
		goto test2_end;

	printf("Short button press and release second time\n");
	/* Press button short one more time - back to deactivated state */
	res = second_press_button_st_release();
	if (res) {
		printf("Short button press second time test failed\n");
		goto test2_end;
	}

	/* Medium duration button press */
	printf("Medium duration button press and release\n");
	res = press_button_mt_release();
	if (res) {
		printf("Medium button press test failed\n");
		goto test2_end;
	}

	/* write test entry */
	entry = 2;
	rv = write_test_entry(entry);
	if (rv < 0) {
		res = XPAI_MMI_OTHER_ERROR;
		goto test2_end;
	}
	goto test2_end;

test_entry2:
	/* Restart mmi server and check the state */
	res = subscribe_to_server(XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE);
	if (res != XPAI_MMI_SUCCESS)
		goto test2_end;

	printf("Short press button again - deactivate maintenance mode\n");
	res = second_press_button_st_release();
	if (res) {
		printf("Deactivating maintenance mode failed\n");
		goto test2_end;
	}

	/* write test entry */
	entry = 0;
	rv = write_test_entry(entry);
	if (rv < 0)
		res = XPAI_MMI_OTHER_ERROR;

test2_end:

	handle_uio_clear();

	return res;
}

/* Negative test case verifies that maintenace state cannot be set from initial
 * state if there are no transitions from inital state to new state */
static int test3()
{
	int res;
	union itc_msg *msg = NULL;
	bool positive = true;

	res = create_slave(!positive);
	if (res) {
		printf("%s: Failed to create and subscribe test thread.\n",
		       __func__);
		return XPAI_MMI_EXIT_FAILURE;
	}

	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	if (msg->msgno == SLAVE_STATUS) {
		res = msg->slave_status.error;
		pthread_join(sub_thread, NULL);
	} else {
		res = XPAI_MMI_EXIT_FAILURE;
	}

	itc_free(&msg);
	return res;
}

static void print_usage(char *pgm)
{
	printf("Usage: %s -tc <test case number>\n\n"
	       "\twhere <test case number> is one of:\n"
	       "\t1: Test going through MMI states with SetMaintenanceState\n"
	       "\t2: Test MMI button interrupt\n"
	       "\t3: Negative test for XDAI_SetMaintenanceState\n",
	       pgm);
}


int main(int argc, char **argv)
{
	int result = XPAI_MMI_SUCCESS;
	int tc = 0;

	if (argc < 3) {
		print_usage(argv[0]);
		exit(XPAI_MMI_EXIT_FAILURE);
	}

	if (argc >= 3) {
		if (strstr(argv[1], "-tc") != NULL) {
			tc = atoi(argv[2]);
		}
	}

	if (handle_init())
		return XPAI_MMI_EXIT_FAILURE;

	switch (tc) {
	case 1:
		printf("Go through MMI states with XDAI_SetMaintenanceState:\n");
		result = test1();
		break;
	case 2:
		printf("Test MMI button interrupt:\n");
		result = test2();
		break;
	case 3:
		printf("Negative test for XDAI_SetMaintenanceState:\n");
		result = test3();
		break;
	default:
		printf("Invalid tc number %d\n", tc);
		print_usage(argv[0]);
		result = XPAI_MMI_EXIT_FAILURE;
	}

	if (!result) {
		printf("\n*** SUCCESS ***\n\n"); 
	} else {
		printf("\n*** FAIL, error code:  %d ***\n", result);
	}

	return result;
}

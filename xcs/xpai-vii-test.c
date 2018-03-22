#include "common.h"
#include "xpai_xmmi_if.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <itc.h>
#include <stdbool.h>
#include <pthread.h>
#include <unistd.h>

/* In this test file we only test the XPAI interface for VII,
   other features will be tested together with MMI*/

#define UNUSED __attribute__((__unused__))
#define MAX_MAILBOX_NUM    32
#define MBOX "VII_TEST"

static itc_mbox_id_t server_mbox;
static itc_mbox_id_t main_mbox;

static uint32_t client_ref = 1511; /*dummy value for VII*/

static bool board_type_judge;/*judge bp or trxm */

struct test_events {
	uint32_t req; /* requested changes */
	uint32_t new; /* expected status */
};

static struct test_events fault_led[] = {
	{XPAI_VII_ERROR,    XPAI_VII_ERROR},
	{XPAI_VII_FAULT,    XPAI_VII_FAULT}
};

static struct test_events operation_led[] = {
	{XPAI_VII_MISSING_RESOURCE_START, XPAI_VII_MISSING_RESOURCE_START},
	{XPAI_VII_LOADTEST_START,         XPAI_VII_LOADTEST_START},
	{XPAI_VII_O_BUSY,                 XPAI_VII_O_BUSY},
	{XPAI_VII_BOOTTEST_START,         XPAI_VII_BOOTTEST_START},
	{XPAI_VII_NO_POWER,               XPAI_VII_NO_POWER}
};

static struct test_events maint_led[] = {
	{XPAI_VII_M_ALARMS_SUPPRESSED,     XPAI_VII_M_ALARMS_SUPPRESSED},
	{XPAI_VII_BOARD_BUSY_START,        XPAI_VII_BOARD_BUSY_START},
	{XPAI_VII_M_REMOVING_TRAFFIC,      XPAI_VII_M_REMOVING_TRAFFIC},
	{XPAI_VII_SHUTDOWN_START,          XPAI_VII_SHUTDOWN_START},
	{XPAI_VII_M_FULL_MAINTENANCE_MODE, XPAI_VII_M_FULL_MAINTENANCE_MODE},
	{XPAI_VII_BOARD_LOCKED,            XPAI_VII_BOARD_LOCKED}
};

static uint32_t fault_led_clear[] = {
	XPAI_VII_NO_ERROR,
	XPAI_VII_NO_FAULT
};

static uint32_t operation_led_clear[] = {
	XPAI_VII_MISSING_RESOURCE_END,
	XPAI_VII_LOADTEST_END,
	XPAI_VII_O_BUSY_END,
	XPAI_VII_BOOTTEST_END,
	XPAI_VII_POWER
};

static uint32_t maint_led_clear[] = {
	XPAI_VII_M_ALARMS_SUPPRESSED_END,
	XPAI_VII_BOARD_BUSY_END,
	XPAI_VII_M_REMOVING_TRAFFIC_END,
	XPAI_VII_SHUTDOWN_END,
	XPAI_VII_M_FULL_MAINTENANCE_MODE_END,
	XPAI_VII_BOARD_UNLOCKED
};

#define FAULT_LED_EVENTS (sizeof(fault_led)/sizeof(*fault_led))
#define OP_LED_EVENTS (sizeof(operation_led)/sizeof(*operation_led))
#define MAINT_LED_EVENTS (sizeof(maint_led)/sizeof(*maint_led))

static uint32_t test1_clear(void);
static uint32_t test2_clear_check(void);

static int handle_init(void)
{
	int res;

	if(itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!\n", __func__);
		return 1;
	}

	main_mbox = itc_create_mailbox(MBOX, 0);
	if (main_mbox == ITC_NO_ID) {
		printf("%s: Cannot create ITC mailbox \"%s\"!\n",
		       __func__, MBOX);
		return 1;
	}

	server_mbox = itc_locate(RHD_VII_MAILBOX);
	if (server_mbox == ITC_NO_ID) {
		printf("%s: Cannot find vii server \"%s\"!\n",
		       __func__, RHD_VII_MAILBOX);
		return 1;
	}

	res = xpai_vii_init(client_ref);
	if (res != INIT_OK) {
		printf("VII init failed, return %d\n", res);
		return 1;
	}

	return 0;
}

static char *ind2str(uint32_t ind)
{
	switch(ind) {
	case XPAI_VII_NO_FAULT:
		return "XPAI_VII_NO_FAULT";
	case XPAI_VII_ERROR:
		return "XPAI_VII_ERROR";
	case XPAI_VII_FAULT:
		return "XPAI_VII_FAULT";
	case XPAI_VII_POWER:
		return "XPAI_VII_POWER";
	case XPAI_VII_MISSING_RESOURCE_START:
		return "XPAI_VII_MISSING_RESOURCE_START";
	case XPAI_VII_LOADTEST_START:
		return "XPAI_VII_LOADTEST_START";
	case XPAI_VII_O_BUSY:
		return "XPAI_VII_BOOTTEST_START/XPAI_VII_O_BUSY";
	case XPAI_VII_NO_POWER:
		return "XPAI_VII_NO_POWER";
	case XPAI_VII_BOARD_UNLOCKED:
		return "XPAI_VII_BOARD_UNLOCKED";
	case XPAI_VII_M_ALARMS_SUPPRESSED:
		return "XPAI_VII_M_ALARMS_SUPPRESSED";
	case XPAI_VII_BOARD_BUSY_START:
		return "XPAI_VII_BOARD_BUSY_START";
	case XPAI_VII_M_REMOVING_TRAFFIC:
		return "XPAI_VII_SHUTDOWN_START/XPAI_VII_M_REMOVING_TRAFFIC";
	case XPAI_VII_M_FULL_MAINTENANCE_MODE:
		return "XPAI_VII_BOARD_LOCKED/XPAI_VII_M_FULL_MAINTENANCE_MODE";
	default:
		return "Unknown indication";
	}
	return "";
}

static uint32_t fault_led_test(void)
{
	uint32_t i;
	uint32_t res, ind;
	uint32_t led = XPAI_VII_FAULT_LED;

	printf("Test fault led\n");
	for (i = 0; i < FAULT_LED_EVENTS; i++) {
		ind = fault_led[i].req;
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Set fault LED to %s failed,"
			       " error code %u\n", ind2str(ind), res);
			return 1;
		}
		res = XPAI_GetVii2(led, &ind);
		if (res != XPAI_GET_VII2_OK) {
			printf("Get VII2 failed, error code %u\n", res);
			return 1;
		}
		if (fault_led[i].new != ind) {
			printf("Indication value check failed,"
			       "expect %s while get %s\n",
			       ind2str(fault_led[i].new),
			       ind2str(ind));
			return 1;
		}
		ind = fault_led_clear[i];
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Clear fault LED failed, error code %u\n", res);
			return 1;
		}
	}
	return 0;

}

static uint32_t operation_led_test(void)
{
	uint32_t res, ind, i;
	uint32_t led = XPAI_VII_OPERATIONAL_LED;

	printf("Test operational led\n");
	for (i = 0; i < OP_LED_EVENTS; i++) {
		ind = operation_led[i].req;
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Set operational LED failed, error code %u\n", res);
			return 1;
		}
		res = XPAI_GetVii2(led, &ind);
		if (res != XPAI_GET_VII2_OK) {
			printf("Get VII2 failed, error code %u\n", res);
			return 1;
		}
		if (operation_led[i].new != ind) {
			printf("Indication value check failed,"
			       "expect %s while get %s\n",
			       ind2str(operation_led[i].new),
			       ind2str(ind));
			return 1;
		}
		ind = operation_led_clear[i];
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Clear operational LED failed,"
			       " error code %u\n", res);
			return 1;
		}
	}
	return 0;

}

static uint32_t maint_led_test(void)
{
	uint32_t i;
	uint32_t res, ind;
	uint32_t led = XPAI_VII_MAINTENANCE_LED;

	printf("Test maintenance led\n");
	for (i = 0; i < MAINT_LED_EVENTS; i++) {
		ind = maint_led[i].req;
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Set maintenance LED failed,"
			       " error code %u\n", res);
			return 1;
		}
		res = XPAI_GetVii2(led, &ind);
		if (res != XPAI_GET_VII2_OK) {
			printf("Get VII2 failed, error code %u\n", res);
			return 1;
		}
		if (maint_led[i].new != ind) {
			printf("Indication value check failed,"
			       "expect %s while get %s\n",
			       ind2str(maint_led[i].new),
			       ind2str(ind));
			return 1;
		}
		ind = maint_led_clear[i];
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Clear maintenance LED failed,"
			       " error code %u\n", res);
			return 1;
		}
	}
	return 0;

}

static uint32_t set_leds_status(void)
{
	uint32_t res, ind;
	printf("start test 1, set leds status\n");
	/*set 3 common LEDs */
	ind = XPAI_VII_FAULT;
	res = XPAI_Vii(ind);
	if (res != XPAI_VII_OK) {
		printf("Set fault LED failed, error code %u\n", res);
		return 1;
	}

	ind = XPAI_VII_O_BUSY;
	res = XPAI_Vii(ind);
	if (res != XPAI_VII_OK) {
		printf("Set operation LED failed, error code %u\n", res);
		return 1;
	}
	if (board_type_judge){
		ind = XPAI_VII_M_REMOVING_TRAFFIC;
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Set Maintenance LED failed, error code %u\n", res);
			return 1;
		}
	}
	/* Invalid indication check */
	ind |= XPAI_VII_STEADY;
	res = XPAI_Vii(ind);
	if (res != XPAI_VII_INVALID_REQ) {
		printf("Invalid indication check failed, return code %u\n", res);
		return 1;
	}
#ifdef SPECIAL_LED
	/* Set special LED */
	ind = XPAI_VII_STEADY | VII_SPECIAL_LED;
	res = XPAI_Vii(ind);
	if (res != XPAI_VII_OK) {
		printf("Set special LEDs failed, error code %u\n", res);
		return 1;
	}
#endif
	return 0;
}

static uint32_t check_leds_status(void)
{
	uint32_t res, ind;
	uint32_t red, green, blue;

	printf("check_leds_status, get common leds status.\n");
	if (board_type_judge){
	     ind = XPAI_VII_FAULT | XPAI_VII_O_BUSY | XPAI_VII_M_REMOVING_TRAFFIC;
	}
	else{
	     ind = XPAI_VII_FAULT | XPAI_VII_O_BUSY;
	}
	res = XPAI_GetVii(&red, &green, &blue);
	if (res != XPAI_GET_VII_OK) {
		printf("Get VII failed, error code %u\n", res);
		return 1;
	}

	if (board_type_judge){
		if ((red | green | blue) != ind) {
			printf("Indication value check failed, ind = %u"
			       "red = %u, green = %u, blue = %u\n",
			       ind, red, green, blue);
			return 1;
		}
	}
	else{
		if ((red | green) != ind) {
			printf("Indication value check failed, ind = %u"
			       "red = %u, green = %u\n",
			       ind, red, green);
			return 1;
	     }
	}

#ifdef SPECIAL_LED
	uint32_t led;

	printf("get special leds status\n");
	led = XPAI_VII_SPECIAL0_LED;

	res = XPAI_GetVii2(led, &ind);
	if (res != XPAI_GET_VII2_OK) {
		printf("Get VII2 failed, error code %u\n", res);
		return 1;
	}
	if (XPAI_VII_STEADY != ind) {
		printf("Indication value check failed, ind = %u\n", ind);
		return 1;
	}
#endif
	return 0;
}

static uint32_t test_1(void)
{
	if (set_leds_status())
		return 1;

	if (check_leds_status())
		return 1;

	if (test1_clear()) {
		printf("Set LEDs to default failed after test1\n");
		return 1;
	}
	return 0;
}

static uint32_t test_2(void)
{
	if (fault_led_test())
		return 1;

	if (operation_led_test())
		return 1;
	if (board_type_judge) {
		 if (maint_led_test())
		 return 1;
	}
	/* Set all LEDs to default and quit*/
	if (test2_clear_check()) {
		printf("Set LEDs to default failed\n");
		return 1;
	}

	return 0;
}

static uint32_t test1_clear(void)
{
	uint32_t res, ind;
	uint32_t red, green, blue;

	ind = XPAI_VII_NO_FAULT;
	res = XPAI_Vii(ind);
	if (res != XPAI_VII_OK) {
		printf("Set fault LED failed, error code %u\n", res);
		return 1;
	}

	ind = XPAI_VII_O_BUSY_END;
	res = XPAI_Vii(ind);
	if (res != XPAI_VII_OK) {
		printf("Set operation LED failed, error code %u\n", res);
		return 1;
	}
	if (board_type_judge){
		ind = XPAI_VII_M_REMOVING_TRAFFIC_END;
		res = XPAI_Vii(ind);
		if (res != XPAI_VII_OK) {
			printf("Set Maintenance LED failed, error code %u\n", res);
			return 1;
		}
	}
#ifdef SPECIAL_LED
	/* Set special LED */
	ind = XPAI_VII_OFF | VII_SPECIAL_LED;
	res = XPAI_Vii(ind);
	if (res != XPAI_VII_OK) {
		printf("Set special LEDs failed, error code %u\n", res);
		return 1;
	}
#endif
	/* check led status */
	res = XPAI_GetVii(&red, &green, &blue);
	if (res != XPAI_GET_VII_OK) {
		printf("Get VII failed, error code %u\n", res);
		return 1;
	}
	if (board_type_judge) {
		if ((red | green | blue) != (XPAI_VII_NO_FAULT |
					    XPAI_VII_POWER |
					    XPAI_VII_BOARD_UNLOCKED)) {
			printf("Indication value check failed, "
			       "red = %s, green = %s, blue = %s\n",
			       ind2str(red), ind2str(green), ind2str(blue));
			return 1;
		}
	}else {
		if ((red | green) != (XPAI_VII_NO_FAULT | XPAI_VII_POWER)) {
			printf("Indication value check failed, "
			       "red = %s, green = %s\n",
			       ind2str(red), ind2str(green));
			return 1;

			}
	}
	return 0;
}

uint32_t test2_clear_check(void)
{

	uint32_t res;
	uint32_t red, green, blue;

	/* check led status */
	res = XPAI_GetVii(&red, &green, &blue);
	if (res != XPAI_GET_VII_OK) {
		printf("Get VII failed, error code %u\n", res);
		return 1;
	}
	if (board_type_judge) {
		if ((red | green | blue) != (XPAI_VII_NO_FAULT |
					    XPAI_VII_POWER |
			                    XPAI_VII_BOARD_UNLOCKED)) {
			printf("Indication value check failed, "
			       "red = %s, green = %s, blue = %s\n",
			       ind2str(red), ind2str(green), ind2str(blue));
			return 1;
		}
	}
	if (!board_type_judge) {
		if ((red | green) != (XPAI_VII_NO_FAULT | XPAI_VII_POWER)) {
			printf("Indication value check failed, "
			       "red = %s, green = %s\n",
			       ind2str(red), ind2str(green));
			return 1;

		}
	}
	return 0;
}

int main(int UNUSED argc, char UNUSED **argv)
{
	int res;
	char* boardtype;

	res = handle_init();
	if (res)
		return -1;
	if ((boardtype = getenv("SYS_BOARD_TYPE")) == NULL) {
	   printf("SYS_BOARD_TYPE not found, aborting");
	   return -1;
	}
	if (strcmp(boardtype ,"BP") == 0) {
		board_type_judge = 1;
	} else if (strcmp(boardtype ,"TRXM") == 0) {
		  board_type_judge = 0;
	} else {
		printf("SYS_BOARD_TYPE is wrong value, aborting");
			return -1;
	}
	/* Set and check the LEDs with XPAI_Vii */
	if (test_1())
		goto stop_test;

	/* check all the status for normal leds */
	if (test_2())
		goto stop_test;

	printf("\n*** All tests have passed ***\n");
stop_test:
	printf("Quit test\n");

	return 0;
}


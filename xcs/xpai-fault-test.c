#include "xpai_xcbc_fault_if.h"
#include "common.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <itc.h>
#include <stdbool.h>
#include <pthread.h>
#include <unistd.h>

#define UNUSED __attribute__((__unused__))
#define MAX_MAILBOX_NUM    32
#define MAX_NOF_SUB     XPAI_SUBSCRIBE_FAULTS_MAX_NO_SUBSCRIBERS /* = 5 */
#define MSG_TIMEOUT 1000
#define MBOX "XCBC_TEST"
#define SLAVE_ANNOUNCE 0x12341001 /* dummy value for announce signal */
#define SLAVE_END      0x12341002

static itc_mbox_id_t server_mbox;
static itc_mbox_id_t main_mbox;
static itc_mbox_id_t sub_mbox[MAX_NOF_SUB];
static pthread_t sub_thread[MAX_NOF_SUB];
static uint32_t client_ref = 1234;

static void *slave(void UNUSED *ptr);

union itc_msg {
	uint32_t msgno;
	struct XPAI_FaultIndS fault_ind;
};

static char *type_to_str(uint16_t type)
{
	switch (type) {
	case XPAI_GENERAL_SW_ERROR:
		return "XPAI_GENERAL_SW_ERROR";
	case XPAI_GENERAL_HW_ERROR:
		return "XPAI_GENERAL_HW_ERROR";
	default:
		return "Unknown fault type";
	}
	return "";
}

static char *act_to_str(uint16_t act)
{
	switch (act) {
	case XPAI_NO_SUGGESTED_ACTION:
		return "XPAI_NO_SUGGESTED_ACTION";
	case XPAI_ALIGN_STATES:
		return "XPAI_ALIGN_STATES";
	case XPAI_ENTITY_RESTART:
		return "XPAI_ENTITY_RESTART";
	case XPAI_ENTITY_FAILURE:
		return "XPAI_ENTITY_FAILURE";
	case XPAI_ENTITY_DEGRADED:
		return "XPAI_ENTITY_DEGRADED";
	default:
		return "Unkonwn action";
	}
	return "";
}


static int create_slaves(void)
{
	union itc_msg *msg;
	uint32_t announce_filter[] = {1, SLAVE_ANNOUNCE};

	/* create 5 client to subscribe to the server */
	printf("Starting up slaves\n");
	for (int i = 0; i < MAX_NOF_SUB; i++) {
		int res = pthread_create(&sub_thread[i], NULL, slave, NULL);
		if (res) {
			printf("%s: Failed to create thread, error: %d\n",
			       __func__, res);
			return -1;
		}
		msg = itc_receive(announce_filter, MSG_TIMEOUT, ITC_FROM_ALL);
		if(!msg) {
			printf("%s: Timeout waiting for the thread to \
			       announce its existence\n", __func__);
			return -1;
		}
		sub_mbox[i] = itc_sender(msg);
		itc_free(&msg);
	}

	usleep(10000);

	return 0;

}

static void close_slaves(void)
{
	union itc_msg *msg;
	void *retval;
	for (int i = 0; i < MAX_NOF_SUB; i++) {
		msg = itc_alloc(sizeof(uint32_t), SLAVE_END);
		itc_send(&msg, sub_mbox[i], ITC_MY_MBOX);
		pthread_join(sub_thread[i], &retval);
	}

}
static uint32_t test_1(void)
{
	uint32_t res;
	printf("Start test 1\n");
	res  = XPAI_SubscribeFaults(main_mbox);
	if (res != XPAI_SUBSCRIBE_FAULTS_NOK_UNSUPPORTED_CAPABILITY) {
		printf("%s: XPAI_SubscribeFaults max clients test failed, "
		       "return value %u!\n",
		       __func__, res);
		return 1;
	}

	return 0;
}

static uint32_t test_2(void)
{
	printf("Start test 2\n");
	uint32_t res;
	uint16_t fault_type = XPAI_GENERAL_SW_ERROR;
	uint16_t recov_act = XPAI_NO_SUGGESTED_ACTION;
	char *descr = "test fault";

	res = XPAI_Fault(fault_type, recov_act, descr);
	if (res != XPAI_FAULT_OK) {
		printf("%s: XPAI_Fault test failed,"
		       " return value %u!\n",
		       __func__, res);
		return 1;
	}

	return 0;
}

static uint32_t test_3(void)
{
	printf("Start test 3\n");
	uint32_t res;
	uint16_t fault_type = XPAI_GENERAL_SW_ERROR;
	uint16_t recov_act = XPAI_NO_SUGGESTED_ACTION;

	res = XPAI_FaultClear(fault_type, recov_act);
	if (res != XPAI_FAULT_CLEAR_OK) {
		printf("%s: XPAI_Fault test failed,\
		       return value %u!\n",
		       __func__, res);
		return 1;
	}

	return 0;
}

static uint32_t test_4(void)
{
	printf("Start test 4\n");
	uint32_t res;
	uint16_t fault_type = XPAI_GENERAL_SW_ERROR;
	uint16_t recov_act = XPAI_NO_SUGGESTED_ACTION;
	char *descr = "test 4 fault";
	char *descr_long = "Trying to use longer fault description than defined"
	                   " and checking if that is passing or whole system "
	                   " breaks and everything fails";

	/* Positive tests */

	res = XPAI_Fault(fault_type, recov_act, NULL);
	if (res != XPAI_FAULT_OK) {
		printf("%s: XPAI_Fault failed, return value %u!\n",
		       __func__, res);
		return 1;
	}
	usleep(50000);

	res = XPAI_FaultClear(fault_type, recov_act);
	if (res != XPAI_FAULT_CLEAR_OK) {
		printf("%s: XPAI_Fault clear failed, return value %u!\n",
		       __func__, res);
		return 1;
	}

	res = XPAI_Fault(fault_type, recov_act, descr_long);
	if (res != XPAI_FAULT_OK) {
		printf("%s: XPAI_Fault failed, return value %u!\n",
		       __func__, res);
		return 1;
	}
	usleep(50000);

	res = XPAI_FaultClear(fault_type, recov_act);
	if (res != XPAI_FAULT_CLEAR_OK) {
		printf("%s: XPAI_Fault clear failed, return value %u!\n",
		       __func__, res);
		return 1;
	}

	/* Negative tests */

	res = XPAI_Fault(fault_type, XPAI_FAULT_CLEAR_RECOVERY_ACTION_ALL, descr);
	if (res != XPAI_FAULT_NOT_OK) {
		printf("%s: XPAI_Fault should have failed, return value %u!\n",
		       __func__, res);
		return 1;
	}

	res = XPAI_Fault(XPAI_FAULT_CLEAR_FAULT_TYPE_ALL, recov_act, descr);
	if (res != XPAI_FAULT_NOT_OK) {
		printf("%s: XPAI_Fault should have failed, return value %u!\n",
		       __func__, res);
		return 1;
	}

	return 0;
}

static void *slave(void UNUSED *ptr)
{
	union itc_msg *msg = NULL;
	itc_mbox_id_t my_mbox;
	char my_mbox_name[50];
	uint32_t res;

	sprintf(my_mbox_name, MBOX "_SLAVE_%08x", (uint32_t) pthread_self());
	my_mbox = itc_create_mailbox(my_mbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		sprintf("Slave unable to create ITC mailbox \"%s\"!\n",
		        my_mbox_name);
	}
	msg = itc_alloc(sizeof(uint32_t), SLAVE_ANNOUNCE);
	itc_send(&msg, main_mbox, ITC_MY_MBOX);

	/* Subscribe to fault server */
	res = XPAI_SubscribeFaults(my_mbox);
	if (res != XPAI_SUBSCRIBE_FAULTS_OK)
		printf("%s: Subscribe to fault server failed, return %u!\n",
		       __func__, res);

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch (msg->msgno) {
		case SLAVE_END:
			goto the_end;
		case XPAI_FAULT_IND:
			printf("Receive fault indication, "
			       "fault type: %s, recovery action: %s,"
			       " fault description: %s\n",
			       type_to_str(msg->fault_ind.faultType),
			       act_to_str(msg->fault_ind.recoveryAction),
			       msg->fault_ind.faultDescription);
			break;
		default:
			printf("Receive unknown message 0x%08x\n",
			       msg->msgno);
			break;
		}
		itc_free(&msg);
	}
the_end:
	itc_free(&msg);
	if(0 == XPAI_UnsubscribeFaults(my_mbox))
	    printf("XPAI_UnsubscribeFaults return 0.\n");
	itc_delete_mailbox(my_mbox);
	printf("Bye. %s exiting.\n", my_mbox_name);
	pthread_exit(NULL);
}

int main(int UNUSED argc, char UNUSED **argv)
{
	int res;

	if(itc_init(MAX_NOF_SUB + 1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!\n", __func__);
		return -1;
	}

	main_mbox = itc_create_mailbox(MBOX, 0);
	if (main_mbox == ITC_NO_ID) {
		printf("%s: Cannot create ITC mailbox \"%s\"!\n",
		       __func__, MBOX);
		return -1;
	}

	server_mbox = itc_locate(FAULT_SERVER_MAILBOX);
	if (server_mbox == ITC_NO_ID) {
		printf("%s: Cannot find fault server \"%s\"!\n",
		       __func__, FAULT_SERVER_MAILBOX);
		return -1;
	}

	res = xpai_fault_init(client_ref);
	if (res != INIT_OK) {
		printf("%s: Initialize conncetion failed\n", __func__);
		return -1;
	}
	res = create_slaves();
	if (res) {
		printf("%s: Failed to create and subscribe \
		       test threads.\n", __func__);
		return -1;
	}
	/* fault server can handle 5 clients at maximum */
	if (test_1())
		goto stop_test;
	/* Report fault and check distribute */
	if (test_2())
		goto stop_test;
	usleep(50000);
	/* Clear all faults */
	if (test_3())
		goto stop_test;
	/* Parameters parsing */
	if (test_4())
		goto stop_test;
	printf("\n*** All tests have passed ***\n");
stop_test:
	printf("Closing down slaves\n");
	if(0 == XPAI_UnsubscribeFaults(main_mbox))
	printf("XPAI_UnsubscribeFaults return 0.\n");
	close_slaves();

	return 0;
}

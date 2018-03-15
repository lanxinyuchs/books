/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <pthread.h>
#include <itc.h>
#include "a4ci.h"
#include "test.h"


#define PASSED  					(1)
#define FAILED  					(0)


union itc_msg {
	uint32_t					msgno;
	struct test_done			tdone;				
};


/* This message requests the PP to return the HW PID */
#define CC_HW_PID_REQ 			0x08
#define CC_HW_PID_CFM 			0x09
#define CC_HW_PID_REJ 			0x0A


#define TIMEOUTMS1 				(100) /* for scanning devices */



#define SCU  					0x20
#define SAU  					0x21
#define CLU  					0x2C
#define UNKNOWN_DEV				0xff


/* SCU */
#define SCU_GET_STATUS_REQ		0xCC
#define SCU_GET_STATUS_CFM		0xCD
#define SCU_GET_STATUS_REJ		0xCE

/* SAU */
#define SAU_GET_STATUS_REQ		0xEC
#define SAU_GET_STATUS_CFM		0xED
#define SAU_GET_STATUS_REJ		0xEE

/* BFU */
#define BFU2_GET_STATUS_REQ		0x7C
#define BFU2_GET_STATUS_CFM		0x7D
#define BFU2_GET_STATUS_REJ		0x7E

/* PSU */
#define PSU_GET_STATUS_REQ		0x4C
#define PSU_GET_STATUS_CFM		0x4D
#define PSU_GET_STATUS_REJ		0x4E

/* PDU */
#define PDU_GET_STATUS_REQ		0xAC
#define PDU_GET_STATUS_CFM		0xAD
#define PDU_GET_STATUS_REJ		0xAE

/* PDU2 */
#define PDU2_GET_STATUS_REQ		0x30
#define PDU2_GET_STATUS_CFM		0x31
#define PDU2_GET_STATUS_REJ		0x32

/* CLU */
#define CLU_GET_STATUS_REQ		0x8C
#define CLU_GET_STATUS_CFM		0x8D
#define CLU_GET_STATUS_REJ		0x8E

#define CLIENT_IDENTIFIER 		0x00

#define TIMEOUTMS 				(25)


static uint8_t do_test(uint8_t base_addr, uint16_t port, uint32_t count)
{
	uint32_t i, failc = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result, client_id;
	int client_xor;
	struct timespec ts = {0,5000000};

	clock_gettime(CLOCK_REALTIME, &ts);
	client_xor = (int)ts.tv_nsec;

	ts.tv_sec = 0;
	ts.tv_nsec = 5000000;

	for (i = 0; i < count; i++) {
		client_id = (int)(rand() ^ client_xor);
		data[0] = CC_HW_PID_REQ;
		data[1] = client_id;
		res = a4ciDataReq(0, port, base_addr, 2, &data[0], TIMEOUTMS, &result);
		failc++;

		if (res) {
			if ((i % 50) == 0) {
				if (i)
					LOG("HWPID successfully read %d times\n", i);
				else
					print_data(&res->data[0], res->size, 16);
			}

			if (result != A4CI_SERVER_SUCCESS) {
				LOG("Data returned but error = %d\n", result);
				print_data(&res->data[0], res->size, 16);

			} else if ((res->data[1] != client_id)) {
				LOG("Wrong transaction ID in reply %02X (exp. %02X)\n",
					res->data[1], client_id);
				print_data(&res->data[0], res->size, 16);

			} else if ((res->data[0] != CC_HW_PID_CFM)) {
				LOG("Wrong L3 mtype in reply %02X (exp. %02X)\n",
					res->data[1], CC_HW_PID_CFM);
				print_data(&res->data[0], res->size, 16);
			} else {
				failc = 0;
			}
		} else {
			switch (result) {
				case A4CI_SERVER_SUCCESS:
					LOG("No data returned, but result is SUCCESS\n");
					break;

				case A4CI_SERVER_TIME_OUT_NO_RESPONSE:
					LOG("A4CI Timeout\n");
					sleep(1);
					break;

				case A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE:
					LOG("A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE\n");
					break;

				case A4CI_SERVER_OTHER_ERROR:
					LOG("A4CI_SERVER_OTHER_ERROR\n");
				case A4CI_SERVER_FAILURE:
					LOG("A4CI_SERVER_FAILURE\n");
				case A4CI_SERVER_DRIVER_OPEN_ERROR:
					LOG("A4CI_SERVER_DRIVER_OPEN_ERROR\n");
				default:
					LOG("Unknown error code %d returned\n", result);
					break;
			}
		}

		if (res)
			free(res);
		if (failc == 5) {
			LOG("HWPID failure after %d transactions\n", i);
			return FAILED;
		}

		clock_nanosleep(CLOCK_MONOTONIC, 0, &ts, NULL);
	}

	LOG("DONE: HWPID read %d times\n", count);
	return PASSED;
}

static uint8_t check_scu_device(uint32_t device)
{
	uint8_t passed = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result;

	data[0] = SCU_GET_STATUS_REQ;
	data[1] = CLIENT_IDENTIFIER;

	res = a4ciDataReq(0, 1, device, 2, &data[0], TIMEOUTMS1, &result);
	if(res) {
		passed = ((	(res->data[0] == SCU_GET_STATUS_CFM) ||
					(res->data[0] == SCU_GET_STATUS_REJ)) &&
					(result == A4CI_SERVER_SUCCESS));
		free(res);
	}
	return passed;
}

static uint8_t check_sau_device(uint32_t device)
{
	uint8_t passed = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result;

	data[0] = SAU_GET_STATUS_REQ;
	data[1] = CLIENT_IDENTIFIER;

	res = a4ciDataReq(0,2,device,2,&data[0],TIMEOUTMS1,&result);

	if(res) {
		passed = ((	(res->data[0] == SAU_GET_STATUS_CFM) ||
					(res->data[0] == SAU_GET_STATUS_REJ)) &&
					(result == A4CI_SERVER_SUCCESS));
		free(res);
	}
	return passed;
}

static uint8_t check_psu_device(uint32_t device)
{
	uint8_t passed = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result;

	data[0] = PSU_GET_STATUS_REQ;
	data[1] = CLIENT_IDENTIFIER;

	res = a4ciDataReq(0, 1, device, 2, &data[0], TIMEOUTMS1, &result);

	if(res) {
		passed = ((	(res->data[0] == PSU_GET_STATUS_CFM) ||
					(res->data[0] == PSU_GET_STATUS_REJ)) &&
					(result == A4CI_SERVER_SUCCESS));
		free(res);
	}
	return passed;
}

static uint8_t check_bfu_device(uint32_t device) 
{
	uint8_t passed = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result;

	data[0] = BFU2_GET_STATUS_REQ;
	data[1] = CLIENT_IDENTIFIER;

	res = a4ciDataReq(0, 1, device, 2, &data[0], TIMEOUTMS1, &result);

	if(res) {
		passed = ((	(res->data[0] == BFU2_GET_STATUS_CFM) ||
					(res->data[0] == BFU2_GET_STATUS_REJ)) &&
					(result == A4CI_SERVER_SUCCESS));
		free(res);
	}
	return passed;
}

static uint8_t check_pdu_device(uint32_t device)
{
	uint8_t passed = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result;

	data[0] = PDU_GET_STATUS_REQ;
	data[1] = CLIENT_IDENTIFIER;

	res = a4ciDataReq(0, 1, device, 2, &data[0], TIMEOUTMS1, &result);

	if(res) {
		passed = ((	(res->data[0] == PDU_GET_STATUS_CFM) ||
					(res->data[0] == PDU_GET_STATUS_REJ)) &&
					(result == A4CI_SERVER_SUCCESS));
		free(res);
	}
	return passed;
}

static uint8_t get_device_baseaddress(char *type, uint16_t *port)
{
	uint8_t i;

	if (strcasecmp(type, "pdu") == 0 ){
		for (i = 0x50; i <= 0x5F; i++ ) {
			if(check_pdu_device(i))
				return i;
		}
	}

	if (strcasecmp(type, "bfu") == 0 ){
		for (i = 0x40; i <= 0x4F; i++ ) {
			if(check_bfu_device(i)) return i;
		}
	}

	if (strcasecmp(type, "scu") == 0 ){
		for (i = 0x60; i <= 0x66; i++ ) {
			if(check_scu_device(i)) return i;
		}
		if(check_scu_device(0x20)) return 0x20;
	}

	if (strcasecmp(type, "sau") == 0 ){
		*port = 2;
		if(check_sau_device(0x21)) return 0x21;
	}

	if (strcasecmp(type, "psu") == 0 ){
		for (i = 0x30; i <= 0x3F; i++ ) {
			if(check_psu_device(i)) return i;
		}
	}

	return UNKNOWN_DEV;
}

static uint8_t bounds_test()
{
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result, pass = FAILED;

	res = a4ciDataReq(0,1,0,1,&data[0],10,&result);
	if (res || (result != A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE)) {
		if (res)
			free(res);
		goto exit;
	}

	res = a4ciDataReq(0,1,255,1,&data[0],10,&result);
	if (res || (result != A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE)) {
		if (res)
			free(res);
		goto exit;
	}
	
	pass = PASSED;
	res = a4ciDataReq(0,1,20,79,&data[0],10,&result);
	if (res || (result != A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE)) {
		if (res)
			free(res);
		pass = FAILED;
	}

 exit:
	return pass;
}


void *g2_a4ci_thread(void *param)
{
	uint32_t select[2] = {1, TEST_DONE};
	struct thread_data *td = param;
	uint8_t addr = UNKNOWN_DEV;
	uint16_t port = 1;
	uint8_t pass = FAILED;
	union itc_msg *msg;
	itc_mbox_id_t me;

	td->result = 1;
	me = itc_create_mailbox("g2_a4ci_thread", 0);
	if (me == ITC_NO_ID) {
		LOG("created mbox failure");
		goto exit;
	}

	if (a4ciOpen() != A4CI_SERVER_SUCCESS) {
		LOG("a4ciOpen() failed\n");
		td->result = 2;
		goto exit;
	}

	pass = bounds_test();
	if (pass != PASSED) {
		LOG("bounds_test returns %d\n", pass);
		td->result = 3;
		goto exit;
	}

	msg = itc_receive(select, 15000, ITC_FROM_ALL);
	if (msg == NULL) {
		LOG("A4CI test timeout. Quitting...\n");
		td->result = 4;
		goto exit;
	}
	itc_send(&msg, itc_sender(msg), me);
	msg = itc_receive(select, 15000, ITC_FROM_ALL);
	if (msg == NULL) {
		LOG("A4CI test timeout. Quitting...\n");
		td->result = 5;
		goto exit;
	}
	itc_free(&msg);


	if (td->pp_type) {
		int j;
		for (j = 0; (addr == UNKNOWN_DEV) && (j < 5); j++) {
			addr = get_device_baseaddress(td->pp_type, &port);
			sleep(1);
		}
	}


	if (addr == UNKNOWN_DEV) {
		LOG("No device of type %s found\n", td->pp_type);
		td->result = 6;
		goto exit;
	}

	td->result = 0;
	pass = do_test(addr, port, td->count);
	if (pass != PASSED) {
		LOG("CC_HW_PID test failed\n");
		td->result = 6;
	}

 exit:
	itc_delete_mailbox(me);
	pthread_exit(NULL);
	return NULL;
}

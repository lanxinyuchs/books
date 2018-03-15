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


static void
print_data(uint8_t *data, uint32_t size, uint32_t noelem)
{
	char ascii[128];
	uint32_t k,j;
	for (k = 0, j = 0; k < size; k++, j++) {
		if (j == noelem) {
			ascii[j] = 0;
			printf("  \"%s\"\n", ascii);
			j = 0;
		}
		printf("%.2x ",data[k]);
		if (isprint((int)data[k]))
			ascii[j] = (char)data[k];
		else
			ascii[j] = '.';
	}
	if (j) {
		ascii[j] = 0;
		for (;j < noelem; j++)
			printf("   ");

		printf("  \"%s\"\n", ascii);
	}
	printf("\n");
}


static uint8_t do_test(uint8_t base_addr, uint32_t count)
{
	uint32_t i, j = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result, passed = PASSED;
	struct timespec ts = {0,5000000};

	for (i = 0; i < count; i++) {
		data[0] = CC_HW_PID_REQ;
		data[1] = CLIENT_IDENTIFIER;
		res = a4ciDataReq(0, 0, base_addr, 2, &data[0], TIMEOUTMS, &result);

		if (res) {
			if (((i + 1) % 20) == 0) {
				printf("HWPID successfully read %d times\n", i + 1);
				if (i == 19)
					print_data(&res->data[0], res->size, 16);
			}

			passed = (((res->data[0] == CC_HW_PID_CFM) || 
						(res->data[0] == CC_HW_PID_REJ)) && 
						(result == A4CI_SERVER_SUCCESS));

			free(res);
			j = 0;
		} else if (++j > 5) {
			printf("CC_HW_PID_REQ Test failed with return value %d\n",result);
			passed = FAILED;
		} else
			printf("++++++++++++++++++++++\n");

		if (!passed)
			return FAILED;

		clock_nanosleep(CLOCK_MONOTONIC, 0, &ts, NULL);
	}
	return passed;
}

static uint8_t check_scu_device(uint32_t device)
{
	uint8_t passed = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result;

	data[0] = SCU_GET_STATUS_REQ;
	data[1] = CLIENT_IDENTIFIER;

	res = a4ciDataReq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);
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

	res = a4ciDataReq(0,0,device,2,&data[0],TIMEOUTMS1,&result);

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

	res = a4ciDataReq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);

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

	res = a4ciDataReq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);

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

	res = a4ciDataReq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);

	if(res) {
		passed = ((	(res->data[0] == PDU_GET_STATUS_CFM) ||
					(res->data[0] == PDU_GET_STATUS_REJ)) &&
					(result == A4CI_SERVER_SUCCESS));
		free(res);
	}
	return passed;
}

static uint8_t get_device_baseaddress(char *type)
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

	res = a4ciDataReq(0,0,0,1,&data[0],10,&result);
	if (res || (result != A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE)) {
		if (res)
			free(res);
		goto exit;
	}

	res = a4ciDataReq(0,0,255,1,&data[0],10,&result);
	if (res || (result != A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE)) {
		if (res)
			free(res);
		goto exit;
	}
	
	pass = PASSED;
	res = a4ciDataReq(0,0,20,79,&data[0],10,&result);
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
	uint8_t pass = FAILED;
	union itc_msg *msg;
	itc_mbox_id_t me;

	td->result = 1;
	me = itc_create_mailbox("g2_a4ci_thread", 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure");
		goto exit;
	}

	if (a4ciOpen() != A4CI_SERVER_SUCCESS) {
		printf("a4ciOpen() failed\n");
		td->result = 2;
		goto exit;
	}

	pass = bounds_test();
	if (pass != PASSED) {
		printf("bounds_test returns %d\n", pass);
		td->result = 3;
		goto exit;
	}

	msg = itc_receive(select, 15000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("A4CI test timeout. Quitting...\n");
		td->result = 4;
		goto exit;
	}
	itc_send(&msg, itc_sender(msg), me);
	msg = itc_receive(select, 15000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("A4CI test timeout. Quitting...\n");
		td->result = 5;
		goto exit;
	}
	itc_free(&msg);


	if (td->pp_type)
		addr = get_device_baseaddress(td->pp_type);


	if (addr == UNKNOWN_DEV) {
		printf("No device of type %s found\n", td->pp_type);
		td->result = 6;
		goto exit;
	}

	td->result = 0;
	pass = do_test(addr, td->count);
	if (pass != PASSED) {
		printf("CC_HW_PID test failed\n");
		td->result = 6;
	}

 exit:
	itc_delete_mailbox(me);
	pthread_exit(NULL);
	return NULL;
}

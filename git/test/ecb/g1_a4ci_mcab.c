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
#include <arpa/inet.h>
#include <itc.h>
#include "g1_a4ci.sig"
#include "test.h"


#define PASSED  					(1)
#define FAILED  					(0)

#define A4CI_MAX_DATA_SIZE                   78
#define MAX_FRAME                           256
#define M_HDLC_ADDR                         254
#define L_HDLC_ADDR                           1


#define A4CI_SERVER_SUCCESS                                        0
#define A4CI_SERVER_TIME_OUT_NO_RESPONSE                           1
#define A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE                     2
#define A4CI_SERVER_OTHER_ERROR                                    3
#define A4CI_SERVER_FAILURE                                        4
#define A4CI_SERVER_DRIVER_OPEN_ERROR                              5

#define A4CI_MCAB_PROTOCOL_REV					2

struct a4ciRx
{
   uint16_t 	cabinet;
   uint16_t 	port;
   uint8_t  	hdlcA;
   uint32_t 	size;
   uint8_t  	data[1];
};


union itc_msg {
	uint32_t						msgno;
	struct test_done				tdone;
	struct a4ci_connEstablishReqS	conn_req;
	struct a4ci_connEstablishCfmS	conn_cfm;
	struct a4ci_connEstablishRejS	conn_rej;
	struct a4ci_dataFwdS			data_fwd;

	struct a4ci_data2ReqS			data_req;
	struct a4ci_data2CfmS			data_cfm;
	struct a4ci_data2RejS			data_rej;

	struct a4ci_linkStatReqS		link_req;
	struct a4ci_linkStatCfmS		link_cfm;
	struct a4ci_linkStatRejS		link_rej;
};


static itc_mbox_id_t server_mbox = ITC_NO_ID;


static uint64_t get_tick(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000ULL + ts.tv_nsec/1000000;
}


static 
union itc_msg *transact(union itc_msg *msg, uint32_t reply)
{
	uint32_t select[3];

	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	select[0] = 2;
	select[1] = reply;
	select[2] = reply + 1;

	msg = itc_receive(select, 3000, server_mbox);
	if (msg == NULL) {
		printf("transact timeout @%s\n", __FILE__);
	}
	return msg;

}

static uint8_t a4ci_open(void)
{
	union itc_msg *msg;
	uint8_t ret = A4CI_SERVER_OTHER_ERROR;

	msg = itc_alloc(sizeof(struct a4ci_connEstablishReqS),
						A4CI_CONN_ESTABLISH_REQ);

	msg->conn_req.protocolRev = htons(A4CI_MCAB_PROTOCOL_REV);
	msg = transact(msg, A4CI_CONN_ESTABLISH_CFM);

	if (msg) {
		if (msg->msgno == A4CI_CONN_ESTABLISH_CFM)
			ret = A4CI_SERVER_SUCCESS;

		itc_free(&msg);
	}
	return ret;
}

static uint8_t
a4ci_datafwd(uint16_t cabinet, uint16_t port, uint8_t hdlcAddr,
                 uint16_t length, const void *txData)
{
	union itc_msg *msg;

	if (server_mbox == ITC_NO_ID)
		return A4CI_SERVER_DRIVER_OPEN_ERROR;

	if (hdlcAddr < L_HDLC_ADDR || hdlcAddr > M_HDLC_ADDR ||
	    length > A4CI_MAX_DATA_SIZE) {
		return A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE;
	}

	msg = itc_alloc(sizeof(struct a4ci_dataFwdS), A4CI_DATA_FWD);
	msg->data_fwd.port = htons(port);
	msg->data_fwd.hdlcAddr = hdlcAddr;
	msg->data_fwd.length = htons(length);

	memcpy(msg->data_fwd.data, txData, length);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	return  A4CI_SERVER_SUCCESS;
}

static struct a4ciRx *
a4ci_datareq(uint16_t cabinet, uint16_t port, uint8_t hdlcAddr,
			uint16_t length, const void *txData,
			uint32_t timeout, uint8_t *result)
{
	union itc_msg *msg;
	struct a4ciRx *rxp;
	static uint32_t cref = 0x10111213;

	(void)cabinet;
	(void)timeout;

	if (result == NULL)
		return NULL;

	if (server_mbox == ITC_NO_ID) {
		*result = A4CI_SERVER_DRIVER_OPEN_ERROR;
		return NULL;
	}

	if ((hdlcAddr < L_HDLC_ADDR) || (hdlcAddr > M_HDLC_ADDR) ||
		(length > A4CI_MAX_DATA_SIZE)) {
		*result =  A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE;
		return NULL;
	}

	msg = itc_alloc(sizeof(struct a4ci_data2ReqS), A4CI_DATA2_REQ);
	msg->data_req.clientRef = ++cref;
	msg->data_req.port = htons(port);
	msg->data_req.hdlcAddr = hdlcAddr;
	msg->data_req.length = htons(length);

	memcpy(msg->data_req.data, txData, length);
	msg = transact(msg, A4CI_DATA2_CFM);
	*result = A4CI_SERVER_FAILURE;

	if (msg && (msg->msgno == A4CI_DATA2_CFM)) {

		if (msg->data_cfm.clientRef != cref) {
			printf("cref error\n");
			*result = A4CI_UNEXPECTED_PARAMETER_VALUE;
			return NULL;
		}
		length = ntohs(msg->data_cfm.length);
		rxp = malloc(sizeof(struct a4ciRx) + length);
		rxp->cabinet = cabinet;
		rxp->port = port;
		rxp->hdlcA = msg->data_cfm.hdlcAddr;
		rxp->size = (uint32_t)length;

		memcpy(rxp->data, msg->data_cfm.data, length);
		itc_free(&msg);
		*result = A4CI_SERVER_SUCCESS;
		return rxp;
	} else if (msg) {

		switch (msg->data_rej.errorCode) {
			case A4CI_UNEXPECTED_PARAMETER_VALUE:
				*result = A4CI_SERVER_UNEXPECTED_PARAMETER_VALUE;
				break;

			case A4CI_OTHER_ERROR:
				*result = A4CI_SERVER_OTHER_ERROR;
				break;

			case A4CI_TIME_OUT:
				*result = A4CI_SERVER_TIME_OUT_NO_RESPONSE;
				break;
	
			default:
				break;
		}
		itc_free(&msg);
	}
	return NULL;
}



/* This message requests the PP to enter the Starting state */
#define CC_RESET_FWD 			0x03
/* This message requests the PP to clear the start flag */
#define CC_CLEAR_START_FLAG_REQ 0x04
#define CC_CLEAR_START_FLAG_CFM 0x05


#define CC_FW_PID_REQ 			0x00
#define CC_FW_PID_CFM 			0x01
#define CC_FW_PID_REJ 			0x02

/* This message requests the PP to return the HW PID */
#define CC_HW_PID_REQ 			0x08
#define CC_HW_PID_CFM 			0x09
#define CC_HW_PID_REJ 			0x0A

/* This message is used by the MP to set the proper visual indication on the PP */
#define CC_VISUAL_INDICATION_REQ 0x0C
#define CC_VISUAL_INDICATION_CFM 0x0D
#define CC_VISUAL_INDICATION_REJ 0x0E

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


#define VI_RESET 				0x00
#define VI_NO_FAULT 			0x30
#define VI_PP_FAULT 			0x31
#define VI_MISSING_RESOURCE 	0x40
#define VI_MISSING_RESOURCE_END 0x41

#define HDLC_LINK_LOST_TIMEOUT 	(30)
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

static uint8_t do_test(uint8_t base_addr)
{
	uint8_t passed, test, i;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result;
	uint8_t rtries;
	uint64_t txtime;
	uint32_t elapse;

	for (i = 0; i < 2; i++) {
		/*CC_RESET_FWD*/
		passed = PASSED;
		data[0] = CC_RESET_FWD;
		data[1] = CLIENT_IDENTIFIER;
		test = a4ci_datafwd(0, 0, base_addr, 2, &data[0]);

		if (test != A4CI_SERVER_SUCCESS) {
			printf("\nCC_RESET_FWD test for %d FAILED\n\n",base_addr);
			return FAILED;
		}
		sleep(2);

		/* CC_HW_PID_REQ */
		rtries = 0;
		do {
			passed = PASSED;
			data[0] = CC_HW_PID_REQ;
			data[1] = CLIENT_IDENTIFIER;
			txtime = get_tick();
			res = a4ci_datareq(0, 0, base_addr, 2, &data[0], TIMEOUTMS, &result);

			if(res) {
				print_data(&res->data[0], res->size, 16);
				passed &= (((res->data[0] == CC_HW_PID_CFM) || 
							(res->data[0] == CC_HW_PID_REJ)) && 
							(result == A4CI_SERVER_SUCCESS));
				free(res);
				break;
			} else {
				elapse = (uint32_t)(get_tick() - txtime);
				printf("CC_HW_PID_REQ Test failed with return value %d\n",result);
				passed = FAILED;

				if (result == A4CI_SERVER_TIME_OUT_NO_RESPONSE)
				{
					printf("Result after time out of %dms\n"
						"configured timeout is %dms\n", elapse, TIMEOUTMS);
				}
				sleep(1);
			}
		} while (rtries++ < 3);

		if (!passed) {
			printf("CC_HW_PID_REQ test for %d FAILED\n", base_addr);
			printf("Link lost or Received Unexpected Data \n");
			return FAILED;
		}

		/* CC_VISUAL_INDICATION_REQ */
		rtries = 0;
		do {
			passed = PASSED;
			data[0] = CC_VISUAL_INDICATION_REQ;
			data[1] = CLIENT_IDENTIFIER;
			data[2] = VI_MISSING_RESOURCE_END;

			txtime = get_tick();
			res = a4ci_datareq(0, 0, base_addr, 3, &data[0], TIMEOUTMS, &result);

			if (res) {
				print_data(&res->data[0], res->size, 16);
				passed &= (((res->data[0] == CC_VISUAL_INDICATION_CFM) ||
							(res->data[0] == CC_VISUAL_INDICATION_REJ)) &&
							(result == A4CI_SERVER_SUCCESS));
				free(res);
				break;
			} else {
				elapse = (uint32_t)(get_tick() - txtime);
				printf("CC_VISUAL_INDICATION_REQ Test failed with return value %d\n",result);
				passed = FAILED;
				if (result == A4CI_SERVER_TIME_OUT_NO_RESPONSE)
					printf("Result after time out of %dms\n"
						"configured timeout is %dms\n", elapse, TIMEOUTMS);

				sleep(HDLC_LINK_LOST_TIMEOUT);
			}
		} while (rtries++ < 3);

		if (!passed) {
			printf("CC_VISUAL_INDICATION_REQ test for %d FAILED\n",base_addr);
			printf("Link lost or Received Unexpected Data\n");
			return FAILED;
		}

		/* CC_CLEAR_START_FLAG_REQ */
		rtries = 0;
		do {
			passed = PASSED;
			data[0] = CC_CLEAR_START_FLAG_REQ;
			data[1] = CLIENT_IDENTIFIER;

			txtime = get_tick();
			res = a4ci_datareq(0, 0, base_addr, 2, &data[0], TIMEOUTMS, &result);

			if (res) {
				print_data(&res->data[0], res->size, 16);
				passed = (((res->data[0] == CC_CLEAR_START_FLAG_CFM)) &&
							(result == A4CI_SERVER_SUCCESS));
				free(res);
				break;
			} else {
				elapse = (uint32_t)(get_tick() - txtime);
				printf("CC_CLEAR_START_FLAG_REQ Test failed with return value %d\n",result);
				passed = FAILED;
				if (result == A4CI_SERVER_TIME_OUT_NO_RESPONSE)
					printf("Result after time out of %u ms\n"
						"configured timeout is %u ms\n", elapse, TIMEOUTMS);

				sleep(HDLC_LINK_LOST_TIMEOUT);
			}
		} while (rtries++ < 3);

		if (!passed) {
			printf("CC_CLEAR_START_FLAG_REQ test for %d FAILED\n",base_addr);
			printf("Link lost or Received Unexpected Data \n");
			return FAILED;
		}
	}
	return passed;
}

static uint8_t do_test2(uint8_t base_addr, uint32_t count)
{
	uint32_t i, j = 0;
	uint8_t data[10] = {0};
	struct a4ciRx *res;
	uint8_t result, passed = PASSED;
	struct timespec ts = {0,5000000};

	for (i = 0; i < count; i++) {
		data[0] = CC_HW_PID_REQ;
		data[1] = CLIENT_IDENTIFIER;
		res = a4ci_datareq(0, 0, base_addr, 2, &data[0], TIMEOUTMS, &result);

		if (res) {
			if (((i + 1) % 20) == 0)
				printf("HWPID successfully read %d times\n", i + 1);

			passed = (((res->data[0] == CC_HW_PID_CFM) ||
						(res->data[0] == CC_HW_PID_REJ)) &&
						(result == A4CI_SERVER_SUCCESS));

			free(res);
			j = 0;
		} else if (++j > 5) {
			printf("CC_HW_PID_REQ Test failed with return value %d\n",result);
			passed = FAILED;
		} else
			printf("++++++++++ %s ++++++++++++\n", __FILE__);

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

	res = a4ci_datareq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);
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

	res = a4ci_datareq(0,0,device,2,&data[0],TIMEOUTMS1,&result);

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

	res = a4ci_datareq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);

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

	res = a4ci_datareq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);

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

	res = a4ci_datareq(0, 0, device, 2, &data[0], TIMEOUTMS1, &result);

	if(res) {
		print_data(res->data, res->size, 16);

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

static uint8_t dump_stats(void)
{
	static uint32_t select[2] = {1, A4CI_LINK_STAT_CFM};
	union itc_msg *msg;
	
	static uint32_t cref = 0x12345678;
	static uint32_t port = 0x9abc;

	msg = itc_alloc(sizeof(struct a4ci_linkStatReqS), A4CI_LINK_STAT_REQ);
	msg->link_req.clientRef = htonl(++cref);
	msg->link_req.port = htons(++port);
	itc_send(&msg, server_mbox, ITC_MY_MBOX);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("No A4CI_LINK_STAT_CFM received\n");
		return FAILED;
	}
	if (ntohl(msg->link_cfm.clientRef) != cref) {
		printf("Unexpected clientRef in LSTAT_CFM\n");
		return FAILED;
	}

	if (ntohs(msg->link_cfm.port) != port) {
		printf("Unexpected port in LSTAT_CFM\n");
		return FAILED;
	}

	if (msg->link_cfm.nrOfUartOverrunsIsValid == 0) {
		printf("ERROR - nrOfUartOverrunsIsValid: 0\n");
		return FAILED;
	}

	itc_free(&msg);
	return PASSED;
}

void *a4ci_mcab_thread(void *param)
{
	static uint32_t select[2] = {1, TEST_DONE};
	struct thread_data *td = param;
	union itc_msg *msg;
	uint8_t addr = UNKNOWN_DEV;
	uint8_t pass = FAILED;
	itc_mbox_id_t me;
	int ret = 0;

	me = itc_create_mailbox("a4ci_mcab_thread", 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure");
		ret = 1;
		goto exit;
	}

	server_mbox = hunt_peer("ctr_link/A4ciServer");
	a4ci_open();

	msg = itc_receive(select, 150000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("No DONE signal from a4ci_thread\n");
		ret = 2;
		goto exit;
	}

	pass = dump_stats();
	if (pass != PASSED) {
		ret = 3;
		goto exit;
	}

	if (td->pp_type)
		addr = get_device_baseaddress(td->pp_type);

	pass = dump_stats();
	if (pass != PASSED) {
		ret = 4;
		goto exit;
	}

	if (addr == UNKNOWN_DEV) {
		printf("No device of type %s found\n", td->pp_type);
		ret = 5;
		goto exit;
	}

	pass = do_test(addr);
	if (pass != PASSED) {
		printf("CC_RESET test failed\n");
		ret = 6;
		goto exit;
	}

	pass = dump_stats();
	if (pass != PASSED) {
		ret = 7;
		goto exit;
	}

	itc_send(&msg, itc_sender(msg), me);
	msg = itc_receive(select, 15000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("No DONE from a4ci_thread\n");
		ret = 8;
		goto exit;
	}
	itc_free(&msg);
	printf("Doing CC_HWPID test. Number of iterations: %d\n", td->count);

	pass = do_test2(addr, td->count);
	if (pass != PASSED) {
		printf("CC_HW_PID test failed\n");
		ret = 9;
	}

	pass = dump_stats();
	if (pass != PASSED) {
		ret = 10;
		goto exit;
	}

 exit:
	td->result = ret;
	itc_delete_mailbox(me);
	pthread_exit(NULL);
	return NULL;
}

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>

#include <itc.h>

#include "atfi.h"
#include "atfi.sig"

#define  MAILBOX_SIZE 16
#define  TEST_MUX_CLIENT "mux_test_client"
#define  ATF_SERVER      "XXP_0"
#define  TEST_RECEIVE_TMO   2000
static itc_mbox_id_t  cmbox = ITC_NO_ID;


union itc_msg {
	uint32_t                    msgNo;
#include "atfiUnionContent.h"
};

enum tc_result {
	TC_PASSED = 0,
	TC_FAILED = 1
};

static int initItc();
enum tc_result test_reset(void);
enum tc_result test_aisg_scan(void);
enum tc_result test_du_sec_connect_disconnect(void);
enum tc_result test_au2_connect_disconnect(void);
static void send_reset(itc_mbox_id_t spid, uint32_t lnh);
static void send_con_est(itc_mbox_id_t spid, uint32_t lnh, uint16_t pv);
static void send_aisg_scan(itc_mbox_id_t spid, uint32_t lnh, uint16_t bdrt,
                           uint16_t tmo, uint8_t mask_len, uint8_t mask_id[],
                           uint8_t hw_len, uint8_t hw_id[]);
static void send_con_map(itc_mbox_id_t spid,
                         uint32_t lnh,
                         uint16_t la,
                         uint16_t pa);
static void send_connect(itc_mbox_id_t spid, uint32_t lnh, uint16_t la, uint16_t type);
static void
send_disconnect(itc_mbox_id_t spid, uint32_t lnh, uint16_t la, uint16_t type);


#define CHECK_SIG_RETURN_ON_ERROR(sig_exp, sig_rec, str)\
	do{if (sig_exp != sig_rec){\
	                fprintf(stderr, str);\
	                fprintf(stderr, "received msgno %u", sig_rec);\
	                fprintf(stderr, "\n");\
	                return TC_FAILED;\
	} }while(0)

static const char *help = {

	"Usage: test-mux -tc <test case nbr> \n\n"
		"    Where <test case nbr> is one of:\n"
		"    1:  test reset\n"
		"    2:  test aisg device scan\n"
		"    3:  test_du_sec_connect_disconnect\n"
		"    4:  test_au2_aisg_connect_disconnect\n"
		"***********************************************************\n"
		"NOTE: test for now use test mux library which checks hardcoded "
		"values so the same values needs to be used with this \n"
		"client and also the server, meaning that atf server should be \n"
		"started like: \n"
		" \"/usr/bin/atfid -d /dev/ttyS2 -s XXP_0 -m libtest-atfmi-mux.so /sys/atf/anp-profile-standard 0\""
		"before execution of TC, also bare in mind that test library \n"
		"libtest-atfmi-mux.so needs to be available in /usr/lib \n"
};

static itc_mbox_id_t hunt(char *name)
{
	uint32_t       sel[] = { 1, ITC_LOCATE_DEFAULT_NO };
	itc_mbox_id_t  pid;
	union itc_msg *sig;

	itc_locate_async(name, NULL, ITC_MY_MBOX);
	sig = itc_receive(sel, 5000, ITC_FROM_ALL);
	if (sig) {
		pid = itc_sender(sig);
		itc_free(&sig);
	} else {
		pid = 0;
	}

	return pid;
}


static int initItc()
{
	int res;

	res = itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if (res) {
		printf("\n Unable to inizalize ITC!, reason: ");
		switch (res) {
			case ITC_EINTERNAL_ERROR:
				printf("ITC_EINTERNAL_ERROR\n");
				break;
			case ITC_EALREADY_INITIALISED:
				printf("ITC_EALREADY_INITIALISED\n");
				break;
			case ITC_ENS_TO_LONG:
				printf("ITC_ENS_TO_LONG\n");
				break;
			case ITC_EOUT_OF_MEMORY:
				printf("ITC_EOUT_OF_MEMORY\n");
				break;
			case ITC_ENO_WORLD:
				printf("ITC_ENO_WORLD\n");
				break;
			case ITC_EILLEGAL_ALLOC_CFG:
				printf("ITC_EILLEGAL_ALLOC_CFG\n");
				break;
			default:
				printf("<unknown error %d> \n", res);
				break;
		}
		return -1;
	}

	cmbox = itc_create_mailbox(TEST_MUX_CLIENT, 0);
	if (cmbox == ITC_NO_ID) {
		printf("Could not create client mailbox \"%s\"\n",
		       TEST_MUX_CLIENT);
		return -1;
	}
	return 0;
}

static void send_con_est(itc_mbox_id_t spid, uint32_t lnh, uint16_t pv)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiConnEstablishReqS),
			ATFI_CONN_ESTABLISH_REQ);
	sig->atfiConnEstablishReq.addrInfo.linkHandle = htonl(lnh);
	sig->atfiConnEstablishReq.protocolRev = htons(pv);

	itc_send(&sig, spid, cmbox);
}

static void send_reset(itc_mbox_id_t spid, uint32_t lnh)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiResetReqS),
			ATFI_RESET_REQ);

	sig->atfiResetReq.addrInfo.linkHandle = htonl(lnh);

	itc_send(&sig, spid, cmbox);

}

static void send_aisg_scan(itc_mbox_id_t spid, uint32_t lnh, uint16_t bdrt,
                           uint16_t tmo, uint8_t mask_len, uint8_t mask_id[],
                           uint8_t hw_len, uint8_t hw_id[])
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiAisgDeviceScanReqS),
	                ATFI_AISG_DEVICE_SCAN_REQ);
	memset(sig->atfiAisgDeviceScanReq.uniqueHwIdMask, 0,
	ATFI_MAX_UNIQUE_HW_ID_LENGTH);
	memset(sig->atfiAisgDeviceScanReq.uniqueHwId, 0,
	ATFI_MAX_UNIQUE_HW_ID_LENGTH);

	printf("lnh %u, bdrt %u tmo %u mask_len %u\n", lnh, bdrt, tmo, mask_len);

	sig->atfiAisgDeviceScanReq.addrInfo.linkHandle = htonl(lnh);

	sig->atfiAisgDeviceScanReq.baudrate = htons(bdrt);

	sig->atfiAisgDeviceScanReq.timeout =  htons(tmo);

	sig->atfiAisgDeviceScanReq.uniqueHwIdMaskLength = htons(mask_len);
	sig->atfiAisgDeviceScanReq.uniqueHwIdMask[0] = htons(mask_id[0]);
	printf("mask_id[0] = %u\n", mask_id[0]);

	sig->atfiAisgDeviceScanReq.uniqueHwIdLength = htons(hw_len);
	sig->atfiAisgDeviceScanReq.uniqueHwId[0] = htons(hw_id[0]);
	printf("hw_id[0] = %u\n", hw_id[0]);

	itc_send(&sig, spid, cmbox);
}

static void send_con_map(itc_mbox_id_t spid,
                         uint32_t lnh,
                         uint16_t la,
                         uint16_t pa)
{

	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiAddConnMapReqS),
	                ATFI_ADD_CONN_MAP_REQ);

	sig->atfiAddConnMapReq.physicalAddress = htons(pa);
	sig->atfiAddConnMapReq.logicalAddress = htons(la);

	sig->atfiAddConnMapReq.addrInfo.linkHandle = htonl(lnh);

	itc_send(&sig, spid, cmbox);
}

static void send_connect(itc_mbox_id_t spid, uint32_t lnh, uint16_t la, uint16_t type)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_CONNECT2_REQ);

	sig->atfiConnect2Req.addrInfo.linkHandle = htonl(lnh);
	sig->atfiConnect2Req.logicalAddress = htons(la);
	sig->atfiConnect2Req.typeOfUnit = htons(type);

	itc_send(&sig, spid, cmbox);

}


static void
send_disconnect(itc_mbox_id_t spid, uint32_t lnh, uint16_t la, uint16_t type)
{
	union itc_msg *sig;
	sig = itc_alloc(sizeof(struct atfiDisconnectReqS), ATFI_DISCONNECT_REQ);

	sig->atfiConnect2Req.addrInfo.linkHandle = htonl(lnh);
	sig->atfiConnect2Req.logicalAddress = htons(la);
	sig->atfiConnect2Req.typeOfUnit = htons(type);

	itc_send(&sig, spid, cmbox);
}

enum tc_result test_reset(void)
{
	itc_mbox_id_t spid = 0;
	union itc_msg *rec_msg = NULL;
	uint32_t msgno;

	uint16_t pv = 3;
	uint32_t lnh = 0;

	spid = hunt(ATF_SERVER);
	if (spid == 0) {
		printf("Server \"%s\" does not exist\n", ATF_SERVER);
		return 1;
	}

	send_con_est(spid, lnh, pv);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_CONN_ESTABLISH_CFM,
	                          msgno,
	                          "ATFI_CONN_ESTABLISH_CFM not received");

	send_reset(spid, lnh);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_RESET_CFM, msgno,
	                          "ATFI_RESET_CFM not received");

	printf("test_reset PASSED\n");
	return TC_PASSED;
}

enum tc_result test_aisg_scan(void)
{
	itc_mbox_id_t spid = 0;
	union itc_msg *rec_msg = NULL;
	uint32_t msgno;

	uint8_t msk_len = 1, hw_len = 1;
	uint16_t bdrt = 1, tmo = 100, pv = 3;
	uint32_t lnh = 0;

	uint8_t mask_id[1] = {0x77};
	uint8_t hw_id[1] = {0xAA};

	spid = hunt(ATF_SERVER);
	if (spid == 0) {
		printf("Server \"%s\" does not exist\n", ATF_SERVER);
		return 1;
	}

	send_con_est(spid, lnh, pv);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_CONN_ESTABLISH_CFM,
	                          msgno,
	                          "ATFI_CONN_ESTABLISH_CFM not received");

	send_aisg_scan(spid, lnh, bdrt, tmo, msk_len, mask_id, hw_len, hw_id);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_AISG_DEVICE_SCAN_CFM,
	                          msgno,
	                          "ATFI_AISG_DEVICE_SCAN_CFM not received");

	printf("test_aisg_scan PASSED\n");
	return TC_PASSED;

}


enum tc_result test_du_sec_connect_disconnect(void)
{

	itc_mbox_id_t spid = 0;
	union itc_msg *rec_msg = NULL;
	uint32_t msgno;

	uint16_t pv = 3, la = 1, pa = 1, unit_type = 5;
	uint32_t lnh = 0;

	spid = hunt(ATF_SERVER);
	if (spid == 0) {
		printf("Server \"%s\" does not exist\n", ATF_SERVER);
		return 1;
	}

	send_con_est(spid, lnh, pv);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_CONN_ESTABLISH_CFM,
	                          msgno,
	                          "ATFI_CONN_ESTABLISH_CFM not received");

	send_con_map(spid, lnh, la, pa);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_ADD_CONN_MAP_CFM,
	                          msgno,
	                          "ATFI_ADD_CONN_MAP_CFM not received");

	send_connect(spid, lnh, la, unit_type);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_CONNECT2_CFM,
	                          msgno,
	                          "ATFI_CONNECT2_CFM not received");

	send_disconnect(spid, lnh, la, unit_type);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_DISCONNECT_CFM,
	                          msgno,
	                          "ATFI_DISCONNECT_CFM not received");

	printf("test_du_sec_connect_disconnect PASSED\n");
	return TC_PASSED;
}

enum tc_result test_au2_connect_disconnect(void)
{
	itc_mbox_id_t spid = 0;
	union itc_msg *rec_msg = NULL;
	uint32_t msgno;

	uint16_t pv = 3, la = 1, pa = 1, unit_type = 1;
	uint32_t lnh = 0;

	spid = hunt(ATF_SERVER);
	if (spid == 0) {
		printf("Server \"%s\" does not exist\n", ATF_SERVER);
		return 1;
	}

	send_con_est(spid, lnh, pv);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_CONN_ESTABLISH_CFM,
	                          msgno,
	                          "ATFI_CONN_ESTABLISH_CFM not received");

	send_con_map(spid, lnh, la, pa);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_ADD_CONN_MAP_CFM,
	                          msgno,
	                          "ATFI_ADD_CONN_MAP_CFM not received");

	send_connect(spid, lnh, la, unit_type);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_CONNECT2_CFM,
	                          msgno,
	                          "ATFI_CONNECT2_CFM not received");

	send_disconnect(spid, lnh, la, unit_type);

	rec_msg = itc_receive(ITC_NOFILTER, TEST_RECEIVE_TMO, ITC_FROM_ALL);

	if (rec_msg) {
		msgno = rec_msg->msgNo;
		itc_free(&rec_msg);
	} else {
		printf("No response from %s within %dms\n",
		       ATF_SERVER,
		       TEST_RECEIVE_TMO);
		return -1;
	}

	CHECK_SIG_RETURN_ON_ERROR(ATFI_DISCONNECT_CFM,
	                          msgno,
	                          "ATFI_DISCONNECT_CFM not received");

	printf("test_au2_aisg_connect_disconnect PASSED\n");
	return TC_PASSED;
}

int main(int argc, char *argv[])
{
	uint32_t ret = 1;

	if (initItc())
		goto err;

	if (argc < 3 || strcmp(argv[1], "-tc") != 0)
		goto cmd_fail;

	if (strcmp(argv[2], "1") == 0) {
		ret = test_reset();
		goto cmd_ok;
	} else if (strcmp(argv[2], "2") == 0) {
		ret = test_aisg_scan();
		goto cmd_ok;
	} else if (strcmp(argv[2], "3") == 0) {
		ret = test_du_sec_connect_disconnect();
		goto cmd_ok;
	} else if (strcmp(argv[2], "4") == 0) {
		ret = test_au2_connect_disconnect();
		goto cmd_ok;
	}

cmd_fail:
	printf(help);
err:
	itc_delete_mailbox(cmbox);
	return 1;
cmd_ok:
	itc_delete_mailbox(cmbox);
	return ret;
}

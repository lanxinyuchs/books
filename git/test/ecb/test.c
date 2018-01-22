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
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <itc.h>

#include "ecb-link-api.h"
#include "atfi.sig"
#include "test.h"

#define ATFI_MAX_NUMBER_OF_TEST_CLIENTS 	(ATFI_MAX_NUMBER_OF_CLIENTS     - 1)
#define ATFI_MAX_NUMBER_OF_TEST_CONNS		(ATFI_MAX_NUMBER_OF_CONNECTIONS - 1)


union itc_msg {
	uint32_t							msgno;
	struct ecb_link_event				lkind;
	struct test_done					tdone;

	struct atfiConnEstablishReqS		connEstablishReq;
	struct atfiConnEstablishCfmS		connEstablishCfm;
	struct atfiConnEstablishRejS		connEstablishRej;

	struct atfiAddConnMapReqS			addConnMapReq;
	struct atfiAddConnMapCfmS			addConnMapCfm;
	struct atfiAddConnMapRejS			addConnMapRej;

	struct atfiAddAu3MapReqS			atfiAddAu3MapReq;
	struct atfiAddAu3MapCfmS			atfiAddAu3MapCfm;
	struct atfiAddAu3MapRejS			atfiAddAu3MapRej;

	struct atfiRemoveConnMapReqS		removeConnMapReq;
	struct atfiRemoveConnMapCfmS		removeConnMapCfm;
	struct atfiRemoveConnMapRejS		removeConnMapRej;

	struct atfiConnect2ReqS				connect2Req;
	struct atfiConnect2CfmS				connect2Cfm;
	struct atfiConnect2RejS				connect2Rej;
	struct atfiConnect2IndS				connect2Ind;

	struct atfiDisconnectReqS			disconnectReq;
	struct atfiDisconnectCfmS			disconnectCfm;
	struct atfiDisconnectRejS			disconnectRej;
	struct atfiDisconnectIndS			disconnectInd;

	struct atfiResetReqS				atfiResetReq;
	struct atfiResetCfmS				atfiResetCfm;
	struct atfiResetRejS				atfiResetRej;

	struct atfiAuditReqS				auditReq;
	struct atfiAuditCfmS				auditCfm;
	struct atfiAuditRejS				auditRej;

	struct atfiGetAu3PortReqS			atfiGetAu3PortReq;
	struct atfiGetAu3PortCfmS			atfiGetAu3PortCfm;
	struct atfiGetAu3PortRejS			atfiGetAu3PortRej;

	struct atfiGetAisgUniqueIdReqS		atfiGetAisgUniqueIdReq;
	struct atfiGetAisgUniqueIdCfmS		atfiGetAisgUniqueIdCfm;
	struct atfiGetAisgUniqueIdRejS		atfiGetAisgUniqueIdRej;

	struct atfiAddAisgMap2ReqS			atfiAddAisgMap2Req;
	struct atfiAddAisgMapCfmS			atfiAddAisgMapCfm;
	struct atfiAddAisgMapRejS			atfiAddAisgMapRej;

	struct atfiAddCpriMap2ReqS			atfiAddCpriMap2Req;
	struct atfiAddCpriMapCfmS			atfiAddCpriMapCfm;
	struct atfiAddCpriMapRejS			atfiAddCpriMapRej;

	struct atfiGetPhyEndpointReqS		atfiGetPhyEndpointReq;
	struct atfiGetPhyEndpointCfmS		atfiGetPhyEndpointCfm;
	struct atfiGetPhyEndpointRejS		atfiGetPhyEndpointRej;

	struct atfiSetupIdcpA3ReqS			atfiSetupIdcpA3Req;
	struct atfiSetupIdcpA3CfmS			atfiSetupIdcpA3Cfm;
	struct atfiSetupIdcpA3RejS			atfiSetupIdcpA3Rej;

	struct atfiSetupIdcpB3ReqS			atfiSetupIdcpB3Req;
	struct atfiSetupIdcpB3CfmS			atfiSetupIdcpB3Cfm;
	struct atfiSetupIdcpB3RejS			atfiSetupIdcpB3Rej;

	struct atfiReleaseIdcpAReqS			atfiReleaseIdcpAReq;
	struct atfiReleaseIdcpACfmS			atfiReleaseIdcpACfm;
	struct atfiReleaseIdcpARejS			atfiReleaseIdcpARej;

	struct atfiReleaseIdcpBReqS			atfiReleaseIdcpBReq;
	struct atfiReleaseIdcpBCfmS			atfiReleaseIdcpBCfm;
	struct atfiReleaseIdcpBRejS			atfiReleaseIdcpBRej;

	struct atfiAisgDeviceScanReqS		atfiAisgDeviceScanReq;
	struct atfiAisgDeviceScanCfmS		atfiAisgDeviceScanCfm;
	struct atfiAisgDeviceScanRejS		atfiAisgDeviceScanRej;
};

itc_mbox_id_t hunt_peer(char *peer_name)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	union itc_msg *msg;
	itc_mbox_id_t mbox;

	itc_locate_async(peer_name, NULL, ITC_MY_MBOX);
	msg = itc_receive(select, ITC_NO_TMO, ITC_FROM_ALL);
	mbox = itc_sender(msg);
	itc_free(&msg);	
	return mbox;
}

static void dump_error(uint32_t msgno)
{
	uint32_t select[2];
	union itc_msg *msg;

	select[0] = 1;
	select[1] = msgno;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg) {
		printf("msgno 0x%x  error code: %d\n",
			msgno, ntohs(msg->disconnectRej.errorCode));
		itc_free(&msg);
	}
}

static int test_atfi_unsupported(itc_mbox_id_t atfi_mbox)
{
	uint32_t select[2];
	uint32_t linkhandle = 0xabadbabe;
	union itc_msg *msg;

	msg = itc_alloc(sizeof(struct atfiAddAu3MapReqS), ATFI_ADD_AU3_MAP_REQ);
	msg->atfiAddAu3MapReq.addrInfo.linkHandle = linkhandle;
	msg->atfiAddAu3MapReq.logicalAddress = htons(23);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_ADD_AU3_MAP_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 1;

	if (itc_sender(msg) != atfi_mbox)
		return 2;

	if (msg->atfiAddAu3MapRej.addrInfo.linkHandle != linkhandle)
		return 3;

	if (ntohs(msg->atfiAddAu3MapRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 4;

	itc_free(&msg);
	linkhandle++;


	msg = itc_alloc(sizeof(struct atfiGetAu3PortReqS), ATFI_GET_AU3_PORT_REQ);
	msg->atfiGetAu3PortReq.addrInfo.linkHandle = linkhandle;
	msg->atfiGetAu3PortReq.logicalAddress = htons(24);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_GET_AU3_PORT_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 5;

	if (itc_sender(msg) != atfi_mbox)
		return 6;

	if (msg->atfiGetAu3PortRej.addrInfo.linkHandle != linkhandle)
		return 7;

	if (ntohs(msg->atfiGetAu3PortRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 8;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiGetPhyEndpointReqS), ATFI_GET_PHY_ENDPOINT_REQ);
	msg->atfiGetAu3PortReq.addrInfo.linkHandle = linkhandle;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_GET_PHY_ENDPOINT_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 9;

	if (itc_sender(msg) != atfi_mbox)
		return 10;

	if (msg->atfiGetPhyEndpointRej.addrInfo.linkHandle != linkhandle)
		return 11;

	if (ntohs(msg->atfiGetPhyEndpointRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 12;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiGetPhyEndpointReqS), ATFI_GET_PHY_ENDPOINT_REQ);
	msg->atfiGetAu3PortReq.addrInfo.linkHandle = linkhandle;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_GET_PHY_ENDPOINT_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 13;

	if (itc_sender(msg) != atfi_mbox)
		return 14;

	if (msg->atfiGetPhyEndpointRej.addrInfo.linkHandle != linkhandle)
		return 15;

	if (ntohs(msg->atfiGetPhyEndpointRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 16;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiSetupIdcpA3ReqS), ATFI_SETUP_IDCP_A3_REQ);
	msg->atfiSetupIdcpA3Req.addrInfo.linkHandle = linkhandle;
	msg->atfiSetupIdcpA3Req.snidA = 10;
	msg->atfiSetupIdcpA3Req.snidB = 20;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_SETUP_IDCP_A3_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 17;

	if (itc_sender(msg) != atfi_mbox)
		return 18;

	if (msg->atfiSetupIdcpA3Rej.addrInfo.linkHandle != linkhandle)
		return 19;

	if (ntohs(msg->atfiSetupIdcpA3Rej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 20;

	itc_free(&msg);
	linkhandle++;




	msg = itc_alloc(sizeof(struct atfiSetupIdcpB3ReqS), ATFI_SETUP_IDCP_B3_REQ);
	msg->atfiSetupIdcpB3Req.addrInfo.linkHandle = linkhandle;
	msg->atfiSetupIdcpB3Req.snidA = 10;
	msg->atfiSetupIdcpB3Req.snidB = 20;
	msg->atfiSetupIdcpB3Req.cepIdB = 28;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_SETUP_IDCP_B3_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 21;

	if (itc_sender(msg) != atfi_mbox)
		return 22;

	if (msg->atfiSetupIdcpB3Rej.addrInfo.linkHandle != linkhandle)
		return 23;

	if (ntohs(msg->atfiSetupIdcpB3Rej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 24;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiReleaseIdcpAReqS), ATFI_RELEASE_IDCP_A_REQ);
	msg->atfiReleaseIdcpAReq.addrInfo.linkHandle = linkhandle;
	msg->atfiReleaseIdcpAReq.snidA = 10;
	msg->atfiReleaseIdcpAReq.snidB = 20;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_RELEASE_IDCP_A_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 25;

	if (itc_sender(msg) != atfi_mbox)
		return 26;

	if (msg->atfiReleaseIdcpARej.addrInfo.linkHandle != linkhandle)
		return 27;

	if (ntohs(msg->atfiReleaseIdcpARej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 28;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiReleaseIdcpBReqS), ATFI_RELEASE_IDCP_B_REQ);
	msg->atfiReleaseIdcpBReq.addrInfo.linkHandle = linkhandle;
	msg->atfiReleaseIdcpBReq.snidA = 10;
	msg->atfiReleaseIdcpBReq.snidB = 20;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_RELEASE_IDCP_B_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 29;

	if (itc_sender(msg) != atfi_mbox)
		return 30;

	if (msg->atfiReleaseIdcpBRej.addrInfo.linkHandle != linkhandle)
		return 31;

	if (ntohs(msg->atfiReleaseIdcpBRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 32;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiAddAisgMap2ReqS), ATFI_ADD_AISG_MAP2_REQ);
	msg->atfiAddAisgMap2Req.addrInfo.linkHandle = linkhandle;
	msg->atfiAddAisgMap2Req.logicalAddress = 50;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_ADD_AISG_MAP_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 33;

	if (itc_sender(msg) != atfi_mbox)
		return 34;

	if (msg->atfiAddAisgMapRej.addrInfo.linkHandle != linkhandle)
		return 35;

	if (ntohs(msg->atfiAddAisgMapRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 36;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiAddCpriMap2ReqS), ATFI_ADD_CPRI_MAP2_REQ);
	msg->atfiAddCpriMap2Req.addrInfo.linkHandle = linkhandle;
	msg->atfiAddCpriMap2Req.logicalAddress = 50;
	msg->atfiAddCpriMap2Req.cascadeNo = 2;
	msg->atfiAddCpriMap2Req.physicalAddress = 3;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_ADD_CPRI_MAP_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 37;

	if (itc_sender(msg) != atfi_mbox)
		return 38;

	if (msg->atfiAddCpriMapRej.addrInfo.linkHandle != linkhandle)
		return 30;

	if (ntohs(msg->atfiAddCpriMapRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 40;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiGetAisgUniqueIdReqS), ATFI_GET_AISG_UNIQUE_ID_REQ);
	msg->atfiGetAisgUniqueIdReq.addrInfo.linkHandle = linkhandle;
	msg->atfiGetAisgUniqueIdReq.logicalAddress = 50;
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[0] = 1;
	select[1] = ATFI_GET_AISG_UNIQUE_ID_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 41;

	if (itc_sender(msg) != atfi_mbox)
		return 42;

	if (msg->atfiGetAisgUniqueIdRej.addrInfo.linkHandle != linkhandle)
		return 43;

	if (ntohs(msg->atfiGetAisgUniqueIdRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 44;

	itc_free(&msg);
	linkhandle++;
	return 0;
}


struct client_data {
	pthread_mutex_t		*mtx;
	itc_mbox_id_t 		serv;
	pthread_t			thread;
	int					result;
};

static void *atfi_client(void *param)
{
	uint32_t select[3] = {2, ATFI_CONN_ESTABLISH_CFM, ATFI_CONN_ESTABLISH_REJ};
	struct client_data *cd = param;
	uint32_t linkhandle = (uint32_t)pthread_self();
	union itc_msg *msg;
	char mbox_name[32];
	itc_mbox_id_t me;

	sprintf(mbox_name, "client_%u", linkhandle);

	me = itc_create_mailbox(mbox_name, 0);
	if (me == ITC_NO_ID)
		goto done;

	msg = itc_alloc(sizeof(struct atfiConnEstablishReqS), ATFI_CONN_ESTABLISH_REQ);
	msg->connEstablishReq.addrInfo.linkHandle = linkhandle;
	msg->connEstablishReq.protocolRev = htons(ATFI_PROTOCOL_REV);
	itc_send(&msg, cd->serv, ITC_MY_MBOX);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	//cd->result = -1;

	if (msg == NULL) {
		printf("FAIL...sleeping.\n");
		sleep(100000);
		goto done;
	}

	if (msg->msgno == ATFI_CONN_ESTABLISH_REJ) {
		printf("ATFI_CONN_ESTABLISH_REJ: error - %u\n", 
			ntohs(msg->connEstablishRej.errorCode));
		sleep(100000);
	}

	if (msg->connEstablishCfm.addrInfo.linkHandle != linkhandle)
		goto done;

	itc_free(&msg);
	cd->result = 0;

 done:
	pthread_mutex_lock(cd->mtx);
	pthread_mutex_unlock(cd->mtx);
	pthread_exit(param);
	return NULL;
}

static int test_atfi_establish(itc_mbox_id_t atfi_mbox)
{
	uint32_t select[2] = {1, ATFI_CONN_ESTABLISH_REJ};
	struct client_data cdata[ATFI_MAX_NUMBER_OF_CLIENTS];
	uint32_t linkhandle = 0xabadbabe;
	pthread_mutex_t mtx;
	union itc_msg *msg;
	int count, cnt, ret = 0;

	msg = itc_alloc(sizeof(struct atfiConnEstablishReqS), ATFI_CONN_ESTABLISH_REQ);
	msg->connEstablishReq.addrInfo.linkHandle = linkhandle;
	msg->connEstablishReq.protocolRev = htons(ATFI_PROTOCOL_REV + 1);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 1;

	if (itc_sender(msg) != atfi_mbox)
		return 2;

	if (msg->connEstablishRej.addrInfo.linkHandle != linkhandle)
		return 3;

	if (ntohs(msg->connEstablishRej.errorCode) != ATFI_UNEXPECTED_PARAMETER_VALUE)
		return 4;


	itc_free(&msg);
	pthread_mutex_init(&mtx, NULL);

	for (count = 0; count < 3; count++) {
		struct timespec ts = { .tv_sec = 0, .tv_nsec = 100000};
		struct timespec t2 = { .tv_sec = 0, .tv_nsec = 50000000};
		pthread_mutex_lock(&mtx);

		for (cnt = 0; cnt < ATFI_MAX_NUMBER_OF_TEST_CLIENTS; cnt++) {
			cdata[cnt].mtx = &mtx;
			cdata[cnt].serv = atfi_mbox;
			cdata[cnt].result = (int)0xdeadbabe;

			if (pthread_create(&cdata[cnt].thread, 0, atfi_client, &cdata[cnt]))
				return 5;
		}

 wait_loop:
		clock_nanosleep(CLOCK_MONOTONIC, 0, &ts, NULL);

		for (cnt = 0; cnt < ATFI_MAX_NUMBER_OF_TEST_CLIENTS; cnt++)
			if (cdata[cnt].result == (int)0xdeadbabe)
				goto wait_loop;

		msg = itc_alloc(sizeof(struct atfiConnEstablishReqS), ATFI_CONN_ESTABLISH_REQ);
		msg->connEstablishReq.addrInfo.linkHandle = linkhandle;
		msg->connEstablishReq.protocolRev = htons(ATFI_PROTOCOL_REV);
		itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

		msg = itc_receive(select, 3000, ITC_FROM_ALL);

		if (msg == NULL)
			ret =  6;

		else if (itc_sender(msg) != atfi_mbox)
			ret =  7;

		else if (msg->connEstablishRej.addrInfo.linkHandle != linkhandle)
			ret = 8;

		else if (ntohs(msg->connEstablishRej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
			ret =  9;


		pthread_mutex_unlock(&mtx);

		for (cnt = 0; cnt < ATFI_MAX_NUMBER_OF_TEST_CLIENTS; cnt++) {
			if (pthread_join(cdata[cnt].thread, NULL))
				return 10;

			if (cdata[cnt].result)
				return 11;
		}

		if (ret)
			return ret;

		clock_nanosleep(CLOCK_MONOTONIC, 0, &t2, NULL);
	}

	msg = itc_alloc(sizeof(struct atfiConnEstablishReqS), ATFI_CONN_ESTABLISH_REQ);
	msg->connEstablishReq.addrInfo.linkHandle = linkhandle;
	msg->connEstablishReq.protocolRev = htons(ATFI_PROTOCOL_REV);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_CONN_ESTABLISH_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 12;

	if (itc_sender(msg) != atfi_mbox)
		return 13;

	if (msg->connEstablishCfm.addrInfo.linkHandle != linkhandle)
		return 14;

	itc_free(&msg);
	return 0;
}



static int test_atfi_add_conn(itc_mbox_id_t atfi_mbox)
{
	uint32_t select[2] = {1, ATFI_ADD_CONN_MAP_REJ};
	uint32_t linkhandle = ~0xabadbabe;
	union itc_msg *msg;
	uint16_t log;
	int cnt;


	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons(0);
	msg->addConnMapReq.physicalAddress = htons(0);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL)
		return 1;

	if (itc_sender(msg) != atfi_mbox)
		return 2;

	if (msg->addConnMapRej.addrInfo.linkHandle != linkhandle)
		return 3;

	if (ntohs(msg->addConnMapRej.errorCode) != ATFI_UNEXPECTED_PARAMETER_VALUE)
		return 4;

	itc_free(&msg);
	linkhandle ^= 0x12345678;


	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons(255);
	msg->addConnMapReq.physicalAddress = htons(255);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL)
		return 5;

	if (itc_sender(msg) != atfi_mbox)
		return 6;

	if (msg->addConnMapRej.addrInfo.linkHandle != linkhandle)
		return 7;

	if (ntohs(msg->addConnMapRej.errorCode) != ATFI_UNEXPECTED_PARAMETER_VALUE)
		return 8;

	itc_free(&msg);
	linkhandle ^= 0x9abcdef1;



	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons(1);
	msg->addConnMapReq.physicalAddress = htons(1);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_ADD_CONN_MAP_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 9;

	if (itc_sender(msg) != atfi_mbox)
		return 10;

	if (msg->addConnMapCfm.addrInfo.linkHandle != linkhandle)
		return 11;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons(1);
	msg->addConnMapReq.physicalAddress = htons(1);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_ADD_CONN_MAP_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 12;

	if (itc_sender(msg) != atfi_mbox)
		return 13;

	if (msg->addConnMapCfm.addrInfo.linkHandle != linkhandle)
		return 14;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons(1);
	msg->addConnMapReq.physicalAddress = htons(2);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_ADD_CONN_MAP_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 15;

	if (itc_sender(msg) != atfi_mbox)
		return 16;

	if (msg->addConnMapRej.addrInfo.linkHandle != linkhandle)
		return 17;

	if (ntohs(msg->addConnMapRej.errorCode) != ATFI_UNEXPECTED_PARAMETER_VALUE)
		return 18;

	itc_free(&msg);
	linkhandle++;


	select[1] = ATFI_ADD_CONN_MAP_CFM;

	for (log = 1, cnt = 0; cnt < ATFI_MAX_NUMBER_OF_TEST_CONNS; cnt++, log++) {
		msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
		msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
		msg->addConnMapReq.logicalAddress = htons(log);
		msg->addConnMapReq.physicalAddress = htons(log);
		itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

		msg = itc_receive(select, 3000, ITC_FROM_ALL);

		if (msg == NULL)
			return 19;

		if (itc_sender(msg) != atfi_mbox)
			return 20;

		if (msg->addConnMapCfm.addrInfo.linkHandle != linkhandle)
			return 21;

		itc_free(&msg);
		linkhandle++;
	}


	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons(log);
	msg->addConnMapReq.physicalAddress = htons(log);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_ADD_CONN_MAP_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 22;

	if (itc_sender(msg) != atfi_mbox)
		return 23;

	if (msg->addConnMapRej.addrInfo.linkHandle != linkhandle)
		return 24;

	if (ntohs(msg->addConnMapRej.errorCode) != ATFI_OTHER_ERROR)
		return 25;


	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiRemoveConnMapReqS), 
								ATFI_REMOVE_CONN_MAP_REQ);
	msg->removeConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->removeConnMapReq.logicalAddress = htons(log);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_REMOVE_CONN_MAP_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 26;

	if (itc_sender(msg) != atfi_mbox)
		return 27;

	if (msg->removeConnMapRej.addrInfo.linkHandle != linkhandle)
		return 28;

	if (ntohs(msg->addConnMapRej.errorCode) != ATFI_UNEXPECTED_PARAMETER_VALUE)
		return 29;


	itc_free(&msg);
	linkhandle++;

	select[1] = ATFI_REMOVE_CONN_MAP_CFM;

	for (log = 1, cnt = 0; cnt < ATFI_MAX_NUMBER_OF_TEST_CONNS; cnt++, log++) {
		msg = itc_alloc(sizeof(struct atfiRemoveConnMapReqS), 
									ATFI_REMOVE_CONN_MAP_REQ);
		msg->removeConnMapReq.addrInfo.linkHandle = linkhandle;
		msg->removeConnMapReq.logicalAddress = htons(log);
		itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

		msg = itc_receive(select, 3000, ITC_FROM_ALL);

		if (msg == NULL)
			return 30;

		if (itc_sender(msg) != atfi_mbox)
			return 31;

		if (msg->addConnMapCfm.addrInfo.linkHandle != linkhandle)
			return 32;

		itc_free(&msg);
		linkhandle++;
	}

	return 0;
}

static int test_atfi_connect(itc_mbox_id_t atfi_mbox)
{
	uint32_t select[2] = {1, ATFI_CONNECT2_REJ};
	uint32_t linkhandle = 0xabadbabe;
	union itc_msg *msg;


	msg = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_CONNECT2_REQ);
	msg->connect2Req.addrInfo.linkHandle = linkhandle;
	msg->connect2Req.logicalAddress = htons(1);
	msg->connect2Req.typeOfUnit = htons(ATFI_AU3);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_CONNECT2_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 1;

	if (itc_sender(msg) != atfi_mbox)
		return 2;

	if (msg->connect2Rej.addrInfo.linkHandle != linkhandle)
		return 3;

	if (ntohs(msg->connect2Rej.errorCode) != ATFI_UNSUPPORTED_CAPABILITY)
		return 4;


	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_CONNECT2_REQ);
	msg->connect2Req.addrInfo.linkHandle = linkhandle;
	msg->connect2Req.logicalAddress = htons(1);
	msg->connect2Req.typeOfUnit = htons(ATFI_DU_PRIMARY);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_CONNECT2_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 5;

	if (itc_sender(msg) != atfi_mbox)
		return 6;

	if (msg->connect2Rej.addrInfo.linkHandle != linkhandle)
		return 7;

	if (ntohs(msg->connect2Rej.errorCode) != ATFI_UNEXPECTED_PARAMETER_VALUE)
		return 8;


	itc_free(&msg);
	linkhandle++;
	

	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons(1);
	msg->addConnMapReq.physicalAddress = htons(1);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_ADD_CONN_MAP_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 9;

	if (itc_sender(msg) != atfi_mbox)
		return 10;

	if (msg->addConnMapCfm.addrInfo.linkHandle != linkhandle)
		return 11;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_CONNECT2_REQ);
	msg->connect2Req.addrInfo.linkHandle = linkhandle;
	msg->connect2Req.logicalAddress = htons(1);
	msg->connect2Req.typeOfUnit = htons(ATFI_DU_PRIMARY);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_CONNECT2_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 12;

	if (itc_sender(msg) != atfi_mbox)
		return 13;

	if (msg->connect2Cfm.addrInfo.linkHandle != linkhandle)
		return 14;

	if (ntohs(msg->connect2Cfm.state) != ATFI_DISCONNECTED)
		return 15;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_DISCONNECT_REQ);
	msg->disconnectReq.addrInfo.linkHandle = linkhandle;
	msg->disconnectReq.logicalAddress = htons(2);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_DISCONNECT_REJ;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 16;

	if (itc_sender(msg) != atfi_mbox)
		return 17;

	if (msg->disconnectRej.addrInfo.linkHandle != linkhandle)
		return 18;

	if (ntohs(msg->disconnectRej.errorCode) != ATFI_UNEXPECTED_PARAMETER_VALUE)
		return 19;

	itc_free(&msg);
	linkhandle++;



	msg = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_DISCONNECT_REQ);
	msg->disconnectReq.addrInfo.linkHandle = linkhandle;
	msg->disconnectReq.logicalAddress = htons(1);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_DISCONNECT_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 20;

	if (itc_sender(msg) != atfi_mbox)
		return 21;

	if (msg->disconnectRej.addrInfo.linkHandle != linkhandle)
		return 22;

	itc_free(&msg);
	linkhandle++;


	msg = itc_alloc(sizeof(struct atfiRemoveConnMapReqS), 
								ATFI_REMOVE_CONN_MAP_REQ);
	msg->removeConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->removeConnMapReq.logicalAddress = htons(1);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_REMOVE_CONN_MAP_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL) {
		dump_error(ATFI_REMOVE_CONN_MAP_REJ);
		return 23;
	}

	if (itc_sender(msg) != atfi_mbox)
		return 24;

	if (msg->removeConnMapCfm.addrInfo.linkHandle != linkhandle)
		return 25;


	itc_free(&msg);
	return 0;
}

static int test_atfi_spray(itc_mbox_id_t atfi_mbox, uint32_t addr, 
							uint32_t count, uint32_t size)
{
	uint32_t select[2] = {1, ATFI_CONNECT2_REJ};
	uint32_t linkhandle = 0xabadbabe;
	struct thread_data td = {0};
	union itc_msg *msg;
	pthread_t thread;

	msg = itc_alloc(sizeof(struct atfiAddConnMapReqS), ATFI_ADD_CONN_MAP_REQ);
	msg->addConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->addConnMapReq.logicalAddress = htons((uint16_t)addr);
	msg->addConnMapReq.physicalAddress = htons((uint16_t)addr);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_ADD_CONN_MAP_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 1;

	if (itc_sender(msg) != atfi_mbox)
		return 2;

	if (msg->addConnMapCfm.addrInfo.linkHandle != linkhandle)
		return 3;

	itc_free(&msg);
	linkhandle++;

	td.address = addr;
	td.count = count;
	td.size = size;
	td.result = 1;

	if (pthread_create(&thread, NULL, ping_thread, &td))
		return 4;

	msg = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_CONNECT2_REQ);
	msg->connect2Req.addrInfo.linkHandle = linkhandle;
	msg->connect2Req.logicalAddress = htons((uint16_t)addr);
	msg->connect2Req.typeOfUnit = htons(ATFI_DU_PRIMARY);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_CONNECT2_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 5;

	if (itc_sender(msg) != atfi_mbox)
		return 6;

	if (msg->connect2Cfm.addrInfo.linkHandle != linkhandle)
		return 7;

	if (ntohs(msg->connect2Cfm.state) != ATFI_DISCONNECTED)
		return 8;

	itc_free(&msg);
    //linkhandle++;

	select[1] = ATFI_CONNECT2_IND;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg) 
		itc_free(&msg);

	pthread_join(thread, NULL);
	if (td.result) {
		printf("ping_thread: %d\n", td.result);
		return 9;
	}

	select[1] = ATFI_DISCONNECT_IND;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 10;

	if (itc_sender(msg) != atfi_mbox)
		return 11;

	if (msg->disconnectInd.addrInfo.linkHandle != linkhandle)
		return 12;

	itc_free(&msg);
    linkhandle++;


	msg = itc_alloc(sizeof(struct atfiConnect2ReqS), ATFI_DISCONNECT_REQ);
	msg->disconnectReq.addrInfo.linkHandle = linkhandle;
	msg->disconnectReq.logicalAddress = htons((uint16_t)addr);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_DISCONNECT_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 13;

	if (itc_sender(msg) != atfi_mbox)
		return 14;

	if (msg->disconnectRej.addrInfo.linkHandle != linkhandle)
		return 15;


	itc_free(&msg);
    linkhandle++;

	msg = itc_alloc(sizeof(struct atfiRemoveConnMapReqS),
								ATFI_REMOVE_CONN_MAP_REQ);
	msg->removeConnMapReq.addrInfo.linkHandle = linkhandle;
	msg->removeConnMapReq.logicalAddress = htons((uint16_t)addr);
	itc_send(&msg, atfi_mbox, ITC_MY_MBOX);

	select[1] = ATFI_REMOVE_CONN_MAP_CFM;
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == NULL) {
		dump_error(ATFI_REMOVE_CONN_MAP_REJ);
		return 16;
	}

	if (itc_sender(msg) != atfi_mbox)
		return 17;

	if (msg->removeConnMapCfm.addrInfo.linkHandle != linkhandle)
		return 18;

	return 0;
}

void *ctrl_thread(void *param)
{
	uint32_t select[3] = {2, ECB_LINK_EVENT_MSG, TEST_DONE};
	struct ecb_link_config cfg;
	void *handle = NULL;
	union itc_msg *msg;
	itc_mbox_id_t me;
	uint32_t linkid = 0;
	int master = *(int*)param;
	int ret;

	me = itc_create_mailbox("test_ctrl", 0);
	if (me == ITC_NO_ID)
		pthread_exit(NULL);

	ret = ecb_link_init(&handle);
	if (ret) {
		printf("ecb_link_init, %d\n", ret);
		goto done;
	}

	cfg.address = 254;
	cfg.station = master ? ECB_STATION_PRIMARY : ECB_STATION_SECONDARY;
	strcpy(cfg.name, "ctr_link");

	ret = ecb_link_create(handle, &cfg, &linkid);
	if (ret) {
		printf("ecb_link_create(%s), %d\n", cfg.name, ret);
		goto done;
	}

	for (;;) {
		msg = itc_receive(select, ITC_NO_TMO, ITC_FROM_ALL);
		switch (msg->msgno) {
		case ECB_LINK_EVENT_MSG:
			if (msg->lkind.event == ECB_LINK_STATE_DOWN) {
				printf("ctr_link #%d lost.\n", msg->lkind.link);
				goto exit;
			}
			break;

		case TEST_DONE:
			itc_free(&msg);
			goto exit;
			break;

		default:
			break;
		}

		itc_free(&msg);
	}


 exit:
	ecb_link_destroy(handle, linkid);
	ecb_link_shutdown(handle);
	usleep(5000);
 done:
	itc_delete_mailbox(me);
	pthread_exit(NULL);
	return NULL;
}

static char help[] =
      "-a <HDLC address> -m <MAC address> [-p]\n"
      "\t-a <HDLC address>\n"
      "\t-m <MAC address>\n"
      "\t-s Max packet size in spray test\n"
      "\t-c Number of spray signals\n"
      "\t-i Number of A4CI transactions towards PP device\n"
      "\t-t Type of PP device to access (pdu bfu scu)\n"
      "\t-p This station is PRIMARY\n"
      "\t-h Print this help text\n";


int main(int argc, char *argv[]) 
{
	uint32_t select[3] = {2, TEST_DONE, ITC_MONITOR_DEFAULT_NO};
	struct thread_data td_a4c = {0};
	pthread_t g2a4ci, ctrlnh;
	uint32_t size = 70;
	uint32_t count = 1000;
	uint32_t addr = 4;
	uint32_t iter = 1000;
	itc_mbox_id_t mbox, peer, test_peer;
	union itc_msg *msg;
	char *pp_type = NULL;
	int passed = 0;
	int master = 0;
	int ret = 0;

	while ((ret = getopt(argc, argv, "ha:m:s:c:t:i:p")) != -1) {
		switch(ret) {
		case 'a':
			addr = atoi(optarg);
			break;
		case 'c':
			count = atoi(optarg);
			break;
		case 'i':
			iter = atoi(optarg);
			break;
		case 's':
			size = atoi(optarg);
			break;
		case 't':
			pp_type = optarg;
			break;
		case 'm':
			break;
		case 'p':
			master = 1;
			break;
		default:
			printf("%s %s\n", argv[0], help);
			exit(0);
			break;
		}
	}

	ret = itc_init(64, ITC_MALLOC, NULL, 0, 0);
	if (ret) {
		printf("itc_init failure: %d\n",ret);
		return passed;
	}

	mbox = itc_create_mailbox("atfi_test", 0);
	if (mbox == ITC_NO_ID) {
		printf("created mbox failure: %d\n",-errno);
		return passed;
	}

	if (pthread_create(&ctrlnh, NULL, ctrl_thread, &master)) {
		printf("Create thread error %d. Quitting...\n", -errno);
		itc_delete_mailbox(mbox);
		return passed;		
	}

	test_peer = hunt_peer("ctr_link/atfi_test");
	printf("Peer board found...0x%x\n", test_peer);
	itc_monitor(test_peer, NULL);

	if (master) {
		iter = 1000;
		count = 400;
	} else {
		iter = 100;
	}

	if (!master) {
		pthread_t pong, a4ci, mcab;
		struct thread_data td_pong = {.result = 1};
		struct thread_data td_a4ci = {	.result = 1, 
										.pp_type = pp_type, 
										.count = iter
									 };

		struct thread_data td_mcab = {	.result = 1, 
										.pp_type = pp_type, 
										.count = iter
									 };

		if (pthread_create(&pong, NULL, pong_thread, &td_pong))
			printf("Create thread error\n");

		if (pthread_create(&a4ci, NULL, a4ci_thread, &td_a4ci))
			printf("Create thread error\n");

		if (pthread_create(&mcab, NULL, a4ci_mcab_thread, &td_mcab))
			printf("Create thread error\n");

		pthread_join(pong, NULL);
		pthread_join(a4ci, NULL);
		pthread_join(mcab, NULL);
		passed = (td_pong.result << 16) | (td_a4ci.result << 8) | td_mcab.result;

		msg = itc_alloc(sizeof(struct test_done), TEST_DONE);
		msg->tdone.ecode = (uint32_t)passed;
		itc_send(&msg, test_peer, ITC_MY_MBOX);
		msg = itc_receive(select, 5000, ITC_FROM_ALL);
		if (msg)
			itc_free(&msg);
		
		goto exit;
	}

	peer = hunt_peer("ctr_link/MXP_0");
	printf("ATFI server found (0x%x)\n", peer);

	td_a4c.pp_type = pp_type;
	td_a4c.count = iter;

	if (pthread_create(&g2a4ci, NULL, g2_a4ci_thread, &td_a4c)) {
		printf("Create thread error %d. Quitting...\n", -errno);
		goto exit;
	}
	ret = test_atfi_unsupported(peer);

	if (ret) {
		printf("test_atfi_unsupported,  %d\n", ret);
		goto exit;
	}
	printf("test_atfi_unsupported...: SUCCESS\n");

	ret = test_atfi_establish(peer);
	if (ret) {
		printf("test_atfi_establish,  %d\n", ret);
		goto exit;
	}
	printf("test_atfi_establish...: SUCCESS\n");

	ret = test_atfi_add_conn(peer);
	if (ret) {
		printf("test_atfi_add_conn,  %d\n", ret);
		goto exit;
	}
	printf("test_atfi_add_conn...: SUCCESS\n");

	ret = test_atfi_connect(peer);
	if (ret) {
		printf("test_atfi_connect,  %d\n", ret);
		goto exit;
	}
	printf("test_atfi_connec...: SUCCESS\n");

	ret = test_atfi_spray(peer, addr, count, size);
	if (ret) {
		printf("test_atfi_spray,  %d\n", ret);
		goto exit;
	}
	printf("test_atfi_spray...: SUCCESS\n");

	pthread_join(g2a4ci, NULL);
	if (td_a4c.result) {
		printf("a4ci_thread: %d\n", td_a4c.result);
		goto exit;
	}

	msg = itc_receive(select, 5*60000, ITC_FROM_ALL);
	if (msg) {
		if (msg->msgno == TEST_DONE) {
			if (msg->tdone.ecode == 0)
				passed = 1;
			else
				printf("Error from secondary board: %d\n", msg->tdone.ecode);

			itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
		} else {
			printf("Secondary board lost.\n");
			itc_free(&msg);
		}
	} else 
		printf("No response from secondary board.\n");

 exit:

	peer = itc_locate("test_ctrl");
	if (peer != ITC_NO_ID) {
		msg = itc_alloc(sizeof(struct test_done), TEST_DONE);
		itc_send(&msg, peer, ITC_MY_MBOX);
		pthread_join(ctrlnh, NULL);
	}

	itc_delete_mailbox(mbox);
	sleep(1);

	if (master)
		printf("Primary QUIT (Test: %s)\n", passed ? "PASS" : "FAIL");
	else
		printf("Secondary QUIT (Test: %s)\n", passed ? "FAIL" : "PASS");

	itc_exit();
	return passed;
}

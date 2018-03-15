
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>

#include <itc.h>
#include <itc_system.h>

#include "atfi.h"
#include "atfi.sig"


union itc_msg {
	uint32_t                    msgno;
#include "atfiUnionContent.h"
};


static int atf_start(char *instance)
{
	itc_mbox_id_t client_mbox;
	char          name[64];

	setbuf(stdout, 0);
	itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	snprintf(name, sizeof(name), "ATF_Client_%s", instance);

	client_mbox = itc_create_mailbox(name, 0);
	if (client_mbox == ITC_NO_ID) {
		printf("Could not create client mailbox \"%s\"\n", name);
		return -1;
	}
	printf("Client \"%s\" (0x%x) successfully started\n",
	       name, client_mbox);

	printf("Press Control-C to exit\n");

	return 0;
}

static void atf_handle_message(void)
{
	uint32_t              all[] = { 0 };
	union itc_msg        *msg;
	struct itc_mbox_info *mbox_info;
	char                 *name;
	itc_mbox_id_t         mbox;

	msg = itc_receive(all, ITC_NO_TMO, ITC_FROM_ALL);
	mbox = itc_sender(msg);

	mbox_info = itc_get_mailbox_info(mbox);
	if (!mbox_info) {
		name = "UNKNOWN";
	} else {
		name = mbox_info->name;
	}

	switch (msg->msgno) {

		/*
		** CLIENT TO SERVER MSGNALS.
		*/

	case ATFI_CONN_ESTABLISH_REQ:
		printf("Sent ATFI_CONN_ESTABLISH_REQ(lh:%u,pr:%u) to %s",
		       ntohl(msg->atfiConnEstablishReq.addrInfo.linkHandle),
		       ntohs(msg->atfiConnEstablishReq.protocolRev),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_ADD_CONN_MAP_REQ:
		printf("Sent ATFI_ADD_CONN_MAP_REQ(lh:%u,la:%u,pa:%u) to %s",
		       ntohl(msg->atfiAddConnMapReq.addrInfo.linkHandle),
		       ntohs(msg->atfiAddConnMapReq.logicalAddress),
			     ntohs(msg->atfiAddConnMapReq.physicalAddress),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_REMOVE_CONN_MAP_REQ:
		printf("Sent ATFI_REMOVE_CONN_MAP_REQ(lh:%u,la:%u) to %s",
		       ntohl(msg->atfiRemoveConnMapReq.addrInfo.linkHandle),
		       ntohs(msg->atfiRemoveConnMapReq.logicalAddress),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_CONNECT2_REQ:
		printf("Sent ATFI_CONNECT2_REQ(lh:%u,la:%u,tu:%u) to %s",
		       ntohl(msg->atfiConnect2Req.addrInfo.linkHandle),
		       ntohs(msg->atfiConnect2Req.logicalAddress),
		       ntohs(msg->atfiConnect2Req.typeOfUnit),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_DISCONNECT_REQ:
		printf("Sent ATFI_DISCONNECT_REQ(lh:%u,la:%u) to %s",
		       ntohl(msg->atfiDisconnectReq.addrInfo.linkHandle),
		       ntohs(msg->atfiDisconnectReq.logicalAddress),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_RESET_REQ:
		printf("Sent ATFI_RESET_REQ(lh:%u) to %s",
		       ntohl(msg->atfiResetReq.addrInfo.linkHandle),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_AUDIT_REQ:
		printf("Sent ATFI_AUDIT_REQ(lh:%u,la:%u) to %s",
		       ntohl(msg->atfiAuditReq.addrInfo.linkHandle),
		       ntohs(msg->atfiAuditReq.logicalAddress),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_GET_AISG_UNIQUE_ID_REQ:
		printf("Sent ATFI_GET_AISG_UNIQUE_ID_REQ(lh:%u,la:%u) to %s",
		       ntohl(msg->atfiGetAisgUniqueIdReq.addrInfo.linkHandle),
		       ntohs(msg->atfiGetAisgUniqueIdReq.logicalAddress),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_AISG_DEVICE_SCAN_REQ:
		printf("Sent ATFI_AISG_DEVICE_SCAN_REQ("
		       "lh:%u,br:%u,to:%u,"
		       "ml:%u,m:%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
		       "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x,"
		       "ul:%u,u:%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
		       "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x"
		       ") to %s",
		       ntohl(msg->atfiAisgDeviceScanReq.addrInfo.linkHandle),
		       ntohs(msg->atfiAisgDeviceScanReq.baudrate),
		       ntohs(msg->atfiAisgDeviceScanReq.timeout),
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMaskLength,
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[0],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[1],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[2],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[3],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[4],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[5],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[6],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[7],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[8],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[9],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[10],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[11],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[12],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[13],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[14],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[15],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[16],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[17],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdMask[18],
		       msg->atfiAisgDeviceScanReq.uniqueHwIdLength,
		       msg->atfiAisgDeviceScanReq.uniqueHwId[0],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[1],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[2],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[3],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[4],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[5],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[6],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[7],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[8],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[9],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[10],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[11],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[12],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[13],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[14],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[15],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[16],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[17],
		       msg->atfiAisgDeviceScanReq.uniqueHwId[18],
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case ATFI_ADD_AISG_MAP2_REQ:
		printf("Sent ATFI_ADD_AISG_MAP2_REQ(lh:%u,la:%u,dt:%u,len:%u,id:"
		       "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
		       "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x) to %s",
		       ntohl(msg->atfiAddAisgMap2Req.addrInfo.linkHandle),
		       ntohs(msg->atfiAddAisgMap2Req.logicalAddress),
		       ntohs(msg->atfiAddAisgMap2Req.deviceType),
		       msg->atfiAddAisgMap2Req.uniqueHwIdLength,
		       msg->atfiAddAisgMap2Req.uniqueHwId[0],
		       msg->atfiAddAisgMap2Req.uniqueHwId[1],
		       msg->atfiAddAisgMap2Req.uniqueHwId[2],
		       msg->atfiAddAisgMap2Req.uniqueHwId[3],
		       msg->atfiAddAisgMap2Req.uniqueHwId[4],
		       msg->atfiAddAisgMap2Req.uniqueHwId[5],
		       msg->atfiAddAisgMap2Req.uniqueHwId[6],
		       msg->atfiAddAisgMap2Req.uniqueHwId[7],
		       msg->atfiAddAisgMap2Req.uniqueHwId[8],
		       msg->atfiAddAisgMap2Req.uniqueHwId[9],
		       msg->atfiAddAisgMap2Req.uniqueHwId[10],
		       msg->atfiAddAisgMap2Req.uniqueHwId[11],
		       msg->atfiAddAisgMap2Req.uniqueHwId[12],
		       msg->atfiAddAisgMap2Req.uniqueHwId[13],
		       msg->atfiAddAisgMap2Req.uniqueHwId[14],
		       msg->atfiAddAisgMap2Req.uniqueHwId[15],
		       msg->atfiAddAisgMap2Req.uniqueHwId[16],
		       msg->atfiAddAisgMap2Req.uniqueHwId[17],
		       msg->atfiAddAisgMap2Req.uniqueHwId[18],
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;


		/*
		** SERVER TO CLIENT MSGNALS.
		*/

	case ATFI_CONN_ESTABLISH_CFM:
		printf("Received ATFI_CONN_ESTABLISH_CFM(lh:%u) from %s",
		       ntohl(msg->atfiConnEstablishCfm.addrInfo.linkHandle),
		       name);
		break;

	case ATFI_CONN_ESTABLISH_REJ:
		printf("Received ATFI_CONN_ESTABLISH_REJ(lh:%u,ec:%u,pr:%u)"
		       " from %s",
		       ntohl(msg->atfiConnEstablishRej.addrInfo.linkHandle),
		       ntohs(msg->atfiConnEstablishRej.errorCode),
		       ntohs(msg->atfiConnEstablishRej.highestSupportedProtocolRev),
		       name);
		break;

	case ATFI_ADD_CONN_MAP_CFM:
		printf("Received ATFI_ADD_CONN_MAP_CFM(lh:%u) from %s",
		       ntohl(msg->atfiAddConnMapCfm.addrInfo.linkHandle),
		       name);
		break;

	case ATFI_ADD_CONN_MAP_REJ:
		printf("Received ATFI_ADD_CONN_MAP_REJ(lh:%u,ec:%u) from %s",
		       ntohl(msg->atfiAddConnMapRej.addrInfo.linkHandle),
		       ntohs(msg->atfiAddConnMapRej.errorCode),
		       name);
		break;

	case ATFI_REMOVE_CONN_MAP_CFM:
		printf("Received ATFI_REMOVE_CONN_MAP_CFM(lh:%u) from %s",
		       ntohl(msg->atfiRemoveConnMapCfm.addrInfo.linkHandle),
		       name);
		break;

	case ATFI_REMOVE_CONN_MAP_REJ:
		printf("Received ATFI_REMOVE_CONN_MAP_REJ(lh:%u,ec:%u) from %s",
		       ntohl(msg->atfiRemoveConnMapRej.addrInfo.linkHandle),
		       ntohs(msg->atfiRemoveConnMapRej.errorCode),
		       name);
		break;

	case ATFI_CONNECT2_CFM:
		printf("Received ATFI_CONNECT2_CFM(lh:%u,st:%u) from %s",
		       ntohl(msg->atfiConnect2Cfm.addrInfo.linkHandle),
		       ntohs(msg->atfiConnect2Cfm.state),
		       name);
		break;

	case ATFI_CONNECT2_REJ:
		printf("Received ATFI_CONNECT2_REJ(lh:%u,ec:%u) from %s",
		       ntohl(msg->atfiConnect2Rej.addrInfo.linkHandle),
		       ntohs(msg->atfiConnect2Rej.errorCode),
		       name);
		break;

	case ATFI_CONNECT2_IND:
		printf("Received ATFI_CONNECT2_IND(lh:%u,la:%u) from %s",
		       ntohl(msg->atfiConnect2Req.addrInfo.linkHandle),
		       ntohs(msg->atfiConnect2Req.logicalAddress),
		       name);
		break;

	case ATFI_DISCONNECT_CFM:
		printf("Received ATFI_DISCONNECT_CFM(lh:%u) from %s",
		       ntohl(msg->atfiDisconnectCfm.addrInfo.linkHandle),
		       name);
		break;

	case ATFI_DISCONNECT_REJ:
		printf("Received ATFI_DISCONNECT_REJ(lh:%u,ec:%u) from %s",
		       ntohl(msg->atfiDisconnectRej.addrInfo.linkHandle),
		       ntohs(msg->atfiDisconnectRej.errorCode),
		       name);
		break;

	case ATFI_DISCONNECT_IND:
		printf("Received ATFI_DISCONNECT_IND(lh:%u,la:%u) from %s",
		       ntohl(msg->atfiDisconnectInd.addrInfo.linkHandle),
		       ntohs(msg->atfiDisconnectInd.logicalAddress),
		       name);
		break;

	case ATFI_RESET_CFM:
		printf("Received ATFI_RESET_CFM(lh:%u) from %s",
		       ntohl(msg->atfiResetCfm.addrInfo.linkHandle),
		       name);
		break;

	case ATFI_RESET_REJ:
		printf("Received ATFI_RESET_REJ(lh:%u) from %s",
		       ntohl(msg->atfiResetRej.addrInfo.linkHandle),
		       name);
		break;

	case ATFI_AUDIT_CFM:
		printf("Received ATFI_AUDIT_CFM(lh:%u,st:%u) from %s",
		       ntohl(msg->atfiAuditCfm.addrInfo.linkHandle),
		       ntohs(msg->atfiAuditCfm.state),
		       name);
		break;

	case ATFI_AUDIT_REJ:
		printf("Received ATFI_AUDIT_REJ(lh:%u,ec:%u) from %s",
		       ntohl(msg->atfiAuditRej.addrInfo.linkHandle),
		       ntohs(msg->atfiAuditRej.errorCode),
		       name);
		break;

	case ATFI_GET_AISG_UNIQUE_ID_CFM:
		printf("Received ATFI_GET_AISG_UNIQUE_ID_CFM(lh:%u,len:%u,id:"
		       "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
		       "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x) from %s",
		       ntohl(msg->atfiGetAisgUniqueIdCfm.addrInfo.linkHandle),
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwIdLength,
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[0],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[1],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[2],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[3],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[4],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[5],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[6],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[7],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[8],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[9],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[10],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[11],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[12],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[13],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[14],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[15],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[16],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[17],
		       msg->atfiGetAisgUniqueIdCfm.uniqueHwId[18],
		       name);
		break;

	case ATFI_GET_AISG_UNIQUE_ID_REJ:
		printf("Received ATFI_GET_AISG_UNIQUE_ID_REJ(lh:%u,ec:%u) "
		       "from %s",
		       htonl(msg->atfiGetAisgUniqueIdRej.addrInfo.linkHandle),
		       htons(msg->atfiGetAisgUniqueIdRej.errorCode),
		       name);
		break;

	case ATFI_AISG_DEVICE_SCAN_CFM:
		printf("Received ATFI_AISG_DEVICE_SCAN_CFM(lh:%u,re:%u,dt:%u,"
		       "ul:%u,u:%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x."
		       "%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x.%02x) "
		       "from %s",
		       htonl(msg->atfiAisgDeviceScanCfm.addrInfo.linkHandle),
		       htons(msg->atfiAisgDeviceScanCfm.result),
		       htons(msg->atfiAisgDeviceScanCfm.deviceType),
		       msg->atfiAisgDeviceScanCfm.uniqueHwIdLength,
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[0],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[1],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[2],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[3],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[4],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[5],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[6],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[7],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[8],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[9],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[10],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[11],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[12],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[13],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[14],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[15],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[16],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[17],
		       msg->atfiAisgDeviceScanCfm.uniqueHwId[18],
		       name);
		break;

	case ATFI_AISG_DEVICE_SCAN_REJ:
		printf("Received ATFI_AISG_DEVICE_SCAN_REJ(lh:%u,ec:%u) from %s",
		       htonl(msg->atfiAisgDeviceScanRej.addrInfo.linkHandle),
		       htons(msg->atfiAisgDeviceScanRej.errorCode),
		       name);
		break;

	case ATFI_ADD_AISG_MAP_CFM:
		printf("Received ATFI_ADD_AISG_MAP_CFM(lh:%u) from %s",
		       htonl(msg->atfiAddAisgMapCfm.addrInfo.linkHandle),
		       name);
		break;

	case ATFI_ADD_AISG_MAP_REJ:
		printf("Received ATFI_ADD_AISG_MAP_REJ(lh:%u,ec:%u) from %s",
		       htonl(msg->atfiAddAisgMapRej.addrInfo.linkHandle),
		       htons(msg->atfiAddAisgMapRej.errorCode),
		       name);
		break;

	default:
		printf("Received unexpected signal 0x%08x from %s",
		       msg->msgno, name);
		break;
	}

	if (msg) {
		itc_free(&msg);
	}

	if (mbox_info) {
		itc_free((union itc_msg**)(void*) &mbox_info);
	}
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		printf("Syntax: ./atfi_client <instance>\n");
		return -1;
	}

	atf_start(argv[1]);

	for (;;) {
		atf_handle_message();
	}
}

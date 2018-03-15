
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>

#include <itc.h>

#include "atfi.h"
#include "atfi.sig"


static const char *help = {
	"ATF commands:\n"
	"atf -ce <instance> <server> <address> <protocol revision>\n"
	"atf -acm <instance> <server> <address> <logical address> <physical address>\n"
	"atf -rcm <instance> <server> <address> <logical address>\n"
	"atf -co <instance> <server> <address> <logical address> <type of unit>\n"
	"atf -dc <instance> <server> <address> <logical address>\n"
	"atf -reset <instance> <server> <address>\n"
	"atf -audit <instance> <server> <address> <logical address>\n"
	"atf -ads <instance> <server> <address> <baudrate> <timeout> <maskM> <mb0 ... mbM> <uidN> <ub0 ... ubN>\n"
	"atf -gauid <instance> <server> <address> <logical address>\n"
	"atf -aam <instance> <server> <address> <logical address> <device type> <uidN> <ub0 ... ubN>\n"
};

static const char *lazy = {
	"ATF command examples:\n"
	"atf -ce 0 MXP_0 0 3\n"
	"atf -acm 0 MXP_0 0 1 1\n"
	"atf -rcm 0 MXP_0 0 1\n"
	"atf -co 0 MXP_0 0 1 0\n"
	"atf -dc 0 MXP_0 0 0\n"
	"atf -reset 0 MXP_0 0\n"
	"atf -ads 0 XXP_0 0 1 500 3 255 255 255 3 69 82 49\n"
};

union itc_msg {
	uint32_t                    sigNo;
#include "atfiUnionContent.h"
};


static void init(char *name)
{
	setbuf(stdout, 0);
	itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if (itc_create_mailbox(name, 0) == ITC_NO_ID) {
		printf("Could not create mailbox \"%s\"\n", name);
		exit(-1);
	}
}

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

static itc_mbox_id_t hunt_client(char *name, const char *instance)
{
	(void) sprintf(name, "ATF_Client_%s", instance);
	return hunt(name);
}

static int atf_connection_establish(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 6) {
		printf("The Connection Establish command takes 4 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiConnEstablishReqS),
			ATFI_CONN_ESTABLISH_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiConnEstablishReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiConnEstablishReq.protocolRev);
	if (status != 1) {
		printf("Invalid value \"%s\" for protocol revision\n", argv[5]);
		return 1;
	}

	sig->atfiConnEstablishReq.addrInfo.linkHandle =
		htonl(sig->atfiConnEstablishReq.addrInfo.linkHandle);
	sig->atfiConnEstablishReq.protocolRev =
		htons(sig->atfiConnEstablishReq.protocolRev);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_add_connection_map(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 7) {
		printf("The Add Connection Map command takes 5 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiAddConnMapReqS),
			ATFI_ADD_CONN_MAP_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiAddConnMapReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiAddConnMapReq.logicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for logical address\n", argv[5]);
		return 1;
	}

	status = sscanf(argv[6], "%hu",
			&sig->atfiAddConnMapReq.physicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for physical address\n", argv[6]);
		return 1;
	}

	sig->atfiConnEstablishReq.addrInfo.linkHandle =
		htonl(sig->atfiConnEstablishReq.addrInfo.linkHandle);
	sig->atfiAddConnMapReq.logicalAddress =
		htons(sig->atfiAddConnMapReq.logicalAddress);
	sig->atfiAddConnMapReq.physicalAddress =
		htons(sig->atfiAddConnMapReq.physicalAddress);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_remove_connection_map(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 6) {
		printf("The Remove Connect Map command takes 4 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiRemoveConnMapReqS),
			ATFI_REMOVE_CONN_MAP_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiRemoveConnMapReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiRemoveConnMapReq.logicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for logical address\n", argv[5]);
		return 1;
	}

	sig->atfiRemoveConnMapReq.addrInfo.linkHandle =
		htonl(sig->atfiRemoveConnMapReq.addrInfo.linkHandle);
	sig->atfiRemoveConnMapReq.logicalAddress =
		htons(sig->atfiRemoveConnMapReq.logicalAddress);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_connect(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 7) {
		printf("The Connect command takes 5 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiConnect2ReqS),
			ATFI_CONNECT2_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiConnect2Req.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiConnect2Req.logicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for logical address\n", argv[5]);
		return 1;
	}

	status = sscanf(argv[6], "%hu",
			&sig->atfiConnect2Req.typeOfUnit);
	if (status != 1) {
		printf("Invalid value \"%s\" for type of unit\n", argv[6]);
		return 1;
	}

	sig->atfiConnect2Req.addrInfo.linkHandle =
		htonl(sig->atfiConnect2Req.addrInfo.linkHandle);
	sig->atfiConnect2Req.logicalAddress =
		htons(sig->atfiConnect2Req.logicalAddress);
	sig->atfiConnect2Req.typeOfUnit =
		htons(sig->atfiConnect2Req.typeOfUnit);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_disconnect(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 6) {
		printf("The Disconnect command takes 4 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiDisconnectReqS),
			ATFI_DISCONNECT_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiDisconnectReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiDisconnectReq.logicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for logical address\n", argv[5]);
		return 1;
	}

	sig->atfiConnect2Req.addrInfo.linkHandle =
		htonl(sig->atfiConnect2Req.addrInfo.linkHandle);
	sig->atfiConnect2Req.logicalAddress =
		htons(sig->atfiConnect2Req.logicalAddress);
	sig->atfiConnect2Req.typeOfUnit =
		htons(sig->atfiConnect2Req.typeOfUnit);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_reset(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 5) {
		printf("The Reset command takes 3 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiResetReqS),
			ATFI_RESET_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiResetReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	sig->atfiResetReq.addrInfo.linkHandle =
		htonl(sig->atfiResetReq.addrInfo.linkHandle);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_audit(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 6) {
		printf("The Audit command takes 4 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiAuditReqS), ATFI_AUDIT_REQ);

	status = sscanf(argv[4], "%u", &sig->atfiAuditReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu", &sig->atfiAuditReq.logicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for logical address\n", argv[5]);
		return 1;
	}

	sig->atfiAuditReq.addrInfo.linkHandle =
		htonl(sig->atfiAuditReq.addrInfo.linkHandle);
	sig->atfiAuditReq.logicalAddress =
		htons(sig->atfiAuditReq.logicalAddress);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_aisg_device_scan(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status, cnt, idx;
	uint16_t       dummy;

	if (argc < 10) {
		printf("The AISG Device Scan command takes some parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiAisgDeviceScanReqS),
			ATFI_AISG_DEVICE_SCAN_REQ);
	memset(sig->atfiAisgDeviceScanReq.uniqueHwIdMask, 0,
	       ATFI_MAX_UNIQUE_HW_ID_LENGTH);
	memset(sig->atfiAisgDeviceScanReq.uniqueHwId, 0,
	       ATFI_MAX_UNIQUE_HW_ID_LENGTH);

	status = sscanf(argv[4], "%u",
			&sig->atfiAisgDeviceScanReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiAisgDeviceScanReq.baudrate);
	if (status != 1) {
		printf("Invalid value \"%s\" for baudrate\n", argv[5]);
		return 1;
	}

	status = sscanf(argv[6], "%hu",
			&sig->atfiAisgDeviceScanReq.timeout);
	if (status != 1) {
		printf("Invalid value \"%s\" for timeout\n", argv[6]);
		return 1;
	}

	status = sscanf(argv[7], "%hu", &dummy);
	if (status != 1) {
		printf("Invalid value \"%s\" for mask length\n", argv[7]);
		return 1;
	}
	sig->atfiAisgDeviceScanReq.uniqueHwIdMaskLength = (uint8_t) dummy;

	for (idx = 0, cnt = 8;
	     idx < sig->atfiAisgDeviceScanReq.uniqueHwIdMaskLength && cnt < argc;
	     idx++, cnt++) {
		status = sscanf(argv[cnt], "%hu", &dummy);
		if (status != 1) {
			printf("Invalid value \"%s\" for mask\n",
			       argv[cnt]);
			return 1;
		}
		sig->atfiAisgDeviceScanReq.uniqueHwIdMask[idx] = dummy;
	}

	status = sscanf(argv[cnt], "%hu", &dummy);
	if (status != 1) {
		printf("Invalid value \"%s\" for unique identifier length\n", argv[cnt]);
		return 1;
	}
	sig->atfiAisgDeviceScanReq.uniqueHwIdLength = (uint8_t) dummy;
	cnt++;

	for (idx = 0;
	     idx < sig->atfiAisgDeviceScanReq.uniqueHwIdLength && cnt < argc;
	     idx++, cnt++) {
		status = sscanf(argv[cnt], "%hu", &dummy);
		if (status != 1) {
			printf("Invalid value \"%s\" for unique identifier\n",
			       argv[cnt]);
			return 1;
		}
		sig->atfiAisgDeviceScanReq.uniqueHwId[idx] = (uint8_t) dummy;
	}

	sig->atfiAisgDeviceScanReq.addrInfo.linkHandle =
		htonl(sig->atfiAisgDeviceScanReq.addrInfo.linkHandle);
	sig->atfiAisgDeviceScanReq.baudrate =
		htons(sig->atfiAisgDeviceScanReq.baudrate);
	sig->atfiAisgDeviceScanReq.timeout =
		htons(sig->atfiAisgDeviceScanReq.timeout);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_get_aisg_unique_identifier(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status;

	if (argc != 6) {
		printf("The Get AISG Unique Identifier command takes 4 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiGetAisgUniqueIdReqS),
			ATFI_GET_AISG_UNIQUE_ID_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiGetAisgUniqueIdReq.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiGetAisgUniqueIdReq.logicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for logical address\n", argv[5]);
		return 1;
	}

	sig->atfiGetAu3PortReq.addrInfo.linkHandle =
		htonl(sig->atfiGetAu3PortReq.addrInfo.linkHandle);
	sig->atfiGetAu3PortReq.logicalAddress =
		htons(sig->atfiGetAu3PortReq.logicalAddress);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int atf_add_aisg_map(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];
	int            status, i;
	uint16_t       dummy;

	if (argc < 8 && argc > 27) {
		printf("The Add AISG Map command takes 6 to 25 parameters\n");
		return 1;
	}

	cPid = hunt_client(name, argv[2]);
	if (cPid == 0) {
		printf("Client \"%s\" does not exist\n", name);
		return 1;
	}

	sPid = hunt(argv[3]);
	if (sPid == 0) {
		printf("Server \"%s\" does not exist\n", argv[3]);
		return 1;
	}

	sig = itc_alloc(sizeof(struct atfiAddAisgMap2ReqS),
			ATFI_ADD_AISG_MAP2_REQ);

	status = sscanf(argv[4], "%u",
			&sig->atfiAddAisgMap2Req.addrInfo.linkHandle);
	if (status != 1) {
		printf("Invalid value \"%s\" for address\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "%hu",
			&sig->atfiAddAisgMap2Req.logicalAddress);
	if (status != 1) {
		printf("Invalid value \"%s\" for logical address\n", argv[5]);
		return 1;
	}

	status = sscanf(argv[6], "%hu",
			&sig->atfiAddAisgMap2Req.deviceType);
	if (status != 1) {
		printf("Invalid value \"%s\" for device type\n", argv[6]);
		return 1;
	}

	status = sscanf(argv[7], "%hu", &dummy);
	sig->atfiAddAisgMap2Req.uniqueHwIdLength = (uint8_t) dummy;
	if (status != 1 ||
	    argc + 8 < sig->atfiAddAisgMap2Req.uniqueHwIdLength ||
	    sig->atfiAddAisgMap2Req.uniqueHwIdLength > ATFI_MAX_UNIQUE_HW_ID_LENGTH) {
		printf("Invalid value \"%s\" for length\n", argv[7]);
		return 1;
	}

	memset(&sig->atfiAddAisgMap2Req.uniqueHwId, 0,
	       ATFI_MAX_UNIQUE_HW_ID_LENGTH);
	for (i = 0; i < sig->atfiAddAisgMap2Req.uniqueHwIdLength; i++) {
		status = sscanf(argv[8 + i], "%hu", &dummy);
		sig->atfiAddAisgMap2Req.uniqueHwId[i] = (uint8_t) dummy;
		if (status != 1) {
			printf("Invalid value \"%s\" for HW Identifier\n", argv[8 + i]);
			return 1;
		}
	}

	sig->atfiAddAisgMap2Req.addrInfo.linkHandle =
		htonl(sig->atfiAddAisgMap2Req.addrInfo.linkHandle);
	sig->atfiAddAisgMap2Req.logicalAddress =
		htons(sig->atfiAddAisgMap2Req.logicalAddress);
	sig->atfiAddAisgMap2Req.deviceType =
		htons(sig->atfiAddAisgMap2Req.deviceType);

	itc_send(&sig, cPid, sPid);

	return 0;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		goto atf_cmd_fail;
	}

	init("ATF_Client_Command");

	if (strcmp(argv[1], "-ce") == 0) {
		return atf_connection_establish(argc, argv);
	} else if (strcmp(argv[1], "-acm") == 0) {
		return atf_add_connection_map(argc, argv);
	} else if (strcmp(argv[1], "-rcm") == 0) {
		return atf_remove_connection_map(argc, argv);
	} else if (strcmp(argv[1], "-co") == 0) {
		return atf_connect(argc, argv);
	} else if (strcmp(argv[1], "-dc") == 0) {
		return atf_disconnect(argc, argv);
	} else if (strcmp(argv[1], "-reset") == 0) {
		return atf_reset(argc, argv);
	} else if (strcmp(argv[1], "-audit") == 0) {
		return atf_audit(argc, argv);
	} else if (strcmp(argv[1], "-ads") == 0) {
		return atf_aisg_device_scan(argc, argv);
	} else if (strcmp(argv[1], "-gauid") == 0) {
		return atf_get_aisg_unique_identifier(argc, argv);
	} else if (strcmp(argv[1], "-aam") == 0) {
		return atf_add_aisg_map(argc, argv);
	} else if (strcmp(argv[1], "-help") == 0) {
		printf(help);
		return 0;
	} else if (strcmp(argv[1], "-lazy") == 0) {
		printf(lazy);
		return 0;
	}

atf_cmd_fail:
	printf("atf -help\n");
	return 1;
}

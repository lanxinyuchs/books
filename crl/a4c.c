
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>

#include <itc.h>

#include "a4ci.h"
#include "a4ci.sig"


static const char *help =
	"A4C commands:\n"
	"a4c -ce <instance> <server> <protocol revision>\n"
	"a4c -df <instance> <server> <port> <address> <length> <1:st byte> <2:nd byte>...\n"
	"a4c -dr <instance> <server> <port> <address> <length> <1:st byte> <2:nd byte>...\n";

static const char *lazy =
	"A4C command examples:\n"
	"a4c -ce 0 A4ciServer 1\n"
	"a4c -df 0 A4ciServer 1 1 5 1 2 3 4 5\n"
	"a4c -dr 0 A4ciServer 1 1 2 0x6c 0x00\n"
	"# CC HW PID (address FCU=0x2e, BFU=0x2d)\n"
	"a4c -dr 0 A4ciServer 1 0x2e 2 0x8 1\n"
	"a4c -dr 0 A4ciServer 1 0x2e 0\n";

union itc_msg {
	uint32_t                      sigNo;
	struct a4ci_connEstablishReqS a4ciConnEstablishReq;
	struct a4ci_connEstablishCfmS a4ciConnEstablishCfm;
	struct a4ci_connEstablishRejS a4ciConnEstablishRej;
	struct a4ci_dataFwdS          a4ciDataFwd;
	struct a4ci_dataReqS          a4ciDataReq;
	struct a4ci_dataCfmS          a4ciDataCfm;
	struct a4ci_dataRejS          a4ciDataRej;
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

	sig = itc_alloc(sizeof(uint32_t), 0xcafebabe);
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
	(void) sprintf(name, "A4C_Client_%s", instance);
	return hunt(name);
}

static int a4c_connection_establish(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t cPid, sPid;
	char          name[64];
	int           status;

	if (argc != 5) {
		printf("The connection establish command takes three parameters\n");
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

	sig = itc_alloc(sizeof(struct a4ci_connEstablishReqS),
			A4CI_CONN_ESTABLISH_REQ);

	status = sscanf(argv[4], "%hu",
			&sig->a4ciConnEstablishReq.protocolRev);
	if (status != 1) {
		printf("Invalid value \"%s\" for protocol revision\n", argv[4]);
		return 1;
	}

	sig->a4ciConnEstablishReq.protocolRev =
		htons(sig->a4ciConnEstablishReq.protocolRev);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int a4c_data_fwd(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	int            status;
	uint32_t       tmp;
	uint16_t       idx;
	char           name[64];

	if (argc < 7) {
		printf("Too few parameters\n");
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

	sig = itc_alloc(sizeof(struct a4ci_dataFwdS),
	                A4CI_DATA_FWD);

	status = sscanf(argv[4], "%hu",	&sig->a4ciDataFwd.port);
	if (status != 1) {
		printf("Invalid value \"%s\" for port\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "0x%x", &tmp);
	if (status != 1) {
		status = sscanf(argv[5], "%u", &tmp);
		if (status != 1) {
			printf("Invalid value \"%s\" for HDLC address\n",
			       argv[5]);
			return 1;
		}
	}
	sig->a4ciDataFwd.hdlcAddr = (uint8_t) tmp;

	status = sscanf(argv[6], "%hu", &sig->a4ciDataFwd.length);
	if (status != 1) {
		printf("Invalid value \"%s\" for length\n", argv[6]);
		return 1;
	}

	if (argc < (7 + sig->a4ciDataFwd.length)) {
		printf("Too few data parameters\n");
		return 1;
	}

	for (idx = 0; idx < sig->a4ciDataFwd.length; idx++) {
		status = sscanf(argv[7 + idx], "0x%x", &tmp);
		if (status != 1) {
			status = sscanf(argv[7 + idx], "%u", &tmp);
			if (status != 1) {
				printf("Invalid value \"%s\" for data\n",
				       argv[7 + idx]);
				return 1;
			}
		}
		sig->a4ciDataFwd.data[idx] = (uint8_t) tmp;
	}

	sig->a4ciDataFwd.port =
		htons(sig->a4ciDataFwd.port);
	sig->a4ciDataFwd.length =
		htons(sig->a4ciDataFwd.length);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int a4c_data_req(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	int            status;
	uint32_t       tmp;
	uint16_t       idx;
	char           name[64];

	if (argc < 7) {
		printf("Too few parameters\n");
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

	sig = itc_alloc(sizeof(struct a4ci_dataReqS),
	                A4CI_DATA_REQ);

	status = sscanf(argv[4], "%hu",	&sig->a4ciDataReq.port);
	if (status != 1) {
		printf("Invalid value \"%s\" for port\n", argv[4]);
		return 1;
	}

	status = sscanf(argv[5], "0x%x", &tmp);
	if (status != 1) {
		status = sscanf(argv[5], "%u", &tmp);
		if (status != 1) {
			printf("Invalid value \"%s\" for HDLC address\n",
			       argv[5]);
			return 1;
		}
	}
	sig->a4ciDataReq.hdlcAddr = (uint8_t) tmp;

	status = sscanf(argv[6], "%hu", &sig->a4ciDataReq.length);
	if (status != 1) {
		printf("Invalid value \"%s\" for length\n", argv[6]);
		return 1;
	}

	if (argc < (7 + sig->a4ciDataReq.length)) {
		printf("Too few data parameters\n");
		return 1;
	}

	for (idx = 0; idx < sig->a4ciDataReq.length; idx++) {
		status = sscanf(argv[7 + idx], "0x%x", &tmp);
		if (status != 1) {
			status = sscanf(argv[7 + idx], "%u", &tmp);
			if (status != 1) {
				printf("Invalid value \"%s\" for data\n",
				       argv[7 + idx]);
				return 1;
			}
		}
		sig->a4ciDataReq.data[idx] = (uint8_t) tmp;
	}

	sig->a4ciDataReq.port =
		htons(sig->a4ciDataReq.port);
	sig->a4ciDataReq.length =
		htons(sig->a4ciDataReq.length);

	itc_send(&sig, cPid, sPid);

	return 0;
}


int main(int argc, char **argv)
{
	if (argc < 2) {
		goto a4c_cmd_fail;
	}

	init("A4C_Client_Command");

	if (strcmp(argv[1], "-ce") == 0) {
		return a4c_connection_establish(argc, argv);
	} else if (strcmp(argv[1], "-df") == 0) {
		return a4c_data_fwd(argc, argv);
	} else if (strcmp(argv[1], "-dr") == 0) {
		return a4c_data_req(argc, argv);
	} else if (strcmp(argv[1], "-help") == 0) {
		printf(help);
		return 0;
	} else if (strcmp(argv[1], "-lazy") == 0) {
		printf(lazy);
		return 0;
	}

a4c_cmd_fail:
	printf("Commands are; -ce, -df, -dr, -help, -lazy\n");
	return 1;
}

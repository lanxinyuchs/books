
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>

#include <itc.h>

#include "a2ci.h"
#include "a2ci.sig"


static const char *help =
	"A2C commands:\n"
	"a2c -start <instance>\n"
	"a2c -kill <instance>\n"
	"a2c -ce <instance> <server> <protocol revision>\n"
	"a2c -df <instance> <server> <length> <1:st byte> <2:nd byte>...\n"
	"a2c -reset <instance> <server>\n";

static const char *lazy =
	"A2C command examples:\n"
	"a2c -start 0\n"
	"a2c -kill  0\n"
	"a2c -ce 0 BXP_0/EPP_AISG_42_2 1\n"
	"a2c -df 0 BXP_0/EPP_AISG_42_2 5 1 2 3 4 5\n"
	"a2c -reset 0 BXP_0/EPP_AISG_42_2\n";

union itc_msg {
	uint32_t                      sigNo;
	struct a2ci_connEstablishReqS a2ciConnEstablishReq;
	struct a2ci_connEstablishCfmS a2ciConnEstablishCfm;
	struct a2ci_connEstablishRejS a2ciConnEstablishRej;
	struct a2ci_dataFwdS          a2ciDataFwd;
	struct a2ci_dataIndS          a2ciDataInd;
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
	(void) sprintf(name, "A2C_Client_%s", instance);
	return hunt(name);
}

static int a2c_connection_establish(int argc, char **argv)
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

	sig = itc_alloc(sizeof(struct a2ci_connEstablishReqS),
			A2CI_CONN_ESTABLISH_REQ);

	status = sscanf(argv[4], "%hu",
			&sig->a2ciConnEstablishReq.protocolRev);
	if (status != 1) {
		printf("Invalid value \"%s\" for protocol revision\n", argv[4]);
		return 1;
	}

	sig->a2ciConnEstablishReq.protocolRev =
		htons(sig->a2ciConnEstablishReq.protocolRev);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int a2c_data_fwd(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	int            status;
	uint32_t       tmp;
	uint16_t       idx;
	char           name[64];

	if (argc < 6) {
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

	sig = itc_alloc(sizeof(struct a2ci_dataFwdS),
			A2CI_DATA_FWD);

	status = sscanf(argv[4], "%hu",	&sig->a2ciDataFwd.length);
	if (status != 1) {
		printf("Invalid value \"%s\" for length\n", argv[4]);
		return 1;
	}

	if (argc < (5 + sig->a2ciDataFwd.length)) {
		printf("Too few data parameters\n");
		return 1;
	}

	for (idx = 0; idx < sig->a2ciDataFwd.length; idx++) {
		status = sscanf(argv[5 + idx], "0x%x", &tmp);
		if (status != 1) {
			status = sscanf(argv[5 + idx], "%u", &tmp);
			if (status != 1) {
				printf("Invalid value \"%s\" for data\n", argv[5 + idx]);
				return 1;
			}
		}
		sig->a2ciDataFwd.data[idx] = (uint8_t) tmp;
	}

	sig->a2ciDataFwd.length =
		htons(sig->a2ciDataFwd.length);

	itc_send(&sig, cPid, sPid);

	return 0;
}

static int a2c_reset_fwd(int argc, char **argv)
{
	union itc_msg *sig;
	itc_mbox_id_t  cPid, sPid;
	char           name[64];

	if (argc != 4) {
		printf("The connection establish command takes two parameters\n");
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

	sig = itc_alloc(sizeof(struct a2ci_resetFwdS), A2CI_RESET_FWD);

	itc_send(&sig, cPid, sPid);

	return 0;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		goto a2c_cmd_fail;
	}

	init("A2C_Client_Command");

	if (strcmp(argv[1], "-ce") == 0) {
		return a2c_connection_establish(argc, argv);
	} else if (strcmp(argv[1], "-df") == 0) {
		return a2c_data_fwd(argc, argv);
	} else if (strcmp(argv[1], "-reset") == 0) {
		return a2c_reset_fwd(argc, argv);
	} else if (strcmp(argv[1], "-help") == 0) {
		printf(help);
		return 0;
	} else if (strcmp(argv[1], "-lazy") == 0) {
		printf(lazy);
		return 0;
	}

a2c_cmd_fail:
	printf("Commands are; -ce, -df, -reset, -help, -lazy\n");
	return 1;
}

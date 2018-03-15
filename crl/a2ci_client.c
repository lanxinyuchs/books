
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>

#include <itc.h>
#include <itc_system.h>

#include "a2ci.h"
#include "a2ci.sig"


union itc_msg {
	uint32_t                      msgno;
	struct a2ci_connEstablishReqS a2ciConnEstablishReq;
	struct a2ci_connEstablishCfmS a2ciConnEstablishCfm;
	struct a2ci_connEstablishRejS a2ciConnEstablishRej;
	struct a2ci_dataFwdS          a2ciDataFwd;
	struct a2ci_dataIndS          a2ciDataInd;
};


static int a2c_start(char *instance)
{
	itc_mbox_id_t client_mbox;
	char          name[64];

	setbuf(stdout, 0);
	itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	snprintf(name, sizeof(name), "A2C_Client_%s", instance);

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

static void hexdump(char *prefix, uint8_t *data, uint32_t size)
{
	char     text[3 * 16 + 1], *ptr;
	uint32_t idx0, idx1;

	printf(prefix);
	for (idx0 = 0; idx0 < size; idx0 += 16) {
		for (idx1 = idx0, ptr = text;
		     idx1 < idx0 + 16 && idx1 < size;
		     idx1++) {
			ptr += sprintf(ptr, "%02x ", data[idx1]);
		}
		printf(text);
	}
}

static void a2c_handle_message(void)
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

	case A2CI_CONN_ESTABLISH_REQ:
		printf("Sent A2CI_CONN_ESTABLISH_REQ(p:%u) to %s",
		       ntohs(msg->a2ciConnEstablishReq.protocolRev),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case A2CI_DATA_FWD:
		printf("Sent A2CI_DATA_FWD(l:%u) to %s",
		       ntohs(msg->a2ciDataFwd.length),
		       name);
		hexdump("A2CI_DATA_FWD.data:",
			msg->a2ciDataFwd.data,
			ntohs(msg->a2ciDataFwd.length));
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case A2CI_RESET_FWD:
		printf("Sent A2CI_RESET_FWD to %s",
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;


		/*
		** SERVER TO CLIENT MSGNALS.
		*/

	case A2CI_CONN_ESTABLISH_CFM:
		printf("Received A2CI_CONN_ESTABLISH_CFM from %s",
		       name);
		break;

	case A2CI_CONN_ESTABLISH_REJ:
		printf("Received A2CI_CONN_ESTABLISH_REJ(e:0x%x,p:%u) from %s",
		       ntohs(msg->a2ciConnEstablishRej.errorCode),
		       ntohs(msg->a2ciConnEstablishRej.protocolRev),
		       name);
		break;

	case A2CI_DATA_IND:
		printf("Received A2CI_DATA_IND(l:%u) from %s",
		       ntohs(msg->a2ciDataInd.length),
		       name);
		hexdump("A2CI_DATA_IND.data:",
			msg->a2ciDataInd.data,
			ntohs(msg->a2ciDataInd.length));
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
		printf("Syntax: ./a2ci_client <instance>\n");
		return -1;
	}

	a2c_start(argv[1]);

	for (;;) {
		a2c_handle_message();
	}
}

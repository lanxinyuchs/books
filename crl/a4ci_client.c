
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>

#include <itc.h>
#include <itc_system.h>

#include "a4ci.h"
#include "a4ci.sig"


union itc_msg {
	uint32_t                      msgno;
	struct a4ci_connEstablishReqS a4ciConnEstablishReq;
	struct a4ci_connEstablishCfmS a4ciConnEstablishCfm;
	struct a4ci_connEstablishRejS a4ciConnEstablishRej;
	struct a4ci_dataFwdS          a4ciDataFwd;
	struct a4ci_dataReqS          a4ciDataReq;
	struct a4ci_dataCfmS          a4ciDataCfm;
	struct a4ci_dataRejS          a4ciDataRej;
};


static int a4c_start(char *instance)
{
	itc_mbox_id_t client_mbox;
	char          name[64];

	setbuf(stdout, 0);
	itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	snprintf(name, sizeof(name), "A4C_Client_%s", instance);

	client_mbox = itc_create_mailbox(name, 0);
	if (client_mbox == ITC_NO_ID) {
		fprintf(stderr,
		        "Could not create client mailbox \"%s\"\n", name);
		return -1;
	}
	printf("Client \"%s\" (0x%x) successfully started\n",
	       name, client_mbox);

	printf("Press Control-C to exit\n");

	return 0;
}

static void hexdump(char *prefix, uint8_t *data, uint32_t size)
{
	uint32_t idx0, idx1;

	printf(prefix);
	for (idx0 = 0; idx0 <= size; idx0 += 16) {
		for (idx1 = idx0; idx1 < (idx0 + 16) && idx1 < size; idx1++) {
			printf("%02x ", data[idx1]);
		}
		putchar('\n');
	}
}

static void a4c_handle_message(void)
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
		** CLIENT TO SERVER SIGNALS.
		*/

	case A4CI_CONN_ESTABLISH_REQ:
		printf("Sent A4CI_CONN_ESTABLISH_REQ(p:%u) to %s\n",
		       ntohs(msg->a4ciConnEstablishReq.protocolRev),
		       name);
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case A4CI_DATA_FWD:
		printf("Sent A4CI_DATA_FWD(p:%u,a:0x%x,l:%u) to %s\n",
		       ntohs(msg->a4ciDataFwd.port),
		       msg->a4ciDataFwd.hdlcAddr,
		       ntohs(msg->a4ciDataFwd.length),
		       name);
		hexdump("A4CI_DATA_FWD.data:",
		        msg->a4ciDataFwd.data,
			ntohs(msg->a4ciDataFwd.length));
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;

	case A4CI_DATA_REQ:
		printf("Sent A4CI_DATA_REQ(p:%u,a:0x%x,l:%u) to %s\n",
		       ntohs(msg->a4ciDataReq.port),
		       msg->a4ciDataReq.hdlcAddr,
		       ntohs(msg->a4ciDataReq.length),
		       name);
		hexdump("A4CI_DATA_REQ.data:",
			msg->a4ciDataReq.data,
			ntohs(msg->a4ciDataReq.length));
		itc_send(&msg, mbox, ITC_MY_MBOX);
		break;


		/*
		** SERVER TO CLIENT SIGNALS.
		*/

	case A4CI_CONN_ESTABLISH_CFM:
		printf("Received A4CI_CONN_ESTABLISH_CFM from %s\n",
		       name);
		break;

	case A4CI_CONN_ESTABLISH_REJ:
		printf("Received A4CI_CONN_ESTABLISH_REJ(e:0x%x,p:%u) from %s\n",
		       ntohs(msg->a4ciConnEstablishRej.errorCode),
		       ntohs(msg->a4ciConnEstablishRej.protocolRev),
		       name);
		break;

	case A4CI_DATA_CFM:
		printf("Received A4CI_DATA_CFM(p:%u,a:0x%x,l:%u) from %s\n",
		       ntohs(msg->a4ciDataCfm.port),
		       msg->a4ciDataCfm.hdlcAddr,
		       ntohs(msg->a4ciDataCfm.length),
		       name);
		hexdump("A4CI_DATA_CFM.data:",
			msg->a4ciDataCfm.data,
			ntohs(msg->a4ciDataCfm.length));
		break;

	case A4CI_DATA_REJ:
		printf("Received A4CI_DATA_REJ(e:0x%x,p:%u,a:0x%x) from %s\n",
		       ntohs(msg->a4ciDataRej.errorCode),
		       ntohs(msg->a4ciDataRej.port),
		       msg->a4ciDataRej.hdlcAddr,
		       name);
		break;

	default:
		printf("Received unexpected signal 0x%08x from %s\n",
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
		fprintf(stderr, "Syntax: ./a4ci_client <instance>\n");
		return -1;
	}

	a4c_start(argv[1]);

	for (;;) {
		a4c_handle_message();
	}
}

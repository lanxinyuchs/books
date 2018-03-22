#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <getopt.h>
#include <arpa/inet.h>

#include <itc.h>

#include "atfi.sig"
#include "log.h"

#define ATFID_DEFAULT_SRV	"MXP_0"
#define ATFID_RECEIVE_TMO	1000
#define ATFID_MAILBOX_SIZE	16
#define ITC_SENDER_NAME_LEN	128

union itc_msg {
	uint32_t                           msgno;
	struct atfiConnEstablishReqS       atfiConnEstablishReq;
	struct atfiConnEstablishCfmS       atfiConnEstablishCfm;
	struct atfiConnEstablishRejS       atfiConnEstablishRej;
};

/* :-( */
static int srvexit = 0;

static void sig_handler(int signo)
{
	if (signo == SIGINT || signo == SIGTERM)
		srvexit = 1;
}

static void handle_conn_req(struct atfiConnEstablishReqS *req,
			   itc_mbox_id_t mbox)
{
	union itc_msg *msg;

	req->protocolRev = ntohs(req->protocolRev);

	log_trace2("Received ATFI_CONN_ESTABLISH_REQ(pr:%u)",
	       req->protocolRev);

	msg = itc_alloc(sizeof(struct atfiConnEstablishRejS),
			ATFI_CONN_ESTABLISH_REJ);

	msg->atfiConnEstablishRej.errorCode =
		htons(ATFI_OTHER_ERROR);
	msg->atfiConnEstablishRej.addrInfo.linkHandle =
		req->addrInfo.linkHandle;
	msg->atfiConnEstablishRej.highestSupportedProtocolRev =
		htons(ATFI_PROTOCOL_REV);
	log_trace2("Sent ATFI_CONN_ESTABLISH_REJ(hpr:%u)",
	       ATFI_PROTOCOL_REV);
	itc_send(&msg, mbox, ITC_MY_MBOX);
}

static void handle_requests()
{
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char sender[ITC_SENDER_NAME_LEN] = {0};

	log_info("%s:Starting minimal server", ATFID_DEFAULT_SRV);

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ATFID_RECEIVE_TMO, ITC_FROM_ALL);
		if (srvexit)
			break;

		if (!msg)
			continue;

		mbox = itc_sender(msg);
		switch (msg->msgno) {
		case ATFI_CONN_ESTABLISH_REQ:
			handle_conn_req(&msg->atfiConnEstablishReq, mbox);
			break;
		default:
			itc_get_name(mbox, sender, ITC_SENDER_NAME_LEN);
			log_info("Received an unexpected message 0x%x, from %s",
                                 msg->msgno, sender);
			break;
		}
		itc_free(&msg);
	}

	log_info("%s:Terminating minimal server", ATFID_DEFAULT_SRV);
}


static int run()
{
	itc_mbox_id_t srv_mbox;

	const struct sigaction act = {
		.sa_handler = sig_handler,
		.sa_flags = 0
	};

	/* Setup signal handler. */
	sigaction(SIGINT, &act, NULL);
	sigaction(SIGTERM, &act, NULL);

	/* Initiate ITC. */
	itc_init(ATFID_MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	srv_mbox = itc_create_mailbox(ATFID_DEFAULT_SRV, 0);
	if (srv_mbox == ITC_NO_ID) {
		log_err("itc_create_mailbox on minimal server failed");
		return -1;
	}

	handle_requests();

	itc_delete_mailbox(srv_mbox);
	return 0;
}

int main(int argc, char *argv[])
{
	int daemonize = 1;
	int c;
	const char *const   sopt = "hn";
	const struct option lopt[] = {
		{ "help", no_argument, NULL, 'h'},
		{ "foreground", no_argument, NULL, 'n'},
		{ 0, 0, 0, 0 }
	};

	while ((c = getopt_long(argc, argv, sopt, lopt, NULL)) != -1) {
		switch (c) {
		case 'h':
			printf("Usage: atfi_minimald [option]\n"
			       "where option is one of:\n"
			       "-h|--help                 "
			       "Display this help message\n"
			       "-n|--foreground           "
			       "Avoid auto-backgrounding. This is\n"
			       "                          "
			       "needed especially if the atfi minimal server\n"
			       "                          "
			       "is started and controlled by init(8)\n");
			return 0;
		case 'n':
			/* Auto-backgrounding should be avoided */
			daemonize = 0;
			break;
		default:
			break;
		}
	}

	if (daemonize) {
		if (daemon(0,0)) {
			perror("daemon");
			return -1;
		}
	}

	log_info("ATFI minimal daemon starting.");

	run();

	if (daemonize) {
		log_info("ATFI minimal daemon exiting.");
	}

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>

#include <itc.h>

#include <a4ci.h>
#include <a4cid.h>
#include "a4cid.sig"


#define CMD_TMO 2000

static int _exit_;


union itc_msg {
	uint32_t                    msgno;
	struct a4cid_info_req       info_req;
	struct a4cid_info_rsp       info_rsp;
};

char usage[] = "a4cistat [-r -h]\n"
               "Display A4CI link statistics\n"
               "OPTIONS:\n"
               " -a              display whole EC bus statistics\n"
               " -r              reset statistics\n"
               " -h              prints this message\n";


static void sig_handler(int signo)
{
	if (signo == SIGINT || signo == SIGTERM) {
		_exit_ = 1;
	}
}


int main(int argc, char **argv)
{
	union itc_msg *msg;
	itc_mbox_id_t my_mbox, srv_mbox;

	int opt, type = 0;

	const struct sigaction act = { .sa_handler = sig_handler,
	                               .sa_flags = 0 };

	while ((opt = getopt(argc, argv, "hra")) != -1) {
		switch(opt) {
		case 'a':
			type = A4CID_TYPE_STAT_ALL;
			break;
		case 'r':
			type = A4CID_TYPE_RESET;
			break;
		case 'h':
			printf("%s", usage);
			return 0;
		default:
			printf("%s", usage);
			return 0;
		}
	}

	/* Setup signal handler. */
	sigaction(SIGINT, &act, NULL);
	sigaction(SIGTERM, &act, NULL);
	sigaction(SIGPIPE, &act, NULL);

	itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	my_mbox = itc_create_mailbox("a4ci_stat", 0);

	srv_mbox = itc_locate(A4CI_PHYSICAL_THREAD);
	if(srv_mbox == ITC_NO_ID) {
		printf(" mbox %s not found\n", A4CI_PHYSICAL_THREAD);
		return -1;
	}

	itc_monitor(srv_mbox, NULL);

	msg = itc_alloc(sizeof(struct a4cid_info_req),
	                A4CID_INFO_REQ);
	msg->info_req.type = type;

	itc_send(&msg, srv_mbox, ITC_MY_MBOX);

	while(!_exit_) {
		msg = itc_receive(ITC_NOFILTER, CMD_TMO, ITC_FROM_ALL);
		if(msg == NULL) {
			printf("No response from %s within %dms\n",
			       A4CI_PHYSICAL_THREAD, CMD_TMO);
			return -1;
		}

		switch(msg->msgno) {
		case ITC_MONITOR_DEFAULT_NO:
			_exit_ = 1;
			break;
		case A4CID_INFO_RSP:
			if(msg->info_rsp.result == A4CI_CODE_OK)
				printf("%s", msg->info_rsp.infotext);
			else
				printf("%s returned error: %x\n",
				       A4CI_PHYSICAL_THREAD,
				       msg->info_rsp.result);
			_exit_ = 1;
			break;
		default:
			printf("Unexpected message receive: 0x%x from mailbox 0x%x\n",
				       msg->msgno, itc_sender(msg));
			_exit_ = 1;
			break;
		}
		itc_free(&msg);

	}

	itc_delete_mailbox(my_mbox);
	itc_exit();

	return 0;
}

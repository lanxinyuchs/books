#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>

#include <itc.h>

#include <a2ci.h>
#include <atfid.sig>
#include <atfid.h>


#define CMD_TMO 2000

static int _exit_;

union itc_msg {
	uint32_t                    msgno;
	struct atfid_info_req       info_req;
	struct atfid_info_rsp       info_rsp;
};

char usage[] = "atfistat [<server_name> -d -a <logical_address> -t <type_of_unit>]\n"
        " <server_name>   display all statistics per server <default>\n"
	" -d all detailed statistics\n"
	" -a <logical_address> "
	" display statistics about specific address \n"
	" -t <type_of_unit> "
	" display statistics about device with specific unit type\n"
	" -r reset statistics\n"
	" -h display this message\n"
	"=================================================================\n"
	"Example how to use command:\n"
	"atfistat XXP_0\n"
	"atfistat XXP_0 -d \n"
	"atfistat XXP_0 -a 3\n"
	"atfistat XXP_0 -t 0\n";

int main(int argc, char **argv)
{
	union itc_msg *msg;
	itc_mbox_id_t srv_mbox, my_mbox;
	int info_type = ATFID_GET_ALL_CONN, info_reset = 0;
	long info_input = -1;
	char *srv_name;
	int opt;

	if (argc < 2) {
		fprintf(stderr, "Too few arguments to atfistat\n%s\n", usage);
		return -1;
	}

	srv_name = argv[1];

	if (srv_name[0] == '-') {
		fprintf(stderr,
		        "Argument 1 should be a server name name not an option\n%s\n",
		        usage);
		return -1;
	}

	while ((opt = getopt(argc, argv, "da:t:hr")) != -1) {
		switch (opt) {
			case 'a':
				info_type = ATFID_GET_ONE_CONN;
				info_input = strtol(optarg, NULL, 0);
				break;
			case 'd':
				info_type = ATFID_GET_ALL_DETAILED;
				break;
			case 't':
				info_type = ATFID_GET_TYPE_CONN;
				info_input = strtol(optarg, NULL, 0);
				break;
			case 'r':
				info_reset = ATFID_RESET_STAT;
				break;
			case 'h':
				printf("%s", usage);
				return 0;
			default:
				printf("%s", usage);
				return 0;
		}
	}

	itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	my_mbox = itc_create_mailbox("atfid_stat", 0);

	srv_mbox = itc_locate(srv_name);
	if (srv_mbox == ITC_NO_ID) {
		printf("atfi server \"%s\" not found\n", srv_name);
		return -1;
	}

	itc_monitor(srv_mbox, NULL);

	msg = itc_alloc(sizeof(struct atfid_info_req),
	                ATFID_INFO_REQ);
	msg->info_req.info_type = info_type;
	msg->info_req.info_input = info_input;
	msg->info_req.info_reset = info_reset;

	itc_send(&msg, srv_mbox, ITC_MY_MBOX);

	while (!_exit_) {
		msg = itc_receive(ITC_NOFILTER, CMD_TMO, ITC_FROM_ALL);
		if (msg == NULL) {
			printf("No response from %s within %dms\n",
			       srv_name,
			       CMD_TMO);
			return -1;
		}

		switch (msg->msgno) {
			case ITC_MONITOR_DEFAULT_NO:
				_exit_ = 1;
				break;
			case ATFID_INFO_RSP:
				if (msg->info_rsp.result == ATFID_CODE_OK)
					printf("%s", msg->info_rsp.infotext);
				else
					printf("%s returned error: %d\n",
					       srv_name, msg->info_rsp.result);
				_exit_ = 1;
				break;
			default:
				printf("Unexpected message received: 0x%x from mailbox 0x%x\n",
				       msg->msgno,
				       itc_sender(msg));
				_exit_ = 1;
				break;
		}
		itc_free(&msg);
	}

	itc_delete_mailbox(my_mbox);
	itc_exit();

	return 0;
}


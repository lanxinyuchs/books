#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include "ulh_lnh_msg.h"

#define CMD_TMO 2000

union itc_msg {
	uint32_t msgno;

	struct ulh_lnhmsg_info_req info_req;
	struct ulh_lnhmsg_info_rsp info_rsp;
};

char usage[] = "itclnh <linkhandler> [-l -d <link id> -n <link name> -m -r]\n"
	"Name of linkhandler mailbox/thread"
	" -l              display links (default)\n"
	" -d <link id>    display detailed information about a link\n"
	" -n <link name>  display detailed information about a link\n"
	" -m              display local endpoints\n"
	" -r              display remote endpoints\n";



int main(int argc, char **argv)
{
	union itc_msg *msg;
	itc_mbox_id_t lnh_id, my_id;
	int info_type = ULH_INFO_LINK_SUMMARY;
	long info_input = -1;
	char *lnhname, tmpname[128] = "";
	int opt, last = 0;

	if(argc < 2) {
		fprintf(stderr, "Too few arguments to itclnh\n%s\n",
			usage);
		return -1;
	}

	lnhname = argv[1];
	if(lnhname[0] == '-') {
		fprintf(stderr, "Argument 1 should be a linkhandler name not an option\n%s\n",
			usage);
		return -1;
	}

        while ((opt = getopt(argc, argv, "ld:n:mrh?")) != -1) {
		switch(opt) {
                case 'l':
			info_type = ULH_INFO_LINK_SUMMARY;
                        break;
                case 'd':
			info_type  = ULH_INFO_LINK_DETAILED;
			info_input = strtol(optarg, NULL, 0);
                        break;
                case 'n':
			info_type  = ULH_INFO_LINK_DETAILED;
			info_input = -1;
			strcpy(tmpname, optarg);
                        break;
                case 'm':
			info_type = ULH_INFO_LOCAL_EP;
                        break;
                case 'r':
			info_type = ULH_INFO_REMOTE_EP;
                        break;
                case '?':
                        printf("%s", usage);
			return 0;
                case 'h':
			printf("%s", usage);
			return 0;
                default:
			printf("%s", usage);
                        return 0;
                }
        }

	itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	my_id = itc_create_mailbox("itclnh", 0);

	lnh_id = itc_locate(lnhname);
	if(lnh_id == ITC_NO_ID) {
		printf("Linkhandler %s not found\n", lnhname);
		return -1;
	}

	itc_monitor(lnh_id, NULL);

	msg = itc_alloc(sizeof(struct ulh_lnhmsg_info_req) + strlen(tmpname),
			ULH_LNHMSG_INFO_REQ);
	msg->info_req.info_type = info_type;
	msg->info_req.info_input = info_input;
	strcpy(msg->info_req.info_name, tmpname);
	itc_send(&msg, lnh_id, ITC_MY_MBOX);

	while(!last) {
		msg = itc_receive(ITC_NOFILTER, CMD_TMO, ITC_FROM_ALL);
		if(msg == NULL) {
			printf("No response from linkhandler %s within %dms\n",
			       lnhname, CMD_TMO);
			return -1;
		}

		switch(msg->msgno) {
		case ITC_MONITOR_DEFAULT_NO:
			last = 1;
			break;
		case ULH_LNHMSG_INFO_RSP:
			if(msg->info_rsp.result == 0)
				printf("%s", msg->info_rsp.infotext);
			else
				printf("Linkhandler returned error: %d(%s)\n",
				       msg->info_rsp.result,
				       strerror(-msg->info_rsp.result));
			last = msg->info_rsp.last;
			break;
		default:
			if(itc_get_name(itc_sender(msg), tmpname, sizeof(tmpname)))
				printf("Unexpected message receive: 0x%x from mailbox 0x%x %s\n",
				       msg->msgno, itc_sender(msg), tmpname);
			else
				printf("Unexpected message receive: 0x%x from mailbox 0x%x\n",
				       msg->msgno, itc_sender(msg));
			last = 1;
			break;
		}
	}

	itc_delete_mailbox(my_id);
	itc_exit();

	return 0;
}

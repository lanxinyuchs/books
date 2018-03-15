#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include "ulh_lnh_msg.h"

#define CMD_TMO 2000
#define THROUGHPUT_DEFAULT_INTERVAL 1000
#define THROUGHPUT_MIN_INTERVAL     10
#define THROUGHPUT_MAX_INTERVAL     (60 * 60 * 1000)

union itc_msg {
	uint32_t msgno;

	struct ulh_lnhmsg_info_req           info_req;
	struct ulh_lnhmsg_info_rsp           info_rsp;
	struct ulh_lnhmsg_link_info_req link_info_req;
	struct ulh_lnhmsg_link_info_rsp link_info_rsp;
};

char usage[] = "itclnh <linkhandler> [-l -d <link id> -n <link name> -m -r -t <enable/disable/print> -i <timeout> ]\n"
	"Name of linkhandler mailbox/thread"
	" -l                display links (default)\n"
	" -d <link id>      display detailed information about a link\n"
	" -n <link name>    display detailed information about a link\n"
	" -m                display local endpoints\n"
	" -r                display remote endpoints\n"
	" -t enable         enable capturing throughput info, default timeout 1000ms\n"
	" -i <timeout>      timeout in ms for throughput info, timeout should be minimum of 10ms" \
                            " and maximum of 3600000ms (i.e 1hour), this should be used with \"-t enable\"\n"
	" -t disable        disable capturing throughput info\n"
	" -t print          display throughput info\n";


int get_thrput_option( char* thrput_option, int *info_type)
{
        if( !strcmp(thrput_option, "enable")) {
                *info_type = ULH_INFO_ENABLE_THROUGHPUT;
        } else if ( !strcmp(thrput_option, "print")) {
                *info_type = ULH_INFO_DISPLAY_THROUGHPUT;
        } else if ( !strcmp(thrput_option, "disable")) {
                *info_type = ULH_INFO_DISABLE_THROUGHPUT;
        } else {
                printf("Error: Unknown option specified\n");
                return 1;
        }
        return 0;
}

static void
handle_link_info_rsp(union itc_msg *msg, int info_type, char *tmpname)
{
        int i;
        union itc_msg *rec_msg = NULL;
        union itc_msg *msg_send = NULL;
        itc_mbox_id_t link_mboxid;
        bool first = false;
        int last = 0;

        for (i = 0; ((i< 32) && msg->link_info_rsp.link_mboxid[i]); i++) {

                msg_send = itc_alloc(sizeof(struct ulh_lnhmsg_info_req),
                                ULH_LNHMSG_INFO_REQ);
                msg_send->info_req.info_type = info_type;
                if ((i == 0) || first) {
                        msg_send->info_req.info_first = true;
                }else {
                        msg_send->info_req.info_first = false;
                }
                link_mboxid = msg->link_info_rsp.link_mboxid[i];
                itc_send(&msg_send, link_mboxid, ITC_MY_MBOX);

                last = 0;
                while (!last) {
                        rec_msg = itc_receive(ITC_NOFILTER, CMD_TMO, ITC_FROM_ALL);
                        if(rec_msg == NULL) {
                                printf("No response from link %d within %dms\n",
                                        link_mboxid, CMD_TMO);
                                first = true;
                                last = 1;
                                continue;
                        }
                        if(rec_msg->info_rsp.result == 0) {
                                printf("%s", rec_msg->info_rsp.infotext);
                        }
                        else {
                                printf("Link thread : 0x%x returned error: %d\n", itc_sender(rec_msg),
                                       rec_msg->info_rsp.result);
                        }
                        last = rec_msg->info_rsp.last;
                        itc_free(&rec_msg);
                }
        }
}

int main(int argc, char **argv)
{
        union itc_msg *msg;
        itc_mbox_id_t lnh_id, my_id;
        int info_type = ULH_INFO_LINK_SUMMARY;
        long info_input = -1, thrput_interval = THROUGHPUT_DEFAULT_INTERVAL;
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

        while ((opt = getopt(argc, argv, "ld:n:mrt:i:h?")) != -1) {
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
                        case 't':
                                if(!strcmp(lnhname,"ecmlnh")) {
                                        if(get_thrput_option(optarg, &info_type)) {
                                                return 0;
                                        }
                                } else {
                                        printf("This option is not available for %s\n", argv[1]);
                                        return 0;
                                }
                                break;
                        case 'i':
                                thrput_interval = strtol(optarg, NULL, 0);
                                if(thrput_interval < THROUGHPUT_MIN_INTERVAL ||
                                                thrput_interval > THROUGHPUT_MAX_INTERVAL) {
                                        printf("Error: invalid timeout value\n");
                                        return 0;
                                }
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

        msg = itc_alloc(sizeof(struct ulh_lnhmsg_link_info_req) + strlen(tmpname) + 1,
                        ULH_LNHMSG_LINK_INFO_REQ);
        msg->link_info_req.info_type = info_type;

        if (info_type == ULH_INFO_LINK_DETAILED){
                msg->link_info_req.info_input = info_input;
                strncpy(msg->link_info_req.info_name, tmpname, strlen(tmpname));
                msg->link_info_req.info_name[strlen(tmpname)] = '\0';
        }else if (info_type == ULH_INFO_ENABLE_THROUGHPUT) {
                msg->link_info_req.info_input = thrput_interval;
        }
        itc_send(&msg, lnh_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, CMD_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                printf("No response from linkhandler %s within %dms\n",
                        lnhname, CMD_TMO);
                return -1;
        }
        switch(msg->msgno) {
                case ULH_LNHMSG_LINK_INFO_RSP:
                        printf("%s\n", msg->link_info_rsp.infotext);
                        handle_link_info_rsp(msg, info_type, tmpname);
                        break;
                case ITC_MONITOR_DEFAULT_NO:
                        break;
                default:
                        if(itc_get_name(itc_sender(msg), tmpname, sizeof(tmpname)))
                                printf("Unexpected message receive: 0x%x from mailbox 0x%x %s\n",
                                                msg->msgno, itc_sender(msg), tmpname);
                        else
                                printf("Unexpected message receive: 0x%x from mailbox 0x%x\n",
                                                msg->msgno, itc_sender(msg));
                        break;
        }

        itc_delete_mailbox(my_id);
        itc_exit();

        return 0;
}

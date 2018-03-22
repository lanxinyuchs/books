#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include <pthread.h>

#include "itc.h"

#include "itc_linx.h"


#define LNHNAME "linx_lnh"

struct lnhs {
        struct lnhs *next;
        char name[1];
};

union itc_msg {
        uint32_t            msgno;

        struct add_linx_lnh add_linx_lnh;
};

static char helptext[] = "linxtest ITC linx linkhandler test command\n"
                         "[-m <Max mboxes>] -l <linkhandler path>, can be several\n";
static struct lnhs *lnhlist = NULL;

void printhelp(void)
{
        printf("%s", helptext);
}

int main(int argc, char *argv[])
{
        int opt, res;
        pthread_t lnh_tid;
        itc_mbox_id_t mbox_id, lnh_id;
        struct lnhs *lnh;
        int mboxes = 1000;
        char lnhname[] = LNHNAME;
        union itc_msg *msg;

        while ((opt = getopt(argc, argv, "m:l:")) != -1) {
                switch(opt) {
                case 'm':
                        mboxes = strtol(optarg, NULL, 0);
                        break;
                case 'l':
                        lnh = malloc(sizeof(struct lnhs) + strlen(optarg));
                        if(lnh == NULL) {
                                printf("No memory from malloc\n");
                                exit(-1);
                        }
                        strcpy(lnh->name, optarg);
                        lnh->next = lnhlist;
                        lnhlist = lnh;
                        break;
                default:
                        printhelp();
                        exit(-1);
                        break;
                }
        }

        if(itc_init(mboxes, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) != 0) {
                printf("itc_init failed, exiting\n");
                exit(-1);
        }
        mbox_id = itc_create_mailbox("linxtest", 0);

        res = pthread_create(&lnh_tid, NULL, linxlnh_main, lnhname);
        if(res != 0) {
                printf("linx_init error\n");
                exit(-1);
        }

        itc_locate_async(lnhname, NULL, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
        if(msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                printf("Unexpected message received 0x%x\n", msg->msgno);
                exit(-1);
        }
        lnh_id = itc_sender(msg);
        itc_free(&msg);

        for(lnh = lnhlist    ;
            lnh != NULL      ;
            lnh = lnh->next) {
                msg = itc_alloc((sizeof(struct add_linx_lnh) + strlen(lnh->name)), 
                                ADD_LINX_LNH);
                strcpy(msg->add_linx_lnh.lnhpath, lnh->name);
                itc_send(&msg, lnh_id, ITC_MY_MBOX);
        }

        pthread_join(lnh_tid, NULL);

        itc_delete_mailbox(mbox_id);
        itc_exit();

        return 0;
}

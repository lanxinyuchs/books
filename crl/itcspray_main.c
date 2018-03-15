#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <getopt.h>

#include "itc.h"
#include "itcspray.h"
#include "itcspray.sig"

#define MBOX_NAME_LEN 128

union itc_msg
{
  uint32_t                 sigNo;

  struct ItcSprayPrio      ItcSprayPrio;
  struct ItcSprayTest      ItcSprayTest;
  struct ItcSprayTestR     ItcSprayTestR;
  struct ItcSprayTestStart ItcSprayTestStart;
  struct ItcSprayTestReady ItcSprayTestReady;
};

struct itcspray_test {
        struct itcspray_test *next;

        itc_mbox_id_t         my_mbox;
        int                   loops;
        int                   datasize;
        int                   burst;
        int                   delay_time;
        int                   req_time;

        char                 *master_name;
        itc_mbox_id_t         master_mid;
        itc_monitor_id_t      master_monid;

        char                 *slave_name;
        itc_mbox_id_t         slave_mid;
        itc_monitor_id_t      slave_monid;

        unsigned long int     test_us;
};

struct started_threads {
        struct started_threads *next;
        pthread_t               tid;
        void                 *(*start_routine) (void *);
        char                    name[1];
};

struct alloc_scheme {
        itc_alloc_scheme alloc_scheme;
        union itc_scheme *scheme_par;
};

char helptext[] = "itcspray ITC spray test command\n"
                  "-l length      length, datasize to use in test\n"
                  "-c loops       count, loops to use in test\n"
                  "-d delay       delay time per loop to use in test\n"
                  "-b burst       burst messages per loop to use in test\n"
                  "-s slave       slave name, process is created\n"
                  "-m master      master name, process is created\n"
                  "-t             if defined a test is run,\n"
                  "               if not only slave and master processes are created\n"
                  "-p slave       slave name, process is not created\n"
                  "-o master      master name, process is not created\n"
                  "-a poolsize    Use pool as allocation scheme\n"
                  "-r maxlooptime Max looptime for success\n"
                  "-h             print this help text\n";

void printhelp(void)
{
        printf("%s", helptext);
}

int main(int argc, char *argv[])
{
        uint32_t itcloc[] = {1, ITC_SPRAY_TEST};
        uint32_t itcspraytest_ready[] = {2, ITC_SPRAY_TEST_READY, ITC_MONITOR_DEFAULT_NO};
        union itc_msg *msg;
        struct started_threads *thread_list = NULL;
        struct itcspray_test test_settings, *tmptest, *runtests = NULL;
        unsigned long looptime;
        struct started_threads *tmp, *tls;
        char *tmpname, mbox_name[MBOX_NAME_LEN];
        int i, opt, test_result=0, tests = 0;
        struct alloc_scheme alloc_scheme = { ITC_MALLOC, NULL };
        struct itc_pool_parameters pool_par;

        /* Setup default test settings */
        test_settings.next        = NULL;
        test_settings.loops       = 1000;
        test_settings.datasize    = 0;
        test_settings.burst       = 1;
        test_settings.delay_time  = 0;
        test_settings.req_time    = 0;
        test_settings.master_name = NULL;
        test_settings.slave_name  = NULL;


        while ((opt = getopt(argc, argv, "l:c:d:b:s:m:tp:a:o:r:h?")) != -1) {
                switch(opt) {
                case 'l':
                        test_settings.datasize = strtol(optarg, NULL, 0);
                        break;
                case 'c':
                        test_settings.loops = strtol(optarg, NULL, 0);
                        break;
                case 'd':
                        test_settings.delay_time = strtol(optarg, NULL, 0);
                        break;
                case 'b':
                        test_settings.burst = strtol(optarg, NULL, 0);
                        break;
                case 's': {
                        tmp = malloc(sizeof(struct started_threads) + strlen(optarg));
                        strcpy(tmp->name, optarg);
                        tmp->next = NULL;
                        tmp->start_routine = itcspray_slave;
                        test_settings.slave_name = tmp->name;

                        if(thread_list == NULL) {
                                thread_list = tmp;
                        } else {
                                tls = thread_list;
                                while(tls->next != NULL) {
                                        tls = tls->next;
                                }
                                tls->next = tmp;
                        }

                        break;
                }
                case 'm': {
                        tmp = malloc(sizeof(struct started_threads) + strlen(optarg));
                        strcpy(tmp->name, optarg);
                        tmp->next = NULL;
                        tmp->start_routine = itcspray_master;
                        test_settings.master_name = tmp->name;

                        if(thread_list == NULL) {
                                thread_list = tmp;
                        } else {
                                tls = thread_list;
                                while(tls->next != NULL) {
                                        tls = tls->next;
                                }
                                tls->next = tmp;
                        }

                        break;
                }
                case 't':
                        if(test_settings.master_name == NULL ||
                           test_settings.slave_name  == NULL) {
                                printf("Missing slave or master name\n");
                                exit(-1);
                        }
                        tmptest = malloc(sizeof(struct itcspray_test));
                        memset(tmptest, 0, sizeof(struct itcspray_test));
                        tmptest->loops       = test_settings.loops;
                        tmptest->datasize    = test_settings.datasize;
                        tmptest->burst       = test_settings.burst;
                        tmptest->delay_time  = test_settings.delay_time;
                        tmptest->req_time    = test_settings.req_time;
                        tmptest->master_name = test_settings.master_name;
                        tmptest->slave_name  = test_settings.slave_name;

                        tmptest->next        = runtests;
                        runtests             = tmptest;

                        break;
                case 'p': {
                        tmpname = malloc(strlen(optarg));
                        strcpy(tmpname, optarg);
                        test_settings.slave_name = tmpname;
                        break;
                }
                case 'o': {
                        tmpname = malloc(strlen(optarg));
                        strcpy(tmpname, optarg);
                        test_settings.master_name = tmpname;
                        break;
                }
                case 'a': {
                        pool_par.size = strtol(optarg, NULL, 0);
                        alloc_scheme.alloc_scheme = ITC_POOL;
                        alloc_scheme.scheme_par = (union itc_scheme *)&pool_par;
                        break;
                }
                case 'r': {
                        test_settings.req_time = strtol(optarg, NULL, 0);
                        break;
                }
                case '?':
                        printhelp();
                        exit(0);
                case 'h':
                        printhelp();
                        exit(0);
                default:
                        printhelp();
                        exit(-1);
                        break;
                }
        }

        if(itc_init(200, alloc_scheme.alloc_scheme,
                    alloc_scheme.scheme_par, 
                    ITC_NO_NAMESPACE, 0) != 0) {
                printf("ITC init failed, exiting\n");
                exit(-1);
        }

        tmp = thread_list;
        while(tmp != NULL) {
                pthread_create(&(tmp->tid), NULL, tmp->start_routine, tmp->name);

                tmp = tmp->next;
        }

        if(runtests != NULL) {

                /* Create my own mailbox */
                sprintf(mbox_name, "%s_command", test_settings.master_name);
                test_settings.my_mbox = itc_create_mailbox(mbox_name, 0);

                for(tmptest = runtests ;
                    tmptest != NULL    ;
                    tmptest = tmptest->next) {

                        /* hunt for master process */
                        msg = itc_alloc(sizeof( struct ItcSprayTest ), ITC_SPRAY_TEST);
                        itc_locate_async(tmptest->master_name, &msg, ITC_MY_MBOX);
                        msg = itc_receive(itcloc, ITC_NO_TMO, ITC_FROM_ALL);
                        if (msg == NULL)
                        {
                                printf("Error: Master process %s could not be reached.\n",
                                       tmptest->master_name);
                                exit(-1);
                        }
                        tmptest->master_mid = itc_sender(msg);
                        tmptest->master_monid = itc_monitor(tmptest->master_mid, NULL);
                        itc_free(&msg);

                        /* hunt for slave process */
                        msg = itc_alloc(sizeof( struct ItcSprayTest ), ITC_SPRAY_TEST);
                        itc_locate_async(tmptest->slave_name, &msg, ITC_MY_MBOX);
                        msg = itc_receive(itcloc, ITC_NO_TMO, ITC_FROM_ALL);
                        if (msg == NULL)
                        {
                                printf("Error: Slave process %s could not be reached.\n",
                                       tmptest->slave_name);
                                exit(-1);
                        }
                        tmptest->slave_mid = itc_sender(msg);
                        tmptest->slave_monid = itc_monitor(tmptest->slave_mid, NULL);
                        itc_free(&msg);

                        tests++;
                }

                tmptest = runtests;
                printf("OS signal loop test starting.\n");
                printf("spray master = %s (0x%08x)\n", tmptest->master_name, tmptest->master_mid);
                printf("spray slave  = %s (0x%08x)\n", tmptest->slave_name, tmptest->slave_mid);
                printf("loops = %d \n", tmptest->loops);
                printf("data size = %d bytes\n", tmptest->datasize);
                printf("burst = %d \n", tmptest->burst);
                printf("delay in loop = %d ms\n", tmptest->delay_time);
                printf("signal loops in process: %s\n", tmptest->master_name);

                for(tmptest = runtests ;
                    tmptest != NULL    ;
                    tmptest = tmptest->next) {

                        /* send request to start measure */
                        msg = itc_alloc(sizeof(struct ItcSprayTestStart), ITC_SPRAY_TEST_START);
                        msg->ItcSprayTestStart.slave_mbox   = tmptest->slave_mid;
                        msg->ItcSprayTestStart.loops_U32    = tmptest->loops;
                        msg->ItcSprayTestStart.dataSize_U32 = tmptest->datasize;
                        msg->ItcSprayTestStart.delayTime    = tmptest->delay_time;
                        msg->ItcSprayTestStart.burst_U32    = tmptest->burst;
                        itc_send(&msg, tmptest->master_mid, ITC_MY_MBOX);
                }

                for(i=0 ; i<tests ; i++) {
                        msg = itc_receive(itcspraytest_ready, ITC_NO_TMO, ITC_FROM_ALL);
                        switch(msg->sigNo) {
                                case ITC_MONITOR_DEFAULT_NO:
                                        printf("Lost contact with master or slave process, test aborted\n");
                                        i = tests;
                                        test_result = -1;
                                        break;
                                case ITC_SPRAY_TEST_READY:
                                        for(tmptest = runtests ;
                                            tmptest != NULL    ;
                                            tmptest = tmptest->next) {
                                                if(tmptest->master_mid == itc_sender(msg)) {
                                                        break;
                                                }
                                        }

                                        if(tmptest != NULL &&
                                           tmptest->test_us == 0) {
                                                tmptest->test_us = msg->ItcSprayTestReady.time;
                                                itc_unmonitor(tmptest->master_monid);
                                                itc_unmonitor(tmptest->slave_monid);
                                        } else {
                                                printf("2 Responses from the same test 0x%08x %s\n",
                                                       tmptest->master_mid, tmptest->master_name);
                                                i = tests;
                                                test_result = -1;
                                        }

                                        break;
                                default:
                                        printf("This is a bug, should never get here!!!\n");
                                        break;
                        }
                        itc_free(&msg);
                }

                i=0;
                for(tmptest = runtests ;
                    tmptest != NULL    ;
                    tmptest = tmptest->next) {

                        if(i != 0) {
                                printf("spray master = %s (0x%08x)\n", tmptest->master_name, tmptest->master_mid);
                                printf("spray slave  = %s (0x%08x)\n", tmptest->slave_name, tmptest->slave_mid);
                                printf("loops = %d \n", tmptest->loops);
                                printf("data size = %d bytes\n", tmptest->datasize);
                                printf("burst = %d \n", tmptest->burst);
                                printf("delay in loop = %d ms\n", tmptest->delay_time);
                                printf("signal loops in process: %s\n\n", tmptest->master_name);
                        }
                        if(tmptest->test_us != 0) {
                                printf("The test took %lu milliseconds.\n",
                                       tmptest->test_us / 1000);
                                looptime = tmptest->test_us / tmptest->loops;
                                if(tmptest->req_time != 0 &&
                                   looptime > tmptest->req_time) {
                                        test_result = (0 - looptime);
                                }
                                printf("Signalling loop time is then %lu microseconds.\n\n",
                                       looptime);
                        } else {
                                printf("Test %d not completed\n\n", i);
                        }
                        i++;
                }
        } else {
                tmp = thread_list;

                printf("Waiting for created threads to exit\n");
                while(tmp != NULL) {
                        pthread_join(tmp->tid, NULL);
                }
                printf("All created threads have exited so exit command\n");
                itc_exit();
                exit(0);
        }

        /* Cleanup when test completed */
        tmp = thread_list;
        while(tmp != NULL) {
                struct started_threads *old;

                old = tmp;
                pthread_cancel(old->tid);
                pthread_join(old->tid, NULL);
                tmp = tmp->next;
                free(old);
        }

        itc_delete_mailbox(test_settings.my_mbox);
        itc_exit();

        return test_result;
}

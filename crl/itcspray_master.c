#include <pthread.h>

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
/*#include <linux/time.h>*/

#include <arpa/inet.h>

#include "itc.h"
#include "itcspray.h"
#include "itcspray.sig"

#ifndef CLOCK_REALTIME
#define CLOCK_REALTIME 0
#endif

union itc_msg {
        uint32_t sigNo;

        struct ItcSprayTest      ItcSprayTest;
        struct ItcSprayTestR     ItcSprayTestR;
        struct ItcSprayTestStart ItcSprayTestStart;
        struct ItcSprayTestReady ItcSprayTestReady;
};

void *itcspray_master(void *name)
{
        union itc_msg *sigSend_pn, *sigRec_pn, *sigSendTest_pn, *sigRecTest_pn;
        static uint32_t ItcSprayTestStart_a[] = {1, ITC_SPRAY_TEST_START};
        struct timespec start_time, stop_time;
        int old;

        int delayTime;
        int loop, i, j, burst, size;
        itc_mbox_id_t mid;
        itc_mbox_id_t my_mbox;

        my_mbox = itc_create_mailbox((char *)name, 0);

        if(pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old) != 0) {
                exit(-1);
        }

        if(pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old) != 0) {
                exit(-1);
        }

        /* Outer loop to receive command to start perfmeter */
        for (;;) {
                sigRec_pn = itc_receive(ItcSprayTestStart_a, ITC_NO_TMO, ITC_FROM_ALL);

                loop      =  (int) sigRec_pn->ItcSprayTestStart.loops_U32;
                burst     =  (int) sigRec_pn->ItcSprayTestStart.burst_U32;
                size      = sigRec_pn->ItcSprayTestStart.dataSize_U32 + sizeof(struct ItcSprayTest);
                mid       = sigRec_pn->ItcSprayTestStart.slave_mbox;
                delayTime = sigRec_pn->ItcSprayTestStart.delayTime;

                if ( delayTime == 0) {
                        /* use a version of inner loop without any delay system calls */
                        clock_gettime(CLOCK_REALTIME, &start_time);
                        for (i = 0; i < loop; i=i+burst) {
                                for (j=0; j<burst; j++) {
                                        sigSendTest_pn = itc_alloc(size, ITC_SPRAY_TEST);
                                        sigSendTest_pn->ItcSprayTest.sigSize_U32 = htonl(size);
                                        sigSendTest_pn->ItcSprayTest.seq_no      = htonl(i + j);
                                        itc_send(&sigSendTest_pn, mid, ITC_MY_MBOX);
                                }
                                for (j=0; j<burst; j++) {
                                        sigRecTest_pn = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                                        if(sigRecTest_pn->sigNo != ITC_SPRAY_TEST_R) {
                                                printf("Unexpected message received 0x%x\n",
                                                       sigRecTest_pn->sigNo);
                                                j--;
                                        } else if(ntohl(sigRecTest_pn->ItcSprayTestR.seq_no) != (i + j)) {
                                                printf("Sequence number mismatch expected %d got %d\n",
                                                       (i + j), sigRecTest_pn->ItcSprayTestR.seq_no);
                                        }
                                        itc_free(&sigRecTest_pn);
                                }
                        }
                        clock_gettime(CLOCK_REALTIME, &stop_time);
                } else {
                        /* inner loop with delay system call */
                        clock_gettime(CLOCK_REALTIME, &start_time);
                        for (i = 0; i < loop; i=i+burst) {
                                for (j=0; j<burst; j++) {
                                        sigSendTest_pn = itc_alloc(size, ITC_SPRAY_TEST);
                                        sigSendTest_pn->ItcSprayTest.sigSize_U32 = htonl(size);
                                        sigSendTest_pn->ItcSprayTest.seq_no      = htonl(i + j);
                                        itc_send(&sigSendTest_pn, mid, ITC_MY_MBOX);
                                }
                                for (j=0; j<burst; j++) {
                                        sigRecTest_pn = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                                        if(sigRecTest_pn->sigNo != ITC_SPRAY_TEST_R) {
                                                printf("Unexpected message received 0x%x\n",
                                                       sigRecTest_pn->sigNo);
                                                j--;
                                        } else if(ntohl(sigRecTest_pn->ItcSprayTestR.seq_no) != (i + j)) {
                                                printf("Sequence number mismatch expected %d got %d\n",
                                                       (i + j), sigRecTest_pn->ItcSprayTestR.seq_no);
                                        }
                                        itc_free(&sigRecTest_pn);
                                }
                                /* Delay time is in ms */
                                usleep(delayTime*1000);
                        }
                        clock_gettime(CLOCK_REALTIME, &stop_time);
                }

                sigSend_pn = itc_alloc(sizeof(struct ItcSprayTestReady), ITC_SPRAY_TEST_READY);
                /* I assume that this clock will not wrap for now since I do not know how to deal with it */
                sigSend_pn->ItcSprayTestReady.time = ((stop_time.tv_sec - start_time.tv_sec) * 1000000 +
                                                      stop_time.tv_nsec/1000 - start_time.tv_nsec/1000);
                itc_send(&sigSend_pn, itc_sender(sigRec_pn), ITC_MY_MBOX);
                itc_free(&sigRec_pn);
        }

        itc_delete_mailbox(my_mbox);
}

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <pthread.h>

#include <arpa/inet.h>

#include "itc.h"
#include "itcspray.h"
#include "itcspray.sig"

union itc_msg {
        uint32_t sigNo;

        struct ItcSprayTest  ItcSprayTest;
        struct ItcSprayTestR ItcSprayTestR;
};

void *itcspray_slave(void *name)
{
        union itc_msg *sigSendTest_pn, *sigRecTest_pn;
        itc_mbox_id_t my_mbox, mid;
        int old;

        my_mbox = itc_create_mailbox((char *)name, 0);

        if(pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old) != 0) {
                exit(-1);
        }

        if(pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old) != 0) {
                exit(-1);
        }

        for (;;)
        {
                sigRecTest_pn = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                switch (sigRecTest_pn->sigNo)
                {
                        case ITC_SPRAY_TEST:
                        {
                                mid = itc_sender(sigRecTest_pn);
                                sigSendTest_pn = itc_alloc(ntohl(sigRecTest_pn->ItcSprayTest.sigSize_U32),
                                                           ITC_SPRAY_TEST_R);
                                sigSendTest_pn->ItcSprayTestR.seq_no = htonl(ntohl(sigRecTest_pn->ItcSprayTest.seq_no));
                                itc_free(&sigRecTest_pn);
                                itc_send(&sigSendTest_pn, mid, ITC_MY_MBOX);
                                break;
                        }
                        default:
                                printf("Unexpected message received 0x%x\n",
                                       sigRecTest_pn->sigNo);
                                itc_free(&sigRecTest_pn);
                                break;
                }
        }

        itc_delete_mailbox(my_mbox);

        return NULL;
}

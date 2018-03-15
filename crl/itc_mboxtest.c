#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>

#include "itc.h"
#include "itc_system.h"

#include "itcspray.h"
#include "itcspray.sig"

#include "itc_ct.h"

#define MAX_TESTS 10
#define MAX_NAME_LEN 32

#define REC_TMO       15000

#define FILTERTEST_MSGNO 0xA0000

union itc_msg
{
        uint32_t                     sigNo;
        uint32_t                     msgno;

        struct ItcSprayPrio          ItcSprayPrio;
	struct ItcSprayTest          ItcSprayTest;
        struct ItcSprayTestR         ItcSprayTestR;
        struct ItcSprayTestStart     ItcSprayTestStart;
        struct ItcSprayTestReady     ItcSprayTestReady;

        struct createshared_mbox     createshared_mbox;
        struct deleteshared_mbox     deleteshared_mbox;
        struct rxshared              rxshared;
        struct rxshared_repl         rxshared_repl;

        struct genmsg                genmsg;

	struct itc_mbox_added        itc_mbox_added;
	struct itc_mbox_removed      itc_mbox_removed;
	struct itc_locate_unresolved itc_locate_unresolved;
};

struct thr_data {
        char          name[ITC_NAME_MAXLEN];
        itc_mbox_id_t static_id;
};

struct mbox_test {
        int        no_tests;
        uint32_t   loops;
        uint32_t   data_size;
        uint32_t   delay_time;
        uint32_t   burst;
        uint32_t   max_looptime;
};

static struct mbox_test mbox_tests[] = { {  1,  100,    20, 0,   1, 1000 },
                                         {  2,   10,   100, 0,  10, 1000 },
                                         {  3,   10,  1000, 0,  10, 1000 },
                                         {  4,   10,  8000, 0,  10, 1000 },
                                         {  5,   10,   100, 0,  10, 1000 },
                                         {  6,   10,  1000, 0,  10, 1000 },
                                         {  7,  100,    20, 0,   1, 1000 },
                                         {  8,  100,   100, 0,   1, 1000 },
                                         {  9,  100,   100, 0, 100, 1000 },
                                         { 10, 1000,   100, 0,  10, 1000 },
                                         {  0,    0,     0, 0,   0,    0 } };

static int test_mboxes(int       no_tests,
                       char     *master_name,
                       char     *slave_name,
                       uint32_t  loops,
                       uint32_t  data_size,
                       uint32_t  delay_time,
                       uint32_t  burst,
                       uint32_t  max_looptime)
{
        union itc_msg *msg;
        uint32_t itcloc[] = {1, ITC_LOCATE_DEFAULT_NO};
        uint32_t itcspraytest_ready[] = {2, ITC_SPRAY_TEST_READY,
                                         ITC_MONITOR_DEFAULT_NO};
        pthread_t master_tid[MAX_TESTS], slave_tid[MAX_TESTS];
        itc_mbox_id_t master_mid[MAX_TESTS], slave_mid[MAX_TESTS];
        itc_monitor_id_t master_monid[MAX_TESTS], slave_monid[MAX_TESTS];
        int i, j, looptime, ok_tests = 0, result = 0;
        char *master_cname[MAX_TESTS], *slave_cname[MAX_TESTS];

        if(no_tests > MAX_TESTS) {
                return -__LINE__;
        }

        /* Start test processes */
        for(i=0 ; i<no_tests ; i++) {
                /* Setup master process */
                master_cname[i] = malloc(strlen(master_name) + 4);
                if(master_cname[i] == NULL) {
                        return -__LINE__;
                }
                sprintf(master_cname[i], "%s_%d", master_name, i);
                pthread_create(&master_tid[i], NULL, itcspray_master, master_cname[i]);
                itc_locate_async(master_cname[i], NULL, ITC_MY_MBOX);
                msg = itc_receive(itcloc, ITC_NO_TMO, ITC_FROM_ALL);
                if (msg == NULL)
                {
                        return -__LINE__;
                }
                master_mid[i] = itc_sender(msg);
                master_monid[i] = itc_monitor(master_mid[i], NULL);
                itc_free(&msg);

                /* Setup slave process */
                slave_cname[i] = malloc(strlen(slave_name) + 4);
                if(slave_cname[i] == NULL) {
                        return -__LINE__;
                }
                sprintf(slave_cname[i], "%s_%d", slave_name, i);
                pthread_create(&slave_tid[i], NULL, itcspray_slave, slave_cname[i]);
                itc_locate_async(slave_cname[i], NULL, ITC_MY_MBOX);
                msg = itc_receive(itcloc, ITC_NO_TMO, ITC_FROM_ALL);
                if (msg == NULL)
                {
                        return -__LINE__;
                }
                slave_mid[i] = itc_sender(msg);
                slave_monid[i] = itc_monitor(slave_mid[i], NULL);
                itc_free(&msg);
        }

        /* Start test */
        for(i=0 ; i<no_tests ; i++) {
                /* send request to start measure */
                msg = itc_alloc(sizeof(struct ItcSprayTestStart), ITC_SPRAY_TEST_START);
                msg->ItcSprayTestStart.slave_mbox   = slave_mid[i];
                msg->ItcSprayTestStart.loops_U32    = loops;
                msg->ItcSprayTestStart.dataSize_U32 = data_size;
                msg->ItcSprayTestStart.delayTime    = delay_time;
                msg->ItcSprayTestStart.burst_U32    = burst;
                itc_send(&msg, master_mid[i], ITC_MY_MBOX);
        }

        /* Collect results */
        for(i=0 ; i<no_tests ; i++) {
                msg = itc_receive(itcspraytest_ready, ITC_NO_TMO, ITC_FROM_ALL);
                switch(msg->sigNo) {
                        case ITC_MONITOR_DEFAULT_NO:
                                for(j=0 ; j<MAX_TESTS ; j++) {
                                        if(itc_sender(msg) == master_mid[j]) {
                                                master_tid[j] = 0;
                                        } else if(itc_sender(msg) == slave_mid[j]) {
                                                slave_tid[j] = 0;
                                        }
                                }

                                result = -__LINE__;
                                break;
                        case ITC_SPRAY_TEST_READY:
                                ok_tests++;
                                looptime = msg->ItcSprayTestReady.time / loops;
                                if(looptime > max_looptime) {
                                        result = -looptime;
                                }
                                break;
                        default:
                                result = -__LINE__;
                                break;
                }
                itc_free(&msg);
        }

        if(ok_tests != no_tests) {
                result = -__LINE__;
        }

        /* Cleanup after test */
        for(i=0 ; i<no_tests ; i++) {
                itc_unmonitor(master_monid[i]);
                itc_unmonitor(slave_monid[i]);

		pthread_cancel(master_tid[i]);
		pthread_join(master_tid[i], NULL);

		pthread_cancel(slave_tid[i]);
		pthread_join(slave_tid[i], NULL);
        }

        return result;
}

static int test_getownname(void)
{
	char myname[ITC_NAME_MAXLEN];

	if(itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) != 0)
		return -__LINE__;

	if(itc_create_mailbox("test_getownname", 0) == ITC_NO_ID)
		return -__LINE__;

	if(!itc_get_name(ITC_MY_MBOX, myname, ITC_NAME_MAXLEN) ||
	   (strcmp(myname, "test_getownname") != 0))
		return -__LINE__;

	itc_delete_mailbox(ITC_MY_MBOX);
	itc_exit();

	return 0;
}

static int test_msgremove(int loops)
{
	itc_mbox_id_t mbox_id;
        union itc_msg **msg, *rxmsg, *txmsg;
        int i;

        if(loops < 5) {
                /* Not sufficient loops requested return error */
                return -__LINE__;
        }

        msg = malloc(sizeof(union itc_msg *) * loops);
        if(msg == NULL) {
                return -__LINE__;
        }
        memset(msg, 0, (sizeof(union itc_msg *) * loops));

	mbox_id = itc_current_mbox();
	if(mbox_id == ITC_NO_ID) {
		return -__LINE__;
	}

        msg[0] = itc_alloc(sizeof(uint32_t), MBOX_TEST_MSG);
        txmsg = msg[0];
        itc_send(&txmsg, mbox_id, ITC_MY_MBOX);

        rxmsg = itc_remove(ITC_MY_MBOX, msg[0]);
        if(rxmsg != msg[0]) {
                if(rxmsg != NULL) {
                        itc_free(&rxmsg);
                }
                return -__LINE__;
        }
        itc_free(&rxmsg);

        rxmsg = itc_receive(ITC_NOFILTER, 10, ITC_FROM_ALL);
        if(rxmsg != NULL) {
                itc_free(&rxmsg);
                return -__LINE__;
        }

        for(i=0 ; i<loops ; i++) {
                msg[i] = itc_alloc(sizeof(uint32_t), MBOX_TEST_MSG);
                txmsg = msg[i];
                itc_send(&txmsg, mbox_id, ITC_MY_MBOX);
        }

        rxmsg = itc_remove(ITC_MY_MBOX, msg[0]);
        if(rxmsg != msg[0]) {
                itc_free(&rxmsg);
                return -__LINE__;
        }
        itc_free(&rxmsg);

        rxmsg = itc_remove(ITC_MY_MBOX, msg[loops/2]);
        if(rxmsg != msg[loops/2]) {
                itc_free(&rxmsg);
                return -__LINE__;
        }
        itc_free(&rxmsg);

        rxmsg = itc_remove(ITC_MY_MBOX, msg[loops-1]);
        if(rxmsg != msg[loops-1]) {
                itc_free(&rxmsg);
                return -__LINE__;
        }
        itc_free(&rxmsg);

        for(i=0 ; i<loops ; i++) {
                if(!(i == 0         ||
                     i == (loops/2) ||
                     i == (loops-1))) {
                        rxmsg = itc_receive(ITC_NOFILTER, 10, ITC_FROM_ALL);
                        if(rxmsg == NULL) {
                                return -__LINE__;
                        } else if(rxmsg->sigNo != MBOX_TEST_MSG) {
                                itc_free(&rxmsg);
                                return -__LINE__;
                        } else if(rxmsg != msg[i]) {
                                itc_free(&rxmsg);
                                return -__LINE__;
                        }
                        itc_free(&rxmsg);
                }
        }

        rxmsg = itc_receive(ITC_NOFILTER, 10, ITC_FROM_ALL);
        if(rxmsg != NULL) {
                itc_free(&rxmsg);
                return -__LINE__;
        }

        return 0;
}

static int test_mboxfd(int loops)
{
	itc_mbox_id_t mbox_id;
	union itc_msg *msg;
        int mbox_fd, res, i;
        fd_set fdset;
	struct timeval us100_tmo = { 0, 100000 };

	mbox_id = itc_current_mbox();
	if(mbox_id == ITC_NO_ID) {
		return -__LINE__;
	}

        mbox_fd = itc_get_fd();
        if(mbox_fd == ITC_NO_ID) {
                return -__LINE__;
        }

        /* Check that select works as intended for 1 message */
        msg = itc_alloc(sizeof(uint32_t), MBOX_TEST_MSG);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        FD_ZERO(&fdset);
        FD_SET(mbox_fd, &fdset);

        res = select((mbox_fd + 1), &fdset, NULL, NULL, &us100_tmo);
        if(res == -1) {
                return -__LINE__;
        } else if(res != 1) {
                return -__LINE__;
        }

        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg == NULL) {
                return -__LINE__;
        }
        itc_free(&msg);

        /* Check that select times out when the mailbox is empty */
        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        res = select((mbox_fd + 1), &fdset, NULL, NULL, &us100_tmo);
        if(res == -1) {
                return -__LINE__;
        } else if(res != 0) {
                return -__LINE__;
        }

        /* Check that select works as intended for several messages */
        for(i=0 ; i<loops ; i++) {
                msg = itc_alloc(sizeof(uint32_t), MBOX_TEST_MSG);
                itc_send(&msg, mbox_id, ITC_MY_MBOX);
        }

        for(i=0 ; i<loops ; i++) {
                FD_ZERO(&fdset);
                FD_SET(mbox_fd, &fdset);

                res = select((mbox_fd + 1), &fdset, NULL, NULL, &us100_tmo);
                if(res == -1) {
                        return -__LINE__;
                } else if(res != 1) {
                        return -__LINE__;
                }

                msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }

        /* Check that the fd is empty after all messages have been drained */
        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        res = select((mbox_fd + 1), &fdset, NULL, NULL, &us100_tmo);
        if(res == -1) {
                return -__LINE__;
        } else if(res != 0) {
                return -__LINE__;
        }

        return 0;
}

static void create_shared(char *name)
{
        itc_create_mailbox(name, ITC_MBOX_SHARED);
}

static void delete_shared(itc_mbox_id_t mbox_id)
{
        itc_delete_mailbox(mbox_id);
}

static void rxshared(itc_mbox_id_t mbox_id,
                     int nr,
                     itc_mbox_id_t repl_id)
{
        union itc_msg *msg;
        int i;

        for(i=0 ; i<nr ; i++)
        {
                msg = itc_receive_mbox(mbox_id, ITC_NOFILTER, ITC_NO_TMO);
                itc_send(&msg, itc_sender(msg), mbox_id);
        }

        msg = itc_alloc(sizeof(struct rxshared_repl), RXSHARED_REPL);
        msg->rxshared_repl.success = true;
        itc_send(&msg, repl_id, ITC_MY_MBOX);
}

static void generate_msgs(uint32_t msgno,
                          int nr,
                          int dly,
                          itc_mbox_id_t mbox_id)
{
        union itc_msg *msg;
        int i;

        usleep(dly*1000);
        for(i=0 ; i<nr ; i++) {
                msg = itc_alloc(sizeof(uint32_t), (msgno + i));
                itc_send(&msg, mbox_id, ITC_MY_MBOX);
        }
}

static void *sharedmbox_thread(void *data)
{
        union itc_msg *msg;
        itc_mbox_id_t mbox_id;
        bool cont_thread = true;
        struct thr_data *thr = data;

#ifdef ITC_STAT_MBOXID
        if(thr->static_id == 0) {
                mbox_id = itc_create_mailbox(thr->name, 0);
        } else {
                mbox_id = itc_create_mailbox(thr->name,
                                             ITC_MBOX_SET_STAT_ID(thr->static_id));
        }
#else
        mbox_id = itc_create_mailbox(thr->name, 0);
#endif

        free(thr);

        while(cont_thread) {
                msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                switch(msg->msgno) {
                case CREATESHARED_MBOX:
                        create_shared(msg->createshared_mbox.name);
                        itc_free(&msg);
                        break;
                case DELETESHARED_MBOX:
                        delete_shared(msg->deleteshared_mbox.mbox_id);
                        itc_free(&msg);
                        break;
                case RXSHARED:
                        rxshared(msg->rxshared.mbox_id,
                                 msg->rxshared.nr,
                                 itc_sender(msg));
                        itc_free(&msg);
                        break;
                case GENMSG:
                        generate_msgs(msg->genmsg.genmsgno,
                                      msg->genmsg.nr,
                                      msg->genmsg.dly,
                                      itc_sender(msg));
                        itc_free(&msg);
                        break;
                case STOP_THREAD:
                        cont_thread = false;
                        itc_free(&msg);
                        break;
                case KILL_THREAD:
                        itc_free(&msg);
                        return NULL;
                        break;
                default:
                        itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
                        break;
                }
        }

        itc_delete_mailbox(mbox_id);

        return NULL;
}

static int create_sharedthread(char *name,
                               itc_mbox_id_t static_id,
                               itc_mbox_id_t *mbox_id,
                               pthread_t *tid)
{
        union itc_msg *msg;
        struct thr_data *thr;
        static uint32_t loc_msgno = LOC_BASE;
        static itc_mbox_id_t last_id = ITC_NO_ID;
        static union itc_msg *last_msg = NULL;

        thr = malloc(sizeof(struct thr_data));
        if(thr == NULL) {
                return -__LINE__;
        }

        strcpy(thr->name, name);
        thr->static_id = static_id;
        pthread_create(tid, NULL, sharedmbox_thread, thr);

        msg = itc_alloc(sizeof(uint32_t), loc_msgno);
        itc_locate_async(name, &msg, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != loc_msgno) {
                return -__LINE__;
        }

        *mbox_id = itc_sender(msg);
        if(*mbox_id == last_id) {
                return -__LINE__;
        }
        last_id = *mbox_id;

        if(last_msg)
                itc_free(&last_msg);
        last_msg = msg;

        msg = itc_alloc(sizeof(uint32_t), *mbox_id);
        itc_monitor(*mbox_id, &msg);
        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        loc_msgno++;

        return 0;
}

static int test_sharedmbox(char *mboxname1, char *mboxname2, char *sharedname, int nr)
{
        itc_mbox_id_t mbox_id1, mbox_id2, shared_id;
        pthread_t tid1, tid2;
        union itc_msg *msg;
        uint32_t pingmsg[] = { 1, PING_THREAD };
        int i, res;

        res = create_sharedthread(mboxname1, 0, &mbox_id1, &tid1);
        if(res != 0) {
                return (res - 10000);
        }

        res = create_sharedthread(mboxname2, 0, &mbox_id2, &tid2);
        if(res != 0) {
                return (res - 20000);
        }

        msg = itc_alloc((sizeof(struct createshared_mbox) + strlen(sharedname)),
                        CREATESHARED_MBOX);
        strcpy(msg->createshared_mbox.name, sharedname);
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);

        itc_locate_async(sharedname, ITC_NOFILTER, ITC_MY_MBOX);
        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != ITC_LOCATE_DEFAULT_NO) {
                return -__LINE__;
        }
        shared_id = itc_sender(msg);
        itc_free(&msg);

        itc_monitor(shared_id, NULL);
        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(struct rxshared), RXSHARED);
        msg->rxshared.mbox_id = shared_id;
        msg->rxshared.nr = nr;
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);

        msg = itc_alloc(sizeof(struct rxshared), RXSHARED);
        msg->rxshared.mbox_id = shared_id;
        msg->rxshared.nr = nr;
        itc_send(&msg, mbox_id2, ITC_MY_MBOX);

        for(i=0 ; i<(nr*2) ; i++) {
                msg = itc_alloc(sizeof(uint32_t), PING_THREAD);
                itc_send(&msg, shared_id, ITC_MY_MBOX);
        }
        for(i=0 ; i<(nr*2) ; i++) {
                msg = itc_receive(pingmsg, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }
        for(i=0 ; i<2 ; i++) {
                msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL ||
                   msg->msgno != RXSHARED_REPL) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }


        for(i=0 ; i<(nr*2) ; i++) {
                msg = itc_alloc(sizeof(uint32_t), PING_THREAD);
                itc_send(&msg, shared_id, ITC_MY_MBOX);
        }
        msg = itc_alloc(sizeof(struct rxshared), RXSHARED);
        msg->rxshared.mbox_id = shared_id;
        msg->rxshared.nr = nr;
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);

        msg = itc_alloc(sizeof(struct rxshared), RXSHARED);
        msg->rxshared.mbox_id = shared_id;
        msg->rxshared.nr = nr;
        itc_send(&msg, mbox_id2, ITC_MY_MBOX);

        for(i=0 ; i<(nr*2) ; i++) {
                msg = itc_receive(pingmsg, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }
        for(i=0 ; i<2 ; i++) {
                msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL ||
                   msg->msgno != RXSHARED_REPL) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }


        msg = itc_alloc(sizeof(struct deleteshared_mbox), DELETESHARED_MBOX);
        msg->deleteshared_mbox.mbox_id = shared_id;
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);


        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != ITC_MONITOR_DEFAULT_NO) {
                return -__LINE__;
        }
        itc_free(&msg);

        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                return -__LINE__;
        }

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id1, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != mbox_id1) {
                return -__LINE__;
        }
        itc_free(&msg);


        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id2, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != mbox_id2) {
                return -__LINE__;
        }
        itc_free(&msg);


        return 0;
}

static int filter_test(itc_mbox_id_t mbox_id,
                       uint32_t      msgno,
                       int           nr_msgs,
                       int           nr_filter,
                       uint32_t      *filter,
                       int           nr_recvd)
{
        union itc_msg *msg;
        uint32_t rxfilter[5];
        int act_nrfilter, i;

        if(nr_recvd > nr_msgs) {
                return -__LINE__;
        }

        act_nrfilter = abs(nr_filter);
        if(act_nrfilter > 4) {
                return -__LINE__;
        }

        rxfilter[0] = nr_filter;
        for(i=0 ; i<act_nrfilter ; i++) {
                rxfilter[i+1] = filter[i];
        }


        msg = itc_alloc(sizeof(struct genmsg), GENMSG);
        msg->genmsg.genmsgno = msgno;
        msg->genmsg.nr       = nr_msgs;
        msg->genmsg.dly      = 10;
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        for(i=0 ; i<nr_recvd ; i++) {
                msg = itc_receive(rxfilter, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                }
        }
        msg = itc_receive(rxfilter, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        for(i=0 ; i<(nr_msgs - nr_recvd) ; i++) {
                msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                }
        }
        msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
        if(msg != NULL) {
                itc_free(&msg);
                return -__LINE__;
        }

        return 0;
}

static int test_filters(char *mboxname)
{
        union itc_msg *msg;
        itc_mbox_id_t mbox_id;
        pthread_t tid;
        uint32_t filter[5];
        int res;

        res = create_sharedthread(mboxname, 0, &mbox_id, &tid);
        if(res != 0) {
                return (res - 10000);
        }

        filter[0] = (FILTERTEST_MSGNO + 1);
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 2, 1, filter, 1);
        if(res < 0) {
                return (res - 20000);
        }

        filter[0] = FILTERTEST_MSGNO;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 2, 1, filter, 1);
        if(res < 0) {
                return (res - 30000);
        }

        filter[0] = FILTERTEST_MSGNO;
        filter[1] = FILTERTEST_MSGNO + 3;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 10, 2, filter, 2);
        if(res < 0) {
                return (res - 40000);
        }

        filter[0] = FILTERTEST_MSGNO;
        filter[1] = FILTERTEST_MSGNO + 3;
        filter[2] = FILTERTEST_MSGNO + 4;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 10, 3, filter, 3);
        if(res < 0) {
                return (res - 50000);
        }

        filter[0] = FILTERTEST_MSGNO;
        filter[1] = FILTERTEST_MSGNO + 3;
        filter[2] = FILTERTEST_MSGNO + 5;
        filter[3] = FILTERTEST_MSGNO + 7;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 10, 4, filter, 4);
        if(res < 0) {
                return (res - 60000);
        }



        filter[0] = (FILTERTEST_MSGNO + 1);
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 2, -1, filter, 1);
        if(res < 0) {
                return (res - 70000);
        }

        filter[0] = FILTERTEST_MSGNO;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 2, -1, filter, 1);
        if(res < 0) {
                return (res - 80000);
        }

        filter[0] = FILTERTEST_MSGNO;
        filter[1] = FILTERTEST_MSGNO + 3;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 10, -2, filter, 8);
        if(res < 0) {
                return (res - 90000);
        }

        filter[0] = FILTERTEST_MSGNO;
        filter[1] = FILTERTEST_MSGNO + 3;
        filter[2] = FILTERTEST_MSGNO + 4;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 10, -3, filter, 7);
        if(res < 0) {
                return (res - 100000);
        }

        filter[0] = FILTERTEST_MSGNO;
        filter[1] = FILTERTEST_MSGNO + 3;
        filter[2] = FILTERTEST_MSGNO + 5;
        filter[3] = FILTERTEST_MSGNO + 7;
        res = filter_test(mbox_id, FILTERTEST_MSGNO, 10, -4, filter, 6);
        if(res < 0) {
                return (res - 110000);
        }



        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != mbox_id) {
                return -__LINE__;
        }
        itc_free(&msg);

        return 0;
}

#ifdef ITC_STAT_MBOXID
static int test_staticid(char *mboxname, itc_mbox_id_t static_id, int nr)
{
        itc_mbox_id_t mbox_id;
        pthread_t tid;
        union itc_msg *msg;
        uint32_t pingmsg[] = { 1, PING_THREAD };
        int i, res;

        res = create_sharedthread(mboxname, static_id, &mbox_id, &tid);
        if(res != 0) {
                return (res - 10000);
        }

        if(mbox_id != itc_get_real_mbox(static_id)) {
                return -__LINE__;
        }

        for(i=0 ; i<nr ; i++) {
                msg = itc_alloc(sizeof(uint32_t), PING_THREAD);
                itc_send(&msg, mbox_id, ITC_MY_MBOX);
        }
        for(i=0 ; i<nr ; i++) {
                msg = itc_receive(pingmsg, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }

        for(i=0 ; i<nr ; i++) {
                msg = itc_alloc(sizeof(uint32_t), PING_THREAD);
                itc_send(&msg, static_id, ITC_MY_MBOX);
        }
        for(i=0 ; i<nr ; i++) {
                msg = itc_receive(pingmsg, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }

        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL ||
           msg->msgno != mbox_id) {
                return -__LINE__;
        }
        itc_free(&msg);

        return 0;
}
#endif

static int test_maxmboxes(int mboxes, int loops)
{
	itc_mbox_id_t *mboxids, mbox_id, last_id = ITC_NO_ID;
        int i, j, res, stop_idx, create_idx;
        char name[25];
        pthread_t tid;
        union itc_msg *msg;

        mboxids = malloc(mboxes * sizeof(itc_mbox_id_t));
        if(mboxids == NULL) {
                return -__LINE__;
        }

        if(itc_init((mboxes + 1), ITC_MALLOC, NULL,
                    ITC_NO_NAMESPACE, 0) != 0) {
                return -__LINE__;
        }

	mbox_id = itc_create_mailbox("maxtest_mbox", 0);

        for(i=0 ; i<mboxes ; i++) {
                sprintf(name, "test_mbox_%d", i);
                res = create_sharedthread(name, 0, &mboxids[i], &tid);
                if(res < 0) {
                        return (res - 10000);
                }
                if(last_id == mboxids[i]) {
                        return -__LINE__;
                }
                last_id = mboxids[i];
        }

        for(i=0 ; i<mboxes ; i++) {
                msg = itc_alloc(sizeof(uint32_t),
                                (0x12340000 + i));
                itc_send(&msg, mboxids[i], ITC_MY_MBOX);
                msg = itc_receive(ITC_NOFILTER,
                                  REC_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        return -__LINE__;
                } else if(msg->msgno != (0x12340000 + i)) {
                        itc_free(&msg);
                        return -__LINE__;
                }
                itc_free(&msg);
        }

        stop_idx = 0;
        create_idx = 0;
        for(i=0 ; i<loops ; i++) {

                for(j=0 ; j<(mboxes/3) ; j++) {
                        msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
                        itc_send(&msg, mboxids[stop_idx], ITC_MY_MBOX);

                        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
                        if(msg == NULL ||
                           msg->msgno != mboxids[stop_idx]) {
                                return -__LINE__;
                        }
                        itc_free(&msg);
                        mboxids[stop_idx] = ITC_NO_ID;
                        stop_idx++;
                        if(stop_idx == mboxes) {
                                stop_idx = 0;
                        }
                }

                msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
                if(msg != NULL) {
                        return -__LINE__;
                }

                last_id = ITC_NO_ID;
                for(j=0 ; j<(mboxes/3) ; j++) {
                        if(mboxids[create_idx] != ITC_NO_ID) {
                                return -__LINE__;
                        }
                        sprintf(name, "recreated_mbox_%d", create_idx);
                        create_sharedthread(name, 0,
                                            &mboxids[create_idx],
                                            &tid);

                        if(last_id == mboxids[create_idx]) {
                                return -__LINE__;
                        }
                        last_id = mboxids[create_idx];

                        create_idx++;
                        if(create_idx == mboxes) {
                                create_idx = 0;
                        }
                }

                for(j=0 ; j<mboxes ; j++) {
                        msg = itc_alloc(sizeof(uint32_t),
                                        (0x12340000 + j));
                        itc_send(&msg, mboxids[j], ITC_MY_MBOX);
                        msg = itc_receive(ITC_NOFILTER,
                                          REC_TMO, ITC_FROM_ALL);
                        if(msg == NULL) {
                                return -__LINE__;
                        } else if(msg->msgno != (0x12340000 + j)) {
                                itc_free(&msg);
                                return -__LINE__;
                        }
                        itc_free(&msg);
                }
        }

        for(i=0 ; i<mboxes ; i++) {
                msg = itc_alloc(sizeof(uint32_t), STOP_THREAD);
                itc_send(&msg, mboxids[i], ITC_MY_MBOX);

                msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
                if(msg == NULL ||
                   msg->msgno != mboxids[i]) {
                        return -__LINE__;
                }
                itc_free(&msg);
        }

	itc_delete_mailbox(mbox_id);
        itc_exit();

        return 0;
}

static void *nombox_thread(void *data)
{
        union itc_msg *msg;
        itc_mbox_id_t mbox_id;

        mbox_id = *((itc_mbox_id_t *)data);

        msg = itc_alloc(sizeof(uint32_t), PING_THREAD);
        itc_send(&msg, mbox_id, mbox_id);

        return NULL;
}

static int test_nomboxsend(void)
{
        union itc_msg *msg;
        itc_mbox_id_t mbox_id;
        pthread_t tid;
        int retval = 0;

        mbox_id = itc_current_mbox();

        pthread_create(&tid, NULL, nombox_thread, &mbox_id);

        msg = itc_receive(ITC_NOFILTER, REC_TMO, ITC_FROM_ALL);
        if(msg == NULL) {
                pthread_join(tid, NULL);
                return -__LINE__;
        } else if(msg->msgno != PING_THREAD) {
                retval = -__LINE__;
        }

        itc_free(&msg);
        pthread_join(tid, NULL);

        return retval;
}

static void *mbox_thread(void *data)
{
        union itc_msg *msg;
        itc_mbox_id_t mbox_id;
	char *name = data;

        mbox_id = itc_create_mailbox(name, 0);

	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	itc_free(&msg);

	itc_delete_mailbox(mbox_id);

        return NULL;
}

static int test_mbox_events(void)
{
        union itc_msg *msg;
	uint32_t mb_added[] = { 1, ITC_MBOX_ADDED };
	uint32_t mb_removed[] = { 1, ITC_MBOX_REMOVED };
        itc_mbox_id_t mbox_id;
        pthread_t tid;
        int last = 0, retval = 0;

	itc_subscribe_events(ITC_EVENT_MBOXES_ADDED |
			     ITC_EVENT_MBOXES_REMOVED);

	while(!last) {
		msg = itc_receive(mb_added, 100, ITC_FROM_ALL);
		if(msg == NULL) {
			return -__LINE__;
		}
		last = msg->itc_mbox_added.last;
		itc_free(&msg);
	}

        pthread_create(&tid, NULL, mbox_thread, "mb_added");

	msg = itc_receive(mb_added, 100, ITC_FROM_ALL);
	if(msg == NULL) {
		retval = -__LINE__;
		goto out;
	}
	if(strcmp(msg->itc_mbox_added.mbox_name, "mb_added") != 0) {
		itc_free(&msg);
		retval = -__LINE__;
		goto out;
	}
	mbox_id = msg->itc_mbox_added.mbox_id;
	itc_free(&msg);

	msg = itc_alloc(sizeof(uint32_t), 0xADDE);
	itc_send(&msg, mbox_id, ITC_MY_MBOX);
	pthread_join(tid, NULL);

	msg = itc_receive(mb_removed, 100, ITC_FROM_ALL);
	if(msg == NULL) {
		return -__LINE__;
	}
	if(strcmp(msg->itc_mbox_removed.mbox_name, "mb_added") != 0) {
		itc_free(&msg);
		return -__LINE__;
	}
	if(msg->itc_mbox_removed.mbox_id != mbox_id) {
		itc_free(&msg);
		return -__LINE__;
	}
	itc_free(&msg);

	itc_unsubscribe_events(ITC_EVENT_ALL);

        return 0;
out:
        pthread_cancel(tid);
	pthread_join(tid, NULL);

        return retval;
}

static int test_locate_events(void)
{
        union itc_msg *msg;
	uint32_t unres_locate[] = { 1, ITC_LOCATE_UNRESOLVED };

	itc_subscribe_events(ITC_EVENT_LOCATE_UNRESOLVED);

	itc_locate_async("notcreatedmb", NULL, ITC_MY_MBOX);
	msg = itc_receive(unres_locate, 100, ITC_FROM_ALL);
	if(msg == NULL) {
		return -__LINE__;
	}
	if(strcmp(msg->itc_locate_unresolved.mbox_name, "notcreatedmb") != 0) {
		itc_free(&msg);
		return-__LINE__;
	}
	if(msg->itc_locate_unresolved.mbox_id != itc_current_mbox()) {
		itc_free(&msg);
		return-__LINE__;
	}
	itc_free(&msg);

	itc_unsubscribe_events(ITC_EVENT_LOCATE_UNRESOLVED);

	itc_locate_async("notcreatedmb2", NULL, ITC_MY_MBOX);
	msg = itc_receive(unres_locate, 100, ITC_FROM_ALL);
	if(msg != NULL) {
		itc_free(&msg);
		return -__LINE__;
	}

	return 0;
}

int run_mbox_tests(void)
{
        itc_mbox_id_t mbox_id;
        int i=0, result;

        if(itc_init(21, ITC_MALLOC, NULL,
                    ITC_NO_NAMESPACE, 0) != 0) {
                return -1;
        }

        mbox_id = itc_create_mailbox("mbox_test", 0);

        while(mbox_tests[i].no_tests != 0) {
                result = test_mboxes(mbox_tests[i].no_tests,
                                     "test_master",
                                     "test_slave",
                                     mbox_tests[i].loops,
                                     mbox_tests[i].data_size,
                                     mbox_tests[i].delay_time,
                                     mbox_tests[i].burst,
                                     mbox_tests[i].max_looptime);
                if(result != 0) {
                        result -= (i * 100000);
                        break;
                }
                i++;
        }
        if(result != 0) {
                goto out;

        }
        result = test_msgremove(10);
        if(result != 0) {
                result -= 1000000;
                goto out;
        }

        result = test_mboxfd(10);
        if(result != 0) {
                result -= 1010000;
                goto out;
        }

        result = test_sharedmbox("mbox1", "mbox2", "sharedmbox", 5);
        if(result != 0) {
                result -= 1020000;
                goto out;
        }

        result = test_filters("filter_mbox");
        if(result != 0) {
                result -= 1030000;
                goto out;
        }

#ifdef ITC_STAT_MBOXID
        result = test_staticid("staticmbox", 10, 10);
        if(result != 0) {
                result -= 1040000;
                goto out;
        }
#endif

        result = test_nomboxsend();
        if(result != 0) {
                result -= 1050000;
                goto out;
        }

        result = test_mbox_events();
        if(result != 0) {
                result -= 1060000;
                goto out;
        }

        result = test_locate_events();
        if(result != 0) {
                result -= 1070000;
                goto out;
        }

        itc_delete_mailbox(mbox_id);
        itc_exit();

        result = test_maxmboxes(120, 20);
        if(result != 0) {
                return result -= 1080000;
        }

        result = test_maxmboxes(6000, 7);
        if(result != 0) {
                return result -= 1090000;
        }

	result = test_getownname();
	if(result != 0) {
		result -= 1100000;
		goto out;
	}

out:
        return result;
}

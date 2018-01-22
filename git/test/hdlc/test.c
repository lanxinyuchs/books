/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <arpa/inet.h>

#include "rolc-link-api.h"
#include "rolc-link-int.h"


struct thrdata {
	uint32_t	thread_num;
	uint32_t	link_id;
	int			active;
	int			msize;
	int			mcnt;
	char		link_name[32];
};


struct testdata {
	uint32_t					msgno;
	char						data[1];
};

union itc_msg {
	uint32_t					msgno;
	struct rolc_linkstate_ind	lkind;
	struct testdata				test;
	struct rolc_spray_done		sdone;
};


static itc_mbox_id_t 			g_mbox;
static itc_mbox_id_t 			mboxes[512];
static pthread_t 	 			threads[512];

static pthread_mutex_t 			mbox_lock;
static int 						mbox_slot = 0;
static int 						thrd_slot = 0;

/*
#define CRC_INIT            	0xffffffff
#define CRC_HIGH            	0x80000000
#define CRC_POLY            	0x04c11db7


unsigned long crc32(char* p, int len)
{
    unsigned long i = 0, j = 0, c = 0, bit = 0;
    unsigned long crc = CRC_INIT;

    for (i = 0; i < len; i++)  {
        c = (unsigned long)*p++;

        for (j=0x80; j; j>>=1) {
            bit = crc & CRC_HIGH;
            crc<<= 1;

            if (c & j)
                bit ^= CRC_HIGH;
            if (bit)
                crc ^= CRC_POLY;
        }
    }
    return crc;
}

static void calc_crc(union itc_msg *msg)
{
	int length = itc_size(msg);
	char *p = (char*)msg + length - 4;
	uint32_t fcs = htonl(crc32((char*)msg, length - 4));

	memcpy(p, &fcs, 4);		
}

static int check_crc(union itc_msg *msg)
{
	static int tot_err = 0;
	int size = (int)itc_size(msg);

	if (crc32((char*)msg, size)) {
		tot_err++;
		printf("CRC error (total: %d) in %d byte packet\n", tot_err, size);
		return 1;
	}
	return 0;
}
*/


static itc_mbox_id_t hunt_peer(char *peer_name, itc_monitor_id_t *mtor)
{
	static uint32_t sel[3] = {2,ITC_LOCATE_DEFAULT_NO,ROLC_LINKSTATE_IND};
	union itc_msg *msg;
	itc_mbox_id_t mbox;

	itc_locate_async(peer_name, NULL, ITC_MY_MBOX);

	for (;;) {
		msg = itc_receive(sel, ITC_NO_TMO, ITC_FROM_ALL);
		if (msg->msgno == ITC_LOCATE_DEFAULT_NO)
			break;

		itc_free(&msg);
	}

	mbox = itc_sender(msg);
	itc_free(&msg);

	if (*mtor == ITC_NO_ID)
		*mtor = itc_monitor(mbox, NULL);

	return mbox;
}

static void do_ping(char *peer_name, uint32_t msgno,
			int size, int nsig, uint32_t linkid)
{
	struct timespec ts = {0,1000000};
	itc_monitor_id_t mtor = ITC_NO_ID;
	itc_mbox_id_t peer, me, p2;
	union itc_msg *msg;
	uint32_t rx_count = 0;
	int dump_tmo = 0;

	me = itc_current_mbox();
	peer = hunt_peer(peer_name, &mtor);
	msgno += 0x1000;

	msg = itc_alloc(size, msgno);
	memset((char*)msg + 4, (char)me, size - 4);
	//calc_crc(msg);
	itc_send(&msg, peer, me);

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, 5000, ITC_FROM_ALL);
		if (msg == NULL) {
			if (dump_tmo) {
				printf("-- RX_TIMEOUT [%s] rx_count: %u\n",
						peer_name, rx_count);
				dump_tmo = 0;
			}
			p2 = peer;
			peer = hunt_peer(peer_name, &mtor);

			if (peer != p2) {
				printf("-- NEW mbox ID --\n");
			}
			continue;
		}

		if (msg->msgno == ROLC_LINKSTATE_IND) {
			if ((msg->lkind.link_id == linkid) && 
				(msg->lkind.available == 0)) {
				printf("-- ROLC_LINK_DOWN [%s]\n", peer_name);
				peer = hunt_peer(peer_name, &mtor);
			}
			itc_free(&msg);
			continue;
		}

		if (msg->msgno == ITC_MONITOR_DEFAULT_NO) {
			printf("-- MBOX REMOVED  [%s] (rx_count: %d)\n",
						peer_name, rx_count);
			mtor = ITC_NO_ID;
			peer = hunt_peer(peer_name, &mtor);
			itc_free(&msg);
			msg = itc_alloc(size, msgno);
			memset((char*)msg + 4, (char)me, size - 4);
			//calc_crc(msg);
			itc_send(&msg, peer, me);
			continue;
		}

		//check_crc(msg);

		if (((msg->msgno != msgno) || (itc_sender(msg) != peer))) {
			char mbox_name[64];
			char mbox_name_[64];

			itc_get_name(itc_sender(msg), mbox_name, sizeof(mbox_name));
			itc_get_name(peer, mbox_name_, sizeof(mbox_name_));

			printf("BAD message! (exp. '%s'  actual: '%s') "
					"(msgexp: %u   msgrcv: %u  sndr: 0x%x  peer: 0x%x)\n", 
					mbox_name_, mbox_name, msgno, msg->msgno,
					itc_sender(msg), peer);

			for (;;) sleep(100);
		}

		itc_free(&msg);

		if (++rx_count >= nsig)
			break;
		
		if ((rx_count % 500) == 0)
			nanosleep(&ts, NULL);

		msg = itc_alloc(size, msgno);
		memset((char*)msg + 4, (char)me, size - 4);
		//calc_crc(msg);
		itc_send(&msg, peer, me);
	}

	msg = itc_alloc(32, 0);
	itc_send(&msg, peer, ITC_MY_MBOX);

	msg = itc_alloc(size, msgno - 0x1000);
	itc_send(&msg, g_mbox, me);
}

static void do_pong(char *peer_name, uint32_t msgno, uint32_t linkid)
{
	itc_monitor_id_t mtor = ITC_NO_ID;
	itc_mbox_id_t peer, me, p2;
	union itc_msg *msg;
	int mcnt = 0;
	int dump_tmo = 1;

	me = itc_current_mbox();
	peer = hunt_peer(peer_name, &mtor);
	msgno += 0x1000;

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, 10000, ITC_FROM_ALL);
		if (msg == NULL) {
			if (dump_tmo) {
				printf("-- RX_TIMEOUT [%s] (mcnt: %d)\n", peer_name, mcnt);
				dump_tmo = 0;
			}
			p2 = peer;
			peer = hunt_peer(peer_name, &mtor);

			if (peer != p2) {
				printf("-- NEW mbox ID --\n");
			}
			continue;
		}

		if (msg->msgno == ROLC_LINKSTATE_IND) {
			if ((msg->lkind.link_id == linkid) && (msg->lkind.available == 0)) {
				peer = hunt_peer(peer_name, &mtor);
			}
			itc_free(&msg);
			continue;
		}

		if (msg->msgno == ITC_MONITOR_DEFAULT_NO) {
			printf("-- MBOX REMOVED  [%s]. Quitting after %d received\n",
							peer_name, mcnt);
			itc_free(&msg);
			mtor = ITC_NO_ID;
			peer = hunt_peer(peer_name, &mtor);
			continue;
		}

		if (msg->msgno == 0){
			//printf("Received %d signals (0x%x). Quitting.\n", mcnt, msg->msgno);
			itc_free(&msg);
			break;
		}

		/*
		if (((msg->msgno != msgno)) || (itc_sender(msg) != peer)) {
			char mbox_name[64];
			char mbox_name_[64];

			itc_get_name(itc_sender(msg), mbox_name, sizeof(mbox_name));
			itc_get_name(peer, mbox_name_, sizeof(mbox_name_));

			printf("BAD message! (exp. '%s'  actual: '%s')"
					"(msgexp: %u   msgrcv: %u) (sndr: 0x%x peer: 0x%x)\n", 
					mbox_name_, mbox_name, msgno, msg->msgno, itc_sender(msg), peer);

			for (;;) sleep(100);
		}*/


		itc_send(&msg, itc_sender(msg), me);

		if ((++mcnt % 5000) == 0)
			printf("received %u signals\n",mcnt);
	}

	msg = itc_alloc(32, msgno - 0x1000);
	itc_send(&msg, g_mbox, me);
}

static char *mb_pre[4] = {"mboxA","mboxB","mboxC","mboxD"};
static int mb_idx = 0;

static void *wx_thread(void *param)
{
	struct thrdata *td = param;
	char peer_name[64];
	char mbox_name[32];
	itc_mbox_id_t me;

	if (td->active) {
		sprintf(mbox_name, "%s_%03u", mb_pre[mb_idx], td->thread_num);
		sprintf(peer_name, "%s/%s_%03u", td->link_name, mb_pre[mb_idx+1],
									td->thread_num);
	} else {
		sprintf(mbox_name, "%s_%03u", mb_pre[mb_idx+1], td->thread_num);
		sprintf(peer_name, "%s/%s_%03u", td->link_name, mb_pre[mb_idx],
									td->thread_num);
	}
	me = itc_create_mailbox(mbox_name, 0);
	if (me == ITC_NO_ID) {
		printf("%s mbox failure. Terminating thread", __func__);
		pthread_exit(0);
	}

	pthread_mutex_lock(&mbox_lock);
	mboxes[mbox_slot++] = me;
	pthread_mutex_unlock(&mbox_lock);

	if (td->active)
		do_ping(peer_name, td->thread_num, td->msize, td->mcnt, td->link_id);
	else
		do_pong(peer_name, td->thread_num, td->link_id);

	sleep(1);
	itc_delete_mailbox(me);
	free(param);
	pthread_exit(0);
}

static void launch_threads(char *link_name, uint32_t base, uint32_t link,
							 uint32_t num, int size, int act)
{
	struct thrdata *td;
	uint32_t cnt;

	if (act == ROLC_MASTER)
		act = 1;
	else
		act = 0;

	for (cnt = 0; cnt < num; cnt++) {
		td = malloc(sizeof(*td));
		td->thread_num = base++;
		td->link_id = link;
		td->active = act;
		td->msize = size;
		td->mcnt = 200;
		strncpy(td->link_name, link_name, 32);

		if (pthread_create(&threads[thrd_slot++], NULL, wx_thread, td))
			printf("create thread failed, %d\n", errno);
	}
}

static int tc_count = 0;

void *test_spray(void *ctx)
{
	struct test_args *targ = ctx;
	struct rolc_link_config cfg;
	union itc_msg *msg;
	struct rolc_if *handle;
	itc_mbox_id_t me;
	uint32_t link_id[64];
	int ret, cnt, mode = ROLC_SLAVE;
	int nthread = 2, size = 100;
	uint32_t nlinks = 4;
	uint32_t ntests;

	mbox_slot = 0;
	thrd_slot = 0;

	memset(mboxes, 0, sizeof(mboxes));
	memset(threads, 0, sizeof(threads));
	memset(&cfg, 0, sizeof(cfg));
	pthread_mutex_init(&mbox_lock, NULL);

	size = targ->size;
	nlinks = targ->nlink;
	nthread = targ->nthread;

	if (targ->master)
		mode = ROLC_MASTER;

	me = itc_create_mailbox("rapp", 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure: %d\n",me);
		return NULL;
	}

	ntests = nlinks*nthread;
	g_mbox = me;

	ret = rolc_init(&handle);
	if (ret) {
		printf("rolc_link_init failed, %d\n", ret);
		return NULL;
	}

	for (cnt = 0; cnt < nlinks; cnt++) {

		char link_name[32];

		cfg.ri_addr = cnt;
		cfg.mode = mode;

		if (mode == ROLC_SLAVE)
			sprintf(link_name, "master_%02d", cnt);
		else
			sprintf(link_name, "slave_%02d", cnt);

		ret = rolc_link_setup(handle, link_name, &cfg, &link_id[cnt]);
		if (ret) {
			printf("rolc_link_create(%s) failed, %d\n", link_name, ret);
			return NULL;
		}
		launch_threads(link_name, cnt*nthread, link_id[cnt], nthread, size, mode);
	}

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch (msg->msgno) {
		case ROLC_LINKSTATE_IND:
		{
			union itc_msg *sig;
			int i;

			for (i = 0; i < mbox_slot; i++) {
				sig = itc_alloc(sizeof(sig->lkind), ROLC_LINKSTATE_IND);
				sig->lkind = msg->lkind;
				itc_send(&sig, mboxes[i], me);
			}
			break;
		}
		default:
			ntests--;
			if (((ntests % 100) == 0) || (ntests < 10))
				printf("Test #%u done. %d tests still running\n", msg->msgno, ntests);
			
			if (ntests == 0)
				goto get_out;
			break;
		}
		itc_free(&msg);
	}

 get_out:

	itc_free(&msg);

	for (cnt = 0; cnt < thrd_slot; cnt++)
		pthread_join(threads[cnt], NULL);

	rolc_shutdown(&handle);
	printf("   ***** ROLC SHUTDOWN COMPLETE... (%d threads  %d times) *****\n", cnt, ++tc_count);

	msg = itc_alloc(sizeof(msg->sdone),ROLC_SPRAY_DONE);
	msg->sdone.result = 0;
	itc_send(&msg, targ->parent_mbox, ITC_MY_MBOX);
	usleep(500);
	itc_delete_mailbox(me);
	mb_idx = (mb_idx + 2) & 3; /* 0,2,0,2,0,... */
	return NULL;
}

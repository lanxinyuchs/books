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
#include "ecb-link-api.h"


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

struct linkstate {
	uint32_t		linkid;
	uint32_t		link_lost;
};

union itc_msg {
	uint32_t					msgno;
	struct ecb_link_event		lkevt;
	struct testdata				test;
	struct rolc_spray_done		sdone;
};


static itc_mbox_id_t 			g_mbox;
static itc_mbox_id_t 			mboxes[512];
static pthread_t 	 			threads[512];

static pthread_mutex_t 			mbox_lock;
static int 						mbox_slot = 0;
static int 						thrd_slot = 0;



static
itc_mbox_id_t hunt_peer(char *peer_name, 
						itc_monitor_id_t *mtor,
						struct linkstate *ls)
{
	static uint32_t sel[3] = {2, ITC_LOCATE_DEFAULT_NO, ECB_LINK_EVENT_MSG};
	union itc_msg *msg;
	itc_mbox_id_t mbox;

	itc_locate_async(peer_name, NULL, ITC_MY_MBOX);

	for (;;) {
		msg = itc_receive(sel, 5000, ITC_FROM_ALL);

		if (msg == NULL) {
			printf("Timeout (5 seconds) while hunting for %s\n", peer_name);
			for (;;) sleep(1000);
			continue;
		}

		switch (msg->msgno) {
			case ITC_LOCATE_DEFAULT_NO:
				goto peer_found;
				break;

			case ECB_LINK_EVENT_MSG:
				if (msg->lkevt.link == ls->linkid) {
					if (msg->lkevt.event == ECB_LINK_STATE_DOWN) {
						printf("-- ROLC_LINK_DOWN [%s]\n", peer_name);
						ls->link_lost = 1;
					} else if (ls->link_lost) {
						printf("-- ROLC_LINK_UP [%s]\n", peer_name);
						ls->link_lost = 0;
					}
				}
				break;

			default:
				printf("Unexpected msgno 0x%x for %s\n", msg->msgno, peer_name);
				break;
		}

		itc_free(&msg);
	}

 peer_found:

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
	struct linkstate lstat;
	int dump_tmo = 0;

	lstat.linkid = linkid;
	lstat.link_lost = 0;

	me = itc_current_mbox();
	peer = hunt_peer(peer_name, &mtor, &lstat);
	msgno += 0x1000;

	msg = itc_alloc(size, msgno);
	memset((char*)msg + 4, (char)linkid, size - 4);
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
			peer = hunt_peer(peer_name, &mtor, &lstat);

			if (peer != p2) {
				printf("-- NEW mbox ID --\n");
			}
			continue;
		}

		if (msg->msgno == ECB_LINK_EVENT_MSG) {
			if (msg->lkevt.link == linkid) {
				if (msg->lkevt.event == ECB_LINK_STATE_DOWN) {
					printf("-- ROLC_LINK_DOWN [%s]\n", peer_name);
					lstat.link_lost = 1;
					peer = hunt_peer(peer_name, &mtor, &lstat);
				} else if (lstat.link_lost) {
					printf("-- ROLC_LINK_UP [%s]\n", peer_name);
					lstat.link_lost = 0;
				}
			}
			itc_free(&msg);
			continue;
		}

		if (msg->msgno == ITC_MONITOR_DEFAULT_NO) {
			printf("-- MBOX REMOVED  [%s] (rx_count: %d)\n",
						peer_name, rx_count);
			mtor = ITC_NO_ID;
			peer = hunt_peer(peer_name, &mtor, &lstat);
			itc_free(&msg);
			msg = itc_alloc(size, msgno);
			memset((char*)msg + 4, (char)me, size - 4);
			itc_send(&msg, peer, me);
			continue;
		}

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
		memset((char*)msg + 4, (char)linkid, size - 4);
		itc_send(&msg, peer, me);
	}

	msg = itc_alloc(32, 0);
	itc_send(&msg, peer, ITC_MY_MBOX);
}

static void do_pong(char *peer_name, uint32_t msgno, uint32_t linkid)
{
	itc_monitor_id_t mtor = ITC_NO_ID;
	itc_mbox_id_t peer, me, p2;
	union itc_msg *msg;
	int mcnt = 0;
	int dump_tmo = 1;

	struct linkstate lstat;

	lstat.linkid = linkid;
	lstat.link_lost = 0;

	me = itc_current_mbox();
	peer = hunt_peer(peer_name, &mtor, &lstat);
	msgno += 0x1000;

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, 10000, ITC_FROM_ALL);
		if (msg == NULL) {
			if (dump_tmo) {
				printf("-- RX_TIMEOUT [%s] (mcnt: %d)\n", peer_name, mcnt);
				dump_tmo = 0;
			}
			p2 = peer;
			peer = hunt_peer(peer_name, &mtor, &lstat);

			if (peer != p2) {
				printf("-- NEW mbox ID --\n");
			}
			continue;
		}

		if (msg->msgno == ECB_LINK_EVENT_MSG) {
			if (msg->lkevt.link == linkid) {
				if (msg->lkevt.event == ECB_LINK_STATE_DOWN) {
					printf("-- ROLC_LINK_DOWN [%s]\n", peer_name);
					lstat.link_lost = 1;
					peer = hunt_peer(peer_name, &mtor, &lstat);
				} else if (lstat.link_lost) {
					printf("-- ROLC_LINK_UP [%s]\n", peer_name);
					lstat.link_lost = 0;
				}
			}
			itc_free(&msg);
			continue;
		}

		if (msg->msgno == ITC_MONITOR_DEFAULT_NO) {
			printf("-- MBOX REMOVED  [%s]. Quitting after %d received\n",
							peer_name, mcnt);
			itc_free(&msg);
			mtor = ITC_NO_ID;
			if (mcnt < 10)
				peer = hunt_peer(peer_name, &mtor, &lstat);
			else {
				printf("¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤   WAIT ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤\n");
				break;
			}
			continue;
		}

		if (msg->msgno == 0){
			//printf("Received %d signals from %s. Quitting.\n", mcnt, peer_name);
			itc_free(&msg);
			break;
		}


		if (((msg->msgno != msgno)) || (itc_sender(msg) != peer)) {
			char mbox_name[64];
			char mbox_name_[64];

			itc_get_name(itc_sender(msg), mbox_name, sizeof(mbox_name));
			itc_get_name(peer, mbox_name_, sizeof(mbox_name_));

			printf("BAD message! (exp. '%s'  actual: '%s')"
					"(msgexp: %u   msgrcv: %u) (sndr: 0x%x peer: 0x%x)\n",
					mbox_name_, mbox_name, msgno, msg->msgno, itc_sender(msg), peer);

			for (;;) sleep(100);
		}

		itc_send(&msg, itc_sender(msg), me);

		if ((++mcnt % 5000) == 0)
			printf("received %u signals\n",mcnt);
	}
}

static char *mb_pre[4] = {"mboxA","mboxB","mboxC","mboxD"};
static int mb_idx = 0;

static void *wx_thread(void *param)
{
	struct timespec ts = {0, 200000000};
	union itc_msg *msg;
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
	msg = itc_alloc(32, td->thread_num);
	itc_send(&msg, g_mbox, me);

	/* Give the signal 200 ms to be picked up */
	clock_nanosleep(CLOCK_MONOTONIC, 0, &ts, NULL);

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
		td->mcnt = 10;//200;
		strncpy(td->link_name, link_name, 32);

		if (pthread_create(&threads[thrd_slot++], NULL, wx_thread, td))
			printf("create thread failed, %d\n", errno);
	}
}

static int tc_count = 0;

void *test_spray(void *ctx)
{
	struct test_args *targ = ctx;
	struct ecb_link_config cfg;
	union itc_msg *msg;
	void *handle;
	itc_mbox_id_t me;
	uint32_t link_id[64];
	uint32_t link_refcnt[64];
	int ret, cnt, mode = ROLC_SLAVE;
	int nthread = 2, size = 100;
	uint32_t nlinks = 4, lnk;
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

	ret = ecb_link_init(&handle);
	if (ret) {
		printf("ecb_link_init failed, %d\n", ret);
		return NULL;
	}

	printf("Doing spray testing...\n");

	for (cnt = 0; cnt < nlinks; cnt++) {

		cfg.address = cnt + 128 + (tc_count & 3)*nlinks;
		cfg.station = (mode == ROLC_MASTER) ? ECB_STATION_PRIMARY : ECB_STATION_SECONDARY;

		if (mode == ROLC_SLAVE)
			sprintf(cfg.name, "master_%02d", cnt);
		else
			sprintf(cfg.name, "slave_%02d", cnt);

		ret = ecb_link_create(handle, &cfg, &link_id[cnt]);
		if (ret) {
			printf("ecb_link_create(%s) failed, %d\n", cfg.name, ret);
			return NULL;
		}
		link_refcnt[cnt] = nthread;
		launch_threads(cfg.name, cnt*nthread, link_id[cnt], nthread, size, mode);
	}

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch (msg->msgno) {
		case ECB_LINK_EVENT_MSG:
		{
			union itc_msg *sig;
			int i;

			for (i = 0; i < mbox_slot; i++) {
				sig = itc_alloc(sizeof(sig->lkevt), ECB_LINK_EVENT_MSG);
				sig->lkevt = msg->lkevt;
				itc_send(&sig, mboxes[i], me);
			}
			break;
		}
		default:
			ntests--;
			lnk = msg->msgno/nthread;
			if (--link_refcnt[lnk] == 0) {
				ecb_link_destroy(handle, link_id[lnk]);
			}

			//if (((ntests % 100) == 0) || (ntests < 10))
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

	ecb_link_shutdown(&handle);
	printf("   ***** ROLC SHUTDOWN COMPLETE... (%d links %d threads  %d times) *****\n",
			nlinks, nthread, ++tc_count);
	msg = itc_alloc(sizeof(msg->sdone),ROLC_SPRAY_DONE);
	msg->sdone.result = 0;
	itc_send(&msg, targ->parent_mbox, ITC_MY_MBOX);
	usleep(500);
	itc_delete_mailbox(me);
	mb_idx = (mb_idx + 2) & 3; /* 0,2,0,2,0,... */
	return NULL;
}

/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <errno.h>
#include <syslog.h>
#include <ctype.h>
#include <sys/types.h>
#include <itc.h>
#include <rbs-sys-api.h>
#include <ulh_cm.h>
#include <ulh_lnh.h>
#include <ulh_lnh_msg.h>
#include "ulh_rio.h"

#include "eolc-link-api.h"
#include "eolc-link-internal.h"

#define __debug_print(...)					\
        {									\
			fprintf(stderr, __VA_ARGS__);	\
			fprintf(stderr, "\n");			\
        }


/*#define RIOLNH_VERBOSE*/
#if defined(RIOLNH_VERBOSE)
#define DBG(...) __debug_print(__VA_ARGS__)
#else
#define DBG(...)
#endif


#define RIO_UCM_NAME					"riocm"
#define RIO_ULNH_NAME					"riolh"
#define RIOLNH_SCHEDFIFO_PRIO                            2

struct rio_link {
	struct rio_link	   *next;
	void			   *owner;
	itc_mbox_id_t		owner_mbox;
	itc_monitor_id_t	owner_mtor;
	uint32_t			link;
	uint32_t			peer;
	char				name[EOLC_LINKNAME_SIZE];
};

struct rio_data {
	itc_mbox_id_t		server_mbox;
	struct rio_link		*conn_list;
};

union itc_msg {
	uint32_t         		 			msgno;

	struct eolc_create_req				ctreq;
	struct eolc_create_rsp				ctrsp;
	struct eolc_delete_req				dsreq;
	struct eolc_delete_rsp				dsrsp;
	struct eolc_linkstate_ind			lkind;

	struct ulh_lnhmsg_createcm_req    	cmreq;
	struct ulh_lnhmsg_createcm_rsp    	cmrsp;
	struct ulh_lnhmsg_createlink_req  	lcreq;
	struct ulh_lnhmsg_createlink_rsp  	lcrsp;
	struct ulh_lnhmsg_destroylink_req 	ldreq;
	struct ulh_lnhmsg_destroylink_rsp 	ldrsp;
	struct ulh_lnhmsg_notify			ulind;
};



static struct rio_data rioif;

static struct rio_link *get_by_linkid(uint32_t linkid, int unhook)
{
	struct rio_link **pp, *p;

	for (pp = &rioif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->link == linkid) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct rio_link *get_by_owner(void *owner, int unhook)
{
	struct rio_link **pp, *p;

	for (pp = &rioif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->owner == owner) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct rio_link *get_by_mbox(itc_mbox_id_t mbox, int unhook)
{
	struct rio_link **pp, *p;

	for (pp = &rioif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->owner_mbox == mbox) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static int remove_link(struct rio_link *p)
{
	int ret = -EIO;
	uint32_t select[2] = {1, ULH_LNHMSG_DESTROYLINK_RSP};
	union itc_msg *msg = itc_alloc(	sizeof(msg->ldreq), 
									ULH_LNHMSG_DESTROYLINK_REQ
								  );

	msg->ldreq.seq = 0;
	msg->ldreq.lid = p->link;
	itc_send(&msg, rioif.server_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 500, rioif.server_mbox);

	if (msg) {
		ret = (int)msg->ldrsp.result;
		itc_free(&msg);
	}

	free(p);
	return ret;
}

static int collision_detect(uint32_t peer, char *name)
{
	static int rcode[4] = {0, EADDRINUSE, EADDRINUSE, EEXIST};
	struct rio_link	*p;

	for (p = rioif.conn_list; p; p = p->next) {
		int cmp = 0;
		if (strcmp(p->name, name) == 0)
			cmp |= 1;
		if (p->peer == peer)
			cmp |= 2;
		if (cmp)
			return -rcode[cmp];
    }
	return 0;
}


static void handle_create(union itc_msg *cfg)
{
	uint32_t select[2] = {1, ULH_LNHMSG_CREATECM_RSP};
	struct ulh_cm_rio_config *cfp;
	union itc_msg *msg;
	struct rio_link *p;
	itc_mbox_id_t sndr;
	uint32_t cmid;
	uint32_t peer;
	int ret;
	int is_dus = rbs_sys_getboardtype() & RBS_BOARD_DUSX1;
	
	peer = cfg->ctreq.config.g1.emca_id;
	sndr = itc_sender(cfg);

	if (is_dus)
		peer >>= 8;

	if ((ret = collision_detect(peer, cfg->ctreq.name)))
		goto done;


	msg = itc_alloc(sizeof(msg->cmreq)+sizeof(*cfp), ULH_LNHMSG_CREATECM_REQ);
	cfp = (struct ulh_cm_rio_config *)&msg->cmreq.config;
	memset(cfp, 0 , sizeof(*cfp));

	cfp->cmn.cfg_size = sizeof(*cfp);
	cfp->peer_id = (uint16_t)peer;

	if (is_dus == 0) {
		cfp->letter 	= 2;
		cfp->channel 	= 1;
		cfp->src_mac[0] = 2;
		cfp->src_mac[2] = 1;
		cfp->src_mac[4] = 0xa;
		cfp->dst_mac[0] = 2;
		cfp->dst_mac[2] = 1;
		cfp->dst_mac[4] = 0x25;
		strcpy(cfp->ifname, "riomsg");
	} else
		strcpy(cfp->ifname, "riodio");

	msg->cmreq.seq = 0;
	strcpy(msg->cmreq.cm_name, RIO_UCM_NAME);
    sprintf(msg->cmreq.cm_instance, "rio_conn%04X", (unsigned short)peer);

	itc_send(&msg, rioif.server_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATECM_RSP;
	msg = itc_receive(select, ITC_NO_TMO, rioif.server_mbox);

	ret = msg->cmrsp.result;
	cmid = msg->cmrsp.cmid;
	itc_free(&msg);

	if (ret)
		goto done;

	if ((p = malloc(sizeof(*p))) == NULL) {
		ret = -ENOMEM;
		goto done;
	}

	msg = itc_alloc(sizeof(msg->lcreq), ULH_LNHMSG_CREATELINK_REQ);
	msg->lcreq.prio = 0;
	msg->lcreq.cmid = cmid;
	strcpy(msg->lcreq.name, cfg->ctreq.name);

	itc_send(&msg, rioif.server_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATELINK_RSP;

	msg = itc_receive(select, ITC_NO_TMO, rioif.server_mbox);
	ret = msg->lcrsp.result;
	p->link = msg->lcrsp.lid;
	itc_free(&msg);

	if (ret) {
		free(p);
		goto done;
	}

	p->owner_mtor 	= (itc_monitor_id_t)-1;
	if (!get_by_mbox(sndr, 0))
		p->owner_mtor 	= itc_monitor(sndr, NULL);

	p->next			= rioif.conn_list;
	p->owner 		= cfg->ctreq.owner;
	p->owner_mbox 	= sndr;
	p->peer			= peer;
	strcpy(p->name, cfg->ctreq.name);

	rioif.conn_list = p;
	cfg->ctrsp.linkid = p->link;

 done:

	cfg->msgno = EOLC_CREATE_RSP;
	cfg->ctrsp.result = ret;
	itc_send(&cfg, sndr, ITC_MY_MBOX);
}

static void handle_destroy(union itc_msg *msg)
{
	struct rio_link *p;

	for (;;) {
		if (msg->dsreq.owner)
			p = get_by_owner(msg->dsreq.owner, 1);
		else
			p = get_by_linkid(msg->dsreq.linkid, 1);

		if (p) {
			if (p->owner_mtor != (itc_monitor_id_t)-1)
				itc_unmonitor(p->owner_mtor);
			remove_link(p);
		} else
			break;
	}

	msg->msgno = EOLC_DELETE_RSP;
	msg->dsrsp.result = 0;
	itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_monitor(union itc_msg *msg)
{
	itc_mbox_id_t mbox = itc_sender(msg);
	struct rio_link *p;

	for (p = get_by_mbox(mbox, 1); p; ) {
		remove_link(p);
		p = get_by_mbox(mbox, 1);
	}
	itc_free(&msg);
}

static void handle_notify(union itc_msg *msg)
{
	union itc_msg *ind;
	struct rio_link *p = get_by_linkid(msg->ulind.lid, 0);

	if (p) {
		ind = itc_alloc(sizeof(ind->lkind), EOLC_LINKSTATE_IND);
		ind->lkind.link_id = p->link;
		ind->lkind.state = EOLC_LINK_UP;

		if (msg->ulind.state == ULH_LINK_DOWN)
			ind->lkind.state = EOLC_LINK_DOWN;

		itc_send(&ind, p->owner_mbox, ITC_MY_MBOX);
	}
	itc_free(&msg);
}


int main(int argc, char *argv[])
{
	union itc_msg *msg;
	itc_mbox_id_t me;
	void *lnh;
	int ret;
	struct sched_param schedParams;

	/* Raise scheduling class to SCHED_FIFO with prio 2*/
	memset(&schedParams, 0, sizeof(struct sched_param)); 
	schedParams.sched_priority = RIOLNH_SCHEDFIFO_PRIO;

	ret = pthread_setschedparam(pthread_self(), SCHED_FIFO, &schedParams);
	if(ret) {
		syslog(LOG_ERR, "pthread_setschedparam failure: %d\n",ret);
	}

	ret = itc_init(512, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		syslog(LOG_ERR, "itc_init failure: %d\n",ret);
		printf("itc_init failure: %d\n (errno: %d)",ret, errno);
		return -1;
	}

	me = itc_create_mailbox(EOLC_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "created mbox failure: %d\n",me);
		itc_exit();
		return -1;
	}

	ret = ulh_lnh_init(32);
	if (ret) {
		syslog(LOG_ERR, "ulh_lnh_init() failed, %d\n", ret);
		goto eout;
	}

	ret = ulh_rio_init(RIO_UCM_NAME);
	if (ret) {
		syslog(LOG_ERR, "ulh_rio_init() failed, %d\n", ret);
		goto eout;
	}

	lnh = ulh_lnh_create(RIO_ULNH_NAME);
	if (!lnh) {
		syslog(LOG_ERR, "ulh_lnh_create() failed\n");
		goto eout;
	}

	for (;; sleep(1)) {
		rioif.server_mbox = itc_locate(RIO_ULNH_NAME);
		if (rioif.server_mbox != ITC_NO_ID)
			break;
	}

	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL); 
		switch(msg->msgno) {

			case EOLC_CREATE_REQ:
				handle_create(msg);
				break;

			case EOLC_DELETE_REQ:
				handle_destroy(msg);
				break;

			case ITC_MONITOR_DEFAULT_NO:
				handle_monitor(msg);
				break;

			case ULH_LNHMSG_NOTIFY:
				handle_notify(msg);
				break;

			default:
				itc_free(&msg);
				break;
		}
	}
	ulh_lnh_destroy(lnh);

 eout:
	itc_delete_mailbox(me);
	itc_exit();
	return -1;
}


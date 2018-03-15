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

#define _GNU_SOURCE

#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <pthread.h>
#include <syslog.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <itc.h>
#include <ulh_transport.h>
#include <ulh_lnh_msg.h>
#include <ulh_hdlc.h>
#include <ulh_cm.h>
#include <ulh_lnh.h>
#include <rhai-ecp.h>
#include <rhai-ric.h>

#define TRACEPOINT_DEFINE
#include <com_ericsson_system_start.h>
#include "hdlclnh_lttng.h"
#include "trace.h"

#include "rolc-link-api.h"
#include "rolc-link-internal.h"

#define HDLC_SCHEDFIFO_PRIO				30

#define HDLC_CM_NAME					"hdlccm"
#define HDLC_LNH_NAME					"hdlc_lnh"

#define	FNV32_INIT	 					0x811c9dc5
#define	FNV32_PRIM	 					0x01000193
#define MAX_SUBS						4

#define ECP_CIDS       					64
#define ECP_MTU        					120

#define ADD_NEW							1
#define DO_NOT_ADD_NEW					0

#define MAX_REDUNDANT_IDS				1000

union itc_msg {
	uint32_t         		 			msgno;
	struct rolc_create_req				ctreq;
	struct rolc_create_rsp				ctrsp;
	struct rolc_delete_req				dlreq;
	struct rolc_delete_rsp				dlrsp;
	struct rolc_inband_req				ibreq;
	struct rolc_inband_rsp				ibrsp;
	struct rolc_noband_req				nbreq;
	struct rolc_noband_rsp				nbrsp;
	struct rolc_remoteportid_req		rpreq;
	struct rolc_remoteportid_rsp		rprsp;
	struct rolc_linkswitch_req			lsreq;
	struct rolc_linkswitch_rsp			lsrsp;
	struct rolc_remoteport_ind			rpind;
	struct rolc_linkswitch_ind			lsind;
	struct rolc_inband_ind				ibind;
	struct rolc_linkstate_ind			lkind;
	struct ulh_lnhmsg_createcm_req    	cmreq;
	struct ulh_lnhmsg_createcm_rsp    	cmrsp;
	struct ulh_lnhmsg_createlink_req  	lcreq;
	struct ulh_lnhmsg_createlink_rsp  	lcrsp;
	struct ulh_lnhmsg_destroylink_req 	ldreq;
	struct ulh_lnhmsg_destroylink_rsp 	ldrsp;
	struct ulh_lnhmsg_notify			ulind;
	struct ulh_hdlcmsg_remoteportid_ind	hrpind;
	struct ulh_hdlcmsg_linkswitch_ind	hlsind;
};


struct ecp_conn {
	uint32_t 				ecp_cid;
	uint32_t 				ulh_cid;
};
           
struct ecp_tran {
	struct rhai_ecp_if 		*ecp_handle;
	struct ecp_conn 		*conns[ECP_CIDS];
};

struct link_data {
	struct link_data		*next;
	void					*owner;
	itc_mbox_id_t			owner_mbox;
	itc_monitor_id_t		owner_mtor;
	uint32_t 				linkid;
	uint32_t 				ecid;
	uint32_t 				tcid;
	uint32_t 				rolc_port;
	uint32_t				addr;
	char					name[ROLC_LINKNAME_SIZE];
};

struct subscribers {
	struct subscribers		*next;
	int				rolc_port;
	int				addr;
	int				num;
	itc_mbox_id_t			subs[MAX_SUBS];
};


static itc_mbox_id_t 		server_mbox;
static struct link_data 	*links;
static struct subscribers 	**subs_hash;
static struct ecp_tran 		transp;


static uint32_t hash32(uint32_t key)
{
	uint32_t hash = FNV32_INIT;
    uint8_t *bp = (uint8_t*)&key;

	hash ^= (uint32_t)*bp++;hash *= FNV32_PRIM;
	hash ^= (uint32_t)*bp++;hash *= FNV32_PRIM;
	hash ^= (uint32_t)*bp++;hash *= FNV32_PRIM;
	hash ^= (uint32_t)*bp;  hash *= FNV32_PRIM;

	return ((hash >> 7) ^ hash) & 0x7F;
}

static struct subscribers *get_subscribers(int rolc_port, int addr, int add)
{
	struct subscribers *p;
	uint32_t idx = hash32((rolc_port << 16) | addr);

	for (p = subs_hash[idx]; p; p = p->next)
		if ((p->rolc_port == rolc_port) && (p->addr == addr))
			return p;

	if ((add == ADD_NEW) && (p = malloc(sizeof(*p)))) {
		memset(p, 0, sizeof(*p));
		p->next = subs_hash[idx];
		subs_hash[idx] = p;
		p->rolc_port = rolc_port;
		p->addr = addr;
	}
	return p;
}

static struct link_data *get_by_owner(void *owner, int unhook)
{
	struct link_data **pp, *p;
	for (pp = &links; *pp; pp = &p->next) {
		p = *pp;
		if (p->owner == owner) {
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct link_data *get_by_mbox(itc_mbox_id_t mbox, int unhook)
{
	struct link_data **pp, *p;
	for (pp = &links; *pp; pp = &p->next) {
		p = *pp;
		if (p->owner_mbox == mbox) {
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct link_data *get_by_linkid(uint32_t linkid, int unhook)
{
	struct link_data **pp, *p;
	for (pp = &links; *pp; pp = &p->next) {
		p = *pp;
		if (p->linkid == linkid) {
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static uint32_t link_name_exists(char* name)
{
	struct link_data *p;
	for (p = links; p; p = p->next) {
		if (strcmp(p->name, name) == 0)
			return 1;
	}
	return 0;
}

static int collision_detect(uint32_t addr, uint32_t rolc_port, char *name)
{
	static int rcode[4] = {0, EADDRINUSE, EADDRINUSE, EEXIST};
	struct link_data *p;

	for (p = links; p; p = p->next) {
		int cmp = 0;
		if (strcmp(p->name, name) == 0)
			cmp |= 1;
		if ((p->addr == addr) && (p->rolc_port == rolc_port))
			cmp |= 2;
		if (cmp)
			return -rcode[cmp];
    }
	return 0;
}

static int free_link(struct link_data *p)
{
	int ret = -EIO;
	uint32_t select[2] = {1, ULH_LNHMSG_DESTROYLINK_RSP};
	union itc_msg *msg = itc_alloc(	sizeof(msg->ldreq),
                                        ULH_LNHMSG_DESTROYLINK_REQ);

	rhai_ecp_deletechannel(transp.ecp_handle, p->ecid);
	ulh_trans_destroy_conn(p->tcid);

	msg->ldreq.seq = 0;
	msg->ldreq.lid = p->linkid;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 500, server_mbox);

	if (msg) {
		ret = (int)msg->ldrsp.result;
		itc_free(&msg);
	}

	free(p);
	return ret;
}

/* for redundant link we will get the same name in request
 * as for primary nonredundant, so we have to create unique name
 * from it
 */
static int prepare_link_name(uint32_t mode, char *name, char *result_name) {
	int i;

	if (mode == ROLC_MASTER_REDUNDANT) {
		for (i = 0; i < MAX_REDUNDANT_IDS; i++) {
			snprintf(result_name, ROLC_LINKNAME_SIZE, "%.*s_R#%d",
				ROLC_LINKNAME_SIZE - 7, name, i);
			if (!link_name_exists(result_name))
				break;
		}
		if (i >= MAX_REDUNDANT_IDS) {
			syslog(LOG_ERR, "ulh_trans_create_conn() - "
				"exceeded max number of redundant links");
			return -1;
		}
	}
	else
		strcpy(result_name, name);

	return 0;
}

static int create_link(union itc_msg *msg, uint32_t *link_id)
{
	uint32_t select[2] = {1, ULH_LNHMSG_CREATECM_RSP};
	struct ulh_trans_addr ecp_addr;
	struct ulh_cm_hdlc_config *cfp;
	union itc_msg *req;
	itc_mbox_id_t client;
	struct link_data *p = NULL;
	uint32_t ecid = 0, addr, rolc_port, cmid;
	uint32_t tcid = ULH_TRANS_NOCONN;
	char link_name[ROLC_LINKNAME_SIZE];
	int ret;

	client  = itc_sender(msg);
	addr = msg->ctreq.ri_addr;
	rolc_port = msg->ctreq.ri_port;

	if ((ret = prepare_link_name(msg->ctreq.mode, msg->ctreq.name, link_name)))
		return ret;

	if ((ret = collision_detect(addr, rolc_port, link_name)))
		return ret;

	if ((ret = rhai_ecp_addchannel(transp.ecp_handle,
                                       RHAI_RICRI_PORT(rolc_port),
                                       addr, &ecid)))
           return ret;

	memset(&ecp_addr, 0, sizeof(ecp_addr));
	ecp_addr.data[0] = ecid;
	tcid = ulh_trans_create_conn("ecp0", &ecp_addr, &ecp_addr);

	if (tcid == ULH_TRANS_NOCONN) {
		syslog(LOG_ERR, "ulh_trans_create_conn() failed");
		ret = -EFAULT;
		goto equit;
	}

	req = itc_alloc(sizeof(req->cmreq)+sizeof(*cfp),
                        ULH_LNHMSG_CREATECM_REQ);

	cfp = (struct ulh_cm_hdlc_config*)&req->cmreq.config;
	cfp->cmn.cfg_size 		= sizeof(*cfp);
	cfp->cmn.cfg_version 	= ULH_HDLC_CFGVER_SWITCHING;
	cfp->cmn.cid 			= tcid;
	cfp->cmn.uref 			= 0;
	cfp->cmn.mbox 			= 0;
	cfp->primary 			= 1;
	cfp->hdlc_addr   		= 3;
	cfp->redundant			= (msg->ctreq.mode == ROLC_MASTER_REDUNDANT);
	cfp->port				= rolc_port;
	cfp->ecp_addr			= addr;
	cfp->notify_mbox		= itc_locate(ROLC_DAEMON_NAME);

	req->cmreq.seq			= 0;
	strcpy(req->cmreq.cm_name, HDLC_CM_NAME);
    sprintf(req->cmreq.cm_instance, "hdlc_%02u_%02u", rolc_port, addr);

	if (msg->ctreq.mode == ROLC_SLAVE) {
		cfp->primary   = 0;
		cfp->hdlc_addr = 0;
	}

	itc_send(&req, server_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATECM_RSP;
	req = itc_receive(select, ITC_NO_TMO, server_mbox);

	ret  = req->cmrsp.result;
	cmid = req->cmrsp.cmid;
	itc_free(&req);

	if (ret)
		goto equit;

	if ((p = malloc(sizeof(*p))) == NULL) {
		ret = -ENOMEM;
		goto equit;
	}

	req = itc_alloc(sizeof(req->lcreq), ULH_LNHMSG_CREATELINK_REQ);
	req->lcreq.prio = 0;
	req->lcreq.cmid = cmid;
	strcpy(req->lcreq.name, link_name);

	itc_send(&req, server_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATELINK_RSP;
	req = itc_receive(select, ITC_NO_TMO, server_mbox);

	ret = req->lcrsp.result;
	p->linkid = req->lcrsp.lid;
	itc_free(&req);

	if (ret)
		goto equit;

	p->next = links;
	p->owner = msg->ctreq.owner;
	p->owner_mbox = client;
	p->owner_mtor = (itc_monitor_id_t)-1;

	if (!get_by_mbox(client, 0))
		p->owner_mtor = itc_monitor(client, NULL);

	p->tcid = tcid;
	p->ecid = ecid;
	p->rolc_port = rolc_port;
	p->addr = addr;
	strncpy(p->name , link_name, ROLC_LINKNAME_SIZE);
	links = p;
	*link_id = p->linkid;
	return 0;

 equit:

	rhai_ecp_deletechannel(transp.ecp_handle, ecid);

	if (tcid != ULH_TRANS_NOCONN)
		ulh_trans_destroy_conn(tcid);
	if (p)
		free(p);
	return ret;
}

static int delete_link(union itc_msg *msg)
{
	struct link_data *p;

	for (;;) {
		if (msg->dlreq.owner)
			p = get_by_owner(msg->dlreq.owner, 1);
		else
			p = get_by_linkid(msg->dlreq.linkid, 1);

		if (p) {
			if (p->owner_mtor != (itc_monitor_id_t)-1)
				itc_unmonitor(p->owner_mtor);
			free_link(p);
		} else
			break;
	}
	return 0;
}

static void handle_monitor(union itc_msg *msg)
{
	itc_mbox_id_t client = itc_sender(msg);
	struct link_data *p = get_by_mbox(client, 1);

	for (;p;p = get_by_mbox(client, 1))
		free_link(p);
}

static void handle_notify(union itc_msg *msg)
{
	union itc_msg *ind;
	struct link_data *p = get_by_linkid(msg->ulind.lid, 0);

	if (p) {
		ind = itc_alloc(sizeof(ind->lkind), ROLC_LINKSTATE_IND);
		ind->lkind.link_id = (uint32_t)p->linkid;
		ind->lkind.available = 1;

		if (msg->ulind.state == ULH_LINK_DOWN)
			ind->lkind.available = 0;

		itc_send(&ind, p->owner_mbox, ITC_MY_MBOX);
	}
}

static void ecp_inband(void * ctx, struct rhai_ecp_inband *msg)
{
	struct subscribers *sub;
	union itc_msg *ind;
	int idx;

        /* we receive RHAI port so we have to convert it to ROLC PORT(ID)
           for a subscriber */
	sub = get_subscribers(RHAI_RICRI_ID(msg->ri_port), msg->ri_addr, DO_NOT_ADD_NEW);
	if (sub) {
		for (idx = 0; idx < sub->num; idx++) {
			ind = itc_alloc(sizeof(ind->ibind), ROLC_INBAND_IND);
			ind->ibind.ri_port = RHAI_RICRI_ID(msg->ri_port);
			ind->ibind.ri_addr = msg->ri_addr;
			ind->ibind.ru_port = msg->ru_port;
			ind->ibind.status  = msg->alarms;
			itc_send(&ind, sub->subs[idx], ITC_MY_MBOX);
		}
	}
}

static int add_subscriber(union itc_msg *msg, itc_mbox_id_t client)
{
	struct subscribers *p;
	int idx;
        int ret;

        p = get_subscribers(msg->ibreq.port, msg->ibreq.addr, ADD_NEW);

        if (p && !p->num)
        {
           if ((ret = rhai_ecp_enableinband(transp.ecp_handle,
                                           RHAI_RICRI_PORT(msg->ibreq.port),
                                           msg->ibreq.addr)))
           {
              syslog(LOG_ERR, "Failed to enable inband over ECP, port %d, addr %d result %d",
                     RHAI_RICRI_PORT(msg->ibreq.port),
                     msg->ibreq.addr,
                     ret);

              return -1;
           }
        }

	if (p) {
		for (idx = 0; idx < p->num; idx++)
			if (p->subs[idx] == client)
				return 0;

		if (idx < MAX_SUBS) {
			p->subs[p->num++] = client;
			return 0;
		}
	}

	return -1;
}

static int del_subscriber(union itc_msg *msg, itc_mbox_id_t client)
{
	struct subscribers *p;
	int idx;
        int ret;

	p = get_subscribers(msg->nbreq.port, msg->nbreq.addr, DO_NOT_ADD_NEW);
	if (p) {
		for (idx = 0; idx < p->num; idx++)
			if (p->subs[idx] == client)
				break;

		if (idx < p->num)
			p->subs[idx] = p->subs[--p->num];

                if (!p->num)
                {

                   if ((ret = rhai_ecp_disableinband(transp.ecp_handle,
                                            RHAI_RICRI_PORT(msg->nbreq.port),
                                                    msg->nbreq.addr)))
                      syslog(LOG_ERR, "Failed to disable inband over ECP, port %d, addr %d result %d",
                             RHAI_RICRI_PORT(msg->nbreq.port),
                             msg->nbreq.addr,
                             ret);
                }
	}

	return 0;
}

static int ecp_create_conn(void *tref, uint32_t tcid,
       struct ulh_trans_addr *src, struct ulh_trans_addr *dst, void **cref)
{
	struct ecp_tran *tran = tref;
	struct ecp_conn *conn;
	uint32_t ecid = (uint32_t)src->data[0];

	(void)dst;

	conn = malloc(sizeof(struct ecp_conn));
	if (!conn) 
		return -ENOMEM;

	tran->conns[ecid] = conn;
	conn->ecp_cid = ecid;
	conn->ulh_cid = tcid;

	*cref = conn;
	return 0;
}

static int ecp_destroy_conn(void *tref, void *cref)
{
	struct ecp_tran *tran = tref;
	struct ecp_conn *conn = cref;

	if (conn == NULL)
		return -EINVAL;

	tran->conns[conn->ecp_cid] = NULL;
	free(conn);
	return 0;
}

static int ecp_transmit(void *tref, void *cref, struct ulh_tbuff *tbuff)
{
	uint32_t size = ulh_tbuff_len(tbuff);
	struct ecp_tran *tran  = tref;
	struct ecp_conn *conn  = cref;
	struct rhai_ecp_pkt pt;
	int ret;

	ret = rhai_ecp_pkt_alloc(tran->ecp_handle, conn->ecp_cid, size, &pt);
	if (ret)
		goto quit;

	memcpy(pt.data + pt.p_offset, ulh_tbuff_get(tbuff), size);
	ret = rhai_ecp_pkt_tx(tran->ecp_handle, &pt);

	if (ret)
		(void)rhai_ecp_pkt_free(tran->ecp_handle, &pt);

 quit:
	ulh_tbuff_free(tbuff);
	return ret;
}

static int ecp_receive(void *tref, struct rhai_ecp_pkt *pkt)
{
	struct ecp_tran *tran = tref;
	struct ecp_conn *conn = tran->conns[pkt->cid];
	struct ulh_tbuff tbuff;
	int ret;

	if (conn) {
		ret = ulh_tbuff_alloc(conn->ulh_cid, pkt->p_size, &tbuff);
		if (ret)
			return ret;

		memcpy(tbuff.data, pkt->data + pkt->p_offset, pkt->p_size);
		ulh_trans_deliver(conn->ulh_cid, &tbuff);
	}
	return 0;

}

static int ecp_getmtu(void *param, void *conn_ref)
{
	(void)param;
	(void)conn_ref;
	return ECP_MTU;
}

static int32_t thread_init(void *ctx)
{
	char mbox_name[32];
	itc_mbox_id_t mbox;

	(void)ctx;

	if (itc_current_mbox() != ITC_NO_ID)
		return 0;

	sprintf(mbox_name, "ecp_%u", (uint32_t)pthread_self());
	mbox = itc_create_mailbox(mbox_name, 0);

	if (mbox != ITC_NO_ID)
		return 0;

	return -1;
}

static struct ulh_trans_ops ecp_ops = {
	.create_conn 	= ecp_create_conn,
	.destroy_conn 	= ecp_destroy_conn,
	.transmit 		= ecp_transmit,
	.getmtu 		= ecp_getmtu
};

static int ecp_init(void)
{
	struct rhai_ecp_if *handle;
	int ret = -EAGAIN;

	for (; ret; sleep(2)) {
		ret = rhai_ecp_init(&handle);

		if (ret == 0)
			break;

		if (ret != -EAGAIN) {
			syslog(LOG_ERR, "rhai_ecp_init: %d", ret);
			sleep(10);
		}
	}

	transp.ecp_handle = handle;
	if ((ret = ulh_trans_register("ecp0", &ecp_ops, &transp)))
		goto eout0;

	ret = rhai_ecp_inband_start(handle, thread_init, ecp_inband, &transp);
	if (ret)
		goto eout1;

	ret = rhai_ecp_pkt_rx_start(handle, thread_init, ecp_receive, &transp);
	if (ret == 0)
		return 0;

	rhai_ecp_inband_stop(handle);

 eout1:
	ulh_trans_unregister("ecp0");

 eout0:
	rhai_ecp_shutdown(&handle);
	return ret;
}

static void config_sched()
{
	struct sched_param schedp;
    int num_cores, ret, i;
    cpu_set_t cpuset;

    CPU_ZERO(&cpuset);
	num_cores = sysconf(_SC_NPROCESSORS_ONLN);

    /* Disallow core 0 */
    for (i = 1; i < num_cores; i++) {
        CPU_SET(i, &cpuset);
    }

    ret = pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t),
							&cpuset);
	if (ret) {
		syslog(LOG_ERR, "pthread_setaffinity_np failure: %d (errno: %d)",
						ret, errno);
	}

	/* 	Raise scheduling class to SCHED_FIFO with prio 30.*/
	memset(&schedp, 0, sizeof(struct sched_param));
	schedp.sched_priority = HDLC_SCHEDFIFO_PRIO;

	ret = pthread_setschedparam(pthread_self(), SCHED_FIFO, &schedp);
	if (ret) {
		syslog(LOG_ERR, "pthread_setschedparam failure: %d (errno: %d)\n",
						ret, errno);
	}
}

static void switch_link_data(uint32_t link_id1, uint32_t link_id2)
{
	struct link_data *ld1 = get_by_linkid(link_id1, 0);
	struct link_data *ld2 = get_by_linkid(link_id2, 0);
	uint32_t tmp_u32;
	tmp_u32 = ld1->linkid;
	ld1->linkid = ld2->linkid;
	ld2->linkid = tmp_u32;
	char tmp_link_name[ROLC_LINKNAME_SIZE];
	memcpy(tmp_link_name, ld1->name, ROLC_LINKNAME_SIZE);
	memcpy(ld1->name, ld2->name, ROLC_LINKNAME_SIZE);
	memcpy(ld2->name, tmp_link_name, ROLC_LINKNAME_SIZE);
}

int main(int argc, char *argv[])
{
	union itc_msg *msg, *rsp;
	itc_mbox_id_t me, snd;
	int ret;
	void *lnh;

	config_sched();

	ret = itc_init(512, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		syslog(LOG_ERR, "itc_init failure: %d\n",ret);
		return -1;
	}

	me = itc_create_mailbox(ROLC_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "created mbox failure: %d\n",me);
		itc_exit();
		return -1;
	}

	ret = ulh_lnh_init(64);
	if (ret) {
		syslog(LOG_ERR, "ulh_lnh_init() failed, %d\n", ret);
		goto error_out;
	}

	ret = ulh_hdlc_init(HDLC_CM_NAME);
	if (ret) {
		syslog(LOG_ERR, "ulh_ecb_init() failed, %d\n", ret);
		goto error_out;
	}

	lnh = ulh_lnh_create(HDLC_LNH_NAME);
	if (!lnh) {
		syslog(LOG_ERR, "ulh_lnh_create() failed\n");
		goto error_out;
	}

	ret = sizeof(struct subscribers*) * 128;
	subs_hash = malloc(ret);
	if (subs_hash == NULL) {
		syslog(LOG_ERR, "malloc(%d), %d\n", ret, errno);
		goto error_out;
	}

	memset(subs_hash, 0, ret);

	for (;; sleep(1)) {
		server_mbox = itc_locate(HDLC_LNH_NAME);
		if (server_mbox != ITC_NO_ID)
			break;
	}

	ret = ecp_init();
	if (ret < 0) {
		syslog(LOG_ERR, "ecp_init: %d\n",ret);
		goto error_out;
	}

	event_system_start("hdlclnh ready");

	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		snd = itc_sender(msg);

		switch(msg->msgno) {
			case ROLC_CREATE_REQ:
				HDLCLNH_DBG("received ROLC_CREATE_REQ: p:%d ec:%d m:%d n:%s",
					msg->ctreq.ri_port, msg->ctreq.ri_addr,
					msg->ctreq.mode, msg->ctreq.name);
				rsp = itc_alloc(sizeof(rsp->ctrsp), ROLC_CREATE_RSP);
				rsp->ctrsp.result = create_link(msg, &rsp->ctrsp.linkid);
				strcpy(rsp->ctrsp.name, msg->ctreq.name);
				HDLCLNH_DBG("sent ROLC_CREATE_RSP: r:%d l:%d (p:%d ec:%d m:%d n:%s)",
					rsp->ctrsp.result, rsp->ctrsp.linkid,
					msg->ctreq.ri_port, msg->ctreq.ri_addr,
					msg->ctreq.mode, msg->ctreq.name);
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ROLC_DELETE_REQ:
				HDLCLNH_DBG("received ROLC_DELETE_REQ: l:%d",
					msg->dlreq.linkid);
				rsp = itc_alloc(sizeof(rsp->dlrsp), ROLC_DELETE_RSP);
				rsp->dlrsp.result 	= delete_link(msg);
				rsp->dlrsp.owner 	= msg->dlreq.owner;
				rsp->dlrsp.linkid 	= msg->dlreq.linkid;
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ROLC_INBAND_SUB_REQ:
				rsp = itc_alloc(sizeof(rsp->ibrsp), ROLC_INBAND_SUB_RSP);
				rsp->ibrsp.result	= add_subscriber(msg, snd);
				rsp->ibrsp.port		= msg->ibreq.port;
				rsp->ibrsp.addr		= msg->ibreq.addr;
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ROLC_INBAND_UNSUB_REQ:
				rsp = itc_alloc(sizeof(rsp->nbrsp), ROLC_INBAND_UNSUB_RSP);
				rsp->nbrsp.result	= del_subscriber(msg, snd);
				rsp->nbrsp.port		= msg->nbreq.port;
				rsp->nbrsp.addr		= msg->nbreq.addr;
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ROLC_REMOTEPORTID_REQ:
				HDLCLNH_DBG("received ROLC_REMOTEPORTID_REQ: l:%d",
					msg->rpreq.link);
				rsp = itc_alloc(sizeof(rsp->rprsp), ROLC_REMOTEPORTID_RSP);
				rsp->rprsp.result	= ulh_hdlc_initiate_get_remote_port_id(lnh,
					msg->rpreq.link);
				rsp->rprsp.link		= msg->rpreq.link;
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ROLC_LINKSWITCH_REQ:
				HDLCLNH_DBG("received ROLC_LINKSWITCH_REQ: l:%d, rp:%d, to_l:%d",
					msg->lsreq.link, msg->lsreq.remote_port, msg->lsreq.to_link);
				rsp = itc_alloc(sizeof(rsp->lsrsp), ROLC_LINKSWITCH_RSP);
				rsp->lsrsp.result		= ulh_hdlc_initiate_linkswitch(lnh,
					msg->lsreq.to_link, msg->lsreq.link,
					msg->lsreq.remote_port);
				rsp->lsrsp.link			= msg->lsreq.link;
				rsp->lsrsp.remote_port	= msg->lsreq.remote_port;
				rsp->lsrsp.to_link		= msg->lsreq.to_link;
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ULH_HDLCMSG_REMOTEPORTID_IND:
				HDLCLNH_DBG("received ULH_HDLCMSG_REMOTEPORTID_IND: l:%d, rp:%d",
					msg->hrpind.link_id, msg->hrpind.port_id);
				rsp = itc_alloc(sizeof(rsp->rpind), ROLC_REMOTEPORTID_IND);
				rsp->rpind.result = msg->hrpind.result;
				rsp->rpind.link_id = msg->hrpind.link_id;
				rsp->rpind.port_id = msg->hrpind.port_id;
				itc_send(&rsp,
					(get_by_linkid(msg->hrpind.link_id, 0))->owner_mbox,
					ITC_MY_MBOX);
				break;

			case ULH_HDLCMSG_LINKSWITCH_IND:
				HDLCLNH_DBG("received ULH_HDLCMSG_LINKSWITCH_IND: l:%d, to_l:%d",
					msg->hlsind.link_id, msg->hlsind.to_link_id);
				switch_link_data(msg->hlsind.link_id, msg->hlsind.to_link_id);
				rsp = itc_alloc(sizeof(rsp->lsind), ROLC_LINKSWITCH_IND);
				rsp->lsind.result = msg->hlsind.result;
				rsp->lsind.link_id = msg->hlsind.link_id;
				rsp->lsind.to_link_id = msg->hlsind.to_link_id;
				itc_send(&rsp,
					(get_by_linkid(msg->hlsind.link_id, 0))->owner_mbox,
					ITC_MY_MBOX);
				break;

			case ULH_LNHMSG_NOTIFY:
				handle_notify(msg);
				break;

			case ITC_MONITOR_DEFAULT_NO:
				handle_monitor(msg);
				break;


			default:
				break;
		}
		itc_free(&msg);
	}

 error_out:

	itc_delete_mailbox(me);
	itc_exit();
	return -1;
}

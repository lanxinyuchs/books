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
#include <errno.h>
#include <ctype.h>
#include <pthread.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <itc.h>
#include <linx.h>
#include <linxcfg.h>
#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-ecpx.h>

#include "rolc-link-api.h"
#include "rolc-link-internal.h"


#define HDLC_LNH_NAME					"hdlc_lnh"

#define	FNV32_INIT	 					0x811c9dc5
#define	FNV32_PRIM	 					0x01000193
#define MAX_SUBS						4

#define ECPX_PORTS						6	
#define ECPX_RUADDRS					64


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
	struct rolc_inband_ind				ibind;
	struct rolc_linkstate_ind			lkind;
};

union LINX_SIGNAL {
	LINX_SIGSELECT			signo;
	struct linx_new_link	nlink;
};

struct link_data {
	struct link_data		*next;
	void					*owner;
	itc_mbox_id_t			owner_mbox;
	itc_monitor_id_t		owner_mtor;
	uint32_t 				linkid;
	uint32_t 				port;
	uint32_t				addr;
	uint32_t				chan;
	LINX_SPID				lspid;
	LINX_OSATTREF			attref;
	char					name[ROLC_LINKNAME_SIZE];
};

struct subscribers {
	struct subscribers		*next;
	int						port;
	int						addr;
	int						num;
	itc_mbox_id_t			subs[MAX_SUBS];
};

static struct gdata {
	int						sd;
	LINX				   *lx;
} g;

static struct link_data *links;
static struct subscribers **subs_hash;

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

static struct subscribers *get_subscribers(int port, int addr, int add)
{
	struct subscribers *p;
	uint32_t idx = hash32((port << 16) | addr);

	for (p = subs_hash[idx]; p; p = p->next)
		if ((p->port == port) && (p->addr == addr))
			return p;

	if (add && (p = malloc(sizeof(*p)))) {
		memset(p, 0, sizeof(*p));
		p->next = subs_hash[idx];
		subs_hash[idx] = p;
		p->port = port;
		p->addr = addr;
	}
	return p;
}

static struct link_data *get_by_name(char *name, int unhook)
{
	struct link_data **pp, *p;
	for (pp = &links; *pp; pp = &p->next) {
		p = *pp;
		if (!strcmp(p->name, name)) {
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
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

static struct link_data *get_by_spid(LINX_SPID spid)
{
	struct link_data **pp;

	for (pp = &links; *pp; pp = &(*pp)->next)
		if ((*pp)->lspid == spid)
			return *pp;

	return NULL;
}



static int collision_detect(uint32_t addr, uint32_t port, char *name)
{
	static int rcode[4] = {0, EADDRINUSE, EADDRINUSE, EEXIST};
	struct link_data *p;

	for (p = links; p; p = p->next) {
		int cmp = 0;
		if (strcmp(p->name, name) == 0)
			cmp |= 1;
		if ((p->addr == addr) && (p->port == port))
			cmp |= 2;
		if (cmp)
			return -rcode[cmp];
    }
	return 0;
}

static int alloc_channel(uint32_t *cid, uint32_t port, uint32_t addr)
{
	struct rbs_ecpx_ioc_addchannel ioc;

	if ((port >= ECPX_PORTS) || (addr >= ECPX_RUADDRS))
		return -EINVAL;

	memset(&ioc, 0, sizeof(ioc));
	ioc.addr = addr;
	ioc.port = port;
	ioc.inband_en = 1;

	if (ioctl(g.sd, RBS_ECPXIOC_ACHANNEL, &ioc))
		return -errno;

	*cid = ioc.cid;
	return 0;
}

void free_channel(uint32_t cid)
{
	(void) ioctl(g.sd, RBS_ECPXIOC_DCHANNEL, &cid);
}

static void linx_free(struct link_data *p)
{
	char *cons = NULL;
	if (p->attref != LINX_ILLEGAL_ATTREF)
		linx_detach(g.lx, &p->attref);

	linx_remove_link(p->name, &cons);
	if (cons) {
		linx_remove_connection(cons);
		free(cons);
	}

	free_channel(p->chan);
	free(p);
}

static int linx_link(union itc_msg *msg, uint32_t *link_id) 
{
	struct linx_link_arg link_arg;
	union linx_con_arg arg;
	itc_mbox_id_t client;
	struct link_data *p;
	char cname[64];
	char *cons = NULL;
	uint32_t cid = 0, addr, port;
	int ret;

	client  = itc_sender(msg);
	addr = msg->ctreq.ri_addr;
	port = msg->ctreq.ri_port;

	if ((ret = collision_detect(addr, port, msg->ctreq.name)))
		return ret;

	if ((ret = alloc_channel(&cid, port, addr)))
		return ret;

	sprintf(cname, "ecp_%u", port);
	memset(&arg, 0, sizeof(arg));
	arg.hdlc.name 		= cname;
	arg.hdlc.ecpif 		= "ecp0";
	arg.hdlc.channel 	= cid;
	arg.hdlc.mode 		= 1;

	if (msg->ctreq.mode == ROLC_SLAVE)
		arg.hdlc.mode = 0;

	if ((ret = linx_create_connection(LINX_CON_HDLC, &arg, &cons))) {
		free_channel(cid);
		return ret;
	}

	memset(&link_arg, 0, sizeof(link_arg));
	link_arg.name = msg->ctreq.name;
	link_arg.connections = cons;

	if ((ret = linx_create_link(&link_arg))) {
		linx_remove_connection(cons);
		free_channel(cid);
		free(cons);
		return ret;
	}

	p = malloc(sizeof(*p));
	if (p) {
		p->next = links;
		p->owner = msg->ctreq.owner;
		p->owner_mbox = client;
		p->owner_mtor = (itc_monitor_id_t)-1;

		if (!get_by_mbox(client, 0))
			p->owner_mtor = itc_monitor(client, NULL);

		p->linkid = (uint32_t)p;
		p->port = port;
		p->addr = addr;
		p->chan = cid;
		p->lspid = LINX_ILLEGAL_SPID;
		p->attref = LINX_ILLEGAL_ATTREF;
		strncpy(p->name , msg->ctreq.name, ROLC_LINKNAME_SIZE);
		links = p;
		*link_id = p->linkid;
		return 0;
	}

	free_channel(cid);
	linx_remove_link(p->name, &cons);
	if (cons) {
		linx_remove_connection(cons);
		free(cons);
	}
	return -ENOMEM;
}

static int linx_destroy(union itc_msg *msg)
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
			linx_free(p);
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
		linx_free(p);
}

static void handle_new_link(LINX *lnx, union LINX_SIGNAL *sig)
{
	char *link_name = sig->nlink.buf + sig->nlink.name;
	struct link_data *p = get_by_name(link_name, 0);
	union itc_msg *ind;

	if (p) {
		ind = itc_alloc(sizeof(ind->lkind), ROLC_LINKSTATE_IND);
		ind->lkind.link_id = (uint32_t)p;
		ind->lkind.available = 1;
		itc_send(&ind, p->owner_mbox, ITC_MY_MBOX);

		p->lspid	= linx_sender(lnx, &sig);
		p->attref	= linx_attach(lnx, NULL, p->lspid);
	}

	linx_request_new_link(lnx, sig->nlink.token);
}

static void handle_lost_link(LINX *lnx, union LINX_SIGNAL *sig)
{
	LINX_SPID spid = linx_sender(lnx, &sig);
	struct link_data *p = get_by_spid(spid);
	union itc_msg *ind;

	if (p) {
		ind = itc_alloc(sizeof(ind->lkind), ROLC_LINKSTATE_IND);
		ind->lkind.link_id = (uint32_t)p;
		ind->lkind.available = 0;
		itc_send(&ind, p->owner_mbox, ITC_MY_MBOX);

		p->lspid	= LINX_ILLEGAL_SPID;
		p->attref	= LINX_ILLEGAL_ATTREF;
	}
}

static void handle_linx_signal(LINX *lnx, union LINX_SIGNAL *sig)
{
	if (sig == LINX_NIL)
		return;

	switch (sig->signo) {
		case LINX_OS_NEW_LINK_SIG:
			handle_new_link(lnx, sig);
			break;

		case LINX_OS_ATTACH_SIG:
			handle_lost_link(lnx, sig);
			break;

		default:
			break;
	}

	linx_free_buf(lnx, &sig);	
}

static void deliver_inband(struct rbs_ecpx_message *msg)
{
	struct subscribers *sub;
	union itc_msg *ind;
	uint32_t status = 0;
	int idx;

	sub = get_subscribers(msg->mc.inband.ru_port,
						  msg->mc.inband.rua, 0);
	if (sub) {
		if (msg->mc.inband.pfa)
			status |= ROLC_INBANDSTATUS_PFA;
		if (msg->mc.inband.lof)
			status |= ROLC_INBANDSTATUS_LOF;
		if (msg->mc.inband.los)
			status |= ROLC_INBANDSTATUS_LOS;
		if (msg->mc.inband.sdi)
			status |= ROLC_INBANDSTATUS_SDI;
		if (msg->mc.inband.rai)
			status |= ROLC_INBANDSTATUS_RAI;

		for (idx = 0; idx < sub->num; idx++) {
			ind = itc_alloc(sizeof(ind->ibind), ROLC_INBAND_IND);
			ind->ibind.ri_port = msg->mc.inband.port;
			ind->ibind.ri_addr = msg->mc.inband.rua;
			ind->ibind.ru_port = msg->mc.inband.ru_port;
			ind->ibind.status = status;
			itc_send(&ind, sub->subs[idx], ITC_MY_MBOX);
		}
	}
}

static void handle_ecp_message(int sd)
{
	struct rbs_ecpx_message msg;
	struct pollfd pfd;
	int tmo, ret;

	for (tmo = 100;; tmo = 0) {
		pfd.fd = sd;
		pfd.events = POLLIN;
		pfd.revents = 0;

		ret = poll(&pfd, 1, tmo);
		if (ret <= 0)
			return;

		ret = recv(sd, &msg, sizeof(msg), 0);
		if (ret <= 0)
			return;

		if (msg.msg_no == RBS_ECPXMSG_INBAND)
			deliver_inband(&msg);
	}
}

static int add_subscriber(union itc_msg *msg, itc_mbox_id_t client)
{
	struct subscribers *p;
	int idx;

	p = get_subscribers(msg->ibreq.port, msg->ibreq.addr, 1);
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

	p = get_subscribers(msg->ibreq.port, msg->ibreq.addr, 0);
	if (p) {
		for (idx = 0; idx < p->num; idx++)
			if (p->subs[idx] == client)
				break;

		if (idx < p->num)
			p->subs[idx] = p->subs[--p->num];
	}
	return 0;
}

int main(int argc, char *argv[])
{
	static LINX_SIGSELECT anysig[1] = {0};
	union LINX_SIGNAL *sig;
	union itc_msg *msg, *rsp;
	itc_mbox_id_t me, snd;
	LINX_NLREF nlref = 0;
	LINX *lnx = NULL;
	int sd = -1, ret;

	ret = itc_init(32, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n",ret);
		goto error_nop;
	}

	me = itc_create_mailbox(ROLC_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure: %d\n",me);
		itc_exit();
		goto error_nop;
	}

	while (sd < 0) {
		if ((sd = socket(AF_RBS, SOCK_RAW, RBS_PROTO_ECPX)) < 0)
			sleep(1);
	}

	lnx = linx_open(HDLC_LNH_NAME,0,NULL);
	if (lnx == NULL) {
		printf("linx_open failed, %d\n", errno);
		goto error_out;
	}

	nlref = linx_request_new_link(lnx, 0);
	if (nlref == LINX_ILLEGAL_NLREF) {
		printf("linx_request_new_link, %d\n", errno);
		goto error_out;
	}

	ret = sizeof(struct subscribers*) * 128;
	subs_hash = malloc(ret);
	if (subs_hash == NULL) {
		printf("malloc(%d), %d\n", ret, errno);
		goto error_out;
	}
	memset(subs_hash, 0, ret);
	g.sd = sd;
	g.lx = lnx;
	
	for(;;) {

		handle_ecp_message(sd);

		ret = linx_receive_w_tmo(lnx, &sig, 0, anysig);
		if (ret > 0)
			handle_linx_signal(lnx, sig);
	
		msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
		if (msg == NULL)
			continue;

		snd = itc_sender(msg);
		switch(msg->msgno) {
			case ROLC_CREATE_REQ:
				rsp = itc_alloc(sizeof(rsp->ctrsp), ROLC_CREATE_RSP);
				rsp->ctrsp.result = linx_link(msg, &rsp->ctrsp.linkid);
				strcpy(rsp->ctrsp.name, msg->ctreq.name);
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ROLC_DELETE_REQ:
				rsp = itc_alloc(sizeof(rsp->dlrsp), ROLC_DELETE_RSP);
				rsp->dlrsp.result = linx_destroy(msg);
				rsp->dlrsp.owner = msg->dlreq.owner;
				rsp->dlrsp.linkid = msg->dlreq.linkid;
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
	close(sd);

	if (lnx)
		linx_close(lnx);

 error_nop:

	printf(" HDLCLNH daemon failure !!!\n");
	for (;;)
		sleep(180);

	return -1;
}

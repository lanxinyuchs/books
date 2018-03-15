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
#include <sys/types.h>
#include <itc.h>
#include <linx.h>
#include <linxcfg.h>
#include "ecb-link-api.h"
#include "ecb-link-internal.h"


#define ECB_LNH_NAME					"ecb_lnh"


struct ecb_link {
	struct ecb_link	   		*next;
	void			   		*owner;
	itc_mbox_id_t			owner_mbox;
	itc_monitor_id_t		owner_mtor;
	LINX_SPID				lspid;
	LINX_OSATTREF			attref;
	uint32_t				link;
	uint32_t				addr;
	char					name[ECB_LINKNAME_SIZE];
};

struct ecb_data {
	LINX 					*lnx;
	struct ecb_link			*conn_list;
};


union LINX_SIGNAL {
	LINX_SIGSELECT			signo;
	struct linx_new_link	nlink;
};


union itc_msg {
	uint32_t					msgno;

	struct ecb_link_create		ctreq;
	struct ecb_link_create		ctrsp;
	struct ecb_link_destroy		dsreq;
	struct ecb_link_destroy		dsrsp;
	struct ecb_link_event		lkind;
};


static struct ecb_data ecbif;

static struct ecb_link *get_by_name(char *name, int unhook)
{
	struct ecb_link **pp, *p;

	for (pp = &ecbif.conn_list; *pp; pp = &p->next) {
		p = *pp;
		if (!strcmp(p->name, name)) {
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct ecb_link *get_by_spid(LINX_SPID spid, int unhook)
{
	struct ecb_link **pp, *p;

	for (pp = &ecbif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->lspid == spid) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct ecb_link *get_by_linkid(uint32_t link, int unhook)
{
	struct ecb_link **pp, *p;

	for (pp = &ecbif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->link == link) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct ecb_link *get_by_owner(void *owner, int unhook)
{
	struct ecb_link **pp, *p;

	for (pp = &ecbif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->owner == owner) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct ecb_link *get_by_mbox(itc_mbox_id_t mbox, int unhook)
{
	struct ecb_link **pp, *p;

	for (pp = &ecbif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->owner_mbox == mbox) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static int remove_link(struct ecb_link *p)
{
	char *cons = NULL;
	if (p->attref != LINX_ILLEGAL_ATTREF)
		linx_detach(ecbif.lnx, &p->attref);

	linx_remove_link(p->name, &cons);
	if (cons) {
		linx_remove_connection(cons);
		free(cons);
	}
	free(p);
	return 0;
}

static int collision_detect(uint32_t addr, char *name)
{
	static int rcode[4] = {0, EADDRINUSE, EADDRINUSE, EEXIST};
	struct ecb_link	*p;

	for (p = ecbif.conn_list; p; p = p->next) {
		int cmp = 0;
		if (strcmp(p->name, name) == 0)
			cmp |= 1;
		if (p->addr == addr)
			cmp |= 2;
		if (cmp)
			return -rcode[cmp];
    }
	return 0;
}

static void handle_create(union itc_msg *msg)
{
	struct linx_link_arg link_arg;
	union linx_con_arg arg;
	struct ecb_link *p;
	itc_mbox_id_t sndr;
	char *cons = NULL;
	char cname[64];
	int ret;

	sndr = itc_sender(msg);

	if ((ret = collision_detect(msg->ctreq.address, msg->ctreq.name)))
		goto done;

	sprintf(cname, "ecb_%u", msg->ctreq.address);
	memset(&arg, 0, sizeof(arg));
	arg.ecb.name		= cname;
	arg.ecb.mtu 		= 78;
	arg.ecb.wnd_size 	= 1;
	arg.ecb.queue_size 	= 128;
	arg.ecb.mode	 	= (uint8_t)msg->ctreq.station;
	arg.ecb.address 	= (uint8_t)msg->ctreq.address;

	if ((ret = linx_create_connection(LINX_CON_ECB, &arg, &cons)))
		goto done;

	memset(&link_arg, 0, sizeof(link_arg));
	link_arg.name = msg->ctreq.name;
	link_arg.connections = cons;

	if ((ret = linx_create_link(&link_arg))) {
		linx_remove_connection(cons);
		goto done;
	}

	if ((p = malloc(sizeof(*p)))) {
		p->next = ecbif.conn_list;
		p->owner_mbox = sndr;
		p->owner_mtor = (itc_monitor_id_t)-1;
		if (!get_by_mbox(sndr, 0))
			p->owner_mtor = itc_monitor(sndr, NULL);

		p->lspid = LINX_ILLEGAL_SPID;
		p->attref = LINX_ILLEGAL_ATTREF;
		p->link = (uint32_t)p;
		p->addr = msg->ctreq.address;
		strncpy(p->name , msg->ctreq.name, ECB_LINKNAME_SIZE);
		ecbif.conn_list = p;
		msg->ctrsp.linkid = p->link;
	} else {
		ret = -ENOMEM;
		linx_remove_link(msg->ctreq.name, &cons);
		if (cons)
			linx_remove_connection(cons);
	}

 done:

	if (cons)
		free(cons);

	msg->msgno = ECB_LINK_CREATE_RSP;
	msg->ctrsp.result = ret;
	itc_send(&msg, sndr, ITC_MY_MBOX);

}

static void handle_destroy(union itc_msg *msg)
{
	struct ecb_link *p;

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

	msg->msgno = ECB_LINK_DESTROY_RSP;
	msg->dsrsp.result = 0;
	itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_monitor(union itc_msg *msg)
{
	itc_mbox_id_t mbox = itc_sender(msg);
	struct ecb_link *p;

	for (p = get_by_mbox(mbox, 1); p; ) {
		remove_link(p);
		p = get_by_mbox(mbox, 1);
	}
	itc_free(&msg);
}

static void handle_new_link(LINX *lnx, union LINX_SIGNAL *sig)
{
	char *link_name = sig->nlink.buf + sig->nlink.name;
	struct ecb_link *p = get_by_name(link_name, 0);

	if (p) {
		union itc_msg *ind;
		ind = itc_alloc(sizeof(ind->lkind), ECB_LINK_EVENT_MSG);
		ind->lkind.link = p->link;
		ind->lkind.event = ECB_LINK_STATE_UP;
		itc_send(&ind, p->owner_mbox, ITC_MY_MBOX);

		p->lspid	= linx_sender(lnx, &sig);
		p->attref	= linx_attach(lnx, NULL, p->lspid);
	}

	linx_request_new_link(lnx, sig->nlink.token);
}

static void handle_lost_link(LINX *lnx, union LINX_SIGNAL *sig)
{
	LINX_SPID spid = linx_sender(lnx, &sig);
	struct ecb_link *p = get_by_spid(spid, 0);

	if (p) {
		union itc_msg *ind;
		ind = itc_alloc(sizeof(ind->lkind), ECB_LINK_EVENT_MSG);
		ind->lkind.link = p->link;
		ind->lkind.event = ECB_LINK_STATE_DOWN;
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


int main(int argc, char *argv[])
{
	static LINX_SIGSELECT anysig[1] = {0};
	union LINX_SIGNAL *sig;
	union itc_msg *msg;
	itc_mbox_id_t me;
	LINX_NLREF nlref = 0;
	LINX *lnx = NULL;
	int ret;

	ret = itc_init(32, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n",ret);
		return -1;
	}

	me = itc_create_mailbox(ECB_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure: %d\n",me);
		itc_exit();
		return -1;
	}

	lnx = linx_open(ECB_LNH_NAME,0,NULL);
	if (lnx == NULL) {
		printf("linx_open failed, %d\n", errno);
		goto error_out;
	}

	nlref = linx_request_new_link(lnx, 0);
	if (nlref == LINX_ILLEGAL_NLREF) {
		printf("linx_request_new_link, %d\n", errno);
		goto error_out;
	}

	ecbif.lnx = lnx;

	for(;;) {
		ret = linx_receive_w_tmo(lnx, &sig, 100, anysig);
		if (ret > 0)
			handle_linx_signal(lnx, sig);
	
		msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
		if (msg == NULL)
			continue;

		switch(msg->msgno) {

			case ECB_LINK_CREATE_REQ:
				handle_create(msg);
				break;

			case ECB_LINK_DESTROY_REQ:
				handle_destroy(msg);
				break;

			case ITC_MONITOR_DEFAULT_NO:
				handle_monitor(msg);
				break;

			default:
				itc_free(&msg);
				break;
		}

	}

 error_out:

	itc_delete_mailbox(me);
	itc_exit();

	if (lnx)
		linx_close(lnx);

	return -1;
}

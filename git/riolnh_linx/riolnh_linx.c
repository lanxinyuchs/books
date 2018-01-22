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
#include <rbs-sys-api.h>
#include <syslog.h>

#include "eolc-link-api.h"
#include "eolc-link-internal.h"


#define RIO_LNH_NAME		"rio_lnh"


struct rio_link {
	struct rio_link	   		*next;
	void			   		*owner;
	itc_mbox_id_t			owner_mbox;
	itc_monitor_id_t		owner_mtor;
	LINX_SPID				lspid;
	LINX_OSATTREF			attref;
	uint32_t				link;
	uint32_t				peer;
	char					name[EOLC_LINKNAME_SIZE];
};

struct rio_data {
	LINX 					*lnx;
	struct rio_link			*conn_list;
};


union LINX_SIGNAL {
	LINX_SIGSELECT			signo;
	struct linx_new_link	nlink;
};


union itc_msg {
	uint32_t					msgno;

	struct eolc_create_req		ctreq;
	struct eolc_create_rsp		ctrsp;
	struct eolc_delete_req		dsreq;
	struct eolc_delete_rsp		dsrsp;
	struct eolc_linkstate_ind	lkind;
};



static struct rio_data rioif;


static struct rio_link *get_by_name(char *name, int unhook)
{
	struct rio_link **pp, *p;

	for (pp = &rioif.conn_list; *pp; pp = &p->next) {
		p = *pp;
		if (!strcmp(p->name, name)) {
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct rio_link *get_by_spid(LINX_SPID spid, int unhook)
{
	struct rio_link **pp, *p;

	for (pp = &rioif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->lspid == spid) {
			p = *pp;
			if (unhook)
				*pp = p->next;
			return p;
		}
	}
	return NULL;
}

static struct rio_link *get_by_linkid(uint32_t link, int unhook)
{
	struct rio_link **pp, *p;

	for (pp = &rioif.conn_list; *pp; pp = &(*pp)->next) {
		if ((*pp)->link == link) {
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
	char *cons = NULL;
	if (p->attref != LINX_ILLEGAL_ATTREF)
		linx_detach(rioif.lnx, &p->attref);

	linx_remove_link(p->name, &cons);
	if (cons) {
		linx_remove_connection(cons);
		free(cons);
	}
	free(p);
	return 0;
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


static void handle_create(union itc_msg *msg)
{
	struct linx_link_arg link_arg;
	union linx_con_arg arg;
	struct rio_link *p;
	itc_mbox_id_t sndr;
	char *cons = NULL;
	char cname[64];
	char lname[64];
	uint32_t peer;
	int is_dus = rbs_sys_getboardtype() & RBS_BOARD_DUSX1;
	int ret;

	peer = msg->ctreq.config.g1.emca_id;
	sndr = itc_sender(msg);

	if (is_dus)
		peer >>= 8;

	if ((ret = collision_detect(peer, msg->ctreq.name)))
		goto done;

	memset(&arg, 0, sizeof(arg));
	memset(lname, 0, sizeof(lname));

	arg.srio.local_port	= 1;
	arg.srio.peer_id	= (uint16_t)peer;

	if (is_dus) {
		sprintf(cname, "rio_con_%u", peer);
		arg.srio.srio_if = "riodio";
	} else {
		strcpy(cname, "rio_con_a");
		arg.srio.srio_if = "riomsg";
		arg.srio.channel = 1;
		arg.srio.src_mac = "02:00:01:00:0a:00";
		arg.srio.dst_mac = "02:00:01:00:25:00";
		arg.srio.letter = 2;                                                   
	}

	sprintf(lname, "sriocm/%s", cname);
	arg.srio.name = cname;

	if ((ret = linx_create_connection(LINX_CON_SRIO, &arg, &cons))) {
		syslog(LOG_ERR, " linx_create_connection:  %d  ", ret);
		goto done;
	}

	memset(&link_arg, 0, sizeof(link_arg));
	link_arg.name = msg->ctreq.name;
	link_arg.connections = lname;
	link_arg.features = "";
	link_arg.attributes = "";

	if ((ret = linx_create_link(&link_arg))) {
		linx_remove_connection(cons);
		goto done;
	}

	if ((p = malloc(sizeof(*p)))) {
		p->next = rioif.conn_list;
		p->owner_mbox = sndr;
		p->owner_mtor = (itc_monitor_id_t)-1;

		if (!get_by_mbox(sndr, 0))
			p->owner_mtor = itc_monitor(sndr, NULL);

		p->lspid = LINX_ILLEGAL_SPID;
		p->attref = LINX_ILLEGAL_ATTREF;
		p->link = (uint32_t)p;
		p->peer = peer;
		strncpy(p->name , msg->ctreq.name, EOLC_LINKNAME_SIZE);
		rioif.conn_list = p;
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

	msg->msgno = EOLC_CREATE_RSP;
	msg->ctrsp.result = ret;
	itc_send(&msg, sndr, ITC_MY_MBOX);

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



static void handle_new_link(LINX *lnx, union LINX_SIGNAL *sig)
{
	char *link_name = sig->nlink.buf + sig->nlink.name;
	struct rio_link *p = get_by_name(link_name, 0);

	if (p) {
		union itc_msg *ind;
		ind = itc_alloc(sizeof(ind->lkind), EOLC_LINKSTATE_IND);
		ind->lkind.link_id = p->link;
		ind->lkind.state = EOLC_LINK_UP;
		itc_send(&ind, p->owner_mbox, ITC_MY_MBOX);

		p->lspid	= linx_sender(lnx, &sig);
		p->attref	= linx_attach(lnx, NULL, p->lspid);
	}

	linx_request_new_link(lnx, sig->nlink.token);
}

static void handle_lost_link(LINX *lnx, union LINX_SIGNAL *sig)
{
	LINX_SPID spid = linx_sender(lnx, &sig);
	struct rio_link *p = get_by_spid(spid, 0);

	if (p) {
		union itc_msg *ind;
		ind = itc_alloc(sizeof(ind->lkind), EOLC_LINKSTATE_IND);
		ind->lkind.link_id = p->link;
		ind->lkind.state = EOLC_LINK_DOWN;
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
		syslog(LOG_ERR, "itc_init failure: %d\n",ret);
		goto exit;
	}

	me = itc_create_mailbox(EOLC_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "created mbox failure: %d\n",me);
		itc_exit();
		goto exit;
	}

	lnx = linx_open(RIO_LNH_NAME,0,NULL);
	if (lnx == NULL) {
		syslog(LOG_ERR, "linx_open failed, %d\n", errno);
		goto error_out;
	}

	nlref = linx_request_new_link(lnx, 0);
	if (nlref == LINX_ILLEGAL_NLREF) {
		syslog(LOG_ERR, "linx_request_new_link, %d\n", errno);
		goto error_out;
	}

	rioif.lnx = lnx;

	for(;;) {
		ret = linx_receive_w_tmo(lnx, &sig, 100, anysig);
		if (ret > 0)
			handle_linx_signal(lnx, sig);
	
		msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
		if (msg == NULL)
			continue;

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
 exit:
	for (;;) {
		syslog(LOG_ERR, "riolnh_linx - init failed, looping forever\n");
		sleep(600);
	}
	return -1;
}

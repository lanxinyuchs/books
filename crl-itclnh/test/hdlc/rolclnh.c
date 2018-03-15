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
#include <fcntl.h>
#include <ctype.h>
#include <pthread.h>
#include <syslog.h>
#include <poll.h>
#include <arpa/inet.h>
#include <itc.h>
#include <ulh_transport.h>
#include <ulh_lnh_msg.h>
#include <ulh_hdlc.h>
#include <ulh_cm.h>
#include <ulh_lnh.h>
#include <rhai-ecb.h>

#include "rolc-link-api.h"
#include "rolc-link-int.h"
#include "ecb-link-api.h"


#define TRACEPOINT_DEFINE
#include <com_ericsson_system_start.h>


#define HDLC_CM_NAME					"rolc_cm"
#define HDLC_LNH_NAME					"rolc_lh"

#define ROLC_CIDS       				64
#define ROLC_MTU        				1500
#define ECP_MTU	        				120

#define ETHERTYPE_ROLC					0xebcd

#define ECB_LINK_HDLC_ADDR				254

union itc_msg {
	uint32_t         		 			msgno;
	struct ecb_link_event				lkevt;

	struct rolc_create_req				ctreq;
	struct rolc_create_rsp				ctrsp;
	struct rolc_delete_req				dlreq;
	struct rolc_delete_rsp				dlrsp;
	struct rolc_test_done				tdone;
	struct rolc_spray_done				sdone;
	struct rolc_linkstate_ind			lkind;

	struct ulh_lnhmsg_createcm_req    	cmreq;
	struct ulh_lnhmsg_createcm_rsp    	cmrsp;
	struct ulh_lnhmsg_createlink_req  	lcreq;
	struct ulh_lnhmsg_createlink_rsp  	lcrsp;
	struct ulh_lnhmsg_destroylink_req 	ldreq;
	struct ulh_lnhmsg_destroylink_rsp 	ldrsp;
	struct ulh_lnhmsg_notify			ulind;
};


struct rolc_conn {
	uint32_t 				ulh_cid;
	uint8_t 				skip_rx;
	uint8_t 				skip_tx;
	uint8_t 				port;
	uint8_t 				addr;
};

struct rolc_tran {
	void					*handle;
	itc_mbox_id_t 			peer;
	itc_mbox_id_t 			rx_thread;
	struct rolc_conn 		*conns[ROLC_CIDS + 2];
};

struct link_data {
	struct link_data		*next;
	void					*owner;
	itc_mbox_id_t			owner_mbox;
	itc_monitor_id_t		owner_mtor;
	uint32_t 				linkid;
	uint32_t 				tcid;
	uint32_t 				port;
	uint32_t				addr;
	char					name[ROLC_LINKNAME_SIZE];
};


static itc_mbox_id_t 		server_mbox;
static struct link_data 	*links;
static struct rolc_tran		transp;


void set_rx_skip_mode(uint8_t addr, uint8_t mode)
{
	struct rolc_conn *conn = transp.conns[addr];
	if (conn)
		conn->skip_rx = mode;
}

void set_tx_skip_mode(uint8_t addr, uint8_t mode)
{
	struct rolc_conn *conn = transp.conns[addr];
	if (conn)
		conn->skip_tx = mode;
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

static int free_link(struct link_data *p)
{
	int ret = -EIO;
	uint32_t select[2] = {1, ULH_LNHMSG_DESTROYLINK_RSP};
	union itc_msg *msg = itc_alloc(	sizeof(msg->ldreq), 
									ULH_LNHMSG_DESTROYLINK_REQ
								  );

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

static int create_link(union itc_msg *msg, uint32_t *link_id)
{
	uint32_t select[2] = {1, ULH_LNHMSG_CREATECM_RSP};
	struct ulh_trans_addr rolc_addr;
	struct ulh_cm_hdlc_config *cfp;
	union itc_msg *req;
	itc_mbox_id_t client;
	struct link_data *p = NULL;
	uint32_t addr, port, cmid, tcid;
	int ret;

	client  = itc_sender(msg);
	addr = msg->ctreq.ri_addr;
	port = msg->ctreq.ri_port;

	if ((ret = collision_detect(addr, port, msg->ctreq.name)))
		return ret;


	memset(&rolc_addr, 0, sizeof(rolc_addr));
	rolc_addr.data[0] = (uint8_t)port;
	rolc_addr.data[1] = (uint8_t)addr;
	tcid = ulh_trans_create_conn("rolc0", &rolc_addr, &rolc_addr);

	if (tcid == ULH_TRANS_NOCONN) {
		ret = -EFAULT;
		goto equit;
	}

	req = itc_alloc(sizeof(req->cmreq)+sizeof(*cfp),
									ULH_LNHMSG_CREATECM_REQ);
	cfp = (struct ulh_cm_hdlc_config*)&req->cmreq.config;
	cfp->cmn.cfg_size 		= sizeof(*cfp);
	cfp->cmn.cfg_version 	= 0;
	cfp->cmn.cid 			= tcid;
	cfp->cmn.uref 			= 0;
	cfp->cmn.mbox 			= 0;
	cfp->primary 			= 1;
	cfp->hdlc_addr   		= addr;
	req->cmreq.seq			= 0;
	strcpy(req->cmreq.cm_name, HDLC_CM_NAME);
    sprintf(req->cmreq.cm_instance, "hdlc_%02u_%02u", port, addr);

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
	strcpy(req->lcreq.name, msg->ctreq.name);

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
		p->owner_mtor = (itc_monitor_id_t)-1;//itc_monitor(client, NULL);

	p->tcid = tcid;
	p->port = port;
	p->addr = addr;
	strncpy(p->name , msg->ctreq.name, ROLC_LINKNAME_SIZE);
	links = p;
	*link_id = p->linkid;
	return 0;

 equit:

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

static int rolc_create_conn(void *tref, uint32_t tcid,
       struct ulh_trans_addr *src, struct ulh_trans_addr *dst, void **cref)
{
	struct rolc_tran *tran = tref;
	struct rolc_conn *conn;

	(void)dst;

	conn = malloc(sizeof(struct rolc_conn));
	if (!conn)
		return -ENOMEM;

	memset(conn, 0, sizeof(*conn));
	conn->ulh_cid = tcid;
	conn->port = src->data[0];
	conn->addr = src->data[1];
	tran->conns[conn->addr] = conn;

	*cref = conn;
	return 0;
}

static int rolc_destroy_conn(void *tref, void *cref)
{
	struct rolc_tran *tran = tref;
	struct rolc_conn *conn = cref;

	if (conn == NULL)
		return -EINVAL;

	tran->conns[conn->addr] = NULL;
	free(conn);
	return 0;
}


struct hdlc_packet {
	uint8_t 	hdlc_address;
	uint8_t 	hdlc_control;
	uint16_t 	frag_bits;
	uint32_t 	msg_src;
	uint32_t 	msg_dst;
	uint32_t 	msg_size;
	uint16_t 	reserved;
	uint16_t 	type;
	uint32_t	addr;
	char		name[1];
};

#define TEST_QUERY_NAME		10

static int rolc_transmit(void *tref, void *cref, struct ulh_tbuff *tbuff)
{
	struct hdlc_packet *pkt;
	struct rolc_tran *tran  = tref;
	struct rolc_conn *conn  = cref;
	union itc_msg *msg;

	if (conn->skip_tx) {
		ulh_tbuff_free(tbuff);
		return 0;
	}

	msg = itc_alloc(tbuff->size + 4, 0);
	memcpy(msg, tbuff->data, tbuff->size);

	if (tbuff->size > sizeof(*pkt)) {
		pkt = (struct hdlc_packet*)msg;

		if ((pkt->msg_src == 0) && (pkt->msg_dst == 0))
			if (ntohs(pkt->type) == 1)
				pkt->type = htons(TEST_QUERY_NAME);
	}

	itc_send(&msg, tran->rx_thread, ITC_MY_MBOX);
	ulh_tbuff_free(tbuff);
	return 0;
}

static void rolc_tbuff_free(struct ulh_ref *ref)
{
	struct ulh_tbuff_rbuf *dbuf = container_of(ref,
			struct ulh_tbuff_rbuf, ref);

	free(dbuf);
}

static int rolc_alloc_buff(void *tref, void *cref, uint32_t size,
											struct ulh_tbuff *tbuff)
{
	struct ulh_tbuff_rbuf *dbuf;
	(void)tref;
	(void)cref;

	dbuf = malloc(sizeof(*dbuf) + size);
	if (!dbuf)
		return -ENOMEM;

	ulh_init_ref(&dbuf->ref, 1, rolc_tbuff_free);

	dbuf->buf = (uint8_t *)(dbuf + 1);
	tbuff->rbuf = dbuf;
	tbuff->data = dbuf->buf;
	tbuff->size = size;
	return 0;
}

static int rolc_getmtu(void *tref, void *cref)
{
	(void)tref;
	(void)cref;
	return ECP_MTU;
}

static void *rolc_rx_thread(void *param)
{
	struct rolc_tran *tran = param;
	struct rolc_conn *conn;
	struct ulh_tbuff tb;
	union itc_msg *msg;
	itc_mbox_id_t me;

	me = itc_create_mailbox("rolc_rx_thread", 0);
	if (me == ITC_NO_ID) {
		printf("create mbox failure. Terminating");
		pthread_exit(0);
	}

	for (;;) {
		uint8_t addr;
		uint32_t size;

		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		size = itc_size(msg) - 4;
		addr = *(uint8_t*)msg;

		if (addr >= ROLC_CIDS)
			continue;

		conn = tran->conns[addr];
		if (conn && !conn->skip_rx) {
			rolc_alloc_buff(0, 0, size, &tb);
			tb.size = size;
			memcpy(tb.data, msg, size);
			ulh_trans_deliver(conn->ulh_cid, &tb);
		}
		itc_free(&msg);
	}
	pthread_exit(0);
}

static struct ulh_trans_ops rolc_ops = {
	.create_conn 	= rolc_create_conn,
	.destroy_conn 	= rolc_destroy_conn,
	.transmit 		= rolc_transmit,
	.getmtu 		= rolc_getmtu,
	.alloc_buff 	= rolc_alloc_buff
};


static int rolc_initialize(int master)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct ecb_link_config cfg;
	union itc_msg *msg;
	uint32_t link_id;
	pthread_t rxthr;
	int ret;

	ret = ecb_link_init(&transp.handle);
	if (ret) {
		printf("ecb_link_init failed, %d\n", ret);
		return ret;
	}

	cfg.address = ECB_LINK_HDLC_ADDR;
	cfg.station = master ? ECB_STATION_PRIMARY : ECB_STATION_SECONDARY;
	strcpy(cfg.name, "ecblink");

	ret = ecb_link_create(transp.handle, &cfg, &link_id);
	if (ret) {
		printf("ecb_link_create(%s), %d\n", cfg.name, ret);
		goto err_out;
	}

	if ((ret = ulh_trans_register("rolc0", &rolc_ops, &transp)))
		goto err_out;


	if (pthread_create(&rxthr, NULL, rolc_rx_thread, &transp)) {
		ulh_trans_unregister("rolc0");
		goto err_out;
	}

	itc_locate_async("ecblink/rolc_rx_thread", NULL, ITC_MY_MBOX);
	msg = itc_receive(select, 50000, ITC_FROM_ALL);

	if (msg == NULL) {
		printf("failed to locate peer: slave/rolc_rx_thread\n");
		goto err_out;
	}

	transp.rx_thread = itc_sender(msg);
	itc_free(&msg);
	return 0;


  err_out:

	ecb_link_shutdown(transp.handle);
    return ret;
}


static inline uint64_t get_tick(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000ULL + ts.tv_nsec / 1000000;
}


int main(int argc, char *argv[])
{
	union itc_msg *msg, *rsp;
	itc_mbox_id_t me, snd;
	pthread_t test_thread;
	int ret, result = -1;
	void *lnh = NULL;
	char *mode = "Secondary";
	struct test_args targ = {0,0,0,0,0,200,10,2,0};
	int num_spray_tests = 5;
	int link_up = 1;

	uint64_t t_start = get_tick();

	/*
	** size, number of links & number of threads per link is hard coded
	*/

	/*
	if (argc > 1)
		targ.size = atoi(argv[1]);

	if (argc > 2)
		targ.nlink = atoi(argv[2]);

	if (argc > 3)
		targ.nthread = atoi(argv[3]);
	*/

	if (argc > 5) {
		mode = "Primary";
		targ.master = 1;
	}

	ret = itc_init(1024, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf( "itc_init failure: %d\n",ret);
		printf("Primary QUIT (Test: FAIL)\n");
		return -1;
	}

	me = itc_create_mailbox(ROLC_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		itc_exit();
		printf( "created mbox failure: %d\n",me);
		printf("Primary QUIT (Test: FAIL)\n");
		return -1;
	}

	ret = ulh_lnh_init(64);
	if (ret) {
		printf( "ulh_lnh_init() failed, %d\n", ret);
		goto error_out;
	}

	ret = ulh_hdlc_init(HDLC_CM_NAME);
	if (ret) {
		printf( "ulh_ecb_init() failed, %d\n", ret);
		goto error_out;
	}

	lnh = ulh_lnh_create(HDLC_LNH_NAME);
	if (!lnh) {
		printf( "ulh_lnh_create() failed\n");
		goto error_out;
	}

	for (;; sleep(1)) {
		server_mbox = itc_locate(HDLC_LNH_NAME);
		if (server_mbox != ITC_NO_ID)
			break;
	}

	ret = rolc_initialize(targ.master);
	if (ret < 0) {
		printf( "rolc_init: %d\n",ret);
		goto error_out;
	}

	targ.parent_mbox = me;
	event_system_start("rolclnh ready");

	if (targ.master) {
		if (pthread_create(&test_thread, NULL, test_prog_m, &targ)) {
			printf("create test prog thread, %d\n", -errno);
			goto error_out;
		}
	} else {
		if (pthread_create(&test_thread, NULL, test_prog_s, &targ)) {
			printf("create test prog thread, %d\n", -errno);
			goto error_out;
		}
	}

	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		snd = itc_sender(msg);

		switch(msg->msgno) {
			case ROLC_CREATE_REQ:
				rsp = itc_alloc(sizeof(rsp->ctrsp), ROLC_CREATE_RSP);
				rsp->ctrsp.result = create_link(msg, &rsp->ctrsp.linkid);
				strcpy(rsp->ctrsp.name, msg->ctreq.name);
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ROLC_DELETE_REQ:
				rsp = itc_alloc(sizeof(rsp->dlrsp), ROLC_DELETE_RSP);
				rsp->dlrsp.result 	= delete_link(msg);
				rsp->dlrsp.owner 	= msg->dlreq.owner;
				rsp->dlrsp.linkid 	= msg->dlreq.linkid;
				itc_send(&rsp, snd, ITC_MY_MBOX);
				break;

			case ECB_LINK_EVENT_MSG:
				if (link_up && (msg->lkevt.event == ECB_LINK_STATE_DOWN)) {
					printf("Link to peer lost\n");
					result = -ENOLINK;
					goto error_out;
				}
				break;

			case ROLC_TEST_DONE:
				link_up = 0;
				sleep(1);
				ecb_link_shutdown(transp.handle);
				result = msg->tdone.result;

				if (result)
					goto error_out;

				pthread_join(test_thread, NULL);
				if (pthread_create(&test_thread, NULL, test_spray, &targ)) {
					printf("create test prog thread, %d\n", -errno);
					goto error_out;
				}
				break;

			case ROLC_SPRAY_DONE:
				result = msg->sdone.result;

				if (result) 
					goto error_out;

				pthread_join(test_thread, NULL);
				if (--num_spray_tests > 0) {
					if (pthread_create(&test_thread, NULL, test_spray, &targ)) {
						printf("create test prog thread, %d\n", -errno);
						goto error_out;
					}
				} else {
					itc_free(&msg);
					goto error_out;
				}
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
	if (result == 0) {
		t_start = get_tick() - t_start;
		printf("%s QUIT (Test: PASS) (after %u seconds)\n", mode, (uint32_t)t_start/1000);
	} else
		printf("%s QUIT (Test: FAIL)\n", mode);

	itc_delete_mailbox(me);
	return 0;
}


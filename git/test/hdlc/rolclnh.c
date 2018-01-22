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
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>
#include <linux/if_arp.h>
#include <linux/if.h>
#include <net/ethernet.h>
#include <itc.h>
#include <ulh_transport.h>
#include <ulh_lnh_msg.h>
#include <ulh_hdlc.h>
#include <ulh_cm.h>
#include <ulh_lnh.h>
#include "rolc-link-api.h"
#include "rolc-link-int.h"


#define TRACEPOINT_DEFINE
#include <com_ericsson_system_start.h>


#define HDLC_CM_NAME					"rolc_cm"
#define HDLC_LNH_NAME					"rolc_lh"

#define ROLC_CIDS       				64
#define ROLC_MTU        				1500

#define ETHERTYPE_ROLC					0xebcd


union itc_msg {
	uint32_t         		 			msgno;
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



struct cm_header {
	uint8_t 				hdlc_address;
	uint8_t 				hdlc_control;
	uint16_t 				frag_bits;
	uint32_t 				msg_src;
	uint32_t 				msg_dst;
	uint32_t 				msg_size;
};

struct rolc_conn {
	uint32_t 				ulh_cid;
	uint8_t 				skip_rx;
	uint8_t 				skip_tx;
	uint8_t 				port;
	uint8_t 				addr;
};

struct __attribute__ ((__packed__))rolc_head {
	uint8_t 				dst[6];
	uint8_t 				src[6];
	uint16_t 				proto;
	uint8_t 				plen;
	uint8_t 				addr;
};

struct rolc_tran {
	int				 		sock;
	struct sockaddr_ll 		saddr;
	struct rolc_head		rhead;
	pthread_t				rx_thread;
	struct rolc_conn 		*conns[ROLC_CIDS];
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
	} else
		printf("Not freeing link\n");

	free(p);
	return ret;
}

static void get_mac(char *src, uint8_t *dst)
{
	char *p, *q;
	int cnt = 0;

	for (p = q = src; *q; q++)
		if (*q == ':') {
			*q = 0;
			dst[cnt++] = (uint8_t)strtoul(p, NULL, 16);
			p = ++q;
		}

	dst[cnt++] = (uint8_t)strtoul(p, NULL, 16);

	if (cnt != 6)
		printf("Warning..MAC address contains %d octets\n", cnt);

}

static int create_link(union itc_msg *msg, uint32_t *link_id) 
{
	uint32_t select[2] = {1, ULH_LNHMSG_CREATECM_RSP};
	struct ulh_trans_addr ecp_addr;
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


	memset(&ecp_addr, 0, sizeof(ecp_addr));
	ecp_addr.data[0] = (uint8_t)port;
	ecp_addr.data[1] = (uint8_t)addr;
	tcid = ulh_trans_create_conn("rolc0", &ecp_addr, &ecp_addr);

	if (tcid == ULH_TRANS_NOCONN) {
		printf( "ulh_trans_create_conn() failed");
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


static void 
print_data(uint8_t *data, uint32_t size, uint32_t noelem)
{
	char ascii[128];
	uint32_t k,j;

	
	if (size)
		printf("  CRC ERROR %d bytes\n  %p: ", size, (void*)data);

	for (k = 0, j = 0; k < size; k++, j++) {
		if (j == noelem) {
			ascii[j] = 0;
			printf("  \"%s\"\n  %p: ", ascii, (void*)(data + k));
			j = 0;
		}
		printf("%.2x ",data[k]);
		if (isprint((int)data[k]))
			ascii[j] = (char)data[k];
		else
			ascii[j] = '.';
	}
	if (j) {
		ascii[j] = 0;
		for (;j < noelem; j++)
			printf("   ");

		printf("  \"%s\"\n", ascii);
	}
	printf("\n");
}


static int ecp_create_conn(void *tref, uint32_t tcid, 
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

static int ecp_destroy_conn(void *tref, void *cref)
{
	struct rolc_tran *tran = tref;
	struct rolc_conn *conn = cref;

	if (conn == NULL)
		return -EINVAL;

	tran->conns[conn->addr] = NULL;
	free(conn);
	return 0;
}

#define CRC_INIT            	0xffffffff
#define CRC_HIGH            	0x80000000
#define CRC_POLY            	0x04c11db7


static uint32_t fcs32(char* p, int len)
{
    unsigned long i, j, c, bit;
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

static void calc_crc(void *msg, int length)
{
	uint32_t fcs;
	char *p = (char*)msg + length;

	fcs = htonl(fcs32((char*)msg, length));
	memcpy(p, &fcs, 4);		
}


#define HDLC_IFRAME     0
#define HDLC_SFRAME     1
#define HDLC_UFRAME     3

struct hdlc_header {
	uint8_t hdlc_address;
	uint8_t hdlc_control;
};

static inline int extract_seqno(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control >> 5) & 0x7;
}

static inline int extract_type(struct hdlc_header *hdlc)
{
	return (hdlc->hdlc_control & 1) ? (hdlc->hdlc_control & 3) : HDLC_IFRAME;
}

static int ecp_transmit(void *tref, void *cref, struct ulh_tbuff *tbuff)
{
	struct rolc_tran *tran  = tref;
	struct rolc_conn *conn  = cref;
	struct rolc_head *head;
	int tx_len = tbuff->size;

	if (conn->skip_tx) {
		ulh_tbuff_free(tbuff);
		return 0;
	}

	ulh_tbuff_push(tbuff, sizeof(*head));
	head = (struct rolc_head*)tbuff->data;

	*head = tran->rhead;
	head->addr = conn->addr;
	head->plen = (uint8_t)tx_len;

	calc_crc(tbuff->data, tbuff->size);

    tx_len = sendto(tran->sock, tbuff->data, tbuff->size + 4, 0,
					(struct sockaddr*)&tran->saddr, sizeof(tran->saddr));


	if (tx_len != (tbuff->size + 4))
		printf("sendto failed, (errno: %d) (tx: %d actual: %d)\n",
							errno, tbuff->size, tx_len);

	ulh_tbuff_pop(tbuff, sizeof(*head));
	ulh_tbuff_free(tbuff);
	return 0;
}

static void ecp_tbuff_free(struct ulh_ref *ref)
{
	struct ulh_tbuff_rbuf *dbuf = container_of(ref,
			struct ulh_tbuff_rbuf, ref);

	free(dbuf);
}

static int ecp_alloc_buff(void *tref, void *cref, uint32_t size, 
											struct ulh_tbuff *tbuff)
{
	struct ulh_tbuff_rbuf *dbuf;
	(void)tref;
	(void)cref;

	dbuf = malloc(sizeof(*dbuf) + size + 4 + sizeof(struct rolc_head));
	if (!dbuf)
		return -ENOMEM;
	ulh_init_ref(&dbuf->ref, 1, ecp_tbuff_free);

	dbuf->buf = (uint8_t *)(dbuf + 1);
	tbuff->rbuf = dbuf;
	tbuff->data = dbuf->buf + sizeof(struct rolc_head);
	tbuff->size = size;

	return 0;
}

static int ecp_getmtu(void *tref, void *cref)
{
	(void)tref;
	(void)cref;
	return ROLC_MTU;
}

static void *rolc_rx_thread(void *param)
{
	struct rolc_tran *tran = param;
	struct rolc_conn *conn;
	struct rolc_head *head;
	struct ulh_tbuff tb;
	itc_mbox_id_t me;
	int sock;
	int ret = -1;
	
	me = itc_create_mailbox("rolc_rx_thread", 0);
	if (me == ITC_NO_ID) {
		printf("create mbox failure. Terminating");
		pthread_exit(0);
	}

    if ((sock = socket(AF_PACKET, SOCK_RAW, htons(ETHERTYPE_ROLC))) < 0) {
		printf("socket failed, %d", -errno);
		return NULL;
	}

    if (bind(sock, (const struct sockaddr *)&transp.saddr, 
							sizeof(transp.saddr)) < 0) {
		printf("bind failed, %d", -errno);
		return NULL;
	}

	for (;;) {
		if (ecp_alloc_buff(0, 0, ROLC_MTU, &tb)) {
			sleep(1);
			continue;
		}
		ulh_tbuff_push(&tb, sizeof(*head));
		ret = recvfrom(sock, tb.data, tb.size, 0, NULL, NULL);

		if (ret > sizeof(*head)) {
			head  = (struct rolc_head*)tb.data;

			if (fcs32((char*)head, sizeof(*head) + 4 + head->plen)) {
				print_data((uint8_t*)head, sizeof(*head)+4+head->plen, 16);
				ulh_tbuff_free(&tb);
				//exit(8);
				continue;
			}

			if (ntohs(head->proto) != ETHERTYPE_ROLC) {
				ulh_tbuff_free(&tb);
				continue;
			}

			if (head->addr >= ROLC_CIDS) {
				ulh_tbuff_free(&tb);
				continue;
			}

			conn = tran->conns[head->addr];
			if (conn && !conn->skip_rx) {
				ulh_tbuff_pop(&tb, sizeof(*head));
				tb.size = head->plen;
				ulh_trans_deliver(conn->ulh_cid, &tb);
			} else
				ulh_tbuff_free(&tb);
		} else {
			printf("recvfrom eror, %d (%d)\n", ret, -errno);
			ulh_tbuff_free(&tb);
		}
	}
	pthread_exit(0);
}


static struct ulh_trans_ops ecp_ops = {
	.create_conn 	= ecp_create_conn,
	.destroy_conn 	= ecp_destroy_conn,
	.transmit 		= ecp_transmit,
	.getmtu 		= ecp_getmtu,
	.alloc_buff 	= ecp_alloc_buff
};



static int ecp_init(uint8_t *peer_mac)
{
    const char ifname[] = "eth0";
    struct ifreq ifr;
	int sd, rc;

    if ((sd = socket(AF_PACKET, SOCK_RAW, htons(ETHERTYPE_ROLC))) < 0)
        return -1;

    strncpy(ifr.ifr_name, ifname, sizeof(ifr.ifr_name));

	if ((rc = ioctl(sd, SIOCGIFINDEX, &ifr)) < 0)
		return -1;


	memset(&transp.saddr, 0, sizeof(transp.saddr));

    transp.saddr.sll_ifindex   = ifr.ifr_ifindex;
    transp.saddr.sll_family    = AF_PACKET;
    transp.saddr.sll_halen     = ETHER_ADDR_LEN;
    transp.saddr.sll_protocol  = htons(ETHERTYPE_ROLC);

    if ((rc = ioctl(sd, SIOCGIFHWADDR, &ifr)) < 0)
        goto err_out;

	if (ifr.ifr_hwaddr.sa_family != ARPHRD_ETHER)
		goto err_out;
	
	memcpy(transp.saddr.sll_addr, peer_mac, 6);
	memcpy(transp.rhead.src, ifr.ifr_hwaddr.sa_data, 6);
	memcpy(transp.rhead.dst, peer_mac, 6);

	transp.rhead.proto = htons(ETHERTYPE_ROLC);
	transp.sock = sd;

	if ((rc = ulh_trans_register("rolc0", &ecp_ops, &transp)))
		goto err_out;


	if (pthread_create(&transp.rx_thread, NULL, rolc_rx_thread, &transp)) {
		ulh_trans_unregister("rolc0");
		goto err_out;
	}

	return 0;

  err_out:

    close(sd);
    return rc;
}

int main(int argc, char *argv[])
{
	uint8_t peer_mac[8];
	union itc_msg *msg, *rsp;
	itc_mbox_id_t me, snd;
	pthread_t test_thread;
	int ret, result = -1;
	void *lnh = NULL;
	char *mode = "Secondary";
	struct test_args targ = {0,0,0,0,100,1,1,0};
	int num_spray_tests = 10;
	
	get_mac(argv[1], peer_mac);

	if (argc > 2)
		targ.size = atoi(argv[2]);

	if (argc > 3)
		targ.nlink = atoi(argv[3]);

	if (argc > 4)
		targ.nthread = atoi(argv[4]);

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

	ret = ecp_init(peer_mac);
	if (ret < 0) {
		printf( "ecp_init: %d\n",ret);
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

			case ROLC_TEST_DONE:
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
	if (result == 0)
		printf("%s QUIT (Test: PASS)\n", mode);
	else
		printf("%s QUIT (Test: FAIL)\n", mode);

	itc_delete_mailbox(me);
	return 0;
}

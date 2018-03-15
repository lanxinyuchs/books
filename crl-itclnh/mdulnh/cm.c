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
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdarg.h>
#include <syslog.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>
#include <linux/if_arp.h>
#include <linux/if.h>
#include <net/ethernet.h>
#include <ulh_dl_list.h>
#include <itc.h>
#include <itc_system.h>
#include "cm.h"
#include "mhp3_proto.h"
#include "trace.h"

#ifdef LTTNG
#include "mdu_lttng.h"
#endif



#define STATE_DISCONNECTING				   	0
#define STATE_DOWN							1
#define STATE_HELLO							2
#define STATE_UP							3


union itc_msg {
	uint32_t msg_no;
};


#ifdef LTTNG
static const char *type2str(uint8_t type)
{
	switch (type) {
	case MHP_CONN:	return "CONN";
	case MHP_DISC:	return "DISC";
	case MHP_ACK:	return "ACK";
	case MHP_NACK:	return "NACK";
	case MHP_DATA:	return "DATA";
	default:
		break;
	}
	return "????";
}

void mdu_print_header(char *name, const char *text,
                             char *data, uint32_t length)
{
	struct mhp_header   *hdr;
	struct mhp_conn     *chdr;
	struct mhp_disc     *dhdr;
	struct mhp_data     *data_hdr;
	struct mhp_data_hdr *ddata_hdr;

	hdr = (struct mhp_header*)data;

	MDU_TRACE(mhp_hdr, (char*)name, (char *)text, length,
             (char*)type2str(hdr->type), hdr->ns, hdr->nr);

	switch(hdr->type) {
	case MHP_CONN:
		chdr = (struct mhp_conn *)hdr;
		MDU_TRACE(mhp_connect, chdr->local_cid, chdr->remote_cid);
		break;

	case MHP_DISC:
		dhdr = (struct mhp_disc *)hdr;
		MDU_TRACE(mhp_disconnect, dhdr->local_cid, dhdr->remote_cid);
		break;

	case MHP_DATA:
		data_hdr  = (struct mhp_data *)hdr;
		ddata_hdr = (struct mhp_data_hdr *)(data_hdr + 1);
		MDU_TRACE(mhp_data, data_hdr->size, data_hdr->frag, 
                          ddata_hdr->src, ddata_hdr->dst, 
                          ddata_hdr->extra);
		break;
	default:
		break;
	}
}

void mdu_trace_error(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_mdu, mdu_error, file, line, buffer);
}

void mdu_trace_info(char *file, int line, const char *format, ...)
{
        va_list args;
        char buffer[512];

        va_start(args, format);
        vsnprintf(buffer, sizeof buffer, format, args);
        va_end(args);

        tracepoint(com_ericsson_mdu, mdu_info, file, line, buffer);
}

#endif

static inline uint64_t mhp_gettime(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000ULL + ts.tv_nsec / 1000000;
}


struct tx_buff *tx_alloc(int length)
{
	struct tx_buff *tb = malloc(length + sizeof(*tb));
	if (tb)
		tb->size = length;
	return tb;	
}

void tx_free(struct tx_buff *tb)
{
	if (tb)
		free(tb);
}
void tx_send(struct mhp_link *co, struct tx_buff *tb)
{
	int tx_siz = tb->size + sizeof(tb->txhdr);
	int tx_len;

	MDU_HEADER(co->name, "MHP TX", tb->data, tb->size);

	memcpy(&tb->txhdr, &co->txhdr, sizeof(tb->txhdr));
    tx_len = sendto(g.sock, &tb->txhdr, tx_siz, 0,
					(struct sockaddr*)&co->saddr, sizeof(co->saddr));


	if (tx_len != tx_siz) {
		MDU_ERROR("sendto failed, (errno: %d) (tx: %d actual: %d)\n",
							errno, tx_siz, tx_len);
	}
	co->rx_outstanding = 0;
	co->keep_alive = 50;
}

void tx_queue(struct tx_queue *que, struct tx_buff *tb)
{
	if (que->tail == NULL)
		que->head = tb;
	else
		que->tail->next = tb;

	que->tail = tb;
	tb->next = NULL;
}

struct tx_buff *tx_dequeue(struct tx_queue *que)
{
	struct tx_buff *p = que->head;

	if (p) {
		que->head = p->next;
		if (que->head == NULL)
			que->tail = NULL;
	}
	return p;
}

int ecom_init(struct mhp_link *co, struct mdu_link_config *cfg)
{
	memset(&co->saddr, 0 , sizeof(co->saddr));
	memset(&co->txhdr, 0 , sizeof(co->txhdr));

	memcpy(co->saddr.sll_addr, cfg->peer_mac, 6);
	memcpy(co->txhdr.dst, cfg->peer_mac, 6);

    co->saddr.sll_family    = AF_PACKET;
    co->saddr.sll_halen     = ETHER_ADDR_LEN;
    co->saddr.sll_protocol  = htons(ETHERTYPE_MDU);

	co->txhdr.proto = htons(ETHERTYPE_MDU);
	memcpy(co->txhdr.src, &g.own_mac, 6);

	co->saddr.sll_ifindex = g.ifindex;
	co->mtu = MDU_RMTU - ETHER_HDRSIZ;
	return 0;
}

static void mhp_tx_ctrl(struct mhp_link *co, uint8_t type)
{
	struct mhp_conn *conn;
	struct tx_buff *tb;

	tb = tx_alloc(sizeof(*conn));
	if (!tb)
		return;

	conn = (struct mhp_conn *) tb->data;
	conn->cmn.feature = 0;
	conn->cmn.prio = 0;
	conn->cmn.type = type;
	conn->cmn.ns = htons(co->tx_seq);
	conn->cmn.nr = htons(co->rx_seq);
	tb->size = sizeof(struct mhp_header);

	if ((type == MHP_CONN) || (type == MHP_DISC)) {
		tb->size = sizeof(struct mhp_conn);
		conn->local_cid = htons(co->local_cid);
		conn->remote_cid = htons(co->remote_cid);
	}


	tb->size = sizeof(struct mhp_conn);
	tx_send(co, tb);
	tx_free(tb);
}

static void mhp_reset_link(struct mhp_link *co)
{
	struct tx_buff *tb;

	co->rx_seq = 0;
	co->tx_seq += 10000;
	co->tx_outstanding = 0;
	co->rx_outstanding = 0;
	co->tx_ack = co->tx_seq - 1;
	co->conn_alive = 150;
	co->keep_alive = 50;

	for (tb = tx_dequeue(&co->txq); tb; tb = tx_dequeue(&co->txq))
		tx_free(tb);

	for (tb = tx_dequeue(&co->axq); tb; tb = tx_dequeue(&co->axq))
		tx_free(tb);

	if (co->rx_msg)
		itc_free(&co->rx_msg);

	co->remote_cid = 0;
	if (!++co->local_cid)
		co->local_cid = 1;
 }

static void mhp_disconnect(struct mhp_link *co)
{
	uc_disconnected(co);
	mhp_tx_ctrl(co, MHP_DISC);
	mhp_reset_link(co);
	co->cm_state = STATE_DOWN;
}

static void mhp_tx(struct mhp_link *co)
{
	uint64_t tnow = mhp_gettime();
	struct mhp_header *hdr;
	struct tx_buff *tb;
	
	for (;co->tx_outstanding < co->tx_wnd;) {
		if ((tb = tx_dequeue(&co->txq)) == NULL)
			break;

		hdr = (struct mhp_header *) &tb->data;
		hdr->ns = htons(++co->tx_seq);
		hdr->nr = htons(co->rx_seq);

		tx_queue(&co->axq, tb);
		tx_send(co, tb);
		tb->time = tnow + 500;
		co->tx_outstanding++;
	}
}

static void mhp_retransmit(struct mhp_link *co, int force)
{
	struct mhp_header *hdr;
	struct tx_buff *tb;
	uint64_t tnow;
	uint16_t nrtx = 0;
	
	for (tnow = mhp_gettime(), tb = co->axq.head; tb; tb = tb->next) {
		if (force || (tnow >= tb->time)) {
			hdr = (struct mhp_header *)&tb->data;
			hdr->nr = htons(co->rx_seq);

			if (co->rt_cnt++ > (5*co->tx_wnd)) {
				MDU_INFO("DISCONNECT rtx count: %d", co->rt_cnt);
				mhp_disc(co);
				break;
			}

			nrtx++;
			tx_send(co, tb);
			tb->time = tnow + 500;
		}
	}

	if (nrtx) {
		uint16_t ns_end = ntohs(hdr->ns);
		uint16_t ns_start = ns_end - (uint16_t)co->tx_outstanding + 1;
		(void)ns_start;
		
		MDU_INFO("%s retransmit ns: [%u...%u]  nr: %u out: %d  ack: %d"
				 "force: %d\n" ,co->name, ns_start, ns_end, co->rx_seq,
					co->tx_outstanding, co->tx_ack, force);
	}
}

static void mhp_process_ack(struct mhp_link *co, uint16_t nr)
{
	struct mhp_header *hdr;
	struct tx_buff *tb;

	if (nr == co->tx_ack)
		return;

	if ((uint16_t)(nr - co->tx_ack) > co->tx_outstanding) {
		MDU_INFO("%s BAD N(R) %u received. (ack: %u  out: %u",
				co->name, nr, co->tx_ack, co->tx_outstanding);
		return;
	}

	for (;co->tx_ack != nr;) {		
		if ((tb = tx_dequeue(&co->axq))) {
			hdr = (struct mhp_header*)tb->data;
			co->tx_ack = ntohs(hdr->ns);
			co->tx_outstanding--;
			co->rt_cnt = 0;
			tx_free(tb);
		} else
			break;
	}
	mhp_tx(co);
}

static void mhp_rx_conn(struct mhp_link *co, void *tb)
{
	struct mhp_conn *hdr = tb;
	uint16_t lcid = ntohs(hdr->local_cid);
	uint16_t rcid = ntohs(hdr->remote_cid);

	if (!lcid) {
		MDU_ERROR("%s: CONN with bad lconnid:%u", co->name, lcid);
		return;
	}

	switch (co->cm_state) {
	case STATE_DISCONNECTING:break;
	case STATE_DOWN:
	case STATE_HELLO:
		if (!rcid) {
			co->remote_cid = lcid;
			co->cm_state = STATE_HELLO;
			co->rx_seq = ntohs(hdr->cmn.ns);
			mhp_tx_ctrl(co, MHP_CONN);
			break;
		}
		if (rcid != co->local_cid)
			break;

		co->remote_cid = lcid;
		co->rx_seq = ntohs(hdr->cmn.ns);
		co->cm_state = STATE_UP;
		uc_connected(co);
		break;
	case STATE_UP:
		break;
	default:MDU_ERROR("BUG %d\n",0);
		break;
	}
}

static void mhp_rx_disc(struct mhp_link *co, void *tb)
{
	struct mhp_disc *hdr = tb;
	uint16_t lcid = ntohs(hdr->local_cid);
	uint16_t rcid = ntohs(hdr->remote_cid);

	if (!rcid) {
		MDU_ERROR("%s: DISC with bad rconnid:%u", co->name, rcid);
		return;
	}

	if ((lcid != co->remote_cid) || (rcid != co->local_cid))
		return;

	switch (co->cm_state) {
	case STATE_DISCONNECTING:break;
	case STATE_DOWN:
	case STATE_HELLO:
		co->cm_state = STATE_DOWN;
		mhp_reset_link(co);
		break;
	case STATE_UP:
		mhp_disc(co);
		break;
	default:MDU_ERROR("BUG %d\n",0);
		break;
	}
}

static void mhp_rx_ack(struct mhp_link *co, void *tb)
{
	struct mhp_header *hdr = tb;
	uint16_t nr = ntohs(hdr->nr);

	switch (co->cm_state) {
	case STATE_DISCONNECTING:break;
	case STATE_DOWN:
		break;
	case STATE_HELLO:
		if (nr != co->tx_seq) 
			break;
		co->cm_state = STATE_UP;
		uc_connected(co);
	case STATE_UP:
		mhp_process_ack(co, nr);
		break;
	default:MDU_ERROR("BUG %d\n",0);
		break;
	}
}

static void mhp_rx_nack(struct mhp_link *co, void *tb)
{
	struct mhp_header *hdr = tb;
	uint16_t nr = ntohs(hdr->nr);

	switch (co->cm_state) {
	case STATE_DISCONNECTING:break;
	case STATE_DOWN:
		break;
	case STATE_HELLO:
		if (nr != co->tx_seq) 
			break;
		co->cm_state = STATE_UP;
		uc_connected(co);
	case STATE_UP:
		mhp_process_ack(co, nr);
		mhp_retransmit(co, 1);
		break;
	default:MDU_ERROR("BUG %d\n", 0);
		break;
	}
}

static void mhp_rx_data(struct mhp_link *co, void *tb, int length)
{
	uint16_t nr, ns, size, frag, frag_no;
	uint32_t rsize, frag_max_size;
	union itc_msg *msg = NULL;
	struct mhp_data *hdr = tb;
	struct mhp_data_hdr *dhdr = (struct mhp_data_hdr *)(hdr + 1);
	uint8_t *payload = (uint8_t*)(dhdr + 1);

	if (length <= (sizeof(*hdr) + sizeof(*dhdr))) {
		MDU_ERROR("Too short frame (%d)...discaring", length);
		return;
	}

	nr = ntohs(hdr->cmn.nr);
	ns = ntohs(hdr->cmn.ns);
	size = ntohs(hdr->size);
	frag = ntohs(hdr->frag);
	frag_no = frag & MHP_DATA_FRAGNO_MASK;
	
	if (co->cm_state == STATE_HELLO) {
		if (nr == co->tx_seq) {
			uint16_t rseq = co->rx_seq;
			co->cm_state = STATE_UP;
			co->rx_seq += 1;
			uc_connected(co);
			co->rx_seq = rseq;
		}
	}

	if ((uint16_t)(co->rx_seq + 1) != ns) {

		if ((int16_t)(co->rx_seq - ns) >= 0) {
			MDU_INFO("MHP: %s: duplicate packet dropped"
				" (exp:%u ns:%u nr:%u ack:%u)", co->name, co->rx_seq + 1,
						ns, nr, co->tx_ack);
			mhp_tx_ctrl(co, MHP_ACK);
		} else if (!co->nack_sent) {
			MDU_INFO("MHP: %s: out of order packet dropped, NACKED"
				" (exp:%u ns:%u nr:%u ack:%u)", co->name, co->rx_seq + 1,
						ns, nr, co->tx_ack);
			mhp_tx_ctrl(co, MHP_NACK);
			co->nack_sent = 1;
		} else {
			MDU_INFO("MHP: %s: out of order packet dropped"
				" (exp:%u ns:%u nr:%u ack:%u)", co->name, co->rx_seq + 1,
						ns, nr, co->tx_ack);
		}

		return;
	}

	co->nack_sent = 0;

	if (frag_no == 0) {
		if (frag & MHP_DATA_F_FRAG) {
			MDU_ERROR("MHP: %s unfragmented DATA with FRAG set", co->name);
			mhp_disc(co);
			return;
		}
		if (co->rx_msg) {
			MDU_ERROR("MHP: %s unexpected unfragmented DATA", co->name);
			mhp_disc(co);
			return;
		}

		co->rx_head.size = size;
		co->rx_head.dst = ntohl(dhdr->dst);
		co->rx_head.src = ntohl(dhdr->src);

                if (co->rx_head.size > co->max_itc_msg_size)
                {
                   MDU_ERROR("MHP: %s too big message size %d, allowed %d, dst %d, src %d",
                             co->name,
                             co->rx_head.size,
                             co->max_itc_msg_size,
                             co->rx_head.dst,
                             co->rx_head.src);
                   mhp_disc(co);
                   return;
                }

		msg = itc_alloc(size, 0);
		memcpy(msg, payload, size);
	} else {
		frag_max_size = co->mtu - sizeof(struct mhp_data) -
							sizeof(struct mhp_data_hdr);

		if (frag & MHP_DATA_F_FRAG) {
			if (co->rx_msg) {
				MDU_ERROR("MHP: %s unexpected 1st fragment", co->name);
				mhp_disc(co);
				return;
			}
			/* size = first_size + (num_fragmens - 1) * frag_max_size */
			co->rx_head.size = size + (frag_no - 1) * frag_max_size;
			co->rx_head.dst = ntohl(dhdr->dst);
			co->rx_head.src = ntohl(dhdr->src);

                        if (co->rx_head.size > co->max_itc_msg_size)
                        {
                           MDU_ERROR("MHP: %s too big message size frag %d, allowed %d, dst 0x%x, src 0x%x",
                                     co->name,
                                     co->rx_head.size,
                                     co->max_itc_msg_size,
                                     co->rx_head.dst,
                                     co->rx_head.src);
                           mhp_disc(co);
                           return ;
                        }

			co->rx_msg = itc_alloc(co->rx_head.size, 0);
			co->rx_pos = (uint8_t *)co->rx_msg;
			co->rx_frag = frag_no;

		} else if (!co->rx_msg) {
			MDU_ERROR("MHP: %s unexpected non-1st frag", co->name);
			mhp_disc(co);
			return;
		}

		if (frag_no != co->rx_frag) {
			MDU_ERROR("MHP: %s unexpected FragNo (exp:%u rcv:%u)",
								co->name, co->rx_frag, frag_no);
			mhp_disc(co);
			return;
		}

		memcpy(co->rx_pos, payload, size);
		co->rx_pos += size;

		if (--co->rx_frag == 0) {
			rsize = co->rx_pos - (uint8_t *)co->rx_msg;
			msg = co->rx_msg;

			if (rsize != co->rx_head.size) {
				MDU_ERROR("MHP: %s unexpected reassembly size",co->name);
				mhp_disc(co);
				return;
			}
		}
	}

	co->rx_seq += 1;
	co->nack_sent = 0;

	if (++co->rx_outstanding > 3)
		mhp_tx_ctrl(co, MHP_ACK);

	mhp_process_ack(co, nr);

	if (msg) {
		co->rx_msg = NULL;
		uc_deliver(co, &co->rx_head, msg);
	}
}

int mhp_send(struct mhp_link *co, struct mhp_msghdr *hdr,
					union itc_msg *msg)
{
	struct mhp_data_hdr *ddhdr;
	struct mhp_data *dhdr;
	struct tx_buff *tb;
	uint32_t rsize = hdr->size;
	uint32_t hsize = sizeof(*ddhdr) + sizeof(*dhdr);
	uint32_t frag_size;
	uint32_t frag_max_size = co->mtu - hsize;
	uint32_t frags, cnt = 0;
	uint8_t *pos = (uint8_t*)msg;

	if (co->cm_state != STATE_UP) {
		if (co->cm_state != STATE_DISCONNECTING) {
			MDU_INFO("MHP: %s: dc_transmit in state %d",
					co->name, co->cm_state);
		}
		return -EINVAL;
	}

	frags = (rsize - 1) / frag_max_size + 1;
	if (frags > MHP_DATA_MAXFRAGS) {
		MDU_ERROR("MHP: %s: packet is too big", co->name);
		return -EINVAL;
	}

	while (rsize) {
		frag_size = rsize;

		if (frag_size % frag_max_size)
			frag_size = (frag_size % frag_max_size);

		if (frag_size > frag_max_size)
			frag_size = frag_max_size;

		tb = tx_alloc(frag_size + hsize);
		if (tb == NULL) {
			MDU_ERROR("%s: unable to allocate packet", co->name);
			return -ENOMEM;
		}

		dhdr = (struct mhp_data *) tb->data;
		ddhdr = (struct mhp_data_hdr *) (dhdr + 1);

		memset(dhdr, 0, hsize);
		dhdr->cmn.type = MHP_DATA;
		dhdr->size = htons(frag_size);

		/* fill frag word if fragmented message */
		if (frags > 1)
			dhdr->frag = htons((frags-cnt)|(MHP_DATA_F_FRAG & (cnt-1)));

		if (cnt == 0) {
			ddhdr->src   = htonl(hdr->src);
			ddhdr->dst   = htonl(hdr->dst);
			ddhdr->extra = htonl(hdr->size);
		}

		memcpy(ddhdr + 1, pos, frag_size);
		pos += frag_size;
		tx_queue(&co->txq, tb);
		rsize -= frag_size;
		cnt++;
	}

	mhp_tx(co);
	return 0;
}

void mhp_tick(struct mhp_link *co)
{
	if (co->cm_state == STATE_UP) {
		co->keep_alive--;
		mhp_retransmit(co, 0);

		if (co->rx_outstanding || (co->keep_alive == 0))
			mhp_tx_ctrl(co, MHP_ACK);

		if (!--co->conn_alive) {
			MDU_ERROR("Alive counter hits zero (%d).. DISCONNECTING",
				co->conn_alive);
			mhp_disc(co);
		}

		return;
	}
	switch (co->cm_state) {
	case STATE_DISCONNECTING:
		mhp_disconnect(co);
		break;
	case STATE_DOWN:
		mhp_tx_ctrl(co, MHP_CONN);
		break;
	case STATE_HELLO:
		break;
	default:MDU_ERROR("BUG %d\n", 0);
		break;
	}
}

void mhp_disc(struct mhp_link *co)
{
	co->cm_state = STATE_DISCONNECTING;
}

void mhp_exit(struct mhp_link *mhp)
{
	mhp_reset_link(mhp);
}

int mhp_init(struct mhp_link *mhp)
{
	mhp->tx_wnd   = 8;
	mhp_reset_link(mhp);
	mhp->cm_state = STATE_DOWN;
	return 0;
}

void rx_packet(void *buffer, int len)
{
	struct ether_hdr *pkt = (struct ether_hdr*)buffer;
	uint8_t *ebh = (uint8_t*)(pkt + 1);
	int hlen = sizeof(*pkt);
	struct mhp_link *co;
	uint16_t proto;
	uint32_t cnt;

	static uint32_t link_idx = 0;

	proto = ntohs(pkt->proto);
	if (proto == ETHERTYPE_802_1Q) {
		uint16_t *proto_p = (uint16_t *)ebh;
		hlen += 4;
		ebh  += 4;
		proto = ntohs(proto_p[1]);
	}

	if (proto != ETHERTYPE_MDU) {
		MDU_TRACE(eth_hdr, "Discarding Rx", pkt->dst, pkt->src, proto);
		MDU_ERROR("Discarding Rx 0x%x", proto);
		return;
	}

	for (cnt = 0; cnt < MAX_LINK; cnt++) {
			co = g.links[link_idx];
			if (co && !memcmp(&pkt->src, co->txhdr.dst, 6))
				break;

			link_idx = (link_idx + 1) & (MAX_LINK - 1);
			co = NULL;
	}

	if (co && (co->cm_state != STATE_DISCONNECTING)) {
			struct mhp_header *hdr = (struct mhp_header *)ebh;
			len -= hlen;
			co->conn_alive = 200;

			MDU_HEADER(co->name, "MHP RX", hdr, len);

			switch (hdr->type) {
			case MHP_DATA:
				mhp_rx_data(co, hdr, len);
				break;
			case MHP_CONN:
				if (len >= sizeof(struct mhp_conn))
					mhp_rx_conn(co, hdr);
				break;
			case MHP_DISC:
				if (len >= sizeof(struct mhp_disc))
					mhp_rx_disc(co, hdr);
				break;
			case MHP_ACK:
				mhp_rx_ack(co, hdr);
				break;
			case MHP_NACK:
				mhp_rx_nack(co, hdr);
				break;
			default:
				MDU_ERROR("MHP: %s: unknown packet type 0x%x",
					co->name, hdr->type);
				break;
			}
	} else if (co) {
				MDU_INFO("MDU: Discarding %s %02x:%02x:%02x:%02x:%02x:%02x",
					co->name, pkt->src[0],pkt->src[1],pkt->src[2],
					pkt->src[3],pkt->src[4],pkt->src[5]);
	}
}

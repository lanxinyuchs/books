#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <time.h>
#include <arpa/inet.h>

#include <itc.h>

#include "ulh_cm.h"
#include "ulh_transport.h"
#include "ulh_mhp.h"
#include "ulh_mhp_proto.h"
#include "ulh_timer.h"

#include "ulh_trace.h"
#ifdef LTTNG
#include "ulh_lttng.h"
#include "../libitclnh/ulh_rlnh.h"
#endif

#define STATE_INIT	0
#define STATE_IDLE	1
#define STATE_DOWN	2
#define STATE_HELLO	3
#define STATE_UP	4



#define ULH_MHPMAXLINKS	1

#ifdef LTTNG
#define MHP_PRINT_HEADER(name, prfx, tb) mhp_print_header(name, prfx, tb)
#define RLNH_PRINT_HEADER(name, p, h, b) rlnh_print_header(name, p, h, b)
#else
#define MHP_PRINT_HEADER(name, prfx, tb)
#define RLNH_PRINT_HEADER(name, p, h, b)
#endif


union itc_msg {
	uint32_t msg_no;
};

struct ulh_mhp_link {
	struct ulh_cm_uc_ops *uc;
	void *uc_data;
	int connected;

	struct ulh_tbuff_queue tx_queue;

	/* reassembly */
	union itc_msg *rx_msg;
	uint8_t *rx_pos;
	uint16_t rx_frag_no;
	uint32_t rx_size;
	struct ulh_cm_msghdr rx_cmhdr;
};

struct ulh_mhp_conn {
	int state;

	uint32_t cid;
	int mtu;
	int active;

	/* */
	uint16_t rx_seq;
	uint16_t rx_outstanding;
	uint16_t tx_seq;
	uint16_t tx_outstanding;
	uint16_t tx_ack;
	uint16_t tx_wnd;

	uint16_t nack_sent;
	struct ulh_tbuff_queue tx_unack_queue;
	uint32_t rt_cnt;

	/* */
	uint16_t lconnid;
	uint16_t rconnid;

	/* links */
	uint16_t nlinks;
	struct ulh_mhp_link links[ULH_MHPMAXLINKS];

	struct ulh_timerqueue *tqueue;
	struct ulh_tbuff_pool tbpool;

	struct ulh_timer conn_timer;
	struct ulh_timer ack_timer;
	struct ulh_timer rt_timer;
	struct ulh_timer kpa_timer;
	struct ulh_timer sup_timer;

	uint32_t rt_tmo;
	uint32_t rt_limit;
	uint32_t ack_tmo;
	uint32_t kpa_tmo;
	uint32_t sup_tmo;
	uint32_t conn_tmo;

	char *name;
	itc_mbox_id_t mbox;
	unsigned long uref;
};

#define mhp_timer_arm(_co, _timer, _tmo) \
	ulh_timer_arm((_timer), (_co)->tqueue, (_tmo))

#define mhp_timer_cancel(_co, _timer) \
	ulh_timer_cancel((_timer))

#ifdef LTTNG

static const char *__mhp_type2str(uint8_t type)
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

static void mhp_print_header(const char *name, const char *prfx,
                             struct ulh_tbuff *tb)
{
	struct timespec      ts;
	struct mhp_header   *hdr;
        struct mhp_conn     *conn_hdr;
        struct mhp_disc     *disc_hdr;
        struct mhp_data     *data_hdr;
        struct mhp_data_hdr *ddata_hdr;

	clock_gettime(CLOCK_MONOTONIC, &ts);
	hdr = (struct mhp_header *) ulh_tbuff_get(tb);

        /* The trace adapts the sequence numbers to be in network byte order. */
        ULH_TRACE(mhp_hdr, (char *)name, (char *)prfx, ulh_tbuff_len(tb),
                  (char *)__mhp_type2str(hdr->type), hdr->type,
                  hdr->prio, hdr->ns, hdr->nr);

        switch(hdr->type) {
        case MHP_CONN:
                conn_hdr = (struct mhp_conn *)hdr;
                ULH_TRACE(mhp_connect, conn_hdr->lconnid, 
                          conn_hdr->rconnid);
                break;

        case MHP_DISC:
                disc_hdr = (struct mhp_disc *)hdr;
                ULH_TRACE(mhp_disconnect, disc_hdr->lconnid, 
                          disc_hdr->rconnid);
                break;
        case MHP_DATA:
                data_hdr  = (struct mhp_data *)hdr;
                ddata_hdr = (struct mhp_data_hdr *)(data_hdr + 1);
                ULH_TRACE(mhp_data, data_hdr->size, data_hdr->frag, 
                          ddata_hdr->src, ddata_hdr->dst, 
                          ddata_hdr->extra);
                break;
        default:
                break;
        }
}

void rlnh_print_header(char *name, char *prfx,
                       struct ulh_cm_msghdr *chdr, uint8_t *buf)
{
        struct rlnh_header            *rhdr;        
        struct rlnh_msg_init          *rihdr;
        struct rlnh_msg_initreply     *rirhdr;
        struct rlnh_msg_publish       *rphdr;
        struct rlnh_msg_unpublish     *ruhdr;
        struct rlnh_msg_unpublish_ack *ruahdr;
        struct rlnh_msg_queryname     *rqhdr;

        rhdr = (struct rlnh_header *)buf;

	switch (ntohs(rhdr->type)) {
	case RLNH_INIT: 
                rihdr = (struct rlnh_msg_init *)rhdr;
                ULH_TRACE(rlnh_init, name, prfx,
                          rhdr->reserved, rhdr->type,
                          rihdr->version);
                break;
	case RLNH_INIT_REPLY:
                rirhdr = (struct rlnh_msg_initreply *)rhdr;
                ULH_TRACE(rlnh_init_reply, name, prfx,
                          rhdr->reserved, rhdr->type,
                          rirhdr->status, "");
                break;
	case RLNH_PUBLISH:
                rphdr = (struct rlnh_msg_publish *)rhdr;
                ULH_TRACE(rlnh_publish, name, prfx,
                          rhdr->reserved, rhdr->type,
                          rphdr->laddr, rphdr->name);
                break;
	case RLNH_QUERY_NAME:
                rqhdr = (struct rlnh_msg_queryname *)rhdr;
                ULH_TRACE(rlnh_query, name, prfx,
                          rhdr->reserved, rhdr->type,
                          rqhdr->laddr, rqhdr->name);
                break;
	case RLNH_UNPUBLISH:
                ruhdr = (struct rlnh_msg_unpublish *)rhdr;
                ULH_TRACE(rlnh_unpublish, name, prfx,
                          rhdr->reserved, rhdr->type,
                          ruhdr->laddr);
                break;
	case RLNH_UNPUBLISH_ACK:
                ruahdr = (struct rlnh_msg_unpublish_ack *)rhdr;
                ULH_TRACE(rlnh_unpublish_ack, name, prfx,
                          rhdr->reserved, rhdr->type,
                          ruahdr->laddr);
                break;
	default:
                ULH_TRACE(rlnh_data, name, prfx,
                          chdr->src, chdr->dst, chdr->size,
                          *((uint32_t *)buf));
		break;
	}
        
}

#endif

static void __mhp_uc_connected(struct ulh_mhp_conn *co)
{
	int i;

	for (i = 0; i < co->nlinks; i++) 
		co->links[i].uc->uc_connected(co->links[i].uc_data);
}

static void __mhp_uc_disconnected(struct ulh_mhp_conn *co)
{
	int i;

	for (i = 0; i < co->nlinks; i++) 
		co->links[i].uc->uc_disconnected(co->links[i].uc_data);
}

static void mhp_tx_conn(struct ulh_mhp_conn *co)
{
	struct ulh_tbuff *tb;
	struct mhp_conn *conn;

	tb = ulh_tbuff_pool_get(&co->tbpool);
	if (!tb)
		return;
	if (ulh_tbuff_alloc(co->cid, sizeof(*conn), tb)) {
		ulh_tbuff_pool_put(&co->tbpool, tb);
		return;
	}

	conn = (struct mhp_conn *) ulh_tbuff_get(tb);
	conn->cmn.feature = 0;
	conn->cmn.prio = 0;
	conn->cmn.type = MHP_CONN;
	conn->cmn.ns = htons(co->tx_seq);
	conn->cmn.nr = htons(co->rx_seq);

	conn->lconnid = htons(co->lconnid);
	conn->rconnid = htons(co->rconnid);

	MHP_PRINT_HEADER(co->name, "MHP TX", tb);

	ulh_trans_transmit(co->cid, tb);
	ulh_tbuff_pool_put(&co->tbpool, tb);
}

static void mhp_tx_disc(struct ulh_mhp_conn *co)
{
	struct ulh_tbuff *tb;
	struct mhp_disc *disc;

	tb = ulh_tbuff_pool_get(&co->tbpool);
	if (!tb)
		return;
	if (ulh_tbuff_alloc(co->cid, sizeof(*disc), tb)) {
		ulh_tbuff_pool_put(&co->tbpool, tb);
		return;
	}

	disc = (struct mhp_disc *) ulh_tbuff_get(tb);
	disc->cmn.feature = 0;
	disc->cmn.prio = 0;
	disc->cmn.type = MHP_DISC;
	disc->cmn.ns = htons(co->tx_seq);
	disc->cmn.nr = htons(co->rx_seq);

	disc->lconnid = htons(co->lconnid);
	disc->rconnid = htons(co->rconnid);

	MHP_PRINT_HEADER(co->name, "MHP TX", tb);

	ulh_trans_transmit(co->cid, tb);
	ulh_tbuff_pool_put(&co->tbpool, tb);
}

static void mhp_tx_ack(struct ulh_mhp_conn *co)
{
	struct ulh_tbuff *tb;
	struct mhp_header *ack;

	tb = ulh_tbuff_pool_get(&co->tbpool);
	if (!tb)
		return;
	if (ulh_tbuff_alloc(co->cid, sizeof(*ack), tb)) {
		ulh_tbuff_pool_put(&co->tbpool, tb);
		return;
	}

	ack = (struct mhp_header *) ulh_tbuff_get(tb);
	ack->feature = 0;
	ack->prio = 0;
	ack->type = MHP_ACK;
	ack->ns = htons(co->tx_seq);
	ack->nr = htons(co->rx_seq);
	
	MHP_PRINT_HEADER(co->name, "MHP TX", tb);

	ulh_trans_transmit(co->cid, tb);
	ulh_tbuff_pool_put(&co->tbpool, tb);
	co->rx_outstanding = 0;
}

static void mhp_tx_nack(struct ulh_mhp_conn *co)
{
	struct ulh_tbuff *tb;
	struct mhp_header *nack;

	tb = ulh_tbuff_pool_get(&co->tbpool);
	if (!tb)
		return;
	if (ulh_tbuff_alloc(co->cid, sizeof(*nack), tb)) {
		ulh_tbuff_pool_put(&co->tbpool, tb);
		return;
	}

	nack = (struct mhp_header *) ulh_tbuff_get(tb);
	nack->feature = 0;
	nack->prio = 0;
	nack->type = MHP_NACK;
	nack->ns = htons(co->tx_seq);
	nack->nr = htons(co->rx_seq);
	
	MHP_PRINT_HEADER(co->name, "MHP TX", tb);

	ulh_trans_transmit(co->cid, tb);
	ulh_tbuff_pool_put(&co->tbpool, tb);
}

static void __mhp_reset(struct ulh_mhp_conn *co)
{
	struct ulh_tbuff *tb;
	int i;

	co->rx_seq = 0;
	co->tx_seq += 10000;
	co->tx_outstanding = 0;
	co->rx_outstanding = 0;
	co->tx_ack = co->tx_seq - 1;
	co->rt_cnt = 0;

	/* flush TX queues */
	while ((tb = ulh_tbuff_dequeue(&co->tx_unack_queue))) {
		ulh_tbuff_free(tb);
		ulh_tbuff_pool_put(&co->tbpool, tb);
	}

	for (i = 0; i < co->nlinks; i++) {
		while ((tb = ulh_tbuff_dequeue(&co->links[i].tx_queue))) {
			ulh_tbuff_free(tb);
			ulh_tbuff_pool_put(&co->tbpool, tb);
		}
		if (co->links[i].rx_msg)
			itc_free(&co->links[i].rx_msg);
		co->links[i].rx_msg = NULL;
	}

	co->rconnid = 0;
	co->lconnid++;
	if (!co->lconnid)
		co->lconnid = 1;
}

static void mhp_disconnect(struct ulh_mhp_conn *co, int reconnect)
{
	mhp_timer_cancel(co, &co->conn_timer);
	mhp_timer_cancel(co, &co->ack_timer);
	mhp_timer_cancel(co, &co->rt_timer);
	mhp_timer_cancel(co, &co->kpa_timer);
	mhp_timer_cancel(co, &co->sup_timer);

	switch (co->state) {
	case STATE_INIT:
	case STATE_IDLE:
		return;
	case STATE_DOWN:
		__mhp_reset(co);
		break;
	case STATE_HELLO:
		mhp_tx_disc(co);
		co->state = STATE_DOWN;
		__mhp_reset(co);
		break;
	case STATE_UP:
		mhp_tx_disc(co);

		co->state = STATE_DOWN;
		__mhp_reset(co);
		__mhp_uc_disconnected(co);
		break;
	}

	if (co->state != STATE_DOWN)
		return;
	
	if (reconnect)
		mhp_timer_arm(co, &co->conn_timer, co->conn_tmo);
	else {
		co->state = STATE_IDLE;
		ulh_trans_detach(co->cid, co->mbox, co->uref);
	}
}

static void mhp_tx(struct ulh_mhp_conn *co)
{
	struct ulh_tbuff *tb;
	struct mhp_header *hdr;
	uint16_t prio = 0 /* XXX highest? */;

	if (co->tx_outstanding >= co->tx_wnd)
		return;

	do {
next_link:
		tb = ulh_tbuff_dequeue(&co->links[prio].tx_queue);
		if (!tb) {
			prio++;
			if (prio == co->nlinks)
				break;
			goto next_link;
		}

		/* cancel ack timer */
		mhp_timer_cancel(co, &co->ack_timer);

		/* step TX sequence */
		co->tx_seq = co->tx_seq + 1;

		/* update header */
		hdr = (struct mhp_header *) ulh_tbuff_get(tb);
		hdr->nr = htons(co->rx_seq);
		hdr->ns = htons(co->tx_seq);

		/* add to unack queue */
		ulh_tbuff_queue(&co->tx_unack_queue, tb);

		/* increment ref to avoid tb to be freed by transport */
		ulh_tbuff_hold(tb);
		
		MHP_PRINT_HEADER(co->name, "MHP TX", tb);

		/* send */
		ulh_trans_transmit(co->cid, tb);

		co->tx_outstanding++;
	} while (co->tx_outstanding < co->tx_wnd);

	/* (re-)arm retransmission timer */
	if (co->rt_tmo)
		mhp_timer_arm(co, &co->rt_timer, co->rt_tmo);
}

static void mhp_retransmit(struct ulh_mhp_conn *co)
{
	struct ulh_tbuff *tb;
	struct mhp_header *hdr;
        int i;

        if(co->tx_outstanding == 0) {
                co->rt_cnt = 0;
                return;
        }

	co->rt_cnt++;
	if (co->rt_cnt > co->rt_limit) {
		ULH_TRACE_ERROR("MHP: %s retransmission limit reached\n",
				co->name);
		mhp_disconnect(co, 1);
		return;
	}

        for(i=0 ; i<co->tx_outstanding ; i++) {
                tb = ulh_tbuff_dequeue(&co->tx_unack_queue);
                if (!tb) {
                        ULH_TRACE_ERROR("");
                        return;
                }

                /* update header */
                hdr = (struct mhp_header *) ulh_tbuff_get(tb);
                hdr->nr = htons(co->rx_seq);

                /* increment ref to avoid fb to be freed by transport */
                ulh_tbuff_hold(tb);
		
                MHP_PRINT_HEADER(co->name, "MHP TX", tb);
                ulh_trans_transmit(co->cid, tb);
                
                ulh_tbuff_queue(&co->tx_unack_queue, tb);
        }

	if (co->rt_tmo)	
		mhp_timer_arm(co, &co->rt_timer, co->rt_tmo);
}

static void mhp_process_ack(struct ulh_mhp_conn *co,
		uint16_t nr)
{
	struct ulh_tbuff *tb;
	struct mhp_header *hdr;
	uint16_t seq;
	int end = 0;

	if (co->tx_ack == nr) {
		mhp_timer_arm(co, &co->sup_timer, co->sup_tmo);
		return;
	}

	co->tx_ack = nr;

	/* free acknowledged packets */
	while (!end && (tb = ulh_tbuff_dequeue(&co->tx_unack_queue))) {
		hdr = (struct mhp_header *) ulh_tbuff_get(tb);
		seq = ntohs(hdr->ns);

		if (seq == co->tx_ack)
			end = 1;

		ulh_tbuff_free(tb);
		ulh_tbuff_pool_put(&co->tbpool, tb);

		co->tx_outstanding--;

		/* reset retransmission counter, as we are having progress */
		co->rt_cnt = 0;
	}

	/* send more */	
	mhp_tx(co);

	/* restart keep-alive timer */
	mhp_timer_arm(co, &co->kpa_timer, co->kpa_tmo);
	mhp_timer_arm(co, &co->sup_timer, co->sup_tmo);
}

static int mhp_process_data(struct ulh_mhp_conn *co,
		uint16_t ns, struct ulh_tbuff *tb)
{
	struct mhp_data *hdr;
	struct mhp_data_hdr *dhdr;
	uint16_t frag, size, frag_no;
	uint8_t prio;
	union itc_msg *msg;
	struct ulh_mhp_link *link;
	int complete = 0;
	uint32_t hdr_size = sizeof(struct mhp_data) +
		sizeof(struct mhp_data_hdr);
	uint32_t frag_max_size = co->mtu - hdr_size;

	/* check if in-order */
	if ((uint16_t)(co->rx_seq + 1) != ns) {
		ULH_TRACE_INFO("MHP: %s: out of order packet dropped, nack sent "
				"(rx_seq:%u ns:%u)\n", co->name,
				co->rx_seq, ns);
                if(!co->nack_sent) {
                        co->nack_sent = 1;
                        mhp_tx_nack(co);
                }

                /* Return -1 to avoid generating an ack for this packet */
		return -1;
	}
        co->nack_sent = 0;

	hdr = (struct mhp_data *) ulh_tbuff_get(tb);
	frag = ntohs(hdr->frag);
	size = ntohs(hdr->size);
	frag_no = frag & MHP_DATA_FRAGNO_MASK;

	prio = hdr->cmn.prio;
	if (prio >= co->nlinks) {
                
		ULH_TRACE_ERROR("MHP: %s: DATA with unexpected prio field, %u\n",
				co->name, prio);
		mhp_disconnect(co, 1);
		return -1;
	}

	link = &co->links[prio];

	dhdr = (struct mhp_data_hdr *) (hdr + 1);
	ulh_tbuff_pop(tb, sizeof(struct mhp_data) +
			sizeof(struct mhp_data_hdr));

	if (!size || size > ulh_tbuff_len(tb)) {
		ULH_TRACE_ERROR("MHP: %s: incorrect DATA size - %u, payload "
				"size - %u\n",
				co->name, size, ulh_tbuff_len(tb));
		mhp_disconnect(co, 1);
		return -1;
	}

	/* take simple case first */
	if (!frag_no) {
		if (frag & MHP_DATA_F_FRAG) {
			ULH_TRACE_ERROR("MHP: %s unfragmented DATA with FRAG "
					"bit set\n", co->name);
			mhp_disconnect(co, 1);
			return -1;
		}
		if (link->rx_msg) {
			ULH_TRACE_ERROR("MHP: %s unexpected unfragmented DATA\n",
					co->name);
			mhp_disconnect(co, 1);
			return -1;
		}

		link->rx_cmhdr.size = size;
		link->rx_cmhdr.dst = ntohl(dhdr->dst);
		link->rx_cmhdr.src = ntohl(dhdr->src);

		link->rx_msg = itc_alloc(ulh_tbuff_len(tb), 0);
		memcpy(link->rx_msg, ulh_tbuff_get(tb), size);
		complete = 1;
	} else {
		/* first fragment? */
		if (frag & MHP_DATA_F_FRAG) {
			if (link->rx_msg) {
				ULH_TRACE_ERROR("MHP: %s unexpected first "
						"fragment\n", co->name);
				mhp_disconnect(co, 1);
				return -1;
			}
			/* 
			 * calculate total packet size:
			 * total_size = first_fragment_size + 
			 *           (number_of_fragmens - 1) * frag_max_size
			 */
			link->rx_size = size + (frag_no - 1) * frag_max_size;
			link->rx_msg = itc_alloc(link->rx_size, 0);
			link->rx_pos = (uint8_t *) link->rx_msg;
			link->rx_frag_no = frag_no;

			/* link header */
			link->rx_cmhdr.size = link->rx_size;
			link->rx_cmhdr.dst = ntohl(dhdr->dst);
			link->rx_cmhdr.src = ntohl(dhdr->src);

		} else if (!link->rx_msg) {
			ULH_TRACE_ERROR("MHP: %s unexpected non-first "
					"fragment\n", co->name);
			mhp_disconnect(co, 1);
			return -1;
		}

		/* check frag_no */
		if (frag_no != link->rx_frag_no) {
			ULH_TRACE_ERROR("MHP: %s unexpected FragNo "
					"(exp:%u rcv:%u)\n", co->name,
					link->rx_frag_no, frag_no);
			mhp_disconnect(co, 1);
			return -1;
		}

		/* copy data */
		memcpy(link->rx_pos, ulh_tbuff_get(tb), size);
		link->rx_pos += size;
		link->rx_frag_no--;

		if (!link->rx_frag_no) {
			uint32_t rsize = link->rx_pos - (uint8_t *)link->rx_msg;
			complete = 1;
			/* sanity check */
			if (rsize != link->rx_size) {
				if ((rsize + 1) != link->rx_size) {
					ULH_TRACE_ERROR("MHP: %s unexpected reassembly size\n",
						co->name);
					mhp_disconnect(co, 1);
					return -1;
				}
				link->rx_size 		= rsize;
				link->rx_cmhdr.size = rsize;
			}
		}
	}

	/* packet is considered to be valid at this point, ack it */
	co->rx_seq += 1;

	if (co->rx_outstanding++ == 0) {
		mhp_timer_arm(co, &co->ack_timer, co->ack_tmo);
	} else if (co->rx_outstanding >= 4) {
		mhp_timer_cancel(co, &co->ack_timer);
		mhp_tx_ack(co);
	}

	/* deliver */
	if (complete) {
		msg = link->rx_msg;
		link->rx_msg = NULL;

                RLNH_PRINT_HEADER(co->name, "RX", &link->rx_cmhdr, (uint8_t *)msg);
		co->links[prio].uc->uc_delivery(co->links[prio].uc_data,
				&link->rx_cmhdr, msg);
	}
	return 0;
}

static void mhp_connect(struct ulh_mhp_conn *co)
{
	switch (co->state) {
	case STATE_INIT:
		return;
	case STATE_IDLE:
		if (ulh_trans_attach(co->cid, co->mbox, co->uref)) {
			ULH_TRACE_ERROR("%s: failed to attach\n", __func__);
			mhp_timer_arm(co, &co->conn_timer, 1000);
			return;
		}
		__mhp_reset(co);
		co->state = STATE_DOWN;
		/* fall */
	case STATE_DOWN:
		if (!co->active)
			return;
		mhp_tx_conn(co);
		mhp_timer_arm(co, &co->conn_timer, co->conn_tmo);
		break;
	case STATE_HELLO:
		mhp_tx_disc(co);
		co->state = STATE_DOWN;
		__mhp_reset(co);
		mhp_timer_arm(co, &co->conn_timer, co->conn_tmo);
		break;
	default:
		break;
	}
}

static void mhp_rx_conn(struct ulh_mhp_conn *co, struct ulh_tbuff *tb)
{
	struct mhp_conn *hdr;
	uint16_t lconnid, rconnid;

	if (ulh_tbuff_len(tb) < sizeof(struct mhp_conn)) {
		ULH_TRACE_ERROR("%s: CONN packet is too short, %u\n", co->name,
				ulh_tbuff_len(tb));
		return;
	}

	hdr = (struct mhp_conn *) ulh_tbuff_get(tb);
	lconnid = ntohs(hdr->lconnid);
	rconnid = ntohs(hdr->rconnid);

	if (!lconnid) {
		ULH_TRACE_ERROR("%s: CONN with incorrect LCONNID:%u\n", co->name,
				lconnid);
		return;
	}

	switch (co->state) {
	case STATE_INIT:
	case STATE_IDLE:
		ULH_TRACE_ERROR("%s: CONN in unexpected state %u\n", co->name,
				co->state);
		break;
	case STATE_DOWN:
	case STATE_HELLO:
		if (!rconnid) {
			co->rconnid = lconnid;
			co->state = STATE_HELLO;
			co->rx_seq = ntohs(hdr->cmn.ns);
			mhp_tx_conn(co);
			mhp_timer_arm(co, &co->conn_timer, 1000);
			break;
		}
		if (rconnid != co->lconnid)
			break;

		co->rconnid = lconnid;
		co->rx_seq = ntohs(hdr->cmn.ns);
		mhp_timer_arm(co, &co->ack_timer, co->ack_tmo);
		mhp_timer_arm(co, &co->kpa_timer, co->kpa_tmo);
		mhp_timer_arm(co, &co->sup_timer, co->sup_tmo);

		co->state = STATE_UP;
		__mhp_uc_connected(co);
		break;
	case STATE_UP:
		break;
	}
}

static void mhp_rx_disc(struct ulh_mhp_conn *co, struct ulh_tbuff *tb)
{
	struct mhp_disc *hdr;
	uint16_t lconnid, rconnid;

	if (ulh_tbuff_len(tb) < sizeof(struct mhp_disc)) {
		ULH_TRACE_ERROR("%s: DISC packet is too short, %u\n", co->name,
				ulh_tbuff_len(tb));
		return;
	}

	hdr = (struct mhp_disc *) ulh_tbuff_get(tb);
	lconnid = ntohs(hdr->lconnid);
	rconnid = ntohs(hdr->rconnid);
	if (!rconnid) {
		ULH_TRACE_ERROR("%s: DISC with incorrect RCONNID:%u\n", co->name,
				rconnid);
		return;
	}

	switch (co->state) {
	case STATE_INIT:
	case STATE_IDLE:
		ULH_TRACE_ERROR("%s: DISC in unexpected state %u\n", co->name,
				co->state);
		break;
	case STATE_DOWN:
	case STATE_HELLO:
		if (lconnid != co->rconnid || rconnid != co->lconnid)
			break;
		co->state = STATE_DOWN;
		__mhp_reset(co);

		if (co->active)
			mhp_timer_arm(co, &co->conn_timer, 1000);
		break;
	case STATE_UP:
		if (lconnid != co->rconnid || rconnid != co->lconnid)
			break;

		mhp_disconnect(co, 1);
		break;
	}
}

static void mhp_rx_ack(struct ulh_mhp_conn *co, struct ulh_tbuff *tb)
{
	struct mhp_header *hdr;
	uint16_t nr;

	hdr = (struct mhp_header *) ulh_tbuff_get(tb);
	nr = ntohs(hdr->nr);

	switch (co->state) {
	case STATE_INIT:
	case STATE_IDLE:
		ULH_TRACE_ERROR("%s: ACK in unexpected state %u\n", co->name,
				co->state);
		break;
	case STATE_DOWN:
		break;
	case STATE_HELLO:
		if (nr != co->tx_seq) 
			break;
		co->state = STATE_UP;
		__mhp_uc_connected(co);
		/* fall */
	case STATE_UP:
		mhp_process_ack(co, nr);
		/* XXX NACK if ns is not what we think??? */
		break;
	}
}

static void mhp_rx_nack(struct ulh_mhp_conn *co, struct ulh_tbuff *tb)
{
	struct mhp_header *hdr;
	uint16_t nr;

	hdr = (struct mhp_header *) ulh_tbuff_get(tb);
	nr = ntohs(hdr->nr);

	switch (co->state) {
	case STATE_INIT:
	case STATE_IDLE:
		ULH_TRACE_ERROR("%s: NACK in unexpected state %u\n", co->name,
				co->state);
		break;
	case STATE_DOWN:
		break;
	case STATE_HELLO:
		if (nr != co->tx_seq) 
			break;
		co->state = STATE_UP;
		__mhp_uc_connected(co);
		/* fall */
	case STATE_UP:
		mhp_process_ack(co, nr);
                /* Trigger retransmission of remaining packets in unack queue. */
                mhp_retransmit(co);
		break;
	}
}

static void mhp_rx_data(struct ulh_mhp_conn *co, struct ulh_tbuff *tb)
{
	struct mhp_data *hdr;
	uint16_t nr, ns;
	
	if (ulh_tbuff_len(tb) < sizeof(struct mhp_data)) {
		ULH_TRACE_ERROR("%s: DATA packet is too short, %u\n", co->name,
				ulh_tbuff_len(tb));
		mhp_disconnect(co, 1);
		return;
	}

	hdr = (struct mhp_data *) ulh_tbuff_get(tb);
	nr = ntohs(hdr->cmn.nr);
	ns = ntohs(hdr->cmn.ns);

	switch (co->state) {
	case STATE_INIT:
	case STATE_IDLE:
	case STATE_DOWN:
		ULH_TRACE_ERROR("%s: DATA in unexpected state %u\n", co->name,
				co->state);
		break;
	case STATE_HELLO:
		if (nr != co->tx_seq) 
			break;
		co->state = STATE_UP;
		__mhp_uc_connected(co);
		/* fall */
	case STATE_UP:
		if (!mhp_process_data(co, ns, tb))
			mhp_process_ack(co, nr);
		break;
	}
}


static void mhp_conn_tmo(void *handle)
{
	struct ulh_mhp_conn *co = handle;
	mhp_connect(co);
}

static void mhp_ack_tmo(void *handle)
{
	struct ulh_mhp_conn *co = handle;
	mhp_tx_ack(co);
	mhp_timer_arm(co, &co->kpa_timer, co->kpa_tmo);
}

static void mhp_rt_tmo(void *handle)
{
	struct ulh_mhp_conn *co = handle;
	
	mhp_retransmit(co);
}

static void mhp_kpa_tmo(void *handle)
{
	struct ulh_mhp_conn *co = handle;
	
	mhp_tx_ack(co);
	mhp_timer_arm(co, &co->kpa_timer, co->kpa_tmo);
}

static void mhp_sup_tmo(void *handle)
{
	struct ulh_mhp_conn *co = handle;
	
	mhp_disconnect(co, 1);
}

static int __mhp_all_init(struct ulh_mhp_conn *co)
{
	int i;

	for (i = 0; i < co->nlinks; i++) {
		if (!co->links[i].uc)
			return 0;
	}

	return 1;
}

static int __mhp_all_connect(struct ulh_mhp_conn *co)
{
	int i;

	for (i = 0; i < co->nlinks; i++) {
		if (!co->links[i].uc || !co->links[i].connected)
			return 0;
	}

	return 1;
}

static int mhp_dc_init(void *handle, struct ulh_cm_uc_ops *uc,
		void *uc_data, uint32_t prio)
{
	struct ulh_mhp_conn *co = handle;

	if (!uc || !co)
		return -EINVAL;
	if (prio >= co->nlinks)
		return -EINVAL;

	if (co->state != STATE_INIT)
		return -EINVAL;

	if (co->links[prio].uc)
		return -EBUSY;

	co->links[prio].uc = uc;
	co->links[prio].uc_data = uc_data;
	co->links[prio].connected = 0;

	if (__mhp_all_init(co))
		co->state = STATE_IDLE;

	return 0;
}

static int mhp_dc_fini(void *handle, uint32_t prio)
{
	struct ulh_mhp_conn *co = handle;

	if (!co)
		return -EINVAL;
	if (prio >= co->nlinks)
		return -EINVAL;
	if (!co->links[prio].uc)
		return 0;

	if (co->state > STATE_IDLE)
		mhp_disconnect(co, 0);

	co->links[prio].uc = NULL;
	co->links[prio].connected = 0;
	co->state = STATE_INIT;

	return 0;
}

static int mhp_dc_connect(void *handle, uint32_t prio)
{
	struct ulh_mhp_conn *co = handle;

	if (!co)
		return -EINVAL;
	if (prio >= co->nlinks)
		return -EINVAL;
	if (!co->links[prio].uc)
		return -EINVAL;
	if (co->links[prio].connected)
		return 0;

	co->links[prio].connected = 1;
	if (__mhp_all_connect(co))
		mhp_connect(co);

	return 0;
}

static int mhp_dc_disconnect(void *handle, uint32_t prio)
{
	struct ulh_mhp_conn *co = handle;

	if (!co)
		return -EINVAL;
	if (prio >= co->nlinks)
		return -EINVAL;
	if (!co->links[prio].uc)
		return -EINVAL;
	if (!co->links[prio].connected)
		return 0;

	co->links[prio].connected = 0;
	
	if (co->state > STATE_IDLE)
		mhp_disconnect(co, 0);

	return 0;
}

static int mhp_dc_transmit(void *handle, uint32_t prio,
		struct ulh_cm_msghdr *hdr, union itc_msg *msg)
{
	struct ulh_tbuff *tb;
	struct ulh_mhp_conn *co = handle;
	uint32_t rsize = hdr->size;
	uint32_t frag_size;
	uint32_t hdr_size = sizeof(struct mhp_data) +
		sizeof(struct mhp_data_hdr);
	uint32_t frag_max_size = co->mtu - hdr_size;
	uint32_t frags, cnt;
	uint8_t *pos = (uint8_t *) msg;
	struct mhp_data *dhdr;
	struct mhp_data_hdr *ddhdr;

	RLNH_PRINT_HEADER(co->name, "TX", hdr, (uint8_t *)msg);

	if (prio >= co->nlinks)
		return -EINVAL;

	if (co->state != STATE_UP) {
		ULH_TRACE_INFO("MHP: %s: dc_transmit in wrong state\n", co->name);
		return -EINVAL;
	}

	frags = (rsize - 1) / frag_max_size + 1;
	if (frags > MHP_DATA_MAXFRAGS) {
		ULH_TRACE_ERROR("MHP: %s: packet is too big\n", co->name);
		mhp_disconnect(co, 1);
		return -EINVAL;
	}

	cnt = 0;
	while (rsize) {
		tb = ulh_tbuff_pool_get(&co->tbpool);
		if (!tb) {
			ULH_TRACE_ERROR("MHP: %s: unable to allocate tb\n", co->name);
			mhp_disconnect(co, 1);
			return -ENOMEM;
		}

		frag_size = rsize;

		if (frags > 1) {
			if ((cnt == 0) && (rsize % frag_max_size))
				frag_size = (rsize % frag_max_size) + (rsize & 1);
			else if (frag_size > frag_max_size)
				frag_size = frag_max_size;		
		}

		if (ulh_tbuff_alloc(co->cid, frag_size + hdr_size, tb)) {
			ULH_TRACE_ERROR("%s: unable to allocate packet\n", co->name);
			mhp_disconnect(co, 1);
			return -ENOMEM;
		}

		dhdr = (struct mhp_data *) ulh_tbuff_get(tb);
		ddhdr = (struct mhp_data_hdr *) (dhdr + 1);

		/* fill common and data headers */
		memset(ddhdr, 0, sizeof(*ddhdr));

		dhdr->cmn.feature = 0;
		dhdr->cmn.type = MHP_DATA;
		dhdr->cmn.prio = prio;
		dhdr->size = htons(frag_size);

		/* fill FragNo field */
		if (frags > 1)
			dhdr->frag = frags - cnt;
		else 
			dhdr->frag = 0;
		/* fill src, dst and frag. flag in the first 
		 * fragment */

		if (!cnt) {
			if (frags > 1)
				dhdr->frag |= MHP_DATA_F_FRAG;
			ddhdr->src   = htonl(hdr->src);
			ddhdr->dst   = htonl(hdr->dst);
			ddhdr->extra = htonl(hdr->size);
		}
		dhdr->frag = htons(dhdr->frag);

		/* copy user data */
		memcpy(ddhdr + 1, pos, frag_size);
		pos += frag_size;

		ulh_tbuff_queue(&co->links[prio].tx_queue, tb);
		rsize -= frag_size;
		cnt++;
	}

	mhp_tx(co);
	return 0;
}

static void mhp_dc_receive(void *handle, uint32_t cid, struct ulh_tbuff *tb)
{
	struct ulh_mhp_conn *co = handle;
	struct mhp_header *mhp;

	if (co->state < STATE_DOWN) {
		ulh_tbuff_free(tb);
		return;
	}

	if (ulh_tbuff_len(tb) < sizeof(struct mhp_header)) {
		ULH_TRACE_ERROR("MHP: %s: packet is too short %u\n", co->name,
				ulh_tbuff_len(tb));
		ulh_tbuff_free(tb);
		return;
	}

	MHP_PRINT_HEADER(co->name, "MHP RX", tb);

	mhp = (struct mhp_header *) ulh_tbuff_get(tb);
	/* XXX other fields verification??? */
	switch (mhp->type) {
	case MHP_CONN:
		mhp_rx_conn(co, tb);
		break;
	case MHP_DISC:
		mhp_rx_disc(co, tb);
		break;
	case MHP_ACK:
		mhp_rx_ack(co, tb);
		break;
	case MHP_NACK:
		mhp_rx_nack(co, tb);
		break;
	case MHP_DATA:
		mhp_rx_data(co, tb);
		break;
	default:
		ULH_TRACE_INFO("MHP: %s: unsupport packet type 0x%x\n",
				co->name, mhp->type);
                /* XXX Link down??? */
		break;
	}

	ulh_tbuff_free(tb);
}

static struct ulh_cm_dc_ops mhp_cm_dc = {
	.dc_init = mhp_dc_init,
	.dc_fini = mhp_dc_fini,
	.dc_connect = mhp_dc_connect,
	.dc_disconnect = mhp_dc_disconnect,
	.dc_transmit = mhp_dc_transmit,
	.dc_receive = mhp_dc_receive,
};

static int mhp_create_instance(void *unused, const char *name,
	struct ulh_cm_instance *instance,
	struct ulh_cm_config *config, struct ulh_timerqueue *tqueue)
{
	struct ulh_mhp_conn *co;
	int i;
	struct ulh_cm_mhp_config *mhp_cfg =
		(struct ulh_cm_mhp_config *) config;

	if (mhp_cfg->cmn.cfg_size != sizeof(struct ulh_cm_mhp_config))
		return -EINVAL;
	if (!mhp_cfg->prios || mhp_cfg->prios > 1)
		return -EINVAL;

	co = malloc(sizeof(*co));
	if (!co)
		goto fail;
	memset(co, 0, sizeof(*co));
	co->name = strdup(name);
	if (!co->name)
		goto fail;

        co->tqueue   = tqueue;
	co->cid      = mhp_cfg->cmn.cid;
	co->mbox     = mhp_cfg->cmn.mbox;
	co->uref     = mhp_cfg->cmn.uref;
	co->active   = mhp_cfg->active;
	co->rt_tmo   = mhp_cfg->rt_tmo;
	co->ack_tmo  = mhp_cfg->ack_tmo;
	co->kpa_tmo  = mhp_cfg->kpa_tmo;
	co->sup_tmo  = mhp_cfg->sup_tmo;
	co->tx_wnd   = mhp_cfg->tx_wnd;
	co->rt_limit = mhp_cfg->rt_limit;
	co->conn_tmo = mhp_cfg->conn_tmo;
	co->mtu      = ulh_trans_getmtu(co->cid);
	if (co->mtu < 0)
		goto fail;
	co->state = STATE_INIT;
	co->nlinks = mhp_cfg->prios;
	for (i = 0; i < co->nlinks; i++) {
		ulh_tbuff_queue_init(&co->links[i].tx_queue);
		co->links[i].rx_msg = NULL;
	}

	ulh_timer_init(&co->conn_timer, mhp_conn_tmo, co);
	ulh_timer_init(&co->ack_timer, mhp_ack_tmo, co);
	ulh_timer_init(&co->rt_timer, mhp_rt_tmo, co);
	ulh_timer_init(&co->kpa_timer, mhp_kpa_tmo, co);
	ulh_timer_init(&co->sup_timer, mhp_sup_tmo, co);

	ulh_tbuff_queue_init(&co->tx_unack_queue);
	ulh_tbuff_pool_init(&co->tbpool, 1024);

	ULH_TRACE_INFO("MHP connection %s (active:%d cid:%u mtu:%u) "
                       "created\n", co->name, co->active,
                       co->cid, co->mtu);

	instance->instance = co;
	instance->ops = &mhp_cm_dc;

	return 0;

fail:
	if (co) {
		if (co->name)
			free(co->name);
		free(co);
	}
	return -EFAULT;
}

static int mhp_destroy_instance(void *unused,
		struct ulh_cm_instance *instance)
{
	struct ulh_mhp_conn *co;

	if (!instance || !instance->instance)
		return -EINVAL;

	co = (struct ulh_mhp_conn *) instance->instance;
	if (co->name)
		free(co->name);
	free(co);

	instance->instance = NULL;
	return 0;
}


static struct ulh_cm_ops mhp_ops = {
	.create_instance = mhp_create_instance,
	.destroy_instance = mhp_destroy_instance,
};

int ulh_mhp_init(const char *name)
{
	return ulh_cm_register(name, &mhp_ops, NULL);
}

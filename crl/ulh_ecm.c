/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file ulh_ecm.c
 *
 *   [Enter a longer description about the file. More than one line
 *   can be used, but leave an empty line before the copyright line.]
 *
 *   [All text within [...] should be changed or removed.]
 *
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : [2010-02-01 Lars Magnus Ericsson]
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

#include <pthread.h>
#include <errno.h>

#include <itc.h>

#include "ecm_proto.h"

#include "ulh_cm.h"
#include "ulh_transport.h"
#include "ulh_ecm.h"
#include "ulh_timer.h"

#include "ulh_trace.h"
#ifdef LTTNG
#include "ulh_lttng.h"
#include "../libitclnh/ulh_rlnh.h"
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#ifdef LTTNG
#define ECM_PRINT_HEADER(name, prfx, tb) ecm_print_header(name, prfx, tb)
#define RLNH_PRINT_HEADER(name, prfx, chdr, buf) rlnh_print_header(name, prfx, chdr, buf)
#else
#define ECM_PRINT_HEADER(name, prfx, tb)
#define RLNH_PRINT_HEADER(name, prfx, chdr, buf)
#endif

#define ecm_timer_arm(_co, _timer, _tmo)				\
	do {								\
		if(_co->state < STATE_DISCONNECTED)			\
			abort();					\
		ulh_timer_arm((_timer), (_co)->tqueue, (_tmo));		\
	} while(0)

#define ecm_timer_cancel(_co, _timer)				\
	do {							\
		if(_co->state < STATE_DISCONNECTED)		\
			abort();				\
		ulh_timer_cancel((_timer));			\
	} while(0)

#define ecm_timer_init(_timer, _fcn, _co)			\
	do {							\
		ulh_timer_init((_timer), (_fcn), (_co));	\
	} while(0)

#define ULH_ECM_MAXLINKS 1
#define ULH_ECM_MAXFRAGS 100

#define STATE_INIT	        0
#define STATE_IDLE	        1
#define STATE_DISCONNECTED	2
#define STATE_CONNECTING_1	3
#define STATE_CONNECTING_2	4
#define STATE_CONNECTED         5

#define IN_WINDOW(n, l, w) ((l <= n) ? ((n-l) < w) : ((ECM_SEQ_MASK+1-l+n) < w))

#define MAXINFO         2048
#define RXQ_LENGTH      128
#define RXQ_MASK        (RXQ_LENGTH - 1)
#define RT_TMO_SEQ_CNT  5

#define MAX_THROUGHPUT_HISTORY   10
#define MAX_THROUGHPUT_INFO_PER_LINK  1024

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
struct ulh_ecm_rx_reassembly {
	int tot_size;
	int num_frags;
	int frag_no;
	struct ulh_tbuff *tb[ULH_ECM_MAXFRAGS];
};

struct ulh_ecm_link {
	struct ulh_cm_uc_ops *uc;
	void *uc_data;
	int connected;

	struct ulh_tbuff_queue tx_queue;

	/* reassembly */
	struct ulh_ecm_rx_reassembly reassm;

	struct ulh_cm_msghdr rx_cmhdr;
};

struct ecm_throughput {
       uint64_t tx_bytes;
       uint64_t rx_bytes;
       unsigned int tx_packet;
       unsigned int rx_packet;
};

struct ecm_stats {
	unsigned int tx;
	unsigned int txdata;
	unsigned int txfrag;
	unsigned int txack;
	unsigned int txnack;
	unsigned int txresend_tmo;
	unsigned int txresend_nack;
	unsigned int rx;
	unsigned int rxdata;
	unsigned int rxfrag;
	unsigned int rxack;
	unsigned int rxnack;
	uint64_t     txbytes;
	uint64_t     rxbytes;
	struct ecm_throughput thrp_hist[MAX_THROUGHPUT_HISTORY];
	struct ecm_throughput temp_thrp;
	unsigned int curr_thrp_idx;
};

struct ulh_ecm_conn {
	int state;

	uint32_t cid;
	int mtu;
	int active;

	struct ulh_tbuff *rx_queue[RXQ_LENGTH];

	/* */
	uint16_t rx_seq;
	uint16_t rx_outstanding;
	uint16_t tx_seq;
	uint16_t tx_outstanding;
	uint16_t tx_ack;
	uint16_t tx_wnd;

	uint8_t  rconnid;
	struct ulh_tbuff_queue tx_unack_queue;
	uint32_t rt_cnt;

	/* links */
	uint16_t nlinks;
	struct ulh_ecm_link links[ULH_ECM_MAXLINKS];

	struct ulh_timerqueue *tqueue;
	struct ulh_tbuff_pool tbpool;
	struct ulh_tbuff_pool rxpool;

	struct ulh_timer conn_timer;
	struct ulh_timer ack_timer;
	struct ulh_timer rt_timer;
	struct ulh_timer sup_timer;

	uint32_t rt_tmo;
	uint32_t rt_limit;
	uint32_t ack_tmo;
	uint32_t kpa_tmo;
	uint32_t sup_tmo;
	uint32_t conn_tmo;

	struct ecm_stats stats;

	uint8_t dstmac[ETH_ALEN];
	uint8_t srcmac[ETH_ALEN];

	char *name;
	itc_mbox_id_t mbox;
	unsigned long uref;
        bool shutdown;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static char *state_to_string[] = { "ECM_INIT        ",
				   "ECM_IDLE        ",
				   "ECM_DISCONNECTED",
				   "ECM_CONNECTING_1",
				   "ECM_CONNECTING_2",
				   "ECM_CONNECTED   " };

/* Retransmit tmo in fibonacci series */
static uint8_t rt_tmo_fibonacci[RT_TMO_SEQ_CNT] = {1, 2, 3, 5, 8};

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static void ecm_disconnect(struct ulh_ecm_conn *co, int reconnect,
			   const char *function, const int line);

/* ===================================================================== */
/**
 *   [Enter a brief one-line function comment and leave next line empty.]
 *
 *   @param [param1    Description of param1. Use a hyphen (-) as param1
 *                     (and no description) if no parameters are used]
 *   [@param param2    Description of param2]
 *
 *   @return           [Comment the return of the function. Use a hyphen
 *                     (-) if no return value is used]
 *
 *   @par Globals:
 *                     [List of all globals variables that are used by
 *                     this function. Use two hyphens (--) if no global
 *                     variables are used]
 *
 *   [A longer multi-line and multi-sentence description of the function
 *   can be entered here. This way of tagging the comments is based
 *   on the function doxygen - A documentation system for generating
 *   descriptions from source code.]
 *
 *   [The short-line description is required, but the longer description
 *   is optional for simple functions. The intended audience is advanced,
 *   and no novice programmers.]
 *
 *   [Inside functions, use comments to structure the code and explain
 *   the meaning of instructions or groups of instructions. Do not
 *   explain what the code does, but what your intentions are and what
 *   the code (or code section) does to achieve that goal. Do not
 *   comment things that are obvious to anyone reading the source
 *   code.]
 *
 *   [Nevertheless: "When in doubt, comment it!"].
 */
/* ===================================================================== */
#ifdef LTTNG
static void ecm_print_header(const char *name, const char *prfx,
                             struct ulh_tbuff *tb)
{
	uint32_t            *hdr;
	uint32_t             next;

	hdr = (uint32_t *) ulh_tbuff_get(tb);

        next = ECM_MAIN_GET_NEXT(hdr);
	ULH_TRACE(ecm_main, (char *)name, (char *) prfx,
		  next,
		  ECM_MAIN_GET_VER(hdr),
		  ECM_MAIN_GET_CONNID(hdr),
		  ECM_MAIN_GET_PKTSIZE(hdr));

	ECM_MAIN_ADVANCE(hdr);
	while(next != ECM_LAST) {
		switch(next) {
		case ECM_CONN:
			next = ECM_CONN_GET_NEXT(hdr);
			ULH_TRACE(ecm_conn, next,
				  ECM_CONN_GET_TYPE(hdr),
				  ECM_CONN_GET_SIZE(hdr),
				  ECM_CONN_GET_WIN(hdr),
				  ECM_CONN_GET_DST(hdr),
				  ECM_CONN_GET_SRC(hdr),
				  ECM_CONN_GET_FEATURES(hdr));
			if(next != ECM_LAST) {
				ULH_TRACE_ERROR("Malformed ECM packet", 0);
				next = ECM_LAST;
			}
			break;

		case ECM_UDATA:
			next = ECM_UDATA_GET_NEXT(hdr);
			ULH_TRACE(ecm_udata, next,
				  ECM_UDATA_GET_O(hdr),
				  ECM_UDATA_GET_M(hdr),
				  ECM_UDATA_GET_FRAGNO(hdr),
				  ntohl(ECM_UDATA_GET_DST(hdr)),
				  ntohl(ECM_UDATA_GET_SRC(hdr)));
			ECM_UDATA_ADVANCE(hdr);
			break;

		case ECM_FRAG:
			next = ECM_FRAG_GET_NEXT(hdr);
			ULH_TRACE(ecm_frag, next,
				  ECM_FRAG_GET_M(hdr),
				  ECM_FRAG_GET_FRAGNO(hdr));
			ECM_FRAG_ADVANCE(hdr);
			break;

		case ECM_ACK:
			next = ECM_ACK_GET_NEXT(hdr);
			ULH_TRACE(ecm_ack, next,
				  ECM_ACK_GET_R(hdr),
				  ECM_ACK_GET_ACKNO(hdr),
				  ECM_ACK_GET_SEQNO(hdr));
			ECM_ACK_ADVANCE(hdr);
			break;

		case ECM_NACK:
			next = ECM_NACK_GET_NEXT(hdr);
			ULH_TRACE(ecm_nack, next,
				  ECM_NACK_GET_COUNT(hdr),
				  ECM_NACK_GET_SEQNO(hdr));
			ECM_NACK_ADVANCE(hdr);
			break;

		default:
			next = ECM_LAST;
			ULH_TRACE_ERROR("Malformed ECM packet", 0);
			break;
		}
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

static void free_buffer_to_pool(struct ulh_tbuff_pool *pool,
                                struct ulh_tbuff *tb)
{
	ulh_tbuff_free(tb);
	ulh_tbuff_pool_put(pool, tb);
}

static void __ecm_uc_connected(struct ulh_ecm_conn *co)
{
	int i;

	co->tx_seq = (uint16_t)-1;
	co->rx_seq = 0;
	co->tx_ack = 0;
	co->rx_outstanding = 0;
	co->tx_outstanding = 0;
        co->shutdown = 0;
	co->links[0].reassm.tot_size = 0;
	co->links[0].reassm.num_frags = 0;
	co->links[0].reassm.frag_no = 0;

	ULH_TRACE_INFO("ECM connection up, cid:%d co:%p name:%s",
		       co->cid, co, co->name);

	for (i = 0; i < co->nlinks; i++)
		co->links[i].uc->uc_connected(co->links[i].uc_data);
}

static void __ecm_uc_disconnected(struct ulh_ecm_conn *co)
{
	int i;

	ULH_TRACE_INFO("ECM connection down, cid:%d co:%p name:%s",
		       co->cid, co, co->name);

	for (i = 0; i < co->nlinks; i++)
		co->links[i].uc->uc_disconnected(co->links[i].uc_data);
}

static void ecm_tx_connect(struct ulh_ecm_conn *co,
			   uint8_t type)
{
	struct ulh_tbuff *tb;
        char features[] = "";
	uint32_t *hdr;
	int size;

	tb = ulh_tbuff_pool_get(&co->tbpool);
	if (!tb) {
		ULH_TRACE_ERROR("Alloc tbuff failed");
		return;
	}

	size = ECM_MAIN_SIZE + ECM_CONN_SIZE(features);

	if (ulh_tbuff_alloc(co->cid, size, tb)) {
		ULH_TRACE_INFO("Failed to allocate buffer");
		ulh_tbuff_pool_put(&co->tbpool, tb);
		return;
	}

	hdr = (uint32_t *) ulh_tbuff_get(tb);
	ECM_MAIN_SET(hdr, ECM_CONN, ECM_VER_3,
		     0, size);
	ECM_MAIN_ADVANCE(hdr);
	ECM_CONN_SET(hdr, ECM_LAST,
		     type, ETH_ALEN,
		     (ffs(co->tx_wnd) - 1), co->cid);
	ECM_CONN_SET_DST(hdr, co->dstmac);
	ECM_CONN_SET_SRC(hdr, co->srcmac);
	ECM_CONN_SET_FEATURES(hdr, features);

	ECM_PRINT_HEADER(co->name, "ECM TX", tb);

	ulh_trans_transmit(co->cid, tb);
	ulh_tbuff_pool_put(&co->tbpool, tb);
}

static void ecm_tx_ack(struct ulh_ecm_conn *co,
		       uint8_t req_ack)
{
	struct ulh_tbuff *tb;
	uint32_t *hdr;
	int size;

	tb = ulh_tbuff_pool_get(&co->tbpool);
	if (!tb) {
		ULH_TRACE_ERROR("Alloc tbuff failed");
		return;
	}

	co->stats.txack++;
        co->stats.tx++;

	size = ECM_MAIN_SIZE + ECM_ACK_SIZE;

	if (ulh_tbuff_alloc(co->cid, size, tb)) {
		ULH_TRACE_INFO("Failed to allocate buffer");
		ulh_tbuff_pool_put(&co->tbpool, tb);
		return;
	}

	hdr = (uint32_t *) ulh_tbuff_get(tb);
	ECM_MAIN_SET(hdr, ECM_ACK, ECM_VER_3,
		     co->rconnid, size);
	ECM_MAIN_ADVANCE(hdr);
	ECM_ACK_SET(hdr, ECM_LAST, req_ack,
		    co->rx_seq, 0);

	ECM_PRINT_HEADER(co->name, "ECM TX", tb);

	ulh_trans_transmit(co->cid, tb);
	ulh_tbuff_pool_put(&co->tbpool, tb);
	co->rx_outstanding = 0;
	ecm_timer_arm(co, &co->ack_timer, co->kpa_tmo);
}

static void ecm_tx_nack(struct ulh_ecm_conn *co, uint16_t nack_ns,
			uint16_t count)
{
	struct ulh_tbuff *tb;
	uint32_t *hdr;
	int size;

        ULH_TRACE_INFO("ECM: %s: out of order data packet"
                        " dropped, nack sent : %d, count : %d \n", co->name,
                         nack_ns, count);


	tb = ulh_tbuff_pool_get(&co->tbpool);
	if (!tb) {
		ULH_TRACE_ERROR("Alloc tbuff failed");
		return;
	}

	size = ECM_MAIN_SIZE + ECM_NACK_SIZE;

	if (ulh_tbuff_alloc(co->cid, size, tb)) {
		ULH_TRACE_INFO("Failed to allocate buffer");
		ulh_tbuff_pool_put(&co->tbpool, tb);
		return;
	}

	co->stats.txnack++;
        co->stats.tx++;

	hdr = (uint32_t *) ulh_tbuff_get(tb);
	ECM_MAIN_SET(hdr, ECM_NACK, ECM_VER_3,
		     co->rconnid, size);
	ECM_MAIN_ADVANCE(hdr);
	ECM_NACK_SET(hdr, ECM_LAST, count, nack_ns);

	ECM_PRINT_HEADER(co->name, "ECM TX", tb);

	ulh_trans_transmit(co->cid, tb);
	ulh_tbuff_pool_put(&co->tbpool, tb);
}

static void ecm_tx(struct ulh_ecm_conn *co)
{
	struct ulh_tbuff *tb;
	uint32_t *hdr, next, msgno;
	int tx_out = co->tx_outstanding;

	if (co->tx_outstanding >= co->tx_wnd)
		return;

	do {
		/* We only support one priority and  allocate for one TX. */
		tb = ulh_tbuff_dequeue(&co->links[0].tx_queue);
		if(!tb) {
			/* Nothing more to send, break loop. */
			break;
		}

		/* step TX sequence */
		co->tx_seq++;
		co->tx_seq &= ECM_SEQ_MASK;

		/* update header */
		hdr = (uint32_t *) ulh_tbuff_get(tb);
		ECM_MAIN_ADVANCE(hdr);
		next = ECM_ACK_GET_NEXT(hdr);
                if(next == ECM_UDATA) {
                        /* Getting msg no from hdr ack *(uint32_t *)(hdr+4) */
                        msgno = ECM_GET_MSGNO_FROM_ACK(hdr);
                        ULH_TRACE(ulh_trace_sig, __FILE__, __LINE__, msgno, __func__);
                 }
		ECM_ACK_SET(hdr, next, 0, co->rx_seq, co->tx_seq);

		/* add to unack queue */
		ulh_tbuff_queue(&co->tx_unack_queue, tb);

		/* increment ref to avoid tb to be freed by transport */
		ulh_tbuff_hold(tb);

		ECM_PRINT_HEADER(co->name, "ECM TX", tb);

		co->tx_outstanding++;
		co->stats.tx++;
                co->stats.txbytes += tb->size;

		/* send */
		ulh_trans_transmit(co->cid, tb);

	} while (co->tx_outstanding < co->tx_wnd);

	if (co->tx_outstanding != tx_out) {
		co->rx_outstanding = 0;
		ecm_timer_arm(co, &co->ack_timer, co->kpa_tmo);

		if (tx_out == 0)
			ecm_timer_arm(co, &co->rt_timer, co->rt_tmo);
	}
}

static int ecm_retransmit(struct ulh_ecm_conn *co, uint16_t seq, int count)
{
	struct ulh_tbuff *tb;
	uint32_t *hdr, next, tx_seq, tx_done = 0;

	if (co->tx_outstanding == 0) {
		co->rt_cnt = 0;
		return 0;
	}


	dl_list_foreach(tb, &co->tx_unack_queue.queue, link) {
		hdr = (uint32_t *) ulh_tbuff_get(tb);
		ECM_MAIN_ADVANCE(hdr);
		next = ECM_ACK_GET_NEXT(hdr);
		tx_seq = ECM_ACK_GET_SEQNO(hdr);

		if (tx_seq == seq) {

			ECM_ACK_SET(hdr, next, 0, co->rx_seq, tx_seq);

			/* increment ref to avoid fb to be freed by transport */
			ulh_tbuff_hold(tb);
			ECM_PRINT_HEADER(co->name, "ECM TX", tb);
			ulh_trans_transmit(co->cid, tb);
			seq = (seq + 1) & ECM_SEQ_MASK;
			tx_done++;

			if (--count == 0)
				goto done;
		}
	}

 done:

	if (tx_done) {
		co->rx_outstanding = 0;
		ecm_timer_arm(co, &co->ack_timer, co->kpa_tmo);
                return 1;
        }
        return 0;
}

static int process_conn_connect(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
        char *features;

	hdr = (uint32_t *)ulh_tbuff_get(tb);

	switch(co->state) {
	case STATE_INIT: /* no break on purpose! */
	case STATE_IDLE:
		/* We are not ready to deal with messages yet, ignore it! */
		break;
	case STATE_DISCONNECTED:
		ecm_timer_cancel(co, &co->conn_timer);
		ecm_tx_connect(co, ECM_CONNTYPE_CONNECT_ACK);
		co->state = STATE_CONNECTING_2;
		break;
	case STATE_CONNECTING_1:
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
		ecm_timer_arm(co, &co->conn_timer, co->conn_tmo/5);
		co->state = STATE_DISCONNECTED;
		break;
	case STATE_CONNECTING_2:
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
		ecm_timer_arm(co, &co->conn_timer, co->conn_tmo/5);
		co->state = STATE_DISCONNECTED;
		break;
	default: /* STATE_CONNECTED */
		/* We send reset and return to idle, how should this be handled! */
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		break;
	}
        features = ECM_CONN_GET_FEATURES(hdr);
	ulh_tbuff_pop(tb, ECM_CONN_SIZE(features));
	return ECM_CONN_GET_NEXT(hdr);
}

static int process_conn_connect_ack(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
        char *features;

	hdr = (uint32_t *)ulh_tbuff_get(tb);

	switch(co->state) {
	case STATE_INIT: /* no break on purpose! */
	case STATE_IDLE:
		/* We are not ready to deal with messages yet, ignore it! */
		break;
	case STATE_DISCONNECTED:
		/* Should not get this message in this state, lets respond with a
		   reset and keep our state. */
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
		break;
	case STATE_CONNECTING_1:
		ecm_tx_connect(co, ECM_CONNTYPE_ACK);
		co->rconnid = ECM_CONN_GET_CONNID(hdr);
		co->state = STATE_CONNECTED;
		__ecm_uc_connected(co);
		ecm_timer_arm(co, &co->ack_timer, co->kpa_tmo);
		ecm_timer_arm(co, &co->sup_timer, co->sup_tmo);
		ecm_timer_arm(co, &co->rt_timer, co->rt_tmo);
		ecm_timer_cancel(co, &co->conn_timer);
		break;
	case STATE_CONNECTING_2:
		/* We should not get a connect in this state, lets send reset and
		   go to state disconnected. */
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
		co->state = STATE_DISCONNECTED;
		break;
	default: /* STATE_CONNECTED */
		/* We send reset and return to idle, how should this be handled! */
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		break;
	}

        features = ECM_CONN_GET_FEATURES(hdr);
	ulh_tbuff_pop(tb, ECM_CONN_SIZE(features));
	return ECM_CONN_GET_NEXT(hdr);
}

static int process_conn_ack(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
        char *features;

	hdr = (uint32_t *)ulh_tbuff_get(tb);

	switch(co->state) {
	case STATE_INIT: /* no break on purpose! */
	case STATE_IDLE:
		/* We are not ready to deal with messages yet, ignore it! */
		break;
	case STATE_DISCONNECTED:
		/* Should not get this message in this state, lets respond with a
		   reset and keep our state. */
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
		break;
	case STATE_CONNECTING_1:
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
		co->state = STATE_DISCONNECTED;
		break;
	case STATE_CONNECTING_2:
		co->rconnid = ECM_CONN_GET_CONNID(hdr);
		co->state = STATE_CONNECTED;
		__ecm_uc_connected(co);
		ecm_timer_arm(co, &co->ack_timer, co->kpa_tmo);
		ecm_timer_arm(co, &co->sup_timer, co->sup_tmo);
		ecm_timer_arm(co, &co->rt_timer, co->rt_tmo);
		ecm_timer_cancel(co, &co->conn_timer);
		break;
	default: /* STATE_CONNECTED */
		/* We send reset and return to idle, how should this be handled! */
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		break;
	}

        features = ECM_CONN_GET_FEATURES(hdr);
	ulh_tbuff_pop(tb, ECM_CONN_SIZE(features));
	return ECM_CONN_GET_NEXT(hdr);
}

static int process_conn_reset(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
        char *features;

	hdr = (uint32_t *)ulh_tbuff_get(tb);

	switch(co->state) {
	case STATE_INIT: /* no break on purpose! */
	case STATE_IDLE:
		/* We are not ready to deal with messages yet, ignore it! */
		break;
	case STATE_DISCONNECTED:
		/* We are already disconnected, ignore. */
		break;
	case STATE_CONNECTING_1:
		co->state = STATE_DISCONNECTED;
		ecm_timer_arm(co, &co->conn_timer, co->conn_tmo/10);
		break;
	case STATE_CONNECTING_2:
		co->state = STATE_DISCONNECTED;
		ecm_timer_arm(co, &co->conn_timer, co->conn_tmo/10);
		break;
	default: /* STATE_CONNECTED */
		/* We send reset and return to idle, how should this be handled! */
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		break;
	}

        features = ECM_CONN_GET_FEATURES(hdr);
	ulh_tbuff_pop(tb, ECM_CONN_SIZE(features));
	return ECM_CONN_GET_NEXT(hdr);
}

static int ecm_rx_main(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;

	hdr = (uint32_t *)ulh_tbuff_get(tb);
	if(ECM_MAIN_GET_VER(hdr) != ECM_VER_3) {
		ULH_TRACE_ERROR("Malformed packet received, not supported version: %d",
				ECM_MAIN_GET_VER(hdr));
		return ECM_LAST;
	}
	if(ECM_MAIN_GET_NEXT(hdr) != ECM_CONN &&
	   ECM_MAIN_GET_CONNID(hdr) != co->cid) {
		ULH_TRACE_ERROR("Packet received from incorrect connection %d should be %d",
				ECM_MAIN_GET_CONNID(hdr), co->cid);
		return ECM_LAST;
	}

	tb->size = ECM_MAIN_GET_PKTSIZE(hdr);
	ulh_tbuff_pop(tb, ECM_MAIN_SIZE);
	ecm_timer_arm(co, &co->sup_timer, co->sup_tmo);
	return ECM_MAIN_GET_NEXT(hdr);
}

static int ecm_rx_conn(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
	int next;

	hdr = (uint32_t *)ulh_tbuff_get(tb);
	switch(ECM_CONN_GET_TYPE(hdr)) {
	case ECM_CONNTYPE_CONNECT:
		next = process_conn_connect(co, tb);
		break;
	case ECM_CONNTYPE_CONNECT_ACK:
		next = process_conn_connect_ack(co, tb);
		break;
	case ECM_CONNTYPE_ACK:
		next = process_conn_ack(co, tb);
		break;
	case ECM_CONNTYPE_RESET:
		next = process_conn_reset(co, tb);
		break;
	default:
		ULH_TRACE_ERROR("Malformed packet received, unknown connect type: %d",
				ECM_CONN_GET_TYPE(hdr));
		next = ECM_LAST;
		break;
	}

	return next;
}

static int free_acked_packets(struct ulh_ecm_conn *co, uint16_t ackno)
{
	struct ulh_tbuff *tb;
	int num_freed = 0;
	uint32_t *hdr;
	uint16_t seq = (co->tx_ack + 1) & ECM_SEQ_MASK;


        /* Check if this ackno has been processed already */
        if (co->tx_ack == ackno)
                return 0;

	if (!IN_WINDOW(ackno, seq, co->tx_outstanding)) {
		return 0;
	}

	co->tx_ack = ackno;
	/* free acknowledged packets */
	while ((tb = ulh_tbuff_dequeue(&co->tx_unack_queue)) != NULL) {

		hdr = (uint32_t *) ulh_tbuff_get(tb);
		ECM_MAIN_ADVANCE(hdr);
		seq = ECM_ACK_GET_SEQNO(hdr);

		/* the ack signals the next sequence number expected. */
		if (seq == co->tx_ack) {
			ulh_tbuff_queue_head(&co->tx_unack_queue, tb);
			break;
		}

		free_buffer_to_pool(&co->tbpool, tb);
		co->tx_outstanding--;
		num_freed++;
	}

	if (num_freed) {
		/* reset retransmission counter, as we are having progress */
		ecm_timer_arm(co, &co->rt_timer, co->rt_tmo);
		co->rt_cnt = 0;
	}
	return num_freed;
}

static int ecm_rx_udata(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr, msgno;
	struct ulh_ecm_link *link;
	union itc_msg *msg;

	/*only one link supported in the ECM protocol, use link 0. */
	link = &co->links[0];
	if(link->reassm.tot_size != 0) {
		ULH_TRACE_ERROR("%s: UDATA received when expecting a fragment",
				co->name);
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		free_buffer_to_pool(&co->rxpool, tb);
		return ECM_LAST;
	}

	hdr = (uint32_t *) ulh_tbuff_get(tb);
	if(ECM_UDATA_GET_O(hdr)) {
		/* We do not support out of band data, should we?? */
		ULH_TRACE_ERROR("%s: UDATA received out of band data, we do not support this",
				co->name);
		/* Should we reset connection here? We do so for now. */
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		free_buffer_to_pool(&co->rxpool, tb);
		return ECM_LAST;
	}
	link->rx_cmhdr.dst = ECM_UDATA_GET_DST(hdr);
	link->rx_cmhdr.src = ECM_UDATA_GET_SRC(hdr);
	ulh_tbuff_pop(tb, ECM_UDATA_SIZE);

	co->stats.rxdata++;
        co->stats.rxbytes += tb->size;

	if(ECM_UDATA_GET_M(hdr)) {
		link->reassm.tot_size = ulh_tbuff_len(tb);
		link->reassm.frag_no   = ECM_UDATA_GET_FRAGNO(hdr);
		link->reassm.num_frags = 1;
		link->reassm.tb[0]     = tb;
	} else {
		if(ECM_UDATA_GET_FRAGNO(hdr) != 0x7FFF) {
			ULH_TRACE_ERROR("%s: Incorrect fragment number in not fragmented packet, frag: %d",
					co->name, ECM_UDATA_GET_M(hdr));
			ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
			free_buffer_to_pool(&co->rxpool, tb);
			return ECM_LAST;
		}
		link->rx_cmhdr.size = ulh_tbuff_len(tb);
		msg = itc_alloc(link->rx_cmhdr.size, 0);
		memcpy(msg, ulh_tbuff_get(tb), link->rx_cmhdr.size);
		RLNH_PRINT_HEADER(co->name, "RX ", &link->rx_cmhdr,
				  (uint8_t *)msg);

		free_buffer_to_pool(&co->rxpool, tb);

                /* Getting msg no from hdr udata *(uint32_t *)(hdr+3) */
                msgno = ECM_GET_MSGNO_FROM_DATA(hdr);
                ULH_TRACE(ulh_trace_sig, __FILE__, __LINE__, msgno, __func__);
		link->uc->uc_delivery(link->uc_data,
				      &link->rx_cmhdr,
				      msg);
	}

	if(co->state != STATE_CONNECTED) {
		ULH_TRACE_INFO("%s: Link disconnected in uc_delivery abort handling packet",
			       co->name, ECM_UDATA_GET_M(hdr));
		return ECM_LAST;
	}

	if (co->rx_outstanding++ == 0) {
		ecm_timer_arm(co, &co->ack_timer, co->ack_tmo);
	} else if (co->rx_outstanding >= (co->tx_wnd/4)) {
		ecm_tx_ack(co, 0);
	}

	return ECM_UDATA_GET_NEXT(hdr);
}

static int ecm_rx_frag(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
	struct ulh_ecm_link *link;
	union itc_msg *msg;
	char *tmp;
	int i;

	/*only one link supported in the ECM protocol, use link 0. */
	link = &co->links[0];

	if(link->reassm.tot_size == 0) {
		ULH_TRACE_ERROR("Fragment received when "
				"udata frame expected", 0);
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		free_buffer_to_pool(&co->rxpool, tb);
		return ECM_LAST;
	}

	hdr = (uint32_t *) ulh_tbuff_get(tb);
	if(ECM_FRAG_GET_FRAGNO(hdr) != link->reassm.frag_no) {
		ULH_TRACE_ERROR("%s: incorrect fragment received "
				"frag %d should be %d",
				co->name, ECM_FRAG_GET_FRAGNO(hdr),
				link->reassm.frag_no);
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		free_buffer_to_pool(&co->rxpool, tb);
		return ECM_LAST;
	}
	ulh_tbuff_pop(tb, ECM_FRAG_SIZE);

	co->stats.rxfrag++;
        co->stats.rxbytes += tb->size;

	link->reassm.tot_size += ulh_tbuff_len(tb);
	link->reassm.tb[link->reassm.num_frags] = tb;
	link->reassm.num_frags++;

	if (co->rx_outstanding++ == 0) {
		ecm_timer_arm(co, &co->ack_timer, co->ack_tmo);
	} else if (co->rx_outstanding >= (co->tx_wnd/4)) {
		ecm_tx_ack(co, 0);
	}

	if(!ECM_FRAG_GET_M(hdr)) {
		msg = itc_alloc(link->reassm.tot_size, 0);
		tmp = (char *)msg;
		for(i=0 ; i<link->reassm.num_frags ; i++) {
			memcpy(tmp, ulh_tbuff_get(link->reassm.tb[i]),
			       ulh_tbuff_len(link->reassm.tb[i]));
			tmp += ulh_tbuff_len(link->reassm.tb[i]);
			free_buffer_to_pool(&co->rxpool, link->reassm.tb[i]);
		}
		link->rx_cmhdr.size = link->reassm.tot_size;
		RLNH_PRINT_HEADER(co->name, "RX", &link->rx_cmhdr,
				  (uint8_t *)msg);
		link->uc->uc_delivery(link->uc_data,
				      &link->rx_cmhdr,
				      msg);
		link->reassm.tot_size  = 0;
		link->reassm.num_frags = 0;
	}

	return ECM_FRAG_GET_NEXT(hdr);
}

static int ecm_rx_nack(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
	uint16_t nack_seq;
	uint16_t nack_cnt;

	if(co->state != STATE_CONNECTED) {
		ULH_TRACE_INFO("%s: NACK in unexpected state %u", co->name,
			       co->state);
		return ECM_LAST;
	}

	hdr = (uint32_t *) ulh_tbuff_get(tb);
	nack_seq = ECM_NACK_GET_SEQNO(hdr);
	nack_cnt = ECM_NACK_GET_COUNT(hdr);

	co->stats.rxnack++;
	co->stats.txresend_nack += nack_cnt;

	(void) ecm_retransmit(co, nack_seq, nack_cnt);
	return ECM_NACK_GET_NEXT(hdr);
}

static int ecm_rx_data(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	struct ulh_tbuff *q;
	uint32_t *hdr, msgno;
	uint16_t ns;
	int j, out_of_order = 1;

	hdr = (uint32_t *)ulh_tbuff_get(tb);
	ns = ECM_ACK_GET_SEQNO(hdr);

	if (co->rx_queue[ns & RXQ_MASK])
		return ECM_LAST;

	q = ulh_tbuff_pool_get(&co->rxpool);
	if (!q) {
		ULH_TRACE_ERROR("ECM: %s: unable to alloc rxTB", co->name);
		ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
		return ECM_LAST;
	}

	q->data = tb->data;
	q->size = tb->size;
	q->rbuf = tb->rbuf;
	ulh_tbuff_hold(q);

	/* Add message to rxq, then attempt to deliver the rxq  */
	co->rx_queue[ns & RXQ_MASK] = q;
	for (j = co->rx_seq & RXQ_MASK, q = co->rx_queue[j]; q; ) {
		co->rx_queue[j] = NULL;
		hdr = (uint32_t *)ulh_tbuff_get(q);
		ulh_tbuff_pop(q, ECM_ACK_SIZE);

		(ECM_ACK_GET_NEXT(hdr) == ECM_UDATA) ? 
			ecm_rx_udata(co, q) : ecm_rx_frag(co, q);

		co->rx_seq = (co->rx_seq + 1) & ECM_SEQ_MASK;
		j = (j + 1) & RXQ_MASK;
		q = co->rx_queue[j];
		out_of_order = 0;
	}

	if (out_of_order) {
		uint16_t nack_seq = ns;
		uint16_t nack_cnt;
				 j = ns & RXQ_MASK;

		/* Nothing was delivered. Check if we made  a new "hole". */
		for (j = (j - 1) & RXQ_MASK; nack_seq != co->rx_seq;) {
			if (co->rx_queue[j])
				break;
			nack_seq = (nack_seq - 1) & ECM_SEQ_MASK;
			j = (j - 1) & RXQ_MASK;
		}
                if(ECM_GET_ACK_NEXT_FROM_ACK(hdr) == ECM_UDATA) {
                        /* Getting msg no from hdr act *(uint32_t *)(hdr+4) */
                        msgno = ECM_GET_MSGNO_FROM_ACK(hdr);
                        ULH_TRACE(ulh_trace_sig, __FILE__, __LINE__, msgno, __func__);
                }

		nack_cnt = (ns - nack_seq) & ECM_SEQ_MASK;
		if (nack_cnt)
			ecm_tx_nack(co, nack_seq, nack_cnt);
	}
	return ECM_LAST;
}

static int ecm_rx_ack(struct ulh_ecm_conn *co, struct ulh_tbuff *tb)
{
	uint32_t *hdr;
	uint16_t ns, nr;
	int next;

	if(co->state != STATE_CONNECTED) {
		ULH_TRACE_INFO("%s: ACK in unexpected state %u", co->name,
			       co->state);
		return ECM_LAST;
	}

	hdr = (uint32_t *)ulh_tbuff_get(tb);
	ns = ECM_ACK_GET_SEQNO(hdr);
	nr = ECM_ACK_GET_ACKNO(hdr);
	next = ECM_ACK_GET_NEXT(hdr);

	if (ECM_ACK_GET_R(hdr))
		ecm_tx_ack(co, 0);

	if ((next == ECM_UDATA) || (next == ECM_FRAG)) {
		if (!IN_WINDOW(ns, co->rx_seq, co->tx_wnd)) {

			/* Packet not in receive window. Note that this can
			** happen e.g in case we have requested a resend while
			** the peers resend timer has expired, i.e duplicate
			** resends.
			** Disconnect if this ns can not be a duplicate.
			*/

			uint16_t base = (co->rx_seq - co->tx_wnd) & ECM_SEQ_MASK;

			if (!IN_WINDOW(ns, base, 2*co->tx_wnd)) {

				ULH_TRACE_ERROR("%s: bad N(S)=%d received. "
                                      "Expected  %d", co->name, ns, co->rx_seq);

				ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
			}
			return ECM_LAST;
		}
		next = ecm_rx_data(co, tb);

	} else {
		ulh_tbuff_pop(tb, ECM_ACK_SIZE);
		co->stats.rxack++;
	}

	/* send more */
	if (free_acked_packets(co, nr))
		ecm_tx(co);

	return next;
}

static void ecm_connect(struct ulh_ecm_conn *co)
{
	if(co->state > STATE_DISCONNECTED) {
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
	} else if(co->state == STATE_IDLE) {
		while (ulh_trans_attach(co->cid, co->mbox, co->uref)) {
			ULH_TRACE_INFO("%s: failed to attach", __func__);
			usleep(100000);
		}
	}

	ecm_tx_connect(co, ECM_CONNTYPE_CONNECT);
	/* To spend 20% of the time in state connecting 1 and the rest of
	   the time waiting for connects from the other side (grace period).*/
	co->state = STATE_CONNECTING_1;
	ecm_timer_arm(co, &co->conn_timer, co->conn_tmo/10);
}

static void ecm_disconnect(struct ulh_ecm_conn *co, int reconnect,
			   const char *function, const int line)
{
	struct ulh_tbuff *fb;
	int j;

	ULH_TRACE_INFO("ECM disconnect called, cid:%d co:%p "
		       "state:%d name: %s from %s line %d",
		       co->cid, co, co->state, co->name, function, line);

	if (co->state == STATE_CONNECTED) {
		__ecm_uc_disconnected(co);
	}
	if (co->state > STATE_DISCONNECTED) {
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
	}
	if (co->state > STATE_IDLE) {
		co->state = STATE_DISCONNECTED;

		ecm_timer_cancel(co, &co->conn_timer);
		ecm_timer_cancel(co, &co->ack_timer);
		ecm_timer_cancel(co, &co->rt_timer);
		ecm_timer_cancel(co, &co->sup_timer);
	}

	while ((fb = ulh_tbuff_dequeue(&co->tx_unack_queue)))
		free_buffer_to_pool(&co->tbpool, fb);

	while ((fb = ulh_tbuff_dequeue(&co->links[0].tx_queue)))
		free_buffer_to_pool(&co->tbpool, fb);

	for (j = 0; j < RXQ_LENGTH; j++) {
		fb = co->rx_queue[j];
		if (fb) {
			free_buffer_to_pool(&co->rxpool, fb);
			co->rx_queue[j] = NULL;
		}
	}

	for (j = 0; j < co->links[0].reassm.num_frags; j++) {
		fb = co->links[0].reassm.tb[j];
		if (fb) {
			free_buffer_to_pool(&co->rxpool, fb);
			co->links[0].reassm.tb[j] = NULL;
		}
	}

	if (reconnect &&
	    co->state == STATE_DISCONNECTED)
		ecm_timer_arm(co, &co->conn_timer, co->conn_tmo);
	else {
		co->state = STATE_IDLE;
		ulh_trans_detach(co->cid, co->mbox, co->uref);
	}
}

static void ecm_conn_tmo(void *handle)
{
	struct ulh_ecm_conn *co = handle;

	if(co->state == STATE_CONNECTING_1) {
		co->state = STATE_DISCONNECTED;
		ecm_tx_connect(co, ECM_CONNTYPE_RESET);
		/* Trigger the timer to leave 80% of the conn_tmo
		   in state disonnected. */
		ecm_timer_arm(co, &co->conn_timer, ((co->conn_tmo * 9)/10));
	} else {
		ecm_connect(co);
	}
}

static void ecm_ack_tmo(void *handle)
{
	struct ulh_ecm_conn *co = handle;

	ecm_tx_ack(co, 1);
	ecm_timer_arm(co, &co->ack_timer, co->kpa_tmo);
}

static void ecm_rt_tmo(void *handle)
{
	struct ulh_ecm_conn *co = handle;
	uint16_t ns;
        uint32_t idx;

	if (co->tx_outstanding) {
                idx = (co->rt_cnt) % (RT_TMO_SEQ_CNT);

                if (++co->rt_cnt > co->rt_limit) {
			ULH_TRACE_INFO("ECM: %s retransmission limit reached",co->name);
			ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
			return;
		}

		ns = (co->tx_seq + 1 - co->tx_outstanding) & ECM_SEQ_MASK;
		co->stats.txresend_tmo += co->tx_outstanding;
		if (ecm_retransmit(co, ns, co->tx_outstanding)) {
                        ecm_timer_arm(co, &co->rt_timer,
                                      (rt_tmo_fibonacci[idx] * co->rt_tmo));
                }
	}
}


static void ecm_sup_tmo(void *handle)
{
	struct ulh_ecm_conn *co = handle;

	ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
}


static int __ecm_all_init(struct ulh_ecm_conn *co)
{
	int i;

	for (i = 0; i < co->nlinks; i++) {
		if (!co->links[i].uc)
			return 0;
	}

	return 1;
}

static int __ecm_all_connect(struct ulh_ecm_conn *co)
{
	int i;

	for (i = 0; i < co->nlinks; i++) {
		if (!co->links[i].uc || !co->links[i].connected)
			return 0;
	}

	return 1;
}

static int ecm_dc_init(void *handle, struct ulh_cm_uc_ops *uc,
		       void *uc_data, uint32_t prio)
{
	struct ulh_ecm_conn *co = handle;

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

	if (__ecm_all_init(co))
		co->state = STATE_IDLE;

	return 0;
}

static int ecm_dc_fini(void *handle, uint32_t prio)
{
	struct ulh_ecm_conn *co = handle;

	if (!co)
		return -EINVAL;
	if (prio >= co->nlinks)
		return -EINVAL;
	if (!co->links[prio].uc)
		return 0;
	if (co->state != STATE_IDLE)
		return -EBUSY;

	co->links[prio].uc = NULL;
	co->links[prio].connected = 0;
	co->state = STATE_INIT;

	return 0;
}

static int ecm_dc_connect(void *handle, uint32_t prio)
{
	struct ulh_ecm_conn *co = handle;

	if (!co)
		return -EINVAL;
	if (prio >= co->nlinks)
		return -EINVAL;
	if (!co->links[prio].uc)
		return -EINVAL;
	if (co->links[prio].connected)
		return 0;

	co->links[prio].connected = 1;
	if (__ecm_all_connect(co)) {
		ecm_connect(co);
	}

	return 0;
}

static int ecm_dc_disconnect(void *handle, uint32_t prio)
{
	struct ulh_ecm_conn *co = handle;

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
		ecm_disconnect(co, 0, __FUNCTION__, __LINE__);

	return 0;
}

static int ecm_dc_transmit(void *handle, uint32_t prio,
			   struct ulh_cm_msghdr *cmhdr, union itc_msg *msg)
{
        struct ulh_tbuff *tb;
        struct ulh_ecm_conn *co = handle;
        uint32_t size = cmhdr->size;
        uint32_t frag_size, frag_no;
        uint32_t hdr_size = ECM_MAIN_SIZE + ECM_ACK_SIZE + ECM_UDATA_SIZE;
        uint32_t frag_max_size = co->mtu - hdr_size;
        uint8_t *pos = (uint8_t *) msg;
        uint32_t *hdr;
        int more;

        RLNH_PRINT_HEADER(co->name, "TX", cmhdr, (uint8_t *)msg);
        ULH_TRACE(ulh_trace_sig, __FILE__, __LINE__, *(uint32_t*)msg, __func__);

        if (prio >= co->nlinks)
                return -EINVAL;
        if (co->state != STATE_CONNECTED) {
                ULH_TRACE_INFO("ECM: %s: dc_transmit in wrong state", co->name);
                return -EINVAL;
        }

        if(size > frag_max_size) {
                frag_size = frag_max_size;
                more = 1;
                frag_no = 0;
        } else {
                frag_size = size;
                more = 0;
                frag_no = 0x7FFF;
        }

        /* Send first packet as a regular udata packet. */
        tb = ulh_tbuff_pool_get(&co->tbpool);
        if (!tb) {
                /* this change is to handle unable to allocat tb issue.
                 * without this change, ecm_disconnect being called recursively
                 * which instead tries to allocate again and again buffers from 8192 buffers.
                 */
                if (!co->shutdown)
                        co->shutdown = true;
                else
                        return -ENOMEM;

                ULH_TRACE_ERROR("Unable to allocate tb, name:%s unack cnt : %u, pool cnt : %u",
                                co->name, ulh_tbuff_queue_count(&co->tx_unack_queue),ulh_tbuff_queue_count(&co->links[0].tx_queue));



                ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
                return -ENOMEM;
        }

        if (ulh_tbuff_alloc(co->cid, frag_size + hdr_size, tb)) {
                ULH_TRACE_ERROR("%s: unable to allocate packet", co->name);
                ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
                return -ENOMEM;
        }

        co->stats.txdata++;

        hdr = (uint32_t *) ulh_tbuff_get(tb);
        /* The packet will consist of main hdr, ack hdr and udata hdr.
           The ack header will be written at transmit and is not touched here. */
        ECM_MAIN_SET(hdr, ECM_ACK, ECM_VER_3,
                        co->rconnid, (frag_size + hdr_size));
        ECM_MAIN_ADVANCE(hdr);
        ECM_ACK_SET(hdr, ECM_UDATA, 0, 0, 0);
        ECM_ACK_ADVANCE(hdr);
        ECM_UDATA_SET(hdr, ECM_LAST, 0, more, frag_no);
        ECM_UDATA_SET_DST(hdr, cmhdr->dst);
        ECM_UDATA_SET_SRC(hdr, cmhdr->src);
        ECM_UDATA_ADVANCE(hdr);

        memcpy(hdr, pos,frag_size);
        pos += frag_size;
        size -= frag_size;
        ulh_tbuff_queue(&co->links[prio].tx_queue, tb);

        while(size) {
                hdr_size = ECM_MAIN_SIZE + ECM_ACK_SIZE + ECM_FRAG_SIZE;

                if(size > frag_max_size) {
                        frag_size = frag_max_size;
                        more = 1;
                } else {
                        frag_size = size;
                        more = 0;
                }

                tb = ulh_tbuff_pool_get(&co->tbpool);
                if (!tb) {
                        /* this change is to handle unable to allocat tb issue.
                         * without this change, ecm_disconnect being called recursively
                         * which instead tries to allocate again and again buffers from 8192 buffers.
                         */
                        if (!co->shutdown)
                                co->shutdown = true;
                        else
                                return -ENOMEM;
                        ULH_TRACE_ERROR("Unable to allocate tb, name:%s unack cnt : %u, pool cnt : %u",
                                        co->name, ulh_tbuff_queue_count(&co->tx_unack_queue),ulh_tbuff_queue_count(&co->links[0].tx_queue));

                        ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
                        return -ENOMEM;
                }

                if (ulh_tbuff_alloc(co->cid, frag_size + hdr_size, tb)) {
                        ULH_TRACE_ERROR("%s: unable to allocate packet", co->name);
                        ecm_disconnect(co, 1, __FUNCTION__, __LINE__);
                        return -ENOMEM;
                }

                co->stats.txfrag++;

                hdr = (uint32_t *) ulh_tbuff_get(tb);
                /* The packet will consist of main hdr, ack hdr and udata hdr.
                   The ack header will be written at transmit and is not touched here. */
                ECM_MAIN_SET(hdr, ECM_ACK, ECM_VER_3,
                                co->rconnid, (frag_size + hdr_size));
                ECM_MAIN_ADVANCE(hdr);
                ECM_ACK_SET(hdr, ECM_FRAG, 0, 0, 0);
                ECM_ACK_ADVANCE(hdr);
                ECM_FRAG_SET(hdr, ECM_LAST, more, frag_no);
                ECM_FRAG_ADVANCE(hdr);

                memcpy(hdr, pos, frag_size);
                pos += frag_size;
                size -= frag_size;
                ulh_tbuff_queue(&co->links[prio].tx_queue, tb);
        }

        ecm_tx(co);
        return 0;
}

static void ecm_dc_receive(void *handle, uint32_t cid, struct ulh_tbuff *tb)
{
	struct ulh_ecm_conn *co = handle;
	int next;

	if (co->state < STATE_DISCONNECTED) {
		ulh_tbuff_free(tb);
		return;
	}

	ECM_PRINT_HEADER(co->name, "ECM RX", tb);
	co->stats.rx++;

	next = ECM_MAIN;
	while(next != ECM_LAST) {
		switch (next) {
		case ECM_MAIN:
			next = ecm_rx_main(co, tb);
			break;
		case ECM_CONN:
			next = ecm_rx_conn(co, tb);
			break;
		case ECM_ACK:
			next = ecm_rx_ack(co, tb);
			break;
		case ECM_NACK:
			next = ecm_rx_nack(co, tb);
			break;
		default:
			ULH_TRACE_ERROR("ECM: %s: unsupport header type 0x%x",
					co->name, next);
			/* XXX Link down??? */
			next = ECM_LAST;
			break;
		}
	}

	ulh_tbuff_free(tb);
}

static int display_thrput_info(struct ulh_ecm_conn *co,
                                        char *buf, int len)
{
        char text[MAX_THROUGHPUT_INFO_PER_LINK], *tmp_text;
        int i, buf_remain, idx, total_len = 0;
        tmp_text = text;
        buf_remain = len;
        len = snprintf(tmp_text,
                       buf_remain,
                       "\n************ Throughput of %s in last %d durations ************",
                       co->name, MAX_THROUGHPUT_HISTORY);

        tmp_text += len;
        buf_remain -= len;
        total_len = len;

        len = snprintf(tmp_text,
                       buf_remain,
                       "\n%4s %12s %14s %14s  %14s\n",
                       "S.NO", "tx_packet", "tx_bytes", "rx_packet",
                       "rx_bytes");
        if( len >= buf_remain)
                return -ERANGE;
        tmp_text += len;
        buf_remain -= len;
        total_len += len;
        idx = co->stats.curr_thrp_idx - 1;
        for (i = 1; i <= MAX_THROUGHPUT_HISTORY; i++) {
                if(idx < 0)
                        idx = (MAX_THROUGHPUT_HISTORY - 1);
                len = snprintf(tmp_text,
                               buf_remain,
                               "%2d %14d %14llu %14d  %14llu \n",
                               i,
                               co->stats.thrp_hist[idx].tx_packet,
                               co->stats.thrp_hist[idx].tx_bytes,
                               co->stats.thrp_hist[idx].rx_packet,
                               co->stats.thrp_hist[idx].rx_bytes);
                tmp_text += len;
                buf_remain -= len;
                total_len += len;
                idx--;
                if (buf_remain < 128)
                        return -ERANGE;
        }
        strcpy(buf, text);
        return total_len;

}

int ecm_dc_info(void *handle, ulh_cm_info lvl,
		char *text, int maxtextlen)
{
	struct ulh_ecm_conn *co = handle;
	char tmptext[MAXINFO];
	int i, avail, consumed_chars;
	int ret = 0;

	avail = (MAXINFO < maxtextlen) ? MAXINFO : maxtextlen;

	switch(lvl) {
	case CM_HEADING:
		consumed_chars = snprintf(tmptext, avail,
					  " cid state             rcid name             ");
		if(consumed_chars >= avail)
			return -ERANGE;
		ret = ulh_trans_conn_info(co->cid, TRANS_HEADING, (tmptext + consumed_chars),
					  (avail - consumed_chars));
		break;
	case CM_SUMMARY:
		consumed_chars = snprintf(tmptext, avail,
					  "%4d %s %5d %-16s ",
					  co->cid, state_to_string[co->state],
					  co->rconnid, co->name);
		if(consumed_chars >= avail)
			return -ERANGE;
		ret = ulh_trans_conn_info(co->cid, TRANS_SUMMARY, (tmptext + consumed_chars),
					  (avail - consumed_chars));
		break;
	case CM_DETAILED:
		consumed_chars = snprintf(tmptext, avail,
					  "ecm cid:             %d\n"
					  "ecm name:            %s\n"
					  "ecm state:           %s\n"
					  "ecm mbox:            0x%x\n"
					  "ecm mtu:             %d\n"
					  "ecm rem id:          %d\n"
					  "ecm rx seq:          %d\n"
					  "ecm rx out:          %d\n"
					  "ecm tx seq:          %d\n"
					  "ecm tx out:          %d\n"
					  "ecm tx ack:          %d\n"
					  "ecm tx window:       %d\n"
					  "ecm rt tmo:          %d\n"
					  "ecm rt limit:        %d\n"
					  "ecm ack tmo:         %d\n"
					  "ecm kpa tmo:         %d\n"
					  "ecm sup tmo:         %d\n"
					  "ecm conn tmo:        %d\n"
					  "ecm unack queue:     %d\n",
					  co->cid, co->name, state_to_string[co->state],
					  co->mbox, co->mtu, co->rconnid,
					  co->rx_seq, co->rx_outstanding, co->tx_seq,
					  co->tx_outstanding, co->tx_ack, co->tx_wnd,
					  co->rt_tmo, co->rt_limit, co->ack_tmo,
					  co->kpa_tmo, co->sup_tmo, co->conn_tmo,
					  ulh_tbuff_queue_count(&co->tx_unack_queue));
		if(consumed_chars >= avail)
			return -ERANGE;

		for(i=0 ; i<ULH_ECM_MAXLINKS ; i++) {
			if(co->links[i].connected) {
				consumed_chars += snprintf((tmptext + consumed_chars),
							   (avail - consumed_chars),
							   "ecm link(%d) txq:     %d\n",
							   i, ulh_tbuff_queue_count(&co->links[i].tx_queue));
				if(consumed_chars >= avail)
					return -ERANGE;

			}
		}
		consumed_chars += snprintf((tmptext + consumed_chars),
					   (avail - consumed_chars),
					   "ecm tx packets:     %d\n"
					   "ecm tx data:        %d\n"
					   "ecm tx frag:        %d\n"
					   "ecm tx ack:         %d\n"
					   "ecm tx nack:        %d\n"
					   "ecm tx resend tmo:  %d\n"
					   "ecm tx resend nack: %d\n"
					   "ecm rx packets:     %d\n"
					   "ecm rx data:        %d\n"
					   "ecm rx frag:        %d\n"
					   "ecm rx ack:         %d\n"
					   "ecm rx nack:        %d\n",
					   co->stats.tx, co->stats.txdata,
					   co->stats.txfrag, co->stats.txack,
					   co->stats.txnack, co->stats.txresend_tmo,
					   co->stats.txresend_nack, co->stats.rx,
					   co->stats.rxdata, co->stats.rxfrag,
					   co->stats.rxack, co->stats.rxnack);
		if(consumed_chars >= avail)
			return -ERANGE;
		ulh_trans_conn_info(co->cid, TRANS_DETAILED, (tmptext + consumed_chars),
				    (avail - consumed_chars));
		break;
	case CM_THROUGHPUT:
		ret = display_thrput_info(co, tmptext, maxtextlen);
		if(ret < 0)
			return -ERANGE;
		break;
	default:
		ret = -EINVAL;
		break;
	}

	if(ret < 0)
		return ret;

	strcpy(text, tmptext);

	return 0;
}

static void clear_thrput_history(void *handle)
{
        struct ulh_ecm_conn *co = handle;
        struct ecm_stats *stats = &(co->stats);
        memset(stats->thrp_hist, 0,
               MAX_THROUGHPUT_HISTORY * sizeof(struct ecm_throughput));
        stats->curr_thrp_idx = 0;

}

void disable_thrput_info(void *handle)
{
        clear_thrput_history(handle);
        return 0;
}

void enable_thrput_info(void *handle)
{
        struct ulh_ecm_conn *co = handle;
        struct ecm_throughput *temp_thrp = &(co->stats.temp_thrp);
        struct ecm_stats *stats = &(co->stats);
        /* copy Tx and Rx stats to temp when throughput is enabled */
        temp_thrp->tx_packet = stats->txdata + stats->txfrag;
        temp_thrp->rx_packet = stats->rxdata + stats->rxfrag;
        temp_thrp->tx_bytes  = stats->txbytes;
        temp_thrp->rx_bytes  = stats->rxbytes;
        return;
}

void update_thrput_info(void *handle)
{
        struct ulh_ecm_conn *co = handle;
        unsigned int curr_idx;
        struct ecm_throughput *temp_thrp = &(co->stats.temp_thrp);
        struct ecm_stats *stats = &(co->stats);

        curr_idx = stats->curr_thrp_idx;
        stats->thrp_hist[curr_idx].tx_packet = ((stats->txdata + stats->txfrag)
                                                        - temp_thrp->tx_packet);
        stats->thrp_hist[curr_idx].tx_bytes  = (stats->txbytes - temp_thrp->tx_bytes);
        stats->thrp_hist[curr_idx].rx_packet = ((stats->rxdata + stats->rxfrag)
                                                        - temp_thrp->rx_packet);
        stats->thrp_hist[curr_idx].rx_bytes  = (stats->rxbytes - temp_thrp->rx_bytes);

        temp_thrp->tx_packet = stats->txdata + stats->txfrag;
        temp_thrp->rx_packet = stats->rxdata + stats->rxfrag;
        temp_thrp->tx_bytes  = stats->txbytes;
        temp_thrp->rx_bytes  = stats->rxbytes;

        stats->curr_thrp_idx++;
        if (stats->curr_thrp_idx >= MAX_THROUGHPUT_HISTORY)
                stats->curr_thrp_idx = 0;

        return;
}

void ecm_dc_config(void* handle, lnh_cm_act_on option)
{
        switch(option)
        {
        case LNH_THROUGHPUT_ENABLE:
             enable_thrput_info(handle);
             break;
        case LNH_THROUGHPUT_DISABLE:
             disable_thrput_info(handle);
             break;
        case LNH_THROUGHPUT_UPDATE:
             update_thrput_info(handle);
             break;
        default:
                break;

        }

}

static struct ulh_cm_dc_ops ecm_cm_dc = {
	.dc_init       = ecm_dc_init,
	.dc_fini       = ecm_dc_fini,
	.dc_connect    = ecm_dc_connect,
	.dc_disconnect = ecm_dc_disconnect,
	.dc_transmit   = ecm_dc_transmit,
	.dc_receive    = ecm_dc_receive,
	.dc_info       = ecm_dc_info,
        .dc_config     = ecm_dc_config,
};

static int ecm_create_instance(void *unused, const char *name,
			       struct ulh_cm_instance *instance,
			       struct ulh_cm_config *config,
			       struct ulh_timerqueue *tqueue)
{
	struct ulh_ecm_conn *co;
	int i;
	struct ulh_cm_ecm_config *ecm_cfg =
		(struct ulh_cm_ecm_config *) config;

	if (ecm_cfg->cmn.cfg_size != sizeof(struct ulh_cm_ecm_config))
		return -EINVAL;
	if (!ecm_cfg->prios || ecm_cfg->prios > ULH_ECM_MAXLINKS)
		return -EINVAL;

	co = malloc(sizeof(*co));
	if (!co)
		goto fail;
	memset(co, 0, sizeof(*co));
	co->name = strdup(name);
	if (!co->name)
		goto fail;

        co->tqueue   = tqueue;
	co->cid      = ecm_cfg->cmn.cid;
	co->mbox     = ecm_cfg->cmn.mbox;
	co->uref     = ecm_cfg->cmn.uref;
	co->rt_tmo   = ecm_cfg->rt_tmo;
	co->ack_tmo  = ecm_cfg->ack_tmo;
	co->kpa_tmo  = ecm_cfg->kpa_tmo;
	co->sup_tmo  = ecm_cfg->sup_tmo;
	co->tx_wnd   = ecm_cfg->tx_wnd;
	co->rt_limit = ecm_cfg->rt_limit;
	co->conn_tmo = ecm_cfg->conn_tmo;
	co->mtu      = ulh_trans_getmtu(co->cid);
	if (co->mtu < 0)
		goto fail;
	memcpy(co->dstmac, ecm_cfg->dstmac, ETH_ALEN);
	memcpy(co->srcmac, ecm_cfg->srcmac, ETH_ALEN);
	co->state = STATE_INIT;
	co->nlinks = ecm_cfg->prios;
	for (i = 0; i < co->nlinks; i++) {
		ulh_tbuff_queue_init(&co->links[i].tx_queue);
		memset(&co->links[i].reassm, 0,
		       sizeof(struct ulh_ecm_rx_reassembly));
	}

	if (co->tx_wnd & (co->tx_wnd - 1)) {
		uint32_t wndsize = (uint32_t)(co->tx_wnd * 2);
		wndsize = (0x80000000 >> __builtin_clz(wndsize));
		co->tx_wnd = (uint16_t)wndsize;

		if (co->tx_wnd > 128)
			co->tx_wnd = 128;

		ULH_TRACE_INFO("%s adjusting window size to %d",
		       co->name, co->tx_wnd);
	}

	ecm_timer_init(&co->conn_timer, ecm_conn_tmo, co);
	ecm_timer_init(&co->ack_timer, ecm_ack_tmo, co);
	ecm_timer_init(&co->rt_timer, ecm_rt_tmo, co);
	ecm_timer_init(&co->sup_timer, ecm_sup_tmo, co);

	ulh_tbuff_queue_init(&co->tx_unack_queue);
	ulh_tbuff_pool_init(&co->tbpool, 8192);
	ulh_tbuff_pool_init(&co->rxpool, RXQ_LENGTH + ULH_ECM_MAXFRAGS);

	ULH_TRACE_INFO("ECM connection created cid:%u co:%p name:%s",
		       co->cid, co, co->name);

	instance->instance = co;
	instance->ops = &ecm_cm_dc;

	return 0;

fail:
	if (co) {
		if (co->name)
			free(co->name);
		free(co);
	}
	return -EFAULT;
}

static int ecm_destroy_instance(void *unused,
				struct ulh_cm_instance *instance)
{
	struct ulh_ecm_conn *co = (struct ulh_ecm_conn *)instance->instance;
	int rc = -EINVAL;

	ULH_TRACE_INFO("ECM connection destroyed cid:%u co:%p name:%s",
                       co->cid, co, co->name);

	/* If state > Idle we disconnect the connection. */
	if(co->state > STATE_IDLE)
		ecm_disconnect(co, 0, __FUNCTION__, __LINE__);

	if (co) {
		if (co->name)
			free(co->name);

		free(co);
		rc = 0;
	}
	return rc;
}


static struct ulh_cm_ops ecm_ops = {
	.create_instance = ecm_create_instance,
	.destroy_instance = ecm_destroy_instance,
};

int ulh_ecm_init(const char *name)
{
	return ulh_cm_register(name, &ecm_ops, NULL);
}


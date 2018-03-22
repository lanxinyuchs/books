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

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <arpa/inet.h>
#include <itc.h>

#include "ulh_cm.h"
#include "ulh_transport.h"
#include "ulh_hdlc.h"
#include "ulh_hdlc_proto.h"
#include "ulh_timer.h"

#ifdef LTTNG
#define TRACEPOINT_DEFINE
#include "hdlc_lttng.h"
#endif

#include "trace.h"



/*******************************************************************************
**  #defines
*******************************************************************************/
#define STATE_IDLE				0
#define STATE_CONNECTING		1
#define STATE_CONNECTED   		2

#define SEQ_MODULO				8
#define ECP_PADDING				32

#ifndef MSG_PRIO_BIT
#define MSG_PRIO_BIT 0
#endif

/*******************************************************************************
**  types
*******************************************************************************/
struct ulh_conn {
	char 					name[32];
	int 					state;
	int 					primary;
	uint8_t 				hdlc_addr;

	uint32_t                 msg_prio_bit;

	uint32_t 				cid;
	unsigned long 			uref;
	itc_mbox_id_t 			mbox;
	struct ulh_cm_uc_ops 	*uc;
	void 					*uc_data;

	struct ulh_timerqueue 	*tqueue;
	struct ulh_tbuff_pool 	tb_pool;
	struct ulh_tbuff_queue 	tx_lprio_queue;
	struct ulh_tbuff_queue 	tx_hprio_queue;
	struct ulh_tbuff_queue 	tx_unack_queue;

	struct ulh_timer 		connect_tmo;
	struct ulh_timer 		rt_tmo;
	struct ulh_timer 		keepalive_tmo;
	struct ulh_timer		losttoken_tmo;
	struct ulh_timer		no_cmd_tmo;

	union itc_msg 			*rx_frag;
	uint32_t 				rx_frag_off;
	struct ulh_cm_msghdr 	rx_frag_hdr;

	int 	 				dc;
	int 	 				pf;

	uint32_t 				rx_seq;
	uint32_t 				tx_seq;
	uint32_t 				tx_outstanding;
	uint32_t 				rx_outstanding;
	uint32_t 				alive_cnt;
	uint32_t 				resend_cnt;

	uint32_t 				mtu;
	uint32_t 				tx_wnd_size;
	uint32_t 				cfg_rt_tmo;
	uint32_t 				cfg_keepalive_tmo;
	uint32_t 				cfg_no_cmd_tmo;
};

union itc_msg {
	uint32_t msg_no;
};

/*******************************************************************************
**  prototypes
*******************************************************************************/
static void hdlc_send_uframe(struct ulh_conn *, int, unsigned, int);
static void hdlc_send_sframe(struct ulh_conn *, int, uint32_t, int);
static void hdlc_stop(struct ulh_conn *);
static inline void hdlc_mark_conn_alive(struct ulh_conn *);
static int  hdlc_conn_dead(struct ulh_conn *);
static void hdlc_pf_sent(struct ulh_conn *);
static void hdlc_pf_received(struct ulh_conn *);
static void handle_conn_reset(struct ulh_conn *, int, int);
static void handle_UA_frame(struct ulh_conn *);
static void handle_SARM_frame(struct ulh_conn *, unsigned);
static void pri_rx_uframe(struct ulh_conn *, struct ulh_tbuff *);
static void sec_rx_uframe(struct ulh_conn *, struct ulh_tbuff *);
static void pri_rx_sframe(struct ulh_conn *, struct ulh_tbuff *);
static void sec_rx_sframe(struct ulh_conn *, struct ulh_tbuff *);
static void hdlc_rx_iframe(struct ulh_conn *, struct ulh_tbuff *);
static void hdlc_rx_deliver(struct ulh_conn *, struct ulh_tbuff *);
static int  hdlc_rx_iframe_inorder(struct ulh_conn*,struct ulh_tbuff*,uint32_t);
static int  hdlc_tx(struct ulh_conn *);
static void hdlc_retrans_task(void *);
static void hdlc_connect_task(void *);
static void hdlc_keepalive_task(void *);
static void hdlc_tx_free_ack(struct ulh_conn *, int);
static int hdlc_dc_init(void *, struct ulh_cm_uc_ops *,void *, uint32_t);
static int hdlc_dc_fini(void *, uint32_t);
static int hdlc_dc_connect(void *, uint32_t);
static int hdlc_dc_disconnect(void *, uint32_t);
static int hdlc_dc_transmit(void*,uint32_t,struct ulh_cm_msghdr*,union itc_msg*);
static void hdlc_dc_receive(void *, uint32_t, struct ulh_tbuff *);
static int hdlc_create_instance(void *, const char *,struct ulh_cm_instance *,
								struct ulh_cm_config *, struct ulh_timerqueue *);
static int hdlc_destroy_instance(void *,struct ulh_cm_instance *);
int ulh_hdlc_init(const char *);


/*******************************************************************************
**  locals
*******************************************************************************/
static void hdlc_send_uframe(struct ulh_conn *co,int type,unsigned addr,int pf)
{
	struct ulh_tbuff *fb;
	struct hdlc_header *hdlc;

	fb = ulh_tbuff_pool_get(&co->tb_pool);
	if (!fb)
		return;

	if (ulh_tbuff_alloc(co->cid, HDLC_HLEN + ECP_PADDING, fb)) {
		ulh_tbuff_pool_put(&co->tb_pool, fb);
		return;
	}

	fb->size -= ECP_PADDING;
	hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
	hdlc_build_uframe(hdlc, addr, type, pf);
	hdlc_pf_sent(co);

	HDLC_HEADER(co->name, "HDLC TX", fb->data, fb->size);
	ulh_trans_transmit(co->cid, fb);
	ulh_tbuff_pool_put(&co->tb_pool, fb);
}

static void hdlc_send_sframe(struct ulh_conn *co, int type,
		uint32_t rx_seq, int pf)
{
	struct ulh_tbuff *fb;
	struct hdlc_header *hdlc;

	fb = ulh_tbuff_pool_get(&co->tb_pool);
	if (!fb)
		return;
	if (ulh_tbuff_alloc(co->cid, HDLC_HLEN, fb)) {
		ulh_tbuff_pool_put(&co->tb_pool, fb);
		return;
	}

	hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
	hdlc_build_sframe(hdlc, co->hdlc_addr, type, rx_seq, pf);
	hdlc_pf_sent(co);

	HDLC_HEADER(co->name, "HDLC TX", fb->data, fb->size);

	ulh_trans_transmit(co->cid, fb);
	ulh_tbuff_pool_put(&co->tb_pool, fb);
}


/*
** We are disconnecting... cancel all timers, flush all buffer queues and
** and reset counters.
*/
static void hdlc_stop(struct ulh_conn *co)
{
	struct ulh_tbuff *fb;

	ulh_timer_cancel(&co->connect_tmo);
	ulh_timer_cancel(&co->rt_tmo);
	ulh_timer_cancel(&co->keepalive_tmo);
	ulh_timer_cancel(&co->losttoken_tmo);
	ulh_timer_cancel(&co->no_cmd_tmo);

	while ((fb = ulh_tbuff_dequeue(&co->tx_lprio_queue))) {
		ulh_tbuff_free(fb);
		ulh_tbuff_pool_put(&co->tb_pool, fb);
	}

	while ((fb = ulh_tbuff_dequeue(&co->tx_hprio_queue))) {
		ulh_tbuff_free(fb);
		ulh_tbuff_pool_put(&co->tb_pool, fb);
	}

	while ((fb = ulh_tbuff_dequeue(&co->tx_unack_queue))) {
		ulh_tbuff_free(fb);
		ulh_tbuff_pool_put(&co->tb_pool, fb);
	}

	if (co->rx_frag) {
		itc_free(&co->rx_frag);
		co->rx_frag = NULL;
	}

	co->tx_outstanding = 0;
	co->rx_outstanding = 0;
	co->tx_seq = 0;
	co->rx_seq = 0;
	co->pf 	   = 0;
	co->alive_cnt 	= 0;
	co->resend_cnt 	= 0;

	if (co->primary)
		co->pf = 1;
}



static inline void hdlc_mark_conn_alive(struct ulh_conn *co)
{
	co->alive_cnt = 5;
	if (co->primary)
		ulh_timer_arm(&co->keepalive_tmo, co->tqueue,
				co->cfg_keepalive_tmo);
}

static int hdlc_conn_dead(struct ulh_conn *co)
{
	if (!--co->alive_cnt) {
		HDLC_INFO(" %s: connection lost. No reponse from peer", co->name);
		return 1;
	}
	return 0;
}

static void hdlc_pf_sent(struct ulh_conn *co)
{
	if (co->primary && co->pf)
		ulh_timer_arm(&co->losttoken_tmo, co->tqueue, 500);

	co->pf = 0;
}

static void hdlc_pf_received(struct ulh_conn *co)
{
	ulh_timer_cancel(&co->losttoken_tmo);
	co->pf = 1;
}

static void handle_conn_reset(struct ulh_conn *co, int dc, int line)
{
	HDLC_INFO("%s reset invoked from %d in state %d",
						co->name, line, co->state);

	if (dc)
		hdlc_send_uframe(co, dc, co->hdlc_addr, co->pf);

	hdlc_stop(co);

	switch (co->state) {
	case STATE_IDLE:
		return;
	case STATE_CONNECTING:
		break;

	case STATE_CONNECTED:
		co->state = STATE_CONNECTING;
		co->uc->uc_disconnected(co->uc_data);
		break;

	default:
		HDLC_ERROR("%s BUG", __func__);;
		break;
	}

	if (co->primary)
		ulh_timer_arm(&co->connect_tmo, co->tqueue, 500);
}

static void handle_UA_frame(struct ulh_conn *co)
{
	switch (co->state) {
	case STATE_IDLE:
		break;

	case STATE_CONNECTING:
		hdlc_mark_conn_alive(co);
		co->state = STATE_CONNECTED;
		co->uc->uc_connected(co->uc_data);
		break;

	case STATE_CONNECTED:
		handle_conn_reset(co, HDLC_UFRAME_DISC, __LINE__);
		break;

	default:
		HDLC_ERROR("%s BUG", __func__);;
		break;
	}
}

static void handle_SARM_frame(struct ulh_conn *co, unsigned addr)
{
	switch (co->state) {
	case STATE_IDLE:
		break;

	case STATE_CONNECTING:
		co->hdlc_addr = addr;
		hdlc_send_uframe(co, HDLC_UFRAME_UA, co->hdlc_addr, co->pf);
		co->state = STATE_CONNECTED;
		co->uc->uc_connected(co->uc_data);
		ulh_timer_arm(&co->no_cmd_tmo, co->tqueue, co->cfg_no_cmd_tmo);
		break;

	case STATE_CONNECTED:
		handle_conn_reset(co, 0, __LINE__);
		break;

	default:
		HDLC_ERROR("%s BUG", __func__);;
		break;
	}
}

static void pri_rx_uframe(struct ulh_conn *co, struct ulh_tbuff *fb)
{
	struct hdlc_header *hdlc = (struct hdlc_header *)ulh_tbuff_get(fb);
	int uframe_type = hdlc_extract_uframe_type(hdlc);

	switch (uframe_type) {
	case HDLC_UFRAME_UA:
		handle_UA_frame(co);
		break;

	case HDLC_UFRAME_DM:
		handle_conn_reset(co, 0, __LINE__);
		break;

	case HDLC_UFRAME_FRMR:
		handle_conn_reset(co, 0, __LINE__);
		break;

	default:
		HDLC_ERROR("%s: %s-Secondary sent unsupported"
			   " frame 0x%x", __func__, co->name, uframe_type);
		break;
	}
}

static void sec_rx_uframe(struct ulh_conn *co, struct ulh_tbuff *fb)
{
	struct hdlc_header *hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
	int uframe_type = hdlc_extract_uframe_type(hdlc);

	switch (uframe_type) {
	case HDLC_UFRAME_SARM:
		handle_SARM_frame(co, (unsigned)hdlc->hdlc_address);
		break;

	case HDLC_UFRAME_DISC:
		handle_conn_reset(co, co->state == STATE_CONNECTED ?
		                  HDLC_UFRAME_UA : 0, __LINE__);
		break;

	default:
		HDLC_ERROR("%s: %s-Primary sent unsupported"
			   " frame 0x%x", __func__, co->name, uframe_type);
		break;
	}
}

static void pri_rx_sframe(struct ulh_conn *co, struct ulh_tbuff *fb)
{
	struct hdlc_header *hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
	int sframe_type = hdlc_extract_sframe_type(hdlc);

	switch (sframe_type) {
	case HDLC_SFRAME_RR:
		hdlc_tx(co);
		break;

	case HDLC_SFRAME_RNR:
		return;

	case HDLC_SFRAME_REJ:
		ulh_timer_cancel(&co->rt_tmo);
		hdlc_retrans_task(co);
		break;

	default:
		HDLC_ERROR("%s: %s-Secondary sent unsupported"
			   " frame 0x%x", __func__, co->name, sframe_type);
		break;

	}
}

static void sec_rx_sframe(struct ulh_conn *co, struct ulh_tbuff *fb)
{
	struct hdlc_header *hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
	int sframe_type = hdlc_extract_sframe_type(hdlc);

	switch (sframe_type) {
	case HDLC_SFRAME_RR:
		hdlc_tx(co);
		break;

	case HDLC_SFRAME_RNR:
		return;

	case HDLC_SFRAME_REJ:
		HDLC_ERROR("%s: %s-Primary sent REJ",
							__func__, co->name);
		ulh_timer_cancel(&co->rt_tmo);
		hdlc_retrans_task(co);
		break;

	default:
		HDLC_ERROR("%s: %s-Primary sent unsupported"
			   " frame 0x%x", __func__, co->name, sframe_type);
		break;
	}

	/* return token if secondary and nothing to transmit */
	if (co->pf)
		hdlc_send_sframe(co, HDLC_SFRAME_RR, co->rx_seq, co->pf);
}

static void hdlc_rx_deliver(struct ulh_conn *co, struct ulh_tbuff *fb)
{
	struct ulh_cm_msghdr cmhdr;
	struct msg_header *hdr;

	if (ulh_tbuff_len(fb) < sizeof(*hdr)) {
		HDLC_ERROR("%s: %s: too short message",
								__func__, co->name);
		return;
	}

	hdr = (struct msg_header *) ulh_tbuff_get(fb);
	ulh_tbuff_pop(fb, sizeof(*hdr));
	cmhdr.size = ntohl(hdr->msg_size);

	if (cmhdr.size == ulh_tbuff_len(fb)) {
		union itc_msg *msg;
		cmhdr.src = ntohl(hdr->msg_src);
		cmhdr.dst = ntohl(hdr->msg_dst);

		msg = itc_alloc(ulh_tbuff_len(fb), 0);
		memcpy(msg, ulh_tbuff_get(fb), ulh_tbuff_len(fb));

		LNH_HEADER(co->name, "RX", &cmhdr, msg);
		co->uc->uc_delivery(co->uc_data, &cmhdr, msg);
	} else
		HDLC_ERROR("%s: size field (%d bytes) mismatch."
		"Buffer size is %d bytes", co->name, cmhdr.size, ulh_tbuff_len(fb));

}

static int hdlc_rx_iframe_inorder(struct ulh_conn *co,
		struct ulh_tbuff *fb, uint32_t seq)
{
	struct frag_header *fraghdr;
	struct msg_header *hdr;
	unsigned short fragcntl;

	ulh_tbuff_pop(fb, sizeof(struct hdlc_header));

	if (ulh_tbuff_len(fb) < sizeof(struct frag_header)) {
		HDLC_ERROR("%s fragmentation header missing!", __func__);
		return -1;
	}
	fraghdr = (struct frag_header *) ulh_tbuff_get(fb);
	fragcntl = ntohs(fraghdr->frag_bits);
	ulh_tbuff_pop(fb, sizeof(struct frag_header));

	/* simple case first */
	if ((fragcntl & FRAG_FIRST) && (fragcntl & FRAG_LAST)) {
		/* do we have ongoing reassembly? */
		if (co->rx_frag) {
			HDLC_ERROR("%s: unexpected unfragmented packet", __func__);
			return -1;
		}

		hdlc_rx_deliver(co, fb);
		return 0;
	}

	if (!co->rx_frag && !(fragcntl & FRAG_FIRST)) {
		HDLC_ERROR("%s: unexpected non-first fragment", __func__);
		return -1;
	}
	if (co->rx_frag && (fragcntl & FRAG_FIRST)) {
		HDLC_ERROR("%s: unexpected first fragment", __func__);
		return -1;
	}

	if (fragcntl & FRAG_FIRST) {
		if (ulh_tbuff_len(fb) < sizeof(*hdr)) {
			HDLC_ERROR("%s: %s: too short message",
					__func__, co->name);
			return -1;
		}

		hdr = (struct msg_header *) ulh_tbuff_get(fb);
		ulh_tbuff_pop(fb, sizeof(*hdr));

		co->rx_frag_hdr.src = ntohl(hdr->msg_src);
		co->rx_frag_hdr.dst = ntohl(hdr->msg_dst);
		co->rx_frag_hdr.size = ntohl(hdr->msg_size);

		co->rx_frag = itc_alloc(co->rx_frag_hdr.size, 0);
		memcpy(co->rx_frag, ulh_tbuff_get(fb), ulh_tbuff_len(fb));
		co->rx_frag_off = ulh_tbuff_len(fb);
		return 0;
	}

	memcpy((uint8_t *) co->rx_frag + co->rx_frag_off,
			ulh_tbuff_get(fb), ulh_tbuff_len(fb));
	co->rx_frag_off += ulh_tbuff_len(fb);

	if (fragcntl & FRAG_LAST) {
		union itc_msg *msg = co->rx_frag;
		if (co->rx_frag_off != co->rx_frag_hdr.size) {
			HDLC_ERROR("%s: invalid packet size (%u, %u)",
					__func__, co->rx_frag_off, co->rx_frag_hdr.size);
		}
		co->rx_frag = NULL;

		LNH_HEADER(co->name, "RX", &co->rx_frag_hdr, msg);
		co->uc->uc_delivery(co->uc_data, &co->rx_frag_hdr, msg);
	}
	return 0;
}

static void hdlc_rx_iframe(struct ulh_conn *co, struct ulh_tbuff *fb)
{
	struct hdlc_header *hdlc;
	uint32_t seq;

	hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
	seq = hdlc_extract_iframe_send_seqno(hdlc);

	/* out of order? */
	if (seq != co->rx_seq) {
		if (((seq + 1) % SEQ_MODULO) != co->rx_seq)
			HDLC_ERROR("%s: sequence error seq:%u exp:%u\n",
							co->name, seq, co->rx_seq);
		goto out;
	}

	/* in-order frame received */
	if (hdlc_rx_iframe_inorder(co, fb, seq) == 0) {
		co->rx_seq = (co->rx_seq + 1) % SEQ_MODULO;
		co->rx_outstanding = 1;
	}

out:
	if (hdlc_tx(co) == 0) {
		if (co->rx_outstanding) {
			hdlc_send_sframe(co, HDLC_SFRAME_RR, co->rx_seq, co->pf);
			co->rx_outstanding = 0;
		}
	}
}

static void hdlc_tx_free_ack(struct ulh_conn *co, int ack)
{
	struct ulh_tbuff *fb;
	struct hdlc_header *hdlc;
	int seq;

	while ((fb = ulh_tbuff_dequeue(&co->tx_unack_queue))) {
		hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
		seq = hdlc_extract_iframe_send_seqno(hdlc);

		if (seq == ack) {
			ulh_tbuff_queue_head(&co->tx_unack_queue, fb);
			break;
		}

		ulh_tbuff_free(fb);
		ulh_tbuff_pool_put(&co->tb_pool, fb);

		co->tx_outstanding--;
		co->resend_cnt = 0;
	}

	if (co->tx_outstanding == 0)
		ulh_timer_cancel(&co->rt_tmo);
}

static int hdlc_tx(struct ulh_conn *co)
{
	struct hdlc_header *hdlc;
	struct ulh_tbuff *fb;
	int tx_done = 0;

	while (co->tx_outstanding < co->tx_wnd_size) {

		if (!(fb = ulh_tbuff_dequeue(&co->tx_hprio_queue))) {
			if (!(fb = ulh_tbuff_dequeue(&co->tx_lprio_queue))) {
				break;
			}
		}

		hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
		hdlc_build_iframe(hdlc, co->hdlc_addr, co->rx_seq,
				co->tx_seq, co->pf);
		hdlc_pf_sent(co);

		/* add to unack queue */
		ulh_tbuff_queue(&co->tx_unack_queue, fb);

		/* increment ref to avoid fb to be freed by transport */
		ulh_tbuff_hold(fb);
		HDLC_HEADER(co->name, "HDLC TX", fb->data, fb->size);

		/* send */
		ulh_trans_transmit(co->cid, fb);
		co->tx_outstanding++;
		co->rx_outstanding = 0;
		co->tx_seq = (co->tx_seq + 1) % SEQ_MODULO;
		tx_done++;
	}

	/* start re-transmit timer */
	if (tx_done)
		ulh_timer_arm(&co->rt_tmo, co->tqueue, co->cfg_rt_tmo);
	return tx_done;
}

static void hdlc_connect_task(void *data)
{
	struct ulh_conn *co = data;

	switch (co->state) {
	case STATE_IDLE:
		if (ulh_trans_attach(co->cid, co->mbox, co->uref)) {
			HDLC_ERROR("%s: failed to attach", __func__);
			ulh_timer_arm(&co->connect_tmo, co->tqueue, 1000);
			return;
		}
		co->state = STATE_CONNECTING;

	case STATE_CONNECTING:
		if (co->primary) {
			hdlc_send_uframe(co, HDLC_UFRAME_SARM,
					co->hdlc_addr, 1);
		}
		break;

	case STATE_CONNECTED:
		if (hdlc_conn_dead(co)) {
			handle_conn_reset(co, co->dc, __LINE__);
		}
		break;

	default:
		HDLC_ERROR("%s: unexpected state %d", __func__,
												co->state);
		break;
	}

	ulh_timer_arm(&co->connect_tmo, co->tqueue, 500);
}

static void hdlc_retrans_task(void *data)
{
	struct ulh_conn *co = data;
	struct ulh_tbuff *fb;
	struct hdlc_header *hdlc;
	uint32_t seq;

	if (++co->resend_cnt > 10) {
		handle_conn_reset(co, co->dc, __LINE__);
		return;
	}

	/* retransmit first unacknowledged packet */
	fb = ulh_tbuff_dequeue(&co->tx_unack_queue);
	HDLC_INFO("%s retransmit fb = %p", co->name, (void*)fb);

	if (!fb)
		return;

	/* rebuild hdlc header, but keep send sequence */
	hdlc = (struct hdlc_header *) ulh_tbuff_get(fb);
	seq = hdlc_extract_iframe_send_seqno(hdlc);
	hdlc_build_iframe(hdlc, co->hdlc_addr, co->rx_seq, seq, co->pf);
	hdlc_pf_sent(co);


	/* increment ref to avoid fb to be freed by transport */
	ulh_tbuff_hold(fb);

    HDLC_HEADER(co->name, "HDLC TX", fb->data, fb->size);
	ulh_trans_transmit(co->cid, fb);
	ulh_tbuff_queue_head(&co->tx_unack_queue, fb);
	ulh_timer_arm(&co->rt_tmo, co->tqueue, co->cfg_rt_tmo);
}

static void hdlc_keepalive_task(void *data)
{
	struct ulh_conn *co = data;

	if (hdlc_tx(co) == 0)
		hdlc_send_sframe(co, HDLC_SFRAME_RR, co->rx_seq, co->pf);

	ulh_timer_arm(&co->keepalive_tmo, co->tqueue, co->cfg_keepalive_tmo);
}

static void hdlc_losttoken_task(void *data)
{
	struct ulh_conn *co = data;

	if (co->state == STATE_CONNECTED) {
		HDLC_DBG("%s token lost. Restoring", co->name);
	}
	co->pf = 1;
}

static void hdlc_no_cmd_task(void *data)
{
	struct ulh_conn *co = data;

	handle_conn_reset(co, 0, __LINE__);
}

static int hdlc_dc_init(void *handle, struct ulh_cm_uc_ops *uc,
		void *uc_data, uint32_t prio)
{
	struct ulh_conn *co = handle;

	co->uc = uc;
	co->uc_data = uc_data;
	co->rx_frag = NULL;
	return 0;
}

static int hdlc_dc_fini(void *handle, uint32_t prio)
{
	struct ulh_conn *co = handle;
	co->uc = NULL;
	co->uc_data = NULL;
	return 0;
}

static int hdlc_dc_connect(void *handle, uint32_t prio)
{
	struct ulh_conn *co = handle;
	if (!co->uc)
		return -EINVAL;

	co->state = STATE_IDLE;
	hdlc_connect_task(handle);
	return 0;
}

static int hdlc_dc_disconnect(void *handle, uint32_t prio)
{
	struct ulh_conn *co = handle;

	hdlc_send_uframe(co, co->dc, co->hdlc_addr, co->pf);
	hdlc_stop(co);

	if (co->state == STATE_IDLE)
		return 0;

	HDLC_INFO("%s invoked", __func__);
	ulh_trans_detach(co->cid, co->mbox, co->uref);
	co->state = STATE_IDLE;
	return 0;
}

static int hdlc_dc_transmit(void *handle, uint32_t prio,
		struct ulh_cm_msghdr *hdr, union itc_msg *msg)
{
	struct ulh_conn *co = handle;
	struct frag_header *fhdr;
	struct hdlc_header *hdlc;
	struct ulh_tbuff *fb;
	uint32_t hdr_size = HDLC_HLEN + sizeof(*fhdr);
	uint32_t pos = 0, frag_size;
	uint32_t mtu = co->mtu - hdr_size;
	struct msg_header *msghdr;
	uint32_t msg_size = hdr->size;
	uint32_t size = hdr->size;
	struct ulh_tbuff_queue *tx_queue;
	uint16_t frag_prio_bit;

	if (co->state != STATE_CONNECTED)
		return -ENOTCONN;

	LNH_HEADER(co->name, "TX", hdr, msg);

	/* message header is going to be prepended */
	size += sizeof(*msghdr);

	if (msg->msg_no & co->msg_prio_bit) {
		msg->msg_no &= ~co->msg_prio_bit;
		tx_queue = &co->tx_hprio_queue;
		frag_prio_bit = FRAG_PRIORITY;
	} else {
		tx_queue = &co->tx_lprio_queue;
		frag_prio_bit = 0;
	}

	while (size) {
		frag_size = size;
		if (frag_size > mtu)
		       frag_size = mtu;

		size -= frag_size;

		fb = ulh_tbuff_pool_get(&co->tb_pool);
		if (!fb) {
			HDLC_ERROR("%s: unable to allocate fb",
					__func__);
			return -EBUSY;
		}

		if (ulh_tbuff_alloc(co->cid, hdr_size + frag_size +
					ECP_PADDING,fb)) {
			HDLC_ERROR("%s: unable to allocate fb", __func__);
			return -ENOBUFS;
		}

		fb->size -= ECP_PADDING;
		hdlc = (struct hdlc_header*)ulh_tbuff_get(fb);
		fhdr = (struct frag_header*)(hdlc + 1);

		/* fill frag header */
		fhdr->frag_bits = frag_prio_bit;
		if (!size)
			fhdr->frag_bits |= FRAG_LAST;
		if (!pos)
			fhdr->frag_bits |= FRAG_FIRST;
		fhdr->frag_bits = htons(fhdr->frag_bits);

		/* prepend message header to the first fragment */
		if (!pos) {
			msghdr = (struct msg_header *) (fhdr + 1);
			msghdr->msg_src = htonl(hdr->src);
			msghdr->msg_dst = htonl(hdr->dst);
			msghdr->msg_size = htonl(msg_size);
			memcpy((msghdr + 1), msg,frag_size - sizeof(*msghdr));
			pos += frag_size - sizeof(*msghdr);
		} else {
			memcpy((fhdr + 1), ((uint8_t *) msg) + pos,frag_size);
			pos += frag_size;
		}

		ulh_tbuff_queue(tx_queue, fb);
	}

	hdlc_tx(co);
	return 0;
}

static void hdlc_dc_receive(void *handle, uint32_t cid, struct ulh_tbuff *fb)
{
	struct hdlc_header *hdlc = (struct hdlc_header*)ulh_tbuff_get(fb);
	struct ulh_conn *co = handle;
	int type = hdlc_extract_frame_type(hdlc);


	if ((type != HDLC_UFRAME) && (co->state != STATE_CONNECTED)) {
		ulh_tbuff_free(fb);
		return;
	}


	HDLC_HEADER(co->name, "HDLC RX", fb->data, fb->size);

	if (hdlc_extract_pf(hdlc))
		hdlc_pf_received(co);

	if (type != HDLC_UFRAME) {
		hdlc_mark_conn_alive(co);
		hdlc_tx_free_ack(co, hdlc_extract_receive_seqno(hdlc));

		if (!co->primary) {
			ulh_timer_arm(&co->no_cmd_tmo, co->tqueue, co->cfg_no_cmd_tmo);
		}

		switch (type) {
			case HDLC_IFRAME:
				hdlc_rx_iframe(co, fb);
				break;
			case HDLC_SFRAME:
				co->primary ? pri_rx_sframe(co, fb) : sec_rx_sframe(co, fb);
				break;
		}
	} else
		co->primary ? pri_rx_uframe(co, fb) : sec_rx_uframe(co, fb);

	ulh_tbuff_free(fb);
}

int hdlc_dc_info(void *handle, ulh_cm_info lvl, char *text, int tlen)
{
	struct ulh_conn *co = handle;

	if (lvl == CM_HEADING)
		return 0;

	snprintf(text, tlen,
			"\nconn name     : %s\n"
			"state : %u\n"
			"tx_outstanding : %u\n"
			"rx_outstanding : %u\n"
			"tx_queue count : %u\n"
			"tx_uackq count : %u\n",
			co->name, co->state, co->tx_outstanding, co->rx_outstanding,
			ulh_tbuff_queue_count(&co->tx_lprio_queue) +
			ulh_tbuff_queue_count(&co->tx_hprio_queue),
			ulh_tbuff_queue_count(&co->tx_unack_queue));
	return 0;
}

static struct ulh_cm_dc_ops hdlc_cm_dc = {
	.dc_init = hdlc_dc_init,
	.dc_fini = hdlc_dc_fini,
	.dc_connect = hdlc_dc_connect,
	.dc_disconnect = hdlc_dc_disconnect,
	.dc_transmit = hdlc_dc_transmit,
	.dc_receive = hdlc_dc_receive,
	.dc_info = hdlc_dc_info,
};

static int hdlc_create_instance(void *unused, const char *name,
	struct ulh_cm_instance *instance,
	struct ulh_cm_config *config, struct ulh_timerqueue *tqueue)
{
	struct ulh_conn *co;
	struct ulh_cm_hdlc_config *hdlc_cfg =
		(struct ulh_cm_hdlc_config *) config;

	if (hdlc_cfg->cmn.cfg_size != sizeof(struct ulh_cm_hdlc_config))
		return -EINVAL;

	co = malloc(sizeof(*co));
	if (!co)
		return -ENOMEM;

	memset(co, 0, sizeof(*co));
    strncpy(co->name, name, sizeof(co->name));

	co->tqueue = tqueue;
	co->cid = hdlc_cfg->cmn.cid;
	co->mbox = hdlc_cfg->cmn.mbox;
	co->uref = hdlc_cfg->cmn.uref;
	co->state = STATE_IDLE;
	co->primary = hdlc_cfg->primary;
	co->hdlc_addr = hdlc_cfg->hdlc_addr;
	co->mtu = ulh_trans_getmtu(co->cid);
	co->dc = co->primary ? HDLC_UFRAME_DISC : HDLC_UFRAME_DM;
	co->msg_prio_bit = htonl(MSG_PRIO_BIT);

	co->cfg_rt_tmo = 100;
	co->cfg_keepalive_tmo = 100;
	co->cfg_no_cmd_tmo = 1000;
	co->tx_wnd_size = 1;

	ulh_timer_init(&co->connect_tmo, hdlc_connect_task, co);
	ulh_timer_init(&co->rt_tmo, hdlc_retrans_task, co);
	ulh_timer_init(&co->keepalive_tmo, hdlc_keepalive_task, co);
	ulh_timer_init(&co->losttoken_tmo, hdlc_losttoken_task, co);
	ulh_timer_init(&co->no_cmd_tmo, hdlc_no_cmd_task, co);

	ulh_tbuff_queue_init(&co->tx_lprio_queue);
	ulh_tbuff_queue_init(&co->tx_hprio_queue);
	ulh_tbuff_queue_init(&co->tx_unack_queue);

	ulh_tbuff_pool_init(&co->tb_pool, 8192);

	if (co->primary)
		co->pf = 1;

	HDLC_INFO("connection %s (hdlc_addr:%u cid:%u mtu:%u pri:%u) "
			"created", co->name, co->hdlc_addr,
			co->cid, co->mtu, co->primary);

	instance->instance = co;
	instance->ops = &hdlc_cm_dc;
	return 0;
}

static int hdlc_destroy_instance(void *unused,
		struct ulh_cm_instance *instance)
{
	struct ulh_conn *co = instance->instance;
	ulh_tbuff_pool_free(&co->tb_pool);
	free(co);
	return 0;
}


static struct ulh_cm_ops hdlc_ops = {
	.create_instance = hdlc_create_instance,
	.destroy_instance = hdlc_destroy_instance,
};

int ulh_hdlc_init(const char *name)
{
	return ulh_cm_register(name, &hdlc_ops, NULL);
}

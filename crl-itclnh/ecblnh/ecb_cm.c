/*
 * Copyright 2013 Ericsson AB
 * uabkiju <kimmo.juujarvi@ericsson.com>
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
  *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <itc.h>
#include <ulh_cm.h>
#include <ulh_transport.h>
#include <ulh_timer.h>
#include <arpa/inet.h>

#include "ulh_ecb.h"
#include "rhai-ecb.h"
#include "ecb-link-api.h"
#include "ecb-link-internal.h"
#include "ecb_transport.h"
#include "ecb_trace.h"


/*******************************************************************************
**  #defines
*******************************************************************************/


/* Fragment Header bit definitions. */
#define ECB_FRAG_SF_SIZE        14
#define ECB_FRAG_IL_SIZE        2
#define ECB_FRAG_SF_HDRSIZE     (ECB_FRAG_SF_SIZE + 2)
#define ECB_FRAG_IL_HDRSIZE     (ECB_FRAG_IL_SIZE + 2)

#define ECB_FRAG_S_HEADER       0xc000
#define ECB_FRAG_F_HEADER       0x8000
#define ECB_FRAG_I_HEADER       0x0000
#define ECB_FRAG_L_HEADER       0x4000
#define ECB_FRAG_V_MASK         0xc000
#define ECB_FRAG_P_HIGH         0x0100

/* HDLC Control Field definitions and macros. */
#define HDLC_U_FORMAT(c)        (((c) & 0x03) == 0x03)
#define HDLC_S_FORMAT(c)        (((c) & 0x03) == 0x01)
#define HDLC_I_FORMAT(c)        (((c) & 0x01) == 0x00)
#define HDLC_U_FUNCTION(c)      ((c) & 0xef)
#define HDLC_S_FUNCTION(c)      ((c) & 0x0f)
#define HDLC_SNRM               0x83
#define HDLC_DISC               0x43
#define HDLC_UA                 0x63
#define HDLC_DM                 0x0f
#define HDLC_FRMR               0x87
#define HDLC_RR                 0x01
#define HDLC_RNR                0x05
#define HDLC_I                  0x00
#define HDLC_NS(c)              (((c) & 0x0e) >> 1)
#define HDLC_NR(c)              (((c) & 0xe0) >> 5)
#define HDLC_VS_VR(vs, vr)      (((vs) << 1) | ((vr) << 5))
#define HDLC_VR(vr)             ((vr) << 5)
#define HDLC_PF                 0x10


#define DISC_ORDERED            0   /* Reasond for disconn */
#define DISC_PEER_CMD           1
#define DISC_NET_DOWN           2
#define DISC_CONN_TMO           3
#define DISC_ACK_TMO            4
#define DISC_BAD_IFRAME         5
#define DISC_FRM_REJECT         6
#define DISC_BAD_SFRAME         7
#define DISC_BUG                8

/* HDLC Window mask. */
#define HDLC_WINDOW_MASK        0x7

/* The Secondary Station is requested to Set NRM with this interval. */
#define HDLC_CONNECT_TMO        1000

/* The Secondary Station is at least polled once within this interval. */
#define HDLC_POLL_TMO      		30
#define HDLC_REPOLL_TMO      	10
#define EBUS_FREE_CONN_TMO      50
#define EBUS_FREE_DISC_TMO    	20

/* The Secondary Station should be able to respond within this time. */
#define HDLC_RESPONSE_TMO       300

/* Number of retransmissions. */
#define HDLC_MAX_RESENDS        2


/*******************************************************************************
**  types
*******************************************************************************/

#pragma pack(1)
union fraghdr {
	uint16_t       header;
	struct {
		uint16_t     header;
		uint32_t     src_addr;
		uint32_t     dst_addr;
		uint32_t     size;
		uint8_t      data[1];
	} first, single;

	struct {
		uint16_t     header;
		uint8_t      data[1];
	} inter, last;

	uint8_t        frmr[3];
};

struct frame {
	uint8_t          addr;
	uint8_t          ctrl;
	union fraghdr    info;
};
#pragma pack()

typedef enum {
	PRIMARY = 0, SECONDARY = 1
} modus_t;

typedef enum {
	NDM = 0, CNG = 1, NRM = 2
} state_t;


struct conn_object {
    modus_t                 modus;
    state_t                 state;
    void                    *lobj;
    struct ulh_cm_uc_ops    uc;

    uint8_t                 address;
    uint8_t                 vs;
    uint8_t                 vr;
    uint8_t                 va;
    uint8_t                 cp;

	struct ulh_tbuff_pool 	tb_pool;
	struct ulh_timerqueue	*tqueue;
	struct ulh_timer 		polltmo;

    struct timespec         cn_time;
	uint32_t                eb_time;
	uint32_t                cid;
	uint32_t                queued;
	unsigned long           uref;
	itc_mbox_id_t           mbox;

	struct ulh_tbuff_queue  tx_queue;

    struct ulh_tbuff		*tx_frags[8];
    struct timespec         tx_times[8];
    int                     tx_resend;
    unsigned                tx_pendin;
    unsigned                tx_queued;
    unsigned                tx_mqueue;
    unsigned                tx_window;
    unsigned                tx_mtu;

    uint32_t                 max_itc_msg_size;

    struct ulh_tbuff		*rx_frags[8];
	union itc_msg			*rx_current;
	uint32_t                rx_offset;
	struct ulh_cm_msghdr 	rx_hdr;

    uint32_t                fcs_error;
    char                    name[24];
};


/*******************************************************************************
**  prototypes
*******************************************************************************/

static void lock_bus(struct conn_object *co);
static void unlock_bus(struct conn_object *co);
static void tmo_ms(struct timespec* ts, uint32_t ms);
static int time_has_expired(struct timespec* ts);
static int  rx_iframe(struct conn_object *co, struct ulh_tbuff* skb);
static int  tx_iframe(struct conn_object *co);
static void rxp_uframe(struct conn_object *co, uint8_t ctrl);
static void rxs_uframe(struct conn_object *co, uint8_t ctrl);
static void rx_ctrframe(struct conn_object *co, uint8_t ctrl);
static void __disconnect(struct conn_object *co, state_t state, int, int, int);
static void __connect(struct conn_object *co);
static int send_iframe(struct conn_object *co, struct ulh_tbuff *skb);
static int  update_window(struct conn_object *co, uint8_t ack);
static void run_queued_tasks(void);
static void schedule_disconnect(struct conn_object *co);
static void do_poll_primary(struct conn_object *co);
static void poll_task_primary(void *data);
static void poll_task_secondary(void *data);
static void free_conn_buffers(struct conn_object *co);
static int dc_init(void *co, struct ulh_cm_uc_ops *uc, void *lo, uint32_t prio);
static int dc_finalize(void *co, uint32_t prio);
static int dc_connect(void *co, uint32_t prio);
static int dc_disconnect(void *co, uint32_t prio);
static int dc_transmit(void *co, uint32_t prio, struct ulh_cm_msghdr *hdr,
                                union itc_msg *msg);
static void dc_receive(void *co, uint32_t cid, struct ulh_tbuff *tbuf);
static int ecb_create_instance(void *priv, const char *name,
			       struct ulh_cm_instance *instance,
			       struct ulh_cm_config *config,
			       struct ulh_timerqueue *tqueue);
static int ecb_destroy_instance(void *priv, struct ulh_cm_instance *inst);
static void ecb_destroy(void *priv);


/*******************************************************************************
**  locals
*******************************************************************************/
static struct ulh_cm_dc_ops ecb_cm_ops = {
    .dc_init            = dc_init,
    .dc_fini            = dc_finalize,
    .dc_connect         = dc_connect,
    .dc_disconnect      = dc_disconnect,
    .dc_transmit        = dc_transmit,
    .dc_receive         = dc_receive,
};

static struct ulh_cm_ops ecb_ops = {
    .create_instance    = ecb_create_instance,
    .destroy_instance   = ecb_destroy_instance,
    .destroy            = ecb_destroy,
};

extern pthread_mutex_t 	ebus_lock;
static uint32_t        	ebus_busy;
static struct ulh_timer ebus_free;
static modus_t			station_mode = SECONDARY;

static uint8_t			addresses_used[256];
static uint8_t			pending_slave_station;

#define TASK_VOID				0x1
#define TASK_DISC				0x2
#define TASK_MASK				0x3
#define CONN_MASK				(~(TASK_MASK))

#define TASK_Q_LENGTH			16

static uintptr_t task_queue[TASK_Q_LENGTH];
static uint32_t queue_entry;
static uint32_t dequeue_entry;


/*******************************************************************************
**  code
*******************************************************************************/

static void lock_bus(struct conn_object *co)
{
	if ((co->modus == PRIMARY) && !ebus_busy) {
		ECB_DEBUG("+++ %s LOCKING BUS +++", co->name);
		ebus_busy = 1;
		pthread_mutex_lock(&ebus_lock);
		ulh_timer_arm(&ebus_free, co->tqueue, co->eb_time);
		pending_slave_station = co->address;
	}
}

static void unlock_bus(struct conn_object *co)
{
	if ((co->modus == PRIMARY) && ebus_busy) {
		ECB_DEBUG("--- %s UNLOCKING BUS ---", co->name);
		ebus_busy = 0;
		pthread_mutex_unlock(&ebus_lock);
		ulh_timer_cancel(&ebus_free);
	}
}

static void tmo_ms(struct timespec* ts, uint32_t tmo)
{
    long int nsec;
    clock_gettime(CLOCK_MONOTONIC, ts);
    ts->tv_sec += (tmo/1000);
	tmo %= 1000;

    nsec = ts->tv_nsec + (tmo*1000000);
    ts->tv_sec += (nsec/1000000000);
    ts->tv_nsec =  nsec%1000000000;
}

static int time_has_expired(struct timespec* ts)
{
    struct timespec tm;
    clock_gettime(CLOCK_MONOTONIC, &tm);

    if (tm.tv_sec == ts->tv_sec)
        return (tm.tv_nsec >= ts->tv_nsec) ? 1 : 0;

    return (tm.tv_sec > ts->tv_sec) ? 1 : 0;
}

static void release_tbuff(struct conn_object *co, struct ulh_tbuff *fb)
{
	if (fb) {
		ulh_tbuff_free(fb);
		ulh_tbuff_pool_put(&co->tb_pool, fb);
	}
}

int _ulh_transmit(struct conn_object *co, uint8_t ctrl, void *buf, int len)
{
	int ret;
	lock_bus(co);

	ret = ecb_transmit(NULL, co->address, ctrl, buf, len);
	if (ret) {
		ECB_ERROR("ecb_transmit, %d", ret);
	}
	return ret;
}

static void send_ctrl(struct conn_object *co, uint8_t ctrl, uint8_t *info)
{
	int len = info ? 3 : 0;
	_ulh_transmit(co, ctrl, info, len);
}

static int send_iframe(struct conn_object *co, struct ulh_tbuff *new)
{
    uint8_t ctrl = (HDLC_I | HDLC_PF | HDLC_VS_VR(co->vs, co->vr));

    if (new) {
        co->tx_frags[co->vs]  = new;
        co->vs = (co->vs + 1) & HDLC_WINDOW_MASK;
        tmo_ms(&co->tx_times[co->vs], HDLC_RESPONSE_TMO);
        co->tx_pendin++;
    } else {
        new  = co->tx_frags[co->va];
        tmo_ms(&co->tx_times[co->va], HDLC_RESPONSE_TMO);
        ctrl = (HDLC_I | HDLC_PF | HDLC_VS_VR(co->va, co->vr));
    }

	if (_ulh_transmit(co, ctrl, new->data, new->size))
        return 0;

    return 1;
}

static int tx_iframe(struct conn_object *co)
{
	struct ulh_tbuff *skb;

    if (co->state != NRM)
        return 0;

    if (co->tx_pendin && (co->modus == SECONDARY)) {
		ECB_TRACE("[%s] Secondary resending buffer", co->name);
		return send_iframe(co, NULL);
	}

    if (co->tx_pendin && time_has_expired(&co->tx_times[co->va])) {
        if (co->tx_resend++ < HDLC_MAX_RESENDS) {
            ECB_TRACE("[%s] Resending buffer due to TMO", co->name);
            return send_iframe(co, NULL);
        }
        else
            __disconnect(co,CNG,DISC_ACK_TMO,1,__LINE__);

    } else if (co->tx_pendin < co->tx_window) {
        if ((skb = ulh_tbuff_dequeue(&co->tx_queue))) {
            co->tx_queued--;
            if (!send_iframe(co, skb)) {
                ulh_tbuff_queue_head(&co->tx_queue, skb);
                co->tx_queued++;
                return 0;
            }
            return 1;
        }
    }

	return 0;
}

static int update_window(struct conn_object *co, uint8_t ack)
{
    uint8_t mb = co->vs + 1;
    uint8_t me = co->va - 1;
    uint8_t msk, xor = 0xff;

    if (co->va <= co->vs) {
        mb  = co->va;
        me  = co->vs;
        xor = 0;
    }

    msk =  (0xff >> mb) & (0xff << (7 - me));
    if (!((msk ^ xor) & (0x80 >> ack))) {
        return 0;
    }

    for (mb = co->va; mb != ack; mb = (mb + 1) & HDLC_WINDOW_MASK) {
        release_tbuff(co, co->tx_frags[mb]);
        co->tx_frags[mb] = 0;
		co->tx_resend = 0;
        co->tx_pendin--;
    }

    co->va = mb;
    return 1;
}

static void free_conn_buffers(struct conn_object *co)
{
	struct ulh_tbuff *fb;
    int cnt;

    for (cnt = 0; cnt < 8; cnt++) {

		if (co->tx_frags[cnt]) {
			release_tbuff(co, co->tx_frags[cnt]);
			co->tx_frags[cnt] = 0;
		}

		if (co->rx_frags[cnt]) {
			release_tbuff(co, co->rx_frags[cnt]);
			co->rx_frags[cnt] = 0;
		}
    }

    if (co->rx_current)
		itc_free(&co->rx_current);

	while ((fb = ulh_tbuff_dequeue(&co->tx_queue)))
		release_tbuff(co, fb);
}

static void __connect(struct conn_object *co)
{
    co->state = NRM;
	co->eb_time = EBUS_FREE_CONN_TMO;

    co->vs = co->vr = co->va = 0;
    co->tx_resend = 0;
    co->tx_pendin = 0;
    co->tx_queued = 0;
    co->fcs_error = 0;
    tmo_ms(&co->cn_time, HDLC_CONNECT_TMO);

    co->uc.uc_connected(co->lobj);
}

static void __disconnect(struct conn_object *co, state_t state,
							int reason, int dc, int line)
{
    static char* cause[] = {"NET","RXTMO","TXTMO","IFRM","REJ","SFRM","BUG"};

    if (dc && (co->modus == PRIMARY))
        send_ctrl(co, HDLC_DISC | HDLC_PF, NULL);

    if (reason >= DISC_NET_DOWN) {
        if (reason > DISC_BUG)
            reason = DISC_BUG;

		if (reason >= DISC_NET_DOWN) {
			ECB_ERROR("[%s (%02X)] Disconnect (line:%d) due to %s",
			  co->name, co->address, line, cause[reason - DISC_NET_DOWN]);
		} else {
			ECB_TRACE("[%s (%02X)] Disconnect (line:%d) due to %s",
			  co->name, co->address, line, cause[reason - DISC_NET_DOWN]);
		}
    }

    co->cp = 0;
    co->state = state;
	co->eb_time = EBUS_FREE_DISC_TMO;
    free_conn_buffers(co);

	if (reason != DISC_ORDERED)
		co->uc.uc_disconnected(co->lobj);

    tmo_ms(&co->cn_time, HDLC_CONNECT_TMO);
}

static int rx_iframe(struct conn_object *co, struct ulh_tbuff* fb)
{
    struct frame *frm = (struct frame *)fb->data;
	struct ulh_tbuff* skb;
    uint8_t seq = HDLC_NS(frm->ctrl);
    uint16_t hdr;

    if (((seq + 1) & HDLC_WINDOW_MASK) == co->vr) {
		ECB_TRACE("[%s] Duplicate Iframe (lost ACK ?) (seq: %d,"
				  " expected: %d)", co->name, (int)seq,(int)co->vr);
		return 1;
	}

	if ((skb  = ulh_tbuff_pool_get(&co->tb_pool)) == NULL)
		return 0;

	skb->data = fb->data;
	skb->size = fb->size;
	skb->rbuf = fb->rbuf;

	for (ulh_tbuff_hold(skb),co->rx_frags[seq] = skb; co->rx_frags[co->vr];) {

		skb = co->rx_frags[co->vr];
        co->rx_frags[co->vr] = 0;
        co->vr = (co->vr + 1) & HDLC_WINDOW_MASK;

		frm = (struct frame *)skb->data;
		hdr = ntohs(frm->info.header);

		if (hdr & ECB_FRAG_F_HEADER) {

			if (co->rx_current || (skb->size <= ECB_FRAG_SF_HDRSIZE)) {
				ECB_ERROR("[%s] Ffrag current: %p  len %d", co->name,
							(void*)co->rx_current, skb->size);
				goto eout;
			}
			co->rx_hdr.src  = ntohl(frm->info.first.src_addr);
			co->rx_hdr.dst  = ntohl(frm->info.first.dst_addr);
			co->rx_hdr.size = ntohl(frm->info.first.size);

                        if (co->rx_hdr.size > co->max_itc_msg_size)
                        {
                           ECB_ERROR("[%s] too big message: %d , max allowed %d, dst %d src %d",
                                     co->name,
                                     co->rx_hdr.size,
                                     co->max_itc_msg_size,
                                     co->rx_hdr.dst,
                                     co->rx_hdr.src);
                           goto eout;
                        }

			co->rx_current  = itc_alloc(co->rx_hdr.size, 0);

			if (co->rx_current == NULL)
				goto eout;

			ulh_tbuff_pop(skb, ECB_FRAG_SF_HDRSIZE);

			if (skb->size > co->rx_hdr.size) {
				ECB_ERROR("[%s] Ffrag error (hdr %d frag %d)", co->name,
						co->rx_hdr.size, skb->size);
				itc_free(&co->rx_current);
				goto eout;
			}
			memcpy(co->rx_current, skb->data, skb->size);
			co->rx_offset = skb->size;

		} else if (co->rx_current && (skb->size > ECB_FRAG_IL_HDRSIZE)){

			ulh_tbuff_pop(skb, ECB_FRAG_IL_HDRSIZE);
			if ((co->rx_offset + skb->size) > co->rx_hdr.size) {
				ECB_ERROR("[%s] Mfrag error (hdr %d actual %d)", co->name,
								co->rx_hdr.size, co->rx_offset + skb->size);
				itc_free(&co->rx_current);
				goto eout;
			}
			memcpy((char*)co->rx_current + co->rx_offset, skb->data, skb->size);
			co->rx_offset += skb->size;

		} else {
			ECB_TRACE("[%s] Mfrag current: %p  len %d", co->name,
									(void*)co->rx_current,skb->size);
			goto eout;
		}

		release_tbuff(co, skb);

		if (hdr & ECB_FRAG_L_HEADER) {
			union itc_msg *rx_msg = co->rx_current;
			co->rx_current = NULL;
			co->rx_hdr.size = co->rx_offset;
			co->uc.uc_delivery(co->lobj, &co->rx_hdr, rx_msg);
        }
    }

    return 1;

 eout:

	release_tbuff(co, skb);
	return 0;
}

static void rxs_uframe(struct conn_object *co, uint8_t ctrl)
{
    switch (HDLC_U_FUNCTION(ctrl)) {

        case HDLC_SNRM:
            if (co->state == NRM)
                __disconnect(co, CNG, DISC_PEER_CMD, 0, __LINE__);

            __connect(co);
            break;

        case HDLC_DISC:
            __disconnect(co, CNG, DISC_PEER_CMD, 0, __LINE__);
            break;

        default:
          return;
    }

    send_ctrl(co, HDLC_UA | HDLC_PF, NULL);
}

static void rxp_uframe(struct conn_object *co, uint8_t ctrl)
{
    if (ctrl == (HDLC_UA | HDLC_PF)) {
        if ((co->state == CNG) && co->cp )
            __connect(co);

    } else if (ctrl == (HDLC_DM | HDLC_PF)) {
        if (co->state == NRM)
            __disconnect(co, CNG, DISC_PEER_CMD, 0, __LINE__);

    } else if (ctrl == (HDLC_FRMR | HDLC_PF)) {
        __disconnect(co, CNG, DISC_FRM_REJECT, 0, __LINE__);
    }
}

static void rx_ctrframe(struct conn_object *co, uint8_t ctr)
{
    switch (co->modus) {

    case PRIMARY:

        if (HDLC_U_FORMAT(ctr)) {
            rxp_uframe(co, ctr);
            return;
        }

        if (co->state == NRM) {
            if (HDLC_S_FUNCTION(ctr) == HDLC_RR) {
                update_window(co, HDLC_NR(ctr));
			}
            else
                __disconnect(co, CNG, DISC_BAD_SFRAME, 1, __LINE__);
        }
        break;


    case SECONDARY:

        if (HDLC_U_FORMAT(ctr)) {
            rxs_uframe(co, ctr);
            return;
        }

        if (co->state == NRM) {
            int rj = 1, tx = 0;
            if (HDLC_S_FUNCTION(ctr) == HDLC_RR) {

                if (update_window(co, HDLC_NR(ctr))) {
                    tx = tx_iframe(co);
                    rj = 0;
                }
            }

            if (rj) {
                uint8_t info[3] = {ctr, (co->vr << 5) | (co->vs << 1), 0};
                send_ctrl(co, HDLC_FRMR | HDLC_PF, info);
                __disconnect(co, CNG, DISC_FRM_REJECT, 0, __LINE__);
            } else if (!tx) {
                send_ctrl(co,HDLC_RR|HDLC_PF|HDLC_VR(co->vr),0);
			}

        } else {
			send_ctrl(co, HDLC_DM | HDLC_PF, NULL);
		}
        break;

    default: ECB_ERROR("[%s] BUG @line %d", co->name, __LINE__);
             break;

    }
}

static void purge_run_queue(struct conn_object* co)
{
	int j;

	for (j = 0; j < TASK_Q_LENGTH; j++)
		if ((task_queue[j] & CONN_MASK) == (uintptr_t)co)
			task_queue[j] |= TASK_VOID;
}

static void run_queued_tasks(void)
{
	struct conn_object* co;
	uintptr_t task;

	while (!ebus_busy) {
		task = task_queue[dequeue_entry];
		co = (struct conn_object*)(task & CONN_MASK);

		if (!co)
			return;

		task_queue[dequeue_entry] = 0;
		dequeue_entry = (dequeue_entry + 1) & (TASK_Q_LENGTH - 1);

		if ((task & TASK_MASK) == 0) {
			co->queued = 0;
			do_poll_primary(co);
		} else if ((task & TASK_MASK) == TASK_DISC) {
			send_ctrl(co, HDLC_DISC | HDLC_PF, NULL);
			purge_run_queue(co);
			free(co);
		}
		/* Do nothing in case TASK_VOID is set */
	}
}

static void do_poll_primary(struct conn_object* co)
{
    if (co->state == NDM)
        return;

	ulh_timer_arm(&co->polltmo, co->tqueue, HDLC_POLL_TMO);

    if (co->state == NRM) {
        if (time_has_expired(&co->cn_time)) {
            __disconnect(co, CNG, DISC_CONN_TMO, 1, __LINE__);
            return;
        } else if (!tx_iframe(co))
			send_ctrl(co, HDLC_RR | HDLC_PF | HDLC_VR(co->vr), NULL);

    } else if (time_has_expired(&co->cn_time)) {
        send_ctrl(co, HDLC_SNRM | HDLC_PF, NULL);
        tmo_ms(&co->cn_time, HDLC_CONNECT_TMO);
        co->cp = 1;
    }
}

static void poll_task_primary(void *data)
{
	struct conn_object* co = data;
    if (co->state == NDM)
        return;

	if (co->queued == 0) {
		task_queue[queue_entry] = (uintptr_t)co;
		co->queued = queue_entry + 1;
		queue_entry = (queue_entry + 1) & (TASK_Q_LENGTH - 1);
	}
	run_queued_tasks();
}

static void poll_task_secondary(void *data)
{
	struct conn_object* co = data;

    if (co->state == NDM)
        return;

	ulh_timer_arm(&co->polltmo, co->tqueue, HDLC_POLL_TMO);

    if ((co->state == NRM) && time_has_expired(&co->cn_time))
		__disconnect(co, CNG, DISC_CONN_TMO, 0, __LINE__);
}

static void lost_task(void *data)
{
	if (ebus_busy) {
		ECB_ERROR("Poll bit lost @%02X. Releasing bus after timeout.",
						pending_slave_station);
		ebus_busy = 0;
		pthread_mutex_unlock(&ebus_lock);
		run_queued_tasks();
	}
}

static void schedule_disconnect(struct conn_object *co)
{
	int j;

	if (co->modus != PRIMARY)
		return;

	if (co->state != NRM)
		return;

	if (!ebus_busy) {
		send_ctrl(co, HDLC_DISC | HDLC_PF, NULL);
		return;
	}

	for (j = 0; j < TASK_Q_LENGTH; j++)
		if ((task_queue[j] & CONN_MASK) == (uintptr_t)co) {
			task_queue[j] |= TASK_DISC;
			return;
		}

	task_queue[queue_entry] = (uintptr_t)co | TASK_DISC;
	co->queued = queue_entry + 1;
	queue_entry = (queue_entry + 1) & (TASK_Q_LENGTH - 1);
}

static int dc_init(void *cob, struct ulh_cm_uc_ops *uc, void *lo, uint32_t pri)
{
    struct conn_object *co = (struct conn_object*)cob;

	(void)pri;

    co->state   = NDM;
    co->lobj    = lo;
    co->uc      = *uc;

    return 0;
}

static int dc_finalize(void *cobj, uint32_t prio)
{
	return 0;
}

static int dc_connect(void *cobj, uint32_t prio)
{
    struct conn_object *co  = (struct conn_object *)cobj;
	int ret;

    co->state = CNG;
    clock_gettime(CLOCK_MONOTONIC, &co->cn_time);
	ulh_timer_arm(&co->polltmo, co->tqueue, 0);

	ret = ulh_trans_attach(co->cid, co->mbox, co->uref);
	if (ret && (ret != -EALREADY))
		syslog(LOG_ERR, "ulh_trans_attach failed, %d", ret);

	return 0;
}

static int dc_disconnect(void *cobj, uint32_t prio)
{
    struct conn_object *co  = (struct conn_object *)cobj;

	schedule_disconnect(co);
    __disconnect(co, NDM, DISC_ORDERED, 0, __LINE__);
	ulh_timer_cancel(&co->polltmo);
	ulh_trans_detach(co->cid, co->mbox, co->uref);
    return 0;
}


#define MIN(a,b) ((uint32_t)a < (uint32_t)b ? a : b)

static int dc_transmit(void *cobj, uint32_t prio, struct ulh_cm_msghdr *head,
                       union itc_msg *msg)
{
    struct conn_object *co = cobj;
	struct ulh_tbuff *skb;
	union fraghdr  *hdr;
    char *src = (char*)msg;
    uint32_t msg_size = head->size;
    uint32_t cpy_size = MIN(msg_size, (co->tx_mtu - ECB_FRAG_SF_SIZE));

    if (co->state != NRM)
		return -ENOTCONN;

    if (co->tx_queued >= co->tx_mqueue) {
        ECB_TRACE("[%s] Tx Discard", co->name);
        return -EBUSY;
    }

	skb = ulh_tbuff_pool_get(&co->tb_pool);
	if (!skb)
		return -ENOMEM;

	if (ulh_tbuff_alloc(co->cid, co->tx_mtu, skb))
		return -ENOMEM;

	hdr = (union fraghdr*)skb->data;
    hdr->first.header   = htons(ECB_FRAG_F_HEADER);
    hdr->first.src_addr = htonl(head->src);
    hdr->first.dst_addr = htonl(head->dst);
    hdr->first.size     = htonl(head->size);

	memcpy(hdr->first.data, src, cpy_size);
	skb->size = cpy_size + ECB_FRAG_SF_SIZE;
	msg_size -= cpy_size;
	src += cpy_size;

    while (msg_size) {

		ulh_tbuff_queue(&co->tx_queue, skb);
		co->tx_queued++;

		skb = ulh_tbuff_pool_get(&co->tb_pool);
		if (!skb)
			return -ENOMEM;

		if (ulh_tbuff_alloc(co->cid, co->tx_mtu, skb))
			return -ENOMEM;

		hdr = (union fraghdr*)skb->data;
		hdr->header = 0;

		cpy_size = MIN(msg_size, (co->tx_mtu - ECB_FRAG_IL_SIZE));

		memcpy(hdr->inter.data, src, cpy_size);
		skb->size = cpy_size + ECB_FRAG_IL_SIZE;
		msg_size -= cpy_size;
		src += cpy_size;
    }

	hdr->header |= htons(ECB_FRAG_L_HEADER);
	ulh_tbuff_queue(&co->tx_queue, skb);
	co->tx_queued++;
    return 0;
}


static void dc_receive(void *cobj, uint32_t cid, struct ulh_tbuff *skb)
{
    struct conn_object* co = cobj;
    uint8_t ctr;
    int wndok;

	hdlc_print_header(co->name, "HDLC RX", skb->data, skb->size);
	unlock_bus(co);

    if (co->state == NDM) {
		ulh_tbuff_free(skb);
		if (co->modus == SECONDARY) {
			send_ctrl(co, HDLC_DM | HDLC_PF, NULL);
			return;
		}
        goto done;
	}

    ctr = ((struct frame *)skb->data)->ctrl;
    tmo_ms(&co->cn_time, HDLC_CONNECT_TMO);

    if (!HDLC_I_FORMAT(ctr)) {
		ulh_tbuff_free(skb);
        rx_ctrframe(co, ctr);
        goto done;
    }

    if (co->state != NRM) {
		ulh_tbuff_free(skb);
        goto done;
    }

	rx_iframe(co, skb);
	ulh_tbuff_free(skb);

    wndok = update_window(co, HDLC_NR(ctr));
    if (co->modus == SECONDARY) {

        if (!wndok) {
            uint8_t info[3] = {ctr, (co->vr << 5) | (co->vs << 1), 0};
            send_ctrl(co, HDLC_FRMR | HDLC_PF, info);
            __disconnect(co, CNG, DISC_FRM_REJECT, 0, __LINE__);
        } else if (!tx_iframe(co)) {
            send_ctrl(co, HDLC_RR | HDLC_PF | HDLC_VR(co->vr), NULL);
		}
		return;
    }

 done:
	if (co->modus == PRIMARY)
		run_queued_tasks();
}


static int ecb_create_instance(void *priv, const char *name,
                               struct ulh_cm_instance *inst,
                               struct ulh_cm_config *config,
                               struct ulh_timerqueue *tqueue)
{
	struct ulh_cm_ecb_config *cfg = (struct ulh_cm_ecb_config*)config;
    struct conn_object *co = malloc(sizeof(*co));

    (void)priv;

    if (co == NULL)
        return -ENOMEM;

    memset(co,0,sizeof(*co));

    co->cid		= cfg->cmn.cid;
    co->uref	= cfg->cmn.uref;
    co->mbox	= cfg->cmn.mbox;
    co->tqueue  = tqueue;
    co->max_itc_msg_size = cfg->cmn.max_itc_msg_size;

	co->eb_time   = EBUS_FREE_DISC_TMO;
    co->address   = (uint8_t)cfg->address;
    co->tx_window = 1;//cfg->window_size;
    co->tx_mqueue = 128;//cfg->defer_queue_size;
    co->tx_mtu    = ulh_trans_getmtu(co->cid);
    co->modus     = PRIMARY;
    station_mode  = PRIMARY;

	if (cfg->station == ECB_STATION_SECONDARY) {
		co->modus = SECONDARY;
		station_mode  = SECONDARY;
		addresses_used[co->address] = 1;
	}

	ulh_tbuff_pool_init(&co->tb_pool, 256);
	ulh_tbuff_queue_init(&co->tx_queue);

	if (co->modus == PRIMARY)
		ulh_timer_init(&co->polltmo, poll_task_primary, co);
	else
		ulh_timer_init(&co->polltmo, poll_task_secondary, co);

	inst->instance = co;
	inst->ops = &ecb_cm_ops;
    strncpy(co->name, name, sizeof(co->name));
	return 0;
}

static int ecb_destroy_instance(void *priv, struct ulh_cm_instance *inst)
{
    struct conn_object *co = (struct conn_object*)inst->instance;

    (void)priv;

	ulh_tbuff_pool_free(&co->tb_pool);
    free_conn_buffers(co);

	if (co->queued == 0)
		free(co);

    return 0;
}

static void ecb_destroy(void *priv)
{
	(void)priv;
}

int ulh_ecb_init(const char *name)
{
	ulh_timer_init(&ebus_free, lost_task, NULL);
	return ulh_cm_register(name, &ecb_ops, 0);
}

int check_for_dm(uint8_t addr)
{
	if ((station_mode == SECONDARY) && addresses_used[addr])
		return 1;

	return 0;
}

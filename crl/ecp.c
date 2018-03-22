/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/time.h>
#include <arpa/inet.h> /* htons/ntohl */
#include <pthread.h>
#include <string.h>
#include <uio_helper.h>
#include <ulh_timer.h>
#include <ulh_transport.h>
#include <ecp.h>
#include <libecp.h>
#include <libecp_internal.h>
#include "log.h"

/*******************************************************************************
**  #defines
*******************************************************************************/
#define ECP_MTU                                 100
#define ECP_MRU                                 400

#define ECP_BP_ING_MSG(_b)                      (0x00 + (_b) * 0xc)
#define ECP_BP_ING_CMD(_b)                      (0x04 + (_b) * 0xc)
#define ECP_BP_ING_STAT(_b)                     (0x08 + (_b) * 0xc)
#define ECP_BP_EGR_MSG(_b)                      (0x18 + (_b) * 0xc)
#define ECP_BP_EGR_CMD(_b)                      (0x1c + (_b) * 0xc)
#define ECP_BP_EGR_STAT(_b)                     (0x20 + (_b) * 0xc)

#define ECP_IRQ_STATUS                          (0x0e00)
#define ECP_ISR_PKT_AVAIL_MASK(_prio)           (1 << ((_prio) + 2))
#define ECP_ISR_SPACE_AVAIL_MASK(_prio)         (1 << (_prio))

#define ECP_BP_ING_CMD_IRQ_EN                   0x00000008
#define ECP_BP_ING_CMD_COMP_PKT                 0x00000001
#define ECP_BP_ING_STAT_SPC_AV                  0x00000001
#define ECP_BP_ING_CMD_VALID_BYTES_SET(__vb)    (((__vb) & 0x3) << 1)

#define ECP_BP_EGR_CMD_IRQ_EN                   0x00000002
#define ECP_BP_EGR_CMD_RELEASE                  0x00000001
#define ECP_BP_EGR_STAT_PKT_AVAIL               0x00000001

#define ECP_BP_EGR_STAT_CRC_ERR                 0x00080000
#define ECP_BP_EGR_STAT_LEN_ERR                 0x00040000
#define ECP_BP_EGR_STAT_L1_ERR                  0x00020000
#define ECP_BP_EGR_STAT_HW_ERR                  0x00008000
#define ECP_BP_EGR_STAT_BUFFULL                 0x00004000
#define ECP_BP_EGR_STAT_PKT_ERR \
	(ECP_BP_EGR_STAT_CRC_ERR | \
	 ECP_BP_EGR_STAT_LEN_ERR | \
	 ECP_BP_EGR_STAT_L1_ERR |  \
	 ECP_BP_EGR_STAT_HW_ERR |  \
	 ECP_BP_EGR_STAT_BUFFULL)

#define ECP_BP_EGR_STAT_RX_BYTES_SHIFT          2
#define ECP_BP_EGR_STAT_RX_BYTES                0x00003ffc
#define ECP_BP_EGR_STAT_RX_BYTES_GET(__dw) \
	(((__dw) & ECP_BP_EGR_STAT_RX_BYTES) >> ECP_BP_EGR_STAT_RX_BYTES_SHIFT)

#define ECP_MSG_ADDR_MASK                       0x0000003f
#define ECP_MSG_ADDR_SHIFT                      10
#define ECP_MSG_ADDR_GET(__dw) \
	(((__dw) & \
	  (ECP_MSG_ADDR_MASK << ECP_MSG_ADDR_SHIFT)) >> ECP_MSG_ADDR_SHIFT)
#define ECP_MSG_ADDR_SET(__dw) \
	(((__dw) & ECP_MSG_ADDR_MASK) << ECP_MSG_ADDR_SHIFT)

/*******************************************************************************
**  prototypes
*******************************************************************************/
static inline void ecp_net_tx_irq_disable(struct ecp_device *edev,
                uint32_t buff_idx);
static inline void ecp_net_rx_irq_disable(struct ecp_device *edev,
                uint32_t buff_idx);
static inline void ecp_net_rx_irq_enable(struct ecp_device *edev,
                uint32_t buff_idx);
static void ecp_irq_enable(struct ecp_device *edev, uint32_t buff_idx);
static void ecp_irq_disable(struct ecp_device *edev, uint32_t buff_idx);
static void ecp_irq_disable_all(struct ecp_device *edev);
static int  ecp_xmit(void *tref, void *cref, struct ulh_tbuff *tbuff);

struct ulh_tbuff  *ecp_tbuff_alloc(struct ecp_device *edev,
                                   struct ecp_connection *econ, uint32_t size);

/*******************************************************************************
**  locals
*******************************************************************************/
static void ecp_irq_enable(struct ecp_device *edev, uint32_t buff_idx)
{
	ecp_net_tx_irq_disable(edev, buff_idx);
	ecp_net_rx_irq_enable(edev, buff_idx);
}
static void ecp_irq_disable(struct ecp_device *edev, uint32_t buff_idx)
{
	ecp_net_rx_irq_disable(edev, buff_idx);
	ecp_net_tx_irq_disable(edev, buff_idx);
}
static void ecp_irq_disable_all(struct ecp_device *edev)
{
	ecp_irq_disable(edev, 0);
	ecp_irq_disable(edev, 1);
}

static int create_connection(void *tref, uint32_t cid,
                           struct ulh_trans_addr *src,
                           __attribute__((unused)) struct ulh_trans_addr *dst,
                           void **cref)
{
	struct ecp_device *edev = tref;
	struct ecp_connection *econ = NULL;

	if (!edev || !src || !cref)
		return  -EFAULT;

	if (cid >= ECP_CIDS)
		return -EINVAL;

	if (src->data[0] >= ECP_ADDRS)
		return -EINVAL;

	if (src->data[1] >= ECP_MAX_NO_OF_BUFFS)
		return -EINVAL;

	econ = edev->conns[src->data[0]];
	if (econ)
		return -EBUSY;

	econ = malloc(sizeof(struct ecp_connection));
	if (!econ)
		return -ENOMEM;

	edev->conns[src->data[0]] = econ;
	econ->addr = src->data[0];
	econ->buff_index = src->data[1];
	econ->cid = cid;
	econ->tbuff_busy_mask = ~0U;

	*cref = econ;

	return 0;
}
/**
 * Destroy a ecp connection
 */
static int destroy_connection(void *tref, void *cref)
{
	struct ecp_device *edev = tref;
	struct ecp_connection *econ = cref;

	if (!edev || !econ)
		return -EINVAL;

	edev->conns[econ->addr] = NULL;
	free(econ);

	return 0;
}
/**
 * Get the maximum transmition value.
 */
static int ecp_get_tx_mtu(__attribute__((unused)) void *param,
                          __attribute__((unused)) void *conn_ref)
{
	return ECP_MTU;
}
/**
 * Get the maximum receive value.
 */
static int ecp_get_rx_mru(void)
{
	return ECP_MRU;
}
/**
 * Write ECP registers.
 */
static inline void ecp_out(void *base, uint32_t off, uint32_t val)
{
	(*(volatile uint32_t *)((intptr_t)base + off)) = val;
}
/**
 * Read ECP registers.
 */
static inline uint32_t ecp_in(void *base, uint32_t off)
{
	return (*(volatile uint32_t *)((intptr_t)base + off));
}
/**
 * Disable the tx interrupt.
 */
static inline void ecp_net_tx_irq_disable(struct ecp_device *edev,
                uint32_t buff_idx)
{
	uint32_t val;

	pthread_mutex_lock(&edev->mutex);
	val = ecp_in(edev->regs, ECP_BP_ING_CMD(buff_idx));
	val &= ~ECP_BP_ING_CMD_IRQ_EN;
	ecp_out(edev->regs, ECP_BP_ING_CMD(buff_idx), val);
	pthread_mutex_unlock(&edev->mutex);
}

static inline void ecp_net_tx_irq_enable_nolock(struct ecp_device *edev,
                uint32_t buff_idx)
{
	uint32_t val;

	val = ecp_in(edev->regs, ECP_BP_ING_CMD(buff_idx));
	val |= ECP_BP_ING_CMD_IRQ_EN;
	ecp_out(edev->regs, ECP_BP_ING_CMD(buff_idx), val);
}
/**
 * Disable the rx interrupt.
 */
static inline void ecp_net_rx_irq_disable(struct ecp_device *edev,
                uint32_t buff_idx)
{
	uint32_t val;

	pthread_mutex_lock(&edev->mutex);
	val = ecp_in(edev->regs, ECP_BP_EGR_CMD(buff_idx));
	val &= ~ECP_BP_EGR_CMD_IRQ_EN;
	ecp_out(edev->regs, ECP_BP_EGR_CMD(buff_idx), val);
	pthread_mutex_unlock(&edev->mutex);
}
/**
 * Enable the rx interrupt.
 */
static inline void ecp_net_rx_irq_enable(struct ecp_device *edev,
                uint32_t buff_idx)
{
	uint32_t val;

	pthread_mutex_lock(&edev->mutex);
	val = ecp_in(edev->regs, ECP_BP_EGR_CMD(buff_idx));
	val |= ECP_BP_EGR_CMD_IRQ_EN;
	ecp_out(edev->regs, ECP_BP_EGR_CMD(buff_idx), val);
	pthread_mutex_unlock(&edev->mutex);
}
/**
 * Release the RX buffer.
 */
static inline void ecp_release_rx_buffer(struct ecp_device *edev,
                uint32_t buff_idx)
{
	uint32_t reg;

	pthread_mutex_lock(&edev->mutex);
	reg = ecp_in(edev->regs, ECP_BP_EGR_CMD(buff_idx));
	reg |= ECP_BP_EGR_CMD_RELEASE;
	ecp_out(edev->regs, ECP_BP_EGR_CMD(buff_idx), reg);
	pthread_mutex_unlock(&edev->mutex);
}
/**
 * Receive packet from the RX buffer.
 */
static struct ulh_tbuff *ecp_net_rx_pkt(struct ecp_device *edev, uint32_t stat,
                                        uint32_t *cid, uint32_t buff_idx)
{
	size_t size;
	size_t size_pad;
	uint32_t hdr;
	uint32_t reg;
	void *pkt;
	struct ecp_connection *econ = NULL;
	struct ulh_tbuff *tbuff;

	if (unlikely(stat & ECP_BP_EGR_STAT_PKT_ERR))
		goto out_err;

	size = ECP_BP_EGR_STAT_RX_BYTES_GET(stat);
	size -= 1; /* CRC not used */

	hdr = ecp_in(edev->regs, ECP_BP_EGR_MSG(buff_idx));

	econ = edev->conns[ECP_MSG_ADDR_GET(hdr)];
	if (unlikely(!econ)) {
		++edev->stats.data[buff_idx].rx_addr_errors;
		goto out_err;
	}

	if (unlikely(size < 4)) {
		++edev->stats.data[buff_idx].rx_size_errors;
		goto out_err;
	}
	size -= 4;
	size_pad = (size + 3) & ~3;

	tbuff = ecp_tbuff_alloc(edev, econ, size_pad);
	if (unlikely(!tbuff)) {
		++edev->stats.data[buff_idx].rx_alloc_errors;
		goto out_err;
	}

	tbuff->size = size;
	pkt = ulh_tbuff_get(tbuff);
	*cid = econ->cid;

	while (size_pad) {
		size_pad -= 4;
		reg = ecp_in(edev->regs, ECP_BP_EGR_MSG(buff_idx));
		*(uint32_t *)pkt = ntohl(reg);
		pkt = (void *)((uintptr_t)pkt + 4);
	}

	return tbuff;

out_err:

	return NULL;
}
/**
 * Send packet to the tx buffer.
 */
static int ecp_net_tx_pkt(struct ecp_device *edev, struct ulh_tbuff *tbuff,
                          struct ecp_connection *econ)
{
	uint8_t *pkt;
	int word_len;
	size_t size;
	uint32_t plc;
	uint32_t pkt_header;
	uint32_t cmd;

	if (econ->cid >= ECP_CIDS) {
		++edev->stats.data[econ->buff_index].tx_addr_errors;
		return -ENOENT;
	}

	pkt = ulh_tbuff_get(tbuff);
	size = ulh_tbuff_len(tbuff);

	/* write packet header */
	plc = (size ? (size - 1) >> 3 : 0);
	pkt_header = ECP_MSG_ADDR_SET(econ->addr) | plc << 4 | 2 /* msgno */;
	ecp_out(edev->regs, ECP_BP_ING_MSG(econ->buff_index), pkt_header);

	/* write payload */
	word_len = (size ? size % 4 : 4) - 1;
	size_t size_pad = (size + 3) & ~3;
	while (size_pad) {
		uint32_t reg = *(uint32_t *)pkt;
		reg = htonl(reg);
		ecp_out(edev->regs, ECP_BP_ING_MSG(econ->buff_index), reg);
		pkt = (void *)((uintptr_t)pkt + 4);
		size_pad -= 4;
	}

	pthread_mutex_lock(&edev->mutex);
	cmd = ecp_in(edev->regs, ECP_BP_ING_CMD(econ->buff_index));
	cmd &= ECP_BP_ING_CMD_IRQ_EN;
	cmd |= ECP_BP_ING_CMD_VALID_BYTES_SET(word_len) |
	       ECP_BP_ING_CMD_COMP_PKT;
	ecp_out(edev->regs, ECP_BP_ING_CMD(econ->buff_index), cmd);
	pthread_mutex_unlock(&edev->mutex);

	return 0;
}
/**
 * Handle the receive interrupt
 */
static int ecp_receive(struct ecp_device *edev, uint32_t buff_idx)
{
	uint32_t st;

	/* read all packets */
	while ((st = ecp_in(edev->regs, ECP_BP_EGR_STAT(buff_idx))) &
	       ECP_BP_EGR_STAT_PKT_AVAIL) {
		uint32_t cid;
		struct ulh_tbuff *tbuff = ecp_net_rx_pkt(edev, st, &cid, buff_idx);
		if (!tbuff) {
			edev->stats.data[buff_idx].rx_dropped++;
		} else {
			edev->stats.data[buff_idx].rx_packets++;
			edev->stats.data[buff_idx].rx_bytes += ulh_tbuff_len(tbuff);
			ulh_trans_deliver(cid, tbuff);
		}
		ecp_release_rx_buffer(edev, buff_idx);
	}

	ecp_net_rx_irq_enable(edev, buff_idx);

	return 0;
}
/**
 * Interrupt callback function.
 */
static void ecp_int_notifier(void *data)
{
	struct client_data *cd = data;
	struct ecp_device *edev = cd->edev;
	uint32_t buff_idx = 1;

	uint32_t isr = ecp_in(edev->regs, ECP_IRQ_STATUS);

	if (isr & (ECP_ISR_SPACE_AVAIL_MASK(0) | ECP_ISR_PKT_AVAIL_MASK(0))) {
		buff_idx = 0;
	}

	if (isr & (ECP_ISR_SPACE_AVAIL_MASK(1) | ECP_ISR_PKT_AVAIL_MASK(1))) {
		buff_idx = 1;
	}

	if (isr & ECP_ISR_SPACE_AVAIL_MASK(buff_idx)) {
		ecp_net_tx_irq_disable(edev, buff_idx);
		pthread_mutex_lock(&edev->mutex);
		edev->stop_tx_queue = 0;
		pthread_cond_signal(&edev->cond);
		pthread_mutex_unlock(&edev->mutex);
	}
	if (isr & ECP_ISR_PKT_AVAIL_MASK(buff_idx)) {
		ecp_net_rx_irq_disable(edev, buff_idx);
		ecp_receive(edev, buff_idx);
	}

	/* enable UIO top interrupt */
	uio_enable_irq(edev->uio_handle);
}
/**
 * Transmit data from the user.
 */

static int ecp_xmit(void *tref, void *cref, struct ulh_tbuff *tbuff)
{
	struct ecp_device *edev = tref;
	struct ecp_connection *econ = cref;
        uint32_t st;
	int rt = 0;

	pthread_mutex_lock(&edev->mutex);
        st = ecp_in(edev->regs, ECP_BP_ING_STAT(econ->buff_index));
        edev->stop_tx_queue = st & ECP_BP_ING_STAT_SPC_AV ? 0 : 1;
        if (edev->stop_tx_queue) {
                struct timespec ts;

                rt = clock_gettime(CLOCK_MONOTONIC, &ts);
                if (rt != 0) {
                        log_err("clock_gettime() failed with error: %s",
                                strerror(errno));
                        rt = errno;
                } else {
                        ts.tv_sec +=  (ECP_TX_TMO / 1000);
                        ts.tv_nsec += (ECP_TX_TMO % 1000) * 1000000;
                        if (ts.tv_nsec >= 1000000000) {
                                ts.tv_sec++;
                                ts.tv_nsec -= 1000000000;
                        }
                        ecp_net_tx_irq_enable_nolock(edev, econ->buff_index);
                        while (edev->stop_tx_queue && !rt)
                                rt = pthread_cond_timedwait(&edev->cond,
                                                            &edev->mutex,
                                                            &ts);
                }
	}
	pthread_mutex_unlock(&edev->mutex);

	if (rt) {
		edev->stats.data[econ->buff_index].tx_dropped++;
                rt = -rt;
	} else {
		rt = ecp_net_tx_pkt(edev, tbuff, econ);
		if (rt) {
			edev->stats.data[econ->buff_index].tx_dropped++;
		} else {
			edev->stats.data[econ->buff_index].tx_packets++;
			edev->stats.data[econ->buff_index].tx_bytes += ulh_tbuff_len(tbuff);
		}
	}

	ulh_tbuff_free(tbuff);
	ulh_timerqueue_schedule(&edev->tqueue);

	return rt;
}

struct ecp_hw_ops hw_ops = {
	.uio_addr_offset = NULL,
	.irq_enable    = ecp_irq_enable,
	.irq_disable   = ecp_irq_disable_all,
	.get_mtu       = ecp_get_tx_mtu,
	.get_mru       = ecp_get_rx_mru,
	.create_conn   = create_connection,
	.destroy_conn  = destroy_connection,
	.dev_hard_xmit = ecp_xmit,
	.irq_callback  = ecp_int_notifier,

};
/**
 * Register hw dependent function to the LIBECP.
 */
void ecp_init_hw_ops(struct ecp_device *edev)
{
	edev->hw_ops = &hw_ops;
}

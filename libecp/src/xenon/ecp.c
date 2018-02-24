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
#include <errno.h>
#include <arpa/inet.h> /* htons/ntohl */
#include <pthread.h>
#include <sys/time.h>
#include <ulh_transport.h>
#include <stdlib.h>
#include <string.h>
#include <ulh_timer.h>
#include <uio_helper.h>
#include <libecp.h>
#include <ecp.h>
#include <libecp_internal.h>
#include "log.h"
/*******************************************************************************
**  #defines
*******************************************************************************/
#define ECP_MTU                100
#define ECP_MRU                2048

/* ECP IRQ reg content*/
#define ECP_CHN_0              0x00000001
#define ECP_CHN_1              0x00000002
#define ECP_CHN_2              0x00000004
#define ECP_CHN_3              0x00000008
#define ECP_CHN_4              0x00000010

#define ECP_SR_TX_ERR          0x01000000
#define ECP_SR_TX_MEM_EMPTY    0x00200000
#define ECP_SR_TX_MEM_FULL     0x00100000
#define ECP_SR_TX_COMP         0x00010000
#define ECP_SR_RX_LOST         0x00000400
#define ECP_SR_RX_ERR          0x00000200
#define ECP_SR_RX_CRC_ERR      0x00000100
#define ECP_SR_RX_MEM_EMPTY    0x00000020
#define ECP_SR_RX_MEM_FULL     0x00000010
#define ECP_SR_RX_MSG_REC      0x00000001

#define ECP_RX_SR_ADDR_LSB     24
#define ECP_RX_SR_MSG_NR_LSB   20
#define ECP_RX_SR_PCL_LSB      12
#define ECP_RX_SR_LENGTH_LSB   0

#define ECP_TX_CR_BC_LSB       30
#define ECP_TX_CR_ADDR_LSB     24
#define ECP_TX_CR_MSG_NR_LSB   16
#define ECP_TX_CR_LENGTH_LSB   0

#define ECP_RX_SR_LENGTH_MAX   2048

#define ECP_TX_CMD_RESET       0x00000010
#define ECP_TX_CMD_SEND        0x00000001
#define ECP_RX_CMD_RESET       0x00000001

/* ECP HW interface registers. */
#define ECP_SR_RD(ioaddr,ch,chn_offset,value)       \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x00))))
#define ECP_SR_TRAP_RD(ioaddr,ch,chn_offset,value)  \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x04))))
#define ECP_SR_TRAP_WR(ioaddr,ch,chn_offset,value)  \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x04)) = value))
#define ECP_SR_MASK_RD(ioaddr,ch,chn_offset,value)  \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x08))))
#define ECP_SR_MASK_WR(ioaddr,ch,chn_offset,value)  \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x08)) = value))
#define ECP_SR_FORCE_WR(ioaddr,ch,chn_offset,value) \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x0C)) = value)
#define ECP_SR_DB_RD(ioaddr,ch,chn_offset,value)    \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x10))))
#define ECP_SR_TRIG_WR(ioaddr,ch,chn_offset,value)  \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x14)) = value))

#define ECP_RX_SR_RD(ioaddr,ch,chn_offset,value)    \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x28))))
#define ECP_RX_CMD_WR(ioaddr,ch,chn_offset,value)   \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x2C)) = value))
#define ECP_RX_MEM_RD(ioaddr,ch,chn_offset,value)   \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x30))))
#define ECP_TX_CR_WR(ioaddr,ch,chn_offset,value)    \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x34)) = value))
#define ECP_TX_CR_RD(ioaddr,ch,chn_offset,value)   \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x34))))
#define ECP_TX_CMD_WR(ioaddr,ch,chn_offset,value)   \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x38)) = value))
#define ECP_TX_MEM_WR(ioaddr,ch,chn_offset,value)   \
	((*(volatile uint32_t *)((intptr_t)ioaddr + ((ch*chn_offset) + 0x3C)) = value))

#define ECP_IRQ_RD(ioaddr, hwId, cpri0_offset, value) \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr - (hwId * ECP_DEV_OFFSET) + cpri0_offset + (hwId * ECP_CPRI_OFFSET) + 0x00)))
#define ECP_IRQ_TRAP_RD(ioaddr, hwId, cpri0_offset, value) \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr - (hwId * ECP_DEV_OFFSET) + cpri0_offset + (hwId * ECP_CPRI_OFFSET) + 0x04)))
#define ECP_IRQ_TRAP_WR(ioaddr, hwId, cpri0_offset, value) \
	((*(volatile uint32_t *)((intptr_t)ioaddr - (hwId * ECP_DEV_OFFSET) + cpri0_offset + (hwId * ECP_CPRI_OFFSET) + 0x04)) = value)
#define ECP_IRQ_MASK_RD(ioaddr, hwId, cpri0_offset, value) \
	(value = (*(volatile uint32_t *)((intptr_t)ioaddr - (hwId * ECP_DEV_OFFSET) + cpri0_offset + (hwId * ECP_CPRI_OFFSET) + 0x08)))
#define ECP_IRQ_MASK_WR(ioaddr, hwId, cpri0_offset, value) \
	((*(volatile uint32_t *)((intptr_t)ioaddr - (hwId * ECP_DEV_OFFSET) + cpri0_offset + (hwId * ECP_CPRI_OFFSET) + 0x08)) = value)
#define ECP_IRQ_TRIG_WR(ioaddr, hwId, cpri0_offset, value) \
	((*(volatile uint32_t *)((intptr_t)ioaddr - (hwId * ECP_DEV_OFFSET) + cpri0_offset + (hwId * ECP_CPRI_OFFSET) + 0x14)) = value)

/*******************************************************************************
**  prototypes
*******************************************************************************/
struct ulh_tbuff  *ecp_tbuff_alloc(struct ecp_device *edev,
                                   struct ecp_connection *econ, uint32_t size);

/*******************************************************************************
**  locals
*******************************************************************************/
static int ecp_devs = 0;
static int ecp_chn_offset = 0;
static int ecp_cpri0_offset = 0;
/**
 * For ECPn, need to add n multiplies the offset.
 */
static void ecp_uio_addr_offset(uint32_t *addr, uint32_t dev_no)
{
	*addr += dev_no * ECP_DEV_OFFSET;
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
static void ecp_irq_disable(struct ecp_device *edev)
{
	ECP_IRQ_MASK_WR(edev->regs, edev->hw_port, ecp_cpri0_offset, 0);
}
static int create_connection(void *tref, uint32_t cid,
                           struct ulh_trans_addr *src,
                           __attribute__((unused)) struct ulh_trans_addr *dst,
                           void **cref)
{
	struct ecp_device *edev = tref;
	struct ecp_connection *econ = NULL;

	if (!edev || !src || !cref){
		log_info("ABN:NULL pointer");
		return -EINVAL;
	}

	if (cid >= ECP_CIDS){
		log_info("ABN:invalid cid: 0x%x", cid);
		return -EINVAL;
	}

	if (src->data[1] >= ECP_MAX_NO_OF_BUFFS){
		log_info("ABN:invalid buffer index: 0x%x", src->data[1]);
		return -EINVAL;
	}

	econ = edev->conns[src->data[1]];
	if (econ) {
		log_info("ABN:buffer 0x%x is already used by others", src->data[1]);
		return -EBUSY;
	}

	econ = malloc(sizeof(struct ecp_connection));
	if (!econ) {
		log_err("failed to allocate memory for ECP connection");
		return -ENOMEM;
	}

	edev->conns[src->data[1]] = econ;
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

	edev->conns[econ->buff_index] = NULL;
	free(econ);

	return 0;
}
static void ecp_irq_enable(struct ecp_device *edev, uint32_t ch)
{
	uint32_t value = 0;

	/* Release receive buffer. Not necessary ! */
	ECP_RX_CMD_WR(edev->regs, ch, ecp_chn_offset, ECP_RX_CMD_RESET);

	/* Clear send buffer. Not necessary ! */
	ECP_TX_CMD_WR(edev->regs, ch, ecp_chn_offset, ECP_TX_CMD_RESET);
	/* Clear all interrupts */
	ECP_SR_TRAP_WR(edev->regs, ch, ecp_chn_offset,
		       ECP_SR_TX_MEM_EMPTY |
		       ECP_SR_TX_MEM_FULL  |
		       ECP_SR_RX_MEM_FULL  |
		       ECP_SR_RX_MEM_EMPTY |
		       ECP_SR_TX_COMP |
	               ECP_SR_RX_MSG_REC |
	               ECP_SR_TX_ERR |
	               ECP_SR_RX_LOST |
	               ECP_SR_RX_ERR |
	               ECP_SR_RX_CRC_ERR);
	/* Set level sens (0) or pos edge trigged (1) interrupts. */
	ECP_SR_TRIG_WR(edev->regs, ch, ecp_chn_offset, 0x0);

	/* Enable interrupts */
	ECP_SR_MASK_WR(edev->regs, ch, ecp_chn_offset,
	               ECP_SR_TX_ERR |
	               ECP_SR_TX_COMP |
	               ECP_SR_RX_MSG_REC |
	               ECP_SR_RX_LOST |
	               ECP_SR_RX_ERR |
	               ECP_SR_RX_CRC_ERR);

	ECP_IRQ_TRIG_WR(edev->regs, edev->hw_port, ecp_cpri0_offset, 0x0);

	/* Clear interrupt. */
	ECP_IRQ_TRAP_WR(edev->regs, edev->hw_port, ecp_cpri0_offset, 1 << ch);
	/* Enable Unit */
	ECP_IRQ_MASK_RD(edev->regs, edev->hw_port, ecp_cpri0_offset, value);
	ECP_IRQ_MASK_WR(edev->regs, edev->hw_port, ecp_cpri0_offset, (value | (1 << ch)));


}

/**
 * Send packet to the tx buffer.
 */
static int ecp_net_tx_pkt(struct ecp_device *edev, struct ulh_tbuff *tbuff,
                          struct ecp_connection *econ)
{
	uint32_t ch = 0;
	int offs = 0;
	int word_len = 0;
	int rc = -EFAULT;
	uint8_t  bc    = 0;
	uint8_t  ecp_addr  = 0; /*not used in xenon1.0*/
	uint8_t msg_nr = 1; /* 1; */  /* 1=RE O&M, 2=AP O&M. */
	uint8_t *pkt = NULL;
	uint32_t txCr = 0;
	size_t size = 0;

	if (!edev || !econ || !tbuff){
		log_info("ABN:NULL pointer");
		return -EINVAL;
	}

	ch = econ->buff_index;

	pkt = ulh_tbuff_get(tbuff);
	if (!pkt){
		log_info("ABN:Failed to get packet data");
		return rc;
	}

	size = ulh_tbuff_len(tbuff);

	if(size > ECP_MTU){
		log_info("ABN:Invalid packet size: 0x%x", size);
		return rc;
	}

	ECP_TX_CMD_WR(edev->regs, ch, ecp_chn_offset, ECP_TX_CMD_RESET);

	/* Send configuration. */
	txCr = ( ( (bc & 0x1) << ECP_TX_CR_BC_LSB) |
	         ( (ecp_addr & 0x3f) << ECP_TX_CR_ADDR_LSB) |
	         ( (msg_nr & 0x0f) << ECP_TX_CR_MSG_NR_LSB) |
	         ( (size & 0x0fff) ) );
	ECP_TX_CR_WR(edev->regs, ch, ecp_chn_offset, txCr);

	/* write payload */
	rc = 0;
	offs = 0;
	while (size) {
		uint32_t dw = 0;
		uint8_t *c = (uint8_t *)&dw;
		int i;

		word_len = (size >= sizeof(uint32_t) ? sizeof(uint32_t) : size);

		for (i = 0; i < word_len; i++)
			c[i] = pkt[offs + i];

		/* Write send data. */
		dw = htonl(dw);
		ECP_TX_MEM_WR(edev->regs, ch, ecp_chn_offset, dw);
		size -= word_len;
		offs += word_len;
		word_len--;
	}

	/* Init send. */
	ECP_TX_CMD_WR(edev->regs, ch, ecp_chn_offset, ECP_TX_CMD_SEND);

	return rc;
}
/**
 * Receive packet from the RX buffer.
 */
static struct ulh_tbuff *ecp_net_rx_pkt(struct ecp_device *edev, uint32_t *cid,
                                        uint32_t ch)
{
	size_t size;
	size_t size_pad;
	uint32_t reg;
	void *pkt;
	struct ecp_connection *econ = NULL;
	struct ulh_tbuff *tbuff;
	uint32_t rxSr;

	ECP_RX_SR_RD(edev->regs, ch, ecp_chn_offset, rxSr);

	econ = edev->conns[ch];

	if (unlikely(!econ)) {
		log_err("econ is NULL");
		++edev->stats.data[ch].rx_addr_errors;
		goto out_err;
	}
	size = rxSr & 0x0fff;
	size_pad = (size + 3) & ~3;

	tbuff = ecp_tbuff_alloc(edev, econ, size_pad);
	if (unlikely(!tbuff)) {
		log_err("RX buffer is NULL");
		++edev->stats.data[ch].rx_alloc_errors;
		goto out_err;
	}

	tbuff->size = size;
	pkt = ulh_tbuff_get(tbuff);
	*cid = econ->cid;
	while (size_pad) {
		size_pad -= 4;
		ECP_RX_MEM_RD(edev->regs, ch, ecp_chn_offset, reg);
		*(uint32_t *)pkt = ntohl(reg);
		pkt = (void *)((uintptr_t)pkt + 4);
	}
	return tbuff;
out_err:
	return NULL;
}

/**
 * Handle the receive interrupt
 */
static int ecp_receive(struct ecp_device *edev, uint32_t ch)
{
	uint32_t cid;
	struct ulh_tbuff *tbuff = NULL;

	if (!edev){
		log_err("edev is NULL");
		return -EINVAL;
	}

	tbuff = ecp_net_rx_pkt(edev, &cid, ch);
	if (!tbuff) {
		edev->stats.data[ch].rx_dropped++;
	} else {
		edev->stats.data[ch].rx_packets++;
		edev->stats.data[ch].rx_bytes += ulh_tbuff_len(tbuff);
		ulh_trans_deliver(cid, tbuff);
	}

	return 0;
}

/**
 * Transmit data from the user.
 */
static int ecp_xmit(void *tref, void *cref, struct ulh_tbuff  *tbuff)
{
	struct ecp_device *edev = tref;
	struct ecp_connection *econ = cref;
	int rt = 0;
	uint32_t ch;

	if (!edev || !econ || !tbuff){
		log_info("ABN:NULL pointer");
		return -EINVAL;
	}

	ch = econ->buff_index;

	if (econ->cid >= ECP_CIDS) {
		log_info("ABN:invalid cid: 0x%x", econ->cid);
		++edev->stats.data[ch].tx_addr_errors;
		return -ENOENT;
	}

	pthread_mutex_lock(&edev->mutex);
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
			while (edev->stop_tx_queue && rt == 0) {
				rt = pthread_cond_timedwait(&edev->cond,
				                            &edev->mutex,
				                            &ts);
			}
		}
	}
	pthread_mutex_unlock(&edev->mutex);

	if (rt != 0) {
		edev->stats.data[ch].tx_lost_err_cnt++;
		if(rt == ETIMEDOUT)
		{
			log_err("TX timeout!\r\n");
			pthread_mutex_lock(&edev->mutex);
			edev->stop_tx_queue=0;
			pthread_mutex_unlock(&edev->mutex);
			ECP_TX_CMD_WR(edev->regs, ch,ecp_chn_offset, ECP_TX_CMD_RESET);
		}
                rt = -rt;
	} else {
		pthread_mutex_lock(&edev->mutex);
		edev->stop_tx_queue = 1;
		pthread_mutex_unlock(&edev->mutex);
		rt = ecp_net_tx_pkt(edev, tbuff, econ);
		if (rt) {
			pthread_mutex_lock(&edev->mutex);
			edev->stop_tx_queue = 0;
			pthread_mutex_unlock(&edev->mutex);
			edev->stats.data[ch].tx_dropped++;
		} else {
			edev->stats.data[ch].tx_packets++;
			edev->stats.data[ch].tx_bytes += ulh_tbuff_len(tbuff);
		}
	}

	ulh_tbuff_free(tbuff);
	ulh_timerqueue_schedule(&edev->tqueue);

	return rt;
}

/**
 * Handle all the interrupts
 */
static void ecp_irq_cb(struct ecp_device *edev, uint32_t ch)
{
	uint32_t isr = 0;
	uint32_t rxSr = 0;
	uint32_t length = 0;
	uint32_t length_32b = 0;

	/* Read status. */
	ECP_SR_TRAP_RD(edev->regs, ch, ecp_chn_offset, isr);
	/* Clear any type of interrupt */
	ECP_SR_TRAP_WR(edev->regs, ch, ecp_chn_offset, isr);

	if (isr & ECP_SR_RX_LOST) {
		edev->stats.data[ch].rx_lost_err_cnt++;
	}

	if (isr & ECP_SR_RX_MSG_REC) {
		ECP_RX_SR_RD(edev->regs, ch, ecp_chn_offset, rxSr);
		length = rxSr & 0x0FFF;
		if (length && !(isr & (ECP_SR_RX_ERR | ECP_SR_RX_CRC_ERR))) {
			ecp_receive(edev, ch);
		} else {
			if (rxSr & ECP_SR_RX_ERR) {
				if (length == ECP_MRU) {
					edev->stats.data[ch].rx_length_err_cnt++;
				} else {
					edev->stats.data[ch].rx_packet_err_cnt++;
				}
			}

			if (rxSr & ECP_SR_RX_CRC_ERR) {
				edev->stats.data[ch].rx_crc_err_cnt++;
			}
			/* Calculate number of 32b data. */
			length_32b = (length + 3) >> 2;
			while (length_32b > 0) {
				uint32_t dummy = 0;
				ECP_RX_MEM_RD(edev->regs, ch, ecp_chn_offset, dummy);
				(void) dummy;
				length_32b--;
			}
		}
	} else {
		if (isr & ECP_SR_RX_LOST) {
			ECP_RX_CMD_WR(edev->regs, ch, ecp_chn_offset, ECP_RX_CMD_RESET);
			edev->stats.data[ch].rx_lost_without_rec++;
		}
	}

	/* If TX_ERR? */
	if (isr & ECP_SR_TX_ERR) {
		edev->stats.data[ch].tx_packet_err_cnt++;
	}
	if (isr & ECP_SR_TX_COMP) {
		pthread_mutex_lock(&edev->mutex);
		edev->stop_tx_queue = 0;
		pthread_cond_signal(&edev->cond);
		pthread_mutex_unlock(&edev->mutex);
	}

}

/**
 * Interrupt callback function.
 */
static void ecp_int_notifier(void *data)
{
	struct client_data *cd = data;
	struct ecp_device *edev = cd->edev;
	uint32_t value = 0;
	uint32_t ch = 0;

	ECP_IRQ_TRAP_RD(edev->regs, cd->dev_no, ecp_cpri0_offset, value);

	ch = 31 - __builtin_clz(value);

	if (ch >= ECP_MAX_NO_OF_BUFFS) {

		log_err("Spurious %s interrupt (%u)", edev->name, ch);

	} else {

		ecp_irq_cb(edev, ch);

		/* Clear IRQ interrupt */
		ECP_IRQ_TRAP_WR(edev->regs, cd->dev_no, ecp_cpri0_offset, (1 << ch));
		/* Synchronize return with clear by reading from ECP. */
		ECP_IRQ_TRAP_RD(edev->regs, cd->dev_no, ecp_cpri0_offset, value);

	}

	/* enable UIO top interrupt */
	uio_enable_irq(edev->uio_handle);
}

struct ecp_hw_ops hw_ops = {
	.uio_addr_offset = ecp_uio_addr_offset,
	.irq_enable      = ecp_irq_enable,
	.irq_disable     = ecp_irq_disable,
	.get_mtu         = ecp_get_tx_mtu,
	.get_mru         = ecp_get_rx_mru,
	.create_conn     = create_connection,
	.destroy_conn    = destroy_connection,
	.dev_hard_xmit   = ecp_xmit,
	.irq_callback    = ecp_int_notifier,
};
/**
 * Register hw dependent function to the LIBECP.
 */
void ecp_init_hw_ops(struct ecp_device *edev)
{
	edev->hw_ops = &hw_ops;
	
        /*Init local variables related to board.*/
	if (strcmp("TRXM", getenv("SYS_BOARD_TYPE")) == 0)
	{
		ecp_devs = 1;
	}
	else if (strcmp("BP", getenv("SYS_BOARD_TYPE")) == 0)
	{
		ecp_devs = 7;
	}

	ecp_chn_offset = ecp_devs * ECP_DEV_OFFSET;
	ecp_cpri0_offset = ECP_CIDS * ecp_chn_offset;
	printf("ecp_devs %d, ecp_chn_offset 0x%x, ecp_cpri0_offset 0x%x\n", ecp_devs, ecp_chn_offset, ecp_cpri0_offset);

}

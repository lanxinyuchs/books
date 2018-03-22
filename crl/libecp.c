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
#include <string.h>
#include <arpa/inet.h> /* htons/ntohl */
#include <pthread.h>
#include <uio_helper.h>
#include <ulh_timer.h>
#include <ulh_transport.h>
#include <ecp.h>
#include <libecp.h>
#include "libecp_internal.h"
#include "log.h"

/* This is a workaround for the previously somewhat weak ulh_ref API!
 * Once old versions of ulh_hold_ref() can be completely excluded from builds
 * this workaround can be removed. */
#ifndef HAVE_ULH_HOLD_REF_RET
#define ulh_hold_ref ecp_ulh_hold_ref
static inline int ecp_ulh_hold_ref(struct ulh_ref *ref)
{
        return __chgref(ref, 1);
}
#endif

/*******************************************************************************
**  #defines
*******************************************************************************/
#define ECP_GC_TIMER             5000 /* ms */
#define ECP_GC_MAX_VISITS        12   /* 12 * ECP_GC_TIMER */

/**
 * Garbage collection data
 */
struct gc_data {
	struct ecp_device *edev;
	struct ulh_tbuff_rbuf *rbuf;
	struct ulh_timer timer;
	int visits;
};

/*******************************************************************************
**  prototypes
*******************************************************************************/
static int  ecp_init_uio(struct ecp_device *edev, uint32_t dev_no);
static void ecp_up(struct ulh_ref *ref, uint32_t buff_idx);
static void ecp_down(struct ulh_ref *ref);
static int  ecp_enable(void *tref, void *cref);
static int  ecp_disable(void *tref, void *cref);
static int  ecp_transmit(void *tref, void *cref, struct ulh_tbuff  *tbuff);

struct ulh_tbuff  *ecp_tbuff_alloc(struct ecp_device *edev,
                                   struct ecp_connection *econ, uint32_t size);

/*******************************************************************************
**  locals
*******************************************************************************/
static inline int is_invalid_handle(void *handle)
{
	if (!handle)
		return -EINVAL;
	return 0;
}
/**
 * Get the size of the tbuff
 */
static int ecp_tbuff_size(struct ecp_device *edev)
{
	if(edev->hw_ops->get_mru)
		return (sizeof(struct ulh_tbuff_rbuf) +                 \
		        sizeof(struct ecp_tbuff_ref) + edev->hw_ops->get_mru() + 4);
	else
		return 0;
}
/**
 * To be called when the ecp is enabled.
 */
static void ecp_up(struct ulh_ref *ref, uint32_t buff_idx)
{
	struct ecp_device *edev = container_of(ref, struct ecp_device, ref);

	uio_enable_irq(edev->uio_handle);

	if(edev->hw_ops->irq_enable)
		edev->hw_ops->irq_enable(edev, buff_idx);
}

static void ecp_down(struct ulh_ref *ref)
{
	struct ecp_device *edev = container_of(ref, struct ecp_device, ref);

	if(edev->hw_ops->irq_disable)
		edev->hw_ops->irq_disable(edev);

	/* Release a possible thread blocked in TX */
	pthread_mutex_lock(&edev->mutex);
	edev->stop_tx_queue = 0;
	pthread_cond_signal(&edev->cond);
	pthread_mutex_unlock(&edev->mutex);

	uio_disable_irq(edev->uio_handle);
}
/**
 * Free memory function.
 */
static void ecp_tbuff_free(struct ulh_ref *ref)
{
	struct ecp_tbuff_ref *eref;
	struct ulh_tbuff_rbuf *rbuf = container_of(ref,
	                              struct ulh_tbuff_rbuf, ref);

	/* Release buffer */
	eref = (struct ecp_tbuff_ref *)((uintptr_t)rbuf -
	                                sizeof(struct ecp_tbuff_ref));
	if (likely(eref->tbuff_no_mask > 1))
		__sync_fetch_and_and(&eref->econ->tbuff_busy_mask,
		                     ~eref->tbuff_no_mask);
	eref->tbuff_no_mask = 0;
}
/**
 * Fast allocate memory function.
 */
struct ulh_tbuff *ecp_tbuff_alloc(struct ecp_device *edev,
                                  struct ecp_connection *econ, uint32_t size)
{
	struct ulh_tbuff_rbuf *rbuf;
	struct ulh_tbuff *tbuff;
	struct ecp_tbuff_ref *eref;
	uint32_t tbuff_busy_mask = ~econ->tbuff_busy_mask;
	int tbuff_no;

	if (unlikely(!tbuff_busy_mask)) {
		/* fall-back to default (slow) allocation */
		tbuff = &econ->tbuff[0];
		if (ulh_tbuff_alloc(econ->cid, size, tbuff)){
			log_err("Failed to allocate RX buffer");
			return NULL;
		}
		++edev->stats.data[econ->buff_index].rx_slow_allocs;
		return tbuff;
	}

	tbuff_no = __builtin_ctz(tbuff_busy_mask);
	rbuf = econ->rbuf[tbuff_no];
	eref = (struct ecp_tbuff_ref *)rbuf;
	rbuf = (struct ulh_tbuff_rbuf *)((uintptr_t)rbuf +
	                                 sizeof(struct ecp_tbuff_ref));
	tbuff = &econ->tbuff[tbuff_no];

	/* Calling ulh_init_ref() here is most likely cheaper than using
	 * ulh_hold_ref() and only call ulh_init_ref() in ecp_enable().
	 * The reason is that ulh_hold_ref() is using atomic increment
	 * which is really not needed in this context since we know
	 * the buffer is available only to current thread. */
	ulh_init_ref(&rbuf->ref, 1, ecp_tbuff_free);
	eref->tbuff_no_mask = (1 << tbuff_no);
	__sync_fetch_and_or(&econ->tbuff_busy_mask, eref->tbuff_no_mask);
	tbuff->data = rbuf->buf;
	tbuff->size = size;

	++edev->stats.data[econ->buff_index].rx_fast_allocs;

	return tbuff;
}
/**
 * Garbage collection function.
 */
static void gc_run(void *data)
{
	struct gc_data *gc = data;
	struct ecp_tbuff_ref *eref = (struct ecp_tbuff_ref *)gc->rbuf;

	fprintf(stderr, "   >>>>> %s <<<<< \n", __func__);

	gc->visits++;
	if (eref->tbuff_no_mask) {
		fprintf(stderr, "tbuff %p is still dangling!\n", (void *)gc->rbuf);
		if (gc->visits < ECP_GC_MAX_VISITS) {
			ulh_timer_arm(&gc->timer, &gc->edev->tqueue,
			              ECP_GC_TIMER);
			return;
		}
		log_info("ABN: garbage collection failed;"
		       " tbuff=%p is permanently lost (leak of %d bytes)!",
		       (void *)eref, ecp_tbuff_size(gc->edev));
		gc->edev->stats.data[0].rx_leak_bytes += ecp_tbuff_size(gc->edev);
		free(gc);
		return;
	}

	fprintf(stderr, "tbuff %p can be freed safely now!\n", (void *)eref);
	free(gc->rbuf);
	free(gc);
}
/**
 * Interrupt callback function.
 */
static void ecp_int_notifier_post(void *data)
{
	struct client_data *cd = data;
	struct ecp_device *edev = cd->edev;

	if (edev->irq_mbox == ITC_NO_ID) {
		char buf[32];
		snprintf(buf, 32, "%s-irq", edev->name);
		edev->irq_mbox = itc_create_mailbox(buf, 0);
	}

	/* Change interrupt handler */
	uio_irq_set_notifier(edev->uio_handle, edev->hw_ops->irq_callback, cd);

	if(edev->hw_ops->irq_callback)
		edev->hw_ops->irq_callback(data);
}

/**
 * Interrupt setup
 */
static int ecp_init_irq(struct client_data *cd,
                        int __attribute__((unused)) dev)
{
	struct ecp_device *edev = cd->edev;

	uio_disable_irq(edev->uio_handle);

	/**
	 * Set the notifier for the interrupts.
	 */
	if (uio_irq_set_notifier(edev->uio_handle, ecp_int_notifier_post, cd)) {
		log_err("unable to set UIO interrupt notifier");
		uio_close(edev->uio_handle);
		return -EFAULT;
	}

	/* start interrupt handler */
	if (uio_bind_irq_rt(edev->uio_handle, 60)) {
		log_err("unable to start UIO interrupt handler");
		uio_close(edev->uio_handle);
		return -EFAULT;
	}

	edev->irq_mbox = ITC_NO_ID;

	if(edev->hw_ops->irq_disable)
		edev->hw_ops->irq_disable(edev);

	return 0;
}
/**
 * Create a ecp connection
 */
static int ecp_create_conn(void *tref, uint32_t cid,
                           struct ulh_trans_addr *src,
                           __attribute__((unused)) struct ulh_trans_addr *dst,
                           void **cref)
{
	struct ecp_device *edev = tref;

	if (!edev)
		return -EINVAL;

	if(edev->hw_ops->create_conn)
		return edev->hw_ops->create_conn(tref, cid, src, dst, cref);
	else
		return -EFAULT;
}
/**
 * Destroy a ecp connection
 */
static int ecp_destroy_conn(void *tref, void *cref)
{
	struct ecp_device *edev = tref;

	if (!edev)
		return -EINVAL;

	if(edev->hw_ops->destroy_conn)
	  return edev->hw_ops->destroy_conn(tref, cref);
	else
	  return -EFAULT;

}

static int ecp_enable(void *tref, void *cref)
{
	struct ecp_connection *econ = cref;
	struct ecp_device *edev = tref;
	int i;

	/* tbuff[0] is reserved for default (slow) tbuff allocation */
	econ->rbuf[0] = NULL;
	for (i = 1; i < ECP_NO_OF_TBUFF; i++) {
		struct ecp_tbuff_ref *eref;
		struct ulh_tbuff_rbuf *rbuf;
		/* Add some extra four (4) bytes to allow for accessing the
		 * buffer using w32 multiples */
		rbuf = malloc(ecp_tbuff_size(edev));
		if (!rbuf){
			log_err("Failed to pre-allocate RX buffer");
			return -ENOMEM;
		}
		econ->rbuf[i] = rbuf;
		eref = (struct ecp_tbuff_ref *)rbuf;
		rbuf = (struct ulh_tbuff_rbuf *)((uintptr_t)rbuf +
		                                 sizeof(struct ecp_tbuff_ref));
		rbuf->buf = (uint8_t *)(rbuf + 1);
		econ->tbuff[i].rbuf = rbuf;
		eref->econ = econ;
		eref->tbuff_no_mask = 0;
	}

	/* make sure unused and reserved buffer(s) are busy marked */
	econ->tbuff_busy_mask =
	        ~((1 << (ECP_NO_OF_TBUFF > 32 ? 32 : ECP_NO_OF_TBUFF)) - 1);
	econ->tbuff_busy_mask |= 1;
	__sync_synchronize();

	if (ulh_hold_ref(&edev->ref) == 1)
		ecp_up(&edev->ref, econ->buff_index);

	return 0;
}

static int ecp_disable(void *tref, void *cref)
{
	struct ecp_connection *econ = cref;
	struct ecp_device *edev = tref;
	int i, tbuff_size;

        if (!ulh_read_ref(&edev->ref))
                return -EINVAL;
	ulh_unhold_ref(&edev->ref);

	tbuff_size = ecp_tbuff_size(edev);

	uint32_t tmp_mask = econ->tbuff_busy_mask;
	econ->tbuff_busy_mask = ~0;
	__sync_synchronize();

	/* tbuff[0] is reserved for default tbuff allocation */
	for (i = 1; i < ECP_NO_OF_TBUFF; i++) {
		if (tmp_mask & (1 << i)) {
			struct gc_data *gc;
			struct ecp_tbuff_ref *eref =
			        (struct ecp_tbuff_ref *)econ->rbuf[i];

			eref->tbuff_no_mask = 1;

			log_info("tbuff_busy_mask=0x%08x;"
			       " starting garbage collection of tbuff=%p",
				 tmp_mask, (void *)eref);
			gc = malloc(sizeof(struct gc_data));
			if (gc) {
				gc->rbuf = econ->rbuf[i];
				gc->edev = edev;
				gc->visits = 0;
				ulh_timer_init(&gc->timer, gc_run, gc);
				ulh_timer_arm(&gc->timer, &edev->tqueue,
				              ECP_GC_TIMER);
			} else {
				log_info("ABN: garbage collection"
				       " could not be started; tbuff=%p is "
				       "permanently lost (leak of %d bytes)!",
					 (void *)eref, tbuff_size);

				edev->stats.data[econ->buff_index].rx_leak_bytes += tbuff_size;
			}
		} else {
			free(econ->rbuf[i]);
		}
		econ->rbuf[i] = NULL;
	}

	return 0;
}

static int ecp_transmit(void *tref, void *cref, struct ulh_tbuff  *tbuff)
{
	struct ecp_device *edev = tref;
	struct ecp_connection *econ = cref;

	if (!edev || !econ || !tbuff)
		return -EINVAL;

	if(edev->hw_ops->dev_hard_xmit)
		return edev->hw_ops->dev_hard_xmit(edev, econ, tbuff);
	else
		return -EINVAL;
}

static int ecp_get_mtu(void *param,
                       __attribute__((unused)) void *conn_ref)
{
	struct ecp_device *edev = param;

	if (!edev)
		return -EINVAL;

	if(edev->hw_ops->get_mtu)
		return edev->hw_ops->get_mtu();
	else
		return 0;/*which is the proper value*/
}

static struct ulh_trans_ops ecp_ops = {
	.create_conn = ecp_create_conn,
	.destroy_conn = ecp_destroy_conn,
	.enable = ecp_enable,
	.disable = ecp_disable,
	.transmit = ecp_transmit,
	.getmtu = ecp_get_mtu,
	.alloc_buff = NULL,
	.destroy = NULL
};

/**
 * Initialize uio device of ECP.
 */
static int ecp_init_uio(struct ecp_device *edev, uint32_t dev_no)
{
	/**
	 * Try to open the UIO device and mmap.
	 */
	edev->uio_handle = (void *) uio_open(edev->name);
	if (edev->uio_handle == (UIO_HANDLE_) - 1) {
		log_err("failed to open UIO device %s", edev->name);
		return -1;
	}

	edev->regs = uio_mmap(edev->uio_handle);
	if (edev->regs == MAP_FAILED) {
		log_err("unable to peform UIO memory mapping");
		uio_close(edev->uio_handle);
		return -1;
	}

        /* There could be offset due to IWD, 0x400 for 4412. 0x400 for M-MIMO. */
        edev->regs += 0x400;

	if(edev->hw_ops->uio_addr_offset)
		edev->hw_ops->uio_addr_offset((uint32_t *)&edev->regs, dev_no);

	return 0;
}

int ecp_init(void **handle, uint32_t dev)
{
	struct client_data *cd = NULL;
	struct ecp_device *edev = NULL;
        pthread_condattr_t cattr;
	char name[16] = {0};
	int i;

	if (!handle)
		goto init_error;

	cd = (struct client_data *)malloc(sizeof(struct client_data));
	if (cd == NULL) {
		log_err("Failed to allocate memory for client data");
		goto init_error;
	}

	cd->dev_no = dev;

	edev = (struct ecp_device *)malloc(sizeof(struct ecp_device));
	if (edev == NULL) {
		log_err("Failed to allocate memory for ECP device");
		goto init_error;
	}
	cd->edev = edev;

	memset(&edev->stats, 0, sizeof(struct ecp_device_stats));

	pthread_mutex_init(&edev->mutex, NULL);
        pthread_condattr_init(&cattr);
        pthread_condattr_setclock(&cattr, CLOCK_MONOTONIC);
	pthread_cond_init(&edev->cond, &cattr);
        pthread_condattr_destroy(&cattr);

	edev->hw_port = cd->dev_no;

	edev->stop_tx_queue = 0;

	snprintf(name, 16, "ecp%d", dev);
	edev->name = strdup(name);

	ecp_init_hw_ops(edev);

	for (i = 0; i < ECP_CIDS; i++)
		edev->conns[i] = NULL;

	ulh_timerqueue_init(&edev->tqueue);
	ulh_init_ref(&edev->ref, 0, ecp_down);

	if (ecp_init_uio(edev, dev))
		goto init_error;

	if (ecp_init_irq(cd, dev))
		goto init_error;

	if (ulh_trans_register(edev->name, &ecp_ops, edev)) {
		log_err("Failed to register ECP device in transport layer");
		goto init_error;
	}

	*handle = cd;

	return 0;

init_error:
	if (cd)
		free(cd);
	if (edev) {
		if(edev->uio_handle != UIO_OPEN_FAILED)
			uio_close(edev->uio_handle);
		free(edev->name);
		free(edev);
	}
	return -EINVAL;
}

void ecp_shutdown(void **handle)
{
	struct client_data *cd;
	struct ecp_device *edev;
	int i;

	if (!handle || is_invalid_handle(*handle))
		return;
	cd = *handle;
	edev = cd->edev;

	uio_disable_irq(edev->uio_handle);

	if(edev->hw_ops->irq_disable)
		edev->hw_ops->irq_disable(edev);

	uio_close(edev->uio_handle);

	for (i = 0; i < ECP_CIDS; i++)
		if (edev->conns[i])
			free(edev->conns[i]);

	ulh_timerqueue_destroy(&edev->tqueue);
	free(edev->name);
	free(edev);
	free(cd);
	*handle = NULL;
}

int ecp_stats(void *handle, struct ecp_device_stats *stats)
{
	struct client_data *cd;
	struct ecp_device *edev;

	if (is_invalid_handle(handle))
		return -EINVAL;

	cd = handle;
	edev = cd->edev;

	memcpy(stats, &edev->stats, sizeof(struct ecp_device_stats));

	return 0;
}

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/queue.h>
#include <assert.h>
#include <pthread.h>

#include <uio_helper.h>

#include <ulh_cm.h>

#include "ulh-shmem-cm.h"
#include "shmem-cm.h"

#define TRACEPOINT_PROVIDER com_ericsson_ulh_shmem_cm
#include <tpt_create.h>
#include <tpt.h>

#define UIO_MBOX_PREFIX "hwmbox"

struct ulh_conn {

	void                        *shmem_tx_mbox;
	void                        *shmem_tx_virt_start;
	uint32_t                     shmem_tx_size;
	void                        *shmem_rx_mbox;
	void                        *shmem_rx_virt_start;
	uint32_t                     shmem_rx_size;

	struct shmem_cm_fifo         fifo_rx;
	struct shmem_cm_fifo         fifo_tx;

	char                         name[32];

	struct ulh_cm_uc_ops        *uc;
	void                        *uc_data;
	uint32_t                     cid;
	unsigned long                uref;
	itc_mbox_id_t                mbox;

	UIO_HANDLE_                  uio_tx_mbox_handle;
	UIO_HANDLE_                  uio_rx_mbox_handle;
	int                          tx_mbox_nbr;
	int                          rx_mbox_nbr;

	STAILQ_HEAD(listhead, tx_queue_entry) tx_wait_queue;

	struct ulh_timerqueue       *tqueue;
	struct ulh_timer             watchdog;
	struct ulh_timer             watchdog_timeout;

	uint32_t                     watchdog_interval_ms;
	uint32_t                     watchdog_ms;
	bool                         pong;
	itc_mbox_id_t                irq_mbox;
	pthread_mutex_t              irq_mutex;
};

union itc_msg {
	uint32_t msgno;
	struct ulh_transmsg_data data;
};

struct tx_queue_entry {
	uint32_t  size0;
	void     *data0;
	uint32_t  size1;
	void     *data1;
	STAILQ_ENTRY(tx_queue_entry) entry;
};


static int wait_fifo_enqueue(struct ulh_conn *co,
                             const void *data0, uint32_t size0,
                             const void *data1, uint32_t size1)
{
	struct tx_queue_entry *e;

	TPT_TRACE(2, "wait_fifo_enqueue()");

	assert(size0 > 0);

	e = malloc(sizeof(struct tx_queue_entry));
	if (e == NULL) {
		goto err_malloc;
	}

	e->size0 = size0;
	e->data0 = malloc(size0);
	if (e->data0 == NULL) {
		free(e);
		goto err_malloc;
	}
	memcpy(e->data0, data0, size0);

	if (size1 > 0) {
		e->size1 = size1;
		e->data1 = malloc(size1);
		if (e->data1 == NULL) {
			free(e->data0);
			free(e);
			goto err_malloc;
		}
		memcpy(e->data1, data1, size1);
	} else {
		e->size1 = 0;
		e->data1 = NULL;
	}

	STAILQ_INSERT_TAIL(&co->tx_wait_queue, e, entry);

	TPT_INFO("No buffer available, queueing message");
	return 0;

err_malloc:
	TPT_INFO("No buffer available, could not enqueue message");
	return -1;
}

static void wait_fifo_dequeue(struct ulh_conn *co)
{
	struct tx_queue_entry *e = STAILQ_FIRST(&co->tx_wait_queue);
	struct tx_queue_entry *next;

	TPT_TRACE(2, "wait_fifo_dequeue()");

	/* Move messages from wait queue to the queue in shared memory. */
	while (e) {

		next = STAILQ_NEXT(e, entry);

		/* Enqueue message in shared memory. */
		if (shmem_cm_fifo_enqueue(&co->fifo_tx,
		                          e->data0, e->size0,
		                          e->data1, e->size1) != 0) {
			break;
		}

		/* Remove message from wait queue. */
		STAILQ_REMOVE(&co->tx_wait_queue, e, tx_queue_entry, entry);
		free(e->data0);
		if (e->data1) {
			free(e->data1);
		}
		free(e);

		e = next;
	}
}

static int enqueue_message(struct ulh_conn *co,
                           const void *data0, uint32_t size0,
                           const void *data1, uint32_t size1)
{
	int status;

	pthread_mutex_lock(&co->irq_mutex);

	if (STAILQ_FIRST(&co->tx_wait_queue) != NULL) {

		/* If wait fifo is not empty, enqueue this on wait fifo too. */
		status = wait_fifo_enqueue(co, data0, size0, data1, size1);

	} else if (shmem_cm_fifo_enqueue(&co->fifo_tx,
	                                 data0, size0, data1, size1) != 0) {

		/* Out of shared memory, enqueue this on wait fifo. */
		status = wait_fifo_enqueue(co, data0, size0, data1, size1);

	} else {

		status = 0;

	}

	pthread_mutex_unlock(&co->irq_mutex);

	return status;
}

static void cleanup(struct ulh_conn *co)
{
	if (co) {

		ulh_timer_cancel(&co->watchdog);
		ulh_timer_cancel(&co->watchdog_timeout);

		if (co->uio_rx_mbox_handle != (UIO_HANDLE_) -1)
			uio_disable_irq(co->uio_rx_mbox_handle);

		if (co->uio_tx_mbox_handle != (UIO_HANDLE_) -1)
			uio_disable_irq(co->uio_tx_mbox_handle);

		if (co->shmem_rx_virt_start)
			munmap(co->shmem_rx_virt_start, co->shmem_rx_size);

		if (co->shmem_tx_virt_start)
			munmap(co->shmem_tx_virt_start, co->shmem_tx_size);

		if (co->uio_tx_mbox_handle != (UIO_HANDLE_) -1)
			uio_close(co->uio_tx_mbox_handle);

		if (co->uio_rx_mbox_handle != (UIO_HANDLE_) -1)
			uio_close(co->uio_rx_mbox_handle);

		free(co);
	}
}

static void mbox_destroy_cb(void *arg)
{
	struct ulh_conn *co = (struct ulh_conn *) arg;

	TPT_TRACE(2, "mbox_destroy_cb()");

	if (co->irq_mbox != ITC_NO_ID) {
		itc_delete_mailbox(co->irq_mbox);
	}
}

static void mbox_rx_cb(void *arg)
{
	struct ulh_conn *co = (struct ulh_conn *) arg;

	TPT_TRACE(2, "mbox_rx_cb()");

	shmem_cm_fifo_rx(&co->fifo_rx);

	uio_enable_irq(co->uio_rx_mbox_handle);
}

static void mbox_tx_cb(void *arg)
{
	struct ulh_conn *co = (struct ulh_conn *) arg;

	TPT_TRACE(2, "mbox_tx_cb()");

	shmem_cm_fifo_tx(&co->fifo_tx);

	uio_enable_irq(co->uio_tx_mbox_handle);
}

static void mbox_rx_cb_init(void *arg)
{
	struct ulh_conn *co = (struct ulh_conn *) arg;

	TPT_TRACE(2, "mbox_rx_cb_init()");

	/* Create mailbox for subsequent irq callbacks */
	if (co->irq_mbox == ITC_NO_ID) {
		char name[sizeof("rx-mbox-irq") + 2];
		sprintf(name, "rx-mbox-irq%d", co->rx_mbox_nbr);
		co->irq_mbox = itc_create_mailbox(name, 0);
	}

	if (uio_irq_set_notifier(co->uio_rx_mbox_handle, mbox_rx_cb, co) != 0) {
		TPT_ERROR("Failed to set rx notifier");
	}

	mbox_rx_cb(arg);
}

static void watchdog_cb(void *arg)
{
	static const uint32_t  cm_msg_ping = SHMEM_CM_MSG_TYPE_PING;
	struct ulh_conn       *co = (struct ulh_conn *) arg;

	TPT_TRACE(2, "watchdog_cb()");

	if (enqueue_message(co, &cm_msg_ping, sizeof(cm_msg_ping), 0, 0) != 0) {
		co->uc->uc_disconnected(co->uc_data);
	} else {
		ulh_timer_arm(&co->watchdog_timeout, co->tqueue, co->watchdog_ms);
	}
}

static void watchdog_timeout_cb(void *arg)
{
	struct ulh_conn *co = (struct ulh_conn *) arg;

	TPT_TRACE(2, "watchdog_timeout_cb()");

	if (co->pong) {
		uint32_t left = co->watchdog_interval_ms - co->watchdog_ms;
		co->pong = false;
		ulh_timer_arm(&co->watchdog, co->tqueue, left);
	} else {
		co->uc->uc_disconnected(co->uc_data);
		ulh_timer_cancel(&co->watchdog);
		ulh_timer_cancel(&co->watchdog_timeout);
		uio_disable_irq(co->uio_rx_mbox_handle);
	}
}


static void upcall_rx(void *object, uint32_t *data, uint32_t size)
{
	struct ulh_conn    *co = (struct ulh_conn*) object;
	union shmem_cm_msg *cm_msg = (union shmem_cm_msg*) data;
	union itc_msg      *msg;

	TPT_TRACE(2, STR("upcall_rx(0x%08x, %u)", *data, size));

	if (cm_msg->type == SHMEM_CM_MSG_TYPE_PONG) {
		co->pong = true;
	} else {
		msg = itc_alloc(sizeof(struct ulh_transmsg_data), ULH_TRANSMSG_DATA);
		msg->data.data.size = size * sizeof(uint32_t);
		msg->data.data.data = (uint8_t*) malloc(msg->data.data.size);
		msg->data.cid = co->cid;
		msg->data.uref = co->uref;
		if (msg->data.data.data != NULL) {
			memcpy(msg->data.data.data, data, msg->data.data.size);
		}
		itc_send(&msg, co->mbox, ITC_MY_MBOX);
	}
}

static void upcall_tx(void *object, uint32_t *data, uint32_t size)
{
	struct ulh_conn *co = (struct ulh_conn*) object;

	TPT_TRACE(2, "upcall_tx()");

	pthread_mutex_lock(&co->irq_mutex);
	wait_fifo_dequeue(co);
	pthread_mutex_unlock(&co->irq_mutex);
}

static void * map_shmem(uint32_t start, uint32_t size)
{
	static int fd = -1;

	if (fd < 0) {
		fd = open("/dev/mem", O_RDWR | O_SYNC);
		if (fd == -1)
			return NULL;
	}

	void *ptr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, start);
	return ptr == MAP_FAILED ? NULL : ptr;
}

static int cm_dc_init(void *handle, struct ulh_cm_uc_ops *uc,
                      void *uc_data, uint32_t prio)
{
	struct ulh_conn *co = handle;
	TPT_TRACE(2, "cm_dc_init()");

	co->uc = uc;
	co->uc_data = uc_data;
	return 0;
}

static int cm_dc_fini(void *handle, uint32_t prio)
{
	struct ulh_conn *co = handle;
	TPT_TRACE(2, "cm_dc_fini()");

	co->uc = NULL;
	co->uc_data = NULL;
	return 0;
}

static int cm_dc_connect(void *handle, uint32_t prio)
{
	static const struct shmem_cm_msg_conn cm_msg_conn = {
		.type = SHMEM_CM_MSG_TYPE_CONN,
		.version = SHMEM_CM_MSG_VERSION
	};
	struct ulh_conn *co = handle;

	if (!co->uc)
		return -EINVAL;

	TPT_TRACE(2, "cm_dc_connect()");

	uio_enable_irq(co->uio_rx_mbox_handle);
	uio_enable_irq(co->uio_tx_mbox_handle);

	return enqueue_message(co, &cm_msg_conn, sizeof(cm_msg_conn), 0, 0);
}

static int cm_dc_disconnect(void *handle, uint32_t prio)
{
	static const uint32_t cm_msg_disc = SHMEM_CM_MSG_TYPE_DISC;
	struct ulh_conn *co = handle;

	TPT_TRACE(2, "cm_dc_disconnect()");

	/* This link is disconnected,
	   whether the remote side receives this DISC message or not. */
	(void) enqueue_message(co, &cm_msg_disc, sizeof(cm_msg_disc), 0, 0);

	ulh_timer_cancel(&co->watchdog);
	ulh_timer_cancel(&co->watchdog_timeout);

	uio_disable_irq(co->uio_rx_mbox_handle);
	uio_disable_irq(co->uio_tx_mbox_handle);

	return 0;
}

static int cm_dc_transmit(void *handle, uint32_t prio,
                          struct ulh_cm_msghdr *hdr, union itc_msg *msg)
{
	struct shmem_cm_msg_data  cm_msg_data = {
		.type = SHMEM_CM_MSG_TYPE_DATA,
		.src = hdr->src,
		.dst = hdr->dst,
		.size = hdr->size
	};
	struct ulh_conn          *co = (struct ulh_conn *) handle;

	TPT_TRACE(2, "cm_dc_transmit()");

	if (enqueue_message(co,
	                    &cm_msg_data, sizeof(cm_msg_data),
	                    msg, hdr->size) != 0) {
		co->uc->uc_disconnected(co->uc_data);
		ulh_timer_cancel(&co->watchdog);
		ulh_timer_cancel(&co->watchdog_timeout);
	}

	return 0; /* Status isn't checked by caller */
}

static void cm_dc_receive(void *handle, uint32_t cid, struct ulh_tbuff *fb)
{
	struct ulh_conn      *co = (struct ulh_conn *) handle;
	union shmem_cm_msg   *cm_msg = (union shmem_cm_msg*) fb->data;
	union itc_msg        *msg;
	struct ulh_cm_msghdr  hdr;

	/* If a message was lost due to memory shortage, then disconnect. */
	if (cm_msg == NULL) {
		TPT_INFO(STR("No memory for received message of size %u",
		             fb->size));
		co->uc->uc_disconnected(co->uc_data);
		ulh_timer_cancel(&co->watchdog);
		ulh_timer_cancel(&co->watchdog_timeout);
		return;
	}

	TPT_TRACE(2, STR("cm_dc_receive(%u)", cm_msg->type));

	switch (cm_msg->type) {

	case SHMEM_CM_MSG_TYPE_DATA:
		hdr.src = cm_msg->data.src;
		hdr.dst = cm_msg->data.dst;
		hdr.size = cm_msg->data.size;
		msg = itc_alloc(cm_msg->data.size, 0);
		memcpy(msg, cm_msg->data.data, cm_msg->data.size);
		co->uc->uc_delivery(co->uc_data, &hdr, msg);
		break;

	case SHMEM_CM_MSG_TYPE_ACK:
		if (co->watchdog_interval_ms) {
			ulh_timer_arm(&co->watchdog, co->tqueue,
			              co->watchdog_interval_ms);
		}
		co->uc->uc_connected(co->uc_data);
		break;

	case SHMEM_CM_MSG_TYPE_NACK:
		TPT_ERROR(STR("Remote side CM message version is %u differs from %u",
		              cm_msg->nack.version, SHMEM_CM_MSG_VERSION));
		break;

	/* PONG messages are handled in upcall_rx, this is here just in case. */
	case SHMEM_CM_MSG_TYPE_PONG:
		co->pong = true;
		break;

	default:
		TPT_ERROR(STR("Received unknown CM message %u", cm_msg->type));
		break;
	}

	free(cm_msg);
}

static struct ulh_cm_dc_ops cm_dc = {
	.dc_init = cm_dc_init,
	.dc_fini = cm_dc_fini,
	.dc_connect = cm_dc_connect,
	.dc_disconnect = cm_dc_disconnect,
	.dc_transmit = cm_dc_transmit,
	.dc_receive = cm_dc_receive,
};

#define REQ(x, s) if (!(x)) do {  \
	TPT_ERROR("Required '" #x "' to be true, returning: " #s); \
	status = (s); \
	goto exit; \
} while (0)

static int cm_create_instance(void *unused, const char *name,
                              struct ulh_cm_instance *instance,
                              struct ulh_cm_config *cfg,
                              struct ulh_timerqueue *tqueue)
{
	struct ulh_conn *co = NULL;
	struct ulh_shmem_cm_config *cm_cfg = (struct ulh_shmem_cm_config *) cfg;
	char uio_mbox[sizeof(UIO_MBOX_PREFIX) + 2 + 1];
	pthread_mutexattr_t mutex_attrs;
	void *mbox_regs;
	int status = 0;

	REQ(cm_cfg->common.cfg_size == sizeof(struct ulh_shmem_cm_config), -EINVAL);

	co = calloc(1, sizeof(struct ulh_conn));
	REQ(co, -ENOMEM);

	co->cid = cm_cfg->common.cid;
	co->uref = cm_cfg->common.uref;
	co->mbox = cm_cfg->common.mbox;
	co->uio_rx_mbox_handle = (UIO_HANDLE_) -1;
	co->uio_tx_mbox_handle = (UIO_HANDLE_) -1;
	co->irq_mbox = ITC_NO_ID;

	pthread_mutexattr_init(&mutex_attrs);
	pthread_mutexattr_settype(&mutex_attrs, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&co->irq_mutex, &mutex_attrs);

	STAILQ_INIT(&co->tx_wait_queue);

	REQ(cm_cfg->tx_mbox <= 31, -EINVAL);
	REQ(cm_cfg->rx_mbox <= 31, -EINVAL);

	co->tx_mbox_nbr = cm_cfg->tx_mbox;
	co->rx_mbox_nbr = cm_cfg->rx_mbox;

	sprintf(uio_mbox, UIO_MBOX_PREFIX "%d", co->rx_mbox_nbr);
	co->uio_rx_mbox_handle = uio_open(uio_mbox);
	REQ(co->uio_rx_mbox_handle != (UIO_HANDLE_) -1, -EIO);
	uio_disable_irq(co->uio_rx_mbox_handle);

	mbox_regs = uio_mmap(co->uio_rx_mbox_handle);
	REQ(mbox_regs != MAP_FAILED, -EIO);
	co->shmem_rx_mbox = mbox_regs + co->rx_mbox_nbr * sizeof(uint32_t);

	sprintf(uio_mbox, UIO_MBOX_PREFIX "%d", co->tx_mbox_nbr);
	co->uio_tx_mbox_handle = uio_open(uio_mbox);
	REQ(co->uio_tx_mbox_handle != (UIO_HANDLE_) -1, -EIO);
	uio_disable_irq(co->uio_tx_mbox_handle);

	mbox_regs = uio_mmap(co->uio_tx_mbox_handle);
	REQ(mbox_regs != MAP_FAILED, -EIO);
	co->shmem_tx_mbox = mbox_regs + co->tx_mbox_nbr * sizeof(uint32_t);

	co->shmem_tx_virt_start = map_shmem(cm_cfg->tx.start, cm_cfg->tx.size);
	REQ(co->shmem_tx_virt_start, -EIO);
	co->shmem_tx_size = cm_cfg->tx.size;

	co->shmem_rx_virt_start = map_shmem(cm_cfg->rx.start, cm_cfg->rx.size);
	REQ(co->shmem_rx_virt_start, -EIO);
	co->shmem_rx_size = cm_cfg->rx.size;

	/* Configure RX FIFO */
	shmem_cm_fifo_init(&co->fifo_rx,
	                   co, &upcall_rx,
	                   (uint32_t*) co->shmem_rx_mbox,
	                   (uint32_t*) co->shmem_rx_virt_start,
	                   co->shmem_rx_size / 4);

	/* Configure TX FIFO */
	shmem_cm_fifo_init(&co->fifo_tx,
	                   co, &upcall_tx,
	                   (uint32_t*) co->shmem_tx_mbox,
	                   (uint32_t*) co->shmem_tx_virt_start,
	                   co->shmem_tx_size / 4);

	REQ(!uio_irq_set_notifier(co->uio_rx_mbox_handle, mbox_rx_cb_init, co), -EIO);
	REQ(!uio_bind_irq2(co->uio_rx_mbox_handle, NULL, mbox_destroy_cb, co), -EIO);

	REQ(!uio_irq_set_notifier(co->uio_tx_mbox_handle, mbox_tx_cb, co), -EIO);
	REQ(!uio_bind_irq(co->uio_tx_mbox_handle), -EIO);

	co->tqueue = tqueue;

	REQ(cm_cfg->watchdog_interval_ms >= cm_cfg->watchdog_ms, -EINVAL);
	if (cm_cfg->watchdog_interval_ms) {
		REQ(cm_cfg->watchdog_ms, -EINVAL);
		ulh_timer_init(&co->watchdog, watchdog_cb, co);
		ulh_timer_init(&co->watchdog_timeout, watchdog_timeout_cb, co);
	}
	co->watchdog_interval_ms = cm_cfg->watchdog_interval_ms;
	co->watchdog_ms = cm_cfg->watchdog_ms;

	strncpy(co->name, name, sizeof(co->name));
	instance->instance = co;
	instance->ops = &cm_dc;

	TPT_INFO(STR("Created instance with tx mbox %d and rx mbox %d",
	             co->tx_mbox_nbr, co->rx_mbox_nbr));

exit:
	if (status)
		cleanup(co);

	return status;
}

static int cm_destroy_instance(void *unused, struct ulh_cm_instance *instance)
{
	struct ulh_conn *co = instance->instance;

	/* TODO: should we send a disconnect? */

	cleanup(co);
	return 0;
}

static struct ulh_cm_ops cm_ops = {
	.create_instance = cm_create_instance,
	.destroy_instance = cm_destroy_instance,
};

int ulh_shmem_init(const char *name)
{
	return ulh_cm_register(name, &cm_ops, NULL);
}

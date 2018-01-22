#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/epoll.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-rio-cm.h>

#include <itc.h>
#include <ulh_dl_list.h>

#include <ulh_cm.h>
#include <ulh_transport.h>
#include <ulh_rio.h>
#include <ulh_timer.h>


#define __warn(...)                                     \
        {                                               \
	fprintf(stderr, "[%s:%s:%d] ",          \
		__FILE__, __func__, __LINE__);  \
	fprintf(stderr, __VA_ARGS__);           \
	fprintf(stderr, "\n");                  \
        }

#define __debug_print(...)                              \
        {                                               \
	fprintf(stderr, __VA_ARGS__);           \
	fprintf(stderr, "\n");                  \
        }

#define ULH_RIO_WARNINGS
/*#define ULH_RIO_VERBOSE*/

#if defined(ULH_RIO_VERBOSE)
#define DBG(...) __debug_print(__VA_ARGS__)
#else
#define DBG(...)
#endif

#if defined(ULH_RIO_WARNINGS)
#define WARN(...) __warn(__VA_ARGS__)
#else
#define WARN(...)
#endif

#define ptr(p, n) ((__u8 *)(p) + (n))

struct rio_conn {
	__s32                     so;
	__u32                     cid;
	unsigned long             uref;
        itc_mbox_id_t             mbox;
	int                       maint_state;
	int                       uc_state;
	void                     *uc_data;
	struct ulh_cm_uc_ops     *uc;
	struct dl_list            node;
	struct ulh_ref            ref;
	struct rio_adm           *rio;
	struct sriocm_ioctl_dctx *tx_buf;
 	struct sriocm_ioctl_dctx *rx_buf;
	struct sriocm_ioctl_dbg   dbg;
};

#define RIO_MAINT                   0
#define RIO_READER                  1
#define RIO_THREADS                 2

#define RIO_EPOLL_MAX               7 /* 6 * EMCA + 1 itc mbox */

#define NOTIFY_CLOSE                0
#define NOTIFY_SUCCESS              1
#define NOTIFY_DISASTER            -EFAULT

#define RIO_MSG_ACCEPT              0
#define RIO_MSG_REJECT              1
#define UC_CONNECT                  1
#define UC_DISCONNECT               2

#define CREATE_PENDING              0
#define CREATE_DONE                 1
#define DESTROY_DONE                3
#define MAJOR_TROUBLE              -EFAULT

struct rio_adm_threads {
        pthread_mutex_t        wait_lock;
	pthread_cond_t         wait;
	pthread_t              tid;
	itc_mbox_id_t          mbox;
	int                    mbox_fd;
	int                    epoll_fd;
	struct dl_list         epoll_list;
	struct dl_list         conn_list;
	__s32                  notify;
};

struct rio_adm {
	char                   *name;
	struct ulh_ref          ref;
	struct rio_adm_threads  t[RIO_THREADS];
};

struct rio_poll {
	struct dl_list          node;
	int                     fd;
	struct rio_adm_threads *t;
	void                   *data;
	int (*cb)(struct rio_adm_threads *t, __u32 events, void *data);
};

#define ULH_RIO_STOP_MSG                  0xddd00
#define ULH_RIO_CREATE_MSG                0xddd01
#define ULH_RIO_DESTROY_MSG               0xddd02
#define ULH_RIO_NOTIFY_CREATE_MSG         0xddd03
#define ULH_RIO_NOTIFY_DESTROY_MSG        0xddd04

struct rio_create_co {
	__u32                      msgno;
	struct rio_conn           *conn;
	size_t                     arg_sz;
	struct sriocm_ioctl_create arg;
};
struct rio_destroy_co {
	__u32                      msgno;
	struct rio_conn           *conn;
};
struct rio_notify {
	__u32                      msgno;
	struct rio_conn           *conn;
	int                       *notify_done;
        pthread_mutex_t           *lock;
        pthread_cond_t            *wait;
};

union itc_msg {
        uint32_t                   msgno;
	struct rio_create_co       create;
	struct rio_destroy_co      destroy;
	struct rio_notify          notify;
	struct ulh_transmsg_data   transmsg;
};

static int handle_itc_fd(struct rio_adm_threads *t,
			 __u32 events, void *data);

static int handle_conn_fd(struct rio_adm_threads *t,
			  __u32 events, void *data);

static void __release_conn(struct ulh_ref *ref)
{
        struct rio_conn *conn = container_of(ref, struct rio_conn, ref);

	DBG("cid %u", conn->cid);

	if (conn->so)
		close(conn->so);

	if (conn->tx_buf)
		free(conn->tx_buf);
	if (conn->rx_buf)
		free(conn->rx_buf);

	free(conn);
}
static void __rio_free_rbuf(struct ulh_ref *ref)
{
        struct ulh_tbuff_rbuf *rbuf = container_of(ref, struct ulh_tbuff_rbuf, ref);

        free(rbuf);
}

static inline int __conn_get(struct rio_conn *conn)
{
	if (!conn) {
		WARN("Gah! framework misbehaves!!!");
		return EINVAL;
	}
	if (conn->maint_state == CREATE_DONE) {
		ulh_hold_ref(&conn->ref);
		return 0;
	}
	if (conn->maint_state == CREATE_PENDING)
		return EAGAIN;

	if (conn->maint_state == MAJOR_TROUBLE)
		return EFAULT;

	if (conn->maint_state == DESTROY_DONE)
		return EINVAL;

	return EFAULT;
}

static inline void __conn_put(struct rio_conn *conn)
{
	ulh_unhold_ref(&conn->ref);
}
static int add_epoll_cb(int epoll_fd, struct dl_list *epoll_list,
			int fd, void *data,
			int (*cb)(struct rio_adm_threads *t,
				  __u32 events, void *data))
{
	struct rio_poll   *p = malloc(sizeof(*p));
	struct epoll_event event;
	int rc;

	if (!p)
		return -ENOMEM;

	p->fd = fd;
	p->data = data;
	p->cb = cb;
	dl_list_init(&p->node);

	event.events = POLLIN;
	event.data.ptr = p;
	rc = epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd, &event);
	if (rc) {
		rc = -errno;
		free(p);
	}
	dl_list_insert_tail(epoll_list, &p->node);
	return rc;
}
static int thread_init(struct rio_adm_threads *t, const char *mb_name, int id)
{
	DBG("id %d", id);

	t->mbox = itc_create_mailbox(mb_name, 0);
	if (t->mbox == ITC_NO_ID) {
		WARN("itc create mailbox %s fail", mb_name);
		return -EFAULT;
	}
	if (id == RIO_READER) {
		t->mbox_fd = itc_get_fd();
		if (t->mbox_fd == ITC_NO_ID) {
			WARN("itc get fd for mailbox %s fail", mb_name);
			itc_delete_mailbox(t->mbox);
			return -EFAULT;
		}
		t->epoll_fd = epoll_create(RIO_EPOLL_MAX);
		if (t->epoll_fd < 0) {
			WARN("epoll create fail");
			return -EFAULT;
		}
		dl_list_init(&t->epoll_list);
		if (add_epoll_cb(t->epoll_fd, &t->epoll_list, t->mbox_fd, NULL, handle_itc_fd)) {
			WARN("epoll add itc fd fail");
			return -EFAULT;
		}
	} else
		dl_list_init(&t->conn_list);

	return 0;
}

static void stop_thread(struct rio_adm  *rio, int id)
{
	struct rio_adm_threads *t = &rio->t[id];

	if (t->notify == NOTIFY_SUCCESS) {
		union itc_msg   *msg = itc_alloc(sizeof(__u32), ULH_RIO_STOP_MSG);
		struct timespec  ts;
		int              rc = 0;

		DBG("id %d", id);

		itc_send(&msg, t->mbox, ITC_MY_MBOX);

		pthread_mutex_lock(&t->wait_lock);
		clock_gettime(CLOCK_REALTIME, &ts);
		ts.tv_sec += 10;
		while (t->notify && !rc)
			rc = pthread_cond_timedwait(&t->wait, &t->wait_lock, &ts);
		pthread_mutex_unlock(&t->wait_lock);

		if (rc || (t->notify != NOTIFY_CLOSE))
			WARN("rio thread %d ignores stop signal?!", id);

		pthread_cancel(t->tid);
		pthread_join(t->tid, NULL);
	}
}
static void __release_rio(struct ulh_ref *ref)
{
        struct rio_adm *rio = container_of(ref, struct rio_adm, ref);

	DBG("name %s", rio->name);

	stop_thread(rio, RIO_READER);
	stop_thread(rio, RIO_MAINT);
	if (rio->name)
		free(rio->name);
	free(rio);
}

static int start_thread(struct rio_adm  *rio, int id, void *(*foo)(void *))
{
	struct rio_adm_threads *t = &rio->t[id];
	struct timespec         ts;
	int                     rc = 0;

	DBG("id %d", id);

	pthread_mutex_init(&t->wait_lock, NULL);
	pthread_cond_init(&t->wait, NULL);
	if (pthread_create(&t->tid, NULL, foo, rio))
		return -EFAULT;

	pthread_mutex_lock(&t->wait_lock);
	clock_gettime(CLOCK_REALTIME, &ts);
	ts.tv_sec += 10;
	while (!t->notify && !rc)
		rc = pthread_cond_timedwait(&t->wait, &t->wait_lock, &ts);
	pthread_mutex_unlock(&t->wait_lock);

	if (rc || (t->notify != NOTIFY_SUCCESS)) {
		pthread_cancel(t->tid);
		pthread_join(t->tid, NULL);
                return -EFAULT;
        }
	return 0;
}

static void thread_notify(struct rio_adm_threads *t, __s32 result)
{
        pthread_mutex_lock(&t->wait_lock);
        t->notify = result;
        pthread_cond_broadcast(&t->wait);
        pthread_mutex_unlock(&t->wait_lock);
}
static int notify_reader(struct rio_adm *rio, struct rio_conn *conn, __u32 signo)
{
	union itc_msg   *msg = itc_alloc(sizeof(struct rio_notify), signo);
	pthread_mutex_t  lock;
	pthread_cond_t   wait;
	__s32            notify_done = 0;
	struct timespec  ts;
	int              rc = 0;

	pthread_mutex_init(&lock, NULL);
        pthread_cond_init(&wait, NULL);
	msg->notify.lock        = &lock;
	msg->notify.wait        = &wait;
	msg->notify.notify_done = &notify_done;
	msg->notify.conn        = conn;
	itc_send(&msg, rio->t[RIO_READER].mbox, ITC_MY_MBOX);

	pthread_mutex_lock(&lock);
	clock_gettime(CLOCK_REALTIME, &ts);
	ts.tv_sec += 10;

        while (!notify_done && !rc)
		rc = pthread_cond_timedwait(&wait, &lock, &ts);
        pthread_mutex_unlock(&lock);

	if (rc || (notify_done != NOTIFY_SUCCESS)) {
		WARN("notify rio_reader sig %u fail", signo);
		return -EFAULT;
	}
	return 0;
}
static int reader_notify_done(union itc_msg *msg, __s32 result)
{
        pthread_mutex_lock(msg->notify.lock);
        *msg->notify.notify_done = result;
        pthread_cond_broadcast(msg->notify.wait);
        pthread_mutex_unlock(msg->notify.lock);
	itc_free(&msg);
	return (result == NOTIFY_SUCCESS ? 0 : 1);
}

/**
 * Defer to thread:
 * create/destroy RIO CM will block
 */

static void handle_create(struct rio_adm *rio, struct rio_adm_threads *t, union itc_msg *msg)
{
	struct rio_conn            *conn = msg->create.conn;
	struct sriocm_ioctl_create *arg  = &msg->create.arg;
	int                         rc;

	DBG("cid %u", conn->cid);

	rc = ioctl(conn->so, RBS_RIO_CM_CREATE_IOC, arg);
	if (!rc) {
		if (notify_reader(rio, conn, ULH_RIO_NOTIFY_CREATE_MSG))
			conn->maint_state = MAJOR_TROUBLE;
		else
			conn->maint_state = CREATE_DONE;

		dl_list_insert_tail(&t->conn_list, &conn->node);
	} else {
		WARN("rc %d errno %d", rc, -errno);
		conn->maint_state = MAJOR_TROUBLE;
		ulh_unhold_ref(&conn->ref);
	}
	itc_free(&msg);
}

static void handle_destroy(struct rio_adm *rio, struct rio_adm_threads *t, union itc_msg *msg)
{
	struct rio_conn            *conn, *tmp;

	DBG("cid %u", msg->destroy.conn->cid);

	dl_list_foreach_safe(conn, tmp, &t->conn_list, node) {
		if (conn == msg->destroy.conn) {
			dl_list_remove(&conn->node);
			conn->maint_state = DESTROY_DONE;
			notify_reader(rio, conn, ULH_RIO_NOTIFY_DESTROY_MSG);
			ioctl(conn->so, RBS_RIO_CM_DESTROY_IOC, NULL);
			ulh_unhold_ref(&conn->ref);
			break;
		}
	}
	itc_free(&msg);
}

static void maint_shutdown(struct rio_adm *rio, struct rio_adm_threads *t)
{
	struct rio_conn            *conn, *tmp;

	stop_thread(rio, RIO_READER);

	dl_list_foreach_safe(conn, tmp, &t->conn_list, node) {

		dl_list_remove(&conn->node);
		ioctl(conn->so, RBS_RIO_CM_DESTROY_IOC, NULL);
		ulh_unhold_ref(&conn->ref);
	}
}
static void *maint_thread(void *priv)
{
	struct rio_adm             *rio = (struct rio_adm *)priv;
	struct rio_adm_threads     *t = &rio->t[RIO_MAINT];

	if (thread_init(t, "rio_adm", RIO_MAINT)) {
		thread_notify(t, NOTIFY_DISASTER);
		pthread_exit(0);
	}
	thread_notify(t, NOTIFY_SUCCESS);

	for (;;) {
		union itc_msg *msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {
		case ULH_RIO_CREATE_MSG:
			handle_create(rio, t, msg);
			break;
		case ULH_RIO_DESTROY_MSG:
			handle_destroy(rio, t, msg);
			break;
		case ULH_RIO_STOP_MSG:
			DBG("ULH_RIO_STOP_MSG");

			goto exit;
			break;
		default:
			WARN("received unexpected itc msg no %u", msg->msgno);
			itc_free(&msg);
		}
	}
exit:
	maint_shutdown(rio, t);
	thread_notify(t, NOTIFY_CLOSE);
	itc_delete_mailbox(t->mbox);
	pthread_exit(0);
}
/**
 * recv on all RIO CM sockets
 * also deferred to thread, but different thread
 */
static int add_reader_conn(struct rio_adm_threads *t, union itc_msg *msg)
{
	struct rio_conn *conn = msg->notify.conn;
	int              rc = NOTIFY_SUCCESS;

	DBG("cid %u", conn->cid);

	ulh_hold_ref(&conn->ref);
	if (add_epoll_cb(t->epoll_fd, &t->epoll_list, conn->so, conn, handle_conn_fd)) {
		ulh_unhold_ref(&conn->ref);
		WARN("epoll add conn fd fail");
		rc = NOTIFY_DISASTER;
	}
	return rc;
}
static int remove_reader_conn(struct rio_adm_threads *t, union itc_msg *msg)
{
	struct rio_conn *conn = msg->notify.conn;
	struct rio_poll *p;

	dl_list_foreach(p, &t->epoll_list, node) {
		if (p->data == conn) {
			DBG("cid %u", conn->cid);

			epoll_ctl(t->epoll_fd, EPOLL_CTL_DEL, p->fd, NULL);
			dl_list_remove(&p->node);
			ulh_unhold_ref(&conn->ref);
			free(p);
			return NOTIFY_SUCCESS;
		}
	}
	return NOTIFY_SUCCESS; /* ??? */
}
static int msg_peek(struct rio_conn *conn, int len)
{
	struct cm_sig_adm *sig_adm = &conn->rx_buf->sig_adm;
	int                result;

	if (len < sizeof(*sig_adm) ||
	    (len - sizeof(*sig_adm)) < sig_adm->size) {
		WARN("unexp. msg size %d", len);
		result = RIO_MSG_REJECT;

	} else if (BUF_TYPE_CONNECTED(sig_adm->buffer_type)) {
		conn->uc_state = UC_CONNECT;
		result  = RIO_MSG_ACCEPT;

	} else if (BUF_TYPE_DISCONNECTED(sig_adm->buffer_type)) {

		if (conn->uc_state == UC_CONNECT)
			result  = RIO_MSG_ACCEPT;
		else  /* paranoid */
			result = RIO_MSG_REJECT;

		conn->uc_state = UC_DISCONNECT;

	} else if (BUF_TYPE_UDATA(sig_adm->buffer_type)) {

		if (conn->uc_state == UC_CONNECT)
			result  = RIO_MSG_ACCEPT;
		else
			result = RIO_MSG_REJECT;

	} else {
		WARN("unexp. msg type %u", sig_adm->buffer_type);
		result = RIO_MSG_REJECT;
	}

	return result;
}
static void rio_trans_deliver(struct rio_conn *conn, int len)
{
	struct cm_sig_adm     *sig_adm = &conn->rx_buf->sig_adm;
	struct ulh_tbuff_rbuf *rbuf = malloc(len + sizeof(*rbuf));
	union itc_msg         *msg;

	if (!rbuf) {
		WARN("Out of memory?!");
		return;
	}
	msg = itc_alloc(sizeof(struct ulh_transmsg_data),
			ULH_TRANSMSG_DATA);

	ulh_init_ref(&rbuf->ref, 1, __rio_free_rbuf);
	memcpy(rbuf + 1, conn->rx_buf, len);
	rbuf->buf               = (__u8 *) (rbuf + 1);
	msg->transmsg.data.rbuf = rbuf;
	msg->transmsg.data.size = sig_adm->size;
	msg->transmsg.data.data = rbuf->buf + sizeof(*sig_adm);
	msg->transmsg.cid       = conn->cid;
	msg->transmsg.uref      = conn->uref;

	itc_send(&msg, conn->mbox, ITC_MY_MBOX);
}
static int handle_conn_fd(struct rio_adm_threads *t,
			  __u32 events, void *data)
{
	struct rio_conn *conn = (struct rio_conn *)data;

	if (events & POLLIN) {
		int len = recv(conn->so, conn->rx_buf, ULH_RIO_MTU, MSG_DONTWAIT);

		if (msg_peek(conn, len) == RIO_MSG_ACCEPT)
			rio_trans_deliver(conn, len);
	}
	return 0;
}
static int handle_itc_fd(struct rio_adm_threads *t,
			 __u32 events, void *data)
{
	if (events & POLLIN) {
		union itc_msg     *msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {
		case ULH_RIO_STOP_MSG:
			DBG("ULH_RIO_STOP_MSG");
			return 1;

		case ULH_RIO_NOTIFY_CREATE_MSG:
			return reader_notify_done(msg, add_reader_conn(t, msg));

		case ULH_RIO_NOTIFY_DESTROY_MSG:
			return reader_notify_done(msg, remove_reader_conn(t, msg));

		default:
			WARN("received unexpected itc msg no %u", msg->msgno);
			itc_free(&msg);
		}
	}
	return 0; /* ??? */
}
static void reader_shutdown(struct rio_adm_threads *t)
{
	struct rio_poll   *p, *tmp;

	dl_list_foreach_safe(p, tmp, &t->epoll_list, node) {
		struct rio_conn *conn = (struct rio_conn *)p->data;

		epoll_ctl(t->epoll_fd, EPOLL_CTL_DEL, p->fd, NULL);
		dl_list_remove(&p->node);
		if (conn)
			ulh_unhold_ref(&conn->ref);
		free(p);
	}
}
static void *reader_thread(void *priv)
{
	struct rio_adm             *rio = (struct rio_adm *)priv;
	struct rio_adm_threads     *t = &rio->t[RIO_READER];
	struct epoll_event          events[RIO_EPOLL_MAX];

	if (thread_init(t, "rio_reader", RIO_READER)) {
		thread_notify(t, NOTIFY_DISASTER);
		pthread_exit(0);
	}
	thread_notify(t, NOTIFY_SUCCESS);

	for (;;) {
		int nfds = epoll_wait(t->epoll_fd, events, RIO_EPOLL_MAX, -1);
		int i;

                if (nfds < 0 && errno != EINTR) {
			WARN("epoll wait fail");
                        goto exit;
                }
                for (i = 0; i < nfds; i++) {
			struct rio_poll *p = events[i].data.ptr;

			if ((events[i].events & ~POLLIN) != 0) {
				WARN("epoll events %u", events[i].events);
				goto exit;

			}
			if (p->cb(t, events[i].events, p->data))
				goto exit;
                }
	}
exit:
	reader_shutdown(t);
	thread_notify(t, NOTIFY_CLOSE);
	pthread_exit(0);
}

/**
 * Racy perhaps? But faster than messing with
 * with those nasty futex locks and if I get this
 * correct then all DC functions and create/destroy
 * are serialized in single lnh thread.
 * The worst thing that can happen, provided that
 * lnh sticks to the contract, is that create
 * CM is not finished when lnh calls dc_init()
 * in that case EAGAIN is ret.
 *
 */
static int dc_init(void *data, struct ulh_cm_uc_ops *uc, void *uc_data, __u32 prio)
{
	struct rio_conn *conn = (struct rio_conn *)data;
	int              rc = __conn_get(conn);

	if (rc)
		return rc;

	conn->uc = uc;
	conn->uc_data = uc_data;

	if (conn->dbg.dbg_union.__dbg_bits)
		rc = ioctl(conn->so, RBS_RIO_CM_SET_DBG_IOC, &conn->dbg);
	if (!rc)
		rc = ioctl(conn->so, RBS_RIO_CM_DC_INI_IOC, NULL);

	ulh_unhold_ref(&conn->ref);

	return rc;
}
static int dc_fini(void *data, __u32 prio)
{
	struct rio_conn *conn = (struct rio_conn *)data;
	int              rc = __conn_get(conn);

	if (rc)
		return rc;

	rc = ioctl(conn->so, RBS_RIO_CM_DC_FINI_IOC, NULL);
	conn->uc = NULL;
	conn->uc_data = NULL;
	__conn_put(conn);

	return rc;
}
static int dc_connect(void *data, __u32 prio)
{
	struct rio_conn *conn = (struct rio_conn *)data;
	int              rc = __conn_get(conn);

	if (rc)
		return rc;

	if (!conn->uc) {
		__conn_put(conn);
		return -EINVAL;
	}
	/** connect - up calls possible after this point */
	rc = ioctl(conn->so, RBS_RIO_CM_DC_CONN_IOC, NULL);

	__conn_put(conn);

	return rc;
}
/**
 * dc disc & dc tx must be tolerated between
 * dc connect and dc disc, regardless of
 * uc disc. state.
 * uc connect & uc deliver are tolerated
 * by rlnh until uc disconnect.
 */
static int dc_disconnect(void *data, __u32 prio)
{
	struct rio_conn *conn = (struct rio_conn *)data;
	int              rc = __conn_get(conn);

	if (rc)
		return rc;

	if (!conn->uc) {
		__conn_put(conn);
		return -EINVAL;
	}
	rc = ioctl(conn->so, RBS_RIO_CM_DC_DISC_IOC, NULL);

	__conn_put(conn);

	return rc;
}
static int dc_transmit(void *data, __u32 prio, struct ulh_cm_msghdr *hdr,
		       union itc_msg *msg)
{
	struct rio_conn *conn = (struct rio_conn *)data;
	int              rc = __conn_get(conn);

	if (rc)
		return rc;

	if (!conn->uc) {
		__conn_put(conn);
		return -EINVAL;
	}
	conn->tx_buf->sig_adm.src  = hdr->src;
	conn->tx_buf->sig_adm.dst  = hdr->dst;
	conn->tx_buf->sig_adm.size = hdr->size;
	memcpy(conn->tx_buf->data, msg, hdr->size);
	rc = ioctl(conn->so, RBS_RIO_CM_DC_TX_IOC, conn->tx_buf);
        /* lnh handles itc_free msg */
	__conn_put(conn);

	return rc;

}
static void dc_receive(void *data, __u32 cid, struct ulh_tbuff *tbuf)
{
	struct rio_conn       *conn = (struct rio_conn *)data;
	struct ulh_tbuff_rbuf *rbuf = tbuf->rbuf;
	struct cm_sig_adm     *sig_adm = (struct cm_sig_adm *)rbuf->buf;

	if (__conn_get(conn)) { /* Hu? */
		ulh_tbuff_free(tbuf);
		return;
	}
	if (!conn->uc) {
		__conn_put(conn);
		ulh_tbuff_free(tbuf);
		return;
	}
	if (BUF_TYPE_CONNECTED(sig_adm->buffer_type))
		conn->uc->uc_connected(conn->uc_data);
	else if (BUF_TYPE_DISCONNECTED(sig_adm->buffer_type))
		conn->uc->uc_disconnected(conn->uc_data);
	else if (BUF_TYPE_UDATA(sig_adm->buffer_type)) {
		union itc_msg *msg = itc_alloc(sig_adm->size, 0);

		memcpy(msg, ulh_tbuff_get(tbuf), ulh_tbuff_len(tbuf));
		conn->uc->uc_delivery(conn->uc_data, (struct ulh_cm_msghdr *)sig_adm, msg);
	}
	ulh_tbuff_free(tbuf);
	__conn_put(conn);

}
static struct ulh_cm_dc_ops rio_cm_ops = {
        .dc_init = dc_init,
        .dc_fini = dc_fini,
        .dc_connect = dc_connect,
        .dc_disconnect = dc_disconnect,
        .dc_transmit = dc_transmit,
        .dc_receive = dc_receive,
};

static int rio_create_instance(void *priv, const char *name,
			       struct ulh_cm_instance *instance,
			       struct ulh_cm_config *config,
			       struct ulh_timerqueue *tqueue)
{
	struct rio_adm             *rio = (struct rio_adm *)priv;
	struct ulh_cm_rio_config   *rio_cfg = (struct ulh_cm_rio_config *)config;
	struct rio_conn            *conn;
	union itc_msg              *msg;
	struct sriocm_ioctl_create *arg;
	size_t                     arg_sz;
	__s32                      i, rc;

	DBG("name %s", name);

	if (!rio || !rio_cfg)
		return -EINVAL;

	conn = calloc(1, sizeof(*conn));
	if (!conn) {
		WARN("Out of memory?!");
		return -ENOMEM;
	}
	ulh_init_ref(&conn->ref, 1, __release_conn);
	conn->tx_buf = malloc(sizeof(*conn->tx_buf) + ULH_RIO_MTU);
	conn->rx_buf = malloc(sizeof(*conn->rx_buf) + ULH_RIO_MTU);
	if (!conn->tx_buf || !conn->rx_buf) {
		WARN("Out of memory?!");
		ulh_unhold_ref(&conn->ref);
		return -ENOMEM;
	}
	conn->cid = rio_cfg->cmn.cid;
	conn->mbox = rio_cfg->cmn.mbox;
	conn->uref = rio_cfg->cmn.uref;
	conn->rio = rio;
	conn->maint_state = CREATE_PENDING;
	conn->dbg.dbg_union.__dbg_bits = rio_cfg->dbg.dbg_union.__dbg_bits;

	dl_list_init(&conn->node);
	conn->so = socket(AF_RBS, SOCK_RAW, RBS_PROTO_RIOCM);
	if (conn->so < 0) {
		rc = -errno;
		WARN("RBS_PROTO_RIOCM socket create errno %d", rc);
		ulh_unhold_ref(&conn->ref);
		return rc;
	}
	arg_sz = strlen(name) + 1;
	arg_sz += strlen(rio_cfg->ifname) + 1;

	msg = itc_alloc(sizeof(struct rio_create_co) + arg_sz, ULH_RIO_CREATE_MSG);
	arg = &msg->create.arg;
	msg->create.arg_sz = arg_sz + sizeof(*arg);
	msg->create.conn = conn;
	arg->remote_port = 0; /* fixme remove */
	arg->local_port  = 1;
	arg->peer_id     = rio_cfg->peer_id;
	arg->mbox        = rio_cfg->mbox;
	arg->letter      = rio_cfg->letter;
	arg->channel     = rio_cfg->channel;

	for (i=0; i<6; i++) {
		arg->src_mac[i] = rio_cfg->src_mac[i];
		arg->dst_mac[i] = rio_cfg->dst_mac[i];
	}

	arg->name = sizeof(*arg);
	arg->name_len = strlen(name);
	memcpy(ptr(arg, arg->name), name, arg->name_len + 1);

	arg->dev = arg->name + arg->name_len + 1;
	arg->dev_len = strlen(rio_cfg->ifname);
	memcpy(ptr(arg, arg->dev), rio_cfg->ifname, arg->dev_len + 1);

	ulh_hold_ref(&conn->ref);
	itc_send(&msg, rio->t[RIO_MAINT].mbox, ITC_MY_MBOX);
	instance->instance = conn;
	instance->ops = &rio_cm_ops;
	return 0;
}

static int rio_destroy_instance(void *priv,
				struct ulh_cm_instance *instance)
{
	struct rio_adm             *rio = (struct rio_adm *)priv;
	struct rio_conn            *conn;
	union itc_msg              *msg;

	if (!rio || !instance)
		return -EINVAL;

	conn = (struct rio_conn *)instance->instance;
	msg = itc_alloc(sizeof(struct rio_destroy_co), ULH_RIO_DESTROY_MSG);
	msg->destroy.conn = conn;
	itc_send(&msg, rio->t[RIO_MAINT].mbox, ITC_MY_MBOX);
	ulh_unhold_ref(&conn->ref);
	return 0;
}

static void rio_destroy(void *priv)
{
	struct rio_adm  *rio = (struct rio_adm *)priv;

	ulh_unhold_ref(&rio->ref);
}


static struct ulh_cm_ops rio_ops = {
        .create_instance  = rio_create_instance,
        .destroy_instance = rio_destroy_instance,
	.destroy          = rio_destroy,
};


/* may sleep, ok? */
int ulh_rio_init(const char *name)
{
	struct rio_adm  *rio = calloc(1, sizeof(*rio));

	DBG("name %s", name);

	if (!rio) {
		WARN("Out of memory?!");
		return -ENOMEM;
	}
	ulh_init_ref(&rio->ref, 1, __release_rio);

	rio->name = strdup(name);
	if (!rio->name)
		goto err;

	if (start_thread(rio, RIO_MAINT, maint_thread))
		goto err;

	if (start_thread(rio, RIO_READER, reader_thread))
		goto err;

	DBG("ulh_cm_register %s", name);
        return ulh_cm_register(name, &rio_ops, rio);
err:
	WARN("ulh_cm %s fail to register", name);
	ulh_unhold_ref(&rio->ref);
	return -EFAULT;
}


#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <sys/ioctl.h>
#include <sys/un.h>
#include <signal.h>
#include <poll.h>
#include <linux/types.h>

#include <ulh_dl_list.h>
#include <ulh_ref.h>
#include <ulh_lnh_msg.h>

#include <emca-link-api.h>
#include <riocmd-api.h>

#define EMCA_API_WARNINGS

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

#if defined(EMCA_API_VERBOSE)
#define DBG(...) __debug_print(__VA_ARGS__)
#else
#define DBG(...)
#endif

#if defined(EMCA_API_WARNINGS)
#define WARN(...) __warn(__VA_ARGS__)
#else
#define WARN(...)
#endif

#define EMCAAPI_IF_MAGIC          (void *)(0x19950920)
#define IF_EPOLL_MAX              2

#define NOTIFY_CLOSE                0
#define NOTIFY_SUCCESS              1
#define NOTIFY_DISASTER            -EFAULT

/* FIXME! */
#define INSTANCE_SHUTDOWN_REQ     0xddd07
#define LINK_CREATE_REQ           0xddd08
#define LINK_DESTROY_REQ          0xddd09
/**/
struct user_req {
	__u32                     msgno;
	__u32                     snid;
	__u32                     link_instance;
	itc_mbox_id_t             mbox;
	pthread_mutex_t          *lock;
        pthread_cond_t           *wait;
	int                      *notify;
	__u32                    *lid;
};

struct if_poll {
	struct dl_list            node;
	int                       fd;
	void                     *data;
	int (*cb)(__u32 events, void *data);
};

struct link_object {
	struct ulh_ref            ref;
	struct dl_list            node;
	itc_mbox_id_t             owner_mbox;
	struct emca_link_if      *if_instance;
	union itc_msg            *msg;
	int                       server_seq;
	__u32                     link_state;
	__u32                     lid;
	char                      link_name[ULH_LNHNAMESIZ];
};

struct emca_link_if {
	void                     *magic;
	struct ulh_ref            ref;
	struct dl_list            epoll_list;
	struct dl_list            link_list;
	struct dl_list            req_list;
	pthread_t                 tid;
	pthread_mutex_t           lock;
	pthread_cond_t            wait;
	itc_mbox_id_t             mbox;
	char                      name[64];
	union riocmd_msg          server_msg;
	int                       msg_sz;
	__u32                     seq;
	int                       api_fd;
	int                       mbox_fd;
	int                       epoll_fd;
	int                       notify;
	int                       instance_stop;
};

union itc_msg {
	__u32                  msgno;
	struct user_req        req;
	struct emca_link_event eevent;
};

static int cb_add(struct emca_link_if *if_instance, int fd,
		  int (*cb)(__u32 events, void *data))
{
        struct if_poll       *p = calloc(1, sizeof(*p));
        struct epoll_event    event;
        int                   rc;

        if (!p)
                return -ENOMEM;

        p->fd = fd;
        p->data = if_instance;
        p->cb = cb;
        dl_list_init(&p->node);
        event.events = POLLIN | POLLERR;
        event.data.ptr = p;
        if((rc = epoll_ctl(if_instance->epoll_fd, EPOLL_CTL_ADD, fd, &event))) {
                WARN("epoll_ctrl() failed with errno %d", errno);
                rc = -errno;
                free(p);
        }
        dl_list_insert_tail(&if_instance->epoll_list, &p->node);
        return rc;
}
static void cb_destroy(struct emca_link_if *if_instance, int fd)
{
	struct if_poll *p, *tmp;

	dl_list_foreach_safe(p, tmp, &if_instance->epoll_list, node) {
		if (p->fd == fd) {
			epoll_ctl(if_instance->epoll_fd, EPOLL_CTL_DEL, p->fd, NULL);
			dl_list_remove(&p->node);
			free(p);
			break;
		}
	}
}
static inline void __notify(struct emca_link_if *if_instance, __s32 result)
{
	pthread_mutex_lock(&if_instance->lock);
        if_instance->notify = result;
        pthread_cond_signal(&if_instance->wait);
        pthread_mutex_unlock(&if_instance->lock);
}
static inline void __user_req_notify(union itc_msg *msg, __s32 result)
{
	pthread_mutex_lock(msg->req.lock);
        *msg->req.notify = result;
        pthread_cond_signal(msg->req.wait);
        pthread_mutex_unlock(msg->req.lock);
	itc_free(&msg);
}
static void __release_if_instance(struct ulh_ref *ref)
{
        struct emca_link_if *if_instance = container_of(ref, struct emca_link_if, ref);

	DBG("if_instance %p", (void *)if_instance);

	if (!dl_list_empty(&if_instance->epoll_list))
		WARN("if_instance %p", (void *)if_instance);
	if (!dl_list_empty(&if_instance->req_list))
		WARN("if_instance %p", (void *)if_instance);
	if (!dl_list_empty(&if_instance->link_list))
		WARN("if_instance %p", (void *)if_instance);

	if (if_instance->api_fd != -1) {
		WARN("if_instance %p", (void *)if_instance);
		cb_destroy(if_instance, if_instance->api_fd);
		close(if_instance->api_fd);
	}
	if (if_instance->mbox_fd != -1) {
		WARN("if_instance %p", (void *)if_instance);
                cb_destroy(if_instance, if_instance->mbox_fd);
                close(if_instance->mbox_fd);
        }
	if (if_instance->mbox != ITC_NO_ID) {
		WARN("if_instance %p", (void *)if_instance);
		if (pthread_self() == if_instance->tid)
			itc_delete_mailbox(if_instance->mbox);
	}
	pthread_cond_destroy(&if_instance->wait);
	free(if_instance);
}

static struct emca_link_if *alloc_if_instance(void)
{
        struct emca_link_if *tmp = calloc(1, sizeof(*tmp));

        if (!tmp)
                return NULL;

	tmp->magic         = EMCAAPI_IF_MAGIC;
	tmp->api_fd        = -1;
	tmp->mbox          = ITC_NO_ID;
        tmp->mbox_fd       = ITC_NO_ID;
	tmp->notify        = 0;
	tmp->instance_stop = 0;
	tmp->seq           = 0;
	tmp->msg_sz        = 0;
	ulh_init_ref(&tmp->ref, 1, __release_if_instance);
	dl_list_init(&tmp->epoll_list);
	dl_list_init(&tmp->link_list);
	dl_list_init(&tmp->req_list);
	pthread_mutex_init(&tmp->lock, NULL);
	pthread_cond_init(&tmp->wait, NULL);
	return tmp;
}

struct emca_link_if *instance_ptr(void *handle)
{
	struct emca_link_if *if_instance;

	if (!handle) {
		WARN("handle == NULL");
		return NULL;
	}
	if_instance = handle;
	if (if_instance->magic != EMCAAPI_IF_MAGIC) {
		WARN("if_instance %p bad magic", (void *)if_instance);
		return NULL;
	}
	if (if_instance->instance_stop) {
		WARN("if_instance %p stop cnt %d", (void *)if_instance, if_instance->instance_stop);
		return NULL;
	}
	return if_instance;
}

static void __release_lobj(struct ulh_ref *ref)
{
        struct link_object *lobj = container_of(ref, struct link_object, ref);

	ulh_unhold_ref(&lobj->if_instance->ref);
	free(lobj);

}
static struct link_object *alloc_lobj(struct emca_link_if *if_instance,
				      union itc_msg *msg)
{
	struct link_object *tmp = calloc(1, sizeof(*tmp));

	if (!tmp) {
		WARN("WTF! calloc fail");
		return NULL;
	}
	ulh_init_ref(&tmp->ref, 1, __release_lobj);
	dl_list_init(&tmp->node);
	tmp->msg = msg;
	tmp->link_state = EMCA_LINK_DOWN;
	tmp->if_instance = if_instance;
	ulh_hold_ref(&if_instance->ref);

	return tmp;
}

static union itc_msg *alloc_user_req(struct emca_link_if *if_instance,
				     pthread_mutex_t *lock,
				     pthread_cond_t *wait,
				     int *notify,
				     __u32 msg_no)
{
	union itc_msg *tmp = itc_alloc(sizeof(struct user_req), msg_no);

	if (!tmp)
		return NULL;

	pthread_mutex_init(lock, NULL);
        pthread_cond_init(wait, NULL);
	tmp->req.lock        = lock;
        tmp->req.wait        = wait;
        tmp->req.notify      = notify;

	return tmp;
}
static struct link_object *get_link(struct emca_link_if *if_instance, __u32 link_instance)
{
        struct link_object *lobj;

        dl_list_foreach(lobj, &if_instance->link_list, node) {
                if (lobj->lid == link_instance) {
			dl_list_remove(&lobj->node);
			return lobj;
		}
	}
	return NULL;
}
static struct link_object *get_req(struct emca_link_if *if_instance, __u32 seq)
{
        struct link_object *lobj;

        dl_list_foreach(lobj, &if_instance->req_list, node) {
                if (lobj->server_seq == seq) {
			dl_list_remove(&lobj->node);
			return lobj;
		}
	}
	return NULL;
}

static void process_link_create(struct emca_link_if *if_instance, union itc_msg *msg)
{
	struct link_object        *lobj = alloc_lobj(if_instance, msg);
	struct riocmd_create_link  create;
	int                        rc;

	if (!lobj) {
		__user_req_notify(msg, NOTIFY_DISASTER);
		return;
	}
	lobj->owner_mbox = msg->req.mbox;
	lobj->server_seq = if_instance->seq++;

	create.hdr.id     = RIOCMD_REQ_CREATE_LINK;
	create.hdr.seq    = lobj->server_seq;
	create.hdr.sz     = sizeof(create);
	create.snid       = msg->req.snid;
	create.dbg_bits   = 0;
	create.hdr.result = 0;

	dl_list_insert_tail(&if_instance->req_list, &lobj->node);

	rc = send(if_instance->api_fd, &create, create.hdr.sz, MSG_DONTWAIT);

	if (rc != create.hdr.sz) {
		WARN("Send api req fail with errno %d", errno);
		dl_list_remove(&lobj->node);
		ulh_unhold_ref(&lobj->ref);
		__user_req_notify(msg, NOTIFY_DISASTER);
	}
}
static void process_link_destroy(struct emca_link_if *if_instance, union itc_msg *msg)
{
	struct link_object  *lobj = get_link(if_instance, msg->req.link_instance);
	struct req_hdr       hdr;
	int                  rc;

	if (!lobj) { /* hm... */
		__user_req_notify(msg, NOTIFY_SUCCESS);
		return;
	}
	lobj->msg  = msg;
	hdr.id     = RIOCMD_REQ_DESTROY_LINK;
	hdr.seq    = lobj->server_seq;
	hdr.sz     = sizeof(hdr);
	hdr.lid    = lobj->lid;
	hdr.result = 0;

	dl_list_insert_tail(&if_instance->req_list, &lobj->node);

	rc = send(if_instance->api_fd, &hdr, hdr.sz, MSG_DONTWAIT);

	if (rc != hdr.sz) {
		WARN("Send api req fail with errno %d", errno);
		dl_list_remove(&lobj->node);
		ulh_unhold_ref(&lobj->ref);
		__user_req_notify(msg, NOTIFY_DISASTER);
	}
}

static int handle_user_req(__u32 events, void *data)
{
	struct emca_link_if *if_instance = data;
	union itc_msg       *msg;

	if (events & ~POLLIN) {
		WARN("if_instance %p Fatal epoll err", (void *)if_instance);
		return -EFAULT;
	}
	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

	switch(msg->msgno) {
	case LINK_CREATE_REQ:
		process_link_create(if_instance, msg);
		return 0;
	case LINK_DESTROY_REQ:
		process_link_destroy(if_instance, msg);
		return 0;
	case INSTANCE_SHUTDOWN_REQ:
		DBG("process shutdown on if_instance %p", (void *)if_instance);
	default:
		itc_free(&msg);
	}
	return -1;
}
static void handle_create_response(struct emca_link_if *if_instance)
{
	struct link_object  *lobj = get_req(if_instance, if_instance->server_msg.hdr.seq);
	union itc_msg       *msg;

	if (!lobj) /* hm... */
		return;

	msg = lobj->msg;
	if (if_instance->server_msg.hdr.result) {
		ulh_unhold_ref(&lobj->ref);
		__user_req_notify(msg, NOTIFY_DISASTER);
		return;
	}
	lobj->lid = if_instance->server_msg.hdr.lid;
	strcpy(lobj->link_name, if_instance->server_msg.hdr.link_name);

	dl_list_insert_tail(&if_instance->link_list, &lobj->node);
	*msg->req.lid = lobj->lid;
	__user_req_notify(msg, NOTIFY_SUCCESS);
}
static void send_link_event(struct emca_link_if *if_instance, struct link_object *lobj, __u32 msgno)
{
	union itc_msg *msg = itc_alloc(sizeof(struct emca_link_event) +
				       strlen(lobj->link_name) + 1, msgno);

	if (!msg) {
		WARN("itc_alloc fail");
		return;
	}
	lobj->link_state = msgno;
	msg->eevent.instance = lobj->lid;
	strcpy(msg->eevent.link_name, lobj->link_name);
	itc_send(&msg, lobj->owner_mbox, ITC_MY_MBOX);
}
static void handle_destroy_response(struct emca_link_if *if_instance)
{
	struct link_object  *lobj = get_req(if_instance, if_instance->server_msg.hdr.seq);
	union itc_msg       *msg;

	if (!lobj) /* hm... */
		return;

	msg = lobj->msg;
	if (if_instance->server_msg.hdr.result) {
		ulh_unhold_ref(&lobj->ref);
		__user_req_notify(msg, NOTIFY_DISASTER);
		return;
	}
	if (lobj->link_state != EMCA_LINK_DOWN)
		send_link_event(if_instance, lobj, EMCA_LINK_DOWN);

	ulh_unhold_ref(&lobj->ref);
	__user_req_notify(msg, NOTIFY_SUCCESS);
}

static void handle_link_event(struct emca_link_if *if_instance)
{
	struct link_object *lobj;

	dl_list_foreach(lobj, &if_instance->link_list, node) {
		if (lobj->lid == if_instance->server_msg.hdr.lid) {
			__u32          msgno;

			if (if_instance->server_msg.hdr.id == RIOCMD_LINK_UP_EVENT)
				msgno = EMCA_LINK_UP;
			else
				msgno = EMCA_LINK_DOWN;
			if (lobj->link_state == msgno)
				break;
			send_link_event(if_instance, lobj, msgno);
			break;
		}
	}
}
static void process_server_msg(struct emca_link_if *if_instance)
{
	switch (if_instance->server_msg.hdr.id) {
        case RIOCMD_REQ_CREATE_LINK:
                handle_create_response(if_instance);
                break;
        case RIOCMD_REQ_DESTROY_LINK:
                handle_destroy_response(if_instance);
                break;
	case RIOCMD_LINK_UP_EVENT:
	case RIOCMD_LINK_DOWN_EVENT:
		handle_link_event(if_instance);
		break;
        default:
		/* UhHuh! */
		break;
        }
        if_instance->msg_sz = 0;
}
static int server_recv(struct emca_link_if *if_instance, int buf_sz)
{
        int offs = if_instance->msg_sz;

        int rc = recv(if_instance->api_fd,
                      if_instance->server_msg.data + offs,
                      buf_sz - offs,
                      MSG_DONTWAIT);

        if (rc < 0 && errno == EAGAIN)
                return EAGAIN;

        if (rc < 0) {
                syslog(LOG_ERR, "If_Instance %s recv fail with errno %d", if_instance->name, errno);
                return -EFAULT;
        }
        if_instance->msg_sz += rc;
        if (rc == (buf_sz - offs))
                return 0;

        return EAGAIN;
}

static int handle_server_api(__u32 events, void *data)
{
	struct emca_link_if *if_instance = data;
	int                  rc;

	if (events & ~POLLIN) {
                WARN("if_instance %p Fatal epoll err", (void *)if_instance);
                return -EFAULT;
        }
	if (if_instance->msg_sz < sizeof(if_instance->server_msg.hdr)) {
                rc = server_recv(if_instance, sizeof(if_instance->server_msg.hdr));
		if (rc < 0)
			return -1;
		if (rc)
			return 0;
		process_server_msg(if_instance);
		return 0;
	}
	return 0;
}
static void link_destroy(struct emca_link_if *if_instance)
{
	struct link_object *lobj, *tmp;

        dl_list_foreach_safe(lobj, tmp, &if_instance->link_list, node) {
		if (lobj->link_state == EMCA_LINK_UP)
			send_link_event(if_instance, lobj, EMCA_LINK_DOWN);
		dl_list_remove(&lobj->node);
		ulh_unhold_ref(&lobj->ref);
	}
}
static void req_cancel(struct emca_link_if *if_instance)
{
        struct link_object *lobj, *tmp;

        dl_list_foreach_safe(lobj, tmp, &if_instance->req_list, node) {
		union itc_msg *msg = lobj->msg;

		dl_list_remove(&lobj->node);
                ulh_unhold_ref(&lobj->ref);
		__user_req_notify(msg, NOTIFY_DISASTER);
	}
}

static void instance_shutdown(struct emca_link_if *if_instance)
{
	DBG("if_instance %p shutdown", (void *)if_instance);

	/* server handles cleanup when client dies */
	cb_destroy(if_instance, if_instance->api_fd);
	close(if_instance->api_fd);
	if_instance->api_fd = -1;

	if (!dl_list_empty(&if_instance->link_list)) {
		link_destroy(if_instance);
	}
	if (!dl_list_empty(&if_instance->req_list)) {
		req_cancel(if_instance);
	}
	cb_destroy(if_instance, if_instance->mbox_fd);
	if_instance->mbox_fd = ITC_NO_ID;

	itc_delete_mailbox(if_instance->mbox);
	if_instance->mbox = ITC_NO_ID;
}
static int init_instance(struct emca_link_if *if_instance)
{
	struct sockaddr_un un;
	int                size;

	if_instance->epoll_fd = epoll_create(IF_EPOLL_MAX);
	if (if_instance->epoll_fd < 0) {
		WARN("epoll create fail");
		return -EFAULT;
	}
	sprintf(if_instance->name, "emca_if_%x", (__u32)if_instance);
	if_instance->mbox = itc_create_mailbox(if_instance->name, 0);
        if (if_instance->mbox == ITC_NO_ID) {
                WARN("itc_create_mailbox() failed");
                return -1;
        }
        if_instance->mbox_fd = itc_get_fd();
        if (if_instance->mbox_fd == ITC_NO_ID) {
                WARN("itc_get_fd() failed");
		return -1;
        }
	if (cb_add(if_instance, if_instance->mbox_fd, handle_user_req)) {
		WARN("cb_add() fail");
		return -1;
	}
	if_instance->api_fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (if_instance->api_fd < 0) {
		WARN("socket() fail with errno %d", errno);
                return -1;
        }
	un.sun_family = AF_UNIX;
	size = sizeof(un.sun_family) +
                sprintf(un.sun_path, RIOCMD_SERVER_NAME);
	un.sun_path[0] = 0;
	if (connect(if_instance->api_fd, (struct sockaddr *)&un, size) < 0) {
		WARN("connect(server) fail with errno %d", errno);
                return -1;
        }
	if (cb_add(if_instance, if_instance->api_fd, handle_server_api)) {
                WARN("cb_add() fail");
                return -1;
        }
	return 0;
}

static void *instance_thread(void *priv)
{
	struct emca_link_if *if_instance = priv;
	struct epoll_event   events[IF_EPOLL_MAX];

	if (init_instance(if_instance)) {
		instance_shutdown(if_instance);
		__notify(if_instance, NOTIFY_DISASTER);
		pthread_exit(0);
        }
	__notify(if_instance, NOTIFY_SUCCESS);

	for (;;) {
		int nfds = epoll_wait(if_instance->epoll_fd, events, IF_EPOLL_MAX, -1);
                int i;

		for (i = 0; i < nfds; i++) {
                        struct if_poll *p = events[i].data.ptr;

			if (p->cb(events[i].events, p->data))
                                goto done;
		}
	}
done:
	instance_shutdown(if_instance);
        __notify(if_instance, NOTIFY_CLOSE);
        pthread_exit(0);
}

static int instance_wait(struct emca_link_if *if_instance,
			 pthread_mutex_t *lock,
			 pthread_cond_t *wait,
			 int *notify)
{
	struct timespec       ts;
	int                   rc = 0;

        pthread_mutex_lock(lock);
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += 60;

        while (!*notify && rc == 0)
                rc = pthread_cond_timedwait(wait, lock, &ts);
        pthread_mutex_unlock(lock);

	return rc;
}

int emca_link_init(struct emca_link_if **handle)
{
	struct emca_link_if *if_instance;
	struct timespec      ts;
        int                  rc = 0;

        if (!handle)
                return -EINVAL;

	if_instance = alloc_if_instance();
	if (!if_instance)
		return -ENOMEM;

	if (pthread_create(&if_instance->tid, NULL, instance_thread, if_instance))
		goto err;

	pthread_mutex_lock(&if_instance->lock);
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += 60;
         while (!if_instance->notify && !rc)
                rc = pthread_cond_timedwait(&if_instance->wait, &if_instance->lock, &ts);
        pthread_mutex_unlock(&if_instance->lock);

        if (rc || (if_instance->notify != NOTIFY_SUCCESS)) {
                pthread_cancel(if_instance->tid);
                pthread_join(if_instance->tid, NULL);
                goto err;
        }
	*handle = if_instance;
	return 0;
err:
	WARN("Start FAIL if_instance %p", (void *)if_instance);
	ulh_unhold_ref(&if_instance->ref);
	return -EFAULT;
}

void emca_link_shutdown(struct emca_link_if **handle)
{
	struct emca_link_if *if_instance = (handle ? instance_ptr(*handle) : NULL);
	struct timespec      ts;
	union itc_msg        *msg;
	int                   rc = 0;

	if (!if_instance)
		return;

	if_instance->instance_stop++;
	msg = itc_alloc(sizeof(__u32), INSTANCE_SHUTDOWN_REQ);
	if (!msg)
		goto stop;

	itc_send(&msg, if_instance->mbox, ITC_MY_MBOX);
	pthread_mutex_lock(&if_instance->lock);
	clock_gettime(CLOCK_REALTIME, &ts);
	ts.tv_sec += 60;
	while (if_instance->notify && !rc)
		rc = pthread_cond_timedwait(&if_instance->wait, &if_instance->lock, &ts);
	pthread_mutex_unlock(&if_instance->lock);

	if (rc || (if_instance->notify != NOTIFY_CLOSE))
		WARN("if_instance %p ignores stop", (void *)if_instance);
stop:
	pthread_cancel(if_instance->tid);
	pthread_join(if_instance->tid, NULL);

	ulh_unhold_ref(&if_instance->ref);
	*handle = NULL;
}

int emca_link_create(struct emca_link_if *handle, struct emca_link_config *cfg, uint32_t *link_instance)
{
	struct emca_link_if *if_instance = instance_ptr(handle);
        pthread_mutex_t       lock;
        pthread_cond_t        wait;
	union itc_msg        *msg;
	int                   notify = 0;
	__u32                 instance = 0;
	int                   rc = 0;

	if (!if_instance)
		return -EINVAL;

	msg = alloc_user_req(if_instance, &lock, &wait, &notify,
			     LINK_CREATE_REQ);
	if (!msg)
		return -ENOMEM;

	msg->req.snid           = cfg->snid;
	msg->req.mbox           = cfg->owner_mbox;
	msg->req.lid            = &instance;
	itc_send(&msg, if_instance->mbox, ITC_MY_MBOX);
	rc = instance_wait(if_instance, &lock, &wait, &notify);

        if (rc || (notify != NOTIFY_SUCCESS)) {
                WARN("if_instance %p snid %x", (void *)if_instance, cfg->snid);
                return -EFAULT;
        }
	*link_instance = instance;
        return 0;
}

int emca_link_destroy(struct emca_link_if *handle, uint32_t link_instance)
{
	struct emca_link_if *if_instance = instance_ptr(handle);
        pthread_mutex_t       lock;
        pthread_cond_t        wait;
	union itc_msg        *msg;
	int                   notify = 0;
	int                   rc = 0;

	if (!if_instance)
		return -EINVAL;

	msg = alloc_user_req(if_instance, &lock, &wait, &notify,
			     LINK_DESTROY_REQ);
	if (!msg)
		return -ENOMEM;

	msg->req.link_instance = link_instance;

	itc_send(&msg, if_instance->mbox, ITC_MY_MBOX);
	rc = instance_wait(if_instance, &lock, &wait, &notify);

        if (rc || (notify != NOTIFY_SUCCESS)) {
                WARN("if_instance %p link_instance %x", (void *)if_instance, link_instance);
                return -EFAULT;
        }
        return 0;
}

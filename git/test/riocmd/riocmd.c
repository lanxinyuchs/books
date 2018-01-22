#include "riocmd-internal.h"

struct riocmd_poll {
        struct dl_list            node;
        int                       fd;
        void                     *data;
	void                     *cookie;
        int (*cb)(__u32 events, void *data);
};

union itc_msg {
	__u32                     msg_no;
};

int restart_flag = 0;
int kill_flag = 0;
int state_clean = 1;

static void cleanup(struct riocmd_control *c)
{
	struct riocmd_client *client;

	state_clean = CLEANUP_STARTED;

	/* don't accept new clients */
	if (c->listen_fd != -1) {
                cb_destroy(c, RIOCMD_LFD_MAGIC, c);
                close(c->listen_fd);
		c->listen_fd = -1;
        }
	/* kill all client connections and
	 * release client resources
	 */
	dl_list_foreach(client, &c->client_list, node)
		client_cleanup(client);

	syslog(LOG_INFO, "Release daemon resources");
	ulh_unhold_ref(&c->ref);
}
static int set_board_defaults(struct riocmd_control *c, struct cfg_data *cfg)
{
	/* FXIME! Move this to kernel instead, no
	 * need to know this in u-space
	 */
	if (cfg->board_type > DUW)
		return -1;

	c->board_type = cfg->board_type;

	if (c->board_type == DUW) {
		char src_mac[6] = {0x02, 0x00, 0x01, 0x00,
				   cfg->master_destid >> 8,
				   cfg->master_destid & 0xff};

		memcpy(c->src_mac, src_mac, 6);
	}
	return 0;
}
static int cb_prepare(struct riocmd_control *c, void *cookie,
		      int fd, void *data,
		      int (*cb)(__u32 events, void *data))
{
	struct riocmd_poll   *p = calloc(1, sizeof(*p));
	struct epoll_event    event;
	int                   rc;

	if (!p)
		return -ENOMEM;

	p->fd = fd;
	p->data = data;
	p->cookie = cookie;
	p->cb = cb;
	dl_list_init(&p->node);
	event.events = POLLIN | POLLERR;
	event.data.ptr = p;
	if((rc = epoll_ctl(c->epoll_fd, EPOLL_CTL_ADD, fd, &event))) {
		syslog(LOG_ERR, "epoll_ctrl() failed with errno %d", errno);
		rc = -errno;
		free(p);
	}
	dl_list_insert_tail(&c->epoll_list, &p->node);
	return rc;
}

static int handle_listen_fd(__u32 events, void *data)
{
	struct riocmd_control *c = data;
	struct riocmd_client *client;
	struct sockaddr_un    client_address;
	socklen_t             size = sizeof(client_address);
	int                   fd = accept(c->listen_fd, (struct sockaddr *)&client_address, &size);

	if (fd < 0) {
		syslog(LOG_ERR, "accept() on listening sk fail with errno %d", errno);
		return 0;
	}
	syslog(LOG_INFO, "Incomming client");
	client = alloc_client(c);
	if (!client) {
		close(fd);
		return 0;
	}
	client->fd = fd;
	if (cb_prepare(c, NULL, fd, (void *)client, handle_client_fd)) {
		syslog(LOG_ERR, "Install epoll cb() for %s failed", client->name);
		goto fail;
	}
	return 0;
fail:
	ulh_unhold_ref(&client->ref);
	return 0;
}

static struct riocmd_control *__init(void)
{
	struct cfg_data        cfg;
	union itc_msg         *msg;
	struct riocmd_control *c;
	struct sockaddr_un     un;
        int                    skt_option = 1;
        int                    size = sizeof(skt_option);

	c = alloc_control();
	if (!c)
		terminate(NULL, EXIT_FAILURE);

	read_config_file(NULL, &cfg);

	syslog(LOG_INFO, "cfg: cfg_file_name     %s\n", cfg.cfg_file_name);
	syslog(LOG_INFO, "cfg: lnh_mbox_name     %s\n", cfg.lnh_mbox_name);
	syslog(LOG_INFO, "cfg: board_type        %s\n", cfg.board_type == DUW ? "DUW" : "DUS");
	syslog(LOG_INFO, "cfg: master_destid     %hx\n", cfg.master_destid);

	if (set_board_defaults(c, &cfg)) {
		syslog(LOG_ERR, "set_board_defaults() failed");
                goto fail;
        }
	/* use ITC when talking to lnh  */
	itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	c->mbox = itc_create_mailbox("riocmd", 0);
	if (c->mbox == ITC_NO_ID) {
		syslog(LOG_ERR, "itc_create_mailbox() failed");
		goto fail;
	}
	c->mbox_fd = itc_get_fd();
        if (c->mbox_fd == ITC_NO_ID) {
		syslog(LOG_ERR, "itc_get_fd() failed");
		goto fail;
	}
	/* no point in doing anything if lnh is down */
	/* get name from cfg file, perhaps? */
	syslog(LOG_INFO, "Locate lnh mailbox: %s", cfg.lnh_mbox_name);
	itc_locate_async(cfg.lnh_mbox_name, NULL, ITC_MY_MBOX);
	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		if (msg->msg_no != ITC_LOCATE_DEFAULT_NO) {
			itc_free(&msg);
			continue;
		}
		c->lnh_mbox = itc_sender(msg);
		itc_free(&msg);
		break;
        }
	syslog(LOG_INFO, "Locate lnh mailbox success");
	msg = itc_alloc(sizeof(__u32), ULH_LNHMSG_LMBOX_DEAD);
	c->lnh_mon_id = itc_monitor(c->lnh_mbox, &msg);
	/* and unix domain sk for API clients */
	c->listen_fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (c->listen_fd < 0) {
		syslog(LOG_ERR, "Create listen_fd failed with errno %d\n", errno);
		goto fail;
	}
	if (setsockopt(c->listen_fd, SOL_SOCKET, SO_REUSEADDR, &skt_option, size) < 0) {
		syslog(LOG_ERR, "setsockopt() failed with errno %d\n", errno);
		goto fail;
	}
	un.sun_family = AF_UNIX;
        size = sizeof(un.sun_family) +
                sprintf(un.sun_path, RIOCMD_SERVER_NAME);
        un.sun_path[0] = 0;
        if (bind(c->listen_fd, (struct sockaddr *) &un, size) < 0) {
                syslog(LOG_ERR, "bind() failes with errno %d", errno);
		goto fail;
	}
	if (listen(c->listen_fd, MAX_CLIENTS) < 0) {
                syslog(LOG_ERR, "listen() failed with errno %d", errno);
		goto fail;
	}
	c->epoll_fd = epoll_create(RIO_EPOLL_MAX);
	if (c->epoll_fd < 0) {
                syslog(LOG_ERR, "epoll_create() failed with errno %d", errno);
		goto fail;
	}
	if (cb_prepare(c, RIOCMD_IFD_MAGIC, c->mbox_fd, (void *)c, handle_itc_fd)) {
		syslog(LOG_ERR, "Install epoll cb() for itc_fd failed");
		goto fail;
	}
	if (cb_prepare(c, RIOCMD_LFD_MAGIC, c->listen_fd, (void *)c, handle_listen_fd)) {
		syslog(LOG_ERR, "Install epoll cb() for listen_fd failed");
		goto fail;
	}
	syslog(LOG_INFO, "Init succes");

	return c;
fail:
	syslog(LOG_ERR, "riocmd init fail");
	terminate(c, EXIT_FAILURE);
	return NULL; /* for compiler */
}
static void run_main_loop(void)
{
	struct riocmd_control *c = NULL;
	struct epoll_event     events[RIO_EPOLL_MAX];
	int                    do_restart;

restart:
	do_restart = 0;
	c = __init();

	for (;;) {
                int nfds = epoll_wait(c->epoll_fd, events, RIO_EPOLL_MAX, -1);
                int i;

                if (nfds < 0 && errno != EINTR) {
			syslog(LOG_ERR, "epoll wait fail with errno %d", errno);
			/* no way out of this pickle */
			terminate(NULL, EXIT_FAILURE);
                }
		if (kill_flag) {
			syslog(LOG_INFO, "Received kill signal");
			terminate(c, EXIT_SUCCESS);
		}
		if (restart_flag) {
			block_sighup(c);
			syslog(LOG_INFO, "Received restart signal");
			restart_flag = 0;
			unblock_sighup(c);
			if (!do_restart) {
				do_restart = 1;
				if (state_clean == NOT_CLEAN)
					cleanup(c);
			} /* else in progress already */
		}
		if (do_restart) {
			if (state_clean == CLEANUP_DONE) {
				syslog(LOG_INFO, "Restarting proxy daemon");
				goto restart;
			} else
				syslog(LOG_NOTICE, "Waiting for daemon cleanup to finish");
		}
                for (i = 0; i < nfds; i++) {
                        struct riocmd_poll *p = events[i].data.ptr;

                        if (p->cb(events[i].events, p->data))
                                terminate(c, EXIT_FAILURE);
                }
        }
}

static inline struct riocmd_preq *__locate_link_obj(void *data, __u32 lid)
{
	struct riocmd_client *client = data;
	struct riocmd_preq   *preq;

	dl_list_foreach(preq, &client->lnh_obj_list, node) {
		if (preq->lid == lid)
			return preq;
	}
	return NULL;
}
struct riocmd_preq *locate_link_obj(struct riocmd_control *c, __u32 lid)
{
	struct riocmd_preq   *preq;
	struct riocmd_poll   *p;

	dl_list_foreach(p, &c->epoll_list, node) {
		if (p->cookie == RIOCMD_LFD_MAGIC ||
		    p->cookie == RIOCMD_IFD_MAGIC)
			continue;

		preq = __locate_link_obj(p->data, lid);
		if (preq)
			return preq;
	}
	dl_list_foreach(preq, &c->req_list, node) {
		if (preq->lid == lid)
                        return preq;
	}
	return NULL;
}
void cb_destroy(struct riocmd_control *c, void *magic, void *data)
{
	struct riocmd_poll *p;

	dl_list_foreach(p, &c->epoll_list, node) {
		if (p->data == data && p->cookie == magic) {
			epoll_ctl(c->epoll_fd, EPOLL_CTL_DEL, p->fd, NULL);
			dl_list_remove(&p->node);
			free(p);
			return;
		}
	}
}

void terminate(struct riocmd_control *c, int return_code)
{
	if (c)
		cleanup(c);

        exit(return_code);
}

int main(int argc, char **argv)
{
	daemonize();
	syslog(LOG_INFO, "Daemon started");
	run_main_loop();
}


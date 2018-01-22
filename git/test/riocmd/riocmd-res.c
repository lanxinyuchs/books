
#include "riocmd-internal.h"


static void __release_control(struct ulh_ref *ref)
{
        struct riocmd_control *c = container_of(ref, struct riocmd_control, ref);

	if (c->lnh_mon_id != (itc_monitor_id_t)ITC_NO_ID) {
		itc_unmonitor(c->lnh_mon_id);
	}
	if (c->mbox_fd != ITC_NO_ID) {
		cb_destroy(c, RIOCMD_IFD_MAGIC, c);
	}
        if (c->mbox != ITC_NO_ID) {
                itc_delete_mailbox(c->mbox);
	}
        itc_exit();
        if (c->listen_fd != -1) {
		cb_destroy(c, RIOCMD_LFD_MAGIC, c);
                close(c->listen_fd);
	}
	/* paranoid check */
	if (!dl_list_empty(&c->client_list))
		syslog(LOG_ERR, "clients reamin at daemon reset");
	syslog(LOG_INFO, "Daemon control destroyed");
	state_clean = CLEANUP_DONE;
        free(c);
}

struct riocmd_control *alloc_control(void)
{
        struct riocmd_control *tmp = calloc(1, sizeof(*tmp));

        if (!tmp) {
                syslog(LOG_ERR, "WTF! alloc control fail");
                return NULL;
        }
        ulh_init_ref(&tmp->ref, 1, __release_control);
        dl_list_init(&tmp->epoll_list);
        dl_list_init(&tmp->req_list);
        dl_list_init(&tmp->client_list);
        tmp->mbox = ITC_NO_ID;
        tmp->mbox_fd = ITC_NO_ID;
        tmp->lnh_mbox = ITC_NO_ID;
        tmp->lnh_mon_id = (itc_monitor_id_t)ITC_NO_ID;
        tmp->listen_fd = -1;
        tmp->epoll_fd = -1;
        tmp->req_seq = 0;
	state_clean = NOT_CLEAN;
	syslog(LOG_INFO, "Daemon control created");
        return tmp;
}

static void __release_client(struct ulh_ref *ref)
{
        struct riocmd_client *client = container_of(ref, struct riocmd_client, ref);

        if (client->fd != -1) {
		cb_destroy(client->control, NULL, client);
                close(client->fd);
	}
	dl_list_remove(&client->node);
	client->control->clients--;
        ulh_unhold_ref(&client->control->ref);
        free(client);
}

struct riocmd_client *alloc_client(struct riocmd_control *c)
{
        struct riocmd_client *tmp = calloc(1, sizeof(*tmp));

        if (c->clients >= MAX_CLIENTS) {
                syslog(LOG_ERR, "Max clients exceeded");
                free(tmp);
                return NULL;
        }
        if (!tmp) {
                syslog(LOG_ERR, "WTF! alloc client fail");
                return NULL;
        }
        tmp->control = c;
        tmp->fd = -1;
        sprintf(tmp->name, "client%x", (__u32)tmp);
        tmp->msg_sz = 0;
	tmp->state = 0;
        c->clients++;
        dl_list_init(&tmp->lnh_obj_list);
	dl_list_init(&tmp->node);
	dl_list_insert_tail(&c->client_list, &tmp->node);
        ulh_hold_ref(&c->ref);
        ulh_init_ref(&tmp->ref, 1, __release_client);

        return tmp;
}
static void __release_preq(struct ulh_ref *ref)
{
        struct riocmd_preq *preq = container_of(ref, struct riocmd_preq, ref);

        ulh_unhold_ref(&preq->client->ref);
        free(preq);
}

struct riocmd_preq *alloc_preq(struct riocmd_client *client, __u32 snid)
{
        struct riocmd_control     *c = client->control;
        struct riocmd_preq        *tmp = calloc(1, sizeof(*tmp));

        if (!tmp) {
                syslog(LOG_ERR, "WTF! alloc pending request data fail");
                return NULL;
        }
        tmp->lnh_seq      = c->req_seq++;
        tmp->api_id       = client->msg.hdr.id;
        tmp->api_seq      = client->msg.hdr.seq;
        tmp->client       = client;
	tmp->pending      = 1;
	tmp->snid         = snid;
	tmp->cmid         = (__u32)-1;
	tmp->lid          = (__u32)-1;
	tmp->ret          = (__u32)-1;
	tmp->link_name[0] = '\0';
        dl_list_init(&tmp->node);
        ulh_init_ref(&tmp->ref, 1, __release_preq);
        ulh_hold_ref(&client->ref);

        return tmp;
}

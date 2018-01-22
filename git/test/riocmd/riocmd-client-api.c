#include "riocmd-internal.h"


static inline __u32 __snid2dest(struct riocmd_control *c, __u32 snid)
{
        if (c->board_type == DUS)
                return (snid >> 8);

        return snid;
}

static inline int __client_alive(struct riocmd_client *client)
{
        return client->state == 0;
}

static inline struct riocmd_preq *get_obj(struct riocmd_client *client, __u32 lid)
{
        struct riocmd_preq            *preq;

        dl_list_foreach(preq, &client->lnh_obj_list, node) {
                if (preq->lid == lid) {
			preq->pending = 1;
                        dl_list_remove(&preq->node);
                        return preq;
                }
        }
        return NULL;
}

static void client_send_response(struct riocmd_client *client, struct req_hdr *hdr)
{
        if (__client_alive(client)) {
                int rc = send(client->fd, hdr, hdr->sz, MSG_DONTWAIT);

                if (rc != hdr->sz) {
                        int err = errno;
                        syslog(LOG_ERR, "Send client response fail with errno %d", errno);
                        if (err == ECONNRESET)
                                client_cleanup(client);
                }
        }
}

static void handle_create_req(struct riocmd_client *client)
{
        struct riocmd_control     *c = client->control;
        struct riocmd_create_link *req = &client->msg.create;
        struct ulh_cm_rio_config   cfg;
        struct riocmd_preq        *preq = alloc_preq(client, req->snid);
        __u16                      cm_peer = __snid2dest(c, req->snid);

        if (!preq) {
                client->msg.hdr.result = ENOMEM;
                client_send_response(client, &client->msg.hdr);
                return;
        }
        dl_list_insert_tail(&c->req_list, &preq->node);

        cfg.cmn.cfg_size = sizeof(cfg);
        cfg.cmn.cfg_version = 0;
        cfg.cmn.cid = cm_peer; /* don't care - not used? */

        cfg.dbg.dbg_union.__dbg_bits = req->dbg_bits;
        cfg.peer_id = cm_peer;
        cfg.mbox    = 0;
        if (c->board_type == DUS)
                strcpy(cfg.ifname, "riodio");
        else {
                char dst_mac[6] = {0x02, 0x00, 0x01, 0x00, cm_peer >> 8, cm_peer & 0xff };

                strcpy(cfg.ifname, "riomsg");
                cfg.letter  = 2;
                cfg.channel = 1;
                memcpy(cfg.src_mac, c->src_mac, 6);
                memcpy(cfg.dst_mac, dst_mac, 6);
        }
	create_cm_req(preq, &cfg);
}
static void handle_destroy_req(struct riocmd_client *client)
{
        struct riocmd_control     *c = client->control;
        struct riocmd_preq        *preq = get_obj(client, client->msg.hdr.lid);

        if (!preq) {
                client->msg.hdr.result = ENODEV;
                client_send_response(client, &client->msg.hdr);
                return;
        }

	preq->api_id = client->msg.hdr.id;
	preq->api_seq = client->msg.hdr.seq;
	preq->lnh_seq = c->req_seq++;
        dl_list_insert_tail(&c->req_list, &preq->node);
	destroy_link_req(preq);
}

static void client_process_req(struct riocmd_client *client)
{
        switch (client->msg.hdr.id) {
        case RIOCMD_REQ_CREATE_LINK:
                handle_create_req(client);
                break;
        case RIOCMD_REQ_DESTROY_LINK:
                handle_destroy_req(client);
                break;
        default:
		syslog(LOG_ERR, "Client %s : create ivalid request", client->name);
                client->msg.hdr.result = EINVAL;
                client_send_response(client, &client->msg.hdr);
        }
        client->msg_sz = 0;
}

static int client_process_hdr(struct riocmd_client *client)
{
        if (client->msg.hdr.sz == client->msg_sz) {
                client_process_req(client);
                return 1; /* done */
        }
        if (client->msg.hdr.sz > RIOCMD_REQ_MAX_SZ) {
                syslog(LOG_ERR, "Client %s bad request id %u size %u",
                       client->name, client->msg.hdr.id, client->msg.hdr.sz);
                client->msg.hdr.result = EINVAL;
                client_send_response(client, &client->msg.hdr);
                client->msg_sz = 0;
                return -EFAULT;
        }
        return 0;
}

static int client_recv(struct riocmd_client *client, int buf_sz)
{
	int offs = client->msg_sz;

        int rc = recv(client->fd,
                      client->msg.data + offs,
                      buf_sz - offs,
                      MSG_DONTWAIT);

        if (rc < 0 && errno == EAGAIN)
                return -EAGAIN;

        if (rc < 0) {
                syslog(LOG_ERR, "Client %s recv fail with errno %d", client->name, errno);
                client_cleanup(client);
                return -EFAULT;
        }
        client->msg_sz += rc;
        if (rc == (buf_sz - offs))
                return 0;

        return -EAGAIN;
}
int handle_client_fd(__u32 events, void *data)
{
        struct riocmd_client *client = data;

        if ((events & ~POLLIN) != 0) {
                syslog(LOG_ERR, "Client %s hung-up? %x", client->name, events);
                client_cleanup(client);
		return 0;
        }
        if (client->msg_sz < sizeof(client->msg.hdr)) {
                if (client_recv(client, sizeof(client->msg.hdr))) {
                        return 0;
		}
                if (client_process_hdr(client)) {
                        return 0;
		}
        }
        if (client_recv(client, client->msg.hdr.sz)) {
                return 0;
	}
        client_process_req(client);
	return 0;
}

void client_rsp(struct riocmd_preq *preq)
{
        struct riocmd_control *c = preq->client->control;
        struct riocmd_client  *client = preq->client;
        struct req_hdr         hdr;
	int                    destroy_req = 0;

        hdr.id     = preq->api_id;
        hdr.seq    = preq->api_seq;
        hdr.lid    = preq->lid;
        hdr.result = preq->ret;
        hdr.sz     = sizeof(hdr);

        if (!preq->ret && preq->api_id == RIOCMD_REQ_CREATE_LINK) {
		if (__client_alive(client)) {
			/* keep req data, add to client obj list */
			preq->pending = 0;
			strcpy(hdr.link_name, preq->link_name);
			dl_list_insert_tail(&client->lnh_obj_list, &preq->node);
		} else {
			/* client died while we were waiting
			 * do cleanup:
			 * put link obj back into req queue
			 */
			dl_list_insert_tail(&c->req_list, &preq->node);
			preq->api_id = RIOCMD_REQ_DESTROY_LINK;
			destroy_link_req(preq);
			return;
		}
        } else
		destroy_req = 1;

        client_send_response(client, &hdr);

	/* if client is dead and this is the
	 * last pending request then client
	 * data release will be called from
	 * the request release function
	 */
	if (destroy_req) {
                ulh_unhold_ref(&preq->ref);
	}
}
void client_event(struct riocmd_preq *preq, __u32 state)
{
        struct riocmd_client  *client = preq->client;
        struct req_hdr         hdr;

	if (state == ULH_LINK_UP)
		hdr.id     = RIOCMD_LINK_UP_EVENT;
	else
		hdr.id     = RIOCMD_LINK_DOWN_EVENT;
        hdr.seq    = preq->api_seq;
        hdr.lid    = preq->lid;
        hdr.result = 0;
	strcpy(hdr.link_name, preq->link_name);
        hdr.sz     = sizeof(hdr);

        client_send_response(client, &hdr);
}

/**
 * User API thread handles user notify
 * for all link objects when the client socket
 * is closed.
 * The daemon handles cleanup of allocated
 * lnh resources.
 * lnh requests pending response will be
 * silently dropped when received, unless
 * they are link_create responses, in which
 * case they should generate a link destroy
 * request instead...
 * client data is released when last response
 * is received.
 * Just about everything may turn out in
 * a totally unexpected way I guess:(
 * Could we make lnh monitor link owner
 * and kill all resources when owner dies
 * perhaps?
 */
void client_cleanup(struct riocmd_client *client)
{
	struct riocmd_control *c = client->control;
	struct riocmd_preq    *preq, *tmp;

	client->state++;   /* stop sending client response */
	/* remove client from epoll fd */
	cb_destroy(c, NULL, client);
	close(client->fd);
	client->fd = -1;

        dl_list_foreach_safe(preq, tmp, &client->lnh_obj_list, node) {
		preq->pending = 1;
		dl_list_remove(&preq->node);
		dl_list_insert_tail(&c->req_list, &preq->node);
		preq->api_id = RIOCMD_REQ_DESTROY_LINK;
		destroy_link_req(preq);
	}
	ulh_unhold_ref(&client->ref);
}



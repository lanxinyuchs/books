#include "riocmd-internal.h"

union itc_msg {
        __u32                             msgno;
	struct ulh_lnhmsg_createcm_req    createcm_req;
        struct ulh_lnhmsg_createcm_rsp    createcm_rsp;
        struct ulh_lnhmsg_createlink_req  createlink_req;
        struct ulh_lnhmsg_createlink_rsp  createlink_rsp;
	struct ulh_lnhmsg_destroylink_req destroylink_req;
        struct ulh_lnhmsg_destroylink_rsp destroylink_rsp;
	struct ulh_lnhmsg_notify          notify;
};

static inline struct riocmd_preq *get_request(struct riocmd_control *c, __u32 seq)
{
        struct riocmd_preq            *preq;

        dl_list_foreach(preq, &c->req_list, node) {
                if (preq->lnh_seq == seq) {
                        dl_list_remove(&preq->node);
                        return preq;
                }
        }
        return NULL;
}

static void create_link(struct riocmd_preq  *preq, __u32 cmid)
{
        struct riocmd_control *c = preq->client->control;
        union itc_msg         *msg = itc_alloc(sizeof(struct ulh_lnhmsg_createlink_req),
					       ULH_LNHMSG_CREATELINK_REQ);

        dl_list_insert_tail(&c->req_list, &preq->node);

	sprintf(preq->link_name, "emca_link%u", cmid);
        msg->createlink_req.seq = preq->lnh_seq;
        strcpy(msg->createlink_req.name, preq->link_name);
        msg->createlink_req.prio = 0;
        msg->createlink_req.cmid = cmid;

        itc_send(&msg, c->lnh_mbox, ITC_MY_MBOX);
}
static void handle_createlink_rsp(struct riocmd_control *c, union itc_msg *msg)
{
        struct riocmd_preq  *preq = get_request(c, msg->createlink_rsp.seq);

        if (!preq) {
                /* Oh my... */
                syslog(LOG_ERR, "Stray createlink_rsp received");
		return;
        }
        preq->ret = msg->createlink_rsp.result;
        if (preq->ret == EAGAIN) {
                create_link(preq, preq->cmid);
		return;
        } else if (preq->ret)
                syslog(LOG_ERR, "createlink_rsp result %u", preq->ret);

        preq->lid = msg->createlink_rsp.lid;
        client_rsp(preq);
}
static void handle_createcm_rsp(struct riocmd_control *c, union itc_msg *msg)
{
        struct riocmd_preq  *preq = get_request(c, msg->createcm_rsp.seq);

        if (!preq) {
                /* Oh my... */
                syslog(LOG_ERR, "Stray createcm_rsp received");
                return;
        }
        preq->ret = msg->createcm_rsp.result;
        preq->cmid = msg->createcm_rsp.cmid;
        if (preq->ret) {
                syslog(LOG_ERR, "createcm_rsp result %u", preq->ret);
                client_rsp(preq);
        } else {
                create_link(preq, msg->createcm_rsp.cmid);
	}
}
static void handle_destroylink_rsp(struct riocmd_control *c, union itc_msg *msg)
{
        struct riocmd_preq  *preq = get_request(c, msg->createlink_rsp.seq);

        if (!preq) {
                /* Oh my... */
                syslog(LOG_ERR, "Stray destroylink_rsp received");
		return;
        }
        preq->ret = msg->destroylink_rsp.result;
	if (preq->ret)
                syslog(LOG_ERR, "destroylink_rsp result %u", preq->ret);

        preq->lid = msg->destroylink_rsp.lid;
        client_rsp(preq);
}
static void handle_notify(struct riocmd_control *c, union itc_msg *msg)
{
	struct riocmd_preq *preq = locate_link_obj(c, msg->notify.lid);

	if (!preq) {
                /* mostly harmless I guess */
                syslog(LOG_INFO, "Stray notify received");
                return;
        }
	client_event(preq, msg->notify.state);
}
void create_cm_req(struct riocmd_preq *preq, struct ulh_cm_rio_config *cfg)
{
	struct riocmd_control *c = preq->client->control;
	union itc_msg         *msg = itc_alloc(sizeof(struct ulh_lnhmsg_createcm_req) +
					       sizeof(*cfg),
					       ULH_LNHMSG_CREATECM_REQ);


	msg->createcm_req.seq = preq->lnh_seq;
	strcpy(msg->createcm_req.cm_name, "srio");
        sprintf(msg->createcm_req.cm_instance, "rio_conn@%u", preq->snid);

        memcpy(msg->createcm_req.config, cfg, sizeof(*cfg));
        itc_send(&msg, c->lnh_mbox, ITC_MY_MBOX);
}
void destroy_link_req(struct riocmd_preq *preq)
{
        struct riocmd_control *c = preq->client->control;
        union itc_msg         *msg = itc_alloc(sizeof(struct ulh_lnhmsg_destroylink_req),
					       ULH_LNHMSG_DESTROYLINK_REQ);

        msg->destroylink_req.seq = preq->lnh_seq;
        msg->destroylink_req.lid = preq->lid;
        itc_send(&msg, c->lnh_mbox, ITC_MY_MBOX);
}
int handle_itc_fd(__u32 events, void *data)
{
        struct riocmd_control *c = data;
	int                    rc = 0;

        if ((events & ~POLLIN) != 0) {
                syslog(LOG_ERR,
                       "Totaly unexpected and un-handled event %x on mbox_fd",
                       events);
                return -EFAULT;
        }
        if (events & POLLIN) {
                union itc_msg     *msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

                switch (msg->msgno) {
                case ULH_LNHMSG_CREATECM_RSP:
                        handle_createcm_rsp(c, msg);
                        break;
                case ULH_LNHMSG_CREATELINK_RSP:
                        handle_createlink_rsp(c, msg);
                        break;
                case ULH_LNHMSG_DESTROYLINK_RSP:
                        handle_destroylink_rsp(c, msg);
                        break;
		case ULH_LNHMSG_NOTIFY:
			handle_notify(c, msg);
			break;
                case ULH_LNHMSG_LMBOX_DEAD:
                        /* lnh died? */
			rc = -EFAULT;
			break;
                default:
                        syslog(LOG_NOTICE, "Ignoring unexpected itc msg signo %x\n", msg->msgno);
                }
		itc_free(&msg);
        }
        return rc;
}

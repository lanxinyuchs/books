/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>
#include <ctype.h>
#include <sys/types.h>
#include <pthread.h>
#include <itc.h>
#include <ulh_cm.h>
#include <ulh_lnh.h>
#include <ulh_lnh_msg.h>
#include "ulh_mhp.h"
#include "ulh_ebcom.h"
#include "ulh_dl_list.h"

#include "eolc-link-api.h"
#include "eolc-link-internal.h"

#define TRACEPOINT_DEFINE
#include <com_ericsson_system_start.h>

#include "ulh_trace.h"
#ifdef LTTNG
#include "ulh_lttng.h"
#endif

#define MHP3_UCM_NAME       "mhpcm"
#define MHP3_ULNH_NAME      "mhplh"
#define MHP3_EBCOM_NAME     "mhpebcom"
#define MHP3_SCHEDFIFO_PRIO 2


#define MHP3_MONITOR_LINK 0x0190041f
struct mon_msg {
        uint32_t msgno;        
        uint32_t cid;        
};

struct mhp3_link {
        struct dl_list        list;

        struct ulh_trans_addr ebcom_addr;
        void                 *owner;
        itc_mbox_id_t         owner_mbox;
        uint32_t              cid;
        uint32_t              cmid;
        uint32_t              lid;
        itc_monitor_id_t      mon_id;

        char                  name[EOLC_LINKNAME_SIZE];
};

struct mhp3_data {
        struct dl_list       mhp3_links;

        itc_mbox_id_t        server_mbox;
};

union itc_msg {
        uint32_t                                  msgno;

        struct eolc_create_req                    eolccreate_req;
        struct eolc_create_rsp                    eolccreate_rsp;
        struct eolc_delete_req                    eolcdestroy_req;
        struct eolc_delete_rsp                    eolcdestroy_rsp;
        struct eolc_linkstate_ind                 lkstate_ind;

        struct ulh_lnhmsg_createcm_req            createcm_req;
        struct ulh_lnhmsg_createcm_rsp            createcm_rsp;
        struct ulh_lnhmsg_createlink_req          createlink_req;
        struct ulh_lnhmsg_createlink_rsp          createlink_rsp;
        struct ulh_lnhmsg_destroylink_req         destroylink_req;
        struct ulh_lnhmsg_destroylink_rsp         destroylink_rsp;
        struct ulh_lnhmsg_notify                  notify;

        struct mon_msg                            mon;
};



static struct mhp3_data mhp3i;
static int own_active_con = 1;



static struct mhp3_link *get_by_cid(uint32_t cid)
{
        struct mhp3_link *lnk;

        dl_list_foreach(lnk, &mhp3i.mhp3_links, list) {
                if (cid == lnk->cid) {
                        return lnk;
                }
        }

        return NULL;
}

static struct mhp3_link *get_by_lid(uint32_t lid)
{
        struct mhp3_link *lnk;

        dl_list_foreach(lnk, &mhp3i.mhp3_links, list) {
                if (lid == lnk->lid) {
                        return lnk;
                }
        }

        return NULL;
}

static int collision_detect(char *name)
{
        struct mhp3_link *lnk;

        dl_list_foreach(lnk, &mhp3i.mhp3_links, list) {
                if (strcmp(lnk->name, name) == 0) {
                        return -EEXIST;
                }
        }
        
        return 0;
}

static void create_mhp(itc_mbox_id_t mbox, 
                       uint32_t      cid, 
                       const char   *name,
                       int           active)
{
	struct ulh_cm_mhp_config c;
	union itc_msg *msg;

	c.cmn.cfg_size    = sizeof(c);
	c.cmn.cfg_version = 0;
	c.cmn.cid         = cid;

	c.rt_tmo          = 200;
	c.ack_tmo         = 10;
	c.kpa_tmo         = 5000;
	c.sup_tmo         = 15000;
	c.tx_wnd          = 10;
	c.rt_limit        = 5;
	c.conn_tmo        = 100;

	c.active          = active;
	c.prios           = 1;

	msg = itc_alloc(sizeof(struct ulh_lnhmsg_createcm_req) +
			sizeof(c),
			ULH_LNHMSG_CREATECM_REQ);
	msg->createcm_req.seq = cid;
	strcpy(msg->createcm_req.cm_name, MHP3_UCM_NAME);
	strcpy(msg->createcm_req.cm_instance, name);

	memcpy(msg->createcm_req.config, &c, sizeof(c));
	itc_send(&msg, mbox, ITC_MY_MBOX);
}

static void handle_create(union itc_msg *cfg)
{
        union itc_msg           *msg;
        struct eolc_g2hw_config *hw_cfg;
        struct ulh_trans_addr    dest, src;
        uint32_t                 cid;
        int                      ret;
        struct mhp3_link        *new_mhp3;

        hw_cfg = &cfg->eolccreate_req.config.g2;

        if ((ret = collision_detect(cfg->eolccreate_req.name))) {
                goto done;
        }


        /* XXX Check which VLAN and qix to use */
        if(ulh_ebcom_makeaddr(hw_cfg->emca_mac, hw_cfg->emca_pcep,
													&dest) == -1) {
                ret = -EIO;
                goto done;
        }
        /* XXX Check which own MAC, VLAN, PCEC and qix to use */
        if(ulh_ebcom_makeaddr(hw_cfg->emca_mac, (hw_cfg->emca_id << 4),
													&src) == -1) {
                ret = -EIO;
                goto done;
        }
        
        cid = ulh_trans_create_conn(MHP3_EBCOM_NAME, &src, &dest);
        if (cid == ULH_TRANS_NOCONN) {
                ret = -EIO;
                goto done;
        }

        create_mhp(mhp3i.server_mbox, cid, 
                   cfg->eolccreate_req.name, own_active_con);

        new_mhp3 = malloc(sizeof(struct mhp3_link));
        if(new_mhp3 == NULL) {
                ret = -ENOMEM;
                goto done;
        }
        
        new_mhp3->ebcom_addr = dest;
        new_mhp3->owner      = cfg->eolccreate_req.owner;
        new_mhp3->owner_mbox = itc_sender(cfg);
        new_mhp3->cid        = cid;
        strcpy(new_mhp3->name, cfg->eolccreate_req.name);
        dl_list_insert_tail(&mhp3i.mhp3_links, &new_mhp3->list);

        return;
 done:
        msg = itc_alloc(sizeof(struct eolc_create_rsp), 
                        EOLC_CREATE_RSP);
        msg->eolccreate_rsp.result = ret;
        itc_send(&msg, itc_sender(cfg), ITC_MY_MBOX);
}

static void handle_destroy(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct mhp3_link *lnk;

        lnk = get_by_cid(cfg->eolcdestroy_req.linkid);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore 
                   so ignore this response. */
                return;
        }
        
        if(lnk->mon_id != ITC_NO_ID) {
                itc_unmonitor(lnk->mon_id);
                lnk->mon_id = ITC_NO_ID;
        }

        ulh_trans_destroy_conn(lnk->cid);

        msg = itc_alloc(sizeof(msg->destroylink_req), 
                        ULH_LNHMSG_DESTROYLINK_REQ);
        msg->destroylink_req.seq = lnk->cid;
        msg->destroylink_req.lid = lnk->lid;
        itc_send(&msg, mhp3i.server_mbox, ITC_MY_MBOX);
}

static void handle_cmresp(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct mhp3_link *lnk;

        lnk = get_by_cid(cfg->createcm_rsp.seq);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore 
                   so ignore this response. */
                return;
        }

        if(cfg->createcm_rsp.result != 0) {
                msg = itc_alloc(sizeof(struct eolc_create_rsp), 
                                EOLC_CREATE_RSP);
                msg->eolccreate_rsp.result = cfg->createcm_rsp.result;
                itc_send(&msg, lnk->owner_mbox, ITC_MY_MBOX);

                dl_list_remove(&lnk->list);
                free(lnk);
        } else {
                lnk->cmid = cfg->createcm_rsp.cmid;

                msg = itc_alloc(sizeof(struct ulh_lnhmsg_createlink_req),
                                ULH_LNHMSG_CREATELINK_REQ);
                msg->createlink_req.seq = lnk->cid;
                strcpy(msg->createlink_req.name, lnk->name);
                msg->createlink_req.prio = 0;
                msg->createlink_req.cmid = cfg->createcm_rsp.cmid;
                
                itc_send(&msg, mhp3i.server_mbox, ITC_MY_MBOX);        
        }
}


static void handle_linkresp(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct mhp3_link *lnk;

        lnk = get_by_cid(cfg->createlink_rsp.seq);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore 
                   so ignore this response. */
                return;
        }

        if(cfg->createlink_rsp.result == 0) {
                lnk->lid = cfg->createlink_rsp.lid;

                msg = itc_alloc(sizeof(struct mon_msg), 
                                MHP3_MONITOR_LINK);
                msg->mon.cid = lnk->cid;
                lnk->mon_id = itc_monitor(lnk->owner_mbox, &msg);
        }

        msg = itc_alloc(sizeof(struct eolc_create_rsp), 
                        EOLC_CREATE_RSP);
        msg->eolccreate_rsp.linkid = lnk->cid;
        msg->eolccreate_rsp.result = cfg->createlink_rsp.result;
        memcpy(msg->eolccreate_rsp.name, lnk->name, EOLC_LINKNAME_SIZE);
        itc_send(&msg, lnk->owner_mbox, ITC_MY_MBOX);

        if(cfg->createlink_rsp.result != 0) {
                dl_list_remove(&lnk->list);
                free(lnk);
        }
}

static void handle_destroyresp(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct mhp3_link *lnk;

        if(cfg->destroylink_rsp.seq == (uint32_t)-1) {
                /* This destory has already been handled 
                   to completion so just ignore it.*/
                return;
        }

        lnk = get_by_cid(cfg->destroylink_rsp.seq);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore 
                   so ignore this response. */
                return;
        }

        msg = itc_alloc(sizeof(struct ulh_lnhmsg_destroylink_rsp), 
                        EOLC_DELETE_RSP);
        msg->eolcdestroy_rsp.result = cfg->destroylink_rsp.result;
	itc_send(&msg, lnk->owner_mbox, ITC_MY_MBOX);

        dl_list_remove(&lnk->list);
        free(lnk);
}

static void handle_monitor(union itc_msg *msg)
{
        struct mhp3_link *lnk;

        lnk = get_by_cid(msg->mon.cid);
        if(lnk == NULL) {
                /* Already destroyed, ignore. */
                return;
        }

        if(lnk->mon_id != ITC_NO_ID) {
                itc_unmonitor(lnk->mon_id);
                lnk->mon_id = ITC_NO_ID;
        }

        msg = itc_alloc(sizeof(msg->destroylink_req), 
                        ULH_LNHMSG_DESTROYLINK_REQ);
        /* Set seq to -1 to signify that this has 
           been done from a monitor so no response 
           shall be sent for this request.*/
        msg->destroylink_req.seq = (uint32_t)-1;
        msg->destroylink_req.lid = lnk->lid;
        itc_send(&msg, mhp3i.server_mbox, ITC_MY_MBOX);

        dl_list_remove(&lnk->list);
        free(lnk);        
}

static void handle_notify(union itc_msg *cfg)
{
        union itc_msg    *msg;
        struct mhp3_link *lnk;

        lnk = get_by_lid(cfg->notify.lid);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore 
                   so ignore this response. */
                return;
        }
        
        msg = itc_alloc(sizeof(struct eolc_linkstate_ind), 
                        EOLC_LINKSTATE_IND);
        msg->lkstate_ind.link_id = lnk->cid;
        if (cfg->notify.state == ULH_LINK_DOWN) {
                msg->lkstate_ind.state = EOLC_LINK_DOWN;
        } else {
                msg->lkstate_ind.state = EOLC_LINK_UP;
        }
        itc_send(&msg, lnk->owner_mbox, ITC_MY_MBOX);
}

int main(int argc, char *argv[])
{
	union itc_msg *msg;
	struct sched_param schedParams;
	uint32_t locrepl[] = { 1, ITC_LOCATE_DEFAULT_NO };
	itc_mbox_id_t me;
	void *lnh;
	int ret;


	/* Raise scheduling class to SCHED_FIFO with prio 2.
		This is needed as long as some RATs are using ITC for
		real-time signalling on DUS52, ie until "simple
		services" are available as a replacement */

	memset(&schedParams, 0, sizeof(struct sched_param));
	schedParams.sched_priority = MHP3_SCHEDFIFO_PRIO;

	ret = pthread_setschedparam(pthread_self(), SCHED_FIFO, &schedParams);
	if (ret) {
		syslog(LOG_ERR, "pthread_setschedparam failure: %d (errno: %d)\n",
						ret, errno);
	}

	dl_list_init(&mhp3i.mhp3_links);

	ret = itc_init(256, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		ULH_TRACE_ERROR("itc_init failure: %d\n",ret);
		return -1;
	}

	me = itc_create_mailbox(EOLC_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		ULH_TRACE_ERROR("created mbox failure: %d\n",me);
		itc_exit();
		return -1;
	}

	ret = ulh_lnh_init(32);
	if (ret) {
		ULH_TRACE_ERROR("ulh_lnh_init() failed, %d\n", ret);
		goto eout;
	}

	ret = ulh_mhp_init(MHP3_UCM_NAME);
	if (ret) {
		ULH_TRACE_ERROR("ulh_mhp_init() failed, %d\n", ret);
                goto eout;
	}

	ret = ulh_ebcom_init(MHP3_EBCOM_NAME);
	if (ret) {
		ULH_TRACE_ERROR("ulh_ebcom_init() failed, %d\n", ret);
                goto eout;
	}

	lnh = ulh_lnh_create(MHP3_ULNH_NAME);
	if (!lnh) {
		ULH_TRACE_ERROR("ulh_lnh_create() failed\n");
		goto eout;
	}

	itc_locate_async(MHP3_ULNH_NAME, NULL, ITC_MY_MBOX);
	msg = itc_receive(locrepl, ITC_NO_TMO, ITC_FROM_ALL);
	mhp3i.server_mbox = itc_sender(msg);
	itc_free(&msg);

	event_system_start("mhp3lnh ready");

	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL); 
		switch(msg->msgno) {
		case EOLC_CREATE_REQ:
			handle_create(msg);
			break;

		case EOLC_DELETE_REQ:
			handle_destroy(msg);
			break;
                        
		case ULH_LNHMSG_CREATECM_RSP:
			handle_cmresp(msg);
			break;
                        
		case ULH_LNHMSG_CREATELINK_RSP:
			handle_linkresp(msg);
			break;
                 
		case ULH_LNHMSG_DESTROYLINK_RSP:
			handle_destroyresp(msg);
			break;

		case MHP3_MONITOR_LINK:
			handle_monitor(msg);
			break;

		case ULH_LNHMSG_NOTIFY:
			handle_notify(msg);
			break;
                        
		default:
			break;
		}
		itc_free(&msg);
	}

	ulh_lnh_destroy(lnh);

	/* Do we need to do more cleanup? */

 eout:
	itc_delete_mailbox(me);
	itc_exit();
	return -1;
}

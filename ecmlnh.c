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
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

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
#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <itc.h>
#include <ulh_cm.h>
#include <ulh_lnh.h>
#include <ulh_lnh_msg.h>
#include "ulh_ecm.h"
#include "ulh_ecmt.h"
#include "ulh_dl_list.h"
#include "ecmlnh.h"
#include "ecm-link-api.h"
#include "ecm-link-internal.h"


#define TRACEPOINT_DEFINE
#include "ulh_trace.h"
#ifdef LTTNG
#include "ulh_lttng.h"
#endif

#define ECM_UCM_NAME       "ecmcm"
#define ECM_ULNH_NAME      "ecmlnh"
#define ECM_ECMT_NAME      "ecmt"
#define ECM_SCHEDFIFO_PRIO 2

#define MAX_LNH_LINK 32
#define MAX_LNH_ENDP 2048

#define ECM_INTERFACE_NAME "efd"

#define UNUSED(x) (void)x

#define ECM_MONITOR_LINK 0x0190041f
struct mon_msg {
        uint32_t msgno;
        uint32_t cid;
};

struct ecm_link {
        struct dl_list        list;

        struct ulh_trans_addr ecmt_addr;
        void                 *owner;
        itc_mbox_id_t         owner_mbox;
        uint32_t              cid;
        uint64_t	      cmid;
        uint32_t              lid;
        itc_monitor_id_t      mon_id;

        char                  name[ECM_LINKNAME_SIZE];
};

struct ecm_data {
        struct dl_list       ecm_links;

        itc_mbox_id_t        server_mbox;
};

union itc_msg {
        uint32_t                                  msgno;

        struct ecm_create_req                     ecmcreate_req;
        struct ecm_create_rsp                     ecmcreate_rsp;
        struct ecm_delete_req                     ecmdestroy_req;
        struct ecm_delete_rsp                     ecmdestroy_rsp;
        struct ecm_linkstate_ind                  lkstate_ind;

        struct ulh_lnhmsg_createcm_req            createcm_req;
        struct ulh_lnhmsg_createcm_rsp            createcm_rsp;
        struct ulh_lnhmsg_createlink_req          createlink_req;
        struct ulh_lnhmsg_createlink_rsp          createlink_rsp;
        struct ulh_lnhmsg_destroylink_req         destroylink_req;
        struct ulh_lnhmsg_destroylink_rsp         destroylink_rsp;
        struct ulh_lnhmsg_notify                  notify;

        struct mon_msg                            mon;
};


/* Keeper of ECM interface data. */
static struct ecm_data ecmi;


/* ===================================================================== */
/**
 *   Get interface MAC address.
 *
 *   @param if_name    Interface name.
 *
 *   @param mac_addr   Out: MAC address (array of 6 bytes).
 *
 *   @return           0 if MAC address was retrieved, otherwise -1.
 *
 *   @par Globals:     --
 *
 */
/* ===================================================================== */
static int get_interface_mac(char *if_name, unsigned char *mac_addr)
{
   struct ifreq ifr;
   int fd, i, ret = -1;

   fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);

   strcpy(ifr.ifr_name, if_name);
   if (ioctl(fd, SIOCGIFHWADDR, &ifr) == 0) {
      for(i = 0; i < ETH_ALEN; i++)
         mac_addr[i] = (unsigned char)ifr.ifr_addr.sa_data[i];
      ret = 0;
   }  else {
      ret = -1;
   }

   close(fd);
   return ret;
}



static struct ecm_link *get_by_cid(uint32_t cid)
{
        struct ecm_link *lnk;

        dl_list_foreach(lnk, &ecmi.ecm_links, list) {
                if (cid == lnk->cid) {
                        return lnk;
                }
        }

        return NULL;
}

static struct ecm_link *get_by_lid(uint32_t lid)
{
        struct ecm_link *lnk;

        dl_list_foreach(lnk, &ecmi.ecm_links, list) {
                if (lid == lnk->lid) {
                        return lnk;
                }
        }

        return NULL;
}

static int collision_detect(char *name)
{
        struct ecm_link *lnk;

        dl_list_foreach(lnk, &ecmi.ecm_links, list) {
                if (strcmp(lnk->name, name) == 0) {
                        return -EEXIST;
                }
        }

        return 0;
}

static void create_ecm(itc_mbox_id_t mbox,
                       uint32_t      cid,
                       const char   *name)
{
	struct ulh_cm_ecm_config c;
	union itc_msg *msg;

	c.cmn.cfg_size    = sizeof(c);
	c.cmn.cfg_version = 0;
	c.cmn.cid         = cid;

	c.rt_tmo          = 100;
	c.ack_tmo         = 10;
	c.kpa_tmo         = 300;
	c.sup_tmo         = 2000;
	c.tx_wnd          = 128;
	c.rt_limit        = 5;
	c.conn_tmo        = 10000;

	c.prios           = 1;

	msg = itc_alloc(sizeof(struct ulh_lnhmsg_createcm_req) +
			sizeof(c),
			ULH_LNHMSG_CREATECM_REQ);
	msg->createcm_req.seq = cid;
	strcpy(msg->createcm_req.cm_name, ECM_UCM_NAME);
	strcpy(msg->createcm_req.cm_instance, name);

	memcpy(msg->createcm_req.config, &c, sizeof(c));
	itc_send(&msg, mbox, ITC_MY_MBOX);
}

static void handle_create(union itc_msg *cfg)
{
        union itc_msg           *msg;
        struct ecm_link_config  *hw_cfg;
        struct ulh_trans_addr    dest, src;
        uint32_t                 cid;
        int                      ret;
        struct ecm_link         *new_ecm;
	uint8_t                  tmpmac[ETH_ALEN];

        hw_cfg = &cfg->ecmcreate_req.config;

        if ((ret = collision_detect(cfg->ecmcreate_req.name))) {
		ULH_TRACE_ERROR("collision detected");
                goto done;
        }

        if(ulh_ecmt_makeaddr(hw_cfg->dst_mac, hw_cfg->dst_vlan,
			     hw_cfg->device, &dest) == -1) {
		ULH_TRACE_ERROR("ulh_ecmt_makeaddr failed");
                ret = -EIO;
                goto done;
        }

	if(get_interface_mac(hw_cfg->device, tmpmac) == -1) {
		ULH_TRACE_ERROR("get_interface_mac failed");
		ret = -EIO;
		goto done;
	}

        if(ulh_ecmt_makeaddr(tmpmac, hw_cfg->dst_vlan,
			     hw_cfg->device, &src) == -1) {
		ULH_TRACE_ERROR("ulh_ecmt_makeaddr failed");
                ret = -EIO;
                goto done;
        }

        cid = ulh_trans_create_conn(ECM_ECMT_NAME, &src, &dest);
        if (cid == ULH_TRANS_NOCONN) {
		ULH_TRACE_ERROR("ulh_trans_create_conn failed");
                ret = -EIO;
                goto done;
        }

        create_ecm(ecmi.server_mbox, cid,
                   cfg->ecmcreate_req.name);

        new_ecm = malloc(sizeof(struct ecm_link));
        if(new_ecm == NULL) {
		ULH_TRACE_ERROR("Out of memory");
                ret = -ENOMEM;
                goto done;
        }

        new_ecm->ecmt_addr  = dest;
        new_ecm->owner      = cfg->ecmcreate_req.owner;
        new_ecm->owner_mbox = itc_sender(cfg);
        new_ecm->cid        = cid;
        strcpy(new_ecm->name, cfg->ecmcreate_req.name);
        dl_list_insert_tail(&ecmi.ecm_links, &new_ecm->list);

        return;
 done:
        msg = itc_alloc(sizeof(struct ecm_create_rsp),
                        ECM_CREATE_RSP);
        msg->ecmcreate_rsp.result = ret;
        itc_send(&msg, itc_sender(cfg), ITC_MY_MBOX);
}

static void handle_destroy(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct ecm_link *lnk;

        lnk = get_by_cid(cfg->ecmdestroy_req.linkid);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore
                   so ignore this response. */
                return;
        }

        if(lnk->mon_id != ITC_NO_ID) {
                itc_unmonitor(lnk->mon_id);
                lnk->mon_id = ITC_NO_ID;
        }

        msg = itc_alloc(sizeof(msg->destroylink_req),
                        ULH_LNHMSG_DESTROYLINK_REQ);
        msg->destroylink_req.seq = lnk->cid;
        msg->destroylink_req.lid = lnk->lid;
        itc_send(&msg, ecmi.server_mbox, ITC_MY_MBOX);
}

static void handle_cmresp(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct ecm_link  *lnk;

        lnk = get_by_cid(cfg->createcm_rsp.seq);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore
                   so ignore this response. */
                return;
        }

        if(cfg->createcm_rsp.result != 0) {
                msg = itc_alloc(sizeof(struct ecm_create_rsp),
                                ECM_CREATE_RSP);
                msg->ecmcreate_rsp.result = cfg->createcm_rsp.result;
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

                itc_send(&msg, ecmi.server_mbox, ITC_MY_MBOX);
        }
}


static void handle_linkresp(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct ecm_link  *lnk;

        lnk = get_by_cid(cfg->createlink_rsp.seq);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore
                   so ignore this response. */
                return;
        }

        if(cfg->createlink_rsp.result == 0) {
                lnk->lid = cfg->createlink_rsp.lid;

                msg = itc_alloc(sizeof(struct mon_msg),
                                ECM_MONITOR_LINK);
                msg->mon.cid = lnk->cid;
                lnk->mon_id = itc_monitor(lnk->owner_mbox, &msg);
        }

        msg = itc_alloc(sizeof(struct ecm_create_rsp),
                        ECM_CREATE_RSP);
        msg->ecmcreate_rsp.linkid = lnk->cid;
        msg->ecmcreate_rsp.result = cfg->createlink_rsp.result;
        memcpy(msg->ecmcreate_rsp.name, lnk->name, ECM_LINKNAME_SIZE);
        itc_send(&msg, lnk->owner_mbox, ITC_MY_MBOX);

        if(cfg->createlink_rsp.result != 0) {
                dl_list_remove(&lnk->list);
                free(lnk);
        }
}

static void handle_destroyresp(union itc_msg *cfg)
{
	union itc_msg    *msg;
        struct ecm_link  *lnk;

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

        ulh_trans_destroy_conn(lnk->cid);

        msg = itc_alloc(sizeof(struct ecm_delete_rsp),
                        ECM_DELETE_RSP);
        msg->ecmdestroy_rsp.owner  = lnk->owner;
        msg->ecmdestroy_rsp.result = cfg->destroylink_rsp.result;
        msg->ecmdestroy_rsp.linkid = lnk->lid;
	itc_send(&msg, lnk->owner_mbox, ITC_MY_MBOX);

        dl_list_remove(&lnk->list);
        free(lnk);
}

static void handle_monitor(union itc_msg *msg)
{
        struct ecm_link *lnk;

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
        itc_send(&msg, ecmi.server_mbox, ITC_MY_MBOX);

        dl_list_remove(&lnk->list);
        free(lnk);
}

static void handle_notify(union itc_msg *cfg)
{
        union itc_msg    *msg;
        struct ecm_link  *lnk;

        lnk = get_by_lid(cfg->notify.lid);
        if(lnk == NULL) {
                /* This connection/link does not exist anymore
                   so ignore this response. */
                return;
        }

        msg = itc_alloc(sizeof(struct ecm_linkstate_ind),
                        ECM_LINKSTATE_IND);
        msg->lkstate_ind.link_id = lnk->cid;
        if (cfg->notify.state == ULH_LINK_DOWN) {
                msg->lkstate_ind.state = ECM_LINK_DOWN;
        } else {
                msg->lkstate_ind.state = ECM_LINK_UP;
        }
        itc_send(&msg, lnk->owner_mbox, ITC_MY_MBOX);
}

void *ecmlnh_thread(void *data)
{
        union itc_msg *msg;
        uint32_t locrepl[] = { 1, ITC_LOCATE_DEFAULT_NO };
        itc_mbox_id_t me;
        void *lnh;
        int ret;

	UNUSED(data);

        dl_list_init(&ecmi.ecm_links);

        me = itc_create_mailbox(ECM_DAEMON_NAME, 0);
        if (me == ITC_NO_ID) {
                ULH_TRACE_ERROR("created mbox failure: %d\n",me);
                itc_exit();
		abort();
        }

        ret = ulh_lnh_init(32);
        if (ret) {
                ULH_TRACE_ERROR("ulh_lnh_init() failed, %d\n", ret);
                goto eout;
        }

	ret = ulh_ecm_init(ECM_UCM_NAME);
	if (ret) {
		ULH_TRACE_ERROR("ulh_ecm_init() failed, %d\n", ret);
                goto eout;
	}

	ret = ulh_ecmt_create(ECM_ECMT_NAME);
	if (ret) {
		ULH_TRACE_ERROR("ulh_ecmt_create() failed, %d\n", ret);
                goto eout;
	}

        lnh = ulh_lnh_create_w_cfg(ECM_ULNH_NAME, MAX_LNH_LINK, MAX_LNH_ENDP);
        if (!lnh) {
                ULH_TRACE_ERROR("ulh_lnh_create() failed\n");
                goto eout;
        }

        itc_locate_async(ECM_ULNH_NAME, NULL, ITC_MY_MBOX);
        msg = itc_receive(locrepl, ITC_NO_TMO, ITC_FROM_ALL);
        ecmi.server_mbox = itc_sender(msg);
        itc_free(&msg);

        for(;;) {
                msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
                switch(msg->msgno) {
                case ECM_CREATE_REQ:
                        handle_create(msg);
                        break;

                case ECM_DELETE_REQ:
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

                case ECM_MONITOR_LINK:
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

        /* TODO: Do we need to do more cleanup? */

 eout:
        itc_delete_mailbox(me);
        itc_exit();
	abort();
}


/* ==================================================================== */
/**
 *   Start linkhandler.
 *
 *   @brief            Starts linkhandler pthread.
 *
 *   @param            -
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void start_ecmlnh(void)
{
	int ret;
	pthread_t tid;
	static int started = 0;

	if(started)
		return;

	started = 1;
	ret = pthread_create(&tid, NULL, ecmlnh_thread, NULL);
	if (ret != 0) {
		fprintf(stderr, "Failed to create the %s thread: %d (%s)\n",
			ECM_THREAD_NAME, ret, strerror(ret));
		abort();
	}

	/* Make it easier to find the linkhandler when debugging. */
	ret = pthread_setname_np(tid, ECM_THREAD_NAME);
}

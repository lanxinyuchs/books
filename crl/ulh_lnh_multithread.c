/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014-2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include <arpa/inet.h>
#include <itc.h>
#include <itc_system.h>
#include <ulh_dl_list.h>
#include <syslog.h>
#include <stdbool.h>

#include "ulh_timer.h"
#include "ulh_transport.h"
#include "ulh_lnh_msg.h"
#include "ulh_cm.h"
#include "ulh_ref.h"
#include "ulh_rlnh.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define FNV32_INIT	 				0x811c9dc5
#define FNV32_PRIM	 				0x01000193

#define HTAB_SIZE        				8192
#define HTAB_MASK        				(HTAB_SIZE - 1)

#define ULH_LNHLINKRECONNECT_TMO                        1000

#define STATE_CONNECTING        		        0
#define STATE_INITIATING        		        1
#define STATE_UP        				2
#define STATE_DISCONNECTED        		        3

#define FREE_SLOT					((uint16_t)0xffff)
#define ZOMBIE_SLOT					((uint16_t)0xfffe)

#define MAX_INFO_PER_MSG         		        2048
#define LNH_INIT_SUCCESS         	        	0
#define LNH_INIT_FAILURE         	        	1

#define ULH_THROUGHPUT_TMO			        1000

/* throughput info per each link is 512 bytes */
#define MAX_THROUGHPUT_INFO_PER_MSG		        1024 * 32

//#define TARGET_DEBUG
#ifdef TARGET_DEBUG
#define lnh_info(...) printf(__VA_ARGS__)
#define lnh_error(...) printf(__VA_ARGS__)
#define lnh_warn(...) printf( __VA_ARGS__)
#else
#define lnh_info(...)
#define lnh_warn(...) syslog(LOG_WARNING, __VA_ARGS__)
#define lnh_error(...) syslog(LOG_ERR, __VA_ARGS__)
#endif


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
#define ASYNC_QUERY_REPLY        		        (0xbabecafe)
struct query_reply {
        uint32_t              			        msgno;
        int32_t               			        linkid;
        char                  		        	name[1];
};

struct ulnh_rep {
        struct dl_list					link;
        struct ulnh_link 	   			*lref;
        itc_mbox_id_t 					mbox;
        uint32_t 					laddr;
        uint64_t			 		tx_msg;
        uint64_t			 		rx_msg;
        uint32_t			 		mbox_hash;
        uint32_t			 		addr_hash;
};

struct ulnh_lep {
        itc_mbox_id_t 					mbox;
        itc_monitor_id_t				mtor;
        uint32_t 					mbox_hash;
        uint64_t			 		tx_msg;
        uint64_t			 		rx_msg;
        uint64_t			 		links;
        uint16_t 					laddr[64];
};

struct ulh_locate {
        struct dl_list 					link;
        char					        name[ULH_LNHNAMESIZ];
};

struct ulnh_link {
        uint32_t 		                        lid;
        uint32_t				        state;
        struct ulh_ref 					ref;
        struct ulh_cm_instance 			        conn;
        struct ulh_lnh 					*lnh;
        struct dl_list  				locs;
        struct dl_list 					reps;
        uint32_t 					prio;
        uint32_t 					zombies;
        struct ulh_timer 				reconn_tmo;
        struct ulh_timer 				reaper_tmo;
        itc_monitor_id_t				mtor;
        itc_mbox_id_t 					owner;
        uint16_t 			                *amap;
        pthread_t 					link_thread;
        char 			                        thread_name[20];
        itc_mbox_id_t 					mbox_id;
        struct ulh_timerqueue         		        tqueue;
        char 						name[1];
};

struct ulh_lnh {
        itc_mbox_id_t 					mbox;
        struct ulnh_link 				**links;
        pthread_rwlock_t                                lnh_rwlock;
        struct ulnh_lep        				**lep_mbox_hash;
        struct ulnh_rep					**rep_mbox_hash;
        struct ulnh_rep					**rep_addr_hash;
        uint32_t 		                        max_endp;
        uint32_t 					max_link;
        pthread_t 					thread;
        itc_mbox_id_t 					parent;
	struct ulh_timerqueue 			        thrp_queue;
        bool                                            enable_thrput;
        struct ulh_timer 				thrp_timer;
        uint32_t		                        thrput_tmo;
        char 						name[1];
};


union itc_msg {
   uint32_t         			 msg_no;
   struct itc_locate_lnh                 locate_lnh;
   struct itc_locate_lnh_reply         	 locate_lnh_reply;
   struct query_reply        	         query_reply;
   struct ulh_lnhmsg_notify              notify;
   struct ulh_lnhmsg_shutdown         	 shut;
   struct ulh_lnhmsg_query         	 query;
   struct ulh_transmsg_data         	 data;
   struct ulh_lnhmsg_createcm_req        createcm_req;
   struct ulh_lnhmsg_createcm_rsp        createcm_rsp;
   struct ulh_lnhmsg_createlink_req      createlink_req;
   struct ulh_lnhmsg_createlink_rsp      createlink_rsp;
   struct ulh_lnhmsg_destroylink_req     destroylink_req;
   struct ulh_lnhmsg_destroylink_rsp     destroylink_rsp;
   struct ulh_lnhmsg_info_req        	 info_req;
   struct ulh_lnhmsg_info_rsp        	 info_rsp;
   struct ulh_lnh_link_forward_lep_dead  link_forward_lep_dead;
   struct ulh_lnh_link_forward_remotemsg link_forward_remotmesg;
   struct ulh_lnh_link_forward_transmsg  link_forward_transmsg;
   struct ulh_lnh_link_forward_locate    link_forward_locate;
   struct ulh_lnh_link_exit              link_exit;
   struct ulh_lnh_link_forward_lep_found link_forward_lep_found;
   struct ulh_lnh_link_create_success    link_create_success;
   struct ulh_lnh_forward_lep_monitor    forward_lep_monitor;
};



/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static void *lnh_local_thread(void *param);
static int tx_rlnh_unpublish(struct ulnh_link*, struct ulnh_lep*);
static char *state_to_string[] = { "RLNH_CONNECTING  ",
        			   "RLNH_INITIATING  ",
        			   "RLNH_UP          ",
        			   "RLNH_DISCONNECTED" };

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static uint32_t hash32(uint32_t key)
{
        uint32_t hash = FNV32_INIT;
        uint8_t *bp = (uint8_t*)&key;

        hash ^= (uint32_t)*bp++;hash *= FNV32_PRIM;
        hash ^= (uint32_t)*bp++;hash *= FNV32_PRIM;
        hash ^= (uint32_t)*bp++;hash *= FNV32_PRIM;
        hash ^= (uint32_t)*bp;  hash *= FNV32_PRIM;

        return ((hash >> (ffs(HTAB_SIZE) - 1)) ^ hash) & HTAB_MASK;
}

static struct ulnh_lep *get_lep_by_id(struct ulh_lnh *lnh, uint32_t key)
{
        struct ulnh_lep *p;
        int cnt, idx;
        idx = hash32(key);

        for (cnt = 0; cnt < HTAB_SIZE; cnt++) {
        	if ((p = lnh->lep_mbox_hash[(idx + cnt) & HTAB_MASK])){
        		if (p->mbox == (itc_mbox_id_t)key) {
        			return p;
                        }
                }
        }
        return NULL;
}

static struct ulnh_lep *get_lep_by_addr(struct ulnh_link *link,
        					uint32_t addr)
{
        struct ulh_lnh *lnh = link->lnh;
        struct ulnh_lep *p = NULL;

        if (addr <= lnh->max_endp)
        	if ((addr = link->amap[addr]) < HTAB_SIZE)
        		p = lnh->lep_mbox_hash[addr];

        return p;
}

static struct ulnh_rep *get_rep_by_id(struct ulh_lnh *lnh,
        	uint32_t key)
{
        struct ulnh_rep *p;
        int cnt, idx;

        pthread_rwlock_rdlock(&lnh->lnh_rwlock);
        idx = hash32(key);

        for (cnt = 0; cnt < HTAB_SIZE; cnt++) {
        	if ((p = lnh->rep_mbox_hash[(idx + cnt) & HTAB_MASK])){
        		if (p->mbox == (itc_mbox_id_t)key) {
                                pthread_rwlock_unlock(&lnh->lnh_rwlock);
        			return p;
                        }
                }
        }
        pthread_rwlock_unlock(&lnh->lnh_rwlock);
        return NULL;
}

static struct ulnh_rep *get_rep_by_addr(struct ulnh_link *link,
        					uint32_t addr)
{
        struct ulh_lnh *lnh = link->lnh;
        struct ulnh_rep *p;
        int cnt, idx;

        idx = hash32((uint32_t)(uintptr_t)link ^ addr);

        for (cnt = 0; cnt < HTAB_SIZE; cnt++){
        	if ((p = lnh->rep_addr_hash[(idx + cnt) & HTAB_MASK])){
        		if ((p->laddr == addr) && (p->lref == link)) {
        			return p;
                        }
                }
        }
        return NULL;
}

static struct ulh_locate *get_locate(struct ulnh_link *lnk,
        	const char *name)
{
        struct ulh_locate *loc;

        dl_list_foreach(loc, &lnk->locs, link) {
        	if (!strcmp(loc->name, name))
        		return loc;
        }
        return NULL;
}

static struct ulnh_link *get_link_by_name(struct ulh_lnh *lnh,
        				  const char *name)
{
        struct ulnh_link *link;
        int i;

        for (i = 0; i < lnh->max_link; i++) {
        	link = lnh->links[i];
        	if (link && (strcmp(link->name, name) == 0))
        		return link;
        }
        return NULL;
}

static int add_to_htab(void **htab, void* elem, uint32_t key)
{
        int cnt;
        int idx = hash32(key);

        for (cnt = 0; cnt < HTAB_SIZE; cnt++) {
        	if (htab[idx] == NULL) {
        		htab[idx] = elem;
        		return idx;
        	}
        	idx = (idx + 1) & HTAB_MASK;
        }
        return -1;
}

static void free_link(struct ulh_ref *ref)
{
        struct ulnh_link *link =
        	container_of(ref, struct ulnh_link, ref);

        if (link->lnh->links[link->lid] == link)
        	link->lnh->links[link->lid] = NULL;

        if (link->conn.ops->dc_fini)
        	link->conn.ops->dc_fini(link->conn.instance, link->prio);

        ulh_cm_destroy_instance(&link->conn);
        free(link->amap);
        free(link);
}

static struct ulnh_link *alloc_link(struct ulh_lnh *lnh,
        			const char *lname)
{
        uint32_t lid = (uint32_t) -1, i, size;
        char name[ULH_LNHNAMESIZ];
        struct ulnh_link *link;

        for (i = 0; lname[i] && (i < ULH_LNHNAMESIZ); i++) {
        	if ((name[i] = lname[i]) == '/')
        		break;
        }
        if (i == ULH_LNHNAMESIZ) {
        name[i - 1] = 0;
        } else{
        name[i] = 0;
        }

        for (i = 0; i < lnh->max_link; i++) {
        	if (!lnh->links[i]) {
        		if (lid == (uint32_t) -1)
        			lid = i;
        	} else if (!strcmp(lnh->links[i]->name, name)) {
        		lnh_error("[LNH] name %s exists\n", name);
        		return NULL;
        	}
        }

        if (lid == (uint32_t)-1) {
        	lnh_error("[LNH] no available slot (%d)\n", lnh->max_link);
        	return NULL;
        }

        size = strlen(name) + 1;
        link = malloc(sizeof(*link) + size);

        if (!link)
        	return NULL;

        memset(link, 0, sizeof(*link));
        strncpy(link->name, name, size);

        size = sizeof(uint16_t) * (lnh->max_endp + 1);
        link->amap = (uint16_t*)malloc(size);
        if (!link->amap) {
        	free(link);
        	return NULL;
        }
        memset(link->amap, 0xff, size);


        dl_list_init(&link->reps);
        dl_list_init(&link->locs);

        link->owner = ITC_NO_ID;
        link->lid = lid;
        link->lnh = lnh;
        link->mbox_id = 0;
        lnh->links[lid] = link;
        ulh_timerqueue_init(&link->tqueue);

        return link;
}

static void free_lep(struct ulh_lnh *lnh, struct ulnh_lep *lep)
{
        itc_unmonitor(lep->mtor);
        if (lep->mbox_hash < HTAB_SIZE) {

        	lnh->lep_mbox_hash[lep->mbox_hash] = NULL;
        }
        free(lep);
}

static void free_rep(struct ulh_lnh *lnh, struct ulnh_rep *rep)
{
        itc_delete_mailbox(rep->mbox);
        dl_list_remove(&rep->link);

        lnh->rep_mbox_hash[rep->mbox_hash] = NULL;
        lnh->rep_addr_hash[rep->addr_hash] = NULL;

        free(rep);
}

static void free_endpoints(struct ulnh_link *link)
{
        struct ulh_lnh *lnh = link->lnh;
        struct ulnh_rep *rep, *rep_tmp;
        struct ulnh_lep *lep;
        uint64_t bit = 0x8000000000000000 >> link->lid;
        int cnt;


        for (cnt = 1; cnt <= lnh->max_endp; cnt++) {
                pthread_rwlock_wrlock(&lnh->lnh_rwlock);
                if (link->amap[cnt] < (uint32_t)ZOMBIE_SLOT) {

                        lep = lnh->lep_mbox_hash[link->amap[cnt]];

                        if (lep) {
                                if(lep->links & bit) {
                                        lep->links &= ~bit;

                                        tx_rlnh_unpublish(link, lep);
                                        if ((lep->links == 0)) {
                                                free_lep(lnh, lep);
                                        }
                                }

                        }
                }
                link->amap[cnt] = FREE_SLOT;
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
        }

        pthread_rwlock_wrlock(&lnh->lnh_rwlock);
        dl_list_foreach_safe(rep, rep_tmp, &link->reps, link)
                free_rep(lnh, rep);
        pthread_rwlock_unlock(&lnh->lnh_rwlock);
}

static void notify_owner(struct ulnh_link *link, uint32_t state)
{
        if (link->owner != ITC_NO_ID) {
        	union itc_msg *msg = itc_alloc(sizeof(*msg), ULH_LNHMSG_NOTIFY);
        	msg->notify.state = state;
        	msg->notify.lid = link->lid;
        	itc_send(&msg, link->owner, ITC_MY_MBOX);
        }
}

static void link_drop(struct ulnh_link *link, int reconnect)
{
        if (link->state == STATE_UP) {
        	lnh_info("[LNH] deassigning linkhandler %s\n",link->name);
        	itc_deassign_linkhandler(link->name, link->lnh->mbox);

        	if (reconnect)
        		notify_owner(link, ULH_LINK_DOWN);
        }
        free_endpoints(link);

        ulh_timerqueue_destroy(&link->tqueue);

        if (link->state != STATE_DISCONNECTED) {
        	link->conn.ops->dc_disconnect(
        			link->conn.instance, link->prio);
        }
        link->state = STATE_DISCONNECTED;

        if (reconnect) {

           ulh_timer_arm(&link->reconn_tmo, &link->tqueue,
        			ULH_LNHLINKRECONNECT_TMO);
        }
}

static void tx_rlnh(struct ulnh_link *link, void *msg, uint32_t size)
{
        struct ulh_cm_msghdr lhhdr;
        struct rlnh_header *hdr = msg;
        int ret;

        (void)hdr;

        lhhdr.size = size;
        lhhdr.src = 0;
        lhhdr.dst = 0;

        ulh_rlnh_print_header(link->name, "RLNH TX", &lhhdr, hdr->type);

        ret = link->conn.ops->dc_transmit(link->conn.instance,
                                          link->prio, &lhhdr, msg);
        if (ret && (ret != -ENOTCONN)) {
        	lnh_error("[LNH] %s::dc_transmit error, %d\n", link->name, ret);
        }
}

static int tx_rlnh_init(struct ulnh_link *link, uint32_t version)
{
        struct rlnh_msg_init *msg;

        msg = (struct rlnh_msg_init *)itc_alloc(sizeof(*msg), 0);
        msg->hdr.reserved = 0;
        msg->hdr.type = htons(RLNH_INIT);
        msg->version = htonl(version);

        tx_rlnh(link, msg, sizeof(*msg));
        itc_free((union itc_msg **)&msg);

        return 0;
}

static int tx_rlnh_init_reply(struct ulnh_link *link,
        							struct rlnh_msg_init *init)
{
        struct rlnh_msg_initreply *msg;
        uint32_t version = ntohl(init->version);
        uint32_t status = 0;

        if (version != RLNH_VERSION)
        	status = 1;

        msg = (struct rlnh_msg_initreply *)itc_alloc(sizeof(*msg), 0);
        msg->hdr.reserved 	= 0;
        msg->hdr.type 		= htons(RLNH_INIT_REPLY);
        msg->status 		= htonl(status);

        tx_rlnh(link, msg, sizeof(*msg));
        itc_free((union itc_msg**)&msg);
        return 0;
}

static int tx_rlnh_unpublish_lep(struct ulnh_link *link, uint32_t amap)
{
        union itc_msg *buf;
        struct rlnh_msg_unpublish *msg;

        buf = itc_alloc(sizeof(*msg), 0);
        msg = (struct rlnh_msg_unpublish *) buf;

        msg->hdr.reserved = 0;
        msg->hdr.type = htons(RLNH_UNPUBLISH);
        msg->laddr = htonl((uint32_t)amap);

        lnh_info("[LNH] tx UNPUBLISH addr %d on %s\n",
                  amap, link->name);

        tx_rlnh(link, buf, sizeof(*msg));
        itc_free(&buf);
        return 0;
}

static int tx_rlnh_unpublish(struct ulnh_link *link, struct ulnh_lep *lep)
{
        union itc_msg *buf;
        struct rlnh_msg_unpublish *msg;

        buf = itc_alloc(sizeof(*msg), 0);
        msg = (struct rlnh_msg_unpublish *) buf;

        msg->hdr.reserved = 0;
        msg->hdr.type = htons(RLNH_UNPUBLISH);
        msg->laddr = htonl((uint32_t)lep->laddr[link->lid]);
        lnh_info("[LNH] tx UNPUBLISH addr %d on %s\n",
                  lep->laddr[link->lid], link->name);
        tx_rlnh(link, buf, sizeof(*msg));
        itc_free(&buf);
        return 0;
}

static void handle_forward_lep_dead(struct ulnh_link *link, union itc_msg *msg)
{

        tx_rlnh_unpublish_lep(link, msg->link_forward_lep_dead.amap);
        link->amap[msg->link_forward_lep_dead.amap] = ZOMBIE_SLOT;
        link->zombies++;
        ulh_timer_arm(&link->reaper_tmo, &link->tqueue, 2000);
}

static int tx_rlnh_unpublish_ack(struct ulnh_link *link, uint32_t laddr)
{
        union itc_msg *buf;
        struct rlnh_msg_unpublish_ack *msg;

        buf = itc_alloc(sizeof(*msg), 0);
        msg = (struct rlnh_msg_unpublish_ack *) buf;

        msg->hdr.reserved = 0;
        msg->hdr.type = htons(RLNH_UNPUBLISH_ACK);
        msg->laddr = htonl(laddr);

        tx_rlnh(link, buf, sizeof(*msg));
        itc_free(&buf);
        return 0;
}

static int tx_rlnh_publish(struct ulnh_link *link, const char *name,
        	           struct ulnh_lep *lep)
{
        struct rlnh_msg_publish *msg;
        union itc_msg *buf;
        int len;
        struct ulh_lnh *lnh = link->lnh;

        len = strlen(name);
        buf = itc_alloc((sizeof(*msg) + len), 0);
        msg = (struct rlnh_msg_publish *) buf;

        msg->hdr.reserved = 0;
        msg->hdr.type = htons(RLNH_PUBLISH);
        pthread_rwlock_rdlock(&lnh->lnh_rwlock);
        msg->laddr = htonl(lep->laddr[link->lid]);
        pthread_rwlock_unlock(&lnh->lnh_rwlock);

        lnh_info("[LNH] tx PUBLISH: %s/%s  (addr: %d)\n",
                  link->name, name, lep->laddr[link->lid]);

        strcpy(msg->name, name);
        tx_rlnh(link, buf, sizeof(*msg) + len);
        itc_free(&buf);
        return 0;
}

static int tx_rlnh_query_name(struct ulnh_link *link,
        	struct ulnh_lep *lep,  const char *name)
{
        union itc_msg *buf;
        struct rlnh_msg_queryname *msg;
        int str_len;
        uint32_t size;
        struct ulh_lnh *lnh=link->lnh;

        str_len = strlen(name) + 1;
        size = (sizeof(*msg) + str_len + 3) & ~0x3;

        buf = itc_alloc(size, 0);
        msg = (struct rlnh_msg_queryname *) buf;

        msg->hdr.reserved = 0;
        msg->hdr.type = htons(RLNH_QUERY_NAME);
        pthread_rwlock_rdlock(&lnh->lnh_rwlock);
        msg->laddr = htonl(lep->laddr[link->lid]);
        pthread_rwlock_unlock(&lnh->lnh_rwlock);

        lnh_info("[LNH] tx QUERY: %s/%s\n", link->name, name);

        msg->hdr.type = htons(RLNH_QUERY_NAME);
        memcpy(msg->name, name, str_len);
        tx_rlnh(link, buf, size);
        itc_free(&buf);
        return 0;
}

static int tx_rlnh_umsg(struct ulnh_lep *lep,
        	   struct ulnh_rep *rep, union itc_msg *msg)
{
        struct ulnh_link *link = rep->lref;
        struct ulh_cm_msghdr lhhdr;
        uint32_t size;
        int ret;
        struct ulh_lnh *lnh=link->lnh;

        size = itc_size(msg);
        msg->msg_no = htonl(msg->msg_no);

        lhhdr.size = size;
        pthread_rwlock_rdlock(&lnh->lnh_rwlock);
        lhhdr.src = lep->laddr[link->lid];
        lhhdr.dst = rep->laddr;
        pthread_rwlock_unlock(&lnh->lnh_rwlock);

        ulh_rlnh_print_header(link->name, "RLNH TX", &lhhdr, msg->msg_no);

        lep->tx_msg++;
        rep->rx_msg++;

        ret = link->conn.ops->dc_transmit(link->conn.instance,
        		link->prio, &lhhdr, msg);

        if (ret && (ret != -ENOTCONN)) {
        	lnh_error("[LNH] %s::dc_transmit error, %d\n", link->name, ret);
        }
        return 0;
}


static void forward_lep_monitor(struct ulh_lnh *lnh, itc_mbox_id_t mbox)
{
        union itc_msg *msg;

        msg = itc_alloc(sizeof(struct ulh_lnh_forward_lep_monitor),
                        ULH_LNH_FORWARD_LEP_MONITOR);
        msg->forward_lep_monitor.mbox = mbox;
        itc_send(&msg, lnh->mbox, ITC_MY_MBOX);
}

static void publish_lep( struct ulnh_link *link,
        		struct ulnh_lep **lepp, itc_mbox_id_t mbox)
{
        struct ulh_lnh *lnh = link->lnh;
        struct ulnh_lep *lep;
        uint64_t bit = 0x8000000000000000 >> link->lid;
        char publish_name[ITC_NAME_MAXLEN];
        uint32_t addr;

        pthread_rwlock_wrlock(&lnh->lnh_rwlock);
        *lepp = get_lep_by_id(link->lnh, mbox);

        if (*lepp && ((*lepp)->links & bit)) {
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
                return;
        }

        if (!itc_get_name(mbox, publish_name, sizeof(publish_name))) {
                lnh_warn("LNH: publish (itc_get_name) mbox 0x%x failed on %s\n",
                                mbox, link->name);
                *lepp = NULL;
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
                return;
        }


        if (*lepp == NULL) {
                lep = malloc(sizeof(*lep));
                memset(lep, 0, sizeof(*lep));
                lep->mbox = mbox;
                lep->mbox_hash = add_to_htab((void**)lnh->lep_mbox_hash,
                                lep, mbox);

                if ((int)lep->mbox_hash < 0) {
                        lnh_error("LNH: publish %s on %s - LEP hash full\n",
                                        publish_name, link->name);
                        free(lep);
                        pthread_rwlock_unlock(&lnh->lnh_rwlock);
                        return;
                }
                lep->mtor = ITC_NO_ID;
                forward_lep_monitor(link->lnh, mbox);

        }else {
                lep = *lepp;

        }

        for (addr = 1; addr <= lnh->max_endp; addr++)
                if (link->amap[addr] == FREE_SLOT)
                        break;

        if (addr > lnh->max_endp) {
                lnh_error("[LNH] - allocate local address"
                                "for %s on link %s failed",
                                publish_name, link->name);
                if (*lepp)
                        *lepp = NULL;
                else{
                        free_lep(lnh, lep);
                }
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
                return;
        }
        *lepp = lep;

        link->amap[addr] = (uint16_t)lep->mbox_hash;
        lep->laddr[link->lid] = addr;
        lep->links |= bit;
        pthread_rwlock_unlock(&lnh->lnh_rwlock);
        tx_rlnh_publish(link, publish_name, lep);
}

static void rx_rlnh_init_reply(struct ulnh_link *link,
        	struct rlnh_msg_initreply *msg)
{
        struct ulh_locate *loc;
        struct ulnh_lep *lep;
        struct ulh_lnh *lnh = link->lnh;
        uint32_t status = ntohl(msg->status);

        if (link->state != STATE_INITIATING)
        	return;

        if (status) {
        	lnh_error("LNH: %s: remote end refused connection "
        			"(status:%u)\n", link->name, status);
        	link_drop(link, 1);
        	return;
        }

        link->state = STATE_UP;
        /* publish ourselfs to ITC */
        itc_assign_linkhandler(link->name, link->lnh->mbox);
        lnh_info("LNH: link %s/ published\n", link->name);
        notify_owner(link, ULH_LINK_UP);

        publish_lep(link, &lep, lnh->mbox);
        if (lep == NULL) {
        	lnh_error("LNH: rx_rlnh_init_reply for %s, publish lep failed\n",
        			link->name);
        	link_drop(link, 1);
        	return;
        }

        /* re-query incomplete remotes */
        dl_list_foreach(loc, &link->locs, link) {
        	tx_rlnh_query_name(link, lep, loc->name);
        }
}

static void rx_rlnh_publish(struct ulnh_link *link,
        	struct rlnh_msg_publish *msg)
{
        char name[ITC_NAME_MAXLEN];
        struct ulh_lnh *lnh = link->lnh;
        uint32_t laddr = ntohl(msg->laddr);
        struct ulnh_rep *rep;

        snprintf(name, sizeof(name), "%s/%s", link->name, msg->name);

        rep = malloc(sizeof(*rep));
        if (!rep) {
                lnh_error("[LNH]: Remote endpoint %s allocation failed\n", name);
        	return;
        }

        memset(rep, 0, sizeof(*rep));
        rep->laddr = laddr;
        rep->lref = link;
        rep->mbox = itc_clone_mailbox(lnh->mbox, name);
        if (rep->mbox != ITC_NO_ID) {
                struct ulh_locate *loc;
                pthread_rwlock_wrlock(&lnh->lnh_rwlock);
                rep->mbox_hash = add_to_htab((void**)lnh->rep_mbox_hash,
                                             rep, rep->mbox);

                rep->addr_hash = add_to_htab((void**)lnh->rep_addr_hash,
                                              rep, rep->laddr ^ (uint32_t)(uintptr_t)rep->lref);
                pthread_rwlock_unlock(&lnh->lnh_rwlock);


        	if (((int)rep->mbox_hash < 0) || ((int)rep->addr_hash < 0)) {
        		lnh_error("LNH: REP hash full (mhash: %d  ahash: %d). "
        				  "Failed to publish remote\n",
        			(int)rep->mbox_hash, (int)rep->addr_hash);

        		itc_delete_mailbox(rep->mbox);
        		free(rep);
        		return;
        	}

        	dl_list_init(&rep->link);
        	dl_list_insert_tail(&link->reps, &rep->link);

        	loc = get_locate(link, msg->name);
        	if (loc) {
        		dl_list_remove(&loc->link);
        		free(loc);
        	}
        } else {
        	lnh_error("[LNH] itc_clone_mailbox(%s) failed\n", name);
        	free(rep);
        }
}

static void rx_rlnh_query(struct ulnh_link *link,
        				struct rlnh_msg_queryname *msg)
{
        struct ulnh_lep *lep;
        itc_mbox_id_t mbox;
        union itc_msg *loc;
        int size;

        lnh_info("[LNH] received QUERY for %s on %s\n", msg->name, link->name);
        mbox = itc_locate(msg->name);
        if (mbox != ITC_NO_ID) {
                publish_lep(link, &lep, mbox);
        	if (lep == NULL) {
        		lnh_error("LNH: rx_rlnh_query for %s on %s, publish lep failed\n",
        				msg->name, link->name);
        	}
        	return;
        }

        size = sizeof(struct query_reply) + strlen(msg->name);
        size = (size + 3) & ~3;

        loc = itc_alloc(size, ASYNC_QUERY_REPLY);
        loc->query_reply.linkid = link->lid;
        strcpy(loc->query_reply.name, msg->name);
        lnh_info("[LNH] itc_locate_async(%s) from %s\n",msg->name, link->name);
        itc_locate_async(msg->name, &loc, link->lnh->mbox);
}

static void rx_rlnh_unpublish(struct ulnh_link *link,
        	struct rlnh_msg_unpublish *msg)
{
        struct ulnh_rep *rep;
        struct ulh_lnh *lnh = link->lnh;
        uint32_t laddr = ntohl(msg->laddr);

        pthread_rwlock_wrlock(&lnh->lnh_rwlock);
        rep = get_rep_by_addr(link, laddr);
        if (!rep) {
                lnh_info("LNH: %s: UNPUBLISH for unknown laddr:0x%x\n",
                                link->name, laddr);
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
                return;
        }

        free_rep(link->lnh, rep);
        pthread_rwlock_unlock(&lnh->lnh_rwlock);
        lnh_info("[LNH] rx UNPUBLISH for 0x%x\n", rep->mbox);
        tx_rlnh_unpublish_ack(link, laddr);
}

static void rx_rlnh_unpublish_ack(struct ulnh_link *link,
        	struct rlnh_msg_unpublish_ack *msg)
{
        struct ulh_lnh *lnh = link->lnh;
        uint32_t laddr = ntohl(msg->laddr);

        if(link->amap[laddr] == ZOMBIE_SLOT) {
                link->amap[laddr] = FREE_SLOT;
                link->zombies--;
        }
        else if(link->amap[laddr] != FREE_SLOT)
                lnh_warn("LNH: Unpublish ack received on %s for slot which is already occupied "
                         "slot:0x%x laddr:0x%x\n",
                         link->name, link->amap[laddr], laddr);


        if (!link->zombies) {
                pthread_rwlock_wrlock(&lnh->lnh_rwlock);
                ulh_timer_cancel(&link->reaper_tmo);
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
        }
}

static void rx_rlnh(struct ulnh_link *link, union itc_msg *msg,
        	uint32_t size)
{
        struct rlnh_header *hdr = (struct rlnh_header *) msg;

        switch (ntohs(hdr->type)) {
        case RLNH_INIT:
        	(void) tx_rlnh_init_reply(link, (struct rlnh_msg_init *)hdr);
        	break;
        case RLNH_INIT_REPLY:
        	rx_rlnh_init_reply(link, (struct rlnh_msg_initreply *) hdr);
        	break;
        case RLNH_PUBLISH:
        	rx_rlnh_publish(link, (struct rlnh_msg_publish *) hdr);
        	break;
        case RLNH_QUERY_NAME:
        	rx_rlnh_query(link, (struct rlnh_msg_queryname *) hdr);
        	break;
        case RLNH_UNPUBLISH:
        	rx_rlnh_unpublish(link,(struct rlnh_msg_unpublish *) hdr);
        	break;
        case RLNH_UNPUBLISH_ACK:
        	rx_rlnh_unpublish_ack(link,(struct rlnh_msg_unpublish_ack *) hdr);
        	break;
        default:
        	lnh_error("LNH: %s: unexpected RLNH message type 0x%x\n",
        			link->name, ntohs(hdr->type));
        	link_drop(link, 1);
        	break;
        }

        itc_free(&msg);
}

static void rx_umsg(struct ulnh_link *link, uint32_t src,
        		uint32_t dst, union itc_msg *msg, uint32_t size)
{
        struct ulnh_rep *rep;
        struct ulnh_lep  *lep;
        struct ulh_lnh *lnh = link->lnh;

        msg->msg_no = ntohl(msg->msg_no);

        pthread_rwlock_wrlock(&lnh->lnh_rwlock);
        lep = get_lep_by_addr(link, dst);
        rep = get_rep_by_addr(link, src);

        if (lep && rep) {
                lep->rx_msg++;
                rep->tx_msg++;

                itc_send(&msg, lep->mbox, rep->mbox);
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
                return;
        }
        pthread_rwlock_unlock(&lnh->lnh_rwlock);

        if (lep) {
                lnh_error("[LNH]: msgno 0x%x to 0x%x from unknown src_addr %d)\n",
                                msg->msg_no, lep->mbox, src);
        } else if (rep) {
                lnh_warn("[LNH]: msgno 0x%x to unknown dst_addr %d\n",
                                msg->msg_no,  dst);
                itc_free(&msg);
                return;
        } else {
                lnh_error("[LNH]: msgno 0x%x unknown src %d  unknown dst %d\n",
                                msg->msg_no, src, dst);
        }
        itc_free(&msg);
        link_drop(link, 1);
}

static void uc_lnh_connected(void *handle)
{
        struct ulnh_link *link = handle;

        lnh_info("LNH: %s: connected (state: %d)\n", link->name, link->state);

        if (link->state != STATE_CONNECTING)
        	return;

        link->state = STATE_INITIATING;
        tx_rlnh_init(link, RLNH_VERSION);
}

static void uc_lnh_disconnected(void *handle)
{
        struct ulnh_link *link = handle;

        lnh_info("LNH: %s: disconnected @state %d\n",
        					link->name, link->state);

        if (link->state == STATE_UP) {
        	itc_deassign_linkhandler(link->name, link->lnh->mbox);
        	lnh_info("LNH: link %s/ unpublished\n", link->name);
        	notify_owner(link, ULH_LINK_DOWN);
        }
        free_endpoints(link);
        link->state = STATE_CONNECTING;
}

static void uc_lnh_delivery(void *handle, struct ulh_cm_msghdr *hdr,
        	union itc_msg *msg)
{
        struct ulnh_link *link = handle;

        ulh_rlnh_print_header(link->name, "RLNH RX", hdr,
        			*((uint16_t *)(msg) + 1));

        if (hdr->dst || hdr->src)
        	rx_umsg(link, hdr->src, hdr->dst, msg, hdr->size);
        else
        	rx_rlnh(link, msg, hdr->size);
}

static struct ulh_cm_uc_ops lnh_uc_ops = {
        .uc_connected 		= uc_lnh_connected,
        .uc_disconnected 	= uc_lnh_disconnected,
        .uc_delivery 		= uc_lnh_delivery,
};

static void lnh_link_reconnect_tmo(void *handle)
{
        struct ulnh_link *link = handle;
        int ret;

        if (link->state != STATE_DISCONNECTED)
        	return;

        ret = link->conn.ops->dc_connect(
        	  link->conn.instance,link->prio);

        if (ret) {
        	/* try again later */
        	ulh_timer_arm(&link->reconn_tmo, &link->tqueue,
        			ULH_LNHLINKRECONNECT_TMO);
        } else
        	link->state = STATE_CONNECTING;
}

static void lnh_link_reaper_tmo(void *handle)
{
        struct ulnh_link *link = handle;
        struct ulh_lnh *lnh = link->lnh;
        int cnt;

        for (cnt = 1; cnt <= lnh->max_endp; cnt++)
        	if (link->amap[cnt] == ZOMBIE_SLOT)
        		link->amap[cnt] = FREE_SLOT;

        link->zombies = 0;
}

static void handle_createcm(struct ulh_lnh *lnh,
        	            union itc_msg *msg)
{
        union itc_msg *rsp, *tmp;
        size_t msize = itc_size(msg);

        tmp = itc_alloc(msize,0);
        memcpy(tmp, msg, msize);

        rsp = itc_alloc(sizeof(struct ulh_lnhmsg_createcm_rsp),
        		ULH_LNHMSG_CREATECM_RSP);
        rsp->createcm_rsp.seq = msg->createcm_req.seq;
        rsp->createcm_rsp.cmid = (uint64_t)(uintptr_t)tmp;
        rsp->createcm_rsp.result = 0;
        itc_send(&rsp, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_createlink(struct ulh_lnh *lnh,
                              union itc_msg *msg)
{
        union itc_msg *rsp, *req, *mon;
        struct ulh_cm_config *cfg;
        struct ulnh_link *link;
        int ret;
        void *res;
        union itc_msg *send_msg;
        union itc_msg *rec_msg;
        uint32_t lnh_init_filter[] = {1, ULH_LNH_LINK_CREATE_SUCCESS };

        rsp = itc_alloc(sizeof(struct ulh_lnhmsg_createlink_rsp),
        		ULH_LNHMSG_CREATELINK_RSP);
        rsp->createlink_rsp.seq = msg->createlink_req.seq;

        req = (union itc_msg*)(uintptr_t)msg->createlink_req.cmid;
        cfg = (struct ulh_cm_config*)&req->createcm_req.config;

        link = alloc_link(lnh, msg->createlink_req.name);
        if (!link) {
        	ret = -ENOMEM;
        	goto err_exit;
        }

        rsp->createlink_rsp.lid = link->lid;
        rsp->createlink_rsp.result = 0;

        cfg->mbox = lnh->mbox;
        cfg->uref = link->lid;

        ret = ulh_cm_create_instance(req->createcm_req.cm_name,
        		req->createcm_req.cm_instance, &link->conn,
        		cfg, &link->tqueue);

        if (ret) {
        	lnh_error("[LNH] ulh_cm_create_instance(), %d\n",ret);
        	free(link);
        	goto err_exit;
        }

        ulh_init_ref(&link->ref, 1, free_link);
        link->prio = msg->createlink_req.prio;
        ret = link->conn.ops->dc_init(link->conn.instance,
        		&lnh_uc_ops, link, link->prio);

        for (; ret == EAGAIN;) {
        	sleep(1);
        	lnh_error("retry on dc_init\n");
        	ret = link->conn.ops->dc_init(link->conn.instance,
        		&lnh_uc_ops, link, link->prio);
        }

        if (ret) {
        	lnh_error("[LNH] dc_init: %d\n", ret);
        	goto out_err;
        }

        link->state = STATE_CONNECTING;
        link->owner = itc_sender(msg);
        ulh_timer_init(&link->reconn_tmo, lnh_link_reconnect_tmo, link);
        ulh_timer_init(&link->reaper_tmo, lnh_link_reaper_tmo, link);

        /* Create thread here and waiting till mailbox is created for newly
         * created thread. dc_init and dc_connect would be called by lnh thread
         * from there onwards everything will be handled by new thread.
         */
        snprintf(link->thread_name, 15, "lnh_%s", link->name);
        pthread_create(&link->link_thread, NULL, lnh_local_thread, link);
        pthread_setname_np(link->link_thread, link->thread_name);

        rec_msg = itc_receive(lnh_init_filter, ITC_NO_TMO, ITC_FROM_ALL);
        if (rec_msg) {
                itc_free(&rec_msg);
        }
        ret = link->conn.ops->dc_connect(link->conn.instance, link->prio);
        if (ret) {
        	lnh_error("[LNH] dc_connect: %d\n", ret);
         	goto out_err;
        }
        itc_free(&req);

        mon = itc_alloc(sizeof(uint32_t), ULH_LNHMSG_OWNER_DEAD);
        link->mtor = itc_monitor(link->owner, &mon);
        itc_send(&rsp, itc_sender(msg), ITC_MY_MBOX);
        return;

out_err:
        if (link->mbox_id) {

                send_msg = itc_alloc(sizeof(struct ulh_lnh_link_exit), ULH_LNH_LINK_EXIT );
                itc_send(&send_msg, link->mbox_id, ITC_MY_MBOX);

                if (pthread_join(link->link_thread, &res)) {
                        syslog(LOG_ERR, "Phtread join failed:%s\n", link->name);

                }
                else {
                        syslog(LOG_ERR, "Pthread join success:%s\n", link->name );
                }
        }
        ulh_unhold_ref(&link->ref);

err_exit:
        itc_free(&req);
        rsp->createlink_rsp.result = ret;
        itc_send(&rsp, itc_sender(msg), ITC_MY_MBOX);
        lnh_error("[LNH] handle_createlink: %d\n", ret);
}

static int handle_transdata(struct ulh_lnh *lnh,
                        	union itc_msg *msg)
{
        struct ulnh_link *link = NULL;
        union itc_msg* send_msg;

        if (msg->data.uref < lnh->max_link)
                link = lnh->links[msg->data.uref];

        if (!link) {
                lnh_warn("[LNH] data for non-existing LID: %lu\n",
                         msg->data.uref);
                ulh_tbuff_free(&msg->data.data);
                return 1;
        }

        if (link->mbox_id) {
                send_msg = itc_alloc(sizeof(struct ulh_lnh_link_forward_transmsg),
                                 ULH_LNH_LINK_FORWARD_TRANSMSG);
                send_msg->link_forward_transmsg.msg = msg;
                send_msg->link_forward_transmsg.link = link;

                itc_send(&send_msg, link->mbox_id, ITC_MY_MBOX);
                return 0;
        }
        else {
                lnh_error("[LNH] Packets received for not available link:%s\n", link->name);
                return 1;
        }
}

static void handle_forward_locate(struct ulnh_link *link, union itc_msg *msg)
{
        struct ulnh_lep *lep;
        itc_mbox_id_t mbox;

        publish_lep(link, &lep, msg->link_forward_locate.from);
        if (lep == NULL) {
        	lnh_error("LNH: locate for %s, publish lep failed\n",
        			msg->link_forward_locate.src_name);
        	return;
        }

        mbox = itc_locate(msg->link_forward_locate.org_name);
        if (mbox == ITC_NO_ID) {
                struct ulh_locate *loc = get_locate(link, msg->link_forward_locate.src_name);
        	if (loc == NULL) {
        		loc = malloc(sizeof(*loc));
        		strncpy(loc->name,msg->link_forward_locate.src_name , ULH_LNHNAMESIZ);
        		dl_list_init(&loc->link);
        		dl_list_insert_tail(&link->locs, &loc->link);
        	}
                tx_rlnh_query_name(link, lep,msg->link_forward_locate.src_name );
        }
}

static void handle_locate(struct ulh_lnh *lnh, union itc_msg *msg)
{
        char link_name[ULH_LNHNAMESIZ], *dst = link_name, *src;
        struct ulnh_link *link;
        int idx;
        union itc_msg *locate;

        lnh_info("[LNH] locate for %s\n", msg->locate_lnh.name);

        for (src = msg->locate_lnh.name; *src; dst++, src++)
        	if ((*dst = *src) == '/') {
        		*dst = 0;
        		break;
        	}

        for (idx = 0; idx < lnh->max_link; idx++) {
        	link = lnh->links[idx];
        	if (link && (strcmp(link_name, link->name) == 0))
        		break;
        	link = NULL;
        }

        if (link == NULL) {
        	lnh_error("LNH: locate for %s, link is not found\n",
        			msg->locate_lnh.name);
        	return;
        }

        if (link->state != STATE_UP)
        	return;

      if (link->mbox_id) {
                locate = itc_alloc(sizeof(struct ulh_lnh_link_forward_locate), ULH_LNH_LINK_FORWARD_LOCATE) ;
                locate->link_forward_locate.from= msg->locate_lnh.from;
                strncpy(locate->link_forward_locate.src_name, ++src, ULH_LNHNAMESIZ);
                locate->link_forward_locate.src_name[ULH_LNHNAMESIZ - 1] = '\0';
                strncpy(locate->link_forward_locate.org_name,msg->locate_lnh.name, ULH_LNHNAMESIZ);
                locate->link_forward_locate.org_name[ULH_LNHNAMESIZ - 1] = '\0';
                itc_send(&locate, link->mbox_id ,ITC_MY_MBOX);
      }
}


static int shutdown_link(struct ulh_lnh *lnh, struct ulnh_link *link)
{
        struct ulh_locate *loc, *loc_tmp;
        itc_mbox_id_t link_mbox_id = ITC_NO_ID;
        uint32_t loclnh[] = {1, ITC_LOCATE_DEFAULT_NO};
        union itc_msg *msg, *send_msg;
        void *res;


        itc_locate_async(link->thread_name, NULL, ITC_MY_MBOX);
        msg = itc_receive(loclnh, ITC_NO_TMO, ITC_FROM_ALL);
        link_mbox_id = itc_sender(msg);
        itc_free(&msg);


        if (link_mbox_id != ITC_NO_ID) {
                send_msg = itc_alloc(sizeof(struct ulh_lnh_link_exit), ULH_LNH_LINK_EXIT );
                itc_send(&send_msg, link_mbox_id, ITC_MY_MBOX);

                if (pthread_join(link->link_thread, &res)) {
                        syslog(LOG_ERR, "phtread join failed \n.");

                }
                else {
                        syslog(LOG_ERR, "pthread join success : 0x%s \n", link->name );
                        link->mbox_id = 0;

                }
        }
        ulh_timer_cancel(&link->reconn_tmo);
        ulh_timer_cancel(&link->reaper_tmo);
        itc_unmonitor(link->mtor);
        link_drop(link, 0);

        dl_list_foreach_safe(loc, loc_tmp, &link->locs, link)
                free(loc);

        ulh_unhold_ref(&link->ref);
        return 0;
}

static void lnh_shutdown(struct ulh_lnh *lnh)
{
        uint32_t i;

        for (i = 0; i < lnh->max_link; i++) {
        	if (lnh->links[i])
        		shutdown_link(lnh, lnh->links[i]);
        }

        for (i = 1; i < HTAB_SIZE; i++)
        	if (lnh->lep_mbox_hash[i]) {
        		free(lnh->lep_mbox_hash[i]);
        		lnh->lep_mbox_hash[i] = NULL;
        	}

        free(lnh->links);
        free(lnh->lep_mbox_hash);
        free(lnh->rep_mbox_hash);
        free(lnh->rep_addr_hash);

        itc_delete_mailbox(lnh->mbox);
        pthread_exit(0);
}

static void handle_destroylink(struct ulh_lnh *lnh,
        			   union itc_msg *msg)
{
        struct ulnh_link *link = NULL;
        union itc_msg *rsp;

        rsp = itc_alloc(sizeof(struct ulh_lnhmsg_destroylink_rsp),
                        ULH_LNHMSG_DESTROYLINK_RSP);
        rsp->destroylink_rsp.seq = msg->destroylink_req.seq;
        rsp->destroylink_rsp.lid = msg->destroylink_req.lid;
        rsp->destroylink_rsp.result = -EINVAL;

        if (msg->destroylink_req.lid < lnh->max_link)
        	link = lnh->links[msg->destroylink_req.lid];

        if (link) {
        	rsp->destroylink_rsp.result = 0;
                shutdown_link(lnh, link);
        }
        itc_send(&rsp, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_lep_dead(struct ulh_lnh *lnh,
        	            union itc_msg *msg)
{
        struct ulnh_lep *lep;
        uint32_t lnk, lzw;
        union itc_msg *send_msg;

        pthread_rwlock_wrlock(&lnh->lnh_rwlock);
        lep = get_lep_by_id(lnh, itc_sender(msg));
        if (lep == NULL){
                pthread_rwlock_unlock(&lnh->lnh_rwlock);

                return;
        }
        if (lep->links) {
                for (lnk = 0; lep->links; lnk++) {
                        lzw = __builtin_clzll(lep->links);
                        lnk += lzw;
                        lep->links <<= (lzw + 1);
                        if (lnh->links[lnk]) {
                                struct ulnh_link *link = lnh->links[lnk];

                                send_msg  = itc_alloc(sizeof(struct ulh_lnh_link_forward_lep_dead),
                                                  ULH_LNH_LINK_FORWARD_LEP_DEAD);
                                send_msg->link_forward_lep_dead.amap = lep->laddr[link->lid];
                                itc_send(&send_msg, link->mbox_id, ITC_MY_MBOX);
                        }
                }
        }
        lep->mtor = ITC_NO_ID;

        free_lep(lnh, lep);
        pthread_rwlock_unlock(&lnh->lnh_rwlock);
}

static void handle_forward_lep_found(struct ulnh_link *link, itc_mbox_id_t mbox)
{
        struct ulnh_lep *lep ;

        publish_lep(link, &lep, mbox);
        if (lep == NULL) {
                lnh_error("LNH: lep found fo but publish lep failed on %s\n",
                          link->name);
        }
}


static void handle_lep_found(struct ulh_lnh *lnh,
        	             union itc_msg *msg)
{
        union itc_msg *send_msg;
        struct ulnh_link *link = lnh->links[msg->query_reply.linkid];

        if (link && (link->state == STATE_UP)) {

                send_msg  = itc_alloc(sizeof(struct ulh_lnh_link_forward_lep_found),
        					ULH_LNH_LINK_FORWARD_LEP_FOUND		);
                send_msg->link_forward_lep_found.mbox_id = itc_sender(msg);
                itc_send(&send_msg, link->mbox_id, ITC_MY_MBOX);
        }
}

static void handle_garbage_collect(struct ulh_lnh *lnh,
        				union itc_msg *msg)
{
        itc_mbox_id_t dead = itc_sender(msg);
        struct ulnh_link *link;
        int idx;

        for (idx = 0; idx < lnh->max_link; idx++) {
        	link = lnh->links[idx];

        	if (link && (link->owner == dead))
        		shutdown_link(lnh, link);
        }
}

static void handle_forward_lep_monitor(struct ulh_lnh *lnh,
        			       union itc_msg *msg)
{
        struct ulnh_lep *lep;
 
        pthread_rwlock_wrlock(&lnh->lnh_rwlock);
        lep = get_lep_by_id(lnh, msg->forward_lep_monitor.mbox);
        if (lep == NULL){
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
                return;
        }
        lep->mtor = itc_monitor(msg->forward_lep_monitor.mbox, NULL);
        pthread_rwlock_unlock(&lnh->lnh_rwlock);
}

static void send_info(char *infotext, int last,
        		   int result, itc_mbox_id_t mbox_id)
{
        union itc_msg *msg;

        msg = itc_alloc((sizeof(struct ulh_lnhmsg_info_rsp) +
        		 strlen(infotext)), ULH_LNHMSG_INFO_RSP);
        msg->info_rsp.last   = last;
        msg->info_rsp.result = result;
        strcpy(msg->info_rsp.infotext, infotext);
        itc_send(&msg, mbox_id, ITC_MY_MBOX);
}

static void handle_link_info(struct ulh_lnh *lnh,
        		     itc_mbox_id_t ret_id)
{
        struct ulnh_link *link;
        char buf[MAX_INFO_PER_MSG], *tmp = buf;
        int i, ret, buf_remain = MAX_INFO_PER_MSG;
        int first = 1;

        strcpy(buf, "");

        for (i = 0; i < lnh->max_link; i++) {
        	if ((link = lnh->links[i]) == NULL)
        		continue;

        	if (first) {
        		strcpy(tmp, "lid  rlnh state        name       ");
        		buf_remain -= strlen(tmp);
        		tmp += strlen(tmp);
        		if(link->conn.ops->dc_info != NULL) {
        			ret = link->conn.ops->dc_info(link->conn.instance,
        								CM_HEADING, tmp, buf_remain);
        			if (ret < 0) {
        				send_info("", 1, ret, ret_id);
        				return;
        			}
        			buf_remain -= strlen(tmp);
        			tmp += strlen(tmp);
        		}
        		if (buf_remain > 0) {
        			strcat(tmp, "\n");
        			buf_remain--;
        			tmp++;
        		}
        		first = 0;
        	}

        	if (buf_remain < 256) {
        		send_info(buf, 0, 0, ret_id);
        		buf_remain = MAX_INFO_PER_MSG;
        		tmp = buf;
        	}

        	snprintf(tmp, buf_remain, "%3d  %s %-10s ",
        		link->lid, state_to_string[link->state], link->name);
        	buf_remain -= strlen(tmp);
        	tmp += strlen(tmp);
        	if (link->conn.ops->dc_info != NULL) {
        		ret = link->conn.ops->dc_info(link->conn.instance,
        								CM_SUMMARY, tmp, buf_remain);
        		if (ret < 0) {
        			send_info("", 1, ret, ret_id);
        			return;
        		}

        		buf_remain -= strlen(tmp);
        		tmp += strlen(tmp);
        	}
        	if (buf_remain > 0) {
        		strcat(tmp, "\n");
        		buf_remain--;
        		tmp++;
        	}
        }
        send_info(buf, 1, 0, ret_id);
}

static void handle_detailed_link_info(struct ulh_lnh *lnh,
        			      int link_id, char *link_name,
        			      itc_mbox_id_t ret_id)
{
        struct ulnh_link *link;
        char buf[MAX_INFO_PER_MSG], *tmp = buf;
        int ret, buf_remain = MAX_INFO_PER_MSG;

        if (link_id == -1) {
        	link = get_link_by_name(lnh, link_name);
        	if (link == NULL) {
        		send_info("", 1, -ENOENT, ret_id);
        		return;
        	}
        } else {
        	if (link_id >= lnh->max_link) {
        		send_info("", 1, -E2BIG, ret_id);
        		return;
        	}

        	link = lnh->links[link_id];
        	if (!link) {
        		send_info("", 1, -ENOENT, ret_id);
        		return;
        	}
        }

        snprintf(tmp,
        	 buf_remain,
        	 "lnh id:    %d\n"
        	 "lnh state: %s\n"
        	 "lnh name:  %s\n"
        	 "lnh owner: 0x%x\n"
        	 "lnh prio:  %d\n",
        	 link->lid, state_to_string[link->state],
        	 link->name, link->owner, link->prio);
        buf_remain -= strlen(tmp);
        tmp += strlen(tmp);
        if (link->conn.ops->dc_info != NULL) {
        	ret = link->conn.ops->dc_info(link->conn.instance, CM_DETAILED,
        				      tmp, buf_remain);
        	if (ret < 0) {
        		send_info("", 1, ret, ret_id);
        		return;
        	}
        }
        send_info(buf, 1, 0, ret_id);
}

static void handle_lep_info(struct ulh_lnh *lnh,
        		    itc_mbox_id_t ret_id)
{
        struct ulnh_lep *lep;
        char ep_name[64];
        char buf[MAX_INFO_PER_MSG], *tmp = buf;
        int len, i, buf_remain = MAX_INFO_PER_MSG;

        len = snprintf(tmp, buf_remain, "\n%4s %10s %18s %14s %14s  %s\n",
        			"idx","mbox","links","txmsg","rxmsg","name");

        tmp += len;
        buf_remain -= len;

        for (i = 0; i < HTAB_SIZE; i++) {
                pthread_rwlock_rdlock(&lnh->lnh_rwlock);
        	if ((lep = lnh->lep_mbox_hash[i]) == NULL) {
                        pthread_rwlock_unlock(&lnh->lnh_rwlock);
        		continue;
                }

        	itc_get_name(lep->mbox, ep_name, sizeof(ep_name));
        	if (buf_remain < 128) {
        		send_info(buf, 0, 0, ret_id);
        		tmp = buf;
        		buf_remain = MAX_INFO_PER_MSG;
        	}

        	len = snprintf(tmp, buf_remain,"%4d 0x%08x 0x%016llx"
        			" %14llu %14llu  %s\n", lep->mbox_hash,
                                lep->mbox, lep->links, lep->tx_msg, lep->rx_msg, ep_name);
                pthread_rwlock_unlock(&lnh->lnh_rwlock);

        	tmp += len;
        	buf_remain -= len;
        }
        send_info(buf, 1, 0, ret_id);
}

static void handle_rep_info(struct ulh_lnh *lnh,
        		    itc_mbox_id_t ret_id)
{
        struct ulnh_link *lnk;
        struct ulnh_rep *rep;
        char ep_name[64];
        char buf[MAX_INFO_PER_MSG], *tmp = buf;
        int len, i, buf_remain = MAX_INFO_PER_MSG;

        len = snprintf(tmp, buf_remain, "\n%10s %5s %14s %14s  %s\n",
                        "mbox","addr","txmsg","rxmsg","name");
        tmp += len;
        buf_remain -= len;

        for (i = 0; i < lnh->max_link; i++) {
                if ((lnk = lnh->links[i]) == NULL)
                        continue;
                pthread_rwlock_rdlock(&lnh->lnh_rwlock);

                dl_list_foreach(rep, &lnk->reps, link) {
                        itc_get_name(rep->mbox, ep_name, sizeof(ep_name));
                        if (buf_remain < 128) {
                                send_info(buf, 0, 0, ret_id);
                                tmp = buf;
                                buf_remain = MAX_INFO_PER_MSG;
                        }

                        len = snprintf(tmp, buf_remain, "0x%08x %5d %14llu %14llu  %s\n", 
                                       rep->mbox, rep->laddr, rep->tx_msg, rep->rx_msg, ep_name);

                        tmp += len;
                        buf_remain -= len;
                }
                pthread_rwlock_unlock(&lnh->lnh_rwlock);
        }
        send_info(buf, 1, 0, ret_id);
}

static void lnh_throughput_set(void *handle, lnh_cm_act_on action)
{
        struct ulh_lnh *lnh = handle;
        struct ulnh_link *link;
        int i;

        for (i = 0; i < lnh->max_link; i++) {
                if ((link = lnh->links[i]) == NULL)
                        continue;

                if(link->conn.ops->dc_config != NULL) {
                        /*  act on given state*/
                        link->conn.ops->dc_config(link->conn.instance, action);
                }
        }

}


static void lnh_throughput_tmo(void *handle)
{
        struct ulh_lnh *lnh = handle;

        lnh_throughput_set(handle, LNH_THROUGHPUT_UPDATE);
        ulh_timer_arm(&lnh->thrp_timer, &lnh->thrp_queue, lnh->thrput_tmo);
}

static void handle_enable_throughput(struct ulh_lnh *lnh,
                                     uint32_t timeout,
                                     itc_mbox_id_t ret_id)
{
        char success_info[250];
        if(lnh->enable_thrput) {
                send_info("Info: Throughput capture is already enabled\n",
                                                            1, 0, ret_id);
                return;
        }
        /* enable throughput flag */
        lnh->enable_thrput = true;
        lnh->thrput_tmo = timeout;
        lnh_throughput_set((void *)lnh, LNH_THROUGHPUT_ENABLE);
        /* Initialize throughput timer */
        ulh_timer_init(&lnh->thrp_timer, lnh_throughput_tmo, (void *)lnh);
        ulh_timer_arm(&lnh->thrp_timer, &lnh->thrp_queue, lnh->thrput_tmo);
        snprintf(success_info, 249, "Info: Throughput capture is enabled " \
                                    "with timeout %ums\n", lnh->thrput_tmo);
        send_info(success_info, 1, 0, ret_id);
}

static void handle_disable_throughput(struct ulh_lnh *lnh, itc_mbox_id_t ret_id)
{
        if( !lnh->enable_thrput) {
                send_info("Warning: Throughput capture is already disabled\n",
                                                              1, 0, ret_id);
                return;
        }
        /* disable throughput flag */
        lnh->enable_thrput = false;

        /* Cancel throughput timer */
        ulh_timer_cancel(&lnh->thrp_timer);
        lnh_throughput_set((void *)lnh, LNH_THROUGHPUT_DISABLE);
        send_info("Info: Throughput capture is disabled\n", 1, 0, ret_id);
}

static void handle_display_throughput_info(struct ulh_lnh *lnh,
			                   itc_mbox_id_t ret_id)
{
        struct ulnh_link *link;
        char buf[MAX_THROUGHPUT_INFO_PER_MSG], *tmp = buf;
        int i, buf_remain = MAX_THROUGHPUT_INFO_PER_MSG, ret;

        if ( !lnh->enable_thrput) {
                send_info("Error: Throughput capture is not enabled\n",
                                                       1, ret, ret_id);
                return;
        }
        snprintf(buf, MAX_THROUGHPUT_INFO_PER_MSG - 1,
                 "Throughput info time duration: %ums\n",
                 lnh->thrput_tmo);
        buf_remain -= strlen(buf);
        tmp += strlen(buf);
        for(i = 0; i < lnh->max_link; i++) {
                if ((link = lnh->links[i]) == NULL)
                        continue;

                if (link->conn.ops->dc_info != NULL) {
                        ret = link->conn.ops->dc_info(link->conn.instance,
                                                            CM_THROUGHPUT,
                                                                      tmp,
                                                               buf_remain);
                        if (ret < 0) {
                                lnh_error("[LNH]: handle_throughput_info: " \
                                                     "insufficient buffer\n");
                                send_info("", 1, ret, ret_id);
                                return;
                        }

                        buf_remain -= strlen(tmp);
                        tmp += strlen(tmp);
                }
        }
        send_info(buf, 1, 0, ret_id);

}

static void handle_info(struct ulh_lnh *lnh, union itc_msg *msg)
{
        switch(msg->info_req.info_type) {
        case ULH_INFO_LINK_SUMMARY:
        	handle_link_info(lnh, itc_sender(msg));
        	break;
        case ULH_INFO_LINK_DETAILED:
        	handle_detailed_link_info(lnh, msg->info_req.info_input,
        				  msg->info_req.info_name,
        				  itc_sender(msg));
        	break;
        case ULH_INFO_LOCAL_EP:
        	handle_lep_info(lnh, itc_sender(msg));
        	break;
        case ULH_INFO_REMOTE_EP:
        	handle_rep_info(lnh, itc_sender(msg));
        	break;
	case ULH_INFO_ENABLE_THROUGHPUT:
                handle_enable_throughput(lnh, msg->info_req.info_input, itc_sender(msg));
		break;
	case ULH_INFO_DISABLE_THROUGHPUT:
                handle_disable_throughput(lnh, itc_sender(msg));
		break;
	case ULH_INFO_DISPLAY_THROUGHPUT:
                handle_display_throughput_info(lnh, itc_sender(msg));
		break;
        default:
        	send_info("", 1, -EINVAL, itc_sender(msg));
        	break;
        }
}

static void handle_localmsg(struct ulh_lnh *lnh, union itc_msg **msg_p)
{
        union itc_msg *msg = *msg_p;

        switch (msg->msg_no) {
                case ULH_LNHMSG_SHUTDOWN:
                        itc_free(msg_p);
                        lnh_shutdown(lnh);
                        break;
                case ULH_TRANSMSG_DATA:
                        if(handle_transdata(lnh, msg)) {
                                if (*msg_p)
                                        itc_free(msg_p);
                        }
                        break;
                case ULH_LNHMSG_CREATECM_REQ:
                        handle_createcm(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ULH_LNHMSG_CREATELINK_REQ:
                        handle_createlink(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ULH_LNHMSG_DESTROYLINK_REQ:
                        handle_destroylink(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ULH_LNHMSG_INFO_REQ:
                        handle_info(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ITC_LOCATE_LNH:
                        handle_locate(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ASYNC_QUERY_REPLY:
                        handle_lep_found(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ITC_LOCATE_DEFAULT_NO:
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ULH_LNHMSG_OWNER_DEAD:
                        handle_garbage_collect(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ULH_LNH_FORWARD_LEP_MONITOR:
                        handle_forward_lep_monitor(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
                case ITC_MONITOR_DEFAULT_NO:
                        handle_lep_dead(lnh, msg);
                        if (*msg_p)
                                itc_free(msg_p);
                        break;

                default:
                        lnh_error("%s: unknown message 0x%x from 0x%x\n", __func__,
                                        msg->msg_no, itc_sender(msg));
                        if (*msg_p)
                                itc_free(msg_p);
                        break;
        }
}

void handle_forward_remotemsg(struct ulh_lnh *lnh, union itc_msg *msg_p, itc_mbox_id_t to)
{
        itc_mbox_id_t from = itc_sender(msg_p);
        struct ulnh_rep *rep = get_rep_by_id(lnh, to);
        struct ulnh_lep *lep;

        if (rep) {
                publish_lep(rep->lref, &lep, from);
                if (lep) {
                        tx_rlnh_umsg(lep, rep, msg_p);
                         return;
                }
         }
         lnh_error("LNH: remote msg LEP (id = %p): %p REP: %p\n",
                   (void*)(uintptr_t)from, (void*)lep, (void*)rep);
}


static void handle_remotemsg(struct ulh_lnh *lnh, union itc_msg **msg_p,
        	             itc_mbox_id_t to)
{
        char link_name[ITC_NAME_MAXLEN];
        char extract_link_name[7];
        struct ulnh_link *link;
        union itc_msg* msg;

        if (!itc_get_name(to, link_name, ITC_NAME_MAXLEN)) {
                lnh_error("LNH: publish (itc_get_name) mbox 0x%x failed on %s\n",
                           to, link_name);
                 if (*msg_p)
                          itc_free(msg_p);
                          return;
        }

        strncpy(extract_link_name, link_name, 6);
        extract_link_name[6] = '\0';

        if (!(link = get_link_by_name(lnh, extract_link_name))) {

                 lnh_error("[LNH] Retrieving link name failed link : %s \n", extract_link_name);
                 if (*msg_p)
                        itc_free(msg_p);
                 return;
        }


        if (link->mbox_id) {
                msg = itc_alloc(sizeof(struct ulh_lnh_link_forward_remotemsg),
                                ULH_LNH_LINK_FORWARD_REMOTEMSG);
                msg->link_forward_remotmesg.msg = *msg_p;
                msg->link_forward_remotmesg.lnh = lnh;
                msg->link_forward_remotmesg.who = to;

                itc_send(&msg, link->mbox_id, ITC_MY_MBOX);
        }
        else {
                lnh_error("[LNH] Link :%s exists, link mailbox does not exist \n", link->name);
                if (*msg_p)
                        itc_free(msg_p);
        }
}


void handle_forward_transmsg(union itc_msg *msg, struct ulnh_link *link)
{
        if (link->conn.ops->dc_receive)
                link->conn.ops->dc_receive(link->conn.instance,
        			           msg->data.cid, &msg->data.data);
        else
                syslog(LOG_ERR, "no dc receive configured\n");
}


static void *lnh_local_thread(void *param)
{
        struct ulnh_link *link = param;
        int tnext;
        int32_t rcv_tmo;
        union itc_msg *msg, *msg1;

        link->mbox_id = itc_create_mailbox((char *)link->thread_name, 0);

        msg = itc_alloc(4, ULH_LNH_LINK_CREATE_SUCCESS);
        itc_send(&msg, link->lnh->mbox, ITC_MY_MBOX);

        for (;;)
        {
                rcv_tmo = ITC_NO_TMO;
                tnext = ulh_timerqueue_schedule(&link->tqueue);
                if (tnext >= 0)
                        rcv_tmo = tnext;

                msg = itc_receive(ITC_NOFILTER,rcv_tmo,ITC_FROM_ALL);
                if (msg) {
                        switch(msg->msg_no){
                                case ULH_LNH_LINK_FORWARD_REMOTEMSG:
                                        msg1 =  msg->link_forward_remotmesg.msg;
                                        handle_forward_remotemsg(link->lnh,
                                                                 msg->link_forward_remotmesg.msg,
                                                                 msg->link_forward_remotmesg.who);
                                        if (msg1) {
                                                itc_free(&msg1);
                                        }
                                        break;
                                case ULH_LNH_LINK_FORWARD_TRANSMSG:
                                        msg1 = msg->link_forward_transmsg.msg;
                                        handle_forward_transmsg((union itc_msg *)msg->link_forward_transmsg.msg,
                                                                msg->link_forward_transmsg.link);
                                        if (msg1) {
                                                itc_free(&msg1);
                                        }
                                        break;
                                case ULH_LNH_LINK_FORWARD_LOCATE:
                                        handle_forward_locate(link, msg);
                                        break;
                                case ULH_LNH_LINK_EXIT:
                                        pthread_exit(0);
                                        break;
                                case ULH_LNH_LINK_FORWARD_LEP_FOUND:
                                        handle_forward_lep_found(link, msg->link_forward_lep_found.mbox_id);
                                        break;
                                case ULH_LNH_LINK_FORWARD_LEP_DEAD:
                                        handle_forward_lep_dead(link, msg);
                                        break;
                                default :
                                        syslog(LOG_ERR, "Received unknown message : 0x%x \n", msg->msg_no);
                                        break;
                        }

                        if (msg)
                                itc_free(&msg);
                }
        }
}

static int lnh_init(struct ulh_lnh *lnh)
{
        uint32_t size;

        lnh->mbox = itc_create_mailbox(lnh->name, 0);
        if (lnh->mbox == ITC_NO_ID) {
        	lnh_error("[LNH] failed to create mailbox, %d\n", errno);
        	pthread_exit(NULL);
        }

        ulh_timerqueue_init(&lnh->thrp_queue);

        size = sizeof(struct ulnh_link *) * lnh->max_link;
        lnh->links = malloc(size);
        if (!lnh->links)
        	goto out_err;
        memset(lnh->links, 0, size);

        size = sizeof(struct ulnh_lep *) * HTAB_SIZE;
        lnh->lep_mbox_hash = malloc(size);
        if (!lnh->lep_mbox_hash)
        	goto out_err;
        memset(lnh->lep_mbox_hash, 0, size);

        pthread_rwlock_init(&lnh->lnh_rwlock, NULL);

        size = sizeof(struct ulnh_rep *) * HTAB_SIZE;
        lnh->rep_mbox_hash = malloc(size);
        if (!lnh->rep_mbox_hash)
        	goto out_err;
        memset(lnh->rep_mbox_hash, 0, size);

        size = sizeof(struct ulnh_rep *) * HTAB_SIZE;
        lnh->rep_addr_hash = malloc(size);
        if (!lnh->rep_addr_hash)
        	goto out_err;
        memset(lnh->rep_addr_hash, 0, size);

        lnh->enable_thrput = false;

        return 1;

out_err:

        if (lnh->links)
        	free(lnh->links);
        if (lnh->lep_mbox_hash)
        	free(lnh->lep_mbox_hash);
        if (lnh->rep_mbox_hash)
        	free(lnh->rep_mbox_hash);
        if (lnh->rep_addr_hash)
        	free(lnh->rep_addr_hash);

        itc_delete_mailbox(lnh->mbox);
        return 0;
}

static void *lnh_thread(void *param)
{
        struct ulh_lnh *lnh = param;
        int  tnext, run;
        int32_t rcv_tmo;
        union itc_msg *msg;
        itc_mbox_id_t who;

        run = lnh_init(lnh);
        msg = itc_alloc(4,run);
        itc_send(&msg, lnh->parent, ITC_MY_MBOX);


        for (;run;) {
                rcv_tmo = ITC_NO_TMO;
                if (lnh->enable_thrput) {
                        tnext = ulh_timerqueue_schedule(&lnh->thrp_queue);
                        if (tnext >= 0)
                                rcv_tmo = tnext;
                }

                msg = itc_receive(ITC_NOFILTER, rcv_tmo, ITC_FROM_ALL);
                if (msg) {

                        who = itc_receiver(msg);
                        if (who == lnh->mbox){

                                handle_localmsg(lnh, &msg);
                        }
                        else {
                                handle_remotemsg(lnh, &msg, who);
                        }
                }
        }
        pthread_exit(0);
}

int ulh_lnh_init(uint32_t max_connections)
{
        int ret;

        openlog(NULL, LOG_PID | LOG_ODELAY, LOG_DAEMON);
        ret = ulh_trans_init(max_connections);
        if (ret)
        	return ret;

        return ulh_cm_init();
}

struct ulh_lnh *ulh_lnh_create_w_cfg(const char *name,
        			uint32_t max_link, uint32_t max_endp)
{
        struct ulh_lnh *lnh;
        union itc_msg *msg;
        uint32_t lnh_init_filter[] = {2, LNH_INIT_SUCCESS, LNH_INIT_FAILURE};
        int size, start_done = 0;

        if (!name)
        	return NULL;

        size = strlen(name) + 1;
        lnh = malloc(sizeof(*lnh) + size);

        if (!lnh)
        	return NULL;

        memset(lnh, 0, sizeof(*lnh));
        strncpy(lnh->name, name, size);

        lnh->max_link = max_link;
        lnh->max_endp = max_endp;
        lnh->parent = itc_current_mbox();

        if (pthread_create(&lnh->thread, NULL, lnh_thread, lnh)) {
                free(lnh);
                return NULL;
        }

        /* Make it easier to find the linkhandler when debugging. */

        if (pthread_setname_np(lnh->thread, name) < 0) {
                lnh_error("pthread_setname_np failed:%d(%s)",
                          errno, strerror(errno));
        }

        msg = itc_receive(lnh_init_filter, 5000, ITC_FROM_ALL);
        if (msg) {
                start_done = msg->msg_no;
                itc_free(&msg);
        }

        if (start_done != 1) {
           pthread_join(lnh->thread, NULL);
           free(lnh);
           return NULL;
        }
        return lnh;
}

struct ulh_lnh *ulh_lnh_create(const char *name)
{
        return ulh_lnh_create_w_cfg(name, 64, 128);
}

itc_mbox_id_t ulh_lnh_getmbox(struct ulh_lnh *lnh)
{
        return lnh->mbox;
}

void ulh_lnh_destroy(struct ulh_lnh *lnh)
{
        union itc_msg *msg;

        if (!lnh)
        	return;

        msg = itc_alloc(sizeof(struct ulh_lnhmsg_shutdown),
        	        ULH_LNHMSG_SHUTDOWN);
        itc_send(&msg, lnh->mbox, ITC_MY_MBOX);
        pthread_join(lnh->thread, NULL);
        free(lnh);

}

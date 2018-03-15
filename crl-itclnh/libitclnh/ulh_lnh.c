/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014-2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
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

#ifdef LTTNG
#define TRACEPOINT_DEFINE
#include "itclnh_lttng.h"
#else
#define tracepoint(...)
#endif

#define	FNV32_INIT	 				0x811c9dc5
#define	FNV32_PRIM	 				0x01000193

#define HTAB_SIZE					8192
#define HTAB_MASK					(HTAB_SIZE - 1)

#define ULH_LNHLINKRECONNECT_TMO	1000

#define STATE_CONNECTING			0
#define STATE_INITIATING			1
#define STATE_UP					2
#define STATE_DISCONNECTED			3

#define	FREE_SLOT					((uint16_t)0xffff)
#define	ZOMBIE_SLOT					((uint16_t)0xfffe)

#define MAX_INFO_PER_MSG 			2048
#define LNH_INIT_SUCCESS 			0
#define LNH_INIT_FAILURE 			1

#define ULH_THROUGHPUT_TMO			1000
/* throughput info per each link is 512 bytes */
#define MAX_THROUGHPUT_INFO_PER_MSG		1024 * 16

#define ITCLNH_MON_NO_ID         ITC_NO_ID
#define POSSIBLE_LOCATES         (bool)true
#define NO_LOCATES               (bool)false

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



#define ASYNC_QUERY_REPLY			(0xbabecafe)
struct query_reply {
        uint32_t      				msgno;
        int32_t       				linkid;
        char          				name[1];
};

struct ulnh_rep {
	struct dl_list					link;
	struct ulnh_link 	   			*lref;
	itc_mbox_id_t 					mbox;
	uint32_t 						laddr;
	uint64_t			 			tx_msg;
	uint64_t			 			rx_msg;
	uint32_t			 			mbox_hash;
	uint32_t			 			addr_hash;
};

struct ulnh_lep {
	itc_mbox_id_t 					mbox;
	uint32_t 						mbox_hash;
	uint64_t			 			tx_msg;
	uint64_t			 			rx_msg;
	uint64_t			 			links;
	uint16_t 						laddr[64];
};

struct ulh_locate {
	struct dl_list 					link;
        itc_mbox_id_t 					lep_mbox_id;
	char						name[ULH_LNHNAMESIZ];
};

struct ulh_mtor {
	struct dl_list 					link;
        itc_monitor_id_t 			        mtor_id;
        itc_mbox_id_t                                    mbox_id;
};

struct ulnh_link {
	uint32_t               lid;
	uint32_t               state;
	struct ulh_ref         ref;
	struct ulh_cm_instance conn;
	struct ulh_lnh         *lnh;
	struct dl_list         locs;
	struct dl_list         reps;
	uint32_t               prio;
	uint32_t               zombies;
	struct ulh_timer       reconn_tmo;
	struct ulh_timer       reaper_tmo;
	itc_monitor_id_t       mtor;
	itc_mbox_id_t          owner;
	uint16_t               *amap;
	uint8_t                filter_queries;
	uint32_t               allowed_queries_size;
	struct ulh_query       *allowed_queries;
	char                   name[1];
};

struct ulh_diag_config {
	/* Should mailbox watermark and message types be tracked */
	bool						diagnose;
	/* Minimal watermark before reporting watermark changes */
	int 						mailbox_watermark_minimum;
	/* Number of received messages between before checking watermark */
	int 						mailbox_watermark_delay;
	/* Number of received messages before reporting how many RHAI and
	 * application messages there was */
	uint32_t						mailbox_msg_type_delay;
};

struct ulh_diag_state {
	/* Mailbox watermark check delay counter */
	int 						mailbox_countdown;
	/* Longest mailbox message queue so far */
	int 						mailbox_watermark;
	/* Number of RHAI messages processed so far*/
	uint32_t						rhai_messages;
	/* Number of application messages processed so far*/
	uint32_t						app_messages;
};

struct ulh_lnh {
	itc_mbox_id_t 					mbox;
	struct ulh_timerqueue 			tqueue;
        struct dl_list  				mtors;
	struct ulnh_link 				**links;
	struct ulnh_lep					**lep_mbox_hash;

	struct ulnh_rep					**rep_mbox_hash;
	struct ulnh_rep				**rep_addr_hash;

	uint32_t 						max_endp;
	uint32_t 						max_link;

	itc_mbox_id_t 					parent;
	pthread_t 						thread;
	bool                                            enable_thrput;
	struct ulh_timer 				thrp_timer;
	uint32_t						thrput_tmo;

	struct ulh_diag_config 			diag_config;
	struct ulh_diag_state 			diag;

	/* Keep it as a last element, struct memory is allocated to fit the
	 * name length and the name is written in those extra bytes.
	 */
	char 							name[1];
};


union itc_msg {
	uint32_t                          msg_no;
	struct itc_locate_lnh             locate_lnh;
	struct itc_locate_lnh_reply       locate_lnh_reply;
	struct query_reply                query_reply;
	struct ulh_lnhmsg_notify          notify;
	struct ulh_lnhmsg_shutdown        shut;
	struct ulh_lnhmsg_query           query;
	struct ulh_transmsg_data          data;
	struct ulh_lnhmsg_createcm_req    createcm_req;
	struct ulh_lnhmsg_createcm_rsp    createcm_rsp;
	struct ulh_lnhmsg_createlink_req  createlink_req;
	struct ulh_lnhmsg_createlink_rsp  createlink_rsp;
	struct ulh_lnhmsg_createlink2_req createlink2_req;
	struct ulh_lnhmsg_createlink2_rsp createlink2_rsp;
	struct ulh_lnhmsg_destroylink_req destroylink_req;
	struct ulh_lnhmsg_destroylink_rsp destroylink_rsp;
	struct ulh_lnhmsg_info_req        info_req;
	struct ulh_lnhmsg_info_rsp        info_rsp;
	struct ulh_lnhmsg_enable_diag     enable_diag;
	struct ulh_cmmsg_general          cm_msg;
};


static int tx_rlnh_unpublish(struct ulnh_link*, struct ulnh_lep*);

static void for_each_link_in_lep(struct ulh_lnh *lnh,
                                struct ulnh_lep *lep,
                                int (*cb)(struct ulnh_link *link,
                                          struct ulh_lnh *lnh,
                                          struct ulnh_lep *lep));


static char *state_to_string[] = { "RLNH_CONNECTING  ",
				   "RLNH_INITIATING  ",
				   "RLNH_UP          ",
				   "RLNH_DISCONNECTED" };


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
	int cnt, idx = hash32(key);

	for (cnt = 0; cnt < HTAB_SIZE; cnt++)
		if ((p = lnh->lep_mbox_hash[(idx + cnt) & HTAB_MASK]))
			if (p->mbox == (itc_mbox_id_t)key)
				return p;

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
	int cnt, idx = hash32(key);

	for (cnt = 0; cnt < HTAB_SIZE; cnt++)
		if ((p = lnh->rep_mbox_hash[(idx + cnt) & HTAB_MASK]))
			if (p->mbox == (itc_mbox_id_t)key)
				return p;
	return NULL;
}

static struct ulnh_rep *get_rep_by_addr(struct ulnh_link *link,
						uint32_t addr)
{
	struct ulh_lnh *lnh = link->lnh;
	struct ulnh_rep *p;
	int cnt, idx = hash32((uint32_t)(uintptr_t)link ^ addr);

	for (cnt = 0; cnt < HTAB_SIZE; cnt++)
		if ((p = lnh->rep_addr_hash[(idx + cnt) & HTAB_MASK]))
			if ((p->laddr == addr) && (p->lref == link))
				return p;
	return NULL;
}


static struct ulh_locate *get_locate(struct ulnh_link *lnk,
                                     const char *name,
                                     itc_mbox_id_t lep_mbox_id)
{
	struct ulh_locate *loc;
	dl_list_foreach(loc, &lnk->locs, link) {

           if (name == NULL) {
              if (loc->lep_mbox_id == lep_mbox_id)
                 return loc;
           } else if (!strcmp(loc->name, name) &&
                      ((loc->lep_mbox_id == lep_mbox_id) ||
                       lep_mbox_id == ITC_NO_ID))
              return loc;
	}

	return NULL;
}

struct ulh_mtor * get_monitor(struct ulh_lnh *lnh,
                              itc_mbox_id_t mbox_id)
{
   struct ulh_mtor *mtor;

   dl_list_foreach(mtor, &lnh->mtors, link){

      if (mbox_id == mtor->mbox_id)
         return mtor;
	}

   return NULL;
}

static int add_monitor(struct ulh_lnh *lnh, itc_mbox_id_t mbox_id)
{

   struct ulh_mtor *mtor;

   mtor = malloc(sizeof(*mtor));
   if (!mtor)
      return 1;

   memset(mtor, 0, sizeof(*mtor));
   mtor->mtor_id = itc_monitor(mbox_id, NULL);
   mtor->mbox_id = mbox_id;

   dl_list_init(&mtor->link);
   dl_list_insert_tail(&lnh->mtors, &mtor->link);

   return 0;
}

static void remove_monitor(struct ulh_lnh *lnh, itc_mbox_id_t mbox_id)
{
   struct ulh_mtor *mtor;

   mtor = get_monitor(lnh, mbox_id);
   if (mtor)
   {
      dl_list_remove(&mtor->link);
      itc_unmonitor(mtor->mtor_id);
      free(mtor);
   }
}


static void add_locate(struct ulnh_link *link,
                       const char* locate_name,
                       itc_mbox_id_t owner_mbox_id)
{
   struct ulh_locate *loc = get_locate(link,
                                       locate_name,
                                       owner_mbox_id);

   if (loc != NULL)
      return;

   tracepoint(com_ericsson_itclnh, add_locate,
              link->name, locate_name, (uint32_t)owner_mbox_id);

      loc = malloc(sizeof(*loc));
      loc->lep_mbox_id = owner_mbox_id;
      strncpy(loc->name, locate_name, ULH_LNHNAMESIZ);
      dl_list_init(&loc->link);
      dl_list_insert_tail(&link->locs, &loc->link);
}

static void
remove_locates_from_links_by_mbox(struct ulh_lnh *lnh, itc_mbox_id_t mbox_id)
{
   struct ulnh_link *link;
   int idx;
   struct ulh_locate *loc, *loc_temp;

   for (idx = 0; idx < lnh->max_link; idx++)
   {
      if ((link = lnh->links[idx]))
      {
         dl_list_foreach_safe(loc, loc_temp, &link->locs, link) {

            if (loc->lep_mbox_id == mbox_id)
            {
               tracepoint(com_ericsson_itclnh, remove_locate,
                          link->name, loc->name, (uint32_t)mbox_id);
               dl_list_remove(&loc->link);
               free(loc);
            }
         }
      }
   }
}

static void
remove_locates_from_link_by_name(struct ulnh_link *link, const char* name)
{
   struct ulh_locate *loc, *loc_temp;

   dl_list_foreach_safe(loc, loc_temp, &link->locs, link) {

      if (!strcmp(loc->name, name))
      {
         tracepoint(com_ericsson_itclnh, remove_locate,
                    link->name, loc->name, (uint32_t)0);
         dl_list_remove(&loc->link);
         free(loc);
      }
   }
}

static struct ulh_locate *
check_locates_on_all_links(struct ulh_lnh *lnh, itc_mbox_id_t mbox_id)
{
   struct ulnh_link *link;
   int idx;
   struct ulh_locate *loc = NULL;

   for (idx = 0; idx < lnh->max_link; idx++)
   {
      if ((link = lnh->links[idx]))
      {
         if((loc = get_locate(link, NULL, mbox_id)))
            return loc;
      }
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
	if (link->allowed_queries)
		free(link->allowed_queries);
	free(link->amap);
	free(link);
}

static struct ulnh_link *alloc_link(struct ulh_lnh *lnh, const char *lname,
                                    uint8_t filter_queries,
                                    uint32_t allowed_queries_size,
                                    struct ulh_query *allowed_queries)
{
	uint32_t lid = (uint32_t) -1, i, size;
	char name[ULH_LNHNAMESIZ];
	struct ulnh_link *link;

	for (i = 0; lname[i] && (i < ULH_LNHNAMESIZ); i++) {
		if ((name[i] = lname[i]) == '/')
			break;
	}
	name[i] = 0;

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

	link->filter_queries = filter_queries;

	if (filter_queries && allowed_queries_size) {
		link->allowed_queries_size = allowed_queries_size;

		size = link->allowed_queries_size * sizeof(struct ulh_query);
		link->allowed_queries = malloc(size);
		if (!link->allowed_queries) {
			free(link->amap);
			free(link);
			return NULL;
		}

		for (i = 0; i < link->allowed_queries_size; i++)
			snprintf(link->allowed_queries[i].name, ITC_NAME_MAXLEN,
			         "%s", allowed_queries[i].name);
	}

	dl_list_init(&link->reps);
	dl_list_init(&link->locs);

	link->owner = ITC_NO_ID;
	link->lid = lid;
	link->lnh = lnh;
	lnh->links[lid] = link;

	return link;
}

static void free_lep(struct ulh_lnh *lnh,
                     struct ulnh_lep *lep,
                     bool possible_lep_locates)
{
   /* check if there is a still unresolved locates for this lep on any of the
      created links. If not we don't need to monitor the lep client. */
   if (possible_lep_locates)
   {
      if (!check_locates_on_all_links(lnh, lep->mbox))
      {
         remove_monitor(lnh, lep->mbox);
      }
   }
   else
      remove_monitor(lnh, lep->mbox);

   if (lep->mbox_hash < HTAB_SIZE)
      lnh->lep_mbox_hash[lep->mbox_hash] = NULL;

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
		if (link->amap[cnt] < (uint32_t)ZOMBIE_SLOT) {
			lep = lnh->lep_mbox_hash[link->amap[cnt]];

			if (lep && (lep->links & bit)) {
				lep->links &= ~bit;

				tx_rlnh_unpublish(link, lep);

				if (lep->links == 0)
                                   free_lep(lnh, lep, POSSIBLE_LOCATES);
			}
		}
		link->amap[cnt] = FREE_SLOT;
	}

	dl_list_foreach_safe(rep, rep_tmp, &link->reps, link)
		free_rep(lnh, rep);

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

   tracepoint(com_ericsson_itclnh, link_drop,
              link->name, link->state);

	if (link->state == STATE_UP) {
		lnh_info("[LNH] deassigning linkhandler %s\n",link->name);
		itc_deassign_linkhandler(link->name, link->lnh->mbox);

		if (reconnect)
			notify_owner(link, ULH_LINK_DOWN);
	}

	free_endpoints(link);

	if (link->state != STATE_DISCONNECTED) {
		link->conn.ops->dc_disconnect(
				link->conn.instance, link->prio);
	}
	link->state = STATE_DISCONNECTED;

	if (reconnect)
		ulh_timer_arm(&link->reconn_tmo, &link->lnh->tqueue,
				ULH_LNHLINKRECONNECT_TMO);
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

static int tx_rlnh_unpublish(struct ulnh_link *link,
                             struct ulnh_lep *lep)
{
	union itc_msg *buf;
	struct rlnh_msg_unpublish *msg;

	buf = itc_alloc(sizeof(*msg), 0);
	msg = (struct rlnh_msg_unpublish *) buf;

	msg->hdr.reserved = 0;
	msg->hdr.type = htons(RLNH_UNPUBLISH);
	msg->laddr = htonl((uint32_t)lep->laddr[link->lid]);

        tracepoint(com_ericsson_itclnh, tx_rlnh_unpublish,
                   link->name, lep->laddr[link->lid]);

	tx_rlnh(link, buf, sizeof(*msg));
	itc_free(&buf);
	return 0;
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

static int tx_rlnh_publish(struct ulnh_link *link,
                           const char *name,
                           struct ulnh_lep *lep)
{
	struct rlnh_msg_publish *msg;
	union itc_msg *buf;
	int len;

	len = strlen(name);
	buf = itc_alloc((sizeof(*msg) + len), 0);
	msg = (struct rlnh_msg_publish *) buf;

	msg->hdr.reserved = 0;
	msg->hdr.type = htons(RLNH_PUBLISH);
	msg->laddr = htonl(lep->laddr[link->lid]);

        tracepoint(com_ericsson_itclnh, tx_rlnh_publish,
                   (char*)name, link->name, lep->laddr[link->lid]);

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

	str_len = strlen(name) + 1;
	size = (sizeof(*msg) + str_len + 3) & ~0x3;

	buf = itc_alloc(size, 0);
	msg = (struct rlnh_msg_queryname *) buf;

	msg->hdr.reserved = 0;
	msg->hdr.type = htons(RLNH_QUERY_NAME);
	msg->laddr = htonl(lep->laddr[link->lid]);

        tracepoint(com_ericsson_itclnh, tx_rlnh_query, (char*)name, link->name);

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

	size = itc_size(msg);
	msg->msg_no = htonl(msg->msg_no);

	lhhdr.size = size;
	lhhdr.src = lep->laddr[link->lid];
	lhhdr.dst = rep->laddr;

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

static void publish_lep( struct ulnh_link *link,
                         struct ulnh_lep **lepp,
                         itc_mbox_id_t mbox)
{
	struct ulh_lnh *lnh = link->lnh;
	struct ulnh_lep *lep = *lepp;
	uint64_t bit = 0x8000000000000000 >> link->lid;
	char publish_name[ITC_NAME_MAXLEN];
	uint32_t addr;

	if (lep && (lep->links & bit))
		return;

	if (!itc_get_name(mbox, publish_name, sizeof(publish_name))) {
		lnh_warn("LNH: publish (itc_get_name) mbox 0x%x failed on %s\n",
				mbox, link->name);
		*lepp = NULL;
		return;
	}

	if (lep == NULL) {
		lep = malloc(sizeof(*lep));
		memset(lep, 0, sizeof(*lep));
		lep->mbox = mbox;
		lep->mbox_hash = add_to_htab((void**)lnh->lep_mbox_hash,
						lep, mbox);
		if ((int)lep->mbox_hash < 0) {
			lnh_error("LNH: publish %s on %s - LEP hash full\n",
				       	publish_name, link->name);
			free(lep);
			return;
		}

                /* If the lep has a unresolved locates before we
                   still have his monitor */
                if (!get_monitor(lnh, mbox))
                {
                   if(add_monitor(lnh, mbox))
                   {
                      lnh_error("LNH: failed to make monitor, link %s mbox %s",
                                link->name, publish_name);
                      return;
                   }
                }
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
		else
                   free_lep(lnh, lep, POSSIBLE_LOCATES);

		return;
	}
	*lepp = lep;

	link->amap[addr] = (uint16_t)lep->mbox_hash;
	lep->laddr[link->lid] = addr;
	lep->links |= bit;
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

        tracepoint(com_ericsson_itclnh, rx_rlnh_init, link->name);

	notify_owner(link, ULH_LINK_UP);

	lep = get_lep_by_id(lnh, lnh->mbox);
	publish_lep(link, &lep, lnh->mbox);
	if (lep == NULL) {
		lnh_error("LNH: rx_rlnh_init_reply for %s, publish lep failed\n",
				link->name);
		link_drop(link, 1);
		return;
	}

	/* re-query incomplete remotes */
        /* if the link was disconnected at this point we should have
           removed all reps and leps and unpublish them to the remote side.
           That means that all reps are removed on remote side a well
           and all unresolved hunts should have been also
           removed on remote so we must send them again. */
	dl_list_foreach(loc, &link->locs, link) {
           lep = get_lep_by_id(lnh, loc->lep_mbox_id);
           publish_lep(link, &lep, loc->lep_mbox_id);

           if (lep == NULL) {
              lnh_error("LNH: publish lep failed , locate %s , link %s",
                        loc->name, link->name);
           }
           else
           {
              tx_rlnh_query_name(link, lep, loc->name);
           }
	}
}


static int alloc_rep(struct ulh_lnh *lnh,
                     struct ulnh_link *link,
                     struct ulnh_rep **rep_out,
                     uint32_t remote_addr,
                     const char* remote_mbox_name)
{
   struct ulnh_rep *new_rep = malloc(sizeof(*new_rep));

   if (!new_rep)
      return 1;

   *rep_out = new_rep;

   memset(new_rep, 0, sizeof(*new_rep));
   new_rep->laddr = remote_addr;
   new_rep->lref = link;
   new_rep->mbox = itc_clone_mailbox(lnh->mbox, remote_mbox_name);
   if (new_rep->mbox == ITC_NO_ID)
   {
      lnh_error("[LNH] itc_clone_mailbox(%s) failed",
                remote_mbox_name);
      free(new_rep);
      return 1;
   }

   return 0;
}

static int store_rep(struct ulh_lnh *lnh,
                     struct ulnh_link *link,
                     struct ulnh_rep *rep)
{
   rep->mbox_hash = add_to_htab((void**)lnh->rep_mbox_hash,
                                rep, rep->mbox);

   if ((int)rep->mbox_hash < 0)
   {
      lnh_error("LNH: REP hash full (mhash: %d) "
                "Failed to publish remote", (int)rep->mbox_hash);
      goto out_err;
   }

   rep->addr_hash = add_to_htab((void**)lnh->rep_addr_hash,
                                rep, rep->laddr ^ (uint32_t)(uintptr_t)rep->lref);

   if ((int)rep->addr_hash < 0)
   {
      lnh_error("LNH: REP addr hash full (ahash: %d). "
                "Failed to publish remote", (int)rep->addr_hash);
      goto out_err;
   }

   /* Everything is ok, add remote end point to the list */
   dl_list_init(&rep->link);
   dl_list_insert_tail(&link->reps, &rep->link);

   return 0;

out_err:
   itc_delete_mailbox(rep->mbox);
   free(rep);
   return 1;
}

static void rx_rlnh_publish(struct ulnh_link *link,
		struct rlnh_msg_publish *msg)
{
	char name[ITC_NAME_MAXLEN];
	struct ulh_lnh *lnh = link->lnh;
	uint32_t laddr = ntohl(msg->laddr);
	struct ulnh_rep *rep;
        struct ulh_locate *loc;

	snprintf(name, sizeof(name), "%s/%s", link->name, msg->name);

        tracepoint(com_ericsson_itclnh, rx_rlnh_publish,
                   msg->name, laddr, link->name);

        if (alloc_rep(lnh, link, &rep, laddr, name))
           return;

        (void)store_rep(lnh, link, rep);

        remove_locates_from_link_by_name(link, msg->name);
}

static bool allow_query(const struct ulnh_link *link, char *path)
{
	char *name;
	unsigned int i;

	if (!link->filter_queries)
		return true;

	if (link->allowed_queries_size) {
		name = strrchr(path, '/');
		name = name ? name + 1 : path;

		for (i = 0; i < link->allowed_queries_size; i++)
			if (!strcmp(link->allowed_queries[i].name, name))
				return true;
	}

	return false;
}

static void rx_rlnh_query(struct ulnh_link *link,
					struct rlnh_msg_queryname *msg)
{
	struct ulnh_lep *lep;
        struct ulnh_rep *rep;
	itc_mbox_id_t mbox;
	union itc_msg *loc;
        uint32_t laddr = ntohl(msg->laddr);
	int size;

        tracepoint(com_ericsson_itclnh, rx_rlnh_query, msg->name, link->name);

	if (!allow_query(link, msg->name)) {
		lnh_error("[LNH] discarding query %s on link %s", msg->name, link->name);
		return;
	}

        rep = get_rep_by_addr(link, laddr);
        if (!rep)
        {
           lnh_error("[LNH] received QUERY from unpublished remote user: addr %d, query %s, link name %s",
                     laddr, msg->name, link->name);
           return;
        }

	mbox = itc_locate(msg->name);
	if (mbox != ITC_NO_ID) {
		lep = get_lep_by_id(link->lnh, mbox);
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

        /* We are using here locate with "from" mailbox of cloned remote end point.
           We have to do this to make sure that when remote client who made the hunt
           dies all his hunts are removed in ITC localy as well. And that will
           happen automatically when we delete his cloned mailbox.  */
	itc_locate_async(msg->name, &loc, rep->mbox);
}

static void rx_rlnh_unpublish(struct ulnh_link *link,
		struct rlnh_msg_unpublish *msg)
{
	struct ulnh_rep *rep;
	uint32_t laddr = ntohl(msg->laddr);

	rep = get_rep_by_addr(link, laddr);
	if (!rep) {
		lnh_info("LNH: %s: UNPUBLISH for unknown laddr:0x%x\n",
				link->name, laddr);
		return;
	}

        tracepoint(com_ericsson_itclnh, rx_rlnh_unpublish,
                   link->name, laddr);

	free_rep(link->lnh, rep);
	tx_rlnh_unpublish_ack(link, laddr);
}

static void rx_rlnh_unpublish_ack(struct ulnh_link *link,
		struct rlnh_msg_unpublish_ack *msg)
{
	uint32_t laddr = ntohl(msg->laddr);
        if(link->amap[laddr] == ZOMBIE_SLOT) {
	        link->amap[laddr] = FREE_SLOT;
        	link->zombies--;
        }
        else if(link->amap[laddr] != FREE_SLOT)
                lnh_warn("LNH: Unpublish ack received on %s for slot which is already occupied "
                               "slot:0x%x laddr:0x%x\n",
                               link->name, link->amap[laddr], laddr);

	if (!link->zombies)
		ulh_timer_cancel(&link->reaper_tmo);
}

#define TEST_QUERY_NAME		10
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
	case TEST_QUERY_NAME:
		rx_rlnh_query(link, (struct rlnh_msg_queryname *) hdr);
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

	msg->msg_no = ntohl(msg->msg_no);

	lep = get_lep_by_addr(link, dst);
	rep = get_rep_by_addr(link, src);

	if (lep && rep) {
		lep->rx_msg++;
		rep->tx_msg++;
		itc_send(&msg, lep->mbox, rep->mbox);
		return;
	}

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

        tracepoint(com_ericsson_itclnh, link_connect_state,
                   link->name, link->state);

	if (link->state != STATE_CONNECTING)
		return;

	link->state = STATE_INITIATING;
	tx_rlnh_init(link, RLNH_VERSION);
}

static void uc_lnh_disconnected(void *handle)
{
	struct ulnh_link *link = handle;

        tracepoint(com_ericsson_itclnh, link_disconnect_state,
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
		ulh_timer_arm(&link->reconn_tmo, &link->lnh->tqueue,
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

static int create_link(struct ulh_lnh *lnh, itc_mbox_id_t owner,
                       union itc_msg *cmreq, const char *name, uint32_t prio,
                       uint8_t filter_queries, uint32_t allowed_queries_size,
                       struct ulh_query allowed_queries[], uint32_t *lid)
{
	union itc_msg *mon;
	struct ulh_cm_config *cfg;
	struct ulnh_link *link;
	int ret;

	cfg = (struct ulh_cm_config *)&cmreq->createcm_req.config;

	link = alloc_link(lnh, name, filter_queries,
	                  allowed_queries_size, allowed_queries);
	if (!link) {
		ret = -ENOMEM;
		goto create_err;
	}

	cfg->mbox = lnh->mbox;
	cfg->max_itc_msg_size = itc_get_max_size();
	cfg->uref = link->lid;

	ret = ulh_cm_create_instance(cmreq->createcm_req.cm_name,
			cmreq->createcm_req.cm_instance, &link->conn,
			cfg, &lnh->tqueue);

	if (ret) {
		lnh_error("[LNH] ulh_cm_create_instance: %d\n", ret);
		if (link->allowed_queries)
			free(link->allowed_queries);
		free(link->amap);
		free(link);
		goto create_err;
	}

	ulh_init_ref(&link->ref, 1, free_link);
	link->prio = prio;
	ret = link->conn.ops->dc_init(link->conn.instance,
			&lnh_uc_ops, link, link->prio);

	for (; ret == EAGAIN;) {
		sleep(1);
		lnh_error("[LNH] retry on dc_init\n");
		ret = link->conn.ops->dc_init(link->conn.instance,
			&lnh_uc_ops, link, link->prio);
	}

	if (ret) {
		lnh_error("[LNH] dc_init: %d\n", ret);
		goto init_err;
	}

	link->state = STATE_CONNECTING;
	link->owner = owner;
	ulh_timer_init(&link->reconn_tmo, lnh_link_reconnect_tmo, link);
	ulh_timer_init(&link->reaper_tmo, lnh_link_reaper_tmo, link);

	ret = link->conn.ops->dc_connect(link->conn.instance, link->prio);
	if (ret) {
		lnh_error("[LNH] dc_connect: %d\n", ret);
 		goto init_err;
	}

	mon = itc_alloc(sizeof(uint32_t), ULH_LNHMSG_OWNER_DEAD);
	link->mtor = itc_monitor(link->owner, &mon);

	itc_free(&cmreq);
	*lid = link->lid;
	return 0;

init_err:
	ulh_unhold_ref(&link->ref);

create_err:
	lnh_error("[LNH] create_link: %d\n", ret);

	itc_free(&cmreq);
	*lid = 0;
	return ret;
}

static void handle_createlink(struct ulh_lnh *lnh, union itc_msg *msg)
{
	union itc_msg *cmreq, *rsp;
	uint32_t lid;
	int ret;

	cmreq = (union itc_msg *)(uintptr_t)msg->createlink_req.cmid;

	ret = create_link(lnh, itc_sender(msg), cmreq, msg->createlink_req.name,
	                  msg->createlink_req.prio, false, 0, NULL, &lid);

	rsp = itc_alloc(sizeof(struct ulh_lnhmsg_createlink_rsp),
	                ULH_LNHMSG_CREATELINK_RSP);

	rsp->createlink_rsp.seq = msg->createlink_req.seq;
	rsp->createlink_rsp.lid = lid;
	rsp->createlink_rsp.result = ret;

	itc_send(&rsp, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_createlink2(struct ulh_lnh *lnh, union itc_msg *msg)
{
	union itc_msg *cmreq, *rsp;
	uint32_t lid;
	int ret;

	cmreq = (union itc_msg *)(uintptr_t)msg->createlink2_req.cmid;

	ret = create_link(lnh, itc_sender(msg), cmreq, msg->createlink2_req.name,
	                  msg->createlink2_req.prio,
	                  msg->createlink2_req.filter_queries,
	                  msg->createlink2_req.allowed_queries_size,
	                  msg->createlink2_req.allowed_queries, &lid);

	rsp = itc_alloc(sizeof(struct ulh_lnhmsg_createlink2_rsp),
	                ULH_LNHMSG_CREATELINK2_RSP);

	rsp->createlink2_rsp.seq = msg->createlink2_req.seq;
	rsp->createlink2_rsp.lid = lid;
	rsp->createlink2_rsp.result = ret;

	itc_send(&rsp, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_transdata(struct ulh_lnh *lnh,
		union itc_msg *msg)
{
	struct ulnh_link *link = NULL;

	if (msg->data.uref < lnh->max_link)
		link = lnh->links[msg->data.uref];

	if (!link) {
		lnh_warn("[LNH] data for non-existing LID: %lu\n",
							   msg->data.uref);
		ulh_tbuff_free(&msg->data.data);
		return;
	}

	if (link->conn.ops->dc_receive)
		link->conn.ops->dc_receive(link->conn.instance,
				msg->data.cid, &msg->data.data);
}

static void handle_locate(struct ulh_lnh *lnh,
                          union itc_msg *msg)
{
	char link_name[ULH_LNHNAMESIZ], *dst = link_name, *src;
	struct ulnh_lep *lep;
	struct ulnh_link *link;
	itc_mbox_id_t locate_owner_mbox = msg->locate_lnh.from;
	int idx;

        tracepoint(com_ericsson_itclnh, handle_locate,
                   lnh->name, msg->locate_lnh.name, (uint32_t)locate_owner_mbox);

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


        /* There is a possibility that ITC queues the locate message but
           in the mean time link changes the state. That is why we have to save the
           locate because otherwise nothing will resend it unless all hdlc links
           disconnect and connect again which is unlikely. */
	if (link->state != STATE_UP)
        {
           if (!get_monitor(lnh, locate_owner_mbox))
           {
              if(add_monitor(lnh, locate_owner_mbox))
              {
                 lnh_error("LNH: failed to make monitor on link %s, mbox %d",
                           link->name, locate_owner_mbox);
                 return;
              }
           }

           add_locate(link, ++src, locate_owner_mbox);
           return;
        }

           lep = get_lep_by_id(lnh, locate_owner_mbox);
           publish_lep(link, &lep, locate_owner_mbox);
           if (lep == NULL) {
              lnh_error("LNH: locate for %s, publish lep failed\n",
                        msg->locate_lnh.name);
              return;
           }

           if (itc_locate(msg->locate_lnh.name) == ITC_NO_ID)
           {
              add_locate(link, ++src, lep->mbox);
              tx_rlnh_query_name(link, lep, src);
           }
}

static int shutdown_link(struct ulh_lnh *lnh, struct ulnh_link *link)
{
	struct ulh_locate *loc, *loc_tmp;

        tracepoint(com_ericsson_itclnh, shutdown_link,
                   link->name);

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
        struct ulh_mtor * mtor, *temp_mtor;

        tracepoint(com_ericsson_itclnh, lnh_shutdown_req,
                   lnh->name, (uint32_t)lnh->mbox);

	for (i = 0; i < lnh->max_link; i++) {
		if (lnh->links[i])
			shutdown_link(lnh, lnh->links[i]);
	}

	for (i = 1; i < HTAB_SIZE; i++)
		if (lnh->lep_mbox_hash[i]) {
			free(lnh->lep_mbox_hash[i]);
			lnh->lep_mbox_hash[i] = NULL;
		}

        dl_list_foreach_safe(mtor, temp_mtor, &lnh->mtors, link){
           dl_list_remove(&mtor->link);
           itc_unmonitor(mtor->mtor_id);
           free(mtor);
	}

	free(lnh->links);
	free(lnh->lep_mbox_hash);
	free(lnh->rep_mbox_hash);
	free(lnh->rep_addr_hash);

	ulh_timerqueue_destroy(&lnh->tqueue);
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


/* Call given callback for all links on which lep is published.
   If callback returns 1 exit the loop */
static void for_each_link_in_lep(struct ulh_lnh *lnh,
                                 struct ulnh_lep *lep,
                                 int (*cb)(struct ulnh_link *link,
                                           struct ulh_lnh *lnh,
                                           struct ulnh_lep *lep))
{
   uint32_t lnk, lzw;

   for (lnk = 0; lep->links; lnk++)
   {
      lzw = __builtin_clzll(lep->links);
      lnk += lzw;
      lep->links <<= (lzw + 1);
      cb(lnh->links[lnk], lnh, lep);
   }
}

/* Remove all unresolved locates associated
   with the client that has just died.  */
static int handle_lep_dead_cb(struct ulnh_link *link,
                              struct ulh_lnh *lnh,
                              struct ulnh_lep *lep)
{
   if (link)
   {
        tx_rlnh_unpublish(link, lep);
        link->amap[lep->laddr[link->lid]] = ZOMBIE_SLOT;

        ulh_timer_arm(&link->reaper_tmo, &lnh->tqueue, 2000);
        link->zombies++;
   }

   return 0;
}

static void handle_lep_dead(struct ulh_lnh *lnh,
                            union itc_msg *msg)
{
   itc_mbox_id_t died_mbox_id = itc_sender(msg);
   struct ulnh_lep *lep = get_lep_by_id(lnh, died_mbox_id);

   /* We removed it from the hash table and then lep died.
      Now we have to clean all his locates.  */
   remove_locates_from_links_by_mbox(lnh, died_mbox_id);

   if (lep)
   {
      for_each_link_in_lep(lnh, lep, handle_lep_dead_cb);
      free_lep(lnh, lep, NO_LOCATES);
   }
   else
      remove_monitor(lnh, died_mbox_id);
}

static void handle_lep_found(struct ulh_lnh *lnh,
					union itc_msg *msg)
{
	struct ulnh_lep *lep = get_lep_by_id(lnh, itc_sender(msg));
	struct ulnh_link *link = lnh->links[msg->query_reply.linkid];

	if (link && (link->state == STATE_UP)) {
           publish_lep(link, &lep, itc_sender(msg));
           if (lep == NULL) {
              lnh_error("LNH: lep found for %s but publish lep failed on %s\n",
                        msg->query_reply.name, link->name);
           }
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

static void handle_diag(struct ulh_lnh *lnh,
					union itc_msg *msg)
{
	lnh->diag_config.diagnose = true;
	lnh->diag_config.mailbox_watermark_delay = msg->enable_diag.mailbox_watermark_delay;
	lnh->diag_config.mailbox_msg_type_delay = msg->enable_diag.mailbox_msg_type_delay;
	lnh->diag_config.mailbox_watermark_minimum = msg->enable_diag.mailbox_watermark_minimum;
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

	if (buf_remain > 0 && lnh->diag_config.diagnose) {
		snprintf(tmp, buf_remain, "MBox watermark: %d\nRHAI messages: %u\nApp messages: %u\n",
		         lnh->diag.mailbox_watermark,
		         lnh->diag.rhai_messages,
		         lnh->diag.app_messages);
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
		if ((lep = lnh->lep_mbox_hash[i]) == NULL)
			continue;

		itc_get_name(lep->mbox, ep_name, sizeof(ep_name));
		if (buf_remain < 128) {
			send_info(buf, 0, 0, ret_id);
			tmp = buf;
			buf_remain = MAX_INFO_PER_MSG;
		}

		len = snprintf(tmp, buf_remain,"%4d 0x%08x 0x%016llx"
				" %14llu %14llu  %s\n", lep->mbox_hash, lep->mbox,
				lep->links, lep->tx_msg, lep->rx_msg, ep_name);

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
        ulh_timer_arm(&lnh->thrp_timer, &lnh->tqueue, lnh->thrput_tmo);
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
        ulh_timer_arm(&lnh->thrp_timer, &lnh->tqueue, lnh->thrput_tmo);
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
                                                       1, -1, ret_id);
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

static void handle_cm_msg(struct ulh_lnh *lnh, union itc_msg *msg)
{
	uint32_t lid = msg->cm_msg.link_id;
	struct ulh_cm_instance *cmi;
	int ret = 0;

	if (lid >= lnh->max_link) {
		lnh_error("[LNH] wrong link id %d in CM msg from 0x%x\n",
			lid, itc_sender(msg));
		return;
	}

	if (!lnh->links[lid]) {
		lnh_error("[LNH] non-existing link id %d in CM msg from 0x%x\n",
			lid, itc_sender(msg));
		return;
	}

	cmi = &lnh->links[lid]->conn;

	if (!cmi->cm_ref) {
		ret = -EINVAL;
		goto end;
	}

	if (!cmi->ops->dc_handle_cm_msg) {
		ret = -ENOENT;
		goto end;
	}

	ret = cmi->ops->dc_handle_cm_msg(cmi->instance, &msg->cm_msg);

end:
	if (ret)
		lnh_error("[LNH] handling CM message failed (%d), link id %d, "
			"from 0x%x\n", ret, lid, itc_sender(msg));
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
		handle_transdata(lnh, msg);
		break;
	case ULH_LNHMSG_CREATECM_REQ:
		handle_createcm(lnh, msg);
		break;
	case ULH_LNHMSG_CREATELINK_REQ:
		handle_createlink(lnh, msg);
		break;
	case ULH_LNHMSG_CREATELINK2_REQ:
		handle_createlink2(lnh, msg);
		break;
	case ULH_LNHMSG_DESTROYLINK_REQ:
		handle_destroylink(lnh, msg);
		break;
	case ULH_LNHMSG_INFO_REQ:
		handle_info(lnh, msg);
		break;
	case ITC_LOCATE_LNH:
		handle_locate(lnh, msg);
		break;
	case ASYNC_QUERY_REPLY:
		handle_lep_found(lnh, msg);
		break;
	case ITC_MONITOR_DEFAULT_NO:
		handle_lep_dead(lnh, msg);
		break;
	case ITC_LOCATE_DEFAULT_NO:
		break;
	case ULH_LNHMSG_OWNER_DEAD:
		handle_garbage_collect(lnh, msg);
		break;
	case ULH_LNHMSG_ENABLE_DIAG:
		handle_diag(lnh, msg);
		break;
	case ULH_CMMSG_GENERAL:
		handle_cm_msg(lnh, msg);
		break;
	default:
		lnh_error("%s: unknown message 0x%x from 0x%x\n", __func__,
				msg->msg_no, itc_sender(msg));
		break;
	}
}

static int handle_remote_ctrlmsg(struct ulh_lnh *lnh,
                                 union itc_msg *msg_p)
{
   switch (msg_p->msg_no) {
      case ASYNC_QUERY_REPLY:
         handle_lep_found(lnh, msg_p);
         break;
      default:
         return 0;
   }

   return 1;
}

static void handle_remotemsg(struct ulh_lnh *lnh, union itc_msg *msg_p,
		itc_mbox_id_t to)
{
       itc_mbox_id_t from;
	struct ulnh_rep *rep;
	struct ulnh_lep *lep;

        if (handle_remote_ctrlmsg(lnh, msg_p))
           return;

        from = itc_sender(msg_p);
	rep = get_rep_by_id(lnh, to);
	lep = get_lep_by_id(lnh, from);

	if (rep) {
           publish_lep(rep->lref, &lep, from);
		if (lep) {
                      tx_rlnh_umsg(lep, rep, msg_p);
                      return;
                }
        }

	lnh_warn("LNH: remote msg LEP (id = %p): %p REP: %p\n",
                 (void*)(uintptr_t)from, (void*)lep, (void*)rep);
}

static int lnh_init(struct ulh_lnh *lnh)
{
	uint32_t size;

	lnh->mbox = itc_create_mailbox(lnh->name, 0);
	if (lnh->mbox == ITC_NO_ID) {
		lnh_error("[LNH] failed to create mailbox, %d\n", errno);
		pthread_exit(NULL);
	}

	ulh_timerqueue_init(&lnh->tqueue);

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

#ifdef FLOW_DIAG
static void lnh_log_mailbox(struct ulh_lnh *lnh, union itc_msg *msg)
{
	int msg_count;

	if (!lnh->diag_config.diagnose) {
		return;
	}

	if (msg->msg_no == ULH_TRANSMSG_DATA) {
		lnh->diag.rhai_messages++;
	}
	else {
		lnh->diag.app_messages++;
	}

	if (lnh->diag.mailbox_countdown-- <= 0) {
		lnh->diag.mailbox_countdown = lnh->diag_config.mailbox_watermark_delay;
		msg_count = itc_get_num_msg();

		if (msg_count >= lnh->diag_config.mailbox_watermark_minimum &&
		    msg_count > lnh->diag.mailbox_watermark) {
			syslog(LOG_INFO, "[LNH]: mailbox watermark raised to: %d, processed %u RHAI and %u application messages",
			       msg_count,
			       lnh->diag.rhai_messages,
			       lnh->diag.app_messages);
			lnh->diag.mailbox_watermark = msg_count;
		}
	}
	else if (lnh->diag.rhai_messages + lnh->diag.app_messages >= lnh->diag_config.mailbox_msg_type_delay) {
		syslog(LOG_INFO, "[LNH]: processed %u RHAI and %u application messages",
		       lnh->diag.rhai_messages,
		       lnh->diag.app_messages);
		lnh->diag.rhai_messages = 0;
		lnh->diag.app_messages = 0;
	}
}
#endif

static void *lnh_thread(void *param)
{
	struct ulh_lnh *lnh = param;
	int tnext, run;
	int32_t rcv_tmo;
	union itc_msg *msg;
	itc_mbox_id_t who;

	run = lnh_init(lnh);
	msg = itc_alloc(4,run);
	itc_send(&msg, lnh->parent, ITC_MY_MBOX);

	for (;run;) {
		rcv_tmo = ITC_NO_TMO;
		tnext = ulh_timerqueue_schedule(&lnh->tqueue);

		if (tnext >= 0)
			rcv_tmo = tnext;

		msg = itc_receive(ITC_NOFILTER, rcv_tmo, ITC_FROM_ALL);
		if (msg) {
#ifdef FLOW_DIAG
			lnh_log_mailbox(lnh, msg);
#endif
			who = itc_receiver(msg);
			if (who == lnh->mbox)
				handle_localmsg(lnh, &msg);
			else {
				handle_remotemsg(lnh, msg, who);
			}

			if (msg)
				itc_free(&msg);
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

        dl_list_init(&lnh->mtors);

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

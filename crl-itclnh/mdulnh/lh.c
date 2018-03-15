/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#define _GNU_SOURCE

#include <stdint.h>
#include <unistd.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>
#include <linux/if_arp.h>
#include <linux/if.h>
#include <net/ethernet.h>
#include <ulh_dl_list.h>
#include <itc.h>
#include <itc_system.h>
#include "rlnh_proto.h"
#include "cm.h"
#include "mdu-link-api.h"
#include "mdu-link-internal.h"
#include "ngns_conf_if.h"

#define TRACEPOINT_DEFINE
#include <com_ericsson_system_start.h>
#include "mdu_lttng.h"
#include "trace.h"

#define STATE_CONNECTING                                        0
#define STATE_INITIATING                                        1
#define STATE_UP                                                2

#define HTAB_SIZE                                               2048
#define HTAB_MASK                                               (HTAB_SIZE - 1)

#define PTR_MASK                                                (~(uintptr_t)0x0000001F)
#define LINK_MASK                                               0x0000001E
#define TYPE_MASK                                               0x00000001

#define HTAB_TYPE_REP                                           0
#define HTAB_TYPE_LEP                                           1

#define SCHEDFIFO_PRIO                                          2



#define ASYNC_LOCATE_REPLY					(0xbabecafe)
struct locate_reply {
        uint32_t      msgno;
        int32_t       linkid;
        char          name[1];
};


union itc_msg {
        uint32_t                                        msg_no;

        struct mdu_create_req                           create_req;
        struct mdu_create_rsp                           create_rsp;
        struct mdu_delete_req                           destroy_req;
        struct mdu_delete_rsp                           destroy_rsp;
        struct mdu_linkstate_ind                        state_ind;

        struct itc_locate_lnh                           locate_req;
        struct itc_locate_lnh_reply                     locate_rsp;
        struct locate_reply                             locate_reply;
};


typedef void (*walkfxn)(void*,void*);

static uintptr_t        htab[HTAB_SIZE];
static uint32_t         htab_bmap[(HTAB_SIZE - 1)/32 + 1];
static uint32_t         htab_maxwalk;

struct ummhp g;


static uint32_t phash(uint32_t key)
{
    key = (key + 0x7ed55d16) + (key << 12);
    key = (key ^ 0xc761c23c) ^ (key >> 19);
    key = (key + 0x165667b1) + (key <<  5);
    key = (key + 0xd3a2646c) ^ (key <<  9);
    key = (key + 0xfd7046c5) + (key <<  3);
    key = (key ^ 0xb55a4f09) ^ (key >> 16);
    return key >> 21;
}

static uint32_t shash(uint32_t key)
{
    key -= (key <<  6);
    key ^= (key >> 17);
    key -= (key <<  9);
    key ^= (key <<  4);
    key -= (key <<  3);
    key ^= (key << 10);
    key ^= (key >> 15);
    return key >> 21;
}

static void update_bmap(uint32_t idx, int set)
{
	uint32_t word = idx >> 5;
	uint32_t bit = (0x80000000 >> (idx & 31));

	if (set)
		htab_bmap[word] |= bit;
	else
		htab_bmap[word] &= ~bit;
}

static void htab_del(uint32_t hash)
{
	if (hash < HTAB_SIZE) {
		htab[hash] = 0;
		update_bmap(hash, 0);
	}
}

static int htab_add(uint32_t key, uintptr_t data)
{
	uint32_t idx = phash(key);
	uint32_t cnt = HTAB_SIZE;

	if (htab[idx])
		for (idx = shash(key); htab[idx] && cnt; cnt--)
			idx = (idx + 1) & HTAB_MASK;

	if (cnt) {
		if ((HTAB_SIZE - cnt) > htab_maxwalk)
			htab_maxwalk = HTAB_SIZE - cnt;

		update_bmap(idx,1);
		htab[idx] = data;
		return idx;
	}
	return -1;
}

static void *htab_get(uint32_t key, uint32_t type)
{
	uint32_t idx = phash(key);
	uint32_t cnt;
	uintptr_t *ptr;
	uintptr_t val = htab[idx];

	if ((val & PTR_MASK)  && ((val & TYPE_MASK) == type)) {
		ptr = (uint32_t*)(val & PTR_MASK);
		if (ptr[0] == key)
			return ptr;
	}

	for (idx = shash(key), cnt = htab_maxwalk + 1; cnt; cnt--) {
		val = htab[idx];
		if ((val & PTR_MASK)  && ((val & TYPE_MASK) == type)) {
			ptr = (uint32_t*)(val & PTR_MASK);
			if (ptr[0] == key)
				return ptr;
		}
		idx = (idx + 1) & HTAB_MASK;
	}
	return NULL;
}

static uint32_t htab_walk(uint32_t mask, uint32_t pattern,
                          void *ctx, walkfxn fn)
{
   uint32_t cnt, map, idx, lzw;
   uintptr_t val;

   for (cnt = 0; cnt < HTAB_SIZE/32; cnt++) {
      map = htab_bmap[cnt];
      idx = cnt << 5;

      while (map) {
         lzw = __builtin_clz(map);
         idx += lzw;
         map = (lzw < 31) ? map << (lzw + 1) : 0;
         val = htab[idx++];

         if ((val & PTR_MASK) && ((val & mask) == pattern))
            fn((void*)(val & PTR_MASK), ctx);
      }
   }

   return (uint32_t)-1;
}

static struct mhp_locate *find_locate(struct mhp_link *link, char *name)
{
	struct mhp_locate **p, *q = NULL;

	for (p = &link->locates; *p; p = &((*p)->next))
		if (!strcmp((*p)->name, name)) {
			q = *p;
			*p = (*p)->next;
			break;
		}

	return q;
}

static void tx_rlnh(struct mhp_link *link, void *msg, uint32_t size)
{
	struct mhp_msghdr lhhdr;
	struct rlnh_header *hdr = msg;

	(void)hdr;

	lhhdr.size = size;
	lhhdr.src = 0;
	lhhdr.dst = 0;

	mhp_send(link, &lhhdr, msg);
}

static int tx_rlnh_init(struct mhp_link *link, uint32_t version)
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

static int tx_rlnh_init_reply(struct mhp_link *link,
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

static int tx_rlnh_unpublish(struct mhp_link *link, uint32_t laddr)
{
	struct rlnh_msg_unpublish *msg;
	msg = (struct rlnh_msg_unpublish *) itc_alloc(sizeof(*msg), 0);
	msg->hdr.reserved = 0;
	msg->hdr.type = htons(RLNH_UNPUBLISH);
	msg->laddr = htonl(laddr);

	MDU_INFO("[MDU] tx UNPUBLISH for %s addr %d", link->name, laddr);

	tx_rlnh(link, msg, sizeof(*msg));
	itc_free((union itc_msg **)&msg);
	return 0;
}

static int tx_rlnh_unpublish_ack(struct mhp_link *link, uint32_t laddr)
{
	struct rlnh_msg_unpublish_ack *msg;

	msg = (struct rlnh_msg_unpublish_ack *)itc_alloc(sizeof(*msg), 0);
	msg->hdr.reserved = 0;
	msg->hdr.type = htons(RLNH_UNPUBLISH_ACK);
	msg->laddr = htonl(laddr);

	MDU_INFO("[MDU] tx UNPUBLISH ACK for %s addr %d", link->name, laddr);

	tx_rlnh(link, msg, sizeof(*msg));
	itc_free((union itc_msg **)&msg);
	return 0;
}

static int tx_rlnh_umsg(struct mhp_lep *lep,
		   struct mhp_rep *rep, union itc_msg *msg)
{
	struct mhp_link *link = rep->link;
	struct mhp_msghdr hdr;

	msg->msg_no = htonl(msg->msg_no);

	hdr.size = itc_size(msg);
	hdr.src = lep->addr[link->linkid];
	hdr.dst = rep->addr;

	mhp_send(link, &hdr, msg);
	return 0;
}

static int tx_rlnh_publish(struct mhp_link *link, const char *name,
		struct mhp_lep *lep)
{
	struct rlnh_msg_publish *msg;
	int len = strlen(name);

	msg = (struct rlnh_msg_publish *)itc_alloc((sizeof(*msg)+len), 0);
	msg->hdr.reserved = 0;
	msg->hdr.type = htons(RLNH_PUBLISH);
	msg->laddr = htonl(lep->addr[link->linkid]);

	MDU_INFO("[MDU] tx PUBLISH: %s (0x%x) on %s (addr: %d)",
			name, lep->mbox, link->name, lep->addr[link->linkid]);

	strcpy(msg->name, name);
	tx_rlnh(link, msg, sizeof(*msg) + len);
	itc_free((union itc_msg **)&msg);
	return 0;
}

static int tx_rlnh_query_name(struct mhp_link *link,
		struct mhp_lep *lep,  const char *name)
{
	struct rlnh_msg_queryname *msg;
	int str_len;
	uint32_t size;

	str_len = strlen(name) + 1;
	size = (sizeof(*msg) + str_len + 3) & ~0x3;

	msg = (struct rlnh_msg_queryname *) itc_alloc(size, 0);
	msg->hdr.reserved = 0;
	msg->hdr.type = htons(RLNH_QUERY_NAME);
	msg->laddr = htonl(lep->addr[link->linkid]);

	MDU_INFO("[MDU] tx QUERY: %s/%s", link->name, name);

	memcpy(msg->name, name, str_len);
	tx_rlnh(link, msg, size);
	itc_free((union itc_msg **)&msg);
	return 0;
}

static void link_drop(struct mhp_link *link)
{
	if (link->lh_state == STATE_UP) {
		MDU_INFO("[MDU] %s deassign linkhandler %s",
				__func__, link->name);
		itc_deassign_linkhandler(link->name, g.mbox);
	}
	mhp_disc(link);
	link->lh_state = STATE_CONNECTING;
}

static void notify_ngns(struct mhp_link *link)
{
	struct ns_msg *msg;
	int len = strlen(link->name) + 1;
	int siz = (sizeof(*msg) + len) & ~3;

	if (g.ns_mbox == ITC_NO_ID)
		return;

	msg = (struct ns_msg*)itc_alloc(siz, NGNS_ADD_NEIGHBOUR_REQ);
	msg->m_total = 1;
	msg->m_body_count = 1;
	msg->m_len[0] = len;
	strcpy(msg->m_body, link->name);

	if (link->lh_state != STATE_UP)
    	msg->m_type = NGNS_REM_NEIGHBOUR_REQ;

	itc_send((union itc_msg**)&msg, g.ns_mbox, ITC_MY_MBOX);
}

static void notify_owner(struct mhp_link *link, uint32_t state)
{
	union itc_msg *msg;
	msg = itc_alloc(sizeof(struct mdu_linkstate_ind),
                        MDU_LINKSTATE_IND);

	msg->state_ind.link_id = link->linkid;
	msg->state_ind.state = state;
	itc_send(&msg, link->owner_mbox, ITC_MY_MBOX);

	notify_ngns(link);
}

static void free_lep(struct mhp_lep *lep, struct mhp_link *link)
{
	uint32_t laddr;

	if (link == NULL) {
		uint32_t lid;
		uintptr_t lbt = htab[lep->hash];

		for (lid = 0; lid < MAX_LINK; lid++, lbt >>= 1) {
			link = g.links[lid];
			laddr = lep->addr[lid];

			if (laddr || (lbt & 2)) {
				if (link) {
					if (laddr) {
						lep->addr[link->linkid] = 0;
						link->lep_table[laddr] = NULL;
						tx_rlnh_unpublish(link, laddr);
					} else {
						MDU_INFO("[MDU]: LEP 0x%x published on "
					  "%s but addr is 0", lep->mbox, link->name);
					}
				} else {
					MDU_INFO("[MDU]: LEP 0x%x (addr: %d) on "
					 "non-existing link", lep->mbox, laddr);
				}
			}
		}
		itc_unmonitor(lep->mtor);
		htab_del(lep->hash);
		free(lep->aptr);
		return;
	}

	laddr = lep->addr[link->linkid];
	lep->addr[link->linkid] = 0;
	link->lep_table[laddr] = NULL;
	htab[lep->hash] &= ~link->linkbt;

	if ((htab[lep->hash] & LINK_MASK) == 0) {
		itc_unmonitor(lep->mtor);
		htab_del(lep->hash);
		free(lep->aptr);
	}
	tx_rlnh_unpublish(link, laddr);
}

static void free_rep(struct mhp_rep *rep, struct mhp_link *link)
{
	link = rep->link;
	htab_del(rep->hash);
	itc_delete_mailbox(rep->mbox);
	link->rep_table[rep->addr] = 0;
	free(rep->aptr);
}

static int publish_lep( struct mhp_link *link, struct mhp_lep **lepp,
							itc_mbox_id_t mbox)
{
	struct mhp_lep *lep = *lepp;
	char mbox_name[64];
	uint32_t addr;
	int ret = 0;

	if (lep && (htab[lep->hash] & link->linkbt))
		return ret;

	*lepp = lep = htab_get(mbox, HTAB_TYPE_LEP);

	if (lep && (htab[lep->hash] & link->linkbt))
		goto done;

	ret = -1;

	if (lep == NULL) {
		void *aptr = malloc(sizeof(*lep) + ~PTR_MASK + 1);
		uintptr_t data = ((uintptr_t)aptr + ~PTR_MASK) & PTR_MASK;

		if (aptr == NULL) {
			MDU_ERROR("[MDU]: malloc failure in %s", __func__);
			goto done;
		}
		*lepp = lep = (struct mhp_lep *)data;
		memset(lep, 0, sizeof(*lep));
		lep->mbox = mbox;
		lep->aptr = aptr;
		lep->hash = htab_add(mbox, data | HTAB_TYPE_LEP);

		if (lep->hash == (uint32_t)-1) {
			MDU_ERROR("[MDU]: hash table full in %s", __func__);
			free(aptr);
			goto done;
		}

		lep->mtor = itc_monitor(mbox, NULL);
		if (lep->mtor == ITC_NO_ID) {
			MDU_ERROR("[MDU]: itc_monitor failed for 0x%x", mbox);
			htab_del(lep->hash);
			free(aptr);
			goto done;
		}
	}

	for (addr = 1; addr <= MAX_LEP_ADDR; addr++)
		if (link->lep_table[addr] == 0)
			break;

	if (addr > MAX_LEP_ADDR) {
		MDU_ERROR("[MDU] - LEP table full (%d slots)", MAX_LEP_ADDR);
		goto done;
	}

	if (!itc_get_name(mbox, mbox_name, sizeof(mbox_name))) {
		MDU_ERROR("[MDU] - itc_get_name failed for 0x%x", mbox);
		goto done;
	}

	link->lep_table[addr] = lep;
	lep->addr[link->linkid] = addr;
	htab[lep->hash] |= link->linkbt;
	tx_rlnh_publish(link, mbox_name, lep);
	ret = 0;

 done:
	return ret;
}

static void rx_rlnh_init_reply(struct mhp_link *link,
		struct rlnh_msg_initreply *msg)
{
	struct mhp_locate *loc;
	uint32_t status = ntohl(msg->status);
	struct mhp_lep *lep;

	if (link->lh_state != STATE_INITIATING)
		return;

	if (status) {
		MDU_ERROR("[MDU]: %s: remote end refused connection "
				"(status:%u)", link->name, status);
		link_drop(link);
		return;
	}

	link->lh_state = STATE_UP;
	itc_assign_linkhandler(link->name, g.mbox);
	MDU_INFO("[MDU]: link %s/ published", link->name);
	notify_owner(link, MDU_LINK_UP);

	lep = htab_get(g.mbox, HTAB_TYPE_LEP);
	publish_lep(link, &lep, g.mbox);

	for (loc = link->locates; loc; loc = loc->next)
		tx_rlnh_query_name(link, lep, loc->name);
}

static void rx_rlnh_publish(struct mhp_link *link,
		struct rlnh_msg_publish *msg)
{
	uint32_t laddr = ntohl(msg->laddr);
	uintptr_t data;
	struct mhp_locate *loc;
	struct mhp_rep *rep;
	itc_mbox_id_t mbox;
	char name[64];
	void *aptr;

	snprintf(name, sizeof(name), "%s/%s", link->name, msg->name);
	MDU_INFO("[MDU] rx PUBLISH: %s (laddr: %d)", name, laddr);

	mbox = itc_locate(name);
	if (mbox != ITC_NO_ID) {
		MDU_INFO("[MDU] WARNING '%s' already exists", name);
	}

	aptr = malloc(sizeof(*rep) + ~PTR_MASK + 1);
	if (!aptr)
		return;
	data = ((uintptr_t)aptr + ~PTR_MASK) & PTR_MASK;
	rep = (struct mhp_rep *)data;
	rep->aptr = aptr;
	rep->addr = laddr;
	rep->link = link;

	rep->mbox = itc_clone_mailbox(g.mbox, name);

	if (rep->mbox == ITC_NO_ID) {
		MDU_ERROR("[MDU] itc_clone_mailbox failed for %s", name);
		free(rep);
		return;
	}

	data |= (link->linkbt | HTAB_TYPE_REP);
	rep->hash = htab_add(rep->mbox, data);
	link->rep_table[laddr] = rep;

	loc = find_locate(link, msg->name);
	if (loc)
		free(loc);
}

static void rx_rlnh_query(struct mhp_link *link,
					struct rlnh_msg_queryname *msg)
{
	struct mhp_lep *lep;
	itc_mbox_id_t mbox;
	union itc_msg *loc;
	int size;

	MDU_INFO("[MDU] rx QUERY for %s from %s",msg->name, link->name);

	mbox = itc_locate(msg->name);
	if (mbox != ITC_NO_ID) {
		lep = htab_get(mbox, HTAB_TYPE_LEP);

		if (publish_lep(link, &lep, mbox) == 0)
			return;
	}

	size = sizeof(struct locate_reply) + strlen(msg->name);
	size = (size + 3) & ~3;

	loc = itc_alloc(size, ASYNC_LOCATE_REPLY);
	loc->locate_reply.linkid = link->linkid;
	strcpy(loc->locate_reply.name, msg->name);

	MDU_INFO("[MDU] itc_locate_async(%s) from %s",msg->name, link->name);
	itc_locate_async(msg->name, &loc, g.mbox);
}

static void rx_rlnh_unpublish(struct mhp_link *link,
		struct rlnh_msg_unpublish *msg)
{
	struct mhp_rep *rep;
	uint32_t laddr = ntohl(msg->laddr);

	rep = link->rep_table[laddr];
	if (!rep) {
		MDU_INFO("[MDU]: %s: UNPUBLISH for unknown laddr:0x%x",
				link->name, laddr);
		return;
	}

	MDU_INFO("[MDU] rx UNPUBLISH addr %d on %s", laddr, link->name);

	free_rep(rep, link);
	tx_rlnh_unpublish_ack(link, laddr);
}

static void rx_rlnh(struct mhp_link *link, union itc_msg *msg,
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
		break;
	default:
		MDU_ERROR("[MDU]: %s: unexpected RLNH message type 0x%x",
				link->name, ntohs(hdr->type));
		link_drop(link);
		break;
	}

	itc_free(&msg);
}

static void rx_umsg(struct mhp_link* link, struct mhp_msghdr *hdr,
												union itc_msg* msg)
{
	struct mhp_rep *rep;
	struct mhp_lep *lep;

	if ((hdr->src > MAX_REP_ADDR) || (hdr->dst > MAX_LEP_ADDR)) {
		MDU_ERROR("[MDU]: Bad adress(es) dst: %d  src: %d)",
								   hdr->dst, hdr->src);
		link_drop(link);
		itc_free(&msg);
		return;
	}

	rep = link->rep_table[hdr->src];
	lep = link->lep_table[hdr->dst];
	msg->msg_no = ntohl(msg->msg_no);

	if (lep && rep) {
		itc_send(&msg, lep->mbox, rep->mbox);
		return;
	}

	if (rep) {
		MDU_INFO("[MDU]: msgno 0x%x from 0x%x to unknown dst_addr %d",
								   msg->msg_no, rep->mbox, hdr->dst);
		itc_free(&msg);
		return;
	} else if (lep) {
		MDU_ERROR("[MDU]: msgno 0x%x to 0x%x from unknown src_addr %d)",
								   msg->msg_no, lep->mbox, hdr->src);
	} else {
		MDU_ERROR("[MDU]: msgno 0x%x unknown src %d  unknown dst %d",
								   msg->msg_no, hdr->src, hdr->dst);
	}
	itc_free(&msg);
	link_drop(link);
}

void uc_connected(struct mhp_link *link)
{
	MDU_INFO("[MDU]: %s: connected (state: %d)",
			link->name, link->lh_state);

	if (link->lh_state != STATE_CONNECTING)
		return;

	tx_rlnh_init(link, RLNH_VERSION);
	link->lh_state = STATE_INITIATING;
}

void uc_disconnected(struct mhp_link *link)
{
	uint32_t mask, patt;

	MDU_ERROR("[MDU]: %s: disconnected", link->name);

	if (link->lh_state == STATE_UP) {
		MDU_INFO("[MDU] %s deassign linkhandler %s",
				__func__, link->name);
		itc_deassign_linkhandler(link->name, g.mbox);
		link->lh_state = STATE_CONNECTING;
		notify_owner(link, MDU_LINK_DOWN);
	}

	mask = link->linkbt | TYPE_MASK;
	patt = link->linkbt | HTAB_TYPE_LEP;
	htab_walk(mask, patt, link, (walkfxn)free_lep);

	patt = link->linkbt | HTAB_TYPE_REP;
	htab_walk(mask, patt, link, (walkfxn)free_rep);
}

void uc_deliver(struct mhp_link *link, struct mhp_msghdr *hdr,
											union itc_msg *msg)
{
	if (hdr->dst || hdr->src)
		rx_umsg(link, hdr, msg);
	else
		rx_rlnh(link, msg, hdr->size);
}

static int collision_detect(char *name)
{
	int cnt;
	for (cnt = 0; (cnt < MAX_LINK) && g.links[cnt]; cnt++)
		if (strcmp(g.links[cnt]->name, name) == 0)
			return -EEXIST;

	return 0;
}

static int alloc_link(uint32_t *linkid)
{
	int cnt;
	for (cnt = 0; cnt < MAX_LINK; cnt++)
		if (g.links[cnt] == NULL) {
			*linkid = (uint32_t)cnt;
			return 0;
		}

	return -1;
}

static void handle_locate(union itc_msg *msg)
{
	char link_name[64], *dst = link_name, *src;
	struct mhp_link *link;
	struct mhp_locate *loc;
	struct mhp_lep *lep;
	itc_mbox_id_t mbox;
	int idx;

	MDU_INFO("[MDU] locate for %s from 0x%x", msg->locate_req.name,
					 msg->locate_req.from);

	for (src = msg->locate_req.name; *src; dst++, src++)
		if ((*dst = *src) == '/') {
			*dst = 0;
			break;
		}

	for (idx = 0; idx < MAX_LINK; idx++) {
		link = g.links[idx];
		if (link && (strcmp(link_name, link->name) == 0))
			break;
		link = NULL;
	}

	if (link == NULL) {
		MDU_ERROR("[MDU]: locate for %s, link is not found",
				msg->locate_req.name);
		return;
	}

	if (link->lh_state != STATE_UP)
		return;

	lep = htab_get(msg->locate_req.from, HTAB_TYPE_LEP);
	publish_lep(link, &lep, msg->locate_req.from);
	mbox = itc_locate(msg->locate_req.name);

	if (mbox != ITC_NO_ID)
		return;

	loc = find_locate(link, ++src);
	if (loc == NULL) {
		loc = malloc(sizeof(*loc));
		strcpy(loc->name, src);
	}

	loc->next = link->locates;
	link->locates = loc;

	tx_rlnh_query_name(link, lep, src);
}

static void handle_locate_reply(union itc_msg *msg)
{
	itc_mbox_id_t mbox = itc_sender(msg);
	struct mhp_link *link = g.links[msg->locate_reply.linkid];
	struct mhp_lep *lep = htab_get(mbox, HTAB_TYPE_LEP);

	MDU_INFO("%s linkid: %u  link: %p", __func__,
				msg->locate_reply.linkid, (void*)link);

	if (link && (link->lh_state == STATE_UP))
		publish_lep(link, &lep, mbox);
}

static void handle_lep_dead(union itc_msg *msg)
{
	itc_mbox_id_t mbox = itc_sender(msg);
	struct mhp_lep *lep = htab_get(mbox, HTAB_TYPE_LEP);

	MDU_INFO("%s mbox: %u  lep: %p", __func__,
				mbox, (void*)lep);

	if (lep) {
		free_lep(lep,NULL);
	}
}

static void handle_locate_ngns(union itc_msg *msg)
{
	int i;
	char name[64];
	itc_mbox_id_t mbox = itc_sender(msg);

	if (g.ns_mbox != ITC_NO_ID)
		return;

	if (itc_get_name(mbox, name, sizeof(name)) == 0)
		return;

	if (strcmp(name, NAME_SERVER_CONF))
		return;

	for (g.ns_mbox = mbox, i = 0; i < MAX_LINK; i++)
		if (g.links[i] && (g.links[i]->lh_state == STATE_UP))
			notify_ngns(g.links[i]);
}

static void destroy_link(struct mhp_link *link)
{
	struct mhp_locate *p, *q;
	uint32_t mask, patt;

	g.links[link->linkid] = NULL;

	for (p = link->locates; p; p = q) {
		q = p->next;
		free(p);
	}

	mask = link->linkbt | TYPE_MASK;
	patt = link->linkbt | HTAB_TYPE_LEP;
	htab_walk(mask, patt, link, (walkfxn)free_lep);

	patt = link->linkbt | HTAB_TYPE_REP;
	htab_walk(mask, patt, link, (walkfxn)free_rep);
	itc_deassign_linkhandler(link->name, g.mbox);
	link->lh_state = STATE_CONNECTING;
	notify_ngns(link);
	mhp_exit(link);
	free(link);
}

static void handle_destroy(union itc_msg *msg)
{
	union itc_msg *rsp;
	uint32_t lnk;

	rsp = itc_alloc(sizeof(struct mdu_delete_rsp),MDU_DELETE_RSP);
	rsp->destroy_rsp.linkid = msg->destroy_req.linkid;
	rsp->destroy_rsp.owner = msg->destroy_req.owner;
	rsp->destroy_rsp.result = -EINVAL;

	if (msg->destroy_req.owner) {
		for (lnk = 0; lnk  < MAX_LINK; lnk++) {
			if (g.links[lnk]) {
				if (g.links[lnk]->owner == msg->destroy_req.owner) {
					destroy_link(g.links[lnk]);
					rsp->destroy_rsp.result = 0;
				}
			}
		}
	} else if (g.links[msg->destroy_req.linkid]) {
		destroy_link(g.links[msg->destroy_req.linkid]);
		rsp->destroy_rsp.result = 0;
	}

	itc_send(&rsp, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_create(union itc_msg *cfg)
{
	struct mdu_link_config *hw_cfg = &cfg->create_req.config;
	struct mhp_link *mhp = NULL;
	union itc_msg *msg = NULL;
	uint32_t linkid = 0;
	int ret = -1;

	ret = collision_detect(cfg->create_req.name);
	if (ret)
		goto done;

	ret = alloc_link(&linkid);
	if (ret)
		goto done;

	ret = -ENOMEM;
	mhp = malloc(sizeof(struct mhp_link));
	if (mhp == NULL)
		goto done;

	memset(mhp, 0, sizeof(*mhp));
	ret = ecom_init(mhp, hw_cfg);
	if (ret)
		goto done;

	ret = mhp_init(mhp);
	if (ret)
		goto done;

	msg = itc_alloc(sizeof(struct mdu_delete_req), MDU_DELETE_REQ);
	msg->destroy_req.owner = cfg->create_req.owner;

        mhp->max_itc_msg_size = itc_get_max_size();
	mhp->linkid = linkid;
	mhp->linkbt = 2 << linkid;
	mhp->owner = cfg->create_req.owner;
	mhp->owner_mbox = itc_sender(cfg);
	mhp->owner_mtor = itc_monitor(mhp->owner_mbox, &msg);
	strncpy(mhp->name, cfg->create_req.name, 32);
	g.links[linkid] = mhp;
	mhp = NULL;

 done:
	if (mhp)
		free(mhp);

	msg = itc_alloc(sizeof(struct mdu_create_rsp), MDU_CREATE_RSP);
	strncpy(msg->create_rsp.name, cfg->create_req.name, 32);
	msg->create_rsp.linkid = linkid;
	msg->create_rsp.result = ret;
	itc_send(&msg, itc_sender(cfg), ITC_MY_MBOX);
}

static void handle_remotemsg(union itc_msg *msg, itc_mbox_id_t to)
{
	itc_mbox_id_t from = itc_sender(msg);
	struct mhp_lep *lep = htab_get(from, HTAB_TYPE_LEP);
	struct mhp_rep *rep;

	rep = htab_get(to, HTAB_TYPE_REP);
	if (rep) {
		publish_lep(rep->link, &lep, from);
		if (lep) {
			tx_rlnh_umsg(lep, rep, msg);
			return;
		}
	}
	MDU_ERROR("[MDU]: remote msg LEP (id = %p): %p REP: %p",
						(void *)(uintptr_t)from, (void *)lep, (void *)rep);
}

static int handle_localmsg(union itc_msg *msg)
{
	switch (msg->msg_no) {
	case MDU_CREATE_REQ:
		handle_create(msg);
		break;
	case MDU_DELETE_REQ:
		handle_destroy(msg);
		break;
	case MDU_DELETE_RSP:
		break;
	case ITC_LOCATE_LNH:
		handle_locate(msg);
		break;
	case ITC_MONITOR_DEFAULT_NO:
		handle_lep_dead(msg);
		break;
	case ASYNC_LOCATE_REPLY:
		handle_locate_reply(msg);
		break;
	case ITC_LOCATE_DEFAULT_NO:
		handle_locate_ngns(msg);
		break;

	default:
		MDU_ERROR("%s: unknown message 0x%x from 0x%x", __func__,
				msg->msg_no, itc_sender(msg));
		break;
	}
	return 0;
}

static int open_socket(uint16_t vlan_id)
{
	struct sockaddr_ll saddr;
    struct ifreq ifr;
	int sd, rc, cnt, flags;

    if ((sd = socket(AF_PACKET, SOCK_RAW, htons(ETHERTYPE_MDU))) < 0)
        return -1;

    sprintf(ifr.ifr_name, "vei0.%u", vlan_id);
    //sprintf(ifr.ifr_name, "eth%d", 0);

	for (cnt = 0;;sleep(1)) {
		if ((rc = ioctl(sd, SIOCGIFINDEX, &ifr)) == 0)
			break;
		if (errno != ENODEV)
			goto err_out;

		if ((cnt < 600) && ((cnt++ % 60) == 0)) {
			MDU_INFO("Waiting for device %s to come up", ifr.ifr_name);
		}
	}

	memset(&saddr, 0 , sizeof(saddr));
    saddr.sll_ifindex 	= ifr.ifr_ifindex;
    saddr.sll_family    = AF_PACKET;
    saddr.sll_halen     = ETHER_ADDR_LEN;
    saddr.sll_protocol  = htons(ETHERTYPE_MDU);

    if ((rc = ioctl(sd, SIOCGIFHWADDR, &ifr)) < 0)
        goto err_out;

	if (ifr.ifr_hwaddr.sa_family != ARPHRD_ETHER)
		goto err_out;

	g.ifindex = saddr.sll_ifindex;
	memcpy(g.own_mac, ifr.ifr_hwaddr.sa_data, 6);


	flags = fcntl(sd, F_GETFL);
	if (flags != -1)
		flags = fcntl(sd, F_SETFL, flags | O_NONBLOCK);

	if (flags == -1) {
		MDU_ERROR("WARNING, Failed to set O_NONBLOCK: %d(%s)",
				errno, strerror(errno));

		printf("WARNING, Failed to set O_NONBLOCK: %d(%s)\n",
				errno, strerror(errno));
	}

    if (!(rc = bind(sd, (const struct sockaddr *)&saddr, sizeof(saddr))))
        return sd;

  err_out:
    close(sd);
    return rc;
}


static void read_socket(int sock)
{
	char buffer[1600];
	int len;

 read_again:

	len = recvfrom(g.sock, buffer, sizeof(buffer), 0, NULL, NULL);
	if (len > 0) {
		rx_packet(buffer, len);
		goto read_again;
	}
}

static inline uint64_t get_tick(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (uint64_t) ts.tv_sec * 1000ULL + ts.tv_nsec / 1000000;
}

static void config_sched()
{
	struct sched_param schedp;
    int num_cores, ret, i;
    cpu_set_t cpu;

    CPU_ZERO(&cpu);
	num_cores = sysconf(_SC_NPROCESSORS_ONLN);

    /* Exclude core 0 from affinity mask */
    for (i = 1; i < num_cores; i++) {
        CPU_SET(i, &cpu);
    }

    ret = pthread_setaffinity_np(pthread_self(), sizeof(cpu), &cpu);
	if (ret) {
		MDU_ERROR("pthread_setaffinity_np failure: %d",ret);
	}

	memset(&schedp, 0, sizeof(struct sched_param));
	schedp.sched_priority = SCHEDFIFO_PRIO;

	ret = pthread_setschedparam(pthread_self(), SCHED_FIFO, &schedp);
	if (ret) {
		MDU_ERROR("pthread_setschedparam failure: %d\n", ret);
	}
}

int main(int argc, char *argv[])
{
	uint32_t sel[2] = {1, MDU_CREATE_REQ};
	struct timeval tv = {0, 2000};
	uint64_t tick_time = 0, tnow;
	union itc_msg *msg;
	int ret = 0, i;
	int num, fdm, mfd, sfd;
	fd_set fds;

	config_sched();

	ret = itc_init(512, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		MDU_ERROR("itc_init failure: %d",ret);
		return -1;
	}

	g.mbox = itc_create_mailbox(MDU_DAEMON_NAME, 0);
	if (g.mbox == ITC_NO_ID) {
		MDU_ERROR("created mbox failure: %d", -errno);
		goto eout;
	}

	mfd = itc_get_fd();
	fdm = mfd;
	event_system_start("mdu ready");
	msg = itc_receive(sel, ITC_NO_TMO, ITC_FROM_ALL);

	g.sock = open_socket(msg->create_req.config.vlan_id);
	if (g.sock < 0) {
		MDU_ERROR("open socket failed, %d", -errno);
		printf("open socket failed, %d", -errno);
		goto eout;
	}

	g.ns_mbox = ITC_NO_ID;
	itc_locate_async(NAME_SERVER_CONF, NULL, ITC_MY_MBOX);

	handle_create(msg);
	itc_free(&msg);
	sfd = g.sock;
	fdm = (sfd > fdm) ? sfd : fdm;

	FD_ZERO(&fds);

	for (;;) {
		FD_SET(mfd, &fds);
		FD_SET(sfd, &fds);

		num = select(fdm + 1, &fds, NULL, NULL, &tv);
		if (num > 0) {
			if (FD_ISSET(sfd, &fds)) {
				read_socket(sfd);
			}

			if (FD_ISSET(mfd, &fds)) {
				msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
				if (msg) {
					itc_mbox_id_t who = itc_receiver(msg);
					if (who != g.mbox)
						handle_remotemsg(msg, who);
					else
						handle_localmsg(msg);

					itc_free(&msg);
				}
			}
		} else
			tv.tv_usec = 2000; /* EINTR modifies tv. restore */

		tnow = get_tick();
		if (tnow >= tick_time){
			for (i = 0; i < MAX_LINK; i++) {
				if (g.links[i])
					mhp_tick(g.links[i]);
			}
			tick_time = get_tick() + 10;
		}
	}

 eout:

	for (;;) sleep(600);
	return -1;
}

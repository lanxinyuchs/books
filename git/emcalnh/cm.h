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


#ifndef _CM_H_
#define _CM_H_


#include "rhai-mhp.h"
#include "eolc-link-api.h"
#include "eolc-link-internal.h"

#define MAX_NAME_LENGTH				32
#define MAX_LEP_ADDR				128
#define MAX_REP_ADDR				128
#define MAX_LINK					5



struct __attribute__ ((__packed__)) tx_buff {
	struct tx_buff			*next;
	uint32_t				size;
	uint64_t 				time;
	uint8_t					*data;
};

struct tx_queue {
	struct tx_buff			*head;
	struct tx_buff			*tail;
};

struct mhp_rep {
	itc_mbox_id_t 			mbox;
	uint32_t			 	hash;
	struct mhp_link 		*link;
	uint32_t 				addr;
	void					*aptr;
};

struct mhp_lep {
	itc_mbox_id_t 			mbox;
	uint32_t			 	hash;
	itc_monitor_id_t		mtor;
	uint8_t 				addr[MAX_LINK];
	void					*aptr;
};

struct mhp_msghdr {
	uint32_t 				src;
	uint32_t 				dst;
	uint32_t 				size;
};

struct mhp_link {
	uint16_t				cm_state;
	uint16_t				lh_state;

	uint32_t				mtu;
	uint32_t				connid;

	uint32_t				linkid;
	uint32_t				linkbt;

	uint16_t 				nack_sent;
	uint16_t 				rt_cnt;
	uint16_t 				rx_seq;
	uint16_t 				rx_outstanding;
	uint16_t 				tx_seq;
	uint16_t 				tx_outstanding;
	uint16_t 				tx_ack;
	uint16_t 				tx_wnd;

	uint16_t 				local_cid;
	uint16_t 				remote_cid;

	struct tx_queue			txq;
	struct tx_queue			axq;

	union itc_msg 			*rx_msg;
	uint8_t 				*rx_pos;
	uint16_t 				rx_frag;
	struct mhp_msghdr		rx_head;

	uint32_t 				conn_alive;
	uint32_t 				keep_alive;

	struct mhp_lep 			*lep_table[MAX_LEP_ADDR+1];
	struct mhp_rep 			*rep_table[MAX_REP_ADDR+1];

	void					*owner;
	itc_mbox_id_t			link_mbox;
	itc_mbox_id_t			owner_mbox;
	itc_monitor_id_t		owner_mtor;
	char					name[MAX_NAME_LENGTH];
};

struct ummhp {
	itc_mbox_id_t		mbox;
	struct rhai_mhp_if 	*mhp_handle;
	struct mhp_link		*links[MAX_LINK];
};

extern struct ummhp g;


int  mhp_init(struct mhp_link *co);
void mhp_tick(struct mhp_link *co);
void mhp_exit(struct mhp_link *co);
void mhp_disc(struct mhp_link *co);
int  mhp_send(struct mhp_link*, struct mhp_msghdr*, union itc_msg*);
void rx_packet(void *data, uint16_t length, void *cref);
void *mhp_recv(void *param);

int  ecom_init(struct mhp_link *mhp, struct eolc_g2hw_config *cfg);

void uc_connected(struct mhp_link *link);
void uc_disconnected(struct mhp_link *link);
void uc_deliver(struct mhp_link *, struct mhp_msghdr *,union itc_msg *);

#endif

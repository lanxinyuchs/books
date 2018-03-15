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

#include "mdu-link-api.h"
#include "mdu-link-internal.h"

#define MAX_NAME_LENGTH				32
#define MAX_LEP_ADDR				512
#define MAX_REP_ADDR				512
#define MAX_LINK					4

#define ETHERTYPE_802_1Q			0x8100
#define ETHERTYPE_MDU				0xebd0
#define MDU_VLAN					10
#define MDU_RMTU					1500


#define ETHER_HDRSIZ				14


struct __attribute__ ((__packed__))ether_hdr {
	uint8_t 				dst[6];
	uint8_t 				src[6];
	/*uint16_t 				type;
	uint16_t 				tci;*/
	uint16_t 				proto;
};

struct __attribute__ ((__packed__)) tx_buff {
	struct tx_buff			*next;
	uint32_t				size;
	uint64_t 				time;
	struct ether_hdr		txhdr;
	uint8_t					data[1];
};

struct tx_queue {
	struct tx_buff			*head;
	struct tx_buff			*tail;
};

struct mhp_locate {
	struct mhp_locate		*next;
	char					name[64];
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
	uint16_t 				addr[4];
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

	int 					mtu;
	struct sockaddr_ll 		saddr;
	struct ether_hdr		txhdr;

	struct mhp_lep 			*lep_table[MAX_LEP_ADDR+1];
	struct mhp_rep 			*rep_table[MAX_REP_ADDR+1];
        uint32_t                         max_itc_msg_size;
	struct mhp_locate		*locates;
	void					*owner;
	itc_mbox_id_t			owner_mbox;
	itc_monitor_id_t		owner_mtor;
	char					name[MAX_NAME_LENGTH];
};

struct ummhp {
	int						sock;
	volatile int			running;
	itc_mbox_id_t			mbox;
	itc_mbox_id_t			ns_mbox;

	struct mhp_link			*links[MAX_LINK];
	int						ifindex;
	uint8_t					own_mac[8];
};

extern struct ummhp g;


int  mhp_init(struct mhp_link *co);
void mhp_tick(struct mhp_link *co);
void mhp_exit(struct mhp_link *co);
void mhp_disc(struct mhp_link *co);
int  mhp_send(struct mhp_link*, struct mhp_msghdr*, union itc_msg*);
void rx_packet(void *param, int len);


int ecom_init(struct mhp_link *co, struct mdu_link_config *cfg);

void uc_connected(struct mhp_link *link);
void uc_disconnected(struct mhp_link *link);
void uc_deliver(struct mhp_link *, struct mhp_msghdr *,union itc_msg *);


#endif

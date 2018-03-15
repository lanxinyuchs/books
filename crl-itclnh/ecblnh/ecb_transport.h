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

#ifndef _ECB_TRANSPORT_H_
#define _ECB_TRANSPORT_H_

#include <itc.h>
#include <rhai-ser.h>
#include <ulh_transport.h>
#include "atfi.h"


#define ECB_HLEN                		5
#define ECB_MTU							78

#define CHAR_ESC                		0x7d
#define CHAR_END						0x7e

#define DISCONNECTED 					0
#define CONNECTING 						1
#define CONNECTED 						2



struct atf_client {
	itc_mbox_id_t		pid;
	void*				cookie;
	uint32_t			conn_map;
};


struct atf_conn {
	uint16_t			state;
	uint16_t			type;
	uint16_t			phy_addr;
	uint16_t			log_addr;
	uint32_t			tran_id;
	uint32_t			link_id;
	char				*name;
	uint32_t			client_map;
	uint32_t			client_info[ATFI_MAX_NUMBER_OF_CLIENTS];
};


struct atf_data {
	itc_mbox_id_t		mpa4ci_mbox;
	itc_mbox_id_t		mpatfi_mbox;
	itc_mbox_id_t		server_mbox;
	uint8_t				conn_lookup[256];
	struct atf_client	clients[ATFI_MAX_NUMBER_OF_CLIENTS];
	struct atf_conn		conns[ATFI_MAX_NUMBER_OF_CONNECTIONS];
};


extern struct atf_data atfi;



#define EBUS_MTU							512

struct ser_port {
	struct rhai_ser_if  *handle;
	uint32_t			rx_octets;
	uint32_t			tx_octets;
	uint32_t			rx_frames;
	uint32_t			tx_frames;
	uint32_t			fcserrors;
	uint32_t			ctrl_errs;
	uint32_t			addr_errs;
	uint32_t			timeouts;
	uint8_t				*data;
	int					len;
	uint8_t				rx_data[EBUS_MTU];
};

#define ATOMIC_ADD(ptr, value)  	\
		__sync_add_and_fetch((uint32_t*)ptr, (uint32_t)value);

#define ATOMIC_CPY(dst, src)  								\
	do {													\
		uint32_t tmp;										\
		tmp = __sync_add_and_fetch((uint32_t*)src, 0);		\
				__sync_sub_and_fetch((uint32_t*)src, tmp);	\
		*(dst) = htonl(tmp);									\
	} while (0);


int ecb_transmit(struct ser_port*, uint8_t, uint8_t, const void*, int);


extern int check_for_dm(uint8_t addr);
extern void *a4ci_thread(void *ctx);
extern struct ser_port *g_port;
extern int ecb_init(void);
extern struct ser_port * sau_init(void);


#endif

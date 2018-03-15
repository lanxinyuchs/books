/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#ifndef LIBECP_INTERNAL_H_
#define LIBECP_INTERNAL_H_

#ifdef __GNUC__
#define likely(x)                __builtin_expect(!!(x), 1)
#define unlikely(x)              __builtin_expect(!!(x), 0)
#else
#define likely(x)                (x)
#define unlikely(x)              (x)
#endif

#define ECP_TX_TMO               500  /* Transmit H/W timeout in ms */


/**
 * Structure to be passed by the calling client as a handle
 */
struct client_data {
	uint32_t dev_no;
	struct ecp_device *edev;
};

struct ecp_tbuff_ref {
	uint32_t tbuff_no_mask;
	struct ecp_connection *econ;
};

struct  ecp_hw_ops {
	void (*uio_addr_offset)(uint32_t *, uint32_t);
	void (*irq_enable)(struct ecp_device *, uint32_t);
	void (*irq_disable)(struct ecp_device *);
	int  (*get_mtu)();
	int  (*get_mru)();
	int  (*create_conn)(void *tref, uint32_t cid,
	                    struct ulh_trans_addr *src,
	                    struct ulh_trans_addr *dst,
	                    void **cref);
	int  (*destroy_conn)(void *tref, void *cref);
	int  (*dev_hard_xmit)(void *, void *, struct ulh_tbuff *);
	void (*irq_callback)(void *);
};

/**
 * ECP device data
 */
struct ecp_device {
	struct ecp_device_stats stats;
	char *name;
	void *uio_handle;
	uint32_t *regs;
	int stop_tx_queue;
	itc_mbox_id_t irq_mbox;
	pthread_mutex_t mutex;
	pthread_cond_t cond;
	struct ecp_connection *conns[ECP_CIDS];
	struct ulh_timerqueue tqueue;
	struct ulh_ref ref;
	struct ecp_hw_ops *hw_ops;
	uint32_t hw_port;
};
void ecp_init_hw_ops(struct ecp_device *edev);
#endif

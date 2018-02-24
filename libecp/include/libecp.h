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
#ifndef LIBECP_H_
#define LIBECP_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __MACHINE_ZYNQMP_MIMO
#define ECP_MAX_NO_OF_BUFFS 2
#else
#define ECP_MAX_NO_OF_BUFFS 5
#endif

/* Counters stepped more frequently should be placed first to increase the
 * chance they populate the same cache line (provided of course that the
 * counters are placed cache-line aligned). */
struct ecp_stats {
	unsigned long   rx_packets;
	unsigned long   tx_packets;
	unsigned long   rx_bytes;
	unsigned long   tx_bytes;
	unsigned long   tx_dropped;
	unsigned long   rx_dropped;
	unsigned long   rx_size_errors;
	unsigned long   tx_addr_errors;
	unsigned long   rx_addr_errors;
	unsigned long   rx_alloc_errors;
	unsigned long   rx_crc_err_cnt;
	unsigned long   tx_packet_err_cnt;
	unsigned long   rx_packet_err_cnt;
	unsigned long   tx_lost_err_cnt;
	unsigned long   rx_lost_err_cnt;
	unsigned long   rx_length_err_cnt;
	unsigned long   rx_leak_bytes;
	unsigned long   rx_lost_without_rec;
	unsigned long   rx_slow_allocs;
	unsigned long   rx_fast_allocs;
};
struct ecp_device_stats {
	struct ecp_stats data[ECP_MAX_NO_OF_BUFFS];
};

/**
 * Initialize ECP interface library
 *
 * @param[out] handle - interface handle, should be passed to all further
 * interfaces calls
 * @param[in] dev - device number, identifies what physical ECP device to use
 *
 * @return 0 - success
 */
int ecp_init(void **handle, uint32_t dev);

/**
 * Shutdown ECP interface library
 *
 * This is last call to the interface, user should make sure that there are
 * no other outstanding calls to the interface (if there are, behaviour is
 * undefined)
 *
 * @param[in] handle - interface handle
 */
void ecp_shutdown(void **handle);

/**
 * Read ECP packet statistic counters
 *
 * @param[in] handle - interface handle
 */
int ecp_stats(void *handle, struct ecp_device_stats *stats);

#ifdef __cplusplus
}
#endif

#endif /* LIBECP_H_ */

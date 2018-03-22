#ifndef __DP_CFG_H
#define __DP_CFG_H

#include <stdint.h>

#define DP_CFG_OFFSET ( ( 64 - 1 ) * 1024 )

struct dp_cfg_link {
	uint32_t tx_shmem_start;
	uint32_t tx_shmem_size;
	uint32_t tx_mbox_id;
	uint32_t rx_shmem_start;
	uint32_t rx_shmem_size;
	uint32_t rx_mbox_id;
};

struct dp_cfg {
	struct dp_cfg_link link_cfg;
};

#endif /* __DP_CFG_H */

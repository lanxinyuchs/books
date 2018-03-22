#ifndef __ULH_SHMEM_CM_H
#define __ULH_SHMEM_CM_H

#include <ulh_cm.h>

/* Describes a shared memory area used for storing messages */
struct ulh_shmem_cm_shmem_config {
	uint32_t start;
	uint32_t size;
};

/* Configuration sent with the ULH_LNHMSG_CREATECM_REQ signal */
struct ulh_shmem_cm_config {
	struct ulh_cm_config common;   /* Only .cfg_size to be filled in */
	uint32_t watchdog_interval_ms; /* How often to ping the other side */
	uint32_t watchdog_ms;          /* How long a ping may take */
	uint32_t tx_mbox;              /* The UIO RX mailbox number (0-31) */
	uint32_t rx_mbox;              /* The UIO TX mailbox number (0-31) */
	struct ulh_shmem_cm_shmem_config tx;
	struct ulh_shmem_cm_shmem_config rx;
};

extern int ulh_shmem_init(const char *name);

#endif /* __ULH_SHMEM_CM_H */

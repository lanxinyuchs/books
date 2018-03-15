#ifndef ULH_MHP_H__
#define ULH_MHP_H__

#include <stdint.h>

struct ulh_cm_mhp_config {
	struct ulh_cm_config cmn;

	int                  rt_tmo;   /* Retransmission timeout (ms). */
	int                  ack_tmo;  /* Ack timeout (ms). */
        int                  kpa_tmo;  /* Keep alive timeout (ms). */
	int                  sup_tmo;  /* Supervision timeout (ms). */
	int                  tx_wnd;   /* Transmit window size (packets). */
	int                  rt_limit; /* Retransmission limit. */
	int                  conn_tmo; /* Connect timeout (ms). */

	int                  active;   /* active\passive; 1=active; 
                                                          0=passive */
	uint16_t             prios;    /* number of priorities */
};

int ulh_mhp_init(const char *name);

#endif /* ULH_MHP_H__ */

#ifndef ULH_HDLC_H__
#define ULH_HDLC_H__

#include <stdint.h>

struct ulh_cm_hdlc_config {
	struct ulh_cm_config cmn;
	int primary;
	uint32_t hdlc_addr; 
};

int ulh_hdlc_init(const char *name);

#endif /* !ULH_HDLC_H__ */

#ifndef ULH_ECMT_H__
#define ULH_ECMT_H__

#include <stdint.h>


int ulh_ecmt_create(const char *name);
int ulh_ecmt_makeaddr(uint8_t *mac, int vlan, const char *devname,
		      struct ulh_trans_addr *addr);
#endif /* ULH_ECMT_H__ */

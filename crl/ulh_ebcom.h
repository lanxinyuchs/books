#ifndef ULH_EBCOM_H__
#define ULH_EBCOM_H__

#include <stdint.h>

int ulh_ebcom_init(const char *name);
int ulh_ebcom_shutdown(void);

int ulh_ebcom_makeaddr(uint8_t *mac,  uint32_t cep,
                       struct ulh_trans_addr *addr);

#endif /* ULH_UDP_H__ */

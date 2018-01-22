#ifndef ULH_LNH_H__
#define ULH_LNH_H__

#include <stdint.h>
#include <itc.h>

struct ulh_lnh;

/* initialize CM, transport frameworks */
int ulh_lnh_init(uint32_t max_connections);

/* create linkhandler instance */
struct ulh_lnh *ulh_lnh_create(const char *name);
struct ulh_lnh *ulh_lnh_create_w_cfg(const char *name,
				     uint32_t max_link,
				     uint32_t max_endp);

/* destroy linkhandler instance */
void ulh_lnh_destroy(struct ulh_lnh *lnh);

/* get linkhandler mailbox */
itc_mbox_id_t ulh_lnh_getmbox(struct ulh_lnh *lnh);

#endif /* ULH_LNH_H__ */

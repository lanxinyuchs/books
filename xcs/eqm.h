#ifndef _EQM_H
#define _EQM_H

#include <stdint.h>
#include <itc.h>

struct eqm_args {
	itc_mbox_id_t server_mbox;
	uint32_t id;
};

#define EQM_INSTANCE_CREATED_SUCCESS 0xff
#define EQM_INSTANCE_CREATED_FAIL    0xfe

extern void *xenon_dp_thrfxn(void *arg);

#endif /* _EQM_H */

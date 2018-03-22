#ifndef __RIOCMD_API_H_
#define __RIOCMD_API_H_

#include <linux/types.h>

#include <ulh_lnh_msg.h>

#define RIOCMD_SERVER_NAME        " emcalnh_dux1"

#define RIOCMD_MSG_DATA_MAX       1024
#define RIOCMD_REQ_MAX_SZ         (sizeof(struct req_hdr) + RIOCMD_MSG_DATA_MAX)

#define RIOCMD_REQ_CREATE_LINK    0
#define RIOCMD_REQ_DESTROY_LINK   1
#define RIOCMD_LINK_UP_EVENT      2
#define RIOCMD_LINK_DOWN_EVENT    3

struct req_hdr {
        __u32                     id;
        __u32                     sz;
        __u32                     seq;
        __u32                     lid;
	__u32                     result;
	char                      link_name[ULH_LNHNAMESIZ];
};

struct riocmd_create_link {
	struct req_hdr            hdr;
	__u32                     snid;
	__u32                     dbg_bits;
	__u8                      reserved[64];   /* future use */
};

union riocmd_msg {
	struct req_hdr                hdr;
	struct riocmd_create_link     create;
	char                          data[RIOCMD_REQ_MAX_SZ];
};


#endif

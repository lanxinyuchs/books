#ifndef ULH_LNH_INTERNAL_H__
#define ULH_LNH_INTERNAL_H__

#include <stdint.h>
#include "ulh_cm.h"

#define ULH_LNHNAMESIZ	64
#define ULH_LINK_UP      0
#define ULH_LINK_DOWN    1

#define ULH_LNHMSG_SHUTDOWN			(0x0190040f)
struct ulh_lnhmsg_shutdown {
	uint32_t msg_no;
};

#define ULH_LNHMSG_QUERY			(0x01900410)
struct ulh_lnhmsg_query {
	uint32_t msg_no;
	char name[ULH_LNHNAMESIZ];
};

#define ULH_LNHMSG_LMBOX_DEAD		(0x01900411)

#define ULH_LNHMSG_CREATELINK_REQ	(0x01900412)
struct ulh_lnhmsg_createlink_req {
	uint32_t msg_no;
	unsigned long seq;
	char name[ULH_LNHNAMESIZ];
	uint32_t prio;
	uint64_t cmid;
};

#define ULH_LNHMSG_CREATELINK_RSP	(0x01900413)
struct ulh_lnhmsg_createlink_rsp {
	uint32_t msg_no;
	unsigned long seq;
	uint32_t result;
	uint32_t lid;
};

#define ULH_LNHMSG_DESTROYLINK_REQ	(0x01900414)
struct ulh_lnhmsg_destroylink_req {
	uint32_t msg_no;
	unsigned long seq;
	uint32_t lid;
};

#define ULH_LNHMSG_DESTROYLINK_RSP	(0x01900415)
struct ulh_lnhmsg_destroylink_rsp {
	uint32_t msg_no;
	unsigned long seq;
	uint32_t result;
	uint32_t lid;
};

#define ULH_LNHMSG_CREATECM_REQ		(0x01900416)
struct ulh_lnhmsg_createcm_req {
	uint32_t msg_no;
	unsigned long seq;
	char cm_name[ULH_LNHNAMESIZ];
	char cm_instance[ULH_LNHNAMESIZ];
	uint8_t config[1]; /* should be last (variable size) */
};

#define ULH_LNHMSG_CREATECM_RSP		(0x01900417)
struct ulh_lnhmsg_createcm_rsp {
	uint32_t msg_no;
	unsigned long seq;
	uint64_t cmid;
	uint32_t result;
};

#define ULH_LNHMSG_NOTIFY			(0x01900418)
struct ulh_lnhmsg_notify {
	uint32_t msg_no;
	uint32_t lid;
	uint32_t state;
};

#define ULH_LNHMSG_OWNER_DEAD		(0x01900419)

#define ULH_INFO_LINK_SUMMARY  0
#define ULH_INFO_LINK_DETAILED 1
#define ULH_INFO_LOCAL_EP      2
#define ULH_INFO_REMOTE_EP     3

#define ULH_LNHMSG_INFO_REQ		(0x0190041A)
struct ulh_lnhmsg_info_req {
	uint32_t msg_no;
	int info_type;
	int info_input;
	char info_name[1];
};

#define ULH_LNHMSG_INFO_RSP		(0x0190041B)
struct ulh_lnhmsg_info_rsp {
	uint32_t msg_no;
	int result;
	int last;
	char infotext[1];  /* should be last (variable size) */
};

#endif /* ULH_LNH_INTERNAL_H__ */

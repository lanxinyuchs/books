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

#define ULH_INFO_LINK_SUMMARY          0
#define ULH_INFO_LINK_DETAILED         1
#define ULH_INFO_LOCAL_EP              2
#define ULH_INFO_REMOTE_EP             3
#define ULH_INFO_ENABLE_THROUGHPUT     4
#define ULH_INFO_DISABLE_THROUGHPUT    5
#define ULH_INFO_DISPLAY_THROUGHPUT    6

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

#define ULH_LNH_LINK_CREATE_SUCCESS     (0x01900426)
struct ulh_lnh_link_create_success {
        uint32_t msg_no;
};

#define ULH_LNH_LINK_EXIT               (0x01900427)
struct ulh_lnh_link_exit {
        uint32_t msg_no;
};

#define ULH_LNH_LINK_FORWARD_REMOTEMSG   (0x01900428)
struct ulh_lnh_link_forward_remotemsg {
        uint32_t msg_no;
        struct ulh_lnh *lnh;
        union itc_msg *msg;
        itc_mbox_id_t who;
};

#define ULH_LNH_LINK_FORWARD_TRANSMSG (0x01900429)
struct ulh_lnh_link_forward_transmsg {
        uint32_t msg_no;
        struct ulnh_link *link;
        union itc_msg *msg;
};

#define ULH_LNH_LINK_FORWARD_LOCATE             (0x0190042a)
struct ulh_lnh_link_forward_locate {
        uint32_t msg_no;
        itc_mbox_id_t from;
        char src_name[ULH_LNHNAMESIZ];
        char org_name[ULH_LNHNAMESIZ];
};


#define ULH_LNH_LINK_FORWARD_LEP_FOUND   (0x0190042b)
struct ulh_lnh_link_forward_lep_found {
        uint32_t msg_no;
        uint32_t mbox_id;
};

#define ULH_LNH_LINK_FORWARD_LEP_DEAD    (0x0190042c)
struct ulh_lnh_link_forward_lep_dead {
        uint32_t msg_no;
        uint32_t amap;
};

#define ULH_LNH_FORWARD_LEP_MONITOR      (0x0190042d)
struct ulh_lnh_forward_lep_monitor {
        uint32_t msg_no;
        itc_mbox_id_t mbox;
};

#endif /* ULH_LNH_INTERNAL_H__ */

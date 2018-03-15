#ifndef ULH_LNH_INTERNAL_H__
#define ULH_LNH_INTERNAL_H__

#include <stdint.h>
#include <itc.h>
#include "ulh_cm.h"

#define ULH_LNHNAMESIZ	64
#define ULH_LINK_UP      0
#define ULH_LINK_DOWN    1

struct ulh_query {
	char name[ITC_NAME_MAXLEN];
};

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
        bool info_first;
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

#define ULH_LNHMSG_LINK_INFO_REQ      (0x0190041C)
struct ulh_lnhmsg_link_info_req {
	uint32_t msg_no;
	int info_type;
	int info_input;
	char info_name[1];
};

#define ULH_LNHMSG_LINK_INFO_RSP      (0x0190041D)
struct ulh_lnhmsg_link_info_rsp {
	uint32_t msg_no;
        uint32_t result;
        uint32_t link_mboxid[32];
	char infotext[1];  /* should be last (variable size) */
};

#define ULH_LNHMSG_LINK_THROUGHPUT      (0x0190041E)
struct ulh_lnhmsg_link_throughput {
	uint32_t msg_no;
        int action;
};

#define ULH_LNH_LINK_CREATE_SUCCESS     (0x01900426)
struct ulh_lnh_link_create_success {
        uint32_t msg_no;
};

#define ULH_LNH_LINK_EXIT               (0x01900427)
struct ulh_lnh_link_exit {
        uint32_t msg_no;
};

#define ULH_LNH_LINK_SHUTDOWN           (0x01900428)
struct ulh_lnh_link_shutdown {
        uint32_t msg_no;
};

#define ULH_LNH_TMP_MBOX_CREATED        (0x0190042E)
struct ulh_lnh_tmp_mbox_created {
        uint32_t msg_no;
};

#define ULH_LNHMSG_ENABLE_DIAG          (0x0190042F)
struct ulh_lnhmsg_enable_diag {
        uint32_t msg_no;
        uint32_t mailbox_watermark_minimum;
        uint32_t mailbox_watermark_delay;
        uint32_t mailbox_msg_type_delay;
};

#define ULH_LNHMSG_CREATELINK2_REQ      (0x01900436)
struct ulh_lnhmsg_createlink2_req {
	uint32_t msg_no;
	unsigned long seq;
	char name[ULH_LNHNAMESIZ];
	uint32_t prio;
	uint64_t cmid;
	uint8_t filter_queries;
	uint32_t allowed_queries_size;
	struct ulh_query allowed_queries[1];
};

#define ULH_LNHMSG_CREATELINK2_RSP      (0x01900437)
struct ulh_lnhmsg_createlink2_rsp {
	uint32_t msg_no;
	unsigned long seq;
	uint32_t result;
	uint32_t lid;
};

#endif /* ULH_LNH_INTERNAL_H__ */

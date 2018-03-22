
/**
 * {GENERATED FILE, DO NOT EDIT}
 *
 *
    Interwork description for RHAMI_LH

 *
 * Prepared:
 * Infotype:
 * Responsible:
 * Docno:
 * Checked:
 * Rev:
 *
 * Documentation: {STILL TO BE ADDED USING XSLT}.
 *
 */

#ifndef _RHAMI_LH_H_
#define _RHAMI_LH_H_
#include <itc.h>
#include "rhami_bitmap.h"
#include "rhd-msg-base.h"
#include "conn-establish.h"

#define RHAMI_LH_MAILBOX             "RHAMI_LH"
#define LH_SERVER_VERSIONS           1

#define RHAMI_LH_STRUCTS       \
	struct lh_startlink_req startlink_req;     \
	struct lh_startlink_cfm startlink_cfm;     \
	struct lh_startlink_rej startlink_rej;     \
	struct lh_ecpstats_req ecpstat_req;     \
	struct lh_ecpstats_rej ecpstat_rej;     \
	struct lh_ecpstats_cfm ecpstat_cfm;     \


#define RHAMI_LH_CONN_ESTABLISH_REQ    (RHD_LH_MSG_BASE + 0x01)
#define RHAMI_LH_CONN_ESTABLISH_CFM    (RHD_LH_MSG_BASE + 0x02)
#define RHAMI_LH_CONN_ESTABLISH_REJ    (RHD_LH_MSG_BASE + 0x03)
#define RHAMI_LH_CONN_DISCONNECT_REQ   (RHD_LH_MSG_BASE + 0x04)
#define RHAMI_LH_CONN_DISCONNECT_CFM   (RHD_LH_MSG_BASE + 0x05)
#define RHAMI_LH_CONN_DISCONNECT_REJ   (RHD_LH_MSG_BASE + 0x06)
#define RHAMI_LH_CONN_MONITOR_FWD      (RHD_LH_MSG_BASE + 0x07)

#define RHAMI_LH_MSG_HEADER \
	uint32_t msgno;         \
	uint32_t procedure_ref; \
	uint32_t connection_ref;

enum lh_result {
      LH_RESULT_OK = 0,
      LH_RESULT_FAILURE = 1,
};

#define RHAMI_LH_STARTLINK_REQ         (RHD_LH_MSG_BASE + 0x09)
struct lh_startlink_req {
	RHAMI_LH_MSG_HEADER
	int32_t port;
	int32_t channel_id;
	int32_t buff_idx;
};

#define RHAMI_LH_STARTLINK_CFM         (RHD_LH_MSG_BASE + 0x0A)
struct lh_startlink_cfm {
	RHAMI_LH_MSG_HEADER
	int32_t port;
	int32_t channel_id;
};

#define RHAMI_LH_STARTLINK_REJ         (RHD_LH_MSG_BASE + 0x0B)
struct lh_startlink_rej{
	RHAMI_LH_MSG_HEADER
};

#define RHAMI_LH_ECPSTATS_REQ         (RHD_LH_MSG_BASE + 0x0C)
struct lh_ecpstats_req {
	RHAMI_LH_MSG_HEADER
	uint32_t  buff_idx;
};

#define RHAMI_LH_ECPSTATS_CFM         (RHD_LH_MSG_BASE + 0x0D)
struct lh_ecpstats_cfm {
	RHAMI_LH_MSG_HEADER
	char buffer[1];
};

#define RHAMI_LH_ECPSTATS_REJ         (RHD_LH_MSG_BASE + 0x0E)
struct lh_ecpstats_rej {
	RHAMI_LH_MSG_HEADER
};

void rhami_lh_send_startlink_rsp(itc_mbox_id_t destination, uint32_t client_ref,
                                 int32_t port, int32_t channel_id, uint32_t procedure_ref,
                                 enum lh_result result);

#endif //_RHAMI_LH_H_

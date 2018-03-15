/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <pthread.h>
#include <sys/queue.h>
#include <asm/types.h>
#include <sys/socket.h>
#include <linux/netlink.h>
#include <sys/reboot.h>
#include <itc.h>
#include <itc_system.h>
#include <rhami_lh_spec.h>
#include <libecp.h>
#include <ulh_lnh.h>
#include <ulh_lnh_msg.h>
#include <ulh_hdlc.h>
#include <evti.h>
#include "rhd-common.h"
#include "client_info.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_lh
#include "tpt_create.h"
#include "tpt.h"

#define HDLC_CM_NAME            "hdlccm"
#define HDLC_LNH_NAME           "hdlc_lnh"

#define DAEMON_NAME             "rhd-lhd"


/* The MAILBOX_SIZE needs to cover both internal and possible ITCLNH mailboxes
 * created as a result of clones (remote processes). Currently the number of
 * supported REPs (remote end points) per link in ITCLNH is 64.
 * But seriously, there must be a better solution than for the user having to
 * hard-code knowledge like this! This definition should be revisted as soon
 * as possible when a proper solution exists! */
#define MAILBOX_SIZE            128

#define MAX_DEV_NAME            7       /* lhnn */
#define STRINGIFY(EXPR)         (#EXPR)
#define XSTRINGIFY(EXPR)        STRINGIFY(EXPR)
#define LH_UIO_DEV              "ecp"
#ifdef __MACHINE_ZYNQMP_MIMO
#define LH_MAX_DEVICES		7
#define ECP_MAX_NOF_BUFFS	2
#else
#define LH_MAX_DEVICES          2
#define ECP_MAX_NOF_BUFFS       5
#endif

/**
 * Add 1, one for the '_''
 * (Actually we have two slack spaces already because of the two '\0'
 * in the string constants, but to be clear we add 1)
 */
#define MAILBOX_NAME_LENGTH     (sizeof(RHAMI_LH_MAILBOX) + 6 + 1)
#define DAEMON_NAME_LENGTH      (sizeof(DAEMON_NAME) + 6 + 1)
#define ECP_DEV_NAME_LENGTH     (sizeof(LH_UIO_DEV) + 6 + 1)

#define LNHNAME "linx_lnh"

#define RHD_LOCATE_TIMEOUT     2000

#define NOF_PATH_TAGS (LH_MAX_DEVICES + LH_MAX_DEVICES * ECP_MAX_NOF_BUFFS)


union itc_msg {
	uint32_t            msgno;
	RHAMI_LH_STRUCTS

	struct ulh_lnhmsg_createcm_req          cmreq;
	struct ulh_lnhmsg_createcm_rsp          cmrsp;
	struct ulh_lnhmsg_createlink_req        lcreq;
	struct ulh_lnhmsg_createlink_rsp        lcrsp;
	struct ulh_lnhmsg_destroylink_req       ldreq;
	struct ulh_lnhmsg_destroylink_rsp       ldrsp;
	struct ulh_lnhmsg_notify                ulind;

	struct itc_lnh_added   itc_lnh_added;
	struct itc_lnh_removed itc_lnh_removed;

	struct EVTI_DistributeIndS evti_distribute_ind;
};

struct conn_establish_msg_numbers  lh_server_conn_messages = {
	RHAMI_LH_CONN_ESTABLISH_REQ,
	RHAMI_LH_CONN_ESTABLISH_CFM,
	RHAMI_LH_CONN_ESTABLISH_REJ,
	RHAMI_LH_CONN_DISCONNECT_REQ,
	RHAMI_LH_CONN_DISCONNECT_CFM,
	RHAMI_LH_CONN_DISCONNECT_REJ,
	RHAMI_LH_CONN_MONITOR_FWD
};


static itc_mbox_id_t lh_mbox = ITC_NO_ID;
static itc_mbox_id_t hdlc_lnh_mbox = ITC_NO_ID;
static void *ecp_handle = NULL;
static void *lnh_handle = NULL;
static char daemon_name[DAEMON_NAME_LENGTH];
static int lh_device = 0;
static int lh_max_devices = 0;
static int ecp_max_nof_buffs = 0;
static int mailbox_name_length = 0;
static int daemon_name_length = 0;
static int ecp_dev_name_length = 0;
static int primary = 0;
#ifdef __MACHINE_ZYNQMP_MIMO
static const struct {
	const int   lh_device;
	const char *path;
	const char *tag_postfix;
} path_tag[NOF_PATH_TAGS] = {
	{ 0, "bp0/", "0" },
	{ 1, "bp1/", "1" },
        { 2, "bp2/", "2" },
        { 3, "trxm0/", "3" },
        { 4, "trxm1/", "4" },
        { 5, "trxm2/", "5" },
        { 6, "trxm3/", "6" }
};
#else
static const struct {
        const int   lh_device;
        const char *path;
        const char *tag_postfix;
} path_tag[NOF_PATH_TAGS] = {
        { 0, "bp0/", "0" },
        { 1, "bp1/", "1" },
        { 0, "bp0_0/", "0_0" },
        { 0, "bp0_1/", "0_1" },
        { 0, "bp0_2/", "0_2" },
        { 0, "bp0_3/", "0_3" },
        { 0, "bp0_4/", "0_4" },
        { 1, "bp1_0/", "1_0" },
        { 1, "bp1_1/", "1_1" },
        { 1, "bp1_2/", "1_2" },
        { 1, "bp1_3/", "1_3" },
        { 1, "bp1_4/", "1_4" }
};
#endif


struct link_data {
	uint32_t port;
	uint32_t cid;
	uint32_t lid;
	char *cname;
	char *lname;
};


/**
 * Perform a blocking ITC locate, which times out after RHD_LOCATE_TIMEOUT milliseconds.
 * The caller should have a valid mailbox initialized.
 */
static itc_mbox_id_t rhd_itc_locate_blocking(const char *mailbox)
{
	static itc_mbox_id_t mbox = ITC_NO_ID;
	uint32_t rx_filter[2] = {1, ITC_LOCATE_DEFAULT_NO};
	union itc_msg *msg;

	if (itc_get_fd() < 0) {
		TPT_ERROR("Local ITC mailbox not initialized");
		return mbox;
	}

	itc_locate_async(mailbox, NULL, ITC_MY_MBOX);

	msg = itc_receive(rx_filter, RHD_LOCATE_TIMEOUT, ITC_FROM_ALL);
	if (msg) {
		switch (msg->msgno) {
		case ITC_LOCATE_DEFAULT_NO:
			mbox = itc_sender(msg);
			break;
		default:
			break;
		}
		itc_free(&msg);
	}
	return mbox;
}


#define ECP_STATS_TEXT(s)						\
	"%-9s %-9s %-19s%-10s %-19s%-10s\n\r"				\
	"%-9s %-9s %-29s %-29s\n\r"					\
	"%-9s %-9s %-19s%-10lu %-19s%-10lu\n\r"				\
	"%-9s %-9s %-19s%-10lu\n\r"					\
	"%-9s %-9s %-19s%-10lu %-19s%-10lu\n\r"				\
	"%-9s %-9s %-19s%-10lu %-19s%-10lu\n\r"				\
	"%-9s %-9s %-19s%-10lu\n\r"					\
	"%-9s %-9s %-19s%-10lu %-19s%-10lu\n\r"				\
	"%-9s %-9s %-19s%-10lu\n\r"					\
	"\n\rNote 1: Statistics may be invalid due to overflow (32 bits).\n\r" \
	"Note 2: Ongoing link communication may"                        \
	"confuse interpretation of statistics.\n\r",\
		"Procedure", "HDLC addr", "Received","No","Transmitted","No", \
		"---------", "---------", "-----------------------------", \
		"-----------------------------",			\
		"All", "All","Bytes",(s).rx_bytes,"Bytes",(s).tx_bytes,	\
		"", "","Err,FCS",(s).rx_crc_err_cnt,			\
		"", "","Err,packet",(s).rx_packet_err_cnt,"",(s).tx_packet_err_cnt, \
		"", "","Err,dropped",(s).rx_dropped,"", (s).tx_dropped, \
		"", "","Err,length",(s).rx_length_err_cnt,			\
		"", "","Err,lost_error",(s).rx_lost_err_cnt,"",(s).tx_lost_err_cnt, \
		"", "","Err,lostWithoutRec",(s).rx_lost_without_rec


static char *format_ecpstats(uint32_t buff_idx)
{

	struct ecp_device_stats stats;
	uint32_t len = 0;
	char *stats_str = NULL;

	if (!ecp_handle || ecp_stats(ecp_handle, &stats)) {
		TPT_ERROR("invalid ecp_handle");
		return NULL;

	}

	len = snprintf(NULL, 0, ECP_STATS_TEXT(stats.data[buff_idx]));

	stats_str = (char *)malloc(len + 1);
	if (stats_str == NULL) {
		TPT_ERROR("failed to malloc");
		return NULL;
	}

	snprintf(stats_str, len, ECP_STATS_TEXT(stats.data[buff_idx]));

	stats_str[len] = '\0';

	return stats_str;

}

static void handle_ecpstats(union itc_msg  *msg, uint32_t client_ref)
{

	union itc_msg *reply;
	uint32_t msg_len;
	char *format_str;

	format_str = format_ecpstats(msg->ecpstat_req.buff_idx);
	if (format_str == NULL) {
		reply = itc_alloc(sizeof(struct  lh_ecpstats_rej),
		                  RHAMI_LH_ECPSTATS_REJ);
		reply->ecpstat_rej.connection_ref = client_ref;
		reply->ecpstat_rej.procedure_ref = msg->ecpstat_req.procedure_ref;
		itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
		return;
	}

	msg_len = snprintf(NULL, 0, "%s", format_str);

	reply = itc_alloc(sizeof(struct  lh_ecpstats_cfm) + msg_len + 1,
	                  RHAMI_LH_ECPSTATS_CFM);

	strncpy(reply->ecpstat_cfm.buffer, format_str, msg_len);

	reply->ecpstat_cfm.buffer[msg_len] = '\0';
	reply->ecpstat_cfm.connection_ref = client_ref;
	reply->ecpstat_cfm.procedure_ref = msg->ecpstat_req.procedure_ref;

	itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);

	free(format_str);

}

static void distribute_tag(const char *tag_prefix, const char *tag_postfix)
{
	itc_mbox_id_t  mbox;
	union itc_msg *msg;

	mbox = itc_locate(EVTI_SERVER_NAME);
	if (mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Could not locate \"%s\"", EVTI_SERVER_NAME));
		exit(-ENOSYS);
	} else {
		msg = itc_alloc(sizeof(EVTI_DistributeIndS),
		                EVTI_DISTRIBUTE_IND);
		snprintf(msg->evti_distribute_ind.tag,
		         sizeof(msg->evti_distribute_ind.tag),
		         "%s%s", tag_prefix, tag_postfix);
		msg->evti_distribute_ind.data[0] = '\0';
		TPT_SEND_SIG(msg->msgno, mbox,
		             STR("EVTI_DISTRIBUTE_IND: tag=\"%s\"",
		                 msg->evti_distribute_ind.tag));
		itc_send(&msg, mbox, ITC_MY_MBOX);
	}
}

static void handle_lnh_event(const char *ev_path, const char *tag_prefix)
{
	int idx;

	for (idx = 0; idx < NOF_PATH_TAGS; idx++) {
		if (path_tag[idx].path != NULL){
		if (lh_device == path_tag[idx].lh_device &&
		    strcmp(ev_path, path_tag[idx].path) == 0) {
			distribute_tag(tag_prefix, path_tag[idx].tag_postfix);
			break;
		}
		}
	}
}

static void lh_shutdown()
{

	TPT_INFO(STR("Closing down: %s", DAEMON_NAME));

	if (lnh_handle)
		ulh_lnh_destroy(lnh_handle);
	if (ecp_handle)
		ecp_shutdown(&ecp_handle);

	if (lh_mbox != ITC_NO_ID)
		itc_delete_mailbox(lh_mbox);

}

static int lh_init(uint32_t device)
{
	char mailboxname[MAILBOX_NAME_LENGTH];
	char lnhname[MAILBOX_NAME_LENGTH];
	int ret;

	/* Initialize ITC */
	itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if (snprintf(mailboxname, mailbox_name_length, "%s_%d", RHAMI_LH_MAILBOX,
	             device) < 0) {
		TPT_ERROR("Unable to initialize mailbox name");
		goto error_out;
	}

	/* Create our mailbox. */
	lh_mbox = itc_create_mailbox(mailboxname, 0);
	if (lh_mbox == ITC_NO_ID)
		goto error_out;

	ret = ulh_lnh_init(RHD_NOF_ECP_ADDR);
	if (ret) {
		TPT_ERROR(STR("ulh_lnh_init() failed, %d", ret));
		goto error_out;
	}

	ret = ulh_hdlc_init(HDLC_CM_NAME);
	if (ret) {
		TPT_ERROR(STR("ulh_ecb_init() failed, %d", ret));

		goto error_out;
	}

	if (snprintf(lnhname, mailbox_name_length, "%s%d", HDLC_LNH_NAME,
	             device) < 0) {
		TPT_ERROR("Unable to initialize lnh name");
		goto error_out;
	}

	lnh_handle = ulh_lnh_create(lnhname);
	if (!lnh_handle) {
		TPT_ERROR("ulh_lnh_create() failed");
		goto error_out;
	}

	for (;; sleep(1)) {
		hdlc_lnh_mbox = rhd_itc_locate_blocking(lnhname);
		if (hdlc_lnh_mbox != ITC_NO_ID)
			break;
	}

	if (ecp_init(&ecp_handle, device))
		goto error_out;

	itc_subscribe_events(ITC_EVENT_LNH_ADDED | ITC_EVENT_LNH_REMOVED);

	return 0;
error_out:
	TPT_ERROR("init failed");
	lh_shutdown();
	return -1;
}

static void free_link_data(struct link_data *p)
{
	if (p) {
		if (p->cname)
			free(p->cname);
		if (p->lname)
			free(p->lname);
		free(p);
	}
}

static int
create_connection(__attribute__((unused)) uint32_t port,
                  __attribute__((unused)) uint32_t cid, __attribute__((unused)) char **s)
{
	return 0;
}

static int
create_link(conn_server_handle_t handle,
            struct conn_client_info *client_info, uint32_t port, uint32_t cid,
            uint32_t buffer_index)
{

	char devicename[ECP_DEV_NAME_LENGTH];
	uint32_t select[2] = {1, ULH_LNHMSG_CREATECM_RSP};
	struct ulh_cm_hdlc_config *cfp;
	union itc_msg *req;
	struct link_data *p;
	uint32_t cmid;
	uint32_t lid;
	int ret;

	memset(devicename, 0, ECP_DEV_NAME_LENGTH);

	snprintf(devicename, ecp_dev_name_length, "%s%d", LH_UIO_DEV, lh_device);

	/* Only one link/client supported (should be extended) */
	if (client_info->client_data) {
		p = client_info->client_data;
		if (p->cid == buffer_index && p->port == port) {
			TPT_INFO(STR("reused connection '%s'", p->cname));
			TPT_INFO(STR("reused link '%s'", p->lname));
			return 0;
		}
		return -EBUSY;
	}

	struct ulh_trans_addr ecp_addr;
	memset(&ecp_addr, 0, sizeof(ecp_addr));
	ecp_addr.data[0] = cid;
	ecp_addr.data[1] = buffer_index;
	int32_t trans_cid;

	trans_cid = ulh_trans_create_conn(devicename, &ecp_addr, &ecp_addr);
	if (trans_cid < 0) {
		TPT_ERROR("ulh_trans_create_conn() failed");
		return -EFAULT;
	}

	p = malloc(sizeof(struct link_data));
	if (!p) {
		TPT_ERROR("failed to allocate link data");
		return -ENOMEM;
	}
	p->cname = NULL;
	p->lname = NULL;

	req = itc_alloc(sizeof(req->cmreq) + sizeof(*cfp), ULH_LNHMSG_CREATECM_REQ);
	cfp = (struct ulh_cm_hdlc_config *)&req->cmreq.config;
	cfp->cmn.cfg_size               = sizeof(*cfp);
	cfp->cmn.cfg_version            = 0;
	cfp->cmn.cid                    = trans_cid;
	cfp->cmn.uref                   = 0;
	cfp->cmn.mbox                   = 0;
	if (primary == 1) {
		cfp->primary                    = 1;
		cfp->hdlc_addr                  = 3;
	}
	else {
		cfp->primary                    = 0;
		cfp->hdlc_addr                  = 0;
	}
	req->cmreq.seq                  = 0;
	strcpy(req->cmreq.cm_name, HDLC_CM_NAME);
	snprintf(req->cmreq.cm_instance, ULH_LNHNAMESIZ, "hdlccon_%d_%d", lh_device,
	         buffer_index);
	p->cname = strdup(req->cmreq.cm_instance);

	TPT_TRACE(3, STR("cfg_size:%d, cfg_version:%d, "
	                 "cid:%d, uref:%lu, mbox:%d, "
	                 "primary:%d, hdlc_addr:%d, "
	                 "seq:%lu, cm_name:%s, cm_instance:%s",
	                 cfp->cmn.cfg_size,
	                 cfp->cmn.cfg_version,
	                 cfp->cmn.cid,
	                 cfp->cmn.uref,
	                 cfp->cmn.mbox,
	                 cfp->primary,
	                 cfp->hdlc_addr,
	                 req->cmreq.seq,
	                 req->cmreq.cm_name,
	                 req->cmreq.cm_instance));
	itc_send(&req, hdlc_lnh_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATECM_RSP;
	req = itc_receive(select, 2000, hdlc_lnh_mbox);

	if ((req == NULL) || (req->cmrsp.result)) {
		ret = -EIO;
		if (req) {
			TPT_TRACE(3, STR("result:%d", req->cmrsp.result));
			ret = req->cmrsp.result;
			TPT_ERROR(STR("failed to create connection '%s'", p->cname));
			free_link_data(p);
			itc_free(&req);
		}
		return ret;
	}

	TPT_TRACE(3, STR("result:%d, cmid:%llu", req->cmrsp.result, req->cmrsp.cmid));
	cmid = req->cmrsp.cmid;
	TPT_INFO(STR("created connection '%s'", p->cname));
	itc_free(&req);

	req = itc_alloc(sizeof(req->lcreq), ULH_LNHMSG_CREATELINK_REQ);
	req->lcreq.prio = 0;
	req->lcreq.cmid = cmid;
	if (strcmp("BP", getenv("SYS_BOARD_TYPE")) == 0)
	{
		if (lh_device < 3)
			snprintf(req->lcreq.name, ULH_LNHNAMESIZ, "bp%d", lh_device);	
		else
			snprintf(req->lcreq.name, ULH_LNHNAMESIZ, "trxm%d", lh_device-3);
	}
	else
	{
		if(buffer_index == 0)
			snprintf(req->lcreq.name, ULH_LNHNAMESIZ, "bp%d", lh_device);
		else
			snprintf(req->lcreq.name, ULH_LNHNAMESIZ, "bp%d_%d", lh_device, buffer_index);
	}

	p->lname = strdup(req->lcreq.name);
	TPT_TRACE(3, STR("prio:%d, cmid:%llu", req->lcreq.prio, req->lcreq.cmid));
	itc_send(&req, hdlc_lnh_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATELINK_RSP;
	req = itc_receive(select, 2000, hdlc_lnh_mbox);

	if ((req == NULL) || (req->lcrsp.result)) {
		ret = -EIO;
		if (req) {

			TPT_TRACE(3, STR("result:%d", req->lcrsp.result));
			ret = req->lcrsp.result;
			TPT_TRACE(3, STR("failed to create link '%s'", p->lname));
			free_link_data(p);
			itc_free(&req);
		}
		return ret;
	}

	TPT_TRACE(3, STR("result:%d, lid:%d", req->lcrsp.result, req->lcrsp.lid));
	lid = req->lcrsp.lid;
	itc_free(&req);

	TPT_INFO(STR("created link '%s' (lid:%d)", p->lname, lid));

	p->port = port;
	p->cid = buffer_index;
	p->lid = lid;
	client_info->client_data = p;

	ret = conn_set_client_data(handle, client_info->server_ref, p);
	if (ret != CONN_ESTABLISH_CLIENT_DATA_OK)
		TPT_INFO("Failed to set client data");
	return 0;
}

static int destroy_connection(__attribute__((unused)) uint32_t port,
                              __attribute__((unused)) uint32_t cid)
{
	return 0;
}

static int destroy_link(struct conn_client_info *client_info, uint32_t port,
                        uint32_t cid)
{

	uint32_t select[2] = {1, ULH_LNHMSG_DESTROYLINK_RSP};
	struct link_data *p;
	union itc_msg *req;

	if (!client_info->client_data)
		return -EFAULT;
	p = client_info->client_data;
	if (p->cid != cid || p->port != port)
		return -EFAULT;

	req = itc_alloc(sizeof(req->ldreq), ULH_LNHMSG_DESTROYLINK_REQ);
	req->ldreq.lid = p->lid;

	TPT_TRACE(3, STR("lid:%d", req->ldreq.lid));
	itc_send(&req, hdlc_lnh_mbox, ITC_MY_MBOX);
	req = itc_receive(select, 2000, hdlc_lnh_mbox);
	if ((req == NULL) || (req->ldrsp.result)) {
		int ret = -EIO;
		if (req) {
			TPT_TRACE(3, STR("result:%d", req->ldrsp.result));
			ret = req->ldrsp.result;
			TPT_INFO(STR("failed to remove link '%s'", p->lname));
			itc_free(&req);
		}
		return ret;
	}

	TPT_TRACE(3, STR("result:%d", req->ldrsp.result));
	itc_free(&req);
	TPT_INFO(STR("removed connection '%s'", p->cname));
	TPT_INFO(STR("removed link '%s' (lid:%d)", p->lname, p->lid));
	free_link_data(p);

	if (client_info->client_data != NULL) {
		if (conn_clear_client_data(client_info->server_handle,
		                           client_info->server_ref,
		                           (void **)&client_info) != CONN_ESTABLISH_CLIENT_DATA_OK)
			TPT_INFO("failed to clear conn client data");
	}

	return 0;
}

/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{
	TPT_TRACE(1, STR("Client with mailbox 0x%08x, server_ref 0x%08x and "
	                 "client_ref 0x%08x has %s.",
	                 client_info->connected_mailbox,
	                 client_info->server_ref,
	                 client_info->client_ref,
	                 client_info->state ==
	                 CONN_ESTABLISH_STATUS_DISCONNECTING ?
	                 "disconnected." : "died (or forgot to disconnect.)"));

	struct link_data *p;
	p = client_info->client_data;
	if (p) {
		destroy_link(client_info, p->port, p->cid);
	}
}

static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	struct conn_event_callbacks cb = { NULL, client_disconnect,
		                client_disconnect, NULL
	};

	uint32_t supported_versions[] = {LH_SERVER_VERSIONS};
	int conn_result = conn_establish_server_init( &handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &lh_server_conn_messages, 0, &cb);

	if( conn_result != CONN_INIT_OK)
		return NULL;

	return handle;
}

/**
 *
        Response to a request to start a link handler instance.

 */
void rhami_lh_send_startlink_rsp(itc_mbox_id_t destination, uint32_t client_ref,
                                 int32_t port, int32_t channel_id, uint32_t procedure_ref,
                                 enum lh_result result)
{
	union itc_msg *requestmsg;

	if (result == LH_RESULT_OK) {
		requestmsg = itc_alloc(sizeof(struct lh_startlink_cfm),
		                       RHAMI_LH_STARTLINK_CFM);
		requestmsg->startlink_cfm.port = port;
		requestmsg->startlink_cfm.channel_id = channel_id;
		requestmsg->startlink_cfm.connection_ref = client_ref;
		requestmsg->startlink_cfm.procedure_ref = procedure_ref;
	}
	else {
		requestmsg = itc_alloc(sizeof(struct lh_startlink_rej),
		                       RHAMI_LH_STARTLINK_REJ);
		requestmsg->startlink_rej.connection_ref = client_ref;
		requestmsg->startlink_rej.procedure_ref = procedure_ref;
	}

	itc_send(&requestmsg, destination, ITC_MY_MBOX);
}

static bool handle_events(union itc_msg* msg)
{
	bool ret = true;
	switch (msg->msgno)
	{
	case ITC_LNH_ADDED:
		handle_lnh_event(msg->itc_lnh_added.lnhpath,
		                 "XHP_LinkUp");
		break;

	case ITC_LNH_REMOVED:

		handle_lnh_event(msg->itc_lnh_removed.lnhpath,
		                 "XHP_LinkDown");
		break;
	case ULH_LNHMSG_NOTIFY:
		TPT_TRACE(3, STR("state:%d", msg->ulind.state));
		break;
	default:
		ret = false;
	}
	if (ret) itc_free(&msg);
	return ret;
}

/**
 * Start reading LH messages through ITC and process them.
 */
static void read_messages(conn_server_handle_t handle)
{
	union itc_msg *msg;
	struct conn_client_info client_info;
	itc_mbox_id_t mailbox;

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		mailbox = itc_sender(msg);
		if (handle_events(msg)) continue;

		/* Handle CONN_ESTABLISH msgs and msgs from unknown clients.*/
		if (!conn_check_client(handle, &msg, &client_info))
			continue;

		switch (msg->msgno) {

		case RHAMI_LH_STARTLINK_REQ: {
			int status;
			struct lh_startlink_req *req = &msg->startlink_req;
			TPT_TRACE(3, STR("client_ref:%d, port:%d, channel_id:%d, buff_idx:%d",
			                 client_info.client_ref, req->port, req->channel_id, req->buff_idx));

			char *s = NULL;
			status = create_connection(req->port,
			                           req->channel_id, &s);
			if (!status) {
				status = create_link(handle, &client_info, req->port,
				                     req->channel_id, req->buff_idx);
				if (status)
					destroy_connection(req->port,
						           req->channel_id);
			}
			if (s)
				free(s);

			rhami_lh_send_startlink_rsp(mailbox,
			                            client_info.client_ref,
			                            req->port,
			                            req->channel_id,
			                            req->procedure_ref,
			                            status ? LH_RESULT_FAILURE : LH_RESULT_OK);
			break;
		}

		case RHAMI_LH_ECPSTATS_REQ:
			handle_ecpstats(msg, client_info.client_ref);
			break;
		default:
			TPT_INFO(STR("ABN: unexpected message 0x%08x",
			             msg->msgno));
			break;
		}

		itc_free(&msg);
	}

}

static void print_usage()
{

	printf("Usage: rhd-lh [options] <device>\n\n"
	       "Where <device> is less than %d\n\n"
	       "Options:\n"
	       "    -h        Display usage information (this message).\n"
	       "    -d        Daemonize the program.\n\n", lh_max_devices);

}

/**
 * Main function to start the LH daemon.
 */
int main(int argc, char *argv[])
{
	static char short_options[] = "hd";
	int daemonize = 0;
	char *endptr;
	int c;
	void *handle = NULL;
	


	while ((c = getopt(argc, argv, short_options)) != -1) {
		switch (c) {
		case 'h':
			print_usage();
			exit(0);
			break;
		case 'd':
			daemonize = 1;
			break;
		default:
			print_usage();
			exit(-EINVAL);
		}
	}

	if (argc - optind != 2) {
		printf("No device supplied to rhd-lh\n");
		print_usage();
		exit(-EINVAL);
	}

	if  (strcmp("TRXM", getenv("SYS_BOARD_TYPE")) == 0)
	{
		printf("TRXM board type\n");
		TPT_INFO("TRXM board type\n");
		lh_max_devices = 1;
		ecp_max_nof_buffs = 2;
		mailbox_name_length = sizeof(RHAMI_LH_MAILBOX) + sizeof(XSTRINGIFY(lh_max_devices)) + 1;
	}
	else if (strcmp("BP", getenv("SYS_BOARD_TYPE")) == 0)
	{
		printf("BP board type\n");
		TPT_INFO("BP board type\n");
		lh_max_devices = 7;
		ecp_max_nof_buffs = 2;
	}
	else
	{
		printf("default board type\n");
		TPT_INFO("default board type\n");
		lh_max_devices = LH_MAX_DEVICES;
		ecp_max_nof_buffs = ECP_MAX_NOF_BUFFS;
	}

	mailbox_name_length = sizeof(RHAMI_LH_MAILBOX) + sizeof(XSTRINGIFY(lh_max_devices)) + 1;
	daemon_name_length = sizeof(DAEMON_NAME) + sizeof(XSTRINGIFY(lh_max_devices)) + 1;
	ecp_dev_name_length = sizeof(LH_UIO_DEV) + sizeof(XSTRINGIFY(lh_max_devices)) + 1;

	lh_device = strtol(argv[argc - 2], &endptr, 10);
	if (lh_device >= lh_max_devices) {
		printf("Invalid lh device: %s\n", argv[argc - 2]);
		print_usage();
		exit(-EINVAL);
	}

	primary = strtol(argv[argc - 1], &endptr, 10);
        if (*endptr != '\0' || ((primary != 1) && (primary != 0))) {
                printf("Invalid primary value: %s\n", argv[argc - 1]);
                print_usage();
                exit(-EINVAL);
        }

	snprintf(daemon_name, daemon_name_length, "%s-%d",
	         DAEMON_NAME, lh_device);

	if (rhd_try_lock(daemon_name)) {
		printf("failed to obtain lock: %s\n", daemon_name);
		return -EFAULT;
	}

	if (!daemonize || !daemon(0, 1)) {
		TPT_INFO("Starting daemon");

		handle = conn_server_init();
		if (!lh_init(lh_device) && (handle != NULL)) {
			TPT_INFO("Daemon running");
			/* Start processing ITC messages */
			read_messages(handle);
			lh_shutdown();
		} else {
			TPT_ERROR("Failed to initialize daemon");
			return -1;
		}
	} else {
		TPT_ERROR("Failed to start daemon");
		return -1;
	}
	return 0;
}

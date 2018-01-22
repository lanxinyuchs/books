/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <ctype.h>
#include <pthread.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <itc.h>
#include <itc_system.h>
#include <ulh_cm.h>
#include <ulh_lnh.h>
#include <ulh_transport.h>
#include <ulh_lnh_msg.h>
#include <rhai-ecb.h>
#include "ulh_ecb.h"
#include "a4ci.sig"
#include "atfi.sig"
#include "ecb-link-api.h"
#include "ecb-link-internal.h"

#define TRACEPOINT_DEFINE
#include <com_ericsson_system_start.h>
#include "ecb_lttng.h"
#include "ecb_trace.h"


#define ECB_UCM_NAME					"ecbcm"
#define ECB_ULNH_NAME					"ecblh"
#define ECB_MTU							78
#define ECB_HLEN                		5
#define CHAR_ESC                		0x7d

#define DISCONNECTED 					0
#define CONNECTING 						1
#define CONNECTED 						2

#define ATFID_HDLC_ADDR_MIN            	1
#define ATFID_HDLC_ADDR_MAX          	254


struct atf_client {
	itc_mbox_id_t		pid;
	void*				cookie;
	uint32_t			conn_map;
};

struct atf_conn {
	uint16_t			state;
	uint16_t			type;
	uint16_t			phy_addr;
	uint16_t			log_addr;
	uint32_t			tran_id;
	uint32_t			link_id;
	char				*name;
	uint32_t			client_map;
	uint32_t			client_info[ATFI_MAX_NUMBER_OF_CLIENTS];
};


struct atf_data {
	itc_mbox_id_t		mpatfi_mbox;
	itc_mbox_id_t		server_mbox;
	uint8_t				conn_lookup[256];
	struct atf_client	clients[ATFI_MAX_NUMBER_OF_CLIENTS];
	struct atf_conn		conns[ATFI_MAX_NUMBER_OF_CONNECTIONS];
};


struct ecb_data {
	itc_mbox_id_t		server_mbox;
	struct ecb_link		*conn_list;
	struct ecb_link		*conn_array[256];
};


struct ecb_tran {
	void				*ecb_handle;
	uint32_t			alloc_cid;
};


union itc_msg {
	uint32_t         		 			msgno;

	struct ecb_link_create				ctreq;
	struct ecb_link_create				ctrsp;
	struct ecb_link_destroy				dsreq;
	struct ecb_link_destroy				dsrsp;
	struct ecb_link_event				lkind;

	struct ulh_lnhmsg_createcm_req    	cmreq;
	struct ulh_lnhmsg_createcm_rsp    	cmrsp;
	struct ulh_lnhmsg_createlink_req  	lcreq;
	struct ulh_lnhmsg_createlink_rsp  	lcrsp;
	struct ulh_lnhmsg_destroylink_req 	ldreq;
	struct ulh_lnhmsg_destroylink_rsp 	ldrsp;
	struct ulh_lnhmsg_notify			ulind;

	struct atfiConnEstablishReqS		connEstablishReq;
	struct atfiConnEstablishCfmS		connEstablishCfm;
	struct atfiConnEstablishRejS		connEstablishRej;

	struct atfiAddConnMapReqS			addConnMapReq;
	struct atfiAddConnMapCfmS			addConnMapCfm;
	struct atfiAddConnMapRejS			addConnMapRej;

	struct atfiAddAu3MapReqS			atfiAddAu3MapReq;
	struct atfiAddAu3MapCfmS			atfiAddAu3MapCfm;
	struct atfiAddAu3MapRejS			atfiAddAu3MapRej;

	struct atfiRemoveConnMapReqS		removeConnMapReq;
	struct atfiRemoveConnMapCfmS		removeConnMapCfm;
	struct atfiRemoveConnMapRejS		removeConnMapRej;

	struct atfiConnect2ReqS				connect2Req;
	struct atfiConnect2CfmS				connect2Cfm;
	struct atfiConnect2RejS				connect2Rej;
	struct atfiConnect2IndS				connect2Ind;

	struct atfiDisconnectReqS			disconnectReq;
	struct atfiDisconnectCfmS			disconnectCfm;
	struct atfiDisconnectRejS			disconnectRej;
	struct atfiDisconnectIndS			disconnectInd;

	struct atfiResetReqS				atfiResetReq;
	struct atfiResetCfmS				atfiResetCfm;
	struct atfiResetRejS				atfiResetRej;

	struct atfiAuditReqS				auditReq;
	struct atfiAuditCfmS				auditCfm;
	struct atfiAuditRejS				auditRej;

	struct atfiGetAu3PortReqS			atfiGetAu3PortReq;
	struct atfiGetAu3PortCfmS			atfiGetAu3PortCfm;
	struct atfiGetAu3PortRejS			atfiGetAu3PortRej;

	struct atfiGetAisgUniqueIdReqS		atfiGetAisgUniqueIdReq;
	struct atfiGetAisgUniqueIdCfmS		atfiGetAisgUniqueIdCfm;
	struct atfiGetAisgUniqueIdRejS		atfiGetAisgUniqueIdRej;

	struct atfiAddAisgMap2ReqS			atfiAddAisgMap2Req;
	struct atfiAddAisgMapCfmS			atfiAddAisgMapCfm;
	struct atfiAddAisgMapRejS			atfiAddAisgMapRej;

	struct atfiAddCpriMap2ReqS			atfiAddCpriMap2Req;
	struct atfiAddCpriMapCfmS			atfiAddCpriMapCfm;
	struct atfiAddCpriMapRejS			atfiAddCpriMapRej;

	struct atfiGetPhyEndpointReqS		atfiGetPhyEndpointReq;
	struct atfiGetPhyEndpointCfmS		atfiGetPhyEndpointCfm;
	struct atfiGetPhyEndpointRejS		atfiGetPhyEndpointRej;

	struct atfiSetupIdcpA3ReqS			atfiSetupIdcpA3Req;
	struct atfiSetupIdcpA3CfmS			atfiSetupIdcpA3Cfm;
	struct atfiSetupIdcpA3RejS			atfiSetupIdcpA3Rej;

	struct atfiSetupIdcpB3ReqS			atfiSetupIdcpB3Req;
	struct atfiSetupIdcpB3CfmS			atfiSetupIdcpB3Cfm;
	struct atfiSetupIdcpB3RejS			atfiSetupIdcpB3Rej;

	struct atfiReleaseIdcpAReqS			atfiReleaseIdcpAReq;
	struct atfiReleaseIdcpACfmS			atfiReleaseIdcpACfm;
	struct atfiReleaseIdcpARejS			atfiReleaseIdcpARej;

	struct atfiReleaseIdcpBReqS			atfiReleaseIdcpBReq;
	struct atfiReleaseIdcpBCfmS			atfiReleaseIdcpBCfm;
	struct atfiReleaseIdcpBRejS			atfiReleaseIdcpBRej;

	struct atfiAisgDeviceScanReqS		atfiAisgDeviceScanReq;
	struct atfiAisgDeviceScanCfmS		atfiAisgDeviceScanCfm;
	struct atfiAisgDeviceScanRejS		atfiAisgDeviceScanRej;
};


static struct atf_data atfi;
static struct ecb_tran trans = {NULL, ULH_TRANS_NOCONN};
pthread_mutex_t ebus_lock = PTHREAD_MUTEX_INITIALIZER;


static void
handle_reset(const struct atfiResetReqS *req,
            const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiResetCfmS), ATFI_RESET_REJ);
	sig->atfiResetRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiResetRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_addAu3Map(const struct atfiAddAu3MapReqS *req,
                const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiAddAu3MapRejS), ATFI_ADD_AU3_MAP_REJ);
	sig->atfiAddAu3MapRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiAddAu3MapRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_getAu3Port(const struct atfiGetAu3PortReqS *req,
                 const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiGetAu3PortRejS), ATFI_GET_AU3_PORT_REJ);
	sig->atfiGetAu3PortRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiGetAu3PortRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_getPhyEndpoint(const struct atfiGetPhyEndpointReqS *req,
                     const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiGetPhyEndpointRejS), ATFI_GET_PHY_ENDPOINT_REJ);
	sig->atfiGetPhyEndpointRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiGetPhyEndpointRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_setupIdcpA(const struct atfiSetupIdcpA3ReqS *req,
                 const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiSetupIdcpA3RejS), ATFI_SETUP_IDCP_A3_REJ);
	sig->atfiSetupIdcpA3Rej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiSetupIdcpA3Rej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_setupIdcpB(const struct atfiSetupIdcpB3ReqS *req,
                 const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiSetupIdcpB3RejS), ATFI_SETUP_IDCP_B3_REJ);
	sig->atfiSetupIdcpB3Rej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiSetupIdcpB3Rej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_releaseIdcpA(const struct atfiReleaseIdcpAReqS *req,
                   const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiReleaseIdcpARejS), ATFI_RELEASE_IDCP_A_REJ);
	sig->atfiReleaseIdcpARej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiReleaseIdcpARej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_releaseIdcpB(const struct atfiReleaseIdcpBReqS *req,
                   const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiReleaseIdcpBRejS), ATFI_RELEASE_IDCP_B_REJ);
	sig->atfiReleaseIdcpBRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiReleaseIdcpBRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_addAisgMap2(const struct atfiAddAisgMap2ReqS *req,
                  const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiAddAisgMapRejS), ATFI_ADD_AISG_MAP_REJ);
	sig->atfiAddAisgMapRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiAddAisgMapRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_addCpriMap2(const struct atfiAddCpriMap2ReqS *req,
                  const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiAddCpriMapRejS), ATFI_ADD_CPRI_MAP_REJ);
	sig->atfiAddCpriMapRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiAddCpriMapRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static void
handle_getAisgUniqueIdReq(const struct atfiGetAisgUniqueIdReqS *req,
                         const itc_mbox_id_t pid)
{
	union itc_msg *sig;

	sig = itc_alloc(sizeof(struct atfiGetAisgUniqueIdRejS), ATFI_GET_AISG_UNIQUE_ID_REJ);
	sig->atfiGetAisgUniqueIdRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	sig->atfiGetAisgUniqueIdRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
	itc_send(&sig, pid, itc_receiver((union itc_msg*)req));
}

static int
client_check(itc_mbox_id_t pid)
{
	int i;

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CLIENTS; i++)
		if (atfi.clients[i].pid  == pid)
			return ++i;

	ECB_ERROR("Received a request from a non-client mbox 0x%x", pid);
	return 0;
}


static int
do_connect(struct ecb_link_config *cfg, struct atf_conn *conn)
{
	uint32_t select[2] = {1, ULH_LNHMSG_CREATECM_RSP};
	struct ulh_trans_addr ecb_addr;
	struct ulh_cm_ecb_config *cfp;
	union itc_msg *msg;
	uint32_t cmid, tcid;
	int ret;

	memset(&ecb_addr, 0, sizeof(ecb_addr));
	ecb_addr.data[0] = cfg->address;
	tcid = ulh_trans_create_conn("ecb0", &ecb_addr, &ecb_addr);

	if (tcid == ULH_TRANS_NOCONN) {
		syslog(LOG_ERR, "ulh_trans_create_conn() failed");
		return -EFAULT;
	}

	if (trans.alloc_cid == ULH_TRANS_NOCONN)
		trans.alloc_cid = tcid;

	msg = itc_alloc(sizeof(msg->cmreq)+sizeof(*cfp), ULH_LNHMSG_CREATECM_REQ);
	cfp = (struct ulh_cm_ecb_config *)&msg->cmreq.config;
	cfp->cmn.cfg_size 		= sizeof(*cfp);
	cfp->cmn.cfg_version 	= 0;
	cfp->cmn.cid 			= tcid;
	cfp->address 			= cfg->address;
	cfp->station 			= cfg->station;
	msg->cmreq.seq			= 0;
	strcpy(msg->cmreq.cm_name, ECB_UCM_NAME);
    sprintf(msg->cmreq.cm_instance, "ecb_conn%02X", (char)cfg->address);

	itc_send(&msg, atfi.server_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATECM_RSP;
	msg = itc_receive(select, ITC_NO_TMO, atfi.server_mbox);
	ret = msg->cmrsp.result;
	cmid = msg->cmrsp.cmid;
	itc_free(&msg);

	if (ret)
		return ret;

	msg = itc_alloc(sizeof(msg->lcreq), ULH_LNHMSG_CREATELINK_REQ);
	msg->lcreq.prio = 0;
	msg->lcreq.cmid = cmid;
	strcpy(msg->lcreq.name, cfg->name);

	itc_send(&msg, atfi.server_mbox, ITC_MY_MBOX);
	select[1] = ULH_LNHMSG_CREATELINK_RSP;
	msg = itc_receive(select, ITC_NO_TMO, atfi.server_mbox);

	conn->link_id = msg->lcrsp.lid;
	conn->tran_id = tcid;

	ret = msg->lcrsp.result;
	itc_free(&msg);

	if (ret == 0) {
		conn->state = CONNECTING;
		atfi.conn_lookup[conn->phy_addr] = (uint8_t)(conn - atfi.conns);
	}
	return ret;
}

static int
do_disconnect(struct atf_conn *conn)
{
	int ret = -EIO;
	uint32_t select[2] = {1, ULH_LNHMSG_DESTROYLINK_RSP};
	union itc_msg *msg = itc_alloc(	sizeof(msg->ldreq), 
									ULH_LNHMSG_DESTROYLINK_REQ
								  );

	conn->state = DISCONNECTED;
	
	msg->ldreq.seq = 0;
	msg->ldreq.lid = conn->link_id;
	itc_send(&msg, atfi.server_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 5000, atfi.server_mbox);

	if (msg) {
		ret = (int)msg->ldrsp.result;
		itc_free(&msg);
	}
	return ret;
}


static void 
handle_new_client(struct atfiConnEstablishReqS *req, itc_mbox_id_t pid)
{
	union itc_msg *s;
	uint16_t rev = ntohs(req->protocolRev);
	int i,j = ATFI_MAX_NUMBER_OF_CLIENTS;
	itc_mbox_id_t from = itc_receiver((union itc_msg*)req);


	ECB_DEBUG("Received ESTABLISH_REQ (rev %d) for mbox 0x%x", rev, pid);

	s = itc_alloc(sizeof(s->connEstablishRej), ATFI_CONN_ESTABLISH_REJ);
	s->connEstablishRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	s->connEstablishRej.highestSupportedProtocolRev = htons(ATFI_PROTOCOL_REV);

	itc_free((union itc_msg**)&req);

	if (rev != ATFI_PROTOCOL_REV) {
		s->connEstablishRej.errorCode = htons(ATFI_UNEXPECTED_PARAMETER_VALUE);
		itc_send(&s, pid, from);
		return;
	}

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CLIENTS; i++) {
		if (atfi.clients[i].pid == pid) {
			s->msgno = ATFI_CONN_ESTABLISH_CFM;
			itc_send(&s, pid, ITC_MY_MBOX);
			return;
		} else if (atfi.clients[i].pid == 0)
			j = i;
	}

	if (j == ATFI_MAX_NUMBER_OF_CLIENTS) {
		s->connEstablishRej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
		itc_send(&s, pid, from);
		return;
	}

	atfi.clients[j].pid = pid;
	atfi.clients[j].conn_map = 0;
	atfi.clients[j].cookie = NULL;

	(void)itc_monitor(pid, NULL);

	s->msgno = ATFI_CONN_ESTABLISH_CFM;
	itc_send(&s, pid, from);
	return;
}

static void
handle_lost_client(union itc_msg *msg, itc_mbox_id_t pid)
{
	struct atf_conn *co;
	uint32_t map, msk;
	int i, j = 0;

	itc_free(&msg);

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CLIENTS; i++)
		if (atfi.clients[i].pid == pid) {
			atfi.clients[i].pid = 0;
			break;
		}

	if (i == ATFI_MAX_NUMBER_OF_CLIENTS) {
		ECB_TRACE("Lost client 0x%x not found", pid);
		return;
	}

	msk = (0x80000000 >> i);
	for (map = atfi.clients[i].conn_map; map; map <<= 1, j++) {
		if ((map & 0x80000000) == 0)
			continue;

		co = &atfi.conns[j];
		co->client_map &= ~msk;

		if (co->client_map == 0) {
			do_disconnect(co);

			if (co->name) {
				free(co->name);
				co->phy_addr = 0;
			}
		}
	}
}

static void
handle_add_map(struct atfiAddConnMapReqS *req, itc_mbox_id_t pid)
{
	union itc_msg *s;
	uint16_t log = ntohs(req->logicalAddress);
	uint16_t phy = ntohs(req->physicalAddress);
	int i, j = ATFI_MAX_NUMBER_OF_CONNECTIONS;
	itc_mbox_id_t from = itc_receiver((union itc_msg*)req);

	ECB_DEBUG("Received ADD_CONN_MAP_REQ (pa:%u la:%u)", phy, log);

	if (!client_check(pid)) {
		itc_free((union itc_msg**)&req);
		return;
	}

	s = itc_alloc(sizeof(s->addConnMapRej), ATFI_ADD_CONN_MAP_REJ);
	s->addConnMapRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	s->addConnMapRej.errorCode = htons(ATFI_UNEXPECTED_PARAMETER_VALUE);
	itc_free((union itc_msg**)&req);

	if ((phy < ATFID_HDLC_ADDR_MIN) || (phy > ATFID_HDLC_ADDR_MAX)) {
		itc_send(&s, pid, from);
		return;
	}

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (atfi.conns[i].phy_addr) {
			if (atfi.conns[i].log_addr == log) {
				if (atfi.conns[i].phy_addr == phy)
					s->msgno = ATFI_ADD_CONN_MAP_CFM;

				itc_send(&s, pid, from);
				return;
			}
		} else
			j = i;
	}

	if (j == ATFI_MAX_NUMBER_OF_CONNECTIONS) {
		s->addConnMapRej.errorCode = htons(ATFI_OTHER_ERROR);
		itc_send(&s, pid, from);
		return;
	}

	(void)memset(&atfi.conns[j], 0, sizeof(struct atf_conn));
	atfi.conns[j].phy_addr = phy;
	atfi.conns[j].log_addr = log;

	s->msgno = ATFI_ADD_CONN_MAP_CFM;
	itc_send(&s, pid, from);
}


static void
handle_remove_map(struct atfiRemoveConnMapReqS* req, itc_mbox_id_t pid)
{
	union itc_msg *s;
	uint16_t log = ntohs(req->logicalAddress);
	int i, j = 0;
	itc_mbox_id_t from = itc_receiver((union itc_msg*)req);

	if (!client_check(pid)) {
		itc_free((union itc_msg**)&req);
		return;
	}

	s = itc_alloc(sizeof(s->removeConnMapRej), ATFI_REMOVE_CONN_MAP_REJ);
	s->removeConnMapRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	s->removeConnMapRej.errorCode = htons(ATFI_UNEXPECTED_PARAMETER_VALUE);
	itc_free((union itc_msg**)&req);


	for (i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (atfi.conns[i].phy_addr && (atfi.conns[i].log_addr == log)) {
			if (atfi.conns[i].state == DISCONNECTED) {
				uint32_t map, msk = ~(0x80000000 >> i);

				for (map = atfi.conns[i].client_map; map; j++, map <<= 1)
					if (map & 0x80000000)
						atfi.clients[j].conn_map &= msk;

				atfi.conns[i].phy_addr = 0;
				s->msgno = ATFI_REMOVE_CONN_MAP_CFM;

			} else {
				s->removeConnMapRej.errorCode = 
							htons(ATFI_SIGNAL_IN_WRONG_STATE);
			}
			break;
		}
	}
	itc_send(&s, pid, from);
}


static void
handle_connect(struct atfiConnect2ReqS *req, itc_mbox_id_t pid)
{
	union itc_msg	*s;
	uint16_t log, type;
	uint32_t msk;
	int i, j, cli;
	itc_mbox_id_t from = itc_receiver((union itc_msg*)req);

	log = ntohs(req->logicalAddress);
	type = ntohs(req->typeOfUnit);

	ECB_DEBUG("Received ATFI_CONNECT2_REQ (la:%u type:%u)", log, type);

	if ((cli = client_check(pid)) == 0) {
		itc_free((union itc_msg**)&req);
		return;
	}

	cli--;
	s = itc_alloc(sizeof(s->connect2Rej), ATFI_CONNECT2_REJ);
	s->connect2Rej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	s->connect2Rej.errorCode = htons(ATFI_UNEXPECTED_PARAMETER_VALUE);
	itc_free((union itc_msg**)&req);

	if ((type != ATFI_DU_SECONDARY) && (type != ATFI_DU_PRIMARY)) {
		s->connect2Rej.errorCode = htons(ATFI_UNSUPPORTED_CAPABILITY);
		itc_send(&s, pid, from);
		return;
	}

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (atfi.conns[i].phy_addr && (atfi.conns[i].log_addr == log)) {
			if ((atfi.conns[i].state != DISCONNECTED) &&
							(atfi.conns[i].type != type)) {
				itc_send(&s, pid, from);
				return;
			}
			break;
		}
	}

	if (i == ATFI_MAX_NUMBER_OF_CONNECTIONS) {
		itc_send(&s, pid, from);
		return;
	}

	for (j = 0; j < ATFI_MAX_NUMBER_OF_CONNECTIONS; j++) {
		if ((atfi.conns[j].phy_addr == atfi.conns[i].phy_addr) &&
			(atfi.conns[j].state != DISCONNECTED) &&
			(atfi.conns[j].log_addr != atfi.conns[i].log_addr)) {

			s->connect2Rej.errorCode = htons(ATFI_OTHER_ERROR);
			itc_send(&s, pid, from);
			return;
		}
	}

	atfi.clients[cli].conn_map |= (0x80000000 >> i);
	msk = (0x80000000 >> cli);

	if ((atfi.conns[i].client_map & msk) == 0) {
		atfi.conns[i].client_map |= msk;
		atfi.conns[i].client_info[cli] = s->connect2Cfm.addrInfo.linkHandle;
	}

	atfi.conns[i].type = type;

	s->msgno = ATFI_CONNECT2_CFM;
	s->connect2Cfm.state = htons(ATFI_DISCONNECTED);

	switch (atfi.conns[i].state) {
	case DISCONNECTED: {
		struct ecb_link_config cfg;

		cfg.address = atfi.conns[i].phy_addr;
		cfg.station = ECB_STATION_PRIMARY;

		if (type == ATFI_DU_PRIMARY)
			cfg.station = ECB_STATION_SECONDARY;
			
		snprintf(cfg.name, sizeof(cfg.name), "port_0_dev_%d/",
					atfi.conns[i].log_addr);
		
		if (do_connect(&cfg, &atfi.conns[i])) {
			s->msgno = ATFI_CONNECT2_REJ;
			s->connect2Rej.errorCode = htons(ATFI_OTHER_ERROR);
		}
		break;
	}
	case CONNECTING:
		s->connect2Cfm.state = htons(ATFI_DISCONNECTED);
		break;
	case CONNECTED:
		s->connect2Cfm.state = htons(ATFI_CONNECTED);
		break;
	default:
		s->msgno = ATFI_CONNECT2_REJ;
		s->connect2Rej.errorCode = htons(ATFI_OTHER_ERROR);
		break;
	}
	itc_send(&s, pid, from);
}

static void
handle_disconnect(struct atfiDisconnectReqS *req, itc_mbox_id_t pid)
{
	union itc_msg *s;
	uint16_t log;
	int i, cli;
	itc_mbox_id_t from = itc_receiver((union itc_msg*)req);

	log = ntohs(req->logicalAddress);
	ECB_DEBUG("Received ATFI_DISCONNECT_REQ (la:%u)", log);

	if ((cli = client_check(pid)) == 0) {
		itc_free((union itc_msg**)&req);
		return;
	}

	cli--;
	s = itc_alloc(sizeof(s->disconnectRej), ATFI_DISCONNECT_REJ);
	s->disconnectRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	s->disconnectRej.errorCode = htons(ATFI_UNEXPECTED_PARAMETER_VALUE);
	itc_free((union itc_msg**)&req);

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (atfi.conns[i].phy_addr && (atfi.conns[i].log_addr == log)) {
			uint32_t msk = 0x80000000 >> cli;

			if ((atfi.conns[i].client_map & msk) == 0) {
				s->disconnectRej.errorCode = htons(ATFI_OTHER_ERROR);
				break;
			}

			atfi.clients[cli].conn_map &= ~(0x80000000 >> i);
			atfi.conns[i].client_map &= ~msk;

			if (atfi.conns[i].client_map == 0) {
				if (atfi.conns[i].state != DISCONNECTED)
					(void)do_disconnect(&atfi.conns[i]);
			}
			s->msgno = ATFI_DISCONNECT_CFM;
			break;
		}
	}
	itc_send(&s, pid, from);
}

static void
handle_audit(struct atfiAuditReqS *req, itc_mbox_id_t pid)
{
	union itc_msg *s;
	uint16_t log = ntohs(req->logicalAddress);
	int cli, i;
	itc_mbox_id_t from = itc_receiver((union itc_msg*)req);

	if ((cli = client_check(pid)) == 0) {
		itc_free((union itc_msg**)&req);
		return;
	}

	cli--;
	s = itc_alloc(sizeof(s->auditRej), ATFI_AUDIT_REJ);
	s->auditRej.addrInfo.linkHandle = req->addrInfo.linkHandle;
	s->auditRej.errorCode = htons(ATFI_UNEXPECTED_PARAMETER_VALUE);
	itc_free((union itc_msg**)&req);

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (atfi.conns[i].phy_addr && (atfi.conns[i].log_addr == log)) {
			if (atfi.conns[i].state == CONNECTED) { 
				uint32_t msk = 0x80000000 >> cli;

				if ((atfi.conns[i].client_map & msk) == 0) {
					atfi.conns[i].client_map |= msk;
					atfi.conns[i].client_info[cli] 
								= s->auditCfm.addrInfo.linkHandle;
				}
			} else
				s->auditCfm.state = htons(ATFI_DISCONNECTED);

			s->msgno = ATFI_AUDIT_CFM;
			break;
		}
	}
	itc_send(&s, pid, from);
}

static void handle_create(union itc_msg *msg, itc_mbox_id_t pid)
{
	struct ecb_link_config cfg;
	int i, cli = ATFI_MAX_NUMBER_OF_CLIENTS;
	int conn = ATFI_MAX_NUMBER_OF_CONNECTIONS;
	int ret = -EBUSY;


	ECB_DEBUG("Create name: %s  address: %d  station:%d",
				msg->ctreq.name, msg->ctreq.address, msg->ctreq.station);


	for (i = 0; i < ATFI_MAX_NUMBER_OF_CLIENTS; i++) {
		if (atfi.clients[i].pid == pid) {
			cli = i;
			break;
		} else if (atfi.clients[i].pid == 0)
			cli = i;
	}

	if (cli == ATFI_MAX_NUMBER_OF_CLIENTS)
		goto done;

	if (atfi.clients[cli].pid == 0) {
		atfi.clients[cli].cookie = NULL;
		atfi.clients[cli].conn_map = 0;
		(void)itc_monitor(pid, NULL);
	}

	cfg.address	= msg->ctreq.address;
	cfg.station	= msg->ctreq.station;
	strcpy(cfg.name,msg->ctreq.name);

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (atfi.conns[i].phy_addr) {
			if (atfi.conns[i].phy_addr == cfg.address) {
				ret = -EEXIST;
				goto done;
			}

			if (atfi.conns[i].name) {
				if (!strcmp(atfi.conns[i].name, cfg.name)) {
					ret = -EALREADY;
					goto done;
				}
			}
		} else
			conn = i;
	}

	if (conn == ATFI_MAX_NUMBER_OF_CONNECTIONS)
		goto done;

	(void)memset(&atfi.conns[conn], 0, sizeof(struct atf_conn));
	atfi.conns[conn].phy_addr = (uint16_t)cfg.address;
	atfi.conns[conn].log_addr = (uint16_t)-1;


	ret = do_connect(&cfg, &atfi.conns[conn]);
	if (ret == 0) {
		atfi.clients[cli].pid = pid;
		atfi.clients[cli].cookie = msg->ctreq.owner;
		atfi.clients[cli].conn_map |= (0x80000000 >> conn);

		atfi.conns[conn].type = (cfg.station == ECB_STATION_PRIMARY)
							  ? ATFI_DU_SECONDARY : ATFI_DU_PRIMARY;
		atfi.conns[conn].name = strdup(cfg.name);
		atfi.conns[conn].client_map = (0x80000000 >> cli);
		msg->ctrsp.linkid = atfi.conns[conn].link_id;
	}

 done:
	msg->msgno = ECB_LINK_CREATE_RSP;
	msg->ctrsp.result = ret;
	itc_send(&msg, pid, ITC_MY_MBOX);
}

static void handle_destroy(union itc_msg *msg, itc_mbox_id_t pid)
{
	int i, j;
	uint32_t map;
	
	for (i = 0; i < ATFI_MAX_NUMBER_OF_CLIENTS; i++)
		if (atfi.clients[i].pid == pid)
			break;

	if (i == ATFI_MAX_NUMBER_OF_CLIENTS)
		goto done;

	for (j = 0, map = atfi.clients[i].conn_map; map; map <<= 1, j++) {
		if (map & 0x80000000) {
			if (msg->dsreq.owner || 
				(atfi.conns[j].link_id == msg->dsreq.linkid)) {

				do_disconnect(&atfi.conns[j]);
				atfi.conns[j].phy_addr = 0;
				free(atfi.conns[j].name);
				atfi.clients[i].conn_map &= ~(0x80000000 >> j);
			}
		}
	}

	if (atfi.clients[i].conn_map == 0)
		atfi.clients[i].pid = 0;

 done:
	msg->msgno = ECB_LINK_DESTROY_RSP;
	msg->dsrsp.result = 0;
	itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
}

static void handle_notify(union itc_msg *msg)
{	
	union itc_msg *ind;
	uint32_t map;
	int i, j = 0;
	itc_mbox_id_t from = ITC_MY_MBOX;

	for (i = 0; i < ATFI_MAX_NUMBER_OF_CONNECTIONS; i++) {
		if (atfi.conns[i].phy_addr && 
			(atfi.conns[i].link_id == msg->ulind.lid)) {

			if (msg->ulind.state == ULH_LINK_UP)
				atfi.conns[i].state = CONNECTED;
			else if (atfi.conns[i].state == CONNECTED)
				atfi.conns[i].state = CONNECTING;

			for (map = atfi.conns[i].client_map; map; map <<= 1, j++) {
		
				if ((map & 0x80000000) == 0)
					continue;

				if (atfi.conns[i].name) {
					ind = itc_alloc(sizeof(ind->lkind), ECB_LINK_EVENT_MSG);
					ind->lkind.link = atfi.conns[i].link_id;
					ind->lkind.event = ECB_LINK_STATE_UP;

					if (msg->ulind.state == ULH_LINK_DOWN)
						ind->lkind.event = ECB_LINK_STATE_DOWN;

				} else {
					from = atfi.mpatfi_mbox;
					ind = itc_alloc(sizeof(struct atfiDisconnectIndS),
										ATFI_DISCONNECT_IND);
					ind->disconnectInd.addrInfo.linkHandle = 
									atfi.conns[i].client_info[j];
					ind->disconnectInd.logicalAddress =
									htons(atfi.conns[i].log_addr);

					if (msg->ulind.state == ULH_LINK_UP)
						ind->msgno = ATFI_CONNECT2_IND;

				}
				itc_send(&ind, atfi.clients[j].pid, from);
			}
		}
	}
	itc_free(&msg);
}

static int ecb_create_conn(void *tref, uint32_t tcid, 
       struct ulh_trans_addr *src, struct ulh_trans_addr *dst, void **cref)
{
	(void)tcid;
	(void)src;
	(void)dst;

	*cref = tref;
	return 0;
}

static int ecb_destroy_conn(void *tref, void *cref)
{
	(void)tref;
	(void)cref;

	return 0;
}

static int ecb_transmit(void *tref, void *cref, struct ulh_tbuff *tbuff)
{
	struct ecb_tran *tran = tref;
	int len;
	(void)cref;

	len = rhai_ecb_write(tran->ecb_handle, ulh_tbuff_get(tbuff),
										(int)ulh_tbuff_len(tbuff));

	INC_STAT(nrOfTransmittedOctets, len);
	INC_STAT(nrOfTransmittedHDLCFrames, 1);

	ulh_tbuff_free(tbuff);
	if (len > 0)
		return 0;

	syslog(LOG_ERR, "[ECBLNH] rhai_ecb_write failure, %d", len);
	return len;
}

static int ecb_getmtu(void *param, void *conn_ref)
{
	(void)param;
	(void)conn_ref;
	return ECB_MTU;
}

static void *ecb_reader(void *context)
{
	struct ecb_tran *tran = context;
	void *handle = tran->ecb_handle;
	struct ulh_tbuff tbuff;
	itc_mbox_id_t me;

	me = itc_create_mailbox("ecb_reader", 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "[ECBLNH] create mbox failure...sleep forever");
		for (;;)sleep(600);
	}

	for (;;) {
		int ret = ulh_tbuff_alloc(trans.alloc_cid, 256, &tbuff);
		int len;

		if (ret) {
			if (ret != -EINVAL)
				syslog(LOG_ERR, "ulh_tbuff_alloc: -ENOMEM");

			sleep(1);
			continue;
		}

		len = rhai_ecb_read(handle, ulh_tbuff_get(&tbuff), 256);
		if (len >= ECB_HLEN) {
			struct atf_conn *p;
			uint8_t addr = tbuff.data[0];

			if (addr == CHAR_ESC)
				addr = tbuff.data[1] ^ 0x20;

			p = &atfi.conns[atfi.conn_lookup[addr]];
			if (p->state != DISCONNECTED) {
				tbuff.size = len;
				ulh_trans_deliver(p->tran_id, &tbuff);
			} else
				ulh_tbuff_free(&tbuff);
		} else
			ulh_tbuff_free(&tbuff);
	}
    return context;
}

static struct ulh_trans_ops ecb_ops = {
	.create_conn 	= ecb_create_conn,
	.destroy_conn 	= ecb_destroy_conn,
	.transmit 		= ecb_transmit,
	.getmtu 		= ecb_getmtu
};


static int ecb_init(void)
{
	pthread_t rtid;
	void *handle;
	int ret = -EAGAIN;

	for (; ret; sleep(2)) {
		ret = rhai_ecb_init(&handle);

		if (ret == 0)
			break;

		if (ret != -EAGAIN) {
			syslog(LOG_ERR, "rhai_ecb_init: %d", ret);
			sleep(10);
		}
	}

	trans.ecb_handle = handle;
	if ((ret = ulh_trans_register("ecb0", &ecb_ops, &trans)))
		goto eout0;

	if ((ret = pthread_create(&rtid, NULL, ecb_reader, &trans)) == 0)
		return ret;

	ulh_trans_unregister("ecb0");

 eout0:
	rhai_ecb_shutdown(handle);
	return ret;
}


extern void *a4ci_thread(void *ctx);

int main(int argc, char *argv[])
{
	pthread_t thread;
	union itc_msg *msg;
	itc_mbox_id_t me, pid;
	void *lnh;
	int ret;

	openlog(NULL, LOG_PID | LOG_ODELAY, LOG_DAEMON);

	ret = itc_init(512, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		syslog(LOG_ERR, "itc_init failure: %d\n",ret);
		goto exit;
	}

	me = itc_create_mailbox(ECB_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "created mbox failure: %d\n",-errno);
		itc_exit();
		goto exit;
	}

	atfi.mpatfi_mbox = itc_clone_mailbox(me, "MXP_0");
	if (atfi.mpatfi_mbox == ITC_NO_ID) {
		syslog(LOG_ERR, "clone mbox failure: %d\n",-errno);
		goto eout;
	}

	ret = ulh_lnh_init(32);
	if (ret) {
		syslog(LOG_ERR, "ulh_lnh_init() failed, %d\n", ret);
		goto eout;
	}

	ret = ulh_ecb_init(ECB_UCM_NAME);
	if (ret) {
		syslog(LOG_ERR, "ulh_ecb_init() failed, %d\n", ret);
		goto eout;
	}

	lnh = ulh_lnh_create(ECB_ULNH_NAME);
	if (!lnh) {
		syslog(LOG_ERR, "ulh_lnh_create() failed\n");
		goto eout;
	}

	for (;; sleep(1)) {
		atfi.server_mbox = itc_locate(ECB_ULNH_NAME);
		if (atfi.server_mbox != ITC_NO_ID)
			break;
	}

	ret =  pthread_create(&thread, NULL, a4ci_thread, 0);
	if (ret < 0) {
		syslog(LOG_ERR, "ecb_init: %d\n",ret);
		goto eout;
	}

	ret = ecb_init();
	if (ret < 0) {
		syslog(LOG_ERR, "ecb_init: %d\n",ret);
		goto eout;
	}

	event_system_start("ATFI/A4CI Servers ready");
	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		pid = itc_sender(msg);

		switch(msg->msgno) {
			case ECB_LINK_CREATE_REQ:
				handle_create(msg, pid);
				break;
			case ECB_LINK_DESTROY_REQ:
				handle_destroy(msg, pid);
				break;
			case ITC_MONITOR_DEFAULT_NO:
				handle_lost_client(msg, pid);
				break;
			case ULH_LNHMSG_NOTIFY:
				handle_notify(msg);
				break;
			case ATFI_CONN_ESTABLISH_REQ:
				handle_new_client(&msg->connEstablishReq, pid);
				break;
			case ATFI_ADD_CONN_MAP_REQ:
				handle_add_map(&msg->addConnMapReq, pid);
				break;
			case ATFI_REMOVE_CONN_MAP_REQ:
				handle_remove_map(&msg->removeConnMapReq, pid);
				break;
			case ATFI_CONNECT2_REQ:
				handle_connect(&msg->connect2Req, pid);
				break;
			case ATFI_DISCONNECT_REQ:
				handle_disconnect(&msg->disconnectReq, pid);
				break;
			case ATFI_AUDIT_REQ:
				handle_audit(&msg->auditReq, pid);
				break;
			case ATFI_RESET_REQ:
				handle_reset(&msg->atfiResetReq, pid);
				break;
			case ATFI_ADD_AU3_MAP_REQ:
				handle_addAu3Map(&msg->atfiAddAu3MapReq, pid);
				break;
			case ATFI_GET_AU3_PORT_REQ:
				handle_getAu3Port(&msg->atfiGetAu3PortReq, pid);
				break;
			case ATFI_GET_PHY_ENDPOINT_REQ:
				handle_getPhyEndpoint(&msg->atfiGetPhyEndpointReq, pid);
				break;
			case ATFI_SETUP_IDCP_A3_REQ:
				handle_setupIdcpA(&msg->atfiSetupIdcpA3Req, pid);
				break;
			case ATFI_SETUP_IDCP_B3_REQ:
				handle_setupIdcpB(&msg->atfiSetupIdcpB3Req, pid);
				break;
			case ATFI_RELEASE_IDCP_A_REQ:
				handle_releaseIdcpA(&msg->atfiReleaseIdcpAReq, pid);
				break;
			case ATFI_RELEASE_IDCP_B_REQ:
				handle_releaseIdcpB(&msg->atfiReleaseIdcpBReq, pid);
				break;
			case ATFI_ADD_AISG_MAP2_REQ:
				handle_addAisgMap2(&msg->atfiAddAisgMap2Req, pid);
				break;
			case ATFI_ADD_CPRI_MAP2_REQ:
				handle_addCpriMap2(&msg->atfiAddCpriMap2Req, pid);
				break;
			case ATFI_GET_AISG_UNIQUE_ID_REQ:
				handle_getAisgUniqueIdReq(&msg->atfiGetAisgUniqueIdReq, pid);
				break;
			default:
				itc_free(&msg);
				break;
		}
	}
	ulh_lnh_destroy(lnh);

 eout:
	itc_delete_mailbox(me);
	itc_exit();

 exit:
	for (;;) {
		syslog(LOG_ERR, "ecblnh - init failed, looping forever\n");
		sleep(600);
	}
	return -1;
}

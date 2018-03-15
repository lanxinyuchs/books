/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2013 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef _ROLC_LINK_INTERNAL_H_
#define _ROLC_LINK_INTERNAL_H_

#include "rolc-link-api.h"


#define ROLC_DAEMON_NAME				"rolclh"


#define ROLC_CREATE_REQ				(0x01900407)
struct rolc_create_req {
	uint32_t	msgno;
	void 		*owner;
	uint32_t 	ri_port;
	uint32_t 	ri_addr;
	uint32_t 	mode;
	char	 	name[ROLC_LINKNAME_SIZE];
};

#define ROLC_CREATE_RSP				(0x01900408)
struct rolc_create_rsp {
	uint32_t 	msgno;
	uint32_t 	linkid;
	int			result;
	char	 	name[ROLC_LINKNAME_SIZE];
};

#define ROLC_DELETE_REQ				(0x01900409)
struct rolc_delete_req {
	uint32_t	msgno;
	int			reserved;
	void 		*owner;
	uint32_t 	linkid;
};

#define ROLC_DELETE_RSP				(0x0190040a)
struct rolc_delete_rsp {
	uint32_t	msgno;
	int			result;
	void 		*owner;
	uint32_t 	linkid;
};

#define ROLC_INBAND_SUB_REQ			(0x0190040b)
struct rolc_inband_req {
	uint32_t 	msgno;
	uint32_t 	resv;
	uint32_t 	port;
	uint32_t 	addr;
};

#define ROLC_INBAND_SUB_RSP			(0x0190040c)
struct rolc_inband_rsp {
	uint32_t 	msgno;
	uint32_t 	result;
	uint32_t 	port;
	uint32_t 	addr;
};

#define ROLC_INBAND_UNSUB_REQ		(0x0190040d)
struct rolc_noband_req {
	uint32_t 	msgno;
	uint32_t 	resv;
	uint32_t 	port;
	uint32_t 	addr;
};

#define ROLC_INBAND_UNSUB_RSP		(0x0190040e)
struct rolc_noband_rsp {
	uint32_t 	msgno;
	uint32_t 	result;
	uint32_t 	port;
	uint32_t 	addr;
};

#define ROLC_REMOTEPORTID_REQ		(0x01900432)
struct rolc_remoteportid_req {
	uint32_t 	msgno;
	uint32_t 	link;
};

#define ROLC_REMOTEPORTID_RSP		(0x01900433)
struct rolc_remoteportid_rsp {
	uint32_t 	msgno;
	uint32_t 	result;
	uint32_t 	link;
};

#define ROLC_LINKSWITCH_REQ			(0x01900434)
struct rolc_linkswitch_req {
	uint32_t 	msgno;
	uint32_t 	link;
	uint32_t 	remote_port;
	uint32_t 	to_link;
};

#define ROLC_LINKSWITCH_RSP			(0x01900435)
struct rolc_linkswitch_rsp {
	uint32_t 	msgno;
	uint32_t 	result;
	uint32_t 	link;
	uint32_t 	remote_port;
	uint32_t 	to_link;
};
#endif

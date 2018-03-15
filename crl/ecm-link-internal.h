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

#ifndef _ECM_LINK_INTERNAL_H_
#define _ECM_LINK_INTERNAL_H_

#include "ecm-link-api.h"


#define ECM_DAEMON_NAME				"ecmlc"
#define ECM_LINKNAME_SIZE				32


#define ECM_CREATE_REQ				(0x0190041b)
struct ecm_create_req {
	uint32_t		msgno;
	void 			*owner;
	struct ecm_link_config	config;
	char	 		name[ECM_LINKNAME_SIZE];
};

#define ECM_CREATE_RSP				(0x0190041c)
struct ecm_create_rsp {
	uint32_t 	msgno;
	uint32_t 	linkid;
	int		result;
	char	 	name[ECM_LINKNAME_SIZE];
};

#define ECM_DELETE_REQ				(0x0190041d)
struct ecm_delete_req {
	uint32_t	msgno;
	int		reserved;
	void 		*owner;
	uint32_t 	linkid;
};

#define ECM_DELETE_RSP				(0x0190041e)
struct ecm_delete_rsp {
	uint32_t	msgno;
	int		result;
	void 		*owner;
	uint32_t 	linkid;
};

#endif

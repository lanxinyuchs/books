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

#ifndef _ECB_LINK_INTERNAL_H_
#define _ECB_LINK_INTERNAL_H_

#include "ecb-link-api.h"


#define ECB_DAEMON_NAME					"ecbld"

#define ECB_LINK_CREATE_REQ			(0x01900401)
#define ECB_LINK_CREATE_RSP			(0x01900402)

struct ecb_link_create {
	uint32_t				msgno;
	void					*owner;
	uint32_t				address;
	uint32_t				station;
	uint32_t				linkid;
	int						result;
	char					name[ECB_LINKNAME_SIZE];
};


#define ECB_LINK_DESTROY_REQ		(0x01900403)
#define ECB_LINK_DESTROY_RSP		(0x01900404)

struct ecb_link_destroy {
	uint32_t				msgno;
	void					*owner;
	uint32_t				linkid;
	int						result;
};


#endif

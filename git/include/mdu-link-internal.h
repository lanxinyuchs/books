/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef _MDU_LINK_INTERNAL_H_
#define _MDU_LINK_INTERNAL_H_

#include "mdu-link-api.h"


#define MDU_DAEMON_NAME					"mdulh"
#define MDU_LINKNAME_SIZE				32


#define MDU_CREATE_REQ				(0x01900422)
struct mdu_create_req {
	uint32_t					msgno;
	void 						*owner;
	struct mdu_link_config		config;
	char	 					name[MDU_LINKNAME_SIZE];
};

#define MDU_CREATE_RSP				(0x01900423)
struct mdu_create_rsp {
	uint32_t 	msgno;
	uint32_t 	linkid;
	int			result;
	char	 	name[MDU_LINKNAME_SIZE];
};

#define MDU_DELETE_REQ				(0x01900424)
struct mdu_delete_req {
	uint32_t	msgno;
	int			reserved;
	void 		*owner;
	uint32_t 	linkid;
};

#define MDU_DELETE_RSP				(0x01900425)
struct mdu_delete_rsp {
	uint32_t	msgno;
	int			result;
	void 		*owner;
	uint32_t 	linkid;
};

#endif

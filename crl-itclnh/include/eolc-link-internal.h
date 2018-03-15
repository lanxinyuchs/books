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

#ifndef _EOLC_LINK_INTERNAL_H_
#define _EOLC_LINK_INTERNAL_H_

#include "eolc-link-api.h"


#define EOLC_DAEMON_NAME				"eolclh"
#define EOLC_LINKNAME_SIZE				32


#define EOLC_CREATE_REQ				(0x0190041b)
struct eolc_create_req {
	uint32_t					msgno;
	void 						*owner;
	union eolc_link_config		config;
	char	 					name[EOLC_LINKNAME_SIZE];
};

#define EOLC_CREATE_RSP				(0x0190041c)
struct eolc_create_rsp {
	uint32_t 	msgno;
	uint32_t 	linkid;
	int			result;
	char	 	name[EOLC_LINKNAME_SIZE];
};

#define EOLC_DELETE_REQ				(0x0190041d)
struct eolc_delete_req {
	uint32_t	msgno;
	int			reserved;
	void 		*owner;
	uint32_t 	linkid;
};

#define EOLC_DELETE_RSP				(0x0190041e)
struct eolc_delete_rsp {
	uint32_t	msgno;
	int			result;
	void 		*owner;
	uint32_t 	linkid;
};

#endif

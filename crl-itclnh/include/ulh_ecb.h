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

#ifndef _ULH_ECB_H_
#define _ULH_ECB_H_

#include <stdint.h>

struct ulh_cm_ecb_config {
	struct ulh_cm_config    cmn;
	uint32_t				address;
	uint32_t				station;
};


extern int ulh_ecb_init(const char *name);

#endif


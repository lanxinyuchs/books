/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and dissemination to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef _SIGNALS_H
#define _SIGNALS_H

#include <stdint.h>

typedef enum {
	SERVER_ERROR,
	SERVER_OK
} server_status_e;


#define TPT_TEST_ONE_REQ       0x09
struct tpt_test_one_req {
	uint32_t msgno;
	uint32_t int1;
	uint32_t int2;
};

#define TPT_TEST_ONE_CFM       0x09
struct tpt_test_one_cfm {
	uint32_t        msgno;
	server_status_e status;
};

#define TPT_TEST_TWO_REQ       0x0b
struct tpt_test_two_req {
	uint32_t msgno;
	uint32_t an_array[10];
};

#endif /* _SIGNALS_H */

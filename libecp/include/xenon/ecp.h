/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#ifndef ECP_H_
#define ECP_H_

#define ECP_NO_OF_TBUFF          16

struct ecp_connection {
	uint32_t cid;
	uint32_t buff_index;
	uint32_t tbuff_busy_mask;
	struct ulh_tbuff tbuff[ECP_NO_OF_TBUFF];
	struct ulh_tbuff_rbuf *rbuf[ECP_NO_OF_TBUFF];
};

#endif

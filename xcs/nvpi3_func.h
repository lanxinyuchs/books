/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#ifndef _NVPI3_FUNC_H_
#define _NVPI3_FUNC_H_

#include "conn-establish-helper.h"
#include "nvpi3_msg.h"

#define NVPI3_CONN_ESTABLISH_MESSAGES_STRUCT(name)              \
	static struct conn_establish_msg_numbers name =         \
	{                                                       \
		NVPI3_CONN_ESTABLISH_REQ,                       \
		NVPI3_CONN_ESTABLISH_CFM,                       \
		NVPI3_CONN_ESTABLISH_REJ,                       \
		NVPI3_CONN_DISCONNECT_REQ,                      \
		NVPI3_CONN_DISCONNECT_CFM,                      \
		NVPI3_CONN_DISCONNECT_REJ,                      \
		NVPI3_CONN_MONITOR_FWD,                         \
	}

#endif

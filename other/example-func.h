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

#ifndef _EXAMPLE_FUNC_H_
#define _EXAMPLE_FUNC_H_

#include "conn-establish-helper.h"
#include "conn-establish.h"

/*
  You can use a macro like this to define the set of messages to use for
  the connection establish mechanism on both server and client side.
  This information is needed by conn_establish_server_init(), conn_establish()
   and conn_disconnect().
*/
#define EXAMPLE_CONN_ESTABLISH_MESSAGES_STRUCT(name)              \
	struct conn_establish_msg_numbers  name =                 \
	{                                                         \
		EXAMPLE_CONN_ESTABLISH_REQ,                       \
		EXAMPLE_CONN_ESTABLISH_CFM,                       \
		EXAMPLE_CONN_ESTABLISH_REJ,                       \
		EXAMPLE_CONN_DISCONNECT_REQ,                      \
		EXAMPLE_CONN_DISCONNECT_CFM,                      \
		EXAMPLE_CONN_DISCONNECT_REJ,                      \
		EXAMPLE_CONN_MONITOR_FWD,                         \
	}

/*Your own functions to do the real work.*/
int example_interact_with_server(itc_mbox_id_t server_mbox_id,
                                 uint32_t server_ref,
                                 uint32_t client_ref,
                                 uint32_t procedure_ref,
                                 uint32_t argument);
#endif

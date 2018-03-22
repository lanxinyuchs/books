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

#ifndef _EXAMPLE_MSG_H_
#define _EXAMPLE_MSG_H_

#include "conn-establish.h"

#define EXAMPLE_SERVER_VERSIONS  4,3,2,1

#define EXAMPLE_SERVER_NAME     "example_server"

#define EXAMPLE_MSG_BASE 0
/* These message numbers must NEVER change.
   If they do, the connection establish mechanism will break.
   (A suggestion is to put them in the beginning of the message
   number range. Then they will not end up in the middle of the
   range if the interface expands over time.)
*/
#define EXAMPLE_CONN_ESTABLISH_REQ  (EXAMPLE_MSG_BASE + 0x01)
#define EXAMPLE_CONN_ESTABLISH_CFM  (EXAMPLE_MSG_BASE + 0x02)
#define EXAMPLE_CONN_ESTABLISH_REJ  (EXAMPLE_MSG_BASE + 0x03)
#define EXAMPLE_CONN_DISCONNECT_REQ (EXAMPLE_MSG_BASE + 0x04)
#define EXAMPLE_CONN_DISCONNECT_CFM (EXAMPLE_MSG_BASE + 0x05)
#define EXAMPLE_CONN_DISCONNECT_REJ (EXAMPLE_MSG_BASE + 0x06)
#define EXAMPLE_CONN_MONITOR_FWD    (EXAMPLE_MSG_BASE + 0x07)

/* Your "real" interface */
#define EXAMPLE_MESSAGE_REQ (EXAMPLE_MSG_BASE + 0x10)
#define EXAMPLE_MESSAGE_CFM (EXAMPLE_MSG_BASE + 0x11)
#define EXAMPLE_MESSAGE_REJ (EXAMPLE_MSG_BASE + 0x12)



/* Your message structs */

#define EXAMPLE_MESSAGES                                          \
	conn_any_msg_t               any_msg;                     \
	conn_establish_req_t         example_conn_establish_req;  \
	conn_establish_cfm_t         example_conn_establish_cfm;  \
	conn_establish_rej_t         example_conn_establish_rej;  \
	conn_disconnect_req_t        example_conn_disconnect_req; \
	conn_disconnect_cfm_t        example_conn_disconnect_cfm; \
	conn_disconnect_rej_t        example_conn_disconnect_rej; \
	struct example_message_req_s example_message_req;         \
	struct example_message_cfm_s example_message_cfm;         \
	struct example_message_rej_s example_message_rej

/* All messages must have a header consisting of
        uint32_t msgno;
        uint32_t procedure_ref;
        uint32_t connection_ref;

  See "conn_establish.h" for more information about the header.

  To facilitate this you might want to use macro to define them. */

#define EXAMPLE_MSG_HEADER                      \
	uint32_t msgno;                         \
	uint32_t procedure_ref;                 \
	uint32_t connection_ref;


struct example_message_req_s {
	EXAMPLE_MSG_HEADER
	uint32_t your_payload;
};
struct example_message_cfm_s {
	EXAMPLE_MSG_HEADER
	uint32_t your_payload;
};
struct example_message_rej_s {
	EXAMPLE_MSG_HEADER
	uint32_t your_payload;
};

#endif

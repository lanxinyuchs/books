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

/* This file contains stuff that we don't want to expose to the
   user application */

#ifndef _EVENT_SERVER_INTERNAL_H_
#define _EVENT_SERVER_INTERNAL_H_
#ifdef __cplusplus
extern "C" {
#endif

/* Internal messages for EVTI. */
#define EVTI_MSGBASE 0x77770000
#define EVTI_SLAVE_CONFIG_IND (EVTI_MSGBASE + 0)
#define EVTI_SLAVE_LOCATE     (EVTI_MSGBASE + 1)
#define EVTI_SUBSC_CMD_REQ    (EVTI_MSGBASE + 2)
#define EVTI_SUBSC_CMD_CFM    (EVTI_MSGBASE + 3)
#define EVTI_SUBSC_CMD_REJ    (EVTI_MSGBASE + 4)
#define EVTI_SUBSC_CMD_IND    (EVTI_MSGBASE + 5)

#define EVTI_TAG_SIZE 32 /*Identical to EVTI_MAX_TAG_LENGTH*/

/* Message structs locally used within this repository. */
typedef struct evti_slave_config_ind {
	uint32_t msgno;
	uint32_t num_subscribers;
} evti_slave_config_ind_s;

typedef struct evti_subsc_cmd_req {
	uint32_t msgno;
} evti_subsc_cmd_req_s;

typedef struct evti_subsc_cmd_cfm {
	uint32_t msgno;
} evti_subsc_cmd_cfm_s;

typedef struct evti_subsc_cmd_rej_s {
	uint32_t msgno;
} evti_subsc_cmd_rej_s;

typedef struct evti_subsc_cmd_ind {
	uint32_t      msgno;
	char          tag[EVTI_TAG_SIZE];
	uint32_t      num_subscribers;
	itc_mbox_id_t subscribers[1]; /* Variable length */
} evti_subsc_cmd_ind_s;

#ifdef __cplusplus
}
#endif

#endif /* _EVENT_SERVER_INTERNAL_H_ */

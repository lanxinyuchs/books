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
#include "signals.h"
#include "server.h"
#include "server_log.h"

typedef enum {
	SERVER_STATE_INACTIVE,
	SERVER_STATE_ACTIVE
} server_state_e;

void server_handle_one(struct tpt_test_one_req *one_req)
{
	static server_state_e server_state = SERVER_STATE_INACTIVE;
	TPT_TRACE_OBJ(1, SERVER_OBJ_ONE, "Handle one signal");
	TPT_TRACE_OBJ(2, SERVER_OBJ_ONE, STR("int1 = %u int2 = %u", one_req->int1, one_req->int2));

	if(server_state == SERVER_STATE_INACTIVE) {
		server_state = SERVER_STATE_ACTIVE;
		TPT_OBJ_STATE(SERVER_OBJ_ONE, "SERVER_STATE_INACTIVE => SERVER_STATE_ACTIVE");
	} else {
		server_state = SERVER_STATE_INACTIVE;
		TPT_OBJ_STATE(SERVER_OBJ_ONE, "SERVER_STATE_ACTIVE => SERVER_STATE_INACTIVE");
	}

	return;
}

void server_handle_two(struct tpt_test_two_req *two_req)
{
	static uint32_t some_array[10];

	TPT_TRACE_OBJ(1, SERVER_OBJ_TWO, "Handle two signal");

	memcpy(some_array, two_req->an_array, sizeof(some_array));
	TPT_OBJ_DATA(SERVER_OBJ_TWO, "Some array set to",
			(const char *)some_array, sizeof(some_array));

	return;
}

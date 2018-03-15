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

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "xpai_xte_if.h"
#include "log_tpt.h"

#define TS_RESTART "ts restart --if-correlate-enabled"
#define TE_RESTART "te restart"

/****
 *
 *      XPAI_SetUtcTime
 *
 *****/

static void exec(char * cmd)
{
	int res = 0;

	//res = system(cmd);

	if (res == -1) {
		TPT_ERROR(STR("Failed to execute '%s'", cmd));
	}
	else if (res != 0) {
		TPT_ERROR(STR("'%s' returned non-zero error code %d", cmd, res));
	}
}

S32 XPAI_SetUtcTime(U32 seconds, U32 microseconds)
{
	struct timespec tp;
	int res;

	TPT_TRACE(1, STR("XPAI_SetUtcTime, seconds: %u, microseconds: %u",
			seconds, microseconds));

	tp.tv_sec = seconds;
	tp.tv_nsec = (long)microseconds * 1000; /* convert to ns */

	res = clock_settime(CLOCK_REALTIME, &tp);
	if (res) {
		TPT_ERROR(STR("clock_settime returned %d", res));
		return XPAI_SET_UTC_TIME_NOK_OTHER;
	}

	/*
	 * Best effort to restart the logging, since lttng currently doesn't
	 * properly support the system time changing. Failure will be logged,
	 * but doesn't cause this call to fail.
	 */

	exec(TS_RESTART);
	exec(TE_RESTART);


	return XPAI_SET_UTC_TIME_OK;
}

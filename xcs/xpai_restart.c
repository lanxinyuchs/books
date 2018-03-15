/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
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
#include <unistd.h>
#include <libgen.h>

#include "common.h"
#include "libreboot.h"
#include "xpai_xcbc_basic_if.h"
#include "log_tpt.h"

#define MAX_PROGRAM_NAME_SIZE 512

/****
 *
 *      Function XPAI_RestartBoard2
 *
 *****/
void XPAI_RestartBoard2(char *loadmodule, char *traceInformation)
{
	char *program = NULL;
	char buf[MAX_PROGRAM_NAME_SIZE];
	FILE *f = fopen("/proc/self/cmdline", "r");

	TPT_TRACE(1, "XPAI_RestartBoard2");

	if (f) {
		program = fgets(buf, MAX_PROGRAM_NAME_SIZE, f);
		program = basename(program);
		fclose(f);
	}

	/* Fail safe */
	if (!program)
		program = "Application";

	reboot_with_pid(program, traceInformation, loadmodule);
}

int32_t xpai_restart_init(void)
{
	return INIT_OK;
}

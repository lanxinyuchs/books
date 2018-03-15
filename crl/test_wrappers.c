/**
 *   Wrapper for function run_shell_cmd.
 *
 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2016-04-06 Fredrik Skog
 *   Change  : First version.
 *
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

int __real_run_shell_cmd(const char *cmd);

int __wrap_run_shell_cmd(const char *cmd)
{
   return 0;
}


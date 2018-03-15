/**
 *   Test of process termination handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2011 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2011-04-01 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ose.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
union SIGNAL
{
    SIGSELECT   sig_no;
};

static int  error_handled = 0;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Block error handler that is used during test of process termination.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zero if
 *                     called via error() and error2().
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           Set to non-zero to indicate that the error is
 *                     handled
 *
 *   @par Globals:
 *                     error_handled
 */
/* ===================================================================== */
static OSADDRESS
block_error_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra)
{
    if ( !user_called && (ecode == 0x107) )
    {
        error_handled = 1;
        return 1;
    }

#if STATIC_TERMINATION_NOT_FATAL
    if ( !user_called && (ecode == 0x80000048) )
        exit(EXIT_SUCCESS);
#endif

    return 0;
}

/** ==================================================================== */
/**
 *   Test process that terminates "abnormally".
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(test_proc)
{
    PROCESS  pid = current_process();

    printf("The dynamic process 0x%08x (%d) is started\n", pid, pid);
}

/** ==================================================================== */
/**
 *   Main test process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     error_handled
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    PROCESS       pid;
    union SIGNAL  *sig;
    SIGSELECT     sel_attach[] = {1, OS_ATTACH_SIG};
    int           passed = 1;

    create_error_handler(get_bid(current_process()), block_error_handler, 200);

    pid = create_process(OS_PRI_PROC, "terminate", test_proc, 2000, 15, (OSTIME)0,
                         0,0, 0, 0);
    attach(NULL, pid);
    start(pid);

    if ( (sig = receive_w_tmo(1000, sel_attach)) == NIL )
    {
        passed = 0;
        printf("The dynamic process 0x%08x (%d) is not terminated: Fail\n", pid, pid);
    }
    else if ( !error_handled )
    {
        passed = 0;
        printf("The dynamic process 0x%08x (%d) is terminated but no error was issued: Fail\n",
               pid, pid);
    }
    else
    {
        passed = sender(&sig) == pid;
        printf("The dynamic process 0x%08x (%d) is terminated", pid, pid);
        printf(": %s\n", passed ? "OK" : "Fail");
    }

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

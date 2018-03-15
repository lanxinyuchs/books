/**
 *   Test of start hook handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2011-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-01-09 Lars Jönsson EAB/FJP/TB
 *   Change  : Increased time for collecting hunt signals.
 *
 *   Revised : 2011-04-01 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <ose.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define HUNT_SIG	1234567890
#define	NUM_HUNTS	6
#define	NUM_HOOKS	(NUM_HUNTS + 1)

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

static int  passed = 1;
static int  num_hooks = 0;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Check of start hook handling
 *
 *   @param str        String to precede the result string
 *   @param expected   Expected result of hunt
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 *
 *   The hooks are tested by hunting for a static process. The process
 *   should not be visible to start hook 1 but visible to start hook 2.
 */
/* ===================================================================== */
static void
check_hook(const char *str, OSBOOLEAN expected)
{
    PROCESS      pid;
    union SIGNAL *sig;
    OSBOOLEAN    res;
    int          test_passed;

    num_hooks++;

    pid = current_process();
    printf("%s PID: %u (0x%08x) - ", str, pid, pid);

    sig = alloc(sizeof(SIGSELECT), HUNT_SIG);

    res = hunt("static_proc", 0, &pid, &sig);

    test_passed  = res == expected;
    passed      &= test_passed;

    if ( res )
        printf("\"static_proc\" PID: %u (0x%08x)", pid, pid);
    else
        printf("\"static_proc\" is not found");

    printf(": %s\n", test_passed ? "OK" : "Fail");
}

/** ==================================================================== */
/**
 *   A couple of start hook 1 and 2
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     passed
 */
/* ===================================================================== */
void hook1_number_1(void)
{
    check_hook(__FUNCTION__, 0);
}
void hook1_number_2(void)
{
    check_hook(__FUNCTION__, 0);
}
void hook1_number_3(void)
{
    check_hook(__FUNCTION__, 0);
}
void hook2_number_1(void)
{
    check_hook(__FUNCTION__, 1);
}
void hook2_number_2(void)
{
    check_hook(__FUNCTION__, 1);
}
void hook2_number_3(void)
{
    check_hook(__FUNCTION__, 1);
}

/** ==================================================================== */
/**
 *   Last Start hook 2, which collects and checks that the expected number
 *   of hunt signals are received.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     passed
 */
/* ===================================================================== */
void
collect_hunt_sigs(void)
{
    union SIGNAL *sig;
    SIGSELECT    all[] = {0};
    int          i = 0;
    int          test_passed;

    num_hooks++;

    while( (sig = receive_w_tmo(50, all)) != NULL )
    {
        free_buf(&sig);
        i++;
    }

    test_passed  = i == NUM_HUNTS;
    passed      &= test_passed;

    printf("Collected %d hunt signals: %s\n", i, test_passed ? "OK" : "Fail");
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
 *                     passed
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    int  test_passed;

    test_passed  = num_hooks == NUM_HOOKS;
    passed      &= test_passed;

    printf("%d start hooks have been run: %s\n",
           num_hooks, test_passed ? "OK" : "Fail");

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

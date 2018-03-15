/**
 *   Test stack usage. Assert that Lits internal message printer uses
 *   less stack than fprintf.
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
 *   Revised : 2014-04-09 Ravineet Singh EAB/FJPHB
 *   Change  : Adapt the test case in case env variable LITS_MIN_STACK_SIZE
 *             is set.
 *
 *   Revised : 2014-02-07 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.

 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/time.h>

#include <lits_internal.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define	protect_expected_segv()   pthread_mutex_lock(&expected_segv_mutex)
#define	unprotect_expected_segv() pthread_mutex_unlock(&expected_segv_mutex)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static int  expected_segv = 0;
static pthread_mutex_t expected_segv_mutex = PTHREAD_MUTEX_INITIALIZER;
/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
/** ==================================================================== */
/**
 *   Read var
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
read_expected_segv(void)
{
    int read;
    protect_expected_segv();
    read = expected_segv;
    unprotect_expected_segv();
    return read;
}

/** ==================================================================== */
/**
 *   Write var
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
write_expected_segv(int write)
{
    protect_expected_segv();
    expected_segv = write;
    unprotect_expected_segv();
}

/** ==================================================================== */
/**
 *   SIGSEGV signal handler, does not return
 *
 *   @param          signo
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
handler_segv(int signo)
{
    (void)signo;

    if (read_expected_segv())
    {
        printf("PASS\n");
        exit(0);
    }
    else
    {
        printf("FAIL\n");
        exit(1);
    }
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
 *                     --
 */
/* ===================================================================== */
#define PARAMS "This messages should %s be printed", read_expected_segv() ? "not":"indeed"
OS_PROCESS(test)
{
    struct sigaction segv;
    stack_t sigstack;
    int ret;

    /* Use signal stack and install signal handler for SEGV */
    sigstack.ss_sp = malloc(SIGSTKSZ);
    assert (sigstack.ss_sp != NULL);
    sigstack.ss_size = SIGSTKSZ;
    sigstack.ss_flags = 0;
    ret = sigaltstack(&sigstack, NULL);
    assert (ret ==0);

    memset(&segv, 0, sizeof(sigaction));
    sigemptyset (&segv.sa_mask);
    segv.sa_flags   = SA_SIGINFO|SA_ONSTACK;
    segv.sa_handler = handler_segv;
    ret = sigaction (SIGSEGV, &segv, NULL);
    assert(ret == 0);

    /* Lits message printer should be fine with low stack usage. */
    write_expected_segv(0);
    lits_print_message(PARAMS);

    /*
     * If application has defined min stack size (statically configured or
     * runtime configured), this may or may not pass depending on the size
     * defined. The size on the other hand can not be retained...
     * so don't test, just pass.
     */
#if defined LITS_MIN_STACK_SIZE
    printf("Not tested (LITS_MIN_STACK_SIZE was staticaly configured)\n");
    printf("PASS\n");
    exit(0);
#endif

    if (get_env(get_bid(current_process()), "LITS_MIN_STACK_SIZE"))
    {
        printf("Not tested (env var LITS_MIN_STACK_SIZE was set).\n");
        printf("PASS\n");
        exit(0);
    }

    /* fprintf, stderr should use more stack, hence SEGV. */
    write_expected_segv(1);
    fprintf(stderr, PARAMS);
    fprintf(stderr, "\n");
    printf("FAIL\n");

    exit(-1);
}

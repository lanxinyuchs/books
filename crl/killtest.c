/**
 *   Test of process kill handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2011-2013 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-02-01 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test of stop + kill.
 *
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
#define	NUM_PROCS	8

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

static const int order[] = {5, 6, 4, 1, 2, 7, 3, 0};

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Test process that just stops after a short time.
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
#if CHECK_OF_FILE_HANLDING_AT_KILL
    {
        char name[] = "lars-XXXXXX";
        FILE  *fh;

        fh = fopen(mktemp(name), "w");
        printf("File %s is created and open\n", name);
    }
#endif
    while (1)
        delay(1000);
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
 *                     order[]
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    union SIGNAL  *sig;
    SIGSELECT     selAll[] = {0};
    PROCESS       pid[NUM_PROCS];
    PROCESS       current;
    PROCESS       killed;
    int           i;
    char          name[] = "test_proc-0";
    int           passed = 1;

    /*
    **  Start all dynamic processes
    */
    for (i = 0; i < NUM_PROCS; i++)
    {
        name[strlen(name)-1] = (char)('0' + (i % 10));

        pid[i] = create_process(OS_PRI_PROC, name, test_proc, 2000, 15, (OSTIME)0, 0,0,
                                0, 0);
        attach(NULL, pid[i]);
        start(pid[i]);
        delay(100);
    }

    /*
    **  Kill all dynamic processes
    */
    for (i = 0; i < NUM_PROCS; i++)
    {
        current = pid[order[i]];
        printf("Killing the dynamic process 0x%08x (%d)\n", current, current);

        /* Test with both stopped and non-stopped processes */
        if ( (i % 2) == 0 )
            stop(current);

        kill_proc(current);

        if ( (sig = receive_w_tmo(100, selAll)) == NIL )
        {
            passed = 0;
            printf("The dynamic process 0x%08x (%d) is not terminated now", current,
                   current);
            printf(": %s\n", passed ? "OK" : "Fail");
            continue;
        }

        switch (sig->sig_no)
        {
            case OS_ATTACH_SIG:
                killed = sender(&sig);
                passed &= killed == current;
                printf("The dynamic process 0x%08x (%d) is terminated", killed, killed);
                printf(": %s\n", killed == current ? "OK" : "Fail");
                break;

            default:
                printf("Unknown signal [%d] from process %d\n", sig->sig_no, sender(&sig));
                break;
        }
    }

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

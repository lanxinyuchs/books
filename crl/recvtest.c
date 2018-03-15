/**
 *   Test of receive timeout.
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
 *   Revised : 2014-03-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Ensured that test cases will run with both real-time
 *             echeduling enabled and disabled.
 *
 *   Revised : 2011-04-13 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <sched.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
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

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Worker process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   The signal number is used as a delay time before the signal is
 *   returned to the sender.
 */
/* ===================================================================== */
OS_PROCESS(worker)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;

    while(1)
    {
        sig = receive(selAll);
        if ( sig->sig_no ) delay(sig->sig_no);
        send(&sig, sender(&sig));
    }
}

/** ==================================================================== */
/**
 *   Checks the result of environment variables handling
 *
 *   @param str        String to precede the result string
 *   @param timeout    Time to wait for signal
 *   @param from       Process ID to recevi from. Recevive from any if
 *                     set to 0.
 *   @param expected   0 if no signal is expected to be recevied.
 *                     Otherwise non-zero.
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_recv_tmo(char *str, OSTIME timeout, PROCESS from, int expected)
{
    int           passed = 1;
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;

    if ( from )
        sig = zzreceive_from(timeout, selAll, from);
    else
        sig = receive_w_tmo(timeout, selAll);

    if ( expected )
        passed &= sig != NIL;
    else
        passed &= sig == NIL;

    if ( from )
        printf("%s [%ssignal received from pid=%d]: %s\n",
               str, sig == NIL ? "no " : "", from, passed ? "OK" : "Fail");
    else
        printf("%s [%ssignal received]: %s\n",
               str, sig == NIL ? "no " : "", passed ? "OK" : "Fail");

    if ( sig != NIL )
        free_buf(&sig);

    return passed;
}

/** ==================================================================== */
/**
 *   Restricts execution of the current process and children to the
 *   specified core.
 *
 *   @param core       Core number
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
set_core(int core)
{
    int       passed = 1;
    cpu_set_t mask;

    /*
    **  Make both proceses are running on the same core, to ensure that the
    **  worker process always runs before the test process
    */
    CPU_ZERO(&mask);
    CPU_SET(core, &mask);

    passed &= sched_setaffinity(0, sizeof(mask), &mask) == 0;

    printf("Restrict execution to one core [core=%d]: %s\n",
           core, passed ? "OK" : "Fail");

    return passed;
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
 *
 *   All environment variables are actually set on block level and share
 *   among all processes.
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    int           passed = 1;
    int           prio_enabled = 0;
    union SIGNAL  *sig;
    PROCESS       worker_pid;

    if (set_pri(15) < 32)
    {
        /*
        **  Make this process and all children run on the same core, to ensure
        **  that the worker process always runs before the test process (use
        **  process priorities  for synchronization)
        */
        prio_enabled = 1;
        passed &= set_core(sched_getcpu());
    }
    else
        printf("Priority is not enabled! Skipping test that rely in priorities.\n\n");

    worker_pid = create_process(OS_PRI_PROC, "worker", worker, 2000, 10, 0, 0, 0, 0,
                                0);
    start(worker_pid);

    printf("No signal in queue:\n");
    passed &= check_recv_tmo("- Receive from a non-sending process", 100,
                             current_process(), 0);
    passed &= check_recv_tmo("- Receive from any process", 100, 0, 0);

    sig = alloc(sizeof(union SIGNAL), 0);
    send(&sig, worker_pid);

    printf("Signal in queue:\n");
    passed &= check_recv_tmo("- Receive from a non-sending process", 100,
                             current_process(), 0);
    passed &= check_recv_tmo("- Receive from a sending process", 100, worker_pid,
                             1);

    sig = alloc(sizeof(union SIGNAL), 0);
    send(&sig, worker_pid);

    printf("Signal in queue:\n");
    passed &= check_recv_tmo("- Receive from a non-sending process", 100,
                             current_process(), 0);
    passed &= check_recv_tmo("- Receive from any process", 100, 0, 1);

    if (prio_enabled)
    {
        sig = alloc(sizeof(union SIGNAL), 0);
        send(&sig, worker_pid);

        printf("Signal in queue (with timeout set to 0):\n");
        passed &= check_recv_tmo("- Receive from a non-sending process", 0,
                                 current_process(), 0);
        passed &= check_recv_tmo("- Receive from a sending process", 0, worker_pid, 1);

        sig = alloc(sizeof(union SIGNAL), 0);
        send(&sig, worker_pid);

        printf("Signal in queue (with timeout set to 0):\n");
        passed &= check_recv_tmo("- Receive from a non-sending process", 0,
                                 current_process(), 0);
        passed &= check_recv_tmo("- Receive from any process", 0, 0, 1);
    }

    sig = alloc(sizeof(union SIGNAL), 500);
    send(&sig, worker_pid);

    printf("Delayed signal in queue (to short timeout):\n");
    passed &= check_recv_tmo("- Receive from a non-sending process", 100,
                             current_process(), 0);
    passed &= check_recv_tmo("- Receive from any process", 100, 0, 0);

    printf("Delayed signal in queue (timeout long enough):\n");
    passed &= check_recv_tmo("- Receive from a non-sending process", 100,
                             current_process(), 0);
    passed &= check_recv_tmo("- Receive from any process", 500, 0, 1);

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

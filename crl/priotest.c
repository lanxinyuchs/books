/**
 *   Test of priority handling.
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
 *   Revised : 2014-09-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Phantom priority is 0(zero), always.
 *
 *   Revised : 2014-03-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Ensured that test cases will run with both real-time
 *             echeduling enabled and disabled.
 *
 *   Revised : 2014-02-03 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed definitions of priority levels. They are available
 *             in lits_internal.h.
 *
 *   Revised : 2014-01-23 Stanislav Vovk
 *   Change  : Added rlimit not set warning message
 *
 *   Revised : 2013-12-02 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test of background processes.
 *
 *   Revised : 2013-11-28 Lars Jönsson EAB/FJP/TB
 *   Change  : Changed mapping of OSE priorities to pthread priorities.
 *
 *   Revised : 2011-04-01 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <pthread.h>
#include <sched.h>
#include <stdlib.h>
#include <ose.h>
#include <stdio.h>
#include <sys/resource.h>

#include <lits_internal.h>
#include <tcb.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/*
**  Convertion of OSE priorities to Pthread priorities
*/
#define	PTHREAD_PRIO(prio, base, levels)	((base + levels - 1) - prio)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Test interrupt process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(int_proc)
{
}

/** ==================================================================== */
/**
 *   Test background process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(bg_proc)
{
    stop(current_process());
}

/** ==================================================================== */
/**
 *   Checks the result of the priority setting and prints the result
 *
 *   @param str        String to precede the result string
 *   @param pid        Process ID of handled process
 *   @param prio       Expected OSE priority
 *   @param pt_prio    Expected pthread priority
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result(char *str, PROCESS pid, OSPRIORITY prio, int pt_prio)
{
    OSPRIORITY          measured_prio;
    int                 measured_pt_prio;
    int                 passed = 1;
    int                 policy;
    struct sched_param  param;
    tcb_t               *tcb;
    struct OS_pcb       *pcb;

    pcb = get_pcb(pid);
    measured_prio = pcb->priority;
    free_buf( (union SIGNAL **) &pcb);

    /*
    ** Return value of get_tcb() is not checked because the validity of
    ** the pid is already checked in get_pcb() above
    */
    tcb = get_tcb(pid);
    pthread_getschedparam(tcb->tid, &policy, &param);
    measured_pt_prio = param.sched_priority;

    passed  = prio == measured_prio;
    passed &= pt_prio == measured_pt_prio;

    printf("%s - ", str);
    printf("%s - ",
           policy == SCHED_FIFO ? "SCHED_FIFO" :
           policy == SCHED_RR ? "SCHED_RR" :
           policy == SCHED_OTHER ? "SCHED_OTHER" :
           "(unknown)");
    printf("prio=%-2d [expected=%-2d]", measured_prio, prio);
    printf(", pthread prio=%-2d [expected=%-2d]", measured_pt_prio, pt_prio);
    printf(": %s\n",passed ? "OK" : "Fail");

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
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    int  passed = 1;
    int  prio_enabled = 1;
    int  i;
    PROCESS pid;
    struct OS_redir_entry redir[1];
    struct rlimit rlim;

    if ((getrlimit(RLIMIT_RTPRIO, &rlim) == 0) &&
            (rlim.rlim_cur < MAX_USED_RT_PRIO))
    {
        lits_print_message(
            "Required real-time prioity limit(%d) is not set,"
            " falling back to SCHED_OTHER\n", MAX_USED_RT_PRIO);
        /*           1         2         3         4 */
        /*  1234567890123456789012345678901234567890 */
        lits_print_message(
            "Either run as root or update rtprio for current user in "
            "/etc/security/limits.conf\n"
            "(or similar) to really verify the priority handling.\n");
        prio_enabled = 0;
    }

    /*
    ** Set all prios for the prioritized processes and check that both the OSE
    ** priorities and the pthread priorities are set corerctly.
    */
    printf("Checking priority of Prioritized processes:\n");
    pid = current_process();

    for (i = 0; i < 32; i++)
    {
        set_pri(i);

        if (prio_enabled)
            passed &= check_result("- Checking prio", pid, i,
                                   PTHREAD_PRIO(i, PRIO_BASE_PRI_PROC, OSE_PRIORITY_LEVELS));
        else
            passed &= check_result("- Checking prio", pid, UNKNOWN_PRIORITY, 0);
    }

#ifdef ENABLE_INT_PROC
    /*
    ** Set all prios for the interrupt processes and check that both the OSE
    ** priorities and the pthread priorities are set corerctly.
    */
    printf("Checking priority of Interrupt processes:\n");

    for (i = 0; i < 32; i++)
    {
        pid = create_process(OS_INT_PROC, "int_proc", int_proc, 2000, i, (OSTIME)0, 0,0,
                             0, 0);

        if (prio_enabled)
            passed &= check_result("- Checking prio", pid, i,
                                   PTHREAD_PRIO(i, PRIO_BASE_INT_PROC, OSE_PRIORITY_LEVELS));
        else
            passed &= check_result("- Checking prio", pid, UNKNOWN_PRIORITY, 0);

        kill_proc(pid);
    }
#endif

    /*
    ** Create a phantom process and check that both the OSE
    ** priorities and the pthread priorities are set correctly.
    */
    printf("Checking priority of Phantom processes:\n");
    redir[0].sig = 1;
    redir[0].pid = current_process();
    pid = create_process(OS_PHANTOM, "Phantom", NULL, 0, 0, 0, 0, redir, 0, 0);

    if (prio_enabled)
#ifdef ENABLE_INT_PROC
       passed &= check_result("- Checking prio", pid, 0,
                              PTHREAD_PRIO(0, PRIO_BASE_PHANTOM, 1));
#else
       passed &= check_result("- Checking prio", pid, 0,
                              (PRIO_BASE_PRI_PROC + OSE_PRIORITY_LEVELS));
#endif
    else
       passed &= check_result("- Checking prio", pid, 0, 0);

    kill_proc(pid);

    /*
    ** Create a background process and check that both the OSE
    ** priorities and the pthread priorities are set correctly.
    */
    printf("Checking priority of Background processes:\n");
    pid = create_process(OS_BG_PROC, "bg_proc", bg_proc, 0, 0, 0, 0, NULL, 0, 0);
    start(pid);
    passed &= check_result("- Checking prio", pid, 0,
                           PTHREAD_PRIO(0, PRIO_BASE_BG_PROC, 1));
    kill_proc(pid);

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

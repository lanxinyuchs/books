/**
 *   Test of sysinfo handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2013-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-02-06 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed compiler warnings.
 *
 *   Revised : 2013-11-19 Stanislav Vovk
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
#include <ose_sysinfo.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define TEST_PRIO 15
#define TEST_SIGSZ 32
#define SIGNO 111


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
 *   Test process
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
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;

    while(1)
    {
        sig = receive(selAll);
        send(&sig, sender(&sig));
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
 *                     order[]
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    union SIGNAL  *sig;
    SIGSELECT sel[] = {1, SIGNO};
    PROCESS pid;
    OSBUFSIZE size;
    OSPRIORITY prio;
    int ret;
    char name[] = "test_proc-0";
    int passed = 1;
    char pnm[32];
    unsigned int state, sst;

    pid = create_process(OS_PRI_PROC, name, test_proc,
                         2000, TEST_PRIO, (OSTIME)0, 0,0, 0, 0);
    start(pid);

    /*
     * sysinfo_proc_name
     */
    printf("Get name for a process, pid:0x%x\n", pid);
    ret = sysinfo_proc_name(pid, sizeof(pnm), pnm);
    if (ret != SYSINFO_NO_ERR)
    {
        printf(" - sysinfo_proc_name() failed\n");
        passed = 0;
    }
    if (strcmp(name, pnm) != 0)
    {
        printf(" -  Wrong name, exp:%s, got:%s\n", name, pnm);
        passed = 0;
    }

    /*
     * sysinfo_proc_state
     */
    printf("\nGet state for a running process, pid:0x%x\n", pid);
    ret = sysinfo_proc_state(pid, &state, &sst);
    if (ret != SYSINFO_NO_ERR)
    {
        printf(" - sysinfo_proc_prio() failed\n");
        passed = 0;
    }
    if (state != SYSINFO_STATE_RUNNING)
    {
        printf(" -  Wrong state, exp:%d, got:%d\n", SYSINFO_STATE_RUNNING, state);
        passed = 0;
    }

    printf("\nGet state for a stopped process, pid:0x%x\n", pid);
    stop(pid);
    ret = sysinfo_proc_state(pid, &state, &sst);
    if (ret != SYSINFO_NO_ERR)
    {
        printf(" - sysinfo_proc_prio() failed\n");
        passed = 0;
    }
    if (state != SYSINFO_STATE_STOPPED)
    {
        printf(" -  Wrong state, exp:%d, got:%d\n", SYSINFO_STATE_STOPPED, state);
        passed = 0;
    }
    start(pid);

    /*
     * sysinfo_proc_prio (returns 255 if real-time scheduling is disabled)
     */
    printf("\nGet prio for a running process, pid:0x%x\n", pid);
    ret = sysinfo_proc_prio(pid, &prio);
    if (ret != SYSINFO_NO_ERR)
    {
        printf(" - sysinfo_proc_prio() failed\n");
        passed = 0;
    }

    if ((prio != 255) && (prio != TEST_PRIO))
    {
        printf(" -  Wrong priority, exp:%d, got:%d\n", TEST_PRIO, prio);
        passed = 0;
    }

    /*
     * sysinfo_signal_size
     */
    printf("\nGet signal size\n");
    sig = alloc(TEST_SIGSZ, SIGNO);
    send(&sig, pid);
    sig = receive_w_tmo(10, sel);
    ret = sysinfo_signal_size(sig, &size);
    free_buf(&sig);
    if (ret != SYSINFO_NO_ERR)
    {
        printf(" - sysinfo_signal_size() failed\n");
        passed = 0;
    }
    if (size != TEST_SIGSZ)
    {
        printf(" -  Wrong signal size, exp:%d, got:%d\n", TEST_SIGSZ, (int)size);
        passed = 0;
    }

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");
    exit(passed ? 0 : -1);
}

/**
 *   Test of process handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2011-2016 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2016-02-12 Anette Sch�tt
 *   Change  : Add one more test case for getenv test of variables set in
 *             osemain.c.
 *
 *   Revised : 2016-02-09 Anette Sch�tt
 *   Change  : Add test for the usage of global variable instead for
 *             setenv() and getenv() in osemain.c and exec.c.
 *
 *   Revised : 2015-12-16 Magnus Lindberg
 *   Change  : Updated process test to not distinguish between process
 *             types OS_ILLEGAL and OS_ZOOMBIE.
 *
 *   Revised : 2015-03-27 Ravineet Singh
 *   Change  : Removed check for LITS_ITC. Removed trailing whitespaces.
 *
 *   Revised : 2014-10-03 Ravineet Singh
 *   Change  : Made testcase buildable for linx.
 *
 *   Revised : 2014-08-27 Stanislav Vovk
 *   Change  : Added a test for a race while adding tcb.
 *
 *   Revised : 2014-06-08 Stanislav Vovk
 *   Change  : Added test of getting pcb of killed process and
 *             non-existing process
 *
 *   Revised : 2014-02-06 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed compiler warnings.
 *
 *   Revised : 2013-12-09 Stanislav Vovk
 *   Change  : Added test for static2real pid conversion
 *
 *   Revised : 2013-12-02 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test of background processes.
 *
 *   Revised : 2013-11-29 Marcus Ahlberg
 *   Change  : Test of static pids is only enabled when using ITC.
 *
 *   Revised : 2013-11-14 Stanislav Vovk
 *   Change  : Added test for process with static pid
 *
 *   Revised : 2012-02-14 Lars Jönsson EAB/FJP/TB
 *   Change  : Added check of process type.
 *
 *   Revised : 2012-01-26 Lars Jönsson EAB/FJP/TB
 *   Change  : Added more test cases for phantom processes.
 *
 *   Revised : 2012-01-09 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test of static phantom processes.
 *
 *   Revised : 2011-12-19 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test of phantom processes.
 *
 *   Revised : 2011-10-28 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test of sending signals to a killed process.
 *
 *   Revised : 2011-03-31 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <unistd.h>
#include <ose.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lits_extended.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define PROC_NO 64

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef  struct error_info_t_
{
    OSBOOLEAN  user_called;
    OSERRCODE  ecode;
    OSERRCODE  extra;

} error_info_t;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
extern PROCESS Worker1_;
extern PROCESS Worker2_;
extern PROCESS real_proxy_;
extern PROCESS real_proxy_default_redir_;
extern PROCESS no_proxy_;
extern PROCESS test_;

union SIGNAL
{
    SIGSELECT   sig_no;
    PROCESS     pid;
};

/*
**  Storage of errorhandler info
*/
static error_info_t  einfo;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Temporary block error handler that is used during test of error
 *   handling.
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
 *                     einfo
 */
/* ===================================================================== */
static OSADDRESS
block_error_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra)
{
    einfo.user_called = user_called;
    einfo.ecode       = ecode;
    einfo.extra       = extra;

    return 1;
}

/** ==================================================================== */
/**
 *   Checks the result of error handling
 *
 *   @param str        String to precede the result string
 *   @param user_called
 *                     Expected value of user_called
 *   @param ecode      Expected error code
 *   @param extra      Expected extra information
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     einfo
 *
 *   The einfo struct is reset after check.
 */
/* ===================================================================== */
static int
check_error_handling(char *str, OSBOOLEAN user_called, OSERRCODE ecode,
                     OSERRCODE extra)
{
    int  passed = 1;

    passed &= user_called == einfo.user_called;
    passed &= ecode == einfo.ecode;
    passed &= extra == einfo.extra;

    printf("%s [ecode=0x%lx, extra=%lu]: %s\n",
           str, einfo.ecode, einfo.extra, passed ? "OK" : "Fail");

    memset(&einfo, 0, sizeof(einfo));

    return passed;
}

/** ==================================================================== */
/**
 *   Test error handling of processes
 *
 *   @param            -
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 *
 *   Test handling of non-existing processes (low numbers are never valid
 *   Process IDs). Start a already running process (current process).
 */
/* ===================================================================== */
static int
test_error_handling(void)
{
    OSERRH*  old;
    OSERRH*  tmp;
    int      passed = 1;
    int      res;

    /* Ensure that einfo is reset */
    memset(&einfo, 0, sizeof(einfo));

    /* Add a block error handler for error checking */
    old = create_error_handler(get_bid(current_process()),
                               block_error_handler, 200);

    /* Set error handler for a non-existing process */
    tmp = create_error_handler(1, block_error_handler, 200);
    res = check_error_handling("Set error handler for a non-existing process",
                               0, 0x32, 1);

    passed &= res;

    /* Restore the old process error handler, if the change did not fail */
    if ( res == 0 )
        create_error_handler(1, tmp, 200);

    /* Stop a non-existing process */
    stop(2);
    passed &= check_error_handling("Starting a non-existing process",
                                   0, 0x32, 2);

    /* Start a non-existing process */
    start(3);
    passed &= check_error_handling("Stopping a non-existing process",
                                   0, 0x32, 3);

    /* Start current process, which is erronous */
    start(current_process());
    passed &= check_error_handling("Starting a running process",
                                   0, 0x5b, current_process());

    /* Restore the original block error handler */
    create_error_handler(get_bid(current_process()), old, 200);

    return passed;
}

/** ==================================================================== */
/**
 *   Prints the specified process type
 *
 *   @param type       Process type
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
print_ptype(enum PROCESS_TYPE type)
{
    switch( type )
    {
        case OS_BLOCK:
                printf("OS_BLOCK");
            break;

        case OS_PRI_PROC:
            printf("OS_PRI_PROC");
            break;

        case OS_BG_PROC:
            printf("OS_BG_PROC");
            break;

        case OS_INT_PROC:
            printf("OS_INT_PROC");
            break;

        case OS_TI_PROC:
            printf("OS_TI_PROC");
            break;

        case OS_PHANTOM:
            printf("OS_PHANTOM");
            break;

        case OS_ZOOMBIE:
            printf("OS_ZOOMBIE");
            break;

        case OS_ILLEGAL:
            printf("OS_ILLEGAL");
            break;

        default:
            printf("(unknown)");
    }
}

/** ==================================================================== */
/**
 *   Checks the result of a process handling action and prints the result
 *
 *   @param str        String to precede the result string
 *   @param pid        Process ID of handled process
 *   @param spid       I non-zero, the sending process should have this
 *                     process ID
 *   @param signo      Signal number of signal to send
 *   @param num_send   Number of signals to send
 *   @param num_recv   Expected number of signals to be received
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result(char *str, PROCESS pid, PROCESS spid,
             SIGSELECT signo, int num_send, int num_recv)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    int           passed = 1;
    int           count = 0;

    while ( num_send-- > 0 )
    {
        sig = alloc(sizeof(union SIGNAL), signo);
        send(&sig, pid);
    }

    while ( (sig = receive_w_tmo(100, selAll)) != NIL )
    {
        if ( spid )
            passed = passed && (spid == sender(&sig));

        free_buf(&sig);
        count++;
    }

    passed = passed && (count == num_recv);

    printf("%s [pid=%u]: %s\n", str, pid, passed ? "OK" : "Fail");

    return passed;
}

/** ==================================================================== */
/**
 *   Check if the environment variable is set.
 *
 *   @param str          String to precede the result string
 *   @param expetced_env Expected environment variabl
 *   @param do_exist     1 if the variable shall exist, otherwise 0
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_environment(char *str, char *expected_env, int do_exist)
{
    char               *env =NULL;
    int                passed = 1;

    env = get_env(get_bid(current_process()), expected_env);

    if (do_exist)
       passed = passed && (env != NULL);
    else
       passed = passed && (env == NULL);

    printf("%s ", str);
    printf("%s", env);
    printf(" [expected=");
    printf("%s", expected_env);
    printf("]: %s\n", passed ? "OK" : "Fail");

    return passed;
}

/** ==================================================================== */
/**
 *   Checks the process type and prints the result
 *
 *   @param str        String to precede the result string
 *   @param pid        Process ID of handled process
 *   @param type       Expected process type
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_ptype(char *str, PROCESS pid, enum PROCESS_TYPE type)
{
    enum PROCESS_TYPE  ptype;
    int                passed = 1;

    ptype = get_ptype(pid);

    passed = passed && (ptype == type);

    printf("%s ", str);
    print_ptype(ptype);
    printf(" [expected=");
    print_ptype(type);
    printf("]: %s\n", passed ? "OK" : "Fail");

    return passed;
}

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
 */
/* ===================================================================== */
OS_PROCESS(worker)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    PROCESS       pid = current_process();

    printf(" Process 0x%08x (%u) is started\n", pid, pid);

    while(1)
    {
        sig = receive(selAll);
        send(&sig, sender(&sig));
    }
}

/** ==================================================================== */
/**
 *   Temporary parent process which creates another process then dies
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(parent)
{
    PROCESS pid = current_process(), child;
    char name[16] = {0};
    union SIGNAL  *sig;

    sprintf(name, "child-%d", pid);
    child = create_process(OS_PRI_PROC, name, worker, 2000, 16, 0, 0, 0, 0, 0);
    start(child);

    printf(" - Parent 0x%08x (%u) created a child 0x%08x (%u)\n",
           pid, pid, child, child);

    sig = alloc(sizeof(union SIGNAL), 123);
    sig->pid = child;
    send(&sig, test_);
    kill_proc(current_process());
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
    struct OS_redir_entry redir[3];
    PROCESS  pid;
    int      passed = 1, i;
    struct OS_pcb *pcb;
    union SIGNAL  *sig;
    SIGSELECT     selAll[] = {0};

    char *pname;

    /* Test of PROCESS and BLOCK variabels set in osemain.con */
    printf("Read block and process variables:\n");
    passed &= check_environment(" - Environment", "MAX_PROCS", 1);
    passed &= check_environment(" - Environment", "MAX_PROCS_XX", 0);
    passed &= check_environment(" - Environment", "OSE_LM_POOL_SIZE", 1);
    passed &= check_environment(" - Environment", "OSE_LM_SIGNAL_SIZES", 1);
    passed &= check_environment(" - Environment", "OSE_LM_SIGNAL", 0);

    delay(100); /* Just to avoid messed up printouts */

    /* Test error handling of process related system calls */
    passed &= test_error_handling();

    pid = Worker1_;

    printf("Static process:\n");
    passed &= check_ptype(" - Process type", pid, OS_PRI_PROC);

    /* Stop a running process */
    stop(pid);
    passed &= check_result(" - Stopping process", pid, 0, 0, 1, 0);

    /* Start the stopped process */
    start(pid);
    passed &= check_result(" - Starting process", pid, 0, 0, 0, 1);

    /* Create a new process */
    pid = create_process(OS_PRI_PROC, "Worker3", worker, 2000, 16, 0, 0, 0, 0, 0);
    passed &= check_result("Creating process", pid, 0, 0, 1, 0);
    passed &= check_ptype(" - Process type", pid, OS_PRI_PROC);

    /* Start the newly created process */
    start(pid);
    passed &= check_result(" - Starting process", pid, 0, 0, 0, 1);

    /* Kill the dynamic process */
    kill_proc(pid);
    while ( hunt("Worker3", 0, NULL, NULL) != 0 ) delay(1);
    passed &= check_result(" - Killing process", pid, 0, 0, 1, 0);

    /* Get pcb of killed proc */
    printf("Get pcb of killed process: ");
    pcb = get_pcb(pid);
    print_ptype(pcb->type);
    passed &= (pcb->type == OS_ZOOMBIE);
    if (pcb->type != OS_ZOOMBIE) printf(" -> Fail\n");
    else printf(" -> OK\n");
    free_buf((union SIGNAL **) &pcb);

    /* Get pcb of never-existing proc */
    printf("Get pcb of never-existing process: ");
    pcb = get_pcb(pid + 100);
    print_ptype(pcb->type);
    passed &= (pcb->type == OS_ILLEGAL ||
	       pcb->type == OS_ZOOMBIE);
    if (pcb->type != OS_ILLEGAL &&
	pcb->type != OS_ZOOMBIE) printf(" -> Fail\n");
    else printf(" -> OK\n");
    free_buf((union SIGNAL **) &pcb);

    pid = Worker2_;

    printf("Static background process:\n");
    passed &= check_ptype(" - Process type", pid, OS_BG_PROC);

    /* Stop a running process */
    stop(pid);
    passed &= check_result(" - Stopping process", pid, 0, 0, 1, 0);

    /* Stop the stopped process once more */
    stop(pid);
    passed &= check_result(" - Stopping process", pid, 0, 0, 1, 0);

    /* Start a stopped process, which has to be started once more */
    start(pid);
    passed &= check_result(" - Starting process", pid, 0, 0, 0, 0);

    /* Start a stopped process */
    start(pid);
    passed &= check_result(" - Starting process", pid, 0, 0, 0, 2);

    /* Create a background process */
    pid = create_process(OS_BG_PROC, "Worker4", worker, 2000, 0, 0, 0, 0, 0, 0);
    passed &= check_result("Creating background process", pid, 0, 0, 1, 0);
    passed &= check_ptype(" - Process type", pid, OS_BG_PROC);

    /* Start the newly created process */
    start(pid);
    passed &= check_result(" - Starting process", pid, 0, 0, 0, 1);

    /* Kill the dynamic process */
    kill_proc(pid);
    while ( hunt("Worker4", 0, NULL, NULL) != 0 ) delay(1);
    passed &= check_result(" - Killing background process", pid, 0, 0, 1, 0);

    /* Create a phantom process with only a default pid */
    printf("Phantom process with only a default pid:\n");
    redir[0].sig = 1;
    redir[0].pid = current_process();
    pid = create_process(OS_PHANTOM, "Phantom", NULL, 0, 0, 0, 0, redir, 0, 0);
    passed &= check_ptype(" - Process type", pid, OS_PHANTOM);
    passed &= check_result(" - Signal number 0 (default pid)",
                           pid, current_process(), 0, 1, 1);
    passed &= check_result(" - Signal number 1 (default pid)",
                           pid, current_process(), 1, 1, 1);
    passed &= check_result(" - Signal number 2 (default pid)",
                           pid, current_process(), 2, 1, 1);
    passed &= check_result(" - Signal number 3 (default pid)",
                           pid, current_process(), 3, 1, 1);

    /* Kill the phantom process */
    kill_proc(pid);
    while ( hunt("Phantom", 0, NULL, NULL) != 0 ) delay(1);
    passed &= check_result("Killing phantom process", pid, 0, 0, 1, 0);

    /* Create a phantom process with default pid */
    printf("Phantom process with default pid:\n");
    redir[0].sig = 3;
    redir[0].pid = current_process();
    redir[1].sig = 1;
    redir[1].pid = Worker1_;
    redir[2].sig = 2;
    redir[2].pid = Worker2_;
    pid = create_process(OS_PHANTOM, "Phantom", NULL, 0, 0, 0, 0, redir, 0, 0);
    passed &= check_ptype(" - Process type", pid, OS_PHANTOM);
    passed &= check_result(" - Signal number 0 (default pid)",
                           pid, current_process(), 0, 1, 1);
    passed &= check_result(" - Signal number 1 (Worker1)",
                           pid, Worker1_, 1, 1, 1);
    passed &= check_result(" - Signal number 2 (Worker2)",
                           pid, Worker2_, 2, 1, 1);
    passed &= check_result(" - Signal number 3 (default pid)",
                           pid, current_process(), 3, 1, 1);

    /* Kill the phantom process */
    kill_proc(pid);
    while ( hunt("Phantom", 0, NULL, NULL) != 0 ) delay(1);
    passed &= check_result("Killing phantom process", pid, 0, 0, 1, 0);

    /* Create a phantom process with no default pid */
    printf("Phantom process without default pid:\n");
    redir[0].sig = 3;
    redir[0].pid = 0;
    redir[1].sig = 1;
    redir[1].pid = Worker1_;
    redir[2].sig = 2;
    redir[2].pid = Worker2_;
    pid = create_process(OS_PHANTOM, "Phantom", NULL, 0, 0, 0, 0, redir, 0, 0);
    passed &= check_ptype(" - Process type", pid, OS_PHANTOM);
    passed &= check_result(" - Signal number 0 (no redirect)",
                           pid, current_process(), 0, 1, 0);
    passed &= check_result(" - Signal number 1 (Worker1)",
                           pid, Worker1_, 1, 1, 1);
    passed &= check_result(" - Signal number 2 (Worker2)",
                           pid, Worker2_, 2, 1, 1);
    passed &= check_result(" - Signal number 3 (no redirect)",
                           pid, current_process(), 3, 1, 0);

    /* Kill the phantom process */
    kill_proc(pid);
    while ( hunt("Phantom", 0, NULL, NULL) != 0 ) delay(1);
    passed &= check_result("Killing phantom process", pid, 0, 0, 1, 0);

    /* Test a static phantom process with default pid */
    printf("Static Phantom process with default pid:\n");
    pid = real_proxy_;
    passed &= check_ptype(" - Process type", pid, OS_PHANTOM);
    passed &= check_result(" - Signal number 0 (default pid)",
                           pid, current_process(), 0, 1, 1);
    passed &= check_result(" - Signal number 1 (Worker1)",
                           pid, Worker1_, 1, 1, 1);
    passed &= check_result(" - Signal number 2 (Worker2)",
                           pid, Worker2_, 2, 1, 1);
    passed &= check_result(" - Signal number 3 (default pid)",
                           pid, current_process(), 3, 1, 1);

    /* Test a static phantom process with "DEFAULT" default pid */
    printf("Static Phantom process with \"DEFAULT\" (i.e. no) default pid:\n");
    pid = real_proxy_default_redir_;
    passed &= check_ptype(" - Process type", pid, OS_PHANTOM);
    passed &= check_result(" - Signal number 0 (no redirect)",
                           pid, 0, 0, 1, 0);
    passed &= check_result(" - Signal number 1 (Worker1)",
                           pid, Worker1_, 1, 1, 1);
    passed &= check_result(" - Signal number 2 (Worker2)",
                           pid, Worker2_, 2, 1, 1);
    passed &= check_result(" - Signal number 3 (no redirect)",
                           pid, 0, 3, 1, 0);

    /* Test a static phantom process with no redirection table */
    printf("Static Phantom process with no redirection table:\n");
    pid = no_proxy_;
    passed &= check_ptype(" - Process type", pid, OS_PHANTOM);
    passed &= check_result(" - Signal number 0 (no redirect)",
                           pid, 0, 0, 1, 0);
    passed &= check_result(" - Signal number 1 (no redirect)",
                           pid, Worker1_, 1, 1, 0);
    passed &= check_result(" - Signal number 2 (no redirect)",
                           pid, Worker2_, 2, 1, 0);
    passed &= check_result(" - Signal number 3 (no redirect)",
                           pid, 0, 3, 1, 0);

    /* Create processes with user defined pid */
    printf("Create process with user defined (static) pid:\n");
    redir[0].sig = 0;
    redir[0].pid = 1;
    redir[1].sig = PROC_ATTR_PID;
    redir[1].pid = 11;
    pname = "pri_stat_proc";
    pid = create_process(OS_PRI_PROC, pname, worker,
                         10000, 16, 0, 0, &redir[0], 0, 0);
    if (pid != redir[1].pid) passed &= 0;
    printf(" - Returned pid: 0x%x; static pid: 0x%x\n", pid, redir[1].pid);

    attach(NULL, redir[1].pid);
    start(redir[1].pid);

    pcb = get_pcb(redir[1].pid);
    if (strcmp((char *)&pcb->strings[pcb->name], pname) != 0) passed &= 0;
    printf(" - Expected process name: %s\n", pname);
    printf(" - Fetched process name using static pid %d: %s\n",
           redir[1].pid, (char *)&pcb->strings[pcb->name]);

    printf(" - Send a signal to pid=%d: %s\n", pid, pname);
    sig = alloc(sizeof(union SIGNAL), 123);
    send(&sig, pid);
    if ( (sig = receive_w_tmo(100, selAll)) == NIL)
    {
        printf(" - No signal received back from pid 0x%x\n", pid);
        passed &= 0;
    }
    else
    {
        if (sender(&sig) != lits_get_real_pid(redir[1].pid)) passed &= 0;
        printf(" - Expected real pid: 0x%x; converted pid: 0x%x\n",
               sender(&sig), lits_get_real_pid(redir[1].pid));
        free_buf(&sig);
    }

    printf(" - Kill process using pid=%d: %s\n", redir[1].pid, pname);
    kill_proc(redir[1].pid);
    if ( (sig = receive_w_tmo(100, selAll)) == NIL)
    {
        printf(" - Dynamic process is not terminated now (error), %s(%d)\n",
               pname, redir[1].pid);
        passed &= 0;
    }
    if (sig->sig_no == OS_ATTACH_SIG)
    {
        printf(" - Dynamic process is terminated, %s(%d)\n", pname, redir[1].pid);
    }
    else
        passed &= 0;

    /* This step tests a following race. When a dynamic process creates another
     * dynamic process and then the creator terminates there is a race while
     * adding/removing tcb. If test step fails the test will crash therefore no result
     * is updated.
     */
    printf("Create %d processes from another temporary process\n", PROC_NO);
    PROCESS procs2kill[PROC_NO];
    for (i = 0; i < PROC_NO; i++)
    {
        start(create_process(OS_PRI_PROC, "parent", parent, 2000, 16, 0, 0, 0, 0, 0));
        sig = receive(selAll);
        procs2kill[i] = sig->pid;
        free_buf(&sig);
    }
    for (i = 0; i < PROC_NO; i++)
        kill_proc(procs2kill[i]);

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");
    exit(passed ? 0 : -1);
}

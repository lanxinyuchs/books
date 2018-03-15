/**
 *   Test of hunt functionality.
 *
 *   @file
 *
 *   This file is a part of the test programs for the lits (Legacy IPC and
 *   Task Support) lib.
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
 *   Revised : 2015-03-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed LINX impl.
 *
 *   Revised : 2014-02-10 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed a warning.
 *
 *   Revised : 2014-01-10 Lars Jönsson EAB/FJP/TB
 *   Change  : Improved test of processes in other programs.
 *
 *   Revised : 2012-02-14 Lars Jönsson EAB/FJP/TB
 *   Change  : Added check of process type.
 *
 *   Revised : 2012-02-09 Lars Jönsson EAB/FJP/TB
 *   Change  : Now also checks that all hunt helper processes, which are
 *             just Linx endpoints, are removed when the tests are
 *             completed.
 *
 *   Revised : 2011-11-03 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test of hunt with signal on a remote process.
 *
 *   Revised : 2011-08-11 Lars Jönsson EAB/FJP/TB
 *   Change  : Minor changes to make the hunt test work with automake.
 *
 *   Revised : 2011-04-19 Lars Jönsson EAB/FJP/TB
 *   Change  : Restricted execution to one core, which seems to fix a
 *             problem with fork() that sometimes just hangs.
 *
 *   Revised : 2011-04-04 Lars Jönsson EAB/FJP/TB
 *   Change  : Automated the tests.
 *
 *   Revised : 2011-02-08 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <sched.h>
#include <ose.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <libgen.h>
#include <signal.h>

#include <lits_internal.h>
#include <sys/ioctl.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define HUNT_DELAY      (1000)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
#define	HUNT_SIG	1

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
 *   Server process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(server_proc)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    PROCESS       pid;

    pid = current_process();

    while (1)
    {
        sig = receive(selAll);
        pid = sender(&sig);
        send(&sig, pid);
    }
}

/** ==================================================================== */
/**
 *   Starts a local server process with the specified name
 *
 *   @param name       Process name
 *
 *   @return           Process ID if OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static PROCESS
start_local_process(char *name)
{
    PROCESS pid;

    pid = create_process(OS_PRI_PROC, name, server_proc,
                         2000, 15, (OSTIME)0, 0,0, 0, 0);

    if ( pid == 0 )
        return 0;

    attach(NULL, pid);
    delay(50);
    start(pid);

    return pid;
}

/** ==================================================================== */
/**
 *   Kills a process
 *
 *   @param pid        Process ID of process to kill
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
kill_local_process(PROCESS pid)
{
    union SIGNAL  *sig;
    SIGSELECT     sel_attach[] = {1, OS_ATTACH_SIG};

    kill_proc(pid);

    sig = receive(sel_attach);

    while ( pid != sender(&sig) )
    {
        free_buf(&sig);
        sig = receive(sel_attach);
    }

    free_buf(&sig);

    return 0;
}

/** ==================================================================== */
/**
 *   Starts a program with the specified name
 *
 *   @param name       Program name, including an optional path
 *
 *   @return           Linux Process ID if OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static pid_t
start_prog(char *prog)
{
    pid_t  pid;

    if ( (pid = fork()) < 0 )
    {
        perror(basename(prog));
        return 0;
    }

    if ( pid == 0 )
    {
        /* Child */

        execl(prog, prog, NULL);

        /* exec() never returns */
    }

    /* Parent */

    return pid;
}

/** ==================================================================== */
/**
 *   Kills a Linux process (program)
 *
 *   @param pid        Linux Process ID of program to kill
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
kill_prog(pid_t pid)
{
    int res = kill(pid, SIGKILL);

    /* Needed to ensure that the process is killed */
    delay(50);

    return res;
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
 *   Get the pid of the named process
 *
 *   @param name       Process name including link handler path
 *
 *   @return           Process ID result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static PROCESS
get_pid(char *name)
{
    PROCESS pid;

    if ( hunt(name, 0, &pid, NULL) == 0 )
        pid = 0;

    return pid;
}

/** ==================================================================== */
/**
 *   Checks the result of hunt handling and prints the result
 *
 *   @param str        String to precede the result string
 *   @param name       Process name including link handler path
 *   @param expected   Expected Process ID of the process to hunt for.
 *                     Special values:
 *                       0 - Process is not expected to be found
 *                      -1 - Process ID should not be checked, but the
 *                           process is expected to be found
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 *
 *   Hunt for the specified process and checks is the pid is correct.
 */
/* ===================================================================== */
static int
check_result(char *str, char *name, PROCESS expected)
{
    PROCESS pid;
    int     huntresult;
    int     passed;

    huntresult = hunt(name, 0, &pid, NULL);

    printf("%s - hunting for \"%s\", ", str, name);

    if ( huntresult )
        printf("process %u (0x%08x) found", pid, pid);
    else
        printf("no process found");

    if ( expected == 0 )
        passed = !huntresult;
    else if ( expected == -1 )
        passed = huntresult;
    else
        passed = huntresult && (pid == expected);

    printf(": %s\n", passed ? "OK" : "Fail");

    return passed;
}

/** ==================================================================== */
/**
 *   Checks the result of hunt handling with signal and prints the result
 *
 *   @param str        String to precede the result string
 *   @param name       Process name including link handler path
 *   @param expected   Expected Process ID of the process to hunt for.
 *                     Special values:
 *                       0 - Process is not expected to be found
 *                      -1 - Process ID should not be checked, but the
 *                           process is expected to be found
 *   @param verify     Do not hunt, just verify that a signal is recevied
 *                     from a previous hunt call
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 *
 *   Hunt for the specified process and checks is the pid is correct.
 */
/* ===================================================================== */
static int
check_hunt_w_sig(char *str, char *name, PROCESS expected, int verify)
{
    SIGSELECT     sel_hunt[] = {1, HUNT_SIG};
    union SIGNAL  *sig;
    PROCESS       pid;
    int           huntresult;
    int           passed;

    printf("%s - ", str);

    if ( !verify )
    {
        sig = alloc(sizeof(union SIGNAL), HUNT_SIG);
        hunt(name, 0, NULL, &sig);
        printf("hunting for \"%s\", ", name);
    }

    sig = receive_w_tmo(0, sel_hunt);
    huntresult = sig != NULL;
    pid = sig ? sender(&sig) : 0;
    if ( sig ) free_buf(&sig);

    if ( huntresult )
        printf("signal from process %u (0x%08x) received", pid, pid);
    else
        printf("no signal received");

    if ( expected == 0 )
        passed = !huntresult;
    else if ( expected == -1 )
        passed = huntresult;
    else
        passed = huntresult && (pid == expected);

    printf(": %s\n", passed ? "OK" : "Fail");

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
 *   Main entry point
 *
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 *
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int
main(int argc, char **argv)
{
    PROCESS pid;
    char    server[256];
    pid_t   prog_id;
    int     passed = 1;

    /*
    **  Make this process and all children run on the same core. This seems
    **  to fix a problem with fork(), which sometimes just hangs.
    */
    passed &= set_core(sched_getcpu());

    if ( argc > 1 )
        sprintf(server, "%s", argv[1]);
    else
        sprintf(server, "%s/%s", dirname(argv[0]), "remote-server");

    /*
    **  Hunt for a process in this program (local process)
    */
    printf("Hunt for a process in this program (local process)\n");
    pid = start_local_process("server");
    passed &= check_result("- Verify started process", "server", pid);
    passed &= check_ptype("- Verify process type", pid, OS_PRI_PROC);

    /*
    **  Kill process and verify it by using hunt
    */
    kill_local_process(pid);
    passed &= check_result("- Verify killed process", "server", 0);

    /*
    **  Hunt for a local process, using a signal
    */
    printf("Hunt for a local process, using a signal\n");
    passed &= check_hunt_w_sig("- Verify non-existing process", "sig-server", 0, 0);
    pid = start_local_process("sig-server");
    passed &= check_hunt_w_sig("- Verify started process", "sig-server", pid, 1);
    passed &= check_ptype("- Verify process type", pid, OS_PRI_PROC);

    /*
    **  Kill process and verify it by using hunt with signal
    */
    kill_local_process(pid);
    passed &= check_hunt_w_sig("- Verify killed process", "sig-server", 0, 0);

    /*
    **  Hunt for a process in another program (remote process)
    */
    printf("Hunt for a process in another program (remote process)\n");
    prog_id = start_prog(server);
    delay(HUNT_DELAY);
    passed &= check_result("- Verify started process", "server", -1);
    passed &= check_ptype("- Verify process type", get_pid("server"), OS_PRI_PROC);

    /*
    **  Kill processes and verify it by using hunt
    */
    kill_prog(prog_id);
    passed &= check_result("- Verify killed process", "server", 0);

    /*
    **  Hunt for a process in another program (remote process),
    **  using a signal
    */
    printf("Hunt for a process in another program (remote process),"
           " using a signal\n");
    passed &= check_hunt_w_sig("- Verify non-existing process", "server", 0, 0);
    prog_id = start_prog(server);
    delay(HUNT_DELAY);
    passed &= check_hunt_w_sig("- Verify started process", "server", -1, 1);
    passed &= check_ptype("- Verify process type", get_pid("server"), OS_PRI_PROC);

    /*
    **  Kill processes and verify it by using hunt
    */
    kill_prog(prog_id);
    passed &= check_hunt_w_sig("- Verify killed process", "server", 0, 0);

    /*
    **  Test that the local server process is found by hunt, even if a remote
    **  server process is available.
    **
    **  First start the remote server and then the local server
    */
    printf("Test that a block local process is found\n");
    printf("- Start order: remote server process, local server process\n");
    prog_id = start_prog(server);
    delay(HUNT_DELAY);
    passed &= check_result("- Verify started remote process", "server", -1);
    pid = start_local_process("server");
    passed &= check_result("- Verify hunt on local process", "server", pid);

    /*
    **  Kill processes and verify it by using hunt
    */
    kill_prog(prog_id);
    kill_local_process(pid);
    passed &= check_result("- Verify killed processes", "server", 0);

    /*
    **  Test that the local server process is found by hunt, even if a remote
    **  server process is available.
    **
    **  First start the local server and then the remote server
    */
    printf("Test that a block local process is found\n");
    printf("- Start order: local server process, remote server process\n");
    pid = start_local_process("server");
    passed &= check_result("- Verify started local process", "server", pid);
    prog_id = start_prog(server);
    delay(HUNT_DELAY);
    passed &= check_result("- Verify hunt on local process", "server", pid);

    /*
    **  Kill processes and verify it by using hunt
    */
    kill_prog(prog_id);
    kill_local_process(pid);
    passed &= check_result("- Verify killed processes", "server", 0);

    /*
    **  Check that all hunt helper endpoints closed
    */
    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);

    return 0;
}

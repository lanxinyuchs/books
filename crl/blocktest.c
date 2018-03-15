/**
 *   Test of block handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2011-2012 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2011-04-01 Lars Jönsson EAB/FJP/TB
 *   Change  : Added check of ptype of the block.
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
#define	SIG_OFFSET	1000

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
/*
**  Use sig_no - SIG_OFFSET as index into pids[] and bids[]
*/
union SIGNAL
{
    SIGSELECT   sig_no;
};

static PROCESS  *pids;
static PROCESS  *bids;


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
 *                     pids[], bids[]
 *
 *   Updates updates pids[] and bids[] with the pid resp. bid of the
 *   current procees. A signal is used for synchronization.
 */
/* ===================================================================== */
OS_PROCESS(test_proc)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;

    sig = receive(selAll);
    pids[sig->sig_no - SIG_OFFSET] = current_process();
    bids[sig->sig_no - SIG_OFFSET] = get_bid(current_process());
    send(&sig, sender(&sig));

    stop(current_process());
}


/** ==================================================================== */
/**
 *   Prints name and bid of the specified block.
 *
 *   @param bid        Block ID
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
print_block(PROCESS bid)
{
    struct OS_pcb *pid_info;
    char          *name = "(unknown process)";

    pid_info = zzget_pcb(bid);
    name     = &pid_info->strings[pid_info->name];
    printf("Block process  \"%s\" [bid=%u]\n", name, bid);
    free_buf((union SIGNAL **)&pid_info);
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
 *   Checks the result of a pid and bid reading, and prints the result
 *
 *   @param str        String to precede the result string
 *   @param pid        Process ID of handling process
 *   @param bid        Block ID of handling process
 *   @param expected   Expected Block ID of handling process
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result(char *str, PROCESS pid, PROCESS bid, PROCESS expected, char *nm)
{
    struct OS_pcb *pid_info = NULL;
    char          *name = "(unknown process)";
    int           passed;

    passed = (bid == expected) && (pid != expected);

    if ( pid )
    {
        pid_info = zzget_pcb(pid);
        name     = &pid_info->strings[pid_info->name];
        if (strcmp(nm, name) != 0) passed = 0;
    }

    printf("%s \"%s\" [pid=%u, bid=%u]: %s\n",
           str, name, pid, bid, passed ? "OK" : "Fail");

    if ( pid_info )
        free_buf((union SIGNAL **)&pid_info);

    return passed;
}

/** ==================================================================== */
/**
 *   Construct process name
 *
 *   @param name       String with initial proc name
 *   @param i          loop counter
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void proc_name(char *name, int i)
{
    name[strlen(name)-1] = (char)('0' + (i % 10));
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
 *                     pids[], bids[]
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    union SIGNAL  *sig;
    SIGSELECT     selAll[] = {0};
    PROCESS       pid;
    PROCESS       bid;
    int           i;
    char          name[] = "test_proc-0";
    int           passed = 1;

    bids = malloc(NUM_PROCS * sizeof(PROCESS));
    pids = malloc(NUM_PROCS * sizeof(PROCESS));

    if ( (bids == 0) || (pids == 0) )
        passed = 0;

    if ( passed )
    {
        memset(bids, 0, NUM_PROCS * sizeof(PROCESS));
        memset(pids, 0, NUM_PROCS * sizeof(PROCESS));

        /*
        **  Start all dynamic processes
        */
        for (i = 0; i < NUM_PROCS; i++)
        {
            proc_name(name, i);
            pid = create_process(OS_PRI_PROC, name, test_proc, 2000, 15, (OSTIME)0, 0,0, 0,
                                 0);
            start(pid);
            sig = alloc(sizeof(union SIGNAL), i + SIG_OFFSET); /* sig_no is used as index */
            send(&sig, pid);
        }

        /*
        **  Wait for all processes to set the data
        */
        for (i = 0; i < NUM_PROCS; i++)
        {
            if ( (sig = receive_w_tmo(100, selAll)) == NIL )
            {
                printf("One of more processes failed to set the data within the time limits\n");
                passed = 0;
                break;
            }

            free_buf(&sig);
        }

        bid = get_bid(current_process());
        print_block(bid);
        passed &= check_ptype(" - Process type", bid, OS_BLOCK);

        /*
        **  Check all results
        */
        for (i = 0; i < NUM_PROCS; i++)
        {
            proc_name(name, i);
            passed &= check_result("Child process", pids[i], bids[i], bid, name);
        }
    }

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

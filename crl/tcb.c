/**
 *   Routines for handling search trees for the task control blocks.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
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
 *   Revised : 2014-10-08 Henrik Wallin
 *   Change  : get_pcb: Make sure to also dereference the pointer
 *             returned from tfind inside the protected section.
 *             The binary tree can be changed by other threads and the
 *             pointer might not point to our tcb pointer any more.
 *
 *   Revised : 2014-08-27 Stanislav Vovk
 *   Change  : add_tcb: Protect while in add_tcb. There seem to be a
 *             race when removing a tcb and evaluating a newly added
 *             tcb.
 *
 *   Revised : 2014-06-03 Ravineet Singh EAB/FJP/HB
 *   Change  : Pid is now weighed in compare/insert_tcb_by_name.
 *             add/remove_tcb apply on both trees before returning.
 *
 *   Revised : 2014-04-11 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed unused func, delete_tcb
 *
 *   Revised : 2014-03-18 Ravineet Singh EAB/FJP/HB
 *   Change  : Added block of THREAD_STOP_SIGNAL while operating in tcb tree
 *
 *   Revised : 2013-08-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Added inclusion of stdlib.h
 *
 *   Revised : 2012-05-07 Lars Jönsson EAB/FJP/TB
 *   Change  : Added remove_tcb().
 *
 *   Revised : 2012-02-20 Lars Jönsson EAB/FJP/TB
 *   Change  : Made get_tcb_by_name() more robust by using const argument.
 *
 *   Revised : 2011-03-31 Lars Jönsson EAB/FJP/TE
 *   Change  : Corrected get_tcb() for non-existing pids.
 *
 *   Revised : 2011-03-14 Lars Jönsson EAB/FJP/TE
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <search.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <lits_internal.h>

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
/*
**  Mutex for protecting critical regions when modifying the
**  TCB tables
*/
static pthread_mutex_t tcb_mutex = PTHREAD_MUTEX_INITIALIZER;

static void *tcb_tab = NULL;
static void *tcb_name_tab = NULL;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Set or unset signal blocker
 *   We cannot handle an interruption of THREAD_STOP_SIGNAL, disable it.
 *
 *   @param how        SIG_BLOCK|SIG_UNBLOCK
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 */
/* ===================================================================== */
static void inline __attribute__((always_inline))
toggle_stop_signal(int how)
{
    sigset_t mask;
    int ret;

    sigemptyset (&mask);
    sigaddset (&mask, THREAD_STOP_SIGNAL);

    ret = sigprocmask(how, &mask, NULL);
    assert(0 == ret);
}

/** ==================================================================== */
/**
 *   Protect tcb tree , disable THREAD_STOP_SIGNAL and take mutex.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     tcb_mutex
 *
 */
/* ===================================================================== */
static void inline __attribute__((always_inline))
protect_tcb(void)
{
    toggle_stop_signal(SIG_BLOCK);
    pthread_mutex_lock(&tcb_mutex);
}

/** ==================================================================== */
/**
 *   Release tcb tree , release mutex and enable THREAD_STOP_SIGNAL
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     tcb_mutex
 *
 */
/* ===================================================================== */
static void inline __attribute__((always_inline))
unprotect_tcb(void)
{
    pthread_mutex_unlock(&tcb_mutex);
    toggle_stop_signal(SIG_UNBLOCK);
}

/** ==================================================================== */
/**
 *   Compare algorithm for binary tree search.
 *
 *   @param pa         Element A
 *   @param pb         Element B
 *
 *   @return           -1 if A < B, 0 if A = B, 1 A > B
 *
 *   @par Globals:
 *                     --
 *
 *   This algorthm compares the pid of processes/threads.
 */
/* ===================================================================== */
static int
compare_tcb(const void *pa, const void *pb)
{
    const tcb_t  *tcb1 = pa;
    const tcb_t  *tcb2 = pb;

    if ( tcb1->pid < tcb2->pid )
        return -1;

    if ( tcb1->pid > tcb2->pid )
        return 1;

    return 0;
}

/** ==================================================================== */
/**
 *   Compare algorithm for binary tree search.
 *
 *   @param pa         Element A
 *   @param pb         Element B
 *
 *   @return           -1 if A < B, 0 if A = B, 1 A > B
 *
 *   @par Globals:
 *                     --
 *
 *   This algorthm compares the name of processes/threads.
 */
/* ===================================================================== */
static int
compare_tcb_by_name(const void *pa, const void *pb)
{
    const tcb_t  *tcb1 = pa;
    const tcb_t  *tcb2 = pb;
    int res = strcmp(tcb1->name, tcb2->name);

    if (0 == tcb1->pid || 0 != res)
        return res;

    if (tcb1->pid < tcb2->pid)
        return -1;

    if (tcb1->pid > tcb2->pid)
        return 1;

    return 0;
}

/** ==================================================================== */
/**
 *   Compare algorithm for binary tree search, name and pid weighed.
 *
 *   @param pa         Element A
 *   @param pb         Element B
 *
 *   @return           -1 if A < B, 1 A > B
 *
 *   @par Globals:
 *                     --
 *
 *   This algorthm compares the name and pid of processes/threads and is
 *   used when inserting elements into the name based tree.
 */
/* ===================================================================== */
static int
insert_tcb_by_name(const void *pa, const void *pb)
{
    const tcb_t  *tcb1 = pa;
    const tcb_t  *tcb2 = pb;
    int          res;

    assert(tcb1->pid != tcb2->pid);
    res = strcmp(tcb1->name, tcb2->name);

    if (res == 0)
    {
        if (tcb1->pid < tcb2->pid)
            res = -1;

        if (tcb1->pid > tcb2->pid)
            res = 1;
    }

    return res;
}


/** ==================================================================== */
/**
 *   Dummy for removing the root of a binary tree.
 *
 *   @param nodep      Pointer to the element
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   This is used when removing the process/thread named base binary
 *   tree, where the element is just a pointe to the elements of the
 *   pid based tree.
 */
/* ===================================================================== */
static void
free_name_node(void *nodep)
{
    (void)nodep;
}

/** ==================================================================== */
/**
 *   Destroys the pid and name based binary trees.
 *
 *   @param            -
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     tcb_name_tab, tcb_tab
 */
/* ===================================================================== */
int
destroy_tcb_tab(void)
{
    tdestroy(tcb_name_tab, free_name_node);
    tdestroy(tcb_tab, free);

    return 0;
}

/** ==================================================================== */
/**
 *   Adds a new process/thread to the pid and named based binary trees.
 *
 *   @param tcb        Pointer to the new element
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     tcb_name_tab, tcb_tab
 */
/* ===================================================================== */
int
add_tcb(tcb_t *tcb)
{
    tcb_t  **res_n;
    tcb_t  **res;
    int ret = 0;

    protect_tcb();
    res =   tsearch(tcb, &tcb_tab, compare_tcb);
    res_n = tsearch(tcb, &tcb_name_tab, insert_tcb_by_name);

    if ( (res_n == 0) || (*res_n != tcb) )
    {
        ret = -1;
    }
    else if ( (res == 0) || (*res != tcb) )
    {
        ret = -2;
    }

    unprotect_tcb();
    return ret;
}

/** ==================================================================== */
/**
 *   Searches for a process/thread with the specified pid.
 *
 *   @param pid        Process ID
 *
 *   @return           Pointer to the TCB or NULL is not found
 *
 *   @par Globals:
 *                     tcb_tab
 */
/* ===================================================================== */
tcb_t *
get_tcb(PROCESS pid)
{
    tcb_t  tcb;
    tcb_t  **res;
    tcb_t  *ret;

    tcb.pid = pid;

    protect_tcb();
    res = tfind(&tcb, &tcb_tab, compare_tcb);
    ret = res ? *res : NULL;
    unprotect_tcb();

    return ret;
}

/** ==================================================================== */
/**
 *   Searches for a process/thread with the specified name.
 *
 *   @param name       Process name
 *
 *   @return           Pointer to the TCB or 0 is not found
 *
 *   @par Globals:
 *                     tcb_tab
 */
/* ===================================================================== */
tcb_t *
get_tcb_by_name(const char *name)
{
    tcb_t  tcb;
    tcb_t  **res;
    tcb_t  *ret;

    tcb.pid = 0;
    strncpy(tcb.name, name, MAX_PROC_NAME_LEN);
    tcb.name[MAX_PROC_NAME_LEN] = '\0';

    protect_tcb();
    res = tfind(&tcb, &tcb_name_tab, compare_tcb_by_name);
    ret = res ? *res : NULL;
    unprotect_tcb();

    return ret;
}

/** ==================================================================== */
/**
 *   Removes a process/thread from the pid and named based binary trees.
 *
 *   @param tcb        Pointer to the element to be removed
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     tcb_name_tab, tcb_tab
 */
/* ===================================================================== */
int
remove_tcb(tcb_t *tcb)
{
    tcb_t  **res_n;
    tcb_t  **res;

    protect_tcb();
    res_n = tdelete(tcb, &tcb_name_tab, compare_tcb_by_name);
    res =   tdelete(tcb, &tcb_tab, compare_tcb);
    unprotect_tcb();

    if ( res_n == 0 )
        return -1;

    if ( res == 0 )
        return -2;

    return 0;
}

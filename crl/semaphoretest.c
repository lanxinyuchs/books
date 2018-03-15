/**
 *   Test of semaphore handling.
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
 *   Revised : 2013-10-22 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <ose.h>
#include <stdio.h>
#include <stdlib.h>

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
extern PROCESS Worker1_;
extern PROCESS Worker2_;

typedef struct
{
    SIGSELECT   sig_no;
    OSSEMVAL    val;  /* :Output*/
} SemVal;

union SIGNAL
{
    SIGSELECT sig_no;
    SemVal    val;
};


/** ==================================================================== */
/**
 *   Checks the result of semaphore handling and prints the result
 *
 *   @param str          String to precede the result string
 *   @param pid          Process ID of worker process
 *   @param tmo          receive tmo
 *   @param expected_val Expexted value of semaphore
 *   @param sig_no      Signal number of signal to receive
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result(char *str, PROCESS pid, OSTIME tmo,
             OSSEMVAL expected_val, int sig_no)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    int           passed = 1;
    OSSEMVAL      val = 0;

    printf("%s - ", str);
    sig = receive_from(tmo, selAll, pid);

    if ( sig_no )
    {
        SemVal *s = (SemVal*)sig;

        printf(" [expected:sig_no=%u, val=%ld]", sig_no, expected_val);

        passed = (sig != NIL) && (sig->sig_no == sig_no);
        passed = passed && (sig != NIL && expected_val == s->val);

        if ( sig != NIL )
        {
            val = s->val;
            printf("signal number=%u, val=%ld", sig->sig_no, val);
            free_buf(&sig);
        }
        else
        {
            printf("No signal received");
        }
    }
    else
    {
        printf(" [expected no signal]");
        passed = (sig == NIL);
        if(sig)
        {
            printf("signal number=%u, val=%ld", sig->sig_no, val);
            free_buf(&sig);
        }
        else
        {
            printf("No signal received");
        }
    }

    printf(": %s\n", passed ? "OK" : "Fail");

    return passed;
}

static SEMAPHORE * r_sem;
static void
get_resource_val(OSSEMVAL* val, int wait)
{

    assert(r_sem != NULL);
    if(wait)
    {
        wait_sem(r_sem);
    }
    *val = get_sem(r_sem);
}


/** ==================================================================== */
/**
 *   Worker process, wait for sem. return value of sem in reply
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

    printf("Process 0x%08x (%u) is started\n", pid, pid);

    while(1)
    {
        SemVal *s;
        sig = receive(selAll);
        s = (SemVal*)sig;
        get_resource_val(&s->val, 1);
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
 *                     --
 *
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    union SIGNAL *sig;
    OSSEMVAL val;
    int  passed = 1;

    delay(100); /* Just to avoid messed up printouts */
    r_sem =  create_sem(0);


    /* Check sem value */
    get_resource_val(&val, 0);
    passed &= (0 == val);

    /* Worker 1, should hang. */
    sig = alloc(sizeof(SemVal), (SIGSELECT)Worker1_);
    send(&sig, Worker1_);
    passed &= check_result(" - Worker1 get sem",  Worker1_, 10, 0, 0);

    /* Check sem value */
    get_resource_val(&val, 0);
    passed &= (0 == val);

    /* Worker 2, should hang. */
    sig = alloc(sizeof(SemVal), (SIGSELECT)Worker2_);
    send(&sig, Worker2_);
    passed &= check_result(" - Worker2 get sem",  Worker2_, 10, 0, 0);

    /* Check sem value */
    get_resource_val(&val, 0);
    passed &= (0 == val);

    /* trigger */
    signal_sem(r_sem);

    /* Worker 1, should get resource. */
    passed &= check_result(" - Worker1 get sem",  Worker1_, 1, 0,
                           (SIGSELECT)Worker1_);

    /* Check sem value */
    get_resource_val(&val, 0);
    passed &= (0 == val);

    /* trigger */
    signal_sem(r_sem);

    /* Worker 2, should get resource. */
    passed &= check_result(" - Worker2 get sem",  Worker2_, 1, 0,
                           (SIGSELECT)Worker2_);

    /* Check sem value */
    get_resource_val(&val, 0);
    passed &= (0 == val);

    /* trigger x 2 */
    signal_sem(r_sem);
    signal_sem(r_sem);
    get_resource_val(&val, 0);
    passed &= (2 == val);

    /* Worker 1, should return at once. */
    sig = alloc(sizeof(SemVal), (SIGSELECT)Worker1_);
    send(&sig, Worker1_);
    passed &= check_result(" - Worker1 get sem",  Worker1_, 1, 1,
                           (SIGSELECT)Worker1_);

    get_resource_val(&val, 0);
    passed &= (1 == val);

    kill_sem(r_sem);

    exit((int)!passed);
}

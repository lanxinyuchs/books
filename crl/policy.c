/**
 *   Test of scheduler policies.
 *   Fixme: INT_PROC not tested.
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
 *   Revised : 2015-06-15 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <sched.h>
#include <stdio.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <time.h>

#include <ose.h>
#include <lits_internal.h>
#include <tcb.h>

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
OS_PROCESS(worker)
{
    while(1)
        delay(1);
}

/** ==================================================================== */
/**
 *   Get user desired sched policy fron env
 *
 *   @param policy     Default policy
 *
 *   @return           Possibly updated policy, via env.
 *
 *   @par Globals:
 *                     --
 */
/** ==================================================================== */
static int
policy_from_env(int policy)
{
    char *env;
    int ret;

    ret = policy;
    env = get_env(get_bid(current_process()), "LITS_SCHED_POLICY");
    if (env != NULL)
    {
        if (0 == strcmp("rr", env) || 0 == strcmp("RR", env))
        {
            ret = SCHED_RR;
        }
        else if (0 == strcmp("fifo", env) || 0 == strcmp("FIFO", env))
        {
            ret = SCHED_FIFO;
        }
        else if (0 == strcmp("other", env) || 0 == strcmp("OTHER", env))
        {
            ret = SCHED_OTHER;
        }
        free_buf((union SIGNAL **)&env);
    }

    return ret;
}

/** ==================================================================== */
/**
 *   Convert sched policy to string
 *   Leakes malloc mem.
 *
 *   @param policy      sched policy
 *
 *   @return            policy string or "Unknown"
 *
 *   @par Globals:
 *                     --
 */
/** ==================================================================== */
static char*
policy_str(int policy)
{
    char* ret = malloc(64);

    assert(NULL != ret);
    switch(policy)
    {
        case SCHED_FIFO:
            sprintf(ret, "%s", "SCHED_FIFO");
            break;
        case SCHED_RR:
            sprintf(ret, "%s", "SCHED_RR");
            break;
        case SCHED_OTHER:
            sprintf(ret, "%s", "SCHED_OTHER");
            break;
        default:
            sprintf(ret, "%s", "Unknown");
            break;
    }

    return ret;
}

/** ==================================================================== */
/**
 *   Check if running sched policy is the desired policy
 *
 *   @param pid        Process ID
 *
 *   @return            0: success
 *                     -1: effictive policy != desired policy
 *
 *   @par Globals:
 *                     --
 */
/** ==================================================================== */
static int
desired_policy_is_running_policy(PROCESS pid)
{
    tcb_t *tcb;
    int   rv;
    int   ret = 0;
    int   policy;
    int   policy_env;
    struct sched_param  param;

    tcb = get_tcb(pid);
    rv = pthread_getschedparam(tcb->tid, &policy, &param);
    assert(0 == rv);

#ifdef LITS_SCHED_FIFO
    policy_env = policy_from_env(SCHED_FIFO);
#elif defined LITS_SCHED_ROUND_ROBIN
    policy_env = policy_from_env(SCHED_RR);
#else
    policy_env = policy_from_env(SCHED_OTHER);
#endif


    if(policy_env == SCHED_FIFO || policy_env == SCHED_RR)
    {
        struct rlimit rlim;

        ret = getrlimit(RLIMIT_RTPRIO, &rlim);
        if ( ret != 0 || (rlim.rlim_cur < MAX_USED_RT_PRIO))
        {
            fprintf(stderr,
                    "\nWarning: User does not have rtprio permission, bailing\n");
            return 77; /* Not tested */
        }
    }

    if(policy != policy_env)
    {
        fprintf(stderr, "\nError: Desired policy:%s(%d), effective policy:%s(%d)\n",
                policy_str(policy_env), policy_env,  policy_str(policy), policy);
        ret = -1;
    }
    return ret;
}

/** ==================================================================== */
/**
 *   Main test process
 *
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    int rv;
    PROCESS workerpid;

    printf("Checking policy of main thread...");
    fflush(stdout);
    rv = desired_policy_is_running_policy(current_process());
    if(0 != rv) goto out;
    printf("OK!\n");
    workerpid = create_process(OS_PRI_PROC, "worker", worker, 2000, 15,
                               (OSTIME)0, 0,0, 0, 0);
    start(workerpid);
    printf("Checking policy of created thread...");
    fflush(stdout);
    rv = desired_policy_is_running_policy(workerpid);
    if(0 != rv) goto out;
    printf("OK!\n");

out:
    printf("\n Test suite result: %s\n\n",
           rv == 0 ? "PASSED" : rv == 77 ? "SKIP": "FAILED");
    exit(rv);
}

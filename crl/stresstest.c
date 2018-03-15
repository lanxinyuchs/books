/**
 *   Test of tcb integrety while randomly starting/stopping/killing
 *   threads.
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
 *   Revised : 2014-06-03 Ravineet Singh EAB/FJP/HB
 *   Change  : tcb search bug fixed in tcb.c, comment removed here.
 *
 *   Revised : 2014-04-14 Ravineet Singh EAB/FJP/HB
 *   Change  : Update to reentrant rand_r, nr of procs increased to 64.
 *             Test procs now have unique names. Stats added and printed.
 *
 *   Revised : 2014-03-18 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <lits_internal.h>
#include <ose.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <tcb.h>
#include <time.h>
#include <unistd.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define NR_PROCS (64)
#define M_PROC_NAME "stresstest_proc_"

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
unsigned int seedp;
uint32_t created = 0;
uint32_t killed = 0;
uint32_t suiside = 0;
uint32_t stopped = 0;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
/** ==================================================================== */
/**
 *   SIGALRM handler
 *
 *   @param         signo
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
PROCESS locked_pid = 0;
static void
timesup(int signo)
{
    lits_print_message("Test timed out (%x)!\n", (int)locked_pid);
    printf("\n Test suite result: FAILED\n\n");
    abort();
    exit(1);
}

/** ==================================================================== */
/**
 *   Process error handler
 *
 *   @param            user_called
 *   @param            ecode
 *   @param            extra
 *
 *   @return           OSADDRESS
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static OSADDRESS
e_entrypoint(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra)
{
    lits_print_message("Error handler called: \n"
                       "user_called: %d, ecode 0x%x , extra: 0x%x\n");
    printf("\n Test suite result: FAILED\n\n");

    /* Not handled. */
    return 0;
}

/** ==================================================================== */
/**
 *   Test process
 *    Run lits calls that involve tcb locks, a lot.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(pri_proc)
{
    tcb_t               *tcb;
    struct OS_pcb       *pcb;
    struct OS_pcb       *test;
    PROCESS pid = current_process();
    int r;
    char *env;

    r = 100 + (rand_r(&seedp)  %10000);
    while(1)
    {
        r--;
        (void)create_error_handler(pid, e_entrypoint, 2048);

        test = get_pcb(32);
        assert(test);
        assert(test->type == OS_ILLEGAL);
        free_buf( (union SIGNAL **) &test);

        pcb = get_pcb(pid);
        //Fixme assert(pcb->type != OS_ZOOMBIE);
        free_buf( (union SIGNAL **) &pcb);

        (void)get_bid(pid);
        tcb = get_tcb(pid);
        (void)tcb;

        if((r % 12) == 0)
        {
            suiside++;
            kill_proc(current_process());
            assert(!"Shall not get here!");
        }

        env = get_env(pid, "USER");
        if(env)
        {
            free_buf((union SIGNAL **)&env);
        }
        (void)get_pri(pid);
        (void)get_ptype(pid);

        if(0 == r)
        {
            stopped++;
            stop(pid);
        }
    }
}

/** ==================================================================== */
/**
 *   procs_left
 *    Check if any create test procs are alive.
 *
 *   @param            p   process lits array
 *                     l   nr of elemets in array
 *
 *   @return           1 if someone is alive, 0 otherwise.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static unsigned int
procs_left(PROCESS* p, unsigned int l)
{
    while(--l)
    {
        char name[80];

        sprintf(name, M_PROC_NAME "%d", l);
        if(hunt(name, 0, NULL, NULL))
        {
            return 1;
        }
    }
    return 0;
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
    int  i,r;
    int st = 0;
    PROCESS pids[NR_PROCS] = {0};
    struct sigaction a;
    unsigned int sec = 6 + 10*(NR_PROCS/100);

    seedp = (unsigned int)time(NULL);

    /* Install a sig handler for SIGALRM and set alarm. */
    a.sa_handler = timesup;
    sigemptyset(&a.sa_mask);
    a.sa_flags = 0;
    sigaction(SIGALRM,  &a, NULL);
    printf("alarm set for %d secs, %d procs \n", sec, NR_PROCS);
    (void)alarm(sec);

    /* Create procs, start most of them, stop the rest. */
    for (i = 0; i < NR_PROCS; i++)
    {
        int sa;
        char name[80];

        sprintf(name, M_PROC_NAME "%d", i);
        pids[i] = create_process(OS_PRI_PROC, name, pri_proc, 0, 0, 0, 0, NULL, 0, 0);
        created++;
        r = rand_r(&seedp) % NR_PROCS;

        sa = (int)((r % 8) == 0);
        if(pids[i])
        {
            if( sa)
            {
                stop(pids[i]);
                stopped++;
            }
            else
            {
                start(pids[i]);
            }
        }
    }

    /*
     * Randomly, kill or stop the created procs until
     * they are all dead (dramatic).
     */
    do
    {
        PROCESS pid;
        struct OS_pcb * pcb;

        r = rand_r(&seedp) % NR_PROCS;
        pid = pids[r];
        assert(r<NR_PROCS);

        pcb = get_pcb(pid);
        if(OS_ZOOMBIE == pcb->type)
        {
            free_buf((union SIGNAL **)&pcb);
            continue;
        }

        free_buf((union SIGNAL **)&pcb);

        st = (int)!st;

        if(st)
        {
            if(pid)
            {
                stop(pid);
                stopped++;
            }
        }
        else
        {
            if(pid)
            {
                locked_pid = pid;
                kill_proc(pid);
                killed++;
                pids[r] = 0;
            }
        }

    }
    while ( procs_left(pids, NR_PROCS) );

    printf("created %d, killed %d suiside(started) %d stopped %d \n",
           created, killed, suiside, stopped);
    printf("\n Test suite result: PASSED\n\n");
    exit(0);
}

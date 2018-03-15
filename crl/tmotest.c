/**
*   Test of timeout handling.
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
 *   Revised : 2016-01-25 Fredrik Skog
 *   Change  : Improved the tmo test case for removing a tmo signal from the
 *             queue when restarting the timeout.
 *
 *   Revised : 2015-04-09 Ravineet Singh
 *   Change  : Added test case to verify sigselect with only one sig in filter
 *             TODO: Rewrite OS_PROCESS(tester) in functions?
 *
 *   Revised : 2015-02-09 Ravineet Singh
 *   Change  : Static test process now created NUM_PROCS dynamic processes
 *             and awaits them. To test several clients and cleanup of
 *             timers after thread death.
 *
 *   Revised : 2014-10-03 Ravineet Singh
 *   Change  : Decreased tmo in loop from 200 -> 20.
 *
 *   Revised : 2014-05-08 Stanislav Vovk
 *   Change  : Addded test for restart_tmo, signal already in recv queue
 *
 *   Revised : 2014-02-06 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed compiler warnings.
 *
 *   Revised : 2014-01-24 Lars Jönsson EAB/FJP/TB
 *   Change  : Added a test case to check that the signal pointer is set
 *             to NIL after a call to request_tmo_sig().
 *
 *   Revised : 2014-01-17 Lars Jönsson EAB/FJP/TB
 *   Change  : Moved the check of oustanding timeouts to the end of the
 *             test suite.
 *
 *   Revised : 2014-07-07 Stanislav Vovk
 *   Change  : Added test for 0 ms timeout with signal
 *
 *   Revised : 2013-12-17 Stanislav Vovk
 *   Change  : Added test for 0 ms timeout
 *
 *   Revised : 2013-12-02 Lars Jönsson EAB/FJP/TB
 *   Change  : Changed the timeout tim from 2000ms to 200ms, when testing
 *             with many timeouts.
 *
 *   Revised : 2013-08-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Changed timout value from 50 to 30 after restarting
 *             the timeout.
 *
 *   Revised : 2012-02-20 Lars Jönsson EAB/FJP/TB
 *   Change  : Removed compiler warnings.
 *
 *   Revised : 2012-01-18 Lars Jönsson EAB/FJP/TB
 *   Change  : Added test case for cancel_tmo_sig().
 *
 *   Revised : 2011-12-13 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <ose.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "tmoserver.h"
/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define NUM_PROCS 3
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
    struct tmo_wrap tmo_wrap;
};

const uint8_t  data[] = {1, 2, 3, 4, 5, 6, 0x55, 0xaa, 0x55, 10, 11};

static PROCESS test_results[NUM_PROCS*2] = {0};

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Set return value from test slav
 *
 *   @param pid        pid
 *   @param result     resukt
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
set_result(PROCESS pid, int result)
{
    int i;
    for(i=0; i<NUM_PROCS; i++)
    {
        if(test_results[i*2] == pid)
        {
            test_results[i*2 + 1] = result;
            return;
        }
    }
    abort();
}

/** ==================================================================== */
/**
 *   Checks the content of a signal
 *
 *   @param str        String to precede the result string
 *   @param sig        Signal to check
 *   @param content    Expected content
 *   @param size       Size of content
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_content(char *str, union SIGNAL *sig, const uint8_t *content, int size)
{
    int      passed = 1;
    uint8_t  *ptr = &((uint8_t *)sig)[sizeof(SIGSELECT)];

    printf("%s - ", str);

    passed &= (size + sizeof(SIGSELECT)) <= sigsize(&sig);
    printf("content size=%d", (int)(sigsize(&sig) - sizeof(SIGSELECT)));
    printf(" [expected>=%d]", size);

    passed &= memcmp(ptr, data, size) == 0;

    printf(": %s\n", passed ? "OK" : "Fail");

    return passed;
}


static int check_sig_delay(int sig_no, OSTIME should_arrive_in)
{

   int i = 0;
   int count = 0;
   SIGSELECT     selAll[] = {0};
   union SIGNAL  *sig;
   int passed = 0;

   for(i=0;i<100;i++)
   {
      count++;
      sig = receive_w_tmo(1, selAll);
      if (sig)
      {
         printf("pid 0x%x, Warning !!!  signal %d received after %d , but should after %d \n",
                current_process(),
                sig->sig_no,
                should_arrive_in + (OSTIME)(1*count),
                should_arrive_in);

         if (sig->sig_no == sig_no)
         {
            passed = 1;
         }
         else
         {
            printf("Wrong signal arrived\n");
         }
         free_buf(&sig);

         break;
      }
   }

   return passed;
}

/** ==================================================================== */
/**
 *   Checks the result of a timeout handling action and prints the result
 *
 *   @param str        String to precede the result string
 *   @param sig_no     Signal number of the expected signal, 0 if no
 *                     signal is expected
 *   @param wait       Time to wait for a timeout signal
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result(char *str, int sig_no, OSTIME wait)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    int           passed = 1;


    sig = receive_w_tmo(wait, selAll);

    if ( sig_no )
    {
       passed = (sig != NIL) && (sig->sig_no == sig_no);
       if (!sig)
       {
          /* signal is received but not in time */
          passed = check_sig_delay(sig_no,wait);
       }
    }
    else
    {
        passed = sig == NIL;

    }

    printf("%s (pid 0x%x) - ", str,current_process());

    if ( sig != NIL )
    {
        printf("signal number=%u", sig->sig_no);
        free_buf(&sig);
    }
    else
    {
        printf("No signal received");
    }

    if ( sig_no )
        printf(" [expected=%u]", sig_no);
    else
        printf(" [expected no signal]");

    printf(": %s\n", passed ? "OK" : "Fail");

    return passed;
}

/** ==================================================================== */
/**
 *   Checks the result of a timeout handling action and prints the result
 *   Only receive selected signo
 *
 *   @param str        String to precede the result string
 *   @param sig_no     Signal number of the expected signal, 0 if no
 *                     signal is expected
 *   @param wait       Time to wait for a timeout signal
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result_selected(char *str, int sig_no, OSTIME wait)
{
    SIGSELECT     selOne[] = {1, 0};
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    int           passed = 1;

    if(sig_no)
    {
        selOne[1] = sig_no;
        sig = receive_w_tmo(wait, selOne);
    }
    else
    {
        sig = receive_w_tmo(wait, selAll);
    }

    if ( sig_no )
    {
       passed = (sig != NIL) && (sig->sig_no == sig_no);
       if (!sig)
       {
          /* signal is received but not in time */
          passed = check_sig_delay(sig_no,wait);
       }
    }
    else
        passed = sig == NIL;

    printf("%s (pid 0x%x) - ", str,current_process());

    if ( sig != NIL )
    {
        printf("signal number=%u", sig->sig_no);
        free_buf(&sig);
    }
    else
    {
        printf("No signal received");
    }

    if ( sig_no )
        printf(" [expected=%u]", sig_no);
    else
        printf(" [expected no signal]");

    printf(": %s\n", passed ? "OK" : "Fail");

    return passed;
}


/** ==================================================================== */
/**
 *   Checks the result of a timeout handling action including tmo ref.
 *
 *   @param str        String to precede the result string
 *   @param sig_no     Signal number of the expected signal, 0 if no
 *                     signal is expected
 *   @param wait       Time to wait for a timeout signal
 *   @param prev_ref   Timeout reference to compare with ref in signal
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result_ref(char *str, int sig_no, OSTIME wait, OSTMOREF prev_ref)
{
   SIGSELECT     selAll[] = {0};
   union SIGNAL  *sig;
   int           passed = 1;

   sig = receive_w_tmo(wait, selAll);

   if (sig_no)
   {
      passed = (sig != NIL) && (sig->sig_no == sig_no);
      if (!sig)
      {
         /* signal is received but not in time */
         passed = check_sig_delay(sig_no,wait);
      }
   }
   else
      passed = sig == NIL;

   printf("%s (pid 0x%x) - ", str,current_process());

   if (sig != NIL)
   {
      if (sig->tmo_wrap.ref == prev_ref) {
         passed = 0;
      }
      printf("received tmo_ref=%u %s equal to previous tmo_ref=%u.",
             sig->tmo_wrap.ref, passed ? "not" : "", prev_ref);
      printf(" signo=%u", sig->sig_no);
      free_buf(&sig);
   }
   else
   {
      printf("No signal received");
   }

   if (sig_no)
      printf(" [expected=%u]", sig_no);
   else
      printf(" [expected no signal]");

   printf(": %s\n", passed ? "OK" : "Fail");

   return passed;
}


/** ==================================================================== */
/**
 *   Checks the signal pointer and prints the result
 *
 *   @param str        String to precede the result string
 *   @param sig        Reference to the signal
 *   @param expected   Expected signal pointer value
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_sig_ptr(char *str, union SIGNAL **sig, union SIGNAL *expected)
{
    int passed = 1;

    printf("%s (pid 0x%x) - ", str,current_process());

    if ( sig )
    {
        passed = *sig == expected;
        printf("%p [expected=%p]", *sig, expected);
    }
    else
    {
        passed = 0;
        printf("No signal supplied");
    }

    printf(": %s\n", passed ? "OK" : "Fail");

    return passed;
}

/** ==================================================================== */
/**
 *   Test executer process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(tester)
{
    union SIGNAL *sig;
    OSTMOREF tmo_ref, tmo_ref_1;
    int  passed = 1;
    int  signo = 0;
    int i, k, cnt = 0, loop = 0;
    const int no_loops = 5;
    const int tmo_2can = 50, tmo_no = 3 * tmo_2can;
    OSTMOREF *tref = calloc(tmo_no, sizeof(OSTMOREF));
    SIGSELECT  sel[] = {0};

    printf("(pid: 0x%x) Request timeout (50ms) receive selected:\n",current_process());
    request_tmo(50, ++signo);
    passed &= check_result_selected(" - before expire", 0, 30);
    passed &= check_result_selected(" - after expire", signo, 70);

    printf("(pid: 0x%x) Request timeout (50ms):\n",current_process());
    request_tmo(50, ++signo);
    passed &= check_result(" - before expire", 0, 30);
    passed &= check_result(" - after expire", signo, 70);

    printf("(pid: 0x%x) Request timeout (50ms) and restart it (50ms):\n",current_process());
    tmo_ref = request_tmo(50, ++signo);
    passed &= check_result(" - before restart", 0, 30);
    restart_tmo(&tmo_ref, 50);
    passed &= check_result(" - after restart", 0, 30);
    passed &= check_result(" - after expire", signo, 70);

    printf("(pid: 0x%x) Cancel an expired tmo:\n",current_process());
    cancel_tmo(&tmo_ref);
    passed &= check_result(" - after cancel", 0, 90);

    printf("(pid: 0x%x) Request timeout (50ms) and cancel it:\n",current_process());
    tmo_ref = request_tmo(50, ++signo);
    passed &= check_result(" - before cancel", 0, 10);
    cancel_tmo(&tmo_ref);
    passed &= check_result(" - after cancel", 0, 90);

    printf("(pid: 0x%x) Cancel a canceled tmo:\n",current_process());
    cancel_tmo(&tmo_ref);
    passed &= check_result(" - after cancel", 0, 90);

    printf("(pid: 0x%x) Request timeout with signal (50ms):\n",current_process());
    sig = alloc(sizeof(union SIGNAL), ++signo);
    passed &= check_sig_ptr(" - signal ptr before timeout request", &sig, sig);
    request_tmo_sig(50, &sig);
    passed &= check_sig_ptr(" - signal ptr after timeout request", &sig, NIL);
    passed &= check_result(" - before expire", 0, 30);
    passed &= check_result(" - after expire", signo, 70);

    printf("(pid: 0x%x) Cancel an expired tmo (with signal) :\n",current_process());
    cancel_tmo(&tmo_ref);
    passed &= check_result(" - after cancel", 0, 90);

    printf("(pid: 0x%x) Request timeout with signal (50ms) and restart it (50ms):\n",current_process());
    sig = alloc(sizeof(union SIGNAL), ++signo);
    tmo_ref = request_tmo_sig(50, &sig);
    passed &= check_result(" - before restart", 0, 30);
    restart_tmo(&tmo_ref, 50);
    passed &= check_result(" - after restart", 0, 30);
    passed &= check_result(" - after expire", signo, 70);

    printf("(pid: 0x%x) Request timeout with signal (50ms), wait 60ms and restart it (50ms):\n",current_process());
   sig = alloc(sizeof(union SIGNAL), ++signo);
   tmo_ref = request_tmo_sig(50, &sig);
   tmo_ref_1 = tmo_ref; /* Save for later verification. */
    /* No timeout signal shall be received before the timeout has been
     * triggered.
     */
   passed &= check_result(" - before restart", 0, 30);
   delay(30);
    /* Timeout triggered after delay, restart_tmo shall remove the timeout
     * signal from the queue, and new timer will be created
     * (with a new tmo_ref).
     */
   restart_tmo(&tmo_ref, 50);
   /* Check that the timeout reference has been changed. */
   if (tmo_ref_1 == tmo_ref) {
      passed = 0;
   }
    /* Check that no signal is received immediately after the restart, i.e.
     * verify that the first timeout has been removed from the queue.
     */
    passed &= check_result(" - after restart", 0, 0);
    /* Wait until timer expires and check that the new tmo_ref is received,
     * and not the first one.
     */
    passed &= check_result_ref(" - after expire", signo, 70, tmo_ref_1);

    printf("(pid: 0x%x) Request timeout with signal (50ms) and cancel it:\n",current_process());
    sig = alloc(sizeof(union SIGNAL), ++signo);
    tmo_ref = request_tmo_sig(50, &sig);
    passed &= check_result(" - before cancel", 0, 10);
    cancel_tmo(&tmo_ref);
    passed &= check_result(" - after cancel", 0, 90);

    printf("(pid: 0x%x) Cancel a canceled tmo with sig:\n",current_process());
    cancel_tmo(&tmo_ref);
    passed &= check_result(" - after cancel", 0, 90);

    printf("(pid: 0x%x) Request two timeouts (50ms) and cancel one and check signal:\n",current_process());
    sig = alloc(sizeof(SIGSELECT) + sizeof(data), ++signo);
    memcpy(&((uint8_t *)sig)[sizeof(SIGSELECT)], data, sizeof(data));
    tmo_ref = request_tmo_sig(50, &sig);
    request_tmo(50, ++signo);
    passed &= check_result(" - before cancel", 0, 10);
    delay(80);
    sig = cancel_tmo_sig(&tmo_ref);
    passed &= check_content(" - cancel first timeout",
                            sig, data, sizeof(data));
    passed &= check_result(" - check other timeout", signo, 10);
    passed &= check_result(" - check first timeout", 0, 10);

    printf("(pid: 0x%x) Request timeout (0ms):\n",current_process());
    request_tmo(0, ++signo);
    passed &= check_result(" - after expire", signo, 1);

    printf("(pid: 0x%x) Request timeout (0ms) with signal:\n",current_process());
    sig = alloc(sizeof(union SIGNAL), ++signo);
    request_tmo_sig(0, &sig);
    passed &= check_result(" - after expire", signo, 1);

    printf("(pid: 0x%x) Request many timeouts, cancel a few, receive the rest:\n",current_process());
    while (loop < no_loops)
    {
        for (i = 0, signo = 0; i < tmo_no; i++)
            tref[i] = request_tmo(20, ++signo);
        for (i = 0; i < tmo_2can; i++)
            cancel_tmo(&tref[i]);
        delay(400);
        for (i = i; i < (tmo_2can + tmo_2can); i++)
            cancel_tmo(&tref[i]);
        for (k = i; k < tmo_no; k++)
        {
            sig = receive_w_tmo(0, sel);
            if (sig)
            {
                cnt++;
                free_buf(&sig);
            }
        }
        if (cnt == (tmo_no - tmo_2can - tmo_2can)) passed &= 1;
        else passed &= 0;
        printf("(pid: 0x%x) - Signals recv: %d, expected: %d, loop no: %d(%d)\n",
               current_process(),cnt, (tmo_no - tmo_2can - tmo_2can), ++loop, no_loops);
        cnt = 0;
    }

    printf("(pid: 0x%x) Checking outstanding timeouts:\n",current_process());
    passed &= check_result(" - Waiting for 500ms", 0, 500);

    /* create a timer and die. */
    sig = alloc(sizeof(union SIGNAL), ++signo);
    request_tmo_sig(0, &sig);

    set_result(current_process(), passed);
    kill_proc(current_process());
}

/** ==================================================================== */
/**
 *   Main test process, starts slave(s), waits for them to finish,
 *   check and reports result.
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
    unsigned int seedp = (unsigned int)time(NULL);
    int i, r;
    PROCESS pids[NUM_PROCS];

    printf("\n Test suite started, creating %d test slaves\n", NUM_PROCS);
    for(i=0; i<NUM_PROCS; i++)
    {
        r = rand_r(&seedp) % 2000;
        delay(r);
        pids[i] = create_process(OS_PRI_PROC, "tester", tester, 2000, 15,
                                 (OSTIME)0, 0,0, 0, 0);
        test_results[i*2] = pids[i];
        attach(NULL, pids[i]);
        start(pids[i]);
    }

    for(i=0; i<NUM_PROCS; i++)
    {
        union SIGNAL *sig;
        SIGSELECT     sel_attach[] = {1, OS_ATTACH_SIG};

        sig = receive(sel_attach);
        free_buf(&sig);
    }

    for(i=0; i<NUM_PROCS; i++)
    {
        int result = test_results[i*2 +1];
        if(1 !=  result)
        {
            printf("\n Test suite result: FAILED\n\n");
            exit(-1);
        }
    }

    printf("\n Test suite (%d threads) result: PASSED\n\n", NUM_PROCS);
    exit(0);
}

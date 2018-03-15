/**
 *   Client test program.
 *
 *   @file
 *
 *   This file is a part of the test programs for the lits (Legacy IPC and
 *   Task Support) lib.
 *
 *   Copyright (C) 2011 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-03-27 Ravineet Singh
 *   Change  : Removed LINX dependency. Removed trailing whitespaces.
 *
 *   Revised : 2011-04-05 Lars Jönsson EAB/FJP/TB
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
#include <errno.h>
#include <time.h>
#include <sys/time.h>

#include <assert.h>

#include "client_server.sig"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define LOCAL_SERVER_NAME "local/server"

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
const char* serverPath = "server";

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Get number of micro seconds from start_time, i.e. delta from
 *   start_time.
 *
 *   @param start_time Start time
 *
 *   @return           Delta time
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static  unsigned long long
get_usecs(struct timeval *start_time)
{
    struct timeval  now;

    gettimeofday(&now, 0);
    return ((unsigned long long)(now.tv_sec - start_time->tv_sec) * 1000000) +
           (unsigned long long)(now.tv_usec - start_time->tv_usec);
}


/** ==================================================================== */
/**
 *   Calculates the integer fraction of a divided by b, with the specified
 *   number of digits.
 *
 *   @param a          Numerator
 *   @param a          Denominator
 *   @param dogits     Number of digits in the result
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static unsigned long long
calc_fract(unsigned long long a, unsigned long long b, int digits)
{
    int mul = 1;

    if (digits > 6)
        digits = 6;

    while (digits-- > 0)
        mul *= 10;

    return ((a%b)*mul)/b;
}

/** ==================================================================== */
/**
 *   Test ping pong towards the specified server.
 *
 *   @param name       Server name including link handler path
 *   @param num_loops  Number of loops for ping pong
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
test_server(char *name, int num_loops)
{
    unsigned long long  time[10];
    unsigned long long  loop_time;
    unsigned long long  loop_time_one_shot;
    int                 i = 0;
    struct timeval      start_time;
    SIGSELECT           selAll[] = {0};
    PROCESS             server = 0;
    int                 huntresult;
    union SIGNAL        *sig;
    int                 loop;

    printf("\nSend and receive to process \"%s\" 1 time\n", name);

    gettimeofday(&start_time, 0);
    huntresult =  hunt(name, 0, &server, NULL);
    (void)huntresult;
    time[i++] = get_usecs(&start_time);

    printf("Server PID: %u (0x%08x)\n", server, server);

    time[i++] = get_usecs(&start_time);
    sig = alloc(sizeof(struct ping_sig), CL_SER_PING);
    time[i++] = get_usecs(&start_time);
    send(&sig, server);
    time[i++] = get_usecs(&start_time);

    time[i++] = get_usecs(&start_time);
    sig = receive(selAll);
    time[i++] = get_usecs(&start_time);

    assert(sender(&sig) == server);

    time[i++] = get_usecs(&start_time);
    free_buf(&sig);
    time[i++] = get_usecs(&start_time);

    printf("Send and receive to process \"%s\" %d times\n", name, num_loops);

    gettimeofday(&start_time, 0);

    for (loop = 0; loop < num_loops; loop++)
    {
        sig = alloc(sizeof(struct ping_sig), CL_SER_PING);
        send(&sig, server);
        sig = receive(selAll);
        free_buf(&sig);
    }

    loop_time = get_usecs(&start_time);
    loop_time_one_shot =
        time[3] - time[1] + time[5] - time[4] + time[7] - time[6];

    printf("\nResult - send and receive to process \"%s\"\n", name);
    printf("hunt()     - %4llu us\n", time[0]);
    printf("alloc()    - %4llu us\n", time[2] - time[1]);
    printf("send()     - %4llu us\n", time[3] - time[2]);
    printf("receive()  - %4llu us\n", time[5] - time[4]);
    printf("free_buf() - %4llu us\n", time[7] - time[6]);

    printf("\nResult - send and receive to process \"%s\" %d times\n", name,
           num_loops);
    printf("Total time: %8llu us\n", loop_time);
    printf("Loop time:  %8llu us\n", loop_time/(2*num_loops));
    printf("hunt()     - %4llu.%02llu us (estimated)\n",
           (time[0]*loop_time)/(2*num_loops*loop_time_one_shot),
           calc_fract(time[0]*loop_time, 2*num_loops*loop_time_one_shot, 2));
    printf("alloc()    - %4llu.%02llu us (estimated)\n",
           ((time[2]-time[1])*loop_time)/(2*num_loops*loop_time_one_shot),
           calc_fract((time[2]-time[1])*loop_time, 2*num_loops*loop_time_one_shot, 2));
    printf("send()     - %4llu.%02llu us (estimated)\n",
           ((time[3]-time[2])*loop_time)/(2*num_loops*loop_time_one_shot),
           calc_fract((time[3]-time[2])*loop_time, 2*num_loops*loop_time_one_shot, 2));
    printf("receive()  - %4llu.%02llu us (estimated)\n",
           ((time[5]-time[4])*loop_time)/(2*num_loops*loop_time_one_shot),
           calc_fract((time[5]-time[4])*loop_time, 2*num_loops*loop_time_one_shot, 2));
    printf("free_buf() - %4llu.%02llu us (estimated)\n",
           ((time[7]-time[6])*loop_time)/(2*num_loops*loop_time_one_shot),
           calc_fract((time[7]-time[6])*loop_time, 2*num_loops*loop_time_one_shot, 2));


}

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
    PROCESS       client;

    while (1)
    {
        sig = receive(selAll);
        client = sender(&sig);
        free_buf(&sig);
        sig = alloc(sizeof(struct pong_sig), CL_SER_PONG);
        send(&sig, client);
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
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    start(create_process(OS_PRI_PROC, LOCAL_SERVER_NAME, server_proc, 2000, 15,
                         (OSTIME)0, 0,0, 0, 0));

    test_server((char *)serverPath, 10000);
    test_server((char *)LOCAL_SERVER_NAME, 10000);

    printf("\n");

    exit(0);
}

/**
 *   Test of time handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
 *
 *   Copyright (C) 2012 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2012-05-09 Lars Jönsson EAB/FJP/TB
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
#include <sys/time.h>

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
 *   Calculates and returns the diff in microseconds.
 *
 *   @param sec        Array with two times in seconds
 *   @param usec       Array with two times in microseconds
 *
 *   @return           Diff in microseconds
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static uint32_t
calc_diff(uint32_t *sec, uint32_t *usec)
{
    uint32_t diff;

    if ( usec[1] >= usec[0] )
    {
        diff  = usec[1] - usec[0];
        diff += (sec[1] - sec[0])*1000000;
    }
    else
    {
        diff  = 1000000 + usec[1] - usec[0];
        diff += (sec[1] - 1 - sec[0])*1000000;
    }

    return diff;
}

/** ==================================================================== */
/**
 *   Calculates absolute diff of the values.
 *
 *   @param val1       First value
 *   @param val2       Second value
 *
 *   @return           Absolute difference
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static uint32_t
abs_diff(uint32_t val1, uint32_t val2)
{
    if ( val1 > val2 )
        return val1 - val2;

    return val2 - val1;
}

/** ==================================================================== */
/**
 *   Checks the result of system time retrieval
 *
 *   @param str        String to precede the result string
 *   @param tv         Array with two time of day times
 *   @param ticks      Array with two tick values
 *   @param microsecs  Array with two values of elapsed times since
 *                     last tick, NULL if it should not be checked
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
check_result(char *str, struct timeval *tv, OSTICK *ticks, OSTICK *microsecs)
{
    int      passed = 1;
    uint32_t tod_sec[2];
    uint32_t tod_usec[2];
    uint32_t tod_diff;
    uint32_t tick_sec[2];
    uint32_t tick_usec[2];
    uint32_t tick_diff;
    uint32_t ticks_per_sec = (uint32_t)(1000000/system_tick());
    uint32_t diff;
    uint32_t max_diff = 1000;

    tod_sec[0] = (uint32_t)tv[0].tv_sec;
    tod_sec[1] = (uint32_t)tv[1].tv_sec;
    tod_usec[0] = (uint32_t)tv[0].tv_usec;
    tod_usec[1] = (uint32_t)tv[1].tv_usec;
    tick_sec[0] = (uint32_t)ticks[0]/ticks_per_sec;
    tick_sec[1] = (uint32_t)ticks[1]/ticks_per_sec;
    tick_usec[0] = (uint32_t)ticks[0]%ticks_per_sec*1000;
    tick_usec[1] = (uint32_t)ticks[1]%ticks_per_sec*1000;

    if ( microsecs )
    {
        tick_usec[0] += (uint32_t)microsecs[0];
        tick_usec[1] += (uint32_t)microsecs[1];
        max_diff = 12;
    }

    tod_diff  = calc_diff(tod_sec, tod_usec);
    tick_diff = calc_diff(tick_sec, tick_usec);

#if 0
    printf("\n\n");
    printf("tod_sec:   %u, %u\n", tod_sec[0], tod_sec[1]);
    printf("tod_usec:  %u, %u\n", tod_usec[0], tod_usec[1]);
    printf("tick_sec:  %u, %u\n", tick_sec[0], tick_sec[1]);
    printf("tick_usec: %u, %u\n", tick_usec[0], tick_usec[1]);
    printf("tod_diff:  %uus\n", tod_diff);
    printf("tick_diff: %uus\n", tick_diff);
    printf("\n\n");
#endif

    diff = abs_diff(tod_diff, tick_diff);
    passed &= diff < max_diff;

    printf("%s - ", str);
    printf("diff=%uus (max allowed %uus)", diff, max_diff);

    printf(": %s\n", passed ? "OK" : "Fail");

    return passed;
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
    int     passed = 1;
    OSTICK  microsecs[2];
    OSTICK  ticks[2];
    struct timeval tv[2];

    printf("Verify retrieval system time and tick using time of day:\n");
    printf("One system tick is %u microseconds \n",system_tick());

    passed &= (gettimeofday(&tv[0], NULL) == 0);
    ticks[0] = get_ticks();
    delay(200);
    passed &= (gettimeofday(&tv[1], NULL) == 0);
    ticks[1] = get_ticks();
    passed &= check_result(" - ticks method 1 [get_ticks()]",
                           tv, ticks, NULL);

    passed &= (gettimeofday(&tv[0], NULL) == 0);
    ticks[0] = get_systime(NULL);
    delay(125);
    passed &= (gettimeofday(&tv[1], NULL) == 0);
    ticks[1] = get_systime(NULL);
    passed &= check_result(" - ticks method 2 [get_systime(NULL)]",
                           tv, ticks, NULL);

    passed &= (gettimeofday(&tv[0], NULL) == 0);
    ticks[0] = get_systime(&microsecs[0]);
    delay(227);
    passed &= (gettimeofday(&tv[1], NULL) == 0);
    ticks[1] = get_systime(&microsecs[1]);
    passed &= check_result(" - ticks and time elapsed since last tick",
                           tv, ticks, microsecs);

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}

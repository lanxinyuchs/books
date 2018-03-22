/**
 *   Time handling functions.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2012-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-03-16 Ravineet Singh EAB/FJP/HB
 *   Change  : Timers implementation moved to tmoserver.c. The new interface
 *             tmoserver.h  is used for tmo functionality.
 *
 *   Revised : 2015-02-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Tmo entry (and the Linux timer) are removed when user receives
 *             the tmo signal.
 *
 *   Revised : 2015-02-24 Ravineet Singh EAB/FJP/HB
 *   Change  : User signal allocated (if not provided) in lits_tmo_remove.
 *             Timer entry removed only when user cancels tmo or restart
 *             tmo. At restart, old tmo entry is removed and new entry is
 *             created (with new ref).
 *
 *   Revised : 2015-02-16 Ravineet Singh EAB/FJP/HB
 *   Change  : Timer is only deleted when the user cancels the tmo or when
 *             the user restarts the tmo. At restart, timer is deleted and
 *             a new timer is created. The entry is only removed when user
 *             signal is processed and when user restarts the tmo. At
 *             restart, a new entry is created with old ref and usersig.
 *
 *   Revised : 2015-02-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Usersignal is wrapped to be able to dectect it in zzreceive[_xx]
 *             despite the underlaying IPC mechanism chinging the pointer.
 *
 *   Revised : 2014-10-03 Ravineet Singh EAB/FJP/HB
 *   Change  : Rewritten remove_tmo (signal lock region smaller) and
 *             also independent of when TMO_SIGNAL is taken.
 *             Added/converted list (signal blocked) funtions.
 *             Made lits_tmo_cleanup, a bit more readable.
 *
 *   Revised : 2014-09-15 Ravineet Singh EAB/FJP/HB
 *   Change  : Fixed typo in remove_tmo()
 *
 *   Revised : 2014-09-03 Henrik Wallin
 *   Change  : Add assert(lits_data).
 *             lits functions must be called from a lits created thread.
 *
 *   Revised : 2014-06-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Improved performance for cleaning up recevied timeouts. The
 *             queue protection locks are not taken if the timeout queue is
 *             empty. This will be even more improved when a separate queue
 *             for expired timeouts is introduced.
 *
 *   Revised : 2014-05-26 Stanislav Vovk
 *   Change  : Delete timer instance when canceling a timeout with
 *             tmo signal already in recv queue.
 *
 *   Revised : 2014-05-21 Stanislav Vovk
 *   Change  : Removed alloc from signal handler. Signal is instead
 *             allocated at timeout request. Removed lits_error as
 *             well since it uses vsprintf
 *
 *   Revised : 2014-05-15 Ravineet Singh EAB/FJP/HB
 *   Change  : Cancelling expired tmo is now allowed as to mimic
 *             OSE.
 *
 *   Revised : 2014-05-08 Stanislav Vovk
 *   Change  : Fixed restart_tmo when signal is already in recv queue
 *             Reusing same timer instance at restart_tmo
 *             Removed timer_delete from sig handler
 *
 *   Revised : 2014-02-14 Ravineet Singh EAB/FJP/HB
 *   Change  : Renamed the define LITS_TMP_TIME to ITC_OVER_LINX
 *
 *   Revised : 2014-02-14 Lars Jönsson EAB/FJP/TB
 *   Change  : Updated after change of SIGNAL definition.
 *
 *   Revised : 2014-02-10 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed a warning.
 *
 *   Revised : 2014-02-04 Lars Jönsson EAB/FJP/TB
 *   Change  : Timeout implementation for ITC slightly rewritten to correct
 *             some bugs.
 *
 *   Revised : 2014-01-24 Stanislav Vovk
 *   Change  : Increased 1us tmo to 10us for better stability
 *
 *   Revised : 2014-01-24 Lars Jönsson EAB/FJP/TB
 *   Change  : The signal pointer now set to NIL in request_tmo_sig() to
 *             mimic the behaviour of OSE. Nothing of this is specified
 *             in the OSE manual.
 *
 *   Revised : 2014-01-07 Stanislav Vovk
 *   Change  : Fix: copy signal pointer before requesting a timeout
 *
 *   Revised : 2013-12-17 Stanislav Vovk
 *   Change  : When requested timeout is 0ms a 1us timeout is created
 *
 *   Revised : 2013-09-25 Stanislav Vovk
 *   Change  : Added timer functionality which uses kernel timers
 *             It is used when build with ITC support
 *
 *   Revised : 2012-05-10 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for retrieving system time and tick.
 *
 *   Revised : 2012-01-09 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <time.h>
#include <tmoserver.h>
#include <unistd.h>

#include "lits_internal.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/*
**  NOTE! If this value is changed, the routines using it should be checked
**        for overflow in the calculations
*/
#define LITS_SYSTEM_TICK (1000) /* Number of us per tick, i.e. 1 ms */

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

/* ========================================================================
 *   Internal Time Handling Functions
 * ========================================================================
 */


/* ========================================================================
 *   Global Time Handling Functions
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Delay execution for the specified time.
 *
 *   @param timeout    Time to sleep in  milliseconds.
 *   @param ecode      Error code
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzdelay(OSTIME timeout)
{
    time_t seconds = timeout/1000;
    long milliseconds = timeout%1000;
    struct timespec sleep_time = { seconds, milliseconds * 1000000 };
    int result;

    assert(lits_data);

    do
    {
        result = nanosleep(&sleep_time, &sleep_time);

        if ( (result == -1 && errno == EINVAL) )
            lits_error("Delay time out of range");

    }
    while (result == -1 && errno == EINTR);
}

/** ==================================================================== */
/**
 *   Cancel the specified timeout.
 *
 *   @param tmoref     Timeout reference
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzcancel_tmo(OSTMOREF *tmoref)
{
    assert(lits_data);
    assert(tmoref);

    union SIGNAL *sig;
    if(tmoref)
    {
        sig = tmo_server_cancel(*tmoref);
        if(sig)
        {
            free_buf(&sig);
        }
    }
}

/** ==================================================================== */
/**
 *   Cancel the specified timeout and get the timeout signal.
 *
 *   @param tmoref     Timeout reference
 *
 *   @return           Timeout signal
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
union SIGNAL *
        zzcancel_tmo_sig(OSTMOREF *tmoref)
{
    union SIGNAL *sig;

    assert(lits_data);
    assert(tmoref);

    sig = NULL;
    if(tmoref)
    {
        sig = tmo_server_cancel(*tmoref);
    }
    return sig;
}

/** ==================================================================== */
/**
 *   Request a timeout.
 *
 *   @param timeoout   Timeout time in milliseconds
 *   @param tmosigno   Signal number for the signal that is sent when the
 *                     timeout expires
 *
 *   @return           Timeout reference
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSTMOREF
zzrequest_tmo(OSTIME timeout, SIGSELECT tmosigno)
{
    assert(lits_data);
    return tmo_server_request(timeout, tmosigno, NULL);
}

/** ==================================================================== */
/**
 *   Request a timeout with the specified signal.
 *
 *   @param timeoout   Timeout time in milliseconds
 *   @param tmosig     Signal that is sent when the timeout expires
 *
 *   @return           Timeout reference
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSTMOREF
zzrequest_tmo_sig(OSTIME timeout, union SIGNAL **tmosig)
{
    assert(lits_data);
    return tmo_server_request(timeout, 0, tmosig);
}

/** ==================================================================== */
/**
 *   Restart (re-trigger) the specified timeout.
 *
 *   @param tmoref     Timeout reference
 *   @param timeoout   Timeout time in milliseconds
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
void
zzrestart_tmo(OSTMOREF *tmoref, OSTIME timeout)
{
    assert(lits_data);
    assert(tmoref);
    *tmoref = tmo_server_restart(*tmoref, timeout);
}

/** ==================================================================== */
/**
 *   Get system tick length in microseconds.
 *
 *   @param            -
 *
 *   @return           System tick length in microseconds
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSTIME
zzsystem_tick(void)
{
    return LITS_SYSTEM_TICK;
}

/** ==================================================================== */
/**
 *   Get system time, i.e. number of ticks since last boot and number of
 *   microseonds since last tick.
 *
 *   @param microsecs  Placeholder for returning number of microseconds
 *                     since last tick. Set to NULL if the number of
 *                     microseconds should not be returned
 *
 *   @return           Number of ticks since last boot
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSTICK
zzget_systime(OSTICK* microsecs)
{
    struct timespec tp;

    if ( clock_gettime(CLOCK_MONOTONIC, &tp) != 0 )
        lits_error("Failed to get system time");

    if ( microsecs )
        *microsecs = (tp.tv_nsec/1000) % LITS_SYSTEM_TICK;

    return tp.tv_sec * (1000*1000/LITS_SYSTEM_TICK) +
           tp.tv_nsec/1000/LITS_SYSTEM_TICK;
}

/** ==================================================================== */
/**
 *   Get number of ticks since last boot.
 *
 *   @param            -
 *
 *   @return           Number of ticks since last boot
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OSTICK
zzget_ticks(void)
{
    return(get_systime(NULL));
}

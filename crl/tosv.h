/* COPYRIGHT-ENEA-SRC-R2 *
 **************************************************************************
 * Copyright (C) 2004-2006 by Enea Software AB.
 * All rights reserved.
 *
 * This Software is furnished under a software license agreement and
 * may be used only in accordance with the terms of such agreement.
 * Any other use or reproduction is prohibited. No title to and
 * ownership of the Software is hereby transferred.
 *
 * PROPRIETARY NOTICE
 * This Software consists of confidential information.
 * Trade secret law and copyright law protect this Software.
 * The above notice of copyright on this Software does not indicate
 * any actual or intended publication of such Software.
 **************************************************************************
 * COPYRIGHT-END */

#ifndef _TOSV_H
#define _TOSV_H

/*
 * Old-style OSE4 API for timeout services, emulated in OSE5 on top of
 * the timeout service specified in ose.h. 
 *
 * DO NOT USE this file!! Use ose.h instead.
 */ 

#include "ose.h"
#include "osetypes.h"

#ifdef __cplusplus
extern "C" {
#endif

#define OSE_CANCEL_TMO_SIG              0x470000
#define OSE_CANCEL_TMO                  0x480000

struct TosvInternal;    /* Internal TOSV structure. */

/*
 *===========================================================================
 *			CANCEL_INFO
 *===========================================================================
 * Description: Contains vital information for identification of correct time-
 *              out request at cancellation.
 *
 * Parameters:  sigNo          Timeout signal number (bits 0-31).
 *              sigNo2         Timeout signal number (bits 32-63).
 *              instance       Unique instance number for time-out request.
 *              pointer        Direct access pointer to time-out request.
 */

typedef struct
{
    SIGSELECT             sigNo;
    SIGSELECT             sigNo2;
    U32                   instance;
    struct TosvInternal * pointer;
} CANCEL_INFO;

/*
 *===========================================================================
 *                           requestTmo()
 *===========================================================================
 *
 * Description: Request a time-out with application specified signal.
 *
 * Parameters:  cancelInfo     Identification of time-out for cancellation.
 *              timeOut        Time-out in milliseconds.
 *              tmoSig         User supplied signal to return at time-out.
 *
 * Returns:     Nothing.
 */

void requestTmo(CANCEL_INFO *cancelInfo, OSTIME timeOut, union SIGNAL **tmoSig);

/*
 *===========================================================================
 *                           fRequestTmo()
 *===========================================================================
 *
 * Description: Request a time-out with TOSV default signal.
 *
 * Parameters:  cancelInfo     Identification of time-out for cancellation.
 *              timeOut        Time-out time in milliseconds.
 *              client         Client process identifier. Must be equal to
 *                             current_process() of caller.
 *              tmoSigNo       Signal number for default signal.
 *
 * Returns:     Nothing.
 */

void fRequestTmo(CANCEL_INFO *cancelInfo, OSTIME timeOut, PROCESS pid, SIGSELECT tmoSigNo);

/*
 *===========================================================================
 *                           cancelTmo()
 *===========================================================================
 *
 * Description: Cancel (normal or fast) time-out request and return time-out
 *              signal.
 *
 * Parameters:  cancelInfo     Identification of time-out to cancel.
 *
 * Returns:     Signal specified in requestTmo call or default signal (with
 *              user specified signo) if fRequestTmo was used.
 */

union SIGNAL *cancelTmo(CANCEL_INFO *cancelInfo);

/*
 *===========================================================================
 *                           fCancelTmo()
 *===========================================================================
 *
 * Description: Cancel (normal or fast) time-out request and dispose of
 *              (free) time-out signal.
 *
 * Parameters:  cancelInfo     Identification of time-out to cancel.
 *
 * Returns:     Nothing.
 */

void fCancelTmo(CANCEL_INFO *cancelInfo);

/*
 *===========================================================================
 *                           resetTmo()
 *===========================================================================
 *
 * Description: Restart the specified (normal or fast) time-out.
 *              NOTE: an expired fast time-out will be transformed into a
 *                    normal time-out.
 *
 * Parameters:  cancelInfo     Identification of time-out to reset.
 *              timeOut        New time-out in milliseconds.
 *
 * Returns:     Nothing.
 */

void resetTmo(CANCEL_INFO *cancelInfo, OSTIME timeOut);

#ifdef __cplusplus
}
#endif

#endif
/* #ifndef _TOSV_H */

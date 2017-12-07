/*
 *
 * Copyright (c) Ericsson AB  2013 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 * IDENTIFICATION
 * --------------
 * Description:  This file contains the signal definitions of the Generic
 *               License Management Solution (GLMS) internal signals.
 *
 *  !!! Signals from 0x019001f0 to 0x019001ff are reserved  !!!
 *  !!! for GLMS internal signals                           !!!
 *
 */

#ifndef GLMS_INTERNAL_SIG
#define GLMS_INTERNAL_SIG

#ifdef __cplusplus
extern "C" {
#endif


/******************************************************************************
 *
 * Signal : GlmsTimerExpiredInd
 *
 * Descr  : Used by GLMS timer service to notify GLMS component 
 *          when a timer has expired.
 *
 * Dir    : GlmsTimer -> GLMS
 *
 * Data   : timerId    - TimerId of the expired timer
 *          clientId   - clientId is set by GLMS when a timer is requested, it
 *                       is then returned to GLMS when the timer expires.
 *
 *****************************************************************************/
#define  GLMS_TIMER_EXPIRED_IND (0x19001f0) /*!- SIGNO(GlmsTimerExpiredInd) -!*/
typedef struct
{
   uint32_t     sigNo;
   uint32_t     timerId;
   uint32_t     clientId;
} GlmsTimerExpiredInd;


#ifdef __cplusplus
}
#endif

#endif /* GLMS_INTERNAL_SIG */

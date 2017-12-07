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
 *
 * IDENTIFICATION
 * --------------
 * Description:  This file contains the signal definitions of the Generic
 *               License Management Solution (GLMS) Timer interface.
 *
 *
 * @(#) ClearCase ID:
 *
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: 2013-05-29 Ramakrushna Mishra
 * Change:  First version
 *
 */

#ifndef GLMS_TIMI_SIG
#define GLMS_TIMI_SIG

#ifdef __cplusplus
extern "C" {
#endif

/*
 ******************************************************************************
 * INCLUDE FILES
 ******************************************************************************
 */

/*#include "stdint.h"*/
#include "glmsadpi/glmsDataTypes.h"
#include <ose.h>

/*
 ******************************************************************************
 * TYPES
 ******************************************************************************
 */


/*
 ******************************************************************************
 * SIGNALS
 ******************************************************************************
 */
/******************************************************************************
 *
 * Signal : GlmsTimerExpiredInd
 *
 * Descr  : Used by the adaptation layer to start the GLMS timer.
 *
 * Dir    : GlmsTimer -> GLMS
 *
 * Data   : timerId : TimerId of the expired timer
 *
 *****************************************************************************/

#define  GLMS_TIMER_EXPIRED_IND (0x40001) /*!- SIGNO(GlmsTimerExpiredInd) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     timerId;
   uint32_t     clientId;
} GlmsTimerExpiredInd;


#ifdef __cplusplus
}
#endif

#endif /* GLMS_TIMI_SIG */

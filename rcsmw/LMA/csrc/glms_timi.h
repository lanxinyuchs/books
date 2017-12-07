/*
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
 * Description:  This headerfile holds the function prototype to start the
 *               GLMS timer component.
 *
 */

#ifndef GLMS_TIMI_H
#define GLMS_TIMI_H

#ifdef __cplusplus
extern "C"
{
#endif

#include "glmsadpi/glmsDataTypes.h"

/*
 ******************************************************************************
 * DEFINITIONS
 ******************************************************************************
 */

#define   GLMS_SECONDS_IN_ONE_DAY           (86400)
#define   GLMS_SECONDS_IN_SEVEN_DAYS        (604800)
#define   GLMS_SECONDS_IN_14_DAYS           (1209600)
#define   GLMS_SECONDS_IN_TWENTYONE_DAYS    (1814400)
#define   GLMS_SECONDS_IN_36_HOURS          (129600)
#define   GLMS_WARNING_PERIOD               (259200) /* 72h */

/* Defines used as clientId in calls to timi functions */
#define   GLMS_DAILY_VALIDATION_TIMER   (0)
#define   GLMS_EU_EXPIRY_WARNING_TIMER  (1)
#define   GLMS_EU_EXPIRY_TIMER          (2)
#define   GLMS_IU_EXPIRY_WARNING_TIMER  (3)
#define   GLMS_IU_EXPIRY_TIMER          (4)
#define   GLMS_PU_EXPIRY_TIMER          (5)
#define   GLMS_AM_EXPIRY_TIMER          (6)
#define   GLMS_GP_EXPIRY_WARNING_TIMER  (7)
#define   GLMS_GP_EXPIRY_TIMER          (8)

/*
 ******************************************************************************
 * TYPE DEFINITIONS
 ******************************************************************************
 */

struct Timer
{
   uint32_t        timerId;
   uint32_t        clientId;
   itc_mbox_id_t   clientMid;
   int             fd;
   GlmsBool        periodic;
   struct itimerspec timerLength;
   struct Timer *nextTimer;
};


/*
 ******************************************************************************
 * FUNCTION PROTOTYPES
 ******************************************************************************
 */
void
initTimers();

void
terminateTimerThread();

void
wakeUpHandler();

uint32_t
requestTimer(time_t32 timerLength, GlmsBool periodic, uint32_t clientId);

GlmsBool
deleteTimer(uint32_t timerId);

void
deleteAllTimers();

GlmsBool
isTimerActive(uint32_t clientId);

void
printAllTimerStatus();

void
printCurrentTime();

#ifdef __cplusplus
}
#endif
#endif /* GLMS_TIMI_H */

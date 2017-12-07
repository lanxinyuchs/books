/**
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */


/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <itc.h>

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <inttypes.h>
#include <unistd.h>

#include "com_ericsson_glms.h"

#include "glmsadpi/glmsDataTypes.h"
#include "glmsadpi/glms_adpi.sig"
#include "glms_main.h"
#include "glms_timi.h"
#include "glmsUtils.h"
#include "persistentStorage.h"
#include "data_unlocks.h"
#include "data_lm.h"
#include "data_keyFile.h"
#include "clients.h"
#include "data_common.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
union itc_msg
{
   uint32_t  sigNo;

   GlmsAdpiPersistentParameterIndexListRsp  glmsPpIndexListRsp;
   GlmsAdpiSoftwareParameterIndexListRsp    glmsSpIndexListRsp;
   GlmsAdpiPersistentParameterGetRsp        glmsPpGetRsp;
   GlmsAdpiSoftwareParameterGetRsp          glmsSpGetRsp;
   GlmsAdpiMoUpdateEuInd                    glmsAdpiMoUpdateEuInd;
   GlmsAdpiEuAlarmInd                       glmsAdpiEuAlarmInd;
   GlmsAdpiMoUpdateIuInd                    glmsAdpiMoUpdateIuInd;
   GlmsAdpiMoUpdateAmInd                    glmsAdpiMoUpdateAmInd;
   GlmsAdpiAmAlarmInd                       glmsAdpiAmAlarmInd;

};
/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

EmergencyUnlockState    euData;
IntegrationUnlockState  iuData;
ProductionUnlockState   puData;
AutonomousModeData      amData;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

static GlmsBool
unlocks_isParameterGetRspOk(union itc_msg *sig)
{
   if (!isSignalResultOk(sig->glmsPpGetRsp.result))
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Failed to read unlock stored parameters");
      return GLMS_FALSE;
   }

   if(sig->glmsPpGetRsp.index != 1)
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Unexpected table index when reading unlock stored data",
                 sig->glmsPpGetRsp.index);
   }

   return GLMS_TRUE;
}

static int32_t
unlocks_parseStoredParameters(union itc_msg *sig,
                              int32_t readStatus[],
                              int32_t(*exec)(union itc_msg *sig))
{
   int32_t table = getParameterTableFromSig(sig->sigNo);

   if ((sig->sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP) ||
       (sig->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP))
      readStatus[table] = common_handleGlmsAdpiXXIndexListRsp(sig, NULL, NULL);
   else if ((sig->sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP) ||
            (sig->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP))
      readStatus[table] = exec(sig);
   else
	   tracepoint(com_ericsson_glms, error_trace_w_hex_arg,
                  "Unknown signal in unlocks_parseStoredParameters",
                  sig->sigNo);

   if (readStatus[GLMS_PP] == 1 || readStatus[GLMS_PP] == -1)
   {
      readStatus[GLMS_PP] = 0;
      readStatus[GLMS_SP] = 0;
      return 1; /* All stored parameters are read */
   }

   return 2; /* We expect to read more stored parameters */
}

/*
 * Functions for EU data.
 *
 */
void
euDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "euDataInit");

   euData.activationState      = GLMS_ACTIVATION_STATE_INACTIVE;
   euData.expiration           = 0;
   euData.activationsLeft      = 2;
   euData.ppPendingUpdates     = GLMS_FALSE;
   euData.sendMoUpdateInd      = GLMS_FALSE;
   euData.readStatus[GLMS_SP]  = 0;
   euData.readStatus[GLMS_PP]  = 0;

   euData.euWarningTimerId     = 0;
   euData.euExpiryTimerId      = 0;
}

uint32_t
eu_getWarningTimerId()
{
  return euData.euWarningTimerId;
}

uint32_t
eu_getExpiryTimerId()
{
  return euData.euExpiryTimerId;
}

GlmsActivationState
eu_getActivationState()
{
   return euData.activationState;
}

void
eu_setActivationState(GlmsActivationState newState)
{
   if(euData.activationState != newState)
   {
      tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
                 "eu_setActivationState", newState);

      euData.activationState  = newState;
      euData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

time_t32
eu_getEuLength(void)
{
   return (GLMS_SECONDS_IN_SEVEN_DAYS / glms_getTimeScaling());
}

time_t32
eu_getDoubleEuLength(void)
{
   return (GLMS_SECONDS_IN_14_DAYS / glms_getTimeScaling());
}

time_t32
eu_getExpiration()
{
   return euData.expiration;
}

void
eu_setExpiration(time_t32 newExpiration)
{
   if(euData.expiration !=  newExpiration)
   {
      tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
                 "eu_setExpiration", newExpiration);

      euData.expiration       = newExpiration;
      euData.ppPendingUpdates = GLMS_TRUE;
      euData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

uint32_t
eu_getActivationsLeft()
{
   return euData.activationsLeft;
}

void
eu_setActivationsLeft(uint32_t newActivationsLeft)
{
   if(euData.activationsLeft != newActivationsLeft)
   {
      tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
                 "eu_setActivationsLeft", newActivationsLeft);

      euData.activationsLeft  = newActivationsLeft;
      euData.ppPendingUpdates = GLMS_TRUE;
      euData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

GlmsBool
eu_isEuActive()
{
   if(euData.activationState == GLMS_ACTIVATION_STATE_ACTIVATED ||
      euData.activationState == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
   {
      return GLMS_TRUE;
   }
   else
   {
      return GLMS_FALSE;
   }
}

void
eu_storeParameters()
{
   char *parameter;
   char  expiration_timetStr[20];
   char  activationsLeft_uint32tStrr[20];
   uint32_t paramSize;
   
   /* Store persistent parameters:
      euData.activationsLeft
      euData.expiration
   */

   if(euData.ppPendingUpdates == GLMS_TRUE)
   {
      paramSize = 0;
      sprintf(activationsLeft_uint32tStrr, "%"PRIu32, euData.activationsLeft);
      paramSize += strlen(activationsLeft_uint32tStrr);
      paramSize += 2; /* Delimiter */
      sprintf(expiration_timetStr, "%jd", (intmax_t)euData.expiration);
      paramSize += strlen(expiration_timetStr);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      strcat(parameter, activationsLeft_uint32tStrr);
      strcat(parameter, ";:");
      strcat(parameter, expiration_timetStr);

      pp_set(GLMS_TABLE_RID_EU, 1, parameter);

      euData.ppPendingUpdates = GLMS_FALSE;
      free(parameter);
   }

}


void
eu_fetchParameters()
{
   pp_requestIndexList(GLMS_TABLE_RID_EU);
}

/* handleGlmsAdpiXXGetRsp handles both
   GlmsAdpiSoftwareParameterGetRsp and
   GlmsAdpiPersistentParameterGetRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */
static int32_t
eu_handleGlmsAdpiXXGetRsp(union itc_msg *sig)
{
   char *paramStr, *parameter;
   uint32_t paramCount;

   int table = getParameterTableFromSig(sig->sigNo);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "eu_handleGlmsAdpiXXGetRsp", table);

   if (!unlocks_isParameterGetRspOk(sig))
      return 1;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);


   if(table == GLMS_PP)
   {
      if(paramCount < 1)
      {
         tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                    "The EuPersistentData parameter string "
                    "contains less than the expected 1 parameter delimiters",
                    paramStr);
      }
      else
      {
         /* Read parameters:
            euData.activationsLeft
            euData.expiration
         */
         parameter = getNextParameter(";:", paramStr);
         sscanf(parameter, "%"SCNu32, &euData.activationsLeft);

         parameter = getNextParameter(";:", NULL);
         euData.expiration = (time_t32)strtoll(parameter, NULL, 10);
      }
   }

   return 1; /* We have read all parameters of this parameter type so return 1. */
}


int32_t
eu_parseStoredParameters(union itc_msg *sig)
{
   return unlocks_parseStoredParameters(sig, euData.readStatus, eu_handleGlmsAdpiXXGetRsp);
}

void
eu_sendMoUpdateEuInd()
{
  if(adapter_isSubscribedForMoUpdateInd()
     && euData.sendMoUpdateInd
     && adapter_isAdpiActivated())
    {
        union itc_msg *sigSend;

        sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateEuInd),
                        GLMS_ADPI_MO_UPDATE_EU_IND);

        sigSend->glmsAdpiMoUpdateEuInd.activationState =
               eu_getActivationState();

        sigSend->glmsAdpiMoUpdateEuInd.expiration =
           eu_getExpiration();
        sigSend->glmsAdpiMoUpdateEuInd.activationsLeft =
           eu_getActivationsLeft();
        sendMsgToAdapter(sigSend);
    }
    euData.sendMoUpdateInd = GLMS_FALSE;
}

GlmsBool
eu_activateEu()
{
  struct timespec currentTime;

  tracepoint(com_ericsson_glms, call_to_function,
             "eu_activateEu");

  if(eu_getActivationsLeft() == 2)
  {
     if(!isTimerActive(GLMS_EU_EXPIRY_WARNING_TIMER))
     {
        euData.euWarningTimerId = requestTimer(eu_getEuLength() - glms_getWarningPeriod(),
                                               GLMS_FALSE, GLMS_EU_EXPIRY_WARNING_TIMER);
     }

     clock_gettime(CLOCK_REALTIME, &currentTime);
     eu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
     eu_setExpiration(currentTime.tv_sec + eu_getEuLength());
     eu_setActivationsLeft(1);
     eu_storeParameters();
     eu_sendMoUpdateEuInd();
     eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED, GLMS_ALARM_WARNING);

     lm_calculateLmState();
     lm_sendMoUpdateLmInd();

     return GLMS_TRUE;
  }

  if(eu_getActivationsLeft() == 1)
  {
     if(eu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED)
     {
        (void)deleteTimer(eu_getWarningTimerId());
        eu_setExpiration(eu_getExpiration() + eu_getEuLength());
     }
     else if(eu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
     {
        (void)deleteTimer(eu_getExpiryTimerId());
        eu_setExpiration(eu_getExpiration() + eu_getEuLength());
     }
     else /* GLMS_ACTIVATION_STATE_INACTIVE */
     {
        clock_gettime(CLOCK_REALTIME, &currentTime);
        eu_setExpiration(currentTime.tv_sec + eu_getEuLength());
     }

     eu_setActivationsLeft(0);
     eu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
     eu_storeParameters();
     eu_sendMoUpdateEuInd();
     eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED, GLMS_ALARM_MAJOR);

     lm_calculateLmState();
     lm_sendMoUpdateLmInd();

     if(!isTimerActive(GLMS_EU_EXPIRY_WARNING_TIMER))
     {
        clock_gettime(CLOCK_REALTIME, &currentTime);
        euData.euWarningTimerId = requestTimer(eu_getExpiration()
                                               - currentTime.tv_sec - glms_getWarningPeriod(),
                                               GLMS_FALSE,
                                               GLMS_EU_EXPIRY_WARNING_TIMER);
     }

     return GLMS_TRUE;
  }

  return GLMS_FALSE;
}

void
eu_handleEuExpiryWarningTimer()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "eu_handleEuExpiryWarningTimer");

   if(!isTimerActive(GLMS_EU_EXPIRY_TIMER))
   {
      euData.euExpiryTimerId = requestTimer(glms_getWarningPeriod(),
                                            GLMS_FALSE,
					    GLMS_EU_EXPIRY_TIMER);
   }

   eu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
   eu_storeParameters();
   eu_sendMoUpdateEuInd();

   glms_logEvent(GLMS_LOG_LOW,
                 "Emergency Unlock expiry warning received.");

   lm_calculateLmState();
   lm_sendMoUpdateLmInd();
}

void
eu_handleEuExpiryTimer()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "eu_handleEuExpiryTimer");

   if(eu_getActivationsLeft() == 0)
   {
      eu_setActivationState(GLMS_ACTIVATION_STATE_EXPIRED);
   }
   if(eu_getActivationsLeft() > 0)
   {
      eu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
   }

   eu_setExpiration(0);
   eu_storeParameters();
   eu_sendMoUpdateEuInd();

   glms_logEvent(GLMS_LOG_LOW,
                 "Emergency Unlock expired.");

   lm_calculateLmState(); /* Update Ind will be sent in lm_validateLicenses*/
   lm_validateLicenses();
}

void
eu_sendEuAlarmInd(GlmsAlarmState alarmState,GlmsAlarmSeverity alarmSeverity)
{
   union itc_msg *sig;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "eu_sendEuAlarmInd",
              alarmState);

   sig = itc_alloc(sizeof(GlmsAdpiEuAlarmInd),
               GLMS_ADPI_EU_ALARM_IND);
   sig->glmsAdpiEuAlarmInd.alarmState = alarmState;
   sig->glmsAdpiEuAlarmInd.alarmSeverity  = alarmSeverity;
   sendMsgToAdapter(sig);
}

void
eu_calculateActivationState()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "eu_calculateActivationState");

   clock_gettime(CLOCK_REALTIME, &currentTime);
   switch(eu_getActivationsLeft())
   {
      case 0:
         if(currentTime.tv_sec > eu_getExpiration())
         {
            eu_setActivationState(GLMS_ACTIVATION_STATE_EXPIRED);
         }
         else if(currentTime.tv_sec < (eu_getExpiration() - glms_getWarningPeriod()))
         {
            eu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
         }
         else if(currentTime.tv_sec >= (eu_getExpiration() - glms_getWarningPeriod()) &&
                 currentTime.tv_sec < eu_getExpiration())
         {
            eu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
         }
	 break;

      case 1:
         if(currentTime.tv_sec > eu_getExpiration())
         {
            eu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
         }
         else if(currentTime.tv_sec < (eu_getExpiration() - glms_getWarningPeriod()))
         {
            eu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
         }
         else if(currentTime.tv_sec >= (eu_getExpiration() - glms_getWarningPeriod()) &&
                 currentTime.tv_sec < eu_getExpiration())
         {
            eu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
         }
	 break;

      case 2:
         if(!(eu_getExpiration()))
         {
            eu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
         }
	 else
	 {
            tracepoint(com_ericsson_glms, error_trace,
                       "EU expiration is not 0 but actionsLeft is 2");
	 }

	 break;

      default:
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown EU activation left",
                    eu_getActivationsLeft());
   }

   eu_sendMoUpdateEuInd();
}

void
eu_handleGlmsRestart()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "eu_handleGlmsRestart");

   switch(eu_getActivationState())
   {
      case GLMS_ACTIVATION_STATE_INACTIVE:
         if(eu_getActivationsLeft() == 1)
	 eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED,GLMS_ALARM_WARNING);
         return;

      case GLMS_ACTIVATION_STATE_ACTIVATED:
         if(!isTimerActive(GLMS_EU_EXPIRY_WARNING_TIMER))
         {
            clock_gettime(CLOCK_REALTIME, &currentTime);
            euData.euWarningTimerId = requestTimer((eu_getExpiration())
                                                   - currentTime.tv_sec
                                                   - glms_getWarningPeriod(),
                                                   GLMS_FALSE,
                                                   GLMS_EU_EXPIRY_WARNING_TIMER);
         }
	 if(eu_getActivationsLeft() == 0)
	 {
	 eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED,GLMS_ALARM_MAJOR);
	 }
	 else if(eu_getActivationsLeft() == 1)
	 eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED,GLMS_ALARM_WARNING);
	 return;

      case GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING:
         if(!isTimerActive(GLMS_EU_EXPIRY_TIMER))
         {
            clock_gettime(CLOCK_REALTIME, &currentTime);
            euData.euExpiryTimerId = requestTimer((eu_getExpiration())
                                                  - currentTime.tv_sec,
                                                  GLMS_FALSE,
                                                  GLMS_EU_EXPIRY_TIMER);
         }
	 if(eu_getActivationsLeft() == 0)
	 {
	 eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED,GLMS_ALARM_MAJOR);
	 }
	 else if(eu_getActivationsLeft() == 1)
	 eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED,GLMS_ALARM_WARNING);
	 return;

      case GLMS_ACTIVATION_STATE_EXPIRED:
	 eu_sendEuAlarmInd(GLMS_ALARM_ACTIVATED,GLMS_ALARM_MAJOR);
	 return;

      default:
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown EU activation state",
                    eu_getActivationState());
   }
}

GlmsBool
eu_getPpPendingUpdates()
{
   return euData.ppPendingUpdates;
}

GlmsBool
eu_getSendMoUpdateInd()
{
   return euData.sendMoUpdateInd;
}

int32_t
eu_getStartupReadStatus(uint32_t elem)
{
   return euData.readStatus[elem];
}

void
eu_reset()
{
   if(eu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED)
      (void)deleteTimer(eu_getWarningTimerId());

   if(eu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
      (void)deleteTimer(eu_getExpiryTimerId());

   eu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
   eu_setExpiration(0);
   eu_setActivationsLeft(2);
   eu_storeParameters();
   eu_sendMoUpdateEuInd();
   eu_sendEuAlarmInd(GLMS_ALARM_CEASED,GLMS_ALARM_NONE);

   glms_logEvent(GLMS_LOG_LOW, "Emergency Unlock is reset.");
}

/*
 * Functions for IU data.
 *
 */
void
iuDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "iuDataInit");

   iuData.activationState      = GLMS_ACTIVATION_STATE_INACTIVE;
   iuData.expiration           = 0;
   iuData.activationsLeft      = 1;
   iuData.ppPendingUpdates     = GLMS_FALSE;
   iuData.sendMoUpdateInd      = GLMS_FALSE;
   iuData.readStatus[GLMS_SP]  = 0;
   iuData.readStatus[GLMS_PP]  = 0;

   iuData.iuWarningTimerId     = 0;
   iuData.iuExpiryTimerId      = 0;
}

uint32_t
iu_getWarningTimerId()
{
  return iuData.iuWarningTimerId;
}

uint32_t
iu_getExpiryTimerId()
{
  return iuData.iuExpiryTimerId;
}

GlmsActivationState
iu_getActivationState()
{
   return iuData.activationState;
}

void
iu_setActivationState(GlmsActivationState newState)
{
   if(iuData.activationState != newState)
   {
      tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
                 "iu_setActivationState",
                 newState);
      iuData.activationState  = newState;
      iuData.sendMoUpdateInd  = GLMS_TRUE;
   }
}


time_t32
iu_getIuLength(void)
{
   return (GLMS_SECONDS_IN_TWENTYONE_DAYS / glms_getTimeScaling());
}

time_t32
iu_getExpiration()
{
   return iuData.expiration;
}

void
iu_setExpiration(time_t32 newExpiration)
{
   if(iuData.expiration !=  newExpiration)
   {
      tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
                 "iu_setExpiration",
                 newExpiration);
      iuData.expiration       = newExpiration;
      iuData.ppPendingUpdates = GLMS_TRUE;
      iuData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

uint32_t
iu_getActivationsLeft()
{
   return iuData.activationsLeft;
}

void
iu_setActivationsLeft(uint32_t newActivationsLeft)
{
   if(iuData.activationsLeft != newActivationsLeft)
   {
      tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
                 "iu_setActivationsLeft",
                 newActivationsLeft);
      iuData.activationsLeft  = newActivationsLeft;
      iuData.ppPendingUpdates = GLMS_TRUE;
      iuData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

GlmsBool
iu_isIuActive()
{
   if(iuData.activationState == GLMS_ACTIVATION_STATE_ACTIVATED ||
      iuData.activationState == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
   {
      return GLMS_TRUE;
   }
   else
   {
      return GLMS_FALSE;
   }
}

void
iu_storeParameters()
{
   char *parameter;
   char  expiration_timetStr[20];
   char  activationsLeft_uint32tStrr[20];
   uint32_t paramSize;

   /* Store persistent parameters:
      iuData.activationsLeft
      iuData.expiration
   */

   if(iuData.ppPendingUpdates == GLMS_TRUE)
   {
      paramSize = 0;
      sprintf(activationsLeft_uint32tStrr, "%"PRIu32, iuData.activationsLeft);
      paramSize += strlen(activationsLeft_uint32tStrr);
      paramSize += 2; /* Delimiter */
      sprintf(expiration_timetStr, "%jd", (intmax_t)iuData.expiration);
      paramSize += strlen(expiration_timetStr);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      strcat(parameter, activationsLeft_uint32tStrr);
      strcat(parameter, ";:");
      strcat(parameter, expiration_timetStr);
      pp_set(GLMS_TABLE_RID_IU, 1, parameter);

      iuData.ppPendingUpdates = GLMS_FALSE;
      free(parameter);
   }

}


void
iu_fetchParameters()
{
   pp_requestIndexList(GLMS_TABLE_RID_IU);
}

/* handleGlmsAdpiXXGetRsp handles both
   GlmsAdpiSoftwareParameterGetRsp and
   GlmsAdpiPersistentParameterGetRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */
static int32_t
iu_handleGlmsAdpiXXGetRsp(union itc_msg *sig)
{
   char *paramStr, *parameter;
   uint32_t paramCount;

   int table = getParameterTableFromSig(sig->sigNo);

   if (!unlocks_isParameterGetRspOk(sig))
      return 1;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);


   if(table == GLMS_PP)
   {
      if(paramCount < 1)
      {
         tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                    "The IUPersistentData parameter string " \
                    "contains less than the expected 1 parameter delimiters",
                    paramStr);
      }
      else
      {
         /* Read parameters:
            iuData.activationsLeft
            iuData.expiration
         */
         parameter = getNextParameter(";:", paramStr);
         sscanf(parameter, "%"SCNu32, &iuData.activationsLeft);

         parameter = getNextParameter(";:", NULL);
         iuData.expiration = (time_t32)strtoll(parameter, NULL, 10);
      }
   }

   return 1; /* We have read all parameters of this parameter type so return 1. */
}


int32_t
iu_parseStoredParameters(union itc_msg *sig)
{
   return unlocks_parseStoredParameters(sig, iuData.readStatus, iu_handleGlmsAdpiXXGetRsp);
}

void
iu_sendMoUpdateIuInd()
{
  if(adapter_isSubscribedForMoUpdateInd() &&
     iuData.sendMoUpdateInd &&
     adapter_isAdpiActivated())
    {
        union itc_msg *sigSend;
        sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateIuInd),
                        GLMS_ADPI_MO_UPDATE_IU_IND);

        sigSend->glmsAdpiMoUpdateIuInd.activationState =
               iu_getActivationState();

        sigSend->glmsAdpiMoUpdateIuInd.expiration =
           iu_getExpiration();
        sigSend->glmsAdpiMoUpdateIuInd.activationsLeft =
           iu_getActivationsLeft();
        sendMsgToAdapter(sigSend);
    }
    iuData.sendMoUpdateInd = GLMS_FALSE;
}

GlmsBool
iu_activateIu()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "iu_activateIu");

   if(keyFile_getSequenceNumber() > 0 ||
      iu_getActivationState() == GLMS_ACTIVATION_STATE_EXPIRED)
   {
      return GLMS_FALSE;
   }

   if(iu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED ||
      iu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
   {
      return GLMS_TRUE;
   }

   if(keyFile_getSequenceNumber() == 0 &&
      iu_getActivationState() == GLMS_ACTIVATION_STATE_INACTIVE)
   {
      if(!isTimerActive(GLMS_IU_EXPIRY_WARNING_TIMER))
      {
         iuData.iuWarningTimerId = requestTimer(iu_getIuLength() - glms_getWarningPeriod(),
                                                GLMS_FALSE,
                                                GLMS_IU_EXPIRY_WARNING_TIMER);
      }

      clock_gettime(CLOCK_REALTIME, &currentTime);
      iu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
      iu_setExpiration((currentTime.tv_sec + iu_getIuLength()));
      iu_setActivationsLeft(iu_getActivationsLeft()-1);
      iu_storeParameters();
      iu_sendMoUpdateIuInd();

      lm_calculateLmState();
      lm_sendMoUpdateLmInd();

      return GLMS_TRUE;
   }

   return GLMS_FALSE;
}

void
iu_handleIuExpiryWarningTimer()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "iu_handleIuExpiryWarningTimer");

   if(!isTimerActive(GLMS_IU_EXPIRY_TIMER))
   {
      iuData.iuExpiryTimerId = requestTimer(glms_getWarningPeriod(),
                                            GLMS_FALSE,
					    GLMS_IU_EXPIRY_TIMER);
   }

   iu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
   iu_storeParameters();
   iu_sendMoUpdateIuInd();

   glms_logEvent(GLMS_LOG_LOW,
                 "Integration Unlock expiry warning received.");

   lm_calculateLmState();
   lm_sendMoUpdateLmInd();
}

void
iu_handleIuExpiryTimer()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "iu_handleIuExpiryTimer");

   iu_setActivationState(GLMS_ACTIVATION_STATE_EXPIRED);
   iu_setExpiration(0);

   iu_storeParameters();
   iu_sendMoUpdateIuInd();

   glms_logEvent(GLMS_LOG_LOW,
                 "Integration Unlock expired.");

   lm_calculateLmState(); /* Update Ind will be sent in  lm_validateLicenses*/
   lm_validateLicenses();
}

void
iu_calculateActivationState()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "iu_calculateActivationState");

   if(keyFile_getSequenceNumber() > 0)
   {
      iu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
   }
   else
   {
      clock_gettime(CLOCK_REALTIME, &currentTime);
      switch(iu_getActivationsLeft())
      {
         case 0:
            if(currentTime.tv_sec > iu_getExpiration())
            {
               iu_setActivationState(GLMS_ACTIVATION_STATE_EXPIRED);
            }
            else if(currentTime.tv_sec < (iu_getExpiration() - glms_getWarningPeriod()))
            {
               iu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
            }
            else if(currentTime.tv_sec >= (iu_getExpiration() - glms_getWarningPeriod()) &&
                    currentTime.tv_sec < iu_getExpiration())
            {
               iu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
            }
            break;

         case 1:
            iu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
            break;

         default:
            tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                       "Unknown IU activations left",
                       iu_getActivationsLeft());
      }
   }

   iu_sendMoUpdateIuInd();
}

void
iu_handleGlmsRestart()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "iu_handleGlmsRestart");

   switch(iu_getActivationState())
   {
      case GLMS_ACTIVATION_STATE_INACTIVE:
         return;

      case GLMS_ACTIVATION_STATE_ACTIVATED:
         if(!isTimerActive(GLMS_IU_EXPIRY_WARNING_TIMER))
         {
            clock_gettime(CLOCK_REALTIME, &currentTime);
            iuData.iuWarningTimerId = requestTimer((iu_getExpiration())
                                                   - currentTime.tv_sec
                                                   - glms_getWarningPeriod(),
                                                   GLMS_FALSE,
                                                   GLMS_IU_EXPIRY_WARNING_TIMER);
         }

         return;

      case GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING:
         if(!isTimerActive(GLMS_IU_EXPIRY_TIMER))
         {
            clock_gettime(CLOCK_REALTIME, &currentTime);
            iuData.iuExpiryTimerId = requestTimer((iu_getExpiration())
                                                  - currentTime.tv_sec,
                                                  GLMS_FALSE,
                                                  GLMS_IU_EXPIRY_TIMER);
         }

	 return;

      case GLMS_ACTIVATION_STATE_EXPIRED:
         return;

      default:
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown IU activation state",
                    iu_getActivationState());
   }
}

GlmsBool
iu_getPpPendingUpdates()
{
   return iuData.ppPendingUpdates;
}

GlmsBool
iu_getSendMoUpdateInd()
{
   return iuData.sendMoUpdateInd;
}

int32_t
iu_getStartupReadStatus(uint32_t elem)
{
   return iuData.readStatus[elem];
}

void
iu_reset()
{
   if(iu_isIuActive())
   {
      if(iu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED)
         (void)deleteTimer(iu_getWarningTimerId());

      if(iu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
         (void)deleteTimer(iu_getExpiryTimerId());

      iu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
      iu_setExpiration(0);

      glms_logEvent(GLMS_LOG_LOW, "Integration Unlock is ceased.");
   }

   iu_setActivationsLeft(0);
   iu_storeParameters();
   iu_sendMoUpdateIuInd();
}


/*
 * Functions for PU data.
 *
 */
void
puDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "puDataInit");

   puData.activationState      = GLMS_ACTIVATION_STATE_INACTIVE;
   puData.expiration           = 0;
   puData.activationsLeft      = 1;
   puData.puDeactivated        = 0;
   puData.ppPendingUpdates     = GLMS_FALSE;
   puData.readStatus[GLMS_SP]  = 0;
   puData.readStatus[GLMS_PP]  = 0;
   puData.puExpiryTimerId      = 0;
}

uint32_t
pu_getExpiryTimerId()
{
  return puData.puExpiryTimerId;
}

GlmsActivationState
pu_getActivationState()
{
   return puData.activationState;
}

void
pu_setActivationState(GlmsActivationState newState)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "pu_setActivationState", newState);
   if(puData.activationState != newState)
   {

       puData.activationState  = newState;
   }
}

time_t32
pu_getExpiration()
{
   return puData.expiration;
}

void
pu_setExpiration(time_t32 newExpiration)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "pu_setExpiration", newExpiration);

   if(puData.expiration !=  newExpiration)
   {

       puData.expiration       = newExpiration;
       puData.ppPendingUpdates = GLMS_TRUE;
   }
}

time_t32
pu_getPuLength(void)
{
   return (GLMS_SECONDS_IN_SEVEN_DAYS / glms_getTimeScaling());
}

uint32_t
pu_getActivationsLeft()
{
   return puData.activationsLeft;
}

void
pu_setActivationsLeft(uint32_t newActivationsLeft)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "pu_setActivationsLeft",
              newActivationsLeft);
   if(puData.activationsLeft != newActivationsLeft)
   {
      puData.activationsLeft  = newActivationsLeft;
      puData.ppPendingUpdates = GLMS_TRUE;
   }
}

uint32_t
pu_getPuDeactivated()
{
   return puData.puDeactivated;
}

void
pu_setPuDeactivated(uint32_t newPuDeactivated)
{
    tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
               "pu_setPuDeactivated",
                newPuDeactivated);
    if(puData.puDeactivated != newPuDeactivated)
   {
      puData.puDeactivated  = newPuDeactivated;
      puData.ppPendingUpdates = GLMS_TRUE;
   }
}

GlmsBool
pu_isPuActive()
{
   if(puData.activationState == GLMS_ACTIVATION_STATE_ACTIVATED )
   {
      return GLMS_TRUE;
   }
   else
   {
      return GLMS_FALSE;
   }
}

void
pu_storeParameters()
{
   char *parameter;
   char  expiration_timetStr[20];
   char  activationsLeft_uint32tStrr[20];
   char  puDeactivated_uint32tStr[20];
   uint32_t paramSize;

   /* Store persistent parameters:
      puData.activationsLeft
      puData.puDeactivated
      puData.expiration
   */

   if(puData.ppPendingUpdates == GLMS_TRUE)
   {
      paramSize = 0;
      sprintf(activationsLeft_uint32tStrr, "%"PRIu32, puData.activationsLeft);
      paramSize += strlen(activationsLeft_uint32tStrr);
      paramSize += 2; /* Delimiter */
      sprintf(puDeactivated_uint32tStr, "%"PRIu32, puData.puDeactivated);
      paramSize += strlen(puDeactivated_uint32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(expiration_timetStr, "%jd", (intmax_t)puData.expiration);
      paramSize += strlen(expiration_timetStr);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      strcat(parameter, activationsLeft_uint32tStrr);
      strcat(parameter, ";:");
      strcat(parameter, puDeactivated_uint32tStr);
      strcat(parameter, ";:");
      strcat(parameter, expiration_timetStr);
      pp_set(GLMS_TABLE_RID_PU, 1, parameter);

      puData.ppPendingUpdates = GLMS_FALSE;
      free(parameter);
   }

}


void
pu_fetchParameters()
{
   pp_requestIndexList(GLMS_TABLE_RID_PU);
}



/* handleGlmsAdpiXXIndexListRsp handles both
   GlmsAdpiSoftwareParameterIndexListRsp and
   GlmsAdpiPersistentParameterIndexListRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */

/* handleGlmsAdpiXXGetRsp handles both
   GlmsAdpiSoftwareParameterGetRsp and
   GlmsAdpiPersistentParameterGetRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */
static int32_t
pu_handleGlmsAdpiXXGetRsp(union itc_msg *sig)
{
   char *paramStr, *parameter;
   uint32_t paramCount;

   int table = getParameterTableFromSig(sig->sigNo);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "pu_handleGlmsAdpiXXGetRsp",
              table);

   if (!unlocks_isParameterGetRspOk(sig))
      return 1;


   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);


   if(table == GLMS_PP)
   {
      if(paramCount < 2)
      {
         tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                    "The PUPersistentData parameter string " \
                    "contains less than the expected 2 parameter delimiters",
                    paramStr);
      }
      else
      {
         /* Read parameters:
            puData.activationsLeft
            puData.puDeactivated
            puData.expiration
         */
         parameter = getNextParameter(";:", paramStr);
         sscanf(parameter, "%"SCNu32, &puData.activationsLeft);

         parameter = getNextParameter(";:", NULL);
         sscanf(parameter, "%"SCNu32, &puData.puDeactivated);

         parameter = getNextParameter(";:", NULL);
         puData.expiration = (time_t32)strtoll(parameter, NULL, 10);
      }
   }

   return 1; /* We have read all parameters of this parameter type so return 1. */
}


int32_t
pu_parseStoredParameters(union itc_msg *sig)
{
   return unlocks_parseStoredParameters(sig, puData.readStatus, pu_handleGlmsAdpiXXGetRsp);
}

GlmsBool
pu_activatePu()
{
  struct timespec currentTime;

  tracepoint(com_ericsson_glms, call_to_function,
             "pu_activatePu");

  if( (keyFile_getSequenceNumber() > 0)
     ||(pu_getActivationState() == GLMS_ACTIVATION_STATE_EXPIRED)
     || pu_getPuDeactivated())
  {
     return GLMS_FALSE;
  }

  if(pu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED)
  {
     return GLMS_TRUE;
  }

  if((keyFile_getSequenceNumber() == 0) &&
     (pu_getActivationState() == GLMS_ACTIVATION_STATE_INACTIVE))
  {

     if(!isTimerActive(GLMS_PU_EXPIRY_TIMER))
     {
        puData.puExpiryTimerId = requestTimer(pu_getPuLength(),
                                              GLMS_FALSE,
                                              GLMS_PU_EXPIRY_TIMER);
     }
     
     clock_gettime(CLOCK_REALTIME, &currentTime);
     pu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
     pu_setExpiration((currentTime.tv_sec + pu_getPuLength()));
     pu_setActivationsLeft(pu_getActivationsLeft()-1);
     pu_storeParameters();
     
     lm_calculateLmState();
     lm_sendMoUpdateLmInd();

     return GLMS_TRUE;
  }

  return GLMS_FALSE;
}

GlmsBool
pu_deactivatePu()
{
  tracepoint(com_ericsson_glms, call_to_function,
             "pu_deactivatePu");

  if(isTimerActive(GLMS_PU_EXPIRY_TIMER))
  {
     (void)deleteTimer(pu_getExpiryTimerId());
  }
  
  pu_setActivationState(GLMS_ACTIVATION_STATE_DEACTIVATED);
  pu_setExpiration(0);
  pu_setPuDeactivated(1);
  pu_storeParameters();
  
  lm_calculateLmState();
  lm_sendMoUpdateLmInd();
  
  return GLMS_TRUE;
}

void
pu_handlePuExpiryTimer()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "pu_handlePuExpiryTimer");

   pu_setActivationState(GLMS_ACTIVATION_STATE_EXPIRED);
   pu_setExpiration(0);
   
   pu_storeParameters();
   
   lm_calculateLmState(); /* Update Ind will be sent in  lm_validateLicenses*/
   lm_validateLicenses();
}

void
pu_calculateActivationState()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "pu_calculateActivationState");

   clock_gettime(CLOCK_REALTIME, &currentTime);
   switch(pu_getActivationsLeft())
   {
      case 0:
         if( pu_getPuDeactivated() )
	 {
	    pu_setActivationState(GLMS_ACTIVATION_STATE_DEACTIVATED);
	 }
         else if(!pu_getExpiration())
         {
            pu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
         }
         else if(currentTime.tv_sec > pu_getExpiration())
         {
            pu_setActivationState(GLMS_ACTIVATION_STATE_EXPIRED);
         }
         else
         {
            pu_setActivationState(GLMS_ACTIVATION_STATE_ACTIVATED);
         }
	 return;

      case 1:
         if( pu_getPuDeactivated() )
	 {
	    pu_setActivationState(GLMS_ACTIVATION_STATE_DEACTIVATED);
	 }
	 else
            pu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);

	 return;

      default:
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown PU activation left",
                    pu_getActivationsLeft());
   }


}

void
pu_handleGlmsRestart()
{
    struct timespec currentTime;

    tracepoint(com_ericsson_glms, call_to_function,
              "pu_handleGlmsRestart");

    switch(pu_getActivationState())
    {
       case GLMS_ACTIVATION_STATE_ACTIVATED:
          if(!isTimerActive(GLMS_PU_EXPIRY_TIMER))
          {
             clock_gettime(CLOCK_REALTIME, &currentTime);
             puData.puExpiryTimerId = requestTimer((pu_getExpiration())
                                                   - currentTime.tv_sec,
                                                   GLMS_FALSE,
                                                   GLMS_PU_EXPIRY_TIMER);
          }
          break;
          
       case GLMS_ACTIVATION_STATE_INACTIVE:
       case GLMS_ACTIVATION_STATE_DEACTIVATED:
       case GLMS_ACTIVATION_STATE_EXPIRED:
          break;
          
      default:
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown PU activation state",
                    pu_getActivationState());
   }
}

GlmsBool
pu_getPpPendingUpdates()
{
   return puData.ppPendingUpdates;
}


int32_t
pu_getStartupReadStatus(uint32_t elem)
{
   return puData.readStatus[elem];
}

void
pu_reset()
{
   if(pu_isPuActive())
   {
      if(pu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED)
         (void)deleteTimer(pu_getExpiryTimerId());

      pu_setActivationState(GLMS_ACTIVATION_STATE_INACTIVE);
      pu_setExpiration(0);
   }

   pu_setActivationsLeft(0);
   pu_storeParameters();
}

/*
 * Functions for AM data.
 *
 */
void
amDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "amDataInit");

   amData.expiration           = 0;
   amData.ppPendingUpdates     = GLMS_FALSE;
   amData.sendMoUpdateInd      = GLMS_FALSE;
   amData.readStatus[GLMS_SP]  = 0;
   amData.readStatus[GLMS_PP]  = 0;
   amData.amExpiryTimerId      = 0;
}

uint32_t
am_getExpiryTimerId()
{
  return amData.amExpiryTimerId;
}

GlmsActivationState
am_getActivationState()
{
   if(amData.expiration != 0)
   {
      return GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING;
   }
   else
   {
      return GLMS_ACTIVATION_STATE_INACTIVE;
   }
}

time_t32
am_getExpiration()
{
   return amData.expiration;
}

time_t32
am_getAmLength()
{
   return (GLMS_SECONDS_IN_36_HOURS / glms_getTimeScaling());
}

void
am_storeParameters()
{
   char *parameter;
   char  expiration_timetStr[20];
   uint32_t paramSize;

   /* Store persistent parameters:
      amData.expiration
   */

   if(amData.ppPendingUpdates == GLMS_TRUE)
   {
      paramSize = 0;
      sprintf(expiration_timetStr, "%jd", (intmax_t)amData.expiration);
      paramSize += strlen(expiration_timetStr);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      strcat(parameter, expiration_timetStr);

      pp_set(GLMS_TABLE_RID_AM, 1, parameter);

      amData.ppPendingUpdates = GLMS_FALSE;
      free(parameter);
   }

}


void
am_fetchParameters()
{
   pp_requestIndexList(GLMS_TABLE_RID_AM);
}


/* handleGlmsAdpiXXIndexListRsp handles both
   GlmsAdpiSoftwareParameterIndexListRsp and
   GlmsAdpiPersistentParameterIndexListRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */


/* handleGlmsAdpiXXGetRsp handles both
   GlmsAdpiSoftwareParameterGetRsp and
   GlmsAdpiPersistentParameterGetRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */
static int32_t
am_handleGlmsAdpiXXGetRsp(union itc_msg *sig)
{
   char *paramStr, *parameter;

   int table = getParameterTableFromSig(sig->sigNo);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "am_handleGlmsAdpiXXGetRsp", table);

   if (!unlocks_isParameterGetRspOk(sig))
      return 1;


   paramStr = sig->glmsPpGetRsp.value;

   if(table == GLMS_PP)
   {
      /* Read parameters:
	 amData.expiration
      */
      parameter = getNextParameter(";:", paramStr);
      amData.expiration = (time_t32)strtoll(parameter, NULL, 10);
   }

   return 1; /* We have read all parameters of this parameter type so return 1. */
}


int32_t
am_parseStoredParameters(union itc_msg *sig)
{
   return unlocks_parseStoredParameters(sig, amData.readStatus, am_handleGlmsAdpiXXGetRsp);
}


void
am_sendMoUpdateAmInd()
{
   union itc_msg *sigSend;

   if(adapter_isSubscribedForMoUpdateInd()
      && amData.sendMoUpdateInd
      && adapter_isAdpiActivated())
   {

      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateAmInd),
                      GLMS_ADPI_MO_UPDATE_AM_IND);
      sigSend->glmsAdpiMoUpdateAmInd.activationState =
         am_getActivationState();
      sigSend->glmsAdpiMoUpdateAmInd.expiration =
         am_getExpiration();
      sendMsgToAdapter(sigSend);
   }
   amData.sendMoUpdateInd = GLMS_FALSE;
}


void
am_activateAutonomousMode()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "am_activateAutonomousMode");
   
   if(am_getActivationState() == GLMS_ACTIVATION_STATE_INACTIVE)
   {
      amData.amExpiryTimerId = requestTimer(am_getAmLength(),
                                            GLMS_FALSE,
                                            GLMS_AM_EXPIRY_TIMER);
      
      clock_gettime(CLOCK_REALTIME, &currentTime);
      amData.expiration = currentTime.tv_sec + am_getAmLength();
      amData.ppPendingUpdates = GLMS_TRUE;
      amData.sendMoUpdateInd  = GLMS_TRUE;
      
      am_storeParameters();
      am_sendMoUpdateAmInd();
      am_sendAmAlarmInd(GLMS_ALARM_ACTIVATED);
      
      glms_logEvent(GLMS_LOG_LOW,
                    "Autonomous Mode activated.");
      
      lm_calculateLmState();
      lm_sendMoUpdateLmInd();
   }
}


void
am_ceaseAutonomousMode()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "am_ceaseAutonomousMode");

   if(am_getActivationState() != GLMS_ACTIVATION_STATE_INACTIVE)
   {
     if(isTimerActive(GLMS_AM_EXPIRY_TIMER))
     {
        deleteTimer(amData.amExpiryTimerId);
     }

     amData.expiration       = 0;
     amData.amExpiryTimerId  = 0;
     amData.ppPendingUpdates = GLMS_TRUE;
     amData.sendMoUpdateInd  = GLMS_TRUE;

     am_storeParameters();
     am_sendMoUpdateAmInd();
     am_sendAmAlarmInd(GLMS_ALARM_CEASED);

     glms_logEvent(GLMS_LOG_LOW,
                   "Autonomous Mode ceased.");

     lm_calculateLmState();
     lm_sendMoUpdateLmInd();
   }
}


void
am_handleAmExpiryTimer()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "am_handleAmExpiryTimer");

   am_ceaseAutonomousMode();
   lm_validateLicenses();
}


void
am_sendAmAlarmInd(GlmsAlarmState alarmState)
{
   union itc_msg *sig;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "am_sendAmAlarmInd",
              alarmState);

   sig = itc_alloc(sizeof(GlmsAdpiAmAlarmInd),
               GLMS_ADPI_AM_ALARM_IND);
   sig->glmsAdpiAmAlarmInd.alarmState     = alarmState;
   sendMsgToAdapter(sig);
}


void
am_handleGlmsRestart()
{
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "am_handleGlmsRestart");

   clock_gettime(CLOCK_REALTIME, &currentTime);
   if(amData.expiration != 0)
   {
      if(amData.expiration < currentTime.tv_sec)
      {
         amData.expiration        = 0;
         amData.ppPendingUpdates  = GLMS_TRUE;
         amData.sendMoUpdateInd   = GLMS_TRUE;

         am_storeParameters();
         am_sendMoUpdateAmInd();
      }
      else
      {
         if((amData.expiration - currentTime.tv_sec) > am_getAmLength())
         {
            tracepoint(com_ericsson_glms, info_trace,
                       "Autonomous mode with period longer than 36 hours at startup."
                       " Resetting period to 36 hours.");

            amData.expiration = currentTime.tv_sec + am_getAmLength();
            amData.ppPendingUpdates  = GLMS_TRUE;
            amData.sendMoUpdateInd   = GLMS_TRUE;

            am_storeParameters();
            am_sendMoUpdateAmInd();
         }

         if(!isTimerActive(GLMS_AM_EXPIRY_TIMER))
         {
            amData.amExpiryTimerId = requestTimer(amData.expiration,
                                                  GLMS_FALSE,
                                                  GLMS_AM_EXPIRY_TIMER);
         }

         am_sendAmAlarmInd(GLMS_ALARM_ACTIVATED);
      }

      lm_calculateLmState();
      lm_sendMoUpdateLmInd();
   }
}

GlmsBool
am_getPpPendingUpdates()
{
   return amData.ppPendingUpdates;
}

GlmsBool
am_getSendMoUpdateInd()
{
   return amData.sendMoUpdateInd;
}

int32_t
am_getStartupReadStatus(uint32_t elem)
{
   return amData.readStatus[elem];
}

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

#include "com_ericsson_glms.h"

#include "glmsadpi/glmsDataTypes.h"
#include "glmsadpi/glms_adpi.sig"
#include "glms_main.h"
#include "glmsUtils.h"
#include "persistentStorage.h"
#include "data_lm.h"
#include "data_keyFile.h"
#include "data_featureKey.h"
#include "data_capacityKey.h"
#include "data_unlocks.h"
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
   GlmsAdpiMoUpdateLmInd                    glmsAdpiMoUpdateLmInd;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

LmData lmData;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
lmDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "lmDataInit");

   lmData.fingerprint[0]               = '\0'; /* Software Parameter */
   lmData.lastInventoryChange          = 0;    /* Persistent Parameter */
   lmData.lastLicenseInventoryRefresh  = 0;    /* Persistent Parameter */
   lmData.referenceToLicenseServer     = '\0';
   lmData.lmState                      = GLMS_LOCKED;

   /* Interna data */
   lmData.ppPendingUpdates             = GLMS_FALSE;
   lmData.spPendingUpdates             = GLMS_FALSE;
   lmData.sendMoUpdateInd              = GLMS_FALSE;
   lmData.readStatus[GLMS_SP]          = 0;
   lmData.readStatus[GLMS_PP]          = 0;
   lmData.featureConfList              = 0;
   lmData.featureConfListLen           = 0;
}

char*
lm_getFingerprint()
{
   return lmData.fingerprint;
}

void
lm_setFingerprint(char *newFingerprint)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "lm_setFingerprint", newFingerprint);

   if(strcmp(lmData.fingerprint, newFingerprint) != 0)
   {
      strncpy(lmData.fingerprint,newFingerprint,GLMS_FINGERPRINT_LEN);
      lmData.fingerprint[GLMS_FINGERPRINT_LEN - 1] = '\0';
      lmData.spPendingUpdates = GLMS_TRUE;
      lmData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

GlmsBool
lm_getFingerprintUpdatable()
{
   if(keyFile_getSequenceNumber() == 0)
      return GLMS_TRUE;
   else
      return GLMS_FALSE;
}

GlmsLmState
lm_getLmState()
{
   return lmData.lmState;
}

void
lm_setLmState(GlmsLmState newState)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "lm_setLmState", newState);

   if(lmData.lmState != newState)
   {
      lmData.lmState          = newState;
      lmData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

void
lm_calculateLmState()
{
   if(am_getActivationState() != GLMS_ACTIVATION_STATE_INACTIVE)
   {
      lm_setLmState(GLMS_AUTONOMOUS_MODE);
   }
   else if(iu_isIuActive())
   {
      lm_setLmState(GLMS_INTEGRATION_UNLOCK);
   }
   else if(eu_isEuActive())
   {
      lm_setLmState(GLMS_EMERGENCY_UNLOCK);
   }
   /* sequence number over 0 mean an kf has been read */
   else if(keyFile_getSequenceNumber())
   {
      lm_setLmState(GLMS_NORMAL);
   }
   else
   {
      lm_setLmState(GLMS_LOCKED);
   }
}

time_t32
lm_getLastInventoryChange()
{
   return lmData.lastInventoryChange;
}

void
lm_setLastInventoryChange(time_t32 lastInventoryChange)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "lm_setLastInventoryChange", lastInventoryChange);

   if(lmData.lastInventoryChange != lastInventoryChange)
   {
      lmData.lastInventoryChange = lastInventoryChange;
      lmData.ppPendingUpdates = GLMS_TRUE;
      lmData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

time_t32
lm_getLastInventoryRefresh()
{
   return lmData.lastLicenseInventoryRefresh;
}

void
lm_setLastInventoryRefresh(time_t32 lastInventoryRefresh)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "lm_setLastInventoryRefresh", lastInventoryRefresh);

   if(lmData.lastLicenseInventoryRefresh != lastInventoryRefresh)
   {
      lmData.lastLicenseInventoryRefresh = lastInventoryRefresh;
      lmData.ppPendingUpdates = GLMS_TRUE;
      lmData.sendMoUpdateInd  = GLMS_TRUE;
   }
}

char
lm_getReferenceToLicenseServer()
{
   return lmData.referenceToLicenseServer;
}

void
lm_validateLicenses()
{
   FeatureState  *fs, *nextfs;
   CapacityState *cs, *nextCs;
   GlmsBool inventoryUpdated = GLMS_FALSE;
   struct timespec currentTime;
   uint32_t updateResult = 0;

   tracepoint(com_ericsson_glms, call_to_function,
              "lm_validateLicenses");

   inventoryUpdated = GLMS_FALSE;
   for(fs = featureState_getFirst();
       fs != NULL;
       fs = nextfs)
   {
      /* updateKeyState may delete the current fs, so read the next fs now */
      nextfs = featureState_getNext(fs);
      
      updateResult = featureState_updateKeyState(fs);

      if(updateResult != 0 &&  /* No change */
         updateResult != 5)    /* fs deleted */
      {
         featureState_sendMoUpdateFeatureStateInd(fs);
      }

      if(updateResult == 1 || /* License Key updated */
         updateResult == 2)   /* Unlock method activated or ceased */
      {
         featureState_sendLfciChangeIndToAllSubscribers(fs,
                      LICENSE_FEATURE_CONTROL_I_VERSION_1);
      }

      if(updateResult == 4)   /* alarmCorrelationEventId changed */
      {
         featureState_sendLfciChangeIndToAllSubscribers(fs,
                      LICENSE_FEATURE_CONTROL_I_VERSION_2);
      }

      if(updateResult == 1 || /* License Key updated */
         updateResult == 3)   /* License Key updated while unlock method active */
      {
         inventoryUpdated = GLMS_TRUE;
      }
   }
   
   for(cs = capacityState_getFirst();
       cs != NULL;
       cs = nextCs)
   {
      nextCs = capacityState_getNext(cs);
      
      updateResult = capacityState_updateKeyState(cs);

      if(updateResult != 0)
      {
         capacityState_sendMoUpdateCapacityStateInd(cs);
      }

      if(updateResult == 1 || /* License Key updated */
         updateResult == 2)   /* Unlock method activated or ceased */
      {
         capacityState_sendLcciChangeIndToAllSubscribers(cs);
      }

      if(updateResult == 1 || /* License Key updated */
         updateResult == 3)   /* License Key updated while unlock method active */
      {
         inventoryUpdated = GLMS_TRUE;
      }
   }
   
   clock_gettime(CLOCK_REALTIME, &currentTime);
   if(inventoryUpdated == GLMS_TRUE)
   {
      lm_setLastInventoryChange(currentTime.tv_sec);
   }

   lm_calculateLmState();
   lm_setLastInventoryRefresh(currentTime.tv_sec);
   lm_storeParameters();
   lm_sendMoUpdateLmInd();

   handleLicenseKeyCloseToExpirationAlarm();
}


void
lm_storeParameters()
{
   char *parameter;
   char  lastInventoryChange_timetStr[20];
   char  lastLicenseInventoryRefresh_timetStr[20];
   uint32_t paramSize;

   /* Store persistent parameters:
      lmData.lastInventoryChange
      lmData.lastLicenseInventoryRefresh
   */

   tracepoint(com_ericsson_glms, call_to_function,
              "lm_storeParameters");

   if(lmData.ppPendingUpdates == GLMS_TRUE)
   {
      paramSize = 0;
      sprintf(lastInventoryChange_timetStr, "%jd",
              (intmax_t)lmData.lastInventoryChange);
      paramSize += strlen(lastInventoryChange_timetStr);
      paramSize += 2; /* Delimiter */
      sprintf(lastLicenseInventoryRefresh_timetStr, "%jd",
              (intmax_t)lmData.lastLicenseInventoryRefresh);
      paramSize += strlen(lastLicenseInventoryRefresh_timetStr);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      strcat(parameter, lastInventoryChange_timetStr);
      strcat(parameter, ";:");
      strcat(parameter, lastLicenseInventoryRefresh_timetStr);

      pp_set(GLMS_TABLE_RID_LM, 1, parameter);

      lmData.ppPendingUpdates = GLMS_FALSE;
      free(parameter);
   }


   /* Store software parameters:
      lmData.fingerprint
   */

   if(lmData.spPendingUpdates == GLMS_TRUE)
   {
      paramSize = 0;
      paramSize += strlen(lmData.fingerprint);
      paramSize += strInStr(";", lmData.fingerprint);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      fillParameter(parameter, lmData.fingerprint, GLMS_TRUE);

      sp_set(GLMS_TABLE_RID_LM, 1, parameter);

      lmData.spPendingUpdates = GLMS_FALSE;
      free(parameter);
   }
}


void
lm_fetchParameters()
{
   tracepoint(com_ericsson_glms, call_to_function, "lm_fetchParameters");

   sp_requestIndexList(GLMS_TABLE_RID_LM);
   pp_requestIndexList(GLMS_TABLE_RID_LM);
}



/* handleGlmsAdpiXXIndexListRsp handles both
   GlmsAdpiSoftwareParameterIndexListRsp and
   GlmsAdpiPersistentParameterIndexListRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */
static int32_t
handleGlmsAdpiXXIndexListRsp(union itc_msg *sig)
{
   return common_handleGlmsAdpiXXIndexListRsp(sig, NULL, NULL);
}

void
lm_parsePp(union itc_msg *sig)
{
   char *paramStr, *parameter;
   uint32_t paramCount;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);

   if (paramCount < 1)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "The LicenseManagerPersistentData parameter string "
                 "contains less than the expected 1 parameter delimiters",
                 paramStr);
   }
   else
   {
      /* Read parameters:
         lmData.lastInventoryChange
         lmData.lastLicenseInventoryRefresh
      */
      parameter = getNextParameter(";:", paramStr);
      lmData.lastInventoryChange = (time_t32)strtoll(parameter, NULL, 10);

      parameter = getNextParameter(";:", NULL);
      lmData.lastLicenseInventoryRefresh = (time_t32)strtoll(parameter, NULL, 10);
   }
}

void
lm_parseSp(union itc_msg *sig)
{
   char *paramStr, *parameter;

   paramStr = sig->glmsPpGetRsp.value;

   /* Read parameters:
      lmData.fingerprint
   */
   parameter = getNextParameter(";:", paramStr);
   strncpy(lmData.fingerprint,
           parameter,
           GLMS_FINGERPRINT_LEN);
   lmData.fingerprint[GLMS_FINGERPRINT_LEN - 1] = '\0';
   cleanupEscapeCharacters(lmData.fingerprint);
}

/* handleGlmsAdpiXXGetRsp handles both
   GlmsAdpiSoftwareParameterGetRsp and
   GlmsAdpiPersistentParameterGetRsp signals.

   Both these signal structures are defined the same way so we
   will only use one of them to read both types of signals.
   This is only possible as long as both signals definitions
   look exactly the same. */
static int32_t
handleGlmsAdpiXXGetRsp(union itc_msg *sig)
{
   int table = common_prepareForParsingStoredParameters(sig, GLMS_TRUE, GLMS_TRUE);

   if (table == -1)
      return 1;

   if(sig->glmsPpGetRsp.index != 1)
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Unexpected table index when reading LM stored data",
                 sig->glmsPpGetRsp.index);
   }

   if (table == GLMS_SP)
   {
      lm_parseSp(sig);
   }
   else if (table == GLMS_PP)
   {
      lm_parsePp(sig);
   }

   return 1; /* We have read all parameters of this parameter type so return 1. */
}


int32_t
lm_parseStoredParameters(union itc_msg *sig)
{
   return common_parseStoredParameters(sig, lmData.readStatus, handleGlmsAdpiXXGetRsp, handleGlmsAdpiXXIndexListRsp, GLMS_TRUE, GLMS_TRUE);
}

void
lm_sendMoUpdateLmInd()
{
   union itc_msg *sigSend;

   tracepoint(com_ericsson_glms, call_to_function,
              "lm_sendMoUpdateLmInd");

   if(adapter_isSubscribedForMoUpdateInd() && lmData.sendMoUpdateInd && adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateLmInd),
                      GLMS_ADPI_MO_UPDATE_LM_IND);

      strcpy(sigSend->glmsAdpiMoUpdateLmInd.fingerprint,
             lm_getFingerprint());

      sigSend->glmsAdpiMoUpdateLmInd.fingerprintUpdateable =
         lm_getFingerprintUpdatable();
      sigSend->glmsAdpiMoUpdateLmInd.lmState =
         lm_getLmState();
      sigSend->glmsAdpiMoUpdateLmInd.lastInventoryChange =
         lm_getLastInventoryChange();
      sigSend->glmsAdpiMoUpdateLmInd.lastLicenseInventoryRefresh =
         lm_getLastInventoryRefresh();
      sigSend->glmsAdpiMoUpdateLmInd.referenceToLicenseServer[0] = '\0';

      sendMsgToAdapter(sigSend);
   }

   lmData.sendMoUpdateInd = GLMS_FALSE;
}

void
lm_setFeatureConfList(const GlmsFeatureConfigurationData* featureConfList,
                      uint32_t featureConfListLen)
{
  uint32_t featureConfListSize = featureConfListLen*sizeof(GlmsFeatureConfigurationData);

  if(lmData.featureConfList != NULL)
  {
    free(lmData.featureConfList);
  }

  lmData.featureConfList = malloc(featureConfListSize);
  memcpy(lmData.featureConfList, featureConfList, featureConfListSize);

  lmData.featureConfListLen = featureConfListLen;
}

const GlmsFeatureConfigurationData*
lm_getFeatureConfData(const char* keyId)
{
  GlmsFeatureConfigurationData* listItem = lmData.featureConfList;

  for(uint32_t i=0; i<lmData.featureConfListLen; i++)
  {
    if(strncmp(listItem->keyId, keyId, GLMS_MO_KEY_LEN) == 0)
    {
      return listItem;
    }
    else
    {
      listItem++;
    }
  }

  return NULL;
}

GlmsBool
lm_getPpPendingUpdates()
{
   return lmData.ppPendingUpdates;
}

GlmsBool
lm_getSpPendingUpdates()
{
   return lmData.spPendingUpdates;
}

GlmsBool
lm_getSendMoUpdateInd()
{
   return lmData.sendMoUpdateInd;
}

int32_t
lm_getStartupReadStatus(uint32_t elem)
{
   return lmData.readStatus[elem];
}

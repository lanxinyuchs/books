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
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include "com_ericsson_glms.h"

#include "glmsadpi/glmsDataTypes.h"
#include "glmsadpi/glms_adpi.sig"
#include "lihi/licenseFeatureControlICommon.h"
#include "lihi/licenseFeatureControlI.sig"
#include "glms_main.h"
#include "glmsUtils.h"
#include "persistentStorage.h"
#include "clients.h"
#include "data_featureState.h"
#include "data_featureKey.h"
#include "data_keyFile.h"
#include "glms_main.h"
#include "data_lm.h"
#include "data_unlocks.h"
#include "data_common.h"

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t sigNo;

   GlmsAdpiCreateFeatureStateMoReq          glmsCreateFeatureStateMoReq;
   GlmsAdpiDeleteFeatureStateMoReq          glmsDeleteFeatureStateMoReq;
   GlmsAdpiPersistentParameterGetReq        glmsPpGetReq;
   GlmsAdpiPersistentParameterGetRsp        glmsPpGetRsp;
   GlmsAdpiPersistentParameterIndexListReq  glmsPpIndexListReq;
   GlmsAdpiPersistentParameterIndexListRsp  glmsPpIndexListRsp;
   GlmsAdpiSoftwareParameterGetReq          glmsSpGetReq;
   GlmsAdpiSoftwareParameterGetRsp          glmsSpGetRsp;
   GlmsAdpiSoftwareParameterIndexListReq    glmsSpIndexListReq;
   GlmsAdpiSoftwareParameterIndexListRsp    glmsSpIndexListRsp;
   GlmsAdpiLicenseKeyNotAvailableAlarmReq   glmsLicenseKeyNotAvailableAlarmReq;
   GlmsAdpiLicenseKeyNotAvailableAlarmRsp   glmsLicenseKeyNotAvailableAlarmRsp;
   GlmsAdpiMoUpdateFeatureStateInd          glmsAdpiMoUpdateFeatureStateInd;
   GlmsAdpiDumpFeatureStateDataRsp          glmsDumpFeatureStateDataRsp;

   /* LIHI - LFCI */
   #include "lihi/licenseFeatureControlIUnionContent.h"
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

FeatureStateData fsData;


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

static void featureState_clearAllFeatureStateMos();

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
featureStateDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "featureStateDataInit");

   fsData.tableIndexPool                 = 1;
   fsData.indexesInTable[GLMS_PP]        = 0;
   fsData.indexesInTable[GLMS_SP]        = 0;
   fsData.readIndexesFromTable[GLMS_PP]  = 0;
   fsData.readIndexesFromTable[GLMS_SP]  = 0;
   fsData.readStatus[GLMS_PP]            = 0;
   fsData.readStatus[GLMS_SP]            = 0;
   fsData.tableIndex[GLMS_PP]            = NULL;
   fsData.tableIndex[GLMS_SP]            = NULL;
   fsData.firstFeatureState              = NULL;
   fsData.lastFeatureState               = NULL;
}

void
featureStateDataClear()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "featureStateDataClear");

   if(fsData.tableIndex[GLMS_PP] != NULL)
      free(fsData.tableIndex[GLMS_PP]);
   if(fsData.tableIndex[GLMS_SP] != NULL)
      free(fsData.tableIndex[GLMS_SP]);

   /* Deleting keys will clear subscriptions and key instances as well */
   featureState_clearAllFeatureStateMos();

   featureStateDataInit();
}

FeatureState
*featureState_getFirst()
{
   return fsData.firstFeatureState;
}

FeatureState
*featureState_getLast()
{
   return fsData.lastFeatureState;
}

FeatureState
*featureState_getNext(FeatureState *fsElement)
{
   return (fsElement == NULL) ? NULL : fsElement->nextFeatureState;
}

FeatureState
*featureState_getPrevious(FeatureState *fsElement)
{
   FeatureState *fsPrevElement = featureState_getFirst();

   if (fsElement == NULL)
      return NULL;

   while (fsPrevElement != NULL)
   {
      if (fsPrevElement->nextFeatureState == fsElement)
      {
         return fsPrevElement;
      }
      fsPrevElement = featureState_getNext(fsPrevElement);
   }

   return NULL;
}


FeatureState
*featureState_findByFeatureStateId(const char *featureStateId)
{
   FeatureState *fsElement = featureState_getFirst();

   while (fsElement != NULL)
   {
      if (compareMoKeys(featureStateId,
            featureState_getFeatureStateId(fsElement)))
      {
         break;
      }
      fsElement = featureState_getNext(fsElement);
   }

   return fsElement;
}


FeatureState
*featureState_findByKeyId(const char *keyId)
{
   FeatureState *fsElement = featureState_getFirst();

   while (fsElement != NULL)
   {
      if (compareKeyIds(keyId,
                 featureState_getKeyId(fsElement)))
      {
         break;
      }
      fsElement = featureState_getNext(fsElement);
   }

   return fsElement;
}


FeatureState
*featureState_findByTableIndex(uint32_t index)
{
   FeatureState *fsElement = featureState_getFirst();

   while (fsElement != NULL)
   {
      if (fsElement->tableIndex == index)
      {
         break;
      }
      fsElement = featureState_getNext(fsElement);
   }

   return fsElement;
}

static void
createFeatureKeysFromLatentKeys(const char *keyId)
{
   FeatureKey *fk;
   LatentFeatureKey *lk;

   lk = featureKey_findLatentKey(keyId);
   while(lk)
   {
      fk = featureKey_createKeyWithMoIndication(lk->keyId,
                                                lk->validFrom,
                                                lk->expiration,
                                                lk->keyType);
      featureKey_markAtKfParsing(fk);

      featureKey_clearLatentKey(lk);
      lk = featureKey_findLatentKey(keyId);
   }
}

static FeatureState*
featureState_createFeatureStateWithoutMoIndication(const char *featureStateId,
                                                   const char *description,
                                                   const char *keyId,
                                                   const char *name,
                                                   const char *productType,
                                                   GlmsFeatureState featureState,
                                                   GlmsLicenseState licenseState,
                                                   time_t32 markedForDeletion,
                                                   uint32_t tableIndex,
                                                   GlmsBool performLicenseValidation)
{
   FeatureState *fstate;

   tracepoint(com_ericsson_glms, featureState_createFeatureStateWithoutMoIndication,
              featureStateId,
              description,
              keyId,
              name,
              productType,
              featureState,
              licenseState,
              markedForDeletion,
              tableIndex,
              performLicenseValidation);

   fstate = featureState_findByKeyId(keyId);
   if (!fstate)
   {
      fstate = featureState_findByTableIndex(tableIndex);
   }

   if (fstate)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "FeatureState already exist when atempting to create new MO",
                 keyId);

      glms_logEvent(GLMS_LOG_LOW,
                    "FeatureState already exist when atempting to create new MO: %s %d",
                    keyId,
                    tableIndex);

      return fstate;
   }

   fstate = (FeatureState *) malloc(sizeof(FeatureState));

   fstate->markedForDeletion  = markedForDeletion;
   fstate->tableIndex         = tableIndex;
   fstate->featureState       = featureState;
   fstate->licenseState       = licenseState;
   fstate->licenseStateAccordingToKf = licenseState;

   /* Always create with default settings */
   fstate->licenseKeyNotAvailableAlarmActive = GLMS_FALSE;
   fstate->licenseKeyExpirationAlarmState    = ALARM_CEASED;
   fstate->lknaAlarmEventId             = 0;
   fstate->sentAlarmCorrelationEventId  = 0;
   fstate->expiryTime                   = 0;
   fstate->pendingUpdates[GLMS_PP]      = GLMS_FALSE;
   fstate->pendingUpdates[GLMS_SP]      = GLMS_FALSE;
   fstate->sendMoUpdateInd              = GLMS_TRUE;
   fstate->keyList                      = NULL;
   fstate->lfciSubList                  = NULL;

   strncpy(fstate->featureStateId, featureStateId,
           GLMS_MO_KEY_LEN);
   fstate->featureStateId[GLMS_MO_KEY_LEN - 1] = '\0';

   strncpy(fstate->keyId, keyId,
           GLMS_KEY_ID_LEN);
   fstate->keyId[GLMS_KEY_ID_LEN - 1] = '\0';

   fstate->activeFeatureKeyId[0] = '\0';

   strncpy(fstate->name, name,
           GLMS_KEY_NAME_LEN);
   fstate->name[GLMS_KEY_NAME_LEN - 1] = '\0';

   strncpy(fstate->productType, productType,
           GLMS_PRODUCT_TYPE_LEN);
   fstate->productType[GLMS_PRODUCT_TYPE_LEN - 1] = '\0';

   strncpy(fstate->description, description,
           GLMS_DESCRIPTION_LEN);
   fstate->description[GLMS_DESCRIPTION_LEN - 1] = '\0';

   fstate->nextFeatureState = NULL;

   if(fsData.lastFeatureState == NULL)
   {
      fsData.lastFeatureState = fstate;
      fsData.firstFeatureState = fstate;
   }
   else
   {
      fsData.lastFeatureState->nextFeatureState = fstate;
      fsData.lastFeatureState = fstate;
   }

   /* Update the license state in case we have a key installed
      or an unlock method is active. */
   if (performLicenseValidation == GLMS_TRUE)
   {
     if(featureState_updateKeyState(fstate) == 5)
     {
       /* fstate deleted */
       fstate = 0;
     }
   }

   return fstate;
}

void
featureState_doMoDeletionsAtKfParsing(struct timespec currentTime,
                                      GlmsKeyType keyType)
{
   FeatureState  *fs;
   FeatureKey    *fk, *nextfk;
   for (fs = featureState_getFirst();
        fs != NULL;
        fs = featureState_getNext(fs))
   {
      for (fk = featureState_getFirstFeatureKey(fs);
           fk != NULL;
           fk = nextfk)
      {
         nextfk = featureKey_getNext(fk);

         if (!featureKey_isMarkedAtKfParsing(fk) &&
             featureKey_getKeyType(fk) == keyType)
         {
            featureKey_deleteKeyAndMo(fk);

            if (featureState_getFirstFeatureKey(fs) == NULL &&
                featureState_getMarkedForDeletion(fs) == 0 &&
                featureState_getFirstSub(fs) == NULL)
            {
               featureState_setMarkedForDeletion(fs, currentTime.tv_sec);
               /* markedForDeletion is persistently stored below in call to
                  featureState_storeParametersOfAllFeatureStatesAndKeys */
            }
         }
      }
   }
}

/* featureState_createFeatureState creates a new FeatureState 
   data structure and MO. The featureStateId of the new MO will
   be the same as the keyId. */
FeatureState
*featureState_createFeatureState(const char *description,
                                 const char *keyId,
                                 const char *name,
                                 const char *productType,
                                 GlmsFeatureState featureState)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_createFeatureState",
              keyId);
   
   FeatureState *fstate = featureState_createFeatureStateWithoutMoIndication(keyId,
                                                    description,
                                                    keyId,
                                                    name,
                                                    productType,
                                                    featureState,
                                                    GLMS_LICENSESTATE_DISABLED, /* licenseState */
                                                    0,                          /* markedForDeletion */
                                                    fsData.tableIndexPool++,
                                                    GLMS_TRUE); /* Do license validation */



   glms_logEvent(GLMS_LOG_LOW,
                 "Creating FeatureState MO instance for %s",
                 featureState_getKeyId(fstate));

   /* Send Create FeatureState MO to adapter */
   union itc_msg *sig = itc_alloc(sizeof(GlmsAdpiCreateFeatureStateMoReq),
                                  GLMS_ADPI_CREATE_FEATURE_STATE_MO_REQ);
   sig->glmsCreateFeatureStateMoReq.featureState = featureState_getFeatureState(fstate);
   sig->glmsCreateFeatureStateMoReq.licenseState = featureState_getLicenseState(fstate);
   sig->glmsCreateFeatureStateMoReq.serviceState = featureState_getServiceState(fstate);
   strcpy(sig->glmsCreateFeatureStateMoReq.featureStateId,
          featureState_getFeatureStateId(fstate));
   strcpy(sig->glmsCreateFeatureStateMoReq.description,
          featureState_getDescription(fstate));
   strcpy(sig->glmsCreateFeatureStateMoReq.keyId,
          featureState_getKeyId(fstate));
   strcpy(sig->glmsCreateFeatureStateMoReq.activeFeatureKeyId,
          featureState_getActiveFeatureKeyId(fstate));
   sendMsgToAdapter(sig);

   fstate->pendingUpdates[GLMS_PP]   = GLMS_TRUE;
   fstate->pendingUpdates[GLMS_SP]   = GLMS_TRUE;

   featureState_storeParameters(fstate);

   /* FeatureState MO update indication is sent at a later stage
      after license validation */

   createFeatureKeysFromLatentKeys(fstate->keyId);

   return fstate;
}

static void
featureState_clearAllFeatureStateMos()
{
   FeatureState *fs, *delFs;

   tracepoint(com_ericsson_glms, call_to_function,
              "featureState_clearAllFeatureStateMos");

   for (fs = featureState_getFirst(); fs != NULL; )
   {
      delFs = fs;
      fs = featureState_getNext(fs);
      featureState_clearFeatureState(delFs);
   }
}

void
featureState_clearFeatureState(FeatureState *fsElement)
{
   FeatureState *fsPrevElement = featureState_getPrevious(fsElement);
   
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_clearFeatureState",
              featureState_getKeyId(fsElement));

   if (fsElement == fsData.firstFeatureState &&
       fsElement == fsData.lastFeatureState)
   {
      fsData.firstFeatureState = NULL;
      fsData.lastFeatureState = NULL;
   }
   else if (fsElement == fsData.firstFeatureState)
   {
      fsData.firstFeatureState = fsElement->nextFeatureState;
   }
   else if (fsElement == fsData.lastFeatureState)
   {
      fsData.lastFeatureState = fsPrevElement;
   }

   if (fsPrevElement != NULL)
   {
      fsPrevElement->nextFeatureState = fsElement->nextFeatureState;
   }

   featureState_clearSubscriberList(fsElement);
   featureState_clearFeatureKeyList(fsElement);

   free(fsElement);
}

void
featureState_deleteFeatureStateAndMo(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempted to delete a FeatureState MO from null pointer");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_deleteFeatureStateAndMo",
              featureState_getKeyId(fsElement));

   if (featureState_getFirstFeatureKey(fsElement) != NULL &&
       featureState_getFirstSub(fsElement) != NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempted to delete a FeatureState MO which has either a"
                 " FeatureKey MO or a LIHI subscriber");
      return;
   }
      
   glms_logEvent(GLMS_LOG_LOW,
                 "Deleting FeatureState MO instance for %s",
                 featureState_getKeyId(fsElement));

   union itc_msg *sig = itc_alloc(sizeof(GlmsAdpiDeleteFeatureStateMoReq),
               GLMS_ADPI_DELETE_FEATURE_STATE_MO_REQ);
   strcpy(sig->glmsDeleteFeatureStateMoReq.featureStateId,
          featureState_getFeatureStateId(fsElement));
   sendMsgToAdapter(sig);

   sp_deleteIndex(GLMS_TABLE_RID_FEATURESTATE,
                  featureState_getTableIndex(fsElement));

   featureState_clearFeatureState(fsElement);
}

char*
featureState_getFeatureStateId(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read featureStateId attribute from "
                 "null pointer in featureState_getFeatureStateId");
      return NULL;
   }

   return fsElement->featureStateId;
}

char*
featureState_getKeyId(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read keyId attribute from "
                 "null pointer in featureState_getKeyId");
      return NULL;
   }

   return fsElement->keyId;
}

char*
featureState_getName(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read name attribute from "
                 "null pointer in featureState_getName");
      return NULL;
   }

   return fsElement->name;
}

void
featureState_setName(FeatureState *fsElement, const char *newName)
{
   FeatureKey *fkElement;

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write name attribute to "
                 "null pointer in featureState_setName");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_setName",
              featureState_getKeyId(fsElement));

   if (strcmp(fsElement->name, newName) != 0)
   {
      strncpy(fsElement->name, newName, GLMS_KEY_NAME_LEN);
      fsElement->name[GLMS_KEY_NAME_LEN - 1] = '\0';
      fsElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;

      for (fkElement = featureState_getFirstFeatureKey(fsElement);
           fkElement != NULL;
           fkElement = featureKey_getNext(fkElement))
      {
         featureKey_sendMoUpdateFeatureKeyNameInd(fkElement);
      }
   }
}

char*
featureState_getProductType(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read productType attribute from "
                 "null pointer in featureState_getProductType");
      return NULL;
   }

   return fsElement->productType;
}

void
featureState_setProductType(FeatureState *fsElement, const char *newProductType)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write productType attribute to "
                 "null pointer in featureState_setProductType");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_setProductType",
              featureState_getKeyId(fsElement));

   if(strcmp(fsElement->productType, newProductType) != 0)
   {
      strncpy(fsElement->productType, newProductType, GLMS_PRODUCT_TYPE_LEN);
      fsElement->productType[GLMS_PRODUCT_TYPE_LEN - 1] = '\0';
      fsElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      
      /* No MO update indication is required for FeatureKey since the 
         productType should be set before the FeatureKey MO is created. */
   }
}

char*
featureState_getDescription(FeatureState *fsElement)
{
   if(fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read description attribute from null pointer");
      return NULL;
   }

   return fsElement->description;
}

void
featureState_setDescription(FeatureState *fsElement, const char *newDescription)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write description attribute to "
                 "null pointer in featureState_setDescription");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_setDescription",
              featureState_getKeyId(fsElement));

   if (strcmp(fsElement->description, newDescription) != 0)
   {
      strncpy(fsElement->description, newDescription, GLMS_DESCRIPTION_LEN);
      fsElement->description[GLMS_DESCRIPTION_LEN - 1] = '\0';
      fsElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      fsElement->sendMoUpdateInd = GLMS_TRUE;
   }
}

time_t32
featureState_getMarkedForDeletion(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read markedForDeletion attribute from "
                 "null pointer in featureState_getMarkedForDeletion");
      return 0;
   }

   return fsElement->markedForDeletion;
}

void
featureState_setMarkedForDeletion(FeatureState *fsElement, time_t32 newMarkedForDeletion)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write markedForDeletion attribute to "
                 "null pointer in featureState_setMarkedForDeletion");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_setMarkedForDeletion",
              featureState_getKeyId(fsElement));

   if (fsElement->markedForDeletion != newMarkedForDeletion)
   {
      fsElement->markedForDeletion = newMarkedForDeletion;
      fsElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
   }
}

GlmsBool
featureState_isMoDeleteTimeoutReached(FeatureState *fsElement)
{
   time_t32 deleteMark = featureState_getMarkedForDeletion(fsElement);
   struct timespec currentTime;

   clock_gettime(CLOCK_REALTIME, &currentTime);

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_isMoDeleteTimeoutReached");
      return GLMS_FALSE;
   }

   /* deleteMark == 0 means the MO is not marked for deletion*/
   if ((deleteMark != 0) &&
       (currentTime.tv_sec >= (deleteMark + GLMS_PENDING_MO_DELETE_TIME)))
   {
      return GLMS_TRUE;
   }

   return GLMS_FALSE;
}

GlmsFeatureState
featureState_getFeatureState(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read fatureState attribute from "
                 "null pointer in featureState_getFeatureState");
      return GLMS_FEATURESTATE_DEACTIVATED;
   }

   return fsElement->featureState;
}

void
featureState_setFeatureState(FeatureState *fsElement,
                             GlmsFeatureState newFeatureState)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write featureState attribute to "
                 "null pointer in featureState_setFeatureState");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_setFeatureState",
              featureState_getKeyId(fsElement));

   if (fsElement->featureState != newFeatureState)
   {
      fsElement->featureState = newFeatureState;
      fsElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      fsElement->sendMoUpdateInd = GLMS_TRUE;
      featureState_handleLicenseKeyNotAvailableAlarm(fsElement);
      featureState_handleLicenseKeyCloseToExpirationAlarm(fsElement);
      handleLicenseKeyCloseToExpirationAlarm();

      featureState_sendLfciChangeIndToAllSubscribers(fsElement,
                                                     LICENSE_FEATURE_CONTROL_I_VERSION_1);
   }
}

GlmsLicenseState
featureState_getLicenseState(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read licenseState attribute from "
                 "null pointer in featureState_getLicenseState");
      return GLMS_LICENSESTATE_DISABLED;
   }

   return fsElement->licenseState;
}

void
featureState_setLicenseState(FeatureState *fsElement,
                             GlmsLicenseState newLicenseState)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write licenseState attribute to "
                 "null pointer in featureState_setLicenseState");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_setLicenseState",
              featureState_getKeyId(fsElement));

   if (fsElement->licenseState != newLicenseState)
   {
      fsElement->licenseState = newLicenseState;
      fsElement->sendMoUpdateInd = GLMS_TRUE;
   }
}

GlmsLicenseState
featureState_getLicenseStateAccordingToKf(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read licenseStateAccordingToKf"
                 " attribute from null pointer");
      return GLMS_LICENSESTATE_DISABLED;
   }

   return fsElement->licenseStateAccordingToKf;
}

void
featureState_setLicenseStateAccordingToKf(FeatureState *fsElement,
                                          GlmsLicenseState newLicenseStateAccordingToKf)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write licenseStateAccordingToKf"
                 " attribute to null pointer");
      return;
   }

   fsElement->licenseStateAccordingToKf = newLicenseStateAccordingToKf;
}

void
featureState_setLicenseStateAccordingToKfForAll()
{
   FeatureState  *fk;

   tracepoint(com_ericsson_glms, call_to_function,
              "featureState_setLicenseStateAccordingToKfForAll");

   for (fk = featureState_getFirst();
        fk != NULL;
        fk = featureState_getNext(fk))
   {
      featureState_setLicenseStateAccordingToKf(fk, featureState_getLicenseState(fk));
   }
}


GlmsServiceState
featureState_getServiceState(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read serviceState attribute from null pointer");
      return GLMS_SERVICESTATE_INOPERABLE;
   }

   if (featureState_getFeatureState(fsElement) == GLMS_FEATURESTATE_ACTIVATED &&
       featureState_getLicenseState(fsElement) == GLMS_LICENSESTATE_ENABLED)
   {
      return GLMS_SERVICESTATE_OPERABLE;
   }

   return GLMS_SERVICESTATE_INOPERABLE;
}


uint32_t
featureState_getAlarmCorrelationEventId(FeatureState *fsElement)
{
   if(keyFile_getKffAlarmEventId())
   {
      return keyFile_getKffAlarmEventId();
   }

   return fsElement->lknaAlarmEventId;
}


GlmsBool
featureState_fillLfciLicenseInfoByKeyId(LfciLicenseInfoS *dstLicInfo,
                                        const char *licenseKeyId)
{
   FeatureState *featureState = featureState_findByKeyId(licenseKeyId);

   if (featureState == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Failed to find License Key in call to "
                 "featureState_fillLfciLicenseInfoByKeyId",
                 licenseKeyId);
      return GLMS_FALSE;
   }

   strncpy(dstLicInfo->licenseKeyId, licenseKeyId,
           MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID);
   dstLicInfo->licenseKeyId[MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';
   dstLicInfo->serviceState = wr16(featureState_getServiceState(featureState));
   dstLicInfo->licenseState = wr16(featureState_getLicenseState(featureState));
   dstLicInfo->featureState = wr16(featureState_getFeatureState(featureState));

   return GLMS_TRUE;
}


GlmsBool
featureState_fillLfciLicenseInfo2ByKeyId(LfciLicenseInfo2S *dstLicInfo,
                                         const char *licenseKeyId)
{
   FeatureState *featureState = featureState_findByKeyId(licenseKeyId);

   if (featureState == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Failed to find License Key in call to "
                 "featureState_fillLfciLicenseInfoByKeyId",
                 licenseKeyId);
      return GLMS_FALSE;
   }

   strncpy(dstLicInfo->licenseKeyId, licenseKeyId,
           MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID);
   dstLicInfo->licenseKeyId[MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';
   dstLicInfo->serviceState = wr16(featureState_getServiceState(featureState));
   dstLicInfo->licenseState = wr16(featureState_getLicenseState(featureState));
   dstLicInfo->featureState = wr16(featureState_getFeatureState(featureState));
   dstLicInfo->alarmCorrelationEventId =
      wr32(featureState_getAlarmCorrelationEventId(featureState));

   featureState->sentAlarmCorrelationEventId =
      featureState_getAlarmCorrelationEventId(featureState);

   return GLMS_TRUE;
}


void
featureState_resetAllKfParsingMarks(GlmsKeyType keyType)
{
   FeatureState *fsElement;
   FeatureKey *fkElement;

   for (fsElement = featureState_getFirst();
        fsElement != NULL;
        fsElement = featureState_getNext(fsElement))
   {
      for (fkElement = featureState_getFirstFeatureKey(fsElement);
           fkElement != NULL;
           fkElement = featureKey_getNext(fkElement))
      {
         featureKey_resetKfParsingMark(fkElement, keyType);
      }
   }

   featureKey_resetLatentKeysMark(GLMS_KEY_TYPE_CENTRALIZED);
}

/* Re-use the KeyFile installation marking to delete all MOs of a key type. */
void
featureState_deleteFeatureKeyMosAndLatentKeys(GlmsKeyType keyType)
{
   struct timespec currentTime;

   featureState_resetAllKfParsingMarks(keyType);

   clock_gettime(CLOCK_REALTIME, &currentTime);
   featureState_doMoDeletionsAtKfParsing(currentTime,
                                         keyType);
   featureKey_clearUnmarkedLatentKeys(keyType);
}
uint32_t
featureState_getNrOfFeatureKeysOfType(FeatureState *fsElement,
                                      GlmsKeyType keyType)
{
   uint32_t count;
   FeatureKey *fkElement;

   count = 0;

   for (fkElement = featureState_getFirstFeatureKey(fsElement);
        fkElement != NULL;
        fkElement = featureKey_getNext(fkElement))
   {
     if(featureKey_getKeyType(fkElement) == keyType)
     {
       count++;
     }
   }

   return count;
}

uint32_t
featureState_getTableIndex(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_getTableIndex");
      return 0;
   }

   return fsElement->tableIndex;
}


void
featureState_clearFeatureKeyList(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_clearFeatureKeyList");
      return;
   }

   if (fsElement->keyList == NULL)
   {
      return;
   }

   featureKey_clearKeys(fsElement->keyList, (GlmsKeyType)-1);
}


FeatureKey*
featureState_getFirstFeatureKey(FeatureState *fsElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_getFirstFeatureKey");
      return NULL;
   }
   
   return fsElement->keyList;
}


void
featureState_setFirstFeatureKey(FeatureState *fsElement, FeatureKey *fkElement)
{
   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_setFirstFeatureKey");
      return;
   }
   
   fsElement->keyList = fkElement;
}


struct FeatureKey_s 
*featureState_findFeatureKeyByStartDateExpirationAndKeyType(FeatureState *fs,
                                                           time_t32 startDate,
                                                           time_t32 expiration,
                                                           GlmsKeyType keyType)
{
   FeatureKey *fk;
   
   if (fs == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "featureState_findFeatureKeyByStartDateExpirationAndKeyType");
      return NULL;
   }

   
   for (fk = featureState_getFirstFeatureKey(fs);
        fk != NULL;
        fk = featureKey_getNext(fk))
   {
      if ((difftime(featureKey_getValidFrom(fk), startDate) == 0) &&
          (difftime(featureKey_getExpiration(fk), expiration) == 0) &&
          featureKey_getKeyType(fk) == keyType)
      {
         return fk;
      }
   }

   return NULL;
}

char *
featureState_getActiveFeatureKeyId(FeatureState *fs)
{
   return fs ? fs->activeFeatureKeyId : NULL;
}

void
featureState_updateActiveFeatureKey(FeatureState *fs, FeatureKey *fk)
{
   if (fk == NULL &&
       fs->activeFeatureKeyId[0] != '\0')
   {
      fs->activeFeatureKeyId[0] = '\0';
      fs->sendMoUpdateInd = GLMS_TRUE;
   }
   else if (fk != NULL &&
           !compareKeyIds(fs->activeFeatureKeyId,
                   featureKey_getFeatureKeyId(fk)))
   {
      strncpy(fs->activeFeatureKeyId,
              featureKey_getFeatureKeyId(fk),
              GLMS_MO_KEY_LEN);
      fs->activeFeatureKeyId[GLMS_MO_KEY_LEN - 1] = '\0';
      
      fs->sendMoUpdateInd = GLMS_TRUE;
   }
}

GlmsBool
shallUpdateInventoryTime(FeatureState *fsElement, time_t32 validFrom, time_t32 expiration)
{
   /* See if the lastInventoryChange attribute shall be updated */
   GlmsLicenseState oldLsAccordingToKf = featureState_getLicenseStateAccordingToKf(fsElement);
   if (oldLsAccordingToKf == GLMS_LICENSESTATE_ENABLED &&
       validFrom == 0 &&
       expiration == 0)
   {
      featureState_setLicenseStateAccordingToKf(fsElement, GLMS_LICENSESTATE_DISABLED);
      return GLMS_TRUE;
   }

   if (oldLsAccordingToKf == GLMS_LICENSESTATE_DISABLED &&
            (validFrom != 0 && expiration != 0))
   {
      featureState_setLicenseStateAccordingToKf(fsElement, GLMS_LICENSESTATE_ENABLED);
      return GLMS_TRUE;
   }

   return GLMS_FALSE;
}

static void
findMostGenerousFeatureKey(FeatureState *fsElement,
                           time_t32 *validFrom,
                           time_t32 *expiration,
                           time_t32 *consecutiveExpiration,
                           GlmsKeyType *keyType)
{
   uint32_t loopAgain;

   /* Find the most generous key */
   *validFrom  = 0;
   *expiration = 0;
   *consecutiveExpiration = 0;
   *keyType = -1;

   struct timespec currentTime;
   clock_gettime(CLOCK_REALTIME, &currentTime);

   FeatureKey *tmpi = fsElement->keyList;
   while(tmpi != NULL)
   {
      if (featureKey_getValidFrom(tmpi) <= currentTime.tv_sec &&
          featureKey_getExpiration(tmpi) == GLMS_NO_STOP_DATE)
      {
         *validFrom  = featureKey_getValidFrom(tmpi);
         *expiration = featureKey_getExpiration(tmpi);
         *consecutiveExpiration = GLMS_NO_STOP_DATE;
         *keyType = featureKey_getKeyType(tmpi);
         break;
      }
      else if (featureKey_getValidFrom(tmpi) <= currentTime.tv_sec &&
               featureKey_getExpiration(tmpi) >= currentTime.tv_sec)
      {
         if(featureKey_getExpiration(tmpi) >= *expiration &&
            *expiration != GLMS_NO_STOP_DATE)
         {
            *validFrom  = featureKey_getValidFrom(tmpi);
            *expiration = featureKey_getExpiration(tmpi);
            *keyType    = featureKey_getKeyType(tmpi);
         }

         if(featureKey_getExpiration(tmpi) > *consecutiveExpiration)
         {
            *consecutiveExpiration = featureKey_getExpiration(tmpi);
         }
      }

      tmpi = featureKey_getNext(tmpi);
   }

   loopAgain = 1;
   while(loopAgain)
   {
      loopAgain = 0;
      tmpi = fsElement->keyList;
      while(*consecutiveExpiration != GLMS_NO_STOP_DATE &&
            tmpi != NULL)
      {
         if (featureKey_getValidFrom(tmpi) <= *consecutiveExpiration &&
             (featureKey_getExpiration(tmpi) > *consecutiveExpiration ||
              featureKey_getExpiration(tmpi) == GLMS_NO_STOP_DATE))
         {
            *consecutiveExpiration = featureKey_getExpiration(tmpi);
            loopAgain = 1;
         }

         tmpi = featureKey_getNext(tmpi);
      }
   }
}

void
disableLicensesIfAmInactive(FeatureState *fsElement,
                            GlmsLicenseState oldLs,
                            time_t32 validFrom,
                            time_t32 expiration)
{
   /* We will not disable licenses if autonomous mode is active. */
   if (am_getActivationState() == GLMS_ACTIVATION_STATE_INACTIVE ||
       oldLs == GLMS_LICENSESTATE_DISABLED)
   {
      if (oldLs == GLMS_LICENSESTATE_DISABLED &&
          (validFrom != 0 || expiration != 0))
      {
         tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                    "featureState_updateKeyState license enabled",
                    featureState_getKeyId(fsElement));

         featureState_setLicenseState(fsElement, GLMS_LICENSESTATE_ENABLED);
      }
      else if (oldLs == GLMS_LICENSESTATE_ENABLED &&
               (validFrom == 0 || expiration == 0) &&
               !(eu_isEuActive() || iu_isIuActive() || pu_isPuActive()))
      {
         tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                    "featureState_updateKeyState license disabled",
                    featureState_getKeyId(fsElement));

         featureState_setLicenseState(fsElement, GLMS_LICENSESTATE_DISABLED);
      }
   }
}

void
enableLicensesIfUnlockActive(FeatureState *fsElement, GlmsLicenseState oldLs)
{
   /* If an unlock method is active we set the license state to enabled. */
   if (eu_isEuActive() || iu_isIuActive() || pu_isPuActive())
   {
      if (oldLs == GLMS_LICENSESTATE_DISABLED)
      {
         tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                    "featureState_updateKeyState license enabled by unlock method",
                    featureState_getKeyId(fsElement));

         featureState_setLicenseState(fsElement, GLMS_LICENSESTATE_ENABLED);
      }
   }
}

/* featureState_updateKeyState updates the license state of a featureState MO.
   It will also check if the FeatureState MO shall be deleted.

   It returns one of the following values:

   0: No change in license state.
   1: License changed as a result of License Key value. Send change ind and
      update inventory.
   2: License change as result of unlock period activated or ceased. Send
      change ind but dont update inventory.
   3: License state didn't change since an unlock method is active, but
      the License Key values changed so an inventory update is required.
   4: Only alarmCorrelationId has changed. This typically happens when
      the state go from 'no key file installed' to 'license key not available'.
   5: MO is deleted.
*/
uint32_t
featureState_updateKeyState(FeatureState *fsElement)
{
   GlmsBool updateInventoryTime;
   GlmsLicenseState oldLs;
   GlmsKeyType keyType;
   time_t32 validFrom, expiration, consecutiveExpiration;

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_updateKeyState");
      return 0;
   }

   if (keyFile_isRestricted(fsElement->keyId))
      return 0;

   if (featureState_isMoDeleteTimeoutReached(fsElement))
   {
      featureState_deleteFeatureStateAndMo(fsElement);
      return 5;
   }

   findMostGenerousFeatureKey(fsElement,
                              &validFrom,
                              &expiration,
                              &consecutiveExpiration,
                              &keyType);

   fsElement->expiryTime = consecutiveExpiration;

   updateInventoryTime = shallUpdateInventoryTime(fsElement,
                                                  validFrom,
                                                  expiration);

   /* Update the license state */
   oldLs = featureState_getLicenseState(fsElement);

   disableLicensesIfAmInactive(fsElement, oldLs, validFrom, expiration);

   featureState_updateActiveFeatureKey(fsElement, 
                featureState_findFeatureKeyByStartDateExpirationAndKeyType(
                                                  fsElement,
                                                  validFrom,
                                                  expiration,
                                                  keyType));
   
   enableLicensesIfUnlockActive(fsElement, oldLs);

   /* If the license state has changed we will have to send change indications */
   GlmsBool sendChangeInd;
   if (oldLs != featureState_getLicenseState(fsElement))
      sendChangeInd = GLMS_TRUE;
   else
      sendChangeInd = GLMS_FALSE;

   featureState_handleLicenseKeyNotAvailableAlarm(fsElement);
   featureState_handleLicenseKeyCloseToExpirationAlarm(fsElement);

   if (sendChangeInd == GLMS_TRUE && updateInventoryTime == GLMS_TRUE)
      return 1;

   if (sendChangeInd == GLMS_TRUE && updateInventoryTime == GLMS_FALSE)
      return 2;

   if (sendChangeInd == GLMS_FALSE && updateInventoryTime == GLMS_TRUE)
      return 3;

   if (fsElement->sentAlarmCorrelationEventId !=
       featureState_getAlarmCorrelationEventId(fsElement))
      return 4;

   return 0;
}


uint32_t
featureState_getNrOfFeatureKeyMos()
{
   FeatureState *fs;
   FeatureKey *fk;
   uint32_t nrOfKeys = 0;

   for (fs = featureState_getFirst();
        fs != NULL;
        fs = featureState_getNext(fs))
   {
      for (fk = featureState_getFirstFeatureKey(fs);
           fk != NULL;
           fk = featureKey_getNext(fk))
      {
         nrOfKeys++;
      }
   }

   return nrOfKeys;
}


uint32_t
featureState_getNrOfFeatureStateMos()
{
   FeatureState *fs;
   uint32_t nrOfKeys = 0;

   for (fs = featureState_getFirst();
        fs != NULL;
        fs = featureState_getNext(fs))
   {
         nrOfKeys++;
   }

   return nrOfKeys;
}


GlmsBool
featureState_haveAnInstalledLicenseKey(FeatureState *fsElement)
{
   if(fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_haveAnInstalledLicenseKey");
      return GLMS_FALSE;
   }

   return (fsElement->keyList == NULL) ? GLMS_FALSE : GLMS_TRUE;
}

void
featureState_addFeatureKey(FeatureState *fsElement,
                           struct FeatureKey_s *fkElement)
{
   FeatureKey *fk;

   if (fsElement == NULL || fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_addFeatureKey");
      return;
   }

   fk = featureState_getFirstFeatureKey(fsElement);
   if (fk == NULL)
   {
      fsElement->keyList = fkElement;
      fkElement->nextFeatureKey = NULL;
      fkElement->prevFeatureKey = NULL;
   }
   else
   {
      while (fk->nextFeatureKey != NULL)
      {
         fk = fk->nextFeatureKey;
      }

      fk->nextFeatureKey = fkElement;
      fkElement->nextFeatureKey = NULL;
      fkElement->prevFeatureKey = fk;
   }
}


/* featureState_clearSubscriptionsByKeyIdMidAndServerRef returns true if a 
 * subscription was cleared, otherwise false is returned.
 */  
GlmsBool
featureState_clearSubscriptionsByKeyIdMidAndServerRef(char *keyId,
                                                      itc_mbox_id_t mid,
                                                      int32_t serverRef)
{
   FeatureState *fsElement;
   LfciSubscriber *subi, *subPrev, *subNext;
   GlmsBool returnVal = GLMS_FALSE;
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_clearSubscriptionsByKeyIdMidAndServerRef",
              keyId);

   fsElement = featureState_findByKeyId(keyId);

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_clearSubscriptionsByKeyIdPidAndServerRef");
      return returnVal;
   }

   subi = fsElement->lfciSubList;
   subPrev = NULL;
   while (subi != NULL)
   {
      subNext = featureState_getNextSub(subi);

      if (featureState_getSubMid(subi) == mid &&
          (serverRef == 0 || featureState_getSubServerRef(subi) == serverRef))
      {
         if(subPrev == NULL)
            fsElement->lfciSubList = subNext;
         else
            subPrev->nextSub = subNext;

         returnVal = GLMS_TRUE;
         free(subi);

         /* Check if the feature key have no more subscribers, or active
            licenses, and in that case set the markedForDeletion */
         if(featureState_getMarkedForDeletion(fsElement) == 0 &&
            fsElement->lfciSubList == NULL &&
            featureState_haveAnInstalledLicenseKey(fsElement) == GLMS_FALSE)
         {
            clock_gettime(CLOCK_REALTIME, &currentTime);
            if(currentTime.tv_sec == -1)
            {
               tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                          "time returned -1 at deletion of "
                          "FeatureState MO. errno is",
                          errno);
            }
            else
            {
               featureState_setMarkedForDeletion(fsElement, currentTime.tv_sec);
               featureState_storeParameters(fsElement);
            }
         }
         
         featureState_handleLicenseKeyNotAvailableAlarm(fsElement);
         featureState_handleLicenseKeyCloseToExpirationAlarm(fsElement);
         handleLicenseKeyCloseToExpirationAlarm();
      }
      else
      {
         subPrev = subi;
      }

      subi = subNext;
   }

   return returnVal;
}


void
featureState_clearSubscriptionsByMidAndServerRef(itc_mbox_id_t mid, int32_t serverRef)
{
   FeatureState *fsElement;
   LfciSubscriber *subi, *subPrev, *subNext;
   struct timespec currentTime;

   fsElement = featureState_getFirst();

   tracepoint(com_ericsson_glms, call_to_function_w_hex_arg,
              "featureState_clearSubscriptionsByMidAndServerRef",
              mid);

   while (fsElement != NULL)
   {
      subi = fsElement->lfciSubList;
      subPrev = NULL;
      while (subi != NULL)
      {
         subNext = featureState_getNextSub(subi);

         if (featureState_getSubMid(subi) == mid &&
             (serverRef == 0 || featureState_getSubServerRef(subi) == serverRef))
         {
            if (subPrev == NULL)
               fsElement->lfciSubList = subNext;
            else
               subPrev->nextSub = subNext;

            free(subi);

            /* Check if the feature key have no more subscribers, or active
               licenses, and in that case set the markedForDeletion */
            if (featureState_getMarkedForDeletion(fsElement) == 0 &&
                fsElement->lfciSubList == NULL &&
                featureState_haveAnInstalledLicenseKey(fsElement) == GLMS_FALSE)
            {
               clock_gettime(CLOCK_REALTIME, &currentTime);
               if (currentTime.tv_sec == -1)
               {
                  tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                             "time returned -1 at deletion of "
                             "FeatureState MO. errno is",
                             errno);
               }
               else
               {
                  featureState_setMarkedForDeletion(fsElement, currentTime.tv_sec);
                  featureState_storeParameters(fsElement);
               }
            }

            featureState_handleLicenseKeyNotAvailableAlarm(fsElement);
            featureState_handleLicenseKeyCloseToExpirationAlarm(fsElement);
            handleLicenseKeyCloseToExpirationAlarm();
         }
         else
         {
            subPrev = subi;
         }

         subi = subNext;
      }

      fsElement = featureState_getNext(fsElement);
   }
}


void
featureState_clearSubscriberList(FeatureState *fsElement)
{
   LfciSubscriber *tmpi, *tmpk;

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to featureState_clearSubscriberList");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_clearSubscriberList",
              featureState_getKeyId(fsElement));

   if (fsElement->lfciSubList == NULL)
      return;

   tmpi = fsElement->lfciSubList;
   fsElement->lfciSubList = NULL;

   while (featureState_getNextSub(tmpi) != NULL)
   {
      tmpk = tmpi;
      tmpi = featureState_getNextSub(tmpi);
      free(tmpk);
   }
   free(tmpi);
}


void
featureState_addSubscriber(const char *keyId,
                           const char *name,
                           const char *description,
                           itc_mbox_id_t clientMid,
                           int32_t serverRef,
                           uint32_t clientRef)
{
   FeatureState *fsElement;
   LfciSubscriber *lfciSub, *lfciSub_i;

   tracepoint(com_ericsson_glms, featureState_addSubscriber,
              keyId,
              name,
              description,
              clientMid,
              serverRef,
              clientRef);

   fsElement = featureState_findByKeyId(keyId);

   if (fsElement == NULL)
   {
      fsElement = featureState_createFeatureState(description,
                                                  keyId,
                                                  name,
                                                  "", /* productType */
                                                  GLMS_FEATURESTATE_DEACTIVATED);
   }
   else if (featureState_getFirstSub(fsElement) == NULL)
   {
      /* The FeatureState instance could have been created when a key file
         was installed, but if this is the first subscriber we need to
         set the name of the feature key MO instance. */
      /* This will also update the MO instance name in the case where
         the subscriber has decided to rename the MO by subscribing with
         a new name. */
      featureState_setName(fsElement, name);
      featureState_setDescription(fsElement, description);
      featureState_setMarkedForDeletion(fsElement, 0);
   }

   lfciSub = (LfciSubscriber *) malloc(sizeof(LfciSubscriber));
   lfciSub->clientMid = clientMid;
   lfciSub->serverRef = serverRef;
   lfciSub->clientRef = clientRef;
   lfciSub->nextSub   = NULL;

   lfciSub_i = featureState_getFirstSub(fsElement);
   if (lfciSub_i == NULL)
   {
      fsElement->lfciSubList = lfciSub;
   }
   else
   {
      while (lfciSub_i->nextSub != NULL)
      {
         lfciSub_i = lfciSub_i->nextSub;
      }
      lfciSub_i->nextSub = lfciSub;
   }

   featureState_handleLicenseKeyNotAvailableAlarm(fsElement);
   featureState_handleLicenseKeyCloseToExpirationAlarm(fsElement);

   featureState_storeParameters(fsElement);
   featureState_sendMoUpdateFeatureStateInd(fsElement);
}


LfciSubscriber
*featureState_getFirstSub(FeatureState *fsElement)
{
   return (fsElement == NULL) ? NULL : fsElement->lfciSubList;
}


LfciSubscriber
*featureState_getNextSub(LfciSubscriber *subscriber)
{
   return (subscriber == NULL) ? NULL : subscriber->nextSub;
}


void
featureState_handleLicenseKeyNotAvailableAlarm(FeatureState *fsElement)
{
   union itc_msg *sig;
   static uint32_t alarmRsp[] = {1, GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_RSP};

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "featureState_handleLicenseKeyNotAvailableAlarm");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_handleLicenseKeyNotAvailableAlarm",
              featureState_getKeyId(fsElement));


   /* Activate alarm */
   if (fsElement->featureState == GLMS_FEATURESTATE_ACTIVATED &&
       featureState_getLicenseState(fsElement) == GLMS_LICENSESTATE_DISABLED &&
       keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_CEASED &&
       fsElement->licenseKeyNotAvailableAlarmActive == GLMS_FALSE &&
       featureState_getFirstSub(fsElement) != NULL &&
       eu_isEuActive() == GLMS_FALSE &&
       pu_isPuActive() == GLMS_FALSE &&
       iu_isIuActive() == GLMS_FALSE)
   {
      fsElement->licenseKeyNotAvailableAlarmActive = GLMS_TRUE;

      /* Send alarm indication to adapter */
      sig = itc_alloc(sizeof(GlmsAdpiLicenseKeyNotAvailableAlarmReq),
                  GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_REQ);
      strcpy(sig->glmsLicenseKeyNotAvailableAlarmReq.moId,
             fsElement->featureStateId);
      sig->glmsLicenseKeyNotAvailableAlarmReq.licenseType = GLMS_FEATURE_KEY;
      sig->glmsLicenseKeyNotAvailableAlarmReq.alarmState  = GLMS_ALARM_ACTIVATED;

      if (featureState_getFirstFeatureKey(fsElement) != NULL)
         sig->glmsLicenseKeyNotAvailableAlarmReq.alarmReason  =
            GLMS_ALARM_REASON_KEY_EXPIRED;
      else
         sig->glmsLicenseKeyNotAvailableAlarmReq.alarmReason  =
            GLMS_ALARM_REASON_KEY_NOT_AVAILABLE;

      sendMsgToAdapter(sig);

      sig = itc_receive(alarmRsp, ITC_NO_TMO, ITC_FROM_ALL);
      fsElement->lknaAlarmEventId = sig->glmsLicenseKeyNotAvailableAlarmRsp.lknaAlarmEventId;
      itc_free(&sig);
   }
   /* Cease alarm */
   else if ((fsElement->featureState == GLMS_FEATURESTATE_DEACTIVATED ||
             featureState_getLicenseState(fsElement) == GLMS_LICENSESTATE_ENABLED ||
             featureState_getFirstSub(fsElement) == NULL ||
             keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_ACTIVATED ||
             eu_isEuActive()) &&
             fsElement->licenseKeyNotAvailableAlarmActive == GLMS_TRUE)
   {
      fsElement->licenseKeyNotAvailableAlarmActive = GLMS_FALSE;

      /* Send alarm cease indication to adapter */
      sig = itc_alloc(sizeof(GlmsAdpiLicenseKeyNotAvailableAlarmReq),
                  GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_REQ);
      strcpy(sig->glmsLicenseKeyNotAvailableAlarmReq.moId,
             fsElement->featureStateId);
      sig->glmsLicenseKeyNotAvailableAlarmReq.licenseType = GLMS_FEATURE_KEY;
      sig->glmsLicenseKeyNotAvailableAlarmReq.alarmState  = GLMS_ALARM_CEASED;
      sig->glmsLicenseKeyNotAvailableAlarmReq.alarmReason = GLMS_ALARM_REASON_CEASED;
      sendMsgToAdapter(sig);

      sig = itc_receive(alarmRsp, ITC_NO_TMO, ITC_FROM_ALL);
      fsElement->lknaAlarmEventId = sig->glmsLicenseKeyNotAvailableAlarmRsp.lknaAlarmEventId;
      itc_free(&sig);
   }
}


void
featureState_handleLicenseKeyCloseToExpirationAlarm(FeatureState *fsElement)
{
   struct timespec currentTime;
   uint32_t expiring = 0;

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "featureState_handleLicenseKeyCloseToExpirationAlarm");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureState_handleLicenseKeyCloseToExpirationAlarm",
              featureState_getKeyId(fsElement));

   clock_gettime(CLOCK_REALTIME, &currentTime);
   if(fsElement->expiryTime != GLMS_NO_STOP_DATE &&
      ((fsElement->expiryTime - GLMS_LICENSEKEY_EXPIRATION_ALARM_TIME) < currentTime.tv_sec))
   {
      expiring = 1;
   }

   /* Activate alarm */
   if (expiring == 1 &&
       featureState_getFirstSub(fsElement) != NULL &&
       fsElement->featureState == GLMS_FEATURESTATE_ACTIVATED  &&
       featureState_getLicenseState(fsElement) == GLMS_LICENSESTATE_ENABLED &&
       keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_CEASED &&
       (fsElement->licenseKeyExpirationAlarmState == ALARM_CEASED ||
        fsElement->licenseKeyExpirationAlarmState == ALARM_CEASED_PENDING) &&
       eu_isEuActive() == GLMS_FALSE &&
       pu_isPuActive() == GLMS_FALSE &&
       iu_isIuActive() == GLMS_FALSE)
   {
      fsElement->licenseKeyExpirationAlarmState = ALARM_ACTIVE_PENDING;
   }
   /* Cease alarm */
   else if ((fsElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE ||
             fsElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING) &&
            (fsElement->featureState == GLMS_FEATURESTATE_DEACTIVATED ||
             featureState_getLicenseState(fsElement) == GLMS_LICENSESTATE_DISABLED ||
             keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_ACTIVATED ||
             featureState_getFirstSub(fsElement) == NULL ||
             eu_isEuActive() ||
             expiring == 0))
   {
      fsElement->licenseKeyExpirationAlarmState = ALARM_CEASED_PENDING;
   }
}

/*
  featureState_getNrOfExpiringKeys:
  Fills totalActivated with the total number of Feature Keys that are
  affected by the LicenseKeyCloseToExpiration alarm.

  Function returns 1 if any of the Feature Keys have an pending alarm state.
*/
uint32_t
featureState_getNrOfExpiringKeys(uint32_t *totalActivated)
{
   uint32_t pending = 0;
   FeatureState *fsElement;

   *totalActivated = 0;

   fsElement = featureState_getFirst();

   while (fsElement != NULL)
   {
      if(fsElement->licenseKeyExpirationAlarmState == ALARM_CEASED_PENDING ||
         fsElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING)
      {
         pending = 1;
      }

      if(fsElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE ||
         fsElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING)
      {
         (*totalActivated)++;
      }

      fsElement = featureState_getNext(fsElement);
   }

   return pending;
}

size_t
featureState_fillExpirationAlarmAddInfo(char *fillStr)
{
   FeatureState *fsElement;
   char *fillStrStart = fillStr;

   fillStr[0] = '\0';

   fsElement = featureState_getFirst();
   while (fsElement != NULL)
   {
      if(fsElement->licenseKeyExpirationAlarmState == ALARM_CEASED_PENDING)
      {
         fsElement->licenseKeyExpirationAlarmState = ALARM_CEASED;
      }

      if(fsElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING)
      {
         fsElement->licenseKeyExpirationAlarmState = ALARM_ACTIVE;
      }

      if(fsElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE)
      {
         fillStr = strcat(fillStr, fsElement->name);
         fillStr = strcat(fillStr, " (");
         fillStr = strcat(fillStr, fsElement->keyId);
         fillStr = strcat(fillStr, "), ");
      }

      /* The fillStr can become very large so to improve strcat times we
         move the fillStr pointer to the end of the string. */
      fillStr += strlen(fillStr);
      fsElement = featureState_getNext(fsElement);
   }

   return fillStr - fillStrStart;
}


itc_mbox_id_t
featureState_getSubMid(LfciSubscriber *subscriber)
{
   return (subscriber == NULL) ? 0 : subscriber->clientMid;
}


uint32_t
featureState_getSubClientRef(LfciSubscriber *subscriber)
{
   return (subscriber == NULL) ? 0 : subscriber->clientRef;
}


int32_t
featureState_getSubServerRef(LfciSubscriber *subscriber)
{
   return (subscriber == NULL) ? 0 : subscriber->serverRef;
}


int32_t
featureState_getSubClientServerRef(itc_mbox_id_t mid, LfciSubscriber *subscriber)
{
   if(subscriber == NULL)
     return 0;
   
   LfciClient *lfciClient = lfci_findByMidAndServerRef(mid, subscriber->serverRef);
   return lfci_getClientServerRef(lfciClient);
}

void
featureState_sendLfciChangeIndToAllSubscribers(FeatureState *fsElement,
                                               uint32_t indForPv)
{
   LfciSubscriber *subscriber;

   for (subscriber = featureState_getFirstSub(fsElement);
        subscriber != NULL;
        subscriber = featureState_getNextSub(subscriber))
   {
      featureState_sendLfciChangeIndToSubscriber(subscriber, fsElement, indForPv);
   }

   fsElement->sentAlarmCorrelationEventId =
      featureState_getAlarmCorrelationEventId(fsElement);
}

/* indForPv - This variable is used to only send indications to clients
              that has negotiated a PV of this level or greater. Ie,
              if indForPv is 2 then PV 1 clients shall not receive the
              indication.
*/
void
featureState_sendLfciChangeIndToSubscriber(LfciSubscriber *subscriber,
                                           FeatureState *fs,
                                           uint32_t indForPv)
{
   union itc_msg *sig;
   LfciClient *lfciClient;

   if ((fs == NULL) || (subscriber == NULL))
      return;

   lfciClient = lfci_findByMidAndServerRef(featureState_getSubMid(subscriber),
                                           featureState_getSubServerRef(subscriber));

   if(lfciClient == NULL)
   {
      glms_logEvent(GLMS_LOG_HIGH,
                    "Failed to find LFCI client of MID=0x%x, serverRef=%d",
                    featureState_getSubMid(subscriber),
                    featureState_getSubServerRef(subscriber));
      return;
   }

   if(lfci_getPv(lfciClient) < indForPv)
   {
      return;
   }
   else if(lfci_getPv(lfciClient) == LICENSE_FEATURE_CONTROL_I_VERSION_1)
   {
      LfciLicenseInfoS *lfciLicInfo;

      sig = itc_alloc(sizeof(LfciFeatureLicenseChangeIndS),
                      LFCI_FEATURE_LICENSE_CHANGE_IND);
      sig->lfciFeatureLicenseChangeInd.addressInfo.clientRef =
         wr32(featureState_getSubClientRef(subscriber));
      sig->lfciFeatureLicenseChangeInd.addressInfo.serverRef =
         wr32(featureState_getSubClientServerRef(featureState_getSubMid(subscriber),
                                                 subscriber));

      lfciLicInfo = &(sig->lfciFeatureLicenseChangeInd.licenseInfo);
      lfciLicInfo->serviceState = wr16(featureState_getServiceState(fs));
      lfciLicInfo->licenseState = wr16(featureState_getLicenseState(fs));
      lfciLicInfo->featureState = wr16(featureState_getFeatureState(fs));
      strncpy(lfciLicInfo->licenseKeyId,
              featureState_getKeyId(fs),
              MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID);
      lfciLicInfo->licenseKeyId[MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';

      glms_logEvent(GLMS_LOG_HIGH,
                    "Sending LFCI change ind to 0x%x; key = %s, "
                    "serviceState = %d, licenseState = %d, featureState = %d",
                    featureState_getSubMid(subscriber),
                    lfciLicInfo->licenseKeyId,
                    rd16(lfciLicInfo->serviceState),
                    rd16(lfciLicInfo->licenseState),
                    rd16(lfciLicInfo->featureState));

      itc_send(&sig, featureState_getSubMid(subscriber), ITC_MY_MBOX);
   }
   else
   {
      LfciLicenseInfo2S *lfciLicInfo;

      sig = itc_alloc(sizeof(LfciFeatureLicenseChange2IndS),
                      LFCI_FEATURE_LICENSE_CHANGE2_IND);
      sig->lfciFeatureLicenseChange2Ind.addressInfo.clientRef =
         wr32(featureState_getSubClientRef(subscriber));
      sig->lfciFeatureLicenseChange2Ind.addressInfo.serverRef =
         wr32(featureState_getSubClientServerRef(featureState_getSubMid(subscriber),
                                                 subscriber));

      lfciLicInfo = &(sig->lfciFeatureLicenseChange2Ind.licenseInfo);
      lfciLicInfo->serviceState = wr16(featureState_getServiceState(fs));
      lfciLicInfo->licenseState = wr16(featureState_getLicenseState(fs));
      lfciLicInfo->featureState = wr16(featureState_getFeatureState(fs));
      lfciLicInfo->alarmCorrelationEventId = wr32(featureState_getAlarmCorrelationEventId(fs));
      strncpy(lfciLicInfo->licenseKeyId,
              featureState_getKeyId(fs),
              MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID);
      lfciLicInfo->licenseKeyId[MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';

      glms_logEvent(GLMS_LOG_HIGH,
                    "Sending LFCI change ind to 0x%x; key = %s, "
                    "serviceState = %d, licenseState = %d, featureState = %d, "
                    "alarmCorrelationEventId = %d",
                    featureState_getSubMid(subscriber),
                    lfciLicInfo->licenseKeyId,
                    rd16(lfciLicInfo->serviceState),
                    rd16(lfciLicInfo->licenseState),
                    rd16(lfciLicInfo->featureState),
                    rd32(lfciLicInfo->alarmCorrelationEventId));

      itc_send(&sig, featureState_getSubMid(subscriber), ITC_MY_MBOX);
   }
}


void
featureState_storeParameters(FeatureState *fsElement)
{
   char *parameter;
   char  featureState_enumStr[4];
   char  markedForDeletion_timetStr[20];
   uint32_t paramSize;

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "featureState_storeParameters");
      return;
   }

   /* Store software parameters:
      fsElement->featureStateId
      fsElement->name
      fsElement->productType
      fsElement->description
      fsElement->keyId
      fsElement->featureState
      fsElement->markedForDeletion */
   if (fsElement->pendingUpdates[GLMS_SP] == GLMS_TRUE)
   {
      paramSize = 0;
      paramSize += strlen(fsElement->featureStateId);
      paramSize += strInStr(";", fsElement->featureStateId);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(fsElement->name);
      paramSize += strInStr(";", fsElement->name);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(fsElement->productType);
      paramSize += strInStr(";", fsElement->productType);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(fsElement->description);
      paramSize += strInStr(";", fsElement->description);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(fsElement->keyId);
      paramSize += strInStr(";", fsElement->keyId);
      paramSize += 2; /* Delimiter */
      sprintf(featureState_enumStr, "%"PRIu32, (uint32_t)fsElement->featureState);
      paramSize += strlen(featureState_enumStr);
      paramSize += 2; /* Delimiter */
      sprintf(markedForDeletion_timetStr, "%jd", (intmax_t)fsElement->markedForDeletion);
      paramSize += strlen(markedForDeletion_timetStr);
      paramSize += 2; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      fillParameter(parameter, fsElement->featureStateId, GLMS_FALSE);
      fillParameter(parameter, fsElement->name, GLMS_FALSE);
      fillParameter(parameter, fsElement->productType, GLMS_FALSE);
      fillParameter(parameter, fsElement->description, GLMS_FALSE);
      fillParameter(parameter, fsElement->keyId, GLMS_FALSE);
      fillParameter(parameter, featureState_enumStr, GLMS_FALSE);
      fillParameter(parameter, markedForDeletion_timetStr, GLMS_TRUE);

      sp_set(GLMS_TABLE_RID_FEATURESTATE,
             fsElement->tableIndex,
             parameter);

      fsElement->pendingUpdates[GLMS_SP] = GLMS_FALSE;
      free(parameter);
   }
}

void
featureState_storeParametersOfAllFeatureStatesAndKeys()
{
   FeatureState *fsElement;
   FeatureKey *fkElement;

   for (fsElement = featureState_getFirst();
        fsElement != NULL;
        fsElement = featureState_getNext(fsElement))
   {
      featureState_storeParameters(fsElement);
      featureState_sendMoUpdateFeatureStateInd(fsElement);

      for (fkElement = featureState_getFirstFeatureKey(fsElement);
           fkElement != NULL;
           fkElement = featureKey_getNext(fkElement))
      {
         featureKey_storeParameters(fkElement);
      }
   }
}


void
featureState_fetchParameters()
{
   /* Software Parameters shall be requested first and then
      Persistent Parameters. This is because Persistent Parameters
      needs to be handled before the Software Parameters for each
      Feature Key. */
   sp_requestIndexList(GLMS_TABLE_RID_FEATURESTATE);
   pp_requestIndexList(GLMS_TABLE_RID_FEATURESTATE);
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
   return common_handleGlmsAdpiXXIndexListRsp(sig, fsData.tableIndex, fsData.indexesInTable);
}


void
featureState_parseSp(union itc_msg *sig)
{
   char *paramStr, *parameter;
   char *keyId, *name, *productType, *description, *featureStateId;
   uint32_t paramCount;
   time_t32 markedForDeletion;
   GlmsFeatureState featureState;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);
   if (paramCount < 6)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "The FeatureState software parameter string contains "
                 "less than the expected 6 parameter delimiters",
                 paramStr);
   }
   else
   {
      /* Read parameters:
         fsElement->featureStateId
         fsElement->name
         fsElement->productType
         fsElement->description
         fsElement->keyId
         fsElement->featureState
         fsElement->markedForDeletion
      */

      featureStateId = getNextParameter(";:", paramStr);
      cleanupEscapeCharacters(featureStateId);

      name = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(name);

      productType = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(productType);

      description = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(description);

      keyId = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(keyId);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNu32, &featureState);

      parameter = getNextParameter(";:", NULL);
      markedForDeletion = (time_t32)strtoll(parameter, NULL, 10);

      /* Create a FeatureState with the read parameters */
      featureState_createFeatureStateWithoutMoIndication(featureStateId,
                                                         description,
                                                         keyId,
                                                         name,
                                                         productType,
                                                         featureState,
                                                         GLMS_LICENSESTATE_DISABLED,
                                                         markedForDeletion,
                                                         sig->glmsPpGetRsp.index,
                                                         GLMS_FALSE); /* No license validation */
   }
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
   int ret = common_prepareForParsingStoredParameters(sig, GLMS_TRUE, GLMS_TRUE);

   if (ret == -1)
      return -1;

   if (ret == GLMS_SP)
   {
      featureState_parseSp(sig);
   }
   else if (ret == GLMS_PP)
   {
      tracepoint(com_ericsson_glms, info_trace_w_str_arg,
                 "No persistent parameters were expected for FeatureState",
                 sig->glmsPpGetRsp.value);
   }

   /* Store the index that we have read */
   fsData.tableIndex[ret][fsData.readIndexesFromTable[ret]] = sig->glmsPpGetRsp.index;
   fsData.readIndexesFromTable[ret]++;

   if (fsData.readIndexesFromTable[ret] != fsData.indexesInTable[ret])
   {
      return 3; /* We expect more parameters */
   }
   /* All the indexes from this table are read */
   else
   {
      common_findHighestIndexInTable(fsData.tableIndex[ret], fsData.readIndexesFromTable[ret], &fsData.tableIndexPool);

      for (FeatureState *fs = featureState_getFirst();
           fs != NULL;
           fs = featureState_getNext(fs))
      {
         for (uint32_t i = 0; i < fsData.readIndexesFromTable[ret]; i++)
         {
            if (fsData.tableIndex[ret][i] == featureState_getTableIndex(fs))
            {
               fsData.tableIndex[ret][i] = 0;
            }
         }
      }

      common_sanityCheckIndexesInTable(fsData.tableIndex[ret], fsData.readIndexesFromTable[ret]);
      common_resetIndexesInTable(&fsData.indexesInTable[ret], &fsData.readIndexesFromTable[ret], &fsData.tableIndex[ret]);

      return 1; /* We have read all parameters so return 1. */
   }
}


int32_t
featureState_parseStoredParameters(union itc_msg *sig)
{
   static uint32_t featureKeyDataFetched = 1;
   featureKeyDataFetched = (sig->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP) ? 0 : featureKeyDataFetched;
   int32_t ret = common_parseStoredParameters(sig, fsData.readStatus, handleGlmsAdpiXXGetRsp, handleGlmsAdpiXXIndexListRsp, GLMS_TRUE, GLMS_TRUE);

   if ((ret == 1) && (featureKeyDataFetched == 0))
   {
      featureKeyDataFetched = 1;
      featureKey_fetchParameters();
   }

   return ret;
}

void
featureState_sendMoUpdateFeatureStateInd(FeatureState *fsElement)
{
   union itc_msg *sigSend;

   if (fsElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "featureState_sendMoUpdateFeatureStateInd");
      return;
   }

   if (adapter_isSubscribedForMoUpdateInd() &&
       fsElement->sendMoUpdateInd &&
       adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateFeatureStateInd),
                          GLMS_ADPI_MO_UPDATE_FEATURE_STATE_IND);
      strcpy(sigSend->glmsAdpiMoUpdateFeatureStateInd.featureStateId,
             featureState_getFeatureStateId(fsElement));
      strcpy(sigSend->glmsAdpiMoUpdateFeatureStateInd.description,
             featureState_getDescription(fsElement));
      strcpy(sigSend->glmsAdpiMoUpdateFeatureStateInd.keyId,
             featureState_getKeyId(fsElement));
      strcpy(sigSend->glmsAdpiMoUpdateFeatureStateInd.activeFeatureKeyId,
             featureState_getActiveFeatureKeyId(fsElement));
      sigSend->glmsAdpiMoUpdateFeatureStateInd.featureState =
          featureState_getFeatureState(fsElement);
      sigSend->glmsAdpiMoUpdateFeatureStateInd.licenseState =
         featureState_getLicenseState(fsElement);
      sigSend->glmsAdpiMoUpdateFeatureStateInd.serviceState =
         featureState_getServiceState(fsElement);
      sendMsgToAdapter(sigSend);
   }

   fsElement->sendMoUpdateInd = GLMS_FALSE;
}

uint32_t
writeFeatureStateStatus(FeatureState *fstate, char *storage, uint32_t offset)
{
   uint32_t ret;
   char markedForDeletion_s[30];
   time_t markedForDeletion = (time_t) featureState_getMarkedForDeletion(fstate);
   
   strncpy(markedForDeletion_s, ctime(&markedForDeletion), 30);
   markedForDeletion_s[29] = '\0';

   /* Read status of Feature State */
   ret = snprintf(storage + offset,
                  GLMS_MAX_DUMPSTR_LEN - offset,
                  "\nFeatureStateId\t\t: %s\n"
                  "KeyId\t\t\t: %s\n"
                  "Name\t\t\t: %s\n"
                  "ProductType\t\t: %s\n"
                  "Description\t\t: %s\n"
                  "FeatureState\t\t: %d\n"
                  "LicenseState\t\t: %d\n"
                  "LicenseStateAccordingToKf : %d\n"
                  "MarkedForDeletion\t: %s"
                  "PendingUpdates\t\t: %d %d\n"
                  "LkNotAvailableAlarm\t: %d\n"
                  "SendMoUpdateInd\t\t: %d\n"
                  "TableIndex\t\t: %d\n",
                  featureState_getFeatureStateId(fstate),
                  featureState_getKeyId(fstate),
                  featureState_getName(fstate),
                  featureState_getProductType(fstate),
                  featureState_getDescription(fstate),
                  featureState_getFeatureState(fstate),
                  featureState_getLicenseState(fstate),
                  featureState_getLicenseStateAccordingToKf(fstate),
                  markedForDeletion_s,
                  fstate->pendingUpdates[0],
                  fstate->pendingUpdates[1],
                  fstate->licenseKeyNotAvailableAlarmActive,
                  fstate->sendMoUpdateInd,
                  fstate->tableIndex);

   if (ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpFeatureStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   return ret;
}

uint32_t
writeFeatureKeyInstance(FeatureKey *fkey, char *storage, uint32_t offset, uint32_t s)
{
   uint32_t ret;
   time_t validFrom, expiration;
   validFrom = (time_t) featureKey_getValidFrom(fkey);
   expiration = (time_t) featureKey_getExpiration(fkey);

   char validFrom_s[30], expiration_s[30];
   strncpy(validFrom_s, ctime(&validFrom), 30);
   validFrom_s[29] = '\0';
   strncpy(expiration_s, ctime(&expiration), 30);
   expiration_s[29] = '\0';

   ret = snprintf(storage + offset,
                  GLMS_MAX_DUMPSTR_LEN - offset,
                  "  %d: featureKeyId\t= %s\n"
                  "  %d: validFrom\t\t= %s"
                  "  %d: expiration\t\t= %s"
                  "  %d: markedAtKfParsing\t= %d\n"
                  "  %d: tableIndex\t\t= %d\n",
                  s, featureKey_getFeatureKeyId(fkey),
                  s, validFrom_s,
                  s, expiration_s,
                  s, featureKey_isMarkedAtKfParsing(fkey),
                  s, featureKey_getTableIndex(fkey));

   if(ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpFeatureStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   return ret;
}

uint32_t
writeFeatureKeyList(FeatureState *fstate, char *storage, uint32_t offset)
{
   uint32_t ret, s = 1;

   /* Read key list of Feature Key */
   ret = snprintf(storage + offset,
                  GLMS_MAX_DUMPSTR_LEN - offset,
                  "FeatureKeys:\n");

   if (ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpFeatureStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   FeatureKey *fkey = featureState_getFirstFeatureKey(fstate);
   while(fkey != NULL)
   {
      ret += writeFeatureKeyInstance(fkey, storage, ret + offset, s);
      ++s;
      fkey = featureKey_getNext(fkey);
   }

   return ret;
}

uint32_t
writeFeatureStateSubsriberList(FeatureState *fstate, char *storage, uint32_t offset)
{
   uint32_t ret;

   /* Read subscriber list of FeatureState */
   ret = snprintf(storage + offset,
                  GLMS_MAX_DUMPSTR_LEN,
                  "Subscribers:\n");

   if (ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpFeatureStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   uint32_t tmp;
   LfciSubscriber *lfciSub = featureState_getFirstSub(fstate);
   while (lfciSub != NULL)
   {
      tmp = snprintf(storage + offset + ret,
                      GLMS_MAX_DUMPSTR_LEN - offset - ret,
                      "  MailboxId\t: 0x%08x\n"
                      "    ServerRef\t: %d\n"
                      "    ClientRef\t: %d\n",
                      featureState_getSubMid(lfciSub),
                      featureState_getSubServerRef(lfciSub),
                      featureState_getSubClientRef(lfciSub));

      if (tmp >= GLMS_MAX_DUMPSTR_LEN - offset)
      {
         tracepoint(com_ericsson_glms, error_trace,
                    "Dump string was truncated in call"
                    " to handleGlmsAdpiDumpFeatureStateDataReq");
         tmp = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
      }

      ret += tmp;
      lfciSub = featureState_getNextSub(lfciSub);
   }

   return ret;
}

void
sendGlmsDumpFeatureStateDataRsp(char *dump,
                                uint32_t dumpSize,
                                uint32_t currentBufferUsed,
                                uint32_t lastResponse,
                                union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiDumpFeatureStateDataRsp) + dumpSize,
                                       GLMS_ADPI_DUMP_FEATURE_STATE_DATA_RSP);
   sigSend->glmsDumpFeatureStateDataRsp.result       = GLMS_OK;
   strncpy(sigSend->glmsDumpFeatureStateDataRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sigSend->glmsDumpFeatureStateDataRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsDumpFeatureStateDataRsp.lastResponse = lastResponse;
   sigSend->glmsDumpFeatureStateDataRsp.sizeOfDump = currentBufferUsed + 1;
   strncpy(sigSend->glmsDumpFeatureStateDataRsp.dump, dump, dumpSize);
   sigSend->glmsDumpFeatureStateDataRsp.dump[dumpSize] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

void
handleGlmsAdpiDumpFeatureStateDataReq(union itc_msg *sigRec)
{
   uint32_t dataBufferSizeIter = 0;
   uint32_t dataBufferSize     = 0;
   uint32_t currentBufferUsed  = 0;
   char *buf;
   char dumpStr[GLMS_MAX_DUMPSTR_LEN];
   uint32_t dumpStrLen;
   uint32_t dumpStrOffset = 0;

   dataBufferSize = GlmsDumpSize[dataBufferSizeIter];
   buf = malloc(dataBufferSize);
   buf[0] = '\0';

   dumpStrLen = snprintf(dumpStr, GLMS_MAX_DUMPSTR_LEN,
                         "readIndexesFromTable\t: %d %d\n"
                         "indexesInTable\t\t: %d %d\n"
                         "startupReadStatus\t: %d %d\n"
                         "tableIndexPool\t\t: %d\n",
                         fsData.readIndexesFromTable[0],
                         fsData.readIndexesFromTable[1],
                         fsData.indexesInTable[0],
                         fsData.indexesInTable[1],
                         fsData.readStatus[0],
                         fsData.readStatus[1],
                         fsData.tableIndexPool);

   if (dumpStrLen >= GLMS_MAX_DUMPSTR_LEN - dumpStrOffset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpFeatureStateDataReq");
      dumpStrLen = GLMS_MAX_DUMPSTR_LEN - 1 - dumpStrOffset;
   }

   /* Copy dumpStr to the final dump buffer */
   strcpy(buf, dumpStr);

   currentBufferUsed += dumpStrLen;

   FeatureState *fstate = featureState_getFirst();
   while(fstate != NULL)
   {
      dumpStrOffset = 0;
      dumpStrOffset += writeFeatureStateStatus(fstate, dumpStr, dumpStrOffset);
      dumpStrOffset += writeFeatureKeyList(fstate, dumpStr, dumpStrOffset);
      dumpStrOffset += writeFeatureStateSubsriberList(fstate, dumpStr, dumpStrOffset);

      /* If the total dump size will be greater than the maximum allowed
         dump size, then send the already existing dump text and start
         a new dump string. */
      if((currentBufferUsed + dumpStrOffset) >
         GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT])
      {
         sendGlmsDumpFeatureStateDataRsp(buf, GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT],
                                         currentBufferUsed, 0, sigRec);

         /* Reset the buffer to write a new string. We keep the previously
            malloced buffer to store the new dump string. */
         buf[0] = '\0';
         currentBufferUsed = 0;
      }
      /* If the total dump size will be greater than the currently allowed
         dump size, then reallocate the dump memory to allow it to be larger. */
      else if((currentBufferUsed + dumpStrOffset) >
              GlmsDumpSize[dataBufferSizeIter])
      {
         dataBufferSizeIter++;
         buf = realloc(buf, GlmsDumpSize[dataBufferSizeIter]);
      }


      /* We now have enough space in 'buf' to add the new text from 'dumpStr' */
      strcat(buf, dumpStr);
      currentBufferUsed += dumpStrOffset;
      fstate = featureState_getNext(fstate);
   }

   /* 'buf' now contains the last part of the dump. Send it to the adapter
      and free the allocated memory. */
   sendGlmsDumpFeatureStateDataRsp(buf, currentBufferUsed,
                                   currentBufferUsed, 1, sigRec);

   free(buf);
}

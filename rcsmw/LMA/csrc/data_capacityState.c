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
#include "lihi/licenseCapacityControlICommon.h"
#include "lihi/licenseCapacityControlI.sig"
#include "glms_main.h"
#include "glmsUtils.h"
#include "persistentStorage.h"
#include "clients.h"
#include "data_capacityState.h"
#include "data_capacityKey.h"
#include "data_keyFile.h"
#include "glms_main.h"
#include "data_lm.h"
#include "data_unlocks.h"
#include "glms_timi.h"
#include "data_common.h"


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t  sigNo;

   GlmsAdpiCreateCapacityStateMoReq         glmsCreateCapacityStateMoReq;
   GlmsAdpiDeleteCapacityStateMoReq         glmsDeleteCapacityStateMoReq;
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
   GlmsAdpiDumpCapacityStateDataRsp         glmsDumpCapacityStateDataRsp;
   GlmsAdpiMoUpdateCapacityStateInd         glmsAdpiMoUpdateCapacityStateInd;
   GlmsAdpiMoUpdateCapacityStateCapacityUnitInd glmsAdpiMoUpdateCapacityStateCapacityUnitInd;
   
   GlmsAdpiCreateGracePeriodMoReq           glmsCreateGracePeriodMoReq;
   GlmsAdpiDeleteGracePeriodMoReq           glmsDeleteGracePeriodMoReq;
   GlmsAdpiMoUpdateGracePeriodInd           glmsAdpiMoUpdateGracePeriodInd;
   GlmsAdpiGpAlarmInd                       glmsAdpiGpAlarmInd;
   /* LIHI - LCCI */
   #include "lihi/licenseCapacityControlIUnionContent.h"
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

CapacityStateData csData;


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

void capacityState_clearAllKeys();

static char *
capacityState_getHwacString(CapacityState *csElement);

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
capacityStateDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "capacityStateDataInit");

   csData.tableIndexPool                 = 1;
   csData.indexesInTable[GLMS_PP]        = 0;
   csData.indexesInTable[GLMS_SP]        = 0;
   csData.readIndexesFromTable[GLMS_PP]  = 0;
   csData.readIndexesFromTable[GLMS_SP]  = 0;
   csData.readStatus[GLMS_PP]            = 0;
   csData.readStatus[GLMS_SP]            = 0;
   csData.tableIndex[GLMS_PP]            = NULL;
   csData.tableIndex[GLMS_SP]            = NULL;

   /* Keys are only cleared if this is a call from capacityStateDataClear() */
   capacityState_clearAllKeys();
   csData.firstCapacityState             = NULL;
   csData.lastCapacityState              = NULL;
}

void
capacityStateDataClear()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "capacityStateDataClear");

   if(csData.tableIndex[GLMS_PP] != NULL)
      free(csData.tableIndex[GLMS_PP]);

   if(csData.tableIndex[GLMS_SP] != NULL)
      free(csData.tableIndex[GLMS_SP]);

   capacityStateDataInit();
}

CapacityState
*capacityState_getFirst()
{
   return csData.firstCapacityState;
}

CapacityState
*capacityState_getLast()
{
   return csData.lastCapacityState;
}

CapacityState
*capacityState_getNext(CapacityState *csElement)
{
   return (csElement == NULL) ? NULL : csElement->nextCapacityState;
}

CapacityState
*capacityState_getPrevious(CapacityState *csElement)
{
   if (csElement == NULL)
      return NULL;

   CapacityState *csPrevElement = capacityState_getFirst();
   while (csPrevElement != NULL)
   {
      if (csPrevElement->nextCapacityState == csElement)
         return csPrevElement;

      csPrevElement = capacityState_getNext(csPrevElement);
   }

   return NULL;
}

CapacityState
*capacityState_findByCapacityStateId(char *capacityStateId)
{
   CapacityState *csElement = capacityState_getFirst();

   while(csElement != NULL)
   {
      if (compareMoKeys(capacityStateId, capacityState_getCapacityStateId(csElement)))
         break;

      csElement = capacityState_getNext(csElement);
   }

   return csElement;
}


CapacityState
*capacityState_findByKeyId(char *keyId)
{
   CapacityState *csElement = capacityState_getFirst();

   while(csElement != NULL)
   {
      if (compareKeyIds(keyId, capacityState_getCapacityKeyId(csElement)))
         break;

      csElement = capacityState_getNext(csElement);
   }

   return csElement;
}

CapacityState
*capacityState_findByGpTimerId(uint32_t timerId)
{
   CapacityState *csElement = capacityState_getFirst();

   while(csElement != NULL)
   {
      if(csElement->gpTimerId == timerId)
         return csElement;

      csElement = capacityState_getNext(csElement);
   }

   return csElement;
}

void
capacityState_stopGpTimer(CapacityState *cs)
{
   if (cs != NULL)
   {
      (void)deleteTimer(cs->gpTimerId);
      cs->gpTimerId = 0;
   }
}

CapacityState
*capacityState_findByTableIndex(uint32_t index)
{
   CapacityState *csElement = capacityState_getFirst();

   while(csElement != NULL)
   {
      if(csElement->tableIndex == index)
         break;

      csElement = capacityState_getNext(csElement);
   }

   return csElement;
}


static void
capacityState_createGpMo(CapacityState *cstate)
{
   union itc_msg *sig;
  
   if (cstate == NULL)
      return;

   if (capacityState_isGracePeriodMoCreated(cstate) == GLMS_FALSE)
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "Creating GracePeriod MO instance for %s",
                    capacityState_getCapacityKeyId(cstate));

      cstate->isGracePeriodMoCreated = GLMS_TRUE;
      cstate->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      
      /* Send Create Grace Period MO request to adapter */
      sig = itc_alloc(sizeof(GlmsAdpiCreateGracePeriodMoReq),
                  GLMS_ADPI_CREATE_GRACE_PERIOD_MO_REQ);
      capacityState_copyGracePeriod(cstate, &sig->glmsCreateGracePeriodMoReq.gracePeriod);
      sendMsgToAdapter(sig);
   }
}


static void
capacityState_deleteGpMo(CapacityState *cstate)
{
   union itc_msg *sig;
  
   if (cstate == NULL)
      return;

   if(capacityState_isGracePeriodMoCreated(cstate) == GLMS_TRUE)
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "Deleting GracePeriod MO instance of %s",
                    capacityState_getCapacityKeyId(cstate));

      cstate->isGracePeriodMoCreated = GLMS_FALSE;
      cstate->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      
      /* Send Create Grace Period MO request to adapter */
      sig = itc_alloc(sizeof(GlmsAdpiDeleteGracePeriodMoReq),
                  GLMS_ADPI_DELETE_GRACE_PERIOD_MO_REQ);
      strcpy(sig->glmsDeleteGracePeriodMoReq.gracePeriodId,
             capacityState_getCapacityStateId(cstate));
      sendMsgToAdapter(sig);
   }
}


GlmsBool
capacityState_isGracePeriodMoCreated(CapacityState *cstate)
{
   return (cstate == NULL) ? GLMS_FALSE : cstate->isGracePeriodMoCreated;
}


static CapacityState*
capacityState_createCapacityStateWithoutMoIndication(char *capacityStateId,
                                                     char *description,
                                                     char *keyId,
                                                     char *name,
                                                     char *capacityUnit,
                                                     char *productType,
                                                     GlmsLicenseState licenseState,
                                                     time_t32 markedForDeletion,
                                                     uint32_t tableIndex,
                                                     GlmsBool performLicenseValidation,
                                                     GlmsCapacityValue currentCapacityValue,
                                                     uint16_t isGracePeriodControlled,
                                                     uint32_t gracePeriodActivationValue,
                                                     GlmsGracePeriod gracePeriod,
                                                     uint32_t isGracePeriodMoCreated)
{
   CapacityState *cstate = NULL;

   tracepoint(com_ericsson_glms, capacityState_createCapacityStateWithoutMoIndication,
              capacityStateId,
              description,
              keyId,
              name,
              capacityUnit,
              productType,
              licenseState,
              markedForDeletion,
              tableIndex,
              performLicenseValidation);

   tracepoint(com_ericsson_glms, capacityState_createCapacityStateWithoutMoIndication_part2,
              currentCapacityValue.noLimit,
              currentCapacityValue.value,
              isGracePeriodControlled,
              gracePeriodActivationValue,
              gracePeriod.gracePeriodExpiration,
              gracePeriod.gpConfiguredLength,
              gracePeriod.gpConfiguredResetThreshold,
              gracePeriod.gpConfiguredActivationThreshold,
              gracePeriod.gracePeriodState,
              gracePeriod.gracePeriodId);

   cstate = capacityState_findByKeyId(keyId);
   if (!cstate)
      cstate = capacityState_findByTableIndex(tableIndex);

   if (cstate)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "CapacityState already exist when atempting to create new MO",
                 keyId);

      glms_logEvent(GLMS_LOG_LOW,
                    "CapacityState already exist when atempting to create new MO: %s %d",
                    keyId,
                    tableIndex);

      return cstate;
   }

   cstate = (CapacityState *) malloc(sizeof(CapacityState));

   cstate->markedForDeletion  = markedForDeletion;
   cstate->tableIndex         = tableIndex;
   cstate->licenseState       = licenseState;
   cstate->licenseStateAccordingToKf  = licenseState;
   cstate->isGracePeriodControlled    = isGracePeriodControlled;
   cstate->gracePeriodActivationValue = gracePeriodActivationValue;
   cstate->isNotContractuallyLimited  = GLMS_FALSE;
   cstate->isGracePeriodMoCreated     = isGracePeriodMoCreated;

   /* Always create with default settings */
   cstate->licenseKeyNotAvailableAlarmActive = GLMS_FALSE;
   cstate->licenseKeyExpirationAlarmState    = ALARM_CEASED;
   cstate->pendingUpdates[GLMS_PP]   = GLMS_FALSE;
   cstate->pendingUpdates[GLMS_SP]   = GLMS_FALSE;
   cstate->sendMoUpdateInd           = GLMS_FALSE;
   cstate->sendGpMoUpdateInd         = GLMS_FALSE;
   cstate->keyList                   = NULL;
   cstate->lcciSubList               = NULL;
   cstate->gpTimerId                 = 0;
   cstate->expiryTime                = 0;
   memset(cstate->currentHwacString, 0, sizeof(cstate->currentHwacString));
 
   strncpy(cstate->capacityStateId, capacityStateId,
           GLMS_MO_KEY_LEN);
   cstate->capacityStateId[GLMS_MO_KEY_LEN - 1] = '\0';

   strncpy(cstate->capacityKeyId, keyId,
           GLMS_KEY_ID_LEN);
   cstate->capacityKeyId[GLMS_KEY_ID_LEN - 1] = '\0';

   cstate->activeCapacityKeyId[0] = '\0';

   strncpy(cstate->name, name,
           GLMS_KEY_NAME_LEN);
   cstate->name[GLMS_KEY_NAME_LEN - 1] = '\0';
   
   strncpy(cstate->capacityUnit, capacityUnit,
           GLMS_CAPACITY_UNIT_LEN);
   cstate->capacityUnit[GLMS_CAPACITY_UNIT_LEN - 1] = '\0';

   strncpy(cstate->productType, productType,
           GLMS_PRODUCT_TYPE_LEN);
   cstate->productType[GLMS_PRODUCT_TYPE_LEN - 1] = '\0';

   strncpy(cstate->description, description,
           GLMS_DESCRIPTION_LEN);
   cstate->description[GLMS_DESCRIPTION_LEN - 1] = '\0';
   
   strncpy(cstate->gracePeriod.gracePeriodId, gracePeriod.gracePeriodId,
           GLMS_MO_KEY_LEN);
   cstate->gracePeriod.gracePeriodId[GLMS_MO_KEY_LEN - 1] = '\0';
   cstate->gracePeriod.gpConfiguredLength
      = gracePeriod.gpConfiguredLength;
   cstate->gracePeriod.gracePeriodState
      = gracePeriod.gracePeriodState;
   cstate->gracePeriod.gracePeriodExpiration
      = gracePeriod.gracePeriodExpiration;
   cstate->gracePeriod.gpConfiguredResetThreshold
      = gracePeriod.gpConfiguredResetThreshold;
   cstate->gracePeriod.gpConfiguredActivationThreshold
      = gracePeriod.gpConfiguredActivationThreshold;
  
   cstate->currentCapacityValue.noLimit = currentCapacityValue.noLimit;
   cstate->currentCapacityValue.value   = currentCapacityValue.value;   

   cstate->nextCapacityState = NULL;

   if (csData.lastCapacityState == NULL)
   {
      csData.lastCapacityState = cstate;
      csData.firstCapacityState = cstate;
   }
   else
   {
      csData.lastCapacityState->nextCapacityState = cstate;
      csData.lastCapacityState = cstate;
   }

   cstate->sentGpAvailableState = capacityState_getGracePeriodAvailable(cstate);

   /* Update the license state in case we have a key installed
      or an unlock method is active. */
   if(performLicenseValidation == GLMS_TRUE)
      (void)capacityState_updateKeyState(cstate);

   return cstate;
}


/* capacityState_createCapacityState creates a new CapacityState 
   data structure and MO. The capacityStateId of the new MO will
   be the same as the keyId. */
CapacityState
*capacityState_createCapacityState(char *description,
                                   char *keyId,
                                   char *name,
                                   char *capacityUnit,
                                   char *productType,
                                   time_t32 markedForDeletion,
                                   uint16_t isGracePeriodControlled)
{
   union itc_msg *sig;
   CapacityState *cstate;
   GlmsGracePeriod    gracePeriod;
   GlmsCapacityValue  currentCapacityValue;
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_createCapacityState",
              keyId);

   gracePeriod = capacityState_getDefaultGracePeriod();
   strncpy(gracePeriod.gracePeriodId, keyId,
           GLMS_MO_KEY_LEN);
   gracePeriod.gracePeriodId[GLMS_MO_KEY_LEN - 1] = '\0';

   currentCapacityValue = capacityState_getDefaultCapacityValue();
   cstate = capacityState_createCapacityStateWithoutMoIndication(keyId,
                                                    description,
                                                    keyId,
                                                    name,
                                                    capacityUnit,
                                                    productType,
                                                    GLMS_LICENSESTATE_DISABLED, /* licenseState */
                                                    markedForDeletion,
                                                    csData.tableIndexPool++,
                                                    GLMS_TRUE, /* Do license validation */
                                                    currentCapacityValue,
                                                    isGracePeriodControlled,
                                                    0, /* gracePeriodActivationValue */
                                                    gracePeriod,
                                                    0); /* isGracePeriodMoCreated */



   glms_logEvent(GLMS_LOG_LOW,
                 "Creating CapacityState MO instance for %s",
                 capacityState_getCapacityKeyId(cstate));

   /* Send Create CapacityState MO to adapter */
   sig = itc_alloc(sizeof(GlmsAdpiCreateCapacityStateMoReq),
               GLMS_ADPI_CREATE_CAPACITY_STATE_MO_REQ);
   sig->glmsCreateCapacityStateMoReq.licenseState = capacityState_getLicenseState(cstate);
   sig->glmsCreateCapacityStateMoReq.licensedCapacityLimitReached = 
                                     capacityState_getLicensedCapacityLimitReached(cstate);
   sig->glmsCreateCapacityStateMoReq.grantedCapacityLevel =
                                     capacityState_getGrantedCapacityLevel(cstate);
   sig->glmsCreateCapacityStateMoReq.currentCapacityValue =
                                     capacityState_getCurrentCapacityValue(cstate);
   strcpy(sig->glmsCreateCapacityStateMoReq.capacityUnit,
                                     capacityState_getCapacityUnit(cstate));
   strcpy(sig->glmsCreateCapacityStateMoReq.capacityStateId,
          capacityState_getCapacityStateId(cstate));
   strcpy(sig->glmsCreateCapacityStateMoReq.description,
          capacityState_getDescription(cstate));
   strcpy(sig->glmsCreateCapacityStateMoReq.keyId,
          capacityState_getCapacityKeyId(cstate));
   strcpy(sig->glmsCreateCapacityStateMoReq.activeCapacityKeyId,
          capacityState_getActiveCapacityKeyId(cstate));
   sendMsgToAdapter(sig);

   /* Store software parameters */
   cstate->pendingUpdates[GLMS_PP]   = GLMS_TRUE;
   cstate->pendingUpdates[GLMS_SP]   = GLMS_TRUE;
   capacityState_storeParameters(cstate);

   /* CapacityState MO update indication is sent at a later stage
      after license validation */

   return cstate;
}


void
capacityState_clearAllKeys()
{
   CapacityState *cs, *delCs;

   tracepoint(com_ericsson_glms, call_to_function,
              "capacityState_clearAllKeys");

   for(cs = capacityState_getFirst(); cs != NULL; )
   {
      delCs = cs;
      cs = capacityState_getNext(cs);
      capacityState_clearCapacityState(delCs);
   }
}


void
capacityState_clearCapacityState(CapacityState *csElement)
{
   CapacityState *csPrevElement = capacityState_getPrevious(csElement);
   
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_clearCapacityState",
              capacityState_getCapacityKeyId(csElement));


   if (csElement == csData.firstCapacityState &&
       csElement == csData.lastCapacityState)
   {
      csData.firstCapacityState = NULL;
      csData.lastCapacityState = NULL;
   }
   else if (csElement == csData.firstCapacityState)
      csData.firstCapacityState = csElement->nextCapacityState;
   else if (csElement == csData.lastCapacityState)
      csData.lastCapacityState = csPrevElement;

   if (csPrevElement != NULL)
       csPrevElement->nextCapacityState = csElement->nextCapacityState;

   capacityState_clearSubscriberList(csElement);
   capacityState_clearCapacityKeyList(csElement);

   free(csElement);
}

void
capacityState_doMoDeletionsAtKfParsing(struct timespec currentTime)
{
   CapacityState  *cs;
   CapacityKey    *ck, *nextCk;
   for (cs = capacityState_getFirst();
        cs != NULL;
        cs = capacityState_getNext(cs))
   {
      for (ck = capacityState_getFirstCapacityKey(cs);
           ck != NULL;
           ck = nextCk)
      {
         nextCk = capacityKey_getNext(ck);

         if (!capacityKey_isMarkedAtKfParsing(ck))
         {
            capacityKey_deleteKeyAndMo(ck);

            if (capacityState_getFirstCapacityKey(cs) == NULL &&
                capacityState_getMarkedForDeletion(cs) == 0 &&
                capacityState_getFirstSub(cs) == NULL)
               capacityState_setMarkedForDeletion(cs, currentTime.tv_sec);
               /* markedForDeletion is persistently stored below in call to
                  capacityState_storeParametersOfAllCapacityStatesAndKeys */
         }
      }
   }
}

void
capacityState_deleteCapacityStateAndMo(CapacityState *csElement)
{
   union itc_msg *sig;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempted to delete a CapacityState MO from null pointer");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_deleteCapacityStateAndMo",
              capacityState_getCapacityKeyId(csElement));

   if (capacityState_getFirstCapacityKey(csElement) != NULL &&
       capacityState_getFirstSub(csElement) != NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempted to delete a CapacityState MO which has either a"
                 " CapacityKey MO or a LIHI subscriber");
      return;
   }

   capacityState_deleteGpMo(csElement);

   glms_logEvent(GLMS_LOG_LOW,
                 "Deleting CapacityState MO instance for %s",
                 capacityState_getCapacityKeyId(csElement));

   sig = itc_alloc(sizeof(GlmsAdpiDeleteCapacityStateMoReq),
               GLMS_ADPI_DELETE_CAPACITY_STATE_MO_REQ);
   strcpy(sig->glmsDeleteCapacityStateMoReq.capacityStateId,
          capacityState_getCapacityStateId(csElement));
   sendMsgToAdapter(sig);

   sp_deleteIndex(GLMS_TABLE_RID_CAPACITYSTATE,
                  capacityState_getTableIndex(csElement));

   pp_deleteIndex(GLMS_TABLE_RID_CAPACITYSTATE,
                  capacityState_getTableIndex(csElement));

   capacityState_clearCapacityState(csElement);
}


char*
capacityState_getCapacityStateId(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read capacityStateId attribute from "
                 "null pointer in capacityState_getCapacityStateId");
      return NULL;
   }

   return csElement->capacityStateId;
}


char*
capacityState_getCapacityKeyId(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read capacityKeyId attribute from "
                 "null pointer in capacityState_getCapacityKeyId");
      return NULL;
   }

   return csElement->capacityKeyId;
}


char*
capacityState_getName(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read name attribute from "
                 "null pointer in capacityState_getName");
      return NULL;
   }

   return csElement->name;
}


void
capacityState_setName(CapacityState *csElement, char *newName)
{
   CapacityKey *ckElement;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write name attribute to "
                 "null pointer in capacityState_setName");
      return;
   }
   
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_setName",
              capacityState_getCapacityKeyId(csElement));

   if (strcmp(csElement->name, newName) != 0)
   {
      strncpy(csElement->name,
              newName,
              GLMS_KEY_NAME_LEN);
      csElement->name[GLMS_KEY_NAME_LEN - 1] = '\0';

      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;

      for (ckElement = capacityState_getFirstCapacityKey(csElement);
           ckElement != NULL;
           ckElement = capacityKey_getNext(ckElement))
         capacityKey_sendMoUpdateCapacityKeyNameInd(ckElement);
   }
}


void
capacityState_sendMoUpdateCapacityStateCapacityUnitInd(CapacityState *csElement)
{
   union itc_msg *sigSend;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "NULL pointer argument "
                 "capacityState_sendMoUpdateCapacityStateCapacityUnitInd");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_sendMoUpdateCapacityStateCapacityUnitInd",
              capacityState_getCapacityKeyId(csElement));

   if (adapter_isSubscribedForMoUpdateInd() &&
       adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateCapacityStateCapacityUnitInd),
                      GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_CAPACITY_UNIT_IND);
      strcpy(sigSend->glmsAdpiMoUpdateCapacityStateCapacityUnitInd.capacityUnit,
             capacityState_getCapacityUnit(csElement));
      strcpy(sigSend->glmsAdpiMoUpdateCapacityStateCapacityUnitInd.capacityStateId,
             capacityState_getCapacityStateId(csElement));
      sendMsgToAdapter(sigSend);
   }
}


void
capacityState_setCapacityUnit(CapacityState *csElement,
                              char *newCapacityUnit)
{
   CapacityKey *ckElement;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write capacityUnit attribute to "
                 "null pointer in capacityState_setCapacityUnit");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_setCapacityUnit",
              capacityState_getCapacityKeyId(csElement));

   if (strcmp(csElement->capacityUnit, newCapacityUnit) != 0)
   {
      strncpy(csElement->capacityUnit,
              newCapacityUnit,
              GLMS_CAPACITY_UNIT_LEN);
      csElement->capacityUnit[GLMS_CAPACITY_UNIT_LEN- 1] = '\0';

      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;

      capacityState_sendMoUpdateCapacityStateCapacityUnitInd(csElement);

      for (ckElement = capacityState_getFirstCapacityKey(csElement);
           ckElement != NULL;
           ckElement = capacityKey_getNext(ckElement))
         capacityKey_sendMoUpdateCapacityKeyCapacityUnitInd(ckElement);
   }
}


char*
capacityState_getProductType(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read productType attribute from "
                 "null pointer in capacityState_getProductType");
      return NULL;
   }

   return csElement->productType;
}


char*
capacityState_getCapacityUnit(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read capacityUnit attribute from "
                 "null pointer in capacityState_getCapacityUnit");
      return NULL;
   }

   return csElement->capacityUnit;
}


int32_t
capacityState_getGrantedCapacityLevel(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read grantedCapacityLevel attribute from "
                 "null pointer in capacityState_getGrantedCapacityLevel");
      return 0;
   }

   return csElement->currentCapacityValue.value;
}


GlmsBool
capacityState_getLicensedCapacityLimitReached(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read LicensedCapacityLimitReached attribute from "
                 "null pointer in capacityState_getLicensedCapacityLimitReached");
      return GLMS_FALSE;
   }

   return GLMS_FALSE;
}


GlmsBool
capacityState_isNotContractuallyLimited(CapacityState *csElement)
{
   if (csElement != NULL)
      return (csElement->isNotContractuallyLimited == 1) ? GLMS_TRUE : GLMS_FALSE;

   tracepoint(com_ericsson_glms, error_trace,
              "Attempt to read isNotConstractuallyLimited attribute from "
              "null pointer in capacityState_isNotContractuallyLimited");
   return GLMS_FALSE;
}


void
capacityState_setNotContractuallyLimited(CapacityState *csElement,
                                         uint32_t newNotContractuallyLimited)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to set isNotConstractuallyLimited attribute from "
                 "null pointer in capacityState_setNotContractuallyLimited");
      return;
   }

   csElement->isNotContractuallyLimited = newNotContractuallyLimited;
}


void
capacityState_copyGracePeriod(CapacityState *csElement, GlmsGracePeriod *gpDest)
{
   if (csElement == NULL ||
       gpDest == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_copyGracePeriod");
      return;
   }

   gpDest->gracePeriodExpiration =
      csElement->gracePeriod.gracePeriodExpiration;
   gpDest->gpConfiguredLength =
      csElement->gracePeriod.gpConfiguredLength;
   gpDest->gracePeriodState = 
      csElement->gracePeriod.gracePeriodState;
   gpDest->gpConfiguredResetThreshold =
      csElement->gracePeriod.gpConfiguredResetThreshold;
   gpDest->gpConfiguredActivationThreshold =
      csElement->gracePeriod.gpConfiguredActivationThreshold;

   strncpy(gpDest->gracePeriodId,
           csElement->gracePeriod.gracePeriodId,
           GLMS_MO_KEY_LEN);
   gpDest->gracePeriodId[GLMS_MO_KEY_LEN - 1] = '\0';
}


void
capacityState_setIsGpControlled(CapacityState *csElement, uint16_t isGracePeriodControlled)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_setIsGpControlled");
      return;
   }

   if (csElement->isGracePeriodControlled != isGracePeriodControlled)
   {
      csElement->isGracePeriodControlled = isGracePeriodControlled;
      csElement->sentGpAvailableState = capacityState_getGracePeriodAvailable(csElement);
      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
   }
}


uint16_t
capacityState_getIsGpControlled(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getIsGpControlled");
      return 0;
   }
   
   return csElement->isGracePeriodControlled;
}


uint32_t
capacityState_getGracePeriodAvailable(CapacityState *csElement)
{
   uint32_t unlockActive = 0;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read gracePeriodAvailable attribute from "
                 "null pointer in capacityState_getGracePeriodAvailable");
      return 1;
   }

   if (eu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED ||
       eu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
      unlockActive = 1;

   if (iu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED ||
       iu_getActivationState() == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
      unlockActive = 1;

   if (capacityState_getIsGpControlled(csElement) &&
       capacityState_getGracePeriodExpiration(csElement) == 0 &&
       capacityState_isNotContractuallyLimited(csElement) == 0 &&
       unlockActive == 0)
      /* GP Monitoring shall be performed by the client */
      return 1;

   /* GP Monitoring shall not be performed by the client */
   return 0;
}


void
capacityState_setGracePeriodState(CapacityState *csElement,
                                  GlmsActivationState gpState)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to set gracePeriodState attribute from "
                 "null pointer in capacityState_setGracePeriodState");
      return;
   }

   if (csElement->gracePeriod.gracePeriodState != gpState)
   {
      csElement->gracePeriod.gracePeriodState = gpState;
      csElement->sendGpMoUpdateInd = GLMS_TRUE;
   }
}


GlmsActivationState
capacityState_getGracePeriodState(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to get gracePeriodState attribute from "
                 "null pointer in capacityState_getGracePeriodState");
      return GLMS_ACTIVATION_STATE_EXPIRED;
   }
   
   return csElement->gracePeriod.gracePeriodState;
}


GlmsBool
capacityState_isGpActive(CapacityState *csElement)
{
   if(csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to check gracePeriodActive information from "
                 "null pointer in capacityState_isGpActive");
      return GLMS_FALSE;
   }

   return (capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_ACTIVATED ||
           capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
}


void
capacityState_setGracePeriodExpirationAndState(CapacityState *csElement,
                                               time_t32 gpExpiration)
{
   struct timespec currentTime;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to set gracePeriodExpiration attribute from "
                 "null pointer in capacityState_setGracePeriodExpirationAndState");
      return;
   }
   
   if (csElement->gracePeriod.gracePeriodExpiration != gpExpiration)
   {
      csElement->gracePeriod.gracePeriodExpiration = gpExpiration;

      clock_gettime(CLOCK_REALTIME, &currentTime);
      if (gpExpiration == 0)
         capacityState_setGracePeriodState(csElement, GLMS_ACTIVATION_STATE_INACTIVE);
      else if (currentTime.tv_sec > (gpExpiration - glms_getWarningPeriod()) &&
               currentTime.tv_sec < gpExpiration)
         capacityState_setGracePeriodState(csElement, GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
      else if(currentTime.tv_sec < gpExpiration)
         capacityState_setGracePeriodState(csElement, GLMS_ACTIVATION_STATE_ACTIVATED);
      else if(currentTime.tv_sec > gpExpiration)
         capacityState_setGracePeriodState(csElement, GLMS_ACTIVATION_STATE_EXPIRED);
      else
      {
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown GP state: gpExpiration", gpExpiration);
         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown GP state: currentTime", currentTime.tv_sec);
         capacityState_setGracePeriodState(csElement, 
                                           GLMS_ACTIVATION_STATE_EXPIRED);
      }
      
      csElement->pendingUpdates[GLMS_PP] = GLMS_TRUE;
      csElement->sendGpMoUpdateInd = GLMS_TRUE;
   }
}


time_t32
capacityState_getGracePeriodExpiration(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to get gracePeriodExpiration attribute from "
                 "null pointer in capacityState_getGracePeriodExpiration");
      return 0;
   }
   
   return csElement->gracePeriod.gracePeriodExpiration;
}


void
capacityState_setGracePeriodActivationValue(CapacityState *csElement, uint32_t value)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_setGracePeriodActivationValue");
      return;
   }
   
   if (csElement->gracePeriodActivationValue != value)
   {
      csElement->gracePeriodActivationValue = value;
      csElement->pendingUpdates[GLMS_PP] = GLMS_TRUE;
   }
}


uint32_t
capacityState_getGracePeriodActivationValue(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getGracePeriodActivationValue");
      return 0;
   }
   
   return csElement->gracePeriodActivationValue;
}


void
capacityState_setGpConfigurableAttributes(CapacityState *csElement,
                                          int32_t activationThreshold,
                                          int32_t resetThreshold,
                                          int32_t length)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_setGpConfigurableAttributes");
      return;
   }
   
   glms_logEvent(GLMS_LOG_LOW,
                 "Setting GP attributes to; Activaton Threshold = %d, "
                 "Reset Threshold = %d, Configured Length = %d",
                 activationThreshold,
                 resetThreshold,
                 length);

   if (csElement->gracePeriod.gpConfiguredActivationThreshold != activationThreshold ||
       csElement->gracePeriod.gpConfiguredResetThreshold != resetThreshold ||
       csElement->gracePeriod.gpConfiguredLength != length)
   {
      csElement->gracePeriod.gpConfiguredActivationThreshold = activationThreshold;
      csElement->gracePeriod.gpConfiguredResetThreshold = resetThreshold;
      csElement->gracePeriod.gpConfiguredLength = length;
      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      csElement->sendGpMoUpdateInd = GLMS_TRUE;
   }
}


GlmsBool
capacityState_activateGp(CapacityState *csElement)
{
   time_t32 gpExpiration;
   struct timespec currentTime;
   uint32_t updateResult;

   tracepoint(com_ericsson_glms, call_to_function,
              "capacityState_activateGp");

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_activateGp");
      return GLMS_FALSE;
   }

  if (capacityState_isNotContractuallyLimited(csElement) == GLMS_TRUE ||
      capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_EXPIRED)
     /* If Capacity have notContractuallyLimited tag or GP has already expired
        then GP will not be triggered */
     return GLMS_FALSE;

  if (capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_ACTIVATED||
      capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
     return GLMS_TRUE;

  if (capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_INACTIVE &&
      capacityState_getIsGpControlled(csElement))
  {
     gpExpiration = capacityState_getGpLength(csElement);

     csElement->gpTimerId = requestTimer(gpExpiration - glms_getWarningPeriod(),
                                         GLMS_FALSE, GLMS_GP_EXPIRY_WARNING_TIMER);

     clock_gettime(CLOCK_REALTIME, &currentTime);
     capacityState_setGracePeriodExpirationAndState(csElement,
                                                    (currentTime.tv_sec + gpExpiration));

     capacityState_setGracePeriodActivationValue(csElement, 
                                                 csElement->currentCapacityValue.value);

     
     /* Update Capacity State with GP activation */
     updateResult = capacityState_updateKeyState(csElement);
     if (updateResult == 1 || /* License Key updated */
         updateResult == 2)   /* Unlock method or GP activated or ceased */
     {
        capacityState_sendLcciChangeIndToAllSubscribers(csElement);
        capacityState_sendMoUpdateCapacityStateInd(csElement);
     }
     
     if (updateResult == 1 || /* License Key updated */
         updateResult == 3)   /* License Key updated while unlock method active */
     {
        clock_gettime(CLOCK_REALTIME, &currentTime);
        lm_setLastInventoryChange(currentTime.tv_sec);
        lm_sendMoUpdateLmInd();
     }
     
     capacityState_storeParameters(csElement);
     capacityState_sendMoUpdateGracePeriodInd(csElement);
     capacityState_sendGpAlarmInd(csElement, GLMS_ALARM_ACTIVATED, GLMS_ALARM_MINOR);

     glms_logEvent(GLMS_LOG_LOW,
                   "Activated Grace Period for %s",
                   capacityState_getCapacityKeyId(csElement));

     return GLMS_TRUE;
  }

  return GLMS_FALSE;
}


void
capacityState_handleGpWarningTimer(uint32_t timerId)
{
   CapacityState *csElement; 

   tracepoint(com_ericsson_glms, call_to_function,
              "capacityState_handleGpWarningTimer");

   csElement  = capacityState_findByGpTimerId(timerId);    
   if (csElement == NULL)
      return;
   
   csElement->gpTimerId = requestTimer(glms_getWarningPeriod(),
                                       GLMS_FALSE,
                                       GLMS_GP_EXPIRY_TIMER);
   
   capacityState_setGracePeriodState(csElement,
                                     GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING);
   capacityState_sendMoUpdateGracePeriodInd(csElement);
   capacityState_sendGpAlarmInd(csElement,
                                GLMS_ALARM_ACTIVATED,
                                GLMS_ALARM_MAJOR);
}


void
capacityState_handleGpExpiryTimer(uint32_t timerId)
{
   uint32_t updateResult;
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function,
              "capacitystate_handleGpExpiryTimer");
   CapacityState *csElement;
   
   csElement  = capacityState_findByGpTimerId(timerId);   
   if (csElement == NULL)
     return;

   csElement->gpTimerId = 0; 
   capacityState_setGracePeriodState(csElement, GLMS_ACTIVATION_STATE_EXPIRED);
   capacityState_sendMoUpdateGracePeriodInd(csElement);
   capacityState_sendGpAlarmInd(csElement, GLMS_ALARM_CEASED, GLMS_ALARM_NONE);

   /* Set capacity Value as per LKF */
   updateResult = capacityState_updateKeyState(csElement);
   if (updateResult == 1 || /* License Key updated */
       updateResult == 2)   /* Unlock method activated or ceased */
   {
      capacityState_sendLcciChangeIndToAllSubscribers(csElement);
      capacityState_sendMoUpdateCapacityStateInd(csElement);
   }
   
   if (updateResult == 1 || /* License Key updated */
       updateResult == 3)   /* License Key updated while unlock method active */
   {
      clock_gettime(CLOCK_REALTIME, &currentTime);
      lm_setLastInventoryChange(currentTime.tv_sec);
      lm_sendMoUpdateLmInd();
   }
}


void
capacityState_sendGpAlarmInd(CapacityState *cs,
                             GlmsAlarmState alarmState,
                             GlmsAlarmSeverity alarmSeverity)
{
   union itc_msg *sig;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "capacityState_sendGpAlarmInd",
              alarmState);

   sig = itc_alloc(sizeof(GlmsAdpiGpAlarmInd),
               GLMS_ADPI_GP_ALARM_IND);
   sig->glmsAdpiGpAlarmInd.alarmState = alarmState;
   sig->glmsAdpiGpAlarmInd.alarmSeverity  = alarmSeverity;
   strcpy(sig->glmsAdpiGpAlarmInd.gracePeriodId,
          capacityState_getCapacityStateId(cs));
   sendMsgToAdapter(sig);
}


void
capacityState_handleGlmsRestart()
{
   struct timespec currentTime;
   CapacityState *csElement = capacityState_getFirst();
   
   tracepoint(com_ericsson_glms, call_to_function,
              "capacityState_handleGlmsRestart");

   clock_gettime(CLOCK_REALTIME, &currentTime);

   while (csElement != NULL)
   {
      if (capacityState_getGracePeriodState(csElement) ==
         GLMS_ACTIVATION_STATE_ACTIVATED)
      {
         csElement->currentCapacityValue.noLimit = 1;
         csElement->gpTimerId = requestTimer(
                                 capacityState_getGracePeriodExpiration(csElement)
                                 - currentTime.tv_sec
                                 - glms_getWarningPeriod(),
                                 GLMS_FALSE,
                                 GLMS_GP_EXPIRY_WARNING_TIMER);

         capacityState_sendGpAlarmInd(csElement, GLMS_ALARM_ACTIVATED, GLMS_ALARM_MINOR);
      }
      else if (capacityState_getGracePeriodState(csElement) ==
               GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
      {
         csElement->currentCapacityValue.noLimit = 1;
         csElement->gpTimerId = requestTimer(
                                      capacityState_getGracePeriodExpiration(csElement)
                                      - currentTime.tv_sec,
                                      GLMS_FALSE,
                                      GLMS_GP_EXPIRY_TIMER);

         capacityState_sendGpAlarmInd(csElement, GLMS_ALARM_ACTIVATED, GLMS_ALARM_MAJOR);
      }

      capacityState_sendMoUpdateGracePeriodInd(csElement);
      csElement = capacityState_getNext(csElement);
   }
}


char *
capacityState_getGracePeriodId(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read gracePeriodId attribute from "
                 "null pointer in capacityState_getGracePeriodId");
      return NULL;
   }
   
   return csElement->gracePeriod.gracePeriodId;  
}


int32_t
capacityState_getGracePeriodResetThreshold(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read gpConfiguredResetThreshold attribute from "
                 "null pointer in capacityState_getGracePeriodResetThreshold");
      return 0;
   }
   
   return csElement->gracePeriod.gpConfiguredResetThreshold;  
}


int32_t
capacityState_getGracePeriodActivationThreshold(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read gpConfiguredActivationThreshold attribute from "
                 "null pointer in capacityState_getGracePeriodActivationThreshold");
      return 0;
   }

   return csElement->gracePeriod.gpConfiguredActivationThreshold;  
}


int32_t
capacityState_getGpResetCapacityValue(CapacityState *csElement)
{
   int32_t capacityResetValue, capacityThresholdValue;
   
   capacityResetValue = capacityState_getGracePeriodActivationValue(csElement);
   capacityThresholdValue = ((capacityState_getGracePeriodResetThreshold(csElement) *
                              capacityState_getGracePeriodActivationValue(csElement)) / 100);

   if (capacityThresholdValue == 0)
   {
      /* Capacity needs to be increased by atleast 1 to reset GP. */
      capacityThresholdValue = 1;
   }

   capacityResetValue += capacityThresholdValue;

   return capacityResetValue;
}


uint32_t
capacityState_getGpConfiguredLength(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read gpConfiguredLength attribute from "
                 "null pointer in capacityState_getGpConfiguredLength");
      return 0;
   }
   
   return csElement->gracePeriod.gpConfiguredLength;  
}


time_t32
capacityState_getGpLength(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read gpConfiguredLength attribute from "
                 "null pointer in capacityState_getGpLength");
      return 0;
   }
   
   return (time_t32)((capacityState_getGpConfiguredLength(csElement) *
                    GLMS_SECONDS_IN_ONE_DAY) /
                   glms_getTimeScaling());
}


GlmsGracePeriod
capacityState_getDefaultGracePeriod()
{
   GlmsGracePeriod gracePeriod;
   
   strcpy(gracePeriod.gracePeriodId,"");
   gracePeriod.gpConfiguredLength               = 14  /* default is 14 days */;
   gracePeriod.gracePeriodState                 = GLMS_ACTIVATION_STATE_INACTIVE;
   gracePeriod.gracePeriodExpiration            = 0;
   gracePeriod.gpConfiguredResetThreshold       = 10; /* default 10% */
   gracePeriod.gpConfiguredActivationThreshold  = 1;  /* default 0.1% */

   return gracePeriod;
}


GlmsCapacityValue
capacityState_getCurrentCapacityValue(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read currentCapacityValue attribute from "
                 "null pointer in capacityState_getCurrentCapacityValue");
      return capacityState_getDefaultCapacityValue();
   }

   return csElement->currentCapacityValue;
}


void
capacityState_setCurrentCapacityValue(CapacityState *csElement,
                                      int32_t capacityValue,
				      GlmsBool noLimit)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to set currentCapacityValue attribute from "
                 "null pointer in capacityState_setCurrentCapacityValue");
      return;
   }
   
   if (csElement->currentCapacityValue.value != capacityValue ||
       csElement->currentCapacityValue.noLimit != noLimit)
   {
      csElement->currentCapacityValue.value = capacityValue;
      csElement->currentCapacityValue.noLimit = noLimit;
      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      csElement->sendMoUpdateInd = GLMS_TRUE;
   }
}


GlmsCapacityValue
capacityState_getDefaultCapacityValue()
{
   GlmsCapacityValue licensedCapacityValue;
   
   licensedCapacityValue.value = 0;
   licensedCapacityValue.noLimit = GLMS_FALSE;
   
   return licensedCapacityValue;
}


void
capacityState_setProductType(CapacityState *csElement, char *newProductType)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write productType attribute to "
                 "null pointer in capacityState_setProductType");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_setProductType",
              capacityState_getCapacityKeyId(csElement));

   if (strcmp(csElement->productType, newProductType) != 0)
   {
      strncpy(csElement->productType,
              newProductType,
              GLMS_PRODUCT_TYPE_LEN);
      csElement->productType[GLMS_PRODUCT_TYPE_LEN - 1] = '\0';

      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      
      /* No MO update indication is required for CapacityKey since the 
         productType should be set before the CapacityKey MO is created. */
   }
}


char*
capacityState_getDescription(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read description attribute from null pointer");
      return NULL;
   }

   return csElement->description;
}


void
capacityState_setDescription(CapacityState *csElement, char *newDescription)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write description attribute to "
                 "null pointer in capacityState_setDescription");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_setDescription",
              capacityState_getCapacityKeyId(csElement));

   if (strcmp(csElement->description, newDescription) != 0)
   {
      strncpy(csElement->description,
              newDescription,
              GLMS_DESCRIPTION_LEN);
      csElement->description[GLMS_DESCRIPTION_LEN - 1] = '\0';

      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
      csElement->sendMoUpdateInd = GLMS_TRUE;
   }
}


time_t32
capacityState_getMarkedForDeletion(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read markedForDeletion attribute from "
                 "null pointer in capacityState_getMarkedForDeletion");
      return 0;
   }

   return csElement->markedForDeletion;
}


void
capacityState_setMarkedForDeletion(CapacityState *csElement, time_t32 newMarkedForDeletion)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write markedForDeletion attribute to "
                 "null pointer in capacityState_setMarkedForDeletion");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_setMarkedForDeletion",
              capacityState_getCapacityKeyId(csElement));

   if (csElement->markedForDeletion != newMarkedForDeletion)
   {
      csElement->markedForDeletion = newMarkedForDeletion;
      csElement->pendingUpdates[GLMS_SP] = GLMS_TRUE;
   }
}


GlmsBool
capacityState_isMoDeleteTimeoutReached(CapacityState *csElement)
{
   struct timespec currentTime;
   time_t32 deleteMark = capacityState_getMarkedForDeletion(csElement);

   clock_gettime(CLOCK_REALTIME, &currentTime);

   /* deleteMark == 0 means the MO is not marked for deletion*/
   if ((deleteMark != 0) &&
       (currentTime.tv_sec >= (deleteMark + GLMS_PENDING_MO_DELETE_TIME)))
      return GLMS_TRUE;

   return GLMS_FALSE;
}


GlmsLicenseState
capacityState_getLicenseState(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read licenseState attribute from "
                 "null pointer in capacityState_getLicenseState");
      return GLMS_LICENSESTATE_DISABLED;
   }

   return csElement->licenseState;
}


void
capacityState_setLicenseState(CapacityState *csElement,
                             GlmsLicenseState newLicenseState)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write licenseState attribute to "
                 "null pointer in capacityState_setLicenseState");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_setLicenseState",
              capacityState_getCapacityKeyId(csElement));

   if (csElement->licenseState != newLicenseState)
   {
      csElement->licenseState = newLicenseState;
      csElement->sendMoUpdateInd = GLMS_TRUE;
   }
}


GlmsLicenseState
capacityState_getLicenseStateAccordingToKf(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read licenseStateAccordingToKf"
                 " attribute from null pointer");
      return GLMS_LICENSESTATE_DISABLED;
   }

   return csElement->licenseStateAccordingToKf;
}


void
capacityState_setLicenseStateAccordingToKf(CapacityState *csElement,
                                          GlmsLicenseState newLicenseStateAccordingToKf)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to write licenseStateAccordingToKf"
                 " attribute to null pointer");
      return;
   }

   csElement->licenseStateAccordingToKf = newLicenseStateAccordingToKf;
}


void
capacityState_setLicenseStateAccordingToKfForAll()
{
   CapacityState  *ck;

   tracepoint(com_ericsson_glms, call_to_function,
              "capacityState_setLicenseStateAccordingToKfForAll");

   for (ck = capacityState_getFirst();
        ck != NULL;
        ck = capacityState_getNext(ck))
   {
      capacityState_setLicenseStateAccordingToKf(ck, capacityState_getLicenseState(ck));
   }
}


GlmsBool
capacityState_fillLcciLicenseInfoByKeyId(LcciLicenseInfoS *dstLicInfo,
                                        char *licenseKeyId)
{
   CapacityState *capacityState;
   GlmsCapacityValue  currentCapacityValue;
   capacityState = capacityState_findByKeyId(licenseKeyId);

   if (capacityState == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Failed to find License Key in call to "
                 "capacityState_fillLcciLicenseInfoByKeyId",
                 licenseKeyId);
      return GLMS_FALSE;
   }

   strncpy(dstLicInfo->licenseKeyId,
           licenseKeyId,
           MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID);
   dstLicInfo->licenseKeyId[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';
   dstLicInfo->gracePeriodAvailable = wr16(capacityState_getGracePeriodAvailable(capacityState));
   dstLicInfo->gracePeriodActivationThreshold =
                            wr32(capacityState_getGracePeriodActivationThreshold(capacityState));
   currentCapacityValue = capacityState_getCurrentCapacityValue(capacityState);

   if(currentCapacityValue.noLimit)
      dstLicInfo->capacityLimit.noLimit = wr16(1);
   else
      dstLicInfo->capacityLimit.noLimit = wr16(0);

   dstLicInfo->capacityLimit.value = wr32(currentCapacityValue.value);
   dstLicInfo->licenseState = wr16(capacityState_getLicenseState(capacityState));

   return GLMS_TRUE;
}

GlmsBool
capacityState_fillLcciLicenseInfo2ByKeyId(LcciLicenseInfo2S *dstLicInfo,
                                          char *licenseKeyId)
{
   CapacityState *capacityState;
   GlmsCapacityValue  currentCapacityValue;
   capacityState = capacityState_findByKeyId(licenseKeyId);

   if (capacityState == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Failed to find License Key in call to "
                 "capacityState_fillLcciLicenseInfo2ByKeyId",
                 licenseKeyId);
      return GLMS_FALSE;
   }

   strncpy(dstLicInfo->licenseKeyId,
           licenseKeyId,
           MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID);
   dstLicInfo->licenseKeyId[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';
   dstLicInfo->gracePeriodAvailable = wr16(capacityState_getGracePeriodAvailable(capacityState));
   dstLicInfo->gracePeriodActivationThreshold =
                            wr32(capacityState_getGracePeriodActivationThreshold(capacityState));
   currentCapacityValue = capacityState_getCurrentCapacityValue(capacityState);

   if(currentCapacityValue.noLimit)
      dstLicInfo->capacityLimit.noLimit = wr16(1);
   else
      dstLicInfo->capacityLimit.noLimit = wr16(0);

   dstLicInfo->capacityLimit.value = wr32(currentCapacityValue.value);
   dstLicInfo->licenseState = wr16(capacityState_getLicenseState(capacityState));

   strcpy(dstLicInfo->hwacString, capacityState_getHwacString(capacityState));

   return GLMS_TRUE;
}

static char *
capacityState_getHwacString(CapacityState *csElement)
{

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getHwacString");
      return "";
   }

   CapacityKey *ck = capacityKey_findByCapacityKeyId(capacityState_getActiveCapacityKeyId(csElement));
   if (!ck)
   {
     return "";
   }
  
   return ck->hwacString;
}

void
capacityState_resetAllKfParsingMarks()
{
   CapacityState *csElement;
   CapacityKey *ckElement;

   for (csElement = capacityState_getFirst();
        csElement != NULL;
        csElement = capacityState_getNext(csElement))
   {
      for (ckElement = capacityState_getFirstCapacityKey(csElement);
           ckElement != NULL;
           ckElement = capacityKey_getNext(ckElement))
         capacityKey_resetKfParsingMark(ckElement);
   }
}


uint32_t
capacityState_getTableIndex(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getTableIndex");
      return 0;
   }

   return csElement->tableIndex;
}


void
capacityState_clearCapacityKeyList(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_clearCapacityKeyList");
      return;
   }

   if (csElement->keyList == NULL)
      return;

   capacityKey_clearAllKeys(csElement->keyList);
   csElement->keyList = NULL;
}


CapacityKey*
capacityState_getFirstCapacityKey(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getFirstCapacityKey");
      return NULL;
   }
   
   return csElement->keyList;
}


void
capacityState_setFirstCapacityKey(CapacityState *csElement, CapacityKey *ckElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_setFirstCapacityKey");
      return;
   }
   
   csElement->keyList = ckElement;
}


struct CapacityKey_s 
*capacityState_findCapacityKeyByStartDateAndExpirationAndCapacityValue(CapacityState *cs,
                                                                       time_t32 startDate,
                                                                       time_t32 expiration,
                                                                       GlmsCapacityValue glmsCapacityValue,
                                                                       char *hwacString)
{
   CapacityKey *ck;
   GlmsCapacityValue capacityValue;

   if (cs == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "capacityState_findCapacityKeyByStartDateAndExpirationAndCapacityValue");
      return NULL;
   }

   
   for (ck = capacityState_getFirstCapacityKey(cs);
        ck != NULL;
        ck = capacityKey_getNext(ck))
   {
      capacityValue = capacityKey_getLicensedCapacityValue(ck);

      if ((difftime(capacityKey_getValidFrom(ck), startDate) == 0) &&
          (difftime(capacityKey_getExpiration(ck), expiration) == 0)&&
          (glmsCapacityValue.noLimit == capacityValue.noLimit) &&
	      (glmsCapacityValue.value == capacityValue.value) &&
          (strcmp(capacityKey_getHwacString(ck),hwacString) == 0))
          
         return ck;
   }

   return NULL;
}


char *
capacityState_getActiveCapacityKeyId(CapacityState *cs)
{
   return cs ? cs->activeCapacityKeyId : NULL;
}


void
capacityState_updateActiveCapacityKey(CapacityState *cs, CapacityKey *ck)
{
   if (ck == NULL &&
       cs->activeCapacityKeyId[0] != '\0')
   {
      cs->activeCapacityKeyId[0] = '\0';
      cs->currentHwacString[0] = '\0';
      cs->sendMoUpdateInd = GLMS_TRUE;
   }
   else if (ck != NULL &&
            !compareKeyIds(cs->activeCapacityKeyId, capacityKey_getCapacityKeyId(ck)))
   {
      strncpy(cs->activeCapacityKeyId,
              capacityKey_getCapacityKeyId(ck),
              GLMS_MO_KEY_LEN);
      cs->activeCapacityKeyId[GLMS_MO_KEY_LEN - 1] = '\0';

      strncpy(cs->currentHwacString,
              ck->hwacString,
              MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING);
      cs->currentHwacString[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING - 1] = '\0';

      cs->sendMoUpdateInd = GLMS_TRUE;
   }
}

void
findMostGenerousCapacityKey(CapacityState *csElement,
                            time_t32 *validFrom,
                            time_t32 *expiration,
                            GlmsCapacityValue *newCapacityValue,
                            GlmsBool *noLimitLicenseFound,
                            uint32_t *newNotContractuallyLimited,
                            char **newHwacString,
                            time_t32 *consecutiveExpiration)
{
   CapacityKey *tmpi;
   struct timespec currentTime;
   uint32_t loopAgain;

   clock_gettime(CLOCK_REALTIME, &currentTime);
   tmpi = csElement->keyList;
   newCapacityValue->noLimit = 0;
   newCapacityValue->value = 0;
   *validFrom = 0;
   *expiration = 0;
   *noLimitLicenseFound = GLMS_FALSE;
   *newNotContractuallyLimited = 0;
   *consecutiveExpiration = 0;

   while (tmpi != NULL)
   {
      if (!(capacityKey_getValidFrom(tmpi) <= currentTime.tv_sec &&
          (capacityKey_getExpiration(tmpi) == GLMS_NO_STOP_DATE ||
           capacityKey_getExpiration(tmpi) > currentTime.tv_sec)))
      {
         tmpi = capacityKey_getNext(tmpi);
         continue;
      }

      if (tmpi->licensedCapacityValue.noLimit ||
          tmpi->notContractuallyLimited)
      {
         newCapacityValue->noLimit    = tmpi->licensedCapacityValue.noLimit;
         newCapacityValue->value      = tmpi->licensedCapacityValue.value;
         *validFrom                   = capacityKey_getValidFrom(tmpi);
         *expiration                  = capacityKey_getExpiration(tmpi);
         *consecutiveExpiration       = capacityKey_getExpiration(tmpi);
         *noLimitLicenseFound         = GLMS_TRUE;
         *newNotContractuallyLimited  = tmpi->notContractuallyLimited;
         *newHwacString = capacityKey_getHwacString(tmpi);
      }
      else if (tmpi->licensedCapacityValue.value >= newCapacityValue->value &&
               *noLimitLicenseFound == GLMS_FALSE)
      {
         newCapacityValue->noLimit = tmpi->licensedCapacityValue.noLimit;
         newCapacityValue->value   = tmpi->licensedCapacityValue.value;
         *validFrom                = capacityKey_getValidFrom(tmpi);
         *expiration               = capacityKey_getExpiration(tmpi);
         *consecutiveExpiration       = capacityKey_getExpiration(tmpi);
         *newNotContractuallyLimited  = tmpi->notContractuallyLimited;
         *newHwacString = capacityKey_getHwacString(tmpi);
      }

      tmpi = capacityKey_getNext(tmpi);
   }

   loopAgain = *validFrom ? 1: 0;
   while(loopAgain)
   {
      loopAgain = 0;
      tmpi = csElement->keyList;
      while(*consecutiveExpiration != GLMS_NO_STOP_DATE &&
            tmpi != NULL)
      {
         if (capacityKey_getValidFrom(tmpi) <= *consecutiveExpiration &&
             (capacityKey_getExpiration(tmpi) > *consecutiveExpiration ||
              capacityKey_getExpiration(tmpi) == GLMS_NO_STOP_DATE))
         {
            *consecutiveExpiration = capacityKey_getExpiration(tmpi);
            loopAgain = 1;
         }

         tmpi = capacityKey_getNext(tmpi);
      }
   }
}

/* capacityState_updateKeyState updates the license value of a capacityState MO.
   It will also check if the CapacityState MO shall be deleted.

   It returns one of the following values:
   
   0: No change in license value, or the MO is deleted.
   1: License changed as a result of License Key value. Send change ind and
      update inventory.
   2: License change as result of unlock period activated or ceased. Send 
      change ind but dont update inventory. Starting GP while a unlock
      method is active may fall into this cathegory if gracePeriodAvailable
      go from 1 to 0, but all other parameters stay the same.
   3: License state didn't change since an unlock method is active, but
      the License Key values changed so an inventory update is required.
*/
uint32_t
capacityState_updateKeyState(CapacityState *csElement)
{
   time_t32 validFrom, expiration, consecutiveExpiration;
   GlmsCapacityValue oldCapacityValueAccordingToKf;
   GlmsCapacityValue newCapacityValue;
   uint32_t newNotContractuallyLimited, oldNotContractuallyLimited;
   GlmsBool updateInventoryTime, noLimitLicenseFound;
   char *currentHwacString = csElement->currentHwacString;
   char *newHwacString = "";
   GlmsBool sendChangeInd = GLMS_FALSE;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_updateKeyState");
      return 0;
   }

   if (keyFile_isRestricted(csElement->capacityKeyId))
      /* Capacity license is restricted, nothing to do */
      return 0;

   if (capacityState_isMoDeleteTimeoutReached(csElement))
   {
      capacityState_deleteCapacityStateAndMo(csElement);
      return 0;
   }

   findMostGenerousCapacityKey(csElement,
                               &validFrom,
                               &expiration,
                               &newCapacityValue,
                               &noLimitLicenseFound,
                               &newNotContractuallyLimited,
                               &newHwacString,
                               &consecutiveExpiration);

   csElement->expiryTime = consecutiveExpiration;

   /* See if the lastInventoryChange attribute shall be updated */
   oldCapacityValueAccordingToKf = capacityState_getCurrentCapacityValue(csElement);
   updateInventoryTime = GLMS_FALSE;
   if ((oldCapacityValueAccordingToKf.value != newCapacityValue.value) ||
       (oldCapacityValueAccordingToKf.noLimit != newCapacityValue.noLimit))
      /* The license values have changed according to the
         LKF so update the inventory time */
      updateInventoryTime = GLMS_TRUE;

   /* We will not reduce capacity when Autonomous Mode is active */
   if (am_getActivationState() != GLMS_ACTIVATION_STATE_INACTIVE)
   {
      if (oldCapacityValueAccordingToKf.noLimit > newCapacityValue.noLimit ||
          oldCapacityValueAccordingToKf.value > newCapacityValue.value)
      {
         newCapacityValue.noLimit = oldCapacityValueAccordingToKf.noLimit;
         newCapacityValue.value   = oldCapacityValueAccordingToKf.value;
      }
   }

   /* If HWAC has changed, send a change indication over LCCI */
   if(currentHwacString && newHwacString && (strcmp(currentHwacString, newHwacString) != 0))
   {
      tracepoint(com_ericsson_glms, info_trace_w_str_arg,
                 "Hwac string changed, newHwacString ", newHwacString);
      sendChangeInd = GLMS_TRUE;
   }
   
   capacityState_updateActiveCapacityKey(csElement, 
                capacityState_findCapacityKeyByStartDateAndExpirationAndCapacityValue
                                         (csElement,
                                          validFrom,
                                          expiration,
                                          newCapacityValue,
                                          newHwacString));

   /* If an unlock method or GP is active we set noLimit to true */
   if (eu_isEuActive() ||
       iu_isIuActive() ||
       pu_isPuActive() ||
       capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_ACTIVATED ||
       capacityState_getGracePeriodState(csElement) == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
      newCapacityValue.noLimit = 1;


   /* If either noLimit, value, gpAvailable or HWAC has changed we send
      a change indication over LCCI */
   
   if (newCapacityValue.noLimit != oldCapacityValueAccordingToKf.noLimit ||
       newCapacityValue.value != oldCapacityValueAccordingToKf.value ||
       csElement->sentGpAvailableState != capacityState_getGracePeriodAvailable(csElement))
   {
      tracepoint(com_ericsson_glms, capacity_value_updated,
                 capacityState_getCapacityKeyId(csElement),
                 newCapacityValue.noLimit,
                 newCapacityValue.value);

      sendChangeInd = GLMS_TRUE;
      capacityState_setCurrentCapacityValue(csElement,
                                            newCapacityValue.value,
                                            newCapacityValue.noLimit);

      csElement->sentGpAvailableState = capacityState_getGracePeriodAvailable(csElement);
   }
   
   /* Update the license state of CapacityState */
   if (newCapacityValue.noLimit == 0 &&
       newCapacityValue.value == 0)
      capacityState_setLicenseState(csElement, GLMS_LICENSESTATE_DISABLED);
   else
      capacityState_setLicenseState(csElement, GLMS_LICENSESTATE_ENABLED);

   oldNotContractuallyLimited = capacityState_isNotContractuallyLimited(csElement);
   if (oldNotContractuallyLimited != newNotContractuallyLimited)
   {
      capacityState_setNotContractuallyLimited(csElement, newNotContractuallyLimited);

      if (capacityState_isGracePeriodMoCreated(csElement) == GLMS_FALSE &&
          capacityState_getIsGpControlled(csElement) == 1 &&
          capacityState_isNotContractuallyLimited(csElement) == GLMS_FALSE)
         capacityState_createGpMo(csElement);
      else if (capacityState_isGracePeriodMoCreated(csElement) == GLMS_TRUE &&
               (capacityState_getIsGpControlled(csElement) == 0 ||
                capacityState_isNotContractuallyLimited(csElement) == GLMS_TRUE))
         capacityState_deleteGpMo(csElement);
   }
   
   capacityState_handleLicenseKeyNotAvailableAlarm(csElement);
   capacityState_handleLicenseKeyCloseToExpirationAlarm(csElement);


   if (sendChangeInd == GLMS_TRUE && updateInventoryTime == GLMS_TRUE)
      return 1;

   if (sendChangeInd == GLMS_TRUE && updateInventoryTime == GLMS_FALSE)
      return 2;

   if (sendChangeInd == GLMS_FALSE && updateInventoryTime == GLMS_TRUE)
      return 3;

   return 0;
}


uint32_t
capacityState_getNrOfCapacityKeyMos()
{
   CapacityState *cs;
   CapacityKey *ck;
   uint32_t nrOfKeys = 0;

   for (cs = capacityState_getFirst();
        cs != NULL;
        cs = capacityState_getNext(cs))
   {
      for (ck = capacityState_getFirstCapacityKey(cs);
           ck != NULL;
           ck = capacityKey_getNext(ck))
         nrOfKeys++;
   }

   return nrOfKeys;
}


uint32_t
capacityState_getNrOfCapacityStateMos()
{
   CapacityState *cs;
   uint32_t nrOfKeys = 0;

   for (cs = capacityState_getFirst();
        cs != NULL;
        cs = capacityState_getNext(cs))
      nrOfKeys++;

   return nrOfKeys;
}


GlmsBool
capacityState_haveAnInstalledLicenseKey(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "capacityState_haveAnInstalledLicenseKey");
      return GLMS_FALSE;
   }

   return (csElement->keyList == NULL) ? GLMS_FALSE : GLMS_TRUE;
}


void
capacityState_addCapacityKey(CapacityState *csElement,
                           struct CapacityKey_s *ckElement)
{
   CapacityKey *ck;

   if (csElement == NULL || ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_addCapacityKey");
      return;
   }

   ck = capacityState_getFirstCapacityKey(csElement);
   if (ck == NULL)
   {
      csElement->keyList = ckElement;
      ckElement->nextCapacityKey = NULL;
      ckElement->prevCapacityKey = NULL;
   }
   else
   {
      while(ck->nextCapacityKey != NULL)
         ck = ck->nextCapacityKey;

      ck->nextCapacityKey = ckElement;
      ckElement->nextCapacityKey = NULL;
      ckElement->prevCapacityKey = ck;
   }

   return;
}


/* capacityState_clearSubscriptionsByKeyIdMidAndServerRef returns true if a 
 * subscription was cleared, otherwise false is returned.
 */  
GlmsBool
capacityState_clearSubscriptionsByKeyIdMidAndServerRef(char *keyId, itc_mbox_id_t mid, int32_t serverRef)
{
   CapacityState *csElement;
   LcciSubscriber *subi, *subPrev, *subNext;
   GlmsBool returnVal = GLMS_FALSE;
   struct timespec currentTime;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_clearSubscriptionsByKeyIdMidAndServerRef",
              keyId);

   csElement = capacityState_findByKeyId(keyId);

   if (csElement == NULL)
      return returnVal;

   subi = csElement->lcciSubList;
   subPrev = NULL;
   while (subi != NULL)
   {
      subNext = capacityState_getNextSub(subi);

      if (capacityState_getSubMid(subi) == mid &&
          (serverRef == 0 || capacityState_getSubServerRef(subi) == serverRef))
      {
         if (subPrev == NULL)
            csElement->lcciSubList = subNext;
         else
            subPrev->nextSub = subNext;

         returnVal = GLMS_TRUE;
         free(subi);

         /* Check if the capacity key have no more subscribers, or active
            licenses, and in that case set the markedForDeletion */
         if(capacityState_getMarkedForDeletion(csElement) == 0 &&
            csElement->lcciSubList == NULL &&
            capacityState_haveAnInstalledLicenseKey(csElement) == GLMS_FALSE)
         {
            if (clock_gettime(CLOCK_REALTIME, &currentTime) == -1)
               tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                          "time returned -1 at deletion of "
                          "CapacityState MO. errno is",
                          errno);
            else
            {
               capacityState_setMarkedForDeletion(csElement, currentTime.tv_sec);
               capacityState_storeParameters(csElement);
            }
         }
         
         capacityState_handleLicenseKeyNotAvailableAlarm(csElement);
         handleLicenseKeyCloseToExpirationAlarm();
      }
      else
         subPrev = subi;

      subi = subNext;
   }

   return returnVal;
}


void
capacityState_clearSubscriptionsByMidAndServerRef(itc_mbox_id_t mid, int32_t serverRef)
{
   CapacityState *csElement;
   LcciSubscriber *subi, *subPrev, *subNext;
   struct timespec currentTime;

   csElement = capacityState_getFirst();

   tracepoint(com_ericsson_glms, call_to_function_w_hex_arg,
              "capacityState_clearSubscriptionsByMidAndServerRef",
              mid);

   while (csElement != NULL)
   {
      subi = csElement->lcciSubList;
      subPrev = NULL;
      while (subi != NULL)
      {
         subNext = capacityState_getNextSub(subi);

         if (capacityState_getSubMid(subi) == mid &&
             (serverRef == 0 || capacityState_getSubServerRef(subi) == serverRef))
         {
            if (subPrev == NULL)
               csElement->lcciSubList = subNext;
            else
               subPrev->nextSub = subNext;

            free(subi);

            /* Check if the capacity key have no more subscribers, or active
               licenses, and in that case set the markedForDeletion */
            if (capacityState_getMarkedForDeletion(csElement) == 0 &&
                csElement->lcciSubList == NULL &&
                capacityState_haveAnInstalledLicenseKey(csElement) == GLMS_FALSE)
            {
               if (clock_gettime(CLOCK_REALTIME, &currentTime) == -1)
               {
                  tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                             "time returned -1 at deletion of "
                             "CapacityState MO. errno is",
                             errno);
               }
               else
               {
                  capacityState_setMarkedForDeletion(csElement, currentTime.tv_sec);
                  capacityState_storeParameters(csElement);
               }
            }
            
            capacityState_handleLicenseKeyNotAvailableAlarm(csElement);
            handleLicenseKeyCloseToExpirationAlarm();
         }
         else
            subPrev = subi;

         subi = subNext;
      }

      csElement = capacityState_getNext(csElement);
   }
}


void
capacityState_clearSubscriberList(CapacityState *csElement)
{
   LcciSubscriber *tmpi, *tmpk;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_clearSubscriberList");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_clearSubscriberList",
              capacityState_getCapacityKeyId(csElement));

   if (csElement->lcciSubList == NULL)
      return;

   tmpi = csElement->lcciSubList;
   csElement->lcciSubList = NULL;

   while(capacityState_getNextSub(tmpi) != NULL)
   {
      tmpk = tmpi;
      tmpi = capacityState_getNextSub(tmpi);
      free(tmpk);
   }

   free(tmpi);
}


void
capacityState_addSubscriber(char *keyId,
                            char *name,
                            char* capacityUnit,
                            char *description,
                            uint16_t isGracePeriodControlled,
                            itc_mbox_id_t clientMid,
                            int32_t serverRef,
                            uint32_t clientRef)
{
   CapacityState *csElement;
   LcciSubscriber *lcciSub, *lcciSub_i;
   char capacityUnit_overflowProtected[GLMS_CAPACITY_UNIT_LEN];

   strncpy(capacityUnit_overflowProtected,
           capacityUnit,
           GLMS_CAPACITY_UNIT_LEN);
   capacityUnit_overflowProtected[GLMS_CAPACITY_UNIT_LEN - 1] = '\0';

   tracepoint(com_ericsson_glms, capacityState_addSubscriber,
              keyId,
              name,
              capacityUnit_overflowProtected,
              description,
              clientMid,
              serverRef,
              clientRef);

   csElement = capacityState_findByKeyId(keyId);

   if (csElement == NULL)
      csElement = capacityState_createCapacityState(description,
                                                    keyId,
                                                    name,
                                                    capacityUnit_overflowProtected,
                                                    "", /* productType */
                                                    0,
                                                    isGracePeriodControlled);
   
   if (capacityState_getFirstSub(csElement) == NULL)
   {
      /* The CapacityState instance could have been created when a key file
         was installed, but if this is the first subscriber we need to
         set the name of the capacity key MO instance. */
      /* This will also update the MO instance name in the case where
         the subscriber has decided to rename the MO by subscribing with
         a new name. */
      capacityState_setName(csElement, name);
      capacityState_setCapacityUnit(csElement, capacityUnit_overflowProtected);
      capacityState_setDescription(csElement, description);
      capacityState_setMarkedForDeletion(csElement, 0);
      capacityState_setIsGpControlled(csElement, isGracePeriodControlled);
      
      if (capacityState_isGracePeriodMoCreated(csElement) == GLMS_FALSE &&
          isGracePeriodControlled &&
          capacityState_isNotContractuallyLimited(csElement) == GLMS_FALSE)
         capacityState_createGpMo(csElement);
   }

   lcciSub = (LcciSubscriber *) malloc(sizeof(LcciSubscriber));
   lcciSub->clientMid = clientMid;
   lcciSub->serverRef = serverRef;
   lcciSub->clientRef = clientRef;
   lcciSub->nextSub   = NULL;

   lcciSub_i = capacityState_getFirstSub(csElement);
   if (lcciSub_i == NULL)
      csElement->lcciSubList = lcciSub;
   else
   {
      while (lcciSub_i->nextSub != NULL)
         lcciSub_i = lcciSub_i->nextSub;

      lcciSub_i->nextSub = lcciSub;
   }

   capacityState_handleLicenseKeyNotAvailableAlarm(csElement);
   capacityState_handleLicenseKeyCloseToExpirationAlarm(csElement);

   capacityState_storeParameters(csElement);
   capacityState_sendMoUpdateCapacityStateInd(csElement);
}


LcciSubscriber
*capacityState_getFirstSub(CapacityState *csElement)
{
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getFirstSub");
      return NULL;
   }

   return csElement->lcciSubList;
}


LcciSubscriber
*capacityState_getNextSub(LcciSubscriber *subscriber)
{
   if(subscriber == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getNextSub");
      return NULL;
   }

   return subscriber->nextSub;
}


void
capacityState_handleLicenseKeyNotAvailableAlarm(CapacityState *csElement)
{
   union itc_msg *sig;
   static uint32_t alarmRsp[] = {1, GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_RSP};

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_handleLicenseKeyNotAvailableAlarm",
              capacityState_getCapacityKeyId(csElement));

   /* Activate alarm */
   if (capacityState_getLicenseState(csElement) == GLMS_LICENSESTATE_DISABLED &&
       capacityState_getFirstSub(csElement) != NULL &&
       keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_CEASED &&
       csElement->licenseKeyNotAvailableAlarmActive == GLMS_FALSE &&
       eu_isEuActive() == GLMS_FALSE &&
       pu_isPuActive() == GLMS_FALSE &&
       iu_isIuActive() == GLMS_FALSE)
   {
      csElement->licenseKeyNotAvailableAlarmActive = GLMS_TRUE;

      /* Send alarm indication to adapter */
      sig = itc_alloc(sizeof(GlmsAdpiLicenseKeyNotAvailableAlarmReq),
                  GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_REQ);
      strcpy(sig->glmsLicenseKeyNotAvailableAlarmReq.moId,
             csElement->capacityStateId);
      sig->glmsLicenseKeyNotAvailableAlarmReq.licenseType = GLMS_CAPACITY_KEY;
      sig->glmsLicenseKeyNotAvailableAlarmReq.alarmState  = GLMS_ALARM_ACTIVATED;

      sig->glmsLicenseKeyNotAvailableAlarmReq.alarmReason  =
         (capacityState_getFirstCapacityKey(csElement) != NULL) ?
         GLMS_ALARM_REASON_KEY_EXPIRED :
         GLMS_ALARM_REASON_KEY_NOT_AVAILABLE;

      sendMsgToAdapter(sig);

      sig = itc_receive(alarmRsp, ITC_NO_TMO, ITC_FROM_ALL);
      /* Alarm correlation for capacity licenses may be implemented at a later point */
      /* csElement->lknaAlarmEventId = sig->glmsLicenseKeyNotAvailableAlarmRsp.lknaAlarmEventId; */
      itc_free(&sig);
   }
   /* Cease alarm */
   else if ((capacityState_getLicenseState(csElement) == GLMS_LICENSESTATE_ENABLED ||
             keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_ACTIVATED ||
             capacityState_getFirstSub(csElement) == NULL ||
             eu_isEuActive()) &&
            csElement->licenseKeyNotAvailableAlarmActive == GLMS_TRUE)
   {
      csElement->licenseKeyNotAvailableAlarmActive = GLMS_FALSE;

      /* Send alarm cease indication to adapter */
      sig = itc_alloc(sizeof(GlmsAdpiLicenseKeyNotAvailableAlarmReq),
                  GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_REQ);
      strcpy(sig->glmsLicenseKeyNotAvailableAlarmReq.moId,
             csElement->capacityStateId);
      sig->glmsLicenseKeyNotAvailableAlarmReq.licenseType = GLMS_CAPACITY_KEY;
      sig->glmsLicenseKeyNotAvailableAlarmReq.alarmState  = GLMS_ALARM_CEASED;
      sig->glmsLicenseKeyNotAvailableAlarmReq.alarmReason = GLMS_ALARM_REASON_CEASED;
      sendMsgToAdapter(sig);

      sig = itc_receive(alarmRsp, ITC_NO_TMO, ITC_FROM_ALL);
      /* Alarm correlation for capacity licenses may be implemented at a later point */
      /* csElement->lknaAlarmEventId = sig->glmsLicenseKeyNotAvailableAlarmRsp.lknaAlarmEventId; */
      itc_free(&sig);
   }
}

void
capacityState_handleLicenseKeyCloseToExpirationAlarm(CapacityState *csElement)
{
   struct timespec currentTime;
   uint32_t expiring = 0;

   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to "
                 "capacityState_handleLicenseKeyCloseToExpirationAlarm");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityState_handleLicenseKeyCloseToExpirationAlarm",
              capacityState_getCapacityKeyId(csElement));

   clock_gettime(CLOCK_REALTIME, &currentTime);
   if(csElement->expiryTime != GLMS_NO_STOP_DATE &&
      ((csElement->expiryTime - GLMS_LICENSEKEY_EXPIRATION_ALARM_TIME) < currentTime.tv_sec))
   {
      expiring = 1;
   }

   /* Activate alarm */
   if (expiring == 1 &&
       capacityState_getLicenseState(csElement) == GLMS_LICENSESTATE_ENABLED &&
       keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_CEASED &&
       capacityState_getFirstSub(csElement) != NULL &&
       (csElement->licenseKeyExpirationAlarmState == ALARM_CEASED ||
        csElement->licenseKeyExpirationAlarmState == ALARM_CEASED_PENDING) &&
       eu_isEuActive() == GLMS_FALSE &&
       pu_isPuActive() == GLMS_FALSE &&
       iu_isIuActive() == GLMS_FALSE)
   {
      csElement->licenseKeyExpirationAlarmState = ALARM_ACTIVE_PENDING;
   }
   /* Cease alarm */
   else if ((csElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE ||
             csElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING) &&
            (capacityState_getLicenseState(csElement) == GLMS_LICENSESTATE_DISABLED ||
             keyFile_getKeyFileFaultAlarmState() == GLMS_ALARM_ACTIVATED ||
             capacityState_getFirstSub(csElement) == NULL ||
             eu_isEuActive() ||
             expiring == 0))
   {
      csElement->licenseKeyExpirationAlarmState = ALARM_CEASED_PENDING;
   }
}

/*
  capacityState_getNrOfExpiringKeys:
  Fills totalActivated with the total number of Capacity Keys that are
  affected by the LicenseKeyCloseToExpiration alarm.

  Function returns 1 if any of the Capacity Keys have a pending alarm state.
*/
uint32_t
capacityState_getNrOfExpiringKeys(uint32_t *totalActivated)
{
   uint32_t pending = 0;
   CapacityState *csElement;

   *totalActivated = 0;

   csElement = capacityState_getFirst();

   while (csElement != NULL)
   {
      if(csElement->licenseKeyExpirationAlarmState == ALARM_CEASED_PENDING ||
         csElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING)
      {
         pending = 1;
      }

      if(csElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE ||
         csElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING)
      {
         (*totalActivated)++;
      }

      csElement = capacityState_getNext(csElement);
   }

   return pending;
}

size_t
capacityState_fillExpirationAlarmAddInfo(char *fillStr)
{
   CapacityState *csElement;
   char *fillStrStart = fillStr;

   fillStr[0] = '\0';

   csElement = capacityState_getFirst();
   while (csElement != NULL)
   {
      if(csElement->licenseKeyExpirationAlarmState == ALARM_CEASED_PENDING)
      {
         csElement->licenseKeyExpirationAlarmState = ALARM_CEASED;
      }

      if(csElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE_PENDING)
      {
         csElement->licenseKeyExpirationAlarmState = ALARM_ACTIVE;
      }

      if(csElement->licenseKeyExpirationAlarmState == ALARM_ACTIVE)
      {
         fillStr = strcat(fillStr, csElement->name);
         fillStr = strcat(fillStr, " (");
         fillStr = strcat(fillStr, csElement->capacityKeyId);
         fillStr = strcat(fillStr, "), ");
      }

      /* The fillStr can become very large so to improve strcat times we
         move the fillStr pointer to the end of the string. */
      fillStr += strlen(fillStr);
      csElement = capacityState_getNext(csElement);
   }

   return fillStr - fillStrStart;
}


itc_mbox_id_t
capacityState_getSubMid(LcciSubscriber *subscriber)
{
   if (subscriber == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getSubPid");
      return 0;
   }

   return subscriber->clientMid;
}


uint32_t
capacityState_getSubClientRef(LcciSubscriber *subscriber)
{
   if (subscriber == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getSubClientRef");
      return 0;
   }

   return subscriber->clientRef;
}

int32_t
capacityState_getSubServerRef(LcciSubscriber *subscriber)
{
   if (subscriber == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getSubServerRef");
      return 0;
   }

   return subscriber->serverRef;
}


int32_t
capacityState_getSubClientServerRef(itc_mbox_id_t mid, LcciSubscriber *subscriber)
{
   LcciClient *lcciClient;
   
   if (subscriber == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_getSubClientServerRef");
      return 0;
   }

   lcciClient = lcci_findByMidAndServerRef(mid, subscriber->serverRef);

   return lcci_getClientServerRef(lcciClient);
}

void
capacityState_sendLcciChangeIndToAllSubscribers(CapacityState *csElement)
{
   LcciSubscriber *subscriber;

   for (subscriber = capacityState_getFirstSub(csElement);
        subscriber != NULL;
        subscriber = capacityState_getNextSub(subscriber))
      capacityState_sendLcciChangeIndToSubscriber(subscriber, csElement);
}

void
capacityState_sendLcciChangeIndToSubscriber(LcciSubscriber *subscriber, CapacityState *cs)
{
   union itc_msg *sig;
   GlmsCapacityValue currentCapacityValue;
   LcciClient *lcciClient;
   
   if (cs == NULL ||
       subscriber == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_sendLcciChangeIndToSubscriber");
      return;
   }

   lcciClient = lcci_findByMidAndServerRef(capacityState_getSubMid(subscriber),
                                           capacityState_getSubServerRef(subscriber));

   if(lcciClient == NULL)
   {
      glms_logEvent(GLMS_LOG_HIGH,
                    "Failed to find LCCI client of MID=0x%x, serverRef=%d",
                    capacityState_getSubMid(subscriber),
                    capacityState_getSubServerRef(subscriber));
      return;
   }

   if (lcci_getPv(lcciClient) == LICENSE_CAPACITY_CONTROL_I_VERSION_1)
   {
      LcciLicenseInfoS *lcciLicInfo;
     
      sig = itc_alloc(sizeof(LcciCapacityLicenseChangeIndS),
               LCCI_CAPACITY_LICENSE_CHANGE_IND);
      sig->lcciCapacityLicenseChangeInd.addressInfo.clientRef =
        wr32(capacityState_getSubClientRef(subscriber));
      sig->lcciCapacityLicenseChangeInd.addressInfo.serverRef =
        wr32(capacityState_getSubClientServerRef(capacityState_getSubMid(subscriber), 
                                                 subscriber));

      lcciLicInfo = &(sig->lcciCapacityLicenseChangeInd.licenseInfo);

      strncpy(lcciLicInfo->licenseKeyId,
              capacityState_getCapacityKeyId(cs),
              MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID);
      lcciLicInfo->licenseKeyId[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';

      lcciLicInfo->licenseState = wr16(capacityState_getLicenseState(cs));
      lcciLicInfo->gracePeriodAvailable = wr16(capacityState_getGracePeriodAvailable(cs));
      lcciLicInfo->gracePeriodActivationThreshold =
        wr32(capacityState_getGracePeriodActivationThreshold(cs));
      currentCapacityValue = capacityState_getCurrentCapacityValue(cs);

      if (capacityState_getGracePeriodState(cs) == GLMS_ACTIVATION_STATE_ACTIVATED||
          capacityState_getGracePeriodState(cs) == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
        lcciLicInfo->capacityLimit.noLimit = wr16(1);
      else
        lcciLicInfo->capacityLimit.noLimit = wr16(currentCapacityValue.noLimit);

      lcciLicInfo->capacityLimit.value = wr32(currentCapacityValue.value);

      glms_logEvent(GLMS_LOG_HIGH,
                    "Sending LCCI change ind to 0x%x; key = %s, "
                    "gpAvailable = %d, gpActivationThreshold = %d, "
                    "licenseState = %d, noLimit = %d, value = %d",
                    capacityState_getSubMid(subscriber),
                    lcciLicInfo->licenseKeyId,
                    rd32(lcciLicInfo->gracePeriodAvailable),
                    rd32(lcciLicInfo->gracePeriodActivationThreshold),
                    rd16(lcciLicInfo->licenseState),
                    rd16(lcciLicInfo->capacityLimit.noLimit),
                    rd32(lcciLicInfo->capacityLimit.value));

      itc_send(&sig, capacityState_getSubMid(subscriber), ITC_MY_MBOX);    
   }
   else
   {
      LcciLicenseInfo2S *lcciLicInfo;

      sig = itc_alloc(sizeof(LcciCapacityLicenseChange2IndS),
               LCCI_CAPACITY_LICENSE_CHANGE2_IND);
      sig->lcciCapacityLicenseChange2Ind.addressInfo.clientRef =
        wr32(capacityState_getSubClientRef(subscriber));
      sig->lcciCapacityLicenseChange2Ind.addressInfo.serverRef =
        wr32(capacityState_getSubClientServerRef(capacityState_getSubMid(subscriber), 
                                                 subscriber));

      lcciLicInfo = &(sig->lcciCapacityLicenseChange2Ind.licenseInfo);

      strncpy(lcciLicInfo->licenseKeyId,
              capacityState_getCapacityKeyId(cs),
              MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID);
      lcciLicInfo->licenseKeyId[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID - 1] = '\0';

      lcciLicInfo->licenseState = wr16(capacityState_getLicenseState(cs));
      lcciLicInfo->gracePeriodAvailable = wr16(capacityState_getGracePeriodAvailable(cs));
      lcciLicInfo->gracePeriodActivationThreshold =
        wr32(capacityState_getGracePeriodActivationThreshold(cs));
      currentCapacityValue = capacityState_getCurrentCapacityValue(cs);

      if (capacityState_getGracePeriodState(cs) == GLMS_ACTIVATION_STATE_ACTIVATED||
          capacityState_getGracePeriodState(cs) == GLMS_ACTIVATION_STATE_ACTIVATED_EXPIRING)
        lcciLicInfo->capacityLimit.noLimit = wr16(1);
      else
        lcciLicInfo->capacityLimit.noLimit = wr16(currentCapacityValue.noLimit);

      lcciLicInfo->capacityLimit.value = wr32(currentCapacityValue.value);

      /* HWAC */
      strcpy(lcciLicInfo->hwacString, capacityState_getHwacString(cs));

      glms_logEvent(GLMS_LOG_HIGH,
                    "Sending LCCI change 2 ind to 0x%x; key = %s, "
                    "gpAvailable = %d, gpActivationThreshold = %d, "
                    "licenseState = %d, noLimit = %d, value = %d, "
                    "hwac = %s",
                    capacityState_getSubMid(subscriber),
                    lcciLicInfo->licenseKeyId,
                    rd32(lcciLicInfo->gracePeriodAvailable),
                    rd32(lcciLicInfo->gracePeriodActivationThreshold),
                    rd16(lcciLicInfo->licenseState),
                    rd16(lcciLicInfo->capacityLimit.noLimit),
                    rd32(lcciLicInfo->capacityLimit.value),
                    lcciLicInfo->hwacString);

      itc_send(&sig, capacityState_getSubMid(subscriber), ITC_MY_MBOX);   
   }
}

void
capacityState_storeParameters(CapacityState *csElement)
{
   char *parameter;
   char  capacityValue_int32tStr[20];
   char  capacityLimit_enumStr[4];
   char  markedForDeletion_timetStr[20];
   char  isGracePeriodControlled_uint16tStr[20];
   char  isGracePeriodMoCreated_uint32tStr[20];
   char  gracePeriodActivationValue_uint32tStr[20];
   char  gracePeriodExpiration_timetStr[20];
   char  gracePeriodDuration_uint32tStr[20];
   char  gpConfiguredResetThreshold_int32tStr[20];
   char  gpConfiguredActivationThreshold_int32tStr[20];
   uint32_t paramSize;
   
   if (csElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Null pointer argument passed to capacityState_storeParameters");
      return;
   }

   /* Store software parameters:
      csElement->capacityStateId
      csElement->name
      csElement->capacityUnit
      csElement->productType
      csElement->description
      csElement->keyId
      csElement->markedForDeletion
      csElement->isGracePeriodControlled
      csElement->graceperiod.gpConfiguredLength
      csElement->gracePeriod.gpConfiguredResetThreshold
      csElement->gracePeriod.gpConfiguredActivationThreshold
      csElement->currentCapacityValue.value
      csElement->currentCapacityValue.noLimit
      csElement->isGracePeriodMoCreated
   */

   if (csElement->pendingUpdates[GLMS_SP] == GLMS_TRUE)
   {
      paramSize = 0;
      paramSize += strlen(csElement->capacityStateId);
      paramSize += strInStr(";",csElement->capacityStateId);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(csElement->name);
      paramSize += strInStr(";",csElement->name);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(csElement->capacityUnit);
      paramSize += strInStr(";",csElement->capacityUnit);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(csElement->productType);
      paramSize += strInStr(";",csElement->productType);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(csElement->description);
      paramSize += strInStr(";",csElement->description);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(csElement->capacityKeyId);
      paramSize += strInStr(";",csElement->capacityKeyId);
      paramSize += 2; /* Delimiter */
      sprintf(markedForDeletion_timetStr, "%jd", (intmax_t)csElement->markedForDeletion);
      paramSize += strlen(markedForDeletion_timetStr);
      paramSize += 2; /* Delimiter */
      sprintf(isGracePeriodControlled_uint16tStr, "%"PRIu16, (uint16_t)csElement->isGracePeriodControlled);
      paramSize += strlen(isGracePeriodControlled_uint16tStr);
      paramSize += 2; /* Delimiter */
      sprintf(gracePeriodDuration_uint32tStr, "%"PRIu32,
              csElement->gracePeriod.gpConfiguredLength);
      paramSize += strlen(gracePeriodDuration_uint32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(gpConfiguredResetThreshold_int32tStr, "%"PRId32,
              csElement->gracePeriod.gpConfiguredResetThreshold);
      paramSize += strlen(gpConfiguredResetThreshold_int32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(gpConfiguredActivationThreshold_int32tStr, "%"PRId32,
              csElement->gracePeriod.gpConfiguredActivationThreshold);
      paramSize += strlen(gpConfiguredActivationThreshold_int32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(capacityValue_int32tStr, "%"PRId32, csElement->currentCapacityValue.value);
      paramSize += strlen(capacityValue_int32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(capacityLimit_enumStr, "%"PRIu32, (uint32_t)csElement->currentCapacityValue.noLimit);
      paramSize += strlen(capacityLimit_enumStr);
      paramSize += 2; /* Delimiter */
      sprintf(isGracePeriodMoCreated_uint32tStr, "%"PRIu32, (uint32_t)csElement->isGracePeriodMoCreated);
      paramSize += strlen(isGracePeriodMoCreated_uint32tStr);
      paramSize += 2; /* NULL */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      fillParameter(parameter, csElement->capacityStateId, GLMS_FALSE);
      fillParameter(parameter, csElement->name, GLMS_FALSE);
      fillParameter(parameter, csElement->capacityUnit, GLMS_FALSE);
      fillParameter(parameter, csElement->productType, GLMS_FALSE);
      fillParameter(parameter, csElement->description, GLMS_FALSE);
      fillParameter(parameter, csElement->capacityKeyId, GLMS_FALSE);
      fillParameter(parameter, markedForDeletion_timetStr, GLMS_FALSE);
      fillParameter(parameter, isGracePeriodControlled_uint16tStr, GLMS_FALSE);
      fillParameter(parameter, gracePeriodDuration_uint32tStr, GLMS_FALSE);
      fillParameter(parameter, gpConfiguredResetThreshold_int32tStr, GLMS_FALSE);
      fillParameter(parameter, gpConfiguredActivationThreshold_int32tStr, GLMS_FALSE);
      fillParameter(parameter, capacityValue_int32tStr, GLMS_FALSE);
      fillParameter(parameter, capacityLimit_enumStr, GLMS_FALSE);
      fillParameter(parameter, isGracePeriodMoCreated_uint32tStr, GLMS_TRUE);

      sp_set(GLMS_TABLE_RID_CAPACITYSTATE,
             csElement->tableIndex,
             parameter);

      csElement->pendingUpdates[GLMS_SP] = GLMS_FALSE;
      free(parameter);
   }

   /* Store persistent parameters:
      csElement->capacityStateId
      csElement->gracePeriodActivationValue
      csElement->gracePeriod.gracePeriodExpiration
   */
   if (csElement->pendingUpdates[GLMS_PP] == GLMS_TRUE)
   {
      paramSize = 0;
      paramSize += strlen(csElement->capacityStateId);
      paramSize += strInStr(";",csElement->capacityStateId);
      paramSize += 2; /* Delimiter */
      sprintf(gracePeriodActivationValue_uint32tStr, "%"PRIu32, (uint32_t)csElement->gracePeriodActivationValue);
      paramSize += strlen(gracePeriodActivationValue_uint32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(gracePeriodExpiration_timetStr, "%jd",
             (intmax_t)csElement->gracePeriod.gracePeriodExpiration);
      paramSize += strlen(gracePeriodExpiration_timetStr);
      paramSize += 2; /* NULL */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      fillParameter(parameter, csElement->capacityStateId, GLMS_FALSE);
      fillParameter(parameter, gracePeriodActivationValue_uint32tStr, GLMS_FALSE);
      fillParameter(parameter, gracePeriodExpiration_timetStr, GLMS_TRUE);

      pp_set(GLMS_TABLE_RID_CAPACITYSTATE,
             csElement->tableIndex,
             parameter);

      csElement->pendingUpdates[GLMS_PP] = GLMS_FALSE;
      free(parameter);
   }
}

void
capacityState_storeParametersOfAllCapacityStatesAndKeys()
{
   CapacityState *csElement;
   CapacityKey *ckElement;

   for (csElement = capacityState_getFirst();
        csElement != NULL;
        csElement = capacityState_getNext(csElement))
   {
      capacityState_storeParameters(csElement);
      capacityState_sendMoUpdateCapacityStateInd(csElement);
      capacityState_sendMoUpdateGracePeriodInd(csElement);

      for (ckElement = capacityState_getFirstCapacityKey(csElement);
           ckElement != NULL;
           ckElement = capacityKey_getNext(ckElement))
         capacityKey_storeParameters(ckElement);
   }
}


void
capacityState_fetchParameters()
{
   /* Software Parameters shall be requested first and then
      Persistent Parameters. This is because Persistent Parameters
      needs to be handled before the Software Parameters for each
      Capacity Key. */
   sp_requestIndexList(GLMS_TABLE_RID_CAPACITYSTATE);
   pp_requestIndexList(GLMS_TABLE_RID_CAPACITYSTATE);
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
   return common_handleGlmsAdpiXXIndexListRsp(sig, csData.tableIndex, csData.indexesInTable);
}


void
capacityState_parsePp(union itc_msg *sig)
{
   char *capacityStateId;
   CapacityState *cs;
   uint32_t paramCount;
   uint32_t gracePeriodActivationValue;
   time_t32 gracePeriodExpiration;
   char *paramStr, *parameter;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);
   if (paramCount < 2)
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "The CapacityState persistent parameter string contains "
                 "less than the expected 2 parameter delimiters",
                 paramStr);
   else
   {
      /* Read parameters:
         csElement->capacityStateId
         csElement->gracePeriodActivationValue
         csElement->gracePeriod.gracePeriodExpiration
      */

      capacityStateId = getNextParameter(";:", paramStr);
      cleanupEscapeCharacters(capacityStateId);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNu32, &gracePeriodActivationValue);

      parameter = getNextParameter(";:", NULL);
      gracePeriodExpiration = (time_t32)strtoll(parameter, NULL, 10);


      cs = capacityState_findByCapacityStateId(capacityStateId);
      if (cs != NULL)
      {
         capacityState_setGracePeriodActivationValue(cs, gracePeriodActivationValue);
         capacityState_setGracePeriodExpirationAndState(cs, gracePeriodExpiration);
         cs->pendingUpdates[GLMS_PP] = GLMS_FALSE;
      }
   }
}

void
capacityState_parseSp(union itc_msg *sig)
{
   char *keyId, *name, *productType, *description, *capacityStateId, *capacityUnit;
   uint32_t paramCount, gracePeriodDuration, capacityLimit;
   uint32_t isGracePeriodMoCreated;
   int32_t gpConfiguredResetThreshold, gpConfiguredActivationThreshold;
   uint16_t isGracePeriodControlled;
   int32_t capacityValue;
   time_t32 markedForDeletion;
   GlmsGracePeriod    gracePeriod;
   GlmsCapacityValue  currentCapacityValue;
   char *paramStr, *parameter;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);

   if (paramCount < 13)
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "The CapacityState software parameter string contains "
                 "less than the expected 13 parameter delimiters",
                 paramStr);
   else
   {
      /* Read parameters:
         csElement->capacityStateId
         csElement->name
         csElement->capacityUnit
         csElement->productType
         csElement->description
         csElement->keyId
         csElement->markedForDeletion
         csElement->isGracePeriodControlled
         csElement->graceperiod.gpConfiguredLength
         csElement->gracePeriod.gpConfiguredResetThreshold
         csElement->gracePeriod.gpConfiguredActivationThreshold
         csElement->currentCapacityValue.value
         csElement->currentCapacityValue.noLimit
         csElement->isGracePeriodMoCreated
      */

      capacityStateId = getNextParameter(";:", paramStr);
      cleanupEscapeCharacters(capacityStateId);

      name = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(name);

      capacityUnit = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(capacityUnit);

      productType = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(productType);

      description = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(description);

      keyId = getNextParameter(";:", NULL);
      cleanupEscapeCharacters(keyId);

      parameter = getNextParameter(";:", NULL);
      markedForDeletion = (time_t32)strtoll(parameter, NULL, 10);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNu16, &isGracePeriodControlled);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNu32, &gracePeriodDuration);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNd32, &gpConfiguredResetThreshold);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNd32, &gpConfiguredActivationThreshold);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNd32, &capacityValue);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNu32, &capacityLimit);

      parameter = getNextParameter(";:", NULL);
      sscanf(parameter, "%"SCNu32, &isGracePeriodMoCreated);

      strncpy(gracePeriod.gracePeriodId, capacityStateId,
              GLMS_MO_KEY_LEN);
      gracePeriod.gracePeriodId[GLMS_MO_KEY_LEN - 1] = '\0';
      gracePeriod.gpConfiguredLength              = gracePeriodDuration;
      gracePeriod.gracePeriodState                = GLMS_ACTIVATION_STATE_INACTIVE;
      gracePeriod.gracePeriodExpiration           = 0;
      gracePeriod.gpConfiguredResetThreshold      = gpConfiguredResetThreshold;
      gracePeriod.gpConfiguredActivationThreshold = gpConfiguredActivationThreshold;

      currentCapacityValue.noLimit = capacityLimit;
      currentCapacityValue.value   = capacityValue;

      /* Create a CapacityState with the read parameters */
      capacityState_createCapacityStateWithoutMoIndication(capacityStateId,
                                                                description,
                                                                keyId,
                                                                name,
                                                                capacityUnit,
                                                                productType,
                                                                GLMS_LICENSESTATE_DISABLED,
                                                                markedForDeletion,
                                                                sig->glmsPpGetRsp.index,
                                                                GLMS_FALSE, /* No license validation */
                                                                currentCapacityValue,
                                                                isGracePeriodControlled,
                                                                0, /* gracePeriodActivationValue */
                                                                gracePeriod,
                                                                isGracePeriodMoCreated);
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
   int table = common_prepareForParsingStoredParameters(sig, GLMS_TRUE, GLMS_TRUE);

   if (table == -1)
      return -1;

   if (table == GLMS_SP)
      capacityState_parseSp(sig);
   else if (table == GLMS_PP)
      capacityState_parsePp(sig);

   /* Store the index that we have read */
   csData.tableIndex[table][csData.readIndexesFromTable[table]] = sig->glmsPpGetRsp.index;
   csData.readIndexesFromTable[table]++;

   if (csData.readIndexesFromTable[table] != csData.indexesInTable[table])
      return 3; /* We expect more parameters */

   common_findHighestIndexInTable(csData.tableIndex[table], csData.readIndexesFromTable[table], &csData.tableIndexPool);

   for (CapacityState *cs = capacityState_getFirst();
        cs != NULL;
        cs = capacityState_getNext(cs))
   {
      for (uint32_t i = 0; i < csData.readIndexesFromTable[table]; ++i)
      {
         if (csData.tableIndex[table][i] == capacityState_getTableIndex(cs))
         {
            csData.tableIndex[table][i] = 0;
         }
      }
   }

   common_sanityCheckIndexesInTable(csData.tableIndex[table], csData.readIndexesFromTable[table]);
   common_resetIndexesInTable(&csData.indexesInTable[table], &csData.readIndexesFromTable[table], &csData.tableIndex[table]);

   return 1; /* We have read all parameters so return 1. */
}


int32_t
capacityState_parseStoredParameters(union itc_msg *sig)
{
   static uint32_t capacityKeyDataFetched;
   capacityKeyDataFetched = (sig->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP) ? 0 : capacityKeyDataFetched;
   int32_t ret = common_parseStoredParameters(sig, csData.readStatus, handleGlmsAdpiXXGetRsp, handleGlmsAdpiXXIndexListRsp, GLMS_TRUE, GLMS_TRUE);

   if ((ret == 1) && (capacityKeyDataFetched == 0))
   {
      capacityKeyDataFetched = 1;
      capacityKey_fetchParameters();
   }

   return ret;
}

void
capacityState_sendMoUpdateCapacityStateInd(CapacityState *csElement)
{
   union itc_msg *sigSend;

   if (adapter_isSubscribedForMoUpdateInd() &&
       csElement->sendMoUpdateInd &&
       adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateCapacityStateInd),
                      GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_IND);
      strcpy(sigSend->glmsAdpiMoUpdateCapacityStateInd.capacityStateId,
             capacityState_getCapacityStateId(csElement));
      strcpy(sigSend->glmsAdpiMoUpdateCapacityStateInd.description,
             capacityState_getDescription(csElement));
      strcpy(sigSend->glmsAdpiMoUpdateCapacityStateInd.keyId,
             capacityState_getCapacityKeyId(csElement));
      strcpy(sigSend->glmsAdpiMoUpdateCapacityStateInd.activeCapacityKeyId,
             capacityState_getActiveCapacityKeyId(csElement));
      sigSend->glmsAdpiMoUpdateCapacityStateInd.licenseState =
         capacityState_getLicenseState(csElement);
      sigSend->glmsAdpiMoUpdateCapacityStateInd.grantedCapacityLevel =
         capacityState_getGrantedCapacityLevel(csElement);
      sigSend->glmsAdpiMoUpdateCapacityStateInd.licensedCapacityLimitReached =
             capacityState_getLicensedCapacityLimitReached(csElement);
      sigSend->glmsAdpiMoUpdateCapacityStateInd.currentCapacityValue =
         capacityState_getCurrentCapacityValue(csElement);

      sendMsgToAdapter(sigSend);
   }

   csElement->sendMoUpdateInd = GLMS_FALSE;
}

void
capacityState_sendMoUpdateGracePeriodInd(CapacityState *csElement)
{
   union itc_msg *sigSend;

   if (adapter_isSubscribedForMoUpdateInd() &&
       csElement->sendGpMoUpdateInd &&
       adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateGracePeriodInd),
                      GLMS_ADPI_MO_UPDATE_GRACE_PERIOD_IND);
      capacityState_copyGracePeriod(csElement, &sigSend->glmsAdpiMoUpdateGracePeriodInd.gracePeriod);
      sendMsgToAdapter(sigSend);
   }

   csElement->sendGpMoUpdateInd = GLMS_FALSE;
}

uint32_t
writeCapacityStateData(CapacityState *cstate, char *storage, uint32_t offset)
{
   char markedForDeletion_s[30], gracePeriodExpiration_s[30];
   GlmsCapacityValue currentCapacityValue = capacityState_getCurrentCapacityValue(cstate);
   time_t markedForDeletion = (time_t) capacityState_getMarkedForDeletion(cstate);
   strncpy(markedForDeletion_s, ctime(&markedForDeletion), 30);
   markedForDeletion_s[29] = '\0';

   time_t gracePeriodExpiration = (time_t) capacityState_getGracePeriodExpiration(cstate);
   strncpy(gracePeriodExpiration_s, ctime(&gracePeriodExpiration), 30);
   gracePeriodExpiration_s[29] = '\0';

   uint32_t ret;
   ret = snprintf(storage + offset,
                  GLMS_MAX_DUMPSTR_LEN - offset,
                  "\nCapacityStateId\t\t: %s\n"
                  "CapacityKeyId\t\t: %s\n"
                  "Name\t\t\t: %s\n"
                  "CapacityUnit\t\t: %s\n"
                  "ProductType\t\t: %s\n"
                  "Description\t\t: %s\n"
                  "LicenseState\t\t: %d\n"
                  "LicenseStateAccordingToKf : %d\n"
                  "CurrentCapacityLimit\t: %d\n"
                  "CurrentCapacityValue\t: %d\n"
                  "IsGracePeriodControlled\t: %d\n"
                  "GracePeriodId\t\t: %s\n"
                  "GracePeriodExpiration\t: %s"
                  "GpConfiguredLength\t: %d\n"
                  "GpResetThreshold\t: %d\n"
                  "GpActivationThreshold\t: %d\n"
                  "GracePeriodState\t: %d\n"
                  "MarkedForDeletion\t: %s"
                  "PendingUpdates\t\t: %d %d\n"
                  "LkNotAvailableAlarm\t: %d\n"
                  "SendMoUpdateInd\t\t: %d\n"
                  "SendGpMoUpdateInd\t\t: %d\n"
                  "TableIndex\t\t: %d\n",
                   capacityState_getCapacityStateId(cstate),
                   capacityState_getCapacityKeyId(cstate),
                   capacityState_getName(cstate),
                   capacityState_getCapacityUnit(cstate),
                   capacityState_getProductType(cstate),
                   capacityState_getDescription(cstate),
                   capacityState_getLicenseState(cstate),
                   capacityState_getLicenseStateAccordingToKf(cstate),
                   currentCapacityValue.noLimit,
                   currentCapacityValue.value,
                   capacityState_getIsGpControlled(cstate),
                   capacityState_getGracePeriodId(cstate),
                   gracePeriodExpiration_s,
                   capacityState_getGpConfiguredLength(cstate),
                   capacityState_getGracePeriodResetThreshold(cstate),
                   capacityState_getGracePeriodActivationThreshold(cstate),
                   capacityState_getGracePeriodState(cstate),
                   markedForDeletion_s,
                   cstate->pendingUpdates[0],
                   cstate->pendingUpdates[1],
                   cstate->licenseKeyNotAvailableAlarmActive,
                   cstate->sendMoUpdateInd,
                   cstate->sendGpMoUpdateInd,
                   cstate->tableIndex);

   if (ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpCapacityStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   return ret;
}

uint32_t
writeCapacityKeyInstance(CapacityKey *ckey, char *storage, uint32_t offset, uint32_t s)
{
   uint32_t ret = 0;
   GlmsCapacityValue licensedCapacityValue;
   time_t validFrom, expiration;
   char validFrom_s[30], expiration_s[30];

   licensedCapacityValue = capacityKey_getLicensedCapacityValue(ckey);
   validFrom = (time_t) capacityKey_getValidFrom(ckey);
   expiration = (time_t) capacityKey_getExpiration(ckey);

   strncpy(validFrom_s, ctime(&validFrom), 30);
   validFrom_s[29] = '\0';
   strncpy(expiration_s, ctime(&expiration), 30);
   expiration_s[29] = '\0';

   ret += snprintf(storage + offset,
                   GLMS_MAX_DUMPSTR_LEN - offset,
                   "  %d: capacityKeyId\t= %s\n"
                   "  %d: validFrom\t\t= %s"
                   "  %d: expiration\t\t= %s"
                   "  %d: licensedCapacityLimit = %d\n"
                   "  %d: licensedCapacityValue = %d\n"
                   "  %d: markedAtKfParsing\t= %d\n"
                   "  %d: tableIndex\t\t= %d\n",
                   s, capacityKey_getCapacityKeyId(ckey),
                   s, validFrom_s,
                   s, expiration_s,
                   s, licensedCapacityValue.noLimit,
                   s, licensedCapacityValue.value,
                   s, capacityKey_isMarkedAtKfParsing(ckey),
                   s, capacityKey_getTableIndex(ckey));

   if (ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpCapacityStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   return ret;
}

uint32_t
writeCapacityKeyList(CapacityState *cstate, char *storage, uint32_t offset)
{
   uint32_t ret = 0, s;
   CapacityKey *ckey = NULL;

   ret = snprintf(storage + offset,
                  GLMS_MAX_DUMPSTR_LEN - offset,
                  "CapacityKeys:\n");

   if (ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpCapacityStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   s = 1;
   ckey = capacityState_getFirstCapacityKey(cstate);
   while (ckey != NULL)
   {
      ret += writeCapacityKeyInstance(ckey, storage, ret + offset, s);
      ++s;
      ckey = capacityKey_getNext(ckey);
   }

   return ret;
}

uint32_t
writeCapacityStateSubscriberList(CapacityState *cstate, char *storage, uint32_t offset)
{
   uint32_t ret;

   /* Read subscriber list of CapacityState */
   ret = snprintf(storage + offset,
                  GLMS_MAX_DUMPSTR_LEN,
                  "Subscribers:\n");

   if (ret >= GLMS_MAX_DUMPSTR_LEN - offset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpCapacityStateDataReq");
      ret = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
   }

   uint32_t tmp;
   LcciSubscriber *lcciSub = capacityState_getFirstSub(cstate);
   while (lcciSub != NULL)
   {
      tmp = snprintf(storage + offset + ret,
                      GLMS_MAX_DUMPSTR_LEN - offset - ret,
                      "  MailboxId\t: 0x%08x\n"
                      "    ServerRef\t: %d\n"
                      "    ClientRef\t: %d\n",
                      capacityState_getSubMid(lcciSub),
                      capacityState_getSubServerRef(lcciSub),
                      capacityState_getSubClientRef(lcciSub));

      if (tmp >= GLMS_MAX_DUMPSTR_LEN - offset)
      {
         tracepoint(com_ericsson_glms, error_trace,
                    "Dump string was truncated in call"
                    " to handleGlmsAdpiDumpCapacityStateDataReq");
         tmp = GLMS_MAX_DUMPSTR_LEN - 1 - offset;
      }

      ret += tmp;
      lcciSub = capacityState_getNextSub(lcciSub);
   }

   return ret;
}

void
sendGlmsDumpCapacityStateDataRsp(char *dump,
                                 uint32_t dumpSize,
                                 uint32_t currentBufferUsed,
                                 uint32_t lastResponse,
                                 union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiDumpCapacityStateDataRsp) + dumpSize,
                                       GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_RSP);
   sigSend->glmsDumpCapacityStateDataRsp.result       = GLMS_OK;
   strncpy(sigSend->glmsDumpCapacityStateDataRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sigSend->glmsDumpCapacityStateDataRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsDumpCapacityStateDataRsp.lastResponse = lastResponse;
   sigSend->glmsDumpCapacityStateDataRsp.sizeOfDump = currentBufferUsed + 1;
   strncpy(sigSend->glmsDumpCapacityStateDataRsp.dump, dump, dumpSize);
   sigSend->glmsDumpCapacityStateDataRsp.dump[dumpSize] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

void
handleGlmsAdpiDumpCapacityStateDataReq(union itc_msg *sigRec)
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
                         csData.readIndexesFromTable[0],
                         csData.readIndexesFromTable[1],
                         csData.indexesInTable[0],
                         csData.indexesInTable[1],
                         csData.readStatus[0],
                         csData.readStatus[1],
                         csData.tableIndexPool);

   if (dumpStrLen >= GLMS_MAX_DUMPSTR_LEN - dumpStrOffset)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Dump string was truncated in call"
                 " to handleGlmsAdpiDumpCapacityStateDataReq");
      dumpStrLen = GLMS_MAX_DUMPSTR_LEN - 1 - dumpStrOffset;
   }

   /* Copy dumpStr to the final dump buffer */
   strcpy(buf, dumpStr);

   currentBufferUsed += dumpStrLen;

   CapacityState *cstate = capacityState_getFirst();
   while (cstate != NULL)
   {
      dumpStrOffset = 0;
      dumpStrOffset += writeCapacityStateData(cstate, dumpStr, dumpStrOffset);
      dumpStrOffset += writeCapacityKeyList(cstate, dumpStr, dumpStrOffset);
      dumpStrOffset += writeCapacityStateSubscriberList(cstate, dumpStr, dumpStrOffset);

      /* If the total dump size will be greater than the maximum allowed
         dump size, then send the already existing dump text and start
         a new dump string. */
      if((currentBufferUsed + dumpStrOffset) >
         GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT])
      {
         sendGlmsDumpCapacityStateDataRsp(buf, GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT],
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
      cstate = capacityState_getNext(cstate);
   }

   /* 'buf' now contains the last part of the dump. Send it to the adapter
      and free the allocated memory. */
   sendGlmsDumpCapacityStateDataRsp(buf, currentBufferUsed,
                                    currentBufferUsed, 1, sigRec);

   free(buf);
}

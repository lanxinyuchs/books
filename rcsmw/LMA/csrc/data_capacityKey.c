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
#include "data_capacityKey.h"
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
   uint32_t  sigNo;

   GlmsAdpiCreateCapacityKeyMoReq            glmsCreateCapacityKeyMoReq;
   GlmsAdpiDeleteCapacityKeyMoReq            glmsDeleteCapacityKeyMoReq;
   GlmsAdpiPersistentParameterGetReq         glmsPpGetReq;
   GlmsAdpiPersistentParameterGetRsp         glmsPpGetRsp;
   GlmsAdpiPersistentParameterIndexListReq   glmsPpIndexListReq;
   GlmsAdpiPersistentParameterIndexListRsp   glmsPpIndexListRsp;
   GlmsAdpiSoftwareParameterGetReq           glmsSpGetReq;
   GlmsAdpiSoftwareParameterGetRsp           glmsSpGetRsp;
   GlmsAdpiSoftwareParameterIndexListReq     glmsSpIndexListReq;
   GlmsAdpiSoftwareParameterIndexListRsp     glmsSpIndexListRsp;
   GlmsAdpiMoUpdateCapacityKeyNameInd        glmsMoUpdateCapacityKeyNameInd;
   GlmsAdpiMoUpdateCapacityKeyCapacityUnitInd glmsMoUpdateCapacityKeyCapacityUnitInd;

   /* LIHI - LCCI */
   #include "lihi/licenseCapacityControlIUnionContent.h"
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

CapacityKeyData ckData;


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */


/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
capacityKeyDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "capacityKeyDataInit");

   ckData.tableIndexPool                 = 1;
   ckData.indexesInTable[GLMS_PP]        = 0;
   ckData.indexesInTable[GLMS_SP]        = 0;
   ckData.readIndexesFromTable[GLMS_PP]  = 0;
   ckData.readIndexesFromTable[GLMS_SP]  = 0;
   ckData.readStatus[GLMS_PP]            = 0;
   ckData.readStatus[GLMS_SP]            = 0;
   ckData.tableIndex[GLMS_PP]            = NULL;
   ckData.tableIndex[GLMS_SP]            = NULL;
}

void
capacityKeyDataClear()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "capacityKeyDataClear");

   if (ckData.tableIndex[GLMS_PP] != NULL)
      free(ckData.tableIndex[GLMS_PP]);

   if (ckData.tableIndex[GLMS_SP] != NULL)
      free(ckData.tableIndex[GLMS_SP]);

   capacityKeyDataInit();
}

GlmsBool
isCapacityKeyLast(CapacityKey *ckElement)
{
   return (ckElement->nextCapacityKey == NULL) ? GLMS_TRUE : GLMS_FALSE;
}

CapacityKey
*capacityKey_getNext(CapacityKey *ckElement)
{
   return (ckElement == NULL) ? NULL : ckElement->nextCapacityKey;
}

CapacityKey
*capacityKey_getPrev(CapacityKey *ckElement)
{
   return (ckElement == NULL) ? NULL : ckElement->prevCapacityKey;
}

CapacityKey
*capacityKey_findByCapacityKeyId(char *capacityKeyId)
{
   CapacityKey *ck;
   CapacityState *cs;

   for(cs = capacityState_getFirst();
       cs != NULL;
       cs = capacityState_getNext(cs))
   {
      for(ck = capacityState_getFirstCapacityKey(cs);
          ck != NULL;
          ck = capacityKey_getNext(ck))
      {
         if (compareMoKeys(capacityKeyId, capacityKey_getCapacityKeyId(ck)))
            return ck;
      }
   }

   return NULL;
}


CapacityState*
capacityKey_getParentCapacityState(CapacityKey *ckElement)
{
   if (ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read parentCapacityState from null pointer");
      return NULL;
   }

   return ckElement->parentCapacityState;
}


static CapacityKey*
capacityKey_createKeyWithoutMoIndication(char *keyId,
                                         char *capacityKeyId,
                                         time_t32 validFrom,
                                         time_t32 expiration,
                                         uint32_t tableIndex,
                                         GlmsCapacityValue licensedCapacityValue,
                                         uint32_t notContractuallyLimited,
                                         char *hwacString)
{
   CapacityKey *ckey;
   CapacityState *cstate;

   tracepoint(com_ericsson_glms, capacityKey_createKeyWithoutMoIndication,
              keyId,
              capacityKeyId,
              validFrom,
              expiration,
              tableIndex,
              licensedCapacityValue.noLimit,
              licensedCapacityValue.value,
              notContractuallyLimited);

   cstate = capacityState_findByKeyId(keyId);

   if(cstate == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Attempted to create CapacityKey for keyId for which"
                 " an CapacityState did not exist",
                 keyId);
      return NULL;
   }

   ckey = (CapacityKey *) malloc(sizeof(CapacityKey));

   ckey->validFrom          = validFrom;
   ckey->expiration         = expiration;
   ckey->markedAtKfParsing  = GLMS_FALSE;
   ckey->tableIndex         = tableIndex;
   ckey->licensedCapacityValue   = licensedCapacityValue;
   ckey->notContractuallyLimited = notContractuallyLimited;

   strncpy(ckey->hwacString, hwacString,
           MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING);
   ckey->hwacString[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING - 1] = '\0';
   
   /* Always create with default settings */
   ckey->pendingUpdates[GLMS_PP] = GLMS_FALSE;
   ckey->pendingUpdates[GLMS_SP] = GLMS_FALSE;

   strncpy(ckey->capacityKeyId, capacityKeyId,
           GLMS_MO_KEY_LEN);
   ckey->capacityKeyId[GLMS_MO_KEY_LEN - 1] = '\0';


   /* ckey->nextCapacityKey and ckey->prevCapacityKey are updated
      in call to capacityState_addCapacityKey */
   capacityState_addCapacityKey(cstate, ckey);
   ckey->parentCapacityState = cstate;

   return ckey;
}

static int32_t
capacityKey_getMoIdNumber(CapacityKey *ck)
{
   char *ckMoId = capacityKey_getCapacityKeyId(ck);
   char *moIdNumber;
   int32_t moIdNumber_i;

   if(ckMoId == NULL)
      return 1;

   moIdNumber = strrchr(ckMoId, '_');

   if(moIdNumber == NULL)
      return 1;

   /* moIdNumber points to the last '-' in the MO Id. 
      moIdNumber+1 is the MO Id number. */
   sscanf((moIdNumber+1), "%"SCNi32, &moIdNumber_i);

   return moIdNumber_i;
}


static int32_t
capacityKey_getNextMoIdNumber(char *keyId)
{
   CapacityState *cs;
   CapacityKey   *ck;
   int32_t       moIdNumber, highestMoIdNumber;

   cs = capacityState_findByKeyId(keyId);
   
   if(cs == NULL)
      return 1;

   ck = capacityState_getFirstCapacityKey(cs);

   if(ck == NULL)
      return 1;

   highestMoIdNumber = 1;
   while(ck != NULL)
   {
      moIdNumber = capacityKey_getMoIdNumber(ck);
      if(moIdNumber > highestMoIdNumber)
         highestMoIdNumber = moIdNumber;

      ck = capacityKey_getNext(ck);
   }

   return (highestMoIdNumber + 1);
}


CapacityKey
*capacityKey_createKeyWithMoIndication(char *keyId,                                       
                                       time_t32 validFrom,
                                       time_t32 expiration,
                                       GlmsCapacityValue licensedCapacityValue,
                                       uint32_t notContractuallyLimited,
                                       char *hwacString)
{
   union itc_msg *sig;
   CapacityKey *ckey;
   char capacityKeyId[GLMS_MO_KEY_LEN];
   int32_t moIdNumber;

   moIdNumber = capacityKey_getNextMoIdNumber(keyId);

   snprintf(capacityKeyId, GLMS_MO_KEY_LEN, "%s_%d",
            keyId,
            moIdNumber);
	    
   capacityKeyId[GLMS_MO_KEY_LEN - 1] = '\0';

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityKey_createKeyWithMoIndication",
              capacityKeyId);

   ckey = capacityKey_createKeyWithoutMoIndication(keyId,
                                                   capacityKeyId,
                                                   validFrom,
                                                   expiration,
                                                   ckData.tableIndexPool++,
                                                   licensedCapacityValue,
                                                   notContractuallyLimited,
                                                   hwacString);

   if(ckey != NULL)
   {
      glms_logEvent(GLMS_LOG_MEDIUM,
                    "Creating CapacityKey MO instance for %s",
                    capacityState_getCapacityKeyId(ckey->parentCapacityState));
      
      /* Send Create CapacityKey MO to adapter */
      sig = itc_alloc(sizeof(GlmsAdpiCreateCapacityKeyMoReq),
                  GLMS_ADPI_CREATE_CAPACITY_KEY_MO_REQ);
      sig->glmsCreateCapacityKeyMoReq.validFrom = capacityKey_getValidFrom(ckey);
      sig->glmsCreateCapacityKeyMoReq.expiration = capacityKey_getExpiration(ckey);
      sig->glmsCreateCapacityKeyMoReq.licensedCapacityValue = 
                                      capacityKey_getLicensedCapacityValue(ckey);
      sig->glmsCreateCapacityKeyMoReq.licensedCapacityLimitReached =
                                      capacityKey_getLicensedCapacityLimitReached(ckey);
      sig->glmsCreateCapacityKeyMoReq.grantedCapacityLevel =
                                      capacityKey_getGrantedCapacityLevel(ckey);
      strcpy(sig->glmsCreateCapacityKeyMoReq.capacityUnit, capacityKey_getCapacityUnit(ckey));
      strcpy(sig->glmsCreateCapacityKeyMoReq.capacityKeyId, capacityKey_getCapacityKeyId(ckey));
      strcpy(sig->glmsCreateCapacityKeyMoReq.keyId, capacityKey_getKeyId(ckey));
      strcpy(sig->glmsCreateCapacityKeyMoReq.name, capacityKey_getName(ckey));
      strcpy(sig->glmsCreateCapacityKeyMoReq.productType, capacityKey_getProductType(ckey));
      sendMsgToAdapter(sig);

      ckey->pendingUpdates[GLMS_PP] = GLMS_TRUE;
      ckey->pendingUpdates[GLMS_SP] = GLMS_TRUE;

      capacityKey_storeParameters(ckey);
   }

   return ckey;
}


/* capacityKey_clearAllKeys clears all keys under the ckElement argument, 
 * including ckElement itself.
 **/
void
capacityKey_clearAllKeys(CapacityKey *ckElement)
{
   CapacityKey *delCk;

   tracepoint(com_ericsson_glms, call_to_function,
              "capacityKey_clearAllKeys");

   while (ckElement != NULL)
   {
      delCk = ckElement;
      ckElement = capacityKey_getNext(ckElement);
      capacityKey_clearKey(delCk);
   }
}

void
capacityKey_clearKey(CapacityKey *ckElement)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityKey_clearKey",
              capacityKey_getKeyId(ckElement));

   if(ckElement == NULL)
      return;

   /* Update parent CapacityState's CapacityKey list pointer if
      ckElement is the first CapacityKey in the list */
   if(capacityState_getFirstCapacityKey(ckElement->parentCapacityState) == ckElement)
      capacityState_setFirstCapacityKey(ckElement->parentCapacityState,
                                      capacityKey_getNext(ckElement)); 

   if(ckElement->nextCapacityKey != NULL)
      (ckElement->nextCapacityKey)->prevCapacityKey = ckElement->prevCapacityKey;

   if(ckElement->prevCapacityKey != NULL)
      (ckElement->prevCapacityKey)->nextCapacityKey = ckElement->nextCapacityKey;

   free(ckElement);
}


void
capacityKey_deleteKeyAndMo(CapacityKey *ckElement)
{
   union itc_msg *sig;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityKey_deleteKeyAndMo",
              capacityKey_getCapacityKeyId(ckElement));

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "Deleting CapacityKey MO instance for %s",
                 capacityKey_getKeyId(ckElement));

   sig = itc_alloc(sizeof(GlmsAdpiDeleteCapacityKeyMoReq),
               GLMS_ADPI_DELETE_CAPACITY_KEY_MO_REQ);
   strcpy(sig->glmsDeleteCapacityKeyMoReq.capacityKeyId,
          capacityKey_getCapacityKeyId(ckElement));
   sendMsgToAdapter(sig);

   pp_deleteIndex(GLMS_TABLE_RID_CAPACITYKEY,
                  capacityKey_getTableIndex(ckElement));

   capacityKey_clearKey(ckElement);
}


char*
capacityKey_getCapacityKeyId(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read capacityKeyId attribute from null pointer");
      return NULL;
   }

   return ckElement->capacityKeyId;
}


char*
capacityKey_getKeyId(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read keyId attribute from null pointer");
      return NULL;
   }

   return capacityState_getCapacityKeyId(capacityKey_getParentCapacityState(ckElement));
}


char*
capacityKey_getName(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read name attribute from null pointer");
      return NULL;
   }

   return (ckElement->parentCapacityState == NULL) ?
          NULL : ckElement->parentCapacityState->name;
}

char*
capacityKey_getCapacityUnit(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read capacityUnit attribute from null pointer");
      return NULL;
   }

   return (ckElement->parentCapacityState == NULL) ?
          NULL : ckElement->parentCapacityState->capacityUnit;
}

uint32_t
capacityKey_getGrantedCapacityLevel(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read grantedCapacityLevel attribute from null pointer");
      return 0;
   }

   return ckElement->licensedCapacityValue.value;
}

GlmsBool
capacityKey_getLicensedCapacityLimitReached(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read licensedCapacityLimitReached attribute "
		  "from null pointer");
      return GLMS_FALSE;
   }

   return (ckElement->parentCapacityState == NULL) ?
          GLMS_FALSE :
          capacityState_getLicensedCapacityLimitReached(ckElement->parentCapacityState);
}

GlmsBool
capacityKey_isNotContractuallyLimited(CapacityKey *ckElement)
{
   if (ckElement != NULL)
      return (ckElement->notContractuallyLimited == 1) ? GLMS_TRUE : GLMS_FALSE;

   tracepoint(com_ericsson_glms, error_trace,
              "Attempt to read notContractuallyLimited attribute "
              "from null pointer");

   return GLMS_FALSE;
}

void
capacityKey_sendMoUpdateCapacityKeyNameInd(CapacityKey *ckElement)
{
   union itc_msg *sigSend;

   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to send name change indication for NULL pointer");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityKey_sendMoUpdateCapacitykeyNameInd",
              capacityKey_getCapacityKeyId(ckElement));

   if(adapter_isSubscribedForMoUpdateInd() &&
      adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateCapacityKeyNameInd),
                      GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_NAME_IND);
      strcpy(sigSend->glmsMoUpdateCapacityKeyNameInd.capacityKeyId,
             capacityKey_getCapacityKeyId(ckElement));
      strcpy(sigSend->glmsMoUpdateCapacityKeyNameInd.name,
             capacityKey_getName(ckElement));
      sendMsgToAdapter(sigSend);
   }
}

void
capacityKey_sendMoUpdateCapacityKeyCapacityUnitInd(CapacityKey *ckElement)
{
   union itc_msg *sigSend;

   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "NULL pointer argument in "
                 "capacityKey_sendMoUpdateCapacityKeyCapacityUnitInd");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "capacityKey_sendMoUpdateCapacitykeyCapacityUnitInd",
              capacityKey_getCapacityKeyId(ckElement));

   if(adapter_isSubscribedForMoUpdateInd() &&
      adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateCapacityKeyCapacityUnitInd),
                      GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_CAPACITY_UNIT_IND);
      strcpy(sigSend->glmsMoUpdateCapacityKeyCapacityUnitInd.capacityUnit,
             capacityKey_getCapacityUnit(ckElement));
      strcpy(sigSend->glmsMoUpdateCapacityKeyCapacityUnitInd.capacityKeyId,
             capacityKey_getCapacityKeyId(ckElement));
      sendMsgToAdapter(sigSend);
   }
}

char*
capacityKey_getProductType(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read productType attribute from null pointer");
      return NULL;
   }

   return (ckElement->parentCapacityState == NULL) ?
		   NULL : ckElement->parentCapacityState->productType;
}

time_t32
capacityKey_getValidFrom(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read validFrom attribute from null pointer");
      return 0;
   }

   return ckElement->validFrom;
}

time_t32
capacityKey_getExpiration(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read expiration attribute from null pointer");
      return 0;
   }

   return ckElement->expiration;
}

GlmsCapacityValue
capacityKey_getLicensedCapacityValue(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read licensedCapacityValue attribute from null pointer");
      return capacityState_getDefaultCapacityValue();
   }

   return ckElement->licensedCapacityValue;
}

uint32_t
capacityKey_getTableIndex(CapacityKey *ckElement)
{
   if(ckElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read tableIndex attribute from null pointer");
      return 0;
   }

   return ckElement->tableIndex;
}

char *
capacityKey_getHwacString(CapacityKey *ckElement)
{
  if(ckElement == NULL)
  {
    tracepoint(com_ericsson_glms, error_trace,
               "Attempt to read hwac attribute from null pointer");
    return "";
  }

  return ckElement->hwacString;
}

void
capacityKey_resetKfParsingMark(CapacityKey *ckElement)
{
   if (ckElement != NULL)
      ckElement->markedAtKfParsing = GLMS_FALSE;
}


void
capacityKey_markAtKfParsing(CapacityKey *ckElement)
{
   if(ckElement != NULL)
   {
      ckElement->markedAtKfParsing = GLMS_TRUE;
   }
}


GlmsBool
capacityKey_isMarkedAtKfParsing(CapacityKey *ckElement)
{
   return (ckElement == NULL) ? GLMS_FALSE : ckElement->markedAtKfParsing;
}


void
capacityKey_storeParameters(CapacityKey *ckElement)
{
   char *parameter;
   char  validFrom_timetStr[20];
   char  expiration_timetStr[20];
   char  noLimit_uint32tStr[20];
   char  value_uint32tStr[20];
   char  ncl_uint32tStr[20];
   uint32_t paramSize;

   if(ckElement == NULL)
      return;

   /* Store persistent parameters:
      ckElement->capacityKeyId
      ckElement->parentCapacityState->keyId
      ckElement->validFrom
      ckElement->expiration
      ckElement->licensedCapacityValue.noLimit
      ckElement->licensedCapacityValue.value
      ckElement->notContractuallyLimited
   */
   if(ckElement->pendingUpdates[GLMS_PP] == GLMS_TRUE)
   {
      paramSize = 0;
      paramSize += strlen(ckElement->capacityKeyId);
      paramSize += strInStr(";",ckElement->capacityKeyId);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(capacityState_getCapacityKeyId(capacityKey_getParentCapacityState(ckElement)));
      paramSize += strInStr(";",
                            capacityState_getCapacityKeyId(capacityKey_getParentCapacityState(ckElement)));
      paramSize += 2; /* Delimiter */
      sprintf(validFrom_timetStr, "%jd", (intmax_t)ckElement->validFrom);
      paramSize += strlen(validFrom_timetStr);
      paramSize += 2; /* Delimiter */
      sprintf(expiration_timetStr, "%jd", (intmax_t)ckElement->expiration);
      paramSize += strlen(expiration_timetStr);
      paramSize += 2; /* Delimiter */
      sprintf(noLimit_uint32tStr, "%"PRIu32, ckElement->licensedCapacityValue.noLimit);
      paramSize += strlen(noLimit_uint32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(value_uint32tStr, "%"PRId32, ckElement->licensedCapacityValue.value);
      paramSize += strlen(value_uint32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(ncl_uint32tStr, "%"PRIu32, ckElement->notContractuallyLimited);
      paramSize += strlen(value_uint32tStr);
      paramSize += 2; /* Delimiter */
      
      paramSize += strlen(ckElement->hwacString);
      paramSize += strInStr(";",ckElement->hwacString);
      
      
      paramSize += 1; /* NULL */
      
      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      fillParameter(parameter, ckElement->capacityKeyId, GLMS_FALSE);
      fillParameter(parameter, capacityState_getCapacityKeyId(capacityKey_getParentCapacityState(ckElement)), GLMS_FALSE);
      fillParameter(parameter, validFrom_timetStr, GLMS_FALSE);
      fillParameter(parameter, expiration_timetStr, GLMS_FALSE);
      fillParameter(parameter, noLimit_uint32tStr, GLMS_FALSE);
      fillParameter(parameter, value_uint32tStr, GLMS_FALSE);
      fillParameter(parameter, ncl_uint32tStr, GLMS_FALSE);
      fillParameter(parameter, ckElement->hwacString, GLMS_TRUE);

      pp_set(GLMS_TABLE_RID_CAPACITYKEY,
             ckElement->tableIndex,
             parameter);

      ckElement->pendingUpdates[GLMS_PP] = GLMS_FALSE;
      free(parameter);
   }
}


void
capacityKey_fetchParameters()
{
   /* Software Parameters shall be requested first and then
      Persistent Parameters. This is because Persistent Parameters
      needs to be handled before the Software Parameters for each
      Capacity Key. */
   sp_requestIndexList(GLMS_TABLE_RID_CAPACITYKEY);
   pp_requestIndexList(GLMS_TABLE_RID_CAPACITYKEY);
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
   return common_handleGlmsAdpiXXIndexListRsp(sig, ckData.tableIndex, ckData.indexesInTable);
}

void
capacityKey_parsePp(union itc_msg *sig)
{
   char *paramStr, *parameter, *capacityKeyId, *keyId;
   char *hwacString = "";
   uint32_t paramCount;
   CapacityState *cs;
   time_t32 validFrom, expiration;
   GlmsCapacityValue licensedCapacityValue;
   uint32_t notContractuallyLimited;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);

   if(paramCount < 6)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "The CapacityKey persistent parameter string contains "
                 "less than the expected 6 or 7 parameter delimiters",
                 paramStr);
      return;
   }

   /* Read parameters:
      ckElement->capacityKeyId
      keyId  (used to find parent CapacityState)
      ckElement->validFrom
      ckElement->expiration
      ckElement->licensedCapacityValue.noLimit
      ckElement->licensedCapacityValue.value
      ckElement->notContractuallyLimited
      ckElement->hwacString
   */

   capacityKeyId = getNextParameter(";:", paramStr);
   cleanupEscapeCharacters(capacityKeyId);

   keyId = getNextParameter(";:", NULL);
   cleanupEscapeCharacters(keyId);

   parameter = getNextParameter(";:", NULL);
   validFrom = (time_t32)strtoll(parameter, NULL, 10);

   parameter = getNextParameter(";:", NULL);
   expiration = (time_t32)strtoll(parameter, NULL, 10);

   parameter = getNextParameter(";:", NULL);
   sscanf(parameter, "%"SCNu32, &licensedCapacityValue.noLimit);

   parameter = getNextParameter(";:", NULL);
   sscanf(parameter, "%"SCNd32, &licensedCapacityValue.value);

   parameter = getNextParameter(";:", NULL);
   sscanf(parameter, "%"SCNu32, &notContractuallyLimited);

   if (paramCount > 6)
   {
     hwacString = getNextParameter(";:", NULL);
     cleanupEscapeCharacters(hwacString);
   }

   /* CapacityState should have been handled before CapacityKey. If no CapacityState
      has been created earlier we will just discard this data. The result is
      that we may lose the stored license state in rollbacks/restores, which
      could result in losing an active autonomous mode state for this capacity.
      This should be a very small issue since we shouldn't expect capacitys
      that didn't exist in an earlier software version to work when doing a
      rollback to that software version. */

   cs = capacityState_findByKeyId(keyId);
   if (cs == NULL)
      tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                 "Unexpected CapacityKeyPersistentParameters received "
                 "(possibly a rollback)",
                 keyId);
   else
      capacityKey_createKeyWithoutMoIndication(capacityState_getCapacityKeyId(cs),
                                               capacityKeyId,
                                               validFrom,
                                               expiration,
                                               sig->glmsPpGetRsp.index,
                                               licensedCapacityValue,
                                               notContractuallyLimited,
                                               hwacString);
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

   if(table == GLMS_SP)
      tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                 "Unexpected CapacityKeySoftwareParameters received "
                 "(possibly rollback)",
                 sig->glmsPpGetRsp.value);
   else if(table == GLMS_PP)
      capacityKey_parsePp(sig);

   /* Store the index that we have read */
   ckData.tableIndex[table][ckData.readIndexesFromTable[table]] = sig->glmsPpGetRsp.index;
   ckData.readIndexesFromTable[table]++;

   if(ckData.readIndexesFromTable[table] != ckData.indexesInTable[table])
      return 3; /* We expect more parameters */

   /* All the indexes from this table are read */
   common_findHighestIndexInTable(ckData.tableIndex[table], ckData.readIndexesFromTable[table], &ckData.tableIndexPool);

   for (CapacityState *cs = capacityState_getFirst();
        cs != NULL;
        cs = capacityState_getNext(cs))
   {
      for (CapacityKey *ck = capacityState_getFirstCapacityKey(cs);
           ck != NULL;
           ck = capacityKey_getNext(ck))
      {
         for (uint32_t i = 0; i < ckData.readIndexesFromTable[table]; i++)
         {
            if (ckData.tableIndex[table][i] == capacityKey_getTableIndex(ck))
               ckData.tableIndex[table][i] = 0;

         }
      }
   }

   common_sanityCheckIndexesInTable(ckData.tableIndex[table], ckData.readIndexesFromTable[table]);
   common_resetIndexesInTable(&ckData.indexesInTable[table], &ckData.readIndexesFromTable[table], &ckData.tableIndex[table]);

   return 1; /* We have read all parameters so return 1. */
}


int32_t
capacityKey_parseStoredParameters(union itc_msg *sig)
{
   return common_parseStoredParameters(sig, ckData.readStatus, handleGlmsAdpiXXGetRsp, handleGlmsAdpiXXIndexListRsp, GLMS_TRUE, GLMS_TRUE);
}

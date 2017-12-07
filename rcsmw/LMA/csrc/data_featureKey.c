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
   uint32_t  sigNo;

   GlmsAdpiCreateFeatureKeyMoReq            glmsCreateFeatureKeyMoReq;
   GlmsAdpiDeleteFeatureKeyMoReq            glmsDeleteFeatureKeyMoReq;
   GlmsAdpiPersistentParameterGetReq        glmsPpGetReq;
   GlmsAdpiPersistentParameterGetRsp        glmsPpGetRsp;
   GlmsAdpiPersistentParameterIndexListReq  glmsPpIndexListReq;
   GlmsAdpiPersistentParameterIndexListRsp  glmsPpIndexListRsp;
   GlmsAdpiSoftwareParameterGetReq          glmsSpGetReq;
   GlmsAdpiSoftwareParameterGetRsp          glmsSpGetRsp;
   GlmsAdpiSoftwareParameterIndexListReq    glmsSpIndexListReq;
   GlmsAdpiSoftwareParameterIndexListRsp    glmsSpIndexListRsp;
   GlmsAdpiMoUpdateFeatureKeyNameInd        glmsMoUpdateFeatureKeyNameInd;

   /* LIHI - LFCI */
   #include "lihi/licenseFeatureControlIUnionContent.h"
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

FeatureKeyData fkData;


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */


/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
featureKeyDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "featureKeyDataInit");

   fkData.tableIndexPool                 = 1;
   fkData.indexesInTable[GLMS_PP]        = 0;
   fkData.indexesInTable[GLMS_SP]        = 0;
   fkData.readIndexesFromTable[GLMS_PP]  = 0;
   fkData.readIndexesFromTable[GLMS_SP]  = 0;
   fkData.readStatus[GLMS_PP]            = 0;
   fkData.readStatus[GLMS_SP]            = 0;
   fkData.tableIndex[GLMS_PP]            = NULL;
   fkData.tableIndex[GLMS_SP]            = NULL;

   fkData.latentKeyList                  = NULL;
}

void
featureKeyDataClear()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "featureKeyDataClear");

   if(fkData.tableIndex[GLMS_PP] != NULL)
      free(fkData.tableIndex[GLMS_PP]);
   if(fkData.tableIndex[GLMS_SP] != NULL)
      free(fkData.tableIndex[GLMS_SP]);

   featureKey_clearLatentKeys();

   featureKeyDataInit();
}

GlmsBool
isFeatureKeyLast(FeatureKey *fkElement)
{
   return (fkElement->nextFeatureKey == NULL) ? GLMS_TRUE : GLMS_FALSE;
}

FeatureKey
*featureKey_getNext(FeatureKey *fkElement)
{
   return (fkElement == NULL) ? NULL : fkElement->nextFeatureKey;
}

FeatureKey
*featureKey_getPrev(FeatureKey *fkElement)
{
   return (fkElement == NULL) ? NULL : fkElement->prevFeatureKey;
}

FeatureKey
*featureKey_findByFeatureKeyId(const char *featureKeyId)
{
   FeatureKey *fk;
   FeatureState *fs;

   for (fs = featureState_getFirst();
        fs != NULL;
        fs = featureState_getNext(fs))
   {
      for (fk = featureState_getFirstFeatureKey(fs);
           fk != NULL;
           fk = featureKey_getNext(fk))
      {
         if (compareMoKeys(featureKeyId,
                           featureKey_getFeatureKeyId(fk)))
         {
            return fk;
         }
      }
   }

   return NULL;
}


FeatureState*
featureKey_getParentFeatureState(FeatureKey *fkElement)
{
   if (fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read parentFeatureState from null pointer");
      return NULL;
   }

   return fkElement->parentFeatureState;
}


static int32_t
featureKey_getMoIdNumber(FeatureKey *fk)
{
   char *fkMoId = featureKey_getFeatureKeyId(fk);
   char *moIdNumber;
   int32_t moIdNumber_i;

   if (fkMoId == NULL)
   {
      return 1;
   }

   moIdNumber = strrchr(fkMoId, '_');

   if (moIdNumber == NULL)
   {
      return 1;
   }

   /* moIdNumber points to the last '-' in the MO Id. 
      moIdNumber+1 is the MO Id number. */
   sscanf((moIdNumber+1), "%"SCNi32, &moIdNumber_i);

   return moIdNumber_i;
}


static int32_t
featureKey_getNextMoIdNumber(const char *keyId)
{
   FeatureState *fs;
   FeatureKey   *fk;
   int32_t       moIdNumber, highestMoIdNumber;

   fs = featureState_findByKeyId(keyId);
   
   if (fs == NULL)
   {
      return 1;
   }

   fk = featureState_getFirstFeatureKey(fs);

   if (fk == NULL)
   {
      return 1;
   }

   highestMoIdNumber = 1;
   while(fk != NULL)
   {
      moIdNumber = featureKey_getMoIdNumber(fk);
      if(moIdNumber > highestMoIdNumber)
      {
         highestMoIdNumber = moIdNumber;
      }

      fk = featureKey_getNext(fk);
   }

   return (highestMoIdNumber + 1);
}


static FeatureKey*
featureKey_createKeyWithoutMoIndication(const char *keyId,
                                        const char *featureKeyId,
                                        time_t32 validFrom,
                                        time_t32 expiration,
                                        GlmsKeyType keyType,
                                        uint32_t tableIndex)
{
   tracepoint(com_ericsson_glms, featureKey_createKeyWithoutMoIndication,
              keyId,
              featureKeyId,
              validFrom,
              expiration,
              tableIndex);

   FeatureState *fstate = featureState_findByKeyId(keyId);

   if (fstate == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Attempted to create FeatureKey for keyId for which"
                 " an FeatureState did not exist",
                 keyId);
      return NULL;
   }

   FeatureKey *fkey = (FeatureKey *) malloc(sizeof(FeatureKey));
   fkey->validFrom          = validFrom;
   fkey->expiration         = expiration;
   fkey->keyType            = keyType;
   fkey->markedAtKfParsing  = GLMS_FALSE;
   fkey->tableIndex         = tableIndex;

   /* Always create with default settings */
   fkey->pendingUpdates[GLMS_PP]   = GLMS_FALSE;
   fkey->pendingUpdates[GLMS_SP]   = GLMS_FALSE;

   strncpy(fkey->featureKeyId, featureKeyId,
           GLMS_MO_KEY_LEN);
   fkey->featureKeyId[GLMS_MO_KEY_LEN - 1] = '\0';


   /* fkey->nextFeatureKey and fkey->prevFeatureKey are updated
      in call to featureState_addFeatureKey */
   featureState_addFeatureKey(fstate, fkey);
   fkey->parentFeatureState = fstate;

   return fkey;
}

FeatureKey
*featureKey_createKeyWithMoIndication(const char *keyId,
                                      time_t32 validFrom,
                                      time_t32 expiration,
                                      GlmsKeyType keyType)
{
   int32_t moIdNumber = featureKey_getNextMoIdNumber(keyId);

   char featureKeyId[GLMS_MO_KEY_LEN];
   snprintf(featureKeyId, GLMS_MO_KEY_LEN, "%s_%d",
            keyId,
            moIdNumber);
   featureKeyId[GLMS_MO_KEY_LEN - 1] = '\0';

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureKey_createKeyWithMoIndication",
              featureKeyId);

   FeatureKey *fkey = featureKey_createKeyWithoutMoIndication(keyId,
                                                              featureKeyId,
                                                              validFrom,
                                                              expiration,
                                                              keyType,
                                                              fkData.tableIndexPool++);

   if (fkey != NULL)
   {
      glms_logEvent(GLMS_LOG_MEDIUM,
                    "Creating FeatureKey MO instance for %s",
                    featureState_getKeyId(fkey->parentFeatureState));
      
      /* Send Create FeatureKey MO to adapter */
      union itc_msg *sig = itc_alloc(sizeof(GlmsAdpiCreateFeatureKeyMoReq),
                                     GLMS_ADPI_CREATE_FEATURE_KEY_MO_REQ);
      sig->glmsCreateFeatureKeyMoReq.validFrom = featureKey_getValidFrom(fkey);
      sig->glmsCreateFeatureKeyMoReq.expiration = featureKey_getExpiration(fkey);
      strcpy(sig->glmsCreateFeatureKeyMoReq.featureKeyId, featureKey_getFeatureKeyId(fkey));
      strcpy(sig->glmsCreateFeatureKeyMoReq.keyId, featureKey_getKeyId(fkey));
      strcpy(sig->glmsCreateFeatureKeyMoReq.name, featureKey_getName(fkey));
      strcpy(sig->glmsCreateFeatureKeyMoReq.productType, featureKey_getProductType(fkey));
      sig->glmsCreateFeatureKeyMoReq.shared = featureKey_getKeyType(fkey);
      sendMsgToAdapter(sig);

      fkey->pendingUpdates[GLMS_PP]   = GLMS_TRUE;
      fkey->pendingUpdates[GLMS_SP]   = GLMS_TRUE;

      featureKey_storeParameters(fkey);
   }

   return fkey;
}

/* featureKey_clearKeys clears all keys, of the given key type,
 * under the fk argument. 'fk' itself will be cleared if it
 * is of the given key type.
 *
 * This function can take the special KeyType value of -1 which will clear all
 * Feature Keys no matter which key type it has.
 *
 **/
void
featureKey_clearKeys(FeatureKey *fk,
                     GlmsKeyType keyType)
{
   FeatureKey *delFk;

   tracepoint(com_ericsson_glms, call_to_function,
              "featureKey_clearKeys");

   while(fk != NULL)
   {
      if(keyType == (GlmsKeyType)-1 ||
         featureKey_getKeyType(fk) == keyType)
      {
         delFk = fk;
         fk = featureKey_getNext(fk);

         featureKey_clearKey(delFk);
      }
   }
}

void
featureKey_clearKey(FeatureKey *fkElement)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureKey_clearKey",
              featureKey_getKeyId(fkElement));

   if (fkElement == NULL)
   {
      return;
   }

   /* Update parent FeatureState's FeatureKey list pointer if
      fkElement is the first FeatureKey in the list */
   if (featureState_getFirstFeatureKey(fkElement->parentFeatureState) == fkElement)
   {
      featureState_setFirstFeatureKey(fkElement->parentFeatureState,
                                      featureKey_getNext(fkElement)); 
   }

   if (fkElement->nextFeatureKey != NULL)
   {
      (fkElement->nextFeatureKey)->prevFeatureKey = fkElement->prevFeatureKey;
   }
   if (fkElement->prevFeatureKey != NULL)
   {
      (fkElement->prevFeatureKey)->nextFeatureKey = fkElement->nextFeatureKey;
   }

   free(fkElement);
}


void
featureKey_deleteKeyAndMo(FeatureKey *fkElement)
{
   union itc_msg *sig;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureKey_deleteKeyAndMo",
              featureKey_getFeatureKeyId(fkElement));

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "Deleting FeatureKey MO instance for %s",
                 featureKey_getKeyId(fkElement));

   sig = itc_alloc(sizeof(GlmsAdpiDeleteFeatureKeyMoReq),
                   GLMS_ADPI_DELETE_FEATURE_KEY_MO_REQ);
   strcpy(sig->glmsDeleteFeatureKeyMoReq.featureKeyId,
          featureKey_getFeatureKeyId(fkElement));
   sendMsgToAdapter(sig);

   pp_deleteIndex(GLMS_TABLE_RID_FEATUREKEY,
                  featureKey_getTableIndex(fkElement));

   featureKey_clearKey(fkElement);
}


char*
featureKey_getFeatureKeyId(FeatureKey *fkElement)
{
   if(fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read featureKeyId attribute from null pointer");
      return NULL;
   }

   return fkElement->featureKeyId;
}


char*
featureKey_getKeyId(FeatureKey *fkElement)
{
   if(fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read keyId attribute from null pointer");
      return NULL;
   }

   return featureState_getKeyId(featureKey_getParentFeatureState(fkElement));
}


char*
featureKey_getName(FeatureKey *fkElement)
{
   if(fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read name attribute from null pointer");
      return NULL;
   }

   return (fkElement->parentFeatureState == NULL) ?
		   NULL : fkElement->parentFeatureState->name;
}

void
featureKey_sendMoUpdateFeatureKeyNameInd(FeatureKey *fkElement)
{
   if (fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to send name change indication for NULL pointer");
      return;
   }

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "featureKey_sendMoUpdateFeaturekeyNameInd",
              featureKey_getFeatureKeyId(fkElement));

   if (adapter_isSubscribedForMoUpdateInd() &&
       adapter_isAdpiActivated())
   {
      union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateFeatureKeyNameInd),
                                         GLMS_ADPI_MO_UPDATE_FEATURE_KEY_NAME_IND);
      strcpy(sigSend->glmsMoUpdateFeatureKeyNameInd.featureKeyId,
             featureKey_getFeatureKeyId(fkElement));
      strcpy(sigSend->glmsMoUpdateFeatureKeyNameInd.name,
             featureKey_getName(fkElement));
      sendMsgToAdapter(sigSend);
   }
}

char*
featureKey_getProductType(FeatureKey *fkElement)
{
   if (fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read productType attribute from null pointer");
      return NULL;
   }

   return (fkElement->parentFeatureState == NULL) ?
           NULL : fkElement->parentFeatureState->productType;
}

time_t32
featureKey_getValidFrom(FeatureKey *fkElement)
{
   if (fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read validFrom attribute from null pointer");
      return 0;
   }

   return fkElement->validFrom;
}

time_t32
featureKey_getExpiration(FeatureKey *fkElement)
{
   if (fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read expiration attribute from null pointer");
      return 0;
   }

   return fkElement->expiration;
}

GlmsKeyType
featureKey_getKeyType(FeatureKey *fkElement)
{
   if (fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read key type attribute from null pointer");
      return 0;
   }

   return fkElement->keyType;
}

uint32_t
featureKey_getTableIndex(FeatureKey *fkElement)
{
   if (fkElement == NULL)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Attempt to read tableIndex attribute from null pointer");
      return 0;
   }

   return fkElement->tableIndex;
}


void
featureKey_resetKfParsingMark(FeatureKey *fkElement, GlmsKeyType keyType)
{
   if(fkElement != NULL &&
      featureKey_getKeyType(fkElement) == keyType)
   {
      fkElement->markedAtKfParsing = GLMS_FALSE;
   }
}


void
featureKey_markAtKfParsing(FeatureKey *fkElement)
{
   if(fkElement != NULL)
   {
      fkElement->markedAtKfParsing = GLMS_TRUE;
   }
}


GlmsBool
featureKey_isMarkedAtKfParsing(FeatureKey *fkElement)
{
   if(fkElement != NULL)
   {
      return fkElement->markedAtKfParsing;
   }

   return GLMS_FALSE;
}

uint32_t
featureKey_getStoreParametersSize(FeatureKey *fkElement,
                                  char validFrom_timetStr[20],
                                  char expiration_timetStr[20],
                                  char keyTypeStr[2])
{
   uint32_t ret = 0;
   ret += strlen(fkElement->featureKeyId);
   ret += strInStr(";", fkElement->featureKeyId);
   ret += 2; /* Delimiter */
   ret += strlen(featureState_getKeyId(featureKey_getParentFeatureState(fkElement)));
   ret += strInStr(";",
   featureState_getKeyId(featureKey_getParentFeatureState(fkElement)));
   ret += 2; /* Delimiter */
   sprintf(validFrom_timetStr, "%jd", (intmax_t)fkElement->validFrom);
   ret += strlen(validFrom_timetStr);
   ret += 2; /* Delimiter */
   sprintf(expiration_timetStr, "%jd", (intmax_t)fkElement->expiration);
   ret += strlen(expiration_timetStr);
   ret += 2; /* Delimiter */
   sprintf(keyTypeStr, "%d", fkElement->keyType % 10);
   ret += 1; /* KeyType */
   ret += 1; /* Null */
   return ret;
}

void
featureKey_storeParameters(FeatureKey *fkElement)
{
   if(fkElement == NULL)
   {
      return;
   }

   /* Store persistent parameters:
      fkElement->featureKeyId
      fkElement->parentFeatureState->keyId
      fkElement->validFrom
      fkElement->expiration
      fkElement->keyType
   */
   if (fkElement->pendingUpdates[GLMS_PP] == GLMS_TRUE)
   {
      char  validFrom_timetStr[20];
      char  expiration_timetStr[20];
      char  keyTypeStr[2];
      uint32_t paramSize = featureKey_getStoreParametersSize(fkElement,
                                                             validFrom_timetStr,
                                                             expiration_timetStr,
                                                             keyTypeStr);
      char *parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      fillParameter(parameter, fkElement->featureKeyId, GLMS_FALSE);
      fillParameter(parameter,
                    featureState_getKeyId(featureKey_getParentFeatureState(fkElement)),
                    GLMS_FALSE);
      fillParameter(parameter, validFrom_timetStr, GLMS_FALSE);
      fillParameter(parameter, expiration_timetStr, GLMS_FALSE);
      fillParameter(parameter, keyTypeStr, GLMS_TRUE);

      pp_set(GLMS_TABLE_RID_FEATUREKEY, fkElement->tableIndex, parameter);

      fkElement->pendingUpdates[GLMS_PP] = GLMS_FALSE;
      free(parameter);
   }
}


void
featureKey_fetchParameters()
{
   /* Software Parameters shall be requested first and then
      Persistent Parameters. This is because Persistent Parameters
      needs to be handled before the Software Parameters for each
      Feature Key. */
   sp_requestIndexList(GLMS_TABLE_RID_FEATUREKEY);
   pp_requestIndexList(GLMS_TABLE_RID_FEATUREKEY);
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
   return common_handleGlmsAdpiXXIndexListRsp(sig, fkData.tableIndex, fkData.indexesInTable);
}

void
featureKey_parsePp(union itc_msg *sig)
{
   char *paramStr, *parameter, *featureKeyId, *keyId;
   uint32_t paramCount;
   FeatureState *fs;
   time_t32 validFrom, expiration;
   GlmsKeyType keyType = GLMS_KEY_TYPE_NODE;

   paramStr = sig->glmsPpGetRsp.value;
   paramCount = strInStr(";:", paramStr);

   if (paramCount < 3)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "The FeatureKey persistent parameter string contains "
                 "less than the expected 3 parameter delimiters",
                 paramStr);
      return;
   }

   /* Read parameters:
      fkElement->featureKeyId
      keyId  (used to find parent FeatureState)
      fkElement->validFrom
      fkElement->expiration
      fkElement->keyType
    */

   featureKeyId = getNextParameter(";:", paramStr);
   cleanupEscapeCharacters(featureKeyId);

   keyId = getNextParameter(";:", NULL);
   cleanupEscapeCharacters(keyId);

   parameter = getNextParameter(";:", NULL);
   validFrom = (time_t32)strtoll(parameter, NULL, 10);

   parameter = getNextParameter(";:", NULL);
   expiration = (time_t32)strtoll(parameter, NULL, 10);

   if(paramCount >= 4)
   {
      parameter = getNextParameter(";:", NULL);
      keyType = (GlmsKeyType)strtoll(parameter, NULL, 10);
   }

   /* FeatureState should have been handled before FeatureKey. If no FeatureState
      has been created earlier we will just discard this data. The result is
      that we may lose the stored license state in rollbacks/restores, which
      could result in losing an active autonomous mode state for this feature.
      This should be a very small issue since we shouldn't expect features
      that didn't exist in an earlier software version to work when doing a
      rollback to that software version. */
   fs = featureState_findByKeyId(keyId);
   if (fs == NULL)
   {
      tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                 "Unexpected FeatureKeyPersistentParameters received "
                 "(possibly rollback)",
                 keyId);
   }
   else
   {
      featureKey_createKeyWithoutMoIndication(featureState_getKeyId(fs),
                                              featureKeyId,
                                              validFrom,
                                              expiration,
                                              keyType,
                                              sig->glmsPpGetRsp.index);
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
      return 1;

   if (table == GLMS_SP)
   {
      tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                 "Unexpected FeatureKeySoftwareParameters received "
                 "(possibly rollback)",
                 sig->glmsPpGetRsp.value);
   }
   else if (table == GLMS_PP)
   {
      featureKey_parsePp(sig);
   }

   /* Store the index that we have read */
   fkData.tableIndex[table][fkData.readIndexesFromTable[table]] = sig->glmsPpGetRsp.index;
   fkData.readIndexesFromTable[table]++;

   if(fkData.readIndexesFromTable[table] != fkData.indexesInTable[table])
   {
      return 3; /* We expect more parameters */
   }

   /* All the indexes from this table are read */
   common_findHighestIndexInTable(fkData.tableIndex[table], fkData.readIndexesFromTable[table], &fkData.tableIndexPool);

   for (FeatureState *fs = featureState_getFirst();
        fs != NULL;
        fs = featureState_getNext(fs))
   {
      for (FeatureKey *fk = featureState_getFirstFeatureKey(fs);
           fk != NULL;
           fk = featureKey_getNext(fk))
      {
         for (uint32_t i = 0; i < fkData.readIndexesFromTable[table]; i++)
         {
            if (fkData.tableIndex[table][i] == featureKey_getTableIndex(fk))
            {
               fkData.tableIndex[table][i] = 0;
            }
         }
      }
   }

   common_sanityCheckIndexesInTable(fkData.tableIndex[table], fkData.readIndexesFromTable[table]);
   common_resetIndexesInTable(&fkData.indexesInTable[table], &fkData.readIndexesFromTable[table], &fkData.tableIndex[table]);

   return 1; /* We have read all parameters so return 1. */
}

int32_t
featureKey_parseStoredParameters(union itc_msg *sig)
{
   return common_parseStoredParameters(sig, fkData.readStatus, handleGlmsAdpiXXGetRsp, handleGlmsAdpiXXIndexListRsp, GLMS_TRUE, GLMS_TRUE);
}

LatentFeatureKey *
featureKey_findLatentKey(const char *keyId)
{
   LatentFeatureKey *key = fkData.latentKeyList;

   while(key)
   {
      if(!strcmp(keyId, key->keyId))
      {
         return key;
      }

      key = key->nextFeatureKey;
   }

   return NULL;
}

LatentFeatureKey *
featureKey_findUniqueLatentKey(const char *keyId,
                               time_t32 validFrom,
                               time_t32 expiration,
                               GlmsKeyType keyType)
{
   LatentFeatureKey *key = fkData.latentKeyList;

   while(key)
   {
      if(!strcmp(keyId, key->keyId) &&
         key->validFrom == validFrom &&
         key->expiration == expiration &&
         key->keyType == keyType)
      {
         return key;
      }

      key = key->nextFeatureKey;
   }

   return NULL;
}

void
featureKey_resetLatentKeysMark(GlmsKeyType keyType)
{
   LatentFeatureKey *key = fkData.latentKeyList;

   while(key)
   {
      if(key->keyType == keyType)
      {
         key->markedAtKfParsing = GLMS_FALSE;
         key = key->nextFeatureKey;
      }
   }
}

void
featureKey_markLatentKey(LatentFeatureKey *key)
{
   key->markedAtKfParsing = GLMS_TRUE;
}

static void
clearLatentKeys(GlmsKeyType keyType, GlmsBool unmarkedOnly)
{
   LatentFeatureKey *key = fkData.latentKeyList;
   LatentFeatureKey *pkey = NULL;
   LatentFeatureKey *nextKey = NULL;
   GlmsBool markedCheck;

   while(key)
   {
      nextKey = key->nextFeatureKey; /* Save next in case 'key' is free'd */
      markedCheck = unmarkedOnly ? !key->markedAtKfParsing : GLMS_TRUE;

      if(key->keyType == keyType &&
         markedCheck)
      {
         if(key == fkData.latentKeyList)
            fkData.latentKeyList = key->nextFeatureKey;

         if(pkey)
            pkey->nextFeatureKey = key->nextFeatureKey;

         free(key);
      }
      else
      {
         pkey = key;
      }

      key = nextKey;
   }
}

void
featureKey_clearUnmarkedLatentKeys(GlmsKeyType keyType)
{
   clearLatentKeys(keyType, GLMS_TRUE);
}

void
featureKey_clearLatentKeysOfKeyType(GlmsKeyType keyType)
{
   clearLatentKeys(keyType, GLMS_FALSE);
}

void
featureKey_clearLatentKeys(void)
{
   LatentFeatureKey *rkey = fkData.latentKeyList;
   LatentFeatureKey *nkey;

   while(rkey)
   {
      nkey = rkey->nextFeatureKey;
      free(rkey);
      rkey = nkey;
   }
}

LatentFeatureKey *
featureKey_addLatentKey(const char *keyId,
                        time_t32 validFrom,
                        time_t32 expiration,
                        GlmsKeyType keyType)
{
   LatentFeatureKey *key = featureKey_findUniqueLatentKey(keyId,
                                                          validFrom,
                                                          expiration,
                                                          keyType);
   if(!key)
   {
      key = (LatentFeatureKey *)malloc(sizeof(LatentFeatureKey));

      strncpy(key->keyId, keyId, GLMS_KEY_ID_LEN);
      key->keyId[GLMS_KEY_ID_LEN - 1] = '\0';

      key->validFrom          = validFrom;
      key->expiration         = expiration;
      key->keyType            = keyType;
      key->markedAtKfParsing  = GLMS_FALSE;

      key->nextFeatureKey     = fkData.latentKeyList;
      fkData.latentKeyList    = key;
   }

   return key;
}

void
featureKey_clearLatentKey(LatentFeatureKey *key)
{
   LatentFeatureKey *pkey = NULL;

   if(key == fkData.latentKeyList)
   {
     /* key is first in the list */
     fkData.latentKeyList = key->nextFeatureKey;
   }
   else
   {
     pkey = fkData.latentKeyList;
   }

   while(pkey)
   {
     if(pkey->nextFeatureKey == key)
     {
       pkey->nextFeatureKey = key->nextFeatureKey;
       break;
     }
     pkey = pkey->nextFeatureKey;
   }

   free(key);
}

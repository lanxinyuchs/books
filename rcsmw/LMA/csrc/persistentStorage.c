
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
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "com_ericsson_glms.h"

#include "glmsadpi/glmsDataTypes.h"
#include "glmsadpi/glms_adpi.sig"
#include "glms_main.h"
#include "persistentStorage.h"
#include "data_lm.h"
#include "data_capacityKey.h"
#include "data_featureKey.h"
#include "data_capacityState.h"
#include "data_featureState.h"
#include "data_keyFile.h"
#include "data_unlocks.h"
#include "data_licenseSupport.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t  sigNo;

   GlmsAdpiPersistentParameterIndexListReq    ppIndexListReq;
   GlmsAdpiPersistentParameterIndexListRsp    ppIndexListRsp;
   GlmsAdpiPersistentParameterSetReq          ppSetReq;
   GlmsAdpiPersistentParameterSetRsp          ppSetRsp;
   GlmsAdpiPersistentParameterGetReq          ppGetReq;
   GlmsAdpiPersistentParameterGetRsp          ppGetRsp;
   GlmsAdpiPersistentParameterDeleteIndexReq  ppDeleteIndexReq;
   GlmsAdpiPersistentParameterDeleteIndexRsp  ppDeleteIndexRsp;

   GlmsAdpiSoftwareParameterIndexListReq      spIndexListReq;
   GlmsAdpiSoftwareParameterIndexListRsp      spIndexListRsp;
   GlmsAdpiSoftwareParameterSetReq            spSetReq;
   GlmsAdpiSoftwareParameterSetRsp            spSetRsp;
   GlmsAdpiSoftwareParameterGetReq            spGetReq;
   GlmsAdpiSoftwareParameterGetRsp            spGetRsp;
   GlmsAdpiSoftwareParameterDeleteIndexReq    spDeleteIndexReq;
   GlmsAdpiSoftwareParameterDeleteIndexRsp    spDeleteIndexRsp;
};

typedef struct
{
   char spTableName[100];
   char ppTableName[100];
   int32_t (*parseStoredParameters)(union itc_msg *sig);
} RidMap;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static GlmsBool ridMapSet = GLMS_FALSE;
RidMap ridMap[GLMS_NO_OF_PARAMETER_TABLES];

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

int32_t
getParameterTableFromSig(uint32_t sigNo)
{
   if ((sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP) ||
       (sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP))
      return GLMS_PP;

   if ((sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP) ||
       (sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP))
      return GLMS_SP;

   tracepoint(com_ericsson_glms, error_trace_w_hex_arg,
     "Faulty signal in function getParameterTableFromSig",
     sigNo);

   return -1;
}

void
initRidMap()
{
   for (int currentRid = 0; currentRid < GLMS_NO_OF_PARAMETER_TABLES; ++currentRid)
   {
      switch (currentRid)
      {
         case GLMS_TABLE_RID_LM:
            strcpy(ridMap[currentRid].spTableName, "LicenseManagerSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "LicenseManagerPersistentData");
            ridMap[currentRid].parseStoredParameters = lm_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_KEYFILE:
            strcpy(ridMap[currentRid].spTableName, "KeyFileSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "KeyFilePersistentData");
            ridMap[currentRid].parseStoredParameters = keyFile_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_FEATURESTATE:
            strcpy(ridMap[currentRid].spTableName, "FeatureStateSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "FeatureStatePersistentData");
            ridMap[currentRid].parseStoredParameters = featureState_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_FEATUREKEY:
            strcpy(ridMap[currentRid].spTableName, "FeatureKeySoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "FeatureKeyPersistentData");
            ridMap[currentRid].parseStoredParameters = featureKey_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_EU:
            strcpy(ridMap[currentRid].spTableName, "EUSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "EUPersistentData");
            ridMap[currentRid].parseStoredParameters = eu_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_IU:
            strcpy(ridMap[currentRid].spTableName, "IUSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "IUPersistentData");
            ridMap[currentRid].parseStoredParameters = iu_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_PU:
            strcpy(ridMap[currentRid].spTableName, "PUSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "PUPersistentData");
            ridMap[currentRid].parseStoredParameters = pu_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_AM:
            strcpy(ridMap[currentRid].spTableName, "AmSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "AmPersistentData");
            ridMap[currentRid].parseStoredParameters = am_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_CAPACITYSTATE:
            strcpy(ridMap[currentRid].spTableName, "CapacityStateSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "CapacityStatePersistentData");
            ridMap[currentRid].parseStoredParameters = capacityState_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_CAPACITYKEY:
            strcpy(ridMap[currentRid].spTableName, "CapacityKeySoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "CapacityKeyPersistentData");
            ridMap[currentRid].parseStoredParameters = capacityKey_parseStoredParameters;
            break;
         case GLMS_TABLE_RID_LICENSESUPPORT:
            strcpy(ridMap[currentRid].spTableName, "LicenseSupportSoftwareData");
            strcpy(ridMap[currentRid].ppTableName, "LicenseSupportPersistentData");
            ridMap[currentRid].parseStoredParameters = ls_parseStoredParameters;
            break;
      }
   }
}

int32_t
ridMap_parseStoredParameters(union itc_msg *sig, uint32_t rid)
{
   if (!ridMapSet)
   {
      initRidMap();
      ridMapSet = GLMS_TRUE;
   }

   return ridMap[rid].parseStoredParameters(sig);
}

char *
ridMap_getTableName(uint32_t rid, uint32_t tableType)
{
   if (!ridMapSet)
   {
      initRidMap();
      ridMapSet = GLMS_TRUE;
   }

   if (tableType == GLMS_PP)
      return ridMap[rid].ppTableName;
   else if (tableType == GLMS_SP)
      return ridMap[rid].spTableName;

   return NULL;
}

void
pp_requestIndexList(uint32_t requestId)
{
   union itc_msg *sig;
   char *table = ridMap_getTableName(requestId, GLMS_PP);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "pp_requestIndexList", requestId);
   tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
              "Requesting index list from table", table);

   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiPersistentParameterIndexListReq),
                                GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_REQ);
   sig->ppIndexListReq.requestId = requestId;
   strncpy(sig->ppIndexListReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->ppIndexListReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';
   sendMsgToAdapter(sig);
}

void
pp_set(uint32_t requestId, uint32_t index, const char *value)
{
   union itc_msg *sig;
   uint32_t valueLen;
   char *table = ridMap_getTableName(requestId, GLMS_PP);

   tracepoint(com_ericsson_glms, pp_set,
              requestId, table, index, value);

   valueLen = strlen(value);
   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiPersistentParameterSetReq) + valueLen,
                                GLMS_ADPI_PERSISTENT_PARAMETER_SET_REQ);

   sig->ppSetReq.requestId = requestId;
   sig->ppSetReq.index     = index;
   strncpy(sig->ppSetReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->ppSetReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';
   strcpy(sig->ppSetReq.value,
          value);

   sendMsgToAdapter(sig);
}


void
pp_get(uint32_t requestId)
{
   union itc_msg *sig;
   char *table = ridMap_getTableName(requestId, GLMS_PP);

   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiPersistentParameterGetReq),
                                GLMS_ADPI_PERSISTENT_PARAMETER_GET_REQ);

   sig->ppGetReq.requestId = requestId;
   strncpy(sig->ppGetReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->ppGetReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';

   sendMsgToAdapter(sig);
}


void
pp_deleteIndex(uint32_t requestId, uint32_t index)
{
   union itc_msg *sig;
   char *table = ridMap_getTableName(requestId, GLMS_PP);

   tracepoint(com_ericsson_glms, pp_deleteIndex,
              requestId, table, index);

   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiPersistentParameterDeleteIndexReq),
                                GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_REQ);

   sig->ppDeleteIndexReq.requestId = requestId;
   sig->ppDeleteIndexReq.index     = index;
   strncpy(sig->ppDeleteIndexReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->ppDeleteIndexReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';

   sendMsgToAdapter(sig);
}


void
sp_requestIndexList(uint32_t requestId)
{
   union itc_msg *sig;
   char *table = ridMap_getTableName(requestId, GLMS_SP);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "sp_requestIndexList", requestId);
   tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
              "Requesting index list from table", table);

   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiSoftwareParameterIndexListReq),
                                GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_REQ);

   sig->spIndexListReq.requestId = requestId;
   strncpy(sig->spIndexListReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->spIndexListReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';

   sendMsgToAdapter(sig);
}


void
sp_set(uint32_t requestId, uint32_t index, const char *value)
{
   union itc_msg *sig;
   uint32_t valueLen;
   char *table = ridMap_getTableName(requestId, GLMS_SP);

   tracepoint(com_ericsson_glms, sp_set,
              requestId, table, index, value);

   valueLen = strlen(value);

   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiSoftwareParameterSetReq) + valueLen,
                                     GLMS_ADPI_SOFTWARE_PARAMETER_SET_REQ);
   sig->spSetReq.requestId = requestId;
   sig->spSetReq.index     = index;

   strncpy(sig->spSetReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->spSetReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';

   strcpy(sig->spSetReq.value,
          value);

   sendMsgToAdapter(sig);
}


void
sp_get(uint32_t requestId)
{
   union itc_msg *sig;
   char *table = ridMap_getTableName(requestId, GLMS_SP);

   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiSoftwareParameterGetReq),
                                GLMS_ADPI_SOFTWARE_PARAMETER_GET_REQ);

   sig->spGetReq.requestId = requestId;
   strncpy(sig->spGetReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->spGetReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';

   sendMsgToAdapter(sig);
}


void
sp_deleteIndex(uint32_t requestId, uint32_t index)
{
   union itc_msg *sig;
   char *table = ridMap_getTableName(requestId, GLMS_SP);

   tracepoint(com_ericsson_glms, sp_deleteIndex,
              requestId, table, index);

   sig = (union itc_msg *) itc_alloc(sizeof(GlmsAdpiSoftwareParameterDeleteIndexReq),
                                GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_REQ);
   sig->spDeleteIndexReq.requestId = requestId;
   sig->spDeleteIndexReq.index     = index;
   strncpy(sig->spDeleteIndexReq.table,
           table,
           GLMS_TABLE_NAME_LEN);
   sig->spDeleteIndexReq.table[GLMS_TABLE_NAME_LEN - 1] = '\0';
   sendMsgToAdapter(sig);
}


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
#include "clients.h"

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
};

int32_t
common_handleGlmsAdpiXXIndexListRsp(union itc_msg *sig,
                                    uint32_t *tableIndex[],
                                    uint32_t indexesInTable[])
{
   int table = getParameterTableFromSig(sig->sigNo);
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleGlmsAdpiXXIndexListRsp", table);

   /* Both signals can be read as if it is a glmsPpIndexListRsp */
   if (isSignalResultOk(sig->glmsPpIndexListRsp.result))
   {
      if (sig->glmsPpIndexListRsp.nrOfIndexes == 0)
      {
         return 1; /* We dont request any parameters since the table is empty */
      }
      else
      {
         if ((tableIndex != NULL) && (indexesInTable != NULL))
         {
            indexesInTable[table] = sig->glmsPpIndexListRsp.nrOfIndexes;
            tableIndex[table] = (uint32_t *)malloc(indexesInTable[table] * sizeof(uint32_t));

            for (uint32_t i = 0; i < indexesInTable[table]; ++i)
            {
               tableIndex[table][i] = sig->glmsPpIndexListRsp.index[i];
            }
         }

         if (table == GLMS_SP)
         {
            sp_get(sig->glmsPpIndexListRsp.requestId);
         }
         else if(table == GLMS_PP)
         {
            pp_get(sig->glmsPpIndexListRsp.requestId);
         }

         return 2; /* We expect some parameters */
      }
   }
   else
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Failed to request index list of table",
                 table);
      return -1;
   }
}

int32_t
common_prepareForParsingStoredParameters(union itc_msg *sig,
                                         GlmsBool canReceiveSp,
                                         GlmsBool canReceivePp)
{
   int table = getParameterTableFromSig(sig->sigNo);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "common_prepareForParsingStoredParameters", table);

   if (!isSignalResultOk(sig->glmsPpGetRsp.result))
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Failed to prepare for parsing stored parameters",
                 table);
      return -1;
   }

   if (canReceivePp && (table == GLMS_PP))
      return GLMS_PP;
   else if (canReceiveSp && (table == GLMS_SP))
      return GLMS_SP;

   return -1;
}

int32_t
common_parseStoredParameters(union itc_msg *sig,
                              int32_t readStatus[],
                              int32_t(*getRspExec)(union itc_msg *sig),
                              int32_t(*listRspExec)(union itc_msg *sig),
                              GlmsBool checkPp,
                              GlmsBool checkSp)
{
   int32_t table = getParameterTableFromSig(sig->sigNo);

   if ((sig->sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP) ||
       (sig->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP))
      readStatus[table] = listRspExec(sig);
   else if ((sig->sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP) ||
            (sig->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP))
      readStatus[table] = getRspExec(sig);

   GlmsBool ppReadOk = (readStatus[GLMS_PP] == 1) || (readStatus[GLMS_PP] == -1);
   GlmsBool spReadOk = (readStatus[GLMS_SP] == 1) || (readStatus[GLMS_SP] == -1);

   GlmsBool allRead = checkPp ? ppReadOk : GLMS_TRUE;
   allRead &= checkSp ? spReadOk : GLMS_TRUE;

   if (allRead)
   {
      readStatus[GLMS_PP] = 0;
      readStatus[GLMS_SP] = 0;
      return 1;
   }

   return 2;
}

void
common_findHighestIndexInTable(uint32_t tableIndex[],
                               uint32_t noOfReadIndexes,
                               uint32_t *highestIndex)
{
   for (uint32_t i = 0; i < noOfReadIndexes; ++i)
   {
      if (tableIndex[i] >= *highestIndex)
      {
         *highestIndex = tableIndex[i] + 1;
      }
   }
}

void
common_sanityCheckIndexesInTable(uint32_t tableIndex[],
                                 uint32_t noOfReadIndexes)
{
   for (uint32_t i = 0; i < noOfReadIndexes; ++i)
   {
      if (tableIndex[i] != 0)
      {
         tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                    "Unexpected index in table (normal for rollbacks)",
                    tableIndex[i]);
         tableIndex[i] = 0;
      }
   }
}

void
common_resetIndexesInTable(uint32_t *indexesInTable,
                           uint32_t *noOfReadIndexes,
                           uint32_t *tableIndex[])
{
   *indexesInTable = 0;
   *noOfReadIndexes = 0;
   free(*tableIndex);
   *tableIndex = NULL;
}

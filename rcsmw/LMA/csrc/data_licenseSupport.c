/**
 *   Copyright (C) 2017 by Ericsson AB. All rights reserved. The
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
#include "glms_main.h"
#include "glmsUtils.h"
#include "persistentStorage.h"
#include "data_licenseSupport.h"
#include "data_common.h"
#include "data_featureState.h"

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t sigNo;

   GlmsAdpiPersistentParameterGetReq        glmsPpGetReq;
   GlmsAdpiPersistentParameterGetRsp        glmsPpGetRsp;
   GlmsAdpiPersistentParameterIndexListReq  glmsPpIndexListReq;
   GlmsAdpiPersistentParameterIndexListRsp  glmsPpIndexListRsp;
   GlmsAdpiSoftwareParameterGetReq          glmsSpGetReq;
   GlmsAdpiSoftwareParameterGetRsp          glmsSpGetRsp;
   GlmsAdpiSoftwareParameterIndexListReq    glmsSpIndexListReq;
   GlmsAdpiSoftwareParameterIndexListRsp    glmsSpIndexListRsp;

   GlmsAdpiUpdateAreaIdReq                  glmsUpdateAreaIdReq;
   GlmsAdpiUpdateAreaIdRsp                  glmsUpdateAreaIdRsp;
   GlmsAdpiMoUpdateLicenseSupportInd        glmsMoUpdateLicenseSupportInd;
};

#define ALKF_PARAM_DELIMITER  "@"
#define ALKF_NO_STOP_DATE  "-1"
#define ALKF_EMERGENCY_RESET_KEY_ID  "emergencyReset"

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

LicenseSupportData lsData;


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static const char* getNextAlkfParameter(const char* alkf);


/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
licenseSupportDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "licenseSupportDataInit");

   lsData.areaId[0] = '\0';
   lsData.seqNr     = 0;

   lsData.sendMoUpdateInd         = GLMS_FALSE;
   lsData.pendingUpdates[GLMS_PP] = GLMS_FALSE;
   lsData.pendingUpdates[GLMS_SP] = GLMS_FALSE;
   lsData.tableIndex[GLMS_PP]     = NULL;
   lsData.tableIndex[GLMS_SP]     = NULL;
   lsData.indexesInTable[GLMS_PP] = 0;
   lsData.indexesInTable[GLMS_SP] = 0;
   lsData.readStatus[GLMS_PP]     = 0;
   lsData.readStatus[GLMS_SP]     = 0;
   lsData.alkf                    = NULL;
}

void
licenseSupportDataClear()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "licenseSupportDataClear");

   if(lsData.tableIndex[GLMS_PP])
      free(lsData.tableIndex[GLMS_PP]);
   if(lsData.tableIndex[GLMS_SP])
      free(lsData.tableIndex[GLMS_SP]);
   if(lsData.alkf)
     free(lsData.alkf);
}

const char*
ls_getAreaId()
{
   return lsData.areaId;
}

static uint32_t
ls_getAreaSeqNr()
{
  return lsData.seqNr;
}

static void
ls_setAreaSeqNr(uint32_t seqNr)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "ls_setAreaSeqNr",
              seqNr);

   if(lsData.seqNr != seqNr)
   {
      lsData.seqNr = seqNr;
      lsData.pendingUpdates[GLMS_PP] = GLMS_TRUE;
   }
}

void
ls_setAreaId(const char *newAreaId)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "ls_setAreaId",
              newAreaId);

   if(strcmp(newAreaId, lsData.areaId))
   {
      strncpy(lsData.areaId,
              newAreaId,
              GLMS_AREA_ID_LEN);
      lsData.areaId[GLMS_AREA_ID_LEN - 1] = '\0';
      lsData.pendingUpdates[GLMS_PP] = GLMS_TRUE;
      lsData.sendMoUpdateInd = GLMS_TRUE;

      ls_setAreaSeqNr(0);

      featureState_deleteFeatureKeyMosAndLatentKeys(GLMS_KEY_TYPE_CENTRALIZED);
      lm_validateLicenses();

      ls_storeParameters();
      ls_sendMoUpdateLicenseSupportInd();
   }

   glms_logEvent(GLMS_LOG_LOW,
                 "Area ID set to '%s'",
                 lsData.areaId);
}

void
ls_setAlkf(const char *alkf)
{
  sp_set(GLMS_TABLE_RID_LICENSESUPPORT,
         1,
         alkf);
}

static const char*
getNextAlkfParameter(const char* alkf)
{
   const char* s = strstr(alkf, ALKF_PARAM_DELIMITER);

   if(s)
   {
      s++;  /* Step past the delimiter */
   }

   return s;
}

static int32_t
checkAlkfVersion(const char *alkf)
{
   uint32_t vmajor, vminor;

   if(sscanf(alkf, "v%"SCNu32".%"SCNu32 ALKF_PARAM_DELIMITER, &vmajor, &vminor) != 2)
   {
      return -1;
   }

   if(vmajor > 1)
   {
      return -2;
   }

   return 0;
}

static int32_t
checkAlkfAreaId(const char **alkf)
{
   const char *areaId;
   size_t areaIdLen;

   areaId = strstr(*alkf, ALKF_PARAM_DELIMITER "AREA");
   if(areaId == NULL)
   {
      return -1;
   }
   areaId++;

   areaIdLen = strcspn(areaId, ALKF_PARAM_DELIMITER);

   if(strncmp(areaId, lsData.areaId, areaIdLen) != 0)
   {
      return -2;
   }

   *alkf = getNextAlkfParameter(areaId);
   return 0;
}

static int32_t
readAlkfSeqNr(const char *alkf)
{
   uint32_t seqNr;

   if(sscanf(alkf, "%"SCNu32 ALKF_PARAM_DELIMITER, &seqNr) != 1)
   {
      return -1;
   }

   if(lsData.seqNr > seqNr)
   {
      return -2;
   }

   ls_setAreaSeqNr(seqNr);
   ls_storeParameters();

   return 0;
}

static AreaLicenseKey *
readAreaLicenseKey(const char *key)
{
   size_t attrlen;
   AreaLicenseKey *lk = NULL;

   if(!strncmp(key, "f:", 2) || !strncmp(key, ALKF_EMERGENCY_RESET_KEY_ID, strlen(ALKF_EMERGENCY_RESET_KEY_ID)))
   {
      /* Read feature key or emergencyReset key */
      lk = (AreaLicenseKey *)malloc(sizeof(AreaLicenseKey));
      memset(lk, 0, sizeof(AreaLicenseKey));

      /* License type */
      if(strncmp(key, ALKF_EMERGENCY_RESET_KEY_ID, strlen(ALKF_EMERGENCY_RESET_KEY_ID)) == 0)
      {
        lk->licenseType = GLMS_EMERGENCY_RESET_KEY;
      }
      else
      {
        lk->licenseType = GLMS_FEATURE_KEY;
        key = key + 2; /* Step key past "f:" */
      }

      /* Key id */
      attrlen = strcspn(key, ":" ALKF_PARAM_DELIMITER);
      if(attrlen >= GLMS_KEY_ID_LEN)
      {
         glms_logEvent(GLMS_LOG_LOW,
                       "Key Id in Area LKF is too long: %d.", attrlen);
         free(lk);
         return NULL;
      }
      strncpy(lk->keyId, key, attrlen);
      lk->keyId[attrlen] = '\0';

      /* Start date */
      key = key + attrlen + 1;
      attrlen = strcspn(key, ":" ALKF_PARAM_DELIMITER);
      lk->validFrom = (time_t32)strtoll(key, NULL, 10);

      /* Stop date */
      key = key + attrlen + 1;
      if(strncmp(key, ALKF_NO_STOP_DATE, strlen(ALKF_NO_STOP_DATE)) == 0)
      {
        /* Stop date is set to "-1" which means no stop date */
        lk->expiration = GLMS_NO_STOP_DATE;
      }
      else
      {
        lk->expiration = (time_t32)strtoll(key, NULL, 10);
      }
   }

   return lk;
}

static void
handleAlkfFeatureKey(const AreaLicenseKey *lk,
                     GlmsBool createFeatureStateMO)
{
   FeatureState *fs;
   FeatureKey *fk;
   LatentFeatureKey *latkey;
   const GlmsFeatureConfigurationData *featureConfData;

   fs = featureState_findByKeyId(lk->keyId);
   featureConfData = lm_getFeatureConfData(lk->keyId);

   if(fs == NULL && featureConfData != NULL && createFeatureStateMO)
   {
     fs = featureState_createFeatureState("", /*  description */
                                          lk->keyId,
                                          "", /* name */
                                          "", /* productType */
                                          GLMS_FEATURESTATE_DEACTIVATED);
   }

   if(fs != NULL)
   {
     /*
      * NB: The FeatureState MO may exist even if the license key is not
      *     included in the feature configuration list because the list is not
      *     checked at subscription time. In such case the FeatureKey MO will be
      *     created here even if the license key is not in the feature
      *     configuration list.
      */
     fk = featureState_findFeatureKeyByStartDateExpirationAndKeyType(fs,
                                                                     lk->validFrom,
                                                                     lk->expiration,
                                                                     GLMS_KEY_TYPE_CENTRALIZED);
     if(fk == NULL)
     {
       fk = featureKey_createKeyWithMoIndication(lk->keyId,
                                                 lk->validFrom,
                                                 lk->expiration,
                                                 GLMS_KEY_TYPE_CENTRALIZED);
     }
     featureKey_markAtKfParsing(fk);
   }
   else if(featureConfData != NULL)
   {
     latkey = featureKey_addLatentKey(lk->keyId,
                                      lk->validFrom,
                                      lk->expiration,
                                      GLMS_KEY_TYPE_CENTRALIZED);
     featureKey_markLatentKey(latkey);
   }
}

static void
handleAlkfEmergencyReset(const AreaLicenseKey *lk,
                         uint32_t oldSeqNr)
{
  if(ls_getAreaSeqNr() > oldSeqNr &&
     validLkDates(lk->validFrom, lk->expiration) &&
     (eu_getActivationsLeft() != 2 ))
  {
    eu_reset();
  }
}


/* ls_installAreaLicenseKeys installs an Area Key File.

   See glms_adpi.sig for specification of the alkf string.

   Return:
     GlmsInstallAreaLicenseKeyResult - See glmsDataTypes.h for list of result codes.
*/
GlmsInstallAreaLicenseKeyResult
ls_installAreaLicenseKeys(const char *alkf,
                          GlmsBool createStateMOs)
{
   static char verFail[] = "Failed to read Area Key File format version.";
   static char verMismatch[] = "Cannot read Area Key File format.";
   static char areaIdFail[] = "Failed to find area Id in Area Key File.";
   static char areaIdMismatch[] = "Area Id in Area Key File is not matching Area Id of node.";
   static char seqNrFail[] = "Failed to read sequence number in Area Key File.";
   static char seqNrMismatch[] = "Sequence number in the area license keys string was lower than the sequence number of the node.";
   static char unknownKey[] = "Unknown key in the Area Key File.";

   struct timespec currentTime;
   AreaLicenseKey *lk;

   uint32_t oldSeqNr = ls_getAreaSeqNr();

   switch(checkAlkfVersion(alkf))
   {
      case -1:
         glms_logEvent(GLMS_LOG_LOW, verMismatch);
         return GLMS_ALKF_INSTALL_NOT_READABLE;

      case -2:
         glms_logEvent(GLMS_LOG_LOW, verFail);
         return GLMS_ALKF_INSTALL_UNSUPPORTED_VERSION;

      default:
         break;
   }

   switch(checkAlkfAreaId(&alkf))
   {
      case -1:
         glms_logEvent(GLMS_LOG_LOW, areaIdFail);
         return GLMS_ALKF_INSTALL_NOT_READABLE;

      case -2:
         glms_logEvent(GLMS_LOG_LOW, areaIdMismatch);
         return GLMS_ALKF_INSTALL_LICENSE_AREA_ID_MISMATCH;

      default:
         break;
   }

   switch(readAlkfSeqNr(alkf))
   {
      case -1:
         glms_logEvent(GLMS_LOG_LOW, seqNrFail);
         return GLMS_ALKF_INSTALL_NOT_READABLE;

      case -2:
         glms_logEvent(GLMS_LOG_LOW, seqNrMismatch);
         return GLMS_ALKF_INSTALL_INVALID_SEQUENCE_NUMBER;

      default:
         break;
   }

   featureState_resetAllKfParsingMarks(GLMS_KEY_TYPE_CENTRALIZED);

   /* Read license keys */
   alkf = getNextAlkfParameter(alkf);
   while(alkf)
   {
      if((lk = readAreaLicenseKey(alkf)) == NULL)
      {
         glms_logEvent(GLMS_LOG_LOW, unknownKey);
         alkf = getNextAlkfParameter(alkf);
         continue;
      }

      switch(lk->licenseType)
      {
         case GLMS_FEATURE_KEY:
            handleAlkfFeatureKey(lk, createStateMOs);
            break;

         case GLMS_EMERGENCY_RESET_KEY:
            handleAlkfEmergencyReset(lk, oldSeqNr);
            break;

         default:
            glms_logEvent(GLMS_LOG_LOW,
                          "Unknown license type %d.",
                          lk->licenseType);
      }

      alkf = getNextAlkfParameter(alkf);
      free(lk);
   }

   /* Delete MO:s from previous Area LKF */
   clock_gettime(CLOCK_REALTIME, &currentTime);
   featureState_doMoDeletionsAtKfParsing(currentTime, GLMS_KEY_TYPE_CENTRALIZED);

   featureKey_clearUnmarkedLatentKeys(GLMS_KEY_TYPE_CENTRALIZED);

   /* Request daily validation timer if it isn't already active */
   if(!isTimerActive(GLMS_DAILY_VALIDATION_TIMER))
   {
      (void)requestTimer(GLMS_SECONDS_IN_ONE_DAY, GLMS_TRUE, GLMS_DAILY_VALIDATION_TIMER);
   }

   return GLMS_ALKF_INSTALL_SUCCESS;
}

void
ls_reinstallAreaLicenseKeys()
{
  GlmsInstallAreaLicenseKeyResult resultCode;

  if(lsData.alkf != NULL)
  {
    resultCode = ls_installAreaLicenseKeys(lsData.alkf, GLMS_FALSE /*createStateMOs*/);
    if(resultCode != GLMS_ALKF_INSTALL_SUCCESS)
    {
      glms_logEvent(GLMS_LOG_LOW, "Failed to re-install previously stored ALKF, result code: %d", resultCode);

      /* Reset the sequence number to 0 so that ENM/CLS pushes a new ALKF. */
      ls_setAreaSeqNr(0);
      ls_storeParameters();
    }

    /* The stored ALKF is not needed anymore */
    free(lsData.alkf);
    lsData.alkf = NULL;
  }
}

void
ls_storeParameters()
{
   char seqNr_uint32tStr[20];
   char *parameter;
   uint32_t paramSize;

   /* Store persistent parameters:
      lsData.seqNr + lsData.areaId (in same index)
   */
   if (lsData.pendingUpdates[GLMS_PP] == GLMS_TRUE)
   {
      paramSize = 0;
      sprintf(seqNr_uint32tStr, "%"PRIu32, lsData.seqNr);
      paramSize += strlen(seqNr_uint32tStr);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(lsData.areaId);
      paramSize += strInStr(";", lsData.areaId);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      fillParameter(parameter, seqNr_uint32tStr, GLMS_FALSE);
      fillParameter(parameter, lsData.areaId, GLMS_TRUE);

      pp_set(GLMS_TABLE_RID_LICENSESUPPORT,
             1,
             parameter);

      lsData.pendingUpdates[GLMS_PP] = GLMS_FALSE;
      free(parameter);
   }
}

void
ls_parseSp(union itc_msg *sig)
{
   char *paramStr, *parameter;

   paramStr = sig->glmsSpGetRsp.value;

   /* Read parameters:
      lsData.alkf
   */
   parameter = getNextParameter(";:", paramStr);

   free(lsData.alkf);
   lsData.alkf = malloc(strlen(parameter) + 1);
   strcpy(lsData.alkf, parameter);
}

void
ls_parsePp(union itc_msg *sig)
{
   char *paramStr, *parameter;

   paramStr = sig->glmsPpGetRsp.value;

   /* Read parameters:
      lsData.seqNr
      lsData.areaId
   */
   parameter = getNextParameter(";:", paramStr);
   sscanf(parameter, "%"SCNu32, &lsData.seqNr);

   parameter = getNextParameter(";:", NULL);
   strncpy(lsData.areaId,
           parameter,
           GLMS_AREA_ID_LEN);
   lsData.areaId[GLMS_AREA_ID_LEN - 1] = '\0';
   cleanupEscapeCharacters(lsData.areaId);
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
   return common_handleGlmsAdpiXXIndexListRsp(sig, lsData.tableIndex, lsData.indexesInTable);
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

   if(sig->glmsSpGetRsp.index != 1)
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Unexpected table index when reading LicenseSupport stored data",
                 sig->glmsSpGetRsp.index);
   }

   if (table == GLMS_SP)
   {
      ls_parseSp(sig);

      /* If additional indexes are added to the LS table then handleGlmsAdpiXXGetRsp
         needs to return based on lsData.indexesInTable and lsData.tableIndex. For now
         we only have one index per SP and PP so always return 1. */
      return 1;
   }

   if (table == GLMS_PP)
   {
      ls_parsePp(sig);

      /* See comment for SP parameter above. */
      return 1;
   }

   /* Unexpected, return -1 to continue. */
   return -1;
}

void
ls_fetchParameters()
{
   sp_requestIndexList(GLMS_TABLE_RID_LICENSESUPPORT);
   pp_requestIndexList(GLMS_TABLE_RID_LICENSESUPPORT);
}

int32_t
ls_parseStoredParameters(union itc_msg *sig)
{
   return common_parseStoredParameters(sig,
                                       lsData.readStatus,
                                       handleGlmsAdpiXXGetRsp,
                                       handleGlmsAdpiXXIndexListRsp,
                                       GLMS_TRUE, GLMS_TRUE);
}

void
ls_sendMoUpdateLicenseSupportInd()
{
   union itc_msg *sigSend;

   if (adapter_isSubscribedForMoUpdateInd() &&
       lsData.sendMoUpdateInd &&
       adapter_isAdpiActivated())
   {
      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateLicenseSupportInd),
                          GLMS_ADPI_MO_UPDATE_LICENSE_SUPPORT_IND);
      strcpy(sigSend->glmsMoUpdateLicenseSupportInd.areaId,
             ls_getAreaId());
      sendMsgToAdapter(sigSend);
   }

   lsData.sendMoUpdateInd = GLMS_FALSE;
}


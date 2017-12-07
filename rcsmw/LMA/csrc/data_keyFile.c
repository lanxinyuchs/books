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

#include <itc.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <inttypes.h>
#include <unistd.h>
#include <errno.h>

#include "com_ericsson_glms.h"

#include "glmsadpi/glmsDataTypes.h"
#include "glms_main.h"
#include "glms_timi.h"
#include "glmsUtils.h"
#include "data_keyFile.h"
#include "data_lm.h"
#include "data_featureState.h"
#include "data_capacityState.h"
#include "data_unlocks.h"
#include "persistentStorage.h"

#include "glmsadpi/glms_adpi.sig"
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
   GlmsAdpiKeyFileFaultAlarmReq             glmsKeyFileFaultAlarmReq;
   GlmsAdpiKeyFileFaultAlarmRsp             glmsKeyFileFaultAlarmRsp;
   GlmsAdpiMoUpdateKeyfileInd               glmsAdpiMoUpdateKeyfileInd;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
KeyFileData kfData;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
keyFileDataInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "keyFileDataInit");

   kfData.sequenceNumber     = 0;  /* Persistent Parameter */
   kfData.installationTime   = 0;  /* Persistent Parameter */
   kfData.locatable          = GLMS_FALSE;
   kfData.productType[0]     = '\0';

   kfData.reportProgress.actionId                = 0;
   kfData.reportProgress.actionName[0]           = '\0';
   kfData.reportProgress.additionalInfo[0]       = '\0';
   kfData.reportProgress.progressInfo[0]         = '\0';
   kfData.reportProgress.progressPercentage      = 0;
   kfData.reportProgress.result                  = GLMS_NOT_AVAILABLE;
   kfData.reportProgress.resultInfo[0]           = '\0';
   kfData.reportProgress.state                   = GLMS_FINISHED;
   kfData.reportProgress.timeActionCompleted     = 0;
   kfData.reportProgress.timeActionStarted       = 0;
   kfData.reportProgress.timeOfLastStatusUpdate  = 0;


   /* Internal data */
   kfData.pendingUpdates             = GLMS_FALSE;
   kfData.installActionOngoing       = GLMS_FALSE;
   kfData.keyFileFaultAlarmActive    = GLMS_FALSE;
   kfData.kffAlarmEventId            = 0;
   kfData.sendMoUpdateInd            = GLMS_FALSE;
   kfData.kfInstallData              = NULL;
   kfData.installedLicenses          = NULL;
   kfData.pendingInstallations       = GLMS_FALSE;
}

uint32_t
keyFile_getSequenceNumber()
{
   return kfData.sequenceNumber;
}

void
keyFile_setSequenceNumber(uint32_t newSequenceNumber)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "keyFile_setSequenceNumber", newSequenceNumber);

   if(kfData.sequenceNumber != newSequenceNumber)
   {
      kfData.sequenceNumber  = newSequenceNumber;
      kfData.pendingUpdates  = GLMS_TRUE;
      kfData.sendMoUpdateInd = GLMS_TRUE;
   }
}

char *
keyFile_getInstalledLicenses()
{
   return kfData.installedLicenses;
}

void
keyFile_freeInstalledLicenses()
{
   if (kfData.installedLicenses != NULL)
   {
      free(kfData.installedLicenses);
      kfData.installedLicenses = NULL;
   }
}

void
keyFile_addInstalledLicense(char *key)
{
   if (kfData.installedLicenses == NULL)
   {
     kfData.pendingInstallations = GLMS_TRUE;
     char *str = (key == NULL) ? "\0" : key;
     kfData.installedLicenses = malloc(strlen(str) + 1);
     strcpy(kfData.installedLicenses, str);
   }
   else if (key && !strstr(kfData.installedLicenses, key))
   {
      kfData.pendingInstallations = GLMS_TRUE;
      kfData.installedLicenses = realloc(kfData.installedLicenses, strlen(key) +
                                       strlen(kfData.installedLicenses) + 3);
      strcat(kfData.installedLicenses, ";:");
      strcat(kfData.installedLicenses, key);
   }
}

void
keyFile_storeInstalledLicenses()
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "keyFile_storeInstalledLicenses", kfData.pendingInstallations);

   if (kfData.pendingInstallations == GLMS_TRUE)
   {
      pp_set(GLMS_TABLE_RID_KEYFILE, 2, kfData.installedLicenses);

      kfData.pendingInstallations = GLMS_FALSE;
   }
}

time_t32
keyFile_getInstallationTime()
{
   return kfData.installationTime;
}

void
keyFile_setInstallationTime(time_t32 newKfInstallationTime)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "keyFile_setInstallationTime", newKfInstallationTime);

   if(kfData.installationTime != newKfInstallationTime)
   {
      kfData.installationTime = newKfInstallationTime;
      kfData.pendingUpdates = GLMS_TRUE;
      kfData.sendMoUpdateInd = GLMS_TRUE;
   }
}

GlmsBool
keyFile_getLocatable()
{
   return kfData.locatable;
}

void
keyFile_setLocatable(GlmsBool kfRead)
{
   if(kfData.locatable != kfRead)
   {
      kfData.locatable = kfRead;
      kfData.sendMoUpdateInd = GLMS_TRUE;
   }
}

char *
keyFile_getProductType()
{
   return kfData.productType;
}

void
keyFile_setProductType(char *newProductType)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "keyFile_setProductType", newProductType);

   if(strcmp(newProductType, kfData.productType) != 0)
   {
      strncpy(kfData.productType,
              newProductType,
              GLMS_PRODUCT_TYPE_LEN);
      kfData.productType[GLMS_PRODUCT_TYPE_LEN - 1] = '\0';
      kfData.sendMoUpdateInd = GLMS_TRUE;
   }
}

GlmsBool
keyFile_isRestricted(char *keyId)
{
   if (adapter_getRestrictedLicenses() == NULL)
   {
      return GLMS_FALSE;
   }

   if (!strstr(adapter_getRestrictedLicenses(), keyId))
   {
      /* 'keyId' is not a restricted license */
      return GLMS_FALSE;
   }

   if (keyFile_getInstalledLicenses() == NULL)
   {
      return GLMS_TRUE;
   }

   /* If 'keyId' is currently or previously installed the feature
    * is not considered to be restricted */
   return strstr(keyFile_getInstalledLicenses(), keyId) ?
          GLMS_FALSE : GLMS_TRUE;
}

uint32_t
keyFile_getKffAlarmEventId()
{
   return kfData.kffAlarmEventId;
}


/*
 * Install key file action functions
 *
 */
GlmsAsyncAction
*keyFile_getPointerToReportProgress()
{
   return &(kfData.reportProgress);
}

void
keyFile_installActionStart()
{
   struct timespec currentTime;
   
   clock_gettime(CLOCK_REALTIME, &currentTime);

   tracepoint(com_ericsson_glms, call_to_function,
              "keyFile_installActionStart");

   kfData.reportProgress.actionId                =
   glms_increment_uint32(kfData.reportProgress.actionId);
   
   kfData.reportProgress.progressPercentage      = 0;
   kfData.reportProgress.result                  = GLMS_NOT_AVAILABLE;
   kfData.reportProgress.state                   = GLMS_RUNNING;
   kfData.reportProgress.timeActionCompleted     = 0;
   kfData.reportProgress.timeActionStarted       = currentTime.tv_sec;
   kfData.reportProgress.timeOfLastStatusUpdate  = currentTime.tv_sec;

   strcpy(kfData.reportProgress.actionName, "installKeyFile");
   strcpy(kfData.reportProgress.additionalInfo, "Installing Key File");
   strcpy(kfData.reportProgress.progressInfo, "Downloading Key File");
   strcpy(kfData.reportProgress.resultInfo, "");

   kfData.installActionOngoing = GLMS_TRUE;
   kfData.sendMoUpdateInd      = GLMS_TRUE;

   keyFile_sendMoUpdateKeyfileInd();
}

void
keyFile_installActionLkfDownloaded()
{
   struct timespec currentTime;
   
   clock_gettime(CLOCK_REALTIME, &currentTime);

   tracepoint(com_ericsson_glms, call_to_function,
              "keyFile_installActionLkfDownloaded");

   if(!kfData.installActionOngoing)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Call to keyFile_installActionLkfDownloaded "
                 "when no install action is ongoing");
      return;
   }

   kfData.reportProgress.progressPercentage      = 50;
   kfData.reportProgress.timeOfLastStatusUpdate  = currentTime.tv_sec;

   strcpy(kfData.reportProgress.progressInfo, "Reading Key File");
   kfData.sendMoUpdateInd      = GLMS_TRUE;

   keyFile_sendMoUpdateKeyfileInd();
}

void
keyFile_installActionLkfInstalled()
{
   struct timespec currentTime;
   
   clock_gettime(CLOCK_REALTIME, &currentTime);

   tracepoint(com_ericsson_glms, call_to_function,
              "keyFile_installActionLkfInstalled");

   if(!kfData.installActionOngoing)
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Call to keyFile_installActionLkfInstalled "
                 "when no install action is ongoing");
      return;
   }

   kfData.reportProgress.progressPercentage      = 100;
   kfData.reportProgress.result                  = GLMS_SUCCESS;
   kfData.reportProgress.state                   = GLMS_FINISHED;
   kfData.reportProgress.timeActionCompleted     = currentTime.tv_sec;
   kfData.reportProgress.timeOfLastStatusUpdate  = currentTime.tv_sec;

   strcpy(kfData.reportProgress.progressInfo, "Key File Installed");
   strcpy(kfData.reportProgress.resultInfo, "Key File installed successfully");
   strcpy(kfData.reportProgress.additionalInfo, "");

   if(kfData.kfInstallData != NULL)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
   }

   kfData.sendMoUpdateInd = GLMS_TRUE;
   keyFile_sendMoUpdateKeyfileInd();
}

void
keyFile_installActionFailed(char *resultInfo)
{
   struct timespec currentTime;
   
   clock_gettime(CLOCK_REALTIME, &currentTime);

   tracepoint(com_ericsson_glms, call_to_function,
              "keyFile_installActionFailed");

   if(!kfData.installActionOngoing)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Call to keyFile_installActionFailed "
                 "when no install action is ongoing",
                 resultInfo);
      return;
   }

   kfData.reportProgress.progressPercentage      = 0;
   kfData.reportProgress.result                  = GLMS_FAILURE;
   kfData.reportProgress.state                   = GLMS_FINISHED;
   kfData.reportProgress.timeActionCompleted     = currentTime.tv_sec;
   kfData.reportProgress.timeOfLastStatusUpdate  = currentTime.tv_sec;

   strcpy(kfData.reportProgress.additionalInfo, "Install Failed");
   strcpy(kfData.reportProgress.progressInfo, "Install of Key File has stopped");
   strncpy(kfData.reportProgress.resultInfo, resultInfo, GLMS_ASYNC_ACTION_RESULTINFO_LEN);

   if(strlen(resultInfo) > GLMS_ASYNC_ACTION_RESULTINFO_LEN)
   {
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "In keyFile_installActionFailed: resultInfo longer"
                 " than the allowed GLMS_ASYNC_ACTION_RESULTINFO_LEN",
                 resultInfo);
   }

   kfData.installActionOngoing = GLMS_FALSE;
   kfData.sendMoUpdateInd      = GLMS_TRUE;

   if(kfData.kfInstallData != NULL)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
   }
}

GlmsBool
keyFile_isInstallActionRunning()
{
   return (kfData.reportProgress.state == GLMS_RUNNING ? GLMS_TRUE : GLMS_FALSE);
}

uint32_t
keyFile_getInstallActionId()
{
   return kfData.reportProgress.actionId;
}

void
keyFile_fillReportProgressStruct(GlmsAsyncAction *reportProgress)
{
   *reportProgress = kfData.reportProgress;
}

char *
keyFile_readAndValidateKeyFile(const char *kfLocation)
{
   ParseResult parseResult;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "keyFile_readAndValidateKeyFile", (char *)kfLocation);

   if(access(kfLocation, F_OK) == -1)
   {
      tracepoint(com_ericsson_glms, info_trace_w_int_arg,
                 "keyFile_readAndValidateKeyFile failed with errno",
                 errno);
      return "file not accessible";
   }

   /* If old install data is not free'd we do that now */
   if(kfData.kfInstallData != NULL)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
   }

   parseResult = parseKeyFile(kfLocation, &(kfData.kfInstallData));
   if(parseResult != LKF_SUCCESS)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
      return "failed to parse the key file";
   }

   if(strcmp(kfData.kfInstallData->fingerprint, lm_getFingerprint()) != 0)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
      return "bad fingerprint";
   }

   if(kfData.kfInstallData->sequenceNumber < keyFile_getSequenceNumber())
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
      return "bad sequence number";
   }

   if(kfData.kfInstallData->fingerprintMethod != FINGER_PRINT_METHOD_11)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
      return "bad fingerprint method";
   }

   if(strcmp(kfData.kfInstallData->swlt, "") == 0)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
      return "non-existing SWLT";
   }

   if(strcmp(kfData.kfInstallData->formatVersion, "2.0") != 0)
   {
      freeKeyFileInstallData(&(kfData.kfInstallData));
      return "bad format version";
   }

   return KEYFILE_PARSE_OK;
}


void
keyFile_resetEuAtKfActivation(uint32_t oldSeqNr)
{
   /* Reset EU if we have a valid emergency reset key */
   if(kfData.kfInstallData->sequenceNumber > oldSeqNr &&
      kfData.kfInstallData->emergencyKey.resetKeyAvailable &&
      validLkDates(kfData.kfInstallData->emergencyKey.startDate,
                   kfData.kfInstallData->emergencyKey.stopDate) &&
      (eu_getActivationsLeft() != 2 ))
   {
      eu_reset();
   }
}

void
keyFile_createAndUpdateFeatureKeys(time_t32 *earliestStartDate)
{
   FeatureKeyInstallData *fkinstall;
   FeatureKeyInstanceInstallData *fkinstance;
   FeatureState  *fs;
   FeatureKey    *fk;

   for(fkinstall = kfData.kfInstallData->featureKeyList;
       fkinstall != NULL;
       fkinstall = fkinstall->nextFeatureKey)
   {
      /* See if we already have an instance of the Key Id */
      fs = featureState_findByKeyId(fkinstall->keyId);

      if(fs == NULL)
      {
         /* If FeatureState does not exist we need to create it. */

         /* If the FeatureState MO is created as a result of installing
            a Key File we will set the name and description to empty
            strings. The attributes are updated when a client subscribes
            to the feature. */
         fs = featureState_createFeatureState("", /* description */
                                              fkinstall->keyId,
                                              "", /* name */
                                              kfData.kfInstallData->productType,
                                              GLMS_FEATURESTATE_DEACTIVATED);
      }
      else if(featureState_getMarkedForDeletion(fs) != 0)
      {
         /* The FeatureState existed but it was marked
            for deletion: clear the mark */
         featureState_setMarkedForDeletion(fs, 0);
      }
      else
      {
         /* The FeatureState already existed but set the ProductType
            in case this is the first FeatureKey that is installed
            for the Feature License. */
         featureState_setProductType(fs, kfData.kfInstallData->productType);
      }

      /* Build the new Feature Key instance list */
      for(fkinstance = fkinstall->instanceList;
          fkinstance != NULL;
          fkinstance = fkinstance->nextInstance)
      {
         fk = featureState_findFeatureKeyByStartDateExpirationAndKeyType(fs,
                                                fkinstance->startDate,
                                                fkinstance->stopDate,
                                                GLMS_KEY_TYPE_NODE);

         if(fk == NULL)
         {
            /* It is a new FeatureKey. Create the data structure and send
               an create MO indication. */
            fk = featureKey_createKeyWithMoIndication(fkinstall->keyId,
                                                      fkinstance->startDate,
                                                      fkinstance->stopDate,
                                                      GLMS_KEY_TYPE_NODE);
         }

         featureKey_markAtKfParsing(fk);

         if(fkinstance->startDate < *earliestStartDate ||
            *earliestStartDate == 0)
         {
            *earliestStartDate = fkinstance->startDate;
         }
      }

      keyFile_addInstalledLicense(fkinstall->keyId);
   }
}

void
keyFile_createAndUpdateCapacityKeys(time_t32 *earliestStartDate)
{
   CapacityKeyInstallData *ckinstall;
   CapacityKeyInstanceInstallData *ckinstance;
   CapacityState  *cs;
   CapacityKey    *ck;

   for(ckinstall = kfData.kfInstallData->capacityKeyList;
       ckinstall != NULL;
       ckinstall = ckinstall->nextCapacityKey)
   {
      /* See if we already have an instance of the Key Id */
      cs = capacityState_findByKeyId(ckinstall->keyId);

      if(cs == NULL)
      {
         /* If CapacityState does not exist we need to create it. */

         /* If the CapacityState MO is created as a result of installing
            a Key File we will set the name, description and capacityUnit
            to empty strings. The attributes are updated when a client
            subscribes to the capacity. GracePeriod will be created with
            default settings. */
         cs = capacityState_createCapacityState("", /* description */
                                                ckinstall->keyId,
                                                "", /* name */
                                                "", /* capacityUnit */
                                                kfData.kfInstallData->productType,
                                                0,
                                                0); /* isGracePeriodControlled */
      }
      else if(capacityState_getMarkedForDeletion(cs) != 0)
      {
         /* The CapacityState existed but it was marked
            for deletion: clear the mark */
         capacityState_setMarkedForDeletion(cs, 0);
      }
      else
      {
         /* The CapacityState already existed but set the ProductType
            in case this is the first CapacityKey that is installed
            for the Capacity License. */
         capacityState_setProductType(cs, kfData.kfInstallData->productType);
      }

      /* Build the new Capacity Key instance list */
      for(ckinstance = ckinstall->instanceList;
          ckinstance != NULL;
          ckinstance = ckinstance->nextInstance)
      {
         ck = capacityState_findCapacityKeyByStartDateAndExpirationAndCapacityValue(
                cs,
                ckinstance->startDate,
                ckinstance->stopDate,
                ckinstance->capacity,
                ckinstance->hwacString);

         if(ck == NULL)
         {
            /* It is a new CapacityKey. Create the data structure and send
               a create MO indication. */
            ck = capacityKey_createKeyWithMoIndication(ckinstall->keyId,
                                                       ckinstance->startDate,
                                                       ckinstance->stopDate,
                                                       ckinstance->capacity,
                                                       ckinstance->notContractuallyLimited,
                                                       ckinstance->hwacString);
         }

         capacityKey_markAtKfParsing(ck);

         if(ckinstance->startDate < *earliestStartDate ||
            *earliestStartDate == 0)
         {
            *earliestStartDate = ckinstance->startDate;
         }

         if(capacityState_getIsGpControlled(cs) == 1 &&
            capacityState_getGracePeriodExpiration(cs) != 0 &&
            (ckinstance->capacity.value >= capacityState_getGpResetCapacityValue(cs) ||
             ckinstance->capacity.value == -1))
         {
            capacityState_setGracePeriodActivationValue(cs, 0);
            capacityState_setGracePeriodExpirationAndState(cs, 0);
            capacityState_stopGpTimer(cs);
            capacityState_sendGpAlarmInd(cs, GLMS_ALARM_CEASED, GLMS_ALARM_NONE);
         }
      }

      keyFile_addInstalledLicense(ckinstall->keyId);
   }
}

void
keyFile_activateKeyFile(GlmsBool install)
{
   uint32_t oldSeqNr;
   struct timespec currentTime;
   time_t32 earliestStartDate;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "keyFile_activateKeyFile", install);

   clock_gettime(CLOCK_REALTIME, &currentTime);

   /* Key File is parsed and accepted, update our local data */
   oldSeqNr = keyFile_getSequenceNumber();
   keyFile_setSequenceNumber(kfData.kfInstallData->sequenceNumber);

   if(install)
   {
      keyFile_setInstallationTime(currentTime.tv_sec);
   }

   keyFile_storeParameters();

   keyFile_resetEuAtKfActivation(oldSeqNr);
   iu_reset();
   pu_reset();

   /* We reset the markedAtKfParsing parameter on the FeatureKey and CapacityKey
      instances. After going through all License keys in the parsed LKF we will
      check if there are any featureKey instance that still has its
      markedAtKfParsing set to false. In that case we will send a MO delete
      indication and delete the FeatureKey. */
   featureState_resetAllKfParsingMarks(GLMS_KEY_TYPE_NODE);
   capacityState_resetAllKfParsingMarks();

   earliestStartDate = 0;

   /* Step through the newly parsed Key File and compare it to the existing
      FeatureKey and CapacityKey instances. */
   keyFile_createAndUpdateFeatureKeys(&earliestStartDate);
   keyFile_createAndUpdateCapacityKeys(&earliestStartDate);

   /* If current time is earlier than the earliest start date in the parsed
      Key File then activate autonomous mode. */
   if(currentTime.tv_sec < earliestStartDate)
   {
      am_activateAutonomousMode();
   }
   else
   {
      am_ceaseAutonomousMode();
   }


   /* Look for FeatureKey's that doesn't have the markedAtKfParsing set. Delete
      those FeatureKey's data structures and send MO delete indications. If,
      when deleting a FeatureKey, its parent FeatureState has no other
      FeatureKey's or LIHI subscribers we mark it for deletion. */
   featureState_doMoDeletionsAtKfParsing(currentTime, GLMS_KEY_TYPE_NODE);

   /* Look for CapacityKey's that doesn't have the markedAtKfParsing set. Delete
      those CapacityKey's data structures and send MO delete indications. If,
      when deleting a CapacityKey, its parent CapacityState has no other
      capacityKey's or LIHI subscribers we mark it for deletion. */
   capacityState_doMoDeletionsAtKfParsing(currentTime);

   keyFile_storeInstalledLicenses();

   /* A Key file have been read so set KeyFileInformation.locatable to true */
   keyFile_setLocatable(GLMS_TRUE);
   keyFile_setProductType(kfData.kfInstallData->productType);
   freeKeyFileInstallData(&(kfData.kfInstallData));

   keyFile_sendMoUpdateKeyfileInd();
   keyFile_ceaseKeyFileFaultAlarm();

   lm_validateLicenses();
   featureState_storeParametersOfAllFeatureStatesAndKeys();
   capacityState_storeParametersOfAllCapacityStatesAndKeys();

   handleLicenseKeyCloseToExpirationAlarm();

   /* Request daily validation timer if it isn't already active */
   if(!isTimerActive(GLMS_DAILY_VALIDATION_TIMER))
   {
      (void)requestTimer(GLMS_SECONDS_IN_ONE_DAY, GLMS_TRUE, GLMS_DAILY_VALIDATION_TIMER);
   }
}


void
keyFile_activateKeyFileFaultAlarm()
{
   union itc_msg *sig;
   static uint32_t alarmRsp[] = {1, GLMS_ADPI_KEY_FILE_FAULT_ALARM_RSP};

   tracepoint(com_ericsson_glms, call_to_function,
              "keyFile_activateKeyFileFaultAlarm");

   if(kfData.keyFileFaultAlarmActive == GLMS_FALSE)
   {
      sig = itc_alloc(sizeof(GlmsAdpiKeyFileFaultAlarmReq),
                  GLMS_ADPI_KEY_FILE_FAULT_ALARM_REQ);
      sig->glmsKeyFileFaultAlarmReq.alarmState = GLMS_ALARM_ACTIVATED;
      sendMsgToAdapter(sig);

      kfData.keyFileFaultAlarmActive = GLMS_TRUE;

      glms_logEvent(GLMS_LOG_LOW,
                    "Key File Fault alarm activated.");

      sig = itc_receive(alarmRsp, ITC_NO_TMO, ITC_FROM_ALL);
      kfData.kffAlarmEventId = sig->glmsKeyFileFaultAlarmRsp.kffAlarmEventId;
      itc_free(&sig);
   }
}


void
keyFile_ceaseKeyFileFaultAlarm()
{
   union itc_msg *sig;
   static uint32_t alarmRsp[] = {1, GLMS_ADPI_KEY_FILE_FAULT_ALARM_RSP};

   tracepoint(com_ericsson_glms, call_to_function,
              "keyFile_ceaseKeyFileFaultAlarm");

   if(kfData.keyFileFaultAlarmActive == GLMS_TRUE)
   {
      sig = itc_alloc(sizeof(GlmsAdpiKeyFileFaultAlarmReq),
                  GLMS_ADPI_KEY_FILE_FAULT_ALARM_REQ);
      sig->glmsKeyFileFaultAlarmReq.alarmState = GLMS_ALARM_CEASED;
      sendMsgToAdapter(sig);

      kfData.keyFileFaultAlarmActive = GLMS_FALSE;

      glms_logEvent(GLMS_LOG_LOW,
                    "Key File Fault alarm ceased.");

      sig = itc_receive(alarmRsp, ITC_NO_TMO, ITC_FROM_ALL);
      kfData.kffAlarmEventId = sig->glmsKeyFileFaultAlarmRsp.kffAlarmEventId;
      itc_free(&sig);
   }
}


GlmsAlarmState
keyFile_getKeyFileFaultAlarmState()
{
   if(kfData.keyFileFaultAlarmActive == GLMS_FALSE)
   {
      return GLMS_ALARM_CEASED;
   }
   else
   {
      return GLMS_ALARM_ACTIVATED;
   }
}


void
keyFile_storeParameters()
{
   char *parameter;
   char  sequenceNumber_uint32tStr[20];
   char  installationTime_timetStr[20];
   uint32_t paramSize;


   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "keyFile_storeParameters", kfData.pendingUpdates);

   /* Store parameters:
      kfData.sequenceNumber
      kfData.installationTime
      kfData.productType
   */

   if(kfData.pendingUpdates == GLMS_TRUE)
   {
      paramSize = 0;
      sprintf(sequenceNumber_uint32tStr, "%"PRIu32, kfData.sequenceNumber);
      paramSize += strlen(sequenceNumber_uint32tStr);
      paramSize += 2; /* Delimiter */
      sprintf(installationTime_timetStr, "%jd", (intmax_t)kfData.installationTime);
      paramSize += strlen(installationTime_timetStr);
      paramSize += 2; /* Delimiter */
      paramSize += strlen(kfData.productType);
      paramSize += 1; /* Null */

      parameter = (char*) malloc(paramSize);
      strcpy(parameter, "");
      fillParameter(parameter, sequenceNumber_uint32tStr, GLMS_FALSE);
      fillParameter(parameter, installationTime_timetStr, GLMS_FALSE);
      fillParameter(parameter, kfData.productType, GLMS_TRUE);

      pp_set(GLMS_TABLE_RID_KEYFILE, 1, parameter);

      free(parameter);

      kfData.pendingUpdates = GLMS_FALSE;
   }
}


void
keyFile_fetchParameters()
{
   /*sp_requestIndexList(GLMS_TABLE_RID_KEYFILE, "KeyFileSoftwareData");*/
   pp_requestIndexList(GLMS_TABLE_RID_KEYFILE);
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
keyFile_parsePp(union itc_msg *sig)
{
   char *paramStr, *parameter;
   uint32_t paramCount;

   if (sig->glmsPpGetRsp.index == 1)
   {
      paramStr = sig->glmsPpGetRsp.value;
      paramCount = strInStr(";:", paramStr);

      if(paramCount < 1)
      {
         tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                    "The KeyFilePersistentData parameter string contains "
                    "less than the expected 1 parameter delimiters",
                    paramStr);
      }
      else
      {
         /* Read parameters:
            kfData.sequenceNumber
            kfData.installationTime
            kfData.productType
         */
         parameter = getNextParameter(";:", paramStr);
         sscanf(parameter, "%"SCNu32, &kfData.sequenceNumber);

         parameter = getNextParameter(";:", NULL);
         kfData.installationTime = (time_t32)strtoll(parameter, NULL, 10);

         if(paramCount > 1)
         {
            parameter = getNextParameter(";:", NULL);
            cleanupEscapeCharacters(parameter);
            strncpy(kfData.productType,
                    parameter,
                    GLMS_PRODUCT_TYPE_LEN);
            kfData.productType[GLMS_PRODUCT_TYPE_LEN - 1] = '\0';
         }
      }
   }
   else if (sig->glmsPpGetRsp.index == 2)
   {
      /* Save the entire entry, value contains all currently, and previously,
       * installed licenses */
      if (sig->glmsPpGetRsp.value[0] != '\0')
      {
         kfData.installedLicenses = malloc(strlen(sig->glmsPpGetRsp.value) + 1);
         strcpy(kfData.installedLicenses, sig->glmsPpGetRsp.value);
      }
      else
      {
         kfData.installedLicenses = NULL;
      }
   }
   else
   {
      tracepoint(com_ericsson_glms, info_trace_w_int_arg,
                 "Cannot handle table index when reading LM persistent data, ignoring index",
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
   int ret = common_prepareForParsingStoredParameters(sig, GLMS_FALSE, GLMS_TRUE);

   if ((ret == -1) || (ret == GLMS_SP))
      return 1;

   keyFile_parsePp(sig);

   return 1; /* We have read all parameters of this parameter type so return 1. */
}


int32_t
keyFile_parseStoredParameters(union itc_msg *sig)
{
   return common_parseStoredParameters(sig,
                                       kfData.readStatus,
                                       handleGlmsAdpiXXGetRsp,
                                       handleGlmsAdpiXXIndexListRsp,
                                       GLMS_TRUE,
                                       GLMS_FALSE);
}

void
keyFile_sendMoUpdateKeyfileInd()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "keyFile_sendMoUpdateKeyfileInd");

   if(adapter_isSubscribedForMoUpdateInd() &&
      kfData.sendMoUpdateInd &&
      adapter_isAdpiActivated())
   {
      union itc_msg *sigSend;

      sigSend = itc_alloc(sizeof(GlmsAdpiMoUpdateKeyfileInd),
                      GLMS_ADPI_MO_UPDATE_KEY_FILE_IND);

      sigSend->glmsAdpiMoUpdateKeyfileInd.sequenceNumber =
         keyFile_getSequenceNumber();
      sigSend->glmsAdpiMoUpdateKeyfileInd.installationTime =
         keyFile_getInstallationTime();
      sigSend->glmsAdpiMoUpdateKeyfileInd.locatable =
         keyFile_getLocatable();

      strcpy(sigSend->glmsAdpiMoUpdateKeyfileInd.productType,
             keyFile_getProductType());

      keyFile_fillReportProgressStruct(&(sigSend->glmsAdpiMoUpdateKeyfileInd.
                                         installKeyFile));
      sendMsgToAdapter(sigSend);

   }
   kfData.sendMoUpdateInd = GLMS_FALSE;
}

int32_t
keyFile_getStartupReadStatus(uint32_t elem)
{
   return kfData.readStatus[elem];
}

GlmsBool
keyFile_getPendingUpdates()
{
   return kfData.pendingUpdates;
}

GlmsBool
keyFile_getInstallActionOngoing()
{
   return kfData.installActionOngoing;
}

GlmsBool
keyFile_getSendMoUpdateInd()
{
   return kfData.sendMoUpdateInd;
}

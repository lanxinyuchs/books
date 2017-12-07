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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include <time.h>
#include <inttypes.h> /* PRI and SCN macros */
#include <itc.h>
#include <ngns_if.h>

#include "glmsadpi/glmsDataTypes.h"
#include "glms_main.h"
#include "glmsUtils.h"
#include "persistentStorage.h"
#include "data_featureKey.h"
#include "data_capacityKey.h"
#include "data_unlocks.h"
#include "data_lm.h"
#include "data_keyFile.h"
#include "data_licenseSupport.h"
#include "clients.h"
#include "kfParser.h"
#include "lihi_feature.h"
#include "lihi_capacity.h"
#include "glms_timi.h"

#include "glms_internal.sig"
#include "glmsadpi/glms_adpi.sig"
#include "lihi/licenseFeatureControlI.sig"
#include "lihi/licenseCapacityControlI.sig"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define TRACEPOINT_DEFINE
#include "com_ericsson_glms.h"

uint32_t  GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT+1] = { 2000,
                                                          6000,
                                                          18000,
                                                          30000,
                                                          50000 };

#define LFCI 0
#define LCCI 1

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t  sigNo;

   /* Internal signals */
   GlmsTimerExpiredInd         glmsTimerExpiredInd;

   /* ADPI */
   GlmsAdpiActivateReq         glmsActivateReq;
   GlmsAdpiActivateRsp         glmsActivateRsp;
   GlmsAdpiDeactivateReq       glmsDeactivateReq;
   GlmsAdpiDeactivateRsp       glmsDeactivateRsp;
   GlmsAdpiSetFingerprintReq   glmsSetFingerprintReq;
   GlmsAdpiSetFingerprintRsp   glmsSetFingerprintRsp;
   GlmsAdpiInstallKeyFileReq   glmsInstallKeyFileReq;
   GlmsAdpiInstallKeyFileRsp   glmsInstallKeyFileRsp;
   GlmsAdpiInstallKeyFileInd   glmsInstallKeyFileInd;
   GlmsAdpiInstallAreaLicenseKeysReq glmsInstallAreaLicenseKeysReq;
   GlmsAdpiInstallAreaLicenseKeysRsp glmsInstallAreaLicenseKeysRsp;
   GlmsAdpiSetFeatureStateReq  glmsSetFeatureStateReq;
   GlmsAdpiSetFeatureStateRsp  glmsSetFeatureStateRsp;
   GlmsAdpiDownloadKeyFileReq  glmsDownloadKeyFileReq;
   GlmsAdpiDownloadKeyFileRsp  glmsDownloadKeyFileRsp;
   GlmsAdpiPkiVerificationReq  glmsPkiVerificationReq;
   GlmsAdpiPkiVerificationRsp  glmsPkiVerificationRsp;
   GlmsAdpiHeartbeatReq        glmsAdpiHeartbeatReq;
   GlmsAdpiHeartbeatRsp        glmsAdpiHeartbeatRsp;
   GlmsAdpiStoreKeyFileReq     glmsStoreKeyFileReq;
   GlmsAdpiStoreKeyFileRsp     glmsStoreKeyFileRsp;
   GlmsAdpiGetKeyFileLocationReq           glmsGetKeyFileLocationReq;
   GlmsAdpiGetKeyFileLocationRsp           glmsGetKeyFileLocationRsp;
   GlmsAdpiCreateFeatureStateMoRsp         glmsCreateFeatureStateMoRsp;
   GlmsAdpiDeleteFeatureStateMoRsp         glmsDeleteFeatureStateMoRsp;
   GlmsAdpiCreateFeatureKeyMoRsp           glmsCreateFeatureKeyMoRsp;
   GlmsAdpiDeleteFeatureKeyMoRsp           glmsDeleteFeatureKeyMoRsp;
   GlmsAdpiCreateCapacityStateMoRsp        glmsCreateCapacityStateMoRsp;
   GlmsAdpiDeleteCapacityStateMoRsp        glmsDeleteCapacityStateMoRsp;
   GlmsAdpiCreateCapacityKeyMoRsp          glmsCreateCapacityKeyMoRsp;
   GlmsAdpiDeleteCapacityKeyMoRsp          glmsDeleteCapacityKeyMoRsp;
   GlmsAdpiRefreshLicenseInventoryReq      glmsRefreshLicenseInventoryReq;
   GlmsAdpiRefreshLicenseInventoryRsp      glmsRefreshLicenseInventoryRsp;
   GlmsAdpiReadMoKeyFileInformationReq     glmsReadMoKeyFileInformationReq;
   GlmsAdpiReadMoKeyFileInformationRsp     glmsReadMoKeyFileInformationRsp;
   GlmsAdpiReadMoLmReq                     glmsReadMoLmReq;
   GlmsAdpiReadMoLmRsp                     glmsReadMoLmRsp;
   GlmsAdpiReadAmMoReq                     glmsReadAmMoReq;
   GlmsAdpiReadAmMoRsp                     glmsReadAmMoRsp;
   GlmsAdpiGetFeatureKeyMoListRsp          glmsGetFeatureKeyMoListRsp;
   GlmsAdpiGetCapacityKeyMoListRsp         glmsGetCapacityKeyMoListRsp;
   GlmsAdpiReadMoFeatureKeyReq             glmsReadMoFeatureKeyReq;
   GlmsAdpiReadMoFeatureKeyRsp             glmsReadMoFeatureKeyRsp;
   GlmsAdpiGetFeatureStateMoListRsp        glmsGetFeatureStateMoListRsp;
   GlmsAdpiReadMoFeatureStateReq           glmsReadMoFeatureStateReq;
   GlmsAdpiReadMoFeatureStateRsp           glmsReadMoFeatureStateRsp;
   GlmsAdpiReadMoCapacityKeyReq            glmsReadMoCapacityKeyReq;
   GlmsAdpiReadMoCapacityKeyRsp            glmsReadMoCapacityKeyRsp;
   GlmsAdpiGetCapacityStateMoListRsp       glmsGetCapacityStateMoListRsp;
   GlmsAdpiReadMoCapacityStateReq          glmsReadMoCapacityStateReq;
   GlmsAdpiReadMoCapacityStateRsp          glmsReadMoCapacityStateRsp;
   GlmsAdpiReadMoGracePeriodReq            glmsReadMoGracePeriodReq;
   GlmsAdpiReadMoGracePeriodRsp            glmsReadMoGracePeriodRsp;  
   GlmsAdpiReadMoLicenseSupportReq         glmsReadMoLicenseSupportReq;
   GlmsAdpiReadMoLicenseSupportRsp         glmsReadMoLicenseSupportRsp;
   GlmsAdpiReadMoInstallKeyFileReportProgressReq
   glmsReadMoInstallKeyFileReportProgressReq;
   GlmsAdpiReadMoInstallKeyFileReportProgressRsp
   glmsReadMoInstallKeyFileReportProgressRsp;
   GlmsAdpiSubscribeMoUpdatesRsp           glmsAdpiSubscribeMoUpdatesRsp;
   GlmsAdpiPersistentParameterIndexListRsp glmsPpIndexListRsp;
   GlmsAdpiPersistentParameterGetRsp       glmsPpGetRsp;
   GlmsAdpiPersistentParameterSetRsp       glmsPpSetRsp;
   GlmsAdpiPersistentParameterDeleteIndexRsp glmsPpDeleteIndexRsp ;
   GlmsAdpiSoftwareParameterIndexListRsp   glmsSpIndexListRsp;
   GlmsAdpiSoftwareParameterGetRsp         glmsSpGetRsp;
   GlmsAdpiSoftwareParameterSetRsp         glmsSpSetRsp;
   GlmsAdpiSoftwareParameterDeleteIndexRsp glmsSpDeleteIndexRsp ;
   GlmsAdpiDumpGlmsStateDataRsp            glmsDumpGlmsStateDataRsp;
   GlmsAdpiUpdateGracePeriodAttributesReq  glmsUpdateGracePeriodAttributesReq;
   GlmsAdpiUpdateGracePeriodAttributesRsp  glmsUpdateGracePeriodAttributesRsp;
   GlmsAdpiReadEuMoReq                     glmsAdpiReadEuMoReq;
   GlmsAdpiReadEuMoRsp                     glmsAdpiReadEuMoRsp;
   GlmsAdpiActivateEuReq                   glmsAdpiActivateEuReq;
   GlmsAdpiActivateEuRsp                   glmsAdpiActivateEuRsp;
   GlmsAdpiReadIuMoReq                     glmsAdpiReadIuMoReq;
   GlmsAdpiReadIuMoRsp                     glmsAdpiReadIuMoRsp;
   GlmsAdpiActivateIuReq                   glmsAdpiActivateIuReq;
   GlmsAdpiActivateIuRsp                   glmsAdpiActivateIuRsp;
   GlmsAdpiActivatePuReq                   glmsAdpiActivatePuReq;
   GlmsAdpiActivatePuRsp                   glmsAdpiActivatePuRsp;
   GlmsAdpiDeactivatePuReq                 glmsAdpiDeactivatePuReq;
   GlmsAdpiDeactivatePuRsp                 glmsAdpiDeactivatePuRsp;
   GlmsAdpiLicenseKeyExpirationAlarmInd    glmsLicenseKeyExpirationAlarmInd;
   GlmsAdpiFeatureConfigurationListReq     glmsAdpiFeatureConfigurationListReq;
   GlmsAdpiFeatureConfigurationListRsp     glmsAdpiFeatureConfigurationListRsp;
   GlmsAdpiUpdateAreaIdReq                 glmsUpdateAreaIdReq;
   GlmsAdpiUpdateAreaIdRsp                 glmsUpdateAreaIdRsp;
   GlmsAdpiStateMoAuditReq                 glmsAdpiStateMoAuditReq;
   GlmsAdpiStateMoAuditRsp                 glmsAdpiStateMoAuditRsp;

   #include "lihi/licenseFeatureControlIUnionContent.h"
   #include "lihi/licenseCapacityControlIUnionContent.h"
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

AdapterData adapterData;
GlmsBool firstCallToFunction_handleReadingStoredParameters;
uint32_t interfacePublished[2];


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

static void
handleGlmsAdpiActivateReq(union itc_msg *sigRec);

static void
handleGlmsAdpiDeactivateReq(void);

static void
handleGlmsAdpiSetFingerprintReq(union itc_msg *sigRec);

static void
handleGlmsAdpiInstallKeyFileReq(union itc_msg *sigRec);

static void
handleGlmsAdpiInstallAreaLicenseKeysReq(union itc_msg *sigRec);

static void
handleGlmsAdpiSetFeatureStateReq(union itc_msg *sigRec);

static void
handleGlmsAdpiUpdateGracePeriodAttributesReq(union itc_msg *sigRec);

static void
handleGlmsAdpiReadMoKeyFileInformationReq(union itc_msg *sigRec);

static void
handleGlmsTimerExpiredInd(union itc_msg *sigRec);

static void
publishInterfaceInNs(char *ifName);

static void
unpublishInterfaceFromNs(char *ifName);

static void
rejectKeyFileInstallation(union itc_msg *sigRec, char *errorstr);

static void
sendInstallAreaLicenseKeysRsp(union itc_msg *sigRec,
                              GlmsInstallAreaLicenseKeyResult resultCode);

static void
adapter_sendDeactivateRsp(void);

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
adapter_init()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "adapter_init");

   adapterData.adapterMid = 0;
   adapterData.monitorId  = 0;
   adapterData.adpiActivationState        = ADPI_DEACTIVATED;
   adapterData.isSubscribedForMoUpdateInd = GLMS_FALSE;
   adapterData.isLicenseRefreshRunning    = GLMS_FALSE;
   adapterData.logLevel                   = GLMS_LOG_DISABLED;
   adapterData.timeScaling                = 1; /* Default use no time scaling */
   adapterData.warningPeriod              = GLMS_WARNING_PERIOD;
   adapterData.restrictedLicenses         = NULL;

#ifdef GLMS_ALLOW_TIME_SCALING
   if(getenv("GLMS_TIME_SCALE") != NULL)
   { 
      adapterData.timeScaling = (time_t32)strtoull(getenv("GLMS_TIME_SCALE"), NULL, 10);
      if (adapterData.timeScaling > GLMS_SECONDS_IN_ONE_DAY)
      {
         adapterData.timeScaling = GLMS_SECONDS_IN_ONE_DAY;
      }
   }
   else 
   {
      adapterData.timeScaling = 1;
   }

   adapterData.warningPeriod = adapterData.warningPeriod / adapterData.timeScaling;
#endif
}

time_t32
glms_getTimeScaling(void)
{
   return adapterData.timeScaling;
}

time_t32
glms_getWarningPeriod(void)
{
   return adapterData.warningPeriod;
}

void
adapter_setMid(itc_mbox_id_t newMid)
{
   tracepoint(com_ericsson_glms, call_to_function,
              "adapter_setMid");

   adapterData.adapterMid = newMid;

   if (newMid != 0)
   {
      adapterData.monitorId = itc_monitor(newMid, NULL);
   }
}

void
adapter_unmonitor()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "adapter_unmonitor");

   if (adapterData.monitorId != 0)
   {
      itc_unmonitor(adapterData.monitorId);
      adapterData.monitorId = 0;
   }
}

itc_mbox_id_t
adapter_getMid(void)
{
   return adapterData.adapterMid;
}

void
adapter_setActivationState(AdpiActivationState adpiState)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "adapter_setActivationState", adpiState);

   adapterData.adpiActivationState = adpiState;
}

AdpiActivationState
adapter_getActivationState(void)
{
   return adapterData.adpiActivationState;
}

void
adapter_subscribeForMoUpdateInd(void)
{
   adapterData.isSubscribedForMoUpdateInd = GLMS_TRUE;
}

GlmsBool
adapter_isSubscribedForMoUpdateInd(void)
{
   return adapterData.isSubscribedForMoUpdateInd;
}

void
adapter_setLogLevel(GlmsLogLevel logLevel)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "adapter_setLogLevel", logLevel);

   adapterData.logLevel = logLevel;
}

char *
adapter_getRestrictedLicenses()
{
   return adapterData.restrictedLicenses;
}

void
adapter_setRestrictedLicenses(char *restrictedLicenses)
{
   adapterData.restrictedLicenses = malloc(strlen(restrictedLicenses) + 1);
   strcpy(adapterData.restrictedLicenses, restrictedLicenses);
}

void
adapter_freeRestrictedLicenses()
{
   if (adapterData.restrictedLicenses != NULL)
   {
      free(adapterData.restrictedLicenses);
      adapterData.restrictedLicenses = NULL;
   }
}

GlmsLogLevel
adapter_getLogLevel(void)
{
   return adapterData.logLevel;
}

GlmsBool
adapter_isAdpiActivated(void)
{
   return (adapterData.adpiActivationState == ADPI_ACTIVATED) ? GLMS_TRUE : GLMS_FALSE;
}

GlmsBool
adapter_isLicenseRefreshRunning(void)
{
   return adapterData.isLicenseRefreshRunning;
}

void
adapter_startLicenseRefresh(void)
{
   tracepoint(com_ericsson_glms, call_to_function,
              "adapter_startLicenseRefresh");

   glms_logEvent(GLMS_LOG_LOW,
                 "Starting refreshLicenseInventory action");

   adapterData.isLicenseRefreshRunning = GLMS_TRUE;
}

void
adapter_sendRspAndEndLicenseRefresh(GlmsResult result)
{
   union itc_msg *sig;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "adapter_sendRspAndEndLicenseRefresh", result);

   adapterData.isLicenseRefreshRunning = GLMS_FALSE;

   sig = itc_alloc(sizeof(GlmsAdpiRefreshLicenseInventoryRsp),
               GLMS_ADPI_REFRESH_LICENSE_INVENTORY_RSP);

   if (isSignalResultOk(result))
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "refreshLicenseInventory action finished with result OK");

      strncpy(sig->glmsRefreshLicenseInventoryRsp.resultInfo,
              "",
              GLMS_RESULT_INFO_LEN);
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "refreshLicenseInventory action finished with result NOK");

      strncpy(sig->glmsRefreshLicenseInventoryRsp.resultInfo,
              "refreshLicenseInventory action failed, possibly"
              " LKF could not be read or verified",
              GLMS_RESULT_INFO_LEN);
   }

   sig->glmsRefreshLicenseInventoryRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sig->glmsRefreshLicenseInventoryRsp.result = result;
   sendMsgToAdapter(sig);
}

void
sendMsgToAdapter(union itc_msg *sig)
{
   if(adapter_getMid())
   {
      itc_send(&sig, adapter_getMid(), ITC_MY_MBOX);
   }
   else
   {
      itc_free(&sig);
   }
}

/* deactivateGlms will clear all subscriptions, disconnect all clients,
   clear all data and clear all timers. It will unmonitor all clients
   but it will not unmonitor the adapter layer. */
static void
deactivateGlms()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "deactivateGlms");

   glms_logEvent(GLMS_LOG_LOW,
                 "Deactivating GLMS");

   deleteAllTimers();
   terminateTimerThread();

   unpublishInterfaceFromNs(LICENSE_FEATURE_CONTROL_I_SERVICE_NAME);
   unpublishInterfaceFromNs(LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME);
   adapter_freeRestrictedLicenses();

   /* No data needs to be reset for LM Data */
   keyFile_freeInstalledLicenses();
   disconnectAndDeleteAllLfciClients("GLMS deactivated");
   featureStateDataClear(); /* Clears all FeatureState and FeatureKey data */
   featureKeyDataClear();
   disconnectAndDeleteAllLcciClients("GLMS deactivated");
   capacityStateDataClear();
   capacityKeyDataClear();
   lihiClientClear();
   licenseSupportDataClear();

   adapter_sendDeactivateRsp();
   adapter_setMid(0);
   adapter_setActivationState(ADPI_DEACTIVATED);
}


static void
publishInterfaceInNs(char *ifName)
{
   int result;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "publishInterfaceInNs", ifName);

   result = ns_publish(ifName, NGNS_NO_DOMAIN, itc_current_mbox(), "");

   if (result == NGNS_OK)
   {
      glms_logEvent(GLMS_LOG_LOW, "Publishing interface %s to name server",
                    ifName);
      tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                 "Published interface", ifName);
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW, "Failed to publish interface %s to name server (%d)",
                    ifName,
                    result);
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Failed to publish interface", result);
   }

   if(strcmp(ifName, LICENSE_FEATURE_CONTROL_I_SERVICE_NAME) == 0)
   {
      interfacePublished[LFCI] = 1;
   }
   else if(strcmp(ifName, LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME) == 0)
   {
      interfacePublished[LCCI] = 1;
   }
}


static void
unpublishInterfaceFromNs(char *ifName)
{
   int result;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "unpublishInterfaceFromNs", ifName);

   result = ns_unpublish(ifName, NGNS_NO_DOMAIN, itc_current_mbox(), "");
   
   if (result == NGNS_OK)
   {
      glms_logEvent(GLMS_LOG_LOW, "Unpublished interface %s from name server",
                    ifName);
      tracepoint(com_ericsson_glms, debug_trace_w_str_arg,
                 "Unpublished interface", ifName);
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW, "Failed to unpublish interface %s from name server (%d)",
                    ifName,
                    result);
      tracepoint(com_ericsson_glms, error_trace_w_str_arg,
                 "Failed to unpublish interface", ifName);
   }

   if(strcmp(ifName, LICENSE_FEATURE_CONTROL_I_SERVICE_NAME) == 0)
   {
      interfacePublished[LFCI] = 0;
   }
   else if(strcmp(ifName, LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME) == 0)
   {
      interfacePublished[LCCI] = 0;
   }
}

uint32_t
isLihiPublished(void)
{
   if(interfacePublished[LFCI] == 1 &&
      interfacePublished[LCCI] == 1)
   {
      return 1;
   }
   return 0;
}

void
adapter_sendActivateRsp(GlmsBool pvOk, GlmsBool isDeactivated, itc_mbox_id_t sendTo)
{
   union itc_msg *activateRsp = itc_alloc(sizeof(GlmsAdpiActivateRsp),
                                          GLMS_ADPI_ACTIVATE_RSP);
   activateRsp->glmsActivateRsp.highestSupportedPv = 1;

   if (!isDeactivated)
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "GLMS activation rejected as it is already active");
      glms_logEvent(GLMS_LOG_LOW,
                    "GLMS activation rejected as it is already active");

      activateRsp->glmsActivateRsp.result = GLMS_NOK;
      strncpy(activateRsp->glmsActivateRsp.resultInfo,
              "GLMS is already activated",
              GLMS_RESULT_INFO_LEN);
      activateRsp->glmsActivateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   }
   else if (!pvOk)
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "GLMS activation rejected as the requested PV"
                 "is not supported");
      glms_logEvent(GLMS_LOG_LOW,
                    "GLMS activation rejected as the requested PV"
                    "is not supported");

      activateRsp->glmsActivateRsp.result = GLMS_NOK;
      strncpy(activateRsp->glmsActivateRsp.resultInfo,
              "GLMS does not support the requested protocol version",
              GLMS_RESULT_INFO_LEN);
      activateRsp->glmsActivateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   }
   else
   {
      activateRsp->glmsActivateRsp.result = GLMS_OK;
      strncpy(activateRsp->glmsActivateRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
      activateRsp->glmsActivateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   }

   itc_send(&activateRsp, sendTo, ITC_MY_MBOX);
}

static void
adapter_sendDeactivateRsp(void)
{
   union itc_msg *sig = itc_alloc(sizeof(GlmsAdpiDeactivateRsp),
                                  GLMS_ADPI_DEACTIVATE_RSP);
   strncpy(sig->glmsDeactivateRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sig->glmsDeactivateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sig->glmsDeactivateRsp.result = GLMS_OK;
   sendMsgToAdapter(sig);
}

static void
handleGlmsAdpiActivateReq(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, call_to_function,
              "handleGlmsAdpiActivateReq");

   if (adapter_getActivationState() != ADPI_DEACTIVATED)
   {
      adapter_sendActivateRsp(GLMS_FALSE, GLMS_FALSE, itc_sender(sigRec));
      return;
   }

   if (sigRec->glmsActivateReq.protocolVersion != 1)
   {
      adapter_sendActivateRsp(GLMS_FALSE, GLMS_TRUE, itc_sender(sigRec));
      return;
   }

   /* Initiate Data */
   initTimers();
   featureStateDataInit();
   featureKeyDataInit();
   capacityStateDataInit();
   capacityKeyDataInit();
   euDataInit();
   iuDataInit();
   puDataInit();
   amDataInit();
   lmDataInit();
   keyFileDataInit();
   lihiClientInit();
   licenseSupportDataInit();
   firstCallToFunction_handleReadingStoredParameters = GLMS_TRUE;

   adapter_setMid(itc_sender(sigRec)); /* setMid will also start to monitor the adapter */
   adapter_setLogLevel(sigRec->glmsActivateReq.logLevel);
   adapter_setRestrictedLicenses(sigRec->glmsActivateReq.restrictedLicenses);

   lm_fetchParameters();
   keyFile_fetchParameters();
   featureState_fetchParameters();
   /* featureKey_fetchParameters is called when FeatureStates have been fetched */
   capacityState_fetchParameters();
   /* capacityKey_fetchParameters is called when CapacityStates have been fetched */
   eu_fetchParameters();
   iu_fetchParameters();
   pu_fetchParameters();
   am_fetchParameters();
   ls_fetchParameters();

   adapter_sendActivateRsp(GLMS_TRUE, GLMS_TRUE, itc_sender(sigRec));

   adapter_setActivationState(ADPI_INITIATING);

   tracepoint(com_ericsson_glms, debug_trace, "GLMS activated");
   glms_logEvent(GLMS_LOG_LOW, "GLMS activated with PV %d",
                 sigRec->glmsActivateReq.protocolVersion);
}

static void
handleGlmsAdpiDeactivateReq()
{
   adapter_unmonitor();
   deactivateGlms();
}

static void
handleGlmsAdpiHeartbeatReq(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, debug_trace,
              "Sending Heartbeat response");

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiHeartbeatRsp),
                                      GLMS_ADPI_HEARTBEAT_RSP);
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiRefreshLicenseInventoryReq(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, call_to_function,
              "handleGlmsAdpiRefreshLicenseInventoryReq");

   adapter_startLicenseRefresh();

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiGetKeyFileLocationReq),
                   GLMS_ADPI_GET_KEY_FILE_LOCATION_REQ);
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiUpdateAreaIdReq(union itc_msg *sigRec)
{
   union itc_msg *sig;

   ls_setAreaId(sigRec->glmsUpdateAreaIdReq.areaId);

   sig = itc_alloc(sizeof(GlmsAdpiUpdateAreaIdRsp),
                   GLMS_ADPI_UPDATE_AREA_ID_RSP);
   sig->glmsUpdateAreaIdRsp.result = GLMS_OK;
   itc_send(&sig, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiStateMoAuditReq(union itc_msg *sigRec)
{
   union itc_msg *sig;
   FeatureState *fs, *nextFs;
   FeatureKey *fk;


   /* Send response to the adapter */
   sig = itc_alloc(sizeof(GlmsAdpiStateMoAuditRsp),
                   GLMS_ADPI_STATE_MO_AUDIT_RSP);
   sig->glmsAdpiStateMoAuditRsp.result = GLMS_OK;
   itc_send(&sig, itc_sender(sigRec), ITC_MY_MBOX);


   fs = featureState_getFirst();
   while (fs != NULL)
   {
     if(featureState_getFirstSub(fs) == NULL &&
        featureState_getFeatureState(fs) == GLMS_FEATURESTATE_DEACTIVATED &&
        featureState_getNrOfFeatureKeysOfType(fs, GLMS_KEY_TYPE_NODE) == 0)
     {
       /* There are no subscribers, the feature state is DEACTIVATED
        * and no key is local   --> delete the MOs ...
        */

       fk = featureState_getFirstFeatureKey(fs);
       while(fk != NULL)
       {
         /* ... but save the delete keys in case a subscription arrives later */
         featureKey_addLatentKey(featureState_getKeyId(fs),
                                 fk->validFrom,
                                 fk->expiration,
                                 fk->keyType);

         featureKey_deleteKeyAndMo(fk);
         fk = featureState_getFirstFeatureKey(fs);
       }

       nextFs = featureState_getNext(fs);
       featureState_deleteFeatureStateAndMo(fs);
       fs = nextFs;
     }
     else
     {
       fs = featureState_getNext(fs);
     }
   }
}

/* handleLicenseKeyCloseToExpirationAlarm will activate or cease the
   LicenseKeyCloseToExpiration alarm depending on the License Key states
   of FeatureState's and CapacityState's. Only if a change has happened
   an alarm indication will be sent.
*/
void
handleLicenseKeyCloseToExpirationAlarm(void)
{
   union itc_msg *sig;
   uint32_t infoLen, fsExpirations, fsPending, csExpirations, csPending;
   size_t fsWritten, csWritten;

   fsPending = featureState_getNrOfExpiringKeys(&fsExpirations);
   csPending = capacityState_getNrOfExpiringKeys(&csExpirations);

   if(fsPending || csPending)
   {
      infoLen = fsExpirations * (GLMS_KEY_NAME_LEN + GLMS_KEY_ID_LEN + 5);
      infoLen += csExpirations * (GLMS_KEY_NAME_LEN + GLMS_KEY_ID_LEN + 5);
      sig = itc_alloc(sizeof(GlmsAdpiLicenseKeyExpirationAlarmInd) + infoLen,
                      GLMS_ADPI_LICENSE_KEY_EXPIRATION_ALARM_IND);

      fsWritten = featureState_fillExpirationAlarmAddInfo(
                       sig->glmsLicenseKeyExpirationAlarmInd.additionalInfo);
      csWritten = capacityState_fillExpirationAlarmAddInfo(
                       &(sig->glmsLicenseKeyExpirationAlarmInd.additionalInfo[fsWritten]));

      if(fsWritten == 0 && csWritten == 0)
      {
         sig->glmsLicenseKeyExpirationAlarmInd.alarmState  = GLMS_ALARM_CEASED;
      }
      else
      {
         sig->glmsLicenseKeyExpirationAlarmInd.alarmState  = GLMS_ALARM_ACTIVATED;

         /* Remove last ',' on addinfo */
         sig->glmsLicenseKeyExpirationAlarmInd.
            additionalInfo[fsWritten + csWritten - 2] = '\0';
      }

      glms_logEvent(GLMS_LOG_LOW, "License Key Close to Expiration Alarm: %s",
                    (fsWritten + csWritten) ? "Activated" : "Ceased");

      sendMsgToAdapter(sig);
   }
}

static void
handleGlmsAdpiSubscribeMoUpdatesReq(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, call_to_function,
              "handleGlmsAdpiSubscribeMoUpdatesReq");

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiSubscribeMoUpdatesRsp),
                                      GLMS_ADPI_SUBSCRIBE_MO_UPDATES_RSP);

   if (adapter_getActivationState() == ADPI_DEACTIVATED)
   {
      strncpy(sigSend->glmsAdpiSubscribeMoUpdatesRsp.resultInfo,
              "GLMS is not activated", GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiSubscribeMoUpdatesRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsAdpiSubscribeMoUpdatesRsp.result = GLMS_NOK;
      itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
      return;
   }

   adapter_subscribeForMoUpdateInd();

   if (adapter_getActivationState() == ADPI_ACTIVATED)
   {
      strncpy(sigSend->glmsAdpiSubscribeMoUpdatesRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiSubscribeMoUpdatesRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsAdpiSubscribeMoUpdatesRsp.result = GLMS_OK;
      itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
   }
}

static void
handleGlmsAdpiReadEuMoReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadEuMoRsp),
                                      GLMS_ADPI_READ_EU_MO_RSP);

   sigSend->glmsAdpiReadEuMoRsp.expiration = eu_getExpiration();
   sigSend->glmsAdpiReadEuMoRsp.activationsLeft = eu_getActivationsLeft();
   sigSend->glmsAdpiReadEuMoRsp.activationState = eu_getActivationState();
   strncpy(sigSend->glmsAdpiReadEuMoRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sigSend->glmsAdpiReadEuMoRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsAdpiReadEuMoRsp.result = GLMS_OK;
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiActivateEuReq(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, call_to_function,
              "handleGlmsAdpiActivateEmergencyUnlockReq");

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiActivateEuRsp),
                                      GLMS_ADPI_ACTIVATE_EU_RSP);

   if (eu_activateEu())
   {
      glms_logEvent(GLMS_LOG_LOW, "Emergency unlock activated.");

      sigSend->glmsAdpiActivateEuRsp.result = GLMS_OK;
      strncpy(sigSend->glmsAdpiActivateEuRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "Failed to activate emergeny unlock. EU.activationsState = %d,"
                    " EU.activationsLeft = %d.",
                    eu_getActivationState(),
                    eu_getActivationsLeft());

      sigSend->glmsAdpiActivateEuRsp.result = GLMS_NOK;
      strncpy(sigSend->glmsAdpiActivateEuRsp.resultInfo,
              "EU was not activated. Possibly it has been"
              " activated two times already.",
              GLMS_RESULT_INFO_LEN);
   }

   sigSend->glmsAdpiActivateEuRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

   /* Need to set licensed State to enabled for all Feature Keys and Capacity Keys
      if EU is active and send Mo update Ind accordingly */
   lm_validateLicenses();
}

static void
handleGlmsAdpiReadIuMoReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend;
   sigSend = itc_alloc(sizeof(GlmsAdpiReadIuMoRsp),
               GLMS_ADPI_READ_IU_MO_RSP);

   sigSend->glmsAdpiReadIuMoRsp.expiration = iu_getExpiration();
   sigSend->glmsAdpiReadIuMoRsp.activationsLeft = iu_getActivationsLeft();
   sigSend->glmsAdpiReadIuMoRsp.activationState = iu_getActivationState();
   strncpy(sigSend->glmsAdpiReadIuMoRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sigSend->glmsAdpiReadIuMoRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsAdpiReadIuMoRsp.result = GLMS_OK;
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiActivateIuReq(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, call_to_function,
              "handleGlmsAdpiActivateIntegrationUnlockReq");

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiActivateIuRsp),
                                      GLMS_ADPI_ACTIVATE_IU_RSP);
   sigSend->glmsAdpiActivateIuRsp.result = GLMS_NOK;

   if (iu_activateIu())
   {
      glms_logEvent(GLMS_LOG_LOW, "Integration unlock activated.");
      
      strncpy(sigSend->glmsAdpiActivateIuRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiActivateIuRsp.result = GLMS_OK;
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "Failed to activate integration unlock. IU.activationsState = %d,"
                    " IU.activationsLeft = %d, KFI.sequenceNumber = %d.",
                    iu_getActivationState(),
                    iu_getActivationsLeft(),
                    keyFile_getSequenceNumber());

      strncpy(sigSend->glmsAdpiActivateIuRsp.resultInfo,
              "Integration Unlock was not activated. Check that no LKF"
              " is installed, and that IU is not already activated.",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiActivateIuRsp.result = GLMS_NOK;
   }

   sigSend->glmsAdpiActivateIuRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

   /* Need to set licensed State to enabled for all Feature Keys and Capacity Keys
      if IU is active and send Mo update Ind accordingly */

   lm_validateLicenses();
}

static void
handleGlmsAdpiActivatePuReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend;
   sigSend = itc_alloc(sizeof(GlmsAdpiActivatePuRsp),
               GLMS_ADPI_ACTIVATE_PU_RSP);
   sigSend->glmsAdpiActivatePuRsp.result = GLMS_NOK;

   if (pu_activatePu())
   {
      glms_logEvent(GLMS_LOG_LOW, "Production unlock activated.");

      strncpy(sigSend->glmsAdpiActivatePuRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiActivatePuRsp.result = GLMS_OK;
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "Failed to activate production unlock. PU.activationsState = %d,"
                    " PU.activationsLeft = %d, KFI.sequenceNumber = %d.",
                    pu_getActivationState(),
                    pu_getActivationsLeft(),
                    keyFile_getSequenceNumber());

      strncpy(sigSend->glmsAdpiActivatePuRsp.resultInfo,
              "Production Unlock was not activated. Check that no LKF"
              " is installed, and that PU is not already activated.",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiActivatePuRsp.result = GLMS_NOK;
   }

   sigSend->glmsAdpiActivatePuRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsAdpiActivatePuRsp.expiration = pu_getExpiration();
   sigSend->glmsAdpiActivatePuRsp.activationsLeft = pu_getActivationsLeft();
   sigSend->glmsAdpiActivatePuRsp.activationState = pu_getActivationState();
   sigSend->glmsAdpiActivatePuRsp.puDeactivated = pu_getPuDeactivated();
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

   /* Need to set licensed State to enabled for all Feature Keys and Capacity Keys
      if PU is active and send Mo update Ind accordingly */
   lm_validateLicenses();
}

static void
handleGlmsAdpiDeactivatePuReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiDeactivatePuRsp),
                                      GLMS_ADPI_DEACTIVATE_PU_RSP);
   sigSend->glmsAdpiDeactivatePuRsp.result = GLMS_NOK;

   if (pu_deactivatePu())
   {
      glms_logEvent(GLMS_LOG_LOW, "Production unlock deactivated.");

      strncpy(sigSend->glmsAdpiDeactivatePuRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiDeactivatePuRsp.result = GLMS_OK;
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "Failed to deactivate production unlock. PU.activationsState = %d,"
                    " PU.activationsLeft = %d, KFI.sequenceNumber = %d.",
                    pu_getActivationState(),
                    pu_getActivationsLeft(),
                    keyFile_getSequenceNumber());

      strncpy(sigSend->glmsAdpiDeactivatePuRsp.resultInfo,
              "Production Unlock was not deactivated. Check that it is active.",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsAdpiDeactivatePuRsp.result = GLMS_NOK;
   }

   sigSend->glmsAdpiDeactivatePuRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsAdpiDeactivatePuRsp.expiration = pu_getExpiration();
   sigSend->glmsAdpiDeactivatePuRsp.activationsLeft = pu_getActivationsLeft();
   sigSend->glmsAdpiDeactivatePuRsp.activationState = pu_getActivationState();
   sigSend->glmsAdpiDeactivatePuRsp.puDeactivated = pu_getPuDeactivated();
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

   /* Need to set licensed State to disabled for all Feature Keys and Capacity Keys
      if IU is deactive and license is not installed .
      send Mo update Ind accordingly*/

   lm_validateLicenses();
}

static void
handleGlmsAdpiReadAmMoReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadAmMoRsp),
                                      GLMS_ADPI_READ_AM_MO_RSP);
   sigSend->glmsReadAmMoRsp.expiration       = am_getExpiration();
   sigSend->glmsReadAmMoRsp.activationState  = am_getActivationState();
   strncpy(sigSend->glmsReadAmMoRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sigSend->glmsReadAmMoRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsReadAmMoRsp.result = GLMS_OK;
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiReadMoKeyFileInformationReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoKeyFileInformationRsp),
                                      GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_RSP);

   sigSend->glmsReadMoKeyFileInformationRsp.sequenceNumber =
      keyFile_getSequenceNumber();
   sigSend->glmsReadMoKeyFileInformationRsp.installationTime =
      keyFile_getInstallationTime();
   sigSend->glmsReadMoKeyFileInformationRsp.locatable =
      keyFile_getLocatable();
   strcpy(sigSend->glmsReadMoKeyFileInformationRsp.productType,
          keyFile_getProductType());
   sigSend->glmsReadMoKeyFileInformationRsp.result = GLMS_OK;
   strncpy(sigSend->glmsReadMoKeyFileInformationRsp.resultInfo, "",
           GLMS_RESULT_INFO_LEN);
   sigSend->glmsReadMoKeyFileInformationRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiReadMoInstallKeyFileReportProgressReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoInstallKeyFileReportProgressRsp),
                                      GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_RSP);

   keyFile_fillReportProgressStruct(&(sigSend->glmsReadMoInstallKeyFileReportProgressRsp.
                                      reportProgress));

   sigSend->glmsReadMoInstallKeyFileReportProgressRsp.result = GLMS_OK;
   strncpy(sigSend->glmsReadMoInstallKeyFileReportProgressRsp.resultInfo, "",
           GLMS_RESULT_INFO_LEN);
   sigSend->glmsReadMoInstallKeyFileReportProgressRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiReadMoLmReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoLmRsp),
                                      GLMS_ADPI_READ_MO_LM_RSP);
   strcpy(sigSend->glmsReadMoLmRsp.fingerprint, lm_getFingerprint());
   sigSend->glmsReadMoLmRsp.fingerprintUpdateable =
      lm_getFingerprintUpdatable();
   sigSend->glmsReadMoLmRsp.lmState =
      lm_getLmState();
   sigSend->glmsReadMoLmRsp.lastInventoryChange =
      lm_getLastInventoryChange();
   sigSend->glmsReadMoLmRsp.lastLicenseInventoryRefresh =
      lm_getLastInventoryRefresh();
   sigSend->glmsReadMoLmRsp.referenceToLicenseServer[0] = '\0';
   sigSend->glmsReadMoLmRsp.result = GLMS_OK;
   strncpy(sigSend->glmsReadMoLmRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sigSend->glmsReadMoLmRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiReadMoLicenseSupportReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoLicenseSupportRsp),
                                      GLMS_ADPI_READ_MO_LICENSE_SUPPORT_RSP);
   strcpy(sigSend->glmsReadMoLicenseSupportRsp.areaId, ls_getAreaId());
   sigSend->glmsReadMoLicenseSupportRsp.result = GLMS_OK;
   strcpy(sigSend->glmsReadMoLicenseSupportRsp.resultInfo, "");
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiGetFeatureStateMoListReq(union itc_msg *sigRec)
{
   FeatureState *fs;
   uint32_t      nrOfFeatureStateMos = featureState_getNrOfFeatureStateMos();
   uint32_t      i;

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiGetFeatureStateMoListRsp)
                                      + (nrOfFeatureStateMos * GLMS_MO_KEY_LEN),
                                      GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_RSP);
   sigSend->glmsGetFeatureStateMoListRsp.result              = GLMS_OK;
   sigSend->glmsGetFeatureStateMoListRsp.nrOfFeatureStateIds = nrOfFeatureStateMos;

   i = 0;
   for(fs = featureState_getFirst();
       fs != NULL;
       fs = featureState_getNext(fs), i++)
   {
      strncpy(sigSend->glmsGetFeatureStateMoListRsp.featureStateId[i],
              featureState_getFeatureStateId(fs), GLMS_MO_KEY_LEN);
      sigSend->glmsGetFeatureStateMoListRsp.featureStateId[i][GLMS_MO_KEY_LEN] = '\0';
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiReadMoFeatureStateReq(union itc_msg *sigRec)
{
   FeatureState *fs = featureState_findByFeatureStateId(sigRec->glmsReadMoFeatureStateReq.
                                                        featureStateId);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoFeatureStateRsp),
                                      GLMS_ADPI_READ_MO_FEATURE_STATE_RSP);
   strcpy(sigSend->glmsReadMoFeatureStateRsp.featureStateId,
          sigRec->glmsReadMoFeatureStateReq.featureStateId);

   if (fs != NULL)
   {
      strncpy(sigSend->glmsReadMoFeatureStateRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsReadMoFeatureStateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoFeatureStateRsp.result = GLMS_OK;
      strcpy(sigSend->glmsReadMoFeatureStateRsp.keyId, featureState_getKeyId(fs));
      strcpy(sigSend->glmsReadMoFeatureStateRsp.description, featureState_getDescription(fs));
      strcpy(sigSend->glmsReadMoFeatureStateRsp.activeFeatureKeyId, featureState_getActiveFeatureKeyId(fs));

      sigSend->glmsReadMoFeatureStateRsp.featureState = featureState_getFeatureState(fs);
      sigSend->glmsReadMoFeatureStateRsp.licenseState = featureState_getLicenseState(fs);
      sigSend->glmsReadMoFeatureStateRsp.serviceState = featureState_getServiceState(fs);
   }
   else
   {
      snprintf(sigSend->glmsReadMoFeatureStateRsp.resultInfo,
               GLMS_RESULT_INFO_LEN,
               "Feature State id %s was not found",
               sigRec->glmsReadMoFeatureStateReq.featureStateId);
      sigSend->glmsReadMoFeatureStateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoFeatureStateRsp.result = GLMS_NOK;
      strcpy(sigSend->glmsReadMoFeatureStateRsp.keyId, "");
      strcpy(sigSend->glmsReadMoFeatureStateRsp.description, "");
      strcpy(sigSend->glmsReadMoFeatureStateRsp.activeFeatureKeyId, "");

      sigSend->glmsReadMoFeatureStateRsp.featureState = GLMS_FEATURESTATE_DEACTIVATED;
      sigSend->glmsReadMoFeatureStateRsp.licenseState = GLMS_LICENSESTATE_DISABLED;
      sigSend->glmsReadMoFeatureStateRsp.serviceState = GLMS_SERVICESTATE_INOPERABLE;
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiGetFeatureKeyMoListReq(union itc_msg *sigRec)
{
   FeatureState *fs;
   FeatureKey   *fk;
   uint32_t      nrOfFeatureKeyMos = featureState_getNrOfFeatureKeyMos();
   uint32_t      i;
   
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiGetFeatureKeyMoListRsp)
                                      + (nrOfFeatureKeyMos * GLMS_MO_KEY_LEN),
                                      GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_RSP);
   sigSend->glmsGetFeatureKeyMoListRsp.result            = GLMS_OK;
   sigSend->glmsGetFeatureKeyMoListRsp.nrOfFeatureKeyIds = nrOfFeatureKeyMos;

   i = 0;
   for (fs = featureState_getFirst();
        fs != NULL;
        fs = featureState_getNext(fs))
   {
      for (fk = featureState_getFirstFeatureKey(fs);
           fk != NULL;
           fk = featureKey_getNext(fk))
      {
         strncpy(sigSend->glmsGetFeatureKeyMoListRsp.featureKeyId[i],
                 featureKey_getFeatureKeyId(fk),
                 GLMS_MO_KEY_LEN);
         sigSend->glmsGetFeatureKeyMoListRsp.featureKeyId[i][GLMS_MO_KEY_LEN] = '\0';
         i++;
      }
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiReadMoFeatureKeyReq(union itc_msg *sigRec)
{
   FeatureKey   *fk = featureKey_findByFeatureKeyId(sigRec->glmsReadMoFeatureKeyReq.
                                      featureKeyId);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoFeatureKeyRsp),
                                      GLMS_ADPI_READ_MO_FEATURE_KEY_RSP);
   strcpy(sigSend->glmsReadMoFeatureKeyRsp.featureKeyId,
          sigRec->glmsReadMoFeatureKeyReq.featureKeyId);

   if(fk != NULL)
   {
      strncpy(sigSend->glmsReadMoFeatureKeyRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsReadMoFeatureKeyRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoFeatureKeyRsp.result = GLMS_OK;
      strcpy(sigSend->glmsReadMoFeatureKeyRsp.keyId,
             featureKey_getKeyId(fk));
      strcpy(sigSend->glmsReadMoFeatureKeyRsp.name,
             featureKey_getName(fk));
      strcpy(sigSend->glmsReadMoFeatureKeyRsp.productType,
             featureKey_getProductType(fk));
      sigSend->glmsReadMoFeatureKeyRsp.validFrom =
         featureKey_getValidFrom(fk);
      sigSend->glmsReadMoFeatureKeyRsp.expiration =
         featureKey_getExpiration(fk);
      sigSend->glmsReadMoFeatureKeyRsp.shared =
         featureKey_getKeyType(fk);
   }
   else
   {
      snprintf(sigSend->glmsReadMoFeatureKeyRsp.resultInfo,
               GLMS_RESULT_INFO_LEN,
               "The requested FeatureKey Id %s could not be found",
               sigRec->glmsReadMoFeatureKeyReq.featureKeyId);
      sigSend->glmsReadMoFeatureKeyRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoFeatureKeyRsp.result = GLMS_NOK;
      strcpy(sigSend->glmsReadMoFeatureKeyRsp.keyId, "");
      strcpy(sigSend->glmsReadMoFeatureKeyRsp.name, "");
      strcpy(sigSend->glmsReadMoFeatureKeyRsp.productType, "");
      sigSend->glmsReadMoFeatureKeyRsp.validFrom    = 0;
      sigSend->glmsReadMoFeatureKeyRsp.expiration   = 0;
      sigSend->glmsReadMoFeatureKeyRsp.shared       = GLMS_FALSE;
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiGetCapacityStateMoListReq(union itc_msg *sigRec)
{
   CapacityState *cs;
   uint32_t      nrOfCapacityStateMos = capacityState_getNrOfCapacityStateMos();
   uint32_t      i;

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiGetCapacityStateMoListRsp)
                                      + (nrOfCapacityStateMos * GLMS_MO_KEY_LEN),
                                      GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_RSP);
   sigSend->glmsGetCapacityStateMoListRsp.result              = GLMS_OK;
   sigSend->glmsGetCapacityStateMoListRsp.nrOfCapacityStateIds = nrOfCapacityStateMos;


   i = 0;
   for (cs = capacityState_getFirst();
        cs != NULL;
        cs = capacityState_getNext(cs), i++)
   {
      strncpy(sigSend->glmsGetCapacityStateMoListRsp.capacityStateId[i],
              capacityState_getCapacityStateId(cs),
              GLMS_MO_KEY_LEN);
      sigSend->glmsGetCapacityStateMoListRsp.capacityStateId[i][GLMS_MO_KEY_LEN] = '\0';
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiReadMoCapacityStateReq(union itc_msg *sigRec)
{
   CapacityState *cs = capacityState_findByCapacityStateId(
		 sigRec->glmsReadMoCapacityStateReq.capacityStateId);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoCapacityStateRsp),
                                      GLMS_ADPI_READ_MO_CAPACITY_STATE_RSP);
   strcpy(sigSend->glmsReadMoCapacityStateRsp.capacityStateId,
          sigRec->glmsReadMoCapacityStateReq.capacityStateId);

   if(cs != NULL)
   {
      strncpy(sigSend->glmsReadMoCapacityStateRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsReadMoCapacityStateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoCapacityStateRsp.result = GLMS_OK;
      strcpy(sigSend->glmsReadMoCapacityStateRsp.capacityUnit,
             capacityState_getCapacityUnit(cs));
      strcpy(sigSend->glmsReadMoCapacityStateRsp.keyId,
             capacityState_getCapacityKeyId(cs));
      strcpy(sigSend->glmsReadMoCapacityStateRsp.description,
             capacityState_getDescription(cs));
      strcpy(sigSend->glmsReadMoCapacityStateRsp.activeCapacityKeyId,
             capacityState_getActiveCapacityKeyId(cs));
      sigSend->glmsReadMoCapacityStateRsp.licensedCapacityLimitReached =
         capacityState_getLicensedCapacityLimitReached(cs);
      sigSend->glmsReadMoCapacityStateRsp.licenseState =
         capacityState_getLicenseState(cs);
      sigSend->glmsReadMoCapacityStateRsp.grantedCapacityLevel =
         capacityState_getGrantedCapacityLevel(cs);
      sigSend->glmsReadMoCapacityStateRsp.currentCapacityValue =
         capacityState_getCurrentCapacityValue(cs);
   }
   else
   {
      snprintf(sigSend->glmsReadMoCapacityStateRsp.resultInfo,
               GLMS_RESULT_INFO_LEN,
               "Capacity State id %s was not found",
               sigRec->glmsReadMoCapacityStateReq.capacityStateId);
      sigSend->glmsReadMoCapacityStateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoCapacityStateRsp.result = GLMS_NOK;
      strcpy(sigSend->glmsReadMoCapacityStateRsp.keyId, "");
      strcpy(sigSend->glmsReadMoCapacityStateRsp.capacityUnit, "");
      strcpy(sigSend->glmsReadMoCapacityStateRsp.description, "");
      strcpy(sigSend->glmsReadMoCapacityStateRsp.activeCapacityKeyId, "");
      sigSend->glmsReadMoCapacityStateRsp.licensedCapacityLimitReached = GLMS_FALSE;
      sigSend->glmsReadMoCapacityStateRsp.licenseState = GLMS_LICENSESTATE_DISABLED;
      sigSend->glmsReadMoCapacityStateRsp.grantedCapacityLevel = 0;
      sigSend->glmsReadMoCapacityStateRsp.currentCapacityValue = 
               capacityState_getDefaultCapacityValue();
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiReadMoGracePeriodReq(union itc_msg *sigRec)
{
   CapacityState *cs = capacityState_findByCapacityStateId(
		                 sigRec->glmsReadMoGracePeriodReq.capacityStateId);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoGracePeriodRsp),
                                      GLMS_ADPI_READ_MO_GRACE_PERIOD_RSP);
   
   strncpy(sigSend->glmsReadMoGracePeriodRsp.capacityStateId,
           sigRec->glmsReadMoGracePeriodReq.capacityStateId,
           GLMS_MO_KEY_LEN);
   sigSend->glmsReadMoGracePeriodRsp.capacityStateId[GLMS_MO_KEY_LEN - 1] = '\0';

   if (cs == NULL)
   {
      strncpy(sigSend->glmsReadMoGracePeriodRsp.resultInfo,
              "Capacity State id was not found",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsReadMoGracePeriodRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoGracePeriodRsp.result = GLMS_NOK;
      sigSend->glmsReadMoGracePeriodRsp.gracePeriod =
               capacityState_getDefaultGracePeriod();
   }
   else if(capacityState_getIsGpControlled(cs) == 0)
   {
      strncpy(sigSend->glmsReadMoGracePeriodRsp.resultInfo,
              "Capacity State is not grace period controlled",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsReadMoGracePeriodRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoGracePeriodRsp.result = GLMS_NOK;
      sigSend->glmsReadMoGracePeriodRsp.gracePeriod =
               capacityState_getDefaultGracePeriod();
   }
   else
   {
      strncpy(sigSend->glmsReadMoGracePeriodRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsReadMoGracePeriodRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoGracePeriodRsp.result = GLMS_OK;
      capacityState_copyGracePeriod(cs, &sigSend->glmsReadMoGracePeriodRsp.gracePeriod);
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiGetCapacityKeyMoListReq(union itc_msg *sigRec)
{
   CapacityState *cs;
   CapacityKey   *ck;
   uint32_t      nrOfCapacityKeyMos = capacityState_getNrOfCapacityKeyMos();
   uint32_t      i;
   
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiGetCapacityKeyMoListRsp)
                                      + (nrOfCapacityKeyMos * GLMS_MO_KEY_LEN),
                                      GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_RSP);
   sigSend->glmsGetCapacityKeyMoListRsp.result            = GLMS_OK;
   sigSend->glmsGetCapacityKeyMoListRsp.nrOfCapacityKeyIds = nrOfCapacityKeyMos;

   i = 0;
   for (cs = capacityState_getFirst();
        cs != NULL;
        cs = capacityState_getNext(cs))
   {
      for (ck = capacityState_getFirstCapacityKey(cs);
           ck != NULL;
           ck = capacityKey_getNext(ck))
      {
         strncpy(sigSend->glmsGetCapacityKeyMoListRsp.capacityKeyId[i],
                 capacityKey_getCapacityKeyId(ck),
                 GLMS_MO_KEY_LEN);
         sigSend->glmsGetCapacityKeyMoListRsp.capacityKeyId[i][GLMS_MO_KEY_LEN] = '\0';
         i++;
      }
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiReadMoCapacityKeyReq(union itc_msg *sigRec)
{
   CapacityKey   *ck = capacityKey_findByCapacityKeyId(
		                 sigRec->glmsReadMoCapacityKeyReq.capacityKeyId);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiReadMoCapacityKeyRsp),
                                      GLMS_ADPI_READ_MO_CAPACITY_KEY_RSP);
   strcpy(sigSend->glmsReadMoCapacityKeyRsp.capacityKeyId,
          sigRec->glmsReadMoCapacityKeyReq.capacityKeyId);

   if (ck != NULL)
   {
      strncpy(sigSend->glmsReadMoCapacityKeyRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsReadMoCapacityKeyRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoCapacityKeyRsp.result = GLMS_OK;
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.keyId,
             capacityKey_getKeyId(ck));
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.name,
             capacityKey_getName(ck));
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.productType,
             capacityKey_getProductType(ck));
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.capacityUnit,
             capacityKey_getCapacityUnit(ck));
      sigSend->glmsReadMoCapacityKeyRsp.grantedCapacityLevel =
         capacityKey_getGrantedCapacityLevel(ck);
      sigSend->glmsReadMoCapacityKeyRsp.licensedCapacityLimitReached =
         capacityKey_getLicensedCapacityLimitReached(ck);
      sigSend->glmsReadMoCapacityKeyRsp.licensedCapacityValue =
         capacityKey_getLicensedCapacityValue(ck);
      sigSend->glmsReadMoCapacityKeyRsp.validFrom =
         capacityKey_getValidFrom(ck);
      sigSend->glmsReadMoCapacityKeyRsp.expiration =
         capacityKey_getExpiration(ck);
   }
   else
   {
      snprintf(sigSend->glmsReadMoCapacityKeyRsp.resultInfo,
               GLMS_RESULT_INFO_LEN,
               "The requested CapacityKey Id %s could not be found",
               sigRec->glmsReadMoCapacityKeyReq.capacityKeyId);
      sigSend->glmsReadMoCapacityKeyRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      sigSend->glmsReadMoCapacityKeyRsp.result = GLMS_NOK;
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.keyId, "");
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.name, "");
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.capacityUnit, "");
      strcpy(sigSend->glmsReadMoCapacityKeyRsp.productType, "");
      sigSend->glmsReadMoCapacityKeyRsp.validFrom = 0;
      sigSend->glmsReadMoCapacityKeyRsp.expiration = 0;
      sigSend->glmsReadMoCapacityKeyRsp.grantedCapacityLevel = 0;
      sigSend->glmsReadMoCapacityKeyRsp.licensedCapacityLimitReached = GLMS_FALSE;
      sigSend->glmsReadMoCapacityKeyRsp.licensedCapacityValue =
         capacityState_getDefaultCapacityValue();
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiSetFingerprintReq(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "handleGlmsAdpiSetFingerprintReq",
              sigRec->glmsSetFingerprintReq.fingerprint);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiSetFingerprintRsp),
                                      GLMS_ADPI_SET_FINGERPRINT_RSP);
   sigSend->glmsSetFingerprintRsp.result = GLMS_NOK;

   if (lm_getFingerprintUpdatable())
   {
      tracepoint(com_ericsson_glms, debug_trace, "New fingerprint accepted");
      glms_logEvent(GLMS_LOG_LOW, "New fingerprint accepted: %s",
                    sigRec->glmsSetFingerprintReq.fingerprint);
      lm_setFingerprint(sigRec->glmsSetFingerprintReq.fingerprint);
      lm_storeParameters();
      lm_sendMoUpdateLmInd();

      sigSend->glmsSetFingerprintRsp.result = GLMS_OK;
      strncpy(sigSend->glmsSetFingerprintRsp.resultInfo, "",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsSetFingerprintRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   }
   else
   {
      tracepoint(com_ericsson_glms, debug_trace, "New fingerprint rejected");
      glms_logEvent(GLMS_LOG_LOW, "New fingerprint rejected: %s",
                    sigRec->glmsSetFingerprintReq.fingerprint);

      sigSend->glmsSetFingerprintRsp.result = GLMS_NOK;
      strncpy(sigSend->glmsSetFingerprintRsp.resultInfo,
              "Not allowed to update fingerprint. "
              "Verify that sequence number is 0.",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsSetFingerprintRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   }

   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
sendGlmsAdpiStoreKeyFileReq(char * kfLocation)
{
   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "sendGlmsAdpiStoreKeyFileReq",
              kfLocation);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiStoreKeyFileReq),
                                      GLMS_ADPI_STORE_KEY_FILE_REQ);
   strncpy(sigSend->glmsStoreKeyFileReq.kfLocation, kfLocation,
           GLMS_KEYFILE_LOCATION_LEN);
   sigSend->glmsStoreKeyFileReq.kfLocation[GLMS_KEYFILE_LOCATION_LEN - 1] = '\0';
   sendMsgToAdapter(sigSend);
}

GlmsBool
checkIfInstallationOngoingAndSendRsp(union itc_msg *sigRec)
{
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiInstallKeyFileRsp),
                                      GLMS_ADPI_INSTALL_KEY_FILE_RSP);
   /* If an install action is already running we reply with a actionId = 0
    * and cancel the install request. The already running install action will
    * continue to run. */
   if (keyFile_isInstallActionRunning())
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "Parallel installKeyFile action rejected because the"
                 " action is already running");
      glms_logEvent(GLMS_LOG_LOW,
                 "Parallel installKeyFile action rejected because the"
                 " action is already running");

      sigSend->glmsInstallKeyFileRsp.result    = GLMS_NOK;
      sigSend->glmsInstallKeyFileRsp.actionId  = 0;
      itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

      return GLMS_TRUE;
   }

   keyFile_installActionStart();

   sigSend->glmsInstallKeyFileRsp.result   = GLMS_OK;
   sigSend->glmsInstallKeyFileRsp.actionId = keyFile_getInstallActionId();
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

   return GLMS_FALSE;
}

GlmsBool
downloadKeyFile(union itc_msg *sigRec)
{
   if (strncmp(sigRec->glmsInstallKeyFileReq.kfUri, "sftp:", 5) == 0 ||
       strncmp(sigRec->glmsInstallKeyFileReq.kfUri, "ftp:", 4) == 0 ||
       strncmp(sigRec->glmsInstallKeyFileReq.kfUri, "ftpes:", 6) == 0)
   {
      tracepoint(com_ericsson_glms, debug_trace,
                 "Send GlmsAdpiDownloadKeyFileReq");

      union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiDownloadKeyFileReq),
                                         GLMS_ADPI_DOWNLOAD_KEY_FILE_REQ);
      strncpy(sigSend->glmsDownloadKeyFileReq.kfUri,
              sigRec->glmsInstallKeyFileReq.kfUri,
              GLMS_KEYFILE_URI_LEN);
      sigSend->glmsDownloadKeyFileReq.kfUri[GLMS_KEYFILE_URI_LEN - 1] = '\0';
      strncpy(sigSend->glmsDownloadKeyFileReq.password,
              sigRec->glmsInstallKeyFileReq.password,
              GLMS_PASSWORD_LEN);
      sigSend->glmsDownloadKeyFileReq.password[GLMS_PASSWORD_LEN - 1] = '\0';
      itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

      return GLMS_TRUE;
   }

   return GLMS_FALSE;
}

GlmsBool
parseAndVerifyKeyFile(union itc_msg *sigRec)
{
   if (strncmp(sigRec->glmsInstallKeyFileReq.kfUri, "file:", 5) == 0 ||
       strncmp(sigRec->glmsInstallKeyFileReq.kfUri, "/", 1) == 0)
   {
      char kfLocation[GLMS_KEYFILE_LOCATION_LEN];

      /* If the first char is a '/' we expect a direct path. */
      if (sigRec->glmsInstallKeyFileReq.kfUri[0] == '/')
      {
         strncpy(kfLocation,
                 sigRec->glmsInstallKeyFileReq.kfUri,
                 GLMS_KEYFILE_LOCATION_LEN);
      }
      /* Otherwise we remove the 'file:/' part to get the direct path. */
      else
      {
         strncpy(kfLocation,
                 sigRec->glmsInstallKeyFileReq.kfUri + 6, /* +6 to remove the 'file:/' part */
                 GLMS_KEYFILE_LOCATION_LEN - 6); /* -6 to not parse too much from kfUri */
      }

      kfLocation[GLMS_KEYFILE_LOCATION_LEN - 1] = '\0';

      if(access(kfLocation, F_OK) == -1)
      {
         tracepoint(com_ericsson_glms, info_trace_w_int_arg,
                    "parseAndVerifyKeyFile failed with errno",
                    errno);
         rejectKeyFileInstallation(sigRec, "Failed to access the Key File");
         return GLMS_TRUE;
      }

      tracepoint(com_ericsson_glms, debug_trace,
                 "Send GlmsAdpiPkiVerificationReq");

      union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiPkiVerificationReq),
                                         GLMS_ADPI_PKI_VERIFICATION_REQ);
      strncpy(sigSend->glmsPkiVerificationReq.kfLocation,
              kfLocation,
              GLMS_KEYFILE_LOCATION_LEN);
      sigSend->glmsPkiVerificationReq.kfLocation[GLMS_KEYFILE_LOCATION_LEN - 1] = '\0';
      sendMsgToAdapter(sigSend);

      return GLMS_TRUE;
   }

   return GLMS_FALSE;
}

static void
rejectKeyFileInstallation(union itc_msg *sigRec, char *errorstr)
{
   tracepoint(com_ericsson_glms, info_trace_w_str_arg,
              "InstallKeyFile action rejected", errorstr);
   glms_logEvent(GLMS_LOG_LOW,
                 "InstallKeyFile action rejected - %s", errorstr);

   keyFile_installActionFailed(errorstr);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiInstallKeyFileInd),
                                      GLMS_ADPI_INSTALL_KEY_FILE_IND);
   sigSend->glmsInstallKeyFileInd.result = GLMS_NOK;
   strcpy(sigSend->glmsInstallKeyFileInd.additionalInfo, errorstr);
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

   keyFile_sendMoUpdateKeyfileInd();
}

static void
handleGlmsAdpiInstallKeyFileReq(union itc_msg *sigRec)
{

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "handleGlmsAdpiInstallKeyFileReq",
              sigRec->glmsInstallKeyFileReq.kfUri);

   if (checkIfInstallationOngoingAndSendRsp(sigRec))
      return;

   keyFile_sendMoUpdateKeyfileInd();

   /* If the key file is located on an FTP server we request the adapter layer to
      download the key file. */
   if (downloadKeyFile(sigRec))
      return;

   /* If the key file is local on the node we don't need to request the
      adapter layer to download it. In that case we parse and verify the key file. */
   if (parseAndVerifyKeyFile(sigRec))
      return;

   /* If the path does not start with any of the expected strings we reject the
      install action */
   rejectKeyFileInstallation(sigRec, "Failed to parse the URI to the key file");
}

static void
sendInstallAreaLicenseKeysRsp(union itc_msg *sigRec,
                              GlmsInstallAreaLicenseKeyResult resultCode)
{
   tracepoint(com_ericsson_glms, info_trace_w_int_arg,
              "InstallAreaLicenseKeys action rejected", resultCode);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiInstallAreaLicenseKeysRsp),
                                      GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_RSP);
   sigSend->glmsInstallAreaLicenseKeysRsp.resultCode = resultCode;
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}

static void
handleGlmsAdpiInstallAreaLicenseKeysReq(union itc_msg *sigRec)
{
   GlmsInstallAreaLicenseKeyResult resultCode;

   tracepoint(com_ericsson_glms, call_to_function,
              "handleGlmsAdpiInstallAreaLicenseKeysReq");

   resultCode = ls_installAreaLicenseKeys(sigRec->glmsInstallAreaLicenseKeysReq.keys,
                                          sigRec->glmsInstallAreaLicenseKeysReq.createStateMOs);

   if(resultCode == GLMS_ALKF_INSTALL_SUCCESS)
   {
      lm_validateLicenses();
      ls_sendMoUpdateLicenseSupportInd();
      ls_setAlkf(sigRec->glmsInstallAreaLicenseKeysReq.keys);
   }

   sendInstallAreaLicenseKeysRsp(sigRec, resultCode);
}

static void
handleGlmsAdpiDownloadKeyFileRsp(union itc_msg *sigRec)
{
   union itc_msg *sigSend;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleGlmsAdpiDownloadKeyFileRsp",
              sigRec->glmsDownloadKeyFileRsp.result);

   if (isSignalResultOk(sigRec->glmsDownloadKeyFileRsp.result))
   {
      /* Update the install actions report status */
      keyFile_installActionLkfDownloaded();
      
      tracepoint(com_ericsson_glms, debug_trace,
                 "Send GlmsAdpiPkiVerificationReq");
      
      sigSend = itc_alloc(sizeof(GlmsAdpiPkiVerificationReq),
                          GLMS_ADPI_PKI_VERIFICATION_REQ);
      strncpy(sigSend->glmsPkiVerificationReq.kfLocation,
              sigRec->glmsDownloadKeyFileRsp.kfLocation,
              GLMS_KEYFILE_LOCATION_LEN);
      sigSend->glmsPkiVerificationReq.kfLocation[GLMS_KEYFILE_LOCATION_LEN - 1] = '\0';
      sendMsgToAdapter(sigSend);
   }
   else
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "InstallKeyFile action rejected - Failed to download"
                    " the key file");

      keyFile_installActionFailed("Failed to download the key file");
      sigSend = itc_alloc(sizeof(GlmsAdpiInstallKeyFileInd),
                      GLMS_ADPI_INSTALL_KEY_FILE_IND);
      sigSend->glmsInstallKeyFileInd.result = GLMS_NOK;
      strcpy(sigSend->glmsInstallKeyFileInd.additionalInfo,
             "Failed to download the key file");
      sendMsgToAdapter(sigSend);

      keyFile_sendMoUpdateKeyfileInd();
   }
}

void
doActionsAtKeyFileVerficationPassed(union itc_msg *sigRec)
{
    if(keyFile_isInstallActionRunning())
    {
       /* If an install action is running we will request the adapter
        * to store the new key file. When the store is confirmed the
        * new key file will be activated. */

       sendGlmsAdpiStoreKeyFileReq(sigRec->glmsPkiVerificationRsp.kfLocation);
    }
    else if(adapter_isLicenseRefreshRunning())
    {
       /* If no install action is running it means the key is being read at
        * glms activation or licenseRefresh. No store is required, instead activate
        * the new key file right away. */
       keyFile_activateKeyFile(GLMS_FALSE);
       adapter_sendRspAndEndLicenseRefresh(GLMS_OK);
    }
    else
    {
       /* Activate key file and publish LIHI at GLMS activation */
       keyFile_activateKeyFile(GLMS_FALSE);
       publishInterfaceInNs(LICENSE_FEATURE_CONTROL_I_SERVICE_NAME);
       publishInterfaceInNs(LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME);
    }
}

void
doActionsAtKeyFileVerficationFailed(char *verifyResult)
{
   union itc_msg *sigSend;
   char failStr[200];
   strcpy(failStr, "Failed to verify the key file: ");
   strcat(failStr, verifyResult);

   if (keyFile_isInstallActionRunning())
   {
      tracepoint(com_ericsson_glms, info_trace,
                 failStr);
      glms_logEvent(GLMS_LOG_LOW,
                    "InstallKeyFile action rejected - %s", failStr);

      keyFile_installActionFailed(failStr);

      sigSend = itc_alloc(sizeof(GlmsAdpiInstallKeyFileInd),
                          GLMS_ADPI_INSTALL_KEY_FILE_IND);
      sigSend->glmsInstallKeyFileInd.result = GLMS_NOK;
      strcpy(sigSend->glmsInstallKeyFileInd.additionalInfo, failStr);
      sendMsgToAdapter(sigSend);

      keyFile_sendMoUpdateKeyfileInd();
   }
   else if(adapter_isLicenseRefreshRunning())
   {
      tracepoint(com_ericsson_glms, info_trace,
                 failStr);
      glms_logEvent(GLMS_LOG_LOW,
                    "licenseRefresh action failed - %s", failStr);

      adapter_sendRspAndEndLicenseRefresh(GLMS_NOK);
   }
   else
   {
      tracepoint(com_ericsson_glms, info_trace, failStr);
      glms_logEvent(GLMS_LOG_LOW, "Failed to read key file at startup - %s", failStr);

      publishInterfaceInNs(LICENSE_FEATURE_CONTROL_I_SERVICE_NAME);
      publishInterfaceInNs(LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME);

      keyFile_activateKeyFileFaultAlarm();
      am_activateAutonomousMode();
      lm_validateLicenses();
   }
}

/* GlmsAdpiPkiVerificationRsp can be received in three occations.
   1: During installKeyFile action.
   2: When the key file is read at GLMS activation. LIHI needs to be published to NS.
   3: When the key file is read at refreshLicenseInventory action.
      GlmsAdpiRefreshLicenseInventoryRsp needs to be sent to adapter.
*/
static void
handleGlmsAdpiPkiVerificationRsp(union itc_msg *sigRec)
{
   union itc_msg *sigSend;
   char *verifyResult;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleGlmsAdpiPkiVerificationRsp",
              sigRec->glmsPkiVerificationRsp.result);

   if (isSignalResultOk(sigRec->glmsPkiVerificationRsp.result))
   {
      verifyResult = keyFile_readAndValidateKeyFile(sigRec->glmsPkiVerificationRsp.
                                                    kfLocation);

      tracepoint(com_ericsson_glms, info_trace_w_str_arg,
                 "keyFile_readAndValidateKeyFile returned", verifyResult);

      if (!strcmp(verifyResult, KEYFILE_PARSE_OK))
      {
         doActionsAtKeyFileVerficationPassed(sigRec);
      }
      else
      {
         doActionsAtKeyFileVerficationFailed(verifyResult);
      }
   }
   else if (adapter_isLicenseRefreshRunning())
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "Failed to verify PKI signature at licenseRefresh action");
      glms_logEvent(GLMS_LOG_LOW,
                    "Failed to verify PKI signature at licenseRefresh action");

      adapter_sendRspAndEndLicenseRefresh(GLMS_NOK);
   }
   else if(keyFile_isInstallActionRunning())
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "InstallKeyFile action rejected - "
                 "Signature verification of key file failed");
      glms_logEvent(GLMS_LOG_LOW,
                    "InstallKeyFile action rejected - "
                    "Signature verification of key file failed");

      keyFile_installActionFailed("Signature verification of key file failed");

      sigSend = itc_alloc(sizeof(GlmsAdpiInstallKeyFileInd),
                      GLMS_ADPI_INSTALL_KEY_FILE_IND);
      sigSend->glmsInstallKeyFileInd.result = GLMS_NOK;
      strcpy(sigSend->glmsInstallKeyFileInd.additionalInfo,
             "Failed to verify signature of the key file");
      sendMsgToAdapter(sigSend);
      keyFile_sendMoUpdateKeyfileInd();
   }
   else
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "Failed to verify PKI signature at GLMS activation");
      glms_logEvent(GLMS_LOG_LOW,
                    "Failed to verify PKI signature at GLMS activation");

      publishInterfaceInNs(LICENSE_FEATURE_CONTROL_I_SERVICE_NAME);
      publishInterfaceInNs(LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME);

      keyFile_activateKeyFileFaultAlarm();
      am_activateAutonomousMode();
      lm_validateLicenses();
   }
}

static void
handleGlmsAdpiStoreKeyFileRsp(union itc_msg *sigRec)
{
   union itc_msg *sigSend;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleGlmsAdpiStoreKeyFileRsp",
              sigRec->glmsStoreKeyFileRsp.result);

   if(!isSignalResultOk(sigRec->glmsStoreKeyFileRsp.result))
   {
      glms_logEvent(GLMS_LOG_LOW, "InstallKeyFile action rejected - "
                                  "Failed to store the key file");

      keyFile_installActionFailed("Failed to store the key file");

      sigSend = itc_alloc(sizeof(GlmsAdpiInstallKeyFileInd),
                          GLMS_ADPI_INSTALL_KEY_FILE_IND);
      sigSend->glmsInstallKeyFileInd.result = GLMS_NOK;
      strcpy(sigSend->glmsInstallKeyFileInd.additionalInfo,
             "Failed to store the key file");
      sendMsgToAdapter(sigSend);

      keyFile_sendMoUpdateKeyfileInd();
      return;
   }

   keyFile_activateKeyFile(GLMS_TRUE);
   keyFile_installActionLkfInstalled();

   glms_logEvent(GLMS_LOG_LOW, "InstallKeyFile action successful - "
                               "Key file is installed");

   sigSend = itc_alloc(sizeof(GlmsAdpiInstallKeyFileInd),
                       GLMS_ADPI_INSTALL_KEY_FILE_IND);
   sigSend->glmsInstallKeyFileInd.result = GLMS_OK;
   strcpy(sigSend->glmsInstallKeyFileInd.additionalInfo,
          "Key File Installed");
   sendMsgToAdapter(sigSend);
}


/* GlmsAdpiGetKeyFileLocation is requested once when GLMS is activated.
   Calling it will lead to the LFCI and LCCI interfaces being published to
   the nameserver.

   GlmsAdpiGetKeyFileLocation is also called during the refreshLicenseInventory
   action, see GlmsAdpiRefreshLicenseInventoryReq. */
static void
handleGlmsAdpiGetKeyFileLocationRsp(union itc_msg *sigRec)
{
   GlmsBool publishLihiToNsOrSendLicenseInventoryRsp = GLMS_FALSE;

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleGlmsAdpiGetKeyFileLocationRsp",
              sigRec->glmsGetKeyFileLocationRsp.result);

   if (isSignalResultOk(sigRec->glmsGetKeyFileLocationRsp.result))
   {
      if (strcmp("", sigRec->glmsGetKeyFileLocationRsp.kfLocation))
      {
         tracepoint(com_ericsson_glms, debug_trace,
                    "Send GlmsAdpiPkiVerificationReq");

         union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiPkiVerificationReq),
                                            GLMS_ADPI_PKI_VERIFICATION_REQ);
         strncpy(sigSend->glmsPkiVerificationReq.kfLocation,
                 sigRec->glmsGetKeyFileLocationRsp.kfLocation,
                 GLMS_KEYFILE_LOCATION_LEN);
         sigSend->glmsPkiVerificationReq.kfLocation[GLMS_KEYFILE_LOCATION_LEN - 1] = '\0';
         sendMsgToAdapter(sigSend);

         publishLihiToNsOrSendLicenseInventoryRsp = GLMS_FALSE;
      }
      else
      {
         /* No key file location exist. Possibly the first startup of GLMS. */
         publishLihiToNsOrSendLicenseInventoryRsp = GLMS_TRUE;
      }
   }
   else
   {
      tracepoint(com_ericsson_glms, error_trace,
                 "Failed to get key file location");
      publishLihiToNsOrSendLicenseInventoryRsp = GLMS_TRUE;
   }

   /* Publish LIHI, or send LicenseInventoryRsp,
      even if we fail to read the key file */
   if (publishLihiToNsOrSendLicenseInventoryRsp)
   {
      tracepoint(com_ericsson_glms, debug_trace,
                 "publishLihiToNsOrSendLicenseInventoryRsp is true");

      if(adapter_isLicenseRefreshRunning())
      {
         adapter_sendRspAndEndLicenseRefresh(GLMS_NOK);
      }
      else /* GLMS activate sequence */
      {
         glms_logEvent(GLMS_LOG_LOW,
                       "Failed to get key file location, or key file "
                       "has not been installed");
         
         publishInterfaceInNs(LICENSE_FEATURE_CONTROL_I_SERVICE_NAME);
         publishInterfaceInNs(LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME); 

         keyFile_activateKeyFileFaultAlarm();

         /* No LKF location is normal for initial configuration when no LKF
            have been installed. For that reason we will not start autonomous
            mode at that situation. For any other reason if we fail to read the
            LKF we will start AM. */
         if(!((strcmp("", sigRec->glmsGetKeyFileLocationRsp.kfLocation) == 0)
            && isSignalResultOk(sigRec->glmsGetKeyFileLocationRsp.result)))
         {
            am_activateAutonomousMode();
         }

         lm_validateLicenses();
      }
   }
}


static void
handleGlmsAdpiSetFeatureStateReq(union itc_msg *sigRec)
{
   FeatureState *fs = featureState_findByFeatureStateId(sigRec->glmsSetFeatureStateReq.
                                                        featureStateId);

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "handleGlmsAdpiSetFeatureStateReq",
              sigRec->glmsSetFeatureStateReq.featureStateId);

   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "Setting feature state to",
              sigRec->glmsSetFeatureStateReq.featureState);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiSetFeatureStateRsp),
                                      GLMS_ADPI_SET_FEATURE_STATE_RSP);
   strncpy(sigSend->glmsSetFeatureStateRsp.featureStateId,
           sigRec->glmsSetFeatureStateReq.featureStateId,
           GLMS_MO_KEY_LEN);
   sigSend->glmsSetFeatureStateRsp.featureStateId[GLMS_MO_KEY_LEN - 1] = '\0';

   if (fs == NULL)
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "Set FeatureState rejected because"
                 " FeatureState data structure was not found");
      glms_logEvent(GLMS_LOG_LOW, "Set FeatureState rejected because"
                    " FeatureState data structure was not found");

      sigSend->glmsSetFeatureStateRsp.result = GLMS_NOK;
      strncpy(sigSend->glmsSetFeatureStateRsp.resultInfo,
              "The FeatureState data does not exist",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsSetFeatureStateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
      return;
   }

   glms_logEvent(GLMS_LOG_LOW,
                 "FeatureState of %s is being set to %d",
                 featureState_getKeyId(fs),
                 sigRec->glmsSetFeatureStateReq.featureState);

   featureState_setFeatureState(fs, sigRec->glmsSetFeatureStateReq.featureState);
   featureState_storeParameters(fs);
   featureState_sendMoUpdateFeatureStateInd(fs);

   sigSend->glmsSetFeatureStateRsp.result = GLMS_OK;
   strncpy(sigSend->glmsSetFeatureStateRsp.resultInfo, "",
           GLMS_RESULT_INFO_LEN);
   sigSend->glmsSetFeatureStateRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleGlmsAdpiUpdateGracePeriodAttributesReq(union itc_msg *sigRec)
{
   int32_t oldActivationThreshold, newActivationThreshold;
   CapacityState *cs = capacityState_findByCapacityStateId(
                         sigRec->glmsUpdateGracePeriodAttributesReq.capacityStateId);

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "handleGlmsAdpiUpdateGracePeriodAttributesReq",
              sigRec->glmsUpdateGracePeriodAttributesReq.capacityStateId);

   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiUpdateGracePeriodAttributesRsp),
                                      GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_RSP);
   strncpy(sigSend->glmsUpdateGracePeriodAttributesRsp.capacityStateId,
           sigRec->glmsUpdateGracePeriodAttributesReq.capacityStateId,
           GLMS_MO_KEY_LEN);
   sigSend->glmsUpdateGracePeriodAttributesRsp.capacityStateId[GLMS_MO_KEY_LEN - 1] = '\0';

   if (cs == NULL)
   {
      tracepoint(com_ericsson_glms, info_trace,
                 "Update GP attributes rejected because"
                 " CapacityState data structure was not found");
      glms_logEvent(GLMS_LOG_LOW, "Update GP attributes rejected because"
                 " CapacityState data structure was not found");

      sigSend->glmsUpdateGracePeriodAttributesRsp.result = GLMS_NOK;
      strncpy(sigSend->glmsUpdateGracePeriodAttributesRsp.resultInfo,
              "The CapacityState data does not exist",
              GLMS_RESULT_INFO_LEN);
      sigSend->glmsUpdateGracePeriodAttributesRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
      itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
      return;
   }

   oldActivationThreshold = capacityState_getGracePeriodActivationThreshold(cs);

   capacityState_setGpConfigurableAttributes(cs,
           sigRec->glmsUpdateGracePeriodAttributesReq.gpActivationThreshold,
           sigRec->glmsUpdateGracePeriodAttributesReq.gpResetThreshold,
           sigRec->glmsUpdateGracePeriodAttributesReq.gpLength);
   capacityState_storeParameters(cs);
   capacityState_sendMoUpdateGracePeriodInd(cs);

   newActivationThreshold = capacityState_getGracePeriodActivationThreshold(cs);
   if (newActivationThreshold != oldActivationThreshold)
   {
      capacityState_sendLcciChangeIndToAllSubscribers(cs);
   }

   sigSend->glmsUpdateGracePeriodAttributesRsp.result = GLMS_OK;
   strncpy(sigSend->glmsUpdateGracePeriodAttributesRsp.resultInfo, "",
           GLMS_RESULT_INFO_LEN);
   sigSend->glmsUpdateGracePeriodAttributesRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
}


static void
handleReadingStoredParameters(union itc_msg *sigRec)
{
   uint32_t requestId, i, allRead;
   static uint32_t glmsActivated;
   union itc_msg *sigSend;
   static int32_t parametersReadStatus[GLMS_NO_OF_PARAMETER_TABLES];

   /* It is only at the time when GLMS becomes active that we will read the persistent
      parameters. The variable firstCallToFunction_handleReadingStoredParameters
      is set to true when GLMS becomes active. At the first call we reset the parametersReadStatus
      array to show that we have not read any of the existing tables. When all tables
      are read we can read the LKF and publish LIHI interfaces to the nameserver.*/
   if (firstCallToFunction_handleReadingStoredParameters)
   {
      for (i = 0; i<GLMS_NO_OF_PARAMETER_TABLES; i++)
      {
         parametersReadStatus[i] = 0;
      }
      
      glmsActivated = 0;
      firstCallToFunction_handleReadingStoredParameters = GLMS_FALSE;
   }

   if(sigRec->sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP)
   {
      requestId = sigRec->glmsPpIndexListRsp.requestId;
   }
   else if(sigRec->sigNo == GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP)
   {
      requestId = sigRec->glmsPpGetRsp.requestId;
   }
   else if(sigRec->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP)
   {
      requestId = sigRec->glmsSpIndexListRsp.requestId;
   }
   else if(sigRec->sigNo == GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP)
   {
      requestId = sigRec->glmsSpGetRsp.requestId;
   }
   else
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Faulty signal in call to handleReadingStoredParameters",
                 sigRec->sigNo);
      return;
   }

   if (requestId < GLMS_NO_OF_PARAMETER_TABLES)
   {
      parametersReadStatus[requestId] = ridMap_parseStoredParameters(sigRec, requestId);
   }
   else
   {
      tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                 "Unknown request id", requestId);
   }

   allRead = 0;
   for (i = 0; i < GLMS_NO_OF_PARAMETER_TABLES; i++)
   {
      /* If we fail to read the parameter we */
      /* still consider it read. */

      if(parametersReadStatus[i] == 1 ||
         parametersReadStatus[i] == -1)
         allRead++;
   }

   if (allRead == GLMS_NO_OF_PARAMETER_TABLES &&
       glmsActivated == 0)
   {
      /* All persistant parameters are read and we can now proceed to
         request the feature configuration list from the Adapter */

      glmsActivated = 1;

      sigSend = itc_alloc(sizeof(GlmsAdpiFeatureConfigurationListReq),
                          GLMS_ADPI_FEATURE_CONFIGURATION_LIST_REQ);
      itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
   }
}

static void
handleGlmsAdpiFeatureConfigurationListRsp(union itc_msg *sigRec)
{
   union itc_msg *sigSend;

   if (isSignalResultOk(sigRec->glmsAdpiFeatureConfigurationListRsp.result))
   {
     glms_logEvent(GLMS_LOG_LOW, "Received feature configuration list with %u features",
                   sigRec->glmsAdpiFeatureConfigurationListRsp.featureConfListLen);

     /* Set the feature configuration list in LM data */
     lm_setFeatureConfList(sigRec->glmsAdpiFeatureConfigurationListRsp.featureConfList,
                           sigRec->glmsAdpiFeatureConfigurationListRsp.featureConfListLen);

     /* With the feature configuration list in place, re-install the Area LKF (if any).
      * NB: lm_validateLicenses() will be called later as part of installing the *local* LKF
      * (even if no local LKF exists).
      */
     ls_reinstallAreaLicenseKeys();

     /* Get the local LKF (if any) */
     sigSend = itc_alloc(sizeof(GlmsAdpiGetKeyFileLocationReq),
                         GLMS_ADPI_GET_KEY_FILE_LOCATION_REQ);
     itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

     /* Send GLMS_ADPI_SUBSCRIBE_MO_UPDATES_RSP if subscribed */
     if (adapter_isSubscribedForMoUpdateInd())
     {
       sigSend = itc_alloc(sizeof(GlmsAdpiSubscribeMoUpdatesRsp),
                           GLMS_ADPI_SUBSCRIBE_MO_UPDATES_RSP);
       strncpy(sigSend->glmsAdpiSubscribeMoUpdatesRsp.resultInfo,
               "",
               GLMS_RESULT_INFO_LEN);
       sigSend->glmsAdpiSubscribeMoUpdatesRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
       sigSend->glmsAdpiSubscribeMoUpdatesRsp.result = GLMS_OK;
       itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);
     }

     adapter_setActivationState(ADPI_ACTIVATED);

     /* Handle the Emergency Unlock States and timers */
     eu_calculateActivationState();
     eu_handleGlmsRestart();

     /* Handle the Integration Unlock States and timers */
     iu_calculateActivationState();
     iu_handleGlmsRestart();

     /* Handle the Production Unlock States and timers */
     pu_calculateActivationState();
     pu_handleGlmsRestart();

     /* Handle the Autonomous Mode States and timers */
     am_handleGlmsRestart();

     /* Handle Grace Period */
     capacityState_handleGlmsRestart();

     /* Handle LmState as it can be changed due to  Unlock Methods*/
     lm_calculateLmState();
     lm_sendMoUpdateLmInd();
   }
   else
   {
     tracepoint(com_ericsson_glms, error_trace,
                "Failed to get feature configuration list");

     glms_logEvent(GLMS_LOG_LOW, "Failed to get feature configuration list");
   }
}


void
handleGlmsAdpiDumpGlmsStateDataReq(union itc_msg *sigRec)
{
   uint32_t dataBufferSizeIter = 0;
   uint32_t dataBufferSize     = 0;
   char *buf;
   GlmsBool dumpDone = GLMS_FALSE;
   uint32_t dumpStrLen;

   time_t lastInventChange  = (time_t) lm_getLastInventoryChange();
   time_t lastInventRefresh = (time_t) lm_getLastInventoryRefresh();
   time_t installationTime  = (time_t) keyFile_getInstallationTime();
   time_t euExpiration      = (time_t) eu_getExpiration();
   time_t iuExpiration      = (time_t) iu_getExpiration();
   time_t puExpiration      = (time_t) pu_getExpiration();

   char lastInventChange_s[30];
   char lastInventRefresh_s[30];
   char installationTime_s[30];
   char euExpiration_s[30];
   char iuExpiration_s[30];
   char puExpiration_s[30];

   strncpy(lastInventChange_s, ctime(&lastInventChange), 30);
   lastInventChange_s[29] = '\0';
   strncpy(lastInventRefresh_s, ctime(&lastInventRefresh), 30);
   lastInventRefresh_s[29] = '\0';
   strncpy(installationTime_s, ctime(&installationTime), 30);
   installationTime_s[29] = '\0';
   strncpy(euExpiration_s, ctime(&euExpiration), 30);
   euExpiration_s[29] = '\0';
   strncpy(iuExpiration_s, ctime(&iuExpiration), 30);
   iuExpiration_s[29] = '\0';
   strncpy(puExpiration_s, ctime(&puExpiration), 30);
   puExpiration_s[29] = '\0';

   dataBufferSize = GlmsDumpSize[dataBufferSizeIter];
   buf = malloc(dataBufferSize);
   buf[0] = '\0';

   while(dumpDone == GLMS_FALSE)
   {
      dumpStrLen = snprintf(buf, dataBufferSize,
                            "Adapter MailboxId\t: 0x%010x\n"
                            "isLicenseRefreshRunning\t: %d\n"
                            "isSubbedForMoUpdateInd\t: %d\n"
                            "logLevel\t\t: %d\n"
                            "\n"
                            "lmState\t\t\t: %d\n"
                            "fingerprint\t\t: %s\n"
                            "lastInventoryChange\t: %s"
                            "lastLicInventoryRefresh\t: %s"
                            "lmPpPendingUpdates\t: %d\n"
                            "lmSpPendingUpdates\t: %d\n"
                            "lmSendMoUpdateInd\t: %d\n"
                            "lmStartupReadStatus\t: %d %d\n"
                            "\n"
                            "sequenceNumber\t\t: %d\n"
                            "installationTime\t: %s"
                            "kfStartupReadStatus\t: %d %d\n"
                            "kfPendingUpdates\t: %d\n"
                            "kffAlarmActive\t\t: %d\n"
                            "kfSendMoUpdateInd\t: %d\n"
                            "kfInstallActionOngoing\t: %d\n"
                            "kfReportProgress\t: \n"
                            "\n"
                            "euActivationState\t: %d\n"
                            "euExpiration\t\t: %s"
                            "euActivationsLeft\t: %d\n"
                            "euPpPendingUpdates\t: %d\n"
                            "euSendMoUpdateInd\t: %d\n"
                            "euStartupReadStatus\t: %d %d\n"
                            "euWarningTimerId\t: %d\n"
                            "euExpiryTimerId\t\t: %d\n"
                            "\n"
                            "iuActivationState\t: %d\n"
                            "iuExpiration\t\t: %s"
                            "iuActivationsLeft\t: %d\n"
                            "iuPpPendingUpdates\t: %d\n"
                            "iuSendMoUpdateInd\t: %d\n"
                            "iuStartupReadStatus\t: %d %d\n"
                            "iuWarningTimerId\t: %d\n"
                            "iuExpiryTimerId\t\t: %d\n"
                            "\n"
                            "puActivationState\t: %d\n"
                            "puExpiration\t\t: %s"
                            "puActivationsLeft\t: %d\n"
                            "puPpPendingUpdates\t: %d\n"
                            "puStartupReadStatus\t: %d %d\n"
                            "puExpiryTimerId\t\t: %d\n",
                            adapter_getMid(),
                            adapter_isLicenseRefreshRunning(),
                            adapter_isSubscribedForMoUpdateInd(),
                            adapter_getLogLevel(),
                            lm_getLmState(),
                            lm_getFingerprint(),
                            lastInventChange_s,
                            lastInventRefresh_s,
                            lm_getPpPendingUpdates(),
                            lm_getSpPendingUpdates(),
                            lm_getSendMoUpdateInd(),
                            lm_getStartupReadStatus(0),
                            lm_getStartupReadStatus(1),
                            keyFile_getSequenceNumber(),
                            installationTime_s,
                            keyFile_getStartupReadStatus(0),
                            keyFile_getStartupReadStatus(1),
                            keyFile_getPendingUpdates(),
                            keyFile_getKeyFileFaultAlarmState(),
                            keyFile_getSendMoUpdateInd(),
                            keyFile_getInstallActionOngoing(),
                            /* fill kfReportProgress here */
                            eu_getActivationState(),
                            euExpiration_s,
                            eu_getActivationsLeft(),
                            eu_getPpPendingUpdates(),
                            eu_getSendMoUpdateInd(),
                            eu_getStartupReadStatus(0),
                            eu_getStartupReadStatus(1),
                            eu_getWarningTimerId(),
                            eu_getExpiryTimerId(),
                            iu_getActivationState(),
                            iuExpiration_s,
                            iu_getActivationsLeft(),
                            iu_getPpPendingUpdates(),
                            iu_getSendMoUpdateInd(),
                            iu_getStartupReadStatus(0),
                            iu_getStartupReadStatus(1),
                            iu_getWarningTimerId(),
                            iu_getExpiryTimerId(),
                            pu_getActivationState(),
                            puExpiration_s,
                            pu_getActivationsLeft(),
                            pu_getPpPendingUpdates(),
                            pu_getStartupReadStatus(0),
                            pu_getStartupReadStatus(1),
                            pu_getExpiryTimerId());

      if (dumpStrLen >= dataBufferSize &&
          dataBufferSizeIter != GLMS_LAST_DUMP_SIZE_ELEMENT)
      {
         /* Increase buffer size and redo the action */
         dataBufferSizeIter++;
         buf = realloc(buf, GlmsDumpSize[dataBufferSizeIter]);
         continue;
      }
      else
      {
         dumpDone = GLMS_TRUE;
      }
   }

   /* 'buf' now contains the dump. Send it to the adapter
      and free the allocated memory. */
   union itc_msg *sigSend = itc_alloc(sizeof(GlmsAdpiDumpGlmsStateDataRsp) + dumpStrLen,
                                      GLMS_ADPI_DUMP_GLMS_STATE_DATA_RSP);
   sigSend->glmsDumpGlmsStateDataRsp.result = GLMS_OK;
   strncpy(sigSend->glmsDumpGlmsStateDataRsp.resultInfo, "", GLMS_RESULT_INFO_LEN);
   sigSend->glmsDumpGlmsStateDataRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsDumpGlmsStateDataRsp.lastResponse = 1;
   sigSend->glmsDumpGlmsStateDataRsp.sizeOfDump = dumpStrLen + 1;
   strncpy(sigSend->glmsDumpGlmsStateDataRsp.dump, buf, dumpStrLen);
   sigSend->glmsDumpGlmsStateDataRsp.dump[dumpStrLen] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

   free(buf);
}


void
handleMonitorSig(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleMonitorSig", itc_sender(sigRec));

   /* Handle adapter monitor message as if we receive an GLMS deactivate req. */
   if(itc_sender(sigRec) == adapter_getMid())
   {
      tracepoint(com_ericsson_glms, debug_trace,
                 "Monitor signal received from adapter");
      deactivateGlms();
   }
   else
   {
      /* Remove data belonging to the client without sending
         a LfciFeatureLicenseDisconnectIndS (or Lcci..) and without
         unmonitoring the client. */
      lfci_deleteByMidAndServerRef(itc_sender(sigRec),
                                   0,
                                   GLMS_FALSE);

      lcci_deleteByMidAndServerRef(itc_sender(sigRec),
                                   0,
                                   GLMS_FALSE);
   }
}


static void
handleGlmsTimerExpiredInd(union itc_msg *sigRec)
{
   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "Received timer with clientId",
              sigRec->glmsTimerExpiredInd.clientId);

   switch(sigRec->glmsTimerExpiredInd.clientId)
   {
      case GLMS_DAILY_VALIDATION_TIMER:
         glms_logEvent(GLMS_LOG_LOW, "GLMS daily validation starting");
         lm_validateLicenses();
         return;

      case GLMS_EU_EXPIRY_WARNING_TIMER:
         eu_handleEuExpiryWarningTimer();
         return;

      case GLMS_IU_EXPIRY_WARNING_TIMER:
         iu_handleIuExpiryWarningTimer();
         return;

      case GLMS_EU_EXPIRY_TIMER:
         eu_handleEuExpiryTimer();
         return;

      case GLMS_IU_EXPIRY_TIMER:
         iu_handleIuExpiryTimer();
         return;

      case GLMS_PU_EXPIRY_TIMER:
         pu_handlePuExpiryTimer();
         return;

      case GLMS_AM_EXPIRY_TIMER:
         am_handleAmExpiryTimer();
         return;

      case GLMS_GP_EXPIRY_WARNING_TIMER:
         capacityState_handleGpWarningTimer(
	               sigRec->glmsTimerExpiredInd.timerId);
         return;

      case GLMS_GP_EXPIRY_TIMER:
         capacityState_handleGpExpiryTimer(
	              sigRec->glmsTimerExpiredInd.timerId);
         return;

      default:
         glms_logEvent(GLMS_LOG_LOW,
                       "Unknown timer type received: %d",
                       sigRec->glmsTimerExpiredInd.clientId);

         tracepoint(com_ericsson_glms, error_trace_w_int_arg,
                    "Unknown timer type",
                    sigRec->glmsTimerExpiredInd.clientId);
   }

   return;
}


/** ==================================================================== */
/**
 *   Generic License Management Solution
 */
/* ===================================================================== */
void
runGlms(void)
{
   static uint32_t anySig[] = {0};
   union itc_msg *sigRec;
   itc_mbox_id_t myMid;

   myMid = itc_create_mailbox("glms", 0);

   tracepoint(com_ericsson_glms, glms_startup,
              "########################################");
   tracepoint(com_ericsson_glms, debug_trace_w_hex_arg,
              "glms_main thread created with mailbox id", myMid);

   interfacePublished[LFCI] = 0;
   interfacePublished[LCCI] = 0;
   adapter_init(); /* Reset GLMS activation state */

   for(;;)
   {
      sigRec = itc_receive(anySig,
                           ITC_NO_TMO,
                           ITC_FROM_ALL);

      tracepoint(com_ericsson_glms, debug_trace_w_hex_arg,
                 "Received message",
                 sigRec->sigNo);

      switch(sigRec->sigNo)
      {
         /*
          *  GLMS_ADPI
          */

         case GLMS_ADPI_ACTIVATE_REQ:
            handleGlmsAdpiActivateReq(sigRec);
            break;

         case GLMS_ADPI_DEACTIVATE_REQ:
            handleGlmsAdpiDeactivateReq();
            break;

         case GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_REQ:
            handleGlmsAdpiReadMoKeyFileInformationReq(sigRec);
            break;

         case GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_REQ:
            handleGlmsAdpiReadMoInstallKeyFileReportProgressReq(sigRec);
            break;

         case GLMS_ADPI_READ_MO_LM_REQ:
            handleGlmsAdpiReadMoLmReq(sigRec);
            break;

         case GLMS_ADPI_READ_MO_LICENSE_SUPPORT_REQ:
            handleGlmsAdpiReadMoLicenseSupportReq(sigRec);
            break;

         case GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_REQ:
            handleGlmsAdpiGetFeatureStateMoListReq(sigRec);
            break;

         case GLMS_ADPI_READ_MO_FEATURE_STATE_REQ:
            handleGlmsAdpiReadMoFeatureStateReq(sigRec);
            break;

         case GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_REQ:
            handleGlmsAdpiGetFeatureKeyMoListReq(sigRec);
            break;

         case GLMS_ADPI_READ_MO_FEATURE_KEY_REQ:
            handleGlmsAdpiReadMoFeatureKeyReq(sigRec);
            break;
	    
         case GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_REQ:
            handleGlmsAdpiGetCapacityStateMoListReq(sigRec);
            break;

         case GLMS_ADPI_READ_MO_CAPACITY_STATE_REQ:
            handleGlmsAdpiReadMoCapacityStateReq(sigRec);
            break;

         case GLMS_ADPI_CREATE_GRACE_PERIOD_MO_RSP:
         case GLMS_ADPI_DELETE_GRACE_PERIOD_MO_RSP:
            break;
	    
         case GLMS_ADPI_READ_MO_GRACE_PERIOD_REQ:
            handleGlmsAdpiReadMoGracePeriodReq(sigRec);
            break;
	    
         case GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_REQ:
            handleGlmsAdpiGetCapacityKeyMoListReq(sigRec);
            break;

         case GLMS_ADPI_READ_MO_CAPACITY_KEY_REQ:
            handleGlmsAdpiReadMoCapacityKeyReq(sigRec);
            break;
	    
         case GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP:
         case GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP:
         case GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP:
         case GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP:
            handleReadingStoredParameters(sigRec);
            break;

         case GLMS_ADPI_FEATURE_CONFIGURATION_LIST_RSP:
            handleGlmsAdpiFeatureConfigurationListRsp(sigRec);
            break;

         case GLMS_ADPI_PERSISTENT_PARAMETER_SET_RSP:
            tracepoint(com_ericsson_glms, parameter_set_rsp,
                       "PersistentParameter",
                       sigRec->glmsPpSetRsp.result,
                       sigRec->glmsPpSetRsp.requestId);
            break;

         case GLMS_ADPI_SOFTWARE_PARAMETER_SET_RSP:
            tracepoint(com_ericsson_glms, parameter_set_rsp,
                       "SoftwareParameter",
                       sigRec->glmsSpSetRsp.result,
                       sigRec->glmsSpSetRsp.requestId);
            break;

         case GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_RSP:
            tracepoint(com_ericsson_glms, parameter_delete_index_rsp,
                       "PersistentParameter",
                       sigRec->glmsPpDeleteIndexRsp.result,
                       sigRec->glmsPpDeleteIndexRsp.requestId);
            break;

         case GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_RSP:
            tracepoint(com_ericsson_glms, parameter_delete_index_rsp,
                       "SoftwareParameter",
                       sigRec->glmsSpDeleteIndexRsp.result,
                       sigRec->glmsSpDeleteIndexRsp.requestId);
            break;

         case GLMS_ADPI_SET_FINGERPRINT_REQ:
            handleGlmsAdpiSetFingerprintReq(sigRec);
            break;

         case GLMS_ADPI_INSTALL_KEY_FILE_REQ:
            handleGlmsAdpiInstallKeyFileReq(sigRec);
            break;

         case GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_REQ:
            handleGlmsAdpiInstallAreaLicenseKeysReq(sigRec);
            break;

         case GLMS_ADPI_STORE_KEY_FILE_RSP:
            handleGlmsAdpiStoreKeyFileRsp(sigRec);
            break;

         case GLMS_ADPI_DOWNLOAD_KEY_FILE_RSP:
            handleGlmsAdpiDownloadKeyFileRsp(sigRec);
            break;

         case GLMS_ADPI_PKI_VERIFICATION_RSP:
            handleGlmsAdpiPkiVerificationRsp(sigRec);
            break;

         case GLMS_ADPI_SET_FEATURE_STATE_REQ:
            handleGlmsAdpiSetFeatureStateReq(sigRec);
            break;

         case GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_REQ:
            handleGlmsAdpiUpdateGracePeriodAttributesReq(sigRec);
            break;

         case GLMS_ADPI_GET_KEY_FILE_LOCATION_RSP:
            handleGlmsAdpiGetKeyFileLocationRsp(sigRec);
            break;

         case GLMS_ADPI_CREATE_FEATURE_KEY_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiCreateFeatureKeyMoRsp with result",
                       sigRec->glmsCreateFeatureKeyMoRsp.result);
            break;

         case GLMS_ADPI_DELETE_FEATURE_KEY_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiDeleteFeatureKeyMoRsp with result",
                       sigRec->glmsDeleteFeatureKeyMoRsp.result);
            break;

         case GLMS_ADPI_CREATE_FEATURE_STATE_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiCreateFeatureStateMoRsp with result",
                       sigRec->glmsCreateFeatureStateMoRsp.result);
            break;

         case GLMS_ADPI_DELETE_FEATURE_STATE_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiDeleteFeatureStateMoRsp with result",
                       sigRec->glmsDeleteFeatureStateMoRsp.result);
            break;
        
         case GLMS_ADPI_CREATE_CAPACITY_KEY_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiCreateCapacityKeyMoRsp with result",
                       sigRec->glmsCreateCapacityKeyMoRsp.result);
            break;

         case GLMS_ADPI_DELETE_CAPACITY_KEY_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiDeleteCapacityKeyMoRsp with result",
                       sigRec->glmsDeleteCapacityKeyMoRsp.result);
            break;

         case GLMS_ADPI_CREATE_CAPACITY_STATE_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiCreateCapacityStateMoRsp with result",
                       sigRec->glmsCreateCapacityStateMoRsp.result);
            break;

         case GLMS_ADPI_DELETE_CAPACITY_STATE_MO_RSP:
            tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                       "GlmsAdpiDeleteCapacityStateMoRsp with result",
                       sigRec->glmsDeleteCapacityStateMoRsp.result);
            break;
         case GLMS_ADPI_HEARTBEAT_REQ:
            handleGlmsAdpiHeartbeatReq(sigRec);
            break;

         case GLMS_ADPI_SUBSCRIBE_MO_UPDATES_REQ:
            handleGlmsAdpiSubscribeMoUpdatesReq(sigRec);
            break;

         case GLMS_ADPI_READ_EU_MO_REQ:
            handleGlmsAdpiReadEuMoReq(sigRec);
            break;

         case GLMS_ADPI_ACTIVATE_EU_REQ:
            handleGlmsAdpiActivateEuReq(sigRec);
            break;

         case GLMS_ADPI_READ_IU_MO_REQ:
            handleGlmsAdpiReadIuMoReq(sigRec);
            break;

         case GLMS_ADPI_ACTIVATE_IU_REQ:
            handleGlmsAdpiActivateIuReq(sigRec);
            break;

         case GLMS_ADPI_ACTIVATE_PU_REQ:
            handleGlmsAdpiActivatePuReq(sigRec);
            break;

         case GLMS_ADPI_DEACTIVATE_PU_REQ:
            handleGlmsAdpiDeactivatePuReq(sigRec);
            break;

         case GLMS_ADPI_READ_AM_MO_REQ:
            handleGlmsAdpiReadAmMoReq(sigRec);
            break;

         case GLMS_ADPI_REFRESH_LICENSE_INVENTORY_REQ:
            handleGlmsAdpiRefreshLicenseInventoryReq(sigRec);
            break;

         case GLMS_ADPI_UPDATE_AREA_ID_REQ:
            handleGlmsAdpiUpdateAreaIdReq(sigRec);
            break;

         case GLMS_ADPI_STATE_MO_AUDIT_REQ:
            handleGlmsAdpiStateMoAuditReq(sigRec);
            break;

         case GLMS_ADPI_DUMP_GLMS_STATE_DATA_REQ:
            handleGlmsAdpiDumpGlmsStateDataReq(sigRec);
            break;

         case GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_REQ:
            handleGlmsAdpiDumpLfciClientDataReq(sigRec);
            break;

         case GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_REQ:
            handleGlmsAdpiDumpLcciClientDataReq(sigRec);
            break;

         case GLMS_ADPI_DUMP_FEATURE_STATE_DATA_REQ:
            handleGlmsAdpiDumpFeatureStateDataReq(sigRec);
            break;

         case GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_REQ:
            handleGlmsAdpiDumpCapacityStateDataReq(sigRec);
            break;

            /*
             *  LFCI
             */

         case LFCI_CONN_TO_SERVER_REQ:
            handleLfciConnToServerReq(sigRec);
            break;

         case LFCI_FEATURE_LICENSE_SUBSCRIBE_REQ:
            handleLfciFeatureLicenseSubscribeReq(sigRec);
            break;

         case LFCI_FEATURE_LICENSE_UNSUBSCRIBE_REQ:
            handleLfciFeatureLicenseUnsubscribeReq(sigRec);
            break;

            /*
             *  LCCI
             */

         case LCCI_CONN_TO_SERVER_REQ:
            handleLcciConnToServerReq(sigRec);
            break;

         case LCCI_CAPACITY_LICENSE_SUBSCRIBE_REQ:
            handleLcciCapacityLicenseSubscribeReq(sigRec);
            break;

         case LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_REQ:
            handleLcciCapacityLicenseUnsubscribeReq(sigRec);
            break;

         case LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD:
            handleLcciCapacityLicenseGpActivatedFwd(sigRec);
            break;

         /*
          *  OTHER
          */

         case GLMS_TIMER_EXPIRED_IND:
            handleGlmsTimerExpiredInd(sigRec);
            break;

         case ITC_MONITOR_DEFAULT_NO:
            handleMonitorSig(sigRec);
            break;

         default:
            glms_logEvent(GLMS_LOG_LOW,
                          "Unknown message received in GLMS main loop: 0x%x",
                          sigRec->sigNo);
            
            tracepoint(com_ericsson_glms, error_trace_w_hex_arg,
                       "Unknown message received in GLMS main loop",
                       sigRec->sigNo);
            break;
      }

      itc_free(&sigRec);
   }
}

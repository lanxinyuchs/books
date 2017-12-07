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
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "com_ericsson_glms.h"

#include "lihi/licenseFeatureControlI.sig"
#include "glmsadpi/glmsDataTypes.h"
#include "glmsUtils.h"
#include "glms_main.h"
#include "lihi_feature.h"
#include "clients.h"
#include "data_featureState.h"
#include "data_featureKey.h"


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

   /* LIHI - LFCI */
   #include "lihi/licenseFeatureControlIUnionContent.h"
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static void
lfci_sendLfciFeatureLicenseSubscribeCfm(LfciClient *lfciClient,
                                        union itc_msg *sigReq);
static void
lfci_sendLfciFeatureLicenseSubscribe2Cfm(LfciClient *lfciClient,
                                         union itc_msg *sigReq);

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
handleLfciConnToServerReq(union itc_msg *sig)
{
   union itc_msg *sigRsp;
   itc_mbox_id_t clientMid;
   uint32_t      negotiatedPv, i;
   int32_t       clientServerRef;
   LfciClient    *lfciClient;

   clientMid     = itc_sender(sig);
   negotiatedPv  = 0;
   clientServerRef = rd32(sig->lfciConnToServerReq.addressInfo.serverRef);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleLfciConnToServerReq", clientMid);

   if(!isLihiPublished())
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "New LFCI client rejected: LFCI not published");

      sigRsp = itc_alloc(sizeof(LfciConnToServerRejS),
                     LFCI_CONN_TO_SERVER_REJ);
      sigRsp->lfciConnToServerRej.addressInfo.clientRef =
         sig->lfciConnToServerReq.addressInfo.clientRef;
      sigRsp->lfciConnToServerRej.addressInfo.serverRef = wr32(0);
      sprintf(sigRsp->lfciConnToServerRej.errorMsg,
              "LFCI is not published");
      itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
      return;
   }

   for(i=0; 
       i < rd32(sig->lfciConnToServerReq.sizeOfProtocolRevisions);
       i++)
   {
      if(rd32(sig->lfciConnToServerReq.protocolRevisions[i]) ==
         LICENSE_FEATURE_CONTROL_I_VERSION_1 ||
         rd32(sig->lfciConnToServerReq.protocolRevisions[i]) ==
         LICENSE_FEATURE_CONTROL_I_VERSION_2)
      {
         negotiatedPv = rd32(sig->lfciConnToServerReq.protocolRevisions[i]);
         break;
      }
   }

   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "Negotiated PV", negotiatedPv);

   if(negotiatedPv == 0)
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "New LFCI client rejected: failed PV negotiation");

      sigRsp = itc_alloc(sizeof(LfciConnToServerRejS),
                     LFCI_CONN_TO_SERVER_REJ);
      sigRsp->lfciConnToServerRej.addressInfo.clientRef =
         sig->lfciConnToServerReq.addressInfo.clientRef;
      sigRsp->lfciConnToServerRej.addressInfo.serverRef = wr32(0);
      sprintf(sigRsp->lfciConnToServerRej.errorMsg,
              "Failed to negotiate a PV. Highest allowed PV is %d",
              LICENSE_FEATURE_CONTROL_I_VERSION_2);
      itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
      return;
   }

   lfciClient = lfci_create(clientMid, negotiatedPv, clientServerRef);

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "LFCI client connected with PV %d and serverRef %d, from MID 0x%x",
                 negotiatedPv,
                 lfci_getServerRef(lfciClient),
                 clientMid);

   sigRsp = itc_alloc(sizeof(LfciConnToServerCfmS),
                  LFCI_CONN_TO_SERVER_CFM);
   sigRsp->lfciConnToServerCfm.addressInfo.clientRef =
      sig->lfciConnToServerReq.addressInfo.clientRef;
   sigRsp->lfciConnToServerCfm.addressInfo.serverRef =
      wr32(lfci_getServerRef(lfciClient));
   sigRsp->lfciConnToServerCfm.protocolRevision = wr32(lfci_getPv(lfciClient));
   itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
}


static void
disconnectLfciClient(itc_mbox_id_t clientMid,
                     int32_t clientRef,
                     int32_t serverRef,
                     char *errorInfo)
{
   union itc_msg *sig;
   LfciClient   *lfciClient;
   int32_t clientServerRef;
   
   lfciClient = lfci_findByMidAndServerRef(clientMid, serverRef);
   if(lfciClient != NULL)
   {
      clientServerRef = lfci_getClientServerRef(lfciClient);
   }
   else
   {
      clientServerRef = -1;
   }

   tracepoint(com_ericsson_glms, disconnect_client,
              "Disconnect LFCI client",
              clientMid, clientRef, serverRef, errorInfo);

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "Disconnecting LFCI client with serverRef %d, from MID 0x%x",
                 lfci_getServerRef(lfciClient),
                 clientMid);

   sig = itc_alloc(sizeof(LfciFeatureLicenseDisconnectIndS),
               LFCI_FEATURE_LICENSE_DISCONNECT_IND);
   sig->lfciFeatureLicenseDisconnectInd.addressInfo.clientRef = wr32(clientRef);
   sig->lfciFeatureLicenseDisconnectInd.addressInfo.serverRef = wr32(clientServerRef);
   strncpy(sig->lfciFeatureLicenseDisconnectInd.errorInfo,
           errorInfo,
           MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO);
   sig->lfciFeatureLicenseDisconnectInd.errorInfo
      [MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO - 1] = '\0';
   itc_send(&sig, clientMid, ITC_MY_MBOX);

   lfci_deleteByMidAndServerRef(clientMid, serverRef, GLMS_TRUE);
}


void
disconnectAndDeleteAllLfciClients(char *errorInfo)
{
   union itc_msg *sig;
   LfciClient   *lfciClient;
   LihiClient   *lihiClient, *nextLihiClient;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "disconnectAndDeleteAllLfciClients", errorInfo);

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "Disconnecting all LFCI clients");

   for(lihiClient = client_getFirst();
       lihiClient != NULL;
       )
   {
      nextLihiClient = client_getNext(lihiClient);

      for(lfciClient = lfci_getFirst(lihiClient);
          lfciClient != NULL;
          lfciClient = lfci_getNext(lfciClient))
      {
         sig = itc_alloc(sizeof(LfciFeatureLicenseDisconnectIndS),
                     LFCI_FEATURE_LICENSE_DISCONNECT_IND);
         sig->lfciFeatureLicenseDisconnectInd.addressInfo.clientRef = wr32(0);
         sig->lfciFeatureLicenseDisconnectInd.addressInfo.serverRef =
            wr32(lfci_getClientServerRef(lfciClient));
         strncpy(sig->lfciFeatureLicenseDisconnectInd.errorInfo,
                 errorInfo,
                 MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO);
         sig->lfciFeatureLicenseDisconnectInd.errorInfo
            [MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO - 1] = '\0';
         itc_send(&sig, client_getMid(lihiClient), ITC_MY_MBOX);
      }

      lfci_deleteByMidAndServerRef(client_getMid(lihiClient),
                                   0, /* 0 means delete all LFCI clients from the LIHI client */
                                   GLMS_TRUE); /* Unmonitor client */
      
      lihiClient = nextLihiClient;
   }
}


void
handleLfciFeatureLicenseUnsubscribeReq(union itc_msg *sig)
{
   union itc_msg  *sigRsp;
   uint32_t       i;
   GlmsBool       clearSubscriptionResult = GLMS_FALSE;
   char           errorInfo[MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO];
   itc_mbox_id_t  clientMid;
   LfciClient    *lfciClient;
   uint32_t       clientRef;
   int32_t        serverRef;


   if(sig == NULL)
   {
      return;
   }
   if(sig->sigNo != LFCI_FEATURE_LICENSE_UNSUBSCRIBE_REQ)
   {
      return;
   }

   serverRef = rd32(sig->lfciFeatureLicenseUnsubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sig->lfciFeatureLicenseUnsubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sig);

   tracepoint(com_ericsson_glms, handleLfciFeatureLicenseUnsubscribeReq,
              clientMid, clientRef, serverRef);

   if(!isLihiPublished())
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "LFCI is not published to name server and "
               "thus not ready to handle unsubscribe requests");
      disconnectLfciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
   }

   lfciClient = lfci_findByMidAndServerRef(clientMid, serverRef);
   if(lfciClient == NULL)
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "No LFCI client exist with MBOXID 0x%x and ServerRef %d",
               clientMid,
               serverRef);
      disconnectLfciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);

      return;
   }

   sigRsp = itc_alloc(sizeof(LfciFeatureLicenseUnsubscribeCfmS) +
                  rd32(sig->lfciFeatureLicenseUnsubscribeReq.sizeOfLicenseData) *
                  sizeof(LfciKeyDataS),
                  LFCI_FEATURE_LICENSE_UNSUBSCRIBE_CFM);
   sigRsp->lfciFeatureLicenseUnsubscribeCfm.addressInfo.clientRef = wr32(clientRef);
   sigRsp->lfciFeatureLicenseUnsubscribeCfm.addressInfo.serverRef =
      wr32(lfci_getClientServerRef(lfciClient));
   sigRsp->lfciFeatureLicenseUnsubscribeCfm.sizeOfLicenseInfo =
      sig->lfciFeatureLicenseUnsubscribeReq.sizeOfLicenseData;

   for(i = 0;
       i < rd32(sig->lfciFeatureLicenseUnsubscribeReq.sizeOfLicenseData);
       i++)
   {
      strncpy(sigRsp->lfciFeatureLicenseUnsubscribeCfm.licenseInfo[i].licenseKeyId,
              sig->lfciFeatureLicenseUnsubscribeReq.licenseData[i].licenseKeyId,
              MAX_SIZE_OF_LFCI_KEY_DATA_S_LICENSE_KEY_ID);
      sigRsp->lfciFeatureLicenseUnsubscribeCfm.licenseInfo[i].
         licenseKeyId[MAX_SIZE_OF_LFCI_KEY_DATA_S_LICENSE_KEY_ID - 1] = '\0';

      if(featureState_clearSubscriptionsByKeyIdMidAndServerRef(
                    sig->lfciFeatureLicenseUnsubscribeReq.licenseData[i].licenseKeyId,
                    itc_sender(sig),
                    lfci_getServerRef(lfciClient)) == GLMS_TRUE)
      {
         clearSubscriptionResult = GLMS_TRUE;

         glms_logEvent(GLMS_LOG_MEDIUM,
                       "Unsubscribing LFCI client from %s. ServerRef %d, MID 0x%x",
                       sig->lfciFeatureLicenseUnsubscribeReq.licenseData[i].licenseKeyId,
                       lfci_getServerRef(lfciClient),
                       clientMid);
      }
   }

   if(clearSubscriptionResult == GLMS_TRUE)
   {
      itc_send(&sigRsp, itc_sender(sig), ITC_MY_MBOX);
   }
   else
   {
      itc_free(&sigRsp);
      disconnectLfciClient(itc_sender(sig),
                           clientRef,
                           serverRef,
                           "Unsubscribe request for a FeatureKey that was not subscribed to");
   }
}


void
handleLfciFeatureLicenseSubscribeReq(union itc_msg *sig)
{
   itc_mbox_id_t  clientMid;
   LfciClient    *lfciClient;
   char           errorInfo[MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO];
   uint32_t       clientRef, i;
   int32_t        serverRef;
   LfciLicenseDataS *lfciSubData;

   if(sig == NULL)
   {
      return;
   }
   if(sig->sigNo != LFCI_FEATURE_LICENSE_SUBSCRIBE_REQ)
   {
      return;
   }

   serverRef = rd32(sig->lfciFeatureLicenseSubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sig->lfciFeatureLicenseSubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sig);

   tracepoint(com_ericsson_glms, handleLfciFeatureLicenseSubscribeReq,
              clientMid, clientRef, serverRef);

   if(!isLihiPublished())
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "LFCI is not published to name server and "
               "thus not ready to handle subscribe requests");
      disconnectLfciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
   }

   lfciClient = lfci_findByMidAndServerRef(clientMid, serverRef);
   if(lfciClient == NULL)
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "No LFCI client exist with MBOXID 0x%x and ServerRef %d",
               clientMid,
               serverRef);
      disconnectLfciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);

      return;
   }

   lfciSubData = sig->lfciFeatureLicenseSubscribeReq.licenseData;
   for(i = 0;
       i < rd32(sig->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      if(verifyMoStringValidity(lfciSubData[i].licenseKeyId) == GLMS_FALSE ||
         verifyMoStringValidity(lfciSubData[i].licenseMoId) == GLMS_FALSE ||
         verifyMoStringValidity(lfciSubData[i].licenseName) == GLMS_FALSE)
      {
         snprintf(errorInfo,
                  MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
                  "Faulty subscriber data; licenseKeyId=%s, licenseMoId=%s, licenseName=%s",
                  lfciSubData[i].licenseKeyId,
                  lfciSubData[i].licenseMoId,
                  lfciSubData[i].licenseName);
         disconnectLfciClient(clientMid,
                              clientRef,
                              serverRef,
                              errorInfo);
         return;
      }
   }

   for(i = 0;
       i < rd32(sig->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      featureState_addSubscriber(lfciSubData[i].licenseKeyId,
                                 lfciSubData[i].licenseMoId,
                                 lfciSubData[i].licenseName,
                                 clientMid,
                                 serverRef,
                                 clientRef);
   }

   handleLicenseKeyCloseToExpirationAlarm();

   if(lfci_getPv(lfciClient) == LICENSE_FEATURE_CONTROL_I_VERSION_1)
   {
      lfci_sendLfciFeatureLicenseSubscribeCfm(lfciClient, sig);
   }
   else
   {
      lfci_sendLfciFeatureLicenseSubscribe2Cfm(lfciClient, sig);
   }
}

static void
lfci_sendLfciFeatureLicenseSubscribeCfm(LfciClient *lfciClient,
                                        union itc_msg *sigReq)
{
   char              tmpBuf[MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_KEY_ID + 60];
   union itc_msg    *sigRsp;
   LfciLicenseDataS *lfciSubData;
   LfciLicenseInfoS *lfciSubInfo;
   itc_mbox_id_t     clientMid;
   uint32_t          clientRef, i;
   int32_t           serverRef;
   char             *logStr;

   serverRef = rd32(sigReq->lfciFeatureLicenseSubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sigReq->lfciFeatureLicenseSubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sigReq);

   sigRsp = itc_alloc(sizeof(LfciFeatureLicenseSubscribeCfmS) +
                  rd32(sigReq->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData)
                    * sizeof(LfciLicenseInfoS),
                  LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM);
   sigRsp->lfciFeatureLicenseSubscribeCfm.addressInfo.clientRef = wr32(clientRef);
   sigRsp->lfciFeatureLicenseSubscribeCfm.addressInfo.serverRef =
      wr32(lfci_getClientServerRef(lfciClient));
   sigRsp->lfciFeatureLicenseSubscribeCfm.sizeOfLicenseInfo =
      sigReq->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData;


   lfciSubData = sigReq->lfciFeatureLicenseSubscribeReq.licenseData;
   lfciSubInfo = sigRsp->lfciFeatureLicenseSubscribeCfm.licenseInfo;
   for(i = 0;
       i < rd32(sigReq->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      if(featureState_fillLfciLicenseInfoByKeyId(&(lfciSubInfo[i]),
                                                 lfciSubData[i].licenseKeyId) == GLMS_FALSE)
      {
         itc_free(&sigRsp);
         disconnectLfciClient(clientMid,
                              clientRef,
                              serverRef,
                              "Failed to find FeatureKey");
         return;
      }
   }


   logStr = malloc(100 +
                   (rd32(sigRsp->lfciFeatureLicenseSubscribeCfm.sizeOfLicenseInfo) *
                    (MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_KEY_ID + 60)));
   sprintf(logStr,
           "Sending LFCI subscribe cfm to MID 0x%x serverRef %d;",
           clientMid,
           serverRef);

   for(i = 0;
       i < rd32(sigRsp->lfciFeatureLicenseSubscribeCfm.sizeOfLicenseInfo);
       i++)
   {
      if(rd32(sigRsp->lfciFeatureLicenseSubscribeCfm.sizeOfLicenseInfo) > 1)
      {
         strcat(logStr, "\n   -");
      }

      sprintf(tmpBuf,
              " key = %s, licState = %d",
              sigRsp->lfciFeatureLicenseSubscribeCfm.licenseInfo[i].licenseKeyId,
              rd32(sigRsp->lfciFeatureLicenseSubscribeCfm.licenseInfo[i].licenseState));

      strcat(logStr, tmpBuf);
   }

   glms_logEvent(GLMS_LOG_MEDIUM, logStr);
   free(logStr);

   itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
}

static void
lfci_sendLfciFeatureLicenseSubscribe2Cfm(LfciClient *lfciClient,
                                         union itc_msg *sigReq)
{
   char               tmpBuf[MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_KEY_ID + 60];
   union itc_msg     *sigRsp;
   LfciLicenseDataS  *lfciSubData;
   LfciLicenseInfo2S *lfciSubInfo;
   itc_mbox_id_t      clientMid;
   uint32_t           clientRef, i;
   int32_t            serverRef;
   char              *logStr;

   serverRef = rd32(sigReq->lfciFeatureLicenseSubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sigReq->lfciFeatureLicenseSubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sigReq);

   sigRsp = itc_alloc(sizeof(LfciFeatureLicenseSubscribe2CfmS) +
                  rd32(sigReq->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData)
                    * sizeof(LfciLicenseInfoS),
                  LFCI_FEATURE_LICENSE_SUBSCRIBE2_CFM);
   sigRsp->lfciFeatureLicenseSubscribe2Cfm.addressInfo.clientRef = wr32(clientRef);
   sigRsp->lfciFeatureLicenseSubscribe2Cfm.addressInfo.serverRef =
      wr32(lfci_getClientServerRef(lfciClient));
   sigRsp->lfciFeatureLicenseSubscribe2Cfm.sizeOfLicenseInfo =
      sigReq->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData;


   lfciSubData = sigReq->lfciFeatureLicenseSubscribeReq.licenseData;
   lfciSubInfo = sigRsp->lfciFeatureLicenseSubscribe2Cfm.licenseInfo;
   for(i = 0;
       i < rd32(sigReq->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      if(featureState_fillLfciLicenseInfo2ByKeyId(&(lfciSubInfo[i]),
                                                  lfciSubData[i].licenseKeyId) == GLMS_FALSE)
      {
         itc_free(&sigRsp);
         disconnectLfciClient(clientMid,
                              clientRef,
                              serverRef,
                              "Failed to find FeatureKey");
         return;
      }
   }


   logStr = malloc(100 +
                   (rd32(sigRsp->lfciFeatureLicenseSubscribe2Cfm.sizeOfLicenseInfo) *
                    (MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_KEY_ID + 60)));
   sprintf(logStr,
           "Sending LFCI subscribe cfm to MID 0x%x serverRef %d;",
           clientMid,
           serverRef);

   for(i = 0;
       i < rd32(sigRsp->lfciFeatureLicenseSubscribe2Cfm.sizeOfLicenseInfo);
       i++)
   {
      if(rd32(sigRsp->lfciFeatureLicenseSubscribe2Cfm.sizeOfLicenseInfo) > 1)
      {
         strcat(logStr, "\n   -");
      }

      sprintf(tmpBuf,
              " key = %s, licState = %d",
              sigRsp->lfciFeatureLicenseSubscribe2Cfm.licenseInfo[i].licenseKeyId,
              rd32(sigRsp->lfciFeatureLicenseSubscribe2Cfm.licenseInfo[i].licenseState));

      strcat(logStr, tmpBuf);
   }

   glms_logEvent(GLMS_LOG_MEDIUM, logStr);
   free(logStr);

   itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
}

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

#include "lihi/licenseCapacityControlI.sig"
#include "glmsadpi/glmsDataTypes.h"
#include "glmsUtils.h"
#include "glms_main.h"
#include "lihi_capacity.h"
#include "clients.h"
#include "data_capacityState.h"
#include "data_capacityKey.h"


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

   /* LIHI - LCCI */
   #include "lihi/licenseCapacityControlIUnionContent.h"
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
lcci_sendLcciCapacityLicenseSubscribeCfm(LcciClient *lcciClient,
                                         union itc_msg *sigReq);

static void
lcci_sendLcciCapacityLicenseSubscribe2Cfm(LcciClient *lcciClient,
                                          union itc_msg *sigReq);

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

void
handleLcciConnToServerReq(union itc_msg *sig)
{
   union itc_msg *sigRsp;
   itc_mbox_id_t clientMid;
   uint32_t      negotiatedPv, i;
   int32_t       clientServerRef;
   LcciClient    *lcciClient;

   clientMid     = itc_sender(sig);
   negotiatedPv  = 0;
   clientServerRef = rd32(sig->lcciConnToServerReq.addressInfo.serverRef);

   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "handleLcciConnToServerReq", clientMid);

   if(!isLihiPublished())
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "New LCCI client rejected: LCCI not published");

      sigRsp = itc_alloc(sizeof(LcciConnToServerRejS),
                     LCCI_CONN_TO_SERVER_REJ);
      sigRsp->lcciConnToServerRej.addressInfo.clientRef =
         sig->lcciConnToServerReq.addressInfo.clientRef;
      sigRsp->lcciConnToServerRej.addressInfo.serverRef = wr32(0);
      sprintf(sigRsp->lcciConnToServerRej.errorMsg,
              "LCCI is not published");
      itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
      return;
   }

   for(i=0; i < rd32(sig->lcciConnToServerReq.sizeOfProtocolRevisions); i++)
   {
      if(rd32(sig->lcciConnToServerReq.protocolRevisions[i]) ==
         LICENSE_CAPACITY_CONTROL_I_VERSION_1 ||
         rd32(sig->lcciConnToServerReq.protocolRevisions[i]) ==
         LICENSE_CAPACITY_CONTROL_I_VERSION_2)
      {
         negotiatedPv = rd32(sig->lcciConnToServerReq.protocolRevisions[i]);
         break;
      }
   }

   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "Negotiated PV", negotiatedPv);

   if(negotiatedPv == 0)
   {
      glms_logEvent(GLMS_LOG_LOW,
                    "New LCCI client rejected: failed PV negotiation");

      sigRsp = itc_alloc(sizeof(LcciConnToServerRejS),
                     LCCI_CONN_TO_SERVER_REJ);
      sigRsp->lcciConnToServerRej.addressInfo.clientRef =
         sig->lcciConnToServerReq.addressInfo.clientRef;
      sigRsp->lcciConnToServerRej.addressInfo.serverRef = wr32(0);
      sprintf(sigRsp->lcciConnToServerRej.errorMsg,
              "Failed to negotiate a PV. Highest allowed PV is %d",
              LICENSE_CAPACITY_CONTROL_I_VERSION_2);
      itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
      return;
   }

   lcciClient = lcci_create(clientMid, negotiatedPv, clientServerRef);

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "LCCI client connected with PV %d and serverRef %d, from MID 0x%x",
                 negotiatedPv,
                 lcci_getServerRef(lcciClient),
                 clientMid);

   sigRsp = itc_alloc(sizeof(LcciConnToServerCfmS),
                  LCCI_CONN_TO_SERVER_CFM);
   sigRsp->lcciConnToServerCfm.addressInfo.clientRef =
      sig->lcciConnToServerReq.addressInfo.clientRef;
   sigRsp->lcciConnToServerCfm.addressInfo.serverRef =
     wr32(lcci_getServerRef(lcciClient));
   sigRsp->lcciConnToServerCfm.protocolRevision = wr32(lcci_getPv(lcciClient));
   itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
}


static void
disconnectLcciClient(itc_mbox_id_t clientMid,
                     int32_t clientRef,
                     int32_t serverRef,
                     char *errorInfo)
{
   union itc_msg *sig;
   LcciClient   *lcciClient;
   int32_t clientServerRef;
   
   lcciClient = lcci_findByMidAndServerRef(clientMid, serverRef);
   if(lcciClient != NULL)
   {
      clientServerRef = lcci_getClientServerRef(lcciClient);
   }
   else
   {
      clientServerRef = -1;
   }

   tracepoint(com_ericsson_glms, disconnect_client,
              "Disconnect LCCI client",
              clientMid, clientRef, serverRef, errorInfo);

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "Disconnecting LCCI client with serverRef %d, from MID 0x%x",
                 lcci_getServerRef(lcciClient),
                 clientMid);

   sig = itc_alloc(sizeof(LcciCapacityLicenseDisconnectIndS),
               LCCI_CAPACITY_LICENSE_DISCONNECT_IND);
   sig->lcciCapacityLicenseDisconnectInd.addressInfo.clientRef = wr32(clientRef);
   sig->lcciCapacityLicenseDisconnectInd.addressInfo.serverRef = wr32(clientServerRef);
   strncpy(sig->lcciCapacityLicenseDisconnectInd.errorInfo,
           errorInfo,
           MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO);
   sig->lcciCapacityLicenseDisconnectInd.errorInfo
      [MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO - 1] = '\0';
   itc_send(&sig, clientMid, ITC_MY_MBOX);

   lcci_deleteByMidAndServerRef(clientMid, serverRef, GLMS_TRUE);
}


void
disconnectAndDeleteAllLcciClients(char *errorInfo)
{
   union itc_msg *sig;
   LcciClient   *lcciClient;
   LihiClient   *lihiClient, *nextLihiClient;

   tracepoint(com_ericsson_glms, call_to_function_w_str_arg,
              "disconnectAndDeleteAllLcciClients", errorInfo);

   glms_logEvent(GLMS_LOG_MEDIUM,
                 "Disconnecting all LCCI clients");

   for(lihiClient = client_getFirst();
       lihiClient != NULL;
       )
   {
      nextLihiClient = client_getNext(lihiClient);
      
      for(lcciClient = lcci_getFirst(lihiClient);
          lcciClient != NULL;
          lcciClient = lcci_getNext(lcciClient))
      {
         sig = itc_alloc(sizeof(LcciCapacityLicenseDisconnectIndS),
                     LCCI_CAPACITY_LICENSE_DISCONNECT_IND);
         sig->lcciCapacityLicenseDisconnectInd.addressInfo.clientRef = wr32(0);
         sig->lcciCapacityLicenseDisconnectInd.addressInfo.serverRef =
            wr32(lcci_getClientServerRef(lcciClient));
         strncpy(sig->lcciCapacityLicenseDisconnectInd.errorInfo,
                 errorInfo,
                 MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO);
         sig->lcciCapacityLicenseDisconnectInd.errorInfo
            [MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO - 1] = '\0';
         itc_send(&sig, client_getMid(lihiClient), ITC_MY_MBOX);

      }   
      
      lcci_deleteByMidAndServerRef(client_getMid(lihiClient),
                                   0,
                                   GLMS_TRUE); /* Unmonitor client */
      lihiClient = nextLihiClient;
   }
}


void
handleLcciCapacityLicenseUnsubscribeReq(union itc_msg *sig)
{
   union itc_msg  *sigRsp;
   uint32_t       i;
   GlmsBool       clearSubscriptionResult = GLMS_FALSE;
   char           errorInfo[MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO];
   itc_mbox_id_t  clientMid;
   LcciClient    *lcciClient;
   uint32_t       clientRef;
   int32_t        serverRef;

   if(sig == NULL)
   {
      return;
   }
   if(sig->sigNo != LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_REQ)
   {
      return;
   }

   serverRef = rd32(sig->lcciCapacityLicenseUnsubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sig->lcciCapacityLicenseUnsubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sig);

   tracepoint(com_ericsson_glms, handleLcciCapacityLicenseUnsubscribeReq,
              clientMid, clientRef, serverRef);

   if(!isLihiPublished())
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "LCCI is not published to name server and "
               "thus not ready to handle unsubscribe requests");
      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
   }

   lcciClient = lcci_findByMidAndServerRef(clientMid, serverRef);
   if(lcciClient == NULL)
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "No LCCI client exist with MBOXID 0x%x and ServerRef %d",
               clientMid,
               serverRef);
      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);

      return;
   }

   sigRsp = itc_alloc(sizeof(LcciCapacityLicenseUnsubscribeCfmS) +
                      rd32(sig->lcciCapacityLicenseUnsubscribeReq.sizeOfLicenseData) *
                      sizeof(LcciKeyDataS),
                      LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_CFM);
   sigRsp->lcciCapacityLicenseUnsubscribeCfm.addressInfo.clientRef = wr32(clientRef);
   sigRsp->lcciCapacityLicenseUnsubscribeCfm.addressInfo.serverRef =
      wr32(lcci_getClientServerRef(lcciClient));
   sigRsp->lcciCapacityLicenseUnsubscribeCfm.sizeOfLicenseInfo =
      sig->lcciCapacityLicenseUnsubscribeReq.sizeOfLicenseData;

   for(i = 0;
       i < rd32(sig->lcciCapacityLicenseUnsubscribeReq.sizeOfLicenseData);
       i++)
   {
      strncpy(sigRsp->lcciCapacityLicenseUnsubscribeCfm.licenseInfo[i].licenseKeyId,
              sig->lcciCapacityLicenseUnsubscribeReq.licenseData[i].licenseKeyId,
              MAX_SIZE_OF_LCCI_KEY_DATA_S_LICENSE_KEY_ID);
      sigRsp->lcciCapacityLicenseUnsubscribeCfm.licenseInfo[i].
         licenseKeyId[MAX_SIZE_OF_LCCI_KEY_DATA_S_LICENSE_KEY_ID - 1] = '\0';

      glms_logEvent(GLMS_LOG_MEDIUM,
                    "Unsubscribing LCCI client from %s. ServerRef %d, MID 0x%x",
                    sig->lcciCapacityLicenseUnsubscribeReq.licenseData[i].licenseKeyId,
                    lcci_getServerRef(lcciClient),
                    clientMid);

      if(capacityState_clearSubscriptionsByKeyIdMidAndServerRef(
                    sig->lcciCapacityLicenseUnsubscribeReq.licenseData[i].licenseKeyId,
                    itc_sender(sig),
                    lcci_getServerRef(lcciClient)) == GLMS_TRUE)
      {
         clearSubscriptionResult = GLMS_TRUE;
      }
   }

   if(clearSubscriptionResult == GLMS_TRUE)
   {
      itc_send(&sigRsp, itc_sender(sig), ITC_MY_MBOX);
   }
   else
   {
      itc_free(&sigRsp);
      disconnectLcciClient(itc_sender(sig),
                           clientRef,
                           serverRef,
                           "Unsubscribe request for a capacity key that was not subscribed to");
   }
}

void
handleLcciCapacityLicenseGpActivatedFwd(union itc_msg *sig)
{  
   char           errorInfo[MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO];
   itc_mbox_id_t  clientMid;
   LcciClient    *lcciClient;
   uint32_t       clientRef;
   int32_t        serverRef;
   CapacityState *cs;
   
   if(sig == NULL)
   {
      return;
   }
   if(sig->sigNo != LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD)
   {
      return;
   }

   serverRef = rd32(sig->lcciCapacityLicenseGpActivatedFwd.addressInfo.serverRef);
   clientRef = rd32(sig->lcciCapacityLicenseGpActivatedFwd.addressInfo.clientRef);
   clientMid = itc_sender(sig);

   tracepoint(com_ericsson_glms, handleLcciCapacityLicenseGpActivated,
              clientMid, clientRef, serverRef);

   if(!isLihiPublished())
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "LCCI is not published to name server and "
               "thus not ready to handle GP activations");
      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
   }

   lcciClient = lcci_findByMidAndServerRef(clientMid, serverRef);
   
   if(lcciClient == NULL)
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "No LCCI client exist with MBOXID 0x%x and ServerRef %d",
               clientMid,
               serverRef);

      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
      
      return;
   }

   cs = capacityState_findByKeyId(sig->lcciCapacityLicenseGpActivatedFwd.licenseKeyId);
   
   if(cs == NULL)
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "Capacity State MO not found when attempting to activate GP for %s",
               sig->lcciCapacityLicenseGpActivatedFwd.licenseKeyId);

      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
   }
   
   if(capacityState_getGracePeriodAvailable(cs) == 0)
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "Grace Period Not Available for %s",
               sig->lcciCapacityLicenseGpActivatedFwd.licenseKeyId);

      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
      return;
   }

   capacityState_activateGp(cs);
}

void
handleLcciCapacityLicenseSubscribeReq(union itc_msg *sig)
{
   itc_mbox_id_t  clientMid;
   LcciClient    *lcciClient;
   char           errorInfo[MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO];
   uint32_t       clientRef, i;
   int32_t        serverRef;
   LcciLicenseDataS *lcciSubData;

   if(sig == NULL)
   {
      return;
   }
   if(sig->sigNo != LCCI_CAPACITY_LICENSE_SUBSCRIBE_REQ)
   {
      return;
   }
  
   serverRef = rd32(sig->lcciCapacityLicenseSubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sig->lcciCapacityLicenseSubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sig);

   tracepoint(com_ericsson_glms, handleLcciCapacityLicenseSubscribeReq,
              clientMid, clientRef, serverRef);

   if(!isLihiPublished())
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "LCCI is not published to name server and "
               "thus not ready to handle subscribe requests");
      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);
   }

   lcciClient = lcci_findByMidAndServerRef(clientMid, serverRef);
   
   if(lcciClient == NULL)
   {
      snprintf(errorInfo,
               MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
               "No LCCI client exist with MBOXID 0x%x and ServerRef %d",
               clientMid,
               serverRef);
      disconnectLcciClient(clientMid,
                           clientRef,
                           serverRef,
                           errorInfo);

      return;
   }

   lcciSubData = sig->lcciCapacityLicenseSubscribeReq.licenseData;
   for(i = 0;
       i < rd32(sig->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      if(verifyMoStringValidity(lcciSubData[i].licenseKeyId) == GLMS_FALSE ||
         verifyMoStringValidity(lcciSubData[i].licenseMoId) == GLMS_FALSE ||
         verifyMoStringValidity(lcciSubData[i].licenseName) == GLMS_FALSE)
      {
         snprintf(errorInfo,
                  MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO,
                  "Faulty subscriber data; licenseKeyId=%s, licenseMoId=%s, licenseName=%s",
                  lcciSubData[i].licenseKeyId,
                  lcciSubData[i].licenseMoId,
                  lcciSubData[i].licenseName);
         disconnectLcciClient(clientMid,
                              clientRef,
                              serverRef,
                              errorInfo);

         return;
      }
   }

   for(i = 0;
       i < rd32(sig->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      capacityState_addSubscriber(lcciSubData[i].licenseKeyId,
                                  lcciSubData[i].licenseMoId,
                                  lcciSubData[i].capacityUnit,
                                  lcciSubData[i].licenseName,
                                  lcciSubData[i].isGracePeriodControlled,
                                  clientMid,
                                  serverRef,
                                  clientRef);
   }

   handleLicenseKeyCloseToExpirationAlarm();

   if(lcci_getPv(lcciClient) == LICENSE_CAPACITY_CONTROL_I_VERSION_1)
   {
      lcci_sendLcciCapacityLicenseSubscribeCfm(lcciClient, sig);
   }
   else
   {
      lcci_sendLcciCapacityLicenseSubscribe2Cfm(lcciClient, sig);
   }


}


static void
lcci_sendLcciCapacityLicenseSubscribeCfm(LcciClient *lcciClient,
                                         union itc_msg *sigReq)
{
   union itc_msg     *sigRsp;
   char              tmpBuf[MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_KEY_ID + 60];
   LcciLicenseDataS  *lcciSubData;
   LcciLicenseInfoS  *lcciSubInfo;
   itc_mbox_id_t     clientMid;
   uint32_t          clientRef, i;
   int32_t           serverRef;
   char              *logStr;

   serverRef = rd32(sigReq->lcciCapacityLicenseSubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sigReq->lcciCapacityLicenseSubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sigReq);
   
   sigRsp = itc_alloc(sizeof(LcciCapacityLicenseSubscribeCfmS) +
                      rd32(sigReq->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData)
                      * sizeof(LcciLicenseInfoS),
                      LCCI_CAPACITY_LICENSE_SUBSCRIBE_CFM);
   sigRsp->lcciCapacityLicenseSubscribeCfm.addressInfo.clientRef = wr32(clientRef);
   sigRsp->lcciCapacityLicenseSubscribeCfm.addressInfo.serverRef =
           wr32(lcci_getClientServerRef(lcciClient));
   sigRsp->lcciCapacityLicenseSubscribeCfm.sizeOfLicenseInfo =
      sigReq->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData;


   lcciSubData = sigReq->lcciCapacityLicenseSubscribeReq.licenseData;
   lcciSubInfo = sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo;
   for(i = 0;
       i < rd32(sigReq->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      if(capacityState_fillLcciLicenseInfoByKeyId(&(lcciSubInfo[i]),
                                                 lcciSubData[i].licenseKeyId) == GLMS_FALSE)
      {
         itc_free(&sigRsp);
         disconnectLcciClient(clientMid,
                              clientRef,
                              serverRef,
                              "Failed to find CapacityKey");
         return;
      }
   }

   logStr = malloc(100 + 
                   (rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.sizeOfLicenseInfo) * 
                    (MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_KEY_ID + 100)));
   sprintf(logStr,
           "Sending LCCI subscribe cfm to MID 0x%x serverRef %d;",
           clientMid,
           serverRef);

   for(i = 0;
       i < rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.sizeOfLicenseInfo);
       i++)
   {
      if(rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.sizeOfLicenseInfo) > 1)
      {
         strcat(logStr, "\n   -");
      }

      sprintf(tmpBuf,
              " key = %s, licState = %d, noLimit = %d, value = %d, gpAvailable = %d",
              sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].licenseKeyId,
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].licenseState),
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].capacityLimit.noLimit),
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].capacityLimit.value),
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].gracePeriodAvailable));
              
              
      strcat(logStr, tmpBuf);
   }

   glms_logEvent(GLMS_LOG_MEDIUM, logStr);
   free(logStr);

   itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
}

static void
lcci_sendLcciCapacityLicenseSubscribe2Cfm(LcciClient *lcciClient,
                                          union itc_msg *sigReq)
{
   union itc_msg     *sigRsp;
   char              tmpBuf[MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_KEY_ID +
                            MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING + 60];
   LcciLicenseDataS  *lcciSubData;
   LcciLicenseInfo2S *lcciSubInfo;
   itc_mbox_id_t     clientMid;
   uint32_t          clientRef, i;
   int32_t           serverRef;
   char              *logStr;

   serverRef = rd32(sigReq->lcciCapacityLicenseSubscribeReq.addressInfo.serverRef);
   clientRef = rd32(sigReq->lcciCapacityLicenseSubscribeReq.addressInfo.clientRef);
   clientMid = itc_sender(sigReq);
   
   sigRsp = itc_alloc(sizeof(LcciCapacityLicenseSubscribe2CfmS) +
                      rd32(sigReq->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData)
                      * sizeof(LcciLicenseInfo2S),
                      LCCI_CAPACITY_LICENSE_SUBSCRIBE2_CFM);
   sigRsp->lcciCapacityLicenseSubscribe2Cfm.addressInfo.clientRef = wr32(clientRef);
   sigRsp->lcciCapacityLicenseSubscribe2Cfm.addressInfo.serverRef =
           wr32(lcci_getClientServerRef(lcciClient));
   sigRsp->lcciCapacityLicenseSubscribe2Cfm.sizeOfLicenseInfo =
      sigReq->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData;


   lcciSubData = sigReq->lcciCapacityLicenseSubscribeReq.licenseData;
   lcciSubInfo = sigRsp->lcciCapacityLicenseSubscribe2Cfm.licenseInfo;
   for(i = 0;
       i < rd32(sigReq->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData);
       i++)
   {
      if(capacityState_fillLcciLicenseInfo2ByKeyId(&(lcciSubInfo[i]),
                                                   lcciSubData[i].licenseKeyId) == GLMS_FALSE)
      {
         itc_free(&sigRsp);
         disconnectLcciClient(clientMid,
                              clientRef,
                              serverRef,
                              "Failed to find CapacityKey");
         return;
      }
   }

   logStr = malloc(100 + 
                   (rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.sizeOfLicenseInfo) * 
                    (MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_KEY_ID +
                     MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING + 100)));
   sprintf(logStr,
           "Sending LCCI subscribe cfm 2 to MID 0x%x serverRef %d;",
           clientMid,
           serverRef);

   for(i = 0;
       i < rd32(sigRsp->lcciCapacityLicenseSubscribe2Cfm.sizeOfLicenseInfo);
       i++)
   {
      if(rd32(sigRsp->lcciCapacityLicenseSubscribe2Cfm.sizeOfLicenseInfo) > 1)
      {
         strcat(logStr, "\n   -");
      }

      sprintf(tmpBuf,
              " key = %s, licState = %d, noLimit = %d, value = %d, gpAvailable = %d, hwac = %s",
              sigRsp->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[i].licenseKeyId,
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].licenseState),
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].capacityLimit.noLimit),
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].capacityLimit.value),
              rd32(sigRsp->lcciCapacityLicenseSubscribeCfm.licenseInfo[i].gracePeriodAvailable),
              sigRsp->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[i].hwacString);
              
              
      strcat(logStr, tmpBuf);
   }

   glms_logEvent(GLMS_LOG_MEDIUM, logStr);
   free(logStr);

   itc_send(&sigRsp, clientMid, ITC_MY_MBOX);
}

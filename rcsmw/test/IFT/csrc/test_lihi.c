/* ----------------------------------------------------------------------
 * %CCaseFile:	test_lihi.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/R8A/3 %
 * %CCaseDate:	2017-01-16 %
 * %CCaseDocNo: %
 * Author:	etxivri
 * Author: Ivan Ribrant, <ivan.ribrant@ericsson.com>
 *
 * Short description:
 * Super simple test LICI block in RCS using legacy interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *b
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R2A/1      2014-01-31  etxpejn     Created
 * R2A/3      2014-02-18  etxpejn     Updated
 * R2A/4      2014-03-19  etxpejn     Added attributes to LfciFeatureLicenseSubscribeReq and
 *                                    LfciFeatureLicenseUnsubscribeReq
 * R3A/1      2014-11-24  etxpejn     Added capacity signals
 * R5A/1      2016-01-26  etxpejn     Added new lfci signals
 * R58/1      2017-01-15  etxpejn     Added new lcci signals
 * R8A/1      2017-01-16  erarafo     Adjusted for VRCS64
 * R8A/2      2017-01-16 erarafo     ClearCase version embedded in executable
 * ----------------------------------------------------------------------
 */

#include "master.h"
#include "licenseFeatureControlICommon.h"
#include "licenseFeatureControlI.sig"
#include "licenseCapacityControlI.sig"
#include "licenseCapacityControlICommon.h"
#include "licenseFeatureControlIUnionContent.h"

#include "osens.sig"

#define LfciConnToServerReq_no 1
#define LfciFeatureLicenseSubscribeReq_no 2
#define LfciFeatureLicenseUnsubscribeReq_no 3

#define LcciConnToServerReq_no 6
#define LcciCapacityLicenseSubscribeReq_no 7
#define LcciCapacityLicenseUnsubscribeReq_no 8
#define LcciCapacityLicenseGpActivatedFwd_no 9

union SIGNAL
{
  SIGSELECT sigNo;

  struct LfciConnToServerReqS                 lfciConnToServerReq;
  struct LfciConnToServerCfmS                 lfciConnToServerCfm;
  struct LfciConnToServerRejS                 lfciConnToServerRej;
  struct LfciFeatureLicenseSubscribeReqS      lfciFeatureLicenseSubscribeReq;
  struct LfciFeatureLicenseSubscribeCfmS      lfciFeatureLicenseSubscribeCfm;
  struct LfciFeatureLicenseUnsubscribeReqS    lfciFeatureLicenseUnsubscribeReq;
  struct LfciFeatureLicenseUnsubscribeCfmS    lfciFeatureLicenseUnsubscribeCfm;
  struct LfciFeatureLicenseChangeIndS         lfciFeatureLicenseChangeInd;
  struct LfciFeatureLicenseDisconnectIndS     lfciFeatureLicenseDisconnectInd;
  struct LfciOseAttachClientDownS             lfciOseAttachClientDown;
  struct LfciFeatureLicenseSubscribe2CfmS     lfciFeatureLicenseSubscribe2Cfm;
  struct LfciFeatureLicenseChange2IndS        lfciFeatureLicenseChange2Ind;

  struct LcciConnToServerReqS                 lcciConnToServerReq;
  struct LcciConnToServerCfmS                 lcciConnToServerCfm;
  struct LcciCapacityLicenseSubscribeReqS     lcciCapacityLicenseSubscribeReq;
  struct LcciCapacityLicenseSubscribeCfmS     lcciCapacityLicenseSubscribeCfm;
  struct LcciCapacityLicenseUnsubscribeReqS   lcciCapacityLicenseUnsubscribeReq;
  struct LcciCapacityLicenseUnsubscribeCfmS   lcciCapacityLicenseUnsubscribeCfm;
  struct LcciCapacityLicenseChangeIndS        lcciCapacityLicenseChangeInd;
  struct LcciCapacityLicenseDisconnectIndS    lcciCapacityLicenseDisconnectInd;
  struct LcciCapacityLicenseGpActivatedFwdS   lcciCapacityLicenseGpActivatedFwd;
  struct LcciCapacityLicenseSubscribe2CfmS    lcciCapacityLicenseSubscribe2Cfm;
  struct LcciCapacityLicenseChange2IndS       lcciCapacityLicenseChange2Ind;

  struct NSHuntRequest nsHuntRequest;
};

char *test_lihi_version = "test_lihi.c %CCaseRev:	/main/R2A/R3A/R4A/R5A/R8A/3 %";

ei_x_buff
send_sig_lihi(void **liciMemory_p, union SIGNAL *sig_p, int func, ei_x_buff args) {
  
  ei_x_buff resp;
   
  static SIGSELECT sigSelect1[] = { 1, LFCI_HUNT };
  static SIGSELECT sigSelect2[] = { 1, LCCI_HUNT };

   PROCESS pid;
   U16 tagLen;
   int isGracePeriodControlled, capacityType;
   U32 size;
   U32 protVersion;
   U32 serverRef;
   U32 clientRef;

   int type, size2;
   char *licenseKeyId;
   char *licenseMoId;
   char *licenseName;
   char *capacityUnit;

   typedef struct
   {
     SIGSELECT    sigNo;
   } HuntSignal;

   HuntSignal *huntRsp;
   HuntSignal *huntRsp2;
  
   ei_x_new(&resp);

  switch (func) {
    
  case LfciConnToServerReq_no:
    //APPLOG("LfciConnToServerReq_no");

    hunt ("ose_ns", 0, &pid, NULL);

    tagLen = (U16) (((strlen (LICENSE_FEATURE_CONTROL_I_SERVICE_NAME) + 4) / 4) * 4);
    size = sizeof (struct NSHuntRequest) + tagLen + sizeof (SIGSELECT);

    sig_p = alloc (size, NS_HUNT_REQUEST);
    sig_p->nsHuntRequest.tagOffset = 0;
    sig_p->nsHuntRequest.huntSignalOffset = tagLen;
    strncpy ((char *) sig_p->nsHuntRequest.data, LICENSE_FEATURE_CONTROL_I_SERVICE_NAME,
	     tagLen);

    huntRsp =  (HuntSignal *) & sig_p->nsHuntRequest.data[tagLen];
    huntRsp->sigNo = LFCI_HUNT;
    send (&sig_p, pid);
    
    sig_p = receive (sigSelect1);
    pid = sender (&sig_p);
    
    ei_decode_tuple_header(args.buff, &args.index, NULL);
    protVersion = decodeToU32(&args);
    clientRef = decodeToU32(&args);
    //APPLOG("protVersion %u", protVersion);
    //APPLOG("clientRef %u", clientRef);

    sig_p = alloc(sizeof(LfciConnToServerReqS), LFCI_CONN_TO_SERVER_REQ);
    
    sig_p->lfciConnToServerReq.addressInfo.clientRef    = clientRef;
    sig_p->lfciConnToServerReq.addressInfo.serverRef    = 0;
    sig_p->lfciConnToServerReq.sizeOfProtocolRevisions  = 1;
    sig_p->lfciConnToServerReq.protocolRevisions[0]     = protVersion;
 
    send(&sig_p, pid);
   	
    ei_x_format(&resp, "{ok, conn_to_server_req}");
    return resp;

 case LcciConnToServerReq_no:
   hunt ("ose_ns", 0, &pid, NULL);

   tagLen = (U16) (((strlen (LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME) + 4) / 4) * 4);
   size = sizeof (struct NSHuntRequest) + tagLen + sizeof (SIGSELECT);
   
   sig_p = alloc (size, NS_HUNT_REQUEST);
   sig_p->nsHuntRequest.tagOffset = 0;
   sig_p->nsHuntRequest.huntSignalOffset = tagLen;
   strncpy ((char *) sig_p->nsHuntRequest.data, LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME,
	    tagLen);

   huntRsp2 =  (HuntSignal *) & sig_p->nsHuntRequest.data[tagLen];
   huntRsp2->sigNo = LCCI_HUNT;
   send (&sig_p, pid);
   
   sig_p = receive (sigSelect2);
   pid = sender (&sig_p);
   
   ei_decode_tuple_header(args.buff, &args.index, NULL);
   protVersion = decodeToU32(&args);
   clientRef = decodeToU32(&args);
 
   sig_p = alloc(sizeof(LcciConnToServerReqS), LCCI_CONN_TO_SERVER_REQ);
   
   sig_p->lcciConnToServerReq.addressInfo.clientRef    = clientRef;
   sig_p->lcciConnToServerReq.addressInfo.serverRef    = 0;
   sig_p->lcciConnToServerReq.sizeOfProtocolRevisions  = 1;
   sig_p->lcciConnToServerReq.protocolRevisions[0]     = protVersion;
   
   send(&sig_p, pid);
   
   ei_x_format(&resp, "{ok, conn_to_server_req}");
   return resp;
    
  case LfciFeatureLicenseSubscribeReq_no:
    //APPLOG("LfciFeatureLicenseSubscribeReq");

    hunt ("ose_ns", 0, &pid, NULL);

    tagLen = (U16) (((strlen (LICENSE_FEATURE_CONTROL_I_SERVICE_NAME) + 4) / 4) * 4);
    size = sizeof (struct NSHuntRequest) + tagLen + sizeof (SIGSELECT);

    sig_p = alloc (size, NS_HUNT_REQUEST);
    sig_p->nsHuntRequest.tagOffset = 0;
    sig_p->nsHuntRequest.huntSignalOffset = tagLen;
    strncpy ((char *) sig_p->nsHuntRequest.data, LICENSE_FEATURE_CONTROL_I_SERVICE_NAME,
	     tagLen);

    huntRsp =  (HuntSignal *) & sig_p->nsHuntRequest.data[tagLen];
    huntRsp->sigNo = LFCI_HUNT;
    send (&sig_p, pid);
    
    sig_p = receive (sigSelect1);
    pid = sender (&sig_p);

    ei_decode_tuple_header(args.buff, &args.index, NULL);
    serverRef = decodeToU32(&args);
    clientRef = decodeToU32(&args);
    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseKeyId = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseKeyId);

    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseMoId = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseMoId);

    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseName = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseName);
    
    //APPLOG("serverRef %u", serverRef);
    //APPLOG("licenseKeyId %s", licenseKeyId);
    //APPLOG("licenseMoId %s", licenseMoId);
    //APPLOG("licenseName %s", licenseName);
  
    sig_p = alloc(sizeof(LfciFeatureLicenseSubscribeReqS),
		  LFCI_FEATURE_LICENSE_SUBSCRIBE_REQ);

   sig_p->lfciFeatureLicenseSubscribeReq.addressInfo.clientRef    = clientRef;
   sig_p->lfciFeatureLicenseSubscribeReq.addressInfo.serverRef    = serverRef;
   sig_p->lfciFeatureLicenseSubscribeReq.sizeOfLicenseData        = 1;
   strcpy(sig_p->lfciFeatureLicenseSubscribeReq.licenseData[0].licenseKeyId, licenseKeyId);
   strcpy(sig_p->lfciFeatureLicenseSubscribeReq.licenseData[0].licenseMoId, licenseMoId);
   strcpy(sig_p->lfciFeatureLicenseSubscribeReq.licenseData[0].licenseName, licenseName);

   send(&sig_p, pid);
   
   free(licenseKeyId);
   free(licenseMoId);   
   free(licenseName);

   
   ei_x_format(&resp, "{ok, feature_license_subscribe_req}");
   return resp;

  case LcciCapacityLicenseSubscribeReq_no:
    hunt ("ose_ns", 0, &pid, NULL);

    tagLen = (U16) (((strlen (LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME) + 4) / 4) * 4);
    size = sizeof (struct NSHuntRequest) + tagLen + sizeof (SIGSELECT);

    sig_p = alloc (size, NS_HUNT_REQUEST);
    sig_p->nsHuntRequest.tagOffset = 0;
    sig_p->nsHuntRequest.huntSignalOffset = tagLen;
    strncpy ((char *) sig_p->nsHuntRequest.data, LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME,
	     tagLen);
    
    huntRsp2 =  (HuntSignal *) & sig_p->nsHuntRequest.data[tagLen];
    huntRsp2->sigNo = LCCI_HUNT;
    send (&sig_p, pid);
    
    sig_p = receive (sigSelect2);
    pid = sender (&sig_p);

    ei_decode_tuple_header(args.buff, &args.index, NULL);
    serverRef = decodeToU32(&args);
    clientRef = decodeToU32(&args);
   
    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseKeyId = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseKeyId);

    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseMoId = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseMoId);

    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseName = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseName);

    ei_get_type(args.buff, &args.index, &type, &size2); 
    capacityUnit = malloc(size2+1); 
    ei_decode_string(args.buff, &args.index, capacityUnit); 

    isGracePeriodControlled = (int)decodeToU32(&args);
    capacityType = (int)decodeToU32(&args);

    sig_p = alloc(sizeof(LcciCapacityLicenseSubscribeReqS),
		  LCCI_CAPACITY_LICENSE_SUBSCRIBE_REQ);

   sig_p->lcciCapacityLicenseSubscribeReq.addressInfo.clientRef    = clientRef;
   sig_p->lcciCapacityLicenseSubscribeReq.addressInfo.serverRef    = serverRef;
   sig_p->lcciCapacityLicenseSubscribeReq.sizeOfLicenseData        = 1;
   strcpy(sig_p->lcciCapacityLicenseSubscribeReq.licenseData[0].licenseKeyId, licenseKeyId);
   strcpy(sig_p->lcciCapacityLicenseSubscribeReq.licenseData[0].licenseMoId, licenseMoId);
   strcpy(sig_p->lcciCapacityLicenseSubscribeReq.licenseData[0].licenseName, licenseName);
   strcpy(sig_p->lcciCapacityLicenseSubscribeReq.licenseData[0].capacityUnit, capacityUnit); 
   sig_p->lcciCapacityLicenseSubscribeReq.licenseData[0].isGracePeriodControlled = isGracePeriodControlled; 
   sig_p->lcciCapacityLicenseSubscribeReq.licenseData[0].capacityType = capacityType; 

  send(&sig_p, pid);
   
   free(licenseKeyId);
   free(licenseMoId);
   free(licenseName);
   free(capacityUnit);
   ei_x_format(&resp, "{ok, capacity_license_subscribe_req}");
   return resp;

  case LfciFeatureLicenseUnsubscribeReq_no:
    //APPLOG("LfciFeatureLicenseUnsubscribeReq");
    
        hunt ("ose_ns", 0, &pid, NULL);

    tagLen = (U16) (((strlen (LICENSE_FEATURE_CONTROL_I_SERVICE_NAME) + 4) / 4) * 4);
    size = sizeof (struct NSHuntRequest) + tagLen + sizeof (SIGSELECT);

    sig_p = alloc (size, NS_HUNT_REQUEST);
    sig_p->nsHuntRequest.tagOffset = 0;
    sig_p->nsHuntRequest.huntSignalOffset = tagLen;
    strncpy ((char *) sig_p->nsHuntRequest.data, LICENSE_FEATURE_CONTROL_I_SERVICE_NAME,
	     tagLen);

    huntRsp =  (HuntSignal *) & sig_p->nsHuntRequest.data[tagLen];
    huntRsp->sigNo = LFCI_HUNT;
    send (&sig_p, pid);
    
    sig_p = receive (sigSelect1);
    pid = sender (&sig_p);

    ei_decode_tuple_header(args.buff, &args.index, NULL);
    serverRef = decodeToU32(&args);
    clientRef = decodeToU32(&args);
    
    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseKeyId = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseKeyId);

    sig_p = alloc(sizeof(LfciFeatureLicenseUnsubscribeReqS),
		  LFCI_FEATURE_LICENSE_UNSUBSCRIBE_REQ);

    sig_p->lfciFeatureLicenseUnsubscribeReq.addressInfo.clientRef    = clientRef;
    sig_p->lfciFeatureLicenseUnsubscribeReq.addressInfo.serverRef    = serverRef;
    sig_p->lfciFeatureLicenseUnsubscribeReq.sizeOfLicenseData        = 1;
    strcpy(sig_p->lfciFeatureLicenseUnsubscribeReq.licenseData[0].licenseKeyId,
	   licenseKeyId);
    
    send(&sig_p, pid);
   
    free(licenseKeyId);

    ei_x_format(&resp, "{ok, feature_license_unsubscribe_req}");
    return resp;

  case LcciCapacityLicenseUnsubscribeReq_no:
    hunt ("ose_ns", 0, &pid, NULL);

    tagLen = (U16) (((strlen (LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME) + 4) / 4) * 4);
    size = sizeof (struct NSHuntRequest) + tagLen + sizeof (SIGSELECT);
    
    sig_p = alloc (size, NS_HUNT_REQUEST);
    sig_p->nsHuntRequest.tagOffset = 0;
    sig_p->nsHuntRequest.huntSignalOffset = tagLen;
    strncpy ((char *) sig_p->nsHuntRequest.data, LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME,
	     tagLen);

    huntRsp2 =  (HuntSignal *) & sig_p->nsHuntRequest.data[tagLen];
    huntRsp2->sigNo = LCCI_HUNT;
    send (&sig_p, pid);
    
    sig_p = receive (sigSelect2);
    pid = sender (&sig_p);

    ei_decode_tuple_header(args.buff, &args.index, NULL);
    serverRef = decodeToU32(&args);
    clientRef = decodeToU32(&args);
    
    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseKeyId = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseKeyId);

    sig_p = alloc(sizeof(LcciCapacityLicenseUnsubscribeReqS),
		  LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_REQ);

    sig_p->lcciCapacityLicenseUnsubscribeReq.addressInfo.clientRef    = clientRef;
    sig_p->lcciCapacityLicenseUnsubscribeReq.addressInfo.serverRef    = serverRef;
    sig_p->lcciCapacityLicenseUnsubscribeReq.sizeOfLicenseData        = 1;
    strcpy(sig_p->lcciCapacityLicenseUnsubscribeReq.licenseData[0].licenseKeyId,
	   licenseKeyId);
    
    send(&sig_p, pid);
    free(licenseKeyId);
    ei_x_format(&resp, "{ok, capacity_license_unsubscribe_req}");
    return resp;

  case LcciCapacityLicenseGpActivatedFwd_no:
    hunt ("ose_ns", 0, &pid, NULL);

    tagLen = (U16) (((strlen (LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME) + 4) / 4) * 4);
    size = sizeof (struct NSHuntRequest) + tagLen + sizeof (SIGSELECT);
    
    sig_p = alloc (size, NS_HUNT_REQUEST);
    sig_p->nsHuntRequest.tagOffset = 0;
    sig_p->nsHuntRequest.huntSignalOffset = tagLen;
    strncpy ((char *) sig_p->nsHuntRequest.data, LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME,
	     tagLen);

    huntRsp2 =  (HuntSignal *) & sig_p->nsHuntRequest.data[tagLen];
    huntRsp2->sigNo = LCCI_HUNT;
    send (&sig_p, pid);
    
    sig_p = receive (sigSelect2);
    pid = sender (&sig_p);

    ei_decode_tuple_header(args.buff, &args.index, NULL);
    serverRef = decodeToU32(&args);
    clientRef = decodeToU32(&args);

    ei_get_type(args.buff, &args.index, &type, &size2);
    licenseKeyId = malloc(size2+1);
    ei_decode_string(args.buff, &args.index, licenseKeyId);

    sig_p = alloc(sizeof(LcciCapacityLicenseGpActivatedFwdS),
		  LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD);
    
    sig_p->lcciCapacityLicenseGpActivatedFwd.addressInfo.clientRef    = clientRef;
    sig_p->lcciCapacityLicenseGpActivatedFwd.addressInfo.serverRef    = serverRef;
    strcpy(sig_p->lcciCapacityLicenseGpActivatedFwd.licenseKeyId, licenseKeyId);
    
    send(&sig_p, pid);
    free(licenseKeyId);
    ei_x_format(&resp, "{ok, capacity_license_gp_activated}");
    return resp;
  }
  
  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}

ei_x_buff
recv_sig_lihi(union SIGNAL *sig_p) {
  ei_x_buff resp;

  ei_x_new(&resp);
  switch(sig_p->sigNo) {
    
  case LFCI_CONN_TO_SERVER_CFM:
    ei_x_format(&resp, "{signal, {~i,~i,~i}}",
		(unsigned int) LFCI_CONN_TO_SERVER_CFM,
		(unsigned int) sig_p->lfciConnToServerCfm.protocolRevision,
		(unsigned int) sig_p->lfciConnToServerCfm.addressInfo.serverRef);
    free_buf(&sig_p);
    return resp;

  case LCCI_CONN_TO_SERVER_CFM:
    ei_x_format(&resp, "{signal, {~i,~i,~i}}",
		(unsigned int) LCCI_CONN_TO_SERVER_CFM,
		(unsigned int) sig_p->lcciConnToServerCfm.protocolRevision,
		(unsigned int) sig_p->lcciConnToServerCfm.addressInfo.serverRef);
    free_buf(&sig_p);
    return resp;

  case LFCI_CONN_TO_SERVER_REJ:
    ei_x_format(&resp, "{signal, {~i}}",
		(unsigned int) LFCI_CONN_TO_SERVER_REJ);
    free_buf(&sig_p);
    return resp;

  case LCCI_CONN_TO_SERVER_REJ:
    ei_x_format(&resp, "{signal, {~i}}",
		(unsigned int) LCCI_CONN_TO_SERVER_REJ);
    free_buf(&sig_p);
    return resp;

  case LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM:
    ei_x_format(&resp, "{signal, {~i,~i}}",
		(unsigned int) LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM,
		(unsigned int) sig_p->lfciFeatureLicenseSubscribeCfm.addressInfo.clientRef);
    free_buf(&sig_p);
    return resp;

  case LFCI_FEATURE_LICENSE_SUBSCRIBE2_CFM:
    ei_x_format(&resp, "{signal, {~i,~i,~i}}",
		(unsigned int) LFCI_FEATURE_LICENSE_SUBSCRIBE2_CFM,
		(unsigned int) sig_p->lfciFeatureLicenseSubscribe2Cfm.addressInfo.clientRef,
		sig_p->lfciFeatureLicenseSubscribe2Cfm.licenseInfo[0].alarmCorrelationEventId);
    free_buf(&sig_p);
    return resp;
    
  case LCCI_CAPACITY_LICENSE_SUBSCRIBE_CFM:
    // This reply needs to be changed if support for multiple CXCs is added in 
    // LCCI_CAPACITY_LICENSE_SUBSCRIBE_REQ
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i,~i,~i,~i,~i}}",
		(unsigned int) LCCI_CAPACITY_LICENSE_SUBSCRIBE_CFM,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribeCfm.sizeOfLicenseInfo,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribeCfm.addressInfo.clientRef,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribeCfm.licenseInfo[0].licenseState,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribeCfm.licenseInfo[0].gracePeriodAvailable,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribeCfm.licenseInfo[0].gracePeriodActivationThreshold,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribeCfm.licenseInfo[0].capacityLimit.value,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribeCfm.licenseInfo[0].capacityLimit.noLimit);
    free_buf(&sig_p);
    return resp;

  case LCCI_CAPACITY_LICENSE_SUBSCRIBE2_CFM:
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i,~i,~i,~i,~i,~s}}",
		(unsigned int) LCCI_CAPACITY_LICENSE_SUBSCRIBE2_CFM,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribe2Cfm.sizeOfLicenseInfo,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribe2Cfm.addressInfo.clientRef,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[0].licenseState,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[0].gracePeriodAvailable,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[0].gracePeriodActivationThreshold,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[0].capacityLimit.value,
		(unsigned int) sig_p->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[0].capacityLimit.noLimit,
		sig_p->lcciCapacityLicenseSubscribe2Cfm.licenseInfo[0].hwacString);
    free_buf(&sig_p);
    return resp;


  case LFCI_FEATURE_LICENSE_UNSUBSCRIBE_CFM:
    //APPLOG("LFCI_FEATURE_LICENSE_UNSUBSCRIBE_CFM");
    ei_x_format(&resp, "{signal, {~i,~i}}",
		(unsigned int) LFCI_FEATURE_LICENSE_UNSUBSCRIBE_CFM,
		(unsigned int) sig_p->lfciFeatureLicenseUnsubscribeCfm.addressInfo.clientRef);
    free_buf(&sig_p);
    return resp;

  case LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_CFM:
    ei_x_format(&resp, "{signal, {~i,~i}}",
		(unsigned int) LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_CFM,	
		(unsigned int) sig_p->lcciCapacityLicenseUnsubscribeCfm.addressInfo.clientRef);
    free_buf(&sig_p);
    return resp;

  case LFCI_FEATURE_LICENSE_CHANGE_IND:
    //APPLOG("LFCI_FEATURE_LICENSE_CHANGE_IND");
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i,~s}}",
		(unsigned int) LFCI_FEATURE_LICENSE_CHANGE_IND,
		(unsigned int) sig_p->lfciFeatureLicenseChangeInd.addressInfo.clientRef,
		(unsigned int) sig_p->lfciFeatureLicenseChangeInd.licenseInfo.featureState,
		(unsigned int) sig_p->lfciFeatureLicenseChangeInd.licenseInfo.licenseState,
		sig_p->lfciFeatureLicenseChangeInd.licenseInfo.licenseKeyId);
    free_buf(&sig_p);
    return resp;

  case LFCI_FEATURE_LICENSE_CHANGE2_IND:
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i,~s,~i}}",
		(unsigned int) LFCI_FEATURE_LICENSE_CHANGE2_IND,
		(unsigned int) sig_p->lfciFeatureLicenseChange2Ind.addressInfo.clientRef,
		(unsigned int) sig_p->lfciFeatureLicenseChange2Ind.licenseInfo.featureState,
		(unsigned int) sig_p->lfciFeatureLicenseChange2Ind.licenseInfo.licenseState,
		sig_p->lfciFeatureLicenseChange2Ind.licenseInfo.licenseKeyId,
		sig_p->lfciFeatureLicenseChange2Ind.licenseInfo.alarmCorrelationEventId);
    free_buf(&sig_p);
    return resp;

  case LCCI_CAPACITY_LICENSE_CHANGE_IND:
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i,~i,~i,~i}}",
		LCCI_CAPACITY_LICENSE_CHANGE_IND,
		sig_p->lcciCapacityLicenseChangeInd.addressInfo.clientRef,
		sig_p->lcciCapacityLicenseChangeInd.licenseInfo.licenseState,
		sig_p->lcciCapacityLicenseChangeInd.licenseInfo.gracePeriodAvailable,
		sig_p->lcciCapacityLicenseChangeInd.licenseInfo.gracePeriodActivationThreshold,
		sig_p->lcciCapacityLicenseChangeInd.licenseInfo.capacityLimit.value,
		sig_p->lcciCapacityLicenseChangeInd.licenseInfo.capacityLimit.noLimit);
    free_buf(&sig_p);
    return resp;

  case LCCI_CAPACITY_LICENSE_CHANGE2_IND:
    ei_x_format(&resp, "{signal, {~i,~i,~i,~i,~i,~i,~i,~s}}",
		LCCI_CAPACITY_LICENSE_CHANGE2_IND,
		sig_p->lcciCapacityLicenseChange2Ind.addressInfo.clientRef,
		sig_p->lcciCapacityLicenseChange2Ind.licenseInfo.licenseState,
		sig_p->lcciCapacityLicenseChange2Ind.licenseInfo.gracePeriodAvailable,
		sig_p->lcciCapacityLicenseChange2Ind.licenseInfo.gracePeriodActivationThreshold,
		sig_p->lcciCapacityLicenseChange2Ind.licenseInfo.capacityLimit.value,
		sig_p->lcciCapacityLicenseChange2Ind.licenseInfo.capacityLimit.noLimit,
		sig_p->lcciCapacityLicenseChange2Ind.licenseInfo.hwacString);
    free_buf(&sig_p);
    return resp;

  case LFCI_FEATURE_LICENSE_DISCONNECT_IND:
    ei_x_format(&resp, "{signal, {~i,~i}}",
		(unsigned int) LFCI_FEATURE_LICENSE_DISCONNECT_IND,
		(unsigned int) sig_p->lfciFeatureLicenseDisconnectInd.addressInfo.clientRef);
    free_buf(&sig_p);
    return resp;

  case LCCI_CAPACITY_LICENSE_DISCONNECT_IND:
    ei_x_format(&resp, "{signal, {~i,~i}}",
		(unsigned int) LCCI_CAPACITY_LICENSE_DISCONNECT_IND,
		(unsigned int) sig_p->lcciCapacityLicenseDisconnectInd.addressInfo.clientRef);
    free_buf(&sig_p);
    return resp;
   
  default:
    ei_x_format(&resp, "{signal, ~i}", (unsigned int) sig_p->sigNo);
    return resp;
  }
}

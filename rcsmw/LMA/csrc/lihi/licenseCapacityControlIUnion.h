/*****************************************************************************
* 
*  COPYRIGHT (C) by Ericsson AB
* 
*  The copyright to the computer program(s) herein is the property
*  of Ericsson AB. 
* 
*  The program(s) may be used and/or copied only with the written
*  permission from Ericsson AB or in accordance with the terms 
*  and conditions stipulated in the agreement/contract under which 
*  the program(s) have been supplied. 
*
*
*****************************************************************************/
#include "licenseCapacityControlI.sig"

union itc_msg
{
  uint32_t  sigNo;


  struct LcciConnToServerReqS                  lcciConnToServerReq;
  struct LcciConnToServerCfmS                  lcciConnToServerCfm;
  struct LcciConnToServerRejS                  lcciConnToServerRej;
  struct LcciCapacityLicenseSubscribeReqS      lcciCapacityLicenseSubscribeReq;
  struct LcciCapacityLicenseSubscribeCfmS      lcciCapacityLicenseSubscribeCfm;
  struct LcciCapacityLicenseUnsubscribeReqS    lcciCapacityLicenseUnsubscribeReq;
  struct LcciCapacityLicenseUnsubscribeCfmS    lcciCapacityLicenseUnsubscribeCfm;
  struct LcciCapacityLicenseChangeIndS         lcciCapacityLicenseChangeInd;
  struct LcciCapacityLicenseDisconnectIndS     lcciCapacityLicenseDisconnectInd;
  struct LcciCapacityLicenseGpActivatedFwdS    lcciCapacityLicenseGpActivatedFwd;
  struct LcciOseAttachClientDownS              lcciOseAttachClientDown;
  struct LcciCapacityLicenseSubscribe2CfmS     lcciCapacityLicenseSubscribe2Cfm;
  struct LcciCapacityLicenseChange2IndS        lcciCapacityLicenseChange2Ind;
};

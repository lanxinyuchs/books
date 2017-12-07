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
#include "licenseFeatureControlI.sig"

union itc_msg
{
  uint32_t  sigNo;


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

};

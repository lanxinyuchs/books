%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lihi.hrl %
%%% Author:	etxpejn
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R11A/1').
-hrl_date('2017-10-09').
-hrl_author('etxpejn').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
%%% 
%%% The information in this document is the property of Ericsson.
%%% 
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2014-05-06   etxpejn   Moved from RCT to LMA
%%% R3A/2      2014-05-06   etxpejn   Added capacity signals and new LKF.
%%% R5A/4      2016-06-21   etxkols   GIT migration
%%% R7A/1      2016-09-16   etxpejn   Changed NO_OF_TRIES and NO_OF_TRIES_XL
%%% R7A/2      2016-10-12   etxpejn   Added new LKF RCS_MSR_161004_081623.xml
%%% R8A/1      2017-01-23   etxpejn   Added new LcciCapacity 2 signals
%%% R8A/2      2017-01-25   etxpejn   Added new LKF 
%%% R9A/1      2017-05-30   etxpejn   Added new LKF 
%%% R11A/1     2017-10-09   etxpejn   Prolonged NO_OF_TRIES*
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

-define(LfciConnToServerReq, 1).
-define(LfciFeatureLicenseSubscribeReq, 2).
-define(LfciFeatureLicenseUnsubscribeReq, 3).

-define(LcciConnToServerReq, 6).
-define(LcciCapacityLicenseSubscribeReq, 7).
-define(LcciCapacityLicenseUnsubscribeReq, 8).
-define(LcciCapacityLicenseGpActivatedFwd, 9).

%% Values fetched from licenseFeatureControllI.sig
-define(LfciConnToServerCfm, 24386817).
-define(LfciConnToServerRej, 24386818).
-define(LfciFeatureLicenseSubscribeCfm, 24386820).
-define(LfciFeatureLicenseSubscribe2Cfm, 24386826).
-define(LfciFeatureLicenseUnsubscribeCfm, 24386822).
-define(LfciFeatureLicenseChangeInd, 24386823).
-define(LfciFeatureLicenseChange2Ind, 24386827).
-define(LfciFeatureLicenseDisconnectInd, 24386824).

%% Values fetched from licenseCapacityControllI.sig
-define(LcciConnToServerCfm, 24387073).
-define(LcciConnToServerRej, 24387074).
-define(LcciCapacityLicenseSubscribeCfm, 24387076).
-define(LcciCapacityLicenseSubscribe2Cfm, 24387083).
-define(LcciCapacityLicenseUnsubscribeCfm, 24387078).
-define(LcciCapacityLicenseChangeInd, 24387079).
-define(LcciCapacityLicenseChange2Ind, 24387084).
-define(LcciCapacityLicenseDisconnectInd, 24387080).


-define(RPC_CALL(M, F, A), rct_rpc:call(rpc_1, M, F, A, 10000)).

-define(PROJ_DIR, "/proj/rcs-tmp/stps/").
-define(TEST_DIR, "/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/LMA/LMA_CNX9013077/test/suites/").
-define(GIT_TEST_DIR, {"RCS_TOP","LMA/test/suites/","LMA/LMA_CNX9013077/test/suites/"}).
-define(LKF, "RCS_MSR_170517_130105.xml").
-define(NO_OF_TRIES, 7*2).
-define(NO_OF_TRIES_XL, 40*2).

-define(Fingerprint, "RCS_MSR").

-define(LM_MO, "ManagedElement=1,SystemFunctions=1,Lm=1").
-define(FM_MO, "ManagedElement=1,SystemFunctions=1,Fm=1").
-define(EU_MO, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1").
-define(IU_MO, "ManagedElement=1,SystemFunctions=1,Lm=1,IntegrationUnlock=1").
-define(KEYFILE_MGMT_MO, "ManagedElement=1,SystemFunctions=1,Lm=1,KeyFileManagement=1").
-define(KEYFILE_INFO_MO, "ManagedElement=1,SystemFunctions=1,Lm=1,KeyFileManagement=1,KeyFileInformation=1").


-define(LIHI,13).
-define(ClientRef, 1).
-define(ClientRef_2, 2).
-define(ClientRef_3, 3).

-define(FEATURE_STATE_CXC1234567, "ManagedElement=1,SystemFunctions=1,Lm=1,FeatureState=CXC1234567/1000").
-define(FEATURE_STATE_CXC1234567_v2, "ManagedElement=1,SystemFunctions=1,Lm=1,FeatureState=CXC1234567/2000").


-define(ACTIVATED, 1).
-define(DEACTIVATED, 0).

-define(GP_AVAILABLE_1, 1).
-define(GP_NOT_AVAILABLE_0, 0).

-define(GP_monitoring_shall_be_done_1, 1).
-define(GP_monitoring_shall_NOT_be_done_0, 0).


-define(SW_CAPACITYTYPE, 0).
-define(HWAC_CAPACITYTYPE, 1).
-define(SW_AND_HWAC_CAPACITYTYPE, 2).

-define(LicenseState_Enabled, 1).
-define(LicenseState_Disabled, 0).

-define(NoLimit_active_1, 1).
-define(NoLimit_deactive_0, 0).

-define(ActivationThreshold_default, 1).

-define(CapacityValue_1, 1).
-define(CapacityNoValue_0, 0).

-define(AlarmCorrelationCeased, 0).

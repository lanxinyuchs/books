%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lma.hrl %
%%% Author:	etxbjca
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R3A/R4A/R5A/R11A/R12A/1').
-hrl_date('2017-11-02').
-hrl_author('echhedb').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/1      2013-10-08   etxpejn     Created
%%% R2A/2      2013-10-11   etxpejn     Added lma_KFLocation
%%% R2A/3      2013-11-13   etxpejn     Added unlock tables
%%% R2A/5      2014-01-14   etxpejn     Added amPersistentData
%%% R2A/7      2014-02-05   etxpejn     Added sequenceNo to lma_KFLocation
%%% R2A/8      2014-03-12   etxpejn     Removed lma_id and added featureStates tabels
%%% R3A/1      2014-10-06   etxpejn     Added capacity tables
%%% R3A/2      2014-11-25   etxpejn     Added GP_ID
%%% R4A/1      2015-06-09   etxpejn     Added lmaModel
%%% R4A/2      2015-06-15   etxpejn     testMom added to lma_KFLocation
%%% R12A/1     2017-10-31   echhedb     SP086: Added LicenseSupport related tables.
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

-define(ID(Rdn), {"1","1","1",Rdn}).
-define(GP_ID(Rdn), {"1","1","1",Rdn,Rdn}).
-define(BLOCK, "lma").


-record(lmaLicenseKeyData, {keyId, prodNo, prodRev}).

-record(licenseManagerSoftwareData, {index,info}).
-record(licenseManagerPersistentData, {index,info}).

-record(licenseSupportSoftwareData, {index,info}).
-record(licenseSupportPersistentData, {index,info}).

-record(featureKeySoftwareData, {index,info}).
-record(featureKeyPersistentData, {index,info}).

-record(keyFilePersistentData, {index,info}).

-record(eUPersistentData, {index,info}).

-record(iUPersistentData, {index,info}).

-record(pUPersistentData, {index,info}).

-record(amPersistentData, {index,info}).

-record(featureStateSoftwareData, {index,info}).
-record(featureStatePersistentData, {index,info}).

-record(capacityStateSoftwareData, {index,info}).
-record(capacityStatePersistentData, {index,info}).

-record(capacityKeySoftwareData, {index,info}).
-record(capacityKeyPersistentData, {index,info}).

-record(lma_KFLocation, {index, info, sequenceNo, testMom}).
-record(lmaModel, {index, transId}).








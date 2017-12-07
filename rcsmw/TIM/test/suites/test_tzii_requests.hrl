%%% Generated file; do not edit; to generate: 'clearmake test-headers' in CAX directory
%%% generated: Mon Sep 19 20:34:24 CEST 2016
%%% Source: /vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/IFT/IFT_CNX9012887/IFT_CAX1033316/csrc/test_tzii_requests.h
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
-define(TEST_TZII_REQUESTS_DEP, {"/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/TIM/TIM_CNX9013408/test/suites/test_tzii_requests.hrl", ["/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/IFT/IFT_CNX9012887/IFT_CAX1033316/csrc/test_tzii_requests.h"]}).
-define(iftRequest_initiateMemory, 1).
-define(iftRequest_freeMemory, 2).
-define(iftRequest_initiateService, 3).
-define(iftRequest_terminateService, 4).
-define(iftRequest_internal, 5).
-define(iftRequest_subscribeDaylightSavingTime, 6).
-define(iftRequest_subscribeLeapSeconds, 7).
-define(iftRequest_startExample, 8).
-define(iftRequest_stopExample, 9).
-define(iftRequest_getPid, 10).
-define(iftRequest_setMailbox, 11).
-define(iftRequest_internalForPeer, 12).
-define(iftRequest_internalBad, 91).
-define(iftRequest_version, 92).
-define(iftRequest_boardTime, 93).

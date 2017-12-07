%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_lici.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_lici).
-vsn('/main/R1A/4').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([initiate_service/1,
	 terminate_service/1]).

-export([feature_subscribe/2,
	 feature_unsubscribe/2,
	 capacity_subscribe/2,
	 status_subscribe/1,
	 is_LKF_installed/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec initiate_service(WantedPvList) -> Result when
      WantedPvList :: [WantedPv, ...],
      Result ::	{lici_init_cfm, LiciId, SignalRevision, SelectedPV} |
		{lici_init_rej, SignalRevision, RejectReason} |
		{lici_init_sus, SignalRevision, SelectedPV},
      WantedPv :: protocol_version(),
      LiciId :: lici_id(),
      SignalRevision :: signal_revision(),
      SelectedPV :: protocol_version(),
      RejectReason :: reject_reason().
initiate_service(WantedPvList) when is_list(WantedPvList) ->
    SignalRevision = 1,
    lih_lici_server:initiate_service({SignalRevision, WantedPvList}).

-spec terminate_service(LiciId) ->  Result when
      LiciId :: lici_id(),
      Result :: lici_term_cfm.
terminate_service(LiciId) ->
    lih_lici_server:terminate_service(LiciId).

-spec feature_subscribe(LiciId, FeatureId) -> Result when
      LiciId :: lici_id(),
      FeatureId :: string(),
      Result :: {lici_feature_subscr_cfm, FeatureId, FeatureStatus,
		 ChangeReason} |
		{lici_feature_subscr_rej, FeatureId, RejectReason},
      FeatureStatus :: feature_status(),
      ChangeReason :: change_reason(),
      RejectReason :: reject_reason().
feature_subscribe(LiciId, FeatureId) when is_list(FeatureId) ->
    lih_lici_worker:feature_subscribe(LiciId, FeatureId).

-spec feature_unsubscribe(LiciId, FeatureId) -> Result when
      LiciId :: lici_id(),
      FeatureId :: string(),
      Result :: {lici_feature_unsubscr_cfm, FeatureId}.
feature_unsubscribe(LiciId, FeatureId) when is_list(FeatureId) ->
    lih_lici_worker:feature_unsubscribe(LiciId, FeatureId).

-spec capacity_subscribe(LiciId, CapacityId) -> Result when
      LiciId :: lici_id(),
      CapacityId :: string(),
      Result :: {lici_capacity_subscr_cfm, CapacityId, LicensedLevel,
		 HardLimit, ChangeReason} |
		{lici_capacity_subscr_rej, CapacityId, RejectReason},
      LicensedLevel :: capacity_limit(),
      HardLimit :: capacity_limit(),
      ChangeReason :: change_reason(),
      RejectReason :: reject_reason().
capacity_subscribe(LiciId, CapacityId) when is_list(CapacityId) ->
    lih_lici_worker:capacity_subscribe(LiciId, CapacityId).

-spec status_subscribe(LiciId) -> Result when
      LiciId :: lici_id(),
      Result :: {lici_status_subscr_cfm, LicMgrStatus, EmergencyCounter} |
		{lici_status_subscr_rej, RejectReason},
      LicMgrStatus :: lic_mgr_status(),
      EmergencyCounter :: emergency_counter(),
      RejectReason :: reject_reason().
status_subscribe(LiciId) ->
    lih_lici_worker:status_subscribe(LiciId).

-spec is_LKF_installed(LiciId) -> Result when
      LiciId :: lici_id(),
      Result :: {lici_is_lkf_installed_rsp, SignalRevision, LKFResult},
      SignalRevision :: signal_revision(),
      LKFResult :: boolean().
is_LKF_installed(LiciId) ->
    lih_lici_worker:is_LKF_installed(LiciId).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

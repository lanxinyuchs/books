%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_lihi.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_lihi).
-vsn('/main/R1A/2').
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
	 capacity_subscribe/2,
	 grace_period_activated/2,
	 feature_unsubscribe/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici.hrl").
-include("lih_lihi.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec initiate_service(WantedPvList) -> Result when
      WantedPvList :: [WantedPv, ...],
      Result ::	{lihi_init_cfm, LihiId, SelectedPV} |
		{lihi_init_rej, RejectReason} |
		{lihi_init_sus, SelectedPV},
      WantedPv :: protocol_version(),
      LihiId :: lihi_id(),
      SelectedPV :: protocol_version(),
      RejectReason :: reject_reason().
initiate_service(WantedPvList) ->
    lih_lihi_server:initiate_service(WantedPvList).

-spec terminate_service(LihiId) -> Result when
      LihiId :: lihi_id(),
      Result :: lihi_term_cfm.
terminate_service(LihiId) ->
    lih_lihi_server:terminate_service(LihiId).

-spec feature_subscribe(LihiId, {FeatureKeyId, FeatureRdn}) -> Result when
      LihiId :: lihi_id(),
      FeatureKeyId :: feature_key_id(),
      FeatureRdn :: feature_rdn(),
      Result :: {lici_feature_subscr_cfm, FeatureKeyId, ServiceState} |
		{lici_feature_subscr_rej, FeatureKeyId, RejectReason},
      ServiceState :: service_state(),
      RejectReason :: reject_reason().
feature_subscribe(LihiId, {FeatureKeyId, FeatureRdn}) ->
    lih_lihi_worker:feature_subscribe(LihiId, {FeatureKeyId, FeatureRdn}).

-spec feature_unsubscribe(LihiId, {FeatureKeyId, FeatureRdn}) -> Result when
      LihiId :: lihi_id(),
      FeatureKeyId :: feature_key_id(),
      FeatureRdn :: feature_rdn(),
      Result :: {lihi_feature_unsubscr_cfm, FeatureKeyId}.
feature_unsubscribe(LihiId, {FeatureKeyId, FeatureRdn}) ->
    lih_lihi_worker:feature_unsubscribe(LihiId, {FeatureKeyId, FeatureRdn}).

-spec capacity_subscribe(LihiId,
			 {CapacityKeyId, CapacityRdn, CapacityUnit}) -> Result when
      LihiId :: lihi_id(),
      CapacityKeyId :: capacity_key_id(),
      CapacityRdn :: capacity_rdn(),
      CapacityUnit :: capacity_unit(),
      Result :: {lihi_capacity_subscr_cfm, CapacityKeyId, CapacityLimit,
		 GracePeriodAvailable},
      CapacityLimit :: capacity_limit(),
      GracePeriodAvailable :: boolean().
capacity_subscribe(LihiId, {CapacityKeyId, CapacityRdn, CapacityUnit}) ->
    lih_lihi_worker:capacity_subscribe(LihiId,
				       {CapacityKeyId, CapacityRdn, CapacityUnit}).

-spec grace_period_activated(LihiId,
			     {CapacityKeyId, GracePeriodActivated}) -> Result when
      LihiId :: lihi_id(),
      CapacityKeyId :: capacity_key_id(),
      GracePeriodActivated :: boolean(),
      Result :: ok.
grace_period_activated(LihiId, {CapacityKeyId, GracePeriodActivated}) ->
    lih_lihi_worker:grace_period_activated(LihiId,
					   {CapacityKeyId,
					    GracePeriodActivated}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

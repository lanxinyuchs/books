%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_erlang_handler.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_erlang_handler).
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

-export([send_lici_init_cfm/2,
	 send_lici_init_rej/2,
	 send_lici_init_sus/2,
	 send_lici_term_cfm/1,
	 send_lici_feature_subscr_cfm/2,
	 send_lici_feature_subscr_rej/2,
	 send_lici_feature_change_ind/2,
	 send_lici_feature_unsubscr_cfm/2,
	 send_lici_capacity_subscr_cfm/2,
	 send_lici_capacity_subscr_rej/2,
	 send_lici_capacity_change_ind/2,
	 send_lici_status_subscr_cfm/2,
	 send_lici_status_subscr_rej/2,
	 send_lici_status_change_ind/2,
	 send_lici_is_lkf_installed_rsp/2
	]).

-export([
	 send_lihi_init_cfm/2,
	 send_lihi_init_rej/2,
	 send_lihi_init_sus/2,
	 send_lihi_term_cfm/1,
	 send_lihi_feature_subscr_cfm/2,
	 send_lihi_feature_subscr_rej/2,
	 send_lihi_feature_change_ind/2,
	 send_lihi_feature_unsubscr_cfm/2,
	 send_lihi_capacity_subscr_cfm/2,
	 send_lihi_capacity_subscr_rej/2,
	 send_lihi_capacity_change_ind/2
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici.hrl").
-include("lih_lihi.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

send_lici_init_cfm(From, {SignalRevision, SelectedPV}) ->
    gen_server:reply(From, {lici_init_cfm, self(), SignalRevision, SelectedPV}).

send_lici_init_rej(From, {SignalRevision, RejectReason}) ->
    gen_server:reply(From, {lici_init_rej, SignalRevision,
			    convert_RejectReason(RejectReason)}).

send_lici_init_sus(From, {SignalRevision, SelectedPV}) ->
    gen_server:reply(From, {lici_init_sus, SignalRevision, SelectedPV}).

send_lici_term_cfm(From) ->
    gen_server:reply(From, lici_term_cfm).

send_lici_feature_subscr_cfm(From, {FeatureId, FeatureStatus, ChangeReason}) ->
    gen_server:reply(From,
		     {lici_feature_subscr_cfm, FeatureId,
		      convert_FeatureStatus(FeatureStatus),
		      convert_ChangeReason(ChangeReason)}).

send_lici_feature_subscr_rej(From, {FeatureId, RejectReason}) ->
    gen_server:reply(From,
		     {lici_feature_subscr_rej, FeatureId,
		      convert_RejectReason(RejectReason)}).

send_lici_feature_change_ind(Pid, {FeatureId, FeatureStatus, ChangeReason}) ->
    Pid ! {lici_feature_change_ind, FeatureId,
	   convert_FeatureStatus(FeatureStatus),
	   convert_ChangeReason(ChangeReason)}.

send_lici_feature_unsubscr_cfm(From, FeatureKeyId) ->
    gen_server:reply(From, {lici_feature_unsubscr_cfm, FeatureKeyId}).

send_lici_capacity_subscr_cfm(From,
			      {CapacityId, LicensedLevel, HardLimit,
			       ChangeReason}) ->
    gen_server:reply(From,
		     {lici_capacity_subscr_cfm, CapacityId,
		      convert_capacity_limit(LicensedLevel),
		      convert_capacity_limit(HardLimit),
		      convert_ChangeReason(ChangeReason)}).

send_lici_capacity_subscr_rej(From, {CapacityId, RejectReason}) ->
    gen_server:reply(From,
		     {lici_capacity_subscr_rej, CapacityId,
		      convert_RejectReason(RejectReason)}).

send_lici_capacity_change_ind(Pid, {CapacityId, LicensedLevel, HardLimit,
				    ChangeReason}) ->
    Pid ! {lici_capacity_change_ind, CapacityId,
	   convert_capacity_limit(LicensedLevel),
	   convert_capacity_limit(HardLimit),
	   convert_ChangeReason(ChangeReason)}.

send_lici_status_subscr_cfm(From, {LicMgrStatus, EmergencyCounter}) ->
    gen_server:reply(From, {lici_status_subscr_cfm,
			    convert_LicMgrStatus(LicMgrStatus),
			    convert_EmergencyCounter(EmergencyCounter)}).

send_lici_status_subscr_rej(From, RejectReason) ->
    gen_server:reply(From, {lici_status_subscr_rej,
			    convert_RejectReason(RejectReason)}).

send_lici_status_change_ind(Pid, {LicMgrStatus, EmergencyCounter}) ->
    Pid ! {lici_status_change_ind,
	   convert_LicMgrStatus(LicMgrStatus),
	   convert_EmergencyCounter(EmergencyCounter)}.

send_lici_is_lkf_installed_rsp(From, {SignalRev, LFKResult}) ->
    gen_server:reply(From,
		     {lici_is_lkf_installed_rsp, SignalRev,
		      convert_LFKResult(LFKResult)}).

send_lihi_init_cfm(From, SelectedPV) ->
    gen_server:reply(From, {lihi_init_cfm, self(), SelectedPV}).

send_lihi_init_rej(From, RejectReason) ->
    gen_server:reply(From, {lihi_init_rej, convert_RejectReason(RejectReason)}).

send_lihi_init_sus(From, SelectedPV) ->
    gen_server:reply(From, {lihi_init_sus, SelectedPV}).

send_lihi_term_cfm(From) ->
    gen_server:reply(From, lihi_term_cfm).

send_lihi_feature_subscr_cfm(From, {FeatureKeyId, ServiceState}) ->
    gen_server:reply(From,
		     {lihi_feature_subscr_cfm, FeatureKeyId,
		      convert_ServiceState(ServiceState)}).

send_lihi_feature_subscr_rej(From, {FeatureKeyId, RejectReason}) ->
    gen_server:reply(From,
		     {lihi_feature_subscr_rej, FeatureKeyId,
		      convert_RejectReason(RejectReason)}).

send_lihi_feature_change_ind(Pid, {FeatureKeyId, ServiceState}) ->
    Pid ! {lihi_feature_change_ind, FeatureKeyId,
	   convert_ServiceState(ServiceState)}.

send_lihi_feature_unsubscr_cfm(From, FeatureKeyId) ->
    gen_server:reply(From, {lihi_feature_unsubscr_cfm, FeatureKeyId}).

send_lihi_capacity_subscr_cfm(From,
			      {CapacityKeyId, CapacityLimit,
			       GracePeriodAvailable}) ->
    gen_server:reply(From,
		     {lici_capacity_subscr_cfm, CapacityKeyId,
		      convert_capacity_limit(CapacityLimit),
		      GracePeriodAvailable}).

send_lihi_capacity_subscr_rej(From, {CapacityKeyId, RejectReason}) ->
    gen_server:reply(From,
		     {lihi_capacity_subscr_rej, CapacityKeyId,
		      convert_RejectReason(RejectReason)}).

send_lihi_capacity_change_ind(Pid, {CapacityKeyId, CapacityLimit,
				    GracePeriodAvailable}) ->
    Pid ! {lihi_capacity_change_ind, CapacityKeyId,
	   convert_capacity_limit(CapacityLimit), GracePeriodAvailable}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

convert_ChangeReason(?CELLO_LICI_LICENSED_VALUE)   -> licensed_value;
convert_ChangeReason(?CELLO_LICI_EMERGENCY_REASON) -> emergency_reason;
convert_ChangeReason(?CELLO_LICI_NOT_ACTIVATED)    -> not_activated.

convert_RejectReason(?CELLO_LICI_UNEXPECTED_ERROR)         ->
    unexpected_error;
convert_RejectReason(?CELLO_LICI_ALREADY_SUBSCRIBED)       ->
    already_subscribed;
convert_RejectReason(?CELLO_LICI_INVALID_PROTOCOL_VERSION) ->
    invalid_protocol_version.

convert_LicMgrStatus(?CELLO_LICI_EMERGENCY_ACTIVATED)   -> activated;
convert_LicMgrStatus(?CELLO_LICI_EMERGENCY_DEACTIVATED) -> deactivated.

convert_capacity_limit(?LICI_NO_LIMIT) -> nolimit;
convert_capacity_limit(?LICI_LIMIT(Value)) -> Value.

convert_FeatureStatus(?CELLO_LICI_FEATURE_ENABLED)  -> enabled;
convert_FeatureStatus(?CELLO_LICI_FEATURE_DISABLED) -> disabled.

convert_EmergencyCounter(?CELLO_LICI_NO_EMERGENCY)    -> no_emergency;
convert_EmergencyCounter(?CELLO_LICI_EMERGENCY_ONCE)  -> emergency_once;
convert_EmergencyCounter(?CELLO_LICI_EMERGENCY_TWICE) -> emergency_twice.

convert_LFKResult(?CELLO_LICI_LKF_INSTALLED)     -> true;
convert_LFKResult(?CELLO_LICI_LKF_NOT_INSTALLED) -> false.

convert_ServiceState(?CELLO_LIHI_SERVICE_OPERABLE)   -> operable;
convert_ServiceState(?CELLO_LIHI_SERVICE_INOPERABLE) -> inoperable.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

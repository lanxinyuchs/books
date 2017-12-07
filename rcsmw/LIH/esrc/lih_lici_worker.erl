%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_lici_worker.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_lici_worker).
-vsn('/main/R1A/R2A/R5A/1').
-author('etxpeno').
-behaviour(gen_server).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R1A/4      2012-03-26 etxpeno     Removed io:format
%%% R5A/1      2016-02-23 etxpeno     TR HU43928 handle restarts of subscribing applications
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([
	 start/1,
	 start_link/1,
	 stop/1
	]).

-export([
	 get_info/1
	]).

%%% API used by lih_lici_server.erl
-export([
	 feature_changed/2,
	 capacity_changed/2,
	 status_changed/2
	]).

%%% API used by lici.erl
-export([
	 feature_subscribe/2,
	 feature_unsubscribe/2,
	 capacity_subscribe/2,
	 status_subscribe/1,
	 is_LKF_installed/1
	]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici_sig.hrl").
-include("lih_lici.hrl").

-record(state, {
	  module :: atom(),
	  appl_pid,
	  signal_revision :: non_neg_integer(),
	  selected_pv :: non_neg_integer(),
	  appl_ref :: reference() | 'undefined'
	 }
       ).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start({Spid, SignalRevision, SelectedPV}) ->
    Args = {Spid, SignalRevision, SelectedPV},
    gen_server:start(?MODULE, Args, []).

start_link({Spid, SignalRevision, SelectedPV}) ->
    Args = {Spid, SignalRevision, SelectedPV},
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getInfo).

feature_changed(PidList, {FeatureId, FeatureStatus, ChangeReason}) ->
    lists:foreach(
      fun(Pid) ->
	      gen_server:cast(Pid,
			      {featureChanged,
			       {FeatureId, FeatureStatus, ChangeReason}})
      end, PidList).

capacity_changed(PidList,
		 {CapacityId, LicensedLevel, HardLimit, ChangeReason}) ->
    lists:foreach(
      fun(Pid) ->
	      gen_server:cast(Pid,
			      {capacityChanged,
			       {CapacityId, LicensedLevel,
				HardLimit, ChangeReason}})
      end, PidList).

status_changed(PidList, {LicMgrStatus, EmergencyCounter}) ->
    lists:foreach(
      fun(Pid) ->
	      gen_server:cast(Pid,
			      {statusChanged,
			       {LicMgrStatus, EmergencyCounter}})
      end, PidList).

feature_subscribe(Pid, FeatureId) ->
    gen_server:call(Pid, {featureSubscribe, FeatureId}).

feature_unsubscribe(Pid, FeatureId) ->
    gen_server:call(Pid, {featureUnsubscribe, FeatureId}).

capacity_subscribe(Pid, CapacityId) ->
    gen_server:call(Pid, {capacitySubscribe, CapacityId}).

status_subscribe(Pid) ->
    gen_server:call(Pid, statusSubscribe).

is_LKF_installed(Pid) ->
    gen_server:call(Pid, isLkfInstalled).

init({From, SignalRevision, SelectedPV}) ->
    erlang:process_flag(trap_exit, true),

    case From of
	Spid when is_integer(Spid) ->
	    Module = lih_linx_handler,
	    Name = "LICI_"++integer_to_list(Spid),
	    Port = itc:open(Name),
	    ApplPid = {Port, Spid},
	    ApplRef = undefined,

	    Module:send_lici_init_cfm(ApplPid, {SignalRevision, SelectedPV}),

	    itc:attach(Port, Spid),
	    itc:listen(Port);
	{Spid, _Tag} ->
	    ApplPid = Spid,
	    Module = lih_erlang_handler,
	    ApplRef = monitor(process, ApplPid),

	    Module:send_lici_init_cfm(From, {SignalRevision, SelectedPV})
    end,

    {ok, #state{module          = Module,
		appl_pid        = ApplPid,
		signal_revision = SignalRevision,
		selected_pv     = SelectedPV,
		appl_ref        = ApplRef}}.

handle_call({featureSubscribe, RawFeatureId}, From,
	    #state{module      = Module,
		   selected_pv = Pv} = S) ->
    handle_feature_subscribe_req(Module, From, RawFeatureId, Pv),
    {noreply, S};

handle_call({featureUnsubscribe, RawFeatureId}, From,
	    #state{module = Module} = S) ->
    handle_feature_unsubscribe_req(Module, From, RawFeatureId),
    {noreply, S};

handle_call({capacitySubscribe, RawCapacityId}, From,
	    #state{module      = Module,
		   selected_pv = Pv} = S) ->
    handle_capacity_subscribe_req(Module, From, RawCapacityId, Pv),
    {noreply, S};

handle_call(statusSubscribe, From,
	    #state{module = Module} = S) ->
    handle_status_subscribe_req(Module, From),
    {noreply, S};

handle_call(isLkfInstalled, From,
	    #state{module          = Module,
		   signal_revision = SignalRevision} = S) ->
    handle_is_lkf_installed_req(Module, From, SignalRevision),
    {noreply, S};

handle_call(getInfo, _From, #state{appl_pid        = ApplPid,
				   signal_revision = SignalRevision,
				   selected_pv     = SelectedPv,
				   appl_ref        = ApplRef} = S) ->
    Reply = [{appl_pid,        ApplPid},
	     {signal_revision, SignalRevision},
	     {selected_pv,     SelectedPv},
	     {appl_ref,        ApplRef}
	    ],
    {reply, Reply, S};
handle_call(_Msg, _From, S)->
    {reply, _Msg, S}.

handle_cast({featureChanged, {FeatureId, FeatureStatus, ChangeReason}},
	    #state{module = Module, appl_pid = ApplPid} = S) ->
    NewChangeReason = get_new_change_reason(ChangeReason, S),
    Module:send_lici_feature_change_ind(ApplPid,
					{FeatureId, FeatureStatus,
					 NewChangeReason}),
    {noreply, S};

handle_cast({capacityChanged,
	     {CapacityId, LicensedLevel, HardLimit, ChangeReason}},
	    #state{module = Module, appl_pid = ApplPid} = S) ->
    NewChangeReason = get_new_change_reason(ChangeReason, S),
    Module:send_lici_capacity_change_ind(ApplPid,
					 {CapacityId, LicensedLevel, HardLimit,
					  NewChangeReason}),
    {noreply, S};

handle_cast({statusChanged, {LicMgrStatus, EmergencyCounter}},
	    #state{module = Module, appl_pid = ApplPid} = S) ->
    Module:send_lici_status_change_ind(ApplPid,
				       {LicMgrStatus, EmergencyCounter}),
    {noreply, S};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({message, Port, {Spid, _ToMboxId, SigNo, Data}},
	    #state{module          = Module,
		   appl_pid        = ApplPid = {Port, Spid},
		   selected_pv     = Pv,
		   signal_revision = SignalRevision} = S) ->
    case lih_linx_handler:parse_linx({SigNo, Data}) of
	{lici_feature_subscribe_req, RawFeatureId} ->
	    handle_feature_subscribe_req(Module, ApplPid, RawFeatureId, Pv),
	    {noreply, S};
	{lici_capacity_subscribe_req, RawCapacityId} ->
	    handle_capacity_subscribe_req(Module, ApplPid, RawCapacityId, Pv),
	    {noreply, S};
	lici_status_subscribe_req ->
	    handle_status_subscribe_req(Module, ApplPid),
	    {noreply, S};
	lici_is_lkf_installed_req ->
	    handle_is_lkf_installed_req(Module, ApplPid, SignalRevision),
	    {noreply, S};
	lici_client_down_ind ->
	    Reason = normal,
	    {stop, Reason, S};
	_ ->
	    {noreply, S}
    end;
handle_info({mailbox_down, Port, _Ref, Spid},
	    #state{appl_pid = {Port, Spid}} = S) ->
    Reason = normal,
    {stop, Reason, S};
handle_info({'DOWN', ApplRef, process, ApplPid, Reason},
            #state{appl_pid = ApplPid,
                   appl_ref = ApplRef} = S) ->
    {stop, Reason, S};
handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

handle_feature_subscribe_req(Module, ApplPid, RawFeatureId, Pv) ->
    case lih_lici_server:add_subscription({feature, RawFeatureId}) of
	{false, FeatureId, RejectReason} ->
	    Module:send_lici_feature_subscr_rej(ApplPid,
						{FeatureId, RejectReason});
	{true, FeatureId, FeatureStatus, ChangeReason} ->
	    NewChangeReason = get_new_change_reason(ChangeReason, Pv),
	    Module:send_lici_feature_subscr_cfm(ApplPid,
						{FeatureId, FeatureStatus,
						 NewChangeReason})
    end.

handle_feature_unsubscribe_req(Module, ApplPid, RawFeatureId) ->
    {true, FeatureId} =
	lih_lici_server:remove_subscription({feature, RawFeatureId}),

    Module:send_lici_feature_unsubscr_cfm(ApplPid, FeatureId).

handle_capacity_subscribe_req(Module, ApplPid, RawCapacityId, Pv) ->
    case lih_lici_server:add_subscription({capacity, RawCapacityId}) of
	{false, CapacityId, RejectReason} ->
	    Module:send_lici_capacity_subscr_rej(ApplPid,
						 {CapacityId, RejectReason});
	{true, CapacityId, LicensedLevel, HardLimit, ChangeReason} ->
	    NewChangeReason = get_new_change_reason(ChangeReason, Pv),
	    Module:send_lici_capacity_subscr_cfm(ApplPid,
						 {CapacityId,LicensedLevel,
						  HardLimit, NewChangeReason})
    end.

handle_status_subscribe_req(Module, ApplPid) ->
    case lih_lici_server:add_subscription(status) of
	{false, RejectReason} ->
	    Module:send_lici_status_subscr_rej(ApplPid, RejectReason);
	{true, LicMgrStatus, EmergencyCounter} ->
	    Module:send_lici_status_subscr_cfm(ApplPid,
					       {LicMgrStatus, EmergencyCounter})
    end.

handle_is_lkf_installed_req(Module, ApplPid, SignalRevision) ->
    LFKResult = lih_lici_server:get_LKF_installed_rsp(),
    Module:send_lici_is_lkf_installed_rsp(ApplPid, {SignalRevision, LFKResult}).

get_new_change_reason(?CELLO_LICI_NOT_ACTIVATED, ?CELLO_LICI_PV1) ->
    %% Change reason CELLO_LICI_NOT_ACTIVATED was introduced in
    %% protocol version 2. Use change reason CELLO_LICI_LICENSED_VALUE instead
    %% if protocol version 1 is selected
    ?CELLO_LICI_LICENSED_VALUE;
get_new_change_reason(ChangeReason, _Pv) ->
    ChangeReason.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_lihi_worker.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_lihi_worker).
-vsn('/main/R1A/R2A/1').
-author('etxpeno').
-behaviour(gen_server).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2013 All rights reserved.
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
%%% R1A/3      2012-03-01 etxpeno     Dialyzer fixes
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

%%% API used by lih_lihi_server.erl
-export([
         feature_changed/2
        ]).

-export([
	 feature_subscribe/2,
	 capacity_subscribe/2,
	 grace_period_activated/2,
	 feature_unsubscribe/2
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

-record(state, {
	  module :: atom(),
	  appl_pid,
	  selected_pv :: non_neg_integer(),
	  appl_ref :: reference() | 'undefined'
	 }).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start(Args) ->
    gen_server:start(?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

feature_subscribe(Pid, {FeatureKeyId, FeatureRdn}) ->
    gen_server:call(Pid, {featureSubscribe, FeatureKeyId, FeatureRdn}).

feature_unsubscribe(Pid, {FeatureKeyId, FeatureRdn}) ->
    gen_server:call(Pid, {featureUnsubscribe, FeatureKeyId, FeatureRdn}).

capacity_subscribe(Pid, {CapacityKeyId, CapacityRdn, CapacityUnit}) ->
    gen_server:call(Pid, {capacitySubscribe, CapacityKeyId, CapacityRdn,
			  CapacityUnit}).

grace_period_activated(Pid, {CapacityKeyId, GracePeriodActivated}) ->
    gen_server:call(Pid, {gracePeriodActivated, CapacityKeyId,
			  GracePeriodActivated}).

feature_changed(PidList, {FeatureKeyId, ServiceState}) ->
    lists:foreach(
      fun(Pid) ->
              gen_server:cast(Pid,
                              {featureChanged, {FeatureKeyId, ServiceState}})
      end, PidList).

get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, getInfo).

init({From, SelectedPV}) ->
    erlang:process_flag(trap_exit, true),

    case From of
        Spid when is_integer(Spid) ->
            Module = lih_linx_handler,
            Name = "LIHI_"++integer_to_list(Spid),
            Port = itc:open(Name),
            ApplPid = {Port, Spid},
	    ApplRef = undefined,

            Module:send_lihi_init_cfm(ApplPid, SelectedPV),

            itc:attach(Port, Spid),
            itc:listen(Port);
        {Spid, _Tag} ->
            ApplPid = Spid,
            Module = lih_erlang_handler,
	    ApplRef = monitor(process, ApplPid),

            Module:send_lihi_init_cfm(From, SelectedPV)
    end,

    {ok, #state{module      = Module,
                appl_pid    = ApplPid,
                selected_pv = SelectedPV,
		appl_ref    = ApplRef}}.

handle_call(getInfo, _From, #state{module      = Module,
				   appl_pid    = ApplPid,
				   selected_pv = SelectedPv,
				   appl_ref    = ApplRef} = S) ->
    Reply = [{module,      Module},
	     {appl_pid,    ApplPid},
             {selected_pv, SelectedPv},
	     {appl_ref,    ApplRef}],
    {reply, Reply, S};

handle_call({featureSubscribe, FeatureKeyId, FeatureRdn}, From,
	    #state{module = Module} = S) ->
    handle_feature_subscribe_req(Module, From, FeatureKeyId, FeatureRdn),
    {noreply, S};
handle_call({featureUnsubscribe, FeatureKeyId, FeatureRdn}, From,
	    #state{module = Module} = S) ->
    handle_feature_unsubscribe_req(Module, From, FeatureKeyId, FeatureRdn),
    {noreply, S};
handle_call({capacitySubscribe, CapacityKeyId, CapacityRdn, CapacityUnit},
	    From,
	    #state{module = Module} = S) ->
    handle_capacity_subscribe_req(Module, From, CapacityKeyId,
				  CapacityRdn, CapacityUnit),
    {noreply, S};
handle_call({gracePeriodActivated, CapacityKeyId, GracePeriodActivated},
	    _From, S) ->
    handle_grace_period_activated_req(CapacityKeyId, GracePeriodActivated),
    Reply = ok,
    {reply, Reply, S};
handle_call(_Msg, _From, S)->
    {reply, _Msg, S}.

handle_cast({featureChanged, {FeatureKeyId, ServiceState}},
            #state{module = Module, appl_pid = ApplPid} = S) ->
    Module:send_lihi_feature_change_ind(ApplPid, {FeatureKeyId, ServiceState}),
    {noreply, S};
handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({mailbox_down, Port, _Ref, Spid},
            #state{appl_pid = {Port, Spid}} = S) ->
    itc:send(Port, Spid, ?OSA_LICI_CLIENT_DOWN_IND, <<>>),
    {noreply, S};
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

handle_feature_subscribe_req(Module, ApplPid, RawFeatureKeyId, FeatureRdn) ->
    case lih_lihi_server:add_subscription({feature, RawFeatureKeyId,
					   FeatureRdn}) of
	{false, FeatureKeyId, RejectReason} ->
	    Module:send_lihi_feature_subscr_rej(ApplPid,
						{FeatureKeyId, RejectReason});
	{true, FeatureKeyId, ServiceState} ->
	    Module:send_lihi_feature_subscr_cfm(ApplPid,
						{FeatureKeyId, ServiceState})
    end.

handle_feature_unsubscribe_req(Module, ApplPid, RawFeatureKeyId, FeatureRdn) ->
    {true, FeatureKeyId} =
	lih_lihi_server:remove_subscription({feature, RawFeatureKeyId,
					     FeatureRdn}),
    Module:send_lihi_feature_unsubscr_cfm(ApplPid, FeatureKeyId).

handle_capacity_subscribe_req(Module, ApplPid, RawCapacityKeyId,
			      CapacityRdn, CapacityUnit) ->
    case lih_lihi_server:add_subscription({capacity, RawCapacityKeyId,
					   CapacityRdn, CapacityUnit}) of
	{false, CapacityKeyId, RejectReason} ->
	    Module:send_lihi_capacity_subscr_rej(ApplPid,
						 {CapacityKeyId, RejectReason});
	{true, CapacityKeyId, CapacityLimit, GracePeriodAvailable} ->
	    Module:send_lihi_feature_subscr_cfm(ApplPid,
						{CapacityKeyId, CapacityLimit,
						 GracePeriodAvailable})
    end.

handle_grace_period_activated_req(_CapacityKeyId, _GracePeriodActivated) ->
    ok.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

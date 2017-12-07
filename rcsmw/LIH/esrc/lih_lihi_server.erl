%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_lihi_server.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_lihi_server).
-vsn('/main/R1A/R2A/R7A/1').
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
%%% R1A/4      2012-03-26 etxpeno     Dialyzer fixes
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([
         start/0,
         start_link/0,
         stop/0
        ]).

-export([
         activate/0,
         get_info/0
	]).

-export([
         initiate_service/1,
         terminate_service/1
        ]).

-export([add_subscription/1,
	 remove_subscription/1]).

-export([update_feature_state/1]).

%% Internal gen_server API
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

-include("lih_lici.hrl").
-include("lih_lihi.hrl").

-record(state,
	{itc_port,
	 lici_pid :: pid() | 'undefined'}).

-define(SERVER, ?MODULE).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    Args = [],
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

start_link() ->
    Args = [],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, stop).

activate() ->
    gen_server:cast(?MODULE, activate).

-spec get_info() -> [{atom(), term()}].
get_info() ->
    gen_server:call(?SERVER, getInfo).

initiate_service(WantedPvList) ->
    try gen_server:call(?SERVER, {initiateService, WantedPvList}) of
	{lihi_init_cfm, Pid, _SelectedPV} = Result ->
	    put({lihi_ref, Pid}, monitor(process, Pid)),
	    Result;
	Result ->
	    Result
    catch
	exit:{noproc, _Reason} ->
	    timer:sleep(100),
	    initiate_service(WantedPvList)
    end.

terminate_service(Pid) ->
    case erase({lihi_ref, Pid}) of
	undefined                  -> ok;
	Ref when is_reference(Ref) -> demonitor(Ref, [flush])
    end,

    gen_server:call(?SERVER, {terminateService, Pid}).

add_subscription(SubscriptionId) ->
    gen_server:call(?SERVER, {addSubscription, SubscriptionId, self()}).

remove_subscription(SubscriptionId) ->
    gen_server:call(?SERVER, {removeSubscription, SubscriptionId, self()}).

-spec update_feature_state({KeyId, Rdn, FeatureState}) -> ok when
      KeyId :: feature_key_id(),
      Rdn :: feature_rdn(),
      FeatureState :: feature_state().
update_feature_state({KeyId, Rdn, FeatureState}) ->
    gen_server:call(?SERVER, {updateFeatureState, KeyId, Rdn, FeatureState}).

-spec init(_) -> {'ok',#state{}}.
init(_Args) ->
    erlang:process_flag(trap_exit, true),

    lihi_feature_subscription =
	ets:new(lihi_feature_subscription,
		[named_table, {keypos, #lihi_feature_subscription.id}]),
    lihi_feature_license =
	ets:new(lihi_feature_license,
		[named_table, {keypos, #lihi_feature_license.id}]),
    lih_worker =
	ets:new(lih_worker, [named_table, bag, {keypos, #lih_worker.spid}]),

    case is_local_restart() of
	false ->
	    {ok, #state{}};
	true ->
	    {ok, handle_activate(#state{})}
    end.

handle_call({initiateService, WantedPvList}, From, S) ->
    {noreply, handle_init_req({From, WantedPvList}, S)};

handle_call({terminateService, Pid}, From, S) ->
    {noreply, handle_term_req({From, Pid}, S)};

handle_call({addSubscription, {feature, RawKeyId, Rdn}, Pid}, _From, S) ->
    KeyId = lih_lib:convert_id(RawKeyId),
    Reply = add_feature_subscription({KeyId, Rdn, Pid}, S),
    {reply, Reply, S};

handle_call({removeSubscription, {feature, RawKeyId, Rdn}, Pid},
	    _From, S) ->
    KeyId = lih_lib:convert_id(RawKeyId),
    Reply = remove_feature_subscription({KeyId, Rdn, Pid}, S),
    {reply, Reply, S};

handle_call({updateFeatureState, KeyId, Rdn, FeatureState}, _From, S) ->
    Id = {KeyId, Rdn},
    Subscriptions = ets:lookup(lihi_feature_subscription, Id),

    update_feature_subscription(Subscriptions, [{feature, FeatureState}]),

    Reply = ok,

    {reply, Reply, S};

handle_call(getInfo, _From, S) ->
    handle_get_info(S);
handle_call(_Msg, _From, S) ->
    {reply, _Msg, S}.

handle_cast(activate, S) ->
    {noreply, handle_activate(S)};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({lici_feature_change_ind, KeyId, NewLicenseState, _ChangeReason},
	    S) ->
    update_feature_subscriptions({KeyId, NewLicenseState}),
    {noreply, S};

handle_info({'EXIT', Pid, _What}, S) ->
    remove_feature_subscriptions(Pid, S),
    remove_workers(Pid),
    {noreply, S};

handle_info({'DOWN', _Ref, process, OldLiciPid, _What},
            #state{lici_pid = OldLiciPid} = S) ->
    %% LICI is down. Initiate LICI service again.
    {lici_init_cfm, LiciPid, _SignalRevision, ?CELLO_LICI_PV3} =
	lih_lici:initiate_service([?CELLO_LICI_PV3]),
    NewState = S#state{lici_pid = LiciPid},
    reread_feature_licenses(NewState),
    {noreply, NewState};
handle_info({'DOWN', _Ref, process, _Pid, _What}, S) ->
    check_lihi_checker(),
    {noreply, S};

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, S) ->
    lici_term_cfm = lih_lici:terminate_service(S#state.lici_pid),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

handle_get_info(S) ->
    Reply = [
             {itc_port,              S#state.itc_port},
	     {workers,               get_workers()},
	     {feature_subscriptions, get_feature_subscriptions()},
	     {feature_licenses,      get_feature_licenses()},
	     {lici_pid,              S#state.lici_pid}
	    ],

    {reply, Reply, S}.

handle_init_req({From, WantedPvList}, S) ->
    CheckPv = check_pv(WantedPvList),

    case From of
        {Spid, _Tag} ->
            Fun =
                fun(Arg1) ->
                        lih_erlang_handler:send_lihi_init_cfm(From, Arg1)
                end
    end,

    if
	CheckPv =:= false ->
            Fun(?CELLO_LIHI_INVALID_PROTOCOL_VERSION);
	true ->
	    {true, SelectedPV} = CheckPv,
            case lih_lihi_worker:start_link({From, SelectedPV}) of
                {ok, Pid} ->
		    add_worker({Spid, Pid});
                {error, _Error} ->
                    Fun(?CELLO_LIHI_UNEXPECTED_ERROR)
            end
    end,

    S.

handle_term_req({From, Pid}, S) ->
    case From of
        Spid when is_integer(Spid) ->
            ok;
        {Spid, _Tag} ->
            ok
    end,

    remove_worker({Spid, Pid}, S),

    if
        %% is_integer(Spid) ->
        %%     ApplPid = {S#state.linx_handler_pid, Spid},
        %%     lih_linx_handler:send_lihi_term_cfm(ApplPid);
        is_pid(Spid) ->
            lih_erlang_handler:send_lihi_term_cfm(From)
    end,

    S.

handle_activate(S) ->
    Port = itc:open("LIHI"),
    itc:listen(Port),

    {lici_init_cfm, LiciPid, _SignalRevision, ?CELLO_LICI_PV3} =
     	lih_lici:initiate_service([?CELLO_LICI_PV3]),

    S#state{itc_port = Port,
	    lici_pid = LiciPid}.

check_pv([])                                 -> false;
check_pv([Pv|_]) when Pv =:= ?CELLO_LIHI_PV1 -> {true, Pv};
check_pv([_|T])                              -> check_pv(T).

get_workers() ->
    ets:tab2list(lih_worker).

add_worker({Spid, Pid}) ->
    ets:insert(lih_worker, #lih_worker{spid = Spid, worker_pid = Pid}).

remove_worker({Spid, Pid}, S) ->
    Workers = ets:lookup(lih_worker, Spid),

    lists:foreach(
      fun(#lih_worker{worker_pid = Pid0} = Obj) when Pid =:= Pid0;
						     Pid =:= all ->
	      unlink(Pid0),
	      remove_feature_subscriptions(Pid0, S),
	      lih_lihi_worker:stop(Pid0),
	      ets:delete_object(lih_worker, Obj);
	 (_) ->
	      ok
      end, Workers).

remove_workers(Pid) ->
    true = ets:match_delete(lih_worker,
			    #lih_worker{worker_pid = Pid, _ = '_'}).

get_feature_subscriptions() ->
    ets:tab2list(lihi_feature_subscription).

add_feature_subscription({KeyId, Rdn, Pid}, S) ->
    Id = {KeyId, Rdn},
    case ets:lookup(lihi_feature_subscription, Id) of
        [] ->
	    LicenseState = add_feature_license(Id, S),
	    FeatureState = lih_db:get_feature_state(Id),
	    ServiceState = get_service_state(FeatureState, LicenseState),

	    lih_db:create_option_feature({KeyId, Rdn, FeatureState,
					  LicenseState, ServiceState}),

	    Subscription =
		#lihi_feature_subscription{id            = Id,
					   pid_list      = [Pid],
					   feature_state = FeatureState,
					   license_state = LicenseState,
					   service_state = ServiceState},
            ets:insert(lihi_feature_subscription, Subscription),

	    {true, KeyId, ServiceState};
	[Subscription] ->
	    PidList = Subscription#lihi_feature_subscription.pid_list,
            IsAlreadySubscribed = lists:member(Pid, PidList),
            if
                IsAlreadySubscribed ->
                    {false, KeyId, ?CELLO_LIHI_ALREADY_SUBSCRIBED};
		true ->
		    NewSubscription =
			Subscription#lihi_feature_subscription{pid_list = [Pid|PidList]},
		    ets:insert(lihi_feature_subscription, NewSubscription),
		    ServiceState =
			Subscription#lihi_feature_subscription.service_state,
		    {true, KeyId, ServiceState}
	    end
    end.

remove_feature_subscription({KeyId, Rdn, Pid}, S) ->
    Id = {KeyId, Rdn},
    case ets:lookup(lihi_feature_subscription, Id) of
        [] ->
	    ok;
	[Subscription] ->
	    PidList = Subscription#lihi_feature_subscription.pid_list,
	    case PidList -- [Pid] of
		[] ->
		    %% The last subscriber...
		    ets:delete(lihi_feature_subscription, Id),
		    ok = remove_feature_license(Id, S),
		    lih_db:delete_option_feature(Rdn);
		PidList ->
		    %% No change
		    ok;
		NewPidList ->
		    %% Still subscribers left..
		    NewSubscription =
			Subscription#lihi_feature_subscription{pid_list = NewPidList},
		    ets:insert(lihi_feature_subscription, NewSubscription)
	    end
    end,
    {true, KeyId}.

update_feature_subscriptions({KeyId, NewLicenseState}) ->
    RdnList = update_feature_license(KeyId, NewLicenseState),
    IdList = [{KeyId, Rdn} || Rdn <- RdnList],

    F = fun(Id) -> ets:lookup(lihi_feature_subscription, Id) end,
    Subscriptions = lists:flatmap(F, IdList),
    update_feature_subscription(Subscriptions, [{license, NewLicenseState}]).

update_feature_subscription(Subscriptions,
			    Changes) when is_list(Subscriptions) ->
    [update_feature_subscription(Subscription, Changes) ||
	Subscription <- Subscriptions],
    ok;
update_feature_subscription(Subscription, Changes) ->
    {KeyId, Rdn} = Subscription#lihi_feature_subscription.id,
    FeatureState = Subscription#lihi_feature_subscription.feature_state,
    LicenseState = Subscription#lihi_feature_subscription.license_state,
    ServiceState = Subscription#lihi_feature_subscription.service_state,

    NewFeatureState = proplists:get_value(feature, Changes, FeatureState),
    NewLicenseState = proplists:get_value(license, Changes, LicenseState),
    NewServiceState = get_service_state(NewFeatureState, NewLicenseState),

    case ServiceState of
	NewServiceState ->
	    ok;
	_ ->
	    error_logger:info_report(["Feature changed",
				      {keyId, KeyId},
				      {rdn, Rdn},
				      {serviceState, NewServiceState}]),
	    PidList = Subscription#lihi_feature_subscription.pid_list,
	    lih_lihi_worker:feature_changed(PidList, {KeyId, NewServiceState})
    end,

    lih_db:update_option_feature({KeyId, Rdn, NewFeatureState,
				  NewLicenseState, NewServiceState}),
    NewSubscription =
	Subscription#lihi_feature_subscription{license_state = NewLicenseState,
					       feature_state = NewFeatureState,
					       service_state = NewServiceState},
    ets:insert(lihi_feature_subscription, NewSubscription).

remove_feature_subscriptions(Pid, S) ->
    lists:foreach(
      fun(Subscription) ->
	      PidList = Subscription#lihi_feature_subscription.pid_list,
	      case PidList -- [Pid] of
		  [] ->
		      %% The last subscriber...
		      Id = Subscription#lihi_feature_subscription.id,
		      ets:delete(lihi_feature_subscription, Id),
		      ok = remove_feature_license(Id, S);
		  PidList ->
		      %% No change
		      ok;
		  NewPidList ->
		      %% Still subscribers left..
		      NewSubscription =
			  Subscription#lihi_feature_subscription{pid_list = NewPidList},
		      ets:insert(lihi_feature_subscription, NewSubscription)
	      end
      end, get_feature_subscriptions()),
    ok.

get_service_state(activated, enabled) -> ?CELLO_LIHI_SERVICE_OPERABLE;
get_service_state(_ , _)              -> ?CELLO_LIHI_SERVICE_INOPERABLE.

get_feature_licenses() ->
    ets:tab2list(lihi_feature_license).

reread_feature_licenses(S) ->
    lists:foreach(
      fun(License) ->
	      KeyId = License#lihi_feature_license.id,
	      OldLicenseState = License#lihi_feature_license.state,

	      case lih_lici:feature_subscribe(S#state.lici_pid, KeyId) of
		  {lici_feature_subscr_cfm, KeyId, OldLicenseState,
		   _ChangeReason} ->
		      %% No change
		      ok;
		  {lici_feature_subscr_cfm, KeyId, NewLicenseState,
		   _ChangeReason} ->
		      update_feature_subscriptions({KeyId, NewLicenseState})
	      end
      end, get_feature_licenses()),

    ok.

add_feature_license({KeyId, Rdn}, S) ->
    case ets:lookup(lihi_feature_license, KeyId) of
	[] ->
	    {lici_feature_subscr_cfm, KeyId, LicenseState, _ChangeReason} =
		lih_lici:feature_subscribe(S#state.lici_pid, KeyId),
	    License = #lihi_feature_license{id       = KeyId,
					    state    = LicenseState,
					    rdn_list = [Rdn]},
	    ets:insert(lihi_feature_license, License),

	    LicenseState;
	[License] ->
	    RdnList = License#lihi_feature_license.rdn_list,
	    NewLicense = License#lihi_feature_license{rdn_list = [Rdn|RdnList]},
	    ets:insert(lihi_feature_license, NewLicense),

	    NewLicense#lihi_feature_license.state
    end.

remove_feature_license({KeyId, Rdn}, S) ->
    [License] = ets:lookup(lihi_feature_license, KeyId),

    RdnList = License#lihi_feature_license.rdn_list,
    case RdnList -- [Rdn] of
	[] ->
	    {lici_feature_unsubscr_cfm, KeyId} =
		lih_lici:feature_unsubscribe(S#state.lici_pid, KeyId),
	    ets:delete(lihi_feature_license, KeyId);
	RdnList ->
	    ok;
	NewRdnList ->
	    NewLicense = License#lihi_feature_license{rdn_list = NewRdnList},
	    ets:insert(lihi_feature_license, NewLicense)
    end,

    ok.

update_feature_license(KeyId, NewLicenseState) ->
    [License] = ets:lookup(lihi_feature_license, KeyId),
    NewLicense = License#lihi_feature_license{state = NewLicenseState},
    ets:insert(lihi_feature_license, NewLicense),
    NewLicense#lihi_feature_license.rdn_list.

is_local_restart() ->
    {Started, _Pid} = start_lihi_checker(true),
    Started.

check_lihi_checker() ->
    start_lihi_checker(false),
    ok.

start_lihi_checker(Monitor) ->
    case whereis(lihi_checker) of
	undefined ->
	    {Pid, _Ref} =
		spawn_monitor(
		  fun() -> register(lihi_checker, self()), lihi_checker() end),
	    {false, Pid};
	Pid when Monitor ->
	    monitor(process, Pid),
	    {true, Pid};
	Pid ->
	    {true, Pid}
    end.

lihi_checker() ->
    receive _ -> lihi_checker() end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

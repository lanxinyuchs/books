%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsAppRegistry.erl %
%%% @private
%%% Description:     
%%%
%%%   pmsAppRegistry provides a mnesias table containing pids
%%%   to the session process, and the corresponding pmsAppJob process.
%%%   The key is the PM Groups the application creating the session
%%%   provides.
%%%   
%%%   This mnesia table, #pmsAppRegistry, is used by pmsJob to find
%%%   all sessions that leads to applications with the PM Groups
%%%   defined in the pmsJob.
%%% ----------------------------------------------------------
-module(pmsAppRegistry).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R11A/1').
-date('2017-10-16').
-author('eolaand').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%% -----      ---------- --------    ------------------------
%%% R1A/1      2013-01-28 uabesvi     Created
%%% R5A/1      2016-02-10 eolaand     Merge pmsAppJob2 into pmsAppJob
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).

%% gen_server functions
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2]).

%% app registry functions for PMI
-export([initialize/3]).
-export([finalize/1]).

%% app registry functions for PMI2
-export([initialize_pmi2/4]).
-export([counter_map_pmi2/5]).
-export([finalize_pmi2/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("pms.hrl").
-include("RcsPm.hrl").

-define(INITIALIZE_TIMEOUT, 60000).
-define(CALL_TIMEOUT, 10000).
-define(SERVER, ?MODULE).
-define(REG_SERVER, {global, ?SERVER}).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% start() -> ok.
%% 
%%========================================================================
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%========================================================================
%% initialize(PmGroups, PmiCb, CmPid) -> ok
%% 
%%========================================================================
initialize(PmGroups, PmiCb, CmPid) -> 
    call({initialize, {PmGroups, PmiCb, CmPid}}, ?INITIALIZE_TIMEOUT).


%%========================================================================
%% initialize_pmi2(Callbacks, CounterMap, CbMod, CmPid) -> ok
%% 
%%========================================================================
initialize_pmi2(Callbacks, CounterMap, CbMod, CmPid) -> 
    SubscCb = proplists:get_value(?SUBSCRIBE_CB, Callbacks, false),
    RepRopCb = proplists:get_value(?REPORT_ROP_CB, Callbacks, false),
    if
	not SubscCb xor RepRopCb ->
	    call({initialize_pmi2, {Callbacks, CounterMap, CbMod, CmPid}},
		 ?INITIALIZE_TIMEOUT);
	true ->
	    {error, ?ERR_INCONSISTENT_CALLBACKS}
    end.

%%========================================================================
%% counter_map_pmi2(AppJobPid, PmiCb, NewGroups, RmGroups, CmPid) -> ok
%% 
%%========================================================================
counter_map_pmi2(AppJobPid, PmiCb, NewGroups, RmGroups, CmPid) -> 
    call({counter_map_pmi2, 
	  {NewGroups, RmGroups, AppJobPid, CmPid, PmiCb}},
	 ?INITIALIZE_TIMEOUT).


%%========================================================================
%% finalize(CmPid) -> ok
%% 
%%========================================================================
finalize(CmPid) -> 
    call({finalize, CmPid}).


%%========================================================================
%% finalize_pmi2(CmPid) -> ok
%% 
%%========================================================================
finalize_pmi2(CmPid) -> 
    call({finalize_pmi2, CmPid}).


%%========================================================================
%% call(Msg) -> ok | {error, Reason}
%% 
%%========================================================================
call(Msg) ->
    call(Msg, ?CALL_TIMEOUT).


call(Msg, Timeout) ->
    call(?REG_SERVER, Msg, Timeout).


call(Server, Msg, Timeout) ->
    try
	gen_server:call(Server, Msg, Timeout)
    catch _E:R ->
	    {error, format_reason(R)}
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% init(_) -> {ok, State}
%% 
%% gen_server started.
%%========================================================================
init(_) ->
    init_active(sysEnv:role()).


init_active(active) ->
    ?LOG_RAM(?SEV_1, "Started.~n"),
    yes = global:register_name(?SERVER, self()),
    {ok, []};

init_active(_Regular) ->
    ignore.


%%========================================================================
%% handle_call(Command, From, State) -> {reply, Res, State}
%%========================================================================
handle_call({initialize, Data}, _From, State) ->
    Res = handle_initialize(Data),
    {reply, Res, start_monitoring(Res, Data, State)};
handle_call({initialize_pmi2, Data}, _From, State) ->
    Res = handle_initialize_pmi2(Data),
    {reply, Res, start_monitoring(Res, Data, State)};
handle_call({counter_map_pmi2, Data}, _From, State) ->
    Res = handle_counter_map_pmi2(cm_message, Data),
    {reply, Res, State};
handle_call({finalize, Data}, _From, State) ->
    Res = handle_finalize(Data),
    {reply, Res, stop_monitoring(Res, Data, State)};
handle_call({finalize_pmi2, Data}, _From, State) ->
    Res = handle_finalize_pmi2(Data),
    {reply, Res, stop_monitoring(Res, Data, State)};
handle_call(get_loop_data, _From, State) ->
    {reply, State, State};
handle_call(get_proc_info, _From, State) ->
    {reply,
     {app_reg, 
      [{loop_size, erts_debug:flat_size(State)},
        {proc_info, pmsLib:get_proc_info(self())}]},
     State};
handle_call(Msg, _From, State) ->
    ?LOG_RAM(?SEV_WARNING,
	     {"Received unknown handle_call msg: ~p~n", [Msg]}),
    {reply, ok, State}.


%%========================================================================
%% handle_cast(Message, State) -> {noreply, State}
%%========================================================================
handle_cast({test_terminate, Reason}, State) ->
    ?LOG_RAM(?SEV_5, {"cast ~p~n", [{test_terminate, Reason}]}),
    {stop, Reason, State};
handle_cast(Msg, State) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Received unknown handle_cast msg: ~p~n", [Msg]}),
    {noreply, State}.


%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
handle_info({'DOWN', _MRef, process, Pid, Reason}, State) ->
    PidData = proplists:get_value(Pid, State),
    {noreply, handle_monitor(PidData, Pid, State, Reason)};
handle_info(Info, State) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Received unknown handle_info msg: ~p~n", [Info]}),
    {noreply, State}.


%%========================================================================
%% code_change(OldVsn, State, Extra) -> {ok, State}
%%========================================================================
code_change(OldVsn, State, Extra) ->
    ?LOG_RAM(?SEV_1, 
	     {"Code change. ~n"
	      "  OldVersion = ~p~n"
	      "  Extra      = ~p~n", [OldVsn, Extra]}),
    {ok, State}.


%%========================================================================
%% terminate(Reason, State) -> Reason
%%========================================================================
terminate(Reason ,_State) ->
    ?LOG_RAM(?SEV_1, {"Terminated.  Reason = ~p~n", [Reason]}),
    Reason.
    

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% handle_initialize({RegData, PmiCb, SessionPid}) -> {ok, Pid} | {error, Error}
%%
%% Start a pmsAppJob process and store info in #pmsAppRegistry.
%% NOTE: the pmsAppJob process is not monitored, perhaps it should?
%%========================================================================
handle_initialize({RegData, PmiCb, SessionPid}) -> 
    ?LOG_RAM(?SEV_1,
	     {" <=== INITIALIZE PMI~n"
	      "  Session = ~p~n"
	      "  RegData = ~p~n"
	      "  CB      = ~p~n", 
	      [SessionPid, RegData, PmiCb]}),
    {ok, OkGroups} = hi_find_grp(RegData),
    StartRes = pmsAppJob:pmi_start(OkGroups, PmiCb, SessionPid),
    {ok, TopLDNGroups} = hi_find_ldn_grp(RegData),
    hi_rc(StartRes, OkGroups, TopLDNGroups, PmiCb, SessionPid).

%%====================================================
%% store the pids in mnesia if everything is ok
%%====================================================
hi_rc({ok, JobPid} = Res, PmGroups, TopLDNGroups, PmiCb, SesPid) ->
    {ok, _} = pmsDb:app_reg_set(hi_tab(PmGroups, JobPid, PmiCb, SesPid)),
    {ok, _} = pmsDb:pms_app_mo_ldn_set(hi_ldn_tab(TopLDNGroups, JobPid)),
    Res;
hi_rc(Error, _, _, _, _) ->
    Error.


hi_tab(NewPmGroups, JobPid, PmiCb, SessionPid) ->
    [#pmsAppRegistry{pm_group    = PmGroup,
		     job_pid     = JobPid,
		     pmi_cb      = PmiCb,
		     session_pid = SessionPid} || PmGroup <- NewPmGroups].

hi_ldn_tab(LDNGroups, JobPid) ->
    [#pmsScAppMoLdns{ldn         = hi_ldn_format(string:tokens(LDN, ",")),
		     app_job_pid = JobPid,
		     pm_groups   = Groups} || {LDN, Groups} <- LDNGroups].

hi_find_grp(Groups) ->
    hi_find_grp(Groups, []).

hi_find_grp([{_LDN, Groups} | T], OkGroups) ->
    hi_find_grp(Groups ++ T, OkGroups);
hi_find_grp([Grp | T], OkGroups) ->
    case pmsDb:pm_group_get({"1", "1", "1", Grp}) of
	{ok, [#pmGroup{validity = true}]} -> 
	    hi_find_grp(T, [to_binary(Grp) | OkGroups]);
	_ -> 
	    hi_find_grp(T, OkGroups)
    end;
hi_find_grp([], OkGroups) ->
    {ok, OkGroups}.

hi_find_ldn_grp(RegData) ->
    hi_find_ldn_grp(RegData, []).

hi_find_ldn_grp([{LDN, _Groups} = LG | T], Acc) 
  when LDN =/= undefined, LDN =/= [] ->
    hi_find_ldn_grp(T, [LG | Acc]);
hi_find_ldn_grp([_Grp | T], Acc) ->
    hi_find_ldn_grp(T, Acc);
hi_find_ldn_grp([], Acc) ->
    {ok, Acc}.


hi_ldn_format([_ME | T]) ->
    string:join(["ManagedElement=1" | T], ",").
    


%%========================================================================
%% handle_initialize_pmi2({Callbacks, CounterMap, CbMod, SessionPid}) -> 
%%      {ok, Pid} | {error, Error}
%%
%% Start a pmsAppJob process and store info in #pmsAppRegistry.
%%========================================================================
handle_initialize_pmi2({Callbacks, CounterMap, CbMod, SessionPid}) -> 
    ?LOG_RAM(?SEV_1, 
	     {" <=== INITIALIZE PMI2~n"
	      "  Session      = ~p~n"
	      "  CounterMapId = ~p~n"
	      "  Callbacks    = ~p~n"
	      "  CB           = ~p~n", 
	      [SessionPid, CounterMap, Callbacks, CbMod]}),
    case pmsDb:pms_counter_aliases_get(CounterMap) of
	undefined when is_list(CounterMap) ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {"COUNTER MAP file not found~n"
		      "  CounterMap = ~p~n",
		      [CounterMap]}),
	    sysInitI:error_report([{module, ?MODULE},
				   {function, pmi2Initialize},
				   {error, {no_counter_map_found, 
					    CounterMap}}]),
	    {error, ?ERR_UNKNOWN_MAP_ID};
	CmAliases ->
	    {Groups, Map} = format_counter_map(CmAliases),
	    AppJobPid = pmsAppJob:pmi2_start(Callbacks, CbMod, Groups, Map, 
					      SessionPid),
	    handle_counter_map_pmi2(CounterMap,
				    {Groups,
				     [],
				     AppJobPid,
				     SessionPid, 
				     {CbMod, Callbacks}}),
	    AppJobPid
	    %% hi2_rc(AppJobPid, {CbMod, Callbacks}, SessionPid)
    end.


%% hi2_rc({ok, AppJobPid} = Res, PmiCb, SessionPid) ->
%%    {ok, _} = pmsDb:app_reg_set(hi_tab([], AppJobPid, PmiCb, SessionPid)),
%%     Res;
%% hi2_rc(Error, _, _) ->
%%     Error.


format_counter_map(undefined) ->
    {[], undefined};
format_counter_map(Vals) ->
    %% GrpAlias = [G || {_, Grp} = G <- Vals, is_list(Grp)],
    GrpAlias = [G || {_, Grp} = G <- Vals, is_binary(Grp)],
    Groups   = [Grp || {_, Grp} <- GrpAlias],
    Maps     = format_map(GrpAlias, Vals, []),
    {Groups, Maps}.

format_map([], _, Acc) ->
    lists:append(Acc);
format_map([{GrpAlias, Gid} | T], Vals, Acc) ->
    Mts = [{Gid, GrpAlias, [{MtId, MtAlias}]} || {{GA, MtAlias}, {G, MtId}} 
						     <- Vals, 
						 GA == GrpAlias, G == Gid],
    format_map(T, Vals, [Mts | Acc]).



%%========================================================================
%% handle_counter_map_pmi2({NewPmGroups, RmPmGroups, AppJobPid, SessionPid, 
%%                          PmiCb}) ->
%%     ok | {error, Error}
%%
%% 
%%========================================================================
%% Init when no counter map file was received
handle_counter_map_pmi2(undefined, _) ->
    ok;
%% Init with counter map file, or counter map message
handle_counter_map_pmi2(_,
			{NewPmGroups, 
			 RmPmGroups, 
			 AppJobPidIn, 
			 SessionPid,
			 PmiCb}) -> 
    AppJobPid = case AppJobPidIn of
		    {ok, Pid} -> Pid;
		    Pid when is_pid(AppJobPidIn) -> Pid
		end,
    {ok, _} = pmsDb:app_reg_set(hi_tab(NewPmGroups, 
				       AppJobPid, 
				       PmiCb, 
				       SessionPid)),
    RmRecs = hi_tab(RmPmGroups, AppJobPid, PmiCb, SessionPid),
    [{ok, _} = pmsDb:app_reg_delete(RmR) || RmR <- RmRecs],
    ?LOG_RAM(?SEV_1, 
	     {" <=== COUNTER_MAP PMI2~n"
	      "  NewGroups = ~p~n"
	      "  RemGroups = ~p~n"
	      "  Session   = ~p~n"
	      "  AppJob    = ~p~n", 
	      [NewPmGroups, RmPmGroups, SessionPid, AppJobPid]}),
    ok.


%%========================================================================
%% handle_finalize(AppJobPid) -> ok
%%
%% Terminate the pmsAppJob process and remove data from #pmsAppRegistry
%%========================================================================
handle_finalize(AppJobPid) ->
    ?LOG_RAM(?SEV_1, {" <=== FINALIZE PMI~n  AppJob ~p ~n", [AppJobPid]}),
    Match = #pmsAppRegistry{job_pid = AppJobPid, _ = '_'},
    {ok, _} = pmsDb:app_reg_delete(Match),
    MatchSC = #pmsScAppMoLdns{app_job_pid = AppJobPid, _ = '_'},
    {ok, _} = pmsDb:pms_app_mo_ldn_delete(MatchSC),
    pmsAppJob:stop(AppJobPid).
    
%%========================================================================
%% handle_finalize_pmi2(AppJobPid) -> ok
%%
%% Terminate the pmsAppJob process and remove data from #pmsAppRegistry
%%========================================================================
handle_finalize_pmi2(AppJobPid) ->
    ?LOG_RAM(?SEV_1, {" <=== FINALIZE PMI2~n  AppJob ~p ~n", [AppJobPid]}),
    Match = #pmsAppRegistry{job_pid = AppJobPid, _ = '_'},
    {ok, _} = pmsDb:app_reg_delete(Match),
    MatchSC = #pmsScAppMoLdns{app_job_pid = AppJobPid, _ = '_'},
    {ok, _} = pmsDb:pms_app_mo_ldn_delete(MatchSC),
    pmsAppJob:stop(AppJobPid).
    
%%========================================================================
%% start_monitoring(PmGroups, CmPid) -> ok
%%
%% Initialize. Start monitoring of the session and AppJob processes.
%%========================================================================
start_monitoring(AppJobPid, SessionPid, State) 
  when is_pid(AppJobPid), is_pid(SessionPid)  ->
    AppJobExists = proplists:is_defined(AppJobPid, State),
    SessionExists = proplists:is_defined(SessionPid, State),
    NewState = start_mon(AppJobExists, AppJobPid, app_job, SessionPid, State),
    start_mon(SessionExists, SessionPid, session, AppJobPid, NewState);

start_monitoring({ok, AppJobPid}, {_, _, SessionPid}, State) ->
    start_monitoring(AppJobPid, SessionPid, State);

start_monitoring({ok, AppJobPid}, {_, _, _, SessionPid}, State) ->
    start_monitoring(AppJobPid, SessionPid, State);

start_monitoring(_, _, State) ->
    State.

start_mon(true, _, _, _, State) ->
    State;
start_mon(false, Pid, Type, PeerPid, State) ->
    MRef = erlang:monitor(process, Pid),
    [{Pid, {Type, MRef, PeerPid}} | State].


%%========================================================================
%% stop_monitoring(PmGroups, CmPid) -> ok
%%
%% Finalize. Session process is taken down controlled.
%%========================================================================
stop_monitoring(ok, AppJobPid, State) ->
    case proplists:get_value(AppJobPid, State) of
	{_Type, _MRef, SessionPid} = Val ->
	    NewState = stop_mon(Val, AppJobPid, State),
	    stop_monitoring(SessionPid, NewState);
	undefined ->
	    State
    end.
%% stop_monitoring(Error, _, _) ->
%%     Error.

stop_monitoring(Pid, State) ->
    stop_mon(proplists:get_value(Pid, State), Pid, State).


stop_mon(undefined, _, State) ->
    State;
stop_mon({_Type, MRef, _Peer}, Pid, State) ->
    erlang:demonitor(MRef, [flush]),
    proplists:delete(Pid, State).


%%========================================================================
%% handle_monitor(PmGroups, CmPid) -> ok
%%
%% Session process died uncontrolled.
%% Do clean up, i.e kill the pmsAppJob process etc.
%%========================================================================
handle_monitor(undefined, _Pid, State, _Reason) ->
    State;

handle_monitor({session, _MRef, AppJobPid}, Pid, State, Reason) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Session process ~p died unexpectedly ~p~n", [Pid, Reason]}),
    ok = handle_finalize(AppJobPid),
    NewState = proplists:delete(Pid, State),
    stop_monitoring(AppJobPid, NewState);

handle_monitor({app_job, _MRef, SessionPid}, Pid, State, Reason) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"AppJob process ~p died unexpectedly ~p~n"
	      "Kill session ~p~n", [Pid, Reason, SessionPid]}),
    ok = handle_finalize(Pid),
    NewState = stop_monitoring(SessionPid, proplists:delete(Pid, State)),
    exit(SessionPid, kill),
    NewState.


format_reason({timeout, _}) ->
    timeout;
format_reason({noproc, _}) ->
    app_reg_not_running;
format_reason(_) ->
    failed_to_call_app_reg.


to_binary(Term) ->
    pmsLib:to_binary(Term).
    
%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

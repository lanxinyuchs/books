%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesAppRegistry.erl %
%%% @private
%%% Description:     
%%%
%%%   pmsAppRegistry provides a mnesias table containing pids
%%%   to the session process, and the corresponding pmsAppJob process.
%%%   The key is the PM Groups the application creating the session
%%%   provides.
%%%   
%%%   This mnesia table, #peiAppRegistry, is used by peiJob to find
%%%   all sessions that leads to applications with the PM Groups
%%%   defined in the peiJob.
%%% ----------------------------------------------------------
-module(pesAppRegistry).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-04-05').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R4A/2      2015-09-08 uabesvi     error_logger -> sysInitI
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

%% app registry functions for PEI
-export([initialize_pei/4]).
-export([finalize_pei/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("pes.hrl").
-include("RcsPMEventM.hrl").

%% -define(LOG(_LogLevel, _LogMsg), 
%% 	udda_log:log(self(), {?MODULE, 1, self()}, _LogLevel, _LogMsg)).

-define(INITIALIZE_TIMEOUT, 60000).
-define(SERVER, ?MODULE).

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
%% initialize_pei(EventMapId, Callbacks, PmiCb, CmPid) -> ok
%% 
%%========================================================================
initialize_pei(EventMapId, Callbacks, PmiCb, CmPid) -> 
    gen_server:call(?SERVER, 
		    {initialize_pei, {EventMapId, Callbacks, PmiCb, CmPid}},
		    ?INITIALIZE_TIMEOUT).


%%========================================================================
%% finalize_pei(CmPid) -> ok
%% 
%%========================================================================
finalize_pei(CmPid) -> 
    gen_server:call(?SERVER, {finalize_pei, CmPid}).

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
    ?LOG_RAM(?SEV_1, "Started app registry process~n"),
    %% global:register_name(?SERVER, self()),
    {ok, []};

init_active(_Regular) ->
    ignore.


%%========================================================================
%% handle_call(Command, From, State) -> {reply, Res, State}
%%========================================================================
handle_call({initialize_pei, Data}, _From, State) ->
    Res = handle_initialize_pei(Data),
    {reply, Res, State};
%%    {reply, Res, start_monitoring(Res, Data, State)};
handle_call({finalize_pei, Data}, _From, State) ->
    Res = handle_finalize_pei(Data),
    {reply, Res, stop_monitoring(Res, Data, State)};
handle_call(Msg, _From, State) ->
    ?LOG_RAM(?SEV_1, {"Received unknown handle_call msg: ~p~n", [Msg]}),
    {reply, ok, State}.


%%========================================================================
%% handle_cast(Message, State) -> {noreply, State}
%%========================================================================
handle_cast({test_terminate, Reason}, State) ->
    ?LOG_RAM(?SEV_1, {"cast ~p~n", [{test_terminate, Reason}]}),
    {stop, Reason, State};
handle_cast(Msg, State) ->
    ?LOG_RAM(?SEV_1, {"Received unknown handle_cast msg: ~p~n", [Msg]}),
    {noreply, State}.


%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
handle_info({'DOWN', _MRef, process, Pid, Reason}, State) ->
    PidData = proplists:get_value(Pid, State),
    {noreply, handle_monitor(PidData, Pid, State, Reason)};
handle_info(Info, State) ->
    ?LOG_RAM(?SEV_1, {"Received unknown handle_info msg: ~p~n", [Info]}),
    {noreply, State}.


%%========================================================================
%% code_change(OldVsn, State, Extra) -> {ok, State}
%%========================================================================
code_change(OldVsn, State, Extra) ->
    ?LOG_RAM(?SEV_1, {"Code_change: ~p~n", [{OldVsn, Extra}]}),
    {ok, State}.


%%========================================================================
%% terminate(Reason, State) -> Reason
%%========================================================================
terminate(Reason ,_State) ->
    ?LOG_RAM(?SEV_1, {"terminated ~p~n", [Reason]}),
    Reason.
    

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%========================================================================
%% handle_initialize_pei({Callbacks, PmiCb, SessionPid}) -> 
%%      {ok, Pid} | {error, Error}
%%
%% Start a peiAppJob2 process and store info in #pesAppRegistry.
%%========================================================================
handle_initialize_pei({EventMapId, Callbacks, PmiCb, SessionPid}) -> 
    ?LOG_RAM(?SEV_1, 
	     {" <=== INITIALIZE PEI~n"
	      "  EventMapId = ~p~n"
	      "  Session    = ~p~n"
	      "  Callbacks  = ~p~n"
	      "  CB         = ~p~n", 
	      [EventMapId, SessionPid, Callbacks, PmiCb]}),
    hi_ca(hi_get_aliases(EventMapId), Callbacks, PmiCb, SessionPid).

hi_ca(Aliases, Callbacks, PmiCb, SessionPid) when is_list(Aliases) ->
    hi2_rc(pesAppJob:start(Aliases, Callbacks, PmiCb, SessionPid), 
	   [Type || {Type, _} <- Aliases], 
	   PmiCb, 
	   Callbacks,
	   SessionPid);
hi_ca(Error, _, _, _) ->
    Error.

hi_get_aliases(undefined) ->
    [];
hi_get_aliases(EventMapId) ->
    hi_ga(pesDb:pes_type_aliases_get(EventMapId), EventMapId).

hi_ga(undefined, EventMapId) ->
    sysInitI:error_report([{module, ?MODULE},
			       {function, peiInitialize},
			       {error, {no_event_map_found, EventMapId}}]),
    {error, ?ERR_UNKNOWN_MAP_ID};
hi_ga(Aliases, _EventMapId) ->
    Aliases.


hi2_rc({ok, AppJobPid} = Res, Types, PmiCb, Callbacks, SessionPid) ->
    ok = pesDb:app_reg_set(hi_tab(Types, AppJobPid, PmiCb, Callbacks, 
				  SessionPid)),
    Res;
hi2_rc(Error, _, _, _, _) ->
    Error.


hi_tab(NewTypes, JobPid, PmiCb, Callbacks, SessionPid) ->
    PidEntry = #pesAppRegPid{job_pid     = JobPid,
			     event_types = NewTypes,
			     callbacks   = {PmiCb, Callbacks},  
			     session_pid = SessionPid},
    CbFlag = proplists:get_value(peiEventJobCallback, Callbacks, false),
    hi_tab_cb(CbFlag, PidEntry, NewTypes, JobPid).


hi_tab_cb(true, PidEntry, NewTypes, JobPid) ->
    {[#pesAppRegType{event_type  = Type,
		     job_pid     = JobPid} || Type <- NewTypes],
     PidEntry};

hi_tab_cb(_False, PidEntry, _NewTypes, _JobPid) ->
    {[], PidEntry}.



%%========================================================================
%% handle_finalize(AppJobPid) -> ok
%%
%% Terminate the peiAppJob process and remove data from #pesAppRegistry
%%========================================================================
handle_finalize(_AppJobPid) ->
    ok.
%%     ?LOG_IF(" <=== FINALIZE~n  AppJob ~p ~n", [AppJobPid]),
%%     Match = #pesAppRegistry{job_pid = AppJobPid, _ = '_'},
%%     {ok, _} = peiDb:app_reg_delete(Match),
%%     MatchSC = #peiScAppMoLdns{app_job_pid = AppJobPid, _ = '_'},
%%     {ok, _} = peiDb:pei_app_mo_ldn_delete(MatchSC),
%%     peiAppJob:stop(AppJobPid).
    
%%========================================================================
%% handle_finalize_pei(AppJobPid) -> ok
%%
%% Terminate the peiAppJob process and remove data from #pesAppRegistry
%%========================================================================
handle_finalize_pei(AppJobPid) ->
    ?LOG_RAM(?SEV_1, {" <=== FINALIZE PEI~n  AppJob = ~p ~n", [AppJobPid]}),
    MatchType = #pesAppRegType{job_pid = AppJobPid, _ = '_'},
    MatchPid  = #pesAppRegPid{job_pid = AppJobPid, _ = '_'},
    ok = pesDb:app_reg_type_delete(MatchType),
    ok = pesDb:app_reg_pid_delete(MatchPid),
    pesAppJob:stop(AppJobPid).
    
%%========================================================================
%% start_monitoring(PmGroups, CmPid) -> ok
%%
%% Initialize. Start monitoring of the session and AppJob processes.
%%========================================================================
%% start_monitoring({ok, AppJobPid}, {_, _, SessionPid}, State) ->
%%     AppJobExists = proplists:is_defined(AppJobPid, State),
%%     SessionExists = proplists:is_defined(SessionPid, State),
%%     NewState = start_mon(AppJobExists, AppJobPid, app_job, SessionPid, State),
%%     start_mon(SessionExists, SessionPid, session, AppJobPid, NewState).

%% start_mon(true, _, _, _, State) ->
%%     State;
%% start_mon(false, Pid, Type, PeerPid, State) ->
%%     MRef = erlang:monitor(process, Pid),
%%     [{Pid, {Type, MRef, PeerPid}} | State].


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
%% Do clean up, i.e kill the peiAppJob process etc.
%%========================================================================
handle_monitor(undefined, _Pid, State, _Reason) ->
    State;

handle_monitor({session, _MRef, AppJobPid}, Pid, State, Reason) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Session process ~p died unexpectedly ~p~n", [Pid, Reason]}),
    sysInitI:info_msg("Session process ~p died unexpectedly: ~p~n", 
		      [Pid, Reason]),
    ok = handle_finalize(AppJobPid),
    NewState = proplists:delete(Pid, State),
    stop_monitoring(AppJobPid, NewState);

handle_monitor({app_job, _MRef, SessionPid}, Pid, State, Reason) ->
    ?LOG_RAM(?SEV_WARNING,
	     {"AppJob process ~p died unexpectedly ~p~n"
	      "Kill session ~p~n", 
	      [Pid, Reason, SessionPid]}),
    sysInitI:info_msg("AppJob process ~p died unexpectedly: ~p~n"
		      "Kill the client/session ~p~n",
		      [Pid, Reason, SessionPid]),
    ok = handle_finalize(Pid),
    NewState = stop_monitoring(SessionPid, proplists:delete(Pid, State)),
    exit(SessionPid, kill),
    NewState.


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_c_pmi.erl %
%%% Author:	erarafo
%%% 
%%% Description: An implementation of the PM interface that uses
%%% the IFT application as a bridge towards the CS PMS.
%%%
%%% Modules used: rct_proxy, pms_c_util
%%%
%%% ----------------------------------------------------------
-module(pms_c_pmi).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/2').
-date('2015-04-28').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R2A/2    2013-03-03 erarafo     Created
%%% R2A/12   2013-04-26 erarafo     Added 'do_not_reply' feature in pmiData()
%%% R2A/13   2013-04-26 erarafo     Fixed dialyzer fault
%%% R2A/14   2013-04-26 uabesvi     added {wait, WaitTime, Bundles} to data-msg
%%% R2A/18   2013-05-02 erarafo     Cleared compiler warnings
%%% R2A/4    2014-04-22 uabesvi     show counters
%%% ----------------------------------------------------------


%% PM interface functions
-export([
	 pmiInitialize/1,
	 pmiInitialize/2,
	 pmiInitialize/3,
	 pmiInitialize_2/4,
	 pmiData/5,
	 pmiDataShowCounters/5,
	 pmiFinalize/0,
	 pmiFinalize/1,
	 pmiSubscribeCallback/4,
	 pmiReportCallback/4,
	 pmiReportShowCountersCallback/4
	]).


%% Interface functions for tests only.
-export([
	 emulAppDeath/2,
	 pmiSetCecPort/1
	]).


%% CT hook callback functions
-export([
	 init/2,
	 pre_init_per_suite/3,
	 post_init_per_suite/4,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 post_end_per_suite/4,
	 on_tc_fail/3,
	 terminate/1
	]).

-export([initLoop/3]).

-export([
	 get_children/0,
	 get_children/1
	]).


-type result()::integer().
-type request_id()::integer().
-type mo_ldn()::string().
-type max_response_time()::integer().
-type pm_group()::string().
-type reg_data() :: [PMGroup::pm_group()] | 
		    [{TopLDN::string(), [PMGroup::pm_group()]}].
-type granularity_period() :: integer().
-type meas_timestamp() :: integer().
-type deadline() :: integer().
-type measurement_type() :: string().
-type counter_spec() :: {pm_group, [measurement_type()]}.
-type value_bundle() :: {pm_group, [{measurement_type(), [integer()]}]}.
-type sc_value_bundle() :: {measurement_type(), [integer()]}.
-type callback_module() :: atom().
-type child() :: atom().



-include("pms_c.hrl").

-record(state, {
	  sessionByPid,
	  sessionByHandle,
	  sessionByChild,
	  socketFd,
	  nextChild=1,
	  cecHost     :: string(),
	  cecPort=0   :: integer(),
	  cs_node,
	  trace_child=false
	 }).

-record(pmSession, {
	  handle    :: string(),
	  child     :: child(),
	  appPid    :: pid(),
	  cbModule  :: module()
	 }).


-record(cth_state, {
	  trace_child,
	  trace_ift,
	  trace,
	  cec_host,
	  cec_port,
	  cs_erl_node
	 }).

-define(SERVER, ?MODULE).

-define(TEST_INFO(Format, Items), pms_test_lib:ct_log(Format, Items)).

-define(TEST_DEBUG(Format, Items), pms_test_lib:ct_log(Format, Items)).

-define(CHILD_PREFIX, "ift_client_").



%% @doc Set a non-zero CEC port. This is required for tests where
%% the CEC server side is not provided by the regular CS. Call
%% this function before the first application initializes a
%% PM session.

-spec pmiSetCecPort(integer()) -> ok.

pmiSetCecPort(Port) ->
    ?SERVER ! {signal, {setCecPort, self(), Port}},
    receive
	{response, setCecPort} ->
	    ok
    end.


%% @doc Equivalent to pmiInitialize(RegData, pms_c_pmi).

-spec pmiInitialize(RegData::reg_data()) -> 
    {ok, Handle::pid()} | 
	{error, Reason::string()}.
%% -spec pmiInitialize([pm_group()]) ->
%%     ok | {error, already_initialized}.

pmiInitialize(RegData) ->
    pmiInitialize(RegData, ?MODULE).


%% @doc Equivalent to pmiInitialize(RegData, CallbackModule, self()).

-spec pmiInitialize(RegData::reg_data(), CallbackModule::callback_module()) -> 
    {ok, Handle::pid()} | 
	{error, Reason::string()}.
%% -spec pmiInitialize([pm_group()], callback_module()) ->
%%     ok | {error, already_initialized}.

pmiInitialize(RegData, CallbackModule) ->
    pmiInitialize(RegData, CallbackModule, self()).


%% @doc Initializes a PM session from an application
%% with the given pid. The given callback module must
%% implement pmiSubscribeCallback/4 and pmiReportCallback/4.

-spec pmiInitialize(RegData::reg_data(), CallbackModule::callback_module(), 
		    AppPid::pid()) -> {ok, Handle::pid()} | 
				      {error, Reason::string()}.
%% -spec pmiInitialize([pm_group()], callback_module(), pid()) ->
%%     ok | {error, already_initialized}.

pmiInitialize(RegData, CallbackModule, AppPid) ->
    ?SERVER ! {signal, 
	       {initialize, self(), RegData, CallbackModule, AppPid}},
    receive
	{response, initialize, already_initialized} ->
	    {error, already_initialized};
	
	{response, initialize, Child, Handle} ->
	    ?SERVER ! {signal, 
		       {setupForCallback, self(), Child, Handle}},
	    receive
		{response, setupForCallback} ->
		    ok
	    end	
    end.

pmiInitialize_2(TopMoLdn, PmGroups, CallbackModule, AppPid) ->
    ?SERVER ! {signal, 
	       {initialize_2, self(), TopMoLdn, PmGroups, CallbackModule, AppPid}},
    receive
	{response, initialize_2, already_initialized} ->
	    {error, already_initialized};
     
	{response, initialize_2, Child, Handle} ->
	    ?SERVER ! {signal, 
		       {setupForCallback, self(), Child, Handle}},
	    receive
		{response, setupForCallback} ->
		    ok
	    end	
    end.


%% @doc Send PM data to the PM service. The subscriber was received
%% in a previous pmiSubscribeCallback/4 invocation.
%%
%% The Bundles argument may be given as the atom 'do_not_reply', 
%% meaning "do not reply to a previous 'report' message". In that
%% case the sending of a downstream pmiData request is skipped.

-spec pmiData(atom(), 
	      granularity_period(), 
	      meas_timestamp(), 
	      string(), 
	      [value_bundle()] | do_not_reply | {wait, integer(), [value_bundle()]}) -> ok.

pmiData(Subscriber,
	GranularityPeriod, 
	TimeSpec, 
	MeasObjLDN, 
	Bundles) ->
    ?SERVER ! {signal, 
	       {data, Subscriber, GranularityPeriod, TimeSpec, MeasObjLDN, Bundles}},
    receive
	{response, data} ->
	    ok
    end.





%% @doc Send PM data to the PM service. The subscriber was received
%% in a previous pmiSubscribeCallback/4 invocation.
%%
%% The Bundles argument may be given as the atom 'do_not_reply', 
%% meaning "do not reply to a previous 'report' message". In that
%% case the sending of a downstream pmiData request is skipped.

-spec pmiDataShowCounters(atom(), 
			  request_id(), 
			  result(), 
			  string(),
			  [sc_value_bundle()] | do_not_reply | {wait, integer(), [sc_value_bundle()]}) -> ok.

pmiDataShowCounters(Subscriber, RequestId, Result, ErrorStr, Bundles) ->
    ?SERVER ! {signal, 
	       {data_sc, Subscriber, RequestId, Result, ErrorStr, Bundles}},
    receive
	{response, data_sc} ->
	    ok
    end.




%% @doc Equivalent to pmiFinalize(self()).

-spec pmiFinalize() -> ok.

pmiFinalize() ->
    pmiFinalize(self()).


%% @doc Finalize a PM application session. The AppPid is trusted
%% to be the same that was used in pmiInitialize/3.

-spec pmiFinalize(pid()) -> ok.

pmiFinalize(AppPid) ->
    ?SERVER ! {signal, {finalize, AppPid}},	
    receive
	{response, finalize} ->
	    ok
    after 30000 ->
	    ct:log("Failed to finalize ~p: timeout", [AppPid]),
	    {error, timeout}
    end.


%% @doc Default implementation of pmiSubscribeCallback, sending
%% a {pmiSubscribe, {Subscriber, GranularityPeriod, CounterSpecs}}
%% tuple to the application. The Subscriber must be saved by the
%% application for use in subsequent pmiData messages.

-spec pmiSubscribeCallback(pid(), atom(), granularity_period(), [counter_spec()]) -> ok.

pmiSubscribeCallback(AppPid, Subscriber, GranularityPeriod, CounterSpecs) -> 
    AppPid ! {pmiSubscribe, {Subscriber, GranularityPeriod, CounterSpecs}},
    ok.


%% @doc Default implementation of pmiReportCallback, sending a
%% {pmiReport, {GranularityPeriod, TimeSpec, DeadLine}} tuple
%% to the application.

-spec pmiReportCallback(pid(), 
			granularity_period(), 
			meas_timestamp(), 
			deadline()) -> ok.

pmiReportCallback(AppPid, GranularityPeriod, TimeSpec, Deadline) ->
    AppPid ! {pmiReport, {GranularityPeriod, TimeSpec, Deadline}},
    ok.


%% @doc Default implementation of pmiReportCallback, sending a
%% {pmiReport, {GranularityPeriod, TimeSpec, DeadLine}} tuple
%% to the application.

-spec pmiReportShowCountersCallback(pid(), 
				    request_id(), 
				    mo_ldn(), 
				    max_response_time()) -> ok.

pmiReportShowCountersCallback(AppPid, RequestId, MoLdn, MaxResponseTime) ->
    AppPid ! {pmiReportShowCounters, {RequestId, MoLdn, MaxResponseTime}},
    ok.


%% @doc Make the C node behave as a dead application would.
%% Only the specifed application is affected. If CloseSocket
%% is true the socket is closed from the application side,
%% otherwise it is left unattended.

emulAppDeath(AppPid, CloseSocket) ->
    ?SERVER ! {signal, {emulAppDeath, AppPid, CloseSocket}},	
    receive
	{response, emulAppDeath} ->
	    ok
    end.


get_children() ->
    ?SERVER ! {signal, {get_children, self()}},	
    receive
	{response, get_children, Res} ->
	    Res
    end.

get_children(Pid) ->
    ?SERVER ! {signal, {get_children, self(), Pid}},	
    receive
	{response, get_children, Res} ->
	    Res
    end.



%% @doc Mandatory CT hook callback function.

init(Id, Options) ->
    ?TEST_INFO("~w: init(~p, ~p)", [?MODULE, Id, Options]),
    {CecHost, CecPort} = proplists:get_value(cecService, Options, {"", 0}),
    TraceCh = proplists:get_value(trace_child, Options, false),
    TraceIFT = proplists:get_value(trace_ift, Options, false),
    CTHState = #cth_state{trace_child = TraceCh,
			  trace_ift = TraceIFT,
			  trace = TraceCh orelse TraceIFT,
			  cec_host = CecHost,
			  cec_port = CecPort},
    {ok, CTHState}.


%% @doc This ct_hook callback starts the server loop. The
%% CecHost and CecPort may be set to values provided by
%% the cec_manager CT hook.

pre_init_per_suite(SuiteName, InitData, CTHState) ->
    ?TEST_INFO("~w: pre_init_per_suite(~p, ~s, ~p)",
	       [?MODULE, SuiteName, "_", CTHState]),
    CecHost = CTHState#cth_state.cec_host, 
    CecPort = CTHState#cth_state.cec_port, 
    TraceCh = CTHState#cth_state.trace_child,
    ServerPid = spawn(?MODULE, initLoop, [CecHost, CecPort, TraceCh]),
    register(?SERVER, ServerPid),
    {InitData, CTHState}.


%% @private
post_init_per_suite(_SuiteName, _Config, Return, CTHState) ->
    ?TEST_INFO("~w: post_init_per_suite(~p, ~s, ~p)",
	       [?MODULE, _SuiteName, "_", CTHState]),
    case get_CsErlNode(?NODE) of
	{ok, CsErlNode} ->
	    NewCTHState = CTHState#cth_state{cs_erl_node = CsErlNode},
	    clear_te_log(NewCTHState),
	    start_ift_trace(NewCTHState),
	    {Return, NewCTHState};
	_Error ->
	    ?TEST_INFO("~w: Failed to get CS erl node: ~p",
		       [?MODULE, _Error]),
	    {Return, CTHState}
	end.

%% @private
pre_init_per_testcase(TC, Config, CTHState) ->
    ?TEST_INFO("~w: pre_init_per_testcase(~p, ~s, ~p)",
	       [?MODULE, TC, "_", CTHState]),
    case whereis(?SERVER) of
	undefined ->
	    {{skip, "pms_c_pmi server DOWN"}, CTHState};
	_Pid ->
	    {Config, CTHState}
    end.

%% @private
post_end_per_testcase(TC, Config, {SkipOrFail, _Reason} = Ret, CTHState)
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    catch log_te_log(TC, Config, CTHState),
    {Ret, CTHState};

post_end_per_testcase(TC, Config, Return, CTHState) ->
    ?TEST_INFO("~w: post_end_per_testcase(~p, ~s, ~p)",
	       [?MODULE, TC, "_", CTHState]),
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    Pid ! {signal, finalize_all};
	_ ->
	    ok
    end,
    try
	log_te_log(TC, Config, CTHState),
	clear_te_log(CTHState)
    catch _:_ ->
	    ok
    end,
    {Return, CTHState}.


%% @doc This ct_hook callback stops the server loop.
post_end_per_suite(_SuiteName, Config, Return, CTHState) ->
    ?TEST_INFO("~w: post_end_per_suite(~p, ~s, ~p)",
	       [?MODULE, _SuiteName, "_", CTHState]),
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    Pid ! {signal, {stop}};
	_ ->
	    ok
    end,
    try
	stop_ift_trace(CTHState),
	log_te_log(end_per_suite, Config, CTHState)
    catch _:_ ->
	    ok
    end,
    {Return, CTHState}.


%% @private
on_tc_fail(_TC, _Reason, CTHState) ->
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    Pid ! {signal, finalize_all};
	_ ->
	    ok
    end,
    CTHState.
    

%% @private
terminate(_CTHState) ->
    Pid = whereis(?SERVER),
    ?TEST_INFO("~w: terminate, server pid = ~p", [?MODULE, Pid]),
    case Pid of
	undefined ->
	    ok;
	_ ->
	    Pid ! {signal, {stop}},
	    catch exit(Pid, kill)
    end.


%% @private
initLoop(CecHost, CecPort, Trace) ->
    State = #state{trace_child = Trace,
		   cecHost=CecHost,
		   cecPort=CecPort,
		   sessionByHandle = dict:new(), 
		   sessionByPid    = dict:new(),
		   sessionByChild  = dict:new()
		  },
    ?TEST_INFO("~w: starting", [?MODULE]),
    loop(State).




%% @doc The server loop. All messages are received using
%% rct_proxy:receive_proxy(). This includes both callbacks from
%% the PM service and incoming requests from PM users, which all
%% must have the format {signal, _}.
%%
%% Some requests, as well as some callbacks, cause requests to
%% the PM service to be sent. While such a request is handled
%% there may be {signal, _} messages arriving. This is in order
%% since the underlying rct_proxy:send_proxy() function does
%% not pick up messages having the {signal, _} format.

-spec loop(#state{}) -> ok.

loop(State) ->
    ?TEST_DEBUG("~w: loop/1, state: ~p", [?MODULE, State]),
    
    case recv_proxy(rct_proxy:receive_proxy(infinity), State) of
	{ok, {stop}} ->
	    ?TEST_INFO("~w: terminating", [?MODULE]),
	    ok;
	
	{ok, NewState} ->
	    loop(NewState);
	
	%% In case of timeout, just carry on.	
	{error, timeout} ->
	    ?TEST_DEBUG("~w: loop/1: ERROR ~p", [?MODULE, timeout]),
	    loop(State);
	
	Other ->
	    ct:pal("~w: loop/1: cannot handle: ~p", [?MODULE, Other])
    end.





recv_proxy(Res, State) ->
    ?TEST_DEBUG("~w: recv_proxy/2, Received: ~p", [?MODULE, Res]),
    rp(Res, State).


rp({ok, {setCecPort, Caller, Port}}, State) ->
    Caller ! {response, setCecPort},
    {ok, State#state{cecPort=Port}};

rp({ok, {initialize, Caller, _RegData, _CbModule, AppPid} = Data}, State) ->
    case dict:is_key(AppPid, State#state.sessionByPid) of
	true ->
	    ?TEST_DEBUG("~w: recv_proxy/2, init: already init", [?MODULE]),
	    Caller ! {response, initialize, already_initialized},
	    loop(State);
	false ->
	    rp_initialize(Data, State)
    end;
    
rp({ok, {initialize_2, Caller, _TopMoLdn, _RegData, _CbModule, AppPid} = Data},
   State) ->
    case dict:is_key(AppPid, State#state.sessionByPid) of
	true ->
	    ?TEST_DEBUG("~w: recv_proxy/2, init_2: already init", [?MODULE]),
	    Caller ! {response, initialize_2, already_initialized},
	    loop(State);
	false ->
	    rp_initialize_2(Data, State)
    end;
    
rp({ok, {setupForCallback, Caller, Child, Handle}}, State) ->
    {ok, _Socket} = 
	rct_proxy:send_proxy(?NODE, Child, ?PMI_SETUP_CALLBACK, {Handle}),
    Caller ! {response, setupForCallback},
    {ok, State};

rp({ok, {data, _Subscriber, _GP, _TimeSpec, _LDN, _Bundles} = Data}, State) ->
    data(Data, State),
    {ok, State};

rp({ok, {data_sc, _Subscriber, _RequestId, _Res, _, _Bundles} = Data}, State) ->
    data_sc(Data, State),
    {ok, State};

rp({ok, {finalize, AppPid}}, State) ->
    NewState = finalize({finalize, AppPid}, false, State),
    {ok, NewState};

rp({ok, finalize_all}, State) ->
    AppPids = dict:fetch_keys(State#state.sessionByPid),
    NewState = lists:foldl(fun(AppPid, AccState) ->
				   finalize({finalize, AppPid}, true, AccState)
			   end, State, AppPids),
    {ok, NewState};

rp({ok, {emulAppDeath, AppPid, CloseSocket}}, State) ->
    SessionByPid = State#state.sessionByPid,
    #pmSession{child=Child, handle=Handle} = 
	dict:fetch(AppPid, SessionByPid),
    {ok} = 
	rct_proxy:send_proxy(?NODE, 
			     Child, 
			     ?PMI_EMUL_DEATH, 
			     {Handle, CloseSocket}),
    AppPid ! {response, emulAppDeath},
    {ok, State};
	
%% Handle callbacks from the PM service	
rp({ok, {subscribe, {_Handle, _GP, _GroupSpecs} = Subscribe}}, State) ->
    subscribe(Subscribe, State),
    {ok, State};
	
rp({ok, {report, {Handle, GP, Timespec, Deadline}}}, State) ->
    SessionByHandle = State#state.sessionByHandle,
    case dict:is_key(Handle, SessionByHandle) of
	false ->
	    ?TEST_DEBUG(" ~w: no session with handle: ~s", 
		   [?MODULE, Handle]),
	    ok;
	true ->
	    Session = dict:fetch(Handle, SessionByHandle),
	    apply(
	      Session#pmSession.cbModule,
	      pmiReportCallback,
	      [Session#pmSession.appPid, GP, Timespec, Deadline])
    end,
    {ok, State};

rp({ok, {showCounters, {Handle, RequestId, MoLdn, MaxResponseTime}}}, State) ->
    SessionByHandle = State#state.sessionByHandle,
    case dict:is_key(Handle, SessionByHandle) of
	false ->
	    ?TEST_DEBUG(" ~w: no session with handle: ~s", 
		   [?MODULE, Handle]),
	    ok;
	true ->
	    Session = dict:fetch(Handle, SessionByHandle),
	    apply(
	      Session#pmSession.cbModule,
	      pmiReportShowCountersCallback,
	      [Session#pmSession.appPid, 
	       Session#pmSession.child, 
	       RequestId, 
	       MoLdn, 
	       MaxResponseTime])
    end,
    {ok, State};

rp({ok, {get_children, Caller}}, State) ->
    Fun = fun(#pmSession{child = Child}, Acc) -> [Child | Acc] end,
    Res = dict:fold(Fun, [], State#state.sessionByPid),
    Caller ! {response, get_children, Res},
    {ok, State};
rp({ok, {get_children, Caller, AppPid}}, State) ->
    SessionByPid = State#state.sessionByPid,
    #pmSession{child = Child} = dict:fetch(AppPid, SessionByPid),
    Caller ! {response, get_children, [Child]},
    {ok, State};

%% Handle stop message
rp({ok, {stop}} = Res, _State) ->
    Res;

%% In case of timeout, just carry on.	
rp({error, timeout} = Error, _State) ->
    Error;

rp(Other, _State) ->
    Other.



rp_initialize({initialize, Caller, RegData, CbModule, AppPid}, State) ->    
    ChildNumber = State#state.nextChild,
    Child = createChildName(ChildNumber),
    
    case rct_proxy:start_proxy(?NODE, Child, ?PMI) of
	{ok, client_started} -> 
	    ct:pal("~w: proc started ~p ~n", [?MODULE, Child]),
	    ?TEST_INFO("~w: rct proxy started ~n", [?MODULE]);
	ProxyError ->
	    ct:pal("~w: proc info ~p ~n", [?MODULE, process_info(self())]),
	    ?TEST_INFO("~w: rct proxy error ~p ~n", [?MODULE, ProxyError])
    end,
    CecHost = State#state.cecHost,
    CecPort = integer_to_list(State#state.cecPort), 
    InitMsg = {CecHost, CecPort, RegData},
    start_child_trace(Child, State),
    Handle = 
	case rct_proxy:send_proxy(?NODE, Child, ?PMI_INITIALIZE, InitMsg) of
	    {ok, H} -> 
		%%= rct_proxy:send_proxy(?NODE, Child, ?PMI_INITIALIZE, InitMsg),
		ct:pal("~w: pmi init sent ok ~p ~n", [?MODULE, Child]),
		?TEST_INFO("~w: rct proxy: pmiInitialize ~p~n", [?MODULE, H]),
		H;
	    InitError ->
		ct:pal("~w: proc error init send ~p ~n", [?MODULE, InitError]),
		?TEST_INFO("~w: rct proxy error ~p ~n", [?MODULE, InitError]),
		ct:fail({error, InitError})
    end,
    
    
    Session = #pmSession{appPid   = AppPid, 
			 cbModule = CbModule,
			 child    = Child, 
			 handle   = Handle},
    
    NewSBH = dict:store(Handle, Session, State#state.sessionByHandle),
    NewSBP = dict:store(AppPid, Session, State#state.sessionByPid),
    NewSBC = dict:store(Child,  Session, State#state.sessionByChild),
    NewState = State#state{sessionByHandle = NewSBH, 
			   sessionByPid    = NewSBP, 
			   sessionByChild  = NewSBC,
			   nextChild       = ChildNumber + 1
			  },
    
    Caller ! {response, initialize, Child, Handle},
    {ok, NewState}.

rp_initialize_2({initialize_2, Caller, TopMoLdn, RegData, CbModule, AppPid},
		State) ->

    ChildNumber = State#state.nextChild,
    Child = createChildName(ChildNumber),
    
    {ok, client_started} = rct_proxy:start_proxy(?NODE, Child, ?PMI),
    ?TEST_INFO("~w: rct proxy started ~n", [?MODULE]),
    CecHost = State#state.cecHost,
    CecPort = integer_to_list(State#state.cecPort), 
    InitMsg = {CecHost, CecPort, TopMoLdn, RegData},
    start_child_trace(Child, State),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, Child, ?PMI_INITIALIZE_2, InitMsg),
    ?TEST_INFO("~w: rct proxy: pmiInitialize_2 ~p~n", [?MODULE, Handle]),
    
    Session = #pmSession{appPid   = AppPid, 
			 cbModule = CbModule,
			 child    = Child, 
			 handle   = Handle},
    
    NewSBH = dict:store(Handle, Session, State#state.sessionByHandle),
    NewSBP = dict:store(AppPid, Session, State#state.sessionByPid),
    NewSBC = dict:store(Child,  Session, State#state.sessionByChild),
    NewState = State#state{sessionByHandle = NewSBH, 
			   sessionByPid    = NewSBP, 
			   sessionByChild  = NewSBC,
			   nextChild       = ChildNumber + 1
			  },
    
    Caller ! {response, initialize_2, Child, Handle},
    {ok, NewState}.


%% @doc Create a child from the given number.

-spec createChildName(integer()) -> child().

createChildName(Number) ->
    list_to_atom("child-"++integer_to_list(Number)).




finalize({finalize, AppPid}, Async, State) ->
    SessionByPid = State#state.sessionByPid,
    finalize_session(dict_fetch(AppPid, SessionByPid), AppPid, Async, State).


finalize_session({ok, #pmSession{child=Child, handle=Handle}}, AppPid, Async, 
		 State) ->
    SessionByPid    = State#state.sessionByPid,
    SessionByHandle = State#state.sessionByHandle,
    SessionByChild  = State#state.sessionByChild,
    {ok} = rct_proxy:send_proxy(?NODE, Child, ?PMI_FINALIZE, {Handle}),
    {ok, client_stopped} = rct_proxy:stop_proxy(?NODE, Child),
    NewState = 
	State#state{sessionByPid=dict:erase(AppPid, SessionByPid),
		    sessionByHandle=dict:erase(Handle, SessionByHandle),
		    sessionByChild=dict:erase(Child, SessionByChild)
		   },
	
    Async orelse (AppPid ! {response, finalize}),
    NewState;

finalize_session(Error, _, _, State) ->
    ?TEST_DEBUG("skipping sending a 'finalize' message due to ~p", [Error]),
    State.





data({data, Subscriber, _GP, _TimeSpec, _LDN, _} = Data, State) ->
    data_session(dict_fetch(Subscriber, State#state.sessionByChild),
		 Data,
		 State).



data_session({ok, Session}, {data, _Subscriber, _GP, TimeSpec, _LDN, do_not_reply}, State) ->
    ?TEST_DEBUG("skip sending a 'data' message, TimeSpec: ~w", [TimeSpec]),
    Session#pmSession.appPid ! {response, data},
    State;

data_session({ok, Session}, {data, _Subscriber, GP, TimeSpec, LDN, {wait, WaitTime, Bundles}}, State) ->
    {ok} = rct_proxy:send_proxy(
	     ?NODE,
	     Session#pmSession.child, 
	     ?PMI_DATA, 
	     {Session#pmSession.handle, 
	      GP, 
	      TimeSpec, 
	      LDN, 
	      pms_c_util:tuplifyBundles(Bundles)}),
    
    timer:sleep(WaitTime),
    Session#pmSession.appPid ! {response, data},
    State;

data_session({ok, Session}, {data, _Subscriber, GP, TimeSpec, LDN, Bundles}, State) ->
    {ok} = rct_proxy:send_proxy(?NODE,
				Session#pmSession.child, 
				?PMI_DATA, 
				{Session#pmSession.handle, 
				 GP, 
				 TimeSpec, 
				 LDN, 
				 pms_c_util:tuplifyBundles(Bundles)}),
    Session#pmSession.appPid ! {response, data},
    State;

data_session(Error, _, State) ->
    ?TEST_DEBUG("skipping sending a 'data' message due to ~p", [Error]),
    State.



data_sc({data_sc, Subscriber, _, _, _, _} = Data, State) ->
    data_sc_session(dict_fetch(Subscriber, 
			       State#state.sessionByChild), 
		    Data,
		    State).


%% data_sc_session({ok, Session}, {data_sc, _Subscriber, _, _, _}, State) ->
%%     ?TEST_DEBUG("skip sending a 'data_sc' message, TimeSpec: ~w", [TimeSpec]),
%%     Session#pmSession.appPid ! {response, data_sc},
%%     State;

data_sc_session({ok, Session}, 
		{data_sc, _Subs, RequestId, Result, 
		 ErrorStr, Bundles, {wait, WaitTime, Bundles}}, 
		State) ->
    {ok} = rct_proxy:send_proxy(
	     ?NODE,
	     Session#pmSession.child, 
	     ?PMI_DATA_SHOW_COUNTERS, 
	     {Session#pmSession.handle, 
	      RequestId, 
	      Result,
	      ErrorStr,
	      pms_c_util:tuplifyBundles(Bundles)}),
    
    timer:sleep(WaitTime),
    Session#pmSession.appPid ! {response, data_sc},
    State;

data_sc_session({ok, Session}, 
		{data_sc, _Subscriber, RequestId, Result, ErrorStr, Bundles},
		State) ->
    {ok} = rct_proxy:send_proxy(?NODE,
				Session#pmSession.child, 
				?PMI_DATA_SHOW_COUNTERS, 
				{Session#pmSession.handle, 
				 RequestId, 
				 Result,
				 ErrorStr,
				 pms_c_util:tuplifyMTs(Bundles)}),
    Session#pmSession.appPid ! {response, data_sc},
    State;

data_sc_session(Error, _, State) ->
    ?TEST_DEBUG("skipping sending a 'data_sc' message due to ~p", [Error]),
    State.



subscribe({Handle, _GP, _GroupSpecs} = Subscribe, State) ->
    subscribe_session(dict_fetch(Handle, State#state.sessionByHandle), Subscribe, State).

subscribe_session({ok, Session}, {_Handle, GP, GroupSpecs}, State) ->
    apply(Session#pmSession.cbModule, 
	  pmiSubscribeCallback, 
	  [Session#pmSession.appPid, Session#pmSession.child, GP, GroupSpecs]),
    State;
subscribe_session(Error, _, State) ->
    ?TEST_DEBUG("skipping sending a 'subscribe' message due to ~p", [Error]),
    State.


dict_fetch(Subscriber, Session) ->
    try 
	{ok, dict:fetch(Subscriber, Session)}
    catch
	error:badarg ->
	    {error, not_found_in_dict}
    end.
	    

start_ift_trace(#cth_state{cs_erl_node = CsNode, trace_ift = TraceIFT}) 
  when TraceIFT =:= true,
       CsNode =/= undefined ->
    start_trace("ift_master", CsNode);

start_ift_trace(_) ->
    ok.


%% exit_master(#cth_state{cs_erl_node = CsNode}) when CsNode =/= undefined ->
%%     ct:log("Call ~p appmServer:activate() to reset app", [CsNode]), 
%%     ok = rpc:call(CsNode, appmServer, activate, [], 10000),
%%     timer:sleep(10000); %% fix to let the LMs to be started
    
%% exit_master(_) ->
%%     ok.


stop_ift_trace(#cth_state{cs_erl_node = CsNode, trace_ift = TraceIFT}) 
  when TraceIFT =:= true,
       CsNode =/= undefined ->
    stop_trace("ift_master", CsNode).


start_child_trace(Child, State) when State#state.trace_child =:= true ->
    timer:sleep(3000),    
    start_trace(?CHILD_PREFIX ++ atl(Child));

start_child_trace(_Child, _State) ->
    ok.


start_trace(Process) ->
    case get_CsErlNode(?NODE) of
	{ok, CsNode} ->
	    start_trace(Process, CsNode);
	_ ->
	    ok
    end.


start_trace(Process, CsNode) ->
    Res = rpc:call(CsNode, os, cmd, 
		   [te_cmd("enable all " ++ atl(Process), CsNode)]),
    ct:log("Start trace on ~p: ~p", [Process, Res]).


%% stop_trace(Process) ->
%%     case get_CsErlNode(?NODE) of
%% 	{ok, CsNode} ->
%% 	    stop_trace(Process, CsNode);
%% 	_ ->
%% 	    ok
%%     end.


stop_trace(Process, CsNode) ->
    Res = rpc:call(CsNode, os, cmd, 
		   [te_cmd("disable all " ++ atl(Process), CsNode)]),
    ct:log("Stop trace on ~p: ~p", [Process, Res]).


log_te_log(TC, Config, #cth_state{cs_erl_node = CsNode, trace_ift = true})
  when CsNode =/= undefined ->
    TELog = get_te_log(CsNode),
    pms_test_lib:log_te_log(TELog, atl(TC), Config); 

log_te_log(_TC, _Config, _CTHState) ->
    ok.


get_te_log(CsNode) -> 
    Cmd = te_cmd("log read", CsNode),
    get_te_log(2, CsNode, Cmd). 


get_te_log(N, CsNode, Cmd) when N > 0 -> 
    case rpc:call(CsNode, os, cmd, [Cmd]) of
	[] ->
	    timer:sleep(2000),
	    get_te_log(N - 1, CsNode, Cmd);
	TELog ->
	    TELog
    end;

get_te_log(_N, _CsNode, _Cmd) -> 
    [].    

clear_te_log(#cth_state{cs_erl_node = CsNode, trace_ift = true}) 
  when CsNode =/= undefined ->
    rpc:call(CsNode, os, cmd, [te_cmd("log clear", CsNode)]);

clear_te_log(_CTHState) ->
    ok.    


te_cmd(Params, _CsNode) ->
    %% case rpc:call(CsNode, os, getenv, ["COLIDIR"]) of
    %% 	Path when is_list(Path) ->
    %% 	    Cmd = Path ++ "/bin/colish -c " ++ "\"tex " ++ Params ++ "\"",
    %% 	    ct:log("TE log cmd: ~s", [Cmd]),
    %% 	    Cmd;	    
    %% 	_False ->
    %% 	    Cmd = "tex " ++ Params,
    %% 	    ct:log("TE log cmd: ~s", [Cmd]),
    %% 	    Cmd
    %% end.
    Cmd = "tex " ++ Params,
    ct:log("TE log cmd: ~s", [Cmd]),
    Cmd.


%% This function should be exported by rct_proxy
get_CsErlNode(Node) ->
    case ets:info(rct_proxy) of
	undefined ->
	    {error,{rct_proxy, undefined}};
	_ ->
	    case ets:lookup(rct_proxy, Node) of
		[{Node, {_ErlNode, CsErlNode}}] ->
		    {ok, CsErlNode};
		Other ->
		    {error, Other}
	    end
    end.


atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) when is_list(L) ->
    L.


%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_erl_app.erl %
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R3A/8

-module(pes_erl_app).

-include("pes_test.hrl").


%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% CT hook callback functions
-export([
	 id/1,
	 init/2,
	 pre_init_per_suite/3,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 on_tc_fail/3,
	 terminate/1
	]).

%% test_app process functions
-export([start/2]).
-export([stop/1]).
-export([kill/2]).

-export([init/3]).


-export([peiInitialize/3]).
-export([peiFinalize/2]).

-export([aliases/2]).
-export([expected/3]).


-define(PROXY, pes_pei_proxy).
-define(PMI, pesPeI).
-define(TS_TO, 10000).
-define(APP_SUP, ?MODULE).

-record(cth_state, {opts, 
		    name, 
		    stop_apps = on_tc_end}).

-record(loop, {name,
	       rpc,
	       api_server,
	       api_module,
	       cb_module,
	       handle,
	       monitor_ref,
	       expected = [],
	       expected_ref,
	       ts_pid,
	       cb_flags = [],
	       event_map}).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%%===================================================================
%%% Testapp functions
%%%===================================================================
start(Name, Options) ->
    Res = proc(whereis(Name), Name, Options),
    p("=== erl_app === ~p = start(~p, ~p)~n", 
      [Res, Name, Options]),
    case Res of
	{ok, Pid} = Res->
	    store_app({Pid, Name}),
	    Res;
	Error ->
	    Error
    end.


stop(Name) ->
    p("=== erl_app === stop(~p)~n", [Name]),
    remove_app(Name), 
    Name ! {stop, self()},
    receive 
	stopped ->
	    ok
    after 1000 ->
	    Pid = whereis(Name),  
	    p("=== erl_app === Force ~p exit(~p, kill)~n", [Name, Pid]),
	    Pid =:= undefined orelse exit(Pid, kill),
	    ok
    end.

kill(Name, Module) ->
    Name ! {kill, Module}.




proc(undefined, Name, Options) ->
    p("=== erl_app === ~p = proc(undefined, ~p)~n",
      [Name, Options]),
    Self = self(),
    Pid = spawn(?MODULE, init, [Self, Name, Options]),
    %% To avoid race condition, send back Pid
    receive 
	inited -> {ok, Pid}
    after 5000 -> {error, {timeout, test_app}}
    end;
proc(Pid, Name, Options) ->
    p(lightred, "=== erl_app === kill old process ~p~n", [Pid]),
    stop(Name),
    proc(undefined, Name, Options).





peiInitialize(Name, CbFlags, EventMap) ->
    Name ! {initialize, self(), CbFlags, EventMap},
    receive
	{initialize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, {initialize_result, timeout}}
    end.


peiFinalize(Name, Handle) ->
    Name ! {finalize, self(), Handle},
    receive
	{finalize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.



aliases(Name, Aliases) ->
    Name ! {aliases, self(), Aliases}.

expected(Name, Handle, Expected) ->
    Ref = make_ref(),
    Name ! {expected, self(), Ref, Handle, Expected},
    {ok, Ref}.


%%%===================================================================
%%% CT Hooks Callbacks
%%%===================================================================
%% @private
id(_Opts) ->
    ?MODULE.

%% @private
init(Id, Opts) ->
    p("~w: init(~p, ~p)", [?MODULE, Id, Opts]),
    CTHState = #cth_state{name = lta(Id), opts = Opts},
    StopApps = proplists:get_value(stop_apps, Opts, 
				   CTHState#cth_state.stop_apps),
    start_app_sup(CTHState),
    {ok, CTHState#cth_state{stop_apps = StopApps}}.


%% @private
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_suite(_SuiteName, InitData, CTHState) ->
    case whereis(CTHState#cth_state.name) of
	Pid when is_pid(Pid) ->
	    stop_all_apps(Pid, CTHState),
	    {InitData, CTHState};
	_Undef ->
	    {{skip, app_server_down}, CTHState}
    end.

%% @private
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_testcase(_TC, Config, CTHState) ->
    case whereis(CTHState#cth_state.name) of
	Pid when is_pid(Pid) ->
	    {Config, CTHState};
	_Undef ->
	    {{skip, app_server_down}, CTHState}
    end.

%% @private
post_end_per_testcase(_TC, _Config, Return, CTHState) ->
    CTHState#cth_state.stop_apps =:= on_tc_end andalso 
	stop_all_apps(CTHState),
    {Return, CTHState}.


%% @private
on_tc_fail(_TC, _Reason, CTHState) ->
    CTHState#cth_state.stop_apps =:= on_tc_fail andalso 
	stop_all_apps(CTHState),
    CTHState.
    

%% @private
terminate(CTHState) ->
    Server = CTHState#cth_state.name,
    Pid = whereis(Server),
    p("~w: terminate, server ~p pid = ~p", [?MODULE, Server, Pid]),
    case Pid of
	undefined ->
	    ok;
	_ ->
	    stop_all_apps(Pid, CTHState),
	    stop_app_sup(Pid, CTHState),
	    catch exit(Pid, kill)
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
start_app_sup(CTHState) ->
    Pid = spawn(fun() ->
			register(?APP_SUP, self()),
			Tab = ets:new(CTHState#cth_state.name, []),
			app_sup_loop(Tab)
		end),
    Pid ! {ping, self()},
    receive
	{pong, Pid} ->
	    p("~p: Started erl_app sup ~p", [?MODULE, Pid]), 
	    {ok, Pid}
    after 20000 ->
	    exit(Pid, kill),
	    {error, failed_to_start_app_sup}
    end.


app_sup_loop(Tab) ->
    receive
	{store_app, {Pid, _Name} = Item} ->
	    ets:insert(Tab, Item),
	    ct:log("~p: Store ~p in Tab", [?MODULE, Item]),
	    erlang:monitor(process, Pid),
	    app_sup_loop(Tab);
	{remove_app, {Pid, Name}} ->
	    ets:delete(Tab, Pid),
	    ct:log("~p: Remove ~p from Tab", [?MODULE, {Name, Pid}]),
	    app_sup_loop(Tab);
	{remove_app, Name} when is_atom(Name) ->
	    Pid = whereis(Name),
	    ets:delete(Tab, Pid),
	    ct:log("~p: Remove ~p from Tab", [?MODULE, {Name, Pid}]),
	    app_sup_loop(Tab);
	{remove_app, Pid} when is_pid(Pid) ->
	    ets:delete(Tab, Pid),
	    ct:log("~p: Remove ~p from Tab", [?MODULE, Pid]),
	    app_sup_loop(Tab);
	{get_apps, From} ->
	    Apps = ets:tab2list(Tab),
	    From ! {ok, Apps, self()},
	    app_sup_loop(Tab);
	{'DOWN', _MonitorRef, _Type, Object, _Info} ->
	    [] =:= ets:lookup(Tab, Object) orelse
		begin
		    ets:delete(Tab, Object),
		    ct:log("~p: ~p is unexpectedly DOWN, remove from Tab", 
			   [?MODULE, Object])
		end,
	    app_sup_loop(Tab);
	{ping, From} ->
	    From ! {pong, self()},
	    app_sup_loop(Tab);
	{stop, From} ->
	    unregister(?APP_SUP),
	    ets:delete(Tab),
	    ct:log("~p: Delete app Tab and terminate", [?MODULE]),
	    From ! {stopped, self()}
    end.


stop_app_sup(Pid, _CTHState) ->
    Pid ! {stop, self()},
    receive
	{stopped, Pid} ->
	    ok
    after 5000 ->
	    exit(Pid, kill),
	    ok
    end.
	    

store_app(App) ->
    store_app(whereis(?APP_SUP), App).
    

store_app(Server, App) when is_pid(Server) ->
    Server ! {store_app, App},
    ok;

%% store_app(?APP_SUP, App) ->
%%     store_app(whereis(?APP_SUP), App);

store_app(_Server, _App) ->
    ok.


remove_app(App) ->
    remove_app(whereis(?APP_SUP), App).
    

remove_app(Server, App) when is_pid(Server), is_atom(App) ->
    case whereis(App) of
	Pid when is_pid(Pid) ->
	    Server ! {remove_app, {Pid, App}},
	    ok;
	_Undef ->
	    ok
    end;

remove_app(Server, App) when is_pid(Server), is_pid(App) ->
    Server ! {remove_app, App},
    ok;

%% remove_app(?APP_SUP, App) ->
%%     remove_app(whereis(?APP_SUP), App);

remove_app(_Server, _App) ->
    ok.


get_apps(Pid, _CTHState) ->
    Pid ! {get_apps, self()},
    receive
	{ok, Apps, Pid} ->
	    {ok, Apps}
    after 5000 ->
	    {error, timeout}
    end.


stop_all_apps(CTHState) ->
    Pid = whereis(CTHState#cth_state.name),
    stop_all_apps(Pid, CTHState).


stop_all_apps(Pid, CTHState) when is_pid(Pid) ->
    case get_apps(Pid, CTHState) of
	{ok, Apps} ->
	    stop_apps(Apps);
	Error ->
	    p("~p: Failed to fetch test apps: ~p", [?MODULE, Error]),
	    Error
    end;

stop_all_apps(_Pid, _CTHState) ->
    ok.


stop_apps(Apps) ->
    p("~p: Stop test apps: ~p", [?MODULE, Apps]),
    [catch stop(App) || {_Pid, App} <- Apps],
    ok.

%%========================================================================
%% Init function for the Job process
%%========================================================================
init(TsPid, Name, Options) ->
    p("=== APP(~p) ===  init pid: ~p   TS pid: ~p~n", [Name, self(), TsPid]),
    register(Name, self()),
    TsPid ! inited,

    Rpc       = proplists:get_value(rpc, Options, false),
    ApiServer = proplists:get_value(api_server, Options, undefined),
    ApiModule = proplists:get_value(api_module, Options, ?PROXY),
    CbModule  = proplists:get_value(cb_module,  Options, ApiModule),
    
    loop(#loop{name       = Name,
	       rpc        = Rpc,
	       api_server = ApiServer,
	       api_module = ApiModule,
	       cb_module  = CbModule}).




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% loop(#loop{}) 
%%
%% Job loop. Loop until the job is deleted.
%%========================================================================
loop(#loop{name         = Name,
	   rpc          = Rpc,
	   api_server   = ApiServer,
	   api_module   = ApiModule,
	   cb_module    = CbModule,
	   handle       = Handle,
	   ts_pid       = TsPid,
	   expected     = Expected,
	   expected_ref = ExpRef} = Loop) ->
    %%p("######### ERL_APP LOOP ~p~n~p~n#########", [Name, Loop]),

    receive 
	%%===================================================
	%% messages from PES
	%%===================================================
	{peiEventJobCallback = Msg, RecData} ->
	    p("=== PES ---> APP(~p) === ~p ~p~nData: ~p~n",
	      [Name, peiEventJob, self(), RecData]),
	    NewExpected = handle_callback(Name,
					  Msg,
					  RecData,
					  TsPid,
					  Expected,
					  ExpRef),
	    loop(Loop#loop{expected = NewExpected});
	
	{peiMEAttrUpdateCallback = Msg, RecData} ->
	    p("=== PES ---> APP(~p) === ~p ~p~nData: ~p~n",
	      [Name, peiMeAttrUpdate, self(), RecData]),
	    NewExpected = handle_callback(Name,
					  Msg,
					  RecData,
					  TsPid, 
					  Expected,
					  ExpRef),
	    loop(Loop#loop{expected = NewExpected});
	
	%%===================================================
	%% orders from the test suite
	%%===================================================
	{initialize, From, CbFlags, EventMap} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~n"
	      "CbFlags:    ~p~n"
	      "EventMap: ~p~n",
	      [Name, initialize, self(), CbFlags, EventMap]),
	    Res = handle_initialize(Name, 
				    Rpc,
				    ApiServer, 
				    ApiModule, 
				    CbModule, 
				    CbFlags,
				    EventMap),
	    From ! {initialize_result, Res},
	    case Res of
		{ok, NewHandle} ->
		    MRef = erlang:monitor(process, NewHandle),
		    loop_me_attr(Loop#loop{cb_flags    = CbFlags,
					   event_map   = EventMap,
					   handle      = NewHandle,
					   monitor_ref = MRef});
		_ ->
		    loop(Loop)
	    end;

	{finalize, From, Handle} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~n",
	      [Name, finalize, self()]),
	    Res = handle_finalize(Name, Rpc, ApiModule, Handle),
	    From ! {finalize_result, Res},
	    unmonitor(Loop#loop.monitor_ref),
	    loop(Loop#loop{monitor_ref = undefined});
	
	{expected, From, NewExpRef, Handle, NewExpected} ->
	    ct:pal("##### EXPECTED ~p~n", [NewExpected]),
	    p("=== APP(~p) <--- SUITE === ~p ~p ~p~n",
	      [Name, expected, self(), NewExpected]),
	    loop(Loop#loop{handle       = Handle,
			   ts_pid       = From,
			   expected     = NewExpected,
			   expected_ref = NewExpRef});

	{stop, UserPid} ->
	    p("=== APP(~p) === ~p STOPPED ~n", [Name, self()]),
	    unregister(Name),
	    UserPid ! stopped,
	    ok;

	{'DOWN', MonitorRef, _Type, Object, _Info} 
	  when Loop#loop.monitor_ref =:= MonitorRef ->
	    %% FIXME: This should be handled
	    p("=== APP(~p) === Got unexpected DOWN for Handle ~p~n", 
	      [Name, Object]),
	    loop(Loop);

	{'DOWN', _MonitorRef, _Type, Object, _Info} ->
	    p("=== APP(~p) === Got DOWN for unknown Handle ~p~n", 
	      [Name, Object]),
	    loop(Loop);

	UnknownMsg ->
	    p("=== APP(~p) === ~p UNKNOWN MESSAGE ~p ~n~p~n", 
	      [Name, self(), UnknownMsg, Loop]),
	    loop(Loop)
    end.





%% Temporary loop just to receive the first peiMEAttrUpdateCallback
%% after peiInitiialize (only if the callback is defined).
loop_me_attr(Loop) ->
    CbFlag = proplists:get_value(peiMEAttrUpdateCallback, Loop#loop.cb_flags),
    loop_me_attr(CbFlag, Loop).
    

loop_me_attr(true, Loop) ->
    receive 
	{peiMEAttrUpdateCallback, _} ->
	    loop(Loop)
    after 5000 ->
	    {error, {peiMEAttrUpdateCallback, timeout}}
    end;

loop_me_attr(_False, Loop) ->
    loop(Loop).
    

%%========================================================================
%% handle_initialize(Name, Rpc, ApiServer, ApiModule, 
%%                   CbModule, CbFlags, EventMap)
%% 
%% 
%%========================================================================
handle_initialize(Name, 
		  true, 
		  _, 
		  ApiModule, 
		  CbModule, 
		  CbFlags, 
		  EventMap) ->
    p("=== PES <--- APP(~p) === ~p ~p ~p~n"
      "CbFlags:    ~p~n"
      "EventMap: ~p~n",
      [Name, CbModule, initialize, self(), CbFlags, EventMap]),
    Args = [EventMap, CbFlags, CbModule, self()],
    ?PROXY:cs_node_call(ApiModule, peiInitialize, Args);
handle_initialize(Name,
		  _, 
		  undefined, 
		  ApiModule, 
		  CbModule, 
		  CbFlags, 
		  EventMap) ->
    p("=== PES <--- APP(~p) === ~p ~p ~p~n"
      "CbFlags:    ~p~n"
      "EventMap: ~p~n",
      [Name, CbModule, initialize, self(), CbFlags, EventMap]),
    ApiModule:peiInitialize(EventMap, CbFlags, CbModule, self());
handle_initialize(Name,
		  _, 
		  ApiServer, 
		  ApiModule, 
		  CbModule, 
		  CbFlags, 
		  EventMap) ->
    p("=== PES <--- APP(~p) === ~p ~p ~p~n"
      "CbFlags:    ~p~n"
      "EventMap: ~p~n",
      [Name, CbModule, initialize, self(), CbFlags, EventMap]),
    ApiModule:peiInitialize(ApiServer, EventMap, CbFlags, CbModule, self()).



%%========================================================================
%% finalize
%% 
%% 
%%========================================================================
handle_finalize(Name, true, _ApiModule, _Handle) ->
    p("=== APP(~p) <--- SUITE === ~p ~p ~n",
      [Name, finalize, self()]),
    ?PROXY:cs_node_call(?PMI, peiFinalize, []);
handle_finalize(Name, _, ApiModule, Handle) ->
    p("=== APP(~p) <--- SUITE === ~p ~p ~n",
      [Name, finalize, self()]),
    ApiModule:peiFinalize(Handle).


%%========================================================================
%% handle_callback() -> NewExpected || {error, {Expected, ReceivedMsg}}
%% 
%% 
%%========================================================================
%% handle_callback(Name, Msg, Data, _TsPid, [] = Expected, _ExpRef) ->
%%     p("NOT EXPECTED: === APP(~p) === ~p ~nData: ~p~n", [Name, Msg, Data]),
%%     Expected;
handle_callback(_Name, 
		Msg, 
		Data,
		TsPid, 
		[{send_copy, _, _} | _] = Expected,
	        _ExpRef) ->
    TsPid ! {copy, {Msg, Data}},
    Expected;
handle_callback(Name, 
		Msg, 
		Data,
		TsPid, 
		Expected, 
		ExpRef) ->
    io:format(user, "##### Expected ~p~n", [Expected]),
    HcRes = hc(Msg, Data, Expected),
    hc_rc(HcRes, Name, ExpRef, TsPid).


hc_rc({ok, []}, Name, ExpRef, TsPid) ->
    io:format(user, "##### expected sending result ~p~n", 
	      [{expected_result, ExpRef, Name, ok}]),
    TsPid ! {expected_result, ExpRef, Name, ok},
    [];
hc_rc({ok, NewExpected}, _Name, _ExpRef, _) ->
    io:format(user, "##### expected remaining ~p~n", [NewExpected]),
    NewExpected;
hc_rc(Error, Name, ExpRef, TsPid) ->
    io:format(user, "##### expected ERROR ~p~n",
	      [{expected_result, ExpRef, Name, Error}]),
    TsPid ! {expected_result, ExpRef, Name, Error},
    [].


	    


hc(peiEventJobCallback, Data, Expected) ->
    check_event_job(Data, Expected);
hc(peiMEAttrUpdateCallback, Data, Expected) ->
    check_me_attr_update(Data, Expected).



%%---------------------------------------------
%% event job
%%---------------------------------------------
check_event_job(Job, [Job | T]) ->
    {ok, T};
check_event_job(Got, [Exp | _]) ->
    {error, {{expected, Exp}, {received, Got}}}.

%%---------------------------------------------
%% me attr update
%%---------------------------------------------
check_me_attr_update(MeAttr, [MeAttr | T]) ->
    {ok, T};
check_me_attr_update(Got, [Exp | _]) ->
    {error, {{expected, Exp}, {received, Got}}}.







%%========================================================================
%% p(Str, Args) -> ok
%% 
%% Print function.
%%========================================================================
p(Str, Args) ->
    case ct:get_status() of
	List when is_list(List) -> pes_test_lib:ct_pal(Str, Args);
	_                       -> ok
    end.

p(Colour, Str, Args) ->
    case ct:get_status() of
	List when is_list(List) -> ct:pal(Colour, Str, Args);
	_                       -> ok
    end.


lta(L) when is_list(L) ->
    list_to_atom(L);

lta(A) when is_atom(A) ->
    A.


unmonitor(undefined) ->
    ok;

unmonitor(Ref) ->
    demonitor(Ref),
    receive
	{'DOWN', Ref, _Type, _Object, _Info} ->
	    ok
    after 0 ->
	    ok
    end.


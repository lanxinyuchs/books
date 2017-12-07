%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_erl_app_relay.erl %
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R6A/1

-module(pms_erl_app_relay).

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
-export([start/3]).
-export([stop/1]).
-export([kill/2]).

-export([init_pmi/3]).
-export([init_pmi2/3]).


-export([pmi2Initialize/3]).
-export([pmi2Finalize/2]).
-export([pmi2DataRop/6]).
-export([pmi2DataSc/6]).

-export([pmi2CounterMap/3]).

-export([aliases/2]).


-define(PROXY,   pms_pmi_proxy).
-define(PMI,     pmsPmI).
-define(PMI2,    pmsPmI2).
-define(TS_TO,   10000).
-define(APP_SUP, ?MODULE).

-record(cth_state, {opts, 
		    name, 
		    stop_apps = on_tc_end}).

-record(loop, {name,             %% string()  - name of the application instance
	       rpc,              %% bool()    - if rpc is used
	       api_server,       %% atom()    -
	       api_module,       %% atom()    -
	       cb_module,        %% atom()    -
	       handle,           %% pid()     -
	       monitor_ref,      %% ref()     -
	       ts_pid,           %% pid()     - to test server
	       pmi_mod,          %% atom()    -
	       gp,               %% integer() - 
	       timer_ref,        %% ref()     -
	       cb_flags = [],    %% {SubscCbFnc, RopCbFnc, ScCbFnc} - all bool()
	       counter_map,      %%           -
	       meas_type,        %%           -
	       subscr_spec = [], %%           -
	       subscriber,       %%           -
	       ldn,              %% [integer()] - ldn alias
	       cvals = []}).     %% 


%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%%===================================================================
%%% Testapp functions
%%%===================================================================
start(Vsn, Name, Options) ->
    Res = proc(whereis(Name), Vsn, Name, Options),
    p("=== ~p === ~p = start(~p, ~p)~n", [?MODULE, Res, Name, Options]),
    case Res of
	{ok, Pid} = Res->
	    store_app({Pid, Name}),
	    Res;
	Error ->
	    Error
    end.


stop(Name) ->
    p("=== ~p === stop(~p)~n", [?MODULE, Name]),
    remove_app(Name), 
    Name ! {stop, self()},
    receive 
	stopped ->
	    ok
    after 1000 ->
	    Pid = whereis(Name),  
	    p("=== ~p === Force ~p exit(~p, kill)~n", [?MODULE, Name, Pid]),
	    Pid =:= undefined orelse exit(Pid, kill),
	    ok
    end.

kill(Name, Module) ->
    Name ! {kill, Module}.




proc(undefined, Vsn, Name, Options) ->
    p("=== ~p === ~p = proc(undefined, ~p)~n", [?MODULE, Name, Options]),
    Self = self(),
    Init = choose(Vsn == pmi, init_pmi, init_pmi2),
    Pid = spawn(?MODULE, Init, [Self, Name, Options]),
    %% To avoid race condition, send back Pid
    receive 
	inited -> {ok, Pid}
    after 5000 -> {error, {timeout, test_app}}
    end;
proc(Pid, Vsn, Name, Options) ->
    p(lightred, "=== ~p === kill old process ~p~n", [?MODULE, Pid]),
    stop(Name),
    proc(undefined, Vsn, Name, Options).





pmi2Initialize(Name, CbFlags, CounterMap) ->
    Name ! {initialize, self(), CbFlags, CounterMap},
    receive
	{initialize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.


pmi2Finalize(Name, Handle) ->
    Name ! {finalize, self(), Handle},
    receive
	{finalize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.


pmi2DataRop(Name, Handle, GP, ReportId, ValueBundle, FF) ->
    ct:pal("#### ~p  pmi2DataRop ~p ~n", [?MODULE, Name]),
    Name ! {pmi2DataRop, self(), Handle, GP, ReportId, ValueBundle, FF},
    receive
	{pmi2DataRop_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.


pmi2DataSc(Name, Handle, ReportId, Result, ErrorStr, MeasVals) ->
    Name ! {pmi2DataSc, self(), Handle, ReportId, Result, ErrorStr, MeasVals},
    receive
	{pmi2DataSc_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.

pmi2CounterMap(Name, Handle, Maps) ->
    Name ! {counter_map, self(), Handle, Maps},
    receive
	{counter_map_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.


aliases(Name, Aliases) ->
    Name ! {aliases, self(), Aliases}.


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
init_pmi(_TsPid, _Name, _Options) ->
    ok.

init_pmi2(TsPid, Name, Options) ->
    p("=== APP(~p) ===  init pid: ~p   TS pid: ~p~n"
      "Options ~p~n", [Name, self(), TsPid, Options]),
    register(Name, self()),
    TsPid ! inited,

    Rpc       = proplists:get_value(rpc, Options, false),
    ApiServer = proplists:get_value(api_server,   Options, undefined),
    ApiModule = proplists:get_value(api_module,   Options, ?PROXY),
    CbModule  = proplists:get_value(cb_module,    Options, ApiModule),
    
    loop(#loop{name         = Name,
	       rpc          = Rpc,
	       api_server   = ApiServer,
	       api_module   = ApiModule,
	       cb_module    = CbModule,
	       ts_pid       = TsPid}).




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
	   pmi_mod      = _PmiMod,
	   gp           = GP,
	   subscr_spec  = SubscrSpec,
	   ldn          = LDN,
	   cvals        = _CVals} = Loop) ->
    %%p("######### ERL_APP LOOP ~p~n~p~n#########", [Name, Loop]),

    receive 
	%%===================================================
	%% messages from PMS
	%%===================================================
	{pmi2SubscribeRop, {NewGP, NewSubscrSpec}} = Msg ->
	    p("=== PMS ---> APP(~p) === ~p ~p~nSubscrSpec: ~p~n",
	      [Name, pmi2SubscribeRop, self(), NewSubscrSpec]),
	    TsPid ! {copy, Name, Msg},
	    loop(Loop#loop{gp          = NewGP,
			   subscr_spec = NewSubscrSpec});
	
	{pmi2ReportRop, RopRequest} = Msg ->
	    p("=== PMS ---> APP(~p) === ~p ~p~nRequest: ~p~n",
	      [Name, pmi2ReportRop, self(), RopRequest]),
	    TsPid ! {copy, Name, Msg},
	    loop(Loop);
	
	{pmi2ReportShowCounters, ScRequest} = Msg ->
	    p("=== PMS ---> APP(~p) === ~p ~p~nRequest: ~p~n",
	      [Name, pmi2ReportShowCounters, self(), ScRequest]),
	    TsPid ! {copy, Name, Msg},
	    loop(Loop);
	
	%%===================================================
	%% orders from the test suite
	%%===================================================
	{initialize, From, CbFlags, CounterMap} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~n"
	      "CbFlags:    ~p~n"
	      "CounterMap: ~p~n",
	      [Name, initialize, self(), CbFlags, CounterMap]),
	    Res = handle_initialize(Name, 
				    Rpc,
				    ApiServer, 
				    ApiModule, 
				    CbModule, 
				    CbFlags,
				    CounterMap),
	    From ! {initialize_result, Res},
	    case Res of
		{ok, NewHandle} ->
		    MRef = erlang:monitor(process, NewHandle),
		    loop(Loop#loop{cb_flags    = CbFlags,
				   counter_map = CounterMap,
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
	
	{pmi2DataRop, From, Handle, ReqGP, RId, Vals, FF} ->
	    UseGP    = choose(ReqGP == gp, GP, ReqGP),
	    DefVals  = get_default_report_rop_reply(LDN, SubscrSpec),
	    UsedVals = choose(Vals == default, DefVals, Vals),
	    Res = handle_data_rop(Name, Handle, CbModule, UseGP, RId, UsedVals, FF),
	    From ! {pmi2DataRop_result, Res},
	    loop(Loop);

	{pmi2DataSc, From, Handle, ReportId, Result, ErrorStr, MeasVals} ->
	    Res = handle_data_sc(Name, 
				 Handle, 
				 CbModule, 
				 ReportId, 
				 Result, 
				 ErrorStr, 
				 MeasVals),
	    From ! {pmi2DataSc_result, Res},
	    loop(Loop);

	{counter_map, From, Handle, Maps} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p ~p~n",
	      [Name, counter_map, self(), Maps]),
	    Res = handle_counter_map(Name, Rpc, ApiModule, Handle, Maps),
	    From ! {counter_map_result, Res},
	    loop(Loop);

	{aliases, _From, NewAliases} ->
	    p("=== APP(~p) <--- SUITE === ~n~p ~p ~p~n",
	      [Name, aliases, self(), NewAliases]),
	    loop(Loop#loop{ldn = [A || {_, A} <- NewAliases]});

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

	X ->
	    p("=== APP(~p) === ~p UNKNOWN MESSAGE ~p   (~p)~n~p~n",
	      [Name, self(), X, ?MODULE, Loop]),
	    loop(Loop)
    end.





%%========================================================================
%% handle_initialize(Name, Rpc, ApiServer, ApiModule, 
%%                   CbModule, CbFlags, CounterMap)
%% 
%% 
%%========================================================================
handle_initialize(Name, true, _, ApiModule, CbModule, CbFlags, CounterMap) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "CbFlags:    ~p~n"
      "CounterMap: ~p~n",
      [Name, CbModule, initialize2, self(), CbFlags, CounterMap]),
    Args = [CbFlags, CounterMap, CbModule, self()],
    ?PROXY:cs_node_call(ApiModule, pmi2Initialize, Args);
handle_initialize(Name,
		  _, 
		  undefined, 
		  ApiModule, 
		  CbModule, 
		  CbFlags, 
		  CounterMap) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "CbFlags:    ~p~n"
      "CounterMap: ~p~n",
      [Name, CbModule, initialize2, self(), CbFlags, CounterMap]),
    ApiModule:pmi2Initialize(CbFlags, CounterMap, CbModule, self());
handle_initialize(Name,
		  _, 
		  ApiServer, 
		  ApiModule, 
		  CbModule, 
		  CbFlags, 
		  CounterMap) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "CbFlags:    ~p~n"
      "CounterMap: ~p~n",
      [Name, CbModule, initialize2, self(), CbFlags, CounterMap]),
    ApiModule:pmi2Initialize(ApiServer, CbFlags, CounterMap, CbModule, self()).


%%========================================================================
%% handle_data_rop(Name, Rpc, ApiServer, ApiModule, 
%%                   CbModule, CbFlags, CounterMap)
%% 
%% 
%%========================================================================
handle_data_rop(Name, Handle, CbModule, GP, ReportId, ValueBundle, FF) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "GP:       ~p~n"
      "ReportId: ~p~n"
      "Data:     ~p~n"
      "FF:       ~p~n",
      [Name, CbModule, pmi2DataRop, self(), GP, ReportId, ValueBundle, FF]),
    co_send_rop_data(0, Handle, GP, ReportId, ValueBundle, FF).

%%    ?PROXY:pmi2DataRop(Handle, GP, ReportId, ValueBundle, FF).


handle_data_sc(Name, Handle, CbModule, ReportId, Result, ErrorStr, MeasVals) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "ReportId:  ~p~n"
      "Result:    ~p~n"
      "HorrorStr: ~p~n"
      "Data:      ~p~n",
      [Name, CbModule, pmi2DataSc, self(), ReportId, Result, ErrorStr, MeasVals]),
    co_send_sc_data(0, Handle,ReportId, Result, ErrorStr, MeasVals). 

   

%%========================================================================
%% counter_map
%% 
%% 
%%========================================================================
handle_counter_map(_Name, true, _ApiModule, _Handle, Maps) ->
    ?PROXY:cs_node_call(?PMI2, pmi2CounterMap, [Maps]);
handle_counter_map(_Name, _, ApiModule, Handle, Maps) ->
    ApiModule:pmi2CounterMap(Handle, Maps).


%%========================================================================
%% finalize
%% 
%% 
%%========================================================================
handle_finalize(Name, true, _ApiModule, _Handle) ->
    p("=== APP(~p) <--- SUITE === ~p ~p ~n",
      [Name, finalize, self()]),
    ?PROXY:cs_node_call(?PMI2, pmi2Finalize, []);
handle_finalize(Name, _, ApiModule, Handle) ->
    p("=== APP(~p) <--- SUITE === ~p ~p ~n",
      [Name, finalize, self()]),
    ApiModule:pmi2Finalize(Handle).



%%===========================================
%% send rop data
%%===========================================
%% send fragmented reply
co_send_rop_data(0, Handle, GP, Rid, [{B, _} | _] = Vals , FF) when is_boolean(B) ->
    ct:pal("#### co_send_rop_data 1 ~p~n", [Vals]),
    Fun = fun({OverrideFF, V}) -> ?PROXY:pmi2DataRop(Handle, GP, Rid, V, OverrideFF);
	     (V)               -> ?PROXY:pmi2DataRop(Handle, GP, Rid, V, FF) 
	  end,
    lists:foreach(Fun, Vals);
co_send_rop_data(0, Handle, GP, Rid, Vals, FF) ->
    ct:pal("#### co_send_rop_data 2 ~p~n", [Vals]),
    ?PROXY:pmi2DataRop(Handle, GP, Rid, Vals, FF);
co_send_rop_data(Delay, Handle, GP, Rid, [{B, _} | _] = Vals, FF) when is_boolean(B) ->
    ct:pal("#### co_send_rop_data 3 ~p~n", [Vals]),
    Fun = fun({OverrideFF, V}) -> 
		  Args = [Handle, GP, Rid, V, OverrideFF],
		  timer:apply_after(Delay, ?PROXY, pmi2DataRop, Args);
	     (V) -> 
		  Args = [Handle, GP, Rid, V, FF],
		  timer:apply_after(Delay, ?PROXY, pmi2DataRop, Args) 
	  end,
    lists:foreach(Fun, Vals);
co_send_rop_data(Delay, Handle, GP, Rid, Vals, FF) ->
    ct:pal("#### co_send_rop_data 4 ~p~n", [Vals]),
    Args = [Handle, GP, Rid, Vals, FF],
    timer:apply_after(Delay, ?PROXY, pmi2DataRop, Args).


%%===========================================
%% default rop reply
%%===========================================
get_default_report_rop_reply(LDNs, Spec) ->
    gdrrr_spec(Spec, LDNs, []).


gdrrr_spec([], _, Acc) ->
    lists:reverse(Acc);
gdrrr_spec([{Gid, MTs} | T], LDNs, Acc) ->
    LDNVals = [{LDN, gdrrr_vals(MTs, Gid)} || LDN <- LDNs],
    gdrrr_spec(T, LDNs, [{Gid, LDNVals} | Acc]).
    

gdrrr_vals(MTs, Gid) ->
    [{MT, [Gid * 10 + MT]} || MT <- MTs].



%%===========================================
%% send sc data
%%===========================================
co_send_sc_data(0, Handle, Rid, Result, ErrorStr, [V | _] = Vals) 
  when is_tuple(V) ->
    ?PROXY:pmi2DataShowCounters(Handle, Rid, Result, ErrorStr, Vals);
%% send fragmented reply
co_send_sc_data(0, Handle, Rid, Result, ErrorStr, Vals) ->
    Fun = fun(V) -> ?PROXY:pmi2DataShowCounters(Handle, 
						Rid, 
						Result, 
						ErrorStr, 
						V)
	  end,
    lists:foreach(Fun, Vals);
co_send_sc_data(Delay, Handle, Rid, Result, ErrorStr, [V | _] = Vals) 
  when is_tuple(V) ->
    Args = [Handle, Rid, Result, ErrorStr, Vals],
    timer:apply_after(Delay, ?PROXY, pmi2DataShowCounters, Args);
co_send_sc_data(Delay, Handle, Rid, Result, ErrorStr, Vals) ->
    Fun = fun(V) -> 
		  Args = [Handle, Rid, Result, ErrorStr, V],
		  timer:apply_after(Delay, ?PROXY, pmi2DataShowCounters, Args) 
	  end,
    lists:foreach(Fun, Vals).





%%========================================================================
%% p(Str, Args) -> ok
%% 
%% Print function.
%%========================================================================
p(Str, Args) ->
    case ct:get_status() of
	List when is_list(List) -> pms_test_lib:ct_pal(Str, Args);
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


choose(true,  T, _) -> T;
choose(false, _, F) -> F.

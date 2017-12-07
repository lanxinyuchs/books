%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmi_erl_app.erl %
%%% @version /main/R3A/R5A/R6A/1

-module(pmi_erl_app).

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2016-02-19 eolaand     Updated
%%% ----------------------------------------------------------
%%% 
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


-export([pmiInitialize/2]).
-export([pmiInitialize_2/3]).
-export([pmiFinalize/2]).
-export([pmiDataRop/5,
	 pmiDataRop/6]).

-export([expected/3]).

-export([subscribe_timeout/1]).
-export([report_rop_timeout/1]).


-include("pms2_test.hrl").


-define(PROXY, pms_pmi_proxy).
-define(PMI, pmsPmI).
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
	       pmi_mod,
	       gp, 
	       timer_ref, 
	       meas_type,
	       subscr_spec = [],
	       subscriber,
	       ldn   = [],
	       cvals = []}).

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
    p("=== pmi_erl_app === ~p = start(~p, ~p)~n", 
      [Res, Name, Options]),
    case Res of
	{ok, Pid} = Res->
	    store_app({Pid, Name}),
	    Res;
	Error ->
	    Error
    end.


stop(Name) ->
    p("=== pmi_erl_app === stop(~p)~n", [Name]),
    remove_app(Name), 
    Name ! {stop, self()},
    receive 
	stopped ->
	    ok
    after 1000 ->
	    Pid = whereis(Name),  
	    p("=== pmi_erl_app === Force ~p exit(~p, kill)~n", [Name, Pid]),
	    Pid =:= undefined orelse exit(Pid, kill),
	    ok
    end.

kill(Name, Module) ->
    Name ! {kill, Module}.




proc(undefined, Name, Options) ->
    p("=== pmi_erl_app === ~p = proc(undefined, ~p)~n",
      [Name, Options]),
    Self = self(),
    Pid = spawn(?MODULE, init, [Self, Name, Options]),
    %% To avoid race condition, send back Pid
    receive 
	inited -> {ok, Pid}
    after 5000 -> {error, {timeout, test_app}}
    end;
proc(Pid, Name, Options) ->
    p(lightred, "=== pmi_erl_app === kill old process ~p~n", [Pid]),
    stop(Name),
    proc(undefined, Name, Options).





pmiInitialize(Name, Groups) ->
    Name ! {initialize, self(), Groups},
    receive
	{initialize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.

pmiInitialize_2(Name, Groups, TopLdn) ->
    Name ! {initialize_2, self(), Groups, TopLdn},
    receive
	{initialize_2_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.


pmiFinalize(Name, Handle) ->
    Name ! {finalize, self(), Handle},
    receive
	{finalize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.



pmiDataRop(Name, Handle, GP, ReportId, Values) ->
    ct:pal("#### pms_erl_app  pmiDataRop ~p ~n", [Name]),
    Name ! {pmiDataRop, self(), Handle, GP, ReportId, Values},
    receive
	{pmiDataRop_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.



pmiDataRop(Name, Handle, GP, ReportId, MOLDN, Values) ->
    ct:pal("#### pms_erl_app  pmiDataRop ~p ~n", [Name]),
    Name ! {pmiDataRop, self(), Handle, GP, ReportId, MOLDN, Values},
    receive
	{pmiDataRop_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.



expected(Name, Handle, Expected) ->
    Ref = make_ref(),
    Name ! {expected, self(), Ref, Handle, Expected},
    {ok, Ref}.

subscribe_timeout(Name) ->
    Name ! subscribe_timeout.

report_rop_timeout(Name) ->
    Name ! report_rop_timeout.

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
    StopApps = proplists:get_value(stop_apps, 
				   Opts, 
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
	    p("~p: Started pmi_erl_app sup ~p", [?MODULE, Pid]), 
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
    p("=== APP(~p) ===  init pid: ~p   TS pid: ~p~n"
      "Options ~p~n", [Name, self(), TsPid, Options]),
    register(Name, self()),
    TsPid ! inited,

    Rpc       = proplists:get_value(rpc, Options, false),
    ApiServer = proplists:get_value(api_server,   Options, undefined),
    ApiModule = proplists:get_value(api_module,   Options, ?PROXY),
    CbModule  = proplists:get_value(cb_module,    Options, ApiModule),
    Expected  = proplists:get_value(expected,     Options, []),
    ExpRef    = proplists:get_value(expected_ref, Options, []),
    [LDN]     = ?LDN_DEF_1(Name),
    
    loop(#loop{name         = Name,
	       ldn          = LDN,
	       rpc          = Rpc,
	       api_server   = ApiServer,
	       api_module   = ApiModule,
	       cb_module    = CbModule,
	       ts_pid       = TsPid,
	       expected     = Expected,
	       expected_ref = ExpRef}).




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
	   expected_ref = ExpRef,
	   pmi_mod      = _PmiMod,
	   gp           = GP,
	   subscr_spec  = SubscrSpec,
	   ldn          = LDN,
	   cvals        = _CVals} = Loop) ->
    %%p("######### PMI_ERL_APP LOOP ~p~n~p~n#########", [Name, Loop]),

    receive 
	%%===================================================
	%% messages from PMS
	%%===================================================
	{pmiSubscribe, {NewGP, NewSubscrSpec}} = Msg ->
	    p("=== PMS ---> APP(~p) === ~p ~p  (pmi_erl_app)~nSubscrSpec: ~p~n",
	      [Name, pmiSubscribe, self(), NewSubscrSpec]),
	    NewExpected = handle_callback(Name,
					  Msg, 
					  Handle, 
					  TsPid,
					  NewGP,
					  NewSubscrSpec,
					  LDN,
					  Expected,
					  ExpRef),
	    loop(Loop#loop{expected    = NewExpected,
			   gp          = NewGP,
			   subscr_spec = NewSubscrSpec});
	
	{pmiReport, RopRequest} = Msg ->
	    p("=== PMS ---> APP(~p) === ~p ~p  (pmi_erl_app)~nRequest: ~p~n",
	      [Name, pmiReport, self(), RopRequest]),
	    NewExpected = handle_callback(Name,
					  Msg, 
					  Handle, 
					  TsPid, 
					  GP,
					  SubscrSpec,
					  %% [?LDN_DEF_1(Name)],
					  ?LDN_DEF_1(Name),
					  Expected,
					  ExpRef),
	    loop(Loop#loop{expected = NewExpected});
	
	{pmiReportShowCounters, ScRequest} = Msg ->
	    p("=== PMS ---> APP(~p) === ~p ~p  (pmi_erl_app) ~nRequest: ~p~n",
	      [Name, pmiReportShowCounters, self(), ScRequest]),
	    NewExpected = handle_callback(Name,
					  Msg, 
					  Handle, 
					  TsPid, 
					  GP,
					  SubscrSpec,
					  LDN,
					  Expected,
					  ExpRef),
	    loop(Loop#loop{expected = NewExpected});
	
	subscribe_timeout ->
	    io:format(user, "##### subscribe_timeout ~p  (pmi_erl_app)~n", [Expected]),
	    NewExpected = handle_subscribe_timeout(Name, 
						   TsPid, 
						   GP,
						   Expected, 
						   ExpRef),
	    loop(Loop#loop{expected = NewExpected});

	report_rop_timeout ->
	    io:format(user, "##### report_rop_timeout ~p  (pmi_erl_app)~n", [Expected]),
	    NewExpected = handle_report_rop_timeout(Name, 
						    TsPid, 
						    GP,
						    Expected, 
						    ExpRef),
	    loop(Loop#loop{expected = NewExpected});

	%%===================================================
	%% orders from the test suite
	%%===================================================
	{initialize, From, Groups} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p  (pmi_erl_app)~n"
	      "Groups:  ~p~n",
	      [Name, initialize, self(), Groups]),
	    Res = handle_initialize(Name, 
				    Rpc,
				    ApiServer, 
				    ApiModule, 
				    CbModule, 
				    Groups),
	    From ! {initialize_result, Res},
	    case Res of
		{ok, NewHandle} ->
		    MRef = erlang:monitor(process, NewHandle),
		    loop(Loop#loop{handle      = NewHandle,
				   monitor_ref = MRef});
		_ ->
		    loop(Loop)
	    end;

	{initialize_2, From, Groups, TopLdn} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p  (pmi_erl_app)~n"
	      "Groups:  ~p~n"
	      "TopLdn:  ~p~n",
	      [Name, initialize_2, self(), Groups, TopLdn]),
	    Res = handle_initialize_2(Name, 
				      Rpc,
				      ApiServer, 
				      ApiModule, 
				      CbModule, 
				      Groups,
				      TopLdn),
	    From ! {initialize_2_result, Res},
	    case Res of
		{ok, NewHandle} ->
		    MRef = erlang:monitor(process, NewHandle),
		    loop(Loop#loop{handle      = NewHandle,
				   monitor_ref = MRef});
		_ ->
		    loop(Loop)
	    end;

	{finalize, From, Handle} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p  (pmi_erl_app)~n",
	      [Name, finalize, self()]),
	    Res = handle_finalize(Name, Rpc, ApiModule, Handle),
	    From ! {finalize_result, Res},
	    unmonitor(Loop#loop.monitor_ref),
	    loop(Loop#loop{monitor_ref = undefined});
	
	
	{pmiDataRop, From, Handle, ReqGP, RId, Vals} ->
	    UseGP    = choose(ReqGP == gp, GP, ReqGP),
	    DefVals  = get_default_report_rop_reply(SubscrSpec, []),
	    Res = handle_data_rop(Name, Handle, CbModule, UseGP, RId, LDN, 
				  Vals, DefVals),
	    From ! {pmiDataRop_result, Res},
	    loop(Loop);

	{pmiDataRop, From, Handle, ReqGP, RId, MOLDN, Vals} ->
	    UseGP    = choose(ReqGP == gp, GP, ReqGP),
	    DefVals  = get_default_report_rop_reply(SubscrSpec, []),
	    Res = handle_data_rop(Name, Handle, CbModule, UseGP, RId, MOLDN, 
				  Vals, DefVals),
	    From ! {pmiDataRop_result, Res},
	    loop(Loop);

	{expected, From, NewExpRef, Handle, NewExpected} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p   (pmi_erl_app)~n  Expected: ~p~n",
	      [Name, expected, self(), NewExpected]),
	    check_subscribe_request(NewExpected, Name),
	    loop(Loop#loop{handle       = Handle,
			   ts_pid       = From,
			   expected     = NewExpected,
			   expected_ref = NewExpRef});

	{stop, UserPid} ->
	    p("=== APP(~p) === ~p STOPPED   (pmi_erl_app)~n", [Name, self()]),
	    unregister(Name),
	    UserPid ! stopped,
	    ok;

	{'DOWN', MonitorRef, _Type, Object, _Info} 
	  when Loop#loop.monitor_ref =:= MonitorRef ->
	    %% FIXME: This should be handled
	    p("=== APP(~p) === Got unexpected DOWN for Handle ~p  (pmi_erl_app)~n", 
	      [Name, Object]),
	    loop(Loop);

	{'DOWN', _MonitorRef, _Type, Object, _Info} ->
	    p("=== APP(~p) === Got DOWN for unknown Handle ~p  (pmi_erl_app)~n", 
	      [Name, Object]),
	    loop(Loop);

	X ->
	    p("=== APP(~p) === ~p UNKNOWN MESSAGE ~p   (pmi_erl_app)~n~p~n", 
	      [Name, self(), X, Loop]),
	    loop(Loop)
    end.





check_subscribe_request([{no_subscribe_request, _, _} | _] = Expected, 
			Name) ->
    timer:apply_after(5000, ?MODULE, subscribe_timeout, [Name]),
    Expected;
check_subscribe_request(Expected, _) ->
    Expected.


%%========================================================================
%% handle_initialize(Name, Rpc, ApiServer, ApiModule, 
%%                   CbModule, CbFlags, CounterMap)
%% 
%% 
%%========================================================================
handle_initialize(Name, true, _, ApiModule, CbModule, Groups) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p  (pmi_erl_app)~n"
      "Groups:  ~p~n",
      [Name, CbModule, initialize2, self(), Groups]),
    Args = [Groups, CbModule, self()],
    ?PROXY:cs_node_call(ApiModule, pmiInitialize, Args);
handle_initialize(Name,
		  _, 
		  undefined, 
		  ApiModule, 
		  CbModule, 
		  Groups) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p  (pmi_erl_app)~n"
      "Groups:  ~p~n",
      [Name, CbModule, initialize2, self(), Groups]),
    ApiModule:pmiInitialize(Groups, CbModule, self());
handle_initialize(Name,
		  _, 
		  ApiServer, 
		  ApiModule, 
		  CbModule, 
		  Groups) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p  (pmi_erl_app)~n"
      "Groups:  ~p~n",
      [Name, CbModule, initialize2, self(), Groups]),
    ApiModule:pmiInitialize(ApiServer, Groups, CbModule, self()).


%%========================================================================
%% handle_initialize_2(Name, Rpc, ApiServer, ApiModule, 
%%                     CbModule, CbFlags, CounterMap)
%% 
%% 
%%========================================================================
handle_initialize_2(Name, true, _, ApiModule, CbModule, Groups, TopLdn) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p 1  (pmi_erl_app)~n"
      "Groups:  ~p~n"
      "TopLdn:  ~p~n",
      [Name, CbModule, initialize_2, self(), Groups, TopLdn]),
    Args = [[{TopLdn, Groups}], CbModule, self()],
    ?PROXY:cs_node_call(ApiModule, pmiInitialize, Args);
handle_initialize_2(Name,
		    _, 
		    undefined, 
		    ApiModule, 
		    CbModule, 
		    Groups, 
		    TopLdn) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p 2  (pmi_erl_app)~n"
      "Groups:  ~p~n"
      "TopLdn:  ~p~n",
      [Name, CbModule, initialize_2, self(), Groups, TopLdn]),
    ApiModule:pmiInitialize([{TopLdn, Groups}], CbModule, self());
handle_initialize_2(Name,
		    _, 
		    ApiServer, 
		    ApiModule, 
		    CbModule, 
		    Groups, 
		    TopLdn) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p 3  (pmi_erl_app)~n"
      "Groups:  ~p~n"
      "TopLdn:  ~p~n",
      [Name, CbModule, initialize_2, self(), Groups, TopLdn]),
    ApiModule:pmiInitialize(ApiServer, [{TopLdn, Groups}], CbModule, self()).



%%========================================================================
%% handle_data_rop(Name, Rpc, ApiServer, ApiModule, 
%%                   CbModule, CbFlags, CounterMap)
%% 
%% 
%%========================================================================
handle_data_rop(Name, Handle, CbModule, GP, ReportId, LDN, default, DefVals) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p   default~n"
      "GP:       ~p~n"
      "ReportId: ~p~n"
      "LDN:      ~p~n"
      "Data:     ~p~n",
      [Name, CbModule, pmiDataRop, self(), GP, ReportId, LDN, DefVals]),
    co_send_rop_data(0, Handle, GP, ReportId, LDN, DefVals);
handle_data_rop(Name, Handle, CbModule, GP, ReportId, LDN, default_ff, DefVals) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p   default_ff~n"
      "GP:       ~p~n"
      "ReportId: ~p~n"
      "LDN:      ~p~n"
      "Data:     ~p~n",
      [Name, CbModule, pmiDataRop, self(), GP, ReportId, LDN, DefVals]),
    co_send_rop_data(0, Handle, GP, ReportId, LDN, DefVals),
    co_send_rop_data(0, Handle, GP, ReportId, LDN, []);
handle_data_rop(Name, Handle, CbModule, GP, ReportId, LDN, Vals, _DefVals) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "GP:       ~p~n"
      "ReportId: ~p~n"
      "LDN:      ~p~n"
      "Data:     ~p~n",
      [Name, CbModule, pmiDataRop, self(), GP, ReportId, LDN, Vals]),
    co_send_rop_data(0, Handle, GP, ReportId, LDN, Vals).



%%========================================================================
%% finalize
%% 
%% 
%%========================================================================
handle_finalize(Name, true, _ApiModule, _Handle) ->
    p("=== APP(~p) <--- SUITE === ~p ~p  (pmi_erl_app)~n",
      [Name, finalize, self()]),
    ?PROXY:cs_node_call(?PMI, pmiFinalize, []);
handle_finalize(Name, _, ApiModule, Handle) ->
    p("=== APP(~p) <--- SUITE === ~p ~p  (pmi_erl_app) ~n",
      [Name, finalize, self()]),
    ApiModule:pmiFinalize(Handle).


%%========================================================================
%% handle_callback() -> NewExpected || {error, {Expected, ReceivedMsg}}
%% 
%% 
%%========================================================================
%% handle_callback(Name, 
%% 		Msg, 
%% 		_Handle, 
%% 		_TsPid, 
%% 		_GP, 
%% 		_SubscrSpec, 
%% 		_LDN, 
%% 		[] = Expected,
%% 	        _ExpRef) ->
%%     ct:pal("~p #### Got message ~p~nwhen not expecting anything.~n", 
%%            [Name, Msg]),
%%     Expected;
handle_callback(Name, 
		Msg, 
		_Handle, 
		TsPid, 
		_GP, 
		_SubscrSpec, 
		_LDN, 
		[relay] = Expected,
	        _ExpRef) ->
    TsPid ! {copy, Name, Msg},
    Expected;
handle_callback(Name, 
		Msg, 
		_Handle, 
		TsPid, 
		_GP, 
		_SubscrSpec, 
		_LDN, 
		[{send_copy, _, _} | _] = Expected,
	        _ExpRef) ->
    TsPid ! {copy, Name, Msg},
    Expected;
handle_callback(Name, 
		Msg, 
		_Handle, 
		TsPid, 
		_GP, 
		_SubscrSpec, 
		_LDN, 
		[{no_subscribe_request, _, _} | _] = Expected,
	        ExpRef) ->
    Error = {{exp, Expected}, {rcv, Msg}},
    TsPid ! {expected_result, ExpRef, Name, Error},
    [];
handle_callback(_Name, 
		_Msg, 
		_Handle, 
		_TsPid, 
		_GP, 
		_SubscrSpec, 
		_LDN, 
		[{no_rop_report_request, _, _} | _] = Expected,
	        _ExpRef) ->
    Expected;
handle_callback(Name, 
		Msg, 
		Handle, 
		TsPid, 
		GP, 
		SubscrSpec, 
		LDN, 
		Expected, 
		ExpRef) ->
    io:format(user, "#####  ~p (pmi_erl_app) Expected ~p ~n", [Name, Expected]),
    HcRes = hc(Msg, Name, Handle, GP, SubscrSpec, LDN, Expected),
    hc_rc(HcRes, Name, ExpRef, TsPid, GP).


hc_rc({ok, []}, Name, ExpRef, TsPid, _GP) ->
    io:format(user, "#####  ~p (pmi_erl_app) expected sending result ~p to ~p~n", 
	      [Name, {expected_result, ExpRef, Name, ok}, TsPid]),
    TsPid ! {expected_result, ExpRef, Name, ok},
    [];
hc_rc({ok, [{no_rop_report_request, _, _} | _] = Exp}, Name, ExpRef, _, GP) ->
    io:format(user, "#####  ~p (pmi_erl_app) expected after ~p~n", 
	      [Name, {expected_result, ExpRef, Name, ok}]),
    timer:apply_after(GP * 1000, ?MODULE, report_rop_timeout, [Name]),
    Exp;
hc_rc({ok, NewExpected}, Name, _ExpRef, _, _) ->
    io:format(user, "#####  ~p (pmi_erl_app) expected remaining ~p~n", 
	      [Name, NewExpected]),
    NewExpected;
hc_rc(Error, Name, ExpRef, TsPid, _) ->
    io:format(user, "#####  ~p (pmi_erl_app) expected ERROR ~p~n", 
	      [Name, {expected_result, ExpRef, Name, Error}]),
    TsPid ! {expected_result, ExpRef, Name, Error},
    [].


	    


hc({pmiSubscribe, Data}, _, _, _, _SubscrSpec, _LDN, Expected) ->
    check_options_subscribe(Data, Expected);
hc({pmiReport, Data}, _, Handle, GP, SubscrSpec, LDN, Expected) ->
    check_options_report_rop(Data, Handle, GP, SubscrSpec, LDN, Expected);
hc({pmiReportShowCounters, Data}, Name, Handle, _, _, _LDN, Expected) ->
    check_options_report_sc(Data, Name, Handle, Expected).


%%---------------------------------------------
%% subscribe
%%---------------------------------------------
%%.............
%% not expected
%%.............
check_options_subscribe(_, 
			[{no_subscribe_request, _, _}]) ->
    {ok, []};
%%.............
%% expected
%%.............
check_options_subscribe({RcvGP, RcvSpec}, 
			[{pmiSubscribe, _, Options} | T]) ->
    ExpGP   = proplists:get_value(gp,   Options, RcvGP),
    ExpSpec = proplists:get_value(spec, Options, RcvSpec),
    case {ExpGP == RcvGP, cos_sort(ExpSpec) == cos_sort(RcvSpec)} of
	{true,  true}  -> {ok, T};
	{true,  false} -> {error, {{exp, ExpSpec}, {rcv, RcvSpec}}};
	{false, _}     -> {error, {{exp, ExpGP}, {rcv, RcvGP}}}
    end;
check_options_subscribe(Received, [_ | T]) ->
    check_options_subscribe(Received, T);
check_options_subscribe(Received, []) ->
    {error, {no_subscribe_in_expected, Received}}.



cos_sort(Spec) ->
    cos_sort(lists:sort(Spec), []).

cos_sort([], Acc) ->
    Acc;
cos_sort([{Gid, MTs} | T], Acc) ->
    cos_sort(T, [{Gid, lists:sort(MTs)} | Acc]).
    

%%---------------------------------------------
%% report rop
%%---------------------------------------------
%%.............
%% no report
%%.............
check_options_report_rop({_ReqGP, _ReqReportId, _ReqMaxTime}, 
			 _Handle, 
			 _ReqGP, 
			 _SubscrSpec,
			 _LDN, 
			 [{no_rop_report_request, {repeat, _}, _} | _] = Exp) ->
    {ok, Exp};
%%.............
%% ignore case
%%.............
check_options_report_rop({_ReqGP, _ReqReportId, _ReqMaxTime}, 
			 _Handle, 
			 _ReqGP, 
			 _SubscrSpec,
			 _LDN, 
			 [{pmiReport, {ignore, Repeat}, Opts} | T] = Exp) ->

    case Repeat of
	wait_until_subscribe ->
	    {ok, Exp};
	_ when Repeat > 1 ->
	    {ok, [{pmiReport, {ignore, Repeat - 1}, Opts} | T]};
	_ ->
	    {ok, T}
    end;
%%.............
%% repeat case
%%.............
check_options_report_rop({ReqGP, ReqReportId, _ReqMaxTime}, 
			 Handle, 
			 ReqGP, 
			 SubscrSpec,
			 [LDN], 
			 [{pmiReport, {repeat, Repeat}, Opts} | T] = Exp) ->

    DefValue = get_default_report_rop_reply(SubscrSpec, []),

    GP   = proplists:get_value(gp,        Opts, ReqGP),
    Rid  = proplists:get_value(report_id, Opts, ReqReportId),
    Vals = proplists:get_value(values,    Opts, DefValue),

    ok = co_send_rop_data(proplists:get_value(delay, Opts, 0),
			  Handle, 
			  GP,
			  Rid,
			  LDN,
			  Vals),
    
    case Repeat of
	wait_until_subscribe ->
	    {ok, Exp};
	_ when Repeat > 1 ->
	    {ok, [{pmiReport, {repeat, Repeat - 1}, Opts} | T]};
	_ ->
	    {ok, T}
    end;
%%.............
%% unknown case
%%.............
check_options_report_rop(Received, 
			 _Handle, 
			 _GP,
			 _SubscrSpec, 
			 _LDN,
			 Expected) ->
    {error, {Expected, Received}}.



%%===========================================
%% send rop data
%%===========================================
co_send_rop_data(0, _Handle, _GP, _Rid, _LDN, [[]]) ->
    ok;
%% simulates FF
co_send_rop_data(0, Handle, GP, Rid, LDN, [] = Vals) ->
    ?PROXY:pmiData(Handle, GP, Rid, LDN, Vals);
co_send_rop_data(0, Handle, GP, Rid, LDN, [V | _] = Vals) when is_tuple(V) ->
    ?PROXY:pmiData(Handle, GP, Rid, LDN, Vals);
%% send fragmented reply
co_send_rop_data(0, Handle, GP, Rid, LDN, [V | VT]) ->
    ?PROXY:pmiData(Handle, GP, Rid, LDN, V),
    co_send_rop_data(0, Handle, GP, Rid, LDN, VT);
co_send_rop_data(Delay, Handle, GP, Rid, LDN, [V | _] = Vals) when is_tuple(V) ->
    Args = [Handle, GP, Rid, LDN, Vals],
    timer:apply_after(Delay, ?PROXY, pmiData, Args),
    ok;
co_send_rop_data(Delay, Handle, GP, Rid, LDN, Vals) ->
    Fun = fun(V) -> 
		  Args = [Handle, GP, Rid, LDN, V],
		  timer:apply_after(Delay, ?PROXY, pmiData, Args) 
	  end,
    lists:foreach(Fun, Vals),
    ok.
 


%%===========================================
%% default rop reply
%%===========================================
get_default_report_rop_reply([], Acc) ->
    lists:reverse(Acc);
get_default_report_rop_reply([{Gid, MTs} | T], Acc) ->
    Vals = [{MT, [1234]} || MT <- MTs],
get_default_report_rop_reply(T, [{Gid, Vals} | Acc]).
    


%%---------------------------------------------
%% report show counters
%%---------------------------------------------
check_options_report_sc(_Data, _, _, [{pmiReportShowCounters, _, []} | T]) ->
    {ok, T};
check_options_report_sc({ReqReportId, _LDN, _Timeout},
			Name,
			Handle,
			[{pmiReportShowCounters, {repeat, Repeat}, Opts} | T
			 ] = Exp) ->
%%    DefValue = get_default_report_rop_reply(LDN, ScSpec),
    DefValue = [],
    Rid      = proplists:get_value(report_id,  Opts, ReqReportId),
    Result   = proplists:get_value(result,     Opts, 0),
    ErrorStr = proplists:get_value(horror_str, Opts, ""), %% Kalle error
    Vals     = proplists:get_value(values,     Opts, DefValue),
    Delay    = proplists:get_value(delay,      Opts, 0),
    p("=== PMS <--- APP(~p) === ~p pmiDataShowCounters  (pmi_erl_app)~n"
      "~p:pmiDataShowCounters(~p, ~p, ~p, ~p).  Delay = ~p~n",
      [Name, self(), pmsPmI, Handle, ReqReportId, ErrorStr, Vals, Delay]),

    co_send_sc_data(Delay, Handle, Rid, Result, ErrorStr, Vals),
    
    case Repeat of
	wait_until_subscribe ->
	    {ok, Exp};
	_ when Repeat > 1 ->
	    {ok, [{pmiReportShowCounters, {repeat, Repeat - 1}, Opts} | T]};
	_ ->
	    {ok, T}
    end;
check_options_report_sc(_Data, _, _Handle, Expected) ->
    {ok, Expected}.


%%===========================================
%% send sc data
%%===========================================
co_send_sc_data(0, Handle, Rid, Result, ErrorStr, [V | _] = Vals) 
  when is_tuple(V) ->
    ?PROXY:pmiDataShowCounters(Handle, Rid, Result, ErrorStr, Vals);
%% send fragmented reply
co_send_sc_data(0, Handle, Rid, Result, ErrorStr, Vals) ->
    Fun = fun(V) -> ?PROXY:pmiDataShowCounters(Handle, 
						Rid, 
						Result, 
						ErrorStr, 
						V)
	  end,
    lists:foreach(Fun, Vals);
co_send_sc_data(Delay, Handle, Rid, Result, ErrorStr, [V | _] = Vals) 
  when is_tuple(V) ->
    Args = [Handle, Rid, Result, ErrorStr, Vals],
    timer:apply_after(Delay, ?PROXY, pmiDataShowCounters, Args);
co_send_sc_data(Delay, Handle, Rid, Result, ErrorStr, Vals) ->
    Fun = fun(V) -> 
		  Args = [Handle, Rid, Result, ErrorStr, V],
		  timer:apply_after(Delay, ?PROXY, pmiDataShowCounters, Args) 
	  end,
    lists:foreach(Fun, Vals).


%%========================================================================
%% handle_subscribe_timeout
%% 
%% 
%%========================================================================
%% inform the test suite that no subscribe request was received
handle_subscribe_timeout(Name, 
			  TsPid, 
			  _GP,
			  [{no_subscribe_request, _, _}],
			  ExpRef) ->
    TsPid ! {expected_result, ExpRef, Name, ok},
    [];
%% Something is wrong
handle_subscribe_timeout(Name, TsPid, _GP, Expected, ExpRef) ->
    TsPid ! {expected_result, 
	     ExpRef, 
	     Name, 
	     {error, {subscribe_timeout, Expected}}},
    [].




%%========================================================================
%% handle_report_rop_timeout
%% 
%% 
%%========================================================================
%% inform the test suite that no rop report request was received
handle_report_rop_timeout(Name, 
			  TsPid, 
			  _GP,
			  [{no_rop_report_request, {repeat, 1}, _}],
			  ExpRef) ->
    TsPid ! {expected_result, ExpRef, Name, ok},
    [];
%% wait for another GP period
handle_report_rop_timeout(Name, 
			  _TsPid,
			  GP,
			  [{no_rop_report_request, {repeat, N}, _} | T],
			  _ExpRef) ->
    timer:apply_after(GP * 1000, ?MODULE, report_rop_timeout, [Name]),
    [{report_rop_timeout, {repeat, N - 1}} | T];
%% Something is wrong
handle_report_rop_timeout(Name, TsPid, _GP, Expected, ExpRef) ->
    TsPid ! {expected_result, 
	     ExpRef, 
	     Name, 
	     {error, {report_rop_timeout, Expected}}},
    [].




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

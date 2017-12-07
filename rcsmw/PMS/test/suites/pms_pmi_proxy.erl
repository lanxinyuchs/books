%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_pmi_proxy.erl %
%%% @author eolaand
%%% @version /main/R3A/R4A/R5A/R11A/2
%%%
%%% @doc == PMS PMI Proxy CT Hook ==
%%% This module provides an implementation of the PM interface that uses
%%% the IFT application as a bridge towards the CS PMS.
%%% It is also possible to configure it to use the PMS erlang API via RPC.
%%%
%%% The default target is a single CS node but the several instances may be 
%%% configured in order to handle multi DU scenarios.
%%% The module is implemented as a ct_hook in order to facilitate its use in
%%% Common Test suites. The ct_hook determines what target the proxy
%%% should connect to and sets up IP address and port number accordingly.
%%%
%%% The API functions in this module corresponds to functions in the
%%% PMS modules pmsPmI2 and pmsPmI. See PMS documentation for a detailed 
%%% description of these functions.
%%%
%%% The `pms_pmi_proxy' hook is specified in the `suite/0' function of the 
%%% test suite as described below:
%%%
%%% ```suite() -> 
%%%        [{ct_hooks, [{pms_pmi_proxy, Opts}]}].
%%%
%%%    Opts = [Opt]
%%%    Opt = {erl_api, UseErlAPI} | 
%%%          {du_no, No} | 
%%%          {sim_sname, SName} | 
%%%          {instance_name, InstanceName} | 
%%%          {auto_finalize, AutoFinalize} | 
%%%          {restart_ift_on_fail, RstIFTOnFail} | 
%%%          {restart_ift, RstIFT} | 
%%%          {trace_ift, TraceIFT} | 
%%%          {trace_child, TraceIFTChild}'''
%%%
%%%  `UseErlAPI = boolean()' <br/>
%%%    - Indicates if the PMS Erlang API should be used or not.<br/>
%%%      Default is true
%%%
%%%  `No = integer()' <br/>
%%%    - Used to indicate testnode (DU) number when testing towards a specific 
%%%      node in a cluster in simulated or target environment.<br/>
%%%      Default is 1
%%%
%%%  `SName = atom()' <br/>
%%%    - Used to provide a different sim node name than the default 
%%%      when running tests towards a simulator.<br/>
%%%      Default is `$USER' <br/>
%%%
%%%  `InstanceName = atom()' <br/>
%%%    - Used to provide an identity of the CTH instance. Also used as the 
%%%      registered name of the server handling the connection towards the test 
%%%      node. The naming is useful when running multinode tests. 
%%%      See example below.<br/>
%%%      Default is `pms_pmi_proxy' <br/>
%%%
%%%  `AutoFinalize = on_end_tc | on_end_suite | on_fail' <br/>
%%%    - Used to get an automatic finalize of all open sessions in the 
%%%      selected situation.<br/>
%%%      Default is `true' <br/>
%%%
%%%  `RstIFTOnFail = boolean()' <br/>
%%%    - Indicates if the IFT app should be restarted at each failed test 
%%%      case.<br/>
%%%      Default is `true' <br/>
%%%
%%%  `RstIFT = boolean()' <br/>
%%%    - Used to allow or prevent restart of the IFT app.<br/>
%%%      Overrides the `restart_ift_on_fail' parameter.<br/>
%%%      Default is `true' <br/>
%%%
%%%  `TraceIFT = boolean()' <br/>
%%%    - Start LTTNg and TRI trace on IFT master.<br/>
%%%      Default is `true' <br/>
%%%
%%%  `TraceIFTChild = boolean()' <br/>
%%%    - Start LTTNg and TRI trace on IFT child.<br/>
%%%      Default is `true' <br/>
%%%
%%% All parameters described above are optional.
%%%
%%% In a multinode scenario several instances may be specified using different
%%% instance names as in this example.
%%%
%%% ```suite() -> 
%%%     [{ct_hooks, [{pms_pmi_proxy, [{instance_name, pms_proxy_1},
%%%                                   {du_no, 1}]},
%%%                  {pms_pmi_proxy, [{instance_name, pms_proxy_2}
%%%                                   {du_no, 2}]}].'''
%%%
%%% @end
%%% ----------------------------------------------------------
-module(pms_pmi_proxy).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/R11A/2').
-date('2017-10-20').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R3A/1    2014-08-28 eolaand     Created
%%% R11A/1   2017-10-19 etxkols     Support rcs-sim in cloud env
%%% ----------------------------------------------------------

%% PMI functions
-export([
	 pmiInitialize/1,
	 pmiInitialize/2,
	 pmiInitialize/3,
	 pmiInitialize/4,
	 pmiData/5,
	 pmiDataShowCounters/5,
	 pmiFinalize/1]).


%% Default PMI Callbacks
-export([pmiSubscribeCallback/3,
	 pmiReportCallback/4,
	 pmiReportShowCountersCallback/4
	]).


%% PMI2 functions
%% API PMI
-export([pmi2Initialize/5,
	 pmi2Initialize/4,
	 pmi2Initialize/3,
	 pmi2Initialize/2,
	 pmi2Initialize/1,
	 pmi2Initialize/0,
	 pmi2CounterMap/2,
	 pmi2Finalize/1,
	 pmi2DataShowCounters/5,
	 pmi2DataRop/5]).


%% Default PMI2 Callbacks
-export([pmi2SubscribeRopCallback/3,
	 pmi2ReportRopCallback/4,
	 pmi2ReportShowCountersCallback/5]).



%% Non PMI functions in support of test.
-export([
	 emulAppDeath/2
	]).

-export([get_cs_node/0,
	 get_cs_node/1,
	 cs_node_call/3,
	 cs_node_call/4,
	 cs_node_call/5]).

-export([ping_ift_app/0,
	 ping_ift_app/1,
	 restart_ift_app/0,
	 restart_ift_app/1,
	 exit_ift_app/0,
	 exit_ift_app/1,
	 start_trace/2]).

-export([
	 subscribe_testnode/0,
	 subscribe_testnode/1,
	 unsubscribe_testnode/0,
	 unsubscribe_testnode/1
	]).

-export([
	 get_children/0,
	 get_children/1,
	 get_remote_node/0,
	 get_remote_node/1,
	 get_node_status/0,
	 get_node_status/1,
     restart_node/0,
     restart_node/1
	]).

%% Server API
-export([start/0, 
	 start/1, 
	 stop/0, 
	 stop/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% CT hook callback functions
-export([
	 id/1,
	 init/2,
	 pre_init_per_suite/3,
	 post_init_per_suite/4,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 post_end_per_suite/4,
	 on_tc_fail/3,
	 terminate/1
	]).

%% Debug
-export([dump/0,
	 dump/1]).

%% ----------------------------------------------------------
%% Include files
%% ----------------------------------------------------------
-include("pms2_test.hrl").
-include_lib("common_test/include/ct.hrl").

%% ----------------------------------------------------------
%% Macros
%% ----------------------------------------------------------
-define(SERVER, ?MODULE).
-define(HOOK_PRIO, 5).
-define(CT_PAL(Format, Items), pms_test_lib:ct_pal(Format, Items)).
-define(CT_LOG(Format, Items), pms_test_lib:ct_log(Format, Items)).
%% -define(TEST_INFO(Format, Items), pms_test_lib:ct_log(Format, Items)).
%% -define(TEST_DEBUG(Format, Items), pms_test_lib:ct_log(Format, Items)).

-define(TP_PROVIDER, "com_ericsson_pmi*").
-define(LTTNG_EVENT, "debug_trace").
-define(LTTNG_PAR, ?LTTNG_EVENT ++ " " ++ ?TP_PROVIDER).

%% -define(TARGET_DEFAULT_HOST_NAME, du1).
-define(TARGET_DEFAULT_USER, "sirpa").
-define(TARGET_DEFAULT_COOKIE, rcs).
-define(TARGET_DEFAULT_HOST_PREFIX, "du").
-define(TARGET_DEFAULT_DU_NO, 1).
-define(CLOUD_DEFAULT_COOKIE, rcs).
-define(CLOUD_HOST, vrcs).
-define(CLOUD_NODE, 'root@vrcs').
-define(CLOUD_SIM_HOST, 'rcs-sim').
-define(CLOUD_SIM_NODE, 'sirpa@rcs-sim').
-define(IFT_NODE_COOKIE, ift_app).
-define(IFT_APP, "ift_app").
-define(IFT_NODE_NAME(DU), lta("ift_app_sirpa@" ++ atl(DU))).
-define(CS_TARGET_NODE(DU), lta(?TARGET_DEFAULT_USER ++ "@" ++ atl(DU))).

-define(TIMEOUT, 10000).
-define(RECONNECT_TIME, 1000).

-define(TE_LOG_TO, 60000).

-define(DEFAULT_CB_DATA, [{pmiSubscribeCallback, true}, 
			  {pmiReportCallback, true}, 
			  {pmiReportShowCountersCallback, true}]).

-define(PMI2_INIT, pmi2Initialize).
-define(PMI_INIT, pmiInitialize).

-define(DEFAULT_OPTIONS, [instance_name, 
			  erl_api, 
			  auto_finalize, 
			  restart_ift,
			  restart_ift_on_fail,
			  trace_ift,
			  trace_child,
			  trace]).

-define(RESTART_NODE_TO,   5 * 60 * 1000).

%% ----------------------------------------------------------
%% Records
%% ----------------------------------------------------------

-record(state, {
	  server,
	  sim_sname,
	  sessions = [],
	  next_child = 1,
	  remote_node,
	  ift_node,
	  cs_node,
	  node_status,
	  subscribers = [],
	  erl_api,
	  opts = []
	 }).

-record(cth_state, {
	  erl_api = true,
	  opts, 
	  name = ?SERVER, 
	  auto_finalize = on_end_tc,
	  restart_ift = true,
	  restart_ift_on_fail = true,
	  trace_ift = true,
	  trace_child = true,
	  trace = true,
	  remote_node,
	  cs_node,
	  priv_dir
	 }).

%%%===================================================================
%%% PMI functions
%%%===================================================================

pmiInitialize(RegData) ->
    pmiInitialize(RegData, ?MODULE).


pmiInitialize(RegData, CallbackModule) ->
    pmiInitialize(RegData, CallbackModule, self()).

%% Initializes a PM session from an application
%% with the given pid. The given callback module must
%% implement pmiSubscribeCallback/4 and pmiReportCallback/4.

pmiInitialize(RegData, CallbackModule, AppPid) ->
    pmiInitialize(?SERVER, RegData, CallbackModule, AppPid).


pmiInitialize(Server, RegData, CallbackModule, AppPid) ->
    pmi_call(Server, 
	 {?PMI_INIT, [RegData, CallbackModule, AppPid]}).

%% Send PM data to the PM service. 
%%
pmiData(Handle,
	GranularityPeriod, 
	TimeSpec, 
	MeasObjLDN, 
	Bundles) ->
    pmi_call(Handle, {pmiData, [GranularityPeriod, TimeSpec, MeasObjLDN,
				Bundles]}).

%% Send PM Show Counters data to the PM service. 
%%
pmiDataShowCounters(Handle, RequestId, Result, ErrorStr, Bundles) ->
    pmi_call(Handle, {pmiDataShowCounters, [RequestId, Result, ErrorStr,
						   Bundles]}).


pmiFinalize(Handle) ->
    pmi_call(Handle, {pmiFinalize, []}).


%% Default implementation of pmiSubscribeCallback, sending
%% a {pmiSubscribe, {GranularityPeriod, CounterSpecs}}
%% tuple to the application. 

pmiSubscribeCallback(AppPid, GranularityPeriod, CounterSpecs) -> 
    AppPid ! {pmiSubscribe, {GranularityPeriod, CounterSpecs}},
    ok.


%% Default implementation of pmiReportCallback, sending a
%% {pmiReport, {GranularityPeriod, TimeSpec, DeadLine}} tuple
%% to the application.

pmiReportCallback(AppPid, GranularityPeriod, TimeSpec, Deadline) ->
    AppPid ! {pmiReport, {GranularityPeriod, TimeSpec, Deadline}},
    ok.


%% Default implementation of pmiReportCallback, sending a
%% {pmiReport, {GranularityPeriod, TimeSpec, DeadLine}} tuple
%% to the application.

pmiReportShowCountersCallback(AppPid, RequestId, MoLdn, MaxResponseTime) ->
    AppPid ! {pmiReportShowCounters, {RequestId, MoLdn, MaxResponseTime}},
    ok.

%%% ----------------------------------------------------------
%%% PMI2 Functions
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc 
%%% Initialize a PM application instance.
%%% 
%%% The `AppPid' is the pid of an erlang process that must be kept alive 
%%% during the whole lifecycle from pmi2Initialize to pmi2Finalize.
%%% This pid is used by PMS to identify the application instance.
%%% It is also used as the first parameter in all callback functions.
%%%        
%%% @end
%%% ----------------------------------------------------------
pmi2Initialize(Server, Callbacks, CounterMap, CallbackModule, AppPid)
  when is_atom(Server), 
       (is_tuple(Callbacks) orelse is_list(Callbacks)),
       (is_list(CounterMap) orelse
	CounterMap == undefined),
       is_atom(CallbackModule), 
       is_pid(AppPid) -> 
    pmi_call(Server, 
	     {?PMI2_INIT, [Callbacks, CounterMap, CallbackModule, AppPid]}).


pmi2Initialize(Callbacks, CounterMap, CallbackModule, AppPid) 
  when is_tuple(Callbacks) ->
    pmi2Initialize(?SERVER, Callbacks, CounterMap, CallbackModule, AppPid);

pmi2Initialize(Server, Callbacks, CounterMap, CallbackModule) 
  when is_atom(Server) ->
    pmi2Initialize(Server, Callbacks, CounterMap, CallbackModule, self()).


pmi2Initialize(Callbacks, CounterMap, CallbackModule) 
  when (is_tuple(Callbacks) orelse is_list(Callbacks)) -> 
    pmi2Initialize(?SERVER, Callbacks, CounterMap, CallbackModule, self());

pmi2Initialize(Server, Callbacks, CounterMap)
  when is_atom(Server) -> 
    pmi2Initialize(Server, Callbacks, CounterMap, ?MODULE, self()).


pmi2Initialize(Callbacks, CounterMap) 
  when (is_tuple(Callbacks) orelse is_list(Callbacks)) -> 
    pmi2Initialize(?SERVER, Callbacks, CounterMap, ?MODULE, self());

pmi2Initialize(Server, Callbacks) when is_atom(Server) -> 
    pmi2Initialize(Server, Callbacks, ?COUNTER_MAP_DEF, ?MODULE, self()).


pmi2Initialize(Callbacks) 
  when (is_tuple(Callbacks) orelse is_list(Callbacks)) -> 
    pmi2Initialize(?SERVER, Callbacks, ?COUNTER_MAP_DEF, ?MODULE, self());

pmi2Initialize(Server) when is_atom(Server) -> 
    pmi2Initialize(Server, ?DEFAULT_CB_DATA, ?COUNTER_MAP_DEF, ?MODULE, self()).


pmi2Initialize() -> 
    pmi2Initialize(?SERVER, 
		   ?DEFAULT_CB_DATA, 
		   ?COUNTER_MAP_DEF, 
		   ?MODULE, 
		   self()). 


%%% ----------------------------------------------------------
%%% @doc 
%%% Initialize Counters that this session handles.
%%% 
%%% 'Handle' is the handle received from pmi2Initialize.       
%%%        
%%% CounterMaps contains the PmGroups (and their aliases), and
%%% also the MeasurementTypes (and their aliases) that the
%%% session is responsible for.
%%% The aliases will be used in the further interaction in
%%% the session.
%%%
%%% The session client may send several counter map messages,
%%% thus adding new PmGroups/MeasurementTypes.
%%% @end
%%% ----------------------------------------------------------
pmi2CounterMap(Handle, CounterMaps) -> 
    pmi_call(Handle, {pmi2CounterMap, [CounterMaps]}).

%%% ----------------------------------------------------------
%%% @doc Finalize a PM application instance. The `Handle' is the same 
%%%      that was returned from {@link pmi2Initialize/3}. 
%%% @end
%%% ----------------------------------------------------------
pmi2Finalize(Handle) when is_pid(Handle) ->
    pmi_call(Handle, {pmi2Finalize, []}).

%%% ----------------------------------------------------------
%%% @doc Send PM data to PMS.
%%% @end
%%% ----------------------------------------------------------
pmi2DataRop(Handle, GranularityPeriod, ReportId, ValueBundle, FinalFragment)  
  when is_pid(Handle),
       is_integer(GranularityPeriod),
       is_integer(ReportId),
       is_list(ValueBundle),
       is_boolean(FinalFragment) ->
    Attrs = [GranularityPeriod, ReportId, ValueBundle, FinalFragment],
    pmi_call(Handle, {pmi2DataRop, Attrs}).

%%% ----------------------------------------------------------
%%% @doc Send Show Counters data to PMS.
%%% @end
%%% ----------------------------------------------------------
pmi2DataShowCounters(Handle, ReportId, Result, ErrorStr, MeasValues) 
  when is_pid(Handle), is_integer(ReportId), is_list(MeasValues) ->
    Attrs = [ReportId, Result, ErrorStr, MeasValues],
    pmi_call(Handle, {pmi2DataShowCounters, Attrs}).

%%% ----------------------------------------------------------
%%% @doc Default implementation of pmi2SubscribeCallback. Sends an asynchronous 
%%%      message to the AppPid provided in {@link pmi2Initialize/3}.
%%% @end
%%% ----------------------------------------------------------
pmi2SubscribeRopCallback(AppPid, GranularityPeriod, SubscribeSpecs) -> 
    AppPid ! {pmi2SubscribeRop, {GranularityPeriod, SubscribeSpecs}},
    ok.


%%% ----------------------------------------------------------
%%% @doc Default implementation of pmi2ReportRopCallback. Sends an asynchronous 
%%%      message to the AppPid provided in {@link pmi2Initialize/3}.
%%% @end
%%% ----------------------------------------------------------
pmi2ReportRopCallback(AppPid, GranularityPeriod, ReportId, Timeout) ->    
    AppPid ! {pmi2ReportRop, {GranularityPeriod, ReportId, Timeout}},
    ok.


%%% ----------------------------------------------------------
%%% @doc Default implementation of pmi2ReportShowCountersCallback. 
%%%      Sends an asynchronous message to the AppPid provided in 
%%%      {@link pmi2Initialize/3}.
%%% @end
%%% ----------------------------------------------------------
pmi2ReportShowCountersCallback(AppPid, 
			       MeasObjLDNAlias, 
			       ReportId, 
			       ScSpec, 
			       Timeout) ->
    Data = {MeasObjLDNAlias, ReportId, ScSpec, Timeout},
    AppPid ! {pmi2ReportShowCounters, Data},
    ok.


%%%===================================================================
%%% Support functions
%%%===================================================================

%% Make the C node behave as a dead application would.
%% Only the specifed application is affected. If CloseSocket
%% is true the socket is closed from the application side,
%% otherwise it is left unattended.

emulAppDeath(Handle, CloseSocket) ->
    %% TBD
    call(Handle, {emulAppDeath, CloseSocket}).	


get_cs_node() ->
    call(get_cs_node).
    

get_cs_node(Server) ->
    call(Server, get_cs_node).
    

cs_node_call(M, F, A) ->
    call({cs_node_call, {M, F, A}}).
    

cs_node_call(Server, M, F, A) when is_list(A) ->
    call(Server, {cs_node_call, {M, F, A}});

cs_node_call(M, F, A, Timeout) ->
    cs_node_call(?SERVER, M, F, A, Timeout).
    

cs_node_call(Server, M, F, A, Timeout) ->
    call(Server, {cs_node_call, {M, F, A}}, Timeout).
    

ping_ift_app() ->
    call(ping_ift_app).
    

ping_ift_app(Server) ->
    call(Server, ping_ift_app).
    

restart_ift_app() ->
    call(restart_ift_app).
    

restart_ift_app(Server) ->
    call(Server, restart_ift_app).
    

exit_ift_app() ->
    call(exit_ift_app).
    

exit_ift_app(Server) ->
    call(Server, exit_ift_app).
    

get_children() ->
    call(get_children).


get_children(Server) ->
    call(Server, get_children).


get_remote_node() ->
    call(get_remote_node).


get_remote_node(Server) ->
    call(Server, get_remote_node).


get_node_status() ->
    call(get_node_status).


get_node_status(Server) ->
    call(Server, get_node_status).


restart_node() ->
    call(restart_node, ?RESTART_NODE_TO).

restart_node(Server) ->
    call(Server, restart_node, ?RESTART_NODE_TO).


subscribe_testnode() ->
    call({subscribe_testnode, self()}).

subscribe_testnode(Server) ->
    call(Server, {subscribe_testnode, self()}).

unsubscribe_testnode() ->
    call({unsubscribe_testnode, self()}).

unsubscribe_testnode(Server) ->
    call(Server, {unsubscribe_testnode, self()}).

%%%===================================================================
%%% CT Hooks callbacks
%%%===================================================================
%% @private
id(Opts) ->
    get_option(instance_name, Opts).

%% @private
init(Id, Opts) ->
    ?CT_PAL("~w: init(~p, ~p)", [?MODULE, Id, Opts]),
    NewOpts = add_config_opts(add_defaults(Opts)),
    ?CT_LOG("~p: Added default and config Options = ~p", [?MODULE, NewOpts]),
    CTHState = #cth_state{name = lta(Id), opts = NewOpts},
    case start(NewOpts) of
	{ok, {_Pid, CsNode, RNode}} ->
	    NewCTHState = 
		init_cth_state(NewOpts, 
			       CTHState#cth_state{cs_node = CsNode, 
						  remote_node = RNode}),
	    {ok, NewCTHState, ?HOOK_PRIO};
	Error ->
	    ?CT_PAL("~w: init start/2 error: ~p~n", [?MODULE, Error]),
	    {ok, CTHState, ?HOOK_PRIO}
    end.


%% @private
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_suite(_SuiteName, InitData, CTHState) ->
    PrivDir = ?config(priv_dir, InitData),
    NewCTHState = CTHState#cth_state{priv_dir = PrivDir},
    restart_ift_app_if_used(CTHState),
    case check_env(CTHState) of
	ok ->
	    start_ift_trace(CTHState),
	    {InitData, NewCTHState};
	{error, Reason} ->
	    {{skip, Reason}, NewCTHState}
    end.

%% @private
post_init_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, CTHState}.


%% @private
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_testcase(_TC, Config, CTHState) ->
    case check_env(CTHState) of
	ok ->
	    clear_te_log(CTHState),
	    {Config, CTHState};
	{error, Reason} ->
	    {{skip, Reason}, CTHState}
    end.

%% @private
%% post_end_per_testcase(TC, Config, {SkipOrFail, _Reason} = Ret, CTHState)
%%   when SkipOrFail =:= skip; SkipOrFail =:= fail ->
%%     catch log_te_log(TC, Config, CTHState),
%%     {Ret, CTHState};

post_end_per_testcase(TC, Config, Return, CTHState) ->
    case whereis(CTHState#cth_state.name) of
	Pid when is_pid(Pid), 
		 CTHState#cth_state.auto_finalize =:= on_end_tc ->
	    call(Pid, finalize_all);
	_ ->
	    ok
    end,
    try
	log_te_log(TC, Config, CTHState),
	clear_te_log(CTHState)
    catch _:E ->
	    ct:log(lightred,
		   "~p: Failed to log TE log~n~p",
		   [?MODULE, E]),
	    ok
    end,
    {Return, CTHState}.


%% @private
post_end_per_suite(_SuiteName, Config, Return, CTHState) ->
    %% ?CT_PAL("~w: post_end_per_suite(~p, ~s, ~p)",
    %% 	       [?MODULE, _SuiteName, "_", CTHState]),
    try
	CTHState#cth_state.auto_finalize =:= on_end_suite andalso
	    call(CTHState#cth_state.name, finalize_all),
	stop_ift_trace(CTHState),
	log_te_log(end_per_suite, Config, CTHState),
	clear_te_log(CTHState),
	restart_ift_app_if_used(CTHState)
    catch _:_ ->
	    ok
    end,
    {Return, CTHState}.


%% @private
on_tc_fail(_TC, _Reason, CTHState) ->
    try 
	CTHState#cth_state.auto_finalize =:= on_fail andalso
	call(CTHState#cth_state.name, finalize_all),
	CTHState#cth_state.restart_ift_on_fail andalso
	restart_ift_app_if_used(CTHState)
    catch _:_ ->
	    ?CT_LOG("~w: on_tc_fail. State = ~p", [?MODULE, CTHState]),
	    ok
    end,
    CTHState.


%% @private

terminate(CTHState) when is_record(CTHState, cth_state) ->
    Server = CTHState#cth_state.name,
    Pid = whereis(Server),
    ?CT_PAL("~w: terminate, server ~p pid = ~p", [?MODULE, Server, Pid]),
    case Pid of
	undefined ->
	    ok;
	_ ->
	    call(Pid, stop),
	    catch exit(Pid, kill)
    end;
terminate(CTHState) ->
    ?CT_LOG("~w: terminate. State = ~p", [?MODULE, CTHState]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start(add_defaults()).

%% @private
start(Opts) ->
    ?CT_PAL("~p: Start proxy with Opts = ~p~n", [?MODULE, Opts]),
    State = init_state(Opts),
    case init_env(Opts, State) of
	{ok, {CsNode, RNode}} ->
	    start(CsNode, RNode, State);
	Error ->
	    ?CT_PAL("~p: Start proxy. Error = ~p~n", [?MODULE, Error]),
	    Error
    end.


start(CsNode, RNode, State) ->
    Server = State#state.server,
    NewState = State#state{node_status = up,
			   remote_node = RNode,
			   cs_node = CsNode},
    Res = gen_server:start({local, Server}, ?MODULE, NewState, []),
    ?CT_PAL("pms_pmi_proxy (~p) started: ~p", [Server, Res]),
    case Res of
	{ok, Pid} ->
	    {ok, {Pid, CsNode, RNode}};
	Error ->
	    ?CT_PAL("~p: Start proxy gen_server. Error = ~p~n", [?MODULE, Error]),
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stop the server.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    stop(?SERVER).


%% @private
stop(Server) ->
    call(Server, stop).
    

%%%===================================================================
%%% Debug functions
%%%===================================================================
%% @private
dump() ->
    call(dump).


%% @private
dump(Server) ->
    call(Server, dump).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(State) ->
    RNode = State#state.remote_node,
    erlang:monitor_node(RNode, true),
    {ok, State#state{node_status = up}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({pmi_call, _FA}, _From, State) 
  when State#state.node_status =:= down ->
    {reply, {error, node_down}, State};

%% handle_call({pmi_call, {Func, _Args}}, _From, State) 
%%   when Func =:= pmi2Initialize, State#state.erl_api =:= false ->
%%     {reply, {error, ift_app_for_pmi2_not_supported_yet}, State};

handle_call({pmi_call, {Func, Args}}, From, State) 
  when Func =:= ?PMI_INIT; Func =:= ?PMI2_INIT ->
    ChildNo = State#state.next_child,
    Child = create_child_name(Func, ChildNo, State#state.server),
    case handle_pmi_initialize(State, From, Child, Func, Args) of
	{ok, Session} ->
	    erlang:monitor(process, Session),
	    Sessions = State#state.sessions,
	    {noreply, State#state{sessions = [{Session, Child} | Sessions], 
				  next_child = ChildNo + 1}};
	Error ->
	    {reply, Error, State}
    end;

handle_call({pmi_call, {_Func, _Args}}, _From, State) ->
    {reply, {error, invalid_handle}, State};

handle_call(finalize_all, _From, State) ->
    finalize_all(State),
    {reply, ok, State#state{sessions = []}};

handle_call(get_cs_node, _From, State) 
  when State#state.cs_node =/= undefined ->
    {reply, {ok, State#state.cs_node}, State};

handle_call(get_cs_node, _From, State) ->
    {reply, {error, not_available}, State};

handle_call(get_remode_node, _From, State) ->
    {reply, {ok, State#state.remote_node}, State};

handle_call(get_node_status, _From, State) ->
    {reply, {ok, State#state.node_status}, State};

handle_call({subscribe_testnode, Pid}, _From, State) ->
    Subscribers = State#state.subscribers,
    erlang:monitor(process, Pid),
    {reply, ok, State#state{subscribers = [Pid | Subscribers]}};

handle_call({unsubscribe_testnode, Pid}, _From, State) ->
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call({cs_node_call, {M, F, A}}, _From, 
	    #state{cs_node = CsNode} = State) when CsNode =/= undefined ->
    case catch rpc_call(CsNode, M, F, A) of
	{'EXIT', _Reason} = Error ->
	    {reply, {error, Error}, State};
	Res ->
	    {reply, Res, State}
    end;

handle_call({cs_node_call, {_M, _F, _A}}, _From, State) ->
    {reply, {error, cs_node_down}, State};

handle_call(ping_ift_app, _From, State) 
  when State#state.erl_api =:= false ->
    Res = ping_ift_node(State#state.remote_node),
    {reply, Res, State};

handle_call(restart_ift_app, _From, State) 
  when State#state.erl_api =:= false ->
    Reply = restart_ift_app(State#state.remote_node, State#state.cs_node),
    {reply, Reply, State};

handle_call(exit_ift_app, _From, State) 
  when State#state.erl_api =:= false ->
    Reply = exit_ift_node(State#state.remote_node),
    {reply, Reply, State};

handle_call(restart_node, _From, State) ->
    CTHState = #cth_state{
        name        = ?SERVER,
        erl_api     = State#state.erl_api,
        remote_node = State#state.remote_node,
        cs_node     = State#state.cs_node
    },

    NodeRestartRes = handle_restart_node(CTHState),
    {reply, NodeRestartRes, State};

handle_call(Cmd, _From, State) 
  when Cmd =:= ping_ift_app;
       Cmd =:= restart_ift_app;
       Cmd =:= exit_ift_app ->
    {reply, {error, ift_not_initiated}, State};

handle_call(get_children, _From, State) ->
    {reply, {ok, State#state.sessions}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(dump, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    Reply = {error, unknown_call},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, _Type, Object, Info}, State) ->
    ?CT_PAL("[~p] Process ~p is down ~p", [?MODULE, Object, Info]),
    Subscribers = lists:delete(Object, State#state.subscribers),
    %% TBD: Handle unexpected exit of session
    Sessions = lists:keydelete(Object, 1, State#state.sessions),
    {noreply, State#state{subscribers = Subscribers, sessions = Sessions}};

handle_info({nodedown, _Node}, State) ->
    ?CT_PAL("[~p] Node ~p is down!", [?MODULE, _Node]),
    notify_subscribers(State, node_down),
    erlang:start_timer(?RECONNECT_TIME, self(), reconnect_node),
    {noreply, State#state{node_status = down, sessions = []}};

handle_info({timeout, _TRef, reconnect_node}, State) 
  when State#state.node_status =:= down ->
    case ping_remote_node(State) of
	ok ->
	    RNode = State#state.remote_node,
	    erlang:monitor_node(RNode, true),
	    ?CT_PAL("[~p] Node ~p is up!", [?MODULE, RNode]),
	    notify_subscribers(State, node_up),
	    {noreply, State#state{node_status = up}};
	_Error ->
	    erlang:start_timer(?RECONNECT_TIME, self(), reconnect_node),
	    {noreply, State}
    end;

handle_info({timeout, _TRef, reconnect_node}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    ?CT_PAL("[~p] Received unexpected Info: ~p", [?MODULE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ?CT_PAL("pms_pmi_proxy (~p) terminating: ~p", 
	   [State#state.server, _Reason]),
    finalize_all(State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% Init state
%%%===================================================================
init_cth_state(Opts, CTHState) ->
    Fo = get_option(auto_finalize, Opts),
    case get_option(erl_api, Opts) of
	false ->
	    RstIFT = get_option(restart_ift, Opts),
	    RstIFTOF = get_option(restart_ift_on_fail, Opts),
	    TraceCh = get_option(trace_child, Opts),
	    TraceIFT = get_option(trace_ift, Opts),
	    CTHState#cth_state{erl_api = false,
			       auto_finalize = Fo, 
			       restart_ift = RstIFT,
			       restart_ift_on_fail = RstIFTOF,
			       trace_child = TraceCh,
			       trace_ift = TraceIFT,
			       trace = TraceCh orelse TraceIFT};
	_True ->
	    CTHState#cth_state{erl_api = true,
			       auto_finalize = Fo,
			       restart_ift = false,
			       restart_ift_on_fail = false,
			       trace_child = false,
			       trace_ift = false,
			       trace = false}
    end.


init_state(Opts) ->
    Server = lta(get_option(instance_name, Opts)),
    UseErlAPI  = get_option(erl_api, Opts),
    #state{server = Server, erl_api = UseErlAPI, opts = Opts}.

%%%===================================================================
%%% Check that test environment is ok.
%%%===================================================================
check_env(CTHState) ->
    case whereis(CTHState#cth_state.name) of
	Pid when is_pid(Pid) ->
	    %% check_env_ping_nodes(CTHState);
	    check_env_node_status(30, CTHState);
	_Undefined ->
	    {error, "pms_pmi_proxy server DOWN"}
    end.


check_env_node_status(N, CTHState) 
  when CTHState#cth_state.erl_api =:= false ->
    case get_node_status(CTHState#cth_state.name) of
	{ok, up} ->
	    ?CT_PAL("~s is up!", [CTHState#cth_state.remote_node]),
	    ping_cs_node(CTHState#cth_state.cs_node);
	{ok, down} when N > 0 ->
	    timer:sleep(1000),
	    check_env_node_status(N - 1, CTHState);
	{ok, down} ->
	    {error, "IFT app is DOWN"};
	Error ->
	    Error
    end;

check_env_node_status(N, CTHState) ->
    ping_cs_node(N, CTHState#cth_state.remote_node).
    

%%%===================================================================
%%% Call server
%%%===================================================================
pmi_call(Server, {Func, Arg} = Call) ->
    ?CT_LOG("~p: Call ~p(~p)", [?MODULE, Func, Arg]),
    gen_server:call(Server, {pmi_call, Call}, 60000).


call(Arg) ->
    call(?SERVER, Arg).


call(Arg, Timeout) when is_integer(Timeout) ->
    call(?SERVER, Arg, Timeout);


call(Server, Arg) ->
    gen_server:call(Server, Arg, 60000).


call(Server, Arg, Timeout) ->
    gen_server:call(Server, Arg, Timeout).


%%%===================================================================
%%% Init test environment
%%%===================================================================
init_env(Opts, State) ->
    case os:getenv("SIM_OR_TARGET") of
	"target" -> 
	    DUNo = get_target_du_no(Opts),
	    init_cs_target_env(DUNo, State);
	"cloudish" -> 
	    case re:run(atom_to_list(ct:get_config({test_nodes,1})),"^sim") of
		{match,_} -> init_cs_cloud_env(?CLOUD_SIM_NODE, State);
		nomatch   -> init_cs_cloud_env(?CLOUD_NODE, State)
	    end;
	_Sim -> 
	    SName = get_sim_sname(Opts),
	    init_cs_sim_env(SName, State)
    end.


init_cs_cloud_env(CloudNode, State) ->
    try
	Site = get_config({test_nodes, 1}),
	IpStr = get_config({Site, erl_dist_ip}),
	IpTokens = string:tokens(IpStr, "."),
	IP = list_to_tuple([list_to_integer(I) || I <- IpTokens]),
	inet_db:set_lookup([file, native]),
	case CloudNode of
	    ?CLOUD_SIM_NODE -> inet_db:add_host(IP, [atl(?CLOUD_SIM_HOST)]);
	    ?CLOUD_NODE     -> inet_db:add_host(IP, [atl(?CLOUD_HOST)])
	end,
	erlang:set_cookie(CloudNode, ?CLOUD_DEFAULT_COOKIE),
	init_ift_target_env(CloudNode, State)
    catch 
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(ligthred, "init target env failed: ~p~n~p~n", [Error, ST]),
	    {error, Error}
    end.


init_cs_target_env(DUNo, State) when is_integer(DUNo) ->
    try
	Site = get_config({test_nodes, DUNo}),
	IpStr = get_config({Site, erl_dist_ip}),
	IpTokens = string:tokens(IpStr, "."),
	IP = list_to_tuple([list_to_integer(I) || I <- IpTokens]),
	TargetDU = get_du(DUNo),
	inet_db:set_lookup([file, native]),
	inet_db:add_host(IP, [atl(TargetDU)]),
	CsNode = ?CS_TARGET_NODE(TargetDU),
	erlang:set_cookie(CsNode, ?TARGET_DEFAULT_COOKIE),
	init_ift_target_env(CsNode, State)
    catch 
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(ligthred, "init target env failed: ~p~n~p~n", [Error, ST]),
	    {error, Error}
    end.


init_ift_target_env(CsNode, State) when State#state.erl_api =:= false ->
    Tries = 6,
    ok = check_ift_app(Tries, CsNode, ?TIMEOUT),
    IFTNode = lta(?IFT_APP ++ "_" ++ atl(CsNode)),
    erlang:set_cookie(IFTNode, ?IFT_NODE_COOKIE),
    ok = ping_ift_node(IFTNode),
    ?CT_PAL("~s is up!", [?IFT_APP]),
    {ok, {CsNode, IFTNode}};

init_ift_target_env(CsNode, _State) ->
    init_erl_env(CsNode).
    

init_cs_sim_env(SName, State) ->
    CsNode = sim_cs_node(SName),
    if
	State#state.erl_api ->
	    init_erl_env(CsNode);
	true ->
	    IFTNode = lta(?IFT_APP ++ "_" ++ atl(CsNode)),
	    init_sim_env(CsNode, IFTNode)
    end.


init_erl_env(CsNode) ->
    case ping_cs_node(CsNode) of
	ok ->
	    {ok, {CsNode, CsNode}};
	Error ->
	    Error
    end.
    

init_sim_env(CsNode, IFTNode) ->
    try
	%% Tries = 6,
        %% ok = check_ift_app(Tries, CsNode, ?TIMEOUT),
	erlang:set_cookie(IFTNode, ?IFT_NODE_COOKIE),
	ok =  ping_ift_node(IFTNode),
	?CT_PAL("~s is up!", [?IFT_APP]),
	{ok, {CsNode, IFTNode}}
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(ligthred, "init sim env failed: ~p~n~p~n", [Error, ST]),
	    {error, Error}
    end.


ping_remote_node(State) when State#state.erl_api =:= false ->
    ping_ift_node(State#state.remote_node);
    
ping_remote_node(State) ->
    ping_cs_node(State#state.remote_node).


ping_cs_node(CsNode) ->
    ping_cs_node(10, CsNode).


ping_cs_node(N, CsNode) when N >= 0 ->
    case net_adm:ping(CsNode) of
	pong ->
	    ping_pms_app_reg(CsNode);
	_Pang ->
	    timer:sleep(1000),
	    ping_cs_node(N - 1, CsNode)
    end;	    

ping_cs_node(_N, CsNode) ->
    {error, {failed_to_ping, CsNode}}.


ping_pms_app_reg(CsNode) ->
    ping_pms_app_reg(10, CsNode).


ping_pms_app_reg(N, CsNode) when N >= 0 ->
    case where_is_app_reg(CsNode) of
	Pid when is_pid(Pid) ->
	    ?CT_LOG("~p: pmsAppRegistry ~p is up!", [?MODULE, Pid]),
	    ok;
	_NotFound ->
	    timer:sleep(1000),
	    ping_pms_app_reg(N -1 , CsNode)
    end;

ping_pms_app_reg(_N, CsNode) ->
    {error, {failed_to_ping, {pmsAppRegistry, CsNode}}}.


where_is_app_reg(CsNode) ->
    case rpc_call(CsNode, global, whereis_name, [pmsAppRegistry]) of
	undefined ->
	    rpc_call(CsNode, erlang, whereis, [pmsAppRegistry]);
	Pid ->
	    Pid
    end.
    
%%%===================================================================
%%% PMI Session
%%%===================================================================
handle_pmi_initialize(State, From, Child, Func, Args) ->
    IFTNode = State#state.remote_node,
    CsNode = State#state.cs_node,
    Opts = State#state.opts,
    case pms_pmi_proxy_session:start(Child, IFTNode, CsNode, Opts) of
	{ok, Pid} = Res ->
	    gen_server:cast(Pid, {{Func, Args}, From}),
	    Res;
	Error ->
	    Error
    end.

%%%===================================================================
%%% rct_proxy functions
%%%===================================================================
ping_ift_node(IFTNode) ->
    ping_ift_node(10, IFTNode).


ping_ift_node(N, IFTNode) when N >= 0 ->
    {master_ping, IFTNode} ! {self()},
    receive
	{ok, master_pong} ->
	    ok
    after 1000 ->
	    ping_ift_node(N - 1, IFTNode)
    end;

ping_ift_node(_N, IFTNode) ->
    {error, {failed_to_ping, IFTNode}}.


exit_ift_node(IFTNode) ->
    {exit, IFTNode} ! {self()},
    receive
	{ok, exited} ->
	    ok
    after 5000 ->
	   {error, failed_to_exit_ift}
    end.


check_ift_app(TriesLeft, CsNode, Timeout) ->
    case check_ift_app(CsNode, Timeout) of
	ok ->
	    ok;
	{error, _Reason} = Error when TriesLeft =:= 0 ->
	    Error;
	{error, Reason} ->
	    ?CT_PAL("pms_pmi_proxy:check_app() failed. Reason: ~p "
		   "Tries left: ~p", [Reason, TriesLeft]),
	    timer:sleep(timer:seconds(10)),
	    check_ift_app(TriesLeft - 1, CsNode, Timeout)
    end.


check_ift_app(CsNode, Timeout) ->
    case rpc_call(CsNode, appmServer, get_apps, [], Timeout) of
	{badrpc, Error} ->
	    {error, {"no connection to erlang node", Error}};
	AppProplist ->
	    check_ift_app(AppProplist)
    end.


check_ift_app(AppProplist) ->
    case proplists:lookup(?IFT_APP, AppProplist) of
	{?IFT_APP, _} ->
	    ok;
	_ ->
	    {error, "test app has not started"}
    end.

    
restart_ift_app_if_used(CTHState) 
  when CTHState#cth_state.erl_api =:= false,
       CTHState#cth_state.restart_ift =:= true ->
    Result =  restart_ift_app(CTHState#cth_state.remote_node, 
			      CTHState#cth_state.cs_node),
    ?CT_PAL("~p: IFT app restart result ~p", [?MODULE, Result]),
    timer:sleep(3000),
    ping_ift_node(CTHState#cth_state.remote_node),
    check_env(CTHState);

restart_ift_app_if_used(_CTHState) ->
    ?CT_PAL("~p: IFT app not used ~s...", [?MODULE, ?IFT_APP]),
    ok.


restart_ift_app(_IFTNode, CsNode) ->
    %% Best effort for now. No check of result.
    case ping_cs_node(CsNode) of
	ok ->
	    ?CT_PAL("~p: Restarting ~s...", [?MODULE, ?IFT_APP]),
	    rpc_call(CsNode, appmServer, start_lm, [?IFT_APP]);
	Error ->
	    ?CT_PAL("~p: IFT app restart failed ~s. Error ~p", [?MODULE, ?IFT_APP, Error]),
	    Error
    end.

%%%===================================================================
%%% Trace and Error Log
%%%===================================================================
start_ift_trace(#cth_state{cs_node = CsNode, trace_ift = TraceIFT}) 
  when TraceIFT =:= true,
       CsNode =/= undefined ->
    rpc_call(CsNode, os, cmd, [te_cmd("enable " ++ ?LTTNG_PAR, CsNode)]),
    start_trace("ift_master", CsNode);

start_ift_trace(_) ->
    ok.


stop_ift_trace(#cth_state{cs_node = CsNode, trace_ift = TraceIFT}) 
  when TraceIFT =:= true,
       CsNode =/= undefined ->
    rpc_call(CsNode, os, cmd, [te_cmd("disable " ++ ?LTTNG_PAR, CsNode)]),
    stop_trace("ift_master", CsNode).


start_trace(Process, CsNode) ->
    Res = rpc_call(CsNode, os, cmd, 
		   [te_cmd("enable all " ++ atl(Process), CsNode)]),
    ?CT_LOG("Start trace on ~p: ~p", [Process, Res]).


stop_trace(Process, CsNode) ->
    Res = rpc_call(CsNode, os, cmd, 
		   [te_cmd("disable all " ++ atl(Process), CsNode)]),
    ?CT_LOG("Stop trace on ~p: ~p", [Process, Res]).


log_te_log(TC, Config, #cth_state{cs_node = CsNode, trace = Trace} = S)
  when CsNode =/= undefined, Trace =:= true ->
    TELog = get_te_log(CsNode),
    PrivDir = get_priv_dir(Config, S),
    pms2_test_lib:log_te_log(TELog, atl(TC), PrivDir); 

log_te_log(_TC, _Config, CTHState) when CTHState#cth_state.trace =:= true ->
    ct:log(lightred,
	   "~p: Missed to log TE log, CTHState = ~p",
	   [?MODULE, CTHState]),
    ok;

log_te_log(_TC, _Config, _CTHState) ->
    ok.



get_priv_dir(Config, CTHState) when is_list(Config) ->
    case ?config(priv_dir, Config) of
	undefined ->
	    CTHState#cth_state.priv_dir;
	PrivDir ->
	    PrivDir
    end;

get_priv_dir(_Config, CTHState) ->
    CTHState#cth_state.priv_dir.


get_te_log(CsNode) -> 
    Cmd = te_cmd("log read", CsNode),
    get_te_log(2, CsNode, Cmd). 


get_te_log(N, CsNode, Cmd) when N > 0 -> 
    case rpc_call(CsNode, os, cmd, [Cmd], ?TE_LOG_TO) of
	[] ->
	    timer:sleep(2000),
	    get_te_log(N - 1, CsNode, Cmd);
	TELog when is_list(TELog) ->
	    TELog;
	Error ->
	    ?CT_LOG("Failed to read TE log: ~p", [Error]),
	    []
    end;

get_te_log(_N, _CsNode, _Cmd) -> 
    [].    


clear_te_log(#cth_state{cs_node = CsNode, trace_ift = true}) 
  when CsNode =/= undefined ->
    Cmd = te_cmd("log clear", CsNode),
    case rpc_call(CsNode, os, cmd, [Cmd], ?TE_LOG_TO) of
	{badrpc, _} = Error ->
	    ?CT_LOG("Failed to clear TE log: ~p", [Error]);
	_Ok ->
	    ok
    end; 

clear_te_log(_CTHState) ->
    ok.    


te_cmd(Params, _CsNode) ->
    Cmd = "te " ++ Params,
    ?CT_LOG("TE log cmd: ~s", [Cmd]),
    Cmd.



handle_restart_node(CTHState) ->
    %%CheckEnvResBeforeRestart = check_env(CTHState),
    %%?CT_LOG("handle_restart_node~n"
    %%        "State: ~p"
    %%        "CheckEnv: ~p", 
    %%        [CTHState, CheckEnvResBeforeRestart]),

    Node = CTHState#cth_state.cs_node,
    DownTimeout = 1 * 60 * 1000,

    ok = rpc_call(Node, init, reboot, []),

    monitor_node(Node, true),
    {ok, nodedown} = wait_node_down(Node, DownTimeout),
    monitor_node(Node, false),

    CheckEnvRes = check_env(CTHState),
    ?CT_LOG("check_env result: ~n~p", [CheckEnvRes]),
    CheckEnvRes.


wait_node_down(Node, Timeout) ->
    receive
        {nodedown, Node} ->
            ?CT_LOG("Node down: ~p", [Node]),
            {ok, nodedown};
        Unexpected ->
            ?CT_LOG("Unexpected message while waiting for node down:~n~p", [Unexpected]),
            wait_node_down(Node, Timeout)
    after 1 * 60 * 1000 ->
        ?CT_LOG("Timeout while waiting for node down", []),
        {error, node_down_timeout}
    end.


%%%===================================================================
%%% lib functions
%%%===================================================================
add_defaults() ->
    add_defaults([]).


add_defaults(Opts) ->
    DefOpts = [{Key, get_option(Key, Opts)} || Key <- ?DEFAULT_OPTIONS],
    %% ?CT_PAL("Default Options = ~p", [DefOpts]),
    lists:foldl(fun({K, _V} = Opt, AccOpts) ->
			lists:keystore(K, 1, AccOpts, Opt)
		end, Opts, DefOpts).


get_option(Opt, Opts) ->
    case proplists:get_value(Opt, Opts) of
	undefined ->
	    get_default_option(Opt, #cth_state{});
	Val ->
	   Val
    end. 


%% get_option(Opt, Opts, CTHState) ->
%%     case proplists:get_value(Opt, Opts) of
%% 	undefined ->
%% 	    get_default_option(Opt, CTHState);
%% 	Val ->
%% 	   Val
%%     end. 


get_default_option(instance_name, CTHState) ->
    CTHState#cth_state.name;

get_default_option(erl_api, CTHState) ->
    CTHState#cth_state.erl_api;

get_default_option(auto_finalize, CTHState) ->
    CTHState#cth_state.auto_finalize;

get_default_option(restart_ift, CTHState) ->
    CTHState#cth_state.restart_ift;

get_default_option(restart_ift_on_fail, CTHState) ->
    CTHState#cth_state.restart_ift_on_fail;

get_default_option(trace_ift, CTHState) ->
    CTHState#cth_state.trace_ift;

get_default_option(trace_child, CTHState) ->
    CTHState#cth_state.trace_child;

get_default_option(trace, CTHState) ->
    CTHState#cth_state.trace;

get_default_option(_Opt, _CTHState) ->
    undefined.


add_config_opts(Opts) ->
    ConfOpts = pms2_test_lib:get_ct_config(?MODULE),
    lists:foldl(fun({K, _V} = Opt, AccOpts) ->
			lists:keystore(K, 1, AccOpts, Opt)
		end, ConfOpts, Opts).
    

get_sim_sname(Opts) ->
    rct_safe_rpc_lib:get_sim_sname(Opts).


get_target_du_no(Opts) ->    
    rct_safe_rpc_lib:get_target_du_no(Opts).


get_du(N) when is_integer(N) ->
    lta(?TARGET_DEFAULT_HOST_PREFIX ++ integer_to_list(N)).


get_config(Par) ->
    case ct:get_config(Par) of
	undefined ->
	    ct:log(lightred,
		   "~p ~p Could not read config parameter ~p for "
		   "pms_pmi_proxy, Reason: undefined",
		   [?MODULE, get_config, Par]),
	    throw({?MODULE, {{fail, {undefined, Par}}}});
	Val ->	    
	    Val
    end.


notify_subscribers(State, Notification) ->
    lists:foreach(fun(Pid) ->
			  Pid ! {State#state.server, {State#state.remote_node, 
						      Notification}}
		  end, State#state.subscribers).


finalize_all(State) ->
    F = fun({Pid, _Session}) ->
		catch pmi_call(Pid, {pmiFinalize, []})
	end,
    lists:foreach(F, State#state.sessions).


sim_cs_node(SName) when SName =/= undefined ->
    {ok, HostName} = inet:gethostname(),          
    lta(lists:append([atl(SName), "@", HostName])).


rpc_call(Node, Module, Function, Args) ->
    rpc_call(Node, Module, Function, Args, 20000).


rpc_call(Node, Module, Function, Args, Timeout) ->
    rpc:call(Node, Module, Function, Args, Timeout).


create_child_name(Func, Number, InstName) ->
    Prot = func_to_prot(Func),
    ChildName = Prot ++ "_child-" ++ integer_to_list(Number),
    add_inst_to_child_name(InstName, ChildName).


func_to_prot(?PMI2_INIT) ->
    "pmi2";

func_to_prot(?PMI_INIT) ->
    "pmi".


add_inst_to_child_name(?SERVER, ChildName) ->
    lta(ChildName);

add_inst_to_child_name(InstName, ChildName) ->
    lta(atl(InstName) ++ "_" ++ ChildName).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.


atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) when is_list(L) ->
    L.



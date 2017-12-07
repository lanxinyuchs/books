%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/1      2013-08-07 eolaand     Created
%%% R2A/7      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/7      2013-09-26 etxivri     clear handles list when testnode goes 
%%%                                   down.
%%% R2A/10     2013-11-12 eolaand     Check if handle exists before sending 
%%%                                   finalize.
%%% R3A/1      2014-12-18 etxivri     Increase timeout before reconnect node.
%%% R3A/5      2015-03-25 eolaand     Don't restart the erlang node before the 
%%%                                   suite is run, just the safe app.
%%% R4A/1      2015-06-08 eolaand     Updates for cluster.
%%% R6A/1      2016-09-22 eolaand     Restart test app when init SAFE fails.
%%% R11A/1     2017-10-19 etxkols     Support rcs-sim in cloud env
%%% ----------------------------------------------------------
%% @author etxkols
%% @copyright Ericsson AB 2013-2017
%% @doc 
%% This module provides simplified RPC functionality towards the OTP Safe 
%% application executing on a remote node.
%%
%% The module is implemented as a ct_hook in order to facilitate its use in 
%% Common Test suites. Before each test run this ct_hook starts 
%% the Safe application on the remote test node. The Safe application connects
%% towards an RCS node, either on target or in a simulated environment.
%%
%% The ct_hook sets up a connection towards the remote node using the default 
%% value for node name, unless the parameter sname is used, and a hardcoded 
%% cookie `testnode'. A rct_safe_rpc server is started in order to handle the 
%% communication towards the remote test node.  
%%
%% The API functions in {@link rct_safe_imm_oi_rpc}, {@link rct_safe_log_rpc} 
%% and {@link rct_safe_ntf_rpc} respectively should be used to call the safe
%% application via the rct_safe_rpc server provided by this ct_hook.
%% All API functions also has a corresponding higher arity function
%% that takes the registered name of the server as first parameter.
%% The registered name is useful mainly for multinode tests. 
%% See the OTP Safe documentation for more information about the API functions.
%%
%% The `rct_safe_rpc' hook is specified in the `suite/0' function of the 
%% test suite as described below:
%%
%% ```suite() -> 
%%        [{ct_hooks, [{safe_log_rpc, Opts}]}].
%%
%%    Opts = [Opt]
%%    Opt = {safe_services, SafeServices} |
%%          {du_no, No} | 
%%          {sim_sname, Sname} | 
%%          {instance_name, InstanceName} | 
%%          {finalize_on_fail, FoFFlag}'''
%%
%%  `SafeServices = [SafeService]' <br/>
%%  `SafeService = imm | log | ntf' <br/>
%%    - Used to specify which Safe services that should be started.<br/>
%%
%%  `No = integer()' <br/>
%%    - Used to indicate testnode (DU) number when testing towards a specific 
%%      node in a cluster.<br/>
%%      Default is 1
%%
%%  `Sname = atom()' <br/>
%%    - Used to provide a different sim node name than the default 
%%      when running tests towards a simulator.<br/>
%%      Default is `$USER' <br/>
%%
%%  `InstanceName = atom()' <br/>
%%    - Used to provide an identity of the CTH instance. Also used as the 
%%      registered name of the server handling the connection towards the test 
%%      node. The naming is useful when running multinode tests. 
%%      See example below.<br/>
%%      Default is `?MODULE' <br/>
%%
%%  `FoFFlag = boolean()' <br/>
%%    - Determines if all active handles should be automatically finalized
%%      at test case failure. <br/>
%%      Default is `false'<br/>
%%
%% In a multinode scenario several instances may be specified using different
%% instance names as in this example.
%%
%% ```suite() -> 
%%     [{ct_hooks, [{rct_safe_rpc, [{instance_name, safe_1},
%%                              {du_no, 1}]},
%%                  {rct_safe_rpc, [{instance_name, safe_2}
%%                              {du_no, 2}]}].'''
%%
%% @end
-module(rct_safe_rpc).

-behaviour(gen_server).

%% Support API
-export([get_testnode/0, 
	 get_testnode/1,
	 get_node_status/0,
	 get_node_status/1,
	 subscribe_testnode/0, 
	 subscribe_testnode/1,
	 unsubscribe_testnode/0, 
	 unsubscribe_testnode/1
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

%% ct_hooks callbacks
-export([id/1, 
	 init/2, 
	 pre_init_per_suite/3, 
	 pre_init_per_testcase/3, 
	 on_tc_fail/3,
	 terminate/1]).

%% Debug
-export([dump/0,
	 dump/1]).

%-compile(export_all).

-include_lib("safe/include/safe.hrl").
-include("rct_safe_rpc.hrl").

-define(SERVER, ?MODULE). 
-define(IMM_REG_NAMES, [rct_safe_imm_oi_rpc, rct_safe_imm_om_rpc]).
-define(LOG_REG_NAMES, [rct_safe_log_rpc]).
-define(NTF_REG_NAMES, [rct_safe_ntf_rpc]).

-define(START_TIMEOUT, 10000).
-define(TARGET_DEFAULT_USER, sirpa).
-define(TARGET_DEFAULT_HOST_NAME, du1).
-define(TEST_NODE_COOKIE, testnode).
-define(TARGET_DU_NO, 1).
-define(TEST_APP_SAFE, "test_oi").

-define(CLOUD_NODE, 'root@vrcs').
-define(CLOUD_SIM_NODE, 'sirpa@rcs-sim').

-define(RECONNECT_TIME, 5000).
-define(RESTART_SLAVE_TIME, 3000).

-record(cth_state, {opts, 
		    name, 
		    finalize_on_fail = false}).

-record(state, {server, 
		handles = [], 
		remote_node,
		node_name,
		cookie, 
		node_status = up,
		master = true,
		slave, 
		safe_services, 
		rpc_handlers = [],
		subscribers = []}).

%%%===================================================================
%%% Support functions
%%%===================================================================
%% @private
get_testnode() ->
    get_testnode(?SERVER).

%% @private
get_testnode(Server) ->
    call(Server, get_testnode).

%% @private
get_node_status() ->
    get_node_status(?SERVER).

%% @private
get_node_status(Server) ->
    call(Server, get_node_status).

%% @private
subscribe_testnode() ->
    subscribe_testnode(?SERVER).

%% @private
subscribe_testnode(Server) ->
    call(Server, {subscribe_testnode, self()}).

%% @private
unsubscribe_testnode() ->
    unsubscribe_testnode(?SERVER).

%% @private
unsubscribe_testnode(Server) ->
    call(Server, {unsubscribe_testnode, self()}).

%%%===================================================================
%%% CT Hooks callbacks
%%%===================================================================
%% @private
id(Opts) ->
    proplists:get_value(instance_name, Opts, ?SERVER).


%% @private
init(Id, Opts) ->
    start(Opts),
    Fof = proplists:get_value(finalize_on_fail, Opts, false),
    {ok, #cth_state{name = lta(Id), finalize_on_fail = Fof, opts = Opts}}.


%% @private
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};

pre_init_per_suite(_Suite, Config, State) ->
    case whereis(State#cth_state.name) of
	Pid when is_pid(Pid) ->
	    {Config, State};
	undefined ->
	    {{fail, "Failed to start server."}, State}
    end.
	    

%% @private
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};

pre_init_per_testcase(_TC, Config, State) ->
    case catch check_node_up(State) of
	ok ->
	    {Config, State};
	_Error ->
	    ct:print("Test node is down.", []),
	    ct:log("Test node is down: ~p", [_Error]),
	    {{skip, "Test node is down"}, State}
    end.


check_node_up(State) ->
    Server = State#cth_state.name,
    ok = subscribe_testnode(Server),
    case get_node_status(Server) of
	{ok, up} ->
	    ok = unsubscribe_testnode(Server);
	_Down ->
	    wait_node_up(Server)
    end.


wait_node_up(Server) ->
    receive
	{Server, {_Node, node_up}} ->
	    ok = unsubscribe_testnode(Server);
	{Server, {_Node, node_down}} ->
	    wait_node_up(Server)
    after 60000 ->
	    ok = unsubscribe_testnode(Server),
	    {error, timeout}
    end.


%% @private
on_tc_fail(_TC, _Reason, State) ->
    case State#cth_state.finalize_on_fail of
	true ->    
	    catch call(State#cth_state.name, finalize_all);
	_False ->
	    ok
    end,
    State.


%% @private
terminate(State) ->
    catch stop(State#cth_state.name).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start([]).

%% @private
start(Arg) ->
    State = init_state(Arg),
    case catch init_env(State, Arg) of
	{ok, TestNode} ->
	    Server = State#state.server,
	    NewState = State#state{remote_node = TestNode},
	    Res = gen_server:start({local, Server}, ?MODULE, NewState, []),
	    ct:pal("rct_safe_rpc (~p) started: ~p", [State#state.server, Res]),
	    Res;
	Error ->
	    ct:pal("Failed to start rct_safe_rpc : ~p",[Error]),
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
    if 
	State#state.master ->
	    erlang:monitor_node(RNode, true);
	true ->
	    ok
    end,
    Pid = rct_safe_rpc_lib:start_slave(RNode),
    erlang:monitor(process, Pid),
    RPCHandlers = start_rpc_handlers(State),
    lists:foreach(fun(Handler) -> 
			  erlang:monitor(process, Handler)
		  end, RPCHandlers),
    {ok, State#state{slave = Pid, 
		     rpc_handlers = RPCHandlers, 
		     subscribers = RPCHandlers}}.

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
handle_call({safe_call, _MFA}, _From, State) 
  when State#state.slave =:= undefined; State#state.node_status =:= down ->
    {reply, {error, node_down}, State};

handle_call({safe_call, {Mod, Func, Args}}, From, State) 
  when Func =:= initialize; Func =:= initialize_2 ->
    call_safe_initialize(State, From, Mod, Func, Args),
    {noreply, State};

handle_call({safe_call, {Mod, finalize = Func, Args}}, From, State) ->
    [Handle | _] = Args,
    MH = {Mod, Handle},
    Handles = State#state.handles,
    case lists:member(MH, Handles) of
	true ->
	    call_safe(State, From, Mod, Func, Args),
	    {noreply, State#state{handles = lists:delete(MH, Handles)}};
	_False ->
	    {reply, {error, ?SAFE_AIS_ERR_BAD_HANDLE}, State} 
    end;

handle_call({safe_call, {Mod, Func, Args}}, From, State) ->
    call_safe(State, From, Mod, Func, Args),
    {noreply, State};

handle_call({store_handle, Mod, Handle}, _From, State) ->
    NewState = store_handle(Mod, Handle, State),
    {reply, ok, NewState};

handle_call(finalize_all, _From, State) ->
    finalize_all(State),
    {reply, ok, State#state{handles = []}};

handle_call(get_testnode, _From, State) ->
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

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(dump, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_info({?SAFE_CB_TAG, From, Callback, Arg}, State) ->
    exec_callback(From, Callback, Arg),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) 
  when Object =:= State#state.slave ->
    ct:log("[~p] Slave is down: ~p", [?MODULE, _Info]),
    erlang:start_timer(?RESTART_SLAVE_TIME, self(), restart_slave),
    {noreply, State#state{slave = undefined}};

handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) ->
    Subscribers = lists:delete(Object, State#state.subscribers),
    {noreply, State#state{subscribers = Subscribers}};

handle_info({nodedown, _Node}, State) ->
    ct:pal("[~p] Node ~p is down!", [?MODULE, _Node]),
    notify_subscribers(State, node_down),
    erlang:start_timer(2*?RECONNECT_TIME, self(), reinit_safe),
    {noreply, State#state{node_status = down, handles = []}};

handle_info({timeout, _TRef, restart_slave}, State) 
  when State#state.slave =:= undefined, State#state.node_status =:= down ->
    erlang:start_timer(?RESTART_SLAVE_TIME, self(), restart_slave),
    {noreply, State};

handle_info({timeout, _TRef, restart_slave}, State) 
  when State#state.slave =:= undefined ->
    case catch rct_safe_rpc_lib:start_slave(State#state.remote_node) of
	Pid when is_pid(Pid) ->
	    ct:log("[~p] Slave is up!", [?MODULE]),
	    erlang:monitor(process, Pid),
	    {noreply, State#state{slave = Pid}};
	_Error ->
	    erlang:start_timer(?RESTART_SLAVE_TIME, self(), restart_slave),
	    {noreply, State}
    end;

handle_info({timeout, _TRef, restart_slave}, State) ->
    {noreply, State};

handle_info({timeout, _TRef, reinit_safe}, State) ->
    case reinit_safe(State) of
	{ok, Pid} ->
	    ct:pal("[~p] Node ~p is up!", [?MODULE, State#state.remote_node]),
	    notify_subscribers(State, node_up),
	    {noreply, State#state{slave = Pid, node_status = up}};
	_Error ->
	    erlang:start_timer(?RECONNECT_TIME, self(), reinit_safe),
	    {noreply, State}
    end;

handle_info({_Master, {_Node, node_down}}, State) ->
    notify_subscribers(State, node_down),
    {noreply, State};

handle_info({_Master, {_Node, node_up}}, State) 
  when is_pid(State#state.slave) ->
    {noreply, State};

handle_info({_Master, {_Node, node_up}}, State) ->
    case catch rct_safe_rpc_lib:start_slave(State#state.remote_node) of
	Pid when is_pid(Pid) ->
	    erlang:monitor(process, Pid),
	    notify_subscribers(State, node_up),
	    {noreply, State#state{slave = Pid}};
	_Error ->
	    {noreply, State#state{slave = undefined}}
    end;

handle_info(_Info, State) ->
    ct:pal("Received unexpected Info: ~p", [_Info]),
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
    ct:pal("rct_safe_rpc (~p) terminating: ~p", 
	   [State#state.server, _Reason]),
    finalize_all(State),
    rct_safe_rpc_lib:stop_slave(State#state.slave),
    [stop(Handler) || Handler <- State#state.rpc_handlers],
    RNode = State#state.remote_node,
    rct_safe_rpc_lib:stop_safe(State#state.safe_services, RNode),
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
init_state(Arg) ->
    Server = lta(proplists:get_value(instance_name, Arg, ?SERVER)),
    SafeServices = proplists:get_value(safe_services, Arg, []),
    SName  = proplists:get_value(node_name, Arg),
    Cookie = proplists:get_value(cookie, Arg, ?TEST_NODE_COOKIE),
    #state{server        = Server,
	   safe_services = SafeServices,
	   node_name     = SName,
	   cookie        = Cookie}.


init_env(#state{node_name = SName, cookie = Cookie}, Opt) 
  when SName =/= undefined ->
    ct:pal("~p init_env/2 SNAME = ~p   Opt ~p~n", [?MODULE, SName, Opt]),
    case rct_safe_rpc_lib:init_sim_env(SName, Cookie) of
	{ok, TNode} ->
	    init_safe(TNode, Opt);
	Error ->
	    Error
    end;

init_env(_State, Opt) ->
    init_env(Opt).


init_env(Opt) ->
    ct:pal("~p init_env/1 Opt = ~p ~n", [?MODULE, Opt]),
    case os:getenv("SIM_OR_TARGET") of
	"target" -> 
	    DUNo = rct_safe_rpc_lib:get_target_du_no(Opt),
	    init_env(Opt, init_cs_target_env, DUNo);
	"cloudish" -> 
	    DUNo = rct_safe_rpc_lib:get_target_du_no(Opt),
	    case re:run(atom_to_list(ct:get_config({test_nodes,DUNo})),"^sim") of
		{match,_} -> init_env(Opt, init_cs_cloud_env, {?CLOUD_SIM_NODE, DUNo});
		nomatch   -> init_env(Opt, init_cs_cloud_env, {?CLOUD_NODE, DUNo})
	    end;
	_Sim -> 
	    SName = rct_safe_rpc_lib:get_sim_sname(Opt),
	    init_env(Opt, init_cs_sim_env, SName)
    end.

%% init_env(Opt) ->
%%     case rct_safe_rpc_lib:is_target() of
%% 	true ->
%% 	    DUNo = rct_safe_rpc_lib:get_target_du_no(Opt),
%% 	    init_env(Opt, init_cs_target_env, DUNo);
%% 	_False ->
%% 	    SName = rct_safe_rpc_lib:get_sim_sname(Opt),
%% 	    init_env(Opt, init_cs_sim_env, SName)
%%     end.


init_env(Opt, Func, Arg) ->
    case rct_safe_rpc_lib:Func(Arg) of
	{ok, TNode} ->
	    init_safe(TNode, Opt);
	Error ->
	    Error
    end.


start_rpc_handlers(State) 
  when State#state.master =:= true, State#state.server =:= ?SERVER ->
    RegNames = reg_names(State),
    NewState = State#state{master = false},
    Handlers = [Pid || {ok, Pid} <- [start_server(RegName, NewState) 
				     || RegName <- RegNames]],
    lists:foreach(fun({Name, Pid}) -> 
			  ct:pal("rct_safe_rpc (~p) started: {ok, ~p}", 
				 [Name, Pid])
		  end, lists:zip(RegNames, Handlers)),
    Handlers;

start_rpc_handlers(_State) ->
    [].


start_server(Name, State) ->
    gen_server:start_link({local, Name}, ?MODULE, 
			  State#state{server = Name}, []).


init_safe(TNode, Opt) ->
    ok = rct_safe_rpc_lib:wait_testnode(TNode),
    ct:pal("Testnode ~p is up", [TNode]),
    timer:sleep(1000),
    ct:pal("Restart Safe on testnode ~p", [TNode]),
    ok = rct_safe_rpc_lib:stop_safe(TNode),
    Services = proplists:get_value(safe_services, Opt),
    timer:sleep(3000),
    init_safe_retry(5, Services, TNode, Opt).


init_safe_retry(N, Services, TNode, Opt) ->
    case rct_safe_rpc_lib:init_safe(Services, TNode) of
	ok ->
	    {ok, TNode};
	_Error when N > 0 ->
	    restart_test_node(TNode, Opt),
	    init_safe_retry(N - 1, Services, TNode, Opt);
	Error ->
	    Error
    end.


reinit_safe(State) ->
    try
	RNode = State#state.remote_node,
	ok = rct_safe_rpc_lib:wait_testnode(RNode),
	timer:sleep(3000),
	ok = rct_safe_rpc_lib:init_safe(State#state.safe_services, RNode),
	erlang:monitor_node(RNode, true),
	case State#state.slave of
	    undefined ->
		Pid = rct_safe_rpc_lib:start_slave(RNode),
		ct:log("[~p] Slave is up!", [?MODULE]),
		erlang:monitor(process, Pid),
		{ok, Pid};
	    Slave ->
		{ok, Slave}
	end
    catch _:E ->
	    ct:print("No contact with test node!", []),
	    ct:log("No contact with test node: ~p", [E]),
	    {error, E}
    end.


restart_test_node(TNode, Opt) ->
    CSNode = rct_safe_rpc_lib:get_cs_node(Opt),
    ct:pal("Restart testnode ~p", [TNode]),
    flush_node_down(),
    erlang:monitor_node(TNode, true),
    rct_safe_rpc_lib:rpc_call(CSNode, appmServer, start_lm, [?TEST_APP_SAFE]),
    timer:sleep(3000),
    receive
    	{nodedown, _Node} ->
    	    ct:pal("Testnode ~p is DOWN", [TNode]),
	    flush_node_down(),
	    erlang:monitor_node(TNode, true),
    	    ok
    after 30000 ->
    	    throw(failed_to_restart_testnode)
    end.
    

flush_node_down() ->
    receive
    	{nodedown, _Node} ->
    	    ok
    after 0 ->
    	    ok
    end.
    

call(Arg) ->
    call(?SERVER, Arg).


call(Server, Arg) ->
    gen_server:call(Server, Arg, 20000).


reg_names(State) ->
    F = fun({N, _L}) ->
		N;
	   (N) ->
		N
	end,
    Services = lists:usort(lists:map(F, State#state.safe_services)),
    lists:flatmap(fun service_to_if/1, Services).


store_handle(Mod, Handle, State) ->
    Handles = State#state.handles,
    State#state{handles = [{Mod, Handle} | Handles]}.


notify_subscribers(State, Notification) ->
    lists:foreach(fun(Pid) ->
			  Pid ! {State#state.server, {State#state.remote_node, 
						      Notification}}
		  end, State#state.subscribers).


call_safe_initialize(State, From, Mod, Func, Args) ->
    Server = State#state.server,
    Slave = State#state.slave,
    spawn(fun() ->
		  InitRes = rct_safe_rpc_lib:call_safe(Slave, Mod, Func, Args),
		  case InitRes of
		      {ok, Handle, _Vsn} ->
			  gen_server:call(Server, {store_handle, Mod, Handle});
		      _Error ->
			  ok
		  end,
		  gen_server:reply(From, InitRes)
	  end).


call_safe(State, From, Mod, Func, Args) ->
    Slave = State#state.slave,
    spawn(fun() ->
		  Reply = rct_safe_rpc_lib:call_safe(Slave, Mod, Func, Args),
		  gen_server:reply(From, Reply)
	  end).


exec_callback(From, Callback, Arg) ->
    spawn(fun() ->
		  Res = rct_safe_rpc_lib:exec_callback(Callback, Arg),
		  From ! {?SAFE_CB_RES_TAG, Res}
	  end).


finalize_all(State) ->
    F = fun({Mod, Handle}) ->
		rct_safe_rpc_lib:call_safe(State#state.slave, Mod, finalize, 
					   [Handle])
	end,
    catch lists:foreach(F, State#state.handles).


service_to_if(?IMM_SERVICE) ->
    ?IMM_REG_NAMES;
service_to_if(?LOG_SERVICE) ->
    ?LOG_REG_NAMES;
service_to_if(?NTF_SERVICE) ->
    ?NTF_REG_NAMES.


%% atl(A) when is_atom(A) ->
%%     atom_to_list(A);
%% atl(L) when is_list(L) ->
%%     L.

lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.



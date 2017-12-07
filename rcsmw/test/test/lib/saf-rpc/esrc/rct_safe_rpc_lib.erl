%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-06-27 eolaand     Created
%%% R2A/7      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/8      2013-09-10 etxivri     Updates due to changed behaviour in safe.
%%% R4A/1      2015-06-08 eolaand     Updates for cluster.
%%% R11A/1     2017-10-19 etxkols     Support rcs-sim in cloud env
%%% R11A/2     2017-10-23 etxivri     Update due to compile warnings with OTP20.
%%% ----------------------------------------------------------
%% @author etxivri
%% @copyright Ericsson AB 2013-2017
%% @doc 
%% This module provides support functions for using OTP SAFE on 
%% a remote test node.
%% @end
%%%-------------------------------------------------------------------
-module(rct_safe_rpc_lib).

%% API
-export([init_cs_target_env/0,
	 init_cs_target_env/1,
	 init_cs_cloud_env/1,
	 init_cs_sim_env/0,
	 init_cs_sim_env/1,
	 init_sim_env/2,
	 init_safe/2,
	 init_safe/3,
	 stop_safe/1,
	 stop_safe/2,
	 call_safe/4,
	 rpc_call/4,
	 exec_callback/2,
	 get_cb_fun/2,
	 get_cb_fun/4,
	 start_slave/1,
	 stop_slave/1,
	 wait_testnode/1,
	 wait_testnode/2,
	 wait_testnode_up/1,
	 wait_testnode_up/2,
	 wait_testnode_down/1,
	 wait_testnode_down/2,
	 set_safe_debug/2,
	 set_safe_debug/3,
	 unset_safe_debug/2,
	 get_cs_node/1,
	 is_target/0,
	 get_target_du_no/1,
	 get_sim_sname/1
	]).

-include_lib("safe/include/safe.hrl").
-include("rct_safe_rpc.hrl").

%% -define(SAFE_AIS_ERR_TIMEOUT, 5).
%% -define(SAFE_AIS_ERR_LIBRARY, 2).
-define(TARGET_DEFAULT_USER, "sirpa").
-define(TARGET_DEFAULT_COOKIE, rcs).
-define(TARGET_DEFAULT_HOST_PREFIX, "du").
-define(TARGET_DEFAULT_DU_NO, 1).
-define(CLOUD_DEFAULT_COOKIE, rcs).
-define(CLOUD_HOST, vrcs).
-define(CLOUD_NODE, 'root@vrcs').
-define(CLOUD_SIM_HOST, 'rcs-sim').
-define(CLOUD_SIM_NODE, 'sirpa@rcs-sim').
-define(TEST_NODE_COOKIE, testnode).
-define(TEST_NODE_NAME(DU), lta("testnode_sirpa@" ++ atl(DU))).
-define(CLOUD_TEST_NODE_NAME(CloudNode), lta("testnode_" ++ atl(CloudNode))).
-define(SAFE_CALL, safe_call).
-define(SAFE_CALL_RES, safe_call_res).
-define(SAFE_SERVICES, [imm, log, ntf]).

%%%===================================================================
%%% API
%%%===================================================================
init_cs_target_env() ->
    N = get_du_no_arg(),
    init_cs_target_env(N).


init_cs_target_env(DUNo) when is_integer(DUNo) ->
    try
	Site = get_config({test_nodes, DUNo}),
	IpStr = get_config({Site, erl_dist_ip}),
	IpTokens = string:tokens(IpStr, "."),
	IP = list_to_tuple([lti(I) || I <- IpTokens]),
	TargetDU = get_du(DUNo),
	inet_db:set_lookup([file, native]),
	inet_db:add_host(IP, [atom_to_list(TargetDU)]),
	CsNode = get_target_du_cs_node(DUNo),
	erlang:set_cookie(CsNode, ?TARGET_DEFAULT_COOKIE),
	ErlTestNode = ?TEST_NODE_NAME(TargetDU),
	erlang:set_cookie(ErlTestNode, ?TEST_NODE_COOKIE),
	ok = wait_testnode(ErlTestNode),
	{ok, ErlTestNode}
    catch 
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(ligthred, "init target env failed: ~p~n~p~n", [Error, ST]),
	    {error, Error}
    end.

init_cs_cloud_env({CloudNode, DUNo})  when is_integer(DUNo) ->
    try
	Site = get_config({test_nodes, DUNo}),
	IpStr = get_config({Site, erl_dist_ip}),
	IpTokens = string:tokens(IpStr, "."),
	IP = list_to_tuple([lti(I) || I <- IpTokens]),
	inet_db:set_lookup([file, native]),
	case CloudNode of
	    ?CLOUD_NODE ->		
		inet_db:add_host(IP, [atom_to_list(?CLOUD_HOST)]);
	    ?CLOUD_SIM_NODE ->
		inet_db:add_host(IP, [atom_to_list(?CLOUD_SIM_HOST)])
	end,
	erlang:set_cookie(CloudNode, ?CLOUD_DEFAULT_COOKIE),
	ErlTestNode = ?CLOUD_TEST_NODE_NAME(CloudNode),
	erlang:set_cookie(ErlTestNode, ?TEST_NODE_COOKIE),
	ok = wait_testnode(ErlTestNode),
	{ok, ErlTestNode}
    catch 
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(ligthred, "init cloudish env failed: ~p~n~p~n", [Error, ST]),
	    {error, Error}
    end.

init_cs_sim_env() ->
    init_cs_sim_env(get_sim_sname()).


init_cs_sim_env(SName) ->
    SimName = atl(?TEST_NODE_COOKIE) ++ "_" ++ atl(SName),
    init_sim_env(SimName, ?TEST_NODE_COOKIE).


init_sim_env(SimName, _Cookie) ->
    TNode = sim_test_node(SimName),
    case wait_testnode(TNode) of
	ok ->
	    {ok, TNode};
	Error ->
	    Error
    end.


init_safe(Type, TNode) when is_atom(Type) ->
    init_safe([Type], TNode);

init_safe(TypePar, TNode) when is_list(TypePar) ->
    try
	TypeDebug = add_debug_lev(TypePar),
	ct:log("stopping safe ~n"),
	StopRes = rpc_call(TNode, application, stop, [safe]),
	ct:log("safe stop result  ~p ~n", [StopRes]),
	timer:sleep(3000),
	Apps = rpc_call(TNode, application, which_applications, []),
	ct:log("running applications  ~p ~n", [Apps]),
	ok = set_safe_debug(TypeDebug, TNode),
	Types = [Type || {Type, _D} <- TypeDebug],
	EnvSet = rpc_call(TNode, application, set_env, [safe, services, Types]),
	ct:log("safe TNode  ~p~n", [TNode]),
	ct:log("safe set_env  ~p~n", [EnvSet]),
	ok = EnvSet,
	EnvB = rpc_call(TNode, application, get_env, [safe, services]),
	ct:log("safe get_env  ~p  (before sleep) ~n", [EnvB]),
	timer:sleep(5000),
	EnvA = rpc_call(TNode, application, get_env, [safe, services]),
	ct:log("safe get_env  ~p  (after sleep) ~n", [EnvA]),
	ok = rpc_call(TNode, application, start, [safe])
    catch _:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Failed to init safe: ~p~n~p~n", [Error, ST]),
	    {error, Error}
    end.


init_safe(Type, SafeDebugLev, TNode) when is_atom(Type) ->
    init_safe([{Type, SafeDebugLev}], TNode).


wait_testnode(TNode) ->
    wait_testnode_up(TNode).


wait_testnode(Tries, TNode) ->
    wait_testnode_up(Tries, TNode).


wait_testnode_up(TNode) ->
    wait_testnode_up(20, TNode).


wait_testnode_up(0, TNode) ->
    {error, {"Failed to connect with test node", TNode}};

wait_testnode_up(Tries, TNode) when Tries > 0 ->
    case net_adm:ping(TNode) of
	pong ->
	    ok;
	_Pang ->
	    timer:sleep(3000),
	    wait_testnode_up(Tries - 1, TNode)
    end.


wait_testnode_down(TNode) ->
    wait_testnode_down(10, TNode).


wait_testnode_down(0, TNode) ->
    {error, {"Test node not down within 10 seconds:", TNode}};

wait_testnode_down(Tries, TNode) when Tries > 0 ->
    case net_adm:ping(TNode) of
	pang ->
	    ok;
	_Pong ->
	    timer:sleep(1000),
	    wait_testnode_down(Tries - 1, TNode)
    end.


set_safe_debug(TypeDebug, TNode) ->
    lists:foreach(fun({Type, Level}) ->
			  DebugType = debug_type(Type),
			  set_safe_debug(Level, DebugType, TNode)
		  end, TypeDebug).


set_safe_debug(undefined, DebugType, TNode) ->
    rpc_call(TNode,application,unset_env,[safe, DebugType]);

set_safe_debug(Level, DebugType, TNode) when 0 =< Level, Level =< 2 ->
    rpc_call(TNode,application,set_env,[safe, DebugType, Level]).


unset_safe_debug(Type, TNode) when is_atom(Type) ->
    rpc_call(TNode,application,unset_env,[safe, debug_type(Type)]);

unset_safe_debug(Types, TNode) when is_list(Types) ->
    lists:foreach(fun({Type, _}) ->
			  unset_safe_debug(Type, TNode);
		     (Type) ->
			  unset_safe_debug(Type, TNode)
		  end, Types).
    

stop_safe(TNode) ->
    stop_safe(?SAFE_SERVICES,TNode).


stop_safe(Type, TNode) when is_atom(Type) ->
    stop_safe([Type], TNode);

stop_safe(Types, TNode) when is_list(Types) ->
    rpc_call(TNode, application,stop,[safe]),
    unset_safe_debug(Types, TNode).
   

start_slave(TNode) ->
    {M, Bin, _F} = code:get_object_code(?MODULE),
    F = "dummy",
    {module, M} = rpc_call(TNode, code, load_binary, [M, F, Bin]),
    SlaveFun = get_slave_fun(),
    _Pid = rpc_call(TNode, erlang, spawn, [SlaveFun]).


call_safe(Slave, Mod, Func, Arg) when is_pid(Slave) ->
    Slave ! {?SAFE_CALL, self(), Mod, Func, Arg},
    receive
	{?SAFE_CALL_RES, Res} ->
	    Res
    end;

call_safe(_Slave, _Mod, _Func, _Arg) ->
    {error, slave_is_down}.


rpc_call(Node, Module, Function, Args) ->
    rpc:call(Node, Module, Function, Args, 20000).


get_cb_fun(undefined, _) ->
    undefined;

get_cb_fun(ok, _) ->
    fun(_Arg) ->
	    ok
    end;

get_cb_fun({error, Error} = Res, _) when is_integer(Error) ->
    fun(_Arg) ->
	    Res
    end;

get_cb_fun(Callback, Server) when is_function(Callback) ->
    get_cb_fun(Callback, Server, ?SAFE_CB_TAG, ?SAFE_CB_RES_TAG).


get_cb_fun(Callback, Server, CbTag, CbResTag) 
  when is_function(Callback) ->
    fun(Arg) ->
	    Server ! {CbTag, self(), Callback, Arg},
	    receive
		{CbResTag, Res} ->
		    Res
	    after 20000 ->
		    {error, ?SAFE_AIS_ERR_TIMEOUT}
	    end
    end;

get_cb_fun(Callback, Server, _CbTag, _CbResTag) ->
    get_cb_fun(Callback, Server).


exec_callback(Callback, Arg) ->
    case catch Callback(Arg) of
	ok ->
	    ok;
	{error, _SafeErr} = Error ->
	    Error;
	EXIT ->
	    ct:log(lightred, "Failed to execute callback: ~p~n", 
		   [EXIT]), 
	    {error, ?SAFE_AIS_ERR_LIBRARY}
    end.


stop_slave(Slave) when is_pid(Slave) ->
    Slave ! {?SAFE_CALL, self(), stop},
    receive
	{?SAFE_CALL_RES, ok} ->
	    ok;
	{'DOWN', _MonitorRef, _Type, Object, _Info} when Object =:= Slave ->
	    ok
    after 3000 ->
	    {error, timeout}
    end;

stop_slave(_Slave) ->
    ok.


is_target() ->
    case os:getenv("SIM_OR_TARGET") of
	"target" -> 
	    true;
	"cloudish" -> 
	    true;
	_Sim -> 
	    false
    end.


get_cs_node(Opts) ->
    case is_target() of
	true ->    
	    get_target_cs_node(Opts);
	_False ->
	    get_sim_cs_node(Opts)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_debug_lev([{Type, Level} | T]) 
  when Type =:= imm; Type =:= ntf; Type =:= log ->
    (0 =< Level andalso Level =< 2) 
	orelse Level =:= undefined 
	orelse throw({invalid_debug_level, Level}),  
    [{Type, Level} | add_debug_lev(T)];

add_debug_lev([Type | T]) when is_atom(Type) ->
    add_debug_lev([{Type, undefined} | T]);

add_debug_lev([]) ->
    [].


get_slave_fun() ->
    Self = self(),
    fun() ->
	    slave_fun(Self)
    end.


slave_fun(Pid) ->
    erlang:monitor(process, Pid),
    slave_loop().


slave_loop() ->
    process_flag(trap_exit,true),
    receive
	{?SAFE_CALL, From, Mod, Func, Arg} ->
	    Res = apply_safe_call(Mod, Func, Arg),
	    From ! {?SAFE_CALL_RES, Res},
	    slave_loop();
	{?SAFE_CALL, From, stop} ->
	    From ! {?SAFE_CALL_RES, ok};
	{'DOWN', _Ref, _Type, _Obj, _Info} ->
	    io:format(user, "slave_fun call ~p~n", [down]),
	    ok;
	{'EXIT', Port, Reason} ->
	    io:format("Exit from: ~p -> ~p~n", [Port, Reason]),
	    slave_loop();
	X ->
	    io:format(user, "slave_fun UNKNOWN MSG ~p~n", [X]),
	    ok
    end.


apply_safe_call(Mod, Func, Arg) ->
    try 
	apply(Mod, Func, Arg)
    catch
	X:Y ->
	    {error, {X,Y}}
    end.
	
%% slave_fun(Pid) ->
%%     erlang:monitor(process, Pid),
%%     F = fun(G) ->
%% 		receive
%% 		    {safe_call, From, Mod, Func, Arg} ->
%% 			Res = apply(Mod, Func, Arg),
%% 			From ! {safe_call_res, Res},
%% 			G(G);
%% 		    {safe_call, From, stop} ->
%% 			From ! {safe_call_res, ok};
%% 		    {'DOWN', _Ref, _Type, _Obj, _Info} ->
%% 			ok
%% 		end
%% 	end,
%%     F(F).
	

get_du(N) when is_integer(N) ->
    lta(?TARGET_DEFAULT_HOST_PREFIX ++ itl(N)).


get_config(Par) ->
    case ct:get_config(Par) of
	undefined ->
	    ct:log(lightred,
		   "~p ~p Could not read config parameter ~p for "
		   "rct_safe_rpc, Reason: undefined",
		   [?MODULE, get_config, Par]),
	    throw({?MODULE, {{fail, {undefined, Par}}}});
	Val ->	    
	    Val
    end.


debug_type(imm) ->
    imm_debug;

debug_type(ntf) ->
    ntf_debug;

debug_type(log) ->
    log_debug;

debug_type(DebugType) 
  when DebugType =:= imm_debug; 
       DebugType =:= ntf_debug;
       DebugType =:= log_debug ->
    DebugType.


sim_test_node(Name) ->
    {ok, Host} = inet:gethostname(),
    lta(atl(Name) ++ "@" ++ Host).


get_target_cs_node(Opts) ->
    DUNo = get_target_du_no(Opts),
    get_target_du_cs_node(DUNo).


get_target_du_cs_node(DUNo) ->
    TargetDU = get_du(DUNo),
    lta(?TARGET_DEFAULT_USER ++ "@" ++ atl(TargetDU)).


get_sim_cs_node(Opts) ->
    SName = get_sim_sname(Opts),
    sim_test_node(SName).


get_sim_sname() ->
    get_sim_sname([]).
    

get_sim_sname(Opts) ->
    case init:get_argument(sim_sname) of
	{ok, [[Name]]} ->
	    Name;
	_Other ->
	    get_sim_sname_opt(Opts)
    end.


get_sim_sname_opt(Opts) ->
    case proplists:get_value(sim_sname, Opts) of
	undefined ->
	    User = os:getenv("USER"),
	    get_sim_sname_opt_du(User, Opts);
	SName -> 
	    SName
    end.


get_sim_sname_opt_du(User, Opts) ->
    case proplists:get_value(du_no, Opts) of
	N when is_integer(N) ->
	    "du" ++ itl(N) ++ "_" ++ User; 
	_Other ->
	    get_sim_sname_du(User)
    end.


get_sim_sname_du(User) ->
    case init:get_argument(mp) of
	{ok, [[DU]]} ->
	    DU ++ "_" ++ User;
	_Other ->
	    User
    end.


get_target_du_no(Opts) ->
    case proplists:get_value(du_no, Opts) of
	N when is_integer(N) ->
	    N;
	_ ->
	    %% Fix for backwards compatibility (old parameter name)
	    get_du_no_arg(proplists:get_value(targ_du_no, Opts))
    end.


get_du_no_arg() ->
    get_du_no_arg(undefined).


get_du_no_arg(N) when is_integer(N) ->
    N;

get_du_no_arg(_) ->
    case init:get_argument(mp) of
	{ok, [[DU]]} ->
	    lti(DU -- "du");
	_Other ->
	    ?TARGET_DEFAULT_DU_NO
    end.
   

lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) ->
    A.

atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) ->
    L.

itl(I) when is_integer(I) ->
    integer_to_list(I);
itl(L) when is_list(L) ->
    L.


lti(L) when is_list(L) ->
    list_to_integer(L);
lti(I) when is_integer(I) ->
    I.

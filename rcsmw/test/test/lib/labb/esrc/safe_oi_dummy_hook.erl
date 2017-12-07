%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	safe_oi_dummy_hook.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R11A/1
%%% 
%%% @doc ==Fake object implementer for test.==
%%% This module start safe-imm and load code to testnode and start testcode to handle callbacks.
%%% Initialize an object implementer that replies with OK for all ccb callbacks.
%%% For the moment this hook is just suported for target.
%%% <br/><br/>
%%% %% The `safe_oi_dummy_hook' hook is specified in the `suite/0' function of the 
%%% test suite as described below:
%%%
%%% ```suite() -> 
%%%        [{ct_hooks, [{safe_oi_dummy_hook, Opts]}].
%%%
%%%    Opts = [Opt]
%%%    Opt = Not used.'''
%%%
%%% @end

%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2013-04-02 etxivri     Created
%%%                                   Note! This is a temporay hook, for the moment! 
%%%                                   SAFE need to exist on target. And correct path need to be pointed out.
%%% R11A/1     2017-10-23 etxivri     Update due to compile warnings with OTP20.
%%%                                   
%%% ----------------------------------------------------------
%%% 

-module(safe_oi_dummy_hook).

-export([init/2,
	 terminate/1,
	 start/0,
	 stop/0
	]).

-define(DUMMY_OI, dummy_oi).
-define(TargetDefaultUser, sirpa).
-define(TargetDefaultHostName, du1).

-record(state, {remote_node}).
-define(IMPL_NAME_1, "ImplementerOne").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").


%%--------------------------------------------------------------------
%% @doc
%% Setup Remote testnode enviroment, soo it is possible to do rpc to the testnode.<br/>
%% Start safe on remote testnode (test_oi).<br/>
%% Load binary on testnode and start it to handle callbacks.<br/>
%% @end
%%--------------------------------------------------------------------
init(_Id, _Opts) ->
    case is_target() of
	true -> 
	    {ok, RNode} = setup_target_env(1, testnode, testnode),
	    rpc_call(RNode,code,add_patha,["/home/sirpa/dev_patches/vobs/otp/otp_delivery/windriver30_ppc/lib/safe-1.2.7/ebin"]),
	    rpc_call(RNode,application,set_env,[safe, services, [imm]]),
	    rpc_call(RNode,application,start,[safe]),

	    {M, Bin, _F} = code:get_object_code(?MODULE),
	    F = "dummy",
	    {module, M} = rpc_call(RNode, code, load_binary, [M, F, Bin]),
	    rpc_call(RNode,?MODULE,start,[]),
	    State = #state{remote_node = RNode},
	    {ok, State};
	false -> 
	   erlang:error("simulated not supported")
    end.


%%--------------------------------------------------------------------
%% @doc
%% Stop handling of callbacks on testnode.<br/>
%% Stop safe aplication.<br/>
%% @end
%%--------------------------------------------------------------------
terminate(State) ->
    rpc_call(State#state.remote_node, ?MODULE,stop,[]),
    rpc_call(State#state.remote_node, application,stop,[safe]).


%%--------------------------------------------------------------------
%% @doc
%% Initializes an OI handle.<br/>
%% Start loop to handle callbacks.
%% @end
%%--------------------------------------------------------------------
start() ->
    OIFun = fun()->
		    Self = self(),
		    CbFun = fun(Arg) ->
				    Self ! {cb_data, Arg},
				    ok
			    end,
		    
		    io:format("Initialize OI Handle", []),
		    {ok, Handle, Vsn} = safe_imm_oi:initialize_2(CbFun),
		    io:format("Initialize ok: ~p ~p",[Handle, Vsn]),
		    io:format("Set Implementer name: ~p", [?IMPL_NAME_1]),
		    ok = safe_imm_oi:implementer_set(Handle, ?IMPL_NAME_1),
		    io:format("Set Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
		    ok = safe_imm_oi:class_implementer_set(Handle, ?IMM_TEST_CLASS1),
		    loop(Handle)
	    end,
    Pid = spawn(OIFun),
    register(?DUMMY_OI,Pid),
    {ok, Pid}.


%% Internal
%% @hidden
loop(Handle) ->
    receive
	{cb_data, Arg} ->
	    io:format("Arg: ~p~n",[Arg]),
	    loop(Handle);
	stop -> 
	    io:format("Release Class Implementer for: ~p", [?IMM_TEST_CLASS1]),
	    ok = safe_imm_oi:class_implementer_release(Handle, ?IMM_TEST_CLASS1),
	    io:format("Clear Implementer ~p", [?IMPL_NAME_1]),
	    ok = safe_imm_oi:implementer_clear(Handle),
	    io:format("Finalize OI Handle ~p", [Handle]),
	    ok = safe_imm_oi:finalize(Handle)
    end.


%% @hidden
stop() ->
    stop(?DUMMY_OI).
stop(Name) ->
    Name ! stop.


%% @hidden
is_target() ->
    case os:getenv("SIM_OR_TARGET") of
	"target" -> 
	    true;
	_Sim -> 
	    false
    end.


%%% This code is coopied and modified from rct_rpc.
%% @hidden
setup_target_env(N, Name, Cookie) ->
    try
	Site = dpt_test_nodes(ct:get_config({test_nodes, N}), Name, N),
	IP   = dpt_config(ct:get_config({Site, erl_dist_ip}), Name, Site),
	
	%% If HostName not given, use site name
	TargHost = default,
	RealHostName = choose(TargHost == default,
			      ?TargetDefaultHostName,
			      TargHost),
	inet_db:set_lookup([file, native]),
	inet_db:add_host(IP, [atom_to_list(RealHostName)]),
	ErlNode = case Cookie of 
		      default -> 
			  list_to_atom(atom_to_list(?TargetDefaultUser) 
				       ++ "@" 
				       ++ atom_to_list(RealHostName));
		      Cookie -> 
			  ErlNode_ = list_to_atom(atom_to_list(Cookie) 
						  ++ "_" 
						  ++ atom_to_list(?TargetDefaultUser)
						  ++ "@" 
						  ++ atom_to_list(RealHostName)),
			  erlang:set_cookie(ErlNode_, Cookie),
			  ErlNode_
		  end,
	ct:pal("### TestNode: ~p",[ErlNode]),
	{ok, ErlNode}
    catch throw:{?MODULE, Error} ->
	    {error, Error}
    end.


%% @hidden
dpt_test_nodes(undefined, Name, N) ->
    ct:log(lightred,
	   "~p: ~p ~p Could not read config parameter ~p for "
	   "rpc, Reason: undefined",
	   [Name, ?MODULE, pre_init_per_suite, {test_nodes, N}]),
    throw({?MODULE, {{fail, {undefined, {test_nodes, N}}}, [Name]}});
dpt_test_nodes(Site, _, _) ->
    Site.


%% @hidden
dpt_config(undefined, Name, Site) ->
    ct:log(lightred,
	   "~p: ~p ~p Could not read config "
	   "parameter ~p for rpc, Reason: undefined",
	   [Name, 
	    ?MODULE, 
	    pre_init_per_suite, 
	    {Site, erl_dist_ip}]),
    throw({?MODULE, {{fail, {undefined, {Site, erl_dist_ip}}}, [Name]}});
dpt_config(IpStr, _, _) ->
    IpTokens = string:tokens(IpStr, "."),
    list_to_tuple([list_to_integer(I) || I <- IpTokens]).


%% @hidden
choose(true,  T, _) -> T;
choose(false, _, F) -> F.


%% @hidden
rpc_call(Node, Module, Function, Args) ->
    rpc:call(Node, Module, Function, Args, 20000).

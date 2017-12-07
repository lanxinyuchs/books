%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_cover_tgt.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/5
%%%
%%% @doc == Cover CT Hook for code coverage tests on target ==
%%%
%%% Write some instructions here...
%%%
%%% @end


-module(rct_cover_tgt). 
%%% Except as specifically authorized in writing by Ericsson, the receiver of
%%% this document shall keep the information contained herein confidential and
%%% shall protect the same in whole or in part from disclosure and dissemination
%%% to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall only be made
%%% on a strict need to know basis.
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev         Date         Name        What
%%% -----       ---------    --------    ------------------------
%%% R2A/1       2014-04-08   eolaand     Created
%%% ----------------------------------------------------------------------------

-export([init/2, terminate/1]).
-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).

-define(TARGET_DEFAULT_USER, sirpa).
-define(TARGET_DEFAULT_HOST_PREFIX, "du").
-define(TARGET_DEFAULT_DU_NO, 1).
-define(CS_TARGET_COOKIE, rcs).
-define(CS_TARGET_NODE_NAME(DU), lta("sirpa@" ++ atl(DU))).

-record(cth_state, {opts, erlnode = [], first_suite = true}).

%%--------------------------------------------------------------------
%% @doc
%% Load OTP cover.erl on target node and add the node to current cover test.
%% @end
%%--------------------------------------------------------------------
init(_Id, Opts) ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ErlNode = init_sim_cover(Opts),
	    ct:pal("Started rct_cover_tgt~nSim Erlang node: ~p~nOpts = ~p", 
		   [ErlNode, Opts]),
	    {ok, #cth_state{opts = Opts, erlnode = ErlNode}};
	_Target ->
	    ErlNode = init_target_cover(Opts),
	    ct:pal("Started rct_cover_tgt~nTarget Erlang node: ~p~nOpts = ~p", 
		   [ErlNode, Opts]),
	    {ok, #cth_state{opts = Opts, erlnode = ErlNode}}
    end.


%%===========================================================================
%% @doc Add the target erlang node to ct_cover
%% @end
%%===========================================================================
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};

pre_init_per_suite(_Suite, Config, #cth_state{erlnode = ErlNode} = State) 
  when ErlNode =/= [] ->
    ct:pal("Add Erlang node ~p to ct_cover", [ErlNode]),
    {ok, _} = ct_cover:add_nodes([ErlNode]),
    {Config, State};

%% pre_init_per_suite(_Suite, Config, #cth_state{erlnode = ErlNode} = State) 
%%   when State#cth_state.first_suite =:= true, ErlNode =/= [] ->
%%     ct:pal("Add Erlang node ~p to ct_cover", [ErlNode]),
%%     {ok, _} = ct_cover:add_nodes([ErlNode]),
%%     {Config, State#cth_state{first_suite = false}};

pre_init_per_suite(_Suite, Config, State) ->
    {Config, State}.

%%===========================================================================
%% @doc Remove the target erlang node to ct_cover
%% @end
%%===========================================================================
post_end_per_suite(_SUITE, _Config, Return, State) ->
    case State#cth_state.erlnode of
	[] ->
	    ok;
	ErlNode ->
	    ct:pal("Remove Erlang node ~p from ct_cover", [ErlNode]),
	    ok = ct_cover:remove_nodes([ErlNode])
    end,
    {Return, State}.

%%--------------------------------------------------------------------
%% @doc
%% Purge OTP cover.erl on target node and remove the node from ct_cover.
%% @end
%%--------------------------------------------------------------------
terminate(State) when State#cth_state.erlnode =:= [] ->
    ok;

terminate(#cth_state{erlnode = ErlNode}) ->
    ct:pal("Terminating rct_cover_tgt for node ~p", [ErlNode]),
    rpc:call(ErlNode, code, delete, [cover]),
    rpc:call(ErlNode, code, purge, [cover]),
    ok.
    

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
init_target_cover(Opts) ->
    DUNo = proplists:get_value(targ_du_no, Opts, 1),
    case init_cs_target_env(DUNo) of
	{ok, ErlNode} ->
	    {Module, Binary, Filename} = code:get_object_code(cover),
	    {module, cover} = rpc:call(ErlNode, code, load_binary, 
				       [Module, Filename, Binary]),
	    ErlNode;
	_ ->
	    []
    end.


init_cs_target_env(DUNo) when is_integer(DUNo) ->
    try
	Site = get_config({test_nodes, DUNo}),
	IpStr = get_config({Site, erl_dist_ip}),
	IpTokens = string:tokens(IpStr, "."),
	IP = list_to_tuple([list_to_integer(I) || I <- IpTokens]),
	TargetDU = get_du(DUNo),
	inet_db:set_lookup([file, native]),
	inet_db:add_host(IP, [atom_to_list(TargetDU)]),
	CSNode = ?CS_TARGET_NODE_NAME(TargetDU),
	erlang:set_cookie(CSNode, ?CS_TARGET_COOKIE),
	ok = wait_testnode(CSNode),
	{ok, CSNode}
    catch 
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(ligthred, "init target env failed: ~p~n~p~n", [Error, ST]),
	    {error, Error}
    end.


init_sim_cover(Opts) ->
    ErlNode = get_sim_node_name(Opts),
    %% erlang:set_cookie(TNode, Cookie),
    case wait_testnode(ErlNode) of
	ok ->
	    ErlNode;
	Error ->
	    ct:log(ligthred, "init sim env failed: ~p", [Error]),
	    []
    end.
   

get_sim_node_name(Opts) ->
    case proplists:get_value(sim_sname, Opts) of
	undefined ->
	    sim_node(os:getenv("USER"));
	Name ->
	    sim_node(Name)
    end.


sim_node(Name) when is_list(Name), Name =/= [] ->
    [_, Host] = string:tokens(atl(node()), "@"),
    list_to_atom(atl(Name) ++ "@" ++ Host).


wait_testnode(TNode) ->
    wait_testnode_up(TNode).


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


get_du(N) when is_integer(N) ->
    lta(?TARGET_DEFAULT_HOST_PREFIX ++ integer_to_list(N)).


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


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) ->
    A.

atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) ->
    L.



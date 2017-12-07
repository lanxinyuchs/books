%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_proxy.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R10A/R11A/4
%%%
%%% @doc ==Common Test hook for sending and receiving messages to/from a C application ift_app==
%%%
%%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each testcases.<br/>
%%%
%%% ift_app (InterFace Test) is our C-application for use of test legacy C interface. It starts up using dynamically assign port<br/>
%%% application for target: $RCT_TOP/IFT/IFT_CXC1735023/ift/priv/tgt_powerpc/bin/ift_app
%%%
%%% Hook formats:
%%%    ``` [{ct_hooks, [{rct_proxy,[{N, Name, IPType}]}]}.'''
%%%
%%% There are short formats when running towards single node:
%%% ```{rct_proxy, Name}       expands to {rct_proxy,[{1, Name, ssh_lmt_ipv4}]}.'''
%%% 
%%% There are short formats when running towards clustered nodes:
%%% ```{rct_proxy, [Name1,Name2]}       expands to {rct_proxy,[{1, Name1, ssh_lmt_ipv4}{2, Name2, ssh_lmt_ipv4}]}.'''
%%%
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simulated environment.
%%%    Name     = atom()                         Used as identifier
%%%    IPType   = ssh_lmt_ipv4 | ssh_lmt_ipv6    Used in target env to specify which IP address cli uses.
%%%                                              Requires config variables below to be specified in stp.cfg file:
%%%                                              {ssh_lmt_ipv4, [{ssh, string()}]},
%%%                                              {ssh_lmt_ipv6, [{ssh, string()}]},'''
%%%
%%% Before each Suite `init' and `pre_init_per_suite' will:<br/>
%%% - set up cookie to CS erl node and to C-application, to be able to do rpc call to CS,<br/>
%%%   and comunicate with ift_app. To be able to Comunicate with ift_app the app starts up using, <br/>
%%%   a TCP socket.
%%% After each Suite `terminate' will:<br/>
%%% - Close . <br/>
%%%
%%% Example single node:
%%% ```suite() ->
%%%        [{ct_hooks, [{rct_proxy, node1}]}].'''
%%% Will assume following erlang nodes
%%% ```Type    Erl node                Cookie 
%%%    target  ift_app_sirpa@du1       ift_app  For IFT_APP erlang communication
%%%    sim     ift_app_$USER@hostname  ift_app  For IFT_APP erlang communication'''   
%%% Example clustered node:    
%%% ```suite() ->
%%%        [{ct_hooks, [{rct_proxy, [node1, node2]}]}].'''
%%% Will assume following erlang nodes
%%% ```Type    Erl node                    Cookie 
%%%    target  ift_app_sirpa@du1           ift_app  For IFT_APP erlang communication
%%%    target  ift_app_sirpa@du2           ift_app  For IFT_APP erlang communication
%%%    sim     ift_app_du1_$USER@hostname  ift_app  For IFT_APP erlang communication       
%%%    sim     ift_app_du2_$USER@hostname  ift_app  For IFT_APP erlang communication'''       
%%%            
%%% @end

-module(rct_proxy).
-vsn('/main/R1A/R2A/R3A/R4A/R10A/R11A/4').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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

%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev         Date         Name        What
%%% -----       ---------    --------    ------------------------
%%% R1A/6       2012-09-03   etxivri     Change due to ift_app is used instead of test_proxy.
%%%                                      ift_app is included in CXS and therefor automatic loaded
%%%                                      and started on targ and sim.
%%%                                      Note! in sim, multinodes will not be working.
%%% R2A/5       2013-03-04   etxkols     Fixed cookie collision with rct_rpc and netconf.
%%% R2A/6       2013-03-13   etxkols     Sim fix.
%%% R2A/7       2013-03-18   etxkols     if init_per_suite fails, it seems that post_end_per_suite is not run
%%% R2A/8       2013-09-27   etxjovp     add sleep 10s in exit_master. Temporary solution!!
%%% R2A/10      2013-10-30   etxkols     added net_kernel:disconnect(ErlNode) in terminate
%%% R2A/13      2014-03-21   eolaand     Make net_kernel:disconnect(ErlNode) 
%%%                                      configurable.
%%% R3A/1       2015-04-17   erarafo     Fixed copyright complaint
%%% R4A/1       2015-05-28   etxkols     Cluster fixes
%%% R4A/2       2015-11-25   erarafo     Ping with timeout used
%%% R4A/3       2016-03-23   etxkols     Cloud
%%% R4A/4       2016-05-19   etxivri     Increase TIMEOUT.
%%% R4A/5       2016-08-19   etxkols     Git migration requires that CC paths is not used 
%%% R11A/1      2016-08-22   eolaand     Add optional selective receive after
%%%                                      send_proxy
%%% R11A/2      2016-08-24   eolaand     Add selective_receive_proxy
%%% R11A/3      2016-09-29   eolaand     Make restart at fail optional
%%% R11A/4      2017-10-19   etxkols     Support rcs-sim in cloud env
%%% -----------------------------------------------------------------------------
-export([erl_node_loop/1]).
-export([start_proxy/3, start_proxy/4]).
-export([send_proxy/3, send_proxy/4, send_proxy/5, send_proxy/6]).
-export([receive_proxy/0, receive_proxy/1]).
-export([selective_receive_proxy/0, 
	 selective_receive_proxy/1, 
	 selective_receive_proxy/2]).
-export([stop_proxy/2, stop_proxy/3]).
-export([exit_master/1, exit_master/2]).
-export([is_app_running/1]).
-export([ping_ift_app_with_timeout/2]).
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).
-export([on_tc_fail/3]).
-export([terminate/1]).
-export([version/0]).

-include_lib("kernel/include/file.hrl").

%-define(TargCookie, rcs).
%-define(SimCookie, 'RCS').
-define(IftAppCookie, ift_app).
-define(TargetDefaultUser,"sirpa").
%-define(TargetDefaultHostName, du1).
-define(IftAppDefaultUser,"ift_app").
-define(APP_NAME, "ift_app").
-define(TIMEOUT, 60000).
-define(CLOUD_HOSTNAME,"vrcs").
-define(CLOUD_SIM_HOSTNAME,"rcs-sim").

-record(cth_state, {node_data, opts}).

%% @hidden
start_proxy(Node, Name, Prot) ->
    start_proxy(Node, Name, Prot, ?TIMEOUT).
%% @spec start_proxy(Node, Name, Prot, Timeout) -> Msg | {error, timeout}
%% @doc rpc:call to appmServer:get_apps, to check that ift_app is started.<br/>
start_proxy(Node, Name, Prot, Timeout) ->
    {ok, CsNodeName} = get_CsErlNode(Node),
    Tries = 6,
    check_app(Tries, CsNodeName, Timeout),
    {ok, Erlnode} = get_erlnode(Node),
    {start, Erlnode} ! {self(), Name, Prot},
    wait_any(Timeout).

check_app(TriesLeft, CsNodeName, Timeout) ->
    case check_app(CsNodeName, Timeout) of
	ok ->
	    ok;
	{error, Reason} when TriesLeft =:= 0 ->
	    ct:fail(Reason);
	{error, Reason} ->
	    ct:pal("rct_proxy:check_app() failed. Reason: ~p "
		   "Tries left: ~p", [Reason, TriesLeft]),
	    timer:sleep(timer:seconds(10)),
	    check_app(TriesLeft-1, CsNodeName, Timeout)
    end.

check_app(CsNodeName, Timeout) ->
    case rpc:call(CsNodeName, appmServer, get_apps, [], Timeout) of
	{badrpc, nodedown} ->
	    {error, "no connection to erlang node"};
	AppProplist ->
	    case proplists:lookup(?APP_NAME, AppProplist) of
		{?APP_NAME, _} ->
		    ok;
		_ ->
		    ct:pal("AppProplist = ~p", [AppProplist]),
		    {error, "test app has not started"}
	    end
    end.


restart_ift_app() ->
    AppList = ets:tab2list(?MODULE),
    %% Best effort for now. No check of result.
    F1 = fun({_, {_IFTApp, CsNode}}) ->
		 rpc:call(CsNode, appmServer, start_lm, [?APP_NAME], ?TIMEOUT)
	 end,
    F2 = fun({_, {IFTApp, _CsNode}}) ->
		 ping_ift_app_with_timeout(IFTApp, 10000)
	 end,
    lists:foreach(F1, AppList),
    timer:sleep(3000),
    lists:foreach(F2, AppList).


%% @doc Sends a "ping" message to the ift_app instance.
%% The returned value is 'ok' or an error tuple in case
%% of timeout, indicating that the ift_app is likely to
%% have severe problems.
%% @end
-spec ping_ift_app_with_timeout(atom(), non_neg_integer()) -> 
	  ok | {error, {failed_to_ping, atom()}}.

ping_ift_app_with_timeout(ErlNode, Timeout) ->
    {master_ping, ErlNode} ! {self()},
    receive
	{ok, master_pong} ->
	    ok
    after Timeout ->
	    {error, {failed_to_ping, ErlNode}}
    end.


%% @hidden
send_proxy(Node, Name, Func) ->
    send_proxy(Node, Name, Func, {}, ?TIMEOUT).
%% @hidden
send_proxy(Node, Name, Func, Timeout) when is_integer(Timeout)->
    send_proxy(Node, Name, Func, {}, Timeout);
%% @hidden
send_proxy(Node, Name, Func, Args) when is_tuple(Args)->
    send_proxy(Node, Name, Func, Args, ?TIMEOUT).
%% @spec send_proxy(Node, Name, Func, Args, Timeout) -> Msg | {error, timeout}
%% @doc Send message to ift_app.<br/>
send_proxy(Node, Name, Func, Args, Timeout) ->
    send_proxy(Node, Name, Func, Args, Timeout, true).


%% @hidden
send_proxy(Node, Name, Func, Args, Timeout, ReceiveAny) ->
    {ok, Erlnode} = get_erlnode(Node),
    %%ct:pal("Send to ~p, Message: ~p, ~p, ~p, ~p",[Erlnode, self(),Name,Func,Args]),
    {send, Erlnode} ! {self(), Name, Func, Args},
    if
	ReceiveAny ->
	    wait_any(Timeout);
	true ->
	    wait(Timeout)
    end.


%% @hidden
receive_proxy() ->
    receive_proxy(?TIMEOUT).
%% @spec receive_proxy(Timeout) -> {ok, Msg} | {error, Msg}
%% @doc  message from ift_app.<br/>
receive_proxy(Timeout) ->
    receive
    	{signal, Msg} ->
	    %%ct:pal("Rec: ~p", [Msg]),
    	    {ok, Msg};
    	Msg ->
    	    {error, Msg}
    after Timeout ->
    	    {error, timeout}
    end.


%% @hidden
selective_receive_proxy() ->
    selective_receive_proxy(?TIMEOUT).


selective_receive_proxy(Timeout) ->
    receive
    	{signal, Msg} ->
    	    {ok, Msg}
    after Timeout ->
    	    {error, timeout}
    end.


selective_receive_proxy(Msg, Timeout) ->
    receive
    	{signal, Msg} ->
    	    {ok, Msg}
    after Timeout ->
    	    {error, timeout}
    end.


%% @hidden
stop_proxy(Node, Name) ->
    stop_proxy(Node, Name, ?TIMEOUT).
%% @spec stop_proxy(Node, Name, Timeout) -> Msg | {error, timeout}
%% @doc send stop signal to ift_app.<br/>
stop_proxy(Node, Name, Timeout) ->
    {ok, Erlnode} = get_erlnode(Node),
    {stop, Erlnode} ! {self(), Name},
    wait_any(Timeout).


%% @hidden
exit_master(Node) ->
    exit_master(Node, ?TIMEOUT).
%% @spec exit_master(Node, Timeout) -> ok
%% @doc rpc:call to appmServer:activate, which result in application restarts and the TCP port is released.<br/>
exit_master(Node, Timeout) ->
    {ok, CsNodeName} = get_CsErlNode(Node),
    %% Stop the process and start again. Port will be free again.
    ok = rpc:call(CsNodeName, appmServer, activate, [], Timeout),
    %% NOTE! temporary solution
    timer:sleep(timer:seconds(10)). % sleep > 6s

%% @spec is_app_running(Node) -> boolean()
%% @doc Checks if the ift_app is running
is_app_running(Node) ->
    {ok, CsNodeName} = get_CsErlNode(Node),

    Timeout = ?TIMEOUT,
    case rpc:call(CsNodeName, appmServer, get_apps, [], Timeout) of
	{badrpc, _Reason} ->
	    false;
	AppProplist ->
	    proplists:lookup(?APP_NAME, AppProplist) =/= none
    end.

%% @spec init(_Id, Opts) -> {ok, CTHState}
%% @doc Set cookies to CS node and ift_app.<br/>
init(_Id, Opts) ->
    ct:pal("init rct_proxy", []),
    NewOpts = convert_cth_opts(Opts),
    NodeData = proplists:get_value(node_data, NewOpts),
    HookOpts = add_cfg_opts(proplists:get_value(opts, NewOpts, [])),
    {ok, #cth_state{node_data = NodeData, opts = HookOpts}}.


%% @spec pre_init_per_suite(_Suite, Config, States) -> ok
%% @doc Build up a etstable with node data.<br/>
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, State) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, State};
pre_init_per_suite(_Suite, Config, State) ->
    %% ct:pal("~p: pre_init_per_suite/3", [?MODULE]),
    SuiteType = case length(State#cth_state.node_data) > 1 of
    		    true  -> clustered;
    		    false -> single
    		end,
    Target = os:getenv("SIM_OR_TARGET"),
    case do_pre_init_per_suite(State#cth_state.node_data, [], Target, SuiteType, 1) of
	{ok, Proplists} ->
	    register(?MODULE, spawn(?MODULE, erl_node_loop, [Proplists])),
	    {Config, State};
	Other ->
	    {Other, State}
    end.

%% @spec do_pre_init_per_suite(States, Proplists, Target, SuiteType, Num) -> ok
%% @doc construct data to be used for comunicate with ift_app.<br/>
do_pre_init_per_suite([], Proplists, _Target, _SuiteType, _Num) ->
    try
	lists:foreach(fun({_, {IFT, _CS}}) ->
			      case ping_ift_app_with_timeout(IFT, 30000) of
				  ok ->
				      ok;
				  Error ->
				      throw(Error)
			      end
		      end, Proplists),
	{ok, Proplists}
    catch throw:E ->
	    {fail, E}
    end;
do_pre_init_per_suite([{N, Name, SSHType, _Hostname, _Sname}|T], Proplists, Target, SuiteType, Num) -> % Kept for backward compability
    do_pre_init_per_suite([{N, Name, SSHType}|T], Proplists, Target, SuiteType, Num);
do_pre_init_per_suite([Name|T], Proplists, Target, SuiteType, Num) when is_atom(Name) -> 
    do_pre_init_per_suite([{Num, Name, ssh_lmt_ipv4}|T], Proplists, Target, SuiteType, Num);
do_pre_init_per_suite([{N, Name, _SSHType}|T], Proplists, "sim", SuiteType, Num) ->
    ErlNode = rct_cluster_lib:get_sim_erl_node(N,SuiteType),
    IFTAppNode = list_to_atom(?IftAppDefaultUser ++ "_"++ atom_to_list(ErlNode)),
    NewPropList = Proplists ++ [{Name, {IFTAppNode, ErlNode}}],
    erlang:set_cookie(IFTAppNode, ?IftAppCookie),
    do_pre_init_per_suite(T, NewPropList, "sim", SuiteType, Num + 1);
do_pre_init_per_suite([{N, Name, _SSHType}|T], Proplists, Target, SuiteType, Num) ->
    case ct:get_config({test_nodes,N}) of
	undefined ->
	    ct:log(lightred,
		   "~p: ~p ~p Could not read config parameter ~p for rpc, Reason: undefined",
		   [Name, ?MODULE, pre_init_per_suite, {test_nodes,N}]),
	    {fail, {undefined,{test_nodes,N}}};
	Site ->
	    case ct:get_config({Site,erl_dist_ip}) of
		undefined ->
		    ct:log(lightred,
			   "~p: ~p ~p Could not read config parameter ~p for rpc, Reason: undefined",
			   [Name, ?MODULE, pre_init_per_suite, {Site,erl_dist_ip}]),
		    {fail, {undefined,{Site,erl_dist_ip}}};
		    %% {{fail, {undefined,{Site,erl_dist_ip}}}, [Name]};
		IPStr ->
		    IP = list_to_tuple([list_to_integer(I)||I<-string:tokens(IPStr,".")]),
		    {Host,ErlNode} = case Target of
					 "target" ->
					     DU="du" ++ integer_to_list(rct_cluster_lib:get_target_du(N,SuiteType)),
					     {DU, list_to_atom(?TargetDefaultUser ++ "@" ++ DU)};
					 "cloudish" ->
					     case re:run(atom_to_list(ct:get_config({test_nodes,N})),"^sim") of
						 {match,_} -> {?CLOUD_SIM_HOSTNAME,list_to_atom("sirpa@"++?CLOUD_SIM_HOSTNAME)};
						 nomatch   -> {?CLOUD_HOSTNAME,list_to_atom("root@"++?CLOUD_HOSTNAME)}
					     end
%					     {?CLOUD_HOSTNAME,list_to_atom("root@"++?CLOUD_HOSTNAME)}
				   end,
		    IFTAppNode = list_to_atom(?IftAppDefaultUser ++ "_" ++ atom_to_list(ErlNode)),
		    inet_db:set_lookup([file,native]),
		    inet_db:add_host(IP, [Host]),
		    erlang:set_cookie(IFTAppNode, ?IftAppCookie),
		    NewPropList = Proplists ++ [{Name,{IFTAppNode,ErlNode}}],
		    do_pre_init_per_suite(T, NewPropList, Target, SuiteType, Num + 1)
	    end
    end.


%% @hidden
post_end_per_suite(_SuiteName, _Config, Return, CTHState) ->
    %% ct:pal("~p: post_end_per_suite/3", [?MODULE]),
    restart_ift_app(),
    {Return, CTHState}.


%% @hidden
on_tc_fail(_TC, _Reason, CTHState) ->
    proplists:get_value(restart_on_fail, CTHState#cth_state.opts, true)
	andalso restart_ift_app(),
    CTHState.


%% @hidden
terminate(State) ->
    ct:pal("terminate rct_proxy", []),
    %% ct:pal("ets:tab2list(?MODULE) ~p",[ets:tab2list(?MODULE)]),
    case proplists:get_value(rct_disconnect_nodes, State#cth_state.opts) of
	false ->
	    ok;
	_True ->
	    [net_kernel:disconnect(ErlNode) || 
		ErlNode <- lists:flatten([tuple_to_list(ErlNodes) || 
					     {_,ErlNodes} 
						 <- ets:tab2list(?MODULE)])]
    end,
    stop_erl_node_loop().


stop_erl_node_loop() ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    ?MODULE ! {self(), stop},
	    receive 
		{Pid, stopped} ->
		    ok
	    after 10000 ->
		    error
	    end;
	_ ->
	    ok
    end.


%% @doc Returns the ClearCase version as an atom.
%% @end
-spec version() -> atom().

version() ->
    case code:which(?MODULE) of
	Path when is_list(Path) ->
	    case beam_lib:version(Path) of
		{error, _, _} ->
		    'unknown';
		{ok, {_, [Version]}} ->
		    Version
	    end;
	_ ->
	    'unknown'
    end.

    
%% @hidden
wait(Timeout) ->
    receive
	{Tag, _Res} = Msg when Tag =:= ok; 
			       Tag =:= error ->
	    Msg;
	ok ->
	    ok
    after Timeout ->
	    {error, timeout}
    end.


%% @hidden
wait_any(Timeout) ->
    receive
	Msg when element(1, Msg) /= signal ->
	    Msg
    after Timeout ->
	    {error, timeout}
    end.


%% @hidden
get_erlnode(Node) ->
    case ets:info(?MODULE) of
	undefined ->
	    {error,{?MODULE, undefined}};
	_ ->
	    case ets:lookup(?MODULE,Node) of
		[{Node, {ErlNode, _CsErlNode}}] ->
		    {ok, ErlNode};
		Other ->
		    {error, Other}
	    end
    end.

%% @hidden
get_CsErlNode(Node) ->
    case ets:info(?MODULE) of
	undefined ->
	    {error,{?MODULE, undefined}};
	_ ->
	    case ets:lookup(?MODULE,Node) of
		[{Node, {_ErlNode, CsErlNode}}] ->
		    {ok, CsErlNode};
		Other ->
		    {error, Other}
	    end
    end.

%% @hidden
%% Insert proplist to etstable
erl_node_loop(Proplists) ->
    ets:new(?MODULE, [ordered_set, public, named_table]),
    [ets:insert(?MODULE, X) || X <- Proplists],
    receive
	{From, stop} ->
	    ets:delete(?MODULE),
	    From ! {self(), stopped}
    end.

%% @hidden
convert_cth_opts([{node_data, ND}, {opts, Opts}]) when is_tuple(ND) ->
    [{node_data, [ND]}, {opts, Opts}];

convert_cth_opts([{node_data, _ND}, {opts, _Opts}] = CTHOpts) ->
    CTHOpts;

convert_cth_opts({_N, _Name, _SSHType, _Hostname, _Sname} = ND) ->
    [{node_data, [ND]}];

convert_cth_opts([{_N, _Name, _SSHType, _Hostname, _Sname} | _] = ND) ->
    [{node_data, ND}];

convert_cth_opts({_N, _Name, _SSHType} = ND) ->
    [{node_data, [ND]}];

convert_cth_opts([{_N, _Name, _SSHType} | _] = ND) ->
    [{node_data, ND}];

convert_cth_opts(ND) when is_atom(ND) ->
    [{node_data, [ND]}];

convert_cth_opts(ND) when is_list(ND) ->
    [{node_data, ND}].

%% convert_cth_opts(Opts) when is_list(Opts) ->
%%     Opts.

%% @hidden
add_cfg_opts(Opts) ->
    case proplists:get_value(rct_disconnect_nodes, Opts) of
	Val when is_boolean(Val) ->
	    Opts;
	undefined ->
	    DiscFlag = ct:get_config(rct_disconnect_nodes),
	    add_cfg_opts(DiscFlag, Opts)
    end.
	    

add_cfg_opts(Flag, Opts) when is_boolean(Flag) ->
    [{rct_disconnect_nodes, Flag} | Opts];

add_cfg_opts(_Flag, Opts) ->
    case init:get_argument(cover) of
	{ok, _} ->
	    [{rct_disconnect_nodes, false} | Opts];
	_ ->
	    [{rct_disconnect_nodes, true} | Opts]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



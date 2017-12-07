%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	rct_rpc.erl %
%% @author etxkols
%% @copyright Ericsson AB 2012-2017
%% @version /main/R1A/R2A/R3A/R4A/R8A/R9A/R10A/R11A/2
%% @doc Module to enable erlang-erlang communication between Common Test and SUT.
%% 
%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each test suite.<br/>
%% This hook constructs an ets table for mapping between `Name(s)' and the erlang node name(s) of the SUT(s).<br/> 
%% Environment variabel SIM_OR_TARGET ("sim" | "target") defines if test is run on target or in simulated environment, hence erlang node name(s) are constructed in different ways.<br/>
%% If SIM_OR_TARGET="target", a config file containing parameter `erl_dist_ip' is required to define IP address of SUT(s).<br/>
%% The ct_hook can be constructed in several ways depending on if tests are carried out towards one or several SUT(s).<br/>
%% ```suite() -> 
%%        [{ct_hooks, [{rct_rpc, Name = atom() | [{No = integer(), Name = atom()}]}]}].'''
%% `Name' is alias for erlang node name of SUT.<br/>
%% `No' points out which SUT in config file that `Name' references. Also used to build hostname, i.e. du[No]<br/>
%% 
%% The module implements a few function for easy communication with erlang on SUT.<br/>
%% Compared to <a href="http://www.erlang.org/doc/man/ct_rpc.html">ct_rpc</a>, this module exports a few function where nodename and cookie does not have to be given.
%% 
%% Before each suite `pre_init_per_suite' will:<br/>
%% - if SIM_OR_TARGET="target", existence of config parameter `erl_dist_ip' is verified.<br/>
%% - Store `Name(s)' as aliases for erlang node names(s) in ets table.<br/>
%% After each suite `post_end_per_suite' will:<br/>
%% - Delete ets table with erlang node names.
%%
%% Examples:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_rpc,node1}]}]. is short for [{ct_hooks, [{rct_rpc,[{1,node1}]}]}].
%%
%%    Target erlang node name:             sirpa@du1                 (cookie rcs)
%%    rct_run.sh -stp kalle:               kalle@esekilxxen456       (cookie ~/.erlang.cookie)  use node1 to call sirpa@du1
%%
%%    One rcssim per user on Terminal Server or VDI
%%    rcssim:                              etxkols@esekilxxen456     (cookie ~/.erlang.cookie) 
%%    rct_run.sh -sim kalle:               kalle@esekilxxen456       (cookie ~/.erlang.cookie)  use node1 to call etxkols@esekilxxen456
%%
%%    Multi rcssim on VDI (Jenkins) 
%%    rcssim -E xyz -u:                    rcsci1@esekilxxen456      (cookie ~/.erlang.cookie)
%%    rct_run.sh -sim kalle -E xyz         kalle@esekilxxen456       (cookie ~/.erlang.cookie)  use node1 to call du1_sim006@esekilxxen456'''
%%
%% ```suite() -> 
%%        [{ct_hooks, [{rct_rpc,[node1,node2]}]}]. is short for [{ct_hooks, [{rct_rpc,[{1,node1},{2,node2}]}]}]
%%
%%    Target erlang node names:            sirpa@du1,sirpa@du2       (cookie rcs)
%%    rct_run.sh -stp kalle:               kalle@esekilxxen456       (cookie~/.erlang.cookie )  use node1 to call sirpa@du1
%%                                                                                              use node2 to call sirpa@du2'''
%%
%% ```testcase(_) ->
%%        [{sys,"System application for RCS","1.0"}|_] = rct_rpc:call(node1,application,which_applications,[],10000).'''
%% @end
-module(rct_rpc).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-03-08 etxkols     Created
%%% R1A/2      2012-06-12 etxkols     Changed target erlnode to sirpa@du1
%%% R1A/3      2012-10-02 etxivri     Corrected return value from init().
%%%                                   the execution order start in seq of the ct_hooks list.
%%% R1A/3      2012-10-16 etxkols     Added call/6.
%%% R1A/4      2012-10-31 etxkols     Removed red printout when {badrpc,_}.
%%% R1A/5      2012-10-31 etxkols     Removed red printout when {badrpc,_}.
%%%                                   {No, [{Name, HostOrUserName}]}
%%% R1A/6      2012-12-13 etxkols     Disconnect erlang distribution in
%%%                                   post_end_per_suite
%%% R2A/5      2013-03-18 etxkols     Support for changing cookies
%%% R2A/6      2014-03-21 eolaand     Make disconnection of erlang dist
%%%                                   configurable.
%%% R3A/1      2014-11-19 erarafo     Typos fixed.
%%% R3A/2      2015-01-20 etxkols     Removed support for testapp and added
%%%                                   support for cluster
%%% R3A/3      2015-02-12 etxkols     Replace post_end_per_suite/4 with terminate/1
%%%                                   Fix for cookie towards target
%%% R3A/5      2015-02-16 etxberb     Fixed edoc problem for terminate/1.
%%% R3A/6      2015-03-10 etxkols     Removed support for testapp and added
%%%                                   support for cluster
%%% R4A/1      2015-06-03 etxkols     Cluster fixes
%%% R4A/2      2015-10-13 etxkols     More cluster fixes
%%% R4A/3      2016-03-14 etxkols     5G
%%% R4A/5      2016-03-23 etxkols     5G
%%% R8A/1      2016-11-08 etxkols     Added xcall function
%%% R9A/1      2017-01-31 etxkols     Refactoring and 5g suppport
%%% R9A/2      2017-02-01 etxkols     Removed comments
%%% R9A/3      2017-02-03 etxkols     Removed printout
%%% R9A/4      2017-02-09 etxkols     Fix when cloud instance is down at start of testsuite
%%% R10A/1     2017-05-09 etxkols     Type = sd (service discovery) added
%%% R11A/1     2017-08-18 etxkols     terminate/1 ets table fix
%%% R11A/2     2017-10-06 etxkols     Support rcs-sim in cloud env (GIT preparation)
%%% ----------------------------------------------------------
-export([init/2]).
-export([call/5]).
-export([call/6]).
-export([xcall/5]).
-export([xcall/6]).
-export([get_erlnode/1]).
-export([pre_init_per_suite/3]).
-export([terminate/1]).
-export([update_config/2]).

-define(TARGET_USER,"sirpa").
-define(TARGET_HOST,"du1").
-define(TARGET_COOKIE,rcs).
-define(CLOUD_USER,"root").
-define(CLOUD_HOST,"vrcs").
-define(CLOUD_SIM_USER,"sirpa").
-define(CLOUD_SIM_HOST,"rcs-sim").

call(Node, Module, Function, Args, TimeOut) ->
    call(Node, Module, Function, Args, TimeOut, print).
%% @spec call(Node, Module, Function, Args, TimeOut, Print) -> Reply | {error, Reason} | {badrpc, Reason}
%% Node = atom()
%% Reply = term()
%% Reason = term()
%% Print = print | noprint
%% @doc Resolves erlang node name and makes <a href="http://www.erlang.org/doc/man/rpc.html#call-5">rpc:call/5</a> to SUT.<br/>
%% If `Print = noprint', no printouts are made to CT htmllog except for failure.<br/>
%% If `Print = print(default)', progress printouts are made to CT htmllog. This equals call/5.<br/>
%% If connect fails, result will be printed in background color red.<br/>
call(Node, Module, Function, Args, TimeOut, Print) ->
    do_call(no_sethostip, Node, Module, Function, Args, TimeOut, Print).

xcall(Node, Module, Function, Args, TimeOut) ->
    xcall(Node, Module, Function, Args, TimeOut, print).
%% @spec xcall(Node, Module, Function, Args, TimeOut, Print) -> Reply | {error, Reason} | {badrpc, Reason}
%% @doc Sets target hostname IP address before doing rct_rpc:call.
%%
%% When testing towards two target nodes with same hostname (ex vrcs), the IP address for the wanted node needs to be set before rpc:call is made.
xcall(Node, Module, Function, Args, TimeOut, Print) ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    {error, xcall_not_supported_in_sim_env};
	_ ->
	    do_call(sethostip, Node, Module, Function, Args, TimeOut, Print)
    end.

do_call(SetHostIP, Node, Module, Function, Args, TimeOut, Print) ->
    case ct:get_config(make_name_module(Node)) of
	[IP, Host, ErlNode] ->
	    case SetHostIP of
		sethostip -> % When 2 or more nodes have the same hostname, the IP address must be set before rpc:call is made. Ex upgrade in 5G cloud
		    update_inet_db(IP, Host, ErlNode);
		    %% [IP, Host] = ct:get_config(make_name_module(Node)),
		    %% %% [{10,67,239,248},"vrcs"]
		    %% net_kernel:disconnect(ErlNode),
		    %% [inet_db:del_host(IP2)||{Host2, inet, IP2} <- ets:tab2list(inet_hosts_byname), Host2 =:= Host],
		    %% inet_db:set_lookup([file,native]),
		    %% inet_db:add_host(IP, [Host]);		
		_ ->
		    ok
	    end,
	    case Print of
		print -> ct:log(ct_internal,"~p: rpc:call(~p, ~p, ~p, ~p, ~p).",[Node, ErlNode, Module, Function, Args, TimeOut]);
		_ -> ok
	    end,
	    case rpc:call(ErlNode, Module, Function, Args, TimeOut) of
		{badrpc, Reason} ->
		    ct:log(white,"~p: ~p",[Node, {badrpc, Reason}]),
		    {badrpc, Reason};
		Reply -> 
		    case Print of
			print -> ct:log(white,"~p: ~p",[Node, Reply]);
			_ -> ok
		    end,			
		    Reply
	    end;
	Other ->
	    ct:log(lightred,"~p: ~p",[Node, Other])
    end.

update_inet_db(IP, Host, ErlNode) ->
    RC = inet_db:get_rc(),
    case lists:member({host,IP,[Host]}, RC) of
	true -> %% IP address already in inet_db -> no need for update
	    ok;
	false ->
	    {value,{host,IP2,_}} = lists:keysearch([Host],3,RC),
	    net_kernel:disconnect(ErlNode),
	    inet_db:del_host(IP2),
	    inet_db:set_lookup([file,native]),
	    inet_db:add_host(IP, [Host])
    end.

%% @spec get_erlnode(Node) -> {ok, Erlnode} | {error,Reason}
%% Node = atom()
%% Erlnode = term()
%% Reason = term()
%% @doc Resolves erlang nodename from Node <br/>
get_erlnode(Node) ->
    case ct:get_config(make_name_module(Node)) of
	[_IP,_Host,ErlNode] ->
	    {ok, ErlNode};
	Other ->
	    Other
    end.

%% @hidden
init(_Id, Opts) ->
   {ok, Opts}.

%% @spec pre_init_per_suite(TC,Config,States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Creates and stores erlang node names in an ets table.<br/>
pre_init_per_suite(_Suite,Config = {fail,_},States) -> {Config,States};
pre_init_per_suite(_Suite,Config = {skip,_},States) -> {Config,States};
pre_init_per_suite(_Suite,Config,Name) when is_atom(Name) ->
    pre_init_per_suite(_Suite,Config,[Name]);
pre_init_per_suite(_Suite,Config,States) ->    
    case do_pre_init_per_suite(States,[],1) of
    	{ok,AliasToHooks} ->
	    NewConfig = case proplists:get_value(alias_to_hooks,Config) of
			    undefined ->
				Config ++ [{alias_to_hooks,AliasToHooks}];
			    OldAliasToHooks ->
				lists:keyreplace(alias_to_hooks,1,Config,{alias_to_hooks,OldAliasToHooks ++ AliasToHooks})
			end,	    
    	    {NewConfig, States};
    	Other ->
    	    Other
    end.

fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.

do_pre_init_per_suite([],AliasToHooks,_) ->
    {ok,AliasToHooks};
do_pre_init_per_suite([Name|T],AliasToHooks,Num) when is_atom(Name)->
    do_pre_init_per_suite([{Num,Name}|T],AliasToHooks,Num);
do_pre_init_per_suite([{N,Name}|T],AliasToHooks,Num) -> 
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    UserName = os:getenv("USER"),
	    {ok, Host} = inet:gethostname(),     
	    ErlNode = UserName ++ "@" ++ Host,
	    rct_multi_node_cfg:require(make_name_module(Name), [sim,Host,list_to_atom(ErlNode)]),
	    do_pre_init_per_suite(T,AliasToHooks ++ [{Name,{N,?MODULE}}],Num + 1);
	TargetOrCloudish ->
	    case get_user_host(N,Name,TargetOrCloudish) of
		{fail, Reason} ->
		    {fail, Reason};
		{User, Host} ->
		    case store_erlang_node_name(N, Name, User, Host) of
			{fail, Reason} ->
			    {fail, Reason};
			{IP,Host,ErlNode} ->
			    rct_multi_node_cfg:require(make_name_module(Name), [IP,Host,list_to_atom(ErlNode)]),
			    do_pre_init_per_suite(T,AliasToHooks ++ [{Name,{N, ?MODULE}}],Num + 1)
		    end
	    end
    end.
			
store_erlang_node_name(N, Name, User, Host) ->    
    case rct_multi_node_cfg:get_config(N,erl_dist_ip) of
	undefined ->
	    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[erl_dist_ip,Name,N]});
	[] ->
	    case rct_node_state:get_state(N) of
                down ->
		    ErlNode = User ++ "@" ++ Host,
		    {"no_erl_dist_ip",Host,ErlNode};
                _ ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p and node is marked as up",[erl_dist_ip,Name,N]})
	    end;
	IPstr ->
	    {ok,IP} = inet:parse_address(IPstr),
	    ErlNode = User ++ "@" ++ Host,
	    case inet:gethostbyname(Host) of
		{error,nxdomain} ->
		    inet_db:set_lookup([file,native]),
		    inet_db:add_host(IP, [Host]),
		    erlang:set_cookie(list_to_atom(ErlNode), ?TARGET_COOKIE);
		_ ->
		    ok
	    end,
	    {IP,Host,ErlNode}
    end.

%%% get_user_host(1,"target") -> {"sirpa","du1"}, {fail, Reason}
get_user_host(N,Name,TargetOrCloudish) ->
    case rct_node_state:get_type(N) of
	undefined -> % rct_node_state has NOT been used to declare the different node types in multinode suite.
	    case TargetOrCloudish of
%		"cloudish"    -> {?CLOUD_USER,?CLOUD_HOST};
		"cloudish"    -> 
		    case re:run(atom_to_list(ct:get_config({test_nodes,N})),"^sim") of
			{match,_} -> {?CLOUD_SIM_USER,?CLOUD_SIM_HOST};
			nomatch   -> {?CLOUD_USER,?CLOUD_HOST}
		    end;
		"target"      -> {?TARGET_USER,?TARGET_HOST}
	    end;
	du                    -> {?TARGET_USER,?TARGET_HOST};
	Type when Type=:=vnf; 
		  Type=:=vnfm; 
		  Type=:=sd   -> {?CLOUD_USER,?CLOUD_HOST};
	Type ->
	    fail_generic(?FUNCTION_NAME,?LINE,{"Wrong node type specified in rct_node_state hook ~p for ~p in node ~p",[Type,Name,N]})
    end.
	
%% @spec terminate(States) -> ok
%% @doc Deletes ets table with erlang node names.<br/>
terminate(States) ->
    Configs=[ct:get_config(make_name_module(Name)) || Name <- States],
    ErlNodes=[ErlNode||[_IP, _Host, ErlNode] <- Configs],
    is_cover_test() orelse
	[net_kernel:disconnect(ErlNode) || 
	    ErlNode <- ErlNodes],
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    ?MODULE ! stop;		
	_ ->
	    ok
    end.

is_cover_test() ->
    is_cover_test(ct:get_config(cover)).

is_cover_test(Bool) when is_boolean(Bool) ->
    Bool;

is_cover_test(_) ->
    case init:get_argument(cover) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

make_name_module(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(?MODULE)).

%%% @spec update_config(Name,ConfigLine) -> ok | no_configuration_found | not_affected_by_update
%%% @doc callback function for updating configuration data.
%%% ```Name = atom()                          Alias for cli or coli towards node.
%%%    ConfigLine = tuple                     {erl_dist_ip,"10.67.239.248"} '''
update_config(Name, {erl_dist_ip,IPstr}) ->
    Name2 = make_name_module(Name),
    {ok,IP} = inet:parse_address(IPstr),
    case ct:get_config(Name2) of
	undefined -> 
	    no_configuration_found;
	[_IP2, Host, ErlNode] ->
	    rct_multi_node_cfg:remove_config(Name2),
	    ok = rct_multi_node_cfg:require(Name2,[IP,Host,ErlNode]),
	    {ok,IP} = inet:parse_address(IPstr),
	    update_inet_db(IP,Host,ErlNode),
	    ok
    end;
update_config(_Name,_Config) ->
    not_affected_by_update.

%% -----------------------------------------------------------------------------
%% Copyright (c) Ericsson AB 2016 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the receiver of
%% this document shall keep the information contained herein confidential and
%% shall protect the same in whole or in part from disclosure and dissemination
%% to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall only be made
%% on a strict need to know basis.
%% ----------------------------------------------------------
%% #1.    REVISION LOG
%% ----------------------------------------------------------
%% Rev        Date       Name        What
%% -----      ---------  --------    ------------------------
%% R1A/1      2012-01-31  etxkols    Created
%% R1A/2      2012-05-24  etxkols    disconnect/1 added
%% R2A/1      2012-10-26  etxkols    Updated ssh:connect options
%% R2A/1      2012-11-30  etxkols    Added Opts [manual_connect]
%% R2A/4      2013-12-05  etxkols    Removed ct:pal
%% R2A/5      2013-12-17  etxkols    Edoc update
%% R2A/6      2014-03-26  etxkols    Faulty init/2 return value
%% R4A/1      2015-06-03  etxkols    Cluster fixes
%%% R4A/2      2016-03-09 etxkols     5G
%% -----------------------------------------------------------------------------
%% @doc Module to ssh SUT LMT (Local Maintenance Terminal).
%% 
%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each testcases.<br/>
%% ```suite() -> 
%%        [{ct_hooks, [{rct_ssh, [{N, Name, Opts}]}]}].}'''
%% There is a short formats for testing towards one node:
%% ```{rct_ssh, Name}  expands to {rct_ssh, [{1, Name, [{ip_type,ssh_lmt_ipv4}]}]}'''
%% ```{rct_ssh, {Name, Opts}} expands to {rct_ssh, [{1, Name1, Opts}]}'''
%% 
%% There is a short formats for testing towards clustered node:
%% ```{rct_ssh, [Name1,Name2]} expands to {rct_ssh, [{1, Name1, [{ip_type,ssh_lmt_ipv4}]},{2, Name2, [{ip_type,ssh_lmt_ipv4}]}]}'''
%% ```{rct_ssh, [{Name1, Opts1},{Name2, Opts2}]} expands to {rct_ssh, [{1, Name1, Opts},{2, Name2, Opts}]}'''
%% 
%% Argument description:
%% ```Name   = atom()                              Reference to a SUT
%%    Opts   = [Opt]
%%    Opt    = {ip_type, IPType} | manual_connect  manual_connect requires connection to be made inside testcase
%%    IPType = ssh_lmt_ipv4, ssh_lmt_ipv6          Default is ssh_lmt_ipv4
%%    N      = integer()                           Points to a SUT in CT configfile'''
%% For description of `Name', see <a href="http://www.erlang.org/doc/man/ct_ssh.html">ct_ssh</a><br/>
%%
%% Example single node:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_ssh,node}]}.'''
%% Example clustered nodes:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_ssh,[node1,node2]}]}].'''
%% Testcase example:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_ssh,node}]}].'''
%% ```testcase(_) ->
%%        {ok,_} = ct_ssh:exec(node,"ls -l /var/tmp/root/log/",5000),
%%        {ok,_} = rct_ssh:exec(node,"ls -l /var/tmp/root/log/",5000,"alarm").'''
%% ```suite() -> 
%%        [{ct_hooks, [{rct_ssh,{node,[manual_connect]}}]}.'''
%% ```testcase(_) ->
%%        {ok,_} = ct_ssh:connect(node),
%%        {ok,_} = ct_ssh:exec(node,"ls -l /var/tmp/root/log/",5000),
%%        {ok,_} = rct_ssh:exec(node,"ls -l /var/tmp/root/log/",5000,"alarm"),
%%        ok = ct_ssh:disconnect(node).'''
%% @end
-module(rct_ssh). 
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R4A/2').
-date('2016-03-09').
-author('etxkols').

-export([exec/4]).
-export([exec/5]).
-export([connect/1]).
-export([disconnect/1]).
-export([sftp_to_target/3]).
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).
-export([terminate/1]).
-import(ct_telnet,[start_log/1,cont_log/2,end_log/0]).
-include_lib("common_test/include/ct.hrl").

%% @spec exec(Name, Command, Timeout, RE) -> {ok,Reply} | {error,Reason}
%% @equiv exec(Name, Command, Timeout, RE, [])
exec(Name, Command, Timeout, RE) ->
    exec(Name, Command, Timeout, RE, []).

%% @spec exec(Name, Command, Timeout, RE, Options) -> {ok,Reply} | {error,Reason}
%% RE      = string()
%% Options = [ Option ]
%% Name    = atom()
%% Command = string()
%% Timeout = integer()
%% @doc Runs Command and match result<br/>
%% For description of `Name', `Command' and `Timeout', see <a href="http://www.erlang.org/doc/man/ct_ssh.html#exec-3">ct_ssh:exec/3</a><br/>
%% For description of `RE' and `Options', see <a href="http://www.erlang.org/doc/man/re.html#run-3">re:run/3</a><br/>
%% If connect or match fails, result will be printed in background color red<br/>
%% Example:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_ssh,site1}]}].
%% 
%%    testcase(_) ->
%%        {ok,R} = rct_ssh:exec(site1,"ls -l /var/tmp/root/log/",5000,"alarm"),
%%        {ok,R} = rct_ssh:exec(site1,"ls -l /var/tmp/root/log/",5000,"alarm.*audit_trail",[dotall]).'''
exec(Name, Command, Timeout, RE, Options) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
            ct:log(lightred,"~p: ~p ~p Cannot exec, Reason: NOT supported in SIMULATED environment",[Name, ?MODULE, exec]);
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    case ct_ssh:exec(Name,Command,Timeout) of
		{ok,Subject} ->
		    Result = case re:run(Subject, RE, Options) of
				 {match,_} -> 
				     ct:log(white,"~p: Match RE ~p Options ~p Reply ~p ~n~s",[Name, RE, Options, Subject, Subject]),
				     {ok, Subject};
				 nomatch ->
				     ct:log(lightred,"~p: Match RE ~p Options ~p Reply ~p ~n~s",[Name, RE, Options, Subject, Subject]),
				     {error, Subject}
			     end,
		    timer:sleep(100), % seems necessary if several execs in a row, will speak to OTP about it.
		    Result;
		{error, Reason} ->
		    ct:log(lightred,"~p: ~p",[Name, {error, Reason}]),
		    {error, Reason}
	    end
    end.

%% @spec connect(Name) -> ok | {error,Reason}
%% @doc connects ssh to SUT.<br/>
connect(Name) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
            ct:log(lightred,"~p: ~p ~p Cannot connect, Reason: NOT supported in SIMULATED environment",[Name, ?MODULE, connect]);
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    case ct_ssh:connect(Name,host,[{silently_accept_hosts, true}, {user_interaction,false}]) of
		{ok,_} ->
		    ok;
		{error, Reason}->
		    ct:log(lightred,"~p: ~p ~p Could not connect with ssh to SUT, Reason: ~p",[Name, ?MODULE, connect, {error, Reason}]),
		    {error, Reason}
	    end
    end.
    
%% @spec disconnect(Name) -> ok | {error,Reason}
%% @doc disconnects ssh from SUT.<br/>
disconnect(Name) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
            ct:log(lightred,"~p: ~p ~p Cannot disconnect, Reason: NOT supported in SIMULATED environment",[Name, ?MODULE, disconnect]);
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    case ct_ssh:disconnect(Name) of
		ok ->
		    ok;
		{error, Reason}->
		    ct:log(lightred,"~p: ~p ~p Could not disconnect ssh from SUT, Reason: ~p",[Name, ?MODULE, disconnect, {error, Reason}]),
		    {error, Reason}
	    end
    end.
    
%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
%% @spec pre_init_per_suite(TC,Config,States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Verifies existence of config parameters.<br/>
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CthState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CthState};
pre_init_per_suite(Suite,Config,States) when is_atom(States) ->
    pre_init_per_suite(Suite,Config,[States]);
pre_init_per_suite(Suite,Config,States) when is_tuple(States) ->
    pre_init_per_suite(Suite,Config,[States]);
pre_init_per_suite(_Suite,Config,States) ->
    SuiteType = case length(States) > 1 of
    		    true  -> clustered;
    		    false -> single
    		end,
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
            {Config, States};
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    States2 = do_pre_init_per_suite(States,[],SuiteType,1),
	    {Config,States2}
%            do_pre_init_per_suite(Config,States,[])
    end.

do_pre_init_per_suite([],R,_SuiteType,_Num) ->
    R;
do_pre_init_per_suite([Name|T],R,SuiteType,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name,[{ip_type,ssh_lmt_ipv4}]}|T],R,SuiteType,Num);
do_pre_init_per_suite([{Name,Opts}|T],R,SuiteType,Num) ->
    do_pre_init_per_suite([{Num,Name,Opts}|T],R,SuiteType,Num);
do_pre_init_per_suite([State = {N, Name, Opts}|T],R,SuiteType,Num) ->
    IPType = proplists:get_value(ip_type, Opts, ssh_lmt_ipv4),
    BoardNum = rct_cluster_lib:get_target_du(N,SuiteType), 
    Board = ct:get_config({test_nodes,BoardNum}),
    case ct:get_config({Board,IPType}) of
	SSH = [{ssh,_},
	       {port,_},
	       {user,_},
	       {password,_}] ->
	    rct_multi_node_cfg:require(Name,SSH),
	    do_pre_init_per_suite(T,R ++ [State],SuiteType,Num + 1 );

        Other ->
            ct:log(lightred,"~p: ~p ~p Config parameters not matching {ssh_lmt_ipv4,[ssh,port,user,password]} for ssh to SUT, Reason: ~p",[Name, ?MODULE, do_pre_init_per_suite, {error, Other}]),
            {{fail,Other }, [Name]}
    end.


%% @hidden
%% @spec pre_init_per_testcase(TC,Config,States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Verifies existence of config parameters and connects ssh to SUT.<br/>
pre_init_per_testcase(_TC,Config,States) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
            {Config, States};
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    {do_pre_init_per_testcase(Config,States), States}
    end.

do_pre_init_per_testcase(Config,[]) ->
    Config;
do_pre_init_per_testcase(Config,[{_N,Name,Opts}|T]) ->
    case proplists:get_value(manual_connect,Opts,false) of
	true ->
	    do_pre_init_per_testcase(Config,T);
	false ->
	    case connect(Name) of
		ok ->
		    do_pre_init_per_testcase(Config,T);
		{error, Reason} ->
		    ct:log(lightred,"~p: ~p ~p Could not connect with ssh to SUT, Reason: ~p",[Name, ?MODULE, post_end_per_testcase, {error, Reason}]),
		    {fail, Reason}
	    end
    end.

%% @hidden
%% @spec post_end_per_testcase(TC,Config,Return,States) -> {Return, States} | {{fail,Reason}, States}
%% @doc Disconnects ssh from SUT.<br/>
post_end_per_testcase(_TC,_Config,Return,States) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
            {Return, States};
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    {do_post_end_per_testcase(Return,States), States}
    end.

do_post_end_per_testcase(Return,[]) ->
    Return;
do_post_end_per_testcase(Return,[{_N,Name,Opts}|T]) ->
    case proplists:get_value(manual_connect,Opts,false)  of
	true ->
	    do_post_end_per_testcase(Return,T);
	false ->    
	    case ct_ssh:disconnect(Name) of
		ok  ->
		    do_post_end_per_testcase(Return,T);
		{error, Reason} ->
		    ct:log(lightred,"~p: ~p ~p Could not disconnect ssh from SUT, Reason: ~p",[Name, ?MODULE, post_end_per_testcase, {error, Reason}]),
		    {fail, Reason}
	    end
    end.

%%% @hidden
terminate(Names) ->
   case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ok;
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    do_terminate(Names)
    end.

do_terminate([]) ->
    ok;
do_terminate([{_,Name,_}|T])->
    rct_multi_node_cfg:remove_config(Name),
    do_terminate(T).

%ct:require(site1,{dus004, ssh_lmt_ipv4}).
%rct_multi_node_cfg:require(site1, 1,{ssh_lmt_ipv4, [ssh, port,user, password]}).
%ct_ssh:connect(site1, sftp,[]).
%ct_ssh:connect(site1, host,[]).
%ct_ssh:connect(site1, sftp, [{silently_accept_hosts, true}]).

sftp_to_target(Name, From, To) ->
    case file:read_file(From) of
	{ok, Binary} ->
	    case ct_ssh:connect(Name, sftp, [{silently_accept_hosts, true}, {user_interaction,false}]) of
		{ok, _} ->
%	    case ct_ssh:sftp_connect(Name) of
%		{ok, Server} ->
		    case ct_ssh:write_file(Name, To, Binary) of
                        ok ->
			    ok;
			{error, Reason} ->
                            ct:log(lightred,
                                   "~p: ~p ~p Could not write file ~s, Reason: ~p",
                                   [Name, ?MODULE, sftp_to_target, To, {error, Reason}]),
			    {error, Reason}
		    end;
		{error, Reason} ->
		    ct:log(lightred,
			   "~p: ~p ~p Could not connect with sftp to SUT, Reason: ~p",
			   [Name, ?MODULE, sftp_to_target, {error, Reason}]),
		    {error, Reason}
	    end;
	{error, Reason}	->
	    ct:log(lightred,
		   "~p: ~p ~p Could not read file ~s, Reason: ~p",
		   [Name, ?MODULE, sftp_to_target, From, {error, Reason}]),
	    {error, Reason}
    end.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_sftp.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/1
%%% @doc ==Common Test hook for sending storing sftp information==
%%%
%%%
%%% Holds IP, username and password to sftp server in lab<br/>
%%% In simulated environment $RCT_TOP/test/config/sim_sftp_server.cfg is used<br/>
%%% In target environment /proj/rcs-tmp/stps/xxx/config/stp.cfg is used<br/>
%%%
%%% Hook formats:
%%% ```{rct_sftp, [{N, Name}]}'''
%%%
%%% There is a short format for testing towards one node:
%%% ```{rct_sftp, Name} expands to {rct_sftp, [{1, Name}]}'''
%%% 
%%% There is a short format for testing towards clustered nodes:
%%% ```{rct_sftp, [Name1, Name2]} expands to {rct_sftp, [{1, Name1},{2, Name2}]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simuleted environment.
%%%    Name     = atom()                         Used as identifier'''
%%% 
%%% Examples:<br/>
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_sftp,sftp1}]}].'''
%%%
%%% Testcase example.
%%% ```mytest(_) ->
%%%        {ok, Host} = rct_sftp:host(sftp1),
%%%        {ok, Username} = rct_sftp:username(sftp1),
%%%        {ok, Password} = rct_sftp:password(sftp1),
%%%        {ok, [Host,Username,Password]} = rct_sftp:all(sftp1).'''
%%% @end

-module(rct_sftp).
-id('Updated by CCase').
-vsn('/main/R3A/1').
-date('2015-02-26').
-author('etxkols').
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
%%% R3A/1      2015-02-26 etxkols     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([init/2,
	 pre_init_per_suite/3,
	 terminate/1,
	 host/1,
	 username/1,
	 password/1,
	 all/1]).

-include_lib("common_test/include/ct.hrl").
    
%%===========================================================================
%% @spec host(Name) ->
%%    {ok, Host} | {error, Reason}
%% Name = atom()
%% Host = string()
%% Reason = term()
%%
%% @doc Returns IP address to sftp server.<br/>
%% Example: 
%% ``` rct_sftp:host(sftp1)->
%%       {ok,"10.68.200.11"}'''
host(Name) ->
    [{host,Host},_,_] = ct:get_config(make_name_module(Name)),
    Host.
    
%%===========================================================================
%% @spec username(Name) ->
%%    {ok, IP} | {error, Reason}
%% Name = atom()
%% IP = string()
%% Reason = term()
%%
%% @doc Returns username to sftp server.<br/>
%% Example: 
%% ``` rct_sftp:username(sftp1)->
%%       {ok,"mauve"}'''
username(Name) ->
    [_,{username,Username},_] = ct:get_config(make_name_module(Name)),
    Username.

%%===========================================================================
%% @spec password(Name) ->
%%    {ok, IP} | {error, Reason}
%% Name = atom()
%% IP = string()
%% Reason = term()
%%
%% @doc Returns password to sftp server.<br/>
%% Example: 
%% ``` rct_sftp:password(sftp1)->
%%       {ok,"mauve"}'''
password(Name) ->
    [_,_,{password,Password}] = ct:get_config(make_name_module(Name)),
    Password.

%%===========================================================================
%% @spec all(Name) ->
%%    {ok, All} | {error, Reason}
%% Name = atom()
%% All = [string()]
%% Reason = term()
%%
%% @doc Returns password to sftp server.<br/>
%% Example: 
%% ``` rct_sftp:all(sftp1)->
%%       {ok,["10.68.200.11","mauve","dilbert"]}'''
all(Name) ->
    [{host,Host},{username,Username},{password,Password}] = ct:get_config(make_name_module(Name)),
    [Host, Username, Password].

%%% @hidden
%%% init function for ct_hook
init(_Id, Opts) ->
    {ok,Opts}.

%%% @hidden
pre_init_per_suite(_Suite,Config = {fail,_},States) ->
    {Config,States};
pre_init_per_suite(_Suite,Config = {skip,_},States) ->
    {Config,States};
pre_init_per_suite(_Suite,Config,Name) when is_atom(Name) ->
    pre_init_per_suite(_Suite,Config,[Name]);
pre_init_per_suite(_Suite,Config,States) ->    
    SuiteType = case length(States) > 1 of
    		    true  -> clustered;
    		    false -> single
    		end,
    case do_pre_init_per_suite(States,SuiteType,1) of
    	ok ->
    	    {Config, States};
    	Other ->
    	    Other
    end.

do_pre_init_per_suite([],_,_) ->
    ok;
do_pre_init_per_suite([Name|T],SuiteType,Num) when is_atom(Name)->
    do_pre_init_per_suite([{Num,Name}|T],SuiteType,Num);
do_pre_init_per_suite([{N,Name}|T],SuiteType,Num) -> 
    MP = get_ct_arg(mp),
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    No = case {SuiteType,MP} of
			{single,undefined} -> N;
			{single,MP} -> list_to_integer(MP -- "du");
			{clustered,_} -> N
		    end,
	    case rct_multi_node_cfg:require(make_name_module(Name), No, {sftp_server,[host,username,password]}) of
		ok ->
		    do_pre_init_per_suite(T,SuiteType,Num + 1);
		{error, Reason} ->
		    ct:log(lightred,"~p: ~p ~p Config parameters not matching {sftp_server,[host,username,password]}, Reason: ~p",
			   [Name, ?MODULE, do_pre_init_per_suite, {error, Reason}]),
		    {fail, Reason}
	    end;
        "sim" ->
	    case ct:require(make_name_module(Name), {sftp_server,[host,username,password]}) of
		ok ->
		    do_pre_init_per_suite(T,SuiteType,Num + 1);
		{error, Reason} ->
		    ct:log(lightred,"~p: ~p ~p Config parameters not matching {sftp_server,[host,username,password]}, Reason: ~p",
			   [Name, ?MODULE, do_pre_init_per_suite, {error, Reason}]),
		    {fail, Reason}
	    end
    end.

%%% @hidden
terminate([]) ->
    ok;
terminate([Name|T]) ->
    rct_multi_node_cfg:remove_config(make_name_module(Name)),
    terminate(T).

get_ct_arg(Arg) ->
    case init:get_argument(Arg) of
	{ok,[[Reply]]} -> Reply;
	error -> undefined
    end.

make_name_module(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(?MODULE)).

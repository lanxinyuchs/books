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
%% -----------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-01-31  etxkols    Created
%%% R1A/2      2012-04-20  etxkols    Ugly fix for new console server, will be replaced with ct_snmp
%%% R1A/3      2012-05-23  etxkols    Found an easier way to reset port, will be replaced with ct_snmp.
%%%                                   3 sec sleep after reset to give stupid console server time to
%%%                                   clean away everything (problem if trying to use port too soon
%%%                                   after reset).
%%% R1A/4      2012-06-05  etxkols    Completely rewritten to use ct_snmp instead of ct_telent
%%% R1A/5      2012-06-29  etxkols    "137.58.180.24" goes to digi tm
%%% R1A/5      2012-09-11  etxkols    "137.58.180.19" and "137.58.180.25" goes to digi tm
%%% R1A/6      2012-09-17  etxkols    Removed SNMP to Digi CM
%%% R2A/1      2012-10-24  etxkols    Handle simulated env 
%%% R2A/2      2013-08-08  etxkols    Added retries when login fails
%%% R2A/3      2013-08-09  etxkols    Added retries when login fails
%%% R2A/4      2014-03-19  etxkols    Adaption to OTP R17B01
%%% R2A/5      2014-03-19  etxkols    Fixed return value from init/2
%%% R4A/1      2015-06-02  etxkols    Cluster fixes
%%% R4A/2      2015-06-15  etxkols    Added ct_telnet:open retries
%%% R4A/3      2016-03-08  etxkols    5G
%%% ----------------------------------------------------------
%%% 
%% @doc Module to reset tty on console server to guarantie that rs232 interface can be reached through console server.
%% 
%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each testcases.<br/>
%% Hook format:
%% ```{rct_consserv, {[{N, Name}], Opts}}'''
%%
%% There are short formats for testing towards single node:
%% ```{rct_consserv, Name}                  expands to {rct_consserv, [{1,Name}]}'''
%% There are short formats for testing towards clusterd nodes:
%% ```{rct_consserv, [Name1, Name2]}        expands to {rct_consserv, [{1,Name1},{2,Name2}]}'''
%% 
%% Argument description:
%% ```N    = integer()                            Used to match card in stp.cfg file when running on target.
%%                                                Not used in simuleted environment.
%%    Name = atom()                               Used as identifier'''
%% 
%% Examples single node:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_consserv,node}]}].'''
%% Examples clusterd nodes:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_consserv,[node1,node2]}]}].'''
%% ```testcase(_) ->
%%        ok = rct_rs232:login(node1)'''
%%
%% The name of this hook is a bit misleading, but is kept for backwards compability reasons.
%% @end
-module(rct_consserv). 

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% Callbacks for ct_telnet.erl
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([pre_init_per_testcase/3]).
-export([terminate/1]).
-export([connect/5]).
-export([connect/6]).
-export([login/1]).
-export([get_prompt_regexp/0]).

-include_lib("common_test/include/ct.hrl").

-define(NO_OF_OPEN_RETRIES,1).

%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
%%===========================================================================
%% @spec pre_init_per_suite(Suite, Config, States) -> 
%%    {Config, States} | {{fail,Reason}, States}
%%
%% @doc Verify existence of configuration data [telnet,port,username,password].<br/>
%%===========================================================================
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CthState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CthState};
pre_init_per_suite(Suite, Config, States) when is_atom(States)->
    pre_init_per_suite(Suite, Config, [States]);
pre_init_per_suite(_Suite, Config, States) ->
    SuiteType = case length(States) > 1 of
    		    true  -> clustered;
    		    false -> single
    		end,
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Config, States};
        "cloudish" ->
	    {Config, States};
	"target" ->
	    States2 = do_pre_init_per_suite(States,[],SuiteType,1),
	    {Config,States2}
    end.

do_pre_init_per_suite([],R,_SuiteType,_Num) ->
    R;
do_pre_init_per_suite([Name|T],R,SuiteType,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name}|T],R,SuiteType,Num);
do_pre_init_per_suite([{N,Name}|T],R,SuiteType,Num) ->
    BoardNum = rct_cluster_lib:get_target_du(N,SuiteType), 
    Board = ct:get_config({test_nodes,BoardNum}),
    case ct:get_config({Board,console_server}) of
	Consserv = [{telnet,_},
		    {port,_},
		    {username,_},
		    {password,_},
		    {tty,_}] ->
	    rct_multi_node_cfg:require(Name,Consserv),
	    do_pre_init_per_suite(T,R ++ [Name],SuiteType,Num + 1 );
	Other ->
	    ct:log(lightred,"~p: ~p ~p Faulty or missing config parameters for console_server~p",[Name, ?MODULE, pre_init_per_suite, {error, Other}]),
	    {fail, Other}
    end.

%% @hidden
%% @spec pre_init_per_testcase(TC,Config,States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Common test ct_hook which runs before each testcase.<br/>
%% Connects, kill tty and disconnect from power unit.
pre_init_per_testcase(_TC,Config,States) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Config, States};
        "cloudish" ->
	    {Config, States};
	"target" ->
	    do_pre_init_per_testcase(Config,States,States,?NO_OF_OPEN_RETRIES)
    end.

do_pre_init_per_testcase(Config,States,[],_NoOfRetries) ->
    {Config, States};
do_pre_init_per_testcase(Config,States,[Name|T],NoOfRetries) ->
    case ct_telnet:open(Name,telnet,?MODULE) of
	{ok,_Handle} ->
	    case kill_tty(Name) of
		{ok,_} ->
		    ct_telnet:close(Name),
		    do_pre_init_per_testcase(Config,States,T,?NO_OF_OPEN_RETRIES);
		{error, Reason} ->
		    ct_telnet:close(Name),
		    {{fail, Reason}, States}
	    end;
	{error, Reason} ->
	    case NoOfRetries == 0 of
		false ->
		    ct:log(yellow,"~p: ~p ~p Could not open telnet connection to console server, Reason: ~p. Retrying...",[Name, ?MODULE, pre_init_per_testcase, {error, Reason}]),
		    timer:sleep(5000),
		    do_pre_init_per_testcase(Config,States,[Name|T],NoOfRetries - 1);
		true ->
		    ct:log(lightred,"~p: ~p ~p Could not open telnet connection to console server, Reason: ~p",[Name, ?MODULE, pre_init_per_testcase, {error, Reason}]),
		    {{fail, Reason}, States}
	    end
    end.

%%% @hidden
terminate(Names) ->
   case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ok;
        "cloudish" ->
	    ok;
	"target" ->
	    do_terminate(Names)
    end.

do_terminate([]) ->
    ok;
do_terminate([Name|T])->
    rct_multi_node_cfg:remove_config(Name),
    do_terminate(T).

-define(loginprompt,"login: $").
-define(passwdprompt,"password: $").
-define(rootprompt,"#> $").
-define(prx,?loginprompt ++ "|" ++ ?passwdprompt ++ "|" ++ ?rootprompt).
-define(TIMEOUT, 30000).

%% @hidden
%% Mandatory ct_telnet callback function.
get_prompt_regexp() ->
    ?prx.

%% @hidden
%% R17B01 Mandatory ct_telnet callback function.
connect(ConnName,Ip,Port,Timeout,KeepAlive,_Name) ->
    ct_telnet_client:open(Ip,Port,Timeout,KeepAlive,ConnName).

%% @hidden
%% Mandatory ct_telnet callback function.
connect(Ip,Port,Timeout,KeepAlive,_Name) ->
    ct_telnet_client:open(Ip,Port,Timeout,KeepAlive).

kill_tty(Name) ->
    TTY = ct:get_config({Name,tty}),
    case login(Name) of
        ok ->
            case ct_telnet:cmd(Name,"kill tty=" ++ TTY) of
                {ok, Data} ->
                    {ok, Data};
                {error,Reason} ->
                    ct:log(lightred,"~p: ~p ~p Could not execute command \"kill tty=~s\" on console server, Reason: ~p",[Name, ?MODULE, kill_tty, TTY, {error, Reason}]),
                    {error,Reason}
            end;
        {error,Reason} ->
            {error,Reason}
    end.

%% @spec login(Name) -> ok | {error, Reason}
%% Name = atom()
%% @doc Login to console server using data from Config file.
login(Name) ->
    Username = ct:get_config({Name,username}),
    Password = ct:get_config({Name,password}),
    case login(Name,?loginprompt,Username,?passwdprompt,Password,?rootprompt) of
        ok ->
            ok;
        {error,Reason} ->
            ct:log(lightred,"~p: ~p ~p Could not login to console server, Reason: ~p",[Name, ?MODULE, login, {error, Reason}]),
            {error,Reason}
    end.

login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt) ->
    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,5,"").

login(_Name,_Loginprompt,_Username,_Passwdprompt,_Password,_Userprompt,0,Reason) ->
    {error, Reason};
login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N, _) ->
    case ct_telnet:send(Name,"\n") of
        ok ->
            case ct_telnet:expect(Name,[{loginprompt,Loginprompt},{passwdprompt,Passwdprompt},{userprompt,Userprompt}],[{timeout,?TIMEOUT}]) of
                {ok,{userprompt,_}} ->
                    ok;
                {ok,{passwdprompt,_}} ->
                    case ct_telnet:send(Name,Password) of
                        ok ->
                            case ct_telnet:expect(Name,[{userprompt,Userprompt}],[{timeout,?TIMEOUT}]) of
                                {ok,{userprompt,_}} ->
                                    ok;
                                {error,Reason} ->
				    % It happens rarely that password prompt appears instead of use prompt, retrying
				    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N-1,Reason)
%                                    {error,Reason}
                            end;
                        {error,Reason} ->
                            {error,Reason}
                    end;
                {ok,{loginprompt,_}} ->
                    case ct_telnet:send(Name,Username) of
                        ok ->
                            case ct_telnet:expect(Name,[{passwdprompt,Passwdprompt}],[{timeout,?TIMEOUT}]) of
                                {ok,{passwdprompt,_}} ->
                                    case ct_telnet:send(Name,Password) of
                                        ok ->
                                            case ct_telnet:expect(Name,[{userprompt,Userprompt}],[{timeout,?TIMEOUT}]) of
                                                {ok,{userprompt,_}} ->
                                                    ok;
                                                {error,Reason} ->
						    % It happens rarely that password prompt appears instead of use prompt, retrying
						    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N-1,Reason)
%                                                    {error,Reason}
                                            end;
                                        {error,Reason} ->
                                            {error,Reason}
                                    end;
                                {error,Reason} ->
				    % It happens rarely that login prompt appears instead of passwd prompt, retrying
				    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N-1,Reason)
%                                    {error,Reason}
                            end;
                        {error,Reason} ->
                            {error,Reason}
                    end;
                {error,timeout} ->
                    timer:sleep(1000),
                    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N-1,timeout);
                {error,Reason} ->
                    {error,Reason}
            end;
        {error,Reason} ->
            {error,Reason}
    end.

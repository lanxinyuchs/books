%% -----------------------------------------------------------------------------
%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%% Rev        Date        Name        What
%% -----      ---------   --------    ------------------------
%% R2A/1      2014-09-04  etxkivri    Created to comunicate vid led analyzer via rs232.
%%                                    IP and port is defined in stp.config
%%                                    ex. /proj/rcs-tmp/stps/dus5039/config/stp.cfg
%%                                    Most is copied from rct_rs232.erl
%%                                    This hook can not be used on sim env.
%% -----------------------------------------------------------------------------
%% @doc Module to access serial port (rs232) on SUT via telnet to console server.
%% 
%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each testcases.<br/>
%% Hook formats:
%% ```{rct_led_rs232, {[{N, Name}], Opts}}'''
%%
%% There are short formats for testing towards one and more nodes:
%% ```{rct_led_rs232, Name}                  expands to {rct_led_rs232, {[{1,Name}], [{log_type,raw}]}}
%%    {rct_led_rs232, {Name, Opts}}          expands to {rct_led_rs232, {[{1,Name}], Opts}}
%%    {rct_led_rs232, [{1,Name1},{2,Name2}]} expands to {rct_led_rs232, {[{1,Name1},{2,Name2}], [{log_type,raw}]}}'''
%% 
%% Argument description:
%% ```N    = integer()                            Used to match card in stp.cfg file when running on target.
%%    Name = atom()                               Used as identifier
%%    Opts = [Opt]              
%%           {log_type, raw | html | silent}      Default=raw, Used for logging telnet to file, see ct_telnet'''
%% Before each testcase `pre_init_per_testcase' will:<br/>
%% - Verify existance of config parameter `{rs232,[telnet,port]}'.<br/>
%% - Connect to SUT via console server
%% 
%% After each testcase `post_end_per_testcase' will:<br/>
%% - Disconnect from console server<br/>
%% Examples:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_led_rs232,node1}]}].'''
%% ```suite() -> 
%%        [{ct_hooks, [{rct_led_rs232,[{1,node1},{2,node2}]}]}].'''
%% `node1' and `node2' are a references to SUTs.
%% ```testcase(_) ->
%%        ok = rct_led_rs232:login(node1)'''
%% @end
-module(led_rs232). 

-define(TIMEOUT, 30000).
-define(TelnetIp, "10.68.200.64"). %% This can be romved when this exist in config
-define(Port, 2013).               %% This can be romved when this exist in config

%% Callbacks for ct_telnet.erl
-export([login/1]).
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).


%% %% @spec login(Name) -> ok | {error, Reason}
%% %% Name = atom()
%% %% @doc Login to SUT using data from Config file.
login(Name) ->
    LoginData = rct_linux_rs232:get_login_data(Name),
    ct:log("# LoginData: ~p",[LoginData]).
    %%LoginData.
	
%% @hidden
init(Id, Name) when is_atom(Name) ->
    init(Id, {[{1,Name}],[{log_type,raw}]});
init(Id, {Name, Opts}) when is_atom(Name) ->
    init(Id, {[{1,Name}], Opts});
init(Id, Names) when is_list(Names) ->
    init(Id, {Names, [{log_type,raw}]});
init(_Id, {Names, Opts}) ->
    Names2 = [Name||{_,Name}<-Names],
    LogType = proplists:get_value(log_type,Opts,raw),
    rct_multi_node_cfg:append_config(ct_conn_log,[{ct_telnet,[{log_type,LogType},
							      {hosts,Names2}]}]),
    {ok,{Names, Opts}}.

%%===========================================================================
%% @spec pre_init_per_suite(Suite, Config, States) -> 
%%    {Config, States} | {{fail,Reason}, States}
%%
%% @doc Verify existence of configuration data [telnet,port].<br/>
%%===========================================================================
pre_init_per_suite(_Suite, Config, States) ->
    ct:log("# States: ~p", [States]),
    {do_pre_init_per_suite(Config,States), States}.


do_pre_init_per_suite(Config,{[],_Opts}) ->
    Config;
do_pre_init_per_suite(Config,{[{N,Name}|T],Opts}) ->
    Node = ct:get_config({test_nodes,N}),
    ct:log("# Node: ~p" ,[Node]),
    %% case ct:require({Node,led_rs232,[telnet,port]}) of
    %% 	ok ->
    	    BoardType = ct:get_config({Node,board_type}),
    %% 	    %% Rs232Data = ct:get_config({Node,led_rs232}), Use this when this exist in config
	    Rs232Data = [{telnet,?TelnetIp},{port,?Port}],
	    ct:log("# led Rs232Data: ~p" ,[Rs232Data]),
	    % Trick to add board_type to telnet data. Needed to distinguish between linux and ose.
	    rct_multi_node_cfg:require(Name,Rs232Data ++ [{board_type,BoardType}]),
    	    do_pre_init_per_suite(Config,{T,Opts}).
	    %% do_pre_init_per_suite(Config,{T,Opts});
    %% 	{error, Reason} ->
    %% 	    ct:log(lightred,"~p: ~p ~p Config parameters not matching {rs232,[telnet,port]} for rs232, Reason: ~p",[Name, ?MODULE, pre_init_per_suite, {error, Reason}]),
    %% 	    {fail, Reason}
    %% end.


%% @spec pre_init_per_testcase(TC,Config,States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Common test ct_hook which runs before each testcase.<br/>
%% Connects to SUT via console server.
pre_init_per_testcase(_TC,Config,States) ->
    {do_pre_init_per_testcase(Config, States), States}.    

do_pre_init_per_testcase(Config,{[],_}) ->
    Config;
do_pre_init_per_testcase(Config,{[{_N,Name}|T],Opts}) ->
    CallBackModule = rct_linux_rs232,
    ct_telnet:open(Name,telnet,CallBackModule),
    %% ct:pal("# CallBackModule: ~p", [CallBackModule]),
    case ct_telnet:open(Name,telnet,CallBackModule) of
	{ok,_Handle} ->
    		    do_pre_init_per_testcase(Config,{T,Opts});
	{error, Reason} ->
	    ct:log(lightred,"~p: ~p ~p Could not open telnet connection led analyzer over rs232, ~n"
		   "Reason: ~p",[Name, ?MODULE, pre_init_per_testcase, {error, Reason}]),
	    {fail, Reason}
    end.


%% @spec post_end_per_testcase(TC,Config,Return,States) -> {Return, States} | {{fail,Reason}, States}
%% @doc Common test ct_hook which runs after each testcase.<br/>
%% Disconnects from SUT.
post_end_per_testcase(_TC,Config,Return,States) ->
    %% Clients = ?config(clients, Config),
    Clients = proplists:get_value(clients, Config),
    %% [?CLIENT(stop, Pid) || Pid <- Clients],
    lists:foreach(fun(Y)->
    			  %% ?RPC_CALL(led_client, stop, [Y])
			  rct_rpc:call(rpc, led_client, stop, [Y], 10000)
    		  end, Clients),
    {do_post_end_per_testcase(Return,States),States}.

do_post_end_per_testcase(Return,{[],_}) ->
    Return;
do_post_end_per_testcase(Return,{[{_N,Name}|T],Opts}) ->
    case ct_telnet:close(Name) of
	ok  ->
	    do_post_end_per_testcase(Return,{T,Opts});
	{error, Reason} ->
	    {fail, Reason}
    end.    

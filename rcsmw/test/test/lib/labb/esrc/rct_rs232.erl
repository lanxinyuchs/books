%% -----------------------------------------------------------------------------
%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%% R1A/2      2012-03-23  etxkols    sleep 1 sec after ct_telnet:open in pre_init_per_testcase
%% R1A/3      2012-05-23  etxkols    sleep also when States is atom
%% R1A/4      2012-05-24  etxkols    Sleep a short while after post_end_per_testcase
%% R1A/5      2012-05-30  etxkols    Changed rootprompt = ".*root@.*:.*# $" to
%%                                   ".*root@.*:.*# " to match if stuff are written in console
%%                                   after rootprompt has been received
%% R1A/6      2012-06-08  etxkols    Moved login from rct_telnet_common.erl to this module 
%%                                   since this is the only module left using ct_telnet.
%% R1A/7      2012-07-02  etxkols    Removed timeouts when testing old consserv 
%% R2A/1      2012-10-24  etxkols    Handle simulated env 
%% R2A/2      2012-12-07  etxkols    Heavily modified to support both linux and ose 
%% R2A/3      2012-12-07  etxkols    Fix when pre_init_per_testcase fails
%% R2A/4      2013-05-27  etxkols    Retry when no pwd prompt appears after entering username
%% R2A/5      2013-06-11  etxkols    Retry connect option for TLM model
%% R2A/6      2013-09-25  etxkols    Support tcu
%% R2A/7      2014-01-15  etxkols    Changed "\n" to "" when poll login
%% R2A/8      2014-03-19  etxkols    Adaption to OTP R17B01
%% R2A/9      2014-03-19  etxkols    edoc fix
%% R2A/10     2014-07-01  etxkols    Fix for clustering
%% R2A/11     2015-05-19  etxkols    Fix for t605
%% R4A/1      2015-06-01  etxkols    Fix for clustering
%% R4A/2      2015-06-01  etxkols    Cleaned away printouts
%% R4A/3      2015-06-01  etxkols    Fault in pre_init_per_suite/3
%% R4A/4      2015-11-10  etxkols    connect_retries defaults to 2
%% R4A/5      2016-03-08  etxkols    5G
%% R4A/6      2016-05-03  etxkols    idu board type
%% R6A/1      2016-08-30  etxkols    Fix for rcf-bpu configurations
%% R8A/1      2016-11-28  etxkols    c608
%% R8A/2      2017-01-02  etxkols    Allow EE to use serial connection in cloud
%% R8A/4      2017-01-03  etxkols    Allow EE to use serial connection in cloud, second try
%% -----------------------------------------------------------------------------
%% @doc Module to access serial port (rs232) on SUT via telnet to console server.
%% 
%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each testcases.<br/>
%% Hook formats:
%% ```{rct_rs232, {[{N, Name}], Opts}}'''
%%
%% There are short formats for testing towards single node:
%% ```{rct_rs232, Name}                  expands to {rct_rs232, {[{1,Name}], [{log_type,raw}]}}
%%    {rct_rs232, {Name, Opts}}          expands to {rct_rs232, {[{1,Name}], Opts}}'''
%% There are short formats for testing towards clusterd nodes:
%% ```{rct_rs232, [Name1, Name2]}        expands to {rct_rs232, {[{1,Name1},{2,Name2}], [{log_type,raw}]}}
%%    {rct_rs232, {[Name1,Name2], Opts}} expands to {rct_rs232, {[{1,Name1},{2,Name2}], Opts}}'''
%% 
%% Argument description:
%% ```N    = integer()                            Used to match card in stp.cfg file when running on target.
%%                                                Not used in simuleted environment.
%%    Name = atom()                               Used as identifier
%%    Opts = [Opt]              
%%    Opt  = {connect_retries, integer() | false} A need for connect retries was discovered when TLM tests begun.
%%                                                If false, no retries to connect over telnet is made.
%%                                                If integer (default=2), retries to connect over telnet is made integer times with 5 seconds delay.
%%           {log_type, raw | html | silent}      Default=raw, Used for logging telnet to file, see ct_telnet'''
%% Before each testcase `pre_init_per_testcase' will:<br/>
%% - Verify existance of config parameter `{rs232,[telnet,port,username,password]}'.<br/>
%% - Connect to SUT via console server
%% 
%% After each testcase `post_end_per_testcase' will:<br/>
%% - Disconnect from console server<br/>
%% 
%% Examples single node:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_rs232,node1}]}].'''
%% ```suite() -> 
%%        [{ct_hooks, [{rct_rs232,{node1,[{connect_retries, 3}]}}]}].'''
%% Examples clusterd nodes:
%% ```suite() -> 
%%        [{ct_hooks, [{rct_rs232,[node1,node2]}]}].'''
%% ```testcase(_) ->
%%        ok = rct_rs232:login(node1)'''
%% @end
-module(rct_rs232). 

-define(TIMEOUT, 30000).
-define(CONNECT_RETRIES, 2).

%% Callbacks for ct_telnet.erl
-export([login/1]).
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).
-export([terminate/1]).

%% @spec login(Name) -> ok | {error, Reason}
%% Name = atom()
%% @doc Login to SUT using data from Config file.
login(Name) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ct:log(lightred,"~p: ~p ~p Could not login on SUT over rs232, Reason: NOT supported in SIMULATED environment",[Name, ?MODULE, login]);
        Env when Env=="cloudish";
		 Env=="target" ->
%	    case BoardType = ct:get_config({Name,board_type}) of
%		"rcfapa" ->
%		    ct:log(lightred,"~p: ~p ~p Could not login on SUT over rs232, Reason: NOT supported in SIMULATED environment",[Name, ?MODULE, login]);
%		undefined ->
%		    ct:log(lightred,"~p: ~p ~p Could not find board type",[Name, ?MODULE, login]),
%		    {error,unknown_board_type_for_name};
%		_ -> 
		    case ct:get_config(Name) of
			[{telnet,""},{port,0},{username,"root"},{password,"root"},{board_type,"rcf"}] ->
			    ct:log("~p: ~p ~p Cloud instance with invalid IP/Port detected, will not attempt to login over serial connection",[Name, ?MODULE, pre_init_per_testcase]);
			[_,_,_,_,{board_type,undefined}] ->
			    ct:log(lightred,"~p: ~p ~p Could not find board type",[Name, ?MODULE, login]),
			    {error,unknown_board_type_for_name};
			undefined ->
			    ct:log(lightred,"~p: ~p ~p Could not find Name",[Name, ?MODULE, login]),
			    {error,unknown_name};			    
			[_,_,_,_,{board_type,BoardType}] ->
			    do_login(BoardType, Name)
%		    end
	    end
    end.

do_login(BoardType, Name) ->
    case operating_system(BoardType) of
	undefined ->
	    ct:log(lightred,"~p: ~p ~p Unknown board type ~p",[Name, ?MODULE, pre_init_per_testcase, BoardType]),
	    {error, unknown_board_type};
	OS ->
	    LoginData = case OS of 
			    linux ->
				rct_linux_rs232:get_login_data(Name);
			    ose ->
				rct_ose_rs232:get_login_data(Name)
			end,		   	    
	    case login(Name,LoginData) of
		ok ->
		    ok;
		{error,Reason} ->
		    ct:log(lightred,"~p: ~p ~p Could not login on SUT over rs232, Reason: ~p",[Name, ?MODULE, login, {error, Reason}]),
		    {error,Reason}
	    end
    end.

%% @hidden
init(Id, Name) when is_atom(Name) ->
    init2(Id, {[Name],[{log_type,raw}]});
init(Id, {Name, Opts}) when is_atom(Name) ->
    init2(Id, {[Name], Opts});
init(Id, Names) when is_list(Names) ->
    init2(Id, {Names, [{log_type,raw}]});
init(Id, {Names, Opts}) when is_list(Names) ->
    init2(Id, {Names, Opts}).

init2(_Id, {Names, Opts}) ->
    Names2 = get_names(Names,[]),
    LogType = proplists:get_value(log_type,Opts,raw),
    case erlang:system_info(otp_release) of
	OTP_RELEASE when OTP_RELEASE == "R16B02";
			 OTP_RELEASE == "R16B03" ->
	    ok;
	_ ->
	    rct_multi_node_cfg:append_config(ct_conn_log,[{ct_telnet,[{log_type,LogType},{hosts,Names2}]}])
    end,
    {ok,{Names, Opts}}.

get_names([],R) ->
    R;
get_names([Name|T],R) when is_atom(Name) ->
    get_names(T,R ++ [Name]);
get_names([{_,Name}|T],R) ->
    get_names(T,R ++ [Name]).

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
pre_init_per_suite(_Suite, Config, States = {Names,_Opts}) ->
    SuiteType = case length(Names) > 1 of
    		    true  -> clustered;
    		    false -> single
    		end,
    ct:pal("SIM_OR_TARGET ~p",[os:getenv("SIM_OR_TARGET")]),
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Config, States};
        "cloudish" ->
	    States2 = do_pre_init_per_suite(States,[],SuiteType,1),
	    {Config,States2};
	"target" ->
	    States2 = do_pre_init_per_suite(States,[],SuiteType,1),
	    {Config,States2}
    end.

do_pre_init_per_suite({[],Opts},R,_SuiteType,_Num) ->
    {R,Opts};
do_pre_init_per_suite({[Name|T],Opts},R,SuiteType,Num) when is_atom(Name) ->
    do_pre_init_per_suite({[{Num,Name}|T],Opts},R,SuiteType,Num);
do_pre_init_per_suite({[{N,Name}|T],Opts},R,SuiteType,Num) ->
    BoardNum = rct_cluster_lib:get_target_du(N,SuiteType), 
    Board = ct:get_config({test_nodes,BoardNum}),
    BoardType = ct:get_config({Board,board_type}),
    case ct:require({Board,rs232,[telnet,port,username,password]}) of
	ok ->
	    Rs232Data = ct:get_config({Board,rs232}),
						% Trick to add board_type to telnet data. Needed to distinguish between linux and ose.
	    rct_multi_node_cfg:require(Name,Rs232Data ++ [{board_type,BoardType}]),
	    do_pre_init_per_suite({T,Opts},R ++ [Name],SuiteType,Num + 1);
	{error, Reason} ->
	    ct:log(lightred,"~p: ~p ~p Config parameters not matching {rs232,[telnet,port,username,password]} for rs232, Reason: ~p",[Name, ?MODULE, pre_init_per_suite, {error, Reason}]),
	    {fail, Reason}
    end.

%% @hidden
%% @spec pre_init_per_testcase(TC,Config,States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Common test ct_hook which runs before each testcase.<br/>
%% Connects to SUT via console server.
pre_init_per_testcase(_TC,Config,States) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Config, States};
        "cloudish" ->
	    {do_pre_init_per_testcase(Config,States), States};
	"target" ->
	    {do_pre_init_per_testcase(Config,States), States}
    end.

do_pre_init_per_testcase(Config,{[],_}) ->
    Config;
do_pre_init_per_testcase(Config,{[Name|T],Opts}) ->
    ConnectRetries = proplists:get_value(connect_retries,Opts,?CONNECT_RETRIES),
    BoardType = ct:get_config({Name,board_type}),
    case ct:get_config(Name) of
	[{telnet,""},{port,0},{username,"root"},{password,"root"},{board_type,"rcf"}] ->
	    ct:log("~p: ~p ~p Cloud instance with invalid IP/Port detected, will not attempt to connect over serial connection",[Name, ?MODULE, pre_init_per_testcase]),
	    do_pre_init_per_testcase(Config,{T,Opts});
	_ ->
	    case operating_system(BoardType) of
		undefined ->
		    ct:log(lightred,"~p: ~p ~p Unknown board type ~p",[Name, ?MODULE, pre_init_per_testcase, BoardType]),
		    {fail, unknown_board_type};
		OS ->
		    CallBackModule = case OS of 
					 linux ->
					     rct_linux_rs232;
					 ose ->
					     rct_ose_rs232
				     end,
		    
		    case connect(Name,CallBackModule,ConnectRetries)of
			{ok,_Handle} ->
			    do_pre_init_per_testcase(Config,{T,Opts});
			{error, Reason} ->
			    ct:log(lightred,"~p: ~p ~p Could not open telnet connection to SUT over rs232, Reason: ~p",[Name, ?MODULE, pre_init_per_testcase, {error, Reason}]),
			    {fail, Reason}
		    end
	    end
    end.

%% A need to retry telnet connect was discovered when starting testing against TLM model.
connect(Name,CallBackModule,false) ->
    ct_telnet:open(Name,telnet,CallBackModule);
connect(Name,CallBackModule,ConnectRetries) ->
    case ct_telnet:open(Name,telnet,CallBackModule) of
	{ok,_Handle} -> 
	    {ok,_Handle};
	{error, Reason} ->
	    case ConnectRetries of
		0 ->
		    {error, Reason};
		ConnectRetries ->
		    ct:log(internal,"~p: ~p ~p Could not open telnet connection to SUT over rs232, Reason: ~p~nWill retry ~p times with 5 sec interval",
			   [Name, ?MODULE, pre_init_per_testcase, {error, Reason},ConnectRetries]),
		    timer:sleep(5000),
		    connect(Name,CallBackModule,ConnectRetries - 1)
	    end
    end.
    
%% @hidden
%% @spec post_end_per_testcase(TC,Config,Return,States) -> {Return, States} | {{fail,Reason}, States}
%% @doc Common test ct_hook which runs after each testcase.<br/>
%% Disconnects from SUT.
post_end_per_testcase(_TC,_Config,Return,States) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Return, States};
        "cloudish" ->
	    {do_post_end_per_testcase(Return,States),States};
	"target" ->
	    {do_post_end_per_testcase(Return,States),States}
    end.

do_post_end_per_testcase(Return,{[],_}) ->
    Return;
do_post_end_per_testcase(Return,{[Name|T],Opts}) ->
    case ct:get_config(Name) of
	[{telnet,""},{port,0},{username,"root"},{password,"root"},{board_type,"rcf"}] ->
	    do_post_end_per_testcase(Return,{T,Opts});
	_ ->
	    case ct_telnet:close(Name) of
		ok  ->
		    do_post_end_per_testcase(Return,{T,Opts});
		{error, Reason} ->
		    {fail, Reason}
	    end
    end.

%%% @hidden
terminate({Names,_Opts}) ->
   case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ok;
        "cloudish" ->
	    do_terminate(Names);
	"target" ->
	    do_terminate(Names)
    end,
    rct_multi_node_cfg:remove_config(ct_conn_log,ct_telnet),
    ok.

do_terminate([]) ->
    ok;
do_terminate([Name|T]) when is_atom(Name) ->
    rct_multi_node_cfg:remove_config(Name),
    do_terminate(T).

login(Name,{Loginprompt,Username,Passwdprompt,Password,Userprompt}) ->
    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,5,"").

login(_Name,_Loginprompt,_Username,_Passwdprompt,_Password,_Userprompt,0,Reason) ->
    {error, Reason};
login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N, _) ->
    case ct_telnet:send(Name,"") of
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
                                    {error,Reason}
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
                                                    {error,Reason}
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

operating_system([$t,$c,$u|_]) ->
    linux;
operating_system([$t|_]) -> %t605
    linux;
operating_system([$d,$u|_]) ->
    linux;
operating_system([$i,$d,$u|_]) ->
    linux;
operating_system("c608") ->
    linux;
operating_system("rcf") ->
    linux;
operating_system([$R,$U|_]) ->
    ose;
operating_system(_) ->
    undefined.

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_cli_coli_common.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R9A/4
%%%
%%% @doc ==Generic module used by rct_cli.erl and rct_coli.erl ct_hooks==
%%% 
%%% @end

-module(rct_cli_coli_common). 
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R9A/4').
-date('2017-03-09').
-author('etxkols').
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
%% R2A/1      2012-10-19  etxkols    Created
%% R2A/2      2012-10-25  etxkols    Changed spawn ssh_session timeout to 60 sec
%% R2A/3      2012-10-26  etxkols    Updated ssh:connect options
%% R2A/5      2013-01-31  etxivri    Removed check that only same Pid that created ssh connection
%%                                   can use ssh session.
%% R2A/6      2013-02-22  etxivri    Added adjust_window in wait_for_prompt to adjust ssh flowcontrol, 
%%                                   and updated receive in loop_ssh_session for this.
%%                                   This is needed when a lot of message over ssh connection.
%% R2A/7      2013-06-11  etxkols    Increase timeout waiting for silent connection from 10 ms to 200 ms, 
%% R2A/8      2013-08-30  etxkols    Fixed dialyzer complaint, 
%% R2A/9      2013-10-08  etxkols    Changed ssh:connect timeout to 20 sec
%% R2A/10     2014-03-14  eolaand    Export a few more functions.
%% R2A/11     2014-03-26  etxkols    Faulty init/2 return value
%% R2A/12     2014-04-23  etxkols    Adatptions to TLM
%% R3A/1      2015-03-31  etxkols    Fixed adjust window
%% R4A/1      2015-05-28  etxkols    Fixed cluster
%% R4A/2      2015-05-28  etxkols    Added get_ip/1,get_port/3
%% R4A/3      2015-05-28  etxkols    Make cli and coli always run towards du1
%% R4A/4      2015-05-28  etxkols    Debug printout
%% R4A/5      2015-05-28  etxkols    Fix du1 when single suite on clustered nodes
%% R4A/6      2015-05-28  etxkols    Minor fix
%% R4A/7      2015-10-12  uabesvi    added asend
%% R4A/8      2015-11-03  etxkols    Fixed edoc
%% R4A/9      2015-12-03  etxkols    oam_auto default IPType, will select OAMAP or LMT
%% R5A/1      2016-02-02  etxkols    ipv6
%% R5A/2      2016-02-02  etxkols    edoc
%% R5A/3      2016-02-02  etxkols    connect/7 back
%% R5A/4      2016-02-04  etxkols    Temporarily backing out OAMAP for sec board until Jenkins is synced.
%% R5A/5      2016-02-09  etxkols    New flags
%% R5A/6      2016-02-10  etxkols    Documentation
%% R5A/7      2016-02-10  etxkols    Documentation
%% R5A/8      2016-02-12  etxkols    using rct_oamap_ipv:mgmnt_iptype/1
%% R5A/9      2016-02-25  etxkols    Added send/5
%% R5A/10     2016-03-08  etxkols    5G
%% R9A/1      2017-01-31  etxkols    Refactoring and support for multi instances in 5g cloud
%% R9A/2      2017-02-01  etxkols    Removed comments
%% R9A/3      2017-02-03  etxkols    Removed printout
%% R9A/4      2017-02-09  etxkols    ssh_lmt_ipv6 exists in cloud
%% -----------------------------------------------------------------------------
-export([send/2]).
-export([send/3]).
-export([send/4]).
-export([send/5]).
-export([asend/2]).
-export([asend/3]).
-export([connect/1]).
-export([connect/2]).
-export([connect/3]).
-export([connect/4]).
-export([connect/5]).
-export([connect/7]).
-export([connect/8]).
-export([change_prompt/2]).
-export([change_prompt/3]).
-export([start_ssh_session/7]).
-export([disconnect/1]).
-export([disconnect/2]).
-export([init/2]).
-export([pre_init_per_suite/4]).
-export([pre_init_per_testcase/4]).
-export([post_end_per_suite/4]).
-export([post_end_per_testcase/5]).
-export([terminate/1]).
-export([update_config/2]).
-export([cli_ets_proc/2]).
-export([get_ip/1]).
-export([get_port/3]).
-export([get_port/4]).
-define(SSH_CONNECT_TIMEOUT,20).
-include_lib("common_test/include/ct.hrl").

%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
%% @spec pre_init_per_suite(Suite,Config,CthState,Generic) -> {Config, CthState} | {{fail,Reason}, CthStates}| {{skip,Reason}, CthStates}
%% @doc Verifies CT config parameters.<br/>
pre_init_per_suite(_Suite,Config = {fail,_},States,_Generic) -> {Config,States};
pre_init_per_suite(_Suite,Config = {skip,_},States,_Generic) -> {Config,States};
pre_init_per_suite(Suite,Config,Name,Generic) when is_atom(Name) ->
    pre_init_per_suite(Suite,Config,[Name],Generic);
pre_init_per_suite(Suite,Config,{Name, Opts},Generic) ->
    pre_init_per_suite(Suite,Config,[{Name, Opts}],Generic);
pre_init_per_suite(_Suite,Config,CthState,Generic) ->
    case catch {crypto:start(), ssh:start()} of
	R when R =:= {ok, ok};
               R =:= {ok, {error,{already_started,ssh}}};
               R =:= {{error,{already_started,crypto}}, ok};
               R =:= {{error,{already_started,crypto}}, {error,{already_started,ssh}}} ->
	    case do_pre_init_per_suite(CthState, Generic, [], [], 1) of
		{ok, CthState2, AliasToHooks} ->
		    NewConfig = case proplists:get_value(alias_to_hooks,Config) of
				    undefined ->
					Config ++ [{alias_to_hooks,AliasToHooks}];
				    OldAliasToHooks ->
					lists:keyreplace(alias_to_hooks,1,Config,{alias_to_hooks,OldAliasToHooks ++ AliasToHooks})
				end,
		    {NewConfig, CthState2};
		Fail ->
		    {Fail,CthState}		    
	    end;
        _ ->
            {{skip, "Crypto and/or SSH could not be started!"},CthState}
    end.
	    
fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.

%% @spec do_pre_init_per_suite(CthState,Generic,R,AliasToHooks,Num) -> {ok, CthState} | {{fail,Reason}, CthStates}
%% @doc Verifies CT config parameters and parses ct_hooks Opts.<br/>
do_pre_init_per_suite([],_,R,AliasToHooks,_) ->
    {ok, R, AliasToHooks};
do_pre_init_per_suite([Name|T],Generic,R,AliasToHooks,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name,oam_auto,[]}|T],Generic,R,AliasToHooks,Num);
do_pre_init_per_suite([{Name,Opts}|T],Generic,R,AliasToHooks,Num) ->
    do_pre_init_per_suite([{Num,Name,oam_auto,Opts}|T],Generic,R,AliasToHooks,Num);
do_pre_init_per_suite([{N, Name, IPType, Opts}|T],Generic = {CliOrColi,DefUser,DefPwd,TgtPort,DefPrompt},R,AliasToHooks,Num) ->
    ManualConnect = proplists:get_value(manual_connect,Opts,false),
    User = proplists:get_value(user,Opts,DefUser),
    Password = proplists:get_value(password,Opts,DefPwd),
    Print = case proplists:get_value(noprint,Opts,print) of true -> noprint; print -> print end, 
    ConnectTimeout = proplists:get_value(connect_timeout,Opts,?SSH_CONNECT_TIMEOUT),
    case get_port(N, Name, CliOrColi, TgtPort) of
	{ok,Port} ->
	    IPType2 = rct_oamap_ipv:mgmnt_iptype(IPType),
	    Inet = case IPType2 of
		       ssh_lmt_ipv6 -> inet6;
		       ssh_TN_A_ipv6 -> inet6;
		       ssh_TN_A_ipv6_alt -> inet6;
		       _ -> inet						
		   end,
	    case get_ip(N, Name, IPType2) of
		{ok, IP} ->
		    ok = rct_multi_node_cfg:require(Name,[{ssh,IP},
							  {inet,Inet},
							  {port,Port},
							  {user,User},
							  {password,Password},
							  {cli_or_coli,CliOrColi},
							  {default_prompt,DefPrompt},
							  {manual_connect,ManualConnect},
							  {print,Print},
							  {connect_timeout,ConnectTimeout},
							  {n,N},
							  {ip_type,IPType2}]),
		    do_pre_init_per_suite(T,Generic,R ++ [{N, Name, IPType, Opts}],AliasToHooks ++ [{Name,{N,?MODULE}}],Num + 1);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

%%% If `rcstprep.sh' script (wrapped by `rcs_install.sh' script) has been run with `-oamap_ipv4' or `-oamap_ipv6' flag, OaM Accesspoint has been configured with either ipv4 or ipv6 address during installation.
%%%
%%% If `IPType=oam_auto' (default) is set in the testsuite, the management IP address will be automatically selected depending on precense of `-oamap_ipv4' or `-oamap_ipv6' as arguments to `rct_run.sh' or `{jenkins_config,[{oamap_ipv4, []}]}' or `{jenkins_config,[{oamap_ipv6, []}]}' as config parameters, Examples:
%%%
%%% ```rct_run.sh -stp dus5000                  will run netconf commands over ssh_lmt_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv4      will run netconf commands over ssh_TN_A_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv6      will run netconf commands over ssh_TN_A_ipv6'''
%%%
%%% `-oamap_ipv4' and `-oamap_ipv6' has precedence over `{jenkins_config,[{oamap_ipv4, []}]}' and`{jenkins_config,[{oamap_ipv6, []}]}', i.e. the config parameters will only be checked if the arguments are not given to `rct_run.sh'.
%% check_iptype(_N,oam_auto) ->
%%     case init:get_argument(oamap_ipv4) of
%% 	{ok,[[]]} ->
%% 	    ssh_TN_A_ipv4;
%% 	_ ->
%% 	    case init:get_argument(oamap_ipv6) of
%% 		{ok,[[]]} ->
%% 		    ssh_TN_A_ipv6;
%% 		_ ->
%% 		    case ct:get_config({jenkins_config,oamap_ipv4}) of
%% 			[] ->
%% 			    ssh_TN_A_ipv4;
%% 			_ ->
%% 			    case ct:get_config({jenkins_config,oamap_ipv6}) of
%% 				[] ->
%% 				    ssh_TN_A_ipv6;
%% 				_ ->
%% 				    ssh_lmt_ipv4
%% 			    end
%% 		    end
%% 	    end
%%     end;
%% check_iptype(_N,IPType) ->
%%     IPType.

%% @hidden
post_end_per_suite(_Suite,_Config, Return, CthState) ->
    ssh:stop(),
    crypto:stop(),
   {Return,CthState}.

%% @hidden
%% @spec pre_init_per_testcase(TC,Config,CthStates, CliOrColi) -> {Config, CthStates} | {{fail,Reason}, CthStates}
%% @doc Creates ets table for connections and connects to SUT if specified in ct_hook.<br/>
pre_init_per_testcase(_TC,Config, CthStates, CliOrColi) ->
    EtsProcName = make_etsprocname(CliOrColi),
    stop_ets_proc(EtsProcName),
    register(EtsProcName, spawn(?MODULE, cli_ets_proc, [self(),EtsProcName])),
    receive 
	{EtsProcName,ok} -> ok
    end,
    case do_pre_init_per_testcase(CthStates) of
	ok ->
	    {Config, CthStates};
	Other ->
	    Other
    end.

%% Connects to SUT if specified in ct_hook.
%% If connect to SIT CoRef and ChId are stored in ets table
do_pre_init_per_testcase([]) ->
    ok;
do_pre_init_per_testcase([{N, Name, _IPType, _Opts}|T]) ->
    case ct:get_config({Name,manual_connect}) of
    	false -> 
	    case rct_node_state:get_state(N) of
		down ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Attempting to connect to ~p although rct_node_state:get_state(~p) is marked as down",[Name,N]});
		_ ->
		    case connect(Name, 
				 ct:get_config({Name,ssh}), 
				 ct:get_config({Name,inet}), 
				 ct:get_config({Name,port}), 
				 ct:get_config({Name,user}),
				 ct:get_config({Name,password}),
				 ct:get_config({Name,default_prompt}),
				 ct:get_config({Name,print})) of
			ok ->
			    do_pre_init_per_testcase(T);
			{error, Reason} ->
			    fail_generic(?FUNCTION_NAME,?LINE,{"Could not connect to ~p in node ~p, Reason: ~p",[Name,N,Reason]})
		    end
	    end;
    	true ->
	    do_pre_init_per_testcase(T)
    end.

%% @hidden
%% @spec post_end_per_testcase(TC,Config,Return,States, CliOrColi) -> {Return, States} | {{fail,Reason}, States}
%% @doc Disconnects ssh port from SUT.<br/>
post_end_per_testcase(_TC,_Config,Return,States, CliOrColi) ->
    EtsProcName = make_etsprocname(CliOrColi),
    lists:foreach(fun({_N, Name, _IPType, _Opts}) ->
			  disconnect(Name, ct:get_config({Name,print}))
		  end, States),
    stop_ets_proc(EtsProcName),
    {Return, States}.

%% @hidden
%% Clean away items in ct_attributes
terminate(CthState) ->
    do_terminate(CthState),
    ok.

do_terminate([]) ->
    ok;
do_terminate([{_N, Name, _IPType, _Opts}|T]) ->
    rct_multi_node_cfg:remove_config(Name),
    do_terminate(T).

%% @doc see connect/5.
connect(Name) ->
    connect(Name, 
	    ct:get_config({Name,ssh}), 
	    ct:get_config({Name,inet}), 
	    ct:get_config({Name,port}), 
	    ct:get_config({Name,user}),
	    ct:get_config({Name,password}),
	    ct:get_config({Name,default_prompt}),
	    ct:get_config({Name,print})).

%% @doc see connect/5.
connect(Name, Print) ->
    connect(Name, 
	    ct:get_config({Name,ssh}), 
	    ct:get_config({Name,inet}), 
	    ct:get_config({Name,port}), 
	    ct:get_config({Name,user}),
	    ct:get_config({Name,password}),
	    ct:get_config({Name,default_prompt}),
	    Print).

%% @doc see connect/5.
connect(Name, User, Password) ->
    connect(Name, 
	    ct:get_config({Name,ssh}), 
	    ct:get_config({Name,inet}), 
	    ct:get_config({Name,port}), 
	    User, Password,
	    ct:get_config({Name,default_prompt}),
	    ct:get_config({Name,print})).

%% @doc see connect/5.
connect(Name, User, Password, Print) when is_atom(Print) ->
    connect(Name, 
	    ct:get_config({Name,ssh}), 
	    ct:get_config({Name,inet}), 
	    ct:get_config({Name,port}), 
	    User, Password,
	    ct:get_config({Name,default_prompt}),
	    Print);
connect(Name, User, Password, Prompt) ->
    connect(Name, 
	    ct:get_config({Name,ssh}), 
	    ct:get_config({Name,inet}), 
	    ct:get_config({Name,port}), 
	    User, Password,
	    Prompt,
	    ct:get_config({Name,print})).

%% @spec connect(Name, User, Password, Prompt, Print) -> ok | {error, Reason}
%% @doc CLI connects to SUT with User/Password, Print overrides ct_hook setting.
%% ```Name = atom()                          Used as identifier.
%%    User = string()                        Username, default = "expert".
%%    Password = string()                    Password, default = "expert".
%%    Prompt = string()              
%%    Print = print | noprint                sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reason = term()
%%    Short versions of connect
%%    connect(Name).                         ct_hook print option is used
%%    connect(Name, Print).                  print or noprint overrides ct_hook setting
%%    connect(Name, User, Password).         User/Password overrides default. ct_hook print option is used
%%    connect(Name, User, Password, Print).  print or noprint overrides ct_hook setting
%%    connect(Name, User, Password, Prompt). own definition of prompt. ct_hook print option is used'''
connect(Name, User, Password, Prompt, Print) ->
    connect(Name, 
	    ct:get_config({Name,ssh}), 
	    ct:get_config({Name,inet}), 
	    ct:get_config({Name,port}), 
	    User, Password,
	    Prompt,
	    Print).

%% @spec connect(Name, IP, Port, User, Password, Prompt, Print) -> ok | {error,Reason}
%% @doc connects ssh to ipv4 cli port on SUT. Returns data of the connection, connection data is used in 'send' and 'disconnect'.<br/>
connect(Name, IP, Port, User, Password, Prompt, Print) ->
    connect(Name, IP, inet, Port, User, Password, Prompt, Print).

%% @spec connect(Name, IP, Inet, Port, User, Password, Prompt, Print) -> ok | {error,Reason}
%% @doc connects ssh to cli port on SUT. Returns data of the connection, connection data is used in 'send' and 'disconnect'.<br/>
%% ```CoRef = pid()       ssh_connection_ref.
%%    ChRef = integer()   ssh_channel_id'''
connect(Name, IP, Inet, Port, User, Password, Prompt, Print) ->
    case ct:get_config({Name,cli_or_coli}) of
	undefined ->
	    {error, {Name, not_configured_in_hook}};      
	CliOrColi ->
	    EtsProcName = make_etsprocname(CliOrColi),
	    case get_ets_pid(Name, EtsProcName) of
		{ok, _Pid} ->
		    print_connect(Name, IP, Port, User, Password, Print, {error, already_connected}),
		    {error, already_connected};
		{error, connection_not_found} ->
		    print_connect(Name, IP, Port, User, Password, Print, ok),
		    ConnectTimeout = ct:get_config({Name,connect_timeout}), 
		    Pid = spawn(?MODULE,start_ssh_session,[self(), Name, IP, Port, Prompt, [{inet,Inet},
											    {user, User},
											    {password, Password},
											    {silently_accept_hosts, true}, 
											    {user_interaction,false},
											    {connect_timeout,ConnectTimeout*1000}],
							   ConnectTimeout*1000]),
		    receive 
			{Pid, {ok, Reply}} ->
			    CliOrColi = ct:get_config({Name,cli_or_coli}),
			    EtsProcName = make_etsprocname(CliOrColi),
			    ets:insert(EtsProcName,{Name, Pid}),
			    print_reply(Name, Reply, Print),
			    ok;
			{Pid, {error, Reason}} ->
			    ct:log(lightred,"~p: ~p ~p Could not connect with ssh to SUT, Reason: ~p",[Name, ?MODULE, connect, {error, Reason}]),
			    {error, Reason}
		    after 60000 ->
			    {error, timeout_starting_session}
		    end
	    end
    end.

%% @doc see change_prompt/2.
change_prompt(Name, Prompt) ->
    change_prompt(Name, Prompt, ct:get_config({Name,print})).

%% @spec change_prompt(Name, Prompt, Print) -> ok | {error, Reason}
%% @doc Change expected prompt.
%%
%% For description of Prompt, please see <a href="http://www.erlang.org/doc/man/re.html#run-3">re:run/3</a>.
%% ```Name = atom()                Used as identifier.
%%    Prompt = string()            See argument RE in re:run/3.
%%    Print = print | noprint      sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reason = term()
%%    Short version of change_prompt
%%    change_prompt(Name, Prompt). Changes prompt, ct_hook print option is used'''
change_prompt(Name, Prompt, Print) ->
    case ct:get_config({Name,cli_or_coli}) of
       undefined ->
	    {error, {Name, not_configured_in_hook}};      
	CliOrColi ->
	    EtsProcName = make_etsprocname(CliOrColi),
	    case get_ets_pid(Name, EtsProcName) of
		{ok, Pid} ->
		    Result = send_to_ssh_session(Pid, {change_prompt, Prompt}),
		    print_change_prompt(Name, Prompt, Result, Print);
		{error, Reason} ->
		    ct:log(lightred,"~p: ~p ~p Could not change prompt, Reason: ~p",[Name, ?MODULE, change_prompt, {error, Reason}]),
		    {error, Reason}
	    end
    end.

%% @doc see send/5.
send(Name, Cmd) ->
    send(Name, Cmd, no_check, ct:get_config({Name,print}), 5000).
%% @doc see send/5.
send(Name, Cmd, Print) when is_atom(Print) ->
    send(Name, Cmd, no_check, Print, 5000);
%% @doc see send/5.
send(Name, Cmd, Match) when is_list(Match);
			    is_tuple(Match)->    
    send(Name, Cmd, Match, ct:get_config({Name,print}), 5000).    
%% @doc see send/5.
send(Name, Cmd, Match, Print) ->    
    send(Name, Cmd, Match, Print, 5000).    
%% @spec send(Name, Cmd, Match, Print, Timeout) -> {ok, {Reply, Matched}} | {error, Reason}
%% @doc Runs CLI command and match result, Print overrides ct_hook setting.
%%
%% For description of Match, please see <a href="http://www.erlang.org/doc/man/re.html#run-3">re:run/3</a>.
%% ```Name = atom()              Used as identifier.
%%    Cmd = string()             cli command you want to send.
%%    Match = RE | {RE, Options} See OTP re:run/3 documentation.
%%    RE = string()              string you want to match from the AccData.
%%    Options = list()           default = [dotall,{capture, all, list}].
%%    Timeout = integer()        Milliseconds to wait for prompt, default 5000 milliseconds.
%%    Print = print | noprint    sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reply = term()
%%    Matched = term()           Result from re:run/3
%%    Reason = term()
%%    Short versions of send
%%    send(Name, Cmd).           Does no check on reply, ct_hook print option is used
%%    send(Name, Cmd, Print).    Does no check on reply, print or noprint
%%    send(Name, Cmd, Check).    Checks reply, ct_hook print option is used'''
send(Name, Cmd, Match, Print, Timeout) ->
    do_send(Name, Cmd, Match, Print, Timeout).

do_send(Name, Cmd, Match, Print, Timeout) ->
    case ct:get_config({Name,cli_or_coli}) of 
       undefined ->
	    {error, {Name, not_configured_in_hook}};
       CliOrColi ->
	    EtsProcName = make_etsprocname(CliOrColi),
	    case get_ets_pid(Name, EtsProcName) of
		{ok, Pid} ->
		    Result = send_to_ssh_session(Pid, {send, list_to_binary(Cmd++"\n"), Timeout}),
		    print_send(Name,Cmd,Result, Print),
		    case Result of
			{error, Reason} ->
			    {error, Reason};
			ok ->
			    Reply = send_to_ssh_session(Pid, {wait_for_prompt, Timeout}),
			    % Remove echoed command
			    Reply2 = case re:run(Reply,"^"++Cmd++"\n(.*)",[dotall,{capture,[1],list}]) of
					 {match,[Rest]} -> Rest;
					 nomatch -> Reply
				     end,
			    case Match of
				no_check ->
				    print_reply(Name, Reply2, Print),
				    {ok, Reply2};
				Match ->
				    Result2 = check_reply(Reply2,Match),		    
				    print_check(Name, Match, Result2, Print),
				    Result2
			    end	    
		    end;
		{error, Reason} ->
		    ct:log(lightred,"~p: ~p ~p Could not find ssh connection, Reason: ~p",[Name, ?MODULE, send, {error, Reason}]),
		    {error, Reason}
	    end
    end.

%% @doc see asend/4.
asend(Name, Cmd) ->
    do_send(Name, Cmd, no_check, ct:get_config({Name,print}), 0).
%% @spec asend(Name, Cmd, Print) -> {ok, {Reply, Matched}} | {error, Reason}
%% @doc Runs CLI command and match result, Print overrides ct_hook setting.
%%
%% For description of Match, please see <a href="http://www.erlang.org/doc/man/re.html#run-3">re:run/3</a>.
%% ```Name = atom()              Used as identifier.
%%    Cmd = string()             cli command you want to send.
%%    Print = print | noprint    sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reply = term()
%%    Matched = term()           Result from re:run/3
%%    Reason = term()
%%    Short versions of send
%%    send(Name, Cmd).           Does no check on reply, ct_hook print option is used
%%    send(Name, Cmd, Print).    Does no check on reply, print or noprint
%%    send(Name, Cmd, Check).    Checks reply, ct_hook print option is used'''
asend(Name, Cmd, Print) when is_atom(Print) ->
    do_send(Name, Cmd, no_check, Print, 0).
	    
%% @doc see disconnect/2.
disconnect(Name) ->
    disconnect(Name, ct:get_config({Name,print})).

%% @spec disconnect(Name,Print) -> ok | {error, Reason}
%% @doc Disconnects from SUT, Print overrides ct_hook setting.
%% ```Name = atom()              Used as identifier.
%%    Print = print | noprint    sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reason = term()
%%    Short versions of send
%%    disconnect(Name).          Disconnects from SUT, ct_hook print option is used'''
disconnect(Name,Print) ->
    case ct:get_config({Name,cli_or_coli}) of
	undefined ->
	    {error, {Name, not_configured_in_hook}};
	CliOrColi ->
	    EtsProcName = make_etsprocname(CliOrColi),
	    case get_ets_pid(Name, EtsProcName) of	
		{ok, Pid} ->
		    send_to_ssh_session(Pid, disconnect),
		    ets:delete(EtsProcName, Name),
		    print_disconnect(Name, ok, Print);
		{error, _}->
		    print_disconnect(Name,ok,Print),
		    ok
	    end
    end.

print_change_prompt(Name, Prompt, Result, print) ->
    print_change_prompt(Name, Prompt, Result);
print_change_prompt(_Name, _Promp, _Resultt, noprint) ->
    ok.

print_change_prompt(Name, Prompt, ok) ->
    ct:log(internal,"~p CHANGE PROMPT: ~p ",[Name, Prompt]);
print_change_prompt(Name, Prompt, Reason) ->
    ct:log(lightred,"~p CHANGE PROMPT: ~p~~nFAILED: ~p",[Name, Prompt, Reason]).

print_connect(Name, IP, Port, User, Password, print, Result) ->
    print_connect(Name, IP, Port, User, Password, Result);
print_connect(_Name, _IP, _Port, _User, _Password, noprint, _Result) ->
    ok.

print_connect(Name, IP, Port, User, Password, ok) ->
    ct:log(internal,"~p CONNECTING: ~s ~p ~s ~s",[Name, IP, Port, User, Password]);
print_connect(Name, IP, Port, User, Password, Reason) ->
    ct:log(lightred,"~p CONNECTING: ~s ~p ~s ~s~~nFAILED: ~p",[Name, IP, Port, User, Password, Reason]).

print_check(Name,Check,Result,print) ->
    print_check(Name, Check, Result);
print_check(_Name,_Check,_Result,noprint) ->
    ok.
	    
%print_check(Name, Match,{ok, {Reply,Matched}}) when is_tuple(Match) ->
%    ct:log(internal,"~p Match RE: ~p ~s~nMATCHED: ~p",[Name,Match, Reply,Matched]);
print_check(Name, Match,{ok, {Reply,Matched}}) ->
    ct:log(internal,"~p Match RE: ~p ~s~nMATCHED: ~p",[Name,Match, Reply,Matched]);
print_check(Name, Match,{error, {Reply,{RE, Opts}}}) ->
    ct:log(lightred,"~p Match RE: ~p ~s~nFailed: ~p",[Name,Match, Reply,{RE, Opts}]).

check_reply(Reply, {RE, Opts})->
    case re:run(Reply, RE, Opts) of
	{match, Match} ->
	    {ok, {Reply,Match}};
	nomatch ->
	    {error, {Reply,{RE, Opts}}}
    end;
check_reply(Reply, RE) ->
    case re:run(Reply, RE,[dotall,{capture, all, list}]) of
	{match, [Match]} ->
	    {ok, {Reply,Match}};
	nomatch ->
	    {error, {Reply,{RE,[dotall,{capture, all, list}]}}}
    end.

print_reply(Name,Reply,print) ->
    ct:log(internal,"~p REPLY:~s",[Name,Reply]);
print_reply(_Name,_Reply,noprint) ->
    ok.

print_send(Name,Cmd,Result,print) ->
    print_send(Name,Cmd, Result);
print_send(_Name,_Cmd,_Result,noprint) ->
    ok.
	    
print_send(Name,Cmd,ok) ->
    ct:log(internal,"~p SEND:~s",[Name,Cmd]);
print_send(Name,Cmd,{error,Reason}) ->
    ct:log(lightred,"~p SEND:~s~nFAILED, Reason ~p",[Name,Cmd,Reason]).

print_disconnect(Name,Result,print) ->
    print_disconnect(Name, Result);
print_disconnect(_Name,_Result,noprint) ->
    ok.
	    
print_disconnect(Name,ok) ->
    ct:log(internal,"~p DISCONNECTED:",[Name]).
% Dialyzer complaint
%print_disconnect(Name,{error,Reason}) ->
%    ct:log(lightred,"~p DISCONNECTED FAILED, Reason ~p",[Name,Reason]).

wait_for_prompt(CoRef, ChId, Prompt, Timeout) ->
    wait_for_prompt(CoRef, ChId,<<>>, Prompt, Timeout).

wait_for_prompt(CoRef, ChId, Acc, Prompt, Timeout) ->
    receive 
	{ssh_cm, _, {data,0,0,Data}} ->
	    ssh_connection:adjust_window(CoRef, ChId, size(Data)),
	    case re:run(Data,Prompt,[]) of
		{match,_} ->		    
		    wait_for_prompt(CoRef, ChId, iolist_to_binary([Acc]++[Data]),Prompt, 200);
		nomatch ->
		    wait_for_prompt(CoRef, ChId, iolist_to_binary([Acc]++[Data]),Prompt, Timeout)
	    end
    after Timeout -> 
%	    ssh_connection:adjust_window(CoRef, ChId, size(Acc)),
	    binary_to_list(Acc)
    end.

%% @hidden
start_ssh_session(TCPid, Name, IP, Port, Prompt, Options, ConnectTimeout) ->
    case ssh:connect(IP, Port, Options, ConnectTimeout) of
	{ok, CoRef} ->
	    {ok,ChId} = ssh_connection:session_channel(CoRef, 5000),
	    ok = ssh_connection:shell(CoRef, ChId),
	    Reply = wait_for_prompt(CoRef, ChId, Prompt, 5000),
	    TCPid ! {self(), {ok, Reply}},
	    loop_ssh_session(Name, CoRef, ChId, Prompt);
	 {error, Reason} ->
	    TCPid ! {self(), {error, Reason}}
    end.

loop_ssh_session(Name, CoRef, ChId, Prompt) ->
    receive
	{TCPid, {send, Cmd, Timeout}} ->
	    TCPid ! {self(), ssh_connection:send(CoRef, ChId, Cmd, Timeout)},
	    loop_ssh_session(Name, CoRef, ChId, Prompt);
	{TCPid, {wait_for_prompt, Timeout}} ->
	    TCPid ! {self(), wait_for_prompt(CoRef, ChId, Prompt, Timeout)},
	    loop_ssh_session(Name, CoRef, ChId, Prompt);
	{TCPid, {change_prompt, Prompt2}} ->
	    TCPid ! {self(),ok},
	    loop_ssh_session(Name, CoRef, ChId, Prompt2);
	{TCPid, disconnect} ->
	    ssh_connection:close(CoRef, ChId),
	    TCPid ! {self(), ssh:close(CoRef)};    
	{ssh_cm,CoRef,{closed,ChId}} ->
	    CliOrColi = ct:get_config({Name,cli_or_coli}),
	    ct:log(yellow,"~p (~p connection) closed from server side",[Name,CliOrColi])
	end.

send_to_ssh_session(Pid, Msg) ->
    case is_process_alive(Pid) of 
	true ->
	    Pid ! {self(), Msg},
	    receive 
		{Pid, Reply} ->
		    Reply
	    end;
	false ->
	    {error, ssh_session_not_alive}
    end.

%%% @hidden

cli_ets_proc(From, EtsProcName) -> 
    ets:new(EtsProcName,[ordered_set,public,named_table]),
    From ! {EtsProcName,ok},
    receive
        {Pid,stop} ->
            ets:delete(EtsProcName),
	    Pid ! {EtsProcName,stopped}
    end.

stop_ets_proc(EtsProcName) ->
    case whereis(EtsProcName) of
        Pid when is_pid(Pid) -> 
	    EtsProcName ! {self(),stop},
	    receive
		{EtsProcName,stopped} -> ok
	    end;
        _ -> 
	    ok
    end.

get_ets_pid(Name, EtsProcName)->
    case ets:info(EtsProcName) of
       undefined ->
	    {error, connection_not_found};
       _ ->
	    case ets:lookup(EtsProcName,Name) of
		[{Name,Pid}] ->
		    {ok, Pid};
		[] ->
		    {error, connection_not_found}
	    end
    end.

make_etsprocname(CliOrColi) ->
    list_to_atom(atom_to_list(CliOrColi) ++ "_" ++ atom_to_list(?MODULE)).

%% @private  
get_ip(Name) ->
    N = ct:get_config({Name,n}), 
    IPType = ct:get_config({Name,ip_type}), 
    get_ip(N, Name, IPType).

%% @private  
get_ip(N, Name, IPType) ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" -> 
	    {ok, "localhost"};
	_ -> % "target" or "cloudish"
	    case proplists:get_value(ssh, rct_multi_node_cfg:get_config(N,IPType)) of
		undefined ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter ssh in ~p for ~p in node~p",[IPType,Name,N]});
		IP ->
		    {ok,IP}
	    end
    end.
	
%% @private
get_port(Name, CliOrColi, TgtPort) ->
    N = ct:get_config({Name,n}), 
    get_port(N, Name, CliOrColi, TgtPort).

%% @private
get_port(N, Name, CliOrColi, TgtPort) ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    {ok, TgtPort};
	"cloudish" ->
	    {ok, TgtPort};
	"sim" -> 
            User = os:getenv("USER"),
            {ok, HostName} = inet:gethostname(),          
            ErlNode = list_to_atom(User ++ "@" ++ HostName),
            case rpc:call(ErlNode, sysEnv, get_port_conf, [CliOrColi]) of
                Port when is_integer(Port) ->
                    {ok,Port};
                Other ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not get ~p port for ~p (~p) in node~p, Reason ~p",[CliOrColi,Name,ErlNode,N,Other]})
            end
    end.

%% @spec update_config(Name,ConfigLine) -> ok | no_configuration_found | not_affected_by_update
%% @doc callback function for updating configuration data.
%% ```Name = atom()                          Alias for cli or coli towards node.
%%    ConfigLine = tuple                     {ssh_lmt_ipv4, [{ssh, IP}, {port, Port}, {user, User}, {password, Password}]} '''
update_config(Name, Update) ->
    IPType=ct:get_config({Name,ip_type}), 
    update_config(Name,IPType,Update).

update_config(Name, IPType, {IPType, Data}) ->
    Config = ct:get_config(Name),
    IP = proplists:get_value(ssh, Data),
    NewConfig=lists:keyreplace(ssh,1,Config,{ssh, IP}),
    rct_multi_node_cfg:remove_config(Name),
    ok = rct_multi_node_cfg:require(Name, NewConfig);
update_config(_,_,_) ->
    not_affected_by_update.
    
    

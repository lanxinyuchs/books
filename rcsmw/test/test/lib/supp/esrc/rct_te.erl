%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_te.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2017
%%% @version /main/R10A/2

%%% @doc ==Common test hook for collecting logs==
%%% This module contains a common test hook for collecting data 
%%% written by te log read on target or simulated nodes during a 
%%% testcase.
%%%
%%% The collected logfiles can be searched for certain strings.
%%%
%%% Hook format:
%%% ```{rct_te, [{N, Name, Fail, Success, Opts}]}'''
%%%
%%% There are short formats when running towards single node:
%%% ```{rct_te, Fail                   expands to {rct_te, [{1, te1, Fail, [], []}]}
%%%    {rct_te, {Fail, Success}        expands to {rct_te, [{1, te1, Fail, Success, []}]}
%%%    {rct_te, {Fail, Success, Opts}} expands to {rct_te, [{1, te1, Fail, Success, Opts}]}'''
%%% 
%%% There are short formats when running towards clustered nodes:
%%% ```{rct_te, [Fail | {Fail, Success} | {Fail, Success, Opts}]}  expands to {rct_te, [{1, te1, Fail1, Success1, Opts1},{2, te2, Fail2, Fail2, Success2},...]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simulated environment.
%%%    Name     = atom()                         Used as identifier
%%%    Fail     = [string()]                     NOTE: regexp characters MUST be \\
%%%    Success  = [string()]                     NOTE: regexp characters MUST be \\
%%%    Opts     = dont_clear_te_log |            Will not do "te log clear" at beginning of testcase. Useful when system is installed
%%%               {connect_timeout, integer()} | (seconds) used for TLM where ssh connect is slow, default 5 seconds
%%%               {retries, integer()} |         default 5 , number of retries that will be made when ssh connect fails, default 5
%%%               {retry_delay, integer()}       (seconds) default 1 second, approximate time between ssh connect retries'''
%%% For target environment it is required that config variables below is specified in stp.cfg file
%%% ```{ssh_lmt_ipv4, [{ssh, string()}, {port, integer()}, {user, string()}, {password, string()}]}'''
%%% Examples:<br/>
%%% Hook below will fetch trace and error log but not search for stings inside it
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_te, []}]}].'''
%%%
%%% Hook below will fetch trace and error log and fail if the string "error" is found
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_te, ["error"]}]}].'''
%%%
%%% Hook below will fetch trace and error log, count the number of lines containing "error" and subtract with the number of lines containing "ok". If Sum is !0 the testcase will fail.
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_te, ["error"], ["ok"]}]}].'''
%%%
%%% Hook below will fetch logs from two nodes.
%%%```suite() -> 
%%%       [{ct_hooks, [{rct_te, [{["error"], ["ok"]},{["error"], ["ok"]}]}]}],'''
%%% @end


-module(rct_te).
-id('Updated by CCase').
-vsn('/main/R10A/2').
-date('2017-04-28').
-author('etxkols').
-include_lib("common_test/include/ct.hrl").
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R10A/1     2017-04-25 etxkols     Created
%%% R10A/2     2017-04-28 etxkols     Fixed compiler warning
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([clear_te_log/1,
	 change_te_parameters_in_this_tc/3,
	 init/2,
	 pre_init_per_suite/3,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 terminate/1,
	 update_config/2,
	 ssh_connect/6,
	 ets_proc/1]).

-define(WRITE_FILE_SLEEP, 2000). % Erlang seems to buffer output to erlang.log.x
-define(TIMEOUT_FETCH_FILE,  10000).
-define(SSH_CONNECT_TIMEOUT, 5).
-define(SSH_CONNECT_RETRIES_NO, 30).
-define(SSH_CONNECT_RETRY_DELAY,1).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #--------------------------------------------------------- 

%%% @doc Manually run te log clear from a testcase.
clear_te_log(N) when is_integer(N) -> 
    clear_te_log(list_to_atom("te" ++ integer_to_list(N)));
clear_te_log(Name) -> 
    te_log_clear(Name, true).

%%% @doc Changes trace and error log search parameters for a testcase.
%%% 
%%% ```Used for changing parsing of logs from a testcase, see description of paramaters hook description above
%%% 
%%%    Examples:
%%%    rct_te:change_te_parameters_in_this_tc(te1,["ERROR"],["blaha"]).'''
change_te_parameters_in_this_tc(Name, Fail, Success) ->
    put(list_to_atom("change_te_parameters_in_this_tc_" ++ atom_to_list(Name)),[Fail, Success]).

%%% @hidden
init(_Id, Opts) ->
    R = init(Opts, 1, []),
    {ok, R}.

init([], _, _) ->
    [{1, te1, [], [], []}];
init(Tuple, N, R) when is_tuple(Tuple)  -> 
    init([Tuple], N, R);
init([Tuple|T], N, R) when is_tuple(Tuple) -> 
    init2([Tuple|T], N, R);
init([Fail|T], N, R) when is_integer(hd(Fail)) ->
    init([[Fail|T]], N, R);
init([Fail|T], N, R) when is_list(hd(Fail)) ->
    init2([Fail|T], N, R).
	    
init2([], _, R) -> 
    R;
init2([Fail|T], N, R) when is_list(Fail) -> 
    init2(T, N+1, R ++ [{N, list_to_atom("te" ++ integer_to_list(N)), Fail , [], []}]);
init2([{Fail, Success}|T], N, R) -> 
    init2(T, N+1, R ++ [{N, list_to_atom("te" ++ integer_to_list(N)), Fail, Success, []}]);
init2([{Fail, Success, Opts}|T], N, R) -> 
    init2(T, N+1, R ++ [{N, list_to_atom("te" ++ integer_to_list(N)), Fail, Success, Opts}]).

%%% @hidden
%%% @doc Verifies that wanted logs are valid and checks that config variables exist for target.
pre_init_per_suite(_Suite,Config = {fail,_},States) ->
    {Config,States};
pre_init_per_suite(_Suite,Config = {skip,_},States) ->
    {Config,States};
pre_init_per_suite(_Suite,Config,States) ->
    case do_pre_init_per_suite(States,[]) of
	ok ->
	    AliasToHooks = [{Name, {N, ?MODULE}}||{N,Name,_,_,_}<-States],		
	    NewConfig = case proplists:get_value(alias_to_hooks,Config) of
			    undefined ->
				Config ++ [{alias_to_hooks,AliasToHooks}];
			    OldAliasToHooks ->
				lists:keyreplace(alias_to_hooks,1,Config,{alias_to_hooks,OldAliasToHooks ++ AliasToHooks})
			end,
	    {NewConfig, States};
	Fail ->
	    {Fail, States}
    end.

fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.

%%% Verify that wanted logs are valid and check that config variables exist for target.
do_pre_init_per_suite([],R) ->
    ok = rct_multi_node_cfg:require(?MODULE,R);
do_pre_init_per_suite([{N, Name, Fail, Success, Opts}|T], R) ->
    SimOrTarg = os:getenv("SIM_OR_TARGET"),
    case SimOrTarg of
	TargetOrCloud when TargetOrCloud == "target";
			   TargetOrCloud == "cloudish" ->
	    Board = atom_to_list(ct:get_config({test_nodes,N})),
	    case rct_multi_node_cfg:get_config(N,ssh_lmt_ipv4) of
		undefined ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[ssh_lmt_ipv4,Name,N]});
		[{ssh,IP},{port,Port},{user,User},{password,Pwd}] ->
		    ColumnInfo = Board ++ "(" ++ IP ++ ")",
		    rct_multi_node_cfg:require(Name,[N, Fail, Success, Opts, Board, IP, Port, User, Pwd, ColumnInfo]),
		    do_pre_init_per_suite(T, R ++ [{Name,Board,IP,Port,User,Pwd,ColumnInfo}]);
		_LoginData ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[{ssh_lmt_ipv4,[ssh,port,user,password]},Name,N]})
	    end;
	"sim" ->
	    {ok, HostName} = inet:gethostname(),          
	    Username = os:getenv("USER"),
	    ErlNode = list_to_atom(Username ++ "@" ++ HostName),
	    case rpc:call(ErlNode, file, get_cwd, []) of
		{ok,_CWD} ->
		    ColumnInfo = atom_to_list(ErlNode),
		    rct_multi_node_cfg:require(Name,[N, Fail, Success, Opts, ColumnInfo]),
		    do_pre_init_per_suite(T, R ++ [{ColumnInfo}]);
		Other ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not get cwd for ~p, Reason: ~p in node ~p",[ErlNode,Other,N]})
	    end
    end.

%%% @hidden
%%% @doc Store logfile data in etstable, i.e to be used at post_end_per_testcase when logs are fetched.
pre_init_per_testcase(_TC,Config = {fail,_},States) -> {Config,States};
pre_init_per_testcase(_TC,Config = {skip,_},States) -> {Config,States};
pre_init_per_testcase(TC,Config,States) ->
    case lists:keysearch(priv_dir, 1, Config) of
	false ->
	    {{fail, no_priv_dir}, States};
	{value,{priv_dir, PrivDir}} ->
	    register(?MODULE, spawn(?MODULE, ets_proc, [self()])),
	    receive
		ok -> 
		    do_pre_init_per_testcase(TC,Config,States),
		    ets:insert(?MODULE,{priv_dir,PrivDir}),
		    {Config, States}
	    after 1000 ->
		    {{fail, trace_and_error_ets_table}, States}
	    end
    end.

%%% Store data in ets table ?MODULE, i.e. logfile names in common test and logfile names and number of lines on node.
do_pre_init_per_testcase(_TC,_Config,[]) ->
    ok;
do_pre_init_per_testcase(TC,Config,[{_N, Name, _Fail, _Success, _Opts}|T]) ->
    te_log_clear(Name, false),
    do_pre_init_per_testcase(TC,Config,T).
    
te_log_clear(Name, Clear) ->
    [N, _Fail, _Success, Opts, _Board, IP, Port, User, Pwd, _ColumnInfo] = 
	case os:getenv("SIM_OR_TARGET") of
	    "sim" ->
		{sim,sim,sim,sim,sim,sim,sim,sim,sim,"SimColumnInfo"};
	    "cloudish" ->
		ct:get_config(Name);
	    "target" ->
		ct:get_config(Name)
	end,   
    case rct_node_state:get_state(N) of
	down ->
	    ct:log("Node ~p in node ~p is marked as down, will not attempt to clear trace and error logs",[Name, N]);
	_ ->
	    ConnectTimeout = proplists:get_value(connect_timeout,Opts,?SSH_CONNECT_TIMEOUT),
	    Retries = proplists:get_value(retries,Opts,?SSH_CONNECT_RETRIES_NO),
	    RetryDelay = proplists:get_value(retry_delay,Opts,?SSH_CONNECT_RETRY_DELAY),
	    case lists:member(dont_clear_te_log,Opts) of
		true ->
		    case Clear of
			false ->
			    ok;
			true ->
			    clear(N, IP, Port, User, Pwd, ConnectTimeout, Retries, RetryDelay)
		    end;
		false ->
		    clear(N, IP, Port, User, Pwd, ConnectTimeout, Retries, RetryDelay)
	    end
    end.

clear(N, IP, Port, User, Pwd, ConnectTimeout, Retries, RetryDelay) ->
    case ssh_connect(IP, Port, User, Pwd, ConnectTimeout, Retries, RetryDelay) of
	{ok, Pid} ->
	    exec(Pid, "te log clear", 5000),
	    ssh_close(Pid);
	{error, _Reason} -> 
	    ct:pal(yellow,"~p:~p line ~p Target node ~p is down, will not clear trace and error logs",[?MODULE, ?FUNCTION_NAME, ?LINE, N])
    end.
    
%%% @hidden
%%% @doc Fetches the logfiles and searches for strings, also destroys etstable created in pre_init_per_testcase.
post_end_per_testcase(TC,Config,Return,States) ->
    case ets:info(?MODULE) of % In case previous ct_hook has failed, Config is not reliable
	undefined ->
	    {{fail, no_priv_dir}, States};
	_ ->
	    case ets:lookup(?MODULE, priv_dir) of % In case previous ct_hook has failed, Config is not reliable
		[] ->
		    {{fail, no_priv_dir}, States};
		[{priv_dir, TCLogPath}] ->
		    R = do_post_end_per_testcase(TC, TCLogPath,Config,States,[]),
		    R2 = print_logs(R),
		    case whereis(?MODULE) of
			Pid when is_pid(Pid) -> 
			    ?MODULE ! {self(),stop},
			    receive
				stopped -> ok
			    end;
			_ -> 
			    ok
		    end,
		    case R2 of
			{ok,_} -> {Return, States}; % LogParameters ONLY used in one testcase
			_ -> {{fail, trace_and_error_fault}, States}
		    end
	    end
    end.
		  
do_post_end_per_testcase(_TC, _TCLogPath,_Config,[],R) ->
   R;
do_post_end_per_testcase(TC, TCLogPath,Config,[{_N, Name, _Fail, _Success, _Opts}|T],R) ->   
    LogName = atom_to_list(TC) ++ "_" ++ atom_to_list(Name) ++ ".log",
    FilePath = filename:join(TCLogPath,LogName),
    [N, Fail, Success, Opts, _Board, IP, Port, User, Pwd, ColumnInfo] = 
	case os:getenv("SIM_OR_TARGET") of
	    "sim" ->
		{sim,sim,sim,sim,sim,sim,sim,sim,sim,"SimColumnInfo"};
	    "cloudish" ->
		ct:get_config(Name);
	    "target" ->
		ct:get_config(Name)
	end,   
    case rct_node_state:get_state(N) of
	down ->
	    do_post_end_per_testcase(TC, TCLogPath,Config,T,R);
       _ ->
	   ConnectTimeout = proplists:get_value(connect_timeout,Opts,?SSH_CONNECT_TIMEOUT),
	   Retries = proplists:get_value(retries,Opts,?SSH_CONNECT_RETRIES_NO),
	   RetryDelay = proplists:get_value(retry_delay,Opts,?SSH_CONNECT_RETRY_DELAY),
	   Result = case ssh_connect(IP, Port, User, Pwd, ConnectTimeout, Retries, RetryDelay) of
			{ok, Pid} ->
			    case exec(Pid, "te log read", 5000) of
				{ok, Bin} -> 
				    ssh_close(Pid),
				    case file:open(FilePath, [write]) of
					{ok, FileId} ->
					    file:write(FileId, binary_to_list(Bin)),
					    file:close(FileId),
					    case get(list_to_atom("change_te_parameters_in_this_tc_" ++ atom_to_list(Name))) of
						undefined ->
						    [{ok,{ColumnInfo,FilePath,LogName,Fail,Success}}];
						[Fail2, Success2] ->
						    [{ok,{ColumnInfo,FilePath,LogName,Fail2,Success2}}]
					    end;
					{error,Reason} ->
					    ct:log(lightred,"~p:~p line ~p Can not open log file ~s, Reason: ~p",[?MODULE, ?FUNCTION_NAME, ?LINE, FilePath, Reason]),
					    [{error, {ColumnInfo, Reason}}]
				    end;
				{error,Reason} ->
				    ct:log(lightred,"~p:~p line ~p Can not execute te log read, Reason: ~p",[?MODULE, ?FUNCTION_NAME, ?LINE, Reason]),
				    [{error, {ColumnInfo, Reason}}]
			    end;
			{error,Reason} ->
			    ct:log(lightred,"~p:~p line ~p Can not do ssh connect to ~s, Reason: ~p",[?MODULE, ?FUNCTION_NAME, ?LINE, IP, Reason]),
			    [{error, {ColumnInfo, Reason}}]
		    end,
	    do_post_end_per_testcase(TC, TCLogPath,Config,T,R ++ Result)
    end.

%% @hidden
%% Clean away items in ct_attributes
terminate(CthState) ->
    do_terminate(CthState),
    rct_multi_node_cfg:remove_config(?MODULE),
    ok.

do_terminate([]) ->
    ok;
do_terminate({_Logs, _Filter}) ->
    do_terminate([log]);
do_terminate({_Logs, _Filter, _Opts}) ->
    do_terminate([log]);
do_terminate([{_N, Name, _Sname, _Logs, _Filter}|T]) ->
    do_terminate([Name|T]);
do_terminate([{_N, Name, _Sname, _Logs, _Filter, _Opts}|T]) ->
    do_terminate([Name|T]);
do_terminate([Name|T]) ->
    rct_multi_node_cfg:remove_config(make_name_module(Name)),
    do_terminate(T).
	    
%%% @hidden
ets_proc(From) -> 
    ets:new(?MODULE,[ordered_set,public,named_table]),
    From ! ok,
    receive
        {Pid,stop} ->
            ets:delete(?MODULE),
	    Pid ! stopped
    end.
	    
print_logs([]) ->
    {ok,[]};
print_logs(Blades) ->
    format_html("<b><font size=\"3\">Trace and Error log</font></b>"),
    format_html("<table border=\"1\">~n"
		"<tr>~n"),
    [format_html("<td>~s</td>~n",[element(1,Data)])||{_Result,Data}<-Blades],
    format_html("</tr>~n"),
    format_html("<tr>~n"),
    R2=print_log_type(Blades,[]),
    format_html("</tr>~n"
		"</table>~n"),
    look_for_error(R2).

print_log_type([],R) ->
    R;
print_log_type([{ok, {_ColumnInfo,Path,Name,Bad,Good}}|T],R) ->
    R2= case file:read_file_info(Path) of
	    {error,enoent} ->
		format_html("<td>~s</td>", [Name]),
		{ok,Path};
	    {ok,_} ->
		case egrep(Bad,Path,2) of
		    {ok, NBad} ->
			case egrep(Good,Path,2) of
			    {ok, NGood} ->
				case NBad-NGood of
				    N when N =:= 0, NBad =:= 0 ->
					format_html("<td><a href=\"~s\">~s</a></td>", [Path, Name]),
					{ok,Path};
				    N when N =:= 0, NBad > 0 ->
					format_html("<td><a href=\"~s\">~s</a><font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, NBad]),
					{ok,Path};
				    N when N > 0, NGood =:= 0  ->
					format_html("<td><a href=\"~s\">~s</a><font color=red>(~p ERRORS)</font></td>", [Path, Name, N]),
					{error,Path};
				    N when N > 0, NGood > 0  ->
					format_html("<td><a href=\"~s\">~s</a><font color=red>(~p ERRORS)</font> <font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, N, NGood]),
					{error,Path};
				    N when N < 0, NBad =:= 0 ->
					format_html("<td><a href=\"~s\">~s</a><font color=red>(-~p ERRORS)</font></td>", [Path, Name, N]),
					{error,Path};
				    N when N < 0, NBad > 0 ->
					format_html("<td><a href=\"~s\">~s</a><font color=red>(-~p ERRORS)</font> <font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, N, NBad]),
					{error,Path}
				end;
			    {error, Reply} ->
				format_html("<td><a href=\"~s\">~s</a><font color=red>(regexp ERROR ~p)</font></td>", [Path, Name, Reply]),
				{error,Path}
			end;
		    {error, Reply} ->
				format_html("<td><a href=\"~s\">~s</a><font color=red>(regexp ERROR ~p)</font></td>", [Path, Name, Reply]),
			{error,Path}
		end
	end,
    print_log_type(T,R++[R2]);
print_log_type([{error, {_ColumnInfo,Reason}}|T],R) ->
    format_html("<td><font color=red>~p</font></td>", [{error, Reason}]),
    print_log_type(T,R++[{error,Reason}]).

   
look_for_error(L) ->
    case lists:keysearch(error,1,L) of 
        {value,_} -> {error,L}; 
        false     -> {ok,L}
    end.

format_html(String) ->
    ct:log(default, 1, String, [], [no_css]).

format_html(String,Args) ->
    ct:log(default, 1, String, Args, [no_css]).

%te:egrep(["ERROR REPORT","CRASH REPORT"],"/home/test34/ct/log/rst_scx_0_0_erlang.log").
egrep([],_,_) ->
    {ok, 0};
egrep(L,File,N) ->
    [T|H] = lists:reverse(L),
    Cmd=lists:flatten("egrep '(" ++ [X++"|"||X<-lists:reverse(H)]++T++")' "++File++" | wc -l"),
%"egrep '(ERROR REPORT|CRASH REPORT)' erlang.log.1 | wc -l"
    Reply = os:cmd(Cmd)--"\n",
    case re:run(Reply,"^[0-9]+$",[]) of
	nomatch -> 
	    case {Reply, N} of
		{Reply,0} ->
		    ct:log(yellow,"~p:egrep/3 os:cmd(~p) returned ~p", [?MODULE, Cmd, Reply]),
		    {error, Reply};
		_ ->
		    ct:log(yellow,"~p:egrep/3 os:cmd(~p) returned ~p, retrying ~p times", [?MODULE, Cmd, Reply, N]),
		    timer:sleep(1000),
		    egrep(L,File,N-1)
	    end;
	{match,_} -> {ok, list_to_integer(Reply)}
    end.

exec(sim, Cmd, _TMO) ->
    {ok, list_to_binary(os:cmd(Cmd))};
exec(Pid, Cmd, TMO) ->
    ssh_exec(Pid, Cmd, TMO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SSH PART SSH PART SSH PART SSH PART SSH PART SSH PART %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%rct_te:ssh("137.58.180.130",22,"root","root","uname -a", 10000).
%io:format("~p",[[rct_te:ssh("137.58.180.130",22,"root","root","uname -a", 10000)||_<-lists:seq(1,500)]]).
%%% ssh_connect(IP, Port, User, Pwd) -> {ok, Pid} | {error, 
ssh_connect(sim, sim, sim, sim, _,_, _) ->
    {ok, sim};
ssh_connect(IP, Port, User, Pwd, ConnectTimeout, NoRetries, RetryDelay) ->
    crypto:start(),
    ssh:start(),
    Pid = spawn(?MODULE, ssh_connect, [self(), {IP, Port, User, Pwd}, connect, ConnectTimeout, NoRetries, RetryDelay]),
    receive {Pid, Result} -> Result end.

ssh_exec(Pid, Cmd, TMO) ->
    Pid ! {exec, self(), Cmd, TMO},
    receive {Pid, Result} -> Result end.
    
ssh_close(sim) ->
    ok;
ssh_close(Pid) ->
    Pid ! {close, self()},
    receive {Pid, Result} -> Result end.

%%% @hidden
ssh_connect(From, LoginData, connect, ConnectTimeout, Retries, RetryDelay) ->
    case do_ssh_connect(LoginData, "", ConnectTimeout, Retries, RetryDelay) of
	{ok, SSH} ->
	    From ! {self(), {ok, self()}},
	    ssh_connected_loop(From, LoginData, SSH, ConnectTimeout, Retries, RetryDelay);
	Error ->
	    From !  {self(), Error}
    end.

% {ok, SSH} | {error, Reason}
do_ssh_connect(_LoginData, {error, Reason}, _, 0, _) ->
    {error, Reason};
do_ssh_connect(LoginData={IP, Port, User, Pwd}, _Result, ConnectTimeout, Retries, RetryDelay) ->
    case ssh:connect(IP,Port,[{user,User},
			      {password,Pwd},
			      {silently_accept_hosts, true}, 
			      {user_interaction,false},
                              {connect_timeout,ConnectTimeout*1000}],ConnectTimeout*1000) of
	{ok, SSH} ->
	    {ok, SSH};
	{error, Reason} ->
	    ct:log(yellow,"~p: ssh connect ~p failed, Retrying ~p time(s) with ~p seconds delay, Reason: ~p",[?MODULE, IP, Retries, RetryDelay, {error, Reason}]),
	    timer:sleep(RetryDelay * 1000),
	    do_ssh_connect(LoginData, {error, Reason},ConnectTimeout, Retries-1, RetryDelay)
    end.

ssh_connected_loop(From, LoginData, SSH, ConnectTimeout, Retries, RetryDelay) ->
    receive
	{close, From} ->
	    ssh:close(SSH),
	    From ! {self(), ok};
	{exec, From, Cmd, TMO} ->
	    {SSH2, Reply} = ssh_channel(SSH,LoginData, Cmd,TMO, "", ConnectTimeout, Retries, RetryDelay),
	    From ! {self(), Reply},
	    ssh_connected_loop(From, LoginData, SSH2, ConnectTimeout, Retries, RetryDelay);
	Other ->
	    ct:log(yellow,"!!!!!!!!!!!!!!!!!! ~p: ~p",[?MODULE, Other]),
	    ssh_connected_loop(From, LoginData, SSH, ConnectTimeout, Retries, RetryDelay)
end.

%{SSH,{ok,Reply}} | {SSH,{error, Reason}}
ssh_channel(SSH,_LoginData,_Cmd,_TMO,{error, Reason},_,0, _) ->
    {SSH,{error, Reason}};
ssh_channel(SSH,LoginData,Cmd, TMO,_Status,ConnectTimeout,Retries, RetryDelay) ->
    Result = case SSH of
		 disconnected ->
		     do_ssh_connect(LoginData, "",ConnectTimeout, Retries, RetryDelay);
		 SSH ->
		     {ok,SSH}
	     end,
    case Result of
	{ok, SSH2} ->
	    case do_ssh_channel(SSH2,Cmd,TMO) of
		{ok,Reply} ->
		    {SSH2,{ok,Reply}};
		{error, Reason} ->            
		    ct:log(yellow,"~p: ssh channel failed, Retrying ~p time(s), Reason: ~p",[?MODULE, Retries, {error, Reason}]),
		    timer:sleep(1000),
		    ssh_channel(disconnected, LoginData, Cmd,TMO,{error, Reason},ConnectTimeout,Retries-1, RetryDelay)
	    end;
	{error, Reason} ->
	    {SSH,{error, Reason}}
    end.

do_ssh_channel(SSH,Cmd,TMO) ->
    debug("CMD~n~p ~p", [SSH,Cmd]),    
    case ssh_connection:session_channel(SSH, 5000) of
	{ok, Chn} ->
	    Exec = case ssh_connection:exec(SSH, Chn, Cmd, 2000) of
		       success ->
			   do_recv_response(SSH, Chn, [], TMO);
		       failure ->
			   {error, {ssh_connection, exec, failure}};
                       % Undocumented return values
		       {error,Reason} ->
			   ct:log(yellow,"~p: !!!!! Undocumented return value from ssh_connection:exec/4 ~p",[?MODULE, {error,Reason}]),
			   {error, {ssh_connection, exec, Reason}};
		       {closed,Ch} ->
			   ct:log(yellow,"~p: !!!!! Undocumented return value from ssh_connection:exec/4 ~p",[?MODULE, {closed,Ch}]),
			   {error, {ssh_connection, exec,{closed,Ch}}};
		       Other ->
			   ct:log(yellow,"~p: !!!!! Undocumented return value from ssh_connection:exec/4 ~p",[?MODULE, Other]),
			   {error, {ssh_connection, exec,Other}}
		   end,
	    ssh_connection:close(SSH, Chn),
	    Exec;
	{error, Reason} ->
	    {error, {ssh_connection, session_channel, Reason}};
        % OTP ssh bug??????????
	{closed,Ch} ->
	    ct:log(yellow,"############# ~p: CHANNEL_CLOSED ~p",[?MODULE, Ch]),
	    {error,{ssh_connection, session_channel, {closed,Ch}}}
    end.
   
do_recv_response(SSH, Chn, Data, Timeout) ->
%    io:format("process_info do_recv_response ~p",[erlang:process_info(self())]),
    receive	
	{ssh_cm, SSH, {closed,Chn}} ->
	    debug("CLSD~n~p ~p", [SSH,Chn]),
	    {ok,list_to_binary(lists:reverse(Data))};

	{ssh_cm, SSH, {data,Chn,_,NewData}} ->
	    ssh_connection:adjust_window(SSH, Chn, size(NewData)),
	    debug("RECVD~n~p ~p ~p", [SSH, Chn, binary_to_list(NewData)]),
	    do_recv_response(SSH, Chn, [NewData|Data], Timeout);

	{ssh_cm, SSH, {eof,Chn}} ->
	    debug("RECVD EOF~n~p ~p", [SSH,Chn]),
	    do_recv_response(SSH, Chn, Data, Timeout);	    

	{ssh_cm, SSH, {exit_signal,Chn,Signal,Err,_Lang}} ->
	    debug("RECVD exit_signal~n~p ~p ~p ~p", [SSH,Chn,Signal,Err]),
	    do_recv_response(SSH, Chn, Data, Timeout);

	{ssh_cm, SSH, {exit_status,Chn,Status}} ->
	    debug("RECVD exit_status~n~p ~p ~p", [SSH,Chn,Status]),
	    do_recv_response(SSH, Chn, Data, Timeout);

	Other ->
	    ct:log(yellow,"########### ~p: ~p", [?MODULE, Other]),
	    debug("UNEXPECTED MESSAGE~n~p ~p ~p", [SSH,Chn,Other]),
	    do_recv_response(SSH, Chn, Data, Timeout)

    after Timeout ->
	    debug("SSHTIMEOUT~n~p ~p ~p", [SSH,Chn,Data]),
%	    {timeout,list_to_binary(lists:reverse(Data))}
	    {error, {timeout,list_to_binary(lists:reverse(Data))}}
    end.

debug(F,A) ->
%    io:format("~p" ++ F ++ "~n",[time()] ++ A).
    {F,A}.

make_name_module(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(?MODULE)).

%% @spec update_config(Name,ConfigLine) -> ok | no_configuration_found | not_affected_by_update
%% @doc callback function for updating configuration data.
%% ```Name = atom()                          Alias for cli or coli towards node.
%%    ConfigLine = tuple                     {ssh_lmt_ipv4, [{ssh, SSH}, {port, Port}, {user, User}, {password, Password}]} '''
update_config(Name, {ssh_lmt_ipv4, [{ssh, SSH}, {port, Port}, {user, User}, {password, Pwd}]}) ->
    case ct:get_config(Name) of
	undefined -> 
	    no_configuration_found;
	[N, Fail, Success, Opts, Board, _SSH, _Port, _User, _Pwd, _ColumnInfo] ->
	    rct_multi_node_cfg:remove_config(Name),
	    ok = rct_multi_node_cfg:require(Name,[N, Fail, Success, Opts, Board, SSH, Port, User, Pwd, Board ++ "(" ++ SSH ++ ")"])
    end;
update_config(_Name,_Config) ->
    not_affected_by_update.


%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_tlib.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/4
%%%
%%% @doc Module to check cpuload and memory usage on target node using ct_ssh.
%%% 
%%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each testcases.<br/>
%%% Hook formats:
%%% ```{rct_tlib, [{N, Name, Opts}]}'''
%%%
%%% There are 2 short formats when running towards one card:
%%% ```{rct_tlib, Name}         expands to {rct_tlib, [{1, Name, []}]}
%%%    {rct_tlib, {Name, Opts}} expands to {rct_tlib, [{1, Name, Opts}]}'''
%%% 
%%% There are 2 short formats when running towards clustered noded:
%%% ```{rct_tlib, [Name1,Name2]}         expands to {rct_tlib, [{1, Name1, []}{2, Name2, []}]}
%%%    {rct_tlib, [{Name1, Opts1},{Name2, Opts2}}] expands to {rct_tlib, [{1, Name1, Opts1},{1, Name2, Opts2}]}'''
%%% 
%%% Argument description:
%%% ```N         = integer()                      Used to match card in stp.cfg file.
%%%    Name      = atom()                         Used as identifier
%%%    Opts      = [opt()]               
%%%    opt()     = {cpumemory, Cpumemory} | {cpuload, Cpuload}
%%%    Cpumemory = integer()                      Max allowed CPU memory usage in Mbytes
%%%    Cpuload   = integer()                      Max allowed total and cores CPU load in % (0-100)'''
%%% If Opts contain opt(), the values will be checked after each testcase and testcase fails if condition is not met.
%%% 
%%% It is required that config variables below is specified in stp.cfg file
%%% ```{ssh_lmt_ipv4, [{ssh, string()}, {port, integer()}, {user, string()}, {password, string()}]}'''
%%% Example no check after testcase, but checks inside testcase:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_tlib, kalle}]}].
%%%    mytest(_) ->
%%%        {ok,_} = rct_tlib:cpuload(kalle, 1),
%%%        {ok,_} = rct_tlib:cpumemory(kalle, 43).'''
%%% Example checks are made after each testcase:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_tlib, {kalle,[{cpumemory, 30},{cpuload,1}]}}]}].
%%%    mytest(_) ->
%%%        test.'''
%%% @end
-module(rct_tlib). 
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R3A/R4A/4').
-date('2016-05-26').
-author('etxkols').
%%% -----------------------------------------------------------------------------
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
%%%
%%% The information in this document is the property of Ericsson.
%%%
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-06-14 etxkols     Created
%%% R2A/1      2012-10-24 etxkols     Handle simulated env 
%%% R2A/2      2012-10-26 etxkols     Updated ssh:connect options
%%% R2A/3      2013-01-16 etxivri     Added miscellaneous functions,
%%%                                   that could be useful in other SUITEs.
%%% R2A/4      2013-02-01 etxivri     Updated miscellaneous functions.
%%% R2A/5      2013-03-20 etxivri     Added cores CPU load checks.
%%% R2A/6      2013-03-20 etxkols     Print top when load is to high.
%%% R2A/7      2013-10-07 etxivri     Increase timeout value when wait for com to be started.
%%% R2A/8      2013-10-08 etxkols     More robust max mem measurement.
%%% R2A/10     2013-11-29 etxivri     Added get_filename, use this to add 
%%%                                   HW type on filename string.
%%% R2A/11     2014-03-05 etxivri     Added som more functions. useful for write to file.
%%% R2A/12     2014-03-06 etxivri     Update to use old file name if rev is R2A.
%%% R2A/13     2014-03-06 etxivri     Added get filename thar add rev on filename.
%%%                                   Update to correct way to get branch to 
%%%                                   add on filename, instead of rev.
%%%                                   Add writeDataToFile_AddNode.
%%% R2A/14     2014-03-26  etxkols    Faulty init/2 return value
%%% R2A/15     2014-09-01  etxkivri   Update get_branch
%%% R3A/1      2014-12-01  etxkivri   Use SwInventory when get sw version.
%%% R4A/1      2015-06-03  etxkols    Cluster fixes
%%% R4A/2      2016-03-09  etxkols    5G
%%% R4A/3      2016-05-19  etxkols    New output from linux cmd free in wr8
%%% R4A/4      2016-05-26  etxkols    free to support 3 formats
%%% -----------------------------------------------------------------------------

-export([cpuload/2]).
-export([cpumemory/2]).
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([post_end_per_testcase/4]).
-export([terminate/1]).

%% Miscellaneous functions
-export([get_used_memory_size/1]).
-export([writeDataToFile/4]).
-export([wait_for_com_to_start/1]).
-export([get_filename/1]).
-export([get_sw_version/1]). %% Need cli hook to be used from test_SUITE
-export([get_branch/0]).


-include_lib("common_test/include/ct.hrl").
%$RCT_TOP/test/bin/rct_run.sh -stp dus004 -shell -pa /home/etxkols/RCS/cpuload/
%rct_multi_node_cfg:require(kalle,1,{ssh_lmt_ipv4,[ssh,port,user,password]}).
%rct_tlib:cpload(kalle,0). 
%rct_tlib:memory(kalle,42). 

%%% @doc Checks that memory usage is less or equal to MaxMem Mbytes using command free<br/>
%%% ===Arguments===
%%% Name   - ID of node<br/>
%%% MaxMem - Maximum allowed memory usage in Mbytes<br/>
%%% Memory - Used memory in Mbytes<br/>
-spec cpumemory(Name::atom(), MaxMem::integer()) -> {ok, Memory::integer()} | {error, {memory, Memory::integer()}} | {error, Reason::term()}.
cpumemory(Name, MaxMem) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ct:log(lightred,"~p: CPU memory usage NOT supported in SIMULATED environment",[Name]);
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    case get_data(Name, "free", 10000) of	     
		{ok,Result} ->
		    R2 = string:tokens(Result,"\n"),
		    [BuffersCache] = case [S||S<-R2, string:str(S,"-/+ buffers/cache:") == 1] of
					 [BuffersAndCache] -> % 16A and early 16B
					     ct:pal("R4, R5, R6"),
					     [BuffersAndCache];
					 [] -> % late 16B and onwards
					     ct:pal("R7"),
					     [S||S<-R2, string:str(S,"Mem:") == 1]
				     end,
%		    [BuffersCache] = [S||S<-R2, string:str(S,"-/+ buffers/cache:") == 1],
%		    [BuffersCache] = [S||S<-R2, string:str(S,"Mem:") == 1],
		    io:format("R2 ~p",[R2]),
		    Mem = round(list_to_integer(lists:nth(3,string:tokens(BuffersCache," "))) / 1000),
		    case Mem =< MaxMem of
			true ->
			    ct:log(lightgreen,"~p: CPU memory usage ~p Mb (=< ~p Mb)",[Name, Mem, MaxMem]),
			    {ok,Mem};
			false ->
			    ct:log(lightred,"~p: CPU memory usage ~p Mb (=< ~p Mb)~n~s",[Name, Mem, MaxMem, Result]),
			    {error, {memory, Mem}}
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end
    end.

%%% @doc Checks CPU load is less or equal to MaxLoad (0-100%) using command vmstat<br/>
%%% ===Arguments===
%%% Name    - ID of node<br/>
%%% MaxLoad - Maximum allowed total and cores CPU load in % 0-100<br/>
%%% Load    - CPU load in % 0-100<br/>
-spec cpuload(Name::atom(), MaxLoad::integer()) -> {ok, [{Cpu::string(),Load::integer()}]} | {error, {load, Load::integer()}} | {error, Reason::term()}.
cpuload(Name, MaxLoad) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ct:log(lightred,"~p: CPU load NOT supported in SIMULATED environment",[Name]);
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    case get_data(Name, "cat /proc/stat | grep '^cpu[0-9]* '", 5000) of
		{ok,Then1} ->
%		    ct:pal("lightgreen ~s",[Then1]),
		    Then2 = string:tokens(Then1,"\n"),
		    Then = [string:tokens(X," ")||X<-Then2],
		    timer:sleep(5000),
		    case get_data(Name, "cat /proc/stat | grep '^cpu[0-9]* '", 5000) of
			{ok,Now1} ->
%			    ct:pal("lightgreen ~s",[Now1]),
			    Now2 = string:tokens(Now1,"\n"),
			    Now = [string:tokens(X," ")||X<-Now2],
			    Loads = calc_load(Now, Then, []),
			    CheckedLoads = check_loads(Loads, MaxLoad, {ok,[]}),
			    Color = case CheckedLoads of
					{ok, PrintLoads} ->
					    lightgreen;
					{error, PrintLoads} ->
					    lightred
				    end,
			    ct:pal(Color,"~p: ~s~n~s~s",[Name, 
							 lists:flatten([io_lib:format("~12.12s", [CPU]) || {CPU,_} <- Loads]),
							 string:copies(" ", length(atom_to_list(Name)) + 2),
							 lists:flatten([io_lib:format("~12.12s", [Load]) || Load <- PrintLoads])]),				
			    {Result, _} = CheckedLoads,
			    case Result of
				error ->
				    case get_data(Name, "top -b -n 1", 5000) of
					{ok, Top} -> ct:pal("~s",[Top]);
					_ -> ok
				    end;
				_ -> ok
			    end,
			    {Result, Loads};
			{error, Reason} ->
			    {error, Reason}
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end
    end.
  
check_loads([], _MaxLoad, {R, Res}) ->
    {R, Res};
check_loads([{_CPU ,Load}|T], MaxLoad, {_R, Res}) when Load > MaxLoad ->
    check_loads(T, MaxLoad, {error, Res ++ [lists:flatten(io_lib:format("~.1f > ~s",[Load, integer_to_list(MaxLoad)]))]});
check_loads([{_CPU ,Load}|T], MaxLoad, {R, Res}) ->
    check_loads(T, MaxLoad, {R, Res ++ [lists:flatten(io_lib:format("~.1f",[Load]))]}).



calc_load([], [] ,R) ->
    R;
calc_load([N|NTail], [T|TTail] ,R) ->
    Usage = list_to_integer(lists:nth(2,N)) +
	list_to_integer(lists:nth(3,N)) +
	list_to_integer(lists:nth(4,N)) -
	list_to_integer(lists:nth(2,T)) -
	list_to_integer(lists:nth(3,T)) -
	list_to_integer(lists:nth(4,T)),
    Total = Usage + 
	list_to_integer(lists:nth(5,N)) -
	list_to_integer(lists:nth(5,T)),
    calc_load(NTail, TTail, R ++ [{lists:nth(1,N), 100 * Usage / Total}]).

get_data(Name, Command ,Timeout) ->
    case ct_ssh:connect(Name,host,[{silently_accept_hosts, true}, {user_interaction,false}]) of
	{ok,_} ->
	    Result =  case ct_ssh:exec(Name,Command,Timeout) of
			  {ok, Reply} ->
			      {ok, Reply};
			  {error, Reason} ->
			      ct:log(lightred,"~p: ~p ~p Could not execute ~p, Reason: ~p",[Name, ?MODULE, get_data, Command, {error, Reason}]),
			      {error, Reason}
		      end,
	    case ct_ssh:disconnect(Name) of
		ok ->
		    Result;
		{error, Reason2}->
		    ct:log(lightred,"~p: ~p ~p Could not disconnect ssh from SUT, Reason: ~p",[Name, ?MODULE, get_data, {error, Reason2}]),
		    {error, Reason2}
	    end;
	{error, Reason}->
	    ct:log(lightred,"~p: ~p ~p Could not connect with ssh to SUT, Reason: ~p",[Name, ?MODULE, get_data, {error, Reason}]),
	    {error, Reason}
    end.
    
%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
%% @spec pre_init_per_suite(TC,Config,States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Verifies existence of config parameter ssh_lmt_ipv4.<br/>
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
    end.

do_pre_init_per_suite([],R,_SuiteType,_Num) ->
    R;
do_pre_init_per_suite([Name|T],R,SuiteType,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name,[]}|T],R,SuiteType,Num);
do_pre_init_per_suite([{Name,Opts}|T],R,SuiteType,Num) ->
    do_pre_init_per_suite([{Num,Name,Opts}|T],R,SuiteType,Num);
do_pre_init_per_suite([State = {N, Name, _Opts}|T],R,SuiteType,Num) ->
    BoardNum = rct_cluster_lib:get_target_du(N,SuiteType), 
    Board = ct:get_config({test_nodes,BoardNum}),
    case ct:get_config({Board,ssh_lmt_ipv4}) of
	SSH = [{ssh,_},
	       {port,_},
	       {user,_},
	       {password,_}] ->
	    rct_multi_node_cfg:require(Name,SSH),
	    do_pre_init_per_suite(T,R ++ [State],SuiteType,Num + 1 );
        Other ->
	    ct:log(lightred,"~p: ~p ~p Config parameters not matching {ssh_lmt_ipv4,[ssh,port,user,password]} for ssh to SUT, Reason: ~p",[Name, ?MODULE, do_pre_init_per_suite, {error, Other}]),
	    {{fail, Other}, State}
    end.

%% @hidden
%% @spec post_end_per_testcase(TC,Config,Return,States) -> {Return, States} | {{fail,Reason}, States}
%% @doc Checks CPU memory and load if Opts were given in hook<br/>
post_end_per_testcase(_TC,_Config,Return,States) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Return, States};
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    R = do_post_end_per_testcase(States,[]),
	    case lists:keysearch(error,1,R) of
		false ->
		    {Return, States};
		_ -> 
		    {{fail, rct_tlib_fault}, States}
	    end
    end.

do_post_end_per_testcase([],R) ->
    R;
do_post_end_per_testcase(Name,R) when is_atom(Name) ->
    do_post_end_per_testcase([{1, Name, []}],R);
do_post_end_per_testcase({Name, Opts},R) ->
    do_post_end_per_testcase([{1, Name, Opts}],R);
do_post_end_per_testcase([{_N, Name, Opts}|T],R) ->
    Mem = case lists:keysearch(cpumemory,1,Opts) of
	      {value,{cpumemory,MaxMem}} ->
		  cpumemory(Name, MaxMem);
	      false ->
		  {ok,dummy}
	  end,
    Load = case lists:keysearch(cpuload,1,Opts) of
	       {value,{cpuload,MaxLoad}} ->
		   cpuload(Name, MaxLoad);
	       false ->
		   {ok,dummy}
	   end,
    do_post_end_per_testcase(T,[Mem, Load | R]).
    
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

%% Miscellaneous functions
%% ===========================================================================
%% @doc
%% Check used RAM memory size. <br/>
%% Get memory size from, os:cmd "free" and "ps -eo fname,vsz,rss | egrep '(beam)|(com)'". <br/>
%% @spec get_used_memory_size(RPC_HookName) -> int
%% @end
%% ===========================================================================
get_used_memory_size(Name) ->
    ct:pal("### Get Used MemSize",[]),
    {ok,Free} = get_data(Name, "free", 5000),
    %% ct:pal("~p",[Free]),

    {match,[_,Tot]} = re:run(Free,"Mem: *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),

    %%%%
    %% Total used RAM size
    %%%%
    {match,[Tot_bc]} = re:run(Free,"buffers/cache: *([0-9]+).*",[{capture,[1],list}]),

    {ok, PS} = get_data(Name, "ps -eo fname,vsz,rss | egrep '(beam)|(com)'", 5000),

    %%%%
    %% Get used RAM size beam uses. 
    %%%%
    {match,[_Beam_vsz,Beam_rss]} = re:run(PS,"beam.smp *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),

    %%%%
    %% Get used RAM size com uses. 
    %%%%
    {match,[_Com_vsz,Com_rss]} = re:run(PS,"com *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),

    [Tot, Tot_bc, Beam_rss, Com_rss].


%% ===========================================================================
%% @doc
%% Write data to a file.<br/>
%% @spec writeDataToFile(FileDir, FileName, PrintOpts, MeasData) -> ok
%% @end
%% ===========================================================================
writeDataToFile(FileDir, FileName, PrintOpts, Data) ->
    {ok, FD} = file:open(FileDir ++ "/" ++ FileName, [append]),
    io:format(FD, PrintOpts, Data),
    file:close(FD),
    ok.

%% ===========================================================================
%% @doc
%% Check for COM started. <br/>
%% @spec wait_for_com_to_start(Name) -> timestamp
%% @end
%% ===========================================================================
wait_for_com_to_start(Name) ->
    wait_for_com_to_start(Name, 120000).

wait_for_com_to_start(_Name, Timeout) when Timeout < 5000 ->
    ct:fail("COM not started within max timeout.");

wait_for_com_to_start(Name, Timeout) ->
    %% case rct_rpc:call(rpc, os, cmd, ["pgrep com"], 50)  of
    case get_data(Name, "pgrep com", 5000) of
	{ok, []} ->
	    timer:sleep(5000),
	    wait_for_com_to_start(Name, Timeout - 5000);
	{ok, Com} ->
	    %% remove \n from the received list.
	    [ComPid|_] = string:tokens(Com, "\n "),
	    ComPid;
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(5000),
	    wait_for_com_to_start(Name, Timeout - 5000)
    end.

%% ===========================================================================
%% @doc
%% Add Branch and HW type on FileNameStr. <br/>
%% @spec get_filename(FileNameStr) -> FileName
%% @end
%% ===========================================================================
get_filename(FileNameStr) ->
    Branch = get_branch(),
    RecievedData = rct_rpc:call(rpc, os, cmd, 
				["cat /sys/rbs-fn/rbs-sys/board_type"], 
				5000, noprint),
    %% Clean upp HW_type string to a list of HW_type.
    [HW_type] = string:tokens(RecievedData, "\n "),
    ct:pal("### HW type: ~p",[HW_type]),
    case Branch of
	"R2" -> %% Use the old file names.
	    FileName = HW_type ++"_"++ FileNameStr;
	_ ->
	    FileName = Branch ++"_"++ HW_type ++"_"++ FileNameStr
    end,
    FileName.

%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version(Cli_Session) -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version(Cli_Session) ->
    ct:pal("### Get SW version",[]),
    CLI_Name = Cli_Session,
     rct_cli:connect(CLI_Name),
    {ok ,RecievedData} = 
	rct_cli:send(CLI_Name,"show ManagedElement=1,SystemFunctions=1,SwInventory=1"),
    %% ct:pal("RecievedData: ~p", [RecievedData]),

    %% Clean upp RecievedData string to a list of strings.
    Var = string:tokens(RecievedData, "=\r\n "),
    %% drop data until you reach "SwVersionMain", 
    %% the wanted CXS label is the second element.
    [_, CXS | _ ] = lists:dropwhile(fun(X) ->
					    X =/= "SwVersion" 
				    end, Var),
    ct:pal("CXS: ~p", [CXS]),
    rct_cli:disconnect(CLI_Name),

    list_to_atom(CXS).

%% ===========================================================================
%% @doc
%% Get branch. <br/>
%% Get branch. <br/>
%% @spec get_branch() -> Rev
%% @end
%% ===========================================================================
%% get_branch() ->    
%%     ct:pal("Get Branch to add on the measured file name", []),
%%     A = os:cmd("cleartool catcs | awk '{if($2 == \"BranchName\"){print $3}}'"),
%%     [Branch] = string:tokens(A, "\n"),
%%     ct:pal("Branch : ~p", [Branch]),
%%     Branch.

get_branch() -> 
    UpData = rct_rpc:call(rpc, swmI, get_current_up_metadata, [], 10000, print), 
    {productRevision, ProdRev} = lists:keyfind(productRevision, 1, UpData),
    ct:pal("ProdRev: ~p", [ProdRev]),
    {Branch, _} = lists:split(2, ProdRev),
    ct:pal("Branch : ~p", [Branch]),
    Branch.

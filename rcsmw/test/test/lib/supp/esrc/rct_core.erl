%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_core.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R8A/R9A/2
%%% @doc
%%%
%%% == Usage ==
%%%
%%% This module contains a common test hook for collecting core dumps 
%%% from target or simulated nodes after a testcase has been run.
%%%
%%% Hook formats:
%%% ```{rct_core, [{N, Opts}]}'''
%%%
%%% There are short formats when running towards one card:
%%% ```{rct_core, []}            expands to {rct_core, [{1, []}]}
%%%    {rct_core, [Opts]}        expands to {rct_core, [{1, Opts}]}'''
%%% There are short formats when running cluster:
%%%    {rct_core, [[],[]]} expands to {rct_core, [{1, []},{2, []}]}
%%%    {rct_core, [Opts1,Opts2]} expands to {rct_core, [{1, Opts1},{2, Opts2}]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simulated environment.
%%%    Name     = atom()                         Used as identifier
%%%    Opts     = [Opt]
%%%    Opt      = {connect_timeout, integer()} | (seconds) used for TLM where ssh connect is slow, default 5 seconds
%%%               {retries, integer()} |         Number of ssh connection test retries before starting to search for coredumps (default 5)
%%%               {retry_delay, integer()}       Delay between ssh connection tests if they fail (default 1 second)
%%%               {remove_cores, true | false}   Determines if coredumps should be deleted from node after they have been fetched (default true).
%%%                                              A user case for false, is that you want to detect and fetch coredumps, but want them
%%%                                              to remain on the node to be included in a ESI.
%%%'''
%%% For target environment it is required that config variables below is specified in stp.cfg file
%%% ```{ssh_lmt_ipv4, [{ssh, string()}, {port, integer()}, {user, string()}, {password, string()}]}'''
%%% A config parameter, {allowed_coredumps,AllowedDumps}, can be given to allow certain coredumps by matching AllowedDumps to the coredump names
%%% ```{allowed_coredumps,[string()]} Example {allowed_coredumps,["test_oi","eqmBBiblaha"]}.'''
%%% Examples:<br/>
%%% Hook below will search for coredumps at the end of each testcase. If found, the coredumps are fetched and the testcase will fail.
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_core, []}]}].
%%%
%%%    suite() -> 
%%%        [{ct_hooks, [{rct_core, [[{connect_timeout, 60}, {retries, 20}, {retry_delay, 5}]]}]}].
%%%
%%%    suite() -> 
%%%        [{ct_hooks, [{rct_core, core_1}]}].
%%%
%%%    suite() -> 
%%%        [{ct_hooks, [{rct_core, [{1, [{retries, 10},{retry_delay, 20}]},[]]}]}].'''
%%% @end
-module(rct_core).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R8A/R9A/2').
-date('2017-02-02').
-author('etxkols').
-include_lib("common_test/include/ct.hrl").
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
%%% R2A/1      2013-01-21 etxkols     Created
%%% R2A/2      2013-02-15 etxkols     Added coredump_test/0
%%% R2A/4      2013-02-15 etxkols     Added support for global 
%%%                                   disabling of fail on coredumps
%%% R2A/5      2013-03-07 etxkols     Increased silence timeout for ssh_copy
%%%                                   from 1000 ms to 5000 ms 
%%% R2A/6      2013-03-07 etxkols     Increased more timeouts
%%% R2A/7      2013-04-11 etxkols     Added support to reuse Aliases
%%% R2A/8      2013-06-27 etxjovp     Added support new coredump path
%%% R2A/9      2013-07-02 etxjovp     change COREDUMP_PATH_TARGET_ERRMAN
%%% R2A/10     2013-07-05 etxjovp     change ERRMAN_FILE
%%% R2A/11     2013-08-07 etxkols     fixes for new coredump handling.
%%%                                   OBS, simulated env does not generate cores
%%% R2A/12     2013-08-12 etxkols     Fixed timing issues for ebennik.
%%% R2A/13     2013-08-15 etxkols     Temporary fix for change of /dumps/pmd to /pmd.
%%% R2A/14     2013-08-30 etxkols     Fixed dialyzer fault.
%%% R2A/15     2013-09-12 etxkols     Rewritten ssh with retries.
%%% R2A/16     2013-09-17 etxkols     Changed ct:pal to ct:log.
%%% R2A/17     2014-02-03 etxkols     Temporary fix for ssh fault in R16B03.
%%% R2A/19     2014-02-17 etxkols     Improved printouts and more robust if other hooks fail
%%% R2A/21     2014-02-17 etxkols     Bugfix for sim
%%% R2A/22     2014-03-26 etxkols     Faulty init/2 return value
%%% R2A/23     2014-04-01 etxkols     Stupid ct:log format error
%%% R2A/24     2014-04-01 etxkols     Stupid ct:log format error
%%% R2A/25     2014-04-10 etxkols     temp dump dir will be /var/pmd in the future, not /pmd
%%% R2A/27     2014-05-12 etxkols     Longer polling times to suite node CI
%%% R2A/28     2014-05-16 etxkols     Added no_coredump_search/0
%%% R2A/29     2014-10-17 etxkols     Changed printout
%%% R3A/02     2015-03-31 eolaand     Added clause for SkipOrFail
%%% R4A/1      2015-05-26 etxkols     Prepare for cluster
%%% R4A/2      2015-05-27 etxkols     Fix for installation of regular MPs
%%% R4A/3      2015-06-01 etxkols     bug in terminate/1
%%% R4A/4      2015-10-06 etxkols     handle dumps in /rcs/dumps/pmd and existence of /rcs/dumps/appdumps
%%% R4A/5      2016-03-08 etxkols     5G
%%% R8A/1      2016-12-05 etxkols     Coredumps seems to be working in cloud now, so activating it
%%% R9A/1      2017-02-01 etxkols     Refactoring and support for multi instances in 5g cloud
%%% R9A/2      2017-02-02 etxkols     Removed comments
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([coredump_test/0,
	 no_coredump_search/0,
	 init/2,
	 pre_init_per_suite/3,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
         terminate/1,
	 update_config/2,
	 ssh_connect/10]).

%% New method of coredump handling on target.
%% Cores are first written to RAM dir /dumps/pmd (will be changed to /var/pmd/) and then moved to /rcs/dumps/[0-9]/
%% This means that /dumps/pmd must be checked for ongoing dumps and the dumps should
%% be fetched from /rcs/dumps/[0-9]/
%% A number of retries are made to ensure that coredumps are collected even if the board unexpectedly reboots
%% [{"Dir to check for ongoing coredumps", "Dir to fetch coredumps from", "Appended to core dump name"}]
%% [{"ls rexexp to search for cores", "Appended to core dump name"}]
-define(COREDUMP_PARAMS_TARGET,[{"/var/pmd/", "/rcs/dumps/[0-9]*/*", ""},
				{"/var/pmd/", "/rcs/dumps/pmd/[0-9]*/*", ""}]).
-define(COREDUMP_PARAMS_SIM,[{"", "rcs/dumps/core*", "_cs"}, % path extention releative to RCS_ROOT
			     {"", "rcs/comte/core", "_com"}]).
-define(SSH_CONNECT_TIMEOUT, 5).    % Default value for Opt = {connect_timeout, integer()}
                                    % Useful when testing towards TLM
-define(SSH_CONNECT_RETRIES_NO, 10). % Default value for Opt = {retries, integer()}
-define(SSH_CONNECT_RETRY_DELAY,1). % Default value for Opt = {retry_delay, integer()}

%%===========================================================================
%% @spec coredump_test() -> ok
%%
%% @doc Used for TESTING that coredumps can be generated and fetched.
%% In post_end_per_testcase a coredump is expected to have been generated for each node.
%% If not, testcase is failed. Copied coredump(s) are deleted.
%% This function call is only valid for the testcase that executed it, i.e. following testcases have normal behavior.<br/>
%%===========================================================================
coredump_test() ->
    put(coredump_test,true),
    ok.

%%===========================================================================
%% @spec no_coredump_search() -> ok
%%
%% @doc Will disable search for coredumps at end of testcase.<br/>
%%===========================================================================
no_coredump_search() ->
    put(no_coredump_search,true).

%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%%===========================================================================
%% @spec pre_init_per_suite(Suite, Config, CthStates) -> 
%%    {Config, CthStates} | {{fail,Reason}, CthStates}
%%
%% @doc Verifies CT config parameter ssh_lmt_ipv4. Makes a CT target_name() (Alias)
%% containing ssh_lmt_ipv4 data and paths to corefile locations<br/>
%%===========================================================================
pre_init_per_suite(_Suite,Config = {fail,_},States) -> {Config,States};
pre_init_per_suite(_Suite,Config = {skip,_},States) -> {Config,States};
pre_init_per_suite(Suite,Config,[]) ->
    pre_init_per_suite(Suite,Config,[[]]);
pre_init_per_suite(_Suite,Config,CthState) ->
    case os:getenv("SIM_OR_TARGET") of
	TargetOrCloud when TargetOrCloud == "target";
			   TargetOrCloud == "cloudish" ->
	    crypto:start(),
	    ssh:start();
	"sim" ->
	    ok
    end,
    case do_pre_init_per_suite(CthState,[],1) of
	{ok,CthState2} ->                               
	    AliasToHooks = [{Name, {N, ?MODULE}}||{N,Name,_,_}<-CthState2],		
	    NewConfig = case proplists:get_value(alias_to_hooks,Config) of
			    undefined ->
				Config ++ [{alias_to_hooks,AliasToHooks}];
			    OldAliasToHooks ->
				lists:keyreplace(alias_to_hooks,1,Config,{alias_to_hooks,OldAliasToHooks ++ AliasToHooks})
			end,
	    {NewConfig, CthState2};
	Other ->
	    {Other, CthState}
    end.

%%===========================================================
%% Verifies CT config parameter ssh_lmt_ipv4. Makes a CT target_name() 
%% (Alias) containing ssh_lmt_ipv4 data and paths to corefile locations
%%===========================================================
do_pre_init_per_suite([],R,_N) ->
    {ok,R};
do_pre_init_per_suite([Opts|T],R,Num) when is_list(Opts) ->
    Name = list_to_atom(atom_to_list(rcs_core_unique_name) ++ integer_to_list(Num)),
    do_pre_init_per_suite([{Num,Name,Opts}|T],R,Num);    
do_pre_init_per_suite([{N, Name, Opts}|T],R,Num) ->
    case login_data(N, Name) of
	{ok, LoginData} ->
	    case make_paths(N, Name) of
		{ok, Paths, Sname} ->
		    case rct_multi_node_cfg:require(make_name_module(Name), [LoginData] ++ [Paths]) of
			ok ->
			    do_pre_init_per_suite(T,R ++ [{N, Name, Sname, Opts}],Num+1);
			{error, Reason} ->
			    fail_generic(?FUNCTION_NAME,?LINE,{"Could not initiate config parameter ~p for ~p in node ~p, Reason:~p",[LoginData,Name,N,Reason]})
		    end;
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

%%===========================================================================
%% @spec pre_init_per_testcase(TC, Config, CthStates) -> 
%%    {Config, CthStates} | {{fail,Reason}, CthStates}
%%
%% @doc Stores ?config(priv_dir, Config) in process dictionary to be used as 
%% location to store copied corefiles.
%%===========================================================================
pre_init_per_testcase(_TC,Config, CthStates) ->
    case catch lists:keysearch(priv_dir, 1, Config) of % Not always list
	{value,{priv_dir, PrivDir}} -> % timetrap may destroy Config variable, store it here
	    put(rct_core_priv_dir,PrivDir);
	_False -> % Config destroyed in previous hook, retry in post_end_per_testcase
	    ok  
    end,
    {Config, CthStates}.

%%===========================================================================
%% @spec post_end_per_testcase(TC, Config, Return, CthStates) -> 
%%    {Return, CthStates} | {{fail,Reason}, CthStates}
%%
%% @doc Searches, copies and removes corefiles from target or simulated environment
%% and stores them in priv_dir. If corefiles exist, a table with links are printed
%% in html log.
%%===========================================================================
post_end_per_testcase(TC,Config,Return,CthStates) ->
    case get(no_coredump_search) of
	true ->
	    ct:log("NO search for coredumps"),
	    {Return, CthStates};
	_ ->
	    TCLogPath = case get(rct_core_priv_dir) of
			    undefined -> % Config destroyed in pre_init_per_testcase, retry here
				case catch lists:keysearch(priv_dir, 1, Config) of	    
				    {value,{priv_dir, PrivDir}} -> 
					PrivDir;
				    _False -> % Config destroyed in previous hook, exit
					error
				end;
			    PrivDir ->
				PrivDir
			end,
	    case TCLogPath of
		error ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Config variable destroyed while searching for priv_dir",[]});
		TCLogPath ->
		    SimOrTarg = os:getenv("SIM_OR_TARGET"),
			    Result = do_post_end_per_testcase(SimOrTarg, TC, TCLogPath,CthStates,[]),
			    case print_result(Result) of
				ok -> 
				    ct:log("No coredump found"),
				    {Return, CthStates};
				Other ->
				    {Other, CthStates}
			    end
	    end
    end.

%%===========================================================
%% Searches, copies and removes corefiles from target or simulated environment
%% and stores them in priv_dir.
%%===========================================================
do_post_end_per_testcase(_SimOrTarg, _TC, _TCLogPath, [],R) ->
    R;
do_post_end_per_testcase(SimOrTarg, TC, TCLogPath,[{N, Name, Sname, Opts}|T],R) ->
    Header = make_header_name(SimOrTarg, N, Sname),
    case rct_node_state:get_state(N) of
	down ->
	    ct:log("Node ~p in node ~p is marked as down, will not attempt to search for logfiles",[Name, N]),
	    do_post_end_per_testcase(SimOrTarg, TC,TCLogPath,T,R ++ [{Header, ok}]);
	_ ->
	    ConnectTimeout = proplists:get_value(connect_timeout,Opts,?SSH_CONNECT_TIMEOUT),
	    Retries = proplists:get_value(retries,Opts,?SSH_CONNECT_RETRIES_NO),
	    RetryDelay = proplists:get_value(retry_delay,Opts,?SSH_CONNECT_RETRY_DELAY),
	    put(ssh_timeout_opts,{ConnectTimeout, Retries, RetryDelay}),
	    put(remove_cores,proplists:get_value(remove_cores,Opts,true)),
	    [Host, CoreDumpInfo] = ct:get_config(make_name_module(Name)),
	    case os:getenv("SIM_OR_TARGET") of
		"sim" ->
		    ct:log("Search for coredumps on ~s ~p",[Host, [X||{_,X,_}<-CoreDumpInfo]]);
		TargetOrCloud when TargetOrCloud == "target";
				   TargetOrCloud == "cloudish" ->
		    Node = ct:get_config({test_nodes,N}),
		    {IP,_,_,_} = Host,
		    ct:log("Search for coredumps on ~p (~s)",[Node, IP])
	    end,
	    Board = ct:get_config({test_nodes,N}),
	    LogNamePrefix = atom_to_list(TC) ++ "_" ++ atom_to_list(Board) ++"_",
	    case start_searching(SimOrTarg, Host, CoreDumpInfo, LogNamePrefix, TCLogPath, 3) of 
		ok ->
		    do_post_end_per_testcase(SimOrTarg, TC,TCLogPath,T,R ++ [{Header, ok}]);
		{ok,Fetched} ->
		    do_post_end_per_testcase(SimOrTarg,TC, TCLogPath,T,R ++ [{Header, Fetched}]);
		{error, Reason} ->
		    do_post_end_per_testcase(SimOrTarg, TC,TCLogPath,T,R ++ [{Header, [{error, Reason}]}])
	    end
    end.

%%% @hidden
terminate([]) ->
    ok;
terminate([{_,Name,_,_}|T]) ->
    rct_multi_node_cfg:remove_config(make_name_module(Name)),
    terminate(T).

%%===========================================================
%% On Target, Retries number of test attempts to connect to node with ssh is made
%% If ssh connect test succeeds, search, fetch and remove of coredumps is started.
%% If ssh connect test fails or ongoing coredump collection is interupted, the whole procedure will be repeated 2 times 
%% ok | {error, List} | {ok, [{ok,{"/proj/rcs-tmp/stps/dus014/logs/ct_run.dus014@esekilxxen3016.2014-04-24_16.09.43/RCS.rct_netconf.basic_tests_SUITE.nc_get.logs/run.2014-04-24_16.09.46/log_private/nc_get_core_pmd-test_app-3008-2014-04-24-14-03-14.tgz","nc_get_core_pmd-test_app-3008-2014-04-24-14-03-14.tgz"}}]} 	
%%===========================================================
start_searching("target",_, _, _, _, 0) ->
    {error, could_not_fetch_coredumps};
start_searching("cloudish",_, _, _, _, 0) ->
    {error, could_not_fetch_coredumps};
start_searching(TargetOrCloud,Host = {IP, Port, User, Pwd}, CoreDumpInfo, LogNamePrefix, TCLogPath, Retry)  when TargetOrCloud == "target";
														 TargetOrCloud == "cloudish" ->
    {_, Retries, RetryDelay} = get(ssh_timeout_opts),
    case ssh_cmd(IP, Port, User, Pwd, "ls",Retries, RetryDelay, 5000) of % ssh connection test
	{ok,_} ->
	    case get_dumps(Host, CoreDumpInfo, LogNamePrefix, TCLogPath) of
		ok ->
		    ok;
		{error, Reason} ->
		    ct:log(yellow,"ssh to ~p failed while seaching for coredumps, Retrying ~p time(s), Reason ~p:~p ~p",[IP, Retry - 1, ?MODULE,start_searching, {error, Reason}]),
		    start_searching(TargetOrCloud,Host, CoreDumpInfo, LogNamePrefix, TCLogPath, Retry - 1);
		{ok,Fetched} ->
		    {ok,Fetched}
	    end;
	Other -> 
	    ct:log(yellow,"ssh connection test to ~p before start seaching for coredumps failed, Retrying ~p time(s), Reason ~p:~p ~p",[IP, Retry - 1, ?MODULE,start_searching, Other]),
	    start_searching(TargetOrCloud,Host, CoreDumpInfo, LogNamePrefix, TCLogPath, Retry - 1)
    end;
start_searching("sim",Host, CoreDumpInfo, LogNamePrefix, TCLogPath, _Retry) ->
    get_dumps(Host, CoreDumpInfo, LogNamePrefix, TCLogPath).

%%===========================================================
%% Search, fetch and remove coredumps
%% ok | {ok, [{ok,{"/proj/rcs-tmp/stps/dus014/logs/ct_run.dus014@esekilxxen3016.2014-04-24_16.09.43/RCS.rct_netconf.basic_tests_SUITE.nc_get.logs/run.2014-04-24_16.09.46/log_private/nc_get_core_pmd-test_app-3008-2014-04-24-14-03-14.tgz","nc_get_core_pmd-test_app-3008-2014-04-24-14-03-14.tgz"}}]} | {error, List} 
%%===========================================================
get_dumps(Host, CoreDumpInfo, LogNamePrefix, TCLogPath) ->
    case search_for_cores(Host, CoreDumpInfo, LogNamePrefix, []) of
	[] -> % no cores
	    ok;
	{error, Reason} ->
	    {error, Reason};	    
	{timeout, Data} ->
	    {error, {timeout, Data}};	    
	Cores ->
	    case fetch_cores(Host, Cores, TCLogPath, []) of
		{ok, Fetched} ->
		    case get(remove_cores) of
			true -> remove_cores(Host, Cores);
			_    -> ok
		    end,
		    {ok, Fetched};
		{error,Reason} ->
		    {error,Reason}
	    end
    end.

%%===========================================================
%% Searches for corefiles from target or simulated environment
%% Return values from sim and target
%% [{ok,{"/local/scratch/etxkols/RCS_ROOT/home/etxkols/core","mytest_core_cs"}},
%%  {ok,{"/local/scratch/etxkols/RCS_ROOT/rcs/comte/core","mytest_core_com"}}]
%% [{ok,{"/dumps/core-du1-com-1357819376-11-3132-1000-100","mytest_core-du1-com-1357819376-11-3132-1000-100"}}] | {timeout, Data} | {error, Reason}
%%===========================================================
search_for_cores(_Host, [], _LogNamePrefix, R) ->
    R;
search_for_cores(Host, [{TempRE, DumpsRE, Extension}|T], LogNamePrefix, R) ->
    Result = case Host of
		 "localhost" ->	     
		     {ok, os:cmd("ls " ++ DumpsRE)}; % Cannot use erlang since environment variable may be in path
		 {_, _, _, _} ->
		     search_for_cores2(Host,TempRE, DumpsRE)
	     end,
    Dumps = case Result of 
		{ok, CoreFiles} ->
		    case re:run(CoreFiles, "No such file or directory") of
			{match,_} ->
			    [];
			nomatch ->
			    [{ok, {File, LogNamePrefix ++ filename:basename(File) ++ Extension}}
			     || File <- string:tokens(CoreFiles, "\n")]
		    end; 
		Other ->
		    timer:sleep(100), % Timeout just to finish printout to html to not corrupt coredumps table 
		    Other
	    end,
    search_for_cores(Host, T, LogNamePrefix, R ++ Dumps).

%%===========================================================
%% New method of coredump handling on target.
%% Cores are first written to RAM dir /dumps/pmd and then moved to /rcs/dumps/[0-9]/
%% This means that /dumps/pmd must be checked for ongoing dumps and the dumps should
%% be fetched from /rcs/dumps/[0-9]/
%% {ok, Data} | {timeout, Data} | {error, Reason}
%%===========================================================
search_for_cores2(Host, TempRE, DumpsRE) ->
    case search_temp_dir(Host, TempRE, 120) of
	ok ->
	    search_dumps_dir(Host, DumpsRE, "", 10);
	Other ->
	    Other
    end.

%%===========================================================
%% If there are files in /rcs/dumps/[0-9]/, check once more
%% to assure that file properties have not changed.
%% This may occur while copying dumps from /dumps/pmd/.
%% Repeated checks are made every second
%% {ok, Data} | {timeout, Data} | {error, Reason} 
%%===========================================================
search_dumps_dir(_, DumpsRE, _, 0) ->
    ct:log(lightred,"coredumps dir ~s still writes dumps after retries, ~p:~p ", [DumpsRE, ?MODULE, search_dumps_dir]),
    {error, coredir_still_writes_dumps};
search_dumps_dir(Host = {IP, Port, User, Pwd}, DumpsRE, LastLS, N) ->    
    case ssh_cmd(IP, Port, User, Pwd, "ls -l " ++ DumpsRE, 5000) of
	{ok, NewLS} ->
	    %%%%%%%%%%%%%%%%%%  WORKAROUND FOR BUG IN OTP R16B03  %%%%%%%%%%%%%%%%%%%
	    %%%%%%%%%%%%%%%%%% LENGTH INDICATOR INCLUDED IN REPLY %%%%%%%%%%%%%%%%%%%
%	    NewLS2 = search_for_length(NewLS,[]),
%	    io:format("LastLS ~p~nNewLS ~p~nNewLS2 ~p",[LastLS,NewLS,NewLS2]),
%	    case NewLS2 == LastLS of
            %%%%%%%%%%%%%%%%%% END WORKAROUND FOR BUG IN OTP R16B03 %%%%%%%%%%%%%%%%%%%
	    case NewLS == LastLS of
		true ->
		    ssh_cmd(IP, Port, User, Pwd, "ls " ++ DumpsRE, 5000);
		false ->
		    timer:sleep(1000),
	    %%%%%%%%%%%%%%%%%%  WORKAROUND FOR BUG IN OTP R16B03  %%%%%%%%%%%%%%%%%%%
	    %%%%%%%%%%%%%%%%%% LENGTH INDICATOR INCLUDED IN REPLY %%%%%%%%%%%%%%%%%%%
%		    search_dumps_dir(Host, DumpsRE, NewLS2, N-1)
            %%%%%%%%%%%%%%%%%% END WORKAROUND FOR BUG IN OTP R16B03 %%%%%%%%%%%%%%%%%%%
		    search_dumps_dir(Host, DumpsRE, NewLS , N-1)
	    end;
	Other ->
	    ct:log(lightred,"ssh connection to ~p failed while searching for coredumps, Reason ~p:~p ~p", [IP, ?MODULE, search_dumps_dir, Other]),
	    Other
    end.

%%%%%%%%%%%%%%%%%%  WORKAROUND FOR BUG IN OTP R16B03  %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% LENGTH INDICATOR INCLUDED IN REPLY %%%%%%%%%%%%%%%%%%%
%% search_for_length([],R) ->
%%     R;
%% search_for_length([0|T],R) ->
%%     [_,_,_|D] = T,
%%     search_for_length(D,R);
%% search_for_length([H|T],R) ->
%%     search_for_length(T,R ++ [H]).
%%%%%%%%%%%%%%%%%% END WORKAROUND FOR BUG IN OTP R16B03 %%%%%%%%%%%%%%%%%%%


%%===========================================================
%% If there are files in /pmd/, wait until they are moved
%% away to /rcs/dumps/[0-9]/
%% Repeated checks are made every second
%% ok | {timeout, Data} | {error, Reason}
%%===========================================================
search_temp_dir(_, TempRE, 0) ->
    ct:log(lightred,"Temporary coredumps dir ~s (ARM) /dumps/pmd (PPC) still contains dumps after retires, ~p:~p", [TempRE, ?MODULE, search_temp_dir]),
    {error, temp_coredir_contains_files};
search_temp_dir(Host = {IP, Port, User, Pwd}, TempRE, N) ->
    case ssh_cmd(IP, Port, User, Pwd, "ls " ++ TempRE, 5000) of
	{ok,[]} ->
	    ok;
	{ok, CoreFiles} ->
	    case re:run(CoreFiles, "No such file or directory") of
		{match,_} ->
		    ct:log(lightred,"temporary coredumps dir ~s does NOT exist, ~p:~p", [TempRE, ?MODULE, search_temp_dir]),
		    {error, temp_coredir_noexist};
		nomatch ->
		    timer:sleep(1000),
		    search_temp_dir(Host, TempRE, N-1)
	    end; 
	Other ->
	    ct:log(lightred,"ssh connection failed while searching for coredumps, Reason ~p:~p ~p", [Other, ?MODULE, search_temp_dir]),
	    Other
    end.
	       
%%===========================================================
%% Copies corefiles
%% {ok | error,[{ok,{"/proj/rcs-tmp/stps/dus014/logs/ct_run.dus014@esekilxxen3016.2014-04-24_16.09.43/RCS.rct_netconf.basic_tests_SUITE.nc_get.logs/run.2014-04-24_16.09.46/log_private/nc_get_core_pmd-test_app-3008-2014-04-24-14-03-14.tgz","nc_get_core_pmd-test_app-3008-2014-04-24-14-03-14.tgz"}} | {timeout, Data} | {error, Reason}]}
%%===========================================================
fetch_cores(_Host, [], _TCLogPath, R) ->
    case [Result ||{Result,_}<- R, (Result == timeout) or (Result == error)] of
	[] -> {ok,R};
	_ -> {error,R}
    end;
fetch_cores(Host, [{ok, {Core, LogName}}|T], TCLogPath, R) ->
    Log = filename:join(TCLogPath, LogName),
    Result = case Host of
		 "localhost" ->	     
		     file:copy(Core, Log);
		 {IP, Port, User, Pwd} ->
		     ssh_copy(IP, Port, User, Pwd, Core, Log)
	     end,
    Dumps = case Result of 
		{ok, _} ->
		    {ok, {Log, LogName}};
		Other ->
		    ct:log(lightred,"Fetching coredump ~p to ~p failed, Reason ~p:~p ~p", [Core, LogName, ?MODULE, fetch_cores, Other]),
		    Other
	    end,
    fetch_cores(Host, T, TCLogPath, R ++ [Dumps]);
fetch_cores(Host, [Data|T], TCLogPath, R) ->
    fetch_cores(Host, T, TCLogPath, R ++ [Data]).

%%===========================================================
%% Removes corefiles
%%===========================================================
remove_cores(_Host, []) ->
    ok;
remove_cores(Host, [{ok, {Core, _LogName}}|T]) ->
    Result = case Host of
		 "localhost" ->	     
		     file:delete(Core);
		 {IP, Port, User, Pwd} ->
		     RM = ssh_cmd(IP, Port, User, Pwd, "rm " ++ Core, 5000),
		     ssh_cmd(IP, Port, User, Pwd, "sync", 10000), % Needed if next testcase is powercycle
		     RM
	     end,
    case Result of 
 	ok ->
	    ok;
	{ok, _Reply} ->
	    ok;
	Other ->
	    ct:log(lightred,"Deleting coredump ~p failed, Reason ~p:~p ~p", [Core, ?MODULE, remove_cores, Other])
    end,
    remove_cores(Host, T).
    
%%===========================================================
%% Determine login method for node
%% {ok,{"10.86.148.133",22,"root","root"}} | {ok,"localhost"} | {fail, Reason}
%%===========================================================
login_data(N,Name) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" -> 
            {ok, "localhost"};
	TargetOrCloud when TargetOrCloud == "target";
			   TargetOrCloud == "cloudish" ->
	    case rct_multi_node_cfg:get_config(N,ssh_lmt_ipv4) of
		undefined ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[ssh_lmt_ipv4,Name,N]});
		[{ssh,IP},{port,Port},{user,User},{password,Pwd}] ->
		    {ok,{IP, Port, User, Pwd}};			    
		_LoginData ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[{ssh_lmt_ipv4,[ssh,port,user,password]},Name,N]})
            end
    end.

%%===========================================================
%% Returns the paths in which to look for corefiles 
%% {ok,[{"/local/scratch/etxkols/RCS_ROOT/home/$USER/core","_cs"},
%%      {"/local/scratch/etxkols/RCS_ROOT/rcs/comte/core","_com"}]}
%% {ok,[{"/dumps/core*",[]}]}
%% {fail, Reason}
%%===========================================================
make_paths(N, Name) ->
    case os:getenv("SIM_OR_TARGET") of
	TargetOrCloud when TargetOrCloud == "target";
			   TargetOrCloud == "cloudish" ->
	    {ok, ?COREDUMP_PARAMS_TARGET, dummy}; %always same paths on target.
	"sim" ->                                 %paths differs on sim.
	    {ok, HostName} = inet:gethostname(),          
	    Username = os:getenv("USER"),
	    ErlNode = list_to_atom(Username ++ "@" ++ HostName),
	    case rpc:call(ErlNode, file, get_cwd, []) of
		{ok,CWD} ->
		    BASE = filename:dirname(filename:dirname(CWD)),
		    {ok,[{T,filename:join(BASE,P),A} || {T,P,A} <- ?COREDUMP_PARAMS_SIM],Username};
%		    {ok,[{T,filename:join(BASE,P),A} || {T,P,A} <- ?COREDUMP_PARAMS_SIM],hd(string:tokens(atom_to_list(ErlNode),"@"))};
                Other ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not get cwd for ~p (~p), Reason: ~p in node ~p",[Name, ErlNode,Other,N]})
            end
    end.

%%===========================================================
%% Returns a strings to be used as headers in html table
%%===========================================================
make_header_name("sim", _N, Sname) ->
    "core(" ++ Sname ++ ")";
make_header_name(TargetOrCloud, N, _Sname) when TargetOrCloud == "target";
						TargetOrCloud == "cloudish" ->
    Board = atom_to_list(ct:get_config({test_nodes,N})),
    "du" ++ integer_to_list(N) ++ "(" ++ Board ++ ")".
    
%%===========================================================
%% Executes ssh command
%% {ok, Data} | {timeout, Data} | {error, Reason}
%%===========================================================
ssh_cmd(IP, Port, User, Pwd, Cmd, CommandTimeout) ->
    ssh_cmd(IP, Port, User, Pwd, Cmd, 3, 1, CommandTimeout). % Default retries and delay
ssh_cmd(IP, Port, User, Pwd, Cmd, Retries, RetryDelay, CommandTimeout) ->
    {ConnectTimeout, _, _} = get(ssh_timeout_opts),
    Pid = spawn(?MODULE, ssh_connect, [self(), IP, Port, User, Pwd, {cmd, Cmd, ""}, ConnectTimeout, Retries, RetryDelay, CommandTimeout]),
    receive {Pid, Result} -> Result end.

%%===========================================================
%% Copies core dump from target to VDI
%% {ok, Data} | {timeout, Data} | {error, Reason}
%%===========================================================
ssh_copy(IP, Port, User, Pwd, From, To) ->
    {ConnectTimeout, _, _} = get(ssh_timeout_opts),
    case file:open(To, [write]) of
	{ok, Handle} ->
	    Pid = spawn(?MODULE, ssh_connect, [self(), IP, Port, User, Pwd, {copy, "cat " ++ From, Handle}, ConnectTimeout, 3, 1, 10000]),
	    receive 
		{Pid, Result} -> 
		    file:close(Handle),
		    Result 
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.
	    
%% @hidden
%%===========================================================
%% Executes ssh action
%% {pid(), {ok, Data}} | {{pid(), timeout, Data}} | {pid(), {error, Reason}}
%%===========================================================

ssh_connect(From, IP, Port, User, Pwd, {Action, Cmd, Handle}, ConnectTimeout, Retries, RetryDelay, CommandTimeout) -> 
    case do_ssh_connect(IP, Port, User, Pwd, {Action, Cmd, Handle}, ConnectTimeout, CommandTimeout) of
	{ok,Reply} ->
	    From ! {self(), {ok,Reply}};
	Other ->	    
	    case Retries > 0 of
		true ->
		    ct:log(yellow,"ssh to ~p failed while seaching for coredumps, Retrying ~p time(s), Reason ~p:~p ~p",[IP, Retries, ?MODULE, ssh_connect, Other]),
		    timer:sleep(RetryDelay * 1000),
		    ssh_connect(From, IP, Port, User, Pwd, {Action, Cmd, Handle}, ConnectTimeout, Retries - 1, RetryDelay, CommandTimeout);
		false ->
		    From ! {self(), Other}
	    end
    end.

do_ssh_connect(IP, Port, User, Pwd, {Action, Cmd, Handle}, ConnectTimeout, CommandTimeout) -> 
    case ssh:connect(IP,Port,[{user,User},
			      {password,Pwd},
			      {silently_accept_hosts, true}, 
			      {user_interaction,false},
			      {quiet_mode, true},
			      {connect_timeout, ConnectTimeout*1000}], ConnectTimeout*1000) of
	{ok, SSH} ->
	    Reply = case ssh_connection:session_channel(SSH, 5000) of
			{ok, Chn} ->
			    Exec = case ssh_connection:exec(SSH, Chn, Cmd, 2000) of
				       success ->
					   do_recv_response(SSH, Chn, Action, Handle, [], CommandTimeout);
				       failure ->
					   {error, {ssh_connection, exec}};
			               % Undocumented Reply
				       {error, Reason} ->
					   {error, {ssh_connection, exec, Reason}}
				   end,
			    ssh_connection:close(SSH, Chn),
			    Exec;
			{error, Reason} ->
			    {error, {ssh_connection, session_channel, Reason}};
			% OTP ssh bug??????????
			{closed,Ch} ->
			    ct:pal("############# CHANNEL_CLOSED ~p",[Ch]),
			    {error,{ssh_connection, session_channel, closed,Ch}}
		    end,
	    ssh:close(SSH),
	    Reply;
	{error, Reason} ->
	    {error, {ssh_connect, Reason}}
    end.

%%===========================================================
%% ssh receive loop
%%===========================================================
do_recv_response(SSH, Chn, Action, Handle, Data, Timeout) ->
    receive
	{ssh_cm, SSH, {open,Chn,RemoteChn,{session}}} ->
	    debug("RECVD open"),
	    {ok,{open,Chn,RemoteChn,{session}}};

	{ssh_cm, SSH, {closed,Chn}} ->
	    ssh_connection:close(SSH, Chn),
	    debug("CLSD~n~p ~p", [SSH,Chn]),
	    {ok,Data};

	{ssh_cm, SSH, {data,Chn,_,NewData}} ->
	    ssh_connection:adjust_window(SSH, Chn, size(NewData)),
	    debug("RECVD~n~p", [NewData]),
	    case Action of
		copy ->
		    file:write(Handle, NewData),
		    do_recv_response(SSH, Chn, Action, Handle, [], Timeout);
		cmd ->
		    DataAcc = Data ++ binary_to_list(NewData),
		    do_recv_response(SSH, Chn, Action, Handle, DataAcc, Timeout)
	    end;

	{ssh_cm, SSH, {eof,Chn}} ->
	    debug("RECVD EOF~n~p ~p", [SSH,Chn]),
	    {ok,Data};

	{ssh_cm, SSH, {exit_signal,Chn,Signal,Err,_Lang}} ->
	    debug("RECVD exit_signal~n~p ~p~n~p ~p", [SSH,Chn,Signal,Err]),
	    do_recv_response(SSH, Chn, Action, Handle, Data, Timeout);

	{ssh_cm, SSH, {exit_status,Chn,Status}} ->
	    debug("RECVD exit_status~n~p ~p~n~p", [SSH,Chn,Status]),
	    do_recv_response(SSH, Chn, Action, Handle, Data, Timeout);

	Other ->
	    debug("UNEXPECTED MESSAGE~n~p ~p~n~p", [SSH,Chn,Other]),
	    do_recv_response(SSH, Chn, Action, Handle, Data, Timeout)

    after Timeout ->
	    ct:log(lightred,"Timeout in rct_core receive ssh cmd: ~p sec",[Timeout]),
	    {timeout,{do_recv_response, Data}}
    end.

debug(Str) ->
    debug(Str, []).

debug(_Str, _Args) ->
%%    io:format("~n--- ct_ssh debug ---~n" ++ _Str ++ "~n", _Args),
    ok.

%%===========================================================
%% Prints html corefile table if core file exist
%% print_result([{"core(dus004)",
%% 	       [{ok,{"/proj/rcs-tmp/stps/dus004/logs/ct_run.dus004@esekilxxen465.2013-02-14_12.57.12/experiment.core_test.core_SUITE.logs/run.2013-02-14_12.57.20/log_private/mytest_core_core-du1-test_app-1360842918-6-2678-1000-100",
%% 		     "mytest_core_core-du1-test_app-1360842918-6-2678-1000-100"}},
%% 		{ok,{"/proj/rcs-tmp/stps/dus004/logs/ct_run.dus004@esekilxxen465.2013-02-14_12.57.12/experiment.core_test.core_SUITE.logs/run.2013-02-14_12.57.20/log_private/mytest_core_core-du1-test_oi.sh-1360842928-6-2679-1000-100",
%% 		     "mytest_core_core-du1-test_oi.sh-1360842928-6-2679-1000-100"}}]}]) -> {fail,{core_dumps}}
%% print_result([{"core(dus004)",ok}]) -> ok
%%===========================================================
print_result(Result) ->
    case get(coredump_test) of
	undefined ->
	    case [X || {X,R} <- Result, R =/= ok] of
		[] -> ok; % No cores or errors
		_ -> print_result2(Result, false)
	    end;
	true ->
	    print_result2(Result, true)
    end.

print_result2(Result, CoredumpTest) ->
    format_html("<table border=\"1\">~n"
	      "<tr>~n"
	      "<th colspan=" ++ integer_to_list(length(Result)) ++ ">Core dumps</th>~n"
	      "</tr>~n"
	      "<tr>"),
    [format_html("<td>~s</td>",[Node])||{Node,_}<-Result],
    format_html("<tr>"),
    Reply = case CoredumpTest of
		false -> print_loop(Result,ok);
		true -> print_test_loop(Result,ok)
	    end,
    format_html("</tr>"),
    format_html("</table>"),
    erase(coredump_test),
    Reply.

print_loop([],R) ->
    R;
print_loop([{_Node,ok}|T],R) ->
    format_html("<td>~n</td>"),
    print_loop(T,R);
print_loop([{_Node,Results}|T],R) ->
    format_html("<td>"),
    R2 = print_loop2(Results,R),
    format_html("</td>"),
    print_loop(T,R2).

print_loop2([],R) ->
    R;
print_loop2([{ok,{Path, Name}}|T],R) ->
    case ct:get_config(allowed_coredumps) of
	undefined ->
	    format_html("<a href=\"~s\"><font color=red>~s</font></a>", [Path, Name]),
	    print_loop2(T,{fail, {core_dumps}});
	L when is_list(L) ->	    
	    case length([[]||{match,_}<-[re:run(Name,X)||X<-L]]) of
		0 ->
		    format_html("<a href=\"~s\"><font color=red>~s</font></a>", [Path, Name]),
		    print_loop2(T,{fail, {core_dumps}});
		_ ->
		    format_html("<a href=\"~s\"><font color=green>~s</font>~n<font color=red>FILTER = ~p</font></a>", [Path, Name, L]),
		    print_loop2(T,R)
	    end
    end;	
print_loop2([{no_fail_on_coredump,Name}|T],R) ->
    format_html("~s", [Name]),
    print_loop2(T,R);
print_loop2([Error|T],_R) ->
    format_html("<font color=red>~p</font>", [Error]),
    print_loop2(T,{fail, {core_dumps}}). 

%%===========================================================
%% print_test_loop is only used for testing that coredumps
%% can be generated.
%%===========================================================
print_test_loop([],R) ->
    R;
print_test_loop([{_Node,ok}|T],_R) ->
    format_html("<td><font color=red>NO COREDUMP GENERATED</font></td>"),
    print_test_loop(T,{fail, {coredump_test}});
print_test_loop([{_Node,Results}|T],R) ->
    format_html("<td>"),
    Res = print_test_loop2(Results,R),
    format_html("</td>"),
    print_test_loop(T, Res).

print_test_loop2([], R) ->
    R;
print_test_loop2([{ok,{Path, Name}}|T], R) ->
    format_html("~s", [Name]),
    file:delete(Path),
    print_test_loop2(T, R);
print_test_loop2([Error|T], _R) ->
    format_html("<font color=red>~p</font>", [Error]),
    print_test_loop2(T,{fail, {coredump_test}}). 

format_html(String) ->
    ct:log(default, 1, String, [], [no_css]).

format_html(String,Args) ->
    ct:log(default, 1, String, Args, [no_css]).

make_name_module(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(?MODULE)).

fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.

%% @spec update_config(Name,ConfigLine) -> ok | no_configuration_found | not_affected_by_update
%% @doc callback function for updating configuration data.
%% ```Name = atom()                          Alias for cli or coli towards node.
%%    ConfigLine = tuple                     {ssh_lmt_ipv4, [{ssh, SSH}, {port, Port}, {user, User}, {password, Password}]} '''
update_config(Name, {ssh_lmt_ipv4, [{ssh, SSH}, {port, Port}, {user, User}, {password, Pwd}]}) ->
    Name2=make_name_module(Name),
    [_LoginData,PathConfigs] = ct:get_config(Name2),
    NewConfig=[{SSH,Port,User,Pwd},PathConfigs],
    rct_multi_node_cfg:remove_config(Name2),
    ok = rct_multi_node_cfg:require(Name2, NewConfig);
update_config(_Name,_Config) ->
    not_affected_by_update.


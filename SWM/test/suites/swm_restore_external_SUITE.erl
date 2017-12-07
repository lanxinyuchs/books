%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_restore_external_SUITE.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R8A/1
%%%
%%% @doc == Test Suite for restoring an external backup with CS lab settings intact==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(swm_restore_external_SUITE).
-vsn('/main/R3A/R4A/R5A/R6A/R8A/1').
-author('etxberb').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R3A/1      2015-04-28 etxjotj     Created
%%% R4A/1      2015-05-07 etxarnu     Corrected get_progress_report_member
%%% R4A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R5A/1      2016-04-06 ekurnik     Updated for OAM over IPv6 and added
%%%                                   actionCapable checking
%%% R6A/1      2016-08-22 etxkols     Git migration requires that CC paths is not used 
%%% R8A/1      2017-01-10 etxberb     Replaced check_action_capable("CAPABLE")
%%%                                   with wait_for_action_capable().
%%%--------------------------------------------------------------------


%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
         groups/0]).

-export([restore_external/1]).



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [restore_external].

-define(DataDir, rct_cc_git_path:find("RCS_TOP", ["SWM/SWM_CNX9012637/test/suites", "SWM/test/suites"])).


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" -> 
	    [{timetrap, {minutes, 20}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_netconf,{nc1, html}},
			 {cth_conn_log,[]},
			 {rct_core,[]},
			 {rct_coli, {coli, [manual_connect]}},
			 {rct_logging,
			  {all, [{[erlang,swmLog],
			  {["ERROR REPORT","CRASH REPORT"],[]}}]}}
			]}];
	_Other ->
	     [{timetrap, {minutes, 20}},
	     {ct_hooks, 
	      case os:getenv("USER") of
		  "etxjotj" ->[];
		  _ -> 	[{rct_rs232, console}]
	      end++
		  [{rct_htmllink,[]},
		   {rct_rpc, rpc_1},
		   {rct_netconf,{nc1, html}},
		   {cth_conn_log,[]},
		   {rct_core,[]},
		   {rct_coli, {coli, [manual_connect]}},
		   {rct_logging,
		    {all, [{[erlang,swmLog],
			    {["ERROR REPORT","CRASH REPORT"],[]}}]}}
		  ]}]
    end.

%% @hidden
init_per_suite(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),
    MeData = call(comsaI, get_managed_element_data, []),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    Name = "swm_backup_SUITE",
    [{host, Host},{username, Username},{password, Password}] = 
	ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    NewConfig = [{meId, MeId},
		 {backupName, Name},
		 {sftp_host, Host},
		 {sftp_user, Username},
		 {sftp_pass, Password},
		 {sftp_root, RootDir} | Config],
    ct:print("Init per suite complete~n"),
    NewConfig.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    rpc_call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
    rpc_call(swmServer, start_test_event_server, []),
    ct:print("Now executing ~w~n",[TestCase]),
    Config.
%% @hidden
end_per_testcase(TestCase, _Config) ->
    rpc_call(swmServer, stop_test_event_server, []),
    rpc_call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    ct:print("Test case ~w complete.~n",[TestCase]),
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []},  
     {sbc__upgrade__all__1__group, [], []},  
     {sdc__cover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},  
     {sdc__qual__all__1__group, [], []}  
   ].


restore_external(Config) ->
    FileName = 
	case get_ct_arg(backup) of
	    undefined -> 
		ct:fail(no_backup_flag);
	    Backup ->
		Source = normalize_path(Backup),
		case filelib:is_file(Source) of
		    false ->
			ct:fail({no_such_file, Source});
		    true ->
			ok
		end,
		ct:print("Moving file to sftp server area"),
		Dir = proplists:get_value(sftp_root, Config),
		Dest = filename:join(Dir, filename:basename(Source)),
		{ok,Bytes} = file:copy(Source, Dest),
		ct:print("File size is ~w bytes",[Bytes]),
		filename:basename(Backup)
	end,

    ct:pal("Saving LDAP config"),
    Ldap = rpc_call(ets, tab2list, [ldap]),
    Role = rpc_call(ets, tab2list, [role]),
    Rule = rpc_call(ets, tab2list, [rule]),

    MoRef = import_backup([{filename, FileName}|Config]),     
    restore_backup([{moData, Ldap++Role++Rule},{moRef, MoRef}|Config]),

    ok.
    

    

%%--------------------------------------------------------------------
%% Import Backup
%% Using netconf to cause the system to import a backup
%% Initiate the action and wait for it to be complete
%% Create and export a backup if none is available


import_backup(Config) ->
    ImportName = proplists:get_value(filename, Config),

    MeId = proplists:get_value(meId, Config),
    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{progressReport, []}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    Uri = sftp_uri(Config)++"/"++ImportName,
    Password = proplists:get_value(sftp_pass, Config),
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'importBackup',[],
		      [{uri, [], [Uri]},
		       {password, [], [Password]}]}]}]}]}]},
    ct:pal("Executing action importBackup"),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, _} = netconf(action, [nc1, Action]),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
        ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("importBackup")},
                                                       {"CAPABLE", ""}]),
        ok = swmTestLib:wait_for_action_capable(),
	    MoRef = get_result_info(ProgressFilter),
	    ct:pal("Backup ~p succesfully imported.",[MoRef]),
	    MoRef;
	Result ->
	    ct:pal("importBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.




%%--------------------------------------------------------------------
%% @doc NodeUC442.N Restore backup
%% This is not the complete restore backup, which will come later.
%% But as a precondition we want to make sure the boot partition number can
%% be read
%% @end

restore_backup(Config) ->

    MeId = proplists:get_value(meId, Config),
    MoRef = proplists:get_value(moRef, Config),
    BuId = lists:last(string:tokens(MoRef, "=")),

    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{'BrmBackup',
		 [{brmBackupId, [], [BuId]},
		  {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'BrmBackup',
		      [{brmBackupId, [], [BuId]},
		       {restore, []}]}]}]}]}]},
    ct:print("Executing action restore on backup ~p~n",[BuId]),
    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),
    {ok, ActionResponse} = netconf(action, [nc1, Action]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    rpc_call(swmBackup, force_restore, []),
        ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("restoreBackup")}]),
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,

    wait_for_rpc_drop(1000, 60),
    wait_for_rpc(3000, 90),
    wait_for_tables(3000, 90),


    ct:pal("Restoring LDAP config and authorization rules"),
    MoData = proplists:get_value(moData, Config),
    [rpc_call(mnesia, dirty_write, [X])||X<-MoData],
    
    %% Restart test_event_server after restore
    call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
    call(swmServer, start_test_event_server, []),

    ct:pal("Waiting for progress"),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ct:pal("Restore complete~n",[]),
	    ok = swmTestLib:wait_for_action_capable();
	Result ->
	    ct:pal("restoreBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.

wait_for_rpc_drop(_, N) when N<1 ->
    ct:fail(rpc_never_dropped);
wait_for_rpc_drop(Timer, N) ->
    case rpc_call(erlang, list_to_atom, ["ok"]) of
	ok ->
	    timer:sleep(Timer),
	    wait_for_rpc_drop(Timer, N-1);
	R ->
	    ct:pal("Wait for rpc drop: ~p~n",[R])
    end.

wait_for_rpc(_, N) when N<1 ->
    ct:fail(rpc_never_appeared);
wait_for_rpc(Timer, N) ->
    case rpc_call(erlang, list_to_atom, ["ok"]) of
	ok ->
	    ct:pal("Wait for rpc: alive",[]);
	_ ->
	    timer:sleep(Timer),
	    wait_for_rpc(Timer, N-1)
    end.

wait_for_tables(Timer, N) when N<1 ->
    case rpc_call(mnesia, wait_for_tables, [[ldap, role, rule], Timer]) of
	ok -> ok;
	WFT -> ct:fail({wait_for_tables, WFT})
    end;
wait_for_tables(Timer, N) ->
    case rpc_call(mnesia, wait_for_tables, [[ldap, role, rule], Timer]) of
	ok ->
	    ct:pal("Wait for tables: ready",[]);
	{error, E} ->
	    ct:pal("Wait for tables: ~p~n",[{error, E}]),
	    timer:sleep(3000),
	    wait_for_tables(Timer, N-1);
	{timeout, BadTabList}->
	    ct:pal("Wait for tables: ~p~n",[{timeout, BadTabList}]),
	    wait_for_tables(Timer, N-1)
    end.


%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           normalize_path(Path)
%%% Input: Path:string()
%%% Output: string()
%%% Exceptions:
%%% Description: Expand a short or relative path to an absolute path
%%% ----------------------------------------------------------

normalize_path([$/|_]=Path) -> 
    Path;
normalize_path([$~,$/|Path]) ->
    Home = os:getenv("HOME"),
    filename:join(Home, Path);
normalize_path([$~|Path]) ->
    Home = os:getenv("HOME"),
    filename:join([filename:dirname(Home)|filename:split(Path)]);
normalize_path(Path) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, Path).

%%%--------------------------------------------------------------------
%%% Description: Make an rpc call
%%%--------------------------------------------------------------------

rpc_call(M,F,A) ->
    rct_rpc:call(rpc_1, M, F, A, 10000).


%%%--------------------------------------------------------------------
%%% Description: Read parameter
%%%--------------------------------------------------------------------

get_ct_arg(Arg) ->
    case init:get_argument(Arg) of
	{ok, [[Reply]]} ->
	    Reply;
	error -> undefined
    end.


%%%--------------------------------------------------------------------
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(ProgressFilter) ->
    get_progress_report_member(actionId, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(ProgressFilter) ->
    get_progress_report_member(resultInfo, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, ProgressFilter) ->
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
    ct:pal("ProgressFilter result: ~p~n", [A]),
    case extract_element(progressReport, A) of
	{ok, {progressReport, L, _}}  ->
	    case lists:keyfind(unset,1, L) of
		{unset, "true"} ->
		    undefined;
		_ ->
		    extract_member(Member, A)
	    end;
	_ ->
	    extract_member(Member, A)
    end.
extract_member(Member, A) ->
    {ok, {Member, _, [Value]}} = 
	extract_element(Member, A),
    Value.
    

%%%--------------------------------------------------------------------
%%% Description: Convert an backup action error code to a meaningful value
%%%--------------------------------------------------------------------

decode_return_value("0") -> actionStarted;
decode_return_value("1") -> nameValidationFailure;
decode_return_value("2") -> duplicateName;
decode_return_value("3") -> housekeepingRequired;
decode_return_value("4") -> backupNotFound;
decode_return_value("98") -> missingParameter;
decode_return_value("99") -> softwareFault.

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	{ok, R} ->
	    ok = ct_netconfc:close_session(nc1),
	    {ok, R};
	{error, Error} ->
	    {error, Error}
    end.

%%%--------------------------------------------------------------------
%%% Description: Construct sftp
%%%--------------------------------------------------------------------

sftp_uri(Config) ->
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    Dir = proplists:get_value(sftp_root, Config),
    "sftp://"++User++"@"++Host++Dir.

%%%--------------------------------------------------------------------
%%% Description: Make a standardized rpc call
%%%--------------------------------------------------------------------

call(M, F, A) ->
    rct_rpc:call(rpc_1, M, F, A, 10000).

%%%--------------------------------------------------------------------
%%% Description: This function and the associated check_progress_elements
%%%              loops over a netconf get for the progress information until
%%%              the state is FINISHED, otherwise it prints the status and
%%%              progress percentage
%%%--------------------------------------------------------------------


wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(1000),
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    ct:pal("Waiting for updated progress ~s~n~p~n",
				   [OldActionId, Report]),
			    wait_for_progress(Attribute, OldActionId,
					      ProgressFilter);
			_ ->
			    case check_progress_elements(Attribute, Report) of
				loop ->
				    wait_for_progress(Attribute, OldActionId,
						      ProgressFilter);
				{ok, Result} ->
				    Result
			    end
		    end;
		{error, Error} ->
		    ct:pal("Netconf get error:~n~p",[Error]),
		    wait_for_progress(Attribute, OldActionId, ProgressFilter)
	    end;
	{error, Reason} ->
	    ct:log("Netconf open error:~n~p",[Reason]),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    case Report of
	{progressReport, [{unset, "true"}], []} ->
	    loop;
	_ ->
	    {ok, State} = extract_element(state, [Report]),
	    case State of
		{state, _, ["FINISHED"]} ->
		    format_progress_report(Report),
		    ct:pal("~s",[format_progress_report(Report)]),
		    {ok, {result, _, Result}} =
			extract_element(result, [Report]),
		    {ok, Result};
		{state, _, ["CANCELLED"]} ->
		    format_progress_report(Report),
		    ct:pal("~s",[format_progress_report(Report)]),
		    {ok, {result, _, Result}} =
			extract_element(result, [Report]),
		    {ok, Result};
		{state, _, [Current]} ->
		    {ok, {progressPercentage, _, [Percent]}} =
			extract_element(progressPercentage, [Report]),
		    {ok, {progressInfo, _, [Info]}} =
			extract_element(progressInfo, [Report]),
		    {ok, {actionName, _, [ActionName]}} =
			extract_element(actionName, [Report]),
		    ct:pal("Action: ~s~nState: ~s ~s % ready~n~s",
			   [ActionName, Current, Percent, Info]),
		    loop
	    end
    end.



%%%--------------------------------------------------------------------
%%% Description: Format a AsyncActionStruct for printing
%%%--------------------------------------------------------------------


format_progress_report({progressReport, _, Members}) ->
    [io_lib:format("progressReport:~n",[])|format_progress_report(Members)];
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) ->
    ct:pal("Unknown format: ~p~n",[X]),
    ct:fail(unknown_progress_report_format).



%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.


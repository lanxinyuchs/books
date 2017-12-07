%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_err_br_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R5A/R6A/R7A/R8A/R11A/1
%%% 
%%% @doc == Test Suite for testing backup restore mechanism.==
%%% <br/><br/>
%%% @end

-module(swm_err_br_SUITE).
-vsn('/main/R5A/R6A/R7A/R8A/R11A/1').
-author('etxivri').

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
%%% R5A/2      2015-11-23 etxivri     Created due to HU37699
%%% R5A/5      2015-12-04 etxivri     Update restart check to be more robust.
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-09-12 etxivri     Make it more robust.
%%% R8A/1      2016-12-28 etxivri     Make it more robust.
%%% R11A/1     2017-10-13 etxivri     Update to kill apc instead for lratapp.
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0]).
-export([create_backup/1,
	 delete_backup/1,
	 restore_backup/1,
	 test_1/1,
	 test_2/1,
	 test_3/1
	]).

-define(NC_sess, nc1).
-define(Data_Dir, "/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/test/suites/SWM/swm_backup_SUITE_data/").
-define(BU_Name_1, "bu_test_1").

-define(CLI_Sess, cli1).

-define(LRAT_APP, "lratMonitorArmLm").

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
    ].

groups() ->
    [{group_1,[],[create_backup,
		  test_1,
		  test_2,
		  test_3
		  %% delete_backup
		 ]},
    {group_2,[],[
    		]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {minutes, 60}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_rs232,console},
		 {rct_netconf,nc1},
		 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging, 
		  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
				  ["Name : \"apc\""
				  %% "Program ID [0-9]+ has terminated"
				  ]}}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_upgrade,ug1}
		]}].

%% @hidden
init_per_suite(Config) ->
    MeId = swm_test_lib:get_me_id(?NC_sess),
    [{meId, MeId} | Config].
%% @hidden
end_per_suite(_Config) ->
    rct_rpc:call(rpc_1, appmServer, reset_restart_list, [], 10000),
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    rct_rpc:call(rpc_1, appmServer, reset_restart_list, [], 10000),
    Config.
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
	    %% delete_backup(Config)
    end,
    ok.

%%% ===========================================================================
%%  test_1. program crash during restore
%% 1.	Create backup.
%% 2.	restore backup
%% 3.	trig program crash.
%% 4.	check bu is restored
%% 5.   check llog
%% Test Finished
%%% ===========================================================================
test_1(Config) ->
    %% {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    MeId = proplists:get_value(meId, Config),
    %% %% create_backup(Config, ?BU_Name_1, MeId),
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),
    BuId = get_correct_buid(?BU_Name_1, Backups),

    clear_llog(),
    check_warm_not_exist_in_llog(),

    ExpProgressInfo = "Preparing restore database",
	%% "Last chance to cancel. Restore will commence in 30 seconds",
    swm_br_lib:restore_backup(?NC_sess, MeId, BuId),
    ct:pal("# restore bu has started."),
    wait_for_exp_proginfo_in_bu(Config, MeId, BuId, ExpProgressInfo),
    ct:pal("# expected progressinfo rcvd. time to kill appl."),

    ActionId = no_check,

    kill_lrat_appl(),
    ct:pal("# Kill appl is done. Wait for restore done."),

    %% net_kernel:disconnect(ErlNode),
    wait_for_restore_success(Config, MeId, BuId, ActionId),

    %% check_warm_exist_in_llog(),
    print_llog(),
    check_warm_not_exist_in_llog(),
    check_cold_exist_in_llog(),
    ok.

%%% ===========================================================================
%%  test_2. Cancel during restore. Trig an appl crash shall result in 
%%          warm restart. 
%% 1.	Create backup.
%% 2.	restore backup
%% 3.   Cancel the restore
%% 4.	trig program crash.
%% 5.   check llog
%% Test Finished
%%% ===========================================================================
test_2(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% %% create_backup(Config, ?BU_Name_1, MeId),
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),
    BuId = get_correct_buid(?BU_Name_1, Backups),

    clear_llog(),
    check_warm_not_exist_in_llog(),

    ExpProgressInfo = "Preparing restore database",
	%% "Last chance to cancel. Restore will commence in 30 seconds",
    swm_br_lib:restore_backup(?NC_sess, MeId, BuId),
    ct:pal("# restore bu has started."),
    wait_for_exp_proginfo_in_bu(Config, MeId, BuId, ExpProgressInfo),
    ct:pal("# expected progressinfo rcvd. time to cancel restore on bu."),

    perform_cancel_on_bu(MeId, BuId),
    timer:sleep(10000),

    ActionId = no_check,
    wait_for_restore_canceled(Config, MeId, BuId, ActionId),
    timer:sleep(10000),
    
    kill_lrat_appl(),
    ct:pal("# Kill appl is done. Wait for restore done."),

    timer:sleep(30000),
    print_llog(),
    check_warm_exist_in_llog(),
    check_cold_not_exist_in_llog(),
    ok.

%%% ===========================================================================
%%  test_3. Trig an appl crash after restore, but before reboot,
%%          perform cancel before the reboot.
%%          warm restart. 
%% 1.	Create backup.
%% 2.	restore backup
%% 3.	trig program crash.
%% 4.   Cancel the restore
%% 5.   Check crash is handle. Will result in programcrash Cold.
%% 6.   check llog
%% Test Finished
%%% ===========================================================================
test_3(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% %% create_backup(Config, ?BU_Name_1, MeId),
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),
    BuId = get_correct_buid(?BU_Name_1, Backups),

    clear_llog(),
    check_warm_not_exist_in_llog(),

    ExpProgressInfo = "Preparing restore database",
	%% "Last chance to cancel. Restore will commence in 30 seconds",
    swm_br_lib:restore_backup(?NC_sess, MeId, BuId),
    ct:pal("# restore bu has started."),

    kill_lrat_appl(),
    ct:pal("# Kill appl is done."),

    wait_for_exp_proginfo_in_bu(Config, MeId, BuId, ExpProgressInfo),
    ct:pal("# expected progressinfo rcvd. time to cancel restore on bu."),

    perform_cancel_on_bu(MeId, BuId),
    timer:sleep(10000),

    ActionId = no_check,
    wait_for_restore_canceled(Config, MeId, BuId, ActionId),    

    timer:sleep(30000),

    [{"FAILURE", 
      "RESTORE", 
      _ProgressInfo, 
      "The action was interrupted by a restart", 
      "FINISHED",
      _ProgressReport}] = 
	 swm_br_lib:
	 wait_for_expecting_state(?NC_sess, 
				  MeId, 
				  ActionId, 
				  "FINISHED", 
				  {brmBackup, BuId}),

    print_llog(),
    check_warm_not_exist_in_llog(),
    check_cold_exist_in_llog(),
    ok.




%%% ===========================================================================
%%% @doc
%%% Create backup <br/>
%%% Using netconf to trig a backup to be created. <br/>
%%% @spec create_backup(Config) -> ok
%%% @end
%%% ===========================================================================
create_backup(Config) ->  
    MeId = proplists:get_value(meId, Config),
    create_backup(Config, ?BU_Name_1, MeId).


create_backup(_Config, BU_Name, MeId) ->  
    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId),
    ct:pal("# A"),
    swm_br_lib:create_backup(?NC_sess, 
			     MeId, 
			     BU_Name),

    [{"SUCCESS", 
      _ActionName, 
      _ProgressInfo, 
      ResultInfo, 
      _State, 
      _ProgressReport}] = swm_br_lib:wait_for_expecting_state(?NC_sess, 
							      MeId, 
							      ActionId, 
							      "FINISHED"),
    ct:pal("# ResultInfo: ~p", [ResultInfo] ),

    ok.


%%% ===========================================================================
wait_for_restore_success(_Config, MeId, BuId, ActionId) ->
    ct_telnet:expect(console, "Ericsson",
		     [{timeout,60000}, no_prompt_check]),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, 
				 MeId, 
				 ActionId, 
				 "FINISHED", 
				 {brmBackup, BuId}) of
	[{"SUCCESS", 
	  "RESTORE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  ProgressReport}] ->
	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("restoreBackup: ~p~n",[ErrResult]),
    	    ct:fail(ErrResult)
    end.


%%% ===========================================================================
wait_for_restore_canceled(_Config, MeId, BuId, ActionId) ->
    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, 
				 MeId, 
				 ActionId, 
				 "CANCELLED", 
				 {brmBackup, BuId}) of
	[{"FAILURE", 
	  "RESTORE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  "CANCELLED", 
	  ProgressReport}] ->
	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("restoreBackup: ~p~n",[ErrResult]),
    	    ct:fail(ErrResult)
    end.

%%% ===========================================================================

wait_for_exp_proginfo_in_bu(_Config, MeId, BuId, ExpProgrInfo) ->
    ct:log("#### wait for progressInfo: ~p", [ExpProgrInfo]),
    wait_for_exp_proginfo_in_bu(_Config, MeId, BuId, ExpProgrInfo, 300000).

wait_for_exp_proginfo_in_bu(_Config, _MeId, _BuId, ExpProgrInfo, Timeout) when 
      Timeout < 0 ->
    ct:log("### ExpProgrInfo: ~p", [ExpProgrInfo]),
    ct:fail("Expected progressinfo not rcvd withn exp timeout.");

wait_for_exp_proginfo_in_bu(_Config, MeId, BuId, ExpProgrInfo, Timeout) ->
    {progressReport, _, ReportList} = 
	swm_br_lib:get_progress_report(?NC_sess, MeId, {brmBackup, BuId}),
    ct:log("Bu ReportList: ~p", [ReportList]),

    case lists:keyfind(progressInfo,1, ReportList) of
	{progressInfo,[],[ExpProgrInfo]} ->
	    ct:log("### Found ExpProgrInfo: ~p", [ExpProgrInfo]),
	    ok;
	_Other ->
	    timer:sleep(5000),
	    wait_for_exp_proginfo_in_bu(_Config, MeId, BuId, ExpProgrInfo, Timeout- 5000)
    end.


%%% ===========================================================================
%%% @doc
%%% Restore backup <br/>
%%% Using netconf to trig a restore backup . <br/>
%%% @spec restore_backup(_Config) -> ok
%%% @end
%%% ===========================================================================
restore_backup(_Config) ->
    restore_backup(_Config, ?BU_Name_1).

restore_backup(Config, BU_Name) ->
    MeId = proplists:get_value(meId, Config),

    %%%%
    %% Get BuId that belongs to BU_name.
    %%%%
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),

    BuId = get_correct_buid(BU_Name, Backups), %% in a list.
    ct:pal("Selected backup ~p to restore.~n",[BuId]),

    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId, {brmBackup, BuId}),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    swm_br_lib:restore_backup(?NC_sess, MeId, BuId),

    ct_telnet:expect(console, "Ericsson",
		     [{timeout,60000}, no_prompt_check]),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, 
				 MeId, 
				 ActionId, 
				 "FINISHED", 
				 {brmBackup, BuId}) of
	[{"SUCCESS", 
	  "RESTORE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  ProgressReport}] ->
	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("restoreBackup: ~p~n",[ErrResult]),
    	    ct:fail(ErrResult)
    end.


%%% ===========================================================================
%%% @doc
%%% Delete backup <br/>
%%% Using netconf to trig a backup to be removed. <br/>
%%% @spec delete_backup(Config) -> ok
%%% @end
%%% ===========================================================================
delete_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    ActionId = no_check,
    swm_br_lib:delete_backup(?NC_sess, 
			     MeId, 
			     ?BU_Name_1),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  "DELETE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup deleted successfully"),
	    ok;
	Result ->
	    ct:pal("deleteBackup: ~p~n",[Result]),
	    ct:fail(Result)
    end.


%%% Internal
%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------
get_correct_buid(BuName, AllBackUps) ->
    List = lists:dropwhile(fun({_BrmBackup, _, Data}) ->
				   case lists:keysearch(backupName, 1, Data) of
				       {value,{backupName,[],[BuName]}} ->
				   	   false;
				       _ ->
				   	   true
				   end
			   end, AllBackUps),
    ct:pal("BuIdList: ~p", [List]),
    [WantedList | _] = List,
    {_BrmBackup, _, WantedData} = WantedList,
    {value,{brmBackupId,[],[BuId]}} = 
	lists:keysearch(brmBackupId, 1, WantedData),
    [BuId].



perform_cancel_on_bu(MeId, BuId) ->
    ok = rct_cli:connect(?CLI_Sess, noprint),
    {ok, _} = rct_cli:send(?CLI_Sess, 
			   "ManagedElement="++MeId++","
			   "SystemFunctions=1,"
			   "BrM=1,"
			   "BrmBackupManager=1,"
			   "BrmBackup="++BuId++","
			   "cancelCurrentAction", print),
    ok = rct_cli:disconnect(?CLI_Sess, noprint).
    

clear_llog() -> 
    ct:pal("### clear llog.",[]),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "llog -c"),
    timer:sleep(5000),
    ct_telnet:cmd(console, "llog"),
    ok.

print_llog() ->
    ct:pal("## Print llog"),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "llog").

check_warm_exist_in_llog() ->
    ok = check_llog("Warm"),
    ct:pal("Restart Rank Warm exist in llog.").

check_warm_not_exist_in_llog() ->
    nok = check_llog("Warm"),
    ct:pal("Restart Rank Warm NOT exist in llog.").

check_cold_exist_in_llog() ->
    ok = check_llog("Cold"),
    ct:pal("Restart Rank Cold exist in llog.").

check_cold_not_exist_in_llog() ->
    nok = check_llog("Cold"),
    ct:pal("Restart Rank Cold NOT exist in llog.").


check_llog(SearchStr) -> 
    Log = rct_rpc:call(rpc_1, os, cmd, ["llog"], 10000),
    ct:log("### SearchStr: ~p", [SearchStr]),
    LogA = string:tokens(Log , " \n-"),
    ct:log("### Log: ~p", [LogA]),

    SearchList = lists:dropwhile(fun(X) ->
					 X =/= SearchStr
				 end, LogA),
    ct:log("SearchList log: ~p", [SearchList]),
    case re:run(SearchList, SearchStr) of
	{match, _} ->
	    ok;
	nomatch ->
	    nok
    end.

    %% case SearchList of
    %% 	[] ->
    %% 	    nok; %% Warm str not exist in log
    %% 	_Else -> %% check expt str exisyt in log
    %% 	    case SearchStr of
    %% 		"Warm" ->
    %% 		    [SearchStr, "ProgramErrEscalated"|_] = SearchList,
    %% 		    ok;
    %% 		"Cold" ->
    %% 		    [SearchStr |_] = SearchList,
    %% 		    ok;
    %% 		_Other ->
    %% 		    ct:fail("## Test fail due to unknown SearchStr.")
    %% 	    end
    %% end.

kill_lrat_appl() -> 
    %% check appl exist before kill
    ApplList = rct_rpc:call(rpc_1, appmServer, get_apps, [], 60000),
    ct:log("### ~p ",[ApplList]),
    %% [{"lratMonitorArmLm", _}] =
    %% 	[{Appl,_X} || {Appl, _X} <- ApplList, Appl == "lratMonitorArmLm"],
    ct:pal("### pkill -f apc.",[]),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "pkill -f apc"),
    ok.

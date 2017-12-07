%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_failsafe_1_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R5A/2
%%% 
%%% @doc == Test Suite verify that Scheduled Backup Failed alarm genenerates and ceased as expected during failsafe is triggered. ==
%%% <br/><br/>
%%% @end

-module(swm_failsafe_1_SUITE).
-vsn('/main/R2A/R3A/R5A/2').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/2      2014-03-31 etxivri     Created
%%% R2A/3      2014-04-02 etxivri     Update due to TR HS46984
%%% R2A/4      2014-04-04 etxivri     Added deactivate in tc.
%%% R2A/6      2014-04-17 etxivri     Update actionName in check.
%%%                                   Update of failsafe check.
%%% R2A/7      2014-08-19 etxivri     Update to make it more robust.
%%% R2A/8      2014-10-01 etxivri     Update to make it more robust.
%%% R3A/2      2014-10-24 etxivri     More update to make it more robust.
%%% R3A/3      2015-02-03 etxivri     Update to avoid error in ct-shell.
%%% R3A/4      2015-02-10 etxivri     Update to avoid error in ct-shell.
%%% R3A/5      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R5A/1      2016-01-11 etxivri     Update due to changed behaviour.
%%% R5A/2      2016-04-11 etxivri     Update due to new progressinfo.
%%%                                   
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([sched_alarm_during_failsafe_trig_1/1,
	 sched_alarm_during_failsafe_trig_2/1
	]).

-define(ScheduleName, "schedule_backup_name_test").
-define(ScheduleName2, "schedule_backup_name_test_2").
-define(AlarmSchedBuFailed, "9175044").
-define(ScheduleId, "1").
-define(FailsafeId, "1").
-define(NC_Session, nc1).
%% -define(CLI_Session, cli1).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {hours, 2}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 %% {rct_upgrade,ug1},
		 {cth_conn_log, []},
		 {rct_rs232,console},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  []
					 }}]}},
		 %% {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),
    [{meId, MeId} | Config].
    %% Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
            %%%%
	    %% Clean up.
            %%%%
	    MeId = proplists:get_value(meId, Config),
	    swm_br_lib:set_failsafe_attribute(?NC_Session, 
					      MeId, 
					      timeoutLength, 
					      "1200"),
	    %%%%
	    %% Deactivate failsafe backup.
            %%%%
	    swm_br_lib:deactivate_failsafe_bu(?NC_Session, 
					      MeId)
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    %% [test_all].
    [sched_alarm_during_failsafe_trig_1,
     sched_alarm_during_failsafe_trig_2
    ].


%%%--------------------------------------------------------------------
%%% @doc 
%%% Configure schedule single backup that will be triggered after 2min. 
%%% Activate failsafe backup. During failsafe timer, the schedule backup 
%%% will result in alarm. 
%%% After alarm is generated, deactivate failsafe backup. 
%%% Config a new single schedule backup.  
%%% When this is triggered, the alarm will ceased. <br/>
%%% @spec sched_alarm_during_failsafe_trig_1(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
sched_alarm_during_failsafe_trig_1(Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = proplists:get_value(meId, Config),

    %%%%
    %% Get action Id
    %%%%
    %% ActionId = swm_br_lib:get_action_id(?NC_Session, MeId, 
    %% 					{brmBackupScheduler, ?ScheduleId}),
    ActionId = no_check,
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    %%%%
    %% Configire single backup that shall start after 2 min.
    %% No check for FINISHED or SUCCES shall be done.
    %% Fix this when TR HS46984 is fixed.
    %%%%
    %% conf_single_schedule_bu(MeId, ?ScheduleName, ActionId),
    AddTime = 120, % sec 
    swm_br_lib:
	conf_single_schedule_bu(?NC_Session,
				MeId, 
				?ScheduleName, 
				AddTime, 
				?ScheduleId),
    timer:sleep(10000),

    %%%%
    %% Check minorType "9175044" (Scheduled Backup Failed) not exist.
    %%%%   
    ct:pal("Check alarm not exist."),
    case swm_test_lib:is_alarm(?NC_Session, MeId, ?AlarmSchedBuFailed) of
	false ->
	    ok;
	true ->
	    ct:fail("Tc will fail, Scheduled Backup Failed alarm. Shall "
		    "not exists")
    end,

    %%%%
    %% Activate failsafe backup
    %%%%
    swm_br_lib:activate_failsafe_bu(?NC_Session, MeId),

    %%%%
    %% Check minorType "9175044" (Scheduled Backup Failed) exist.
    %%%%    
    ct:pal("Check alarm exist."),
    wait_for_alarm_exist(?NC_Session, MeId, ?AlarmSchedBuFailed),

    %%%%
    %% Check progressReport when alarm is generated when failsafe triggered.
    %%%% 
    check_sched_prog_report_failsafe_triggered(?NC_Session, MeId, ?ScheduleId),

    %%%%
    %% Deactivate failsafe backup.
    %%%%
    swm_br_lib:deactivate_failsafe_bu(?NC_Session, MeId),
    %% test_server:break("BBB"),

    %%%%
    %% Cease Scheduled Backup Failed alarm.
    %% Alarm shall be ceased when a new scheduled is performed.
    %%%%
    ActionId2 = swm_br_lib:get_action_id(?NC_Session, MeId, 
					 {brmBackupScheduler, ?ScheduleId}),
    conf_single_schedule_bu(MeId, ?ScheduleName2, ActionId2),

    %%%%
    %% Check minorType "9175044" (Scheduled Backup Failed) not exist.
    %%%%
    wait_for_alarm_ceased(?NC_Session, MeId, ?AlarmSchedBuFailed),

    AllBackups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:pal("AllBackups: ~p",[AllBackups]),

    delete_all_backups(?NC_Session, MeId),
    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Configure schedule single backup that will be triggered after 2min. 
%%% Activate failsafe backup. During failsafe timer, the schedule backup 
%%% will result in alarm. 
%%% Wait to failsafe bu triggered. 
%%% After failsafe backup restored, check alarm is ceased. <br/>
%%% @spec sched_alarm_during_failsafe_trig_2(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
sched_alarm_during_failsafe_trig_2(Config) ->

    %%%%
    %% Get managedElementId
    %%%%
    %% MeId = swm_test_lib:get_me_id(?NC_Session),
    MeId = proplists:get_value(meId, Config),

    %%%%
    %% Set failsafe attribute, timeoutLengt to 240 sec.
    %%%%
    swm_br_lib:set_failsafe_attribute(?NC_Session, MeId, timeoutLength, "240"),

    %%%%
    %% Get action Id
    %%%%
    %% ActionId = swm_br_lib:get_action_id(?NC_Session, MeId, 
    %% 					{brmBackupScheduler, ?ScheduleId}),
    ActionId = no_check,
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    %%%%
    %% Configire single backup that shall start after 2 min.
    %% No check for FINISHED or SUCCES shall be done.
    %% Fix this when TR HS46984 is fixed.
    %%%%
    %% conf_single_schedule_bu(MeId, ?ScheduleName, ActionId),
    AddTime = 120, % sec 
    swm_br_lib:
	conf_single_schedule_bu(?NC_Session,
				MeId, 
				?ScheduleName, 
				AddTime, 
				?ScheduleId),
    timer:sleep(10000),

    %%%%
    %% Check minorType "9175044" (Scheduled Backup Failed) not exist.
    %%%%   
    ct:pal("Check alarm not exist."),
    case swm_test_lib:is_alarm(?NC_Session, MeId, ?AlarmSchedBuFailed) of
	false ->
	    ok;
	true ->
	    ct:fail("Tc will fail, Scheduled Backup Failed alarm. Shall "
		    "not exists")
    end,

    %%%%
    %% Activate failsafe backup
    %%%%
    swm_br_lib:activate_failsafe_bu(?NC_Session, MeId),

    %%%%
    %% Check minorType "9175044" (Scheduled Backup Failed) exist.
    %%%%    
    ct:pal("Check alarm exist."),
    wait_for_alarm_exist(?NC_Session, MeId, ?AlarmSchedBuFailed),
    
    %%%%
    %% Check progressReport when alarm is generated when failsafe triggered.
    %%%% 
    check_sched_prog_report_failsafe_triggered(?NC_Session, MeId, ?ScheduleId),

    %%%%
    %% Wait for failsafe triggered.
    %%%%
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    ct_netconfc:close_session(?NC_Session),
    %% swm_test_lib:wait_for_node_state(ErlNode, down),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
			      [{total_timeout,600000},
			       {idle_timeout,600000}, no_prompt_check]),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),

    swm_test_lib:check_nc_session(?NC_Session),

    %% %%%%
    %% %% Failsafe done, Check no progress ongoing.
    %% %%%%
    %% {progress,[{unset,"true"}],[]} = 
    %% 	swm_br_lib:get_progress_report(?NC_Session, 
    %% 				       MeId, 
    %% 				       {brmFailsafeBackup, ?FailsafeId}),

    %%%%
    %% Check failsafe is in idle.
    %%%%
    %% check_idle_state(?NC_Session,MeId),
    wait_for_idle_state(?NC_Session,MeId),
    timer:sleep(10000),

    %%%%
    %% Check minorType "9175044" (Scheduled Backup Failed) not exist.
    %%%%    
    case swm_test_lib:is_alarm(?NC_Session, MeId, ?AlarmSchedBuFailed) of
    	false ->
    	    ok;
    	true ->
    	    ct:fail("Tc will fail, FallbackOperationStartingSoon shall "
    		    "not exists")
    end,

    %%%%
    %% Set back do default value.
    %%%%
    swm_br_lib:set_failsafe_attribute(?NC_Session, 
				      MeId, 
				      timeoutLength, 
				      "1200"),
    %%%%
    %% Deactivate failsafe backup.
    %%%%
    swm_br_lib:deactivate_failsafe_bu(?NC_Session, MeId),

    delete_all_backups(?NC_Session, MeId),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% @doc 
%%% Check failsafe is Idle. <br/>
%%% @end
%%%--------------------------------------------------------------------
wait_for_idle_state(?NC_Session,MeId) ->
    wait_for_idle_state(?NC_Session,MeId, 180000).
wait_for_idle_state(?NC_Session, _MeId, Timeout) when Timeout < 0 ->
    ct:fail("TC fail due to Idle state not rcvd within 60sec. ");
wait_for_idle_state(?NC_Session,MeId, Timeout) ->
        Get =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmFailsafeBackup',
    		 [{brmFailsafeBackupId,[],["1"]},
    		  {usageState, []}]}]}]}]}]},
    
    swm_test_lib:check_nc_session(?NC_Session),
    case ct_netconfc:get(?NC_Session, Get) of
	{ok, Res} ->
	    ct:log("Res: ~p",[Res]),
	    [{'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'SystemFunctions',[],
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{xmlns,"urn:com:ericsson:ecim:RcsBrM"}],
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',[],
		    [{brmBackupManagerId,[],["1"]},
		     {'BrmFailsafeBackup',
		      [],
		      Info}]}]}]}]}]
		= Res,
	    ct_netconfc:close(?NC_Session),
	    case lists:keyfind(usageState, 1, Info) of
		{usageState,[],["IDLE"]} ->
		    ct:pal("OK, usageState is IDLE.", []),
		    ok;
		{usageState,[],[State]} ->
		    ct:pal("usageState : ~p, is not expected. "
			   "Sleep and try again.", 
			   [State]),
		    timer:sleep(5000),
		    wait_for_idle_state(?NC_Session,MeId, Timeout-5000)
	    end;
	_Other ->
	    ct:log("Res: ~p not expected, Sleep and try again",[_Other]),
	    timer:sleep(5000),
	    wait_for_idle_state(?NC_Session,MeId, Timeout-5000)
    end.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Configure single schedule backup. <br/>
%%% Start with an unlocked scheduler. <br/>
%%% @spec conf_single_schedule_bu(MeId, Name, ActionId) -> ok
%%% @end
%%%--------------------------------------------------------------------
conf_single_schedule_bu(MeId, Name, ActionId) ->
    AddTime = 120, % sec 

    %%%%
    %% Configure single schedule backup, that trigger after 2 min.
    %%%
    swm_br_lib:
	conf_single_schedule_bu(?NC_Session, MeId, Name, AddTime, ?ScheduleId),

    case swm_br_lib:
	wait_for_expecting_state(?NC_Session, 
				 MeId, 
				 ActionId, 
				 "FINISHED", 
				 {brmBackupScheduler, ?ScheduleId}) of
	[{"SUCCESS", 
	  %% "SCHEDULED", 
	  "SCHEDULED_BACKUP",
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  ProgressReport}] ->
	    ct:pal("brmBackupScheduler: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("brmBackupScheduler: ~p~n",[ErrResult]),
    	    ct:fail(ErrResult)
    end,

    ok.


%%%--------------------------------------------------------------------
%%% @doc 
%%% Wait for alarm to exist. Max timeout is 3 min. <br/>
%%% @end
%%%--------------------------------------------------------------------
wait_for_alarm_exist(NC_Sess, MeId, MinorType) ->
    wait_for_alarm_exist(NC_Sess, MeId, MinorType, 180000).

wait_for_alarm_exist(_NC_Sess, _MeId, MinorType, Timeout) when Timeout < 500 ->
    ct:pal("TC will fail, Alarm: ~p not exist within 3 min.",[MinorType]),
    ct:fail("Alarm not exist, within max timeout: 3 min.");
					 
wait_for_alarm_exist(NC_Session, MeId, MinorType, Timeout) ->
    case swm_test_lib:is_alarm(NC_Session, MeId, MinorType) of
	true ->
	    ok;
	false ->
	    ct:log("Sleep 5 sec and check again."),
	    timer:sleep(5000),
	    wait_for_alarm_exist(NC_Session, MeId, MinorType, Timeout - 5000)
    end.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Wait for alarm to be ceased. Max timeout is 1 min. <br/>
%%% @end
%%%--------------------------------------------------------------------
wait_for_alarm_ceased(NC_Sess, MeId, MinorType) ->
    wait_for_alarm_ceased(NC_Sess, MeId, MinorType, 60000).

wait_for_alarm_ceased(_NC_Sess, _MeId, MinorType, Timeout) when Timeout < 500 ->
    ct:pal("TC will fail, Alarm: ~p not ceased within 1 min.",[MinorType]),
    ct:fail("Alarm not ceased, within max timeout: 1 min.");
					 
wait_for_alarm_ceased(NC_Session, MeId, MinorType, Timeout) ->
    case swm_test_lib:is_alarm(NC_Session, MeId, MinorType) of
	false ->
	    ok;
	true ->
	    ct:log("Sleep 5 sec and check again."),
	    timer:sleep(5000),
	    wait_for_alarm_ceased(NC_Session, MeId, MinorType, Timeout - 5000)
    end.

%%%--------------------------------------------------------------------
%%% @doc 
%%% Check progressReport when alarm is generated when failsafe triggered. <br/>
%%% @end
%%%--------------------------------------------------------------------
check_sched_prog_report_failsafe_triggered(NC_Session, MeId, ScheduleId) ->
    check_sched_prog_report_failsafe_triggered(NC_Session, MeId, ScheduleId, 
					       60000).
check_sched_prog_report_failsafe_triggered(_NC_Session, _MeId, _ScheduleId, 
					   Timeout) when Timeout < 500 ->
    ct:fail("TC will fail, progress not expected: within 1 min.");

check_sched_prog_report_failsafe_triggered(NC_Session, MeId, ScheduleId, 
					   Timeout) ->
    ActionId = no_check,
    ct:pal("Check brmBackupScheduler progress, failsafe activated."),
    case swm_br_lib:
	wait_for_expecting_state(NC_Session, 
				 MeId, 
				 ActionId, 
				 "FINISHED", 
				 {brmBackupScheduler, ScheduleId}) of
	[{"NOT_AVAILABLE", %% Result
	  %% "SCHEDULED", %% ActionName
	  "SCHEDULED_BACKUP",
	  %% "The failsafe backup is activated", %% ProgressInfo
	  "Backup creation suppressed while the failsafe backup is activated",
	  "The making of a scheduled backup was omitted", %% ResultInf
	  _State, 
	  ProgressReport}] ->
	    ct:pal("brmBackupScheduler: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("Progress not expected. sleep and try agian.~n"
		   "brmBackupScheduler: ~p~n",
		   [ErrResult]),
	    timer:sleep(5000),
	    check_sched_prog_report_failsafe_triggered(NC_Session, MeId, 
						      ScheduleId, Timeout-5000)
    end.



%%%%%%%%%%%%%%%%%%%%%%%
delete_all_backups(NC_Session, MeId) ->
    %%%%
    %% Get all backup names.
    %%%%
    AllBackups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:log("AllBackups: ~p",[AllBackups]),

    BackupNames = lists:map(fun({'BrmBackup', _, BrmBackup}) ->
				    {value, {backupName, _, [BackupName]}} = 
					lists:keysearch(backupName,1,BrmBackup),
				    BackupName
			   end, AllBackups),

    ct:log("AllBrmBackups Names: ~p",[BackupNames]),

    %%%%
    %% Delete backups.
    %%%%
    lists:foreach(fun(BuName) ->
			  swm_br_lib:delete_backup(NC_Session, MeId, BuName),
			  check_progress(NC_Session, MeId)
		  end, BackupNames).


 check_progress(NC_Session, MeId) ->
    timer:sleep(5000),
    %% ActionId = swm_br_lib:get_action_id(?NC_Session, 
    %% 				      MeId),
    ActionId = no_check,
    case swm_br_lib:
	wait_for_expecting_state(NC_Session, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  "DELETE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup deleted successfully"),
	    ok;
	Result ->
	    ct:pal("Could not delete deleteBackup: ~p~n",[Result])
	    %% ct:fail(Result)
    end.

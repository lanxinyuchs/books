%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_scheduled_backup_SUITE.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R5A/R6A/R8A/R9A/R11A/1
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(swm_scheduled_backup_SUITE).
-vsn('/main/R3A/R4A/R5A/R6A/R8A/R9A/R11A/1').
-author('etxjotj').
-date('2017-08-11').

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R1A/1      2012-09-20 etxjotj     Created
%%% R2A/1      2013-01-17 etxkols     Change ct:pal to ct:log since
%%%                                   Jenkins detects error in console
%%% R2A/5      2013-02-28 etxkols     Added rct_core hook
%%% R1A/6      2013-06-17 etxjovp     Add hook rct_logging
%%% R2A/16     2014-02-18 etxkols     prolonged rpc:call timeout to 10 sec
%%% R2A/18     2013-07-08 etxarnu     Corrected match to return values
%%%                                   for sysNetloader:get_active_instance
%%% R2A/19     2013-07-08 etxarnu     Reverted previous change :(
%%% R2A/22     2014-03-18 etxjotj     Added new logs
%%% R2A/24     2014-04-08 erarafo     Deprecation warning
%%% R2A/25     2014-04-11 etxjotj     Deprecation warning removed continued
%%%                                   development here
%%% R2A/34     2014-08-12 etxivri     Added use of groups.
%%% R2A/34     2014-08-12 etxivri     Increased timetrap, due to calendar tc
%%%                                   needs longer times.
%%%                                   Added clear if tc fail and in end_per_suit
%%% R2A/36     2014-08-13 etxivri     Removed clear if tc fail due to it could
%%%                                   cause next tc fail.
%%% R2A/37     2014-08-13 etxivri     Updates in groups
%%% R2A/38     2014-09-03 etxivri     Changed ct:pal to ct:log
%%% R2A/39     2014-10-07 etxivri     Update to move SUITE to block SWM.
%%% R2A/40     2014-10-09 etxivri     Moved is_alarm to swmSchedBuLib
%%% R3A/4      2015-02-28 etxkols     Preparation for 2 labs
%%% R4A/1      2015-05-07 etxarnu     Corrected get_progress_report_member
%%% R4A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/3      2015-07-15 etxjovp     Add group definitions used by CS CI
%%% R4A/5      2015-10-09 etxjotj     Clock test removed
%%% R4A/6      2015-10-12 etxjotj     Fixed compiler warning unused function
%%% R5A/1      2016-03-29 ekurnik     Updated with actionCapable checks
%%% R5A/2      2016-04-06 ekurnik     Updated for OAM over IPv6
%%% R6A/1      2016-08-22 etxkols     Git migration requires that CC paths is not used 
%%% R8A/1      2017-01-10 etxberb     Replaced check_action_capable("CAPABLE")
%%%                                   with wait_for_action_capable().
%%%
%%% R9A/1      2017-02-23 enekdav     FTPES test group added
%%% R9A/5      2017-03-07 etxberb     Fixed syntax error.
%%% R9A/7      2017-03-07 enekdav     FTPES test group minor fixes
%%% R9A/9      2017-03-08 enekdav     Autoexport group directory positioning fixes
%%% R9A/11     2017-03-14 enekdav     Restoring version 9
%%% R9A/12     2017-03-31 enekdav     Backtracking to version 0, new suite for FTPES group is created
%%% R11A/1     2017-08-11 etxjotj     Fix calendar based schedule test
%%% ----------------------------------------------------------



-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0
	]).
-export([configure_backup_scheduler_single/1,
	 configure_backup_scheduler_periodic/1,
	 configure_backup_scheduler_calendar/1,

	 schedule_test_single/1,
	 schedule_test_single_unlocked/1,
	 schedule_test_periodic/1,
	 schedule_test_periodic_unlocked/1,
	 schedule_test_calendar/1,
	 schedule_test_calendar_unlocked/1,

	 autoexport/1,
	 autoexport_fail/1, 
	 autoexport_fail_lock/1,
	 autoexport_fail_disable/1,

	 scheduled_housekeeping/1,

	 clear_schedule/1
	]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [configure_backup_scheduler_single,
     configure_backup_scheduler_periodic,
     configure_backup_scheduler_calendar,

     schedule_test_single,
     schedule_test_single_unlocked,
     schedule_test_periodic,
     schedule_test_periodic_unlocked,
     schedule_test_calendar,
     schedule_test_calendar_unlocked,

     autoexport,
     autoexport_fail,
     autoexport_fail_lock,
     autoexport_fail_disable,

     scheduled_housekeeping
    ].

groups() ->
    AllGroup = all(),
    [{default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, schedule_1}]},
     {sbc__def__all__2__group, [], [{group, autoexport_1}]},
     {sdc__cover__sim__1__group, [], [{group, schedule_1}]},
     {sdc__cover__sim__2__group, [], [{group, autoexport_1}]},
     {sdc__def__all__1__group, [], [{group, schedule_1}]},
     {sdc__def__all__2__group, [], [{group, autoexport_1}]},
     {sdc__qual__all__1__group, [], []},
     {schedule, [], [{group, schedule_1}]},
     {schedule_1,[],[configure_backup_scheduler_single,
		     configure_backup_scheduler_periodic,
		     schedule_test_single,
		     schedule_test_single_unlocked,
		     schedule_test_periodic,
		     schedule_test_periodic_unlocked,
		     scheduled_housekeeping,
		     configure_backup_scheduler_calendar
		    ]},
     {autoexport, [], [{group, autoexport_1}]},
     {autoexport_1,[],[autoexport,
		       autoexport_fail,
		       autoexport_fail_lock,
		       autoexport_fail_disable
		      ]},
     {calendar,[],[%%configure_backup_scheduler_calendar, %runs in group schdule
		   schedule_test_calendar,
		   schedule_test_calendar_unlocked
		  ]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {hours, 3}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,{nc1, html}},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging,
		  {upgrade , [{erlang,{["ERROR REPORT","CRASH REPORT"],
				       %% This filter here should be unnecessary
				       %% But it seems the log scan gets
				       %% contanimated with printouts from earlier
				       %% testcases
				       ["Program ID [0-9]+ has terminated"]}}]}}
		]}].

%% @hidden
init_per_suite(Config) ->

    %% {Wallclock,Now}=rct_rpc:call(rpc_1,swmBackupScheduler,clock_test,[],10000),
    %% Diff = (element(1, Wallclock)-element(1, Now))*1000000+
    %%  (element(2, Wallclock)-element(2, Now)),
    %% ct:pal("Wallclock: "++format_date(Wallclock)++"~n"++
    %%         "Now time:  "++format_date(Now)++"~n"++
    %%         "Diff: ~w seconds~n",[Diff]),
    %% case Diff of
    %%  Diff when Diff > 300 ->
    %%      ct:fail(large_time_difference);
    %%  _ ->
    %%      ok
    %% end,

    rct_rpc:call(rpc_1, swmLib, set_variable, [scheduler_debug, true], 10000),
    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),

    Name = "swm_backup_SUITE",
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    [{host, SftpHost},{username, Username},{password, Password}] = 
        ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    [{meId, MeId},
     {backupName, Name},
     {sftp_host, SftpHost},
     {sftp_user, Username},
     {sftp_pass, Password},
     %% {sftp_host, "10.68.200.11"},
     %% {sftp_user, "mauve"},
     %% {sftp_pass, "dilbert"},
     {sftp_root, RootDir} | Config].
%% @hidden
end_per_suite(Config) ->
    ct:print("Ending suite~n",[]),
    rct_rpc:call(rpc_1, swmLib, erase_variable, [scheduler_debug], 10000),
    clear_schedule(Config),
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:print("Running ~w~n",[TestCase]),
    set_admin_state(Config, "LOCKED"),
    clear_schedule(Config),
    rct_rpc:call(rpc_1, swmLib, set_variable, [swm_basic_tests_return_pid, self()], 10000),
    rct_rpc:call(rpc_1, swmServer, start_test_event_server, [], 10000),
    Config.
%% @hidden
end_per_testcase(TestCase, _Config) ->
    ct:print("Ending ~w~n",[TestCase]),
    rct_rpc:call(rpc_1, swmLib, erase_variable, [backup_test_sched_bu_wait], 
		 10000),
    rct_rpc:call(rpc_1, swmLib, erase_variable, [swm_basic_tests_return_pid], 10000),
    rct_rpc:call(rpc_1, swmServer, stop_test_event_server, [], 10000),
    ok.

%% format_date(Now) ->
%%     {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_datetime(Now),
%%     Frac = element(3, Now),
%%     io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w UTC",
%%        [Y,Mo,D,H,Mi,S,Frac]).


%%--------------------------------------------------------------------
%% @doc NodeUC444.N Configure Backup Scheduler, single event
%% @end

configure_backup_scheduler_single(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),
    TS = timestamp(),
    FutureTime = add_time(add_time(), TS),
    NiceCases =
	[{Name++"_timezone", iso_time(FutureTime, extended)},
	 {Name++"_local_time", iso_time(FutureTime, extended_zonefree)},
	 {Name++"_universal_time", iso_time(FutureTime, extended_z)}],
    ok= swmSchedBuLib:set_scheduled_backup_name(MeId, Name),
    [begin
	 Attributes = [{brmSingleEventId, Id},
		       {scheduledTime, Time}],
	 ok = swmSchedBuLib:create_single_event(MeId, Attributes)
     end||{Id,Time} <- NiceCases].

%%--------------------------------------------------------------------
%% @doc NodeUC444.N Configure Backup Scheduler, periodic event
%% @end

configure_backup_scheduler_periodic(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),
    ok = swmSchedBuLib:set_scheduled_backup_name(MeId, Name),

    TS = timestamp(),
    StartTime = iso_time(add_time(1000, TS), extended),
    StopTime = iso_time(add_time(20000, TS), extended),
    Attributes = [{brmPeriodicEventId, "swm_scheduled_backup_SUITE"},
		  {startTime, StartTime},
		  {stopTime, StopTime},
		  {hours, "0"},
		  {minutes, "5"}],
    ok = swmSchedBuLib:create_periodic_event(MeId, Attributes).
%%--------------------------------------------------------------------
%% @doc NodeUC444.N Configure Backup Scheduler, calendar based
%% @end

configure_backup_scheduler_calendar(Config) ->
    MeId = proplists:get_value(meId, Config),
    TS = timestamp(),
    StartTime = iso_time(add_time(1000, TS), extended),
    StopTime = iso_time(add_time(86400*2, TS), extended),
    Time = "13:34:00",
    Attributes = 
	[{brmCalendarBasedPeriodicEventId, "swm_scheduled_backup_SUITE"},
	 {startTime, StartTime},
	 {stopTime, StopTime},
	 {time, Time}],
    ok = swmSchedBuLib:create_calendar_event(MeId, Attributes).

%%--------------------------------------------------------------------
%% @doc NodeUC445.N Create a backup at a configured schedule, single event
%% Configure a single event to occur 15 seconds from now
%% Start with a locked scheduler
%% @end


schedule_test_single(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    ok = swmSchedBuLib:set_scheduled_backup_name(MeId, Name),

    ProgressFilter = get_progress_filter(MeId),
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    FutureTime = add_time(add_time(), TS),
    LegalTime = iso_time(FutureTime, extended),

    ct:pal("Scheduled time is ~p~n",[LegalTime]),

    Attributes = [{brmSingleEventId, [Name++"_sct"]},
		  {scheduledTime,[LegalTime]}],

    ok = swmSchedBuLib:create_single_event(MeId, Attributes),
    set_admin_state(Config, "UNLOCKED"),
    timer:sleep(3000),
    ct:pal("Announced time is ~p~n", [get_next_scheduled_time(MeId)]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,
    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,

    %% case get_next_scheduled_time(MeId) of
    %%  LegalTime ->
    %%      ct:pal("Announced time is ok~n",[]),
    %%      ok;
    %%  NextTime ->
    %%      ct:pal("Scheduling fault!~nExpected: ~p~nAnnounced: ~p~n",
    %%         [LegalTime, NextTime]),
    %%      ct:fail(scheduling_fault)
    %% end,

    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                           {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    ok;
	Result ->
	    ct:pal("SCHEDULED_BACKUP: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC445.N Create a backup at a configured schedule, single event
%% Configure a single event to occur 15 seconds from now
%% Start with an unlocked scheduler
%% @end


schedule_test_single_unlocked(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    set_admin_state(Config, "UNLOCKED"),

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    %% {{Year,_,_},Time} = calendar:now_to_local_time({N1,N2,N3}),
    FutureTime = add_time(add_time(), TS),
    LegalTime = iso_time(FutureTime, extended),

    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {scheduledBackupName, [Name]},
		  {'BrmSingleEvent', [],
		   [{brmSingleEventId, [Name++"_sct"]},
		    {scheduledTime,[LegalTime]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]),
    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    timer:sleep(3000),
    ct:pal("Announced time is ~p~n", [get_next_scheduled_time(MeId)]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,
    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,
    %% case get_next_scheduled_time(MeId) of
    %%  LegalTime ->
    %%      ct:pal("Announced time is ok~n",[]),
    %%      ok;
    %%  NextTime ->
    %%      ct:pal("Scheduling fault!~nExpected: ~p~nAnnounced: ~p~n",
    %%         [LegalTime, NextTime]),
    %%      ct:fail(scheduling_fault)
    %% end,

    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                           {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    ok;
	Result ->
	    ct:pal("SCHEDULED_BACKUP: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC445.N Create a backup at a configured schedule, periodic event
%% Configure a periodic event to occur 15 seconds from now
%% Start with a locked scheduler
%% @end


schedule_test_periodic(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    %% {{Year,_,_},Time} = calendar:now_to_local_time({N1,N2,N3}),
    FutureTime = add_time(add_time(), TS),
    LegalTime = iso_time(FutureTime, extended),

    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {scheduledBackupName, [Name]},
		  {'BrmPeriodicEvent', [],
		   [{brmPeriodicEventId, [Name++"_sct"]},
		    {hours, ["1"]},
						%           {stopTime, ["2036-12-31T00:00:00"]},
		    {startTime,[LegalTime]}]}]}]}]}]}]},
    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    ok = netconf(edit_config, [nc1, running, Set]),
    set_admin_state(Config, "UNLOCKED"),
    timer:sleep(3000),
    Announced = get_next_scheduled_time(MeId),
    ct:pal("Announced time is ~p~n", [Announced]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,
    %% case get_next_scheduled_time(MeId) of
    %%  LegalTime ->
    %%      ct:pal("Announced time is ok~n",[]),
    %%      ok;
    %%  NextTime ->
    %%      ct:pal("Scheduling fault!~nExpected: ~p~nAnnounced: ~p~n",
    %%         [LegalTime, NextTime]),
    %%      ct:fail(scheduling_fault)
    %% end,

    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                           {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    timer:sleep(5000),
	    case get_next_scheduled_time(MeId) of
		Announced ->
		    ct:pal("Scheduling returned the previous time: ~s~n",
			   [Announced]),
		    ct:fail(scheduling_error);
		NextTime2 ->
		    ct:pal("Next scheduled time: ~p~n",[NextTime2]),
		    ok
	    end;
	Result ->
	    ct:pal("SCHEDULED_BACKUP: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC445.N Create a backup at a configured schedule, periodic event
%% Configure a periodic event to occur 15 seconds from now
%% Start with an unlocked scheduler
%% @end


schedule_test_periodic_unlocked(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    set_admin_state(Config, "UNLOCKED"),

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    %% {{Year,_,_},Time} = calendar:now_to_local_time({N1,N2,N3}),
    FutureTime = add_time(add_time(), TS),
    LegalTime = iso_time(FutureTime, extended),

    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {scheduledBackupName, [Name]},
		  {'BrmPeriodicEvent', [],
		   [{brmPeriodicEventId, [Name++"_sct"]},
		    {hours, ["1"]},
						%           {stopTime, ["2036-12-31T00:00:00"]},
		    {startTime,[LegalTime]}]}]}]}]}]}]},

    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    ok = netconf(edit_config, [nc1, running, Set]),
    timer:sleep(3000),
    Announced = get_next_scheduled_time(MeId),
    ct:pal("Announced time is ~p~n", [Announced]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,

    %% case get_next_scheduled_time(MeId) of
    %%  LegalTime ->
    %%      ct:pal("Announced time is ok~n",[]),
    %%      ok;
    %%  NextTime ->
    %%      ct:pal("Scheduling fault!~nExpected: ~p~nAnnounced: ~p~n",
    %%         [LegalTime, NextTime]),
    %%      ct:fail(scheduling_fault)
    %% end,

    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    timer:sleep(5000),
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                           {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    case get_next_scheduled_time(MeId) of
		Announced ->
		    ct:pal("Scheduling returned the previous time: ~s~n",
			   [Announced]),
		    ct:fail(scheduling_error);
		NextTime2 ->
		    ct:pal("Next scheduled time: ~p~n",[NextTime2]),
		    ok
	    end;
	Result ->
	    ct:pal("SCHEDULED_BACKUP: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC445.N Create a backup at a configured schedule, calendar event
%% Configure a calendar based periodic event to occur 15 seconds from now
%% Start with a locked scheduler
%% @end


schedule_test_calendar(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",


    ProgressFilter = get_progress_filter(MeId),
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    FutureTime = add_time(add_time(), TS),
    LegalTime = fn_time(FutureTime, extended_z)--"Z",

    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    Attributes = [{brmCalendarBasedPeriodicEventId, Name++"_sct"},
		  {time, LegalTime}],
    ok = swmSchedBuLib:create_calendar_event(MeId, Attributes),
    set_admin_state(Config, "UNLOCKED"),
    timer:sleep(3000),
    Announced = get_next_scheduled_time(MeId),
    ct:pal("Announced time is ~p~n", [Announced]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,

    %% case get_next_scheduled_time(MeId) of
    %%  LegalTime ->
    %%      ct:pal("Announced time is ok~n",[]),
    %%      ok;
    %%  NextTime ->
    %%      ct:pal("Scheduling fault!~nExpected: ~p~nAnnounced: ~p~n",
    %%         [LegalTime, NextTime]),
    %%      ct:fail(scheduling_fault)
    %% end,

    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    timer:sleep(5000),
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                           {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    case get_next_scheduled_time(MeId) of
		Announced ->
		    ct:pal("Scheduling returned the previous time: ~s~n",
			   [Announced]),
		    ct:fail(scheduling_error);
		NextTime2 ->
		    ct:pal("Next scheduled time: ~p~n",[NextTime2]),
		    ok
	    end;
	Result ->
	    ct:pal("SCHEDULED_BACKUP: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC445.N Create a backup at a configured schedule, calendar event
%% Configure a calendar based periodioc event to occur 15 seconds from now
%% Start with an unlocked scheduler
%% @end


schedule_test_calendar_unlocked(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    set_admin_state(Config, "UNLOCKED"),

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    FutureTime = add_time(add_time(), TS),
    LegalTime = fn_time(FutureTime, extended_z)--"Z",

    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {scheduledBackupName, [Name]},
		  {'BrmCalendarBasedPeriodicEvent', [],
		   [{brmCalendarBasedPeriodicEventId, [Name++"_sct"]},
		    {time,[LegalTime]}]}]}]}]}]}]},

    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    ok = netconf(edit_config, [nc1, running, Set]),
    timer:sleep(3000),
    Announced = get_next_scheduled_time(MeId),
    ct:pal("Announced time is ~p~n", [Announced]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,
    %% case get_next_scheduled_time(MeId) of
    %%  LegalTime ->
    %%      ct:pal("Announced time is ok~n",[]),
    %%      ok;
    %%  NextTime ->
    %%      ct:pal("Scheduling fault!~nExpected: ~p~nAnnounced: ~p~n",
    %%         [LegalTime, NextTime]),
    %%      ct:fail(scheduling_fault)
    %% end,

    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    timer:sleep(5000),
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
                                                           {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    case get_next_scheduled_time(MeId) of
		Announced ->
		    ct:pal("Scheduling returned the previous time: ~s~n",
			   [Announced]),
		    ct:fail(scheduling_error);
		NextTime2 ->
		    ct:pal("Next scheduled time: ~p~n",[NextTime2]),
		    ok
	    end;
	Result ->
	    ct:pal("SCHEDULED_BACKUP: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc Auto export
%% Configure a single event to occur some time from now and enable auto export
%% Start with a locked scheduler
%% @end


autoexport(Config) ->
    case do_autoexport(Config) of
	["SUCCESS"] ->
	    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
							   {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
                                                           {"CAPABLE", ""}]),
	    ok = swmTestLib:wait_for_action_capable(),
	    ok;
	Result ->
	    ct:pal("SCHEDULED_EXPORT: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc Auto export fail
%% Try to use auto export with a faulty password, check for the alarm to 
%% be sent out. Then try auto export with correct password and see that the
%% alarm goes a way
%% @end

autoexport_fail(Config) ->
    MeId = proplists:get_value(meId, Config),

    case is_alarm(MeId, "9175045") of
	true ->
	    ct:fail("AutoExportBackupFailed alarm is already present");
	false ->
	    ok
    end,

    Pass = proplists:get_value(sftp_pass, Config),
    NewConfig = lists:keyreplace(sftp_pass, 1, Config, {sftp_pass, Pass++"1"}),
    case do_autoexport(NewConfig) of
	["FAILURE"] ->
	    timer:sleep(5000),
	    case is_alarm(MeId, "9175045") of
		true ->
		    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
								   {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
								   {"CAPABLE", ""}]),
		    ok = swmTestLib:wait_for_action_capable(),
		    ok;
		false ->
		    ct:fail("AutoExportBackupFailed alarm is not present")
	    end,
	    case do_autoexport(Config) of
		["SUCCESS"] -> 
		    timer:sleep(5000),
		    case is_alarm(MeId, "9175045") of
			true ->
			    ct:fail("AutoExportBackupFailed alarm not cleared");
			false ->
			    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
									   {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
									   {"CAPABLE", ""}]),
			    ok = swmTestLib:wait_for_action_capable(),
			    ok
		    end,
		    ok;
		Result2 ->
		    ct:pal("SCHEDULED_EXPORT: ~s~n",[Result2]),
		    ct:fail(Result2)
	    end;
	[Result] ->
	    ct:pal("Result : ~p~n",[Result]),
	    ct:fail({unexpected_result, Result})
    end.

%%--------------------------------------------------------------------
%% @doc Auto export fail and lock
%% Try to use auto export with a faulty password, check for the alarm to 
%% be sent out. Then lock the scheduler and see that the alarm goes a way
%% @end

autoexport_fail_lock(Config) ->
    MeId = proplists:get_value(meId, Config),

    case is_alarm(MeId, "9175045") of
	true ->
	    ct:fail("AutoExportBackupFailed alarm is already present");
	false ->
	    ok
    end,

    Pass = proplists:get_value(sftp_pass, Config),
    NewConfig = lists:keyreplace(sftp_pass, 1, Config, {sftp_pass, Pass++"1"}),
    case do_autoexport(NewConfig) of
	["FAILURE"] ->
	    timer:sleep(5000),
	    case is_alarm(MeId, "9175045") of
		true ->
		    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
								   {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
								   {"CAPABLE", ""}]),
		    ok = swmTestLib:wait_for_action_capable(),
		    ok;
		false ->
		    ct:fail("AutoExportBackupFailed alarm is not present")
	    end,

	    set_admin_state(Config, "LOCKED"),
	    timer:sleep(5000),
	    case is_alarm(MeId, "9175045") of
		true ->
		    ct:fail("AutoExportBackupFailed alarm not cleared");
		false ->
		    ok
	    end;
	[Result] ->
	    ct:pal("Result : ~p~n",[Result]),
	    ct:fail({unexpected_result, Result})
    end.

%%--------------------------------------------------------------------
%% @doc Auto export fail and disabled
%% Try to use auto export with a faulty password, check for the alarm to 
%% be sent out. Then disable autoexport and see that the alarm goes a way
%% @end

autoexport_fail_disable(Config) ->
    MeId = proplists:get_value(meId, Config),

    case is_alarm(MeId, "9175045") of
	true ->
	    ct:fail("AutoExportBackupFailed alarm is already present");
	false ->
	    ok
    end,

    Pass = proplists:get_value(sftp_pass, Config),
    NewConfig = lists:keyreplace(sftp_pass, 1, Config, {sftp_pass, Pass++"1"}),
    case do_autoexport(NewConfig) of
	["FAILURE"] ->
	    timer:sleep(5000),
	    case is_alarm(MeId, "9175045") of
		true ->
		    ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("SCHEDULED_BACKUP")},
								   {"WAIT", swmTestLib:map_current_action("SCHEDULED_EXPORT")},
								   {"CAPABLE", ""}]),
		    ok = swmTestLib:wait_for_action_capable(),
		    ok;
		false ->
		    ct:fail("AutoExportBackupFailed alarm is not present")
	    end,

	    Set =
		{'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],[MeId]},
		  {'SystemFunctions',
		   [{systemFunctionsId,[],["1"]},
		    {'BrM',
		     [{brMId,[],["1"]},
		      {'BrmBackupManager',
		       [{brmBackupManagerId,[],["1"]},
			{'BrmBackupScheduler',
			 [{brmBackupSchedulerId,[], ["1"]},
			  {autoExport, ["DISABLED"]}]}]}]}]}]},
	    ok = netconf(edit_config, [nc1, running, Set]),
	    ct:pal("Autoexport disabled",[]),
	    timer:sleep(5000),
	    case is_alarm(MeId, "9175045") of
		true ->
		    ct:fail("AutoExportBackupFailed alarm not cleared");
		false ->
		    ok = swmTestLib:wait_for_action_capable(),
		    ok
	    end;
	[Result] ->
	    ct:pal("Result : ~p~n",[Result]),
	    ct:fail({unexpected_result, Result})
    end.

%%--------------------------------------------------------------------
%% @doc Housekeeping for scheduled backups
%% Check that backups are removed 
%% @end


scheduled_housekeeping(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    set_admin_state(Config, "UNLOCKED"),

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},

    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    %% {{Year,_,_},Time} = calendar:now_to_local_time({N1,N2,N3}),
    FutureTime1 = add_time(add_time(), TS),
    FutureTime2 = add_time(add_time()+30, TS),
    FutureTime3 = add_time(add_time()+60, TS),
    FutureTime4 = add_time(add_time()+90, TS),
    LegalTime1 = iso_time(FutureTime1, extended),
    LegalTime2 = iso_time(FutureTime2, extended),
    LegalTime3 = iso_time(FutureTime3, extended),
    LegalTime4 = iso_time(FutureTime4, extended),

    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {scheduledBackupName, [Name]},
		  {maxStoredScheduledBackups, ["3"]},
		  {'BrmSingleEvent', [],
		   [{brmSingleEventId, [Name++"_shk1"]},
		    {scheduledTime,[LegalTime1]}]},
		  {'BrmSingleEvent', [],
		   [{brmSingleEventId, [Name++"_shk2"]},
		    {scheduledTime,[LegalTime2]}]},
		  {'BrmSingleEvent', [],
		   [{brmSingleEventId, [Name++"_shk3"]},
		    {scheduledTime,[LegalTime3]}]},
		  {'BrmSingleEvent', [],
		   [{brmSingleEventId, [Name++"_shk4"]},
		    {scheduledTime,[LegalTime4]}]}
		 ]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]),
    LegalTimes = [LegalTime1, LegalTime2, LegalTime3, LegalTime4],
    wait_for_backups(ProgressFilter, ActionId, MeId, LegalTimes).

wait_for_backups(ProgressFilter, ActionId, MeId, [LegalTime|LegalTimes]) ->

    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    timer:sleep(3000),
    ct:pal("Announced time is ~p~n", [get_next_scheduled_time(MeId)]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    NewActionId = get_action_id(ProgressFilter),
	    Backups = get_all_backups(MeId),
	    ct:print("Number of backups: ~p~n",[length(Backups)]),
	    wait_for_backups(ProgressFilter, NewActionId, MeId, LegalTimes);
	Result ->
	    ct:pal("SCHEDULED_BACKUP: ~s~n",[Result]),
	    ct:fail(Result)
    end;
wait_for_backups(_, _, MeId, []) ->
    Backups = get_all_backups(MeId),
    case length(Backups) of
	L when L < 4 ->
	    ok;
	_ ->
	    ct:pal("Number of backups exceeed: ~n~p~n",[Backups]),
	    ct:fail(number_of_backups_exceeded)
    end.

get_all_backups(MeId) ->
    Get =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]}]}]}]}]},
    {ok, Result} = netconf(get, [nc1, Get]),
    {ok, {_, _, Contents}} = extract_element('BrmBackupManager', Result),
    [BrmBackupE||BrmBackupE<-Contents,
		 element(1, BrmBackupE) == 'BrmBackup',
		 case extract_element(creationType, [BrmBackupE]) of
		     {ok, {_, _, ["SCHEDULED"]}} ->
			 true;
		     _ ->
			 false
		 end].




clear_schedule(Config) ->
    MeId = proplists:get_value(meId, Config),
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'BrM',
	       [{brMId,[],["1"]},
		{'BrmBackupManager',
		 [{brmBackupManagerId,[],["1"]},
		  {'BrmBackupScheduler',
		   [{brmBackupSchedulerId,[], ["1"]}]}]}]}]}]},
    {ok, Res} = netconf(get_config, [nc1, running, Get]),
    {ok, {'BrmBackupScheduler', Attrs, Content}} =
	extract_element('BrmBackupScheduler', Res),
    Delete = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	       {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'BrmBackupScheduler', Attrs,
		      [{brmBackupSchedulerId,[], ["1"]}|
		       [case element(1, Item) of
			    'BrmSingleEvent' = Element ->
				{ok, Id} = extract_element(
					     brmSingleEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]};
			    'BrmPeriodicEvent' = Element->
				{ok, Id} = extract_element(
					     brmPeriodicEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]};
			    'BrmCalendarBasedPeriodicEvent' = Element ->
				{ok, Id} = extract_element(
					     brmCalendarBasedPeriodicEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]}
			end||Item<-Content,
			     (element(1,Item) == 'BrmSingleEvent') or
								     (element(1, Item) == 'BrmPeriodicEvent') or
														(element(1, Item) == 'BrmCalendarBasedPeriodicEvent')]
		      ]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Delete]).




%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

do_autoexport(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config)++"_sct",

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),

    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),
    FutureTime = add_time(add_time(), TS),
    LegalTime = iso_time(FutureTime, extended),
    Password = proplists:get_value(sftp_pass, Config),
    Uri = sftp_uri(Config),
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {scheduledBackupName, [Name]},
		  {autoExport, ["ENABLED"]},
		  {autoExportPassword, [],
		   [{cleartext, [], []},
		    {password, [], [Password]}]},
		  {autoExportUri, [], [Uri]},
		  {'BrmSingleEvent', [],
		   [{brmSingleEventId, [Name++"_sct"]},
		    {scheduledTime,[LegalTime]}]}]}]}]}]}]},
    ct:pal("Scheduled time is ~p~n",[LegalTime]),

    %% This will cause the system to wait 60 seconds for a go ahead message
    %% before it resets the progress report
    ok = rct_rpc:call(rpc_1, swmLib, set_variable, 
		      [backup_test_sched_bu_wait, true], 10000),

    ok = netconf(edit_config, [nc1, running, Set]),
    set_admin_state(Config, "UNLOCKED"),
    timer:sleep(3000),
    Announced = get_next_scheduled_time(MeId),
    ct:pal("Announced time is ~p~n", [Announced]),

    case is_alarm(MeId, "9175044") of
	true ->
	    ct:fail("ScheduledBackupFailed alarm is present");
	false ->
	    ok
    end,

    ok = swmTestLib:wait_for_action_capable(),
    swmTestLib:flush_mnesia_event_messages(),

    BackupDn = 
	case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	    ["SUCCESS"] ->
		Res = get_result_info(ProgressFilter),
		ct:pal("resultInfo: ~p~n",[Res]),
		Res;
	    Result ->
		ct:pal("schedule Backup: ~s~n",[Result]),
		ct:fail(Result)
	end,
    rct_rpc:call(rpc_1, erlang, send, [swmBackupScheduler, go_ahead], 10000),
    Index = lists:last(string:tokens(BackupDn, "=,")),
    ProgressFilter2 = {'ManagedElement',
		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId,[],[MeId]},
			{'SystemFunctions',
			 [{systemFunctionsId,[],["1"]},
			  {'BrM',
			   [{brMId,[],["1"]},
			    {'BrmBackupManager',
			     [{brmBackupManagerId,[],["1"]},
			      {'BrmBackup',
			       [{brmBackupId, [], [Index]},
				{progressReport, []}]}]}]}]}]},
    %% This should be a newly created backup Mo so it won't have a previous
    %% ActionId
    wait_for_progress(progressReport, undefined, ProgressFilter2).

sftp_uri(Config) ->
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    Dir = proplists:get_value(sftp_root, Config),
    "sftp://"++User++"@"++Host++Dir.


is_alarm(MeId, MinorType) ->
    Res = swmSchedBuLib:is_alarm(nc1, MeId, MinorType),
    Res.

get_progress_filter(MeId) ->
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	{'BrM',
	 [{brMId,[],["1"]},
	  {'BrmBackupManager',
	   [{brmBackupManagerId,[],["1"]},
	    {'BrmBackupScheduler',
	     [{brmBackupSchedulerId, [], ["1"]},
	      {progressReport, []}]}]}]}]}]}.


add_time() ->
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode, [], 10000) of
	simulated ->
	    15;
	target ->
	    120
    end.

%%%--------------------------------------------------------------------
%%% Description: Set the BrmBackupScheduler.adminState attribute
%%%--------------------------------------------------------------------

set_admin_state(Config, Value) ->
    MeId = proplists:get_value(meId, Config),
    Set =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'BrM',
             [{brMId,[],["1"]},
              {'BrmBackupManager',
               [{brmBackupManagerId,[],["1"]},
		{'BrmBackupScheduler',
		 [{brmBackupSchedulerId,[], ["1"]},
		  {adminState, [Value]}]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Set]).

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
%%% Description: Read the value of BrmBackupScheduler.nextScheduledTime
%%%--------------------------------------------------------------------

get_next_scheduled_time(MeId) ->
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'BrM',
	       [{brMId,[],["1"]},
		{'BrmBackupManager',
		 [{brmBackupManagerId,[],["1"]},
		  {'BrmBackupScheduler',
		   [{brmBackupSchedulerId, [], ["1"]},
		    {nextScheduledTime, []}]}]}]}]}]},

    {ok, A} = netconf(get, [nc1, Get]),
    case extract_element(nextScheduledTime, A) of
	{ok, {nextScheduledTime, [{unset, "true"}], []}} ->
	    undefined;
	{ok, {nextScheduledTime, _, [Time]}} ->
	    Time
    end.



%%% ----------------------------------------------------------
%%% @doc Convert an os:timestamp() tuple to an ISO 8601 string
%%%
%%% Input: Now  - An os:timestamp() tuple
%%%        Type - basic|extended|extended_zonefree|extended_z
%%% Output: string()
%%% @end
%%% ----------------------------------------------------------

iso_time(Now, Type) ->
    fn_date(Now, Type)++"T"++fn_time(Now, Type).

%% time_offset(Now) ->
%%     DT = calendar:now_to_local_time(Now),
%%     UTC = calendar:now_to_universal_time(Now),
%%     DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
%%         calendar:datetime_to_gregorian_seconds(UTC),
%%     [Sign, DH, DM] = diff(DiffSecs),
%%     lists:append([[Sign], padzero(DH), ":", padzero(DM)]).


fn_date(Now, Type) ->
    {{Y,M,D}, _} = calendar:now_to_local_time(Now),
    case Type of
        basic ->
            lists:append([integer_to_list(Y),
                          padzero(M),
                          padzero(D)]);
	extended_z ->
	    {{YU,MU,DU}, _} = calendar:now_to_universal_time(Now),

            lists:append([integer_to_list(YU), "-",
                          padzero(MU), "-", padzero(DU)]);

        Extended when Extended==extended; Extended==extended_zonefree ->
            lists:append([integer_to_list(Y), "-",
                          padzero(M), "-", padzero(D)])
    end.

fn_time(Now, Type) ->
    DT={_, {H, M, S}} = calendar:now_to_local_time(Now),
    UTC = calendar:now_to_universal_time(Now),
    DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC),
    [Sign, DH, DM] = diff(DiffSecs),
    case Type of
        basic ->
            lists:append([padzero(H),
                          padzero(M),
                          padzero(S),
                          [Sign],
                          padzero(DH),
                          padzero(DM)]);
	extended_z ->
	    {_, {HU, MU, SU}} = UTC,
            lists:append([padzero(HU), ":",padzero(MU), ":",padzero(SU),"Z"]);
        extended ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S),
                          [Sign], padzero(DH), ":", padzero(DM)]);
        extended_zonefree ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S)])
    end.

%% format_date({{Y,M,D},{H,Mi,S}}) ->
%%     lists:append([integer_to_list(Y), "-", padzero(M), "-", padzero(D),"T",
%%        padzero(H), ":", padzero(Mi), ":", padzero(S)]).


padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.


diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
        {0, {H, M,_}} ->
	    [$+, H, M];
        {-1, _} ->
	    {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
	    [$-, H, M]
    end.

%%%--------------------------------------------------------------------
%%% Description: Adds seconds to a now-tuple
%%%--------------------------------------------------------------------

add_time(N, {N1,N2,N3}) ->
    case N2+N of
	F2 when F2 >= 1000000 ->
	    {N1+F2 div 1000000, F2 rem 1000000, N3};
	F2 ->
	    {N1, F2, N3}
    end.

%%%--------------------------------------------------------------------
%%% Description: Get node time
%%%--------------------------------------------------------------------

timestamp() ->
    rct_rpc:call(rpc_1, os, timestamp, [], 10000).


%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------

%% cmd(Cmd) ->
%%     ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
%%     R.

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	{ok, Res} ->
	    ok = ct_netconfc:close_session(nc1),
	    {ok, Res};
	Other ->
	    ct:fail(Other)
    end.

%%%--------------------------------------------------------------------
%%% Description: This function and the associated check_progress_elements
%%%              loops over a netconf get for the progress information until
%%%              the state is FINISHED, otherwise it prints the status and
%%%              progress percentage
%%%--------------------------------------------------------------------


wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(5000),
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    wait_for_progress(Attribute, OldActionId,
					      ProgressFilter);
			not_found ->
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
		    ct:log("Netconf get error:~n~p",[Error]),
		    wait_for_progress(Attribute, OldActionId, ProgressFilter)
	    end;
	{error, Reason} ->
	    ct:log("Netconf open error:~n~p",[Reason]),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    {ok, State} = extract_element(state, [Report]),
    case State of
	{state, _, ["FINISHED"]} ->
	    format_progress_report(Report),
	    ct:pal("~s",[format_progress_report(Report)]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, Result};
	{state, _, [Current]} ->
	    {ok, {progressPercentage, _, [Percent]}} =
		extract_element(progressPercentage, [Report]),
	    {ok, {progressInfo, _, [Info]}} =
		extract_element(progressInfo, [Report]),
	    ct:pal("State: ~s ~s % ready~n~s",[Current, Percent, Info]),
	    loop
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


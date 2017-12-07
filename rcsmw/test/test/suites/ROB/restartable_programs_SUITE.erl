 %%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	restartable_programs_SUITE.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/1
%%%
%%% @doc ==Restart differents startable programs.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%% Note! kill_beam can only be runed on target due to hart does not start beam on sim node.<br/>
%%%
%%%
%%% @end

-module(restartable_programs_SUITE).
-author('etxarnu').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/1').
-date('2017-11-25').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R2A/1      2012-11-15 etxivri     Created
%%% R2A/3      2012-11-22 etxivri     some cleanup.
%%% R2A/4      2012-11-23 etxivri     Added kill beam. For now it is only runed on target.
%%% R2A/5      2012-12-03 etxivri     Added init_restart.
%%% R2A/9      2013-01-07 etxivri     Fixed check of pids after restart of programs controlled by appm.
%%% R2A/10     2013-02-04 etxkols     Changed ct:pal to ct:log to avoid Jenkins match error
%%% R2A/11     2013-02-29 etxkols     Added rct_core hook
%%% R2A/12     2013-03-01 etxivri     Increased timout in kill_beam.
%%% R2A/13     2013-03-07 etxarnu     Wait a while for applications after COM restareted
%%% R2A/14     2013-03-19 etxkols     Replaced rpc:call with telnet when "kill -9 beam"
%%% R2A/15     2013-04-15 etxivri     Removed check of new beam pid in kill_beam.
%%% R2A/16     2013-04-18 etxpeno     Support for new APPM implementation
%%% R2A/17     2013-05-03 etxivri     Increased timeout on rct_rpc:call for "pgrep com"
%%% R2A/18     2013-05-20 etxivri     Added $USER in "pgrep com".
%%% R2A/19     2013-05-23 etxivri     increased timeout in rct_rpc:call.
%%% R2A/20     2013-06-28 etxivri     increased timeout in wait_for_appl_started.
%%% R2A/21     2013-07-01 etxarnu     increased timeout to 10 sec in init_restart
%%%                                   when waiting for applications to start
%%% R2A/23     2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/24     2013-08-19 etxkivri    Activated TC "kill_prog_contr_by_appm" to be runed again.
%%% R2A/25     2013-08-26 etxkivri    removed one kill_prog_contr_by_appm_random TC in groups
%%%                                   due to after 3 kill -9 no more restart of programs 
%%%                                   will be performed.
%%% R2A/26     2013-08-26 etxkivri    Removed more kill_prog_contr_by_appm_random TC from groups.
%%% R2A/27     2013-08-26 etxkivri    Added new TCs to check behaviour when application  
%%%                                   restarts with differents reasons. 
%%%                                   New way to get BeamPid. Remowed kill COM, it is runed in COM suite.
%%% R2A/28     2013-08-27 etxkivri    Removed check_restart_counter_reset_after_5min from groups.
%%% R2A/28     2013-10-11 etxkivri    Comment out kill prog in sim group due to usntable results.
%%% R2A/29     2013-10-14 etxkivri    Comment out init_restart in sim group due to usntable results.
%%% R2A/31     2013-10-14 etxkivri    Prepared two more TCs. And some update in wait  
%%%                                   for appl to be started.
%%% R2A/32     2013-10-15 etxkivri    Added one more TC that check when appl restarts 
%%%                                   more than 3 times within 5 min.
%%% R2A/33     2013-10-18 etxkivri    Updates for TCU.
%%% R2A/34     2013-11-19 etxarnu     Preparations for program groups etc.
%%% R2A/35     2013-11-19 etxarnu     Added test for program escalation 
%%% R2A/35     2013-12-06 etxivri     Removed TC check_appl_restarts_4times_within_5min,
%%%                                   due to behaviour is obsolete.
%%% R2A/35     2013-12-10 etxivri     Increased poll time to check netconf is ok.
%%% R2A/38     2013-12-11 etxivri     Increased timeout for netconf to start.
%%% R2A/39     2013-12-11 etxivri     Increased all rct_rpc timeouts.
%%% R2A/42     2014-01-16 etxkols     Support for dus5201. 
%%% R2A/43     2014-02-27 etxarnu     Adjusted for one less crash before escalation
%%% R2A/44     2014-03-10 etxarnu     Removed enabled/disable escalation calls in start/stop_suite
%%% R2A/46     2014-03-24 etxarnu     Fixed wait_for_new_beam_pid to handle cold
%%% R2A/48     2014-04-02 etxarnu     Adapted to new appmServer printouts
%%% R2A/50     2014-06-03 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/51     2014-06-03 etxivri     Increased timeout in a rpc
%%% R2A/52     2014-09-23 etxivri     Update in escalate_to_revert result in 
%%%                                   get networkloader prompt.
%%% R2A/53     2014-09-23 etxivri     More update to handle sim in escalate_to_revert.
%%% R2A/54     2014-09-26 etxivri     Create new group to be runed after upgrade
%%% R3A/1      2014-10-10 etxivri     Update that kill beam will trig fallback on ARM.
%%% R3A/2      2014-10-17 etxivri     Fix a bug when running on sim.
%%% R3A/3      2014-10-26 etxivri     Removed escalate_to_revert to be runed du to stuck in nl
%%%                                   needs new install, and update kill_beam not to escalate_to_revert.
%%% R3A/4      2014-11-14 etxarnu     Changed 'pgrep com' to 'pgrep -f com/bin/com' to avoid false pgreps
%%% R3A/5      2014-11-30 etxjotj     Use SwInventory to find active version
%%% R3A/7      2014-12-05 etxivri     Try to make the check for login prompt more robust.
%%% R3A/8      2015-01-12 etxivri     fix due to hw test could take more then 5min.
%%%                                   Update check when node restart.
%%% R3A/9      2015-01-20 etxmlar     Added check for cold_with_test restart.
%%% R3A/10     2015-01-30 etxivri     get_sw_version more robust. 
%%% R3A/10     2015-02-18 etxivri     make it more robust. 
%%%                                   Moved cold_with_test to be runed in group_2
%%% R3A/13     2015-04-30 etxivri     Update due to first kill beam results in
%%%                                   node restart.
%%% R3A/14     2015-05-04 etxivri     Update search string when node restarts
%%% R3A/15     2015-05-05 etxivri     More trie to remove error in ct shell.
%%% R3A/16     2015-05-05 etxivri     Cleanup.
%%% R4A/2      2015-07-02 etxarnu     Added wait_for_com_started in escalate_to_cold*
%%% R4A/3      2015-07-02 etxarnu     Added sleep 30 sec after wait_for_com_started
%%% R4A/4      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/6      2015-09-28 etxivri     Update escalate_to_warm.
%%% R4A/7      2015-10-23 etxivri     Remove use of init_restart.
%%%                                   Update to handle warm restart is dafult
%%%                                   enabled on TCU. 
%%% R4A/8      2015-10-23 etxivri     Bug fix
%%% R4A/10     2015-10-23 etxivri     Increased sleep befor kill appl.
%%% R4A/11     2015-11-17 etxivri     Make it more robust.
%%% R4A/12     2015-11-18 etxivri     Warm restart is now enabled for dus also.
%%% R5A/1      2015-12-01 etxivri     Update check for warm restart in llog.
%%% R5A/2      2016-02-23 ekurnik     Added new escalate_to_revert use cases.
%%% R5A/3      2016-02-25 ekurnik     escalate_to_revert TCs added to new group
%%% R5A/4      2016-04-11 ekurnik     Fixed timing issue when polling alarm
%%% R6A/1      2016-05-03 etxarnu     Added check_boot_count
%%% R6A/2      2016-05-11 etxnnor     Retries when checking avli log to avoid intermittent problem
%%% R6A/3      2016-06-01 etxivri     A try to make it more robust.
%%% R6A/4      2016-07-11 etxivri     Update check fro RankWarm in avli log.
%%% R7A/1      2016-09-02 etxarnu     don't use more in os:cmd
%%% R7A/2      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% R7A/3      2016-09-20 etxivri     Update to get more info if TC fail.
%%% R8A/1      2016-11-29 etxivri     Increased timer in rpc call
%%% R8A/2      2017-03-02 etxarnu     enable pgroup restart for suite
%%% R9A/2      2017-03-21 etxivri     Update expected from fallback list due to new behaviour.
%%%                                   Update  escalate_to_revert_no_backup due to new behaviour.
%%% R9A/3      2017-04-02 etxarnu     No restart pgroup tests for 5G nodes (BPU & vrcs)
%%% R9A/4      2017-04-04 etxarnu     No escalate_to_warm test for 5G nodes
%%% R9A/5      2017-07-11 erarube     More robust test case at escalation to warm,
%%%                                   wait for coli communication back.
%%% R11A/1     2017-09-22 etxivri     Update in restore_backup to use swmI.
%%% R12A/1     2017-11-25 etxarnu     Use comsaI:has_consul()
%%%
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 wait_for_appl_started/1,
	 wait_for_com_started/0,
	 wait_for_netconf_started/0,
	 get_sw_version/0,
	 groups/0,
	 all/0,
	 kill_prog_contr_by_appm/1,
	 kill_prog_contr_by_appm_random/1,
	 %% check_appl_restarts_4times_within_5min/1, %% Obsolete
	 check_restart_counter_reset_after_5min/1,
	 %% check_node_restart_after_4_sigabrt/1,
	 kill_beam/1,
	 restart_pgroup/1,
	 escalate_to_pgroup/1,
	 escalate_to_warm/1,
	 escalate_to_cold/1,
	 escalate_to_cold_with_test/1,
	 %% escalate_to_revert/1
	 escalate_to_revert_login/1,
	 escalate_to_revert_latest/1,
	 escalate_to_revert_multiple_backups/1,
 	 escalate_to_revert_no_backup/1,
 	 check_boot_count/1]).

-define(TEST_APP, "test_app").
-define(TEST_OI, "test_oi").
-define(IFT_APP, "ift_app").
-define(TEST_APPS,[?TEST_APP,?TEST_OI,?IFT_APP]).

-define(PGROUP, "mppg1").
-define(APP1, "pg_app1").
-define(APP2, "pg_app2").
-define(APP3, "pg_app3").

-define(APP4, "restart").

-define(ROLLBACK_ESCALATION_STARTED, 'RollbackEscalationStarted').
-define(SYSTEM_GENERAL_PROBLEM, 'SystemGeneralProblem').

-define(BACKUP_LATEST, "backup_latest").
-define(BACKUP_AFTER_UPGRADE, "backup_after_upgrade").
-define(BACKUP_BEFORE_UPGRADE, "backup_before_upgrade").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_power,node},
		 {rct_netconf, nc1},
		 {cth_conn_log, []},
		 {rct_logging, {all,
				[{erlang,
				  {["ERROR REPORT","CRASH REPORT"],
				   ["\"test_app\" died with Exit code 137",
				    "\"test_oi\" died with Exit code 137",
				    "\"ift_app\" died with Exit code 137",
				    "Program ID [0-9]+ has crashed",
				    "Program ID [0-9]+ has terminated",
				    "has failed to start after 3 attempts within 300 seconds"]}
				 }]}},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_coli, {coli, [manual_connect]}},
                 {rct_core,[]}
		]}].


%% @hidden
init_per_suite(Config) ->
    %%%%
    %% Reset restart list in APPM.
    %%%%
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    rct_rpc:call(rpc, appmServer, enable_pg_restart, [0], 10000),
    case os:getenv("SIM_OR_TARGET") of
    	"sim" ->
	    Config;
    	"target" ->
	    BoardType = proplists:get_value(
			  board_type,ct:get_config(
				       ct:get_config({test_nodes,1}))),
	    ct:pal("BoardType: ~p", [BoardType]),
	    [{boardtype, BoardType}| Config]
    end.
%% @hidden
end_per_suite(_Config) ->
    %% Reset restart list in APPM.
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    rct_rpc:call(rpc, appmServer, disable_pg_restart, [0], 10000),
    %% BoardType = proplists:get_value(boardtype, Config),
    %% case BoardType of
    %% 	BoardType when BoardType == "tcu03";
    %% 		       BoardType == "tcu0401" ->
    %% 	    ct:pal("Do not disable warm_restart on TCU.");
    %% 	_Other ->
    %% 	    ct:pal("Disable warm restart"),
    %% 	    rct_rpc:call(rpc, appmServer, disable_warm_restart, [0], 10000)
    %% end,
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% init_per_testcase(escalate_to_warm, Config) ->
%%     ct:pal("TC: escalate_to_warm"),
%%     ct:pal("TC: escalate_to_warm, reset restart list"),
%%     rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    %% BoardType = proplists:get_value(boardtype, Config),
    %% case BoardType of
    %% 	BoardType when BoardType == "tcu03";
    %% 		       BoardType == "tcu0401" ->
    %% 	    ct:pal("Do not enable warm_restart on TCU.");
    %% 	_Other ->
    %% 	    ct:pal("Enable warm restart"),
    %% 	    rct_rpc:call(rpc, appmServer, enable_warm_restart, [0], 10000)
    %% end,
    %% Config;

init_per_testcase(escalate_to_revert, Config) ->
    ct:pal("TC: escalate_to_revert"),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    %% Set rollback time to 60 sec, default is 3600 sec
    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [60], 10000),
    Config;

init_per_testcase(escalate_to_revert_login, Config) ->
    ct:pal("TC: escalate_to_revert_login"),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    ok = create_fallback_backups([{?BACKUP_LATEST, latest}]),
    Config;

init_per_testcase(escalate_to_revert_latest, Config) ->
    ct:pal("TC: escalate_to_revert_latest"),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    %% Set rollback time to 60 sec, default is 3600 sec
    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [60], 10000),
    ok = create_fallback_backups([{?BACKUP_LATEST, latest}]),
    Config;

init_per_testcase(escalate_to_revert_multiple_backups, Config) ->
    ct:pal("TC: escalate_to_revert_multiple_backups"),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    %% Set rollback time to 60 sec, default is 3600 sec
    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [60], 10000),
    ok = create_fallback_backups([{?BACKUP_BEFORE_UPGRADE, before_ug},
				  {?BACKUP_AFTER_UPGRADE, after_ug},
				  {?BACKUP_LATEST, latest}]),
    Config;

init_per_testcase(escalate_to_revert_no_backup, Config) ->
    ct:pal("TC: escalate_to_revert_no_backup"),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    %% Set rollback time to 60 sec, default is 3600 sec
    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [60], 10000),
    %% Due to new behaviour create new bu to use short rollback time to escalate to NL.
    %% Otherwise default time 3600 sec will be used.
    ok = create_fallback_backups([{?BACKUP_LATEST, latest}]),
    Config;

init_per_testcase(check_boot_count, Config) ->
    ct:pal("TC: check_boot_count"),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    %% Set rollback time to 60 sec, default is 3600 sec
    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [60], 10000),
    Config;

init_per_testcase(TestCase, Config) ->
    ct:pal("TC: ~p", [TestCase]),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    Config.

end_per_testcase(escalate_to_revert, Config) ->
    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [3600], 10000),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n"
    	    	   "If networkloader prompt, then try to start node. !", 
		   [Reason]),
	    if_failed_fallback_tc()
    end;

end_per_testcase(escalate_to_revert_login, _Config) ->
	ok = delete_fallback_backups([?BACKUP_LATEST]);

end_per_testcase(escalate_to_revert_latest, _Config) ->
	ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [3600], 10000),
	ok = delete_fallback_backups([?BACKUP_LATEST]);

end_per_testcase(escalate_to_revert_multiple_backups, _Config) ->
	ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [3600], 10000),
	ok = delete_fallback_backups([?BACKUP_LATEST, ?BACKUP_AFTER_UPGRADE, ?BACKUP_BEFORE_UPGRADE]);

end_per_testcase(escalate_to_revert_no_backup, _Config) ->
	ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [3600], 10000);

%% end_per_testcase(escalate_to_warm, Config) ->
%%     ct:pal("TC: escalate_to_warm, END"),
%%     BoardType = proplists:get_value(boardtype, Config),
%%     case BoardType of
%% 	BoardType when BoardType == "tcu03";
%% 		       BoardType == "tcu0401" ->
%% 	    ct:pal("Do not disable warm_restart on TCU.");
%% 	_Other ->
%% 	    ct:pal("Disable warm restart"),
%% 	    rct_rpc:call(rpc, appmServer, disable_warm_restart, [0], 10000)
%%     end;
%%     %% rct_rpc:call(rpc, appmServer, disable_warm_restart, [0], 10000);


end_per_testcase(_TestCase, _Config) ->
    ok.

if_failed_fallback_tc() ->
    timer:sleep(5000),
    ct:pal("Startup node again."),
    ok = ct_telnet:send(console, "sysreboot config"),
    
    ct:pal("Wait for login prompt."),
    {ok,_} = 
	ct_telnet:expect(console, "login:", 
			 [{timeout,180000}, no_prompt_check]),
    wait_for_netconf_started(),
    get_sw_version().

create_fallback_backups([]) ->
	ok;
create_fallback_backups([{Backup, FallbackType} | OtherBackups]) when is_list(Backup), is_atom(FallbackType) ->
	ok = create_backup(Backup),
	ok = rct_rpc:call(rpc, swmFallbackList, add_backup, [FallbackType, Backup], 20000),
	create_fallback_backups(OtherBackups).

delete_fallback_backups([]) ->
	%% refresh list
	ok = rct_rpc:call(rpc, swmFallbackList, audit_fallback_list, [], 30000);
delete_fallback_backups([Backup | OtherBackups]) when is_list(Backup) ->
	ok = rct_rpc:call(rpc, swmBackup, delete_system_created_backup, [Backup, undefined], 30000),
	delete_fallback_backups(OtherBackups).
	

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [	 
 	 kill_prog_contr_by_appm,
 	 kill_prog_contr_by_appm_random,
 	 %% check_appl_restarts_4times_within_5min,
 	 check_restart_counter_reset_after_5min,
 	 %% check_node_restart_after_4_sigabrt,
 	 check_boot_count,
 	 kill_beam,
 	 restart_pgroup,
 	 escalate_to_pgroup,
 	 %% escalate_to_warm,
 	 escalate_to_cold,
 	 escalate_to_cold_with_test,
 	 escalate_to_warm,
 	 %% escalate_to_revert
	 escalate_to_revert_login,
	 escalate_to_revert_latest,
	 escalate_to_revert_multiple_backups,
	 escalate_to_revert_no_backup
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {group_rob_restartable_prog_1, [], [{group, group_rob_restartable_prog_1_1}]},
     {group_rob_restartable_prog_1_1,[],[kill_prog_contr_by_appm,
					 kill_prog_contr_by_appm_random,
					 restart_pgroup,
					 escalate_to_pgroup,
					 %% escalate_to_warm,
					 escalate_to_cold,
					 escalate_to_warm
					 %% escalate_to_cold_with_test
					 %% escalate_to_revert
				      ]},
     {group_rob_restartable_prog_2, [], [{group, group_rob_restartable_prog_2_1}]},
     {group_rob_restartable_prog_2_1,[],[%% kill_beam
				       escalate_to_cold_with_test
				      ]},
     {group_rob_restartable_prog_ug, [], [{group, group_rob_restartable_prog_ug_1}]},
     {group_rob_restartable_prog_ug_1,[],[kill_prog_contr_by_appm,
					  kill_prog_contr_by_appm_random,
					  kill_beam, %% Ok to run now, due to will not result in revert to nl.
					  restart_pgroup,
					  escalate_to_pgroup,
					  %% escalate_to_warm,
					  escalate_to_cold,
					  escalate_to_cold_with_test,
					  escalate_to_warm
				      ]},
     {group_rob_restartable_prog_sim2,[],[%kill_prog_contr_by_appm
					  %kill_prog_contr_by_appm_random,
					 ]},
     {group_rob_restartable_prog_xl_rob,[],[%% check_appl_restarts_4times_within_5min,
					    check_restart_counter_reset_after_5min
					   ]},
     {group_rob_restartable_prog_sim,[],[kill_prog_contr_by_appm,
					 kill_prog_contr_by_appm_random,
					 restart_pgroup,
					 escalate_to_pgroup
					 %%escalate_to_warm
					 %% escalate_to_cold,
					 %% escalate_to_cold_with_test
					 %% escalate_to_revert
					]},
     {group_rob_restartable_prog_escalate_to_revert,[], [
							 check_boot_count,
							 escalate_to_revert_login,
							 escalate_to_revert_latest,
							 escalate_to_revert_multiple_backups,
							 escalate_to_revert_no_backup
							]},
     {sbc__qual__dus__1__group, [], []},
     {sbc__qual__tcu__1__group, [], []},
     {sbc__def__dus__1__group, [], [{group, group_rob_restartable_prog_1_1}]},
     {sbc__def__dus__2__group, [], [{group, group_rob_restartable_prog_2_1}]},
     {sbc__def__tcu__1__group, [], [{group, group_rob_restartable_prog_1_1}]},
     {sbc__def__tcu__2__group, [], [{group, group_rob_restartable_prog_2_1}]},
     {sbc__upgrade__dus__1__group, [], [{group, group_rob_restartable_prog_ug_1}]},
     {sbc__upgrade__tcu__1__group, [], [{group, group_rob_restartable_prog_ug_1}]},
     {sbc__upgrade_short__dus__1__group, [], [{group, group_rob_restartable_prog_ug_1}]},
     {sbc__upgrade_short__tcu__1__group, [], [{group, group_rob_restartable_prog_ug_1}]},
     {sdc__def__dus__1__group, [], [{group, group_rob_restartable_prog_1_1}]},
     {sdc__def__dus__2__group, [], [{group, group_rob_restartable_prog_2_1}]},
     {sdc__def__tcu__1__group, [], [{group, group_rob_restartable_prog_1_1}]},
     {sdc__def__tcu__2__group, [], [{group, group_rob_restartable_prog_2_1}]},
     {sdc__qual__dus__1__group, [], []},
     {sdc__qual__tcu__1__group, [], []}
     
    ].

%%--------------------------------------------------------------------
%% @doc
%% Kill programs controlled by appm .<br/>
%% @spec kill_prog_contr_by_appm(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
kill_prog_contr_by_appm(_Config) ->
    ct:pal("Kill programs contolled by appm, and make sure they "
	   "start up again."),

    %%%%
    %% Get all restartable programs controlled by Appm.
    %%%%
    ApplPidList = get_appl_pids_controlled_by_appm(),
    ct:pal("ApplPidList: ~p",[ApplPidList]),

    %%%%
    %% Kill the applications processes
    %%%%
    ct:pal("Kill Appl. ",[]),
    kill_pid(ApplPidList),

    %%%%
    %% Check new processes is created.
    %%%%
    NewApplPidList = get_appl_pids_controlled_by_appm(),
    ct:pal("NewApplPidList: ~p",[NewApplPidList]),

    %%%%
    %% Check
    %%%%
    check_new_pids_created(ApplPidList, NewApplPidList),

    ok.



%%--------------------------------------------------------------------
%% @doc
%% Kill programs controlled by appm, in random order .<br/>
%% @spec kill_prog_contr_by_appm_random(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
kill_prog_contr_by_appm_random(_Config) ->
    ct:pal("Kill programs controlled by appms, in random order. "
	   "Make sure they start up again."),

    %%%%
    %% Get all restartable programs controlled by Appm.
    %%%%
    ApplPidList = get_appl_pids_controlled_by_appm(),
    ct:pal("ApplPidList: ~p",[ApplPidList]),

    %% Make a random list of the elements in the Pid List.
    RandomApplPidList = lists:sort(fun(_X,_Y) -> % Compares two element.
    				       %% random:uniform()>0.5 %% return true or false
					   rand:uniform()>0.5 %% return true or false

    			       end, ApplPidList),
    ct:pal("RandomPidList: ~p", [RandomApplPidList]),

    %%%%
    %% Kill the applications processes
    %%%%
    ct:pal("Kill Appl. ",[]),
    kill_pid(RandomApplPidList),

    %%%%
    %% Check new processe is created.
    %%%%
    NewApplPidList = get_appl_pids_controlled_by_appm(),
    ct:pal("NewApplPidList: ~p",[NewApplPidList]),

    %%%%
    %% Check
    %%%%
    check_new_pids_created(ApplPidList, NewApplPidList),

    ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% - Kill appl controlled by appm 3 times. within 5 min. Using SIGKILL<br/>
%% %% - Kill applications controlled by appm once again.
%% %% - Check that applications does not exist in appm due to many restart within 5 min.
%% %% @spec check_appl_restarts_4times_within_5min(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% check_appl_restarts_4times_within_5min(_Config)->
%%     ct:pal("Kill programs contolled by appm 4 times, check application does not exist in Appm. within 5min."),

%%     %%%%
%%     %% Precondition. Reset restart list in APPM.
%%     %%%%
%%     ok = rct_rpc:call(rpc, appmServer, reset_restart_list, [], 1000),

%%     NrOfRestarts = lists:seq(1,3),
%%     ct:pal("Kill applications 3 times.",[]),
%%     _ListOfTestAppPids = lists:map(fun(Nr)->
%%                                           %%%%
%% 					  %% Get all restartable programs controlled by Appm.
%%                                           %%%%
%% 					  ApplPidList = get_appl_pids_controlled_by_appm(),
%% 					  ct:pal("ApplPidList: ~p",[ApplPidList]),

%%                                           %%%%
%% 					  %% Kill the applications processes
%%                                           %%%%
%% 					  ct:pal("Kill Appl. Nr: ~p ",[Nr]),
%% 					  kill_pid(ApplPidList),

%%                                           %%%%
%% 					  %% Check new processes is created.
%%                                           %%%%
%% 					  NewApplPidList = get_appl_pids_controlled_by_appm(),
%% 					  ct:pal("NewApplPidList: ~p",[NewApplPidList]),

%%                                           %%%%
%% 					  %% Check
%%                                           %%%%
%% 					  check_new_pids_created(ApplPidList, NewApplPidList)
%% 				  end, NrOfRestarts),

%%     %%%%
%%     %% Get all restartable programs controlled by Appm.
%%     %%%%
%%     ApplPid_List = get_appl_pids_controlled_by_appm(),
%%     ct:pal("ApplPid_List: ~p",[ApplPid_List]),
    
%%     %%%%
%%     %% Kill the applications processes
%%     %%%%
%%     ct:pal("Kill Appl. 4:th time.  ",[]),
%%     kill_pid(ApplPid_List),

%%     %%%%
%%     %% Application shall not exist in appm
%%     %%%%
%%     ok = wait_for_appl_not_exist_in_appm(?TEST_APPS),
    
%%     %%%%
%%     %% Perform a initial restart.
%%     %%%%
%%     rct_rpc:call(rpc, init, restart, [], 1000),

%%     %%%%
%%     %% Check that com is restarted with a new pid.
%%     %%%%
%%     ComPid = wait_for_com_started(),
%%     ct:pal("NewComPid: ~p",[ComPid]),

%%     %%%%
%%     %% Application shall exist again.
%%     %%%%
%%     NewApplPid_List = get_appl_pids_controlled_by_appm(),
%%     ct:pal("NewApplPid_List: ~p",[NewApplPid_List]),
%%     ok.


%%--------------------------------------------------------------------
%% @doc
%% - Kill test_app controlled by appm 3 times. Using SIGKILL<br/>
%% - Wait 5 min.
%% - Kill applications controlled by appm once again.
%% - Check that applications restarts.
%% @spec check_restart_counter_reset_after_5min(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
check_restart_counter_reset_after_5min(_Config) ->
    ct:pal("Kill programs contolled by appm 2 times, check that "
	   "restart counter is reset after 5min."),

    %%%%
    %% Precondition. Reset restart list in APPM.
    %%%%
    ok = rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),

    NrOfRestarts = lists:seq(1,2),
    ct:pal("Kill applications 2 times.",[]),
    ListOfTestAppPids =
	lists:map(
	  fun(Nr)->
		  %%
		  %% Get all restartable programs controlled by Appm.
		  %%
		  ApplPidPropList = get_apps(),
		  ct:log("ApplPidPropList: ~p",[ApplPidPropList]),
		  TestAppPid = proplists:get_value(?TEST_APP, ApplPidPropList),
		  %%
		  %% Kill the applications processes
		  %%
		  ct:pal("Kill Test_App: ~p , nr: ~p. ",[TestAppPid, Nr]),
		  rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ TestAppPid], 10000),
		  %%
		  %% Check new processes is created.
		  %%
		  NewTestAppPid = wait_for_app_restarted(?TEST_APP, TestAppPid),
		  ct:pal("NewTestAppPid: ~p . ",[NewTestAppPid]),
		  NewTestAppPid
		  %%
		  %% Check
		  %%
	  end, NrOfRestarts),
    timer:sleep(5000),
    ct:pal("Sleep 5 min.",[]),
    ct:sleep({minutes, 5}),

    %%
    %% Get latest started test_app.
    %%
    TestApp_Pid =lists:last(ListOfTestAppPids),
    ct:pal("IftApp_Pid: ~p . ",[TestApp_Pid]),
    %%
    %% Kill the applications processes
    %%
    ct:pal("Kill Appl, nr: 3. The restart counter shall be reseted and "
	   "applications shall be restarted.",[]),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ TestApp_Pid], 10000),
    %%
    %% Check new processes is created.
    %%
    NewTestApp_Pid = wait_for_app_restarted(?TEST_APP, TestApp_Pid),
    ct:pal("NewIftAppPid: ~p . ",[NewTestApp_Pid]),
    %%
    %% Cleanup. Reset restart list in APPM.
    %%
    ok = rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% - Kill ift_app controlled by appm 4 times. Using SIGABRT<br/>
%% %% - Node shall be restarted.
%% %% - Check That node starts up again. And application is started.
%% %% @spec check_node_restart_after_4_sigabrt(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% check_node_restart_after_4_sigabrt(_Config) ->
%%     ct:pal("Kill ift_app contolled by appm 4 times, using SIGABRT within 5min. Shall result in node restart."),

%%     %%%%
%%     %% Precondition. Reset restart list in APPM.
%%     %%%%
%%     ok = rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),

%%     NrOfRestarts = lists:seq(1,3),
%%     ct:pal("Kill ift_app 3 times.",[]),
%%     ListOfIftAppPids =
%% 	lists:map(fun(Nr)->
%% 			  %%
%% 			  %% Get all restartable programs controlled by Appm.
%% 			  %%
%% 			  ApplPidPropList = get_apps(),
%% 			  ct:log("ApplPidPropList: ~p",[ApplPidPropList]),
%% 			  IftAppPid = proplists:get_value(?IFT_APP, ApplPidPropList),
%% 			  %%
%% 			  %% Kill the applications processes using ABRT
%% 			  %%
%% 			  ct:pal("Kill Ift_App: ~p , nr: ~p. ",[IftAppPid, Nr]),
%% 			  rct_rpc:call(rpc, os, cmd, ["kill -s ABRT " ++ IftAppPid], 10000),
%% 			  %%
%% 			  %% Check new processes is created.
%% 			  %%
%% 			  NewIftAppPid = wait_for_app_restarted(?IFT_APP, IftAppPid),
%% 			  ct:pal("NewIftAppPid: ~p . ",[NewIftAppPid]),
%% 			  NewIftAppPid
%% 		  end, NrOfRestarts),

%%     %%%%
%%     %% Get latest started ift_app.
%%     %%%%
%%     IftApp_Pid =lists:last(ListOfIftAppPids),
%%     ct:pal("IftApp_Pid: ~p . ",[IftApp_Pid]),

%%     {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
%%     %%%%
%%     %% Kill the applications processes
%%     %%%%
%%     ct:pal("Kill Appl, nr: 4. Node shall be restarted.",[]),
%%     rct_rpc:call(rpc, os, cmd, ["kill -s ABRT " ++ IftApp_Pid], 10000),
%%     net_kernel:disconnect(ErlNode),

%%     %%%%
%%     %% Check node restarts and wait for login prompt.
%%     %%%%
%%     node_restarts()

%%     %%%%
%%     %%  check netconf is started and ok to use
%%     %%%%
%%     wait_for_netconf_started(),

%%     %%%%
%%     %% Check new processes is created.
%%     %%%%  
%%     NewIftApp_Pid = wait_for_app_restarted(?IFT_APP, IftApp_Pid),
%%     ct:pal("NewIftAppPid: ~p . ",[NewIftApp_Pid]),

%%     ok.

%%--------------------------------------------------------------------
%% @doc
%% Kill beam .<br/>
%% @spec kill_beam(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
kill_beam(_Config) ->

    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ct:pal("Kill beam in simulated enviroment."),
	    ct:pal("### TC not implemented for sim enviroment, yet! "
		   "Due to heart does not start not beam again.",[]),
	    ok;
	"target" ->
	    ct:pal("Kill beam on target nonde, and make sure node "
		   "starts up again."),

	    do_kill_beam(),

	    %%
	    %% Get sw version, using cli
	    %%
	    get_sw_version(),

	    ok
    end.


%%% @doc Check that program group mppg1 can be restarted
%%% ===Arguments===
%%% - 
restart_pgroup(_) ->
    case rct_rpc:call(rpc, comsaI,has_consul,[],10000) of
	true ->
	    ct:pal("No pgroup tests in 5G nodes",[]),
	    ok;
	false ->
	    PG=?PGROUP,
	    {PgPids,_} = get_pg_pids(PG),
	    ct:pal("restart_pgroup:PgPids=~p",[PgPids]),
	    rct_rpc:call(rpc, appmServer, stop_group, [PG], 10000),
	    timer:sleep(1000),
	    ok = wait_for_apps_to_stop(PgPids),
	    rct_rpc:call(rpc, appmServer, start_group, [PG], 10000),
	    timer:sleep(1000),
	    {ok,PgPidsNew} = wait_for_apps_to_start(PgPids),
	    [] = check_new_pids(PgPids,PgPidsNew)
    end.

%%% @doc Check that pg_app2 program escalates to pgroup mppg1 when restarted
%%% ===Arguments===
%%% - 
escalate_to_pgroup(_) ->
    case rct_rpc:call(rpc, comsaI,has_consul,[],10000) of
	true ->
	    ct:pal("No pgroup tests in 5G nodes",[]),
	    ok;
	false ->
	    PG=?PGROUP,
	    APP = ?APP2,
	    Apps = get_apps(),

	    {PgPidsOld,_RestOld} = get_pg_pids(PG,Apps),
	    {APP,UnixPid} =  lists:keyfind(APP, 1, Apps),
	    ct:pal("escalate_to_pgroup:First restart of ~p - ~p",[APP,UnixPid]),
	    rct_rpc:call(rpc, os,cmd,["kill -ABRT " ++ UnixPid],10000),
	    timer:sleep(5000),

	    wait_for_app_restarted(APP,UnixPid),
	    NewApps = get_apps(),
	    {PgPidsNew,_} = get_pg_pids(PG,NewApps),
	    [] = check_new_pids(PgPidsOld,PgPidsNew)
    end.
    

%% %%% @doc Check that pg_app2 program escalates to warm when restarted twice
%% %%% ===Arguments===
%% %%% - 
escalate_to_warm(_) ->
    case rct_rpc:call(rpc, comsaI,has_consul,[],10000) of
	true ->
	    ct:pal("No warm (via pgroup) tests in 5G nodes",[]),
	    ok;
	false ->
	    clear_llog(),
	    clear_avli_log(),

	    PG=?PGROUP,
	    APP = ?APP2,
	    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
	    rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),
	    %%
	    %% Get COM pid.
	    %%
	    Com = wait_for_com_started(),
	    %% remove \n from the received list.
	    [ComPid|_] = string:tokens(Com, "\n "),
	    ct:pal("escalate_to_warm: ComPid: ~p",[ComPid]),

	    Apps = get_apps(),
	    FoundApps = [A || {A,_} <-Apps],

	    {PgPidsOld,_RestOld} = get_pg_pids(PG,Apps),
	    {APP,UnixPid} =  lists:keyfind(APP, 1, Apps),
	    ct:pal("escalate_to_warm:First restart of ~p - ~p",[APP,UnixPid]),
	    rct_rpc:call(rpc, os,cmd,["kill -ABRT " ++ UnixPid],10000),
	    timer:sleep(5000),
	    Apps2 = get_apps(),

	    ct:pal("### check llog. Warm shall not exist.",[]),
	    Llog1 = rct_rpc:call(rpc, os,cmd,["llog"],10000),
	    nok = check_programescalate_warm("Warm", Llog1),

	    {APP,UnixPid2} =  lists:keyfind(APP, 1, Apps2),
	    ct:pal("escalate_to_warm:Second restart of ~p -  ~p",[APP,UnixPid2]),
	    rct_rpc:call(rpc, os,cmd,["kill -ABRT " ++ UnixPid2],10000),

%%%%
	    %% Check that com is restarted with a new pid.
%%%%
	    NewComPid = wait_for_com_restarted(ComPid, ErlNode),
	    %% remove \n from the received list.
	    ct:pal("escalate_to_warm: NewComPid: ~p",[NewComPid]),

%%%%
	    %% Check
%%%%
	    case ComPid == NewComPid of
		true ->
		    ct:fail("escalate_to_warm: COM has not restarted.");
		false ->
		    ct:pal("escalate_to_warm: COM has restarted.",[])
	    end,

	    wait_for_appl_started(FoundApps), % wait for all apps to start
	    ct:pal("escalate_to_warm:All applications found after restart ",[]),

	    NewApps = get_apps(),
	    {PgPidsNew,_} = get_pg_pids(PG,NewApps),
	    [] = check_new_pids(PgPidsOld,PgPidsNew),

	    ct:pal("### check llog.",[]),
	    Llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000),
	    rct_rs232:login(console),
	    ct_telnet:cmd(console, "llog"), %% Just for printouts of llog.
	    ok = check_programescalate_warm("Warm", Llog),

	    ok = check_coli_communication_up(30),
	    ct:pal("### check avli log.",[]),
	    %% AvliLog = get_avli_log(5),
	    %% ct:log("### Avli log via coli: ~p", [AvliLog]),
	    %% ok = check_programescalate_warm("RankWarm", AvliLog),
	    ok = check_expected_restart_in_avlilog("RankWarm", 5),

	    ok
    end.

check_coli_communication_up(0) ->
    ct:fail("### coli communication is not up within expected time.");
check_coli_communication_up(NrOfTries) ->
    ct:log("# Check if coli communication is up, number of tries left : ~p ", [NrOfTries]),
    ColiUp = rct_coli:connect(coli),
    ct:log("# ColiUp: ~p", [ColiUp]),
    case ColiUp of
	ok -> 
	    rct_coli:disconnect(coli),
	    ok;
	_  ->
	    ct:log("## coli communication is not up, try check again."),
	    timer:sleep(1000),
	    check_coli_communication_up(NrOfTries - 1)
    end.


clear_llog() -> 
    ct:pal("### clear llog.",[]),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "llog -c"),
    timer:sleep(5000),
    ct_telnet:cmd(console, "llog"),
    ok.

clear_avli_log() ->
    ct:pal("### clear avli log.",[]),
    ok = rct_rpc:call(rpc, alhI, reset_log, [], 10000, print),
    timer:sleep(5000),
    ok.

check_expected_restart_in_avlilog(_SearchStr, 0) ->
    ct:fail("Expected restart cause not exist in avli log.");
check_expected_restart_in_avlilog(SearchStr, NrOfTries) ->
    ct:log("# avlilog, number of checks left : ~p ", [NrOfTries]),
    AvliLog = get_avli_log(),
    ct:log("# AvliLog: ~p", [AvliLog]),
    case re:run(AvliLog, SearchStr) of
	{match, _} -> 
	    ok;
	 nomatch ->
	    ct:log("## restart cause not exist in avlilog, try check again."),
	    timer:sleep(1000),
	    check_expected_restart_in_avlilog(SearchStr, NrOfTries - 1)
    end.

get_avli_log() ->
    ok = rct_coli:connect(coli),
    {ok,_} = rct_coli:send(coli,"/misc/authlevel disabled"),
    {ok, AvliLog} = rct_coli:send(coli,"/log/avli -sl"),
    timer:sleep(1000),
    rct_coli:disconnect(coli),
    ct:log("AvliLog: ~p", [AvliLog]),
    AvliLog.

%% get_avli_log(NoOfChecks) ->
%%     ok = rct_coli:connect(coli),
%%     {ok,_} = rct_coli:send(coli,"/misc/authlevel disabled"),
%%     {ok, AvliLog} = rct_coli:send(coli,"/log/avli -sl"),
%%     timer:sleep(1000),
%%     rct_coli:disconnect(coli),
%%     ct:log("AvliLog: ~p", [AvliLog]),
%%     case re:run(AvliLog, "\r\ncoli [/]-> ") of
%% 	match -> case NoOfChecks == 0 of
%% 		    true ->
%% 			 ok;
%% 		    _ -> timer:sleep(1000),
%% 			 ct:pal("### avli log empty, checking avli log again.",[]),
%% 			 get_avli_log(NoOfChecks - 1)
%% 		end;
%% 	_ -> ok
%%     end,
%%     AvliLog.

check_programescalate_warm(SearchStr, Log) ->
    ct:log("### SearchStr: ~p", [SearchStr]),
    LogA = string:tokens(Log , " \n-"),
    ct:log("### Log: ~p", [LogA]),

    SearchList = lists:dropwhile(fun(X) ->
					 X=/=SearchStr
				 end, LogA),
    ct:log("SearchList log: ~p", [SearchList]),
    case re:run(SearchList, "Warm") of
	{match, _} ->
	    ok;
	nomatch ->
	    nok
    end.
    %% case SearchList of
    %% 	[] ->
    %% 	    nok; %% Warm str not exist in log
    %% 	_Else -> %% check expt str exisyt in log
    %% 	    [SearchStr,"ProgramErrEscalated" | _] = SearchList,
    %% 	    ok
    %% end.


%%% @doc Check that APP4 program escalates to cold when restarted twice
%%% ===Arguments===
%%% - 
escalate_to_cold(_) ->
    rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),

    Apps = get_apps(),
    FoundApps = [A || {A,_} <-Apps],

    kill_and_wait(FoundApps,"first"),

    kill_and_wait(FoundApps,"second"),

    ct:pal("escalate_to_cold:All applications found after restart ",[]),
      
    Com = wait_for_com_started(),
    %% remove \n from the received list.
    [ComPid|_] = string:tokens(Com, "\n "),
    ct:pal("escalate_to_cold: ComPid: ~p",[ComPid]),
    timer:sleep(30000).

%%% @doc Check that APP4 program escalates to cold with test when restarted three times
%%% ===Arguments===
%%% - 
escalate_to_cold_with_test(_) ->
    rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),

    %% Get nr of cold_with_test restarts (hwtest:1) 
    Res = rct_rpc:call(rpc, os, cmd, ["cat /var/log/syslog | grep hwtest:1 | wc -l"], 10000),
    {Nr, _} = string:to_integer(Res),

    Apps = get_apps(),
    FoundApps = [A || {A,_} <-Apps],

    ct:pal("first"),
    kill_and_wait(FoundApps,"first"),

    ct:pal("second"),
    kill_and_wait(FoundApps,"second"),

    ct:pal("third_cwt"),
    kill_and_wait(FoundApps,"third_cwt"),

    ct:pal("wait_for_cold_with_test_restart"),
    NewNr = wait_for_cold_with_test_restart(300000),

    ct:pal("Check"),
    case Nr == NewNr of
	true ->
	    ct:fail("No cold_with_test restart");
	false ->
	    ct:pal("escalate_to_cold_with_test:All applications found after restart ",[])
    end,
    Com = wait_for_com_started(),
    %% remove \n from the received list.
    [ComPid|_] = string:tokens(Com, "\n "),
    ct:pal("escalate_to_cold_with_test: ComPid: ~p",[ComPid]),
    timer:sleep(30000),
    
    ok.
	

%% escalate_to_revert(_) ->
%%      rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),
%% 
%%      Apps = get_apps(),
%%      FoundApps = [A || {A,_} <-Apps],
%% 
%%      kill_and_wait(FoundApps,"first"),
%% 
%%      kill_and_wait(FoundApps,"second"),
%% 
%%      kill_and_wait(FoundApps,"third"),
%% 
%%      kill_and_wait(FoundApps,"fourth"),
%% 
%%      case os:getenv("SIM_OR_TARGET") of
%%  	"sim" ->
%%  	    no_escalate_to_nl(FoundApps);	
%%  	_ ->
%%  	    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
%%  	    ct:pal("Wait for networkloader prompt."),
%%  	    net_kernel:disconnect(ErlNode),
%%  	    {ok,_} = 
%%  		ct_telnet:expect(console, "\\[networkloader\\]", 
%%  				 [{timeout,180000}, no_prompt_check]),
%%  	    timer:sleep(10000),
%%  	    ct:pal("Rcvd networkloader prompt."),
%%  	    timer:sleep(10000),
%%  	    ct:pal("Startup node again."),
%% 		ok = ct_telnet:send(console, "sysreboot config"),
%% 	    
%%  	    ct:pal("Wait for login prompt."),
%%  	    {ok,_} = 
%%  		ct_telnet:expect(console, "login", 
%%  				 [{timeout,300000}, no_prompt_check]),
%%  	    net_kernel:connect(ErlNode),
%% 	    
%%  	    %% check netconf is started and ok to use
%%  	    wait_for_netconf_started()
%%      end,
%% 
%%      ok.

%%% @doc Check that alarm RollbackEscalationStarted is raised after 4 faults.
%%       When the user logs in, rollback timer is stoped, and alarm is changed to SystemGeneralProblem.
%%		 User triggers manual restore and the alarm is cleared.
%%% ===Arguments===
%%% - 
escalate_to_revert_login(Config) ->
	SpecificFun = 
		fun() ->
			%% login to node
			ct:pal("### Logging in to COM CLI",[]),
			ok = rct_cli:connect(cli),
			{ok ,_} = rct_cli:send(cli,"ManagedElement=1"),
			timer:sleep(5000),
			ok = rct_cli:disconnect(cli),
			
            %% Wait for alarm to be raised
			timer:sleep(10000),
	
			%% Check alarms
			false = check_alarm_raised(?ROLLBACK_ESCALATION_STARTED),
			true = check_alarm_raised(?SYSTEM_GENERAL_PROBLEM),
	
			%% Restore backup manually
			ok = restore_backup(?BACKUP_LATEST)
		end,
	escalate_to_revert_generic([{specificFun, SpecificFun} | Config]).

%%% @doc Check that alarm RollbackEscalationStarted is raised after 4 faults.
%%       No user logs in, and after rollback timer expires the latest backup 
%%		 is restored.
%%% ===Arguments===
%%% - 
escalate_to_revert_latest(Config) ->
	SpecificFun = 
		fun() ->
			wait_for_rollback_backup_restore()
		end,
	
	%% precondition
	[?BACKUP_LATEST] = get_restore_escalation_list(),
	
	%%generic revert
	ok = escalate_to_revert_generic([{specificFun, SpecificFun} | Config]),
	
	%% Restore from fallback list successful, therefore the backup is removed
	[?BACKUP_LATEST] = get_restore_escalation_list(),
	
	ok.

%%% @doc Check that alarm RollbackEscalationStarted is raised after 4 faults.
%%       No user logs in, and after rollback timer expires the latest backup 
%%		 is restored. Repeat the procedure until there are no backups
%%% ===Arguments===
%%% - 
escalate_to_revert_multiple_backups(Config) ->
	SpecificFun = 
		fun() ->
			wait_for_rollback_backup_restore()
		end,
	
	%% All possible rollback backups exist in escalation list
	[?BACKUP_LATEST, ?BACKUP_AFTER_UPGRADE, ?BACKUP_BEFORE_UPGRADE] = get_restore_escalation_list(),
	
	ok = escalate_to_revert_generic([{specificFun, SpecificFun} | Config]),
	
	%% Rollback to latest backup
	[?BACKUP_LATEST, ?BACKUP_AFTER_UPGRADE, ?BACKUP_BEFORE_UPGRADE] = get_restore_escalation_list(),
	
	ok = escalate_to_revert_generic([{specificFun, SpecificFun} | Config]),
	
	%% Rollback to backup after upgrade
	[?BACKUP_AFTER_UPGRADE, ?BACKUP_BEFORE_UPGRADE] = get_restore_escalation_list(),
	
	ok = escalate_to_revert_generic([{specificFun, SpecificFun} | Config]),
	
	%% Rollback to backup before upgrade
	[?BACKUP_BEFORE_UPGRADE] = get_restore_escalation_list(),
	
	ok.

%%% @doc Check that alarm RollbackEscalationStarted is raised after 4 faults.
%%       No user logs in, and after rollback timer expires there are no new backups
%%		 to restore. The autointegration is triggered and the node becomes operational
%%% ===Arguments===
%%% - 
escalate_to_revert_no_backup(Config) ->
    ct:pal("First escalation will be to fallback backup."),
    SpecificFun = 
	fun() ->
		wait_for_rollback_backup_restore()
	end,
    %% Note! there will always be an fallback backup.
    %%       If it tries to escalate to this again then it will go to NL.
    [EscDefaultBackup] = get_restore_escalation_list(),
    ct:log("#### EscDefaultBackup : ~p", [EscDefaultBackup]),
    ct:pal("First escalation will be to fallback backup : ~p", [EscDefaultBackup]),

    ok = escalate_to_revert_generic([{specificFun, SpecificFun} | Config]),


    ct:pal("Next escalation will be to NL due to first escallation to fallback did not help."),
    SpecificFun2 = 
	fun() ->				
		ct:pal("# # # Waiting for rollback timer to expire + node reboot",[]),
		%% timer:sleep(90000),
		
		{ok, ErlNode} = rct_rpc:get_erlnode(rpc),
		ct:pal("Wait for networkloader prompt."),
		net_kernel:disconnect(ErlNode),
		{ok,_} =  ct_telnet:expect(console, "Ericsson Version: ",
			      [{timeout,300000}, no_prompt_check]), 
		{ok,_} = ct_telnet:expect(console, "Network Loader Ready:", 
					  [{timeout,180000}, no_prompt_check]),
		%% ct_telnet:expect(console, "\\[networkloader\\]", 
		%% 		     [{timeout,180000}, no_prompt_check]),
		timer:sleep(30000),
		
		ct:pal("Starting autointegration", []),
		aic_httpc:download_files(Config, console),
		aic_httpc:integrate(Config, console, nc1),
		timer:sleep(60000)
	end,

    %% [] = get_restore_escalation_list(),
    [EscDefaultBackup2] = get_restore_escalation_list(),
    ct:log("#### EscDefaultBackup2 : ~p", [EscDefaultBackup2]),
    ok = escalate_to_revert_generic([{specificFun, SpecificFun2} | Config]).
	
	
%%% @doc Generic revert function. 
%%% ===Arguments===
%%% SpecificFun - given by a specific TC. It will execute part of the code after the 
%%% 		      RollbackEscalationStarted alarm is raised. After the execution of
%%%				  SpecificFun it is expected that the all apps are up and running and
%%%				  no alarms are raised (netiher RollbackEscalationStarted nor SystemGeneralProblem).
escalate_to_revert_generic([{specificFun, SpecificFun} | _Config]) ->
	rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),

	Apps = get_apps(),
	FoundApps = [A || {A,_} <-Apps],
	
	kill_and_wait(FoundApps,"first"),
	
	kill_and_wait(FoundApps,"second"),
	
	%% kill_and_wait(FoundApps,"third"),
    kill_and_wait(FoundApps,"third_cwt"),
    
	kill_and_wait(FoundApps,"fourth"),
    
    %% Wait for alarm to be raised
    timer:sleep(10000),
	
	%% Check alarms
	true = check_alarm_raised(?ROLLBACK_ESCALATION_STARTED),
	false = check_alarm_raised(?SYSTEM_GENERAL_PROBLEM),

	%% specific part of each TC
	ct:pal("### Executing specific function for this TC",[]),
	SpecificFun(),
	
	%% Wait for apps to start
	{ok, ErlNode} = rct_rpc:get_erlnode(rpc),
	wait_for_appl_started(FoundApps, ErlNode, 180000),

	%% Check alarms
	false = check_alarm_raised(?ROLLBACK_ESCALATION_STARTED),
	false = check_alarm_raised(?SYSTEM_GENERAL_PROBLEM),

	ok.
		

%% no_escalate_to_nl(FoundApps) ->
%%     NewApps = get_apps(),
%%     NewFoundApps = [B || {B,_} <- NewApps],
%%     case FoundApps -- NewFoundApps of
%% 	[?APP4] ->
%% 	    ct:pal("escalate_to_revert:killed application not restarted at start of revert ",[]);
%% 	Rest->
%% 	    ct:fail("escalate_to_revert:Appl not killed!, ~p ",[Rest])
%%     end,
%%     rct_rpc:call(rpc, appmServer, activate, [], 10000),
%%     wait_for_appl_started(FoundApps).


kill_and_wait(FoundApps,T) ->
    %% BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),

    %% BoardType = proplists:get_value(
    %% 			  board_type,ct:get_config(
    %% 				       ct:get_config({test_nodes,1}))),
    %% 	    ct:pal("BoardType: ~p", [BoardType]),
    timer:sleep(60000),
    Apps = get_apps(),
    {?APP4,UnixPid} =  lists:keyfind(?APP4, 1, Apps),
    ct:pal("Sleep a while before killing app ",[]),
    %% timer:sleep(60000),
    ct:pal("Killing ~p ~p time",[UnixPid,T]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    rct_rpc:call(rpc, os,cmd,["kill  " ++ UnixPid],10000),
    net_kernel:disconnect(ErlNode),
    case T of
	%% "first" -> %only warm restart
	%%     wait_for_appl_started(FoundApps); % wait for all apps to start

	"first" -> %only warm restart
	    wait_for_appl_started(FoundApps, ErlNode, 300000);
	"third_cwt" -> 
	    check_cwt_results_in_two_restarts(),
	    net_kernel:disconnect(ErlNode),
	    %% wait_for_new_beam_pid(BeamPid, ErlNode), %% Not needed
	    wait_for_appl_started(FoundApps, ErlNode, 600000);
	"fourth" -> %revert started
	    timer:sleep(5000);
	_ -> 
	    node_restarts(),
	    %% %% wait_for_new_beam_pid(BeamPid, ErlNode),  %% Not needed
	    wait_for_appl_started(FoundApps, ErlNode, 300000)
    end.

check_boot_count(_Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    %% do manual cold restart to reset boot_counter
    ct:pal("Doing appmI:restart_piu_cold to clear boot counter",[]),
    rct_rpc:call(rpc, appmI, restart_piu_cold, ["Cold restart from TC"], 10000),

    net_kernel:disconnect(ErlNode),

    %%
    %% Check node restarts and wait for login prompt.
    %%
    node_restarts(),
	   
    %%
    %% Check new beam pid is created.
    %%
    NewBeamPid = wait_for_beam_pid(ErlNode),
    ct:pal("### NewBeam ~p",[NewBeamPid]),
    
    %%
    %%  check netconf is started and ok to use
    %%
    wait_for_netconf_started(),

    ct:pal("Checking that boot counter is 1",[]),
    1 = rct_rpc:call(rpc, appmServer, get_boot_count, [], 10000),
 
    ct:pal("stopping applications to not get pmds when killing beam",[]),
    rct_rpc:call(rpc, appmServer, stop_lms, [], 10000),

    %% kill beam , this will cause an Unexpected restart 
    %% and the boot counter is incremented
    ct:pal("Killing beam unexpectedly",[]),
    do_kill_beam(),

    ct:pal("Checking that boot counter is 2",[]),
    2 = rct_rpc:call(rpc, appmServer, get_boot_count, [], 10000),
	

    %% wait 11 minutes
    ct:pal("### waiting 11 minutes",[]),
    timer:sleep(11*60*1000),

    ct:pal("stopping applications to not get pmds when killing beam",[]),
    rct_rpc:call(rpc, appmServer, stop_lms, [], 10000),

    %% kill beam , this will cause an Unexpected restart 
    %% but the boot counter is not incremented
    ct:pal("Killing beam unexpectedly after 11 minutes",[]),
    do_kill_beam(),
   
    ct:pal("Checking that boot counter is now 1",[]),
    1 = rct_rpc:call(rpc, appmServer, get_boot_count, [], 10000),

    ok.  

do_kill_beam() ->
    %%
    %% Get beam Pid.
    %%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    ct:pal("BeamPid: ~p",[BeamPid]),

    %%
    %% Kill the beam process
    %%

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    ct:pal("Kill beam. ",[]),
    ok = rct_rs232:login(console),
    ok = ct_telnet:send(console, "kill -9 " ++ BeamPid),
    %%
    %% Check node restarts and wait for login prompt.
    %%
    node_restarts(),

    %%
    %% Check new beam pid is created.
    %%
    NewBeamPid = wait_for_beam_pid(ErlNode),
    ct:pal("### NewBeam ~p",[NewBeamPid]),

    %%
    %%  check netconf is started and ok to use
    %%
    wait_for_netconf_started().
    

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
get_apps() ->
    get_apps(60).
get_apps(Cnt) ->
    Apps = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    case {Cnt, catch lists:keyfind(undefined,2,Apps)} of
	{_,false} when is_list(Apps) andalso length(Apps) > 0 ->
	    Apps;
	{0,UndefApps} ->
	    ct:fail("Apps still undefined: ~p, giving up",[UndefApps]),
	    Apps;
	{Cnt,UndefApps} ->
	    ct:pal("Apps still undefined: ~p, wait a while",[UndefApps]),
	    timer:sleep(1000),
	    get_apps(Cnt-1)
    end.

node_restarts() ->
    {ok,_} =  ct_telnet:expect(console, "Ericsson Version: ",
			      [{timeout,180000}, no_prompt_check]), 
    case  ct_telnet:expect(console, "login",
			   [{timeout,120000}, no_prompt_check]) of
	{ok,_} ->
	    ok;
	_Other ->
	    ct_telnet:send(console, ""),
	    {ok,_} = ct_telnet:expect(console, "login",
				      [{timeout,30000}, no_prompt_check])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get pids of programs controlled by appm. <br/>
%% @spec get_appl_pids_controlled_by_appm() -> ApplPidList
%% @end
%%--------------------------------------------------------------------
get_appl_pids_controlled_by_appm()->
    ApplPidList = case wait_for_appl_started(?TEST_APPS) of
		      {ok,Pids} ->
			  Pids;
		      _ ->
			  ct:fail("Appl not started!")
		  end,
    ApplPidList.

%%--------------------------------------------------------------------
%% @doc
%% Kill -9 on all pids in list. SIGKILL. <br/>
%% @spec kill_pid(PidList) -> ok
%% @end
%%--------------------------------------------------------------------
kill_pid(PidList) ->
    lists:foreach(fun(Pid) ->
			  ct:pal("Kill Pid: ~p",[Pid]),
    			  rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ Pid], 10000)
    		  end, PidList),
	ok.

%%--------------------------------------------------------------------
%% @doc
%% Wait for a specific apllication to be restarted. <br/>
%% @spec wait_for_app_restarted(AppName, PidList) -> ok
%% @end
%%--------------------------------------------------------------------
wait_for_app_restarted(AppName, AppPid) ->
    wait_for_app_restarted(AppName, AppPid, 90000).
wait_for_app_restarted(_AppName, _AppPid, Timeout) when Timeout < 500 ->
    ct:fail("App has not restarted within expected time!!");
wait_for_app_restarted(AppName, AppPid, Timeout) -> 
    case rct_rpc:call(rpc, appmServer, get_apps, [], 1000) of
	{badrpc,timeout} ->
	    ct:pal("{badrpc,timeout}, wait and try again", []),
	    timer:sleep(5000),
	    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
	NewApplPidPropList ->
	    ct:log("NewApplPidPropList: ~p",[NewApplPidPropList]),
	    ct:log("AppName: ~p",[AppName]),
	    case proplists:get_value(AppName, NewApplPidPropList) of
		undefined ->
		    ct:pal("~p has not started, wait and try again", [AppName]),
		    timer:sleep(5000),
		    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
		{badrpc,timeout} ->
		    ct:pal("{badrpc,timeout}, wait and try again", [AppName]),
		    timer:sleep(5000),
		    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
		NewAppPid ->
		    case NewAppPid of
			AppPid ->
			    ct:pal("ift_app has not restarted, wait and try again", []),
			    timer:sleep(5000),
			    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
			_Other ->
			    NewAppPid
		    end
	    end
    end.
	    
%% ===========================================================================
%% @doc
%% Check for Application to be started. <br/>
%% @spec wait_for_appl_started(Apps) -> {ok,Pids} | {error,Reason}
%% @end
%% ===========================================================================
wait_for_appl_started(Apps) ->
    wait_for_appl_started(Apps, dummy).
wait_for_appl_started(Apps, ErlNode) ->
    wait_for_appl_started(Apps, ErlNode, 120000).

wait_for_appl_started(_, _ErlNode,Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout.");

wait_for_appl_started(Apps, ErlNode,Timeout) ->
    case rct_rpc:call(rpc, appmServer, get_apps, [], 1000) of
	[] ->
	    net_kernel:disconnect(ErlNode),
	    timer:sleep(5000),
	    wait_for_appl_started(Apps, ErlNode, Timeout - 5000);
    	{badrpc, _} ->
	    net_kernel:disconnect(ErlNode),
	    timer:sleep(5000),
	    wait_for_appl_started(Apps, ErlNode, Timeout - 5000);
	AppProplist ->
	    net_kernel:disconnect(ErlNode),
	    ct:log("AppProplist: ~p",[AppProplist]),
	    FoundApps = [A || {A,_} <-AppProplist],
	    ct:log("FoundApps: ~p",[FoundApps]),
	    ct:log("Apps: ~p",[Apps]),
	    case Apps -- FoundApps of
		[] ->
		    Pids = [P || {N1,P} <-AppProplist,
				 N2 <- Apps,
				 N1 == N2],
		    ct:log("WaitPids: ~p",[Apps]),
		    {ok,Pids};
		_Res ->
		    %% {error,not_all_found}
		    ct:pal("Not all expecting apps exist, wait and get apps again.",[]),
		    timer:sleep(5000),
		    wait_for_appl_started(Apps, ErlNode,Timeout - 5000)
	    end

    end.

%% ===========================================================================
%% @doc
%% Check that new Pids for the Applications is new. <br/>
%% @spec check_new_pids_created(ApplPidList, NewApplPidList) -> ok | fail
%% @end
%% ===========================================================================
check_new_pids_created(ApplPidList, NewApplPidList) ->
    check_new_pids_created(ApplPidList, NewApplPidList, 90000).

check_new_pids_created(_ApplPidList, _NewApplPidList, Timeout) when Timeout < 500 ->
    ct:fail(" One or more Application Has not restarted.");

check_new_pids_created(ApplPidList, NewApplPidList, Timeout) ->
    case lists:all(fun(Pid)->
			   not lists:member(Pid, NewApplPidList)
		   end, ApplPidList) of
	true -> % Pids in NewApplPidList does not exist in ApplPidList.
	    ok;
	false -> % sometimes test_oi takes seconds to start_up.
	    timer:sleep(250),
	    New_NewApplPidList = get_appl_pids_controlled_by_appm(),
	    check_new_pids_created(ApplPidList, New_NewApplPidList, Timeout - 250)
    end.

%% ===========================================================================
%% @doc
%% Check for COM started. <br/>
%% @spec wait_for_com_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_com_started() ->
    wait_for_com_started(90000).

wait_for_com_started(Timeout) when Timeout < 500 ->
    %% io:get_line("### COM Check,  press return\r\n");
    ct:fail("COM not started within max timeout.");

wait_for_com_started(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 1000)  of
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(1000),
	    wait_for_com_started(Timeout - 1000);
	[] ->
	    timer:sleep(1000),
	    wait_for_com_started(Timeout - 1000);
	Data ->
	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
		{match, _Result} ->
		    Data;
		_ ->
		    timer:sleep(1000),
		    wait_for_com_started(Timeout - 1000)
	    end
    end.

%% ===========================================================================
%% @doc
%% Check for COM restarted. <br/>
%% @spec wait_for_com_restarted(ComPid, ErlNode) -> Pid
%% @end
%% ===========================================================================
wait_for_com_restarted(ComPid, ErlNode) ->
    wait_for_com_restarted(ComPid, ErlNode, 90000).

wait_for_com_restarted(_ComPid, _ErlNode, Timeout) when Timeout < 500 ->
    ct:fail("COM not started within max timeout.");

wait_for_com_restarted(ComPid, ErlNode, Timeout) ->
    RpcAnsw = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 1000),
    net_kernel:disconnect(ErlNode),
    case RpcAnsw  of
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(5000),
	    wait_for_com_restarted(ComPid, ErlNode, Timeout - 5000);
	[] ->
	    timer:sleep(5000),
	    wait_for_com_restarted(ComPid, ErlNode, Timeout - 5000);
	Data ->
	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
		{match, _Result} ->
		    %% remove \n from the received list.
		    [NewData|_] = string:tokens(Data, "\n "),
		    case ComPid == NewData of
			true ->
			    %% ct:pal("com has not restarted yet!", []),
			    timer:sleep(5000),
			    wait_for_com_restarted(ComPid, ErlNode, Timeout - 5000);
			false -> %% Com is restarted with new Pid.
			    NewData
		    end;
		_ ->
		    timer:sleep(5000),
		    wait_for_com_restarted(ComPid, ErlNode, Timeout - 5000)
	    end
    end.

%% ===========================================================================
%% @doc
%% Check for Netconf to be started. <br/>
%% Wait for ct_netconfc:get_config... returns  ok. <br/>
%% @spec wait_for_netconf_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_netconf_started() ->
    ct:pal("### Check Netconf",[]),
    wait_for_netconf_started(180000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout.");

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    {ok,_} = 
		ct_netconfc:get_config(nc1,running,
				       {'ManagedElement',
					[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					[{managedElementId,[],["1"]}]}),
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	Res  ->
	    ct:log("Res: ~p", [Res]),
	    timer:sleep(5000),
	    wait_for_netconf_started(Timeout - 5000)
    end.


%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    ct:pal("### Get SW version",[]),
    timer:sleep(5000),
    ok = rct_cli:connect(cli),
    {ok ,RecievedData} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,SwInventory=1"),

    %%%%
    %% Clean upp RecievedData string to a list of strings.
    %%%%
    Var = string:tokens(RecievedData, "=\r\n "),
    %% %% drop data until you reach "SwVersion", 
    %% Don't care about result. Could be that all data has not been updated
    %% after restart at the moment when use cli.
    CXS = case lists:dropwhile(fun(X) ->
				       X =/= "SwVersion"
			       end, Var) of
	      [] ->
		  "undefined";
	      [_, Cxs | _ ] ->
		  Cxs
	  end,
    ct:pal("CXS: ~p", [CXS]),
    ok = rct_cli:disconnect(cli).

%% ===========================================================================
%% @doc
%% Wait for beam pid. <br/>
%% @end
%% ===========================================================================
wait_for_beam_pid(ErlNode) ->
    wait_for_beam_pid(ErlNode, 120000).

wait_for_beam_pid(_ErlNode, Timeout) when Timeout < 500 ->
    ct:fail("Beam Pid not recived within expected time!");

wait_for_beam_pid(ErlNode, Timeout) ->
    RpcAnsw = rct_rpc:call(rpc, os, getpid, [], 1000),
    net_kernel:disconnect(ErlNode),
    case RpcAnsw of
	{badrpc,nodedown} ->
	    ct:log("{badrpc,nodedown}", []),
	    timer:sleep(5000),
	    wait_for_beam_pid(ErlNode, Timeout - 5000);
	Res  ->
	    ct:log("#### ~p", [Res]),
	    Res
    end.

%% ===========================================================================
%% @doc
%% Wait for cold_with_test restart <br/>
%% @end
%% ===========================================================================
wait_for_cold_with_test_restart(Timeout) when Timeout < 0 ->
    ct:fail("No cold with test restart within max timeout.");

wait_for_cold_with_test_restart(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["cat /var/log/syslog | grep hwtest:1 | wc -l"], 10000) of
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(1000),
	    wait_for_cold_with_test_restart(Timeout - 10000);
	NewRes ->
	    {NewNr, _} = string:to_integer(NewRes),
	    NewNr
    end.

%%% @doc Get pids for program group
%%% ===Arguments===
%%% - 
get_pg_pids(PG) ->
    Apps = get_apps(),
    get_pg_pids(PG,Apps).

get_pg_pids(PG,Apps) ->
    Grps = rct_rpc:call(rpc, appmServer, get_info, [grps], 10000),
    {PG,Attrs} =  lists:keyfind(PG, 1, Grps),
    {pgmList,Lms} = lists:keyfind(pgmList,1,Attrs),
    Apps = get_apps(),
    PgPids=[{Name,Pid}   || {Name,Pid} <- Apps,
			    {{Name2,_},_} <- Lms,
			    Name == Name2],
    {PgPids, Apps -- PgPids}.
   
%%% @doc Verify that new pids differ from old for listed apps
%%% ===Arguments===
%%% - 
check_new_pids(PgPids,PgPidsNew) ->
    check_new_pids(PgPids,PgPidsNew,[]).

check_new_pids([], _PgPidsNew, Acc)->
    Acc;
check_new_pids([{P,Pid}|PgPids], PgPidsNew, Acc)->
    case lists:keytake(P,1,PgPidsNew) of
	{value, {P,NewPid},Rest} ->
	    if 
		Pid == NewPid ->
		    check_new_pids(PgPids,Rest,[{same,P,Pid}|Acc]);
		true ->
		    check_new_pids(PgPids,Rest,Acc)
	    end;
	false ->
	    check_new_pids(PgPids,PgPidsNew,[{not_found,P,Pid}|Acc])
    end.

%%% @doc Wait for listed apps to start
%%% ===Arguments===
%%% - 
wait_for_apps_to_start(PgPids) ->
    wait_for_apps_to_start(PgPids,[],10).

wait_for_apps_to_start(PgPids,Acc,0) ->
    {error,failed_to_start,PgPids--Acc};
wait_for_apps_to_start([],Acc,_Cnt) ->
    {ok,Acc};
wait_for_apps_to_start(PgPids,Acc,Cnt) ->
    ct:pal("Wait for these to start: ~p~n",[PgPids]),
    Apps = get_apps(),
    {New,Old} = find_pids(PgPids,Apps),
    timer:sleep(1000),
    wait_for_apps_to_start(PgPids--Old, Acc++New, Cnt-1).

%%% @doc Wait for listed apps to stop
%%% ===Arguments===
%%% - 
wait_for_apps_to_stop(PgPids) ->
    wait_for_apps_to_stop(PgPids,10).

wait_for_apps_to_stop(PgPids,0) ->
      {error,failed_to_stop,PgPids};
wait_for_apps_to_stop(PgPids,Cnt) ->
    timer:sleep(1000),
    ct:pal("Wait for these to stop: ~p~n",[PgPids]),
    Apps = get_apps(),
    case (PgPids--Apps)--PgPids of
	[] -> ok;
	_ ->
	    wait_for_apps_to_stop((PgPids--Apps)--PgPids, Cnt-1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_pids(PgPids,Apps) ->
    L = [{{Name,Pid},{Name2,Pid2}} ||  {Name,Pid} <- Apps,
				       {Name2,Pid2} <- PgPids,
				       Name == Name2] ,
    case L of
	[] ->
	    {[],[]};
	L ->
	    lists:unzip(L)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% Check two restarts. This check is made robust.
check_cwt_results_in_two_restarts() ->
    ct_telnet:expect(console, "Ericsson Version:",
		     [{timeout,30000}, no_prompt_check]), 
    %% case  ct_telnet:expect(console, "login:",
    %% 			   [{timeout,30000}, no_prompt_check]) of
    case  ct_telnet:expect(console, "Cold with test boot",
			   [{timeout,30000}, no_prompt_check]) of
	{ok,_} -> 
	    ct:pal("# First restart done. Wait for another restart."),
	    ok;
	_ ->
	    ct_telnet:send(console, ""),
	    ct_telnet:expect(console, "login:",
			     [{timeout,30000}, no_prompt_check]) 
    end,
    
    ct_telnet:expect(console, "Ericsson Version:",
		     [{timeout,120000}, no_prompt_check]), 
    %% case  ct_telnet:expect(console, "login:",
    %% 			   [{timeout,120000}, no_prompt_check]) of
    case  ct_telnet:expect(console, "RBS boot configuration",
			   [{timeout,120000}, no_prompt_check]) of
	{ok,_} -> 
	    ct:pal("## Second and last restart done as expected."),
	    ok;
	_ ->
	    ct_telnet:send(console, ""),
	    ct_telnet:expect(console, "login:",
			     [{timeout,60000}, no_prompt_check]) 
    end,
    
    %% Check that no unexpected restart occours after CWT.
    ct:pal("### check for unexpected restarts 1min after CWT", []),
    {error, _} = ct_telnet:expect(console, "Ericsson Version:",
				       [{timeout,60000}, no_prompt_check]).

%% Check if the alarm with type Alarm was raised
check_alarm_raised(Alarm) when is_list(Alarm)->
	check_alarm_raised(erlang:list_to_atom(Alarm));
check_alarm_raised(Alarm) when is_atom(Alarm) ->
	ct:pal("### Check alarm ~p raised",[Alarm]),
	Raised =
    case rct_rpc:call(rpc, comFm, get_alarms, [Alarm], 10000) of
	[] ->
		false;
	_AlarmDesc ->
		true
	end,
	
	ct:pal("### Alarm ~p raised: ~p",[Alarm, Raised]),
	Raised.

%% Fetches fallbackList from node
get_restore_escalation_list() ->
	case rct_rpc:call(rpc, swmFallbackList, get_fallback_list, [], 10000) of
	undefined ->
		[];
	FallbackList ->
		FallbackList
	end.

%% Create backup with specified Name
create_backup(Name) when is_list(Name) ->
	ct:pal("### Trying to create backup with name ~p",[Name]),
	ok = rct_cli:connect(cli),
	{ok ,_} = rct_cli:send(cli,"ManagedElement=1,SystemFunctions=1,BrM=1,BrmBackupManager=1"),
	{ok ,_} = rct_cli:send(cli,"configure"),
	{ok ,_} = rct_cli:send(cli,"createBackup " ++ Name),
	{ok ,_} = rct_cli:send(cli,"commit"),
	ok = rct_cli:disconnect(cli),
	timer:sleep(10000),
	
	case rct_rpc:call(rpc, swmBackup, get_backup_by_name, [Name], 10000) of
	[] ->
		nok;
	_BackupInfo ->
		ok
	end.

%% Restore backup with specified name
restore_backup(Name) when is_list(Name) ->
    ct:pal("### Trying to restore backup with name ~p",[Name]),
    rct_rpc:call(rpc, swmI, restore_backup, [Name], 10000),
    node_restarts(),
    wait_for_com_started(),
    timer:sleep(30000),
    ok.

%% Waits for node to performs a rollback to one of the fallback backups
wait_for_rollback_backup_restore() ->
	%% Wait for rollback to occur, no login
	ct:pal("### Waiting for rollback timer to expire + node reboot",[]),
	timer:sleep(90000),
	node_restarts(),
	wait_for_com_started(),
	timer:sleep(30000).

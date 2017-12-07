%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_escalate_to_revert_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/R8A/2
%%%
%%% @doc == Test Suite for escalate to revert after upgrade. ==
%%% <br/><br/>
%%% @end

-module(swm_rob_escalate_to_revert_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R8A/2').

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
%%% R2A/2      2014-09-29 etxivri     Created
%%% R3A/1      2014-10-17 etxivri     Update due to changed behaviour.
%%% R3A/2      2014-10-17 etxivri     Update due to changed behaviour.
%%% R3A/3      2014-10-17 etxivri     Update remove up.
%%% R3A/5      2015-01-28 etxivri     update to avoid error in ct-shell.
%%% R3A/6      2015-02-05 etxivri     Fixed edoc error.
%%% R3A/7      2015-02-11 etxivri     Update to be more robust.
%%% R3A/8      2015-02-12 etxivri     Update to be more robust.
%%% R3A/9      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/10     2015-05-28 etxkols     Changed rct_netconf hook format
%%% R4A/1      2015-10-22 etxivri     Update to be more robust.
%%% R4A/2      2015-10-29 etxivri     Update due to warm restart is enabled 
%%%                                   on TCU.
%%% R4A/3      2015-11-19 etxivri     Update due to warm restart is enabled
%%%                                   on DUS.
%%% R5A/1      2016-02-10 etxivri     Make it mor robust.
%%% R5A/2      2016-02-12 etxivri     Add more checks.
%%% R8A/1      2016-12-01 etxivri     In 17B the restored BUs will not be rem.
%%% R8A/2      2016-12-02 etxivri     New updated due to new corrected behaviour
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([%% perform_ug/1,
	 perform_ug/1,
	 remove_up/1,
	 escalate_to_revert/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).
-define(APP4, "restart").

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
		 {rct_upgrade,ug1},
                 {rct_rs232,console},
		 {cth_conn_log,[]},
		 {rct_core,[]},
		 {rct_coli, {coli, [manual_connect, {connect_timeout, 60}]}},
                 {rct_logging, {upgrade,
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  [
					   "Program ID [0-9]+ has terminated"
					  ]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    swm_test_lib:set_housekeeping_delay_variable(rpc_1),
    swm_test_lib:build_valid_ug_packakage(?NC_Session),
    Config.

%% @hidden
end_per_suite(_Config) ->
    swm_test_lib:erase_housekeeping_delay_variable(rpc_1),
    ok.
%% @hidden
init_per_testcase(escalate_to_revert, Config) ->
    rct_rpc:call(rpc_1, appmServer, reset_restart_list, [], 10000),
    %% Set rollback time to 60 sec, default is 3600 sec
    ok = rct_rpc:call(rpc_1, appmServer, set_rollback_time, [60], 10000),
    B = rct_rpc:call(rpc_1, appmServer, get_revert_timeout, [], 10000),
    ct:log("B : ~p",[B]),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(escalate_to_revert, Config) ->
    ok = rct_rpc:call(rpc_1, appmServer, set_rollback_time, [3600], 10000),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
	    remove_all_ups()
    end,
    ok;

end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
	    remove_all_ups()
    end,
    ok.

remove_all_ups() ->
    %% Don't care about the result.
    swm_test_lib:check_nc_session(?NC_Session),
    UPs = swm_test_lib:get_ups(?NC_Session),
    lists:foreach(fun(Label) ->
			  ct:pal("Label:~n~p~n",[Label]),
			  swm_test_lib:remove_upgrade_package(?NC_Session, 
							      Label),
			  timer:sleep(30000)
		  end, UPs).


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [perform_ug, escalate_to_revert, remove_up].


%%%--------------------------------------------------------------------
%%% @doc
%%% Only valid for ARM<br/>
%%% - After an upgrade there is two systemcreated backups. <br/>
%%% - when trig escalate to revert, one backup is restored. <br/>
%%% - After two reverts then no system createdbackups shall exist. <br/>
%%% - And there shall be possible to remove created upgradepackage.
%%% @spec escalate_to_revert(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
escalate_to_revert(_Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    MeId = swm_test_lib:get_me_id(?NC_Session),
    %% SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    %% ct:pal("A_CXS:~n~p~n",[SW_Vers]),
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Ups:~n~p~n",[UPs]),
    %% Highest label after Upgrade.
    LatestLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("HighestLabel:~n~p~n",[LatestLabel]),
    PrevLabel = lists:delete(LatestLabel, UPs),

    %% Check state before escalate to revert.
    "COMMIT_COMPLETED" = 
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED" = 
	swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),

    Start_ExistingBu = swm_br_lib:get_all_backups(?NC_Session, MeId),

    StartLength = length(Start_ExistingBu),
    ct:pal("StartLength : ~p", [StartLength]),

    %% trig escalate to revert first time,
    %% This result in restore backup of Final backup.
    trig_escalate_to_revert(ErlNode),

    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session, 30000),
    %% NrOfExpBu1 = StartLength-1, %% Delay variable is used.
    NrOfExpBu1 = StartLength, %% Delay variable is used.
                              %% Note. In 17B Final BU shall not be removed.
    ct:pal("# 1 # Wait for exp nr of backups: ~p", [NrOfExpBu1]),
    %% test_server:break("A"),

    wait_for_nr_of_exp_bu(NrOfExpBu1, MeId),

    ct:pal("#  Check state after first escalate to revert.", []),
    "COMMIT_COMPLETED" = 
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "PREPARE_COMPLETED" = 
	swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),

    ct:pal("Trig escalate to revert once more.", []),
    ok = rct_rpc:call(rpc_1, appmServer, set_rollback_time, [60], 10000),
    B = rct_rpc:call(rpc_1, appmServer, get_revert_timeout, [], 10000),
    ct:log("B : ~p",[B]),

    %% trig escalate to revert first time,
    %% This result in restore backup of Rollback backup.
    trig_escalate_to_revert(ErlNode),

    timer:sleep(60000),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session, 30000),
    %% NrOfExpBu2 = StartLength-2, %% Delay variable is used.
    NrOfExpBu2 = StartLength-1, %% Delay variable is used.
                                %% Note. In 17B Rollback BU shall not be removed.                               %% Here shall only Final be removed. 
    ct:pal("## 2 ## Wait for exp nr of backups: ~p ##", [NrOfExpBu2]),
    wait_for_nr_of_exp_bu(NrOfExpBu2, MeId),
    %% test_server:break("B"),

    ct:pal("##  Check state after second escalate to revert.", []),
    "PREPARE_COMPLETED" = 
	swm_test_lib:get_up_state(?NC_Session, MeId, LatestLabel),
    "COMMIT_COMPLETED" = 
	swm_test_lib:get_up_state(?NC_Session, MeId, PrevLabel),


    ct:pal("Now it shall be ok to remove upgradepackage.", []),

    timer:sleep(30000),

    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% perform_ug. <br/>
%%% @spec perform_ug(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
perform_ug(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),


    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),

    swm_test_lib:ug_create_match_result(?NC_Session,
    					"SUCCESS",
    					?SftpHost,
    					?SftpUser,
    					?SftpPassword,
    					UGPath,
    					MeId),
    timer:sleep(10000),
    Label = get_latest_up(),

    perform_ug_action(prepare, Label, MeId),
    perform_ug_action(verify, Label, MeId),
    perform_ug_action(activate, Label, MeId),

    ct:pal("Perform ug confirm~n",[]),
    swm_test_lib:ug_confirm(?NC_Session,
			    Label,
			    MeId),

    timer:sleep(60000),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% remove_up. <br/>
%%% @spec remove_up(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
remove_up(_Config) ->
    %% Don't care about the result.
    UPs = swm_test_lib:get_ups(?NC_Session),
    lists:foreach(fun(Label) ->
			  ct:pal("Label:~n~p~n",[Label]),
			  swm_test_lib:remove_upgrade_package(?NC_Session, 
							      Label)
		  end, UPs).

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
get_latest_up() ->
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:log("UPs:~n~p~n",[UPs]),

    Label = swm_test_lib:get_highest_label(UPs),
    ct:log("Label:~n~p~n",[Label]),
    Label.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action, Label, MeId) ->
    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% escalate_to_revert. Only valid for ARM<br/>
%%% @spec trig_escalate_to_revert(ErlNode) -> ok
%%% @end
%%%--------------------------------------------------------------------
trig_escalate_to_revert(ErlNode) ->
    rct_rpc:call(rpc_1, sysInitApp, reset_warm_cnt, [], 10000),
    Apps = rct_rpc:call(rpc_1, appmServer, get_apps, [], 10000),
    FoundApps = [A || {A,_} <-Apps],

    kill_and_wait(FoundApps,"first", ErlNode),

    kill_and_wait(FoundApps,"second", ErlNode),

    kill_and_wait(FoundApps,"third_cwt", ErlNode),

    kill_and_wait(FoundApps,"fourth", ErlNode),
   
    ok.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
%% %% %% Note! Don't use netconf or cli due to the escalate will be aborted.
kill_and_wait(FoundApps, T, ErlNode) ->
    BoardType = proplists:get_value(
			  board_type,ct:get_config(
				       ct:get_config({test_nodes,1}))),
	    ct:pal("BoardType: ~p", [BoardType]),

    BeamPid = rct_rpc:call(rpc_1, os, getpid, [], 10000),
    timer:sleep(60000),
    Apps = rct_rpc:call(rpc_1, appmServer, get_apps, [], 10000),
    {?APP4,UnixPid} =  lists:keyfind(?APP4, 1, Apps),
    ct:pal("Sleep a while before killing app ",[]),
    %% timer:sleep(60000),
    %% timer:sleep(30000),
    ct:pal("Killing ~p ~p time",[UnixPid,T]),
    rct_rpc:call(rpc_1, os,cmd,["kill  " ++ UnixPid],10000),
    net_kernel:disconnect(ErlNode),
    timer:sleep(5000),
    case T of
	"first" -> %only warm restart
	    %% case BoardType of
	    %% 	BoardType when BoardType == "tcu03";
	    %% 		       BoardType == "tcu0401" ->
	    %% ok;
	    %% 	_Other ->
	    %% 	    node_restarts()
	    %% end,
	    wait_for_appl_started(FoundApps);
	"third_cwt" -> 
	    check_cwt_results_in_two_restarts(),
	    %% net_kernel:disconnect(ErlNode),
	    wait_for_appl_started(FoundApps);
    	"fourth" -> %revert started after 60 sec
	    node_restarts(),
	    wait_for_new_beam_pid(BeamPid, ErlNode);
    	_ ->
	    node_restarts(),
    	    wait_for_appl_started(FoundApps) % wait for all apps to start
    end.


node_restarts() ->
    {ok,_} =  ct_telnet:expect(console, "Ericsson Version: ",
			      [{timeout,600000}, no_prompt_check]), 
    case  ct_telnet:expect(console, "login",
			   [{timeout,60000}, no_prompt_check]) of
	{ok,_} ->
	    ok;
	_Other ->
	    ct_telnet:send(console, ""),
	    {ok,_} = ct_telnet:expect(console, "login",
				      [{timeout,30000}, no_prompt_check])
    end.

check_cwt_results_in_two_restarts() ->
    ct_telnet:expect(console, "Ericsson",
		     [{timeout,30000}, no_prompt_check]), 
    case  ct_telnet:expect(console, "login:",
			   [{timeout,30000}, no_prompt_check]) of
	{ok,_} -> 
	    ct:pal("# First restart done. Wait for another restart."),
	    ok;
	_ ->
	    ct_telnet:send(console, ""),
	    ct_telnet:expect(console, "login:",
			     [{timeout,30000}, no_prompt_check]) 
    end,
    
    ct_telnet:expect(console, "Ericsson",
		     [{timeout,120000}, no_prompt_check]), 
    case  ct_telnet:expect(console, "login:",
			   [{timeout,120000}, no_prompt_check]) of
	{ok,_} -> 
	    ct:log("## Second and last restart done as expected."),
	    ok;
	_ ->
	    ct_telnet:send(console, ""),
	    ct_telnet:expect(console, "login:",
			     [{timeout,60000}, no_prompt_check]) 
    end.


%% ===========================================================================
%% @doc
%% Wait for new beam pid. <br/>
%% @end
%% ===========================================================================
wait_for_new_beam_pid(OldBeamPid, ErlNode) ->
    ct:pal("Wait for new beam pid, OldPid=~p", [OldBeamPid]),
    rct_rpc:call(rpc_1, os, putenv, ["WARM","true"], 10000), %marker
    net_kernel:disconnect(ErlNode),
    wait_for_new_beam_pid(300000,OldBeamPid, ErlNode).

wait_for_new_beam_pid(Timeout,_OldBeamPid, _ErlNode) when Timeout < 500 ->
    ct:fail("New Beam Pid not recived within expected time!");

wait_for_new_beam_pid(Timeout,OldBeamPid, ErlNode) ->
    case rct_rpc:call(rpc_1, os, getpid, [], 1000) of
	%% {badrpc,nodedown} ->
	{badrpc, _} ->
	    net_kernel:disconnect(ErlNode),
	    ct:log("{badrpc,nodedown}", []),
	    timer:sleep(5000),
	    wait_for_new_beam_pid(Timeout - 1000,OldBeamPid, ErlNode);
	OldBeamPid ->
	    case   rct_rpc:call(rpc_1, os, getenv, ["WARM"], 10000) of
		{badrpc,nodedown} ->
		    net_kernel:disconnect(ErlNode),
		    ct:log("{badrpc,nodedown}", []),
		    timer:sleep(5000),
		    wait_for_new_beam_pid(Timeout - 1000,OldBeamPid, ErlNode);
		"true" ->
		    net_kernel:disconnect(ErlNode),
		    ct:log("beam not restarted yet", []),
		    timer:sleep(5000),
		    wait_for_new_beam_pid(Timeout - 1000,OldBeamPid, ErlNode);
		false -> %cold restart with same Beam pid
		    net_kernel:disconnect(ErlNode),
		    ct:pal("#### Cold where New = Old Beam Pid ~p", 
			   [OldBeamPid]),
		    OldBeamPid
	    end;
 	Res  ->
	    net_kernel:disconnect(ErlNode),
	    ct:pal("#### New Beam ~p", [Res]),
	    Res
    end.

%% ===========================================================================
%% @doc
%% Check for Application to be started. <br/>
%% @spec wait_for_appl_started(Apps) -> {ok,Pids} | {error,Reason}
%% @end
%% ===========================================================================
wait_for_appl_started(Apps) ->
    wait_for_appl_started(Apps, 900000).

wait_for_appl_started(_,Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout.");

wait_for_appl_started(Apps,Timeout) ->
    case rct_rpc:call(rpc_1, appmServer, get_apps, [], 1000) of
	[] ->
	    timer:sleep(5000),
	    wait_for_appl_started(Apps,Timeout - 5000);
    	{badrpc, _} ->
	    timer:sleep(5000),
	    wait_for_appl_started(Apps,Timeout - 5000);
	AppProplist ->
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
		    ct:pal("Not all expecting apps exist, "
			   "wait and get apps again.",[]),
		    timer:sleep(5000),
		    wait_for_appl_started(Apps,Timeout - 5000)
	    end
    end.

%% ===========================================================================
%% @doc
%% wait_for_nr_of_exp_bu. <br/>
%% @end
%% ===========================================================================
wait_for_nr_of_exp_bu(NrOfExpBu, MeId) ->    
    wait_for_nr_of_exp_bu(NrOfExpBu, MeId, 300000).
wait_for_nr_of_exp_bu(_NrOfExpBu, _MeId, Timeout) when Timeout < 0 -> 
    ct:fail("Nr of expected backups does not exist within expected time.");
wait_for_nr_of_exp_bu(NrOfExpBu, MeId, Timeout) -> 
    New_ExistingBu = swm_br_lib:get_all_backups(?NC_Session, MeId),
    NewLength = length(New_ExistingBu),
    case NewLength == NrOfExpBu of
	true -> 
	    ct:pal("NewLength : ~p, is expected", [NewLength]);
	_Oter ->
	    ct:pal("NewLength: ~p, is NOT expected. Expected : ~p.~n" 
		   "Sleep and try again!", [NewLength, NrOfExpBu]),
	    timer:sleep(5000),
	    wait_for_nr_of_exp_bu(NrOfExpBu, MeId, Timeout-5000)
    end.

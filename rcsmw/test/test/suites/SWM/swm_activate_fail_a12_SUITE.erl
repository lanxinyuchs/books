%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a12_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/3
%%% 
%%% @doc == Test Suite for Upgrade Mechanism, activate fails due to node restart unexpected. Node will be rbooted whenprogressInfo is in Installing operating system  ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a12_SUITE).
-vsn('/main/R2A/R3A/3').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R2A/2      2014-01-29 etxivri     Created
%%% R2A/3      2014-01-31 etxivri     Cleanup SUITE, use of test_swm_lib.
%%% R2A/4      2014-02-13 etxivri     Minor update, change in ct:pal.
%%% R2A/5      2014-02-13 etxivri     Change to make the reboot in mounting software state.
%%% R2A/5      2014-03-04 etxivri     Minor update.
%%% R2A/6      2014-04-16 etxivri     Update to check after Loading OS in 
%%%                                   additionalInfo, then reboot node.
%%% R2A/9      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:
%%% R2A/10     2014-06-25 etxivri     Add delete nc session to clean up on 
%%%                                   testserver side
%%% R3A/1      2015-02-10 etxivri     Update wait for login prompt.
%%% R3A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/3      2015-05-28  etxkols     Changed rct_netconf hook format 
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([activate_fail_due_to_reboot/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).

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
		 {cth_conn_log, []},
		 {rct_rs232,console},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  []
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n"
    		   "Clean up! ", [Reason]),
    	    %% Remove created upgrade package.
	    swm_test_lib:check_nc_session(?NC_Session),
	    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    	    ct:pal("Label:~n~p~n",[Label]),
    	    swm_test_lib:remove_upgrade_package(?NC_Session, Label)
		
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [activate_fail_due_to_reboot].

%%%--------------------------------------------------------------------
%%% @doc 
%%% During activate the node will reboot unexpected.
%%% - Reboot node progressInfo is in "Database backup complete".
%%% <br/><br/>
%%% @spec activate_fail_due_to_reboot(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_fail_due_to_reboot(_Config) ->
    ct:pal("UG activate fails due to node reboots unexpected,"
	   "reboot the node when additionalInfo is in "
	   "Database backup complete",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    swm_test_lib:ug_create_match_result(?NC_Session, 
					"SUCCESS", 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId), 
    
    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					prepare),

    %%%%
    %% Verifies an upgrade package.
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					verify),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %% wait_for_trig_value_exist(?NC_Session, MeId, "Loading OS"),
    wait_for_trig_value_exist(?NC_Session, MeId, "Database backup complete"),

    %%%%
    %% Reboot node.
    %%%%
    ct:pal("### reboot!",[]),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    timer:sleep(20000),
    swm_test_lib:wait_for_login(console),

    %%%%
    %% check node restarts
    %%%%
    ct_netconfc:close_session(?NC_Session), %% Clean up at testserver side.
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    ct:pal("Wait for progress after reboot.", []),
    %% test_server:break("A"),

    case swm_test_lib:
    	wait_for_progress_result(?NC_Session, MeId, ActionId, ErlNode) of
    	[{"FAILURE" = Res, "activate" = ActionName, 
	  "The action was interrupted by a restart" = ProgressInfo, 
	  ResultInfo, State, _ProgReport}] ->
    	    ct:pal("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n"
    		   "state:~p~n",[Res, 
    				 ActionName, 
    				 ProgressInfo, 
    				 ResultInfo,
    				 State]),
    	    ok;
    	Result ->
    	    ct:pal("createUpgradePackage: ~p",[Result]),
    	    ct:fail(Result)
    end,

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_trig_value_exist(NC_sess, MeId, CheckVal) ->
    wait_for_trig_value_exist(NC_sess, MeId, CheckVal, 360).

wait_for_trig_value_exist(_NC_sess, _MeId, CheckVal, Timeout) when Timeout < 0 ->
    ct:fail("trig value: ~p , not found within expected time.", [CheckVal]);

wait_for_trig_value_exist(NC_sess, MeId, CheckVal, Timeout) ->
    Report = swm_test_lib:get_report(NC_sess, MeId),
    ct:pal("Report: ~p", [Report]),
    
    Info = [X || X <- Report, X == {additionalInfo, [], [CheckVal]}],
    case Info of
	[{additionalInfo,[],[CheckVal]}] ->
	    ct:pal("AdditionalInfo: ~p, expect ~p .", 
		   [Info, CheckVal]),
	    ok;
	[]->
	    timer:sleep(1000),
	    wait_for_trig_value_exist(NC_sess, MeId, CheckVal, 
				   Timeout - 1)
    end.

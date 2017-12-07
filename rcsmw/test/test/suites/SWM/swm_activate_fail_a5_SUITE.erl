%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a5_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R6A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism when activate fails due to version is not OK in new UP  ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a5_SUITE).
-vsn('/main/R2A/R3A/R6A/1').

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
%%% R2A/2      2014-01-07 etxivri     Created
%%% R2A/4      2014-01-20 etxivri     Correct a ct:pal
%%% R2A/5      2014-01-31 etxivri     Cleanup, use of swm_test_lib.
%%% R2A/6      2014-02-04 etxivri     Update to get faulty modified UP.
%%% R2A/7      2014-03-20 etxivri     Update check after restart. 
%%%                                   Remove that check when fault not result 
%%%                                   in reboot = TR HS23704.
%%%                                   Comment out some unused code.
%%% R2A/7      2014-03-27 etxivri     Update error flog filter.
%%% R2A/9      2014-04-04 etxivri     Update check of failure.
%%% R2A/10     2014-04-10 etxivri     Simplyfied check.
%%% R2A/11     2014-04-11 etxivri     Update due to gracefull canceling is used.
%%% R2A/12     2014-04-16 etxivri     Removed breaks. Update of filtered error.
%%% R2A/13     2014-05-22 etxivri     Removed filter str in ERROR log.
%%% R2A/14     2014-08-08 etxivri     Update filter str in ERROR log.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
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
-export([activate_when_to_up_version_not_supported/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).
-define(ModScriptName_A5, "swm_mod_fake_a5.sh").

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
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  [
					   "throw: verify_upgrade_failed"
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_faulty_ug_packakage(?NC_Session, 
    					   ?ModScript_Path, 
    					   ?ModScriptName_A5),
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
    		   "Clean up!", [Reason]),
	    %% Remove created upgrade package.
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
    [activate_when_to_up_version_not_supported].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Activate an upgrade package consisting of an not suported version. 
%%% The result shall be gracefull cancelling of the upgrade.
%%% @end
%%%--------------------------------------------------------------------
activate_when_to_up_version_not_supported(_Config) ->
    ct:pal("UG activate fails due to a from verision is not ok in up.",[]),
    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    FromUP = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("FromUP: ~p",[FromUP]),

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
    %% This shall fail due to a fromVersion in to up will be set removed
    %% then the upgrade shall not be allowed.
    %% During activate the node will reboot.
    %%%%
    ActionId_4 = swm_test_lib:get_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
					activate, 
					Label, 
					MeId),

    ct:pal("Wait for progress after activate UP. This shall result in Failure"),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    net_kernel:disconnect(ErlNode),

    %% check_progress(?NC_Session, MeId, "The action could not be completed"),
    [{"FAILURE" = A2, "activate" = B2, C2, D2, E2, ProgReport}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
    					      MeId, 
    					      ActionId_4, 
    					      ErlNode),
    ct:pal("result:~p~n"
    	   "actionName:~p~n"
    	   "progressInfo:~p~n"
    	   "resultInfo:~p~n"
    	   "state:~p~n", [A2, B2, C2, D2, E2]),

    ct:log("result:~p", [ProgReport]),

    %% match_exp_info_str(ProgReport, ErlNode),
    match_exp_info_str(ProgReport),

    %%%%
    %% Fallback.
    %%%%
    %% ct:pal("Wait for node to restarts due to fallback."),
    %% net_kernel:disconnect(ErlNode),
    %% swm_test_lib:wait_for_node_state(ErlNode, down),
    %% swm_test_lib:wait_for_node_state(ErlNode, up),
    %% net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    %% [{"FAILURE", 
    %%   "activate", 
    %%   "The action was interrupted by a restart", 
    %%   _D3, _E3, _ProgReport3}] =
    %% 	swm_test_lib:wait_for_progress_result(?NC_Session, 
    %% 					      MeId, 
    %% 					      ActionId_4, 
    %% 					      ErlNode),

    %% test_server:break("A"),

    %%%%
    %% Check fallback has been performed.
    %%%%
    EndFromUP = swm_test_lib:get_sw_version(?NC_Session, MeId),
    case FromUP of 
    	EndFromUP ->
    	    ct:pal("SwVersionMain is same as it was in beginning.~n"
    		   "Fallback has been performed!"),
    	    ok;
    	_Res ->
    	    ct:pal("NOK: ~p", [_Res]),
    	    ct:fail("NOK")
    end,

    %% test_server:break("B"),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.


%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
%% match_exp_info_str(ProgReport, ErlNode) ->
match_exp_info_str(ProgReport) ->
    case check_exp_info(ProgReport, 
			additionalInfo, 
			"versioning failure for schemas") of
	nomatch ->
	    %% ct:fail("Additinal info not expected");
	    ct:pal("node has probably already restarted due to fallback,~n"
		   "Missed in our wait to get info before node restarted. "
		   "Don't care.");
	{match, _} ->
	    ct:pal("Wait for node to restarts due to fallback."),
	    %% net_kernel:disconnect(ErlNode),
	    %% swm_test_lib:wait_for_node_state(ErlNode, down),
	    %% swm_test_lib:wait_for_node_state(ErlNode, up),
	    %% net_kernel:connect(ErlNode),
	    ok
    end.

check_exp_info(ProgReport, Prog, ExpStr) ->
    Info = [InfoStr || {Key,[],[InfoStr]} <- ProgReport, Key == Prog],
    ct:pal("Info : ~p", [Info]),
    case re:run(Info, ExpStr) of
	{match, _} = Res ->
	    Res;
	nomatch ->
	    ct:pal("Expected ~p not found.  expected: ~p", [Prog, ExpStr]),
	    nomatch
    end.



%% check_progress(NC_sess, MeId, CheckVal) ->
%%     check_progress(NC_sess, MeId, CheckVal, 360000).

%% check_progress(_NC_sess, _MeId, CheckVal, Timeout) when Timeout < 0 ->
%%     ct:fail("trig value: ~p , not found within expected time.", [CheckVal]);

%% check_progress(NC_sess, MeId, CheckVal, Timeout) ->
%%     %% {reportProgress, [{struct,"AsyncActionProgress"}], Report} = 
%%     %% 	swm_test_lib:get_report_progress(NC_sess, MeId),
%%     Report = swm_test_lib:get_report(NC_sess, MeId),
%%     {value, {progressInfo, _, [ProgInfo]}} = 
%% 	lists:keysearch(progressInfo, 1, Report),
%%     ct:pal("ProgInfo: ~p, expect ~p .", [ProgInfo, CheckVal]),
%%     case ProgInfo of
%% 	CheckVal -> %% Check state is in finished and result Failure.
%% 	    ct:pal("Report: ~p", [Report]),
%% 	    {value, {state, _, ["FINISHED"]}} = 
%% 		lists:keysearch(state, 1, Report),
%% 	    {value, {result, _, ["FAILURE"]}} = 
%% 		lists:keysearch(result, 1, Report),
%% 	    ok;
%% 	_ ->
%% 	    timer:sleep(500),
%% 	    check_progress(NC_sess, MeId, CheckVal, Timeout - 500)
%%     end.

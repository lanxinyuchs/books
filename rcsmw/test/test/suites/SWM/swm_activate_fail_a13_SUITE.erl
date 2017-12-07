%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a13_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R6A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism when activate fails due to version is not OK in new UP. From version is higher that to version.  ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a13_SUITE).
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
%%% R2A/2      2014-02-07 etxivri     Created. Note TC need to be modified when 
%%%                                   graceful fallback is implemented.
%%% R2A/3      2014-03-24 etxivri     Update allowed ERROR in log filter.
%%% R2A/4      2014-04-10 etxivri     Simplyfied check.
%%% R2A/4      2014-04-17 etxivri     Update for graceful fallback.
%%% R2A/7      2014-05-22 etxivri     Update str in ERROR log
%%% R2A/8      2014-08-05 etxivri     Update str in ERROR log
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
-export([activate_fail_due_to_lower_version/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).
-define(ModScriptName_A13, "swm_mod_fake_a13.sh").

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
    	   "Create cxps that shall be used for UG.~n"),
    %%% Last character in ugt_1_imm.xml is removed.
    swm_test_lib:build_faulty_ug_packakage(?NC_Session, 
					   ?ModScript_Path, 
					   ?ModScriptName_A13,
					   ug1),
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
    [activate_fail_due_to_lower_version].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Activate an upgrade package consisting of an lower to version <br/>
%%% than from version. <br/>
%%% The result shall be gracefull cancelling of the upgrade. <br/>
%%% @spec activate_fail_due_to_lower_version(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_fail_due_to_lower_version(_Config) ->
    ct:pal("UG activate fails due, to verision is lower than from version in UP.",[]),
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
    %% This shall fail due to metadata is not correct.
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

    [{"FAILURE" = A2, 
      "activate" = B2, 
      %% "The action could not be completed" = C2, 
      C2,
      D2, E2, ProgReport}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId_4, 
					      ErlNode),

    ct:pal("result:~p~n"
    	   "actionName:~p~n"
    	   "progressInfo:~p~n"
    	   "resultInfo:~p~n"
    	   "state:~p~n", [A2, B2, C2, D2, E2]),

    ct:pal("result:~p", [ProgReport]),

    match_exp_info_str(ProgReport, ErlNode),

    %%%%
    %% Graceful Fallback.
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

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

match_exp_info_str(ProgReport, ErlNode) ->
    case check_exp_info(ProgReport, 
			additionalInfo, 
			"versioning failure for schemas") of
	nomatch ->
	    %% ct:fail("Additinal info not expected");
	    ct:pal("node has probably already restarted due to fallback,~n"
		   "Missed in our wait to get info before node restarted. "
		   "Don't care.");
	{match, _} ->
	    case check_exp_info(ProgReport, 
			   additionalInfo, 
			   "demo") of
		{match, _} ->
		    %% ct:pal("Wait for node to restarts due to fallback."),
		    %% %% net_kernel:disconnect(ErlNode),
		    %% swm_test_lib:wait_for_node_state(ErlNode, down),
		    swm_test_lib:wait_for_node_state(ErlNode, up),
		    net_kernel:connect(ErlNode),
		    ok;
		nomatch -> 
		    ct:fail("Additinal info not expected")
	    end
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

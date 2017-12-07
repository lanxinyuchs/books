%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a16_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/R7A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism. Verify that a upgrade fail from a application results in a fallback. ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a16_SUITE).
-vsn('/main/R2A/R3A/R4A/R6A/R7A/1').

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
%%% R2A/2      2014-02-21 etxivri     Created
%%% R2A/3      2014-03-24 etxivri     Update fallback check.
%%% R2A/4      2014-05-07 etxivri     Added check after node restart.
%%% R2A/5      2014-05-22 etxivri     Update ERROR log str.
%%% R2A/6      2014-06-03 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/7      2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R3A/1      2014-06-12 etxivri     update due to changed behaviour
%%% R3A/2      2015-01-29 etxivri     Update to remove error in ct-shell.
%%% R3A/3      2015-01-30 etxivri     More update to remove error in ct-shell.
%%%                                   Add check that node restart two times.
%%% R3A/5      2015-04-30 etxivri     Update wait for login.
%%% R3A/6      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/7      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-09-03 etxivri     Update ERROR filter.
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
-export([activate_upgrade_failure_from_appl/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).
-define(ModScriptName_A16, "swm_mod_fake_a16.sh").

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
		 %% {rct_rs232,console},
		 %% {cth_conn_log, []},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  ["fail upgrade, requested by app"
					  ]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1},
		 {cth_conn_log, []},
		 {rct_rs232,console}
		]}].


%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_faulty_ug_packakage(?NC_Session, 
					   ?ModScript_Path, 
					   ?ModScriptName_A16),
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
    [activate_upgrade_failure_from_appl].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Activate an upgrade package.
%%% Verify that a upgrade fail from a application results in a fallback.. <br/>
%%% And the activate will be FAILURE. <br/>
%%% @spec activate_upgrade_failure_from_appl(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_upgrade_failure_from_appl(_Config) ->
    ct:pal("UG activate. Verify that a upgrade fail from a application results in a fallback.",[]),
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

    OldActionId = no_check,
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
	wait_for_swm_progress_result(?NC_Session, MeId, OldActionId) of
	[{"SUCCESS" = Result,
	  "createUpgradePackage" = ActionName, 
	  ProgressInfo, 
	  ResultInfo, 
	  _State, _ProgReport}] ->
	    ct:log("result:~p~n"
		   "actionName:~p~n"
		   "progressInfo:~p~n"
		   "resultInfo:~p~n",[Result,
				      ActionName, 
				      ProgressInfo, 
				      ResultInfo]),
	    ok;
	Result ->
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end,

    
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

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%
    %% Activates an upgrade package.
    %% - s1v2 application make the upgrade fail.
    %%%%
    ActionId_4 = swm_test_lib:get_action_id(?NC_Session, MeId),
    ct:pal("Activate.", []),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
					activate, 
					Label, 
					MeId),
    net_kernel:disconnect(ErlNode),

    ct:pal("Wait for progress after activate UP. This shall result in Failure"),

    %%%%
    %% check node restarts
    %%%%
    ct:pal("Wait for node to restarts due to activate."),
    RestartStr = swm_test_lib:get_node_restart_str(),
    net_kernel:disconnect(ErlNode),
    ct_telnet:expect(console, RestartStr,
		     [{timeout, 60000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
     			      [{timeout,120000}, no_prompt_check]),

    %% Will results in two restarts.
    ct:pal("Wait for second node restarts."),
    ct_telnet:expect(console, RestartStr,
		     [{timeout, 300000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
     			      [{timeout,120000}, no_prompt_check]),

    swm_test_lib:check_nc_session(?NC_Session),

    ct:pal("Check activate failed."),
    [{"FAILURE" = A2, 
      "activate" = B2, 
      "The action was interrupted by a restart" = C2, 
      D2, E2, ProgReport}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
    					      MeId, 
    					      ActionId_4, 
    					      dummy),

    ct:pal("result:~p~n"
    	   "actionName:~p~n"
    	   "progressInfo:~p~n"
    	   "resultInfo:~p~n"
    	   "state:~p~n", [A2, B2, C2, D2, E2]),

    ct:log("result:~p", [ProgReport]),

    %%% Check expected fault cause is found in dummy log.
    %%% to make sure applications has been runed the fault.
    Cat = rct_rpc:call(
    	    rpc_1, os, cmd, 
    	    ["cat /rcs/applicationlogs/DUMMY-*_CXP9021691_*/log_ugt_s1v2*"], 
    	    10000, noprint),
    ct:pal("Cat:~n~p~n",[string:tokens(Cat,"\n, ")]),
    BBBB = string:tokens(Cat,"\n, "),
    case lists:member("failUpgrade", BBBB) of
    	true ->
    	    ok;
    	false ->
    	    ct:fail("fault failUpgrade not found in DUMMY log")
    end,

    %% test_server:break("AAA"),

    %%%%
    %% Check fallback has been performed.
    %%%% 
    swm_test_lib:check_nc_session(?NC_Session),
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

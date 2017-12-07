%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a11_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R6A/R8A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism. Fallback when activate fails due to dataconversion nok. ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a11_SUITE).
-vsn('/main/R2A/R3A/R4A/R6A/R8A/1').

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
%%% R2A/2      2014-01-23 etxivri     Created, this is not ready to be used.
%%% R2A/3      2014-01-30 etxivri     Cleanup SUITE,use of test_swm_lib instead.
%%% R2A/4      2014-02-05 etxivri     Added create failing UP. 
%%% R2A/5      2014-03-03 etxivri     Update for ARM. 
%%% R2A/6      2014-03-21 etxivri     Added mor checks.
%%% R2A/8      2014-05-07 etxivri     Added check after node restart.
%%% R2A/10     2014-05-15 etxivri     Update to wait for application to start.
%%% R2A/11     2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R3A/1      2015-02-17 etxivri     Update error log filter.
%%% R3A/2      2014-06-04 etxivri     Make it more robust.
%%% R3A/3      2015-04-09 etxivri     Update due to new behaviour.
%%% R3A/4      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/5      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R3A/6      2015-06-26 etxivri     Update due to new behaviour. increased
%%%                                   timeout for fallback due to dataconversio
%%%                                   fail. 
%%% R4A/1      2015-10-06 etxivri     Update due to new behaviour. increased
%%%                                   timeout for fallback due to dataconversio
%%%                                   fail. 
%%% R6A/1      2016-18-17 etxkols     GIT migration
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([activate_data_conversion_fail/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).
-define(ModScriptName_A11_ppc, "swm_mod_fake_a11_ppc.sh").
-define(ModScriptName_A11_arm, "swm_mod_fake_a11_arm.sh").

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
					  ["data conversion taking too long, classes not handled"
					  ]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG.~n"
	   "Note! different mod script used depending on used HW."), 
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ct:fail("sim not implemented yet!");
	"target" -> 
	    BoardType = proplists:get_value(
			  board_type,ct:get_config(
				       ct:get_config({test_nodes,1}))),
	    ct:log("BoardType: ~p", [BoardType]),
	    case BoardType of
		BoardType when BoardType == "dus4101";
			       BoardType == "duw4101" ->
		    swm_test_lib:build_faulty_ug_packakage(?NC_Session, 
							   ?ModScript_Path, 
							   ?ModScriptName_A11_ppc);
		_ -> % tcu03, dus5201, dus3201?
		    %% ct:fail("TC not tested on ARM yet, due to not supported yet!")
		    swm_test_lib:build_faulty_ug_packakage(?NC_Session, 
		    					   ?ModScript_Path, 
		    					   ?ModScriptName_A11_arm)
	    end
    end,
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
    	    swm_test_lib:remove_upgrade_package(?NC_Session, Label),
	    ok
    end,

    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [activate_data_conversion_fail].

%%%--------------------------------------------------------------------
%%% @doc 
%%% During activate a fallback shall be performed due to datatconversion fails. 
%%% <br/><br/>
%%% @spec activate_data_conversion_fail(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_data_conversion_fail(_Config) ->
    ct:pal("UG, Activate fails,"
	   "Fallback due to dataconversion fails. Appl ugt_s1v2 not started.",[]),
    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

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
    %% check ugt_s1v2 is started. 
    %%%%
    Apps = rct_rpc:call(rpc_1, appmServer, get_apps, [], 10000, noprint),
    ct:pal("Apps_1:~n~p~n",[Apps]),
    case lists:keymember("ugt_s1v2", 1, Apps) of
	true ->
	    ok;
	false ->
	    ct:fail("ugt_s1v2 should be started")
    end,

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ct:pal("Activate.", []),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %%%%
    %% check node restarts during activate
    %%%%
    net_kernel:disconnect(ErlNode),
    %% timer:sleep(5000), %% could get nodedown directly after activate!.
    %% swm_test_lib:wait_for_node_state(ErlNode, down),
    %% RestartStr = swm_test_lib:get_node_restart_str(),
    RestartStr = "Ericsson Version: ",
    {ok,_} = ct_telnet:expect(console, RestartStr, 
    			      [{timeout,180000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
     			      [{timeout,120000}, no_prompt_check]),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    %% net_kernel:connect(ErlNode),

    %% %% New behaviour. netconf might not get up before fallback.
    %% swm_test_lib:check_nc_session(?NC_Session),
    %% %%%%
    %% %% check ugt_s1v2 is not started. 
    %% %%%%
    %% Apps_1 = rct_rpc:call(rpc_1, appmServer, get_apps, [], 10000, noprint),
    %% ct:pal("Apps_1:~n~p~n",[Apps_1]),
    %% case lists:keymember("ugt_s1v2", 1, Apps_1) of
    %% 	false ->
    %% 	    ok;
    %% 	true ->
    %% 	    ct:fail("ugt_s1v2 should not be started")
    %% end,

    %%%%
    %% check node restart due to fallback.
    %%%%
    ct:pal("Wait for progress after activate.~n "
	   "This shall result in Fallback due to dataconversion fails."),
    %% net_kernel:disconnect(ErlNode),
    %% timer:sleep(5000),
    %% swm_test_lib:wait_for_node_state(ErlNode, down),
    {ok,_} = ct_telnet:expect(console, RestartStr, 
    			      [{timeout,1800000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
     			      [{timeout,120000}, no_prompt_check]),
    %% swm_test_lib:wait_for_node_state(ErlNode, up),
    %% net_kernel:connect(ErlNode),

    swm_test_lib:check_nc_session(?NC_Session),

    [{"FAILURE" = A, 
      "activate" = B, 
      "The action was interrupted by a restart" = C, 
      D, E, ProgReport}] =
	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId,
					      %% ErlNode),
					      dummy),
    
    ct:pal("result:~p~n"
    	   "actionName:~p~n"
    	   "progressInfo:~p~n"
    	   "resultInfo:~p~n"
    	   "state:~p~n",[A, B, C, D, E]),

    ct:pal("ProgReport:~p~n", [ProgReport]),
    %% test_server:break("AAA"),

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
    %% check ugt_s1v2 is started. 
    %%%%
    wait_for_app_to_be_started(),
    %% Apps_2 = rct_rpc:call(rpc_1, appmServer, get_apps, [], 10000, noprint),
    %% ct:pal("Apps_1:~n~p~n",[Apps_2]),
    %% case lists:keymember("ugt_s1v2", 1, Apps_2) of
    %% 	true ->
    %% 	    ok;
    %% 	false ->
    %% 	    ct:fail("ugt_s1v2 should be started")
    %% end,

    %%% Print dummy log if it should be needed!.
    Cat = rct_rpc:call(
    	    rpc_1, os, cmd, 
    	    ["cat /rcs/applicationlogs/DUMMY-*_CXP9021691_*/log_ugt_s1v2*"], 
    	    10000, noprint),
    %% ct:pal("Cat:~n~p~n",[Cat]),
    %% T = string:tokens(Cat,"\n, "),
    ct:log("Cat : ~n~p", [string:tokens(Cat,"\n, ")]),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
wait_for_app_to_be_started() ->
    wait_for_app_to_be_started(120000).

wait_for_app_to_be_started(Timeout) when Timeout < 0 ->
    ct:fail("No Appl ugt_s1v2 started within max timeout 60sec.");

wait_for_app_to_be_started(Timeout) ->
    case rct_rpc:call(rpc_1, appmServer, get_apps, [], 10000, noprint) of
	{badrpc,_} ->
	    timer:sleep(5000),
	    wait_for_app_to_be_started(Timeout-5000);
	 Apps_2 ->
	    ct:pal("Apps_2:~n~p~n",[Apps_2]),
	    case lists:keymember("ugt_s1v2", 1, Apps_2) of
		true ->
		    ok;
		false ->
		    timer:sleep(5000),
		    wait_for_app_to_be_started(Timeout-5000)
	    end
    end.


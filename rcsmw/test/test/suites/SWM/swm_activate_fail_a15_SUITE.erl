%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_activate_fail_a15_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R6A/R7A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism. Make sure activate does not fail when application copy an unknown class. ==
%%% <br/><br/>
%%% @end

-module(swm_activate_fail_a15_SUITE).
-vsn('/main/R2A/R3A/R6A/R7A/1').

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
%%% R2A/2      2014-02-19 etxivri     Created
%%% R2A/2      2014-03-24 etxivri     Update to trigger fallback after 
%%%                                   action success.
%%% R2A/4      2014-04-16 etxivri     Update to trig fallback.
%%% R2A/5      2014-04-23 etxivri     Update to handle fallback.
%%% R2A/6      2014-04-30 etxivri     Update to trig fallback using reboot.
%%% R2A/7      2014-05-08 etxivri     Update to see what happens on consol
%%%                                   when node restart during activate.
%%% R2A/8      2014-05-26 etxivri     Use reboot -f to trig fallback.
%%% R2A/9      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:
%%% R2A/10     2014-07-04 etxivri     Minor update to prevent ERROR REPORT in 
%%%                                   ct shell when node restart.
%%% R2A/11     2014-08-19 etxivri     Update due to differents behaviour between
%%%                                   tcu03 and dus41.
%%% R3A/1      2015-02-17 etxivri     Update error log filter.
%%% R3A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/3      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R6A/2      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-11-15 etxkivri    Update to be more robust.
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([activate_when_copy_fail/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(ModScript_Path, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/bin/"])).
-define(ModScriptName_A15, "swm_mod_fake_a15.sh").

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
					  ["copy_instances, operation ABORTED because of unknown classes"
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    ct:pal("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_faulty_ug_packakage(?NC_Session, 
					   ?ModScript_Path, 
					   ?ModScriptName_A15),
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
    [activate_when_copy_fail].

%%%--------------------------------------------------------------------
%%% @doc 
%%% Activate an upgrade package.During activate a application copy instance<br/>
%%% when a class is unknown. This shall not make upgrade fail. <br/>
%%% The application will continue to copyt existing class. <br/>
%%% And the activate will be SUCCESS. <br/>
%%% @spec activate_when_copy_fail(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_when_copy_fail(_Config) ->
    ct:pal("UG activate shall not fail when application read not existing class.",[]),
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
    %% - s1v2 application will try to a non existing classs.
    %% - Then it will continue.
    %% - the activate phase shall not fail.
    %% During activate the node will reboot.
    %%%%
    %% swm_test_lib:ug_action_match_result(?NC_Session, 
    %% 					"SUCCESS",
    %% 					Label,
    %% 					MeId, 
    %% 					activate),
    ActionId_2 = swm_test_lib:get_action_id(?NC_Session, MeId),
    ct:pal("### Activate.", []),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
					activate, 
					Label, 
					MeId),

    %%%%
    %% To see what happens on the consol.
    %%%%
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),
    ct:pal("Wait for node to restarts due to activate."),
    net_kernel:disconnect(ErlNode),
    timer:sleep(5000),
    swm_test_lib:wait_for_node_state(ErlNode, down),
    {ok,_} = ct_telnet:expect(console, "Restarting system", 
    			      [{timeout,60000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
     			      [{timeout,60000}, no_prompt_check]),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    %%%%
    %% Wait for success.
    %%%%
    [{"SUCCESS" = _A2, 
     "activate" = _B2, 
      _C2, 
      _D2, _E2, ProgReport}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId_2, 
					      dummy),
    ct:pal("ProgReport: ~p",[ProgReport]),

    %% %%% Check expected fault cause is found in dummy log.
    %% %%% to make sure applications has been runed the fault.
    Cat = rct_rpc:call(
    	    rpc_1, os, cmd, 
    	    ["cat /rcs/applicationlogs/DUMMY-*_CXP9021691_*/log_ugt_s1v2*"], 
    	    10000, noprint),
    ct:log("Cat:~n~p~n",[string:tokens(Cat,"\n, ")]),
    BBBB = string:tokens(Cat,"\n, "),
    case lists:member("fault_a15", BBBB) of
    	true ->
    	    ok;
    	false ->
    	    ct:fail("fault_a15 not found in DUMMY log")
    end,

    timer:sleep(30000),
    %%%%
    %% trig fallback. Then it is possible to remove up.
    %%%%
    %% ct:pal("### reboot to trigger fallback!",[]),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),

    BoardType =	proplists:get_value(board_type,
				    ct:get_config(
				      ct:get_config({test_nodes,1}))),
    ct:pal("### BoardType: ~p ", [BoardType]),
    case BoardType of
	"dus4101" ->
	    ct:pal("### trig fallback using cancel",[]),
	    ok = swm_test_lib:up_action_generic(?NC_Session, 
						cancel, 
						Label, 
						MeId),
	    ok;
	_ ->
	    ct:pal("### reboot to trigger fallback!",[]),
	    ok = ct_telnet:send(console, "reboot -f")
    end,

    timer:sleep(5000),
    %% swm_test_lib:wait_for_node_state(ErlNode, down),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
			      [{timeout,180000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
    			      [{timeout,60000}, no_prompt_check]),

    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    timer:sleep(20000),
    ActionId_4 = no_check,
    [{"FAILURE", 
      "activate", 
      "The action was interrupted by a restart", 
      _D3, _E3, _ProgReport3}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId_4, 
					      ErlNode),

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

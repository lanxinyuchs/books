%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_all_remove_loop_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R11A/1
%%% 
%%% @doc == Upgrade. Perform complete UG, remove UP. This can be looped.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_all_remove_loop_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R11A/1').

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
%%% R2A/2      2014-04-10 etxivri     Created
%%% R2A/4      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/5      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R2A/6      2014-06-12 etxivri     Update due to swm changes off reportprogr.
%%% R2A/7      2014-08-14 etxivri     Update due to handle if remove fail when
%%%                                   it is not allowed to be removed.
%%% R2A/8      2014-08-29 etxivri     Update to use modify_cxs in swm_test_lib.
%%% R2A/9      2014-09-08 etxivri     Update get label to be used.
%%% R3A/1      2014-10-16 etxivri     Update due to new behaviour on backups.
%%% R3A/2      2015-01-29 etxivri     Update to remove error in ct-shell.
%%% R3A/4      2015-02-05 etxivri     Update to remove error in ct-shell.
%%%                                   And some cleanup.
%%% R3A/5      2015-05-28 etxivri     Remove hard coded sftp server data.
%%%                                   
%%% R3A/6      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-09-11 etxivri     Add a ct:pal.
%%% R4A/2      2015-09-23 etxivri     Change order in hooks to make consol log 
%%%                                   work. And removed unnecessery code.
%%% R4A/3      2015-12-01 etxivri     Update remove to be more robust.
%%% R4A/4      2015-12-03 etxivri     Update due to max-up-check is enabled 
%%%                                   after restart.
%%% R5A/1      2016-01-13 etxivri     Make it more robust.
%%% R6A/1      2016-09-14 etxberb     Added fallback_info/0 and robustness.
%%% R7A/1      2016-11-14 etxivri     Update to handle remove UP if several
%%%                                   systems BUs exist.
%%% R8A/1      2016-12-13 etxivri     Make it more robust.
%%% R11A/2     2017-08-30 etxivri     Update for git.
%%% ----------------------------------------------------------

%-compile([export_all]).
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
-export([test_all/1,
	 build_to_up/1,
	 create/1, 
	 prepare/1, 
	 verify/1, 
	 activate/1, 
	 confirm/1,
	 remove/1,
	 disable_max_up_check/1,
	 enable_max_up_check/1,
	 build_same_ug_package/1,
	 set_start_ug_rev_in_ug_top_xml/1
	]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).
-define(RPC_CALL(__M, __F, __A), rct_rpc:call(rpc_1, __M, __F, __A, 10000, noprint)).

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
		 {cth_conn_log, []},
		 {rct_core,[]},
		 {rct_coli, {coli, [manual_connect]}},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  []
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    swm_test_lib:set_housekeeping_delay_variable(rpc_1),
    Config.

%% @hidden
end_per_suite(_Config) ->
    swm_test_lib:erase_housekeeping_delay_variable(rpc_1),
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    disable_max_up_check(Config),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n"
    	    	   "Clean up! TBD!", [Reason]),
	    enable_max_up_check(Config),
    	    %% Remove created upgrade package.
	    swm_test_lib:check_nc_session(?NC_Session),
	    %%%% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
	    remove_all_ups()
	    %% UPs = swm_test_lib:get_ups(?NC_Session),
	    %% Label = swm_test_lib:get_lowest_label(UPs),
    	    %% ct:pal("Label:~n~p~n",[Label]),
    	    %% swm_test_lib:remove_upgrade_package(?NC_Session, Label)
		
    end,
    
    ok.


remove_all_ups() ->
    %% Don't care about the result.
    UPs = swm_test_lib:get_ups(?NC_Session),
    lists:foreach(fun(Label) ->
			  ct:pal("Label:~n~p~n",[Label]),
			  swm_test_lib:remove_upgrade_package(?NC_Session, 
							      Label)
		  end, UPs).


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [%%build_to_up, 
     build_same_ug_package,
     set_start_ug_rev_in_ug_top_xml,
     test_all,
     %% test_all,
     %% test_all,
     %% test_all,
     %% test_all,
     %% test_all,
     %% test_all,
     %% test_all,
     %% test_all,
     test_all
    ].

    %% [create, prepare, verify, activate, confirm, remove].
    %% [create, remove].
    %% [create].
    %% [fake_up].

groups() ->
    [{group_1,[],[%%build_to_up,
		  test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all
		       ]},
     {short_group_1,[],[%%build_to_up,
			test_all,
			test_all
		       ]}
    ].


%%%--------------------------------------------------------------------
%%% @doc Create a valid up.
%%% @end
%%%--------------------------------------------------------------------
build_to_up(_Config) ->
    ct:pal("# init per suite. ~n"
	    "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
build_same_ug_package(_Config) ->
    swm_test_lib:build_same_ug_package(?NC_Session, ug1).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
set_start_ug_rev_in_ug_top_xml(_Config)  ->
    MeId = swm_test_lib:get_me_id(?NC_Session),
    swm_test_lib:set_start_ug_rev_in_ug_top_xml(?NC_Session, MeId).

%%%--------------------------------------------------------------------
%%% @doc Perform a complete upgrade, remove oldest UP.
%%% @end
%%%--------------------------------------------------------------------
test_all(_Config) ->
    ct:pal("UG.",[]),

    fallback_info(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Modify version in cxs to UP.
    %%%%
    %% swm_test_lib:get_action_capable(?NC_Session, MeId),
    %% swm_test_lib:wait_for_action_capable(?NC_Session, MeId, "CAPABLE"),
    modify_cxs(ug1),

    %% DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df = string:tokens(DF, " \n"),
    %% ct:pal("### Df: ~p", [Df]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    create(_Config),

    fallback_info(),
    %%%%
    %% Get Latest UP
    %%%%
    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    %% ct:pal("Label:~n~p~n",[Label]),

    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UPs:~n~p~n",[UPs]),

    Label = swm_test_lib:get_highest_label(UPs),
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

    fallback_info(),
    %%%%
    %% Verifies an upgrade package.
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					verify),
    fallback_info(),
    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    _ActionId_2 = swm_test_lib:get_action_id(?NC_Session, MeId),
    ct:pal("### Activate.", []),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					activate,
					ErlNode,
					console),
    
    fallback_info(),
    %% {ok,_} = ct_telnet:expect(console, "login:", 
    %%  			      [{timeout,120000}, no_prompt_check]),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:disconnect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    %% %%%%
    %% %% Wait for success.
    %% %%%%
    %% [{"SUCCESS" = _A2, 
    %%  "activate" = _B2, 
    %%   _C2, 
    %%   _D2, _E2, ProgReport2}] =
    %% 	swm_test_lib:wait_for_progress_result(?NC_Session, 
    %% 					      MeId, 
    %% 					      ActionId_2, 
    %% 					      dummy),
    %% ct:pal("ProgReport: ~p",[ProgReport2]),

    %%%%
    %% Confirm
    %%%%
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					confirm),

    fallback_info(),

    %%%%
    %% Remove UP
    %%%%
    ct:pal("Remove all upgrade package, don't care about result.",[]),
    LabelList = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UpLabels : ~p", [LabelList]),

    lists:foreach(fun(RemLabel) ->
    			  ct:pal("Remove upgrade package : ~p",[RemLabel]),

    			  swm_test_lib:
    			      remove_upgrade_package(?NC_Session, 
    						     RemLabel),
    			  swm_test_lib:
    			      remove_up_check_allowed_result(?NC_Session,
    						MeId)
    		  end, LabelList),

    fallback_info(),

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

fallback_info() ->
    Cmds = ["df -aT",
%%%	    "lvs",
%%%	    "sfdisk -l /dev/sda",
	    "ls -la /opt/rcs_ee/mounts/boot",
	    "ls -la /software",
	    "ls -la /rcs/swm/home1",
	    "ls -la /rcs/swm/home1/sirpa",
	    "ls -la /rcs/swm/home1/sirpa/software",
	    "ls -la /rcs/swm/home2",
	    "ls -la /rcs/swm/home2/sirpa",
	    "ls -la /rcs/swm/home2/sirpa/software",
	    "ls -la /rcs/swm/archive",
	    "ls -la /rcs/swm/archive/*/",
	    "ls -la /rcs/swm/backup/"],
    [begin
	 Res = ?RPC_CALL(os, cmd, [Cmd]),
	 ct:log("~s ->~n~s", [Cmd, Res])
     end
     || Cmd <- Cmds].

%%%--------------------------------------------------------------------
%%% @doc Modify cxs.
%%% @end
%%%--------------------------------------------------------------------
modify_cxs(UgHook) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:modify_existing_cxs_up_xml(UgHook).

%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
create(_Config) ->
    ct:pal("sleep 60sec before create."),
    timer:sleep(60000),
    ct:pal("Create.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %% SW_Vers = swm_test_lib:
    %% 	get_specific_reportprogress_info(?CLI_Session, "SwVersionMain"),
    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    BeamPid = rct_rpc:call(rpc_1, os, getpid, [], 10000, noprint),
    ct:pal("### BeamPid : ~p", [BeamPid]),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    ct:pal("create",[]),
 
    OldActionId = no_check,
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),
    wait_progress(swm_test_lib:wait_for_swm_progress_result(?NC_Session,
							    MeId,
							    OldActionId,
							    dummy),
		  MeId),
    ok.

%%%--------------------------------------------------------------------
wait_progress([{"SUCCESS" = Result,
		"createUpgradePackage" = ActionName, 
		ProgressInfo, 
		ResultInfo, 
		_State,
		_ProgReport}],
	      _) ->
    ct:log("result:~p~n"
	   "actionName:~p~n"
	   "progressInfo:~p~n"
	   "resultInfo:~p~n", [Result,
			       ActionName, 
			       ProgressInfo, 
			       ResultInfo]),
    ok;
wait_progress([{"FAILURE",
		"createUpgradePackage", 
		"The action could not be completed", 
		[], 
		"FINISHED",
		_}],
	      _) ->
    ct:pal("Create fail due to package already exist. Continue.", []);
wait_progress([{"FAILURE",
		"removeUpgradePackage", 
		"Action complete", 
		"This upgrade package contains active software. It cannot be removed at this time.", 
		"FINISHED",
		ProgReport}],
	      MeId) ->
    ct:pal("Create on hold, removeUpgradePackage still executing...", []),
    {actionId, _, [OldActionId]} = lists:keyfind(actionId, 1, ProgReport),
    wait_progress(swm_test_lib:wait_for_swm_progress_result(?NC_Session,
							    MeId,
							    OldActionId,
							    dummy),
		  MeId);
wait_progress(Result, _) ->
    ct:pal("createUpgradePackage: ~p", [Result]),
    ct:fail(Result).

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
prepare(_Config) ->
    ct:pal("Prepare.",[]),

    BeamPid = rct_rpc:call(rpc_1, os, getpid, [], 10000, noprint),
    ct:pal("### BeamPid : ~p", [BeamPid]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    ct:pal("prepare: : ~s",[Label]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					prepare),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
verify(_Config) ->
    ct:pal("Verify.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    ct:pal("verify: : ~s",[Label]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					verify),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
activate(_Config) ->
    ct:pal("activate.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    ct:pal("activate: : ~s",[Label]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					activate),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
confirm(_Config) ->
    ct:pal("Confirm.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    ct:pal("confirm: : ~s",[Label]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					confirm),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
remove(_Config) ->
    ct:pal("remove.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, 
					"SUCCESS", 
					MeId, 
					Label),

    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
disable_max_up_check(_Config) ->
    swm_test_lib:disable_max_up_check(coli).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
enable_max_up_check(_Config) ->
    swm_test_lib:enable_max_up_check(coli).

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_all_loop_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R11A/1
%%% 
%%% @doc == Upgradde. performs comple upgrades. Can be used in loop. Updates of cxl label in to up is handled in TC.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_all_loop_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R11A/1').

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
%%% R2A/3      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/4      2014-05-23 etxivri     Update in short_group_1 so it can be run
%%%                                   in xl_rob_swm_spec_all.
%%% R2A/5      2014-09-08 etxivri     Update get the label to be used.
%%% R2A/6      2014-09-09 etxivri     Update modify cxs.
%%% R3A/2      2015-01-29 etxivri     Update to prevent error in ct-shell.
%%% R3A/3      2015-01-30 etxivri     Decreased nr of test_all in group_1
%%%                                   due to some TCU have not enough space.
%%% R3A/4      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/5      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-07-07 etxkols     add modify cxs to handle when several ug
%%%                                   runs in a loop and rev increases.
%%% R4A/2      2015-10-21 etxivri     Update modify cxs.   
%%% R4A/3      2015-12-01 etxivri     Update due to new behaviour.
%%% R4A/4      2015-12-03 etxivri     Add coli hook.
%%% R4A/5      2015-12-03 etxivri     Update due to max-up-check is enabled 
%%%                                   after restart.
%%% R5A/1      2016-01-13 etxivri     Make it more robust.
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-11-16 etxkivri    update to get correct active sw 
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
	 groups/0]).
-export([test_all/1,
	 build_to_up/1,
	 create/1, 
	 prepare/1, 
	 verify/1, 
	 activate/1, 
	 confirm/1,
	 remove/1,
	 fake_up/1,
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
    %% ct:log("# init per suite. ~n"
    %% 	   "Create cxps that shall be used for UG."), 
    %% swm_test_lib:build_valid_ug_packakage(?NC_Session),
    %% build_same_ug_package_add_char(?NC_Session),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),
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
	    enable_max_up_check(Config)
	    %% %% Remove created upgrade package.
	    %% swm_test_lib:check_nc_session(?NC_Session),
	    %% UPs = swm_test_lib:get_ups(?NC_Session),
	    %% Label = swm_test_lib:get_lowest_label(UPs),
    	    %% swm_test_lib:remove_upgrade_package(?NC_Session, Label)
		
    end,
    
    ok.

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
     test_all,
     test_all].
    %% [create, prepare, verify, activate, confirm, remove].
    %% [create, remove].
    %% [create].
    %% [fake_up].

groups() ->
    [{group_1,[],[%% build_to_up,
		  %% test_all, %% Some TCU have not enough disc for 10 ug.
		  %% test_all,
		  %% test_all,
		  %% test_all,
		  %% test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all,
		  test_all
		 ]},
     {short_group_1,[],[%% build_to_up,
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
test_all(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Modify version in cxs to UP.
    %%%%
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    %% %% swm_test_lib:modify_cxs(SW_Vers, 1),
    %% modify_cxs_on_cs_build(SW_Vers, 1),
    modify_cxs(ug1),

    DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df = string:tokens(DF, " \n"),
    ct:pal("### Df: ~p", [Df]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),


    case swm_test_lib:
	wait_for_swm_progress_result(?NC_Session, MeId, no_check, dummy) of
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
	[{"FAILURE",
	  "createUpgradePackage", 
	  "The action could not be completed", 
	  [], 
	  "FINISHED", _}] ->
	    ct:pal("Create fail due to package already exist. Continue.",[]);
	Result ->
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end,



    %%%%
    %% Get Latest UP
    %%%%
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
    %% ug_action(MeId, Label, activate, ErlNode),
    net_kernel:disconnect(ErlNode),
    ct:log("~p", [time()]),
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					activate),
    ct:log("~p", [time()]),

    %%%%
    %% Confirm
    %%%%
    %% ug_action(MeId, Label, confirm, ErlNode),
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					confirm),

    DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df1 = string:tokens(DF1, " \n"),
    ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Modify cxs.
%%% @end
%%%--------------------------------------------------------------------
modify_cxs(UgHook) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:modify_existing_cxs_up_xml(UgHook).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
set_start_ug_rev_in_ug_top_xml(_Config)  ->
    MeId = swm_test_lib:get_me_id(?NC_Session),
    swm_test_lib:set_start_ug_rev_in_ug_top_xml(?NC_Session, MeId).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
build_same_ug_package(_Config) ->
    swm_test_lib:build_same_ug_package(?NC_Session, ug1).

%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
fake_up(_Config) ->
    %% test_server:break("A"),
    %% Construct a CXS label.

    [SW_Vers | _] = swm_test_lib:get_ups(?NC_Session),
    ct:pal("SW_Vers:~n~p~n",[SW_Vers]),

    [CXS, Index, ALabel] = string:tokens(SW_Vers, "/-"),
    ct:pal("CXS:~p, Index: ~p, ALabel: ~p ~n",[CXS, Index, ALabel]),
    CXS_LABEL = CXS++"_"++Index++"-"++ALabel,

    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),
    StpName = lists:last(string:tokens(UGPath, "/")),
    ct:log("StpName: ~p", [StpName]),

    ct:log("# Run script that step unpack CXS and step REV nr with 1, ~n"
    	   "then build CXS again. ~n"
    	   "After that it will be moved to UGPath unpacked.",[]),
    %% CMD1 = "rcs_upmod_and_upgradeprep.sh -n " ++ StpName ++ " -r " ++ CXS_LABEL,
    CMD1 = "rcs_upmod_and_upgradeprep.sh -n " ++ StpName ++ " -m $RCT_TOP/test/suites/SWM/bin/swm_mod_fake_a5.sh -r " ++ CXS_LABEL,
    ct:pal("CMD:~n~p~n",[CMD1]),

    os:cmd(CMD1),

    B = os:cmd("ls -l "++UGPath),
    C = string:tokens(B,"\n"),
    ct:log("ls -l ~p:~n~p~n",[UGPath, C]),

    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]).



%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
create(_Config) ->
    ct:pal("Create.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),


    %% SW_Vers = swm_test_lib:
    %% 	get_specific_reportprogress_info(?CLI_Session, "SwVersionMain"),
    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df = string:tokens(DF, " \n"),
    ct:pal("### Df: ~p", [Df]),

    BeamPid = rct_rpc:call(rpc_1, os, getpid, [], 10000, noprint),
    ct:pal("### BeamPid : ~p", [BeamPid]),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    ct:pal("create",[]),
    swm_test_lib:ug_create_match_result(?NC_Session, 
    					"SUCCESS", 
    					?SftpHost, 
    					?SftpUser, 
    					?SftpPassword, 
    					UGPath, 
    					MeId), 

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
prepare(_Config) ->
    ct:pal("Prepare.",[]),

    DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df = string:tokens(DF, " \n"),
    ct:pal("### Df: ~p", [Df]),

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

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
verify(_Config) ->
    ct:pal("Verify.",[]),

    %% DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df = string:tokens(DF, " \n"),
    %% ct:pal("### Df: ~p", [Df]),

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

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
activate(_Config) ->
    ct:pal("activate.",[]),

    %% DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df = string:tokens(DF, " \n"),
    %% ct:pal("### Df: ~p", [Df]),

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

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
confirm(_Config) ->
    ct:pal("Confirm.",[]),

    %% DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df = string:tokens(DF, " \n"),
    %% ct:pal("### Df: ~p", [Df]),

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

    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
remove(_Config) ->
    ct:pal("remove.",[]),

    %% DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df = string:tokens(DF, " \n"),
    %% ct:pal("### Df: ~p", [Df]),

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


    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

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

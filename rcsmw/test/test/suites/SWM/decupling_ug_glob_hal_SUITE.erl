%% coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	decupling_ug_glob_hal_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R7A/R8A/7
%%%
%%% @doc == Test Suite for Upgrade Mechanism, using HAL and Global. ==
%%% <br/><br/>
%%% @end

-module(decupling_ug_glob_hal_SUITE).
-vsn('/main/R5A/R6A/R7A/R8A/7').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R2A/2      2014-03-21 etxivri     Created
%%% R2A/2      2014-04-05 etxivri     Cleanup
%%% R2A/3      2014-04-05 etxivri     Update to work until gmf is fixed.
%%% R2A/5      2014-04-06 etxivri     Changed some ct:pal to ct:log
%%% R2A/6      2014-04-06 etxivri     Update to handle mounted /rcs/swm/sda1
%%% R2A/8      2014-04-15 etxivri     Update due to new behaviour.
%%% R2A/9      2014-04-18 etxivri     Changed index names to be used
%%% R6A/1      2016-05-03 etxivri     Update due to hwcategory changed to OTHER
%%% R6A/2      2016-05-04 etxivri     Update to use correct upgradeprep.sh,
%%%                                   From 17A it will use zip cxs. 
%%% R6A/3      2016-06-09 etxivri     Change match for BB5216 to BB521_S.
%%% R6A/5      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R6A/6      2016-09-08 etxivri     Update to cleanup in tmpdir when use hal maker.
%%% R7A/1      2016-10-03 etxivri     Update to use ne UP xml format.
%%% R7A/2      2016-10-04 etxivri     Update edoc error.
%%% R8A/1      2016-11-24 etxivri     Update due to new behaviour.
%%%                                   After node restart a check that hal algohrithm is ok
%%%                                   And after restart an umount of /rcs/swm/sda1 is 
%%%                                   needed to be able to sen faked HAL dir that has been
%%%                                   linked to /rcs/swm/sda1.
%%% R8A/2      2017-01-09 etxivri     Update check to be more robust.
%%% R8A/3      2017-01-10 etxivri     More update of check.
%%% R8A/4      2017-01-25 etxivri     Updated due to new behaviour.
%%% R8A/5      2017-02-01 etxivri     bugfix
%%% R8A/7      2017-03-22 etxivri     update cleanup to be more careful.
%%%                                   And update what board that can be used
%%%                                   due to not destroy board with real hal.
%%%
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
-export([create/1,
	 prepare/1, 
	 verify/1, 
	 activate/1, 
	 confirm/1,
	 ug_all/1,
	 remove/1,

	 build_valid_up/1,
	 modify_cxs/1,

	 upgrade_prep_zip_with_gup/1,
	 modify_gup/1,

	 set_not_suported_board_in_gup/1,
	 set_suported_board_in_gup/1,
    
	 update_glob_with_indx_HALTESTA/1,
	 update_glob_with_indx_HALTESTB/1,

	 mkdir_haltest/1,
	 mkdir_halswp/1,

	 hal_maker_valid/1, %% Use sw from tftp dir
	 hal1_maker_HALTESTA/1, %% ln -s to sda1
	 hal2_maker_HALTESTB/1, %% ln -s to sda1
	 hal1_maker_HALTESTA_to_sda1/1,  %% sda1 is mounted on haltest dir
	 hal2_maker_HALTESTB_to_sda1/1,  %% sda1 is mounted on haltest dir
	 hal1_maker_HALTESTA_to_sda1_not_cs/1,  %% Don't use EE and MW due to disc shortage when scp to node.
	 hal2_maker_HALTESTB_to_sda1_not_cs/1,  %% Don't use EE and MW due to disc shortage when scp to node.

	 check_ug_result_416_A8/1,
	 check_ug_result_416_A8_test2/1,
	 check_ug_result_416_A9/1,
	 check_ug_result_416_A10/1,

	 %% remove_hal_dir_on_node/1,
	 check_selected_vs_installed_cxps/1,

	 break/1,
	 umount_rcs_swm_sda1/1,
	 mount_sda1_to_haltest_dir/1,
	 umount_haltest_dir/1,
	 cleanup_hal_dir_after_mount_sda1/1,
	 mount_dev_sda1_to_rcs_swm_sda1/1,
	 
	 %% Cleanup
	 install_org_up_to_cleanup/1,

	 %% Not Needed
	 %% scp_org_to_up_xml_to_arch_B/1,
	 %% scp_org_to_up_xml_to_arch_A/1,
	 %% link_hal_dir_to_swm_sda1/1,

	 sleep_5_min/1,
	 sleep_10_min/1,
	 disable_max_up_check/1,
	 enable_max_up_check/1
	]).

-define(NC_Session, nc1).
%% -define(CLI_Session, cli1).

-define(UgCxsXml, "cxs101549_*-up.xml").
-define(HwCompIndx, "16B").
-define(TmpHalXml, "cxs101549-hal.xml").

-define(HalSwp, "halswp"). %% This is created by swm
-define(SWM_HAL_DIR, "/rcs/swm/sda1/halswp/").
-define(SWM_MOUNT_DIR, "/rcs/swm/sda1").
-define(HAL_DIR, "/rcs/haltest/").
-define(HalTestDir, "/rcs/haltest").
-define(HalTestHalswpDir, "/rcs/haltest/halswp/").
-define(HalSwpDir, "/rcs/haltest/halswp").

%% -define(HAL_DIR, "/tmp/haltest/").
%% -define(HalTestDir, "/tmp/haltest").
%% -define(HalTestHalswpDir, "/tmp/haltest/halswp/").


%% Need for gupmaker
-define(EndFileDir, "/proj/rcs-tmp/hw_sw_dec/upgrade/need_for_ug_xml/").
-define(ExtraRuCxps, "/proj/rcs-tmp/hw_sw_dec/upgrade/extra_cxps/g2_*").
-define(Console, console).

%% -define(IGNORELIST , ["BoardType not supported by neither HalUP nor GlobalUP"]).

-define(NewHalRev1, "RCSHALTEST1").
-define(NewHalRev2, "RCSHALTEST2").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    case check_if_vc_board() of
	"yes" -> [{ct_hooks, [{rct_htmllink,[]},
			      %% {rct_cli, {cli1,
			      %% 		 [{user, "SysAdminTest"}, 
			      %% 		  {password, "SysAdminTest"},
			      %% 		  manual_connect]}},
			      %% {cth_conn_log,[]},
			      {rct_upgrade,ug1},
			      {rct_netconf, {nc1, man_auth}},
			      {rct_rs232,console}
			     ]}];
	_->
	    [{timetrap, {hours, 2}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_rs232,console},
			 {cth_conn_log,[]},
			 {rct_power,node},
			 {rct_upgrade,ug1},
			 {rct_coli, {coli, [manual_connect]}},
			 {rct_logging, 
			  {upgrade, 
			   [{erlang,{["ERROR REPORT","CRASH REPORT"],
				     [ 
				     ]}}]}},
			 %% {rct_logging, 
			 %%  {upgrade, 
			 %%   [{erlang,{["ERROR REPORT","CRASH REPORT"],
			 %% 	     [?IGNORELIST]}}
			 %% 	     ]}},
			 {rct_netconf,nc1},
			 {rct_scp, [{1, scp}]}
			]}]
    end.

%% @hidden
init_per_suite(Config) ->
    %% test_server:break("Start"),
    MeId = swm_test_lib:get_me_id(?NC_Session),

    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    ct:log("# Hwa: ~p", [Hwa]),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("# BoardType: ~p", [BoardType]),
    case BoardType of
	BoardType when BoardType == "dus5201"; 
		       BoardType == "dus5301";
		       BoardType == "dus3301"->
	    ok;
	_Other ->
	    ct:fail(" ## Board is not suported in this SUITE")
    end,

    TftpDir = ct:get_config({Hwa, tftpboot}),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    TmpDir = ?config(priv_dir, Config),
    ct:pal("Tmp_Dir:~n~p~n",[TmpDir]),

    {OrgProdNr, OrgProdRev} = decupling_lib:get_hw(rpc_1),
    ct:pal("Add orginal HW data to Config table"),
    ct:pal("### OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("### OrgProdRev: ~p", [OrgProdRev]),

    case OrgProdNr of
	"KDU137848/1" -> %% dus53
	    case OrgProdRev of
		"P1C" ->
		    ct:pal("Board is a dus53 P1C with no real HAL. ~n"
			   "Ok to continue the test.");
		_Other1 ->
		    ct:fail("Board is a dus53 with real HAL. ~n"
			    "Not OK to be used for test. Abort the test!")
	    end;
	"KDU137847/1" -> %% dus33
	    case OrgProdRev of
		"P1B" ->
		    ct:pal("Board is a dus33 P1B with no real HAL. ~n"
			   "Ok to continue the test.");
		_Other2 ->
		    ct:fail("Board is a dus33 with real HAL. ~n"
			    "Not OK to be used for test. Abort the test!")
	    end;
	_OtherX ->
	    ct:pal("Board is a dus52 with no real HAL. ~n"
		   "Ok to be used for test.")
    end,

    %% %% umount /rcs/swm/sda1
    umount_rcs_swm_sda1(),
    LS_df = send_rpc_os_cmd("df -aT"),
    ct:log("LS_df : ~n ~p", [fix_str(LS_df, "\n")]),
    ct:pal(" # BoardType # ~p", [BoardType]),
    timer:sleep(2000),
    case BoardType of
    	"dus5201" -> %% new behaviour, /dev/sda1 mounts on another dir.
    	    umount_ee_stats();
    	_ ->
    	    ok
    end,
    %% test_server:break("A"),
    %% %%%%New behaviour with SWM, CNX9012637-R8S01. halswp dir need to be created.
    %% %% mkdir_halswp(),

    %% %% cleanup_hal_dirs(), %% To ensure cleanuped before tc start

    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    %%% Get build CXS label to be used from Jenkins
    CxsLabel = ct:get_config({jenkins_config, cxp}), %% The key is cxp.
    %% CxsLabel = "CXS101549_8-R9T02",

    [{nodeName, NodeName},
     {me_id, MeId},
     {board_type, BoardType},
     {ug_path, UGPath},
     {tftp_dir, TftpDir},
     {tmp_dir, TmpDir},
     {cxs_label, CxsLabel},
     {org_product_number, OrgProdNr},
     {org_prod_rev, OrgProdRev} | Config].


sleep_5_min(_Config) ->
    ct:pal("Start sleep 5 min"),
    timer:sleep(300000),
    ct:pal("Stop sleep").

sleep_10_min(_Config) ->
    ct:pal("Start sleep 10 min"),
    timer:sleep(600000),
    ct:pal("Stop sleep").

install_org_up_to_cleanup(Config) ->
    rctprep_zip_glob_to_be_used(Config),
    ai_install(Config).


umount_rcs_swm_sda1() ->
    %% %% rct_rpc:call(rpc_1, swmLib, umount_sda1, [], 10000, print),
    rct_rpc:call(rpc_1, os, cmd, ["sudo cup --umount "++?SWM_MOUNT_DIR], 10000, 
    		 print),
    timer:sleep(1000),
    LS = rct_rpc:call(rpc_1, os, cmd, ["ls "++?SWM_MOUNT_DIR], 10000, noprint),
    ct:pal("# # ls ~p: ~p", [?SWM_MOUNT_DIR, fix_str(LS, "\n ")]).
    


umount_rcs_swm_sda1(Config) ->
    ct:pal("Config: ~p", [Config]),
    rct_rpc:call(rpc_1, os, cmd, ["sudo cup --umount "++?SWM_MOUNT_DIR], 10000, 
		 print),
    timer:sleep(1000),
    LS = rct_rpc:call(rpc_1, os, cmd, ["ls "++?SWM_MOUNT_DIR], 10000, noprint),
    ct:pal("## ls ~p: ~p", [?SWM_MOUNT_DIR, fix_str(LS, "\n ")]),
    LS2 = rct_rpc:call(rpc_1, os, cmd, ["ls "++?SWM_HAL_DIR++"*/*"], 
		       10000, noprint),
    ct:pal("## ls2 ~p: ~p", [?SWM_HAL_DIR, fix_str(LS2, "\n ")]),

    LS_df = send_rpc_os_cmd("df -lT"),
    ct:log("LS_df : ~n ~p", [fix_str(LS_df, "\n")]),
    BoardType = proplists:get_value(board_type, Config),
    ct:pal("BoardType: ~p", [BoardType]),
    timer:sleep(5000),
    case BoardType of
	"dus5201" -> %% new behaviour, /dev/sda1 mounts on another dir.
	    umount_ee_stats();
	_ ->
	    ok
    end.

umount_ee_stats() ->
    %% new behaviour, /dev/sda1 mounts on another dir.
    LS3 = send_rpc_os_cmd("ls /opt/rcs_ee/mounts/stats"),
    ct:log("Ls befor umount  /opt/rcs_ee/mounts/stats: ~n ~p", 
	   [fix_str(LS3, "\n ")]),
    Cmd1="sudo cup --umount /opt/rcs_ee/mounts/stats",
    send_rpc_os_cmd(Cmd1),
    timer:sleep(2000),
    LS4 = send_rpc_os_cmd("ls /opt/rcs_ee/mounts/stats"),
    ct:log("Ls after umount /rcs/swm/sda1 : ~n ~p", 
	   [fix_str(LS4, "\n ")]),
    unmounted = 
	check_if_dir_is_unmounted("/opt/rcs_ee/mounts/stats").

mount_sda1_to_haltest_dir(_Config) ->
    %% Cmd = "sudo cup --mount '-o ro /dev/sda1 "++?HalTestDir++"'",
    %% Mount rw
    ct:pal("mount /dev/sda1 on "++?HalTestDir),
    Cmd = "sudo cup --mount '-o rw /dev/sda1 "++?HalTestDir++"'",
    send_rpc_os_cmd(Cmd),
    LS = send_rpc_os_cmd("ls "++?HalTestDir),
    timer:sleep(2000),
    ct:log("## ls ~p: ~p", [?HalTestDir, fix_str(LS, "\n ")]),
    case re:run(LS, "networkloader") of
	{match, _} ->
	    ct:log("/dev/sda1 is not mounted : ~p", [?HalTestDir]);
	nomatch ->
	    ct:fail("/dev/sda1 is not mounted on haltest dir, fail the TC. ")
    end.


mount_dev_sda1_to_rcs_swm_sda1(_Config) ->
    %% Cmd = "sudo cup --mount '-o ro /dev/sda1 /rcs/swm/sda1'",
    %% Mount rw
    %% %% rct_rpc:call(rpc_1, swmLib, mount_sda1, [], 10000, print),
    ct:pal("mount /dev/sda1 on "++?SWM_MOUNT_DIR),
    Cmd = "sudo cup --mount '-o ro /dev/sda1 "++?SWM_MOUNT_DIR++"'",
    send_rpc_os_cmd(Cmd),
    LS = send_rpc_os_cmd("ls "++?SWM_MOUNT_DIR),
    timer:sleep(5000),
    ct:log("## ls ~p: ~p", [?SWM_MOUNT_DIR, fix_str(LS, "\n ")]),
    case re:run(LS, "networkloader") of
	{match, _} ->
	    ct:log("/dev/sda1 is not mounted : ~p", [?SWM_MOUNT_DIR]);
	nomatch ->
	    ct:fail("/dev/sda1 is not mounted on haltest dir, fail the TC. ")
    end.

umount_haltest_dir(_Config) ->
    %% rct_rpc:call(rpc_1, os, cmd, ["sudo cup --umount "++?SWM_MOUNT_DIR], 10000, 
    %% 		 print),
    Cmd="sudo cup --umount "++?HalTestDir,
    send_rpc_os_cmd(Cmd),
    timer:sleep(2000),
    LS = send_rpc_os_cmd("ls "++?HalTestDir),
    ct:log("## ls ~p: ~p", [?HalTestDir, fix_str(LS, "\n ")]).


send_rpc_os_cmd(Cmd) ->
    send_rpc_os_cmd(Cmd, 10000, print).
send_rpc_os_cmd(Cmd, Timeout, PrintOpt) ->
    Answ = rct_rpc:call(rpc_1, os, cmd, [Cmd], Timeout, PrintOpt),
    ct:log("Rpc Answ : ~p", [Answ] ),
    Answ.

check_if_dir_is_unmounted(Dir) ->
    LS = send_rpc_os_cmd("ls "++Dir),
    timer:sleep(2000),
    ct:log("## ls ~p: ~p", [Dir, fix_str(LS, "\n ")]),
    case re:run(LS, "networkloader") of
	{match, _} ->
	    mounted;
	nomatch ->
	    unmounted
    end.

%%%--------------------------------------------------------------------
disable_max_up_check(_Config) ->
    swm_test_lib:disable_max_up_check(coli).
%%%--------------------------------------------------------------------
enable_max_up_check(_Config) ->
    swm_test_lib:enable_max_up_check(coli).
%%%--------------------------------------------------------------------

%% @hidden
end_per_suite(Config) ->
    OrgProdNr = proplists:get_value(org_product_number, Config),
    OrgProdRev = proplists:get_value(org_prod_rev, Config),
    ct:pal("# # OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),
    decupling_lib:check_expected_hw_id(rpc_1, OrgProdNr, OrgProdRev),
    ct:log("## end_per_suite: ~n~p.  \n", [Config]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("## ~p ##", [TestCase]),
    Config.
end_per_testcase(hal_maker_valid, Config) ->
    cleanup_after_tc(Config);
end_per_testcase(_TestCase, Config) ->
    ct:log("## end_per_testcase: ~n~p.  \n", [Config]),
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
	    ok;
    	{failed, Reason}  ->
	    %% test_server:break("TC failed - check"),
	    timer:sleep(120000), %% if node is restarting
	    cleanup_hal_dir_after_mount_sda1(Config),
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,
    ok.
%% @hidden group_A416_A8_1
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, Config) ->
    ct:log("## end_per_group: ~n~p.  \n", [Config]),
    GrouResult = proplists:get_value(tc_group_result, Config),
    case proplists:get_value(failed, GrouResult) of
    	[] -> %% No TC failed.
	    ok;
	[Failed_TC]  ->
	    %% cleanup_hal_dir_after_mount_sda1(Config),
	    ct:pal("Failed TestCase : ~p .  \n", [Failed_TC])
    end,
    LS_df = send_rpc_os_cmd("df -aT"),
    ct:log("End per group. LS_df : ~n ~p", [fix_str(LS_df, "\n")]),
    ok.

cleanup_after_tc(Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
	    Tmp_Dir = proplists:get_value(tmp_dir, Config),
	    ct:pal("Cleanup in tmp dir: ~n ~p", [Tmp_Dir]),
	    CMD1 = "rm -rf "++Tmp_Dir++"*.cxs ",
	    os:cmd(CMD1),
	    CMD2 = "rm -rf "++Tmp_Dir++"*.cxp ",
	    os:cmd(CMD2),
	    CMD3 = "rm -rf "++Tmp_Dir++"*.xml ",
	    os:cmd(CMD3),
	    ct:pal("Ls tmp_dir : ~n~p", [os:cmd("ls "++Tmp_Dir)]),
	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end.


%% cleanup_hal_dirs() -> %% Do not matter if thir already is cleaned.
%%      cleanup().
%% cleanup() ->
%%     ct:pal("#End# Cleanup in /rcs/haltest"),
%%     rct_rpc:call(rpc_1, os, cmd, ["rm -rf "++?HAL_DIR++?HalSwp], 
%% 		 10000,  print),
%%     LsTmpHalDir = rct_rpc:call(rpc_1, os, cmd, ["ls /rcs/haltest"], 10000, 
%% 			       print),
%%     ct:pal("#End# ls /rcs/ : ~n~p", [fix_str(LsTmpHalDir, "\n ")]),
%%     LsSwmHalDir = rct_rpc:call(rpc_1, os, cmd, 
%% 			       ["ls "++?SWM_MOUNT_DIR], 
%% 			       10000, print),
%%     ct:pal("#End# ls ~p: ~n~p", [?SWM_MOUNT_DIR, fix_str(LsSwmHalDir, "\n ")]).

%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [create,
     prepare, 
     verify, 
     activate, 
     confirm].

groups() ->
    [{group_1, [sequence],[
			  ]},
     %% gup not suporting hw, index HALTESTB is used. 
     %% contentinfo from global is used.
     {group_A416_A8_1, [sequence],[%% Need HAL mounted on sda1
				   umount_rcs_swm_sda1,
				   mkdir_haltest,
				   mount_sda1_to_haltest_dir,
				   mkdir_halswp,
				   upgrade_prep_zip_with_gup,
				   %% modify_gup,
				   modify_cxs,
				   %% %% update_glob_with_indx_HALTESTB,
				   %% %% set_not_suported_board_in_gup,
				   hal1_maker_HALTESTA_to_sda1_not_cs,
				   hal2_maker_HALTESTB_to_sda1,
				   umount_haltest_dir,
				   update_glob_with_indx_HALTESTB,
				   set_not_suported_board_in_gup, %% Need to be here.
				   mount_dev_sda1_to_rcs_swm_sda1,
				   %% break,
				   create,prepare,verify,activate,confirm,
				   %% break,
				   check_ug_result_416_A8,
				   sleep_5_min, %% To avoit unexpected audit error
				   cleanup_hal_dir_after_mount_sda1
				  ]},
     %% gup not suporting hw, index HALTESTA is used.
     %% contentinfo from global is used.
     {group_A416_A8_2, [sequence],[%% Need HAL mounted on sda1
				   umount_rcs_swm_sda1,
				   mkdir_haltest,
				   mount_sda1_to_haltest_dir,
				   mkdir_halswp,
				   upgrade_prep_zip_with_gup,
				   modify_cxs,
				   hal1_maker_HALTESTA_to_sda1,
				   hal2_maker_HALTESTB_to_sda1_not_cs,
				   umount_haltest_dir,
				   update_glob_with_indx_HALTESTA,
				   set_not_suported_board_in_gup, %% Need to be here.
				   mount_dev_sda1_to_rcs_swm_sda1,
				   create,prepare,verify,activate,confirm,
				   check_ug_result_416_A8_test2,
				   sleep_5_min, %% To avoit unexpected audit error
				   cleanup_hal_dir_after_mount_sda1
				  ]},
     %% gup suport hw, index from gup is used. 
     %% HAL index is not match gup index.
     {group_A416_A9, [sequence],[%%% Need HAL mounted on sda1
				 %% build_valid_up,
				 umount_rcs_swm_sda1,
				 mkdir_haltest,
				 mount_sda1_to_haltest_dir,
				 mkdir_halswp,
				 upgrade_prep_zip_with_gup,
				 modify_gup,
				 modify_cxs,
				 update_glob_with_indx_HALTESTB,
				 hal1_maker_HALTESTA_to_sda1,
				 umount_haltest_dir,
				 mount_dev_sda1_to_rcs_swm_sda1,
				 create,prepare,verify,activate,confirm,
				 check_ug_result_416_A9,
				 sleep_5_min, %% To avoit unexpected audit error
				 cleanup_hal_dir_after_mount_sda1
				]},
     %% Run this group after A416_A9
     %%% gup suport hw, index match both in gup and one of the hal. HALTESTB.
     {group_A416_A10, [sequence],[umount_rcs_swm_sda1,
				  mkdir_haltest,
				  mount_sda1_to_haltest_dir,
				  mkdir_halswp,
				  upgrade_prep_zip_with_gup,
				  modify_gup,
				  modify_cxs,
				  update_glob_with_indx_HALTESTB,
				  hal1_maker_HALTESTA_to_sda1_not_cs,
				  hal2_maker_HALTESTB_to_sda1,
				  umount_haltest_dir,
				  mount_dev_sda1_to_rcs_swm_sda1,
				  create,prepare,verify,activate,confirm,
				  check_ug_result_416_A10,
				  sleep_5_min, %% To avoit unexpected audit error
				  cleanup_hal_dir_after_mount_sda1
				]}
    ].

break(_Config) ->
    test_server:break("### BREAK ###").
%%--------------------------------------------------------------------
%% @doc
%% @spec ug_all(Config) -> ok
%% @end
%%--------------------------------------------------------------------
ug_all(Config) ->
    create(Config),
    prepare(Config), 
    verify(Config), 
    activate(Config), 
    confirm(Config).

%%--------------------------------------------------------------------
%% @doc
%% @spec mkdir_haltest(Config) -> ok
%% @end
%%--------------------------------------------------------------------
mkdir_haltest(_Config) ->
    %% Mkdir /rcs/haltest/
    rct_rpc:call(rpc_1, os, cmd, ["mkdir "++?HAL_DIR], 10000, 
			 print),
    LS = rct_rpc:call(rpc_1, os, cmd, ["ls /rcs/"], 10000, noprint),
    Ls = fix_str(LS, "\n "),
    ct:pal("ls /rcs: ~p", [Ls]),
    case Ls of
	[] ->
	    ct:fail("Tc fail due to no haltest dir was created");
	_Other ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec mkdir_halswp(Config) -> ok
%% @end
%%--------------------------------------------------------------------
mkdir_halswp(_Config) ->
    %% Mkdir /rcs/haltest/halswp
    %% rct_rpc:call(rpc_1, os, cmd, ["mkdir "++?HAL_DIR++?HalSwp], 10000, 
    %% 			 print),
    rct_rs232:login(?Console),
    ct:pal("mkdir "++?HAL_DIR++?HalSwp),
    A = ct_telnet:send(?Console, "mkdir "++?HAL_DIR++?HalSwp), 
    ct:pal(" A : ~p", [A]),
    timer:sleep(3000),
    LS = rct_rpc:call(rpc_1, os, cmd, ["ls "++?HAL_DIR], 10000, print),
    Ls = fix_str(LS, "\n "),
    ct:pal("ls ~p: ~p", [?HAL_DIR, Ls]),
    case re:run(Ls, "halswp") of
	nomatch ->
	    ct:fail("Tc fail due to no /rcs/haltest/halswp dir was NOT created");
	{match, _} ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec build_valid_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
build_valid_up(_Config) ->
    ct:log("# build valid to up. ~n"
	    "Create cxps that shall be used for UG."),
    swm_test_lib:build_valid_ug_packakage(?NC_Session).

%%--------------------------------------------------------------------
%% @doc
%% @spec modify_cxs(Config) -> ok
%% @end
%%--------------------------------------------------------------------
modify_cxs(Config) ->
    MeId = proplists:get_value(me_id, Config),
    step_rev_in_cxs(MeId).


%%--------------------------------------------------------------------
%% @doc
%% Will make a correct HAL and CXP from CXS that is installed 
%% from /proj/rcs-tmp/tftpboot/. Using index 16B
%% @spec hal_maker_valid(Config) -> ok
%% @end
%%--------------------------------------------------------------------
hal_maker_valid(Config) ->
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    Index = ?HwCompIndx,
    hal_maker(Config, Tmp_Dir, Index),

    GrepList = fix_str(os:cmd("grep CXS101549_* "++Tmp_Dir++"*-hal.xml"), 
		   "\"\n </>"),
    ct:pal("GrepList : ~n~p", [GrepList]),
    HalDirName = decupling_lib:get_name_id_version(GrepList),
        ct:pal("HalDirName : ~n~p", [HalDirName]),

    HalToPath = ?HAL_DIR++HalDirName,
    ct:pal("HalToPath : ~n~p", [HalToPath]),
    scp_to_node(Config, HalToPath),

    softlink_haltest_to_swm_dir(HalToPath),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec hal1_maker_HALTESTA(Config) -> ok
%% @end
%%--------------------------------------------------------------------
hal1_maker_HALTESTA(Config) ->  
    Index = "HALTESTA",
    HalNameAdd = "hal1_",
    %% NewHalRev = "HAL1",
    generic_index_hal_maker(Config, Index, HalNameAdd, ?NewHalRev1),
    remove_hal_xml_from_tmp_dir(Config, HalNameAdd).

hal1_maker_HALTESTA_to_sda1(Config) ->  
    Index = "HALTESTA",
    HalNameAdd = "hal1_",
    generic_index_hal_maker(Config, Index, HalNameAdd, ?NewHalRev1, "mount_sda1"),
    remove_hal_xml_from_tmp_dir(Config, HalNameAdd).

hal1_maker_HALTESTA_to_sda1_not_cs(Config) ->  
    Index = "HALTESTA",
    HalNameAdd = "hal1_",
    generic_index_hal_maker(Config, Index, HalNameAdd, ?NewHalRev1, "mount_sda1", "not_cs"),
    remove_hal_xml_from_tmp_dir(Config, HalNameAdd).
%%--------------------------------------------------------------------
%% @doc
%% @spec hal2_maker_HALTESTB(Config) -> ok
%% @end
%%--------------------------------------------------------------------
hal2_maker_HALTESTB(Config) ->  
    Index = "HALTESTB",
    HalNameAdd = "hal2_",
    %% NewHalRev = "HAL2",
    generic_index_hal_maker(Config, Index, HalNameAdd, ?NewHalRev2),
    %% remove hal from tmp dir, to clean up before next hal maker.
    remove_hal_xml_from_tmp_dir(Config, HalNameAdd).

hal2_maker_HALTESTB_to_sda1(Config) ->  
    Index = "HALTESTB",
    HalNameAdd = "hal2_",
    generic_index_hal_maker(Config, Index, HalNameAdd, ?NewHalRev2, "mount_sda1"),
    %% remove hal from tmp dir, to clean up before next hal maker.
    remove_hal_xml_from_tmp_dir(Config, HalNameAdd).

hal2_maker_HALTESTB_to_sda1_not_cs(Config) ->  
    Index = "HALTESTB",
    HalNameAdd = "hal2_",
    generic_index_hal_maker(Config, Index, HalNameAdd, ?NewHalRev2, "mount_sda1", "not_cs"),
    %% remove hal from tmp dir, to clean up before next hal maker.
    remove_hal_xml_from_tmp_dir(Config, HalNameAdd).

remove_hal_xml_from_tmp_dir(Config, HalNameAdd) ->
    ct:pal("Remove hal from tmp dir to clean up before next hal maker.", []),
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    Cmd = "cd "++Tmp_Dir++" ; rm "++HalNameAdd++"*.xml",
    A = os:cmd(Cmd),
    ct:log("A: ~p", [A]),
    B = os:cmd("ls "++Tmp_Dir),
    ct:log("B: ~p", [B]).


generic_index_hal_maker(Config, Index, HalNameAdd, NewHalRev) ->
    generic_index_hal_maker(Config, Index, HalNameAdd, NewHalRev, "ln_sda1", "use_ee").

generic_index_hal_maker(Config, Index, HalNameAdd, NewHalRev, LnOrMountSda1) ->
    generic_index_hal_maker(Config, Index, HalNameAdd, NewHalRev, LnOrMountSda1, "use_ee").

generic_index_hal_maker(Config, Index, HalNameAdd, NewHalRev, LnOrMountSda1, EEorNot) ->
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    ct:pal("EEorNot : ~p", [EEorNot]),
    hal_maker(Config, Tmp_Dir, Index, EEorNot),
    
    %% Change name on hal to hal1_cxs101549-hal.xml
    NewHalName = HalNameAdd++?TmpHalXml,
    CMD1 = "cd "++Tmp_Dir++"/ ; mv "++?TmpHalXml++" "++NewHalName,
    ct:pal("CMD1 : ~n~p", [CMD1]),
    _OsCmd1 = os:cmd(CMD1),
    
    {Name, Id, Version} = get_name_id_and_version_in_hal(Tmp_Dir, NewHalName),
    modify_rcp_ver(Tmp_Dir, Id, Version, NewHalRev, NewHalName),

    HalDirName = Name++"_"++Id++"_"++NewHalRev,
    ct:pal("HalDirName : ~n~p", [HalDirName]),

    %% HalToPath = ?HAL_DIR++HalDirName,
    %% ct:pal("HalToPath : ~n~p", [HalToPath]),
    %% scp_to_node(Config, HalToPath),

    case LnOrMountSda1 of
	"ln_sda1" ->
	    HalToPath = ?HAL_DIR++HalDirName,
	    ct:pal("HalToPath : ~n~p", [HalToPath]),
	    scp_to_node(Config, HalToPath),
	    softlink_haltest_to_swm_dir(HalToPath);
	"mount_sda1" ->
	    %% mount_sda1_to_haltest_dir(Config),
	    HalToPath = ?HalTestHalswpDir++HalDirName,
	    ct:pal("HalToPath : ~n~p", [HalToPath]),
	    scp_to_node(Config, HalToPath)
	    %% umount_haltest_dir(Config)
    end,    
    ok.

get_name_id_and_version_in_hal(SearchDir, Xml) ->
    ct:pal("get_name_id_and_version_in_hal", []),
    CMD = "cd "++SearchDir++ " ; grep -m 1 product "++Xml,
    ct:pal("grep : ~n~p", [CMD]),
    OsCmdGrep = os:cmd(CMD),
    ct:log("OsCmdGrep : ~p ", [OsCmdGrep]),
    ProductList = fix_str(OsCmdGrep, "<\n\" />"),
    ct:log("ProductList : ~p ", [ProductList]),

    ListA = dropwhile(ProductList, "name="),
    ListB = dropwhile(ProductList, "id="),
    ListC = dropwhile(ProductList, "version="),

    Name = lists:nth(2, ListA),
    Id = lists:nth(2, ListB),
    Version = lists:nth(2, ListC),
    ct:pal("Name : ~p, Id : ~p,  Version : ~p ", [Name, Id, Version]),
    {Name, Id, Version}.

dropwhile(OrgList, SearchStr) ->
    List = lists:dropwhile(fun(X) ->
				   X =/= SearchStr
			   end, OrgList),
    ct:log("Returned List : ~p ", [List]),
    List.

modify_rcp_ver(Dir, Id, Ver, ModStr, Xml) ->
    CMD = 
	"cd "++Dir++" ; sed -i -e '/"++Id++"/s/"++Ver++"/"++ModStr++"/' "++Xml,
    ct:pal("CMD1:~n~p~n",[CMD]),
    GG = os:cmd(CMD),
    ct:pal("GG:~n~p~n",[GG]).


softlink_haltest_to_swm_dir(HalToPath) ->
    LinkCmd = "cd "++?SWM_HAL_DIR++" ; ln -s "++HalToPath,
    ct:pal("LinkCmd : ~n~p", [LinkCmd]),
    A = rct_rpc:call(rpc_1, os, cmd, [LinkCmd], 10000, print),
    ct:pal("Answ from link cmd:  : ~n~p", [A]),
    LsSwmHalDir = rct_rpc:call(rpc_1, os, cmd, ["ls "++?SWM_HAL_DIR++"*/*"], 
			       10000, print),
    ct:pal("LsSwmHalDir :~n~p ~n~p", [HalToPath, fix_str(LsSwmHalDir, "\n ")]),
    case LsSwmHalDir of
	[] -> 
	    ct:fail("Fail tc due to,Not expected dir in /rcs/swm/sda1/halswp/");
	_Other -> 
	    case re:run(LsSwmHalDir, "cannot") of %% check for cannot access
		nomatch ->
		    ok;
		{match, _} ->
		    ct:log("# LsSwmHalDir : ~n~p", [LsSwmHalDir]),
		    ct:fail("Fail tc due to cannot access halswp directory!")
	    end
    end.    


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
hal_maker(Config, Tmp_Dir, HwCompIndx) ->
    hal_maker(Config, Tmp_Dir, HwCompIndx, "use_ee").
hal_maker(Config, Tmp_Dir, HwCompIndx, EEorNot) ->
    ct:pal("## HAL maker ##"),
    ct:pal("## EEorNot: ~p ##", [EEorNot]),
    BoardType = proplists:get_value(board_type, Config),
    Tftp_Dir = proplists:get_value(tftp_dir, Config),
    %% Tmp_Dir = proplists:get_value(tmp_dir, Config),

    UpType = "BASEBAND",
    ct:pal("# uptype: ~p", [UpType]),

    ct:pal("Tmp_Dir : ~n~p", [Tmp_Dir]),

    %% %% Note this could be zip.
    RCP_CXS = "RCS-*_CXS101549*.zip",
    
    %% 1. cp RCP-*_CXS101549*.cxs from tftp dir to tmp_dir
    CMD1 = "cp "++Tftp_Dir++"/"++RCP_CXS++" "++Tmp_Dir,
    ct:pal("CMD1 : ~n~p", [CMD1]),
    OsCmd1 = os:cmd(CMD1),
    ct:log("OsCmd1 : ~n~p", [OsCmd1]),
    LsTmpDir = fix_str(os:cmd("ls "++Tmp_Dir), "\n"),
    ct:pal("Ls 1 : ~n~p", [LsTmpDir]),

%%%%%%%%%%%%%%%%%%
    %% remove cxps and xml before unzip or tar
    CleanUpCxp_TmpDir = "rm -f "++Tmp_Dir++"*.cxp",
    CleanUpXml_TmpDir = "rm -f "++Tmp_Dir++"*.xml",
    ct:pal("CleanUpCxp_TmpDir Cmd : ~n~p", [CleanUpCxp_TmpDir]),
    ct:pal("CleanUpXml_TmpDir Cmd : ~n~p", [CleanUpXml_TmpDir]),

    OsCmdCleanUpCxp = os:cmd(CleanUpCxp_TmpDir),
    ct:pal("OsCmdCleanUpCxp : ~n~p", [OsCmdCleanUpCxp]),
    OsCmdCleanUpXml = os:cmd(CleanUpXml_TmpDir),
    ct:pal("OsCmdCleanUpXml : ~n~p", [OsCmdCleanUpXml]),

    LsTmpDirAfterCleanUp = fix_str(os:cmd("ls "++Tmp_Dir), "\n"),
    ct:pal("Ls after cleanup : ~n~p", [LsTmpDirAfterCleanUp]),
	

%%%%%%%%%%%%%%%%%%
    %% 2. tar -xzf RCP-*_CXS101549*.cxs.
    CMD2 = case re:run(LsTmpDir, ".zip") of
	       {match, _} ->
		   "cd "++Tmp_Dir++" ; unzip "++RCP_CXS;
	       nomatch -> %% .cxs file
		   "cd "++Tmp_Dir++" ; tar -xzf "++RCP_CXS
	   end,
    %% CMD2 = "cd "++Tmp_Dir++" ; tar -xzf "++RCP_CXS++" ",
    ct:pal("CMD2 : ~n~p", [CMD2]),
    OsCmd2 = os:cmd(CMD2),
    ct:log("OsCmd2 : ~n~p", [OsCmd2]),
    ct:pal("Ls 2 : ~n~p", [fix_str(os:cmd("ls "++Tmp_Dir), "\n")]),
    ct:log("cat before hal maker : ~n~s", 
	   [fix_str(os:cmd("cat "++Tmp_Dir++?UgCxsXml), "\n")]),

    %% test_server:break("A"),
    %%%%% Note! gup maker not needed any longer due to UP is using gup maker.
    %% %% %% hal maker
    %% %% CxsXml = ?UgCxsXml,
    %% HW_compat = " $RCS_TOP/HWC/HWC_CXA114005 ",
    %% BBhw_base = " $RCS_TOP/HWC/HWC_CNX9013421/HWC_CAX1034055/xml/BBhw ",
    %% GupMakerCmd = "gupmaker.escript -debug -metafile "++
    %% 	Tmp_Dir++?UgCxsXml++
    %% 	" -out "++Tmp_Dir++
    %% 	" -index "++HwCompIndx++" -uptype "++UpType++HW_compat++BBhw_base,
    %% ct:pal("GupMakerCmd : ~n~p", [GupMakerCmd]),

    %% OsCmd = os:cmd(GupMakerCmd),
    %% ct:log("os cmd : ~n~p", [string:tokens(OsCmd, "\n \",")]),
    %% timer:sleep(2000),
    %% ct:log("cat after hal maker : ~n~p",
    %% 	   [fix_str(os:cmd("cat "++Tmp_Dir++?UgCxsXml), "\n")]),

    %% Note this shall not be needed when hal maker works.
    %% 3. mv cxs-up.xml to -hal.xml
    CMD3 = "cd "++Tmp_Dir++" ; mv "++?UgCxsXml++" "++?TmpHalXml,
    ct:pal("CMD3 : ~n~p", [CMD3]),
    OsCmd3 = os:cmd(CMD3),
    ct:log("OsCmd3 : ~n~p", [OsCmd3]),
    ct:pal("Ls 3 : ~n~p", [fix_str(os:cmd("ls "++Tmp_Dir), "\n")]),

    %% Add index to hal xml
    ct:pal("add index to hal xml"),
    remove_index_from_hal(Tmp_Dir, ?TmpHalXml),
    add_index_to_glob(Tmp_Dir, ?TmpHalXml, HwCompIndx),

    %% 4. Set bundle RCS cxp version to wildcard.
    case BoardType of
	"dus5201" ->
	    add_wildcard_on_product("CXP9031275", Tmp_Dir); %% DUS2 incl EE
	_Dus3 -> %% dus3 RCSEE
	    case EEorNot of
		"not_cs" -> %% EE cxp is not scp to hal dir on node.
		    add_wildcard_on_product("CXP9025317", Tmp_Dir); %% DUS3-EE
		_Else ->
		    ct:log("For RCSEE then it shall be used if it match index and HW.")
	    end
    end,
    add_wildcard_on_product("RCSMW-", Tmp_Dir),
    add_wildcard_on_product("DUMMY-", Tmp_Dir),

    %% 5. Insert a dummy cxp in contentlist, this shall never be used. 
    CMD4 = "cd "++Tmp_Dir++"/"++
    	";"++"sed -i '/<contentinfo>/a <product name=\"TEST33\" id=\"CXP333333_33\" version=\"R33D\" filename=\"TEST33_CXP333333.cxp\"\/>' "++?TmpHalXml,
    os:cmd(CMD4),
    ct:log(" # Ls1: ~p", [os:cmd("grep TEST33 "++Tmp_Dir++"/"++?TmpHalXml)]),
    timer:sleep(2000),

    %% 6. Remove not needed bundle cxp and cxs.
    %% os:cmd("cd "++Tmp_Dir++" ; rm RCS-*.zip ; rm RCS-*.cxp "),
    %% Remove unused CXPs due to disc space problem.
    %% Remove unused cxps also from hal xmls.
    UnusedCxps = case BoardType of
		    BoardType when BoardType == "dus5301";
				   BoardType == "dus3301" ->
			 os:cmd("cd "++Tmp_Dir++" ; rm RCS-*.zip "),
			 os:cmd("cd "++Tmp_Dir++" ; rm COBRA*.cxp "),
			 os:cmd("cd "++Tmp_Dir++" ; rm RCS-DUS2*.cxp "),
			 case EEorNot of
			     "not_cs" ->
				 %% Remov this EE sue to otherwise it will be enospace
				 ct:log("No scp EE cxp to hal dir on node, due to disc shortage!"),
				 os:cmd("cd "++Tmp_Dir++" ; rm RCSMW-ARM*.cxp "),
				 os:cmd("cd "++Tmp_Dir++" ; rm RCSEE-DUS3*.cxp ");
			     _Other ->
				 ct:log("scp EE cxp to hal dir on node")
			 end,
			 %% %% " sed -i /COBRA/d ";
			 %% ["COBRA","RCS-DUS2"]; %% Dont remove"RCS-DUS2" from hal xml, due to it will destroy building up hal dir.
			 ["COBRA"]; %% RCSEE-DUS3 need to be removed, due to when test of gup no kdu match then it will fail due to it not exist in contentinfo.
		     "dus5201" -> %% Using old bundle
			 os:cmd("cd "++Tmp_Dir++" ; rm RCS-*.zip "),
			 %% os:cmd("cd "++Tmp_Dir++" ; rm RCS-*.cxp "),
			 os:cmd("cd "++Tmp_Dir++" ; rm MTIGER*.cxp "),
			 os:cmd("cd "++Tmp_Dir++" ; rm TIGER*.cxp "),
			 os:cmd("cd "++Tmp_Dir++" ; rm RCSEE-DUS3*.cxp "),
			 %% Remov this EE sue to otherwise it will be enospace
			 os:cmd("cd "++Tmp_Dir++" ; rm RCS-DUS2*.cxp "),
			%% " sed -i /TIGER/d "
			 ["TIGER","RCSEE-DUS3"]
    end,

    %%%% Remove unused cxps also from hal xmls.

    lists:foreach(fun(Cxp) ->
			  RemSedCmd = " sed -i /"++Cxp++"/d ",
			  CMD5 = "cd "++Tmp_Dir++"/"++";"++RemSedCmd++
			      ?TmpHalXml,
			  os:cmd(CMD5)
		  end, UnusedCxps),
    
    CAT = os:cmd("cat "++Tmp_Dir++"/"++?TmpHalXml),
    ct:log("cat hal xml : ~n~p", [fix_str(CAT, "\n")]),
    LsTmpDirRem = os:cmd("ls "++Tmp_Dir),
    ct:log(" # LsTmpDir after remove: ~p", [LsTmpDirRem]),
    ok.


add_wildcard_on_product(ProdSearch, Tmp_Dir) ->
    %% grep for first match.
    CMD = "grep -m 1 "++ProdSearch++" "++Tmp_Dir++?TmpHalXml,
    ct:pal("grep : ~n~p", [CMD]),
    OsCmdGrep = os:cmd(CMD),

    ProductList = fix_str(OsCmdGrep, "<\n\" />"),
    ct:log("ProductList : ~p ", [ProductList]),

    ListA = dropwhile(ProductList, "name="),
    ListB = dropwhile(ProductList, "id="),
    ListC = dropwhile(ProductList, "version="),

    Name = lists:nth(2, ListA),
    Id = lists:nth(2, ListB),
    Version = lists:nth(2, ListC),
    ModStr = "*", 
    ct:pal("Name : ~p, Id : ~p, Version : ~p,", [Name, Id, Version]),
    CMD2 = 
    	"cd "++Tmp_Dir++" ; sed -i -e '/"++Id++"/s/"++Version++"/"++ModStr++"/' "++?TmpHalXml,
    ct:pal("CMD1:~n~p~n",[CMD2]),
    _GG = os:cmd(CMD2),

    ok.


upgrade_prep_zip_with_gup(Config) ->
    %% Gup is generated in zip build.
    MeId = proplists:get_value(me_id, Config),

    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),
    [CXS, Index, Label] = string:tokens(SW_Vers, "/-"),
    ct:log("CXS:~p, Index: ~p, Label: ~p ~n",[CXS, Index, Label]),
    CXS_LABEL = CXS++"_"++Index++"-"++Label,
    ct:pal("Active CXS_LABEL:~n~p~n",[CXS_LABEL]),

    [{_, NodeName}] = ct:get_config(test_nodes),
    Node = atom_to_list(NodeName),
    ct:pal("Node:~n~p~n",[Node]),   
    %% os:cmd("/proj/rcs-tmp/hw_sw_dec/upgrade_prep_zip/upgradeprep.sh -stp "++
    %% 	       Node++" "++CXS_LABEL),
    os:cmd("upgradeprep.sh -stp "++Node++" "++CXS_LABEL),
    ok.

modify_gup(Config) ->
    CxsXml = ?UgCxsXml,
    BoardType = proplists:get_value(board_type, Config),
    UGPath = proplists:get_value(ug_path, Config),

    %% Add radio cxps if it dus.
    case BoardType of
	BoardType when BoardType == "tcu03";
		       BoardType == "tcu0401" ->
	    ok;
	_Other ->
	    %% Case dus -> BASEBAND, cp two ru cxp to upgrade path.
	    CP = "cp "++?ExtraRuCxps++" "++UGPath++"/",
	    os:cmd(CP),
	    ct:log(" # Ls afte cp ru cxps: ~p", [os:cmd("ls "++UGPath)])
    end,

    %%%%
    %% 1. Add a not valid cxp in content info. This will not be used.
    %%%%
    CMD1 = "cd "++UGPath++"/"++
    	";"++"sed -i '/<contentinfo>/a <product name=\"TEST12\" id=\"CXP121212_12\" version=\"R12D\" filename=\"TEST12_CXP121212.cxp\"\/>' "++CxsXml,
    os:cmd(CMD1),
    ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000),

    %%%%
    %% 1. Remove two last lines, </boardLists> and </configuration>.
    %%%%
    CMD_1 = "cd "++UGPath++"/"++
    	";"++"sed -i '$d ' "++CxsXml,
    os:cmd(CMD_1), %% Remove last linte.
    os:cmd(CMD_1), %% Remove last linte once more.
    ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000),

    %%%%
    %% 2. add OTHER.
    %%%%
    EndFile = "radio_baseband",
    CMD2 = "cd "++UGPath++
    	";cat "++CxsXml++" "++?EndFileDir++EndFile++" > tmp; mv tmp "++CxsXml,
    os:cmd(CMD2),
    ct:log(" # Ls2: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000).


%%--------------------------------------------------------------------
%% global index match hal2. No hw match in global to UP.
%%--------------------------------------------------------------------
check_ug_result_416_A8(Config) ->
    %% Umount on /rcs/swm/sda1 is needed after node restart, due to 
    %% After a node startup the directory will mounted by swm.
    %% This is needed otherwise the check will fail.
    %% products will only find global, and update it in read_swp_selected.
    check_selected_vs_installed_cxps(Config),

    Grep = rct_rpc:call(rpc_1, os, cmd,
		       ["grep CXS101549_* /home/sirpa/software/cxs101549*-up.xml"], 
		       10000, print),
    GrepList = fix_str(Grep, "\"\n </>"),
    ct:pal("GrepList : ~n~p", [GrepList]),
    GlobalSwpId = decupling_lib:get_name_id_version(GrepList),
    ct:pal("GlobalSwpId : ~n~p", [GlobalSwpId]),

    SwP_Selected = rct_rpc:call(rpc_1, 
				swmBoardList, 
				read_swp_selected, [], 10000, noprint),
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),

    BoardType = proplists:get_value(board_type, Config),
    SwP_Expected = case BoardType of
		       "dus5201" ->
    			   [{hwSwCompatibility,[{index,"HALTESTB"}]},
    			    {swp_id,[{global,GlobalSwpId},
    			   	     {hal,"RCS-DUS2_CXS101549_8_"++?NewHalRev2}]},
    			    {contentinfo,[global]},
    			    {boardList,[{hal,{"BASEBAND","BB521_S"}}]
			    }];
		       BoardType when BoardType == "dus5301";
				      BoardType == "dus3301" ->
			   [{hwSwCompatibility,[{index,"HALTESTB"}]},
    			    {swp_id,[{global,GlobalSwpId},
    			   	     {hal,"RCS-DUS2_CXS101549_8_"++?NewHalRev2}]},
    			    {contentinfo,[global]},
    			    {boardList,[{hal,{"BASEBAND","BB662_3"}}
				       ]}]
    		   end,    
    ct:pal("# SwP_Expected: ~p", [SwP_Expected]),

    Sorted_SwP_Selected= sort_lists(SwP_Selected),
    ct:log("# Sorted_SwP_Selected: ~p", [Sorted_SwP_Selected]),
    Sorted_SwP_Expected= sort_lists(SwP_Expected),
    ct:log("# Sorted_SwP_Expected: ~p", [Sorted_SwP_Expected]),

    true = Sorted_SwP_Selected == Sorted_SwP_Expected,

    HwId = decupling_lib:get_hw(rpc_1),
    SelProducts = decupling_lib:get_selected_products(rpc_1, HwId),
    ct:pal("# SelProducts: ~p", [SelProducts]),

    HalList = get_hal_cxp_list(SelProducts),
    ct:pal("HalList: ~n~p", [HalList]),
    GlobList = get_glob_cxp_list(SelProducts),
    ct:pal("GlobList: ~n~p", [GlobList]),

    check_exp_hal_is_correct(Config, HalList),
    check_global_list(GlobList),
    ok.


get_hal_cxp_list(SelProducts) ->
    %%% This will get hal list
    List = lists:dropwhile(fun({{_Name,_,_},{HalOrGlobal, _CXP}}) ->
    				   hal =/= HalOrGlobal
			   end, SelProducts),
    ct:pal("## ## Returned List : ~p ", [List]),
    List. 
  
get_glob_cxp_list(SelProducts) ->
    %%% This will get hal list
    List = lists:takewhile(fun({{_Name,_,_},{HalOrGlobal, _CXP}}) ->
    				   global == HalOrGlobal
			   end, SelProducts),
    ct:pal("#### Returned Global List : ~p ", [List]),
    List.   

check_exp_hal_is_correct(Config, HalList) ->
    BoardType = proplists:get_value(board_type, Config),
    ExpHalCxp = case BoardType of
		    "dus5201" ->
			Fun = fun({{X,_,_},_}) -> 
				      X == "COBRA-A10" orelse X == "COBRA" end,
			lists:filter(Fun, HalList);		
		    BoardType when BoardType == "dus5301";
    				   BoardType == "dus3301" ->
			%% Case CXP exist then return true and add to new list.
			Fun = fun({{X,_,_},_}) -> 
				      X == "MTIGER" orelse X == "TIGER" end,
			lists:filter(Fun, HalList)
    		end,
    ct:pal("# ExpHalCxp: ~p",[ExpHalCxp]),
    2 = length(ExpHalCxp).

check_global_list(GlobalList) ->
    lists:foreach(fun({{Name,_,_},{Global,CXP}}) ->
			  case Global == global of
			      true ->
				  ct:pal("~p : from global, expected.",[Name]);
			      false ->
				  ct:pal("~p, ~p : not from global, "
					 "NOT expected.", [Name, CXP]),
				  ct:fail("Not expected cxp from hal!")
			  end
		  end, GlobalList).
				  
%%--------------------------------------------------------------------
%% global index match hal1. No hw match in global to UP.
%%--------------------------------------------------------------------
check_ug_result_416_A8_test2(Config) ->
    %% Umount on /rcs/swm/sda1 is needed after node restart, due to 
    %% After a node startup the directory will mounted by swm.
    %% This is needed otherwise the check will fail.
    %% products will only find global, and update it in read_swp_selected.
    check_selected_vs_installed_cxps(Config),

    Grep = rct_rpc:call(rpc_1, os, cmd,
		       ["grep CXS101549_* /home/sirpa/software/cxs101549*-up.xml"], 
		       10000, print),
    GrepList = fix_str(Grep, "\"\n </>"),
    ct:pal("GrepList : ~n~p", [GrepList]),
    GlobalSwpId = decupling_lib:get_name_id_version(GrepList),
    ct:pal("GlobalSwpId : ~n~p", [GlobalSwpId]),

    SwP_Selected = rct_rpc:call(rpc_1, 
				swmBoardList, 
				read_swp_selected, [], 10000, noprint),
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),

    BoardType = proplists:get_value(board_type, Config),
    SwP_Expected = case BoardType of
		       "dus5201" ->
			   [{hwSwCompatibility,[{index,"HALTESTA"}]},
			    {swp_id,[{global,GlobalSwpId},
				     {hal,"RCS-DUS2_CXS101549_8_"++?NewHalRev1}]},
			    {contentinfo,[global]},
			    {boardList,[{hal,{"BASEBAND","BB521_S"}}
				       ]}];
		       BoardType when BoardType == "dus5301";
				      BoardType == "dus3301" ->
			   [{hwSwCompatibility,[{index,"HALTESTA"}]},
    			    {swp_id,[{global,GlobalSwpId},
    			   	     {hal,"RCS-DUS2_CXS101549_8_"++?NewHalRev1}]},
    			    {contentinfo,[global]},
    			    {boardList,[{hal,{"BASEBAND","BB662_3"}}]
			    }]
		   end,
    ct:pal("# SwP_Expected: ~p", [SwP_Expected]),

    Sorted_SwP_Selected= sort_lists(SwP_Selected),
    ct:log("# Sorted_SwP_Selected: ~p", [Sorted_SwP_Selected]),
    Sorted_SwP_Expected= sort_lists(SwP_Expected),
    ct:log("# Sorted_SwP_Expected: ~p", [Sorted_SwP_Expected]),

    true = Sorted_SwP_Selected == Sorted_SwP_Expected,

    HwId = decupling_lib:get_hw(rpc_1),
    SelProducts = decupling_lib:get_selected_products(rpc_1, HwId),
    ct:pal("# SelProducts: ~p", [SelProducts]),

    HalList = get_hal_cxp_list(SelProducts),
    ct:pal("HalList: ~n~p", [HalList]),
    GlobList = get_glob_cxp_list(SelProducts),
    ct:pal("GlobList: ~n~p", [GlobList]),

    check_exp_hal_is_correct(Config, HalList),
    check_global_list(GlobList),
    ok.


%%--------------------------------------------------------------------
%% global new index, hw match. No hal shall be used
%%--------------------------------------------------------------------
check_ug_result_416_A9(Config) ->
    %% Umount on /rcs/swm/sda1 is needed after node restart, due to 
    %% After a node startup the directory will mounted by swm.
    %% This is needed otherwise the check will fail.
    %% products will only find global, and update it in read_swp_selected.
    check_selected_vs_installed_cxps(Config),
    Grep = rct_rpc:call(rpc_1, os, cmd,
		       ["grep CXS101549_* /home/sirpa/software/cxs101549*-up.xml"], 
		       10000, print),
    GrepList = fix_str(Grep, "\"\n </>"),
    ct:pal("GrepList : ~n~p", [GrepList]),
    GlobalSwpId = decupling_lib:get_name_id_version(GrepList),
    ct:pal("GlobalSwpId : ~n~p", [GlobalSwpId]),

    SwP_Selected = rct_rpc:call(rpc_1, 
				swmBoardList, 
				read_swp_selected, [], 10000, noprint),
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),
    BoardType = proplists:get_value(board_type, Config),
    SwP_Selected = case BoardType of
		       "dus5201" ->
			   [{hwSwCompatibility,[{index,"HALTESTB"}]},
			    {swp_id,[{global,GlobalSwpId}]},
			    {boardList,[{global,{"BASEBAND","BB521_S"}},
					{global,{"OTHER","FAKE_OTHER"}}]}];
		       BoardType when BoardType == "dus5301";
				      BoardType == "dus3301" ->
			   [{hwSwCompatibility,[{index,"HALTESTB"}]},
			    {swp_id,[{global,GlobalSwpId}]},
			    {boardList,[{global,{"BASEBAND","BB662_3"}},
					{global,{"OTHER","FAKE_OTHER"}}]}]
		   end,    

    HwId = decupling_lib:get_hw(rpc_1),
    SelProducts = decupling_lib:get_selected_products(rpc_1, HwId),
    ct:pal("# SelProducts: ~p", [SelProducts]),
    %% Check expected hal or global
    ExpHalCxp = "not_exist",
    check_cxp_from_hal_and_global(ExpHalCxp, SelProducts),
    ok.

%%--------------------------------------------------------------------
%% global index match hal2. Hw match both in Global and Hal. 
%%--------------------------------------------------------------------
check_ug_result_416_A10(Config) ->
    %% Umount on /rcs/swm/sda1 is needed after node restart, due to 
    %% After a node startup the directory will mounted by swm.
    %% This is needed otherwise the check will fail.
    %% products will only find global, and update it in read_swp_selected.
    check_selected_vs_installed_cxps(Config),

    Grep = rct_rpc:call(rpc_1, os, cmd,
		       ["grep CXS101549_* /home/sirpa/software/cxs101549*-up.xml"], 
		       10000, print),
    GrepList = fix_str(Grep, "\"\n </>"),
    ct:pal("GrepList : ~n~p", [GrepList]),
    GlobalSwpId = decupling_lib:get_name_id_version(GrepList),
    ct:pal("GlobalSwpId : ~n~p", [GlobalSwpId]),

    SwP_Selected = rct_rpc:call(rpc_1, 
				swmBoardList, 
				read_swp_selected, [], 10000, noprint),
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),

    BoardType = proplists:get_value(board_type, Config),
    SwP_Expected = case BoardType of
		       "dus5201" ->
			   [{hwSwCompatibility,[{index,"HALTESTB"}]},
			    {swp_id,[{global,GlobalSwpId},
				     {hal,"RCS-DUS2_CXS101549_8_"++?NewHalRev2}]},
			    {boardList,[{global,{"BASEBAND","BB521_S"}},
					{hal,{"BASEBAND","BB521_S"}},
					{global,{"OTHER","FAKE_OTHER"}}]}];
		        BoardType when BoardType == "dus5301";
				      BoardType == "dus3301" ->
			   [{hwSwCompatibility,[{index,"HALTESTB"}]},
			    {swp_id,[{global,GlobalSwpId},
				     {hal,"RCS-DUS2_CXS101549_8_"++?NewHalRev2}]},
			    {boardList,[{global,{"BASEBAND","BB662_3"}},
					{hal,{"BASEBAND","BB662_3"}},
					{global,{"OTHER","FAKE_OTHER"}}]}]
		   end,    
    ct:pal("# SwP_Expected: ~p", [SwP_Expected]),

    Sorted_SwP_Selected= sort_lists(SwP_Selected),
    ct:log("# Sorted_SwP_Selected: ~p", [Sorted_SwP_Selected]),
    Sorted_SwP_Expected= sort_lists(SwP_Expected),
    ct:log("# Sorted_SwP_Expected: ~p", [Sorted_SwP_Expected]),

    true = Sorted_SwP_Selected == Sorted_SwP_Expected,

    HwId = decupling_lib:get_hw(rpc_1),
    SelProducts = decupling_lib:get_selected_products(rpc_1, HwId),
    ct:pal("# SelProducts: ~p", [SelProducts]),

    HalList = get_hal_cxp_list(SelProducts),
    ct:pal("HalList: ~n~p", [HalList]),
    GlobList = get_glob_cxp_list(SelProducts),
    ct:pal("GlobList: ~n~p", [GlobList]),

    check_exp_hal_is_correct(Config, HalList),
    check_global_list(GlobList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_cxp_from_hal_and_global(ExpHalCxp, SelProductsList) ->
    ct:log(" # SelProductsList : ~p", [SelProductsList]),
    GlobalList = lists:delete(ExpHalCxp, SelProductsList),
    ct:log(" # GlobalList : ~p", [GlobalList]),
    lists:foreach(fun({{Name,_,_},{Global,CXP}}) ->
			  case Global == global of
			      true ->
				  ct:pal("~p : from global, expected.",[Name]);
			      false ->
				  ct:pal("~p, ~p : not from global, "
					 "NOT expected.", [Name, CXP]),
				  ct:fail("Not expected cxp from hal!")
			  end
		  end, GlobalList).

%% sed_hal(Cmd, HalToPath, HalName) ->
%%     ct:log("# sed hal Cmd: ~p", [Cmd]),

%%     _Sed = rct_rpc:call(rpc_1, os, cmd, [Cmd], 10000, 
%% 			print),

%%     Cat = rct_rpc:call(rpc_1, os, cmd, ["cat "++HalToPath++HalName], 10000, 
%% 			print),
%%     ct:log(" # cat: ~p", [fix_str(Cat, "\n\r\"> ")]),
%%     timer:sleep(1000).


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------

scp_to_node(Config, HAL_TO_PATH) ->
    HAL_FROM_PATH = proplists:get_value(tmp_dir, Config),
    scp_to_node(Config, HAL_FROM_PATH, HAL_TO_PATH).

scp_to_node(_Config, HAL_FROM_PATH, HAL_TO_PATH) ->
    %% Tmp_Dir = proplists:get_value(tmp_dir, Config),

    %% 4. scp cxps and hal xml to a specific path on node.
    Ls_1 = rct_rpc:call(rpc_1, os, cmd, ["ls "++HAL_TO_PATH], 10000, 
    		      noprint),
    ct:log("# LS before scp :~p~n~p", [HAL_TO_PATH, Ls_1]),
    %% _MkDir = rct_rpc:call(rpc_1, os, cmd, ["mkdir "++HAL_TO_PATH], 10000, 
    %% 		      print),
    C = ct_telnet:send(?Console, "mkdir "++HAL_TO_PATH), 
    ct:pal(" C : ~p", [C]),
    
    ct:pal("scp cxps and hal xml to a specific path on node."),
    ct:pal("scp Node HAL to path  : ~p", [HAL_TO_PATH]),
    ct:pal("scp Node HAL from path  : ~p", [HAL_FROM_PATH]),
    %% FileList = fix_str(os:cmd("ls "++Tmp_Dir), "\n"),
    FileList = fix_str(os:cmd("ls "++HAL_FROM_PATH), "\n"),
    ct:pal("FileList : ~n~p", [FileList]),
    lists:foreach(fun(X) ->
			  timer:sleep(5000),
			  ct:pal("scp cxp : ~p", [X]),
			  {ok, A} = rct_scp:to_target(scp, 
						%% Tmp_Dir++X, 
						HAL_FROM_PATH++X, 
						HAL_TO_PATH, 
						90000, noprint),
			  ct:log("# scp : ~p", [A]),
			  case re:run(A, "No space left") of
			      {match,_} ->
				  ct:fail("## scp failed. Fail the SUITE!");
			      nomatch ->
				  ok
			  end
		  end, FileList),
    timer:sleep(1000),    
    %% test_server:break("Check !!!!"),
    Ls = rct_rpc:call(rpc_1, os, cmd, ["ls "++HAL_TO_PATH], 10000, 
    		      noprint),
    LS = fix_str(Ls, "\n\r\"> "),
    ct:pal("# LS after scp : ~p", [LS]),

    %% quick check that scp worked.
    case length(LS) == length(FileList) of
	true ->
	    ok;
	false ->
	    ct:fail("TC will fail due to nr of cxps not expected")
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec set_not_suported_board_in_gup(Config) -> ok
%% @end
%%--------------------------------------------------------------------
set_not_suported_board_in_gup(Config) ->
    OrgProdNr = proplists:get_value(org_product_number, Config),
    UGPath = proplists:get_value(ug_path, Config),

    %% Store org_ug_xml with correct kdu. 
    %% Is needed after upgrade to prevent horror printouts.
    cp_to_up_xml_to_tmp_dir(Config),

    %% OrgProdRev = proplists:get_value(org_prod_rev, Config),
    ct:pal("## OrgProdNr: ~p", [OrgProdNr]),
    %% ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),
    [OrgKdu , OrgInd] = string:tokens(OrgProdNr, "/"),
    ct:log("## OrgKdu: ~p, OrgInd: ~p", [OrgKdu, OrgInd]),

    NotExistingProdNr = OrgKdu++"/99",
    ct:pal("## Update gup: ProdNrs ~p, to ~p", [OrgProdNr, NotExistingProdNr]),

    %% CMD1 = "cd "++UGPath++
    %% 	";"++"sed -i 's/"++OrgKdu++"\\/"++OrgInd++"/"++OrgKdu++"\\/99/g' " ++ ?UgCxsXml " > tmp",
    CMD1 = "cd "++UGPath++
    	" ; "++"sed -i 's/"++OrgKdu++"\\/"++OrgInd++"/"++OrgKdu++"\\/99/g' " ++ ?UgCxsXml " ",
    os:cmd(CMD1),
    ct:log("CMD1:~n~p~n",[CMD1]),
    
    ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec set_suported_board_in_gup(Config) -> ok
%% @end
%%--------------------------------------------------------------------
set_suported_board_in_gup(Config) ->
    OrgProdNr = proplists:get_value(org_product_number, Config),
    OrgProdRev = proplists:get_value(org_prod_rev, Config),

    UGPath = proplists:get_value(ug_path, Config),

    ct:pal("## OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),
    [OrgKdu , OrgInd] = string:tokens(OrgProdNr, "/"),
    ct:log("## OrgKdu: ~p, OrgInd: ~p", [OrgKdu, OrgInd]),

    NotExistingProdNr = OrgKdu++"/99",
    ct:pal("## Update gup: ProdNrs ~p, to ~p", [NotExistingProdNr, OrgProdNr]),

    CMD1 = "cd "++UGPath++
    	" ; "++"sed -i 's/"++OrgKdu++"\\/99/"++OrgKdu++"\\/"++OrgInd++"/g' " ++ ?UgCxsXml " ",
    os:cmd(CMD1),
    ct:log("CMD1:~n~p~n",[CMD1]),
    
    ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000),

    ok.


-define(ToUpDirName, "to_up_org_xml").
cp_to_up_xml_to_tmp_dir(Config) ->
    ct:pal("cp_to_up_xml_to_tmp_dir"),
    %% Store org_ug_xml with correct kdu. 
    %% Is needed after upgrade to prevent horror printouts.
    %% due to new check after upgrade (node restart).
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    UGPath = proplists:get_value(ug_path, Config),

    MkNewDir = "mkdir "++Tmp_Dir++?ToUpDirName,
    os:cmd(MkNewDir),

    NewDir = Tmp_Dir++?ToUpDirName++"/",
    ct:pal("NewDir: ~p", [NewDir]),

    CpCmd = "cp "++UGPath++"/*.xml "++NewDir,
    Cp = os:cmd(CpCmd),
    ct:log("Cp: ~p", [Cp]),

    ct:pal("ls in NewDir: ~p", [os:cmd("ls "++NewDir)]),
    ok.


%% scp_org_to_up_xml_to_arch_B(Config) ->
%%     %% cp org_ug_xml with correct kdu. 
%%     %% Is needed after upgrade to prevent horror printouts.
%%     %% due to new check after upgrade (node restart).
%%     Tmp_Dir = proplists:get_value(tmp_dir, Config),
%%     NewDir = Tmp_Dir++?ToUpDirName++"/",
%%     ct:log("NewDir: ~p", [NewDir]),
%%     ct:pal("ls in NewDir: ~p", [os:cmd("ls "++NewDir)]),

%%     Grep = rct_rpc:call(rpc_1, os, cmd,
%% 			["grep -l -i HALTESTB /rcs/swm/archive/*/* | xargs dirname"],
%% 			60000, print),
%%     ct:pal("Grep : ~p", [Grep]),
%%     [SwmArcPath] = string:tokens(Grep, "\n"),
%%     ct:pal("SwmArcPath : ~p", [SwmArcPath]),
%%     XmlName = rct_rpc:call(rpc_1, os, cmd,
%% 			["grep -l -i HALTESTB "++NewDir++"*.xml | xargs basename"],
%% 			60000, print),
%%     Xml_Name = string:tokens(XmlName, "\n"),
%%     A = rct_scp:to_target(scp, 
%% 			  NewDir++Xml_Name,
%% 			  SwmArcPath,
%% 			  %% "/rcs/swm/archive/RCS-DUS2_CXS101549_8_R8C010/", 
%% 			  60000, noprint),
%%     ct:log("# scp : ~p", [A]),
%%     %% test_server:break("B"),
%%     ok.


%% scp_org_to_up_xml_to_arch_A(Config) ->
%%     %% cp org_ug_xml with correct kdu. 
%%     %% Is needed after upgrade to prevent horror printouts.
%%     %% due to new check after upgrade (node restart).

%%     ct:pal("create a valid to up xml with correct kdu"),
%%     UGPath = proplists:get_value(ug_path, Config),
%%     ct:log("UGPath: ~p", [UGPath]),
%%     %% test_server:break("##AA"),

%%     ct:pal("create a valid to up xml with correct kdu"),
%%     set_suported_board_in_gup(Config),
%%     ct:log("cat up xml in UGPath: ~p", [os:cmd("cat "++UGPath++"/*.xml")]),

%%     Grep = rct_rpc:call(rpc_1, os, cmd,
%% 			["grep -l -i HALTESTA /rcs/swm/archive/*/* | xargs dirname"],
%% 			60000, print),
%%     ct:pal("Grep : ~p", [Grep]),
%%     [SwmArcPath] = string:tokens(Grep, "\n"),
%%     ct:pal("SwmArcPath : ~p", [SwmArcPath]),
%%     XmlName = rct_rpc:call(rpc_1, os, cmd,
%% 			["grep -l -i HALTESTA "++UGPath++"/*.xml | xargs basename"],
%% 			60000, print),
%%     Xml_Name = string:tokens(XmlName, "\n"),
%%     A = rct_scp:to_target(scp, 
%% 			  UGPath++"/"++Xml_Name,
%% 			  SwmArcPath,
%% 			  %% "/rcs/swm/archive/RCS-DUS2_CXS101549_8_R8C010/", 
%% 			  60000, noprint),
%%     ct:log("# scp : ~p", [A]),
%%     %% test_server:break("A"),
%%     ok.


%% link_hal_dir_to_swm_sda1(_Config) ->
%%     ct:pal("This is now needed due to after a restart."),
%%     umount_rcs_swm_sda1(),
%%     GrepA = rct_rpc:call(rpc_1, os, cmd,
%% 			["grep -l -i HALTESTA /rcs/haltest/*/* | xargs dirname"],
%% 			60000, print),
%%     ct:pal("GrepA : ~p", [GrepA]),
%%     [SwmArcPathA] = string:tokens(GrepA, "\n"),
%%     GrepB = rct_rpc:call(rpc_1, os, cmd,
%% 			["grep -l -i HALTESTB /rcs/haltest/*/* | xargs dirname"],
%% 			60000, print),
%%     ct:pal("GrepB : ~p", [GrepB]),
%%     [SwmArcPathB] = string:tokens(GrepB, "\n"),
%%     softlink_haltest_to_swm_dir(SwmArcPathA),
%%     softlink_haltest_to_swm_dir(SwmArcPathB).



%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @spec update_glob_with_indx_16B(Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% update_glob_with_indx_16B(Config) ->
%%     remove_index_from_glob(Config),
%%     Index = "16B",
%%     add_index_to_glob(Config, Index),
%%     ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @spec update_glob_with_indx_17A(Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% update_glob_with_indx_17A(Config) ->
%%     remove_index_from_glob(Config),
%%     Index = "17A",
%%     add_index_to_glob(Config, Index),
%%     ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec update_glob_with_indx_HALTESTA(Config) -> ok
%% @end
%%--------------------------------------------------------------------
update_glob_with_indx_HALTESTA(Config) ->
    remove_index_from_glob(Config),
    Index = "HALTESTA",
    add_index_to_glob(Config, Index),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec update_glob_with_indx_HALTESTB(Config) -> ok
%% @end
%%--------------------------------------------------------------------
update_glob_with_indx_HALTESTB(Config) ->
    remove_index_from_glob(Config),
    Index = "HALTESTB",
    add_index_to_glob(Config, Index),
    ok.


%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @spec remove_hal_dir_on_node(Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% remove_hal_dir_on_node(_Config) ->
%%     %% ct:pal("Cleanup in /rcs/haltest and softlinks from "
%%     %% 	   "/rcs/swm/sda1/halswp/"),
%%     %% rct_rpc:call(rpc_1, os, cmd, ["rm -rf "++?SWM_HAL_DIR++"*"], 10000, 
%%     %% 		 print),
%%     %% %% rct_rpc:call(rpc_1, os, cmd, ["rm -rf "++?SWM_MOUNT_DIR++"/"++?HalSwp], 
%%     %% %% 		 10000, print),
%%     %% LsSwmHalDir = rct_rpc:call(rpc_1, os, cmd, 
%%     %% 			       ["ls "++?SWM_HAL_DIR], 
%%     %% 			       10000, print),
%%     %% ct:log("ls ~p: ~n~p", [?SWM_HAL_DIR, fix_str(LsSwmHalDir, "\n ")]),
%%     %% LsTmpHalDir = rct_rpc:call(rpc_1, os, cmd, ["ls /ls/"], 10000, 
%%     %% 			       print),
%%     %% ct:pal("ls /ls/ : ~n~p", [fix_str(LsTmpHalDir, "\n ")]).
%%     ct:pal("Cleanup in /rcs/haltest/halswp"),
%%     rct_rpc:call(rpc_1, os, cmd, ["rm -rf "++?HalSwpDir], 10000, print),
    
%%     LsRcsHalTestDir = rct_rpc:call(rpc_1, os, cmd, 
%%     			       ["ls "++?HalSwpDir], 
%%     			       10000, print),
%%     ct:log("ls ~p: ~n~p", [?HalSwpDir, fix_str(LsRcsHalTestDir, "\n ")]),
%%     LsTmpHalDir = rct_rpc:call(rpc_1, os, cmd, ["ls "++?HAL_DIR], 
%% 			       10000, print),
%%     ct:pal("ls /rcs/haltest : ~n~p", [fix_str(LsTmpHalDir, "\n ")]).
    
    
    


%%%%%%
%% -define(Console, console).
cleanup_hal_dir_after_mount_sda1(Config) ->
    ct:pal("## cleanup_hal_dir ##"),
    ct:pal("## First umount is needed ##"),
    ct:pal("## Sleep a while before start umount. ##"),
    timer:sleep(120000),
    umount_rcs_swm_sda1(Config),
    %%
    ct:pal("## create /rcs/haltest dir ##"),
    ct:pal("mkdir "++?HalTestDir),
    Cmd1 = "mkdir "++?HalTestDir,
    send_rpc_os_cmd(Cmd1),
    LS = send_rpc_os_cmd("ls /rcs/"),
    Ls = fix_str(LS, "\n "),
    ct:pal("ls /rcs: ~p", [Ls]),

    %%
    ct:pal("## Now mount sda1 to haltest dir ##"),
    ct:pal("# Mount : /dev/sda1 to ~p", [?HalTestDir]),
    mount_sda1_to_haltest_dir(Config),

    timer:sleep(10000),
    Console = ?Console,
    rct_rs232:login(Console),

    ct:pal("ls  : ~p", [?HalSwpDir]),
    rct_rpc:call(rpc_1, os, cmd, ["ls "++?HalSwpDir], 
		 10000, print),

    ct:pal("Remove all faked hal under : ~p", [?HalSwpDir]),
    A = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"/RCS-DUS2_*_"++?NewHalRev1),
    ct:log("A : ~p", [A]),
    B = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"//RCS-DUS2_*_"++?NewHalRev2),
    ct:log("B : ~p", [B]),

    timer:sleep(10000),
    rct_rpc:call(rpc_1, os, cmd, ["ls "++?HalSwpDir], 
		 10000, print),
    
    timer:sleep(10000),
    ct:pal("# Umount : ~p", [?HalTestDir]),
    umount_haltest_dir(Config),
    timer:sleep(5000),
    Ls2 = send_rpc_os_cmd("ls "++?HalTestDir),
    Ls2 = fix_str(Ls2, "\n"),
    ct:log(" Ls check  : ~p", [Ls2]),
    case check_str_exist_in_list(Ls2, "halswp") of
	match ->
	    ct:fail("/rcs/haltest/halswp still exist Unexpected!, Fail TC");
	nomatch ->
	    ct:pal("/rcs/haltest/halswp not exist and it is Expected"),
	    mount_dev_sda1_to_rcs_swm_sda1(Config),
	    ok
    end.

check_str_exist_in_list(List, ExpStr) ->
    case re:run(List, ExpStr) of
	{match,_} ->
	    match;
	nomatch -> 
	    nomatch
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec check_selected_vs_installed_cxps(Config) -> ok
%% @end
%%--------------------------------------------------------------------
check_selected_vs_installed_cxps(_Config) ->
    decupling_lib:check_selected_vs_installed_cxps(rpc_1, ?NC_Session).



%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec create(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create(Config) ->
    From_Label = swm_test_lib:get_sw_version(?NC_Session),
    ct:pal("Active version before upgrade starts: ~p~n",[From_Label]),

    MeId = proplists:get_value(me_id, Config),
    UGPath = proplists:get_value(ug_path, Config),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					SftpHost, 
					Username, 
					Password, 
					UGPath, 
					MeId),
    
    ok = swm_test_lib:wait_for_swm_progress_done(?NC_Session, 
						 "SUCCESS",
						 "createUpgradePackage",
						 MeId,
						 no_check).


%%%--------------------------------------------------------------------
%%% @doc
%%% Prepare. <br/>
%%% @spec prepare(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare(Config) ->
    MeId = proplists:get_value(me_id, Config),
    To_Label = get_latest_up(),
    perform_ug_action(prepare, MeId, To_Label),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% verify. <br/>
%%% @spec verify(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify(Config) ->
    MeId = proplists:get_value(me_id, Config),
    To_Label = get_latest_up(),
    perform_ug_action(verify, MeId, To_Label),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% activate. <br/>
%%% @spec activate(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate(Config) ->
    MeId = proplists:get_value(me_id, Config),
    To_Label = get_latest_up(),
    perform_ug_action(activate, MeId, To_Label),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% confirm. <br/>
%%% @spec confirm(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm(Config) ->
    MeId = proplists:get_value(me_id, Config),
    To_Label = get_latest_up(),

    ct:pal("Perform ug confirm~n",[]),
    swm_test_lib:ug_confirm(?NC_Session,
			    To_Label,
			    MeId),

    ct:pal("sleep 2min to check for unexpected printouts."),
    timer:sleep(120000),
    ct:pal("End sleep 2min."),

    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%% remove. <br/>
%%% @spec remove(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
remove(Config) ->
    MeId = proplists:get_value(me_id, Config),
    Label = get_latest_up(),
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session,
					"SUCCESS",
					MeId,
					Label),
    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
get_latest_up() ->
    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    %% ct:pal("Label:~n~p~n",[Label]),

    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:log("UPs:~n~p~n",[UPs]),

    Label = swm_test_lib:get_highest_label(UPs),
    ct:log("Label:~n~p~n",[Label]),
    Label.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action, MeId, To_Label) ->
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					To_Label,
    					MeId,
    					Action),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------	
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.



fix_str(Str, FixStr) ->
    string:tokens(Str, FixStr).
    
%%%--------------------------------------------------------------------
%%% @doc Modify cxs.
%%% @end
%%%--------------------------------------------------------------------
step_rev_in_cxs(MeId) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 

    SW_Vers = swm_test_lib:get_active_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    swm_test_lib:modify_cxs(SW_Vers, 1).

%%--------------------------------------------------------------------
%% @doc
%% @spec remove_index_from_glob(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
remove_index_from_glob(Config) ->
    ct:log("remove index from global"),
    UGPath = proplists:get_value(ug_path, Config),
    %%%%
    %% 1. remove hwSwCompatibility Index.
    %%%%
    CMD1 = "cd "++UGPath++
    	";sed -i '/hwSwCompatibility/d' "++?UgCxsXml,
    os:cmd(CMD1),
    ct:log("#cat after remove: ~n ~p",[os:cmd("cat "++UGPath++"/"++?UgCxsXml)]),
    timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec add_index_to_glob(Config, HwCompIndx) -> ok
%% @end
%%--------------------------------------------------------------------
add_index_to_glob(Config, HwCompIndx) ->
    ct:log("add index from global"),
    UGPath = proplists:get_value(ug_path, Config),
    %%%%
    %% 1. Add correct hwSwCompatibility Index.
    %%%%
    CMD1 = "cd "++UGPath++
    	";"++"sed -i '/<\\/contentinfo>/a <hwSwCompatibility index=\""++
	HwCompIndx++"\" \/>' "++?UgCxsXml,  %% Note, need a dubble \\
    os:cmd(CMD1),
    ct:log("Add index to glob CMD1:~n~p~n",[CMD1]),
    %% ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    ct:log("#cat after add: ~n ~p",[os:cmd("cat "++UGPath++"/"++?UgCxsXml)]),
    timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec remove_index_from_hal(HalDir, HalXml) -> ok
%% @end
%%--------------------------------------------------------------------
remove_index_from_hal(HalDir, HalXml) ->
    ct:log("remove index from hal: ~p~p", [HalDir,HalXml]),
    %%%%
    %% 1. remove hwSwCompatibility Index.
    %%%%
    CMD1 = "cd "++HalDir++
    	";sed -i '/hwSwCompatibility/d' "++HalXml,
    os:cmd(CMD1),
    ct:log("#cat after remove: ~n ~p",[os:cmd("cat "++HalDir++"/"++HalXml)]),
    timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec add_index_to_glob(HalDir, HalXml, IndexName) -> ok
%% @end
%%--------------------------------------------------------------------
add_index_to_glob(HalDir, HalXml, IndexName) ->
    ct:log("add index: ~p to hal: ~p",[IndexName, HalXml]),
    %%%%
    %% 1. Add correct hwSwCompatibility Index.
    %%%%
    CMD1 = "cd "++HalDir++
    	";"++"sed -i '/<\\/contentinfo>/a <hwSwCompatibility index=\""++
	IndexName++"\" \/>' "++HalXml,  %% Note, need a dubble \\
    os:cmd(CMD1),
    ct:log("Add index to glob CMD1:~n~p~n",[CMD1]),
    %% ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    ct:log("#cat after add: ~n ~p",[os:cmd("cat "++HalDir++"/"++HalXml)]),
    timer:sleep(2000),
    ok.


sort_lists(SwP_List) ->
    %% sort list of lists.
    [{X, lists:sort(Y)} || {X, Y} <- lists:sort(SwP_List)].




ai_install(Config) ->
    get_all(),
    Console = console,
    ct:pal("##### start board restore #####"),
    aic_httpc:board_restore(Config, Console),
    ct:pal("##### end board restore #####"),
    ct:pal("##### start download files #####"),
    aic_httpc:download_files(Config, Console),
    ct:pal("##### end download files #####"),
    ct:pal("##### start check download pogress #####"),
    aic_httpc:request_download_pogress(Config, "https"),
    ct:pal("##### end check download pogress #####"),
    ct:pal("##### start integrate #####"),
    aic_httpc:integrate(Config, console, ?NC_Session),
    ct:pal("##### end integrate #####"),
    %% ct:log("sleep 60sec to make sure node is up and ready for next tests!"),
    %% timer:sleep(60000),
    ok.



%%%--------------------------------------------------------------------
%%% @doc
%%% @spec rctprep_zip_glob_to_be_used(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
rctprep_zip_glob_to_be_used(Config) ->
    %% ct:pal("This is not needed now due to hal up is used now."),
    %% ok.
    TftpDir = proplists:get_value(tftp_dir, Config),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),

    CXS_LABEL = proplists:get_value(cxs_label, Config),
    ct:pal("CXS_LABEL from jenkins config:~n~p~n",[CXS_LABEL]),

    Node = proplists:get_value(nodeName, Config),

    %% RcstPrep_Cmd = "rcstprep.sh "++Node++" "++ZipDir,
    RcstPrep_Cmd = "rcstprep.sh "++Node++" "++CXS_LABEL,
    os:cmd(RcstPrep_Cmd),
    ok.

get_all() ->
    rct_logging:get_all(log1).

%% coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	decoupling_ai_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R6A/R7A/R8A/R9A/1
%%%
%%% @doc == HW-SW-DEC, network loader ai. ==
%%%
%%%
%%% @end

-module(decoupling_ai_SUITE).
-author('erarube').
-vsn('/main/R6A/R7A/R8A/R9A/1').
-date('2017-04-20').

%%% 
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
%%% R6A/1      2016-04-22 etxivri     Created
%%% R6A/3      2016-04-25 etxivri     Updates for tcu0401
%%% R6A/4      2016-04-27 etxivri     Use two cxps in multiple hal test.
%%%                                   Better cleanup handling.
%%% R6A/5      2016-04-29 etxivri     Get cxs label from jenkins config
%%% R6A/6      2016-04-29 etxivri     Update to suport dus3201.
%%%                                   No suport of tcu03
%%% R6A/8      2016-04-29 etxivri     add hwcategory OTHER to be used in one 
%%%                                   of the global up.
%%% R6A/9      2016-05-04 etxivri     Update path to zip not needed now.
%%%                                   rcstprep.sh now use zip default in 17A.
%%% R6A/11     2016-06-09 eransbn     change hwmodel id for dus52 
%%% R7A/1      2016-09-30 etxivri     Update for dus52 to handle unbundle MW cxp
%%% R7A/2      2016-10-11 etxivri     Update to handle Tiger also is selected,
%%%                                   EE is responsible to load correct FPGA.
%%% R8A/1      2016-11-04 etxivri     Update to handle MTIGER also.
%%% R8A/3      2017-01-25 etxivri     Update update for dus53 and dus33.
%%%                                   Note TCU could not be used for this SUITE.
%%% R8A/4      2017-03-22 erarube     Update of HAL Test SW removal.
%%%                                   Check of board without HAL.
%%%                                   Support for dus3301, Support for dus3201 if needed, it has to be updated.
%%%                                   No everything is tested for dus3301. Further updates may needed.
%%% R9A/1      2017-04-20 erarube     Update for DUS33 and DUS53 due to COBRA-A10 removed.
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 groups/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,

	 board_restore/1,
	 download_files/1,
	 request_download_pogress/1,
	 integrate/1,

	 prep_640_N_glob/1, %% using index TESTHAL2

	 cp_hal_1_to_tmp_dir/1,
	 %% add_correct_hw_to_hal_1/1,
	 add_correct_hw_to_hal/1,
	 cleanup_in_tmp_dir/1,

	 make_hal_test_dir_on_node/1,
	 prep_hal_1_on_node/1,
	 scp_hal1_sw_to_node/1,
	 umount_hal_test_dir/1,
	 mount_sda1_to_hal_test_dir/1,
	 cleanup_hal_dir/1,
	 cleanup_hal_dir_no_check/1,
	 %% mod_glob_in_zip_indx_17A/1,

	 %%% This shall be used before test and after test
	 umount_rcs_swm_sda1/1,
	 mount_rcs_swm_sda1/1,
	 mount_rcs_swm_sda1_rw/1,
	 %% cleanup_rcs_swm_sda1_if_needed/1,

	 %% 640_E1
	 prep_640_E1_glob/1, %% using index TESTHAL2
	 set_not_existing_hw_id/1,
	 set_org_hw_id/1,
	 add_not_existing_hw_to_hal_1/1,
	 ai_err_seq/1,

	 %% 640_A6
	 prep_640_A6_glob/1, %% using index TESTHAL1
	 prep_640_A6_glob_test2/1, %% using index TESTHAL1

	 %% 640_A6_test2
	 cp_hal_3_to_tmp_dir/1,
	 scp_hal3_sw_to_node/1,
	 cp_ee_if_dus3_to_tmp_dir/1,

	 %% Use of two Hal at same time.
	 prep_hal_2_on_node/1,
	 cp_hal_2_to_tmp_dir/1,
	 scp_hal2_sw_to_node/1,
	 %% Check result
	 check_group_640_N_glob/1,
	 check_group_640_N_hal_glob/1,
	 check_post_group_640_E1/1,
	 check_group_640_A6_test1/1,
	 check_group_640_A6_test2/1,
	 check_group_multiple_hal/1,

	 break/1,
	 check_node_is_in_nl/1,
	 all/0,
	 coli_reinstall/1,
	 ai_install/1,
	 install_org_up_to_cleanup/1
	]).

-define(NC_Session, nc1).
-define(Console, console).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [
		 {rct_htmllink,[]},
		 {rct_consserv,cs1},
		 {rct_rs232,console},
		 {rct_power,node},
		 {rct_netconf, nc1},
		 {cth_conn_log, []},
		 {rct_logging, {all,
		 		[{erlang,
		 		  {["ERROR REPORT","CRASH REPORT"],
		 		   []}
		 		 }]}},
		 {rct_rpc, rpc_1},
		 {rct_core,[]},
		 {rct_scp, [{1, scp}]},
		 {rct_cli, {cli, [manual_connect]}}
		]}].

%% @hidden
init_per_suite(Config) ->
    ct:pal("umount orginal mount point /rcs/swm/sda1"),
    umount_rcs_swm_sda1(Config),

    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),

    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
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

    %% case BoardType of
    %% 	"tcu03" -> 
    %% 	    ct:fail("Tcu or not suported in this SUITE. No idea to continue.");
    %% 	_Boards ->
    %% 	    ok
    %% end,

    TftpDir = ct:get_config({Hwa, tftpboot}),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),

    TmpDir = ?config(priv_dir, Config),
    ct:pal("Tmp_Dir:~n~p~n",[TmpDir]),

    {OrgProdNr, OrgProdRev} = decupling_lib:get_hw(rpc_1),
    ct:pal("Add orginal HW data to ets table"),
    ct:pal("### OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("### OrgProdRev: ~p", [OrgProdRev]),
    case OrgProdNr of
        OrgProdNr when OrgProdNr == "KDU137848/1" -> %% dus53
	    case OrgProdRev of
		"P1C" ->
		    ct:pal("Board is a dus53 P1C with no real HAL. ~n"
			   "Ok to continue the test.");
		_Other1 ->
		    ct:fail("Board is a dus53with real HAL. ~n"
			    "Not OK to be used for test. Abort the test!")
	    end;
        OrgProdNr when OrgProdNr == "KDU137847/1" -> %% dus33
	    case OrgProdRev of
		"P1B" ->
		    ct:pal("Board is a dus33 P1B with no real HAL. ~n"
			   "Ok to continue the test.");
		_Other1 ->
		    ct:fail("Board is a dus33with real HAL. ~n"
			    "Not OK to be used for test. Abort the test!")
	    end;
	_Other2 ->
	    ct:pal("Board is a dus52 with no real HAL. ~n"
		   "Ok to be used for test.")
    end,

    %%% Get build CXS label to be used from Jenkins
    CxsLabel = ct:get_config({jenkins_config, cxp}), %% The key is cxp.
    %% CxsLabel = "CXS101549_8-R9J59",
    %% %% case BoardType of
    %% %% 	"dus3201" ->
    %% %% 	    CxsLabel = "CXS101549_5-R6A72";
    %% %% 	"dus5201" ->
    %% %% 	    CxsLabel = "CXS101549_8-R7A155";
    %% %% 	_Tcu04 ->
    %% %% 	    CxsLabel = "CXS101549_6-R6A72"
    %% %% end,
    ct:pal("### CxsLabel from jenkins: ~p", [CxsLabel]),

    [
     {nodeName, NodeName},
     {board_type, BoardType},
     {tftp_dir, TftpDir},
     {tmp_dir, TmpDir},
     {cxs_label, CxsLabel},
     {org_prouct_number, OrgProdNr},
     {org_prod_rev, OrgProdRev} | Config].

%% @hidden
end_per_suite(Config) ->
    OrgProdNr = proplists:get_value(org_prouct_number, Config),
    OrgProdRev = proplists:get_value(org_prod_rev, Config),
    ct:pal("# # OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),
    decupling_lib:check_expected_hw_id(rpc_1, OrgProdNr, OrgProdRev),
    %% %% cleanup_hal_dir(Config),
    %% cleanup_hal_dir_no_check(Config), %% Maybe thiscan not be runed in en per suite due to hook!!
    umount_hal_test_dir(Config), %% don not care if it is already unmounted.
    mount_rcs_swm_sda1(Config),
    %% cleanup_rcs_swm_sda1_if_needed(Config), %% Maybe thiscan not be runed in en per suite due to hook!!
    ok.

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("## ~p ##", [TestCase]),
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    %% test_server:break("TC failed !!!"),
	    %% timer:sleep(120000), %% Sleep if node is booting
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
	    case wait_for_netconf_started() of
		ok ->
		    ct:pal("Netconf is up, start clean up.");
		netconf_not_up ->
		    ct:pal("Netconf is NOT up, check nl state."),
			case check_node_is_in_nl(Config) of
			    no_nl ->
				ct:pal("Node is not in NL, "
				       "Start clean up anyway.");
			    nl -> %% node is in network loader.
				prep_org_glob(Config), %% index with hw match
				aic_httpc:download_files(Config, ?Console),
				aic_httpc:integrate(Config, 
				 		    ?Console, 
				 		    ?NC_Session)
			end
	    end,
	    cleanup_hal_dir_no_check(Config),
	    %% cleanup_rcs_swm_sda1_if_needed(Config),
	    aic_httpc:export_ai_log(Config), 
	    aic_httpc:export_esi(Config)
    end,
    ok.


%% get_cxs_label() ->
%%     LsCxsLabel = os:cmd("cd "++TftpDir++" ; ls cxslabel_*"),
%%     ct:pal("LsCxsLabel:~n~p~n",[LsCxsLabel]),
%%     case re:run(LsCxsLabel, "cannot access") of
%%     	nomatch ->
%%     	    ok;
%%     	{match, _} -> 
%%     	    ct:fail("TC shall fail due to cxslabel_ not found in tftpboot dir")
%%     end,
%%     [CXS, Ver] = string:tokens(LsCxsLabel,"cxslabel_\n"),
%%     CXS_LABEL = CXS++"_"++Ver,
%%     CXS_LABEL.

%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [%% Install, Only gup exist
     {group_640_N_glob,[sequence], [
				    prep_640_N_glob, %% Index TESTHAL2
				    ai_install,
				    check_group_640_N_glob
				    %% break
				   ]},
     %% Install, Hal exist and gup, No index match, SW selected from gup,
     %% boardlist.
     {group_640_N_hal_glob,[sequence],[
				       %%% prep_640_N_glob, idx TESTHAL2
				       %%% test1 hal, index TESTHAL1
				       prep_640_N_glob,
				       umount_rcs_swm_sda1,
				       make_hal_test_dir_on_node,
				       mount_sda1_to_hal_test_dir,
				       prep_hal_1_on_node,
				       cp_hal_1_to_tmp_dir,
				       add_correct_hw_to_hal,
				       scp_hal1_sw_to_node,
				       cleanup_in_tmp_dir,
				       umount_hal_test_dir,
				       %% break,
				       ai_install,
				       %% break,
				       check_group_640_N_hal_glob,
				       %% break,
				       cleanup_hal_dir
				       %% cleanup_rcs_swm_sda1_if_needed
				      ]},
     %% %% No hw match neither in hal or gup.
     {group_640_E1,[sequence],[
			       %% %% set_not_existing_hw_id,
			       prep_640_E1_glob, %% set not supported hw in gup
			       umount_rcs_swm_sda1,
			       make_hal_test_dir_on_node,
			       mount_sda1_to_hal_test_dir,
			       prep_hal_1_on_node,
			       cp_hal_1_to_tmp_dir,
			       add_correct_hw_to_hal,
			       scp_hal1_sw_to_node,
			       %% %% add_not_existing_hw_to_hal_1, %not needed
			       cleanup_in_tmp_dir,
			       umount_hal_test_dir,
			       ai_err_seq,
			       check_post_group_640_E1,
			       cleanup_hal_dir
			       %% cleanup_rcs_swm_sda1_if_needed
			       %% %% set_org_hw_id
			      ]},
     %% %% hal and boardlist in gup is used.
     {group_640_A6_test1,[sequence], [
				      %%% gup Index TESTHAL1 match hal.  
				      %%% HW match in gup and hal. boardlist
				      prep_640_A6_glob,  %%set index TESTHAL1
				      umount_rcs_swm_sda1,
				      make_hal_test_dir_on_node,
				      mount_sda1_to_hal_test_dir,
				      prep_hal_1_on_node,
				      cp_hal_1_to_tmp_dir,
				      add_correct_hw_to_hal,
				      scp_hal1_sw_to_node,
				      cleanup_in_tmp_dir,
				      umount_hal_test_dir,
				      ai_install,
				      check_group_640_A6_test1,
				      cleanup_hal_dir
				      %% cleanup_rcs_swm_sda1_if_needed
				     ]},
     %% %% hal and conteninfo in gup is used.
     {group_640_A6_test2,[sequence], [
				      %%% gup Index TESTHAL1 match hal.  
				      %%% HW not match in gup. contentinfo shall be used
				      %% set index TESTHAL1, HW match in HAL.
				      %% Note, when RCSEE-DUS3 is used it must exist in HAL
				      %% due to it does not exist in contentinfo.
				      prep_640_A6_glob_test2,
				      umount_rcs_swm_sda1,
				      make_hal_test_dir_on_node,
				      mount_sda1_to_hal_test_dir,
				      prep_hal_1_on_node,
				      %% cp_hal_1_to_tmp_dir,
				      cp_hal_3_to_tmp_dir,
				      cp_ee_if_dus3_to_tmp_dir,
				      add_correct_hw_to_hal,
				      %% scp_hal1_sw_to_node,
				      scp_hal3_sw_to_node,
				      cleanup_in_tmp_dir,
				      umount_hal_test_dir,
				      ai_install,
				      check_group_640_A6_test2,
				      cleanup_hal_dir
				      %% cleanup_rcs_swm_sda1_if_needed
				     ]},
     %% %% hal and boardlist in gup is used.
     {group_multiple_hal,[sequence], [
				      %%% gup Index TESTHAL2 match hal.  
				      %%% HW match in gup and a hal. boardlist
				      prep_640_N_glob,  %% set index TESTHAL2
				      %% HAL-1
				      umount_rcs_swm_sda1,
				      make_hal_test_dir_on_node,
				      mount_sda1_to_hal_test_dir,
				      prep_hal_1_on_node,
				      cp_hal_1_to_tmp_dir,
				      add_correct_hw_to_hal,
				      scp_hal1_sw_to_node,
				      cleanup_in_tmp_dir,
				      %% HAL-2
				      prep_hal_2_on_node,
				      cp_hal_2_to_tmp_dir,
				      add_correct_hw_to_hal,
				      scp_hal2_sw_to_node, %% same as 1 is used.
				      cleanup_in_tmp_dir,
				      umount_hal_test_dir,
				      ai_install,
				      check_group_multiple_hal,
				      cleanup_hal_dir
				      %% cleanup_rcs_swm_sda1_if_needed
				     ]},
     {group_cleanup,[sequence], [
				 cleanup_hal_dir_no_check
				 %% cleanup_rcs_swm_sda1_if_needed
				]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [	 
    ].

break(_Config) ->
    test_server:break("### Break was ordered.").
	     

board_restore(Config) ->
    aic_httpc:board_restore(Config, ?Console).

download_files(Config) ->
    aic_httpc:download_files(Config, ?Console).

request_download_pogress(Config) ->
    aic_httpc:request_download_pogress(Config, "https").

integrate(Config) ->
    aic_httpc:integrate(Config, ?Console, ?NC_Session).


-define(HalTestDir, "/rcs/hal_test").
-define(HalSwpDir, "/rcs/hal_test/halswp").
-define(HalTest1Dir, "RCS-DUS2_CXS101549_8_TEST1").
-define(TcuHalTest1Dir, "RCS-T_CXS101549_9_TEST1").

make_hal_test_dir_on_node(_Config) ->
    rct_rs232:login(?Console),
    ct:pal("mkdir "++?HalTestDir),
    Cmd1 = "mkdir "++?HalTestDir,
    send_rpc_os_cmd(Cmd1),

    LS = send_rpc_os_cmd("ls /rcs/"),
    Ls = fix_str(LS, "\n "),
    ct:pal("ls /rcs: ~p", [Ls]),
    ok.

%% make_hal_swp_dir_on_node(_Config) ->
%%     ct:pal("mkdir "++?HalSwpDir),
%%     A = ct_telnet:send(?Console, "mkdir "++?HalSwpDir), 
%%     ct:pal(" A : ~p", [A]),
%%     timer:sleep(2000),
%%     ok.

prep_hal_1_on_node(Config) ->
    %% rct_rs232:login(?Console),
    %% ct:pal("mkdir "++?HalTestDir),
    %% Cmd1 = "mkdir "++?HalTestDir,
    %% send_rpc_os_cmd(Cmd1),

    %% LS = send_rpc_os_cmd("ls /rcs/"),
    %% Ls = fix_str(LS, "\n "),
    %% ct:pal("ls /rcs: ~p", [Ls]),

    %% ct:pal("mount /dev/sda1 on "++?HalTestDir),
    %% mount_sda1_to_hal_test_dir(Config),

    ct:pal("make dir halswp on mounted sda1"),
    ct:pal("mkdir "++?HalSwpDir),
    A = ct_telnet:send(?Console, "mkdir "++?HalSwpDir), 
    ct:pal(" A : ~p", [A]),
    timer:sleep(2000),
    BoardType = proplists:get_value(board_type, Config),
    HalDir = case BoardType of
		 BoardType when BoardType == "dus5201";
				BoardType == "dus5301";
				BoardType == "dus3301" ->
		     ?HalTest1Dir; 
		 %% BoardType when BoardType == "tcu03";
		 BoardType ="tcu0401" ->
		     ?TcuHalTest1Dir
	     end,
    ct:pal("mkdir for hal1 "++?HalSwpDir++"/"++HalDir),
    B = ct_telnet:send(?Console, "mkdir "++?HalSwpDir++"/"++HalDir),
    ct:pal(" B : ~p", [B]),
    timer:sleep(5000),
    Ls1 = send_rpc_os_cmd("ls "++?HalTestDir),
    match = check_str_exist_in_list(Ls1, "halswp"),
    Ls2 = send_rpc_os_cmd("ls "++?HalSwpDir),
    match = check_str_exist_in_list(Ls2, "_TEST1"),
    ok.

-define(HalTest2Dir, "RCS-DUS2_CXS101549_8_TEST2").
-define(TcuHalTest2Dir, "RCS-T_CXS101549_6_TEST2").
prep_hal_2_on_node(Config) ->
    BoardType = proplists:get_value(board_type, Config),
    timer:sleep(2000),
    HalDir = case BoardType of
		 BoardType when BoardType == "dus5201";
				BoardType == "dus5301";
				BoardType == "dus3301" ->
		     ?HalTest2Dir;
		 %% BoardType when BoardType == "tcu03";
		 BoardType = "tcu0401" ->
		     ?TcuHalTest2Dir
	     end,
    ct:pal("mkdir "++?HalSwpDir++"/"++HalDir),
    BB = ct_telnet:send(?Console, "mkdir "++?HalSwpDir++"/"++ HalDir),
    ct:pal(" BB : ~p", [BB]),
    timer:sleep(5000),
    Ls1 = send_rpc_os_cmd("ls "++?HalTestDir),
    match = check_str_exist_in_list(Ls1, "halswp"),
    Ls2 = send_rpc_os_cmd("ls "++?HalSwpDir),
    match = check_str_exist_in_list(Ls2, "_TEST2"),
    ok.

-define(FaultyProductNumber, "KDU888888/88").
-define(FaultyProductRevision, "R88A").

%% prep_640_E1(Config) ->
set_not_existing_hw_id(Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    break_auboot(console, Config),
    %% ProductNumber = "KDU888888/88",
    %% ProductRevision = "R88A",
    decupling_lib:change_hw_id(console, 
			       ?FaultyProductNumber, 
			       ?FaultyProductRevision),
    decupling_lib:start_node_from_uboot(?Console),
    decupling_lib:check_expected_hw_id(rpc_1, ?FaultyProductNumber, 
				       ?FaultyProductRevision),
    ok = wait_for_netconf_started().

    


add_not_existing_hw_to_hal_1(Config) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    %% OrgProdNr ="KDU137925/1",
    %% OrgProdRev = "R4A",
    add_hw_to_hal(Config, TmpDir, ?FaultyProductNumber, ?FaultyProductRevision).


set_org_hw_id(Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),

    OrgProdNr = proplists:get_value(org_prouct_number, Config),
    OrgProdRev = proplists:get_value(org_prod_rev, Config),
    ct:pal("# # OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),

    break_auboot(console, Config),
    %% %% ProductName = "Not existing HW",
    %% OrgProdNr = "KDU137925/3",
    %% OrgProdRev = "R5A",

    decupling_lib:change_hw_id(console, 
			       OrgProdNr, 
			       OrgProdRev),
    decupling_lib:start_node_from_uboot(console),
    decupling_lib:check_expected_hw_id(rpc_1, OrgProdNr, OrgProdRev),
    ok = wait_for_netconf_started().


send_rpc_os_cmd(Cmd) ->
    send_rpc_os_cmd(Cmd, 10000, print).
send_rpc_os_cmd(Cmd, Timeout, PrintOpt) ->
    Answ = rct_rpc:call(rpc_1, os, cmd, [Cmd], Timeout, PrintOpt),
    ct:log("Rpc Answ : ~p", [Answ] ),
    Answ.
    
umount_rcs_swm_sda1(Config) ->
    ct:pal("## umount /rcs/swm/sda1 due to it is mounted default"
	   " after Node start up."),
    LS1 = send_rpc_os_cmd("ls /rcs/swm/sda1"),
    ct:log("Ls before umount /rcs/swm/sda1 : ~n ~p", [fix_str(LS1, "\n ")]),
    Cmd="sudo cup --umount /rcs/swm/sda1",
    send_rpc_os_cmd(Cmd),
    timer:sleep(2000),
    LS2 = send_rpc_os_cmd("ls /rcs/swm/sda1"),
    ct:log("umount_rcs_swm_sda1 LS2: ~p", [LS2]),
    unmounted = check_if_dir_is_unmounted("/rcs/swm/sda1"),
    %% ct:log("Ls after umount /rcs/swm/sda1 : ~n ~p", [fix_str(LS2, "\n ")]),
    %% case LS2 of
    %% 	[] ->
    %% 	    ok;
    %% 	_Other ->
    %% 	    ct:fail("/rcs/swm/sda1 is not unmounted!!")
    %% end,
    
    LS_df = send_rpc_os_cmd("df -lT"),
    ct:log("LS_df : ~n ~p", [fix_str(LS_df, "\n ")]),
    BoardType = proplists:get_value(board_type, Config),
    timer:sleep(2000),
    case BoardType of
	"dus5201" -> %% new behaviour, /dev/sda1 mounts on another dir.
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
		check_if_dir_is_unmounted("/opt/rcs_ee/mounts/stats");

	    %% case LS4 of
	    %% 	[] ->
	    %% 	    ok;
	    %% 	_ ->
	    %% 	    ct:fail("/opt/rcs_ee/mounts/stats is not unmounted!!")
	    %% end;
	_ ->
	    ok
    end.

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

umount_hal_test_dir(_Config) ->
    %% rct_rpc:call(rpc_1, os, cmd, ["sudo cup --umount "++?SWM_MOUNT_DIR], 10000, 
    %% 		 print),
    Cmd="sudo cup --umount "++?HalTestDir,
    send_rpc_os_cmd(Cmd),
    timer:sleep(2000),
    LS = send_rpc_os_cmd("ls "++?HalTestDir),
    ct:log("## ls ~p: ~p", [?HalTestDir, fix_str(LS, "\n ")]).

%% Shall be used to restore node back.
mount_rcs_swm_sda1(_Config) ->
    ct:pal("## mount /dev/sda1 on /rcs/swm/sda1 due to it is mounted default"
	   " after Node start up."),
    LS1 = send_rpc_os_cmd("ls /rcs/swm/sda1"),
    ct:log("Ls before mount /rcs/swm/sda1 : ~n ~p", [fix_str(LS1, "\n ")]),
    Cmd = "sudo cup --mount '-o ro /dev/sda1 /rcs/swm/sda1'",
    send_rpc_os_cmd(Cmd),
    timer:sleep(2000),
    LS2 = send_rpc_os_cmd("ls /rcs/swm/sda1"),
    ct:log("Ls after mount /rcs/swm/sda1 : ~n ~p", [fix_str(LS2, "\n ")]).

mount_rcs_swm_sda1_rw(_Config) ->
    ct:pal("## mount /dev/sda1 on /rcs/swm/sda1 due to it is mounted default"
	   " after Node start up."),
    LS1 = send_rpc_os_cmd("ls /rcs/swm/sda1"),
    ct:log("Ls before mount /rcs/swm/sda1 : ~n ~p", [fix_str(LS1, "\n ")]),
    Cmd = "sudo cup --mount '-o rw /dev/sda1 /rcs/swm/sda1'",
    send_rpc_os_cmd(Cmd),
    timer:sleep(2000),
    LS2 = send_rpc_os_cmd("ls /rcs/swm/sda1"),
    ct:log("Ls after mount /rcs/swm/sda1 : ~n ~p", [fix_str(LS2, "\n ")]).


mount_sda1_to_hal_test_dir(_Config) ->
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
	    ct:fail("/dev/sda1 is not mounted on hal_test dir, fail the TC. ")
    end.


%% -define(DusHalXmlDir, "/proj/rcs-tmp/hw_sw_dec/nl_hal/dus/").
-define(DusHalXmlDir, "/proj/rcs-tmp/hw_sw_dec/nl_hal/dus/r7/dus2/").
-define(DusHalCxpDir, "/proj/rcs-tmp/hw_sw_dec/nl_hal/dus/cxps/").
%%%%
-define(DusHalXmlDir_dus3, "/proj/rcs-tmp/hw_sw_dec/nl_hal/dus/r8_dus3/").
-define(DusHalCxpDir_dus3, "/proj/rcs-tmp/hw_sw_dec/nl_hal/dus/r8_dus3/cxps/").
-define(DusHalCxpDir_dus3_EE, "/proj/rcs-tmp/hw_sw_dec/nl_hal/dus/r8_dus3/dummy_build_R8S08_with_EE_R8S04/").

%% -define(DusHalCxpDir, "/proj/rcs-tmp/hw_sw_dec/nl_hal/dus/r7/dus2_cxps/").
-define(HalTest1Xml, "test1_cxs101549-hal.xml").
-define(HalTest2Xml, "test2_cxs101549-hal.xml").
-define(HalTest3Xml, "test3_cxs101549-hal.xml").
%% Test is not runed on TCU
-define(TcuHalXmlDir, "/proj/rcs-tmp/hw_sw_dec/nl_hal/tcu/").
-define(TcuHalCxpDir, "/proj/rcs-tmp/hw_sw_dec/nl_hal/tcu/cxps/").

cp_hal_1_to_tmp_dir(Config) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    cp_hal_to_tmp_dir(Config, TmpDir, ?HalTest1Xml).

cp_hal_2_to_tmp_dir(Config) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    cp_hal_to_tmp_dir(Config, TmpDir, ?HalTest2Xml).

cp_hal_3_to_tmp_dir(Config) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of 
	BoardType when BoardType == "dus5201";
		       BoardType == "dus3201" ->
	    cp_hal_to_tmp_dir(Config, TmpDir, ?HalTest1Xml);
	%% This is used when kdu not match GUP the RCSEE-DUS3, then it not exist in contentinfo.
	BoardType when BoardType == "dus5301"; 
		       BoardType == "dus3301" ->
	    cp_hal_to_tmp_dir(Config, TmpDir, ?HalTest3Xml)
    end.


cp_hal_to_tmp_dir(Config, TmpDir, HalTestXml) ->
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of 
	BoardType when BoardType == "dus5201";
		       BoardType == "dus3201" ->
	    send_os_cmd("cp "++?DusHalXmlDir++HalTestXml++" "++TmpDir),
	    send_os_cmd("cp "++?DusHalCxpDir++"* "++TmpDir);
	%% BoardType when BoardType == "tcu03";
	BoardType when BoardType == "dus5301";
		       BoardType == "dus3301" ->
	    send_os_cmd("cp "++?DusHalXmlDir_dus3++HalTestXml++" "++TmpDir),
	    send_os_cmd("cp "++?DusHalCxpDir_dus3++"*.cxp "++TmpDir)
    end,
    Ls = fix_str(send_os_cmd("ls "++TmpDir), "\n\r\"> "),
    ct:pal("Ls TmpDir:~n~p~n",[Ls]),
    match = check_str_exist_in_list(Ls, "-hal.xml"),
    match = check_str_exist_in_list(Ls, ".cxp"),
    ok.

cp_ee_if_dus3_to_tmp_dir(Config) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of 
	BoardType when BoardType == "dus5201";
		       BoardType == "dus3201" ->
	    ct:pal("Dus2 is used. Do not cp due to boundle exist in contentinfo");
	BoardType when BoardType == "dus5301";
		       BoardType == "dus3301" ->
	    ct:pal("Dus3 is used. Need to use RCSEE-DUS3  due to it NOT exist in contentinfo"),
	    send_os_cmd("cp "++?DusHalCxpDir_dus3_EE++"RCSEE-DUS3*.cxp "++TmpDir),
	    Ls = fix_str(send_os_cmd("ls "++TmpDir), "\n\r\"> "),
	    ct:pal("Ls TmpDir:~n~p~n",[Ls]),
	    match = check_str_exist_in_list(Ls, "RCSEE-DUS3")
    end.

cleanup_in_tmp_dir(Config) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    send_os_cmd("rm -rf "++TmpDir++"*.xml"),
    send_os_cmd("rm -rf "++TmpDir++"*.cxp"),
    Ls = send_os_cmd("ls "++TmpDir++"*.cxp"),
    match = check_str_exist_in_list(Ls, "cannot access"),
    ct:pal("Ls TmpDir after remove, ~p", [fix_str(Ls,"\n\"> ")]),
    ok.

%% add_correct_hw_to_hal_1(Config) ->
add_correct_hw_to_hal(Config) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    OrgProdNr = proplists:get_value(org_prouct_number, Config),
    OrgProdRev = proplists:get_value(org_prod_rev, Config),
    %% OrgProdNr ="KDU137925/1",
    %% OrgProdRev = "R4A",
    add_hw_to_hal(Config, TmpDir, OrgProdNr, OrgProdRev).

add_hw_to_hal(_Config, HalTmpDir, ProdNr, ProdRev) ->
    CMD1 = "cd "++HalTmpDir++
    	";"++"sed -i '/<boardList hwcategory=/a <boardType productNumber=\""++
	ProdNr++"\" revision=\""++
	ProdRev++"\" \/>' "++"*-hal.xml",
    send_os_cmd(CMD1),
    timer:sleep(2000),
    Grep = send_os_cmd("grep "++ProdNr++" "++HalTmpDir++"*-hal.xml"),
    ct:log("Grep : ~p", [Grep]),
    Grep1= fix_str(Grep, "\n\r\"> "),
    ct:log("Grep1 : ~p", [Grep1]),
    match = check_str_exist_in_list(Grep, ProdNr),
    ok.

-define(Cobra1, "COBRA_CXP102171_1.cxp").
-define(Cobra2, "COBRA_CXP102188_2.cxp").
-define(TIGER, "TIGER_CXP102194_1.cxp").
-define(MTIGER, "MTIGER_CXP102201_1.cxp").
-define(RCSEE_DUS3, "RCSEE-DUS3_CXP9025317_6.cxp").
-define(KATLA, "KATLA_CXP102185_1.cxp").
-define(TAIPAN, "TAIPAN_CXP102172_2.cxp").
scp_hal1_sw_to_node(Config) ->
    BoardType = proplists:get_value(board_type, Config),
    HalDirName =  case BoardType of
		      BoardType when BoardType == "dus5201";
				     BoardType == "dus5301";
				     BoardType == "dus3301" ->
			  ?HalTest1Dir;
		      %% BoardType when BoardType == "tcu03";
		      BoardType = "tcu0401" ->
			  ?TcuHalTest1Dir
		  end,
    scp_hal_sw_to_hal_dir(Config, HalDirName, ?HalTest1Xml).

scp_hal2_sw_to_node(Config) ->
    BoardType = proplists:get_value(board_type, Config),
    HalDirName = case BoardType of
		     BoardType when BoardType == "dus5201";
				    BoardType == "dus5301";
				    BoardType == "dus3301" ->
			 ?HalTest2Dir;
		     %% BoardType when BoardType == "tcu03";
		     BoardType = "tcu0401" ->
			 ?TcuHalTest2Dir
		 end,
    scp_hal_sw_to_hal_dir(Config, HalDirName, ?HalTest2Xml).

scp_hal3_sw_to_node(Config) ->
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of
	BoardType when BoardType == "dus5201";
		       BoardType == "dus3201" ->
	    scp_hal_sw_to_hal_dir(Config, ?HalTest1Dir, ?HalTest1Xml);
	%% BoardType when BoardType == "tcu03";
	BoardType when BoardType == "dus5301"; 
		       BoardType == "dus3301"->
	    scp_hal_sw_to_hal_dir(Config, ?HalTest1Dir, ?HalTest3Xml)
    end.

    %% HalDirName =  case BoardType of
    %% 		      BoardType when BoardType == "dus5201";
    %% 				     BoardType == "dus5301";
    %% 				     BoardType == "dus3301" ->
    %% 			  ?HalTest1Dir;
    %% 		      %% BoardType when BoardType == "tcu03";
    %% 		      BoardType = "tcu0401" ->
    %% 			  ?TcuHalTest1Dir
    %% 		  end,
    %% scp_hal_sw_to_hal_dir(Config, HalDirName, ?HalTest3Xml).

scp_hal_sw_to_hal_dir(Config, HalDirName, HalXml) ->
    BoardType = proplists:get_value(board_type, Config),
    TmpDir = proplists:get_value(tmp_dir, Config),
    ct:pal("TmpDir:~n~p~n",[TmpDir]),
    FileList = case BoardType of
		   BoardType when BoardType == "dus5201";
				  BoardType == "dus3201" ->
		       [HalXml, ?Cobra1, ?Cobra2];
		   %% BoardType when BoardType == "tcu03";
		   BoardType when BoardType == "dus5301";
				  BoardType == "dus3301" ->
		       LsTmpDir = fix_str(send_os_cmd("ls "++TmpDir), "\n\r\"> "),
		       ct:pal("Ls TmpDir:~n~p~n",[LsTmpDir]),
		       case check_str_exist_in_list(LsTmpDir, "RCSEE-DUS3") of
			   match ->
			       [HalXml, ?TIGER, ?MTIGER, ?RCSEE_DUS3];
			   _Other -> %% Test not need RCSEE-DUS3
			       [HalXml, ?TIGER, ?MTIGER]
		       end
	       end,
    HalToPath = ?HalSwpDir++"/"++HalDirName++"/",
    ct:pal("scp cxps and cxs to hal dir on node."),
    ct:pal("FileList:~n~p~n",[FileList]),
    ct:pal("HalToPath:~n~p~n",[HalToPath]),
    %% FileList = fix_str(os:cmd("ls "++Tmp_Dir), "\n"),
    lists:foreach(fun(X) ->
			  A = rct_scp:to_target(scp, 
						TmpDir++X, 
						HalToPath, 
						60000, noprint),
			  ct:log("# scp : ~p", [A])
		  end, FileList),
    timer:sleep(1000),    

    Ls = rct_rpc:call(rpc_1, os, cmd, ["ls "++HalToPath], 10000, 
    		      noprint),
    LS = fix_str(Ls, "\n\r\"> "),
    ct:pal("# LS after scp : ~p", [LS]),
    match = check_str_exist_in_list(LS, "-hal.xml"),
    match = check_str_exist_in_list(LS, ".cxp").



cleanup_hal_dir(Config) ->
    ct:pal("## cleanup_hal_dir ##"),
    ct:pal("## First umount is needed ##"),
    umount_rcs_swm_sda1(Config),
    %%
    ct:pal("## create /rcs/hal_test dir ##"),
    ct:pal("mkdir "++?HalTestDir),
    Cmd1 = "mkdir "++?HalTestDir,
    send_rpc_os_cmd(Cmd1),
    LS = send_rpc_os_cmd("ls /rcs/"),
    Ls = fix_str(LS, "\n "),
    ct:pal("ls /rcs: ~p", [Ls]),

    %%
    ct:pal("## Now mount sda1 ti haltest dir ##"),
    ct:pal("# Mount : /dev/sda1 to ~p", [?HalTestDir]),
    mount_sda1_to_hal_test_dir(Config),
    Console = ?Console,
    rct_rs232:login(Console),
    %% ct_telnet:expect(Console, "login:", 
    %% 			  [{timeout,30000}, no_prompt_check]),

    ct:pal("Remove all under : ~p", [?HalSwpDir]),
    ct_telnet:send(Console, "ls "++?HalSwpDir),
    %% A = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"/*_TEST*"),
    A = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"/"++?HalTest1Dir),
    ct:log("A : ~p", [A]),
    B = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"/"++?HalTest2Dir),
    ct:log("B : ~p", [B]),

    timer:sleep(10000),
    ct_telnet:send(Console, "ls "++?HalSwpDir),
    {error,timeout} = ct_telnet:expect(Console, "_TEST", 
    				       [{timeout,5000}, no_prompt_check]),

    ct:pal("# Umount : ~p", [?HalTestDir]),
    umount_hal_test_dir(Config),

    Ls2 = send_rpc_os_cmd("ls "++?HalTestDir),
    Ls2 = fix_str(Ls2, "\n"),
    ct:log(" Ls check  : ~p", [Ls2]),
    case check_str_exist_in_list(Ls2, "halswp") of
	match ->
	    ct:fail("/rcs/hal_test/halswp still exist Unexpected!, Fail TC");
	nomatch ->
	    ct:pal("/rcs/hal_test/halswp not exist and it is Expected"),
	    ok
    end.

cleanup_hal_dir_no_check(Config) ->
    ct:pal("cleanup_hal_dir_no_check"),

    umount_rcs_swm_sda1(Config),
    ct:pal("## create /rcs/hal_test dir even if it exist ##"),
    ct:pal("mkdir "++?HalTestDir),
    Cmd1 = "mkdir "++?HalTestDir,
    send_rpc_os_cmd(Cmd1),
    ct:pal("# Mount : /dev/sda1 to ~p", [?HalTestDir]),
    mount_sda1_to_hal_test_dir(Config),
    Console = ?Console,
    rct_rs232:login(Console),

    ct:pal("Remove all under : ~p", [?HalSwpDir]),
    ct_telnet:send(Console, "ls "++?HalSwpDir),
    %% A = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"/*_TEST*"),
    A = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"/"++?HalTest1Dir),
    ct:pal("A : ~p", [A]),
    B = ct_telnet:send(Console, "rm -rf "++?HalSwpDir++"/"++?HalTest2Dir),
    ct:log("B : ~p", [B]),

    timer:sleep(10000),
    ct:pal("# Umount : ~p", [?HalTestDir]),
    umount_hal_test_dir(Config),

    Ls = send_rpc_os_cmd("ls "++?HalTestDir),
    ct:log(" Ls ~p : ~p", [?HalTestDir, Ls]),
    ok.


%% cleanup_rcs_swm_sda1_if_needed(Config) ->
%%     ct:pal("cleanup_rcs_swm_sda1_if_needed"),
%%     umount_hal_test_dir(Config),
%%     umount_rcs_swm_sda1(Config),
%%     ct:pal("# Mount rw : /dev/sda1 to /rcs/swm/sda1", []),
%%     mount_rcs_swm_sda1_rw(Config),

%%     Ls = send_rpc_os_cmd("ls /rcs/swm/sda1/"),
%%     Ls1 = fix_str(Ls, "\n"),
%%     ct:pal(" Ls  /rcs/swm/sda1/ : ~p", [Ls1]),

%%     case check_str_exist_in_list(Ls1, "halswp") of
%% 	match ->
%% 	    ct:pal("## halswp exist unexpected!, remove rubbish!"),
%% 	    remove_rcs_swm_sda1_halswp();
%% 	nomatch ->
%% 	    ct:pal("## halswp not exist and it is Expected"),
%% 	    ok
%%     end.

%% remove_rcs_swm_sda1_halswp() ->
%%     Console = ?Console,
%%     rct_rs232:login(Console),

%%     ct:pal("Remove all under /rcs/swm/sda1/halswp/*", []),
%%     A = ct_telnet:send(Console, "rm -rf /rcs/swm/sda1/halswp/*"),
%%     ct:pal("A : ~p", [A]),
%%     timer:sleep(2000),
%%     Ls = send_rpc_os_cmd("ls /rcs/swm/sda1/halswp/"),
%%     ct:pal(" Ls after rm  : ~p", [Ls]),
    
%%     ct:pal("Remove halswp under /rcs/swm/sda1/", []),
%%     B = ct_telnet:send(Console, "rm -rf /rcs/swm/sda1/halswp"),
%%     ct:pal("B : ~p", [B]),
%%     timer:sleep(5000),
%%     Ls2 = send_rpc_os_cmd("ls /rcs/swm/sda1/"),
%%     Ls3 = fix_str(Ls2, "\n"),
%%     ct:pal(" Ls after rm  : ~p", [Ls3]),
%%     Ls3 = fix_str(Ls2, "\n"),

%%     case check_str_exist_in_list(Ls3, "halswp") of
%% 	match ->
%% 	    ct:fail("/rcs/swm/sda1/halswp still exist Unexpected!, Fail TC");
%% 	nomatch ->
%% 	    ct:pal("halswp not exist and it is Expected"),
%% 	    ok
%%     end,
%%     ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% Shall be used un cleanupo if tc failed and it is stucked in NL
%%% @spec prep_org_glob(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prep_org_glob(Config) ->
    ct:pal("prep orginal gup"),
    TftpDir = proplists:get_value(tftp_dir, Config),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),
    rctprep_zip_glob_to_be_used(Config).

%%%--------------------------------------------------------------------
%%% @doc
%%% NodeUC 640.N: Network loader - Semi Automatic. <br/>
%%% Test1, only global exist, no Hal.
%%% @spec prep_640_N_glob(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prep_640_N_glob(Config) ->
    TftpDir = proplists:get_value(tftp_dir, Config),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    rctprep_zip_glob_to_be_used(Config),
    {RCS_Name, CxsXml_Name} = cp_zip_to_tmp_dir_and_unzip(TftpDir, Tmp_Dir),
    set_glob_index(Tmp_Dir, CxsXml_Name, "TESTHAL2"),
    add_dummy_cxp_in_contentinfo(Tmp_Dir, CxsXml_Name),
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of
	BoardType when BoardType == "dus5201";
		       BoardType == "dus5301";
		       BoardType == "dus3301" ->
	    add_hwcat_other_with_radio_cxps(Tmp_Dir, CxsXml_Name); 
	_OtherBoards  ->
	    ct:pal("No add of hwcategory OTHER will be done")
    end,
    zip_and_mv_to_tftp_dir(TftpDir, Tmp_Dir, RCS_Name),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% NodeUC 640.A6: Network loader - Semi Automatic HAL-SWMP is used. <br/>
%%% Create a gup with index match HAL. HW match HAL.
%%% @spec prep_640_A6_glob(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prep_640_A6_glob(Config) ->
    TftpDir = proplists:get_value(tftp_dir, Config),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    ct:log("rctprep_zip_glob_to_be_used"),
    rctprep_zip_glob_to_be_used(Config), %% Not needed when zip is used default.
    {RCS_Name, CxsXml_Name} = cp_zip_to_tmp_dir_and_unzip(TftpDir, Tmp_Dir),
    ct:log("set_glob_index"),
    set_glob_index(Tmp_Dir, CxsXml_Name, "TESTHAL1"), %% Index match hal swp
    ct:log("add_dummy_cxp_in_contentinfo"),
    add_dummy_cxp_in_contentinfo(Tmp_Dir, CxsXml_Name),
    ct:log("zip_and_mv_to_tftp_dir"),
    zip_and_mv_to_tftp_dir(TftpDir, Tmp_Dir, RCS_Name),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% NodeUC 640.A6: Network loader - Semi Automatic HAL-SWMP is used. <br/>
%%% Create a gup with index match HAL. HW not match HAL.
%%% @spec prep_640_A6_glob_test2(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prep_640_A6_glob_test2(Config) ->
    TftpDir = proplists:get_value(tftp_dir, Config),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    rctprep_zip_glob_to_be_used(Config),
    {RCS_Name, CxsXml_Name} = cp_zip_to_tmp_dir_and_unzip(TftpDir, Tmp_Dir),
    set_glob_index(Tmp_Dir, CxsXml_Name, "TESTHAL1"), %% Index match hal swp
    set_not_suported_board_in_gup(Config, Tmp_Dir, CxsXml_Name),
    add_dummy_cxp_in_contentinfo(Tmp_Dir, CxsXml_Name),
    zip_and_mv_to_tftp_dir(TftpDir, Tmp_Dir, RCS_Name),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% NodeUC 640.E1: Failed precondition. <br/>
%%% Create a gup with HW and index not match HAL.
%%% @spec prep_640_E1_glob(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prep_640_E1_glob(Config) ->
    TftpDir = proplists:get_value(tftp_dir, Config),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),
    Tmp_Dir = proplists:get_value(tmp_dir, Config),
    rctprep_zip_glob_to_be_used(Config),
    {RCS_Name, CxsXml_Name} = cp_zip_to_tmp_dir_and_unzip(TftpDir, Tmp_Dir),
    set_glob_index(Tmp_Dir, CxsXml_Name, "TESTHAL2"),
    set_not_suported_board_in_gup(Config, Tmp_Dir, CxsXml_Name),
    add_dummy_cxp_in_contentinfo(Tmp_Dir, CxsXml_Name),
    zip_and_mv_to_tftp_dir(TftpDir, Tmp_Dir, RCS_Name),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% set_not_suported_board_in_gup(Config) -> ok
%% @end
%%--------------------------------------------------------------------
set_not_suported_board_in_gup(Config, Tmp_Dir, CxsXml_Name) ->
    OrgProdNr = proplists:get_value(org_prouct_number, Config),
    OrgProdRev = proplists:get_value(org_prod_rev, Config),
    ct:pal("# # OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),
    [OrgKdu , OrgInd] = string:tokens(OrgProdNr, "/"),
    ct:log("## OrgKdu: ~p, OrgInd: ~p", [OrgKdu, OrgInd]),

    NotExistingProdNr = OrgKdu++"/99",
    ct:pal("## Update gup: ProdNrs ~p, to ~p", [OrgProdNr, NotExistingProdNr]),

    CMD1 = "cd "++Tmp_Dir++
    	" ; "++"sed -i 's/"++OrgKdu++"\\/"++OrgInd++"/"++OrgKdu++"\\/99/g' " ++ 
	CxsXml_Name,
    %% send_os_cmd(CMD1),    
    %% ct:log(" #CMD1: ~p", [CMD1]),
    ct:log("# os cmd #: ~s", [CMD1]),
    Answ = os:cmd(CMD1),
    ct:log("# Answ : ~p", [Answ]),
    ct:log(" # cat: ~p", 
	   [fix_str(os:cmd("cat "++Tmp_Dir++"/"++CxsXml_Name), "\"\n </>")]),
    timer:sleep(2000),
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

    %% BoardType = proplists:get_value(board_type, Config),
    %% ZipDir = case BoardType of
    %% 		 BoardType when BoardType == "dus5201";
    %% 				BoardType == "dus5301";
    %% 		  		BoardType == "dus3301" ->
    %% 		     "https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCS-DUS2_CXS101549_8/doc/19010/RCS-DUS2_CXS101549_8.zip@@/"++
    %% 			 CXS_LABEL;
    %% 		 "tcu0401" ->
    %% 		     "https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCS-T_CXS101549_9/doc/19010/RCS-T_CXS101549_9.zip@@/"++
    %% 			 CXS_LABEL
    %% 	     end,
    Node = proplists:get_value(nodeName, Config),

    %% RcstPrep_Cmd = "rcstprep.sh "++Node++" "++ZipDir,
    RcstPrep_Cmd = "rcstprep.sh "++Node++" "++CXS_LABEL,
    os:cmd(RcstPrep_Cmd),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @spec ai_err_seq(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
ai_err_seq(Config) ->
    ct:pal("## Start board_restore"),
    board_restore(Config),
    ct:pal("## Start Download"),
    timer:sleep(5000),
    nl_lib:httpc_request(Config, post, "Download", "https"),
    timer:sleep(5000),
    case ct_telnet:expect(console, 
			  "BoardType not supported by neither "
			  "HalUP nor GlobalUP", 
			  [{timeout,30000}, no_prompt_check]) of
	{ok,_} ->
	    ct_telnet:expect(console, 
			     "Autointegration waiting for user input",
			     [{timeout,30000}, no_prompt_check]),
	    ct:pal("## Download failed, it is expected"),
	    startup_node_ok(Config),
	    ok;
	_Else ->
	    startup_node_ok(Config),
	    ct:fail("TC fail due to download result was not expected.")
    end.

startup_node_ok(Config) ->
    prep_640_N_glob(Config), %% index with hw match
    download_files(Config),
    integrate(Config).


%%%--------------------------------------------------------------------
%%% @doc
%%% @spec check_group_640_N_glob(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
check_group_640_N_glob(Config) ->
    mount_rcs_swm_sda1(Config),
    check_selected_vs_installed_cxps(Config),
    GlobalSwpId = get_global_swp_id(),
    SwP_Selected = get_swp_selected(),

    check_expected_swp_selected_1(Config, SwP_Selected, GlobalSwpId),
    %% No hal shall be selected.
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of
	BoardType when BoardType == "dus5201";
		       BoardType == "dus5301";
		       BoardType == "dus3301" ->
	    %% NrOfExpHalCxp, NrOfExpGlobCxp
	    check_nr_exp_hal_and_glob(Config, 0, 5+3); %% use 2 RU cxps in OTHER +3 FPGA cxps
	"tcu0401" ->
	    check_nr_exp_hal_and_glob(Config, 0, 5)
    end,

    %% check_nr_exp_hal_and_glob(Config, 0, 6), %% NrOfExpHalCxp, NrOfExpGlobCxp
    umount_rcs_swm_sda1(Config),
    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%% @spec check_group_640_N_hal_glob(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
check_group_640_N_hal_glob(Config) ->
    mount_rcs_swm_sda1(Config),
    check_selected_vs_installed_cxps(Config),
    GlobalSwpId = get_global_swp_id(),
    SwP_Selected = get_swp_selected(),
    check_expected_swp_selected_1(Config, SwP_Selected, GlobalSwpId),
    %% No hal shall be selected.
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of
	BoardType when BoardType == "dus5201";
		       BoardType == "dus5301";
		       BoardType == "dus3301" ->
	    %% NrOfExpHalCxp, NrOfExpGlobCxp
	    check_nr_exp_hal_and_glob(Config, 0, 5+3); %% use 2 RU cxps in OTHER +3 FPGA cxps
	"tcu0401" ->
	    check_nr_exp_hal_and_glob(Config, 0, 4)
    end,
    %% check_nr_exp_hal_and_glob(Config, 0, 6), %% NrOfExpHalCxp, NrOfExpGlobCxp
    umount_rcs_swm_sda1(Config),
    ok.

check_post_group_640_E1(Config) ->
    %% Same result as 640_N
    check_group_640_N_hal_glob(Config).


-define(ExpHalList_dus2, [{{"COBRA","CXP102171_1","R31B01"},
			   {hal,"COBRA_CXP102171_1.cxp"}}]).
-define(ExpHalList_dus3, [{{"TIGER","CXP102194_1","R11A01"},
			   {hal,"TIGER_CXP102194_1.cxp"}}
			 ]).
-define(ExpHalList_dus3_test2, [{{"TIGER","CXP102194_1","R11A01"},
				 {hal,"TIGER_CXP102194_1.cxp"}},
				{{"RCSEE-DUS3","CXP9025317_6","R8S04"},
				 {hal,"RCSEE-DUS3_CXP9025317_6.cxp"}}
			       ]).

%%%--------------------------------------------------------------------
%%% @doc
%%% @spec check_group_640_A6_test1(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
check_group_640_A6_test1(Config) ->
    mount_rcs_swm_sda1(Config),
    check_selected_vs_installed_cxps(Config),
    GlobalSwpId = get_global_swp_id(),
    SwP_Selected = get_swp_selected(),
    check_expected_swp_selected_2(Config, SwP_Selected, GlobalSwpId),
    %% 1 hal cxp shall be selected.
    %% check_nr_exp_hal_and_glob(Config, 1, 3), %% NrOfExpHalCxp, NrOfExpGlobCxp
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of
	"dus5201" ->
	    check_exp_hal_is_selected(?ExpHalList_dus2);
	BoardType when BoardType == "dus5301";
		       BoardType == "dus3301" ->
	    check_exp_hal_is_selected(?ExpHalList_dus3)
    end,
    %% mount_rcs_swm_sda1(Config),
    umount_rcs_swm_sda1(Config),
    ok.


check_exp_hal_is_selected(ExpHalList) ->
    HwId = decupling_lib:get_hw(rpc_1),
    SelProducts = decupling_lib:get_selected_products(rpc_1, HwId),
    ct:pal("# SelProducts: ~p", [SelProducts]),
    
    lists:foreach(fun(HalElement) ->
			  ct:log("## Expected  HalElement: ~n~p", [HalElement]),
			  true = lists:member(HalElement, 
					      SelProducts)
		  end, ExpHalList),
    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%% @spec check_group_640_A6_test2(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
check_group_640_A6_test2(Config) ->
    mount_rcs_swm_sda1(Config),
    check_selected_vs_installed_cxps(Config),
    GlobalSwpId = get_global_swp_id(),
    SwP_Selected = get_swp_selected(),
    check_expected_swp_selected_3(Config, SwP_Selected, GlobalSwpId),
    %% 1 hal cxp shall be selected.
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of
	"dus5201" ->
	    check_exp_hal_is_selected(?ExpHalList_dus2);
	BoardType when BoardType == "dus5301";
		       BoardType == "dus3301" ->
	    check_exp_hal_is_selected(?ExpHalList_dus3_test2)
    end,
    %% check_nr_exp_hal_and_glob(Config, 1, 3), %% NrOfExpHalCxp, NrOfExpGlobCxp
    %% %% mount_rcs_swm_sda1(Config),
    umount_rcs_swm_sda1(Config),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @spec check_group_multiple_hal(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
check_group_multiple_hal(Config) ->
    mount_rcs_swm_sda1(Config),
    check_selected_vs_installed_cxps(Config),
    GlobalSwpId = get_global_swp_id(),
    SwP_Selected = get_swp_selected(),
    check_expected_swp_selected_4(Config, SwP_Selected, GlobalSwpId),
    _BoardType = proplists:get_value(board_type, Config),
    check_nr_exp_hal_and_glob(Config, 2, 5),
    %% case BoardType of
    %% 	BoardType when BoardType == "dus5201";
    %% 		       BoardType == "dus3201" ->
    %% 	    %% NrOfExpHalCxp, NrOfExpGlobCxp
    %% 	    check_nr_exp_hal_and_glob(Config, 2, 4); %% use 2 RU cxps in OTHER
    %% 	BoardType when BoardType == "dus5301";
    %% 		       BoardType == "dus3301" ->
    %% 	    check_nr_exp_hal_and_glob(Config, 2, 5) 
    %% end,
    umount_rcs_swm_sda1(Config),
    ok.

%%%--------------------------------------------------------------------
%%% Internal for the check result.
%%%--------------------------------------------------------------------
check_selected_vs_installed_cxps(Config) ->
    BoardType = proplists:get_value(board_type, Config),
    case BoardType of
	"dus5201" -> %% EE is boundled
	    decupling_lib:check_selected_vs_installed_cxps(rpc_1, ?NC_Session);
	_ ->  %% No need to check due to EE is not bundle.
	    ok 
    end.

get_global_swp_id() ->
    Grep = rct_rpc:call(rpc_1, os, cmd,
			["grep CXS101549_* /home/sirpa/software/cxs101549*-up.xml"], 10000, print),
    GrepList = fix_str(Grep, "\"\n </>"),
    ct:pal("GrepList : ~n~p", [GrepList]),
    GlobalSwpId = decupling_lib:get_name_id_version(GrepList),
    ct:pal("GlobalSwpId : ~n~p", [GlobalSwpId]),
    GlobalSwpId.

get_swp_selected() ->
    SwP_Selected = rct_rpc:call(rpc_1, 
				swmBoardList, 
				read_swp_selected, [], 10000, noprint),
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),
    SwP_Selected.

%% Only boardList from global shall be used.
check_expected_swp_selected_1(Config, SwP_Selected, GlobalSwpId) ->
    BoardType = proplists:get_value(board_type, Config),
    GlobBB = get_bb_type_exp_for_glob(BoardType),
    %% when only globals is selected.
    SwP_Selected = [{hwSwCompatibility,[{index,"TESTHAL2"}]},
    			    {swp_id,[{global,GlobalSwpId}]},
    			    {boardList,[{global,{"BASEBAND",GlobBB}},
    					{global,{"OTHER","FAKE_OTHER"}}]}].
    
    %% SwP_Selected = case BoardType of
    %% 		       BoardType when BoardType == "dus3201";
    %% 				      BoardType == "dus5201" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL2"}]},
    %% 			    {swp_id,[{global,GlobalSwpId}]},
    %% 			    {boardList,[{global,{"BASEBAND",GlobBB}},
    %% 					{global,{"OTHER","FAKE_OTHER"}}]}];
    %% 		       BoardType when BoardType == "dus5301";
    %% 				      BoardType == "dus3301" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL2"}]},
    %% 			    {swp_id,[{global,GlobalSwpId}]},
    %% 			    {boardList,[{global,{"BASEBAND",GlobBB}},
    %% 					{global,{"OTHER","FAKE_OTHER"}}]}]
    %% 		       %% BoardType when BoardType == "dus5301";
    %% 		       %% "tcu0401" ->
    %% 		       %% 	   [{hwSwCompatibility,[{index,"TESTHAL2"}]},
    %% 		       %% 	    {swp_id,[{global,GlobalSwpId}]},
    %% 		       %% 	    {boardList,[{global,{"BASEBAND-T",GlobBB}}]}]
    %% 		       %% %% "dus5201" ->
    %% 		       %% %% 	   [{hwSwCompatibility,[{index,"TESTHAL2"}]},
    %% 		       %% %% 	    {swp_id,[{global,GlobalSwpId}]},
    %% 		       %% %% 	    {boardList,[{global,{"BASEBAND","BB5216"}}]}]
		   %% end.

%% boardList from global shall be used and hal.
check_expected_swp_selected_2(Config, SwP_Selected, GlobalSwpId) ->
    BoardType = proplists:get_value(board_type, Config),
    GlobBB = get_bb_type_exp_for_glob(BoardType),
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),
    SwP_Expected = [{hwSwCompatibility,[{index,"TESTHAL1"}]},
		    {swp_id,[{global,GlobalSwpId},
			     {hal,?HalTest1Dir}]},
		    {boardList,[{global,{"BASEBAND",GlobBB}},
				{hal,{"BASEBAND",GlobBB}}]}],
    %% SwP_Expected = case BoardType of
    %% 		       BoardType when BoardType == "dus3201";
    %% 				      BoardType == "dus5201" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL1"}]},
    %% 			    {swp_id,[{global,GlobalSwpId},
    %% 				     {hal,?HalTest1Dir}]},
    %% 			    {boardList,[{global,{"BASEBAND",GlobBB}},
    %% 					{hal,{"BASEBAND","BB521_S"}}]}];
    %% 		       BoardType when BoardType == "dus5301";
    %% 				      BoardType == "dus3301" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL1"}]},
    %% 			    {swp_id,[{global,GlobalSwpId},
    %% 				     {hal,?HalTest1Dir}]},
    %% 			    {boardList,[{global,{"BASEBAND",GlobBB}},
    %% 					{hal,{"BASEBAND","BB662_3"}}]}]
    %% 		   end,
    ct:pal("# SwP_Expected: ~p", [SwP_Expected]),
    Sorted_SwP_Selected= sort_lists(SwP_Selected),
    ct:log("# Sorted_SwP_Selected: ~p", [Sorted_SwP_Selected]),
    Sorted_SwP_Expected= sort_lists(SwP_Expected),
    ct:log("# Sorted_SwP_Expected: ~p", [Sorted_SwP_Expected]),
    true = Sorted_SwP_Selected == Sorted_SwP_Expected.


%% contentInfo from global shall be used and hal.
check_expected_swp_selected_3(Config, SwP_Selected, GlobalSwpId) ->
    BoardType = proplists:get_value(board_type, Config),
    GlobBB = get_bb_type_exp_for_glob(BoardType),
    %% when only globals is selected.
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),
    SwP_Expected = [{hwSwCompatibility,[{index,"TESTHAL1"}]},
		    {swp_id,[{global,GlobalSwpId},
			     {hal,?HalTest1Dir}]},
		    {contentinfo,[global]},
		    {boardList,[{hal,{"BASEBAND",GlobBB}}]}],
    %% SwP_Expected = case BoardType of
    %% 		       BoardType when BoardType == "dus3201";
    %% 				      BoardType == "dus5201" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL1"}]},
    %% 		       	    {swp_id,[{global,GlobalSwpId},
    %% 		       		     {hal,?HalTest1Dir}]},
    %% 		       	    {contentinfo,[global]},
    %% 		       	    {boardList,[{hal,{"BASEBAND","BB5216"}}]}];
    %% 		       BoardType when BoardType == "dus5301";
    %% 				      BoardType == "dus3301" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL1"}]},
    %% 		       	    {swp_id,[{global,GlobalSwpId},
    %% 		       		     {hal,?HalTest1Dir}]},
    %% 		       	    {contentinfo,[global]},
    %% 		       	    {boardList,[{hal,{"BASEBAND","BB662_3"}}]}]
    %% 		   end,
    ct:pal("# SwP_Expected: ~p", [SwP_Expected]),
    Sorted_SwP_Selected= sort_lists(SwP_Selected),
    ct:log("# Sorted_SwP_Selected: ~p", [Sorted_SwP_Selected]),
    Sorted_SwP_Expected= sort_lists(SwP_Expected),
    ct:log("# Sorted_SwP_Expected: ~p", [Sorted_SwP_Expected]),
    true = Sorted_SwP_Selected == Sorted_SwP_Expected.


%% boardList from global shall be used and hal-2.
check_expected_swp_selected_4(Config, SwP_Selected, GlobalSwpId) ->
    BoardType = proplists:get_value(board_type, Config),
    GlobBB = get_bb_type_exp_for_glob(BoardType),
    ct:pal("# SwP_Selected: ~p", [SwP_Selected]),
    %% %% when only globals is selected.
    SwP_Expected = [{hwSwCompatibility,[{index,"TESTHAL2"}]},
		    {swp_id,[{global,GlobalSwpId},
			     {hal,?HalTest2Dir}]},
		    {boardList,[{global,{"BASEBAND",GlobBB}},
				{hal,{"BASEBAND",GlobBB}},
				{global,{"OTHER","FAKE_OTHER"}}]}],

    %% SwP_Expected = case BoardType of
    %% 		       BoardType when BoardType == "dus3301";
    %% 				      BoardType == "dus5301";
    %% 				      BoardType == "dus5201" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL2"}]},
    %% 		       	    {swp_id,[{global,GlobalSwpId},
    %% 		       		     {hal,?HalTest2Dir}]},
    %% 		       	    {boardList,[{global,{"BASEBAND",GlobBB}},
    %% 					{hal,{"BASEBAND","BB5216"}},
    %% 					{global,{"OTHER","FAKE_OTHER"}}]}];
    %% 		       "tcu0401" ->
    %% 			   [{hwSwCompatibility,[{index,"TESTHAL2"}]},
    %% 			    {swp_id,[{global,GlobalSwpId},
    %% 				     {hal,?TcuHalTest2Dir}]},
    %% 			    {boardList,[{global,{"BASEBAND-T",GlobBB}},
    %% 					{hal,{"BASEBAND-T","T605"}}]}]
    %% 		   end,

    ct:pal("# SwP_Expected: ~p", [SwP_Expected]),
    Sorted_SwP_Selected= sort_lists(SwP_Selected),
    ct:log("# Sorted_SwP_Selected: ~p", [Sorted_SwP_Selected]),
    Sorted_SwP_Expected= sort_lists(SwP_Expected),
    ct:log("# Sorted_SwP_Expected: ~p", [Sorted_SwP_Expected]),
    true = Sorted_SwP_Selected == Sorted_SwP_Expected.


get_bb_type_exp_for_glob(BoardType) ->
    GlobBB = case BoardType of
		 "dus3201" ->
		     "BB5212";
		 "dus5201" ->
		     "BB521_S";
		 "dus5301" ->
		     "BB662_3";
		     "dus3301" ->
		     "BB662_3";
		 "tcu0401"->
		     "T605"
	       end,
    ct:pal("# GlobBB: ~p", [GlobBB]),
    GlobBB.

check_nr_exp_hal_and_glob(Config, NrOfExpHalCxp, NrOfExpGlobCxp) ->
    HwId = decupling_lib:get_hw(rpc_1),
    SelProducts = decupling_lib:get_selected_products(rpc_1, HwId),
    ct:pal("# SelProducts: ~p", [SelProducts]),
    HalList = get_hal_cxp_list(SelProducts),
    ct:pal("HalList: ~n~p", [HalList]),
    GlobList = get_glob_cxp_list(SelProducts),
    ct:pal("GlobList: ~n~p", [GlobList]),
    case NrOfExpHalCxp of
	2 -> 
	    check_exp_hal_is_correct_2(Config, HalList, NrOfExpHalCxp);
	_LessThan2 ->
	    check_exp_hal_is_correct(Config, HalList, NrOfExpHalCxp)
    end,
    ct:pal("NrOfExpGlobCxp : ~p", [NrOfExpGlobCxp]),
    ct:pal("Length GlobList : ~p", [length(GlobList)]),

    NrOfExpGlobCxp = length(GlobList),
    ok.

get_hal_cxp_list(SelProducts) ->
    List = [X || {{_,_,_},{hal,_}} = X <- SelProducts],    
    ct:pal("## ## Returned Hal List : ~p ", [List]),
    List. 
  
get_glob_cxp_list(SelProducts) ->
    List = [X || {{_,_,_},{global,_}} = X <- SelProducts],
    ct:pal("#### Returned Global List : ~p ", [List]),
    List.   

check_exp_hal_is_correct(Config, HalList, NrOfExpHal) ->
    BoardType = proplists:get_value(board_type, Config),
    ExpHalCxp = case BoardType of
    		    %% BoardType when BoardType == "tcu03";
		    BoardType = "tcu0401" ->
			%% Case CXP exist then return true and add to new list.
			Fun = fun({{X,_,_},_}) -> 
				      X == "KATLA" end,
			lists:filter(Fun, HalList);
    		    _Dus ->
			Fun = fun({{X,_,_},_}) -> 
				      X == "COBRA-A10" end, %%this is set in hal
			lists:filter(Fun, HalList)		
    		end,
    ct:pal("# ExpHalCxp: ~p",[ExpHalCxp]),
    NrOfExpHal = length(ExpHalCxp).

check_exp_hal_is_correct_2(Config, HalList, NrOfExpHal) ->
    BoardType = proplists:get_value(board_type, Config),
    ExpHalCxp = case BoardType of
		    BoardType when BoardType == "dus3201";
				   BoardType == "dus5201" ->
			Fun = fun({{X,_,_},_}) -> 
				      X == "COBRA-A10" orelse X == "COBRA" end,
			lists:filter(Fun, HalList);
    		    BoardType when BoardType == "dus5301";
				   BoardType == "dus3301"->
			Fun = fun({{X,_,_},_}) -> 
				      X == "MTIGER" orelse X == "TIGER" end,
			lists:filter(Fun, HalList)		
    		end,
    ct:pal("# ExpHalCxp: ~p",[ExpHalCxp]),
    NrOfExpHal = length(ExpHalCxp).

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
set_glob_index(Tmp_Dir, CxsXml_Name, Index) ->
    remove_index_from_glob(Tmp_Dir, CxsXml_Name),
    add_index_to_glob(Tmp_Dir, CxsXml_Name, Index),
    ok.

remove_index_from_glob(Tmp_Dir, CxsXml) ->
    ct:pal("# remove hwSwCompatibility from global"),
    %%% 1. remove hwSwCompatibilityIndex.
    CMD1 = "cd "++Tmp_Dir++
    	";sed -i '/hwSwCompatibility/d' "++CxsXml,
    %% send_os_cmd(CMD1),
    ct:log("# os cmd #: ~s", [CMD1]),
    Answ = os:cmd(CMD1),
    ct:log("# Answ : ~p", [Answ]),
    [] = os:cmd("cd "++Tmp_Dir++" ; grep hwSwCompatibility "++CxsXml).


add_index_to_glob(Tmp_Dir, CxsXml, Index) ->
    ct:pal("# add hwSwCompatibility index ~p , in global", [Index]),
    %%% 1. Add correct hwSwCompatibilityIndex.
    CMD1 = "cd "++Tmp_Dir++
	";"++"sed -i '/<\\/contentinfo>/a <hwSwCompatibility index=\""++
	Index++"\" \/>' "++CxsXml,  %% Note, need a dubble \\
    %% send_os_cmd(CMD1),
    ct:log("# os cmd #: ~s", [CMD1]),
    Answ = os:cmd(CMD1),
    ct:log("# Answ : ~p", [Answ]),
    Grep = os:cmd("cd "++Tmp_Dir++" ; grep hwSwCompatibility "++CxsXml),
    ct:pal("Grep : ~p", [Grep]),
    match = check_str_exist_in_list(Grep, "hwSwCompatibility").


add_dummy_cxp_in_contentinfo(Tmp_Dir, CxsXml) ->
    ct:pal("# add dummy cxp in contentino"),
    CMD1 = "cd "++Tmp_Dir++"/"++
    	";"++"sed -i '/<contentinfo>/a <product name=\"TEST12\" id=\"CXP121212_12\" version=\"R12D\" filename=\"TEST12_CXP121212.cxp\"\/>' "++CxsXml,
    %% send_os_cmd(CMD1),
    ct:log("# os cmd #: ~s", [CMD1]),
    Answ = os:cmd(CMD1),
    ct:log("# Answ : ~p", [Answ]),
    Grep = os:cmd("cd "++Tmp_Dir++" ; grep  TEST12 "++CxsXml),
    ct:pal("Grep : ~p", [Grep]),
    match = check_str_exist_in_list(Grep, "TEST12").


add_hwcat_other_with_radio_cxps(Tmp_Dir, CxsXml_Name) ->
    %%%%
    %% 1. Remove two last lines, </boardLists> and </configuration>.
    %%%%
    CMD1 = "cd "++Tmp_Dir++"/"++
    	";"++"sed -i '$d ' "++CxsXml_Name,
    send_os_cmd(CMD1), %% Remove last linte.
    send_os_cmd(CMD1), %% Remove last linte once more.
    ct:log(" # Ls1: ~p", [os:cmd("ls "++Tmp_Dir)]),
    timer:sleep(2000),

    %%%%
    %% 2. add RADIO to cxs xml.
    %%    Note! that EndFileDir and EndFile is also used from SUITE in /SWM.
    %%%%
    EndFileDir = "/proj/rcs-tmp/hw_sw_dec/upgrade/need_for_ug_xml/",
    EndFile = "radio_baseband",
    CMD2 = "cd "++Tmp_Dir++
    	";cat "++CxsXml_Name++" "++EndFileDir++EndFile++
	" > tmp; mv tmp "++CxsXml_Name,
    send_os_cmd(CMD2),
    ct:log(" # Ls2: ~p", [os:cmd("ls "++Tmp_Dir)]),
    timer:sleep(2000),
    %%%%
    %% 3.  cp two ru cxp to upgrade path.
    %%     Note! that ExtraRuCxps is also used from SUITE in /SWM.
    %%%%
    ExtraRuCxps = "/proj/rcs-tmp/hw_sw_dec/upgrade/extra_cxps/g2_*",
    CP = "cp "++ExtraRuCxps++" "++Tmp_Dir++"/",
    send_os_cmd(CP),
    ct:log(" # Ls afte cp ru cxps: ~p", [os:cmd("ls "++Tmp_Dir)]).


cp_zip_to_tmp_dir_and_unzip(TftpDir, Tmp_Dir) ->
    ct:pal("# cp_zip_to_tmp_dir_and_unzip #"),
    %%% cp .zip to tmp dir
    CpCmd = "cp "++TftpDir++"/RCS-*.zip "++Tmp_Dir, 
    send_os_cmd(CpCmd),
    Ls = send_os_cmd("ls "++Tmp_Dir),
    ct:pal("ls : ~p", [Ls]),
    match = check_str_exist_in_list(Ls, ".zip"),

    %%% Get RCS zip name
    RCS_Name = fix_str(send_os_cmd("cd "++Tmp_Dir++" ; ls RCS-*.zip"), "\n"),
    ct:pal("RCS_Name : ~p", [RCS_Name]),

    %%% unzip RCS-DUS2_CXS101549_5.zip
    UnzipCmd = "cd "++Tmp_Dir++" ; unzip "++RCS_Name,
    %% send_os_cmd(UnzipCmd),
    ct:log("# os cmd #: ~s", [UnzipCmd]),
    Answ = os:cmd(UnzipCmd),
    ct:log("# Answ : ~p", [Answ]),
    timer:sleep(2000),

    %%% Get cxs xml name
    CxsXml_Name = fix_str(send_os_cmd("cd "++Tmp_Dir++" ; ls cxs*.xml"), "\n"),
    ct:pal("CxsXml_Name : ~p", [CxsXml_Name]),
    {RCS_Name, CxsXml_Name}.


zip_and_mv_to_tftp_dir(TftpDir, Tmp_Dir, RCS_Name) ->
    ct:pal("# zip_and_mv_to_tftp_dir"),
    %%%  rm RCS-DUS2_CXS101549_5.zip
    %% send_os_cmd("cd "++Tmp_Dir++" ; rm "++RCS_Name),
    Cmd = "cd "++Tmp_Dir++" ; rm "++RCS_Name,
    ct:log("# os cmd #: ~s", [Cmd]),
    Answ1 = os:cmd(Cmd),
    ct:log("# Answ : ~p", [Answ1]),

    Ls2 = send_os_cmd("ls "++Tmp_Dir),
    ct:pal("ls2 : ~p", [Ls2]),
    nomatch = check_str_exist_in_list(Ls2, ".zip"),

    %%% zip mod_RCS-DUS2_CXS101549_5.zip *
    ZipCmd = "cd "++Tmp_Dir++" ; zip "++"mod_"++RCS_Name++" * ",
    %% send_os_cmd(ZipCmd),
    ct:log("# os cmd #: ~s", [ZipCmd]),
    Answ2 = os:cmd(ZipCmd),
    ct:log("# Answ : ~p", [Answ2]),
    timer:sleep(2000),

    Ls3 = send_os_cmd("ls "++Tmp_Dir),
    ct:pal("ls3 : ~p", [Ls3]),
    match = check_str_exist_in_list(Ls3, "mod_"),

    %%% mv mod_RCS-DUS2_CXS101549_5.zip /tftp dir
    MvCmd = "mv "++Tmp_Dir++"/mod_"++RCS_Name++" "++TftpDir++"/"++RCS_Name,
    %% send_os_cmd(MvCmd),
    ct:log("# os cmd #: ~s", [MvCmd]),
    Answ3 = os:cmd(MvCmd),
    ct:log("# Answ : ~p", [Answ3]),

    Ls4 = send_os_cmd("ls "++Tmp_Dir),
    ct:pal("ls4 : ~p", [Ls4]),
    nomatch = check_str_exist_in_list(Ls4, ".zip"),

    %%% cleanup tmp dir.
    ct:pal("# cleanup tmp dir : ~p", [Ls4]),
    send_os_cmd("cd "++Tmp_Dir++" ; rm -rf *.cxp"),
    send_os_cmd("cd "++Tmp_Dir++" ; rm -rf *.xml"),

    ok.
 

send_os_cmd(Cmd) ->
    ct:log("# os cmd #: ~s", [Cmd]),
    Answ = os:cmd(Cmd),
    ct:log("# Answ : ~p", [Answ]),
    Answ.
    
fix_str(Str, FixStr) ->
    string:tokens(Str, FixStr).

check_str_exist_in_list(List, ExpStr) ->
    case re:run(List, ExpStr) of
	{match,_} ->
	    match;
	nomatch -> 
	    nomatch
    end.


coli_reinstall(_Config) ->
    Console = console,
    rct_rpc:call(rpc_1, sysNetloader, coli_reinstall, [gurka], 5000),
    {ok, _} = ct_telnet:expect(Console, "Ericsson Version:", 
			       [{timeout,180000},no_prompt_check]),
    {ok, _} = ct_telnet:expect(Console, "du1 login", 
			       [{timeout,180000},no_prompt_check]),
    wait_for_appl_started(),
    timer:sleep(10000).


ai_install(Config) ->
    get_all(),
    Console = console,
    ct:pal("##### start board restore #####"),
    aic_httpc:board_restore(Config, Console),
    ct:pal("##### end board restore #####"),
    
    %%% START "Only for manual test"
    %% test_server:break("### To copy NL patch *beam files to tftp directory."),
    %% ct_telnet:send(Console, "/nl/otp/bin/to_erl"),
    %% ct_telnet:send(Console, "nl_lib:patch(nl_sw)."),
    %% ct_telnet:send(Console, "nl_lib:patch(nl_bootp)."),
    %% ct_telnet:send(Console, "nl_lib:patch(nl_req)."),
    %%% END "Only for manual test"
    
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

install_org_up_to_cleanup(Config) ->
    rctprep_zip_glob_to_be_used(Config),
    ai_install(Config).




%%%--------------------------------------------------------------------
%%% wait_for_appl_started
%%%--------------------------------------------------------------------
wait_for_appl_started() ->
    wait_for_appl_started(180000).

wait_for_appl_started(Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout.");

wait_for_appl_started(Timeout) ->
    case rct_rpc:call(rpc_1, appmServer, get_apps, [], 1000) of
	[] ->
	    timer:sleep(5000),
	    wait_for_appl_started(Timeout - 5000);
    	{badrpc, _} ->
	    timer:sleep(5000),
	    wait_for_appl_started(Timeout - 5000);
	Apps ->
	    ct:pal(" # Apps : ~p", [Apps])
    end.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_node_is_in_nl(_Config) ->
    ct_telnet:send(?Console, ""),
    case ct_telnet:expect(?Console, "networkloader]#", 
			  [{timeout,5000},no_prompt_check]) of
	{ok, _} ->
	    nl;
	_ -> %% prompt can be corrupted, try second time
	    timer:sleep(5000),
	    ct_telnet:send(?Console, ""),
	    case ct_telnet:expect(?Console, "networkloader]#", 
			  [{timeout,5000},no_prompt_check]) of
		{ok, _} ->
		    nl;
		_Other ->
		    no_nl
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% break_auboot
%% @end
%%--------------------------------------------------------------------
break_auboot(Console, Config) ->
    BoardType = proplists:get_value(board_type, Config),
    decupling_lib:break_auboot(Console, node, BoardType).

%%%--------------------------------------------------------------------
%%% wait_for_netconf_started
%%%--------------------------------------------------------------------
wait_for_netconf_started() ->
    ct:pal("### Check Netconf",[]),
    wait_for_netconf_started(180000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:pal("Netconf not started within max timeout."),
    netconf_not_up;

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    ct_netconfc:close_session(?NC_Session),
	    ok;
	_Res  ->
	    timer:sleep(10000),
	    wait_for_netconf_started(Timeout - 10000)
    end.

%%%--------------------------------------------------------------------
%%% get_all
%%%--------------------------------------------------------------------
get_all() ->
    rct_logging:get_all(log1).


sort_lists(SwP_List) ->
    %% sort list of lists.
    [{X, lists:sort(Y)} || {X, Y} <- lists:sort(SwP_List)].

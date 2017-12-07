%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	decupling_ug_glob_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R6A/R7A/1
%%%
%%% @doc == Test Suite for Upgrade Mechanism, create, prepare, verify, activate and confirm. To UP is same as installed except rev is stepped in cxs-up xml. ==
%%% <br/><br/>
%%% @end

-module(decupling_ug_glob_SUITE).
-vsn('/main/R5A/R6A/R7A/1').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R2A/2      2014-03-16 etxivri     Created
%%% R2A/3      2014-03-18 etxivri     Add new tc. remove use of ets.
%%% R2A/4      2014-03-18 etxivri     set_not_suported_board_in_gup
%%% R2A/4      2016-04-14 etxivri     update due to hwSwCompatibility.
%%% R2A/6      2016-04-18 etxivri     Update error check
%%% R2A/7      2016-04-25 etxivri     Update to set not supported hw in gup 
%%%                                   instead for change hw on node.
%%% R6A/2      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R6A/3      2016-09-18 etxivri     Make it more robust.
%%% R7A/1      2016-09-30 etxivri     Update for new and real gupmaker built UP
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
	 remove/1,
	 build_valid_up/1,
	 modify_cxs/1,

	 gup_maker/1,
	 remove_index_from_glob/1,
	 add_index_to_glob/1,
	 prepare_fail_no_hw_match/1,
	 prepare_fail_no_index/1,
	 set_not_existing_hw_id/1,
	 set_org_hw_id/1,
	 set_not_suported_board_in_gup/1,

	 check_selected_vs_installed_cxps/1,

	 disable_max_up_check/1,
	 enable_max_up_check/1
	]).

-define(NC_Session, nc1).
%% -define(CLI_Session, cli1).

-define(UgCxsXml, "cxs101549_*-up.xml").
%% -define(HwCompIndx, "16B").
-define(HwCompIndx, "1").

-define(EndFileDir, "/proj/rcs-tmp/hw_sw_dec/upgrade/need_for_ug_xml/").
-define(ExtraRuCxps, "/proj/rcs-tmp/hw_sw_dec/upgrade/extra_cxps/g2_*").

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
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {cth_conn_log,[]},
			 {rct_power,node},
			 {rct_upgrade,ug1},
			 {rct_coli, {coli, [manual_connect]}},
			 {rct_logging, 
			  {upgrade, 
			   [{erlang,{["ERROR REPORT","CRASH REPORT"],
				     [ 
				       "BoardType not supported by neither HalUP nor GlobalUP",
				       "{uc_error,\"BoardType not supported by neither HalUP nor GlobalUP\"}",
				       "Missing hwSwCompatibility Index in Global UP",
				       "{uc_error,\"Missing hwSwCompatibility Index in Global UP\"}"
				     ]}}]}},
			 {rct_netconf,nc1},
			 {rct_rs232,console} ]}]
    end.

%% @hidden
init_per_suite(Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session),

    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("# BoardType: ~p", [BoardType]),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    {OrgProdNr, OrgProdRev} = decupling_lib:get_hw(rpc_1),
    ct:pal("Add orginal HW data to ets table"),
    ct:pal("### OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("### OrgProdRev: ~p", [OrgProdRev]),
    
    [{me_id, MeId},
     {board_type, BoardType},
     {ug_path, UGPath},
     {org_prouct_number, OrgProdNr},
     {org_prod_rev, OrgProdRev} | Config].

%%%--------------------------------------------------------------------
disable_max_up_check(_Config) ->
    swm_test_lib:disable_max_up_check(coli).
%%%--------------------------------------------------------------------
enable_max_up_check(_Config) ->
    swm_test_lib:enable_max_up_check(coli).
%%%--------------------------------------------------------------------

%% @hidden
end_per_suite(Config) ->
    OrgProdNr = proplists:get_value(org_prouct_number, Config),
    OrgProdRev = proplists:get_value(org_prod_rev, Config),
    ct:pal("# # OrgProdNr: ~p", [OrgProdNr]),
    ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),
    decupling_lib:check_expected_hw_id(rpc_1, OrgProdNr, OrgProdRev),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(set_org_hw_id, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
	    set_org_hw_id(Config)
    end,
    ok;
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

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
   [{group_ug_ok, [sequence],[build_valid_up, 
			      gup_maker,
			      create, 
			      prepare, 
			      verify, 
			      activate, 
			      confirm,
			      check_selected_vs_installed_cxps
			     ]},
    {group_no_index, [],[			     
			 modify_cxs,
			 remove_index_from_glob,
			 create, 
			 prepare_fail_no_index,
			 remove,
			 add_index_to_glob,
			 check_selected_vs_installed_cxps	     
			]},
    {group_hw_not_match, [],[
			     %% modify_cxs,
			     %% set_not_existing_hw_id,
			     set_not_suported_board_in_gup,
			     create, 
			     prepare_fail_no_hw_match,
			     remove,
			     %% set_org_hw_id,
			     check_selected_vs_installed_cxps
			    ]}
   ].

build_valid_up(_Config) ->
    ct:log("# build valid to up. ~n"
	    "Create cxps that shall be used for UG."),
    swm_test_lib:build_valid_ug_packakage(?NC_Session).


gup_maker(Config) ->
    ct:pal("gupmaker is not used due to now the up is build by gupmaker."),
    BoardType = proplists:get_value(board_type, Config),
    UGPath = proplists:get_value(ug_path, Config),

    UpType = 
    	case BoardType of
    	    BoardType when BoardType == "tcu03";
    			   BoardType == "tcu0401" ->
    		"BASEBAND-T";
    	    _Other ->
    		%% Case dus -> BASEBAND, cp two ru cxp to upgrade path.
    		CP = "cp "++?ExtraRuCxps++" "++UGPath++"/",
    		os:cmd(CP),
    		ct:log(" # Ls afte cp ru cxps: ~p", [os:cmd("ls "++UGPath)]),
    		"BASEBAND"
    	end,
    ct:pal("# uptype: ~p", [UpType]),

    CxsXml = ?UgCxsXml,
    %% HW_compat = " $RCS_TOP/HWC/HWC_CXA114005 ",
    %% BBhw_base = " $RCS_TOP/HWC/HWC_CNX9013421/HWC_CAX1034055/xml/BBhw ",
    %% GupMakerCmd = "gupmaker.escript -debug -metafile "++
    %% 	UGPath++"/"++CxsXml++
    %% 	%% " -out /home/etxivri/Kista/rcs_ithub/hw_sw_decupling/gupmaker/out/tcu "++
    %% 	" -out "++UGPath++
    %% 	" -index "++?HwCompIndx++" -uptype "++UpType++HW_compat++BBhw_base,
    %% ct:pal("GupMakerCmd : ~n~p", [GupMakerCmd]),

    %% OsCmd = os:cmd(GupMakerCmd),
    %% ct:log("os cmd : ~n~p", [string:tokens(OsCmd, "\n \",")]),
    %% timer:sleep(2000),

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
    CMD11 = "cd "++UGPath++"/"++
    	";"++"sed -i '$d ' "++CxsXml,
    os:cmd(CMD11), %% Remove last linte.
    os:cmd(CMD11), %% Remove last linte once more.
    ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000),

    %%%%
    %% 2. add RADIO.
    %%%%
    %% EndFileDir = "/home/etxivri/tmp/decupling/tmp_mod_up/",
    EndFile = "radio_baseband",
    CMD2 = "cd "++UGPath++
    	";cat "++CxsXml++" "++?EndFileDir++EndFile++" > tmp; mv tmp "++CxsXml,
    os:cmd(CMD2),
    ct:log(" # Ls2: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec remove_index_from_glob(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
remove_index_from_glob(Config) ->
    UGPath = proplists:get_value(ug_path, Config),
    %%%%
    %% 1. remove hwSwCompatibility.
    %%%%
    CMD1 = "cd "++UGPath++
    	";sed -i '/hwSwCompatibility/d' "++?UgCxsXml,
    os:cmd(CMD1),
    ct:log("CMD1:~n~p~n",[CMD1]),
    Cat = os:cmd("cat "++UGPath++"/"++?UgCxsXml),
    ct:log(" # cat #: ~p", [string:tokens(Cat, "\n\" </>")]),
    timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec add_index_to_glob(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
add_index_to_glob(Config) ->
    UGPath = proplists:get_value(ug_path, Config),
    %%%%
    %% 1. Add correct hwSwCompatibility Index.
    %%%%
    CMD1 = "cd "++UGPath++
    	";"++"sed -i '/<\\/contentinfo>/a <hwSwCompatibility index=\""++
	?HwCompIndx++"\" \/>' "++?UgCxsXml,  %% Note, need a dubble \\
    os:cmd(CMD1),
    Cat = os:cmd("cat "++UGPath++"/"++?UgCxsXml),
    ct:log(" # cat #: ~p", [string:tokens(Cat, "\n\" </>")]),
    timer:sleep(2000),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec set_not_suported_board_in_gup(Config) -> ok
%% @end
%%--------------------------------------------------------------------
set_not_suported_board_in_gup(Config) ->
    OrgProdNr = proplists:get_value(org_prouct_number, Config),
    UGPath = proplists:get_value(ug_path, Config),

    %% OrgProdRev = proplists:get_value(org_prod_rev, Config),
    ct:pal("## OrgProdNr: ~p", [OrgProdNr]),
    %% ct:pal("# # OrgProdRev: ~p", [OrgProdRev]),
    [OrgKdu , OrgInd] = string:tokens(OrgProdNr, "/"),
    ct:log("## OrgKdu: ~p, OrgInd: ~p", [OrgKdu, OrgInd]),

    NotExistingProdNr = OrgKdu++"/99",
    ct:pal("## Update gup: ProdNrs ~p, to ~p", [OrgProdNr, NotExistingProdNr]),

    CMD1 = "cd "++UGPath++
    	";"++"sed -i 's/"++OrgKdu++"\\/"++OrgInd++"/"++OrgKdu++"\\/99/g' " ++ 
	?UgCxsXml,
    os:cmd(CMD1),
    ct:log("CMD1:~n~p~n",[CMD1]),
    
    ct:log(" # Ls1: ~p", [os:cmd("ls "++UGPath)]),
    timer:sleep(2000),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec set_not_existing_hw_id(Config) -> ok
%% @end
%%--------------------------------------------------------------------
set_not_existing_hw_id(Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),

    break_auboot(console, Config),
    %% ProductName = "Not existing HW",
    ProductNumber = "KDU999999/99",
    ProductRevision = "R99A",

    decupling_lib:change_hw_id(console, 
			       %% ProductName, 
			       ProductNumber, 
			       ProductRevision),
    decupling_lib:start_node_from_uboot(console),
    decupling_lib:check_expected_hw_id(rpc_1, ProductNumber, ProductRevision),
    wait_for_netconf_started().

%%--------------------------------------------------------------------
%% @doc
%% @spec set_org_hw_id(Config) -> ok
%% @end
%%--------------------------------------------------------------------
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
			       %% ProductName, 
			       OrgProdNr, 
			       OrgProdRev),
    decupling_lib:start_node_from_uboot(console),
    decupling_lib:check_expected_hw_id(rpc_1, OrgProdNr, OrgProdRev),
    wait_for_netconf_started().

%%--------------------------------------------------------------------
%% @doc
%% break_auboot
%% @end
%%--------------------------------------------------------------------
break_auboot(Console, Config) ->
    BoardType = proplists:get_value(board_type, Config),
    decupling_lib:break_auboot(Console, node, BoardType).


%%--------------------------------------------------------------------
%% @doc
%% @spec check_selected_vs_installed_cxps(Config) -> ok
%% @end
%%--------------------------------------------------------------------
check_selected_vs_installed_cxps(_Config) ->
    decupling_lib:check_selected_vs_installed_cxps(rpc_1, ?NC_Session).


%%--------------------------------------------------------------------
%% @doc
%% @spec modify_cxs(Config) -> ok
%% @end
%%--------------------------------------------------------------------
modify_cxs(Config) ->
    MeId = proplists:get_value(me_id, Config),
    step_rev_in_cxs(MeId).


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
    %% %%%%
    %% %% Create UG
    %% %%%%
    %% %% {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    %% %% ct:pal("UGPath:~n~p~n",[UGPath]),
    %% UGPath = proplists:get_value(ug_path, Config),
    %% [{host, SftpHost},{username, Username},{password, Password}] = 
    %% 	ct:get_config(sftp_server),
    %% swm_test_lib:ug_create_match_result(?NC_Session,
    %% 					"SUCCESS",
    %% 					SftpHost,
    %% 					Username,
    %% 					Password,
    %% 					UGPath,
    %% 					MeId),
    %% ok.

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


prepare_fail_no_hw_match(Config) ->
    MeId = proplists:get_value(me_id, Config),
    To_Label = get_latest_up(),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
					prepare, 
					To_Label, 
					MeId),
    CauseStr = "BoardType not supported by neither HalUP nor GlobalUP",
    wait_for_prepare_fail_2(MeId, CauseStr).
    

prepare_fail_no_index(_Config) ->
    timer:sleep(30000), %% To make it more robust.
    MeId = swm_test_lib:get_me_id(?NC_Session),
    To_Label = get_latest_up(),

    ok = swm_test_lib:up_action_generic(?NC_Session, 
					prepare, 
					To_Label, 
					MeId),
    CauseStr = "Missing hwSwCompatibility Index in Global UP",
    swm_test_lib:check_nc_session(?NC_Session),
    wait_for_prepare_fail(MeId, To_Label, CauseStr, 120000).



wait_for_prepare_fail(_MeId, _To_Label, _CauseStr, Timeout) when Timeout < 0->
    ct:fail("## Not received Expected state within max timeout.");

wait_for_prepare_fail(MeId, To_Label, CauseStr, Timeout) ->
    ReportProgress = swm_test_lib:
    	get_up_report_progress(?NC_Session, MeId, To_Label),
    case lists:keyfind(actionName, 1, ReportProgress) of
    	{actionName,[],["prepare"]} ->
    	    case lists:keyfind(result, 1, ReportProgress) of
    		{result,[],["FAILURE"]} ->
		    case lists:keyfind(resultInfo, 1, ReportProgress) of
			{resultInfo,[],[CauseStr]} ->
			    ct_netconfc:close_session(?NC_Session),
			    ok;
			_A ->
			    ct:log(" # Sleep 5 sec, result is not expected: ~p",
				   [_A]),
			    timer:sleep(5000),
			    wait_for_prepare_fail(MeId, To_Label, CauseStr, 
						  Timeout-5000)
		    end;
		_B ->
		    ct:log(" ## Sleep 5 sec, result is not expected: ~p",
			   [_B]),
		    timer:sleep(5000),
		    wait_for_prepare_fail(MeId, To_Label, CauseStr,Timeout-5000)
	    end;
       	Result ->
	    ct:log("Sleep 5 sec, result is not expected: ~p",[Result]),
	    timer:sleep(5000),
	    wait_for_prepare_fail(MeId, To_Label, CauseStr, Timeout-5000)
    end.


wait_for_prepare_fail_2(MeId, CauseStr) ->
    ct:log("Wait for prepare fail due to ~n ~p", [CauseStr]),
    case swm_test_lib:
    	wait_for_expecting_state(?NC_Session, MeId, no_check, "FINISHED") of
	[{"FAILURE" = Result,
    	  "prepare" = ActionName, 
    	  ProgressInfo, 
    	 CauseStr =  ResultInfo, 
    	  "FINISHED" = State, _ProgReport}] ->
    	    ct:log("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n"
    		   "state:~p~n",[Result,
    				 ActionName, 
    				 ProgressInfo, 
    				 ResultInfo,
    				 State]),
    	    ok;
    	Result ->
    	    ct:pal("prepare: ~p",[Result]),
    	    ct:fail(Result)
    end.


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
%%% @doc Modify cxs.
%%% @end
%%%--------------------------------------------------------------------
step_rev_in_cxs(MeId) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 

    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    swm_test_lib:modify_cxs(SW_Vers, 1).

%%%--------------------------------------------------------------------
%%% wait_for_netconf_started
%%%--------------------------------------------------------------------
wait_for_netconf_started() ->
    ct:pal("### Check Netconf",[]),
    wait_for_netconf_started(120000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout.");

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
%%% @doc
%%% @end
%%%--------------------------------------------------------------------	
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

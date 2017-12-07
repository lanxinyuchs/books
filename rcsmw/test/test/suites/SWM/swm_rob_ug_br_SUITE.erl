%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_ug_br_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R9A/R10A/1
%%% 
%%% @doc == Test Suite for rob swm tests. Note shall run on LRAT.==
%%% <br/>
%%% @end

-module(swm_rob_ug_br_SUITE).
-vsn('/main/R3A/R4A/R5A/R6A/R9A/R10A/1').
-author('etxivri').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R3A/1      2015-06-08 etxivri     Created
%%% R3A/2      2015-06-08 etxivri     Removed hardcoded path to UPs.
%%% R3A/2      2015-06-09 etxivri     Removed a break
%%% R3A/5      2015-06-10 etxivri     Add upgrade restore test.
%%% R3A/5      2015-06-10 etxivri     Update test sequence in ug_restore tests.
%%%                                   Also make it more robust.
%%% R4A/1      2015-07-16 etxivri     Update to use correct to label.
%%% R4A/2      2015-09-02 etxivri     Update to be more robust.
%%% R4A/3      2015-10-05 etxivri     Add large configuration test.
%%% R4A/4      2015-11-03 etxivri     Update to use correct to label.
%%% R4A/4      2015-12-18 etxivri     Update due to changed in rcstprep.sh.
%%% R5A/2      2016-01-27 etxivri     Update to use correct to label.
%%% R5A/3      2016-01-27 etxivri     Update to use correct to label.
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R6A/2      2016-09-12 etxivri     bug fix.     
%%% R9A/1      2016-09-12 etxivri     Add more printouts.
%%% R9A/2      2017-03-20 etxivri     Update install_latest_stable to use oam accesspoint if needed.
%%% R9A/3      2017-03-24 etxivri     Update to use correct toup.
%%% R10A/1     2017-05-23 etxivri     Update for R10
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
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
-export([create_backup/1,
	 delete_backup/1,
	 restore_backup/1,
	 create_backup_2/1,
	 restore_backup_2/1,
	 install_latest_stable/1,
	 perform_upgrade_to_latest/1,
	 pre_conf/1,
	 upgrade/1,
	 upgrade_prep_to_up/1,
	 ug_act_commit/1,
	 ug_create/1,
	 ug_create_prepare_verify/1,
	 install_base_for_mass_conf_bu/1,
	 import_mass_conf_bu/1,
	 restore_mass_conf_bu/1
	]).

-define(NC_sess, nc1).
-define(Data_Dir, rct_cc_git_path:find("RCT_TOP", ["test/suites/SWM/swm_backup_SUITE_data/"])).
-define(BU_Name_1, "bu_test_1").
-define(BU_Name_2, "bu_test_2").
-define(EXP_IMP_DIR, "/proj/rcs-tmp/test_swm_brm/").
-define(SuiteName, "swm_br_1_SUITE").

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).

%% Used when test large configuration
-define(BaseCxp, "/proj/rcs-tmp/ivri_mass_conf_bu/R5LM/CXP9024418_2-R5LM.zip").
-define(LargeConfBu, "/proj/rcs-tmp/ivri_mass_conf_bu/R5LM/_um2_large_on_R5LM_1_RadioNode_20151002T112509+0000.zip").
-define(Large_Conf_BU_Name, "um2_large_on_R5LM").
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     restore_backup,
     restore_backup_2
    ].

groups() ->
    [{install, [sequence],[install_latest_stable
			  ]},
     {prepare_for_test, [sequence],[%%pre_conf, %%is done by other SUITE in spec
				    create_backup,
				    perform_upgrade_to_latest,
				    create_backup_2
				   ]},
     {fwd_restore_1, [sequence],[restore_backup,
				 restore_backup_2
				 ]},
     {rob_forward_restore_1, [sequence],[install_latest_stable,
					 pre_conf,
					 create_backup,
					 perform_upgrade_to_latest,
					 create_backup_2,
					 restore_backup,
					 restore_backup_2
					]},
     {prep_ug_restore_test, [sequence],[%%pre_conf, %%is done by other SUITE in spec,
					upgrade_prep_to_up,
					ug_create_prepare_verify,
					create_backup
			     ]},
     {ug_restore_test, [],[ug_act_commit,
			   restore_backup
			  ]}
     %% {mass_conf_ug_restore, [],[%install_base_for_mass_conf_bu,
     %% 				%% import_mass_conf_bu,
     %% 				%% restore_mass_conf_bu
     %% 				%% perform_upgrade_to_latest
     %% 				% restore_mass_conf_bu
     %% 			       ]}
    ].

%%% ===========================================================================
%%  forward restore scenario 1.
%% 1.	Install
%% 2.	Create backup A.
%% 3.	Upgrade till B
%% 4.	Create Backup B
%% 5.	Restore till backup A
%% 6.	Restore till backup B
%% Test Finished
%%% ===========================================================================

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    
    case aic_httpc:check_if_vc_board() of
	"yes" -> [{timetrap, {hours, 72}},
		  {ct_hooks, [{rct_htmllink,[]},
			      {rct_cli, {cli1,
					 [{user, "SysAdminTest"}, 
					  {password, "SysAdminTest"},
					  manual_connect]}},
			      {rct_upgrade,ug1},
			      {rct_netconf, {nc1, man_auth}},
			      {rct_rs232,console},
			      {cth_conn_log,[]}
			     ]}];
	_ ->
	    [{timetrap, {hours, 72}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_rs232,console},
			 {rct_netconf,nc1},
			 {cth_conn_log,[]},
			 {rct_core,[]},
			 {rct_logging, 
			  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					  ["wait_for_classes, operation ABORTED"
					   " because of unknown classes"]}}]}},
			 {rct_cli, {cli1, [manual_connect]}},
			 {rct_upgrade,ug1}
			]}]
    end.

%% @hidden
init_per_suite(Config) ->
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    ct:pal("NodeNmae : ~p",[NodeName]),
    A = os:cmd("mkdir "++?EXP_IMP_DIR++NodeName),
    ct:pal("A : ~p",[A]),
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/*.zip"), 
    ct:log("B : ~p",[B]),

    swm_test_lib:check_nc_session(?NC_sess),
    timer:sleep(10000),
    VcBoard = aic_httpc:check_if_vc_board(),
    MeId = case VcBoard of
    	       "yes" -> 
    		   get_me_id(?NC_sess);
    	       _->
		   swm_test_lib:get_me_id(?NC_sess)
    	   end,

    Get = {'ManagedElement', 
    	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
    	   [{managedElementId, [], [MeId]},
    	    {'SystemFunctions',
    	     [{systemFunctionsId,[],["1"]},
    	      {'SwM',
    	       [],
    	       [{swMId,[],["1"]}]}]}]},
    
    {ok, R} = netconf(get, [nc1, Get]),
    ct:pal("Upgrade packages ~p", [R]),
    Name = ?SuiteName,
    [{nodeName, NodeName},
     {backupName, Name},
     {meId, MeId},
     {sftp_host, ?SftpHost},
     {sftp_user, ?SftpUser},
     {sftp_pass, ?SftpPassword},
     {sec_board,VcBoard}| Config].
    %% Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    timer:sleep(60000),
	    %% test_server:break("##TC failed, have a check"),
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
	    %% aic_httpc:export_ai_log(Config),
	    aic_httpc:export_esi(Config)
	    %% test_server:break("TC failed, have a check in esi!")
    end,
    ok.


get_me_id(Session) ->
    netconf_open(Session, []),
    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]}]}]} =
	ct_netconfc:get(Session,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],[]}]
			}),
    ct_netconfc:close_session(Session),
    ct:pal("MeId:~n~p~n",[MeId]),
    MeId.

netconf_open(Session, Param) ->
    case aic_httpc:check_if_vc_board()  of
	"yes" -> ct_netconfc:open(Session, 
				  [{user, "SysAdminTest"},
				   {password, "SysAdminTest"}|Param]);
	_ -> ct_netconfc:open(Session,Param)
    end.

netconf(F, A) ->
    swm_test_lib:check_nc_session(nc1),
    Res = apply(ct_netconfc, F, A),
    ok = ct_netconfc:close_session(nc1),
    Res.


%%% ===========================================================================
%%% @doc
%%% @spec pre_conf(Config) -> ok
%%% @end
%%% ===========================================================================
pre_conf(Config) ->
    ct:pal("Config:~n~p~n",[Config]),
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

    SecBoard = proplists:get_value(sec_board, Config),
    case SecBoard of
	"yes" ->
	    os:cmd("rcs_exec -m netconf -f /proj/rbs-g2-ci/GIT_repo/R1B1364/ci-support/netconf_xml/config_1_ru_1_sector/LRAT_basic_fru_netconf_create.xml -u SysAdminTest -p SysAdminTest " ++ NodeName);
	_Else ->
	    os:cmd("rcs_exec -m netconf -f /proj/rbs-g2-ci/GIT_repo/R1B1364/ci-support/netconf_xml/config_1_ru_1_sector/LRAT_basic_fru_netconf_create.xml " ++ NodeName)
    end,

    timer:sleep(10000),

    case SecBoard of
	"yes" ->
	    os:cmd("rcs_exec -m netconf -f /home/eransbn/cs/RAT/RBSNC_2_basic_fru_netconf_create_common.xml -u SysAdminTest -p SysAdminTest "++ NodeName);
	_Other ->
	    os:cmd("rcs_exec -m netconf -f /home/eransbn/cs/RAT/RBSNC_2_basic_fru_netconf_create_common.xml "++ NodeName)
    end,

    timer:sleep(20000),
    check(Config).

check(Config) ->
    MeId = proplists:get_value(meId, Config),
    ct:pal("CHECK FRU created is !"),
    Get = {'ManagedElement', 
    	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
    	   [{managedElementId, [], [MeId]},
    	    {'Equipment',
    	     [{equipmentId,[],["1"]},
    	      {'FieldReplaceableUnit',
    	       [],
    	       [{fieldReplaceableUnitId,[],["1"]}]}]}]},
    
    {ok, R} = netconf(get, [nc1, Get]),
    ct:log("FieldReplaceableUnit: ~p", [R]),
    ok. 

get_node_name() ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    Node = atom_to_list(NodeName),
    Node.

%%% ===========================================================================
%%% @doc
%%% Create backup <br/>
%%% Using netconf to trig a backup to be created. <br/>
%%% @spec create_backup(Config) -> ok
%%% @end
%%% ===========================================================================
create_backup(Config) ->  
    create_backup(Config, ?BU_Name_1).

create_backup_2(Config) ->
    create_backup(Config, ?BU_Name_2).

create_backup(Config, BU_Name) ->  
    MeId = proplists:get_value(meId, Config),

    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId),
    ct:pal("# A"),
    swm_br_lib:create_backup(?NC_sess, 
			     MeId, 
			     BU_Name),
    %% SecBoard = proplists:get_value(sec_board, Config),
    [{"SUCCESS", 
      _ActionName, 
      _ProgressInfo, 
      ResultInfo, 
      _State, 
      _ProgressReport}] = swm_br_lib:wait_for_expecting_state(?NC_sess, 
							      MeId, 
							      ActionId, 
							      "FINISHED"),
    ct:pal("# ResultInfo: ~p", [ResultInfo] ),

    ok.

%%% ===========================================================================
%%% @doc
%%% Restore backup <br/>
%%% Using netconf to trig a restore backup . <br/>
%%% @spec restore_backup(_Config) -> ok
%%% @end
%%% ===========================================================================
restore_backup(_Config) ->
    restore_backup(_Config, ?BU_Name_1).

restore_backup_2(_Config) ->
    %% timer:sleep(60000),
    restore_backup(_Config, ?BU_Name_2).
    %% timer:sleep(30000).

restore_backup(Config, BU_Name) ->
    timer:sleep(60000),
    ct:pal("Sleep 1 min before start restore", []),
    MeId = proplists:get_value(meId, Config),

    %%%%
    %% Get BuId that belongs to BU_name.
    %%%%
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),

    BuId = get_correct_buid(BU_Name, Backups), %% in a list.
    ct:pal("Selected backup ~p to restore.~n",[BuId]),

    wait_for_succes_restore(BuId, MeId),
    ct:pal("Sleep 1 min after successfull restore.", []),
    timer:sleep(60000).


wait_for_succes_restore(BuId, MeId) ->
    wait_for_succes_restore(BuId, MeId, 120000).
wait_for_succes_restore(BuId, _MeId, Timeout) when Timeout < 0 ->
    ct:pal("## Restore BuId: ~p will fail du to it is not success",[BuId]),
    ct:fail("restore not succes within max timeout");
wait_for_succes_restore(BuId, MeId, Timeout) ->
    %% ActionId = swm_br_lib:get_action_id(?NC_sess, MeId, {brmBackup, BuId}),
    
    swm_br_lib:restore_backup(?NC_sess, MeId, BuId),

    ct:pal("## start wait for node restart after restore is started."),
    ct_telnet:expect(console, "Ericsson",
    		     [{timeout, 300000}, no_prompt_check]),

    ct:pal("## start wait for expected state after restore."),
    case swm_br_lib:
    	wait_for_expecting_state(?NC_sess, 
    				 MeId, 
    				 %% ActionId, 
				 no_check,
    				 "FINISHED", 
    				 {brmBackup, BuId},
				 600000) of
    	[{"SUCCESS", 
    	  "RESTORE", 
    	  _ProgressInfo, 
    	  _ResultInfo, 
    	  _State, 
    	  ProgressReport}] ->
    	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
    	    ok;
    	ErrResult ->
    	    ct:pal("Failed restoreBackup: ~p~n",[ErrResult]),
	    ct:pal("sleep 30sec and try again"),
	    timer:sleep(30000),
	    wait_for_succes_restore(BuId, MeId, Timeout-30000)
    	    %% ct:fail(ErrResult)
    end.


%%% ===========================================================================
%%% @doc
%%% Delete backup <br/>
%%% Using netconf to trig a backup to be removed. <br/>
%%% @spec delete_backup(Config) -> ok
%%% @end
%%% ===========================================================================
delete_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    ActionId = no_check,
    swm_br_lib:delete_backup(?NC_sess, 
			     MeId, 
			     ?BU_Name_1),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  "DELETE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup deleted successfully"),
	    ok;
	Result ->
	    ct:pal("deleteBackup: ~p~n",[Result]),
	    ct:fail(Result)
    end.

%%% ===========================================================================
%%% @doc
%%% Perform a repare, verify, activate and commit. Not create.<br/>
%%% @spec ug_act_commit(Config) -> ok
%%% @end
%%% ===========================================================================
ug_act_commit(Config) ->
    timer:sleep(30000),
    ToLabel = get_latest_up(),

    perform_ug_action(activate, Config, ToLabel),
    perform_ug_action(confirm, Config, ToLabel),

    MeId = proplists:get_value(meId, Config),
    EndFromUP = swm_test_lib:get_sw_version(?NC_sess, MeId),
    timer:sleep(30000),
    ct:pal("EndFromUP: ~p",[EndFromUP]).

%%% ===========================================================================
%%% @doc
%%% @spec perform_upgrade_to_latest(Config) -> ok
%%% @end
%%% ===========================================================================
perform_upgrade_to_latest(Config) ->
    upgrade_prep_to_up(Config),
    upgrade(Config).
    
upgrade_prep_to_up(Config) ->
    A = os:cmd("ls ${WORKSPACE}"),
    ct:pal("A: ~p", [A]),

    Node = proplists:get_value(nodeName, Config),
    ToUp = ct:get_config({jenkins_config, container}),
    ct:log("ToUp from Jenkins config: ~p", [ToUp]),
    JockeModToUp = "HALDC-"++ToUp++".zip",
    ct:log("JockeModToUp: ~p", [JockeModToUp]),
    Prep_Cmd = "upgradeprep.sh -stp "++Node++" ${WORKSPACE}/"++JockeModToUp,

    ct:pal("Prep_Cmd: ~p", [Prep_Cmd]),
    B = os:cmd(Prep_Cmd),
    ct:log("~s", [B]),
    timer:sleep(60000), % just in case!
    ok.


ug_create(Config) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    MeId = proplists:get_value(meId, Config),
    OldActionId = no_check,
    ok = swm_test_lib:up_create_generic(?NC_sess, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
	wait_for_swm_progress_result(?NC_sess, MeId, OldActionId) of
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
	    ResultInfo = dummy,
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end,

    ct:pal("ResultInfo: ~p", [ResultInfo]),
    ToLabel = lists:last(string:tokens(ResultInfo,"=")),
    ToLabel.

ug_create_prepare_verify(Config) ->
    ToLabel = ug_create(Config),
    perform_ug_action(prepare, Config, ToLabel),
    perform_ug_action(verify, Config, ToLabel).

upgrade(Config) ->
    %%%%
    %% Create UG
    %%%%
    ToLabel = ug_create(Config),

    %% %%%%
    %% %% Get Latest UP
    %% %%%%
    %% Label = get_latest_up(),
    Label = ToLabel,

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    perform_ug_action(prepare, Config, Label),

    %%%%
    %% Verifies an upgrade package.
    %%%%
    perform_ug_action(verify, Config, Label),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    perform_ug_action(activate, Config, Label),

    %%%%
    %% Confirm
    %%%%
    perform_ug_action(confirm, Config, Label),

    MeId = proplists:get_value(meId, Config),
    EndFromUP = swm_test_lib:get_sw_version(?NC_sess, MeId),
    ct:pal("EndFromUP: ~p",[EndFromUP]),

    ok.


%%% ===========================================================================
%%% @doc
%%% @spec install_latest_stable(Config) -> ok
%%% @end
%%% ===========================================================================
install_latest_stable(Config) ->
    %% %% Note! LRAT shall be used.
    A = os:cmd("ls ${WORKSPACE}"),
    ct:pal("A: ~p", [A]),
    B = os:cmd("echo ${WORKSPACE}"),
    ct:pal("B: ~p", [B]),

    Node = proplists:get_value(nodeName, Config),
    LatestStableCXS = ct:get_config({jenkins_config, upgrade_from_container}),
    %% LatestStableCXS ="https://rbs-rde.rnd.ki.sw.ericsson.se/proj/rcs-tmp/pre_bdc_cxp/R9A/26044/HALDC-CXP9024418_6-R10A101",
    ct:log("LatestStableCXS: ~p", [LatestStableCXS]),
    OamType = case ct:get_config({jenkins_config, oamap_ipv4}) of
		  [] ->
		      "-oamap_ipv4";
		  _Other ->
		      case ct:get_config({jenkins_config, oamap_ipv6}) of
			  [] ->
			      "-oamap_ipv6";
			  _Undef -> %% No oamap is used
			      ""
		      end
	      end,
    ct:log("OamType: ~p", [OamType]),

    Prep_Cmd = "rcstprep.sh "++Node++" ${WORKSPACE}/"++LatestStableCXS++".zip "++OamType,
    %% Prep_Cmd = "rcstprep.sh "++Node++" "++LatestStableCXS++".zip "++OamType,
    ct:pal("Prep_Cmd: ~p", [Prep_Cmd]),
    os:cmd(Prep_Cmd),
    timer:sleep(60000), % just in case!
    C = os:cmd("ls /proj/rcs-tmp/tftpboot/"++Node),
    ct:log("C: ~p", [C]),
    install(Config).


install(Config) ->
    ct:pal("# Start board restore."),
    aic_httpc:board_restore(Config, console, ?NC_sess),
    ct:pal("# Done, board restore."),

    ct:pal("## Start Download."),
    aic_httpc:download_files(Config,console),
    ct:pal("## Done, download."),

    ct:pal("### Start Integrate."),
    aic_httpc:integrate(Config, console, ?NC_sess),
    ct:pal("### Done Integrate."),

    MeId = proplists:get_value(meId, Config),
    SwVersion = swm_test_lib:get_sw_version(?NC_sess, MeId),
    ct:pal("# SwVersion: ~p",[SwVersion]),

    SecBoard = proplists:get_value(sec_board, Config),
    case SecBoard of
	"yes" ->
    		   ok;
	_->
	    rct_rpc:call(rpc_1, swmLib, set_variable, 
			 [swm_test_housekeeping_delay, 1000], 
			 10000, print),
	    Q = rct_rpc:call(rpc_1, swmLib, get_variable, 
			     [swm_test_housekeeping_delay], 
			     10000, print),
	    ct:log("### swm_test_housekeeping_delay: ~p",[Q]),
	    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
	    net_kernel:disconnect(ErlNode)
    end,
    ok.

%%% ===========================================================================
%%% @doc
%%% @spec install_base_for_mass_conf_bu(Config) -> ok
%%% @end
%%% ===========================================================================
install_base_for_mass_conf_bu(Config) ->
    %% %% Note! LRAT shall be used.
    ct:pal("NodeBase = R5LM.", []),

    Node = proplists:get_value(nodeName, Config),
    ct:log("Base to be installed before load mass conf backup: ~p", 
	   [?BaseCxp]),
    %% Prep_Cmd = "rcstprep.sh "++Node++" "++?BaseCxp,
    Prep_Cmd = "$RDE_TOP/tools/rcstgt/bin/rcstprep.sh "++Node++" "++?BaseCxp 
	++ " -no_oamap",
    ct:pal("Prep_Cmd: ~p", [Prep_Cmd]),
    A = os:cmd(Prep_Cmd),
    ct:log("~s", [A]),
    timer:sleep(10000), % just in case!
    install(Config),
    ok.

%%% ===========================================================================
%%% @doc
%%% @spec import_mass_conf_bu(Config) -> ok
%%% @end
%%% ===========================================================================
import_mass_conf_bu(Config) ->
    import_backup(Config),
    ok.

%%% ===========================================================================
%%% @doc
%%% @spec restore_mass_conf_bu(Config) -> ok
%%% @end
%%% ===========================================================================
restore_mass_conf_bu(Config) ->
    restore_backup(Config, ?Large_Conf_BU_Name),
    ok.

%%% ===========================================================================
%%% @doc
%%% @spec import_backup(Config) -> ok
%%% @end
%%% ===========================================================================
import_backup(Config) ->
    ct:pal("Import Backup: ~p",[?LargeConfBu]),

    MeId = proplists:get_value(meId, Config),
    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId),
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    SftpUri = "sftp://"++User++"@"++Host++?LargeConfBu,
    Password = proplists:get_value(sftp_pass, Config),

    swm_br_lib:import_backup(?NC_sess, 
			     MeId, 
			     SftpUri,
			     Password),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  "IMPORT", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup imported successfully"),
	    ok;
	Result ->
	    ct:pal("importBackup: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ok.


%% modify_up_name(UP) ->
%%     ct:pal("UP: ~p", [UP]),
%%     [CXS, ProdAndRev] = string:tokens(UP,"/"),
%%     CXC_Name = CXS++"_"++ProdAndRev,
%%     ct:pal("CXC_Name: ~p", [CXC_Name]),
%%     CXC_Name.
    

%%% Internal
%%%--------------------------------------------------------------------
%%% Description: Get latest upgrade package.
%%%--------------------------------------------------------------------
%% get_latest_up() ->
%%     UPs = swm_test_lib:get_ups(?NC_sess),
%%     ct:pal("UPs:~n~p~n",[UPs]),
%%     ToLabel = swm_test_lib:get_highest_label(UPs),
%%     ct:pal("ToLabel:~n~p~n",[ToLabel]),
%%     ToLabel.

%% Use this until from and to labels use same /X in rev. dus pre dc.
get_latest_up() -> 
    AllUps = swm_test_lib:get_ups(?NC_sess),
    ct:log("AllUps:~n~p~n",[AllUps]),
    Flatten_AllUps = lists:flatten(AllUps),
    ToLabel = case re:run(Flatten_AllUps, "HALDC") of
		  {match, _} -> %% Full UP with latest RCS
		      [Label | _] = lists:reverse(AllUps),
		      %% [Label | _] = AllUps,
		      ct:pal("highest Label using list reverse:~n~p~n",[Label]),
		      Label;
		  _Other ->
		      Label = swm_test_lib:get_highest_label(AllUps),
		      ct:pal("highest Label:~n~p~n",[Label]),
		      Label
	      end,
    ToLabel.

%% %%%% This will not work after restore backup.
%% %% get_created_to_up_label(Session, MeId) ->
%% %%     ct_netconfc:open(Session, [{timeout, 30000}]),
%% %%     Res = swm_test_lib:get_report_progress(Session, MeId),
%% %%     ct_netconfc:close_session(Session, 30000),
    
%% %%     ct:log("### Res: ~p", [Res]),
%% %%     {reportProgress, _, Result} = Res,
%% %%     ct:log("### Result: ~p", [Result]),
%% %%     {resultInfo,[],[CreatedToLabel]} = lists:keyfind(resultInfo, 1, Result),
%% %%     ct:log("### CreatedToLabel: ~p", [CreatedToLabel]),
%% %%     ToLabel = lists:last(string:tokens(CreatedToLabel, "=")),
%% %%     ct:pal("### ToLabel: ~p", [ToLabel]),
%% %%     ToLabel.

%%%--------------------------------------------------------------------
%%% Description: Perform upgrade actions
%%%--------------------------------------------------------------------
perform_ug_action(Action, Config, Label) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = proplists:get_value(meId, Config),

    %% %%%%
    %% %% Get Latest UP
    %% %%%%
    %% Label = get_latest_up(),

    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_sess, 
    					"SUCCESS",
    					Label,
    					MeId, 
    					Action),
    ok.

%% %%%--------------------------------------------------------------------
%% %%% Description: Evaluate an os cmd and print both the command and result
%% %%%--------------------------------------------------------------------
%% cmd(Cmd) ->
%%     ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
%%     R.

%%%--------------------------------------------------------------------
%%% Description: Construct sftp
%%%--------------------------------------------------------------------
%% sftp_uri(Config) ->
%%     Host = proplists:get_value(sftp_host, Config),
%%     User = proplists:get_value(sftp_user, Config),
%%     %% Dir = proplists:get_value(sftp_root, Config),
%%     Dir = ?EXP_IMP_DIR,
%%     NodeName = proplists:get_value(nodeName, Config),
%%     "sftp://"++User++"@"++Host++Dir++NodeName.


get_correct_buid(BuName, AllBackUps) ->
    List = lists:dropwhile(fun({_BrmBackup, _, Data}) ->
				   case lists:keysearch(backupName, 1, Data) of
				       {value,{backupName,[],[BuName]}} ->
				   	   false;
				       _ ->
				   	   true
				   end
			   end, AllBackUps),
    ct:pal("BuIdList: ~p", [List]),
    [WantedList | _] = List,
    {_BrmBackup, _, WantedData} = WantedList,
    {value,{brmBackupId,[],[BuId]}} = 
	lists:keysearch(brmBackupId, 1, WantedData),
    [BuId].

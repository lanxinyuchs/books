%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_failsafe_ug_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R5A/R6A/R7A/R8A/R11A/1
%%% 
%%% @doc == Activate failsafe. perform complete UG. Restore. Remove.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_failsafe_ug_SUITE).
-vsn('/main/R2A/R3A/R5A/R6A/R7A/R8A/R11A/1').

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
%%% R2A/6      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/4      2014-06-04 etxivri     Update due to answer in TR HS63631.
%%% R2A/5      2014-06-04 etxivri     Remove of modify cxs.
%%% R2A/7      2014-07-09 etxivri     Update off ERROR log filter.
%%% R2A/8      2014-08-11 etxivri     Update off ERROR log filter.
%%% R2A/9      2014-08-11 etxivri     Correct a ct:pal.
%%% R2A/10     2014-08-15 etxivri     Update due to new behaviour
%%% R3A/1      2014-10-17 etxivri     Update due to new behaviour on backups.
%%% R3A/3      2014-10-22 etxivri     Update when get next buId.
%%% R3A/4      2014-10-22 etxivri     Minor updates.
%%% R3A/5      2014-10-23 etxivri     Minor updates.
%%% R3A/5      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/6      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R5A/1	   2016-02-23 ekurnik	  Added new upgrade with failsafe use cases                              
%%% R5A/2      2016-05-11 etxivri     Add sleep before check failsafe bu is 
%%%                                   removed, to make TC mor robust.
%%% R6A/1      2016-05-17 etxivri     Update due to changed behaviour regarding
%%%                                   handling of BuIds
%%% R7A/1      2016-11-15 etxivri     A try to make it mor robust.
%%% R8A/1      2016-11-30 etxivri     Update to handle new behaviour that 
%%%                                   restored backups will not be removed.
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
-export([
	 %% failsafe_act_restore/1,
	 failsafe_ug_restore/1,
	 build_to_up/1,
	 %% get_prog_report/1,
	 clear_site_config_complete_bu/1,
	 failsafe_ug_activate_failed_restore/1,
	 failsafe_ug_activate_failed_no_restore/1,
	 failsafe_ug_cancel_restore/1,
	 failsafe_ug_cancel_no_restore/1
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
		 {cth_conn_log, []},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{[
					   "ERROR REPORT",
					   "CRASH REPORT"
					  ],
					  [
					   %% "Activate can not be executed when failsafe is active"
					  ]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    swm_test_lib:set_housekeeping_delay_variable(rpc_1),
    build_to_up(Config),
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
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
		
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n"
    	    	   "Clean up! TBD!", [Reason])
	%%swm_test_lib:check_nc_session(?NC_Session),
	%%[Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    %%ct:pal("Label:~n~p~n",[Label]),
    %%swm_test_lib:remove_upgrade_package(?NC_Session, Label)
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [clear_site_config_complete_bu,
     failsafe_ug_restore,
     failsafe_ug_activate_failed_restore,
     failsafe_ug_activate_failed_no_restore,
     failsafe_ug_cancel_restore,
     failsafe_ug_cancel_no_restore].

groups() ->
    [{group_1,[],[build_to_up
		 ]},
     {group_2,[],[build_to_up
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

clear_site_config_complete_bu(_C)-> 
    swm_br_lib:clear_scc_bu_cli(cli1).

%%%--------------------------------------------------------------------
%%% @doc 
%%% This test upgrade in comination with failsafe is active and deactive.
%%% - Activate failsafe
%%% - create, prepare, verify
%%% - activate shall fail due to failsafe is active.
%%% - Deactivate failsafe
%%% - activate again, this time activate shall be SUCCESS
%%% - confirm
%%% - Restore to previous UP.
%%% - Remove UP.
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
failsafe_ug_restore(_Config) ->
    ct:pal("TC: failsafe_ug_restore",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %% %%%%
    %% %% Modify version in cxs to UP.
    %% %%%%
    %% modify_cxs(MeId),

    %%%%
    %% Activate failsafe backup
    %%%%
    swm_br_lib:activate_failsafe_bu(?NC_Session, MeId),

    %%%%
    %% create
    %%%%
    create(MeId),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% prepare, verify
    %%%%
    prepare(MeId, Label),
    verify(MeId, Label),
 
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%
    %% Activate
    %%%%
    %%%%
    %% verify that activate was intercepted.
    %%%%
    ct:pal("Wait for activate fail due to failsafe is activated.", []),
    ExpErrInfoStr = "Activate can not be executed when failsafe is active",
    Res = swm_test_lib:up_action_generic_no_check(?NC_Session, 
						  activate, 
						  Label, 
						  MeId),

    swm_test_lib:check_exp_reply_err_from_nc_action(Res, 
						    ExpErrInfoStr),

    %%%%
    %% deactivate 'failsafe'
    %%%%
    ct:pal("deactivate 'failsafe'.", []),
    swm_br_lib:deactivate_failsafe_bu(?NC_Session, MeId),

    %%%%
    %% activate again
    %%%%
    ct:pal("Activate again. Wait for SUCCESS.", []),
    activate(MeId, Label),

    timer:sleep(30000),
    BuId_UG = get_next_bu_id(MeId),
    ct:pal("# BuId that shall be rollback bu: ~p ", [BuId_UG]),

    %%%%
    %% confirm
    %%%%
    confirm(MeId, Label),

    timer:sleep(30000),
    Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:pal("# All Backups after restore.: ~p ", [Backups]),

    %%%%
    %% Restore bu that has been created during upgrade.
    %%%%
    restore(MeId, [BuId_UG]),

    %% test_server:break("Check B !!!!"),

    %%%%
    %% Remove Last label
    %%%%
    remove(MeId, Label),

    ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% This tests upgrade in combination with failsafe
%%% - Activate failsafe
%%% - create, prepare, verify
%%% - activate shall fail due to failsafe is active.
%%% - wait for timeout to restore to failsafe BU
%%% - remove UP
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
failsafe_ug_activate_failed_restore(Config) ->
	ct:pal("TC: failsafe_ug_activate_failed_restore",[]),

    SpecificFunRestore = 
		fun(MeId, FailsafeBu) ->
			failsafeRestore(MeId, FailsafeBu)
		end,
			
	ok = failsafe_ug_activate_failed_generic([{specificFun, SpecificFunRestore} | Config]).

%%%--------------------------------------------------------------------
%%% @doc 
%%% This tests upgrade in combination with failsafe
%%% - Activate failsafe
%%% - create, prepare, verify
%%% - activate shall fail due to failsafe is active.
%%% - deactivate failsafe BU
%%% - remove UP
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
failsafe_ug_activate_failed_no_restore(Config) ->
	ct:pal("TC: failsafe_ug_activate_failed_no_restore",[]),
	
	SpecificFunNoRestore = 
		fun(MeId, FailsafeBu) ->									  
			noFailsafeRestore(MeId, FailsafeBu)
		end,

	ok = failsafe_ug_activate_failed_generic([{specificFun, SpecificFunNoRestore} | Config]).

%%%--------------------------------------------------------------------
%%% @doc 
%%% Generic part of UP activation fail due to failsafe active
%%% - Activate failsafe
%%% - create, prepare, verify
%%% - activate shall fail due to failsafe is active.
%%% - specific functionallity (restore or no restore)
%%% - remove UP
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
failsafe_ug_activate_failed_generic([{specificFun, SpecificFun} | _Config]) ->
	
	%%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Activate failsafe backup
    %%%%
    swm_br_lib:activate_failsafe_bu(?NC_Session, MeId),
	
    AllBUs = swm_br_lib:get_all_backups(?NC_Session, MeId),
    %% [FailsafeBu | _] = lists:reverse(swm_br_lib:get_all_backups(?NC_Session, MeId)),
    ct:log("##### AllBUs from generic: ~p ",[AllBUs]),

    [FailsafeBu | _] = lists:reverse(AllBUs),

    ct:log("##### FailsafeBu from generic: ~p ",[FailsafeBu]),

    %%%%
    %% create
    %%%%
    create(MeId),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

    %%%%
    %% prepare, verify
    %%%%
    prepare(MeId, Label),
    verify(MeId, Label),
 
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%
    %% Activate
    %%%%
    %%%%
    %% verify that activate was intercepted.
    %%%%
    ct:pal("Wait for activate fail due to failsafe is activated.", []),
    ExpErrInfoStr = "Activate can not be executed when failsafe is active",
    Res = swm_test_lib:up_action_generic_no_check(?NC_Session, 
						  activate, 
						  Label, 
						  MeId),

    swm_test_lib:check_exp_reply_err_from_nc_action(Res, 
						    ExpErrInfoStr),
	
	SpecificFun(MeId, FailsafeBu),
	
	%%%%
    %% Remove Last label
    %%%%
    remove(MeId, Label),

	ok.

%%%--------------------------------------------------------------------
%%% @doc 
%%% This tests upgrade in combination with failsafe
%%% - Activate failsafe
%%% - create, start prepare
%%% - cancel prepare
%%% - wait for timeout to restore to failsafe BU
%%% - remove UP
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
failsafe_ug_cancel_restore(Config) ->
	ct:pal("TC: failsafe_ug_cancel_restore",[]),

    SpecificFunRestore = 
		fun(MeId, FailsafeBu) ->									  
			failsafeRestore(MeId, FailsafeBu)
		end,
	
	ok = failsafe_ug_cancel_generic([{specificFun, SpecificFunRestore} | Config]).

%%%--------------------------------------------------------------------
%%% @doc 
%%% This tests upgrade in combination with failsafe
%%% - Activate failsafe
%%% - create, start prepare
%%% - cancel prepare
%%% - deactivate failsafe
%%% - remove UP
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
failsafe_ug_cancel_no_restore(Config) ->
	ct:pal("TC: failsafe_ug_cancel_no_restore",[]),

    SpecificFunNoRestore = 
		fun(MeId, FailsafeBu) ->									  
			noFailsafeRestore(MeId, FailsafeBu)
		end,
	
	ok = failsafe_ug_cancel_generic([{specificFun, SpecificFunNoRestore} | Config]).

%%%--------------------------------------------------------------------
%%% @doc 
%%% Generic part of UP prepare cancelled test
%%% - Activate failsafe
%%% - create, start prepare
%%% - cancel prepare
%%% - specific functionallity (restore or no restore)
%%% - remove UP
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
failsafe_ug_cancel_generic([{specificFun, SpecificFun} | _Config]) ->

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Activate failsafe backup
    %%%%
    swm_br_lib:activate_failsafe_bu(?NC_Session, MeId),
	
	[FailsafeBu | _] = lists:reverse(swm_br_lib:get_all_backups(?NC_Session, MeId)),

    %%%%
    %% create
    %%%%
    create(MeId),

    %%%%
    %% Get Latest UP
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    ct:pal("Label:~n~p~n",[Label]),

   %%%%
	%% Prepares an upgrade package, 
	%% which means downloading a complete UP
	%%%%
	%% No new action id when doing cancel.
	%% Therefore get action id is done here.
	OldActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
	ok = swm_test_lib:up_action_generic(?NC_Session, 
						prepare, 
						Label, 
						MeId),
	
	%% wait for prepare to start
	timer:sleep(20000),
	
	%%%%
	%% Cancel during prepare.
	%%%%
	ct:pal("### cancel",[]),
	ok = swm_test_lib:up_action_generic(?NC_Session, 
						cancel, 
						Label, 
						MeId),
	
	
	ct:pal("Wait for progress after cancel.", []),
	
	case swm_test_lib:
		wait_for_expecting_state(?NC_Session, MeId, OldActionId, "CANCELLED") of
	[{"FAILURE" = Result,
		  "prepare" = ActionName, 
		  ProgressInfo, 
		  ResultInfo, 
		  "CANCELLED" = State, _ProgReport}] ->
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
	end,
	
	SpecificFun(MeId, FailsafeBu),
	
	%%%%
    %% Remove Last label
    %%%%
    remove(MeId, Label),
	
	ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
create(MeId) ->
    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

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

    ok.

%%%--------------------------------------------------------------------
%%% @doc Prepare.
%%% @end
%%%--------------------------------------------------------------------
prepare(MeId, Label) ->
    ct:pal("Prepare.",[]),

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
%%% @doc Verify.
%%% @end
%%%--------------------------------------------------------------------
verify(MeId, Label) ->
    ct:pal("Verify.",[]),

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
%%% @doc Activate.
%%% @end
%%%--------------------------------------------------------------------
activate(MeId, Label) ->
    ct:pal("activate.",[]),

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
%%% @doc Confirm.
%%% @end
%%%--------------------------------------------------------------------
confirm(MeId, Label) ->
    ct:pal("Confirm.",[]),

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
%%% @doc Remove Latest UPs.
%%% @end
%%%--------------------------------------------------------------------
remove(MeId, Label) ->
    ct:pal("remove UG: ~p.",[Label]),

    OldActionId = no_check,
    swm_test_lib:remove_upgrade_package(?NC_Session, 
					Label),
    swm_test_lib:
	wait_for_progress_done(?NC_Session, 
			       "SUCCESS", 
			       "removeUpgradePackage",
			       MeId, 
			       OldActionId),    
    ok.

%%%--------------------------------------------------------------------
%%% @doc Get next bu id that will be used. str in a list
%%% @end
%%%--------------------------------------------------------------------
get_next_bu_id(MeId) ->
    Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:pal("# Backups: ~p ",[Backups]),

    case Backups of
	[] ->
	    HighestBuId = 0;
	_Elements ->
	    BuIdIntList = get_all_bu_id(Backups),
	    HighestBuId = lists:max(BuIdIntList)
	    %% HighestBuId = integer_to_list(HighestInt),
    end,
    ct:log("# HighestBuId: ~p", [HighestBuId]),
    NextBuId = HighestBuId,
    Next_BuId = integer_to_list(NextBuId),
    ct:log("# Next BuId: ~p", [Next_BuId]),
    Next_BuId.


get_all_bu_id(Backups) ->
    BuIdList = lists:map(fun(Element) ->
				 {'BrmBackup', _, BrmBackup} = Element,
				 {value, {brmBackupId, _, [Id]}} = 
				     lists:keysearch(brmBackupId, 1, BrmBackup),
				 Id
			 end, Backups),
    ct:pal("# BuIdList : ~p ", [BuIdList]),
    BuIdIntList = [list_to_integer(X) || X <- BuIdList],
    ct:pal("# BuId Integer List : ~p ", [BuIdIntList]),
    BuIdIntList.


				 
				 
%%%--------------------------------------------------------------------
%%% @doc Restore given BuId.
%%% @end
%%%--------------------------------------------------------------------
restore(MeId, BuId) ->
    ct:pal("restore BuId: ~p.",[BuId]),    

    ct:pal("Selected backup ~p to restore.~n",[BuId]),

    ActionId = swm_br_lib:get_action_id(?NC_Session, MeId, {brmBackup, BuId}),
    ct:pal("# BU ActionId: ~p", [ActionId]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    timer:sleep(30000),
    swm_br_lib:restore_backup(?NC_Session, MeId, BuId),
    %% timer:sleep(20000),
    case swm_br_lib:
	wait_for_expecting_state(?NC_Session, 
				 MeId, 
				 ActionId, 
				 "FINISHED", 
				 {brmBackup, BuId}) of
	[{"SUCCESS", 
	  "RESTORE", 
	  _ProgressInfo,
	  _ResultInfo, 
	  _State, 
	  ProgressReport}] ->
	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("restoreBackup: ~p~n",[ErrResult]),
    	    ct:fail(ErrResult)
    end,

    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),

    ok.   

%% Node restores due to failsafe timedout
failsafeRestore(MeId, FailsafeBu) ->
	{ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
	
	%%% Failsafe backup should restore in 60 seconds
	swm_br_lib:set_failsafe_attribute(?NC_Session, 
                                      MeId, 
                                      timeoutLength, 
                                      "60"),
	
    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, down),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),
    
    timer:sleep(60000),
    %% The failsafe backup should be removed on successfull restoration
    ct:log("## FailsafeBu: ~p", [FailsafeBu]),
    AllBUs = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:log("## AllBUs: ~p", [AllBUs]),
    %% false = lists:member(FailsafeBu, swm_br_lib:get_all_backups(?NC_Session, MeId)).
    %% false = lists:member(FailsafeBu, AllBUs).
    
    %% New check failsafe backup sitt exist and it is restored.
    %% This is a ne behaviour in 17B.
    {_,_,FailsafeBuList} = FailsafeBu,
    {brmBackupId,[],[BuId]} = lists:keyfind(brmBackupId, 1, FailsafeBuList),
    ct:pal("## Failsafe BuId: ~p", [BuId]),
    Report=
	swm_br_lib:get_progress_report(?NC_Session, MeId, {brmBackup, [BuId]}),
    ct:log("## Report: ~p", [Report]),
    {_,_,ReportList} = Report,
    ct:log("## ReportList: ~p", [ReportList]),
    {actionName,[],["RESTORE"]} = lists:keyfind(actionName, 1, ReportList),
    {result,[],["SUCCESS"]} = lists:keyfind(result, 1, ReportList),
    {state,[],["FINISHED"]} = lists:keyfind(state, 1, ReportList),

    ok.

%% Failsafe is deactivated
noFailsafeRestore(MeId, FailsafeBu) ->	
										
    %% deactivate failsafe backup
    swm_br_lib:deactivate_failsafe_bu(?NC_Session, MeId),
    timer:sleep(30000),

	%% The failsafe backup should be removed on deactivation
	false = lists:member(FailsafeBu, swm_br_lib:get_all_backups(?NC_Session, MeId)).


%% get_prog_report(_C) ->
%%     MeId="1",
%%     BuId = ["3"],
%%     Report=swm_br_lib:get_progress_report(?NC_Session, MeId, {brmBackup, BuId}),
%%     ct:pal("## Report: ~p", [Report]).

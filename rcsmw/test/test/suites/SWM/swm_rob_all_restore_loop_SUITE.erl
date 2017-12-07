%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_all_restore_loop_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R8A/1
%%% 
%%% @doc == Upgrade. After complete UG, restore system created BU, remove UP. Updates of cxs label in to up is handled in TC.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_all_restore_loop_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R7A/R8A/1').

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
%%% R2A/4      2014-05-07 etxivri     Remove up if tc fail.
%%% R2A/6      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/7      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R2A/8      2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R2A/9      2014-06-27 etxivri     Added coding: latin-1
%%% R2A/11     2014-08-29 etxivri     Update to use modify_cxs in swm_test_lib.
%%% R2A/12     2014-09-08 etxivri     Update to use get_highest_label
%%% R3A/1      2014-10-16 etxivri     Update due to new behaviour on backups.
%%% R3A/2      2014-10-16 etxivri     Make it more robust.
%%% R3A/3      2015-01-03 etxivri     Update to remove error in ct shell.
%%% R3A/4      2015-02-11 etxivri     Make it more robust.
%%% R3A/5      2015-02-19 etxivri     Make it more robust.
%%% R3A/6      2015-03-19 etxivri     Make it more robust.
%%% R3A/7      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/8      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-11-17 etxivri     Update to restore to a specific backup.
%%% R4A/2      2015-12-01 etxivri     Make it more robust.
%%% R5A/1      2016-01-13 etxivri     Make it more robust.
%%% R5A/2      2016-02-18 etxivri     Increased timeout when waiting for node
%%%                                   to restart 
%%% R5A/3      2016-05-12 etxivri     add sleep after create in ug_1
%%%                                   to make sure it exist in MO before trig
%%%                                   prepare.
%%% R7A/1      2016-11-11 etxivri     Update when get SITE_CONFIG_ bu id.
%%% R7A/2      2016-11-14 etxivri     Bugfix
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
-export([test_all/1,
	 ug_1/1,
	 build_to_up/1,
	 create/1, 
	 prepare/1, 
	 verify/1, 
	 activate/1, 
	 confirm/1,
	 remove/1,
	 restore/1,
	 create_backup/1,
	 delete_backup/1,
	 build_same_ug_package/1,
	 set_start_ug_rev_in_ug_top_xml/1
	 %% clear_system_bu/1
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
		 {rct_coli, {coli, [manual_connect, {connect_timeout, 60}]}},
                 {rct_logging, {upgrade, 
				[{erlang,{[
					   "ERROR REPORT",
					   "CRASH REPORT"
					  ],
					  []
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->
    swm_test_lib:set_housekeeping_delay_variable(rpc_1),
    DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df = string:tokens(DF, " \n"),
    ct:pal("### Df: ~p", [Df]),

    Config.

%% @hidden
end_per_suite(_Config) ->
    swm_test_lib:erase_housekeeping_delay_variable(rpc_1),
    DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df1 = string:tokens(DF1, " \n"),
    ct:pal("### Df: ~p", [Df1]),

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
    	    	   "Clean up! TBD!", [Reason]),
    	    %% Remove created upgrade package.
	    swm_test_lib:check_nc_session(?NC_Session),
	    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
	    UPs = swm_test_lib:get_ups(?NC_Session),
	    Label = swm_test_lib:get_lowest_label(UPs),
    	    ct:pal("Label:~n~p~n",[Label]),
    	    swm_test_lib:remove_upgrade_package(?NC_Session, Label)		
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
     %% %% test_all,
     %% %% test_all,
     %% %% test_all,
     %% %% test_all,
     %% %% test_all,
     %% test_all,
     test_all
    ].

groups() ->
    [{group_1,[],[%%build_to_up,
		  build_same_ug_package,
		  set_start_ug_rev_in_ug_top_xml,
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
     {group_2,[],[build_to_up,
		  create_backup,
		  ug_1,
		  ug_1,
		  restore,
		  delete_backup,
		  remove
		 ]},
     {short_group_1,[],[build_to_up,
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
%%% @doc 
%%% - After a complete UG, Restore the system created BU. 
%%% - Remove UP
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
test_all(_Config) ->
    ct:pal("UG.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Modify version in cxs to UP.
    %%%%
    modify_cxs(MeId),

    %%%%
    %% create, prepare, verify, activate and confirm.
    %%%%
    cre_prep_ver_act_com(MeId, _Config),

    %%%%
    %% Restore bu that has been created when activate upgrade.
    %%%%
    restore(_Config),

    timer:sleep(30000),

    %%%%
    %% Remove system created bu so it is possible to remove UP.
    %%%%
    swm_br_lib:clear_system_bu(coli),

    %%%%
    %% Remove Last label
    %%%%
    remove(_Config),

    ok.

%% clear_system_bu(_Config) ->
%%     %%%%
%%     %% Remove system created bu so it is possible to remove UP.
%%     %%%%
%%     ct:log("## cleanup system created BUs, coli swm/housekeep", []),
%%     ok = rct_coli:connect(coli),
%%     {ok,Answer} = rct_coli:send(coli,"/swm/housekeep"),
%%     ok = rct_coli:disconnect(coli),
%%     timer:sleep(1000),
%%     ct:log("## coli answer: ~p", [Answer]),
%%     timer:sleep(30000).

%%%--------------------------------------------------------------------
%%% @doc 
%%% - Perform a complete upgrade. 
%%% <br/><br/>
%%% @end
%%%--------------------------------------------------------------------
ug_1(_Config) ->
    ct:pal("UG.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Modify version in cxs to UP.
    %%%%
    modify_cxs(MeId),

    cre_prep_ver_act_com(MeId, _Config),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Modify cxs.
%%% @end
%%%--------------------------------------------------------------------
modify_cxs(MeId) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 

    SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    swm_test_lib:modify_cxs(SW_Vers, 1).




%%%--------------------------------------------------------------------
%%% @doc cre_prep_ver_act_com.
%%% @end
%%%--------------------------------------------------------------------
cre_prep_ver_act_com(MeId, _Config) ->
    %%%%
    %% Create UG
    %%%%
    create(_Config),
    %% test_server:break("A"),
    timer:sleep(30000),

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
    ct:pal("### Wait for node to restarts due to activate."),
    net_kernel:disconnect(ErlNode),
    {ok,_} = ct_telnet:expect(console, "Ericsson Version", 
    			      [{timeout,180000}, no_prompt_check]),
    case ct_telnet:expect(console, "login:", 
			  [{timeout,120000}, no_prompt_check]) of
	{ok,_} -> 
	    ok;
	_ -> %% if failed then try again.
	    ct_telnet:send(console, ""),
	    {ok,_} = ct_telnet:expect(console, "login:",
				      [{timeout,60000}, no_prompt_check]) 
    end,

    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:disconnect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),

    %%%%
    %% Wait for success.
    %%%%
    [{"SUCCESS" = _A2, 
     "activate" = _B2, 
      _C2, 
      _D2, _E2, ProgReport2}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId_2, 
					      dummy),
    ct:pal("ProgReport: ~p",[ProgReport2]),



    %%%%
    %% Confirm
    %%%%
    %% ug_action(MeId, Label, confirm, ErlNode),
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					confirm),


    ok.







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

    %% %%%%
    %% %% Create UG
    %% %%%%
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

    ok.

%%%--------------------------------------------------------------------
%%% @doc Prepare.
%%% @end
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
%%% @doc Verify.
%%% @end
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
%%% @doc Activate.
%%% @end
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
%%% @doc Confirm.
%%% @end
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
%%% @doc Remove Latest UPs.
%%% @end
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
    %% Remove UPs
    %%%%
    ct:pal("Remove all upgrade package, don't care about result.",[]),
    LabelList = swm_test_lib:get_ups(?NC_Session),
    lists:foreach(fun(RemLabel) ->
			  ct:pal("Remove upgrade package : ~p",[RemLabel]),

			  swm_test_lib:
			      remove_upgrade_package(?NC_Session, 
						     RemLabel),
			  swm_test_lib:
			      remove_up_check_allowed_result(?NC_Session,
    						MeId)
		  end, LabelList).

%%%--------------------------------------------------------------------
%%% @doc Restore first BU.
%%% @end
%%%--------------------------------------------------------------------
restore(_Config) ->
    ct:pal("restore.",[]),
    MeId = swm_test_lib:get_me_id(?NC_Session),
    

    Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:pal("Backups: ~p ",[Backups]),
    
    %% Check if system created bu exist after install,
    %% If it exist then it shall not be used due to a restore to this
    %% will result in that changed housekeeping time will have no effect.
    InstallBuName = "Auto integration backup - SITE_CONFIG_COMPLETE",
    %% NewBackups = case get_correct_buid(InstallBuName, Backups) of
    %% 		     [] -> %% SITE_CONFIG_COMPLETE bu does not exist.
    %% 			 Backups;
    %% 		     ["1"] -> %% SITE_CONFIG_COMPLETE bu exist, don not restore to this..
    %% 			 [_H | BuToRestore] = Backups,
    %% 			 BuToRestore
    %% 		 end,


    %% ct:pal("## Backups that shall be to restore to: ~p ",[NewBackups]),
    BuId = get_correct_buid(InstallBuName, Backups), %% in a list.

    %% BuId = 
    %% 	case NewBackups of
    %% 	    [] ->
    %% 		ct:fail("TC will fail due to, No backup to export");
    %% 	    [Element|_] ->
    %% 		{'BrmBackup', _, BrmBackup} = Element,
    %% 		{value, {brmBackupId, _, Id}} = 
    %% 		    lists:keysearch(brmBackupId, 1, BrmBackup),
    %% 		Id
    %% 	end,
    ct:pal("Selected backup ~p to restore.~n",[BuId]),

    ActionId = swm_br_lib:get_action_id(?NC_Session, MeId, {brmBackup, BuId}),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    swm_br_lib:restore_backup(?NC_Session, MeId, BuId),

    case swm_br_lib:
	wait_for_expecting_state(?NC_Session, 
				 MeId, 
				 ActionId, 
				 "RUNNING", 
				 {brmBackup, BuId}) of
	[{_Result, 
	  "RESTORE", 
	  %% "Action started" = _ProgressInfo, 
	  %% "Backup restore (data only)" = _ProgressInfo,
	  _ProgressInfo,
	  _ResultInfo, 
	  "RUNNING" = _State, 
	  ProgressReport}] ->
	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
	    ok;
	ErrResult ->
	    ct:pal("restoreBackup: ~p~n",[ErrResult]),
    	    ct:fail(ErrResult)
    end,

    %%%%
    %% check node restarts
    %%%%
    ct:pal("### Wait for node to restarts due to restore."),
    net_kernel:disconnect(ErlNode),
    
    ct_telnet:expect(console, "Ericsson Version", 
    			      [{timeout,120000}, no_prompt_check]),
    case ct_telnet:expect(console, "login:", 
			  [{timeout,120000}, no_prompt_check]) of
	{ok,_} -> 
	    ok;
	_ -> %% if failed then try again.
	    ct_telnet:send(console, ""),
	    {ok,_} = ct_telnet:expect(console, "login:",
				      [{timeout,60000}, no_prompt_check]) 
    end,
    
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:disconnect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),

    ok.   


get_correct_buid(BuName, AllBackupList) ->
    List = lists:dropwhile(fun({_BrmBackup, _, Data}) ->
				   case lists:keysearch(backupName, 1, Data) of
				       {value,{backupName,[],[BuName]}} ->
				   	   false;
				       _ ->
				   	   true
				   end
			   end, AllBackupList),
    ct:pal("BuList: ~p", [List]),
    BuIdL = case List of
	       [] ->
		   [];
	       _Other ->
		   [WantedList | _] = List,
		   {_BrmBackup, _, WantedData} = WantedList,
		   {value,{brmBackupId,[],[BuId]}} = 
		       lists:keysearch(brmBackupId, 1, WantedData),
		   [BuId]
	   end,
    BuIdL.

%%%--------------------------------------------------------------------
%%% @doc Create Backup with name "bu_1".
%%% @end
%%%--------------------------------------------------------------------
create_backup(_Config) ->  
    MeId = swm_test_lib:get_me_id(?NC_Session),
    ActionId = swm_br_lib:get_action_id(?NC_Session, MeId),
    BU_Name = "bu_1",
    swm_br_lib:create_backup(?NC_Session, 
			     MeId, 
			     BU_Name),

    case swm_br_lib:
	wait_for_expecting_state(?NC_Session, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  _ActionName, 
	  _ProgressInfo, 
	  ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup succesfully created."),
	    ct:pal("ResultInfo: ~p~n",[ResultInfo]),
	    BuId = lists:last(string:tokens(ResultInfo, "=")),
	    ct:pal("BuId: ~p~n",[BuId]);
	Result ->
	    ct:pal("createBackup: ~p~n",[Result]),
	    ct:fail(Result)
    end.

%%% ===========================================================================
%%% @doc Delete Backup with name "bu_1".
%%% @end
%%% @end
%%% ===========================================================================
delete_backup(_Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session),
    %% ActionId = swm_br_lib:get_action_id(?NC_sess, MeId),
    ActionId = no_check,
    BU_Name = "bu_1",
    ct:pal("Delete BU with name : ~p",[BU_Name]),
    swm_br_lib:delete_backup(?NC_Session, 
			     MeId, 
			     BU_Name),

    case swm_br_lib:
	wait_for_expecting_state(?NC_Session, MeId, ActionId, "FINISHED") of
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
    end,

    %% wait_for_backup_removed(MeId, 1), %% 1 BU system created at UG.

    ok.

%% %%% Internal
%% %%%--------------------------------------------------------------------
%% %%% 
%% %%%--------------------------------------------------------------------
%% wait_for_backup_removed(MeId, ExpNrOfBu) ->
%%     wait_for_backup_removed(MeId, ExpNrOfBu, 60000).
%% wait_for_backup_removed(_MeId, _ExpNrOfBu, Timeout) when Timeout < 0 ->
%%     ct:fail("Bacups not removed within expected time.");
%% wait_for_backup_removed(MeId, ExpNrOfBu, Timeout) ->
%%     Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
%%     case length(Backups) of
%% 	ExpNrOfBu ->
%% 	    ok;
%% 	_Res ->
%% 	    ct:pal("¤¤ Nr of Backups: ~p, Nr of exp Backups: ~p .",[_Res, 
%% 								    ExpNrOfBu]),
%% 	    timer:sleep(5000),
%% 	    wait_for_backup_removed(MeId, ExpNrOfBu, Timeout-5000)
%%     end.

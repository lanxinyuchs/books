%% coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_act_restore_loop_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/2
%%% 
%%% @doc == Upgrade. After activate is completed, restore system created BU, remove UP. Updates of cxs label in to up is handled in TC.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_act_restore_loop_SUITE).
-vsn('/main/R2A/R3A/2').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R2A/4      2014-07-08 etxivri     Removed a compile warning.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
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
	 build_to_up/1,
	 create/1, 
	 remove/1,
	 restore/1,
	 create_backup/1,
	 delete_backup/1]).

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

    DF = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    Df = string:tokens(DF, " \n"),
    ct:pal("### Df: ~p", [Df]),

    Config.

%% @hidden
end_per_suite(_Config) ->
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
    	    	   "Clean up! TBD!", [Reason])
    	    %% %% Remove created upgrade package.
	    %% swm_test_lib:check_nc_session(?NC_Session),
	    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    	    %% ct:pal("Label:~n~p~n",[Label]),
    	    %% swm_test_lib:remove_upgrade_package(?NC_Session, Label)
		
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [build_to_up, 
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
    [{group_1,[],[build_to_up,
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
    %% create, prepare, verify, activate.
    %%%%
    cre_prep_ver_act(MeId, _Config),

    %%%%
    %% Restore bu that has been created when activate upgrade.
    %%%%
    restore(_Config),

    %%%%
    %% Remove Last label
    %%%%
    remove(_Config),

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

    %% %% Construct a CXS label.
    [CXS, Index, ALabel] = string:tokens(SW_Vers, "/-"),
    ct:pal("CXS:~p, Index: ~p, ALabel: ~p ~n",[CXS, Index, ALabel]),

    CxsUp = "cxs101549_"++Index++"-up.xml",
    ct:pal("CxsUp: ~p ", [CxsUp]),

    %% %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    %% %% Increase label version with 1.
    {R,Nr}= lists:split(3,ALabel), %% Split R2A
    ToVer = R ++ integer_to_list(list_to_integer(Nr)+1),
    ct:pal("ToVer: ~p ", [ToVer]),

    CMD1 = "sed 's/"++ALabel++"/"++ToVer++"/' "++UGPath++"/"++
    	CxsUp++" > "++ UGPath++"/tmp_cxs101549-up.xml",
    ct:pal("CMD1:~n~p~n",[CMD1]),
    GG = os:cmd(CMD1),
    ct:pal("GG:~n~p~n",[GG]),

    CMD2 = "mv "++UGPath++"/tmp_cxs101549-up.xml "++UGPath++"/"++CxsUp,
    ct:pal("CMD2:~n~p~n",[CMD2]),
    os:cmd(CMD2).




%%%--------------------------------------------------------------------
%%% @doc cre_prep_ver_act_com.
%%% @end
%%%--------------------------------------------------------------------
cre_prep_ver_act(MeId, _Config) ->
    %% {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    %% ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%
    %% Create UG
    %%%%
    %% {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    %% ct:pal("UGPath:~n~p~n",[UGPath]),

    %% swm_test_lib:ug_create_match_result(?NC_Session, 
    %% 					"SUCCESS", 
    %% 					?SftpHost, 
    %% 					?SftpUser, 
    %% 					?SftpPassword, 
    %% 					UGPath, 
    %% 					MeId), 

    create(_Config),
    
    


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
    %% During activate the node will reboot.
    %%%%
    %% ug_action(MeId, Label, activate, ErlNode),
    swm_test_lib:ug_action_match_result(?NC_Session, 
					"SUCCESS",
					Label,
					MeId, 
					activate),

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
    %% swm_test_lib:ug_create_match_result(?NC_Session, 
    %% 					"SUCCESS", 
    %% 					?SftpHost, 
    %% 					?SftpUser, 
    %% 					?SftpPassword, 
    %% 					UGPath, 
    %% 					MeId), 


    OldActionId = no_check,
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
	wait_for_progress_result(?NC_Session, MeId, OldActionId) of
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
    


    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

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
    %% Get Latest UPs
    %%%%
    %% [Label | _] = 
    %% 	lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    [_FirstLabel | LatestUPs] = swm_test_lib:get_ups(?NC_Session),
    ct:pal("Label:~n~p~n",[LatestUPs]),

    lists:foreach(fun(Label)->
			  ct:pal("Remove upgrade package: : ~s",[Label]),
			  swm_test_lib:remove_upgrade_package(?NC_Session, 
							      "SUCCESS", 
							      MeId, 
							      Label)
		  end, LatestUPs),
    


    %% DF1 = rct_rpc:call(rpc_1, os, cmd, ["df"], 10000, noprint),
    %% Df1 = string:tokens(DF1, " \n"),
    %% ct:pal("### Df: ~p", [Df1]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Restore first BU.
%%% @end
%%%--------------------------------------------------------------------
restore(_Config) ->
    ct:pal("restore.",[]),
    MeId = swm_test_lib:get_me_id(?NC_Session),
    

    Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    ct:pal("Backups: ~p ",[Backups]),

    BuId = 
	case Backups of
	    [] ->
		ct:fail("TC will fail due to, No backup exist.");
	    [Element|_] ->
		{'BrmBackup', _, BrmBackup} = Element,
		{value, {brmBackupId, _, Id}} = 
		    lists:keysearch(brmBackupId, 1, BrmBackup),
		Id
	end,
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
	  "Action started" = _ProgressInfo, 
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
    swm_test_lib:wait_for_node_state(ErlNode, down),
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),
    wait_for_backup_removed(MeId, length(Backups)-1), %% 1 BU system created at Activate.

    %% case swm_br_lib:
    %% 	wait_for_expecting_state(?NC_Session, 
    %% 				 MeId, 
    %% 				 ActionId, 
    %% 				 "FINISHED", 
    %% 				 {brmBackup, BuId}) of
    %% 	[{"SUCCESS", 
    %% 	  "RESTORE", 
    %% 	  _ProgressInfo, 
    %% 	  _ResultInfo, 
    %% 	  _State, 
    %% 	  ProgressReport}] ->
    %% 	    ct:pal("restoreBackup: ~p~n",[ProgressReport]),
    %% 	    ok;
    %% 	ErrResult ->
    %% 	    ct:pal("restoreBackup: ~p~n",[ErrResult]),
    %% 	    ct:fail(ErrResult)
    %% end.

    ok.   




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
    end.

%%% Internal
%%%--------------------------------------------------------------------
%%% 
%%%--------------------------------------------------------------------
wait_for_backup_removed(MeId, ExpNrOfBu) ->
    wait_for_backup_removed(MeId, ExpNrOfBu, 60000).
wait_for_backup_removed(_MeId, _ExpNrOfBu, Timeout) when Timeout < 0 ->
    ct:fail("Bacups not removed within expected time.");
wait_for_backup_removed(MeId, ExpNrOfBu, Timeout) ->
    Backups = swm_br_lib:get_all_backups(?NC_Session, MeId),
    case length(Backups) of
	ExpNrOfBu ->
	    ok;
	_Res ->
	    ct:pal("¤¤ Nr of Backups: ~p, Nr of exp Backups: ~p .",[_Res, 
								    ExpNrOfBu]),
	    timer:sleep(5000),
	    wait_for_backup_removed(MeId, ExpNrOfBu, Timeout-5000)
    end.

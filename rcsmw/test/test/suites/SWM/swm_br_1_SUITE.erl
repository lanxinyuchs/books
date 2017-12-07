%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_br_1_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R10A/R11A/3
%%% 
%%% @doc == Test Suite for testing backup restore mechanism.==
%%% <br/><br/>
%%% @end

-module(swm_br_1_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R10A/R11A/3').
-author('etxivri').

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
%%% R1A/1      2014-03-12 etxivri     Created
%%% R1A/4      2014-03-17 etxivri     Update when use upgrade.
%%% R1A/5      2014-04-02 etxivri     Minor updates.
%%% R1A/7      2014-04-04 etxivri     Update to get correct BuId
%%% R1A/8      2014-04-07 etxivri     Changed some ct:pal to ct:log
%%% R1A/8      2014-05-07 etxivri     Update to handle specific backup.
%%% R1A/11     2014-05-08 etxivri     Update how to get specific backup.
%%%                                   cleanup in dir /proj/rcs-tmp/test_swm_brm/
%%%                                   when suite start.
%%% R1A/12     2014-05-23 etxivri     Changed commit to confirm.
%%% R1A/13     2014-06-23 etxivri     Update due to changes of reportProgress.
%%% R1A/14     2014-06-26 etxivri     created new tc max backups.
%%% R1A/15     2014-08-11 etxivri     Update cleanup in test dir before suite 
%%%                                   starts
%%% R1A/16     2014-08-22 etxivri     Update cleanup in init_per_suite
%%% R3A/1      2014-12-19 etxivri     Update to make it more robust.
%%% R3A/2      2015-01-29 etxivri     Update to remove error in ct-shell.
%%% R3A/3      2015-02-19 etxivri     Update to remove error printouts 
%%%                                   in ct shell.
%%% R3A/4      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/5      2015-05-28 etxkols     Changed rct_netconf hook format
%%% R3A/6      2015-06-01 etxivri     Add forward restore TCs.
%%% R3A/7      2015-06-03 etxivri     Bugfix in get latest stable lable.
%%% R4A/1      2015-09-18 etxivri     Update due to max nr o bu is now 20.
%%% R5A/1      2016-01-27 etxnnor     Added export_failure test case to verify unsuccessful bu export
%%% R5A/2      2013-03-18 etxivri     Add restore to rollback backup.
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R8A/1      2016-12-01 etxivri     Update due to new behaviour.
%%%                                   Restored backups can not be deleted.
%%% R10A/1     2016-06-27 etxivri     make it more robust after remove UPs.
%%% R11A/1     2017-09-14 etxivri     Update for git.
%%% R11A/2     2017-09-18 etxivri     More update for git.
%%% R11A/3     2017-09-22 etxivri     More update for git.
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
	 export_backup/1, 
	 export_failure/1,
	 delete_backup/1,
	 delete_backup_fail/1,
	 import_backup/1, 
	 restore_backup/1,
	 perform_upgrade/1,
	 check_restore_after_ug/1,
	 create_inst_before_create_bu/1,
	 check_inst_after_restore_bu/1,
	 create_del_max_nr_of_bu/1,
	 create_backup_2/1,
	 restore_backup_2/1,
	 restore_rollback_backup/1,
	 install_latest_stable/1,
	 ug_create_and_prepare/1,
	 perform_upgrade_to_latest/1,
	 clear_site_config_complete_bu/1,
	 remove_all_ups/1
	]).

-define(NC_sess, nc1).
%% -define(Data_Dir, "/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/test/suites/SWM/swm_backup_SUITE_data/").
-define(Data_Dir, "$RCT_TOP/test/suites/SWM/swm_backup_SUITE_data/").

-define(BU_Name_1, "bu_test_1").
-define(BU_Name_2, "bu_test_2").
-define(EXP_IMP_DIR, "/proj/rcs-tmp/test_swm_brm/").
-define(SuiteName, "swm_br_1_SUITE").

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(CLI_Session, cli1).
-define(NC_Session, nc1).

%% -define(MAX_NR_BU, 51).
-define(MAX_NR_BU, 20).
%% -define(MAX_NR_BU, 3).


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [create_backup,
     export_backup,
     export_failure,
     delete_backup,
     import_backup,
     restore_backup
    ].

groups() ->
    [{group_1,[],[create_inst_before_create_bu,
		  create_backup,
		  export_backup,
		  delete_backup,
		  import_backup,
		  restore_backup,
		  check_inst_after_restore_bu,
		  delete_backup
		 ]},
    {group_2,[],[create_inst_before_create_bu,
		 create_backup,
    		 perform_upgrade,
    		 restore_backup,
    		 check_restore_after_ug,
		 check_inst_after_restore_bu,
		 delete_backup
    		]},
    {group_3,[],[
		 %% clear_site_config_complete_bu,
		 create_inst_before_create_bu,
		 create_backup,
		 export_backup,
		 delete_backup,
		 remove_all_ups, %% Delete all ups that is possible to remove.
		 perform_upgrade,
		 import_backup,
		 restore_backup,
		 check_restore_after_ug,
		 check_inst_after_restore_bu,
		 delete_backup_fail
		]},
     {group_4,[],[create_del_max_nr_of_bu
		]},
     {forward_restore_1,[],[install_latest_stable,
			    create_backup,
			    perform_upgrade_to_latest,
			    create_backup_2,
			    restore_backup,
			    restore_backup_2
			    ]},
     {forward_restore_2,[],[create_backup,
			    export_backup,
			    install_latest_stable,
			    ug_create_and_prepare, %% needed otherwise import will fail.
			    import_backup,
			    restore_backup
			   ]}
    ].

%%% ===========================================================================
%%  forward restore scenario 1.
%% 1.	Install
%% 2.	Create backup A.
%% 3.	Upgrade till XX
%% 4.	Create Backup B
%% 5.	Restore till backup A
%% 6.	Restore till backup B
%% Test Finished
%%% ===========================================================================
%%% ===========================================================================
%%  forward restore scenario 2.
%%  Install B - gors av jenkins
%%  1. Create backup B
%%  2. Export backup B
%%  3. Install A - ladda pa senaste stabila
%%  4. Create and prepare B
%%  5. Import backup B
%%  6. Restore backup B
%%  Test Finished
%%% ===========================================================================

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {minutes, 60}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_rs232,console},
		 {rct_netconf,nc1},
                 {rct_coli, {coli, [manual_connect]}},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging, 
		  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
				  []}}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_upgrade,ug1}
		]}].

%% @hidden
init_per_suite(Config) ->
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    ct:pal("NodeNmae : ~p",[NodeName]),
    A = os:cmd("mkdir "++?EXP_IMP_DIR++NodeName),
    ct:pal("A : ~p",[A]),
    %% B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/*bu_test_1*.zip"),
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/*.zip"), %% could exist other files
    ct:log("B : ~p",[B]),
    %% os:cmd(["chmod a+rwx ", ?EXP_IMP_DIR++NodeName]),
    %% B =os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/RbsBackup_"++?BU_Name_1++"*"),
    %% ct:pal("B : ~p",[B]),
    MeId = swm_test_lib:get_me_id(?NC_sess),
    FromUP = swm_test_lib:get_sw_version(?NC_sess, MeId),
    ct:pal("#### FromUP: ~p",[FromUP]),
    Name = ?SuiteName,
    [{nodeName, NodeName},
     {backupName, Name},
     {meId, MeId},
     {fromUP, FromUP},
     {sftp_host, ?SftpHost},
     {sftp_user, ?SftpUser},
     {sftp_pass, ?SftpPassword} | Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% init_per_testcase(perform_upgrade, Config) ->
    %% FromUP = swm_test_lib:get_specific_reportprogress_info(?CLI_Session, 
    %% 							   "SwVersionMain"),
    
    %% NewConfig =  [{fromUP, FromUP} | Config],
    %% ct:log("Cpnfig: ~p ",[NewConfig]),
    %% NewConfig;
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
            %%%%
	    %% Cleanup exported backup dir.
            %%%%
	    [{_, Node_Name}] = ct:get_config(test_nodes),
	    NodeName =  atom_to_list(Node_Name),
	    ct:pal("NodeNmae : ~p",[NodeName]),
	    A = os:cmd("mkdir "++?EXP_IMP_DIR++NodeName),
	    ct:log("A : ~p",[A]),
	    os:cmd(["chmod a+rwx ", ?EXP_IMP_DIR++NodeName]),
	    %% B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/RbsBackup_"++?BU_Name_1++"*"),
	    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/RbsBackup_*"),
	    ct:log("B : ~p",[B]),
	    set_autodelete_to_enabled()
    end,
    ok.

clear_site_config_complete_bu(_C) ->
    swm_br_lib:clear_scc_bu_cli(coli).

remove_all_ups(_C) ->
    %% Don't care about the result.
    UPs = swm_test_lib:get_ups(?NC_Session),
    lists:foreach(fun(Label) ->
			  ct:pal("Label:~n~p~n",[Label]),
			  swm_test_lib:remove_upgrade_package(?NC_Session, 
							      Label)
		  end, UPs),
    timer:sleep(30000).

%%% ===========================================================================
%%% @doc
%%% Create delete max nr of backups. <br/>
%%% Using netconf to trig a backup to be created. <br/>
%%% @spec create_del_max_nr_of_bu(Config) -> ok
%%% @end
%%% ===========================================================================
create_del_max_nr_of_bu(Config) ->
    MeId = proplists:get_value(meId, Config),

    %% set autoDelete to DISABLED
    set_autodelete_to_disabled(),

    ExistingBu = swm_br_lib:get_all_backups(?NC_sess, MeId),
    %% ct:pal("ExistingBu: ~p.", [ExistingBu]),
    Nr_OfExistingBu = length(ExistingBu),
    ct:pal("Nr_OfExistinBu before create max BUs: ~p.", [Nr_OfExistingBu]),

    NrOfBUList = lists:seq(1, ?MAX_NR_BU),
    lists:foreach(fun(X) ->
    			  Nr = integer_to_list(X),
    			  ct:pal("create bu: ~p", [Nr]),
    			  swm_br_lib:create_backup(?NC_sess, 
    						   MeId, 
    						   "bu_nr_"++Nr)
    		 end, NrOfBUList),

    ct:pal("Simple check to see all BUs created.", []),
    ExpNrOfCreateBUs = ?MAX_NR_BU + Nr_OfExistingBu,
    ct:pal("ExpNrOfCreateBUs : ~p", [ExpNrOfCreateBUs]),
    wait_for_nr_of_exp_bu(ExpNrOfCreateBUs, MeId),
    swm_br_lib:get_all_backups(?NC_sess, MeId),


    Message ={'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {createBackup,[],
		      [{name, [], ["last_bu"]}]}
		    ]}]}]}]},

    %% Try to create one more bu. Shall not be ok to create
    {ok, _} = ct_netconfc:open(?NC_sess, []),
    {error,[{'error-type',[_],[_]},
	    {'error-tag',[_],["operation-failed"]},
	    {'error-severity',[_],["error"]},
	    {'error-message',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"},
			      {'xml:lang',"en"}],
	     ["Request could not be performed - resource not available, [Housekeeping required]"]}]}
	= ct_netconfc:action(?NC_sess, Message, 5000),
    ct_netconfc:close_session(?NC_sess),

    %% set back autoDelete to ENABLED
    set_autodelete_to_enabled(),

    %% Try to create one more bu. Shall be ok to create
    {ok, _} = ct_netconfc:open(?NC_sess, []),
    {ok, _} = ct_netconfc:action(?NC_sess, Message, 5000),
    ok = ct_netconfc:close_session(?NC_sess),

    %% first man created bu is removed due to last bu is 
    %% created and only 20 is created.
    NrOfDelBUList = lists:seq(2, ?MAX_NR_BU),
    lists:foreach(fun(Y) ->
    			  Nr = integer_to_list(Y),
    			  ct:pal("delete bu: ~p", [Nr]),
    			  swm_br_lib:delete_backup(?NC_sess, 
    						  MeId, 
    						  "bu_nr_"++Nr)
    		 end, NrOfDelBUList),

    %% Delete last createt backup.
    swm_br_lib:delete_backup(?NC_sess, 
			     MeId, 
			     "last_bu"),

    ct:pal("Simple check to see all BUs deleted.", []),
    wait_for_nr_of_exp_bu(Nr_OfExistingBu, MeId),

    ok.

set_autodelete_to_disabled() ->
    rct_cli:connect(?CLI_Session, print), 
    rct_cli:send(?CLI_Session, "ManagedElement=1,SystemFunctions=1,BrM=1,BrmBackupManager=1,BrmBackupHousekeeping=1", print),
    rct_cli:send(?CLI_Session, "show autoDelete", print),
    rct_cli:send(?CLI_Session, "configure", print),
    rct_cli:send(?CLI_Session, "autoDelete=DISABLED", print),
    rct_cli:send(?CLI_Session, "commit", print),
    rct_cli:send(?CLI_Session, "show autoDelete", print),
    rct_cli:send(?CLI_Session, "top", print),
    rct_cli:disconnect(?CLI_Session, print),
    timer:sleep(5000).
    
set_autodelete_to_enabled() ->
    rct_cli:connect(?CLI_Session, print), 
    rct_cli:send(?CLI_Session, "ManagedElement=1,SystemFunctions=1,BrM=1,BrmBackupManager=1,BrmBackupHousekeeping=1", print),
    rct_cli:send(?CLI_Session, "show autoDelete", print),
    rct_cli:send(?CLI_Session, "configure", print),
    rct_cli:send(?CLI_Session, "autoDelete=ENABLED", print),
    rct_cli:send(?CLI_Session, "commit", print),
    rct_cli:send(?CLI_Session, "show autoDelete", print),
    rct_cli:send(?CLI_Session, "top", print),
    rct_cli:disconnect(?CLI_Session, print),
    timer:sleep(5000).

%%% ===========================================================================
%%% @doc
%%% Create backup <br/>
%%% Using netconf to trig a backup to be created. <br/>
%%% @spec create_backup(Config) -> ok
%%% @end
%%% ===========================================================================
create_backup(Config) ->  
    create_backup(Config, ?BU_Name_1).

%% create_backup_1(Config) ->
%%     create_backup(Config, ?BU_Name_1).

create_backup_2(Config) ->
    create_backup(Config, ?BU_Name_2).

create_backup(Config, BU_Name) ->  
    MeId = proplists:get_value(meId, Config),
    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId),
    swm_br_lib:create_backup(?NC_sess, 
			     MeId, 
			     BU_Name),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, MeId, ActionId, "FINISHED") of
	[{"SUCCESS", 
	  _ActionName, 
	  _ProgressInfo, 
	  ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup succesfully created. Verifying metadata."),
	    %% DataDir = proplists:get_value(data_dir, Config),
	    DataDir = proplists:get_value(data_dir, [{data_dir, ?Data_Dir}]),
	    SchemaFile = filename:join(DataDir, "backupinfo.xsd"),
	    %% ResultInfo = swm_br_lib:get_result_info(ProgressReport),
	    ct:pal("ResultInfo: ~p~n",[ResultInfo]),
	    BuId = lists:last(string:tokens(ResultInfo, "=")),
	    ct:pal("Config: ~p~n",[Config]),
	    ct:pal("BuId: ~p~n",[BuId]),

	    BuPath = rct_rpc:call(rpc_1, swmLib, backup_dir, [BuId],10000),
	    BuFile = filename:join(BuPath, "backupinfo.xml"),
	    {ok, Bin} = rct_rpc:call(rpc_1, file, read_file, [BuFile],10000),
	    PrivDir = proplists:get_value(priv_dir, Config),
	    MetadataFile = filename:join(PrivDir, "backupinfo.xml"), 
	    file:write_file(MetadataFile, Bin),
	    Res = cmd(["xmllint --schema ", SchemaFile, " ", MetadataFile, " ; echo -n \"Res=$?\""]),
	    %% ct:pal("Res ~p~n",[Res]),

	    case lists:last(string:tokens(Res, "\n=")) of
		"0" ->
		    {ok, ResultInfo};
		Other ->
		    Msg = xmllint_status(Other),
		    ct:pal("xmllint exited with status ~s ~s~n",[Other, Msg]),
		    ct:fail({xmllint, Msg})
	    end;
	Result ->
	    ct:pal("createBackup: ~p~n",[Result]),
	    ct:fail(Result)
    end.

%%% ===========================================================================
%%% @doc
%%% Restore backup <br/>
%%% Using netconf to trig a restore backup . <br/>
%%% @spec restore_backup(_Config) -> ok
%%% @end
%%% ===========================================================================
restore_backup(_Config) ->
    restore_backup(_Config, ?BU_Name_1).

%% restore_backup_1(_Config) ->
%%     restore_backup(_Config, ?BU_Name_1).

restore_backup_2(_Config) ->
    restore_backup(_Config, ?BU_Name_2).

restore_rollback_backup(Config) ->
    %% restore_backup(Config, "Rollback_backup").
    MeId = proplists:get_value(meId, Config),
    BuId = get_rollback_bu_id(MeId),
    perform_restore_backup(MeId, BuId),
    timer:sleep(180000). %% To se if no error in erl log after restore.

restore_backup(Config, BU_Name) ->
    MeId = proplists:get_value(meId, Config),

    %%%%
    %% Get BuId that belongs to BU_name.
    %%%%
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),

    BuId = get_correct_buid(BU_Name, Backups), %% in a list.
    ct:pal("Selected backup ~p to restore.~n",[BuId]),
    perform_restore_backup(MeId, BuId).

perform_restore_backup(MeId, BuId) ->
    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId, {brmBackup, BuId}),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    swm_br_lib:restore_backup(?NC_sess, MeId, BuId),

    ct_telnet:expect(console, "Ericsson",
		     [{timeout,60000}, no_prompt_check]),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, 
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
    end.


%% restore_backup(Config, BU_Name) ->
%%     MeId = proplists:get_value(meId, Config),

%%     %%%%
%%     %% Get BuId that belongs to BU_name.
%%     %%%%
%%     Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),

%%     BuId = get_correct_buid(BU_Name, Backups), %% in a list.
%%     ct:pal("Selected backup ~p to restore.~n",[BuId]),

%%     ActionId = swm_br_lib:get_action_id(?NC_sess, MeId, {brmBackup, BuId}),
%%     {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
%%     net_kernel:disconnect(ErlNode),
%%     swm_br_lib:restore_backup(?NC_sess, MeId, BuId),

%%     ct_telnet:expect(console, "Ericsson",
%% 		     [{timeout,60000}, no_prompt_check]),

%%     case swm_br_lib:
%% 	wait_for_expecting_state(?NC_sess, 
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
%%     	    ct:fail(ErrResult)
%%     end.


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

%% New behaviour in 17B. Restored backups can not be deleted.
delete_backup_fail(Config) ->
    MeId = proplists:get_value(meId, Config),
    ActionId = no_check,
    swm_br_lib:delete_backup(?NC_sess, 
			     MeId, 
			     ?BU_Name_1),

    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, MeId, ActionId, "FINISHED") of
	[{"FAILURE", 
	  "DELETE", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup deleted failed and it is OK."),
	    ok;
	Result ->
	    ct:pal("deleteBackup: ~p~n",[Result]),
	    ct:fail(Result)
    end.


%%% ===========================================================================
%%% @doc
%%% Export backup <br/>
%%% Using netconf to trig a backup to be exported. <br/>
%%% @spec export_backup(Config) -> ok
%%% @end
%%% ===========================================================================
export_backup(Config) ->
    %%%%
    %% Cleanup backup dir.
    %%%%
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    os:cmd(["chmod a+rwx ", ?EXP_IMP_DIR++NodeName]),
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/RbsBackup_"++?BU_Name_1++"*"),
    os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/_" ++?BU_Name_1++"*"), 
    ct:log("B : ~p",[B]),

    [DiscBefore] = check_temp_disc_space_percentage(),

    %%
    MeId = proplists:get_value(meId, Config),
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),
    ct:pal("Backups: ~p ",[Backups]),

    BuId = get_correct_buid(?BU_Name_1, Backups), %% in a list.
    ct:pal("Selected backup ~p for export.~n",[BuId]),

    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId, {brmBackup, BuId}),

    Uri = sftp_uri(Config),
    ct:pal("Uri: ~p",[Uri]),
    Password = proplists:get_value(sftp_pass, Config),
    swm_br_lib:export_backup(?NC_sess, 
			     MeId, 
			     BuId,
			     Uri,
			     Password),

    NodeName = proplists:get_value(nodeName, Config),
    
    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, 
				 MeId, 
				 ActionId, 
				 "FINISHED", 
				 {brmBackup, BuId}) of
	[{"SUCCESS", 
	  "EXPORT", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup exported successfully"),
	    {ok, Files} = file:list_dir(?EXP_IMP_DIR++NodeName),
	    ct:pal("Dir = ~p ~nFiles = ~p~n",[?EXP_IMP_DIR++NodeName, Files]),
	    ok;
	ErrResult ->
	    ct:pal("exportBackup: ~p~n",[ErrResult]),
	    ct:pal("URI: ~p~n",[Uri]),
	    cmd(["ls -la ",?EXP_IMP_DIR++NodeName]),
	    ct:fail(ErrResult)
    end,

    [DiscAfter] = check_temp_disc_space_percentage(),
    %% ct:comment("Before: ~p~nAfter: ~p",[DiscBefore, DiscAfter]),
    list_to_integer(DiscAfter) =< (list_to_integer(DiscBefore)).

%%% ===========================================================================
%%% @doc
%%% Export backup failure with wrong password <br/>
%%% Using netconf to trig a backup to be exported. <br/>
%%% @spec export_failure(Config) -> ok
%%% @end
%%% ===========================================================================
export_failure(Config) ->
    %%%%
    %% Cleanup backup dir.
    %%%%
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    os:cmd(["chmod a+rwx ", ?EXP_IMP_DIR++NodeName]),
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/RbsBackup_"++?BU_Name_1++"*"),
    ct:log("B : ~p",[B]),

    [DiscBefore] = check_temp_disc_space_percentage(),

    %%
    MeId = proplists:get_value(meId, Config),
    Backups = swm_br_lib:get_all_backups(?NC_sess, MeId),
    ct:pal("Backups: ~p ",[Backups]),

    BuId = get_correct_buid(?BU_Name_1, Backups), %% in a list.
    ct:pal("Selected backup ~p for export.~n",[BuId]),

    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId, {brmBackup, BuId}),

    Uri = sftp_uri(Config),
    ct:pal("Uri: ~p",[Uri]),
    Password = "wrongPassword",
    swm_br_lib:export_backup(?NC_sess, 
			     MeId, 
			     BuId,
			     Uri,
			     Password),

    NodeName = proplists:get_value(nodeName, Config),
    
    case swm_br_lib:
	wait_for_expecting_state(?NC_sess, 
				 MeId, 
				 ActionId, 
				 "FINISHED", 
				 {brmBackup, BuId}) of
	[{"FAILURE", 
	  "EXPORT", 
	  _ProgressInfo, 
	  _ResultInfo, 
	  _State, 
	  _ProgressReport}] ->
	    ct:pal("Backup export failed, which is expected"),
	    ok;
	ErrResult ->
	    ct:pal("exportBackup: ~p~n",[ErrResult]),
            ct:pal("The backup file transfer succeeded although it shouldn't"),
	    ct:pal("URI: ~p~n",[Uri]),
	    cmd(["ls -la ",?EXP_IMP_DIR++NodeName]),
	    ct:fail(ErrResult)
    end,

    [DiscAfter] = check_temp_disc_space_percentage(),
    %% ct:comment("Before: ~p~nAfter: ~p",[DiscBefore, DiscAfter]),
    list_to_integer(DiscAfter) =< (list_to_integer(DiscBefore)).

%%% ===========================================================================
%%% @doc
%%% Import backup <br/>
%%% Using netconf to cause the system to import a backup. <br/>
%%% @spec import_backup(Config) -> ok
%%% @end
%%% ===========================================================================
import_backup(Config) ->
    NodeName = proplists:get_value(nodeName, Config),
    {ok, FileName} = file:list_dir(?EXP_IMP_DIR++NodeName),
    ct:pal("FileName: ~p",[FileName]),
    ct:pal("Dir = ~p ~nFiles = ~p~n",[?EXP_IMP_DIR++NodeName, FileName]),
    ImportName = ?EXP_IMP_DIR++NodeName++"/"++FileName,
    ct:pal("ImportName: ~p",[ImportName]),

    MeId = proplists:get_value(meId, Config),
    ActionId = swm_br_lib:get_action_id(?NC_sess, MeId),
    Uri = sftp_uri(Config)++"/"++FileName,
    Password = proplists:get_value(sftp_pass, Config),

    swm_br_lib:import_backup(?NC_sess, 
			     MeId, 
			     Uri,
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

    %%%%
    %% Cleanup backup dir.
    %%%%
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    os:cmd(["chmod a+rwx ", ?EXP_IMP_DIR++NodeName]),
    B = os:cmd("rm "++?EXP_IMP_DIR++NodeName++"/RbsBackup_"++?BU_Name_1++"*"),
    ct:log("B : ~p",[B]),

    ok.

%%% ===========================================================================
%%% @doc
%%% Perfor a complete upgrade. <br/>
%%% @spec perform_upgrade(Config) -> ok
%%% @end
%%% ===========================================================================
perform_upgrade(Config) ->
    %%%%
    %% Get managedElementId.
    %%%%
    MeId = proplists:get_value(meId, Config),

    %%%%
    %% Get FromUP.
    %%%%
    FromUP = swm_test_lib:get_sw_version(?NC_sess, MeId),
    ct:pal("FromUP: ~p",[FromUP]),

    %%%%
    %% Build a valit ti UP.
    %%%%
    ct:log("# init per suite build valid to up. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_sess),
    upgrade(Config),
    ok.

%%% ===========================================================================
%%% @doc
%%% @spec perform_upgrade_to_latest(Config) -> ok
%%% @end
%%% ===========================================================================
perform_upgrade_to_latest(Config) ->
    upgrade_prep_to_up(Config),
    upgrade(Config).

upgrade_prep_to_up(Config) ->
    FromUP = proplists:get_value(fromUP, Config), %% This will be new to UP. 
    Node = proplists:get_value(nodeName, Config),
    ToUp = case swm_test_lib:check_env() of
		git -> 
		    ct:pal("git env is used."),
		    CXS= ct:get_config({jenkins_config, cxp}),
		    %% CXS = "/proj/rcs/DCI/SBC/cache/20170901185144_3a9b6f1f6b98a91401ccbb013ecca6b534068269/RCS-BB_CXS2010013_2.cxs",
		    CXS;
		_Other ->
		    ct:pal("CC env is used."),
		   CXS = modify_up_name(FromUP),
		   CXS
	    end,
    ct:pal("To_Up: ~p", [ToUp]),
    Prep_Cmd = "upgradeprep.sh -stp "++Node++" "++ToUp,
    ct:pal("Prep_Cmd: ~p", [Prep_Cmd]),
    os:cmd(Prep_Cmd),
    timer:sleep(10000), % just in case!
    ok.

upgrade(Config) ->
    %%%%
    %% Create UG
    %%%%
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
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end,

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    perform_ug_action(prepare, Config),

    %%%%
    %% Verifies an upgrade package.
    %%%%
    perform_ug_action(verify, Config),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    perform_ug_action(activate, Config),

    %%%%
    %% Confirm
    %%%%
    perform_ug_action(confirm, Config),

    EndFromUP = swm_test_lib:get_sw_version(?NC_sess, MeId),
    ct:pal("EndFromUP: ~p",[EndFromUP]),

    ok.

%%% ===========================================================================
%%% @doc
%%% A simple check that SwVersionMain from init_per_suite is the same <br/>
%%% restore after a upgrade has been performed.
%%% @spec check_restore_after_ug(Config) -> ok
%%% @end
%%% ===========================================================================
check_restore_after_ug(Config) ->
    MeId = proplists:get_value(meId, Config),
    FromUP = proplists:get_value(fromUP, Config), %% From init per suite.

    NowUP = swm_test_lib:get_sw_version(?NC_sess, MeId),
    ct:pal("FromUP : ~p, NowUP : ~p",[FromUP, NowUP]),

    case FromUP of 
    	 NowUP ->
    	    ct:pal("SwVersionMain is same as it was from beginning.~n"
    		   "OK"),
    	    ok;
    	_Res ->
    	    ct:pal("NOK: ~p", [_Res]),
    	    ct:fail("SwVersionMain is not expected after restore, NOK")
    end,
    
    %%%%
    %% Remove created upgrade package.
    %%%%
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_sess)),
    ct:pal("Label:~n~p~n",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_sess, Label),

    %% Maybe add more checks!
    ok.

%%% ===========================================================================
%%% @doc
%%% create_inst_before_create_bu <br/>
%%% @spec create_inst_before_create_bu(_Config) -> ok
%%% @end
%%% ===========================================================================
create_inst_before_create_bu(_Config) ->
    %%%%
    %% Open Netconf sessions.
    %%%%
    {ok,_} = ct_netconfc:open(?NC_sess, []),

    %%%%
    %% Add instances and attr
    %%%%
    lists:foreach(fun(X) ->
			  InstName = "nc_"++integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(?NC_sess, 
							  InstName, 
							  "12345")
		  end, 
    		  lists:seq(1,10) ),
    ok = ct_netconfc:close_session(?NC_sess),

    %%%%
    %% Checkinstances and attr exist
    %%%%
    {ok,_} = ct_netconfc:open(?NC_sess, []),
    lists:foreach(fun(Y) ->
			  InstName1 = "nc_"++integer_to_list(Y),
			  ok = rct_nc_testclass1_lib:
			      nc_get_mo_instance_check_attr(?NC_sess,
							    InstName1,
							    "12345")
		  end, 
    		  lists:seq(1,10) ),
    ok = ct_netconfc:close_session(?NC_sess),
    ok.


%%% ===========================================================================
%%% @doc
%%% check_inst_after_restore_bu  <br/>
%%% @spec check_inst_after_restore_bu(_Config) -> ok
%%% @end
%%% ===========================================================================
check_inst_after_restore_bu(_Config) ->
    %%%%
    %% Checkinstances and attr exist
    %%%%
    {ok,_} = ct_netconfc:open(?NC_sess, []),
    lists:foreach(fun(Y) ->
			  InstName1 = "nc_"++integer_to_list(Y),
			  ok = rct_nc_testclass1_lib:
			      nc_get_mo_instance_check_attr(?NC_sess,
							    InstName1,
							    "12345")
		  end, 
    		  lists:seq(1,10) ),

    %%%%
    %% CleanUp Delete instances
    %%%%
    lists:foreach(fun(X) ->
			  InstName = "nc_"++integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(?NC_sess, 
						    InstName)
		  end, 
    		  lists:seq(1,10) ),
    ok = ct_netconfc:close_session(?NC_sess),

    ok.

%%% ===========================================================================
%%% @doc
%%% @spec install_latest_stable(Config) -> ok
%%% @end
%%% ===========================================================================
install_latest_stable(Config) ->
    %% LatestStableCXS = ct:get_config({jenkins_config, upgrade_from_cxp}),
    LatestStableCXS = case swm_test_lib:check_env() of
			  git -> 
			      ct:pal("# git env is used."),
			      CXS = ct:get_config({jenkins_config, from_cxp}),
			      CXS;
			  _Other -> 
			      ct:pal("# CC env is used."),
			      CXS = ct:get_config({jenkins_config, upgrade_from_cxp}),
			      %% CXS = "CXS101549_8-R11S15",
			      CXS
		      end,				  
    %% {upgrade_from_cxp ,"CXS101549_5-R3U06"},
    ct:log("LatestStableCXS: ~p", [LatestStableCXS]),
    %% %% LatestStableCXS = "CXS101549_5-R3U06",

    Node = proplists:get_value(nodeName, Config),

    Prep_Cmd = "rcstprep.sh "++Node++" "++LatestStableCXS,
    ct:pal("Prep_Cmd: ~p", [Prep_Cmd]),
    os:cmd(Prep_Cmd),
    timer:sleep(10000), % just in case!
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),

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

    ok.
    
modify_up_name(UP) ->
    ct:pal("UP: ~p", [UP]),
    [CXS, ProdAndRev] = string:tokens(UP,"/"),
    CXC_Name = CXS++"_"++ProdAndRev,
    ct:pal("CXC_Name: ~p", [CXC_Name]),
    CXC_Name.
    

ug_create_and_prepare(Config) ->
    upgrade_prep_to_up(Config),
    MeId = proplists:get_value(meId, Config),
    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    SftpHost = proplists:get_value(sftp_host, Config),
    SftpUser = proplists:get_value(sftp_user, Config),
    SftpPassword = proplists:get_value(sftp_pass, Config),
    
    OldActionId = no_check,
    ok = swm_test_lib:up_create_generic(?NC_sess, 
					SftpHost, 
					SftpUser, 
					SftpPassword, 
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
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end,

    %%%%
    %% Prepares an upgrade package, 
    %% which means downloading a complete UP
    %%%%
    perform_ug_action(prepare, Config),

    ok.


%%% Internal
%%%--------------------------------------------------------------------
%%% Description: Get latest upgrade package.
%%%--------------------------------------------------------------------
get_latest_up() ->
    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_sess)),
    ct:pal("Label:~n~p~n",[Label]),
    Label.

%%%--------------------------------------------------------------------
%%% Description: Perform upgrade actions
%%%--------------------------------------------------------------------
perform_ug_action(Action, Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = proplists:get_value(meId, Config),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

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

%%%--------------------------------------------------------------------
%%% Description: Translate xmllint exit codes to strings
%%%--------------------------------------------------------------------
xmllint_status("1") -> "Unclassified";
xmllint_status("2") -> "Error in DTD";
xmllint_status("3") -> "Validation error";
xmllint_status("4") -> "Validation error";
xmllint_status("5") -> "Error in schema compilation";
xmllint_status("6") -> "Error writing output";
xmllint_status("7") -> "Error in pattern";
xmllint_status("8") -> "Error in Reader registration";
xmllint_status("9") -> "Out of memory error".

%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------
cmd(Cmd) ->
    ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.

%%%--------------------------------------------------------------------
%%% Description: Construct sftp
%%%--------------------------------------------------------------------
sftp_uri(Config) ->
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    %% Dir = proplists:get_value(sftp_root, Config),
    Dir = ?EXP_IMP_DIR,
    NodeName = proplists:get_value(nodeName, Config),
    "sftp://"++User++"@"++Host++Dir++NodeName.


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

%% get_correct_buid(BuName, AllBackUps) ->
%%     BuIds = lists:
%% 	map(fun({'BrmBackup', _, BrmBackup}) ->
%% 		    case lists:keysearch(backupName,1,BrmBackup) of
%% 			{value, {backupName, _, [BuName]}} ->
%% 			    ct:pal("Correct BU name found, "
%% 				   "return bu ID  "),
%% 			    lists:keysearch(backupName,1,BrmBackup),
%% 			    {value, {brmBackupId, _, [Id]}} = 
%% 				lists:keysearch(brmBackupId, 1, BrmBackup),
%% 			    Id;
%% 			_ ->
%% 			    no
%% 		    end
%% 	    end, AllBackUps),
%%     BuId = [X || X <- BuIds, X /= no],
%%     ct:pal("Selected backup ~p to restore.~n",[BuId]),
%%     BuId.


get_rollback_bu_id(MeId) ->
    AllBackUps = swm_br_lib:get_all_backups(?NC_sess, MeId),
    ct:pal("Backups ~p~n",[AllBackUps]),
    List = 
	lists:
	dropwhile(fun({_BrmBackup, _, Data}) ->
			  case lists:keysearch(backupName, 1, Data) of
			      {value,{backupName,[],[BuName]}} ->
				  case re:run(BuName, "Rollback_backup") of
				      {match,_} ->
					  false;
				      _Other ->
					  true
				  end
			  end
		  end, AllBackUps),
    ct:pal("BuIdList: ~p", [List]),
    [WantedList | _] = List,
    {_BrmBackup, _, WantedData} = WantedList,
    {value,{brmBackupId,[],[BuId]}} = 
	lists:keysearch(brmBackupId, 1, WantedData),
    ct:pal("BuId: ~p", [BuId]),
    [BuId].

%%%--------------------------------------------------------------------
%%% Description: Help function to check temp disc space on board
%%%--------------------------------------------------------------------
check_temp_disc_space_percentage() ->
    ok = rct_coli:connect(coli),
    timer:sleep(1000),
    {ok, Reply}  = rct_coli:send(coli, "/os/df"),
    ok = rct_coli:disconnect(coli),
    {match, UsedPercentage} = re:run(Reply, ".*rcs-lv.*\\d+\\s\\d+\\s+\\d+\\s+(\\d+)", [{capture,[1],list}]),
    UsedPercentage.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_nr_of_exp_bu(ExpNr, MeId) ->
        wait_for_nr_of_exp_bu(ExpNr, MeId, 600000).

wait_for_nr_of_exp_bu(ExpNr, _MeId, Timeout) when Timeout < 0 ->
    ct:pal("## ExpNr : ~p does not exist within time, tc will fail", [ExpNr]),
    ct:fail("Expected Nr of BUs does not exist within time.");
    
wait_for_nr_of_exp_bu(ExpNr, MeId, Timeout) ->
    NrOfBu = swm_br_lib:get_all_backups(?NC_sess, MeId),
    Nr = length(NrOfBu),
    %% ct:pal("Nr of BUs : ~p.", [Nr]),
    case Nr of
	ExpNr ->
	    ct:pal("## Nr Expected BUs exist. : ~p", [Nr]),
	    ok;
	 _ ->
	    ct:pal("## ## Nr: ~p, Expected BUs : ~p . not match", [Nr, ExpNr]),
	    timer:sleep(30000),
	    wait_for_nr_of_exp_bu(ExpNr, MeId, Timeout - 30000)
    end.

%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_ug_multiple_actors_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/1
%%%
%%% @doc == Two operator performs upgrade at same times in differents phases. ==
%%% <br/><br/>
%%% @end

-module(swm_ug_multiple_actors_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/1').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/2      2014-06-10 etxivri     Created
%%% R2A/3      2014-06-27 etxivri     Added coding: latin-1
%%% R2A/5      2014-08-15 etxivri     Update test_1 due to correct TR HS87440
%%% R2A/6      2014-08-21 etxivri     more updates.
%%% R2A/7      2014-08-26 etxivri     Updates due to corrected TR.
%%% R2A/8      2014-10-01 etxivri     Update test_4 to make it more robust.
%%% R3A/1      2014-10-23 etxivri     Update due to new behaviour.
%%% R3A/2      2015-01-29 etxivri     Update to remove error in ct-shell.
%%% R3A/3      2015-02-03 etxivri     Update to remove error in ct-shell.
%%% R3A/4      2015-02-10 etxivri     Update to remove error in ct-shell.
%%% R3A/5      2015-02-19 etxivri     Minor update.
%%% R3A/6      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/8      2015-05-29 etxkols     Changed rct_netconf hook format
%%% R3A/9      2015-06-03 etxkols     Update netconf hook.
%%% R4A/1      2015-07-14 etxivri     Make more robust.
%%% R5A/1      2016-01-28 etxivri     Update get highest up label.
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R8A/1      2016-11-25 etxivri     Update due to new behaiour.
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
	 groups/0,
	 all/0]).
-export([test_1/1,
	 test_2/1,
	 test_4/1,
	 create_backup/1,
	 restore_backup/1,
	 delete_backup/1,
	 remove_all_ups/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session1, nc1).
-define(NC_Session2, nc2).
-define(CLI_Session, cli1).

-define(BU_Name_1, "bu_test_1").

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
		 {rct_rs232,console},
		 {rct_core,[]},
                 {rct_logging, {upgrade,
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  ["swmServer: throw eexist"]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1},
		 {rct_netconf,nc2}
		] }].


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
    timer:sleep(30000),
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("Now running TC:  ~w~n",[TestCase]),
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,

    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    %% [create_backup, test_1, test_4, restore_backup].
    [create_backup, test_4, restore_backup, delete_backup, remove_all_ups].

groups() ->
    [{group_1,[],[test_2,
		  remove_all_ups
		 ]},
     {group_2,[],[create_backup, %% to go back to correct sw version.
		  test_1,
		  test_4,
		  restore_backup, 
		  delete_backup,
		  remove_all_ups
		 ]}
    ].

%%%--------------------------------------------------------------------
%%% @doc Two nc sessions performs upgrade on same UP.
%%% - Create up_1.
%%% - 1. Nc1: Create ug-1. <br/>
%%% - 2. Nc1: Start prepare ug-1. <br/>
%%% - 3. Nc2: Create ug-1. Wait for Failure<br/>
%%% - 4. Nc2: Prepare ug-1. ?? Wait for success ?? <br/>
%%% - 5. Nc1: Verify ug-1. Wait for success<br/>
%%% - 6. Nc2: Verify ug-1. ??? Wait for success ??? <br/>
%%% - 7. Nc1: Start activate ug-1. <br/>
%%% - 8. Nc2: Activate ug-1. Wait for success<br/>
%%%
%%% @spec test_1(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
test_1(_Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session1),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ct:pal("ErlNode : ~p", [ErlNode]),

    %%%%
    %% Create valid UP.
    %%%%
    %% create_valid_up(1),
    swm_test_lib:build_valid_ug_packakage(?NC_Session1),

    %% ActionId = no_check,
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    %%%%
    %% 1. Nc1 - Create.
    %%%%
    create(?NC_Session1),

    LabelList1 = swm_test_lib:get_ups(?NC_Session1),
    ct:pal("LabelList1:~n~p~n",[LabelList1]),
    Label1 = swm_test_lib:get_highest_label(LabelList1),
    ct:pal("Label:~n~p~n",[Label1]),

    %%%%
    %% 2. Nc1 - Prepare Label1.
    %%%%
    UP_state2 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 2. Nc1 start prepare, UP state: ~p.",[UP_state2]), 
    ok = swm_test_lib:up_action_generic(?NC_Session1, 
    					prepare, 
    					Label1, 
    					MeId),

    timer:sleep(5000),

    %%%%
    %% 3. Nc2 - Create.
    %%%%
    %% Shall fail due to already created.
    UP_state3 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label1),
    ct:pal("¤¤¤ 3. Nc2 start create, UP state: ~p.",[UP_state3]), 
    swm_test_lib:ug_create_match_result(?NC_Session2,
    					"FAILURE",
    					?SftpHost,
    					?SftpUser,
    					?SftpPassword,
    					UGPath,
    					MeId),

    %%%%
    %% 4. Nc2 - Prepare Label1, shall fail due to state is in PREPARE_COMPLETED.
    %%%%
    UP_state4 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label1),
    ct:pal("¤¤¤ 4. Nc2 start prepare, UP state: ~p. ~n"
	   "This shall fail due to state is in PREPARE_COMPLETED.",
	   [UP_state4]), 

    Res1 = swm_test_lib:up_action_generic_no_check(?NC_Session2, 
						   prepare, 
						   Label1, 
						   MeId),
    ExpErrInfoStr1 = "Prepare can not be executed in state PREPARE_COMPLETED",
    swm_test_lib:check_exp_reply_err_from_nc_action(Res1, 
						    ExpErrInfoStr1),

    %%%%
    %% 5. Nc1 - Verify Label1.
    %%%%
    UP_state5 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 5. Nc1 start verify, UP state: ~p.",[UP_state5]), 
    verify(?NC_Session1, Label1),

    %%%%
    %% 6. Nc2 - Verify Label1.
    %%%%
    UP_state6 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label1),
    ct:pal("¤¤¤ 6. Nc2 start verify, UP state: ~p.",[UP_state6]), 
    verify(?NC_Session2, Label1),

    %%%%
    %% 7. Nc1 - Activate Label1.
    %%%%
    OldActionId_2 = swm_test_lib:get_action_id(?NC_Session2, MeId),
    ct:pal("OldActionId_2 : ~p", [OldActionId_2]),
    UP_state7 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 7. Nc1 start activate, UP state: ~p.",[UP_state7]),
    net_kernel:disconnect(ErlNode),
    ok = swm_test_lib:up_action_generic(?NC_Session1, 
    					activate, 
    					Label1, 
    					MeId),

    timer:sleep(5000),

    %%%%
    %% 8. Nc2 - Activate Label1.
    %%%%
    UP_state8 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label1),
    ct:pal("¤¤¤ 8. Nc2 start activate, UP state: ~p. ~n"
	   "This shall fail due to state is in ACTIVATION_IN_PROGRESS.",
	   [UP_state8]),
    net_kernel:disconnect(ErlNode),
    Res2 = swm_test_lib:up_action_generic_no_check(?NC_Session2, 
						   activate, 
						   Label1, 
						   MeId),
    ExpErrInfoStr2 = 
	"Activate can not be executed in state ACTIVATION_IN_PROGRESS",
    swm_test_lib:check_exp_reply_err_from_nc_action(Res2, 
						    ExpErrInfoStr2),
    net_kernel:disconnect(ErlNode),
    ct_netconfc:close_session(?NC_Session1),
    ct_netconfc:close_session(?NC_Session2),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
			      [{timeout,180000},no_prompt_check]), 
    swm_test_lib:wait_for_login(console),
    %% {ok,_} = ct_telnet:expect(console, "login:", 
    %% 			      [{timeout,30000}, no_prompt_check]),
    swm_test_lib:wait_for_progress_done(?NC_Session1,
					"SUCCESS",
					"activate", %match str in progress
					MeId, 
					no_check,
					ErlNode,
					Label1),

    swm_test_lib:wait_for_exp_up_state(?NC_Session1, 
				       MeId, 
				       Label1, 
				       "WAITING_FOR_COMMIT"),

    %%%%
    %% 9. Nc2 - Activate Label1, again. 
    %% Check Activate can not be executed in state WAITING_FOR_COMMIT
    %%%%
    UP_state9 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label1),
    ct:pal("¤¤¤ 9. Nc2 start activate again, UP state: ~p. ~n"
	   "This shall fail due to state is in WAITING_FOR_COMMIT.",
	   [UP_state9]),
    net_kernel:disconnect(ErlNode),
    Res3 = swm_test_lib:up_action_generic_no_check(?NC_Session2, 
						   activate, 
						   Label1, 
						   MeId),
    ExpErrInfoStr3 = 
	"Activate can not be executed in state WAITING_FOR_COMMIT",
    swm_test_lib:check_exp_reply_err_from_nc_action(Res3, 
						    ExpErrInfoStr3),

    %%%%
    %% Cancel to trig fallback
    %%%%
    net_kernel:disconnect(ErlNode),
    ct:pal("### trig fallback using cancel",[]),
    ok = swm_test_lib:up_action_generic(?NC_Session2, 
    					cancel, 
    					Label1, 
    					MeId),
    %% timer:sleep(5000),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
			      [{timeout,180000}, no_prompt_check]),
    swm_test_lib:wait_for_login(console),
    %% {ok,_} = ct_telnet:expect(console, "login:", 
    %% 			      [{timeout,30000}, no_prompt_check]),
    %% swm_test_lib:wait_for_node_state(ErlNode, down, 180000), %% wait max 3min
    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    swm_test_lib:check_nc_session(?NC_Session2),

    [{"FAILURE", 
      "activate", 
      "The action was interrupted by a restart", 
      _D3, _E3, _ProgReport3}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session2, 
					      MeId, 
					      no_check, 
					      ErlNode),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label1]),
    swm_test_lib:remove_upgrade_package(?NC_Session2, "SUCCESS", MeId, Label1),
    ok.


%%%--------------------------------------------------------------------
%%% @doc Two nc sessions performs upgrade, differents upgrade packages.
%%% - Create up_1.
%%% - 1. Nc1: Create ug-1. <br/>
%%% - Create up_2. <br/>
%%% - 2. Nc1: Start prepare ug-1. <br/>
%%% - 3. Nc2: Create ug-2. Wait for success<br/>
%%% - 4. Nc2: Prepare ug-2. Wait for success<br/>
%%% - 5. Nc1: Verify ug-1. Wait for success<br/>
%%% - 6. Nc2: Verify ug-2. Wait for success<br/>
%%% - 7. Nc1: Start activate ug-1. <br/>
%%% - 8. Nc2: Activate ug-2. Shall not be allowed.
%%% - 9. Nc1: Wait for success. on ug-1<br/>

%%% @spec test_2(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
test_2(_Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session1),

    %%%%
    %% Create, prepare, verify two differents upgradepackage
    %%%%
    {Label1, Label2} = cre_pre_ver_two_ups(MeId),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    %%%%
    %% 7. Nc1 - Activate Label1.
    %%%%
    UP_state7 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 7. Nc1 start activate label1, UP state: ~p.",[UP_state7]),
    net_kernel:disconnect(ErlNode),
    ok = swm_test_lib:up_action_generic(?NC_Session1, 
    					activate, 
    					Label1, 
    					MeId),

    timer:sleep(5000),

    %%%%
    %% 8. Nc2 - Activate Label2.
    %%%%
    UP_state8 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label2),
    ct:pal("¤¤¤ 8. Nc2 start activate label2, UP state: ~p.~n"
	   "Shall not be allowed due to activate is already ongoing.",
	   [UP_state8]),
    ExpErrInfoStr = 
	"An activation is already ongoing, this action will be ignored",
    net_kernel:disconnect(ErlNode),
 
    Res = swm_test_lib:up_action_generic_no_check(?NC_Session2, 
						  activate, 
						  Label2, 
						  MeId),

    swm_test_lib:check_exp_reply_err_from_nc_action(Res, 
						    ExpErrInfoStr),
    net_kernel:disconnect(ErlNode),
    ct_netconfc:close_session(?NC_Session1),
    ct_netconfc:close_session(?NC_Session2),

    {ok,_} = ct_telnet:expect(console, "Restarting system",
			      [{timeout,180000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,30000}, no_prompt_check]),

    %%%%
    %% 9. Nc1 - Wait for SUCCES on activate Label1.
    %%%%
    [{"SUCCESS", 
      "activate", 
      _ProgrInfo, 
      _ResInfo, 
      "FINISHED", 
      _ProgReport}] = swm_test_lib:
    	wait_for_progress_result(?NC_Session1, 
				 MeId, 
				 no_check, 
				 ErlNode, 
				 "activate",
				 Label1), 

    UP_state9 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 9. Nc1, label1, UP state: ~p. After successful activate.",
	   [UP_state9]),

    %%%%
    %% 10. Nc2 - Try to activate Label2 again. Shall fail.
    %%%%
    UP_state10 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label2),
    ct:pal("¤¤¤ 10. Nc2 activate label2, UP state: ~p.",[UP_state10]),
    net_kernel:disconnect(ErlNode),

    Res2 = swm_test_lib:up_action_generic_no_check(?NC_Session2, 
						  activate, 
						  Label2, 
						  MeId),

    swm_test_lib:check_exp_reply_err_from_nc_action(Res2, 
						    ExpErrInfoStr),


    %%%%
    %% Cancel to trig fallback
    %%%%
    ct:pal("### trig fallback using cancel",[]),
    ok = swm_test_lib:up_action_generic(?NC_Session1, 
    					cancel, 
    					Label1, 
    					MeId),


    swm_test_lib:wait_for_node_state(ErlNode, down, 180000), %% wait max 3min
    {ok,_} = ct_telnet:expect(console, "Restarting system", 
    			      [{timeout,60000}, no_prompt_check]),
    swm_test_lib:wait_for_login(console),
    %% {ok,_} = ct_telnet:expect(console, "login:", 
    %% 			      [{timeout,60000}, no_prompt_check]),
    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    net_kernel:connect(ErlNode),
    swm_test_lib:check_nc_session(?NC_Session1),
    net_kernel:disconnect(ErlNode),

    [{"FAILURE", 
      "activate", 
      "The action was interrupted by a restart", 
      _D3, _E3, _ProgReport3}] =
    	swm_test_lib:wait_for_progress_result(?NC_Session1, 
					      MeId, 
					      no_check, 
					      ErlNode,
					      "activate",
					      Label1),

    %% Use tc remove all ups to delete backups.

    ok.


%% %%%--------------------------------------------------------------------
%% %%% @doc  <br/>
%% %%%
%% %%% @spec test_3(_Config) -> ok
%% %%% @end
%% %%%--------------------------------------------------------------------
%% test_3(_Config) ->
%%     %% Runs in test_2.
%%     ok.

%%%--------------------------------------------------------------------
%%% @doc Two nc sessions performs upgrade. <br/>
%%%      Activate second up performed when activate first is SUCCESS. <br/>
%%% - Create up_1.
%%% - 1. Nc1: Create ug-1. <br/>
%%% - Create up_2. <br/>
%%% - 2. Nc1: Start prepare ug-1. <br/>
%%% - 3. Nc2: Create ug-2. Wait for success<br/>
%%% - 4. Nc2: Prepare ug-2. Wait for success<br/>
%%% - 5. Nc1: Verify ug-1. Wait for success<br/>
%%% - 6. Nc2: Verify ug-2. Wait for success<br/>
%%% - 7. Nc1: Activate, Confirm ug-1. Wait for success<br/>
%%% - 8. Nc2: Activate, Confirm ug-2. Wait for success<br/>
%%% - 9. Nc2: Confirm once more, shall not be ok.
%%%
%%% @spec test_4(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
test_4(_Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session1),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    %%%%
    %% Create, prepare, verify two differents upgradepackage
    %%%%
    {Label1, Label2} = cre_pre_ver_two_ups(MeId),

    %%%%
    %% 7. Nc1 - Activate and confirm Label1, successfully.
    %%%%
    timer:sleep(10000),
    UP_state7 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 7. Nc1 start activate label1, UP state: ~p.",[UP_state7]),
    net_kernel:disconnect(ErlNode),
    %% ok = swm_test_lib:up_action_generic(?NC_Session1, 
    %% 					activate, 
    %% 					Label1, 
    %% 					MeId),
    %% timer:sleep(10000), %% needed to ensure activate exist in reportProgress.
    %% [{"SUCCESS", 
    %%   "activate", 
    %%   _D1, _D2, "FINISHED", _ProgReport3}] =
    %% 	swm_test_lib:wait_for_progress_result(?NC_Session1, 
    %% 					      MeId, 
    %% 					      no_check, 
    %% 					      ErlNode, 
    %% 					      activate, 
    %% 					      Label1),

    swm_test_lib:ug_action_match_result(?NC_Session1,
    					"SUCCESS",
    					Label1,
    					MeId,
    					activate,
					dummy,
					console),

    ct:pal("Perform ug confirm on : ~p ~n",[Label1]),
    swm_test_lib:ug_confirm(?NC_Session1,
			    Label1,
			    MeId),

    swm_test_lib:wait_for_exp_up_state(?NC_Session1, 
				       MeId, 
				       Label1, 
				       "COMMIT_COMPLETED"),
    %%%%
    %% 8. Nc2 - Activate and confirm Label2, successfully.
    %%%%
    UP_state8 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label2),
    ct:pal("¤¤¤ 8. Nc2 start activate label2, UP state: ~p.",[UP_state8]),
    net_kernel:disconnect(ErlNode),
    %% swm_test_lib:ug_action_match_result(?NC_Session2,
    %% 					"SUCCESS",
    %% 					Label2,
    %% 					MeId,
    %% 					activate),

    ct_netconfc:close_session(?NC_Session1),
    ct_netconfc:close_session(?NC_Session2),
    swm_test_lib:ug_action_match_result(?NC_Session2,
    					"SUCCESS",
    					Label2,
    					MeId,
    					activate,
					dummy,
					console),

    %% swm_test_lib:wait_for_exp_up_state(?NC_Session2, 
    %% 				       MeId, 
    %% 				       Label2, 
    %% 				       "WAITING_FOR_COMMIT"),

    ct:pal("Perform ug confirm on : ~p ~n",[Label2]),
    swm_test_lib:ug_confirm(?NC_Session2,
    			    Label2,
    			    MeId),

    %%%%
    %% 9. Nc2 - confirm Label2 again, shall not be allowed due to it is 
    %%          already COMMIT_COMPLETED.
    %%%%
    ct:pal("Perform ug confirm on one more time: ~p ~n",[Label2]),
    ExpErrInfoStr = "Confirm can not be executed in state COMMIT_COMPLETED",
    Res = swm_test_lib:up_action_generic_no_check(?NC_Session2, 
						  confirm, 
						  Label2, 
						  MeId),

    swm_test_lib:check_exp_reply_err_from_nc_action(Res, 
						    ExpErrInfoStr),

    swm_test_lib:wait_for_exp_up_state(?NC_Session2, 
				       MeId, 
				       Label2, 
				       "COMMIT_COMPLETED"),
    
    ok.

%%%--------------------------------------------------------------------
%%% - Create up_1.
%%% - 1. Nc1: Create ug-1. <br/>
%%% - Create up_2. <br/>
%%% - 2. Nc1: Start prepare ug-1. <br/>
%%% - 3. Nc2: Create ug-2. Wait for success<br/>
%%% - 4. Nc2: Prepare ug-2. Wait for success<br/>
%%% - 5. Nc1: Verify ug-1. Wait for success<br/>
%%% - 6. Nc2: Verify ug-2. Wait for success<br/>
%%%--------------------------------------------------------------------
cre_pre_ver_two_ups(MeId) ->
    %%%%
    %% Create valid UP.
    %%%%
    %% create_valid_up(1), %% stegar upp cxs och cxp xml:er funkar ej nar Jenkins kor TC.
    swm_test_lib:build_valid_ug_packakage(?NC_Session1),

    %% ActionId = no_check,
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    %%%%
    %% 1. Nc1 - Create.
    %%%%
    create(?NC_Session1),

    LabelList1 = swm_test_lib:get_ups(?NC_Session1),
    ct:pal("LabelList1:~n~p~n",[LabelList1]),
    Label1 = swm_test_lib:get_highest_label(LabelList1),
    ct:pal("Label1:~n~p~n",[Label1]),

    %%%%
    %% Create valid UP.
    %%%%
    %% create_valid_up(2),
    swm_test_lib:modify_cxs(Label1, 1),

    %%%%
    %% 2. Nc1 - Prepare Label1.
    %%%%
    UP_state2 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 2. Nc1 start prepare label1, UP state: ~p.",[UP_state2]), 
    swm_test_lib:ug_action_match_result(?NC_Session1, 
    					"SUCCESS",
    					Label1,
    					MeId, 
    					prepare),

    %%%%
    %% 3. Nc2 - Create second Label.
    %%%%
    create(?NC_Session2),

    LabelList2 = swm_test_lib:get_ups(?NC_Session2),
    ct:pal("LabelList2:~n~p~n",[LabelList2]),
    Label2 = swm_test_lib:get_highest_label(LabelList2),
    ct:pal("Label2:~n~p~n",[Label2]),

    %%%%
    %% 4. Nc2 - Prepare Label2.
    %%%%
    UP_state4 = swm_test_lib:get_up_state(?NC_Session2, MeId, Label2),
    ct:pal("¤¤¤ 4. Nc2 start prepare label2, UP state: ~p.",[UP_state4]), 
    swm_test_lib:ug_action_match_result(?NC_Session2, 
    					"SUCCESS",
    					Label2,
    					MeId, 
    					prepare),

    %%%%
    %% 5. Nc1 - Verify Label1.
    %%%%
    UP_state5 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label1),
    ct:pal("¤¤¤ 5. Nc1 start verify label1, UP state: ~p.",[UP_state5]), 
    swm_test_lib:ug_action_match_result(?NC_Session1, 
					"SUCCESS",
					Label1,
					MeId, 
					verify),
    %%%%
    %% 6. Nc2 - Verify Label1.
    %%%%
    UP_state6 = swm_test_lib:get_up_state(?NC_Session1, MeId, Label2),
    ct:pal("¤¤¤ 6. Nc2 start verify label2, UP state: ~p.",[UP_state6]), 
    swm_test_lib:ug_action_match_result(?NC_Session2, 
					"SUCCESS",
					Label2,
					MeId, 
					verify),

    {Label1, Label2}.




%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec create(NC_Session) -> ok
%%% @end
%%%--------------------------------------------------------------------
create(NC_Session) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    swm_test_lib:ug_create_match_result(NC_Session,
    					"SUCCESS",
    					?SftpHost,
    					?SftpUser,
    					?SftpPassword,
    					UGPath,
    					MeId),
    ok.

%% %%%--------------------------------------------------------------------
%% %%% @doc
%% %%% Prepare. <br/>
%% %%% @spec prepare(NC_Session, Label) -> ok
%% %%% @end
%% %%%--------------------------------------------------------------------
%% prepare(NC_Session, Label) ->

%%     %%%%
%%     %% Prepares an upgrade package,
%%     %% which means downloading a complete UP
%%     %%%%
%%     perform_ug_action(prepare, NC_Session, Label),

%%     ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% verify. <br/>
%%% @spec verify(NC_Session, Label) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify(NC_Session, Label) ->

    %%%%
    %% Verifies an upgrade package.
    %%%%
    perform_ug_action(verify, NC_Session, Label),

    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action, NC_Session, Label) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(NC_Session),

    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),
    ok.


%% %% %%%--------------------------------------------------------------------
%% %% %%% @doc This will not work when test runs by jenkins.
%% %% %%% @end
%% %% %%%--------------------------------------------------------------------
%% create_valid_up(CountNr) ->
%%     ct:pal("CountNr: ~p", [CountNr]),
%%     {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
%%     ct:pal("UGPath: ~p", [UGPath]),
%%     StpName = lists:last(string:tokens(UGPath, "/")),
%%     ct:pal("StpName: ~p", [StpName]),

%%     CMD = "$RCT_TOP/test/bin/rcs_build_valid_to_up.sh -S /proj/rcs-tmp/tftpboot/"++StpName++"/RCP-*.cxs -N "++integer_to_list(CountNr),
%%     ct:pal("CMD: ~p", [CMD]),

%%     A1 = os:cmd(CMD),
%%     ct:pal("GG:~n~p~n",[A1]),

%%     CMD2 = "rm -rf "++UGPath++"/*",
%%     A2 = os:cmd(CMD2),
%%     ct:pal("A2:~n~p~n",[A2]),
    
%%     [Signum] = string:tokens(os:cmd("echo $USER"), "\n"),
    
    

%%     CMD3 = "cp /tmp/"++Signum++"/rcs_mod/* "++UGPath,
%%     A3 = os:cmd(CMD3),
%%     ct:pal("A3:~n~p~n",[A3]),

%%     ok.

%%     %% ct:pal("Remove existing files in upgrade dir",[]),
%%     %% %% CMD = "\rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
%%     %% C_M_D = "chmod 777 /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
%%     %% ct:pal("CMD:~n~p~n",[C_M_D]),
%%     %% os:cmd(C_M_D),
%%     %% CMD = "rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
%%     %% ct:pal("CMD:~n~p~n",[CMD]),
%%     %% os:cmd(CMD),



%%% ===========================================================================
%%% @doc
%%% Create backup <br/>
%%% Using netconf to trig a backup to be created. <br/>
%%% @spec create_backup(Config) -> ok
%%% @end
%%% ===========================================================================
create_backup(_Config) ->  
    MeId = swm_test_lib:get_me_id(?NC_Session1),
    ActionId = swm_br_lib:get_action_id(?NC_Session1, MeId),

    swm_br_lib:create_backup_successful(?NC_Session1, 
					MeId, 
					?BU_Name_1, 
					ActionId).


%%% ===========================================================================
%%% @doc
%%% Restore backup <br/>
%%% Using netconf to trig a restore backup . <br/>
%%% @spec restore_backup(_Config) -> ok
%%% @end
%%% ===========================================================================
restore_backup(_Config) ->
    MeId = swm_test_lib:get_me_id(?NC_Session1),

    %%%%
    %% Get BuId that belongs to BU_name.
    %%%%
    Backups = swm_br_lib:get_all_backups(?NC_Session1, MeId),

    BuId = swm_br_lib:get_correct_buid(?BU_Name_1, Backups), %% in a list.
    ct:pal("Selected buId: ~p, to restore.~n",[BuId]),

    ActionId = swm_br_lib:get_action_id(?NC_Session1, MeId, {brmBackup, BuId}),

    swm_br_lib:restore_backup_successful(?NC_Session1, 
					 MeId, 
					 BuId, 
					 ActionId, 
					 BuId).

%%% ===========================================================================
%%% @doc
%%% Delete backup <br/>
%%% Using netconf to trig a backup to be removed. <br/>
%%% @spec delete_backup(_Config) -> ok
%%% @end
%%% ===========================================================================
delete_backup(_Config) ->
    ct:pal("# Backup shall not be possible to delete"),
    MeId = swm_test_lib:get_me_id(?NC_Session1),
    %% ActionId = swm_br_lib:get_action_id(?NC_Session1, MeId),
    ActionId = no_check,
    %% swm_br_lib:delete_backup_successful(?NC_Session1, 
    %% 					MeId, 
    %% 					?BU_Name_1, 
    %% 					ActionId).
    swm_br_lib:delete_backup(?NC_Session1, 
			     MeId, 
			     ?BU_Name_1), 
    case swm_br_lib:wait_for_expecting_state(?NC_Session1, 
					     MeId, ActionId, "FINISHED") of
    	[{"FAILURE", 
    	  "DELETE", 
    	  _ProgressInfo, 
    	  _ResultInfo, 
    	  _State, 
    	  ProgressReport}] ->
    	    ct:pal("Backup NOT deleted as expected.~n~p", [ProgressReport]),
    	    ok;
    	 _ErrRes ->
    	    ct:pal("deleteBackup: ~p~n",[_ErrRes]),
    	    ct:fail(_ErrRes)
    end,
    ok.

%%% ===========================================================================
%%% @doc
%%% Delete backup <br/>
%%% Using netconf to trig a backup to be removed. <br/>
%%% @spec remove_all_ups(_Config) -> ok
%%% @end
%%% ===========================================================================
remove_all_ups(_Config) ->
    %% Don't care about the result.
    UPs = swm_test_lib:get_ups(?NC_Session1),
    lists:foreach(fun(Label) ->
			  ct:pal("Label:~n~p~n",[Label]),
			  swm_test_lib:remove_upgrade_package(?NC_Session1, 
							      Label)
		  end, UPs).

    %% %%%%
    %% %% Get managedElementId
    %% %%%%
    %% MeId = swm_test_lib:get_me_id(?NC_Session1),

    %% %%%%
    %% %% Get Latest UP
    %% %%%%
    %% [_H | T] = swm_test_lib:get_ups(?NC_Session1),
    %% ct:pal("Labels:~n~p~n",[T]),

    %% %%%%
    %% %% Remove the created up.
    %% %%%%
    %% lists:foreach(fun(Label) ->
    %% 			  ct:pal("Remove upgrade package: : ~s",[Label]),
    %% 			  swm_test_lib:remove_upgrade_package(?NC_Session1, 
    %% 							      "SUCCESS", 
    %% 							      MeId, 
    %% 							      Label)
    %% 		  end, T),
    %% ok.

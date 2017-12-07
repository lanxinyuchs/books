%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_ug_power_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R8A/R11A/1
%%% 
%%% @doc ==  power off/on in differents phases during upgrade.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_ug_power_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R7A/R8A/R11A/1').

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
%%% R2A/2      2014-05-12 etxivri     Created
%%% R2A/3      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/5      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R2A/6      2014-06-25 etxivri     Added check that ntp has synced.
%%% R2A/7      2014-06-26 etxivri     Use function in swm_test_lib
%%% R2A/8      2014-07-11 etxivri     Update remove check.
%%% R2A/9      2014-08-11 etxivri     Update due to new behaivour in 
%%%                                   swm reportProgress.
%%% R2A/10     2014-08-12 etxivri    Update due to new behaviour.
%%% R2A/14     2014-08-14 etxivri    Use check in  swm_test_lib when remove up.
%%% R2A/12     2014-08-29 etxivri    Update to use modify_cxs in swm_test_lib.
%%% R2A/13     2014-09-26 etxivri    Minor updates.
%%% R3A/1      2014-10-17 etxivri    Update to make it more robust.
%%% R3A/2      2014-10-23 etxivri    More update to make it more robust.
%%% R3A/3      2014-11-04 etxivri    More update to make it more robust.
%%% R3A/2      2015-01-13 etxivri    Update to avoid ERROR in ct shell
%%%                                   when node restarts.
%%% R3A/5      2015-01-22 etxivri    Update error filter.
%%% R3A/6      2015-02-02 etxivri    Update check login.
%%% R3A/7      2015-04-10 etxivri    Minor update.
%%% R3A/8      2014-10-17 etxivri    Update to make it more robust.
%%% R3A/9      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/10     2015-05-29 etxkols     Changed rct_netconf hook format 
%%% R3A/11     2015-05-29 etxkols     A try to make it more robust.
%%% R3A/12     2015-06-17 etxivri     A try to make it more robust.
%%% R4A/1      2015-07-16 etxivri     Update get to label.
%%% R4A/2      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/3      2015-10-20 etxivri     Update due to new behaviour.
%%% R4A/4      2015-10-30 etxivri     Update to get erl logs. Update in 
%%%                                   confirm_power for for tcu.
%%% R4A/5     2015-11-03 etxivri     A try to make it more robust.
%%% R4A/6     2015-11-10 etxivri     Update to use get_all only in power tests.
%%% R4A/7     2015-12-01 etxivri     Update due to changed behaviour. 
%%% R5A/1     2016-01-28 etxivri     Update to be more robust. 
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% R8A/1      2016-11-28 etxivri    Update horror filter.
%%% R8A/1      2016-12-28 etxivri    Update to be more robust. 
%%% R11A/1     2017-08-29 etxivri    Update to be more robust.
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
-export([build_to_up/1,
	 modify_cxs/1,
	 %% random_power/1,
	 create_power/1,
	 create_done_power/1,
	 prepare_power/1,
	 prepare_done_power/1,
	 verify_power/1,
	 verify_done_power/1,
	 activate_power_early/1,
	 activate_power_late/1,
	 activate_done_power/1,
	 confirm_power/1,
	 confirm_done_power/1,
	 remove/1
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
    [{timetrap, {hours, 1}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_upgrade,ug1},
		 {rct_power,node},
		 {rct_rs232,console},
		 {cth_conn_log, []},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  [
					   %% "throw {wrongState,3}",
					   %% "error: badarg",
					   %% "{exit_code,2}",
					   %% "{expected_element_start_tag",
					   %% "sync_send_all_state_event"
					   "Confirm failed"
					  ]
					 }}]}},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_netconf,nc1}]}].


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
    %% rct_logging:get_all(log1), %% To get all erl log after power
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Wait for synch so no time diff occours.
    wait_for_ntp_synch(),
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n"
    		   "Clean up! ", [Reason]),
	    ok
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [build_to_up,
     create_power].

groups() ->
    [{group_1,[],[%%build_to_up,
		  modify_cxs,
		  create_power,
		  prepare_power,
		  verify_power,
		  activate_power_early,
		  activate_power_late,
		  confirm_power,
		  remove
		 ]},
     {group_2,[],[%%build_to_up,
		  modify_cxs,
		  create_done_power,
		  prepare_done_power,
		  verify_done_power,
		  activate_done_power,
		  confirm_done_power
		  %% remove
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
%%% @doc Modify cxs.
%%% @end
%%%--------------------------------------------------------------------
modify_cxs(_Config) ->
    modfy_cxs().
modfy_cxs() ->
    %%%%
    %% Get managedElementId
    %%%%
    %% MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Modify version in cxs to UP.
    %%%%
    %% ct:log("# init per suite. ~n"
    %% 	   "Create cxps that shall be used for UG."), 

    %% SW_Vers = swm_test_lib:get_sw_version(?NC_Session, MeId),
    %% ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    %% swm_test_lib:modify_cxs(SW_Vers, 1).
    swm_test_lib:modify_existing_cxs_up_xml(ug1).
 

%%%--------------------------------------------------------------------
%%% @doc Start create and then perform a power cycle.
%%% @spec create_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    %% ActionId = no_check,

    net_kernel:disconnect(ErlNode),

    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    power_cycle(ErlNode),

    ct:pal("Wait for progress after power.", []),

    %% %% After a power off / on the created UP could sometimes not exist
    UpList = swm_test_lib:get_ups(?NC_Session),
    Label = get_latest_up(),
    ct:log("Check Label:~n~p~n",[Label]),
    case length(UpList) of 
	1 -> 
	    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
					       MeId, 
					       Label, 
					       "COMMIT_COMPLETED"),
	    ct:log("New UP not exist after power off/on, create again ", []),
	    swm_test_lib:ug_create_match_result(?NC_Session,
						"SUCCESS",
						?SftpHost,
						?SftpUser,
						?SftpPassword,
						UGPath,
						MeId);
	    
	    _Other ->
		  swm_test_lib:wait_for_exp_up_state(?NC_Session, 
						     MeId, 
						     Label, 
						     "INITIALIZED")
	  end,

    ok.

%%%--------------------------------------------------------------------
%%% @doc Perform create and then perform a power cycle.
%%% @spec create_done_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create_done_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    ActionId = no_check,

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    net_kernel:disconnect(ErlNode),

    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),
  
    [{"SUCCESS" = Result, 	  
      "createUpgradePackage" = ActionName, 
      ProgressInfo, 
      ResultInfo, 
      _State, _ProgReport}] =  
	swm_test_lib:wait_for_swm_progress_result(?NC_Session, MeId, ActionId),
    ct:pal("result:~p~n"
	   "actionName:~p~n"
	   "progressInfo:~p~n"
	   "resultInfo:~p~n",[Result, 
			      ActionName, 
			      ProgressInfo, 
			      ResultInfo]),

    Label = get_latest_up(),

    synch(),
    power_cycle(ErlNode),

    ct:pal("Wait for progress after power.", []),

    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "INITIALIZED"),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove and create to make next TCs more robust: : ~p",[Label]),
    remove_and_create(MeId, Label),
    %% swm_test_lib:remove_upgrade_package(?NC_Session, 
    %% 					"SUCCESS", 
    %% 					MeId, 
    %% 					Label),

    %% swm_test_lib:ug_create_match_result(?NC_Session,
    %% 					"SUCCESS",
    %% 					?SftpHost,
    %% 					?SftpUser,
    %% 					?SftpPassword,
    %% 					UGPath,
    %% 					MeId),
    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start prepare and then perform a power cycle.
%%% @spec prepare_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    %% %%%%
    %% %% Create UG
    %% %%%%
    %% create(MeId),

    Label = get_latest_up(),

    RandomProcentage = get_random_procentage(90),
    ct:pal("TC will wait for procentage : ~p, is reached, ~n"
	   "then power cycle will be performed.", [RandomProcentage]),
    net_kernel:disconnect(ErlNode),

    %%%%
    %% Prepares an upgrade package, 
    %%%%
    ActionId = no_check,
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					prepare, 
    					Label, 
    					MeId),

    %%%%
    %% Wait for prepare procentage is reached,  
    %% Then power cycle.
    %%%%
    wait_for_percentage_to_be_runed(?NC_Session, MeId, RandomProcentage, Label),
    power_cycle(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    %% swm_test_lib:wait_for_exp_up_state(?NC_Session, 
    %% 				       MeId, 
    %% 				       Label, 
    %% 				       "INITIALIZED"),
    %% ct:pal("this need remove and create again",[]),

    A = swm_test_lib:wait_for_progress_result(?NC_Session, 
					      MeId, 
					      ActionId, 
					      ErlNode, 
					      "Prepare", 
					      Label,
					      60000),
    ct:log("Prog Result: ~p", [A]),

    State = swm_test_lib:get_up_state(?NC_Session, MeId, Label),
    ct:pal("State ~p.", [State]),
    case State of
	"INITIALIZED" ->
	    ok;
	"PREPARE_COMPLETED" ->
	    ok
    end,

    remove_and_create(MeId, Label),
    
    %%%%
    %% Prepare, To set correct state.
    %%%%
    prepare(MeId, Label, ErlNode, ActionId),

    ok.

remove_and_create(MeId, Label) ->
    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~s",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session,
					"SUCCESS",
					MeId,
					Label),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("Create from UGPath:~n~p~n",[UGPath]),
    swm_test_lib:ug_create_match_result(?NC_Session,
					"SUCCESS",
					?SftpHost,
					?SftpUser,
					?SftpPassword,
					UGPath,
					MeId).

%%%--------------------------------------------------------------------
%%% @doc Perform prepare and then perform a power cycle.
%%% @spec prepare_done_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare_done_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    ActionId = no_check,
    net_kernel:disconnect(ErlNode),

    %%%%
    %% Prepare
    %%%%
    prepare(MeId, Label, ErlNode, ActionId),

    synch(),
    timer:sleep(10000),

    %%%%
    %% Then power cycle.
    %%%%
    power_cycle(ErlNode),
    timer:sleep(5000),
    ct_telnet:expect(console, "login:", 
		     [{timeout,60000}, no_prompt_check]),

    swm_test_lib:check_nc_session(?NC_Session),
    ct_netconfc:close_session(?NC_Session),
    timer:sleep(10000),
    State = swm_test_lib:get_up_state(?NC_Session, MeId, Label),
    ct:pal("State ~p.", [State]),
    case State of
	"INITIALIZED" ->
	    prepare(MeId, Label, ErlNode, ActionId);
	 "PREPARE_COMPLETED" ->
	    ok
    end,
    
    %% ct:pal("Wait for up state after reboot.", []),
    %% swm_test_lib:wait_for_exp_up_state(?NC_Session, 
    %% 				       MeId, 
    %% 				       Label, 
    %% 				       "PREPARE_COMPLETED"),

    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start verify and then perform a power cycle.
%%% @spec verify_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify_power(_Config) ->
    get_all(),
    timer:sleep(30000), %% Make sure synch is done before next power.

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
    net_kernel:disconnect(ErlNode),

    %%%%
    %% verify. 
    %%%%
    %% ActionId = no_check,
    ct:pal("verify.", []),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					verify, 
    					Label, 
    					MeId),

    %%%%
    %% power cycle.
    %%%%
    power_cycle(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "PREPARE_COMPLETED"),

    timer:sleep(20000),
    verify(MeId, Label, ErlNode, no_check),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Perform verify and then perform a power cycle.
%%% @spec verify_done_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify_done_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
    net_kernel:disconnect(ErlNode),

    ActionId = no_check,

    %%%%
    %% verify. 
    %%%%
    ct:pal("verify.", []),
    verify(MeId, Label, ErlNode, ActionId),

    synch(),

    %%%%
    %% power cycle.
    %%%%
    power_cycle(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "PREPARE_COMPLETED"),

    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start activate and then perform a power cycle in early phase.
%%% @spec activate_power_early(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_power_early(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
    net_kernel:disconnect(ErlNode),

    %% RandomProcentage = get_random_procentage(22),
    %% ct:pal("TC will wait for procentage : ~p, is reached, ~n"
    %% 	   "then power cycle will be performed.", [RandomProcentage]),
    %% %% test_server:break("A"),

    %%%%
    %% activate an upgrade package, 
    %%%%
    %% ActionId = no_check,
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %%%%
    %% Wait for prepare procentage is reached,  
    %% Then power cycle.
    %%%%
    %% wait_for_percentage_to_be_runed(?NC_Session, MeId, RandomProcentage, Label),
    ct:pal("TC will wait for additionalInfo Installing upgrade package exist"),
    wait_for_trig_value_exist(?NC_Session, MeId, "Installing upgrade package", Label),
    ct:pal("Perform power off / on.", []),
    power_cycle(ErlNode),

    %% test_server:break("A"),

    %% ct:pal("Wait for progress after power.", []),

    %% %% Result = swm_test_lib:wait_for_expecting_state(?NC_Session,
    %% %% 						   MeId, 
    %% %% 						   ActionId, 
    %% %% 						   "FINISHED"),
    %% %% ct:pal("Result: ~p.", [Result]),
    %% wait_for_exp_result(MeId, 
    %% 			ActionId, 
    %% 			ErlNode, 
    %% 			"FAILURE",
    %% 			"activate",
    %% 			"The action was interrupted by a restart"),

    ct:pal("Wait for up state after power on.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "PREPARE_COMPLETED"),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Start activate and then perform a power cycle in late phase.
%%%      After expected restart.
%%% @spec activate_power_late(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_power_late(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
    net_kernel:disconnect(ErlNode),

    RandomNr = get_random_number(3),

    %%%%
    %% activate an upgrade package, 
    %%%%
    ActionId = no_check,
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, down),
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    ct:pal("Login prompt rceived. power node soon!"),
    net_kernel:connect(ErlNode),

    %%%%
    %% sleep random nr
    %% Then power cycle.
    %%%%
    ct:sleep({seconds, RandomNr}),
    ct:pal("Perform power off / on.", []),
    power_cycle(ErlNode),

    ct:pal("# Wait for progress after power.", []),
    wait_for_exp_result(MeId, 
			ActionId, 
			ErlNode, 
			"FAILURE",
			"activate",
			"The action was interrupted by a restart"),

%%     %%%%
%%     %% activate,  To set correct state
%%     %%%%
%%     ct:pal("# Again activate, To get into correct state.", []),

%%     activate(MeId, Label, ErlNode, no_check),
%%     ct:pal("### Activate done successful.",[]),

%%     ok.
    %% test_server:break("A"),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Temporary!! Remove upgrade package: : ~p",[Label]),
    %% swm_test_lib:remove_upgrade_package(?NC_Session, "SUCCESS", MeId, Label),
    %% remove_and_create(MeId, Label),

    swm_test_lib:remove_upgrade_package(?NC_Session, 
					Label),
    timer:sleep(60000),
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("Create from UGPath:~n~p~n",[UGPath]),
    swm_test_lib:ug_create_match_result(?NC_Session,
					"SUCCESS",
					?SftpHost,
					?SftpUser,
					?SftpPassword,
					UGPath,
					MeId),

    prepare(MeId, Label, ErlNode, no_check),
    verify(MeId, Label, ErlNode, no_check),
    activate(MeId, Label, ErlNode, no_check),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Perform activate and then perform a power cycle.
%%% @spec activate_done_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_done_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    %% ActionId = swm_test_lib:get_action_id(?NC_Session, MeId),
    ActionId = no_check,
    net_kernel:disconnect(ErlNode),

    %%%%
    %% activate, 
    %%%%
    activate(MeId, Label, ErlNode, no_check),

    synch(),

    timer:sleep(30000),

    %%%%
    %% Then power cycle.
    %%%%
    power_cycle(ErlNode),

    ct:pal("Wait for progress after power.", []),
    wait_for_exp_result(MeId, 
			ActionId, 
			ErlNode, 
			"FAILURE",
			"activate",
			"The action was interrupted by a restart"),

    %%%%
    %% activate,  To set correct state
    %%%%
    activate(MeId, Label, ErlNode, no_check),
    ct:pal("### Activate done successful.",[]),

    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start confirm and then perform a power cycle.
%%% @spec confirm_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
    net_kernel:disconnect(ErlNode),

    %%%%
    %% confirm
    %%%%	    
    %% ActionId = no_check,
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					confirm, 
    					Label, 
    					MeId),

    RandomNr = get_random_number(5),

    %%%%
    %% sleep random nr.
    %% Then power cycle.
    %%%%
    ct:sleep({seconds, RandomNr}),
    power_cycle(ErlNode),
    
    ct:sleep({seconds, 60}),
    ct:pal("Check for up state after power.", []),
    UpState = swm_test_lib:get_up_state(?NC_Session, MeId, Label),
    case UpState of
	UpState when UpState == "PREPARE_COMPLETED";
		     UpState == "COMMIT_COMPLETED" ->
	    ok;
	_Else ->
	    ct:pal("Up State: ~p , is not expected. Tc will fail", [_Else]),
	    ct:fail("Up State is not expected. Tc will fail")
    end,

    ok.


%%%--------------------------------------------------------------------
%%% @doc Perform confirm and then perform a power cycle.
%%% @spec confirm_done_power(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm_done_power(_Config) ->
    get_all(),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
    net_kernel:disconnect(ErlNode),

    %%%%
    %% confirm
    %%%%	    
    %% confirm(MeId, Label, ErlNode, ActionId),
    confirm(MeId, Label),
    synch(),

    %% timer:sleep(30000),

    %%%%
    %% Then power cycle.
    %%%%
    power_cycle(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
    				       MeId, 
    				       Label, 
    				       "COMMIT_COMPLETED"),
    %% swm_test_lib:wait_for_exp_up_state(?NC_Session, 
    %% 				       MeId, 
    %% 				       Label, 
    %% 				       "PREPARE_COMPLETED"),

    sleep(),
    ok.


%%%--------------------------------------------------------------------
%%% @doc Remove old up.
%%% @spec remove(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
remove(_Config) ->
    timer:sleep(60000),  %% Make sure system bu is removed.
    ct:pal("remove.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    
    %% [Label | _] = swm_test_lib:get_ups(?NC_Session),
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UPs:~n~p~n",[UPs]),
    Label = swm_test_lib:get_highest_label(UPs),
    ct:pal("Label:~n~p~n",[Label]),

    swm_test_lib:remove_upgrade_package(?NC_Session, 
    					Label),

    swm_test_lib:remove_up_check_allowed_result(?NC_Session,
    						MeId),
    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc wait for check result.
%%% @end
%%%--------------------------------------------------------------------
wait_for_exp_result(MeId, ActionId, ErlNode, Result, Action) ->
    case swm_test_lib:
    	wait_for_progress_result(?NC_Session, MeId, ActionId, ErlNode) of
    	[{Result = Res, 
    	  Action = ActionName, 
    	  ProgressInfo, 
	  ResultInfo, 
    	  State, _ProgReport}] ->
    	    ct:pal("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n"
    		   "state:~p~n",[Res, 
    				 ActionName, 
    				 ProgressInfo, 
    				 ResultInfo,
    				 State]),
    	    ok;
    	_Res ->
    	    ct:pal("# Fail Result: ~p",[_Res]),
    	    ct:fail(_Res)
    end.

wait_for_exp_result(MeId, ActionId, ErlNode, Result, Action, ResInfo) ->
    case swm_test_lib:
    	wait_for_progress_result(?NC_Session, MeId, ActionId, ErlNode) of
    	[{Result = Res, 
    	  Action = ActionName, 
    	  ProgressInfo, 
	  ResInfo = ResultInfo, 
    	  State, _ProgReport}] ->
    	    ct:pal("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n"
    		   "state:~p~n",[Res, 
    				 ActionName, 
    				 ProgressInfo, 
    				 ResultInfo,
    				 State]),
    	    ok;
    	_Res ->
    	    ct:pal("## Fail Result: ~p",[_Res]),
    	    ct:fail(_Res)
    end.


%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
%% %%%--------------------------------------------------------------------
%% %%% @doc
%% %%% create. <br/>
%% %%% @end
%% %%%--------------------------------------------------------------------
%% create(MeId) ->
%%     %%%%
%%     %% Create UG
%%     %%%%
%%     {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
%%     ct:pal("UGPath:~n~p~n",[UGPath]),

%%     swm_test_lib:ug_create_match_result(?NC_Session,
%%     					"SUCCESS",
%%     					?SftpHost,
%%     					?SftpUser,
%%     					?SftpPassword,
%%     					UGPath,
%%     					MeId),
%%     ok.


%%%--------------------------------------------------------------------
%%% @doc
%%% prepare. <br/>
%%% @end
%%%--------------------------------------------------------------------

prepare(MeId, Label, ErlNode, ActionId) ->
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					prepare, 
    					Label, 
    					MeId),

    wait_for_exp_result(MeId, 
			ActionId, 
			ErlNode, 
			"SUCCESS",
			"prepare"),
ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% verify. <br/>
%%% @end
%%%--------------------------------------------------------------------

verify(MeId, Label, ErlNode, ActionId) ->
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					verify, 
    					Label, 
    					MeId),

    wait_for_exp_result(MeId, 
			ActionId, 
			ErlNode, 
			"SUCCESS",
			"verify"),
    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%% activate. <br/>
%%% @end
%%%--------------------------------------------------------------------
%% activate(MeId, Label) ->

%%     %%%%
%%     %% Activates an upgrade package.
%%     %% During activate the node will reboot.
%%     %%%%
%%     perform_ug_action(activate, MeId, Label),
%%     ok.

activate(MeId, Label, ErlNode, ActionId) ->
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    wait_for_exp_result(MeId, 
			ActionId, 
			ErlNode, 
			"SUCCESS",
			"activate"),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% confirm. <br/>
%%% @end
%%%--------------------------------------------------------------------
confirm(MeId, Label) ->

    %%%%
    %% Confirm
    %%%%
    perform_ug_action(confirm, MeId, Label),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "COMMIT_COMPLETED"),

    ok.


%% confirm(MeId, Label, ErlNode, ActionId) ->
%%     ok = swm_test_lib:up_action_generic(?NC_Session, 
%%     					confirm, 
%%     					Label, 
%%     					MeId),

%%     wait_for_exp_result(MeId, 
%% 			ActionId, 
%% 			ErlNode, 
%% 			"SUCCESS",
%% 			"confirm"),
%%     ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Action, MeId, Label) ->
    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),
    ok.


%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
get_progress_percentage(NC_Session, MeId, UP_Label) ->
    ReportProgress =
	swm_test_lib:get_up_report_progress(NC_Session, MeId, UP_Label),
    %% ct:pal("ReportProgress: ~p", [ReportProgress]),

    {progressPercentage,[],[Percentage]} = 
	lists:keyfind(progressPercentage, 1, ReportProgress),
    ct:pal("Percentage: ~p", [Percentage]),
    Percentage.
%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label) ->
    wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, 360).

wait_for_percentage_to_be_runed(_NC_Session, _MeId, _CheckVal, _UP_Label, Timeout) 
  when Timeout < 0 ->
    ct:fail(" prepare not runed within expected time.");
    
wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, Timeout) ->
    Sec = get_progress_percentage(NC_Session, MeId, UP_Label),
    Percentage = list_to_integer(Sec),
    case Percentage of
	Val when Val >= CheckVal ->
	    ct:pal("## progressPercentage: ~p", 
		   [Percentage]),
	    ok;
	 _ ->
	    timer:sleep(5000),
	    wait_for_percentage_to_be_runed(NC_Session, MeId, CheckVal, UP_Label, 
					 Timeout - 5)
    end.

%%%--------------------------------------------------------------------
%%% @doc get random prgress procentage .
%%% @end
%%%--------------------------------------------------------------------
get_random_procentage(Procentage) ->
    %%%%
    %% Get a random nr between 0-90, that will be used to wait for 
    %%progress procentage, before power cycle.
    %%%%
    Random = rand:uniform(Procentage),
    ct:pal("TC will wait for procentage : ~p, is reached", [Random]),
    Random.

%%%--------------------------------------------------------------------
%%% @doc get random number.
%%% @end
%%%--------------------------------------------------------------------
get_random_number(MaxNr) ->
    %%%%
    %% Get a random nr.
    %%%%
    RandomNr = rand:uniform(MaxNr),
    ct:pal("TC will use Random nr: ~p",[RandomNr]),
    RandomNr.

%%--------------------------------------------------------------------
%%% @doc power cycle .
%%% @end
%%%--------------------------------------------------------------------
power_cycle(ErlNode) ->
    %%%%
    %% Power off/on node during activate.
    %%%%
    ok = ct:pal("### power off/on!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    timer:sleep(10000),
    wait_for_login(),

    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    ct_netconfc:close_session(?NC_Session), %% Clean up at testserver side.
    swm_test_lib:check_nc_session(?NC_Session),
    timer:sleep(3000),
    swm_test_lib:check_nc_session(?NC_Session),

    ok.

wait_for_login() ->
    wait_for_login(90000).
 wait_for_login(Timeout) when Timeout < 0 ->
    ct:fail("No login prompt within expected time");
wait_for_login(Timeout) ->
    case ct_telnet:expect(console, "login:", 
			  [{timeout,30000}, no_prompt_check]) of
	{ok,_} -> 
	    ok;
	_Other ->
	    ct_telnet:send(console, ""),
	    timer:sleep(5000),
	    wait_for_login(Timeout-5000)
    end.




%%--------------------------------------------------------------------
%%% @doc power cycle .
%%% @end
%%%--------------------------------------------------------------------
synch() ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    [] = rct_rpc:call(rpc_1, os, cmd, ["sync"], 50000, print),
    net_kernel:disconnect(ErlNode),

    ok.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_ntp_synch() ->
    swm_test_lib:wait_for_ntp_synch(rpc_1),
    ok.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
wait_for_trig_value_exist(NC_sess, MeId, CheckVal, Label) ->
    wait_for_trig_value_exist(NC_sess, MeId, CheckVal, Label, 360).

wait_for_trig_value_exist(_NC_sess, _MeId, CheckVal, Label,Timeout) when Timeout < 0 ->
    ct:fail("trig value: ~p , not found within expected time. Label:~p ", 
	    [CheckVal, Label]);

wait_for_trig_value_exist(NC_sess, MeId, CheckVal, Label, Timeout) ->
    Report = swm_test_lib:get_up_report_progress(NC_sess, MeId, Label),
    ct:log("Report: ~p", [Report]),
    
    Info = [X || X <- Report, X == {additionalInfo, [], [CheckVal]}],
    case Info of
	[{additionalInfo,[],[CheckVal]}] ->
	    ct:pal("AdditionalInfo: ~p, expect ~p .", 
		   [Info, CheckVal]),
	    ok;
	[]->
	    timer:sleep(1000),
	    wait_for_trig_value_exist(NC_sess, MeId, CheckVal, Label,
				   Timeout - 1)
    end.


get_latest_up() ->
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UPs:~n~p~n",[UPs]),

    ToLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("ToLabel:~n~p~n",[ToLabel]),
    ToLabel.

sleep() ->
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}).


get_all() ->
    rct_logging:get_all(log1).

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_rob_ug_reboot_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R11A/1
%%% 
%%% @doc == reboot in differents phases during upgrade.  ==
%%% <br/><br/>
%%% @end

-module(swm_rob_ug_reboot_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R11A/1').

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
%%% R2A/2      2014-05-21 etxivri     Created
%%% R2A/3      2014-05-23 etxivri     Changed commit to confirm.
%%% R2A/4      2014-05-23 etxivri     Use no check on action id when remove up.
%%% R2A/5      2014-05-26 etxivri     Use reboot -f to trig fallback.
%%% R2A/6      2014-06-26 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/7      2014-06-26 etxivri     Use function in swm_test_lib
%%% R2A/8      2014-07-09 etxivri     corrected a check after create
%%% R2A/9      2014-07-10 etxivri     Minor correction in ct:pal
%%% R2A/10     2014-07-11 etxivri     Update remove check.
%%% R2A/11     2014-08-11 etxivri     Update due to new behaivour in 
%%%                                   swm reportProgress.
%%% R2A/12      2014-08-29 etxivri    Update to use modify_cxs in swm_test_lib.
%%% R2A/13      2014-09-01 etxivri    Minor update.
%%% R2A/14      2014-09-26 etxivri    Minor update.
%%% R3A/1       2014-10-17 etxivri    Update to be mor robust.
%%% R3A/2       2015-01-13 etxivri    Update to avoid ERROR in ct shell
%%%                                   when node restarts.
%%% R3A/3       2015-01-13 etxivri    More update to avoid error in ct-shell.
%%% R3A/5       2015-02-02 etxivri    Update login check.
%%% R3A/6       2015-02-05 etxivri    Make it more robust. And cleanup.
%%% R3A/7       2015-02-17 etxivri    Make it more robust.
%%% R3A/8       2015-04-14 etxivri    Make it more robust.
%%% R3A/9       2015-05-08 etxivri    Make it more robust.
%%% R3A/10      2015-05-28 etxivri    Remove hard coded sftp server data.
%%% R3A/11      2015-05-29 etxkols    Changed rct_netconf hook format 
%%% R4A/1       2015-05-29 etxivri    Make it more robust.
%%% R4A/2       2015-07-16 etxivri    Update get to label.
%%% R4A/3       2015-09-30 etxmlar    now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/4       2015-10-20 etxivri    Update due to changed behaviour.
%%% R4A/6       2015-10-22 etxivri    Update due to changed behaviour.
%%% R4A/7       2015-11-03 etxivri    Update to be more robust.
%%% R4A/8       2015-11-10 etxivri    Update to be more robust.
%%% R4A/9       2015-12-01 etxivri    Update due to new behaviour.
%%% R5A/1       2015-12-01 etxivri    Update to be more robust.
%%% R5A/2       2016-02-18 etxivri    Update so console is logged.
%%% R6A/1       2016-05-04 etxivri    Update due to changed behaviour.
%%% R6A/2       2016-06-01 etxivri    Update to be more robust in create_reboot.
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% R8A/1       2016-12-29 etxivri    Update to be more robust.
%%% R8A/2       2017-01-16 etxivri    Update to be more robust.
%%% R10A/1      2017-06-27 etxivri    Update to be more robust.
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
	 %% random_reboot/1,
	 create_reboot/1,
	 create_done_reboot/1,
	 prepare_reboot/1,
	 prepare_done_reboot/1,
	 verify_reboot/1,
	 verify_done_reboot/1,
	 activate_reboot_early/1,
	 activate_reboot_late/1,
	 activate_done_reboot/1,
	 confirm_reboot/1,
	 confirm_done_reboot/1,
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
					  ["error,sa_ais_err_timeout",
					   "sync_send_all_state_event"
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
    	    %% Remove created upgrade package.
	    swm_test_lib:check_nc_session(?NC_Session),
	    [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    	    ct:pal("Label:~n~p~n",[Label]),
    	    swm_test_lib:remove_upgrade_package(?NC_Session, Label)
    end,
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [build_to_up,
     create_reboot].

groups() ->
    [{group_1,[],[%%build_to_up,
		  %% modify_cxs,
		  create_reboot,
		  prepare_reboot,
		  verify_reboot,
		  activate_reboot_early,
		  activate_reboot_late,
		  confirm_reboot,
		  remove
		 ]},
     {group_2,[],[%%build_to_up,
		  modify_cxs,
		  create_done_reboot,
		  prepare_done_reboot,
		  verify_done_reboot,
		  activate_done_reboot,
		  confirm_done_reboot,
		  remove
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
    swm_test_lib:modify_existing_cxs_up_xml(ug1).
  
%%%--------------------------------------------------------------------
%%% @doc Start create and then perform a reboot.
%%% @spec create_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create_reboot(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    Timer = get_random_number(2)+15,
    

    net_kernel:disconnect(ErlNode),
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    ct:pal("Sleep: ~p sec before reboot", [Timer]),
    ct:sleep({seconds, Timer}),
    reboot(ErlNode),

    Label = get_latest_up(),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "INITIALIZED"),


    %% %% Remove and create to make next TCs more robust.
    %% [Label | _] = lists:reverse(swm_test_lib:get_ups(?NC_Session)),
    %% ct:pal("Label:~n~p~n",[Label]),
    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove and create to make next TCs more robust: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, 
					"SUCCESS", 
					MeId, 
					Label),

    swm_test_lib:ug_create_match_result(?NC_Session,
    					"SUCCESS",
    					?SftpHost,
    					?SftpUser,
    					?SftpPassword,
    					UGPath,
    					MeId),
    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Perform create and then perform a reboot.
%%% @spec create_done_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create_done_reboot(_Config) ->
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
	swm_test_lib:wait_for_swm_progress_result(?NC_Session, 
						  MeId, 
						  ActionId),

    ct:pal("result:~p~n"
	   "actionName:~p~n"
	   "progressInfo:~p~n"
	   "resultInfo:~p~n",[Result, 
			      ActionName, 
			      ProgressInfo, 
			      ResultInfo]),

    Label = get_latest_up(),

    synch(),
    reboot(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "INITIALIZED"),
    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove and create to make next TCs more robust: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session, 
					"SUCCESS", 
					MeId, 
					Label),

    swm_test_lib:ug_create_match_result(?NC_Session,
    					"SUCCESS",
    					?SftpHost,
    					?SftpUser,
    					?SftpPassword,
    					UGPath,
    					MeId),

    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start prepare and then perform a reboot.
%%% @spec prepare_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare_reboot(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    RandomProcentage = get_random_procentage(80),
    ct:pal("TC will wait for procentage : ~p, is reached, ~n"
	   "then reboot will be performed.", [RandomProcentage]),

    net_kernel:disconnect(ErlNode),
    %%%%
    %% Prepares an upgrade package, 
    %%%%
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					prepare, 
    					Label, 
    					MeId),

    %%%%
    %% Wait for prepare procentage is reached,  
    %% Then reboot.
    %%%%
    wait_for_percentage_to_be_runed(?NC_Session, 
				    MeId, 
				    RandomProcentage, 
				    Label),
    reboot(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    %% swm_test_lib:wait_for_exp_up_state(?NC_Session, 
    %% 				       MeId, 
    %% 				       Label, 
    %% 				       "INITIALIZED"),

    Res = swm_test_lib:wait_for_progress_result(?NC_Session, 
						MeId, 
						no_check),
    ct:pal("Res :~p",[Res]),
    
    timer:sleep(5000),

    case swm_test_lib:get_up_state(?NC_Session,
				   MeId,
				   Label) of
	"INITIALIZED" ->
	    ct:pal("UP State is INITIALIZED.~n"
		   " Start prepare for next TC verify."),
	    prepare(MeId, Label, ErlNode, no_check),
	    ok;
	"PREPARE_COMPLETED" ->
	    ct:pal("UP State is PREPARE_COMPLETED"),
	    ok;
	_Other ->
	    cp:log("TC will fail due to unexpected UP state: ~p", [_Other]),
	    ct:fail("TC will fail due to unexpected UP state")
    end,

    sleep(),
    ok.






%%%--------------------------------------------------------------------
%%% @doc Perform prepare and then perform a reboot.
%%% @spec prepare_done_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare_done_reboot(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
    
    %% ActionId = no_check,

    %%%%
    %% Create UG
    %%%%
    net_kernel:disconnect(ErlNode),
    prepare(MeId, Label, ErlNode, no_check),

    %%%%
    %% Then reboot.
    %%%%
    reboot(ErlNode),

    %% swm_test_lib:wait_for_exp_up_state(?NC_Session, 
    %% 				       MeId, 
    %% 				       Label, 
    %% 				       "PREPARE_COMPLETED"),
    ct:pal("Wait for up state after reboot.", []),
    case swm_test_lib:get_up_state(?NC_Session,
				   MeId,
				   Label) of
	"INITIALIZED" ->
	    ct:pal("UP State is INITIALIZED.~n"
		   " Start prepare for next TC verify."),
	    prepare(MeId, Label, ErlNode, no_check),
	    ok;
	"PREPARE_COMPLETED" ->
	    ct:pal("UP State is PREPARE_COMPLETED"),
	    ok;
	_Other ->
	    cp:log("TC will fail due to unexpected UP state: ~p", [_Other]),
	    ct:fail("TC will fail due to unexpected UP state")
    end,

    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start verify and then perform a reboot.
%%% @spec verify_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify_reboot(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    %% %%%%
    %% %% Prepares an upgrade package, 
    %% %%%%
    %% net_kernel:disconnect(ErlNode),
    %% prepare(MeId, Label, ErlNode, no_check),

    %%%%
    %% verify. 
    %%%%
    ct:pal("verify.", []),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					verify, 
    					Label, 
    					MeId),

    %%%%
    %% reboot.
    %%%%
    reboot(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "PREPARE_COMPLETED"),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Perform verify and then perform a reboot.
%%% @spec verify_done_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify_done_reboot(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    %%%%
    %% verify. 
    %%%%
    net_kernel:disconnect(ErlNode),
    ct:pal("verify.", []),
    verify(MeId, Label, ErlNode, no_check),
    synch(),

    %%%%
    %% reboot.
    %%%%
    reboot(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "PREPARE_COMPLETED"),

    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start activate and then perform a reboot in early phase.
%%% @spec activate_reboot_early(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_reboot_early(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    RandomProcentage = get_random_procentage(22),
    ct:pal("TC will wait for procentage : ~p, is reached, ~n"
	   "then reboot will be performed.", [RandomProcentage]),
    %%%%
    %% activate an upgrade package, 
    %%%%
    net_kernel:disconnect(ErlNode),
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %%%%
    %% Wait for prepare procentage is reached,  
    %% Then reboot.
    %%%%
    wait_for_percentage_to_be_runed(?NC_Session, 
				    MeId, 
				    RandomProcentage, 
				    Label),
    reboot(ErlNode),

    ct:pal("Wait for up state after reboot.", []),
    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "PREPARE_COMPLETED"),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start activate and then perform a reboot in late phase.
%%%      After expected restart.
%%% @spec activate_reboot_late(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_reboot_late(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    RandomNr = get_random_number(3),
    net_kernel:disconnect(ErlNode),

    %%%%
    %% activate an upgrade package, 
    %%%%
    ct:pal("## Activate first time.", []),
    ActionId = no_check,
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),

    %%%%
    %% check node restarts
    %%%%
    timer:sleep(5000),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
			      [{total_timeout,300000},
			       {idle_timeout,300000}, no_prompt_check]), 
    {ok,_} = ct_telnet:expect(console, "login:", 
			      [{timeout,60000}, no_prompt_check]),
    ct:pal("Login prompt received. reboot node soon!"),

    %%%%
    %% sleep random nr
    %% Then reboot.
    %%%%
    ct:sleep({seconds, RandomNr}),
    reboot(ErlNode),
    %% ct:pal("Special Reboot!"),
    %% ok = rct_rs232:login(console),
    %% %% ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board"),
    %% %% %% Above cmd dos not work in this state due to no such file.
    %% ok = ct_telnet:send(console, "/sbin/pgh_restartbrd 0"),
    %% net_kernel:disconnect(ErlNode),
    %% {ok,_} = ct_telnet:expect(console, "Restarting system",
    %% 			      [{total_timeout,90000}, 
    %% 			       {idle_timeout,90000}, no_prompt_check]), 
    %% {ok,_} = ct_telnet:expect(console, "login:", 
    %% 			      [{timeout,60000}, no_prompt_check]),
    ct_netconfc:close_session(?NC_Session), %% Clean up at testserver side.
    swm_test_lib:check_nc_session(?NC_Session),
    timer:sleep(30000),
    swm_test_lib:check_nc_session(?NC_Session),



    ct:pal("Wait for progress after reboot.", []),
    wait_for_exp_result(MeId, 
			ActionId, 
			ErlNode, 
			"FAILURE",
			"activate",
			"The action was interrupted by a restart"),

    timer:sleep(10000),

    %%%%
    %% activate,  To set correct state
    %%%%
    ct:pal("### Activate again to set correct state for next TC.",[]),
    activate(MeId, Label, ErlNode, no_check),
    ct:pal("### Activate done successful.",[]),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Perform activate and then perform a reboot.
%%% @spec activate_done_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate_done_reboot(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),

    net_kernel:disconnect(ErlNode),

    ActionId = no_check,
    %%%%
    %% activate, 
    %%%%
    activate(MeId, Label, ErlNode, no_check),
    synch(),
    timer:sleep(30000),

    %%%%
    %% Then reboot.
    %%%%
    reboot(ErlNode),

    ct:pal("Wait for progress after reboot.", []),
    wait_for_exp_result(MeId, 
			ActionId, 
			ErlNode, 
			"FAILURE",
			"activate",
			"The action was interrupted by a restart"),

    activate(MeId, Label, ErlNode, no_check), %% Activate again to clean.
    ct:pal("### Activate done successful.",[]),

    sleep(),
    ok.

%%%--------------------------------------------------------------------
%%% @doc Start confirm and then perform a reboot.
%%% @spec confirm_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm_reboot(_Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),

    Label = get_latest_up(),
	     
    net_kernel:disconnect(ErlNode),

    RandomNr = get_random_number(5),
    
    %%%%
    %% confirm
    %%%%	    
    %% ActionId = no_check,
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					confirm, 
    					Label, 
    					MeId),

    %%%%
    %% sleep random nr.
    %% Then reboot.
    %%%%
    ct:sleep({seconds, RandomNr}),
    reboot(ErlNode),

    ct:sleep({seconds, 60}),
    ct:pal("Check for up state after reboot.", []),
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
%%% @doc Perform confirm and then perform a reboot.
%%% @spec confirm_done_reboot(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm_done_reboot(_Config) ->
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
    confirm(MeId, Label),

    synch(),

    %%%%
    %% Then reboot.
    %%%%
    reboot(ErlNode),

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
    ct:pal("remove.",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(),

    swm_test_lib:remove_upgrade_package(?NC_Session, Label),

    timer:sleep(30000),

    %%%% Check swm report
    Report = swm_test_lib:get_swm_report(?NC_Session, MeId),
    %%%%
    %% Check exp fail result and string is shown in report.
    %%%%
  
    [ {state,[], ["FINISHED"] }] =  
	[A || {state, _ , _} = A <- Report],
    
    case [B || {result, _ , _} = B <- Report] of
	[ {result,[], ["SUCCESS"] }] ->
	    ok;
	_FailRes ->
	    case check_remove(Report) of
		ok ->
		    ok;
		Other ->
		    ct:pal("FailRes: ~p", [Other]),
		    ct:fail("Remove fail")
	    end
    end,

    ok.

check_remove(Report) ->
    ResultInfo_1 = "This upgrade package contains active software. "
	"It cannot be removed at this time.",
    ResultInfo_2 = "This upgrade package is referred by a backup. "
	"It cannot be removed at this time.",
    [ {resultInfo,[], [ExpStr] }] = [B || {resultInfo, _ , _} = B <- Report],
    case ExpStr of
	ResultInfo_1 ->
	    ok;  %% don't care.
	ResultInfo_2 ->
	    ok;  %% don't care.
	_Other ->	
	    _Other
    end.

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
    	    ct:pal("# Tc will fail: ~p",[_Res]),
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
    	    ct:pal("## Tc will fail: ~p",[_Res]),
    	    ct:fail(_Res)
    end.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

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
activate(MeId, Label, ErlNode, ActionId) ->
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					activate, 
    					Label, 
    					MeId),
    ct_netconfc:close_session(?NC_Session),
    ct_telnet:expect(console, 
		     "Ericsson", 
		     [{idle_timeout,300000}, 
		      no_prompt_check]),
    swm_test_lib:check_nc_session(?NC_Session),
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
    ok = swm_test_lib:up_action_generic(?NC_Session, 
    					confirm, 
    					Label, 
    					MeId),

    swm_test_lib:wait_for_exp_up_state(?NC_Session, 
				       MeId, 
				       Label, 
				       "COMMIT_COMPLETED"),

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
    %%progress procentage, before reboot.
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
%%% @doc reboot .
%%% @end
%%%--------------------------------------------------------------------
reboot(ErlNode) ->
    %%%%
    %% Reboot off/on node during activate.
    %%%%
    ok = ct:pal("### reboot!",[]),
    ok = rct_rs232:login(console),
    %% ok = ct_telnet:send(console, "reboot"),
    ok = ct_telnet:send(console, "cup --reboot"),
    %% ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board "),
    net_kernel:disconnect(ErlNode),
    {ok,_} = ct_telnet:expect(console, "Ericsson Version:",
    			      [{total_timeout,90000}, 
    			       {idle_timeout,90000}, no_prompt_check]), 
    wait_for_login(),

    %%%%
    %% check node restarts
    %%%%
    swm_test_lib:wait_for_node_state(ErlNode, up),
    ct_netconfc:close_session(?NC_Session), %% Clean up at testserver side.
    swm_test_lib:check_nc_session(?NC_Session),
    timer:sleep(30000),
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

%%%--------------------------------------------------------------------
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


get_latest_up() ->
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UPs:~n~p~n",[UPs]),

    ToLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("ToLabel:~n~p~n",[ToLabel]),
    ToLabel.

%%%--------------------------------------------------------------------
%%% Description: 
%%%--------------------------------------------------------------------
sleep() ->
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}).

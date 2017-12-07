%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_backup_restore_SUITE.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R6A/R8A/R9A/R11A/R12A/1
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end

-module(swm_backup_restore_SUITE).
-vsn('/main/R6A/R8A/R9A/R11A/R12A/1').
-author('etxjotj').

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
%%% R6A/1      2016-09-07 etxjotj     Created
%%% R8A/1      2017-01-10 etxberb     Replaced check_action_capable("CAPABLE")
%%%                                   with wait_for_action_capable().
%%% R9A/2      2017-02-20 etxjotj     Added debug printouts
%%%                                   Cleanup after test suite
%%% R9A/3      2017-02-22 etxjotj     Bugfixes when not attached to console
%%%                                   Disable restore_backup_timeout for now
%%% R9A/4      2017-02-22 etxjotj     Readded restore_backup-timeout
%%% R11A/1     2017-08-08 etxjotj     Added fast_restore_backup
%%% R11A/2     2017-08-11 etxjotj     Fast backup as a regular test
%%%                                   Extended timeout
%%% R11A/3     2017-10-10 etxjotj     Use AVC for certain ases
%%% R11A/4     2017-10-11 etxjotj     Use polling for certain cases
%%% R11A/5     2017-10-12 etxjotj     Unused functions fixed
%%%--------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0]).

%% Official use cases
-export([restore_backup/1,                    %% NodeUC442.N
	 fast_restore_backup/1,               %% NodeUC442.N
	 restore_backup_fail/1,               %% NodeUC442.E1
	 restore_backup_timeout/1,            %% NodeUC442.E2
	 restore_backup_timeout_poweroff/1,   %% NodeUC442.E2
	 restore_backup_cancel_pre_reboot/1,  %% NodeUC442.A1
	 restore_backup_cancel_post_reboot/1, %% NodeUC442.A1
	 restore_backup_autoconfirm/1         %% NodeUC442.A3
	]).

%% Good to have use cases
-export([clean_backups/1]).
-export([tc/1]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [restore_backup,
     fast_restore_backup,
     restore_backup_fail,
     restore_backup_timeout,
     restore_backup_timeout_poweroff,
     restore_backup_cancel_pre_reboot,
     restore_backup_cancel_post_reboot,
     restore_backup_autoconfirm
    ].

-define(DataDir, "/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/SWM/SWM_CNX9012637/test/suites").
%% -define(DataDir, "/vobs/rcs/test/RCT_CRX901275/test/suites/SWM/swm_backup_SUITE_data").

is_console() ->
    case init:get_argument(console) of
	{ok, [["true"]]} ->
	    true;
	{ok, [["false"]]} ->
	    false;
	error -> %% Flag is not present
	    false
    end.

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    [{timetrap, {minutes, 20}},
	     {ct_hooks, 
	      case is_console() of
		  true ->[];
		  false ->  [{rct_rs232, console}]
	      end++
		  [{rct_htmllink,[]},
		   {rct_rpc, rpc_1},
		   {rct_netconf,{nc1, html}},
		   {rct_netconf,{nc_event, html}},
		   {cth_conn_log,[]},
		   {rct_power, power1},
		   {rct_core,[]},
		   {rct_logging,
		    {all, [{[erlang,swmLog],
			    {["ERROR REPORT","CRASH REPORT"],[]}}]}}
		  ]}];
	_ -> 
	    [{timetrap, {minutes, 20}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_netconf,{nc1, html}},
			 {rct_netconf,{nc_event, html}},
			 {cth_conn_log,[]},
			 {rct_core,[]},
			 {rct_logging,
			  {all, [{[erlang,swmLog],
				  {["ERROR REPORT","CRASH REPORT"],[]}}]}}
			]}]
    end.

%% @hidden
init_per_suite(Config) ->
    ct:print("Init per suite"),
    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),
    ct:print("init_per_suite: RootDir = ~p",[RootDir]),
    MeData = case call(comsaI, get_managed_element_data, []) of
		 {badrpc, nodedown} -> ct:fail(node_is_not_responding);
		 RpcReply -> RpcReply
	     end,
    ct:print("init_per_suite: MeData = ~p~n",[MeData]),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    Name = atom_to_list(?MODULE),
    [{host, Host},{username, Username},{password, Password}] = 
        ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    NewConfig = [{meId, MeId},
		 {backupName, Name},
		 {unique, unique_name()},
		 {sftp_host, Host},
		 {sftp_user, Username},
		 {sftp_pass, Password},
		 {sftp_root, RootDir} | Config],
 
    call(swmLib, set_variable, [confirm_restore, true]),

   ct:print("init_per_suite: NewConfig = ~p~n",[NewConfig]),
    %% call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
    %% call(swmServer, start_test_event_server, []),
%    ct:print("init_per_suite: Clean backups"),
%    clean_backups(NewConfig),
    %% call(swmServer, stop_test_event_server, []),
    ct:print("Init per suite complete"),
    NewConfig.

%% @hidden
end_per_suite(Config) ->
    try clean_backups(Config)
    after
	ok
    end,
    ok.

%% @hidden
   
init_per_testcase(TestCase, Config) ->
    ct:print("Now executing ~w",[TestCase]),
    MeId = proplists:get_value(meId, Config),
    ct:pal("MeId is ~p",[MeId]),
    set_housekeeping_default(MeId),
    set_rollback_timeout(MeId, 3600),
    %% ct:print("Config = ~p",[Config]),
    %% call(swmLib, set_variable, [swm_basic_tests_return_pid, self()]),
    %% call(swmServer, start_test_event_server, []),
    Config.
%% @hidden
end_per_testcase(TestCase, Config) ->
    MeId = proplists:get_value(meId, Config),
    set_rollback_timeout(MeId, 3600),
    ct:print("Test case ~w complete.",[TestCase]),
    call(swmServer, stop_test_event_server, []),
    call(swmLib, erase_variable, [swm_basic_tests_return_pid]),
    swm_event_lib:stop_subscription(),
    ok.


%% @hidden
groups() ->
    AllGroup = all(),
    [{default__group, [], AllGroup},
     {cover__group, [], [ ]},
     {nocover__group, [], [create_backup,
			   create_backup,
			   clean_backups,
			   restore_backup,
			   restore_backup_interrupted]},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__def__all__1__group, [], [{group, default__group}]}, 
     {sdc__cover__sim__1__group, [], [{group, cover__group}]}, 
     {sdc__nocover__sim__1__group, [], [{group, nocover__group}]},
     {sdc__qual__all__1__group, [], []} 
    ].


tc(_Config) ->
    ct:pal("tjohoo!~n"),
    ok.



%%--------------------------------------------------------------------
%% @doc NodeUC442.N Restore backup - Manual restoration
%% @end

restore_backup(Config) ->

    {ok, ActionId, BuId} = restore_prepare(Config, normal),
    MeId = proplists:get_value(meId, Config),
    
    wait_for_nodeup(MeId, ActionId, BuId),
    swm_event_lib:start_subscription(nc_event, []),
    case action(confirmRestore, MeId) of
	{ok, "0"} ->
	    ok;
	{ok, ReturnValue} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    wait_for_success_e(Config, ActionId, BuId).

%%--------------------------------------------------------------------
%% @doc NodeUC442.N Restore backup - Fast restore case
%% @end

fast_restore_backup(Config) ->
    
    {ok, ActionId, BuId} = restore_prepare(Config, fast),
    MeId = proplists:get_value(meId, Config),
    
    wait_for_nodeup(MeId, ActionId, BuId),
    swm_event_lib:start_subscription(nc_event, []),

    case action(confirmRestore, MeId) of
	{ok, "0"} ->
	    ok;
	{ok, ReturnValue} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    wait_for_success_e(Config, ActionId, BuId).

%%--------------------------------------------------------------------
%% @doc NodeUC442.E1 Restore backup - Restoration failure
%% @end

restore_backup_fail(Config) ->
    {ok, ActionId, BuId} = restore_prepare(Config),
    timer:sleep(15000),
    reboot_node(),
    wait_for_failure(Config, ActionId, BuId).

%%--------------------------------------------------------------------
%% @doc NodeUC442.A2 Restoration failure, supervision timer expires
%% @end

restore_backup_timeout(Config) ->
    MeId = proplists:get_value(meId, Config),
    
    set_rollback_timeout(MeId, 240),
    {ok, ActionId, BuId} = restore_prepare(Config),

    wait_for_nodedown(MeId, ActionId, BuId),

    ct:pal("Waiting for confirm timer to expire"),
    
    [begin
    	 ct:print("Waiting for confirm timer to expire, ~w0 secs remaining~n",[X]),
    	 timer:sleep(10000)
     end||X<-lists:seq(30, 1, -1)],

    ct:pal("Read progress info, expect FAILURE~n",[]),

    wait_for_failure(Config, ActionId, BuId).




%%--------------------------------------------------------------------
%% @doc NodeUC442.A2 Restoration failure, timer expires, powerdown
%% The countdown should proceed even if the board is powered off
%% When the deadline is passed, power up the board and verify it 
%% will fail the restore.
%% @end

restore_backup_timeout_poweroff(Config) ->
    MeId = proplists:get_value(meId, Config),
    
    set_rollback_timeout(MeId, 180),
    {ok, ActionId, BuId} = restore_prepare(Config),

    wait_for_erl_nodeup(),
    
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    ct:pal("Powering off"),
    rct_power:off(power1),

    [begin
	 ct:print("Waiting for timeout period to elapse (~w)",[X]),
	 timer:sleep(10000)
     end||X<-lists:seq(20, 1, -1)],

    ct:pal("Powering on"),
    
    rct_power:on(power1),

    [begin
	 ct:print("Waiting for confirm timeout (~w)",[X]),
	 timer:sleep(10000)
     end||X<-lists:seq(20, 1, -1)],
    ct:pal("Confirm timeout should have happened, "
	   "and node should be rebooting~n",[]),

    wait_for_failure(Config, ActionId, BuId).


wait_for_erl_nodeup() ->
    wait_for_erl_nodeup(0).

wait_for_erl_nodeup(60) ->
    ct:fail(wait_for_erl_nodeup_timeout);   
wait_for_erl_nodeup(X) ->
    ct:print("Waiting for beam nodeup (~w)",[X]),
    timer:sleep(10000),
    case call(erlang, now,[]) of
	{badrpc, nodedown} ->
	    ct:print("beam is not responding",[]),
	    wait_for_erl_nodeup(X+1);
	R when size(R) == 3 -> 
	    ct:print("beam is responding");
	R ->
	    ct:pal("Unknown response R=~p",[R])
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC442.A1 Restore backup - Pre reboot cancel
%% @end

restore_backup_cancel_pre_reboot(Config) ->
    MeId = proplists:get_value(meId, Config),
    {ok, ActionId, BuId} = restore_prepare(Config),
    ct:pal("Cancelling"),
    action(cancelCurrentAction, MeId, BuId),

    ProgressFilter = progress_filter(MeId, BuId),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    	cancelled ->
	    case get_progress_report_member(result, ProgressFilter) of
		"FAILURE" -> ok;
		Result -> ct:pal("Progress result = ~p (FAILURE expected)",
				 [Result]),
			  ct:fail(wrong_result_from_cancel)
	    end,
    	    %% ok = swmTestLib:wait_for_action_capable(),
    	    ok;
    	Result ->
    	    ct:pal("restoreBackup: ~s",[Result]),
    	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC442.A1 Restore backup - Post reboot cancel
%% @end

restore_backup_cancel_post_reboot(Config) ->
    MeId = proplists:get_value(meId, Config),
    {ok, ActionId, BuId} = restore_prepare(Config),

    wait_for_nodeup(MeId, ActionId, BuId),

    ct:pal("Cancelling"),
    action(cancelCurrentAction, MeId, BuId),

    wait_for_nodeup(MeId, ActionId, BuId),

    ProgressFilter = progress_filter(MeId, BuId),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    	cancelled ->
	    case get_progress_report_member(result, ProgressFilter) of
		"FAILURE" -> ok;
		Result -> ct:pal("Progress result = ~p (FAILURE expected)",
				 [Result]),
			  ct:fail(wrong_result_from_cancel)
	    end,
    	    %% ok = swmTestLib:wait_for_action_capable(),
    	    ok;
    	Result ->
    	    ct:pal("restoreBackup: ~s",[Result]),
    	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% @doc NodeUC442.A3 Restore backup - autoconfirm
%% @end

restore_backup_autoconfirm(Config) ->
    MeId = proplists:get_value(meId, Config),
    set_rollback_timeout(MeId, 180),

    {ok, ActionId, BuId} = restore_prepare(Config),
    
    wait_for_nodeup(MeId, ActionId, BuId),

    Baseline = get_time_remaining(MeId),
    timer:sleep(5000),
    case get_time_remaining(MeId) of
	Baseline -> 
	    ct:fail("Rollback timer is not counting down");
	Current when is_list(Current) -> ok;
	undefined -> 
	    ct:fail("Rollback timer is unset")
    end,

    wait_for_success(Config, ActionId, BuId).



%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%-------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Set timeAllowedBeforeRollback
%%%--------------------------------------------------------------------

set_rollback_timeout(MeId, Value) when is_integer(Value) ->
    ct:print("Rollback timeout set to ~p~n",[Value]),
    Set = complete_netconf(MeId,
			   {'BrmRollbackAtRestore',
			    [{brmRollbackAtRestoreId, [], ["1"]},
			     {timeAllowedBeforeRollback, [], 
			      [integer_to_list(Value)]}]}),
    ok = netconf(edit_config, [nc1, running, Set]).

%%%--------------------------------------------------------------------
%%% Description: Read timeRemainingBeforeRollback
%%%--------------------------------------------------------------------


get_time_remaining(MeId) ->
    GetC = complete_netconf(MeId, {'BrmRollbackAtRestore',
				   [{brmRollbackAtRestoreId, [], ["1"]},
				    {timeRemainingBeforeRollback, []}]}),
    {ok, GetResult} =  netconf(get, [nc1, GetC]),
    case extract_element(timeRemainingBeforeRollback, GetResult) of
	{ok, {timeRemainingBeforeRollback, _, [UserLabel]}} ->
	    UserLabel;
	{ok, {timeRemainingBeforeRollback, [{unset, "true"}], _}} ->
	    undefined
    end.


%%%--------------------------------------------------------------------
%%% Description: Create a backup and initiate restore. Don't wait for result.
%%%--------------------------------------------------------------------


restore_prepare(Config) ->
    restore_prepare(Config, undefined).

restore_prepare(Config, FastRollback) ->
    ct:print("restore_prepare(~p)~n",[Config]),
    MeId = proplists:get_value(meId, Config),

    %% Set a value in the db, then create a backup to use 

    DefaultUL = get_me_user_label(MeId),
    ct:print("Current user label is ~p~n",[DefaultUL]), 
    TestUL = "BackupTest."++proplists:get_value(unique, Config),
    ct:print("Switching user label to ~p~n",[TestUL]),
    set_me_user_label(MeId, TestUL),
    BuName = "swm_backup_restore_SUITE.restore."++unique_name(),
    LastNewConfig =lists:keyreplace(backupName,1,Config,{backupName,BuName}),
    {ok, MoRef} = create_backup(LastNewConfig),
    ct:print("Create backup is complete ~p",[MoRef]),

    %% Return value to default
    set_me_user_label(MeId, DefaultUL),

    %% Find out the BuId for future actions
    BuId = lists:last(string:tokens(MoRef, "=")),


    case FastRollback of
	fast ->
	    %% Prep the other area
	    ok = rct_rpc:call(rpc_1, swmBackup, install_bootfallback, [BuName], 
			      20*60*1000);
	normal -> 
	    %% Make sure other area is cleared, 
	    %% so that the normal procedure is used
	    call(swmOs, clear, []);
	undefined ->
	    ok
    end,

    %% Do a restore on the backup
    ProgressFilter = progress_filter(MeId, BuId),
    ActionId = get_action_id(ProgressFilter),
    ct:print("ActionId = ~p",[ActionId]),
    ct:pal("Invoking restore~n",[]),
    case action(restore, MeId, BuId) of
	{ok, "0"} -> ok;
	{ok, ReturnValue} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    ct:pal("Restore invoked.~n",[]),
    %% ok = swmTestLib:check_action_capable_progress(
    %% 	   [{"WAIT", swmTestLib:map_current_action("restoreBackup")}]),
    %% Detach from the node while rebooting to prevent error from erlang
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    {ok, ActionId, BuId}.

%%%--------------------------------------------------------------------
%%% Description: Wait for node to come up, confirm backup and wait
%%%--------------------------------------------------------------------

wait_for_success(Config, ActionId, BuId) -> 
    MeId = proplists:get_value(meId, Config),
    
    ProgressFilter = progress_filter(MeId, BuId),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    	["SUCCESS"] ->
	    CurrentUL = get_me_user_label(MeId),
	    case "BackupTest."++proplists:get_value(unique, Config) of
		CurrentUL -> ok;
		ExpectedUL ->
		    ct:pal("Current user label: ~p~nExpected user label: ~p",
			   [CurrentUL, ExpectedUL]),
		    ct:fail(database_mismatch_after_restore)
	    end,
    	    %% ok = swmTestLib:wait_for_action_capable(),
    	    ok;
    	Result ->
    	    ct:pal("restoreBackup: ~s",[Result]),
    	    ct:fail(Result)
    end.

wait_for_success_e(Config, ActionId, BuId) -> 
    MeId = proplists:get_value(meId, Config),
    
    %% ProgressFilter = progress_filter(MeId, BuId),
    case wait_for_progress_e(progressReport, ActionId, {"BrmBackup", BuId}) of
    	{ok, ["SUCCESS"], _} ->
	    CurrentUL = get_me_user_label(MeId),
	    case "BackupTest."++proplists:get_value(unique, Config) of
		CurrentUL -> ok;
		ExpectedUL ->
		    ct:pal("Current user label: ~p~nExpected user label: ~p",
			   [CurrentUL, ExpectedUL]),
		    ct:fail(database_mismatch_after_restore)
	    end,
    	    %% ok = swmTestLib:wait_for_action_capable(),
    	    ok;
    	{ok, Result, _} ->
    	    ct:pal("restoreBackup: ~s",[Result]),
    	    ct:fail(Result)
    end.

%%--------------------------------------------------------------------
%% Description: After restoration failure, wait for node to say "FAILURE"
%%--------------------------------------------------------------------

wait_for_failure(Config, ActionId, BuId) -> 
    MeId = proplists:get_value(meId, Config),
    
    ct:print("Listen on console to detect restart ~n",[]),
    %% Wait for nodeup
    wait_for_console(120),
	   
    %% wait_for_nodeup(MeId, ActionId, BuId),

    %% Polling must be used here since the AVC channel will die
    ProgressFilter = progress_filter(MeId, BuId),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    	["FAILURE"] ->
    	    %% ok = swmTestLib:wait_for_action_capable(),
    	    ok;
    	Result ->
    	    ct:pal("restoreBackup: ~s",[Result]),
    	    ct:fail(Result)
    end.



%%--------------------------------------------------------------------
%% Description: After rebooting, wait for the node come up again
%%--------------------------------------------------------------------

wait_for_nodeup(MeId, ActionId, BuId) ->
    wait_for_console(60),
    ProgressFilter = progress_filter(MeId, BuId),
    wait_for_netconf(progressReport, ActionId, ProgressFilter).

wait_for_nodedown(MeId, ActionId, BuId) ->
    ProgressFilter = progress_filter(MeId, BuId),
    wait_for_netconf_down(progressReport, ActionId, ProgressFilter).



wait_for_console(Time) ->
    ct:print("Wait for console ~n",[]),
    case is_console() of
	true -> 
	    ct:print("Console busy. Waiting ~w seconds ~n",[Time]),
	    timer:sleep(Time*1000),
	    ok;
	false ->
	    case os:getenv("SIM_OR_TARGET") of
		"target" ->
		    %% Wait for node to restart, to prevent error in ct-shell.
		    {ok, _} = ct_telnet:expect(console, 
					       "Ericsson", 
					       [{idle_timeout,240000}, 
						no_prompt_check]);
		_ -> 
		    ok
	    end
    end.



%%--------------------------------------------------------------------
%% Description: Create backup 
%% Using netconf to trig a backup to be created.
%% Initiate the action, and then check the model for the expected instance
%%--------------------------------------------------------------------

create_backup(Config)->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),

    ProgressFilter = progress_filter(MeId, mgr),
    ActionId = get_action_id(ProgressFilter),
    case action(createBackup, MeId, Name) of
	{ok, "0"} ->
	    ok;
	{ok, ReturnValue} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)",[Error, ReturnValue]),
	    ct:fail(Error);
	{error, ErrorMessage} ->
	    Error = decode_error_message(ErrorMessage),
	    ct:print("Action failure",[]),
	    ct:log("~s",[ErrorMessage]),
	    ct:fail(Error)
    end,

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ct:pal("Backup succesfully created."),
	    ResultInfo = get_result_info(ProgressFilter),
	    ct:pal("ResultInfo ~p",[ResultInfo]),
	    {ok, ResultInfo};
	Result ->
	    ct:pal("createBackup: ~s",[Result]),
	    ct:fail(Result)
    end.



%%--------------------------------------------------------------------
%% Description: Delete backup (Same as NodeUC443.N)
%% Using netconf to trig a backup to be removed
%% Initiate the action and wait for it to be deleted. Create a backup
%% if no one exists.
%%--------------------------------------------------------------------

delete_backup(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Backups = get_all_backups(MeId),
    Name =
	case proplists:get_value(force_backup_name, Config, false) of
	    true ->
		proplists:get_value(backupName, Config);
	    false ->
		BName = "swm_backup_restore_SUITE.delete_backup."++unique_name(),
		NewConfig =
		    lists:keyreplace(backupName, 1, Config,
				     {backupName, BName}),
		{ok, _} = create_backup(NewConfig),
		BName
	end,
    ct:pal("Selected backup ~p for deletion.",[Name]),

    ProgressFilter = progress_filter(MeId, mgr),

    ActionId = get_action_id(ProgressFilter),

    case action(deleteBackup, MeId,  Name) of
	{ok, "0"} -> ok;
	{ok, ReturnValue} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)",[Error, ReturnValue]);%% ,
	    %% ct:fail(Error);
	{error, ErrorMessage} ->
	    Error = decode_error_message(ErrorMessage),
	    ct:log("Action invoke failure: ~w ~s",[Error, ErrorMessage])%,
	    %% ct:fail(Error)
    end,

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ct:pal("Backup deleted successfully"),
	    %% if called from clean backups, don't check
	    case proplists:get_value(force_backup_name, Config, false) of
		false ->
		    %% ok = swmTestLib:check_action_capable_progress([{"WAIT", swmTestLib:map_current_action("deleteBackup")},
		    %% 						   {"CAPABLE", ""}]),
		    %% ok = swmTestLib:wait_for_action_capable();
		    ok;
		_ ->
		    ok
	    end;
	Result ->
	    ct:pal("deleteBackup: ~s",[Result])%% ,
	    %% ct:fail(Result)
    end.


%%--------------------------------------------------------------------
%% Description: Removes all removable backups in the system
%% Using netconf to trig all backups to be removed.
%%--------------------------------------------------------------------


clean_backups(Config)->
    MeId = proplists:get_value(meId, Config),
    Backups = get_all_backups(MeId),

    [begin
	 {ok, {_, _, [Name]}} = extract_element(backupName, [Backup]),
	 case extract_element(creationType, [Backup]) of
	     {ok, {_, _, ["SYSTEM_CREATED"]}} ->
		 ok;
	     _ ->
		 NewConfig = lists:keyreplace(backupName,1, Config,
					      {backupName, Name}),
		 delete_backup([{force_backup_name, true}|NewConfig])
	 end
     end||Backup<-Backups],
    ok.

%%%--------------------------------------------------------------------
%%% Description: reboot_node()
%%%--------------------------------------------------------------------

reboot_node() ->
    spawn(fun do_reboot_node/0).

do_reboot_node() ->
    timer:sleep(5000),
    ct:pal("Rebooting..."),
    %% Detach from the node while booting to prevent error from erlang
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc_1),
    net_kernel:disconnect(ErlNode),
    
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    rct_power:cycle(power1),
	    ct:pal("Power cycle ordered");
	_ ->
	    call(erlang, halt, []),
	    ct:pal("erlang:halt() ordered")
    end.



%%%--------------------------------------------------------------------
%%% Description: Action execution
%%%--------------------------------------------------------------------

action(restore, MeId, BuId) ->
    Action = complete_netconf(MeId, 
			      {'BrmBackup',
			       [{brmBackupId, [], [BuId]},
				{restore, []}]}),
    ct:print("Executing action restore on backup ~p",[BuId]),
    do_action_generic(Action);

action(cancelCurrentAction, MeId, BuId) ->
    Action = complete_netconf(MeId, 
			      {'BrmBackup',
			       [{brmBackupId, [], [BuId]},
				{cancelCurrentAction, []}]}),
    ct:print("Executing action cancelCurrentAction on backup ~p",[BuId]),
    do_action_generic(Action);

action(createBackup, MeId, Name) ->
    Action = complete_netconf(MeId, {'BrmBackupManager',
				     [{brmBackupManagerId,[],["1"]},
				      {'createBackup',[],
				       [{name, [], [Name]}]}]}),
    ct:print("Execution action createBackup with name ~p",[Name]),
    do_action_generic(Action);

action(deleteBackup, MeId, Name) ->
    Action = complete_netconf(MeId, 
			      {'BrmBackupManager',
			       [{brmBackupManagerId,[],["1"]},
				{'deleteBackup',[],
				 [{name, [], [Name]}]}]}),
    ct:print("Executing action deleteBackup on backup ~p",[Name]),
    do_action_generic(Action).

action(confirmRestore, MeId) ->
    Action = complete_netconf(MeId, 
			      {'BrmRollbackAtRestore',
			       [{brmRollbackAtRestoreId,[],["1"]},
				{confirmRestore,[]}]}),
    ct:print("Executing action confirmRestore ",[]),
    do_action_generic(Action).


do_action_generic(Action) ->
    %% ok = swmTestLib:wait_for_action_capable(),
    %% swmTestLib:flush_mnesia_event_messages(),
    case netconf(action, [nc1, Action]) of
	{ok, ActionResponse} ->
	    {ok, {returnValue, _, [ReturnValue]}} =
		extract_element(returnValue, ActionResponse),
	    {ok, ReturnValue};
	{error, ErrorResponse} ->
	    {ok, {'error-message', _, [ErrorMessage]}} =
		extract_element('error-message', ErrorResponse),
	    {error, ErrorMessage}
    end.


%%%--------------------------------------------------------------------
%%% Description: Set Managed element user label
%%%--------------------------------------------------------------------


set_me_user_label(MeId, undefined) ->
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	    {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	   [{managedElementId,[],[MeId]},
	    {userLabel, [{'xc:operation',"delete"}], []}]},
    ok = netconf(edit_config, [nc1, running, Set]);
 
set_me_user_label(MeId, UserLabel) ->
    Set = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {userLabel, [], [UserLabel]}]},
    ok = netconf(edit_config, [nc1, running, Set]).


%%%--------------------------------------------------------------------
%%% Description: Get managed element user label
%%%--------------------------------------------------------------------

get_me_user_label(MeId) ->
    GetC = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {userLabel, []}]},
    {ok, GetResult} = netconf(get_config, [nc1, running, GetC]),
    case extract_element(userLabel, GetResult) of
	{ok, {userLabel, _, [UserLabel]}} ->
	    UserLabel;
	{ok, {userLabel, [{unset, "true"}], _}} ->
	    undefined
    end.

%%%--------------------------------------------------------------------
%%% Description: Generate progress filters
%%%--------------------------------------------------------------------

progress_filter(MeId, mgr) ->
    complete_netconf(MeId, 
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {progressReport, []}]});
progress_filter(MeId, BuId) ->
    complete_netconf(MeId, {'BrmBackup',
			    [{brmBackupId, [], [BuId]},
			     {progressReport, []}]}).


%%%--------------------------------------------------------------------
%%% Description: Shorthand for NETCONF commands
%%%--------------------------------------------------------------------

complete_netconf(MeId, Content) when element(1, Content) == 'BrM' ->
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	Content]}]};
complete_netconf(MeId, Content) 
  when element(1, Content) == 'BrmBackupManager';
       element(1, Content) == 'BrmRollbackAtRestore' ->
    complete_netconf(MeId, {'BrM', [{brMId, [], ["1"]}, Content]});
complete_netconf(MeId, Content) 
  when element(1, Content) == 'BrmBackup' ;
       element(1, Content) == 'BrmBackupHousekeeping' ;
       element(1, Content) == 'BrmBackupLabelStore' ;
       element(1, Content) == 'BrmBackupScheduler' ; 
       element(1, Content) == 'BrmFailsafeBackup' ->
    complete_netconf(MeId, {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]}, Content]}).

    


%%%--------------------------------------------------------------------
%%% Description: Update manual housekeeping MO
%%%--------------------------------------------------------------------

set_housekeeping(MeId, MaxNumberOfBackups, Autodelete) ->
    Set = complete_netconf(MeId, 
			   {'BrmBackupHousekeeping',
			    [{brmBackupHousekeepingId, [], ["1"]},
			     {maxStoredManualBackups, [],
			      [integer_to_list(MaxNumberOfBackups)]} ,
			     {autoDelete, [], [Autodelete]}]}),

    ok = netconf(edit_config, [nc1, running, Set]).

set_housekeeping_default(MeId) ->
    ct:print("Default housekeeping"),
    set_housekeeping(MeId, 5, "ENABLED").


%%%--------------------------------------------------------------------
%%% Description: Convert an backup action error code to a meaningful value
%%%--------------------------------------------------------------------

decode_return_value("0") -> actionStarted;
decode_return_value("1") -> nameValidationFailure;
decode_return_value("2") -> duplicateName;
decode_return_value("3") -> housekeepingRequired;
decode_return_value("4") -> backupNotFound;
decode_return_value("98") -> missingParameter;
decode_return_value("99") -> softwareFault.

%%%--------------------------------------------------------------------
%%% Description: Convert an backup action error message to a matchable atom
%%%--------------------------------------------------------------------

decode_error_message(Msg) ->
    Codes = 
	[{"Name validation failure", nameValidationFailure},
	 {"Duplicate name", duplicateName},
	 {"Housekeeping required", housekeepingRequired},
	 {"Backup not found", backupNotFound},
	 {"Function busy", functionBusy},
	 {"Missing parameter", missingParameter},
	 {"Software fault", softwareFault}],
    [Result] = 
	[Code||{Pattern, Code}<-Codes,
	       case re:run(Msg, Pattern) of
		   {match, _} ->
		       true;
		   _ ->
		       false
	       end],
    Result.

%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------

%% cmd(Cmd) ->
%%     ct:pal("~s~n~s",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
%%     R.
%%%--------------------------------------------------------------------
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(ProgressFilter) ->
    get_progress_report_member(actionId, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(ProgressFilter) ->
    get_progress_report_member(resultInfo, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, ProgressFilter) ->
    case  netconf(get, [nc1, ProgressFilter]) of
	{ok, A} ->
%%%    ct:pal("ProgressFilter result: ~p", [A]),
	    case extract_element(progressReport, A) of
		{ok, {progressReport, L, _}}  ->
		    case lists:keyfind(unset,1, L) of
			{unset, "true"} ->
			    undefined;
			_ ->
			    extract_member(Member, A)
		    end;
		_ ->
		    extract_member(Member, A)
	    end;
	_ ->
	    ct:fail(netconf_error)
    end.

extract_member(Member, A) ->
    {ok, {Member, _, [Value]}} = 
	extract_element(Member, A),
    Value.


%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	{ok, R} ->
	    ok = ct_netconfc:close_session(nc1),
	    {ok, R};
	{error, Error} ->
	    {error, Error}
    end.


%%%--------------------------------------------------------------------
%%% Description: Make a standardized rpc call
%%%--------------------------------------------------------------------

call(M, F, A) ->
    rct_rpc:call(rpc_1, M, F, A, 10000).

%%%--------------------------------------------------------------------
%%% Description: Wait for netconf to go down, and then return again
%%% while showing progress reporting
%%%--------------------------------------------------------------------

wait_for_netconf(Attribute, OldActionId, ProgressFilter) ->
    wait_for_netconf(going_down, Attribute, OldActionId, ProgressFilter).

wait_for_netconf_down(Attribute, OldActionId, ProgressFilter) ->
    wait_for_netconf(end_at_down, Attribute, OldActionId, ProgressFilter).
    

wait_for_netconf(State, Attribute, OldActionId, ProgressFilter) ->
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    timer:sleep(1000),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    ct:pal("Waiting for updated progress ~s~n~p",
				   [OldActionId, Report]),
			    wait_for_netconf(State, Attribute, OldActionId,
					     ProgressFilter);
			_ ->
			    check_progress_elements(Attribute, Report),
			    case State of
				coming_up ->
				    ok;
				_ ->
				    wait_for_netconf(State, Attribute, 
						     OldActionId, 
						     ProgressFilter)
			    end
		    end;
		{error, Error} ->
		    ct:pal("Netconf get error:~n~p",[Error]),
		    case State of
			coming_up ->
			    ok;
			_ ->
			    wait_for_netconf(State, Attribute, OldActionId, ProgressFilter)
		    end
	    end;
	{error, _} when State == going_down ->
	    ct:pal("Netconf is down",[]),
	    timer:sleep(30000),
	    wait_for_netconf(coming_up, Attribute, OldActionId, ProgressFilter);
	{error, _} when State == end_at_down ->
	    ct:pal("Netconf is down",[]),
	    ok;
	{error, _} when State == coming_up ->
	    ct:print("Netconf is still down",[]),
	    timer:sleep(3000),
	    wait_for_netconf(coming_up, Attribute, OldActionId, ProgressFilter)
    end.


wait_for_progress_e(Attribute, OldActionId, {MOC, Id}) ->
    wait_for_progress_e(Attribute, OldActionId, {MOC, Id}, 60000).
wait_for_progress_e(Attribute, OldActionId, {MOC, Id}, Timeout) ->
    receive
	{avc, _, MOC, Id, [{Attribute, Fields}]} ->
	    %% ct:print("Received: ~n~p~n",[E]),
	    case proplists:get_value(actionId, Fields) of
		[[OldActionId]] ->
		    ct:pal("Waiting for updated progress ~s~n~p~n",
			   [OldActionId, Fields]),
		    wait_for_progress_e(Attribute, OldActionId, {MOC, Id});
		_ ->
		    case check_progress_elements_e(Fields) of
			loop ->
			    wait_for_progress_e(Attribute,OldActionId,{MOC,Id});
			{ok, Result, Fields} ->
			    {ok, Result, Fields}
		    end
	    end
    after Timeout ->
	    ct:print("Messages at timeout ~n~p~n",
		     [process_info(self(), messages)]),
	    ct:fail(event_timeout)
    end.

check_progress_elements_e([]) -> loop;
check_progress_elements_e(Fields) -> 
    [[State]] = proplists:get_value(state, Fields),
    case State of
	"FINISHED" ->
	    ct:pal(format_fields(Fields), []),
	    [Result] = proplists:get_value(result, Fields),
	    {ok, Result, Fields};
	"CANCELLED" ->
	    ct:pal(format_fields(Fields),[]),
	    [Result] = proplists:get_value(result, Fields),
	    {ok, Result, Fields};
	Current ->
	    [[Percent]] = proplists:get_value(progressPercentage, Fields),
	    [[Info]] = proplists:get_value(progressInfo, Fields),
	    [[ActionName]] = proplists:get_value(actionName, Fields),
	    ct:pal("Action: ~s~nState: ~s ~s % ready~n~s",
		   [ActionName, Current, Percent, Info]),
	    loop
    end.

format_fields(Fields) ->
    [case length(Values) of
	 1 ->
	     [io_lib:format("~w: ~s~n",[Field, hd(Values)])];
	 _ ->
	     [io_lib:format("~w:~n",[Field])|
	      [io_lib:format("  ~s~n",[hd(Value)])||Value<-Values]]
     end||{Field, Values}<-Fields].


%%%--------------------------------------------------------------------
%%% Description: This function and the associated check_progress_elements
%%%              loops over a netconf get for the progress information until
%%%              the state is FINISHED, otherwise it prints the status and
%%%              progress percentage
%%%--------------------------------------------------------------------


wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(1000),
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    ct:pal("Waiting for updated progress ~s~n~p",
				   [OldActionId, Report]),
			    wait_for_progress(Attribute, OldActionId,
					      ProgressFilter);
			_ ->
			    case check_progress_elements(Attribute, Report) of
				loop ->
				    wait_for_progress(Attribute, OldActionId,
						      ProgressFilter);
				{ok, Result} ->
				    Result
			    end
		    end;
		{error, Error} ->
		    ct:pal("Netconf get error:~n~p",[Error]),
		    wait_for_progress(Attribute, OldActionId, ProgressFilter)
	    end;
	{error, Reason} ->
	    ct:log("Netconf open error:~n~p",[Reason]),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    case Report of
	{progressReport, [{unset, "true"}], []} ->
	    loop;
	_ ->
	    {ok, State} = extract_element(state, [Report]),
	    case State of
		{state, _, ["FINISHED"]} ->
		    format_progress_report(Report),
		    ct:pal("~s",[format_progress_report(Report)]),
		    {ok, {result, _, Result}} =
			extract_element(result, [Report]),
		    {ok, Result};
		{state, _, ["CANCELLED"]} ->
		    format_progress_report(Report),
		    ct:pal("~s",[format_progress_report(Report)]),
		    %% {ok, {result, _, Result}} =
		    %% 	extract_element(result, [Report]),
		    {ok, cancelled};
		{state, _, [Current]} ->
		    {ok, {progressPercentage, _, [Percent]}} =
			extract_element(progressPercentage, [Report]),
		    {ok, {progressInfo, _, [Info]}} =
			extract_element(progressInfo, [Report]),
		    {ok, {actionName, _, [ActionName]}} =
			extract_element(actionName, [Report]),
		    ct:pal("Action: ~s~nState: ~s ~s % ready~n~s",
			   [ActionName, Current, Percent, Info]),
		    loop
	    end
    end.



%%%--------------------------------------------------------------------
%%% Description: Format a AsyncActionStruct for printing
%%%--------------------------------------------------------------------


format_progress_report({progressReport, _, Members}) ->
    [io_lib:format("progressReport:~n",[])|format_progress_report(Members)];
format_progress_report({progress, _, Members}) ->
    [io_lib:format("progress:~n",[])|format_progress_report(Members)];
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) ->
    ct:pal("Unknown format: ~p~n",[X]),
    ct:fail(unknown_progress_report_format).



%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

%%%--------------------------------------------------------------------
%%% Description: Returns a list of all backup elements
%%%--------------------------------------------------------------------

get_all_backups(MeId) ->
    Get =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]}]}]}]}]},
    {ok, Result} = netconf(get, [nc1, Get]),
    {ok, {_, _, Contents}} = extract_element('BrmBackupManager', Result),
    [BrmBackupE||BrmBackupE<-Contents,
		 element(1, BrmBackupE) == 'BrmBackup'].


unique_name() ->
    {A,B,C} = os:timestamp(),
    lists:flatten(io_lib:format("~w-~6..0w-~6..0w",[A,B,C])).


%% %%%--------------------------------------------------------------------
%% %%% Description: Checks the progress reports (old style will be replaced)
%% %%%--------------------------------------------------------------------


%% %% %%Checks that the file has loaded. Loops around max ten times if not
%% check_progressreport(Counter)->

%%     {ok,
%%      [{'ManagedElement',
%%        _,
%%        [{managedElementId,[],["1"]},
%%  {'SystemFunctions',[],
%%   [{systemFunctionsId,[],["1"]},
%%    {'BrM',
%%     _,
%%     [{brMId,[],["1"]},
%%      {'BrmBackupManager',[],
%%       [{brmBackupManagerId,[],["1"]},
%%        {backupType,[],[_]},
%%        {backupDomain,[],[_]},
%%        {progressReport,
%%         [{struct,"AsyncActionProgress"}],
%%         ProgressReportList}|_]}]}]}]}]}
%%  =ct_netconfc:get(nc1,{'ManagedElement',
%%                [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%%                [{managedElementId,[],["1"]},
%%                 {'SystemFunctions',
%%              [{systemFunctionsId,[],["1"]},
%%               {'BrM',
%%                [{brMId,[],["1"]},
%%                 {'BrmBackupManager',
%%                  [{brmBackupManagerId,[],["1"]}

%%                  ]}]}]}]}),

%%     {value,{state,_,[State]}}=lists:keysearch(state,1,ProgressReportList),

%%     case State of      %%Looks if it's finished loading and loops around if not
%%  "FINISHED"->
%%      {value,{result,_,[Result]}}=lists:keysearch(result,1,ProgressReportList),
%%      case Result of
%%      "SUCCESS"->
%%          ok;
%%      "FAILURE"->
%%          case lists:keysearch(progressInfo,1,ProgressReportList) of

%%          {value,{progressInfo,[],[]}}->
%%              case lists:keysearch(resultInfo,1,ProgressReportList) of
%%              {value,{resultInfo,[],[]}}->
%%                  ct:fail("unknown");
%%              {value,{resultInfo,[],[ResultInfo]}}->
%%                  ct:fail(ResultInfo);
%%              _ ->
%%                  ct:fail("Something unexpected has happened. ProgressReport is out of order concerning ResultInfo")
%%              end;

%%          {value,{progressInfo,[],[ProgressInfo]}}->
%%              case lists:keysearch(resultInfo,1,ProgressReportList) of
%%              {value,{resultInfo,[],[]}}->
%%                  ct:fail(ProgressInfo);
%%              {value,{resultInfo,[],[ResultInfo]}}->
%%                  ct:fail(ProgressInfo++ResultInfo);
%%              _ ->
%%                  ct:fail(ProgressInfo++"Something unexpected has happened. ProgressReport is out of order concerning ResultInfo")
%%              end;
%%          _ ->
%%              ct:fail("Something unexpected has happened. ProgressReport is out of order conserning ProgressInfo")
%%          end;

%%      _->
%%          ct:fail("Somethings unexpected has happened. The Result in the progressReport is "++Result)
%%      end;


%% %% This is where it goes if it hasn't loaded
%%  _ ->

%%      case Counter>10 of
%%      true->
%%          ct:fail("Failed to load backup");
%%      false->
%%          {value,{progressPercentage,_,[ProgressPercentage]}}=lists:keysearch(progressPercentage,1,ProgressReportList),
%%          io:format("Loading ~s%",[ProgressPercentage]),
%%          erlang:display("Loading"),
%%          erlang:display(ProgressPercentage++"%"),
%%          timer:sleep(2000),
%%          check_progressreport(Counter+1)
%%      end
%%     end.



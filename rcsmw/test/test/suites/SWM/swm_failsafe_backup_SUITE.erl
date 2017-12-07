%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_failsafe_backup_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R5A/R6A/1
%%%
%%% @doc == Test Suite for testing failsafe backups using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(swm_failsafe_backup_SUITE).
-vsn('/main/R2A/R3A/R5A/R6A/1').
-author('etxkols').

%%% ----------------------------------------------------------
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
%%% R2A/1      2014-01-15 etxjotj     Created
%%% R2A/2      2014-04-07 etxivri     quick fix to handle if action id not
%%%                                   changed = HS43611 for tc
%%%                                   restore_while_failsafe.
%%% R2A/13     2014-04-08 erarafo     Deprecation warning
%%% R2A/14     2014-04-14 etxjotj     Deprecation warning removed
%%% R2A/21     2014-08-19 etxivri     Update to be more robust.
%%% R2A/22     2014-08-19 etxivri     Update to make it more robust.
%%% R2A/24     2014-08-21 etxivri     Update to use groups.
%%% R2A/25     2014-08-21 etxivri     Edoc correction
%%% R2A/26     2014-09-03 etxivri     Changed a ct:pal to ct:log.
%%% R2A/28     2014-10-07 etxivri     Update to move SUITE to block SWM.
%%% R2A/29     2014-10-09 etxivri     Update to use swmSchedBuLib.
%%% R3A/1      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R5A/1      2016-01-21 erarafo     Fixed compiler warning
%%% R5A/2      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R6A/1      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------

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
-export([read_usage_state/1,
	 failsafe_configuration/1,               % NodeUC608.N
	 failsafe_backup_fail/1,                 % NodeUC608.E1
	 failsafe_triggered/1,                   % NodeUC608.A1
	 reboot_while_failsafe/1,                % NodeUC608.A2
	 restore_while_failsafe/1,               % NodeUC608.A3
	 configure_timeout_period/1,             % NodeUC609.N
	 configure_timeout_period_activated/1]). % NodeUC609.A1
-export([busy_test/1,                            % 105 65-0771/00692
	 scheduled_backup_test/1]).              % 105 65-0771/00693 00694


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [read_usage_state, configure_timeout_period,
     configure_timeout_period_activated,
     failsafe_configuration, failsafe_backup_fail,
     failsafe_triggered, reboot_while_failsafe, restore_while_failsafe,
     busy_test, scheduled_backup_test].


groups() ->
    [{group_1,[],[read_usage_state, 
		  configure_timeout_period,
		  configure_timeout_period_activated,
		  failsafe_configuration, 
		  failsafe_backup_fail,
		  failsafe_triggered, 
		  reboot_while_failsafe
		 ]},
     {group_2,[],[restore_while_failsafe,
		  busy_test,
		  scheduled_backup_test
		 ]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,nc1},
		 {rct_netconf,nc2},
                 {cth_conn_log,[]},
                 {rct_core,[]},
		 {rct_coli, {coli, [manual_connect]}},
                 {rct_logging,
		  {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
				  ["Simulated failure"]}}]}}
    ]}].

%% @hidden
init_per_suite(Config) ->
    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),

    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    UserLabel = proplists:get_value(userLabel, MeData),
    %% Temporary until netconf events are turned on by default
    rct_rpc:call(rpc_1, swmLib, set_variable, [scheduler_debug, true], 10000),
    %% Temporary fix ends here
    [{meId, MeId}, {userLabel, UserLabel}, {backupName, "swm_failsafe"}
     | Config].
%% @hidden
end_per_suite(_Config) ->
    %% Temporary until netconf events are turned on by default
    rct_rpc:call(rpc_1, swmLib, erase_variable, [scheduler_debug], 10000),
    %% Temporary fix ends here
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
init_per_testcase(TestCase = failsafe_backup, Config) ->
    ct:print("Running ~p~n",[TestCase]),
    flush_msgs(),
    Config;
init_per_testcase(TestCase = scheduled_backup_test, Config) ->
    ct:print("Running ~p~n",[TestCase]),
    clear_schedule(Config),
    Config;
init_per_testcase(TestCase, Config) ->
    ct:print("Running ~p~n",[TestCase]),
    UserLabel = proplists:get_value(userLabel, Config),
    set_user_label(Config, UserLabel),
    flush_msgs(),
    Config.
%% @hidden
end_per_testcase(scheduled_backup_test, Config) ->
    clear_schedule(Config),
    ok;
end_per_testcase(restore_while_failsafe, Config) ->
    rct_rpc:call(rpc_1, swmLib, erase_variable,
		 [swm_failsafe_backup_SUITE_create_fail], 10000),
    MeId = proplists:get_value(meId, Config),
    Deactivate = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'BrM',
		      [{brMId,[],["1"]},
		       {'BrmBackupManager',
			[{brmBackupManagerId,[],["1"]},
			 {'BrmFailsafeBackup',
			  [{brmFailsafeBackupId, [], ["1"]},
			   {'deactivate',[]}]}]}]}]}]},
    ct:pal("Ending testcase: executing action deactivate"),
    {ok, _} = netconf(action, [nc1, Deactivate]),
   
    UserLabel = proplists:get_value(userLabel, Config),
    set_user_label(Config, UserLabel),
    Name = proplists:get_value(backupName, Config),

    DeleteBackUp = {'ManagedElement',
		    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		    [{managedElementId,[],[MeId]},
		     {'SystemFunctions',
		      [{systemFunctionsId,[],["1"]},
		       {'BrM',
			[{brMId,[],["1"]},
			 {'BrmBackupManager',
			  [{brmBackupManagerId,[],["1"]},
			   {'deleteBackup',[],
			    [{name, [], [Name]}]}]}]}]}]},
    {ok, _} = netconf(action, [nc1, DeleteBackUp]);
end_per_testcase(_TestCase, Config) ->
    rct_rpc:call(rpc_1, swmLib, erase_variable,
		 [swm_failsafe_backup_SUITE_create_fail], 10000),
    MeId = proplists:get_value(meId, Config),
    Deactivate = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'BrM',
		      [{brMId,[],["1"]},
		       {'BrmBackupManager',
			[{brmBackupManagerId,[],["1"]},
			 {'BrmFailsafeBackup',
			  [{brmFailsafeBackupId, [], ["1"]},
			   {'deactivate',[]}]}]}]}]}]},
    ct:pal("Ending testcase: executing action deactivate"),
    {ok, _} = netconf(action, [nc1, Deactivate]),
    


    UserLabel = proplists:get_value(userLabel, Config),
    set_user_label(Config, UserLabel),
    ok.

flush_msgs() ->
    receive
	X ->
	    ct:print("Flushed ~p~n",[X]),
	    flush_msgs()
    after 0 ->
	    ok
    end.


%%%--------------------------------------------------------------------
%%% @doc Read the failsafe usage state
%%% @end
%%%--------------------------------------------------------------------

read_usage_state(Config) ->
    Value = get_ro_attribute(Config, usageState),
    ct:pal("Usage state is ~s~n",[Value]),
    Value.

%%%--------------------------------------------------------------------
%%% @doc NodeUC608.N Doing a failsafe configuration
%%% @end
%%%--------------------------------------------------------------------

failsafe_configuration(Config) ->
    MeId = proplists:get_value(meId, Config),

    UserLabel = case proplists:get_value(userLabel, Config) of
		    undefined -> "";
		    UL -> UL
		end,
    set_timeout(Config, 3600),


    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    "BUSY" = read_usage_state(Config),
    set_user_label(Config, UserLabel++"$$failsafe_test$$"),
    flush_msgs(),
    Deactivate = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'BrM',
		      [{brMId,[],["1"]},
		       {'BrmBackupManager',
			[{brmBackupManagerId,[],["1"]},
			 {'BrmFailsafeBackup',
			  [{brmFailsafeBackupId, [], ["1"]},
			   {'deactivate',[]}]}]}]}]}]},
    ct:pal("Executing action deactivate"),
    {ok, _} = netconf(action, [nc1, Deactivate]),
    "IDLE" = read_usage_state(Config),
    %% Event = wait_for_notification('AVC', usageState),
    %% ct:pal("Event = ~p~n",[Event]),
    set_user_label(Config, UserLabel),
    ok.

%%%--------------------------------------------------------------------
%%% @doc NodeUC608.E1 Failsafe backup cannot be created
%%% @end
%%%--------------------------------------------------------------------

failsafe_backup_fail(Config) ->
    %% Simulate a backup failure
    ok = rct_rpc:call(rpc_1, swmLib, set_variable,
		      [swm_failsafe_backup_SUITE_create_fail, true], 10000),

    MeId = proplists:get_value(meId, Config),

    set_timeout(Config, 3600),

    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ct:fail("Activation succeeded!");
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    case read_usage_state(Config) of
		"IDLE" ->
		    ok;
		UsageState ->
		    ct:fail({wrong_usage_state, UsageState})
	    end
    end.

%%%--------------------------------------------------------------------
%%% @doc NodeUC608.A1 Failsafe rollback function is triggered
%%% @end
%%%--------------------------------------------------------------------

failsafe_triggered(Config) ->
    MeId = proplists:get_value(meId, Config),

    UserLabel = case proplists:get_value(userLabel, Config) of
		    undefined -> "";
		    UL -> UL
		end,

    Timeout = 120,
    set_timeout(Config, Timeout),

    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,

    ProgressFilter1 =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmFailsafeBackup',
    		 [{brmFailsafeBackupId, [], ["1"]},
    		  {progress,[]}]}]}]}]}]},

    ResultInfo = get_result_info(ProgressFilter1),
    BuId = lists:last(string:tokens(ResultInfo, "=")),
    "BUSY" = read_usage_state(Config),

    set_user_label(Config, UserLabel++"$$failsafe_test$$"),


    ct:pal("Check that the coli connection goes down"),
    poll_connection(coli, 1000, {error,econnrefused}, Timeout+60),

    ct:pal("Wait for the coli connection"),
    poll_connection(coli, 3000, ok, 60),

    Get =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmBackup',
    		 [{brmBackupId, [], [BuId]}]}]}]}]}]},
    
    %% inserted to avoid compiler warning since value of Get is not
    %% used anywhere else
    put(qwertyuiop, Get),

    ct:pal("Sleep a while to ensure netconf can be used.", []),
    timer:sleep(30000),

    %% Backup will not be removed until an hour has passed ->
    %% Too long execution time
    %% {ok, _} = ct_netconfc:open(nc1, []),
    %% case ct_netconfc:get(nc1, Get) of
    %% 	{ok, GetResult} ->
    %% 	    ct:pal("Backup still exists: ~p~n",[GetResult]),
    %% 	    ct:fail(backup_still_exists);
    %% 	{error, _} ->
    %% 	    %% We shouldn't really accept everything here, but anyway
    %% 	    ok
    %% end,
    %% ok = ct_netconfc:close(nc1),

    %% "IDLE" = read_usage_state(Config),
    %% Value1 = get_ro_attribute(Config, timeRemaining),
    %% timer:sleep(2000),
    %% Value2 = get_ro_attribute(Config, timeRemaining),
    %% case Value1 of
    %% 	Value2 ->
    %% 	    ct:pal("The value of timeRemaining is not updating:~n~p~n~p~n",
    %% 		   [Value1, Value2]),
    %% 	    ct:fail(countdown_stopped_after_reboot);
    %% 	_ ->
    %% 	    ok
    %% end,


    "IDLE" = read_usage_state(Config),
    GetC = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {userLabel, []}]},
    {ok, GetCResult} = netconf(get_config, [nc1, running, GetC]),
    case extract_element(userLabel, GetCResult) of
	{ok, {userLabel, [{unset, "true"}], []}} when
	      UserLabel == undefined;
	      UserLabel == [] ->
	    ok;
	{ok, {userLabel, [{unset, "true"}], []}} when
	      UserLabel /= undefined ->
	    ct:pal("User label was not reset to ~p. It is unset~n",
		   [UserLabel]),
	    ct:fail(restore_failed);
	{ok, {userLabel, _, [UserLabel]}} ->
	    ok;
	{ok, {userLabel, _, [Unexpected]}} ->
	    ct:pal("User label was not reset to ~p. It has value ~p~n",
		   [UserLabel, Unexpected]),
	    ct:fail(restore_failed)
    end.



%%%--------------------------------------------------------------------
%%% @doc NodeUC608.A2 Restart while the failsafe function is activated
%%% @end
%%%--------------------------------------------------------------------

reboot_while_failsafe(Config) ->
    MeId = proplists:get_value(meId, Config),
    set_timeout(Config, 3600),
    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    "BUSY" = read_usage_state(Config),

    ok = rct_rpc:call(rpc_1, init, reboot, [], 10000),

    ct:pal("Check that the coli connection goes down"),
    poll_connection(coli, 1000, {error,econnrefused}, 40),

    ct:pal("Wait for the coli connection"),
    poll_connection(coli, 3000, ok, 60),


    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{'BrmFailsafeBackup',
		 [{brmFailsafeBackupId, [], ["1"]},
		  {progress,[]}]}]}]}]}]},

    wait_for_progress(progress, undefined, ProgressFilter),
    "BUSY" = read_usage_state(Config),
    Value1 = get_ro_attribute(Config, timeRemaining),
    timer:sleep(2000),
    Value2 = get_ro_attribute(Config, timeRemaining),
    case Value1 of
	Value2 ->
	    ct:pal("The value of timeRemaining is not updating:~n~p~n~p~n",
		   [Value1, Value2]),
	    ct:fail(countdown_stopped_after_reboot);
	_ ->
	    ok
    end,
    ok.

%%%--------------------------------------------------------------------
%%% @doc NodeUC608.A2 Restore of backup while failsafe function is activated
%%% @end
%%%--------------------------------------------------------------------

restore_while_failsafe(Config) ->
    MeId = proplists:get_value(meId, Config),
    Action_Id = no_check,
    MoRef = create_backup(Config, Action_Id),

    set_timeout(Config, 3600),

    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    "BUSY" = read_usage_state(Config),

    BuId = lists:last(string:tokens(MoRef, "=")),
    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{'BrmBackup',
		 [{brmBackupId, [], [BuId]},
		  {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    ct:pal("Executing action restore~n"),
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'BrmBackup',
		      [{brmBackupId, [], [BuId]},
		       {restore, []}]}]}]}]}]},
    Ares = netconf(action, [nc1, Action]),
    case Ares of
	{ok, _} -> ok;
	Error2 ->
	    ct:pal("restore action failed: ~p~n",[Error2]),
	    ct:fail(Error2)
    end,
    ct:pal("Check that the coli connection goes down"),
    poll_connection(coli, 1000, {error,econnrefused}, 40),

    ct:pal("Wait for the coli connection"),
    poll_connection(coli, 3000, ok, 90),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    "IDLE" = read_usage_state(Config),
	    ok;
	Result ->
	    ct:pal("restoreBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%% create_backup(Config) ->
%%     create_backup(Config, checkActionId).
create_backup(Config, Action_Id) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),

    ct:pal("create_backup with Name: ~p", [Name]),

    ProgressFilter =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'BrM',
	     [{brMId,[],["1"]},
	      {'BrmBackupManager',
	       [{brmBackupManagerId,[],["1"]},
		{progressReport, []}]}]}]}]},
    case Action_Id of
	no_check -> %% No check that actionId has changed.
	    ActionId = Action_Id;
	_ -> %% Check ActionId has changed.
	    ActionId = get_action_id(ProgressFilter)
    end,
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'createBackup',[],
		      [{name, [], [Name]}]}]}]}]}]},
    ct:pal("Executing action createBackup"),
    {ok, ActionResponse} = netconf(action, [nc1, Action]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,

    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ct:pal("Backup succesfully created"),
	    get_result_info(ProgressFilter);
	Result ->
	    ct:pal("createBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.



%%%--------------------------------------------------------------------
%%% @doc NodeUC609.N Configure the timeout period
%%% @end
%%%--------------------------------------------------------------------

configure_timeout_period(Config) ->
    "IDLE" = read_usage_state(Config),
    Length = get_attribute(Config, timeoutLength),
    NewLength = integer_to_list(list_to_integer(Length)+10),
    ok = set_attribute(Config, timeoutLength, NewLength).

%%%--------------------------------------------------------------------
%%% @doc NodeUC609.A1 Configure the timeout period when failsafe is activated
%%% @end
%%%--------------------------------------------------------------------

configure_timeout_period_activated(Config) ->
    ct:print("a"),
    MeId = proplists:get_value(meId, Config),
    "IDLE" = read_usage_state(Config),
    ct:print("Usage state is IDLE~n",[]),
    ok = set_attribute(Config, timeoutLength, "3600"),

    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    "BUSY" = read_usage_state(Config),

    Length = get_ro_attribute(Config, timeRemaining),
    NewLength = integer_to_list(list_to_integer(Length)+1000),
    set_attribute(Config, timeoutLength, NewLength),
    ActualNewLength = get_ro_attribute(Config, timeRemaining),
    case {list_to_integer(Length), list_to_integer(ActualNewLength)} of
	{Old, New}  when New > Old ->
	    ok;
	{Old, New} ->
	    ct:pal("Timeout length after set is ~w. Before ~w. ~n"
		   "Operation had no value on timeoutLength~n"),
	    ct:fail({Old, New})
    end.

%%%--------------------------------------------------------------------
%%% @doc Not more than one active failsafe backup at any one time
%%% DR 105 65-0771/00695
%%% @end
%%%--------------------------------------------------------------------

busy_test(Config) ->
    MeId = proplists:get_value(meId, Config),

    set_timeout(Config, 3600),

    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    "BUSY" = read_usage_state(Config),
    ct:pal("Executing action activate"),
    {ok, ActionResponse2} = netconf(action, [nc1, Activate]),
    case extract_element(returnValue, ActionResponse2) of
	{ok, {returnValue, _, ["97"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue2]}} ->
	    Error2 = decode_return_value(ReturnValue2),
	    ct:pal("Return value is ~w (~s)~n",[Error2, ReturnValue2]),
	    ct:fail(Error2)
    end.

%%%--------------------------------------------------------------------
%%% @doc Failsafe and scheduled backups DR 105 65-0771/00693 00694
%%% 693: Backup creation prohibited during failsafe configuration
%%% 694: Alarm for prohibited scheduled backup
%%% @end
%%%--------------------------------------------------------------------

scheduled_backup_test(Config) ->
    MeId = proplists:get_value(meId, Config),
    set_timeout(Config, 3600),

    Activate = {'ManagedElement',
		[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		[{managedElementId,[],[MeId]},
		 {'SystemFunctions',
		  [{systemFunctionsId,[],["1"]},
		   {'BrM',
		    [{brMId,[],["1"]},
		     {'BrmBackupManager',
		      [{brmBackupManagerId,[],["1"]},
		       {'BrmFailsafeBackup',
			[{brmFailsafeBackupId, [], ["1"]},
			 {'activate',[]}]}]}]}]}]},
    ct:pal("Executing action activate"),
    {ok, ActionResponse} = netconf(action, [nc1, Activate]),

    case extract_element(returnValue, ActionResponse) of
	{ok, {returnValue, _, ["0"]}} ->
	    ok;
	{ok, {returnValue, _, [ReturnValue]}} ->
	    Error = decode_return_value(ReturnValue),
	    ct:pal("Return value is ~w (~s)~n",[Error, ReturnValue]),
	    ct:fail(Error)
    end,
    "BUSY" = read_usage_state(Config),

    Name = "swm_failsafe_test",

    %% Activate scheduled backups

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous action id is ~p~n",[ActionId]),

    TS = timestamp(),
    FutureTime = add_time(add_time(), TS),
    %% Why do the time differ between erlang on CT and the real node? 
    %% LegalTime = iso_time(FutureTime, extended_zonefree),
    LegalTime = rct_rpc:call(rpc_1, comsaI, iso_time, [FutureTime, extended_zonefree], 10000),

    ct:pal("LegalTime: ~p", [LegalTime]),

    Set =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmBackupScheduler',
    		 [{brmBackupSchedulerId,[], ["1"]},
    		  {scheduledBackupName, [Name]},
    		  {'BrmPeriodicEvent', [],
    		   [{brmPeriodicEventId, [Name++"_sct"]},
		    {hours, ["0"]},
		    %% {minutes, ["5"]},
		    {minutes, ["2"]},
   		    {startTime,[LegalTime]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Set]),
    set_admin_state(Config, "UNLOCKED"),
    ct:pal("Scheduled time is ~p~n",[LegalTime]),
    case get_next_scheduled_time(MeId) of
    	LegalTime ->
    	    ct:pal("Announced time is ok~n",[]),
    	    ok;
    	NextTime ->
    	    ct:pal("Scheduling fault!~nExpected: ~p~nAnnounced: ~p~n",
    		   [LegalTime, NextTime]),
    	    ct:fail(scheduling_fault)
    end,
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
    	["NOT_AVAILABLE"] ->
    	    ok;
    	Result ->
    	    ct:pal("scheduledBackup while suppressed: ~s~n",[Result]),
    	    ct:fail(Result)
    end,

    timer:sleep(5000),

    %% The scheduled backup has been omitted, good, now check for alarm
    case is_alarm(MeId, "9175044") of
	true ->
	    ok;
	false ->
	    ct:fail("ScheduledBackupFailed alarm was not present")
    end,

    NT = get_next_scheduled_time(MeId),
    ct:pal("Next time is: ~p before deactivate the failsafe~n", [NT]),

    %% Now deactivate the failsafe
    Deactivate = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'BrM',
		      [{brMId,[],["1"]},
		       {'BrmBackupManager',
			[{brmBackupManagerId,[],["1"]},
			 {'BrmFailsafeBackup',
			  [{brmFailsafeBackupId, [], ["1"]},
			   {'deactivate',[]}]}]}]}]}]},
    ct:pal("Executing action deactivate"),
    {ok, _} = netconf(action, [nc1, Deactivate]),
    "IDLE" = read_usage_state(Config),
    ActionId2 = get_action_id(ProgressFilter),
    ct:pal("Previous action id is ~p~n",[ActionId2]),

    timer:sleep(10000),
    NextTime2 = get_next_scheduled_time(MeId),
    case LegalTime of
	NextTime2 ->
	    ct:fail("nextScheduledTime has not been changed, still: ~p", [NextTime2]);
	_Else ->
	    ct:pal("Next time is: ~p~n",[NextTime2])
    end,

    case wait_for_progress(progressReport, ActionId2, ProgressFilter) of
    	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
    	    ok;
    	Result2 ->
    	    ct:pal("scheduledBackup: ~s~n",[Result2]),
    	    ct:fail(Result2)
    end,

    timer:sleep(5000),

    %% The scheduled backup has been omitted, good, now check for alarm
    case is_alarm(MeId, "9175044") of
	false ->
	    ok;
	true ->
	    ct:fail("ScheduledBackupFailed alarm was not ceased")
    end.

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

is_alarm(MeId, MinorType) ->
    %% {ok, _} = ct_netconfc:open(nc1, []),
    %% Res = swm_test_lib:is_alarm(nc1, MeId, MinorType),
    %% ct_netconfc:close(nc1),
    Res = swmSchedBuLib:is_alarm(nc1, MeId, MinorType),
    Res.


add_time() ->
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode, [], 10000) of
	simulated ->
	    15;
	target ->
	    120
    end.

clear_schedule(Config) ->
    MeId = proplists:get_value(meId, Config),
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'BrM',
	       [{brMId,[],["1"]},
		{'BrmBackupManager',
		 [{brmBackupManagerId,[],["1"]},
		  {'BrmBackupScheduler',
		   [{brmBackupSchedulerId,[], ["1"]}]}]}]}]}]},
    {ok, Res} = netconf(get_config, [nc1, running, Get]),
    {ok, {'BrmBackupScheduler', Attrs, Content}} =
	extract_element('BrmBackupScheduler', Res),
    Delete = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	       {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'BrmBackupScheduler', Attrs,
		      [{brmBackupSchedulerId,[], ["1"]}|
		       [case element(1, Item) of
			    'BrmSingleEvent' = Element ->
				{ok, Id} = extract_element(
					     brmSingleEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]};
			    'BrmPeriodicEvent' = Element->
				{ok, Id} = extract_element(
					     brmPeriodicEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]};
			    'BrmCalendarBasedPeriodicEvent' = Element ->
				{ok, Id} = extract_element(
					     brmCalendarBasedPeriodicEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]}
			end||Item<-Content,
			     (element(1,Item) == 'BrmSingleEvent') or
				 (element(1, Item) == 'BrmPeriodicEvent') or
				 (element(1, Item) == 'BrmCalendarBasedPeriodicEvent')]
		       ]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Delete]),

    %% Deactivate failsafe
    Deactivate = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]},
		   {'SystemFunctions',
		    [{systemFunctionsId,[],["1"]},
		     {'BrM',
		      [{brMId,[],["1"]},
		       {'BrmBackupManager',
			[{brmBackupManagerId,[],["1"]},
			 {'BrmFailsafeBackup',
			  [{brmFailsafeBackupId, [], ["1"]},
			   {'deactivate',[]}]}]}]}]}]},
    {ok, _} = netconf(action, [nc1, Deactivate]).


%%%--------------------------------------------------------------------
%%% Description: Set the BrmBackupScheduler.adminState attribute
%%%--------------------------------------------------------------------

set_admin_state(Config, Value) ->
    MeId = proplists:get_value(meId, Config),
    Set =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmBackupScheduler',
    		 [{brmBackupSchedulerId,[], ["1"]},
		  {adminState, [Value]}]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Set]).

%%%--------------------------------------------------------------------
%%% Description: Read the value of BrmBackupScheduler.nextScheduledTime
%%%--------------------------------------------------------------------

get_next_scheduled_time(MeId) ->
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'BrM',
	       [{brMId,[],["1"]},
		{'BrmBackupManager',
		 [{brmBackupManagerId,[],["1"]},
		  {'BrmBackupScheduler',
		   [{brmBackupSchedulerId, [], ["1"]},
		    {nextScheduledTime, []}]}]}]}]}]},

    {ok, A} = netconf(get, [nc1, Get]),
    case extract_element(nextScheduledTime, A) of
	{ok, {nextScheduledTime, [{unset, "true"}], []}} ->
	    undefined;
	{ok, {nextScheduledTime, _, [Time]}} ->
	    Time
    end.


%%% ----------------------------------------------------------
%%% @doc Convert an os:timestamp() tuple to an ISO 8601 string
%%%
%%% Input: Now  - An os:timestamp() tuple
%%%        Type - basic|extended|extended_zonefree|extended_z
%%% Output: string()
%%% @end
%%% ----------------------------------------------------------

%% iso_time(Now, Type) ->
%%     fn_date(Now, Type)++"T"++fn_time(Now, Type).

%% fn_date(Now, Type) ->
%%     {{Y,M,D}, _} = calendar:now_to_local_time(Now),
%%     case Type of
%%         basic ->
%%             lists:append([integer_to_list(Y),
%%                           padzero(M),
%%                           padzero(D)]);
%% 	extended_z ->
%% 	    {{YU,MU,DU}, _} = calendar:now_to_universal_time(Now),

%%             lists:append([integer_to_list(YU), "-",
%%                           padzero(MU), "-", padzero(DU)]);

%%         Extended when Extended==extended; Extended==extended_zonefree ->
%%             lists:append([integer_to_list(Y), "-",
%%                           padzero(M), "-", padzero(D)])
%%     end.

%% fn_time(Now, Type) ->
%%     DT={_, {H, M, S}} = calendar:now_to_local_time(Now),
%%     UTC = calendar:now_to_universal_time(Now),
%%     DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
%%         calendar:datetime_to_gregorian_seconds(UTC),
%%     [Sign, DH, DM] = diff(DiffSecs),
%%     case Type of
%%         basic ->
%%             lists:append([padzero(H),
%%                           padzero(M),
%%                           padzero(S),
%%                           [Sign],
%%                           padzero(DH),
%%                           padzero(DM)]);
%% 	extended_z ->
%% 	    {_, {HU, MU, SU}} = UTC,
%%             lists:append([padzero(HU), ":",padzero(MU), ":",padzero(SU),"Z"]);
%%         extended ->
%%             lists:append([padzero(H), ":", padzero(M), ":", padzero(S),
%%                           [Sign], padzero(DH), ":", padzero(DM)]);
%%         extended_zonefree ->
%%             lists:append([padzero(H), ":", padzero(M), ":", padzero(S)])
%%     end.

%% padzero(N) ->
%%     if N<10 -> [$0, N+$0];
%%        true -> integer_to_list(N)
%%     end.


%% diff(Secs) ->
%%     case calendar:seconds_to_daystime(Secs) of
%%         {0, {H, M,_}} ->
%%                 [$+, H, M];
%%         {-1, _} ->
%%                 {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
%%                 [$-, H, M]
%%     end.

%%%--------------------------------------------------------------------
%%% Description: Adds seconds to a now-tuple
%%%--------------------------------------------------------------------

add_time(N, {N1,N2,N3}) ->
    case N2+N of
	F2 when F2 >= 1000000 ->
	    {N1+F2 div 1000000, F2 rem 1000000, N3};
	F2 ->
	    {N1, F2, N3}
    end.

%%%--------------------------------------------------------------------
%%% Description: Get node time
%%%--------------------------------------------------------------------

timestamp() ->
    rct_rpc:call(rpc_1, os, timestamp, [], 2000).

%%%--------------------------------------------------------------------
%%% Description: Test the OAM connectivity
%%% Name =           atom()       Only suport coli (can be built out)
%%% Timer =          integer()    Time between attempts.
%%% NumberAttempts = integer()    Number of Attempts.
%%% ExpectedResult =              Expected return.
%%%--------------------------------------------------------------------

poll_connection(HookName, _Timer, _Result, 0)->
    ct:fail("Tried too many times to connect to rct_~p", [HookName]) ;
poll_connection(HookName, Timer, Result, NumTry)->
    timer:sleep(Timer),
    case HookName of
        coli -> case rct_coli:connect(coli) of
                    Result -> ct:log("OK: ~p",[Result]),
                              rct_coli:disconnect(HookName),
                              ok;
                    _-> ok = rct_coli:disconnect(HookName),
                        poll_connection(HookName, Timer, Result, NumTry - 1)
                end;
        _-> ct:fail("HookName ~p not suported",[HookName])
    end.

%%%--------------------------------------------------------------------
%%% Description: Set the timeoutLength attribute
%%%--------------------------------------------------------------------

set_timeout(Config, Timeout) ->
    set_attribute(Config, timeoutLength, integer_to_list(Timeout)).

%%%--------------------------------------------------------------------
%%% Description: Get the named attribute
%%%--------------------------------------------------------------------

get_attribute(Config, Attribute) ->
    MeId = proplists:get_value(meId, Config),
    GetConfig =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmFailsafeBackup',
    		 [{brmFailsafeBackupId,[],["1"]},
    		  {Attribute, []}]}]}]}]}]},
    {ok, Res} = netconf(get_config, [nc1, running, GetConfig]),
    {ok, {_, _, [Value]}} = extract_element(Attribute, Res),
    Value.

%%%--------------------------------------------------------------------
%%% Description: Get the value of a read only attribute
%%%--------------------------------------------------------------------

get_ro_attribute(Config, Attribute) ->
    MeId = proplists:get_value(meId, Config),
    Get =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmFailsafeBackup',
    		 [{brmFailsafeBackupId,[],["1"]},
    		  {Attribute, []}]}]}]}]}]},
    {ok, Res} = netconf(get, [nc1, Get]),
    {ok, {_, _, [Value]}} = extract_element(Attribute, Res),
    Value.

%%%--------------------------------------------------------------------
%%% Description: Edit the value of an attribute
%%%--------------------------------------------------------------------

set_attribute(Config, Attribute, Value) ->
    MeId = proplists:get_value(meId, Config),
    EditConfig =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmFailsafeBackup',
    		 [{brmFailsafeBackupId,[],["1"]},
    		  {Attribute, [Value]}]}]}]}]}]},
    netconf(edit_config, [nc1, running, EditConfig]).



%%%--------------------------------------------------------------------
%%% Description: Read the ManagedElement.userLabel attribute
%%%--------------------------------------------------------------------

%% get_user_label(Config) ->
%%     MeId = proplists:get_value(meId, Config),
%%     GetMeLabel =
%% 	{'ManagedlEement',
%% 	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 	 [{managedElementId,[],[MeId]},
%% 	  {userLabel, []}]},
%%     {ok, GetRes} = netconf(get_config, [nc1, GetMeLabel]),
%%     {ok, {userLabel, _, [UserLabel]}} = extract_element(userLabel, GetRes),
%%     UserLabel.

%%%--------------------------------------------------------------------
%%% Description: Set the ManagedElement.userLabel attribute
%%%--------------------------------------------------------------------

set_user_label(Config, undefined) ->
    MeId = proplists:get_value(meId, Config),
    SetMeLabel =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	  {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	 [{managedElementId,[],[MeId]},
	  {userLabel, [{'xc:operation', "delete"}], []}]},
    netconf(edit_config, [nc1, running, SetMeLabel]);

set_user_label(Config, UserLabel) ->
    MeId = proplists:get_value(meId, Config),
    SetMeLabel =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {userLabel, [UserLabel]}]},
    netconf(edit_config, [nc1, running, SetMeLabel]).

%%%--------------------------------------------------------------------
%%% Description: Convert an backup action error code to a meaningful value
%%%--------------------------------------------------------------------

decode_return_value("0") -> actionComplete;
decode_return_value("1") -> nameValidationFailure;
decode_return_value("2") -> duplicateName;
decode_return_value("3") -> housekeepingRequired;
decode_return_value("4") -> backupNotFound;
decode_return_value("97") -> functionBusy;
decode_return_value("98") -> missingParameter;
decode_return_value("99") -> softwareFault;
decode_return_value(Value) ->
    ct:fail({unknown_return_value, Value}).


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
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
    case extract_element(progressReport, A) of
    	{ok, {progressReport, [{unset, "true"}], []}}  ->
    	    undefined;
    	_ ->
    	    {ok, {Member, [], [Value]}} =
    		extract_element(Member, A),
    	    Value
    end.


%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------

%% cmd(Cmd) ->
%%     ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
%%     R.

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	{ok, Res} ->
	    ok = ct_netconfc:close_session(nc1),
	    {ok, Res};
	Other ->
	    ct:fail(Other)
    end.



%%%--------------------------------------------------------------------
%%% Description: This function waits for a specified notification to be
%%% received. Othere notifications received are ignored
%%%--------------------------------------------------------------------

%% wait_for_notification('AVC', AttrName) ->
%%     receive
%% 	{notification, _x, Data} ->
%% 	    %% ct:print("Incoming ~p~n",[{notification, _x, Data}]),
%% 	    {ok, {events, _, Events}} = extract_element(events, Data),
%% 	    case parse_events('AVC', AttrName, Events) of
%% 		{match, Values} ->
%% 		    Values;
%% 		nomatch ->
%% 		    wait_for_notification('AVC', AttrName)
%% 	    end
%%     end.

%% parse_events(Type, AttrName, [{Type, _attrs, Content}|Data]) ->
%%     case parse_event_content(AttrName, Content) of
%% 	{match, Values} ->
%% 	    {match, Values};
%% 	nomatch ->
%% 	    parse_events(Type, AttrName, Data)
%%     end;
%% parse_events(Type, AttrName, [_|Data]) ->
%%     parse_events(Type, AttrName, Data);
%% parse_events(_, _, []) -> nomatch.

%% parse_event_content(AttrName, [{attr, Attrs, Values}|Content]) ->
%%     case proplists:get_value(name, Attrs) of
%% 	AttrName ->
%% 	    {match, Values};
%% 	_ ->
%% 	    parse_event_content(AttrName, Content)
%%     end;
%% parse_event_content(_, []) -> nomatch.


%% wait_for_progress(Attribute, OldActionId) ->
%%     [{v, [], RpData}] = wait_for_notification('AVC', Attribute),
%%     Report = format_struct(RpData),
%%     case proplists:get_value(actionId, Report) of
%% 	OldActionId ->
%% 	    wait_for_progress(Attribute, OldActionId);
%% 	_ ->
%% 	    case proplists:get_value(state, Report) of
%% 		["3"] ->
%% 		    Percent = proplists:get_value(progressPercentage, Report),
%% 		    %% Info = proplists:get_value(progressInfo, Report),
%% 		    %% ct:pal("==PROGRESS=============~n"
%% 		    %% 	    "State: ~s~nProgress: ~s % ready~n",
%% 		    %% 	   ["3", Percent]),
%% 		    ct:pal(["==PROGRESS=============\n"|lists:foldl(
%% 			      fun({Attr, Values}, Acc) ->
%% 				      Acc++
%% 					  [io_lib:format("~w: ~s~n",[Attr,Value])||Value<-Values]
%% 			      end, [], Report)],[]),
%% 		    proplists:get_value(result, Report);
%% 		[State] ->
%% 		    Percent = proplists:get_value(progressPercentage, Report),
%% 		    Info = proplists:get_value(progressInfo, Report),
%% 		    ct:pal("==PROGRESS=============~n"
%% 			   "State: ~s~nProgress: ~s % ready~n~s",
%% 			   [State, Percent, Info]),
%% 		    wait_for_progress(Attribute, OldActionId)
%% 	    end
%%     end.


%% format_struct([{elem, [{name, Name}], Vs}|Elements]) ->
%%     Values = case length(Vs) of
%% 		 1 ->
%% 		     [{v, [], V}] = Vs,
%% 		     V;
%% 		 _ ->
%% 		     [hd(V)||{v, [], V}<-Vs]
%% 	     end,
%%     [{list_to_atom(Name), Values}|
%%      format_struct(Elements)];
%% format_struct([]) ->
%%     [].


%%%--------------------------------------------------------------------
%%% Description: This function and the associated check_progress_elements
%%%              loops over a netconf get for the progress information until
%%%              the state is FINISHED, otherwise it prints the status and
%%%              progress percentage
%%%--------------------------------------------------------------------


wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(5000),
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    %% ct:pal("Waiting for updated progress ~n~p~n",[Report]),
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
		    ct:log("Netconf get error:~n~p",[Error]),
		    wait_for_progress(Attribute, OldActionId, ProgressFilter)
	    end;
	{error, Reason} ->
	    ct:log("Netconf open error:~n~p",[Reason]),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    case extract_element(Attribute, ProgressReport) of
	{ok, Report} ->
	    case extract_element(state, [Report]) of
		{ok, {state, _, ["FINISHED"]}} ->
		    format_progress_report(Report),
		    ct:pal("~s",[format_progress_report(Report)]),
		    {ok, {result, _, Result}} = extract_element(result, [Report]),
		    {ok, Result};
		{ok, {state, _, [Current]}} ->
		    {ok, {progressPercentage, _, [Percent]}} =
			extract_element(progressPercentage, [Report]),
		    {ok, {progressInfo, _, [Info]}} =
			extract_element(progressInfo, [Report]),
		    ct:pal("State: ~s ~s % ready~n~s",[Current, Percent, Info]),
		    loop;
		not_found ->
		    loop
	    end;
	not_found ->
	    loop
    end.


%%%--------------------------------------------------------------------
%%% Description: Format a AsyncActionStruct for printing
%%%--------------------------------------------------------------------


format_progress_report({progressReport, _, Members}) ->
    [io_lib:format("progressReport:~n",[])|format_progress_report(Members)];
format_progress_report({progress, _, Members}) ->
    [io_lib:format("progressReport:~n",[])|format_progress_report(Members)];
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


%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cli_users_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R5A/R7A/1
%%%
%%% @doc == Several users using cli operations.==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%%
%%% @end

-module(cli_users_SUITE).

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2013-01-30 etxivri     Created
%%% R2A/3      2013-01-31 etxivri     Some updates
%%% R2A/4      2013-02-08 etxivri     Added cli_users_add_delete_rbsUnit_paralell,
%%%                                   updates in clean up if tc fail.
%%% R2A/5      2013-02-08 etxivri     Added noprint when TC fail in ent per tc.
%%%                                   Some minor updates to make tc more robust.
%%% R2A/6      2013-02-12 etxivri     Updated tc for new idle timeout 5 min.
%%%                                   timeout is defined in/rcs/comte/libcom_cli_agent.cfg !
%%% R2A/7      2013-02-18 etxivri     Updated open_connection_after_restart to take take care
%%%                                   if COM is not started or restarts.
%%% R2A/8      2013-03-01 etxivri     Updated cleanup in end_per_testcase.
%%% R2A/9      2013-03-08 etxivri     Added rct_core hook.
%%% R2A/10     2013-04-17 etxivri     Fixed a timing problem in idle_timout..
%%% R2A/11     2013-05-24 etxivri     Updated tc for new idle timeout 2 min.
%%%                                   Decreased nr of netconf sessions in init_restar TC,
%%%                                   Due to problem in OTP. They take down ssh sessions in sequense
%%%                                   that take 4 sec each.
%%%                                   Added $USER in "pgrep".
%%% R2A/12     2013-06-25 etxivri     Decreased Nr of cli users and increased some timeouts.
%%% R2A/13     2013-06-27 etxivri     Changed timetrap to 30 min.
%%% R2A/14     2013-08-07 etxivri     Simplified check of created RbsUnit.
%%% R2A/15     2013-08-08 etxivri     Changed back the check of created RbsUnit as it was in R2A/13,
%%%                                   and made it work to changed behavior.
%%%                                   Updates of not_commited check.
%%% R2A/16     2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/17     2013-08-27 etxivri     added a filter when kill com is used.
%%% R2A/18     2013-08-27 etxivri     Minor update.
%%% R2A/19     2013-09-02 etxivri     Changed filtered string in ERROR hook.
%%% R2A/20     2013-09-05 etxivri     Changed to a better pgrep com.
%%% R2A/21     2013-09-16 etxivri     Added kill com TC to all.
%%% R2A/22     2013-09-16 etxivri     Added init:reboot TC.
%%% R2A/23     2013-10-01 etxivri     Changed cli command end to top, due to changes in com bd8.
%%% R2A/24     2013-10-07 erarafo     Added testcase for CLI Extensions
%%% R2A/25     2013-10-09 etxivri     Update to make init:reboot more robust.
%%% R2A/26     2013-10-15 etxivri     Update open_connection_after_restart.
%%% R2A/27     2013-10-15 etxivri     More Update of open_connection_after_restart.
%%% R2A/28     2013-10-21 erarafo     Adjusted testcase for CLI Extensions
%%% R2A/28     2013-10-22 etxivri     increased timout for some of the rct_rpc:call.
%%% R3A/1      2015-05-25 etxivri     Remove compile warning.
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warnings and copyright
%%% R5A/2      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R7A/1      2016-09-08 etxivir     Update to use rand instead for random.
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 cli_sessions_idle_timeout/1,
	 cli_users_paralell_show_req/1,
	 cli_users_random_show_req/1,
	 cli_users_exercise_cli_extension/1,
	 cli_users_add_rbsUnit/1,
	 cli_users_add_rbsUnit_commit/1,
	 cli_users_add_rbsUnit_commit_reversed_order/1,
	 cli_users_add_rbsUnit_paralell/1,
	 cli_users_add_delete_rbsUnit_paralell/1,
	 cli_users_add_rbsUnit_paralell_commit/1,
	 cli_users_add_rbsUnit_restart/1,
	 cli_users_add_rbsUnit_reboot/1,
	 cli_users_add_rbsUnit_init_reboot/1,
	 cli_users_add_rbsUnit_power/1,
	 cli_users_add_rbsUnit_kill_com/1
	]).

%% -define(NrOfCliUsers, 1).
-define(NrOfCliUsers, 20).
%% -define(NrOfCliUsers, 50).

-define(CLI_SessionNameList, [list_to_atom("cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_cli hook for each user, to be able to use cli for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of CliHooks touples with differents Names.
    CliHooks = [{rct_cli, {list_to_atom("cli"++integer_to_list(N)), [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)] ,
    %% ct:pal("CliHooks: ~p",[CliHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_core,[]},
		 %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} |
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       ["The job was brutally killed - exiting"]}}]}} |
		 CliHooks
     		]}].

%% @hidden
init_per_suite(Config) ->
    Config.
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
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added rbs units", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"], 10000, noprint),
	    BB = length(string:tokens(B,"\n")),
	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[BB]),

	    %%%%
	    %% Open a connection and clean up rbsUnits.
            %%%%
	    try
	    	lists:foreach(fun(Name) -> cli_connect(rct_cli:connect(Name, noprint), Name) end, ?CLI_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

	    %%%%
	    %% Close connection
	    %%%%
	    lists:foreach(fun(Name1) ->
	    			  %% ct:pal("### Cleanup after fail TC, CloseName: ~p", [Name1]),
	    			  rct_cli:disconnect(Name1, noprint)
	    		  end,
	    		  ?CLI_SessionNameList)

    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),

    ok.

cli_connect({error,already_connected}, Name) ->
    rct_cli:disconnect(Name, noprint), % Clean up at our client, if cli process was killed on node,
    rct_cli:connect(Name, noprint), % Then set up again.
    cli_delete_rbsUnits(Name, ?CLI_SessionNameList),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
    cli_delete_rbsUnits(Name, ?CLI_SessionNameList),
    throw({?MODULE, found});
cli_connect(_, _) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
	cli_sessions_idle_timeout,
	cli_users_paralell_show_req,
	cli_users_random_show_req,
	cli_users_exercise_cli_extension,
	cli_users_add_rbsUnit,
	cli_users_add_rbsUnit_commit,
	cli_users_add_rbsUnit_commit_reversed_order,
	cli_users_add_rbsUnit_paralell,
	cli_users_add_delete_rbsUnit_paralell,
	cli_users_add_rbsUnit_paralell_commit,
	cli_users_add_rbsUnit_restart,
	cli_users_add_rbsUnit_init_reboot,
	cli_users_add_rbsUnit_reboot,
	cli_users_add_rbsUnit_power,
	cli_users_add_rbsUnit_kill_com
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].


%%--------------------------------------------------------------------
%% @doc
%% Verify that idle cli connections will be closed after 2min if no data has been sent.  <br/>
%% @spec cli_sessions_idle_timeout(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_sessions_idle_timeout(_Config) ->
    ct:pal("### Check that cli connections is closed after 2min, when idle!",[]),

    Start = erlang:timestamp(),
    %%%%
    %% Create cli connection.
    %%%%
    {UsedSessionNames,_} = lists:split(10, ?CLI_SessionNameList),
    cli_create_connection(UsedSessionNames),

    ct:pal("### 10 Cli connections created, wait until 1min has passed.",[]),

    End = erlang:timestamp(),
    SetUpTimeSec = trunc(timer:now_diff(End, Start) / 1000 / 1000),

    wait_to_1min_to_pass(60-SetUpTimeSec),
    ct:pal("### 1 min has passed,  Check that cli connections still exist.", []),

    %%%%
    %% Check connections still exist on node.
    %%%%
    CliPids = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"], 10000, noprint),
    %% Build a List with Cli Pids, remove \n from the received list.
    CliPidList = string:tokens(CliPids, "\n"),
    %% ct:pal("CliPidList: ~p",[CliPidList]),
    case length(CliPidList) of
	10 ->
	    ok;
	Nr ->
	    ct:fail("TC fail: the number of existing Cli Pids is not expected! "++integer_to_list(Nr))
    end,

    ct:pal("### sleep 1 min and 30 sec.",[]),
    ct:sleep({seconds, 90}),
    ct:pal("### sleep 1 min and 30 sec has passed. Cli connections shall be closed",[]),

    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"], 10000, noprint),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(UsedSessionNames),

    ok.

wait_to_1min_to_pass(Timeout) when Timeout < 1 ->
    ok;

wait_to_1min_to_pass(Timeout) ->
    %% ct:pal("Timeout: ~p",[Timeout]),
    ct:sleep({seconds, 10}),
    wait_to_1min_to_pass(Timeout-10).


%%--------------------------------------------------------------------
%% @doc
%% Check several cli users do paralell request on show configuration request. <br/>
%% @spec cli_users_paralell_show_req(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_paralell_show_req(_Config) ->
    ct:pal("### Check several cli users do paralell request on show configuration!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% sen show configuration req.
    %%%%
    Self = self(),
    PidList = lists:map(fun(Name) ->
				    spawn(fun() -> ct:pal("### Cli User: ~p , send: show configuraton", [Name]),
						  rct_cli:send(Name, "show configuration"),
						   Self ! ok
					  end)
			    end,
			    ?CLI_SessionNameList),

    %%%%
    %% Wait on answer from add  request.
    %%%%
    lists:foreach(fun(_Pid) ->
			 receive
    			      ok ->
				 ok
			 after 60000 ->
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several cli users do random request on show configuration. <br/>
%% @spec cli_users_random_show_req(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_random_show_req(_Config) ->
    ct:pal("### Check several cli users do random request on show all!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Send cli request "show all" on random connections.
    %%%%
    lists:foreach(fun(N) ->
			  RandomNr = rand:uniform(?NrOfCliUsers),
    			  Name = list_to_atom("cli"++integer_to_list(RandomNr)),
			  ct:pal("### Nr: ~p, Show configuartion with Name: ~p", [N, Name]),
			  {ok ,_} = rct_cli:send(Name, "show configuration")
    		  end,
    		  lists:seq(1,?NrOfCliUsers)),

    %%%%
    %% Close Connections
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Let several CLI users execute CLI Extension commands in parallel. <br/>
%% @end
%%--------------------------------------------------------------------

-spec cli_users_exercise_cli_extension(any()) -> ok.

cli_users_exercise_cli_extension(_Config) ->
    ct:pal("### Let several cli users execute CLI Extension commands in parallel!",[]),

    Enumeration = lists:seq(1,?NrOfCliUsers),
    Self = self(),
    Scenario = ["ManagedElement=1,TestRoot=1", "xthis", "xthat"],
    Timeout = 60000,

    cli_create_connection(?CLI_SessionNameList),

    [begin
	 Fun = fun() ->
		       [begin
			    ct:pal("### CLI User: ~w, send: ~s", [Name, Command]),
			    Response = rct_cli:send(Name, Command),
			    ct:pal("### CLI User: ~w, result: ~p", [Name, Response]),
			    Pause = 5 + 7 * rand:uniform(11),
			    timer:sleep(Pause)
			end
			|| Command <- Scenario],
		       Self ! ok
	       end,
	 spawn(Fun)
     end
    || Name <- ?CLI_SessionNameList],

    [receive
	 ok ->
	     ok
     after Timeout ->
	     ct:fail("Timeout after ~w ms, waiting for CLI user: ~w", [Timeout, K])
     end
    || K <- Enumeration],

    cli_remove_connection(?CLI_SessionNameList),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Cli users, add rbs units. Do commit after each rbsUnit is added. <br/>
%% @spec cli_users_add_rbsUnit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_rbsUnit(_Config) ->
    ct:pal("### Check several Cli users, add rbs units. Do commit after each rbsUnit is added!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    cli_add_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%
    %% Close connections
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Check several Cli users, add rbs units. <br/>
%% %% Do commit after all rbsUnits has been added.  <br/>
%% %% Check that not commited rbsUnits is not commited, until their are commited!.<br/>
%% %% @spec cli_users_add_rbsUnit_commit(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
cli_users_add_rbsUnit_commit(_Config) ->
    ct:pal("### Check several Cli users, add rbs units. Do commit after all rbsUnits has been added!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units, no commit.
    %%%%
    cli_add_rbsUnits_no_commit(?CLI_SessionNameList),

    %%%%
    %% Check rbs units does not exist.
    %%%%
    ct:pal("Check added rbsUnits not commited.",[]),
    ok = cli_check_added_rbsUnits_not_commited(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    ct:pal("Commit.",[]),
    lists:foreach(fun(Name) ->
			  ct:pal("### Commit: ~p", [Name]),
			  cli_commit([Name]),
			  % Check rbs unit is created.
			  ok = cli_check_rbs_units_added([Name]),
			  %% Check that ohter rbs unit is not commited.
			  %% create a list with the not commited rbsUnits.
			  NotCommitedList = lists:dropwhile(fun(X) ->
								    X=/=Name
    							    end, ?CLI_SessionNameList),
			  CompleteNotCommitedList = lists:delete(Name, NotCommitedList),
			  ok = cli_check_added_rbsUnits_not_commited(CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name]),
			  ok = rct_cli:disconnect(Name)
		  end,
    		  ?CLI_SessionNameList),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits_no_commit(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    %% cli_commit(?CLI_SessionNameList),
    lists:foreach(fun(Name1) ->
			  ct:pal("### Commit: ~p", [Name1]),
			  cli_commit([Name1]),
			  % Check rbs unit is deleted.
			  ok = cli_check_rbsUnits_deleted([Name1]),
			  %% Check that ohter rbs unit is not commited.
			  %% create a list with the not commited rbsUnits.
			  NotCommitedList = lists:dropwhile(fun(X) ->
								    X=/=Name1
    							    end, ?CLI_SessionNameList),
			  CompleteNotCommitedList = lists:delete(Name1, NotCommitedList),
			  %% rbsUnits will not be shown.
			  ok = cli_check_rbsUnits_deleted(CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name1]),
			  ok = rct_cli:disconnect(Name1)
		  end,
    		  ?CLI_SessionNameList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Check several Cli users, add rbs units.<br/>
%% %% After all rbsUnits is added, do commit in reverse order. <br/>
%% %% Check that not commited rbsUnits is not commited, until their sessions is closed.<br/>
%% %% @spec cli_users_add_rbsUnit_commit_reversed_order(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
cli_users_add_rbsUnit_commit_reversed_order(_Config) ->
    ct:pal("### Check several Cli users, add rbs units. after all rbsUnits has been added do commit in reverse order!",[]),

    Reversed_SessionNameList = lists:reverse(?CLI_SessionNameList),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units, no commit.
    %%%%
    cli_add_rbsUnits_no_commit(?CLI_SessionNameList),

    %%%%
    %% Check rbs units does not exist.
    %%%%
    ok = cli_check_added_rbsUnits_not_commited(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name) ->
			  ct:pal("### Commit: ~p", [Name]),
			  cli_commit([Name]),
			  % Check rbs unit is created.
			  ok = cli_check_rbs_units_added([Name]),
			  %% Check that ohter rbs unit is not commited.
			  %% create a list with the not commited rbsUnits.
			  NotCommitedList = lists:dropwhile(fun(X) ->
								    X=/=Name
    							    end, Reversed_SessionNameList),
			  CompleteNotCommitedList = lists:delete(Name, NotCommitedList),
			  ok = cli_check_added_rbsUnits_not_commited(CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name]),
			  ok = rct_cli:disconnect(Name)
		  end,
    		  Reversed_SessionNameList),
    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits_no_commit(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    %% cli_commit(?CLI_SessionNameList),
    lists:foreach(fun(Name1) ->
			  ct:pal("### Commit: ~p", [Name1]),
			  cli_commit([Name1]),
			  % Check rbs unit is deleted.
			  ok = cli_check_rbsUnits_deleted([Name1]),
			  %% Check that ohter rbs unit is not commited.
			  %% create a list with the not commited rbsUnits.
			  NotCommitedList = lists:dropwhile(fun(X) ->
								    X=/=Name1
    							    end, Reversed_SessionNameList),
			  CompleteNotCommitedList = lists:delete(Name1, NotCommitedList),
			  %% rbsUnits will not be shown.
			  ok = cli_check_rbsUnits_deleted(CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name1]),
			  ok = rct_cli:disconnect(Name1)
		  end,
		  Reversed_SessionNameList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several cli users, add rbs units paralell. <br/>
%% @spec cli_users_add_rbsUnit_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_rbsUnit_paralell(_Config) ->
    ct:pal("### Check several Cli users, add rbs units paralell!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    Self = self(),
    PidList_Add = lists:map(fun(Name) ->
				    spawn(fun() -> cli_add_rbsUnits([Name]),
						   Self ! add_done
					  end)
			    end,
			    ?CLI_SessionNameList),

    %%%%
    %% Wait on answer from add  request.
    %%%%
    lists:foreach(fun(_Pid) ->
			 receive
    			      add_done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time! expected: add_done!", [])
			 end
		  end, PidList_Add),

    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    PidList_Delete = lists:map(fun(Name) ->
				    spawn(fun() -> cli_delete_rbsUnits([Name]),
						   Self ! delete_done
					  end)
			    end,
			    ?CLI_SessionNameList),

    %%%%
    %% Wait on answer from add  request.
    %%%%
    lists:foreach(fun(_Pid_1) ->
			 receive
    			      delete_done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time! expected: delete_done!", [])
			 end
		  end, PidList_Delete),

    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Check rbs units is deleted
    %%%%
    cli_check_rbsUnits_deleted(?CLI_SessionNameList),
    %% [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several cli users, add, delete rbs units and commit, paralell req. <br/>
%% No rbsUnits should be created. <br/>
%% @spec cli_users_add_delete_rbsUnit_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_delete_rbsUnit_paralell(_Config) ->
    ct:pal("### Check several Cli users, add, delete rbs units paralell!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units
    %%%%
    Self = self(),
    PidList_Add = lists:map(fun(Name) ->
				    spawn(fun() -> cli_add_rbsUnits_no_commit([Name]),
						   cli_delete_rbsUnits([Name]),
						   Self ! done
					  end)
			    end,
			    ?CLI_SessionNameList),

    %%%%
    %% Wait on answer from add/delete  request.
    %%%%
    lists:foreach(fun(_Pid) ->
			 receive
    			      done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time!", [])
			 end
		  end, PidList_Add),

    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Check rbs units is deleted
    %%%%
    cli_check_rbsUnits_deleted(?CLI_SessionNameList),
    %% [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several cli users, add rbs units paralell. <br/>
%% Do commit after all rbsUnit is added. <br/>
%% @spec cli_users_add_rbsUnit_paralell_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_rbsUnit_paralell_commit(_Config) ->
    ct:pal("### Check several Cli users, add rbs units paralell. Do commit after all rbsUnit is added!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Create process that loops and waits for add, commit, delete cmds.
    %%%%
    Self = self(),
    Slave = fun(Name, Loop) ->
		    receive
			add_cmd -> cli_add_rbsUnits_no_commit([Name]),
				   Self ! add_done;
			commit_cmd -> cli_commit([Name]),
				      Self ! commit_done;
			delete_cmd -> cli_delete_rbsUnits_no_commit([Name]),
				      Self ! delete_done;
			exit_slave -> exit(normal)
		    after 120000 ->
			    ct:pal("Exit slave due to timeout",[]),
			    exit(normal)
		    end,
		    Loop(Name, Loop)
	    end,

    PidList = lists:map(fun(Name) ->
				spawn(fun() ->
					      Slave(Name, Slave)
				      end)

			end,
			?CLI_SessionNameList),

    %%%%
    %% Send Add rbsUnit aralell request
    %%%%
    lists:foreach(fun(Pid1) ->
			  Pid1 ! add_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid2) ->
			  Pid2 ! commit_cmd
		  end, PidList),

    %%%%
    %% Wait on answer from the request.
    %%%%
    lists:foreach(fun(_Pid_1) ->
			 receive
    			      add_done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time! expected: add_done!", [])
			 end
		  end, PidList),

    lists:foreach(fun(_Pid_2) ->
			 receive
    			      commit_done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time! expected: commit_done! ", [])
			 end
		  end, PidList),

    %% timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %% [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Send Delete rbs units paralell request
    %%%%
    lists:foreach(fun(Pid3) ->
			  Pid3 ! delete_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid4) ->
    			  Pid4 ! commit_cmd
    		  end, PidList),

    %%%%
    %% Wait on answer from the Delete request.
    %%%%
    lists:foreach(fun(_Pid_3) ->
			 receive
    			      delete_done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time! expected: delete_done!", [])
			 end
		  end, PidList),

    %% Wait on answer from the Commit request.
    lists:foreach(fun(_Pid_4) ->
    			 receive
    			      commit_done ->
    				 ok
    			 after 50000 ->
    				 ct:fail("Error no answer within time! expected: commit_done!", [])
    			 end
    		  end, PidList),

    %% timer:sleep(30000),
    lists:foreach(fun(Pid5) ->
    			  Pid5 ! exit_slave
    		  end, PidList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Check rbs units is deleted
    %%%%
    cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %% [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% CLI users add rbsUnits, then restart.<br/>
%% - Add rbs units, No commit.<br/>
%% - restart.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit<br/>
%% - restart again.<br/>
%% - Check rbsUnits still exist.<br/>
%% @spec cli_users_add_rbsUnit_restart(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_rbsUnit_restart(_Config) ->
    ct:pal("### Check several cli users, add rbs units. Do restart!",[]),

    %%%%
    %% Get COM pid.
    %%%%
    ComPid = wait_for_com_to_start(),
    ct:pal("ComPid: ~p",[ComPid]),

    %%%%
    %% Create cli connection.
    %%%%
    %% cli_create_connection(?CLI_SessionNameList),
    cli_create_connection(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Add rbs units, no commit
    %%%%
    cli_add_rbsUnits_no_commit(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% restart
    %%%%
    ct:pal("### restart!",[]),
    rct_rpc:call(rpc, init, restart, [], 10000),
    timer:sleep(10000),

    %%%%
    %% Wait for com.
    %%%%
    NewComPid = wait_for_com_to_restart(ComPid),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(lists:sublist(?CLI_SessionNameList, 5)),
    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Add rbs units again and commit.
    %%%%
    cli_add_rbsUnits(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% restart again
    %%%%
    ct:pal("### restart again!",[]),
    rct_rpc:call(rpc, init, restart, [], 10000),
    timer:sleep(10000),

    %%%%
    %% Wait for com.
    %%%%
    wait_for_com_to_restart(NewComPid),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(lists:sublist(?CLI_SessionNameList, 5)),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(lists:sublist(?CLI_SessionNameList, 5)),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% CLI users add rbsUnits, then init:reboot.<br/>
%% - Add rbs units, No commit.<br/>
%% - init:reboot.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit<br/>
%% - init:reboot again.<br/>
%% - Check rbsUnits still exist.<br/>
%% @spec cli_users_add_rbsUnit_init_reboot(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_rbsUnit_init_reboot(_Config) ->
    ct:pal("### Check several cli users, add rbs units. Do restart!",[]),

    %%%%
    %% Get COM pid.
    %%%%
    ComPid = wait_for_com_to_start(),
    ct:pal("ComPid: ~p",[ComPid]),

    %%%%
    %% Create cli connection.
    %%%%
    %% cli_create_connection(?CLI_SessionNameList),
    cli_create_connection(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Add rbs units, no commit
    %%%%
    cli_add_rbsUnits_no_commit(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% init:reboot
    %%%%
    ct:pal("### init:reboot !",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    %% rct_rpc:call(rpc, init, reboot, [], 10000),
    do_init_reboot(),
    wait_for_node_go_down(ErlNode),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(lists:sublist(?CLI_SessionNameList, 5)),
    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Add rbs units again and commit.
    %%%%
    cli_add_rbsUnits(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% init:reboot again
    %%%%
    ct:pal("### init:reboot again!",[]),
    net_kernel:disconnect(ErlNode),
    %% rct_rpc:call(rpc, init, reboot, [], 10000),
    do_init_reboot(),
    wait_for_node_go_down(ErlNode),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(lists:sublist(?CLI_SessionNameList, 5)),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(lists:sublist(?CLI_SessionNameList, 5)),

    ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %%  CLI users add rbsUnits, then reboot.<br/>
%% %% - Add rbs units, No commit.<br/>
%% %% - Reboot.<br/>
%% %% - Check no rbsUnits exist.<br/>
%% %% - Add rbs units, do commit<br/>
%% %% - Reboot again.<br/>
%% %% - Check rbsUnits still exist.<br/>
%% %% @spec cli_users_add_rbsUnit_reboot(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
cli_users_add_rbsUnit_reboot(_Config) ->
    ct:pal("### Check several Cli users, add rbs units. Do reboot!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units, no commit
    %%%%
    cli_add_rbsUnits_no_commit(?CLI_SessionNameList),

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),
    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%
    %% Add rbs units again and commit.
    %%%%
    cli_add_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% reboot again
    %%%%
    ct:pal("### reboot again!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    ok = ct_telnet:send(console, "reboot"),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),
    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  CLI users add rbsUnits, then power off/on.<br/>
%% - Add rbs units, No commit.<br/>
%% - Power off/on.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check rbsUnits exist or no exist.
%%   It depends if data has been written to disc before power off.<br/>
%% - Add rbs units, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check rbsUnits exist.<br/>
%% @spec cli_users_add_rbsUnit_power(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_rbsUnit_power(_Config) ->
    ct:pal("### Check several cli users, add rbs units. Do power off/on!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units, no commit
    %%%%
    cli_add_rbsUnits_no_commit(?CLI_SessionNameList),

    %%%%
    %% Power off/on
    %%%%
    ct:pal("### power off/on!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),
    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%%%%%
    %% Add RbsUnits and commit, power off directly, then data could exist in disc. depends on how many rbsUnits.
    %%%%
    %% Add rbs units again and commit.
    %%%%
    cli_add_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Power off/on again
    %%%%
    ct:pal("### power off/on again!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),
    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    check_if_rbs_units_exist_or_removed(?CLI_SessionNameList),

    %%%%
    %% Add RbsUnits and commit, power off after a few seconds so data has been written to disc.
    %%%%
    %%%%
    %% Add rbs units again
    %%%%
    cli_add_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),
    %% sleep to be sure that data exist on disc.
    timer:sleep(30000),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),
    %%%%
    %% Power off/on again
    %%%%
    ct:pal("### power off/on again!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    {ok,_} = ct_telnet:expect(console, "du1 login", [{timeout,60000}, no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),
    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  CLI users add rbsUnits, then kill_com.<br/>
%% - Add rbs units, No commit.<br/>
%% - kill com process.<br/>
%% - Check no rbsUnits exist.<br/>
%% - Add rbs units, do commit<br/>
%% - kill com process again.<br/>
%% - Check rbsUnits still exist.<br/>
%% @spec cli_users_add_rbsUnit_kill_com(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_rbsUnit_kill_com(_Config) ->
    ct:pal("### Check several Cli users, add rbs units. Kill com process!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add rbs units, no commit.
    %%%%
    cli_add_rbsUnits_no_commit(?CLI_SessionNameList),

    %%%%
    %% Get COM pid.
    %%%%
    ComPid = wait_for_com_to_start(),
    ct:pal("ComPid: ~p",[ComPid]),

    %%%%
    %% Kill Com
    %%%%
    ct:pal("Kill Com pid. ",[]),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ ComPid], 10000),

    %%%%
    %% Check that com is restarted with a new pid.
    %%%%
    NewComPid = wait_for_com_to_restart(ComPid),
    ct:pal("NewComPid: ~p",[NewComPid]),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),
    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%
    %% Add rbs units again and commit.
    %%%%
    cli_add_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Kill Com again
    %%%%
    ct:pal("Kill Com pid again. ",[]),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ NewComPid], 10000),

    %%%%
    %% Check that com is restarted with a new pid.
    %%%%
    New_ComPid = wait_for_com_to_restart(NewComPid),
    ct:pal("NewComPid: ~p",[New_ComPid]),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),
    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is added
    %%%%
    ok = cli_check_rbs_units_added(?CLI_SessionNameList),

    %%%%
    %% Delete rbs units
    %%%%
    cli_delete_rbsUnits(?CLI_SessionNameList),

    %%%%
    %% Check rbs units is deleted
    %%%%
    ok = cli_check_rbsUnits_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%% Internal functions
%%--------------------------------------------------------------------
%% @hidden
%% check_if_rbs_units_exist_or_removed(CLI_SessionNameList)
%%--------------------------------------------------------------------
check_if_rbs_units_exist_or_removed(CLI_SessionNameList) ->
    NrOfRbsUnits_afterPowerOffOn = length(rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 10000)),
    ct:log("ets: ~p", [rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 10000)]),

    %% case ?NrOfCliUsers of
    %% 	%% Val when Val < 5 -> %% RbsUnits Always deleted from disc after power off/on
    %% 	%%     ct:pal("### RbsUnits shall not exist on disc.", []),
    %% 	    %% cli_check_rbsUnits_deleted(CLI_SessionNameList);
    %% 	Val when Val < 20 -> %% This numbers of RbsUnits might result in deleted or existing on disc after power off/on
    %% 	    ct:pal("### RbsUnits could exist on disc.", []),
    %% 	    case rct_rpc:call(rpc, ets, tab2list, [rbsUnit], 10000) of
    %% 		[] ->
    %% 		    cli_check_rbsUnits_deleted(CLI_SessionNameList);
    %% 		_ ->
    %% 		    ExistingRbsUnitsList = lists:sublist(CLI_SessionNameList, NrOfRbsUnits_afterPowerOffOn),
    %% 		    ct:pal("ExistingRbsUnitsList: ~p",[ExistingRbsUnitsList]),

    %% 		    DeletedRbsUnitsList = lists:sublist(CLI_SessionNameList, NrOfRbsUnits_afterPowerOffOn+1, ?NrOfCliUsers),
    %% 		    ct:pal("DeletedRbsUnitsList: ~p",[DeletedRbsUnitsList]),

    %% 		    cli_check_rbs_units_added(ExistingRbsUnitsList),
    %% 		    cli_check_rbsUnits_deleted(DeletedRbsUnitsList),
    %% 		    %% Clean up.
    %% 		    cli_delete_rbsUnits(ExistingRbsUnitsList)
    %% 	    end;
    %% 	_ -> %% This numbers of RbsUnits always result in deleted and existing on disc after power off/on
     	    ct:pal("### Some RbsUnits should exist on disc.", []),
	    ExistingRbsUnitsList = lists:sublist(CLI_SessionNameList, NrOfRbsUnits_afterPowerOffOn),
	    ct:pal("ExistingRbsUnitsList: ~p",[ExistingRbsUnitsList]),

	    DeletedRbsUnitsList = lists:sublist(CLI_SessionNameList, NrOfRbsUnits_afterPowerOffOn+1, ?NrOfCliUsers),
	    ct:pal("DeletedRbsUnitsList: ~p",[DeletedRbsUnitsList]),

	    cli_check_rbs_units_added(ExistingRbsUnitsList),
	    cli_check_rbsUnits_deleted(DeletedRbsUnitsList),

	    %% Clean up.
	    cli_delete_rbsUnits(ExistingRbsUnitsList).
    %% end.

%% %%--------------------------------------------------------------------
%% %% @hidden
%% %% Sometimes ssh connection is up but com is not, then the cli connection is not setuped.<br/>
%% %% Sometimes ComsaServer could restart COM due to healthcheck, the the connection will be released.<br/>
%% %% open_connection_after_restart(CLI_SessionNameList)
%% %%--------------------------------------------------------------------
open_connection_after_restart(CLI_SessionNameList)->
    open_connection_after_restart(CLI_SessionNameList, 120000).
open_connection_after_restart(_CLI_SessionNameList, Timeout ) when Timeout < 500 ->
    ct:log("Timeout: ~p.", [Timeout]),
    ct:fail("Cli not ready to be used within max timeout after restart.");
open_connection_after_restart(CLI_SessionNameList, Timeout)->
    ComPid = wait_for_com_to_start(),
    ct:pal("COM pid: ~p", [ComPid]),
    ok = setup_cli_session(CLI_SessionNameList, Timeout),
    %% %% Check number of cli pids match nr of setuped cli.
    ok = check_expected_cli_pids_on_node(CLI_SessionNameList, Timeout),
    %% %% Check COM has not restarted.
    ok = check_com_is_not_restarted(ComPid, CLI_SessionNameList, Timeout).

setup_cli_session(CLI_SessionNameList, Timeout)->
    case return_from_setup_cli(CLI_SessionNameList) of
	ok ->
	    ok;
	Error ->
	    ct:log("found error from setup_cli_session: ~p. Wait and try again.",
		   [Error]),
	    timer:sleep(5000),
	    clean_up_cli_session(CLI_SessionNameList),
	    open_connection_after_restart(CLI_SessionNameList, Timeout-5000)
    end.

return_from_setup_cli([]) ->
    ok;
return_from_setup_cli([H|T]) ->
    case rct_cli:connect(H) of
	ok ->
	    return_from_setup_cli(T);
	Error ->
	    Error
    end.

check_expected_cli_pids_on_node(CLI_SessionNameList, Timeout) ->
    CliPids = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"], 10000), "\n "),
    Length = length(CLI_SessionNameList),
    case length(CliPids) of
	Length ->
	    ok;
	_L ->
	    ct:log("Length of nr cli pids: ~p, is not expected: ~p . Wait and setip again",
		   [_L, Length]),
	    timer:sleep(5000),
	    clean_up_cli_session(CLI_SessionNameList),
	    open_connection_after_restart(CLI_SessionNameList, Timeout-5000)
    end.

check_com_is_not_restarted(ComPid, CLI_SessionNameList, Timeout) ->
    NewComPid = wait_for_com_to_start(),
    case NewComPid of
	ComPid ->
	    ok; % COM has not restarted;
	_Other -> % Com has restarted , set up again.
	    ct:pal("NewCOM pid : ~p, Old COM pid: ~p. ", [NewComPid, ComPid]),
	    timer:sleep(5000),
	    clean_up_cli_session(CLI_SessionNameList),
	    open_connection_after_restart(CLI_SessionNameList, Timeout-5000)
    end.

clean_up_cli_session(CLI_SessionNameList) ->
    lists:foreach(fun(CliSession)->
			  rct_cli:disconnect(CliSession)
		  end, CLI_SessionNameList).


%%--------------------------------------------------------------------
%% @hidden
%% wait_for_com_to_restart(ComPid)
%%--------------------------------------------------------------------
wait_for_com_to_restart(ComPid)->
    wait_for_com_to_restart(ComPid, 60000).

wait_for_com_to_restart(_ComPid, Timeout) when Timeout < 3000 ->
    ct:fail("New COM process not started up within max timeout after restart.");

wait_for_com_to_restart(ComPid, Timeout)->
    NewComPid = wait_for_com_to_start(),
    %% ct:pal("NewComPid: ~p",[NewComPid]),
    case ComPid == NewComPid of
    	true ->
	    timer:sleep(2000),
	    wait_for_com_to_restart(ComPid, Timeout-2000);
    	false ->
	    timer:sleep(2000),
	    NewComPid
    end.

%%--------------------------------------------------------------------
%% @hidden
%% wait_for_com_to_start()
%%--------------------------------------------------------------------
wait_for_com_to_start() ->
    wait_for_com_to_start(90000).

wait_for_com_to_start(Timeout) when Timeout < 6000 ->
    ct:fail("COM not started within max timeout.");

wait_for_com_to_start(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 10000)  of
	[] ->
	    timer:sleep(5000),
	    wait_for_com_to_start(Timeout - 5000);
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(5000),
	    wait_for_com_to_start(Timeout - 5000);
	Com ->
	    %% remove \n from the received list.
	    [ComPid|_] = string:tokens(Com, "\n "),
	    ComPid
    end.



%% CLI
%%--------------------------------------------------------------------
%% @doc
%% cli check rbsUnits exist. <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_rbs_units_added(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->

			  RbsUnitId = atom_to_list(Name),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  rct_cli:send(Name, "show"), %% clean up the message que if before match the RecievedData.
			  {ok ,RecievedData} = rct_cli:send(Name,
							    "show ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId,
							    print),
			  %% ct:pal("~p", [RecievedData]),

			  RbsUnitsData = string:tokens(RecievedData, "\""),
			  %% ct:pal("~p", [RbsUnitsData]),
			  RbsUnitsData1 = string:tokens(lists:flatten(RbsUnitsData), "\r\n "),
			  %% ct:pal("~p", [RbsUnitsData1]),
			  %% Remove > that exist as the last element in the list.
			  CompleteRbsUnitsData = lists:delete(">", RbsUnitsData1),
			  %% ct:pal("~p", [CompleteRbsUnitsData]),

			  %%%% COM BD-8
			  %% ["show",
			  %% "ManagedElement=1,Equipment=1,RbsUnit=cli1",
			  %% "RbsUnit=cli1",
			  %% "administrativeState=LOCKED",
			  %% "availState=NO_STATUS",
			  %% "hwInstallStatus=UNKNOWN",
			  %% "operationalState=DISABLED",
			  %% "userLabel=userLabel_cli1",
			  %% "detectedProductData=",
			  %% "productDesignation=",
			  %% "productRevision=",
			  %% "faultLed",
			  %% "color=RED",
			  %% "ledStatus=OFF",
			  %% "supported=true",
			  %% "maintenanceLed",
			  %% "color=BLUE",
			  %% "ledStatus=OFF",
			  %% "supported=true",
			  %% "operationalLed",
			  %% "color=GREEN",
			  %% "ledStatus=OFF",
			  %% "supported=true",
			  %% "statusLed",
			  %% "color=YELLOW",
			  %% "ledStatus=OFF",
			  %% "supported=true"]

			  [_, _,"RbsUnit="++RbsUnitId,_,_,_,_,
			   "userLabel="++UserLabel,_,_,_,_,
			   _,_,_,_,_,_,_,_,_,_,
			   _,_,_,_,_] = CompleteRbsUnitsData
		  end,
		  CLI_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% CLI struff
%%--------------------------------------------------------------------
%% @doc
%% cli create connection. <br/>
%% @end
%%--------------------------------------------------------------------
cli_create_connection(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### CLI Connect Name: ~p", [Name]),
			  ok = rct_cli:connect(Name)
			  %% ok = rct_cli:connect(Name, "expert", "expert", "[***]\r\n>$")
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli add rbsUnits. <br/>
%% @end
%%--------------------------------------------------------------------
cli_add_rbsUnits(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  ct:pal("### Add RBS unit: ~p with userlable: ~p.", [Name, UserLabel]),
			  rct_cli:send(Name, "configure"),
			  rct_cli:send(Name, "ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId),
			  rct_cli:send(Name, "userLabel="++UserLabel),
			  rct_cli:send(Name, "commit"),
			  rct_cli:send(Name, "top")
		  end,
		  CLI_SessionNameList).


cli_add_rbsUnits_no_commit(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  ct:pal("### Add RBS unit: ~p with userlable: ~p. No commit", [Name, UserLabel]),
			  rct_cli:send(Name, "configure"),
			  rct_cli:send(Name, "ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId),
			  rct_cli:send(Name, "userLabel="++UserLabel),
			  %% rct_cli:send(Name, "exit"),
			  %% rct_cli:send(Name, "exit"),
			  %% rct_cli:send(Name, "exit")
			      rct_cli:send(Name, "top")
		  end,
		  CLI_SessionNameList).


%%--------------------------------------------------------------------
%% @doc
%% cli delete rbsUnits. <br/>
%% @end
%%--------------------------------------------------------------------
cli_delete_rbsUnits(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  ct:pal("### Delete RBS units: ~p", [Name]),
			  rct_cli:send(Name, "configure"),
			  rct_cli:send(Name, "ManagedElement=1,Equipment=1"),
			  rct_cli:send(Name, "no RbsUnit="++RbsUnitId),
			  rct_cli:send(Name, "commit"),
			  rct_cli:send(Name, "top")
		  end,
		  CLI_SessionNameList).

cli_delete_rbsUnits(SessionName, CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  rct_cli:send(SessionName, "configure", noprint),
			  rct_cli:send(SessionName, "ManagedElement=1,Equipment=1", noprint),
			  rct_cli:send(SessionName, "no RbsUnit="++RbsUnitId, noprint),
			  rct_cli:send(SessionName, "commit", noprint),
			  rct_cli:send(SessionName, "top", noprint)
		  end,
		  CLI_SessionNameList).

cli_delete_rbsUnits_no_commit(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  ct:pal("### Delete RBS units: ~p, No commit", [Name]),
			  rct_cli:send(Name, "configure"),
			  rct_cli:send(Name, "ManagedElement=1,Equipment=1"),
			  rct_cli:send(Name, "no RbsUnit="++RbsUnitId),
			  %% rct_cli:send(Name, "exit"),
			  %% rct_cli:send(Name, "exit")
			  rct_cli:send(Name, "top")
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli check rbsUnits does not exist. <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_rbsUnits_deleted(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  %% ct:pal("### Check RBS unit: ~p deleted", [Name]),
			  {ok ,RecievedData} = rct_cli:send(Name, "show ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId),
			  %% ct:pal("~p",  [RecievedData]),
			  case re:run(RecievedData, "ERROR: Specific element not found") of
			      {match, _} ->
				  ok;
			      nomatch ->
				  ct:pal("~p",  [RecievedData]),
				  ct:fail("TC fail due to rbsUnit not deleted!")
			  end
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli check rbsUnits is not commited. <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_added_rbsUnits_not_commited(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  RbsUnitId = atom_to_list(Name),
			  UserLabel = "userLabel_" ++ RbsUnitId,
			  {ok ,RecievedData} = rct_cli:send(Name, "show ManagedElement=1,Equipment=1,RbsUnit="++RbsUnitId),

			  RbsUnitsData = string:tokens(RecievedData, "\""),
			  RbsUnitsData1 = string:tokens(lists:flatten(RbsUnitsData), "\r\n "),
			  %% Remove (config)> that exist as the last element in the list.
			  CompleteRbsUnitsData = lists:delete("(config)>", RbsUnitsData1),
			  %% ct:pal("~p", [CompleteRbsUnitsData]),

			  %%%% COM BD-8
			  %% ["show","ManagedElement=1,Equipment=1,RbsUnit=cli19","RbsUnit=cli19",
			  %%  "userLabel=userLabel_cli19"]
			  [_,
			   _,
			   "RbsUnit="++RbsUnitId,
			   "userLabel="++UserLabel
			   ] = CompleteRbsUnitsData
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli remove connection. <br/>
%% @end
%%--------------------------------------------------------------------
cli_remove_connection(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### Disconnect: ~p", [Name]),
			  ok = rct_cli:disconnect(Name)
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% cli commit request. <br/>
%% @end
%%--------------------------------------------------------------------
cli_commit(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### Commit: ~p", [Name]),
			  %% rct_cli:send(Name, "configure"),
			  rct_cli:send(Name, "commit"),
			  rct_cli:send(Name, "top")
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% wait_for_node_go_down. <br/>
%% @end
%%--------------------------------------------------------------------
wait_for_node_go_down(ErlNode) ->
    wait_for_node_go_down(ErlNode, 60000).
wait_for_node_go_down(_ErlNode, Timeout) when Timeout < 500 ->
    ct:fail("Node has not gone down !");
wait_for_node_go_down(ErlNode, Timeout) ->
        case net_adm:ping(ErlNode) of
	pang ->
	    ok;
	_Res ->
	    %% ct:pal("Ping res: ~p", [Res]),
	    timer:sleep(3000),
	    wait_for_node_go_down(ErlNode, Timeout-3000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% do_init_reboot. <br/>
%% @end
%%--------------------------------------------------------------------
do_init_reboot() ->
    do_init_reboot(3).
do_init_reboot(0) ->
    ct:fail("Node has not rebooted, tried 3 times!!");
do_init_reboot(Nr) ->
    case rct_rpc:call(rpc, init, reboot, [], 10000) of
	ok ->
	    ok;
	{badrpc,nodedown} ->
	    ct:pal("init:reboot() did not take effect! try again",[]),
	    timer:sleep(2000),
	    do_init_reboot(Nr-1)
    end.

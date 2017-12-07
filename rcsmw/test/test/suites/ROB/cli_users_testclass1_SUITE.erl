%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cli_users_testclass1_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R8A/R10A/1
%%%
%%% @doc == Several users using cli operations.==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%%
%%% @end

-module(cli_users_testclass1_SUITE).
%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2013-11-14 etxivri     Created, Ported TC from cli_users_SUITE
%%%                                   Due to model in FEQM shall be removed.
%%% R2A/3      2013-11-20 etxivri     Update to make power tc more robust.
%%% R2A/4      2013-12-04 etxivri     Chnged some ct:pal to ct:log.
%%% R2A/5      2014-02-25 etxivri     Update due to idle timeout changed
%%%                                   to 15 min
%%% R2A/6      2014-02-27 etxivri     Added synch in cli_users_add_testclass1_commit.
%%% R2A/7      2014-06-04 etxivri     Changed check for "du1 login" prompt to
%%%                                   "login:"
%%% R2A/8      2014-07-02 etxivri     fix a compile warning and make it mor robust.
%%% R2A/9      2014-07-08 etxivri     Update due to limit of nr off ssh session
%%%                                   is set to 5 at same time.
%%% R2A/10     2014-07-10 etxivri     minor update.
%%% R2A/11     2014-10-15 etxivri     Update due to new behaviour.
%%% R3A/1      2014-10-22 etxivri     Update to make it more robust.
%%% R3A/2      2015-01-28 etxivri     Update error filter
%%% R3A/3      2015-01-29 etxivri     Make a login check more robust.
%%% R3A/4      2015-02-10 etxivri     Make login check more robust.
%%% R3A/5      2015-02-11 etxivri     Remove error in ct shell.
%%% R3A/6      2015-04-23 etxivri     Make sync cmd more robust.
%%% R4A/1      2015-07-07 etxivri     Skip tc cli_users_exercise_cli_extension
%%%                                   until it test_cmd_module.c is updatet to
%%%                                   use new COM API.
%%% R4A/2      2015-09-03 etxivri     Now it is ok to run
%%%                                   cli_users_exercise_cli_extension
%%% R4A/3      2015-09-08 etxivri     cli_users_exercise_cli_extension still
%%%                                   does not work, Error printouts in ct shell
%%% R4A/4      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp()
%%% R4A/5      2015-10-20 etxivri    Add sleep 35 sec in restart tc due to new
%%%                                  behaviour when write conf to disc.
%%% R4A/6      2015-10-20 etxivri    Update in power tc.
%%% R4A/7      2015-10-20 etxivri    Forgot to remove a break.
%%% R4A/8      2015-10-23 etxivri    Remove init restart test to be used
%%% R5A/1      2016-02-03 etxivri    Update in power tc to be more robust.
%%% R5A/2      2016-02-05 etxivri    Update to restart using appm instead for init:.
%%% R5A/3      2016-02-19 etxkols    Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19.
%%% R10A/1     2017-06-29 etxivri    Update reboot cmd.
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
	 cli_users_add_testclass1/1,
	 cli_users_add_testclass1_commit/1,
	 cli_users_add_testclass1_commit_reversed_order/1,
	 cli_users_add_testclass1_paralell/1,
	 cli_users_add_delete_testclass1_paralell/1,
	 cli_users_add_testclass1_paralell_commit/1,
	 cli_users_add_testclass1_restart/1,
	 cli_users_add_testclass1_reboot/1,
	 cli_users_add_testclass1_init_reboot/1,
	 cli_users_add_testclass1_power/1,
	 cli_users_add_testclass1_kill_com/1
	]).

-define(NrOfCliUsers, 5).  %% One session is check_session.
%% -define(NrOfCliUsers, 20).
%% %% -define(NrOfCliUsers, 50).

-define(CLI_SessionNameList,
	[list_to_atom(
	   "cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(ATTR, "12345").
-define(PRINT_OPT, print).
%% -define(CHECK_SESSION, check_session).

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
		 %% {rct_cli, {check_session, [manual_connect]}},
		 {rct_core,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       ["The job was brutally killed - "
						"exiting",
						"sa_ais_err_exist"]}}]}} |
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
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added instances",
		   [Reason]),
	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"],
			     10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"],
			     10000, noprint),
	    BB = length(string:tokens(B,"\n")),
	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[BB]),

	    %%%%
	    %% Open a connection and clean up instances.
            %%%%
	    try
	    	lists:foreach(fun(Name) ->
				      cli_connect(rct_cli:connect(Name, noprint)
						 , Name)
			      end, ?CLI_SessionNameList)
	    catch throw:{?MODULE, found} ->
	    	    ok
	    end,

	    %%%%
	    %% Close connection
	    %%%%
	    lists:foreach(fun(Name1) ->
	    			  rct_cli:disconnect(Name1, noprint)
	    		  end,
	    		  ?CLI_SessionNameList)

    end,

    sync(),

    ok.

cli_connect({error,already_connected}, Name) ->
    % Clean up at our client, if cli process was killed on node,
    rct_cli:disconnect(Name, noprint),
    % Then set up again.
    rct_cli:connect(Name, noprint),
    cli_delete_instances(Name, ?CLI_SessionNameList),
    throw({?MODULE, found});
cli_connect(ok, Name) ->
    cli_delete_instances(Name, ?CLI_SessionNameList),
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
     %% cli_users_exercise_cli_extension,
     cli_users_add_testclass1,
     cli_users_add_testclass1_commit,
     cli_users_add_testclass1_commit_reversed_order,
     cli_users_add_testclass1_paralell,
     cli_users_add_delete_testclass1_paralell,
     cli_users_add_testclass1_paralell_commit,
     %% cli_users_add_testclass1_restart,
     cli_users_add_testclass1_reboot,
     cli_users_add_testclass1_init_reboot,
     cli_users_add_testclass1_power,
     cli_users_add_testclass1_kill_com
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
%% Verify that idle cli connections will be closed after 15min <br/>
%% if no data has been sent.  <br/>
%% @spec cli_sessions_idle_timeout(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_sessions_idle_timeout(_Config) ->
    ct:pal("### Check that cli connections is closed after 15min, "
	   "when idle!",[]),

    Start = os:timestamp(),
    %%%%
    %% Create cli connection.
    %%%%
    {UsedSessionNames,_} = lists:split(5, ?CLI_SessionNameList),
    cli_create_connection(UsedSessionNames),

    ct:pal("### 10 Cli connections created, wait until 1min has passed.",[]),
    ct:pal("### All cli sessions created.",[]),
    ct:pal("### sleep 14 min. Check that sessions still exist",[]),
    %% ct:sleep({minutes, 14}), %% this is not a thorough check!

    End = os:timestamp(),
    SetUpTimeSec = trunc(timer:now_diff(End, Start) / 1000 / 1000),

    %% wait_to_1min_to_pass(60-SetUpTimeSec),
    wait_to_min_to_pass(840-SetUpTimeSec), %% 14min
    ct:pal("### 14 min has passed,  Check that cli connections still exist.",
	   []),

    %%%%
    %% Check connections still exist on node.
    %%%%
    CliPids = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"], 10000, noprint),
    %% Build a List with Cli Pids, remove \n from the received list.
    CliPidList = string:tokens(CliPids, "\n"),
    %% ct:pal("CliPidList: ~p",[CliPidList]),
    case length(CliPidList) of
	5 ->
	    ok;
	Nr ->
	    ct:fail("TC fail: the number of existing Cli Pids is "
		    "not expected! "++integer_to_list(Nr))
    end,

    ct:pal("### sleep 1 min and 30 sec.",[]),
    ct:sleep({seconds, 90}),
    ct:pal("### sleep 1 min and 30 sec has passed. Cli connections "
	   "shall be closed",[]),

    ct:pal("### Check no cli prcess exist on node.",[]),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"], 10000, noprint),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    ct:pal("### Clean up cli session on CT node.",[]),
    cli_remove_connection(UsedSessionNames),

    ok.

wait_to_min_to_pass(Timeout) when Timeout < 1 ->
    ok;

wait_to_min_to_pass(Timeout) ->
    %% ct:pal("Timeout: ~p",[Timeout]),
    ct:sleep({seconds, 10}),
    %% ct:pal("Remaining time for 14min to pass, in sec: ~p",[Timeout-10]),
    wait_to_min_to_pass(Timeout-10).


%%--------------------------------------------------------------------
%% @doc
%% Check several cli users do paralell request on show  <br/>
%% configuration request. <br/>
%% @spec cli_users_paralell_show_req(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_paralell_show_req(_Config) ->
    ct:pal("### Check several cli users do paralell request on "
	   "show configuration!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% sen show configuration req.
    %%%%
    Self = self(),
    PidList =
	lists:map(fun(Name) ->
			  spawn(fun() ->
					ct:pal("### Cli User: ~p, "
					       "send: show configuraton",
					       [Name]),
					rct_cli:send(Name,"show configuration"),
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
			  ct:pal("### Nr: ~p, Show configuartion with Name: ~p",
				 [N, Name]),
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
    ct:pal("### Let several cli users execute CLI Extension commands in "
	   "parallel!",[]),

    Enumeration = lists:seq(1,?NrOfCliUsers),
    Self = self(),
    Scenario = ["ManagedElement=1,TestRoot=1", "xthis", "xthat"],
    Timeout = 60000,

    cli_create_connection(?CLI_SessionNameList),

    [begin
	 Fun = fun() ->
		       [begin
			    ct:pal("### CLI User: ~w, send: ~s", [Name,
								  Command]),
			    Response = rct_cli:send(Name, Command),
			    ct:pal("### CLI User: ~w, result: ~p", [Name,
								    Response]),
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
	     ct:fail("Timeout after ~w ms, waiting for CLI user: ~w", [Timeout,
								       K])
     end
     || K <- Enumeration],

    cli_remove_connection(?CLI_SessionNameList),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Check several Cli users, add instance. Do commit after each TestClass1
%% is added. <br/>
%% @spec cli_users_add_testclass1(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1(_Config) ->
    ct:pal("### Several Cli users, add testclass1 instance. "
	   "Do commit after each instance is added!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances.
    %%%%
    cli_add_testclass1(?CLI_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Close connections
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several Cli users, add TestClass1. <br/>
%% Do commit after all instances has been added.  <br/>
%% Check that not commited instances is not commited, <br/>
%% until their are commited!.<br/>
%% @spec cli_users_add_testclass1_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1_commit(_Config) ->
    ct:pal("### Check several Cli users, add testclass1 instances. "
	   "Do commit after all instance has been added!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances, no commit.
    %%%%
    ok = cli_add_testclass1_no_commit(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    ct:pal("Commit.",[]),
    lists:foreach(fun(Name) ->
			  %% ct:pal("### Commit: ~p", [Name]),
			  cli_commit([Name]),
			  % Check instances is created.
			  ok = cli_check_testclass1_added([Name]),
			  %% Check that ohter instances is not commited.
			  %% create a list with the not commited instances.
			  NotCommitedList =
			      lists:dropwhile(fun(X) ->
						      X=/=Name
					      end,
					      ?CLI_SessionNameList),
			  _CompleteNotCommitedList =
			      lists:delete(Name, NotCommitedList),
			  %% ok = cli_check_added_testclass1_not_exist(
			  %% 	 ?CHECK_SESSION, CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name]),
			  ok = rct_cli:disconnect(Name)
		  end,
    		  ?CLI_SessionNameList),

    sync(),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances, no commit
    %%%%
    ok = cli_delete_testclass1_no_commit(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name1) ->
			  %% ct:pal("### Commit: ~p", [Name1]),
			  cli_commit([Name1]),
			  % Check instances is deleted.
			  ok = cli_check_testclass1_deleted([Name1]),
			  %% Check that ohter instances is not commited.
			  %% create a list with the not commited instances.
			  NotCommitedList =
			      lists:dropwhile(fun(Y) ->
						      Y=/=Name1
					      end,
					      ?CLI_SessionNameList),
			  _CompleteNotCommitedList =
			      lists:delete(Name1, NotCommitedList),
			  %% %% instances will not be shown.
			  %% ok = cli_check_testclass1_exist(
			  %% 	 ?CHECK_SESSION, CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name1]),
			  ok = rct_cli:disconnect(Name1)
		  end,
    		  ?CLI_SessionNameList),

    sync(),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Check several Cli users, add TestClass1 instances.<br/>
%% %% After all instances is added, do commit in reverse order. <br/>
%% %% Check that not commited instances is not commited, <br/>
%%  until their sessions is closed.<br/>
%% %% @spec cli_users_add_testclass1_commit_reversed_order(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
cli_users_add_testclass1_commit_reversed_order(_Config) ->
    ct:pal("### Check several Cli users, add tesclass1 instances. "
	   "after all instance has been added do commit in reverse order!",[]),

    Reversed_SessionNameList = lists:reverse(?CLI_SessionNameList),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances, no commit.
    %%%%
    ok = cli_add_testclass1_no_commit(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    lists:foreach(fun(Name) ->
			  %% ct:pal("### Commit: ~p", [Name]),
			  cli_commit([Name]),
			  % Check instances is created.
			  ok = cli_check_testclass1_added([Name]),
			  %% Check that ohter instances is not commited.
			  %% create a list with the not commited instances.
			  NotCommitedList =
			      lists:dropwhile(fun(X) ->
						      X=/=Name
					      end, Reversed_SessionNameList),
			  CompleteNotCommitedList =
			      lists:delete(Name, NotCommitedList),
			  %% ok = cli_check_added_testclass1_not_exist(
			  %% 	 ?CHECK_SESSION, CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name]),
			  ok = rct_cli:disconnect(Name),
			  show_to_avoid_idle_timeout(CompleteNotCommitedList)
		  end,
    		  Reversed_SessionNameList),
    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances, no commit
    %%%%
    ok = cli_delete_testclass1_no_commit(?CLI_SessionNameList),

    %%%%
    %% Commit
    %%%%
    %% cli_commit(?CLI_SessionNameList),
    lists:foreach(fun(Name1) ->
			  %% ct:pal("### Commit: ~p", [Name1]),
			  cli_commit([Name1]),
			  % Check instances is deleted.
			  ok = cli_check_testclass1_deleted([Name1]),
			  %% Check that ohter instances is not commited.
			  %% create a list with the not commited instances.
			  NotCommitedList =
			      lists:dropwhile(fun(X) ->
						      X=/=Name1
					      end, Reversed_SessionNameList),
			  CompleteNotCommitedList =
			      lists:delete(Name1, NotCommitedList),
			  %% %% instances will  be shown.
			  %% ok = cli_check_testclass1_exist(
			  %% 	 ?CHECK_SESSION, CompleteNotCommitedList),
			  ct:pal("### Disconnect: ~p", [Name1]),
			  ok = rct_cli:disconnect(Name1),
			  show_to_avoid_idle_timeout(CompleteNotCommitedList)
		  end,
		  Reversed_SessionNameList),

    sync(),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several cli users, add TestClass1 paralell. <br/>
%% Note that there could be conflicts if operations is exct on same time.<br/>
%% Then operation could fail due to another is doing opeartion.<br/>
%% @spec cli_users_add_testclass1_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1_paralell(_Config) ->
    ct:pal("### Check several Cli users, add testclass1 instances "
	   "paralell!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances
    %%%%
    Self = self(),
    PidList_Add =
	lists:map(
	  fun(Name) ->
		  spawn(fun() ->
				ct:pal(" Create testclass1: ~p", [Name]),
				case cli_add_paralell_check_commit(Name) of
				    error ->
					Self ! add_done;
				    SessName ->
					Self ! {add_done, SessName}
				end
			end)
	  end,
	  ?CLI_SessionNameList),

    %%%%
    %% Wait on answer from add  request.
    %%%%
    Sess_Add = lists:map(fun(_Pid) ->
				 receive
				     add_done ->
					 error;
				     {add_done, SessName} ->
					 SessName
				 after 50000 ->
					 ct:fail("Error no answer within time! "
						 "expected: add_done!", [])
				 end
			 end, PidList_Add),

    %% Create a list of sessions there MO instance where created.
    %% Filter out error from list.
    %% error could be returned if operation is same time.
    CreatedInst = lists:filter(fun(X) ->
				       case X of
					   error -> false;
					   _ -> true
				       end
			       end, Sess_Add),
    ct:pal("Created Instances : ~p",[CreatedInst]),

    lists:foreach(fun(PidA) ->
   			  exit(PidA, kill)
    		  end, PidList_Add),

    %% timer:sleep(30000),
    sync(),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(CreatedInst),

    %%%%
    %% Delete instance
    %%%%
    PidList_Delete =
	lists:map(
	  fun(Name1) ->
		  spawn(fun() ->
				ct:pal(" Delete testclass1: ~p", [Name1]),
				case cli_check_commit_after_del(
				       Name1) of
				    error ->
					Self ! delete_done;
				    Sess_Name ->
					Self ! {delete_done, Sess_Name}
				end
			end)
	  end,
	  ?CLI_SessionNameList),

    %%%%
    %% Wait on answer from add  request.
    %%%%
    Sess_Del = lists:map(fun(_Pid_1) ->
				 receive
				     delete_done ->
					 error;
				     {delete_done, Sess_Name2} ->
    					 Sess_Name2
				 after 50000 ->
					 ct:fail("Error no answer within time!"
						 " expected: delete_done!", [])
				 end
			 end, PidList_Delete),

    %% Create a list of sessions there MO instance where deleted.
    %% Filter out error from list.
    %% error could be returned if operation is same time.
    DeletedInst = lists:filter(fun(Y) ->
    					   case Y of
					       error -> false;
    					       _ -> true
    					   end
    				   end, Sess_Del),
    ct:pal("Deleted instances : ~p",[DeletedInst]),

    lists:foreach(fun(PidB) ->
    			  exit(PidB, kill)
    		  end, PidList_Delete),

    %% timer:sleep(30000),
    sync(),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(DeletedInst),

    %%%%
    %% Just a cleanup to ensure all created instances is deleted
    %%%%
    ct:pal("Delete instances again to make sure all instances is deleted.",[]),
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several cli users, add, delete TestClass1 and commit,  <br/>
%% paralell req. <br/>
%% No instances should be created. <br/>
%% @spec cli_users_add_delete_testclass1_paralell(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_delete_testclass1_paralell(_Config) ->
    ct:pal("### Check several Cli users, add, delete testclass1 instances"
	   " paralell!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances.
    %%%%
    Self = self(),
    PidList =
	lists:map(
	  fun(Name) ->
		  spawn(fun() ->
				InstName = atom_to_list(Name),
				rct_cli_testclass1_lib:
				    add_several_mo_inst_and_attr_list_no_commit(
				      Name, [{InstName, ?ATTR}], ?PRINT_OPT),
				cli_delete_testclass1([Name]),
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
		  end, PidList),

    lists:foreach(fun(PidB) ->
    			  exit(PidB, kill)
    		  end, PidList),

    %% timer:sleep(30000),
    sync(),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Check several cli users, add TestClass1 paralell. <br/>
%% Do commit after all instances is added. <br/>
%% @spec cli_users_add_testclass1_paralell_commit(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1_paralell_commit(_Config) ->
    ct:pal("### Check several Cli users, add testclass1 instances paralell. "
	   "Do add the commit, delete then commit!",[]),

    %%%%
    %% Create cli session.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Create process that loops and waits for add, commit, delete cmds.
    %%%%
    Self = self(),
    Slave = fun(Name, Loop) ->
		    InstName = atom_to_list(Name),
		    receive
			add_cmd ->
			    rct_cli_testclass1_lib:
				add_several_mo_inst_and_attr_list_no_commit(
				  Name, [{InstName, ?ATTR}], ?PRINT_OPT),
			    Self ! add_done;
			commit_cmd_after_add ->
			    ct:pal("### Commit after add: ~p", [Name]),
			    case check_commit_paralell_after_add(Name) of
				error ->
				    Self ! commit_done_add;
				SessName ->
				    Self ! {commit_done_add, SessName}
			    end;
			commit_cmd_after_del ->
			    ct:pal("### Commit after del: ~p", [Name]),
			    case check_commit_after_delete (Name) of
				error ->
				    Self ! commit_done_del;
				SessionName ->
				    Self ! {commit_done_del, SessionName}
			    end;
			delete_cmd ->
			    rct_cli_testclass1_lib:
				delete_several_mo_inst_no_commit(Name,
								 [InstName],
								 ?PRINT_OPT),
			    Self ! delete_done;
			exit_slave -> exit(normal)
		    after 120000 ->
			    ct:pal("Exit slave due to timeout",[]),
			    exit(normal)
		    end,
		    Loop(Name, Loop)
	    end,

    PidList = lists:map(fun(Name1) ->
				spawn(fun() ->
					      Slave(Name1, Slave)
				      end)

			end,
			?CLI_SessionNameList),

    %%%%
    %% Send Add instances paralell request
    %%%%
    lists:foreach(fun(Pid1) ->
			  Pid1 ! add_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid2) ->
			  Pid2 ! commit_cmd_after_add
		     end, PidList),

    %%%%
    %% Wait on answer from the request.
    %%%%
    lists:foreach(fun(_Pid_1) ->
			 receive
    			      add_done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time! "
					 "expected: add_done!", [])
			 end
		  end, PidList),

    Sess_Add =
	lists:map(fun(_Pid_2) ->
			  receive
			      commit_done_add ->
				  error;
			      {commit_done_add, SessName} ->
				  SessName
			  after 50000 ->
				  ct:fail("Error no answer within time!"
					  " expected: commit_done_add! ", [])
			  end
		  end, PidList),

    %% Create a list of sessions there MO instance where created.
    %% Filter out error from list.
    %% error could be returned if operation is same time.
    CreatedInst = lists:filter(fun(X) ->
				       case X of
					   error -> false;
					   _ -> true
				       end
			       end, Sess_Add),
    ct:pal("Created Instances : ~p",[CreatedInst]),

    %% timer:sleep(30000),
    sync(),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(CreatedInst),

    %%%%
    %% Send Delete instance paralell request
    %%%%
    lists:foreach(fun(Pid3) ->
			  Pid3 ! delete_cmd
		  end, PidList),

    %%%%
    %% Send commit paralell request
    %%%%
    lists:foreach(fun(Pid4) ->
    			  Pid4 ! commit_cmd_after_del
    		  end, PidList),

    %%%%
    %% Wait on answer from the Delete request.
    %%%%
    lists:foreach(fun(_Pid_3) ->
			 receive
    			      delete_done ->
				 ok
			 after 50000 ->
				 ct:fail("Error no answer within time! "
					 "expected: delete_done!", [])
			 end
		  end, PidList),

    %% Wait on answer from the Commit request.
    Sess_Del =
	lists:map(fun(_Pid_4) ->
			  receive
			      commit_done_del ->
				  ok;
			      {commit_done_del, SessName1} ->
				  SessName1
			  after 50000 ->
				  ct:fail("Error no answer within time! "
					  "expected: commit_done_del!", [])
			  end
    		  end, PidList),

    %% Create a list of sessions there MO instance where deleted.
    %% Filter out error from list.
    %% error could be returned if operation is same time.
    DeletedInst = lists:filter(fun(Y) ->
    					   case Y of
					       error -> false;
    					       _ -> true
    					   end
    				   end, Sess_Del),
    ct:pal("Deleted instances : ~p",[DeletedInst]),


    %% timer:sleep(30000),
    sync(),

    lists:foreach(fun(Pid5) ->
    			  Pid5 ! exit_slave
    		  end, PidList),

    sync(),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(DeletedInst),

    %%%%
    %% Just a cleanup to ensure all created instances is deleted
    %%%%
    ct:pal("Delete instances again to make sure all instances is deleted.",[]),
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% CLI users add TestClass1, then restart.<br/>
%% - Add instances, No commit.<br/>
%% - restart.<br/>
%% - Check no instances exist.<br/>
%% - Add instances, do commit<br/>
%% - restart again.<br/>
%% - Check instances still exist.<br/>
%% @spec cli_users_add_testclass1_restart(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1_restart(_Config) ->
    ct:pal("### Check several cli users, add testclass1 instances. "
	   "Do restart warm!",[]),

    %%%%
    %% Get COM pid.
    %%%%
    ComPid = wait_for_com_to_start(),
    ct:pal("ComPid: ~p",[ComPid]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),
    %% cli_create_connection(lists:sublist(?CLI_SessionNameList, 5)),

    %%%%
    %% Add instances no commit.
    %%%%
    cli_add_testclass1_no_commit(?CLI_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart
    %%%%
    ct:pal("### restart warm!",[]),
    ok = rct_rpc:call(rpc, appmI, restart_piu_warm, ['_'], 100000, print),
    timer:sleep(10000),

    %%%%
    %% Wait for cli process not exist.
    %% Then release cli sessions on application side.
    %%%%
    wait_for_cli_proc_not_exist(),
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Wait for com.
    %%%%
    NewComPid = wait_for_com_to_restart(ComPid),

    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    cli_add_testclass1(?CLI_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart again
    %%%%
    ct:pal("### restart warm again!",[]),
    ok = rct_rpc:call(rpc, appmI, restart_piu_warm, ['_'], 100000, print),
    timer:sleep(10000),

    %%%%
    %% Wait for cli process not exist.
    %% Then release cli sessions on application side.
    %%%%
    wait_for_cli_proc_not_exist(),
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Wait for com.
    %%%%
    wait_for_com_to_restart(NewComPid),

    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    sync(),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% CLI users add TestClass1, then restart cold.<br/>
%% - Add instances, No commit.<br/>
%% - restart cold.<br/>
%% - Check no instances exist.<br/>
%% - Add instances, do commit<br/>
%% - restart cold again.<br/>
%% - Check instances still exist.<br/>
%% @spec cli_users_add_testclass1_init_reboot(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1_init_reboot(_Config) ->
    ct:pal("### Check several cli users, add testclass1 instances. "
	   "Do restart cold!",[]),

    %%%%
    %% Get COM pid.
    %%%%
    ComPid = wait_for_com_to_start(),
    ct:pal("ComPid: ~p",[ComPid]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances no commit.
    %%%%
    cli_add_testclass1_no_commit(?CLI_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart cold
    %%%%
    ct:pal("### restart cold !",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    net_kernel:disconnect(ErlNode),

    wait_for_restarting_system(),
    wait_for_login(),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    cli_add_testclass1(?CLI_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% restart cold again
    %%%%
    ct:pal("### restart cold again!",[]),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    net_kernel:disconnect(ErlNode),

    wait_for_restarting_system(),
    wait_for_login(),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Create connection.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    sync(),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %%  CLI users add TestClass1, then reboot.<br/>
%% %% - Add instances, No commit.<br/>
%% %% - Reboot.<br/>
%% %% - Check no instances exist.<br/>
%% %% - Add instances, do commit<br/>
%% %% - Reboot again.<br/>
%% %% - Check instances still exist.<br/>
%% %% @spec cli_users_add_testclass1_reboot(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
cli_users_add_testclass1_reboot(_Config) ->
    ct:pal("### Check several Cli users, add testclass1 instance. "
	   "Do reboot!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances no commit.
    %%%%
    cli_add_testclass1_no_commit(?CLI_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% reboot
    %%%%
    ct:pal("### reboot!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    %% ok = ct_telnet:send(console, "reboot"),
    ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board"),
    wait_for_restarting_system(),
    wait_for_login(),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    cli_add_testclass1(?CLI_SessionNameList),
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% reboot again
    %%%%
    ct:pal("### reboot again!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    net_kernel:disconnect(ErlNode),
    %% ok = ct_telnet:send(console, "reboot"),
    ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board"),
    wait_for_restarting_system(),
    wait_for_login(),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    sync(),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  CLI users add TestClass1, then power off/on.<br/>
%% - Add instances, No commit.<br/>
%% - Power off/on.<br/>
%% - Check no instances exist.<br/>
%% - Add insytances, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check instances exist or no exist.
%%   It depends if data has been written to disc before power off.<br/>
%% - Add instances, do commit.<br/>
%% - Power off/on again.<br/>
%% - Check instances exist.<br/>
%% @spec cli_users_add_testclass1_power(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1_power(_Config) ->
    ct:pal("### Check several cli users, add testclass1 instance. "
	   "Do power off/on!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances no commit.
    %%%%
    cli_add_testclass1_no_commit(?CLI_SessionNameList),

    %%%%
    %% Power off/on
    %%%%
    ct:pal("### power off/on!",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    net_kernel:disconnect(ErlNode),

    wait_for_restarting_system(),
    wait_for_login(),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%%%%%
    %% Add instances and commit, power off directly,
    %% then data could exist in disc. depends on how many instances is created.
    %%%%
    %%%%
    %% Add instances again and commit.
    %%%%
    cli_add_testclass1(?CLI_SessionNameList),

    %%%%
    %% Power off/on again
    %%%%
    ct:pal("### power off/on again!",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    net_kernel:disconnect(ErlNode),

    wait_for_restarting_system(),
    wait_for_login(),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),


    %%%% Remove this check due to sometimes power off/on takes some times,
    %%%% then it could result that instances exist.
    %% %%%%
    %% %% Check instance is deleted
    %% %%%%
    %% %% check_if_instance_exist_or_removed(?CLI_SessionNameList),
    %% ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Add instances and commit, power off after a few seconds
    %% so data has been written to disc.
    %%%%
    %%%%
    %% Add instances third time and commit.
    %%%%
    cli_add_testclass1(?CLI_SessionNameList),

    %% %% sleep to be sure that data exist on disc.
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Power off/on again
    %%%%
    ct:pal("### power off/on again! 3:rd time",[]),
    net_kernel:disconnect(ErlNode),
    ok = rct_power:cycle(node),
    net_kernel:disconnect(ErlNode),

    wait_for_restarting_system(),
    wait_for_login(),

    %%%%
    %% Clean up, Close Connections.
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    %%%%
    %% Create connection after restart.
    %%%%
    open_connection_after_restart(?CLI_SessionNameList),

    %%%%
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    sync(),

    ok.


%%--------------------------------------------------------------------
%% @doc
%%  CLI users add TestClass1, then kill_com.<br/>
%% - Add instances, No commit.<br/>
%% - kill com process.<br/>
%% - Check instances exist.<br/>
%% - Add instances, do commit<br/>
%% - kill com process again.<br/>
%% - Check instances still exist.<br/>
%% @spec cli_users_add_testclass1_kill_com(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
cli_users_add_testclass1_kill_com(_Config) ->
    ct:pal("### Check several Cli users, add testclass1 instances. "
	   "Kill com process!",[]),

    %%%%
    %% Create cli connection.
    %%%%
    cli_create_connection(?CLI_SessionNameList),

    %%%%
    %% Add instances no commit.
    %%%%
    cli_add_testclass1_no_commit(?CLI_SessionNameList),

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
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    %%%%
    %% Add instances again and commit.
    %%%%
    cli_add_testclass1(?CLI_SessionNameList),

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
    %% Check instances is added
    %%%%
    ok = cli_check_testclass1_added(?CLI_SessionNameList),

    %%%%
    %% Delete instances
    %%%%
    cli_delete_testclass1(?CLI_SessionNameList),

    %%%%
    %% Check instances is deleted
    %%%%
    ok = cli_check_testclass1_deleted(?CLI_SessionNameList),

    sync(),

    %%%%
    %% Close session
    %%%%
    cli_remove_connection(?CLI_SessionNameList),

    ok.


%%% Internal functions
%% %%--------------------------------------------------------------------
%% %% @hidden
%% %% check_if_instance_exist_or_removed(CLI_SessionNameList)
%% %%--------------------------------------------------------------------
%% check_if_instance_exist_or_removed(CLI_SessionNameList) ->
%%     ExistOrRemList =
%%     	lists:map(
%%     	  fun(SessName) ->
%% 		  %% This returns error if instance not exist,
%%     		  %% or session if it exist.
%% 		  InstName = atom_to_list(SessName),
%% 		  {ok, Answ} = rct_cli:send(SessName,
%% 					    "show ManagedElement=1,TestRoot=1,"
%% 					    "TestClass1="++InstName,
%% 					    ?PRINT_OPT),
%% 		  Data = lists:append(string:tokens(Answ, "\r\n")),
%% 		  case re:run(Data, "ERROR", [caseless]) of
%% 		      nomatch ->
%% 			  %% Instance exist.
%% 			  SessName;
%% 		      {match, _} ->
%% 			  case re:run(Data,
%% 				      "ERROR: Specific element not found") of
%% 			      {match, _} ->
%% 				  error;
%% 			      Other ->
%% 				  ct:pal("Rec unexpected answer when "
%% 					 "check if inst is created or deleted: "
%% 					 "~p , Session: ~p", [Other,
%% 							      SessName]),
%% 				  ct:fail("Unexpected answer when create")
%% 			  end
%% 		  end
%% 	  end, CLI_SessionNameList),
%%     ct:log("### ExistOrRemList : ~p", [ExistOrRemList]),

%%     %% %% Filter out error from list list.
%%     ExistList = lists:filter(fun(X) ->
%%     				     case X of
%%     					 error ->
%%     					     false;
%%     					 _ ->
%%     					     true
%%     				     end
%%     			     end, ExistOrRemList),
%%     ct:log("### ExistList : ~p", [ExistList]),
%%     NrOfCreatedInst = length(ExistList),

%%     %% %% %%% Just a easy chek to se if instances is created or not as expected.
%%     %% %% case ?NrOfCliUsers of
%%     %% %% 	%% Instances Always deleted from disc after power off/on
%%     %% %% 	Val when Val < 5 ->
%%     %% %% 	    ct:pal("### Instances shall not exist on disc.", []),
%%     %% %% 	    NrOfCreatedInst = 0;
%%     %% %% 	%% This numbers of instances could result in deleted or
%%     %% %% 	%% existing on disc after power off/on
%%     %% %%     Val when Val < 15 ->
%%     %% %% 	    ct:pal("### instances could exist or not existon disc.", []),
%%     %% %% 	    %% ct:log("### ExistOrRemList : ~p", [ExistOrRemList]),
%%     %% %% 	    ct:pal("NrOfCreatedInst: ~p of Nr of create inst operation"
%%     %% %% 		   " befor power: ~p",
%%     %% %% 		   [NrOfCreatedInst, length(ExistOrRemList)]);
%%     %% %% 	%% This numbers of instances always result in deleted and
%%     %% %% 	%% existing on disc after power off/on
%%     %% %% 	_ ->
%%     %% %% 	    ct:pal("### Some instances shall exist on disc.\n"
%%     %% %% 		   "### NrOfCreatedInst: ~p", [NrOfCreatedInst]),
%%     %% %% 	    case NrOfCreatedInst of
%%     %% %% 		Nr when Nr > 0 ->
%%     %% %% 		    ok;
%%     %% %% 		_ ->
%%     %% %% 		    ct:fail("Expected some instances created, but it was not!")
%%     %% %% 	    end
%%     %% %% end,

%%     %% %% %% Using the above check results in warning when compile.
%%     %% %% %% This is a quick fix that shall be used when ?NrOfCliUsers
%%     %% %% %% is larger than 15.
%%     %% ct:pal("### Some instances shall exist on disc.\n"
%%     %% 	   "### NrOfCreatedInst: ~p", [NrOfCreatedInst]),
%%     %% case NrOfCreatedInst of
%%     %% 	Nr when Nr > 0 ->
%%     %% 	    ok;
%%     %% 	_ ->
%%     %% 	    ct:fail("Expected some instances created, but it was not!")
%%     %% end,

%%     ct:pal("### Nr of Instances that exist on disc: ~p", [NrOfCreatedInst]),
%%     %% NrOfCreatedInst = 0,

%%     ok.

%%--------------------------------------------------------------------
%% @hidden
%% Sometimes ssh connection is up but com is not, <br/>
%% then the cli connection is not setuped. <br/>
%% Sometimes ComsaServer could restart COM due to healthcheck, <br/>
%% the the connection will be released. <br/>
%% open_connection_after_restart(CLI_SessionNameList)
%%--------------------------------------------------------------------
open_connection_after_restart(CLI_SessionNameList)->
    open_connection_after_restart(CLI_SessionNameList, 120000).
open_connection_after_restart(_CLI_SessionNameList, Timeout ) when
      Timeout < 500 ->
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
	    ct:log("found error from setup_cli_session: ~p. "
		   "Wait and try again.",
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
    CliPids = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"],
					 10000), "\n "),
    Length = length(CLI_SessionNameList),
    case length(CliPids) of
	Length ->
	    ok;
	_L ->
	    ct:log("Length of nr cli pids: ~p, is not expected: ~p . "
		   "Wait and setup again",
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
    wait_for_com_to_start(180000).

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
%% cli_check_testclass1_added(CLI_SessionNameList). <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_testclass1_added(CLI_SessionNameList)->
    lists:
	foreach(
	  fun(CliSessName) ->
		  InstName = atom_to_list(CliSessName),
		  ok = cli_check_inst_attr_added(CliSessName,
						 InstName,
						 ?ATTR,
						 ?PRINT_OPT)
	  end, CLI_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% cli_check_inst_attr_added(Name, InstName, ATTR, PrintOpt). <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_inst_attr_added(Name, InstName, ATTR, PrintOpt) ->
    %% InstName = atom_to_list(Name),

    %% Just to make sure that no crap is rcvd first time after power.
    rct_cli:send(Name, "show", PrintOpt),

    {ok , Recieved_Data} =
	rct_cli:send(Name,
		     "show ManagedElement=1,TestRoot=1,"
		     "TestClass1="++InstName,
		     PrintOpt),
    RecievedData = lists:append(string:tokens(Recieved_Data,
					      "\r\n")),
    ct:log("RecievedData : ~p", [RecievedData]),

    case re:run(RecievedData, "TestClass1="++InstName++
		    "ERROR: Specific element not found") of
	{match, _} ->
	    ct:pal("InstName does not exist: ~p.",[InstName]),
	    ct:fail("Expected InstName does not exist");
	nomatch ->
	    %% Check Attributes exist.
	    {match, _} = re:run(RecievedData,
				"int32="++ATTR),
	    %% ct:pal("RecievedData : ~p", [RecievedData]),
	    ok
    end,
    ok.



%%--------------------------------------------------------------------
%% @doc
%% cli_add_testclass1_no_commit(CLI_SessionNameList). <br/>
%% @end
%%--------------------------------------------------------------------
cli_add_testclass1_no_commit(CLI_SessionNameList)->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  ct:pal("### Add instance: ~p with attr. No commit",
				 [Name]),
			  rct_cli_testclass1_lib:
			      add_several_mo_inst_and_attr_list_no_commit(
				Name, [{InstName, ?ATTR}], ?PRINT_OPT)
		  end,CLI_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% cli_delete_testclass1_no_commit(CLI_SessionNameList). <br/>
%% @end
%%--------------------------------------------------------------------
cli_delete_testclass1_no_commit(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  ct:pal("### Delete instance: ~p, No commit", [Name]),
			  rct_cli_testclass1_lib:
			      delete_several_mo_inst_no_commit(Name,
							       [InstName],
							       ?PRINT_OPT)
		  end,
		  CLI_SessionNameList),
    ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% cli_check_added_testclass1_not_exist(?CHECK_SESSION, <br/>
%% %% CLI_SessionNameList). <br/>
%% %% @end
%% %%--------------------------------------------------------------------
%% cli_check_added_testclass1_not_exist(CHECK_SESSION, CLI_SessionNameList) ->
%%     ok = rct_cli:connect(CHECK_SESSION),
%%     lists:foreach(fun(Name) ->
%% 			  InstName = atom_to_list(Name),
%% 			  rct_cli_testclass1_lib:
%% 			      check_mo_inst_deleted(CHECK_SESSION,
%% 						    InstName,
%% 						    ?PRINT_OPT)
%% 		  end, CLI_SessionNameList),
%%     ok = rct_cli:disconnect(CHECK_SESSION),
%%     ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% cli_check_testclass1_exist(?CHECK_SESSION, CLI_SessionNameList). <br/>
%% %% @end
%% %%--------------------------------------------------------------------
%% cli_check_testclass1_exist(CHECK_SESSION, CLI_SessionNameList) ->
%%     ok = rct_cli:connect(CHECK_SESSION),
%%     lists:foreach(fun(Name) ->
%% 			  InstName = atom_to_list(Name),
%% 			  %% rct_cli_testclass1_lib:
%% 			  cli_check_inst_attr_added(CHECK_SESSION,
%% 						    InstName,
%% 						    ?ATTR,
%% 						    ?PRINT_OPT)
%% 		  end, CLI_SessionNameList),
%%     ok = rct_cli:disconnect(CHECK_SESSION),
%%     ok.


%%--------------------------------------------------------------------
%% @doc
%% cli_add_paralell_check_commit(Cli_session). <br/>
%% @end
%%--------------------------------------------------------------------
cli_add_paralell_check_commit(Cli_session) ->
    InstName = atom_to_list(Cli_session),
    rct_cli_testclass1_lib:
	add_several_mo_inst_and_attr_list_no_commit(Cli_session,
						    [{InstName, ?ATTR}],
						    ?PRINT_OPT),
    check_commit_paralell_after_add(Cli_session).

check_commit_paralell_after_add(Cli_session) ->
    {ok, Answ} = rct_cli:send(Cli_session, "commit", ?PRINT_OPT),
    Data = lists:append(string:tokens(Answ, "\r\n")),
    ct:log("## Add Data: ~p",[Answ]),
    case re:run(Data, "ERROR", [caseless]) of
	nomatch ->
	    %% Operation is OK.
	    Cli_session;
	_ ->
	    %% Check error is ok!
	    %% case re:run(Data, "sa_ais_err_exist") of
	    case re:run(Data, "ComFailure") of
		{match, _} ->
		    error;
		Other ->
		    ct:pal("Rec unexpected answer when "
			   "create : ~p , Session: ~p", [Other, Cli_session]),
		    ct:fail("Unexpected answer when create")
	    end

    end.

%%--------------------------------------------------------------------
%% @doc
%% cli_check_commit_after_del(Cli_session). <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_commit_after_del(Cli_session) ->
    InstName = atom_to_list(Cli_session),
    rct_cli_testclass1_lib:
	delete_several_mo_inst_no_commit(Cli_session,
					 [InstName],
					 ?PRINT_OPT),
    check_commit_after_delete(Cli_session).

check_commit_after_delete(Cli_session) ->
    {ok, Answ} = rct_cli:send(Cli_session, "commit", ?PRINT_OPT),
    Data = lists:append(string:tokens(Answ, "\r\n")),
    ct:log("## Del Data ~p",[Answ]),
    case re:run(Data, "ERROR", [caseless]) of
    	nomatch ->
    	    %% Operation is OK.
    	    Cli_session;
    	{match, _} ->
    	    case re:run(Data, "ERROR: Specific element not found") of
    		{match, _} ->
    		    error;
    		Other ->
    		    ct:pal("Rec unexpected answer when "
    			   "delete : ~p , Session: ~p", [Other, Cli_session]),
    		    ct:fail("Unexpected answer when create")
    	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% cli create connection. <br/>
%% @end
%%--------------------------------------------------------------------
cli_create_connection(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:pal("### CLI Connect Name: ~p", [Name]),
			  wait_to_cli_connected(Name)
			  %% ok = rct_cli:connect(Name)
			  %% %% ok = rct_cli:connect(Name, "expert", "expert", "[***]\r\n>$")
		  end,
		  CLI_SessionNameList).

wait_to_cli_connected(Name) ->
    wait_to_cli_connected(Name, 30000).
wait_to_cli_connected(Name, Timeout) when Timeout < 0 ->
    ct:pal("Failed to connect to cli session: ~p." , [Name]),
    ct:fail("could not connect cli within 30 sec. ");
wait_to_cli_connected(Name, Timeout) ->
    case rct_cli:connect(Name) of
	ok ->
	    ok;
	Err ->
	    ct:log("Failed to connect to cli session: ~p. sleep and try again. ~p", [Name, Err]),
	    timer:sleep(1000),
	    wait_to_cli_connected(Name, Timeout-1000)
    end.





%%--------------------------------------------------------------------
%% @doc
%% cli add instances. <br/>
%% @end
%%--------------------------------------------------------------------
cli_add_testclass1(CLI_SessionNameList) ->
    lists:foreach(fun(Name)->
			  InstName = atom_to_list(Name),
			  ct:pal("### Add instance: ~p with attr.", [Name]),
			  rct_cli_testclass1_lib:
			      add_mo_inst_and_attr(Name,
						   InstName,
						   ?ATTR,
						   ?PRINT_OPT)
			      end, CLI_SessionNameList),
		  ok.

%%--------------------------------------------------------------------
%% @doc
%% cli delete instances. <br/>
%% @end
%%--------------------------------------------------------------------
cli_delete_testclass1(CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  ct:pal("### Delete instance: ~p", [Name]),
			  rct_cli_testclass1_lib:
			      delete_mo_inst(Name, InstName, ?PRINT_OPT)
		  end,
		  CLI_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% cli check instances does not exist. <br/>
%% @end
%%--------------------------------------------------------------------
cli_check_testclass1_deleted(CLI_SessionNameList) ->
  lists:foreach(fun(Name) ->
			InstName = atom_to_list(Name),
			%% ct:pal("### Check instance: ~p deleted", [Name]),
			rct_cli_testclass1_lib:
			    check_mo_inst_deleted(Name, InstName, ?PRINT_OPT)
		end, CLI_SessionNameList),
    ok.

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
			  rct_cli:send(Name, "commit"),
			  rct_cli:send(Name, "top")
		  end,
		  CLI_SessionNameList).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% wait_for_node_go_down. <br/>
%% %% @end
%% %%--------------------------------------------------------------------
%% wait_for_node_go_down(ErlNode) ->
%%     wait_for_node_go_down(ErlNode, 60000).
%% wait_for_node_go_down(_ErlNode, Timeout) when Timeout < 500 ->
%%     ct:fail("Node has not gone down !");
%% wait_for_node_go_down(ErlNode, Timeout) ->
%%         case net_adm:ping(ErlNode) of
%% 	pang ->
%% 	    ok;
%% 	_Res ->
%% 	    %% ct:pal("Ping res: ~p", [Res]),
%% 	    timer:sleep(3000),
%% 	    wait_for_node_go_down(ErlNode, Timeout-3000)
%%     end.

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
    %% case rct_rpc:call(rpc, init, reboot, [], 2000) of
    case rct_rpc:call(rpc, appmI, restart_piu_cold, ['_'], 100000, print) of
	ok ->
	    ok;
	{badrpc,nodedown} ->
	    ct:pal("restart cold() did not take effect! try again",[]),
	    timer:sleep(2000),
	    do_init_reboot(Nr-1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% wait_for_cli_proc_not_exist. <br/>
%% @end
%%--------------------------------------------------------------------
wait_for_cli_proc_not_exist()->
    wait_for_cli_proc_not_exist(30000).
wait_for_cli_proc_not_exist(Timeout) when Timeout < 0 ->
    CliSess = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, print),
    ct:pal("TC will fail due to unexpected cli sessions still exist: ~p",
	   [CliSess]),
    ct:fail("TC will fail due to unexpected cli sessions still exist");
wait_for_cli_proc_not_exist(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, print) of
	[] ->
	    ct:log("## Cli sessions not exist.", []),
	    ok;
	_Other ->
	    ct:log("Cli sessions still exist: ~p", [_Other]),
	    timer:sleep(1000),
	    wait_for_cli_proc_not_exist(Timeout-1000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% cli_delete_instances. Used in clanup if TC fails !<br/>
%% @end
%%--------------------------------------------------------------------
cli_delete_instances(SessionName, CLI_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  InstName = atom_to_list(Name),
			  rct_cli_testclass1_lib:
			      delete_several_mo_inst(SessionName, [InstName])
		  end,
		  CLI_SessionNameList).

%%--------------------------------------------------------------------
%% @doc
%% show_to_avoid_idle_timeout(CompleteNotCommitedList) <br/>
%% @end
%%--------------------------------------------------------------------
show_to_avoid_idle_timeout(CompleteNotCommitedList) ->
    ct:log(" send show cmd, avoid idle tiomout on sessions.", []),
    lists:foreach(fun(CliSess) ->
			  rct_cli:send(CliSess,
				       "show ManagedElement=1,TestRoot=1",
				       noprint)
		  end, CompleteNotCommitedList).

%%--------------------------------------------------------------------
%% @doc
%% wait_for_login <br/>
%% @end
%%--------------------------------------------------------------------
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
%% @doc
%% wait_for_restarting_system <br/>
%% @end
%%--------------------------------------------------------------------
wait_for_restarting_system() ->
    ct_telnet:expect(console, "Restarting system",
		     [{total_timeout,30000},
		      {idle_timeout,30000}, no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%% sync <br/>
%% @end
%%--------------------------------------------------------------------
sync() ->
    rct_rpc:call(rpc, os, cmd, ["sync"], 20000, print).

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	restart_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R5A/1
-module(restart_SUITE).
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-01-26 eransbn     Created
%%% R2A/3      2014-01-27 etxivri     Minor update due to edoc error.
%%% R2A/5      2014-01-28 eransbn     Change poll time after restart
%%% R2A/8      2014-01-28 eransbn     Reset restart list in APPM.
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warnings and copyright
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 restart_pri_cold/1,
	 restart_eri_cold/1
]).
%%% ----------------------------------------------------------
%%% Definitions
%%% ----------------------------------------------------------
%%restart command
-define(PRI_COLD, "/dummy/restart pri cold").
-define(PRI_WARM, "/dummy/restart pri warm").
-define(ERI_COLD, "dummy/restart eri cold").
-define(ERI_WARM, "/dummy/restart eri warm").
%%
-define(RE_COLD,".*\\s+Cold.*").
-define(RE_ERI, ".*\\s+Cold.*").
-define(COMMAND_COLD, "grep \"Cold\" /var/log/syslog").
-define(EventList, ["middleware starting","COM is running" ,"non-upgrade","applications started"]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_logging, rct_core
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_power,node},
		 {rct_logging, {all,
				[{erlang,{
				    ["ERROR REPORT","CRASH REPORT"],
				    ["Name : \"restart_app\"",
				     "Name : \"restart\"",
				     "When Server state == {state,lih_erlang_handler", %%lih not used should be removed
				     "initial call: lih_lici_worker:init",
				     "Erlang ssh connection handler failed with reason"]}}]}},
		 {rct_coli, {coli, [manual_connect]}},
		 {rct_ssh,node1,{"bash -l -c"}},
		 {rct_core,[]}]}].
%% @hidden
init_per_suite(Config) ->
    %%
    %% Reset restart list in APPM.
    %%
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    rct_rpc:call(rpc, appmServer, enable_warm_restart, [0], 10000),
    Config.
%% @hidden
end_per_suite(_Config) ->
    %%
    %% Reset restart list in APPM.
    %%
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    rct_rpc:call(rpc, appmServer, disable_warm_restart, [0], 10000),
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    %% case TestCase of
    %% 	restart_eri_cold -> rct_logging:
    %% 	    _->  ok
ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [restart_pri_cold,
     restart_eri_cold
     %%restart_pri_warm,
     %%restart_eri_warm
    ].
%%--------------------------------------------------------------------
%% @doc
%% Restarting node with coli command "/dummy/restart pri cold" (using restart_app on target),
%% Verify by reading /var/log/syslog, checking number of restarts before and after restart.
%% @end
%%--------------------------------------------------------------------
restart_pri_cold(_Config) ->
    AppsBeforeRestart = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    NrOfColdRestartsBefore = nr_of_restart(node1, ?COMMAND_COLD, ?RE_COLD),
    ok = rct_coli:connect(coli),
    ct:pal("Sending ~p",[?PRI_COLD]),
    {ok, {_Reply, _Matched}}  = rct_coli:send(coli, ?PRI_COLD, ".*\\s+ok.*", noprint),
    Start1 = erlang:timestamp(),

    ct:pal("Check that the coli connection goes down"),
    poll_connection(coli, 1000, {error,econnrefused}, 40),

    ct:pal("Wait for the coli connection"),
    poll_connection(coli, 3000, ok, 60),
    End1 = erlang:timestamp(),
    RestartTime = trunc(timer:now_diff(End1, Start1) / 1000 / 1000),

    %% rct_snmpmgr:wait_for_traps([[{type,eriAlarmAlarmListRebuilt}]],
    %% 			       [{wait, true}],
    %% 			       180),
    ct:log("Restart time: ~p",[RestartTime]),
    %%Wait for the /var/log/syslog to be updated
    NrOfColdRestartsAfter =
    	poll_nr_of_restart(node1, ?COMMAND_COLD, ?RE_COLD , NrOfColdRestartsBefore,
			   NrOfColdRestartsBefore +1, 10),
    %%Check that appm started
    check_all_apps_started(AppsBeforeRestart),

    %%Check restart_logging
    RestartLogErrorList = restart_logging(?EventList),
 ct:pal("Number of restart before test:~p and after: ~p"
		     "~n~p",
    		     [NrOfColdRestartsBefore, NrOfColdRestartsAfter,RestartLogErrorList ]),
    %%Check test result
    case  (NrOfColdRestartsAfter == NrOfColdRestartsBefore + 1)
	and (RestartLogErrorList == []) of
    	true -> ok;
    	_ -> ct:fail("Number of restart before test:~p and after: ~p"
		     "~n~p",
    		     [NrOfColdRestartsBefore, NrOfColdRestartsAfter,RestartLogErrorList ])
    end,

    ok.



%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Restarting node with coli command "/dummy/restart pri cold" (using restart_app on target),
%% %% Verify by reading /var/log/syslog, checking number of restarts before and after restart.
%% %% @end
%% %%--------------------------------------------------------------------
%% restart_pri_warm(_Config) ->

%%     NrOfColdRestartsBefore = nr_of_restart(node1, ?COMMAND_COLD, ?RE_COLD),

%%     ok = rct_coli:connect(coli),
%%     ct:pal("Sending ~p",[?PRI_WARM]),
%%     {ok, {_Reply, _Matched}}  = rct_coli:send(coli, ?PRI_WARM, ".*\\s+ok.*", noprint),

%%     ct:pal("Check that the coli connection goes down"),
%%     poll_connection(coli, 1000, {error,econnrefused}, 20),

%%     ct:pal("Wait for the coli connection"),
%%     poll_connection(coli, 3000, ok, 20),

%%     NrOfColdRestartsAfter = nr_of_restart(node1, ?COMMAND_COLD,?RE_COLD),

%%     %%Check test result
%%     case  NrOfColdRestartsAfter == NrOfColdRestartsBefore + 1 of
%% 	true -> ct:log("Number of cold restart before test:~p and after: ~p",
%% 		       [NrOfColdRestartsBefore, NrOfColdRestartsAfter]),
%% 		ok;
%% 	_ -> ct:fail("Number of restart before test:~p and after: ~p",
%% 		     [NrOfColdRestartsBefore, NrOfColdRestartsAfter])
%%     end,

%%     ok.

%%--------------------------------------------------------------------
%% @doc
%% Restarting node with coli command "/dummy/restart eri cold" (using restart_app on target),
%% Verify:
%% 1. Reading /var/log/syslog, checking number cold restarts before and after restart.
%% 2. Reading /var/log/syslog, checking number of ERI restarts before and after restart.
%% 3. Execute dump command and check if pmd is created.
%% @end
%%--------------------------------------------------------------------
restart_eri_cold(_Config) ->
    %%Skip dump check
    ok = rct_core:coredump_test(),

    NrOfColdRestartsBefore = nr_of_restart(node1,?COMMAND_COLD, ?RE_COLD),
    NrOfPmdBefore = nr_of_dumps(node1),
    ct:pal("NrOfColdRestartsBefore ~p  NrOfPmdBefore ~p ",
	   [NrOfColdRestartsBefore,  NrOfPmdBefore]),

    ok = rct_coli:connect(coli),

    ct:pal("Sending coli command: ~p",[?ERI_COLD]),
    {ok,_} = rct_coli:send(coli, ?ERI_COLD),

    ct:pal("Check that the coli connection goes down"),
    poll_connection(coli, 1000, {error,econnrefused}, 40),

    ct:pal("Wait for the coli connection"),
    poll_connection(coli, 3000, ok, 60),

    %%Wait for the /var/log/syslog to be updated
    NrOfColdRestartsAfter =
	poll_nr_of_restart(node1, ?COMMAND_COLD, ?RE_COLD, NrOfColdRestartsBefore, NrOfColdRestartsBefore +1, 10),

    NrOfPmdAfter = nr_of_dumps(node1),

    ct:pal("NrOfColdRestartsAfter ~p  NrOfPmdAfter ~p",[NrOfColdRestartsAfter, NrOfPmdAfter]),

    %%Check test result
    case (NrOfColdRestartsAfter == NrOfColdRestartsBefore + 1)
	and (NrOfPmdAfter == NrOfPmdBefore + 1) of
	true -> ct:log("Cold restart before test:~p and after: ~p~n"
		       "Pmd(s) before test ~p and after ~p",
		       [NrOfColdRestartsBefore, NrOfColdRestartsAfter,
			NrOfPmdBefore, NrOfPmdAfter
		       ]),
		ok;
	_ -> ct:fail("Cold restart before test:~p and after: ~p~n"
		       "Pmd(s) before test ~p and after ~p",
		       [NrOfColdRestartsBefore, NrOfColdRestartsAfter,
			NrOfPmdBefore, NrOfPmdAfter
		       ])
    end,

    ok.
%%-------------------------------%%
%%     Internal functions
%%-------------------------------%%
restart_logging(EventList)->
    TexLog =
	case rct_rpc:call(rpc, os,cmd,["tex log read"], 10000) of %%| grep -i  trace9
	    {badrpc,timeout} ->
		rct_rpc:call(rpc, os,cmd,["tex log read"], 10000);%%| grep -i  trace9
	    Return -> Return
	end,
    ReturnErrorList = check_lttng_trace([TexLog], EventList),
    ReturnErrorList.

check_lttng_trace(ReturnList,CheckList)->
    ErrorList =  check_lttng_trace(ReturnList, CheckList, []),
    ErrorList.

check_lttng_trace(_CheckList,[], ErrorList) ->
    ErrorList;
check_lttng_trace(CheckList, [String|Rest], ErrorList) ->
    Option = [global, {capture, all, list}],
    RE = ".*" ++String,
    case  re:run(CheckList, RE, Option) of
	{match, _Captured} ->
	    check_lttng_trace(CheckList, Rest, ErrorList);
	nomatch ->
	    check_lttng_trace(CheckList, Rest, ErrorList ++ " nomatch string " ++ "\"" ++ String++"\"")
    end.

%%Nr of restarts by reading /var/log/syslog on target
nr_of_restart(Node, Command, RE)->
    case rct_ssh:exec(Node,Command, 5000, RE,
		      [global, {capture, all, list}]) of
	{error,[]} -> 0; %%If no match found in the logfile
	{ok, Result}-> length(string:tokens(Result, "\n"));
%%	Unknown -> ct:fail("Not expected return: ~p ",[Unknown])
	_-> -1
    end.

%%Wait for /var/log/syslog to be updated after restart
poll_nr_of_restart(Node, Command, RE, BeforeRestart, ExpectedRestart, TimeTry) ->
    NumRestart = poll_nr_of_restart(Node, Command, RE, BeforeRestart, ExpectedRestart,TimeTry, 0),
    NumRestart.
poll_nr_of_restart(_Node, _Command, _RE, _BeforeRestart, _ExpectedRestart,
		   TimeTry, NumRestart) when TimeTry == 0->
    NumRestart;
poll_nr_of_restart(Node, Command, RE, BeforeRestart, ExpectedRestart,
		   TimeTry, _ReturnValue) ->
    timer:sleep(1000),
    NumRestart = nr_of_restart(Node, Command, RE),
    case NumRestart == ExpectedRestart of
	true -> poll_nr_of_restart(Node, Command, RE, BeforeRestart, ExpectedRestart,
				  0, NumRestart);
	false ->
	    poll_nr_of_restart(Node, Command, RE, BeforeRestart,
			       ExpectedRestart, TimeTry -1, NumRestart)
    end.

%%Number of pmds by using command "dump"
nr_of_dumps(Node)->
    Command = "bash -l -c dump",
    RE = ".*pmd-cmd_proc.*",
    Option= [global, {capture, all, list}],

    ct:pal("Sending command  ~p",[Command]),
    {_Result, Reply} = rct_ssh:exec(Node, Command, 5000, RE,
				   [global, {capture, all, list}]),
    ct:pal("~p",[re:run(Reply, RE, Option)]),
    NrOfDumps =
	case re:run(Reply, RE, Option) of
	    {match, Reply1} -> length(Reply1);
	     nomatch -> check_of_dumps(Reply, 0)
	end,
    NrOfDumps.

check_of_dumps(Reply, Counter)->
    case Counter of
	0 -> case re:run(Reply,".*NO SAVED DUMPS.*", [global, {capture, all, list}]) of
		 {match,_} -> ct:pal("re:run(Reply,.*NO SAVED DUMPS.* Reply ~p",[Reply]),0;
		 nomatch -> check_of_dumps(Reply, Counter + 1)
		 end;
	%%When pmd has removed, this prinout occur
	1 -> case re:run(Reply,".*ENVELOPE         CONTENTS.*",
			 [global, {capture, all, list}]) of
		 {match,_} ->ct:pal("ENVELOPE         CONTENTS Reply ~p",[Reply]), 0;
		 nomatch -> check_of_dumps(Reply, Counter + 1)
		 end;
	2 -> ct:fail("Not expected Result when checking dumps~nReply ~p",[Reply])
    end.
%% poll_connection(Name, Timer, ExpectedResult, NumberAttempts) -> ok|fail
%% Name =           atom()       Only suport coli (can be built out)
%% Timer =          integer()    Time between attempts.
%% NumberAttempts = integer()    Number of Attempts.
%% ExpectedResult =              Expected return.
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


check_all_apps_started(AppsBeforeRestart)->
    check_all_apps_started(AppsBeforeRestart,[], 30),
    ct:pal("All Apps started").

check_all_apps_started(AppsBeforeRestart,AppsAfterRestart , 0)->
    ct:fail("All apps not started~n Apps before restart ~n~p"
	   "~nApps after restart~n~p",[AppsBeforeRestart, AppsAfterRestart]);
check_all_apps_started(AppsBeforeRestart, _PrevAppsAfterRestart,Timer)->
    AppsAfterRestart = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    timer:sleep(1000),
    case lists:foreach(fun(App) ->
			   lists:keymember(App,1, AppsBeforeRestart)
		   end, AppsAfterRestart) of

	ok ->
	    ok;
	_ -> check_all_apps_started(AppsBeforeRestart, AppsAfterRestart, Timer - 1)
    end.

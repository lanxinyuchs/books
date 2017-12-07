%%% %CCaseFile:	tzii_c_SUITE.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R7A/R9A/R10A/1

%%% @doc ==Tests of TIM==
%%% This test suite exercises the TIM (aka SIB16, aka Time Zone Information) service.

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% -----      -------    --------    ------------------------
%%% R4A/1      2015-09-01 erarafo     First version
%%% R4A/2      2015-09-05 erarafo     Using generated header files
%%% R4A/3      2015-09-10 erarafo     Extended set of tests
%%% R4A/4      2015-09-28 erarafo     More test cases
%%% R4A/5      2015-09-30 erarafo     Signal handler refactored
%%% R4A/7      2015-10-09 erarafo     Test cases improved
%%% R4A/8      2015-10-14 erarafo     Test case added
%%% R4A/9      2015-10-16 erarafo     Adapted to changed behaviours
%%% R4A/10     2015-10-17 erarafo     Reporting timServer version
%%% R4A/11     2015-10-18 erarafo     Adapted to changes in error handling
%%% R4A/12     2015-10-19 erarafo     DST re-subscribe test added
%%% R4A/13     2015-10-20 erarafo     Test of initService repeatedly
%%% R4A/14     2015-10-22 etxpejn     Added cluster sim groups
%%% R4A/15     2015-10-23 erarafo     Adapted to simplified proxy state diagram
%%% R4A/16     2015-10-29 erarafo     Scenario doing init-term repeatedly
%%% R4A/17     2015-10-30 erarafo     Signal handler moved to module by itself
%%% R4A/18     2015-11-02 erarafo     Switch to OTP 18 time API
%%% R4A/19     2015-11-03 erarafo     Using improved signal handler
%%% R4A/20     2015-11-03 erarafo     Removed string 'warning' from log messages
%%% R4A/21     2015-11-05 erarafo     Added test_dst_start_simple
%%% R4A/22     2015-11-09 erarafo     Test of setMailbox
%%% R4A/23     2015-11-11 erarafo     Fixed wall clock dependency
%%% R4A/24     2015-11-12 erarafo     Added test case that runs the example code
%%% R4A/25     2015-11-16 erarafo     Test case for early setMailbox
%%% R4A/26     2015-11-17 erarafo     Test case for Leap Seconds not configured
%%% R4A/27     2015-11-17 erarafo     Unnecessarily long timeout reduced
%%% R4A/28     2015-11-20 erarafo     Temporarily disabling a testcase
%%% R4A/29     2015-11-23 erarafo     Re-enabling testcase
%%% R4A/30     2015-11-30 erarafo     Test coverage increase
%%% R5A/1      2015-12-04 erarafo     Support for capturing ift_app.log
%%% R5A/2      2016-02-01 erarafo     Using ct:pal/2 throughout suite
%%% R5A/3      2016-02-18 erarafo     Lowered a timeout by 3000 ms
%%% R7A/1      2016-09-15 erarafo     More logging from test_dst_start/1
%%% R7A/2      2016-09-19 erarafo     Check board time while dst_start/1 runs
%%% R7A/3      2016-09-19 erarafo     Cleanup
%%% R7A/4      2016-09-20 erarafo     Extended clock checking
%%% R7A/5      2016-09-21 erarafo     Add reporting of load average
%%% R7A/6      2016-09-21 erarafo     Add trace of timServer sleep/wakeup
%%% R7A/7      2016-09-24 erarafo     Refactor: don't do getChildPid repeatedly
%%% R7A/8      2016-09-25 erarafo     Synchronize start of TC to tenth of second,
%%%                                     elaborated clock check
%%% R9A/1      2016-02-03 etxpeno     Update of sc_resubscribe
%%%                                   (Needed due to the fix of TR HV53411)
%%% R9A/4      2016-03-28 etxpeno     Remove test_dst_start from all()
%%% R9A/5      2017-04-03 etomist     Change test_dst_end (HV73602)
%%% R10A/1     2017-07-26 etomist     Change updateTimeSettings (HW12725)
%%% ----------------------------------------------------------

%% TODO, add checking that no spurious signals occur once
%% scenarios have stopped.

%% TODO, various negative tests related to the proxy state diagram.

-module(tzii_c_SUITE).
-id('Updated by CCase').
-vsn('/main/R4A/R5A/R7A/R9A/R10A/1').
-date('2017-07-26').
-author('etomist').

-include("sigHandler.hrl").


-export([test_load_average/1,
	 test_better_sqr/1,
	 test_dst_rule/1,
	 test_memory_not_initiated/1,
	 test_dst_start_simple/1,
	 test_dst_start/1,
	 test_dst_end/1,
	 test_dst_ts_update/1,
	 test_leap/1,
	 test_leap_a/1,
	 test_leap_b/1,
	 test_leap_reconfig/1,
	 test_neg_leap_prewarn/1,
	 test_timesettings_update/1,
	 test_timestep_forward/1,
	 test_timestep_backward/1,

	 test_dependencies/1,               %% TODO
	 test_leap_rejected/1,              %% TODO

%% 	 test_proxy1/1,

	 test_get_versions/1,
	 test_neg_if_mgmt/1,
	 test_set_mailbox/1,
	 test_set_mailbox_alt/1,

	 test_tzii_example/1,

	 test_tzii_unconfigured/1
	]).


%% Common Test callbacks
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0]).


%% Signal handler callbacks (gen_server style)
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).


all() ->
    [test_better_sqr,
     test_get_versions,
     test_dst_rule,
     test_memory_not_initiated,
     % test_dst_start_simple, ...... would not add any value
     % test_dst_start,
     test_dst_end,
     test_dst_ts_update,
     test_leap,
     test_leap_a,
     test_leap_b,
     test_leap_reconfig,
     test_neg_leap_prewarn,
     test_timesettings_update,
     test_timestep_forward,
     test_timestep_backward,
     test_neg_if_mgmt,
     test_set_mailbox,
     test_set_mailbox_alt
    ].


-include_lib("common_test/include/ct.hrl").
-include("test_tzii_requests.hrl").
-include("cello_tzii_sig.hrl").
-include("cello_tzii.hrl").


-define(SAY(A, B), ct:pal(A, B)).

-define(VERSIONS_GOOD, {6, 5, 1}).
-define(VERSIONS_GOOD_ALSO, {17, 1, 0}).
-define(VERSIONS_BAD, {6, 5, 4}).

-define(JANUARY, "JANUARY").
-define(FEBRUARY, "FEBRUARY").
-define(MARCH, "MARCH").
-define(APRIL, "APRIL").
-define(MAY, "MAY").
-define(JUNE, "JUNE").
-define(JULY, "JULY").
-define(AUGUST, "AUGUST").
-define(SEPTEMBER, "SEPTEMBER").
-define(OCTOBER, "OCTOBER").
-define(NOVEMBER, "NOVEMBER").
-define(DECEMBER, "DECEMBER").

-define(IFT_NODE, node1).

-define(PID_BY_CHILD, pidByChild).

-type config()  :: [{any(), any()}].


-record(dstChangeTime, {month=0     :: string(),
			day=""      :: string(),
			time=""     :: string()}).

suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink, []},
		 {rct_rpc, rpc1},
                 {rct_logging, {rabbe_logs, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		 {rct_proxy, [{1, ?IFT_NODE, ssh_lmt_ipv4, du1, username}]},
		 {rct_core, []},
		 {rct_cli, {cli1, [manual_connect]}}]}].



init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ?SAY("============================================ testcase: ~w", [TestCase]),
    try
	lists:foreach(
	  fun(Child) ->
		  {ok, client_started} =
		      rct_proxy:start_proxy(
			?IFT_NODE, Child, ?TZII),
		  {ok, memory_initiated} =
		      rct_proxy:send_proxy(
			?IFT_NODE, Child, ?iftRequest_initiateMemory)
	  end,
	  children(TestCase)),
	PidByChild =
	    [{Child, getChildPid(?IFT_NODE, Child)}
	     ||Child <- children(TestCase)],
	NewConfig =
	    [{?PID_BY_CHILD, orddict:from_list(PidByChild)}] ++ Config,
	NewConfig
    catch
	X:Y ->
	    ct:fail(
	      "init_per_suite/1 failed, ~p~n~p",
	      [{X, Y}, erlang:get_stacktrace()])
    end.

end_per_testcase(TestCase, _Config) ->
    rct_rpc:call(rpc1, timServer, testControl, [clearSignalRecords, []], 4000),
    simFuture(0),
    rct_rpc:call(rpc1, timServer, testControl, [setTraceLevel, [0]], 4000),
    lists:foreach(
      fun(Child) ->
	      {ok, ?CELLO_TZII_OK} =
		  rct_proxy:send_proxy(
		    ?IFT_NODE, Child, ?iftRequest_freeMemory),
	      {ok, client_stopped} =
		  rct_proxy:stop_proxy(
		    ?IFT_NODE, Child)
      end,
      lists:reverse(children(TestCase))),
    ok.

groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     %% This suite can be run on both MPs within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]},
     {sbc__cluster__dual_sim_3__1__group, [], [{group, default__group}]}
    ].


%%% ----------------------------------------------------------
%%% @doc Returns the list of children needed for the given
%%% test case.
%%% @end
%%% ----------------------------------------------------------
children(test_memory_not_initiated) -> [tzii1];
children(test_dst_start_simple) ->     [tzii2];
children(test_dst_start) ->            [tzii1, tzii2, tzii3, tzii4, tzii5, tzii6, tzii7];
children(test_dst_end) ->              [tzii1, tzii2, tzii6];
children(test_dst_ts_update) ->        [tzii1];
children(test_leap) ->                 [tzii1];
children(test_leap_a) ->               [tzii1];
children(test_leap_b) ->               [tzii1];
children(test_leap_reconfig) ->        [tzii1, tzii2, tzii3, tzii4, tzii5];
children(test_neg_leap_prewarn) ->     [tzii1];
children(test_timesettings_update) ->  [tzii1, tzii2];
children(test_timestep_forward) ->     [tzii1];
children(test_timestep_backward) ->    [tzii1];
children(test_get_versions) ->         [tzii1];
children(test_neg_if_mgmt) ->          [tzii1];
children(test_set_mailbox) ->          [tzii1, tzii2];
children(test_set_mailbox_alt) ->      [tzii1, tzii2];
children(test_tzii_example) ->         [tzii1];
children(test_tzii_unconfigured) ->    [tzii1];
children(_) ->                         [].


%%% ----------------------------------------------------------
%%% @doc Report target load average during 20 * 2 seconds.
%%% @end
%%% ----------------------------------------------------------
test_load_average(_Config) ->
    {ok, SysTestServer} =
	rct_rpc:call(rpc1, sysTestServer, start, [], 4000),
    Tag = w,
    rct_rpc:call(rpc1, sysTestServer, load_average_init, [Tag, SysTestServer], 4000),
    test_load_average(Tag, 2000, 20, SysTestServer),
    {ok, ok} = rct_rpc:call(rpc1, sysTestServer, stop, [], 4000).


test_load_average(_, _, 0, _) ->
    ok;

test_load_average(Tag, Period, N, SysTestServer) ->
    timer:sleep(Period),
    {ok, {LA, II, E, NC, S}} =
	rct_rpc:call(rpc1, sysTestServer, load_average, [Tag, SysTestServer], 4000),
    ct:pal(
      "load average o/oo: ~w, IOWait+IRQ: ~w, elapsed: ~w, nCPU: ~w, sanity: ~w",
      [LA, II, E, NC, S]),
    test_load_average(Tag, Period, N-1, SysTestServer).


%%% ----------------------------------------------------------
%%% @doc Tests for pushing up coverage.
test_better_sqr(_Config) ->
    Timeout = 4000,

    {ok, Children} = rct_rpc:call(rpc1, timDataInit, children, [], Timeout),
    ct:pal("TZII children are: ~p", [Children]),

    {ok, _} = rct_rpc:call(rpc1, timDataInit, restart_strategy, [], Timeout),

    % causes a harmless cec:register/2 call
    ok = rct_rpc:call(rpc1, timDataInit, activate, [], Timeout),

    rct_rpc:call(rpc1, timLib, reportTimeProperties, [], Timeout),

    "03:17:00.000" = rct_rpc:call(rpc1, timLib, timeString, [(3*3600 + 17*60)*1000], Timeout),

    ?FALSE = rct_rpc:call(rpc1, timLib, dstStatusCode, [undefined], Timeout).






%%% ----------------------------------------------------------
%%% @doc Tests that source dependencies are satisfied.
%%% @end
%%% ----------------------------------------------------------
test_dependencies(_Config) ->
    Relations = [?CELLO_TZII_DEP, ?TEST_TZII_REQUESTS_DEP, ?CELLO_TZII_SIG_DEP],
    IgnoredDeps = ["/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/IFT/IFT_CNX9012887/IFT_CAX1033316/csrc/master.h"],
    lists:map(
      fun({Target, Deps}) ->
	      TargetTime =
		  calendar:datetime_to_gregorian_seconds(
		    filelib:last_modified(Target)),
	      lists:map(
		fun(Dep) ->
			DepTime =
			    calendar:datetime_to_gregorian_seconds(
			      filelib:last_modified(Dep)),
			Ignore = lists:member(Dep, IgnoredDeps),
			if
			    Ignore ->
				ok;
			    DepTime > TargetTime ->
				ct:fail(
				  "target needs rebuild: ~s, depending on: ~s",
				  [Target, Dep]);
			    true ->
				ok
			end
		end,
		Deps)
      end,
      Relations),
    ?SAY("dependencies verified", []),
    ok.



-record('DstRule', {month, dayRule, time}).

test_dst_rule(_Config) ->
    HourDontCare = "03:30",
    Cases =
	[% fixed day rule, well-behaved cases
	 {{ok, 736258}, 2015, 10, "22"},
	 {{ok, 736237}, 2015, 10, "1"},

	 % obviously bad cases
	 {{bad_day_rule, {2015, {'DstRule', 10, "32", HourDontCare}}}, 2015, 10, "32"},
	 {{bad_day_rule, {2015, {'DstRule', 10, "0", HourDontCare}}}, 2015, 10, "0"},

	 % leap day cases: 2016 is a leap year, 2017 is not
	 {{ok, 736388}, 2016, 2, "29"},
	 {{ok, 736388}, 2016, 2, "lastMon"},
	 {{ok, 736754}, 2017, 2, "29"},

	 % edge cases for early-in-the-month rules
	 {{ok, 736264}, 2015, 10, "Wed>=26"},
	 {{ok, 736628}, 2016, 10, "Wed>=26"},
	 {{bad_day_rule, {2017, {'DstRule', 10, "Wed>=26", HourDontCare}}}, 2017, 10, "Wed>=26"},

	 % well-behaved last-wekkday-in-month cases
	 {{ok, 736261}, 2015, 10, "lastSun"},
	 {{ok, 736267}, 2015, 10, "lastSat"},
	 {{ok, 736266}, 2015, 10, "lastFri"},
	 {{ok, 736265}, 2015, 10, "lastThu"},
	 {{ok, 736264}, 2015, 10, "lastWed"},
	 {{ok, 736263}, 2015, 10, "lastTue"},
	 {{ok, 736262}, 2015, 10, "lastMon"}

	],

    Timeout=4000,

    lists:foreach(
      fun({Expected, Year, Month, DayRule}) ->
	      case
		  rct_rpc:call(rpc1,
			      timServer,
			      testControl,
			      [testDstRule,
			       [#'DstRule'{month=Month,
					   dayRule=DayRule,
					   time=HourDontCare},
				Year]],
			      Timeout) of
		  U when U =:= Expected ->
		      ok;
		  U ->
		      ct:fail("day calculation, actual: ~p, expected: ~p", [U, Expected])
	      end
      end,
      Cases),
    ok.


test_memory_not_initiated(_Config) ->
    % free the memory that was allocated in init_per_testcase/1, this is
    % needed to set up for this negative test.
    {ok, ?CELLO_TZII_OK} = rct_proxy:send_proxy(?IFT_NODE, tzii1, ?iftRequest_freeMemory),
    {ok, ?CELLO_TZII_MEMORY_NOT_INITIATED} = svcInitHelper(?IFT_NODE, tzii1, ?VERSIONS_GOOD),
    ?SAY("correct rejection of init service when memory not initiated", []),
    % restore conditions
    {ok, memory_initiated} = rct_proxy:send_proxy(?IFT_NODE, tzii1, ?iftRequest_initiateMemory),
    ok.


%%% ----------------------------------------------------------
%%% @doc Two subscribers for leap seconds, rejected because of
%%% the very small prewarning time.
%%% @end
%%% ----------------------------------------------------------
test_leap_rejected(_Config) ->
    SmallPrewarnTimeMillis = 1000,
    ok = setMaxPrewarn(SmallPrewarnTimeMillis),
    %ok = test_leap_prewarn(),
    ok = restoreMaxPrewarn(),
    ok.

setMaxPrewarn(Millis) ->
    TimeoutMillis = 5000,
    case rct_rpc:call(
	   rpc1, timServer,
	   testControl, [setMaxPrewarn, [Millis]],
	   TimeoutMillis, print) of
	{badrpc, _Reason}=R ->
	    ct:fail("set max prewarn by RPC failed, ~p", [R]);
	{error, _Reason}=R ->
	    ct:fail("set max prewarn by RPC failed, ~p", [R]);
	ok ->
	    ?SAY("max prewarn time set to to ~w ms", [Millis]),
	    ok
    end.


restoreMaxPrewarn() ->
    TimeoutMillis = 5000,
    case rct_rpc:call(
	   rpc1, timServer,
	   testControl, [restoreMaxPrewarn, []],
	   TimeoutMillis, print) of
	{badrpc, _Reason}=R ->
	    ct:fail("restore max prewarn by RPC failed, ~p", [R]);
	{error, _Reason}=R ->
	    ct:fail("restore max prewarn by RPC failed, ~p", [R]);
	ok ->
	    ?SAY("restored max prewarn time", []),
	    ok
    end.


makeDstChangeTime(Month, Day, Hour, Minute) ->
    #dstChangeTime{month=monthName(Month),
		   day=integer_to_list(Day),
		   time=lists:flatten(
			  io_lib:format("~2..0w:~2..0w",
					[Hour, Minute]))}.



%%% ----------------------------------------------------------
%%% @doc Utility for initiating service; good versions are
%%% provided by default.
%%% @end
%%% ----------------------------------------------------------

svcInit(Node, Child, SigHandler, Config) ->
    svcInit(Node, Child, SigHandler, ?VERSIONS_GOOD, Config).


%%% ----------------------------------------------------------
%%% @doc Utility for initiating service.
%%% @end
%%% ----------------------------------------------------------
-spec svcInit(
	atom(),
	atom(),
	pid(),
	{non_neg_integer(), non_neg_integer(), non_neg_integer()},
	config()) ->
	  ok | {bad_protocol_version, non_neg_integer()}.

svcInit(Node, Child, SigHandler, ProtocolVersions, Config) ->
    ChildPid = orddict:fetch(Child, ?config(?PID_BY_CHILD, Config)),

    {ok, ok} = shApply(SigHandler, fun svcInitHelper/3, [Node, Child, ProtocolVersions]),

    {ok, [#signal{}]} = shGet(SigHandler, Node, Child, ?CELLO_TZII_SERVER_UP_IND, 2000, Config),
    {ok, ok} = shApply(SigHandler, fun internal/2, [Node, Child]),

    Expected = [?CELLO_TZII_INITIATE_SERVICE_CFM, ?CELLO_TZII_INITIATE_SERVICE_SUS],
    case shGet(SigHandler, Node, Child, Expected, 2000, Config) of
	{ok, [#signal{no=?CELLO_TZII_INITIATE_SERVICE_SUS, fields=[_SignalRev, HighestPv]}]} ->
	    {ok, ok} = shApply(SigHandler, fun internal/2, [Node, Child]),
	    ?SAY("rejected service: ~p", [{Node, Child, ChildPid}]),
	    {bad_protocol_version, HighestPv};
	{ok, [#signal{no=?CELLO_TZII_INITIATE_SERVICE_CFM}]} ->
	    {ok, ok} = shApply(SigHandler, fun internal/2, [Node, Child]),
	    ?SAY("initiated service: ~p", [{Node, Child, ChildPid}]),
	    ok
    end.


svcTerm(Node, Child, SigHandler, Config) ->
    {ok, ok} = shApply(SigHandler, fun termService/2, [Node, Child]),
    {ok, [#signal{}]} = shGet(SigHandler, Node, Child, ?CELLO_TZII_TERMINATE_SERVICE_CFM, 2000, Config),
    {ok, ok} = shApply(SigHandler, fun internal/2, [Node, Child]),
    ?SAY("terminated service: ~p", [{Node, Child}]),
    ok.


%%% ----------------------------------------------------------
%%% @doc Performs an "initiate service" call. Returns 'ok' or
%%% the actual rct_proxy result.
%%% @end
%%% ----------------------------------------------------------
-spec svcInitHelper(atom(), atom(), tuple()) -> ok|any().

svcInitHelper(Node, Child, {_PV1, _PV2, _PV3}=ProtocolVersions) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_initiateService, ProtocolVersions) of
	{ok, ?CELLO_TZII_OK} ->
	    ?SAY("svcInitHelper returns ok, child: ~p", [Child]),
	    ok;
	Other ->
	    ?SAY("svcInitHelper returns SOMETHING ELSE, child: ~p, ~p", [Child, Other]),
	    Other
    end.

%%% ----------------------------------------------------------
%%% @doc Performs a "terminate service" call. Returns 'ok' or
%%% the actual rct_proxy result.
%%% @end
%%% ----------------------------------------------------------
termService(Node, Child) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_terminateService) of
	{ok, ?CELLO_TZII_OK} ->
	    ok;
	Other ->
	    Other
    end.


%%% ----------------------------------------------------------
%%% @doc Performs an "internal" call. Returns 'ok' or
%%% the actual rct_proxy result.
%%% @end
%%% ----------------------------------------------------------
internal(Node, Child) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_internal) of
	{ok, ?CELLO_TZII_OK} ->
	    ok;
	Other ->
	    Other
    end.


%%% ----------------------------------------------------------
%%% @doc Performs an "internal" call using the proxy of a peer child.
%%% Returns 'ok' or the actual rct_proxy result.
%%% @end
%%% ----------------------------------------------------------
internalForPeer(Node, Child, PeerSpid) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_internalForPeer, {PeerSpid}) of
	{ok, ?CELLO_TZII_OK} ->
	    ok;
	Other ->
	    Other
    end.


%%% ----------------------------------------------------------
%%% @doc Performs an invalid "internal" call. For negative tets.
%%% Returns 'ok' or the actual rct_proxy result.
%%% @end
%%% ----------------------------------------------------------
internalBad(Node, Child) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_internalBad) of
	{ok, ?CELLO_TZII_OK} ->
	    ok;
	Other ->
	    Other
    end.


%%% ----------------------------------------------------------
%%% @doc Updates the TimeSettings MO.
%%% @end
%%% ----------------------------------------------------------
updateTimeSettings(Attributes) ->
    ?SAY("begin update TimeSettings", []),
    rct_cli:connect(cli1),
    L1 = cliSend(cli1, "scriptmode on", []),
    L2 = cliSend(cli1, "ManagedElement=1,NodeSupport=1,TimeSettings=1", L1),
    L3 = cliSend(cli1, "configure", L2),
    L99 =
    lists:foldl(
      fun({timeOffset=A, V}, Acc) ->
	      cliSend(cli1, atom_to_list(A)++"=\""++V++"\"", Acc);
	 ({daylightSavingTimeOffset=A, V}, Acc) ->
	      cliSend(cli1, atom_to_list(A)++"=\""++V++"\"", Acc);
	 ({gpsToUtcLeapSecondsChangeDate=A, V}, Acc) ->
	      cliSend(cli1, atom_to_list(A)++"=\""++V++"\"", Acc);
	 ({gpsToUtcLeapSeconds=A, V}, Acc) ->
	      cliSend(cli1, atom_to_list(A)++"=\""++integer_to_list(V)++"\"", Acc);
	 ({A, #dstChangeTime{month=Month, day=Day, time=Time}}, Acc) ->
	      L11 = cliSend(cli1,
			   lists:flatten(
			     io_lib:format(
			       "~w,month=~s",
			       [A, Month])), Acc),
	      L12 = cliSend(cli1,
			   lists:flatten(
			     io_lib:format(
			       "~w,dayRule=~c~s~c",
			       [A, $\", Day, $\"])), L11),
	      cliSend(cli1,
			   lists:flatten(
			     io_lib:format(
			       "~w,time=~c~s~c",
			       [A, $\", Time, $\"])), L12);
	 ({A, {Month, DayRule, Time}}, Acc) when
	   A =:= daylightSavingTimeStartDate orelse
	       A =:= daylightSavingTimeEndDate ->
	      ?SAY("+++ DEPRECATED, please use ", []),
	      L11 = cliSend(cli1,
			   lists:flatten(
			     io_lib:format(
			       "~w,month=~s",
			       [A, Month])), Acc),
	      L12 = cliSend(cli1,
			   lists:flatten(
			     io_lib:format(
			       "~w,dayRule=~c~s~c",
			       [A, $\", DayRule, $\"])), L11),
	      cliSend(cli1,
			   lists:flatten(
			     io_lib:format(
			       "~w,time=~c~s~c",
			       [A, $\", Time, $\"])), L12);
	 (Other, Acc) ->
	      R = cliSend(cli1, "abort", Acc),
	      rct_cli:disconnect(cli1),
	      ct:fail("bad attribute: ~p", [Other]),
	      R
      end,
      L3,
      Attributes
	     ),
    L100 = cliSend(cli1, "commit", L99),
    ?SAY("CLI: ~p", [lists:reverse(L100)]),
    rct_cli:disconnect(cli1),
    ?SAY("end update TimeSettings", []).


cliSend(Cli, String, Log) ->
    rct_cli:send(Cli, String),
    [String|Log].


%%% ----------------------------------------------------------
%%% @doc Resets the TimeSettings MO.
%%% @end
%%% ----------------------------------------------------------
resetTimeSettings() ->
    _RC = rct_cli:connect(cli1),
    %?SAY("*** connect returned: ~p", [RC]),
    rct_cli:send(cli1, "scriptmode on"),
    rct_cli:send(cli1, "ManagedElement=1,NodeSupport=1,TimeSettings=1"),
    rct_cli:send(cli1, "configure"),
    rct_cli:send(cli1, "no gpsToUtcLeapSeconds"),
    rct_cli:send(cli1, "no gpsToUtcLeapSecondsChangeDate"),
    rct_cli:send(cli1, "no daylightSavingTimeOffset"),
    rct_cli:send(cli1, "no daylightSavingTimeStartDate"),
    rct_cli:send(cli1, "no daylightSavingTimeEndDate"),
    rct_cli:send(cli1, "timeOffset=\"+00:00\""),
    rct_cli:send(cli1, "commit"),
    _RD = rct_cli:disconnect(cli1),
    %?SAY("*** disconnect returned: ~p", [RD]),
    ok.


%%% ----------------------------------------------------------
%%% @doc Resets the TimeSettings MO.
%%% @end
%%% ----------------------------------------------------------
removeLeapSecondsChangeDate(NewLeapSeconds) ->
    _RC = rct_cli:connect(cli1),
    %?SAY("*** connect returned: ~p", [RC]),
    rct_cli:send(cli1, "scriptmode on"),
    rct_cli:send(cli1, "ManagedElement=1,NodeSupport=1,TimeSettings=1"),
    rct_cli:send(cli1, "configure"),
    rct_cli:send(cli1, "no gpsToUtcLeapSecondsChangeDate"),
    rct_cli:send(cli1, "gpsToUtcLeapSeconds="++integer_to_list(NewLeapSeconds)),
    rct_cli:send(cli1, "commit"),
    _RD = rct_cli:disconnect(cli1),
    %?SAY("*** disconnect returned: ~p", [RD]),
    ok.

-spec subscribe(atom(), atom(), pid(), dst|leapSec, non_neg_integer(), millis(), config()) ->
	  ok|rejected.

subscribe(Node, Child, SigHandler, dst, ClientInfo, PreWarningTime, Config) ->
    {ok, ok} =
	shApply(
	  SigHandler,
	  fun dstSubscribe/4,
	  [Node, Child, ClientInfo, PreWarningTime]),
    Wanted =
	[?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ,
	 ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM],
    case shGet(SigHandler, Node, Child, Wanted, 2000, Config) of
	{ok, [#signal{no=?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ, fields=FF}]} ->
	    ?SAY("subscribe DST rejected, IFT child: ~p, fields: ~p", [Child, FF]),
	    rejected;
	{ok, [#signal{no=?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM, fields=FF}]} ->
	    ?SAY("subscribe DST confirmed, IFT child: ~p, fields: ~p", [Child, FF]),
	    ok
    end;


subscribe(Node, Child, SigHandler, leapSec, ClientInfo, PreWarningTime, Config) ->
    {ok, ok} =
	shApply(
	  SigHandler,
	  fun leapSubscribe/4,
	  [Node, Child, ClientInfo, PreWarningTime]),
    Wanted =
	[?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ,
	 ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM],
    case shGet(SigHandler, Node, Child, Wanted, 2000, Config) of
	{ok, [#signal{no=?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ, fields=FF}]} ->
	    ?SAY("subscribe LEAP rejected, IFT child: ~p, fields: ~p", [Child, FF]),
	    rejected;
	{ok, [#signal{no=?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM, fields=FF}]} ->
	    ?SAY("subscribe LEAP confirmed, IFT child: ~p, fields: ~p", [Child, FF]),
	    ok
    end.


dstSubscribe(Node, Child, ClientInfo, PreWarningTime) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_subscribeDaylightSavingTime,
			      {ClientInfo, PreWarningTime}) of
	{ok, ?CELLO_TZII_OK} ->
	    ok;
	Other ->
	    Other
    end.


leapSubscribe(Node, Child, ClientInfo, PreWarningTime) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_subscribeLeapSeconds,
			      {ClientInfo, PreWarningTime}) of
	{ok, ?CELLO_TZII_OK} ->
	    ok;
	Other ->
	    Other
    end.


getChildPid(Node, Child) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_getPid) of
	{ok, ChildPid} when is_number(ChildPid) ->
	    ChildPid;
	Other ->
	    {error, Other}
    end.

%%% ----------------------------------------------------------
%%% @doc Get target wall clock time, expressed as
%%% a milliseconds value.
%%% @end
%%% ----------------------------------------------------------
-spec getBoardTime(atom(), atom()) -> {ok, integer(), integer()}|{error, any()}.

getBoardTime(Node, Child) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_boardTime) of
	{ok, Secs, Nanos, UptimeSecs} ->
	    {ok, Secs*1000 + (Nanos div 1000000), UptimeSecs};
	{error, Reason} ->
	    {error, Reason};
	Other ->
	    {error, {bad_response, Other}}
    end.


%%% ----------------------------------------------------------
%%% @doc This function can be run as a process. It will compare
%%% hub and target clocks every 2000 ms. The Diff-InitDiff
%%% value should be small if clocks behave well.
%%% @end
%%% ----------------------------------------------------------
clockCheck(Node, Child) ->
    ErlangMonotonicMillisInit =
	rct_rpc:call(rpc1, erlang, monotonic_time, [milli_seconds], 4000),
    OsSystemMillisInit =
	rct_rpc:call(rpc1, os, system_time, [milli_seconds], 4000),
    {ok, SysTestServer} =
	rct_rpc:call(rpc1, sysTestServer, start, [], 4000),
    rct_rpc:call(
      rpc1,
      sysTestServer,
      load_average_init,
      [clockCheck, SysTestServer],
      4000),
    case getBoardTime(Node, Child) of
	{error, Reason} ->
	    ?SAY("clockCheck: stopping, reason: ~p", [Reason]);
	{ok, BoardMillisInit, _UptimeSecs} ->
	    HubMillisInit = ?MILLIS(),
	    clockCheckLoop(
	      Node,
	      Child,
	      HubMillisInit,
	      BoardMillisInit,
	      ErlangMonotonicMillisInit,
	      OsSystemMillisInit,
	      SysTestServer)
    end.

clockCheckLoop(
  Node,
  Child,
  HubMillisInit,
  BoardMillisInit,
  ErlangMonotonicMillisInit,
  OsSystemMillisInit,
  SysTestServer) ->
    {ok, LoadAverage} =
	rct_rpc:call(rpc1, sysTestServer, load_average, [clockCheck, SysTestServer], 4000),
    ErlangMonotonicMillis =
	rct_rpc:call(rpc1, erlang, monotonic_time, [milli_seconds], 4000) -
	    ErlangMonotonicMillisInit,
    OsSystemMillis =
	rct_rpc:call(rpc1, os, system_time, [milli_seconds], 4000) -
	    OsSystemMillisInit,
    case getBoardTime(Node, Child) of
	{error, Reason} ->
	    ?SAY("clockCheck: stopping, reason: ~p", [Reason]);
	{ok, BoardMillisNow, UptimeSecs} ->
	    HubMillis = ?MILLIS() - HubMillisInit,
	    BoardMillis = BoardMillisNow - BoardMillisInit,
	    ?SAY("clockCheck: target C ahead: ~w, "
	    "E ahead: ~w, "
	    "OS ahead: ~w, "
	    "uptime: ~w s~n"
	    "load average: ~p",
		 [BoardMillis - HubMillis,
		  ErlangMonotonicMillis - HubMillis,
		  OsSystemMillis - HubMillis,
		  UptimeSecs,
		  LoadAverage]),
	    receive
		stop ->
		    {ok, ok} = rct_rpc:call(rpc1, sysTestServer, stop, [], 4000),
		    ?SAY("clockCheck: stopped", [])
	    after 2000 ->
		clockCheckLoop(
		  Node,
		  Child,
		  HubMillisInit,
		  BoardMillisInit,
		  ErlangMonotonicMillisInit,
		  OsSystemMillisInit,
		  SysTestServer)
	    end
    end.




setMailbox(Node, Child, OtherPid) ->
    case rct_proxy:send_proxy(Node, Child, ?iftRequest_setMailbox, {OtherPid}) of
	{ok, ?CELLO_TZII_OK} ->
	    ok;
	Other ->
	    {error, Other}
    end.


power(_X, M) when M =:= 0 ->
    1;

power(X, M) ->
    X*power(X, M-1).


%%% ----------------------------------------------------------
%%% @doc Returns time offset as an integer number of minutes.
%%% @end
%%% ----------------------------------------------------------
parseTimeOffset("+"++More) ->
    parseTimeOffsetHelper(More);

parseTimeOffset("-"++More) ->
    -parseTimeOffsetHelper(More);

parseTimeOffset(All) ->
    ct:fail("bad timeOffset: ~p", [All]).
    % parseTimeOffsetHelper(All).

parseTimeOffsetHelper([_, _, $:, _, _]=S) ->
    60*list_to_integer(string:substr(S, 1, 2)) + list_to_integer(string:substr(S, 4, 2)).


parseDstOffset([_, $:, _, _]=S) ->
    60*list_to_integer(string:substr(S, 1, 1)) + list_to_integer(string:substr(S, 3, 2)).


monthName(1) -> ?JANUARY;
monthName(2) -> ?FEBRUARY;
monthName(3) -> ?MARCH;
monthName(4) -> ?APRIL;
monthName(5) -> ?MAY;
monthName(6) -> ?JUNE;
monthName(7) -> ?JULY;
monthName(8) -> ?AUGUST;
monthName(9) -> ?SEPTEMBER;
monthName(10) -> ?OCTOBER;
monthName(11) -> ?NOVEMBER;
monthName(12) -> ?DECEMBER.


%%% ----------------------------------------------------------
%%% @doc Set simulated future by the given number of milliseconds.
%%% @end
%%% ----------------------------------------------------------
-spec simFuture(integer()) -> ok | {badrpc, any()}.

simFuture(Millis) ->
    rct_rpc:call(rpc1, timServer, testControl, [setSimFuture, [Millis]], 4000).


simTimeStep(IncrMillis) ->
    rct_rpc:call(rpc1, timServer, testControl, [simTimestep, [IncrMillis]], 4000).


%%% ----------------------------------------------------------
%%% @doc Hold until the wall clock microseconds reading is in
%%% the 400000..499999 range (the 5th tenth of the second).
%%% @end
%%% ----------------------------------------------------------
-spec synchronizeWithinSecond() -> ok.

synchronizeWithinSecond() ->
    {_, _, Micros} = erlang:timestamp(),
    if
	Micros >= 500000 ->
	    timer:sleep(((1000000 - Micros) div 1000) + 400),
	    synchronizeWithinSecond();
	Micros < 400000 ->
	    timer:sleep((400000 - Micros) div 1000),
	    synchronizeWithinSecond();
	true ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Test the "set mailbox" feature.
%%% @end
%%% ----------------------------------------------------------
test_set_mailbox(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset
    DstOffsetS = "2:22",

    % specify how many minutes ahead the DST change should happen
    % (miminmum 2, maximum is limited by TC timeouts, however if
    % simulated future is applied then timeouts are defeated)
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST start
    {_, {_, _, Sec}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    case MonthL =:= 2 andalso DayL =:= 29 of
	true ->
	    ?SAY("today is a leap day, just let the TC pass", []),
	    ok;
	_ ->

	    % calculate the local datetime of the DST end, some 150 days
	    % ahead (not significant)
	    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
		case calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
		    {{_, 2, 29}, _} ->
			?SAY("avoid ending DST on a leap day", []),
			calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
		    Other ->
			Other
		end,

	    % update the TimeSettings MO
	    updateTimeSettings(
	      [{timeOffset, TimeOffsetS},
	       {daylightSavingTimeOffset, DstOffsetS},
	       {daylightSavingTimeStartDate,
		makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
	       {daylightSavingTimeEndDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
	      ]),

	    % simulate future to speed up the TC (optional)

	    % margin from scenarios start to first expected IND
	    MarginS = 6,

	    % maximum prewarn seconds among scenarios
	    MaxPrewarnS = 30,

	    % recompute current second since the MO setting
	    % took some time
	    UtcDtNew = calendar:universal_time(),
	    UtcNewS = calendar:datetime_to_gregorian_seconds(UtcDtNew),

	    SimFutureMillis =
		(TimeToDstChangeM*60 - MaxPrewarnS - MarginS - Sec - (UtcNewS-UtcS))*1000,

	    simFuture(SimFutureMillis),

	    % scenarios
	    %DstChangeMinute = HourL*60 + MinuteL,
	    %DstOffsetMinutes = parseDstOffset(DstOffsetS),

	    sigHandler:start(?MODULE,
	      {self(),
	       30000,
	       fun(_) -> simFuture(0), resetTimeSettings() end,
	       [% a client that subscribes with 30 s prewarn time
		fun() ->
			sc_set_mailbox(tzii1, 30000, Config
%% , TimeOffsetMinutes, DstOffsetMinutes, 1, DstChangeMinute, 60000, false
				      ) end,

		% a child that will receive signals after a "set mailbox" action
		fun() -> sc_receive_signals(tzii2, Config) end
	       ]}),

	    shWait()
    end.

%%% ----------------------------------------------------------
%%% @doc Test the "set mailbox" feature: Let the setMailbox
%%% operation precede initiateService.
%%% @end
%%% ----------------------------------------------------------
test_set_mailbox_alt(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset
    DstOffsetS = "2:22",

    % specify how many minutes ahead the DST change should happen
    % (miminmum 2, maximum is limited by TC timeouts, however if
    % simulated future is applied then timeouts are defeated)
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST start
    {_, {_, _, Sec}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    case MonthL =:= 2 andalso DayL =:= 29 of
	true ->
	    ?SAY("today is a leap day, just let the TC pass", []),
	    ok;
	_ ->

	    % calculate the local datetime of the DST end, some 150 days
	    % ahead (not significant)
	    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
		case calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
		    {{_, 2, 29}, _} ->
			?SAY("avoid ending DST on a leap day", []),
			calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
		    Other ->
			Other
		end,

	    % update the TimeSettings MO
	    updateTimeSettings(
	      [{timeOffset, TimeOffsetS},
	       {daylightSavingTimeOffset, DstOffsetS},
	       {daylightSavingTimeStartDate,
		makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
	       {daylightSavingTimeEndDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
	      ]),

	    % simulate future to speed up the TC (optional)

	    % margin from scenarios start to first expected IND
	    MarginS = 6,

	    % maximum prewarn seconds among scenarios
	    MaxPrewarnS = 30,

	    % recompute current second since the MO setting
	    % took some time
	    UtcDtNew = calendar:universal_time(),
	    UtcNewS = calendar:datetime_to_gregorian_seconds(UtcDtNew),

	    SimFutureMillis =
		(TimeToDstChangeM*60 - MaxPrewarnS - MarginS - Sec - (UtcNewS-UtcS))*1000,

	    simFuture(SimFutureMillis),

	    % scenarios
	    %DstChangeMinute = HourL*60 + MinuteL,
	    %DstOffsetMinutes = parseDstOffset(DstOffsetS),

	    sigHandler:start(?MODULE,
	      {self(),
	       30000,
	       fun(_) -> simFuture(0), resetTimeSettings() end,
	       [% a client that subscribes with 30 s prewarn time
		fun() -> sc_set_mailbox_alt(tzii1, 30000, Config) end,

		% a child that will receive signals after a "set mailbox" action
		fun() -> sc_receive_signals_alt(tzii2, Config) end
	       ]}),

	    shWait()
    end.


%%% ----------------------------------------------------------
%%% @doc Reduced version of test_dst_start; just one scenario.
%%% @end
%%% ----------------------------------------------------------
test_dst_start_simple(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset
    DstOffsetS = "2:22",

    % specify how many minutes ahead the DST change should happen
    % (miminmum 2, maximum is limited by TC timeouts, however if
    % simulated future is applied then timeouts are defeated)
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST start
    {_, {_, _, Sec}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    case MonthL =:= 2 andalso DayL =:= 29 of
	true ->
	    ?SAY("today is a leap day, just let the TC pass", []),
	    ok;
	_ ->

	    % calculate the local datetime of the DST end, some 150 days
	    % ahead (not significant)
	    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
		case calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
		    {{_, 2, 29}, _} ->
			?SAY("avoid ending DST on a leap day", []),
			calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
		    Other ->
			Other
		end,

	    % update the TimeSettings MO
	    updateTimeSettings(
	      [{timeOffset, TimeOffsetS},
	       {daylightSavingTimeOffset, DstOffsetS},
	       {daylightSavingTimeStartDate,
		makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
	       {daylightSavingTimeEndDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
	      ]),

	    % simulate future to speed up the TC (optional)

	    % margin from scenarios start to first expected IND
	    MarginS = 6,

	    % maximum prewarn seconds among scenarios
	    MaxPrewarnS = 30,

	    % recompute current second since the MO setting
	    % took some time
	    UtcDtNew = calendar:universal_time(),
	    UtcNewS = calendar:datetime_to_gregorian_seconds(UtcDtNew),

	    SimFutureMillis =
		(TimeToDstChangeM*60 - MaxPrewarnS - MarginS - Sec - (UtcNewS-UtcS))*1000,

	    simFuture(SimFutureMillis),

	    % scenarios
	    DstChangeMinute = HourL*60 + MinuteL,
	    DstOffsetMinutes = parseDstOffset(DstOffsetS),

	    sigHandler:start(?MODULE,
	      {self(),
	       30000,
	       fun(_) -> simFuture(0), resetTimeSettings() end,
	       [% a client that subscribes with 30 s prewarn time
		fun() ->
			sc_dst(
			  tzii2, 30000, TimeOffsetMinutes, DstOffsetMinutes, 1,
			  DstChangeMinute, 20000, false, Config) end
	       ]}),

	    shWait()
    end.




%%% ----------------------------------------------------------
%%% @doc Test that the correct IND is obtained when DST starts.
%%% 4 parameters can be varied; the TC should succeed for any
%%% combination. Simulated future is used to speed up the TC;
%%% turning it off should not affect successful outcome.
%%% @end
%%% ----------------------------------------------------------
test_dst_start(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset
    DstOffsetS = "2:22",

    % specify how many minutes ahead the DST change should happen
    % (miminmum 2, maximum is limited by TC timeouts, however if
    % simulated future is applied then timeouts are defeated)
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST start
    {_, {_, _, Sec}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    case MonthL =:= 2 andalso DayL =:= 29 of
	true ->
	    ?SAY("today is a leap day, just let the TC pass", []),
	    ok;
	_ ->

	    % calculate the local datetime of the DST end, some 150 days
	    % ahead (not significant)
	    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
		case calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
		    {{_, 2, 29}, _} ->
			?SAY("avoid ending DST on a leap day", []),
			calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
		    Other ->
			Other
		end,

	    % update the TimeSettings MO
	    updateTimeSettings(
	      [{timeOffset, TimeOffsetS},
	       {daylightSavingTimeOffset, DstOffsetS},
	       {daylightSavingTimeStartDate,
		makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
	       {daylightSavingTimeEndDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
	      ]),

	    % simulate future to speed up the TC (optional)

	    % margin from scenarios start to first expected IND
	    MarginS = 4,

	    % maximum prewarn seconds among scenarios
	    MaxPrewarnS = 30,

	    % synchronize to a defined fraction of the wall clock second
	    synchronizeWithinSecond(),

	    % recompute current second since the MO setting
	    % took some time
	    UtcDtNew = calendar:universal_time(),
	    UtcNewS = calendar:datetime_to_gregorian_seconds(UtcDtNew),

	    SimFutureMillis =
		(TimeToDstChangeM*60 - MaxPrewarnS - MarginS - Sec - (UtcNewS-UtcS))*1000,

	    ?SAY("simulate future: ~w", [SimFutureMillis]),
	    simFuture(SimFutureMillis),

	    rct_rpc:call(rpc1, timServer, testControl, [setTraceLevel, [3]], 4000),

	    % scenarios
	    DstChangeMinute = HourL*60 + MinuteL,
	    DstOffsetMinutes = parseDstOffset(DstOffsetS),

	    ?SAY("start signal handler", []),
	    sigHandler:start(?MODULE,
	      {self(),
	       30000,
	       fun(_) -> simFuture(0), resetTimeSettings() end,
	       [% a client that subscribes with 25 s prewarn time
		fun() ->
			sc_dst(
			  tzii1, 25000, TimeOffsetMinutes, DstOffsetMinutes, 1,
			  DstChangeMinute, 60000, false, Config) end,

		% a client that subscribes with 30 s prewarn time
		fun() ->
			sc_dst(
			  tzii2, 30000, TimeOffsetMinutes, DstOffsetMinutes, 1,
			  DstChangeMinute, 20000, false, Config) end,

		% a client that subscribes with just 1 s prewarn time; waiting
		% for the signal will timeout before the IND would arrive
		fun() ->
			sc_dst(
			  tzii3, 1000, TimeOffsetMinutes, DstOffsetMinutes, 1,
			  DstChangeMinute, 15000, true, Config) end,

		% a client that just initiates and terminates service
		fun() -> sc_idle(tzii4, 4500, 3, Config) end,

		% a client that requests unsupported protocol versions
		fun() -> sc_bad_pv(tzii5, Config) end,

		% a client that subscribes with 20 s prewarn time initially
		% and resubscribes with 10 s later on
		fun() -> sc_resubscribe(tzii6, TimeOffsetMinutes, DstOffsetMinutes, 1,
					DstChangeMinute, MarginS, MaxPrewarnS, Config) end
	       ]}),

	    shWait(),
	    ?SAY("signal handler has ended", [])
    end.


%%% ----------------------------------------------------------
%%% @doc Test that the correct IND is obtained when DST ends.
%%% 4 parameters can be varied; the TC should succeed for any
%%% combination. Simulated future is used to speed up the TC;
%%% turning it off should not affect successful outcome.
%%% @end
%%% ----------------------------------------------------------
test_dst_end(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "+05:00",

    % specify a DST offset
    DstOffsetS = "1:00",

    % specify how many minutes ahead the DST change should happen
    % (miminmum 2, maximum is limited by TC timeouts, however if
    % simulated future is applied then timeouts are defeated)
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST end
    {_, {_, _, Sec}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    DstOffsetMinutes = parseDstOffset(DstOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    case MonthL =:= 2 andalso DayL =:= 29 of
	true ->
	    ?SAY("running on a leap day, just let the TC pass!", []),
	    ok;
	_ ->
	    % calculate the local datetime of the DST start, some 150 days
	    % ahead (not significant), only make sure NOT to hit a leap day
	    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
		case calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
		    {{_, 2, 29}, _} ->
			?SAY("avoiding DST change on leap day", []),
			calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
		    Other ->
			Other
		end,

	    % update the TimeSettings MO
	    updateTimeSettings(
	      [{timeOffset, TimeOffsetS},
	       {daylightSavingTimeOffset, DstOffsetS},
	       {daylightSavingTimeEndDate,
		makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
	       {daylightSavingTimeStartDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
	      ]),

	    % simulate future to speed up the TC (optional)

	    % margin from scenarios start to first expected IND
	    MarginS = 4,

	    % maximum prewarn seconds among scenarios
	    MaxPrewarnS = 30,

	    % recompute current second since the MO setting
	    % took some time
	    UtcDtNew = calendar:universal_time(),
	    UtcNewS = calendar:datetime_to_gregorian_seconds(UtcDtNew),

	    SimFutureMillis =
		(TimeToDstChangeM*60 - MaxPrewarnS - MarginS - Sec - (UtcNewS-UtcS))*1000,

	    simFuture(SimFutureMillis),

	    % scenarios
	    DstChangeMinute = HourL*60 + MinuteL,
	    DstOffsetMinutes = parseDstOffset(DstOffsetS),

	    sigHandler:start(?MODULE,
	      {self(),
	       30000,
	       fun(_) -> simFuture(0), resetTimeSettings() end,
	       [fun() -> sc_dst(tzii1, 25000,
				TimeOffsetMinutes, DstOffsetMinutes, 0,
				DstChangeMinute, 20000, false, Config) end,
		fun() -> sc_dst(tzii2, 30000,
				TimeOffsetMinutes, DstOffsetMinutes, 0,
				DstChangeMinute, 20000, false, Config) end,

		% also test a stupid client that tries to initiate service twice
		fun() -> sc_init_twice(tzii6, Config) end
	       ]}),

	    shWait()
    end.


%%% ----------------------------------------------------------
%%% @doc Execute a scenario that tries to set up service
%%% asking for an unsupported protocol version.
%%% @end
%%% ----------------------------------------------------------
sc_bad_pv(Child, Config) ->
    receive {pid, SigHandler} -> ok end,
    ?SAY("running scenario: ~w, ~p", [Child, self()]),
    try
	Result = svcInit(?IFT_NODE, Child, SigHandler, ?VERSIONS_BAD, Config),
	case Result of
	    {bad_protocol_version, H} ->
		?SAY("as expected, no service; suggested PV is: ~p", [H]),
		ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
		?SAY("end scenario: ~w, ~p", [Child, self()]),
		sigHandler:cast(SigHandler, {done, self()});
	    Other ->
		sigHandler:cast(SigHandler, {done, self(), {fail, {bad_pv, Other}}})
	end
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.


%%% ----------------------------------------------------------
%%% @doc Execute a scenario that just sets up the service,
%%% stays up for a while listening for signals and then
%%% terminates the service. A repeat count is required: 1
%%% or higher.
%%% @end
%%% ----------------------------------------------------------
sc_idle(Child, TimeoutMillis, NofTimes, Config) ->
    receive {pid, SigHandler} -> ok end,
    ?SAY("running scenario: ~w, ~p", [Child, self()]),

    try
	Failures =
	    lists:foldl(
	      fun(U, Acc) ->
		      ok = svcInit(?IFT_NODE, Child, SigHandler, Config),
		      case shGet(
			     SigHandler,
			     ?IFT_NODE,
			     Child,
			     undefined,
			     TimeoutMillis,
			     Config) of
			  timeout ->
			      ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
			      Acc;
			  {ok, Signals} ->
			      % unlikely of course
			      ?SAY("unexpectedly got some signals: ~p", [Signals]),
			      ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
			      Acc++[{unexpected_signals, U, Signals}]
		      end
	      end,
	      [],
	      lists:seq(1, NofTimes)),
	if
	    Failures =/= [] ->
		sigHandler:cast(SigHandler, {done, self(), {fail, Failures}});
	    true ->
		?SAY("end scenario: ~w, ~p", [Child, self()]),
		sigHandler:cast(SigHandler, {done, self()})
	end
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.


%%% ----------------------------------------------------------
%%% @doc This scenario receives signals after a setMailbox
%%% operation.
%%% TODO, validate fields of the received signal
%%% @end
%%% ----------------------------------------------------------
sc_receive_signals(Child, Config) ->
    receive {pid, SigHandler} -> ok end,
    % first thing is to share my spid
    ChildPid = orddict:fetch(Child, ?config(?PID_BY_CHILD, Config)),

    ?SAY("~w, ~w, child pid: ~w: receiver of signals", [Child, self(), ChildPid]),
    register(sc_receive_signals, self()),
    receive
	{getSpid, Caller} ->
	    Caller ! ChildPid,
	    ?SAY("~w, child spid shared: ~p", [Child, ChildPid]),

	    % receive any signals
	    Result =
		sigHandler:getSignals(
		  SigHandler,
		  {ChildPid, undefined},
		  20000),
	    ?SAY("~w, received: ~p", [Child, Result]),

	    % expect nothing more
	    case sigHandler:getSignals(SigHandler, {ChildPid, undefined}, 10000) of
		timeout ->
		    ?SAY("~w, timeout as expected", [Child]),
		    sigHandler:cast(SigHandler, {done, self()});
		Other ->
		    ?SAY("~w, unexpected: ~p", [Child, Other]),
		    sigHandler:cast(SigHandler, {done, self(), {fail, {unexpected, Other}}})
	    end
    after 30000 ->
	?SAY("~w, failed to share child spid", [Child]),
	sigHandler:cast(SigHandler, {done, self(), {fail, child_pid_not_shared}})
    end.


%%% ----------------------------------------------------------
%%% @doc Scenario:
%%%         init service
%%%         subscribe for DST
%%%         set mailbox so that tzii2 gets the DST pre-warning
%%%         terminate service
%%% @end
%%% ----------------------------------------------------------
sc_set_mailbox(Child, PrewarnMillis, Config) ->

    receive {pid, SigHandler} -> ok end,
    ?SAY("running scenario: ~p", [self()]),

    try
	ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

	ClientInfo = clientInfo(Child),

	case subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo, PrewarnMillis, Config) of
	    rejected ->
		% should definitely not happen
		ct:fail("subscription rejected for PreWarn: ~w", [PrewarnMillis]);
	    ok ->
		% now set the mailbox!
		case lookupPid(sc_receive_signals, 9) of
		    undefined ->
			ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
			sigHandler:cast(SigHandler, {done, self(), {fail, no_other_mailbox}});
		    OtherScenario ->
			OtherScenario ! {getSpid, self()},
			OtherChildPid =
			    receive
				R ->
				    R
			    after 60000 ->
				0
			    end,
			?SAY("~w: other child spid: ~p", [Child, OtherChildPid]),
			ApplyResult = shApply(SigHandler, fun setMailbox/3, [?IFT_NODE, Child, OtherChildPid]),
			?SAY("apply result: ~p", [ApplyResult]),

			Result = shGet(
				    SigHandler,
				    ?IFT_NODE,
				    Child,
				    undefined,
				    15000,
				    Config),

			?SAY("~w, expected is timeout: ~p", [Child, Result]),

			% reset the mailbox
			ApplyResult2 = shApply(SigHandler, fun setMailbox/3, [?IFT_NODE, Child, 0]),
			?SAY("second apply result: ~p", [ApplyResult2]),

			ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
			sigHandler:cast(SigHandler, {done, self()})
		end
	end
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.


%%% ----------------------------------------------------------
%%% @doc Scenario:
%%%        register own Erlang pid with name
%%%        expect a query from peer; save peer child spid for later
%%%        respond with own child spid, to be used by peer doing 'setMailbox'
%%%        receive UP_IND
%%%        do 'internal' with peer proxy pointer
%%%        receive SERVICE_CFM
%%%        do 'internal' with peer proxy pointer
%%%        tell peer to subscribe for DST
%%%        verify SUBSCRIBE_CFM
%%%        verify an IND
%%%        tell peer to terminate service
%%%        receive TER_CFM
%%%        do 'internal' with peer proxy pointer
%%% @end
%%% ----------------------------------------------------------
sc_receive_signals_alt(Child, Config) ->
    receive {pid, SigHandler} -> ok end,
    ?SAY("    scenario 'receive signals' is running: ~w, ~p", [Child, self()]),
    ChildPid = orddict:fetch(Child, ?config(?PID_BY_CHILD, Config)),
    ?SAY("    ~w, ~w, child pid: ~w: receiver of signals", [Child, self(), ChildPid]),

    register(sc_receive_signals, self()),

    % this request is executed for its side effect: Signals from the TZI service
    % will go to the signal handler process
    {ok, _ChildPid} = shApply(SigHandler, fun getChildPid/2, [?IFT_NODE, Child]),

    receive
	{getSpid, Caller, PeerSpid} ->
	    Caller ! ChildPid,
	    ?SAY("    ~w, child spid shared: ~p", [Child, ChildPid]),

	    % receive any signals; expect a 'server up'
	    {ok, [#signal{no=?CELLO_TZII_SERVER_UP_IND}]} =
		sigHandler:getSignals(SigHandler, {ChildPid, undefined}, 20000),
	    ?SAY("    ~w, received: ~s", [Child, signalNameX(?CELLO_TZII_SERVER_UP_IND)]),
	    {ok, ok} = shApply(SigHandler, fun internalForPeer/3, [?IFT_NODE, Child, PeerSpid]),

	    % receive any signals; expect a 'service cfm'
	    Result2 = sigHandler:getSignals(SigHandler, {ChildPid, undefined}, 20000),
	    ?SAY("    ~w, received: ~p", [Child, Result2]),
	    {ok, [#signal{no=?CELLO_TZII_INITIATE_SERVICE_CFM}]} = Result2,
	    {ok, ok} = shApply(SigHandler, fun internalForPeer/3, [?IFT_NODE, Child, PeerSpid]),

	    Caller ! proceedToSubscribe,

	    % receive any signals; expect a 'DST subscribe cfm'
	    Result3 = sigHandler:getSignals(SigHandler, {ChildPid, undefined}, 20000),
	    ?SAY("    ~w, received: ~p", [Child, Result3]),
	    {ok, [#signal{no=?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM}]} = Result3,

	    % receive any signals; expect a 'DST IND'
	    Result4 = sigHandler:getSignals(SigHandler, {ChildPid, undefined}, 20000),
	    ?SAY("    ~w, received: ~p", [Child, Result4]),
	    {ok, [#signal{no=?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND}]} = Result4,

	    Caller ! proceedToTerminate,

	    % receive any signals; expect a 'terminate service CFM'
	    Result5 = sigHandler:getSignals(SigHandler, {ChildPid, undefined}, 20000),
	    ?SAY("    ~w, received: ~p", [Child, Result5]),
	    {ok, [#signal{no=?CELLO_TZII_TERMINATE_SERVICE_CFM}]} = Result5,
	    {ok, ok} = shApply(SigHandler, fun internalForPeer/3, [?IFT_NODE, Child, PeerSpid]),

	    sigHandler:cast(SigHandler, {done, self()}),
	    ?SAY("    ~w: scenario ends", [Child])
    after 30000 ->
	?SAY("    ~w, failed to share child spid", [Child]),
	sigHandler:cast(SigHandler, {done, self(), {fail, child_pid_not_shared}})
    end.


%%% ----------------------------------------------------------
%%% @doc Scenario:
%%%        set mailbox, redirecting signals to the peer child
%%%        init service
%%%        wait until peer tells us that service is established
%%%        subscribe for DST changes
%%%        wait until peer tells us to terminate service
%%% @end
%%% ----------------------------------------------------------
sc_set_mailbox_alt(Child, PrewarnMillis, Config) ->
    Node = ?IFT_NODE,
    ChildPid = orddict:fetch(Child, ?config(?PID_BY_CHILD, Config)),

    receive {pid, SigHandler} -> ok end,
    ?SAY("scenario 'set mailbox' is running: ~w, ~p", [Child, self()]),

    try
	case lookupPid(sc_receive_signals, 9) of
	    undefined ->
		% not supposed to happen!
		ok = svcTerm(Node, Child, SigHandler, Config),
		sigHandler:cast(SigHandler, {done, self(), {fail, no_other_mailbox}});
	    OtherScenario ->
		OtherScenario ! {getSpid, self(), ChildPid},
		OtherChildPid =
		    receive
			R ->
			    R
		    after 60000 ->
			0
		    end,
		?SAY("~w: other child spid: ~p", [Child, OtherChildPid]),
		ApplyResult = shApply(SigHandler, fun setMailbox/3, [?IFT_NODE, Child, OtherChildPid]),
		?SAY("setMailbox result: ~p", [ApplyResult]),

		{ok, ok} = shApply(SigHandler, fun svcInitHelper/3, [Node, Child, ?VERSIONS_GOOD]),
		?SAY("~w: done 'initiate service' request", [Child]),

		receive
		    proceedToSubscribe ->
			ok
		end,

		% now subscribe...
		ClientInfo = clientInfo(Child),
		{ok, ok} = shApply(SigHandler, fun dstSubscribe/4, [Node, Child, ClientInfo, PrewarnMillis]),
		?SAY("subscribe DST requested: ~p", [{Node, Child}]),

		receive
		    proceedToTerminate ->
			ok
		end,

		% now terminate
		{ok, ok} = shApply(SigHandler, fun termService/2, [Node, Child]),
		?SAY("terminate service requested: ~p", [{Node, Child}]),


		% end scenario
		sigHandler:cast(SigHandler, {done, self()}),
		?SAY("scenario ends: ~p", [{Node, Child}])
	end
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.


lookupPid(_Name, 0) ->
    undefined;

lookupPid(Name, Count) ->
    case whereis(Name) of
	undefined ->
	    timer:sleep(500),
	    lookupPid(Name, Count - 1);
	Pid ->
	    Pid
    end.


%%% ----------------------------------------------------------
%%% @doc Execute a simple scenario that represents a client that
%%% subscribes for DST changes and receives one DST indication.
%%% The Child parameter must be given as an atom where the 5th
%%% character of the name is a digit, e g 'tzii5'.
%%%
%%% The NewDstStatus parameter should be 1 if the scenario is
%%% that DST is becomig active, 0 otherwise.
%%% @end
%%% ----------------------------------------------------------
sc_dst(
  Child,
  PrewarnMillis,
  TimeOffsetToUtc,
  DstOffsetMinutes,
  NewDstStatus,
  DstChangeMinute,
  TimeoutMillis,
  TimeoutExpected,
  Config) ->

    receive {pid, SigHandler} -> ok end,
    ?SAY("running scenario: ~w, ~p", [Child, self()]),

    try
	ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

	ClientInfo = clientInfo(Child),

	case subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo, PrewarnMillis, Config) of
	    rejected ->
		% should definitely not happen
		ct:fail("subscription rejected for PreWarn: ~w", [PrewarnMillis]);
	    ok ->
		case shGet(
		      SigHandler,
		      ?IFT_NODE,
		      Child,
		      ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND,
		      TimeoutMillis,
		      Config) of
		    timeout ->
			if
			    TimeoutExpected ->
				ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
				sigHandler:cast(SigHandler, {done, self()});
			    not(TimeoutExpected) ->
				ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
				sigHandler:cast(SigHandler, {done, self(), {fail, {timeout, {Child, TimeoutMillis}}}})
			end;
		    {ok, [#signal{fields=Fields}]} ->

			?SAY("got DST IND for ~w, fields: ~p", [Child, Fields]),

			[ClientInfo,
			 TimeOffsetToUtc,
			 DstOffsetMinutes,
			 NewDstStatus,
			 DstChangeMinute,
			 ?CELLO_TZII_PREWARNING_START] = Fields,

			ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
			?SAY("end scenario: ~w, ~p", [Child, self()]),
			sigHandler:cast(SigHandler, {done, self()})
		end
	end
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.


%%% ----------------------------------------------------------
%%% @doc This scenario represents a client that subscribes for
%%% DST changes repeatedly, asking for a different prewarning
%%% time the second time.
%%% @end
%%% ----------------------------------------------------------
sc_resubscribe(
  Child,
  TimeOffsetToUtc,
  DstOffsetMinutes,
  NewDstStatus,
  DstChangeMinute,
  MarginS,
  MaxPrewarnS,
  Config) ->

    receive {pid, SigHandler} -> ok end,
    ?SAY("running scenario: ~w, ~p", [Child, self()]),

    ClockCheck = spawn(fun() -> clockCheck(?IFT_NODE, tzii7) end),

    ScStart = ?MILLIS(),

    try
	ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

	ClientInfo = clientInfo(Child),

	% subscribe with 20 s prewarning;
	% expect the IND in (MaxPrewarnS-20)+MarginS = 14 s
	PrewarnMillis1 = 20000,
	ok = subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo, PrewarnMillis1, Config),

	% expect nothing at all for the nearest 7 s (used to be 10 s)
	% erarafo 2016-02-18: 10000 is sometimes too long to wait,
	% because of network slowness or whatever; change to 7000.
	% etxpeno 2017-03-23: Try 3000 since 7000 seems to be too long in some
	% cases
	timeout = shGet(SigHandler, ?IFT_NODE, Child, undefined, 3000, Config),

	% now resubscribe for 10 s prewarning instead
	PrewarnMillis2 = 10000,
	ClientInfo2 = ClientInfo + 2000000,
	ok = subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo2, PrewarnMillis2, Config),

	% and wait for the IND, to arrive approximately (MaxPrewarnS-10)+MarginS =  24 s after
	% scenario start
	{ok, [#signal{fields=Fields}]} =
	    shGet(
	      SigHandler,
	      ?IFT_NODE,
	      Child,
	      ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND,
	      33000,
	      Config),

	ExpectedScenarioTime = MaxPrewarnS*1000 - PrewarnMillis2 + MarginS*1000,
	Deviation = ExpectedScenarioTime - (?MILLIS() - ScStart),

	?SAY("got DST IND for ~w, fields: ~p", [Child, Fields]),

	?SAY("smallish deviation from the expected ~w ms: ~w",
	     [ExpectedScenarioTime, Deviation]),

	[ClientInfo2,
	 TimeOffsetToUtc,
	 DstOffsetMinutes,
	 NewDstStatus,
	 DstChangeMinute,
	 ?CELLO_TZII_PREWARNING_START] = Fields,

	ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),

	ClockCheck ! stop,

	if
	    %% Change the acceptable deviation from 2000 to 4000. The deviation
	    %% has increased due to the change in ntp.conf "tinker step 3"
	    %% (TR HV53411)
	    %% Increase it to 5000
	    abs(Deviation) > 5000 ->
		sigHandler:cast(SigHandler,
				{done, self(), {fail, {deviation, Deviation}}});
	    true ->
		?SAY("end scenario: ~w, ~p", [Child, self()]),
		sigHandler:cast(SigHandler, {done, self()})
	end
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.




%%% ----------------------------------------------------------
%%% @doc Execute a scenario that initiates the service, stupidly
%%% tries to initiate the service a second time, and then
%%% terminates the service.
%%% @end
%%% ----------------------------------------------------------
sc_init_twice(Child, Config) ->
    Node = ?IFT_NODE,
    ProtocolVersions = ?VERSIONS_GOOD,
    receive {pid, SigHandler} -> ok end,
    ?SAY("running scenario: ~p, ~p", [Child, self()]),

    try
	% first init service, should be fine
	ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

	timer:sleep(2000),

	% now be stupid
	%nok = svcInit(?IFT_NODE, Child, SigHandler),
	What =
	    shApply(SigHandler, fun svcInitHelper/3, [Node, Child, ProtocolVersions]),
	?SAY("being stupid, got: ~p", [What]),
	{ok, {ok, ?CELLO_TZII_ALREADY_INITIATED}} = What,

%% 	% we still expect this one
%% 	{ok, [#signal{}]} = shPoll(SigHandler, Node, Child, ?CELLO_TZII_SERVER_UP_IND, 500, 2000),
%% 	?SAY("being stupid, got SERVER_UP_IND anyway: ~p", [What]),
%% 	{ok, ok} = shApply(SigHandler, fun internal/2, [Node, Child]),
%%
%% 	Expected = [?CELLO_TZII_INITIATE_SERVICE_REJ],
%% 	case shPoll(SigHandler, Node, Child, Expected, 500, 2000) of
%% 	    {ok, [#signal{no=?CELLO_TZII_INITIATE_SERVICE_REJ}]} ->
%% 		{ok, ok} = shApply(SigHandler, fun internal/2, [Node, Child]),
%% 		?SAY("handled 'reject': ~p", [{Node, Child, ChildPid}]),
%% 		ok
%% 	end,

	% no more signals expected; listen for 5000 ms
	case shGet(
	       SigHandler,
	       ?IFT_NODE,
	       Child,
	       undefined,
	       5000,
	       Config) of
	    timeout ->
		ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
		sigHandler:cast(SigHandler, {done, self()});
	    {ok, Signals} ->
		?SAY("unexpectedly got some signals: ~p", [Signals]),
		ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
		sigHandler:cast(SigHandler, {done, self(), {fail, {unexpected_signals, Signals}}})
	end
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.


%%% ----------------------------------------------------------
%%% @doc Test a scenario where TS attributes are updated.
%%% @end
%%% ----------------------------------------------------------
test_dst_ts_update(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset
    DstOffsetS = "2:22",


    % specify the DST start to happen in about 1 hour
    % (we will not wait for it to happen)
    TimeToDstChangeM = 60,

    % calculate the local datetime of the DST start
    UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    case MonthL =:= 2 andalso DayL =:= 29 of
	true ->
	    ?SAY("today is a leap day, just let the TC pass", []),
	    ok;
	_ ->
	    % calculate the local datetime of the DST end, some 150 days
	    % ahead (not significant)
	    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
		case calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
		    {{_, 2, 29}, _} ->
			?SAY("avoid ending DST on a leap day", []),
			calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
		    Other ->
			Other
		end,

	    % update the TimeSettings MO
	    updateTimeSettings(
	      [{timeOffset, TimeOffsetS},
	       {daylightSavingTimeOffset, DstOffsetS},
	       {daylightSavingTimeStartDate,
		makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
	       {daylightSavingTimeEndDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
	      ]),

	    % scenarios
	    sigHandler:start(?MODULE,
	      {self(),
	       30000,
	       fun(_) -> resetTimeSettings() end,
	       [% an actor that updates TimeSettings
		fun() -> sc_dst_ts_update_operator(MonthR, DayR, HourR, MinuteR) end,

		% a client that subscribes for DST changes
		fun() -> sc_dst_ts_update_1(tzii1, Config) end
	       ]}),

	    shWait()
    end.


sc_dst_ts_update_operator(MonthR, DayR, HourR, MinuteR) ->
    MinuteDelta = if MinuteR =:= 0 -> 1; true -> -1 end,
    receive {pid, SigHandler} -> ok end,

    timer:sleep(3000),
    	    updateTimeSettings(
	      [{daylightSavingTimeEndDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR + MinuteDelta)}
	      ]),

    sigHandler:cast(SigHandler, {done, self()}).


sc_dst_ts_update_1(Child, Config) ->
    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),


    ok = subscribe(?IFT_NODE, Child, SigHandler, dst, 12345, 30000, Config),

    % not expected to receive anything
    What = shGet(SigHandler, ?IFT_NODE, Child, undefined, 10000, Config),
    ?SAY("what: ~p", [What]),

    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),

    sigHandler:cast(SigHandler, {done, self()}).













%%% ----------------------------------------------------------
%%% @doc Test a scenario where the leap actually happens.
%%% Uses the simulated future feature.
%%% @end
%%% ----------------------------------------------------------
test_leap(Config) ->
    test_leap_helper("+02:00", "", 3000, true, Config).

test_leap_a(Config) ->
    test_leap_helper("-11:30", "", 3000, true, Config).

test_leap_b(Config) ->
    test_leap_helper("+02:00", "-", 3000, true, Config).


%%% ----------------------------------------------------------
%%% @doc Trying to subscribe with a too large prewarning time.
%%% @end
%%% ----------------------------------------------------------
test_neg_leap_prewarn(Config) ->
    test_leap_helper("+01:00", "", 23*3600*1000 + 1, false, Config).


test_leap_helper(
  TimeOffset,
  IncrementPrefix,
  PrewarnMillis,
  Positive,                     % false if subscribe expected NOT to succeed
  Config) ->
    % configure the MO
    {{YY, Mon, DD}, {_, _, _}} = calendar:universal_time(),
    TodayUtc =
	lists:flatten(io_lib:format("~w-~2..0w-~2..0w", [YY, Mon, DD])),
    LeapSeconds = 17,
    updateTimeSettings(
      [{timeOffset, TimeOffset},
       {gpsToUtcLeapSeconds, LeapSeconds},
       {gpsToUtcLeapSecondsChangeDate, IncrementPrefix++TodayUtc}]),

    % jump into the future: 7 s before UTC midnight
    {{YY2, Mon2, DD2}, {HH, MM, SS}} = calendar:universal_time(),
    SimFuture = (24*3600 - (HH*3600 + MM*60 + SS) - 7)*1000,
    if
	SimFuture < 0 orelse
	    YY2 =/= YY orelse
	    Mon2 =/= Mon orelse
	    DD2 =/= DD ->
	    % freak case: the test was started just before midnight
	    resetTimeSettings(),
	    ok;
	true ->
	    ok = simFuture(SimFuture),
	    ?SAY("simulating ~w ms into the future", [SimFuture]),

	    % start signal handler and specify cleanup
	    Increment = case IncrementPrefix of "-" -> -1; "" -> 1 end,
	    sigHandler:start(?MODULE, {
		   self(),
		   25000,
		   fun(_) -> simFuture(0), resetTimeSettings() end,
		   [fun() -> sc_leap(LeapSeconds, Increment, PrewarnMillis, Positive, Config) end]}),

	    shWait()
    end.


sc_leap(LeapSeconds, Increment, PrewarnMillis, Positive, Config) ->
    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, tzii1, SigHandler, Config),

    ClientInfo = 1111,
    case subscribe(?IFT_NODE, tzii1, SigHandler, leapSec, ClientInfo, PrewarnMillis, Config) of
	rejected when not(Positive) ->
	    % expected, ok

	    ?SAY("subscribe for leap was rejected, as expected", []),
	    ok = svcTerm(?IFT_NODE, tzii1, SigHandler, Config),
	    sigHandler:cast(SigHandler, {done, self()});

	rejected when Positive ->
	    % rejected, not expected
	    ok = svcTerm(?IFT_NODE, tzii1, SigHandler, Config),
	    sigHandler:cast(SigHandler, {done, self(), {fail, subscribe_leap_rejected_unexpectedly}});

	ok when not(Positive) ->
	    % we were allowed to subscribe even though we should not have
	    ok = svcTerm(?IFT_NODE, tzii1, SigHandler, Config),
	    sigHandler:cast(SigHandler, {done, self(), {fail, subscribe_leap_allowed_unexpectedly}});

	ok when Positive ->
	    {ok, [#signal{fields=Fields1}]} =
		shGet(SigHandler, ?IFT_NODE, tzii1, ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND, 20000, Config),
	    ?SAY("got pre-w@rning leap IND, fields: ~p", [Fields1]),
	    LeapSecondsStepped = LeapSeconds+Increment,
	    [ClientInfo, LeapSecondsStepped, ?CELLO_TZII_PREWARNING_START] = Fields1,

%% 	    {ok, [#signal{fields=Fields2}]} =
%% 		shPoll(SigHandler, ?IFT_NODE, tzii1, ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND, 2000, 20000),
%% 	    ?SAY("got MO auto-update leap IND, fields: ~p", [Fields2]),
%% 	    [ClientInfo, LeapSecondsStepped, ?CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE] = Fields2,

	    ok = svcTerm(?IFT_NODE, tzii1, SigHandler, Config),
	    sigHandler:cast(SigHandler, {done, self()})
    end.


%%% ----------------------------------------------------------
%%% @doc Test INDs sent to leap subscribers that have started
%%% their subscriptions at different points in time.
%%% @end
%%% ----------------------------------------------------------

-define(BLINK, 5000).

test_leap_reconfig(Config) ->
    % start with a non-existent configuration
    resetTimeSettings(),
    updateTimeSettings([{gpsToUtcLeapSeconds, 0}]),

    {{Yr1, Mo1, Dy1}=TodayUtc, {H1, M1, S1}} = calendar:universal_time(),    % get today

    TodayUtcG = calendar:date_to_gregorian_days(TodayUtc),
    {Yr2, Mo2, Dy2}=_NextYearUtc = calendar:gregorian_days_to_date(TodayUtcG+365),

    simFuture((24*3600 - (H1*3600 + M1*60 + S1))*1000 - 6*?BLINK),

    FirstLeapSecondsValue = 21,

    % start a number of scenarios
    sigHandler:start(?MODULE, {self(),
	   70000,
	   fun(_) -> simFuture(0), resetTimeSettings() end,
	   [fun() -> sc_lr_operator(Yr1, Mo1, Dy1, Yr2, Mo2, Dy2, FirstLeapSecondsValue) end,
	    fun() -> sc_lr1(tzii1, 1*?BLINK, FirstLeapSecondsValue, Config) end,
	    fun() -> sc_lr2(tzii2, 1*?BLINK, FirstLeapSecondsValue, Config) end,
	    fun() -> sc_lr3(tzii3, 4*?BLINK, FirstLeapSecondsValue, Config) end,
	    fun() -> sc_lr4(tzii4, 2*?BLINK, FirstLeapSecondsValue, Config) end,
	    fun() -> sc_lr5(tzii5, 3*?BLINK, FirstLeapSecondsValue, Config) end
	    ]}),

    shWait().


sc_lr_operator(Yr1, Mo1, Dy1, Yr2, Mo2, Dy2, FirstLeapSecondsValue) ->
    StartTs = ?MILLIS(),
    ComCliSlowness = 2700,
    receive {pid, SigHandler} -> ok end,


    % configure a first leapSeconds value
    waitUntil(StartTs, 2*?BLINK - ComCliSlowness),
    updateTimeSettings(
      [{timeOffset, "+03:00"},
       {gpsToUtcLeapSeconds, FirstLeapSecondsValue}]),

    % specify a leap date
    waitUntil(StartTs, 4*?BLINK - ComCliSlowness),
    updateTimeSettings(
      [{gpsToUtcLeapSecondsChangeDate, leapSecondsChangeDate(1, Yr1, Mo1, Dy1)}]),

    % remove the leap date, increment the leap seconds value
    waitUntil(StartTs, 8*?BLINK - ComCliSlowness),
    removeLeapSecondsChangeDate(FirstLeapSecondsValue+1),

    % add a leap date again
    waitUntil(StartTs, 10*?BLINK - ComCliSlowness),
    updateTimeSettings(
      [{gpsToUtcLeapSecondsChangeDate, leapSecondsChangeDate(1, Yr2, Mo2, Dy2)}]),

    sigHandler:cast(SigHandler, {done, self()}).


waitUntil(StartTs, Millis) ->
    Now = ?MILLIS(),
    NowDiff = Now - StartTs,
    if
	NowDiff >= Millis ->
	    ok;
	true ->
	    timer:sleep(117),
	    waitUntil(StartTs, Millis)
    end.

-define(UPDATE, ?CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE).
-define(PREWARN, ?CELLO_TZII_PREWARNING_START).
-define(CANCEL, ?CELLO_TZII_PREWARNING_CANCEL).

sc_lr1(Child, Prewarn, _FirstLeapSecondsValue, Config) ->
    StartTs = ?MILLIS(),
    ClientInfo = clientInfo(Child),
    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    waitUntil(StartTs, 1*?BLINK),
    ok = subscribe(?IFT_NODE, Child, SigHandler, leapSec, ClientInfo, Prewarn, Config),

    timer:sleep(10*?BLINK),
    {ok, TheLot} = shGet(SigHandler, ?IFT_NODE, Child, undefined, 30000, Config),
    ?SAY("the lot: ~p", [showInds(TheLot)]),

    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
    sigHandler:cast(SigHandler, {done, self()}).


sc_lr2(Child, Prewarn, _FirstLeapSecondsValue, Config) ->
    StartTs = ?MILLIS(),
    ClientInfo = clientInfo(Child),
    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    waitUntil(StartTs, 3*?BLINK),
    ok = subscribe(?IFT_NODE, Child, SigHandler, leapSec, ClientInfo, Prewarn, Config),

    timer:sleep(8*?BLINK + 1000),
    {ok, TheLot} = shGet(SigHandler, ?IFT_NODE, Child, undefined, 30000, Config),
    ?SAY("the lot: ~p", [showInds(TheLot)]),

    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
    sigHandler:cast(SigHandler, {done, self()}).

sc_lr3(Child, Prewarn, _FirstLeapSecondsValue, Config) ->
    StartTs = ?MILLIS(),
    ClientInfo = clientInfo(Child),
    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    waitUntil(StartTs, 3*?BLINK),
    ok = subscribe(?IFT_NODE, Child, SigHandler, leapSec, ClientInfo, Prewarn, Config),

    timer:sleep(8*?BLINK + 2000),
    {ok, TheLot} = shGet(SigHandler, ?IFT_NODE, Child, undefined, 30000, Config),
    ?SAY("the lot: ~p", [showInds(TheLot)]),

    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
    sigHandler:cast(SigHandler, {done, self()}).

sc_lr4(Child, Prewarn, _FirstLeapSecondsValue, Config) ->
    StartTs = ?MILLIS(),
    ClientInfo = clientInfo(Child),
    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    waitUntil(StartTs, 5*?BLINK),
    ok = subscribe(?IFT_NODE, Child, SigHandler, leapSec, ClientInfo, Prewarn, Config),

    timer:sleep(6*?BLINK + 3000),
    {ok, TheLot} = shGet(SigHandler, ?IFT_NODE, Child, undefined, 30000, Config),
    ?SAY("the lot: ~p", [showInds(TheLot)]),

    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
    sigHandler:cast(SigHandler, {done, self()}).

sc_lr5(Child, Prewarn, _FirstLeapSecondsValue, Config) ->
    StartTs = ?MILLIS(),
    ClientInfo = clientInfo(Child),
    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    waitUntil(StartTs, 7*?BLINK),
    ok = subscribe(?IFT_NODE, Child, SigHandler, leapSec, ClientInfo, Prewarn, Config),

    timer:sleep(4*?BLINK + 4000),
    timeout = shGet(SigHandler, ?IFT_NODE, Child, undefined, 4*?BLINK, Config),

    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
    sigHandler:cast(SigHandler, {done, self()}).





-spec showInds([#signal{}]) ->  [string()].

showInds(Signals) ->
    ArrivedMax = lists:foldl(
		   fun(#signal{arrived=A}, Acc) ->
			   max(A, Acc)
		   end,
		   0,
		   Signals),

    Nmarkers = 1 + ArrivedMax div 2000,
    Ints = lists:seq(0, Nmarkers),
    Markers = lists:map(fun(M) -> #signal{no=0, arrived=2000*M, fields=[]} end, Ints),

    lists:map(
      fun(#signal{no=No, arrived=Arrived, fields=Fields}) ->
	      CauseS =
		  case Fields of
		      [_, _, Cause] ->
			  causeName(Cause);
		      _ ->
			  ""
		  end,
	 lists:flatten(
	   io_lib:format(
	     "~6w ~s ~p ~s",
	     [Arrived, signalNameX(No), Fields, CauseS]))
      end,
      ordsets:to_list(ordsets:union(ordsets:from_list(Signals), ordsets:from_list(Markers)))).

causeName(?UPDATE) ->
    "MO update or timestep";

causeName(?PREWARN) ->
    "pre-w@rning";           % the word 'warning' would upset CI

causeName(?CANCEL) ->
    "pre-w@rning cancel".    % the word 'warning' would upset CI


signalNameX(0) ->
    "-----------------------------------------------";

signalNameX(No) ->
    signalName(No).


%% expectInd(SigHandler, Child, ClientInfo, LeapSecondsValue, Cause) ->
%%     case shPoll(SigHandler, ?IFT_NODE, Child, ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND, 1000, 30000) of
%% 	{ok, [#signal{fields=[ClientInfo, LeapSecondsValue, Cause]}]} ->
%% 	    ok;
%% 	Other ->
%% 	    Other
%%     end.
%%
%% expectTwoInds(SigHandler, Child, ClientInfo,
%% 	      [LSV1, LSV2], [Cause1, Cause2]) ->
%%     case shPoll(SigHandler, ?IFT_NODE, Child, ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND, 1000, 30000) of
%% 	{ok, [#signal{fields=[ClientInfo, LSV1, Cause1]},
%% 	      #signal{fields=[ClientInfo, LSV2, Cause2]}]} ->
%% 	    ok;
%% 	Other ->
%% 	    Other
%%     end.


%% showSignals(SigHandler, Child, Timeout) ->
%%     case shPoll(SigHandler, ?IFT_NODE, Child, undefined, 1500, Timeout) of
%% 	timeout ->
%% 	    ?SAY("probably no more signals for: ~p", [Child]),
%% 	    ok;
%% 	{ok, _Signals} ->
%% 	    %?SAY("more signals for: ~p", [Child]),
%% 	    showSignals(SigHandler, Child, Timeout)
%%     end.


%% silence(SigHandler, Child, Timeout) ->
%%     case shPoll(SigHandler, ?IFT_NODE, Child, undefined, 1000, Timeout) of
%% 	timeout ->
%% 	    ok;
%% 	{ok, _Signals}=Result ->
%% 	    Result
%%     end.
















%%% ----------------------------------------------------------
%%% @doc One actor that updates the TimeSettings MO; two subscribers
%%% receiving INDs because of this, since they find themselves in the
%%% prewarning intervals for DST starting and Leap Seconds stepping.
%%% @end
%%% ----------------------------------------------------------
test_timesettings_update(Config) ->

    % typically zero; set a nonzero value to run the test against
    % a node that runs with a clock set back to some previous date
    % HistorySecs = 2*24*3600 - 19,
    HistorySecs = 0,

    TimeOffsetSecs = 3*3600,
    LeapSeconds = 17,
    UtcSec = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - HistorySecs,
    DstAheadSecs = 10*3600,
    NewDstAheadSecs = 5*3600,
    DstOffsetSecs = 2*3600,

    updateTimeSettings(
      UtcSec,
      TimeOffsetSecs,
      DstAheadSecs,
      DstOffsetSecs,
      LeapSeconds,
      1,
      1),

    sigHandler:start(?MODULE, {self(),
	   30000,
	   fun(_) -> resetTimeSettings() end,
	   [
	    fun() -> sc_timesettings_update(UtcSec, TimeOffsetSecs, NewDstAheadSecs, DstOffsetSecs, 5000) end,
	    fun() -> sc_timesettings_update_a(UtcSec, TimeOffsetSecs, NewDstAheadSecs, DstOffsetSecs, Config) end,
	    fun() -> sc_timesettings_update_b(LeapSeconds, Config) end
	   ]}),

    shWait().


%%% ----------------------------------------------------------
%%% @doc Updates time settings: The DST start time is set in
%%% local (non-DST) time at some point ahead in time. The DST
%%% end time is set 100 days later.
%%% @end
%%% ----------------------------------------------------------
updateTimeSettings(
  UtcS,                    % current UTC given as gregorian second
  TimeOffsetSecs,          % local time offset, seconds
  DstAheadSecs,            % set the DST change this many seconds from now
  DstOffsetSecs,           % DST offset, seconds
  LeapSeconds,             % current leap seconds value
  LeapAheadDays,           % 0 for current UTC day, 1 for next etc
  LeapSign                 % 1 or -1
		  ) ->

    TimeOffsetS =
	lists:flatten(
	  io_lib:format("~s~2..0w:~2..0w",
			[if TimeOffsetSecs < 0 -> "-"; true -> "+" end,
			 abs(TimeOffsetSecs) div 3600,
			 (abs(TimeOffsetSecs) rem 3600) div 60])),
    DstOffsetS =
	lists:flatten(
	  io_lib:format("~1..0w:~2..0w",
			[
			 DstOffsetSecs div 3600,
			 (DstOffsetSecs rem 3600) div 60])),

    {{_A, B, C}, {D, E, _F}} =
	calendar:gregorian_seconds_to_datetime(UtcS + TimeOffsetSecs + DstAheadSecs),

    {{_G, H, J}, {K, L, _M}} =
	calendar:gregorian_seconds_to_datetime(UtcS + TimeOffsetSecs + DstAheadSecs + 100 * 24 * 3600),

    {{P, Q, R}, _} =
	calendar:gregorian_seconds_to_datetime(UtcS + LeapAheadDays*24*3600),

    LeapDateS = leapSecondsChangeDate(LeapSign, P, Q, R),

    updateTimeSettings(
      [{timeOffset, TimeOffsetS},
       {daylightSavingTimeOffset, DstOffsetS},

       {daylightSavingTimeStartDate,
	#dstChangeTime{month=monthName(B),
		       day=lists:flatten(io_lib:format("~w", [C])),
		       time=lists:flatten(io_lib:format("~2..0w:~2..0w", [D, E]))}},

       {daylightSavingTimeEndDate,
	#dstChangeTime{month=monthName(H),
		       day=lists:flatten(io_lib:format("~w", [J])),
		       time=lists:flatten(io_lib:format("~2..0w:~2..0w", [K, L]))}},

       {gpsToUtcLeapSecondsChangeDate, LeapDateS},
       {gpsToUtcLeapSeconds, LeapSeconds}
      ]).

leapSecondsChangeDate(S, P, Q, R) ->
    lists:flatten(
      io_lib:format("~s~4..0w-~2..0w-~2..0w",
		    [if S =:= -1 -> "-"; true -> "" end,
		     P, Q, R])).



%%% ----------------------------------------------------------
%%% @doc This scenario sleeps for a while and performs an MO
%%% reconfiguration.
%%% @end
%%% ----------------------------------------------------------

sc_timesettings_update(
  UtcSec, TimeOffsetSecs, NewDstAheadSecs, DstOffsetSecs,
  Delay) ->
    receive {pid, SigHandler} -> ok end,
    timer:sleep(Delay),

    % reconfigure DST attributes
    updateTimeSettings(UtcSec, TimeOffsetSecs, NewDstAheadSecs, DstOffsetSecs, 17, 0, 1),
    sc_timesettings_update_b ! updateDone,

    sigHandler:cast(SigHandler, {done, self()}).


%%% ----------------------------------------------------------
%%% @doc First actor, creates a session and subscribes for
%%% just DST indications. Uses tzii1.
%%% @end
%%% ----------------------------------------------------------
sc_timesettings_update_a(UtcSec, TimeOffsetSecs, DstAheadSecs, DstOffsetSecs, Config) ->
    Child = tzii1,
    receive {pid, SigHandler} -> ok end,
    try
	ClientInfo = power(2, 32) - 1,             % edge value, maximum uint32_t
	TimeOffsetM = TimeOffsetSecs div 60,
	DstOffsetM = DstOffsetSecs div 60,


	ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

	ok = subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo, 7*3600*1000, Config),

	% do NOT expect an "update" IND because neither DST status
	% nor dstOffset was changed in the MO update
	{ok, [#signal{fields=Fields2}]} =
	    shGet(SigHandler, ?IFT_NODE, Child, ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND, 20000, Config),

	?SAY("DST IND received, fields: ~p", [Fields2]),

	{_, {D, E, _}} =
	    calendar:gregorian_seconds_to_datetime(
	      UtcSec + TimeOffsetSecs + DstAheadSecs),
	TimeOfChangeMinutes = D*60 + E,

	ExpectedPayload =
	    [ClientInfo,
	     TimeOffsetM,
	     DstOffsetM,
	     1,
	     TimeOfChangeMinutes,
	     ?CELLO_TZII_PREWARNING_START],

	?SAY("ought to match: ~p", [ExpectedPayload]),
	ExpectedPayload=Fields2,

	ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),

	sigHandler:cast(SigHandler, {done, self()})
    catch
	X:Y ->
	    ?SAY("caught: ~p~n~p", [{X, Y}, erlang:get_stacktrace()]),
	    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {Child, X, Y}}})
    end.


%%% ----------------------------------------------------------
%%% @doc Second actor, creates a session and subscribes for
%%% just LEAP indications. Uses tzii2.
%%% @end
%%% ----------------------------------------------------------
sc_timesettings_update_b(LeapSeconds, Config) ->
    % register so as to be able to receive a sync signal
    register(sc_timesettings_update_b, self()),

    Child = tzii2,
    LeapSecondsNext = LeapSeconds + 1,
    receive {pid, SigHandler} -> ok end,



    try
	ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

	ClientInfo = 2222,
	ok = subscribe(?IFT_NODE, Child, SigHandler, leapSec, ClientInfo, 23*3600*1000, Config),

	% wait for the TS update
	receive
	    updateDone ->
		ok
	end,
	% if the UTC clock says 00:mm:ss then we cannot get
	% a prewarn since the maximum prewarn time (which we
	% have also requested) is 23 hours
	case calendar:universal_time() of
	    {_, {0, _, _}} ->
		% expect no prewarn
		?SAY("no prewarn expected", []),
		timeout = shGet(SigHandler, ?IFT_NODE, Child, undefined, 5000, Config);
	    {_, {H, M, S}} when H*3600 + M*60 + S > 3610 ->
		% definitely expect a prewarn
		{ok, List} = shGet(SigHandler, ?IFT_NODE, Child, undefined, 20000, Config),
		?SAY("expecting prewarn, got signals: ~p", [List]),
		[#signal{fields=[ClientInfo,
				 LeapSecondsNext,
				 ?CELLO_TZII_PREWARNING_START]}] =
		    List;
	    _ ->
		?SAY("edge case, don't care about the LEAP IND", [])
	end,

	ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
	sigHandler:cast(SigHandler, {done, self()})
    catch
	X:Y ->
	    ?SAY("caught: ~p~n~p", [{X, Y}, erlang:get_stacktrace()]),
	    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {Child, X, Y}}})
    end.


%%% ----------------------------------------------------------
%%% @doc Test a scenario where a timestep forward occurs.
%%% @end
%%% ----------------------------------------------------------
test_timestep_forward(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset; not important here
    DstOffsetS = "1:00",

    % specify how many minutes ahead the DST change should happen
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST start
    {_, {_, _, SecsAtTcStart}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    % calculate the local datetime of the DST end, some 150 days
    % ahead (not significant)
    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
	case
	    calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
	    {{_, 2, 29}, _} ->
		?SAY("avoiding leap day", []),
		calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
	    Other ->
		Other
	end,


    % update the TimeSettings MO
    % TODO, fix leap day issue
    updateTimeSettings(
      [{timeOffset, TimeOffsetS},
       {daylightSavingTimeOffset, DstOffsetS},
       {daylightSavingTimeStartDate,
	makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
       {daylightSavingTimeEndDate,
	makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
      ]),

    % calculate a suitable timestep that causes the client
    % to find itself within the prewarn time interval
    Delay = 5,
    Prewarn = 50,
    TimestepMillis =
	(TimeToDstChangeM*60 - SecsAtTcStart - Delay - (Prewarn div 2))*1000,

    % start scenarios
    DstOffsetMinutes = parseDstOffset(DstOffsetS),
    DstStatusExpected = 1,

    sigHandler:start(?MODULE, {
	   self(),
	   30000,
	   fun(_) -> simFuture(0), resetTimeSettings() end,
	   [
	    fun() -> sc_timestep_invoke(Delay * 1000, TimestepMillis) end,
	    fun() -> sc_timestep_forward(
		       tzii1,
		       Prewarn * 1000,
		       TimeOffsetMinutes,
		       DstOffsetMinutes,
		       DstStatusExpected, HourL, MinuteL, Config) end
	   ]}),

    shWait().


sc_timestep_invoke(DelayMillis, StepMillis) ->
    receive {pid, SigHandler} -> ok end,

    ?SAY("invoke step in ~w ms, step size: ~w ms", [DelayMillis, StepMillis]),

    timer:sleep(DelayMillis),
    simTimeStep(StepMillis),
    sigHandler:cast(SigHandler, {done, self()}).


sc_timestep_forward(Child, PrewarnMillis, TimeOffsetToUtc, DstOffsetMinutes, DstStatusExpected,
		    HourL, MinuteL,
		    Config) ->
    receive {pid, SigHandler} -> ok end,
    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    ClientInfo = clientInfo(Child),
    %%     PreWarnMillis = 5000,
    case subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo, PrewarnMillis, Config) of
	rejected ->
	    % should definitely not happen
	    %ct:fail("subscription rejected for PreWarn: ~w", [PrewarnMillis]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {prewarn_too_large, PrewarnMillis}}});
	ok ->
	    % expect the prewarn signal after a while
	    % because of the time step
	    {ok, [#signal{fields=Fields}]} =
		shGet(
		  SigHandler,
		  ?IFT_NODE,
		  Child,
		  ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND,
		  180000,
		  Config),
	    ?SAY("have DST IND, fields: ~p", [Fields]),
	    CauseForChange = ?CELLO_TZII_PREWARNING_START,

	    ExpectedFields =
		[ClientInfo,
		 TimeOffsetToUtc,
		 DstOffsetMinutes,
		 DstStatusExpected,
		 HourL*60 + MinuteL,
		 CauseForChange],

	    ?SAY("expected fields: ~p", [ExpectedFields]),

	    ExpectedFields = Fields,

	    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),

	    sigHandler:cast(SigHandler, {done, self()})
    end.


test_timestep_backward(Config) ->

    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset; not important here
    DstOffsetS = "1:00",

    % specify how many minutes ahead the DST change should happen
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST start
    {_, {_, _, Alpha}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    % calculate the local datetime of the DST end, some 150 days
    % ahead (not significant)
    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
	case
	    calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
	    {{_, 2, 29}, _} ->
		?SAY("avoiding leap day", []),
		calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
	    Other ->
		Other
	end,

    % update the TimeSettings MO
    updateTimeSettings(
      [{timeOffset, TimeOffsetS},
       {daylightSavingTimeOffset, DstOffsetS},
       {daylightSavingTimeStartDate,
	makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
       {daylightSavingTimeEndDate,
	makeDstChangeTime(MonthR, DayR, HourR, MinuteR)}
      ]),

    UtcS2 = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Beta = UtcS2 - UtcS,

    Gamma = 10,
    Delta = 5,

    Prewarn = TimeToDstChangeM*60 - (Alpha + Beta + Gamma),

    % start scenarios
    DstOffsetMinutes = parseDstOffset(DstOffsetS),
    DstStatusExpected = 1,
    TimeOfChange = HourL*60 + MinuteL,
    DstStatusExp2 = 0,

    sigHandler:start(?MODULE,
      {self(),
       30000,
       fun(_) -> simFuture(0), resetTimeSettings() end,
       [
	fun() -> sc_timestep_invoke((Gamma + Delta) * 1000, -2 * Delta * 1000) end,
	fun() -> sc_timestep_backward(
		   tzii1,
		   Prewarn * 1000,
		   TimeOffsetMinutes,
		   DstOffsetMinutes,
		   DstStatusExpected,
		   TimeOfChange,
		   DstStatusExp2,
		   Config) end
       ]}),

    shWait().


sc_timestep_backward(
  Child,
  PrewarnMillis,
  TimeOffsetMinutes,
  DstOffsetMinutes,
  DstStatusExpected, TimeOfChange, DstStatusExp2,
  Config) ->

    receive {pid, SigHandler} -> ok end,

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    ClientInfo = clientInfo(Child),

    case subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo, PrewarnMillis, Config) of
	rejected ->
	    % should definitely not happen
	    %ct:fail("subscription rejected for PreWarn: ~w", [PrewarnMillis]);

	    sigHandler:cast(SigHandler, {done, self(), {fail, {prewarn_too_large, PrewarnMillis}}});
	ok ->
	    % expect the prewarn signal after a while
	    {ok, [#signal{fields=Fields}]} =
		shGet(
		  SigHandler,
		  ?IFT_NODE,
		  Child,
		  ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND,
		  60000,
		  Config),
	    ?SAY("have DST IND, fields: ~p", [Fields]),
	    % [11711,-210,60,1,621,0]

	    Cause1 = ?CELLO_TZII_PREWARNING_START,
	    [ClientInfo,
	     TimeOffsetMinutes,
	     DstOffsetMinutes,
	     DstStatusExpected,
	     TimeOfChange,
	     Cause1] = Fields,

	    % expect a cancel signal because of the timestep
	    {ok, [#signal{fields=Fields2}]} =
		shGet(
		  SigHandler,
		  ?IFT_NODE,
		  Child,
		  ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND,
		  60000,
		  Config),
	    ?SAY("have a second DST IND, fields: ~p", [Fields2]),
	    %[11711,-210,60,0,0,1]

	    Cause2 = ?CELLO_TZII_PREWARNING_CANCEL,
	    [ClientInfo,
	     TimeOffsetMinutes,
	     DstOffsetMinutes,
	     DstStatusExp2,
	     _,
	     Cause2] = Fields2,

	    % expect yet another signal because prewarn happens again
	    {ok, [#signal{fields=Fields3}]} =
		shGet(
		  SigHandler,
		  ?IFT_NODE,
		  Child,

		  ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND,
		  60000,
		  Config),
	    ?SAY("have a third DST IND, fields: ~p", [Fields3]),

	    Cause3 = Cause1,
	    [ClientInfo,
	     TimeOffsetMinutes,
	     DstOffsetMinutes,
	     DstStatusExpected,
	     TimeOfChange,
	     Cause3] = Fields3,

	    ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),

	    sigHandler:cast(SigHandler, {done, self()})
    end.


%%% ----------------------------------------------------------
%%% @doc Applies the given function to the given arguments
%%% in the signalhandler process. The result is wrapped as
%%% {ok, Result}.
%%% @end
%%% ----------------------------------------------------------
-spec shApply(pid(), function(), [any()]) ->
                 {ok, any()}  | {error, any()}.

shApply(SigHandler, Fun, Args) ->
    sigHandler:call(SigHandler, {apply, Fun, Args}).


shWait() ->
    receive
	{stop, {fail, Reason}} ->
	    ct:fail(Reason);
	{stop, ok} ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Get the specified signal(s).
%%% @end
%%% ----------------------------------------------------------
-spec shGet(pid(),
	     atom(),
	     atom(),
	     integer()|[integer()]|undefined,
	     timeout(),
	    config()) ->
	  {ok, [#signal{}]} | timeout.

shGet(SigHandler, _Node, Child, SigSpec, Timeout, Config) ->
    ChildPid = orddict:fetch(Child, ?config(?PID_BY_CHILD, Config)),
    sigHandler:getSignals(SigHandler, {ChildPid, SigSpec}, Timeout).


%%% ----------------------------------------------------------
%%% @doc Generate a number that reflects the child name.
%%% @end
%%% ----------------------------------------------------------
clientInfo(Child) ->
    	ChildNameChar5 = string:substr(atom_to_list(Child), 5, 1),
	77700 + list_to_integer(ChildNameChar5).


%%% ----------------------------------------------------------
%%% doc Launch the dynamic test program.
%%% Work in progress. Not clear how to write a stand-alone
%%% C program using TZII.
%%% end
%%% ----------------------------------------------------------

%% test_proxy1(_Config) ->
%%     rct_proxy:send_proxy(?IFT_NODE, tzii1, ?iftRequest_startProxy1, {}, 4000),
%%     timer:sleep(4000),
%%     rct_proxy:send_proxy(?IFT_NODE, tzii1, ?iftRequest_stopExample, {}, 4000),
%%     ok.

test_get_versions(_Config) ->
    TimServerVersion =
	rct_rpc:call(rpc1, timServer, testControl, [getVersion, []], 4000),
    ?SAY("timServer version: ~p", [TimServerVersion]),

    case rct_proxy:send_proxy(?IFT_NODE, tzii1, ?iftRequest_version) of
	{ok, String} when is_list(String) ->
	    ?SAY("test_tzii version is: ~s", [String]),
	    ok;
	Other ->
	    ct:fail("could not fetch test_tzii version: ~p", [Other])
    end.


%%% ----------------------------------------------------------
%%% @doc Verify that an attempt to free the proxy memory fails
%%% with code CELLO_TZII_ACTIVE_SERVICE if the proxy state is
%%% not UNAVAILABLE.
%%%
%%% /IWD/ freeMemory
%%% /IWD/ initiateService
%%% /IWD/ internal
%%% /IWD/ terminateService
%%% @end
%%% ----------------------------------------------------------
test_neg_if_mgmt(Config) ->
    Child = tzii1,
    Postlude = fun(_) -> ok end,
    Scenarios = [fun() -> sc_neg_free_mem_1(Child, Config) end],
    sigHandler:start(?MODULE, {self(), 10000, Postlude, Scenarios}),
    shWait(),
    ok.

sc_neg_free_mem_1(Child, Config) ->
    receive {pid, SigHandler} -> ok end,

    % try to call termService when proxy state is not AVAILABLE or SUSPENDED
    {ok, {ok, ?CELLO_TZII_SERVER_UNAVAILABLE}} =
	shApply(SigHandler, fun termService/2, [?IFT_NODE, Child]),

    ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

    % try to make a freeMemory call when the proxy state is not UNAVAILABLE
    {ok, ?CELLO_TZII_ACTIVE_SERVICE} =
	rct_proxy:send_proxy(?IFT_NODE, Child, ?iftRequest_freeMemory),

    % try to make an initiateService call when the proxy state is not UNAVAILABLE
    ProtocolVersions = {333, 444, 555},
    {ok, ?CELLO_TZII_ALREADY_INITIATED} =
	rct_proxy:send_proxy(?IFT_NODE, Child, ?iftRequest_initiateService, ProtocolVersions),

    % do not expect any signals at this point
    timeout = shGet(SigHandler, ?IFT_NODE, Child, undefined, 5000, Config),

    % terminate the service
    {ok, ok} = shApply(SigHandler, fun termService/2, [?IFT_NODE, Child]),
    {ok, [#signal{}]} = shGet(SigHandler, ?IFT_NODE, Child, ?CELLO_TZII_TERMINATE_SERVICE_CFM, 2000, Config),

    % test a bad internal call
    {ok, {ok, ?CELLO_TZII_ILLEGAL_SIGNAL}} =
	shApply(SigHandler, fun internalBad/2, [?IFT_NODE, Child]),

    % now do the internal call properly
    {ok, ok} = shApply(SigHandler, fun internal/2, [?IFT_NODE, Child]),
    ?SAY("terminated service: ~p", [{?IFT_NODE, Child}]),

    sigHandler:cast(SigHandler, {done, self()}).


test_tzii_example(_Config) ->
    % specify a timezone offset; the TC should succeed for any setting
    TimeOffsetS = "-03:30",

    % specify a DST offset
    DstOffsetS = "2:22",

    % specify how many minutes ahead the DST change should happen
    % (miminmum 2, maximum is limited by TC timeouts, however if
    % simulated future is applied then timeouts are defeated)
    TimeToDstChangeM = 2,

    % calculate the local datetime of the DST start
    {_, {_, _, _Sec}} = UtcDt = calendar:universal_time(),
    UtcS = calendar:datetime_to_gregorian_seconds(UtcDt),
    TimeOffsetMinutes = parseTimeOffset(TimeOffsetS),
    LocalS = UtcS + (TimeOffsetMinutes + TimeToDstChangeM)*60,
    {{_, MonthL, DayL}, {HourL, MinuteL, _}} =
	calendar:gregorian_seconds_to_datetime(LocalS),

    case MonthL =:= 2 andalso DayL =:= 29 of
	true ->
	    ?SAY("today is a leap day, just let the TC pass", []),
	    ok;
	_ ->

	    % calculate the local datetime of the DST end, some 150 days
	    % ahead (not significant)
	    {{_, MonthR, DayR}, {HourR, MinuteR, _}} =
		case calendar:gregorian_seconds_to_datetime(LocalS + 150*24*3600) of
		    {{_, 2, 29}, _} ->
			?SAY("avoid ending DST on a leap day", []),
			calendar:gregorian_seconds_to_datetime(LocalS + 151*24*3600);
		    Other ->
			Other
		end,

	    % update the TimeSettings MO
	    updateTimeSettings(
	      [{timeOffset, TimeOffsetS},
	       {daylightSavingTimeOffset, DstOffsetS},
	       {daylightSavingTimeStartDate,
		makeDstChangeTime(MonthL, DayL, HourL, MinuteL)},
	       {daylightSavingTimeEndDate,
		makeDstChangeTime(MonthR, DayR, HourR, MinuteR)},
	       {gpsToUtcLeapSeconds, 33}
	      ]),

	    Child = tzii1,
	    A = rct_proxy:send_proxy(?IFT_NODE, Child, ?iftRequest_startExample),
	    ?SAY("tried to start: ~p", [A]),

	    % wait long enough that the DST change occurs
	    timer:sleep(121000),

	    B = rct_proxy:send_proxy(?IFT_NODE, Child, ?iftRequest_stopExample),
	    ?SAY("tried to stop: ~p", [B])
    end.



%%% ----------------------------------------------------------
%%% @doc Test intended for a scratch-installed system where the
%%% TimeSettings MO has not been configured at all. The
%%% scenario is to successfully subscribe to DST and Leap.
%%%
%%% TODO: Maybe ... add actions to configure the MO when the
%%% subscriptions are established.
%%% @end
%%% ----------------------------------------------------------
test_tzii_unconfigured(Config) ->

    sigHandler:start(?MODULE,
		     {self(),
		      30000,
		      fun(_) -> ok end,
		      [% a client that subscribes to DST and Leap
		       fun() -> sc_dst_leap(tzii1, Config) end
		      ]}),
    shWait().

%%% ----------------------------------------------------------
%%% @doc Scenario:
%%%        init service
%%%        subscribe DST
%%%        subscribe Leap
%%%        terminate service
%%% @end
%%% ----------------------------------------------------------
sc_dst_leap(Child, Config) ->

    receive {pid, SigHandler} -> ok end,
    ?SAY("~w: running scenario: ~p", [Child, self()]),

    try
	ok = svcInit(?IFT_NODE, Child, SigHandler, Config),

	ClientInfo = clientInfo(Child),
	PrewarnMillis = 2000,
	ok = subscribe(?IFT_NODE, Child, SigHandler, dst, ClientInfo, PrewarnMillis, Config),

	case subscribe(?IFT_NODE, tzii1, SigHandler, leapSec, ClientInfo, PrewarnMillis, Config) of
	    ok ->
		ok;
	    Other ->
		?SAY("~w: subscribe Leap failed: ~p", [Child, Other])
	end,

	timeout = shGet(SigHandler, ?IFT_NODE, Child, undefined, 5000, Config),

	ok = svcTerm(?IFT_NODE, Child, SigHandler, Config),
	sigHandler:cast(SigHandler, {done, self()})
    catch
	X:Y ->
	    ?SAY("~p, ~p~n~p", [Child, {X, Y}, erlang:get_stacktrace()]),
	    sigHandler:cast(SigHandler, {done, self(), {fail, {X, Y}}})
    end.




-record(state, {timeout=99999999         :: timeout(),
		postlude=none            :: function()|none,
		signals=ordsets:new()    :: ordsets:ordset(#signal{}),
		master                   :: pid(),
		scenarios=ordsets:new()  :: ordsets:ordset(pid()),
		status=ok                :: ok|{fail, any()},
		requests=orddict:new()   :: orddict:orddict()   % pid() -> {pattern, deadline}  TODO
	       }).


-spec init(tuple()) -> {ok, #state{}}|{ok, #state{}, 0}.

init({Master, Timeout, Postlude, Scenarios}) ->
    SigHandler = self(),
    Pids =
	lists:foldl(
	  fun(ScenarioFun, Acc) ->
		  Pid = spawn(ScenarioFun),
		  Pid ! {pid, SigHandler},
		  ordsets:add_element(Pid, Acc)
	  end,
	  ordsets:new(),
	  Scenarios),
    {ok, #state{postlude=Postlude, master=Master, scenarios=Pids}, Timeout}.


handle_call({apply, Fun, Args}, _From, State) ->
    try apply(Fun, Args) of
	Result ->
	    {reply, {ok, Result}, State, State#state.timeout}
    catch
	X:Y ->
	    {reply, {error, {X, Y}}, State, State#state.timeout}
    end;

handle_call(Request, _From, State) ->
    ?SAY("unexpected call: ~p", [Request]),
    {reply, ok, State, State#state.timeout}.


handle_cast({done, Pid}, #state{scenarios=SS}=State) ->
    NewSS = ordsets:del_element(Pid, SS),
    case ordsets:size(NewSS) of
	0 ->
	    {stop, normal, State#state{scenarios=NewSS}};
	_ ->
	    {noreply, State#state{scenarios=NewSS}, State#state.timeout}
    end;

handle_cast({done, Pid, {fail, _}=F}, #state{scenarios=SS}=State) ->
    NewSS = ordsets:del_element(Pid, SS),
    case ordsets:size(NewSS) of
	0 ->
	    ?SAY("setting status: ~p", [F]),
	    {stop, normal, State#state{scenarios=NewSS, status=F}};
	_ ->
	    ?SAY("setting status: ~p", [F]),
	    {noreply, State#state{scenarios=NewSS, status=F}, State#state.timeout}
    end;

handle_cast(Msg, State) ->
    ?SAY("unexpected cast: ~p", [Msg]),
    {noreply, State, State#state.timeout}.


handle_info(Info, State) ->
    ?SAY("ignored unexpected info: ~p", [Info]),
    {noreply, State, State#state.timeout}.


terminate(_Reason, #state{postlude=none, master=Master, status=Status}) ->
    ?SAY("about to terminate, status is: ~p", [Status]),
    Master ! {stop, Status},
    ok;

terminate(_Reason, #state{postlude=Fun}=State) ->
    apply(Fun, [State]),
    terminate(none, State#state{postlude=none}).

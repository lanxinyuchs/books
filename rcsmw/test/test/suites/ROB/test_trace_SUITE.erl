%%% %CCaseFile:	test_trace_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/1

%%% @doc ==Tests of trace by LTTng==
%%%
%%% Very simple tests for the time being. One has to look for trace
%%% manually; see the "Tracing in RBS CS with LTTng" IWD.
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% R2A/1      2014-02-19 erarafo     Created
%%% ----------------------------------------------------------
%%%


-module(test_trace_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/1').
-date('2014-02-19').
-author('erarafo').



-export([test_start/1,
	 test_stop/1,
	 test_start_15_stop/1
	 ]).

-export([
	 suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0
	]).





-include_lib("common_test/include/ct.hrl").

-define(DU, du1).
-define(NODE, node1).
-define(CHILD, child1).

-define(LTTNG, 14).

-define(LTTNG_TEST_TRACE_START, 1).
-define(LTTNG_TEST_TRACE_STOP, 2).


%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].


%% @hidden
-spec suite() -> config().

suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks,
      [{rct_htmllink, []},
       {rct_logging, {all, [{erlang, {["ERROR REPORT", "CRASH REPORT"], []}}]}},
       {rct_proxy, [{1, ?NODE, ssh_lmt_ipv4, ?DU, username}]},
       {rct_core, []}
      ]}
    ].


%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD, ?LTTNG),
    Config.


%% @hidden
-spec end_per_suite(config()) -> any().

end_per_suite(_Config) ->
    ok.


%% @hidden
-spec init_per_group(atom(), config()) -> config().

init_per_group(_GroupName, Config) ->
    Config.


%% @hidden
-spec end_per_group(atom(), config()) -> any().

end_per_group(_GroupName, _Config) ->
    ok.


%% @hidden
-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    Config.


%% @hidden
-spec end_per_testcase(atom(), config()) -> any().

end_per_testcase(_TestCase, _Config) ->
    ok.


%% @hidden
-spec groups() -> [{atom(), list(), list()}].

groups() ->
    [].


%% @hidden
-spec all() -> list() | {skip, term()}.

all() ->
    [test_start_15_stop
    ].


%%% ----------------------------------------------------------
%%% TEST CASES
%%% ----------------------------------------------------------

%%% @doc Start the 'test_trace' program (find it in the FAKE CXC).

test_start(_Config) ->
    {ok, PgmId} = rct_proxy:send_proxy(?NODE, ?CHILD, ?LTTNG_TEST_TRACE_START),
    ct:pal("test_trace pid: ~w~n"
	  "to stop the program use:~n"
	  "env TEST_TRACE_PID=~w rct_run.sh ...", [PgmId, PgmId]),
    ok.


%%% @doc Stop the running 'test_trace' program. Invoke with the
%%% environment variable TEST_TRACE_PID set to a numeric value: the
%%% PID that was reported by the test_stat case.

test_stop(_Config) ->
    case
	os:getenv("TEST_TRACE_PID") of
	false ->
	    ct:fail("environment variable TEST_TRACE_PID must be set");
	PidS ->
	    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD, ?LTTNG_TEST_TRACE_STOP, {PidS}),
	    ct:pal("successfully stopped 'programtest_trace'", []),
	    ok
    end.


%%% @doc Start, run for 15 seconds, stop.

test_start_15_stop(_Config) ->
    {ok, PgmId} = rct_proxy:send_proxy(?NODE, ?CHILD, ?LTTNG_TEST_TRACE_START),
    ct:pal("test_trace pid: ~w", [PgmId]),
    timer:sleep(15000),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD, ?LTTNG_TEST_TRACE_STOP, {PgmId}),
    ct:pal("successfully stopped 'test_trace'", []),
    ok.

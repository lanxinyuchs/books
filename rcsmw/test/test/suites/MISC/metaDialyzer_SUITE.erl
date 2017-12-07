%%% %CCaseFile:	metaDialyzer_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/1

%%% @doc ==Tests of XYZ==
%%% This test suite exercises ...

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% R3A/1      2015-02-23   erarafo     Created
%%% ----------------------------------------------------------
%%% 

-module(metaDialyzer_SUITE).
-id('Updated by CCase').
-vsn('/main/R3A/1').
-date('2015-02-23').
-author('erarafo').

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

-export([test_dialyzer/1]).


-include_lib("common_test/include/ct.hrl").

-define(METADIALYZER_SCRIPT, 
	filename:join(
	  os:getenv("RCT_TOP"), 
	  "test/lib/shell/metaDialyzer.sh")).


%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].


%% @hidden
-spec suite() -> config().

suite() -> [].


%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
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

groups() -> [].


%% @hidden
-spec all() -> list() | {skip, term()}.

all() -> [test_dialyzer].


%%% ----------------------------------------------------------
%%% TEST CASES
%%% ----------------------------------------------------------

test_dialyzer(_Config) ->
    case string:tokens(os:cmd(?METADIALYZER_SCRIPT), [$\n]) of
	[[$f, $a, $i, $l, $u, $r, $e, $:, $\s|Failure]|_] ->
	    ct:fail("script failed: ~s", [Failure]);
	["ok", RunNumberS, PageUrl] ->
	    ct:pal("Jenkins page being analyzed: ~s", [PageUrl]),
	    ct:pal("no significant Dialyzer complaints in run: ~s", [RunNumberS]),
	    ok;
	["ok", RunNumberS, PageUrl|Complaints] ->
	    ct:pal("Jenkins page being analyzed: ~s", [PageUrl]),
	    ct:fail("Dialyzer complaints, run: ~s: complaints -~n~p", [RunNumberS, Complaints])
    end.


%%% ----------------------------------------------------------
%%% NEGATIVE TEST CASES
%%% ----------------------------------------------------------



%%% ----------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

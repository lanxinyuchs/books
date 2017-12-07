%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cli_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R8A/1
%%%
%%% @doc == Cli operations ==
%%% This Test Suite can be used on target environment.<br/>
%%%
%%%
%%% @end

-module(cli_SUITE).

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
%%% R2A/1      2014-08-28 etxberb     Created.
%%% R2A/2      2014-10-08 etxberb     Validate functionality was implemented in
%%%                                   COMSA - test case changed accordingly.
%%% R3A/1      2014-11-26 etxpeno     Testcase 'validate_bidir_incorrect_uses'
%%%                                   is not applicable when using COM 5.0
%%% R3A/2      2015-07-14 etxjovp     Add group definitions used by CS CI
%%% R8A/1      2016-12-12 etxkols     Removing rs232 hook
%%% ----------------------------------------------------------
%%%

%%--------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%%-compile([export_all]).
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0
	]).

%%% Test cases
-export([conflict_sessions_edit_same_mo/1,
	 validate_bidir_missing_reservedBy/1
	]).

%%--------------------------------------------------------------------
-record(cliReq,
	{result,
	 sess,
	 answ
	}).

%%--------------------------------------------------------------------
-define(MODULE_STR, atom_to_list(?MODULE)).
-define(PRINT_OPT, print).

-define(MaxInstances, 5).
-define(MaxSessions, 5).

-define(SESS(No), list_to_atom(?MODULE_STR ++ "_sess" ++ integer_to_list(No))).
-define(INSTANCE(No), ?MODULE_STR ++ "_" ++ integer_to_list(No)).

-define(CLI_SingleSession, [?SESS(1)]).
-define(CLI_MultiSessions, [?SESS(N) || N <- lists:seq(1, ?MaxSessions)]).
-define(CLI_MultiInstances, [?INSTANCE(N) || N <- lists:seq(1, ?MaxInstances)]).

-define(CONFIGURE, "configure").
-define(COMMIT,    "commit").
-define(SHOW,      "show").
-define(TOP,       "top").
-define(UP,        "up").
-define(VALIDATE,  "validate").

-define(TESTROOT, "ManagedElement=1,TestRoot=1").

%%% TestClass including full path (from top)
-define(MO_TESTCLASS(ClassNo, Instance),
	?TESTROOT ++ ",TestClass" ++ ClassNo ++ "=" ++ Instance).
-define(DEL_MO_TESTCLASS(ClassNo, Instance),
	"no "++ ?MO_TESTCLASS(ClassNo, Instance)).
-define(SHOW_MO_TESTCLASS(ClassNo, Instance),
	"show "++ ?MO_TESTCLASS(ClassNo, Instance)).

%%% TestClass from ?TESTROOT level
-define(TESTCLASS(ClassNo, Instance),
	"TestClass" ++ ClassNo ++ "=" ++ Instance).
-define(DEL_TESTCLASS(ClassNo, Instance),
	"no "++ ?TESTCLASS(ClassNo, Instance)).
-define(SHOW_TESTCLASS(ClassNo, Instance),
	"show "++ ?TESTCLASS(ClassNo, Instance)).

%%% Attributes
-define(ATTR_INT32(Value), "int32=" ++ Value).
-define(ATTR_USES(ClassNo, Instance),
	"uses=[\"ManagedElement=1,TestRoot=1,TestClass" ++ ClassNo ++ "=" ++
	Instance ++ "\"]").
-define(ATTR_USES_incorrect(ClassNo, Instance),
	"uses=[\"ManagedElement=1,TestClass" ++ ClassNo ++ "=" ++ Instance ++
	"\"]").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_cli hook for each user, to be able to use cli for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of CliHooks tuples with differents Names.
    CliHooks = [{rct_cli, {?SESS(N), [manual_connect]}} ||
		   N <- lists:seq(1, ?MaxSessions)] ,
    %%ct:pal("### CliHooks: ~p", [CliHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink, []},
		 {cth_conn_log, []},
		 {rct_power, node},
		 %% {rct_cli, {check_session, [manual_connect]}},
		 {rct_core, []},
		 {rct_logging, {all,
				[{erlang,
				  {["ERROR REPORT", "CRASH REPORT"],
				   ["The job was brutally killed - exiting"]}}]
			       }} |
		 CliHooks
     		]}].

%% @hidden
init_per_suite(Config) ->
    ct:pal("### Config: ~p", [Config]),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_group(GroupName, Config) ->
    ct:pal("##################~n### Test group ###  ~p~n##################",
	   [GroupName]),
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("#################~n### Test case ###  ~p~n#################",
	   [TestCase]),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.~nClean up added instances",
		   [Reason]),
	    A = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"],
			     5000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli -u $USER"],
			     5000, noprint),
	    BB = length(string:tokens(B, "\n")),
	    ct:pal("### Com: ~p", [A]),
	    ct:pal("### Cli: ~p", [BB])
    end,
    %%
    %% Close connections
    %%
    session_disconnect(?CLI_MultiSessions),
    %%
    %% Re-open a connection and clean up instances.
    %%
    try
	lists:foreach(fun(Sess) ->
			      clean_up(rct_cli:connect(Sess, noprint), Sess)
		      end,
		      ?CLI_MultiSessions)
    catch
	throw : clean_up_done ->
	    ok
    end,
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, print),
    ok.

%%--------------------------------------------------------------------
clean_up(ok, Sess) ->
    delete_instances(Sess, ?CLI_MultiInstances),
    throw(clean_up_done);
clean_up({error, already_connected}, Sess) ->
    %% Clean up at our client, just in case cli process was killed on node,
    rct_cli:disconnect(Sess, noprint),
    %% Then set up again.
    case rct_cli:connect(Sess, noprint) of
	ok ->
	    delete_instances(Sess, ?CLI_MultiInstances),
	    throw(clean_up_done);
	Error ->
	    ct:pal("### Cleaning up, connection failed:~n"
		   "rct_cli:connect(~p, noprint) -> ~p",
		   [Sess, Error]),
	    Error
    end;
clean_up(Unknown, Sess) ->
    ct:pal("### Cleaning up, connection failed:~n"
	   "rct_cli:connect(~p, noprint) -> ~p",
	   [Sess, Unknown]),
    Unknown.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     conflict_sessions_edit_same_mo,
     validate_bidir_missing_reservedBy
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runned by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []},
     {conflict, [], [conflict_sessions_edit_same_mo]},
     {validate, [], [validate_bidir_missing_reservedBy]}
    ].

%%%====================================================================
%%% Test cases
%%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Add TestClass1, then edit same attr from two different sessions.<br/>
%% - Create an instance with an attribute set to a value.<br/>
%% - Commit.<br/>
%% - Change attribute value from session(1).<br/>
%% - Change attribute value from session(2).<br/>
%% - Commit(1) success.<br/>
%% - Commit(2) fail.<br/>
%% @spec conflict_sessions_edit_same_mo(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
conflict_sessions_edit_same_mo(_Config) ->
    session_connect([?SESS(1), ?SESS(2)]),
    %% ------- prepare, create an instance and set attribute:
    cli_cmd(?SESS(1), ?CONFIGURE),
    check(ok,
	  cli_cmd(?SESS(1), ?MO_TESTCLASS("1", ?INSTANCE(1)))),
    check(ok,
	  cli_cmd(?SESS(1), ?ATTR_INT32("88"))),
    check(ok,
	  cli_cmd(?SESS(1), ?COMMIT),
	  {contains_not, ["ERROR", "WARNING"]},
	  [caseless]),
    %% ------- conflicting change of attribute:
    cli_cmd(?SESS(1), ?CONFIGURE),
    cli_cmd(?SESS(2), ?CONFIGURE),
    check(ok,
	  cli_cmd(?SESS(1), ?ATTR_INT32("11"))),
    check(ok,
	  cli_cmd(?SESS(2), ?MO_TESTCLASS("1", ?INSTANCE(1)))),
    check(ok,
	  cli_cmd(?SESS(2), ?ATTR_INT32("22"))),
    check(ok,
	  cli_cmd(?SESS(1), ?COMMIT),
	  {contains_not, ["ERROR", "WARNING"]},
	  [caseless]),
    check(ok,
	  cli_cmd(?SESS(2), ?COMMIT),
	  {contains, ["ERROR",
		      "Data has been changed in another transaction"]}),
    cli_cmd(?SESS(1), ?UP),
    check(found,
	  cli_show(?SESS(1), ?SHOW_TESTCLASS("1", ?INSTANCE(1))),
	  {contains, ["int32=11"]}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Add TestClass8, then validate missing bidir referenced object.<br/>
%% - Add instance.<br/>
%% - Add correct bidir reference to TestClass9.<br/>
%% - Validate reference to missing class (COMSA).<br/>
%% - Add missing TestClass9 (referenced from TestClass8).<br/>
%% - Commit.<br/>
%% @spec validate_bidir_missing_reservedBy(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
validate_bidir_missing_reservedBy(_Config) ->
    session_connect(?SESS(1)),
    cli_cmd(?SESS(1), ?CONFIGURE),
    check(ok,
	  cli_cmd(?SESS(1), ?MO_TESTCLASS("8", ?INSTANCE(1)))),
    check(ok,
	  cli_cmd(?SESS(1), ?ATTR_USES("9", ?INSTANCE(1)))),
    check(ok,
	  cli_cmd(?SESS(1), ?VALIDATE),
	  {contains, ["Transaction validation failed",
		      "Attribute 'uses' in object",
		      "is a reference to non existent object"]}),
    cli_cmd(?SESS(1), ?TOP),
    check(ok,
	  cli_cmd(?SESS(1), ?MO_TESTCLASS("9", ?INSTANCE(1)))),
    check(ok,
	  cli_cmd(?SESS(1), ?VALIDATE),
	  {contains, ["Transaction is valid!"]}),
    check(ok,
	  cli_cmd(?SESS(1), ?COMMIT),
	  {contains_not, ["ERROR", "WARNING"]},
	  [caseless]),
    cli_cmd(?SESS(1), ?TOP),
    check(found,
	  cli_show(?SESS(1), ?SHOW_MO_TESTCLASS("8", ?INSTANCE(1))),
	  {contains, ["uses"]}),
    check(found,
	  cli_show(?SESS(1), ?SHOW_MO_TESTCLASS("9", ?INSTANCE(1))),
	  {contains, ["reservedBy"]}),
    ok.

%%%====================================================================
%%% Internal functions
%%%====================================================================
%%--------------------------------------------------------------------
check(ExpectedResult, Results) when is_atom(ExpectedResult) ->
    check(lists:duplicate(length(Results), ExpectedResult), Results);
check(ExpectedResults, Results) ->
    check(ExpectedResults, Results, {contains, []}).

%%--------------------------------------------------------------------
check(ExpectedResult, Results, ContentCheck) when is_atom(ExpectedResult) ->
    check(lists:duplicate(length(Results), ExpectedResult),
	  Results,
	  ContentCheck);
%%--------------------------------------------------------------------
check(ExpectedResults, Results, ContentCheck) ->
    check(ExpectedResults, Results, ContentCheck, []).

%%--------------------------------------------------------------------
check([ExpectedResult | Tail1],           % <===================| match
      [#cliReq{result = ExpectedResult,   % <===================|
	       sess = SessionName,
	       answ = Answ} | Tail2],
      ContentCheck,
      Opts) ->
    check_content(ContentCheck, SessionName, Answ, Opts),
    check(Tail1, Tail2, ContentCheck, Opts);
check([ExpectedResult | _],
      [#cliReq{result = Result, sess = SessionName} | _],
      _,
      _) ->
    ct:fail("### Expected result in session ~p: ~p ### Got: ~p",
	    [SessionName, ExpectedResult, Result]);
check([], [], _, _) ->
    ok;
check(ExpectedResult, Results, ContentCheck, Opts)
  when is_atom(ExpectedResult) ->
    check(lists:duplicate(length(Results), ExpectedResult),
	  Results,
	  ContentCheck,
	  Opts).

%%--------------------------------------------------------------------
check_content({contains, ExpectedSubstrings}, SessionName, Answ, Opts) ->
    case string_contains(ExpectedSubstrings, Answ, Opts) of
	ok ->
	    ok;
	Error ->
	    ct:pal("### ~p:~n~s", [SessionName, Answ]),
	    ct:fail("~p", [Error])
    end;
check_content({contains_not, ExpectedSubstrings}, SessionName, Answ, Opts) ->
    case string_contains_not(ExpectedSubstrings, Answ, Opts) of
	ok ->
	    ok;
	Error ->
	    ct:pal("### ~p:~n~s", [SessionName, Answ]),
	    ct:fail("~p", [Error])
    end.

%%--------------------------------------------------------------------
cli_cmd([Sess | Tail], Cmd) ->
    {Result, Answ} = rct_cli:send(Sess, Cmd, ?PRINT_OPT),
    ct:log("### ~p:~n~s", [Sess, Answ]),
    [#cliReq{result = Result, sess = Sess, answ = Answ} | cli_cmd(Tail, Cmd)];
cli_cmd([], _) ->
    [];
cli_cmd(CLI_SessionName, Cmd) when is_atom(CLI_SessionName) ->
    cli_cmd([CLI_SessionName], Cmd).

%%--------------------------------------------------------------------
cli_show([Sess | Tail], Cmd) ->
    {ok, Answ} = rct_cli:send(Sess, Cmd, ?PRINT_OPT),
    Result =
	case string:str(Answ, "Specific element not found") of
	    0 ->
		ct:log("### ~p:~n~s", [Sess, Answ]),
		found;
	    _ ->
		ct:log("### ~p:~n~s~n~s", [Sess, Cmd, "Object not found"]),
		not_found
	end,
    [#cliReq{result = Result, sess = Sess, answ = Answ} | cli_show(Tail, Cmd)];
cli_show([], _) ->
    [];
cli_show(CLI_SessionName, Cmd) when is_atom(CLI_SessionName) ->
    cli_show([CLI_SessionName], Cmd).

%%--------------------------------------------------------------------
delete_instances(SessionName, CLI_InstanceNames) ->
    rct_cli:send(SessionName, ?CONFIGURE, noprint),
    rct_cli:send(SessionName, ?TOP, noprint),
    Fun =
	fun(ClassNo) ->
		[?DEL_MO_TESTCLASS(ClassNo, InstName) ++ "\r" ||
		    InstName <- CLI_InstanceNames]
	end,
    Cmds =
	lists:flatten(lists:map(Fun, ["1", "2", "8", "9"])),
    rct_cli:send(SessionName, Cmds, noprint),
    rct_cli:send(SessionName, ?COMMIT, noprint).

%%--------------------------------------------------------------------
session_connect(CLI_SessionName) when is_atom(CLI_SessionName) ->
    session_connect([CLI_SessionName]);
session_connect(CLI_SessionNames) ->
    Result =
	lists:map(fun(Sess) ->
			  {session_connect_try(Sess), Sess}
		  end,
		  CLI_SessionNames),
    ct:pal("### CLI sessions connected:~n~p", [Result]).

%%--------------------------------------------------------------------
session_connect_try(Sess) ->
    %% ok = rct_cli:connect(Sess, "expert", "expert", "[***]\r\n>$")
    session_connect_try(rct_cli:connect(Sess), Sess, 30000).

session_connect_try(ok, _, _) ->
    ok;
session_connect_try(Err, Sess, Timeout) when Timeout >= 0 ->
    ct:log("Failed to connect to cli session: ~p. Sleep and try again. "
	   "Result: ~p",
	   [Sess, Err]),
    timer:sleep(1000),
    session_connect_try(rct_cli:connect(Sess), Sess, Timeout - 1000);
session_connect_try(Err, Sess, _) ->
    ct:log("Failed to connect to cli session: ~p. Result: ~p", [Sess, Err]),
    ct:fail("### Could not connect to cli session ~p within 30 sec.", [Sess]).

%%--------------------------------------------------------------------
session_disconnect(CLI_SessionName) when is_atom(CLI_SessionName) ->
    session_disconnect([CLI_SessionName]);
session_disconnect(CLI_SessionNames) ->
    Result =
	lists:map(fun(Sess) ->
			  {rct_cli:disconnect(Sess), Sess}
		  end,
		  CLI_SessionNames),
    ct:pal("### CLI sessions disconnected:~n~p", [Result]).

%%--------------------------------------------------------------------
string_contains([Substr | Tail], Str, Opts) ->
    case re:run(Str, Substr, Opts) of
	nomatch ->
	    {error, {does_not_contain, Substr}};
	{match, _} ->
	    string_contains(Tail, Str, Opts)
    end;
string_contains([], _, _) ->
    ok.

%%--------------------------------------------------------------------
string_contains_not([Substr | Tail], Str, Opts) ->
    case re:run(Str, Substr, Opts) of
	nomatch ->
	    string_contains_not(Tail, Str, Opts);
	{match, _} ->
	    {error, {contains, Substr}}
    end;
string_contains_not([], _, _) ->
    ok.

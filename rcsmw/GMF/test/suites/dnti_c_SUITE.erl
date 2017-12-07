%%% %CCaseFile:	dnti_c_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/R4A/1

%%% @doc ==Tests of XYZ==
%%% This test suite exercises the DNTI interface.
%%%
%%% To run the dialyzer on this module:
%%%
%%% dialyzer dnti_c_SUITE.erl $RCT_TOP/test/lib/rct-proxy/esrc/rct_proxy.erl
%%% @end

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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


-module(dnti_c_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R2A/7      2013-02-28 etxkols     Added rct_core hook
%%% R2A/8      2013-04-17 etxjovp     change timetrap to 30
%%% R3A/1      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% ----------------------------------------------------------

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

-export([
	 dnti_oneshot_positive/1,
	 dnti_oneshot_nonexisting_instances_positive/1,
	 dnti_longlived_positive/1,
	 dnti_code_example_positive/1,
	 dnti_massive_test_positive/1,
	 dnti_two_clients_scenario_positive/1,
	 dnti_interactive/1
	 ]).

-export([
	 dnti_terminate_service_negative/1,
	 %% dnti_longlived_invalid_handle_negative/1,
	 dnti_invalid_parameter_direction_negative/1,
	 dnti_mo_class_not_found_negative/1,
	 dnti_parent_not_found_negative/1,
	 dnti_invalid_parameter_todnp_a_negative/1,
	 dnti_invalid_parameter_todnp_b_negative/1
	 ]).

-export([
	 handleForm/3
	]).



-include_lib("common_test/include/ct.hrl").

-include("test_dnti.hrl").


%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].


%% @hidden
-spec suite() -> config().

suite() ->
   [
    {timetrap, {minutes, 30}},
    {ct_hooks, [{rct_htmllink,[]},
		{rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					      []}}]}},
                {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		{rct_core,[]}
	       ]}
   ].


%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    {ok, client_started} =
        rct_proxy:start_proxy(node1, dnti1, ?DNTI),
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
    {ok} =
	send(?CLIENT_0, ?DNTI_CLEANUP, ?IGNORED_TRANSPORT, ?IGNORED_DIRECTION, ""),
    ok.


%% @hidden
-spec groups() -> [{atom(), list(), list()}].

groups() ->
    AllGroup = all(),
    [{default__group, [], AllGroup},
    {sbc__qual__all__1__group, [], []},
    {sbc__def__all__1__group, [], []},
    {sbc__upgrade__all__1__group, [], []},
    {sdc__cover__sim__1__group, [], []},
    {sdc__def__all__1__group, [], []},
    {sdc__qual__all__1__group, [], []}
    ].


%% @hidden
-spec all() -> list() | {skip, term()}.

all() ->
    [
     dnti_oneshot_positive,
     dnti_oneshot_nonexisting_instances_positive,
     dnti_longlived_positive,
     dnti_code_example_positive,
     dnti_massive_test_positive,
     dnti_two_clients_scenario_positive,
     dnti_terminate_service_negative,
     %% dnti_longlived_invalid_handle_negative,
     dnti_invalid_parameter_direction_negative,
     dnti_mo_class_not_found_negative,
     dnti_parent_not_found_negative,
     dnti_invalid_parameter_todnp_a_negative,
     dnti_invalid_parameter_todnp_b_negative
    ].


%%% ----------------------------------------------------------
%%% TEST CASES
%%% ----------------------------------------------------------
dnti_oneshot_positive(_Config) ->

    {ok, ?DNTI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok, ?DNTI_OK, "ManagedElement=1,TestRoot=1,TestClass1=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass1Id=1,TESTMOMtestRootId=1"),

    {ok, ?DNTI_OK, "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,TestRoot=1,TestClass2=1,TestClass3=1"),

    {ok, ?DNTI_OK, "ManagedElement=1,TestRoot=1,TestClass2=1,TestClass3=1"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass3Id=1,testClass2Id=1,TESTMOMtestRootId=1"),

    ok.


%% @doc Transform DNs with RND value parts not present in the current MO tree.

dnti_oneshot_nonexisting_instances_positive(_Config) ->
    {ok, ?DNTI_OK, "testClass1Id=carbon,TESTMOMtestRootId=ALPHA"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=17,TestRoot=ALPHA,TestClass1=carbon"),
    {ok, ?DNTI_OK, "ManagedElement=1,TestRoot=ALPHA,TestClass1=carbon"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass1Id=carbon,TESTMOMtestRootId=ALPHA"),
    ok.


%% @doc Multiple transformations, using long-lived connection, both ways.

dnti_longlived_positive(_Config) ->
    {ok} =
	initiateService(?CLIENT_1),

    {ok, ?DNTI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok, ?DNTI_OK, "ManagedElement=1,TestRoot=1,TestClass1=1"} =
	to3gpp(?CLIENT_1, ?LONGLIVING, "testClass1Id=1,TESTMOMtestRootId=1"),

    {ok} =
	terminateService(?CLIENT_1),

    ok.


%% @doc Exercise the code example from the IWD.

dnti_code_example_positive(_Config) ->
    {ok, ?DNTI_OK} =
	send(
	  ?CLIENT_0,
	  ?DNTI_CODE_EXAMPLE,
	  ?IGNORED_TRANSPORT,
	  ?MIM_TO_IMM, "ManagedElement=1,TestRoot=1,TestClass1=1"),
    ok.


%% @doc Perform multiple transformations. It seems that each transformation
%% takes of the order 50 ms in the simulated environment on a VDI.

dnti_massive_test_positive(_Config) ->
    {ok} =
	initiateService(?CLIENT_1),

    repeatSend(?REPEAT,
	       ?CLIENT_1,
	       ?MIM_TO_IMM,
	       "ManagedElement=1,TestRoot=1,TestClass1=1",
	       "testClass1Id=1,TESTMOMtestRootId=1"),

    {ok} =
	terminateService(?CLIENT_1),

    {ok} =
	initiateService(?CLIENT_1),

    repeatSend(?REPEAT,
	       ?CLIENT_1,
	       ?IMM_TO_MIM,
	       "testClass1Id=1,TESTMOMtestRootId=1",
	       "ManagedElement=1,TestRoot=1,TestClass1=1"),
    {ok} =
	terminateService(?CLIENT_1),

    ok.


%% @doc Scenario where two clients are active at the same time.

dnti_two_clients_scenario_positive(_Config) ->
    {ok} =
	initiateService(?CLIENT_1),

    {ok, ?DNTI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok} =
	initiateService(?CLIENT_2),

    {ok, ?DNTI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok, ?DNTI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_2, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok} =
	terminateService(?CLIENT_1),

    {ok, ?DNTI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
	toImm(?CLIENT_2, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    {ok} =
	terminateService(?CLIENT_2),

    ok.



%% @doc Interactive test. Use a web browser to interact with the test case.
%% Look in the console output for the URL to be opened.

dnti_interactive(_Config) ->
    ct:timetrap({minutes, 100}),
    inets:start(),
    ct:print("service info: ~p", [inets:services_info()]),
    ServerPid = startHttpd(),
    register(dnti_interactive, self()),
    receive
	X ->
	    ct:print("test case received: ~p, finishing", [X])
    end,
    inets:stop(httpd, ServerPid),
    inets:stop(),
    ok.




%%% ----------------------------------------------------------
%%% NEGATIVE TEST CASES
%%% ----------------------------------------------------------

%% @doc This test just verifies behaviour of test_dnti.c;
%% the DNTI interface is not invoked.

dnti_terminate_service_negative(_Config) ->
    {error, "no active connection for client", ?CLIENT_1} =
	terminateService(?CLIENT_1),
    ok.


%% This test is actually not safe to execute for now. It will cause the
%% DNTI interface to try and send data on a random-numbered socket. The
%% operation may result in an error return (which is a good behaviour), or
%% a thread crash (which has also been observed).
%%
%% %% @doc Verify error handling: Trying to use a handle after it was terminated.
%%
%% dnti_longlived_invalid_handle_negative(_Config) ->
%%     {ok} =
%% 	initiateService(?CLIENT_1),
%%
%%     {ok, ?DNTI_OK, "testClass1Id=1,TESTMOMtestRootId=1"} =
%% 	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),
%%
%%     {ok} =
%% 	send(?CLIENT_1, ?DNTI_TERMINATE_SERVICE_KEEP_HANDLE, ?IGNORED_TRANSPORT, ?IGNORED_DIRECTION, ?IGNORED_DN),
%%
%%     {ok, ?DNTI_SEND_ERROR, "DNTI_SEND_ERROR"} =
%% 	toImm(?CLIENT_1, ?LONGLIVING, "ManagedElement=1,TestRoot=1,TestClass1=1"),
%%
%%     ok.


%% @doc Verify error handling: Invalid transformation direction.

dnti_invalid_parameter_direction_negative(_Config) ->
    InvalidDirection = 17,
    {ok, ?DNTI_INVALID_PARAMETER_DIRECTION, "DNTI_INVALID_PARAMETER_DIRECTION"} =
	send(?CLIENT_0, ?DNTI_TRANSFORM, ?ONESHOT, InvalidDirection, "ManagedElement=1,XyzFunction=1"),
    ok.


%% @doc Verify error handling: MO class not found.

dnti_mo_class_not_found_negative(_Config) ->
    {ok, ?DNTI_OBJECT_CLASS_NOT_FOUND, "XyzFunction"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,XyzFunction=1"),

    {ok, ?DNTI_OBJECT_CLASS_NOT_FOUND, "ManagedThing"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedThing=1,XyzFunction=1"),

    {ok, ?DNTI_OBJECT_CLASS_NOT_FOUND, "nosuchClassId"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "nosuchClassId=33"),

    {ok, ?DNTI_OBJECT_CLASS_NOT_FOUND, "testClass99Id"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass3Id=1,testClass99Id=1,TESTMOMtestRootId=1"),

    {ok, ?DNTI_OBJECT_CLASS_NOT_FOUND, "TESTMOMnosuchTestRootId"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass3Id=1,testClass2Id=1,TESTMOMnosuchTestRootId=1"),

    {ok, ?DNTI_OBJECT_CLASS_NOT_FOUND, "MISSINGMOMtestRootId"} =
	to3gpp(?CLIENT_0, ?ONESHOT, "testClass3Id=1,testClass2Id=1,MISSINGMOMtestRootId=1"),

    ok.

dnti_parent_not_found_negative(_Config) ->
    {ok,
     ?DNTI_OBJECT_CLASS_NOT_FOUND,
     "ManagedElement not valid parent for TestClass3"} =
	toImm(?CLIENT_0, ?ONESHOT, "ManagedElement=1,TestClass3=1").



%% @doc Verify error handling: Invoking dntiTransform with an incorrect
%% output parameter: the location specified by the 4th argument should
%% contain NULL but instead contains a non-NULL pointer.

dnti_invalid_parameter_todnp_a_negative(_Config) ->

    {ok, ?DNTI_INVALID_PARAMETER_TODNP, "DNTI_INVALID_PARAMETER_TODNP"} =
	send(?CLIENT_0, ?DNTI_PASS_INVALID_TODNP_A, ?ONESHOT, ?MIM_TO_IMM, "X=1,Y=1"),

    ok.


%% @doc Verify error handling: Invoking dntiTransform with the 4th argument
%% set to NULL.

-spec dnti_invalid_parameter_todnp_b_negative(config()) -> ok.

dnti_invalid_parameter_todnp_b_negative(_Config) ->

    {ok, ?DNTI_INVALID_PARAMETER_TODNP, "DNTI_INVALID_PARAMETER_TODNP"} =
	send(?CLIENT_0, ?DNTI_PASS_INVALID_TODNP_B, ?ONESHOT, ?MIM_TO_IMM, "ManagedElement=1,TestRoot=1,TestClass1=1"),

    ok.



%%% ----------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%% @doc Set up a long-lived connection.

-spec initiateService(integer()) -> tuple().

initiateService(ClientId) ->
    send(ClientId,
	 ?DNTI_INITIATE_SERVICE,
	 ?IGNORED_TRANSPORT,
	 ?IGNORED_DIRECTION,
	 ?IGNORED_DN).


%% @doc Tear down a long-lived connection.

-spec terminateService(integer()) -> tuple().

terminateService(ClientId) ->
    send(ClientId,
	 ?DNTI_TERMINATE_SERVICE,
	 ?IGNORED_TRANSPORT,
	 ?IGNORED_DIRECTION,
	 ?IGNORED_DN).


%% @doc Transform a DN from 3GPP to IMM format.

-spec toImm(integer(), boolean(), string()) -> tuple().

toImm(ClientId, LongLiving, InDn) ->
    send(ClientId,
	 ?DNTI_TRANSFORM,
	 LongLiving,
	 ?MIM_TO_IMM,
	 InDn).


%% @doc Transform a DN from IMM to 3GPP format.

-spec to3gpp(integer(), boolean(), string()) -> tuple().

to3gpp(ClientId, LongLiving, InDn) ->
    send(ClientId,
	 ?DNTI_TRANSFORM,
	 LongLiving,
	 ?IMM_TO_MIM,
	 InDn).


%% @doc Send a request N times. A long-living connection is assumed.

-spec repeatSend(integer(), integer(), integer(), string(), string()) -> ok.

repeatSend(0, _ClientId, _Direction, _InDn, _OutDn) ->
    ok;

repeatSend(N, ClientId, Direction, InDn, OutDn) ->
    {ok, ?DNTI_OK, OutDn} =
	send(ClientId, ?DNTI_TRANSFORM, ?LONGLIVING, Direction, InDn),
    repeatSend(N-1, ClientId, Direction, InDn, OutDn).


%% @doc Send a request to the IFT application.

-spec send(integer(), integer(), boolean(), integer(), string()) -> tuple().

send(ClientId, Function, LongLivingTn, Direction, InDn) ->
    rct_proxy:send_proxy(node1,
			 dnti1,
			 Function,
			 {ClientId, LongLivingTn, Direction, length(InDn), InDn}).


%% @doc Starts an HTTP service for the interactive test case.

-spec startHttpd() -> pid().

startHttpd() ->
    ct:print("startHttpd()", []),
    ServerDir = "/dev/shm/"++os:getenv("LOGNAME")++"/server",
    ct:print("startHttpd() 1", []),
    os:cmd(io_lib:format("mkdir -p ~s", [ServerDir])),
    ct:print("startHttpd() 2", []),
    {ok, Pid} = inets:start(httpd,
			     [{modules, [mod_esi]},
			      {port, ?DNTI_INTERACTIVE_HTTP_PORT},
			      {server_name, "dnti_interactive"},
			      {server_root, ServerDir},
			      {document_root, "/"},
			      {bind_address, any},
			      {ipfamily, inet},
			      {erl_script_alias, {"/esi", [dnti_c_SUITE, io]}},
			      {erl_script_nocache, true}
			     ]),
    ct:print("startHttpd() 3", []),
    ct:print("services info: ~p", [inets:services_info()]),
    ct:print("to continue this testcase point a browser to http://localhost:~w/esi/~s/handleForm",
	     [?DNTI_INTERACTIVE_HTTP_PORT, ?MODULE]),
    Pid.


%% @doc Handles a GET request and delivers an HTML page in response.

-spec handleForm(term(), list(), string()) -> ok | {error, term()}.

handleForm(SessionID, Env, RawInput) ->
    {_, HttpHost} = lists:keyfind(http_host, 1, Env),
    Url = io_lib:format("http://~s/esi/~s/handleForm", [HttpHost, ?MODULE]),
    Input = httpd:parse_query(RawInput),

    Substs = [
	      {url, Url},
	      {to_imm, integer_to_list(?MIM_TO_IMM)},
	      {to_3gpp, integer_to_list(?IMM_TO_MIM)},
	      {module, atom_to_list(?MODULE)},
	      {now, io_lib:format("~p", [os:timestamp()])},
	      {input, io_lib:format("~p", [Input])}
	     ],

    case lists:keyfind("quit", 1, Input) of
	{_, "true"} ->
	    mod_esi:deliver(SessionID,
			    ["Content-Type: text/html\r\n\r\n",
			     byePage()
			    ]),
	    dnti_interactive ! finish;
	_ ->
	    case lists:keyfind("translate", 1, Input) of
		{_, "true"} ->

		    {_, Direction} = lists:keyfind("direction", 1, Input),
		    {_, InputDn} = lists:keyfind("inputDn", 1, Input),

		    {ok, Status, OutputDn} =
			send(?CLIENT_0, ?DNTI_TRANSFORM, false, list_to_integer(Direction), InputDn),

		    OutputDnForDisplay = if Status =:= ?DNTI_OK -> OutputDn; true -> "" end,

		    MoreSubsts = [{status, decodeStatus(Status)}, {outputDn, OutputDnForDisplay}|Substs],

		    mod_esi:deliver(SessionID, [
						"Content-Type: text/html\r\n\r\n",
						subst(MoreSubsts, translationForm(), [])
					       ]);
		_ -> % not supposed to happen!?
		    mod_esi:deliver(SessionID, [
						"Content-Type: text/html\r\n\r\n",
						subst(Substs, translationForm(), [])
					       ])

	    end
    end.



%% @doc Returns a list of strings and atoms that is an HTML page.

-spec byePage() -> [atom() | string()].

byePage() ->
    [
     "<html><head><title>Thank you</title></head>",
     "<body><big>Thank you for using RBS CS.</big></body></html>"
     ].


%% @doc Returns a list of strings and atoms that is an HTML page
%% template. Atoms are intended to be replaced by strings before the
%% document is delivered.

-spec translationForm() -> [atom() | string()].

translationForm() ->
    [
     "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>",
     "<html>",
     "  <head>",
     "    <meta content='text/html; charset=ISO-8859-1'",
     "      http-equiv='Content-Type'>",
     "    <title>Distinguished Name Translation</title>",
     "  </head>",
     "  <body>",
     "    <h2> Distinguished Name Translation</h2>",
     "    <p><big>using the DNTI interface on RBS CS</big><br>",
     "    </p>",
     "    <table border='3' cellpadding='2' cellspacing='2' width='100%'>",
     "      <tbody>",
     "        <tr>",
     "          <td valign='top' bgcolor='#bbffff'> <big>",
     "              <form action='", url,"'",
     "                method='get'><br>",
     "                <input name='direction' value='",to_imm,"' checked='checked'",
     "                  type='radio'> 3GPP -&gt; IMM<br>",
     "                <input name='direction' value='",to_3gpp,"' type='radio'> IMM",
     "                -&gt; 3GPP<br>",
     "                <br>",
     "                &nbsp;<input name='inputDn' size='80' maxlength='80'",
     "                  type='text'><br>",
     "                <br>",
     "                <button name='translate' value='true' type='submit'>Translate</button>",
     "                <button name='quit' value='true' type='submit'>Quit</button><br>",
     "                <br>",
     "                Status: ",status,"<br>",
     "                <br>",
     "                Translation: ",outputDn,"<br>",
     "                <br>",
     "              </form>",
     "            </big> </td>",
     "        </tr>",
     "      </tbody>",
     "    </table>",
     now, "<br>",
     input, "<br>",
     "  </body>",
     "</html>"
    ].


%% @doc Replaces atoms by strings in the given list of atoms and strings.
%% The first argument is the translations that can be made.
%% The second argument is the "input document" to be translated. The
%% third argument is a prefix document in reverse order that will not
%% be translated (typically the empty list).
%%
%% No check is made that the given translations are used. An atom that
%% cannot be looked up in the translation table causes an empty string
%% to be inserted.

-spec subst([{atom(), string()}], [atom() | string()], [string()]) -> [string()].

subst(_Translations, [], Result) ->
    lists:reverse(Result);

subst(Translations, [Item|Tail], Result) when is_atom(Item) ->
    case lists:keyfind(Item, 1, Translations) of
	false ->
	    subst(Translations, Tail, [""|Result]);
	{_, Replacement} ->
	    subst(Translations, Tail, [Replacement|Result])
    end;

subst(Translations, [Item|Tail], Result) ->
    subst(Translations, Tail, [Item|Result]).


%% @doc Translates status code to symbolic name. This function
%% must match the set of 'define' directives for the status codes.

-spec decodeStatus(integer()) -> string().

decodeStatus(Status) ->
    if
	Status =:= ?DNTI_OK ->
	    "DNTI_OK";
	Status =:= ?DNTI_INVALID_PARAMETER_DIRECTION ->
	    "DNTI_INVALID_PARAMETER_DIRECTION";
	Status =:= ?DNTI_INVALID_PARAMETER_TODNP ->
	    "DNTI_INVALID_PARAMETER_TODNP";
	Status =:= ?DNTI_SEND_ERROR ->
	    "DNTI_SEND_ERROR";
	Status =:= ?DNTI_RECEIVE_ERROR ->
	    "DNTI_RECEIVE_ERROR";
	Status =:= ?DNTI_OBJECT_CLASS_NOT_FOUND ->
	    "DNTI_OBJECT_CLASS_NOT_FOUND";
	true ->
	    "undefined"
    end.

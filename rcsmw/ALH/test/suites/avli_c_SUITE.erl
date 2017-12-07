%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	avli_c_SUITE.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/R6A/2
%%%
%%% @doc ==Basic Test Suite for the AVLI interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(avli_c_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/2').
-date('2016-09-05').
-author('etxarnu').

%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R2A/3      2013-02-28 etxkols     Added rct_core hook
%%% R2A/4      2013-04-17 etxjovp     change timetrap to 30
%%% R3A/1      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-09-30 etxberb     Changed 'now()' to 'os:timestamp()'.
%%% R5A/1      2015-12-04 etomist     Added test for AVLI ITC
%%% R5A/2      2015-12-29 etomist     Removed test for AVLI ITC
%%% R6A/1      2016-08-23 etxarnu     Added writeHw5Event (but commented out)
%%%                                   Removed avli_interactive since not used
%%% R6A/2      2016-09-05 etxarnu     Added writeHw5Event 
%%% ----------------------------------------------------------

-export([suite/0,
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
	 avli_fail/1,
	 avli_something/1,
	 avli_normal_empty/1,
	 avli_normal/1
	]).



-include_lib("common_test/include/ct.hrl").
-include("test_avli.hrl").
-include("avli_form_template.hrl").

-define(FORM_FAILURE,
	["<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'> ",
	 "<html> ",
	 "  <head> ",
	 "    <meta content='text/html; charset=ISO-8859-1' ",
	 "      http-equiv='Content-Type'> ",
	 "    <title>Application error</title> ",
	 "  </head> ",
	 "  <body> ",
	 "    <h2>Application error</h2> ",
	 "    <p>", intro, ": ", message, "<br> ", "<br>Stack trace: ", stacktrace, "<br>",
	 "    </p> ",
	 "    <p><a href='", url, "'><b>Continue</b></a><br> ",
	 "    </p> ",
	 "  </body> ",
	 "</html> "
	]
       ).



%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------

-spec suite() -> [tuple()].

suite() ->
   [{timetrap, {minutes, 30}},
    {ct_hooks, [{rct_htmllink,[]},
                {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
                {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		{rct_core,[]}
	       ]}].

%% @hidden
init_per_suite(Config) ->
    {ok, client_started} =
        rct_proxy:start_proxy(node1, avli1, ?AVLI),
    Config.

%% @hidden
end_per_suite(_Config) ->
    timer:sleep(2000),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, avli1),
    ok = rct_proxy:exit_master(node1).

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
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade_short__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []},
     %% This suite can be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     %% avli_something
     %% avli_normal_empty
     avli_normal
     %% avli_fail
    ].


avli_fail(_Config) ->
    {0, 0, 0} = os:timestamp(),
    notOk.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do something.
%% @spec avli_something(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
avli_something(_Config) ->
    {error, "unknown function code", ?NONEXISTENT_FUNCTION_CODE} =
	rct_proxy:send_proxy(node1, avli1, ?NONEXISTENT_FUNCTION_CODE, {}),

%    {ok} =
%	rct_proxy:send_proxy(node1, avli1, ?AVLI_INITIATE_MEMORY, {}),


    ok.


avli_normal_empty(_Config) ->
    prelude(?CLIENT_1),
    postlude(?CLIENT_1),
    ok.

avli_normal(_Config) ->
    prelude(?CLIENT_1),

    {ok, ?CELLO_AVLI_SUCCESS} =
	writeNodeEvent(
	  ?CLIENT_1,
	  ?CELLO_AVLI_TIME_BY_AVLI,
	  ?CELLO_AVLI_OUT_OF_SERVICE,
	  ?CELLO_AVLI_UNOPERATIONAL,
	  0,   % EventId
	  "<AppLog><RestartCompleted/><Cause>traffic enabled</Cause></AppLog>"),

    {ok, {ok, ?CELLO_AVLI_WRITE_CFM}} =
	waitForWriteCfm(),

    {ok, ?CELLO_AVLI_SUCCESS} =
	writePiuEvent(
	  ?CLIENT_1,
	  ?CELLO_AVLI_TIME_BY_AVLI,
	  ?CELLO_AVLI_IN_SERVICE,
	  ?CELLO_AVLI_OPERATIONAL,
	  ?CELLO_AVLI_MP,
	  {1,2,3},
	  {"CXC1320784CXC1320784abcd", "R73D54", "", "", ""},
	  "<AppLog><PiuInfo>just Moore info.\rRoger\nThat</PiuInfo></AppLog>"),

    {ok, {ok, ?CELLO_AVLI_WRITE_CFM}} =
	waitForWriteCfm(),

    {ok, ?CELLO_AVLI_SUCCESS} =
	writeHwEvent(
	  ?CLIENT_1,
	  ?CELLO_AVLI_TIME_BY_AVLI,
	  ?CELLO_AVLI_IN_SERVICE,
	  ?CELLO_AVLI_OPERATIONAL,
	  "webcam",
	  "11,22,33",
	  {"CXC1320784CXC1320784wxyz", "abcdefg", "xx", "yy", "zz"},
	  "<AppLog><Cause>2x2 pixels</Cause></AppLog>"),

    {ok, {ok, ?CELLO_AVLI_WRITE_CFM}} =
	waitForWriteCfm(),

    {ok, ?CELLO_AVLI_SUCCESS} =
    	writeHw5Event(
    	  ?CLIENT_1,
    	  ?CELLO_AVLI_TIME_BY_AVLI,
    	  ?CELLO_AVLI_IN_SERVICE,
    	  ?CELLO_AVLI_OPERATIONAL,
    	  "webcam",
    	  "11,22,33",
    	  {"KDU123456/123", "R1A99", "12345678901234567890123456789012", "20160905", "ABC12345678"},
    	  "<AppLog><Cause>writeHw5Event</Cause></AppLog>"),

    {ok, {ok, ?CELLO_AVLI_WRITE_CFM}} =
    	waitForWriteCfm(),

    ServiceInstance100 =
	"from out of the blue" ++
	"=========3=========4=========5=========6" ++
	"=========7=========8=========9=======99",
    {ok, ?CELLO_AVLI_SUCCESS} =
	writeServiceEvent(
	  ?CLIENT_1,
	  ?CELLO_AVLI_TIME_BY_AVLI,
	  ?CELLO_AVLI_IN_SERVICE,
	  ?CELLO_AVLI_OPERATIONAL,
	  "live webcast",
	  ServiceInstance100,
	  "<AppLog><NodeInfo>6 bit/s</NodeInfo></AppLog>"),

    {ok, {ok, ?CELLO_AVLI_WRITE_CFM}} =
	waitForWriteCfm(),

    {ok, ?CELLO_AVLI_SUCCESS} =
	writeOtherEvent(
	  ?CLIENT_1,
	  ?CELLO_AVLI_TIME_BY_AVLI,
	  ?CELLO_AVLI_IN_SERVICE,
	  ?CELLO_AVLI_OPERATIONAL,
	  "<AppLog><Cause>rain gauge is ready</Cause></AppLog>"),

    {ok, {ok, ?CELLO_AVLI_WRITE_CFM}} =
	waitForWriteCfm(),

    {ok, ?CELLO_AVLI_SUCCESS} =
	writePgmEvent(
	  ?CLIENT_1,
	  ?CELLO_AVLI_TIME_BY_AVLI,
	  ?CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE,
	  ?CELLO_AVLI_SHUTDOWN_COMMAND,
	  ?CELLO_AVLI_MP,
	  {1, 2, 3},
	  {"aaa", "bbb", "ccc", "ddd"},
	  "<AppLog><RankWarm/><Cause>zero hassle software</Cause></AppLog>"),

    {ok, {ok, ?CELLO_AVLI_WRITE_CFM}} =
	waitForWriteCfm(),

    postlude(?CLIENT_1),

    ok.






%%--------------------------------------------------------------------
%% LOCAL FUNCTIONS
%%--------------------------------------------------------------------


prelude(Client) ->

    {ok} =
	initiateMemory(Client),

    {ok, ?CELLO_AVLI_SUCCESS} =
	initiateService(Client),

    {ok, {ok, ?CELLO_AVLI_SERVER_UP_IND, ?CELLO_AVLI_SUCCESS}} =
	waitForServerUp(),

    {ok, {ok, ?CELLO_AVLI2_INITIATE_SERVICE_CFM, ?CELLO_AVLI_SUCCESS}} =
	waitForInitiateServiceCfm().


postlude(Client) ->
    {ok} = freeMemory(Client).


writeNodeEvent(Client, Timestamp, Status, Reason, EventId, Info) ->
    send(Client,
	 ?AVLI4_WRITE_NODE_EVENT,
	 [Timestamp, Status, Reason, EventId, encodeLength(Info)]).


writePiuEvent(Client, Timestamp, Status, Reason, PiuType, PiuHwAddr, HwPid, Info) ->
    send(Client,
	 ?AVLI2_WRITE_PIU_EVENT,
	 [Timestamp, Status, Reason, PiuType, PiuHwAddr, encodeLength(HwPid), encodeLength(Info)]).


writeHwEvent(Client, Timestamp, Status, Reason, HwType, HwAddress, HwPid, Info) ->
    send(Client,
	?AVLI2_WRITE_HW_EVENT,
	[Timestamp, Status, Reason, encodeLength(HwType), encodeLength(HwAddress), encodeLength(HwPid), encodeLength(Info)]).

writeHw5Event(Client, Timestamp, Status, Reason, HwType, HwAddress, HwPid, Info) ->
    send(Client,
	?AVLI5_WRITE_HW_EVENT,
	[Timestamp, Status, Reason, encodeLength(HwType), encodeLength(HwAddress), encodeLength(HwPid), encodeLength(Info)]).

writeServiceEvent(Client, Timestamp, Status, Reason, ServiceType, ServiceInstance, Info) ->
    send(Client,
	 ?AVLI2_WRITE_SERVICE_EVENT,
	 [Timestamp, Status, Reason, encodeLength(ServiceType), encodeLength(ServiceInstance), encodeLength(Info)]).

writeOtherEvent(Client, Timestamp, Status, Reason, AvailabilityInfo) ->
    send(Client,
	 ?AVLI2_WRITE_OTHER_EVENT,
	 [Timestamp, Status, Reason, encodeLength(AvailabilityInfo)]).

writePgmEvent(Client, Timestamp, Status, Reason, PiuType, PiuHwAddr, SwPid, Info) ->
    send(Client,
	 ?AVLI3_WRITE_PGM_EVENT,
	 [Timestamp, Status, Reason, PiuType, PiuHwAddr, encodeLength(SwPid), encodeLength(Info)]).


encodeLength(TupleOfStrings) when is_tuple(TupleOfStrings) ->
    L = tuple_to_list(TupleOfStrings),
    list_to_tuple([encodeLength(S) || S <- L]);

encodeLength(String) ->
    {length(String), String}.



initiateMemory(Client) ->
    send(Client, ?AVLI_INITIATE_MEMORY, []).

initiateService(Client) ->
    send(Client, ?AVLI2_INITIATE_SERVICE, []).


waitForServerUp() ->
    waitForIndication().


waitForInitiateServiceCfm() ->
    waitForIndication().

waitForWriteCfm() ->
    waitForIndication().


freeMemory(Client) ->
    send(Client, ?AVLI2_FREE_MEMORY, []).


send(Client, Function, Data) ->
    rct_proxy:send_proxy(node1, avli1, Function, list_to_tuple([Client|Data])).



waitForIndication() ->
    rct_proxy:receive_proxy().




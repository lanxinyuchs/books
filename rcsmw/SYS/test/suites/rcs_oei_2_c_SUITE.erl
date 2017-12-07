%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rcs_oei_2_c_SUITE.erl %
%%% @author elarrun
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/15
%%%
%%% @doc ==Basic Test Suite for the OEI interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(rcs_oei_2_c_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R11A/1     2017-08-31 elarrun     Created (by copying oei_c.SUITE.erl)
%%% ----------------------------------------------------------

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0
        ]).

-export([all/0,
	 rcsoei_getEventIdentity/1,
         rcsoei_getDerivedCorrelationUUID/1,
         rcsoei_getDerivedCorrelationUUID2/1,
         rcsoei_getDerivedCorrelationUUID_unspecifiedEventId/1,
         rcsoei_getDerivedCorrelationUUID2_unspecifiedEventId/1,
         rcsoei_getDerivedCorrelationUUID2_invalidUUID/1,
         rcsoei_getRcsOeiUUID/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include("test_rcs_oei_2.hrl").
-import(string, [str/2]).


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
suite() ->
    [{timetrap, {minutes, 300}},
     {ct_hooks, [{rct_rpc, rpc},
                 {rct_htmllink,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[
									       ]
                                              }}]}},
                 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
                 {rct_core,[]}
                ]}].

%% @hidden
init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, oei1, ?OEI),
    Config.

%% @hidden
end_per_suite(_Config) ->
    timer:sleep(2000),          %% TODO: delay, why?

    {ok, client_stopped} = rct_proxy:stop_proxy(node1, oei1),
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
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []},
     %% This suite can be run on both MPs within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]},
     {sbc__cluster__dual_sim_3__1__group, [], [{group, default__group}]}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     rcsoei_getEventIdentity,
     rcsoei_getDerivedCorrelationUUID,
     rcsoei_getDerivedCorrelationUUID2,
     rcsoei_getDerivedCorrelationUUID_unspecifiedEventId,
     rcsoei_getDerivedCorrelationUUID2_unspecifiedEventId,
     rcsoei_getDerivedCorrelationUUID2_invalidUUID,
     rcsoei_getRcsOeiUUID
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

rcsoei_getEventIdentity(_Config) ->
    %% No timeout value
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getEventIdentity,
			     {?RCS_OEI_NO_TIMEOUT_VALUE}) of
	{ok,
	 ?RCS_OEI_OK,
	 EventId1} when EventId1 =/= ?RCS_OEI_UNSPECIFIED_EVENT_ID ->
	    ok
    end,

    %% 100 ms timeout value
    Timeout = 100,
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getEventIdentity, {Timeout}) of
	{ok,
	 ?RCS_OEI_OK,
	 EventId2} when EventId2 =/= ?RCS_OEI_UNSPECIFIED_EVENT_ID , EventId2 =/= EventId1 ->
	    ok
    end,

    ok.


rcsoei_getDerivedCorrelationUUID(_Config) ->
    EventId = 12648430,
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getDerivedCorrelationUUID,
			     {EventId, ?RCS_OEI_NO_TIMEOUT_VALUE}) of
	{ok,
	 ?RCS_OEI_OK,
	 DerCorrUUID} ->
             checkCorrId(DerCorrUUID, "00c0", 5, "ffee", 10)
    end.


rcsoei_getDerivedCorrelationUUID2(_Config) ->
    CorrelationId = "01234567-89ab-cdef-0123-456789abcdef",
    EventId = 2119674654,
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getDerivedCorrelationUUID2,
			     {EventId, ?RCS_OEI_NO_TIMEOUT_VALUE, CorrelationId}) of
	{ok,
	 ?RCS_OEI_OK,
	 DerCorrUUID} ->
            checkCorrId(DerCorrUUID, "7e57", 5, "ab1e", 10)
    end.

rcsoei_getDerivedCorrelationUUID_unspecifiedEventId(_Config) ->
    EventId = ?RCS_OEI_UNSPECIFIED_EVENT_ID,
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getDerivedCorrelationUUID,
			     {EventId, ?RCS_OEI_NO_TIMEOUT_VALUE}) of
	{ok,
	 ?RCS_OEI_OK,
         ?RCS_OEI_UNSPECIFIED_CORR_ID} ->
            ok
    end.

rcsoei_getDerivedCorrelationUUID2_unspecifiedEventId(_Config) ->
    CorrelationId = "01234567-89ab-cdef-0123-456789abcdef",
    EventId = ?RCS_OEI_UNSPECIFIED_EVENT_ID,
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getDerivedCorrelationUUID2,
			     {EventId, ?RCS_OEI_NO_TIMEOUT_VALUE, CorrelationId}) of
	{ok,
	 ?RCS_OEI_OK,
         ?RCS_OEI_UNSPECIFIED_CORR_ID} ->
            ok
    end.

rcsoei_getDerivedCorrelationUUID2_invalidUUID(_Config) ->
    % one char to short
    CorrelationId = "01234567-89ab-cdef-0123-456789abcde",
    EventId = 2119674654,
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getDerivedCorrelationUUID2,
			     {EventId, ?RCS_OEI_NO_TIMEOUT_VALUE, CorrelationId}) of
	{ok,
	 ?RCS_OEI_INVALID_UUID_FORMAT,
	 []} ->
            ok
    end.

rcsoei_getRcsOeiUUID(_Config) ->
    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getRcsOeiUUID,
			     {}) of
	{ok,
	 ?RCS_OEI_OK,
	 UUID1} ->
	    case rct_proxy:send_proxy(node1, oei1, ?RcsOei_getRcsOeiUUID,
				     {}) of
     	        {ok,
	         ?RCS_OEI_OK,
	         UUID2} when UUID2 =/= UUID1 ->
		    ok
            end
    end.

checkCorrId(InString, Expected1, AtIndex1, Expected2, AtIndex2) ->
    %ct:pal("InString=~s Expected1=~s AtIndex1=~w Expected2=~s AtIndex2=~w~n", [InString, Expected1, AtIndex1, Expected2, AtIndex2]),

    InStringLen = string:len(InString),
    case str(InString, Expected1) of 
        AtIndex1 when InStringLen == ?RCS_OEI_UUID_LEN -> ok
    end,
    case str(InString, Expected2) of
        AtIndex2 -> ok
    end.


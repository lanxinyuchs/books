%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oei_c_SUITE.erl %
%%% @author elarrun
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R8A/R11A/1
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

-module(oei_c_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R2A/2      2013-02-29 etxkols     Added rct_core hook
%%% R3A/1      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-10-06 etxpejn     Added cluster sim groups
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
	 oei_getEventIdentity/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include("test_oei.hrl").

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
    {ok, memory_initiated} =
        rct_proxy:send_proxy(node1, oei1, ?Oei_initiateMemory),
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
     oei_getEventIdentity
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

oei_getEventIdentity(_Config) ->
    %% No timeout value
    case rct_proxy:send_proxy(node1, oei1, ?Oei_getEventIdentity,
			     {?CELLO_OEI_NO_TIMEOUT_VALUE}) of
	{ok,
	 ?CELLO_OEI_OK,
	 EventId1} when EventId1 =/= ?CELLO_OEI_UNSPECIFIED_EVENT_ID ->
	    ok
    end,

    %% 100 ms timeout value
    Timeout = 100,
    case rct_proxy:send_proxy(node1, oei1, ?Oei_getEventIdentity, {Timeout}) of
	{ok,
	 ?CELLO_OEI_OK,
	 EventId2} when EventId2 =/= ?CELLO_OEI_UNSPECIFIED_EVENT_ID , EventId2 =/= EventId1 ->
	    ok
    end,

    ok.

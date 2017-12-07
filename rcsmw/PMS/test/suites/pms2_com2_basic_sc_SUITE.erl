%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_com2_basic_sc_SUITE.erl %
%%% @version /main/R3A/R4A/R5A/R6A/R9A/2

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% 
%%% @end

-module(pms2_com2_basic_sc_SUITE).
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-10-21 uabesvi     Copied from R2
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0]).


-export([cli_simple/1]).
-export([cli_v/1]).
-export([cli_counter/1]).
-export([cli_minus_one/1]).
-export([cli_minus_two/1]).
-export([cli_minus_two_horror_string/1]).
-export([cli_minus_one_multi/1]).
-export([cli_minus_two_multi/1]).
-export([one_app_three_counters/1]).
-export([one_counter_two_app/1]).
-export([two_counter_two_app/1]).
-export([one_counter_two_app_multi_counters/1]).
-export([two_app_multi_val_len_err/1]).


-define(LIB, pms2_test_lib).

-define(L2B(__L), list_to_binary(__L)).

-define(LOW,    1).
-define(MEDIUM, 2).
-define(HIGH,   3).

-define(ACTIVE, 1).
-define(STOPPED, 2).

-define(TEN_SECONDS, 1).
-define(THIRTY_SECONDS, 2).
-define(ONE_MIN, 3).
-define(FIVE_MIN, 4).
-define(FIFTEEN_MIN, 5).
-define(THIRTY_MIN, 6).
-define(ONE_HOUR, 7).
-define(TWELVE_HOUR, 8).
-define(ONE_DAY, 9).

-define(LDN_TN,  ["ManagedElement=1,Transport=1"]).

-define(PM_GRP_1,  ["Group1"]).
-define(PM_GRP_2,  ["Group2"]).
-define(PM_GRP_12, ["Group1", "Group2"]).


-define(MR_NV, [{<<"currentValue">>,    {9, <<"curry">>}},
		{<<"lastUpdated">>,     {12, "2000-03-01T14:00:00+02:00"}},
		{<<"moClassInstance">>, {11, 27002}},
		{<<"suspectFlag">>,     {10, false}}]).

-define(MR_SPEC(__Grp), 
	[{<<"groupRef">>, 
	  {11, ?L2B("ManagedElement=1,"
		    "SystemFunctions=1,"
		    "Pm=1,"
		    "PmGroup=" ++ __Grp)}},
	 {<<"measurementTypeRef">>, 
	  {11, ?L2B("")}}]).

-define(MR_SPEC(__Grp, __MT), 
	[{<<"groupRef">>, 
	  {11, ?L2B("ManagedElement=1,"
		    "SystemFunctions=1,"
		    "Pm=1,"
		    "PmGroup=" ++ __Grp)}},
	 {<<"measurementTypeRef">>, 
	  {11, ?L2B("ManagedElement=1,"
		    "SystemFunctions=1,"
		    "Pm=1,"
		    "PmGroup=" ++ __Grp ++ "," ++
		    "MeasurementType=" ++ __MT)}}]).

-define(TESTNODE, testnode).

-define(TS_TO, 2500).
-define(SLEEP,  500).

-define(SUNE,   sune).
-define(STINA,  stina).
-define(BERIT,  berit).

-define(TEST_APPS,  [?SUNE]).
-define(TEST_APPS2, [?SUNE, ?STINA, ?BERIT]).


-define(COMTOP,   "urn:com:ericsson:ecim:ComTop").
-define(ECIM_PM,  "urn:com:ericsson:ecim:ECIM_PM").
-define(SYS_FNCS, "urn:com:ericsson:ecim:SYS_FNCS").
-define(PM,       "urn:com:ericsson:ecim:ECIM_PM").
-define(PM_JOB,   "urn:com:ericsson:ecim:PM_JOB").
-define(PM_MR,    "urn:com:ericsson:ecim:PM_MR").

-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
		  {'nc:operation', "delete"}]).


-define(SC_OK,             0).
-define(SC_NO_COUNTERS,    1).
-define(SC_INTERNAL_ERROR, 2).

-define(SC_COM_NO_COUNTERS,    not_exist).
-define(SC_COM_INTERNAL_ERROR, failure).

-define(INT64, 4).
-define(INT64(Int), {?INT64, Int}).




%%=========================================================
%% cli macros
%%=========================================================
-define(CLI_USER, cli_user).

-define(CONFIGURE, "configure").
-define(COMMIT,    "commit").
-define(TOP,       "top").

-define(PRINT_OPT, print).

-define(DEF_SC_MOS, "ManagedElement=1,Erat=1,Elizabeth=1").

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    CliHook = [{rct_cli, {?CLI_USER, [manual_connect]}}],
    ProxyTag = 
	{pms_pmi_proxy, get_ct_config(pms_pmi_proxy, [{erl_api, false}])},
    Hooks    = lists:keystore(pms_pmi_proxy, 1, hooks(), ProxyTag),
    [{ct_hooks, Hooks ++ CliHook}].
	     

%% @hidden
init_per_suite(Config) ->
    %%log_msg(),
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    [{reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    %%rpc(pmsDebug, stop_clear, []),
    ok.

%% @hidden
init_per_group(_Group, Config) ->
    Config.

%% @hidden
end_per_group(_Group, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("~n ########## TC:~p ##########~n~n", [TestCase]),
    cleanup(),
    Config.

%% @hidden
end_per_testcase(session_kill_app_proc, _Config) ->
    cleanup(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    cleanup(),
    close_trans(),
    [catch pms_erl_app:stop(App) || App <- ?TEST_APPS],
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     cli_simple,
     cli_v,
     cli_counter,
     cli_minus_one,
     cli_minus_two,
     cli_minus_two_horror_string,
     cli_minus_one_multi,
     cli_minus_two_multi,
     one_app_three_counters,
     one_counter_two_app,
     two_counter_two_app,
     one_counter_two_app_multi_counters,
     two_app_multi_val_len_err
    ].



groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__sim__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__sim__1__group, [], [{group, default__group}]},  
     {xl__def__all__1__group, [], [{group, default__group}]},  
     {xl__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []} 
    ].


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------


%%========================================================================
%% cli_simple(Config) -> ok.
%% 
%% @doc 
%% show counters simplest case
%% @end
%%========================================================================
cli_simple(_Config) ->
    Val = 47,
    App = ?SUNE,
    {ok, Handle} = start_app(App, Val),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result("ROPType6Sum=" ++ i2l(Val), Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).


%%========================================================================
%% cli_v(Config) -> ok.
%% 
%% @doc 
%% show counters simplest case
%% @end
%%========================================================================
cli_v(_Config) ->
    Val = 1,
    App = ?SUNE,
    {ok, Handle} = start_app(App, Val),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters -v", ?PRINT_OPT),
    ok  = check_result("ROPType6Sum=" ++ i2l(Val), Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).


%%========================================================================
%% cli_counter(Config) -> ok.
%% 
%% @doc 
%% show counters simplest case
%% @end
%%========================================================================
cli_counter(_Config) ->
    Val = 12,
    App = ?SUNE,
    GrpAlias = 4,
    GroupAliases = [{"ROPGroup2", GrpAlias, [{"ROPType6Sum", 41}]}],
    ExpSpec = [{4, [41]}],
    {ok, Handle} = start_app(App, [Val], ?SC_OK, "", GroupAliases, ExpSpec),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters -c ROPType6Sum", ?PRINT_OPT),
    ok  = check_result("ROPType6Sum=" ++ i2l(Val), Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).


%%========================================================================
%% cli_minus_one(Config) -> ok.
%% 
%% @doc 
%% show counters return -1
%% @end
%%========================================================================
cli_minus_one(_Config) ->
    Val = -1,
    App = ?SUNE,
    {ok, Handle} = start_app(App, Val, ?SC_OK, "hej hopp cli_minus_one"),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum=", i2l(Val), "suspect"], Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).


%%========================================================================
%% cli_minus_two(Config) -> ok.
%% 
%% @doc 
%% show counters return -2
%% @end
%%========================================================================
cli_minus_two(_Config) ->
    Val = -2,
    App = ?SUNE,
    {ok, Handle} = start_app(App, Val, ?SC_OK, ""),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum=", i2l(Val), "suspect"], Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).


%%========================================================================
%% cli_minus_two_horror_string(Config) -> ok.
%% 
%% @doc 
%% show counters return -2 and an error string
%% @end
%%========================================================================
cli_minus_two_horror_string(_Config) ->
    Val = -2,
    App = ?SUNE,
    {ok, Handle} = start_app(App, Val, ?SC_OK, "hej hopp horror msg"),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum=", i2l(Val), "suspect"], Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).

%%========================================================================
%% cli_minus_one_multi(Config) -> ok.
%% 
%% @doc 
%% show counters return -1
%% @end
%%========================================================================
cli_minus_one_multi(_Config) ->
    Val = [22, -1, 33],
    App = ?SUNE,
    {ok, Handle} = start_app(App, Val, ?SC_OK, "hej hopp cli_minus_two"),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum",
			i2l(22),
			"suspect",
			i2l(-1), 
			"suspect",
			i2l(33), 
			"suspect"],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok = delete_app(App, Handle).

    
%%========================================================================
%% cli_minus_two_multi(Config) -> ok.
%% 
%% @doc 
%% show counters return -2
%% @end
%%========================================================================
cli_minus_two_multi(_Config) ->
    Val = [44, -2, 55],
    App = ?SUNE,
    {ok, Handle} = start_app(App, Val, ?SC_OK, "hej hopp cli_minus_two"),
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum", i2l(44), i2l(-2), "suspect", i2l(55)],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).







%%========================================================================
%% one_app_three_counters(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
one_app_three_counters(_Config) ->
    
    App = ?SUNE,

    R1 = 13,
    R2 = 213,
    R3 = 3123,

    GrpAlias = 4,
    Counter1 = 41,
    Counter2 = 42,
    Counter3 = 43,
    Groups = [{"ROPGroup2", GrpAlias, [{"ROPType6Sum", Counter1},
				       {"ROPType7Avg", Counter2},
				       {"ROPType8Min", Counter3},
				       {"ROPType9Max", 44}, 
				       {"ROPType10LU", 45}]}],
    {ok, Handle} = create_app(App, Groups),

    {ok, Aliases} = ldn_aliases(App, [?DEF_SC_MOS]),
    LdnAlias = proplists:get_value(?DEF_SC_MOS, Aliases),

    Expected = [{pmi2ReportShowCounters, 
		 {repeat, 1},
		 [{values, 
		   [{GrpAlias, [{LdnAlias, [{Counter1, [R1]}]}]},
		    {GrpAlias, [{LdnAlias, [{Counter2, [R2]}]}]},
		    {GrpAlias, [{LdnAlias, [{Counter3, [R3]}]}]}]}
		 ]}],
    
    {ok, _Ref} = expected(App, Handle, Expected),

    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum", i2l(13), 
			"ROPType7Avg", i2l(213), 
			"ROPType8Min", i2l(3123)
		       ],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App, Handle).



%%========================================================================
%% one_counter_two_app(Config) -> ok.
%% 
%% @doc 
%% show counters when two applications have same counters
%% @end
%%========================================================================
one_counter_two_app(_Config) ->

    App1 = ?SUNE,
    {ok, Handle1} = start_app(App1, 198),
    App2 = ?BERIT,
    {ok, Handle2} = start_app(App2, 298),


    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum=496"], Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App1, Handle1),
    ok  = delete_app(App2, Handle2).
    
    
%%========================================================================
%% two_counter_two_app(Config) -> ok.
%% 
%% @doc 
%% show counters when two applications have same counters
%% @end
%%========================================================================
two_counter_two_app(_Config) ->

    App1 = ?SUNE,
    App2 = ?BERIT,

    GrpAlias = 4,
    Counter1 = 41,
    Counter2 = 42,
    Counter3 = 43,
    Groups = [{"ROPGroup2", GrpAlias, [{"ROPType6Sum", Counter1},
				       {"ROPType7Avg", Counter2},
				       {"ROPType8Min", Counter3},
				       {"ROPType9Max", 44}, 
				       {"ROPType10LU", 45}]}],
    {ok, Handle1} = create_app(App1, Groups),
    {ok, Handle2} = create_app(App2, Groups),

    {ok, Aliases} = ldn_aliases(App1, [?DEF_SC_MOS]),
    LdnAlias = proplists:get_value(?DEF_SC_MOS, Aliases),

    Expected1 = [{pmi2ReportShowCounters, 
		  {repeat, 1},
		  [{values, 
		    [{GrpAlias, [{LdnAlias, [{Counter1, [13]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter2, [44]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter3, [103]}]}]}]}
		  ]}],
    Expected2 = [{pmi2ReportShowCounters, 
		  {repeat, 1},
		  [{values, 
		    [{GrpAlias, [{LdnAlias, [{Counter1, [31]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter2, [22]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter3, [104]}]}]}]}
		  ]}],
    
    {ok, _Ref1} = expected(App1, Handle1, Expected1),
    {ok, _Ref2} = expected(App2, Handle2, Expected2),


    %% NOTE, the result should be merged!!!


    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType6Sum=44", "ROPType7Avg=33", "ROPType8Min=103"],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App1, Handle1),
    ok  = delete_app(App2, Handle2).
    

%%========================================================================
%% one_counter_two_app_multi_counters(Config) -> ok.
%% 
%% @doc 
%% show counters when an app is started
%% @end
%%========================================================================
one_counter_two_app_multi_counters(_Config) ->
    
    Ldn = "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1",

    App1 = ?SUNE,
    App2 = ?BERIT,

    GrpAlias = 5,
    Counter1 = 51,
    Counter2 = 52,
    Counter3 = 53,
    Groups = [{"ROPGroupMult", GrpAlias, [{"ROPType11SumMult", Counter1},
					  {"ROPType12AvgMult", Counter2},
					  {"ROPType13MinMult", Counter3},
					  {"ROPType14MaxMult", 54}, 
					  {"ROPType15LUMult",  55}]}],
    {ok, Handle1} = create_app(App1, Groups),
    {ok, Handle2} = create_app(App2, Groups),

    {ok, Aliases} = ldn_aliases(App1, [Ldn]),
    LdnAlias = proplists:get_value(Ldn, Aliases),

    Expected1 = [{pmi2ReportShowCounters, 
		  {repeat, 1},
		  [{values, 
		    [{GrpAlias, [{LdnAlias, [{Counter1, [1,  11]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter2, [10, 20, 30]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter3, [23, 34, 55, 66]}]}]}]}
		  ]}],
    Expected2 = [{pmi2ReportShowCounters, 
		  {repeat, 1},
		  [{values, 
		    [{GrpAlias, [{LdnAlias, [{Counter1, [2,  22]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter2, [12, 22, 32]}]}]},
		     {GrpAlias, [{LdnAlias, [{Counter3, [45, 11, 44, 77]}]}]}]}
		  ]}],
    
    {ok, _Ref1} = expected(App1, Handle1, Expected1),
    {ok, _Ref2} = expected(App2, Handle2, Expected2),


    %% NOTE, the result should be merged!!!


    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1", 
		 ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result(["ROPType11SumMult", "3", "33", 
			"ROPType12AvgMult", "11", "21", "31",
			"ROPType13MinMult", "23", "11", "44", "66" ],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App1, Handle1),
    ok  = delete_app(App2, Handle2).
    

%%========================================================================
%% two_app_multi_val_len_err(Config) -> ok.
%% 
%% @doc 
%% show counters: 2 apps, multivalue counter
%% @end
%%========================================================================
two_app_multi_val_len_err(_Config) ->
    
    Ldn = "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1",

    App1 = ?SUNE,
    App2 = ?BERIT,

    GrpAlias = 5,
    Counter1 = 51,
    Counter2 = 52,
    Counter3 = 53,
    Groups = [{"ROPGroupMult", GrpAlias, [{"ROPType11SumMult", Counter1},
					  {"ROPType12AvgMult", Counter2},
					  {"ROPType13MinMult", Counter3},
					  {"ROPType14MaxMult", 54}, 
					  {"ROPType15LUMult",  55}]}],
    {ok, Handle1} = create_app(App1, Groups),
    {ok, Handle2} = create_app(App2, Groups),

    {ok, Aliases} = ldn_aliases(App1, [Ldn]),
    LdnAlias = proplists:get_value(Ldn, Aliases),

    Expected1 = [{pmi2ReportShowCounters, 
		  {repeat, 1},
		  [{values, 
		    [{GrpAlias, [{LdnAlias, [{Counter1, [1, 11]}]}]}]}]}],
    Expected2 = [{pmi2ReportShowCounters, 
		  {repeat, 1},
		  [{values, 
		    [{GrpAlias, [{LdnAlias, [{Counter1, [2,  22, 33]}]}]}]}]}],
    
    {ok, _Ref1} = expected(App1, Handle1, Expected1),
    {ok, _Ref2} = expected(App2, Handle2, Expected2),


    %% NOTE, the result should be merged!!!


    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1", 
		 ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters -v", ?PRINT_OPT),
    ok  = check_result(["ROPType11SumMult", "2", "suspect", "22", "suspect"],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app(App1, Handle1),
    ok  = delete_app(App2, Handle2).
    


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%=====================================================================
%% start app
%%=====================================================================
start_app(App, Value) ->
    start_app(App, Value, ?SC_OK).

start_app(App, Value, Result) ->
    start_app(App, Value, Result, "").


start_app(App, Value, Result, ErrorStr) when is_integer(Value) ->
    start_app(App, [Value], Result, ErrorStr);
start_app(App, Value, Result, ErrorStr) ->
    GrpAlias = 4,
    CntAlias = 41,
    GroupsAliases = [{"ROPGroup2", GrpAlias, [{"ROPType6Sum", CntAlias},
					      {"ROPType7Avg", 42},
					      {"ROPType8Min", 43},
					      {"ROPType9Max", 44}, 
					      {"ROPType10LU", 45}]}],
    ExpSpec = [{GrpAlias, [CntAlias, 42, 43, 44, 45]}],
    start_app(App, Value, Result, ErrorStr, GroupsAliases, ExpSpec).


start_app(App, Value, Result, ErrorStr, GroupsAliases, ExpSpec) ->
    GrpAlias = 4,
    CntAlias = 41,
    {ok, Handle} = create_app(App, GroupsAliases),
    {ok, Aliases} = ldn_aliases(App, [?DEF_SC_MOS]),
    LdnAlias = proplists:get_value(?DEF_SC_MOS, Aliases),
    Spec = [{spec, ExpSpec} || [_|_] <- [ExpSpec]],
    Expected = [{pmi2ReportShowCounters, 
		 {repeat, 1},
		 Spec ++ 
		     [{values, [{GrpAlias, [{LdnAlias, [{CntAlias, Value}]}]}]},
		      {result,     Result},
		      {horror_str, ErrorStr}]}], %% Kalle against error
    
    {ok, _Ref} = expected(App, Handle, Expected),
    {ok, Handle}.


%%=====================================================================
%% wait until the expected string is received from CLI
%%=====================================================================
check_result([H | _] = Expected, Received) when is_list(H) ->
    cr(Expected, Received);
check_result(Expected, Received) ->
    check_result([Expected], Received).


cr([], {ok, _})->
    ok;
cr([], Error)->
    Error;
cr([Expected | T], Received)->
    Cont = cr_loop(Expected, Received, 10),
    cr(T, Cont).



cr_loop(Expected, {ok, Received}, N) when N > 0 ->
    Rcvd = re:replace(Received, "[<>]", "", [{return, list},global]),
    ct:pal("### Expected cr loop  ~p~n"
	   "### Received  ~p~n", [Expected, Rcvd]),
    case string:str(Rcvd, Expected) of
	0 ->
	    ct:pal("#### 00000~n"),
	    timer:sleep(1000),
	    Rec = rct_cli:send(?CLI_USER, ?TOP, ?PRINT_OPT),
	    cr_loop(Expected, Rec, N - 1);
	X ->
	    ct:pal("#### ~p~n ", [X]),
	    {ok, string:substr(Received, X)}
    end;
cr_loop(_, {ok, _}, _) ->
    {error, not_received};
cr_loop(_, Error, _) ->
    {error, Error}.



i2l(Int) ->
    integer_to_list(Int).


%% gp(?TEN_SECONDS)    -> 10;
%% gp(?THIRTY_SECONDS) -> 30.

%% to() ->
%%     to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


%% wait_table_size(Tab) ->
%%     wait_table_size(Tab, 0).

%% wait_table_size(Tab, L) ->
%%     wait_table_size(Tab, L, 30).

%% wait_table_size(Tab, L, N) when N >= 0 ->
%%     case get_table(Tab) of
%% 	Elems when length(Elems) =:= L ->
%% 	    ok;
%% 	_ when N > 0 ->
%% 	    timer:sleep(?SLEEP),
%% 	    wait_table_size(Tab, L, N - 1);
%% 	Elems ->
%% 	    {error, {badlength, L, Elems}}
%%     end.


get_ct_config(TC, Default) -> ?LIB:get_ct_config(TC, Default).


%% start(A)    -> {ok, _} = pms_erl_app:start(A).
%% start(A, B) -> {ok, _} = pms_erl_app:start(A, B).
%% stop(A)     -> ok = pms_erl_app:stop(A).   


%%create_app(A)    -> ?LIB:create_app(A).
create_app(A, B) -> ?LIB:create_app(A, B).
delete_app(A, B) -> ?LIB:delete_app(A, B).
%%create_job(A, B) -> ?LIB:create_job(A, B).
%%create_jobs(A)   -> ?LIB:create_jobs(A).
%%delete_jobs(A)   -> ?LIB:delete_jobs(A).
%%update_job(A, B) -> ?LIB:update_job(A, B).
%%delete_job(A)    -> ?LIB:delete_job(A).


expected(A, B, C) -> ?LIB:expected(A, B, C).

ldn_aliases(A, B) -> ?LIB:ldn_aliases(A, B).


%% initialize(A)      -> ok = ?LIB:initialize(A).
%% initialize(A, B)   -> ok = ?LIB:initialize(A, B).
%% initialize_2(A, B) -> ok = ?LIB:initialize_2(A, B).
%% finalize(A)        -> ok = ?LIB:finalize(A).
%% kill(A)            -> ?LIB:kill(A).
%% get_table(A)       -> ?LIB:get_table(A).
%% tables()           -> ?LIB:tables().
%% check_tables(A)    -> ?LIB:check_tables(A).
%% check_processes(A) -> ?LIB:check_processes(A).
cleanup()          -> ?LIB:cleanup().
%% log_msg()          -> ?LIB:log_msg().
rpc(A, B, C)       -> ?LIB:rpc(A, B, C).

%% wait_table_size(A, B)    -> ?LIB:wait_table_size(A, B).
%% wait_table_size(A, B, C) -> ?LIB:wait_table_size(A, B, C).
%% wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
%% wait_subscribe(A, B)     -> ?LIB:wait_subscribe(A, B).
%% wait_subscribe(A, B, C)  -> ?LIB:wait_subscribe(A, B, C).
%% wait_report()            -> ?LIB:wait_report().
%% wait_report(A)           -> ?LIB:wait_report(A).
%% wait_report(A, B)        -> ?LIB:wait_report(A, B).
%% wait_for_report(A, B, C) -> ?LIB:wait_for_report(A, B, C).
%% wait_report_sc(A, B)     -> ?LIB:wait_report_sc(A, B).

%%create_job(A)       -> ?LIB:create_job(A).
%% update_job(A, B)    -> ?LIB:update_job(A, B).
%% delete_job(A)       -> ?LIB:delete_job(A).
%% delete_mr(A, B)     -> ?LIB:delete_mr(A, B).
%% create_job_mr(A, B) -> ?LIB:create_job_mr(A, B).

%% create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).

%% host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%% open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.




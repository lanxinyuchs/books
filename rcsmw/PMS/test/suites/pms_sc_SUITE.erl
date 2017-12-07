%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_sc_SUITE.erl %
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/6

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% 
%%% @end

-module(pms_sc_SUITE).
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-04-18 uabesvi     Created
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% R9A/3      2017-03-21 etxjovp     Adapt to SPI2 towards COM
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
-export([cli_minus_one_multi/1]).
-export([cli_minus_two_multi/1]).
-export([one_job_one_mr/1]).
-export([one_job_two_counters/1]).
-export([one_job_internal_err/1]).
-export([one_job_no_app/1]).
-export([one_job_no_counters/1]).
-export([one_job_two_app/1]).
-export([one_job_two_app_multi_counters/1]).
-export([two_app_multi_val/1]).
-export([two_app_multi_val_len_err/1]).
-export([no_counters/1]).
-export([minus_one/1]).
-export([minus_two/1]).
-export([minus_one_multi/1]).
-export([minus_two_multi/1]).
-export([ignore_request/1]).
-export([one_counter_late_reply/1]).
-export([sc_init/3]).

-define(LIB, pms_test_lib).

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
-define(LDN_ADDR_IPV4, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1").
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

-define(SINGLE_VAL_CNT, 
	[<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ]).

-define(MULTI_VAL_CNT, [<<"PmFlexROPType11SumMult">>,
			<<"PmFlexROPType12AvgMult">>,
			<<"PmFlexROPType13MinMult">>,
			<<"PmFlexROPType14MaxMult">>,
			<<"PmFlexROPType15LUMult">>,
			<<"ROPType11SumMult">>, 
			<<"ROPType12AvgMult">>, 
			<<"ROPType13MinMult">>, 
			<<"ROPType14MaxMult">>, 
			<<"ROPType15LUMult">>]).


%%=========================================================
%% cli macros
%%=========================================================
-define(CLI_USER, cli_user).

-define(CONFIGURE, "configure").
-define(COMMIT, "commit").
-define(TOP, "top").

-define(PRINT_OPT, print).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    CliHook = [{rct_cli, {?CLI_USER, [manual_connect]}}],
    
    case hooks() of
	[] ->
	    [{ct_hooks, CliHook}];
	[{ct_hooks, Hooks}] ->
	    [{ct_hooks, lists:append(Hooks, CliHook)}]
    end.
	     

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
    All = [
	   cli_simple,
	   cli_v,
	   cli_counter,
	   cli_minus_one,
	   cli_minus_two,
	   cli_minus_one_multi,
	   cli_minus_two_multi,
	   one_job_one_mr,
	   one_job_two_counters,
	   one_job_internal_err,
	   one_job_no_app,
	   one_job_no_counters,
	   one_job_two_app,
	   one_job_two_app_multi_counters,
	   two_app_multi_val,
	   two_app_multi_val_len_err,
	   no_counters,
	   minus_one,
	   minus_one_multi,
	   minus_two,
	   minus_two_multi,
	   ignore_request,
	   one_counter_late_reply
	  ],
    All ++ all_host(host()).

all_host(?TESTNODE) ->
    [];
all_host(_PmsSim) ->
    [].




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
    {ok, _} = cli_init(elizabeth),
    rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    cli_end(elizabeth, {?SC_OK, "", [{"ROPType6Sum", [1]}]}),
    rpc(ets, tab2list, [pmGroup]).


%%========================================================================
%% cli_v(Config) -> ok.
%% 
%% @doc 
%% show counters simplest case
%% @end
%%========================================================================
cli_v(_Config) ->
    {ok, _} = cli_init(elizabeth),    
    rct_cli:asend(?CLI_USER, "show-counters -v", ?PRINT_OPT),
    cli_end(elizabeth, {?SC_OK, "", [{"ROPType6Sum", [1]}]}).
    

%%========================================================================
%% cli_counter(Config) -> ok.
%% 
%% @doc 
%% show counters simplest case
%% @end
%%========================================================================
cli_counter(_Config) ->

    {ok, _} = cli_init(elizabeth),
    rct_cli:asend(?CLI_USER, "show-counters -c ROPType6Sum", ?PRINT_OPT),
    cli_end(elizabeth, {?SC_OK, "", [{"ROPType6Sum", [1]}]}).
    


%%========================================================================
%% cli_minus_one(Config) -> ok.
%% 
%% @doc 
%% show counters return -1
%% @end
%%========================================================================
cli_minus_one(_Config) ->

    {ok, _} = cli_init(elizabeth),
    rct_cli:asend(?CLI_USER, "show-counters -c ROPType6Sum", ?PRINT_OPT),
    cli_end(elizabeth, {?SC_OK, "", [{"ROPType6Sum", [-1]}]}, "ROPType6Sum=-1").
    
%%========================================================================
%% cli_minus_two(Config) -> ok.
%% 
%% @doc 
%% show counters return -2
%% @end
%%========================================================================
cli_minus_two(_Config) ->

    {ok, _} = cli_init(elizabeth),
    rct_cli:asend(?CLI_USER, "show-counters -c ROPType6Sum", ?PRINT_OPT),
    cli_end(elizabeth, {?SC_OK, "", [{"ROPType6Sum", [-2]}]}, "ROPType6Sum=-2").
    

%%========================================================================
%% cli_minus_one_multi(Config) -> ok.
%% 
%% @doc 
%% show counters return -1
%% @end
%%========================================================================
cli_minus_one_multi(_Config) ->

    {ok, _} = cli_init(elizabeth),
    rct_cli:asend(?CLI_USER, "show-counters -c ROPType6Sum", ?PRINT_OPT),
    cli_end(elizabeth,
	    {?SC_OK, "", [{"ROPType6Sum", [1, -1]}]},
	    "-1").
    
%%========================================================================
%% cli_minus_two_multi(Config) -> ok.
%% 
%% @doc 
%% show counters return -2
%% @end
%%========================================================================
cli_minus_two_multi(_Config) ->

    {ok, _} = cli_init(elizabeth),
    rct_cli:asend(?CLI_USER, "show-counters -c ROPType6Sum", ?PRINT_OPT),
    cli_end(elizabeth,
	    {?SC_OK, "", [{"ROPType6Sum", [1,-2]}]},
	    "-2").
    








cli_init(elizabeth) ->
    
    ok = rct_cli:connect(?CLI_USER),

    PmGrps = ["ROPGroup2"],
    TopLdn = "ManagedElement=1,Erat=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    rct_cli:send(?CLI_USER, "top", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT).


cli_end(elizabeth, Values) ->
    
    wait_report_sc(any, Values),    

    {ok, {Res, _}} = rct_cli:send(?CLI_USER, ?TOP, "ROPType6Sum=1"),
    PrRes = re:replace(Res, "[<>]", "", [{return, list},global]),
    ct:log("show-counters result: ~s~n", [PrRes]),

    finalize(?SUNE),
    stop(?SUNE),

    ok = rct_cli:disconnect(?CLI_USER).


cli_end(elizabeth, Values, Expected) ->
    
    wait_report_sc(any, Values),    

    {ok, {Res, _}} = rct_cli:send(?CLI_USER, ?TOP, Expected),
    PrRes = re:replace(Res, "[<>]", "", [{return, list},global]),
    ct:log("show-counters result: ~s~n", [PrRes]),

    finalize(?SUNE),
    stop(?SUNE),

    ok = rct_cli:disconnect(?CLI_USER).





%%========================================================================
%% one_job_one_mr(Config) -> ok.
%% 
%% @doc 
%% show counters when an app is started
%% @end
%%========================================================================
one_job_one_mr(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {?SC_OK, "", [{"Type2", [1]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    %% ResV = sc_res([{<<"Type2">>, [?INT64(1)]}]),
    ResV = sc_res_any([[{<<"Type2">>, [?INT64(1)]}],
		       [{<<"Type2">>, [{?INT64(1), false}], []}]]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.

%%========================================================================
%% one_job_two_counters(Config) -> ok.
%% 
%% @doc 
%% show counters when an app is started
%% @end
%%========================================================================
one_job_two_counters(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {?SC_OK, "", [{"Type2", [21]}, {"Type3", [22]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    %% ResV = sc_res([{<<"Type2">>,   [?INT64(21)]}, 
    %% 		   {<<"Type3">>, [?INT64(22)]}]),

    ResV = sc_res_any([[{<<"Type2">>,   [?INT64(21)]}, 
			{<<"Type3">>, [?INT64(22)]}],
		       [{<<"Type2">>,   [{?INT64(21), false}], []}, 
			{<<"Type3">>, [{?INT64(22), false}], []}]]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.

%%========================================================================
%% one_job_internal_err(Config) -> ok.
%% 
%% @doc 
%% show counters: return internal err
%% @end
%%========================================================================
one_job_internal_err(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    LDN    = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{LDN, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {?SC_INTERNAL_ERROR, "int err msg", [{"Type2", [1]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    %% ResV = sc_res({error, ?SC_COM_INTERNAL_ERROR}),
    ResV = sc_res_any([{error, ?SC_COM_INTERNAL_ERROR},
		       {error, ?SC_COM_INTERNAL_ERROR, <<"int err msg">>}]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% no_counters(Config) -> ok.
%% 
%% @doc 
%% show counters when an app is started
%% @end
%%========================================================================
no_counters(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {?SC_OK, "", []},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    ResV = sc_res([]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.

%%========================================================================
%% one_job_no_app(Config) -> ok.
%% 
%% @doc 
%% show counters when no app is started
%% @end
%%========================================================================
one_job_no_app(_Config) ->

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {error, ?SC_COM_NO_COUNTERS},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    ResV = sc_res(Values),
    
    %%to(2),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% one_job_no_counters(Config) -> ok.
%% 
%% @doc 
%% show counters when the MO does not have any counters
%% @end
%%========================================================================
one_job_no_counters(_Config) ->

    L = length(get_table(pmsScAppMoLdns)),

    %% Ldn    = "ManagedElement=1,Transport=1,Router=1,NoCounterMo=1",
    Ldn    = "ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1",
    Counters  = {error, ?SC_COM_NO_COUNTERS},
    Values = {error, ?SC_COM_NO_COUNTERS},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    ResV = sc_res(Values),
    
    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.

%%========================================================================
%% one_job_two_app(Config) -> ok.
%% 
%% @doc 
%% show counters when an app is started
%% @end
%%========================================================================
one_job_two_app(_Config) ->

    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?BERIT, [{ask, true}]),
    initialize_2(?BERIT, [{TopLdn, PmGrps}]),

    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, {?SC_OK, "", [{"Type2", [11]}]}),    
    wait_report_sc(any, {?SC_OK, "", [{"Type2", [22]}]}),    
    %% ResV = sc_res([{<<"Type2">>, [?INT64(33)]}]),
    ResV = sc_res_any([[{<<"Type2">>, [?INT64(33)]}],
		       [{<<"Type2">>, [{?INT64(33), false}], []}]]),
    
    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    finalize(?BERIT),
    stop(?BERIT),

    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% one_job_two_app_multi_counters(Config) -> ok.
%% 
%% @doc 
%% show counters when an app is started
%% @end
%%========================================================================
one_job_two_app_multi_counters(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?BERIT, [{ask, true}]),
    initialize_2(?BERIT, [{TopLdn, PmGrps}]),

    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, {?SC_OK, "", [{"Type2", [1]}, {"Type3", [3]}]}),    
    wait_report_sc(any, {?SC_OK, "", [{"Type2", [4]}]}),    
    %% ResV = sc_res([{<<"Type2">>,   [?INT64(5)]},
    %% 		   {<<"Type3">>, [?INT64(3)]}]),
    ResV = sc_res_any([[{<<"Type2">>,   [?INT64(5)]},
			{<<"Type3">>, [?INT64(3)]}],
		       [{<<"Type2">>,   [{?INT64(5), false}], []},
			{<<"Type3">>, [{?INT64(3), false}], []}]]),
    
    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    finalize(?BERIT),
    stop(?BERIT),

    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% two_app_multi_val(Config) -> ok.
%% 
%% @doc 
%% show counters: 2 apps, multivalue counter
%% @end
%%========================================================================
two_app_multi_val(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    %% PmGrps = ["Group1", "Group2"],
    %% TopLdn = "ManagedElement=1,Transport=1",
    PmGrps = ["ROPGroupMult"],
    TopLdn = "ManagedElement=1,Erat=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?BERIT, [{ask, true}]),
    initialize_2(?BERIT, [{TopLdn, PmGrps}]),

    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn = "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1",
    %% Ldn      = ?LDN_ADDR_IPV4,
    Counters = ?MULTI_VAL_CNT,
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, {?SC_OK, "", [{"ROPType12AvgMult", [1,2,4]}]}),    
    wait_report_sc(any, {?SC_OK, "", [{"ROPType12AvgMult", [3,4,8]}]}),    
    %% ResV = sc_res([[{<<"Type3">>, [?INT64(3), ?INT64(6), ?INT64(9)]}]),
    ResV = sc_res_any([[{<<"ROPType12AvgMult">>, 
			 [?INT64(2), ?INT64(3), ?INT64(6)]}],
		       [{<<"ROPType12AvgMult">>, 
			 [{?INT64(2), false}, 
			  {?INT64(3), false}, 
			  {?INT64(6), false}], []}]]),
    
    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    finalize(?BERIT),
    stop(?BERIT),

    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.



%%========================================================================
%% two_app_multi_val_len_err(Config) -> ok.
%% 
%% @doc 
%% show counters: 2 apps, multivalue counter
%% @end
%%========================================================================
two_app_multi_val_len_err(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    %% PmGrps = ["Group1", "Group2"],
    %% TopLdn = "ManagedElement=1,Transport=1",
    PmGrps = ["ROPGroupMult"],
    TopLdn = "ManagedElement=1,Erat=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?BERIT, [{ask, true}]),
    initialize_2(?BERIT, [{TopLdn, PmGrps}]),

    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    %% Ldn      = ?LDN_ADDR_IPV4,
    %% Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Ldn = "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1",
    Counters = ?MULTI_VAL_CNT,
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, {?SC_OK, "", [{"ROPType11SumMult", [1,2,3]}]}),    
    wait_report_sc(any, {?SC_OK, "", [{"ROPType11SumMult", [2,4]}]}),    
    %% ResV = sc_res({error, ?SC_COM_INTERNAL_ERROR}),
    ResV = sc_res_any([[{<<"ROPType11SumMult">>, 
			 [?INT64(1), ?INT64(2), ?INT64(3)]}],
		       [{<<"ROPType11SumMult">>, 
			 [{?INT64(1), true}, 
			  {?INT64(2), true}, 
			  {?INT64(3), true}], []}]]),
    
    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    finalize(?BERIT),
    stop(?BERIT),

    ok = wait_table_size(pmsScAppMoLdns, L),
    %% L = length(get_table(pmsScAppMoLdns)),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.



%%========================================================================
%% minus_one(Config) -> ok.
%% 
%% @doc 
%% send -1 as answer for sc
%% @end
%%========================================================================
minus_one(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {?SC_OK, "", [{"Type2", [-1]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    ResV = sc_res_any([[{<<"Type2">>, [?INT64(-1)]}],
		       [{<<"Type2">>, [{?INT64(-1), true}], []}]]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% minus_two(Config) -> ok.
%% 
%% @doc 
%% send -2 as answer for sc
%% @end
%%========================================================================
minus_two(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {?SC_OK, "", [{"Type2", [-2]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    ResV = sc_res_any([[{<<"Type2">>, [?INT64(-2)]}],
		       [{<<"Type2">>, [{?INT64(-2), true}], []}]]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.



%%========================================================================
%% minus_one_multi(Config) -> ok.
%% 
%% @doc 
%% send -1 as answer for sc
%% @end
%%========================================================================
minus_one_multi(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    %% PmGrps = ["Group1", "Group2"],
    %% TopLdn = "ManagedElement=1,Transport=1",
    PmGrps = ["ROPGroupMult"],
    TopLdn = "ManagedElement=1,Erat=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    %% Ldn      = ?LDN_ADDR_IPV4,
    %% Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    %% Values   = {?SC_OK, "", [{"Type3", [1,2,-1,4]}]},
    Ldn = "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1",
    Counters = ?MULTI_VAL_CNT,
    Values   = {?SC_OK, "", [{"ROPType12AvgMult", [1,-1,4]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    ResV = sc_res_any([[{<<"ROPType12AvgMult">>, 
			 [?INT64(1), ?INT64(-1), ?INT64(4)]}],
		       [{<<"ROPType12AvgMult">>, 
			 [{?INT64(1), true}, 
			  {?INT64(-1), true}, 
			  {?INT64(4), true}], []}]]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% minus_two_multi(Config) -> ok.
%% 
%% @doc 
%% send -2 as answer for sc
%% @end
%%========================================================================
minus_two_multi(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    %% PmGrps = ["Group1", "Group2"],
    %% TopLdn = "ManagedElement=1,Transport=1",
    PmGrps = ["ROPGroupMult"],
    TopLdn = "ManagedElement=1,Erat=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    %% Ldn      = ?LDN_ADDR_IPV4,
    %% Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    %% Values   = {?SC_OK, "", [{"Type2", [1,2,-2,4]}]},
    Ldn = "ManagedElement=1,Erat=1,Elizabeth=1,Andrew=1",
    Counters = ?MULTI_VAL_CNT,
    Values   = {?SC_OK, "", [{"ROPType12AvgMult", [1,-2,4]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(any, Values),    
    %% ResV = sc_res([{<<"Type2">>, 
    %% 		    [?INT64(1), ?INT64(2), ?INT64(-2), ?INT64(4)]}]),
    ResV = sc_res_any([[{<<"ROPType12AvgMult">>, 
			 [?INT64(1), ?INT64(-2), ?INT64(4)]}],
		       [{<<"ROPType12AvgMult">>, 
			 [{?INT64(1), false}, 
			  {?INT64(-2), true}, 
			  {?INT64(4), false}], []}]]),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% ignore_request(Config) -> ok.
%% 
%% @doc 
%% do not reply on the sc request
%% @end
%%========================================================================
ignore_request(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(do_not_reply, no_values),    
    ResV = sc_res({error, failure}),

    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.


%%========================================================================
%% one_counter_late_reply(Config) -> ok.
%% 
%% @doc 
%% show counters whith reply after timeout
%% @end
%%========================================================================
one_counter_late_reply(_Config) ->
    
    L = length(get_table(pmsScAppMoLdns)),

    PmGrps = ["Group1", "Group2"],
    TopLdn = "ManagedElement=1,Transport=1",

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize_2(?SUNE, [{TopLdn, PmGrps}]),

    Ldn      = ?LDN_ADDR_IPV4,
    Counters = [<<"Type2">>, <<"Type3">>,<<"PmFlexType2">>, <<"PmFlexType3">> ],
    Values   = {?SC_OK, "", [{"Type2", [1]}]},
    
    sc(counters, Ldn),
    ResN = sc_res(Counters),
    
    sc(values, Ldn),
    wait_report_sc(wait, {12000, Values}),    
    ResV = sc_res_any([{error, ?SC_COM_INTERNAL_ERROR},
		       {error, ?SC_COM_INTERNAL_ERROR, <<"int err msg">>}]),
    %%to(2),
    finalize(?SUNE),
    stop(?SUNE),

    ok = wait_table_size(pmsScAppMoLdns, L),

    case {ResN, ResV} of
	{ok, ok} -> ok;
	{ok, E}  -> ct:fail(E);
	{E,  _}  -> ct:fail(E)
    end.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


sc(Case, Ldn) ->
    Self = self(),
    spawn(fun() -> sc_init(Self, Case, Ldn) end).

sc_init(Pid, counters, Ldn) ->
    Res = rpc(pmsShowCountersI, get_measurement_names, [list_to_binary(Ldn)]),
    sc_init_rc(Pid, Res);
sc_init(Pid, values, Ldn) ->
    Res = rpc(pmsShowCountersI, get_measurements, 
	      [list_to_binary(Ldn), [], []], 25000),
    sc_init_rc(Pid, Res);
sc_init(Pid, values_verbose, Ldn) ->
    Res = rpc(pmsShowCountersI, get_measurements, 
	      [list_to_binary(Ldn), [], [{verbose, true}]], 25000),
    sc_init_rc(Pid, Res).

sc_init_rc(Pid, Res) when is_list(Res) ->
    Pid ! {sc_res, lists:sort(Res)};
sc_init_rc(Pid, Res) ->
    Pid ! {sc_res, Res}.
    
    

sc_res(Res) when is_list(Res) -> 
    sc_res2(lists:sort(Res));
sc_res({_, _} = Res) -> 
    sc_res2(Res);
sc_res(X) -> 
    ct:log("sc_res   ~p~n", [X]),
    ok.

sc_res2(Res) -> 
    ct:log("waiting for rpc to reply: ~p~n", [Res]),
    receive 
	{sc_res, Res} ->
	    ok;
	{sc_res, Error} ->
	    ct:log("rpc error ~p~n", [Error]),
	    ct:fail({error, {sc, {Error, Res}}})	    
    after 25000 ->
	    ct:log("timeout waiting rpc reply ~n"),
	    ct:fail({error, {sc, timeout, Res}})
    end.

sc_res_any([ResOld, ResNew] = Res) -> 
    ct:log("waiting for rpc to reply with either of: ~p~n", [Res]),
    receive 
	{sc_res, ResNew} ->
	    ct:log("Got new format: ~p~n", [ResNew]),
	    ok;
	{sc_res, ResOld} ->
	    ct:log("Got old format: ~p~n", [ResOld]),
	    ok;
	{sc_res, Error} ->
	    ct:log("rpc error ~p~n", [Error]),
	    ct:fail({error, {sc, {Error, Res}}})	    
    after 25000 ->
	    ct:log("timeout waiting rpc reply ~n"),
	    ct:fail({error, {sc, timeout, Res}})
    end.


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

wait_table_size(Tab, L) ->
    wait_table_size(Tab, L, 30).

wait_table_size(Tab, L, N) when N >= 0 ->
    case get_table(Tab) of
	Elems when length(Elems) =:= L ->
	    ok;
	_ when N > 0 ->
	    timer:sleep(?SLEEP),
	    wait_table_size(Tab, L, N - 1);
	Elems ->
	    {error, {badlength, L, Elems}}
    end.


%% start(A)    -> {ok, _} = pms_erl_app:start(A).
start(A, B) -> {ok, _} = pms_erl_app:start(A, B).
stop(A)     -> ok = pms_erl_app:stop(A).   

%% initialize(A)      -> ok = ?LIB:initialize(A).
%% initialize(A, B)   -> ok = ?LIB:initialize(A, B).
initialize_2(A, B) -> ok = ?LIB:initialize_2(A, B).
finalize(A)        -> ok = ?LIB:finalize(A).
%% kill(A)            -> ?LIB:kill(A).
get_table(A)       -> ?LIB:get_table(A).
%% tables()           -> ?LIB:tables().
%% check_tables(A)    -> ?LIB:check_tables(A).
%% check_processes(A) -> ?LIB:check_processes(A).
cleanup()          -> ?LIB:cleanup().
%% log_msg()          -> ?LIB:log_msg().
rpc(M, F, A)       -> ?LIB:rpc(M, F, A).
rpc(M, F, A, T)    -> rct_rpc:call(?TESTNODE, M, F, A, T, noprint).

%% wait_table_size(A, B)    -> ?LIB:wait_table_size(A, B).
%% wait_table_size(A, B, C) -> ?LIB:wait_table_size(A, B, C).
%% wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
%% wait_subscribe(A, B)     -> ?LIB:wait_subscribe(A, B).
%% wait_subscribe(A, B, C)  -> ?LIB:wait_subscribe(A, B, C).
%% wait_report()            -> ?LIB:wait_report().
%% wait_report(A)           -> ?LIB:wait_report(A).
%% wait_report(A, B)        -> ?LIB:wait_report(A, B).
%% wait_for_report(A, B, C) -> ?LIB:wait_for_report(A, B, C).
wait_report_sc(A, B)     -> ?LIB:wait_report_sc(A, B).

%%create_job(A)       -> ?LIB:create_job(A).
%% update_job(A, B)    -> ?LIB:update_job(A, B).
%% delete_job(A)       -> ?LIB:delete_job(A).
%% delete_mr(A, B)     -> ?LIB:delete_mr(A, B).
%% create_job_mr(A, B) -> ?LIB:create_job_mr(A, B).

%% create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%% open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.


%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_uc_SUITE.erl %
%%% @version /main/R3A/R4A/R5A/R6A/R8A/R11A/1

%%% @doc 
%%% == Test suite for ROP file Use Case testing ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_rop_uc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("pms2_test.hrl").

%%% ----------------------------------------------------------
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
%%% R3A/1      2014-10-28 eolaand     First vsn of PMI2 ROP test
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

-compile([nowarn_export_all, export_all]).

-define(TESTNODE, testnode).

-define(LIB,     pms2_test_lib).
-define(ERL_APP, pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).
-define(PROXY, pms_pmi_proxy).

-define(NACKA, nacka).
-define(KENTA, kenta).
-define(RONNIE, ronnie).
-define(KENNEDY, kennedy).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).

-define(SLEEP,  500).

-define(HELPER_EXP_SUBSCR_ROPGROUP_1(Type),
	[{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, [{gp, 10}, {spec, [{?ROPGRP1_ALIAS, [Type]}]}]},
	 pmi2ReportRop]).

-define(CURRENT_STATE, 8).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    DefaultOpts = [{erl_api, false},
		   {trace_ift, true},
		   {trace_child, true}],
    ProxyPars = {?PROXY, get_ct_config(?PROXY, DefaultOpts)},
    Hooks = lists:keystore(?PROXY, 1, hooks(), ProxyPars),
    [
     {ct_hooks, Hooks},
     {timetrap, {seconds, 200}}
    ].


%% @hidden
init_per_suite(Config) ->
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    [{reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
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
end_per_testcase(_TestCase, _Config) ->
    cleanup(),
    close_trans(),
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     tc_1app_2job_same_mt,
     tc_1app_1job_64bit_val,
     tc_1app_1job_no_mo_instance,
     tc_rm_active_job,
     tc_pm_capabilities,
     tc_job_id_and_sw_vsn,
     tc_rop_generation
     %% tc_restart                 restarts f-s up the coverage
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
%% @doc
%% Start one application and create two PM jobs, both with the same set of 
%% counters, and verify that a ROP file is generated containing the same 
%% measurements for both jobs and no suspect marking.
%% @end
%%========================================================================
tc_1app_2job_same_mt(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job1 = "tc_1app_2job_same_mt_1",
    Job2 = "tc_1app_2job_same_mt_2",
    GP  = ?GP_10_SEC,
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Start 1st job and wait for subscribe
    %% Start 2nd job, no new subscribe should be received
    %%-------------------------------------------------------------
    ok = create_job(Job1, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    ok = create_job(Job2, get_mrs(MTs)),

    %%-------------------------------------------------------------
    %% Wait for 3 ROP data requests (first GP does not generate a ROP file)
    %% Just to be sure that both jobs are created when fetching data
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221, 3443}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 2st ROP, it should contain both jobs and the data
    %%-------------------------------------------------------------
    ok = check_rop_file(2, [Job1, 
			    Job2, 
			    "<r p=\"1\">1221</r>", "<r p=\"2\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% @doc
%% Start one application and create a PM job. Let the application report
%% counters with values greater and smaller than 16#FFFFFFFF (32 bit) and 
%% verify that a ROP file is generated containing the reported values with
%% no suspect marking.
%% @end
%%========================================================================
tc_1app_1job_64bit_val(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?NACKA,
    Job = "tc_1app_1job_64bit_val",

    {ok, Handle} = create_app(App, undefined, ?LDN_DEF_1(App)),

    %% BigVal = 16#FFFFFFFFFFFFFFFF,
    BigVal = 16#7FFFFFFFFFFFFFFF,
    SmallVal = 16#FFFFFFFF,
    ExpRop  = [{values, 
		[{?ROPGRP1_ALIAS, [{1, [{?ROPGRP1_SUMTYPE_ALIAS,[BigVal]}, 
					{?ROPGRP1_AVGTYPE_ALIAS,[SmallVal]}]}]}
		]}],
    ExpOpts = [{gp, 10}, {spec, [{?ROPGRP1_ALIAS, [?ROPGRP1_SUMTYPE_ALIAS,
						   ?ROPGRP1_AVGTYPE_ALIAS]}]}],
    SubscrOpt = [{pmi2SubscribeRop, ExpOpts},
		 {pmi2ReportRop, ExpRop}],

    {ok, RefSubscr1} = expected(App, Handle, SubscrOpt),
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1, ?ROPType1Sum},
			  {Job ++ "_mr2", ?ROPGroup1, ?ROPType2Avg}]),
    wait_expected_result([{RefSubscr1, App}]),

    {ok, RefSubscr2} = expected(App, Handle, [{pmi2ReportRop, ExpRop}]),
    {ok, RopRef} = ?ROP_HOOK:expected([{1, [itl(BigVal), itl(SmallVal)], 
					   ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr2, App}], [{RopRef, ?ROP_HOOK}]),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs([Job]),
    wait_expected_result([{RefFinal, App}]),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% tc_1app_1job_no_mo_instance(_Config) -> ok
%% 
%% @doc
%% NodeUC556.A2: Start a PM Measurement Job, 
%% activation of counters where no MO instance exists
%% No ROP file should be created
%% @end
%%========================================================================
tc_1app_1job_no_mo_instance(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_stop_15",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% No values because no mo instance
    %% Wait for ROP, no ROP should be generated
    %%-------------------------------------------------------------
    Values = [],
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    

    %%-------------------------------------------------------------
    %% wait one GP to check that no ROP file is generated
    %%-------------------------------------------------------------
    0 = get_noof_rop_files(),
    wait_one_gp(GP),
    0 = get_noof_rop_files(),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).



%%========================================================================
%% tc_rm_active_job(_Config) -> ok
%% 
%% @doc
%% NodeUC558.E1 Try to delete a PM Measurement Job that is running
%% @end
%%========================================================================
tc_rm_active_job(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_stop_15",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% try to delete job in active state
    %%-------------------------------------------------------------
    ExpRes = ["Transaction commit failed, "
	"[Not allowed to remove PmJob in state ACTIVE]"],
    ErrMsg = 'error-message',
    {ok, _} = open_trans(),
    ok      = pms_netconf:delete_job(Job),
    {error, Error} = close_trans(),
    case lists:keyfind(ErrMsg, 1, Error) of
	{ErrMsg, _, ExpRes} -> ok;
	_                   -> ct:fail({not_found, {ExpRes, Error}})
    end,
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% tc_rm_active_job(_Config) -> ok
%% 
%% @doc
%% Setting the attributes in the PmMeasurementcapabilities MO 
%% in the PmMeasurementcapabilities ECIM PM fragment
%% @end
%%========================================================================
tc_pm_capabilities(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    Expected = [{
      pmMeasurementCapabilities,
      {"1","1","1","1"},   %% pmMeasurementCapabilitiesId  
      50,                  %% maxNoOfJobs
      1,                   %% jobStartStopSupport = BASIC
      false,               %% finalROP 
      false,               %% jobPrioritizationSupport
      undefined,           %% maxNoOfMeasurements 
%      250000,              %% maxNoOfMeasurements 
      400,                 %% maxNoOfPmFiles
      true,                %% alignedReportingPeriod 
      true,                %% measurementJobSupport 
      false,               %% realTimeJobSupport 
      false,               %% thresholdJobSupport 
      "/rop",              %% fileLocation
      undefined,           %% fileGroup
      false,               %% fileRPSupported 
      [5],                 %% supportedRopPeriods 
      [5],                 %% supportedMeasJobGps
      undefined,           %% supportedRtJobGps
      undefined,           %% supportedThreshJobGps 
      [0],                 %% supportedCompressionTypes 
      true,                %% jobGroupingSupport 
      true,                %% producesUtcRopFiles 
      1                    %% ropFilenameTimestamp  
     }],
    
    Got = rpc(ets, tab2list, [pmMeasurementCapabilities]),
    ct:log("Expected:~n~p~n~nGot:~n~p~n", [Expected, Got]),
    Expected = Got,
    ok.


%%========================================================================
%% tc_job_id_and_sw_vsn(_Config) -> ok
%% 
%% @doc
%% Use of optional items in the PM statistics file
%% check that job id and sw version are in the ROP file
%% @end
%%========================================================================
tc_job_id_and_sw_vsn(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_stop_15",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    MetaData = rpc(swmI, get_current_up_metadata, []),
    ProdNr   = proplists:get_value(productNumber, MetaData),
    ProdRev  = proplists:get_value(productRevision, MetaData),

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221, 3443}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, [Job, ProdNr, ProdRev]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% tc_rop_generation(_Config) -> ok
%% 
%% @doc
%% Check that the rop file is generated in time
%% @end
%%========================================================================
tc_rop_generation(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_stop_15",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221, 3443}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">1221</r>", "<r p=\"2\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% Check that the rop file is generated in time
    %%-------------------------------------------------------------
    {ok, [RopFile]} = ?ROP_HOOK:list_rop_files(),
    ct:pal("###### ~p~n", [RopFile]),
    Time = calendar:local_time_to_universal_time_dst(calendar:local_time()),
    ok = check_rop_time(RopFile, Time),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% tc_restart(_Config) -> ok
%% 
%% @doc
%% Start two jobs, one active one stopped 
%% restart node
%% check the state of the jobs
%% @end
%%========================================================================
tc_restart(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App  = ?NACKA,
    Job1 = "tc_restart_1",
    Job2 = "tc_restart_2",
    GP   = ?GP_10_SEC,
    MTs  = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, _Handle} = create_app({App, [{expected, [relay]}]}),
    
    %%-------------------------------------------------------------
    %% create_jobs
    %%-------------------------------------------------------------
    ok = create_job(Job1, get_mrs(MTs), [{"requestedJobState", ?STOPPED}]),
    ok = create_job(Job2, get_mrs(MTs)),
    
    SubscrData = get_subscribe_data(GP, MTs),
    ok = wait_subscribe(SubscrData),

    %%-------------------------------------------------------------
    %% restart node and wait until up
    %%-------------------------------------------------------------
    rpc(init, restart, []),

    subscribe_testnode(),
    ok = wait_node_up(120000),

    %%-------------------------------------------------------------
    %% Send initialize again and wait for subscribe
    %% (there may be rop data requests from before the restart
    %% in the in-queue, thus call wait_until_subscribe)
    %%-------------------------------------------------------------
    {ok, HandleRestart} = initialize(App),
    ok = wait_until_subscribe(SubscrData),

    %%-------------------------------------------------------------
    %% Check that job1 is in stopped state and
    %% job2 is in active state
    %%-------------------------------------------------------------
    {ok, [RecJob1]} = rpc(pmsDb, pm_job_get, [{"1","1","1",Job1}]),
    {ok, [RecJob2]} = rpc(pmsDb, pm_job_get, [{"1","1","1",Job2}]),

    ct:pal("#### Jobs ~p~n", [RecJob1]),
    ct:pal("#### Jobs ~p~n", [RecJob2]),

    ?STOPPED = element(?CURRENT_STATE, RecJob1),
    ?ACTIVE  = element(?CURRENT_STATE, RecJob2),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, HandleRestart).




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% misc functions
%%========================================================================

wait_node_up(After) ->
    receive
	{_, {_, node_up}} ->
	    ok;
	{_, {_, node_nown}} ->
	    wait_node_up(After)
    after After ->
	    {error, wait_node_up}
    end.
   
check_rop_time(RopFile, [{_, {H, M, S}}]) ->
    [_, _, RopTime | _] = string:tokens(RopFile, ".-_"),
    RopTimeSec = crt_rop(RopTime),
    TimeSec    = H * 60 * 60 + M * 60 + S,
    ct:pal("#### RopTime ~p   CurrentTime ~p~n", [RopTimeSec, TimeSec]),
    crt(TimeSec - RopTimeSec).
    

crt(Diff) when Diff =< 7 ->
    ok;
crt(Diff) ->
    {error, {rop_not_in_time, Diff}}.


crt_rop([A,B,C,D]) ->
    l2i([A,B]) * 60 * 60 +
    l2i([C,D]) * 60;
crt_rop([A,B,C,D,E,F]) ->
    l2i([A,B]) * 60 * 60 +
    l2i([C,D]) * 60 +
    l2i([E,F]).
    

l2i(L) ->
    list_to_integer(L).


    
%% gp(?TEN_SECONDS)    -> 10;
%% gp(?THIRTY_SECONDS) -> 30.

to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.

%%========================================================================
%% get_values(MTs, LDN, Values) -> AliasValues
%%
%% Create values to be reported in the next rop data request
%%========================================================================
get_values([{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
	   LDN,
	   [{Sum, Min}]) -> 
    Vals = [{?ROPGroup1, [{LDN, [{?ROPType1Sum, [Sum]},
				 {?ROPType3Min, [Min]}]}]}],
    aliasify_values(Vals).




    
get_ct_config(TC) -> ?LIB:get_ct_config(TC).
get_ct_config(TC, DefVal) -> ?LIB:get_ct_config(TC, DefVal).
    
%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).   
cleanup() -> ?LIB:cleanup().

initialize(A) -> ?LIB:initialize(A).
%% finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%% counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%% get_aliases()        -> ?LIB:get_aliases().

create_app(A)       -> ?LIB:create_app(A).
create_app(A, B)    -> ?LIB:create_app(A, B).
create_app(A, B, C) -> ?LIB:create_app(A, B, C).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
create_job(A, B)    -> ?LIB:create_job(A, B).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
create_jobs(A)      -> ?LIB:create_jobs(A).
delete_job(A, B)    -> ?LIB:delete_job(A, B).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
%%update_job(A, B)    -> ?LIB:update_job(A, B).
delete_job(A)       -> ?LIB:delete_job(A).

create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)       -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

expected(A, B, C)               -> ?LIB:expected(A, B, C).
wait_expected_result(A)         -> ?LIB:wait_expected_result(A, []).
wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B, 20000).


check_rop_file(A, B)     -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)  -> ?LIB:check_rop_file(A, B, C).
wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
wait_until_subscribe(A)  -> ?LIB:wait_until_subscribe(A).
wait_report_rop(A, B, C) -> ?LIB:wait_report_rop(A, B, C).
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).
    
subscribe_testnode()     -> ?LIB:subscribe_testnode().


%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).
    
host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

itl(I) when is_integer(I) ->
    integer_to_list(I);
itl(L) when is_list(L) ->
    L.

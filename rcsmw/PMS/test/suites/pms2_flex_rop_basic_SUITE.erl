%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_flex_rop_basic_SUITE.erl %
%%% @version /main/R6A/R11A/1

%%% @doc 
%%% == Test suite for basic testing of ROP files ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_flex_rop_basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("pms2_test.hrl").

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R4A/6      2015-08-25 etxjovp     Add group definitions used by CS CI
%%% R5A/1      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
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

-define(NACKA, nacka).
-define(KENTA, kenta).
-define(RONNIE, ronnie).
-define(KENNEDY, kennedy).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).


-define(SLEEP,  500).



%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    ProxyTag = {pms_pmi_proxy, get_ct_config(pms_pmi_proxy)},
    Hooks    = lists:keystore(pms_pmi_proxy, 1, hooks(), ProxyTag),
    [{ct_hooks, Hooks ++ [{pms_rop_hook, []}, {pms2_erl_app, []}]}].


%% @hidden
init_per_suite(Config) ->
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, putenv, ["FAKE_VNFM", ""]);
        _ ->
            rpc(os, unsetenv, ["FAKE_VNFM"])
    end,
    [{reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    %%rpc(pmsDebug, stop_clear, []),
    %% Temporary fix ends here
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, unsetenv, ["FAKE_VNFM"]);
        _ ->
            ok
    end,
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
    %% [catch ?ERL_APP:stop(App) || App <- ?TEST_APPS],
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     tc_1app_1flexjob,
     tc_1flexjob_1app,
     tc_2app_1flexjob
     %%      tc_1job_2app_2groups,
     %%      tc_1job_1app_2inst_avg,
     %%      tc_1job_1app_2inst_min,
     %%      tc_1job_1app_2inst_max,
     %%      tc_1job_1app_2inst_avg,
     %%      tc_1job_1app_2inst_lu,
     %%      tc_2rop_files,
     %%      tc_neg
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
%% Start one application and a PM job with one counter and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
%%========================================================================
tc_1app_1job(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_1app_1job",
    GP  = ?GP_10_SEC,
    MTs = [{?FlexROPGroup1, [?FlexROPType1Sum, ?FlexROPType3Min]}],

    {ok, Handle} = create_app({App, [{expected, [relay]}]}, ?FLEX_ROP_GRP_1),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    Val2   = get_values(MTs, LDN, [3443, 1212]),
    Values = [{Val2, ?FF_TRUE}],
    ok = wait_report_rop(Values, AppData),    
    ok = wait_report_rop(Values, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">3443</r>", "<r p=\"2\">1212</r>"], []),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    ok = delete_app(App, Handle).



%%========================================================================
%% @doc
%% Start one application and a PM job with one counter and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
%%========================================================================
tc_1app_1flexjob(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_1app_1flexjob",
    GP  = ?GP_10_SEC,
    BaseMTs = [{?FlexROPGroup1, [?FlexROPType1Sum,        ?FlexROPType3Min]}],
    FlexMTs = [{?FlexROPGroup1, [?FlexROPType1SumFilter1, ?FlexROPType3MinFilter1]}],
    

    {ok, Handle} = create_app({App, [{expected, [relay]}]}, ?FLEX_ROP_GRP_1),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    counter_map(App, Handle, aliasify_flex_mts(BaseMTs)),
    counter_map(App, Handle, aliasify_flex_mts(FlexMTs)),

    ok = create_job(Job, get_mrs(BaseMTs)),
    ok = wait_subscribe(get_subscribe_data(GP, BaseMTs)),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    Val2   = get_values(FlexMTs, LDN, [3443, 1212]),
    Values = [{Val2, ?FF_TRUE}],
    ok = wait_report_rop(Values, AppData),    
    ok = wait_report_rop(Values, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">3443</r>", "<r p=\"2\">1212</r>"], []),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    ok = delete_app(App, Handle).


%%========================================================================
%% @doc
%% Start first a PM job with one counter and then an app. Verify that a ROP 
%% file is generated with no suspect marking.
%% @end
%%========================================================================
tc_1flexjob_1app(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_1flexjob_1app",
    GP  = ?GP_10_SEC,
    BaseMTs = [{?FlexROPGroup1, [?FlexROPType1Sum,        ?FlexROPType3Min]}],
    FlexMTs = [{?FlexROPGroup1, [?FlexROPType1SumFilter1, ?FlexROPType3MinFilter1]}],
    

    ok = create_job(Job, get_mrs(BaseMTs)),

    {ok, Handle} = create_app({App, [{expected, [relay]}]}, ?FLEX_ROP_GRP_1),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    counter_map(App, Handle, aliasify_flex_mts(BaseMTs)),
    counter_map(App, Handle, aliasify_flex_mts(FlexMTs)),

    ok = wait_subscribe(get_subscribe_data(GP, BaseMTs)),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    Val2   = get_values(FlexMTs, LDN, [3443, 1212]),
    Values = [{Val2, ?FF_TRUE}],
    ok = wait_report_rop(Values, AppData),    
    ok = wait_report_rop(Values, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">3443</r>", "<r p=\"2\">1212</r>"], []),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    ok = delete_app(App, Handle).


%%========================================================================
%% @doc
%% Start two applications and a PM job with one counter and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
%%========================================================================
tc_2app_1flexjob(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App1 = ?NACKA,
    App2 = ?RONNIE,
    Job = "tc_2app_1flexjob",

    GP  = ?GP_10_SEC,
    BaseMTs = [{?FlexROPGroup1, [?FlexROPType1Sum,        ?FlexROPType3Min]}],
    FlexMTs = [{?FlexROPGroup1, [?FlexROPType1SumFilter1, ?FlexROPType3MinFilter1]}],
    
    {ok, Handle1} = create_app({App1, [{expected, [relay]}]}, ?FLEX_ROP_GRP_1),
    {ok, Handle2} = create_app({App2, [{expected, [relay]}]}, ?FLEX_ROP_GRP_1),

    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(App1)),
    AppData1  = {App1, Handle1, GP},
    AppData2  = {App2, Handle2, GP},
    counter_map(App1, Handle1, aliasify_flex_mts(BaseMTs)),
    counter_map(App1, Handle1, aliasify_flex_mts(FlexMTs)),
    counter_map(App2, Handle2, aliasify_flex_mts(BaseMTs)),
    counter_map(App2, Handle2, aliasify_flex_mts(FlexMTs)),

    ok = create_job(Job, get_mrs(BaseMTs)),
    ok = wait_subscribe(get_subscribe_data(GP, BaseMTs)),
    ok = wait_until_subscribe(get_subscribe_data(GP, BaseMTs)),

    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values1 = [{?FlexROPGroup1, [{LDN, [{?FlexROPType1SumFilter1, [111]},
					{?FlexROPType3MinFilter1, [100]}]}]}],
    
    Values2 = [{?FlexROPGroup1, [{LDN, [{?FlexROPType1SumFilter1, [222]},
					{?FlexROPType3MinFilter1, [200]}]}]}],
    
    AliasValues1 = aliasify_values(Values1),
    AliasValues2 = aliasify_values(Values2),
    
    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    ok = wait_report_rop(AliasValues1, ?FF_TRUE, AppData1),    
    ok = wait_report_rop(AliasValues2, ?FF_TRUE, AppData2),    
    ok = wait_report_rop(AliasValues1, ?FF_TRUE, AppData1),    
    ok = wait_report_rop(AliasValues2, ?FF_TRUE, AppData2),    
    
    {ok, RopRef} = pms_rop_hook:expected([{1, [], ["suspect"]}]),
    wait_expected_result([], [{RopRef, ?ROP_HOOK}]),


    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),

    ok = delete_app(App1, Handle1),
    ok = delete_app(App2, Handle2).

%%========================================================================
%% @doc
%% Start one application and a PM job with multi value counters and verify that
%% a ROP file is generated with no suspect marking.
%% @end
%%========================================================================
tc_1app_1job_multi_val(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_1app_1job",
    GP  = ?GP_10_SEC,
    MTs = [{?FlexROPGroup1, [?FlexROPType1Sum, ?FlexROPType3Min]}],

    {ok, Handle} = create_app({App, [{expected, [relay]}]}, ?FLEX_ROP_GRP_1),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    Val2   = get_values(MTs, LDN, [[3443, 4334], 1212]),
    Values = [{Val2, ?FF_TRUE}],
    ok = wait_report_rop(Values, AppData),    
    ok = wait_report_rop(Values, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">3443</r>", "<r p=\"2\">1212</r>"], []),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    ok = delete_app(App, Handle).


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% misc functions
%%========================================================================
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
get_values([{?FlexROPGroup1, [?FlexROPType1Sum, ?FlexROPType3Min]}],
	   LDN,
	   [{Sum, Min}]) -> 
    Vals = [{?FlexROPGroup1, [{LDN, [{?FlexROPType1Sum, [Sum]},
				     {?FlexROPType3Min, [Min]}]}]}],
    aliasify_values(Vals);

get_values([{RG, MTs}], LDN, Vals) -> 
    MVals = lists:map(fun(Val) when is_list(Val) ->
			      Val;
			 (Val) ->
			      [Val]
		      end, Vals),
    MeasVals = [{RG, [{LDN, lists:zip(MTs, MVals)}]}],
    aliasify_values(MeasVals).
    


    
get_ct_config(TC) -> ?LIB:get_ct_config(TC).
    
%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).   
cleanup() -> ?LIB:cleanup().

%% initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%% finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%% counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%% get_aliases()        -> ?LIB:get_aliases().

create_app(A)       -> ?LIB:create_app(A).
create_app(A, B)    -> ?LIB:create_app(A, B).
create_app(A, B, C) -> ?LIB:create_app(A, B, C).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
create_job(A, B)    -> ?LIB:create_job(A, B).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
%%create_jobs(A)      -> ?LIB:create_jobs(A).
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

%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).
    
host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

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
aliasify_flex_mts(A)     -> ?LIB:aliasify_flex_mts(A).    
    

wait_report_rop(A, B)        -> ?LIB:wait_report_rop(A, B).

counter_map(A, B, C) -> ?LIB:counter_map(A, B, C).

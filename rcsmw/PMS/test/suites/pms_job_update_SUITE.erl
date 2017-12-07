%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_job_update_SUITE.erl %
%%% @version /main/R5A/R11A/1

%%% @doc
%%% == Test suite for ROP testing the use of final fragment ==
%%% This test suite can be used in both simulated and target environment.
%%% @end

-module(pms_job_update_SUITE).

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
%%% R5A/1      2016-02-22 ekrebak     Created
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

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() ->
    ProxyPars = {?PROXY, get_ct_config(?PROXY, [])},
    Hooks = lists:keystore(?PROXY, 1, hooks(), ProxyPars),
    [
     {ct_hooks, Hooks},
     {timetrap, {seconds, 300}}
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
        add_mr,
        rm_mr,
        rm_last_mr
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
%% add_mr(_Config) -> ok
%%
%% @doc
%% Start a job with a single MR
%% Additionally add a second MR to the same job
%% @end
%%========================================================================
add_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App     = ?SUNE,
    [LDN]   = ?LDN_DEF_1(App),
    
    Job     = "add_mr_pmi",
    GP      = ?GP_10_SEC,
    
    MTs1    = [{?ROPGroup1, [?ROPType1Sum]}],
    MTs2    = [{?ROPGroup1, [?ROPType1Sum, ?ROPType2Avg]}],

    Values = get_pmi_values(MTs2, LDN, [[3443], [1221]]),

    MR1  = get_mrs(MTs1),
    MR2  = get_mrs(MTs2),

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app_pmi({App, 
                                  [{expected, [relay]}]}, 
                                  [?ROPGroup1]),

    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, MR1),
    ok = wait_pmi_subscribe({GP, MTs1}),

    %%-------------------------------------------------------------
    %% Check ROP, there shouldn't be any yet
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain both group and one type
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    ok = check_rop_file(1, [?ROPGroup1, ?ROPType1Sum], 
                           [?ROPType2Avg]),

    %%-------------------------------------------------------------
    %% Add MR to the job
    %%-------------------------------------------------------------
    ok = create_job(Job, MR2),
    ok = wait_until_pmi_subscribe({GP, MTs2}),

    %%-------------------------------------------------------------
    %% Second ROP file should still contain the old state.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    ok = check_rop_file(2, [?ROPGroup1, ?ROPType1Sum], 
                           [?ROPType2Avg]),

    %%-------------------------------------------------------------
    %% Third ROP file should now contain the new data.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    ok = check_rop_file(3, [?ROPType1Sum, ?ROPType2Avg]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_pmi_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% rm_mr(_Config) -> ok
%%
%% @doc
%% Star a job with 2 MRs
%% Subsequenlty remove an MR from the job
%% @end
%%========================================================================
rm_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    GP  = ?GP_10_SEC,
    Job = "rm_mr_pmi",

    MTs  = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    MTs2 = [{?ROPGroup1, [?ROPType1Sum]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app_pmi(
        {App, [{expected, [relay]}]},
        [?ROPGroup1]
    ),

    [LDN]   = ?LDN_DEF_1(App),
    AppData = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_pmi_subscribe({GP, MTs}),

    Values = get_pmi_values(MTs, LDN, [[1221], [1222]]),

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (firs complete) RP, a ROP file
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    ok = check_rop_file(1, [?ROPGroup1, ?ROPType1Sum, ?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Remove an MR from the job
    %%-------------------------------------------------------------
    ok = delete_mr(Job, ?ROPType3Min),
    ok = wait_until_pmi_subscribe({GP, MTs2}),

    Values2 = get_pmi_values(MTs2, LDN, [[1221]]),

    %%-------------------------------------------------------------
    %% The deletion of the MR should be reflected in the
    %% very next ROP file, after an incomplete RP.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values2, AppData),
    ok = check_rop_file(2, [?ROPGroup1, ?ROPType1Sum], [?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Delete the job.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_pmi_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% Delete the app.
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% rm_last_mr(_Config) -> ok
%%
%% @doc
%% Star a job with a single MRs
%% Subsequenlty try to remove that MR from
%% the job, which should fail.
%% @end
%%========================================================================
rm_last_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    GP  = ?GP_10_SEC,
    Job = "rm_last_mr_pmi",

    MTs = [{?ROPGroup1, [?ROPType1Sum]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app_pmi(
        {App, [{expected, [relay]}]},
        [?ROPGroup1]
    ),

    [LDN]   = ?LDN_DEF_1(App),
    AppData = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_pmi_subscribe({GP, MTs}),

    Values = get_pmi_values(MTs, LDN, [[1221]]),

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (firs complete) RP, a ROP file
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    ok = check_rop_file(1, [?ROPGroup1, ?ROPType1Sum]),

    %%-------------------------------------------------------------
    %% Try remove the only MR from the job, should fail.
    %%-------------------------------------------------------------
    {error, _Reason} = delete_mr(Job, ?ROPType1Sum),

    %%-------------------------------------------------------------
    %% Delete the job.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_pmi_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% Delete the app.
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% get_pmi_values([{RG, MTs}], LDN, Values) -> ReportValues
%%
%% Create values to be reported in the
%% next ROP data request for PMI interface.
%%========================================================================
get_pmi_values([{RG, MTs}], LDN, Vals) ->
    ToList = fun
        (Val) when is_list(Val) -> Val;
        (Val) -> [Val]
    end,

    MVals  = lists:map(ToList, Vals),
    Zipped = {RG, lists:zip(MTs, MVals)},
    [{LDN, [Zipped]}];

get_pmi_values(MRSpecs, LDN, Vals) ->
    GetEachValue = fun(MRSpec, Values) ->
        get_pmi_values([MRSpec], LDN, Values)
    end,

    Res = lists:zipwith(GetEachValue, MRSpecs, Vals),
    lists:flatten(Res).




%%========================================================================
%% lib functions
%%========================================================================

%%========================================================================
%% misc functions
%%========================================================================

to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.




%%get_ct_config(TC)         -> ?LIB:get_ct_config(TC).
get_ct_config(TC, DefVal) -> ?LIB:get_ct_config(TC, DefVal).

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
create_jobs(A)      -> ?LIB:create_jobs(A).

create_app_pmi(A)       -> ?LIB:create_app_pmi(A).
create_app_pmi(A, B)    -> ?LIB:create_app_pmi(A, B).
create_app_pmi(A, B, C) -> ?LIB:create_app_pmi(A, B, C).
delete_app_pmi(A, B)    -> ?LIB:delete_app_pmi(A, B).


%%create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).

update_job(A, B) -> ?LIB:update_job(A, B).

delete_job(A)       -> ?LIB:delete_job(A).

create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)       -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

rop_data({A, B, C}, D, E, F) -> ?LIB:rop_data(A, B, C, D, E, F).


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
check_rop_file_name(A, B)    -> ?LIB:check_rop_file_name(A, B).
wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
wait_pmi_subscribe(A)        -> ?LIB:wait_pmi_subscribe(A).
wait_until_pmi_subscribe(A)  -> ?LIB:wait_until_pmi_subscribe(A).
wait_pmi_report_rop(A, B, C) -> ?LIB:wait_pmi_report_rop(A, B, C).
wait_pmi_report_rop(A, B)    -> ?LIB:wait_pmi_report_rop(A, B).
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).

rop_file_handling(A)         -> ?LIB:rop_file_handling(A).

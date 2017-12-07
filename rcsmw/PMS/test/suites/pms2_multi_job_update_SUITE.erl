%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_multi_job_update_SUITE.erl %
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/8

%%% @doc
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms2_multi_job_update_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("pms2_test.hrl").

%%% ----------------------------------------------------------
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
%%% R3A/1      2016-02-18 edamkon     Copied from pms_update_job_SUITE
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

-export([add_mr/1]).
-export([rm_mr/1]).
-export([add_pm_group/1]).
-export([rm_pm_group/1]).

-define(TESTNODE, testnode).

-define(LIB,     pms2_test_lib).
-define(ERL_APP, pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).

-define(JOB_1, "job_fh_1").
-define(JOB_2, "job_fh_2").

-define(COMMON_JOB_GROUP, undefined).
-define(JOB_GROUP1,       "GRAT").
-define(JOB_GROUP2,       "WRAT").

-define(SLEEP,  500).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() ->
    ProxyTag = {pms_pmi_proxy, get_ct_config(pms_pmi_proxy)},
    Hooks    = lists:keystore(pms_pmi_proxy, 1, hooks(), ProxyTag),
    [{ct_hooks, Hooks}].



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
    rpc(pmsDb, rop_file_delete_all, []),
    cleanup(),
    Config.

%% @hidden
end_per_testcase(session_kill_app_proc, _Config) ->
    cleanup(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    %%log_rop_files(Config),
    cleanup(),
    close_trans(),
    [catch ?ERL_APP:stop(App) || App <- ?TEST_APPS],
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    All = [
       add_mr,
       rm_mr,
       add_pm_group,
       rm_pm_group
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
%% add_mr(Config) -> ok.
%%
%% @doc
%% - Create app (with pmGroup1)
%% - Create 2 jobs with a single MR
%% - Subsequently add another MR to both jobs
%% @end
%%========================================================================
add_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App  = ?SUNE,
    GP   = ?GP_10_SEC,

    Job1 = "add_mr_job1",
    Job2 = "add_mr_job2",

    MTs1 = [{?Group1, [?Type1]}],
    MTs2 = [{?Group2, [?Type2]}],

    MR1  = get_mrs(MTs1),
    MR2  = get_mrs(MTs2),

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start both jobs, and log the accompanying data.
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, MR1}, {Job2, MR1}]),

    SubscribeData1 = get_subscribe_data(GP, MTs1),
    ok = wait_subscribe(SubscribeData1),

    Values1 = get_values(MTs1, LDN, [{1221}]),

    %%-------------------------------------------------------------
    %% No ROP files should be generated after first incomplete RP.
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values1, ?FF_TRUE, AppData),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% After the second RP (first full one), a job with MR1's
    %% values should have been generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values1, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Add MR2 to both jobs, and log the accompanying data.
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, MR2}, {Job2, MR2}]),

    BothMTs = MTs1 ++ MTs2,

    SubscribeData2 = get_subscribe_data(GP, BothMTs),
    ok = wait_subscribe(SubscribeData2),

    Values2 = get_values(BothMTs, LDN, [{1221}, {1222}]),

    %%-------------------------------------------------------------
    %% After another incomplete RP, new ROP file still
    %% shouldn't contain any new data.
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    ok = check_rop_file(2, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Finally, 4th ROP file should contain new data.
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    ok = check_rop_file(3, [?Group1, ?Type1, ?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Delete both jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% Delete the app.
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% rm_mr(Config) -> ok.
%%
%% @doc
%% - Create app (with pmGroup1 and pmGroup2)
%% - Create 2 jobs, both with 2 MRs
%% - Subsequently remove 1 MR from both jobs
%% @end
%%========================================================================
rm_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App     = ?SUNE,
    GP      = ?GP_10_SEC,

    Job1    = "rm_mr_job1",
    Job2    = "rm_mr_job2",

    MTs1    = [{?Group1, [?Type1]}],
    MTs2    = [{?Group2, [?Type2]}],
    BothMTs = MTs1 ++ MTs2,

    MR1     = get_mrs(MTs1),
    MR2     = get_mrs(MTs2),
    BothMRs = MR1 ++ MR2,

    [{MR2Name, _, _}] = MR2,

    %%-------------------------------------------------------------
    %% Create the app.
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start two jobs, with two MRs, and log the data for both MRs.
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, BothMRs}, {Job2, BothMRs}]),
    ok = wait_subscribe(get_subscribe_data(GP, BothMTs)),
    Values1 = get_values(BothMTs, LDN, [{1221}, {1222}]),

    %%-------------------------------------------------------------
    %% No ROPs should be generated after the first incomplete RP.
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values1, ?FF_TRUE, AppData),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% After the second RP (first full one), a job with both
    %% MRs' values should have been generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values1, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group1, ?Type1, ?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Remove MR2 from both jobs, and log the data for MR1.
    %%-------------------------------------------------------------
    ok = delete_mr_multi([Job1, Job2], MR2Name),
    ok = wait_subscribe(get_subscribe_data(GP, MTs1)),
    Values2 = get_values(MTs1, LDN, [{1221}]),

    %%-------------------------------------------------------------
    %% After another incomplete RP, new ROP file should
    %% immediately reflect the lack of deleted MR2.
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    ok = check_rop_file(2, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Delete both jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% Delete the app.
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% add_pm_group(Config) -> ok.
%%
%% @doc
%% 1 app init pmGroup1 and pmGroup2
%% - create 2 jobs with pmGroup1
%% - add pmGroup2 to both jobs
%% @end
%%========================================================================
add_pm_group(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App  = ?SUNE,
    GP   = ?GP_10_SEC,

    Job1  = "job1",
    Job2  = "job2",

    AppAttrs  = [{expected, [relay]}],
    
    MR1 = "MR1",
    MR2 = "MR2",

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    [{AppData, LDN}] = create_apps([App], AppAttrs, GP),

    %%-------------------------------------------------------------
    %% Start jobs
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, {MR1, ?Group1}},
                      {Job2, {MR1, ?Group1}}]),

    ok = wait_subscribe_multi(1, GP, [?Group1]),

    Values1 = get_values(
        [{?Group1, [?Type1]}],
        LDN,
        [{1221}]
    ),

    RopData1 = [{Values1, AppData}],

    %%-------------------------------------------------------------
    %% Wait for the first RP without ROP files
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),

    %%-------------------------------------------------------------
    %% Wait for the second RP with first ROP file
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    ok = check_rop_file(1, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Add Group2 to the jobs
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, {MR2, ?Group2}},
                      {Job2, {MR2, ?Group2}}]),

    ok = wait_subscribe_multi(1, GP, [?Group1, ?Group2]),

    Values2 = get_values(
        [{?Group1, [?Type1]},
         {?Group2, [?Type2, ?Type3]}],
        LDN,
        [{1221},
         {1222, 1223}]
    ),

    RopData2 = [{Values2, AppData}],

    %%-------------------------------------------------------------
    %% Wait for RP - no changes
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(2, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Wait for RP - check if changes are visible
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(3, [?Group1, ?Group2, ?Type1, ?Type2]),

    %%-------------------------------------------------------------
    %% Delete jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),
    ok = wait_subscribe_multi(1, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete apps.
    %%-------------------------------------------------------------
    ok = delete_apps([AppData]).


%%========================================================================
%% rm_pm_group(Config) -> ok.
%%
%% @doc
%% 1 app init pmGroup1 and pmGroup2
%% - create 2 jobs with pmGroup1 and pmGroup2
%% - remove pmGroup2 from both jobs
%% @end
%%========================================================================
rm_pm_group(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App  = ?SUNE,
    GP   = ?GP_10_SEC,

    Job1  = "job1",
    Job2  = "job2",

    AppAttrs  = [{expected, [relay]}],
    
    MR1 = "MR1",
    MR2 = "MR2",

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    [{AppData, LDN}] = create_apps([App], AppAttrs, GP),

    %%-------------------------------------------------------------
    %% Start jobs
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, [{MR1, ?Group1}, {MR2, ?Group2}]},
                      {Job2, [{MR1, ?Group1}, {MR2, ?Group2}]}]),

    ok = wait_subscribe_multi(1, GP, [?Group1, ?Group2]),

    Values1 = get_values(
        [{?Group1, [?Type1]},
         {?Group2, [?Type2, ?Type3]}],
        LDN,
        [{1221},
         {1222, 1223}]
    ),


    RopData1 = [{Values1, AppData}],

    %%-------------------------------------------------------------
    %% Wait for the first RP without ROP files
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),

    %%-------------------------------------------------------------
    %% Wait for the second RP with first ROP file
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    ok = check_rop_file(1, [?Group1, ?Group2]),

    %%-------------------------------------------------------------
    %% Remove Group2 from the jobs
    %%-------------------------------------------------------------
    ok = delete_mr(Job1, MR2),
    ok = delete_mr(Job2, MR2),
    ok = wait_subscribe_multi(1, GP, [?Group1]),

    Values2 = get_values(
        [{?Group1, [?Type1]}],
        LDN,
        [{1221}]
    ),

    RopData2 = [{Values2, AppData}],

    %%-------------------------------------------------------------
    %% Wait for RP - check if changes are visible
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(2, [?Group1], [?Group2]),

    %%-------------------------------------------------------------
    %% Delete jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),
    ok = wait_subscribe_multi(1, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete apps.
    %%-------------------------------------------------------------
    ok = delete_apps([AppData]).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% create_apps([{App, Attrs, GP}]) -> [{Handle, LDN}]
%%
%% App = Integer()
%% Attrs = [{expected, [relay]}]
%%
%% Create multiple apps.
%%========================================================================
create_apps(AppNames, Attrs, GP)->
    AppDefs = [{AppName, Attrs, GP} || AppName <- AppNames],
    lists:map(fun create_apps/1, AppDefs).

create_apps({AppName, Attrs, GP}) ->
    {ok, Handle} = create_app({AppName, Attrs}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(AppName)),
    AppData      = {AppName, Handle, GP},

    {AppData, LDN}.
    
%%========================================================================
%% wait_subscribe_multi(NoOfApps, GP, [MT], Lazy)
%%                                      -> ok | {error, description}
%%
%% NoOfApps = Integer()
%%
%% Lazy = Boolean() % whether wait_until_subscribe should be 
%%                  % used instead of wait_subscribe
%%
%% Wait for multiple apps to subscribe to the last Job change.
%%========================================================================
wait_subscribe_multi(NoOfApps, GP, MTs) ->
    wait_subscribe_multi(NoOfApps, GP, MTs, false).

wait_subscribe_multi(NoOfApps, GP, MTs, Lazy) ->
    GetSubscribeData = fun(_AppIndex) ->
        get_subscribe_data(GP, MTs)
    end,

    AppIndexes = lists:seq(1, NoOfApps),
    SubscribeData = lists:map(GetSubscribeData, AppIndexes),

    case Lazy of
        true -> wait_until_subscribe(SubscribeData);
           _ -> wait_subscribe(SubscribeData)
    end.
    
%%========================================================================
%% wait_report_rop_multi(FF, [AppInfo]) -> ok | [{error, description}]
%%
%% AppInfo = {AppValues, AppData}
%%
%% Wait for ROP to generated if multiple applications are used.
%%========================================================================
wait_report_rop_multi(FF, AppInfos) ->
    WaitReportRop = fun({AppValues, AppData}) ->
        wait_report_rop(AppValues, FF, AppData)
    end,
    
    Res = lists:map(WaitReportRop, AppInfos),
    all_ok(Res).

%%========================================================================
%% delete_apps([AppData]) -> ok | [{error, description}]
%%
%% AppData = {App, Handle, _GP}
%%
%% Delete multiple apps and provide cumulative deletion results, or
%% list of results if some of the deletion jobs failed.
%%========================================================================
delete_apps(AppDatas) ->
    DeleteApp = fun({App, Handle, _GP}) -> 
        delete_app(App, Handle)
    end,
    
    Res = lists:map(DeleteApp, AppDatas),
    all_ok(Res).

%%========================================================================
%% delete_mr_multi([Job], MR) -> ok | [{error, description}]
%% Delete a single MR for multiple pmJobs.
%%========================================================================
delete_mr_multi(Jobs, MR) ->
    DeleteMR = fun(Job) ->
        delete_mr(Job, MR)
    end,

    Res = lists:map(DeleteMR, Jobs),
    all_ok(Res).

%%========================================================================
%% all_ok([X]) -> ok | [X]
%% Checks whether all the items in the provided list are ok. If not,
%% returns that same list.
%%========================================================================
all_ok(Xs) ->
    Res = lists:all(fun (X) -> ok == X end, Xs),
    case Res of
        true -> ok;
           _ -> Xs
    end.

%%========================================================================
%% get_values(MTs, LDNs, Values) -> AliasValues
%%
%% MTs    = [MT]
%% MT     = {Group, Types}
%% Types  = [Type]
%% LDNs   = [LDN] | LDN
%% LDN    = Integer()
%% Values = [Value]
%% Value  = { Integer()... } % one or more integers in a tuple
%%
%% Create values to be reported in the next ROP data request.
%%========================================================================
get_values(GroupsWithTypes, LDNs, Values) when is_list(LDNs) ->
    Merges = [{GroupsWithTypes, LDN, Values} || LDN <- LDNs],
    lists:map(fun get_values/1, Merges);

get_values(GroupsWithTypes, LDN, Values) ->
    get_values({GroupsWithTypes, LDN, Values}).

get_values({GroupsWithTypes, LDN, Vals}) ->
    MergeTypesAndVals = fun (Type, Value) ->
        {Type, [Value]}
    end,

    MergeAll = fun({Group, Types}, ValuesTuple) ->
        Values = tuple_to_list(ValuesTuple),
        TVs = lists:zipwith(
            MergeTypesAndVals,
            Types,
            Values
        ),

        {Group, [{LDN, TVs}]}
    end,

    Merged = lists:zipwith(
        MergeAll,
        GroupsWithTypes,
        Vals
    ),

    aliasify_values(Merged).


to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


%%log_rop_files(Config) ->
%%    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
%%    log_rop_files(RopData, Config).


get_ct_config(TC) -> ?LIB:get_ct_config(TC).

%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).
cleanup() -> ?LIB:cleanup().

%% initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%% finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%% counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%% get_aliases()        -> ?LIB:get_aliases().

create_app(A)          -> ?LIB:create_app(A).
%%create_app(A, B)     -> ?LIB:create_app(A, B).
%%create_app_pmi(A)    -> ?LIB:create_app_pmi(A).
delete_app(A, B)       -> ?LIB:delete_app(A, B).
%%delete_app_pmi(A, B) -> ?LIB:delete_app_pmi(A, B).
%%create_job(A, B)     -> ?LIB:create_job(A, B).
%create_job(A, B, C)   -> ?LIB:create_job(A, B, C).
create_jobs(A)         -> ?LIB:create_jobs(A).
delete_jobs(A)         -> ?LIB:delete_jobs(A).
%%update_job(A, B)     -> ?LIB:update_job(A, B).
%%delete_job(A)        -> ?LIB:delete_job(A).

%create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)         -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
%%tables()           -> ?LIB:tables().
%%check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

%%expected(A, B, C)               -> ?LIB:expected(A, B, C).
%%wait_expected_result(A)         -> ?LIB:wait_expected_result(A).
%%wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B).
%%wait_expected_error_result(A,B) -> ?LIB:wait_expected_error_result(A, B).
%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%%open_trans()  -> ?LIB:open_trans().
close_trans()   -> Res = ?LIB:close_trans(), to(4), Res.

%%log_rop_files(RopData, Config) -> pms_test_lib:log_rop_files(RopData, Config).
%%check_rop_file_name(A, B)    -> ?LIB:check_rop_file_name(A, B).
%%check_rop_file_name(A, B, C) -> ?LIB:check_rop_file_name(A, B, C).
check_rop_file(A, B)           -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)        -> ?LIB:check_rop_file(A, B, C).
%%wait_one_gp(A)               -> ?LIB:wait_one_gp(A).
%%get_noof_rop_files()         -> ?LIB:get_noof_rop_files().
wait_subscribe(A)              -> ?LIB:wait_subscribe(A).
wait_until_subscribe(A)        -> ?LIB:wait_until_subscribe(A).
wait_report_rop(A, B, C)       -> ?LIB:wait_report_rop(A, B, C).
get_subscribe_data(A, B)       -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)                     -> ?LIB:get_mrs(A).
aliasify_values(A)             -> ?LIB:aliasify_values(A).
%%rop_file_handling(A)         -> ?LIB:rop_file_handling(A).

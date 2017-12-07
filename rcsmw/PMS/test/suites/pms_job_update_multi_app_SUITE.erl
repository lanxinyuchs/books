%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_job_update_multi_app_SUITE.erl %
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/6

%%% @doc
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms_job_update_multi_app_SUITE).

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
%%% R5L/1      2016-02-29 edamkon     Copied from pms_multi_job_update_SUITE
%%% R5L/2      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
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
%% - Create two 2 apps in PMI
%% - Create a job with a single MR
%% - Subsequently add another MR
%% @end
%%========================================================================
add_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    Apps     = [?SUNE, ?TINA],
    AppAttrs = [{expected, [relay]}],

    GP       = ?GP_10_SEC,
    Job      = "two_apps_add_mr_pmi",

    MTs1     = [{?ROPGroup1, [?ROPType1Sum]}],
    MTs2     = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],

    MR1      = get_mrs(MTs1),
    MR2      = get_mrs(MTs2),
    BothMTs  = MTs1 ++ MTs2,

    Groups   = get_groups_from_MTs(BothMTs),
    NoOfApps = length(Apps),

    %%-------------------------------------------------------------
    %% Create both apps.
    %%-------------------------------------------------------------
    {AppsData, LDNs} = create_apps_pmi(Apps, AppAttrs, GP, Groups),

    %%-------------------------------------------------------------
    %% Start job and wait for subscribes for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job, MR1),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, MTs1),

    Values1  = get_pmi_values_multi(MTs1, LDNs, [1221]),
    RopData1 = lists:zip(Values1, AppsData),

    %%-------------------------------------------------------------
    %% No ROP files should be generated after first incomplete RP.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% After the second RP (first full one), a job with MR1's
    %% values should have been generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    ok = check_rop_file(1, [?ROPGroup1, ?ROPType1Sum], [?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Add another MR to the job.
    %%-------------------------------------------------------------
    ok = create_job(Job, MR2),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, MTs2),

    Values2  = get_pmi_values_multi(MTs2, LDNs, [1221, 1222]),
    RopData2 = lists:zip(Values2, AppsData),

    %%-------------------------------------------------------------
    %% After another incomplete RP, new ROP file still
    %% shouldn't contain any new data.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData2),
    ok = check_rop_file(2, [?ROPGroup1, ?ROPType1Sum], [?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Finally, 3rd ROP file should contain new MR data.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData2),
    ok = check_rop_file(3, [?ROPGroup1, ?ROPType1Sum, ?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Delete the job.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete both apps.
    %%-------------------------------------------------------------
    ok = delete_apps(AppsData).

%%========================================================================
%% rm_mr(Config) -> ok.
%%
%% @doc
%% - Create two 2 apps in PMI
%% - Create a job with two MRs
%% - Subsequently remove one MR
%% @end
%%========================================================================
rm_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    Apps     = [?SUNE, ?TINA],
    AppAttrs = [{expected, [relay]}],

    GP       = ?GP_10_SEC,
    Job      = "two_apps_add_mr_pmi",

    MTs1     = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    MTs2     = [{?ROPGroup1, [?ROPType1Sum]}],

     MR1     = get_mrs(MTs1),
    _MR2     = get_mrs(MTs2),
    BothMTs  = MTs1 ++ MTs2,

    Groups   = get_groups_from_MTs(BothMTs),
    NoOfApps = length(Apps),

    %%-------------------------------------------------------------
    %% Create both apps.
    %%-------------------------------------------------------------
    {AppsData, LDNs} = create_apps_pmi(Apps, AppAttrs, GP, Groups),

    %%-------------------------------------------------------------
    %% Start job and wait for subscribes for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job, MR1),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, MTs1),

    Values1  = get_pmi_values_multi(MTs1, LDNs, [1221, 1222]),
    RopData1 = lists:zip(Values1, AppsData),

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (first complete) RP, that should
    %% generate the first ROP file, with data for both MRs.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    ok = check_rop_file(1, [?ROPGroup1, ?ROPType1Sum, ?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Remove an MR from the job.
    %%-------------------------------------------------------------
    ok = delete_mr(Job, ?ROPType3Min),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, MTs2),

    Values2  = get_pmi_values_multi(MTs2, LDNs, [1221]),
    RopData2 = lists:zip(Values2, AppsData),

    %%-------------------------------------------------------------
    %% Removing the MR should be immediately reflected in the
    %% ROP file.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData2),
    ok = check_rop_file(2, [?ROPGroup1, ?ROPType1Sum], [?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Delete the job.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete both apps.
    %%-------------------------------------------------------------
    ok = delete_apps(AppsData).


%%========================================================================
%% add_pm_group(Config) -> ok.
%%
%% @doc
%% - Create two apps
%% - Create a job with one pmGroup
%% - Subsequently add another pmGroup
%% @end
%%========================================================================
add_pm_group(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    Apps     = [?SUNE, ?TINA],
    NoOfApps = length(Apps),
    AppAttrs = [{expected, [relay]}],

    GP       = ?GP_10_SEC,
    Job      = "two_apps_add_pm_group_pmi",

    MR1      = "MR1",
    MR2      = "MR2",

    Group1   = ?ROPGroup1,
    Group2   = ?ROPGroup2,
    Groups   = [Group1, Group2],

    MTs1     = get_MTs_for_group(Group1),
    MTs2     = get_MTs_for_group(Group2),
    BothMTs  = MTs1 ++ MTs2,

    MTs1Vals = [11, 12, 13, 14, 15],
    MTs2Vals = [21, 22, 23, 24, 25],
    BothVals = [MTs1Vals, MTs2Vals],

    %%-------------------------------------------------------------
    %% Create both apps.
    %%-------------------------------------------------------------
    {AppsData, LDNs} = create_apps_pmi(Apps, AppAttrs, GP, Groups),

    %%-------------------------------------------------------------
    %% Start job and wait for subscribes for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job, {MR1, Group1}),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, MTs1),

    Values1  = get_pmi_values_multi(MTs1, LDNs, MTs1Vals),
    RopData1 = lists:zip(Values1, AppsData),

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (first complete) RP, that should
    %% generate the first ROP file, with data for first pmGroup.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    ok = check_rop_file(1, [Group1], [Group2]),

    %%-------------------------------------------------------------
    %% Add new pmGroup to the job.
    %%-------------------------------------------------------------
    ok = create_job(Job, {MR2, Group2}),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, BothMTs),

    Values2  = get_pmi_values_multi(BothMTs, LDNs, BothVals),
    RopData2 = lists:zip(Values2, AppsData),

    %%-------------------------------------------------------------
    %% After another incomplete RP, new ROP file still
    %% shouldn't contain any new data.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData2),
    ok = check_rop_file(2, [Group1], [Group2]),

    %%-------------------------------------------------------------
    %% Newly added group should be visible in the 3rd ROP file
    %% (after a complete RP).
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData2),
    ok = check_rop_file(3, [Group1, Group2]),

    %%-------------------------------------------------------------
    %% Delete the job.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete both apps.
    %%-------------------------------------------------------------
    ok = delete_apps(AppsData).


%%========================================================================
%% rm_pm_group(Config) -> ok.
%%
%% @doc
%% - Create two apps
%% - Create a job with two pmGroups
%% - Subsequently remove one pmGroup
%% @end
%%========================================================================
rm_pm_group(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    Apps     = [?SUNE, ?TINA],
    NoOfApps = length(Apps),
    AppAttrs = [{expected, [relay]}],

    GP       = ?GP_10_SEC,
    Job      = "two_apps_add_pm_group_pmi",

    MR1      = "MR1",
    MR2      = "MR2",

    Group1   = ?ROPGroup1,
    Group2   = ?ROPGroup2,
    Groups   = [Group1, Group2],

    MTs1     = get_MTs_for_group(Group1),
    MTs2     = get_MTs_for_group(Group2),
    BothMTs  = MTs1 ++ MTs2,

    MTs1Vals = [11, 12, 13, 14, 15],
    MTs2Vals = [21, 22, 23, 24, 25],
    BothVals = [MTs1Vals, MTs2Vals],

    %%-------------------------------------------------------------
    %% Create both apps.
    %%-------------------------------------------------------------
    {AppsData, LDNs} = create_apps_pmi(Apps, AppAttrs, GP, Groups),

    %%-------------------------------------------------------------
    %% Start a job with two pmGroups and wait for
    %% subscribes for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job, [{MR1, Group1},
                          {MR2, Group2}]),

    ok = wait_pmi_subscribe_multi(NoOfApps, GP, BothMTs),

    Values1  = get_pmi_values_multi(BothMTs, LDNs, BothVals),
    RopData1 = lists:zip(Values1, AppsData),

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (first complete) RP, that should
    %% generate the first ROP file, with data for first pmGroup.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData1),
    ok = check_rop_file(1, [Group1, Group2]),

    %%-------------------------------------------------------------
    %% Remove the second pmGroup from the job.
    %%-------------------------------------------------------------
    ok = delete_mr(Job, MR2),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, MTs1),

    Values2  = get_pmi_values_multi(MTs1, LDNs, MTs1Vals),
    RopData2 = lists:zip(Values2, AppsData),

    %%-------------------------------------------------------------
    %% Even after an incomplete RP, the lack of the 2nd group
    %% should already be reflected in the 2nd ROP file.
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop_multi(RopData2),
    ok = check_rop_file(2, [Group1], [Group2]),

    %%-------------------------------------------------------------
    %% Delete the job.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_pmi_subscribe_multi(NoOfApps, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete the apps.
    %%-------------------------------------------------------------
    ok = delete_apps(AppsData).


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% get_MTs_for_group(Group) -> [{Group, MTs}]
%%
%% MTs = [integer()]
%%
%% Gets measurement types specification for the given group.
%%========================================================================
get_MTs_for_group(Group) -> 
    UnsortedMTs = ?ALL_MR(Group),
    MTs = lists:sort(UnsortedMTs),
    [{Group, MTs}].

%%========================================================================
%% get_groups_from_MTs([{Groups, MTs}]) -> [Groups]
%%
%% Gets unique groups from the provided MT specifications.
%%========================================================================
get_groups_from_MTs(MTs) ->
    GetGroupFromMT = fun({Group, _}) -> Group end,
    AllGroups = lists:map(GetGroupFromMT, MTs),
    lists:usort(AllGroups).


%%========================================================================
%% create_apps_pmi(AppNames, Attrs, GP, Groups) -> [{Handle, LDN}]
%%
%% AppNames = [String()]
%% Attrs = [{expected, [relay]}]
%%
%% Create multiple apps, with the same parameters
%% (App attributes, groups, GP...).
%%========================================================================
create_apps_pmi(AppNames, Attrs, GP, Groups) ->
    AppDefs = [{AppName, Attrs, GP, Groups} || AppName <- AppNames],
    Results = lists:map(fun create_apps_pmi/1, AppDefs),

    lists:unzip(Results).


create_apps_pmi({AppName, Attrs, GP, Groups}) ->
    {ok, Handle} = create_app_pmi({AppName, Attrs}, Groups),
    [LDN]        = ?LDN_DEF_1(AppName),
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
wait_pmi_subscribe_multi(NoOfApps, GP, MTs) ->
    wait_pmi_subscribe_multi(NoOfApps, GP, MTs, false).

wait_pmi_subscribe_multi(NoOfApps, GP, MTs, Lazy) ->
    GetData = fun(_AppIndex) ->
        {GP, MTs}
    end,

    AppIndexes = lists:seq(1, NoOfApps),
    Data = lists:map(GetData, AppIndexes),

    case Lazy of
        true -> wait_until_pmi_subscribe(Data);
           _ -> wait_pmi_subscribe(Data)
    end.

%%========================================================================
%% wait_report_rop_multi([{AppValues, AppData}]) -> ok | [{error, description}]
%%
%% Wait for ROP to generated if multiple applications are used.
%%========================================================================
wait_pmi_report_rop_multi(AppInfos) ->
    WaitPmiReportRop = fun({AppValues, AppData}) ->
        wait_pmi_report_rop(AppValues, AppData)
    end,

    Res = lists:map(WaitPmiReportRop, AppInfos),
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
%% all_ok([X]) -> ok | [X]
%% Checks whether all the items in the provided list are ok. If not,
%% return that same list.
%%========================================================================
all_ok(Xs) ->
    Res = lists:all(fun (X) -> ok == X end, Xs),
    case Res of
        true -> ok;
           _ -> Xs
    end.

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
    Zipped = lists:zip(MTs, MVals),
    Joined = [{RG, Zipped}],
    [{LDN, Joined}];

get_pmi_values(MRSpecs, LDN, Vals) ->
    GetEachValue = fun(MRSpec, Values) ->
        get_pmi_values([MRSpec], LDN, Values)
    end,

    Res = lists:zipwith(GetEachValue, MRSpecs, Vals),
    lists:flatten(Res).


get_pmi_values_multi(MTs, LDNs, Vals) ->
    GetPmiValues = fun(LDN) ->
        get_pmi_values(MTs, LDN, Vals)
    end,

    lists:map(GetPmiValues, LDNs).


%%% #---------------------------------------------------------
%%% #3.4   TESTING LIBRARY MAPPINGS
%%% #---------------------------------------------------------

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


%% log_rop_files(Config) ->
%%     {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
%%     log_rop_files(RopData, Config).
%%
%% log_rop_files(RopData, Config) ->
%%     pms_test_lib:log_rop_files(RopData, Config).

get_ct_config(TC)         -> ?LIB:get_ct_config(TC).
%%get_ct_config(TC, DefVal) -> ?LIB:get_ct_config(TC, DefVal).

%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).
cleanup() -> ?LIB:cleanup().

%%initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%%finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%%counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%%get_aliases()        -> ?LIB:get_aliases().

%%create_app(A)       -> ?LIB:create_app(A).
%%create_app(A, B)    -> ?LIB:create_app(A, B).
%%create_app(A, B, C) -> ?LIB:create_app(A, B, C).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
create_job(A, B)    -> ?LIB:create_job(A, B).
%%create_job(A, B, C) -> ?LIB:create_job(A, B, C).
%%create_jobs(A)      -> ?LIB:create_jobs(A).

%%create_app_pmi(A)       -> ?LIB:create_app_pmi(A).
create_app_pmi(A, B)    -> ?LIB:create_app_pmi(A, B).
%%create_app_pmi(A, B, C) -> ?LIB:create_app_pmi(A, B, C).
%%delete_app_pmi(A, B)    -> ?LIB:delete_app_pmi(A, B).


%%create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
%%delete_job(A)       -> ?LIB:delete_job(A).

%create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)       -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
%%tables()           -> ?LIB:tables().
%%check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

%%rop_data({A, B, C}, D, E, F) -> ?LIB:rop_data(A, B, C, D, E, F).


%%expected(A, B, C)               -> ?LIB:expected(A, B, C).
%%wait_expected_result(A)         -> ?LIB:wait_expected_result(A, []).
%%wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B, 20000).

%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%%open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

check_rop_file(A, B)     -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)  -> ?LIB:check_rop_file(A, B, C).
%%wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
%%get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
wait_pmi_subscribe(A)        -> ?LIB:wait_pmi_subscribe(A).
wait_until_pmi_subscribe(A)  -> ?LIB:wait_until_pmi_subscribe(A).
%%wait_pmi_report_rop(A, B, C) -> ?LIB:wait_pmi_report_rop(A, B, C).
wait_pmi_report_rop(A, B)    -> ?LIB:wait_pmi_report_rop(A, B).
%%get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
%%aliasify_values(A)       -> ?LIB:aliasify_values(A).

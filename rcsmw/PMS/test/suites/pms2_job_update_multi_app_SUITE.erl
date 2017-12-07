%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_job_update_multi_app_SUITE.erl %
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R6A/1

%%% @doc
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms2_job_update_multi_app_SUITE).

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
%%% R5F/1      2016-02-11 edamkon     Copied from pms_job_update_SUITE
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

-define(LIB,      pms2_test_lib).
-define(ERL_APP,  pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).

-define(JOB_1, "job_1").
-define(JOB_2, "job_2").

-define(COMMON_JOB_GROUP, undefined).
-define(JOB_GROUP1,       "Group1").
-define(JOB_GROUP2,       "Group2").

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
%% Create two apps that consume a job with a single counter.
%% Afterwards, add a second counter to the same job.
%% @end
%%========================================================================
add_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App1 = ?SUNE,
    App2 = ?TINA,
    AppAttrs = [{expected, [relay]}],

    Job = "two_apps_add_mr",

    GP  = ?GP_10_SEC,

    MTs1 = [{?Group2, [?Type2]}],
    MTs2 = [{?Group2, [?Type2, ?Type3]}],

    %%-------------------------------------------------------------
    %% Create both apps
    %%-------------------------------------------------------------
    [App1Info, App2Info] = create_apps([App1, App2], AppAttrs, GP),
    {App1Data, App1LDN}  = App1Info,
    {App2Data, App2LDN}  = App2Info,

    %%-------------------------------------------------------------
    %% Start job and wait for subscribes for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs1)),
    ok = wait_subscribe_multi(2, GP, MTs1),

    [App1Values, App2Values] = get_values(
        MTs1,
        [App1LDN, App2LDN],
        [{1221}]
    ),

    RopData1 = [
        {App1Values, App1Data},
        {App2Values, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (first complete) RP, that should
    %% generate the first ROP file.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    ok = check_rop_file(1, [?Group2, ?Type2], [?Type3]),

    %%-------------------------------------------------------------
    %% Add another MR to the job.
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs2)),
    ok = wait_subscribe_multi(2, GP, MTs2),

    [App1Values2, App2Values2] = get_values(
        MTs2,
        [App1LDN, App2LDN],
        [{1221, 1222}]
    ),

    RopData2 = [
        {App1Values2, App1Data},
        {App2Values2, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Wait for another incomplete RP, only ROP file with
    %% old counter should be created.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(2, [?Group2, ?Type2], [?Type3]),

    %%-------------------------------------------------------------
    %% Wait for another complete RP, this time ROP file with the
    %% second counter should be generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(3, [?Group2, ?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% Delete jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_subscribe_multi(2, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete apps.
    %%-------------------------------------------------------------
    ok = delete_apps([App1Data, App2Data]).

%%========================================================================
%% rm_mr(Config) -> ok.
%%
%% @doc
%% Create two apps that consume a job with a two counters.
%% Afterwards, remove one if the counters.
%% @end
%%========================================================================
rm_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App1 = ?SUNE,
    App2 = ?TINA,
    AppAttrs = [{expected, [relay]}],

    Job = "two_apps_rm_mr",

    GP  = ?GP_10_SEC,

    MTs1 = [{?Group2, [?Type2, ?Type3]}],
    MTs2 = [{?Group2, [?Type2]}],

    %%-------------------------------------------------------------
    %% Create both apps
    %%-------------------------------------------------------------
    [App1Info, App2Info] = create_apps([App1, App2], AppAttrs, GP),
    {App1Data, App1LDN}  = App1Info,
    {App2Data, App2LDN}  = App2Info,

    %%-------------------------------------------------------------
    %% Start job and wait for subscribes for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs1)),
    ok = wait_subscribe_multi(2, GP, MTs1),

    [App1Values, App2Values] = get_values(
        MTs1,
        [App1LDN, App2LDN],
        [{1221, 1223}]
    ),

    RopData1 = [
        {App1Values, App1Data},
        {App2Values, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (first complete) RP, that should
    %% generate the first ROP file.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    ok = check_rop_file(1, [?Group2, ?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% Remove MR from the job.
    %%-------------------------------------------------------------
    ok = delete_mr(Job, ?Type3),
    ok = wait_subscribe_multi(2, GP, MTs2),

    [App1Values2, App2Values2] = get_values(
        MTs2,
        [App1LDN, App2LDN],
        [{1221}]
    ),

    RopData2 = [
        {App1Values2, App1Data},
        {App2Values2, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Removing the MR should be immidiately reflected in the
    %% ROP file.
    %%-------------------------------------------------------------
    ok = rm_mr_no_type3(RopData2, 2),

    %%-------------------------------------------------------------
    %% Delete jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_subscribe_multi(2, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete apps.
    %%-------------------------------------------------------------
    ok = delete_apps([App1Data, App2Data]).

rm_mr_no_type3(RopData2, 1) ->
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    check_rop_file(2, [?Group2, ?Type2], [?Type3]);
rm_mr_no_type3(RopData2, N) ->
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    case check_rop_file(2, [?Group2, ?Type2], [?Type3]) of
	ok -> ok;
	_  -> rm_mr_no_type3(RopData2, N - 1)
    end.
	    

    



%%========================================================================
%% add_pm_group(Config) -> ok.
%%
%% @doc
%% - Create two apps (each with two groups)
%% - Create a job with pmGroup1
%% - Subsequently add pmGroup2
%% @end
%%========================================================================
add_pm_group(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App1 = ?SUNE,
    App2 = ?TINA,
    GP   = ?GP_10_SEC,
    Job  = "two_apps_add_pm_group",

    AppAttrs  = [{expected, [relay]}],
    %%AppGroups = ?CM_GRP_1 ++ ?CM_GRP_2,

    %% Need 2 MRs because updating an existing
    %% one later on is not allowed.
    MR1 = "ExampleMeasurementReader1",
    MR2 = "ExampleMeasurementReader2",

    %%-------------------------------------------------------------
    %% Create both apps
    %%-------------------------------------------------------------
    [App1Info, App2Info] = create_apps([App1, App2], AppAttrs, GP),
    {App1Data, App1LDN}  = App1Info,
    {App2Data, App2LDN}  = App2Info,

    %%-------------------------------------------------------------
    %% Start job and wait for subscribes for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job, {MR1, ?Group1}),
    ok = wait_subscribe_multi(2, GP, [?Group1]),

    [App1Values1, App2Values1] = get_values(
        [{?Group1, [?Type1]}],
        [App1LDN, App2LDN],
        [{1221}]
    ),

    RopData1 = [
        {App1Values1, App1Data},
        {App2Values1, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (first complete) RP, that should
    %% generate the first ROP file.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    ok = check_rop_file(1, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Add another MR to the job.
    %%-------------------------------------------------------------
    ok = create_job(Job, {MR2, ?Group2}),
    ok = wait_subscribe_multi(2, GP, [?Group1, ?Group2]),

    [App1Values2, App2Values2] = get_values(
        [{?Group1, [?Type1]},
         {?Group2, [?Type2, ?Type3]}],
        [App1LDN, App2LDN],
        [{1222},
         {1223, 1224}]
    ),

    RopData2 = [
        {App1Values2, App1Data},
        {App2Values2, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Wait for another incomplete RP, only ROP file with
    %% old group data should be created.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(2, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Wait for another complete RP, this time ROP file with the
    %% second group data should be generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(3, [?Group1, ?Group2, ?Type1, ?Type2]),

    %%-------------------------------------------------------------
    %% Delete jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_subscribe_multi(2, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete apps.
    %%-------------------------------------------------------------
    ok = delete_apps([App1Data, App2Data]).


%%========================================================================
%% rm_pm_group(Config) -> ok.
%%
%% @doc
%% - Create two apps (each with two groups)
%% - Create 1 job with pmGroup1 and pmGroup2
%% - Subsequently remove pmGroup2
%% @end
%%========================================================================
rm_pm_group(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App1 = ?SUNE,
    App2 = ?TINA,
    GP   = ?GP_10_SEC,
    Job  = "two_apps_add_pm_group",

    AppAttrs  = [{expected, [relay]}],
    %%AppGroups = ?CM_GRP_1 ++ ?CM_GRP_2,

    %% Need 2 MRs because updating an existing
    %% one later on is not allowed.
    MR1 = "ExampleMeasurementReader1",
    MR2 = "ExampleMeasurementReader2",

    %%-------------------------------------------------------------
    %% Create both apps
    %%-------------------------------------------------------------
    [App1Info, App2Info] = create_apps([App1, App2], AppAttrs, GP),
    {App1Data, App1LDN}  = App1Info,
    {App2Data, App2LDN}  = App2Info,

    %%-------------------------------------------------------------
    %% Start job with two groups and wait for subscribes
    %% for both apps.
    %%-------------------------------------------------------------
    ok = create_job(Job,
            [{MR1, ?Group1},
             {MR2, ?Group2}]),

    ok = wait_subscribe_multi(2, GP, [?Group1, ?Group2]),

    [App1Values1, App2Values1] = get_values(
        [{?Group1, [?Type1]},
         {?Group2, [?Type2, ?Type3]}],
        [App1LDN, App2LDN],
        [{1222},
         {1223, 1224}]
    ),

    RopData1 = [
        {App1Values1, App1Data},
        {App2Values1, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Wait for the first (incomplete) RP, no ROP files
    %% should be generated.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    {ok, []} = ?ROP_HOOK:list_rop_files(),

    %%-------------------------------------------------------------
    %% Wait for the second (first complete) RP, that should
    %% generate the first ROP file.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData1),
    ok = check_rop_file(1, [?Group1, ?Type1, ?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Remove the pmGroup2 from the job (that is, the MR2 that
    %% contains the pmGroup2).
    %%-------------------------------------------------------------
    ok = delete_mr(Job, MR2),
    ok = wait_subscribe_multi(2, GP, [?Group1], true),

    [App1Values2, App2Values2] = get_values(
        [{?Group1, [?Type1]}],
        [App1LDN, App2LDN],
        [{1222}]
    ),

    RopData2 = [
        {App1Values2, App1Data},
        {App2Values2, App2Data}
    ],

    %%-------------------------------------------------------------
    %% Wait for another incomplete RP, but ROP file should not
    %% contained removed pmGroup2 anymore, since removing MRs
    %% should be handled immidiately, instead after a full RP.
    %%-------------------------------------------------------------
    ok = wait_report_rop_multi(?FF_TRUE, RopData2),
    ok = check_rop_file(2, [?Group1, ?Type1], [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% Delete jobs.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_subscribe_multi(2, GP, [], true),

    %%-------------------------------------------------------------
    %% Delete apps.
    %%-------------------------------------------------------------
    ok = delete_apps([App1Data, App2Data]).


%%========================================================================
%% Misc functions
%%========================================================================

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
    wait_subscribe_multi(NoOfApps, GP, MTs, true).

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


%%========================================================================
%% to(Int) -> ok.
%% Defines timeout.
%%========================================================================
%%to() ->
%%    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.

%%========================================================================
%% Bridging to testing library.
%%========================================================================
get_ct_config(TC) -> ?LIB:get_ct_config(TC).

%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).
cleanup() -> ?LIB:cleanup().

%% initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%% finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%% counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%% get_aliases()        -> ?LIB:get_aliases().

create_app(A)             -> ?LIB:create_app(A).
%%create_app(A, B)          -> ?LIB:create_app(A, B).
%%create_app_pmi(A)       -> ?LIB:create_app_pmi(A).
delete_app(A, B)          -> ?LIB:delete_app(A, B).
%%delete_app_pmi(A, B)    -> ?LIB:delete_app_pmi(A, B).
create_job(A, B)          -> ?LIB:create_job(A, B).
%%create_job(A, B, C)     -> ?LIB:create_job(A, B, C).
%%create_jobs(A)          -> ?LIB:create_jobs(A).
delete_jobs(A)            -> ?LIB:delete_jobs(A).
%%update_job(A, B)         -> ?LIB:update_job(A, B).
%%delete_job(A)           -> ?LIB:delete_job(A).

%%create_job_mr(A, B, C)  -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)           -> ?LIB:delete_mr(A, B).

%%get_table(A)             -> ?LIB:get_table(A).
%%tables()                 -> ?LIB:tables().
%%check_tables(A)          -> ?LIB:check_tables(A).
%%check_processes(A)       -> ?LIB:check_processes(A).

rpc(A, B, C)               -> ?LIB:rpc(A, B, C).

%%expected(A, B, C)               -> ?LIB:expected(A, B, C).
%%wait_expected_result(A)         -> ?LIB:wait_expected_result(A).
%%wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B).
%%wait_expected_error_result(A,B) -> ?LIB:wait_expected_error_result(A, B).
%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%%open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

%%log_rop_files(Config) ->
%%    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
%%    log_rop_files(RopData, Config).
%%
%%log_rop_files(RopData, Config) -> pms_test_lib:log_rop_files(RopData, Config).


%%check_rop_file_name(A, B)    -> ?LIB:check_rop_file_name(A, B).
%%check_rop_file_name(A, B, C) -> ?LIB:check_rop_file_name(A, B, C).
check_rop_file(A, B)         -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)      -> ?LIB:check_rop_file(A, B, C).
%%wait_one_gp(A)             -> ?LIB:wait_one_gp(A).
%%get_noof_rop_files()       -> ?LIB:get_noof_rop_files().
wait_subscribe(A)            -> ?LIB:wait_subscribe(A).
wait_until_subscribe(A)      -> ?LIB:wait_until_subscribe(A).
wait_report_rop(A, B, C)     -> ?LIB:wait_report_rop(A, B, C).
get_subscribe_data(A, B)     -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)                   -> ?LIB:get_mrs(A).
aliasify_values(A)           -> ?LIB:aliasify_values(A).

%%rop_file_handling(A)         -> ?LIB:rop_file_handling(A).

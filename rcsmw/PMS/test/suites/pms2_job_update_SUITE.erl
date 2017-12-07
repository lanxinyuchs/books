%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_job_update_SUITE.erl %
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R5A/20

%%% @doc
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms2_job_update_SUITE).

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
%%% R3A/1      2014-09-04 uabesvi     Copied from pms_job_ext_SUITE
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


-export([add_mr/1]).
-export([rm_mr/1]).
-export([rm_last_mr/1]).
-export([takeover/1]).
-export([takeover_two_jobs/1]).
-export([multi_takeover/1]).
-export([takeover_common_group/1]).
-export([add_mr_pmi/1]).


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
-define(JOB_GROUP_GRAT,   "GRAT").
-define(JOB_GROUP_WRAT,   "WRAT").

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
       rm_last_mr,
       takeover,
       multi_takeover,
       takeover_two_jobs,
       takeover_common_group
    ],
    All ++ all_host(host()).

all_host(?TESTNODE) ->
    [];
all_host(_PmsSim) ->
    [
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
%% add_mr(Config) -> ok.
%%
%% @doc
%%
%% @end
%%========================================================================
add_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "add_mr",
    GP  = ?GP_10_SEC,
    MTs  = [{?Group2, [?Type2]}],
    MTs2 = [{?Group2, [?Type2, ?Type3]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain both group and type
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group2, ?Type2], [?Type3]),

    %%-------------------------------------------------------------
    %% Add MR to the job
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs2)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs2)),

    Values2 = get_values(MTs2, LDN, [{1221, 1222}]),
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    %% ok = check_rop_file(2, [?Group2, ?Type2, ?Type3]),
    %% it may happen that the new MR is not created before
    %% the 2nd ROP file is genereated.
    %% In that case skip testing the 2nd ROP and wait for the 3rd
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    ok = check_rop_file(3, [?Group2, ?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).



%%========================================================================
%% rm_mr(Config) -> ok.
%%
%% @doc
%%
%% @end
%%========================================================================
rm_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "add_mr",
    GP  = ?GP_10_SEC,
    MTs  = [{?Group2, [?Type2, ?Type3]}],
    MTs2 = [{?Group2, [?Type2]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain group and both types
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221, 1223}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group2, ?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% remove one MR from the job
    %%-------------------------------------------------------------
    ok = delete_mr(Job, ?Type3),
    ok = wait_until_subscribe(get_subscribe_data(GP, MTs2)),

    Values2 = get_values(MTs2, LDN, [{1221}]),
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    ok = check_rop_file(2, [?Group2, ?Type2], [?Type3]),
    %% It may happen that the new MR is not created before
    %% the 2nd ROP file is genereated.
    %% In that case skip testing the 2nd ROP and wait for the 3rd
%%     ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
%%     ok = check_rop_file(3, [?Group2, ?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% rm_last_mr(Config) -> ok.
%%
%% @doc
%%
%% @end
%%========================================================================
rm_last_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "remove_last_mr",
    GP  = ?GP_10_SEC,
    MTs  = [{?Group2, [?Type2]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain one group and one type
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group2, ?Type2]),

    %%-------------------------------------------------------------
    %% remove last MR from the job, this should not be allowed
    %%-------------------------------------------------------------
    {error, _} = delete_mr(Job, ?Type2),


    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% takeover(Config) -> ok.
%%
%% @doc
%%
%% @end
%%========================================================================
takeover(_Config) ->
    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),

    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "add_mr",
    GP  = ?GP_10_SEC,
    MTs  = [{?Group2, [?Type2]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain both group and type
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group2, ?Type2], [?Type3]),

    %%-------------------------------------------------------------
    %% Move the job to another jobGroup
    %%-------------------------------------------------------------
    ok = update_job(Job, [{"jobGroup", "takeover"}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(2, [?Group2, ?Type2], [?Type3]),
    ok = check_rop_file_name(2, ["takeover"]),
    %% it may happen that the new MR is not created before
    %% the 2nd ROP file is genereated.
    %% In that case skip testing the 2nd ROP and wait for the 3rd
%%     ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
%%     ok = check_rop_file(3, [?Group2, ?Type2, ?Type3]),
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle),

    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).

%%========================================================================
%% add_mr_pmi(Config) -> ok.
%%
%% @doc
%%
%% @end
%%========================================================================
add_mr_pmi(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "add_mr_pmi",
    GP  = ?GP_10_SEC,
    MTs  = [{?Group2, [?Type2]}],
    MTs2 = [{?Group2, [?Type2, ?Type3]}],
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app_pmi({App, [{expected, [relay]}]}),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain both group and type
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group2, ?Type2], [?Type3]),

    %%-------------------------------------------------------------
    %% Add MR to the job
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs2)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs2)),

    Values2 = get_values(MTs2, LDN, [{1221, 1222}]),
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    %% ok = check_rop_file(2, [?Group2, ?Type2, ?Type3]),
    %% it may happen that the new MR is not created before
    %% the 2nd ROP file is genereated.
    %% In that case skip testing the 2nd ROP and wait for the 3rd
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData),
    ok = check_rop_file(3, [?Group2, ?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app_pmi(App, Handle).


%%========================================================================
%% takeover_two_jobs(Config) -> ok.
%%
%% @doc
%% Creates two jobs in the same JobGroup. Subsequently
%% moves one job into another group.
%% @end
%%========================================================================
takeover_two_jobs(_Config) ->
    App = ?SUNE,
    GP  = ?GP_10_SEC,
    MTs = [{?Group1, [?Type1]}],

    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),

    %%-------------------------------------------------------------
    %% Create app and two jobs in the same group
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),

    create_jobs_in_groups([
        {?JOB_GROUP_GRAT, ?JOB_1},
        {?JOB_GROUP_GRAT, ?JOB_2}
    ]),

    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain both group and type
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData   = {App, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% Check if the ROP file was created
    %%-------------------------------------------------------------
    ok = check_rop_file(1, [?Group1, ?Type1], [?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% Move the second job to another jobGroup
    %%-------------------------------------------------------------
    ok = update_job(?JOB_2, [{"jobGroup", ?JOB_GROUP_WRAT}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(5, [], ["suspect"]),

    %%-------------------------------------------------------------
    %% Check if the last ROP file belongs to the second group
    %%-------------------------------------------------------------
    {ok, Rops} = ?ROP_HOOK:list_rop_files(),
    LastRopIndex = length(Rops),

    ok = check_rop_file_name(LastRopIndex, [?JOB_GROUP_WRAT]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([?JOB_1, ?JOB_2]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle),

    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).


%%========================================================================
%% multi_takeover(Config) -> ok.
%%
%% @doc
%% Do multi takovers
%% @end
%%========================================================================
multi_takeover(_Config) ->
    App = ?SUNE,
    GP  = ?GP_10_SEC,
    MTs = [{?Group1, [?Type1]}],

    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),

    %%-------------------------------------------------------------
    %% Create app and two jobs in the same group
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),

    create_jobs_in_groups([{?JOB_GROUP_GRAT, ?JOB_1},
                           {?JOB_GROUP_GRAT, ?JOB_2}]),

    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain both group and type
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData   = {App, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% Check if the ROP file was created
    %%-------------------------------------------------------------
    ok = check_rop_file(1, [?Group1, ?Type1], [?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% Move the second job around and eventually to jobGroup2
    %%-------------------------------------------------------------
    ok = update_job(?JOB_2, [{"jobGroup", "kalle"}]),
    ok = update_job(?JOB_2, [{"jobGroup", ?DELETE_ATTR}]),
    ok = update_job(?JOB_2, [{"jobGroup", ?JOB_GROUP_WRAT}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    ok = check_rop_file(5, [], ["suspect"]),

    %%-------------------------------------------------------------
    %% Check if the last ROP file belongs to the second group
    %%-------------------------------------------------------------
    {ok, Rops} = ?ROP_HOOK:list_rop_files(),
    LastRopIndex = length(Rops),

    ok = check_rop_file_name(LastRopIndex, [?JOB_GROUP_WRAT]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([?JOB_1, ?JOB_2]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle),

    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).



%%========================================================================
%% takeover_common_group(Config) -> ok.
%%
%% @doc
%% Creates two jobs in the same JobGroup. Subsequently
%% moves one job into the common group.
%% @end
%%========================================================================
takeover_common_group(_Config) ->
    App = ?SUNE,
    GP  = ?GP_10_SEC,
    MTs = [{?Group1, [?Type1]}],

    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),

    %%-------------------------------------------------------------
    %% Create app and two jobs in the same group
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),

    create_jobs_in_groups([
        {?JOB_GROUP_GRAT, ?JOB_1},
        {?JOB_GROUP_GRAT, ?JOB_2}
    ]),

    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Check ROP, it should contain both group and type
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData   = {App, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% Check if the ROP file was created
    %%-------------------------------------------------------------
    ok = check_rop_file(1, [?Group1, ?Type1], [?Type2, ?Type3]),

    %%-------------------------------------------------------------
    %% Move the second job to the common jobGroup. This is
    %% accomplished by deleting the jobGroup "attribute" of
    %% the said job.
    %%-------------------------------------------------------------
    ok = update_job(?JOB_2, [{"jobGroup", ?DELETE_ATTR}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    %%ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% Wait for the ROP file for the common
    %% JobGroup to be generated.
    %%-------------------------------------------------------------
    ok = check_rop_file(3, [], ["suspect"]),

    %%-------------------------------------------------------------
    %% Check if the last batch of ROPs contains file that
    %% was made by the common jobGroup.
    %%-------------------------------------------------------------
    GroupedRops = get_rops_grouped_by_timestamps(),
    LastRopGroup = lists:last(GroupedRops),

    IsMadeByCommonGroup = fun(Filename) ->
        string:str(Filename, ?JOB_GROUP_GRAT) == 0
    end,

    true = lists:any(IsMadeByCommonGroup, LastRopGroup),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([?JOB_1, ?JOB_2]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle),

    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% Create N jobs with their respective groups.
%%========================================================================
create_jobs_in_groups(JobGroups) ->
    MaxIndex = length(JobGroups),
    Indexes = lists:seq(1, MaxIndex),
    JobGroupsWithIndexes = lists:zip(Indexes, JobGroups),
    JobsList = lists:map(fun setup_single_job/1, JobGroupsWithIndexes),

    ok = create_jobs(JobsList),

    JobsList.

%%========================================================================
%% Create a single job that will be a part of the list.
%%========================================================================
setup_single_job({Index, {JobGroup, Job}}) ->
    Mr = Job ++ "_mr" ++ integer_to_list(Index),

    Attrs = [
        {"reportingPeriod",   ?TEN_SECONDS},
        {"granularityPeriod", ?TEN_SECONDS},
        {"jobGroup",          JobGroup}
    ],

    {Job, [{Mr, ?Group1}], Attrs}.

%%========================================================================
%% misc functions
%%========================================================================
%%========================================================================
%% get_values(MTs, LDN, Values) -> AliasValues
%%
%% Create values to be reported in the next rop data request
%%========================================================================
get_values([{?Group1, [?Type1]}], LDN, [{Val}]) ->
    Vals = [{?Group1, [{LDN, [{?Type1, [Val]}]}]}],
    aliasify_values(Vals);

get_values([{?Group2, [?Type2]}], LDN, [{Val}]) ->
    Vals = [{?Group2, [{LDN, [{?Type2, [Val]}]}]}],
    aliasify_values(Vals);

get_values([{?Group2, [?Type2, ?Type3]}], LDN, [{Val2, Val3}]) ->
    Vals = [{?Group2, [{LDN, [{?Type2, [Val2]}, {?Type3, [Val3]}]}]}],
    aliasify_values(Vals).



%%========================================================================
%% getRopsGroupedByTimestamps() -> [Indexes]
%%
%% Gets the indexes of ROP files that were created in the
%% same RP, but only if enough of them are found (as
%% specified by NoOfFiles argument).
%%========================================================================
get_rops_grouped_by_timestamps() ->
    {ok, Rops} = ?ROP_HOOK:list_rop_files(),

    GetTimestamp = fun(Rop) ->
        [_, Timestamp | _] = string:tokens(Rop, "._"),
        Timestamp
    end,

    CompareByTimestamp = fun(Rop1, Rop2) ->
        GetTimestamp(Rop1) == GetTimestamp(Rop2)
    end,

    group_by(CompareByTimestamp, Rops).


%%========================================================================
%% group_by(Fun, SortedList) -> GroupedList
%%
%% Gets a 2D list containing the sublists of grouped items from the
%% provided sorted list, grouped according to the provided fun.
%%========================================================================
group_by(F, [H | T]) ->
    group_by(F, T, [[H]]).

group_by(_, [], Grouped) ->
    EachReversed = lists:map(fun lists:reverse/1, Grouped),
    lists:reverse(EachReversed);

%% GH = Grouped head
%% GT = Grouped tail
%% CE = Comparison element of the last group
group_by(F, [H | T], [[CE | _] = GH | GT]) ->
    case F(H, CE) of
        true  -> group_by(F, T, [[H | GH] | GT]);
        false -> group_by(F, T, [[H], GH  | GT])
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




get_ct_config(TC) -> ?LIB:get_ct_config(TC).

%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).
cleanup() -> ?LIB:cleanup().

%% initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%% finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%% counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%% get_aliases()        -> ?LIB:get_aliases().

create_app(A)       -> ?LIB:create_app(A).
%%create_app(A, B)    -> ?LIB:create_app(A, B).
create_app_pmi(A)   -> ?LIB:create_app_pmi(A).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
delete_app_pmi(A, B) -> ?LIB:delete_app_pmi(A, B).
create_job(A, B)    -> ?LIB:create_job(A, B).
%%create_job(A, B, C) -> ?LIB:create_job(A, B, C).
create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
update_job(A, B)    -> ?LIB:update_job(A, B).
%%delete_job(A)       -> ?LIB:delete_job(A).

%%create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)        -> ?LIB:delete_mr(A, B).

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
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

%%log_rop_files(Config) ->
%%    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
%%    log_rop_files(RopData, Config).

%%log_rop_files(RopData, Config) -> pms_test_lib:log_rop_files(RopData, Config).



check_rop_file_name(A, B)    -> ?LIB:check_rop_file_name(A, B).
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

rop_file_handling(A)         -> ?LIB:rop_file_handling(A).

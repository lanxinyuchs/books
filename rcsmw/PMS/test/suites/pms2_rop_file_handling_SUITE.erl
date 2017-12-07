%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_file_handling_SUITE.erl %
%%% @version /main/R5A/R6A/R12A/1

%%% @doc
%%% == Example test suite for one sim or target env ==
%%% @end

-module(pms2_rop_file_handling_SUITE).

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
%%% R5A/1      2016-01-12 uabesvi     Created
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


-export([single_to_multi/1]).
-export([multi_to_single/1]).
-export([single_to_multi_nw_me/1]).
-export([multi_to_single_nw_me/1]).
-export([jg_with_no_job_single/1]).
-export([single_two_jg_stop_acivate/1]).
-export([multi_and_max_no_rop/1]).


-define(TESTNODE, testnode).

-define(LIB,      pms2_test_lib).
-define(ERL_APP,  pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).


-define(SLEEP,  500).

-define(APP, sune).

-define(JOB_1, "job_fh_1").
-define(JOB_2, "job_fh_2").
-define(JOB_3, "job_fh_3").

-define(JOB_GROUP_COMMON, undefined).
-define(JOB_GROUP_LRAT,   "LRAT").
-define(JOB_GROUP_WRAT,   "WRAT").



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
    ProxyTag = {pms_pmi_proxy, get_ct_config(pms_pmi_proxy)},
    Hooks    = lists:keystore(pms_pmi_proxy, 1, hooks(), ProxyTag),
    CliHook = [{rct_cli, {?CLI_USER, [manual_connect]}}],
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
    rpc(pmsDb, rop_file_delete_all, []),
    cleanup(),
    Config.

%% @hidden
end_per_testcase(session_kill_app_proc, _Config) ->
    cleanup(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    %%log_rop_files(Config),
    unset_nw_me_id(),
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
	   single_to_multi,
	   multi_to_single,
	   single_to_multi_nw_me,
	   multi_to_single_nw_me,
	   single_two_jg_stop_acivate,
	   multi_and_max_no_rop
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
%% single_to_multi(Config) -> ok.
%%
%% @doc
%% Create 3 jobs with different jobGroup
%% change the file handling from single to multi
%% @end
%%========================================================================
single_to_multi(_Config) ->
    unset_nw_me_id(),
    single_to_multi().


%%========================================================================
%% single_to_multi_nw_me(Config) -> ok.
%%
%% @doc
%% Create 3 jobs with different jobGroup
%% change the file handling from single to multi
%% NetworkManagedElementId is set
%% @end
%%========================================================================
single_to_multi_nw_me(_Config) ->
    set_nw_me_id(),
    single_to_multi().




single_to_multi() ->
    MTs = [{?Group1, [?Type1]}],
    GP  = ?GP_10_SEC,
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP),
    
    %%-------------------------------------------------------------
    %% create one app and three jobs
    %%-------------------------------------------------------------
    Handle = create_app_jobs([{?JOB_GROUP_COMMON, ?JOB_1},
			      {?JOB_GROUP_LRAT,   ?JOB_2},
			      {?JOB_GROUP_WRAT,   ?JOB_3}]),
    ok     = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% wait for the common ROP file
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(?APP)),
    AppData   = {?APP, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    ok = update_job("job_fh_2", [{"requestedJobState", ?STOPPED}]),
    
    ok = update_job("job_fh_2", [{"requestedJobState", ?ACTIVE}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?JOB_1, ?JOB_3], []),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(2, [?JOB_1, ?JOB_2, ?JOB_3], []),
    

    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),
    
    %%-------------------------------------------------------------
    %% wait for one more common ROP file
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(3, [?JOB_1, ?JOB_2, ?JOB_3], []),
    
    %%-------------------------------------------------------------
    %% this RP should have 3 ROP files
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% Check which Job belongs to which ROP
    %%-------------------------------------------------------------
    MEData   = rpc(comsaI, get_managed_element_data, []),
    NwMeId = proplists:get_value(networkManagedElementId, MEData),
    
    N = get_file_names(NwMeId),

    [RopExp1, RopExp2, RopExp3] = 
	get_rop_expressions(N, [[?JOB_1], [?JOB_2], [?JOB_3]]),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    %%-------------------------------------------------------------
    %% Check which 3 ROP files are generated with the same time stamp
    %%-------------------------------------------------------------
    [Rop1, Rop2, Rop3] = get_rops_with_same_time_stamp(3),
    
    {ok, RopRef} = ?ROP_HOOK:expected([{Rop1, RopExp1, ["suspect"]},
				       {Rop2, RopExp2, ["suspect"]},
				       {Rop3, RopExp3, ["suspect"]}]),
    
    wait_expected_result([], [{RopRef, ?ROP_HOOK}]),
    
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    delete_app_jobs(Handle, [?JOB_1, ?JOB_2, ?JOB_3]),
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).



%%========================================================================
%% multi_to_single(Config) -> ok.
%%
%% @doc
%% Create 3 jobs with different jobGroup
%% change the file handling from multi to single
%% @end
%%========================================================================
multi_to_single(_Config) ->
    unset_nw_me_id(),
    multi_to_single().

%%========================================================================
%% multi_to_single_nw_me(Config) -> ok.
%%
%% @doc
%% Create 3 jobs with different jobGroup
%% change the file handling from multi to single
%% NetworkManagedElementId is set
%% @end
%%========================================================================
multi_to_single_nw_me(_Config) ->
    set_nw_me_id(),
    multi_to_single().
    


multi_to_single() ->
    MTs = [{?Group1, [?Type1]}],
    GP  = ?GP_10_SEC,

    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),

    %%-------------------------------------------------------------
    %% create one app and three jobs
    %%-------------------------------------------------------------
    Handle = create_app_jobs([{?JOB_GROUP_COMMON, ?JOB_1},
			      {?JOB_GROUP_LRAT,   ?JOB_2},
			      {?JOB_GROUP_WRAT,   ?JOB_3}]),
    ok     = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% wait 3 reporting periods
    %% and check if 3 rop files are generated in second period
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(?APP)),
    AppData   = {?APP, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% Check which Job belongs to which ROP
    %%-------------------------------------------------------------
    MEData = rpc(comsaI, get_managed_element_data, []),
    NwMeId = proplists:get_value(networkManagedElementId, MEData),
    
    N = get_file_names(NwMeId),

    [RopExp1, RopExp2, RopExp3] = 
	get_rop_expressions(N, [[?JOB_1], [?JOB_2], [?JOB_3]]),
    
    %%-------------------------------------------------------------
    %% Check which 3 ROP files are generated with the same time stamp
    %%-------------------------------------------------------------
    [Rop1, Rop2, Rop3] = get_rops_with_same_time_stamp(3),
    
    {ok, RopRef} = ?ROP_HOOK:expected([{Rop1, RopExp1, ["suspect"]},
				       {Rop2, RopExp2, ["suspect"]},
				       {Rop3, RopExp3, ["suspect"]}]),
    wait_expected_result([], [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% set PmSupport to single rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP),
    
    %%-------------------------------------------------------------
    %% wait for common ROP file
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% check the last ROP file
    %%-------------------------------------------------------------
    ok = check_rop_file(11, [?JOB_1, ?JOB_2, ?JOB_3], []),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    delete_app_jobs(Handle, [?JOB_1, ?JOB_2, ?JOB_3]),

    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).




%%========================================================================
%% jg_with_no_job_single(Config) -> ok.
%%
%% @doc
%% Create 2 jobs with different jobGroup
%% single ROP file
%% One job stopped, one job active
%% @end
%%========================================================================
jg_with_no_job_single(_Config) ->

    MTs = [{?Group1, [?Type1]}],
    GP  = ?GP_10_SEC,
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP),
    
    %%-------------------------------------------------------------
    %% create one app and two jobs
    %%-------------------------------------------------------------
    Handle = create_app_jobs([{?JOB_GROUP_LRAT, ?JOB_2},
			      {?JOB_GROUP_WRAT, ?JOB_3}]),
    ok     = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% wait for the common ROP file
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(?APP)),
    AppData   = {?APP, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?JOB_2, ?JOB_3], []),
    
    %%-------------------------------------------------------------
    %% stop job2
    %%-------------------------------------------------------------
    update_job(?JOB_2, [{"requestedJobState", ?STOPPED}]),
    
    %%-------------------------------------------------------------
    %% wait for two more common ROP file
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(2, [?JOB_3], []),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(3, [?JOB_3], []),
    
    %%-------------------------------------------------------------
    %% Check which Job belongs to which ROP
    %%-------------------------------------------------------------
%%     MEData = rpc(comsaI, get_managed_element_data, []),
%%     NwMeId = proplists:get_value(networkManagedElementId, MEData),
    
%%     N = get_file_names(NwMeId),

%%     [_RopExp2, _RopExp3] = get_rop_expressions(N, [[?JOB_2], [?JOB_3]]),
    
%%     ok = wait_report_rop(Values, ?FF_TRUE, AppData),
%%     ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    %%-------------------------------------------------------------
    %% Check which 3 ROP files are generated with the same time stamp
    %%-------------------------------------------------------------
%%     [Rop1, Rop2, Rop3] = get_rops_with_same_time_stamp(3),
    
%%     {ok, RopRef} = ?ROP_HOOK:expected([{Rop2, RopExp2, ["suspect"]},
%% 				       {Rop3, RopExp3, ["suspect"]}]),
    
%%     wait_expected_result([], [{RopRef, ?ROP_HOOK}]),
    
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    delete_app_jobs(Handle, [?JOB_2, ?JOB_3]).
    





%%========================================================================
%% single_two_jg_stop_acivate(Config) -> ok.
%%
%% @doc
%% Single mode
%% Create 2 jobs with different jobGroup
%% Stop and activate one non common job twice
%% Change to multi mode
%% @end
%%========================================================================
single_two_jg_stop_acivate(_Config) ->
    unset_nw_me_id(),

    MTs = [{?Group1, [?Type1]}],
    GP  = ?GP_10_SEC,
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP),
    
    %%-------------------------------------------------------------
    %% create one app and three jobs
    %%-------------------------------------------------------------
    Handle = create_app_jobs([{?JOB_GROUP_COMMON, ?JOB_1},
			      {?JOB_GROUP_LRAT,   ?JOB_2},
			      {?JOB_GROUP_WRAT,   ?JOB_3}]),
    ok     = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% wait for the common ROP file
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(?APP)),
    AppData   = {?APP, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    ok = update_job("job_fh_2", [{"requestedJobState", ?STOPPED}]),
    
    ok = update_job("job_fh_2", [{"requestedJobState", ?ACTIVE}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?JOB_1, ?JOB_3], []),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(2, [?JOB_1, ?JOB_2, ?JOB_3], []),
    

    ok = update_job("job_fh_2", [{"requestedJobState", ?STOPPED}]),
    
    ok = update_job("job_fh_2", [{"requestedJobState", ?ACTIVE}]),

    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(3, [?JOB_1, ?JOB_3], []),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(4, [?JOB_1, ?JOB_2, ?JOB_3], []),
    

    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),
    
    %%-------------------------------------------------------------
    %% wait for one more common ROP file
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(5, [?JOB_1, ?JOB_2, ?JOB_3], []),
    
    %%-------------------------------------------------------------
    %% this RP should have 3 ROP files
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),

    %%-------------------------------------------------------------
    %% Check which Job belongs to which ROP
    %%-------------------------------------------------------------
    MEData   = rpc(comsaI, get_managed_element_data, []),
    NwMeId = proplists:get_value(networkManagedElementId, MEData),
    
    N = get_file_names(NwMeId),

    [RopExp1, RopExp2, RopExp3] = 
	get_rop_expressions(N, [[?JOB_1], [?JOB_2], [?JOB_3]]),
    
    ct:log("Expected expressions in ROP files:~n~p~n~p~n~p", 
	   [RopExp1, RopExp2, RopExp3]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    %%-------------------------------------------------------------
    %% Check which 3 ROP files are generated with the same time stamp
    %%-------------------------------------------------------------
    [Rop1, Rop2, Rop3] = get_rops_with_same_time_stamp(3),
    %% ct:log("Check the following ROP files:~n~p~n~p~n~p", [Rop1, Rop2, Rop3]),
    
    {ok, RopRef} = ?ROP_HOOK:expected([{Rop1, RopExp1, ["suspect"]},
				       {Rop2, RopExp2, ["suspect"]},
				       {Rop3, RopExp3, ["suspect"]}]),
    
    wait_expected_result([], [{RopRef, ?ROP_HOOK}]),
    
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    delete_app_jobs(Handle, [?JOB_1, ?JOB_2, ?JOB_3]),
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).


%%========================================================================
%% multi_and_max_no_rop(Config) -> ok.
%%
%% @doc
%% Multi mode
%% Create maximum number of ROP-files
%% Create 2 jobs with different jobGroup
%% Check that old files are removed when new ROP files are created
%% @end
%%========================================================================
multi_and_max_no_rop(_Config) ->
    set_nw_me_id(),
    N = 400,
    ct:pal("Creating ~p dummy ROP files", [N]),
    create_dummy_rop_files(N),
    N = get_number_of_rop_files(),

    MTs = [{?Group1, [?Type1]}],
    GP  = ?GP_10_SEC,

    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),

    %%-------------------------------------------------------------
    %% create one app and three jobs
    %%-------------------------------------------------------------
    Handle = create_app_jobs([{?JOB_GROUP_COMMON, ?JOB_1},
			      {?JOB_GROUP_LRAT,   ?JOB_2},
			      {?JOB_GROUP_WRAT,   ?JOB_3}]),
    ok     = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% wait 3 reporting periods
    %% and check if 3 rop files are generated in second period
    %%-------------------------------------------------------------
    {ok, LDN} = rpc(gmfI, check_string, ?LDN_DEF_1(?APP)),
    AppData   = {?APP, Handle, GP},
    Values    = get_values(MTs, LDN, [{1221}]),
    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    N1 = get_number_of_rop_files(),
    ct:pal("Number of ROP files = ~p", [N1]),
    true = N >= N1,
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    N2 = get_number_of_rop_files(),
    ct:pal("Number of ROP files = ~p", [N2]),
    true = N >= N2,
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    N3 = get_number_of_rop_files(),
    ct:pal("Number of ROP files = ~p", [N3]),
    true = N >= N3,

    ct:sleep(3000),
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    delete_app_jobs(Handle, [?JOB_1, ?JOB_2, ?JOB_3]),
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP),
    
    %%-------------------------------------------------------------
    %% Verify that number of ROP files are still at the max limit
    %%-------------------------------------------------------------
    ct:sleep(5000),
    N = get_number_of_rop_files(),
    ct:pal("Number of ROP files = ~p", [N]),

    %%-------------------------------------------------------------
    %% Remove the remaining dummy ROP files
    %%-------------------------------------------------------------
    remove_dummy_rop_files().
    

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

get_file_names(undefined) ->
    ["", "_" ++ ?JOB_GROUP_LRAT, "_" ++ ?JOB_GROUP_WRAT];
get_file_names(NwMeId) ->
    [NwMeId,
     NwMeId ++ "_" ++ ?JOB_GROUP_LRAT,
     NwMeId ++ "_" ++ ?JOB_GROUP_WRAT].


%%========================================================================
%% Create list of jobs.
%%========================================================================
create_job_list(_Index, []) ->
    [];
create_job_list(Index, JobGroups) ->
    [{JobGroup, Job} | Tail] = JobGroups,
    [{Job,
    [{Job ++ "_mr" ++ integer_to_list(Index), ?Group1}],
    get_job_attrs(JobGroup)} |
        create_job_list(Index + 1, Tail)].

%%========================================================================
%% Create one app and N jobs.
%%========================================================================
create_app_jobs(JobGroups) ->
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({?APP, [{expected, [relay]}]}),

    %%-------------------------------------------------------------
    %% Create N jobs
    %%-------------------------------------------------------------
    JobsList = create_job_list(1, JobGroups),

    ok = create_jobs(JobsList),
    Handle.

delete_app_jobs(Handle, Jobs) ->
    ok = delete_jobs(Jobs),
    ok = delete_app(?APP, Handle).


get_job_attrs(undefined) ->
    [{"reportingPeriod",   1},
     {"granularityPeriod", 1}];
get_job_attrs(JobGroup) ->
    [{"reportingPeriod",   1},
     {"granularityPeriod", 1},
     {"jobGroup",          JobGroup}].


get_rop_expressions(N, Jobs) ->
    Joined = lists:zip(N,Jobs),
    SortedN = lists:sort(fun({A,_}, {B,_}) -> B > A end,Joined),
    [ Job || {_, Job} <- SortedN].


%%========================================================================
%% Create N dummy ROP-files
%%========================================================================
create_dummy_rop_files(N) when N > 0 ->
    Name = "dummy_rop" ++ integer_to_list(N),
    ok = rpc(pmsDb, rop_file_store, [Name, <<"Dummy ROP contents">>]),
    create_dummy_rop_files(N - 1);

create_dummy_rop_files(_) ->
    ok.

%%========================================================================
%% Remove all dummy ROP-files
%%========================================================================
remove_dummy_rop_files() ->
    {ok, Rops} = ?ROP_HOOK:list_rop_files(),
    DummyRops = [DummyRop || [$d, $u, $m, $m, $y | _] = DummyRop <- Rops], 
    ct:pal("Deleting ~p dummy ROP files", [length(DummyRops)]),
    [ok = rpc(pmsDb, rop_file_delete, [Name]) || Name <- DummyRops],
    ok.


%%========================================================================
%% misc functions
%%========================================================================
%%========================================================================
%% get_values(MTs, LDN, Values) -> AliasValues
%%
%% Create values to be reported in the next rop data request
%%========================================================================
get_values([{?Group1, [?Type1]}],
       LDN,
       [{Val1}]) ->
    Vals = [{?Group1, [{LDN, [{?Type1, [Val1]}]}]}],
    aliasify_values(Vals).


%%========================================================================
%% get_rops_with_same_time_stamp(NoOfFiles) -> [Indexes]
%%
%% Gets the indexes of ROP files that were created in the
%% same RP, but only if enough of them are found (as
%% specified by NoOfFiles argument).
%%========================================================================
get_rops_with_same_time_stamp(NoOfFiles) ->
    {ok, Rops} = ?ROP_HOOK:list_rop_files(),

    Indexes = lists:seq(1,length(Rops)),
    IndexedRops = lists:zip(Rops, Indexes),
    IndexedWithTimetamps = [{Idx, get_rop_ts(Rop), Rop} ||
                            {Rop, Idx}  <- IndexedRops],

    Grouped = group_by(fun compare_ts/2, IndexedWithTimetamps),

    GroupsWithEnoughFiles = lists:filter(
        fun(L) -> length(L) == NoOfFiles end,
        Grouped
    ),

    %% Checking if only once enough files were generated
    %% is enough for the scope of this test.
    [FirstGroup | _] = GroupsWithEnoughFiles,
    [Idx || {Idx, _, _} <- FirstGroup].


%%========================================================================
%% compare_ts(Rop1Data, Rop2Data) -> Boolean
%%
%% Compares timestamps of two triples that contain ROP data:
%% index, timestamp and filename.
%%========================================================================
compare_ts({_, Ts1, _}, {_, Ts2, _}) ->
    Ts1 == Ts2.


%%========================================================================
%% get_rop_ts(RopFilename) -> Timestamp
%%
%% Gets a timestamp from a ROP filename.
%%========================================================================
get_rop_ts(RopFilename) ->
    [_, Timestamp | _] = string:tokens(RopFilename, "._"),
    Timestamp.


%%========================================================================
%% get_number_of_rop_files() -> integer()
%%
%% Get the total number of ROP files.
%%========================================================================
get_number_of_rop_files() ->
    {ok, Rops} = ?ROP_HOOK:list_rop_files(),
    length(Rops).

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

set_nw_me_id() ->
    ok = rct_cli:connect(?CLI_USER),

    rct_cli:send(?CLI_USER, "top", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "ManagedElement=1", ?PRINT_OPT),
    
    rct_cli:send(?CLI_USER, "configure", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "networkManagedElementId=1", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "commit", ?PRINT_OPT),
    ok = rct_cli:disconnect(?CLI_USER).

unset_nw_me_id() ->
    ok = rct_cli:connect(?CLI_USER),

    rct_cli:send(?CLI_USER, "top", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "ManagedElement=1", ?PRINT_OPT),
    
    rct_cli:send(?CLI_USER, "configure", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "no networkManagedElementId", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "commit", ?PRINT_OPT),
    ok = rct_cli:disconnect(?CLI_USER).




%% to() ->
%%     to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.




get_ct_config(TC) -> ?LIB:get_ct_config(TC).

cleanup() -> ?LIB:cleanup().


 create_app(A)       -> ?LIB:create_app(A).
%%create_app(A, B)    -> ?LIB:create_app(A, B).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
%%create_job(A, B)    -> ?LIB:create_job(A, B).
%%create_job(A, B, C) -> ?LIB:create_job(A, B, C).
create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
update_job(A, B)    -> ?LIB:update_job(A, B).
%%delete_job(A)       -> ?LIB:delete_job(A).


rpc(A, B, C) -> ?LIB:rpc(A, B, C).

%%expected(A, B, C)               -> ?LIB:expected(A, B, C).
%%wait_expected_result(A)         -> ?LIB:wait_expected_result(A).
wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B).
%%wait_expected_error_result(A,B) -> ?LIB:wait_expected_error_result(A, B).
%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%% open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

%%log_rop_files(Config) ->
%%    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
%%    log_rop_files(RopData, Config).

%%log_rop_files(RopData, Config) -> pms_test_lib:log_rop_files(RopData, Config).



%%check_rop_file(A, B)     -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)  -> ?LIB:check_rop_file(A, B, C).
%%wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
%%get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
%%wait_until_subscribe(A)  -> ?LIB:wait_until_subscribe(A).
wait_report_rop(A, B, C) -> ?LIB:wait_report_rop(A, B, C).
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
%%get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).

rop_file_handling(A) -> ?LIB:rop_file_handling(A).


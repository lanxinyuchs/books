%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_job_basic_2_SUITE.erl %
%%% @copyright Ericsson AB 2015
%%% @version /main/R5A/3

%%% @doc 
%%% @end

-module(pms_job_basic_2_SUITE).
-include_lib("common_test/include/ct.hrl").

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
%%% R5A/1      2015-12-08 uabesvi     split pms_job_basic_SUITE
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


-export([one_job_one_mr_active_stop_active/1]).
-export([one_job_two_mr/1]).
-export([three_apps_same_jobs/1]).
-export([two_apps_diff_jobs/1]).
-export([two_grps/1]).
-export([merge_two_grps/1]).
-export([update_grp_id/1]).
-export([update_gp/1]).
-export([one_job_two_apps/1]).
-export([fragmented/1]).
-export([no_pm_data/1]).
-export([multi_ldn/1]).


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



%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    hooks().


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
    %% rpc(os, cmd, ["tex log clear"]),
    cleanup(),
    Config.

%% @hidden
end_per_testcase(session_kill_app_proc, _Config) ->
    cleanup(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    %% X = rpc(os, cmd, ["tex log read"]),
    %% ct:pal("TEX LOG ~n~p~n", [io:format(X)]),
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
	   one_job_one_mr_active_stop_active,
 	   three_apps_same_jobs,
	   two_apps_diff_jobs,
	   %% two_grps,          removed until ECIM model handles groupId
	   %% merge_two_grps,    removed until ECIM model handles groupId
	   %% update_grp_id,     removed until ECIM model handles groupId
	   %% update_gp,         removed until ok to upgrade GP/RP
	   one_job_two_apps,
	   no_pm_data,
	   fragmented,
	   multi_ldn
	  ],
    All ++ all_host(host()).

all_host(?TESTNODE) ->
    [];
all_host(_PmsSim) ->
    [
     session_kill_app_proc,    %% causes problems with the test env on testnode
     one_job_two_mr            %% Not valid on target with current ECIM
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
%% one_job_one_mr_active_stop_active(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% activate, stop and reactivate the job
%% @end
%%========================================================================
one_job_one_mr_active_stop_active(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    ct:pal("+++ creating the job~n"),
    {ok, _} = open_trans(),
    ok = create_job_mr("job8", {"job8_reader1", "Group1", "Type1"}),
    ok = close_trans(),
    to(),

    %%-------------------------------------------------------------
    %% check tables and processes
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),

    check_processes(3),

    AppPid = wait_subscribe(["Group1"]),
    wait_report(AppPid, [{"Group1",[{"Type1",[4]}]}]),

    %%-------------------------------------------------------------
    %% stop the job, there should not be any report requests
    %%-------------------------------------------------------------
    ct:pal("+++ stopping the job~n"),
    {ok, _} = open_trans(),
    update_job("job8", [{"requestedJobState", ?STOPPED}]),
    ok = close_trans(),
    AppPid = wait_subscribe([]),
    wait_report(no),

    %%-------------------------------------------------------------
    %% activate the job again, the report request should start again
    %%-------------------------------------------------------------
    ct:pal("+++ activating the job~n"),
    {ok, _} = open_trans(),
    update_job("job8", [{"requestedJobState", ?ACTIVE}]),
    ok = close_trans(),
    AppPid = wait_subscribe(["Group1"]),
    wait_report(AppPid, [{"Group1",[{"Type1",[4]}]}]),
    wait_report(AppPid, [{"Group1",[{"Type1",[7]}]}]),

    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    ct:pal("+++ deleting the job~n"),
    {ok, _} = open_trans(),
    %%delete_mr("job7", "job7_reader1"),
    delete_job("job8"),
    ok = close_trans(),
    to(),

    AppPid = wait_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ct:pal("+++ finialize the app~n"),
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    

%%========================================================================
%% one_job_two_mr(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% @end
%%========================================================================
one_job_two_mr(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize(?SUNE),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job9", [{"job9_reader1", "Group2", "Type2"},
				{"job9_reader2", "Group2", "Type3"}]),
    ok = close_trans(),
    to(),

    %%-------------------------------------------------------------
    %% check tables and processes
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 2),

    check_processes(3),

    AppPid = wait_subscribe(["Group2"]),
    wait_report(AppPid),
    to(),
    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job8", "job8_reader1"),
    %%delete_mr("job8", "job8_reader2"),
    delete_job("job9"),
    ok = close_trans(),
    to(),

    AppPid = wait_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    

%%========================================================================
%% three_apps_same_jobs(Config) -> ok.
%% 
%% @doc 
%% start three apps, all measuring the same counters
%% @end
%%========================================================================
three_apps_same_jobs(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE,  [{ask, true}]),
    start(?STINA, [{ask, true}]),
    start(?BERIT, [{ask, true}]),
    [initialize(App) || App <- ?TEST_APPS2],

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job10", {"job10_reader1", "Group2", "Type2"}),
%%    create_mr("job9", "job9_reader2", "Group2", "Type3"),
    ok = close_trans(),
    to(),

    %%-------------------------------------------------------------
    %% check tables and processes
    %%-------------------------------------------------------------
    to(),
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,
%%    LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),

    check_processes(5),

    AppPid11 = wait_subscribe(["Group2"]),
    AppPid12 = wait_subscribe(["Group2"]),
    AppPid13 = wait_subscribe(["Group2"]),
    RepPid11 = wait_report(),
    RepPid12 = wait_report(),
    RepPid13 = wait_report(),

    [] = [AppPid11, AppPid12, AppPid13] -- [RepPid11, RepPid12, RepPid13],

    %%-------------------------------------------------------------
    %% delete the PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job9", "job9_reader1"),
    %%delete_mr("job9", "job9_reader2"),
    delete_job("job10"),
    ok = close_trans(),
    to(),

    AppPid21 = wait_subscribe([]),
    AppPid22 = wait_subscribe([]),
    AppPid23 = wait_subscribe([]),

    [] = [AppPid11, AppPid12, AppPid13] -- [AppPid21, AppPid22, AppPid23],

    %% LJ = length(get_table(pmJob)),
    ok = wait_table_size(pmJob, LJ),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    [finalize(App) || App <- ?TEST_APPS2],
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    stop(?STINA),
    stop(?BERIT),
    ok.
    


%%========================================================================
%% two_apps_diff_jobs(Config) -> ok.
%% 
%% @doc 
%% start two apps measuring different counters
%% @end
%%========================================================================
two_apps_diff_jobs(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE,  [{ask, true}]),
    start(?BERIT, [{ask, true}]),
    initialize(?SUNE,  ["Group1"]),
    initialize(?BERIT, ["Group2"]),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job11", {"job11_reader1", "Group1", "Type1"}),
    ok = create_job_mr("job12", {"job12_reader2", "Group2", "Type2"}),
    ok = close_trans(),
    to(),

    %%-------------------------------------------------------------
    %% create a MeasurementReader
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 2,
    %% LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 2),
    ok = wait_table_size(measurementReader, LMR + 2),

    check_processes(5),

    {AppPid11, Group11, _} = wait_subscribe(any),
    {AppPid12, Group12, _} = wait_subscribe(any),
    [] = (Group11 ++ Group12) -- ["Group1", "Group2"],

    %% In some rear cases the two reports comes from the same app
    %% so we have to wait for three reports to be sure that both
    %% apps have sent a report
    %% This testing should be done in some other way.
    RepPid11 = wait_report(),
    RepPid12 = wait_report(),
    RepPid13 = wait_report(),

    [] = [AppPid11, AppPid12] -- [RepPid11, RepPid12, RepPid13],
    to(),

    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job11", "job11_reader1"),
    %%delete_mr("job12", "job12_reader2"),
    delete_job("job11"),
    delete_job("job12"),
    ok = close_trans(),
    to(),


    AppPid21 = wait_subscribe([]),
    AppPid22 = wait_subscribe([]),

    [] = [AppPid11, AppPid12] -- [AppPid21, AppPid22],


    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?BERIT),
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    stop(?BERIT),
    ok.
    




%%========================================================================
%% two_grps(Config) -> ok.
%% 
%% @doc 
%% start two jobs in different groups
%% @end
%%========================================================================
two_grps(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE,  [{ask, true}]),
    start(?BERIT, [{ask, true}]),
    initialize(?SUNE,  ["Group1"]),
    initialize(?BERIT, ["Group2"]),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job13", {"job13_reader1", "Group1", "Type1"}),
    Job2Attrs =[{"granularityPeriod", ?TEN_SECONDS},
		{"reportingPeriod",   ?THIRTY_SECONDS},
		{"jobPriority",       ?HIGH},
		{"requestedJobState", ?ACTIVE}],
    ok = create_job_mr({"job14", Job2Attrs}, 
		       {"job14_reader2", "Group2", "Type2"}),
    ok = close_trans(),
    to(),

    %%-------------------------------------------------------------
    %% create a MeasurementReader
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 2,
    %% LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 2),
    ok = wait_table_size(measurementReader, LMR + 2),

    check_processes(6),

    {AppPid11, Group11, _} = wait_subscribe(any),
    {AppPid12, Group12, _} = wait_subscribe(any),
    [] = (Group11 ++ Group12) -- ["Group1", "Group2"],
    RepPid11 = wait_report(),
    RepPid12 = wait_report(),
    RepPid13 = wait_report(),

    [] = [AppPid11, AppPid12] -- [RepPid11, RepPid12, RepPid13],
    to(),

    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job13", "job13_reader1"),
    %%delete_mr("job14", "job14_reader2"),
    delete_job("job13"),
    delete_job("job14"),
    ok = close_trans(),
    to(),

    AppPid21 = wait_subscribe([]),
    AppPid22 = wait_subscribe([]),

    [] = [AppPid11, AppPid12] -- [AppPid21, AppPid22],

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?BERIT),
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    stop(?BERIT),
    ok.
    


%%========================================================================
%% merge_two_grps(Config) -> ok.
%% 
%% @doc 
%% start two jobs in different groups, merge them to same group
%% @end
%%========================================================================
merge_two_grps(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE,  [{ask, true}]),
    start(?BERIT, [{ask, true}]),
    initialize(?SUNE,  ["Group1"]),
    initialize(?BERIT, ["Group2"]),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job15", {"job15_reader1", "Group1", "Type1"}),
    Job2Attrs =[{"granularityPeriod", ?TEN_SECONDS},
		{"reportingPeriod",   ?THIRTY_SECONDS},
		{"jobPriority",       ?HIGH},
		{"currentJobState",   ?ACTIVE}],
    ok = create_job_mr({"job16", Job2Attrs},
		       {"job16_reader2", "Group2", "Type2"}),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check tables and processes
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 2,
    %% LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 2),
    ok = wait_table_size(measurementReader, LMR + 2),

    check_processes(6),

    {AppPid11, Group11, _} = wait_subscribe(any),
    {AppPid12, Group12, _} = wait_subscribe(any),
    [] = (Group11 ++ Group12) -- ["Group1", "Group2"],
    RepPid11 = wait_report(),
    RepPid12 = wait_report(),
    RepPid13 = wait_report(),

    [] = [AppPid11, AppPid12] -- [RepPid11, RepPid12, RepPid13],

    {AppPidGrp1, AppPidGrp2} = case Group11 of
				   ["Group1"] -> {AppPid11, AppPid12};
				   ["Group2"] -> {AppPid12, AppPid11}
			       end,

    %%-------------------------------------------------------------
    %% update the job1 to same group id as job2
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    update_job("job15", [{"jobPriority", ?HIGH}]),
    ok = close_trans(),

    AppPidGrp1 = wait_subscribe([]),
    AppPidGrp1 = wait_subscribe(["Group1"]),

    RepPid21 = wait_report(),
    RepPid22 = wait_report(),
    RepPid23 = wait_report(),
    [] = [AppPidGrp1, AppPidGrp2] -- [RepPid21, RepPid22, RepPid23],

    %% one pmsGroup process less, when both jobs in the same group id
    check_processes(5),

    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job15", "job15_reader1"),
    %%delete_mr("job16", "job16_reader2"),
    delete_job("job15"),
    delete_job("job16"),
    ok = close_trans(),

    AppPid21 = wait_subscribe([]),
    AppPid22 = wait_subscribe([]),

    [] = [AppPidGrp1, AppPidGrp2] -- [AppPid21, AppPid22],

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?BERIT),
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    stop(?BERIT),
    ok.
    

%%========================================================================
%% update_grp_id(Config) -> ok.
%% 
%% @doc 
%% start two jobs in same group, change the group for one job
%% after a reporting period
%% @end
%%========================================================================
update_grp_id(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE,  [{ask, true}]),
    start(?BERIT, [{ask, true}]),
    initialize(?SUNE,  ["Group1"]),
    initialize(?BERIT, ["Group2"]),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job17", {"job17_reader1", "Group1", "Type1"}),
    ok = create_job_mr("job18", {"job18_reader2", "Group2", "Type2"}),
    ok = close_trans(),
    to(),

    %%-------------------------------------------------------------
    %% create a MeasurementReader
    %%-------------------------------------------------------------
    to(),
    %% LJ = length(get_table(pmJob)) - 2,
    %% LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 2),
    ok = wait_table_size(measurementReader, LMR + 2),

    check_processes(5),

    {AppPid11, Group11, _} = wait_subscribe(any),
    {AppPid12, Group12, _} = wait_subscribe(any),
    [] = (Group11 ++ Group12) -- ["Group1", "Group2"],
    RepPid11 = wait_report(),
    RepPid12 = wait_report(),
    RepPid13 = wait_report(),

    [] = [AppPid11,  AppPid12] -- [RepPid11, RepPid12, RepPid13],

    {AppPidGrp1, AppPidGrp2} = case Group11 of
				   ["Group1"] -> {AppPid11, AppPid12};
				   ["Group2"] -> {AppPid12, AppPid11}
			       end,

    %%-------------------------------------------------------------
    %% update the group id for one of the PM Jobs
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    update_job("job18", [{"jobPriority", ?HIGH}]),
    ok = close_trans(),

    check_processes(6),

    AppPidGrp2 = wait_subscribe([]),
    AppPidGrp2 = wait_subscribe(["Group2"]),
    RepPid21 = wait_report(),
    RepPid22 = wait_report(),
    RepPid23 = wait_report(),

    [] = [AppPidGrp1,  AppPidGrp2] -- [RepPid21, RepPid22, RepPid23],

    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job17", "job17_reader1"),
    %%delete_mr("job18", "job18_reader2"),
    delete_job("job17"),
    delete_job("job18"),
    ok = close_trans(),
    to(),

    AppPid21 = wait_subscribe([]),
    AppPid22 = wait_subscribe([]),

    [] = [AppPid11, AppPid12] -- [AppPid21, AppPid22],

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?BERIT),
    finalize(?SUNE),
    %% to(2),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    stop(?BERIT),
    ok.
    




%%========================================================================
%% update_gp(Config) -> ok.
%% 
%% @doc 
%% start two jobs, change the gp one job
%% after a granularity period
%% @end
%%========================================================================
update_gp(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE,  [{ask, true}]),
    start(?BERIT, [{ask, true}]),
    initialize(?SUNE,  ["Group1"]),
    initialize(?BERIT, ["Group2"]),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job20", {"job20_reader1", "Group1", "Type1"}),
    ok = create_job_mr("job21", {"job21_reader2", "Group2", "Type2"}),
    ok = close_trans(),
    to(),

    %%-------------------------------------------------------------
    %% create a MeasurementReader
    %%-------------------------------------------------------------
    %% LJ  = length(get_table(pmJob)) - 2,
    %% LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 2),
    ok = wait_table_size(measurementReader, LMR + 2),

    check_processes(5),

    {AppPid11, Group11, _} = wait_subscribe(any),
    {AppPid12, Group12, _} = wait_subscribe(any),
    [] = (Group11 ++ Group12) -- ["Group1", "Group2"],
    RepPid11 = wait_report(),
    RepPid12 = wait_report(),
    RepPid13 = wait_report(),

    [] = [AppPid11,  AppPid12] -- [RepPid11, RepPid12, RepPid13],

    {AppPidGrp1, AppPidGrp2} = case Group11 of
				   ["Group1"] -> {AppPid11, AppPid12};
				   ["Group2"] -> {AppPid12, AppPid11}
			       end,

    %%-------------------------------------------------------------
    %% update the group id for one of the PM Jobs
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    update_job("job21", [{"granularityPeriod", ?THIRTY_SECONDS}]),
    ok = close_trans(),

    check_processes(5),

    AppPidGrp2 = wait_subscribe([]),
    AppPidGrp2 = wait_subscribe(["Group2"], ok, gp(?THIRTY_SECONDS)),
    %% wait at most 8 report requests to get a report request from AppPidGrp2
    ok = wait_for_report(AppPidGrp2, gp(?THIRTY_SECONDS), 8),
    D1 = date(),
    T1 = time(),
    ct:pal("#### T1 ~p~n", [T1]),

    %% wait at most 6 report requests to get a report request from AppPidGrp2
    ok = wait_for_report(AppPidGrp2, gp(?THIRTY_SECONDS), 6),
    D2 = date(),
    T2 = time(),
    ct:pal("#### T2 ~p~n", [T2]),

    %% The time difference between the report requests should be 30 seconds
    GS1 = calendar:datetime_to_gregorian_seconds({D1, T1}),
    GS2 = calendar:datetime_to_gregorian_seconds({D2, T2}), 
    
    case GS2 - GS1 of
	GS when GS > 25 andalso GS < 35 ->
	    ok;
	GS ->
	    ct:fail("The time between 2 reports for ~p was not valid ~p~n",
		    [AppPidGrp2, GS])
    end,

    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %% delete_mr("job20", "job20_reader1"),
    %% delete_mr("job21", "job21_reader2"),
    delete_job("job20"),
    delete_job("job21"),
    ok = close_trans(),
    to(),

    {AppPid21, [], Gp21} = wait_subscribe(any),
    {AppPid22, [], Gp22} = wait_subscribe(any),
    [] = [Gp21, Gp22] -- [?TEN_SECONDS, ?THIRTY_SECONDS],
    [] = [AppPidGrp1, AppPidGrp2] -- [AppPid21, AppPid22],

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?BERIT),
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),

%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    stop(?BERIT),
    ok.
    

%%========================================================================
%% one_job_two_apps(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% @end
%%========================================================================
one_job_two_apps(_Config) ->
    Tables = tables(),
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE,  [{ask, true}]),
    start(?STINA, [{ask, true}]),
    initialize(?SUNE,  ["Group1"]),
    initialize(?STINA, ["Group2"]),

    %%-------------------------------------------------------------
    %% create a PM Job and MeasurementReader
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job22", [{"job22_reader1", "Group1", "Type1"},
				 {"job22_reader2", "Group2", "Type3"}]),
    to(),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 2,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 2),
    check_processes(4),

    {AppPid11, Group11, _} = wait_subscribe(any),
    {AppPid12, Group12, _} = wait_subscribe(any),
    [] = (Group11 ++ Group12) -- ["Group1", "Group2"],
    RepPid11 = wait_report(),
    RepPid12 = wait_report(),
    RepPid13 = wait_report(),

    [] = [AppPid11,  AppPid12] -- [RepPid11, RepPid12, RepPid13],

%%     {AppPidGrp1, AppPidGrp2} = case Group11 of
%% 				   ["Group1"] -> {AppPid11, AppPid12};
%% 				   ["Group2"] -> {AppPid12, AppPid11}
%% 			       end,

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    delete_job("job22"),
    ok = close_trans(),
    to(),

    AppPid21 = wait_subscribe([]),
    AppPid22 = wait_subscribe([]),

    [] = [AppPid11, AppPid12] -- [AppPid21, AppPid22],


    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    finalize(?STINA),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    stop(?STINA),
    ok.
    

%%========================================================================
%% fragmented(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% the pm data is fragmented
%% @end
%%========================================================================
fragmented(_Config) ->
    Tables = tables(),
    PmGrps = ["Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job and MeasurementReader
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job23", [{"job23_reader1", "Group2", "Type2"},
				 {"job23_reader1", "Group2", "Type3"}]),
    to(),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),
    check_processes(3),

    AppPid = wait_subscribe(["Group2"]),
    wait_report(AppPid, [[{"Group2",[{"Type2",[4]}]}],
			 [{"Group2",[{"Type3",[7]}]}]]),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    delete_job("job23"),
    ok = close_trans(),
    to(),

    AppPid = wait_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    

    
%%========================================================================
%% no_pm_data(Config) -> ok.
%% 
%% @doc 
%% tell app not to send pm data
%% @end
%%========================================================================
no_pm_data(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job and MeasurementReader
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job25", {"job25_reader1", "Group1", "Type1"}),
    to(),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),
    check_processes(3),

    AppPid = wait_subscribe(["Group1"]),
    wait_report(do_not_reply),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    delete_job("job25"),
    ok = close_trans(),
    to(),

    AppPid = wait_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    


%%========================================================================
%% multi_ldn(Config) -> ok.
%% 
%% @doc 
%% simulate multi instances
%% @end
%%========================================================================
multi_ldn(_Config) ->

    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job and MeasurementReader
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job26", {"job26_reader1", "Group1", "Type1"}),
    to(),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),
    check_processes(3),

    AppPid = wait_subscribe(["Group1"]),
    wait_report(AppPid, 
 		{objects, 
 		 [{"ME=1,Erat=1,Charles=1", 
 		   [{"Group1",[{"a", [21]}, {"b", [22]}, {"c", [23]}]}]},
 		  {"ME=1,Erat=1,Charles=2", 
 		   [{"Group1",[{"a", [31]}, {"b", [32]}, {"c", [33]}]}]},
 		  {"ME=1,Erat=1,Charles=3", 
 		   [{"Group1",[{"a", [41]}, {"b", [42]}, {"c", [43]}]}]}]
 		}),
    
    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job26"),
    ok = close_trans(),
    to(),

    AppPid = wait_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    %% to(),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


gp(?TEN_SECONDS)    -> 10;
gp(?THIRTY_SECONDS) -> 30.

to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


%%start(A)    -> {ok, _} = pms_erl_app:start(A).
start(A, B) -> {ok, _} = pms_erl_app:start(A, B).
stop(A)     -> ok = pms_erl_app:stop(A).   

initialize(A)      -> ok = ?LIB:initialize(A).
initialize(A, B)   -> ok = ?LIB:initialize(A, B).
finalize(A)        -> ok = ?LIB:finalize(A).
%%kill(A)            -> ?LIB:kill(A).
get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
check_tables(A)    -> ?LIB:check_tables(A).
check_processes(A) -> ?LIB:check_processes(A).
cleanup()          -> ?LIB:cleanup().
%% log_msg()          -> ?LIB:log_msg().
rpc(A, B, C)       -> ?LIB:rpc(A, B, C).

wait_table_size(A, B)    -> ?LIB:wait_table_size(A, B).
%% wait_table_size(A, B, C) -> ?LIB:wait_table_size(A, B, C).
wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
%%wait_subscribe(A, B)     -> ?LIB:wait_subscribe(A, B).
wait_subscribe(A, B, C)  -> ?LIB:wait_subscribe(A, B, C).
wait_report()            -> ?LIB:wait_report().
wait_report(A)           -> ?LIB:wait_report(A).
wait_report(A, B)        -> ?LIB:wait_report(A, B).
wait_for_report(A, B, C) -> ?LIB:wait_for_report(A, B, C).

%%create_job(A)       -> ?LIB:create_job(A).
update_job(A, B)    -> ?LIB:update_job(A, B).
delete_job(A)       -> ?LIB:delete_job(A).
%%delete_mr(A, B)     -> ?LIB:delete_mr(A, B).
create_job_mr(A, B) -> ?LIB:create_job_mr(A, B).

%%create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

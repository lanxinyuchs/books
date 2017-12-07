%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_job_ext_SUITE.erl %
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/6


-module(pms_job_ext_SUITE).
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
%%% R1A/1      2013-02-28 uabesvi     Created
%%% R1A/11     2013-06-24 uabesvi     Adaptions to new COM
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


-export([pm_job_add_del/1]).
-export([illegal_group/1]).
-export([late_pm_data/1]).
-export([suspect_marked/1]).
-export([illegal_gp/1]).
-export([del_mr_1/1]).
-export([del_mr_2/1]).
-export([non_valid_mr/1]).
-export([valid_and_non_valid_mr/1]).
-export([non_valid_grp/1]).
-export([valid_and_non_valid_grp/1]).
-export([multi_trans/1]).
-export([create_del_trans/1]).
-export([non_legacy_rp/1]).


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
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    %%log_msg(),
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
    [catch pms_erl_app:stop(App) || App <- ?TEST_APPS],
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    All = [
	   pm_job_add_del,
	   late_pm_data,
	   illegal_gp,
	   suspect_marked,
	   non_valid_mr,
	   valid_and_non_valid_mr,
	   non_valid_grp,
	   valid_and_non_valid_grp,
	   multi_trans,
	   create_del_trans,
	   del_mr_1,
	   del_mr_2
	  ],
    All ++ all_host(host()).

all_host(?TESTNODE) ->
    [];
all_host(_PmsSim) ->
    [
     illegal_group            %% causes problems with the test env on testnode
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
%% pm_job_add_del(Config) -> ok.
%% 
%% @doc 
%% create and delete a pm job 
%% @end
%%========================================================================
pm_job_add_del(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ = length(get_table(pmJob)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job and check that it is created in tables
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),

    %% In previous COM releases this returned an error,
    %% it was not possible to create job without mr
    %% now it seems to be OK.
    ok = create_job("job16"),
    {error, _} = close_trans(),

    %% to(),
    %% L = length(get_table(pmJob)),
    ok = wait_table_size(pmJob, LJ),

    {ok, _} = open_trans(),
    ok = delete_job("job16"),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    


%%========================================================================
%% illegal_gp(Config) -> ok.
%% 
%% @doc 
%% create a pm job where GP > RP
%% @end
%%========================================================================
illegal_gp(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    %%L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job and a MR
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    Attrs = [{"granularityPeriod", ?THIRTY_SECONDS},
	     {"reportingPeriod",   ?TEN_SECONDS},
	     {"currentJobState",   ?ACTIVE}],
    ok = create_job_mr({"job17", Attrs}, {"job17_reader1", "Group1", "Type1"}),
    {error, _} = close_trans(),

    %%-------------------------------------------------------------
    %% check the tables
    %%-------------------------------------------------------------

    ok = wait_table_size(pmJob, LJ),
    ok = wait_table_size(measurementReader, LMR),


    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    




%%========================================================================
%% illegal_group(Config) -> ok.
%% 
%% @doc 
%% try to initialize an illegal group
%% @end
%%========================================================================
illegal_group(_Config) ->
    Tables = tables(),
    PmGrps = ["illegal_group"],

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    X = ?LIB:initialize(?SUNE, PmGrps),
    ct:pal(" init ~p~n", [X]),

    check_tables(Tables),
%%    check_processes(1),
    stop(?SUNE),
    ok.
    

%%========================================================================
%% late_pm_data(Config) -> ok.
%% 
%% @doc 
%% tell app to send pm data later
%% @end
%%========================================================================
late_pm_data(_Config) ->
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
    ok = create_job_mr("job18", {"job18_reader1", "Group1", "Type1"}),
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
    wait_report(wait, {2000, [{"Group2",[{"Type2",[4]}]}]}),
    
    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    delete_job("job18"),
    ok = close_trans(),
    to(),

    AppPid = wait_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
%%========================================================================
%% suspect_marked(Config) -> ok.
%% 
%% @doc 
%% tell app to suspect mark the counter
%% @end
%%========================================================================
suspect_marked(_Config) ->
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
    ok = create_job_mr("job19", {"job19_reader1", "Group1", "Type1"}),
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
    wait_report(AppPid, [{"Group2",[{"Type2",[-1]}]}]),
    
    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    delete_job("job19"),
    ok = close_trans(),
    to(),

    AppPid = wait_until_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
%%========================================================================
%% del_mr_1(Config) -> ok.
%% 
%% @doc 
%% create one pm job with one MR, try to only delete the MR
%% @end
%%========================================================================
del_mr_1(_Config) ->
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
    ok = create_job_mr("job20", {"job20_reader1", "Group1", "Type1"}),
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
    wait_report(AppPid, [{"Group1",[{"Type1",[4]}]}]),
    wait_report(AppPid, [{"Group1",[{"Type1",[7]}]}]),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    {error, _} = delete_mr("job20", "job20_reader1"),
    to(),

    {ok, _} = open_trans(),
    ok = delete_job("job20"),
    ok = close_trans(),
    to(),

    AppPid = wait_until_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    %% to(),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
    
%%========================================================================
%% del_mr_2(Config) -> ok.
%% 
%% @doc 
%% create one pm job with one MR, try to only delete the MR
%% @end
%%========================================================================
del_mr_2(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L   = length(get_table(pmsAppRegistry)),
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
    ok = create_job_mr("job26", [{"job26_reader1", "Group1", "Type1"},
				 {"job26_reader2", "Group2", "Type2"}]),
    to(),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 2),
    check_processes(3),

    AppPid = wait_subscribe(["Group1", "Group2"]),
    wait_report(AppPid, [{"Group1",[{"Type1",[4]}]}]),
    wait_report(AppPid, [{"Group1",[{"Type1",[7]}]}]),

    %%-------------------------------------------------------------
    %% delete the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_mr("job26", "job26_reader1"),
    ok = close_trans(),
    to(),
    AppPid = wait_until_subscribe(["Group2"]),

    {ok, _} = open_trans(),
    ok = delete_job("job26"),
    ok = close_trans(),
    to(),

    AppPid = wait_until_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    %% to(),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
%%========================================================================
%% non_valid_mr(Config) -> ok.
%% 
%% @doc 
%% try to create a MR using an invalid MT
%% @end
%%========================================================================
non_valid_mr(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L   = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),


    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job an MR, but use an illegal MT 
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job27", {"job27_reader1", "Group1", "TypeNV1"}),
    ok = close_trans(),

    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),
    check_processes(3),

    %%-------------------------------------------------------------
    %% delete the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job27"),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
%%========================================================================
%% valid_and_non_valid_mr(Config) -> ok.
%% 
%% @doc 
%% try to create a MR using an invalid MT
%% @end
%%========================================================================
valid_and_non_valid_mr(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L   = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),


    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job an MR, but use an illegal MT 
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job28", [{"job28_reader1", "Group1", "Type1"},
				 {"job28_reader2", "Group1", "TypeNV1"}]),
    ok = close_trans(),

    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 2),
    check_processes(3),

    %%-------------------------------------------------------------
    %% delete the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job28"),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
%%========================================================================
%% non_valid_grp(Config) -> ok.
%% 
%% @doc 
%% start one job with a non valid group
%% @end
%%========================================================================
non_valid_grp(_Config) ->
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
    ok = create_job_mr("jobNV1", {"jobVN1_reader1", "GroupNV", "TypeNV1"}),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),
    check_processes(3),

    %%-------------------------------------------------------------
    %% delete the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("jobNV1"),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    %% to(),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
%%========================================================================
%% valid_and_non_valid_grp(Config) -> ok.
%% 
%% @doc 
%% start one job with a valid group and a non valid group
%% @end
%%========================================================================
valid_and_non_valid_grp(_Config) ->
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
    ok = create_job_mr("jobNV2", [{"jobVN2_reader1", "GroupNV", "TypeNV1"},
				  {"jobV2_reader1",  "Group1",  "Type1"}]),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 2),
    check_processes(3),

    AppPid = wait_subscribe(["Group1"]),
    wait_report(AppPid, [{"Group1",[{"Type1",[4]}]}]),

    %%-------------------------------------------------------------
    %% delete the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("jobNV2"),
    ok = close_trans(),

    AppPid = wait_until_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    %% to(),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
    
%%========================================================================
%% multi_trans(Config) -> ok.
%% 
%% @doc 
%% start many transactions
%% @end
%%========================================================================
multi_trans(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, false}]),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create jobs
    %%-------------------------------------------------------------
    Names = ["j1", "j2", "j3", "j4", "j5", "j6", "j7", "j8", "j9"],
    mt_create(Names),

    %%-------------------------------------------------------------
    %% check everything is OK
    %%-------------------------------------------------------------
    ok = wait_table_size(pmJob, LJ + 9),
    ok = wait_table_size(measurementReader, LMR + 9),
    check_processes(11),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    mt_delete(Names),

    ok = wait_table_size(pmJob, LJ),
    ok = wait_table_size(measurementReader, LMR),
    %%check_processes(2),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    %% to(),
    check_tables(Tables),
%%    check_processes(0),
    stop(?SUNE),
    ok.
 
mt_create(Names) ->   
    [mt_cre(N) || N <- Names].
   
mt_cre(Name) ->   
    {ok, _} = open_trans(),
    ok = create_job_mr(Name, {Name, "Group1", "Type1"}),
    ok = close_trans().
    
mt_delete(Names) ->   
    [mt_del(N) || N <- Names].
   
mt_del(Name) ->   
    {ok, _} = open_trans(),
    ok = delete_job(Name),
    ok = close_trans().

%%========================================================================
%% create_del_trans(Config) -> ok.
%% 
%% @doc 
%% create and delete jobs in the same transaction
%% @end
%%========================================================================
create_del_trans(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, false}]),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job and MeasurementReader
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job14", {"job14_reader1", "Group1", "Type1"}),
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

%%     AppPid = wait_subscribe(["Group1"]),
%%     wait_report(AppPid, [{"Group1",[{"Type1",[4]}]}]),

    %%-------------------------------------------------------------
    %% create a new and delete the old pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job14"),
    ok = create_job_mr("job15", {"job15_reader1", "Group1", "Type1"}),
    ok = close_trans(),
    to(),

%%     AppPid = wait_subscribe([]),
%%     AppPid = wait_subscribe(["Group1"]),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job15"),
    ok = close_trans(),
    to(),

%%     AppPid = wait_subscribe([]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    %% to(),
    check_tables(Tables),
    %%check_processes(0),
    stop(?SUNE),
    ok.
    

%%========================================================================
%% non_legacy_rp(Config) -> ok.
%% 
%% @doc 
%% change RP mode to legacy and try to start a job with non legacy RP
%% @end
%%========================================================================
non_legacy_rp(Config) ->

    %%-------------------------------------------------------------
    %% set RP mode to legacy
    %%-------------------------------------------------------------
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, rp_legacy]),
    ct:pal("Reporting period mode = ~p~n", [rp_legacy]),


    %%-------------------------------------------------------------
    %% create a PM Job and MeasurementReader
    %%-------------------------------------------------------------
    Attrs = [{"granularityPeriod", ?THIRTY_SECONDS},
	     {"reportingPeriod",   ?TEN_SECONDS}],

    {ok, _} = open_trans(),
    ok = create_job_mr({"job16", Attrs}, {"job16_reader1", "Group1", "Type1"}),
    {error, _} = close_trans(),

    %%-------------------------------------------------------------
    %% reset RP mode to what it was previously
    %%-------------------------------------------------------------
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    ct:pal("Reporting period mode = ~p~n", [rp_legacy]),

    ok.
    


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%% gp(?TEN_SECONDS)    -> 10;
%% gp(?THIRTY_SECONDS) -> 30.

to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


start(A)    -> {ok, _} = pms_erl_app:start(A).
start(A, B) -> {ok, _} = pms_erl_app:start(A, B).
stop(A)     -> ok = pms_erl_app:stop(A).   

%%initialize(A)      -> ok = ?LIB:initialize(A).
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
%%wait_subscribe(A, B, C)  -> ?LIB:wait_subscribe(A, B, C).
wait_until_subscribe(A)    -> ?LIB:wait_until_subscribe(A).
%%wait_report()            -> ?LIB:wait_report().
%%wait_report(A)           -> ?LIB:wait_report(A).
wait_report(A, B)        -> ?LIB:wait_report(A, B).
%%wait_for_report(A, B, C) -> ?LIB:wait_for_report(A, B, C).

create_job(A)       -> ?LIB:create_job(A).
%%update_job(A, B)    -> ?LIB:update_job(A, B).
delete_job(A)       -> ?LIB:delete_job(A).
delete_mr(A, B)     -> ?LIB:delete_mr(A, B).
create_job_mr(A, B) -> ?LIB:create_job_mr(A, B).

%%create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

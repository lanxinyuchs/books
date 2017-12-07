%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_job_basic_SUITE.erl %
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/R4A/R5A/2

%%% @doc 
%%% @end

-module(pms_job_basic_SUITE).
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


-export([session_init_finalize/1]).
-export([session_kill_app_proc/1]).
-export([session_two_apps/1]).
-export([mr_add_del/1]).
-export([add_mr/1]).
-export([del_job_remaining_mr/1]).
-export([one_job_one_mr/1]).
-export([one_job_one_mr_app/1]).
-export([one_job_one_of_two_mr/1]).


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
	   session_init_finalize,
	   session_two_apps,
	   mr_add_del,
	   del_job_remaining_mr,
	   one_job_one_mr,
 	   one_job_one_mr_app,
	   one_job_one_of_two_mr
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
%% session_init_finalize(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize an application
%% @end
%%========================================================================
session_init_finalize(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    initialize(?SUNE, PmGrps),    

    %%-------------------------------------------------------------
    %% check that the application has registered the groups
    %%-------------------------------------------------------------
    ok = wait_table_size(pmsAppRegistry, L + length(PmGrps)),
    %% to(),
    %% timer:sleep(5000),
    %% L = length(get_table(pmsAppRegistry)) - length(PmGrps),
    
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
%% session_kill_app_proc(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize an application
%% @end
%%========================================================================
session_kill_app_proc(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    {ok, _} = start(?SUNE),
    initialize(?SUNE, PmGrps),    

    %%-------------------------------------------------------------
    %% stop application
    %%-------------------------------------------------------------
    kill(?SUNE),
    to(),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),    
%%     check_processes(0),
    ok.
    

%%========================================================================
%% session_two_apps(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize two application
%% @end
%%========================================================================
session_two_apps(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    start(?STINA),

    initialize(?SUNE,  PmGrps),
    initialize(?STINA, PmGrps),

    %%-------------------------------------------------------------
    %% check that both applications have registered the groups
    %%-------------------------------------------------------------
    %% to(),
    %% L = length(get_table(pmsAppRegistry)) - length(PmGrps)*2,
    ok = wait_table_size(pmsAppRegistry, L + length(PmGrps)*2),

    %%-------------------------------------------------------------
    %% Check that 2 groups are removed from the pmsAppRegistry
    %% after one app is removed
    %%-------------------------------------------------------------
    finalize(?SUNE),
    %% to(),
    %% L = length(get_table(pmsAppRegistry)) - length(PmGrps),
    ok = wait_table_size(pmsAppRegistry, L + length(PmGrps)),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?STINA),
    %% to(),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%     check_processes(0),
    stop(?STINA),
    stop(?SUNE),
    ok.
    


%%========================================================================
%% mr_add_del(Config) -> ok.
%% 
%% @doc 
%% create and delete a pm job and a measurement reader
%% @end
%%========================================================================
mr_add_del(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
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
    ok = create_job_mr("job2", {"job2_reader1", "Group1", "Type1"}),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check the tables
    %%-------------------------------------------------------------
    %% to(),
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,

    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),

    %%-------------------------------------------------------------
    %% check that all processes are created.
    %% one of each: pmsJobGroup, pmsJob, and pmsAppJob
    %%-------------------------------------------------------------
    check_processes(3),
    
    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job1", "job1_reader1"),
    ok = delete_job("job2"),
    ok = close_trans(),
    %% to(),
    ok = wait_table_size(pmJob, LJ),
    ok = wait_table_size(measurementReader, LMR),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%     timer:sleep(11000),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    

%% %%========================================================================
%% %% del_job_remaining_mr(Config) -> ok.
%% %% 
%% %% @doc 
%% %% create a pm job and a measurement reader, delete the job without
%% %% deleting the measurement reader.
%% %% @end
%% %%========================================================================
del_job_remaining_mr(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],

    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    initialize(?SUNE, PmGrps),

    %%-------------------------------------------------------------
    %% create a PM Job and check it is inserted in the table
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job3", {"job3_reader1", "Group1", "Type1"}),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check tables
    %%-------------------------------------------------------------
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),

    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,

    %%-------------------------------------------------------------
    %% delete the PM Job, and check the tables are cleared
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job3"),
    ok = close_trans(),

    %% LJ  = length(get_table(pmJob)),
    %% LMR = length(get_table(measurementReader)),
    ok = wait_table_size(pmJob, LJ),
    ok = wait_table_size(measurementReader, LMR),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
 
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
%%     timer:sleep(11000),
    check_tables(Tables),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    

%%========================================================================
%% one_job_one_mr(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% @end
%%========================================================================
one_job_one_mr(_Config) ->
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
    ok = create_job_mr("job4", {"job4_reader1", "Group1", "Type1"}),
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
    wait_report(AppPid, [{"Group1",[{"a", [21]}, {"b", [22]}, {"c", [23]}]}]),
    wait_report(AppPid, [{"Group1",[{"d", [21]}, {"e", [22]}]}]),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job4"),
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
    

%%========================================================================
%% add_mr(Config) -> ok.
%% 
%% @doc 
%% start one job.
%% try to add another MR to it, it should not be accepted
%% @end
%%========================================================================
add_mr(_Config) ->
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
    ok = create_job_mr("job5", {"job5_reader1", "Group1", "Type1"}),
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
    %% create another MeasurementReader
    %%-------------------------------------------------------------
    {ok,    _} = open_trans(),
    {error, _} = create_mr("job5", "job5_reader2", "Group2", "Type2"),
    to(),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job("job5"),
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
    

%%========================================================================
%% one_job_one_mr(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% start the app after the job is started
%% @end
%%========================================================================
one_job_one_mr_app(_Config) ->
    Tables = tables(),
    PmGrps = ["Group1", "Group2"],
    L = length(get_table(pmsAppRegistry)),
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr("job6", {"job6_reader1", "Group1", "Type1"}),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check tables and processes
    %%-------------------------------------------------------------
    %% LJ = length(get_table(pmJob)) - 1,
    %% LMR = length(get_table(measurementReader)) - 1,
    ok = wait_table_size(pmJob, LJ + 1),
    ok = wait_table_size(measurementReader, LMR + 1),
    check_processes(2),

    %%-------------------------------------------------------------
    %% initialize application
    %%-------------------------------------------------------------
    start(?SUNE, [{ask, true}]),
    initialize(?SUNE, PmGrps),

    AppPid = wait_subscribe(["Group1"]),
    wait_report(AppPid, [{"Group1",[{"Type1",[4]}]}]),
    wait_report(AppPid, [{"Group1",[{"Type1",[7]}]}]),

    check_processes(3),

    %%-------------------------------------------------------------
    %% delete the MeasurementReader and the pm job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    %%delete_mr("job5", "job5_reader1"),
    delete_job("job6"),
    ok = close_trans(),

    AppPid = wait_subscribe([]),
    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
    ok = wait_table_size(pmsAppRegistry, L),
    check_tables(Tables),
%%     timer:sleep(11000),
%%     check_processes(0),
    stop(?SUNE),
    ok.
    

%%========================================================================
%% one_job_one_of_two_mr(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% @end
%%========================================================================
one_job_one_of_two_mr(_Config) ->
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
    ok = create_job_mr("job7", {"job7_reader1", "Group2", "Type2"}),
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
    wait_report(AppPid, [{"Group2",[{"Type2",[4]}]}]),
    wait_report(AppPid, [{"Group2",[{"Type2",[7]}]}]),

    %%-------------------------------------------------------------
    %% delete the pm job
    %% (it is not possible to remove MR in the current ECIM model)
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    delete_job("job7"),
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
    



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


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
kill(A)            -> ?LIB:kill(A).
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
%%wait_report()            -> ?LIB:wait_report().
%%wait_report(A)           -> ?LIB:wait_report(A).
wait_report(A, B)        -> ?LIB:wait_report(A, B).
%%wait_for_report(A, B, C) -> ?LIB:wait_for_report(A, B, C).

%%create_job(A)       -> ?LIB:create_job(A).
%%update_job(A, B)    -> ?LIB:update_job(A, B).
delete_job(A)       -> ?LIB:delete_job(A).
%%delete_mr(A, B)     -> ?LIB:delete_mr(A, B).
create_job_mr(A, B) -> ?LIB:create_job_mr(A, B).

create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

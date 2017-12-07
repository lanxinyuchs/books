%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_capabilities_SUITE.erl %
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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


%%% @doc 
%%% == PMS Capabilities test suite ==
%%% 
%%% @end

-module(pms_capabilities_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("pms2_test.hrl").

%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-06-17 eolaand     Created
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% R4A/2      2015-07-14 etxjovp     Add group definitions used by CS CI
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


%%-export([alarm_max_meas_mt_mr_1_to_1/1]).
-export([alarm_max_no_of_jobs/1]).
-export([alarm_max_no_of_jobs_pmi2/1]).
%%-export([alarm_max_no_of_jobs_restart/1]).

-compile([nowarn_export_all, export_all]).

%%----------------------------------------------------------
%% Macros
%%----------------------------------------------------------
-define(LIB,  pms_test_lib).
-define(LIB2, pms2_test_lib).

-define(L2B(__L), list_to_binary(__L)).

-define(JOB1, "job1").
-define(JOB2, "job2").
-define(JOB3, "job3").

-define(MR1_1, "job1_reader1").
-define(MR1_2, "job1_reader2").
-define(MR1_3, "job1_reader3").
-define(MR1_4, "job1_reader4").
-define(MR1_5, "job1_reader5").

-define(MR2_1, "job2_reader1").
-define(MR2_2, "job2_reader2").
-define(MR2_3, "job2_reader3").
-define(MR2_4, "job2_reader4").
-define(MR2_5, "job2_reader5").

-define(MR3_1, "job3_reader1").
-define(MR3_2, "job3_reader2").
-define(MR3_3, "job3_reader3").
-define(MR3_4, "job3_reader4").
-define(MR3_5, "job3_reader5").

-define(GROUP1, "Group1").
-define(GROUP2, "Group2").

-define(MTYPE1, "Type1").
-define(MTYPE2, "Type2").
-define(MTYPE3, "Type3").

-define(PM_GRP_1,  [?GROUP1]).
-define(PM_GRP_2,  [?GROUP2]).
-define(PM_GRP_12, [?GROUP1, ?GROUP2]).

-define(TESTNODE, testnode).

-define(TS_TO, 2500).
-define(SLEEP,  500).

-define(SUNE,   sune).
-define(STINA,  stina).
-define(BERIT,  berit).

-define(TEST_APPS,  [?SUNE]).
-define(TEST_APPS2, [?SUNE, ?STINA, ?BERIT]).


-define(SNMP_MGR_NAME, snmp1).
-define(NETCONF_SESSION, nc1).

-define(LICENS_TRAP, [{type, eriAlarmCritical},
		      {eriAlarmActiveSpecificProblem,
		       "License Key File Fault"}]).

-define(WAIT_TRAP_OPTS, [{allowed_traps, [?LICENS_TRAP,
					  [{type, eriAlarmHeartBeatNotif}],
					  [{type,eriAlarmAlarmListRebuilt}]]}]).
-define(TRAP_TIMEOUT, 60).

-define(ALARM_MO, "ManagedElement=1,SystemFunctions=1,Pm=1").
-define(ALARM_SP, "Large Number of Counters").

-define(EXPECT_RAISE, 
	[[{type, eriAlarmWarning},
	  {eriAlarmActiveSpecificProblem, ?ALARM_SP},
	  {eriAlarmActiveManagedObject, ?ALARM_MO}]]).

-define(EXPECT_CLEAR, 
	[[{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, ?ALARM_SP},
	  {eriAlarmActiveManagedObject, ?ALARM_MO}]]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 

    [{ct_hooks, Hooks}] = hooks(),
    [{ct_hooks, Hooks ++ [{rct_snmpmgr, ?SNMP_MGR_NAME}]}].

%% @hidden
init_per_suite(Config) ->
    %%log_msg(),
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    {ok, NMeas} = rpc(pmsDb, meas_capabilities_get, [maxNoOfMeasurements]),
    {ok, NJobs} = rpc(pmsDb, meas_capabilities_get, [maxNoOfJobs]),
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, putenv, ["FAKE_VNFM", ""]);
        _ ->
            rpc(os, unsetenv, ["FAKE_VNFM"])
    end,
    ok = trap_receiver_create(?SNMP_MGR_NAME),
    [{maxNoOfMeasurements, NMeas},
     {maxNoOfJobs, NJobs},
    {reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    NMeas = proplists:get_value(maxNoOfMeasurements, Config),
    rpc(pmsDb, meas_capabilities_update, [maxNoOfMeasurements, NMeas]),
    NJobs = proplists:get_value(maxNoOfJobs, Config),
    rpc(pmsDb, meas_capabilities_update, [maxNoOfJobs, NJobs]),
    %%rpc(pmsDebug, stop_clear, []),
    %% configSnmpMgr(false),
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, unsetenv, ["FAKE_VNFM"]);
        _ ->
            ok
    end,
    ok = trap_receiver_delete().

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
	   %%alarm_max_meas_mt_mr_1_to_1,
	   alarm_max_no_of_jobs,
	   alarm_max_no_of_jobs_pmi2
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


%% This test case does not work because PMS can not count
%% the active measurements correctly. PMS does not know
%% how many instances of application MOs there are.


%%========================================================================
%% alarm_max_meas_mt_mr_1_to_1(Config) -> ok.
%% 
%% \@doc 
%% Set the max number of counters. Then create a job with the same number of 
%% MR:s with one counter each and verify that a Large Number of Counters alarm 
%% is raised. Delete the job and verifiy that the alarm is cleared. 
%% \@end
%%
%%========================================================================
%% alarm_max_meas_mt_mr_1_to_1(_Config) ->
%%     %% Tables = tables(),
%%     rpc(pmsDb, meas_capabilities_update, [maxNoOfMeasurements, 2]),
%%     %% LJ  = length(get_table(pmJob)),
%%     %% LMR = length(get_table(measurementReader)),

%%     %%-------------------------------------------------------------
%%     %% Prepare tap receiver for alarm
%%     %%-------------------------------------------------------------
%%     wait_for_traps(?EXPECT_RAISE),
    
%%     {ok, _} = open_trans(),
%%     ok = create_job_mr(?JOB1, [
%% 			       {?MR1_1, ?GROUP1, ?MTYPE1},
%% 			       {?MR1_2, ?GROUP2, ?MTYPE2},
%% 			       {?MR1_3, ?GROUP2, ?MTYPE3}
%% 			      ]),
%%     ok = close_trans(),
    
%%     %% ok = wait_table_size(pmJob, LJ + 1),
%%     %% ok = wait_table_size(measurementReader, LMR + 2),

%%     %%-------------------------------------------------------------
%%     %% Check that alarm is raised
%%     %%-------------------------------------------------------------
%%     ok = rct_snmpmgr:check_traps(),

%%     %%-------------------------------------------------------------
%%     %% Verify that create of additional MR fails.
%%     %%-------------------------------------------------------------
%%     {ok, _} = open_trans(),
%%     ok = create_job_mr(?JOB2, [
%% 			       {?MR2_1, ?GROUP2, ?MTYPE3}
%% 			      ]),
%%     {error, Error} = close_trans(),

%%     ct:log("Failed to create another job MR: ~p", [Error]),
    
%%     %%-------------------------------------------------------------
%%     %% Prepare tap receiver for clear alarm
%%     %%-------------------------------------------------------------
%%     wait_for_traps(?EXPECT_CLEAR),

%%     %%-------------------------------------------------------------
%%     %% Delete the PM Job and its MeasurementReaders
%%     %%-------------------------------------------------------------
%%     {ok, _} = open_trans(),
%%     ok = delete_job(?JOB1),
%%     ok = close_trans(),

%%     %%-------------------------------------------------------------
%%     %% Check that alarm is cleared
%%     %%-------------------------------------------------------------
%%     ok = rct_snmpmgr:check_traps().
%%     %% check_tables(Tables),
%%     %% check_processes(0),


%%========================================================================
%% alarm_max_no_of_jobs_pmi2(Config) -> ok.
%% 
%% @doc 
%% Set the max allowed number of jobs. Then create the same number of jobs 
%% and verify that a Large Number of Counters alarm is raised.
%% Delete one job and verifiy that the alarm is cleared. 
%% @end
%%
%%========================================================================
alarm_max_no_of_jobs_pmi2(_Config) ->
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    App = ?SUNE,
    {ok, Handle} = ?LIB2:create_app(App, ?CM_GRP_DEF),
    
    rpc(pmsDb, meas_capabilities_update, [maxNoOfJobs, 2]),
    rpc(pmsDb, meas_capabilities_update, [maxNoOfMeasurements, 4]),

    %%-------------------------------------------------------------
    %% Prepare tap receiver for alarm
    %%-------------------------------------------------------------
    wait_for_traps(?EXPECT_RAISE),
    
    SubscrOpt = [{pmi2SubscribeRop, []},
		 {pmi2SubscribeRop, []},
		 {pmi2SubscribeRop, []},
		 {pmi2ReportRop, {repeat, wait_until_subscribe}, []},
		 {pmi2SubscribeRop, []}],
    {ok, _} = ?LIB2:expected(App, Handle, SubscrOpt),

    {ok, _} = open_trans(),
    ok = ?LIB2:create_job_mr(?JOB1, [{?MR1_1, ?GROUP1, ?MTYPE1}]),
    ok = ?LIB2:create_job_mr(?JOB2, [{?MR2_1, ?GROUP2, ?MTYPE2}]),
    ok = ?LIB2:create_job_mr(?JOB3, [{?MR3_1, ?GROUP2, ?MTYPE3}]),
    ok = close_trans(),
    
    %%-------------------------------------------------------------
    %% Check that alarm is raised
    %%-------------------------------------------------------------
    ok = rct_snmpmgr:check_traps(),

    %%-------------------------------------------------------------
    %% Verify that create of additional Job fails.
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = ?LIB2:create_job_mr(?JOB3, [{?MR3_1, ?GROUP2, ?MTYPE3}]),
    {error, Error} = close_trans(),

    ct:log("Failed to create another job: ~p", [Error]),
    
    %%-------------------------------------------------------------
    %% Prepare tap receiver for clear alarm
    %%-------------------------------------------------------------
    wait_for_traps(?EXPECT_CLEAR),

    %%-------------------------------------------------------------
    %% Delete a PM job
    %%-------------------------------------------------------------
    SubscrOpt2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
		  {pmi2SubscribeRop, []},
		  {pmi2ReportRop, {repeat, wait_until_subscribe}, []},
		  {pmi2SubscribeRop, []},
		  {pmi2ReportRop, {repeat, wait_until_subscribe}, []},
		  {pmi2SubscribeRop, []}],
    {ok, _} = ?LIB2:expected(App, Handle, SubscrOpt2),

    {ok, _} = open_trans(),
    ok = delete_job(?JOB2),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% Check that alarm is cleared
    %%-------------------------------------------------------------
    ok = rct_snmpmgr:check_traps(),

    %%-------------------------------------------------------------
    %% Delete remaining PM jobs
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = ?LIB2:delete_job(?JOB1),
    ok = ?LIB2:delete_job(?JOB3),
    ok = close_trans(),
    
    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ok = ?LIB2:delete_app(App, Handle),
    ok.



%%========================================================================
%% alarm_max_no_of_jobs(Config) -> ok.
%% 
%% @doc 
%% Set the max allowed number of jobs. Then create the same number of jobs 
%% and verify that a Large Number of Counters alarm is raised.
%% Delete one job and verifiy that the alarm is cleared. 
%% @end
%%
%%========================================================================
alarm_max_no_of_jobs(_Config) ->
    PmGrps = ["Group1", "Group2"],

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    {ok, _} = start(?SUNE),
    initialize(?SUNE, PmGrps),    
    

    rpc(pmsDb, meas_capabilities_update, [maxNoOfJobs, 2]),
    rpc(pmsDb, meas_capabilities_update, [maxNoOfMeasurements, 4]),

    %%-------------------------------------------------------------
    %% Prepare tap receiver for alarm
    %%-------------------------------------------------------------
    wait_for_traps(?EXPECT_RAISE),
    
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?GROUP1, ?MTYPE1}]),
    ok = create_job_mr(?JOB2, [{?MR2_1, ?GROUP2, ?MTYPE2}]),
    ok = create_job_mr(?JOB3, [{?MR3_1, ?GROUP2, ?MTYPE3}]),
    ok = close_trans(),
    
    %%-------------------------------------------------------------
    %% Check that alarm is raised
    %%-------------------------------------------------------------
    ok = rct_snmpmgr:check_traps(),

    %%-------------------------------------------------------------
    %% Verify that create of additional Job fails.
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB3, [{?MR3_1, ?GROUP2, ?MTYPE3}]),
    {error, Error} = close_trans(),

    ct:log("Failed to create another job: ~p", [Error]),
    
    %%-------------------------------------------------------------
    %% Prepare tap receiver for clear alarm
    %%-------------------------------------------------------------
    wait_for_traps(?EXPECT_CLEAR),

    %%-------------------------------------------------------------
    %% Delete a PM job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job(?JOB2),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% Check that alarm is cleared
    %%-------------------------------------------------------------
    ok = rct_snmpmgr:check_traps(),

    %%-------------------------------------------------------------
    %% Delete remaining PM jobs
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = delete_job(?JOB1),
    ok = delete_job(?JOB3),
    ok = close_trans(),
    
    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE),
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


start(A)    -> {ok, _} = pms_erl_app:start(A).
start(A, B) -> {ok, _} = pms_erl_app:start(A, B).
stop(A)     -> ok = pms_erl_app:stop(A).   

initialize(A)      -> ok = ?LIB:initialize(A).
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

create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

%%-------------------------------------------------------------
%% Trap receiver create
%%-------------------------------------------------------------

trap_receiver_create(MgrName) ->
    Attrs = rct_snmpmgr:get_attrs_trap_receiver_create(MgrName),
    netconf_edit(Attrs).

%%-------------------------------------------------------------
%% Trap receiver delete
%%-------------------------------------------------------------
trap_receiver_delete() ->
    Attrs = rct_snmpmgr:get_attrs_trap_receiver_delete(),
    netconf_edit(Attrs).
    
%%-------------------------------------------------------------
%% NetConf edit trap receiver
%%-------------------------------------------------------------
netconf_edit(Attrs) ->
    try
	{ok,_} = ct_netconfc:open(?NETCONF_SESSION,[]),
	ct_netconfc:edit_config(?NETCONF_SESSION, running, Attrs),
	ok = ct_netconfc:close_session(?NETCONF_SESSION),
	timer:sleep(5000),
	ok
    catch
	_:Reason ->
	    ct_netconfc:close_session(?NETCONF_SESSION),
	    ct:fail(Reason)
    end.

%%-------------------------------------------------------------
%% Prepare the trap receiver for receiving expeted trap.
%%-------------------------------------------------------------
wait_for_traps(TrapsExpected) ->
    rct_snmpmgr:wait_for_traps(TrapsExpected, ?WAIT_TRAP_OPTS, ?TRAP_TIMEOUT).
    

wait_node_up() ->
    timer:sleep(5000),
    Rpc = rpc(gen_server, call, [pmsServer, get_current_state]),
    wnup(Rpc, 30).

wnup(ok, _) ->
    timer:sleep(3000);
wnup(Rpc, N) when N =< 0 ->
    {error, Rpc};
wnup(R, N) ->
    ct:pal("##### ~p~n", [R]),
    timer:sleep(1000),
    Rpc = rpc(gen_server, call, [pmsServer, get_current_state]),
    wnup(Rpc, N - 1).


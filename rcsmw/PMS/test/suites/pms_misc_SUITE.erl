%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_misc_SUITE.erl %
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/R8A/1

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms_misc_SUITE).
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
%%% R1A/1      2013-04-16 uabesvi     Created
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


-export([comte/1]).
-export([lib/1]).
-export([db/1]).
-export([db_set/1]).
-export([server/1]).
-export([app_registry/1]).
-export([show_counters/1]).
-export([debug/1]).
-export([app_job/1]).
-export([app_job2/1]).


-define(LIB, pms2_test_lib).
-define(PROXY, pms_pmi_proxy).

-define(L2B(__L), list_to_binary(__L)).



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
    ProxyPars = {?PROXY, get_ct_config(?PROXY, [{erl_api, false}])},
    Hooks     = lists:keystore(?PROXY, 1, hooks(), ProxyPars),
    [
     {ct_hooks, Hooks},
     {timetrap, {seconds, 60}}
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
    %% [catch ?ERL_APP:stop(App) || App <- ?TEST_APPS],
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    All = [
	  ],
    All ++ all_host(host()).

%% all_host(?TESTNODE) ->
%%     [];
all_host(_PmsSim) ->
    [
     comte,
     lib,
     db,
     db_set,
     server,
     app_registry,
     show_counters,
     debug,
     app_job,
     app_job2
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
%% comte(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
comte(_Config) ->
    rpc(pmsComteI, prepare, [dn, user, tid]),
    rpc(pmsComteI, commit,  [dn, user, tid]),
    rpc(pmsComteI, finish,  [dn, user, tid]),
    rpc(pmsComteI, action,  [dn, user, tid]),
    rpc(pmsComteI, action,  [dn, user, params, tid]),


    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    Job = "comte",
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create a job
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    timer:sleep(5000),

    MC = rpc(pmsComteI, getMoAttribute, [
					 [
					  <<"moClass">>,
					  <<"Group1">>,
					  <<"PmGroup">>,
					  <<"1">>,
					  <<"Pm">>,
					  <<"1">>,
					  <<"SystemFunctions">>,
					  <<"1">>,
					  <<"ManagedElement">>
					 ],
					 tid
					]),
    ct:pal("MC ~p~n", [MC]),

    PmGrpId = rpc(pmsComteI, getMoAttribute, [
					      [
					       <<"pmGroupId">>,
					       <<"Group1">>,
					       <<"PmGroup">>,
					       <<"1">>,
					       <<"Pm">>,
					       <<"1">>,
					       <<"SystemFunctions">>,
					       <<"1">>,
					       <<"ManagedElement">>
					      ],
					      tid
					     ]),
    ct:pal("PmGrpId ~p~n", [PmGrpId]),

    MtId = rpc(pmsComteI, getMoAttribute, [
					   [
					    <<"measurementTypeId">>,
					    <<"Type1">>,
					    <<"MeasurementType">>,
					    <<"Group1">>,
					    <<"PmGroup">>,
					    <<"1">>,
					    <<"Pm">>,
					    <<"1">>,
					    <<"SystemFunctions">>,
					    <<"1">>,
					    <<"ManagedElement">>
					   ],
					   tid
					  ]),
    ct:pal("MtId ~p~n", [MtId]),

    Mcap = rpc(pmsComteI, getMoAttribute, [
					   [
					    <<"pmMeasurementCapabilitiesId">>,
					    <<"1">>,
					    <<"PmMeasurementCapabilities">>,
					    <<"1">>,
					    <<"Pm">>,
					    <<"1">>,
					    <<"SystemFunctions">>,
					    <<"1">>,
					    <<"ManagedElement">>
					   ],
					   tid
					  ]),
    ct:pal("Mcap ~p~n", [Mcap]),

    MRNV = rpc(pmsComteI, getMoAttribute, [
					   [
					    <<"measurementReaderNameValue">>,
					    <<"jobbet_reader1">>,
					    <<"MeasurementReader">>,
					    <<"job1">>,
					    <<"PmJob">>,
					    <<"1">>,
					    <<"Pm">>,
					    <<"1">>,
					    <<"SystemFunctions">>,
					    <<"1">>,
					    <<"ManagedElement">>
					   ],
					   tid
					  ]),
    ct:pal("MRNV ~p~n", [MRNV]),
    
    MS = rpc(pmsComteI, getMoAttribute, [
					 [
					  <<"measurementSpecification">>,
					  <<"jobbet_reader1">>,
					  <<"MeasurementReader">>,
					  <<"job1">>,
					  <<"PmJob">>,
					  <<"1">>,
					  <<"Pm">>,
					  <<"1">>,
					  <<"SystemFunctions">>,
					  <<"1">>,
					  <<"ManagedElement">>
					 ],
					 tid
					]),
    ct:pal("MS ~p~n", [MS]),


    PmId = rpc(pmsComteI, getMoAttribute, [
					   [
					    <<"pmId">>,
					    <<"1">>,
					    <<"Pm">>,
					    <<"1">>,
					    <<"SystemFunctions">>,
					    <<"1">>,
					    <<"ManagedElement">>
					   ],
					   tid
					  ]),
    ct:pal("PmId ~p~n", [PmId]),


    delete_jobs(Job),
    ok.


%%========================================================================
%% lib(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
lib(_Config) ->
    Int = [rpc(pmsLib, get_interval, [X]) || X <- [1,2,3,4,5,6,7,8,9]],
    ct:pal("Interval ~p~n", [Int]),

    rpc(pmsLib, get_gp_enum, [10]),
    rpc(pmsLib, get_gp_enum, [30]),
    rpc(pmsLib, get_gp_enum, [60]),
    rpc(pmsLib, get_gp_enum, [60*5]),
    rpc(pmsLib, get_gp_enum, [60*15]),
    rpc(pmsLib, get_gp_enum, [60*30]),
    rpc(pmsLib, get_gp_enum, [60*60]),
    rpc(pmsLib, get_gp_enum, [60*60*12]),
    rpc(pmsLib, get_gp_enum, [60*60*24]),
    rpc(pmsLib, get_report_offset, [3]),
    rpc(pmsLib, get_report_offset, [4]),
    rpc(pmsLib, get_report_offset, [10]),
    rpc(pmsLib, get_meas_info_offset, [3]),
    rpc(pmsLib, get_meas_info_offset, [4]),
    rpc(pmsLib, get_meas_info_offset, [10]),
    rpc(pmsLib, get_rop_data_to, [3]),
    rpc(pmsLib, get_rop_data_to, [4]),
    rpc(pmsLib, get_rop_data_to, [10]),
    rpc(pmsLib, get_rop_offset, [3]),
    rpc(pmsLib, get_rop_offset, [4]),
    rpc(pmsLib, get_rop_offset, [10]),

    {MegS, S, _} = os:timestamp(),
    {_, Time}    = calendar:now_to_local_time({MegS, S, 0}),

    SFH = [rpc(pmsLib, sync_full_hour, [X, Time]) || X <- [1,2,3,4,5,6,7,8,9]],
    ct:pal("sync full hour ~p~n", [SFH]),

    ok.


%%========================================================================
%% db(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
db(_Config) ->
    rpc(pmsDb, measurement_type_get, [{"1","1","1","Group1","Type1"}]),
    rpc(pmsDb, measurement_type_get, [{"1","1","1","Group1","Type1"}, size]),

    rpc(pmsDb, pm_group_match, [{"1","1","1"}]),
    rpc(pmsDb, pm_job_match, [{"1","1","1"}]),
    rpc(pmsDb, measurement_reader_match_pm_group, ["Group1"]),
    rpc(pmsDb, measurement_reader_match_pm_type, 
	[{"1","1","1","Group1","Type1"}]),
    rpc(pmsDb, measurement_reader_match_meas_type, ["Group1","Type1"]),
    rpc(pmsDb, measurement_type_dirty_match, [{"1","1","1","Group1"}]),
    rpc(pmsDb, app_reg_get, [["NonExistGroup1"]]),
    rpc(pmsDb, meas_capabilities_get, []),
    rpc(pmsDb, meas_capabilities_update, [finalROP, false]),
    rpc(pmsDb, pm_job_all_keys, []),
    rpc(pmsDb, pms_env_get, [undef]),
    rpc(pmsDb, rop_file_get_oldest, []),
    rpc(pmsDb, rop_file_get_oldest, ["/no/real/dir"]),
    rpc(pmsDb, rop_file_delete_oldest, []),
    rpc(pmsDb, rop_info_get_existing, ["/no/real/dir", "no_real_file"]),
    rpc(pmsDb, rop_get_oldest, ["/no/real/dir"]),
    rpc(pmsDb, rop_get_oldest_file, ["/no/real/dir", "no_real_file"]),
    rpc(pmsDb, get_no_of_rop_files, ["/no/real/dir"]),
    rpc(pmsDb, pm_job_delete_object, ["no_job"]),
    ok.


%%========================================================================
%% db_set(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
db_set(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    Job = "db_set",
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create a job
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    timer:sleep(5000),

    %%-------------------------------------------------------------
    %% tests
    %%-------------------------------------------------------------
    {ok, [MR]} = rpc(pmsDb,
		     measurement_reader_get,
		     [{"1","1","1","db_set","ROPType1Sum"}]),
    ok = rpc(pmsDb, measurement_reader_set, [MR]),
    
    {ok, [MT]} = rpc(pmsDb,
		     measurement_type_get,
		     [{"1","1","1","Group1","Type1"}]),
    ok = rpc(pmsDb, measurement_type_set, [MT]),

    {ok, [Grp]} = rpc(pmsDb, pm_group_get, [{"1","1","1","Group1"}]),
    ok = rpc(pmsDb, pm_group_set, [Grp]),

    {ok, PmCap} = rpc(pmsDb, meas_capabilities_get, []),
    ok = rpc(pmsDb, meas_capabilities_set, [PmCap]),

    MoC = rpc(pmsDb, pms_mo_class_get, ["Router"]),
    {ok, _} = rpc(pmsDb, pms_mo_class_set, [MoC]),

    Aliases = rpc(pmsDb, pms_counter_aliases_get, ["fake"]),
    ok = rpc(pmsDb, pms_counter_aliases_set, ["fake", Aliases]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job).

%%========================================================================
%% server(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
server(_Config) ->
    rpc(pmsServer, get_current_state, [{"1","1","1","job"}]),
    rpc(gen_server, call, [pmsServer, get_loop_data]),
    rpc(gen_server, call, [pmsServer, "unknown call"]),
    rpc(gen_server, cast, [pmsServer, {test_terminate, normal}]),
    rpc(gen_server, cast, [pmsServer, "unknown cast"]),
    ok.

%%========================================================================
%% app_registry(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
app_registry(_Config) ->
    rpc(gen_server, call, [pmsAppRegistry, invalid_msg]),
    rpc(gen_server, cast, [pmsAppRegistry, invalid_msg]),
    rpc(gen_server, cast, [pmsAppRegistry, {test_terminate, normal}]),
    ok.


%%========================================================================
%% show_counters(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
show_counters(_Config) ->
    rpc(pmsShowCountersI, start, [arg]),
    rpc(pmsShowCountersI, get_measurements, [<<>>, [], []]),
    rpc(pmsShowCountersI, get_job_ids, [<<"ManagedElement=1">>]),
    ok.


%%========================================================================
%% debug(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
debug(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = testing_debug,
    Job = "db_set",
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),

    %%-------------------------------------------------------------
    %% Create a job
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    timer:sleep(5000),

    %%-------------------------------------------------------------
    %% tests
    %%-------------------------------------------------------------
    rpc(pmsDebug, procs, []),
    rpc(pmsDebug, tables, []),
    rpc(pmsDebug, aliases, []),
    rpc(pmsDebug, get_aliases, []),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% app_job(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
app_job(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = app_job,
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app_pmi(App, [?ROPGroup1]),

    {ok, Rec} = wait_rec(100),
    Pid = element(3, Rec),

    %%-------------------------------------------------------------
    %% tests
    %%-------------------------------------------------------------
    rpc(pmsAppJob, gld, [Pid, self()]),
    LoopData = receive 
		   {loop_data, LD} ->
		       LD
	       after 5000 ->
		       ct:fail({error, timeout_loop_data})
	       end,
    SesPid = proplists:get_value(session_pid, LoopData),
    gen_server:call(SesPid, "unknown call"),
    gen_server:cast(SesPid, "unknown cast"),
    SesPid ! "unknown info",

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% app_job2(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
app_job2(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = app_job2,
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),

    {ok, Rec} = wait_rec(100),
    Pid = element(3, Rec),

    %%-------------------------------------------------------------
    %% tests
    %%-------------------------------------------------------------
    rpc(pmsAppJob, get_aliases, [Pid, self()]),
    receive 
	{aliases_data, _} ->
	    ok
    after 5000 ->
 	    ct:fail({error, timeout_aliases})
    end,
    
    rpc(pmsAppJob, gld, [Pid, self()]),
    LoopData = receive 
		   {loop_data, LD} ->
		       LD
	       after 5000 ->
		       ct:fail({error, timeout_loop_data})
	       end,

    SesPid = proplists:get_value(session_pid, LoopData),
    gen_server:call(SesPid, "unknown call"),
    gen_server:cast(SesPid, "unknown cast"),
    SesPid ! "unknown info",


    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).




wait_rec(N) when N < 1 ->
    {error, "pmsAppRegistry empty"};
wait_rec(N) ->
    case rpc(ets, tab2list, [pmsAppRegistry]) of
        [Rec | _] -> 	
	    {ok, Rec};
	[] ->
	    timer:sleep(100),
	    wait_rec(N - 1)
    end.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


get_ct_config(TC, DefVal) -> ?LIB:get_ct_config(TC, DefVal).

cleanup()          -> ?LIB:cleanup().

rpc(M, F, A) -> ?LIB:rpc(M, F, A).
host()        -> ?LIB:host().
hooks()       -> ?LIB:hooks().
%%open_trans()  -> ?LIB:open_trans().
close_trans() -> ?LIB:close_trans().

create_app(A)        -> ?LIB:create_app(A).
delete_app(A, B)     -> ?LIB:delete_app(A, B).
create_app_pmi(A, B) -> ?LIB:create_app_pmi(A, B).

create_job(A, B) -> ?LIB:create_job(A, B).
delete_jobs(A)   -> ?LIB:delete_jobs(A).
get_mrs(A)       -> ?LIB:get_mrs(A).

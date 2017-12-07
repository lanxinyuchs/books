%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_upgrade_SUITE.erl %
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/1

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms_upgrade_SUITE).
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
%%% R1A/1      2013-04-16 uabesvi     Created
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


-export([session_restart_pmssim/1]).
-export([appreg_restart_pmssim/1]).
-export([appdata_job/1]).
-export([invalid_job/1]).
-export([upgrade/1]).
-export([no_grp_upgrade/1]).


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
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_group(_Group, Config) ->
    Config.

%% @hidden
end_per_group(_Group, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    cleanup(),
    [pms_erl_app:start(App) || App <- ?TEST_APPS],
    Config.

%% @hidden
end_per_testcase(session_kill_app_proc, _Config) ->
    cleanup(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    cleanup(),
    close_trans(),
    [pms_erl_app:stop(App) || App <- ?TEST_APPS],
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

all_host(?TESTNODE) ->
    [];
all_host(_PmsSim) ->
    [
     session_restart_pmssim,
     appreg_restart_pmssim,
     appdata_job,
     invalid_job,
     upgrade,
     no_grp_upgrade
    ].




groups() ->
    [].


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------

%%========================================================================
%% session_restart_pmssim(Config) -> ok.
%% 
%% @doc 
%% Kill the session process and parse the appdata file again
%% @end
%%========================================================================
session_restart_pmssim(_Config) ->
    mfa(gen_server, cast, [pmsServer, {test_terminate, normal}]),
    mfa(pmsServer, start, []),
    appdata("pms_app.xml", "session_restart_pmssim", "R2A"),
    mfa(pmsDataInit, init, [node]),
    mfa(pmsDataInit, init_data, []),

    mfa(gen_server, call, [pmsServer, test_suite]),
    mfa(gen_server, cast, [pmsServer, test_suite]),
    mfa(gen_server, info, [pmsServer, test_suite]),
    ok.


%%========================================================================
%% appreg_restart_pmssim(Config) -> ok.
%% 
%% @doc 
%% Kill the app registry process and parse the appdata file again
%% @end
%%========================================================================
appreg_restart_pmssim(_Config) ->
    mfa(gen_server, cast, [pmsAppRegistry, {test_terminate, normal}]),
    mfa(pmsAppRegistry, start, []),

    mfa(gen_server, call, [pmsAppRegistry, test_suite]),
    mfa(gen_server, cast, [pmsAppRegistry, test_suite]),
    mfa(gen_server, info, [pmsAppRegistry, test_suite]),
    ok.

%%========================================================================
%% appdata_job(Config) -> ok.
%% 
%% @doc 
%% Kill the session process and parse the appdata file again
%% @end
%%========================================================================
appdata_job(_Config) ->
    check_processes(0),
    appdata("pms_job_app.xml", "appdata_job", "R2A"),
    to(5),
    check_processes(3),
    ok.

%%========================================================================
%% invalid_job(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
invalid_job(_Config) ->
    check_processes(0),
    appdata("invalid_job.xml", "appdata_job", "R2A"),
    to(5),
    check_processes(2),
    ok.
%%========================================================================
%% upgrade(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
upgrade(_Config) ->
    _Tables = tables(),
    %%====================================================
    %% prepare for the upgrade
    %%====================================================
    ct:pal("upgrade to pms_job_app~n"),
    set_upgrade(true),
    term_server(),
    term_appreg(),
    delete_tables([pm,
		   pmGroup, 
		   measurementType, 
		   pmsAppRegistry, 
		   pmMeasurementCapabilities]), 

    %%====================================================
    %% upgrade using pms_job_app
    %%====================================================
    restart_pms(),
    appdata("pms_job_app.xml", "upgrade", "R1A"),
    set_upgrade(false),

    to(2),
    check_processes(3),

    %%====================================================
    %% prepare for the upgrade
    %%====================================================
    ct:pal("upgrade to pms_app~n"),
    set_upgrade(true),
    term_server(),
    term_appreg(),

    %%====================================================
    %% upgrade using pms_app
    %%====================================================
    restart_pms(),
    appdata("pms_app.xml", "upgrade", "R2A"),
    set_upgrade(false),
    to(2),

    %% removed until the system defined jobs can be removed at upgrade
    %% i.e. pmJobSD class is introduced to ECIM model
    %% check_processes(0),
    %% check_tables(Tables), 

    
    ok.


%%========================================================================
%% no_grp_upgrade(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
no_grp_upgrade(_Config) ->
    _Tables = tables(),
    %%====================================================
    %% prepare for the upgrade
    %%====================================================
    ct:pal("upgrade to pms_job_app~n"),
    set_upgrade(true),
    term_server(),
    term_appreg(),
    delete_tables([pm,
		   pmGroup, 
		   measurementType, 
		   pmsAppRegistry, 
		   pmMeasurementCapabilities]), 
    check_processes(0),

    %%====================================================
    %% upgrade using pms_job_app
    %%====================================================
    restart_pms(),
    appdata("pms_no_grp.xml", "no_grp_upgrade", "R1A"),
    set_upgrade(false),

    to(20),
    check_processes(0),
    [[]] = tables([pmGroup]),
    [[]] = tables([measurementType]),
    [[]] = tables([pmJob]),
    [[]] = tables([measurementReader]),
    

    %%====================================================
    %% prepare for the upgrade
    %%====================================================
    ct:pal("upgrade to pms_app~n"),
    set_upgrade(true),
    term_server(),
    term_appreg(),

    %%====================================================
    %% upgrade using pms_app
    %%====================================================
    restart_pms(),
    appdata("pms_app.xml", "no_grp_upgrade", "R2A"),
    set_upgrade(false),

    check_processes(0),
    %% removed until the system defined jobs can be removed at upgrade
    %% i.e. pmJobSD class is introduced to ECIM model
    %% check_tables(Tables), 

    
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




initialize(A)      -> ?LIB:initialize(A).
initialize(A, B)   -> ?LIB:initialize(A, B).
finalize(A)        -> ?LIB:finalize(A).
ask(A)             -> ?LIB:ask(A).
kill(A)            -> ?LIB:kill(A).
kill_proc(A, B)    -> ?LIB:kill_proc(A, B).
mfa(M, F, A)       -> ?LIB:mfa(M, F, A).
get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
tables(A)          -> ?LIB:tables(A).
check_tables(A)    -> ?LIB:check_tables(A).
cleanup_tables(A)  -> ?LIB:cleanup_tables(A).
delete_tables(A)   -> ?LIB:delete_tables(A).
%%get_processes()  -> ?LIB:get_processes().
check_processes(A) -> ?LIB:check_processes(A).
cleanup()          -> ?LIB:cleanup().
appdata(A, B, C)   -> ?LIB:appdata(A, B, C).

wait_subscribe(A)            -> ?LIB:wait_subscribe(A).
%%wait_subscribe(A, B)       -> ?LIB:wait_subscribe(A, B).
wait_subscribe(A, B, C)      -> ?LIB:wait_subscribe(A, B, C).
%%wait_subscribe(A, B, C, D) -> ?LIB:wait_subscribe(A, B, C, D).
wait_report()                -> ?LIB:wait_report().
wait_report(A)               -> ?LIB:wait_report(A).
wait_report(A, B)            -> ?LIB:wait_report(A, B).
%%wait_report(A, B, C)       -> ?LIB:wait_report(A, B, C).
wait_for_report(A, B)        -> ?LIB:wait_for_report(A, B).
%%wait_for_report(A, B, C)   -> ?LIB:wait_for_report(A, B, C).

create_job(A)         -> ?LIB:create_job(A).
create_job(A, B)      -> ?LIB:create_job(A, B).
update_job(A, B)      -> ?LIB:update_job(A, B).
%%update_job(A, B, C) -> ?LIB:update_job(A, B, C).
delete_job(A)         -> ?LIB:delete_job(A).
%%delete_job(A, B)    -> ?LIB:delete_job(A, B).
create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).
delete_mr(A, B)       -> ?LIB:delete_mr(A, B).


get_upgrade()  -> ?LIB:get_upgrade().
set_upgrade(B) -> ?LIB:set_upgrade(B).
kill_server()  -> ?LIB:kill_server().
term_server()  -> ?LIB:term_server().
term_appreg()  -> ?LIB:term_appreg().
restart_pms()  -> ?LIB:restart_pms().


%%rpc(M, F, A) -> ?LIB:rpc(M, F, A).
%%module() -> ?LIB:module().
host()        -> ?LIB:host().
hooks()       -> ?LIB:hooks().
open_trans()  -> ?LIB:open_trans().
close_trans() -> ?LIB:close_trans().

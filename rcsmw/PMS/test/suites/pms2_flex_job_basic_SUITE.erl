%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_flex_job_basic_SUITE.erl %
%%% @version /main/R6A/R7A/R11A/2

%%% @doc 
%%% 
%%% 
%%% @end

-module(pms2_flex_job_basic_SUITE).

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
%%% R6A/1      2016-04-03 uabesvi     Created
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
-export([session_two_apps/1]).

-export([counter_maps/1]).
-export([counter_maps_redefine/1]).

-export([one_job_one_mr/1]).
-export([one_job_two_mr/1]).
-export([add_mr_failed/1]).

-export([active_stop_active/1]).

-export([three_apps_same_jobs/1]).
-export([two_apps_diff_jobs/1]).

-export([fragmented/1]).
-export([no_rop_data/1]).
-export([multi_ldn/1]).
-export([multi_values/1]).
-export([suspect_values/1]).
-export([ignore_rop/1]).
-export([delay_rop/1]).

-export([wrong_expected_msg/1]).

-export([two_jobs_separate_trans/1]).
-export([three_jobs_separate_trans/1]).
-export([same_app_sequence_jobs/1]).



-define(TESTNODE, testnode).

-define(LIB,     pms2_test_lib).
-define(ERL_APP, pms2_erl_app).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).


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
end_per_testcase(_TestCase, Config) ->
    log_rop_files(Config),
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
	   session_init_finalize,
	   session_two_apps,
	   counter_maps,
	   counter_maps_redefine,
	   one_job_one_mr,
	   one_job_two_mr,
	   add_mr_failed,
	   active_stop_active,
	   three_apps_same_jobs,
%%	   two_apps_diff_jobs,
	   fragmented,
	   no_rop_data,
	   multi_ldn,
	   multi_values,
	   suspect_values,
	   ignore_rop,
	   delay_rop,
	   same_app_sequence_jobs
%%	   two_jobs_separate_trans
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
%% session_init_finalize(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize an application
%% @end
%%========================================================================
session_init_finalize(_Config) ->
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    {ok, Sune} = initialize(?SUNE, ?CB_DEF, ?COUNTER_MAP_FLEX),    
    
    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE, Sune),
    check_processes(0),
    stop(?SUNE),
    ok.


%%========================================================================
%% session_two_apps(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize two applications
%% @end
%%========================================================================
session_two_apps(_Config) ->
    Tables = tables(),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    start(?TINA),

    {ok, Sune} = initialize(?SUNE, ?CB_DEF, ?COUNTER_MAP_FLEX),
    {ok, Tina} = initialize(?TINA, ?CB_DEF, ?COUNTER_MAP_FLEX),

    14 = length(get_table(pmsAppRegistry)),

    finalize(?SUNE, Sune),
    stop(?SUNE),
    finalize(?TINA, Tina),
    stop(?TINA),
    
    0 = length(get_table(pmsAppRegistry)),
    check_tables(Tables),
    check_processes(0),
    ok.
    

%%========================================================================
%% counter_maps(Config) -> ok.
%% 
%% @doc 
%% Start one application, add counters to the session.
%% @end
%%========================================================================
counter_maps(_Config) ->
    Tables = tables(),

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    {ok, Sune} = initialize(?SUNE, ?CB_DEF, undefined),    

    %%-------------------------------------------------------------
    %% send counter_map
    %%-------------------------------------------------------------
    counter_map(?SUNE, Sune, [{"PmFlexGroup1", 1, [{"PmFlexType1", 1}]}]),    
    counter_map(?SUNE, Sune, [{"PmFlexGroup2", 2, [{"PmFlexType2", 34}]}]),    
    to(),
    2 = length(get_table(pmsAppRegistry)),

    %%-------------------------------------------------------------
    %% check aliases
    %%-------------------------------------------------------------
    [{_, Aliases}] = get_aliases(),
    ct:pal("Aliases:~n~p", [Aliases]),

    true = lists:member({1, <<"PmFlexGroup1">>}, Aliases),
    true = lists:member({2, <<"PmFlexGroup2">>}, Aliases),
    true = lists:member({{1, 1},  <<"PmFlexType1">>}, Aliases),
    true = lists:member({{2, 34}, <<"PmFlexType2">>}, Aliases),

    %%-------------------------------------------------------------
    %% check that both applications have registered the groups
    %%-------------------------------------------------------------
    ok = wait_table_size(pmsAppRegistry, 2),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE, Sune),
    0 = length(get_table(pmsAppRegistry)),
    check_tables(Tables),    
    check_processes(0),

    stop(?SUNE),
    ok.


%%========================================================================
%% counter_maps_redefine(Config) -> ok.
%% 
%% @doc 
%% redefine aliases for pm groups and measurement readers
%% @end
%%========================================================================
counter_maps_redefine(_Config) ->

    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    {ok, Handle} = initialize(?SUNE, ?CB_DEF, undefined),    
    cmr(?SUNE, Handle),

    %%-------------------------------------------------------------
    %% add another map and redo the test
    %%-------------------------------------------------------------
    counter_map(?SUNE, Handle, [{"ExtraG", 6, [{"Emt", 1}]}]),    
    cmr(?SUNE, Handle),

    %%-------------------------------------------------------------
    %% test multi MTs
    %%-------------------------------------------------------------
    counter_map(?SUNE, Handle, [{"PmFlexGroup1", 1, [{"PmFlexType1", 1}]}]),    

    counter_map(?SUNE, Handle, [{"PmFlexGroup1", 1, [{"PmFlexType2", 2}]}]),    

    counter_map(?SUNE, Handle, [{"PmFlexGroup1", 1, [{"PmFlexType3", 3}]}]),    

    counter_map(?SUNE, Handle, [{"PmFlexGroup1", 1, [{"PmFlexType2", 4}]}]),    

    counter_map(?SUNE, Handle, [{"PmFlexGroup1", 1, [{"PmFlexType4", 1}]}]),    

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE, Handle),
    stop(?SUNE),
    ok.

cmr(App, Handle) ->
    %%-------------------------------------------------------------
    %% send counter_map
    %%-------------------------------------------------------------
    counter_map(App, Handle, [{"PmFlexGroup1", 1, [{"PmFlexType1", 1}]}]),    
    [{_, A1}] = get_aliases(),
    ct:pal("Aliases:~n~p", [A1]),
    true = lists:member({1, <<"PmFlexGroup1">>}, A1),
    true = lists:member({{1, 1},  <<"PmFlexType1">>}, A1),

    %%-------------------------------------------------------------
    %% reuse the same group alias 
    %%-------------------------------------------------------------
    counter_map(App, Handle, [{"PmFlexGroup2", 1, [{"PmFlexType2", 34}]}]),    
    [{_, A2}] = get_aliases(),
    ct:pal("Aliases:~n~p", [A2]),
    true = lists:member({1, <<"PmFlexGroup2">>}, A2),
    true = lists:member({{1, 34},  <<"PmFlexType2">>}, A2),

    %%-------------------------------------------------------------
    %% reuse the same mt alias 
    %%-------------------------------------------------------------
    counter_map(App, Handle, [{"PmFlexGroup2", 1, [{"PmFlexType3", 34}]}]),    
    [{_, A3}] = get_aliases(),
    ct:pal("Aliases:~n~p", [A3]),
    true = lists:member({1, <<"PmFlexGroup2">>}, A3),
    true = lists:member({{1, 34},  <<"PmFlexType3">>}, A3),

    %%-------------------------------------------------------------
    %% redefine the mt alias 
    %%-------------------------------------------------------------
    counter_map(App, Handle, [{"PmFlexGroup2", 1, [{"PmFlexType3", 99}]}]),    
    [{_, A4}] = get_aliases(),
    ct:pal("Aliases:~n~p", [A4]),
    true = lists:member({1, <<"PmFlexGroup2">>}, A4),
    true = lists:member({{1, 99},  <<"PmFlexType3">>}, A4),
    
    %%-------------------------------------------------------------
    %% redefine the group alias 
    %%-------------------------------------------------------------
    counter_map(App, Handle, [{"PmFlexGroup2", 44, [{"PmFlexType3", 99}]}]),    
    [{_, A5}] = get_aliases(),
    ct:pal("Aliases:~n~p", [A5]),
    true = lists:member({44, <<"PmFlexGroup2">>}, A5),
    true = lists:member({{44, 99},  <<"PmFlexType3">>}, A5),
    
    %%-------------------------------------------------------------
    %% redefine the group alias and mt alias
    %%-------------------------------------------------------------
    counter_map(App, Handle, [{"PmFlexGroup1", 44, [{"PmFlexType1", 99}]}]),
    [{_, A6}] = get_aliases(),
    ct:pal("Aliases:~n~p", [A6]),
    true = lists:member({44, <<"PmFlexGroup1">>}, A6),
    true = lists:member({{44, 99},  <<"PmFlexType1">>}, A6).
    
 
%%========================================================================
%% one_job_one_mr(Config) -> ok.
%% 
%% @doc 
%% create and delete a pm job and a measurement reader
%% @end
%%========================================================================
one_job_one_mr(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "one_job_one_mr",
    GP  = ?GP_10_SEC,
    MTs  = [{?FlexGroup1, [?FlexType1]}],

    ct:pal("###### one_job_one_mr  ~p  ~p  ~n", [App, Job]),
    {ok, Handle} = create_flex_app(App),
    ct:pal("###### one_job_one_mr Handle ~p  ~n", [Handle]),

    CM = [{"PmFlexGroup1", 1, [{"PmFlexType1", 1}]}],
    counter_map(App, Handle, CM),    

    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe({GP, [{1, [1]}]}),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = delete_app(App, Handle).


%%========================================================================
%% one_job_two_mr(Config) -> ok.
%% 
%% @doc 
%% create and delete a pm job with 2 measurement readers
%% @end
%%========================================================================
one_job_two_mr(_Config) ->
    GP  = ?GP_10_SEC,
    MTs  = [{?FlexGroup2, [?FlexType2, ?FlexType3]}],
    CM  = [{?FlexGroup2, 102, [{?FlexType2, 121}, {?FlexType3, 122}]}],

    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "one_job_two_mr",

    {ok, Handle} = create_flex_app(App),
    counter_map(App, Handle, CM),    
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),
    ok = delete_app(App, Handle).


%%========================================================================
%% add_mr_failed(Config) -> ok.
%% 
%% @doc 
%% create a job, try to add a mr to it
%% @end
%%========================================================================
add_mr_failed(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "add_mr_failed",

    GP  = ?GP_10_SEC,
    MTs1  = [{?FlexGroup2, [?FlexType2]}],
    CM  = [{?FlexGroup2, 102, [{?FlexType2, 121}, {?FlexType3, 122}]}],


    {ok, Handle} = create_flex_app(App),
    counter_map(App, Handle, CM),    

    ok = create_job(Job, get_mrs(MTs1)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs1)),

    %%-------------------------------------------------------------
    %% try to create another MeasurementReader
    %%-------------------------------------------------------------

    {ok,    _} = open_trans(),
    {error, _} = 
	create_mr(Job, Job ++ "_new_mr", "PmFlexGroup2", "PmFlexType3"),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% active_stop_active(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% stop and reactivate the job
%% @end
%%========================================================================
active_stop_active(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "active_stop_active",

    {ok, Handle} = create_flex_app(App),

    {ok, RefSubscr} = expected(App, Handle, ?EXP_SUBSCR_GROUP_1),
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result(RefSubscr, App),

    %%-------------------------------------------------------------
    %% stop the job, there should not be any report requests
    %%-------------------------------------------------------------
    RopStop = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, []},
	       no_rop_report_request],
    {ok, RefStop} = expected(App, Handle, RopStop),
    
    update_job(Job, [{"requestedJobState", ?STOPPED}]),
    wait_expected_result(RefStop, App),

    %%-------------------------------------------------------------
    %% activate the job again, the report request should start again
    %%-------------------------------------------------------------
    {ok, RefStart} = expected(App, Handle, [pmi2SubscribeRop, pmi2ReportRop]),
    update_job(Job, [{"requestedJobState", ?ACTIVE}]),
    wait_expected_result(RefStart, App),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).



%%========================================================================
%% three_apps_same_jobs(Config) -> ok.
%% 
%% @doc 
%% start three apps, all measuring the same counter
%% @end
%%========================================================================
three_apps_same_jobs(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App1 = ?SUNE,
    App2 = ?TINA,
    App3 = ?KURT,
    Job = "three_apps_same_jobs",

    {ok, Handle1} = create_flex_app(App1),
    {ok, Handle2} = create_flex_app(App2),
    {ok, Handle3} = create_flex_app(App3),

    {ok, RefSubscr1} = expected(App1, Handle1, ?EXP_SUBSCR_GROUP_1),
    {ok, RefSubscr2} = expected(App2, Handle2, ?EXP_SUBSCR_GROUP_1),
    {ok, RefSubscr3} = expected(App3, Handle3, ?EXP_SUBSCR_GROUP_1),

    ok = create_job(Job, ?JOB_GROUP_1),

    wait_expected_result([{RefSubscr1, App1}, 
			  {RefSubscr2, App2},
			  {RefSubscr3, App3}]),
    
    %%-------------------------------------------------------------
    %% check that all processes are created.
    %% pmsJobGroup, pmsJob, and  3 pmsAppJob
    %%-------------------------------------------------------------
    check_processes(5),

    %%-------------------------------------------------------------
    %% check that all groups are stored in pmsAppRegistry
    %% 3 apps * 2 groups each
    %%-------------------------------------------------------------
    21 = length(get_table(pmsAppRegistry)),

    %%-------------------------------------------------------------
    %% delete the PM Job
    %%-------------------------------------------------------------
    {ok, RefFinal1} = expected(App1, Handle1, ?EXP_FINAL),
    {ok, RefFinal2} = expected(App2, Handle2, ?EXP_FINAL),
    {ok, RefFinal3} = expected(App3, Handle3, ?EXP_FINAL),

    {ok, _} = open_trans(),
    delete_job(Job),
    ok = close_trans(),
    wait_expected_result([{RefFinal1, App1}, 
			  {RefFinal2, App2},
			  {RefFinal3, App3}]),
    
    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ok = delete_app(App1, Handle1),
    ok = delete_app(App2, Handle2),
    ok = delete_app(App3, Handle3),

    %%check_processes(0),
    0 = length(get_table(pmsAppRegistry)),
    check_tables(Tables).
    

%%========================================================================
%% two_apps_diff_jobs(Config) -> ok.
%% 
%% @doc 
%% start two apps measuring different counters
%% @end
%%========================================================================
two_apps_diff_jobs(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App1 = ?SUNE,
    App2 = ?TINA,

    Job1 = "three_apps_same_jobs_1",
    Job2 = "three_apps_same_jobs_2",

    {ok, Handle1} = create_flex_app(App1, ?CM_GRP_1),
    {ok, Handle2} = create_flex_app(App2, ?CM_GRP_2),

    {ok, RefSubscr1} = expected(App1, Handle1, ?EXP_SUBSCR_GROUP_1),
    {ok, RefSubscr2} = expected(App2, Handle2, ?EXP_SUBSCR_GROUP_2),

    ok = create_jobs([{Job1, ?JOB_GROUP_1},
		      {Job2, ?JOB_GROUP_2}]),
    
    wait_expected_result([{RefSubscr1, App1}]),
    wait_expected_result([{RefSubscr2, App2}]),
%%     wait_expected_result([{RefSubscr1, App1}, 
%% 			  {RefSubscr2, App2}]),
    
    check_processes(5),
    2 = length(get_table(pmsAppRegistry)),
    
    %%-------------------------------------------------------------
    %% delete the PM Job
    %%-------------------------------------------------------------
    {ok, RefFinal1} = expected(App1, Handle1, ?EXP_FINAL),
    {ok, RefFinal2} = expected(App2, Handle2, ?EXP_FINAL),

    {ok, _} = open_trans(),
    delete_job(Job1),
    delete_job(Job2),
    ok = close_trans(),
    wait_expected_result([{RefFinal1, App1}, 
			  {RefFinal2, App2}]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ok = delete_app(App1, Handle1),
    ok = delete_app(App2, Handle2),

    check_processes(0),
    0 = length(get_table(pmsAppRegistry)),
    check_tables(Tables).
    

%%========================================================================
%% fragmented(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% the pm data is fragmented
%% @end
%%========================================================================
fragmented(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "fragmented",

    {ok, Handle} = create_flex_app(App),

    ExpRop  = [{values, 
		[{?FF_FALSE, [{?GRP2_ALIAS, [{1, [{?GRP2_TYPE2_ALIAS,[122]}, 
						  {?GRP2_TYPE3_ALIAS,[123]}]}]}]},
		 {?FF_FALSE, [{?GRP2_ALIAS, [{2, [{?GRP2_TYPE2_ALIAS,[222]},
						  {?GRP2_TYPE3_ALIAS,[223]}]}]}]},
		 {?FF_TRUE, [{?GRP2_ALIAS, [{3, [{?GRP2_TYPE2_ALIAS,[322]},
						 {?GRP2_TYPE3_ALIAS,[323]}]}]}]}
		]}],
    
    SubscrOpt = [{pmi2SubscribeRop, ?EXP_SUBSCR_GROUP_2},
		 {pmi2ReportRop,    ExpRop}],
    {ok, RefSubscr} = expected(App, Handle, SubscrOpt),
    ok = create_job(Job, ?JOB_GROUP_2),
    ok = wait_expected_result(RefSubscr, App),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% no_rop_data(Config) -> ok.
%% 
%% @doc 
%% request the app not to send rop data
%% @end
%%========================================================================
no_rop_data(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "no_rop_data",

    {ok, Handle} = create_flex_app(App),

    %%-------------------------------------------------------------
    %% request no reply
    %%-------------------------------------------------------------
    SubscrOpt = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
		 {pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_1},
		 {pmi2ReportRop, {ignore, 1}, []}],
    {ok, RefSubscr} = expected(App, Handle, SubscrOpt),
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result(RefSubscr, App),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% multi_ldn(Config) -> ok.
%% 
%% @doc 
%% multi instances
%% @end
%%========================================================================
multi_ldn(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "multi_ldn",

    {ok, Handle} = create_flex_app(App),

    ExpRop  = [{values, 
		[{?GRP2_ALIAS, [{1, [{?GRP2_TYPE2_ALIAS,[122]}, 
				     {?GRP2_TYPE3_ALIAS,[123]}]}]},
		 {?GRP2_ALIAS, [{2, [{?GRP2_TYPE2_ALIAS,[222]},
				     {?GRP2_TYPE3_ALIAS,[223]}]}]},
		 {?GRP2_ALIAS, [{3, [{?GRP2_TYPE2_ALIAS,[322]},
				     {?GRP2_TYPE3_ALIAS,[323]}]}]}
		]}],
    SubscrOpt = [{pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_2},
		 {pmi2ReportRop, ExpRop}],
    {ok, RefSubscr} = expected(App, Handle, SubscrOpt),
    
    ok = create_job(Job, ?JOB_GROUP_2),
    wait_expected_result(RefSubscr, App),


    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% multi_values(Config) -> ok.
%% 
%% @doc 
%% multi values
%% @end
%%========================================================================
multi_values(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "multi_values",
    
    {ok, Handle} = create_flex_app(App),
    
    ExpRop = [{values, [{?GRP1_ALIAS, 
			 [{1, [{?GRP1_TYPE1_ALIAS, [0, 17, 121, 223]}]}]
			}]}],
    SubscrOpt = [{pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_1},
		 {pmi2ReportRop, ExpRop}],
    {ok, RefSubscr} = expected(App, Handle, SubscrOpt),
    
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result(RefSubscr, App),
    
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% suspect_values(Config) -> ok.
%% 
%% @doc 
%% send supect marked values
%% @end
%%========================================================================
suspect_values(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "suspect_values",
    
    {ok, Handle} = create_flex_app(App),

    ExpRopSing  = [{values, 
		    [{?GRP1_ALIAS, [{1, [{?GRP1_TYPE1_ALIAS,[-1]}]}]},
		     {?GRP1_ALIAS, [{2, [{?GRP1_TYPE1_ALIAS,[-2]}]}]},
		     {?GRP1_ALIAS, [{3, [{?GRP1_TYPE1_ALIAS,[0]}]}]}
		    ]}],
    
    ExpRopMult  = [{values, 
		    [{?GRP1_ALIAS, [{1, [{?GRP1_TYPE1_ALIAS,[0, -1, 233]}]}]},
		     {?GRP1_ALIAS, [{2, [{?GRP1_TYPE1_ALIAS,[22, -2, 0]}]}]},
		     {?GRP1_ALIAS, [{3, [{?GRP1_TYPE1_ALIAS,[-1, -2, 0]}]}]}
		    ]}],
    
    SubscrOpt = [{pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_1},
		 {pmi2ReportRop,    ExpRopSing},
		 {pmi2ReportRop,    ExpRopMult}],
    {ok, RefSubscr} = expected(App, Handle, SubscrOpt),
    
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result(RefSubscr, App),
    
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% ignore_rop(Config) -> ok.
%% 
%% @doc 
%% do not reply to a rop request
%% @end
%%========================================================================
ignore_rop(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "ignore_rop",

    {ok, Handle} = create_flex_app(App),

    SubscrOpt = [{pmi2SubscribeRop, {repeat, 1}, ?OPTS_SUBSCR_GROUP_1},
		 {pmi2ReportRop,    {ignore, 2}, []}],
    {ok, RefSubscr} = expected(App, Handle, SubscrOpt),
    
    
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result(RefSubscr, App),


    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% delay_rop(Config) -> ok.
%% 
%% @doc 
%% delay the rop reply
%% @end
%%========================================================================
delay_rop(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "delay_rop",

    {ok, Handle} = create_flex_app(App),

    SubscrOpt = [{pmi2SubscribeRop, {repeat, 1}, ?OPTS_SUBSCR_GROUP_1},
		 {pmi2ReportRop, [{delay, 1000}]},
		 {pmi2ReportRop, [{delay, 5000}]},
		 pmi2ReportRop],
    {ok, RefSubscr} = expected(App, Handle, SubscrOpt),
    
    
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result(RefSubscr, App),
    

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% wrong_expected_msg(Config) -> ok.
%% 
%% @doc 
%% wrong_expected_msg
%% Note: this is a test of the test environment
%%       should perharps be in another testsuite
%% @end
%%========================================================================
wrong_expected_msg(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "wrong_expected_msg",

    {ok, Handle} = create_flex_app(App),

    {ok, RefSubscr} = expected(App, Handle, ?EXP_SUBSCR_GROUP_1),
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result(RefSubscr, App),

    %%-------------------------------------------------------------
    %% wait for a non received callback
    %%-------------------------------------------------------------
    expected(App, Handle, [{pmi2SubscribeRop, {repeat, 1}, []}]),
    wait_expected_error_result(App, [{pmi2SubscribeRop, {repeat, 1}, []}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).



%%========================================================================
%% two_jobs_separate_trans(Config) -> ok.
%% 
%% @doc 
%% start two apps measuring different counters
%% @end
%%========================================================================
two_jobs_separate_trans(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App1 = ?SUNE,

    Job1 = "two_jobs_separate_trans_1",
    Job2 = "two_jobs_separate_trans_2",

    {ok, Handle1} = create_flex_app(App1, ?CM_GRP_DEF),

    SubscrOpt = [{pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_1},
		 {pmi2SubscribeRop, 
		  [{gp, 10}, 
		   {spec, [{?GRP1_ALIAS, [?GRP1_TYPE1_ALIAS]},
			   {?GRP2_ALIAS, [?GRP2_TYPE2_ALIAS,
					  ?GRP2_TYPE3_ALIAS]}]}]},
		 pmi2ReportRop],
    {ok, RefSubscr1} = expected(App1, Handle1, SubscrOpt),

    ok = create_job(Job1, ?JOB_GROUP_1),
    ok = create_job(Job2, ?JOB_GROUP_2),
    
    wait_expected_result([{RefSubscr1, App1}]),
    
    check_processes(4),
    6 = length(get_table(pmsAppRegistry)),
    
    %%-------------------------------------------------------------
    %% delete the PM Job
    %%-------------------------------------------------------------

    ExpFinal = [{pmi2ReportRop,    {repeat, wait_until_subscribe}, []},
		{pmi2SubscribeRop, {repeat, 1}, [{gp, 10}, {spec, [{?GRP1_ALIAS, [?GRP1_TYPE1_ALIAS]}]}]},
		{pmi2ReportRop,    {repeat, wait_until_subscribe}, []},
		{pmi2SubscribeRop, {repeat, 1}, [{gp, 10}, {spec, []}]}
	       ],
    {ok, RefFinal1} = expected(App1, Handle1, ExpFinal),

    {ok, _} = open_trans(),
    delete_job(Job1),
    delete_job(Job2),
    ok = close_trans(),
    wait_expected_result([{RefFinal1, App1}]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ok = delete_app(App1, Handle1),

    check_processes(0),
    0 = length(get_table(pmsAppRegistry)),
    check_tables(Tables).
    

%%========================================================================
%% three_jobs_separate_trans(Config) -> ok.
%% 
%% @doc 
%% start three apps measuring different counters
%% @end
%%========================================================================
three_jobs_separate_trans(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App1 = ?SUNE,

    Job1 = "three_jobs_separate_trans_1",
    Job2 = "three_jobs_separate_trans_2",
    Job3 = "three_jobs_separate_trans_3",

    {ok, Handle1} = create_flex_app(App1, ?CM_GRP_DEF),

    SubscrOpt = [{pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_1},
		 {pmi2SubscribeRop, 
		  [{gp, 10}, 
		   {spec, [{?GRP1_ALIAS, [?GRP1_TYPE1_ALIAS]},
			   {?GRP2_ALIAS, [?GRP2_TYPE2_ALIAS]}]}]},
		 {pmi2SubscribeRop, 
		  [{gp, 10}, 
		   {spec, [{?GRP1_ALIAS, [?GRP1_TYPE1_ALIAS]},
			   {?GRP2_ALIAS, [?GRP2_TYPE2_ALIAS,
					  ?GRP2_TYPE3_ALIAS]}]}]},
		 pmi2ReportRop],
    {ok, RefSubscr1} = expected(App1, Handle1, SubscrOpt),

    ok = create_job(Job1, ?JOB_GROUP_1),
    {ok, _} = open_trans(),
    ok = create_job_mr(Job2, [{Job2 ++ "_mr", "PmFlexGroup2", "PmFlexType2"}]),
    ok = close_trans(),
    {ok, _} = open_trans(),
    ok = create_job_mr(Job3, [{Job3 ++ "_mr", "PmFlexGroup2", "PmFlexType3"}]),
    ok = close_trans(),
    
    wait_expected_result([{RefSubscr1, App1}]),
    
    check_processes(6),
    6 = length(get_table(pmsAppRegistry)),
    
    %%-------------------------------------------------------------
    %% delete the PM Job
    %%-------------------------------------------------------------

    ExpFinal = [{pmi2ReportRop,    {repeat, wait_until_subscribe}, []},
		{pmi2SubscribeRop, {repeat, 1}, [{gp, 10}, {spec, [{?GRP1_ALIAS, [?GRP1_TYPE1_ALIAS]}]}]},
		{pmi2ReportRop,    {repeat, wait_until_subscribe}, []},
		{pmi2SubscribeRop, {repeat, 1}, [{gp, 10}, {spec, []}]}
	       ],
    {ok, RefFinal1} = expected(App1, Handle1, ExpFinal),

    {ok, _} = open_trans(),
    delete_job(Job1),
    delete_job(Job2),
    delete_job(Job3),
    ok = close_trans(),
    wait_expected_result([{RefFinal1, App1}]),

    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ok = delete_app(App1, Handle1),

    check_processes(0),
    0 = length(get_table(pmsAppRegistry)),
    check_tables(Tables).
    

%%========================================================================
%% same_app_sequence_jobs(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
same_app_sequence_jobs(_Config) ->
    %%-------------------------------------------------------------
    %% create app and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "same_app_sequence_jobs",

    {ok, Handle} = create_flex_app(App),

    %%-------------------------------------------------------------
    %% create job
    %%-------------------------------------------------------------
    ExpJob1 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_2},
	       pmi2ReportRop, 
	       pmi2ReportRop],
    {ok, RefSubscr} = expected(App, Handle, ExpJob1),
    ok = create_job(Job, ?JOB_GROUP_2),
    wait_expected_result(RefSubscr, App),


    %%-------------------------------------------------------------
    %% delete job
    %%-------------------------------------------------------------
    {ok, RefDel1} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefDel1, App),


    %%-------------------------------------------------------------
    %% create job again
    %%-------------------------------------------------------------
    {ok, RefCreate2} = expected(App, Handle, ExpJob1),
    ok = create_job(Job, ?JOB_GROUP_2),
    wait_expected_result(RefCreate2, App),


    %%-------------------------------------------------------------
    %% delete job
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),


    %%-------------------------------------------------------------
    %% delete app
    %%-------------------------------------------------------------

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%========================================================================
%% misc functions
%%========================================================================
%% gp(?TEN_SECONDS)    -> 10;
%% gp(?THIRTY_SECONDS) -> 30.

to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.

    
get_ct_config(TC) -> ?LIB:get_ct_config(TC).
    
start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
stop(A)   -> ok      = ?ERL_APP:stop(A).   
cleanup() -> ?LIB:cleanup().

%% initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
initialize(A, B, C)  -> {ok, _Handle} = ?LIB:initialize(A, B, C).
finalize(A, B)       -> ok = ?LIB:finalize(A, B).
counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
get_aliases()        -> ?LIB:get_aliases().

%% create_app(A, B)      -> ?LIB:create_app({A, [{expected, [relay]}]}, B).
create_flex_app(A)    -> ?LIB:create_flex_app(A).
create_flex_app(A, B) -> ?LIB:create_flex_app(A, B).
delete_app(A, B) -> ?LIB:delete_app(A, B).
create_job(A, B) -> ?LIB:create_job(A, B).
create_jobs(A)   -> ?LIB:create_jobs(A).
delete_jobs(A)   -> ?LIB:delete_jobs(A).
update_job(A, B) -> ?LIB:update_job(A, B).
delete_job(A)    -> ?LIB:delete_job(A).

create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).
create_job_mr(A, B)   -> ?LIB:create_job_mr(A, B, ?DEF_JOB_ATTRS).

get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
check_tables(A)    -> ?LIB:check_tables(A).
check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

get_mrs(A)                   -> ?LIB:get_mrs(A).
get_subscribe_data(A, B)     -> ?LIB:get_subscribe_data(A, B).

wait_subscribe(A)            -> ?LIB:wait_subscribe(A).
wait_until_subscribe(A)      -> ?LIB:wait_until_subscribe(A).
%% wait_report_rop(A, B, C)     -> ?LIB:wait_report_rop(A, B, C).

expected(A, B, C)               -> ?LIB:expected(A, B, C).

wait_expected_result(A)         -> ?LIB:wait_expected_result(A).
wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B).
wait_expected_error_result(A,B) -> ?LIB:wait_expected_error_result(A, B).
wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).
    
host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

log_rop_files(Config) -> 
    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
    log_rop_files(RopData, Config).

log_rop_files(RopData, Config) -> pms_test_lib:log_rop_files(RopData, Config).

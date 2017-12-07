%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_basic_SUITE.erl %
%%% @version /main/R3A/R4A/R11A/1

%%% @doc 
%%% == Test suite for basic testing of ROP files ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_rop_basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("pms2_test.hrl").

%%% ----------------------------------------------------------
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2014-10-28 eolaand     First vsn of PMI2 ROP test
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% R4A/6      2015-08-25 etxjovp     Add group definitions used by CS CI
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


-compile([nowarn_export_all, export_all]).

-define(TESTNODE, testnode).

-define(LIB,     pms2_test_lib).
-define(ERL_APP, pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).

-define(NACKA, nacka).
-define(KENTA, kenta).
-define(RONNIE, ronnie).
-define(KENNEDY, kennedy).

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
    [{ct_hooks, Hooks ++ [{pms_rop_hook, []}, {pms2_erl_app, []}]}].


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
    [
     tc_1job_1app,
     tc_1job_2app,
     tc_1job_2app_2groups,
%%      tc_1job_1app_2inst_avg,
%%      tc_1job_1app_2inst_min,
%%      tc_1job_1app_2inst_max,
%%      tc_1job_1app_2inst_avg,
%%      tc_1job_1app_2inst_lu,
     tc_2rop_files,
     tc_neg
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
%% @doc
%% Start one application and a PM job with one counter and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
%%========================================================================
tc_1app_1job(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?NACKA,
    Job = "tc_1app_1job",

    {ok, Handle} = create_app(App, ?CM_GRP_DEF, ?LDN_DEF_1(App)),

    {ok, RefSubscr1} = expected(App, Handle, ?EXP_SUBSCR_GROUP_1),
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result([{RefSubscr1, App}]),

    {ok, RefSubscr2} = expected(App, Handle, [pmi2ReportRop]),
    {ok, RopRef} = pms_rop_hook:expected([{1, [], ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr2, App}], [{RopRef, ?ROP_HOOK}]),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal, App}]),

    ok = delete_app(App, Handle),
    check_tables(Tables).

%%========================================================================
%% @doc
%% Start one application and a PM job with one counter and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
%%========================================================================
tc_1job_1app(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?NACKA,
    Job = "tc_1_job_1app",

    ok = create_job(Job, ?JOB_GROUP_1),


    StartExpected = [{expected, [{pmi2SubscribeRop, [], ?OPTS_SUBSCR_GROUP_1}]},
		     {expected_ref, Job}],
    {ok, Handle} = create_app({App, StartExpected},
			       ?CM_GRP_DEF, 
			       ?LDN_DEF_1(App)),

    wait_expected_result([{Job, App}]),

    {ok, RefSubscr1} = expected(App, Handle, [pmi2ReportRop]),
    wait_expected_result([{RefSubscr1, App}]),

    {ok, RefSubscr2} = expected(App, Handle, [pmi2ReportRop]),
    {ok, RopRef} = pms_rop_hook:expected([{1, [], ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr2, App}], [{RopRef, ?ROP_HOOK}]),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal, App}]),

    ok = delete_app(App, Handle),
    check_tables(Tables).

%%========================================================================
%% @doc
%% Start two applications and a PM job with one counter and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
%%========================================================================
tc_1job_2app(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App1 = ?NACKA,
    App2 = ?RONNIE,
    Job = "tc_1job_2app",

    {ok, Handle1} = create_app(App1, ?CM_GRP_DEF, ?LDN_DEF_1(App1)),
    {ok, Handle2} = create_app(App2, ?CM_GRP_DEF, ?LDN_DEF_2(App2)),

    {ok, RefSubscr1} = expected(App1, Handle1, ?EXP_SUBSCR_GROUP_1),
    {ok, RefSubscr2} = expected(App2, Handle2, ?EXP_SUBSCR_GROUP_1),
    ok = create_job(Job, ?JOB_GROUP_1),
    wait_expected_result([{RefSubscr1, App1}, {RefSubscr2, App2}]),

    {ok, RefRepRop1} = expected(App1, Handle1, [pmi2ReportRop]),
    {ok, RefRepRop2} = expected(App2, Handle2, [pmi2ReportRop]),
    {ok, RopRef} = pms_rop_hook:expected([{1, [], ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefRepRop1, App1}, {RefRepRop2, App2}],
			 [{RopRef, ?ROP_HOOK}]),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal1} = expected(App1, Handle1, ?EXP_FINAL),
    {ok, RefFinal2} = expected(App2, Handle2, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal1, App1}, {RefFinal2, App2}]),

    ok = delete_app(App1, Handle1),
    ok = delete_app(App2, Handle2),
    check_tables(Tables).

%%========================================================================
%% @doc
%% Start two applications with different Groups and a PM job with two counters,
%% one from each group, and verify that a ROP file is generated with no 
%% suspect marking.
%% @end
%%========================================================================
tc_1job_2app_2groups(_Config) -> 
    Tables = tables(),

    App1 = ?NACKA,
    App2 = ?RONNIE,
    Job = "tc_1job_2app_2groups",

    {ok, Handle1} = create_app(App1, ?ROP_GRP_1, ?LDN_DEF_1(App1)),
    {ok, Handle2} = create_app(App2, ?ROP_GRP_2, ?LDN_DEF_2(App2)),

    {ok, RefSubscr1} = expected(App1, Handle1, ?EXP_SUBSCR_ROPGROUP_1),
    {ok, RefSubscr2} = expected(App2, Handle2, ?EXP_SUBSCR_ROPGROUP_2),
    %% ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1, ?ROPType1Sum},
    %% 			  {Job ++ "_mr2", ?ROPGroup2, ?ROPType6Sum}]),
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1},
			  {Job ++ "_mr2", ?ROPGroup2}]),
			  
    %% wait for subscribe, NOT ROP file			  
    wait_expected_result([{RefSubscr1, App1}, {RefSubscr2, App2}]),

    {ok, RefRepRop1} = expected(App1, Handle1, [pmi2ReportRop]),
    {ok, RefRepRop2} = expected(App2, Handle2, [pmi2ReportRop]),
    {ok, RopRef} = pms_rop_hook:expected([{1, [], ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------

    wait_expected_result([{RefRepRop1, App1}, {RefRepRop2, App2}], 
			 [{RopRef, ?ROP_HOOK}]),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal1} = expected(App1, Handle1, ?EXP_FINAL),
    {ok, RefFinal2} = expected(App2, Handle2, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal1, App1}, {RefFinal2, App2}]),

    ok = delete_app(App1, Handle1),
    ok = delete_app(App2, Handle2),
    check_tables(Tables).

%% ================================= ERASHEG, UABNBAC   #1		     
	    
%% @doc
%% Start two instances of an application and a PM job with LDN_DEF_1 one counter with
%% aggregation AVG (average) and verify that a ROP file is generated and that the counter
%% value is aggregated.
%% @end
tc_1job_1app_2inst_avg(_Config) -> 
	helper_1job_1app_2inst(_Config, 
				?ROPGRP1_AVGTYPE_ALIAS, 
				?ROPType2Avg, 
				helper_search_string(1062)).

%% ================================= ERASHEG, UABNBAC   #2

% @doc
%% Start two instances of an application and a PM job with one counter with
%% aggregation MIN and verify that a ROP file is generated and that the counter
%% value is aggregated.
%% @end
	 
tc_1job_1app_2inst_min(_Config) -> 
	
	helper_1job_1app_2inst(_Config, 
				?ROPGRP1_MINTYPE_ALIAS, 
				?ROPType3Min, 
				helper_search_string(1063) ).
				
%% ================================= ERASHEG, UABNBAC   #3
	 	 
%% @doc
%% Start two instances of an application and a PM job with one counter with
%% aggregation MAX and verify that a ROP file is generated and that the counter
%% value is aggregated.
%% @end
tc_1job_1app_2inst_max(_Config) -> 
	helper_1job_1app_2inst(_Config, 
				?ROPGRP1_MAXTYPE_ALIAS, 
				?ROPType4Max, 
				 helper_search_string(1064)).

%% ================================= ERASHEG, UABNBAC   #4

%% @doc
%% Start two instances of an application and a PM job with one counter with
%% aggregation LU and verify that a ROP file is generated and that the counter
%% value is aggregated.
%% @end
tc_1job_1app_2inst_lu(_Config) -> 
helper_1job_1app_2inst(_Config, 
				?ROPGRP1_LUTYPE_ALIAS, 
				?ROPType5LU, 
				helper_search_string(1065)).

%%========================================================================
%% @doc
%% Create 2 jobs with different jobGroups, thus there should be
%% two rop files generated.
%% @end
%%========================================================================
tc_2rop_files(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?NACKA,
    Job1 = "tc_2rop_files_1",
    Job2 = "tc_2rop_files_2",

    ok = create_job(Job1, ?JOB_GROUP_1),
    ok = create_job(Job2, ?JOB_GROUP_1, [{"reportingPeriod",   1},
					 {"granularityPeriod", 1},
					 {"jobGroup",          "second"}]),

    StartExpected1 = [{expected, [{pmi2SubscribeRop, [], ?OPTS_SUBSCR_GROUP_1}]},
		      {expected_ref, Job1}],
    {ok, Handle} = create_app({App, StartExpected1},
			      ?CM_GRP_DEF, 
			      ?LDN_DEF_1(App)),

    wait_expected_result([{Job1, App}]),

    {ok, RefSubscr1} = expected(App, Handle, [pmi2ReportRop]),
    wait_expected_result([{RefSubscr1, App}]),


    %% NOTE:  There should be a check that the two rop files are genrerated

    {ok, RefSubscr2} = expected(App, Handle, [pmi2ReportRop]),
    {ok, RopRef} = pms_rop_hook:expected([{1, [], ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr2, App}], [{RopRef, ?ROP_HOOK}]),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs([Job1, Job2]),
    wait_expected_result([{RefFinal, App}]),

    ok = delete_app(App, Handle),
    check_tables(Tables).






tc_1job_2app_all_counters(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App1 = ?NACKA,
    App2 = ?RONNIE,
    Job = "tc_1job_2app_all_counters",
    GP  = ?GP_10_SEC,
    
    MTs = [?ROPGroup1],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app({App1, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App1)),
    App1Data      = {App1, Handle1, GP},
    
   
    {ok, Handle2} = create_app({App2, [{expected, [relay]}]}),
    App2Data      = {App2, Handle2, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe([get_subscribe_data(GP, MTs)]),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values1 = [{?ROPGroup1, [{LDN, [{?ROPType1Sum, [1200]},
				   {?ROPType2Avg, [2000]},
				   {?ROPType3Min, [1001]},
				   {?ROPType4Max, [9000]},
				   {?ROPType5LU,  [5665]}]}]}],
				   
    Values2 = [{?ROPGroup1, [{LDN, [{?ROPType1Sum, [2000]},
				   {?ROPType2Avg, [3000]},
				   {?ROPType3Min, [1002]},
				   {?ROPType4Max, [5555]},
				   {?ROPType5LU,  [666]}]}]}],
				   
				   
				   
    AliasValues1 = aliasify_values(Values1),
    AliasValues2 = aliasify_values(Values2),
    
    %%-------------------------------------------------------------
    %% Wait for first ROP file and throw it away
    %%-------------------------------------------------------------
    ok = wait_report_rop(AliasValues1, ?FF_TRUE, App1Data),    
    ok = wait_report_rop(AliasValues2, ?FF_TRUE, App2Data),    
    
    %%-------------------------------------------------------------
    %% Wait for second ROP file 
    %%-------------------------------------------------------------
    ok = wait_report_rop(AliasValues1, ?FF_TRUE, App1Data),    
    ok = wait_report_rop(AliasValues2, ?FF_TRUE, App2Data),   
    %%-------------------------------------------------------------
    %% Check ROP
    %%-------------------------------------------------------------
    RopExpected  = ["<r p=\"1\">3200</r>", 
		    "<r p=\"2\">2500</r>", 
		    "<r p=\"3\">1001</r>", 
		    "<r p=\"4\">9000</r>", 
		    "<r p=\"5\">666</r>"],
    {ok, RopRef} = pms_rop_hook:expected([{1, RopExpected, ["suspect"]}]),
    wait_expected_result([], [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe([get_subscribe_data(GP, []), get_subscribe_data(GP, [])]),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App1, Handle1),
    ok = delete_app(App2, Handle2).


%%========================================================================
%% @doc
%% Start one application and a PM job with one counter. 
%% Counter value is negative.
%% @end
%%========================================================================
tc_neg(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job = "tc_neg",
    GP  = ?GP_10_SEC,
    
    MTs = [?ROPGroup1],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe([get_subscribe_data(GP, MTs)]),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values = [{?ROPGroup1, [{LDN, [{?ROPType1Sum, [-100]},
				   {?ROPType2Avg, [-23]},
				   {?ROPType3Min, [2]},
				   {?ROPType4Max, [0]},
				   {?ROPType5LU,  [-2]}]}]}],
				   
    AliasValues = aliasify_values(Values),
    
    %%-------------------------------------------------------------
    %% Wait for first ROP file and throw it away
    %%-------------------------------------------------------------
    ok = wait_report_rop(AliasValues, ?FF_TRUE, AppData),    
    
    %%-------------------------------------------------------------
    %% Wait for second ROP file 
    %%-------------------------------------------------------------
    ok = wait_report_rop(AliasValues, ?FF_TRUE, AppData),    
    %%-------------------------------------------------------------
    %% Check ROP
    %%-------------------------------------------------------------
    RopExpected  = ["<r p=\"1\">-100</r>", 
		    "<r p=\"2\">-23</r>", 
		    "<r p=\"3\">2</r>", 
		    "<r p=\"4\">0</r>", 
		    "<r p=\"5\"> </r>"],
    {ok, RopRef} = pms_rop_hook:expected([{1, RopExpected, ["suspect"]}]),
    wait_expected_result([], [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe([get_subscribe_data(GP, [])]),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


-define(HELPER_EXP_SUBSCR_ROPGROUP_1(Type),
	[{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, [{gp, 10}, {spec, [{?ROPGRP1_ALIAS, [Type]}]}]},
	 pmi2ReportRop]).


helper_search_string(Value) ->
	Prefix = "<r p=\"1\">",
	Suffix =  "</r>",
	[{1, [Prefix ++ integer_to_list(Value) ++ Suffix], ["suspect"]}].

% @doc
%% Start two instances of an application and a PM job with one counter with
%% aggregation MIN and verify that a ROP file is generated and that the counter
%% value is aggregated.
%% @end
helper_1job_1app_2inst(_Config, RopGrpType, RopType, RopExpect) -> 
	Tables = tables(),

    App1 = ?NACKA,
    App2 = ?RONNIE,
    Job = "tc_1job_1app_2inst_min",
    {ok, Handle1} = create_app(App1, undefined, ?LDN_DEF_1(App1)),
    {ok, Handle2} = create_app(App2, undefined, ?LDN_DEF_1(App1)),
    
    
    {ok, RefSubscr1} = expected(App1, Handle1, ?HELPER_EXP_SUBSCR_ROPGROUP_1(RopGrpType)),
    {ok, RefSubscr2} = expected(App2, Handle2, ?HELPER_EXP_SUBSCR_ROPGROUP_1(RopGrpType)),
    
     
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1, RopType}]),
    
    %% Wait for subscribe
    
    wait_expected_result([{RefSubscr1, App1}, {RefSubscr2, App2}]),
    
    %% Subscription is done
    
    
    
    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    
    {ok, RefRepRop1} = expected(App1, Handle1, [pmi2ReportRop]),
    {ok, RefRepRop2} = expected(App2, Handle2, [pmi2ReportRop]),    
    {ok, RopRef} = pms_rop_hook:expected(RopExpect),
     
    wait_expected_result([{RefRepRop1, App1}, {RefRepRop2, App2}], 
			 [{RopRef, ?ROP_HOOK}]),

    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal1} = expected(App1, Handle1, ?EXP_FINAL),
    {ok, RefFinal2} = expected(App2, Handle2, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal1, App1}, {RefFinal2, App2}]),
    
    ok = delete_app(App1, Handle1),
    ok = delete_app(App2, Handle2),
    check_tables(Tables).





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
    
%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).   
cleanup() -> ?LIB:cleanup().

%% initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%% finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%% counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%% get_aliases()        -> ?LIB:get_aliases().

create_app(A)       -> ?LIB:create_app(A).
create_app(A, B)    -> ?LIB:create_app(A, B).
create_app(A, B, C) -> ?LIB:create_app(A, B, C).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
create_job(A, B)    -> ?LIB:create_job(A, B).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
%%create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
%%update_job(A, B)    -> ?LIB:update_job(A, B).
delete_job(A)       -> ?LIB:delete_job(A).

create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)       -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

expected(A, B, C)               -> ?LIB:expected(A, B, C).
wait_expected_result(A)         -> ?LIB:wait_expected_result(A, []).
wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B, 20000).

%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).
    
host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

check_rop_file(A, B)     -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)  -> ?LIB:check_rop_file(A, B, C).
wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
wait_until_subscribe(A)  -> ?LIB:wait_until_subscribe(A).
wait_report_rop(A, B, C) -> ?LIB:wait_report_rop(A, B, C).
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).    
    


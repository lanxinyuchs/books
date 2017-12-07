%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_mixed_frag_SUITE.erl %
%%% @version /main/R3A/R4A/R5A/R6A/R8A/R11A/1

%%% @doc 
%%% == Test suite for basic testing of ROP files ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_rop_mixed_frag_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("pms2_test.hrl").

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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

-define(LIB,      pms2_test_lib).
-define(ERL_APP,  pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).
-define(PROXY, pms_pmi_proxy).

-define(NACKA, nacka).
-define(KENTA, kenta).
-define(RONNIE, ronnie).
-define(KENNEDY, kennedy).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).


-define(SLEEP,  500).

-define(JOB_GROUP, "PMI2").

-define(VALS_FRAG_PMI2(LDN),
	begin
	    [{_, LDN1}, {_, LDN2}, {_, LDN3}] = LDN,
	    [{?FF_FALSE, 
					 [{3,
					   [{LDN1,
					     [{31,[261]},
					      {32,[262]},
					      {33,[263]},
					      {34,[264]},
					      {35,[265]}]},
					    {LDN2,
					     [{31,[261]},
					      {32,[262]},
					      {33,[263]},
					      {34,[264]},
					      {35,[265]}]}
					   ]}]},
					{?FF_TRUE,
					 [{3,
					   [{LDN3,
					     [{31,[261]},
					      {32,[262]},
					      {33,[263]},
					      {34,[264]},
					      {35,[265]}]}
					   ]}]}]
	end).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    ProxyPars = {?PROXY, get_ct_config(?PROXY, [{erl_api, false}])},
    Hooks = lists:keystore(?PROXY, 1, hooks(), ProxyPars),
    [
     {ct_hooks, Hooks},
     {timetrap, {seconds, 600}}
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
    [
     tc_2app_1job_pmi_first,
     tc_2app_1job_pmi2_first,
     tc_2app_2job,
     tc_2app_2jobgrp_2rop,
     tc_2app_1job_pmi_final_frag     
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
%% Start two jobs, using PMI and PMI2, respectively.
%% Start one job handling both apps.
%% PMI app sends rop data before PMI2
%% @end
%%========================================================================
tc_2app_1job_pmi_first(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    AppPmi1 = ?NACKA,
    AppPmi2 = ?RONNIE,
    Job = "tc_2app_1job_pmi_first",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi(AppPmi1, [?ROPGroup1]),
    {ok, Handle2} = create_app(AppPmi2, ?ROP_GRP_1),

    ExpPmi1 = [{pmiReport, {repeat, wait_until_subscribe}, []},
	       {pmiSubscribe, []},
	       {pmiReport, {repeat,2}, [{delay, 100}]}],

    Ldn = [?LDN_DEF_1(AppPmi2), ?LDN_DEF_3(AppPmi2), ?LDN_DEF_2(AppPmi2)],
    {ok, Aliases} = ldn_aliases(AppPmi2, Ldn),
    ValsPmi2 = ?VALS_FRAG_PMI2(Aliases),
    ExpPmi2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	       {pmi2ReportRop, {repeat,2}, [{delay, 1000}, 
					    {values, ValsPmi2}]}],
    
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ExpPmi2),
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1},
			  {Job ++ "_mr2", ?ROPGroup2}]),
    wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}]),
    
    {ok, RefSubscr12} = expected(AppPmi1, Handle1, [pmiReport]),
    {ok, RefSubscr22} = expected(AppPmi2, Handle2, [pmi2ReportRop]),
    RopExpected  = 
	["<r p=\"1\">261</r>", "<r p=\"1\">261</r>", "<r p=\"1\">1234</r>"],
    {ok, RopRef} = ?ROP_HOOK:expected([{1, RopExpected, ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr12, AppPmi1}, {RefSubscr22, AppPmi2}],
			 [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ExpPmi1Final = [{pmiReport,    {repeat, wait_until_subscribe}, []},
		    {pmiSubscribe, {repeat, 1}, [{gp, 10}, {spec, []}]}],

    {ok, RefFinal1} = expected(AppPmi1, Handle1, ExpPmi1Final),
    {ok, RefFinal2} = expected(AppPmi2, Handle2, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal1, AppPmi1}, {RefFinal2, AppPmi2}]),

    ok = delete_app_pmi(AppPmi1, Handle1),
    ok = delete_app_pmi(AppPmi2, Handle2),
    check_tables(Tables).



%%========================================================================
%% @doc
%% Start two jobs, using PMI and PMI2, respectively.
%% Start one job handling both apps.
%% PMI2 app sends rop data before PMI
%% @end
%%========================================================================
tc_2app_1job_pmi2_first(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    AppPmi1 = ?NACKA,
    AppPmi2 = ?RONNIE,
    Job = "tc_2app_1job_pmi2_first",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi(AppPmi1, [?ROPGroup1]),
    {ok, Handle2} = create_app(AppPmi2, ?ROP_GRP_1),

    ExpPmi1 = [{pmiReport, {repeat, wait_until_subscribe}, []},
	       {pmiSubscribe, []},
	       {pmiReport, {repeat,2}, [{delay, 1000}]}],

    Ldn = [?LDN_DEF_1(AppPmi2), ?LDN_DEF_3(AppPmi2), ?LDN_DEF_2(AppPmi2)],
    {ok, Aliases} = ldn_aliases(AppPmi2, Ldn),
    ValsPmi2 = ?VALS_FRAG_PMI2(Aliases),
    ExpPmi2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	       {pmi2ReportRop, {repeat,2}, [{delay, 100}, {values, ValsPmi2}]}],
    
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ExpPmi2),
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1},
			  {Job ++ "_mr2", ?ROPGroup2}]),
    wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}]),
    
    {ok, RefSubscr12} = expected(AppPmi1, Handle1, [pmiReport]),
    {ok, RefSubscr22} = expected(AppPmi2, Handle2, [pmi2ReportRop]),
    RopExpected  = ["<r p=\"1\">261</r>", "<r p=\"1\">261</r>", 
		    "<r p=\"1\">1234</r>"],
    {ok, RopRef} = ?ROP_HOOK:expected([{1, RopExpected, ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr12, AppPmi1}, {RefSubscr22, AppPmi2}],
			 [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ExpPmi1Final = [{pmiReport,    {repeat, wait_until_subscribe}, []},
		    {pmiSubscribe, {repeat, 1}, [{gp, 10}, {spec, []}]}],

    {ok, RefFinal1} = expected(AppPmi1, Handle1, ExpPmi1Final),
    {ok, RefFinal2} = expected(AppPmi2, Handle2, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal1, AppPmi1}, {RefFinal2, AppPmi2}]),

    ok = delete_app_pmi(AppPmi1, Handle1),
    ok = delete_app_pmi(AppPmi2, Handle2),
    check_tables(Tables).




%%========================================================================
%% @doc
%% Start two jobs, using PMI and PMI2, respectively.
%% Start two jobs, one for each app.
%% @end
%%========================================================================
tc_2app_2job(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    AppPmi1 = ?NACKA,
    AppPmi2 = ?RONNIE,
    Job1 = "tc_2app_2job_1",
    Job2 = "tc_2app_2job_2",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi(AppPmi1, [?ROPGroup1]),
    {ok, Handle2} = create_app(AppPmi2, ?ROP_GRP_1),

    ExpPmi1 = [{pmiReport, {repeat, wait_until_subscribe}, []},
	       {pmiSubscribe, []},
	       {pmiReport, {repeat,2}, []}],
    
    Ldn = [?LDN_DEF_1(AppPmi2), ?LDN_DEF_3(AppPmi2), ?LDN_DEF_2(AppPmi2)],
    {ok, Aliases} = ldn_aliases(AppPmi2, Ldn),
    ValsPmi2 = ?VALS_FRAG_PMI2(Aliases),
    ExpPmi2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	       {pmi2ReportRop, {repeat,2}, [{values, ValsPmi2}]}],
    
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ExpPmi2),
    ok = create_job(Job1, [{Job1 ++ "_mr1", ?ROPGroup1}]),
    ok = create_job(Job2, [{Job2 ++ "_mr2", ?ROPGroup2}]),
     wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}]),

    {ok, RefSubscr12} = expected(AppPmi1, Handle1, [pmiReport]),
    {ok, RefSubscr22} = expected(AppPmi2, Handle2, [pmi2ReportRop]),
    RopExpected  = ["<r p=\"1\">261</r>", "<r p=\"1\">261</r>", 
		    "<r p=\"1\">1234</r>"],
    {ok, RopRef} = ?ROP_HOOK:expected([{1, RopExpected, ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr12, AppPmi1}, {RefSubscr22, AppPmi2}],
			 [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ExpPmi1Final = [{pmiReport,    {repeat, wait_until_subscribe}, []},
		    {pmiSubscribe, {repeat, 1}, [{gp, 10}, {spec, []}]}],

    {ok, RefFinal1} = expected(AppPmi1, Handle1, ExpPmi1Final),
    {ok, RefFinal2} = expected(AppPmi2, Handle2, ?EXP_FINAL),
    ok = delete_jobs([Job1, Job2]),
    wait_expected_result([{RefFinal1, AppPmi1}, {RefFinal2, AppPmi2}]),

    ok = delete_app_pmi(AppPmi1, Handle1),
    ok = delete_app_pmi(AppPmi2, Handle2),
    check_tables(Tables).




%%========================================================================
%% @doc
%% Start two jobs, using PMI and PMI2, respectively.
%% Start two jobs, with different jobGroups, one for each app, resulting
%% in two rop files.
%% @end
%%========================================================================
tc_2app_2jobgrp_2rop(_Config) -> 
    %%-------------------------------------------------------------
    %% set PmSupport to multi rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?MULTI_ROP),

    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    AppPmi1 = ?NACKA,
    AppPmi2 = ?RONNIE,
    Job1 = "tc_2app_2jobgrp_2rop_1",
    Job2 = "tc_2app_2jobgrp_2rop_2",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi(AppPmi1, [?ROPGroup1]),
    {ok, Handle2} = create_app(AppPmi2, ?ROP_GRP_1),

    ExpPmi1 = [{pmiReport, {repeat, wait_until_subscribe}, []},
	       {pmiSubscribe, []},
	       {pmiReport, {repeat,2}, []}],
    
    Ldn = [?LDN_DEF_1(AppPmi2), ?LDN_DEF_3(AppPmi2), ?LDN_DEF_2(AppPmi2)],
    {ok, Aliases} = ldn_aliases(AppPmi2, Ldn),
    ValsPmi2 = ?VALS_FRAG_PMI2(Aliases),
    ExpPmi2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	       {pmi2ReportRop, {repeat,2}, [{values, ValsPmi2}]}],
    
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ExpPmi2),
    ok = create_jobs([{Job1, [{Job1 ++ "_mr1", ?ROPGroup1}]},
		      {Job2,
		       [{Job2 ++ "_mr2", ?ROPGroup1}], 
		       [{"reportingPeriod",   1},
			{"granularityPeriod", 1},
			{"jobGroup",          ?JOB_GROUP}]}]),
    wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}]),

    {ok, RefSubscr12} = expected(AppPmi1, Handle1, [pmiReport]),
    {ok, RefSubscr22} = expected(AppPmi2, Handle2, [pmi2ReportRop]),

    %%-------------------------------------------------------------
    %% Check which Rop belongs to PMI1 and which to PMI2
    %%-------------------------------------------------------------
    MEData   = rpc(comsaI, get_managed_element_data, []),
    UniqueId = proplists:get_value(networkManagedElementId, MEData),

    N = [Pmi1, Pmi2] = case UniqueId of
			   undefined -> 
			       ["", "_" ++ ?JOB_GROUP];
			   _         -> 
			       [UniqueId, "_" ++ UniqueId ++ "_" ++ ?JOB_GROUP]
		       end,
    ct:pal("N ~p~n", [N]),
    {RopExp1, RopExp2} = case lists:sort(N) of
			     [Pmi1, Pmi2] ->
				 {["tc_2app_2jobgrp_2rop_1"],
				  ["tc_2app_2jobgrp_2rop_2"]};
			     [Pmi2, Pmi1] ->
				 {["tc_2app_2jobgrp_2rop_2"],
				  ["tc_2app_2jobgrp_2rop_1"]}
			 end,
    
    ct:pal("expect ~p~n", [{RopExp1, RopExp2}]),

    {ok, RopRef} = ?ROP_HOOK:expected([{2, RopExp2, ["suspect"]}, 
				       {1, RopExp1, ["suspect"]}]),
    
    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([{RefSubscr12, AppPmi1}, {RefSubscr22, AppPmi2}],
 			 [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ExpPmi1Final = [{pmiReport,    {repeat, wait_until_subscribe}, []},
		    {pmiSubscribe, {repeat, 1}, [{gp, 10}, {spec, []}]}],

    {ok, RefFinal1} = expected(AppPmi1, Handle1, ExpPmi1Final),
    {ok, RefFinal2} = expected(AppPmi2, Handle2, ?EXP_FINAL),
    ok = delete_jobs([Job1, Job2]),
    wait_expected_result([{RefFinal1, AppPmi1}, {RefFinal2, AppPmi2}]),

    ok = delete_app_pmi(AppPmi1, Handle1),
    ok = delete_app_pmi(AppPmi2, Handle2),
    check_tables(Tables),
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).




%%========================================================================
%% @doc
%% Start two apps, using PMI and PMI2, respectively.
%% Start one job handling both apps.
%% @end
%%========================================================================
tc_2app_1job_pmi_final_frag(_Config) -> 
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    AppPmi1 = ?NACKA,
    AppPmi2 = ?RONNIE,
    Job = "tc_2app_1job_pmi_final_frag",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %% Both apps announce ROPGroup1
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi(AppPmi1, [?ROPGroup1]),
    {ok, Handle2} = create_app(AppPmi2, ?ROP_GRP_1),

    ValsPmi = [[{"ROPGroup1",
		 [{"ROPType1Sum", [1234]},
		  {"ROPType2Avg", [1235]},
		  {"ROPType3Min", [1236]},
		  {"ROPType4Max", [1237]},
		  {"ROPType5LU",  [1238]}]}],
		[]],
  
    ExpPmi1 = [{pmiReport, {repeat, wait_until_subscribe}, []},
	       {pmiSubscribe, []},
	       {pmiReport, {repeat,2}, [{values, ValsPmi}]}],

    Ldn = [?LDN_DEF_1(AppPmi2), ?LDN_DEF_3(AppPmi2), ?LDN_DEF_2(AppPmi2)],
    {ok, Aliases} = ldn_aliases(AppPmi2, Ldn),
    ValsPmi2 = ?VALS_FRAG_PMI2(Aliases), %% generates values 261-265
    ExpPmi2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	       {pmi2ReportRop, {repeat,2}, [{values, ValsPmi2}]}],
    
    %%-------------------------------------------------------------
    %%Create Job
    %%-------------------------------------------------------------
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ExpPmi2),
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1},
			  {Job ++ "_mr2", ?ROPGroup2}]),

    %%-------------------------------------------------------------
    %% Wait for 1st ROP
    %%-------------------------------------------------------------
    RopExpected  = ["<r p=\"1\">261</r>", 
		    "nacka",
		    "ronnie",
		    "one",
		    "two",
		    "three",
		    "<r p=\"1\">1234</r>"],
    {ok, RopRef} = ?ROP_HOOK:expected([{1, RopExpected, ["suspect"]}]),

    wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}],
			 [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% Wait for 2nd ROP
    %%-------------------------------------------------------------
    {ok, RefSubscr12} = expected(AppPmi1, Handle1, [pmiReport]),
     %% generates default values 61-65 (because no values option defined)
    {ok, RefSubscr22} = expected(AppPmi2, Handle2, [pmi2ReportRop]),
    [_ | RExp] =  RopExpected,
    RopExpected2 =  ["<r p=\"1\">61</r>" | RExp], 
    ct:pal("RopExpected2 = ~s", [RopExpected2]),
    {ok, RopRef2} = ?ROP_HOOK:expected([{2, RopExpected2, ["suspect"]}]),

    wait_expected_result([{RefSubscr12, AppPmi1}, {RefSubscr22, AppPmi2}],
			 [{RopRef2, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ExpPmi1Final = [{pmiReport,    {repeat, wait_until_subscribe}, []},
		    {pmiSubscribe, {repeat, 1}, [{gp, 10}, {spec, []}]}],

    {ok, RefFinal1} = expected(AppPmi1, Handle1, ExpPmi1Final),
    {ok, RefFinal2} = expected(AppPmi2, Handle2, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal1, AppPmi1}, {RefFinal2, AppPmi2}]),

    ok = delete_app_pmi(AppPmi1, Handle1),
    ok = delete_app_pmi(AppPmi2, Handle2),
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



    
%%get_ct_config(TC)         -> ?LIB:get_ct_config(TC).
get_ct_config(TC, DefVal) -> ?LIB:get_ct_config(TC, DefVal).
    
%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).   
cleanup() -> ?LIB:cleanup().

%% initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%% finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%% counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
ldn_aliases(A, B)        -> ?LIB:ldn_aliases(A, B).

create_app(A)       -> ?LIB:create_app(A).
create_app(A, B)    -> ?LIB:create_app(A, B).
create_app(A, B, C) -> ?LIB:create_app(A, B, C).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
create_job(A, B)    -> ?LIB:create_job(A, B).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
create_jobs(A)      -> ?LIB:create_jobs(A).

create_app_pmi(A)       -> ?LIB:create_app_pmi(A).
create_app_pmi(A, B)    -> ?LIB:create_app_pmi(A, B).
create_app_pmi(A, B, C) -> ?LIB:create_app_pmi(A, B, C).
delete_app_pmi(A, B)    -> ?LIB:delete_app_pmi(A, B).


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
wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B, 25000).

%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).
    
host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

rop_file_handling(A) -> ?LIB:rop_file_handling(A).

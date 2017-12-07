%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_mixed_SUITE.erl %
%%% @version /main/R3A/R4A/R5A/R6A/R11A/1

%%% @doc 
%%% == Test suite for basic testing of ROP files ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_rop_mixed_SUITE).

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
%%% R4A/4      2015-10-14 etxpejn     Add cluster sim group
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
     tc_2app_2jobgrp_2rop
     
    ].



groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []},
     %% This suite should only be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
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
    Job = "tc_2app_1job",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi(AppPmi1, [?ROPGroup1]),
    {ok, Handle2} = create_app(AppPmi2, ?ROP_GRP_1),

    ExpPmi1 = [{pmiReport, {repeat, wait_until_subscribe}, []},
	       {pmiSubscribe, []},
	       {pmiReport, {repeat,2}, [{delay, 100}]}],

    ExpPmi2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	       {pmi2ReportRop, {repeat,2}, [{delay, 1000}]}],
    
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ExpPmi2),
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1},
			  {Job ++ "_mr2", ?ROPGroup2}]),
    wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}]),
    
    ExpPmi1Final = [{pmiReport,    {repeat, wait_until_subscribe}, []},
		    {pmiSubscribe, {repeat, 1}, [{gp, 10}, {spec, []}]}],

    {ok, RefFinal1} = expected(AppPmi1, Handle1, ExpPmi1Final),
    {ok, RefFinal2} = expected(AppPmi2, Handle2, ?EXP_FINAL),

    RopExpected  = ["<r p=\"1\">61</r>", "<r p=\"1\">61</r>", "<r p=\"1\">1234</r>"],
    {ok, RopRef} = ?ROP_HOOK:expected([{[1,2], RopExpected, ["suspect"]}]),

    %%-------------------------------------------------------------
    %% Wait for ROP
    %%-------------------------------------------------------------
    wait_expected_result([],
			 [{RopRef, ?ROP_HOOK}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ExpPmi1Final = [{pmiReport,    {repeat, wait_until_subscribe}, []},
		    {pmiSubscribe, {repeat, 1}, [{gp, 10}, {spec, []}]}],

    ok = delete_jobs(Job),
    wait_expected_result([{RefFinal1, AppPmi1}, {RefFinal2, AppPmi2}]),

    ok = delete_app_pmi(AppPmi1, Handle1),
    ok = delete_app_pmi(AppPmi2, Handle2),
    timer:sleep(15000),
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
    Job = "tc_2app_1job",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi(AppPmi1, [?ROPGroup1]),
    {ok, Handle2} = create_app(AppPmi2, ?ROP_GRP_1),

    ExpPmi1 = [{pmiReport, {repeat, wait_until_subscribe}, []},
	       {pmiSubscribe, []},
	       {pmiReport, {repeat,2}, [{delay, 1000}]}],

    ExpPmi2 = [{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	       {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	       {pmi2ReportRop, {repeat,2}, [{delay, 100}]}],
    
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ExpPmi2),
    ok = create_job(Job, [{Job ++ "_mr1", ?ROPGroup1},
			  {Job ++ "_mr2", ?ROPGroup2}]),
    wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}]),
    
    {ok, RefSubscr12} = expected(AppPmi1, Handle1, [pmiReport]),
    {ok, RefSubscr22} = expected(AppPmi2, Handle2, [pmi2ReportRop]),
    RopExpected  = ["<r p=\"1\">61</r>", "<r p=\"1\">61</r>", "<r p=\"1\">1234</r>"],
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
	       pmiReport],
    
    {ok, RefSubscr1} = expected(AppPmi1, Handle1, ExpPmi1),
    {ok, RefSubscr2} = expected(AppPmi2, Handle2, ?EXP_SUBSCR_ROPGROUP_1),
    ok = create_job(Job1, [{Job1 ++ "_mr1", ?ROPGroup1}]),
    ok = create_job(Job2, [{Job2 ++ "_mr2", ?ROPGroup2}]),
     wait_expected_result([{RefSubscr1, AppPmi1}, {RefSubscr2, AppPmi2}]),

    {ok, RefSubscr12} = expected(AppPmi1, Handle1, [pmiReport]),
    {ok, RefSubscr22} = expected(AppPmi2, Handle2, [pmi2ReportRop]),
    RopExpected  = ["<r p=\"1\">61</r>", "<r p=\"1\">61</r>", "<r p=\"1\">1234</r>"],
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
    AppPmi1 = ?NACKA,
    AppPmi2 = ?RONNIE,
    Job1 = "tc_2app_2jogrpb_2rop_1",
    Job2 = "tc_2app_2jobgrp_2rop_2",

    %%-------------------------------------------------------------
    %% Create one app using PMI, and another using PMI2
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app_pmi({AppPmi1, [{expected, [relay]}]}, [?ROPGroup1]),
    {ok, Handle2} = create_app({AppPmi2,     [{expected, [relay]}]}, ?ROP_GRP_2),

    %%-------------------------------------------------------------
    %% Create two jobs, one for each app
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, [{Job1 ++ "_mr1", ?ROPGroup1}]},
		      {Job2,
		       [{Job2 ++ "_mr2", ?ROPGroup2}], 
		       [{"reportingPeriod",   1},
			{"granularityPeriod", 1},
			{"jobGroup",          ?JOB_GROUP}]}]),

    ok = wait_until_rop({4, AppPmi1, Handle1}, {4, AppPmi2, Handle2}),

    %%-------------------------------------------------------------
    %% Check which Rop belongs to PMI1 and which to PMI2
    %%-------------------------------------------------------------
    MEData   = rpc(comsaI, get_managed_element_data, []),
    UniqueId = proplists:get_value(networkManagedElementId, MEData),

    N = [Pmi1, Pmi2] = case UniqueId of
			   undefined -> ["", "-" ++ ?JOB_GROUP];
			   _         -> [UniqueId, "-" ++ ?JOB_GROUP ++ UniqueId]
		   end,
    {RopExp1, RopExp2} = case lists:sort(N) of
			     [Pmi1, Pmi2] ->
				 {["<r p=\"1\">1234</r>"],
				  ["<r p=\"2\">81</r>", "<r p=\"3\">82</r>"]};
			     [Pmi2, Pmi1] ->
				 {["<r p=\"2\">81</r>", "<r p=\"3\">82</r>"],
				  ["<r p=\"1\">1234</r>"]}
			 end,
    
    %%-------------------------------------------------------------
    %% Check wich 2 ROP files are generated with the same time stamp
    %%-------------------------------------------------------------
    {Rop1, Rop2} = get_rops_with_same_time_stamp(),

    {ok, _RopRef} = ?ROP_HOOK:expected([{Rop2, RopExp2, ["suspect"]}, 
					{Rop1, RopExp1, ["suspect"]}]),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),

    ok = delete_app_pmi(AppPmi1, Handle1),
    ok = delete_app(AppPmi2, Handle2),
    
    
    %%-------------------------------------------------------------
    %% set PmSupport to sigle rop files
    %%-------------------------------------------------------------
    ok = rop_file_handling(?SINGLE_ROP).



%%========================================================================
%% misc functions
%%========================================================================
%% gp(?TEN_SECONDS)    -> 10;
%% gp(?THIRTY_SECONDS) -> 30.


wait_until_rop(Pmi, Pmi2) ->
    rcv(Pmi, Pmi2).
    

rcv({CntPmi, _, _}, {CntPmi2, _, _}) when CntPmi =< 0 andalso CntPmi2 =< 0 ->
    ok;
rcv({CntPmi, AppPmi1, Handle1} = Pmi1, {CntPmi2, AppPmi2, Handle2} = Pmi2) ->
    receive
	{copy, _, {pmiReport, {GP, ReportId, _ReqMaxTime}}} = X ->
	    rop_data_pmi(AppPmi1, Handle1, GP, ReportId, default_ff),
	    ct:pal("#### received ~p~n", [X]),
	    rcv({CntPmi - 1, AppPmi1, Handle1}, Pmi2);
	{copy, _, {pmi2ReportRop, {GP, ReportId, _ReqMaxTime}}} = X ->
	    rop_data(AppPmi2, Handle2, GP, ReportId, default, ?FF_TRUE),
	    ct:pal("#### received ~p~n", [X]),
	    rcv(Pmi1, {CntPmi2 - 1, AppPmi2, Handle2})
    after 15000 ->
	    ct:pal("#### ~p~n", [timeout]),
	    {error, {timeout, {Pmi1, Pmi2}}}
    end.



wait_for_rops(N, GP) ->
    wait_for_rops(N, GP, N + 2).

wait_for_rops(N, _GP, X) when X < 0 ->
    ct:fail({error, {timeout, waiting_for_rop_files, N}});
wait_for_rops(N, GP, X) ->
    case ?ROP_HOOK:list_rop_files() of
	{ok, Files} when length(Files) < N ->
	    timer:sleep(GP*1000),
	    wait_for_rops(N, GP, X - 1);
	{ok, Files} ->
	    {ok, Files}
    end.

get_rops_with_same_time_stamp() ->
    {ok, [Rop1, Rop2, Rop3 | _] = Rops} = ?ROP_HOOK:list_rop_files(),
    ct:pal("#### rop files ~p~n", [Rops]),
    [_, Time1 | _] = string:tokens(Rop1, "._"),
    [_, Time2 | _] = string:tokens(Rop2, "._"),
    [_, Time3 | _] = string:tokens(Rop3, "._"),
    grwsts(Time1, Time2, Time3).

grwsts(Time, Time, _) ->
    {1, 2};
grwsts(_, Time, Time) ->
    {2, 3}.




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
%% get_aliases()        -> ?LIB:get_aliases().

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

rop_data(A, B, C, D, E, F)  -> ?LIB:rop_data(A, B, C, D, E, F).
rop_data_pmi(A, B, C, D, E) -> ?LIB:rop_data_pmi(A, B, C, D, E).

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

rop_file_handling(A) -> ?LIB:rop_file_handling(A).

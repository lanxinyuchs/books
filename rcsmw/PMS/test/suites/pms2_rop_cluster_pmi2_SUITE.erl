%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_cluster_pmi2_SUITE.erl %
%%% @version /main/R6A/R11A/1

%%% @doc 
%%% == Test suite for ROP files in a multi DU environment ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_rop_cluster_pmi2_SUITE).

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
%%% R6A/1      2016-05-31 eolaand     Created
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

-export([tc_1job_1regular_app/1, 
	 tc_1job_1regular_1active_app/1, 
	 tc_1job_aggr_1regular_1active_app/1]).

-compile([nowarn_export_all, export_all]).

-define(TESTNODE, testnode).

-define(LIB,     pms2_test_lib).
-define(ERL_APP, pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).
-define(PROXY, pms_pmi_proxy).
-define(P_ACTIVE, active_proxy).
-define(P_REGULAR, regular_proxy).

-define(NACKA, nacka).
-define(KENTA, kenta).
-define(RONNIE, ronnie).
-define(KENNEDY, kennedy).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).

-define(SLEEP,  500).

-define(ALL_TCS, [tc_1job_1regular_app, 
		  tc_1job_1regular_1active_app, 
		  tc_1job_aggr_1regular_1active_app]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    ActiveProxy = {?PROXY, [{instance_name, ?P_ACTIVE},
			    {du_no, 1},
			    {erl_api, false}
			    %% {restart_ift, false},
			    %% {restart_ift_on_fail, false},
			    %% {trace, false}
			   ]},
    RegularProxy = {?PROXY, [{instance_name, ?P_REGULAR},
			     {du_no, 3},
			     {erl_api, false}]},
    Hooks = hooks(),
    [
     {ct_hooks, lists:append(Hooks, [ActiveProxy, RegularProxy])},
     {timetrap, {seconds, 300}}
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
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> ?ALL_TCS.



groups() ->
    [{default__group, [], []},
     {sbc__qual__sim__def__group, [], []},
     {sbc__qual__dus__def__group, [], []},
     {sbc__qual__tcu__def__group, [], []}
    ].


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------

%%========================================================================
%% tc_1job_1regular_app(_Config) -> ok
%% 
%% @doc
%% Start 1 job with two counters.  
%% Start an application on the regular node 
%% Check that the ROP file is generated with correct values for the job.
%% @end
%%========================================================================
tc_1job_1regular_app(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job1 = "tc_1job_1regular_app",
    GP  = ?GP_10_SEC,
    
    %% MT1 = [{?ROPGroup1, [?ROPType1Sum]}],
    %% MT2 = [{?ROPGroup1, [?ROPType3Min]}],
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}, 
				     {api_server, ?P_REGULAR}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Create job
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, get_mrs(MTs)}]),
    %% ok = create_jobs([{Job1, get_mrs(MT1)}, {Job2, get_mrs(MT2)}]),

    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Val1 = get_values(MTs, LDN, [{1221, 3443}]),
    %% Val1 = get_values(MT1, LDN, [1221]),
    %% Val2 = get_values(MT2, LDN, [3443]),
    Values = [{Val1, ?FF_TRUE}],
    ok = wait_report_rop(Values, AppData),    
    ok = wait_report_rop(Values, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">1221</r>", "<r p=\"2\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).

%%========================================================================
%% tc_1job_1regular_1active_app(_Config) -> ok
%% 
%% @doc
%% Start 1 job with two counters.  
%% Start one application on the regular node and one on the active node. 
%% Check that the ROP file is generated with correct values for the job.
%% @end
%%========================================================================
tc_1job_1regular_1active_app(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    AppAct = ?KENTA,
    AppReg = ?NACKA,
    Job1 = "tc_1job_1regular_1active_app",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, HandleAct}  = create_app({AppAct, [{expected, [relay]}, 
					    {api_server, ?P_ACTIVE}]}),
    {ok, LDNAct}     = rpc(gmfI, check_string, ?LDN_DEF_1(AppAct)),
    AppDataAct = {AppAct, HandleAct, GP},
    
    {ok, HandleReg} = create_app({AppReg, [{expected, [relay]}, 
					   {api_server, ?P_REGULAR}]}),
    {ok, LDNReg}    = rpc(gmfI, check_string, ?LDN_DEF_1(AppReg)),
    AppDataReg      = {AppReg, HandleReg, GP},
    
    %%-------------------------------------------------------------
    %% Create job
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, get_mrs(MTs)}]),

    %%-------------------------------------------------------------
    %% Wait for subscribe
    %%-------------------------------------------------------------
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    ok = wait_until_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    ValsAct = get_values(MTs, LDNAct, [{1221, 3443}]),
    ValsReg = get_values(MTs, LDNReg, [{2552, 6776}]),
    ValuesAct = [{ValsAct, ?FF_TRUE}],
    ValuesReg = [{ValsReg, ?FF_TRUE}],
    ok = wait_report_rop(ValuesAct, AppDataAct),    
    ok = wait_report_rop(ValuesReg, AppDataReg),    
    ok = wait_report_rop(ValuesAct, AppDataAct),    
    ok = wait_report_rop(ValuesReg, AppDataReg),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">1221</r>", "<r p=\"2\">3443</r>",
			    "<r p=\"1\">2552</r>", "<r p=\"2\">6776</r>"]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(AppAct, HandleAct),
    ok = delete_app(AppReg, HandleReg).


%%========================================================================
%% tc_1job_aggr_1regular_1active_app(_Config) -> ok
%% 
%% @doc
%% Start 1 job with two counters.  
%% Start one application on the regular node and one on the active node. 
%% Both apps reports values for the same LDN.
%% Check that the ROP file is generated with correct aggregated values for 
%% the job.
%% @end
%%========================================================================
tc_1job_aggr_1regular_1active_app(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    AppAct = ?NACKA,
    AppReg = ?KENTA,
    Job1 = "tc_1job_aggr_1regular_1active_app",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, HandleAct}  = create_app({AppAct, [{expected, [relay]}, 
					    {api_server, ?P_ACTIVE}]}),
    {ok, LDN}     = rpc(gmfI, check_string, ?LDN_DEF_1(AppAct)),
    AppDataAct = {AppAct, HandleAct, GP},
    
    {ok, HandleReg} = create_app({AppReg, [{expected, [relay]}, 
					   {api_server, ?P_REGULAR}]}),
    %% {ok, LDNReg}    = rpc(gmfI, check_string, ?LDN_DEF_1(AppReg)),
    AppDataReg      = {AppReg, HandleReg, GP},
    
    %%-------------------------------------------------------------
    %% Create job
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, get_mrs(MTs)}]),

    %%-------------------------------------------------------------
    %% Wait for subscribe
    %%-------------------------------------------------------------
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    ok = wait_until_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    ValsAct = get_values(MTs, LDN, [{1221, 3443}]),
    ValsReg = get_values(MTs, LDN, [{2552, 6776}]),
    ValuesAct = [{ValsAct, ?FF_TRUE}],
    ValuesReg = [{ValsReg, ?FF_TRUE}],
    ok = wait_report_rop(ValuesAct, AppDataAct),    
    ok = wait_report_rop(ValuesReg, AppDataReg),    
    ok = wait_report_rop(ValuesAct, AppDataAct),    
    ok = wait_report_rop(ValuesReg, AppDataReg),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">3773</r>", "<r p=\"2\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(AppAct, HandleAct),
    ok = delete_app(AppReg, HandleReg).



%%========================================================================
%% get_values(MTs, LDN, Values) -> AliasValues
%%
%% Create values to be reported in the next rop data request
%%========================================================================
get_values([{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
	   LDN,
	   [{Sum, Min}]) -> 
    Vals = [{?ROPGroup1, [{LDN, [{?ROPType1Sum, [Sum]},
				 {?ROPType3Min, [Min]}]}]}],
    aliasify_values(Vals);

get_values([{RG, MTs}], LDN, Vals) -> 
    MVals = lists:map(fun(Val) when is_list(Val) ->
			      Val;
			 (Val) ->
			      [Val]
		      end, Vals),
    MeasVals = [{RG, [{LDN, lists:zip(MTs, MVals)}]}],
    aliasify_values(MeasVals).
    



%%========================================================================
%% lib functions
%%========================================================================

%%========================================================================
%% misc functions
%%========================================================================

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

update_job(A, B) ->    
    {ok, _} = open_trans(),
    ?LIB:update_job(A, B),
    Res = close_trans(),    
    to(4),
    Res.

delete_job(A)       -> ?LIB:delete_job(A).

create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)       -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

rop_data({A, B, C}, D, E, F) -> ?LIB:rop_data(A, B, C, D, E, F).


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
wait_report_rop(A, B)    -> ?LIB:wait_report_rop(A, B).
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).
    


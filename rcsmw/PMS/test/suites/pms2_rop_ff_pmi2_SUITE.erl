%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_ff_pmi2_SUITE.erl %
%%% @version /main/R3A/R4A/R11A/1

%%% @doc 
%%% == Test suite for ROP testing the use of final fragment ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_rop_ff_pmi2_SUITE).

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
%%% R3A/1      2015-04-30 eolaand     Created
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

-define(LIB,     pms2_test_lib).
-define(ERL_APP, pms2_erl_app).
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

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    ProxyPars = {?PROXY, get_ct_config(?PROXY, [])},
    Hooks = lists:keystore(?PROXY, 1, hooks(), ProxyPars),
    [
     {ct_hooks, Hooks},
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
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     tc_2job_diff_mt_1app_frag
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
%% tc_2job_diff_mt_1app_frag(_Config) -> ok
%% 
%% @doc
%% Start two jobs with one counter each 
%% Send fragmented pmiData with one counter value in each fragment.
%% Check that the ROP file is generated with correct values for each job.
%% @end
%%========================================================================
tc_2job_diff_mt_1app_frag(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?NACKA,
    Job1 = "tc_2job_diff_mt_1app_frag_1",
    Job2 = "tc_2job_diff_mt_1app_frag_2",
    GP  = ?GP_10_SEC,
    
    MT1 = [{?ROPGroup1, [?ROPType1Sum]}],
    MT2 = [{?ROPGroup1, [?ROPType3Min]}],
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_jobs([{Job1, get_mrs(MT1)}, {Job2, get_mrs(MT2)}]),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Val1 = get_values(MT1, LDN, [1221]),
    Val2 = get_values(MT2, LDN, [3443]),
    Values = [{Val1, ?FF_FALSE}, {Val2, ?FF_TRUE}],
    ok = wait_report_rop(Values, AppData),    
    ok = wait_report_rop(Values, AppData),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">1221</r>", "<r p=\"1\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job1, Job2]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).



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
    


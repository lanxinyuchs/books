%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_app_del_SUITE.erl %
%%% @version /main/R3A/R4A/R5A/R11A/1

%%% @doc 
%%% == Test suite for ROP when jobs are stopped and started ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms2_rop_app_del_SUITE).

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
    ProxyPars = {?PROXY, get_ct_config(?PROXY, [{erl_api, false},
						{trace_ift, true}, 
						{trace_child, true}])},
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
     tc_15,
     tc_app_delete
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
%% tc_15(_Config) -> ok
%% 
%% @doc
%% Start one job 
%% Check 1st ROP
%% Stop the job between 04 and 15
%% Start the job again, same GP between 04 and 15
%% Check 2nd ROP
%% @end
%%========================================================================
tc_15(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App1 = ?NACKA,
    App2 = ?RONNIE,
    Job = "tc_15",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create apps
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app({App1, [{expected, [relay]}]}),
    {ok, LDN1}    = rpc(gmfI, check_string, ?LDN_DEF_1(App1)),
    AppData1      = {App1, Handle1, GP},
    
    {ok, Handle2} = create_app({App2, [{expected, [relay]}]}),
    AppData2      = {App2, Handle2, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe([get_subscribe_data(GP, MTs), 
			 get_subscribe_data(GP, MTs)]),
    
    %%-------------------------------------------------------------
    %% Wait for ROP requests (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN1, [{1221, 3443}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData1),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData2), 
    %% second ROP request
    ok = wait_report_rop(Values, ?FF_TRUE, AppData1),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData2),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">2442</r>", "<r p=\"2\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% Stop app1 after ROP file recevied,
    %% time should be between 04 and 15
    %%-------------------------------------------------------------
    ok = delete_app(App1, Handle1),

    %%-------------------------------------------------------------
    %% Start app1, time should be between 04 and 15
    %%-------------------------------------------------------------
    {ok, Handle11} = create_app({App1, [{expected, [relay]}]}),
    AppData11      = {App1, Handle11, GP},
    ok = wait_until_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Wait for 2nd ROP 
    %% The first GP after start will be incomplete and the ROP file
    %% should only have data from app2.
    %%-------------------------------------------------------------
    Values2 = get_values(MTs, LDN1, [{3443, 4554}]),
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData11),    
    ok = wait_report_rop(Values2, ?FF_TRUE, AppData2),    
    
    %%-------------------------------------------------------------
    %% Check 2nd ROP 
    %%-------------------------------------------------------------
    ok = check_rop_file(2, ["<r p=\"1\">3443</r>", "<r p=\"2\">4554</r>"]),
    
    %%-------------------------------------------------------------
    %% Wait for 3rd ROP 
    %% Both apps result should be in the rop file
    %%-------------------------------------------------------------
    Values3 = get_values(MTs, LDN1, [{2112, 7887}]),
    ok = wait_report_rop(Values3, ?FF_TRUE, AppData11),    
    ok = wait_report_rop(Values3, ?FF_TRUE, AppData2),    
    
    %%-------------------------------------------------------------
    %% Check 3rd ROP 
    %%-------------------------------------------------------------
    ok = check_rop_file(3, ["<r p=\"1\">4224</r>", "<r p=\"2\">7887</r>"]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App1, Handle11),
    ok = delete_app(App2, Handle2).



%%========================================================================
%% tc_15(_Config) -> ok
%% 
%% @doc
%% Start one job and one app. 
%% Check 1st ROP.
%% Send data but not final fragment.
%% Stop the job.
%% Check 2nd ROP
%% @end
%%========================================================================
tc_app_delete(_Config) -> 
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App1 = ?NACKA,
    Job = "tc_app_delete",
    GP  = ?GP_10_SEC,
    
    MTs = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],
    
    %%-------------------------------------------------------------
    %% Create apps
    %%-------------------------------------------------------------
    {ok, Handle1} = create_app({App1, [{expected, [relay]}]}),
    {ok, LDN1}    = rpc(gmfI, check_string, ?LDN_DEF_1(App1)),
    AppData1      = {App1, Handle1, GP},
    
    %%-------------------------------------------------------------
    %% Wait for first subscribe
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe([get_subscribe_data(GP, MTs)]),
    
    %%-------------------------------------------------------------
    %% Wait for ROP requests (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN1, [{1221, 3443}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData1), 
    %% second ROP request
    ok = wait_report_rop(Values, ?FF_TRUE, AppData1),    
    
    %%-------------------------------------------------------------
    %% Check 1st ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(1, ["<r p=\"1\">1221</r>", "<r p=\"2\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% Stop app1 after report received,
    %%-------------------------------------------------------------
    ok = wait_report_rop(Values, ?FF_FALSE, AppData1),    
    ok = delete_app(App1, Handle1),

    %%-------------------------------------------------------------
    %% Check 2nd ROP
    %%-------------------------------------------------------------
    ok = check_rop_file(2, ["<r p=\"1\">1221</r>", "<r p=\"2\">3443</r>"]),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job).



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
    aliasify_values(Vals).




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
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).
    


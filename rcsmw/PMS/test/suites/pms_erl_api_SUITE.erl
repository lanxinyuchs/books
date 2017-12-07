%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_erl_api_SUITE.erl %
%%% @version /main/R3A/R4A/R11A/1

%%% @doc 
%%% == Test suite for basic testing of ROP files ==
%%% This test suite can be used in both simulated and target environment. 
%%% @end

-module(pms_erl_api_SUITE).

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

-define(LIB,      pms_test_lib).
-define(LIB2,     pms2_test_lib).
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


%%=========================================================
%% sc macros
%%=========================================================
-define(SC_OK, 0).

-define(CLI_USER, cli_user).

-define(CONFIGURE, "configure").
-define(COMMIT, "commit").
-define(TOP, "top").

-define(PRINT_OPT, print).
-define(DEF_SC_MOS, "ManagedElement=1,Erat=1,Elizabeth=1").


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    ProxyTag = {pms_pmi_proxy, get_ct_config(pms_pmi_proxy)},
    Hooks   = lists:keystore(pms_pmi_proxy, 1, hooks(), ProxyTag),
    CliHook = [{rct_cli, {?CLI_USER, [manual_connect]}}],
    [{ct_hooks, Hooks ++ CliHook}].


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
     tc_sc_counter
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
    %%{ok, RefSubscr2} = expected(AppPmi2, Handle2, ?EXP_SUBSCR_ROPGROUP_1),
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
%% tc_sc_counter(Config) -> ok.
%% 
%% @doc 
%% show counters simplest case
%% @end
%%========================================================================
tc_sc_counter(_Config) ->
    App = elizabeth,
    {ok, Handle} = start_app(App, [{"ROPType6Sum", [345]}]),

    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show-counters", ?PRINT_OPT),
    ok  = check_result("ROPType6Sum=345", Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok  = delete_app_pmi(App, Handle).
    

cli_init(elizabeth) ->
    ok = rct_cli:connect(?CLI_USER),

    rct_cli:send(?CLI_USER, "top", ?PRINT_OPT),
    rct_cli:send(?CLI_USER, "ManagedElement=1,Erat=1,Elizabeth=1", ?PRINT_OPT).


cli_end(elizabeth, Values) ->
    wait_report_sc(any, Values),    

    {ok, {Res, _}} = rct_cli:send(?CLI_USER, ?TOP, "ROPType6Sum=1"),
    ct:log("show-counters result: ~s~n", [Res]),

    ok = rct_cli:disconnect(?CLI_USER).

cli_end(elizabeth, Values, Expected) ->
    
    wait_report_sc(any, Values),    

    {ok, {Res, _}} = rct_cli:send(?CLI_USER, ?TOP, Expected),
    ct:log("show-counters result: ~s~n", [Res]),

    ok = rct_cli:disconnect(?CLI_USER).



start_app(App, Values) ->
    start_app(App, Values, ?SC_OK, "").


start_app(App, Values, Result, ErrorStr) ->
    GrpAlias = 4,
    CntAlias = 41,
    _Groups = [{"ROPGroup2", GrpAlias, [{"ROPType6Sum", CntAlias},
				       {"ROPType7Avg", 42},
				       {"ROPType8Min", 43},
				       {"ROPType9Max", 44}, 
				       {"ROPType10LU", 45}]}],

    TopLdn = "ManagedElement=1,Erat=1",
    {ok, Handle} = create_app_pmi(App, [?ROPGroup2], TopLdn),

    Expected = [{pmiReportShowCounters, 
		 {repeat, 1},
		 [{values,     Values},
		  {result,     Result},
		  {horror_str, ErrorStr}]}], %% Kalle against error
    
    {ok, _Ref} = expected(App, Handle, Expected),

    {ok, Handle}.

check_result([H | _] = Expected, Received) when is_list(H) ->
    cr(Expected, Received);
check_result(Expected, Received) ->
    check_result([Expected], Received).


cr([], {ok, _})->
    ok;
cr([], Error)->
    Error;
cr([Expected | T], Received)->
    Cont = cr_loop(Expected, Received, 10),
    cr(T, Cont).



cr_loop(Expected, {ok, Received}, N) when N > 0 ->
    Rcvd = re:replace(Received, "[<>]", "", [{return, list},global]),
    ct:pal("### Expected cr loop  ~p~n"
	   "### Received  ~p~n", [Expected, Rcvd]),
    case string:str(Rcvd, Expected) of
	0 ->
	    ct:pal("#### 00000~n"),
	    timer:sleep(1000),
	    Rec = rct_cli:send(?CLI_USER, ?TOP, ?PRINT_OPT),
	    cr_loop(Expected, Rec, N - 1);
	X ->
	    ct:pal("#### ~p~n ", [X]),
	    {ok, string:substr(Received, X)}
    end;
cr_loop(_, {ok, _}, _) ->
    {error, not_received};
cr_loop(_, Error, _) ->
    {error, Error}.





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



    
start(A, B)          -> {ok, _} = pms_erl_app:start(A, B).
stop(A)              -> ok = pms_erl_app:stop(A).   
initialize_2(A, B)   -> ok = ?LIB:initialize_2(A, B).
finalize(A)          -> ok = ?LIB:finalize(A).
wait_report_sc(A, B) -> ?LIB:wait_report_sc(A, B).


get_ct_config(TC)         -> ?LIB2:get_ct_config(TC).
    
cleanup() -> ?LIB2:cleanup().

create_app(A)       -> ?LIB2:create_app(A).
create_app(A, B)    -> ?LIB2:create_app(A, B).
create_app(A, B, C) -> ?LIB2:create_app(A, B, C).
delete_app(A, B)    -> ?LIB2:delete_app(A, B).
create_job(A, B)    -> ?LIB2:create_job(A, B).
create_job(A, B, C) -> ?LIB2:create_job(A, B, C).
create_jobs(A)      -> ?LIB2:create_jobs(A).

create_app_pmi(A)       -> ?LIB2:create_app_pmi(A).
create_app_pmi(A, B)    -> ?LIB2:create_app_pmi(A, B).
create_app_pmi(A, B, C) -> ?LIB2:create_app_pmi(A, B, C).
delete_app_pmi(A, B)    -> ?LIB2:delete_app_pmi(A, B).


delete_jobs(A)      -> ?LIB2:delete_jobs(A).
delete_job(A)       -> ?LIB2:delete_job(A).

create_job_mr(A, B, C) -> ?LIB2:create_job_mr(A, B, C).
delete_mr(A, B)       -> ?LIB2:delete_mr(A, B).

tables()           -> ?LIB2:tables().
check_tables(A)    -> ?LIB2:check_tables(A).

rpc(A, B, C) -> ?LIB2:rpc(A, B, C).

expected(A, B, C)         -> ?LIB2:expected(A, B, C).
wait_expected_result(A)   -> ?LIB2:wait_expected_result(A, []).
wait_expected_result(A,B) -> ?LIB2:wait_expected_result(A, B, 20000).

    
host()  -> ?LIB2:host().
hooks() -> ?LIB2:hooks().

open_trans()  -> ?LIB2:open_trans().
close_trans() -> Res = ?LIB2:close_trans(), to(4), Res.

ldn_aliases(A, B) -> ?LIB2:ldn_aliases(A, B).

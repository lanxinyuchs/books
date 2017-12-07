%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_rop_rm_add_mr_7_SUITE.erl %
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/4

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms_rop_rm_add_mr_7_SUITE).

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

-export([add_mr_after_cp/1]).

-define(TESTNODE, testnode).

-define(LIB,     pms2_test_lib).
-define(ERL_APP, pms2_erl_app).
-define(ROP_HOOK, pms_rop_hook).

-define(SUNE, sune).
-define(TINA, tina).
-define(KURT, kurt).
-define(TEST_APPS,  [?SUNE]).

-define(JOB_1, "job_fh_1").
-define(JOB_2, "job_fh_2").

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

end_per_testcase(_TestCase, _Config) ->
    %%log_rop_files(Config),
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
       add_mr_after_cp
    ],
    All ++ all_host(host()).

all_host(?TESTNODE) ->
    [];
all_host(_PmsSim) ->
    [].

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
%% add_mr_after_cp(Config) -> ok.
%% 
%% @doc 
%% Creates one job with one mr 
%% and adds another mr after collection period
%% @end
%%========================================================================
add_mr_after_cp(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App      = ?SUNE,
    GP       = ?GP_1_MIN,
    Job      = "add_mr_after_cp",

    MTs1     = [{?ROPGroup1, [?ROPType1Sum]}],
    MTs2     = [{?ROPGroup1, [?ROPType1Sum, ?ROPType3Min]}],

    AppAttrs = [{expected, [relay]}],

    JobAttrs = [{"granularityPeriod", ?ONE_MIN},
                {"reportingPeriod",   ?ONE_MIN},
                {"currentJobState",   ?ACTIVE}],

    %%-------------------------------------------------------------
    %% Create the app.
    %%-------------------------------------------------------------
    {ok, Handle} = create_app_pmi({App, AppAttrs}, [?ROPGroup1]),
    [LDN]        = ?LDN_DEF_1(App),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Start job and wait for subscribe.
    %%-------------------------------------------------------------
    ok = create_job(Job, get_mrs(MTs1), JobAttrs),
    ok = wait_until_pmi_subscribe({GP, MTs1}),
    
    %%-------------------------------------------------------------
    %% Check ROP with FF=true => end cp
    %%-------------------------------------------------------------
    Values = get_pmi_values(MTs1, LDN, [[1221]]) ++ [{LDN, []}],
    ok = wait_pmi_report_rop(Values, AppData),
    ok = wait_pmi_report_rop(Values, AppData),
    ok = check_rop_file(1, [?ROPGroup1, ?ROPType1Sum], [?ROPType3Min]),

    %%-------------------------------------------------------------
    %% Add MR to the job after cp
    %%-------------------------------------------------------------
    ok = wait_pmi_report_rop(Values, AppData),
    ok = create_job(Job, get_mrs(MTs2), JobAttrs),
    ok = wait_until_pmi_subscribe({GP, MTs2}),
    
    Values2 = get_pmi_values(MTs2, LDN, [[1221], [1222]]),
    ok = wait_pmi_report_rop(Values2, AppData),
    ok = check_rop_file(3, [?ROPGroup1, ?ROPType1Sum], [?ROPType3Min]),
    ok = wait_pmi_report_rop(Values2, AppData),
    ok = check_rop_file	(4, [?ROPGroup1, ?ROPType1Sum, ?ROPType3Min], []),

    %%-------------------------------------------------------------
    %% Delete the job.
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_pmi_subscribe({GP, []}),

    %%-------------------------------------------------------------
    %% Delete the app.
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).
    
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% get_pmi_values([{RG, MTs}], LDN, Values, FF) -> ReportValues
%%
%% Create values to be reported in the
%% next ROP data request for PMI interface.
%%========================================================================
get_pmi_values([{RG, MTs}], LDN, Vals) ->
    ToList = fun
        (Val) when is_list(Val) -> Val;
        (Val) -> [Val]
    end,

    MVals  = lists:map(ToList, Vals),
    Zipped = lists:zip(MTs, MVals),
    Joined = [{RG, Zipped}],
    
    [{LDN, Joined}];


get_pmi_values(MRSpecs, LDN, Vals) ->
    GetEachValue = fun(MRSpec, Values) ->
        get_pmi_values([MRSpec], LDN, Values)
    end,

    Res = lists:zipwith(GetEachValue, MRSpecs, Vals),
    lists:flatten(Res).

%%% #---------------------------------------------------------
%%% #3.4   TESTING LIBRARY MAPPINGS
%%% #---------------------------------------------------------

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


%% log_rop_files(Config) ->
%%     {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
%%     log_rop_files(RopData, Config).
%%
%% log_rop_files(RopData, Config) ->
%%     pms_test_lib:log_rop_files(RopData, Config).

get_ct_config(TC)         -> ?LIB:get_ct_config(TC).
%%get_ct_config(TC, DefVal) -> ?LIB:get_ct_config(TC, DefVal).

%% start(A)  -> {ok, _} = ?ERL_APP:start(A, []).
%% stop(A)   -> ok      = ?ERL_APP:stop(A).
cleanup() -> ?LIB:cleanup().

%%initialize(A, B)     -> {ok, _Handle} = ?LIB:initialize(A, B).
%%finalize(A, B)       -> ok = ?LIB:finalize(A, B).
%%counter_map(A, B, C) -> ok = ?LIB:counter_map(A, B, C).
%%get_aliases()        -> ?LIB:get_aliases().

%%create_app(A)       -> ?LIB:create_app(A).
%%create_app(A, B)    -> ?LIB:create_app(A, B).
%%create_app(A, B, C) -> ?LIB:create_app(A, B, C).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
%%create_job(A, B)    -> ?LIB:create_job(A, B).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
%%create_jobs(A)      -> ?LIB:create_jobs(A).

%%create_app_pmi(A)       -> ?LIB:create_app_pmi(A).
create_app_pmi(A, B)    -> ?LIB:create_app_pmi(A, B).
%%create_app_pmi(A, B, C) -> ?LIB:create_app_pmi(A, B, C).
%%delete_app_pmi(A, B)    -> ?LIB:delete_app_pmi(A, B).


%%create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
%%delete_job(A)       -> ?LIB:delete_job(A).

%%create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
%%delete_mr(A, B)       -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
%%tables()           -> ?LIB:tables().
%%check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

%%rop_data({A, B, C}, D, E, F) -> ?LIB:rop_data(A, B, C, D, E, F).


%%expected(A, B, C)               -> ?LIB:expected(A, B, C).
%%wait_expected_result(A)         -> ?LIB:wait_expected_result(A, []).
%%wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B, 20000).

%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).

host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%%open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

%%check_rop_file(A, B)     -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)  -> ?LIB:check_rop_file(A, B, C).
%%wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
%%get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
%%wait_pmi_subscribe(A)        -> ?LIB:wait_pmi_subscribe(A).
wait_until_pmi_subscribe(A)  -> ?LIB:wait_until_pmi_subscribe(A).
%%wait_pmi_report_rop(A, B, C) -> ?LIB:wait_pmi_report_rop(A, B, C).
wait_pmi_report_rop(A, B)    -> ?LIB:wait_pmi_report_rop(A, B).
%%get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
%%aliasify_values(A)       -> ?LIB:aliasify_values(A).

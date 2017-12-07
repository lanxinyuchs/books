%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_rop_rm_add_mr_2_SUITE.erl %
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R6A/1

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms2_rop_rm_add_mr_2_SUITE).

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


-export([rm_mr_in_cp_add_after/1]).

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

-define(COMMON_JOB_GROUP, undefined).
-define(JOB_GROUP1,       "GRAT").
-define(JOB_GROUP2,       "WRAT").

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
        rm_mr_in_cp_add_after
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
%% rm_mr_in_cp_add_after(Config) -> ok.
%% 
%% @doc 
%% Creates one job with two mrs 
%% removes mr in cp and adds it after cp
%% @end
%%========================================================================
rm_mr_in_cp_add_after(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "rm_mr_in_cp_add_after",
    GP  = ?GP_1_MIN,
    MTs  = [{?Group2, [?Type2, ?Type3]}],
    MTs2 = [{?Group2, [?Type2]}],

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},
    
    %%-------------------------------------------------------------
    %% Start job and wait for subscribe
    %%-------------------------------------------------------------
    Attrs = [{"granularityPeriod", ?ONE_MIN},
	         {"reportingPeriod",   ?ONE_MIN},
	         {"currentJobState",   ?ACTIVE}],
    ok = create_job(Job, get_mrs(MTs), Attrs),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Check first ROP with FF_TRUE => end cp
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{1221, 1222}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    ok = check_rop_file(1, [?Group2, ?Type2, ?Type3], []),

    %%-------------------------------------------------------------
    %% Remove MR during cp
    %%-------------------------------------------------------------
    {ok, Id} = enter_cp(GP), %%inside CP
	%% ok = wait_report_rop(Values, ?FF_FALSE, AppData),
    ok = delete_mr(Job, ?Type3),
    ok = wait_subscribe(get_subscribe_data(GP, MTs2)),
    exit_cp(Values, AppData, Id), %%outside CP
    ok = check_rop_file(2, [?Group2, ?Type2, ?Type3], []),
    %%ROP is generated
    %%add mr
    ok = create_job(Job, get_mrs(MTs), Attrs),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),
    
    %%-------------------------------------------------------------
    %% Check rops after rm in cp and add outside of cp
    %%-------------------------------------------------------------    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    %% this ROP should not contain Type3
    ok = check_rop_file(3, [?Group2, ?Type2], [?Type3]),
    %% ok = check_rop_file(3, [], ["suspect"]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),
    %% next ROP should contain Type3
    ok = check_rop_file(4, [?Group2, ?Type2, ?Type3], []),
    %% ok = check_rop_file(4, [], ["suspect"]),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs([Job]),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).
    
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%========================================================================
%% misc functions
%%========================================================================
%%========================================================================
%% get_values(MTs, LDN, Values) -> AliasValues
%%
%% Create values to be reported in the next rop data request
%%========================================================================
get_values([{?Group1, [?Type1]}], LDN, [{Val}]) -> 
    Vals = [{?Group1, [{LDN, [{?Type1, [Val]}]}]}],
    aliasify_values(Vals);

get_values([{?Group2, [?Type2]}], LDN, [{Val}]) -> 
    Vals = [{?Group2, [{LDN, [{?Type2, [Val]}]}]}],
    aliasify_values(Vals);

get_values([{?Group2, [?Type2, ?Type3]}], LDN, [{Val2, Val3}]) -> 
    Vals = [{?Group2, [{LDN, [{?Type2, [Val2]}, {?Type3, [Val3]}]}]}],
    aliasify_values(Vals).


%%========================================================================
%% enter_cp(GP) -> {ok, ReportId} | 
%%                                          {error, Description}
%%
%% Enters collection period = wait_report_rop with FF_FALSE
%%========================================================================
enter_cp(GP) ->
    receive
	{copy, _, {pmi2ReportRop, {_, Id, _}}} ->
	    {ok, Id};
	{copy, _, Unexpected} ->
	    ct:pal("wait report rop: Unexpected ~p~n", [Unexpected]),
	    {error, Unexpected}
    after (GP + 5)*1000 ->
	    {error, no_report_rop_received}
    end.

    
%%========================================================================
%% exit_cp(VFFs, {App, Handle, GP}) -> ok
%%
%% Ends collection period = sends fragment with FF_TRUE
%%========================================================================
exit_cp(Values, {App, Handle, GP}, Id) ->
    e_cp([{Values, ?FF_TRUE}], {App, Handle, GP}, Id),
    ok.
    
e_cp(VFFs, {App, Handle, GP}, Id) ->
    [rop_data(App, Handle, GP, Id, Values, FF) ||
		{Values, FF} <- VFFs].

    
%% gp(?TEN_SECONDS)    -> 10;
%% gp(?THIRTY_SECONDS) -> 30.

%% to() ->
%%     to(1).

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
%%create_app(A, B)    -> ?LIB:create_app(A, B).
%%create_app_pmi(A)   -> ?LIB:create_app_pmi(A).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
%%delete_app_pmi(A, B) -> ?LIB:delete_app_pmi(A, B).
%%create_job(A, B)    -> ?LIB:create_job(A, B).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
%%create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
%%update_job(A, B)    -> ?LIB:update_job(A, B).
%%delete_job(A)       -> ?LIB:delete_job(A).

%%create_job_mr(A, B, C) -> ?LIB:create_job_mr(A, B, C).
delete_mr(A, B)        -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
%%tables()           -> ?LIB:tables().
%%check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

%%expected(A, B, C)               -> ?LIB:expected(A, B, C).
%%wait_expected_result(A)         -> ?LIB:wait_expected_result(A).
%%wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B).
%%wait_expected_error_result(A,B) -> ?LIB:wait_expected_error_result(A, B).
%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).
    
host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

%%open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

%%log_rop_files(Config) -> 
%%    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
%%    log_rop_files(RopData, Config).

%%log_rop_files(RopData, Config) -> pms_test_lib:log_rop_files(RopData, Config).



%%check_rop_file_name(A, B) -> ?LIB:check_rop_file_name(A, B).
%%check_rop_file(A, B)     -> ?LIB:check_rop_file(A, B).
check_rop_file(A, B, C)  -> ?LIB:check_rop_file(A, B, C).
%%wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
%%get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
wait_until_subscribe(A)  -> ?LIB:wait_until_subscribe(A).
wait_report_rop(A, B, C) -> ?LIB:wait_report_rop(A, B, C).
%%wait_report_rop(A, B)    -> ?LIB:wait_report_rop(A, B).
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).

rop_data(A, B, C, D, E, F) -> ?LIB:rop_data(A, B, C, D, E, F).


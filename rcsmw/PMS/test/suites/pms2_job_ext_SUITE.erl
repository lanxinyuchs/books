%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_job_ext_SUITE.erl %
%%% @version /main/R3A/R4A/R5A/R6A/R8A/1

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms2_job_ext_SUITE).

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
%%% R3A/1      2014-09-04 uabesvi     Copied from pms_job_ext_SUITE
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


-export([job_without_mr/1]).
-export([illegal_group/1]).
-export([illegal_gp/1]).
%% -export([one_job_one_mr_del_mr/1]).
%% -export([one_job_two_mr_del_mr/1]).
-export([non_valid_mr/1]).
-export([valid_and_non_valid_mr/1]).
-export([non_valid_grp/1]).
-export([valid_and_non_valid_grp/1]).
%%-export([multi_trans/1]).
-export([create_del_trans/1]).
-export([non_legacy_rp/1]).
-export([no_mr/1]).
-export([no_sr/1]).
-export([multi_mr/1]).




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
	   job_without_mr,
	   illegal_group,
	   illegal_gp,
%% 	   one_job_one_mr_del_mr, %% From R16B it is allowed to update jobs
%% 	   one_job_two_mr_del_mr,
	   non_valid_mr,
	   valid_and_non_valid_mr,
	   non_valid_grp,
	   valid_and_non_valid_grp,
	   create_del_trans,
	   non_legacy_rp,
	   no_mr,
	   no_sr
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
%% job_without_mr(Config) -> ok.
%% 
%% @doc 
%% create and delete a pm job without mr
%% @end
%%========================================================================
job_without_mr(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "job_without_mr",

    {ok, Handle} = create_app(App),

    %%-------------------------------------------------------------
    %% create a job without mr, should not be valid
    %%-------------------------------------------------------------
    {error, _} = create_job(Job, []),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% illegal_gp(Config) -> ok.
%% 
%% @doc 
%% create a pm job where GP > RP
%% @end
%%========================================================================
illegal_gp(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "illegal_gp",

    {ok, Handle} = create_app(App),

    %%-------------------------------------------------------------
    %% try to create a job with GP > RP, should not work.
    %%-------------------------------------------------------------
    Attrs = [{"granularityPeriod", ?THIRTY_SECONDS},
 	     {"reportingPeriod",   ?TEN_SECONDS},
 	     {"currentJobState",   ?ACTIVE}],
    {error, _} = create_job(Job, ?JOB_GROUP_1, Attrs),

    ok = delete_app(App, Handle),
    check_tables(Tables).



%%========================================================================
%% illegal_group(Config) -> ok.
%% 
%% @doc 
%% try to initialize an illegal group
%% @end
%%========================================================================
illegal_group(_Config) ->
    Tables = tables(),

    App = ?SUNE,
    Job = "illegal_group",

    {ok, Handle} = create_app(App),

    {ok, RefSubscr} = expected(App, Handle, [no_subscribe_request]),
    ok = create_job(Job, [{Job ++ "_mr1", "illegal_group"}]),
    wait_expected_result(RefSubscr, App),

    {ok, RefFinal} = expected(App, Handle, [no_subscribe_request]),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% one_job_one_mr_del_mr(Config) -> ok.
%% 
%% \@doc 
%% create one pm job with one MR, try to only delete the MR
%% \@end
%%========================================================================
%% one_job_one_mr_del_mr(_Config) ->
%%     %%-------------------------------------------------------------
%%     %% create apps and jobs
%%     %%-------------------------------------------------------------
%%     Tables = tables(),

%%     App = ?SUNE,
%%     Job = "one_job_one_mr_del_mr",

%%     {ok, Handle} = create_app(App),

%%     {ok, RefSubscr} = expected(App, Handle, ?EXP_SUBSCR_GROUP_1),
%%     ok = create_job(Job, ?JOB_GROUP_1),
%%     wait_expected_result(RefSubscr, App),

%%     %%-------------------------------------------------------------
%%     %% delete the pm job
%%     %% (it is not possible to remove MR in the current ECIM model)
%%     %%-------------------------------------------------------------
%%     {ok, _} = open_trans(),
%%     {error, _} = delete_mr(Job, Job ++ "_mr1"),
%%     to(),


%%     %%-------------------------------------------------------------
%%     %% delete apps and jobs
%%     %%-------------------------------------------------------------
%%     {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
%%     ok = delete_jobs(Job),
%%     wait_expected_result(RefFinal, App),

%%     ok = delete_app(App, Handle),
%%     check_tables(Tables).

    
%%========================================================================
%% one_job_two_mr_del_mr(Config) -> ok.
%% 
%% @doc 
%% create one pm job with one MR, try to only delete the MR
%% @end
%%========================================================================
%% one_job_two_mr_del_mr(_Config) ->
%%     %%-------------------------------------------------------------
%%     %% create apps and jobs
%%     %%-------------------------------------------------------------
%%     Tables = tables(),

%%     App = ?SUNE,
%%     Job = "one_job_two_mr_del_mr",

%%     {ok, Handle} = create_app(App),

%%     {ok, RefSubscr} = expected(App, Handle, ?EXP_SUBSCR_GROUP_2),
%%     ok = create_job(Job, ?JOB_GROUP_2),
%%     wait_expected_result(RefSubscr, App),

%%     %%-------------------------------------------------------------
%%     %% delete the pm job
%%     %% (it is not possible to remove MR in the current ECIM model)
%%     %%-------------------------------------------------------------
%%     {ok, _} = open_trans(),
%%     delete_mr(Job, Job ++ "_mr2"),
%%     {error, _} = close_trans(),

%%     %%-------------------------------------------------------------
%%     %% delete apps and jobs
%%     %%-------------------------------------------------------------
%%     {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
%%     ok = delete_jobs(Job),
%%     wait_expected_result(RefFinal, App),

%%     ok = delete_app(App, Handle),
%%     check_tables(Tables).


%%========================================================================
%% non_valid_mr(Config) -> ok.
%% 
%% \@doc 
%% try to create a MR using an invalid MT
%% \@end
%%========================================================================
non_valid_mr(_Config) ->
    Tables = tables(),

    App = ?SUNE,
    Job = "non_valid_mr",

    {ok, Handle} = create_app(App),

    {ok, RefSubscr} = expected(App, Handle, [no_subscribe_request]),
    ok = create_job(Job, [{Job ++ "_mr1", "Group1", "TypeNV1"}]),
    wait_expected_result(RefSubscr, App),

    {ok, RefFinal} = expected(App, Handle, [no_subscribe_request]),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).

    
%%========================================================================
%% valid_and_non_valid_mr(Config) -> ok.
%% 
%% @doc 
%% try to create a MR using an invalid MT
%% @end
%%========================================================================
valid_and_non_valid_mr(_Config) ->
    Tables = tables(),

    App = ?SUNE,
    Job = "valid_and_non_valid_mr",

    {ok, Handle} = create_app(App),

    {ok, RefSubscr} = expected(App, Handle, [no_subscribe_request]),
    ok = create_job(Job, [{Job ++ "_mr1", "Group1", "Type1"},
			  {Job ++ "_mr1", "Group1", "TypeNV1"}]),
    wait_expected_result(RefSubscr, App),

    {ok, RefFinal} = expected(App, Handle, [no_subscribe_request]),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).

    
%%========================================================================
%% non_valid_grp(Config) -> ok.
%% 
%% @doc 
%% start one job with a non valid group
%% @end
%%========================================================================
non_valid_grp(_Config) ->
    Tables = tables(),

    App = ?SUNE,
    Job = "non_valid_grp",

    {ok, Handle} = create_app(App),

    {ok, RefSubscr} = expected(App, Handle, [no_subscribe_request]),
    ok = create_job(Job, [{Job ++ "_mr1", "GroupNV", "Type1"}]),
    wait_expected_result(RefSubscr, App),

    {ok, RefFinal} = expected(App, Handle, [no_subscribe_request]),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


%%========================================================================
%% valid_and_non_valid_grp(Config) -> ok.
%% 
%% @doc 
%% start one job with a valid group and a non valid group
%% @end
%%========================================================================
valid_and_non_valid_grp(_Config) ->
    Tables = tables(),

    App = ?SUNE,
    Job = "valid_and_non_valid_grp",

    {ok, Handle} = create_app(App),

    {ok, RefSubscr} = expected(App, Handle, ?EXP_SUBSCR_GROUP_1),
    ok = create_job(Job, [{Job ++ "_mr1", "GroupNV", "TypeNV1"},
			  {Job ++ "_mr1", "Group1",  "Type1"}]),
    wait_expected_result(RefSubscr, App),

    {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
    ok = delete_jobs(Job),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    check_tables(Tables).


    
%%========================================================================
%% multi_trans(Config) -> ok.
%% 
%% \@doc 
%% start many transactions
%% \@end
%%========================================================================
%% multi_trans(_Config) ->
%%     %%-------------------------------------------------------------
%%     %% create apps and jobs
%%     %%-------------------------------------------------------------
%%     Tables = tables(),

%%     App = ?SUNE,
%%     {ok, Handle} = create_app(App),

%%     Jobs = ["mt1", "mt2", "mt3"],
%%     mt_create(App, Handle, Jobs),

%%     %%-------------------------------------------------------------
%%     %% delete apps and jobs
%%     %%-------------------------------------------------------------
%%     mt_delete(App, Handle, Jobs),

%%     ok = delete_app(App, Handle),
%%     check_tables(Tables).


 
%% mt_create(App, Handle, Jobs) ->   
%%     [mtc(App, Handle, J) || J <- Jobs].
   
%% mtc(App, Handle, Job) ->   
%%     {ok, RefSubscr} = expected(App, Handle, ?EXP_SUBSCR_GROUP_1),
%%     ok = create_job(Job, ?JOB_GROUP_1),
%%     wait_expected_result(RefSubscr, App).
    
%% mt_delete(App, Handle, Jobs) ->   
%%     [mtd(App, Handle, J) || J <- Jobs].
   
%% mtd(App, Handle, Job) ->   
%%     {ok, RefFinal} = expected(App, Handle, ?EXP_FINAL),
%%     {ok, _} = open_trans(),
%%     ok = delete_job(Job),
%%     wait_expected_result(RefFinal, App),
%%     ok = close_trans().

%%========================================================================
%% create_del_trans(Config) -> ok.
%% 
%% @doc 
%% create and delete jobs in the same transaction
%% @end
%%========================================================================
create_del_trans(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App  = ?SUNE,
    Job1 = "create_del_trans1",
    Job2 = "create_del_trans2",
    GP   = ?GP_10_SEC,
    
    MTs = [{?Group1, [?Type1]}],
    
    %%-------------------------------------------------------------
    %% create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    {ok, LDN}    = rpc(gmfI, check_string, ?LDN_DEF_1(App)),
    AppData      = {App, Handle, GP},

    %%-------------------------------------------------------------
    %% Create first job
    %%-------------------------------------------------------------
    ok = create_job(Job1, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% Wait for ROP (first GP does not generate a ROP file)
    %%-------------------------------------------------------------
    Values = get_values(MTs, LDN, [{7755}]),
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    ok = wait_report_rop(Values, ?FF_TRUE, AppData),    
    
    %%-------------------------------------------------------------
    %% delete old job and create a new in the same transaction
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr(Job2, [{Job2, "Group1", "Type1"}]),
    ok = delete_job(Job1),
    to(),
    ok = close_trans(),

    ok = clt(2, {GP, MTs}, {Values, ?FF_TRUE, AppData}),
    
    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job2),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).



clt(N, _, _) when N < 0 ->
    ok;
clt(N, {GP, MTs}, {Values, FF, AppData}) ->
    %% It depends in which order the create and delete mnesia events
    %% are received in pmServer if there should be
    %% - subscribe with no counters followed by a subscribe with new job's counter
    %% - or, no subscribes and only report rop data requests.
    receive
	{copy, _, {pmi2SubscribeRop, {X, []}}} ->
	    ct:pal("#### clt subscribe ~p~n", [X]),
	    ok = wait_until_subscribe(get_subscribe_data(GP, MTs));
	{copy, _, {pmi2ReportRop, {Values, FF, AppData} = Data}} ->
	    ct:pal("#### clt report Data ~p~n", [Data]),
	    ok;
	{copy, _, Unexpected} ->
	    ct:pal("wait subscribe: Unexpected ~p~n", [Unexpected]),
	    clt(N - 1, {GP, MTs}, {Values, FF, AppData})
    after 10000 ->
	    {error, no_subscribe_received}
    end.




%%========================================================================
%% non_legacy_rp(Config) -> ok.
%% 
%% @doc 
%% change RP mode to legacy and try to start a job with non legacy RP
%% @end
%%========================================================================
non_legacy_rp(Config) ->

    RP = proplists:get_value(reporting_period, Config),

    rpc(pmsDb, pms_env_set, [reporting_period, legacy]),
    ct:pal("Reporting period mode = ~p~n", [legacy]),
    rpc(pmsI, rp_legacy, [[]]),

    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    Tables = tables(),

    App = ?SUNE,
    Job = "non_legacy_rp",

    {ok, Handle} = create_app(App),

    %%-------------------------------------------------------------
    %% Non legacy RP
    %%-------------------------------------------------------------
    Attrs = [{"granularityPeriod", ?TEN_SECONDS},
 	     {"reportingPeriod",   ?TEN_SECONDS},
 	     {"currentJobState",   ?ACTIVE}],
    {error, _} = create_job(Job, ?JOB_GROUP_1, Attrs),

    ok = delete_app(App, Handle),
    check_tables(Tables),

    %%-------------------------------------------------------------
    %% reset to the old mode
    %%-------------------------------------------------------------
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    case RP of
	legacy ->
	    ok;
	_ ->
	    rpc(pmsI, rp_ecim, [[]])
    end.


%%========================================================================
%% no_mr(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
no_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "no_mr",

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    
    %%-------------------------------------------------------------
    %% Start job whithout MR and wait for subscribe
    %%-------------------------------------------------------------
    {error, _} = create_job(Job, ?JOB_NO_MR),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


%%========================================================================
%% no_sr(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
no_sr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "add_mr",

    %%-------------------------------------------------------------
    %% Create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),
    
    %%-------------------------------------------------------------
    %% Start job whithout MR and wait for subscribe
    %%-------------------------------------------------------------
    {error, _} = create_job(Job, ?JOB_NO_SR),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).



%%========================================================================
%% multi_mr(Config) -> ok.
%% 
%% @doc 
%% create job with many mr
%% @end
%%========================================================================
multi_mr(_Config) ->
    %%-------------------------------------------------------------
    %% Definitions
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "multi_mr",
    GP  = ?GP_10_SEC,
    
    MTs = [{?Group1, [?Type1]}],
    
    %%-------------------------------------------------------------
    %% create app
    %%-------------------------------------------------------------
    {ok, Handle} = create_app({App, [{expected, [relay]}]}),

    %%-------------------------------------------------------------
    %% Create first job
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    ok = create_job_mr(Job, get_mrs(2000, [])),
    to(),
    ok = close_trans(),

    ok = create_job(Job, get_mrs(MTs)),
    ok = wait_subscribe(get_subscribe_data(GP, MTs)),

    %%-------------------------------------------------------------
    %% delete jobs
    %%-------------------------------------------------------------
    ok = delete_jobs(Job),
    ok = wait_until_subscribe(get_subscribe_data(GP, [])),

    %%-------------------------------------------------------------
    %% delete apps
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle).


get_mrs(N, Acc) when N < 1 ->
    Acc;
get_mrs(N, Acc) ->
    get_mrs(N - 1, [{"mr_" ++ integer_to_list(N), "Group1", "Type1"} | Acc]).    


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
get_values([{?Group1, [?Type1]}],
	   LDN,
	   [{Val}]) -> 
    Vals = [{?Group1, [{LDN, [{?Type1, [Val]}]}]}],
    aliasify_values(Vals).



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
%% create_app(A, B)    -> ?LIB:create_app(A, B).
delete_app(A, B)    -> ?LIB:delete_app(A, B).
create_job(A, B)    -> ?LIB:create_job(A, B).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
%%create_jobs(A)      -> ?LIB:create_jobs(A).
delete_jobs(A)      -> ?LIB:delete_jobs(A).
%%update_job(A, B)    -> ?LIB:update_job(A, B).
delete_job(A)       -> ?LIB:delete_job(A).

create_job_mr(A, B) -> ?LIB:create_job_mr(A, B, ?DEF_JOB_ATTRS).
%%delete_mr(A, B)       -> ?LIB:delete_mr(A, B).

%%get_table(A)       -> ?LIB:get_table(A).
tables()           -> ?LIB:tables().
check_tables(A)    -> ?LIB:check_tables(A).
%%check_processes(A) -> ?LIB:check_processes(A).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

expected(A, B, C)               -> ?LIB:expected(A, B, C).
%%wait_expected_result(A)         -> ?LIB:wait_expected_result(A).
wait_expected_result(A,B)       -> ?LIB:wait_expected_result(A, B).
%%wait_expected_error_result(A,B) -> ?LIB:wait_expected_error_result(A, B).
%%wait_table_size(A, B)           -> ?LIB:wait_table_size(A, B).
    
host()  -> ?LIB:host().
hooks() -> ?LIB:hooks().

open_trans()  -> ?LIB:open_trans().
close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

log_rop_files(Config) -> 
    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
    log_rop_files(RopData, Config).

log_rop_files(RopData, Config) -> pms_test_lib:log_rop_files(RopData, Config).



%%check_rop_file(A, B)     -> ?LIB:check_rop_file(A, B).
%%check_rop_file(A, B, C)  -> ?LIB:check_rop_file(A, B, C).
%%wait_one_gp(A)           -> ?LIB:wait_one_gp(A).
%%get_noof_rop_files()     -> ?LIB:get_noof_rop_files().
wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
wait_until_subscribe(A)  -> ?LIB:wait_until_subscribe(A).
wait_report_rop(A, B, C) -> ?LIB:wait_report_rop(A, B, C).
get_subscribe_data(A, B) -> ?LIB:get_subscribe_data(A, B).
get_mrs(A)               -> ?LIB:get_mrs(A).
aliasify_values(A)       -> ?LIB:aliasify_values(A).
    

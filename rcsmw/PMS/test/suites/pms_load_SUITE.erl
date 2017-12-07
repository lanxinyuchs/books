%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_load_SUITE.erl %
%%% @version /main/R2A/R4A/R11A/1

%%% @doc 
%%% == Test suite for PM ROP with large number of counters ==
%%% This suite tests the ability of PMS to handle thousands of counters. 
%%% @end
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%% R2A/1      2014-06-18 eolaand     Created
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% ----------------------------------------------------------
%%% 

-module(pms_load_SUITE).

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

-include_lib("common_test/include/ct.hrl").

-define(ROP_DIR, "rop").

-define(KENTA,   kenta).
-define(NACKA,  nacka).
-define(RONNIE,  ronnie).
-define(BILLY, billy).
-define(KLASSE, klasse).
-define(TOM, tom).
-define(HINKEN, hinken).
-define(WERNER, werner).
-define(KENNEDY, kennedy).
-define(MAX, max).

-define(LDN1, "ME=1,App=kenta").
-define(LDN2, "ME=1,App=nacka").
-define(LDN3, "ME=1,App=ronnie").

-define(JOB1, "job1").
-define(JOB2, "job2").

-define(MR1_1, "job1_reader1").
-define(MR1_2, "job1_reader2").
-define(MR1_3, "job1_reader3").
-define(MR1_4, "job1_reader4").
-define(MR1_5, "job1_reader5").

-define(MR2_1, "job2_reader1").
-define(MR2_2, "job2_reader2").
-define(MR2_3, "job2_reader3").
-define(MR2_4, "job2_reader4").
-define(MR2_5, "job2_reader5").

-define(ACTIVE, 1).
-define(STOPPED, 2).

-define(TEN_SECONDS, 1).
-define(THIRTY_SECONDS, 2).
-define(ONE_MINUTE, 3).
-define(FIFTEEN_MINUTES, 5).

-define(DEFAULT_JOB_ATTR, [{"granularityPeriod", ?TEN_SECONDS},
			   {"reportingPeriod",   ?TEN_SECONDS},
			   {"currentJobState",   ?ACTIVE}]).
-define(JOB_ATTR_RP30, [{"granularityPeriod", ?TEN_SECONDS},
			{"reportingPeriod",   ?THIRTY_SECONDS},
			{"currentJobState",   ?ACTIVE}]).

-define(JOB_ATTR_RP15M, [{"granularityPeriod", ?FIFTEEN_MINUTES},
			 {"reportingPeriod",   ?FIFTEEN_MINUTES},
			 {"currentJobState",   ?ACTIVE}]).

-define(JOB_ATTR_RP1M, [{"granularityPeriod", ?ONE_MINUTE},
			{"reportingPeriod",   ?ONE_MINUTE},
			{"currentJobState",   ?ACTIVE}]).

-define(TEST_APPS,  [?KENTA]).
-define(TEST_APPS2, [?KENTA, ?NACKA, ?RONNIE]).
-define(TEST_APPS_10, [?KENTA, ?NACKA, ?RONNIE, ?BILLY, ?KLASSE, ?TOM, 
		       ?HINKEN, ?WERNER, ?KENNEDY, ?MAX]).


-define(BGROUP1, "Group1K1").
-define(BGROUP2, "Group10K1").
-define(GROUP1, "Group1").
-define(GROUP2, "Group2").

-define(MTYPE1, "Type1").
-define(MTYPE2, "Type2").
-define(MTYPE3, "Type3").

-define(MEAS_VAL_MTYPE1(N), [{?MTYPE1, N}]).
-define(MEAS_VAL_MTYPE2(N), [{?MTYPE2, N}]).
-define(MEAS_VAL_MTYPE3(N), [{?MTYPE3, N}]).


-define(ROPGroup1, "ROPGroup1").
-define(ROPGroup2, "ROPGroup2").

-define(ROPType1Sum, "ROPType1Sum").
-define(ROPType2Avg, "ROPType2Avg").
-define(ROPType3Min, "ROPType3Min").
-define(ROPType4Max, "ROPType4Max").
-define(ROPType5LU, "ROPType5LU").

-define(ROPType6Sum, "ROPType6Sum").
-define(ROPType7Avg, "ROPType7Avg").
-define(ROPType8Min, "ROPType8Min").
-define(ROPType9Max, "ROPType9Max").
-define(ROPType10LU, "ROPType10LU").

-define(MEAS_VAL_SUM(N), [{?ROPType1Sum, N}]).
-define(MEAS_VAL_AVG(N), [{?ROPType2Avg, N}]).
-define(MEAS_VAL_MIN(N), [{?ROPType3Min, N}]).
-define(MEAS_VAL_MAX(N), [{?ROPType4Max, N}]).
-define(MEAS_VAL_LU(N), [{?ROPType5LU, N}]).
-define(MEAS_VALS_ALL(N1, N2, N3, N4, N5), 
	lists:append([?MEAS_VAL_SUM(N1),
		      ?MEAS_VAL_AVG(N2),
		      ?MEAS_VAL_MAX(N3),
		      ?MEAS_VAL_MIN(N4),
		      ?MEAS_VAL_LU(N5)])).


-define(MEAS_VAL_SUM_2(N), [{?ROPType6Sum, N}]).
-define(MEAS_VAL_AVG_2(N), [{?ROPType7Avg, N}]).
-define(MEAS_VAL_MIN_2(N), [{?ROPType8Min, N}]).
-define(MEAS_VAL_MAX_2(N), [{?ROPType9Max, N}]).
-define(MEAS_VAL_LU_2(N), [{?ROPType10LU, N}]).
-define(MEAS_VALS_ALL_2(N1, N2, N3, N4, N5), 
	lists:append([?MEAS_VAL_SUM_2(N1),
		      ?MEAS_VAL_AVG_2(N2),
		      ?MEAS_VAL_MAX_2(N3),
		      ?MEAS_VAL_MIN_2(N4),
		      ?MEAS_VAL_LU_2(N5)])).


-define(REP_PERIOD1, 10000).
-define(REP_PERIOD_1MIN, 60000).
-define(REP_PERIOD_15M, 15*60000).
-define(LIB, pms_test_lib).
-define(L2B(__L), list_to_binary(__L)).
-define(B2L(__B), binary_to_list(__B)).

%% @hidden
suite() ->
    Hooks = hooks(),
    Hooks ++ [{timetrap,{minutes,60}}].

%% @hidden
init_per_suite(Config) ->
    ?LIB:set_ct_log(false),
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    [{reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    delete_all_rop_files(),
    ok.

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    delete_all_rop_files(),
    cleanup(),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    log_rop_files(Config),
    cleanup(),
    close_trans(),
    ok.

%% @hidden
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



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     tc_1job_1app_1k,
     tc_1job_3app_1k,
     tc_1job_10app_1k,
     tc_1job_10app_1k_1MRP,
     tc_1job_1app_10k,
     tc_1job_1app_10k_1MRP
    ].


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one PM group containing 1K counters
%% and verify via sftp that a ROP file is generated with no suspect marking.
%% @end
tc_1job_1app_1k(_Config) -> 
    pms_erl_app:start(?KENTA),
    initialize(?KENTA, [?BGROUP1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?BGROUP1}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(2*?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up([?JOB1], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up([?JOB1], [?KENTA]).

%%--------------------------------------------------------------------
%% @doc
%% Start three applications and a PM job with 1K counters and verify via sftp 
%% that a ROP file is generated with no suspect marking.
%% @end
tc_1job_3app_1k(_Config) ->
    lists:foreach(fun(Name) ->
			  pms_erl_app:start(Name),
			  initialize(Name, [?BGROUP1])
		  end, ?TEST_APPS2),
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?BGROUP1}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(2*?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up([?JOB1], ?TEST_APPS2),
	    ct:fail(Error)
    end,
    clean_up([?JOB1], ?TEST_APPS2).

%%--------------------------------------------------------------------
%% @doc
%% Start ten applications and a PM job with 1K counters and verify via sftp 
%% that a ROP file is generated with no suspect marking.
%% @end
tc_1job_10app_1k(_Config) ->
    lists:foreach(fun(Name) ->
			  pms_erl_app:start(Name),
			  initialize(Name, [?BGROUP1])
		  end, ?TEST_APPS_10),
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?BGROUP1}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up([?JOB1], ?TEST_APPS_10),
	    ct:fail(Error)
    end,
    clean_up([?JOB1], ?TEST_APPS_10).

%%%--------------------------------------------------------------------
%% @doc
%% Start ten applications and a PM job with RP 1 minute and 1K counters.
%% Verify via sftp that a ROP file is generated with no suspect marking.
%% @end
tc_1job_10app_1k_1MRP(_Config) ->
    lists:foreach(fun(Name) ->
			  pms_erl_app:start(Name),
			  initialize(Name, [?BGROUP1])
		  end, ?TEST_APPS_10),
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, ?JOB_ATTR_RP1M, {?MR1_1, ?BGROUP1}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD_1MIN),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up([?JOB1], ?TEST_APPS_10),
	    ct:fail(Error)
    end,
    clean_up([?JOB1], ?TEST_APPS_10).

%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one PM group containing 10K counters
%% and verify via sftp that a ROP file is generated with no suspect marking.
%% @end
tc_1job_1app_10k(_Config) -> 
    pms_erl_app:start(?KENTA),
    initialize(?KENTA, [?BGROUP2]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?BGROUP2}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up([?JOB1], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up([?JOB1], [?KENTA]).

%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with RP 1 minute and 10K counters.
%% Verify via sftp that a ROP file is generated with no suspect marking.
%% @end
tc_1job_1app_10k_1MRP(_Config) -> 
    pms_erl_app:start(?KENTA),
    initialize(?KENTA, [?BGROUP2]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, ?JOB_ATTR_RP1M, {?MR1_1, ?BGROUP2}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD_1MIN),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up([?JOB1], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up([?JOB1], [?KENTA]).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
check_rop_cont(ROP, ERegExps) ->
    check_rop_cont(ROP, ERegExps, ["suspect"]).

check_rop_cont(ROP, ERegExps, UERegExps) ->
    EF = fun(ERegExp) ->
		 case re:run(ROP, ERegExp) of
		     {match, _} ->
			 ok;
		     _NoMatch ->
			 ct:print("Failed to match ~s in ROP file", 
				  [ERegExp]),
			 throw({"Failed to match in ROP file:", ERegExp})
		 end
	 end,
    lists:foreach(EF, ERegExps),	 
    UEF = fun(UERegExp) ->
		  case re:run(ROP, UERegExp) of
		      nomatch ->
			  ok;
		      _Match ->
			  ct:print("Unexpected match in ROP file: ~s", 
				 [UERegExp]),
			  throw({"Unexpected match in ROP file:", UERegExp})
		  end
	  end,
    lists:foreach(UEF, UERegExps).	 


get_first_rop_file(RP) ->
    get_rop_file_n(1, RP).


get_second_rop_file(RP) ->
    get_rop_file_n(2, RP).

    
get_rop_file_n(N, RP) ->
    {ok, RopFile} = wait_rop_file(N, (N + 2)*RP + RP div 2),
    Data = get_rop_data(RopFile),
    %% ct:print("ROP Data:~n~s~n", [?B2L(Data)]),
    Data.
    

wait_rop_file(_N, Timeout) when Timeout =< 0 ->
    {error, timeout};

wait_rop_file(N, Timeout) ->
%%    case rpc(pmsDb, rop_files_list, []) of
    case rct_sftp_client:list_dir(?ROP_DIR) of
	{ok, Files} when length(Files) < N ->
	    timer:sleep(1000),
	    wait_rop_file(N, Timeout - 1000);
	{ok, Files} ->
	    {ok, lists:nth(N, Files)}
    end.


get_rop_data(Name) ->
    {ok, ZipData} = rct_sftp_client:read_file(filename:join(?ROP_DIR, Name)),
    zlib:gunzip(ZipData).


list_rop_files() ->
    rct_sftp_client:list_dir(?ROP_DIR).
    

delete_rop_file(File) ->
    FileName = filename:join(?ROP_DIR, File),
    rct_sftp_client:delete_file(FileName).


delete_all_rop_files() ->
    rct_sftp_client:read_files(?ROP_DIR),
    rct_sftp_client:delete_files(?ROP_DIR).
    

delete_n_rop_files(N) ->
    {ok, Files} = rct_sftp_client:list_dir(?ROP_DIR),
    delete_n_rop_files(Files, N).


delete_n_rop_files([File | Files], N) when N > 0 ->
    FileName = filename:join(?ROP_DIR, File),
    rct_sftp_client:read_file(FileName),
    ok = rct_sftp_client:delete_file(FileName),
    delete_n_rop_files(Files, N - 1);

delete_n_rop_files(Files, N) when Files =:= []; N =:= 0 ->
    ok.
    

clean_up(Jobs, Apps) ->
    open_trans(),
    [delete_job(Job) || Job <- Jobs],
    close_trans(),
    timer:sleep(1000),
    [begin finalize(App), pms_erl_app:stop(App) end || App <- Apps],
    ok.
    

rpc(M, F, A) -> ?LIB:rpc(M, F, A).

initialize(A)      -> ?LIB:initialize(A).
initialize(A, B)   -> ?LIB:initialize(A, B).
finalize(A)        -> ?LIB:finalize(A).
create_job(A)    -> ?LIB:create_job(A, ?DEFAULT_JOB_ATTR).
create_job(A, B) -> ?LIB:create_job(A, B).
update_job(A, B)    -> ?LIB:update_job(A, B).
%%update_job(A, B, C) -> ?LIB:update_job(A, B, C).
delete_job(A)    -> ?LIB:delete_job(A).
%%delete_job(A, B) -> ?LIB:delete_job(A, B).
create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).
delete_mr(A, B) -> ?LIB:delete_mr(A, B).
create_job_mr(A, Attr, B) -> ?LIB:create_job_mr({A, Attr}, B).
create_job_mr(A, B) -> ?LIB:create_job_mr({A, ?DEFAULT_JOB_ATTR}, B).

cleanup()          -> ?LIB:cleanup().

%%rpc(M, F, A) -> ?LIB:rpc(M, F, A).
host()   -> ?LIB:host().
%%module() -> ?LIB:module().
%%hooks()  -> [sftp_hook() | ?LIB:hooks()].
hooks()  -> case ?LIB:hooks() of
		[{ct_hooks, Hooks}] ->
		    [{ct_hooks, [sftp_hook() | Hooks]}];
		_ ->
		    [{ct_hooks, [sftp_hook()]}]
	    end.

sftp_hook() ->
   case host() of
       testnode -> 
	   {rct_sftp_client, []};
       _PmsSim ->
	   {rct_sftp_client, [{port, 8899}]}
   end.

open_trans()  -> ?LIB:open_trans().
close_trans() -> ?LIB:close_trans().


log_rop_files(Config) ->
    case rct_sftp_client:read_files(?ROP_DIR) of
	{ok, RopData} ->
	    log_rop_files(RopData, Config);
	Error ->
	    ct:log(lightred, "Failed to read ROP files: ~p", [Error])
    end.

log_rop_files(RopData, Config) ->
    ?LIB:log_rop_files(RopData, Config).


log_rop_file(FileName, Data, Config) ->
    log_rop_files([{FileName, Data}], Config).

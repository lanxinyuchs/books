%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_rop_ext_SUITE.erl %
%%% @version /main/R2A/R3A/R4A/R11A/1

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
%%% R2A/1      2013-03-15 eolaand     Created
%%% R2A/14     2013-09-03 etxkols     Added dbg and rct_logging
%%% R2A/19     2013-10-24 eolaand     Removed dbg
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% ----------------------------------------------------------
%%% 

-module(pms_rop_ext_SUITE).

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

-export([tc_1job_1app_4rop/1,
	 tc_1job_1app_gp10_rp30/1,
	 tc_1job_1app_gp30_rp30/1,
	 tc_1job_1app_stop/1,
	 tc_1job_1app_stopped/1,
	 tc_2job_1app/1,
	 tc_2job_gp10_gp30_1app/1,
	 tc_2job_del_create_1/1,
	 one_job_one_mr_active_stop_active/1,
	 tc_2mr_to_same_mt/1,
	 tc_2mr_to_same_grp_and_mt/1,
	 tc_1job_1app_comp_pdf_nosusp/1,
	 tc_1job_1app_pdf_too_long_susp/1,
	 tc_1job_1app_2inst_comp_pdf_susp/1,
	 tc_4mr_to_one_grp_and_3mt_from_other/1,
	 tc_3job_1grp_each/1,
	 tc_1job_3mt_1app_2susp_vals/1,
	 tc_1job_3mt_1app_all_susp_vals/1,
	 tc_1job_3mt_1app_mix_susp_vals/1
	]).

-compile([nowarn_export_all, export_all]).

-include_lib("common_test/include/ct.hrl").

-define(NACKA,   nacka).
-define(RONNIE,  ronnie).
-define(KENTA,  kenta).

-define(LDN1, "ME=1,App=nacka").
-define(LDN2, "ME=1,App=ronnie").
-define(LDN3, "ME=1,App=kenta").

-define(JOB1, "job1").
-define(JOB2, "job2").
-define(JOB3, "job3").

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

-define(MR3_1, "job3_reader1").
-define(MR3_2, "job3_reader2").
-define(MR3_3, "job3_reader3").
-define(MR3_4, "job3_reader4").
-define(MR3_5, "job3_reader5").

-define(ACTIVE, 1).
-define(STOPPED, 2).

-define(TEN_SECONDS, 1).
-define(THIRTY_SECONDS, 2).

-define(JOB_ATTR_GP10_RP10, [{"granularityPeriod", ?TEN_SECONDS},
			     {"reportingPeriod",   ?TEN_SECONDS},
			     {"currentJobState",   ?ACTIVE}]).
-define(JOB_ATTR_GP10_RP30, [{"granularityPeriod", ?TEN_SECONDS},
			     {"reportingPeriod",   ?THIRTY_SECONDS},
			     {"currentJobState",   ?ACTIVE}]).
-define(JOB_ATTR_GP30_RP30, [{"granularityPeriod", ?THIRTY_SECONDS},
			     {"reportingPeriod",   ?THIRTY_SECONDS},
			     {"currentJobState",   ?ACTIVE}]).

-define(TEST_APPS,  [?NACKA]).
-define(TEST_APPS2, [?NACKA, ?RONNIE, ?KENTA]).

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
-define(ROPGroupMult, "ROPGroupMult").
-define(ROPGroupPDFComp, "ROPGroupPDFComp").
-define(ROPGroupNonExist, "ROPGroupNonExist").

-define(ROPType1Sum, "ROPType1Sum").
-define(ROPType2Avg, "ROPType2Avg").
-define(ROPType3Min, "ROPType3Min").
-define(ROPType4Max, "ROPType4Max").
-define(ROPType5LU,  "ROPType5LU").

-define(ROPType6Sum, "ROPType6Sum").
-define(ROPType7Avg, "ROPType7Avg").
-define(ROPType8Min, "ROPType8Min").
-define(ROPType9Max, "ROPType9Max").
-define(ROPType10LU, "ROPType10LU").

-define(ROPType11SumMult, "ROPType11SumMult").
-define(ROPType12AvgMult, "ROPType12AvgMult").
-define(ROPType13MinMult, "ROPType13MinMult").
-define(ROPType14MaxMult, "ROPType14MaxMult").
-define(ROPType15LUMult, "ROPType15LUMult").

-define(ROPType16SumPDFComp, "ROPType16SumPDFComp").
-define(ROPType17AvgPDFComp, "ROPType17AvgPDFComp").
-define(ROPType18MinPDFComp, "ROPType18MinPDFComp").
-define(ROPType19MaxPDFComp, "ROPType19MaxPDFComp").
-define(ROPType20LUPDFComp, "ROPType20LUPDFComp").

-define(ROPTypeNonExist, "ROPTypeNonExist").

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


-define(MEAS_VAL_SUM_MULT(N), [{?ROPType11SumMult, N}]).
-define(MEAS_VAL_AVG_MULT(N), [{?ROPType12AvgMult, N}]).
-define(MEAS_VAL_MIN_MULT(N), [{?ROPType13MinMult, N}]).
-define(MEAS_VAL_MAX_MULT(N), [{?ROPType14MaxMult, N}]).
-define(MEAS_VAL_LU_MULT(N), [{?ROPType15LUMult, N}]).
-define(MEAS_VALS_ALL_MULT(N1, N2, N3, N4, N5), 
	lists:append([?MEAS_VAL_SUM_MULT(N1),
		      ?MEAS_VAL_AVG_MULT(N2),
		      ?MEAS_VAL_MIN_MULT(N3),
		      ?MEAS_VAL_MAX_MULT(N4),
		      ?MEAS_VAL_LU_MULT(N5)])).


-define(MEAS_VAL_SUM_PDF_COMP(N), [{?ROPType16SumPDFComp, N}]).
-define(MEAS_VAL_AVG_PDF_COMP(N), [{?ROPType17AvgPDFComp, N}]).
-define(MEAS_VAL_MIN_PDF_COMP(N), [{?ROPType18MinPDFComp, N}]).
-define(MEAS_VAL_MAX_PDF_COMP(N), [{?ROPType19MaxPDFComp, N}]).
-define(MEAS_VAL_LU_PDF_COMP(N), [{?ROPType20LUPDFComp, N}]).
-define(MEAS_VALS_ALL_PDF_COMP(N1, N2, N3, N4, N5), 
	lists:append([?MEAS_VAL_SUM_PDF_COMP(N1),
		      ?MEAS_VAL_AVG_PDF_COMP(N2),
		      ?MEAS_VAL_MIN_PDF_COMP(N3),
		      ?MEAS_VAL_MAX_PDF_COMP(N4),
		      ?MEAS_VAL_LU_PDF_COMP(N5)])).


-define(RP_10_SEC, 10).

-define(RP_10, 10000).
-define(RP_30, 30000).
-define(REP_PERIOD1, ?RP_10).
-define(REP_PERIOD2, ?RP_30).
-define(LIB, pms_test_lib).
-define(L2B(__L), list_to_binary(__L)).
-define(B2L(__B), binary_to_list(__B)).

-define(CLEAN_UP_AND_FAIL(Error, Jobs, Apps, RP),
	ct:log(lightred, "Test case failed: ~p.~nStacktrace: ~p", 
	       [Error, erlang:get_stacktrace()]),
	clean_up(Jobs, Apps, RP),
	ct:fail(Error)).

-define(CLEAN_UP_AND_FAIL(Error, Jobs, Apps),
	?CLEAN_UP_AND_FAIL(Error, Jobs, Apps, ?RP_10)).


%% @hidden
suite() ->
    Hooks = hooks(),
    Hooks ++ [{timetrap,{minutes,5}}].

%% @hidden
init_per_suite(Config) ->
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    %%log_msg(),
    %% ok = pms_test_lib:appdata("pms_rop_app.xml", "pms_rop_SUITE", "R1A"),
    [{reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    %%rpc(pmsDebug, stop_clear, []),
    rpc(pmsDb, rop_file_delete_all, []), 
    ok.

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    rpc(pmsDb, rop_file_delete_all, []), 
    %% ?LIB:clear_te_log(),
    cleanup(),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    log_rop_files(Config),
    cleanup(),
    %% ?LIB:log_te_log(TestCase, Config),
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
     tc_1job_1app_4rop,
     tc_1job_1app_gp10_rp30,
     tc_1job_1app_gp30_rp30,
     tc_1job_1app_stop,
     tc_1job_1app_stopped,
     tc_2job_1app,
     tc_2job_gp10_gp30_1app,
     tc_2job_del_create_1,
     one_job_one_mr_active_stop_active,
     tc_2mr_to_same_mt,
     tc_2mr_to_same_grp_and_mt,
     %% tc_1job_1app_comp_pdf_nosusp,
     tc_1job_1app_pdf_too_long_susp,
     tc_1job_1app_2inst_comp_pdf_susp,
     tc_4mr_to_one_grp_and_3mt_from_other,
     tc_3job_1grp_each,
     tc_1job_3mt_1app_2susp_vals,
     tc_1job_3mt_1app_all_susp_vals,
     tc_1job_3mt_1app_mix_susp_vals
    ].



%% @doc
%% Start one application and a PM job with one counter and verify that 4 ROP 
%% files are generated.
%% @end
tc_1job_1app_4rop(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_rop_file_n(4, ?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and a PM job with GP10 and RP30 and verify
%% that a ROP file is generated with no suspect marking. 
%% @end
tc_1job_1app_gp10_rp30(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, ?JOB_ATTR_GP10_RP30, 
		       {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_second_rop_file(?REP_PERIOD2),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?KENTA], ?REP_PERIOD2)
    end,
    clean_up([?JOB1], [?KENTA], ?REP_PERIOD2).


%% @doc
%% Start one application and a PM job with GP 30 and RP30 and verify
%% that a ROP file is generated with no suspect marking. 
%% @end
tc_1job_1app_gp30_rp30(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, ?JOB_ATTR_GP30_RP30, 
		       {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD2),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?KENTA], ?REP_PERIOD2)
    end,
    clean_up([?JOB1], [?KENTA], ?REP_PERIOD2).


%% @doc
%% Start one application and a PM job with one counter. 
%% Stop the job and verify that the job is not in the ROP file
%% @end
tc_1job_1app_stop(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
				?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    
    get_first_rop_file(?REP_PERIOD1),
    
    {ok, _} = open_trans(),
    update_job(?JOB1, [{"requestedJobState", ?STOPPED}]),
    ok = close_trans(),
    
    try
	Timeout = get_rop_wait_time(?REP_PERIOD1, 2),
	{error, timeout} = wait_rop_file(3, Timeout)
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).

%% @doc
%% Start one application and a PM job with one counter.
%% The job is started with requested job state = stopped 
%% Verify that tbe job is not in the ROP file
%% @end
tc_1job_1app_stopped(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
				?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),    
    Attrs = [{"granularityPeriod", ?TEN_SECONDS},
	     {"reportingPeriod",   ?THIRTY_SECONDS},
	     {"requestedJobState", ?STOPPED}],
    
    ok = create_job_mr(?JOB1, Attrs, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
        
    try
	Timeout = get_rop_wait_time(?REP_PERIOD1, 2),
	{error, timeout} = wait_rop_file(2, Timeout)
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and 2 PM jobs and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
tc_2job_1app(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM([1]) ++ ?MEAS_VAL_AVG([2])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = create_job_mr(?JOB2, {?MR2_1, ?ROPGroup1, ?ROPType2Avg}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, [?JOB1,?JOB2])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1, ?JOB2], [?NACKA])
    end,
    clean_up([?JOB1, ?JOB2], [?NACKA]).


%% @doc
%% Start one application and 2 PM jobs with different GP and verify that a ROP 
%% file is generated with no suspect marking.
%% @end
tc_2job_gp10_gp30_1app(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM([1]) ++ ?MEAS_VAL_AVG([2])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB2,  ?JOB_ATTR_GP30_RP30, 
		       {?MR2_1, ?ROPGroup1, ?ROPType2Avg}),
    ok = create_job_mr(?JOB1, ?JOB_ATTR_GP10_RP30, 
		       {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD2),
	ok = check_rop_cont(Data, [?JOB1,?JOB2])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1, ?JOB2], [?NACKA], ?REP_PERIOD2)
    end,
    clean_up([?JOB1, ?JOB2], [?NACKA], ?REP_PERIOD2).


%% @doc
%% Start one application and 2 PM jobs and verify that a ROP file is generated 
%% with both jobs present.
%% Delete one job and verify that the job is missing from the next ROP file.
%% Create the job again and verify that it is back again in next ROP file.
%% @end
tc_2job_del_create_1(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM([1]) ++ ?MEAS_VAL_AVG([2])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, ?JOB_ATTR_GP30_RP30, 
		       {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = create_job_mr(?JOB2, ?JOB_ATTR_GP30_RP30,
		       {?MR2_1, ?ROPGroup1, ?ROPType2Avg}),
    ok = close_trans(),
    try
	Data1 = get_first_rop_file(?REP_PERIOD2),
	ok = check_rop_cont(Data1, [?JOB1,?JOB2])
    catch
	_:Error1 ->
	    ?CLEAN_UP_AND_FAIL(Error1, [?JOB1, ?JOB2], [?NACKA], ?REP_PERIOD2)
    end,
    {ok, _} = open_trans(),
    ok = delete_job(?JOB2),
    ok = close_trans(),
    timer:sleep(1000),
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB2, ?JOB_ATTR_GP30_RP30,
		       {?MR2_1, ?ROPGroup1, ?ROPType2Avg}),
    ok = close_trans(),
    try
	Data2 = get_rop_file_n(2, ?REP_PERIOD2),
	ok = check_rop_cont(Data2, [?JOB1],[?JOB2])
    catch
	_:Error2 ->
	    ?CLEAN_UP_AND_FAIL(Error2, [?JOB1, ?JOB2], [?NACKA], ?REP_PERIOD2)
    end,
    try
	Data3 = get_rop_file_n(3, ?REP_PERIOD2),
	ok = check_rop_cont(Data3, [?JOB1,?JOB2])
    catch
	_:Error3 ->
	    ?CLEAN_UP_AND_FAIL(Error3, [?JOB1, ?JOB2], [?NACKA], ?REP_PERIOD2)
    end,
    clean_up([?JOB1, ?JOB2], [?NACKA], ?REP_PERIOD2).



%%========================================================================
%% one_job_one_mr_active_stop_active(Config) -> ok.
%% 
%% @doc 
%% start one job and wait until it will get the second report request
%% activate, stop and reactivate the job
%% @end
%%========================================================================
one_job_one_mr_active_stop_active(_Config) ->
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 

    %%-------------------------------------------------------------
    %% create a PM Job
    %%-------------------------------------------------------------
    ct:pal("+++ creating the job~n"),
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),

    %%-------------------------------------------------------------
    %% check tables and processes
    %%-------------------------------------------------------------
    try
	Data1 = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data1, [?JOB1])
    catch
	_:Error1 ->
	    ?CLEAN_UP_AND_FAIL(Error1, [?JOB1], [?NACKA])
    end,

    %%-------------------------------------------------------------
    %% stop the job, there should not be any report requests
    %%-------------------------------------------------------------
    ct:pal("+++ stopping the job~n"),
    {ok, _} = open_trans(),
    update_job(?JOB1, [{"requestedJobState", ?STOPPED}]),
    ok = close_trans(),

    try
	Timeout = get_rop_wait_time(?REP_PERIOD1, 2),
	{error, timeout} = wait_rop_file(3, Timeout)
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,

    %%-------------------------------------------------------------
    %% activate the job again, the report request should start again
    %%-------------------------------------------------------------
    ct:pal("+++ activating the job~n"),
    rpc(pmsDb, rop_file_delete_all, []), 

    {ok, _} = open_trans(),
    update_job(?JOB1, [{"requestedJobState", ?ACTIVE}]),
    ok = close_trans(),

    try
	Data2 = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data2, [?JOB1])
    catch
	_:Error2 ->
	    ?CLEAN_UP_AND_FAIL(Error2, [?JOB1], [?NACKA])
    end,


    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    ct:pal("+++ finialize the app~n"),
    clean_up([?JOB1], [?NACKA]).
    

%% @doc
%% Start one application and a PM job with two MR pointing to the same MT.
%% ROP file is generated with one counter and with no suspect marking.
%% @end
tc_2mr_to_same_mt(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?ROPGroup1, ?ROPType1Sum},
			       {?MR1_2, ?ROPGroup1, ?ROPType1Sum}]),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and a PM job with two MR pointing to a Group and
%% to a specific MT in the same Group.
%% ROP file is generated with one counter and with no suspect marking.
%% @end
tc_2mr_to_same_grp_and_mt(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?ROPGroup1},
			       {?MR1_2, ?ROPGroup1, ?ROPType1Sum}]),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one instance of an application and a PM job with one multivalue 
%% counter with aggregation SUM and verify that the measurement is not suspect
%% marked when the length is shorter than configured (compressed PDF).
%% @end
tc_1job_1app_comp_pdf_nosusp(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_AVG_PDF_COMP([1,2])),
    initialize(?NACKA, [?ROPGroupPDFComp]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroupPDFComp, ?ROPType17AvgPDFComp}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, [?ROPType17AvgPDFComp, "1,2"])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one instance of an application and a PM job with one multivalue 
%% counter with aggregation SUM and verify that the measurement is suspect
%% marked when the length is longer than configured.
%% @end
tc_1job_1app_pdf_too_long_susp(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM_MULT([1,2,3])),
    initialize(?NACKA, [?ROPGroupMult]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroupMult, ?ROPType11SumMult}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["suspect"], [])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start two instances of an application and a PM job with one multivalue 
%% counter with aggregation SUM and verify that the measurement is suspect
%% marked when the lengths are shorter than configured (compressed PDF).
%% @end
tc_1job_1app_2inst_comp_pdf_susp(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_AVG_MULT([1,2])),
    {ok, _} = pms_erl_app:start(?RONNIE, ?LDN1, 
		      ?MEAS_VAL_AVG_MULT([1,2])),
    initialize(?NACKA, [?ROPGroupMult]), 
    initialize(?RONNIE, [?ROPGroupMult]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroupMult, ?ROPType12AvgMult}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["suspect"], [])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA, ?RONNIE])
    end,
    clean_up([?JOB1], [?NACKA, ?RONNIE]).


%% @doc
%% Start one application and a PM job with one MR pointing to a Group and
%% three MR each pointing to a specific MT in another Group.
%% ROP file is generated with all counters and with no suspect marking.
%% @end
tc_4mr_to_one_grp_and_3mt_from_other(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1, ?ROPGroup2]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?ROPGroup1},
			       {?MR1_2, ?ROPGroup2, ?ROPType6Sum},
			       {?MR1_3, ?ROPGroup2, ?ROPType7Avg},
			       {?MR1_4, ?ROPGroup2, ?ROPType8Min}]),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, [?ROPGroup1, ?ROPType6Sum, ?ROPType7Avg, 
				   ?ROPType8Min])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and three PM jobs, each with one MR pointing to a 
%% MT in a different Group. ROP file is generated with all counters and with no
%% suspect marking.
%% @end
tc_3job_1grp_each(_Config) ->
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?ROPGroup1,?ROPType1Sum}]),
    ok = create_job_mr(?JOB2, [{?MR2_1, ?ROPGroup2, ?ROPType6Sum}]),
    ok = create_job_mr(?JOB3, [{?MR3_1, ?GROUP2, ?MTYPE3}]),
    ok = close_trans(),
    {ok, _App} = pms_erl_app:start(
		   ?NACKA, 
		   [{?LDN1, [{?RP_10_SEC, [{?ROPGroup1, [?ROPType1Sum]}]}]},
		    {?LDN2, [{?RP_10_SEC, [{?ROPGroup2, [?ROPType6Sum]}]}]},
		    {?LDN3, [{?RP_10_SEC, [{?GROUP2, [?MTYPE3]}]}]}
		   ]),
    
    initialize(?NACKA, [?ROPGroup1, ?ROPGroup2, ?GROUP2]), 

    try
	Data = get_second_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, [?ROPGroup1, ?ROPGroup2, ?GROUP2])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1, ?JOB2, ?JOB3], 
			       [?NACKA])
    end,
    clean_up([?JOB1, ?JOB2, ?JOB3], [?NACKA], ?REP_PERIOD1, 1000).
    
%% @doc
%% Start one application and a PM job with three counters. Verify that
%% two values are blanked out but not suspect marked when the application 
%% reports -2 for two of the counters.
%% @end
tc_1job_3mt_1app_2susp_vals(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([-2]) ++ 
				    ?MEAS_VAL_AVG([1]) ++ 
				    ?MEAS_VAL_MIN([-2])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?ROPGroup1, ?ROPType1Sum},
			       {?MR1_2, ?ROPGroup1, ?ROPType2Avg},
			       {?MR1_3, ?ROPGroup1, ?ROPType3Min}]),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["<r p=\"1\"> </r>", 
				   "<r p=\"2\">1</r>",
				   "<r p=\"3\"> </r>"], [])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and a PM job with three counters. Verify that the 
%% values are blanked out but not suspect marked when the application reports
%% -2 for all three counters.
%% @end
tc_1job_3mt_1app_all_susp_vals(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([-2]) ++ 
				    ?MEAS_VAL_AVG([-2]) ++ 
				    ?MEAS_VAL_MIN([-2])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?ROPGroup1, ?ROPType1Sum},
			       {?MR1_2, ?ROPGroup1, ?ROPType2Avg},
			       {?MR1_3, ?ROPGroup1, ?ROPType3Min}]),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["<r p=\"1\"> </r>", 
				   "<r p=\"2\"> </r>",
				   "<r p=\"3\"> </r>"], [])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and a PM job with three counters. Verify that the 
%% values are blanked out and suspect marked when the application reports
%% -2 for two counters and -1 for one counter.
%% @end
tc_1job_3mt_1app_mix_susp_vals(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([-2]) ++ 
				    ?MEAS_VAL_AVG([-1]) ++ 
				    ?MEAS_VAL_MIN([-2])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, [{?MR1_1, ?ROPGroup1, ?ROPType1Sum},
			       {?MR1_2, ?ROPGroup1, ?ROPType2Avg},
			       {?MR1_3, ?ROPGroup1, ?ROPType3Min}]),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["<r p=\"1\"> </r>", 
				   "<r p=\"2\"> </r>",
				   "<r p=\"3\"> </r>",
				   "suspect"], [])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Create one PM Job with requested state STOPPED. Start one application and 
%% then set requested state to ACTIVE. 
%% verify that a ROP file is generated and all counters present.
%% @end
tc_1job_stopped_then_start_1app(_Config) -> 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    update_job(?JOB1, [{"requestedJobState", ?STOPPED}]),
    ok = close_trans(),
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, 
				?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    timer:sleep(1000),
    {ok, _} = open_trans(),
    update_job(?JOB1, [{"requestedJobState", ?ACTIVE}]),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, [?JOB1, ?ROPType1Sum])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,
    clean_up([?JOB1, ?JOB2], [?NACKA]).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
check_rop_cont(ROP) ->
    check_rop_cont(ROP, []).


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
    WT = get_rop_wait_time(RP, N),
    {ok, RopFile} = wait_rop_file(N, WT),
    Data = get_rop_data(RopFile),
    ct:print("ROP Data:~n~s~n", [?B2L(Data)]),
    Data.


get_rop_wait_time(?RP_10 = RP, N) ->
    (N + 2)*RP;

get_rop_wait_time(RP, N) ->
    (N + 1)*RP + round(RP/2).
    

wait_rop_file(_N, Timeout) when Timeout =< 0 ->
    {error, timeout};

wait_rop_file(N, Timeout) ->
    case rpc(pmsDb, rop_files_list, []) of
	{ok, Files} when length(Files) < N ->
	    timer:sleep(1000),
	    wait_rop_file(N, Timeout - 1000);
	{ok, Files} ->
	    {ok, lists:nth(N, Files)}
    end.


get_rop_data(Name) ->
    {ok, ZipData} = rpc(pmsDb, rop_data_get, [Name]),
    zlib:gunzip(ZipData).


clean_up(Jobs, Apps) ->
    clean_up(Jobs, Apps, ?RP_10).

clean_up(Jobs, Apps, RP) ->
    clean_up(Jobs, Apps, RP, 100).

clean_up(Jobs, Apps, RP, Sleep) ->
    ?LIB:clean_up_and_wait(Jobs, Apps, RP div 1000, Sleep).

    %% open_trans(),
    %% [delete_job(Job) || Job <- Jobs],
    %% close_trans(),
    %% timer:sleep(Sleep),
    %% [begin 
    %% 	 finalize(App),
    %% 	 timer:sleep(Sleep),
    %% 	 ok = pms_erl_app:stop(App) 
    %%  end || App <- Apps],
    %% check_processes(0, RP).
    

check_processes(N) ->
    ?LIB:check_processes(N).


check_processes(N, RP) ->
    ?LIB:check_processes(N, RP div 1000).


rpc(M, F, A) -> ?LIB:rpc(M, F, A).


trace_child(Name) ->
    case ?LIB:get_children(Name) of
	[Child] ->
	    rpc(os, cmd, ["te enable all " ++ atl(Child)]),
	    ok;
	_ ->
	    ok
    end.


initialize(A)      -> 
    ok = ?LIB:initialize(A).

initialize(A, B)   -> 
    ok = ?LIB:initialize(A, B).

finalize(A) -> 
    ok = ?LIB:finalize(A).

create_job(A)    -> ?LIB:create_job(A, ?JOB_ATTR_GP10_RP10).
create_job(A, B) -> ?LIB:create_job(A, B).
update_job(A, B)    -> ?LIB:update_job(A, B).
%%update_job(A, B, C) -> ?LIB:update_job(A, B, C).
delete_job(A)    -> ?LIB:delete_job(A).
%%delete_job(A, B) -> ?LIB:delete_job(A, B).
create_mr(A, B, C, D) -> ?LIB:create_mr(A, B, C, D).
delete_mr(A, B) -> ?LIB:delete_mr(A, B).
create_job_mr(A, Attr, B) -> ?LIB:create_job_mr({A, Attr}, B).
create_job_mr(A, B) -> ?LIB:create_job_mr({A, ?JOB_ATTR_GP10_RP10}, B).
log_msg()           -> ?LIB:log_msg().
wait_subscribe(A)        -> ?LIB:wait_subscribe(A).
wait_report(A, B)        -> ?LIB:wait_report(A, B).

cleanup()          -> ?LIB:cleanup().

%%rpc(M, F, A) -> ?LIB:rpc(M, F, A).
host()   -> ?LIB:host().
%%module() -> ?LIB:module().
add_hooks(A)  -> ?LIB:add_hooks(A).
hooks()  -> ?LIB:hooks().
open_trans()  -> ?LIB:open_trans().
close_trans() -> ?LIB:close_trans().


log_rop_files(Config) ->
    ?LIB:log_rop_files(Config).


log_rop_files(RopData, Config) ->
    ?LIB:log_rop_files(RopData, Config).


log_rop_file(FileName, Data, Config) ->
    ?LIB:log_rop_file(FileName, Data, Config).


atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) when is_list(L) ->
    L.

    

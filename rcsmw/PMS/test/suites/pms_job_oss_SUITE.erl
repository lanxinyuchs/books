%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_job_oss_SUITE.erl %
%%% @version /main/R2A/R3A/R4A/R11A/1

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
%%% R2A/1      2013-03-15 eolaand     Created
%%% R2A/14     2013-09-03 etxkols     Added dbg and rct_logging
%%% R2A/19     2013-10-24 eolaand     Removed dbg
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% R4A/2      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
%%% ----------------------------------------------------------
%%% 

-module(pms_job_oss_SUITE).

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

-export([tc_1job_illegal_pmgrp/1,
	 tc_1job_illegal_mt/1,
	 tc_1job_1illegal_grp_mt_1/1,
	 tc_1job_1illegal_grp_mt_2/1,
	 tc_1job_1illegal_grp_mt_3/1,
	 tc_1job_1illegal_grp_mt_4/1]).

-compile([nowarn_export_all, export_all]).

-include_lib("common_test/include/ct.hrl").

-define(NACKA,  nacka).
-define(RONNIE, ronnie).
-define(KENTA,  kenta).

-define(LDN1, "ME=1,App=nacka").
-define(LDN2, "ME=1,App=ronnie").
-define(LDN3, "ME=1,App=kenta").

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


-define(RP_10, 10000).
-define(RP_30, 30000).
-define(REP_PERIOD1, ?RP_10).
-define(REP_PERIOD2, ?RP_30).
-define(LIB, pms_test_lib).
-define(L2B(__L), list_to_binary(__L)).
-define(B2L(__B), binary_to_list(__B)).

-define(CLEAN_UP_AND_FAIL(Error, Jobs, Apps),
	ct:log(lightred, "Test case failed: ~p.~nStacktrace: ~p", 
	       [Error, erlang:get_stacktrace()]),
	clean_up(Jobs, Apps),
	ct:fail(Error)).


%% @hidden
suite() ->
    hooks() ++ [{timetrap,{minutes,5}}].

%% @hidden
init_per_suite(Config) ->
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, putenv, ["FAKE_VNFM", ""]);
        _ ->
            rpc(os, unsetenv, ["FAKE_VNFM"])
    end,
    %%log_msg(),
    %% ok = pms_test_lib:appdata("pms_rop_app.xml", "pms_rop_SUITE", "R1A"),
    [{reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    %%rpc(pmsDebug, stop_clear, []),
    rpc(pmsDb, rop_file_delete_all, []), 
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, unsetenv, ["FAKE_VNFM"]);
        _ ->
            ok
    end,
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
     tc_1job_illegal_pmgrp,
     tc_1job_illegal_mt,
     tc_1job_1illegal_grp_mt_1,
     tc_1job_1illegal_grp_mt_2,
     tc_1job_1illegal_grp_mt_3,
     tc_1job_1illegal_grp_mt_4
    ].



%% @doc
%% Start one application and a PM job with one counter, 
%% the counter (pmGroup) is not defined in the system.
%% No ROP file should be generated.
%% @end
tc_1job_illegal_pmgrp(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroupNonExist}),
    ok = close_trans(),
        
    try
	Timeout = get_rop_wait_time(?REP_PERIOD1, 1),
	{error, timeout} = wait_rop_file(1, Timeout)
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,

    clean_up([?JOB1], [?NACKA]).

%% @doc
%% Start one application and a PM job with one counter, 
%% the counter (MT) is not defined in the system.
%% No ROP file should be generated.
%% @end
tc_1job_illegal_mt(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPTypeNonExist}),
    ok = close_trans(),
    
    try
	Timeout = get_rop_wait_time(?REP_PERIOD1, 1),
	{error, timeout} = wait_rop_file(1, Timeout)
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,

    clean_up([?JOB1], [?NACKA]).

%% @doc
%% Start one application and a PM job with one counter, 
%% one of the counters is not defined in the system, illegal MR
%% No ROP file should be generated.
%% @end
tc_1job_1illegal_grp_mt_1(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = create_job_mr(?JOB1, {?MR1_2, ?ROPGroup1, ?ROPTypeNonExist}),
    ok = close_trans(),
    
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["<r p=\"1\">1</r>"])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,

    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and a PM job with one counter, 
%% one of the counters is not defined in the system, illegal Grp and MR
%% No ROP file should be generated.
%% @end
tc_1job_1illegal_grp_mt_2(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = create_job_mr(?JOB1, {?MR1_2, ?ROPGroupNonExist, ?ROPTypeNonExist}),
    ok = close_trans(),
    
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["<r p=\"1\">1</r>"])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,

    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and a PM job with one counter, 
%% one of the counters is not defined in the system, illegal MR
%% No ROP file should be generated.
%% @end
tc_1job_1illegal_grp_mt_3(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1}),
    ok = create_job_mr(?JOB1, {?MR1_2, ?ROPGroup2, ?ROPTypeNonExist}),
    ok = close_trans(),
    
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["<r p=\"1\">1</r>"])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,

    clean_up([?JOB1], [?NACKA]).


%% @doc
%% Start one application and a PM job with one counter, 
%% one of the counters is not defined in the system, illegal Grp and MR
%% No ROP file should be generated.
%% @end
tc_1job_1illegal_grp_mt_4(_Config) -> 
    {ok, _} = pms_erl_app:start(?NACKA, ?LDN1, ?MEAS_VAL_SUM([1])),
    initialize(?NACKA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr(?JOB1, {?MR1_1, ?ROPGroup1}),
    ok = create_job_mr(?JOB1, {?MR1_2, ?ROPGroupNonExist}),
    ok = close_trans(),
    
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	ok = check_rop_cont(Data, ["<r p=\"1\">1</r>"])
    catch
	_:Error ->
	    ?CLEAN_UP_AND_FAIL(Error, [?JOB1], [?NACKA])
    end,

    clean_up([?JOB1], [?NACKA]).


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
    WT = get_rop_wait_time(RP, N),
    {ok, RopFile} = wait_rop_file(N, WT),
    Data = get_rop_data(RopFile),
    ct:print("ROP Data:~n~s~n", [?B2L(Data)]),
    Data.


get_rop_wait_time(?RP_10 = RP, N) ->
    (N + 3)*RP;

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
    open_trans(),
    [delete_job(Job) || Job <- Jobs],
    close_trans(),
    timer:sleep(100),
    [begin 
	 finalize(App),
	 timer:sleep(100),
	 ok = pms_erl_app:stop(App) 
     end || App <- Apps],
    ok.
    

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

cleanup()          -> ?LIB:cleanup().

%%rpc(M, F, A) -> ?LIB:rpc(M, F, A).
host()   -> ?LIB:host().
%%module() -> ?LIB:module().
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

    
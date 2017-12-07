%%% %CCaseFile:	pms_sftp_SUITE.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R9A/R11A/1

%%% @doc == Test suite for PM ROP fetch via sftp ==
%%% This suite tests the sftp functionality for fetching of ROP files.
%%% @end

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

%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-08-29 eolaand     Created
%%% R3A/1      2015-01-16 erarafo     Copyright complaint fixed
%%% R4A/1      2015-07-09 etxjovp     Add group definitions used by CS CI
%%% R9A/1      2017-01-23 ekurnik     Added FTPES test group
%%% R9A/2      2017-01-30 ekurnik     FTPES group commented out due to 
%%%                                   FTP client issues
%%% R9A/3      2017-04-10 ekurnik     Minor change to ftpes_hook
%%% ----------------------------------------------------------

-module(pms_sftp_SUITE).

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

-export([tc_1rop_fetch/1,
	 tc_2rop_fetch/1,
	 tc_3rop_fetch/1,
	 tc_4rop_fetch/1,
	 tc_3rop_fetch_2rm/1,
	 tc_1rop_rm_disallowed/1,
	 tc_2rop_rm_disallowed/1,
	 tc_3rop_rm_disallowed/1,
	 tc_2rop_rm_1_disallowed/1
	]).

-compile([nowarn_export_all, export_all]).

-include_lib("common_test/include/ct.hrl").

-define(ROP_DIR, "rop").

-define(KENTA,   kenta).
-define(NACKA,  nacka).
-define(RONNIE,  ronnie).

-define(LDN1, "ME=1,App=kenta").
-define(LDN2, "ME=1,App=nacka").
-define(LDN3, "ME=1,App=ronnie").

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

-define(TEST_APPS,  [?KENTA]).
-define(TEST_APPS2, [?KENTA, ?NACKA, ?RONNIE]).

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
-define(REP_PERIOD_15M, 15*60000).
-define(LIB, pms_test_lib).
-define(L2B(__L), list_to_binary(__L)).
-define(B2L(__B), binary_to_list(__B)).

-define(CLEAN_UP_AND_FAIL(Error, Jobs, Apps, RP),
	ct:log(lightred, "Test case failed: ~p.~nStacktrace: ~p", 
	       [Error, erlang:get_stacktrace()]),
	clean_up(Jobs, Apps, RP),
	ct:fail(Error)).

-define(CLEAN_UP_AND_FAIL(Error, Jobs, Apps),
	?CLEAN_UP_AND_FAIL(Error, Jobs, Apps, ?REP_PERIOD1)).


%% @hidden
suite() ->
    Hooks = hooks(),
    Hooks ++ [{timetrap,{minutes,60}}].

%% @hidden
init_per_suite(Config) ->
    RP = rpc(pmsDb, pms_env_get, [reporting_period]),
    ct:pal("Reporting period mode = ~p~n", [RP]),
    rpc(pmsI, rp_ecim, [[]]),
    %% start adapter with default rct_client - sftp client
    start_ftpes_sftp_adapter(rct_sftp_client),
    [{reporting_period, RP} | Config].

%% @hidden
end_per_suite(Config) ->
    RP = proplists:get_value(reporting_period, Config),
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    
    %% use sftp to clean up since ftpes should be disabled at this point
    set_rct_client(rct_sftp_client),
    start_channel(),
    delete_all_rop_files(),
    stop_channel(),
    %% Stop the adapter
    stop_ftpes_sftp_adapter(),
    ok.

%% @hidden
init_per_group(sftp_group, Config) ->
    %% update rct_client
    set_rct_client(rct_sftp_client),
    Config;

init_per_group(ftpes_group, Config) ->
    %% Check if the server is started (should be removed once ftpes server is 
    %% started automatically)
    Started = ftpes_test_lib:start_server(pms_test_lib:host()),
    
    %% Create NodeCredential and TrustCategory needed for FTPES server to work
    NewConfig = ftpes_test_lib:initialize_nc_tc(Config),
    
    %% Set nodeCredential and trustCategory with newly created NC and TC
    ftpes_test_lib:enable_ftpes_tls(NewConfig),
    
    %% update rct_client + started parameter
    set_rct_client(rct_ftpes_client),
    [{started, Started} | NewConfig];

init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
end_per_group(ftpes_group, Config) ->
    %% Remove NC and TC references from FtpTls
    ftpes_test_lib:disable_ftpes_tls(),
    
    %% If server was started in init_per_group stop it
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_server(pms_test_lib:host());
        yes -> ok
    end,

    %% Delete NodeCredential and TrustCategory
    ftpes_test_lib:clean_nc_tc(Config),
    ok;

end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    start_channel(),
    delete_all_rop_files(),
    cleanup(),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    log_rop_files(Config),
    cleanup(),
    close_trans(),
    stop_channel(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, sftp_group}
     %%,{group, ftpes_group}
    ].

all_tc() ->
    [
     tc_1rop_fetch,
     tc_2rop_fetch,
     tc_3rop_fetch,
     tc_4rop_fetch,
     tc_3rop_fetch_2rm,
     tc_1rop_rm_disallowed,
     tc_2rop_rm_disallowed,
     tc_3rop_rm_disallowed,
     tc_2rop_rm_1_disallowed
    ].

%% @hidden
groups() ->
    AllGroup = all_tc(),
    [
     {default__group, [], AllGroup},
     {sftp_group, [], AllGroup},
     {ftpes_group, [], AllGroup},
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
%% Start one application and a PM job with one counter and verify via sftp 
%% that a ROP file is generated with no suspect marking.
%% @end
tc_1rop_fetch(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job1", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job1"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job1"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that two ROP files are generated with no suspect marking.
%% @end
tc_2rop_fetch(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job2", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_second_rop_file(?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job2"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job2"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that three ROP files are generated with no suspect marking. 
%% The files are stored on disc.
%% @end
tc_3rop_fetch(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job3", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_rop_file_n(3, ?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job3"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job3"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that four ROP files are generated with no suspect marking. 
%% The files are stored on disc.
%% @end
tc_4rop_fetch(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job4", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_rop_file_n(4, ?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job4"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job4"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that three ROP files are generated with no suspect marking. 
%% Remove two of the ROP files and wait for the next ROP file. 
%% The files are stored first on disc and then in RAM.
%% @end
tc_3rop_fetch_2rm(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job5", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_rop_file_n(3, ?REP_PERIOD1),
	nomatch = re:run(Data, "suspect"),
	delete_n_rop_files(2),
	get_second_rop_file(?REP_PERIOD1)
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job5"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job5"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that one ROP file is generated.
%% Try to remove the ROP file and verify that it is not allowed.
%% @end
tc_1rop_rm_disallowed(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job6", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	{ok, RopFile} = wait_rop_file(1, 3*?REP_PERIOD1),
	{error, permission_denied} = delete_rop_file(RopFile),
	ct:log("Delete ~p rejected ok: ~p", [RopFile, permission_denied])
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job6"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job6"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that two ROP files are generated.
%% Try to remove the ROP files and verify that it is not allowed.
%% @end
tc_2rop_rm_disallowed(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job7", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	{ok, _} = wait_rop_file(1, 3*?REP_PERIOD1),
	{ok, _} = wait_rop_file(2, 2*?REP_PERIOD1),
	{ok, [ROPFile1, ROPFile2]} = list_rop_files(),
	{error, permission_denied} = delete_rop_file(ROPFile1),
	ct:log("Delete ~p rejected ok: ~p", [ROPFile1, permission_denied]),
	{error, permission_denied} = delete_rop_file(ROPFile2),
	ct:log("Delete ~p rejected ok: ~p", [ROPFile2, permission_denied])
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job7"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job7"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that three ROP files are generated.
%% Try to remove the ROP files and verify that it is not allowed.
%% @end
tc_3rop_rm_disallowed(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job8", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	{ok, _} = wait_rop_file(1, 3*?REP_PERIOD1),
	{ok, _} = wait_rop_file(2, 2*?REP_PERIOD1),
	{ok, _} = wait_rop_file(3, 2*?REP_PERIOD1),
	{ok, [ROPFile1, ROPFile2, ROPFile3]} = list_rop_files(),
	{error, permission_denied} = delete_rop_file(ROPFile1),
	ct:log("Delete ~p rejected ok: ~p", [ROPFile1, permission_denied]),
	{error, permission_denied} = delete_rop_file(ROPFile2),
	ct:log("Delete ~p rejected ok: ~p", [ROPFile2, permission_denied]),
	{error, permission_denied} = delete_rop_file(ROPFile3),
	ct:log("Delete ~p rejected ok: ~p", [ROPFile3, permission_denied])
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job8"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job8"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that two ROP files are generated.
%% Read one of the files and delete the file. Try to remove the other ROP file
%% and verify that it is not allowed.
%% @end
tc_2rop_rm_1_disallowed(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job9", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	{ok, _} = wait_rop_file(1, 3*?REP_PERIOD1),
	{ok, _} = wait_rop_file(2, 2*?REP_PERIOD1),
	{ok, [ROPFile1, ROPFile2]} = list_rop_files(),
	{error, permission_denied} = delete_rop_file(ROPFile1),
	ct:log("Delete ~p rejected ok: ~p", [ROPFile1, permission_denied]),
	{error, permission_denied} = delete_rop_file(ROPFile2),
	ct:log("Delete ~p rejected ok: ~p", [ROPFile2, permission_denied]),
	_Data = get_rop_data(ROPFile1),
	ok = delete_rop_file(ROPFile1),
	ct:log("Delete ~p ok.", [ROPFile1]),
	{error, permission_denied} = delete_rop_file(ROPFile2),
	ct:log("Delete ~p still rejected ok: ~p", 
	       [ROPFile2, permission_denied])
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job9"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job9"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and two PM jobs with different RP:s.
%% Verify that a ROP file is generated with no suspect marking.
%% @end
tc_2jobs_rop_fetch(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job10", {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = create_job_mr("job11", ?JOB_ATTR_RP30,
		       {?MR2_1, ?ROPGroup1, ?ROPType2Avg}),
    ok = close_trans(),
    try
	Data = get_first_rop_file(3*?REP_PERIOD1),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job10"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job10"], [?KENTA]).


%%--------------------------------------------------------------------
%% @doc
%% Start one application and a PM job with one counter and verify via sftp 
%% that three ROP files are generated with no suspect marking. 
%% The files are stored on disc.
%% @end
tc_3_15m_rop_fetch(_Config) -> 
    pms_erl_app:start(?KENTA, ?LDN1, 
		      ?MEAS_VAL_SUM([1])),
    initialize(?KENTA, [?ROPGroup1]), 
    {ok, _} = open_trans(),
    ok = create_job_mr("job12", ?JOB_ATTR_RP15M, 
		       {?MR1_1, ?ROPGroup1, ?ROPType1Sum}),
    ok = close_trans(),
    try
	Data = get_rop_file_n(3, ?REP_PERIOD_15M),
	nomatch = re:run(Data, "suspect")
    catch
	_:Error ->
	    ST = erlang:get_stacktrace(),
	    ct:log(lightred, "Test case failed: ~p~n~p", [Error, ST]),
	    clean_up(["job12"], [?KENTA]),
	    ct:fail(Error)
    end,
    clean_up(["job12"], [?KENTA]).


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
    {ok, RopFile} = wait_rop_file(N, (N + 2)*RP),
    timer:sleep(1100),
    Data = get_rop_data(RopFile),
    ct:print("ROP Data:~n~s~n", [?B2L(Data)]),
    Data.
    

wait_rop_file(_N, Timeout) when Timeout =< 0 ->
    {error, timeout};

wait_rop_file(N, Timeout) ->
    case list_rop_files() of
	{ok, Files} when length(Files) < N ->
	    timer:sleep(1000),
	    wait_rop_file(N, Timeout - 1000);
	{ok, Files} ->
	    ct:pal("Found ROP files: ~p", [Files]),
	    {ok, lists:nth(N, Files)}
    end.


get_rop_data(Name) ->
    {ok, ZipData} = read_file(filename:join(?ROP_DIR, Name)),
    zlib:gunzip(ZipData).


list_rop_files() ->
    list_rop_files(?ROP_DIR).


list_rop_files(Dir) ->
    case list_dir(Dir) of
	{ok, Files} ->
	    {ok, lists:sort(Files)};
	Error ->
	    Error
    end.


delete_rop_file(File) ->
    FileName = filename:join(?ROP_DIR, File),
    delete_file(FileName).


delete_all_rop_files() ->
    read_files(?ROP_DIR),
    Res = delete_files(?ROP_DIR),
    ct:pal("Delete all ROP files: ~p", [Res]).
    

delete_n_rop_files(N) ->
    timer:sleep(1000),
    {ok, Files} = list_rop_files(?ROP_DIR),
    delete_n_rop_files(Files, N).


delete_n_rop_files([File | Files], N) when N > 0 ->
    FileName = filename:join(?ROP_DIR, File),
    read_file(FileName),
    ct:pal("Delete ROP file: ~s", [FileName]),
    ok = delete_file(FileName),
    delete_n_rop_files(Files, N - 1);

delete_n_rop_files(Files, N) when Files =:= []; N =:= 0 ->
    ok.
    

clean_up(Jobs, Apps) ->
    clean_up(Jobs, Apps, ?REP_PERIOD1).

clean_up(Jobs, Apps, RP) ->
    clean_up(Jobs, Apps, RP, 100).

clean_up(Jobs, Apps, RP, Sleep) ->
    ?LIB:clean_up_and_wait(Jobs, Apps, RP div 1000, Sleep).
    

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
		    [{ct_hooks, [sftp_hook(), ftpes_hook() | Hooks]}];
		_ ->
		    [{ct_hooks, [sftp_hook(), ftpes_hook()]}]
	    end.

sftp_hook() ->
   case host() of
       testnode -> 
	   {rct_sftp_client, [{start_channel_per_tc, false}]};
       _PmsSim ->
	   {rct_sftp_client, [{port, 8899}, {start_channel_per_tc, false}]}
   end.

ftpes_hook() ->
    {rct_ftpes_client, [{start_session_per_tc, false}]}.

open_trans()  -> ?LIB:open_trans().
close_trans() -> ?LIB:close_trans().


log_rop_files(Config) ->
    case read_files(?ROP_DIR) of
	{ok, RopData} ->
	    log_rop_files(RopData, Config);
	Error ->
	    ct:log(lightred, "Failed to read ROP files: ~p", [Error])
    end.

log_rop_files(RopData, Config) ->
    ?LIB:log_rop_files(RopData, Config).


log_rop_file(FileName, Data, Config) ->
    log_rop_files([{FileName, Data}], Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SFTP - FTPES adapter calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_ftpes_sftp_adapter() ->
    start_ftpes_sftp_adapter(undefined).

start_ftpes_sftp_adapter(RctClient) ->
    pms_ftpes_sftp_adapter:start(RctClient).

stop_ftpes_sftp_adapter() ->
    pms_ftpes_sftp_adapter:stop().

set_rct_client(RctClient) ->
    pms_ftpes_sftp_adapter:set_rct_client(RctClient).

start_channel() ->
    pms_ftpes_sftp_adapter:start_channel().

stop_channel() ->
    pms_ftpes_sftp_adapter:stop_channel().

list_dir(Dir) ->
    pms_ftpes_sftp_adapter:command(list_dir, [Dir]).
read_file(Filename) ->
    pms_ftpes_sftp_adapter:command(read_file, [Filename]).
read_files(Dirname) ->
    pms_ftpes_sftp_adapter:command(read_files, [Dirname]).
delete_file(Filename) ->
    pms_ftpes_sftp_adapter:command(delete_file, [Filename]).
delete_files(Dirname) ->
    pms_ftpes_sftp_adapter:command(delete_files, [Dirname]).



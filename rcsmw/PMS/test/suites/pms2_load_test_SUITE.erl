%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_load_test_SUITE.erl %
%%% @doc
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms2_load_test_SUITE).
-vsn('/main/R5A/R6A/R8A/R10A/R11A/R12A/1').
-date('2017-11-30').
-author('eolaand').
-shaid('73cf5c3e1fdd44096c461abc9335a370cb53b302').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R5A/1      2016-04-01 edamkon     Copied from pms_job_ext_SUITE
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([basic/1,
         load_test_1/1,
         load_test_2/1,
         load_test_3/1,
         load_test_4/1,
         load_test_5/1,
         load_test_6/1,
         load_test_cover/1,
	 create_multi_mr/1,
         pmi_data_per_obj/1,
         load_test_long/1]).


-include_lib("common_test/include/ct.hrl").
-include("pms2_load_test.hrl").
-include("pms2_test.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
       %%basic,
       load_test_1,
       load_test_2,
       load_test_3,
       load_test_4,
       load_test_5,
       load_test_6
%%       create_multi_mr
       %%,pmi_data_per_obj
       %%,load_test_long
    ].

suite() ->
    [{ct_hooks, [
		 {rct_netconf, nc1},
		 %% {pms_pmi_proxy,  [{erl_api, false}]},
		 {pms_pmi_proxy,  [{erl_api, false},
				   {trace_ift, false},
				   {trace_child, false}]},
                 {pms_rop_hook,   []},
                 {pms_log_hook,   [{severity, 1}]}
                 %% {pms_log_hook,   []}
                ]}].

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

init_per_testcase(_TestCase, Config) ->
    %% rpc(pmsDb, rop_file_delete_all, []),
    cleanup(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% log_rop_files(Config),
    cleanup(),
    %% rpc(pmsDb, rop_file_delete_all, []),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------

%%------------------------------------------------------------
%% Basic TC for testing pms2_load_test_lib functionality.
%%------------------------------------------------------------
basic(_Config) ->
    ok = run_load_test(4, [%%{mr_per_mt,  true},
                           {n_counters, 1},
                           {n_groups,   1},
                           {n_jobs,     1},
                           {n_objects,  1},
                           {rp,         ?TEN_SECONDS}]).


%%------------------------------------------------------------
%% TC config:l
%% 1 job - 10 grps - 2500 MTs - 10 LDNs - 1 app
%%------------------------------------------------------------
load_test_1(_Config) ->
    ok = run_load_test(3, [{n_counters, 2500},
                           {n_groups,   10},
                           {n_jobs,     1},
                           {n_objects,  10},
                           {n_values,   250000},
                           {rp,         ?ONE_MIN}]).


%%------------------------------------------------------------
%% TC config:
%% 1 job - 250 grps - 100 MTs - 10 LDNs - 1 app
%%------------------------------------------------------------
load_test_2(_Config) ->
    ok = run_load_test(3, [{n_counters, 100},
                           {n_groups,   250},
                           {n_jobs,     1},
                           {n_objects,  10},
                           {n_values,   250000},
                           {rp,         ?ONE_MIN}
			  ]).


%%------------------------------------------------------------
%% TC config:
%% 1 job - 20 grps - 125 MTs - 100 LDNs - 1 app
%%------------------------------------------------------------
load_test_3(_Config) ->
    ok = run_load_test(3, [{n_counters, 125},
                           {n_groups,   20},
                           {n_jobs,     1},
                           {n_objects,  100},
                           {n_values,   250000},
                           {rp,         ?ONE_MIN}
			  ]).


%%------------------------------------------------------------
%% TC config:
%% 1 job - 20 grps - 125 MTs - 200 LDNs - 1 app
%%------------------------------------------------------------
load_test_4(_Config) ->
    ok = run_load_test(3, [{n_counters, 125},
                           {n_groups,   20},
                           {n_jobs,     1},
                           {n_objects,  200},
                           {n_values,   500000},
                           {rp,         ?ONE_MIN}
			  ]).


%%------------------------------------------------------------
%% TC config:
%% 1 job - 20 grps - 125 MTs - 250 LDNs - 1 app
%%------------------------------------------------------------
load_test_5(_Config) ->
    ok = run_load_test(3, [{n_counters, 125},
                           {n_groups,   20},
                           {n_jobs,     1},
                           {n_objects,  250},
                           {n_values,   625000},
                           {rp,         ?FIVE_MIN}
			  ]).


%%------------------------------------------------------------
%% TC config:
%% 1 job - 20 grps - 125 MTs - 300 LDNs - 1 app
%%------------------------------------------------------------
load_test_6(_Config) ->
    ok = run_load_test(3, [{n_counters, 125},
                           {n_groups,   20},
                           {n_jobs,     1},
                           %% {n_objects,  300},
                           %% {n_values,   750000},
                           {n_objects,  400},
                           {n_values,   1000000},
                           {rp,         ?FIVE_MIN}
			  ]).


%%------------------------------------------------------------
%% TC config:
%% 1 job - 10 grps - 2500 MTs - 10 LDNs - 1 app
%%------------------------------------------------------------
load_test_cover(_Config) ->
    ok = run_load_test(3, [{n_counters, 100},
                           {n_groups,   10},
                           {n_jobs,     1},
                           {n_objects,  10},
                           {n_values,   10000},
                           {rp,         ?TEN_SECONDS}]).


%%------------------------------------------------------------
%% TC for checking difference in handling large number
%% of counters between 16A and 16B with "pmi_data_per_obj"
%% option turned on.
%%------------------------------------------------------------
pmi_data_per_obj(_Config) ->
    ok = run_load_test(3, [
			   {n_counters,        40},
                           {n_groups,          20},
                           {n_jobs,            1},
                           {n_objects,         50},
                           {n_values,          40000},
                           {pmi_data_per_obj,  true},
                           {rp,                ?FIVE_MIN}]).
                           %% {rp,                ?ONE_MIN}]).
			   %% {n_counters,        2},
                           %% {n_groups,          2},
                           %% {n_jobs,            1},
                           %% {n_objects,         2},
                           %% {n_values,          8},
                           %% {pmi_data_per_obj,  true},
                           %% {rp,                ?TEN_SECONDS}]).


%%------------------------------------------------------------
%% Same as the TC "load_test_1", but spread
%% over 15 RPs, so it runs for over 15 minutes.
%%
%% TC config:
%% 1 job - 10 grps - 2500 MTs - 10 LDNs - 1 app
%%------------------------------------------------------------
load_test_long(_Config) ->
    ok = run_load_test(15, [{n_counters, 2500},
                            {n_groups,   10},
                            {n_jobs,     1},
                            {n_objects,  10},
                            {rp,         ?ONE_MIN}]).


%%------------------------------------------------------------
%% Create one huge job
%% 
%%
%%
%% 
%%------------------------------------------------------------
create_multi_mr(_Config) ->
    rpc(logRamI, coli_set_severity, [["RcsPmCounters", "9"]]),
    ok = run_load_test(1, [{dirty_jobs, false},
			   {mr_per_mt,  true},
			   {n_counters, 10000},
			   {n_groups,   1},
			   {n_jobs,     1},
			   {n_objects,  1},
			   {rp,         ?ONE_MIN}]).



%%% #---------------------------------------------------------
%%% #3.2   TESTCASE HELPER FUNCTIONS
%%% #---------------------------------------------------------
cleanup() -> ?LIB:cleanup().

%% log_rop_files(A)    -> ?LOAD_LIB:log_rop_files(A).
run_load_test(A, B) -> ?LOAD_LIB:run_load_test(A, B).

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

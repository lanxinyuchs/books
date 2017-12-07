%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_test_lib.erl %
%%% @version /main/R2A/R3A/R4A/R5A/5

%%% @doc
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms_test_lib).
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R1A/1      2013-02-28 uabesvi     Created
%%% R1A/6      2013-06-24 uabesvi     Adaptions to new COM
%%% R4A/4      2015-12-03 erarafo     Support for fetching ift_app.log
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


-export([initialize/1]).
-export([initialize/2]).
-export([initialize_2/1]).
-export([initialize_2/2]).
-export([finalize/1]).
-export([ask/1]).
-export([get_children/1]).
-export([kill/1]).
-export([cleanup/0]).
-export([clean_up_and_wait/2,
	 clean_up_and_wait/3,
	 clean_up_and_wait/4]).
-export([get_table/1]).
-export([tables/0]).
-export([tables/1]).
-export([check_tables/1]).
-export([cleanup_tables/1]).
-export([delete_tables/1]).
-export([get_processes/0]).
-export([get_processes/1]).
-export([check_processes/1]).
-export([check_processes/2]).
-export([wait_subscribe/1]).
-export([wait_subscribe/2]).
-export([wait_subscribe/3]).
-export([wait_subscribe/4]).
-export([wait_until_subscribe/1]).
-export([wait_report/0]).
-export([wait_report/1]).
-export([wait_report/2]).
-export([wait_report/3]).
-export([wait_for_report/3]).
-export([wait_for_report/4]).
-export([wait_report_sc/0]).
-export([wait_report_sc/1]).
-export([wait_report_sc/2]).
-export([wait_report_sc/3]).
-export([wait_table_size/2]).
-export([wait_table_size/3]).

-export([create_job/1]).
-export([create_job/2]).
-export([update_job/2]).
-export([update_job/3]).
-export([delete_job/1]).
-export([delete_job/2]).
-export([create_mr/3]).
-export([create_mr/4]).
-export([delete_mr/2]).

-export([create_job_mr/2]).

-export([kill_proc/2]).
-export([term_proc/2]).
-export([mfa/3]).
-export([appdata/3]).

-export([set_ct_log/1]).
-export([ct_log/1]).
-export([ct_log/2]).
-export([ct_print/1]).
-export([ct_print/2]).
-export([ct_pal/1]).
-export([ct_pal/2]).

-export([log_msg/0]).
-export([log_msg/1]).


%% upgrade/restart interfaces
-export([get_upgrade/0]).
-export([set_upgrade/1]).
-export([kill_server/0]).
-export([term_server/0]).
-export([term_appreg/0]).
-export([restart_pms/0]).

-export([rpc/3]).
-export([host/0]).
-export([module/0]).
-export([hooks/0]).
-export([add_hooks/1]).
-export([open_trans/0]).
-export([close_trans/0]).

-export([log_rop_files/1,
	 log_rop_files/2,
	 log_rop_file/3]).


-export([log_te_log/2,
	 log_te_log/3,
	 clear_te_log/0]).


-define(L2B(__L), list_to_binary(__L)).

-define(LOW,    1).
-define(MEDIUM, 2).
-define(HIGH,   3).

-define(ACTIVE, 1).
-define(STOPPED, 2).

-define(PMS_CT_LOGGING, "PMS_CT_LOGGING").

-define(TEN_SECONDS,    10).
-define(THIRTY_SECONDS, 30).
-define(ONE_MIN,        60).
-define(FIVE_MIN,      300).
-define(FIFTEEN_MIN,   900).
-define(THIRTY_MIN,   1800).
-define(ONE_HOUR,     3600).
-define(TWELVE_HOUR,  3600*12).
-define(ONE_DAY,      3600*24).


%% P   granularity and reporting periods
-define(ENUM_TEN_SECONDS, 1).
-define(ENUM_THIRTY_SECONDS, 2).
-define(ENUM_ONE_MIN, 3).
-define(ENUM_FIVE_MIN, 4).
-define(ENUM_FIFTEEN_MIN, 5).
-define(ENUM_THIRTY_MIN, 6).
-define(ENUM_ONE_HOUR, 7).
-define(ENUM_TWELVE_HOUR, 8).
-define(ENUM_ONE_DAY, 9).



-define(LDN_TN,  ["ManagedElement=1,Transport=1"]).

%% -define(PM_GRP_1,  ["Group1"]).
%% -define(PM_GRP_2,  ["Group2"]).
-define(PM_GRP_12, ["Group1", "Group2"]).


%% -define(MR_NV, [{<<"currentValue">>,    {9, <<"curry">>}},
%% 		{<<"lastUpdated">>,     {12, "2000-03-01T14:00:00+02:00"}},
%% 		{<<"moClassInstance">>, {11, 27002}},
%% 		{<<"suspectFlag">>,     {10, false}}]).

-define(MR_SPEC(__Grp),
	[{<<"groupRef">>,
	  {11, ?L2B("ManagedElement=1,"
		    "SystemFunctions=1,"
		    "Pm=1,"
		    "PmGroup=" ++ __Grp)}},
	 {<<"measurementTypeRef">>,
	  {11, ?L2B("")}}]).

-define(MR_SPEC(__Grp, __MT),
	[{<<"groupRef">>,
	  {11, ?L2B("")}},
	 {<<"measurementTypeRef">>,
	  {11, ?L2B("ManagedElement=1,"
		    "SystemFunctions=1,"
		    "Pm=1,"
		    "PmGroup=" ++ __Grp ++ "," ++
		    "MeasurementType=" ++ __MT)}}]).
%% -define(MR_SPEC(__Grp, __MT),
%% 	[{<<"groupRef">>,
%% 	  {11, ?L2B("ManagedElement=1,"
%% 		    "SystemFunctions=1,"
%% 		    "Pm=1,"
%% 		    "PmGroup=" ++ __Grp)}},
%% 	 {<<"measurementTypeRef">>,
%% 	  {11, ?L2B("ManagedElement=1,"
%% 		    "SystemFunctions=1,"
%% 		    "Pm=1,"
%% 		    "PmGroup=" ++ __Grp ++ "," ++
%% 		    "MeasurementType=" ++ __MT)}}]).


-define(TESTNODE, testnode).

-define(SLEEP,  500).

-define(REPORT_DEF_VALS,  use_default_values).




%%% #---------------------------------------------------------
%%% #3.2   EXPORTED FUNCTIONS
%%% #---------------------------------------------------------




%%========================================================================
%%
%%========================================================================
get_upgrade() ->
    rpc(swm_stub, get_upgrade, []).

%%========================================================================
%%
%%========================================================================
set_upgrade(Bool) ->
    rpc(swm_stub, set_upgrade, [Bool]).

%%========================================================================
%%
%%========================================================================
kill_server() ->
    rpc(swm_stub, kill_server, []).

%%========================================================================
%%
%%========================================================================
term_server() ->
    term_proc(pmsServer, pms_cli).

%%========================================================================
%%
%%========================================================================
term_appreg() ->
    term_proc(pmsAppRegistry, pms_cli).


%%========================================================================
%%
%%========================================================================
restart_pms() ->
    mfa(pmsDataInit, init, [nodes]),
    mfa(pmsDataInit, init_data, []),
    mfa(pmsServer, start, []),
    mfa(pmsAppRegistry, start, []),
    ok.




%%========================================================================
%% initialize(Name) -> ok | {error, Reason}
%% initialize(Name, PmGroups) -> ok | {error, Reason}
%%========================================================================
%% Use default groups
initialize(Name) ->
    initialize(Name, ?PM_GRP_12).

initialize(Name, PmGroups) ->
    pms_erl_app:initialize(Name, module(), PmGroups).

%%========================================================================
%% initialize_2(Name) -> ok | {error, Reason}
%% initialize_2(Name, PmGroups) -> ok | {error, Reason}
%%========================================================================
%% Use default groups
initialize_2(Name) ->
    initialize_2(Name, [{?LDN_TN, ?PM_GRP_12}]).

initialize_2(Name, LdnGroups) ->
    Res = pms_erl_app:initialize_2(Name, module(), LdnGroups),
    wait_table(100),
    Res.

wait_table(N) when N < 1 ->
    {error, {wait_table, timeout}};
wait_table(N) ->
    case rpc(ets, tab2list, [pmsScAppMoLdns]) of
	L when length(L) > 0 -> ok;
	_                    -> timer:sleep(1000), wait_table(N-1)
    end.


%%========================================================================
%% finalize(Name, Res) -> ok | {error, Reason}
%%========================================================================
finalize(Name) ->
    pms_erl_app:finalize(Name, module()).

%%========================================================================
%% ask(Name, Ask) -> ok
%%
%% Ask = 'true' orders the application to ask for results from the testcase
%%========================================================================
ask(Name) -> pms_erl_app:ask(Name, self()).


%%========================================================================
%% get_children(Name) -> [Child] | {error, Reason}
%%
%% Fetch the child C process names from target.
%%========================================================================
get_children(Name) -> pms_erl_app:get_children(Name, module()).


%%========================================================================
%% kill(Name) -> ok | {error, Reason}
%%========================================================================
kill(Name) -> pms_erl_app:kill(Name, module()).


%%========================================================================
%% kill_proc(Name, Reason) ->
%%========================================================================
kill_proc(Name, Reason) ->
    case rpc(erlang, whereis, [Name]) of
	undefined -> undefined;
	Pid       -> rpc(erlang, exit, [Pid, Reason])
    end.


%%========================================================================
%% term_proc(Name, Reason) ->
%%========================================================================
term_proc(Name, Reason) ->
    case rpc(erlang, whereis, [Name]) of
	undefined -> undefined;
	_Pid      -> rpc(gen_server, cast, [Name, {test_terminate, Reason}])
    end.

%%========================================================================
%% mfa(M, F, A) ->
%%========================================================================
mfa(M, F, A) ->
    rpc(M, F, A).

%%========================================================================
%% appdata(AppdataFile, CXP, Rev) -> ok | {error, Reason}
%%
%% AppdataFile = string()
%% CXP         = string()
%% Rev         = string()
%%
%% send an appdata file to PMS
%% The appdata file must be located in etc/
%% CXP is the application name
%% Rev is the revision of the application
%%========================================================================
appdata(File, Cxp, Rev) ->
    appdata2(host(), File, Cxp, Rev).

appdata2(?TESTNODE, File, Cxp, Rev) ->
    RctTop = os:getenv("RCT_TOP"),
    Path   = filename:join([RctTop, "test", "suites", "PMS", File]),
    {AppData, ""} = xmerl_scan:file(Path),
    rpc(pmsAppData, appdata, [Cxp, Rev, AppData]);
appdata2(_, File, Cxp, Rev) ->
    PmssimTop = rpc(os, getenv, ["PMSSIM_TOP"]),
    Path      = filename:join([PmssimTop, "etc", File]),
    {AppData, ""} = xmerl_scan:file(Path),
    rpc(pmsAppData, appdata, [Cxp, Rev, AppData]).

%%========================================================================
%% cleanup() -> ok
%%
%% delete any hanging pmsJob and pmsJobGroup processes and
%% cleanup pmJob and meaurementReader tables
%%========================================================================
cleanup() ->
    delete_old_job_tables(),
    kill_old_job_procs(),
    ok.

%%========================================================================
%% clean_up_and_wait() -> ok
%%
%% Delete pmsJobs and applications.
%% Wait for the Job, JobGroup and AppJob processes to terminate.
%%========================================================================
clean_up_and_wait(Jobs, Apps) ->
    clean_up_and_wait(Jobs, Apps, 10).

clean_up_and_wait(Jobs, Apps, RPSec) ->
    clean_up_and_wait(Jobs, Apps, RPSec, 100).

clean_up_and_wait(Jobs, Apps, _RPSec, Sleep) ->
    open_trans(),
    [delete_job(Job) || Job <- Jobs],
    close_trans(),
    timer:sleep(Sleep),
%%    check_processes(length(Apps), RPSec),
    [begin
	 finalize(App),
	 timer:sleep(Sleep),
	 ok = pms_erl_app:stop(App)
     end || App <- Apps].
%%    check_processes(0).

%%========================================================================
%%
%%========================================================================
get_table(Tab) ->
    T = rpc(ets, tab2list, [Tab]),
    ct:pal("*** get_table ~p~n~p~n", [Tab, T]),
    T.


%%========================================================================
%%
%%========================================================================
tables() ->
    [rpc(ets, tab2list, [pmsAppRegistry], print),
     rpc(ets, tab2list, [pmJob], print),
     rpc(ets, tab2list, [pmGroup], print),
     rpc(ets, tab2list, [measurementType], print),
     rpc(ets, tab2list, [measurementReader], print)
    ].


tables(Tabs) ->
    [rpc(ets, tab2list, [Tab]) || Tab <- Tabs].


%%========================================================================
%%
%%========================================================================
check_tables(Before) ->
    check_tables(Before, 100).

check_tables(Before, N) when N < 0 ->
    Before = tables();
check_tables(Before, N) ->
    to(),
    After = tables(),
    case ct(Before, After) of
	ok -> ok;
	_  -> check_tables(Before, N - 1)
    end.


ct([], []) ->
    ok;
ct([H|TB], [H|TA]) ->
    ct(TB, TA);
ct([HB|_], [HA|_]) ->
    ct:pal("*** Table diff:~nBefore: ~p~nAfter:  ~p~n", [HB, HA]),
    error.

%%========================================================================
%%
%%========================================================================
wait_table_size(Tab, Size) ->
    wait_table_size(Tab, Size, 50).


wait_table_size(Tab, Size, N) when N > 0 ->
    T = rpc(ets, tab2list, [Tab]),
    Length = length(T),
    if
	Length =:= Size ->
	    ct:pal("*** get_table ~p~n~p~n", [Tab, T]),
	    ok;
	true ->
	    timer:sleep(100),
	    wait_table_size(Tab, Size, N - 1)
    end;

wait_table_size(_Tab, _Size, _N) ->
    {error, timeout}.
%%========================================================================
%%
%%========================================================================
cleanup_tables(Tabs) ->
    [cu_t(Tab) || Tab <- Tabs].

cu_t(Tab) ->
    Keys = rpc(mnesia, dirty_all_keys, [Tab]),
    [rpc(mnesia, dirty_delete, [Tab, Key], noprint) || Key <- Keys].

%%========================================================================
%%
%%========================================================================
delete_tables(Tabs) ->
    rpc(swm_stub, delete_tables, [Tabs]).


%%========================================================================
%%
%%========================================================================
get_processes() ->
    get_processes([pmsJob, pmsJobGroup, pmsAppJob]).

get_processes(Mods) ->
    Procs = rpc(erlang, processes, []),
    IC = [{P, gp(P)} || P <- Procs],
    [{P, C} || {P, {_, {M, _, _} = C}} <- IC, lists:member(M, Mods)].

gp(P) ->
    rpc(erlang, process_info, [P, current_function]).

%%========================================================================
%%
%%========================================================================
check_processes(Number) ->
    check_processes(Number, 20).


check_processes(Number, N) when is_integer(N), N > 0 ->
    case get_processes() of
	Procs when length(Procs) == Number ->
	    ok;
	_Procs ->
	    timer:sleep(1000),
	    check_processes(Number, N - 1)
    end;

check_processes(Number, _N) ->
    case get_processes() of
	Procs when length(Procs) == Number ->
	    ok;
	Procs ->
	    ct:pal("ERROR unexpected number of processes: ~p (~p)~n"
		   "      these are running: ~n~p~n",
		   [length(Procs), Number, Procs]),
	    Number = length(Procs) %% make it crash
    end.


%%========================================================================
%% wait_subscribe() -> Pid
%%
%%
%%========================================================================
wait_subscribe(PmGroups) ->
    wait_subscribe(PmGroups, not_applicaple).

wait_subscribe(PmGroups, Res) ->
    wait_subscribe(PmGroups, Res, ?TEN_SECONDS, 10000).

wait_subscribe(PmGroups, Res, GP) ->
    wait_subscribe(PmGroups, Res, GP, 10000).

wait_subscribe(any, timeout = Res, _, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for subscribe message ~n"
	   "Timeout: ~p  PmGroups: ~p  GP: any  Result: ~p~n",
	   [Time, any, Res]),
    {error, timeout} = wait(subscribe, Res, Time),
    ok;
wait_subscribe(any, Res, _, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for subscribe message ~n"
	   "Timeout: ~p  PmGroups: ~p  GP: any  Result: ~p~n",
	   [Time, any, Res]),
    {Pid, _, {GP, CounterSpec}} = wait(subscribe, Res, Time),
    PmGroups = [Grp || {Grp, _} <- CounterSpec],
    {Pid, PmGroups, GP};
wait_subscribe(PmGroups, Res, GP, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for subscribe message~n"
	   "Timeout: ~p  PmGroups: ~p  GP: ~p  Result: ~p~n",
	   [Time, PmGroups, GP, Res]),
    {Pid, _, {GP, CounterSpec}} = wait(subscribe, Res, Time),
    PmGroups = [Grp || {Grp, _} <- CounterSpec],
    Pid.


%%========================================================================
%% wait_until_subscribe(ExpectedData) -> ok | {error, Reason}
%%
%% Wait until PMI subscribe message is received
%%========================================================================
wait_until_subscribe(PmGroups) ->
    wait_until_subscribe(PmGroups, not_applicaple).

wait_until_subscribe(PmGroups, Res) ->
    {Pid, _, {_GP, CounterSpec}} = wait_us(Res, ?TEN_SECONDS),
    PmGroups = [Grp || {Grp, _} <- CounterSpec],
    Pid.


wait_us(Result, Time) ->
    receive
	{Msg, Pid, Name, Args} ->
	    ct:pal("*** App(~p) ---> LIB *** (~p) ~p~n Args: ~p~n",
		   [Name, Msg, Pid, Args]),
	    Pid ! {Msg, Result},
	    {Pid, Name, Args};
	_ ->
	   wait_us(Result, Time) 
    after Time ->
	    ct:pal("*** LIB ---> SUITE ***~p~nHORROR: ~p ~p~n",
		   [subscribe, timeout, Time]),
	    {error, timeout}
    end.




%%========================================================================
%% wait_report() -> Pid
%%
%%
%%========================================================================
wait_report() ->
    wait_report(any, ?REPORT_DEF_VALS).

wait_report(AppPid) ->
    wait_report(AppPid, ?REPORT_DEF_VALS).

wait_report(AppPid, Res) ->
    wait_report(AppPid, Res, 30000).



%%========================================================================
%% wait_report(What, Result, Time) -> Pid
%%
%%  What   - pid() | any |no | do_not_reply | wait
%%  Result - any()
%%  Time   - integer()
%%
%% If What is a
%%   pid()        - the process fetching the result must be that pid
%%   any          - the requesting process may be any process
%%   no           - there should not come any fetch requests from the app
%%   do_not_reply - in this case the app will not send pmData
%%   wait         - the app waits the time in ms given in result,
%%                  Result must be = {WaitTime, Result}
%%
%% Result can be do_not_reply or any counter spec, if a list of counter specs
%%        then there will be as many pmData as elements in the list
%% Time the application should ask for the result within that time.
%%========================================================================
wait_report(no, _, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait report, "
	   "no reply should be received ~p~n", [Time]),
    {error, timeout} = wait(report, ok, Time);
wait_report(do_not_reply, _, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait report, do_not_reply ~p~n", [Time]),
    wait(report, do_not_reply, Time);
wait_report(wait, {WaitTime, Res}, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait report, do_not_reply ~p~n", [Time]),
    R = wait(report, {wait, WaitTime, Res}, Time),
    timer:sleep(WaitTime + ?SLEEP),
    R;
wait_report(any, Res, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for report from ~p~nTimeout: ~p  "
	   "Reply with: ~p~n", [any, Time, Res]),
    {WaitPid, _, _} = wait(report, Res, Time),
    wr_sleep(Res),
    WaitPid;
wait_report(AppPid, Res, Time) when is_pid(AppPid) ->
    wait_report([AppPid], Res, Time);
wait_report(AppPids, Res, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for report from ~p~nTimeout: ~p  "
	   "Result: ~p~n", [AppPids, Time, Res]),
    {WaitPid, _, _} = wait(report, Res, Time),
    wr_sleep(Res),
    case lists:member(WaitPid, AppPids) of
	true  -> WaitPid;
	false -> AppPids = WaitPid
    end.

wr_sleep(?REPORT_DEF_VALS) ->
    timer:sleep(?SLEEP);
wr_sleep({objects, Res}) ->
    timer:sleep(?SLEEP + (length(Res) * 100));
wr_sleep(Res) ->
    timer:sleep(?SLEEP + (length(Res) * 100)).

%%========================================================================
%% wait_for_report(AppPid, GP, N) -> ok | {error, Reason}
%%
%% a report request must be received from AppPid within N report messages
%%========================================================================
wait_for_report(AppPid, GP,N) ->
    wait_for_report(AppPid, GP, N, 30000).

wait_for_report(_, _, N, _) when N < 1 ->
    {error, zero_counter};
wait_for_report(AppPid, GP, N, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for report from ~p~nTimeout: ~p  "
	   "Reply with: ~p~n", [AppPid, Time, ?REPORT_DEF_VALS]),
    {WaitPid, _, _} = wait(report, ?REPORT_DEF_VALS, Time, GP),
    case WaitPid of
	AppPid -> ok;
	_      -> wait_for_report(AppPid, N-1, Time)
    end.


%%========================================================================
%% wait_report_sc() -> Pid
%%
%%
%%========================================================================
wait_report_sc() ->
    wait_report_sc(any, ?REPORT_DEF_VALS).

wait_report_sc(AppPid) ->
    wait_report_sc(AppPid, ?REPORT_DEF_VALS).

wait_report_sc(AppPid, Res) ->
    wait_report_sc(AppPid, Res, 30000).


%%========================================================================
%% wait_report_sc(What, Result, Time) -> Pid
%%
%%  What   - pid() | any |no | do_not_reply | wait
%%  Result - any()
%%  Time   - integer()
%%
%% If What is a
%%   pid()        - the process fetching the result must be that pid
%%   any          - the requesting process may be any process
%%   no           - there should not come any fetch requests from the app
%%   do_not_reply - in this case the app will not send pmData
%%   wait         - the app waits the time in ms given in result,
%%                  Result must be = {WaitTime, Result}
%%
%% Result can be do_not_reply or any counter spec, if a list of counter specs
%%        then there will be as many pmData as elements in the list
%% Time the application should ask for the result within that time.
%%========================================================================
wait_report_sc(no, _, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait show counters, "
	   "no reply should be received ~p~n", [Time]),
    {error, timeout} = wait(report_sc, ok, Time);
wait_report_sc(do_not_reply, _, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait show counters, do_not_reply ~p~n", [Time]),
    wait(report_sc, do_not_reply, Time);
wait_report_sc(wait, {WaitTime, Res}, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait show counters, do_not_reply ~p~n", [Time]),
    R = wait(report_sc, {wait, WaitTime, Res}, Time),
    timer:sleep(WaitTime + ?SLEEP),
    R;
wait_report_sc(any, {_, _, List} = Res, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for show counters from ~p~nTimeout: ~p  "
	   "Reply with: ~p~n", [any, Time, Res]),
    {WaitPid, _, _} = wait(report_sc, Res, Time),
    wr_sleep(List),
    WaitPid;
wait_report_sc(AppPid, Res, Time) when is_pid(AppPid) ->
    wait_report_sc([AppPid], Res, Time);
wait_report_sc(AppPids, {_, _, List} = Res, Time) ->
    ct:pal("*** LIB <--- SUITE *** wait for show counters from ~p~nTimeout: ~p  "
	   "Result: ~p~n", [AppPids, Time, Res]),
    {WaitPid, _, _} = wait(report_sc, Res, Time),
    wr_sleep(List),
    case lists:member(WaitPid, AppPids) of
	true  -> WaitPid;
	false -> AppPids = WaitPid
    end.




%%========================================================================
%% wait(Name, Res) -> ok | {error, Reason}
%%
%%
%%========================================================================
wait(Msg, Result, Time) ->
    receive
	{Msg, Pid, Name, Args} ->
	    ct:pal("*** App(~p) ---> LIB *** (~p) ~p~n Args: ~p~n",
		   [Name, Msg, Pid, Args]),
	    Pid ! {Msg, Result},
	    {Pid, Name, Args}
    after Time ->
	    ct:pal("*** LIB ---> SUITE ***~p~nHORROR: ~p ~p~n",
		   [Msg, timeout, Time]),
	    {error, timeout}
    end.

wait(report = Msg, Result, Time, ReqGp) ->
    receive
	{Msg, Pid, Name, {ReqGp, _, _} = Args} ->
	    ct:pal("*** App(~p) ---> LIB *** (~p) ~p~n Args: ~p~n",
		   [Name, Msg, Pid, Args]),
	    Pid ! {Msg, Result},
	    {Pid, Name, Args};
	{Msg, Pid, Name, {RcvGp, _, _} = Args} ->
	    ct:pal("*** App(~p) ---> LIB *** (~p) ~p~n Args: ~p~n"
		   "Thrown due to ReportGp ~p /= RequestedGp ~p~n",
		   [Name, Msg, Pid, Args, RcvGp, ReqGp]),
	    wait(Msg, Result, Time, ReqGp)
    after Time ->
	    ct:pal("*** LIB ---> SUITE ***~p~nHORROR: ~p ~p~n",
		   [Msg, timeout, Time]),
	    {error, timeout}
    end.

%%========================================================================
%% create_job
%%
%%
%%========================================================================
create_job(Name) ->
    ct:pal("*** PMS <--- SUITE *** create Job: ~p~n", [Name]),
    Attrs = [{"granularityPeriod", ?ENUM_TEN_SECONDS},
	     {"reportingPeriod",   ?ENUM_THIRTY_SECONDS},
	     {"currentJobState",   ?ACTIVE}],
    create_job(Name, Attrs).

create_job(Name, Attrs) ->
    ct:pal("*** PMS <--- SUITE *** create Job: ~p~n", [Name]),
    ct:pal("CREATE JOB host ~p~n", [host()]),
    create_job(host(), Name, Attrs).

create_job(?TESTNODE, Name, Attrs) ->
    Res = pms_netconf:create_job(Name, Attrs),
    to(),
    Res;
create_job(_, Name, Attrs) ->
    Res = rpc(comte_stub, job, [Name, Attrs]),
    to(),
    Res.

%%========================================================================
%% create_job
%%
%%
%%========================================================================
create_job_mr({Name, Attrs}, MR) ->
    ct:pal("*** PMS <--- SUITE *** create job~n Job: ~p~n MR:  ~p~n",
	   [{Name, Attrs}, MR]),
    cj_mr(host(), {Name, Attrs}, MR);
create_job_mr(Name, MR) ->
    ct:pal("*** PMS <--- SUITE *** create job~n Job: ~p~n MR:  ~p~n",
	   [Name, MR]),
    Attrs = [{"granularityPeriod", ?ENUM_TEN_SECONDS},
	     {"reportingPeriod",   ?ENUM_TEN_SECONDS},
	     {"currentJobState",   ?ACTIVE}],
    cj_mr(host(), {Name, Attrs}, MR).


cj_mr(?TESTNODE, {Name, Attrs}, MR) ->
    Res = pms_netconf:create_job_mr(Name, Attrs, MR),
    to(),
    Res;
cj_mr(_, {JobName, JobAttrs}, MR) ->
    Res = cj_mr_pmssim(rpc(comte_stub, job, [JobName, JobAttrs]), JobName, MR),
    to(),
    Res.

cj_mr_pmssim(ok, JobName, {MrName, MrId, MrAttrs}) ->
    create_mr(JobName, MrName, MrId, MrAttrs);
cj_mr_pmssim(ok, JobName, [{MrName, MrId, MrAttrs} | T]) ->
    create_mr(JobName, MrName, MrId, MrAttrs),
    cj_mr_pmssim(ok, JobName, T);
cj_mr_pmssim(Error, _, _) ->
    Error.



%%========================================================================
%% update_job
%%
%%
%%========================================================================
update_job(Name, Attrs) ->
    update_job(host(), Name, Attrs).

update_job(?TESTNODE, Name, Attrs) ->
    Res = pms_netconf:create_job(Name, Attrs),
    to(),
    Res;
update_job(_, Name, Attrs) ->
    Res = rpc(comte_stub, update_job, [Name, Attrs]),
    to(),
    Res.

%%========================================================================
%% delete_job
%%
%%
%%========================================================================
delete_job(Name) ->
    update_job(Name, [{"requestedJobState", ?STOPPED}]),
    ct:pal("*** PMS <--- SUITE *** delete Job: ~p~n", [Name]),
    delete_job(host(), Name).
%%     Res = rpc(comte_stub, rm_job, [Name]),
%%     to(),
%%     Res.

delete_job(?TESTNODE, Name) ->
    Res = pms_netconf:delete_job(Name),
    to(),
    Res;
delete_job(_, Name) ->
    Res = rpc(comte_stub, rm_job, [Name]),
    to(),
    Res.

%%========================================================================
%% create_mr
%%
%%
%%========================================================================
create_mr(Job, Mr, Grp) ->
    create_mr_g(host(), Job, Mr, Grp).

create_mr_g(?TESTNODE, Job, Mr, Grp) ->
    Res = pms_netconf:create_mr(Job, Mr, Grp),
    to(),
    Res;
create_mr_g(_, Job, Mr, Grp) ->
    Attrs = [{"measurementSpecification", ?MR_SPEC(Grp)}],
    Res = rpc(comte_stub, mr, [Job, Mr, Attrs]),
    to(),
    Res.


create_mr(Job, Mr, Grp, Type) ->
    create_mr_t(host(), Job, Mr, Grp, Type).

create_mr_t(?TESTNODE, Job, Mr, Grp, Type) ->
    Res = pms_netconf:create_mr(Job, Mr, Grp, Type),
    to(),
    Res;
create_mr_t(_, Job, Mr, Grp, Type) ->
    Attrs = [{"measurementSpecification", ?MR_SPEC(Grp, Type)}],
    Res = rpc(comte_stub, mr, [Job, Mr, Attrs]),
    to(),
    Res.

%%========================================================================
%% delete_mr
%%
%%
%%========================================================================
delete_mr(Job, MR) ->
    ct:pal("*** PMS <--- SUITE *** delete MR~n Job: ~p~n MR:  ~p~n", [Job, MR]),
    delete_mr(host(), Job, MR).

%% ECIM does not allow removal of MR
delete_mr(?TESTNODE, Job, MR) ->
    Res = pms_netconf:delete_mr(Job, MR),
    to(),
    Res;
delete_mr(_, Job, MR) ->
    Res = rpc(comte_stub, rm_mr, [Job, MR]),
    to(),
    Res.

%%========================================================================
%%
%%========================================================================
kill_old_job_procs() ->
    ct:pal("KILLING JOB PROCESSES ~p~n", [get_processes()]),
    kojp(get_processes()).

kojp([]) ->
    ok;
kojp([{Pid, {pmsJobGroup, send, _}} | T]) ->
    timer:sleep(6000), %% TO specified in pmsJobGroup when sending a message
    %% rpc(pmsJobGroup, delete, [Pid]),
    rpc(pmsJobGroup, terminate, [Pid, []]),
    kojp(T);
kojp([{Pid, {pmsJobGroup, _, _}} | T]) ->
    %% rpc(pmsJobGroup, delete, [Pid]),
    rpc(pmsJobGroup, terminate, [Pid, []]),
    kojp(T);
kojp([{Pid, {pmsJob, send, _}} | T]) ->
    timer:sleep(6000), %% TO specified in pmsJob when sending a message
    %% rpc(pmsJob, delete, [Pid, killed_from_init_per_test_case]),
    rpc(pmsJob, terminate, [Pid, killed_from_init_per_test_case]),
    kojp(T);
kojp([{Pid, {pmsJob, _, _}} | T]) ->
    %% rpc(pmsJob, delete, [Pid, killed_from_init_per_test_case]),
    rpc(pmsJob, terminate, [Pid, killed_from_init_per_test_case]),
    kojp(T);
kojp([{Pid, {pmsAppJob, _, _}} | T]) ->
    rpc(pmsAppJob, stop, [Pid]),
    kojp(T).


%%========================================================================
%%
%%========================================================================
delete_old_job_tables() ->
    Tabs = tables(),
    %%ct:pal("DELETING JOB TABLES ~p~n", [Tabs]),
    dojt(Tabs).

dojt([]) ->
    ok;
dojt([H | T]) ->
    dojt_tab(H),
    dojt(T).

dojt_tab([]) ->
    ok;
dojt_tab([H | T]) when element(1, H) == pmJob;
		       element(1, H) == measurementReader ->
    rpc(mnesia, dirty_delete, [element(1, H), element(2, H)]),
    dojt_tab(T);
dojt_tab([_ | T]) ->
    dojt_tab(T).


%%========================================================================
%% rpc(M, F, A) -> Result
%%
%%
%%========================================================================
rpc(M, F, A) ->
    rct_rpc:call(?TESTNODE, M, F, A, 20000, noprint).

rpc(M, F, A, P) ->
    rct_rpc:call(?TESTNODE, M, F, A, 20000, P).


%%========================================================================
%% host() -> Host
%%
%%
%%========================================================================
host() -> ?TESTNODE.


module() -> pms_pmi_proxy.


add_hooks(AddHooks) ->
    [{ct_hooks, DefHooks}] = hooks(),
    NewHooks = lists:foldl(
		 fun(HookConf, AccHooks) ->
			 lists:keystore(element(1, HookConf), 1, AccHooks,
					HookConf)
		 end, DefHooks, AddHooks),
    [{ct_hooks, NewHooks}].


hooks() -> hooks(host()).

hooks(?TESTNODE) ->
    [{ct_hooks,
      [
       {cth_conn_log,[]},
       %% {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}], 5},
       {rct_rpc, ?TESTNODE},
       {pms_pmi_proxy, [{erl_api, false}]},
       {rct_htmllink,[]},
       %% {rct_logging,
       %% 	{oi_testapp, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
       %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
       {rct_logging, {rabbe_logs, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
       {rct_core, []},
       {rct_netconf, nc1},
       {pms_log_hook, []}
       %% {pms_c_pmi, [{trace_ift, true}, {trace_child, true}], 10}
      ]}];
hooks(_) ->
    [].


open_trans() ->
    open_trans(host()).

open_trans(?TESTNODE) ->
    ct_netconfc:open(nc1, []);
open_trans(_) ->
    {ok, ok}.


close_trans() ->
    close_trans(host()).

close_trans(?TESTNODE) ->
    ct_netconfc:close_session(nc1);
close_trans(_) ->
    ok.





to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


%%========================================================================
%% set_ct_log(Bool) -> any().
%%
%%
%%========================================================================
set_ct_log(true) ->
    os:putenv(?PMS_CT_LOGGING, "true");

set_ct_log(false) ->
    os:putenv(?PMS_CT_LOGGING, "false").

%%========================================================================
%% ct_log() -> any().
%%
%%
%%========================================================================
ct_log(Fmt) ->
    ct_log(Fmt, []).

ct_log(Fmt, Arg) ->
    ct_logging(log, Fmt, Arg).


ct_print(Fmt) ->
    ct_print(Fmt, []).

ct_print(Fmt, Arg) ->
    ct_logging(print, Fmt, Arg).


ct_pal(Fmt) ->
    ct_pal(Fmt, []).

ct_pal(Fmt, Arg) ->
    ct_logging(pal, Fmt, Arg).


ct_logging(F, Fmt, Arg) ->
    case os:getenv(?PMS_CT_LOGGING) of
	"false" ->
	    ok;
	_True ->
	    ct:F(Fmt, Arg)
    end.

%%========================================================================
%% log_msg() -> any().
%%
%%
%%========================================================================
log_msg() ->
    log_msg([pmsJobGroup, pmsJob, pmsAppJob]).

log_msg(Ms) ->
    rpc(pmsDebug, stop_clear, []),
    [rpc(pmsDebug, tpl, [r, M, log_msg]) || M <- Ms].


%%========================================================================
%% log_rop_files() -> any().
%%
%%
%%========================================================================
log_rop_files(Config) ->
    {ok, RopData} = rpc(pmsDb, rop_data_get_all, []),
    log_rop_files(RopData, Config).


log_rop_files(RopData, Config) ->
    FileDir = ?config(priv_dir, Config),
    {ok, RelPath} =  relative_file_path(FileDir),
    F = fun({ZipFile, ZipData}, {F, D}) ->
		Tokens   = string:tokens(ZipFile, "."),
		Data     = zlib:gunzip(ZipData),
		Name     = string:join(Tokens -- ["gz"], "."),
		FileName = Name ++ ".txt",
		File     = filename:join(FileDir, FileName),
		ok       = file:write_file(File, Data),
		{"~n<a href=\"~s\">~s</a>" ++ F,
		 [filename:join(RelPath, FileName), Name | D]}
	end,
    {Format, OutData} = lists:foldl(F, {[], []}, lists:reverse(RopData)),
    ct:log("ROP files:" ++ Format, OutData).


log_rop_file(FileName, Data, Config) ->
    log_rop_files([{FileName, Data}], Config).


%%========================================================================
%% log_te_log() -> any().
%%
%%
%%========================================================================
log_te_log(Prefix, Config) ->
    case host() of
	?TESTNODE ->
	    TELog = rpc(os, cmd, ["tex log read"]),
	    log_te_log(TELog, Prefix, Config);
	_Other ->
	    ok
    end.


log_te_log(TELog, Prefix, Config) when is_atom(Prefix) ->
    log_te_log(TELog, atom_to_list(Prefix), Config);

log_te_log(TELog, Prefix, Config) ->
    FileDir = ?config(priv_dir, Config),
    {ok, RelPath} =  relative_file_path(FileDir),
    Name = "trace_and_error_log",
    FileName = Prefix ++ "_" ++ Name ++ ".txt",
    File = filename:join(FileDir, FileName),
    ok = file:write_file(File, TELog),
    ct:log("TE Log: <a href=\"~s\">~s</a>",
	   [filename:join(RelPath, FileName), Name]).


%%========================================================================
%% clear_te_log() -> any().
%%
%%
%%========================================================================
clear_te_log() ->
    case host() of
	?TESTNODE ->
	    rpc(os, cmd, ["tex log clear"]),
	    ok;
	_ ->
	    ok
    end.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
relative_file_path(FileDir) ->
    {ok, CWD} = file:get_cwd(),
    SplitPath = filename:split(FileDir),
    SplitCWD = filename:split(CWD),
    relative_file_path(SplitCWD, SplitPath).


relative_file_path([H | T1], [H | T2]) ->
    relative_file_path(T1, T2);

relative_file_path([], [_TC, _Run, RelFilePath]) ->
    {ok, RelFilePath};

relative_file_path(_, _) ->
    {error, abspath}.


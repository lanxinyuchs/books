%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_test_lib.erl %
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R5A/2

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pessim.
%%% @end

-module(pes_test_lib).
-include_lib("common_test/include/ct.hrl").
-include("pes_test.hrl").
 
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% Rev        Date        Name        What
%%% -----      ----------  --------    ------------------------
%%% R3A/1      2014-09-04  uabesvi     Copied from pes_test_lib
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


-export([start/1]).
-export([start/2]).
-export([stop/1]).

-export([initialize/1]).
-export([initialize/2]).
-export([initialize/3]).

-export([finalize/2]).

-export([create_app/1]).
-export([create_app/2]).
-export([delete_app/2]).

-export([create_job/2]).
-export([create_job/3]).
-export([delete_job/2]).
-export([delete_job/3]).

-export([delete_jobs/2]).
-export([update_job/2]).

-export([update_me/3]).

-export([expected/3]).
-export([wait_expected_result/1]).
-export([wait_expected_result/2]).
-export([wait_expected_result/3]).
-export([wait_expected_error_result/2]).

-export([cleanup/0]).

-export([open_trans/0]).
-export([close_trans/0]).

-export([get_table/1]).
-export([tables/0]).
-export([check_tables/1]).
-export([wait_table_size/2]).
-export([wait_table_size/3]).

-export([get_processes/0]).
-export([get_processes/1]).
-export([check_processes/1]).
-export([check_processes/2]).

-export([get_aliases/0]).

-export([set_ct_log/1]).
-export([ct_log/1]).
-export([ct_log/2]).
-export([ct_print/1]).
-export([ct_print/2]).
-export([ct_pal/1]).
-export([ct_pal/2]).


-export([rpc/3]).
-export([host/0]).
%%-export([module/0]).
-export([hooks/0]).
-export([get_ct_config/1,
	 get_ct_config/2]).

-export([log_te_log/2,
	 log_te_log/3]).

-export([relative_file_path/1]).

-define(PES_CT_LOGGING, "PES_CT_LOGGING").

-define(L2B(__L), list_to_binary(__L)).

-define(TESTNODE, testnode).
-define(ERL_API, true).

-define(ERL_APP, pes_erl_app).
-define(PROXY,   pes_pei_proxy).


-define(SLEEP,  500).



%%% #---------------------------------------------------------
%%% #3.2   EXPORTED FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% initialize(App) -> ok | {error, Reason}
%% initialize(App, CbFlags) -> ok | {error, Reason}
%% initialize(App, CbFlags, CounterMap) -> ok | {error, Reason}
%%========================================================================
%% Use default groups
initialize(App) ->
    initialize(App, ?CB_DEF).

initialize(App, CbFlags) ->
    ?ERL_APP:peiInitialize(App, CbFlags, ?EVENT_MAP_DEF).

initialize(App, CbFlags, EventMap) ->
    ?ERL_APP:peiInitialize(App, CbFlags, EventMap).

%%========================================================================
%% finalize(App) -> ok | {error, Reason}
%%========================================================================
finalize(App, Handle) ->
    ct:pal("$$$$ FINALIZE ~n"),
    ?ERL_APP:peiFinalize(App, Handle).



%%========================================================================
%% expected(App, Handle, Expected) -> Ref | {error, Reason}
%% 
%% App           - string()   The application name
%% Handle        - term()     Handle received from initialize
%% Expected      - [Expect]   List of expected callbacks
%% Expect        - Action | {Action, [Options]} | {Action, Times, [Options]}
%% Action        - peiSubscribeRop | peiReportRop | peiReportShowCounters
%%                 no_subscribe_request | no_rop_report_request | send_copy
%% Times         - {repeat, N} | {ignore, N}
%% N             - integer() | wait_until_subscribe | wait_until_show_counters
%% Options       - {gp, GP} | {spec, [Spec]} | {values, [Values]} | 
%%                 {reportId, ReportId} | {ff, FinalFragment} |
%%                 {delay, DelayTime}
%%                 'gp' 'spec' are only valid for peiSubscribeRop
%%                 'values' 'reportId' 'ff' are only valid for peiReportRop
%%                 'delay' is valid for all messages
%% GP            - integer() Granularity period in seconds,  
%% Spec          - {GroupAlias, [MtAlias]}
%% GroupAlias    - integer()
%% MtAlias       - integer()
%% Values        - {GroupAlias, [{LdnAlias, [{MtAlias, [Value]}]}]} | [Values]
%% LdnAlias      - integer()
%% Value         - integer()
%% DelayTime     - integer()
%%                 Time in milliseconds the reply will be delayed with
%% ReportId      - integer()   Replace the actual reportId with this value
%% FinalFragment - boolean()
%% Ref           - Reference to be used in wait_expected_results
%% 
%% Expect: Action is equal to {Action, {repeat, 1}, []}
%%         {Action, [Options]} is equal to {Action, {repeat, 1}, [Options]}
%% 
%% Action: Normally wait for subscribe or report rop requests.
%%         - no_subscribe_request: there should not be any subscribe request.
%%           Used when no application has announced the groups.
%%         - no_rop_report_request there should not be any report rop
%%           request for the next GP.
%%           no_rop_report_request is useful when checking that no report
%%           rop request is received when there are no measurements
%%           ongoing
%%         - send_copy, the erl app sends a copy of all received messages
%%           to the test suite
%% Times:  If the same message is to received several times use
%%         {repeat, N} where N indicates the number of times the message
%%         is to be received. 
%%         wait_until_subscribe and wait_until_show_counters can only be 
%%         used together with peiReportRop. 
%%         These are used when there is at least one active job
%%         and waiting for a new subsrcribe request or for a show counters
%%         request, respectively.
%%         {ignore, N} means that the next N messages of type Action shall
%%         be ignored and no reply message is to be replied to PES.
%% Values: List of measurement values. 
%%         If Values is a list of Values each Value will be sent in a 
%%         separate peiRopData message
%% 
%%========================================================================
expected(App, Handle, Expected) ->
    ?ERL_APP:expected(App, Handle, Expected).





%%========================================================================
%% wait_expected_result([{Ref, App}]) -> ok | ct:fail()
%% wait_expected_result(Ref, App) -> ok | ct:fail()
%%
%% wait that the erl app replies with the result of the expected call
%%========================================================================
wait_expected_result(Apps) when is_list(Apps) ->
    wait_expected_result(Apps, []).


wait_expected_result(Ref, App) when is_atom(App) ->
    wait_expected_result([{Ref, App}]);
wait_expected_result(Apps, Time) when is_list(Apps), is_integer(Time) ->
    wer(Apps, [], Time);
wait_expected_result(Apps, Rops) when is_list(Apps), is_list(Rops) ->
    wer(Apps, Rops, 30000).


wait_expected_result(Ref, App, Time) when is_atom(App), is_integer(Time) ->
    wait_expected_result([{Ref, App}], Time);
wait_expected_result(Apps, Rops, Time) 
  when is_list(Apps), is_list(Rops), is_integer(Time) ->
    wer(Apps, Rops, Time).


wer([], [], _Timeout) ->
    ct:pal("%%##### wait_results OKOKOK ~p ~n", [ok]),
    ok;
wer(Apps, Rops, Timeout) ->
    ct:pal("%%##### wait_results waiting ~p ~n", [{Apps, Rops}]),
    receive
	{expected_result, Ref, App, ok} = Msg ->
	    case {lists:member({Ref, App}, Apps),
		  lists:delete({Ref, App}, Apps)} of
		{true, RemApps} ->
		    ct:pal("%%##### wait_results 1 true ~p ~n", [RemApps]),
		    wer(RemApps, Rops, Timeout);
		{false, _} ->
		    ct:pal("%%##### wait_results 1 ~p ~n", [cb_false]),
		    ct:fail({error, {not_found, Msg, Apps, Rops}})
	    end;
	{rop_result, Ref, Rop, ok} = Msg ->
	    case {lists:member({Ref, Rop}, Rops),
		  lists:delete({Ref, Rop}, Rops)} of
		{true, RemRops} ->
		    wer(Apps, RemRops, Timeout);
		{false, _} ->
		    ct:pal("%%##### wait_results 2 ~p ~n", [rop_false]),
		    ct:fail({error, {not_found, Msg, Apps, Rops}})
	    end;
	{Reply, _UnknownRef, AppOrRop, Error} = Msg ->
	    ct:pal("%%##### wait_results 3 Msg ~p  Apps ~p ~n", [Msg, Apps]),
	    ct:fail({error, {AppOrRop, Reply, Error}})
    after Timeout ->
	    ct:pal("%%##### wait_results 4 ~p ~n", [timeout]),
	    ct:fail({error, {timeout, Apps, Rops}})
    end.







%%========================================================================
%% wait_expected_error_result(App, Expected) -> ok | ct:fail()
%%
%% 
%% 
%%========================================================================
wait_expected_error_result(App, Expected) ->
    weer(App, [cb], Expected, 20000).


weer(App, Replies, Expected, Timeout) ->
    receive
	{expected_result, _, App, ok} = Reply ->
	    case lists:member(cb, Replies) of
		true -> 
		    ct:fail({error, {App, Reply, Expected}});
		false ->
		    weer(App, 
			 lists:delete(cb, Replies), 
			 Expected, 
			 Timeout)
	    end;
	{rop_result, _, App, ok} = Reply ->
	    case lists:member(rop, Replies) of
		true -> 
		    ct:fail({error, {App, Reply, Expected}});
		false ->
		    weer(App, 
			 lists:delete(cb, Replies), 
			 Expected, 
			 Timeout)
	    end;
	{expected_result, _, App, {error, {Expected, _}}} ->
	    ok;
	{expected_result, _, App, Error} ->
	    ct:fail({error, {App, expected_result, Error}});
	{rop_result, _, App, {error, {Expected, _}}} ->
	    ok;
	{rop_result, _, App, Error} ->
	    ct:fail({error, {App, rop_result, Error}});
	X ->
	    ct:pal("$$$$$$  ~p~n", [X])

    after Timeout ->
	    ct:fail({error, {App, Replies, timeout}})
    end.




%%========================================================================
%% get_aliases() -> 
%%========================================================================
get_aliases() ->
    rpc(pesDebug, get_aliases, []).





%%========================================================================
%% create_app(App) -> ok.
%% 
%% @doc 
%% start an app and create Job
%% @end
%%========================================================================
create_app(App) ->
    create_app(App, ?CB_DEF).

create_app(App, CbFlags) ->
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(App),
    initialize(App, CbFlags).
  

%%========================================================================
%% delete_app(App, Handle) -> ok.
%% 
%% @doc 
%% delete an app and check all is ok
%% @end
%%========================================================================
delete_app(App, Handle) ->
    finalize(App, Handle),
    ok.
    

%%========================================================================
%% create_job(App, Job) -> ok | {error, Reason}
%% 
%% @doc 
%% start an app and create Job
%% @end
%%========================================================================
create_job(Producer, Attrs) ->
    create_job(Producer, Attrs, ok).

create_job(Producer, Attrs, ExpectedRes) ->
    {ok, _} = open_trans(),
    Res = pes_netconf:create_job(Producer, Attrs),
    TransRes = close_trans(),
    cj_rc(Res, TransRes, ExpectedRes).


cj_rc(_Res, ok, ok) ->
    ok;
cj_rc(Res, {Exp, _}, Exp) ->
    Res;
cj_rc(Res, TransRes, ExpRes) ->
    ct:pal("*** PES <--- SUITE *** create Job: ERROR ~p~n",
	   [{Res, TransRes, ExpRes}]),
    {error, {unexpected, {Res, TransRes, ExpRes}}}.


%%========================================================================
%% delete_jobs(App, Job, Handle) -> ok.
%% 
%% @doc 
%% delete an app and the created Job
%% @end
%%========================================================================
delete_jobs(Producer, [Job | _] = Jobs) when is_list(Job) ->
    dj(Producer, Jobs);
delete_jobs(Producer, Job) ->
    dj(Producer, [Job]).

dj(Producer, Jobs) ->

    %%-------------------------------------------------------------
    %% delete the pm jobs
    %%-------------------------------------------------------------
    DelJobs = [delete_job(Producer, Job) || Job <- Jobs],

    case lists:usort(DelJobs) of
	[ok] -> ok;
	Sort -> {error, Sort}
    end.


%%========================================================================
%% delete_job
%% 
%% 
%%========================================================================
delete_job(Producer, Job) ->
    delete_job(Producer, Job, ok).

delete_job(Producer, Job, ExpectedRes) ->
    Stopped = Job#eventJob{requestedJobState = ?STOPPED},
    update_job(Producer, Stopped),
    ct:pal("*** PES <--- SUITE *** delete Job: ~p~n", [Job]),
    Deleted = Stopped#eventJob{eventGroupRef = undefined,
			       eventTypeRef  = undefined},
    {ok, _} = open_trans(),
    Res = pes_netconf:delete_job(Producer, Deleted),
    TransRes = close_trans(),
    to(),
    dj_rc(Res, TransRes, ExpectedRes).

dj_rc(_Res, ok, ok) ->
    ok;
dj_rc(Res, {Exp, _}, Exp) ->
    Res;
dj_rc(Res, TransRes, ExpRes) ->
    ct:pal("*** PES <--- SUITE *** delete Job: ERROR ~p~n",
	   [{Res, TransRes, ExpRes}]),
    {error, {unexpected, {Res, TransRes, ExpRes}}}.



%%========================================================================
%% update_job
%% 
%% 
%%========================================================================
update_job(Producer, Job) ->
    {ok, _} = open_trans(),
    Res = pes_netconf:create_job(Producer, Job),
    ok = close_trans(),
    to(),
    Res.


%%========================================================================
%% update_me
%% 
%% 
%%========================================================================
update_me(ME, UserLabel, LogicalName) ->
    {ok, _} = open_trans(),
    Res = pes_netconf:update_me(ME, UserLabel, LogicalName),
    ok = close_trans(),
    to(),
    Res.


%%========================================================================
%% create_job
%% 
%% 
%%========================================================================
%% create_job(Name) ->
%%     ct:pal("*** PES <--- SUITE *** create Job: ~p~n", [Name]),
%%     Attrs = [{"granularityPeriod", ?TEN_SECONDS},
%% 	     {"reportingPeriod",   ?TEN_SECONDS},
%% 	     {"currentJobState",   ?ACTIVE}],
%%     create_job2(Name, Attrs).

%% create_job2(Name, Attrs) ->
%%     ct:pal("*** PES <--- SUITE *** create Job: ~p~n", [Name]),
%%     ct:pal("CREATE JOB host ~p~n", [host()]),
%%     create_job2(host(), Name, Attrs).

%% create_job2(?TESTNODE, Name, Attrs) ->
%%     Res = pes_netconf:create_job(Name, Attrs),
%%     to(),
%%     Res;
%% create_job2(_, Name, Attrs) ->
%%     Res = rpc(comte_stub, job, [Name, Attrs]),
%%     to(),
%%     Res.


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
%% Misc functions
%% 
%% 
%%========================================================================


%%========================================================================
%% 
%%========================================================================
get_table(Tab) ->
    T = rpc(ets, tab2list, [Tab]),
    ct:pal("*** get_table ~p~n~p~n", [Tab, T]),
    T.

%%========================================================================
%% tables
%%========================================================================
tables() ->
    [rpc(ets, tab2list, [pesAppRegType]),
     rpc(ets, tab2list, [pesAppRegPid]),
     rpc(ets, tab2list, [eventJob])
    ].


%% tables(Tabs) ->
%%     [rpc(ets, tab2list, [Tab]) || Tab <- Tabs].


%%========================================================================
%% 
%%========================================================================
check_tables(Before) ->
    to(),
    After = tables(),
    ct(Before, After).


ct([], []) ->
    ok;
ct([H|TB], [H|TA]) ->
    ct(TB, TA);
ct([HB|_], [HA|_]) ->
    ct:pal("*** Table diff:~nBefore: ~p~nAfter:  ~p~n", [HB, HA]),
    HB = HA.


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
get_processes() ->
    get_processes([pesJob, pesProducer, pesAppJob]).

get_processes(Mod) when is_atom(Mod) ->
    get_processes([Mod]);
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
%% 
%%========================================================================
kill_old_job_procs() ->
    ct:pal("KILLING JOB PROCESSES ~p~n", [get_processes()]),
    kojp(get_processes()).

kojp([]) ->
    ok;
kojp([{Pid, {pesJob, What, _}} | T]) ->
    kojp_wait(What),
    rpc(pesJob, terminate, [Pid, killed_from_init_per_test_case]),
    kojp(T);
kojp([{Pid, {pesAppJob, _, _}} | T]) ->
    rpc(pesAppJob, stop, [Pid]),
    kojp(T);
kojp([_ | T]) ->
    kojp(T).


kojp_wait(send) ->
    timer:sleep(6000); %% TO specified in modules when sending a message
kojp_wait(_) ->
    ok.
   
%%========================================================================
%% delete_old_job_tables
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
dojt_tab([#eventJob{eventJobId = Job, 
		    jobControl = JobCtrl} | T]) 
  when JobCtrl == undefined orelse
       JobCtrl == ?JobControl_FULL ->
    rpc(mnesia, dirty_delete, [eventJob, Job]),
    dojt_tab(T);
dojt_tab([_ | T]) ->
    dojt_tab(T).




%%========================================================================
%% trans functions
%%========================================================================
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




%%========================================================================
%% rpc(M, F, A) -> Result
%% 
%% 
%%========================================================================
rpc(M, F, A) -> 
    ?PROXY:cs_node_call(M, F, A).

host() -> ?TESTNODE.



hooks() -> hooks(host()).

hooks(?TESTNODE) ->
    [{cth_conn_log,[]},
     {rct_htmllink,[]},
     {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
     {rct_core, []}, 
     {rct_netconf, nc1},
     {pes_pei_proxy, [{erl_api, false},
		      {trace_ift, true},
		      {trace_child, true}]},
     {pes_log_hook, []} 
    ];

hooks(_) ->
    [].


%%========================================================================
%% set_ct_log(Bool) -> any().
%% 
%% 
%%========================================================================
set_ct_log(true) ->
    os:putenv(?PES_CT_LOGGING, "true");

set_ct_log(false) ->
    os:putenv(?PES_CT_LOGGING, "false").

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
    case os:getenv(?PES_CT_LOGGING) of
	"false" ->
	    ok;
	_True ->
	    ct:F(Fmt, Arg)
    end.


to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


get_ct_config(Key) ->
    get_ct_config(Key, []).


get_ct_config(Key, Default) ->
    case ct:get_config(Key) of
	undefined ->
	    Default;
	Val ->
	    Val
    end.

   
%% a2l(A) ->
%%     atom_to_list(A).


start(A)    -> {ok, _} = ?ERL_APP:start(A, [{cb_module, pes_erl_app_cb}]).
start(A, B) -> {ok, _} = ?ERL_APP:start(A, B).

stop(A)  -> ok      = ?ERL_APP:stop(A).   


%%========================================================================
%% log_te_log() -> any().
%% 
%% 
%%========================================================================
log_te_log(Prefix, PrivDir) ->
    case host() of
	?TESTNODE ->
	    TELog = rpc(os, cmd, ["te log read"]),
	    log_te_log(TELog, Prefix, PrivDir);
	_Other ->
	    ok
    end.


log_te_log(TELog, Prefix, PrivDir) when is_atom(Prefix) ->
    log_te_log(TELog, atom_to_list(Prefix), PrivDir);

log_te_log(TELog, Prefix, PrivDir) ->
    {ok, RelPath} =  relative_file_path(PrivDir),
    Name = "trace_and_error_log",
    FileName = Prefix ++ "_" ++ Name ++ ".txt",
    File = filename:join(PrivDir, FileName),
    ok = file:write_file(File, TELog),
    ct:log("TE Log: <a href=\"~s\">~s</a>", 
	   [filename:join(RelPath, FileName), Name]).


%%========================================================================
%% relative_file_path() -> any().
%% 
%%========================================================================
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



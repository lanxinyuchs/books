%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_erl_app.erl %

-module(pms_erl_app).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/1').
-date('2015-05-22').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R2A/1    2014-03-20 uabesvi     Created
%%% ----------------------------------------------------------

%% test_app process functions
-export([start/1,start/2,start/3,start/4]).
-export([stop/1]).
-export([kill/2]).
-export([initialize/2]).
-export([initialize/3]).
-export([initialize_2/3]).
-export([finalize/1]).
-export([finalize/2]).
-export([ask/2]).
-export([get_children/2]).

-export([init/5]).

-export([
	 pmiSubscribeCallback/3,
	 pmiSubscribeCallback/4,
	 pmiReportCallback/4,
	 pmiReportShowCountersCallback/5
	]).

%% when running direct on Erlang node
-export([
	 pmiReportShowCountersCallback/4
	]).




%%-include("../src/ECIM_PM_mp.hrl").

-define(TS_TO, 10000).
-define(REPORT_DEF_VALS,  use_default_values).

-define(SC_OK,             0).
-define(SC_NO_COUNTERS,    1).
-define(SC_INTERNAL_ERROR, 2).


-record(loop, {name, 
	       pmi_mod,
	       handle,
	       ts_pid,
	       gp, 
	       timer_ref, 
	       pm_groups = [], 
	       meas_type,
	       counter_spec = [],
	       subscriber,
	       ldn,
	       cvals = []}).



start(Name) ->
    start(Name, "ME=1,App=" ++ atom_to_list(Name)).


start(Name, [{ask, Ask}]) ->
    start(Name, "ME=1,App=" ++ atom_to_list(Name), [], Ask);
start(Name, LDN) ->
    start(Name, LDN, []).

start(Name, LDN, CVals) ->
    start(Name, LDN, CVals, false).

start(Name, LDN, CVals, Ask) ->
    Res = proc(whereis(Name), Name, LDN, CVals, Ask),
    p("=== erl_app === ~p = start(~p, ~p, ~p, ~p)~n", 
      [Res, Name, LDN, CVals, Ask]),
    Res.

stop(Name) ->
    p("=== erl_app === stop(~p)~n", [Name]),
    Name ! {stop, self()},
    receive 
	stopped -> ok
    after 1000 ->
	    Pid = whereis(Name),  
	    p("=== erl_app === Force ~p exit(~p, kill)~n", [Name, Pid]),
	    Pid =:= undefined orelse exit(Pid, kill),
	    ok
    end.

kill(Name, Module) ->
    Name ! {kill, Module}.


get_children(Name, Mod) ->
    Name ! {get_children, self(), Mod},
    receive
	{get_children_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, erl_app_timeout}
    end.

initialize(Name, PmGroups) ->
    Name ! {initialize, self(), PmGroups},
    receive
	{initialize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.

%% initialize(Name, pmsPmI, PmGroups) ->
%%     initialize(Name, PmGroups);

    %% Name ! {initialize, PmGroups};
initialize(Name, Mod, PmGroups) ->
    Name ! {initialize, self(), Mod, PmGroups},
    receive
	{initialize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.

initialize_2(Name, Mod, RegData) ->
    Name ! {initialize_2, self(), Mod, RegData},
    receive
	{initialize_2_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.



finalize(Name) ->
    Name ! {finalize, self()},
    receive
	{finalize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.

finalize(Name, pmsPmI) ->
    finalize(Name);
    %% Name ! finalize;
finalize(Name, Mod) ->
    Name ! {finalize, self(), Mod},
    receive
	{finalize_result, Res} ->
	    Res
    after ?TS_TO ->
	    {error, timeout}
    end.


ask(Name, TsPid) ->
    Name ! {ask, TsPid}.

pmiSubscribeCallback(AppPid, GranularityPeriod, CounterSpecs) -> 
    p("=== ~p:pmiSubscribeCallback === ~n~p  ~n", 
      [?MODULE, {AppPid, GranularityPeriod, CounterSpecs}]),
    AppPid ! {pmiSubscribe, {not_valid, GranularityPeriod, CounterSpecs}},
    ok.

pmiSubscribeCallback(AppPid, Subscriber, GranularityPeriod, CounterSpecs) -> 
    p("=== ~p:pmiSubscribeCallback === ~n~p  ~n", 
      [?MODULE, {AppPid, Subscriber, GranularityPeriod, CounterSpecs}]),
    AppPid ! {pmiSubscribe, {Subscriber, GranularityPeriod, CounterSpecs}},
    ok.

pmiReportCallback(AppPid, GranularityPeriod, TimeSpec, Deadline) ->
    p("=== ~p:pmiReportCallback === ~n~p  ~n", 
      [?MODULE, {AppPid, GranularityPeriod, TimeSpec, Deadline}]),
    AppPid ! {pmiReport, {GranularityPeriod, TimeSpec, Deadline}},
    ok.


pmiReportShowCountersCallback(AppPid, 
			      Subscriber, 
			      RequestId,
			      MoLdn,
			      MaxResponseTime) ->
    p("=== ~p:pmiReportShowCountersCallback === ~n~p  ~n", 
      [?MODULE, {AppPid, RequestId, MoLdn, MaxResponseTime}]),
    AppPid ! {pmiReportShowCounters, 
	      {Subscriber, RequestId, MoLdn, MaxResponseTime}},
    ok.

pmiReportShowCountersCallback(AppPid, 
			      RequestId,
			      MoLdn,
			      MaxResponseTime) ->
    p("=== ~p:pmiReportShowCountersCallback === ~n~p  ~n", 
      [?MODULE, {AppPid, RequestId, MoLdn, MaxResponseTime}]),
    AppPid ! {pmiReportShowCounters, 
	      {self(), RequestId, MoLdn, MaxResponseTime}},
    ok.


proc(undefined, Name, LDN, CVals, Ask) ->
    p("=== erl_app === ~p = proc(undefined, ~p, ~p, ~p)~n",
      [Name, LDN, CVals, Ask]),
    Self = self(),
    Pid = spawn(?MODULE, init, [Self, Name, LDN, CVals, Ask]),
    %% To avoid race condition, send back Pid
    receive 
	inited -> {ok, Pid}
    after 5000 -> {error, {timeout, test_app}}
    end;
proc(Pid, Name, LDN, CVals, Ask) ->
    p(lightred, "=== erl_app === kill old process ~p~n", [Pid]),
    stop(Name),
    proc(undefined, Name, LDN, CVals, Ask).






%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% Init function for the Job process
%%========================================================================
init(TsPid, Name, LDN, CVals, Ask) ->
    p("=== APP(~p) ===  init pid: ~p   TS pid: ~p~n", [Name, self(), TsPid]),
    register(Name, self()),
    TsPid ! inited,
    AskPid = case Ask of
		 true -> TsPid;
		 _    -> undefined
	     end,
    loop(#loop{name   = Name, 
	       ldn    = LDN, 
	       cvals  = CVals,
	       ts_pid = AskPid}).




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% loop(#loop{}) 
%%
%% Job loop. Loop until the job is deleted.
%%========================================================================
loop(#loop{name         = Name,
	   pmi_mod      = PmiMod,
	   handle       = Handle,
	   ts_pid       = TsPid,
	   pm_groups    = PmGroups,
	   counter_spec = CounterSpec,
	   subscriber   = Subscriber,
	   ldn          = LDN,
	   cvals        = CVals} = Loop) ->
    p("### ERL_APP ~p    ~p###~nLoop ~p~n", [Name, Subscriber, Loop]),

    receive 
	%%===================================================
	%% messages from PMS
	%%===================================================
	{pmiSubscribe, {NewSubscriber, GP, NewCounterSpec} = Subscr} ->
	    {ok, _} = handle_subscribe(TsPid, Name, Subscr),
	    loop(Loop#loop{counter_spec = merge_cspecs(CounterSpec, 
						       [{GP, NewCounterSpec}]),
			   subscriber   = NewSubscriber});
	
	{pmiReport, {GP, TimeSpec, _DeadLine} = Report} ->
	    {ok, Res} = handle_report(TsPid, Name, Report),
	    send_pm_data({Name,
			  LDN, 
			  PmiMod,
			  Handle,
			  GP, 
			  Subscriber, 
			  TimeSpec, 
			  PmGroups, 
			  CounterSpec,
			  CVals},
			 Res),
	    loop(Loop);
	
	{pmiReportShowCounters, 
	 {NewSubscriber, RequestId, MoLdn, _MaxResponseTime} = Report} ->
	    p("### ERL_app pmiReportShowCounters ~p ###~n report: ~p~n", [Subscriber, Report]),
	    {ok, Res} = handle_report_sc(TsPid, Name, Report),
	    send_sc_data({Name, PmiMod, Handle, NewSubscriber, RequestId, 
			  MoLdn},
			 Res,
			 PmGroups),
	    loop(Loop);

	%%===================================================
	%% orders from the test suite
	%%===================================================
	{initialize, From, NewPmGroups} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~nPmGroups: ~p~n",
	      [Name, initialize, self(), NewPmGroups]),
	    Res = handle_initialize(pmsPmI, Name, NewPmGroups, pms_cli),
	    From ! {initialize_result, Res},
	    loop(Loop#loop{pm_groups = NewPmGroups});

	{Func, From, _Mod, _NewPmGroups} 
	  when Handle =/= undefined andalso 
	       (Func =:= initialize orelse Func =:= initialize_2) ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~nPmGroups: ~p~n",
	      [Name, initialize, self(), _NewPmGroups]),
	    From ! {initialize_result, {error, already_initialized}},
	    loop(Loop);

	{initialize, From, Mod, NewPmGroups} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~nPmGroups: ~p~n",
	      [Name, initialize, self(), NewPmGroups]),
	    {Res, H} = 
		handle_initialize(Mod, Name, NewPmGroups, ct:get_status()),
	    From ! {initialize_result, Res},
	    loop(Loop#loop{pm_groups = NewPmGroups,
			   pmi_mod   = Mod,
			   handle    = H});
	
	{initialize_2, From, Mod, [{_TLDN, NewPmGroups}] = RegData} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~nRegData: ~p~n",
	      [Name, initialize_2, self(), RegData]),
	    {Res, H} = 
		handle_initialize_2(Mod, Name, RegData, ct:get_status()),
	    p("initialize_2 Res ~p ~n", [Res]),
	    From ! {initialize_2_result, Res},
	    loop(Loop#loop{pm_groups = NewPmGroups,
			   pmi_mod   = Mod,
			   handle    = H});
	
	{finalize, From} ->
	    p("=== APP(~p) <--- SUITE === ~p ~p~n",
	      [Name, finalize, self()]),
	    Args = [self()],
	    Res = rct_rpc:call(nc1, pmsPmI, pmiFinalize, Args),
	    From ! {finalize_result, Res},
	    loop(Loop);
	
	{finalize, From, Mod} ->
	    p("=== APP(~p) <--- SUITE === ~n" "~p:pmiFinalize().~n",
	      [Name, Mod]),
	    Res = Mod:pmiFinalize(Handle),
	    From ! {finalize_result, Res},
	    loop(Loop);

	{delayed_pm_data, Data, Results} ->
	    p("=== APP(~p) === ~p ~p~n", 
	      [Name, self(), sending_delayed_pm_data]),
	    send_pm_data(Data, Results),
	    loop(Loop);

	{delayed_sc_data, Data, Results} ->
	    p("=== APP(~p) === ~p ~p~n", 
	      [Name, self(), sending_delayed_sc_data]),
	    send_sc_data(Data, Results, PmGroups),
	    loop(Loop);

 	{subscribe, Pid, GP, CounterSpec} ->
 	    p(" ===* ERL_APP ~p SUBSCRIBE ~p ~p~n", [Name, GP, CounterSpec]),
 	    Pid ! {subscribe_res, ok},
 	    loop(Loop);

	{ask, Pid} ->
	    p("=== APP(~p) === ~p ~p~n", [Name, self(), ask]),
	    loop(Loop#loop{ts_pid = Pid});

	{get_children, From, Mod} ->
	    Res = Mod:get_children(self()),
	    From ! {get_children_result, Res},
 	    loop(Loop);

	{kill, pmsPmI} ->
	    p("=== APP(~p) === ~p KILLED ~n", [Name, self()]),
	    unregister(Name),
	    ok;

	{kill, Mod} ->
	    p("=== APP(~p) === ~p KILLED ~n", [Name, self()]),
	    Mod:emulAppDeath(self(), true),
	    unregister(Name),
	    ok;

	{stop, UserPid} ->
	    p("=== APP(~p) === ~p STOPPED ~n", [Name, self()]),
	    unregister(Name),
	    UserPid ! stopped,
	    ok;
	X ->
	    p("=== APP(~p) === ~p UNKNOWN MESSAGE ~p ~n", [Name, self(), X]),
	    loop(Loop)
    end.


handle_initialize(pmsPmI = Mod, Name, NewPmGroups, no_tests_running) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "PmGroups: ~p~n",
      [Name, Mod, initialize, self(), NewPmGroups]),
    hi_res(Mod:pmiInitialize(NewPmGroups, ?MODULE, self()), Mod, local);
handle_initialize(pmsPmI = Mod, Name, NewPmGroups, _) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "PmGroups: ~p~n",
      [Name, Mod, initialize, self(), NewPmGroups]),
    Args = [NewPmGroups, pmsPmI, self()],
    hi_res(rct_rpc:call(testnode, Mod, pmiInitialize, Args), Mod, remote);
handle_initialize(Mod, Name, NewPmGroups, _) ->
    p("=== PMS <--- APP(~p) === ~n"
      "~p:pmiInitialize(~p).~n",
      [Name, Mod, NewPmGroups]),
    hi_res(Mod:pmiInitialize(NewPmGroups, ?MODULE, self()), Mod, local).

handle_initialize_2(pmsPmI = Mod, Name, NewPmGroups, no_tests_running) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "PmGroups: ~p~n",
      [Name, Mod, initialize_2, self(), NewPmGroups]),
    hi_res(Mod:pmiInitialize(NewPmGroups, ?MODULE, self()), Mod, local);
handle_initialize_2(pmsPmI = Mod, Name, NewPmGroups, _) ->
    p("=== PMS <--- APP(~p) === ~p ~p ~p~n"
      "PmGroups: ~p~n",
      [Name, Mod, initialize_2, self(), NewPmGroups]),
    Args = [NewPmGroups, pmsPmI, self()],
    hi_res(rct_rpc:call(testnode, Mod, pmiInitialize, Args), Mod, remote);
handle_initialize_2(Mod, Name, [{_TopMoLdn, _NewPmGroups}] = RegData, _) ->
    p("=== PMS <--- APP(~p) === ~n"
      "~p:pmiInitialize(~p).~n",
      [Name, Mod, RegData]),
    hi_res(Mod:pmiInitialize(RegData, ?MODULE, self()), Mod, 
	   local).
    %% hi_res(Mod:pmiInitialize_2(TopMoLdn, NewPmGroups, ?MODULE, self()), Mod, 
    %% 	   local).



hi_res(ok, _, _) -> 
    p("### hi_res ~p~n", [ok]),
    ok;
hi_res({ok, _} = Res, _, _) -> 
    p("### hi_res ~p~n", [Res]),
    Res;
hi_res({error, _} = Res, _, _) -> 
    p("### hi_res ~p~n", [ok]),
    {Res, undefined};
hi_res(Res, _Mod, local)  -> 
    p("### hi_res local ~p~n", [Res]),
    %%Mod:pmiFinalize(), %% Why?
    Res;
hi_res(Res, _Mod, remote) -> 
    p("### hi_res remote ~p~n", [Res]),
    %%rct_rpc:call(nc1, Mod, pmiFinalize, [self()]),
    Res.

%%========================================================================
%% 
%% 
%% 
%%========================================================================
handle_subscribe(undefined, Name, Subscr) ->
    p("=== PMS ---> APP(~p) === subscribe ~p~n~p~n  Not forwarded to SUITE",
      [Name, self(), Subscr]),
    {ok, ok};
handle_subscribe(TsPid, Name, {_NewSubscriber, GP, CounterSpecs} = Subscr) ->
    p("=== PMS ---> APP(~p) === subscribe ~p~n~p~n",
      [Name, self(), Subscr]),
    TsPid ! {subscribe, self(), Name, {GP, CounterSpecs}},
    Res = receive
	      {subscribe, R} -> 		  
		  p("=== APP(~p) <--- LIB === ~p subscribe reply~n"
		    "result: ~p~n", [Name, self(), R]),
		  {ok, R}
	  after ?TS_TO ->
		  p("=== APP(~p) === ~p no subscribe res from LIB~n"
		    "HORROR: ~p~n", [Name, self(), timeout]),
		  {error, timeout}
	  end,
    Res.
    
	    

%%========================================================================
%% 
%% 
%% 
%%========================================================================
handle_report(undefined, Name, Args) ->
    p("=== PMS ---> APP(~p) === report ~p~n~p~n  Not forwarded to SUITE",
      [Name, self(), Args]),
    {ok, ?REPORT_DEF_VALS};
handle_report(TsPid, Name, {_GP, _TimeSpec, _DeadLine} = Args) ->
    p("=== PMS ---> APP(~p) === ~p ~p~n~p~n",
      [Name, pmiReport, self(), Args]),
    TsPid ! {report, self(), Name, Args},
    Res = receive
	      {report, R} -> 		  
		  p("=== APP(~p) <--- LIB === ~p report reply~n"
		    "result: ~p~n", [Name, self(), R]),
		  {ok, R}
	  after ?TS_TO ->
		  p("=== APP(~p) === ~p no report reply from LIB~n"
		    "HORROR: ~p~n", [Name, self(), timeout]),
		  {ok, timeout}
	  end,
    Res.
    		 
	    
%%========================================================================
%% 
%% 
%% 
%%========================================================================
handle_report_sc(undefined, Name, Args) ->
    p("=== PMS ---> APP(~p) === reportShowCounters ~p~n~p~n  "
      "Not forwarded to SUITE",
      [Name, self(), Args]),
    {ok, ?REPORT_DEF_VALS};
handle_report_sc(TsPid, Name, Args) ->
    p("=== PMS ---> APP(~p) === ~p ~p~n~p~n",
      [Name, pmiReportShowCounters, self(), Args]),
    TsPid ! {report_sc, self(), Name, Args},
    Res = receive
	      {report_sc, R} -> 		  
		  p("=== APP(~p) <--- LIB === ~p reportShowCounters reply~n"
		    "result: ~p~n", [Name, self(), R]),
		  {ok, R}
	  after ?TS_TO ->
		  p("=== APP(~p) === ~p no reportShowCounters reply from LIB~n"
		    "HORROR: ~p~n", [Name, self(), timeout]),
		  {ok, timeout}
	  end,
    Res.
    		 

%%========================================================================
%% send_pm_data(Name, GP, Subscriber, TimeSpec, Groups) ->
%% 
%% 
%%========================================================================
send_pm_data(_Data, do_not_reply) ->
    ok;
send_pm_data(_Data, timeout) ->
    ok;
send_pm_data(Data, {wait, Wait, Results}) ->
    timer:send_after(Wait, self(), {delayed_pm_data, Data, Results});
send_pm_data(Data, {objects, Results}) ->
    spd_objects(Data, Results);
send_pm_data(Data, Results) when length(Results) > 1 ->
    [spd(Data, R) || R <- Results];
send_pm_data(Data, Res) ->
    spd(Data, Res).


spd_objects(_, []) ->
    ok;
spd_objects({Name, _, Mod, H, GP, Subsc, TS, PmGroups, CS, CVals} = Data,
	    [{Ldn, Results} | T])  when length(Results) > 1 ->
    Data2 = {Name, Ldn, Mod, H, GP, Subsc, TS, PmGroups, CS, CVals},
    [spd(Data2, R) || R <- Results],
    spd_objects(Data, T);
spd_objects({Name, _, Mod, H, GP, Subsc, TS, PmGroups, CS, CVals} = Data,
	    [{Ldn, Results} | T]) ->
    Data2 = {Name, Ldn, Mod, H, GP, Subsc, TS, PmGroups, CS, CVals},
    spd(Data2, Results),
    spd_objects(Data, T).
    


spd({_, [], _, _, _, _, _, _, _}, _) ->
    ok;
spd({Name, [{Ldn, CSpec} | T], Mod, H, GP, Subscriber, TimeSpec, PmGroups, 
     CounterSpec, CVals}, Res) ->
    spd({Name, Ldn, Mod, H, GP, Subscriber, TimeSpec, PmGroups, CSpec, CVals}, 
	Res),
    spd({Name, T, Mod, H, GP, Subscriber, TimeSpec, PmGroups, 
	 CounterSpec, CVals}, Res);
spd({Name, Ldn, Mod, H, GP, Subscriber, TimeSpec, PmGroups, CounterSpec, CVals},
    Res) ->
    GPSpec = proplists:get_value(GP, CounterSpec, []),
    Data = get_data(Res, GPSpec, CVals, PmGroups, []),
    case Mod of
	undefined -> 
	    Args = [Subscriber, GP, TimeSpec, Ldn, Data],
	    p("=== PMS <--- APP(~p) === ~p pmiData~n"
	      "~p:pmiData(~p, ~p, ~p, ~p, ~p).~n",
	      [Name, self(), pmsPmI, Subscriber, GP, TimeSpec, Ldn, Data]),
	    rct_rpc:call(nc1, pmsPmI, pmiData, Args);
	Mod -> 
	    p("=== PMS <--- APP(~p) === ~p pmiData~n"
	      "~p:pmiData(~p, ~p, ~p, ~p, ~p).~n",
	      [Name, self(), Mod, Subscriber, GP, TimeSpec, Ldn, Data]),
	    Mod:pmiData(H, GP, TimeSpec, Ldn, Data)
    end.


get_data(?REPORT_DEF_VALS, [], _, _, Acc) ->
    lists:reverse(Acc);
get_data(?REPORT_DEF_VALS = R, [{Grp, MT} | T], CVals, PmGroups, Acc) ->
    case lists:member(Grp, PmGroups) of
	true ->
	    get_data(R, T, CVals, PmGroups, [gd(Grp, MT, CVals, []) | Acc]);
	false ->
	    get_data(R, T, CVals, PmGroups, Acc)
    end;
get_data(Res, _, _, _, _) ->
    Res.

gd(Grp, [], _, Acc) ->
    {Grp, lists:reverse(Acc)};
gd(Grp, [MT | T], CVals, Acc) ->
    case proplists:get_value(MT, CVals) of
	Val when is_list(Val) ->
	    gd(Grp, T, CVals, [{MT, Val} | Acc]);
	_Undef ->
	    gd(Grp, T, CVals, [{MT, [1 + length(Acc)]} | Acc])
    end.


merge_cspecs(CounterSpec, NewCounterSpec) ->
    CSpec = [Spec || {GP, _} = Spec <- CounterSpec,
		     not lists:keymember(GP, 1, NewCounterSpec)],
    CSpec ++ NewCounterSpec.
			  

%%========================================================================
%% send_sc_data(Name, RequestId, MoLdn, MaxResponseTime, Groups) ->
%% 
%% 
%%========================================================================
send_sc_data(_Data, do_not_reply, _) ->
    ok;
send_sc_data(_Data, timeout, _) ->
    ok;
send_sc_data(Data, {wait, Wait, Results}, _) ->
    timer:send_after(Wait, self(), {delayed_sc_data, Data, Results});
send_sc_data(Data, Results, _) when length(Results) > 1 ->
    [sscd(Data, R) || R <- Results];
send_sc_data(Data, ?REPORT_DEF_VALS, [{_, PmGroups}]) ->
    sscd(Data, {?SC_OK, "", get_sc_res(PmGroups, [])});
send_sc_data(Data, Res, _) ->
    sscd(Data, Res).


sscd({Name, Mod, H, Subscriber, RequestId, _Ldn},
     {Result, ErrorStr, Meas}) ->
    case Mod of
	undefined -> 
	    Args = [Subscriber, RequestId, Meas],
	    p("=== PMS <--- APP(~p) === ~p pmiDataShowCounters 1~n"
	      "~p:pmiDataShowCounters(~p, ~p, ~p, ~p).~n",
	      [Name, 
	       self(), 
	       pmsPmI, 
	       Subscriber, 
	       RequestId, 
	       ErrorStr, 
	       Meas]),
	    rct_rpc:call(nc1, pmsPmI, pmiDataShowCounters, Args);
	Mod -> 
	    p("=== PMS <--- APP(~p) === ~p pmiDataShowCounters 2~n"
	      "~p:pmiDataShowCounters(~p, ~p, ~p, ~p).~n",
	      [Name, self(), Mod, Subscriber, RequestId, ErrorStr, Meas]),
	    Mod:pmiDataShowCounters(H, 
				    RequestId, 
				    Result, 
				    ErrorStr, 
				    Meas)
    end.


get_sc_res([], Acc) ->
    Acc;
get_sc_res(["Group1" | T], Acc) ->
    get_sc_res(T, [{"Type1", [1]} | Acc]);
get_sc_res(["Group2" | T], Acc) ->
    get_sc_res(T, [{"Type2", [2]} | Acc]);
get_sc_res(["ROPGroup1" | T], Acc) ->
    get_sc_res(T, [{"ROPType1Sum", [3]}, {"ROPType2Avg", [4]} | Acc]);
get_sc_res(["ROPGroup2" | T], Acc) ->
    get_sc_res(T, [{"ROPType6Sum", [5]} | Acc]);
get_sc_res(["RopGroupMult" | T], Acc) ->
    get_sc_res(T, [{"ROPType11SumMult", [6,7,8]} | Acc]);
get_sc_res(["GroupNonvalid" | T], Acc) ->
    get_sc_res(T, [Acc]).



%%========================================================================
%% p(Str, Args) -> ok
%% 
%% Print function.
%%========================================================================
p(Str, Args) ->
    case ct:get_status() of
	List when is_list(List) -> pms_test_lib:ct_pal(Str, Args);
	_                       -> ok
    end.

p(Colour, Str, Args) ->
    case ct:get_status() of
	List when is_list(List) -> ct:pal(Colour, Str, Args);
	_                       -> ok
    end.



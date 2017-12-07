%% ===========================================================================
%% Copyright (c) Ericsson AB 2010 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% ===========================================================================
%%
%% @copyright Ericsson AB 2014-2016
%% @doc
%% @end

-module(pesDebug).
-vsn('/main/R3A/R5A/2').
-date('2016-04-05').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').

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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    --------------------------------------
%%% R1A/1      2012-01-18 uabesvi     Created
%%% ------------------------------------------------------------------------
%%%
%%% ------------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ------------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ------------------------------------------------------------------------

%% interface
-export([compiled/0]).
-export([compiled/1]).
-export([versions/0]).
-export([versions/1]).

-export([fragment/0]).
-export([procs/0]).
-export([tables/0]).
-export([tables/1]).
-export([app_aliases/0]).
-export([type_aliases/0]).

-export([help/0]).
-export([help/1]).


-include("pes.hrl").
-include("RcsPMEventM.hrl").

%% ===========================================================================
%% EXPORTED FUNCTIONS
%% ===========================================================================

help() ->
    p("~nType pesDebug:help(Function) "),
    p("to get help info about a specific Function.~n"),
    p("~n"),
    p("The following functions are supported:~n"),
    p("  compiled~n"),
    p("  versions~n"),
    p("  fragment~n"),
    p("  procs~n"),
    p("  tables~n"),
    p("  app_aliases~n"),
    p("  type_aliases~n"),
    p("  ~n").
    

help(compiled) ->
    p("~nPrints the compile time of all modules with a given prefix.~n"),
    p("arity/0 will print PES module info.~n"),
    p("E.g. pesDebug:compile().~n"),
    p("~n");
help(versions) ->
    p("~nPrints the compile time of all modules with a given prefix.~n"),
    p("arity/0 will print PES module info.~n"),
    p("E.g. pesDebug:versions().~n"),
    p("~n");
help(fragment) ->
    p("~nPrints the PmEventM fragment as stored in the DB.~n"),
    p("~n");
help(procs) ->
    p("~nPrints all PES internal process information.~n"),
    p("~n");
help(tables) ->
    p("~nPrints all PES internal tables.~n"),
    p("arity/1 will take a table name as parameter.~n"),
    p("~n");
help(app_aliases) ->
    p("~nPrints aliases that are used in PEI sessions.~n"),
    p("~n");
help(type_aliases) ->
    p("~nPrints aliases that are specified in the PES alias appdata files.~n"),
    p("~n");

help(X) ->
    p("~nUnknown function ~p~n", [X]).
    


p(S)    -> io:format(S).
p(S, A) -> io:format(S, A).

%% ===========================================================================
%% @spec compiled(Modules) -> ok
%%
%% where
%%     Modules = [Module] | Prefix
%%     Module  = atom()
%%     Prefix  = atom()
%% 
%% @doc
%% 
%% Prints the compile time for listed modules or modules starting with Prefix.
%% versions(pes) will print all the pes modules.
%% 
%% @end
%% ===========================================================================	
compiled(Modules)
  when is_list(Modules) ->
    attrs(Modules, fun compiled/2);
compiled(Prefix) ->
    compiled(modules(Prefix)).


compiled(Width, Mod) ->
    io:format(": ~*s~19s  ~s~n", [-Width,
                                  Mod,
                                  attr(Mod, time),
                                  opt(attr(Mod, date))]).


compiled() ->
    compiled(pes).


opt("-") ->
    "";
opt(D) ->
    "(" ++ D ++ ")".


%% ===========================================================================	
%% @spec versions(Modules) -> ok
%%
%% where
%%     Modules = [Module] | Prefix
%%     Module  = atom()
%%     Prefix  = atom()
%% 
%% @doc
%% 
%% Prints the versions of the specified modules.
%% versions(pes) will print all the pes modules.
%% @end
%% =========================================================================== 
versions(Modules) ->
    attrs(Modules, fun(W,M) -> attr(W, M, vsn, fun vsn/1) end),
    ok.


versions() ->
    [code:ensure_loaded(M) || M <- ?PES_MODS],
    versions(pes).

%% vsn/1

vsn("/main/" ++ V) ->
    V;
vsn(T) when is_atom(T) ->
    vsn(atom_to_list(T));
vsn(T) ->
    attr(T).

is_char(C) ->
    0 =< C andalso C < 256.




%% ===========================================================================
%% INTERNAL FUNCTIONS
%% ===========================================================================


%%% ----------------------------------------------------------
%%% # attrs(Modules|Prefix, Attr|FormatFun)
%%%
%%% Output: Number of modules listed.
%%%
%%% Description: List an attribute from module_info.
%%% ----------------------------------------------------------

attrs(Modules, Fun)
  when is_list(Modules) ->
    sep(),
    W = 2 + widest(Modules),
    N = lists:foldl(fun(M,A) -> Fun(W,M), A+1 end, 0, Modules),
    sep(),
    N;

attrs(Prefix, Fun) ->
    attrs(modules(Prefix), Fun).

%% attr/1

attr(T) when is_atom(T) ->
    atom_to_list(T);
attr(N) when is_integer(N) ->
    integer_to_list(N);
attr(V) ->
    case is_list(V) andalso lists:all(fun is_char/1, V) of
        true ->  %% string
            V;
        false ->
            io_lib:format("~p", [V])
    end.

%% attr/4

attr(Width, Mod, Attr, VFun) ->
    io:format(": ~*s~s~n", [-Width, Mod, attr(Mod, Attr, VFun)]).

attr(Mod, Attr, VFun) ->
    Key = key(Attr),
    try
        VFun(val(Attr, keyfetch(Attr, Mod:module_info(Key))))
    catch
        _:_ ->
            "-"
    end.

attr(Mod, Attr) ->
    attr(Mod, Attr, fun attr/1).

key(time) -> compile;
key(_)    -> attributes.

val(time, {_,_,_,_,_,_} = T) ->
    lists:flatten(io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                tuple_to_list(T)));
val(_, [V]) ->
    V.



%% sep/[01]

sep() ->
    sep($#).

sep(Ch) ->
    io:format("~c~65c~n", [Ch, $-]).


%% widest/1

widest(List) ->
    lists:foldl(fun widest/2, 0, List).

widest(T, Max)
  when is_atom(T) ->
    widest(atom_to_list(T), Max);

widest(T, Max)
  when is_integer(T) ->
    widest(integer_to_list(T), Max);

widest(T, Max)
  when is_list(T) ->  %% string
    max(length(T), Max).

%% keyfetch/2

keyfetch(Key, List) ->
    {Key,V} = lists:keyfind(Key, 1, List),
    V.


%%% ----------------------------------------------------------
%%% # modules(Prefix|Prefixes)
%%%
%%% Input: Prefix = atom()
%%%
%%% Description: Return the list of all loaded modules with the
%%%              specified prefix.
%%% ----------------------------------------------------------

modules(Prefix)
  when is_atom(Prefix) ->
    lists:sort(mods(Prefix));
modules(Prefixes)
  when is_list(Prefixes) ->
    lists:sort(lists:flatmap(fun modules/1, Prefixes)).

mods(Prefix) ->
    P = atom_to_list(Prefix),
    lists:filter(fun(M) ->
                         lists:prefix(P, atom_to_list(M))
                 end,
                 erlang:loaded()).




%% ===========================================================================
%% MISC FUNCTIONS
%% ===========================================================================





%%========================================================================
%% fragment() -> ok
%%
%% print PM fragment
%%========================================================================
fragment() ->
    io:format("~n   PmEventM - ~p~n",[{"1", "1", "1"}]),
    {ok, Prods} = pesDb:event_producer_match({"1", "1", "1"}),
    [producers(Prod) || Prod <- Prods],
    io:format("~n"),
%%     {ok, PmJobs} = pesDb:pm_job_match({"1", "1", "1"}),
%%     [pm_job(PmJob) || PmJob <- PmJobs],
%%     io:format("~n~n"),
    ok.

producers(#eventProducer{eventProducerId = Id}) ->
    io:format("      Producer - ~p~n", [Id]),
    {ok, Grps} = pesDb:event_group_match(Id),
    [grp(Grp) || Grp <- Grps],
    {ok, Filters} = pesDb:event_filter_match(Id),
    [filter(Filter) || Filter <- Filters],
    {ok, Jobs} = pesDb:event_job_match(Id),
    [job(Job) || Job <- Jobs],
    ok.

grp(#eventGroup{eventGroupId = Id}) ->
    io:format("         EventGroup - ~p~n", [Id]),
    {ok, Types} = pesDb:event_type_match(Id),
    [type(Type) || Type <- Types],
    ok.

    


job(#eventJob{eventJobId    = Id,
	      eventGroupRef = Grps,
	      eventTypeRef  = Types,
	      eventFilter   = Filters}) ->
    io:format("         EventJob - ~p~n", [Id]),
    job_grps(Grps),
    job_types(Types),
    [io:format("            EventFilter - ~p   ~p~n",
	       [N, V]) || {_, N, V} <- Filters],
    ok.
    

job_grps(undefined) ->
    ok;
job_grps([]) ->
    ok;
job_grps([H | T]) ->
    L = binary_to_list(H),
    [_, ME, _, SF, _, PM, _, Prod, _, Grp] = string:tokens(L, "=,"),
    io:format("            EventGroupRef - ~p~n", [{ME, SF, PM, Prod, Grp}]),
    job_grps(T).

job_types(undefined) ->
    ok;
job_types([]) ->
    ok;
job_types([H | T]) ->
    L = binary_to_list(H),
    [_, ME, _, SF, _, PM, _, Prod, _, Grp, _, Ty] = string:tokens(L, "=,"),
    io:format("            EventTypeRef - ~p~n", [{ME, SF, PM, Prod, Grp, Ty}]),
    job_types(T).



type(#eventType{eventTypeId = Id}) ->
    io:format("            EventType - ~p~n", [Id]).

filter(#eventFilterType{eventFilterTypeId = Id}) ->
    io:format("         EventFilterType - ~p~n", [Id]).





%%========================================================================
%% procs() -> ok
%%
%% print PM fragment
%%========================================================================
procs() ->
    procs(erlang:whereis(pesServer)).

procs(Server) when is_pid(Server) ->
    LD       = gen_server:call(Server, get_loop_data),
    ProdIds  = proplists:get_value(prod_ids, LD),
    ProdPids = [ProdPid || {_, ProdPid} <- ProdIds],

    Prods      = proplists:get_value(prod_ids, LD), 
    Actions    = proplists:get_value(actions, LD), 
    RemActions = proplists:get_value(rem_actions, LD), 
    MeData     = proplists:get_value(me_data, LD), 

    io:format("~n =====  Server ~p  =====~n"
	      "     Producers = ~p~n"
	      "     MeData    = ~p~n",
	      [Server, Prods, MeData]),

    case Actions of
	[] ->
	    ok;
	_ ->
	    io:format("            Actions = ~p~n", [Actions])
    end,
    case RemActions of
	[] ->
	    ok;
	_ ->
	    io:format("            Remaining Actions = ~p~n", [Actions])
    end,
    
    [procs_prods(Pid, Server) || Pid <- ProdPids],
%%     io:format("~n"),
    AppRegPid = erlang:whereis(pesAppRegistry),
    io:format("~n =====  AppReg  ~p ===== ~n", [AppRegPid]),
    proc_app_reg(ets:tab2list(pesAppRegPid)),
    ok;
procs(_) ->
    io:format("~n   PES Server not found. ~n").


procs_prods(ProdPid, Server) ->
    {MonitorsAll, _GrpId} = proc_info(ProdPid, pesProducer),
    Monitors = [M || {process, Pid} = M <- MonitorsAll, Pid /= Server],
    LD   = proc_loop(ProdPid),
    Id   = proplists:get_value(producer_id, LD), 
    Jobs = proplists:get_value(event_jobs,  LD), 
    {Server, _} = proplists:get_value(server,         LD),
    
     io:format("~n    =====  EventProducer ~p  =====~n"
	      "            ProducerId = ~p~n"
	      "            Jobs       = ~p~n"
	      "            Server     = ~p~n",
	      [ProdPid, Id, [J || {J, _} <- Jobs], Server]),
    
    remain(LD, [producer_id, 
		event_jobs,
		server]),

    [procs_event_job(Pid) || {process, Pid} <- Monitors].



procs_event_job(JobPid) ->
    {_, JobId} = proc_info(JobPid, pesJob),
    

    LD = proc_loop(JobPid),

    JobId     = proplists:get_value(job_id,    LD), 
    CurState  = proplists:get_value(job_state, LD), 
    ReqState  = proplists:get_value(req_state, LD), 
    Types     = proplists:get_value(types,     LD), 
    RP        = proplists:get_value(rp,        LD), 
    {Prod, _} = proplists:get_value(producer,  LD), 
    
    io:format("~n       =====  EventJob ~p  =====~n"
	      "               JobId     = ~p~n"
	      "               CurState  = ~p ~s~n"
	      "               ReqState  = ~p ~s~n"
	      "               Types     = ~p~n"
	      "               RP        = ~p ~s~n"
	      "               Producer  = ~p~n",
	      [JobPid, JobId, CurState, p_state(CurState),
	       ReqState, p_state(ReqState), Types, 
	       RP, p_period(RP), Prod]),

    remain(LD, [job_id, 
		job_state,
		req_state,
		types,
		rp,
		producer,
		job_rec]).



p_state(?JobState_ACTIVE)  -> "(ACTIVE)"; 
p_state(?JobState_STOPPED) -> "(STOPPED)";
p_state(_)                 -> "".

p_period(?TimePeriod_ONE_MIN)        -> "(ONE_MIN)";
p_period(?TimePeriod_FIVE_MIN)       -> "(FIVE_MIN)";
p_period(?TimePeriod_FIFTEEN_MIN)    -> "(FIFTEEN_MIN)";
p_period(?TimePeriod_THIRTY_MIN)     -> "(THIRTY_MIN)";
p_period(?TimePeriod_ONE_HOUR)       -> "(ONE_HOUR)";
p_period(?TimePeriod_TWELVE_HOUR)    -> "(TWELVE_HOUR)";
p_period(?TimePeriod_ONE_DAY)        -> "(ONE_DAY)";
p_period(?TimePeriod_TEN_SECONDS)    -> "(TEN_SEC)";
p_period(?TimePeriod_THIRTY_SECONDS) -> "(THIRTY_SEC)";
p_period(_)                          -> "".



proc_app_reg(AppRegs) ->
    par(lists:usort(AppRegs)).

par([]) ->
    ok;
par([{pesAppRegPid, AppJobPid, _Types, _Callbacks, SessPid} | T]) ->

    LD = proc_loop_host(AppJobPid),

    Types   = proplists:get_value(types,            LD),
    Cb      = proplists:get_value(pei_cb_mod,       LD),
    CbJob   = proplists:get_value(pei_event_job_cb, LD),
    CbMe    = proplists:get_value(pei_me_update_cb, LD),
    SessPid = proplists:get_value(session_pid,      LD),
    Vals    = proplists:get_value(last_job,         LD),

    io:format("~n    =====  AppJob ~p  =====~n"
	      "         Types   = ~p~n"
	      "         Cb      = {~p, ~p, ~p}~n"
	      "         SessPid = ~p~n"
	      "         LastMsg = ~p~n",
	      [AppJobPid, Types, Cb, CbJob, CbMe, SessPid, Vals]),

    remain(LD, [types,
		pei_cb_mod,
		pei_event_job_cb,
		pei_me_update_cb,
		session_pid,
		last_job]),
    
    par(T);
par([H | T]) ->
    AppJobPid = element(2, H),
    LD = proc_loop_host(AppJobPid),
    io:format("~n    =====  AppJob ~p  =====~n"
	      "         Unknown format ~n"
	      "         ~p~n",
	      [AppJobPid, LD]),
    par(T).


proc_info(Pid, Module) ->
    X = erlang:process_info(Pid, [monitors, dictionary]),
    Dict = proplists:get_value(dictionary, X),
    {Module, Name} = proplists:get_value(name, Dict),
    {proplists:get_value(monitors, X), Name}.
    
proc_loop(Pid) ->
    Pid ! {get_loop_data, self()},
    receive
	{loop_data, Loop} -> Loop
    after 5000 ->
	    timeout
    end.

proc_loop_host(Pid) ->
    pesAppJob:gld(Pid, self()),
    receive
	{loop_data, Loop} -> Loop
    after 5000 ->
	    timeout
    end.


%%========================================================================
%% tables(Tabs) -> ok
%% 
%% Tabs = [atom()]
%%
%% print table contents of the tables in Tabs.
%%========================================================================
tables() ->
    tables([eventProducer, 
	    eventJob, 
	    eventGroup,
	    pesAppRegPid,
	    pesAppRegType,
	    pesGroupAliases,
	    pesTypeAliases
	   ]).

tables(Tabs) ->
    io:format("~n"),
    [table(Tab, ets:tab2list(Tab), true) || Tab <- Tabs],
    ok.



table(Tab, [], Print) ->
    table_name(Tab, Print),
    io:format("~n");
table(Tab, [H|T], Print) ->
    [_ | List] = tuple_to_list(H),
    table_name(Tab, Print),
    Fields = table_fields(Tab),
    L = max_length(Fields, 0),
    [io:format("  ~-*s :  ~p~n", [L, K, V]) || {K, V} <- lists:zip(Fields, List)],
    case T of
	[] -> io:format("~n");
	_  -> io:format("  --------------------------------------"
			"---------------------~n")
    end,
    table(Tab, T, false).


table_name(Tab, true) ->
    io:format("=== ~p ===~n", [Tab]);
table_name(_, _) ->   
    ok.

table_fields(eventProducer)   -> record_info(fields, eventProducer);
table_fields(eventJob)        -> record_info(fields, eventJob);
table_fields(eventGroup)      -> record_info(fields, eventGroup);
table_fields(pesAppRegPid)    -> record_info(fields, pesAppRegPid);
table_fields(pesAppRegType)   -> record_info(fields, pesAppRegType);
table_fields(pesGroupAliases) -> record_info(fields, pesGroupAliases);
table_fields(pesTypeAliases)  -> record_info(fields, pesTypeAliases);
table_fields(_)               -> unknown_record.
    

max_length([], L) ->
    L;
max_length([H|T], L) ->
    case length(atom_to_list(H)) of
	GR when GR > L -> max_length(T, GR);
	_              -> max_length(T, L)
    end.

remain(LD, List) ->
    rem_p(lists:foldl(fun(K,A) -> proplists:delete(K,A) end, LD, List)).
    
rem_p([])  -> ok;
rem_p(Rem) -> io:format("Remaing loop data: ~p~n", [Rem]).



%%========================================================================
%% aliases() -> ok
%% 
%% print pm group aliases 
%%========================================================================
app_aliases() ->
    app_aliases(ets:tab2list(pesAppRegPid)).

app_aliases(E) ->
    Pids = [{Pid, ET} || #pesAppRegPid{event_types = ET, job_pid = Pid} <- E],
    io:format("~n--- pesAppJob ---~n"),
    aliases_p(asort(Pids, [])).



aliases_p([]) ->
    ok;
aliases_p([{Pid, _Types} | T]) ->
    LD = proc_loop_host(Pid),

    Types = proplists:get_value(types, LD),

    io:format("  ~p~n", [Pid]),
    io:format("-----------------~n"),
    [io:format(" ~p   ~s~n", [A, Type]) || {Type, A} <- Types],
    aliases_p(T).


asort([], Acc) ->
    Acc;
asort([{Pid, Type} | T], Acc) ->
    case lists:keytake(Pid, 1, Acc) of
	{value, {Pid, Types}, RestAcc} ->
	    asort(T, [{Pid, [Type | Types]} | RestAcc]);
	_ ->
	    asort(T, [{Pid, [Type]} | Acc])
    end.


%%========================================================================
%% type_aliases() -> ok
%% 
%% get type aliases 
%%========================================================================
type_aliases() ->
    type_aliases(ets:tab2list(pesTypeAliases)).

type_aliases([]) ->
    ok;
type_aliases([#pesTypeAliases{eventMapId = MapId,
			      values     = Vals} | T]) ->
    
    Slav = lists:sort([{A1, T1} || {T1, A1} <- Vals]),
    io:format("~n--- ~s ---~n", [MapId]),
    [io:format(" ~p   ~s~n", [A2, T2]) || {A2, T2} <- Slav],
    type_aliases(T).

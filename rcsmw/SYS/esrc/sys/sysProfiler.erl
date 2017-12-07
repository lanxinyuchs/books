%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%%
%%% Author:          Vijay Lyengar
%%%
%%% Description:     This module implements a function trace tool.
%%%
%%%                  Refer to sysProfiler:help()
%%%
%%% ----------------------------------------------------------
-module(sysProfiler).
-id('682/190 55-LXA 119 1383 Ux').
-vsn('/main/R10A/1').
-date('2017-06-26').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/5 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        AD Date       Name     What
%%% -----      -- ---------- -------  ------------------------
%%% R6A/1      20 2007-11-02 uabesvi  Copied to agsa
%%% R6A/2      20 2007-11-02 qthupha  Minor changes with major impacts
%%% R14A/1        2010-05-03 etxrste  Replaced deprecated integer().
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% Control functions
-export([start/0,
         start_link/0,
	 go_func/1,
	 cast_mfa/3,
	 call_mfa/3,
	 stop/0,
	 help/0
	]).

%% Trace functions
-export([add/0]).
-export([add/1]).
-export([add/2]).
-export([analyse/0]).
-export([print/0]).
-export([print/1]).
-export([print/2]).
-export([seq/0]).
-export([seq/1]).
-export([seq/2]).
-export([show/0]).
-export([sort/0]).
-export([sort/1]).
-export([sort/2]).
-export([sort_n_show/0]).
-export([sort_n_show/1]).
-export([trace/1]).
-export([trace/2]).

-export([count/0,count/1,count/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% gen_server functions
-export([init/1,
         terminate/2,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
	 code_change/3
	]).


%%% ---------------------------------------------------------------------------
%%% #3.    CODE
%%% ---------------------------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ---------------------------------------------------------------------------

%%% ===========================================================================
%%% # start()
%%%
%%% Input:
%%%
%%%
%%% Output:      -
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
start() ->
    A = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    global:re_register_name(?MODULE, whereis(?MODULE)),
    A.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% ===========================================================================
%%% go_func(_Type)
%%%
%%% Input:
%%%
%%%
%%% Output:      -
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
go_func(_Type) ->
    global:re_register_name(?MODULE, whereis(?MODULE)),
    ok.

%%% ===========================================================================
%%% cast_mfa(M,F,A)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Generic cast to trace process
%%%
%%% ===========================================================================
cast_mfa(M,F,A) ->
    gen_server:cast({global, ?MODULE}, {cast_mfa, M, F, A}),
    ok.

%%% ===========================================================================
%%% call_mfa(M,F,A)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Generic call to trace process
%%%
%%% ===========================================================================
call_mfa(M,F,A) ->
    gen_server:call({global, ?MODULE}, {call_mfa, M, F, A}, 30000),
    ok.

%%% ===========================================================================
%%% help()
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
help() ->
    io:format(
      "~n"
      "~n"
      "With sysProfiler you can get traces on {Module, Function, Arity} level~n"
      "without all parameter values.~n"
      "It is handy if you just want to see the sequence of function calls.~n"
      "~n"
      "You can also get time stamps on the funcions to find possible~n"
      "bottle necks.~n"
      "~n"
      "~n"
      "Follow the instructions below:~n"
      "~n"
      "1. Start genny~n"
      "--------------~n"
      "sysProfiler:start().~n"
      "~n"
      "2. Choose tracing~n"
      "-----------------~n"
      "Trace on all global (exported) functions in modules~n"
      "starting with PreFix (e.g. 'otp'):~n"
      "sysProfiler:trace(PreFix).~n"
      "~n"
      "Trace on all global (exported) or local functions in Module: ~n"
      "sysProfiler:trace(Module, g).~n"
      "sysProfiler:trace(Module, l).~n"
      "~n"
      "Trace on global (exported) or local Function in Module: ~n"
      "sysProfiler:trace(Module, Function, g).~n"
      "sysProfiler:trace(Module, Function, l).~n"
      "~n"
      "There is one specially designed call to trace on oab and its external calls:~n"
      "oabDbg:trace(oab_all).~n"
      "3. Start tracing~n"
      "----------------~n"
      "sysProfiler:trace(start).~n"
      "~n"
      "4. Tracing~n"
      "----------~n"
      "Run your test cases.~n"
      "~n"
      "5. Stop tracing~n"
      "---------------~n"
      "sysProfiler:trace(stop).~n"
      "~n"
      "5. Analyse tracing~n"
      "------------------~n"
      "sysProfiler:analyse().~n"
      "~n"
      "6. Print trace results~n"
      "----------------------~n"
      "The default maximum lines to be printed is 100.~n"
      "~n"
      "Block    - atom()     Counting total time for this Block, e.g. 'oab'.~n"
      "MaxLines - integer()  Specified maximum lines to be printed.~n"
      "StartPos - integer()  Print start position in the trace.~n"
      "~n"
      "Print the function calls sequentially without time stamps.~n"
      "sysProfiler:print().~n"
      "sysProfiler:print(MaxLines).~n"
      "sysProfiler:print(StartPos, MaxLines).~n"
      "~n"
      "Print the function calls sequentially with time staps.~n"
      "sysProfiler:seq().~n"
      "sysProfiler:seq(Block).~n"
      "sysProfiler:seq(MaxLines).~n"
      "sysProfiler:seq(StartPos, MaxLines).~n"
      "sysProfiler:seq(MaxLines, Block).~n"
      "sysProfiler:seq(StartPos, MaxLines, Block).~n"
      "~n"
      "Print the function calls sorted after the most time consuming.~n"
      "sysProfiler:sort().~n"
      "sysProfiler:sort(Block).~n"
      "sysProfiler:sort(MaxLines).~n"
      "sysProfiler:sort(MaxLines, Block).~n"
      "~n"
      "Print the function calls sorted after the most time consuming,~n"
      "all calls to the same function added together.~n"
      "sysProfiler:add().~n"
      "sysProfiler:add(Block).~n"
      "sysProfiler:add(MaxLines).~n"
      "sysProfiler:add(MaxLines, Block).~n"
      "~n"
      "Print the function calls sorted after the most called functions,~n"
      "sysProfiler:count().~n"
      "sysProfiler:count(Block).~n"
      "sysProfiler:count(MaxLines).~n"
      "sysProfiler:count(MaxLines, Block).~n"
      "~n"
      "~n"
      "7. Stop genny~n"
      "-------------~n"
      "sysProfiler:stop().~n"
      "~n"
      "~n").

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ===========================================================================
%%% gen_server functions
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================

%%================================================
%% init
%%================================================
init(_) ->
    ets:new(trace_table, [ordered_set, public, named_table]),
    {ok, []}.

%%================================================
%% terminate
%%================================================
terminate(Reason, _L) ->
io:format(" sysProfiler has EXITED  ~p ~n ", [[Reason]]).

%%================================================
%% stop
%%================================================
stop() ->
    gen_server:call(sysProfiler, stop).

%%================================================
%% handle_cast
%%================================================
handle_cast({cast_mfa, M, F, A}, L) ->
    case (catch apply(M, F, A)) of
	{'EXIT', Reason} ->
	   io:format(" sysProfiler has EXITED  ~p ~n ", [[Reason]]),
           ok;
	_ ->
	    ok
    end,
    {noreply, L};
handle_cast(_X, L) ->
    {noreply, L}.

%%================================================
%% handle_call
%%================================================
handle_call({call_mfa, M, F, A}, _From, L) ->
    case (catch apply(M, F, A)) of
	{'EXIT', Reason} ->
	    io_lib:format("~p~n", [{'EXIT', Reason}]),
	    {noreply, L};
	{noreply, State} ->
	    {noreply, State};
	Result ->
	    {reply, Result, L}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, State}.


%%================================================
%% handle_info
%%================================================
handle_info(X, L) ->
    put_in_table(X),
    {noreply, L}.

put_in_table({trace_ts, Pid, CallOrReturn, MFA, {MegS, S, MicS}}) ->
    ets:insert(trace_table, {{MegS, (S*1000000+MicS)}, Pid, CallOrReturn, MFA});
put_in_table({trace_ts, Pid, CallOrReturn, MFA, _, {MegS, S, MicS}}) ->
    ets:insert(trace_table, {{MegS, (S*1000000+MicS)}, Pid, CallOrReturn, MFA}).

%%================================================
%% code_change
%%================================================
code_change(_OldModuleVsn, State, _Extra) ->
{ok, State}.





%%% #---------------------------------------------------------
%%% #3.3   CODE FOR TRACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ===========================================================================
%%% add(_)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Print the function calls sorted after the most time consuming,
%%%              all calls to the same function added together.
%%% ===========================================================================
add() -> add(100, void).

add(Max) when is_integer(Max)  -> add(Max, void);
add(Block) when is_atom(Block) -> add(100, Block);
add(_) ->
    io:format("Parameter must be either integer for maximum no of lines or"
	      "atom for block name.~n").

add(Max, Block) when is_integer(Max),
		     is_atom(Block) ->
    List = ets:tab2list(time_table),
    SortList = add_fncs(lists:keysort(2, List), []),
    print_head("Total time spent in each function", List, Block),
    [print_mfa(A)
     || A <- lists:sublist(lists:reverse(lists:keysort(3, SortList)), Max)],
    ok;
add(_,_) ->
    io:format("First parameter must be an integer for maximum no of lines,~n"
	      "second parameter must be an atom for block name.~n").

add_fncs([], Acc) ->
    Acc;
add_fncs([{X, MFA, T1}, {_, MFA, T2} | Rem], Acc) ->
    add_fncs([{X, MFA, T1+T2} | Rem], Acc);
add_fncs([H | Rem], Acc) ->
    add_fncs(Rem, [H | Acc]).

count() -> count(100, void).

count(Max) when is_integer(Max)  -> count(Max, void);
count(Block) when is_atom(Block) -> count(100, Block);
count(_) ->
    io:format("Parameter must be either integer for maximum no of lines or"
	      "atom for block name.~n").

count(Max, Block) when is_integer(Max),
		       is_atom(Block) ->
    io:format("~n========================================================~n"
	      "Number of times a function is called~n"
	      "========================================================~n"),
    count2(Max, Block).

count2(Max, Block) when is_integer(Max),
			is_atom(Block) ->
    List = ets:tab2list(time_table),
    SortList = cnt(lists:keysort(2, List), []),
    [io:format("~*s~p~n", [-20, integer_to_list(C), MFA])
     || {MFA, C} <- lists:sublist(lists:reverse(lists:keysort(2, SortList)), Max)],
    ok;
count2(_, _) ->
    io:format("First parameter must be an integer for maximum no of lines,~n"
	      "second parameter must be an atom for block name.~n").

cnt([{start,MFA,C},{_,MFA,_}|T],Acc) -> cnt([{start,MFA,C+1}|T],Acc);
cnt([{start,MFA,C},{_,NewMFA,_}|T],Acc) -> cnt([{start,NewMFA,1}|T],[{MFA,C}|Acc]);
cnt([{_, MFA, _}, {_, MFA, _} | Rem], Acc) ->
    cnt([{start, MFA, 2} | Rem], Acc);
cnt([{_, MFA, _}, {_, NewMFA, _} | Rem], Acc) ->
    cnt([{start, MFA, 1}, {start, NewMFA, 1} | Rem], Acc);
cnt([{start, MFA, C}], Acc) ->
    [{MFA, C}|Acc];
cnt([{_, MFA, _}], Acc) ->
    [{MFA, 1}|Acc];
cnt([], Acc) ->
    Acc.


%%% ===========================================================================
%%% analyse()
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Analyse the traces
%%%
%%% ===========================================================================
analyse() ->
    catch ets:delete(counter_table),
    catch ets:delete(call_trace_table),
    catch ets:delete(function_table),
    catch ets:delete(time_table),

    ets:new(counter_table, [ordered_set, public, named_table]),
    ets:insert(counter_table, {count, 0}),
    ets:new(call_trace_table, [ordered_set, public, named_table]),
    ets:new(function_table, [ordered_set, public, named_table]),
    analyse(ets:tab2list(trace_table)),

    ets:new(time_table, [ordered_set, public, named_table]),
    time_table(ets:tab2list(trace_table)).


analyse([{Ts, Pid, call, MFA}|Rest]) ->
    ets:insert(call_trace_table, {{Ts, Pid, MFA}}),
    analyse(Rest);
analyse([{Ts, Pid, return_from, MFA}|Rest]) ->
    CallList = ets:tab2list(call_trace_table),
    RevCallList = lists:reverse(CallList),
    analyse_more({{Ts, Pid, MFA}}, RevCallList),
    analyse(Rest);
analyse([]) ->
    ok.


analyse_more({{{_R,RTs}, Pid, MFA}}, [{{{C, CTs}, Pid, MFA}}| _Rest]) ->
    Ts = RTs - CTs,
    ets:delete(call_trace_table, {{C, CTs}, Pid, MFA}),
    _Count = ets:update_counter(counter_table, count, +1),
    ets:insert(function_table, {{C, CTs}, MFA, Ts}),
    ok;
analyse_more({{{R,RTs}, Pid, MFA}}, [{{{_C, _CTs}, _Pid, _MFA}}| Rest]) ->
    analyse_more({{{R,RTs}, Pid, MFA}}, Rest).



%%% ===========================================================================
%%% print()
%%% print(Max)
%%% print(Max)
%%% print(Start, Max)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Print the function call trace without time stamps.
%%%
%%% ===========================================================================
print() -> print(1, 1, 100).

print(Max) when not is_integer(Max)  ->
    io:format("Number of lines to print must be an integer.~n");
print(Max) when Max > 0 ->
    print(1, 1, Max+1);
print(_) ->
    io:format("Number of lines to print must be at least 1.~n").

print(Start, Max) when not is_integer(Start), not is_integer(Max) ->
    io:format("Start and number of lines to print must be an integer.~n");
print(Start, Max) when Start > 0, Max > 0 ->
    print(1, Start, Max + Start);
print(Start, _) when Start > 0 ->
    io:format("Number of lines to print must be at least 1.~n");
print(_, _)  ->
    io:format("Start line must be at least 1.~n").

print(No, Start, End) ->
    io:format("~n========================================================~n"
	      "Function call trace~n"
	      "========================================================~n"),
    case ets:first(trace_table) of
	'$end_of_table' -> ok;
	{Meg, MicSecs}  -> put(call, 0),
			   put(return_to, 0),
			   print_trace_stack({Meg, MicSecs},
					     {Meg, MicSecs},
					     No,
					     Start,
					     End)
    end.

print_trace_stack(_Now, _, End, _, End) ->
    put(call, 0),
    ok;
print_trace_stack(_Now, '$end_of_table', _, _, _) ->
    put(call, 0),
    ok;
print_trace_stack({Meg, FirstMicSecs}, {Meg, NextMicSec}, No, Start, End)
  when No < Start ->
    print_mfa(false,
	      No,
	      {Meg, FirstMicSecs},
	      ets:lookup(trace_table, {Meg, NextMicSec})),
    print_trace_stack({Meg, FirstMicSecs},
		      ets:next(trace_table, {Meg, NextMicSec}),
		      No + 1,
		      Start,
		      End);
print_trace_stack({Meg, FirstMicSecs}, {Meg, NextMicSec}, No, Start, End)  ->
    print_mfa(true,
	      No,
	      {Meg, FirstMicSecs},
	      ets:lookup(trace_table, {Meg, NextMicSec})),
    print_trace_stack({Meg, FirstMicSecs},
		      ets:next(trace_table, {Meg, NextMicSec}),
		      No + 1,
		      Start,
		      End).




%%% ===========================================================================
%%% seq()
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Print the function calls sequentially with time stamps.
%%%
%%% ===========================================================================
seq() -> seq(1, 101, void).

seq(Max) when is_integer(Max)  -> seq(1, Max, void);
seq(Block) when is_atom(Block) -> seq(1, 101, Block);
seq(_) ->
    io:format("Parameter must be either integer for maximum no of lines or"
	      "atom for block name.~n").

seq(Max, Block) when is_integer(Max),
		     is_atom(Block) ->
    seq(1, Max+1, Block);
seq(Start, Max) when is_integer(Start),
		     is_integer(Max) ->
    seq(1, Max+1, void);
seq(_,_) ->
    io:format("First parameter must be an integer for maximum no of lines,~n"
	      "second parameter must be an atom for block name~n"
	      "or~n"
	      "First parameter must be an integer for start line,~n"
	      "second parameter must be an integer for maximum no of lines.~n").

seq(Start, Max, Block)
  when is_integer(Start),
       is_integer(Max),
       is_atom(Block) ->
    List = ets:tab2list(time_table),
    print_head("Sequencial print out", List, Block),
    [print_mfa(A) || A <- lists:sublist(List, Max)],
    ok;
seq(_,_,_) ->
    io:format("First parameter must be an integer for start line,~n"
	      "second parameter must be an integer for maximum no of lines.~n"
	      "third parameter must be an atom for block name.~n").

%%% ===========================================================================
%%% show()
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
show() ->
    List = ets:tab2list(function_table),
    io:format("~n========================================================~n"
	      "Time Spent          {MODULE,FUNCTION,ARITY}~n"
	      "========================================================~n"),
    [print_mfa(A) || A <- List],
    ok.

%%% ===========================================================================
%%% sort(_)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Print the function calls sorted after the most time consuming
%%%              functions.
%%% ===========================================================================
sort() -> sort(100, void).

sort(Max) when is_integer(Max)  -> seq(Max, void);
sort(Block) when is_atom(Block) -> seq(100, Block);
sort(_) ->
    io:format("Parameter must be either integer for maximum no of lines or"
	      "atom for block name.~n").

sort(Max, Block) when is_integer(Max),
		      is_atom(Block) ->
    List       = ets:tab2list(time_table),
    SortList   = lists:keysort(3, List),
    print_head("Sorted per time spent", SortList, Block),
    [print_mfa(A) || A <- lists:sublist(lists:reverse(SortList), Max)],
    ok;
sort(_,_) ->
    io:format("First parameter must be an integer for maximum no of lines,~n"
	      "second parameter must be an atom for block name.~n").

%%% ===========================================================================
%%% sort_n_show(_)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
sort_n_show() ->
    sort_n_show(10).

sort_n_show(Int) when is_integer(Int) ->
    List = ets:tab2list(function_table),
    Len  = length(List),
    if Int >= Len ->
	    sort_n_show(0, List);
       true ->
	    sort_n_show(Len - Int, List)
    end;
sort_n_show(all) ->
    sort_n_show(0, ets:tab2list(function_table)).

sort_n_show(Count, List) ->
    SortList    = lists:keysort(3, List),
    CountList = lists:nthtail(Count, SortList),
    io:format("~n========================================================~n"
	      "Time Spent          {MODULE,FUNCTION,ARITY}~n"
	      "========================================================~n"),
    [print_mfa(A) || A <- lists:reverse(CountList)],
    ok.


%%% ===========================================================================
%%% trace(_)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Order tracing.
%%%
%%% ===========================================================================
trace(start) ->
    sysProfiler:cast_mfa(erlang, trace, [all, true, [call, arity, timestamp]]);
trace(stop) ->
    sysProfiler:cast_mfa(erlang, trace_pattern, [{'_','_','_'}, false, [global]]),
    sysProfiler:cast_mfa(erlang, trace_pattern, [{'_','_','_'}, false, [local]]);
trace(X) ->
    %% try to load the module, of course this will not work if X is a prefix
    %%code:ensure_loaded(X), 
    case modules(X) of
	[] ->
	    io:format("No modules matched ~p.~n", [X]);
	Modules  ->
	    Return = [{'_', [],[{return_trace}]}],
	    [sysProfiler:cast_mfa(erlang,
			    trace_pattern,
			    [{A, '_', '_'}, Return, [global]]) || A <- Modules],
	    ok
    end.




trace(X, g)                 -> trace(X, global);
trace(X, l)                 -> trace(X, local);
trace(M, Z) when is_atom(M) -> trace({M, '_'}, Z);

trace({M,F}, Z)
  when Z == global;
       Z == local;
       is_atom(M) andalso is_atom(F) ->
    Return = [{'_', [],[{return_trace}]}],
    sysProfiler:cast_mfa(erlang, trace_pattern, [{M, F, '_'}, Return, [Z]]);

trace(_, _) -> help().


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ===========================================================================
%%% pad_spaces(Int)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description: Print a number of spaces.
%%%
%%% ===========================================================================
pad_spaces(0) ->
    ok;
pad_spaces(Int) ->
    io:format(" "),
    pad_spaces(Int-1).


%%% ===========================================================================
%%% print_mfa({_, MFA, Time})
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
print_mfa({_, MFA, Time}) ->
    io:format("~*s~p~n", [-20, integer_to_list(Time), MFA]),
    ok.




print_mfa(_, _, {_FirstMeg, _FirstMicSecs}, [{{_Meg, _MicSec}, in, _MFA}]) ->
    io:format("->~n"),
    ok;
print_mfa(_, _, {_FirstMeg, _FirstMicSecs}, [{{_Meg, _MicSec}, out, _MFA}]) ->
    put(call,0),
    io:format("<-~n"),
    ok;

print_mfa(false, _, _, [{_, _Pid, call, _}]) ->
    put(call, (get(call) +1));
print_mfa(false, _, _, [{_, _Pid, return_from, _}]) ->
    put(call, (get(call) -1));
print_mfa(true, No, _, [{_, _Pid, call, MFA}]) ->
    OldValue = put(call, (get(call) +1)),
    io:format("~*s ->", [5, integer_to_list(No)]),
    pad_spaces(OldValue),
    io:format(" ~w~n", [MFA]),
    ok;
print_mfa(true, No, _, [{_, _Pid, return_from, MFA}]) ->
    OldValue = put(call, (get(call) -1)),
    io:format("~*s <-", [5, integer_to_list(No)]),
    pad_spaces(OldValue-1),
    io:format(" ~w~n", [MFA]),
    ok.

%%% ===========================================================================
%%% print_mfa({_, MFA, Time})
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
print_head(Str, List, void = Block) ->
    {_BlockTime, TotalTime} = total_time(List, Block),
    io:format("~n========================================================~n"
	      "~s~n"
	      "~n"
	      "Total time : ~s~n~n"
	      "Time Spent          {MODULE,FUNCTION,ARITY}~n"
	      "========================================================~n",
	     [Str, integer_to_list(TotalTime)]);
print_head(Str, List, Block) ->
    {BlockTime, TotalTime} = total_time(List, Block),
    io:format("~n========================================================~n"
	      "~s~n"
	      "~n"
	      "Total time : ~s~n"
	      "Time in ~s: ~s~n~n"
	      "Time Spent          {MODULE,FUNCTION,ARITY}~n"
	      "========================================================~n",
	     [Str,
	      integer_to_list(TotalTime),
	      if is_atom(Block) -> atom_to_list(Block);
		 true           -> Block
	      end,
	      integer_to_list(BlockTime)]).

%%% ===========================================================================
%%% time_table(List)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
time_table([{_, _Pid, call, {oabDcMain,wait_loop,2}} | Rest]) ->
    time_table(Rest);
time_table([{{_, TsC}, Pid, call, MFA}, {{_,TsR} = Tag, Pid, return_from, MFA} | Rest]) ->
    ets:insert(time_table, {Tag, MFA, TsR - TsC}),
    time_table(Rest);
time_table([{{_, TsC}, _Pid, call, MFA}, {{_,TsR} = Tag, _, _, _} = Y | Rest]) ->
    T1 = TsR - TsC,
    T2 = time_table_more(Rest, MFA),
    ets:insert(time_table, {Tag, MFA, T1 + T2}),
    time_table([Y | Rest]);
time_table([{_, _Pid, return_from, _} | Rest]) ->
    time_table(Rest);
time_table([]) ->
    ok.


time_table_more([{{_, TsC}, _, _, _}, {{_,TsR}, _, return_from, MFA} | _], MFA) ->
    TsR - TsC;
time_table_more([_ | Rest], MFA) ->
    time_table_more(Rest, MFA);
time_table_more([], MFA) ->
    io:format("Missing return_from matching ~p~n",[MFA]),
    0.


%%% ===========================================================================
%%% total_time(List)
%%%
%%% Input:       -
%%%
%%% Output:      ok
%%%
%%% Exceptions:  -
%%%
%%% Description:
%%%
%%% ===========================================================================
total_time(List, Block) when is_atom(Block) ->
    total_time(List, atom_to_list(Block));

total_time(List, Block) ->
    CntTime = fun({_, {M,_,_}, T}, {Oab, Acc}) ->
		      case lists:prefix(Block, atom_to_list(M)) of
			  true  -> {T+Oab, T+Acc};
			  false -> {Oab,   T+Acc}
		      end
	      end,

    lists:foldl(CntTime, {0, 0}, List).




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

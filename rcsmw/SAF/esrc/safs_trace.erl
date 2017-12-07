%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2012. All Rights Reserved.
%% 
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%% 
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_trace.erl
%%
%% Description:
%%    This file contains trace functionality for the safs application
%%
%%--------------------------------------------------------------------
-module(safs_trace).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/0,
	 stop/0,
	 service_trace_points/1,
	 remove_service_trace_points/1,
	 tp/1,
	 ctp/1,
	 format/1,
	 format/2,
	 com_trace_groups/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 handle_debug/4
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the tracer
%%--------------------------------------------------------------------
start() ->
    F = fun({trace_ts,Pid,call,{M,F,A},_C,Ts}) -> 
		io:format("~s ==> (~w) ~w:~w/~w ~p~n", 
			  [ts(Ts),Pid,M,F,length(A), A]);
	   ({trace_ts,Pid,return_from,{M,F,A},R,Ts}) ->
		io:format("~s <== (~w) ~w:~w/~w ~p~n", 
			  [ts(Ts),Pid,M,F,A,R]);
	   ({trace,Pid,call,{M,F,A},_C}) -> 
		io:format("==> (~w) ~w:~w/~w ~p~n", 
			  [Pid,M,F,length(A), A]);
	   ({trace,Pid,return_from,{M,F,A},R}) ->
		io:format("<== (~w) ~w:~w/~w ~p~n", 
			  [Pid,M,F,A,R]);
	   ({drop, _}) -> ok;
	   (T) -> 
		io:format("~w\n", [T])
	end,

    case running() of
	false ->
	    {ok, _} = ttb:tracer(all,
				 [{shell, {only, F}}, 
				  {handler, {{?MODULE, handle_debug}, initial}}]),
	    init(),
	    {ok, _} = ttb:p(all, [timestamp, c]),
	    ok;
	true ->
	    {error, tracer_already_running}
    end.

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stops the tracer
%%--------------------------------------------------------------------
stop() ->
    ttb:stop().


%%--------------------------------------------------------------------
%% Function: service_trace_points/1
%% Description: 
%%--------------------------------------------------------------------
service_trace_points({imm_om, TL}) ->
    safs_trace:tp(trace_points(safs_imm_om, TL));
service_trace_points({imm_oi, TL}) ->
    safs_trace:tp(trace_points(safs_imm_oi, TL));
service_trace_points({ntf, TL}) ->
    safs_trace:tp(trace_points(safs_ntf, TL));
service_trace_points({imm_db, TL}) ->
    safs_trace:tp(trace_points(safs_imm_db, TL));
service_trace_points({com, TL}) ->
    trace_com(tp,TL).

trace_com(_,[]) ->
    ok;
trace_com(F, all) ->
    trace_com(F, com_trace_groups());
trace_com(F,[socket |L]) ->
    safs_trace:F(trace_points(safs_socket, all)),
    trace_com(F,L);
trace_com(F,[engine |L]) ->
    safs_trace:F(trace_points(safs_com, all)),
    safs_trace:F(trace_points(safs_com_net, all)),
    trace_com(F,L);
trace_com(F,[proxy |L]) ->
    safs_trace:F(trace_points(safs_com_inproxy, all)),
    trace_com(F,L);
trace_com(F,[om |L]) ->
    safs_trace:F(trace_points(safs_imm_om_com, all)),
    trace_com(F,L);
trace_com(F,[oi |L]) ->
    safs_trace:F(trace_points(safs_imm_oi_com, all)),
    trace_com(F,L);
trace_com(F,[ntf |L]) ->
    safs_trace:F(trace_points(safs_ntf_com, all)),
    trace_com(F,L);
trace_com(F,[ct |L]) ->
    safs_trace:F(trace_points(safs_imm_ct_com, all)),
    trace_com(F,L).

com_trace_groups() -> [socket, engine, proxy, om, oi, ntf, ct].

%%--------------------------------------------------------------------
%% Function: remove_service_trace_points/1
%% Description: 
%%--------------------------------------------------------------------
remove_service_trace_points({imm_om, TL}) ->
    safs_trace:ctp(trace_points(safs_imm_om, TL));
remove_service_trace_points({imm_oi, TL}) ->
    safs_trace:ctp(trace_points(safs_imm_oi, TL));
remove_service_trace_points({ntf, TL}) ->
    safs_trace:ctp(trace_points(safs_ntf, TL));
remove_service_trace_points({imm_db, TL}) ->
    safs_trace:ctp(trace_points(safs_imm_db, TL));
remove_service_trace_points({com, TL}) ->
    trace_com(ctp,TL).


%%--------------------------------------------------------------------
%% Function: trace_points/2
%% Description: 
%%--------------------------------------------------------------------
trace_points(Module, all) ->
    tps(Module, 
	Module:trace_groups(), 
	Module:trace_points_list());
trace_points(Module, L) when is_list(L) ->
    tps(Module, 
	L, 
	Module:trace_points_list()).

%%--------------------------------------------------------------------
%% Function: tps/3
%% Description: Get list of trace points 
%%--------------------------------------------------------------------
tps(M, SectionList, TpList) ->
    tps(M, SectionList, TpList, []).

tps(_M, [], _TpList, Acc) ->
    Acc;		     
tps(M, [Section |SectionList], TpList, Acc) ->
    tps(M, SectionList, TpList, 
	[ {M, F, A} || 
	    {F, A} <- case lists:keyfind(Section, 1, TpList) of 
			  {Section, L} -> L; 
			  _ -> throw({bad_key, Section}) 
		      end] ++ Acc).     

%%--------------------------------------------------------------------
%% Function: tp/1
%% Description: Set trace points
%%--------------------------------------------------------------------
tp([Func |Funcs]) ->
    tp(Func),
    tp(Funcs);
tp([]) -> ok;
tp({M, F, A}) -> do_tp(M, F, A);
tp({M, F}) -> do_tp(M, F, '_');
tp(M) -> do_tp(M, '_', '_').

do_tp(M, F, A) ->
    {ok, _} = ttb:tpl(M, F, A, [{'_', [], [{message, {caller}}, {return_trace}, {exception_trace}]}]).

%%--------------------------------------------------------------------
%% Function: ctp/1
%% Description: Clear trace points
%%--------------------------------------------------------------------
ctp([Func |Funcs]) ->
    ctp(Func),
    ctp(Funcs);

ctp([]) -> ok;
ctp({M, F, A}) -> ttb:ctp(M, F, A);
ctp({M, F}) -> ttb:ctp(M, F, '_');
ctp(M) -> ttb:ctp(M, '_', '_').


%%--------------------------------------------------------------------
%% Function: format/1/2
%% Description: 
%%--------------------------------------------------------------------
format(File) ->
    ttb:format(File).
format(File, Out) ->
    ttb:format(File, [{out, Out}]).


%%====================================================================
%% Callback function for call trace
%%====================================================================
handle_debug(Out,Trace,TI,initial) ->
    print_header(Out,TI),
    handle_debug(Out,Trace,TI,0);
handle_debug(_Out,end_of_trace,_TI,N) ->
    N;
handle_debug(Out,Trace,_TI,N) ->
    print_func(Out,Trace,N),
    N+1.


print_func(Out,{trace_ts,P,call,{M,F,A},C,Ts},N) ->
    io:format(Out,
	      "~w: ~s\n"
	      "Process   : ~w\n"
	      "Call      : ~w:~w/~w\n"
	      "Arguments : ~p\n"
	      "Caller    : ~w\n\n",
	      [N,ts(Ts),P,M,F,length(A),A,C]);
print_func(Out,{trace_ts,P,return_from,{M,F,A},R,Ts},N) ->
    io:format(Out, 
	      "~w: ~s\n"
	      "Process      : ~w\n"
	      "Return from  : ~w:~w/~w\n"
	      "Return value : ~p\n\n",
	      [N,ts(Ts),P,M,F,A,R]);
print_func(Out, {drop, X}, N) ->
    io:format(Out,
	      "~w:\n"
	      "Dropped ~p messages\n\n",
	      [N, X]).

%%====================================================================
%% Internal utility functions
%%====================================================================
ts({_, _, Micro} = Now) ->
    {{Y,M,D},{H,Min,S}} = calendar:now_to_local_time(Now),
    io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w,~6.6.0w",
		  [Y,M,D,H,Min,S,Micro]).


% diff({SMeg, SS, SMic},{EMeg, ES, EMic}) ->
%     (EMeg-SMeg)*1000000000000 + (ES-SS)*1000000 + (EMic-SMic).

init() ->
    ttb:write_trace_info(start_time, fun() -> os:timestamp() end).

print_header(Out, TI) ->
    {value,{node,[Node]}} = lists:keysearch(node,1,TI),
    case lists:keysearch(start_time,1,TI) of
	{value,{start_time,[ST]}} ->
	    io:format(Out,
		      "\nTracing started on node ~w at ~s\n",
		      [Node,ts(ST)]);
	false -> % in case this file was not loaded on the traced node 
	    io:format(Out,
		      "\nTracing from node ~w\n",
		      [Node])
    end.

running() ->
    case whereis(ttb) of
	undefined -> false;
	_Pid -> true
    end.

%%----------------------------------------------------------------------
%% END OF MODULE
%%----------------------------------------------------------------------

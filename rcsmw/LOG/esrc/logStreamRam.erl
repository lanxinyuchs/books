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
%% @hidden
%% @author uabesvi
%% @copyright Ericsson AB 2017
%% @doc
%% This module provides a RAM memory for Push LOG events
%% that could not be sent to the defined URL;
%% propably because TN is not up.
%%
%% It takes an attribute specifying the maximum Size of the RAM. 
%% If the size is exeeded oldest entries will be removed.
%% 
%% It provides also a write function and a function to read
%% the first entry in the RAM memory. The latter function
%% will also remove the entry. 
%% 
%% @end
%% ----------------------------------------------------------
%% #1.    REVISION LOG
%% ----------------------------------------------------------
%% Rev        Date       Name        What
%% -----      -------    --------    ------------------------
%% R10A/1     2017-06-27 uabesvi     Created
%% ----------------------------------------------------------

-module(logStreamRam).
-behaviour(gen_server).
-vsn('/main/R10A/R11A/1').
-date('2017-07-17').
-author('uabesvi').
-shaid('5ec403a880dd59bfb32e21ae7245dc2143f26d9a').



-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2]).


-export([start/2]).
-export([stop/2]).

-export([write_udp/2]).
-export([write_tls/2]).


%% Debug commands
-export([get_loop_data/1]).
-export([print_log/1]).



%% interface
%%-export([init/3]).


-include("log.hrl").



-define(CALL_TIMEOUT, 10000).
-define(CHECK_TIME,   100).
%%-define(CHECK_TIME,   10000).


%% record definitions.

-record(state, {name,          %% Log name
		server_pid,    %% logStreamSendServer PID
		size,          %% Size of RM
		udp_index = 0, %% Current index of the last entry
		udp_bytes = 0, %% Current number of bytes in the UDP RAM
		udp_tab,       %% UDP RAM table
		tls_index = 0, %% Current index of the last entry
		tls_bytes = 0, %% Current number of bytes in the TLS RAM
		tls_tab,       %% TLS RAM table
		timer_ref      %% Used if queues are not empty
	       }).

%%===========================================================================
%% EXPORTED FUNCTIONS
%%===========================================================================

%%===========================================================================
%% start
%%
%%       -> ok | {ok, Pid}
%%
%% Start File Log.
%% Spawns a process to handle the log messages.
%%
%%===========================================================================
%% @hidden
start(Name, {ServerPid, Size}) ->
    Module     = ?MODULE,
    Args       = {self(), Name, ServerPid, Size},
    GsOptions  = [],
    gen_server:start_link(Module, Args, GsOptions).


%%===========================================================================
%% stop(Pid) -> ok.
%%
%% Stop the process
%%
%%===========================================================================
%% @hidden
stop(Pid, Reason) ->
    try
	gen_server:call(Pid, {stop, Reason})
    catch exit:E ->
	    {error, E}
    end.

%%===========================================================================
%% write_udp(Pid, Name, Data) -> ok.
%%
%% Write an entry to the UDP RAM memory.
%%
%%===========================================================================
write_udp(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {write_udp, Data});
write_udp(_, _) ->
    ok.

%%===========================================================================
%% write_tls(Pid, Name, Data) -> ok.
%%
%% Write an entry to the TLS RAM memory.
%%
%%===========================================================================
write_tls(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {write_tls, Data});
write_tls(_, _) ->
    ok.




%% Test function
get_loop_data(Pid) ->
    try
	gen_server:call(Pid, get_loop_data)
    catch exit:E ->
	    {error, E}
    end.

%% Test function
print_log(Pid) ->
    try
	gen_server:call(Pid, print_log)
    catch exit:E ->
	    {error, E}
    end.


%%========================================================================
%% handle_call(Msg, From, State) -> {reply, Res, State}
%%========================================================================
%%-----------------------------
%% Stop the process
%%-----------------------------
handle_call({stop, Reason}, _From, State) ->
    {stop, stop_reason(Reason), ok, State};
%%-----------------------------
%% test functions
%%-----------------------------
handle_call(get_loop_data, _From, State) ->
    {reply, format_loop(State), State};
handle_call(print_log, 
	    _From, 
	    #state{name    = Name,
		   udp_tab = UdpTab,
		   tls_tab = TlsTab} = State) ->
    do_print_log(Name, "UDP", UdpTab),
    do_print_log(Name, "TLS", TlsTab),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


%%========================================================================
%% handle_cast(Msg, State) -> {noreply, State}
%%========================================================================
%%-----------------------------
%% UDP
%%-----------------------------
handle_cast({write_udp, Data}, State) ->
    {noreply, handle_write_udp(Data, State)};
%%-----------------------------
%% TLS
%%-----------------------------
handle_cast({write_tls, Data}, State) ->
    {noreply, handle_write_tls(Data, State)};
%%-----------------------------
%% test functions
%%-----------------------------
handle_cast(loop_data, 
	    #state{udp_tab = UdpTab,
		   tls_tab = TlsTab} = State) ->
    io:format("LOOP~n~p~n", [State]),
    io:format("UDP~n~p~n",  [ets:tab2list(UdpTab)]),
    io:format("TLS~n~p~n",  [ets:tab2list(TlsTab)]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
%%-----------------------------
%% retry to send any possible
%% entries in the RAM
%%-----------------------------
handle_info(check_time,
	    #state{udp_tab = UdpTab,
		   tls_tab = TlsTab} = State) ->
    {noreply, handle_resend(ets:info(UdpTab, size), 
			    ets:info(TlsTab, size),
			    State#state{timer_ref = undefined})};

handle_info(_Info, State) ->
    {noreply, State}.


%%========================================================================
%% code_change(OldVsn, State, Extra) -> {ok, State}
%%========================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%========================================================================
%% terminate(Reason, State) -> Reason
%%========================================================================
terminate(Reason, #state{}) ->
    Reason.



%%===========================================================================
%% init(Creator, Name, Options)
%%
%% The function gets executed in a process initiated by start.
%% Creates a file in
%%
%%===========================================================================
init([Name, ServerPid, Size]) ->
    UdpTab = ets:new(measured, [ordered_set]),
    TlsTab = ets:new(measured, [ordered_set]),
    {ok, #state{name       = Name,
		server_pid = ServerPid,
		size       = Size,
		udp_bytes  = 0,
		tls_bytes  = 0,
		udp_tab    = UdpTab,
		tls_tab    = TlsTab}}.



%%===========================================================================
%% INTERNAL FUNCTIONS
%%===========================================================================


%%===========================================================================
%% handle_write_udp(Data, State) -> State
%%
%% Data      = string()
%% State     = #state{}
%%===========================================================================
handle_write_udp(Data, #state{udp_tab   = LogTab,
			      udp_index = Index,
			      size      = Size,
			      udp_bytes = Bytes,
			      timer_ref = TimerRef} = State) ->
    ets:insert(LogTab, {Index, list_to_binary(Data)}),
    NewBytes    = ctrl_size(Bytes + length(Data), 
			    Size, 
			    LogTab,
			    ets:first(LogTab)),
    NewTimerRef = start_timer(TimerRef),
    State#state{udp_bytes = NewBytes,
		udp_index = Index + 1,
		timer_ref = NewTimerRef}.

%%===========================================================================
%% handle_write_tls(Data, State) -> State
%%
%% Data      = string()
%% State     = #state{}
%%===========================================================================
handle_write_tls(Data, #state{tls_tab   = LogTab,
			      tls_index = Index,
			      size      = Size,
			      tls_bytes = Bytes,
			      timer_ref = TimerRef} = State) ->
    ets:insert(LogTab, {Index, list_to_binary(Data)}),
    NewBytes    = ctrl_size(Bytes + length(Data),
			    Size,
			    LogTab,
			    ets:first(LogTab)),
    NewTimerRef = start_timer(TimerRef),
    State#state{tls_bytes = NewBytes,
		tls_index = Index + 1,
		timer_ref = NewTimerRef}.


%%===========================================================================
%% handle_resend(UdpTabSize, TlsTabSize, State) -> State
%%
%% Try to resend the events in the queues.
%%
%%===========================================================================
handle_resend(#state{udp_tab = UdpTab,
		     tls_tab = TlsTab} = State) -> 
    handle_resend(ets:info(UdpTab, size), 
		  ets:info(TlsTab, size),
		  State).


handle_resend(0, 0, State) ->
    State;
handle_resend(_, 0, #state{udp_tab = UdpTab} = State) ->
    hr_read(ets:first(UdpTab), udp, State);
handle_resend(_, _, #state{udp_tab = TlsTab} = State) ->
    hr_read(ets:first(TlsTab), tls, State).


hr_read('$end_of_table', _, State) -> 
    State;
hr_read(Index, 
	Tab, 
	#state{udp_tab    = UdpTab,
	       tls_tab    = TlsTab,
	       udp_bytes  = UdpBytes,
	       tls_bytes  = TlsBytes,
	       server_pid = Pid} = State) -> 
    CurrentTab = choose(Tab == udp, UdpTab, TlsTab), 
    [{_Index, Entry}] = ets:lookup(CurrentTab, Index),
    case logStreamSendServer:try_send(Pid, Entry) of
	ok when Tab == udp -> 
	    ets:delete(UdpTab, Index),
	    handle_resend(State#state{udp_bytes = UdpBytes - size(Entry)});
	ok when Tab == tls -> 
	    ets:delete(TlsTab, Index),
	    handle_resend(State#state{tls_bytes = TlsBytes - size(Entry)});
	error ->
	    State#state{timer_ref = start_timer(undefined)};
	{error, _} = Error ->
	    log(hr_read, Error),
	    State#state{timer_ref = start_timer(undefined)}
    end.




ctrl_size(Bytes, _, _, '$end_of_table') ->
    Bytes;
ctrl_size(Bytes, MaxSize, _, _) when Bytes < MaxSize ->
    Bytes;
ctrl_size(Bytes, MaxSize, LogTab, Index) ->
    First = read_and_delete(ets:lookup(LogTab, Index), LogTab),
    ctrl_size(Bytes - size(First), MaxSize, LogTab, ets:first(LogTab)).



read_and_delete([{Index, First}], LogTab) ->
    ets:delete(LogTab, Index),
    First.


stop_reason(deleted)    -> normal; 
stop_reason(terminated) -> normal;
stop_reason(Else)       -> Else.

start_timer(undefined) ->
    case timer:send_after(?CHECK_TIME, check_time) of
	{ok, TimerRef} -> TimerRef;
	_              -> undefined
    end;
start_timer(TimerRef) ->
    TimerRef.


choose(true,  T, _) -> T;
choose(false, _, F) -> F.

%%===========================================================================
%% Test functions
%%===========================================================================

log(_, _) ->
    ok.

format_loop(State) ->
    F       = record_info(fields, state),
    [_ | L] = tuple_to_list(State),
    lists:zip(F,L).

do_print_log(Name, Log, Tab) ->
    io:format("~n===== ~p  ~p =====~n~p~n~n"
	      "---- Tab size   ----~n~p~n~n",
	      [Name,
	       Log,
	       ets:select(Tab, [{{'$1'}, [], ['$1']}]),
	       ets:info(Tab, memory)]).


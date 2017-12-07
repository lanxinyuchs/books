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
%%-----------------------------------------------------------------
%% File: safs_com_inproxy.erl
%% 
%% Description:
%%    This file contains the "proxy" for incomming connections
%%
%%-----------------------------------------------------------------
-module(safs_com_inproxy).
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_internal.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start/1,
	 send/2,
	 send_receive/3,
	 trace_groups/0,
	 trace_points_list/0
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2, 
	 stop/1,
	 check_port/1
	]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {type, reply_fun, socket, callback, service_state}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the proxy
%%--------------------------------------------------------------------
start(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%--------------------------------------------------------------------
%% Function: send/2
%% Description: Sends a message
%%--------------------------------------------------------------------
send(Pid, Msg) ->
    cast(Pid, {send, Msg}).

%%--------------------------------------------------------------------
%% Function: send_receive/3
%% Description: Sends a message and receives a reply
%%--------------------------------------------------------------------
send_receive(Pid, Msg, ReplyFun) ->
    call(Pid, {send_receive, Msg, ReplyFun}).

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/1
%%-----------------------------------------------------------------
stop(Pid) ->
    cast(Pid, stop).

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [error].

trace_points_list() ->
    [
     {error, 
      [{safs_error, 2}]}     
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description: 
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Server functions
%%====================================================================
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({connect, {ConnectionType, Cb}, Socket}) ->
%%    ?INFO("safs_com_inproxy of type ~p starting\n", [Type]),
    process_flag(trap_exit, true),
    case apply(ConnectionType, init, [self(), Cb]) of
	{ok, SvcState} ->
	    {ok, #state{type = ConnectionType, socket = Socket, callback=Cb, 
			service_state = SvcState}};
	{error, Error} ->
	    {error, Error}
    end.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
%% We may want to kill all proxies before terminating, but the best
%% option should be to let the requests complete (especially for one-way
%% functions it's a better alternative.
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({send_receive, Msg, Replyfun}, _From, State) ->
    %%    io:format("Send Msg:\n~p\n\n", [Msg]),
    Socket = State#state.socket,
    case safs_socket:write(Socket, Msg) of
	ok ->
	    safs_socket:setopts(Socket, [{active, once}]),
	    {reply, ok, State#state{reply_fun = Replyfun}};
	_Error ->
	    {reply, {error, sa_ais_err_bad_handle}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(check_port, _From, State) ->
    {reply, erlang:port_info(State#state.socket), State};
handle_call(X, From, State) ->
    safs_error(handle_call, {"got unknown message", X, From}),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast({send, Msg}, State) ->
%%    io:format("Send Msg:\n~p\n\n", [Msg]),
    Socket = State#state.socket,
    safs_socket:write(Socket, Msg),
    safs_socket:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(X, State) ->
    safs_error(handle_cast, {"got unknown message", X}),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% Normal invocation
handle_info(set_active, State) -> 
    safs_socket:setopts(State#state.socket, [{active, once}]),
    {noreply, State}; 
handle_info({tcp, Socket, Bytes}, State) -> 
    handle_msg(Socket, Bytes, State);
%% Errors, closed connection
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, _Reason}, State) ->
    {stop, normal, State};
%% Servant termination.
handle_info({'EXIT', Pid, normal}, State) ->
    safs_error(handle_info, {"got exit signal", Pid}),
    {noreply, State}; 
handle_info(X, State) ->
    safs_error(handle_info, {"got unknown message", X}),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_msg/2
%%-----------------------------------------------------------------
handle_msg(Socket, Bytes, #state{type = Type, socket = Socket, service_state = SvcState} = State) ->
    NewState = 
	case State#state.reply_fun of
	    undefined ->
		{Ret, NewSvcState} = apply(Type, message, [Bytes, SvcState]),
		case Ret of
		    no_reply ->
			no_reply;
		    {error, Error} ->
			safs_error(handle_msg, {{Type, message}, Error});
		    RetBytes -> 
			safs_socket:write(Socket, RetBytes)
		end,
		State#state{service_state = NewSvcState};	
	    ReplyFun ->
		%%error_logger:info_msg("CB Message arrived\n"),
		case ReplyFun({ok, Bytes}) of
		    not_my_reply ->
			State;
		    _ ->
			State#state{reply_fun = undefined}
		end
	end,
    safs_socket:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_msg( _, Bytes, State) ->
    safs_error(handle_msg, {"got bytes on wrong socket", Bytes}),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_port(Pid) ->
    call(Pid, check_port).

%%====================================================================
%% Internal functions
%%====================================================================
call(Pid, Msg) ->
    try
	gen_server:call(Pid, Msg, infinity)
    catch
    	exit:{noproc, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{normal, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{killed, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{{shutdown, _},_} ->
    		      {error, sa_ais_err_bad_handle}
    end.

cast(Pid, Msg) ->
    try
	gen_server:cast(Pid, Msg)
    catch
    	exit:{noproc, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{normal, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{killed, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{{shutdown, _},_} ->
    		      {error, sa_ais_err_bad_handle}
    end.

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
%%----------------------------------------------------------------------
%% File: safs_com_net.erl
%%
%% Description:
%%    This file contains the communication server
%%
%%----------------------------------------------------------------------
-module(safs_com_net).
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_com.hrl").
-include("safs_internal.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 start/1,
	 connect/3,
	 trace_groups/0,
	 trace_points_list/0
	 % connections/0,
	 % sockname2peername/2,
	 % peername2sockname/2
	]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
	 init/1,
	 terminate/2,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3
	]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(CONNECTION_DB, safs_com_net_db).
-record(state, {ports=[], db}).
-record(listen, {pid, socket, port, type, options}).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: start/1
%% Description:
%%----------------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, safs_com_net}, safs_com_net, Opts, []).


%%----------------------------------------------------------------------
%% Function: connect/3
%% Description:
%%----------------------------------------------------------------------
connect(Type, S, AcceptPid) ->
    gen_server:call(safs_com_net, {connect, Type, S, AcceptPid}, infinity).


% connections() ->
%     do_select([{#connection{peerdata = '$1', _='_'}, [], ['$1']}]).

% sockname2peername(SockHost, SockPort) ->
%     do_select([{#connection{peerdata = '$1',
% 			    localdata = {match_type(SockHost),
% 					 match_type(SockPort)},
% 			    _='_'}, [], ['$1']}]).


% peername2sockname(PeerHost, PeerPort) ->
%     do_select([{#connection{peerdata = {match_type(PeerHost),
% 					match_type(PeerPort)},
% 			    localdata = '$1',
% 			    _='_'}, [], ['$1']}]).

% do_select(Pattern) ->
%     case catch ets:select(?CONNECTION_DB, Pattern) of
% 	{'EXIT', _What} ->
% 	    [];
% 	Result ->
% 	    Result
%     end.

% match_type(0) ->
%     %% Wildcard port number
%     '_';
% match_type("") ->
%     %% Wildcard host
%     '_';
% match_type(Key) ->
%     %% Wildcard not used.
%     Key.
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

%%----------------------------------------------------------------------
%% Server functions
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func: init/1
%%----------------------------------------------------------------------
init(Options) ->
    process_flag(trap_exit, true),
    {ok,
     parse_options(Options, #state{db = ets:new(?CONNECTION_DB, [set, public,
								 named_table,
								 {keypos, 2}])})}.

%%----------------------------------------------------------------------
%% Func: terminate/1
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: parse_options/2
%%----------------------------------------------------------------------
% get_options(_Type, _Options) ->
%     [].

%%----------------------------------------------------------------------
%% Func: parse_options/2
%%----------------------------------------------------------------------
parse_options([{port, Type, IpPort} | Rest], State) ->
    {Port, ListenOptions} = get_port_options(IpPort),
    case safs_socket:listen(Port, ListenOptions) of
	{ok, Listen} ->
	    {ok, Pid} = safs_com_socketsup:start_accept(Listen, Type),
	    link(Pid),
	    ets:insert(?CONNECTION_DB, #listen{pid = Pid, socket = Listen,
					       port = Port, type = Type,
					       options = ListenOptions});
	Error ->
	    safs_error(parse_options, {"safs_socket:listen/2:", Error}),
	    error_logger:error_msg("safs_com_net:parse_options~n"
				   "safs_socket:listen/2 returned~p~n",
				   [Error])
    end,
    parse_options(Rest, State);
parse_options([], State) ->
    State.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%%----------------------------------------------------------------------
% handle_call({remove, Ref}, _From, State) ->
%     case do_select([{#listen{ref = Ref, pid = '$1', socket = '$2',
% 			     type = '$3', _='_'}, [], [{{'$1', '$2', '$3'}}]}]) of
% 	[{Pid, Listen, Type}|_] when is_pid(Pid) ->
% 	    unlink(Pid),
% 	    ets:delete(?CONNECTION_DB, Pid),
% 	    %% Just close the listen socket. Will cause the accept processs
% 	    %% to terminate.
% 	    safs_socket:close(Type, Listen),
% 	    stop_proxies(do_select([{#connection{ref = Ref, pid = '$1', _='_'},
% 				     [], ['$1']}])),
% 	    {reply, ok,
% 	     State#state{queue =
% 			 from_list(
% 			   lists:keydelete(Pid, 1,
% 					   queue:to_list(State#state.queue)))}};
% 	_ ->
% 	    {reply, ok, State}
%     end;
% handle_call({add, IP, Type, Port, Options}, _From, State) ->
%     Family = safs_env:ip_version(),
%     case inet:getaddr(IP, Family) of
% 	{ok, IPTuple} ->
% 	    try [{ip, IPTuple} |get_options(Type, AllOptions)] of
% 		Options ->
%      	            Ref = make_ref(),
% 	            case safs_socket:listen(Type, Port, Options, false) of
% 		        {ok, Listen, NewPort} ->
% 		            {ok, Pid} = safs_com_socketsup:start_accept(Type, Listen, Ref,
% 									    AllOptions),
% 		            link(Pid),
% 		            ets:insert(?CONNECTION_DB, #listen{pid = Pid,
% 						               socket = Listen,
% 						               port = NewPort,
% 						               type = Type, ref = Ref,
% 						               options = Options,
% 						               proxy_options = AllOptions}),
% 		            {reply, {ok, Ref}, State};
% 	          	Error ->
% 		            {reply, Error, State}
% 	            end
%             catch
% 		error:Reason ->
% 		    {reply, {error, Reason}, State}
% 	    end;
% 	Other ->
% 	    {reply, Other, State}
%     end;
handle_call({connect, Type, Socket, _AcceptPid}, _From, State) ->
    % ?INFO("safs_com_net got connection to ~p service\n", [Type]),
    case safs_com_insup:start_connection(Type, Socket) of
	{ok, Pid} when is_pid(Pid) ->
	    link(Pid),
	    {reply, {ok, Pid, true}, State};
	Other ->
	    {reply, Other, State}
    end;
handle_call(_, _, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------
%% Standard gen_server cast handle
%%-----------------------------------------------------------------
handle_cast(_, State) ->
    {noreply,  State}.

%%-----------------------------------------------------------------
%% Standard gen_server handles
%%-----------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    case ets:lookup(?CONNECTION_DB, Pid) of
	[#listen{pid = Pid, socket = Listen, port = Port, type = Type,
		 options = Options}] ->
	    ets:delete(?CONNECTION_DB, Pid),
	    unlink(Pid),
	    {ok, NewPid} = safs_com_socketsup:start_accept(Listen, Type),
	    link(NewPid),
	    ets:insert(?CONNECTION_DB, #listen{pid = NewPid, socket = Listen,
					       port = Port, type = Type,
					       options = Options}),
	    % ?INFO("safs_com_net accept process restarted: ~p\n", [NewPid]),
	    %% Remove the connection if it's in the queue.
	    {noreply, State};
	[] ->
	    {noreply, State}
    end;
handle_info(_, State) ->
    {noreply,  State}.


%%----------------------------------------------------------------------
%% Func: code_change/3
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%======================================================================
%% Internal Functions
%%======================================================================

%%----------------------------------------------------------------------
%% Func: code_change/1
%%----------------------------------------------------------------------
get_port_options({Ip, Port}) ->
    {Port, [{ip, Ip}]};
get_port_options(Port) when is_integer(Port) ->
    {Port, []}.
    
%%----------------------------------------------------------------------
%% END OF MODULE
%%----------------------------------------------------------------------



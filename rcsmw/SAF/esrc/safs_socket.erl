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
%% File: safs_socket.erl
%% 
%% Description:
%%    This file contains a standard interface to the sockets to handle the differences
%%    between the implementations used.
%%
%%-----------------------------------------------------------------
-module(safs_socket).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_internal.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start/0, 
	 connect/3, 
	 listen/2, 
	 accept/1, 
	 write/2,
	 controlling_process/2, 
	 close/1, 
	 peername/1, 
	 sockname/1, 
%	 peerdata/1, 
%	 sockdata/1, 
	 setopts/2,
	 trace_groups/0,
	 trace_points_list/0
	 % clear/1, 
	 % shutdown/2
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
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
%% Description: 
%%--------------------------------------------------------------------
start() ->	
    inet_db:start().

%%-----------------------------------------------------------------
%% Invoke the required setopts (i.e., inet or ssl)
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

%%-----------------------------------------------------------------
%% Connect to Port at Host, in order to 
%% establish a connection.
%%
connect(Host, Port, Options) ->
     Timeout = 5000,
     % Options1 = check_options(Options),
     Options1 = [{active, once},
		 {packet, 4}, 
		 binary |
		 Options],
     case catch gen_tcp:connect(Host, Port, Options1, Timeout) of
 	{ok, Socket} ->
 	    {ok, Socket};
 	{error, Reason} ->
 	    safs_error(connect, {"gen_tcp:connect/4:", Reason}),
	    {error, Reason}
     end.


%%-----------------------------------------------------------------
%% Create a listen socket at Port in CDR mode for 
%% data connection.
%%
listen(Port, Options) ->
    % Options1 = check_options(Options),
    Keepalive = true, %safs_env:keepalive(),

    case catch gen_tcp:listen(Port, [{active, false}, 
				     {packet, 4}, 
				     binary, 
				     {keepalive, Keepalive},
				     {reuseaddr,true} |
				     Options]) of
	{ok, ListenSocket} ->
	    {ok, ListenSocket};
	{error, Reason} ->
 	    safs_error(listen, {"gen_tcp:listen/2:", Reason}),
	    {error, Reason}
    end.

%%-----------------------------------------------------------------
%% Wait in accept on the socket
%% 
accept(ListenSocket) ->
    case catch gen_tcp:accept(ListenSocket) of
	{ok, S} ->
	    {ok, S};
	Error ->
 	    safs_error(accept, {"gen_tcp:accept/1:", Error}),
	    Error
    end.

%%-----------------------------------------------------------------
%% Close the socket
%% 
close(Socket) ->
    (catch gen_tcp:close(Socket)).

%%-----------------------------------------------------------------
%% Write to socket
%% 
write(Socket, Bytes) ->
    gen_tcp:send(Socket, Bytes).

%%-----------------------------------------------------------------
%% Change the controlling process for the socket
%% 
controlling_process(Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid).

%%-----------------------------------------------------------------
%% Get peername
%% 
peername(Socket) ->
    inet:peername(Socket).

%%-----------------------------------------------------------------
%% Get peerdata
%% 
% peerdata(Socket) ->
%     create_data(inet:peername(Socket)).

%%-----------------------------------------------------------------
%% Get sockname
%% 
sockname(Socket) ->
    inet:sockname(Socket).

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

%%-----------------------------------------------------------------
%% Get sockdata
%% 
% sockdata(Socket) ->
%     create_data(inet:sockname(Socket)).


% create_data({ok, {Addr, Port}}) ->
%     {sock_srv_env:addr2str(Addr), Port};
% create_data(_What) ->
%     {"Unable to lookup peer- or sockname", 0}.


%%-----------------------------------------------------------------
%% Shutdown Connection
%% How = read | write | read_write
% shutdown(Socket, How) ->
%     gen_tcp:shutdown(Socket, How).

%%-----------------------------------------------------------------
%% Remove Messages from queue
%%
% clear(Socket) ->
%      tcp_clear(Socket).

%% Inet also checks for the following messages:
%%  * {S, {data, Data}}
%%  * {inet_async, S, Ref, Status}, 
%%  * {inet_reply, S, Status}
%% SSL doesn't.
% tcp_clear(Socket) ->
%     receive
%         {tcp, Socket, _Data} ->
%             tcp_clear(Socket);
%         {tcp_closed, Socket} ->
%             tcp_clear(Socket);
%         {tcp_error, Socket, _Reason} ->
%             tcp_clear(Socket)
%     after 0 -> 
%             ok
%     end.



%%-----------------------------------------------------------------
%% Check Port. If the user supplies 0 we pick any vacant port. But then
%% we must change the associated environment variable
% check_port(0, Socket) ->
%     case inet:port(Socket) of
% 	{ok, Port} ->
% 	    %% safs:configure_override(port, Port),
% 	    Port;
% 	What ->
% 	    What
%     end;
% check_port(Port, _) ->
%     Port.

%%-----------------------------------------------------------------
%% Check Options. 
% check_options(Options) ->
%     [safs:ip_version()|Options].


-module(eTorrentListen).

%% API

-export([get_port/0]).

%% Supervisor interface
-export([start_link/0]).

%% Sys interface
-export([system_code_change/4, system_continue/3, system_terminate/4]).

%% Internal
-export([init/0]).

-include("eTorrent.hrl").

%%% ----------------------------------------------------------------------
%%% INTERNAL API
%%% ----------------------------------------------------------------------

get_port() ->
    call(?MODULE, get_port).

%%% ----------------------------------------------------------------------
%%% SUPERVISOR INTERFACE
%%% ----------------------------------------------------------------------

start_link() ->
    proc_lib:start_link(?MODULE, init, [], infinity, []).

%%% ----------------------------------------------------------------------
%%% GENERIC LISTEN SOCKET LOOP
%%% ----------------------------------------------------------------------

-record(state, {debug = [], % list, stdlib debug opts
		lsock, % listen socket reference
		parent, % pid() 
		port}). % integer() - listen port number

init() ->
    %% The '$ancestors' value is set by proc_lib
    register(?MODULE, self()),
    [Parent|_] = get('$ancestors'),
    proc_lib:init_ack({ok, self()}),
    %% Try the Bit Torrent reserved ports first, then take any port.
    %% The port number will be announced through the tracker anyway.
    Ports = lists:seq(6881, 6889, 1)++[0],
    socket_loop(#state{parent=Parent}, Ports).


socket_loop(State, [Port|Ports]) ->
    Options = [binary,
	       {reuseaddr,true},
	       {keepalive, true},
	       {packet, raw},
	       {active, false}],
    case catch gen_tcp:listen(Port, Options) of
	{ok, LSock} ->
	    gen_server:cast(eTorrent, {port,Port}),
	    loop(State#state{port = Port, lsock = LSock});
	{E, Reason} when E==error; E=='EXIT' ->
	    info_msg("Failed to open port ~w. ~p~n",[Port, {E, Reason}]),
	    socket_loop(State, Ports)
    end;
socket_loop(State, []) ->
    erlang:error(no_ports_available, [State, []]).


loop(State) ->
    case gen_tcp:accept(State#state.lsock, 1000) of
	{ok, Socket} ->
	    %% info_msg("Socket accepted: ~p ~p~n",
	    %% 	     [Socket, inet:peername(Socket)]),
	    inet:setopts(Socket, [{reuseaddr, true},
				  {keepalive, true}]),
	    {ok, Pid} = eTorrentPeer:start(Socket),
	    gen_tcp:controlling_process(Socket, Pid),
	    loop(State);
	{error, timeout} ->
	    %% I would have liked the gen_tcp:accept/2 to be asynchronous
            %% But it turns out that it all boils down to the 
            %% erlang:port_control/3 BIF which isn't, so we have to use
            %% this construct. 
            Parent = case State#state.parent of
                         Pid when is_pid(Pid) -> Pid;
                         Name when is_atom(Name) -> whereis(Name)
                     end,
            receive
                {'EXIT', Parent, Reason} ->
                    system_terminate(Reason, Parent, State#state.debug, State);
                {'EXIT', _, _} = ExitMsg ->
                   system_terminate(ExitMsg, Parent, State#state.debug,State);
                {system, From, Msg} ->
                    ParentA = State#state.parent,
                    DebugA= State#state.debug,
                    sys:handle_system_msg(
                      Msg, From, ParentA, ?MODULE, DebugA, State);
		{call, From, Msg} ->
		    case handle_call(Msg, From, State) of
			{reply, Reply, NewState} ->
			    reply(From, Reply),
			    loop(NewState)
		    end
            after 0 ->
                    loop(State)
            end;
        {error, Reason} -> 
            erlang:error(Reason, [State])
    end.

%%% ----------------------------------------------------------------------
%%% SYS INTERFACE
%%% ----------------------------------------------------------------------

system_terminate(Reason, _, _, State) ->
    gen_tcp:close(State#state.lsock),
    exit(Reason).

system_code_change(State, _, _, _) ->
    {ok, State}.

system_continue(Parent, Debug, State) ->
    loop(State#state{parent = Parent, debug = Debug}).

%% Concepts from gen_server

call(Process, Msg) ->
    call(Process, Msg, 5000).

call(Process, Msg, Timeout) ->
    MonitorRef = monitor(process, Process),
    Process!{call, {self(), MonitorRef}, Msg},
    receive
	{'$eTorrent_reply', MonitorRef, Reply} ->
	    demonitor(MonitorRef),
	    Reply;
	{'DOWN', _, _, _, Info} ->
	    {error, Info}
    after Timeout ->
	    demonitor(MonitorRef),
	    {error, timeout}
    end.

reply({Pid, Ref}, Reply) ->
    Pid!{'$eTorrent_reply', Ref, Reply}.

handle_call(get_port, _, State) ->
    {reply, {ok, State#state.port}, State};
handle_call(_Request, _From, State) ->
    {reply, error, State}.



%% encode_uri([H|T]) when H>127 ; H == $% ->
%%     [$%, hex(H div 16), hex(H rem 16)|encode_uri(T)];
%% encode_uri([H|T]) ->
%%     [H|encode_uri(T)];
%% encode_uri([]) ->
%%     [].
    

%% hex2dec(X) when (X >= $0) andalso (X =< $9) -> X-$0;
%% hex2dec(X) when (X >= $A) andalso (X =< $F) -> X-$A+10;
%% hex2dec(X) when (X >= $a) andalso (X =< $f) -> X-$a+10.

%% hex(X) when X>=10 ->
%%     X+$A-10;
%% hex(X) ->
%%     X+$0.


%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     error_logger:error_msg("~w: "++Format, [?MODULE|Args]).

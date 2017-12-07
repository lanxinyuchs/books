-module(eTracker).

%%% Application interface
-export([start/2, stop/1]).

%%% Supervisor interface
-export([start_link/1]).

%%% Sys interface
-export([system_code_change/4, system_continue/3, system_terminate/4]).

%%% Internal interface
-export([init/1]).

%%% Generic functions api

-export([decode_uri/1, encode_uri/1, hex2dec/1, hex/1]).

-include("eTracker.hrl").
-include("eTrackerInternal.hrl").

%%% ----------------------------------------------------------------------
%%% APPLICATION INTERFACE
%%% ----------------------------------------------------------------------

%%% start(StartType, StartArgs) ->
start(_, _) ->
    mnesia:create_table(eTrackerPeer, 
			[{ram_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, eTrackerPeer)}]),
    eTrackerSuper:start_link().

%%% stop(State) ->
stop(_) ->
    ok.

%%% ----------------------------------------------------------------------
%%% SUPERVISOR INTERFACE
%%% ----------------------------------------------------------------------

start_link(Type) ->
    proc_lib:start_link(?MODULE, init, [Type], infinity, []).

%%% ----------------------------------------------------------------------
%%% INTERNAL INTERFACE
%%% ----------------------------------------------------------------------

-record(state, {debug = [], % list, stdlib debug opts
		lsock,      % listen socket reference
		parent,     % pid() 
		port,       % integer() - listen port number
		type        % www | tracker
	       }).

init(Type) ->
    Options = [binary,
	       {reuseaddr,true},
	       {keepalive, true},
	       {packet, http},
	       {active, false}],
    %% This should not be hard coded here. Move to app file
    Port = case Type of
	       www -> 8080;
	       tracker -> 6969 % Bit Torrent tracker defacto standard?
	   end,
    case catch gen_tcp:listen(Port, Options) of
	{ok, LSock} ->
	    error_logger:info_report(progress, [{eTracker, listening},
						{type, Type},
						{port, Port}]),
	    %% The '$ancestors' value is set by proc_lib
	    [Parent|_] = get('$ancestors'),
	    proc_lib:init_ack({ok, self()}),
	    loop(#state{lsock = LSock, parent = Parent, port=Port, type=Type});
	{E, Reason} when E==error; E=='EXIT' ->
	    error_msg("Failed to open port ~w. ~p~n",[Port, {E,Reason}]),
	    proc_lib:init_ack({ok, self()}),
	    socket_loop(#state{port=Port, type=Type}, Options, 
			calendar:universal_time())
    end.

%% Try open a listen socket for 1 minute then give up

socket_loop(State, Options, Time) ->
    case catch gen_tcp:listen(State#state.port, Options) of
	{ok, LSock} ->
	    loop(State#state{lsock = LSock});
	{E, Reason} when E==error; E=='EXIT' ->
	    info_msg("Failed to oper port ~w. ~p~n",[State#state.port,
						     {E, Reason}]),
	    Now = calendar:universal_time(),
            Diff = calendar:datetime_to_gregorian_seconds(Now)-
                calendar:datetime_to_gregorian_seconds(Time),
            case Diff of
                Diff when Diff>60 -> 
                    erlang:error({E, Reason}, [State, Options, Time]);
                _ ->
                    timer:sleep(1000),
                    socket_loop(State, Options, Time)
            end
    end.


loop(State) ->
    case gen_tcp:accept(State#state.lsock, 1000) of
	{ok, Socket} ->
	    info_msg("Socket accepted: ~p ~p~n",
	    	     [Socket, inet:peername(Socket)]),
	    inet:setopts(Socket, [{reuseaddr, true},
				  {keepalive, true}]),
	    Pid = proc_lib:spawn(
		    fun() ->
			    handler_init(Socket, State#state.type)
		    end),
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
                      Msg, From, ParentA, ?MODULE, DebugA, State)
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


%%% ----------------------------------------------------------------------
%%% ENABLING FUNCTIONS LIBRARY
%%% ----------------------------------------------------------------------

handler_init(Socket, Type) ->
    case catch handle_incoming(Socket, Type) of
	{'EXIT', Reason} ->
	    error_logger:error_report(
              [{?MODULE, handler_init, [Socket, Type]},
               {line, ?LINE},
               {mfa, {?MODULE, handle_incoming, [Socket, Type]}},
               {'EXIT', Reason}]),
	    gen_tcp:close(Socket);
	ok ->
	    ok
    end.

handle_incoming(Socket, Type) ->
    inet:setopts(Socket, [{active, once}]),
    {ok, {Host, Port}} = inet:peername(Socket),
    handle_incoming(Socket, Type, undefined, [{peer, {Host, Port}}]).

handle_incoming(Socket, Type, Uri, HttpValues) ->
    receive
	{http, Socket, http_eoh} ->
	    case proplists:get_value('Content-Length', HttpValues) of
		undefined ->
		    handle_request(Socket, Type, Uri, HttpValues, undefined);
		Lstr ->
		    Length = list_to_integer(Lstr),
		    %% Data is supposed to be empty...
		    {ok, Data} = gen_tcp:recv(Socket, Length),
		    handle_request(Socket, Type, Uri, HttpValues, Data)
	    end;
	{http, Socket, HttpPacket} ->
	    case HttpPacket of
		{http_request, 'GET', NewUri, _HttpVsn} ->
%		    io:format("uri ~p~n",[NewUri]),
		    inet:setopts(Socket, [{active, once}]),
		    handle_incoming(Socket, Type, NewUri, HttpValues);
		{http_header, _Int, Field, _, Value} ->
%		    io:format("Incoming ~p: ~p~n", [Field, Value]),
		    NewHttpValues = [{Field, Value}|HttpValues],
		    inet:setopts(Socket, [{active, once}]),
		    handle_incoming(Socket, Type, Uri, NewHttpValues);
		{http_response, _, _, _}=Resp ->
		    io:format("Unknown: ~p~n",[Resp]),
		    inet:setopts(Socket, [{active, once}]),
		    handle_incoming(Socket, Type, Uri, HttpValues)
	    end
    end.

handle_request(Socket, www, Uri, HttpValues, Data) ->
    %% info_msg("handle_request: ~p ~n",[Uri]),
    eTrackerWWW:handle_request(Socket, Uri, HttpValues, Data);
handle_request(Socket, tracker, Uri, HttpValues, Data) ->
    %% info_msg("handle_request: ~p ~n",[Uri]),
    eTrackerServer:handle_request(Socket, Uri, HttpValues, Data).


							    

%%% ----------------------------------------------------------------------
%%% GENERIC FUNCTIONS LIBRARY
%%% ----------------------------------------------------------------------
decode_uri([$%,A,B|Tail]) ->
    [hex2dec(A)*16+hex2dec(B)|decode_uri(Tail)];
decode_uri([H|T]) ->
    [H|decode_uri(T)];
decode_uri([]) ->
    [].

encode_uri([H|T]) when H>127 ; H == $% ->
    [$%, hex(H div 16), hex(H rem 16)|encode_uri(T)];
encode_uri([H|T]) ->
    [H|encode_uri(T)];
encode_uri([]) ->
    [].
    

hex2dec(X) when (X >= $0) andalso (X =< $9) -> X-$0;
hex2dec(X) when (X >= $A) andalso (X =< $F) -> X-$A+10;
hex2dec(X) when (X >= $a) andalso (X =< $f) -> X-$a+10.

hex(X) when X>=10 ->
    X+$A-10;
hex(X) ->
    X+$0.


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
error_msg(Format, Args) ->
    error_logger:error_msg("~w: "++Format, [?MODULE|Args]).

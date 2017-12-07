-module(eTorrentClient).
-behaviour(gen_server).

%% API
-export([start/1]).
-export([start_link/1]).

-export([piece_complete/3, piece_failed/2]).

%% sys interface

%-export([system_terminate/4, system_code_change/4, system_continue/3]).

%% Internal interface

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-include("eTorrent.hrl").

start(Args) ->
    [InfoHash|_] = Args,
    ChildId = {eTorrentClient, InfoHash},
    ChildSpec = {ChildId, {?MODULE, start_link, [Args]},
		 transient, 10000, worker, [?MODULE]},
    %% info_msg("Starting: ~p~n",[ChildId]),
    case supervisor:start_child(eTorrentSuper, ChildSpec) of
	{error, already_present} ->
	    supervisor:restart_child(eTorrentSuper, ChildId);
	    %% supervisor:delete_child(eTorrentSuper, ChildId),
	    %% supervisor:start_child(eTorrentSuper, ChildSpec);
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	Result ->
	    Result
    end.

%% start_link(Args) ->
%%     proc_lib:start_link(?MODULE, init, [Args]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% report_peer_id(InfoHash, Ip, Port, PeerId) ->
%%     {ok, Client} = get_client(InfoHash),
%%     gen_server:cast(Client, {remote_peer_id, Ip, Port,PeerId}).
    
piece_complete(InfoHash, Index, PeerId) ->
    {ok, Client} = get_client(InfoHash),
    gen_server:cast(Client, {piece_complete, Index, PeerId}).

piece_failed(InfoHash, Index) ->
    {ok, Client} = get_client(InfoHash),
    gen_server:cast(Client, {piece_failed, Index}).


-record(state, {info_hash,     % bin()
		interval = 10, % int()
		peer_id,       % bin()
		port,          % int() - Listening port
		peers = [],    % [{{Ip, Port}, Pid}] - Active peers
		peerlist = [], % [{Ip, Port, PeerId}] - Peers from tracker
		have = [],     % [int()] - List of pieces we have
		missing = [],  % [int()] - List of pieces we don't have
		tasked = [],   % [{Piece:int(), PeerId:bin()}] - Current downl
		priority = []  % [int()] - List of pieces in rarest first order
	       }).


init([InfoHash, PeerId, Port]) ->
    gen_server:cast(self(), init),
    {ok, #state{info_hash = InfoHash, peer_id = PeerId, port = Port}}.

handle_call(get_info, _, State) ->
    {reply, State, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(init,  State) ->
    InfoHash = State#state.info_hash,
    PeerId = State#state.peer_id,
    Port = State#state.port,

    {atomic, [Torrent]} =
	mnesia:transaction(fun() ->
				   mnesia:read({eTorrent, InfoHash})
			   end),

    case Torrent#eTorrent.status of
	complete -> 
	    %% info_msg("File complete~n"),
	    handle_cast({send_tracker_request, undefined}, State),
	    {stop, normal, State};
	Status ->
	    info_msg("Torrent status ~p ~p~n",[Status, InfoHash]),
	    Dir = eTorrent:get_storage_dir(),
	    Name = Torrent#eTorrent.name,
	    Pattern = filename:join([Dir, ".pieces", Name++"*"]),
	    Files = filelib:wildcard(Pattern),
	    Have = 
		lists:foldl(fun(File, A) ->
				    P = lists:last(string:tokens(File, ".")),
				    [list_to_integer(tl(P))|A]
			    end, [], Files),
	    Missing = lists:seq(0, length(Torrent#eTorrent.pieces)-1)--Have,
	    
	    case Missing of 
		[] ->
		    info_msg("All pieces retrieved, assembling file~n"),
		    assemble_file(Torrent),
		    {stop, normal, State};
		_ ->
		    mnesia:subscribe({table, eTorrentBitfield, simple}),
		    NewState = 
			State#state{info_hash = InfoHash, peer_id = PeerId, 
				    port = Port, have = Have, 
				    missing = Missing},
		    gen_server:cast(self(), {send_tracker_request, started}),
		    {noreply, NewState}
	    end
    end;


handle_cast({send_tracker_request, Type}, State) ->
    %% info_msg("Sending tracker request ~w~n",[Type]),
    PeerId = State#state.peer_id,
    Port = State#state.port,
    Torrent = get_torrent(State#state.info_hash),
    case Torrent#eTorrent.status of
	complete ->
	    tracker_request(undefined, Torrent, PeerId, Port),
	    {noreply, State};
	undefined ->
	    check_pieces(Torrent),
	    case tracker_request(Type, Torrent, PeerId, Port) of
		{ok, {_, _, Body}} ->
		    TrackerResponse = bencode:decode(Body),
		    Peers = proplists:get_value(<<"peers">>, TrackerResponse),
		    PeerList = 
			case Peers of
			    Peers when is_binary(Peers) -> 
				%% Compact (which is expected)
				peers_from_string(Peers);
			    Peers -> 
				%% Dictionary (not requested, but what the heck)
				peers_from_dictionary(Peers)
			end,
		    NewState = launch_peers(PeerList, State),
		    Interval = proplists:get_value(<<"interval">>, 
						   TrackerResponse, 10),
		    Timeout = Interval*1000,
		    timer:apply_after(Timeout, gen_server, cast, 
				      [self(), 
				       {send_tracker_request, undefined}]),
		    {noreply, assign_pieces(NewState#state{interval=Interval,
							   peerlist=PeerList})};
		{error, Reason} ->
		    error_msg("Tracker request failed: ~p~n",[Reason]),
		    Timeout = State#state.interval * 1000,
		    timer:apply_after(Timeout, gen_server, cast, 
				      [self(), 
				       {send_tracker_request, undefined}]),
		    {noreply, State}
	    end
    end;
handle_cast({piece_complete, Index, RemotePeerId}, State) ->
    %% Remove task from list
    %% In case of endgame send cancel to other peers which may have 
    %% gotten the request
    NewTasked = [{Task, PeerId}||
		    {Task, PeerId}<-State#state.tasked,
		    case {Task, PeerId} of
			{Index, RemotePeerId} -> false;
			{Index, OtherPeerId} ->
			    {value, {Ip, Port, _}} = 
				lists:keysearch(OtherPeerId, 3, 
						State#state.peerlist),
			    {value, {_, Pid}} = 
				lists:keysearch({Ip,Port},1,State#state.peers),
			    eTorrentPeer:cancel_request(Pid),
			    false;
			_ -> 
			    true
		    end],
			
    case lists:delete(Index, State#state.missing) of
	[] -> %% File complete
	    [Torrent] = mnesia:dirty_read({eTorrent, State#state.info_hash}),
	    assemble_file(Torrent),
	    info_msg("File complete: ~p~n",[Torrent#eTorrent.name]),
	    {stop, normal, State};
	NewMissing ->
	    NewHave = [Index|State#state.have],
	    NewState = assign_pieces(State#state{tasked = NewTasked,
						 have = NewHave,
						 missing = NewMissing}),
	    {noreply, NewState}
    end;
handle_cast({piece_failed, Index}, State) ->
    NewTasked = lists:keydelete(Index, 1, State#state.tasked),
    NewState = assign_pieces(State#state{tasked = NewTasked}),
    {noreply, NewState};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
    
    NewPeers = delete_on_value(Pid, State#state.peers),
    {noreply, State#state{peers = NewPeers}};
handle_info({mnesia_table_event, Event}, State)  ->
    handle_mnesia_event(Event, State);
handle_info(Request, State) ->
    info_msg("Info: ~p~n~p~n",[Request, State]),
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.
    
%%% ----------------------------------------------------------------------
%%% ENABLING FUNCTIONS LIBRARY
%%% ----------------------------------------------------------------------

assemble_file(Torrent) ->
    Pieces = lists:seq(0, length(Torrent#eTorrent.pieces)-1),
    Dir = eTorrent:get_storage_dir(),
    Name = Torrent#eTorrent.name,
    Path = filename:join(Dir, Name),
    {ok, Fd} = file:open(Path, [write, raw, binary]),
    [begin
	 PieceName = Torrent#eTorrent.name++".p"++integer_to_list(Piece),
	 PiecePath = filename:join([Dir, ".pieces", PieceName]),
	 {ok, Bin} = file:read_file(PiecePath),
	 ok = file:delete(PiecePath),
	 ok = file:write(Fd, Bin)
     end||Piece<-Pieces],
    file:close(Fd),
    mnesia:transaction(
      fun() ->
	      T = mnesia:wread({eTorrent, Torrent#eTorrent.info_hash}),
	      mnesia:write(T#eTorrent{status = complete,
				      left = 0})
      end).

get_torrent(InfoHash) ->
    case mnesia:transaction(
	   fun() ->
		   mnesia:read({eTorrent, InfoHash})
	   end) of
	{atomic, [Torrent]} ->
	    Torrent;
	{atomic, []} -> % should not be
	    erlang:error(no_such_torrent, [InfoHash]);
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [InfoHash])
    end.

check_pieces(_Torrent) ->
    ok.

%%% Out: {HttpStatus, Headers, Body}

tracker_request(Type, Torrent, PeerId, Port) ->
    InfoHash = uri_encode(binary_to_list(Torrent#eTorrent.info_hash)),
    RequestInfo = 
	[{"peer_id",    PeerId},
	 {"port",       integer_to_list(Port)},
	 {"uploaded",   integer_to_list(Torrent#eTorrent.uploaded)},
	 {"downloaded", integer_to_list(Torrent#eTorrent.downloaded)},
	 {"left",       integer_to_list(Torrent#eTorrent.left)},
%	 {"compact",    integer_to_list(random:uniform(2)-1)}
	 {"compact",    "0"}
	]++
	case Type of
	    undefined -> [];
	    _ -> [{"event",      atom_to_list(Type)}]
	end,
    Request = Torrent#eTorrent.announce++
	lists:foldl(fun({Key,Value}, Accu)->
			    Accu++"&"++Key++"="++Value
		    end, "?info_hash="++InfoHash, RequestInfo),
    httpc:request(Request).

launch_peers(PeerList, State) ->
    lists:foldl(fun({Ip, Port, _}, S) ->
			start_client(Ip, Port, S)
		end, State, PeerList).

start_client(Ip, Port, State) ->
%    io:format("Peers: ~p~n",[State#state.peers]),
    case lists:keysearch({Ip, Port}, 1, State#state.peers) of
	{value, _} ->
	    State;
	false ->
	    case eTorrentPeer:client_start(Ip, Port, 
					   State#state.info_hash,
					   State#state.peer_id) of
		{ok, Pid} ->
		    erlang:monitor(process, Pid),
		    State#state{peers = [{{Ip, Port}, Pid}|State#state.peers]};
		_ ->
		    State
	    end
    end.

%%% Piece assignment algorithm
%%% 1. Check if there are missing pieces, if not end here
%%% 2. Check if there are available peers which are not tasked with retrieveing
%%%    a piece, if not end here
%%% 3. For each piece in the priolist
%%%    a. Check if we have that piece, if so loop to next
%%%    b. (i)   Check if peer is choked, if so loop to next
%%%       (ii)  Check if peer is tasked, if so loop to next, unless endgame
%%%       (iii) Check if peer has the piece, if not then loop to next
%%%    c. Start a peer process if necessary
%%%    d. Send the assignment
%%%
%%% The "endgame" strategy is to request pieces from all known peers at the
%%% end of the download as to minimize the waiting period

assign_pieces(State) ->
    case State#state.missing of
	[] ->
	    %% io:format("No pieces missing~n"),
	    State;
	Missing when length(Missing) < 5 ->
	    S = do_assign_pieces(State, true, State#state.priority),
	    io:format("~p~n",[S#state.tasked]),
	    S;
	_ ->
	    S = do_assign_pieces(State, false, State#state.priority),
	    io:format("~p~n",[S#state.tasked]),
	    S
    end.


do_assign_pieces(State, Endgame, [Prio1|Prio]) ->
    case {length(State#state.peerlist), length(State#state.tasked)} of
	{Peers, Tasks} when Peers > Tasks ->
	    NewState = do_assign_pieces(State, Endgame, Prio1),
	    do_assign_pieces(NewState, Endgame, Prio);
	{_, 0}  ->
	    NewState = do_assign_pieces(State, Endgame, Prio1),
	    do_assign_pieces(NewState, Endgame, Prio);
	_ ->
	    %% io:format("No peers to task~n"),
	    State
    end;

do_assign_pieces(State, Endgame, Prio1) when is_integer(Prio1) ->
    case lists:member(Prio1, State#state.have) of
	true ->
	    %% io:format("We have piece ~w already.~n",[Prio1]),
	    State;
	false ->
	    case lists:keysearch(Prio1, 1, State#state.tasked) of
		{value, _} when not Endgame -> 
		    State;
		_ ->
		    assign_piece(State, Prio1, State#state.peerlist)
	    end
    end;
do_assign_pieces(State, _, []) -> State.


assign_piece(State, Index, [Peer|PeerList]) ->
    {Ip, Port, PeerId} = Peer,

    InfoHash = State#state.info_hash,
    Key = {Ip, Port, InfoHash},

    Tasked = is_peer_tasked(PeerId, State#state.tasked),
	    
    Choking = 
	case mnesia:dirty_read({eTorrentPeer, Key}) of
	    [PeerObj] ->
		PeerObj#eTorrentPeer.peer_choking == peer_choking;
	    [] ->
		peer_choking
	end,


    HavePiece =
	case mnesia:dirty_read({eTorrentBitfield, {InfoHash, PeerId}}) of
	    [] -> 
		false;
	    [Bitfield] ->
		BF = element(Index+1, Bitfield#eTorrentBitfield.bitfield),
		BF == 1
	end,

    if Choking or Tasked  or (not HavePiece) ->
	    %% io:format("Choking = ~w~nTasked = ~w~nHavePiece = ~w~n",
	    %% 	      [Choking, Tasked, HavePiece]),
	    assign_piece(State, Index, PeerList);
       true ->
	    assign_peer(State, Index, Peer)
    end;
assign_piece(State, _, []) ->
    State.

is_peer_tasked(Peer, Tasked) ->
    case lists:keysearch(Peer, 2, Tasked) of
	{value, _} ->
	    true;
	false ->
	    false
    end.

assign_peer(State, Index, Peer) ->
    {Ip, Port, PeerId} = Peer,
    Tasked = State#state.tasked,
    NewState = start_client(Ip, Port, State),
    {value, {_, Pid}} = lists:keysearch({Ip, Port}, 1, NewState#state.peers),
    case catch eTorrentPeer:retrieve_piece(Pid, Index) of
	ok -> 
	    NewState#state{tasked = [{Index, PeerId}|Tasked]};
	_E ->
	    NewState
    end.

peers_from_string(<<A:8,B:8,C:8,D:8,Port:16,Tail/binary>>) ->
    [{{A,B,C,D},Port, undefined}|peers_from_string(Tail)];
peers_from_string(<<>>) ->
    [].

peers_from_dictionary([Peer|Peers]) ->
    IpStr = binary_to_list(proplists:get_value(<<"ip">>, Peer)),
    Port = proplists:get_value(<<"port">>, Peer),
    PeerId = proplists:get_value(<<"peer id">>, Peer),
    {ok, Ip} = inet_parse:address(IpStr),
    [{Ip, Port, PeerId}|peers_from_dictionary(Peers)];
peers_from_dictionary([]) -> [].


%%% RFC 2396
%% This is not the complete uri set as the reserved chars are not included

-define(uri_set,
	"0123456789"
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	"acbdefghijklmnopqrstuvwxyz"
	"-_.!~*'()").

%%%    Reserved = [$;, $/, $?, $:, $@, $&, $=, $+, $$, $,],

%%% The info_hash contains bytes which are not part of the legal uri set
%%% however the same principle for encoding is followed for those values
%%% also.

uri_encode([H|T]) ->
    case lists:member(H, ?uri_set) of
	true ->
	    [H|uri_encode(T)];
	false ->
	    [$%, hex(H div 16), hex(H rem 16)|uri_encode(T)]
    end;
uri_encode([]) -> [].

hex(X) when X>=10 ->
    X+$A-10;
hex(X) ->
    X+$0.

delete_on_value(Value, [{_, Value}|Tail]) ->
    Tail;
delete_on_value(Value, [H|T]) ->
    [H|delete_on_value(Value, T)];
delete_on_value(_, []) -> [].


%% Must be evaluated within the scope of a transaction
%% Rarest piece first strategy
prioritize_pieces(InfoHash) ->
    WP = mnesia:table_info(eTorrentBitfield, wild_pattern),
    Pattern = WP#eTorrentBitfield{key={InfoHash, '_'}},
    case mnesia:dirty_match_object(Pattern) of
	[] -> 
	    [];
	BitFields ->
	    SumBitfield = sum_bitfields(BitFields),
	    Stats = lists:zip(lists:seq(0, length(SumBitfield)-1), SumBitfield),
	    [Piece||{Piece, _}<-lists:reverse(lists:keysort(2, Stats))]
    end.
	    
sum_bitfields([]) ->
    []; % FIXME: No bitfield info.  Handle this case please
sum_bitfields([BitField|BitFields]) -> % This is actually not recursion
    lists:foldl(
      fun(BF, SubBitField) ->
	      BfList = tuple_to_list(BF#eTorrentBitfield.bitfield),
	      pair_sum(BfList, SubBitField)
      end, tuple_to_list(BitField#eTorrentBitfield.bitfield), BitFields).

pair_sum([A|ListA], [B|ListB]) ->
    [A+B|pair_sum(ListA, ListB)];
pair_sum([], []) ->
    [].


handle_mnesia_event(Event, State) ->
    case Event of
	{write, Record, _} when is_record(Record, eTorrentBitfield) ->
%	    info_msg("~p~n",[Record]),  
	    InfoHash = State#state.info_hash,
	    case Record#eTorrentBitfield.key of
		{InfoHash, _} ->
		    Priority = prioritize_pieces(InfoHash),
		    NewState = assign_pieces(State#state{priority = Priority}),
		    {noreply, NewState};
		_ ->
		    %% This update does not concern me
		    {noreply, State}
	    end;
	%% {write, eTorrentPeer, NewRecord, [OldRecord], ActivityId} ->
	%%     case NewRecord#eTorrentPeer.info_hash of
	%% 	InfoHash when State#state.info_hash == InfoHash->
	%% 	    New = NewRecord#eTorrentPeer.retrieving,
	%% 	    Old = OldRecord#eTorrentPeer.retrieving,
	%% 	    case New of
	%% 		undefined when Old /= undefined ->
	%% 		    %% This means the peer has completed downloading
	%% 		    %% a piece.
	%% 		    info_msg("Piece ~w downloaded. ~n~p~n",[Old, State]),
	%% 		    {noreply, State};
	%% 		_ ->
	%% 		    %% Something else happened. Don't care
	%% 		    {noreply, State}
	%% 	    end;
	%% 	_ -> %% This event doesn't concern us - ignore
	%% 	    {noreply, State}
	%%     end;
	%% {write, eTorrentPeer, NewRecord, [], ActivityId} ->
	%%     {noreply, State};
	Event ->
%	    info_msg("New mnesia event~n~p~n",[Event]),
	    {noreply, State}
    end.

get_client(InfoHash) ->
    get_client(InfoHash, supervisor:which_children(eTorrentSuper)).

get_client(InfoHash, [{{eTorrentClient, InfoHash}, Pid, _, _}|_]) ->
    {ok, Pid};
get_client(InfoHash, [_|Children]) ->
    get_client(InfoHash, Children);
get_client(_, []) ->
    {error, not_found}.
    


info_msg(Format) ->
    info_msg(Format, []).
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


-module(eTorrentPeer).
-behaviour(gen_server).

%%% API
-export([start/1, client_start/4]).
-export([retrieve_piece/2, cancel_request/1]).


%%% Internal
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-export([startup/1, tcp_loop_init/2]).

%% Sys interface
-export([system_code_change/4, system_continue/3, system_terminate/4]).

-include("eTorrent.hrl").
-compile(export_all).
start(Socket) ->
    proc_lib:start(?MODULE, startup, [[Socket]]).

client_start(Ip, Port, InfoHash, PeerId) ->
    proc_lib:start(?MODULE, startup, [[Ip, Port, InfoHash, PeerId]]).

retrieve_piece(Process, Index) ->
    gen_server:call(Process, {retrieve_piece, Index}).

cancel_request(Process) ->
    gen_server:call(Process, cancel_request).


    
%%% ----------------------------------------------------------------------
%%% INTERNAL
%%% ----------------------------------------------------------------------


-record(state, {socket,
		info_hash,      
		local_peer_id,  % Peer id for this peer
		remote_peer_id, % Peer id for the peer on the other end
		peer_key,       % Key to this peers entry in eTorrentPeer
		path,           % Path to the file
		piece_length,   % Length of a piece
		pieces_number,  % The number of pieces
		file_size,      % The total size of the entire file
		current_piece,  % The current piece to be downloaded
		blockstore,     % Reference to an ets table for storing blocks
		blocks,         % All the missing blocks idientifed by their
                                % starting byte
		pending = []}). % Outstanding request for blocks


startup([Socket]) ->
    proc_lib:init_ack({ok, self()}),
    {ok, PeerId} = eTorrent:get_peer_id(),
    handshake(#state{socket = Socket,
		     local_peer_id = PeerId});

startup([Ip, Port, InfoHash, PeerId]) ->
    Opts = [binary, 
	    {reuseaddr, true},
	    {keepalive, true},
	    {packet, raw},
	    {active, false}],
    case gen_tcp:connect(Ip, Port, Opts) of
	{ok, Socket} ->
	    proc_lib:init_ack({ok, self()}),
	    send_handshake(Socket, InfoHash, PeerId, #state{socket=Socket});
	{error, econnrefused} ->
	    proc_lib:init_ack({error, econnrefused})
    end.


send_handshake(Socket, InfoHash, PeerId, State) ->
    Msg = lists:flatten([19, "BitTorrent protocol", 0,0,0,0,0,0,0,0, 
			 binary_to_list(InfoHash), PeerId]),
    ok = gen_tcp:send(Socket, Msg),

    %% Receive handshake response
    {ok, <<PStrLen:8>>} = gen_tcp:recv(Socket, 1),
    {ok, Protocol} = gen_tcp:recv(Socket, PStrLen),
    {ok, <<Reserved:8/binary, RemoteInfoHash:20/binary>>} = 
	gen_tcp:recv(Socket, 28),
    {ok, RemotePeerId} = gen_tcp:recv(Socket, 20),

    error_logger:info_report(
      [{handshake, Socket},
       {protocol, Protocol},
       {reserved, Reserved},
       {info_hash, RemoteInfoHash},
       {peer_id, RemotePeerId}]),

    %% Dirty because no one else would have this information at this 
    %% stage, so there can be no conflicts
    {ok, {Ip, Port}} = inet:peername(Socket),
    Key = {Ip, Port, InfoHash},
    mnesia:dirty_write(#eTorrentPeer{key = Key,
				     pid = self(),
				     socket = Socket,
				     remote_peer_id = RemotePeerId}),
    %% Send bitfield
    {atomic, [Torrent]} = 
	mnesia:transaction(fun() ->
				   mnesia:read({eTorrent, InfoHash})
			   end),
		    Dir = eTorrent:get_storage_dir(),
		    Path = filename:join(Dir, Torrent#eTorrent.name),
    send_bitfield(Socket, Torrent),
    send_keep_alive(Socket),
    PieceLength = Torrent#eTorrent.piece_length,
    PieceNumber = Torrent#eTorrent.pieces_number,
    FileSize = Torrent#eTorrent.length,
    NewState = send_unchoke(send_interested(State#state{socket=Socket,
							peer_key=Key})),
    Blockstore = ets:new(blockstore, [set, public]),
    {ok, Pid} = 
	proc_lib:start_link(?MODULE, tcp_loop_init, [Socket, self()]),
    gen_tcp:controlling_process(Socket, Pid),
    gen_server:enter_loop(?MODULE, [], 
			  NewState#state{info_hash = InfoHash,
					 local_peer_id = PeerId,
					 remote_peer_id = RemotePeerId,
					 path = Path,
					 file_size = FileSize,
					 blockstore = Blockstore,
					 piece_length = PieceLength,
					 pieces_number = PieceNumber}).

%%% Wait for handshake when connection is started at the remote end
handshake(State) ->
    {ok, <<PStrLen:8>>} = gen_tcp:recv(State#state.socket, 1),
    {ok, Protocol} = gen_tcp:recv(State#state.socket, PStrLen),
    {ok, <<Reserved:8/binary, InfoHash:20/binary>>} = 
	gen_tcp:recv(State#state.socket, 28),
    case mnesia:dirty_read({eTorrent, InfoHash}) of
	[] ->
	    %% This infohash is not served. Close
	    gen_tcp:close(State#state.socket);
	[Torrent] ->
	    {ok, PeerId} = gen_tcp:recv(State#state.socket, 20),
	    error_logger:info_report(
	      [{handshake, inet:peername(State#state.socket)},
	       {protocol, Protocol},
	       {reserved, Reserved},
	       {info_hash, InfoHash},
	       {peer_id, PeerId}]),
	    %% Dirty because no one else would have this information at this 
	    %% stage, so there can be no conflicts
	    {ok, {Ip, Port}} = inet:peername(State#state.socket),
	    Key = {Ip, Port, InfoHash},
	    mnesia:dirty_write(#eTorrentPeer{key = Key,
					     pid = self(),
					     socket = State#state.socket,
					     remote_peer_id = PeerId}),
	    Size = size(Protocol),
	    Msg = [Size, Protocol, 0,0,0,0,0,0,0,0, InfoHash,
		   State#state.local_peer_id],
	    case gen_tcp:send(State#state.socket, Msg) of
		ok ->
		    Dir = eTorrent:get_storage_dir(),
		    Path = filename:join(Dir, Torrent#eTorrent.name),
		    send_bitfield(State#state.socket, Torrent),
		    NewState = send_unchoke(
				  send_interested(State#state{peer_key=Key})),
		    PieceLength = Torrent#eTorrent.piece_length,
		    PieceNumber = Torrent#eTorrent.pieces_number,
		    FileSize = Torrent#eTorrent.length,
		    {ok, Pid} = proc_lib:start_link(
				  ?MODULE, tcp_loop_init, 
				  [State#state.socket, self()]),
		    gen_tcp:controlling_process(State#state.socket, Pid),
		    Blockstore = ets:new(blockstore, [set, public]),
		    gen_server:enter_loop(
		      ?MODULE, [], 
		      NewState#state{info_hash=InfoHash,
				     remote_peer_id=PeerId,
				     path = Path,
				     file_size = FileSize,
				     blockstore = Blockstore,
				     piece_length = PieceLength,
				     pieces_number = PieceNumber});
		{error, Reason} ->
		    error_logger:error_report(
		      [{mfa, {gen_tcp, send, [State#state.socket, Msg]}},
		       {error, Reason}]),
		    gen_tcp:close(State#state.socket)
	    end
    end.

%%% This loop shall receive tcp packets asynchronously, and transfer them
%%% as peer wire message to the controlling gen_server process.

-record(loopdata, {parent, debug=[], socket, cache, controller, head=true}).

tcp_loop_init(Socket, Controller) ->
    [Parent|_] = get('$ancestors'),
    LoopData = #loopdata{parent = Parent,
			 socket = Socket,
			 controller = Controller},
    proc_lib:init_ack({ok, self()}),
    inet:setopts(Socket, [{active, true}]),
    loop(LoopData).

loop(LoopData) ->
    receive
	{tcp, Socket, Data} ->
	    do_tcp({tcp, Socket, Data}, LoopData);
	{tcp_closed, Socket} ->
	    do_tcp({tcp_closed, Socket}, LoopData);
	{tcp_error, Socket, Reason} ->
	    do_tcp({tcp_error, Socket, Reason}, LoopData);
	{system, From, Msg} ->
	    Parent = LoopData#loopdata.parent,
	    Debug = LoopData#loopdata.debug,
	    sys:handle_system_msg(
	      Msg, From, Parent, ?MODULE, Debug, LoopData)
	    
    end.

do_tcp({tcp, _, <<0:32>>}, LoopData) ->
    loop(LoopData);
do_tcp({tcp, Socket, <<0:32, Data/binary>>}, LoopData) 
  when LoopData#loopdata.cache == undefined ->
    do_tcp({tcp, Socket, Data}, LoopData);
do_tcp({tcp, Socket, <<Len:32, Payload/binary>> = Data}, LoopData)
  when LoopData#loopdata.cache == undefined ->
    case size(Payload) of
	Len -> 
	    <<Id:8, Msg/binary>> = Payload,
	    gen_server:cast(LoopData#loopdata.controller, {msg, Id, Msg}),
	    loop(LoopData);
	Size when Size > Len ->
	    %% There is more than one message contained in this data
	    %% Handle the first message, and loop the rest.
	    <<ThisPayload:Len/binary, Remaining/binary>> = Payload,
	    <<Id:8, Msg/binary>> = ThisPayload,
	    gen_server:cast(LoopData#loopdata.controller, {msg, Id, Msg}),
	    do_tcp({tcp, Socket, Remaining}, LoopData);
	Size when Size < Len ->
	    %% The message is incomplete. Store the bytes in the cache
	    %% and wait for the next package
	    loop(LoopData#loopdata{cache = Data})
    end;
do_tcp({tcp, Socket, Data}, LoopData) when is_binary(LoopData#loopdata.cache) ->
    Cache = LoopData#loopdata.cache,
    NewData = <<Cache/binary, Data/binary>>,
    do_tcp({tcp, Socket, NewData}, LoopData#loopdata{cache = undefined});
do_tcp({tcp_closed, _}, LoopData) ->
    gen_server:call(LoopData#loopdata.controller, tcp_closed);
do_tcp({tcp_error, Socket, Reason}, LoopData) ->
    gen_server:call(LoopData#loopdata.controller, {tcp_error, Reason}),
    gen_tcp:close(Socket).

%%% ----------------------------------------------------------------------
%%% SYS INTERFACE
%%% ----------------------------------------------------------------------

system_terminate(Reason, _, _, LoopData) ->
    gen_tcp:close(LoopData#loopdata.socket),
    exit(Reason).

system_code_change(LoopData, _, _, _) ->
    {ok, LoopData}.

system_continue(Parent, Debug, LoopData) ->
    loop(LoopData#loopdata{parent = Parent, debug = Debug}).

%%% ----------------------------------------------------------------------
%%% GEN SERVER INTERFACE
%%% ----------------------------------------------------------------------

%% The init function is just a formality as the process is started 
%% in another way and enters gen_server through enter loop
init(State) ->
    {ok, State}. 

handle_call({retrieve_piece, Index}, _, State) ->
    %% info_msg("Retrieving piece ~w from ~p~n",[Index, State#state.remote_peer_id]),
    Blocks = calculate_blocks(Index, State),
    NewState = handle_retrieve_piece(Index, State#state{blocks = Blocks}),
    {reply, ok, NewState};
handle_call(cancel_request, _, State) ->
    NewState = handle_cancel_request(State),
    {reply, ok, NewState};
handle_call(tcp_closed, _, State) ->
    %% info_msg("TCP connection closed by ~p~n",[State#state.remote_peer_id]),
    mnesia:dirty_delete({eTorrentPeer, State#state.peer_key}),
    {stop, normal, ok, State};
handle_call({tcp_error, Reason}, _, State) ->
    error_msg("TCP error: ~p Peer: ~p~n",[Reason, State#state.remote_peer_id]),
    {stop, normal, ok, State};
handle_call(get_info, _, State) ->
    {reply, State, State};
handle_call(_, _, State) ->    
    {reply, ok, State}.

handle_cast({msg, Id, Msg}, State) ->
    handle_msg(Id, Msg, State);
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% ----------------------------------------------------------------------
%%% ENABLING FUNCTIONS LIBRARY
%%% ----------------------------------------------------------------------

calculate_blocks(Index, State) ->
    case determine_blocksize(State, Index, 0) of
	16384 ->
	    [0|calculate_blocks(Index, State, 16384)];
	_ ->
	    [0]
    end.

calculate_blocks(Index, State, Size) ->
    case determine_blocksize(State, Index, Size) of
	_ when Size == State#state.piece_length ->
	    [];
	16384 ->
	    [Size|calculate_blocks(Index, State, Size+16384)];
	0 ->
	    [];
	_ ->
	    [Size]
    end.

handle_retrieve_piece(Index, State) when length(State#state.pending) =< 4 ->
    case State#state.blocks of
	[Block|Blocks] ->
	    Size = determine_blocksize(State, Index, Block),
	    [Peer] = mnesia:dirty_read({eTorrentPeer, State#state.peer_key}),
	    case Peer#eTorrentPeer.peer_choking of
		peer_choking ->
		    State;
		peer_not_choking ->
		    send_request(State#state.socket, Index, Block, Size),
		    NewState = State#state{
				 current_piece = Index,
				 pending = [Block|State#state.pending], 
				 blocks = Blocks},
		    handle_retrieve_piece(Index, NewState)
	    end;
	[] ->
	    State
    end;
handle_retrieve_piece(_, State) ->
    State.
    
    
handle_cancel_request(State) ->
    lists:foldl(fun(Block, S) ->
			send_cancel(S, Block)
		end, State, State#state.pending).

%%% ----------------------------------------------------------------------
%%% SEND PEER WIRE MESSAGES
%%% ----------------------------------------------------------------------

send_keep_alive(Socket) ->
    gen_tcp:send(Socket, <<0:32>>).

send_choke(State) ->
    Packet = <<1:32, 0>>,
    gen_tcp:send(State#state.socket, Packet),
    set_status(State#state.peer_key, am_choking, am_choking),
    State.


send_unchoke(State) ->
    Packet = <<1:32, 1>>,
    gen_tcp:send(State#state.socket, Packet),
    set_status(State#state.peer_key, am_choking, am_not_choking),
    State.

send_interested(State) ->
    Packet = <<1:32, 2>>,
    gen_tcp:send(State#state.socket, Packet),
    set_status(State#state.peer_key, am_interested, am_interested),
    State.

send_not_interested(State) ->
    Packet = <<1:32, 3>>,
    gen_tcp:send(State#state.socket, Packet),
    set_status(State#state.peer_key, am_interested, am_not_interested),
    State.

send_have(Index, InfoHash) ->
    WP = mnesia:table_info(eTorrentPeer, wild_pattern),
    Pattern = WP#eTorrentPeer{key={'_', '_', InfoHash}},
    {atomic, Peers} = 
	mnesia:transaction(
	  fun() ->
		  [Torrent] = mnesia:wread({eTorrent, InfoHash}),
		  NewBitfield = setelement(Index+1,Torrent#eTorrent.bitfield,1),
		  NewTorrent = Torrent#eTorrent{bitfield = NewBitfield},
		  ok = mnesia:write(NewTorrent),
		  mnesia:match_object(Pattern)
	  end),
    Msg = <<4, Index:32>>,
    Size = size(Msg),
    Packet = <<Size:32, Msg/binary>>,    
    [gen_tcp:send(Peer#eTorrentPeer.socket, Packet)||Peer<-Peers],
    ok.

send_bitfield(Socket, Torrent) ->
    BitField = bitfield(Torrent#eTorrent.bitfield),
    Msg = <<5,BitField/binary>>,
    Size = size(Msg),
    Packet = <<Size:32, Msg/binary>>,
    gen_tcp:send(Socket, Packet).



bitfield(BitField) when is_tuple(BitField) ->
    Spare = case size(BitField) rem 8 of
		0 -> [];
		X -> [0||_<-lists:seq(X,7)]
	    end,
		
    bitfield(tuple_to_list(BitField)++Spare, <<>>).


bitfield([H|T], Accu) ->
    bitfield(T, <<Accu/binary, H:1>>);
bitfield([], Accu) ->
    Accu.
    
bitfield_bin2tuple(BitField, Size)  ->
    list_to_tuple(do_bitfield(BitField, Size)).

do_bitfield(<<A:1,B:1,C:1,D:1,E:1,F:1,G:1,H:1,BF/binary>>,Size) when Size>=8 ->
    [A,B,C,D,E,F,G,H|do_bitfield(BF, Size-8)];
do_bitfield(<<A:1,B:1,C:1,D:1,E:1,F:1,G:1,_:1>>, 7) ->
    [A,B,C,D,E,F,G];
do_bitfield(<<A:1,B:1,C:1,D:1,E:1,F:1,_:2>>, 6) ->
    [A,B,C,D,E,F];
do_bitfield(<<A:1,B:1,C:1,D:1,E:1,_:3>>, 5) ->
    [A,B,C,D,E];
do_bitfield(<<A:1,B:1,C:1,D:1,_:4>>, 4) ->
    [A,B,C,D];
do_bitfield(<<A:1,B:1,C:1,_:5>>, 3) ->
    [A,B,C];
do_bitfield(<<A:1,B:1,_:6>>, 2) ->
    [A,B];
do_bitfield(<<A:1,_:7>>, 1) ->
    [A];
do_bitfield(_, 0) ->
    [].

    
send_request(Socket, Index, Begin, Length) ->
    Msg = <<6, Index:32, Begin:32, Length:32>>,
    Size = size(Msg),
    Packet = <<Size:32, Msg/binary>>,
    %% info_msg("Requesting: index ~w, begin ~w, length ~w~n",
    %% 	     [Index, Begin, Length]),
    ok = gen_tcp:send(Socket, Packet).

send_piece(Socket, Index, Begin, Block) ->
%    timer:sleep(1000),
    Msg = <<7,Index:32,Begin:32,Block/binary>>,
    Size = size(Msg),
    Packet = <<Size:32, Msg/binary>>,
    io:format("Sending data ~w bytes~n",[Size]),
    ok = gen_tcp:send(Socket, Packet).
     
send_cancel(State, Begin) -> 
    Socket = State#state.socket,
    Index = State#state.current_piece,
    BlockSize = determine_blocksize(State, Index, Begin),
    Msg = <<8, Index:32, Begin:32, BlockSize:32>>,
    MsgSize = size(Msg),
    Packet = <<MsgSize:32, Msg/binary>>,
    %% info_msg("Sending cancel: index ~w, begin ~w, size ~w~n",
    %% 	     [Index, Begin, BlockSize]),
    ok = gen_tcp:send(Socket, Packet),
    State#state{pending = lists:delete(Begin, State#state.pending)}.

%%% ----------------------------------------------------------------------
%%% HANDLE RECEIVED PEER WIRE MESSAGES
%%% ----------------------------------------------------------------------

handle_msg(0, _, State) -> % choke
    %% info_msg("Received message: ~w from ~p~n",[choke, State#state.remote_peer_id
					      %% ]),
    set_status(State#state.peer_key, peer_choking, peer_choking),
    {noreply, State};
handle_msg(1, _, State) -> % unchoke
    %% info_msg("Received message: ~w from ~p~n",[unchoke,State#state.remote_peer_id]),
    set_status(State#state.peer_key, peer_choking, peer_not_choking),
    case State#state.current_piece of
	undefined ->
	    {noreply, State};
	Index ->
	    {reply, ok, NewState} = 
		handle_call({retrieve_piece, Index}, from, State),
	    {noreply, NewState}
    end;
handle_msg(2, _, State) -> % interested
    %% info_msg("Received message: ~w from ~p~n",[interested, State#state.remote_peer_id]),
    set_status(State#state.peer_key, peer_interested, peer_interested),
    {noreply, State};
handle_msg(3, _, State) -> % not interested
    %% info_msg("Received message: ~w from ~p~n",[not_interested, State#state.remote_peer_id]),
    set_status(State#state.peer_key, peer_interested, peer_not_interested),
    {noreply, State};
handle_msg(4, Msg, State) -> % have
    %% info_msg("Have: ~p (~p)~n",[Msg, State#state.remote_peer_id]),
    <<Index:32>> = Msg, % Remember zero-based index
    Key = {State#state.info_hash, State#state.remote_peer_id},
    {atomic, ok} = 
	mnesia:transaction(
	  fun() ->
		  [Bitfield] =  mnesia:wread({eTorrentBitfield, Key}),
		  New=setelement(Index+1,Bitfield#eTorrentBitfield.bitfield,1),
		  mnesia:write(Bitfield#eTorrentBitfield{bitfield=New})
	  end),
    {noreply, State};
handle_msg(5, Msg, State) -> % bitfield
    BitField = bitfield_bin2tuple(Msg, State#state.pieces_number),
    info_msg("Bitfield: ~p (~p)~n",[BitField, State#state.remote_peer_id]),
    Key = {State#state.info_hash, State#state.remote_peer_id},
    {atomic, ok} = 
	mnesia:transaction(
	  fun() -> 
		  mnesia:write(#eTorrentBitfield{key = Key,
						 bitfield = BitField})
	  end),
    {noreply, State};
handle_msg(6, Msg, State) -> % request
    <<Index:32, Begin:32, Length:32>> = Msg,
    info_msg("Received message: request~n"
	     "Index: ~w~n"
	     "Begin: ~w~n"
	     "Length: ~w~n",[Index, Begin, Length]),
    case is_cancelled(Index, Begin, Length) of
	true ->
	    info_msg("Request cancelled ~w ~w ~w~n",[Index, Begin, Length]),
	    {noreply, State};
	false ->
	    {ok, Block} =
		case file:open(State#state.path, [read, raw, binary]) of
		    {ok, Fd} -> 
			Start = State#state.piece_length*Index+Begin,
			{ok, B} = file:pread(Fd, Start, Length),
			file:close(Fd),
			{ok, B};
		    {error, enoent} -> 
			PiecePath = get_piece_path(State#state.path, Index),
			case file:open(PiecePath, [read, raw, binary]) of
			    {ok, Fd} -> 
				{ok, B} = file:pread(Fd, Begin, Length),
				file:close(Fd),
				{ok, B};
			    {error, Reason} ->
				error_msg("Problem accessing ~p~n",[PiecePath]),
				gen_tcp:close(State#state.socket),
				erlang:error(Reason, [6, Msg, State])
			end;
		    {error, Reason2} ->
			error_msg("Problem accessing ~p~n",
				  [State#state.path]),
			gen_tcp:close(State#state.socket),
			erlang:error(Reason2, [6, Msg, State])
		end,
	    mnesia:transaction(
	      fun() ->
		      [Torrent] = mnesia:wread({eTorrent, State#state.info_hash}),
		      Uploaded = Torrent#eTorrent.uploaded,
		      mnesia:write(Torrent#eTorrent{uploaded = Uploaded + Length})
	      end),
	    send_piece(State#state.socket,Index, Begin, Block),
	    {noreply, State}
    end;
handle_msg(7, Msg, State) -> % piece
    <<Index:32, Begin:32, Block/binary>> = Msg,
    ets:insert(State#state.blockstore, {Begin, Block}),
    InfoHash = State#state.info_hash,
    Length = size(Block),
    {atomic, Torrent} = 
	mnesia:transaction(
	  fun() ->
		  [T] = mnesia:wread({eTorrent, InfoHash}),
		  Downloaded = T#eTorrent.downloaded,
		  NewTorrent = T#eTorrent{downloaded = Downloaded + Length},
		  mnesia:write(NewTorrent),
		  NewTorrent
	  end),
    case State#state.pending of
	[_] ->
	    Bin = assemble_piece(State#state.blockstore),
	    Sha = crypto:sha(Bin), 
	    %% We're only after fixed information so we can do dirty here
	    case lists:nth(Index+1, Torrent#eTorrent.pieces) of
		Sha -> 
		    PiecePath = get_piece_path(State#state.path, Index),
		    filelib:ensure_dir(PiecePath),
		    ok = file:write_file(PiecePath, Bin),
		    %% info_msg("Piece ~w retrieved~n",[Index]),
		    io:format("~5w\r",[Index]),
		    send_have(Index, InfoHash),
		    PeerId = State#state.remote_peer_id,
		    eTorrentClient:piece_complete(InfoHash, Index, PeerId),
		    {noreply, State#state{pending = [],
					  current_piece = undefined}};
		Expected ->
		    error_logger:error_report(
		      [{info_hash, InfoHash},
		       {peer_id, State#state.remote_peer_id},
		       {index, Index},
		       {actual_sha, Sha},
		       {expected_sha, Expected}]),
		    eTorrentClient:piece_failed(InfoHash, Index),
		    {noreply, State#state{pending = [],
					  current_piece = undefined}}
	    end;
	Pending ->
	    NewPending = lists:delete(Begin, Pending),
	    case State#state.blocks of
		[NextBlock|Blocks] ->
		    Size = determine_blocksize(State, Index, NextBlock),
		    case get_status(State#state.peer_key, peer_choking) of
			peer_choking ->
			    {noreply, State#state{pending = NewPending}};
			peer_not_choking ->
			    send_request(State#state.socket, Index, NextBlock, 
					 Size),
			    NewState = State#state{
					 pending = [NextBlock|NewPending], 
					 blocks = Blocks},
			    {noreply, NewState}
		    end;
		[] ->
		    {noreply, State#state{pending = NewPending}}
	    end
    end;

handle_msg(8, _, State) -> % cancel
    %% This message is not to be handled in order.
    %% It must be handled before any request msg
    {noreply, State};
handle_msg(20, <<0:8, Msg/binary>>, State) ->
    Dict = bencode:decode(binary_to_list(Msg)),
    info_msg("Received message: ~w~n~p~n",[extended, Dict]),
    {noreply, State};
handle_msg(Mid, Msg, State) ->
    warning_msg("Received unknown message ~w~n~p~n",[Mid, Msg]),
    {noreply, State}.

get_piece_path(Path, Index) ->
    Dir = filename:dirname(Path),
    Name = filename:basename(Path)++".p"++integer_to_list(Index),
    filename:join([Dir, ".pieces", Name]).



%% Block size is normally 2^14 = 16384 bytes
%% But the last block of the last piece may be shorted due to the file size
determine_blocksize(State, Index, Block) ->
    case Index+1 of % remember zero based index
	S when S == State#state.pieces_number -> % Last piece
	    LastPiece = State#state.file_size rem State#state.piece_length,
	    case (LastPiece div 16384) * 16384 of
		Block -> 
		    %% Last block
		    %% If file size is a multiple of 16384, then we won't
		    %% get here, because the piece is complete already
		    State#state.file_size rem 16384; 
		_ ->
		    16384
	    end;
	_ ->
	    16384
    end.


assemble_piece(Tab) ->
    Blocks = ets:tab2list(Tab),
    ets:delete_all_objects(Tab),
    SortedBlocks = lists:keysort(1, Blocks),
    lists:foldl(fun({_, B}, A) ->
			<<A/binary, B/binary>>
		end, <<>>, SortedBlocks).
    
set_status(Key, Field, Value) ->
    {atomic, ok} = 
	mnesia:transaction(fun() -> do_set_status(Key, Field, Value) end).

do_set_status(Key, Field, Value) ->
    [Peer] = mnesia:read({eTorrentPeer, Key}),
    case Field of
	am_choking ->
	    mnesia:write(Peer#eTorrentPeer{am_choking = Value});
	am_interested ->
	    mnesia:write(Peer#eTorrentPeer{am_interested = Value});
	peer_choking ->
	    mnesia:write(Peer#eTorrentPeer{peer_choking = Value});
	peer_interested->
	    mnesia:write(Peer#eTorrentPeer{peer_interested = Value})
    end.

get_status(Key, Field) ->	    
    {atomic, [Peer]} =
	mnesia:transaction(fun() -> mnesia:read({eTorrentPeer, Key}) end),
    case Field of
	am_choking -> Peer#eTorrentPeer.am_choking;
	am_interested -> Peer#eTorrentPeer.am_interested;
	peer_choking -> Peer#eTorrentPeer.peer_choking;
	peer_interested -> Peer#eTorrentPeer.peer_interested
    end.
			 
is_cancelled(Index, Begin, Length) ->
    case process_info(self(), messages) of
	{messages, []} ->
	    false;
	{messages, Msgs} ->
	    Msg = <<Index:32, Begin:32, Length:32>>,
	    is_cancelled(Msg, Msgs)
    end.

is_cancelled(Msg, [{'gen_cast', {msg, 8, Msg}}|_]) ->
    true;
is_cancelled(Msg, [_|Msgs]) ->
    is_cancelled(Msg, Msgs);
is_cancelled(_, []) ->
    false.
    
	    
				    


%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    error_logger:error_msg("~w: "++Format, [?MODULE|Args]).


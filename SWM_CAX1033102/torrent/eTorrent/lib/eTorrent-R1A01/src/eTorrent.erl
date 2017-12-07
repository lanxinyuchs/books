-module(eTorrent).
-behaviour(gen_server).

%% User interface
-export([get_storage_dir/0]).
-export([get_peer_id/0]).
-export([download_torrent/1, download_file/1, select_file/0]).

%% Application interface
-export([start/2, stop/1]). 

%% Supervisor interface
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-include("eTorrent.hrl").

%%% ----------------------------------------------------------------------
%%% USER INTERFACE
%%% ----------------------------------------------------------------------


get_storage_dir() ->
    case application:get_env(eTorrent, storage_dir) of
	{ok, Dir} -> 
	    Dir;
	undefined ->
	    {ok, Dir} = file:get_cwd(),
	    Dir
    end.

get_peer_id() ->
    gen_server:call(?MODULE, get_peer_id).

download_torrent(URI) ->
    gen_server:call(?MODULE, {download_torrent, URI}).

download_file(InfoHash) ->
    gen_server:call(?MODULE, {download_file, InfoHash}).

select_file() ->
    case ets:tab2list(eTorrent) of
	[] ->
	    io:format("No torrents~n");
	Torrents ->
	    io:format("     ~-9s ~-40s ~-30s~n",["Size", "Name","Tracker"]),
	    list_files(Torrents, 1),
	    {ok, Int} = io:read("Select file: "),
	    Torrent = lists:nth(Int, Torrents),
	    download_file(Torrent#eTorrent.info_hash)
    end.
      
list_files([], _) ->
    ok;
list_files([Torrent|Torrents], N) ->
    io:format("~3w. ~9w ~-40s ~-30s~n",[N, Torrent#eTorrent.length,
				      Torrent#eTorrent.name,
				      Torrent#eTorrent.announce]),
    list_files(Torrents, N+1).


%%% ----------------------------------------------------------------------
%%% APPLICATION INTERFACE
%%% ----------------------------------------------------------------------

%% start(StartType, StartArgs) ->
start(_, _) ->
    create_table(eTorrent, 
		 [{ram_copies, [node()]},
		  {type, set},
		  {attributes, record_info(fields, eTorrent)}]),
    create_table(eTorrentBitfield,
		 [{ram_copies, [node()]},
		  {type, set},
		  {attributes, record_info(fields, eTorrentBitfield)}]),
    %% create_table(eTorrentClient,
    %% 		 [{ram_copies, [node()]},
    %% 		  {type, set},
    %% 		  {attributes, record_info(fields, eTorrentClient)}]),
    create_table(eTorrentPeer,
    		 [{ram_copies, [node()]},
    		  {type, set},
    		  {attributes, record_info(fields, eTorrentPeer)}]),
    eTorrentSuper:start_link().

%% stop(State) ->
stop(_) ->
    ok.

%%% ----------------------------------------------------------------------
%%% SUPERVISOR INTERFACE
%%% ----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% ----------------------------------------------------------------------
%%% GEN_SERVER INTERFACE
%%% ----------------------------------------------------------------------

-record(state, {paths,    % [string()] - known torrents
		peer_id,  % string()   - current peerid
		port      % integer()  - current listen port
	       }).

init(_) ->
    {A,B,C} = os:timestamp(),
    random:seed(A,B,C),
    Base = "ERIC-R1A01-",
    PeerId = Base++[random()||_<-lists:seq(1,20-length(Base))],

    Dir = get_storage_dir(),
    Paths = read_torrents(Dir),
    {ok, #state{peer_id=PeerId, paths=Paths}}.

handle_call({download_torrent, URI}, _, State) ->
    URI,
    {reply, ok, State};
handle_call({download_file, InfoHash}, _, State) ->
    case mnesia:dirty_read({eTorrent, InfoHash}) of
	[Torrent] ->
	    Args = [Torrent#eTorrent.info_hash, 
		    State#state.peer_id, State#state.port],
	    eTorrentClient:start(Args),	
	    %% info_msg("Client started ~p~n",[R]),
	    {reply, ok, State};
	[] ->
	    {reply, no_such_torrent, State}
    end;

handle_call(get_peer_id, _, State) ->
    {reply, {ok, State#state.peer_id}, State}.

handle_cast(check_download_status, State) ->
    handle_check_download_status(State),
    {noreply, State};
handle_cast({port, Port}, State) ->
    error_logger:info_report(
      progress, [{eTorrent, listening},
		 {peerId, State#state.peer_id},
		 {port, Port}]),
    gen_server:cast(self(), check_download_status),
    {noreply, State#state{port=Port}};
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

create_table(Name, Attrs) ->
    Tables =  mnesia:system_info(tables),
    case lists:member(Name, Tables) of
	true ->
	    ok;
	false ->
	    {atomic, ok} = mnesia:create_table(Name, Attrs),
	    ok
    end.



read_torrents(Dir) ->
    Pattern = filename:join(Dir, "*.torrent"),
    Paths = filelib:wildcard(Pattern),
    [begin 
	 Torrent=read_torrent(Path),
	 save_torrent(Torrent),
	 Path
     end||Path<-Paths].


read_torrent(Path) ->
    {ok, Bin} = file:read_file(Path),
    Data = bencode:decode(binary_to_list(Bin)),
    sort_torrent_data(Data, #eTorrent{path=Path}).


save_torrent(Torrent) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(Torrent)
			    end) of
	{atomic, ok} -> ok;
	{aborted, Reason} ->
	    error_msg("mnesia:write(~p) = ~n~p~n",
		      [Torrent,{aborted, Reason}])
    end.
    

sort_torrent_data([{<<"announce">>, Announce}|Data], Torrent) ->
    sort_torrent_data(Data, Torrent#eTorrent{announce = binary_to_list(Announce)});
sort_torrent_data([{<<"comment">>, Comment}|Data], Torrent) ->
    sort_torrent_data(Data, Torrent#eTorrent{comment = binary_to_list(Comment)});
sort_torrent_data([{<<"creation date">>, Date}|Data], Torrent) ->
    sort_torrent_data(Data, Torrent#eTorrent{creation_date = bin2int(Date)});
sort_torrent_data([{<<"info">>, Info}], Torrent) ->
    InfoStr = bencode:encode(Info),
    Hash = crypto:sha(InfoStr),
    sort_torrent_data(Info, Torrent#eTorrent{info_hash = Hash});
sort_torrent_data([{<<"length">>, Length}|Data], Torrent) ->
    sort_torrent_data(Data, Torrent#eTorrent{length = bin2int(Length)});
sort_torrent_data([{<<"name">>, Name}|Data], Torrent) ->
    sort_torrent_data(Data, Torrent#eTorrent{name = binary_to_list(Name)});
sort_torrent_data([{<<"piece length">>, Length}|Data], Torrent) ->
    sort_torrent_data(Data, Torrent#eTorrent{piece_length = bin2int(Length)});
sort_torrent_data([{<<"pieces">>, Pieces}|Data], Torrent) ->
    PiecesList = split_pieces(Pieces),
    Length = length(PiecesList),
    Bitfield = list_to_tuple([0||_<-lists:seq(1,Length)]),
    sort_torrent_data(Data, 
		       Torrent#eTorrent{pieces_number = Length,
					pieces = PiecesList,
					bitfield = Bitfield});
sort_torrent_data([], Torrent) ->
    Torrent.
		       
split_pieces(<<>>) ->[];
split_pieces(<<Piece:20/binary, Pieces/binary>>) ->    
    [Piece|split_pieces(Pieces)].
		       
bin2int(Binary) -> 
    Binary.


handle_check_download_status(State) ->
    {atomic, ok} = mnesia:transaction(
		     fun() -> do_handle_check_download_status() end),
    PeerId = State#state.peer_id,
    Port = State#state.port,
    [begin
	 R = eTorrentClient:start([Torrent#eTorrent.info_hash, PeerId, Port]),	
	 io:format("~p~n",[R])
     end||Torrent<-ets:tab2list(eTorrent),
	  Torrent#eTorrent.status == complete],
    ok.

do_handle_check_download_status() ->
    do_handle_check_download_status(mnesia:first(eTorrent)).

do_handle_check_download_status('$end_of_table') ->
    ok;
do_handle_check_download_status(Key) ->
    [Torrent] = mnesia:wread({eTorrent, Key}),
    DirName = filename:dirname(Torrent#eTorrent.path),
    TargetPath = filename:join(DirName, Torrent#eTorrent.name),
    case filelib:is_file(TargetPath) of
    	true ->
	    BitField = list_to_tuple(
			 [1||_<-lists:seq(1, Torrent#eTorrent.pieces_number)]),
    	    mnesia:write(Torrent#eTorrent{status=complete,
					  bitfield = BitField,
					  left=0});
    	false ->
	    Remaining = calculate_remaining(Torrent),
	    mnesia:write(Torrent#eTorrent{left=Remaining})
    end,
    do_handle_check_download_status(mnesia:next(eTorrent, Key)).

calculate_remaining(Torrent) ->
    DirName = filename:dirname(Torrent#eTorrent.path),
    FileName = Torrent#eTorrent.name,
    PiecesDir = filename:join([DirName, ".pieces", FileName]),
    Size = Torrent#eTorrent.length,
    case file:list_dir(PiecesDir) of
	{ok, Files} -> 
	    Size - calculate_file_sizes(PiecesDir, Files);
	{error, _} ->
	    Size
    end.

calculate_file_sizes(PiecesDir, Files) ->
    lists:foldl(fun(File, Accu) ->
			Path = filename:join(PiecesDir, File),
			Accu + filelib:file_size(Path)
		end, 0, Files).

%%% ----------------------------------------------------------------------
%%% GENERICx FUNCTIONS LIBRARY
%%% ----------------------------------------------------------------------

%% This is not the complete uri set as the reserved chars are not included
%% Avoiding those means not having to encode the peer id, and with 71 
%% remaining characters the uniqueness is pretty much guaranteed

-define(uri_set,
	"0123456789"
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	"acbdefghijklmnopqrstuvwxyz"
	"-_.!~*'()").

random() ->
    lists:nth(random:uniform(length(?uri_set)), ?uri_set).

%% info_msg(Format) ->
%%     info_msg(Format, []).
%% info_msg(Format, Args) ->
%%     error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
error_msg(Format, Args) ->
    error_logger:error_msg("~w: "++Format, [?MODULE|Args]).

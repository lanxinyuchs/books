-module(eTrackerServer).

%% API
-export([handle_request/4]).

%% Supervisor interface

-export([start_link/0]).

%% Internal interface

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-include("eTracker.hrl").
-include("eTrackerInternal.hrl").

%%% ----------------------------------------------------------------------
%%% API
%%% ----------------------------------------------------------------------

handle_request(Socket, Uri, HttpValues, Data) ->
    case Data of
	undefined -> ok;
	_ -> 
	    warning_msg("Unexpected body in http request ~n"
			"HTTP = ~p~nURI = ~p ~nData = ~p~n",
			[Uri, HttpValues, Data])
    end,
    {ok, Request} = decode_request(Uri),
    Response = 
	case gen_server:call(?MODULE, {handle_request, HttpValues, Request}) of
	    {ok, R} -> R;
	    Any ->
		error_msg("handle_request failed: ~p~n",[Any]),
		#response{failure_reason = "General tracker failure"}
	end,
    send_response(Socket, Response).

%%% ----------------------------------------------------------------------
%%% SUPERVISOR INTERFACE
%%% ----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
%%% ----------------------------------------------------------------------
%%% GEN_SERVER INTERFACE
%%% ----------------------------------------------------------------------

init([]) ->
    ets:new(eTracker, [public, set, named_table]),
    {ok, state}.

handle_call({handle_request, HttpValues, Request}, _, State) ->
    %% Fields = record_info(fields, request),
    %% Info = lists:zip(Fields, tl(tuple_to_list(Request))),
    %% error_logger:info_report([{eTracker, request}|Info]),
%    info_msg("Request = ~n~p~n",[Request]),
    update_peer_table(HttpValues, Request),
    Response = make_response(Request),
    {reply, {ok, Response}, State};
handle_call(Request, From, State) ->
    warning_msg("Unknown request: ~p from ~p~n",[Request, From]),
    {reply, ok, State}.

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

decode_request({abs_path, Uri}) ->
    {_, Query} = httpd_util:split_path(Uri),
    [_|RawData] = eTracker:decode_uri(Query),
    decode_request(string:tokens(RawData, "&"), #request{}).

decode_request([Item|Items], Request) ->
    case string:tokens(Item, "=") of
	["info_hash", Value] -> 
	    decode_request(Items, Request#request{
				    info_hash = list_to_binary(Value)});
	["peer_id", Value] -> 
	    decode_request(Items, Request#request{peer_id = Value});
	["port", Value] -> 
	    decode_request(Items, Request#request{
				    port = list_to_integer(Value)});
	["uploaded", Value] -> 
	    decode_request(Items, Request#request{
				    uploaded = list_to_integer(Value)});
	["downloaded", Value] -> 
	    decode_request(Items, Request#request{
				    downloaded = list_to_integer(Value)});
	["left", Value] -> 
	    decode_request(Items, Request#request{
				    left = list_to_integer(Value)});
	["compact", Value] -> 
	    decode_request(Items, Request#request{compact = Value});
	["no_peer_id", Value] -> 
	    decode_request(Items, Request#request{no_peer_id = Value});
	["event", Value] -> 
	    decode_request(Items, Request#request{event = Value});
	["ip", Value] -> 
	    decode_request(Items, Request#request{ip = Value});
	["numwant", Value] -> 
	    decode_request(Items, Request#request{
				    numwant = list_to_integer(Value)});
	["key", Value] -> 
	    decode_request(Items, Request#request{key = Value});
	["tracker_id", Value] -> 
	    decode_request(Items, Request#request{tracker_id = Value});
	["supportcrypto", Value] ->
	    decode_request(Items, Request#request{supportcrypto = Value})
    end;
decode_request([], Request) ->
    {ok, Request}.

send_response(Socket, Response) ->
    Msg = encode_response(Response),
    HttpHeader = 
        "HTTP/1.1 200 OK\r\n"
        "Connection: close\r\n"
        "Content-length: "++integer_to_list(length(Msg))++"\r\n"
        "Content-Type: text/plain\r\n"
        "Date: "++httpd_util:rfc1123_date()++"\r\n"
        "Server: eTorrent\r\n"
        "\r\n",
%    io:format("Sending: ~n~s~n",[HttpHeader++Msg]),
    gen_tcp:send(Socket, HttpHeader++Msg),
    gen_tcp:close(Socket).

encode_response(Response) ->
    case Response#response.failure_reason of
	undefined ->
	    Fields = record_info(fields, response),
	    TupleList = lists:zip(Fields, tl(tuple_to_list(Response))),
	    Dict = do_encode_response(TupleList),
	    bencode:encode(Dict);
	Failure ->
	    bencode:encode([{<<"failure reason">>, list_to_binary(Failure)}])
    end.

do_encode_response([{_, undefined}|Response]) ->
    do_encode_response(Response);
%% do_encode_response([{failure_reason, _}|Response]) ->
%%     do_encode_response(Response);
do_encode_response([{warning_message, Warning}|Response]) ->    
    [{<<"warning message">>, list_to_binary(Warning)}|
     do_encode_response(Response)];
do_encode_response([{interval, Interval}|Response]) ->    
    [{<<"interval">>, Interval}|do_encode_response(Response)];
do_encode_response([{min_interval, Interval}|Response]) ->    
    [{<<"min interval">>, Interval}|do_encode_response(Response)];
do_encode_response([{tracker_id, TrackerId}|Response]) ->    
    [{<<"tracker id">>, list_to_binary(TrackerId)}|
     do_encode_response(Response)];
do_encode_response([{complete, Complete}|Response]) ->    
    [{<<"complete">>, Complete}|do_encode_response(Response)];
do_encode_response([{incomplete, Incomplete}|Response]) ->    
    [{<<"incomplete">>, Incomplete}|do_encode_response(Response)];
do_encode_response([{peers, []}|Response]) -> 
    [{<<"peers">>,<<"">>}|do_encode_response(Response)];
do_encode_response([{peers, Peers}|Response]) ->    
    Enc = 
	case hd(Peers) of
	    Peer when is_list(Peer) ->
		Peers;
	    _ ->
		list_to_binary(Peers)
	end,
    [{<<"peers">>, Enc}|do_encode_response(Response)];
do_encode_response([]) ->
    [].

ipstr(Ip) when size(Ip) == 4->
    ipstr(tuple_to_list(Ip));
ipstr([A,B,C,D]) ->
    list_to_binary(lists:flatten(io_lib:format("~w.~w.~w.~w",[A,B,C,D]))).

%% encode_peer({PeerId, Ip, Port}) ->    
%%     [{<<"peer id">>, PeerId},
%%      {<<"ip">>, ipstr(Ip)}, 
%%      {<<"port">>, list_to_binary(integer_to_list(Port))}].

update_peer_table(HttpValues, Request) ->
    case mnesia:transaction(fun() -> 
				    do_update_peer_table(HttpValues, Request)
			    end) of
	{atomic, ok} -> ok;
	{aborted, Reason} ->
	    error_logger:error_report(
	      [{mfa, {?MODULE, do_update_peer_table, [HttpValues, Request]}},
	       {aborted, Reason}]),
	    ok
    end.

do_update_peer_table(_, Request) when Request#request.event == "stopped" ->
    Key = {Request#request.info_hash, Request#request.peer_id},
    case mnesia:read({eTrackerPeer, Key}) of
	[Obj] ->
	    case {Obj#eTrackerPeer.tracker_id, Request#request.tracker_id} of
		{undefined, _} -> 
		    ok;
		{Same, Same} ->
		    ok;
		_ ->
		    mnesia:abort(tracker_id_mismatch)
	    end,
	    mnesia:delete({eTrackerPeer, Key});
	[] ->
	    ok
    end;
do_update_peer_table(HttpValues, Request) ->
    Key = {Request#request.info_hash, Request#request.peer_id},
    case mnesia:read({eTrackerPeer, Key}) of
	[Obj] ->
	    case {Obj#eTrackerPeer.tracker_id, Request#request.tracker_id} of
		{undefined, _} -> 
		    ok;
		{Same, Same} ->
		    ok;
		_ ->
		    mnesia:abort(tracker_id_mismatch)
	    end,
	    Uploaded = Request#request.uploaded,
	    Downloaded = Request#request.downloaded,
	    Left = Request#request.left,
	    NewPeer = Obj#eTrackerPeer{uploaded = Uploaded,
				       downloaded = Downloaded,
				       left = Left},
	    mnesia:write(NewPeer);
	[] ->
	    Ip = 
		case Request#request.ip of
		    undefined ->
			{PeerIp,_} = proplists:get_value(peer, HttpValues),
			ipstr(PeerIp);
		    ThisIp ->
			ThisIp
		end,
	    InfoHash = Request#request.info_hash, 
	    Port = Request#request.port,
	    garbage_collect_on_address(InfoHash, Ip, Port),
	    Uploaded = Request#request.uploaded,
	    Downloaded = Request#request.downloaded,
	    Left = Request#request.downloaded,
	    Compact = case Request#request.compact of
			  "1" -> compact;
			  "0" -> dict
		      end,

	    NewPeer = #eTrackerPeer{key = Key, ip = Ip, port = Port,
				    uploaded = Uploaded, 
				    downloaded = Downloaded, 
				    left = Left,
				    compact = Compact},
	    mnesia:write(NewPeer)
    end.

garbage_collect_on_address(InfoHash, Ip, Port) ->
    WP = mnesia:table_info(eTrackerPeer, wild_pattern),
    Pattern = WP#eTrackerPeer{ip = Ip, port = Port},
    Objs = mnesia:match_object(Pattern),
    [mnesia:delete_object(Obj)||
	Obj<-Objs, element(1, Obj#eTrackerPeer.key) == InfoHash],
    ok.


make_response(Request) ->
    InfoHash = Request#request.info_hash,
    PeerId = Request#request.peer_id,
    NumWant = Request#request.numwant,
    case mnesia:transaction(fun() -> 
				    do_make_response(InfoHash, PeerId, NumWant) 
			    end) of
	{atomic, Response} ->
	    Response;
	{aborted, Reason} ->
	    error_logger:error_report(
	      [{?MODULE, make_response, [Request]},
	       {mfa, {?MODULE, do_make_response, [InfoHash, PeerId, NumWant]}},
	       {aborted, Reason}]),
	    #response{failure_reason="Database failure"}
    end.

do_make_response(InfoHash, PeerId, NumWant) ->
    WP = mnesia:table_info(eTrackerPeer, wild_pattern),
    Pattern = WP#eTrackerPeer{key={InfoHash, '_'}},
    OwnKey = {InfoHash, PeerId},
    Peers = lists:keydelete(OwnKey, 2, mnesia:match_object(Pattern)),
    case mnesia:read({eTrackerPeer, OwnKey}) of
	[ThisPeer] ->
	    case length(Peers) of
		Length when Length =< NumWant ->
		    format_response(Peers, ThisPeer#eTrackerPeer.compact,
				    ThisPeer#eTrackerPeer.tracker_id);
		_ -> %%% FIXME: Find a better tracker selection algorithm
		    Pattern2 = Pattern#eTrackerPeer{left=0},
		    Completed = lists:keydelete(OwnKey, 2, 
						mnesia:match_object(Pattern2)),
		    case length(Completed) of
			LengthC when LengthC =< NumWant ->
			    format_response(Completed, 
					    ThisPeer#eTrackerPeer.compact,
					    ThisPeer#eTrackerPeer.tracker_id);
			_ ->
			    {Offered, _} = lists:split(NumWant, Completed),
			    format_response(Offered, 
					    ThisPeer#eTrackerPeer.compact,
					    ThisPeer#eTrackerPeer.tracker_id)
		    end
	    end;
	[] ->
	    #response{failure_reason = "No tracker data available"}
    end.

format_response(Peers, Compact, TrackerId) ->
    {NewPeers, Complete, Incomplete} = format_response(Compact, Peers),
    #response{tracker_id = TrackerId,
	      interval = get_interval(),
	      min_interval = get_min_interval(),
	      complete = Complete,
	      incomplete = Incomplete,
	      peers = NewPeers
	     }.
    
format_response(Compact, [Peer|Peers]) ->
    {NewPeers, Complete, Incomplete} = format_response(Compact, Peers),
    if Peer#eTrackerPeer.left == 0 ->
	    {format_peer(Peer, Compact, NewPeers), Complete+1, Incomplete};
       true ->
	    {format_peer(Peer, Compact, NewPeers), Complete, Incomplete+1}
    end;
format_response(_, []) ->
    {[], 0, 0}.

format_peer(Peer, compact, NewPeers) ->
    IpStr = binary_to_list(Peer#eTrackerPeer.ip),
    Port = Peer#eTrackerPeer.port,
    {ok, Ip} = inet_parse:address(IpStr),
    lists:flatten([tuple_to_list(Ip), binary_to_list(<<Port:16>>), NewPeers]);

format_peer(Peer, dict, NewPeers) ->
    [[{<<"peer id">>, list_to_binary(element(2, Peer#eTrackerPeer.key))},
      {<<"ip">>, Peer#eTrackerPeer.ip},
      {<<"port">>, Peer#eTrackerPeer.port}]|NewPeers].

get_interval() -> %% FIXME: Get me some logic
    15.

get_min_interval() -> %% FIXME: Get me some logic
    undefined.
    

    
%% info_msg(Format) ->
%%     info_msg(Format, []).
%% info_msg(Format, Args) ->
%%     error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    error_logger:error_msg("~w: "++Format, [?MODULE|Args]).

#!/usr/bin/env escript
%% -*- erlang -*-
-module(logtest).

main([PortStr, Severity, LogName, Msg]) ->
    try main1([PortStr, Severity, LogName, Msg]) of
	Res ->
	    Res
    catch T:E ->
	    io:format("{~p, ~p}~n~p~n",[T,E,erlang:get_stacktrace()])
    end;
main(_) ->
    io:format("logtest.escript Port Severity LogName Message~n"
	      "Used to make entries in any system log.~n"
	      "Port - port number of the log service (found in port.conf)~n"
	      "Severity - emergency, alert, critical, error~n"
	      "           warning, notice or info~n"
	      "LogName - The name of the log~n"
	      "Message - The message~n").

    
main1([PortStr, Severity, LogName, Msg]) ->
    Port = list_to_integer(PortStr),
    %% io:format("Port = ~w~n",[Port]),
    {A,B,C} = erlang:now(),
    TimeStamp = A*1000000+B,
    %% io:format("Timestamp = ~w~n",[TimeStamp]),
    LogAck = integer_to_list(TimeStamp*1000000+C),
    %% io:format("LogAck = ~p~n",[LogAck]),
    %% io:format("Severity = ~p~n",[Severity]),
    SevToken = case Severity of
		   "emergency" -> 0;
		   "alert" -> 1;
		   "critical" -> 2;
		   "error" -> 3;
		   "warning" -> 4;
		   "notice" -> 5;
		   "info" -> 6
	       end,
    %% io:format("SevToken = ~w~n",[SevToken]),
    Dict = 
	[{<<"logName">>, list_to_binary(LogName)},
	 {<<"logSvcUsrName">>, <<"logtest">>},
	 {<<"logSeverity">>, SevToken},
	 {<<"logTimeStamp">>, TimeStamp},
	 {<<"logAck">>, list_to_binary(LogAck)},
	 {<<"logBuffer">>, list_to_binary(Msg)}],

    LogMsg = [<<"write_log">>, Dict],
    %% io:format("Msg = ~n~p~n",[LogMsg]),
    Bencode = encode(LogMsg),
    %% io:format("sending message: ~s~n",[Bencode]),
    Options = [{active, true}, 
	       {keepalive, true}, 
	       {packet, tpkt},
	       {reuseaddr, true}],
	       
    {ok, Socket} = gen_tcp:connect("localhost", Port, Options, 1000),
    Size = length(Bencode)+4,
    Binary = list_to_binary(Bencode),

    gen_tcp:send(Socket, <<3,0,Size:16,Binary/binary>>),


    await_reply(Socket).

await_reply(Socket) ->
    receive 
	{tcp, Socket, [_,_,_,_|Data]} ->
	    case decode(Data) of
		[<<"log_ack">>,Dict] ->
		    [_, _, {<<"logResult">>,ResultB}] = Dict,
		    io:format("Result: ~s~n",[binary_to_list(ResultB)]),
		    gen_tcp:close(Socket);
		Unknown ->
		    io:format("Unknown response ~p~n",[Unknown]),
		    erlang:error({unknown, Unknown}, [Socket])
	    end;
	{tcp_closed, Socket} ->
	    erlang:error(unexpected_close, [Socket]);
	{tcp_error, Socket, Reason} ->
	    erlang:error({tcp_error, Reason}, [Socket])
    end.

	
    
    
    
    

%%% @doc Decode a bencoded message

-type bencode_type() :: integer() | binary() | [bencode_type()] | 
			[{binary(), bencode_type()}].

-spec decode(Message::string()) -> bencode_type().

decode(Message) ->
    {Data, ""} = do_decode(Message),
    Data.

%%% @hidden

do_decode([$i|Message]) ->
    decode_integer(Message);
do_decode([X|Message]) when X>=$0 andalso X=<$9 ->
    decode_string([X|Message]);
do_decode([$l|Message]) ->
    decode_list(Message);
do_decode([$d|Message]) ->
    decode_dictionary(Message).
    
%%% @hidden

decode_integer(Message) ->
    decode_integer(Message, []).

%%% @hidden

decode_integer([$e|Rest], IntStr) ->
    {list_to_integer(lists:reverse(IntStr)), Rest};
decode_integer([H|T], IntStr) ->
    decode_integer(T, [H|IntStr]).
    
%%% @hidden

decode_string(Message) ->
    decode_string(Message, []).

%%% @hidden

decode_string([$:|Data], LenStr) ->
    Length = list_to_integer(lists:reverse(LenStr)),
    {StringData, Rest} = lists:split(Length, Data),
    {list_to_binary(StringData), Rest}; % separate from list
decode_string([H|T], LenStr) ->
    decode_string(T, [H|LenStr]).

%%% @hidden

decode_list(String) ->
    decode_list(String, []).

%%% @hidden

decode_list([$e|Rest], Accu) ->
    {lists:reverse(Accu), Rest};
decode_list(String, Accu) ->
    {Data, Rest} = do_decode(String),
    decode_list(Rest, [Data|Accu]).

%%% @hidden

decode_dictionary(String) ->
    decode_dictionary(String, []).

%%% @hidden

decode_dictionary([$e|Rest], Accu) ->
    {lists:reverse(Accu), Rest};
decode_dictionary(String, Accu) ->
    {Key, RestT} = do_decode(String),
    {Value, Rest} = do_decode(RestT),
    decode_dictionary(Rest, [{Key,Value}|Accu]).

%%% @doc Encode a bencoded message

-spec encode(bencode_type()) -> string().

encode(X) when is_integer(X) ->
    lists:flatten([$i, integer_to_list(X), $e]);
encode(X) when is_binary(X) ->
    lists:flatten([integer_to_list(size(X)), $:|binary_to_list(X)]);
encode([H|_]=Dict) when is_tuple(H) ->
    lists:flatten([$d, [[encode(Key), encode(Value)]||{Key,Value}<-Dict], $e]);
encode(List) when is_list(List) ->
    lists:flatten([$l, [encode(X)||X<-List], $e]).


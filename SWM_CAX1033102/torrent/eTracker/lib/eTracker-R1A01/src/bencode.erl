-module(bencode).

-export([encode/1, decode/1]).

decode(String) ->
    {Data, ""} = do_decode(String),
    Data.

do_decode([$i|String]) ->
    decode_integer(String);
do_decode([X|String]) when X>=$0 andalso X=<$9 ->
    decode_string([X|String]);
do_decode([$l|String]) ->
    decode_list(String);
do_decode([$d|String]) ->
    decode_dictionary(String).
    
decode_integer(String) ->
    decode_integer(String, []).

decode_integer([$e|Rest], IntStr) ->
    {list_to_integer(lists:reverse(IntStr)), Rest};
decode_integer([H|T], IntStr) ->
    decode_integer(T, [H|IntStr]).
    
decode_string(String) ->
    decode_string(String, []).

decode_string([$:|Data], LenStr) ->
    Length = list_to_integer(lists:reverse(LenStr)),
    {StringData, Rest} = lists:split(Length, Data),
    {list_to_binary(StringData), Rest}; % separate from list
decode_string([H|T], LenStr) ->
    decode_string(T, [H|LenStr]).

decode_list(String) ->
    decode_list(String, []).

decode_list([$e|Rest], Accu) ->
    {lists:reverse(Accu), Rest};
decode_list(String, Accu) ->
    {Data, Rest} = do_decode(String),
    decode_list(Rest, [Data|Accu]).

decode_dictionary(String) ->
    decode_dictionary(String, []).

decode_dictionary([$e|Rest], Accu) ->
    {lists:reverse(Accu), Rest};
decode_dictionary(String, Accu) ->
    {Key, RestT} = do_decode(String),
    {Value, Rest} = do_decode(RestT),
    decode_dictionary(Rest, [{Key,Value}|Accu]).

encode(X) when is_integer(X) ->
    lists:flatten([$i, integer_to_list(X), $e]);
encode(X) when is_binary(X) ->
    lists:flatten([integer_to_list(size(X)), $:|binary_to_list(X)]);
encode([H|_]=Dict) when is_tuple(H) ->
    lists:flatten([$d, [[encode(Key), encode(Value)]||{Key,Value}<-Dict], $e]);
encode(List) when is_list(List) ->
    lists:flatten([$l, [encode(X)||X<-List], $e]).


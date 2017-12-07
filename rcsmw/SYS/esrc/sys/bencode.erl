%%% @author etxberb
%%% @copyright Ericsson AB 2012-2014
%%% @version /main/R1A/R2A/1
%%% @doc Bencode codec
%%%
%%% This module contains a bencode codec translating bencoded
%%% messages to erlang terms and vice versa.
%%%
%%% ==Encoding rules==
%%% ===Simple types===
%%% The basic types of a bencoded message is an <b>integer</b> or a <b>string</b>.
%%% Integers are encoded in base 10 literals enclosed with the letter i and
%%% the letter e. E.g. "i42e"
%%%
%%% A string is encoded using the length of the string in base 10 literals, 
%%% then a colon (:) and then the string in ascii characters. E.g. "5:Hello"
%%% In erlang bencode strings are represented as binaries.
%%%
%%% ===Complex types===
%%% Complex types are lists and dictionaries.
%%%
%%% Lists are encoded using the literals "l" in the beginning and "e" in the end
%%% Lists elements can be simple or complex types in themselves.
%%% Lists are represented in erlang as lists.
%%%
%%% Example:
%%% [1,2,3,4] -> li1ei2ei3ei4ee
%%%
%%% Dictionaries are encoded using the literals "d" in the beginning
%%% and "e" in the end. A dictionary entry consists of a key and a value.
%%% The key must be a string.<br/>
%%% In erlang dictionaries are represented as tuple-lists
%%%
%%% Example:<br/>
%%% [{&lt;&lt; "x" >>, 1}, {&lt;&lt; "banana" >>, &lt;&lt; "monkey" >>}] -><br/>
%%% d1:xi1e6:banana6:monkeye
%%%



%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2014 All rights reserved.
%%% 
%%% The information in this document is the property of Ericsson.
%%% 
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%


-module(bencode).
-vsn('/main/R1A/R2A/1').
-export([encode/1, decode/1]).

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


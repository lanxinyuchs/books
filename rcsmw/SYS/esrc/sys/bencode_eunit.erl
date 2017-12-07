-module(bencode_eunit).
-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
   [
      ?_assert(bencode:encode(0) =:= "i0e"),
      ?_assert(bencode:encode(1) =:= "i1e"),
      ?_assert(bencode:encode([1,2]) =:= "li1ei2ee"),
      ?_assert(bencode:encode(["abc"]) =:= "lli97ei98ei99eee"),
      ?_assertException(error, function_clause, bencode:encode(an_attom))
   ].

decode_test_() ->
   [
      ?_assert(bencode:decode("i0e") =:= 0),
      ?_assert(bencode:decode("li1ei2ee") =:= [1,2]),
      ?_assertException(error, function_clause, bencode:decode("faulty_encode_string"))
   ].

encde_test_() ->
   [
      ?_assert(bencode:decode(bencode:encode(0))      =:= 0),
      ?_assert(bencode:decode(bencode:encode(-1))     =:= -1),
      ?_assert(bencode:decode(bencode:encode(23423))  =:= 23423),
      ?_assert(bencode:decode(bencode:encode("abc")) =:= "abc"),
      ?_assert(bencode:decode(bencode:encode([1,2,3, "abc"])) =:= [1,2,3, "abc"]),
      ?_assert(bencode:decode(bencode:encode([1, [2], [1,2,3]])) =:= [1, [2], [1,2,3]])
   ].

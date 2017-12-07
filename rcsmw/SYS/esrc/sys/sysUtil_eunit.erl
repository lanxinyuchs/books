-module(sysUtil_eunit).
-include_lib("eunit/include/eunit.hrl").

rev_split_test_() ->
   [
      ?_assert(sysUtil:rev_split("R1A") =:= {r_state,ordinary,"1","A",[]}),
      ?_assert(sysUtil:rev_split("P1A") =:= {p_state,ordinary,"1","A",[]}),
      ?_assert(sysUtil:rev_split("R1A/3")  =:= {r_state,special,"1","A","3"}),
      ?_assert(sysUtil:rev_split("P1A/22") =:= {p_state,special,"1","A","22"}),
      ?_assertException(throw, {uc_error, "Invalid revision format", _}, sysUtil:rev_split("P11111A"))
   ].

rev_cmp_test_() ->
   [
      ?_assert(sysUtil:is_rev_equiv("R1A", "R1A") =:= true),
      ?_assert(sysUtil:is_rev_equiv("R1A", "R1B") =:= false),
      ?_assert(sysUtil:is_rev_eqOrHi("R1A", "R1A") =:= true),
      ?_assert(sysUtil:is_rev_eqOrHi("R1A", "R1B") =:= false),
      ?_assert(sysUtil:is_rev_eqOrHi("R2A", "R1B") =:= true),
      ?_assert(sysUtil:is_rev_eqOrHi("R11A", "R11B/22") =:= false),
      ?_assert(sysUtil:is_rev_eqOrHi("R11A/1", "R11B") =:= false)
   ].

#!/usr/bin/env escript
%% -*- erlang -*-

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nextRev.escript
%%% Author:	etxjotj
%%% Description: This script returns the next correct R-state given a valid 
%%%              R-state
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------

-module(nextRev).
-mode(compile).
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Date       Name        What
%%% ---------- --------    ------------------------
%%% 2017-06-15 etxjotj     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([main/1]).
-define(VALID_LETTERS, "ABCDEFGHJKLMNSTUVXYZ").

%%% ----------------------------------------------------------
%%% #           main(Args)
%%% Input: Args::[string()]
%%% Output:
%%% Exceptions:
%%% Description: The starting point of script execution
%%% ----------------------------------------------------------

main([]) ->
    {ok, [Result]} = io:fread(standard_io, "", "~s"),
    do(Result);
main(["--help"]) ->
    io:format("Usage: nextRev.escript [R-state]~n"
	      "~n"
	      "If R-state is not given, it is read from stdin~n"
	      "Set NEXTREV_DEBUG = true for debug printouts",[]);    
%% main(["--loop"]) -> 
%%     loop("R1A01");
main([Input]) ->
    do(Input).

do(Input) ->
    try
	debug("Input: ~p~n",[Input]),
	Split =  validate_rev(Input),
	debug("Parsed: ~p~n",[Split]),
	NextN = next_n(Split),
	NextC = next_c(Split),
	NextV = next_v(Split),
	NextP = next_p(Split),
	io:format("~s:~s:~s:~s~n",[NextN, NextC, NextV, NextP])
    catch Type:Error ->
	    io:format(standard_error, "~w.escript: ~w:~p~n~p~n",
		      [?MODULE, Type, Error, erlang:get_stacktrace()]),
	    erlang:halt(1)	    
    end.

%% loop(Input) ->
%%     Split = validate_rev(Input),
%%     case next_v(Split) of
%% 	"ERROR" ->
%% 	    case next_c(Split) of
%% 		"ERROR" ->
%% 		    case next_n(Split) of
%% 			"ERROR" ->
%% 			    ok;
%% 			NextN ->
%% 			    io:format("~s ******~n",[NextN]),
%% 			    loop(NextN)
%% 		    end;
%% 		NextC ->
%% 		    io:format("~s ***~n",[NextC]),
%% 		    loop(NextC)
%% 	    end;
%% 	NextV ->
%% 	    io:format("~s~n",[NextV]),
%% 	    loop(NextV)
%%     end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           validate_rev(Input)
%%% Input: Input::string()
%%% Output: parsed_rev()
%%% Exceptions:
%%% Description: Validates the input and returns the components of the R-state
%%% Note! These validation rules do not recognized non verification state
%%% R-states. This means further limits on the size of the numbers and letters
%%% components than what's generally allowed. 
%%% The letters validation does not handle the excluded vowel rule for 3 and 4-
%%% letter combinations.
%%% ----------------------------------------------------------


-type parsed_rev()::{State::$P|$R, Number::string(), Letter::string(), VrNo::string()}.
-spec validate_rev(Input::string()) -> parsed_rev().


validate_rev(Input) ->
    try validate_length(Input)
    catch throw:{parsing_error, Msg} ->
	    io:format(standard_error, "~w.escript: ~s~n",[?MODULE, Msg]),
	    halt(127)
	    %% erlang:error({not_valid, Msg}, [Input])
    end.
	     
validate_length(Input) when length(Input) =< 7 ->	     
    validate_state(Input);
validate_length(_) ->	
    throw({parsing_error, "R-state is longer than 7 characters"}).


validate_state([State|Rest]) when State == $P; State == $R ->
    validate_number([State], Rest);
validate_state(_) ->
    throw({parsing_error, "R-state does not begin with 'P' or 'R'"}).

validate_number(_, [$0|_]) ->
    throw({parsing_error, "R-state number begins with 0"});
validate_number(State, Input) ->
    NumStr = lists:takewhile(fun is_number/1, Input),
    case NumStr of
	[] ->
	    throw({parsing_error, "R-state does not contain a number"});
	_ -> 
	    ok
    end,
    case list_to_integer(NumStr) of
	Num when Num > 999 -> 
	    %% Verification state R-states can only have a maximum of 3 digit
	    %% numbers
	    throw({parsing_error, "R-state number is larger than 999"});
	_ -> 
	    ok
    end,
    Rest = lists:dropwhile(fun is_number/1, Input),
    validate_letter(State, NumStr, Rest).

validate_letter(State, NumStr, Input) ->
    Letters = lists:takewhile(fun is_letter/1, Input),
    [case is_valid_letter(L) of
	 true ->
	     ok;
	 false ->
	     throw({parsing_error, "R-state contains the character "++[L]})
     end||L<-Letters],
    case length(Letters) of
	Length when Length > 2 -> 
	    throw({parsing_error, "R-state contains more than two letters"});
	0 -> 
	    throw({parsing_error, "R-state contains no letters"});
	_ ->
	    ok
    end,
    %% Validate verification number
    Rest = lists:dropwhile(fun is_letter/1, Input),
    case Rest of 
	[] -> 
	    throw({parsing_error, "R-state does not have a verification state"});
	_ when length(Rest) < 2 ->
	    throw({parsing_error, "Verification state must be at least 2 digits"});
	_ when length(Rest) > 3 ->
	    throw({parsing_error, "Verification state is more than 3 digits"});
	_ ->
	    case list_to_integer(Rest) of
		0 ->
		    throw({parsing_error, "Verification state 0 is not allowed"});
		RestInt when RestInt < 100 andalso length(Rest) > 2 ->
		    throw({parsing_error, "Verification state smaller than 100 must only be 2 characters"});
		_ ->
		    ok
	    end
    end,
		
    {State, NumStr, Letters, Rest}.


is_number(X) when X >= $0, X =< $9 -> true;
is_number(_) -> false.

is_letter(X) when X >= $A, X =< $Z -> true;
is_letter(_) -> false.

is_valid_letter(X) ->
    lists:member(X, ?VALID_LETTERS).
    
%%% ----------------------------------------------------------
%%% #           next_n(Split::parsed_rev()),
%%% Input: Split::parsed_rev()
%%% Output:
%%% Exceptions:
%%% Description: Calculates the next R-state with functional change
%%% ----------------------------------------------------------


next_n({State, Number, _Letter, _VNum}) ->    
    X = 
	State++integer_to_list(list_to_integer(Number)+1)++"A"++"01",
    try validate_rev(X) of
	_ -> X
    catch error:{not_valid,_} ->
	    "ERROR"
    end.

%%% ----------------------------------------------------------
%%% #           next_c(Split::parsed_rev()),
%%% Input: Split::parsed_rev()
%%% Output:
%%% Exceptions:
%%% Description: Calculates the next R-state for a realization change
%%% ----------------------------------------------------------

next_c({State, Number, Letters, _VNum}) ->
    Stepped = step_letter(Letters),
    X = State++Number++Stepped++"01",
    try validate_rev(X) of
	_ -> X
    catch error:{not_valid,_} ->
	    "ERROR"
    end.

step_letter(Letters) ->
    LettersRev = lists:reverse(Letters),
    SteppedRev = do_step_letters(LettersRev),
    lists:reverse(SteppedRev).

do_step_letters("Z") -> "AA";
%% This rule will cause validation error as verification state R-states does 
%% not allow for three letters, but we generate the number and let it be 
%% caught in validation
do_step_letters("ZZ") -> "AAA"; 
do_step_letters([$Z|T]) ->
    [$A|do_step_letters(T)];
do_step_letters([H|T]) ->
    Remaining = tl(lists:dropwhile(fun(L) when L == H -> false;
				      (_) -> true
				   end, ?VALID_LETTERS)),
    [hd(Remaining)|T].

%%% ----------------------------------------------------------
%%% #           next_v(Split::parsed_rev()),
%%% Input: Split::parsed_rev()
%%% Output:
%%% Exceptions:
%%% Description: Calculates the next R-state for the next verification state
%%% ----------------------------------------------------------

next_v({State, Numbers, Letters, VNum}) ->
    X = State++Numbers++Letters++format_vnumber(list_to_integer(VNum)+1),     
    try validate_rev(X) of
	_ -> X
    catch error:{not_valid, _} ->
	    "ERROR"
    end.

format_vnumber(X) when X<10 ->
    [$0, X+$0];
format_vnumber(X) ->
    integer_to_list(X).

%%% ----------------------------------------------------------
%%% #           next_v(Split::parsed_rev()),
%%% Input: Split::parsed_rev()
%%% Output:
%%% Exceptions:
%%% Description: Calculates the next R-state for the next verification state
%%%              but for Preliminary states
%%% ----------------------------------------------------------

next_p(Split) ->
    NextV = next_v(Split),
    try validate_rev(NextV) of
	{_, Numbers, Letters, VNum} ->
	    "P"++Numbers++Letters++VNum
    catch error:{not_valid, _} ->
	    "ERROR"
    end.
	    
    
%%% ----------------------------------------------------------




debug(Format, Args) ->
    case os:getenv("NEXTREV_DEBUG") of
	false ->
	    ok;
	"true" ->
	    io:format(Format, Args)
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

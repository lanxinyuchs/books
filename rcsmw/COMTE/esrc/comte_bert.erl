%%%-------------------------------------------------------------------
%%% Heavily inspired by Tom Preston-Werner's Erlang BERT
%%% implementation to be found at Github
%%%   http://github.com/mojombo/bert.erl
%%%
%%% @private
%%%-------------------------------------------------------------------
-module(comte_bert).
-export([encode/1,decode/1]).

%%%===================================================================
%%% API

-spec(encode(term()) ->
	     binary()).
%%
encode(Term) ->
    term_to_binary(wrap(Term)).

-spec(decode(binary()) ->
	     term()).
%%
decode(Binary) ->
    unwrap(binary_to_term(Binary)).


%%%===================================================================
%%% Implementation

wrap(Term) ->
    try dict:to_list(Term) of
	List ->
	    {bert,dict,List}
    catch
	error:{badrecord,dict} ->
	    case Term of
		true ->
		    {bert,true};
		false ->
		    {bert,false};
		[] ->
		    {bert,nil};
		{} ->
		    {};
		List when is_list(Term) ->
		    [wrap(Element) || Element <- List];
		Tuple when is_tuple(Term) ->
		    erlang:list_to_tuple(
		      wrap(erlang:tuple_to_list(Tuple)));
		Other ->
		    Other
	    end
    end.

unwrap(BertTerm) ->
    case BertTerm of
	{bert,dict,List} ->
	    dict:from_list(List);
	{bert,true} ->
	    true;
	{bert,false} ->
	    false;
	{bert,nil} ->
	    [];
	{} ->
	    {};
	List when is_list(BertTerm) ->
	    [unwrap(Element) || Element <- List];
	Tuple when is_tuple(BertTerm) ->
	    erlang:list_to_tuple(
	      unwrap(erlang:tuple_to_list(Tuple)));
	Other ->
	    Other
    end.

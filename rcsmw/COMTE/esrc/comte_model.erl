%%%-------------------------------------------------------------------
%%% @private
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @end
%%% Created : 21 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_model).

%% API
-export([ecim_to_comte_key/2]).
-export([comte_key_to_ecim/1]).

%%%===================================================================
%%% API
%%%===================================================================
ecim_to_comte_key(Dn, ClassName) when is_list(Dn) ->
    ecim_to_comte_key(list_to_binary(Dn),ClassName);
ecim_to_comte_key(Dn, ClassName) when is_list(ClassName) ->
    ecim_to_comte_key(Dn,list_to_binary(ClassName));
ecim_to_comte_key(Dn, <<"">>) ->
    split_dn(Dn, []);
ecim_to_comte_key(Dn, ClassName) ->  
    [ClassName | split_dn(Dn, [])].

comte_key_to_ecim(ComteKey) ->
    format_comte_key(lists:reverse(ComteKey)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
split_dn(<<>>,Acc) ->
    Acc;
split_dn(Dn,Acc) ->
    case binary:split(Dn,<<",">>,[trim]) of
	[Elem,Rest] ->
	    NewAcc = split_elem(Elem,Acc),
	    split_dn(Rest,NewAcc);
	[Elem] ->
	    split_elem(Elem,Acc)
    end.

split_elem(Elem,Acc) ->
    case binary:split(Elem,<<"=">>,[trim]) of
	[Name,Key] ->
	    [Key, hd(binary:split(Name,<<".">>,[trim])) | Acc];
	[Name] ->
	    [Name | Acc]
    end.	

format_comte_key([Class,Key|Rest]) ->
    [Class,"=",Key|format_comte_key(Rest)];
format_comte_key([Member]) ->
    [".",Member];
format_comte_key([]) ->
    [].


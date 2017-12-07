%%%-------------------------------------------------------------------
%%% @private
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @end
%%% Created : 21 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_registrar).

%% API
-export([init/0,
         register/2,
         get_callback/1]).

tab_cb() ->
    comte_registrar.

%%%===================================================================
%%% API
%%%===================================================================
%% Called by comte_bert_server
init() ->
    %% ETS opts
    Opts = [set, public, named_table],
    ok = comte_lib:tab_create([tab_cb()], Opts).


register(Key, Cb) when is_atom(Key) ->
    ok = comte_lib:tab_insert(tab_cb(), Key, Cb).

get_callback(Key) when is_atom(Key) ->
    case catch comte_lib:tab_lookup(tab_cb(), Key) of
        {error, Reason} ->
	    {error, Reason};
	{Key, Cb} ->
	    Cb;
        {'EXIT', Reason} ->
            {error, {bad_table, tab_cb(), Reason}}
    end.




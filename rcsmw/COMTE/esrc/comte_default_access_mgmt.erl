%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% Created : 21 Mar 2011 by Lukas Larsson
%%%-------------------------------------------------------------------
-module(comte_default_access_mgmt).
-behaviour(comte_access_mgmt_api).


%% API
-export([start/1,
         getRoles/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Will be called during startup of ComtE
%% @end
-spec start(Args::any()) -> ok.
start(_Args) ->
    ok.

%% @doc
%% Returns the roles for the user
%% @end
-spec getRoles(User::binary()) -> list(binary()).
getRoles(User) when is_binary(User) ->
    [<<"SecurityAdministrator">>].




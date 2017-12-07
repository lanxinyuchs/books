%%%-------------------------------------------------------------------
%%% @private
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @end
%%% Created : 17 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application.
%%--------------------------------------------------------------------
-spec start(StartType :: normal | {takeover,node()} | {failover, node()},
	    StartArgs :: term()) ->
		   {ok, Pid :: pid()} |
		   {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    case comte_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called before an application is stopped.
%%--------------------------------------------------------------------
-spec prep_stop(State :: term()) -> term().
prep_stop(State) ->
    case comte_com:stop_if_started() of
	ok ->
	    ok;
	{error,already_stopped} ->
	    ok
    end,
    State.
%%--------------------------------------------------------------------
%% @private
%% @doc This function is called whenever an application has stopped.
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.



%%%===================================================================
%%% Internal functions
%%%===================================================================

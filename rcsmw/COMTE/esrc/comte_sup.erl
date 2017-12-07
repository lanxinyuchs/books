%%%-------------------------------------------------------------------
%%% @private
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @end
%%% Created : 17 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Types
-type restart_strategy() :: one_for_one |
			    one_for_all |
			    rest_for_one |
			    simple_one_for_one.

-type restart() :: permanent | transient | temporary.

-type shutdown() :: brutal_kill | non_neg_integer() | infinity.

-type child_type() :: worker | supervisor.

-type modules() :: [module()].

-type mfargs() :: {M :: module(), F :: atom(), A :: [term()]}.

-type supflags() :: {RestartStrategy :: restart_strategy(),
		     MaxRestarts :: non_neg_integer(),
		     MaxSecondsBetweenRestarts :: pos_integer()}.

-type childspec() :: {Id :: atom(), StartOpts :: mfargs(),
		      Restart :: restart(), Shutdown :: shutdown(),
		      Type :: child_type(), Modules :: modules()}.

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}.
start_link() ->
    comte_error_logger:start_link(),
    comte_error_logger:add_handler(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Stop the supervisor
%%--------------------------------------------------------------------
stop() ->
    exit(whereis(?MODULE), shutdown).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Info about supervisor children and restart strategy.
%%--------------------------------------------------------------------
-spec init(Args :: []) ->
		  {ok, {SupFlags :: supflags(), [ChildSpec :: childspec()]}}.
init([]) ->

    RestartStrategy = one_for_all,
    {ok, MaxRestarts} = application:get_env(comte, max_restarts),
    {ok, MaxSecondsBetweenRestarts} = 
        application:get_env(comte, max_restarts_time_limit),
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    
    BertServer = {comte_bert_server, {comte_bert_server, start_link, []},
		  Restart, Shutdown, Type, [comte_bert_server]},

    ComteCom = {comte_com, {comte_com, start_link, []},
		Restart, 10000, Type, [comte_com]},

    {ok, {SupFlags, [BertServer, ComteCom]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

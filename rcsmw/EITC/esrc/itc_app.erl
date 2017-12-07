%%% -*-erlang-*-
%%%
%%% %EricssonCopyright%
%%% %CopyrightBegin%
%%%
%%% Copyright Ericsson AB 2013. All Rights Reserved.
%%%
%%% The program may be used and/or copied only with the written permission from
%%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%%% the agreement/contract under which the program has been supplied.
%%%
%%% %CopyrightEnd%
%%%
%%%
%%% @private
-module(itc_app).
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2,stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Application callbacks

-spec start(
	StartType :: normal | {takeover,node()} | {failover,node()},
	StartArgs :: term()) ->
			  {ok,pid()} | {error,Reason::term()}.
start(_, _) ->
    supervisor:start_link(?MODULE, []).

-spec stop(State::term()) -> ok.
stop(_) ->
    try itc:exit() of
	ok ->
	    ok;
	Result ->
	    error_logger:warning_msg("itc:exit() -> ~p.~n", [Result])
    catch
	error:badarg ->
	    error_logger:warning_msg(
	      "itc:exit() badarg during application stop at~n  ~p",
	      erlang:get_stacktrace())
    end,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Supervisor callbacks

%%%% Types
%%-type restart_strategy() :: one_for_one |
%%                            one_for_all |
%%                            rest_for_one |
%%                            simple_one_for_one.
%%
%%-type restart() :: permanent | transient | temporary.
%%
%%-type shutdown() :: brutal_kill | non_neg_integer() | infinity.
%%
%%-type child_type() :: worker | supervisor.
%%
%%-type modules() :: [module()].
%%
%%-type mfargs() :: {M :: module(), F :: atom(), A :: [term()]}.
%%
%%-type supflags() :: {RestartStrategy :: restart_strategy(),
%%                     MaxRestarts :: non_neg_integer(),
%%                     MaxSecondsBetweenRestarts :: non_neg_integer()}.
%%
%%-type childspec() :: {Id :: atom(), StartOpts :: mfargs(),
%%                      Restart :: restart(), Shutdown :: shutdown(),
%%                      Type :: child_type(), Modules :: modules()}.
%%
%%-spec init(Args::[]) ->
%%		  {ok,{SupFlags::supflags(),[ChildSpec::childspec()]}}.
init([]) ->
    Opts = app_opts([driver_path,mailbox_count,alloc_scheme,namespace]),

    try itc:exit() of
	ok ->
	    %% Clean exit means ITC was already initialized
	    error_logger:warning_msg(
	      "itc:exit() ok during application start~n"),
	    ok;
	already_exited ->
	    %% Happens at all but first init
	    ok;
	already_exiting ->
	    error_logger:warning_msg(
	      "itc:exit() already_exiting during application start~n"),
	    ok
    catch
	error:badarg ->
	    %% Driver not loaded, happens at first init
	    ok
    end,
    case itc:init(Opts) of
	ok ->
	    ok;
	Result ->
	    error_logger:info_msg("itc:init(~p) -> ~p.~n", [Opts,Result])
    end,
    SupFlags = {one_for_one,5,10},
    {ok,{SupFlags,[]}}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal

app_opts([]) ->
    [];
app_opts([Opt|Opts]) ->
    case application:get_env(Opt) of
	{ok,Val} ->
	    [{Opt,Val}|app_opts(Opts)];
	undefined ->
	    app_opts(Opts)
    end.

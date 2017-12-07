%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaSyncServer.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R4A/1

%%% @doc ==== Handling sync of latest log in mnesia ====
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(comsaSyncServer).
-behaviour(gen_server).
-vsn('/main/R2A/R4A/1').
-date('2015-10-14').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      2014-04-11 etxtory     Created
%%% R4A/1      2015-10-14 etxpeno     Using the time functionality in OTP 18
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).
-export([sync/0]).

%% Debug interface
-export([debug/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-record(state, {debug = false,
		active = true
               }).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the aicServer process
%%% @end
%%% ----------------------------------------------------------
start_link() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Called after each COM-transaction
%%% This function is called by comsaTransactionServer
%%% after a successful COM-transaction. It will sync mnesia's
%%% latest log so the node will lose as little configuration
%%% data as possible at power outtage.
%%% NOTE: must be a cast, the sync might take long time
%%% @end
%%% ----------------------------------------------------------
sync() ->
    gen_server:cast(?MODULE, sync).

%%% ----------------------------------------------------------
%%% @doc Debug function
%%% Debug = true | false (default false)
%%% Active = true | false (default false)
%%% @end
%%% ----------------------------------------------------------
debug(Debug, Active) ->
    gen_server:cast(?MODULE, {debug, Debug, Active}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

handle_cast(sync, State) ->
    case State#state.active of
	true ->
	    %% Flush any sync-messages in the message queue.
	    flush_sync_msg(),
	    BeforeNow = erlang:monotonic_time(),
	    do_mnesia_sync(),
	    AfterNow = erlang:monotonic_time(),
	    do_debug(State#state.debug, BeforeNow, AfterNow);
	false ->
	    ok
    end,
    {noreply, State};

handle_cast({debug, Debug, Active}, State) ->
    {noreply, State#state{debug = Debug, active = Active}}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Flushing sync messages
%%% Too avoid unnecessary mnesia syncing
%%% ----------------------------------------------------------
flush_sync_msg() ->
    receive
	{'$gen_cast', sync} ->
	    flush_sync_msg()
    after 0 ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% Handles debug
%%% ----------------------------------------------------------
do_debug(false, _ , _ ) ->
    ok;
do_debug(true, BeforeNow, AfterNow) ->
    Diff = diff(BeforeNow, AfterNow),
    io:format("~p: mnesia:sync_log() took ~p seconds~n", [?MODULE, Diff]).

diff(BeforeNow, AfterNow) ->
    Diff = AfterNow-BeforeNow,
    Micro = erlang:convert_time_unit(Diff, native, micro_seconds),
    Micro/1000000.

%%% ----------------------------------------------------------
%%% Calls mnesia to sync mnesia's latest log
%%% ----------------------------------------------------------
do_mnesia_sync() ->
    case erlang:function_exported(mnesia, sync_log, 0) of
	true ->
	    mnesia:sync_log();
	false ->
	    ok
    end.

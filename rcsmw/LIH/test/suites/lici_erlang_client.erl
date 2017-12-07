%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lici_erlang_client.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014
%%% @doc Server running on the SUT used63 by lici_erlang_SUITE.erl
%%%
%%% @end
%%%-------------------------------------------------------------------

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
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

-module(lici_erlang_client).

-behaviour(gen_server).

%% API
-export([start/0, stop/1]).

-export([initiate_service/2,
         terminate_service/2]).

-export([feature_subscribe/3,
         feature_unsubscribe/3,
         capacity_subscribe/3,
         status_subscribe/2,
         is_LKF_installed/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    {ok, Client} = gen_server:start(?MODULE, [], []),
    %% io:format("ClientPid: ~p",[Client]),
    Client.

stop(Client) ->    
    Answer = gen_server:cast(Client, stop),
    %% io:format("### A: ~p",[Answer]),
    Answer.

initiate_service(Client, WantedPvList) ->
    gen_server:call(Client, {apply, initiate_service, [WantedPvList]}).
terminate_service(Client, LiciId) ->
    gen_server:call(Client, {apply, terminate_service, [LiciId]}).

feature_subscribe(Client, LiciId, FeatureId)  ->
    gen_server:call(Client, {apply, feature_subscribe, [LiciId, FeatureId]}).

feature_unsubscribe(Client, LiciId, FeatureId) ->
    gen_server:call(Client, {apply, feature_unsubscribe, [LiciId, FeatureId]}).

capacity_subscribe(Client, LiciId, CapacityId) ->
    gen_server:call(Client, {apply, capacity_subscribe, [LiciId, CapacityId]}).

status_subscribe(Client, LiciId) ->
    gen_server:call(Client, {apply, status_subscribe, [LiciId]}).

is_LKF_installed(Client, LiciId) ->
    gen_server:call(Client, {apply, is_LKF_installed, [LiciId]}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({apply, Func, Arg}, _From, State) ->
    Reply = apply(lih_lici, Func, Arg),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    Reason = normal,
    {stop, Reason, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

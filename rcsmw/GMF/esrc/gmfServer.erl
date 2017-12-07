%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfServer.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R5A/4

-module(gmfServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R5A/4').
-date('2016-03-04').
-author('etxpeno').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R1A/1      2012-04-10 etxbjca     Created
%%% R2A/6      2013-05-29 etxarnu     Changed cast to call in stop_com
%%% R2A/10     2014-03-18 erarafo     Added event for restart log
%%% R2A/12     2014-08-13 etxtory     Removed called to sysNetloader
%%% R3A/4      2014-11-19 etxarnu     Reverted to call:start_com for now
%%% R3A/5      2014-11-20 etxarnu     New try with cast:start_com
%%% R3A/6      2015-01-15 etxpeno     Support for regular role
%%% R3A/7      2015-03-09 erarafo     Startup timing
%%% R5A/1      2015-11-17 etxpeno     remove dead code
%%% R5A/2      2016-02-01 etxarnu     Added some delay before start of COM
%%%                                   after POWERON restart to allow
%%%                                   eriAlarmListRebuilt to be sent
%%% R5A/3      2016-02-19 etxarnu     Removed delay before start of COM
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).
-export([activate/0]).
-export([start_com/1]).
-export([stop_com/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).
-compile(nowarn_unused_vars).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start_link() ->
    start().
start() ->
    % it is assumed that gmfServer is the first among the GMF children
    sysInitI:log_startup_time(gmf_starting),

    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).
stop() ->
    gen_server:call(?MODULE, stop).

activate() ->
    gen_server:cast(?MODULE, activate).
%%    gen_server:call(?MODULE, activate, 60000).

start_com(Opts) ->
    gen_server:cast(?MODULE, {start_com, Opts}).

stop_com() ->
    gen_server:call(?MODULE, stop_com, 60000).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init(_) ->
    ets:new(gmfData, [ordered_set, public, named_table]),
    ets:new(gmfAdminOpPid, [ordered_set, public, named_table]),
    sysInitI:log_startup_time(gmfServer_started),
    {ok, state}.

handle_call(stop, _From, State) ->
    {stop,normal,ok,State};
handle_call(stop_com,  _From, State) ->
    comsaI:stop_com(),
    {reply, ok, State};

%% handle_call(activate, _From,  State) ->
%%     comsaI:start_com([{start_com, true}]),
%%     sysInitI:restart_logger_trace(?MODULE, ?LINE, "requesting COM to start"),
%%     {reply, ok,  State};

handle_call(Other, _, State) ->
    io:format("gmfServer received unknown message ~p~n", [Other]),
    {reply, ok, State}.

handle_cast({start_com, Opts}, State) ->
    comsaI:start_com(Opts),
    {noreply, State};
handle_cast(activate, State) ->
    comsaI:start_com([{start_com, true}]),
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "requesting COM to start"),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

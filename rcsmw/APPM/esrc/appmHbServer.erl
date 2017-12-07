%%%-------------------------------------------------------------------
%%% @author etxarnu
%%% @copyright Ericsson AB 2014-2017
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(appmHbServer).
-behaviour(gen_server).
-vsn('/main/R2A/R4A/R8A/R9A/1').
-date('2017-01-26').
-author('etxarnu').

%%% ----------------------------------------------------------
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% Rev     Date       Name        What
%%% -----   -------    --------    ------------------------
%%% R2A/1   2014-04-11 etxpejn    Created
%%% R8A/1   2016-12-21 etxarnu Change format of input tuple for start_clinet_pid 
%%% ----------------------------------------------------------

%% API
-export([start_link/0,
	 get_state/0,
	 program_started/1,
	 stop_hb_client/1,
	 stop_hb_clients/1]).

% used from LMHI
-export([heartbeat_signal/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("appm.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% -spec heartbeat_signal() -> response().
heartbeat_signal({PgmName, LmId}) ->
    gen_server:cast(?SERVER, {heartbeat_signal, {PgmName, LmId}}).

get_state() ->
     gen_server:call(?MODULE, get_state).

program_started(NameCxc) ->
    gen_server:cast(?SERVER, {program_started, NameCxc}).

stop_hb_client({PgmName, LmId}) ->
    gen_server:cast(?SERVER, {stop_hb_client, {PgmName,LmId}}).

stop_hb_clients([]) ->
    ok;
stop_hb_clients([{_PghId, NameCxc, _Pid} | RestList]) ->
    gen_server:cast(?SERVER, {stop_hb_client, NameCxc}),
    stop_hb_clients(RestList).


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
    process_flag(trap_exit, true),
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
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(Request, _From, State) ->
    sysInitI:info_msg("~p:~n"
			  "Received a handle_call(~p) in State= ~p~n",
			  [?MODULE, Request, State]),
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
handle_cast({heartbeat_signal, {PgmName, LmId}}, State) ->
    NameCxc = {b2l(PgmName), b2l(LmId)},
    appmHbClient:heartbeat_signal(NameCxc),
    {noreply, State};
handle_cast({program_started, NameCxc}, State) ->
    StartDataList = appmAppData:get_lm_start_data(NameCxc),
    ok = start_client_pid(NameCxc, StartDataList),
    {noreply, State};
handle_cast({stop_hb_client, NameCxc}, State) ->
    appmHbClient:stop_hb_client(NameCxc),
    {noreply, State};
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
    %% io:format("appmHbServer:handle_info, Info: ~p, State: ~p~n", [Info, State]),
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
start_client_pid(NameCxc, {ok, _CxpPath, _StartCmd, AppInfo,_}) ->
    case {get_hb_interval(AppInfo), get_no_of_hb(AppInfo)} of
	{undefined, undefined} ->
	    %% No heartbeat request inserted by the application
	    appmLib:log(info, ?MODULE_STRING ++ ":" ++ "Heartbeat not started for ~p~n", 
			[NameCxc]);
	{undefined, _NoOfHb} ->
	    appmLib:log(info, ?MODULE_STRING ++ ":" ++ "Heartbeat can not be started since "
			"heartbeatInterval is undefined for ~p~n", [NameCxc]);
	{_HbInterval, undefined} ->
	    appmLib:log(info, ?MODULE_STRING ++ ":" ++ "Heartbeat can not be started since "
			"cardiacArrest_noOfHb is undefined for ~p~n", [NameCxc]);
	{HbInterval, NoOfHb} ->
	    case appmHbClient:start_link({NameCxc, HbInterval, NoOfHb}) of
		{ok, _Pid} ->
		    ok;
		{error, Error} ->
		    appmLib:log(info, ?MODULE_STRING ++ ":" ++ "Not possible to start heartbeat "
				" supervsion for ~p, reason ~p~n", [NameCxc, Error])
	    end
    end,
    ok.

get_hb_interval(undefined) ->
    undefined;
get_hb_interval(#appmPgmData{heartbeatInterval = undefined}) ->
    undefined;
get_hb_interval(#appmPgmData{heartbeatInterval = PollTime}) ->
    PollTime.

get_no_of_hb(undefined) ->
    undefined;
get_no_of_hb(#appmPgmData{cardiacArrest_noOfHb = undefined}) ->
    undefined;
get_no_of_hb(#appmPgmData{cardiacArrest_noOfHb = NoOfRep}) ->
    NoOfRep.


b2l(List) when is_list(List) ->
    List;
b2l(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).



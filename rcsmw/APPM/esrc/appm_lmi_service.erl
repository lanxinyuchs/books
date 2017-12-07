%%%-------------------------------------------------------------------
%%% @author erarafo
%%% @copyright Ericsson AB 2012-2016
%%% @doc
%%%
%%% @end
%%% Created :  8 Aug 2012 by Arto Nummelin
%%%-------------------------------------------------------------------
-module(appm_lmi_service).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R4A/R8A/1').
-date('2016-12-15').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseFile:	appm_lmi_service.erl %
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
%%% R1A/1     20120808    etxarnu     Created
%%% R1A/3     20140417    etxberb     Added possibility to omit return signal.
%%% R1A/4     20140809    uabesvi     Added error report at cec_setup
%%%                                   and also increased gen_server timeout
%%% R4A/1     20150521    etxarnu     changed now to os:timestamp
%%% R4A/4     201510207   etxarnu     Added cec_takeover
%%% R4A/5     20151207    etxpeno     Don't call the server at cec_setup
%%% R8A/1     20161215    erarafo     Fix compiler warning


-behaviour(gen_server).

%% API

-export([start_link/0]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([cec_setup/1, cec_takeover/1]).


-export([
	dbg_print/2  % to supress compiler warnings
    ]).

-ifdef(debug).
-define(dbg(What, Term), dbg_print(What, Term) ).
-else.
-define(dbg(_What,_Term), true).
-endif.

dbg_print(What, Term) ->
    {_,_,MS} = Now = os:timestamp(),
    Time = {calendar:now_to_local_time(Now),MS},
    io:format("~p: ~p (~p): ~p ~p~n", [Time,?MODULE, self(), What, Term]).

-define(SERVER, ?MODULE).

-record(state, {
	data
    }).

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
    start_link([]).
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).


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

init(_Opts) ->
    erlang:process_flag(trap_exit, true),
    State = #state{},
    {ok, State}.

cec_setup(_Socket) ->
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    Pid;
	undefined ->
	    throw(no_appm_lmi_service)
    end.

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

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

handle_cast({cec_takeover, Socket}, S) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, S};

handle_cast(stop,  S) ->
    {stop, normal, S};

handle_cast(_Msg, S) ->
    ?dbg(cast, _Msg),
    {noreply, S}.

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


handle_info({tcp, Socket, Bytes},  State) ->
    ?dbg(got_message, Bytes),
    case appm_lmi:message(Bytes) of
	undefined ->
	    ok;
	Res ->
	    %% _Size = size(Res),
	    %% ?dbg(result, {_Size, Res}),
	    ok = gen_tcp:send(Socket, Res)
    end,
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, State};

handle_info(_Info, State) ->
    ?dbg(info, {_Info,State}),
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
    ?dbg(terminates, _Reason),
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

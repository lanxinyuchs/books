%%%-------------------------------------------------------------------
%%% @author etxarnu
%%% @copyright Ericsson AB 2013-2017
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(appmPghServer).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R9A/R10A/R11A/R12A/1').
-date('2017-11-09').
-author('etxarnu').

%%% ----------------------------------------------------------
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2015-01-13 etxarnu     Changed to spawnpgmex to handle ns
%%% R3A/2      2015-01-14 etxarnu     only use spawnpgmex for arm
%%% R3A/3      2015-01-21 etxarnu     Now return {error,Reason} from send_to_pghd
%%%                                   instead of only error
%%% R3A/4      2015-01-21 etxpeno     Updated specs to silence dialyzer
%%% R3A/5      2015-01-22 etxarnu     Updated specs to silence dialyzer
%%% R5A/1      2016-03-09 etxarnu     Added req_get_pids/1
%%% R5A/2      2016-03-09 etxarnu     Dialyzer fix
%%% R6A/1      2016-06-02 etxarnu     Use spawnpgmex also for vrcs
%%% R9A/1      2017-03-23 etxarnu     Added req_write_llog
%%% R10A/1     2017-05-10 etxarnu     Handle tcp_closed in VRCS
%%% R10A/2     2017-05-17 etxarnu     Added Reason to pgmterm
%%% R10A/3     2017-05-31 etxarnu     Changed pgmterm w. Reason to pgmtermex
%%% R12A/1     2017-11-09 etxarnu     Removed all group related functions
%%% ----------------------------------------------------------

%% API
-export([start_link/0]).

-export([set_callback/1,
	 req_init/1,
	 req_set_def_cpuset/1,
	 req_spawn_pgm/1,
	 req_signal_to_pgm/1,
	 req_destroy_pgm/1,
	 req_get_pgm_rscusage/1,
	 req_restart_brd/1,
	 req_warm_restart/1,
	 req_write_llog/1,
	 req_get_pids/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("appmPgh.hrl").

-define(SERVER, ?MODULE).

-record(state, {seq      = 1,
		socket,
		pending  = [],
		callback}).

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

set_callback(Callback) ->
    call({callback, Callback}).

-spec req_init(EncodeList :: [init_option()]) -> response().
req_init(EncodeList) ->
    call({req_init, EncodeList}).

-spec req_set_def_cpuset(EncodeList :: [set_def_cpuset_option()]) -> response().
req_set_def_cpuset(EncodeList) ->
    call({req_set_def_cpuset, EncodeList}).

-spec req_spawn_pgm(EncodeList :: [spawn_pgmex_option()]) -> response().
req_spawn_pgm(EncodeList) ->
    call({req_spawn_pgm, EncodeList}).

-spec req_destroy_pgm(EncodeList :: [destroy_pgm_option()]) -> response().
req_destroy_pgm(EncodeList) ->
    call({req_destroy_pgm, EncodeList}).

-spec req_signal_to_pgm(EncodeList :: [signal_to_pgm_option()]) -> response().
req_signal_to_pgm(EncodeList) ->
    call({req_signal_to_pgm, EncodeList}).

-spec req_get_pgm_rscusage(EncodeList :: [get_pgm_rscusage_option()]) ->
				  response().
req_get_pgm_rscusage(EncodeList) ->
    call({req_get_pgm_rscusage, EncodeList}).

-spec req_restart_brd(EncodeList :: [restart_brd_option()]) -> ok.
req_restart_brd(EncodeList) ->
    cast({req_restart_brd, EncodeList}).

-spec req_warm_restart(EncodeList :: [warm_restart_option()]) -> ok.
req_warm_restart(EncodeList) ->
    cast({req_warm_restart, EncodeList}).

-spec req_get_pids(EncodeList :: [get_pids_option()]) ->  response().
req_get_pids(EncodeList) ->
    call({req_get_pids, EncodeList}).

-spec req_write_llog(EncodeList :: [write_llog_option()]) ->  response().
req_write_llog(EncodeList) ->
    call({req_write_llog, EncodeList}).


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

    Address = "127.0.0.1",
    PghdPort = sysEnv:get_port_conf(pghd),
    Options = [binary, {packet, raw}, {active, once}, {nodelay, true}],
    case gen_tcp:connect(Address, PghdPort, Options) of
	{ok, Socket} ->
	    Pid = list_to_integer(os:getpid()),
	    ok = gen_tcp:send(Socket,
			      <<Pid:4/big-unsigned-integer-unit:8>>),
	    ok = inet:setopts(Socket, [{packet, 4}]),
	    Callback = appmServer,
	    {ok, #state{socket   = Socket,
			callback = Callback}};
	{error, Reason} ->
	    sysInitI:info_msg("~p:~n"
				  "Not possible to connect to pghd~n"
				  "TCP port: ~p~n"
				  "Reason: ~s~n",
				  [?MODULE, PghdPort,
				   inet:format_error(Reason)]),
	    {ok, #state{}}
    end.

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
handle_call({callback, Callback}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{callback = Callback}};
handle_call(_Request, _From, #state{socket = undefined} = State) ->
    Reply = {error, pgh_not_started},
    {reply, Reply, State};
handle_call({req_init, EncodeList}, From, State) ->
    MsgType = init,
    send_to_pghd(State, From, {MsgType, EncodeList});
handle_call({req_set_def_cpuset, EncodeList}, From, State) ->
    MsgType = setdefcpuset,
    send_to_pghd(State, From, {MsgType, EncodeList});
handle_call({req_spawn_pgm, EncodeList}, From, State) ->
    MsgType = case sysEnv:architecture() of
		  {"arm",_ }  -> spawnpgmex;
		  _           -> case sysEnv:vrcs() of
				     true -> spawnpgmex;
				     false -> spawnpgm
				 end
	      end,
    send_to_pghd(State, From, {MsgType, EncodeList});
handle_call({req_destroy_pgm, EncodeList}, From, State) ->
    MsgType = destroypgm,
    send_to_pghd(State, From, {MsgType, EncodeList});
handle_call({req_signal_to_pgm, EncodeList}, From, State) ->
    MsgType = signalpgm,
    send_to_pghd(State, From, {MsgType, EncodeList});
handle_call({req_get_pgm_rscusage, EncodeList}, From, State) ->
    MsgType = getpgmrscusage,
    send_to_pghd(State, From, {MsgType, EncodeList});
handle_call({req_get_pids, EncodeList}, From, State) ->
    MsgType = getpgmpid ,
    send_to_pghd(State, From, {MsgType, EncodeList});
handle_call({req_write_llog, EncodeList}, From, State) ->
    MsgType = writellog ,
    send_to_pghd(State, From, {MsgType, EncodeList});
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
handle_cast({req_restart_brd, EncodeList}, State) ->
    MsgType = restartbrd,
    send_to_pghd(State, cast, {MsgType, EncodeList}),
    {noreply, State};
handle_cast({req_warm_restart, EncodeList}, State) ->
    MsgType = warmrestart,
    send_to_pghd(State, cast, {MsgType, EncodeList});
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
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    NewState = handle_rsp(State, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    case sysEnv:vrcs() of
	true ->
	        {stop, {shutdown, tcp_closed}, State};
	false ->
	    {stop, tcp_closed, State}
    end;
handle_info({tcp_error, Socket, _Reason}, #state{socket = Socket} = State) ->
    {stop, tcp_error, State};
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
-define(CALL_TIMEOUT,60000). %long time to handle simulated env
call(Msg) ->
    gen_server:call(?SERVER, Msg, ?CALL_TIMEOUT).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).

send_to_pghd(State, From, {MsgType, EncodeList}) ->
    Seq = State#state.seq,
    Data = appmPgh:encode_req(MsgType, Seq, EncodeList),
    %% appmLib:log(info,"appmPghServer:send_to_pghd ~n"
    %% 		"EncodeList =~p~n"
    %% 		"Data=~p~n",[EncodeList,Data]),
    case gen_tcp:send(State#state.socket, Data) of
	ok ->
	    Pending = State#state.pending,
	    NewPending = [{Seq, From} | Pending],
	    NewState = State#state{seq = Seq+1,
				   pending = NewPending},
	    {noreply, NewState};
	{error, Reason} ->
	    {reply, {error,Reason}, State}
    end.

handle_rsp(State, Data) ->
    Pending = State#state.pending,
    try
	Resp = appmPgh:decode_rsp(Data),
	{value, {msg_type, MsgType}, Resp1} = lists:keytake(msg_type, 1, Resp),
	{value, {seq, Seq}, Resp2} = lists:keytake(seq, 1, Resp1),
	case lists:keytake(Seq, 1, Pending) of
	    false when Seq =:= 0 andalso
		       (MsgType =:= pgmcrash orelse
			MsgType =:= pgmtermex orelse
			MsgType =:= pgmterm) ->
		case State#state.callback of
		    undefined ->
			ok;
		    Callback ->
			{pgm_id, PgmId} = lists:keyfind(pgm_id, 1, Resp2),
			Arg = case MsgType of
				  pgmcrash ->
				      case lists:keyfind(rank, 1, Resp2) of
					  {rank, Rnk} -> Rnk;
					  false -> no_rank
				      end;
				  pgmtermex ->
				      case lists:keyfind(term_reason, 1, Resp2) of
					  {term_reason, Reason} -> Reason;
					  false -> no_reason
				      end;
				  pgmterm ->
				       no_reason
			      end,
			(catch Callback:MsgType(PgmId,Arg))
		end,
		State;
	    false when Seq =:= 0 andalso  (MsgType =:= warmrestart) ->
		case State#state.callback of
		    undefined ->
			ok;
		    Callback ->
			{result, Result} = lists:keyfind(result, 1, Resp2),
			(catch Callback:MsgType(Result))
		end,
		State;
	    false ->
		State;
	    {value, {Seq, From}, NewPending} ->
		Reply =
		    case lists:keytake(result, 1, Resp2) of
			{value, {result, success}, Resp3} ->
			    {ok, Resp3};
			{value, {result, Result}, _} ->
			    {error, Result}
		    end,
		if
		    From =:= cast ->
			ok;
		    true ->
			gen_server:reply(From, Reply)
		end,
		State#state{pending = NewPending}
	end
    catch
	_:_ ->
	    ST = erlang:get_stacktrace(),
	    appmLib:log(warning,
			"~p: Got faulty data from pghd: ~p~n stacktrace: ~p ~n",
			[?MODULE, Data, ST]),
	    State
    end.

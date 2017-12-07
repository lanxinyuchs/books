%%%-------------------------------------------------------------------
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2015
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(appmHbClient).
-behaviour(gen_server).

%%% ----------------------------------------------------------
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
%%% R2A/1      2014-04-11   etxpejn    Created
%%% R2A/3      2014-04-17   etxberb    Continued
%%% R2A/4      2014-04-23   etxberb    Completed pgm restart.
%%% R2A/5      2014-04-29   etxberb    Added DEFAULT_heartbeat_quarantine_time.
%%% ----------------------------------------------------------

%% API
-export([start_link/1,
	 get_clients/0,
	 get_states/0,
	 get_state/1,
	 stop_hb_client/1]).

% used from LMHI
-export([heartbeat_signal/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(HB_HEADING, {?MODULE, 'Heartbeat supervisor'}).
-define(DEFAULT_heartbeat_quarantine_time, 600).   % 600 seconds - 10 minutes

-record(state, {name,
		hbInterval,
		timeRef,
		cardiacArr_noOfHb,
		noOfTimeouts}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(tuplet()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link({NameCxc, _, _} = Arg) ->
    CardiacSup = encode_cardiac_supervisor(NameCxc),
    gen_server:start_link({local, CardiacSup}, ?MODULE, Arg, []).

%%--------------------------------------------------------------------
%% -spec heartbeat_signal() -> response().
heartbeat_signal(NameCxc) ->
    CardiacSup = encode_cardiac_supervisor(NameCxc),
    case whereis(CardiacSup) of
	Pid when is_pid(Pid) ->
	    gen_server:cast(CardiacSup, heartbeat_signal);
	_ ->
	    sysInitI:error_report([?HB_HEADING,
				       {'Got heartbeat_signal from', NameCxc},
				       'Heartbeat not specified in XML'])
    end.

%%--------------------------------------------------------------------
get_clients() ->
    PotentialClients = [encode_cardiac_supervisor(NameCxc) ||
			   {NameCxc, _} <- appmServer:get_info(pgms)],
    [Client || Client <- PotentialClients,
	       is_pid(whereis(Client))].

%%--------------------------------------------------------------------
get_states() ->
    [get_state(Client) || Client <- get_clients()].

%%--------------------------------------------------------------------
get_state({Name, Id} = NameCxc) when is_list(Name) andalso is_list(Id) ->
    get_state(encode_cardiac_supervisor(NameCxc));
get_state(CardiacSup) when is_atom(CardiacSup) ->
    case whereis(CardiacSup) of
	Pid when is_pid(Pid) ->
	    report_format(gen_server:call(CardiacSup, get_state));
	_ ->
	    undefined
    end.

%%--------------------------------------------------------------------
stop_hb_client(NameCxc) ->
    CardiacSup = encode_cardiac_supervisor(NameCxc),
    case whereis(CardiacSup) of
	Pid when is_pid(Pid) ->
	    sysInitI:info_report([?HB_HEADING,
				      {stop_hb_client, NameCxc}]),
	    gen_server:cast(CardiacSup, stop_hb_client);
	_ ->
	    ok
    end.


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
init({NameCxc, HbInterval, NoOfHb}) ->
    process_flag(trap_exit, true),
    %% Start timers
    %% HbInterval should be in sec in the XML
    %% In init, wait extra long for first heartbeat
    Time = ?DEFAULT_heartbeat_quarantine_time * 1000,   % In milliseconds.
    TimerRef = erlang:start_timer(Time, self(), check_heartbeat),
    State = #state{name = NameCxc,
		   hbInterval = HbInterval,
		   timeRef = TimerRef,
		   cardiacArr_noOfHb = NoOfHb,
		   noOfTimeouts = 0},
    sysInitI:info_report([?HB_HEADING | report_format(State)]),
    {ok, State}.

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
    sysInitI:warning_report([{?MODULE, unrecognized_call},
				 {msg, Request} |
				 report_format(State)]),
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
handle_cast(heartbeat_signal, #state{hbInterval = HbInterval,
				     timeRef = TimerRef} = State) ->
    erlang:cancel_timer(TimerRef),
    Time = (HbInterval + (HbInterval div 2)) * 1000,   % In milliseconds.
    NewTimerRef = erlang:start_timer(Time, self(), check_heartbeat),
    {noreply, State#state{timeRef = NewTimerRef, noOfTimeouts = 0}};
handle_cast(stop_hb_client, #state{timeRef = TimerRef} = State) ->
    erlang:cancel_timer(TimerRef),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    sysInitI:warning_report([{?MODULE, unrecognized_cast},
				 {msg, _Msg} |
				 report_format(State)]),
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
handle_info({timeout, _Ref, check_heartbeat},
	    #state{name = NameCxc,
		   hbInterval = HbInterval, 
		   cardiacArr_noOfHb = CardiacArrest_NoOfHb,
		   noOfTimeouts = NoOfTimeouts} = State) ->
    %% No heartbeat from the application
    case (NoOfTimeouts + 1) of
	MissingHeartbeats when MissingHeartbeats < CardiacArrest_NoOfHb ->
	    Time = HbInterval * 1000,   % In milliseconds.
	    NewTimerRef = erlang:start_timer(Time, self(), check_heartbeat),
	    NewState = State#state{noOfTimeouts = MissingHeartbeats, 
				   timeRef = NewTimerRef},
	    WarningInfo = [?HB_HEADING,
			   {timeout, 'expected heartbeat missing'} |
			   report_format(NewState)],
	    sysInitI:warning_report(WarningInfo),
	    {noreply, NewState};
	MissingHeartbeats ->
	    NewState = State#state{noOfTimeouts = MissingHeartbeats, 
				   timeRef = undefined}, 
	    Info = [?HB_HEADING,
		    {timeout, 'Cardiac arrest detected'} |
		    report_format(NewState)],
	    sysInitI:info_report(Info),
	    appmServer:pgmhanging(NameCxc),
	    {stop, normal, NewState}
    end;
handle_info(_Info, State) ->
    sysInitI:warning_report([{?MODULE, unrecognized_info},
				 {msg, _Info} |
				 report_format(State)]),
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
encode_cardiac_supervisor({Name, Id}) ->
    list_to_atom("appmHbClient_" ++ Name ++ "_" ++ Id).

%%%===================================================================
report_format(Rec) when is_tuple(Rec) ->
    try
	Len = length([RecordName | Tail] = tuple_to_list(Rec)),
	[list_to_atom("#" ++ atom_to_list(RecordName) ++ "{}:") |
	 report_format(get_record_info(RecordName, Len), Tail)]
    catch
	Class : Reason ->
	    [{Class, Reason}]
    end;
report_format(Other) ->
    [Other].

report_format([F | TailF], [V | TailV]) ->
    [{F, V} | report_format(TailF, TailV)];
report_format([], []) ->
    [].

get_record_info(state, _) ->
    record_info(fields, state);
get_record_info(_, Len) ->
    default_record_info(Len).

default_record_info(Len) ->
    default_record_info(Len - 1, 1).

default_record_info(Len, Pos) when Len > 0 ->
    [list_to_atom("e" ++ integer_to_list(Pos)) |
     default_record_info(Len - 1, Pos + 1)];
default_record_info(0, _) ->
    [].

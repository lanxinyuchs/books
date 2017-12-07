%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	eqs_buti_service.erl %
%%% Author:	etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(eqs_buti_service).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R8A/R10A/1').
-date('2017-06-14').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2014-02-13 etxpeno     Created
%%% R2A/8      2014-05-06 etxberb     Added options / actions in
%%%                                   sim_button_event/1.
%%% R4A/2      2015-10-08 etxpeno     Added cec_takeover
%%% R5A/1      2016-03-17 etxpeno     add more logging
%%% R10A/1     2017-06-14 etxpeno     Correct usage of cec:get_program_name/1
%%% ----------------------------------------------------------

%% API
-export([start/0,
         start/1,
         start_link/0,
         start_link/1,
         stop/0]).

-export([activate/0]).

-export([cec_setup/1, cec_takeover/1]).

%% Function used by COLI
-export([sim_button_event/1]).

-export([set_button_event/1,
	 set_feedback_blink_mode/1]).
-export([get_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("eqs_vii.hrl").

-define(SERVER, ?MODULE).

-define(EVENT_IND, 1).

-define(INIT_SERVICE,    0).
-define(TERM_SERVICE,    1).
-define(SUBSCRIBE,       2).
-define(UNSUBSCRIBE,     3).
-define(CHANGE_FEEDBACK, 4).

-define(RESET_RESTART_TIMER, 180000). % 3 minutes

-define(RHAI_MMI_FLAG_BUTTON_PRESSED,  16#1).
-define(RHAI_MMI_FLAG_BUTTON_2SECONDS, 16#2).
-define(RHAI_MMI_FLAG_BUTTON_RELEASED, 16#4).

-define(CELLO_BUTI_INITIATE_SERVICE_CFM,         16#60D36).
-define(CELLO_BUTI_INITIATE_SERVICE_REJ,         16#60D37).
-define(CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM,   16#60D38).
-define(CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM, 16#60D39).
-define(CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,     16#60D3A).
-define(CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ,     16#60D3B).
-define(CELLO_BUTI_TERMINATE_SERVICE_CFM,        16#60D3C).
-define(CELLO_BUTI_EVENT_IND,                    16#60D3D).

-define(CELLO_BUTI_NO_PV, 0).
-define(CELLO_BUTI_PV1,   1).

-define(CELLO_BUTI_NO_FEEDBACK_BLINK_MODE, 0).
-define(CELLO_BUTI_FEEDBACK_BLINK_MODE,    1).

-define(CELLO_BUTI_OK,                           0).
-define(CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE, 1).
-define(CELLO_BUTI_SERVER_NOT_FOUND,             2).
-define(CELLO_BUTI_UNKNOWN_SIGNAL,               3).
-define(CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE,   4).
-define(CELLO_BUTI_INTERNAL_ERROR,               5).
-define(CELLO_BUTI_INVALID_PV,                   6).
-define(CELLO_BUTI_MEMORY_NOT_INITIATED,         7).
-define(CELLO_BUTI_SERVICE_BUSY,                 8).

-define(CELLO_BUTI_BUTTON_PRESSED,        0).
-define(CELLO_BUTI_BUTTON_SHORT_RELEASE,  1).
-define(CELLO_BUTI_BUTTON_MEDIUM_RELEASE, 2).

-record(state,
	{
	  clients = gb_trees:empty(),

	  button_state = released,
	  feedback_blink_mode = false,
	  deregister_vii_event = false,

	  itc_port,
	  port,

          restarted = false
	}
       ).

-record(client,
	{
	  spid,
	  notify = false,
	  feedback = false
	}
       ).

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
    start([]).

start(Opts) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

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

stop() ->
    gen_server:cast(?SERVER, stop).

activate() ->
    gen_server:cast(?SERVER, activate).

cec_setup(Socket) ->
    gen_server:call(?SERVER, {cec_setup, Socket}).

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

sim_button_event([]) ->
    print_BUTI_info(get_info());
sim_button_event(["-p" | Tail]) ->
    sim_button_events(Tail),
    print_BUTI_info(get_info());
sim_button_event(Args) ->
    sim_button_events(Args).

sim_button_events(["print_state" | Tail]) ->
    print_button_state(get_info()),
    sim_button_events(Tail);
sim_button_events(["pressed" | Tail]) ->
    set_button_event(pressed),
    sim_button_events(Tail);
sim_button_events(["2seconds" | Tail]) ->
    set_button_event('2seconds'),
    sim_button_events(Tail);
sim_button_events(["released" | Tail]) ->
    set_button_event(released),
    sim_button_events(Tail);
sim_button_events([]) ->
    ok;
sim_button_events([Arg | Tail]) ->
    io:format("Incorrect value: ~s\nType 'help' to see syntax.\n\n", [Arg]),
    sim_button_events(Tail).

set_button_event(ButtonEvent) when ButtonEvent =:= pressed;
				   ButtonEvent =:= '2seconds';
				   ButtonEvent =:= released ->
    gen_server:cast(?SERVER, {set_button_event, ButtonEvent}).

set_feedback_blink_mode(BlinkMode) when is_boolean(BlinkMode) ->
    gen_server:cast(?SERVER, {set_feedback_blink_mode, BlinkMode}).

get_info() ->
    gen_server:call(?SERVER, get_info).


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
    erlang:process_flag(trap_exit, true),

    State = #state{itc_port = itc_open(),
		   port = start_port()},

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
handle_call({cec_setup, Socket}, _From, S) ->
    Reply = self(),
    Client = #client{},
    NewClients = gb_trees:insert(Socket, Client, S#state.clients),

    {reply, Reply, S#state{clients = NewClients}};

handle_call(get_info, _From, State) ->
    Reply = [
	     {clients,              gb_trees:to_list(State#state.clients)},
             {button_state,         State#state.button_state},
	     {feedback_blink_mode,  State#state.feedback_blink_mode},
	     {deregister_vii_event, State#state.deregister_vii_event},
             {itc_port,             State#state.itc_port},
             {port,                 State#state.port},
             {restarted,            State#state.restarted}
            ],

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
handle_cast({set_button_event, ButtonEvent}, S) ->
    Msg = "set_button_event: " ++ atom_to_list(ButtonEvent),
    log(Msg),
    {noreply, handle_set_button_event(ButtonEvent, S)};

handle_cast({set_feedback_blink_mode, BlinkMode}, S) ->
    Msg = "set_feedback_blink_mode: " ++ atom_to_list(BlinkMode),
    log(Msg),
    {noreply, S#state{feedback_blink_mode = BlinkMode}};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(activate, S) ->
    {noreply, S};

handle_cast({cec_takeover, Socket}, S) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, S};

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
handle_info({Port, {data, <<?EVENT_IND:4/native-unsigned-integer-unit:8,
			    ?RHAI_MMI_FLAG_BUTTON_PRESSED:4/native-unsigned-integer-unit:8>>}},
	    #state{port = Port} = State) ->
    log("set_button_event: pressed"),
    {noreply, handle_set_button_event(pressed, State)};
handle_info({Port, {data, <<?EVENT_IND:4/native-unsigned-integer-unit:8,
			    ?RHAI_MMI_FLAG_BUTTON_2SECONDS:4/native-unsigned-integer-unit:8>>}},
	    #state{port = Port} = State) ->
    log("set_button_event: 2seconds"),
    {noreply, handle_set_button_event('2seconds', State)};
handle_info({Port, {data, <<?EVENT_IND:4/native-unsigned-integer-unit:8,
			    ?RHAI_MMI_FLAG_BUTTON_RELEASED:4/native-unsigned-integer-unit:8>>}},
	    #state{port = Port} = State) ->
    log("set_button_event: released"),
    {noreply, handle_set_button_event(released, State)};

handle_info({tcp, Socket, <<ClientPid:4/native-unsigned-integer-unit:8,
			    ?INIT_SERVICE:4/native-unsigned-integer-unit:8,
                            Spid:4/native-unsigned-integer-unit:8,
                            PvFirstWanted:4/native-unsigned-integer-unit:8,
                            PvSecondWanted:4/native-unsigned-integer-unit:8,
                            PvThirdWanted:4/native-unsigned-integer-unit:8,
                            ClientRef:4/native-unsigned-integer-unit:8>>},
            S) ->
    NewState = 'CelloButi_initiateService'({Socket,
					    Spid,
					    PvFirstWanted,
					    PvSecondWanted,
					    PvThirdWanted,
					    ClientRef}, S),
    Msg = "initiate from " ++ get_program_name(ClientPid),
    log(Msg),
    {noreply, NewState};
handle_info({tcp, Socket, <<ClientPid:4/native-unsigned-integer-unit:8,
			    ?TERM_SERVICE:4/native-unsigned-integer-unit:8,
                            ClientRef:4/native-unsigned-integer-unit:8>>},
            S) ->
    NewState = 'CelloButi_terminateService'({Socket, ClientRef}, S),
    Msg = "terminate from " ++ get_program_name(ClientPid),
    log(Msg),
    {noreply, NewState};

handle_info({tcp, Socket, <<ClientPid:4/native-unsigned-integer-unit:8,
			    ?SUBSCRIBE:4/native-unsigned-integer-unit:8,
                            ClientRef:4/native-unsigned-integer-unit:8>>},
            S) ->
    NewState = 'CelloButi_subscribeButtonEvent'({Socket, ClientRef}, S),
    Msg = "subscribe from " ++ get_program_name(ClientPid),
    log(Msg),
    {noreply, NewState};

handle_info({tcp, Socket, <<ClientPid:4/native-unsigned-integer-unit:8,
			    ?UNSUBSCRIBE:4/native-unsigned-integer-unit:8,
                            ClientRef:4/native-unsigned-integer-unit:8>>},
            S) ->
    NewState = 'CelloButi_unsubscribeButtonEvent'({Socket, ClientRef}, S),
    Msg = "unsubscribe from " ++ get_program_name(ClientPid),
    log(Msg),
    {noreply, NewState};

handle_info({tcp, Socket, <<ClientPid:4/native-unsigned-integer-unit:8,
			    ?CHANGE_FEEDBACK:4/native-unsigned-integer-unit:8,
                            FeedbackMode:4/native-unsigned-integer-unit:8,
                            ClientRef:4/native-unsigned-integer-unit:8>>},
            S) ->
    NewState = 'CelloButi_changeFeedBackMode'({Socket, FeedbackMode, ClientRef},
					      S),
    Msg = "changeFeedback " ++ integer_to_list(FeedbackMode) ++ " from " ++
	get_program_name(ClientPid),
    log(Msg),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, S) ->
    NewClients = gb_trees:delete(Socket, S#state.clients),
    NewState = update_feedback_blink_mode(S#state{clients = NewClients}),
    {noreply, NewState};

handle_info({timeout, _, reset_restart_timer}, State) ->
    {noreply, State#state{restarted = false}};

handle_info({'EXIT', Port, Reason}, #state{port      = Port,
					   restarted = false} = State) ->
    sysInitI:info_msg("~p: Port program has terminated~n"
		      "Reason: ~p~n"
		      "Action: Restart port program~n", [?MODULE, Reason]),
    NewState = State#state{port      = start_port(),
                           restarted = true},

    erlang:start_timer(?RESET_RESTART_TIMER, self(), reset_restart_timer),

    {noreply, NewState};
handle_info({'EXIT', Port, Reason}, #state{port      = Port,
					   restarted = true} = State) ->
    sysInitI:info_msg("~p: Port program has terminated~n"
		      "Reason: ~p~n"
		      "Action: Terminate ~p~n", [?MODULE, Reason, ?SERVER]),
    {stop, Reason, State};

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
terminate(_Reason, S) ->
    ok = foreach_client(
	   fun({Socket, _}) ->
		   gen_tcp:close(Socket)
	   end, S#state.clients),

    itc_close(S#state.itc_port),
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

start_port() ->
    AppName = "eqs_buttonctl",
    PrivDir = code:priv_dir(eqs),
    BinDir = sysEnv:target_bin_dir(),
    Prog = filename:join([PrivDir, BinDir, AppName]),
    ExtProg = swmI:find_file(Prog),

    open_port({spawn_executable, ExtProg}, [{packet, 4}, binary]).

'CelloButi_initiateService'({Socket, Spid, PvFirstWanted, PvSecondWanted,
			     PvThirdWanted, ClientRef}, S) ->
    ItcPort = S#state.itc_port,
    SignalRevision = 1,

    SelectedPv =
	get_selected_pv([PvFirstWanted, PvSecondWanted, PvThirdWanted]),

    NewClients =
	case SelectedPv of
	    ?CELLO_BUTI_PV1 ->
 		send_buti_initiate_service_cfm({ItcPort, Spid},
					       {SignalRevision, PvFirstWanted,
						ClientRef}),
		Client = #client{spid = Spid},
		gb_trees:update(Socket, Client, S#state.clients);
	    _ ->
		HighestSupportedPV = ?CELLO_BUTI_PV1,
		RejectReason = ?CELLO_BUTI_INVALID_PV,
		send_buti_initiate_service_rej({ItcPort, Spid},
					       {SignalRevision,
						HighestSupportedPV,
						RejectReason, ClientRef}),
		S#state.clients
	end,

    inet:setopts(Socket, [{active, once}]),
    S#state{clients = NewClients}.

'CelloButi_terminateService'({Socket, ClientRef}, S) ->
    #client{spid = Spid} = gb_trees:get(Socket, S#state.clients),
    send_buti_terminate_service_cfm({S#state.itc_port, Spid}, ClientRef),
    NewClients = gb_trees:delete(Socket, S#state.clients),
    gen_tcp:close(Socket),
    update_feedback_blink_mode(S#state{clients = NewClients}).

'CelloButi_subscribeButtonEvent'({Socket, ClientRef}, S) ->
    Client = gb_trees:get(Socket, S#state.clients),
    Spid = Client#client.spid,
    send_buti_subscribe_button_event_cfm({S#state.itc_port, Spid}, ClientRef),
    NewClient = Client#client{notify = true},
    NewClients = gb_trees:update(Socket, NewClient, S#state.clients),
    inet:setopts(Socket, [{active, once}]),
    S#state{clients = NewClients}.

'CelloButi_unsubscribeButtonEvent'({Socket, ClientRef}, S) ->
    Client = gb_trees:get(Socket, S#state.clients),
    Spid = Client#client.spid,
    send_buti_unsubscribe_button_event_cfm({S#state.itc_port, Spid}, ClientRef),
    NewClient = Client#client{notify = false},
    NewClients = gb_trees:update(Socket, NewClient, S#state.clients),
    inet:setopts(Socket, [{active, once}]),
    S#state{clients = NewClients}.

'CelloButi_changeFeedBackMode'({Socket, FeedbackMode, ClientRef}, S) ->
    ItcPort = S#state.itc_port,
    Client = gb_trees:get(Socket, S#state.clients),
    Spid = Client#client.spid,
    NewFeedback =
	case FeedbackMode of
	    ?CELLO_BUTI_NO_FEEDBACK_BLINK_MODE ->
		send_buti_change_feedback_mode_cfm({ItcPort, Spid},
						   {FeedbackMode, ClientRef}),
		false;
	    ?CELLO_BUTI_FEEDBACK_BLINK_MODE ->
		send_buti_change_feedback_mode_cfm({ItcPort, Spid},
						   {FeedbackMode, ClientRef}),
		true;
	    _ ->
		RejectReason = ?CELLO_BUTI_INTERNAL_ERROR,
		send_buti_change_feedback_mode_rej({ItcPort, Spid},
						   {RejectReason, FeedbackMode,
						    ClientRef}),
		Client#client.feedback
	end,
    NewClient = Client#client{feedback = NewFeedback},
    NewClients = gb_trees:update(Socket, NewClient, S#state.clients),

    inet:setopts(Socket, [{active, once}]),
    update_feedback_blink_mode(S#state{clients = NewClients}).

handle_set_button_event(pressed, #state{button_state        = released,
					feedback_blink_mode = true} = S) ->
    send_button_event(?CELLO_BUTI_BUTTON_PRESSED, S),
    eqs_vii:'CelloVii_visualIndRequest'(?CELLO_VII_SHORT_BUTTON_PRESS_START),

    S#state{button_state = pressed,
	    deregister_vii_event = {true, ?CELLO_VII_SHORT_BUTTON_PRESS_END}};
handle_set_button_event(pressed, #state{button_state = released} = S) ->
    send_button_event(?CELLO_BUTI_BUTTON_PRESSED, S),

    S#state{button_state = pressed};
handle_set_button_event(pressed, S) ->
    %% Should never happen
    S#state{button_state = pressed};

handle_set_button_event('2seconds', #state{button_state        = pressed,
					   feedback_blink_mode = true} = S) ->
    eqs_vii:'CelloVii_visualIndRequest'(?CELLO_VII_MEDIUM_BUTTON_PRESS_START),
    eqs_vii:'CelloVii_visualIndRequest'(?CELLO_VII_SHORT_BUTTON_PRESS_END),

    S#state{button_state = '2seconds',
	    deregister_vii_event = {true, ?CELLO_VII_MEDIUM_BUTTON_PRESS_END}};

handle_set_button_event('2seconds', #state{button_state = pressed} = S) ->
    S#state{button_state = '2seconds'};
handle_set_button_event('2seconds', S) ->
    %% Should never happen
    S#state{button_state = '2seconds'};

handle_set_button_event(released, #state{button_state = pressed,
					 feedback_blink_mode = true} = S) ->
    send_button_event(?CELLO_BUTI_BUTTON_SHORT_RELEASE, S),
    eqs_vii:'CelloVii_visualIndRequest'(?CELLO_VII_SHORT_BUTTON_PRESS_END),

    S#state{button_state = released,
	    deregister_vii_event = false};
handle_set_button_event(released, #state{button_state = pressed} = S) ->
    send_button_event(?CELLO_BUTI_BUTTON_SHORT_RELEASE, S),
    S#state{button_state = released,
	    deregister_vii_event = false};

handle_set_button_event(released, #state{button_state        = '2seconds',
					 feedback_blink_mode = true} = S) ->
    send_button_event(?CELLO_BUTI_BUTTON_MEDIUM_RELEASE, S),
    eqs_vii:'CelloVii_visualIndRequest'(?CELLO_VII_MEDIUM_BUTTON_PRESS_END),

    S#state{button_state         = released,
	    deregister_vii_event = false};
handle_set_button_event(released, #state{button_state        = '2seconds',
					 feedback_blink_mode = false} = S) ->
    send_button_event(?CELLO_BUTI_BUTTON_MEDIUM_RELEASE, S),

    S#state{button_state         = released,
	    deregister_vii_event = false};
handle_set_button_event(released, S) ->
    %% Should never happen
    S#state{button_state = released}.

send_buti_initiate_service_cfm({ItcPort, Spid},
			       {SignalRevision, SelectedPV, ClientRef}) ->
    SigNo = ?CELLO_BUTI_INITIATE_SERVICE_CFM,
    Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
	     SelectedPV:4/native-unsigned-integer-unit:8,
	     ClientRef:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_buti_initiate_service_rej({ItcPort, Spid},
			       {SignalRevision, HighestSupportedPV,
				RejectReason, ClientRef}) ->
    SigNo = ?CELLO_BUTI_INITIATE_SERVICE_REJ,
    Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
	     HighestSupportedPV:4/native-unsigned-integer-unit:8,
	     RejectReason:4/native-unsigned-integer-unit:8,
	     ClientRef:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_buti_terminate_service_cfm({ItcPort, Spid}, ClientRef) ->
    SigNo = ?CELLO_BUTI_TERMINATE_SERVICE_CFM,
    Data = <<ClientRef:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_buti_subscribe_button_event_cfm({ItcPort, Spid}, ClientRef) ->
    SigNo = ?CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM,
    Data = <<ClientRef:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_buti_unsubscribe_button_event_cfm({ItcPort, Spid}, ClientRef) ->
    SigNo = ?CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM,
    Data = <<ClientRef:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_buti_event_ind({ItcPort, Spid}, ButtonEventType) ->
    SigNo = ?CELLO_BUTI_EVENT_IND,
    Data = <<ButtonEventType:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_buti_change_feedback_mode_cfm({ItcPort, Spid},
				   {FeedbackMode, ClientRef}) ->
    SigNo = ?CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,
    Data = <<ClientRef:4/native-unsigned-integer-unit:8,
	     FeedbackMode:4/native-unsigned-integer-unit:8
	   >>,
    itc_send(ItcPort, Spid, SigNo, Data).

send_buti_change_feedback_mode_rej({ItcPort, Spid},
				   {RejectReason, FeedbackMode, ClientRef}) ->
    SigNo = ?CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ,
    Data = <<RejectReason:4/native-unsigned-integer-unit:8,
	     ClientRef:4/native-unsigned-integer-unit:8,
	     FeedbackMode:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).

itc_open() ->
    Name = "BUTI",

    itc:open(Name).

itc_close(undefined) ->
    ok;
itc_close(ItcPort) ->
    itc:close(ItcPort).

itc_send(ItcPort, Spid, SigNo, Data) ->
    itc:send(ItcPort, Spid, SigNo, iolist_to_binary(Data)).

foreach_client(Fun, Clients) ->
    Iter = gb_trees:iterator(Clients),
    foreach_client1(Fun, Iter).

foreach_client1(Fun, Iter) ->
    case gb_trees:next(Iter) of
        none ->
            ok;
        {Key, Val, Iter2} ->
            Fun({Key, Val}),
            foreach_client1(Fun, Iter2)
    end.

send_button_event(ButtonEventType, S) ->
    print_button_event(ButtonEventType),
    ItcPort = S#state.itc_port,
    ok = foreach_client(
	   fun({_Socket, #client{notify = false}}) ->
		   ok;
	      ({_Socket, #client{spid   = Spid,
				 notify = true}}) ->
		   send_buti_event_ind({ItcPort, Spid}, ButtonEventType)
	   end, S#state.clients).

print_button_event(?CELLO_BUTI_BUTTON_PRESSED) ->
    sysInitI:info_msg("~p: Button pressed~n", [?MODULE]);
print_button_event(?CELLO_BUTI_BUTTON_SHORT_RELEASE) ->
    sysInitI:info_msg("~p: Button released short~n", [?MODULE]);
print_button_event(?CELLO_BUTI_BUTTON_MEDIUM_RELEASE) ->
    sysInitI:info_msg("~p: Button released medium~n", [?MODULE]).

get_selected_pv([?CELLO_BUTI_PV1|_]) ->
    ?CELLO_BUTI_PV1;
get_selected_pv([_|T]) ->
    get_selected_pv(T);
get_selected_pv([]) ->
    ?CELLO_BUTI_NO_PV.

update_feedback_blink_mode(S) ->
    FeedbackBlinkMode = get_feedback_blink_mode(S),
    DeregisterViiEvent = get_deregister_vii_event(S, FeedbackBlinkMode),

    S#state{feedback_blink_mode  = FeedbackBlinkMode,
	    deregister_vii_event = DeregisterViiEvent}.

get_feedback_blink_mode(S) ->
    lists:any(
      fun(#client{feedback = Feedback}) ->
	      Feedback
      end, gb_trees:values(S#state.clients)).

get_deregister_vii_event(#state{deregister_vii_event = {true, ViiEvent}},
			 false) ->
    eqs_vii:'CelloVii_visualIndRequest'(ViiEvent),
    false;
get_deregister_vii_event(#state{deregister_vii_event = {true, ViiEvent}},
			 true) ->
    {true, ViiEvent};
get_deregister_vii_event(#state{deregister_vii_event = false}, _) ->
    false.

print_button_state([{button_state, Val} | Tail]) ->
    io:format("BUTTON STATE:         ~s~n", [format_value(Val)]),
    print_button_state(Tail);
print_button_state([_ | Tail]) ->
    print_button_state(Tail);
print_button_state([]) ->
    io:format("~n").

print_BUTI_info([{button_state, Val} | Tail]) ->
    io:format("BUTTON STATE:         ~s~n", [format_value(Val)]),
    print_BUTI_info(Tail);
print_BUTI_info([{feedback_blink_mode, Val} | Tail]) ->
    io:format("FEEDBACK BLINK MODE:  ~s~n", [format_value(Val)]),
    print_BUTI_info(Tail);
print_BUTI_info([{deregister_vii_event, Val} | Tail]) ->
    io:format("DEREGISTER VII EVENT: ~s~n", [format_value(Val)]),
    print_BUTI_info(Tail);
print_BUTI_info([_ | Tail]) ->
    print_BUTI_info(Tail);
print_BUTI_info([]) ->
    io:format("~n").

format_value(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_value(Val) when is_list(Val) ->
    Val;
format_value(_) ->
    "Unknown value".

log(Msg) ->
    sysEnv:rcs_mode_2() /= vrcs andalso
	logI:write_log("MMILog", "BUTI", info, Msg).

get_program_name(ClientPid) ->
    Name = cec:get_program_name(ClientPid),
    gpn(Name).

gpn(Name) when is_list(Name) -> Name;
gpn(undefined)               -> "Unknown".

%%%-------------------------------------------------------------------
%%% @private
%%% @end
%%%-------------------------------------------------------------------

%%
%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, predefined type tuple/N, N>0,  no longer exists
%% instead the userdefined type from 
%% must be defined as a normal tuple {}.


-module(comte_com).

%% API exports

-export(
   [start_link/0,
    start/0,start/1,
    stop/0,stop/1,stop_if_started/0,
    kill/0, kill/1]).

-export(
   [setHaMode/3,healthCheck/0,healthCheck/1,get_pid/0]).

%% Bert server API

-export(
   [acInitialize/1,healthCheckReport/2,haModeAssumed/1,
    prepareTerminationResponse/1]).

%% Behaviour exports

-behaviour(gen_server).
-export([init/1,terminate/2,code_change/3]).
-export([handle_call/3,handle_cast/2,handle_info/2]).

%% Get application env
-export([env_val/1,env_val/2]).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------
-include("comte_ac.hrl").

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------
-type reason() :: normal | shutdown | term().
-type from() :: {pid(), term()}.
-type state() ::
	{init} |
	{init_com,wait_acInitialize} |
	{init_com,setHaMode} |
	{init_com,healthCheck} |
	{start_com,wait_acInitialize} |
	{start_com,setHaMode} |
	{start_com,healthCheck} |
	{external_start_com,setHaMode} |
	{external_start_com,healthCheck} |
	{com_running,normal} |
	{com_running,healthCheck} |
	{com_running,setHaMode} |
	{com_running,interval_healthCheck} |
	{stop_com,prepareTermination} |
	{stop_com,term} |
	{stop_com,kill} |
	{kill_com,term} |
	{kill_com,kill}.

%%                  State transition diagram
%%
%%                             |
%% +-------------------------->+
%% |                           |
%% |                           V
%% |                         {init} (*)
%% |                         | | |
%% |   +---------------------+ | +-------------------+
%% |   |                       |                     |
%% |   V                       V                     |
%% |  {init_com,              {start_com,            |
%% |   wait_acInitialize}      wait_acInitialize}    |
%% |   |                       |                     |
%% |   V                       V                     V
%% |  {init_com,              {start_com,           {external_start_com,
%% |   setHaMode}              setHamode}            setHaMode}
%% |   |                       |                     |
%% |   V                       V                     V
%% |  {init_com,              {start_com,           {external_start_com,
%% |   healthCheck}            healthCheck}          healthCheck}
%% |    |                       |                    |
%% |    |                       V                    |
%% |    +-----------------------+---->+<-------------+
%% |                                  |
%% |                                  V
%% |                          {com_running,normal} (*)
%% |                           | | |  | A  | A  |  A
%% +<--------------------------+ | |  | |  | |  V  |
%% A                             | |  | |  | | {com_running,setHaMode}
%% |                             | |  | |  V |
%% |                             | |  | | {com_running,healthCheck}
%% |                             | |  | |    A
%% |                             | |  V |    |
%% |         +-------------------+ | {com_running,
%% |         |                     |  interval_healthCheck} (+)
%% |         V                     |
%% |  {stop_com,                   |
%% |   prepareTermination}         |
%% |   |  |  |                     |
%% +<--+  |  V                     V
%% A      | {stop_com,term}       {kill_com,term}
%% |      |  |  |                  |  |
%% |      |  V  |                  |  |
%% +<--------+---------------------+  |
%% A      |     |                     |
%% |      V     V                     V
%% |     {stop_com,kill}            {kill_com,kill}
%% |      |                           |
%% |      V                           |
%% +------+<--------------------------+
%%
%% (*) User requests are handled in these two states.
%% (+) User requests are stored as pending in this state
%%     and handled when exiting the state.
%% In the other states user requests get the response
%% {error,action_already_in_progress}. All user requests are supposed
%% to be serialized by having just one process controlling comte_com.
%%
%% Read the code for state transition events and actions.
%%
%% Most states can also stop the server e.g due to timeout. Seriously;
%% if we send a setHaMode and there comes no haModeAssumed within the
%% expected time, we really have no clue what state COM is in...
%%
%% Variables are stored in the process dictionary and the handling of
%% them is intentionally unforgiving. Where possible the previous
%% value is checked and if not consistent there will be a server crash.
%% The state should be enough to know if a variable is valid or not.
%% Exception: the variable 'port' is actually a sub-state to all
%% states but {init}.

%% --------------------------------------------------------------------
%% "Defines"
%% --------------------------------------------------------------------

-compile({inline,
	  [name/0,
	   fix_timeout/1,fix_port_start_timeout/1,
	   default_stop_timeout/0,default_kill_timeout/0,
	   default_hard_kill_timeout/0,
	   var/1]}).

name() -> ?MODULE. %% gen_server name

fix_timeout(Timeout) -> max(Timeout, 100).
fix_port_start_timeout(Timeout) ->
    max(Timeout, 1000).
default_stop_timeout() -> 5000.
default_kill_timeout() -> 3000.
default_hard_kill_timeout() -> 500.

%% Match function that verifies a state variable name
var(Var) -> % State variables
    case Var of
	req -> % User request in progress.
	    %% Defined all states but {init} and {com_running,normal}
	    ok;
	com_pid -> % OS Pid of COM.
	    %% Defined in all states but {init}
	    ok;
	port -> % Port running the COM process.
	    %% Defined in all states but {init} iff we started COM.
	    %% The existence of this variable is used as as sub-state
	    %% or maybe rather a superstate.
	    ok;
	exit_status -> % Exit status from port
	    %% Defined iff exit_status from port has been seen
	    %% hence not in {init}
	    ok;
	healthcheck_interval -> % Timer ref
	    %% Defined in state {com_running,normal}
	    ok
    end.



%% Logging functions return 'ok'

-define(
   INFO(Req, State, Fmt),
   begin
       comte_lib:error_logger(
	 info, ?MODULE, ?LINE, (Req), [State], (Fmt))
   end).
-define(
   INFO(Req, State, Fmt, Args),
   begin
       comte_lib:error_logger(
	 info, ?MODULE, ?LINE, (Req), [State], (Fmt), (Args))
   end).

-define(
   WARNING(Req, State, Fmt),
   begin
       comte_lib:error_logger(
	 warning, ?MODULE, ?LINE, (Req), [State], (Fmt))
   end).
-define(
   WARNING(Req, State, Fmt, Args),
   begin
       comte_lib:error_logger(
	 warning, ?MODULE, ?LINE, (Req), [State], (Fmt), (Args))
   end).

-define(
   ERROR(Req, State, Fmt),
   begin
       comte_lib:error_logger(
	 error, ?MODULE, ?LINE, (Req), [State|erlang:get()], (Fmt))
   end).
-define(
   ERROR(Req, State, Fmt, Args),
   begin
       comte_lib:error_logger(
	 error, ?MODULE, ?LINE, (Req), [State|erlang:get()], (Fmt), (Args))
   end).

-define(
   LOG_STARTED(Func),
   begin
       log_started((Func), ?LINE)
   end).

log_started(Func, Line) ->
    {ok, ComteVsn} = application:get_key(comte, vsn),
    VsnBin = list_to_binary(ComteVsn),
    ok =
	comte:log(
	  ?MODULE, Func, Line, info, 1,
	  <<"Started ComtE: vsn: ", VsnBin/binary>>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the gen_server which starts and supervises COM
%% @end
%%--------------------------------------------------------------------

start_link() ->
    proc_lib:start_link(?MODULE, init, [void]).

%%--------------------------------------------------------------------
%% @doc Start the COM executable.
%%      If any COM instances is running, the wrapper
%%      script will try to kill those instances
%%      to the best of it's ability.
%% @end
%%--------------------------------------------------------------------

start() ->
    start(env_val(start_com_timeout)).

start(Timeout) when is_integer(Timeout) ->
    gen_call({start_com,Timeout}).

%%--------------------------------------------------------------------
%% @doc Stop the COM executable
%%      Tries to shutdown COM in an ordered way via
%%      prepareTermination and terminateProcess
%% @end
%%--------------------------------------------------------------------

stop() ->
    stop(default_stop_timeout()).

stop(Timeout) when is_integer(Timeout) ->
    gen_call({stop_com,Timeout}).

%%--------------------------------------------------------------------
%% @private
%% @doc Called by comte_app in prep_stop
%%--------------------------------------------------------------------

stop_if_started() ->
    gen_call({stop_com_if_started,default_stop_timeout()}).

%%--------------------------------------------------------------------
%% @doc Kill the COM executable
%%      ComtE will send the TERM signal with the timeout given.
%%      If the timer expires, the KILL signal is sent with timeout
%%      500 ms.
%%--------------------------------------------------------------------

kill() ->
    kill(default_kill_timeout()).

kill(Timeout) when is_integer(Timeout) ->
    gen_call({kill_com,Timeout}).


setHaMode(HaMode, HaReason, Timeout) when is_integer(Timeout) ->
    gen_call({setHaMode,HaMode,HaReason,Timeout}).

healthCheck() ->
    healthCheck(env_val(healthcheck_timeout)).

healthCheck(Timeout) when is_integer(Timeout) ->
    gen_call({healthCheck,Timeout}).

%%--------------------------------------------------------------------
%% @private
%% @doc Calls from COM via Bert server
%%--------------------------------------------------------------------

acInitialize(ComPid) ->
    gen_call({acInitialize,ComPid}).

healthCheckReport(HealthReport, RecommendedResponse) ->
    gen_call({healthCheckReport,HealthReport,RecommendedResponse}).

haModeAssumed(Result) ->
    gen_call({haModeAssumed,Result}).

prepareTerminationResponse(Result) ->
    gen_call({prepareTerminationResponse,Result}).

get_pid() ->
    gen_call(get_pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
-spec init(void) -> no_return().

init(void) ->
    process_flag(trap_exit, true),
    Name = name(),
    register(Name, self()),
    ServerSpec = {local,Name},
    P = queue:new(),
    case env_val(start_com) of
        true ->
            %% Trigger start of COM
	    S = start_com(start_com, init_com),
	    gen_server:enter_loop(?MODULE, [], {S,P}, ServerSpec);
        false ->
	    init_ack(),
	    gen_server:enter_loop(?MODULE, [], {{init},P}, ServerSpec)
    end.



%% Start helper

start_com(Event, NewMajorState) ->
    case start_com_prog() of
        {error,Reason} ->
	    erlang:error(Reason);
        {Port, ComPid} ->
	    v_insert(port, Port),
	    v_insert(com_pid, ComPid),
	    case comte:get_env(com_start_mw) of
		{ok,true} ->
		    %% Wait for acInitialize - will trigger next step.
		    {NewMajorState,wait_acInitialize};
		{ok,false} ->
		    statem_com_cast(
		      Event,
		      healthCheck, [],
		      {NewMajorState,healthCheck})
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%--------------------------------------------------------------------
-spec handle_call(
	Request :: term(),
	From    :: from(),
	State   :: state() ) ->
			 {noreply, state()} |
			 {stop, term(), state()}.

handle_call(Request, From, State) ->
    handle_event({call,Request,From}, State).

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: state()) -> none().

handle_cast(Request, State) ->
    erlang:error(undef, [Request,State]).

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: state()) ->
			 {noreply, state()} |
			 {stop, term(), state()}.

handle_info(Event, State) ->
    handle_event(
      %% Do not wrap well known events that are already tagged tuples
      case Event of
	  {timeout,_,_} ->
	      Event;
	  {'EXIT',_,_} ->
	      Event;
	  _ ->
	      {info,Event}
      end,
      State).

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate.
%%--------------------------------------------------------------------
-spec terminate(Reason :: reason(), State :: state()) -> ok.
terminate(_Type, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%--------------------------------------------------------------------
-spec code_change(
	OldVsn :: {down,term()} | term(),
	State :: state(),
	Extra :: term()) ->
			 {ok,NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.



%%%===================================================================
%%% State Machine Engine
%%%
%%% handle_event/2 and handle_events/3
%%%
%%% Implements a state machine on top of gen_server that allows
%%% postponing events for handling in other states.  The postponed
%%% events are queued and retried after state change.
%%%
%%% State variables are kept in the process dictionary abstracted by
%%% the functions v_insert/2, v_get/1-2, v_is_defined/1 and
%%% v_delete/1-2. var/1 ensures that only known variables are used.
%%%
%%% NOTE. A ?DISCARD does not cause postponed events to be retried,
%%% for this a state change is needed, so watch out with depending
%%% on state variables in conjunction with ?DISCARD.  If the state
%%% is consumed by changing states to the same state the postponed
%%% events are retried.  ?DISCARD is regarded as out of the ordinary
%%% and renders an error logger warning.
%%%
%%% All gen_server callbacks i.e handle_call/3 and handle_info/2
%%% (handle_cast/2 is not currently used) passes tagged tuples
%%% to handle_event/2 that implements the State Machine Engine.
%%%
%%% The State Machine Engine calls statem(State, Event) that
%%% implements the actual state machine.
%%%
%%% {noreply,...} is always returned from the gen_server callbacks
%%% so statem/2 have to use gen_server:reply/2 for replies.
%%%
%%% A state can be any term except a list, but right now statem/2
%%% only uses one- or two-tuples of atoms e.g {State,SubState}.
%%%===================================================================

%% These macros shall be used for the final value of statem/2
%% to control the State Machine Engine by returning terms to it.
%% Commands are returned as lists hence a state can not be a list.
%%
%% Macros are used since they can do the trick used in the DISCARD
%% command to get a stacktrace for the line that called the macro.
%%
%% Discard the current event
-define(DISCARD,
	[discard,
	 try throw(discard)
	 catch discard -> erlang:get_stacktrace()
	 end]).
%%
%% Postpone the current event
-define(POSTPONE, [postpone]).
%%
%% Stop the gen_server
-define(STOP(Reason), [stop,begin Reason end]).
%%
%% Next state, may be the same; consume current event.
-define(STATE(State), next_state(State)).
next_state(State) when not is_list(State) -> State.

handle_event(Event, {State,Postponed} = SP) ->
    case statem(State, Event) of
	[discard,Stacktrace] ->
	    ?WARNING(Event, SP, "Discarded event at: ~p", [Stacktrace]),
	    {noreply,SP};
	[postpone] ->
	    {noreply,{State,queue:snoc(Postponed, Event)}};
	[stop,Reason] ->
	    handle_event_log_stop(Event, State, Postponed, Reason),
	    {stop,Reason,SP};
	NewState when not is_list(NewState) ->
	    handle_events(queue:new(), NewState, Postponed)
    end.

handle_events(Q, State, Postponed) ->
    %% Q contains the events from Postponed that have been postponed
    %% again, in the same order.  When the state changes these events
    %% are placed first in the new queue of postponed events so
    %% events are processed in incoming order if possible.
    case queue:is_empty(Postponed) of
	true ->
	    {noreply,{State,Q}};
	false ->
	    Event = queue:head(Postponed),
	    Tail = queue:tail(Postponed),
	    case statem(State, Event) of
		[discard,Stacktrace] ->
		    ?WARNING(
		       Event, {State,{Q,Tail}},
		       "Discarded event at: ~p", [Stacktrace]),
		    handle_events(Q, State, Tail);
		[postpone] ->
		    handle_events(queue:snoc(Q, Event), State, Tail);
		[stop,Reason] ->
		    NewPostponed = queue:join(Q, Tail),
		    handle_event_log_stop(Event, State, NewPostponed, Reason),
		    {stop,Reason,{State,NewPostponed}};
		NewState when not is_list(NewState) ->
		    %% Retry all events
		    handle_events(queue:new(), NewState, queue:join(Q, Tail))
	    end
    end.

handle_event_log_stop(Event, State, Postponed, Reason) ->
    case Reason of
	shutdown ->
	    ?INFO(Event, State, "Stop");
	{error,Error} ->
	    ?WARNING(Event, State, "Stop: ~p ~p", [Error,Postponed])
    end.

%%%===================================================================
%%% State Machine - functions statem and statem_*
%%%===================================================================

statem({init} = State, Event) ->
    case Event of
	%% API Calls
	{call,{start_com,Timeout},From} ->
	    req_start(start_com, Timeout, From),
	    ?STATE(start_com(Event, start_com));
	{call,{StopCom,_},From}
	  when StopCom =:= stop_com;
	       StopCom =:= stop_com_if_started;
	       StopCom =:= kill_com ->
	    reply({error,already_stopped}, From),
	    ?STATE(State);
	{call,{healthCheck,_},From} ->
	    reply({error,com_not_started}, From),
	    ?STATE(State);
	{call,{setHaMode,_,_,_},From} ->
	    reply({error,com_not_started}, From),
	    ?STATE(State);
	%% Events
	{call,{acInitialize,ComPid},From} ->
	    reply(ok, From),
	    ?WARNING(
	       Event, State,
	       "Unexpected since COM was not started by me"),
	    v_insert(com_pid, ComPid),
	    case comte:get_env(com_start_mw) of
		{ok,true} ->
		    statem_com_cast_initial_setHaMode(
		      Event, {external_start_com,setHaMode});
		{ok,false} ->
		    ?LOG_STARTED(statem),
		    healthcheck_interval_start(),
		    ?STATE({com_running,normal})
	    end;
	_ ->
	    statem_common(State, Event)
    end;

statem({StartCom,SubState} = State, Event)
  when StartCom =:= init_com;
       StartCom =:= start_com;
       StartCom =:= external_start_com ->
    case Event of
	{timeout,Tref,Req} ->
	    case v_get(req) of
		{Tref,_} ->
		    ?WARNING(Event, State, "Timeout: ~p", [Req]),
		    req_timeout(Tref),
		    kill_com_brutally_log(Event, v_get(com_pid), State),
		    ?STOP({error,{State,timeout}});
		_ ->
		    statem_common(State, Event)
	    end;
	%% API Calls
	{call,{Action,_},_}
	  when Action =:= start_com;
	       Action =:= stop_com;
	       Action =:= stop_com_if_started;
	       Action =:= kill_com;
	       Action =:= healthCheck ->
	    ?POSTPONE;
	{call,{setHaMode,_,_,_}} ->
	    ?POSTPONE;
	%% Events
	{call,{acInitialize,ComPid},From}
	  when SubState =:= wait_acInitialize ->
	    reply(ok, From),
	    case v_get(com_pid) of
		ComPid ->
		    statem_com_cast_initial_setHaMode(
		      Event, {StartCom,setHaMode});
		KnownPid when is_integer(KnownPid) ->
		    ?ERROR(
		       Event, State,
		       "Startup error: COM pid not ~p - exiting!",
		       [KnownPid]),
		    kill_com_proc(ComPid, kill),
		    kill_com_proc(KnownPid, kill),
		    ?STOP({error,Event})
	    end;
	{call,{haModeAssumed,Result},From}
	  when SubState =:= setHaMode ->
	    reply(ok, From),
	    case Result of
		?ComOk ->
		    statem_com_cast(
		      Event,
		      healthCheck, [],
		      {StartCom,healthCheck});
		_ ->
		    ?ERROR(
		       Event, State,
		       "Startup error - exiting!"),
		    kill_com_proc(v_get(com_pid), kill),
		    ?STOP({error,Event})
	    end;
	{call,{healthCheckReport,Report,Response},From}
	  when SubState =:= healthCheck ->
	    reply(ok, From),
	    case healthCheckReport_reply(Report, Response) of
		?ComOk ->
		    PidStr = integer_to_list(v_get(com_pid)),
		    case StartCom of
			init_com ->
			    init_ack();
			start_com ->
			    req_reply({ok,PidStr});
			external_start_com ->
			    ok
		    end,
		    case comte:get_env(com_start_mw) of
			{ok,true} ->
			    ?INFO(
			       Event, State,
			       "COM[~s] has been started", [PidStr]);
			{ok,false} ->
			    ?INFO(
			       Event, State,
			       "COM[~s] started without middleware",
			       [PidStr])
		    end,
		    ?LOG_STARTED(statem),
		    healthcheck_interval_start(),
		    ?STATE({com_running,normal});
		Error ->
		    ?ERROR(
		       Event, State,
		       "Startup error - exiting!"),
		    kill_com_proc(v_get(com_pid), kill),
		    ?STOP({error,Error})
	    end;
	_ ->
	    statem_common(State, Event)
    end;

statem({com_running,normal} = State, Event) ->
    case Event of
	{timeout,Tref,_} ->
	    case v_get(healthcheck_interval) of
		Tref ->
		    healthcheck_interval_timeout(Tref),
		    Timeout = env_val(healthcheck_timeout),
		    case statem_com_cast(Event, healthCheck, [], ok) of
			ok ->
			    req_start(
			      interval_healthCheck, Timeout, undefined),
			    {com_running,interval_healthCheck};
			Other -> Other
		    end;
		_ ->
		    statem_common(State, Event)
	    end;
	%% API Calls
	{call,{start_com,_},From} ->
	    reply({error,already_started}, From),
	    ?STATE(State);
	{call,{StopCom,Timeout},From}
	  when StopCom =:= stop_com;
	       StopCom =:= stop_com_if_started ->
	    healthcheck_interval_cancel(),
	    case statem_com_cast(Event, prepareTermination, [], ok) of
		ok ->
		    req_start(stop_com, Timeout, From),
		    {stop_com,prepareTermination};
		Other -> Other
	    end;
	{call,{kill_com,Timeout},From} ->
	    healthcheck_interval_cancel(),
	    ComPid = v_get(com_pid),
	    case v_is_defined(port) of
		true ->
		    req_start(kill_com, Timeout, From),
		    kill_com_proc(ComPid),
		    ?STATE({kill_com,term});
		false ->
		    kill_com_brutally_log(Event, ComPid, State),
		    _ = v_delete(com_pid),
		    reply(ok, From),
		    ?STATE({init})
	    end;
	{call,{healthCheck,Timeout},From} ->
	    healthcheck_interval_cancel(),
	    case statem_com_cast(Event, healthCheck, [], ok) of
		ok ->
		    req_start(healthCheck, Timeout, From),
		    {com_running,healthCheck};
		Other -> Other
	    end;
	{call,{setHaMode,Mode,Reason,Timeout},From} ->
	    healthcheck_interval_cancel(),
	    case statem_com_cast(Event, setHaMode, [Mode,Reason], ok) of
		ok ->
		    req_start(setHaMode, Timeout, From),
		    {com_running,setHaMode};
		Other -> Other
	    end;
	_ ->
	    statem_common(State, Event)
    end;

statem({com_running,SubState} = State, Event) ->
    case Event of
	{timeout,Tref,Req} ->
	    case v_get(req) of
		{Tref,_} ->
		    ?WARNING(Event, State, "Timeout: ~p", [Req]),
		    req_timeout(Tref),
		    kill_com_brutally_log(Event, v_get(com_pid), State),
		    ?STOP(shutdown);
		_ ->
		    statem_common(State, Event)
	    end;
	%% API Calls
	{call,{start_com,_},From} ->
	    reply({error,already_started}, From),
	    ?STATE(State);
	{call,{Action,_},_}
	  when Action =:= stop_com;
	       Action =:= stop_com_if_started;
	       Action =:= kill_com ->
	    ?POSTPONE;
	{call,{healthCheck,Timeout},From} ->
	    case SubState of
		interval_healthCheck ->
		    %% Hijack the ongoing request
		    req_cancel(),
		    req_start(healthCheck, Timeout, From),
		    ?STATE({com_running,healthCheck});
		_ ->
		    ?POSTPONE
	    end;
	{call,{setHaMode,_,_,_},_} ->
	    ?POSTPONE;
	%% Events
	{call,{healthCheckReport,Report,Response},From} ->
	    reply(ok, From),
	    Reply = healthCheckReport_reply(Report, Response),
	    case SubState of
		interval_healthCheck ->
		    req_cancel(),
		    case Reply of
			?ComOk ->
			    healthcheck_interval_start(),
			    ?STATE({com_running,normal});
			Error ->
			    ?ERROR(
			       Error, State,
			       "COM interval healthCheck error ~p - exiting!",
			       [Error]),
			    kill_com_proc(v_get(com_pid), kill),
			    ?STOP(shutdown)
		    end;
		healthCheck ->
		    req_reply(Reply),
		    Reply =:= ?ComOk orelse
			?WARNING(Event, State, "COM reply is worrying"),
		    healthcheck_interval_start(),
		    ?STATE({com_running,normal});
		_ ->
		    statem_common(State, Event)
	    end;
	{call,{haModeAssumed,Result},From}
	  when SubState =:= setHaMode ->
	    reply(ok, From),
	    req_reply(Result),
	    healthcheck_interval_start(),
	    ?STATE({com_running,normal});
	_ ->
	    statem_common(State, Event)
    end;

statem({stop_com,prepareTermination} = State, Event) ->
    case Event of
	{timeout,Tref,Req} ->
	    case v_get(req) of
		{Tref,From} ->
		    ?WARNING(Event, State, "Timeout: ~p", [Req]),
		    req_timeout(Tref),
		    ComPid = v_get(com_pid),
		    case v_is_defined(port) of
			true ->
			    %% Hard kill and wait a little while more
			    kill_com_proc(ComPid, kill),
			    req_start(
			      stop_com, default_hard_kill_timeout(), From),
			    ?STATE({stop_com,kill});
			false ->
			    kill_com_brutally_log(Event, ComPid, State),
			    ?STOP(shutdown)
		    end;
		_ ->
		    statem_common(State, Event)
	    end;
	%% API Calls
	{call,{Action,_},_}
	  when Action =:= start_com;
	       Action =:= stop_com;
	       Action =:= stop_com_if_started;
	       Action =:= kill_com;
	       Action =:= healthCheck ->
	    ?POSTPONE;
	{call,{setHaMode,_,_,_}} ->
	    ?POSTPONE;
	%% Events
	{call,{prepareTerminationResponse,Result},From} ->
	    reply(ok, From),
	    case Result of
		?ComOk ->
		    case v_is_defined(port) of
			true ->
			    statem_com_cast(
			      Event,
			      terminateProcess, [],
			      {stop_com,term});
			false ->
			    %% We have no port to wait for so assume OK
			    ?INFO(
			       Event, State,
			       "COM is asked to shutdown nicely"),
			    req_reply(ok),
			    _ = v_delete(com_pid),
			    statem_com_cast(
			      Event,
			      terminateProcess, [],
			      {init})
		    end;
		_ ->
		    case v_is_defined(port) of
			true ->
			    ?WARNING(
			       Event, State,
			       "Not quite the expected reply from ~p",
			       [From]),
			    kill_com_proc(v_get(com_pid)),
			    ?STATE({stop_com,term});
			false ->
			    kill_com_brutally_log(
			      Event, v_delete(com_pid), State),
			    %% We have no port to wait for so assume OK
			    req_reply(ok),
			    ?STATE({init})
		    end
	    end;
	_ ->
	    statem_common(State, Event)
    end;

statem({StopCom,SubState} = State, Event)
  when StopCom =:= stop_com;
       StopCom =:= kill_com ->
    case Event of
	{timeout,Tref,Req} ->
	    case v_get(req) of
		{Tref,From} ->
		    ?WARNING(Event, State, "Timeout: ~p", [Req]),
		    req_timeout(Tref),
		    case SubState of
			term ->
			    %% Hard kill and wait a little while more
			    kill_com_proc(v_get(com_pid), kill),
			    req_start(
			      StopCom, default_hard_kill_timeout(), From),
			    ?STATE({StopCom,kill});
			kill ->
			    ?ERROR(
			       {com_kill,timeout}, State,
			       "COM kill timeout - exiting!"),
			    ?STOP(shutdown)
			end;
		_ ->
		    statem_common(State, Event)
	    end;
	%% API Calls
	{call,{Action,_},_}
	  when Action =:= start_com;
	       Action =:= stop_com;
	       Action =:= stop_com_if_started;
	       Action =:= kill_com;
	       Action =:= healthCheck ->
	    ?POSTPONE;
	{call,{setHaMode,_,_,_}} ->
	    ?POSTPONE;
	_ ->
	    statem_common(State, Event)
    end;

statem(State, Event) ->
    statem_common(State, Event).

%% Event handling common to all states
%%
%% API Calls
statem_common(State, {timeout,_,_}) ->
    %% Simply ignore late timers; they can not be avoided
    ?STATE(State);
statem_common(State, {call,get_pid,From}) ->
    reply(v_get(com_pid, undefined), From),
    ?STATE(State);
statem_common(State, {call,get_comte_com_pid,From}) -> % Debug call
    reply(self(), From),
    ?STATE(State);
%% Events
statem_common(State, {info,{Port,{data,{_Eol,_Data}}}} = Event)
  when is_port(Port) ->
    case v_get(port, undefined) of
	Port ->
	    ?WARNING(Event, State, "Unexpected port data"),
	    ?STATE(State);
	_ ->
	    ?DISCARD
    end;
statem_common(State, {info,{Port,{exit_status,Status}}})
  when is_port(Port) ->
    case v_get(port, undefined) of
	Port ->
	    v_insert(exit_status, Status),
	    ?STATE(State);
	_ ->
	    ?DISCARD
    end;
statem_common(State, {'EXIT',Port,Reason})
  when is_port(Port) ->
    case v_get(port, undefined) of
	Port ->
	    %% Assume error exit from port since we got no exit_status
	    statem_com_shutdown(State, v_delete(exit_status, 127), Reason);
	_ ->
	    ?DISCARD
    end;
%%
statem_common(_, _) ->
    ?DISCARD.



statem_com_shutdown(State, Status, Reason) ->
    _ = v_delete(port),
    ComPid = v_delete(com_pid),
    case State of
	{StopCom,_}
	  when StopCom =:= stop_com;
	       StopCom =:= kill_com ->
	    req_reply(ok),
	    com_shutdown_log_error(State, Status, Reason, ComPid),
	    ?STATE({init});
	_ ->
	    ?ERROR(
	       {exit_status,Status,Reason}, State,
	       "Unexpected COM[~p] exit: ~s - exiting!",
	       [ComPid,code_to_string(Status)]),
	    ?STOP(shutdown)
    end.

statem_com_cast_initial_setHaMode(Event, State) ->
    HaMode =
        case env_val(com_initial_ha_mode, ?ComMwSpiHaModeActive) of
            Mode when Mode == ?ComMwSpiHaModeActive;
                      Mode == ?ComMwSpiHaModeStandby;
                      Mode == ?ComMwSpiHaModeUnassigned ->
                Mode;
            {M,F,A} ->
                apply(M, F, A)
        end,
    HaReason = ?ComMwSpiHaReasonInit,
    statem_com_cast(Event, setHaMode, [HaMode,HaReason], State).

statem_com_cast(Event, F, A, State) ->
    case bert_cast(F, A) of
	ok ->
	    ?STATE(State);
	{error, _Reason} ->
	    kill_com_brutally_log(Event, v_get(com_pid), State),
	    ?STOP(shutdown)
    end.



%%%===================================================================
%%% State Machine Helpers
%%%===================================================================

com_shutdown_log_error(State, 0, normal, ComPid) ->
    ?INFO(
       {exit_status,0,normal}, State,
       "COM[~p] exited", [ComPid]);
com_shutdown_log_error(State, Status, Reason, ComPid) ->
    ?WARNING(
       {exit_status,Status,Reason}, State,
       "COM[~p] error exit: ~s", [ComPid,code_to_string(Status)]),
    ok.

kill_com_brutally_log(Event, ComPid, State) ->
    kill_com_proc(ComPid, kill),
    ?INFO(Event, State, "Killed COM brutally!").

healthCheckReport_reply(HealthReport, RecommendedResponse) ->
    case HealthReport of
        ?ComOk -> ?ComOk;
        _Else -> {HealthReport,RecommendedResponse}
    end.


%%%===================================================================
%%% Internal COM Control Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Starting the COM process
%%
%%--------------------------------------------------------------------
start_com_prog() ->
     ComStartProg =
        filename:join(comte:template_dir(), env_val(start_com_prog)),
    Timeout = env_val(start_com_timeout),
    start_com_prog(ComStartProg, fix_port_start_timeout(Timeout)).

start_com_prog(ComStartProg, PortTimeout) ->
    try
	open_port(
	  {spawn_executable,start_com_wrapper()},
	  [{args,[ComStartProg]},exit_status,{line,80}])
    of
	Port ->
	    receive
		{Port, {data, {eol, PidStr}}} ->
		    try list_to_integer(PidStr) of
			Pid ->
			    {Port, Pid}
		    catch error:badarg ->
			    port_close(Port),
			    {error, {could_not_get_pid, PidStr}}
		    end
	    after PortTimeout ->
		    port_close(Port),
		    {error, timeout}
	    end
    catch error:Reason ->
	    {error, Reason}
    end.

start_com_wrapper() ->
    filename:join(comte:template_dir(), "comte_start_com_wrapper").

%%--------------------------------------------------------------------
%% @private
%% @doc Killing the COM process
%%
%%--------------------------------------------------------------------
kill_com_proc(LinuxPid) ->
    kill_com_proc(LinuxPid, term).

kill_com_proc(LinuxPid, Signal) when is_integer(LinuxPid) ->
    PidStr = integer_to_list(LinuxPid),
    SigStr = integer_to_list(sig_no(Signal)),
    os:cmd("kill -"++SigStr++" "++PidStr).

%%sig_no(Nr) when is_integer(Nr) ->  Nr;
sig_no(term) -> 15;
sig_no(kill) -> 9.

%%--------------------------------------------------------------------
%% @private
%% @doc Helper function for sending operations to bert server
%%--------------------------------------------------------------------
bert_cast(F, A) ->
    comte_bert_server:cast_to_com(undefined, F, A).


%%--------------------------------------------------------------------
%% @private
%% @doc Application environment helper function
%%--------------------------------------------------------------------
env_val(Par) ->
    {ok,Val} = comte:get_env(Par),
    Val.

env_val(Par, Default) ->
    case comte:get_env(Par) of
	{ok,Val} ->
	    Val;
	undefined ->
	    Default
    end.

%%--------------------------------------------------------------------
%% Server helpers
%%--------------------------------------------------------------------

%% Translations of port job exit codes
%% In reality (X band 128) marks killed by signal and
%% (X band 127) contains signal value according to "kill -l".

code_to_string(134) ->
    "SIGABRT(134): The job was killed with an abort signal "
	"and you probably got core dumped";
code_to_string(137) ->
    "SIGKILL(137): The job was brutally killed";
code_to_string(139) ->
    "SIGSEGV(139): The job caused a segmentation fault";
code_to_string(Other) ->
    integer_to_list(Other).


%% interval_healthCheck timer handling

healthcheck_interval_start() ->
    Tref =
	case env_val(healthcheck_interval) of
	    infinity ->
		make_ref();
	    Timeout when is_integer(Timeout) ->
		erlang:start_timer(
		  fix_timeout(Timeout), self(), healthcheck_interval)
	end,
    v_insert(healthcheck_interval, Tref),
    ok.

healthcheck_interval_cancel() ->
    Tref = v_delete(healthcheck_interval),
    cancel_timer(Tref).

healthcheck_interval_timeout(Tref) -> % After timeout
    Tref = v_delete(healthcheck_interval),
    ok.


%% Request (event to be responded to) handling

req_start(Req, Timeout, From) ->
    Tref = erlang:start_timer(fix_timeout(Timeout), self(), Req),
    v_insert(req, {Tref,From}),
    ok.

req_reply(Reply) ->
    {Tref,From} = v_delete(req),
    cancel_timer(Tref),
    reply(Reply, From),
    ok.

req_cancel() ->
    {Tref,_} = v_delete(req),
    cancel_timer(Tref).

req_timeout(Tref) -> % After timeout
    {Tref,_} = v_delete(req),
    ok.


%%% State variable access functions
%%%
%%% A variable name may be any term and is wrapped in a
%%% {?MODULE,VarName} term to not collide with other modules

%% Insert a variable that is supposed to not be defined
v_insert(Var, Value) ->
    var(Var),
    undefined = put({?MODULE,Var}, {Value}),
    ok.

%% Get the value of a variable that is supposed to be defined
v_get(Var) ->
    var(Var),
    {Value} = get({?MODULE,Var}),
    Value.

%% Get the value of a variable that may or may not be defined
v_get(Var, Default) ->
    var(Var),
    case get({?MODULE,Var}) of
	undefined ->
	    Default;
	{Value} ->
	    Value
    end.

%% Is the variable defined?
v_is_defined(Var) ->
    var(Var),
    case get({?MODULE,Var}) of
	undefined ->
	    false;
	{_} ->
	    true
    end.

%% Delete a variable that is supposed to be defined and return its value
v_delete(Var) ->
    var(Var),
    {Value} = erase({?MODULE,Var}),
    Value.

%% Delete a variable that may or may not be defined
v_delete(Var, Default) ->
    var(Var),
    case erase({?MODULE,Var}) of
	{Value} ->
	    Value;
	undefined ->
	    Default
    end.

%% OTP prettyfications

gen_call(Request) ->
    gen_server:call(name(), Request, infinity).

reply(Reply, From) ->
    gen_server:reply(From, Reply).

init_ack() ->
    proc_lib:init_ack({ok,self()}).

cancel_timer(Tref) ->
    _ = erlang:cancel_timer(Tref),
    ok.

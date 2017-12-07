%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/1      2014-02-07 etxpeno     Created
%%% ----------------------------------------------------------
-module(eqs_mmi_service).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R8A/1').
-date('2016-12-28').
-author('uabesvi').

%% API
-export([start/0,
         start/1,
         start_link/0,
         start_link/1,
         stop/0]).

-export([activate/0]).

-export([set_led_behavior/2]).
-export([reset_led_behavior/1]).
-export([get_led_behavior/0, get_led_behavior/1]).

-export([get_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(SET_LED_STATE, 1).
-define(GET_LED_STATE, 2).

-define(RESET_RESTART_TIMER, 180000).

%%% rhai-mmi.h Start

%%% Interface errors
-define(RHAI_MMIERR_SUCCESS, 0). % no error
-define(RHAI_MMIERR_INVAL,  -1). % invalid parameter(s)
-define(RHAI_MMIERR_NOMEM,  -2). % no memory
-define(RHAI_MMIERR_IO,     -3). % no memory

%%% These defines specify the state/behavior of the
%% Light Emitting Diodes.
-define(RHAI_MMI_LED_OFF,              1).  % No light
-define(RHAI_MMI_LED_ON,               2).  % Steady light
-define(RHAI_MMI_LED_SLOW_BLINK,       3).  % Blinking 0.5 Hz
-define(RHAI_MMI_LED_FAST_BLINK,       4).  % Blinking 16 Hz
-define(RHAI_MMI_LED_DOUBLE_FLASH_OFF, 5).  % Double flash overlaying 'off'
-define(RHAI_MMI_LED_DOUBLE_FLASH_ON,  6).  % Double flash overlaying 'on'

%%% These defines specify the type (which LED) of the Light Emitting Diodes.
-define(RHAI_MMI_LED_OPERATIONAL, 1). % Green LED
-define(RHAI_MMI_LED_FAULT,       2). % Red LED
-define(RHAI_MMI_LED_STATUS,      3). % Yellow LED
-define(RHAI_MMI_LED_MAINTENANCE, 4). % Blue LED

-record(state,
	{
          clients = [],

	  fault_req       = [],
	  operational_req = [],
          status_req      = [],
          maintenance_req = [],

	  port,

	  restarted = false
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

set_led_behavior(Indicator, LedBehavior) ->
    call({set_led_behavior, Indicator, LedBehavior, self()}).

reset_led_behavior(Indicator) ->
    call({set_led_behavior, Indicator, undefined, self()}).

get_led_behavior() ->
    call(get_led_behavior).

get_led_behavior(Indicator) ->
    call({get_led_behavior, Indicator}).

get_info() ->
    call(get_info).

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

    Port = start_port(),

    State = #state{port = Port},

    log_led_status(State),

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
handle_call({set_led_behavior, Indicator, LedBehavior, Client}, _From, State) ->
    {Reply, NewState} =
	handle_set_led_behavior(Indicator, LedBehavior, Client, State),
    Clients = State#state.clients,
    NewClients =
	case lists:keyfind(Client, 1, Clients) of
	    false when LedBehavior =:= undefined ->
		Clients;
	    false ->
		MonitorRef = monitor(process, Client),
		[{Client, MonitorRef}|Clients];
	    {Client, MonitorRef} when LedBehavior =:= undefined ->
		demonitor(MonitorRef),
		Clients -- [{Client, MonitorRef}];
	    {Client, _} ->
		Clients
	end,

    {reply, Reply, NewState#state{clients = NewClients}};

handle_call(get_led_behavior, _From, State) ->
    Reply = [{Indicator, handle_get_led_behavior(State#state.port, Indicator)} ||
		Indicator <- [fault, operational, status, maintenance]],
    {reply, Reply, State};

handle_call({get_led_behavior, Indicator}, _From, State) ->
    Reply = handle_get_led_behavior(State#state.port, Indicator),
    {reply, Reply, State};

handle_call(get_info, _From, State) ->
    Reply = [
	     {clients,    State#state.clients},

	     {operational_req, State#state.operational_req},
	     {fault_req,       State#state.fault_req},
	     {maintenance_req, State#state.maintenance_req},
	     {status_req,      State#state.status_req},

	     {operational_led, handle_get_led_behavior(State#state.port,
						       operational)},
	     {fault_led,       handle_get_led_behavior(State#state.port,
						       fault)},
	     {maintenance_led, handle_get_led_behavior(State#state.port,
						       maintenance)},
	     {status_led,      handle_get_led_behavior(State#state.port,
						       status)},

	     {port,      State#state.port},
	     {restarted, State#state.restarted}
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
handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(activate, S) ->
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
handle_info({timeout, _, reset_restart_timer}, State) ->
    {noreply, State#state{restarted = false}};
handle_info({'DOWN', _MonitorRef, process, Client, _Info}, State) ->
    {noreply, remove_client(Client, State)};
handle_info({'EXIT', Port, Reason}, #state{port      = Port,
					   restarted = false} = State) ->
    sysInitI:info_msg("~p: Port program has terminated~n"
		      "Reason: ~p~n"
		      "Action: Restart port program~n", [?MODULE, Reason]),
    NewState = State#state{port      = start_port(),
			   restarted = true},
    set_leds(NewState),
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

handle_set_led_behavior(operational, LedBehavior, Client,
			State) when LedBehavior =:= undefined;
				    LedBehavior =:= off;
				    LedBehavior =:= steady_on;
				    LedBehavior =:= slow_blink;
				    LedBehavior =:= fast_blink;
				    LedBehavior =:= double_flash_off;
				    LedBehavior =:= double_flash_on ->
    OperationalReq =
	case LedBehavior of
	    undefined ->
		lists:keydelete(Client, 1, State#state.operational_req);
	    _ ->
		lists:keystore(Client, 1, State#state.operational_req,
			       {Client, LedBehavior})
	end,
    Result = set_led(State#state.port, operational,
		     calc_led_behavior(operational, OperationalReq)),
    NewState = State#state{operational_req = OperationalReq},
    {Result, NewState};
handle_set_led_behavior(operational, _LedBehavior, _Client, State) ->
    Result = {error, unknown_led_behavior},
    {Result, State};
handle_set_led_behavior(fault, LedBehavior, Client,
			State) when LedBehavior =:= undefined;
				    LedBehavior =:= off;
				    LedBehavior =:= steady_on ->
    FaultReq =
	case LedBehavior of
	    undefined ->
		lists:keydelete(Client, 1, State#state.fault_req);
	    _ ->
		lists:keystore(Client, 1, State#state.fault_req,
			       {Client, LedBehavior})
	end,
    Result =
	set_led(State#state.port, fault, calc_led_behavior(fault, FaultReq)),
    {Result, State#state{fault_req = FaultReq}};
handle_set_led_behavior(fault, _LedBehavior, _Client, State) ->
    Result = {error, unknown_led_behavior},
    {Result, State};
handle_set_led_behavior(maintenance, LedBehavior, Client,
			State) when LedBehavior =:= undefined;
				    LedBehavior =:= off;
				    LedBehavior =:= steady_on;
				    LedBehavior =:= slow_blink;
				    LedBehavior =:= fast_blink ->
    MaintenanceReq =
	case LedBehavior of
	    undefined ->
		lists:keydelete(Client, 1, State#state.maintenance_req);
	    _ ->
		lists:keystore(Client, 1, State#state.maintenance_req,
			       {Client, LedBehavior})
	end,
    Result =
	set_led(State#state.port, maintenance,
		calc_led_behavior(maintenance, MaintenanceReq)),
    {Result, State#state{maintenance_req = MaintenanceReq}};
handle_set_led_behavior(maintenance, _LedBehavior, _Client, State) ->
    Result = {error, unknown_led_behavior},
    {Result, State};
handle_set_led_behavior(status, LedBehavior, Client,
			State) when LedBehavior =:= undefined;
				    LedBehavior =:= off;
				    LedBehavior =:= steady_on;
				    LedBehavior =:= slow_blink ->
    StatusReq =
	case LedBehavior of
	    undefined ->
		lists:keydelete(Client, 1, State#state.status_req);
	    _ ->
		lists:keystore(Client, 1, State#state.status_req,
			       {Client, LedBehavior})
	end,
    Result =
	set_led(State#state.port, status, calc_led_behavior(status, StatusReq)),
    {Result, State#state{status_req = StatusReq}};
handle_set_led_behavior(status, _LedBehavior, _Client, State) ->
    Result = {error, unknown_led_behavior},
    {Result, State};
handle_set_led_behavior(_Indicator, _LedBehavior, _Client, State) ->
    Result = {error, unknown_indicator},
    {Result, State}.

handle_get_led_behavior(Port, Indicator) ->
    case get_led(Port, Indicator) of
	{ok, ?RHAI_MMI_LED_OFF} ->
	    off;
	{ok, ?RHAI_MMI_LED_ON} ->
	    steady_on;
	{ok, ?RHAI_MMI_LED_SLOW_BLINK} ->
	    slow_blink;
	{ok, ?RHAI_MMI_LED_FAST_BLINK} ->
	    fast_blink;
	{ok, ?RHAI_MMI_LED_DOUBLE_FLASH_OFF} ->
	    double_flash_off;
	{ok, ?RHAI_MMI_LED_DOUBLE_FLASH_ON} ->
	    double_flash_on;
	{ok, Value} ->
	    sysInitI:info_msg("~p:get_led returned unknown value ~p~n",
			      [?MODULE, Value]),
	    {error, unknown_value};
	Other ->
	    sysInitI:info_msg("~p:get_led returned ~p~n", [?MODULE, Other]),
	    Other
    end.

remove_client(Client, State) ->
    Clients = lists:keydelete(Client, 1, State#state.clients),

    OperationalReq = lists:keydelete(Client, 1, State#state.operational_req),
    FaultReq       = lists:keydelete(Client, 1, State#state.fault_req),
    StatusReq      = lists:keydelete(Client, 1, State#state.status_req),
    MaintenanceReq = lists:keydelete(Client, 1, State#state.maintenance_req),

    NewState = State#state{clients         = Clients,

			   operational_req = OperationalReq,
			   fault_req       = FaultReq,
			   maintenance_req = MaintenanceReq,
			   status_req      = StatusReq},

    set_leds(NewState),

    NewState.

start_port() ->
    AppName = "eqs_ledctl",
    PrivDir = code:priv_dir(eqs),
    BinDir = sysEnv:target_bin_dir(),
    Prog = filename:join([PrivDir, BinDir, AppName]),
    ExtProg = swmI:find_file(Prog),

    open_port({spawn_executable, ExtProg}, [{packet, 2}, binary]).

set_leds(State) ->
    set_led(State#state.port, operational,
	    calc_led_behavior(operational, State#state.operational_req)),
    set_led(State#state.port, fault,
	    calc_led_behavior(fault, State#state.fault_req)),
    set_led(State#state.port, maintenance,
	    calc_led_behavior(maintenance, State#state.maintenance_req)),
    set_led(State#state.port, status,
	    calc_led_behavior(status, State#state.status_req)).

set_led(Port, Indicator, LedBehavior) ->
    case handle_get_led_behavior(Port, Indicator) of
	LedBehavior ->
	    %% No change of LedBehavior
	    ok;
	_ ->
	    Data = get_set_data(Indicator, LedBehavior),
	    port_command(Port, Data),

	    Result =
		receive
		    {Port, {data, Answer}} ->
			decode_set_answer(Answer)
		end,

	    Msg = lists:flatten(io_lib:format("Set ~p to ~p Result: ~p",
					      [Indicator, LedBehavior, Result])),

	    write_log("MMILog", "MMI", info, Msg),

	    sysInitI:info_msg("~p: Set ~p indicator to ~p~n"
			      "Result: ~p~n",
			      [?MODULE, Indicator, LedBehavior, Result]),
	    Result
    end.

get_led(Port, Indicator) when Indicator =:= fault;
			      Indicator =:= operational;
			      Indicator =:= status;
			      Indicator =:= maintenance ->
    Data = get_get_data(Indicator),
    port_command(Port, Data),

    receive
	{Port, {data, Answer}} ->
	    decode_get_answer(Answer)
    end;
get_led(_Port, _Indicator) ->
    {error, unknown_indicator}.

get_set_data(Indicator, LedBehavior) ->
    [?SET_LED_STATE, get_set_data(Indicator), get_set_data(LedBehavior)].

get_get_data(Indicator) ->
    [?GET_LED_STATE, get_set_data(Indicator)].

get_set_data(fault) ->
    <<?RHAI_MMI_LED_FAULT:4/native-signed-integer-unit:8>>;
get_set_data(operational) ->
    <<?RHAI_MMI_LED_OPERATIONAL:4/native-signed-integer-unit:8>>;
get_set_data(status) ->
    <<?RHAI_MMI_LED_STATUS:4/native-signed-integer-unit:8>>;
get_set_data(maintenance) ->
    <<?RHAI_MMI_LED_MAINTENANCE:4/native-signed-integer-unit:8>>;

get_set_data(off) ->
    <<?RHAI_MMI_LED_OFF:4/native-signed-integer-unit:8>>;
get_set_data(steady_on) ->
    <<?RHAI_MMI_LED_ON:4/native-signed-integer-unit:8>>;
get_set_data(slow_blink) ->
    <<?RHAI_MMI_LED_SLOW_BLINK:4/native-signed-integer-unit:8>>;
get_set_data(fast_blink) ->
    <<?RHAI_MMI_LED_FAST_BLINK:4/native-signed-integer-unit:8>>;
get_set_data(double_flash_off) ->
    <<?RHAI_MMI_LED_DOUBLE_FLASH_OFF:4/native-signed-integer-unit:8>>;
get_set_data(double_flash_on) ->
    <<?RHAI_MMI_LED_DOUBLE_FLASH_ON:4/native-signed-integer-unit:8>>.

decode_set_answer(<<?RHAI_MMIERR_SUCCESS:4/native-signed-integer-unit:8>>) ->
    ok;
decode_set_answer(<<?RHAI_MMIERR_INVAL:4/native-signed-integer-unit:8>>) ->
    {error, invalid_parameter};
decode_set_answer(<<?RHAI_MMIERR_NOMEM:4/native-signed-integer-unit:8>>) ->
    {error, no_memory};
decode_set_answer(<<?RHAI_MMIERR_IO:4/native-signed-integer-unit:8>>) ->
    {error, no_memory};
decode_set_answer(<<RC:4/native-signed-integer-unit:8>>) ->
    {error, RC}.

decode_get_answer(<<?RHAI_MMIERR_SUCCESS:4/native-signed-integer-unit:8,
		    LedBehavior:4/native-signed-integer-unit:8>>) ->
    {ok, LedBehavior};
decode_get_answer(<<?RHAI_MMIERR_INVAL:4/native-signed-integer-unit:8,
		    _:4/native-signed-integer-unit:8>>) ->
    {error, invalid_parameter};
decode_get_answer(<<?RHAI_MMIERR_NOMEM:4/native-signed-integer-unit:8,
		    _:4/native-signed-integer-unit:8>>) ->
    {error, no_memory};
decode_get_answer(<<?RHAI_MMIERR_IO:4/native-signed-integer-unit:8,
		    _:4/native-signed-integer-unit:8>>) ->
    {error, no_memory};
decode_get_answer(<<RC:4/native-signed-integer-unit:8,
		    _:4/native-signed-integer-unit:8>>) ->
    {error, RC}.

calc_led_behavior(operational, []) ->
    steady_on;
calc_led_behavior(_Indicator, []) ->
    off;
calc_led_behavior(Indicator, [{_, LedBehavior}|R]) ->
    calc_led_behavior(Indicator, R, LedBehavior).

calc_led_behavior(_, [], CalcLedBehavior) ->
    CalcLedBehavior;
calc_led_behavior(Indicator, [{_, LedBehavior}|R], CalcLedBehavior) ->
    CalcLedBehaviorPrio = prio(Indicator, CalcLedBehavior),
    LedBehaviorPrio = prio(Indicator, LedBehavior),

    if
	LedBehaviorPrio > CalcLedBehaviorPrio ->
	    calc_led_behavior(Indicator, R, LedBehavior);
	true ->
	    calc_led_behavior(Indicator, R, CalcLedBehavior)
    end.

%% These priorities are fetched from document 1/102 60-CSH 109 152 Uen Rev G
%% Table 10
prio(fault, off)       -> 1;
prio(fault, steady_on) -> 2;

prio(operational, off)              -> 6;
prio(operational, slow_blink)       -> 2;
prio(operational, fast_blink)       -> 3;
prio(operational, steady_on)        -> 1;
prio(operational, double_flash_on)  -> 4;
prio(operational, double_flash_off) -> 5;

prio(status, off)        -> 1;
prio(status, slow_blink) -> 3;
prio(status, steady_on)  -> 2;

prio(maintenance, off)        -> 4;
prio(maintenance, slow_blink) -> 2;
prio(maintenance, fast_blink) -> 3;
prio(maintenance, steady_on)  -> 1.

call(Msg) ->
    gen_server:call(?SERVER, Msg, 60000).

log_led_status(State) ->
    Port = State#state.port,
    lists:foreach(
      fun(Indicator) ->
	      LedBehavior = handle_get_led_behavior(Port, Indicator),
	      Msg = atom_to_list(Indicator) ++ " is " ++ led_behavior(LedBehavior),
	      write_log("MMILog", "MMI", info, Msg)
      end, [fault, operational, status, maintenance]),
    ok.

led_behavior(LedBehavior) when is_atom(LedBehavior) ->
    atom_to_list(LedBehavior);
led_behavior({error, _Reason}) ->
    "unknown".



write_log(Name, User, Severity, Msg) ->
    sysEnv:rcs_mode_2() /= vrcs andalso
	logI:write_log(Name, User, Severity, Msg).


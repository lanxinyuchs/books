%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	eqs_vii_service.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R10A/1

%%% @doc == Visual indication server ==
%%% @end
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-10-03 etxbjca     Created
%%% R1A/16     2014-05-05 etxberb     Added coli/1.
%%% R3A/1      2015-02-06 erarafo     Comments only
%%% R3A/2      2015-03-27 etxpeno     MMI logging
%%% R3A/3      2015-03-27 etxpeno     MMI logging extended
%%% R3A/4      2015-03-29 erarafo     Restart trace added
%%% R3A/4      2015-09-08 etxarnu     catched 'CelloVii...
%%% R4A/4      2015-10-08 etxpeno     Added cec_takeover
%%% R5A/1      2016-02-19 etxpeno     HU52899 'CelloVii_visualIndRequest' and
%%%                                   corresponding C-function is now asynchronous
%%% R10A/1     2017-06-14 etxpeno     Correct usage of cec:get_program_name/1
%%% ----------------------------------------------------------
-module(eqs_vii_service).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R10A/1').
-date('2017-06-14').
-author('etxpeno').

-export([start/0,
	 start/1,
	 start_link/0,
	 start_link/1,
	 stop/0]).

-export([activate/0]).

-export([cec_setup/1, cec_takeover/1]).

-export(['CelloVii_visualIndRequest'/1]).

-export([get_info/0,
	 get_ind_state/0,
	 get_ind_state/1]).

-export([coli/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("eqs_vii.hrl").

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000). % 10 seconds

-define(VISUAL_IND_REQ, 0).
-define(VISUAL_IND_GET, 1).

-type fault_state() :: fault | no_fault.
-type operational_state() :: no_power | backup | boottest | loadtest |
			     missing_resource | power.
-type status_state() :: remote_unit | node | default.
-type maintenance_state() :: medium | short | locked | shutdown |
			     alarm_suppress | unlocked.

-record(state,
	{
	  sockets = [],
	  clients = [],

	  fault = ordsets:new(),

	  no_power = ordsets:new(),
	  backup = ordsets:new(),
	  boottest = ordsets:new(),
	  loadtest = ordsets:new(),
	  missing_resource = ordsets:new(),

	  remote_unit = ordsets:new(),
	  node = ordsets:new(),

	  medium = ordsets:new(),
	  short = ordsets:new(),
	  locked = ordsets:new(),
	  shutdown = ordsets:new(),
	  alarm_suppress = ordsets:new(),

	  fault_state :: fault_state(),
	  operational_state :: operational_state(),
	  status_state :: status_state(),
	  maintenance_state :: maintenance_state()
	}).

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
    gen_server:call(?SERVER, {cec_setup, Socket}, ?TIMEOUT).

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

'CelloVii_visualIndRequest'(Ind) when Ind == ?CELLO_VII_FAULT;
				      Ind == ?CELLO_VII_NO_FAULT;
				      Ind == ?CELLO_VII_NO_POWER;
				      Ind == ?CELLO_VII_POWER;
				      Ind == ?CELLO_VII_BACKUP_START;
				      Ind == ?CELLO_VII_BACKUP_END;
				      Ind == ?CELLO_VII_BOOTTEST_START;
				      Ind == ?CELLO_VII_BOOTTEST_END;
				      Ind == ?CELLO_VII_LOADTEST_START;
				      Ind == ?CELLO_VII_LOADTEST_END;
				      Ind == ?CELLO_VII_MISSING_RESOURCE_START;
				      Ind == ?CELLO_VII_MISSING_RESOURCE_END;
				      Ind == ?CELLO_VII_REMOTE_UNIT_FAULT_START;
				      Ind == ?CELLO_VII_REMOTE_UNIT_FAULT_END;
				      Ind == ?CELLO_VII_NODE_FAULT_START;
				      Ind == ?CELLO_VII_NODE_FAULT_END;
				      Ind == ?CELLO_VII_MEDIUM_BUTTON_PRESS_START;
				      Ind == ?CELLO_VII_MEDIUM_BUTTON_PRESS_END;
				      Ind == ?CELLO_VII_SHORT_BUTTON_PRESS_START;
				      Ind == ?CELLO_VII_SHORT_BUTTON_PRESS_END;
				      Ind == ?CELLO_VII_BOARD_LOCKED;
				      Ind == ?CELLO_VII_BOARD_UNLOCKED;
				      Ind == ?CELLO_VII_SHUTDOWN_START;
				      Ind == ?CELLO_VII_SHUTDOWN_END;
				      Ind == ?CELLO_VII_ALARM_SUPPRESS_START;
				      Ind == ?CELLO_VII_ALARM_SUPPRESS_END ->
    try
	gen_server:cast(?SERVER, {'CelloVii_visualIndRequest', Ind, self()})
    catch
	_:Reason ->
	    sysInitI:error_msg("~p:'CelloVii_visualIndRequest' error~n"
			       "Reason: ~p~n",
			       [?MODULE, Reason]),
	    ?CELLO_VII_FAILED
    end;
'CelloVii_visualIndRequest'(Ind) ->
    sysInitI:error_msg("~p:'CelloVii_visualIndRequest' error~n"
		       "Indication: ~p~n", [?MODULE, Ind]),
    ?CELLO_VII_FAILED.

get_info() ->
    gen_server:call(?SERVER, get_info, ?TIMEOUT).

get_ind_state() ->
    %% Deprecated
    eqs_mmi_service:get_led_behavior().

get_ind_state(Indicator) ->
    %% Deprecated
    eqs_mmi:get_led_behavior(Indicator).

coli(_Args) ->
    io:format("----- VII SERVER INFO -----~n", []),
    io:format("registered clients:~n", []),
    print_clients(get_info()),
    io:format("~n", []),
    print_leds(eqs_mmi_service:get_led_behavior()).


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
    NewState = #state{fault_state       = no_fault,
		      operational_state = power,
		      status_state      = default,
		      maintenance_state = unlocked},
    set_leds(NewState),
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "started"),
    {ok, NewState}.

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

handle_call({cec_setup, Socket}, _From, #state{sockets = Sockets} = S) ->
    Reply = self(),

    {reply, Reply, S#state{sockets = [Socket | Sockets]}};

handle_call(get_info, _From, S) ->
    Reply = [{sockets,           S#state.sockets},
	     {clients,           S#state.clients},
	     {fault,             S#state.fault},
	     {no_power,          S#state.no_power},
	     {backup,            S#state.backup},
	     {boottest,          S#state.boottest},
	     {loadtest,          S#state.loadtest},
	     {missing_resource,  S#state.missing_resource},
	     {remote_unit,       S#state.remote_unit},
	     {node,              S#state.node},
	     {medium,            S#state.medium},
	     {short,             S#state.short},
	     {locked,            S#state.locked},
	     {shutdown,          S#state.shutdown},
	     {alarm_suppress,    S#state.alarm_suppress},
	     {fault_state,       S#state.fault_state},
	     {operational_state, S#state.operational_state},
	     {status_state,      S#state.status_state},
	     {maintenance_state, S#state.maintenance_state}
	    ],
    {reply, Reply, S};

handle_call(Command, _From, S) ->
    {reply, Command, S}.

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
handle_cast({'CelloVii_visualIndRequest', Indication, Client}, S) ->
    Clients = S#state.clients,
    NewClients =
	case lists:member(Client, Clients) of
	    true ->
		Clients;
	    false ->
		_MonitorRef = monitor(process, Client),
		[Client | Clients]
	end,
    Msg = "CelloVii_visualIndRequest: " ++ format_indication(Indication) ++
	" from " ++ format_client(Client),
    write_log("MMILog", "VII", info, Msg),
    NewState = update_state(Client, Indication, S),
    set_leds(NewState, S),
    {noreply, NewState#state{clients = NewClients}};

handle_cast({cec_takeover, Socket}, S) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, S};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(activate, S) ->
    {noreply, S};

handle_cast(_Msg, S) ->
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
handle_info({tcp, Socket, <<ClientPid:4/native-unsigned-integer-unit:8,
			    ?VISUAL_IND_REQ:4/native-unsigned-integer-unit:8,
			    Indication:4/native-signed-integer-unit:8>>},
	    S) ->
    Msg = "CelloVii_visualIndRequest: " ++ format_indication(Indication) ++
	" from " ++ get_program_name(ClientPid),
    write_log("MMILog", "VII", info, Msg),
    NewState = update_state(Socket, Indication, S),
    set_leds(NewState, S),

    inet:setopts(Socket, [{active, once}]),

    {noreply, NewState};

handle_info({tcp, Socket, <<_ClientPid:4/native-unsigned-integer-unit:8,
			    ?VISUAL_IND_GET:4/native-unsigned-integer-unit:8,
			    LedType:4/native-signed-integer-unit:8>>},
	    S) ->
    Indicator = get_indicator(LedType),
    {Result, LedState} = get_indicator_state(Indicator),

    gen_tcp:send(Socket, <<Result:4/native-signed-integer-unit:8,
			   LedState:4/native-signed-integer-unit:8>>),
    inet:setopts(Socket, [{active, once}]),

    %%  The decodeXxx functions are at the end of this file.
    %%
    %%  error_logger:info_msg(
    %%    "LED query result: ~s, ~s~n",
    %%    [decodeResult(Result), decodeState(LedState)]),

    {noreply, S};

handle_info({tcp_closed, Socket}, S) ->
    NewState = remove_socket(Socket, S),
    set_leds(NewState, S),
    {noreply, NewState};

handle_info({'DOWN', _MonitorRef, process, Client, _Info}, S) ->
    NewState = remove_client(Client, S),
    set_leds(NewState, S),
    {noreply, NewState};

handle_info(_Info, S) ->
    {noreply, S}.

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
    [gen_tcp:close(Socket) || Socket <- S#state.sockets],
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "stopped"),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calc_fault_state(#state{fault = P}) when P =/= []->
    fault;
calc_fault_state(_) ->
    no_fault.

calc_operational_state(#state{no_power = P}) when P =/= [] ->
    no_power;
calc_operational_state(#state{backup = P}) when P =/= [] ->
    backup;
calc_operational_state(#state{boottest = P}) when P =/= [] ->
    boottest;
calc_operational_state(#state{loadtest = P}) when P =/= [] ->
    loadtest;
calc_operational_state(#state{missing_resource = P}) when P =/= [] ->
    missing_resource;
calc_operational_state(_) ->
    power.

calc_status_state(#state{remote_unit = P}) when P =/= [] ->
    remote_unit;
calc_status_state(#state{node = P}) when P =/= [] ->
    node;
calc_status_state(_) ->
    default.

calc_maintenance_state(#state{medium = P}) when P =/= [] ->
    medium;
calc_maintenance_state(#state{short = P}) when P =/= [] ->
    short;
calc_maintenance_state(#state{locked = P}) when P =/= [] ->
    locked;
calc_maintenance_state(#state{shutdown = P}) when P =/= [] ->
    shutdown;
calc_maintenance_state(#state{alarm_suppress = P}) when P =/= [] ->
    alarm_suppress;
calc_maintenance_state(_) ->
    unlocked.

set_leds(State) ->
    set_led(fault, State#state.fault_state),
    set_led(operational, State#state.operational_state),
    set_led(status, State#state.status_state),
    set_led(maintenance, State#state.maintenance_state).

set_leds(State, OldState) ->
    set_led(fault, State#state.fault_state, OldState#state.fault_state),
    set_led(operational, State#state.operational_state,
	    OldState#state.operational_state),
    set_led(status, State#state.status_state, OldState#state.status_state),
    set_led(maintenance, State#state.maintenance_state,
	    OldState#state.maintenance_state).

set_led(Indicator, IndState) ->
    case calc_led_behavior(Indicator, IndState) of
	undefined ->
	    eqs_mmi_service:reset_led_behavior(Indicator);
	LedBehavior ->
	    eqs_mmi_service:set_led_behavior(Indicator, LedBehavior)
    end.

set_led(_Indicator, IndState, IndState) ->
    ok;
set_led(Indicator, IndState, _) ->
    set_led(Indicator, IndState).


calc_led_behavior(fault, fault)    -> steady_on;
calc_led_behavior(fault, no_fault) -> undefined;

calc_led_behavior(operational, no_power)         -> off;
calc_led_behavior(operational, backup)           -> double_flash_off;
calc_led_behavior(operational, boottest)         -> fast_blink;
calc_led_behavior(operational, loadtest)         -> fast_blink;
calc_led_behavior(operational, missing_resource) -> slow_blink;
calc_led_behavior(operational, power)            -> undefined;

calc_led_behavior(status, remote_unit) -> slow_blink;
calc_led_behavior(status, node)        -> steady_on;
calc_led_behavior(status, default)     -> undefined;

calc_led_behavior(maintenance, medium)         -> slow_blink;
calc_led_behavior(maintenance, short)          -> fast_blink;
calc_led_behavior(maintenance, locked)         -> steady_on;
calc_led_behavior(maintenance, shutdown)       -> slow_blink;
calc_led_behavior(maintenance, alarm_suppress) -> fast_blink;
calc_led_behavior(maintenance, unlocked)       -> undefined.

update_state(Socket, ?CELLO_VII_FAULT, S) ->
    F = S#state.fault,
    NewF = ordsets:add_element(Socket, F),
    S1 = S#state{fault = NewF},
    FaultState = calc_fault_state(S1),
    S1#state{fault_state = FaultState};
update_state(Socket, ?CELLO_VII_NO_FAULT, S) ->
    F = S#state.fault,
    NewF = ordsets:del_element(Socket, F),
    S1 = S#state{fault = NewF},
    FaultState = calc_fault_state(S1),
    S1#state{fault_state = FaultState};
update_state(Socket, ?CELLO_VII_NO_POWER, S) ->
    N = S#state.no_power,
    NewN = ordsets:add_element(Socket, N),
    S1 = S#state{no_power = NewN},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_POWER, S) ->
    N = S#state.no_power,
    NewN = ordsets:del_element(Socket, N),
    S1 = S#state{no_power = NewN},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_BACKUP_START, S) ->
    B = S#state.backup,
    NewB = ordsets:add_element(Socket, B),
    S1 = S#state{backup = NewB},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_BACKUP_END, S) ->
    B = S#state.backup,
    NewB = ordsets:del_element(Socket, B),
    S1 = S#state{backup = NewB},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_BOOTTEST_START, S) ->
    B = S#state.boottest,
    NewB = ordsets:add_element(Socket, B),
    S1 = S#state{boottest = NewB},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_BOOTTEST_END, S) ->
    B = S#state.boottest,
    NewB = ordsets:del_element(Socket, B),
    S1 = S#state{boottest = NewB},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_LOADTEST_START, S) ->
    L = S#state.loadtest,
    NewL = ordsets:add_element(Socket, L),
    S1 = S#state{loadtest = NewL},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_LOADTEST_END, S) ->
    L = S#state.loadtest,
    NewL = ordsets:del_element(Socket, L),
    S1 = S#state{loadtest = NewL},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_MISSING_RESOURCE_START, S) ->
    M = S#state.missing_resource,
    NewM = ordsets:add_element(Socket, M),
    S1 = S#state{missing_resource = NewM},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_MISSING_RESOURCE_END, S) ->
    M = S#state.missing_resource,
    NewM = ordsets:del_element(Socket, M),
    S1 = S#state{missing_resource = NewM},
    OperationalState = calc_operational_state(S1),
    S1#state{operational_state = OperationalState};
update_state(Socket, ?CELLO_VII_REMOTE_UNIT_FAULT_START, S) ->
    R = S#state.remote_unit,
    NewR = ordsets:add_element(Socket, R),
    S1 = S#state{remote_unit = NewR},
    StatusState = calc_status_state(S1),
    S1#state{status_state = StatusState};
update_state(Socket, ?CELLO_VII_REMOTE_UNIT_FAULT_END, S) ->
    R = S#state.remote_unit,
    NewR = ordsets:del_element(Socket, R),
    S1 = S#state{remote_unit = NewR},
    StatusState = calc_status_state(S1),
    S1#state{status_state = StatusState};
update_state(Socket, ?CELLO_VII_NODE_FAULT_START, S) ->
    N = S#state.node,
    NewN = ordsets:add_element(Socket, N),
    S1 = S#state{node = NewN},
    StatusState = calc_status_state(S1),
    S1#state{status_state = StatusState};
update_state(Socket, ?CELLO_VII_NODE_FAULT_END, S) ->
    N = S#state.node,
    NewN = ordsets:del_element(Socket, N),
    S1 = S#state{node = NewN},
    StatusState = calc_status_state(S1),
    S1#state{status_state = StatusState};
update_state(Socket, ?CELLO_VII_MEDIUM_BUTTON_PRESS_START, S) ->
    M = S#state.medium,
    NewM = ordsets:add_element(Socket, M),
    S1 = S#state{medium = NewM},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_MEDIUM_BUTTON_PRESS_END, S) ->
    M = S#state.medium,
    NewM = ordsets:del_element(Socket, M),
    S1 = S#state{medium = NewM},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_SHORT_BUTTON_PRESS_START, S) ->
    Sh = S#state.short,
    NewSh = ordsets:add_element(Socket, Sh),
    S1 = S#state{short = NewSh},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_SHORT_BUTTON_PRESS_END, S) ->
    Sh = S#state.short,
    NewSh = ordsets:del_element(Socket, Sh),
    S1 = S#state{short = NewSh},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_BOARD_LOCKED, S) ->
    L = S#state.locked,
    NewL = ordsets:add_element(Socket, L),
    S1 = S#state{locked = NewL},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_BOARD_UNLOCKED, S) ->
    L = S#state.locked,
    NewL = ordsets:del_element(Socket, L),
    S1 = S#state{locked = NewL},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_SHUTDOWN_START, S) ->
    Sh = S#state.shutdown,
    NewSh = ordsets:add_element(Socket, Sh),
    S1 = S#state{shutdown = NewSh},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_SHUTDOWN_END, S) ->
    Sh = S#state.shutdown,
    NewSh = ordsets:del_element(Socket, Sh),
    S1 = S#state{shutdown = NewSh},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_ALARM_SUPPRESS_START, S) ->
    A = S#state.alarm_suppress,
    NewA = ordsets:add_element(Socket, A),
    S1 = S#state{alarm_suppress = NewA},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};
update_state(Socket, ?CELLO_VII_ALARM_SUPPRESS_END, S) ->
    A = S#state.alarm_suppress,
    NewA = ordsets:del_element(Socket, A),
    S1 = S#state{alarm_suppress = NewA},
    MaintenanceState = calc_maintenance_state(S1),
    S1#state{maintenance_state = MaintenanceState};

update_state(_Socket, _Indication, S) ->
    S.

remove_socket(Socket, S) ->
    S1 = S#state{sockets        = lists:delete(Socket, S#state.sockets),
		 fault          = ordsets:del_element(Socket, S#state.fault),
		 no_power       = ordsets:del_element(Socket, S#state.no_power),
		 backup         = ordsets:del_element(Socket, S#state.backup),
		 boottest       = ordsets:del_element(Socket, S#state.boottest),
		 loadtest       = ordsets:del_element(Socket, S#state.loadtest),
		 missing_resource = ordsets:del_element(Socket,
							S#state.missing_resource),
		 remote_unit    = ordsets:del_element(Socket,
						      S#state.remote_unit),
		 node           = ordsets:del_element(Socket, S#state.node),
		 medium         = ordsets:del_element(Socket, S#state.medium),
		 short          = ordsets:del_element(Socket, S#state.short),
		 locked         = ordsets:del_element(Socket, S#state.locked),
		 shutdown       = ordsets:del_element(Socket, S#state.shutdown),
		 alarm_suppress = ordsets:del_element(Socket,
						      S#state.alarm_suppress)},

    S1#state{fault_state       = calc_fault_state(S1),
	     operational_state = calc_operational_state(S1),
	     status_state      = calc_status_state(S1),
	     maintenance_state = calc_maintenance_state(S1)}.

remove_client(Client, S) ->
    S1 = S#state{clients        = lists:delete(Client, S#state.clients),
		 fault          = ordsets:del_element(Client, S#state.fault),
		 no_power       = ordsets:del_element(Client, S#state.no_power),
		 backup         = ordsets:del_element(Client, S#state.backup),
		 boottest       = ordsets:del_element(Client, S#state.boottest),
		 loadtest       = ordsets:del_element(Client, S#state.loadtest),
		 missing_resource = ordsets:del_element(Client,
							S#state.missing_resource),
		 remote_unit    = ordsets:del_element(Client,
						      S#state.remote_unit),
		 node           = ordsets:del_element(Client, S#state.node),
		 medium         = ordsets:del_element(Client, S#state.medium),
		 short          = ordsets:del_element(Client, S#state.short),
		 locked         = ordsets:del_element(Client, S#state.locked),
		 shutdown       = ordsets:del_element(Client, S#state.shutdown),
		 alarm_suppress = ordsets:del_element(Client,
						      S#state.alarm_suppress)},

    S1#state{fault_state       = calc_fault_state(S1),
	     operational_state = calc_operational_state(S1),
	     status_state      = calc_status_state(S1),
	     maintenance_state = calc_maintenance_state(S1)}.

get_indicator(?CELLO_VII_LED_OPERATIONAL) -> operational;
get_indicator(?CELLO_VII_LED_FAULT)       -> fault;
get_indicator(?CELLO_VII_LED_STATUS)      -> status;
get_indicator(?CELLO_VII_LED_MAINTENANCE) -> maintenance.

get_indicator_state(Indicator) ->
    case eqs_mmi:get_led_behavior(Indicator) of
	off ->
	    {?CELLO_VII_SUCCESS, ?CELLO_VII_LED_OFF};
	steady_on ->
	    {?CELLO_VII_SUCCESS, ?CELLO_VII_LED_ON};
	slow_blink ->
	    {?CELLO_VII_SUCCESS, ?CELLO_VII_LED_SLOW_BLINK};
	fast_blink ->
	    {?CELLO_VII_SUCCESS, ?CELLO_VII_LED_FAST_BLINK};
	double_flash_off ->
	    {?CELLO_VII_SUCCESS, ?CELLO_VII_LED_DOUBLE_FLASH_OFF};
	double_flash_on ->
	    {?CELLO_VII_SUCCESS, ?CELLO_VII_LED_DOUBLE_FLASH_ON};
	_ ->
	    {?CELLO_VII_FAILED, ?CELLO_VII_LED_OFF}
    end.

print_clients(Info) ->
    RegClients = lists:flatten([Value || {Tag, Value} <- Info,
					 Tag == sockets orelse Tag == clients]),
    print_clients(RegClients, Info).

print_clients([Client | Tail], Info) ->
    print_client(Client, [Indication || {Indication, Clients} <- Info,
					Indication /= sockets andalso
					    Indication /= clients andalso
					    is_list(Clients) andalso
					    lists:member(Client, Clients)]),
    print_clients(Tail, Info);
print_clients([], _) ->
    ok.

print_client(Client, Indications) ->
    ClientInfo =
	lists:flatten(format_client(Client) ++
			  " " ++
			  [" " ++ format_indication(Ind) || Ind <- Indications] ++
			  "~n"),
    io:format(ClientInfo, []).

format_client(Client) when is_pid(Client) ->
    case process_info(Client, registered_name) of
	{registered_name, ClientName} ->
	    atom_to_list(ClientName);
	_ ->
	    io_lib:format("~w", [Client])
    end;
format_client(Client) when is_list(Client) ->
    Client;
format_client(Client) when is_integer(Client) ->
    integer_to_list(Client);
format_client(Client) when is_atom(Client) ->
    atom_to_list(Client);
format_client(Client) ->
    io_lib:format("~w", [Client]).

format_indication(fault)                                -> "FAULT";
format_indication(no_power)                             -> "NO_POWER";
format_indication(backup)                               -> "BACKUP";
format_indication(boottest)                             -> "BOOTTEST";
format_indication(loadtest)                             -> "LOADTEST";
format_indication(missing_resource)                     -> "MISSING_RESOURCE";
format_indication(remote_unit)                          -> "REMOTE_UNIT";
format_indication(node)                                 -> "NODE_FAULT";
format_indication(medium)                               -> "MEDIUM_PRESS";
format_indication(short)                                -> "SHORT_PRESS";
format_indication(locked)                               -> "BOARD_LOCKED";
format_indication(shutdown)                             -> "SHUTDOWN";
format_indication(alarm_suppress)                       -> "ALARM_SUPPRESS";
format_indication(?CELLO_VII_FAULT)                     -> "VII_FAULT";
format_indication(?CELLO_VII_NO_FAULT)                  -> "VII_NO_FAULT";
format_indication(?CELLO_VII_LOADTEST_START)            -> "VII_LOADTEST_START";
format_indication(?CELLO_VII_LOADTEST_END)              -> "VII_LOADTEST_END";
format_indication(?CELLO_VII_NO_POWER)                  -> "VII_NO_POWER";
format_indication(?CELLO_VII_POWER)                     -> "VII_POWER";
format_indication(?CELLO_VII_BOOTTEST_START)            -> "VII_BOOTTEST_START";
format_indication(?CELLO_VII_BOOTTEST_END)              -> "VII_BOOTTEST_END";
format_indication(?CELLO_VII_MISSING_RESOURCE_START)    ->
    "VII_MISSING_RESOURCE_START";
format_indication(?CELLO_VII_MISSING_RESOURCE_END)      ->
    "VII_MISSING_RESOURCE_END";
format_indication(?CELLO_VII_BOARD_LOCKED)              -> "VII_BOARD_LOCKED";
format_indication(?CELLO_VII_BOARD_UNLOCKED)            -> "VII_BOARD_UNLOCKED";
format_indication(?CELLO_VII_BOARD_BLOCKED)             ->
    "VII_BOARD_BLOCKED";
format_indication(?CELLO_VII_BOARD_UNBLOCKED)           ->
    "VII_BOARD_UNBLOCKED";
format_indication(?CELLO_VII_DISC_SYNC_START)           ->
    "VII_DISC_SYNC_START";
format_indication(?CELLO_VII_DISC_SYNC_END)             ->
    "VII_DISC_SYNC_END";
format_indication(?CELLO_VII_BOARD_BUSY_START)          ->
    "VII_BOARD_BUSY_START";
format_indication(?CELLO_VII_BOARD_BUSY_END)            ->
    "VII_BOARD_BUSY_END";
format_indication(?CELLO_VII_SHUTDOWN_START)            -> "VII_SHUTDOWN_START";
format_indication(?CELLO_VII_SHUTDOWN_END)              -> "VII_SHUTDOWN_END";
format_indication(?CELLO_VII_BACKUP_START)              -> "VII_BACKUP_START";
format_indication(?CELLO_VII_BACKUP_END)                -> "VII_BACKUP_END";
format_indication(?CELLO_VII_MEDIUM_BUTTON_PRESS_START) ->
    "VII_MEDIUM_BUTTON_PRESS_START";
format_indication(?CELLO_VII_MEDIUM_BUTTON_PRESS_END)   ->
    "VII_MEDIUM_BUTTON_PRESS_END";
format_indication(?CELLO_VII_SHORT_BUTTON_PRESS_START)  ->
    "VII_SHORT_BUTTON_PRESS_START";
format_indication(?CELLO_VII_SHORT_BUTTON_PRESS_END)    ->
    "VII_SHORT_BUTTON_PRESS_END";
format_indication(?CELLO_VII_ALARM_SUPPRESS_START)      ->
    "VII_ALARM_SUPPRESS_START";
format_indication(?CELLO_VII_ALARM_SUPPRESS_END)        ->
    "VII_ALARM_SUPPRESS_END";
format_indication(?CELLO_VII_NODE_FAULT_START)          ->
    "VII_NODE_FAULT_START";
format_indication(?CELLO_VII_NODE_FAULT_END)            ->
    "VII_NODE_FAULT_END";
format_indication(?CELLO_VII_REMOTE_UNIT_FAULT_START)   ->
    "VII_REMOTE_UNIT_FAULT_START";
format_indication(?CELLO_VII_REMOTE_UNIT_FAULT_END)     ->
    "VII_REMOTE_UNIT_FAULT_END";
format_indication(Ind) when is_integer(Ind)             -> integer_to_list(Ind);
format_indication(_)                                    -> "UnknownIndication".

print_leds(Leds) ->
    io:format("LED                 State~n", []),
    [io:format(format_led(Led) ++ format_state(State) ++ "~n", []) ||
	{Led, State} <- Leds].

format_led(fault)       -> "RED (Fault)         ";
format_led(operational) -> "GREEN (Operational) ";
format_led(status)      -> "YELLOW (Status)     ";
format_led(maintenance) -> "BLUE (Maintenance)  ";
format_led(_)           -> "UNKNOWN_LED         ".

format_state(State) when is_atom(State) -> atom_to_list(State);
format_state(_)                         -> "UNKNOWN_STATE".


%% decodeResult(?CELLO_VII_SUCCESS) -> "ok";
%% decodeResult(?CELLO_VII_FAILED) -> "nok".
%%
%% decodeState(?CELLO_VII_LED_OFF) -> "off";
%% decodeState(?CELLO_VII_LED_ON) -> "on";
%% decodeState(?CELLO_VII_LED_SLOW_BLINK) -> "slow_blink";
%% decodeState(?CELLO_VII_LED_FAST_BLINK) -> "fast_blink";
%% decodeState(?CELLO_VII_LED_DOUBLE_FLASH_OFF) -> "flash_off";
%% decodeState(?CELLO_VII_LED_DOUBLE_FLASH_ON) -> "flash_on".


write_log(Name, User, Severity, Msg) ->
    sysEnv:rcs_mode_2() /= vrcs andalso
	logI:write_log(Name, User, Severity, Msg).

get_program_name(ClientPid) ->
    Name = cec:get_program_name(ClientPid),
    gpn(Name).

gpn(Name) when is_list(Name) -> Name;
gpn(undefined)               -> "Unknown".

%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaLedControl.erl %
%%% Author:	erarafo
%%% Description: Provisional solution for the Status LED. This
%%% process holds a limited representation of the alarm list,
%%% built by analyzing calls going from COMSA into COMTE.
%%%
%%% This server controls the Status LED (WP3722). It also takes
%%% care of clearing application alarms in case of warm restart.
%%%
%%% Modules used: eqs_vii_service, coi, coiServer.
%%%
%%% ----------------------------------------------------------
-module(comsaLedControl).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/6').
-date('2015-11-10').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2015-01-30 erarafo     First version
%%% R3A/2      2015-01-30 erarafo     Adjustments
%%% R3A/3      2015-01-31 erarafo     Comments
%%% R3A/5      2015-02-16 erarafo     WP3722: Status LED
%%% R3A/6      2015-02-17 erarafo     Check against duplicates
%%% R3A/7      2015-02-20 etxberb     Adapted for Cluster System.
%%% R3A/8      2015-02-25 etxberb     Bug fix (cluster interaction and missing
%%%                                   Timeout returns for misc msgs).
%%% R3A/9      2015-02-26 etxberb     Timeouts ignored by non-active MP.
%%% R3A/10     2015-03-13 erarafo     Separate retry counts
%%% R3A/11     2015-03-16 erarafo     Newline inserted in error report
%%% R3A/12     2015-03-27 erarafo     Increased retry count for VII call
%%% R4A/1      2015-05-21 erarafo     Fault handling strengthened
%%% R4A/2      2015-06-02 etxpeno     Remove call to deprecated function
%%% R4A/3      2015-08-28 erarafo     Handle warm restart, preparations
%%% R4A/5      2015-09-15 erarafo     Handle warm restart
%%% R4A/6      2015-11-10 erarafo     "Warm uptime" query added
%%% ----------------------------------------------------------

-include("coi.hrl").
-include("eqs_vii.hrl").

-define(INITIAL_DELAY, 5000).
-define(RETRY_NOW, 0).
-define(RETRY_SOON, 200).
-define(RETRY_COI, 1000).
-define(RETRY_VII, 2000).

-define(MAX_RETRY_COI, 200).
-define(MAX_RETRY_VII, 300).

-define(VII_SERVER, eqs_vii_service).
-define(COI_SERVER, coiServer).

%% WP 3722 "Status LED": ignore this particular alarm.
-define(ALARM_ENC_DOOR_OPEN_MAJOR, 193).
-define(ALARM_ENC_DOOR_OPEN_MINOR, 9175085).

%% List of {Maj, Min} tuples for alarms to be ignored
-define(LED_CONTROL_EXCLUSION_LIST, 
	[{?ALARM_ENC_DOOR_OPEN_MAJOR, ?ALARM_ENC_DOOR_OPEN_MINOR}]).


-export([start_link/0,
	 coi_notify/1]).

-export([update/1,
	 update_request/1]).

-export([get_state/0,
	 info/0,
	 info_all/0,
	 millisSinceWarmRestart/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([warm/0, warm_done/0]).


%% General
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%
-define(SERVER, ?MODULE).

-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).


-type ledState()     :: undefined|default|node.
-type indication()   :: integer().

-record(alarmInst,
	{id=0                     :: non_neg_integer(),
	 major=0                  :: pos_integer(),
	 minor=0                  :: pos_integer(),
	 src                      :: ecim_dn()}).

-record(state,
	{coreState,
	 isSubscribed=false       :: boolean(),
	 alarms=none              :: ordsets:ordset(#alarmInst{}) | none,
	 ledState=undefined       :: undefined|default|node,
	 retryCountVii=0           :: non_neg_integer(),
	 retryCountCoi=0           :: non_neg_integer(),
	 lastWarmStart=erlang:system_time(milli_seconds) :: integer()}).


%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Get the server process state.
%%%
%%% @end
%%% ###=====================================================================###
get_state() ->
    gen_server:call(?SERVER, {?MODULE, get_state}).

%%% ###########################################################################
%%% @doc Print the server process state and information.
%%%
%%% @end
%%% ###=====================================================================###
info() ->
    Msg = {?MODULE, info},
    gen_server:cast(?SERVER, Msg).

%%% ###=====================================================================###
info_all() ->
    [rpc:cast(Node, ?MODULE, info, []) || Node <- clhI:erlang_nodes(all)].


%%% ###########################################################################
%%% @doc Returns the number of milliseconds since the last warm
%%% restart. If no warm restart has been done the time since the
%%% server start is returned.
%%% @end
%%% ###=====================================================================###
millisSinceWarmRestart() ->
    #state{lastWarmStart=LastWarmStart} = get_state(),
    erlang:system_time(milli_seconds) - LastWarmStart.

%%% ###########################################################################
%%% @doc Clear application alarms in case of warm restart. It is expected
%%% that the COI service is already running.
%%% @end
%%% ###=====================================================================###
warm() ->
    case getAlarms([]) of
	{error, coi_not_started} ->
	    sysInitI:error_msg(
	      "COI not started, cannot clear application alarms~n", []);
	{error, Reason} ->
	    sysInitI:error_msg("could not get alarms: ~p~n", [Reason]);
	{ok, AlarmInsts} ->
	    AppAlarms =
		lists:reverse(
		  ordsets:fold(
		    fun(#alarmInst{src=Src}=AlarmInst, Acc) ->
			    case re:run(
				   Src, 
				   <<"ManagedElement=.*,SystemFunctions=1">>, 
				   [anchored]) of
				nomatch ->
				    % application alarm, collect for clearing
				    [AlarmInst|Acc];
				_Match ->
				    % RBS-internal alarm, shall survive
				    Acc
			    end
		    end,
		    [],
		    AlarmInsts)),
	    sysInitI:info_msg(
	      "clearing application alarms at warm restart:~n~p~n", 
	      [AppAlarms]),
	    lists:foreach(
	      fun(#alarmInst{major=Major, minor=Minor, src=Src}) -> 
		      comsaI:clear_alarm_by_instance(Major, Minor, Src)
	      end,
	      AppAlarms)
    end.

%%% ###########################################################################
%%% @doc Tell the server that a warm restart has completed.
%%% @end
%%% ###=====================================================================###
warm_done() ->
    gen_server:cast(?SERVER, {?MODULE, warmDone}).

%%% ###########################################################################
%%% @doc Start the server process.
%%%
%%% @end
%%% ###=====================================================================###
start_link() ->
    ServerName = {local, ?SERVER},
    Module = ?MODULE,
    InitArg = #state{coreState = clhI:core_state()},
    Options = [],
    gen_server:start_link(ServerName, Module, InitArg, Options).

%%% ###########################################################################
%%% @doc Update the server process.
%%%
%%% @end
%%% ###=====================================================================###
update(LedState) ->
    gen_server:cast(?SERVER, {?MODULE, update, LedState}).

%%% ###########################################################################
%%% @doc Request from a server process to be updated.
%%%
%%% @end
%%% ###=====================================================================###
update_request(Node) ->
    gen_server:cast(?SERVER, {?MODULE, update_request, Node}).

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ----------------------------------------------------------
%%% @doc Handles a notification.
%%% @end
%%% ----------------------------------------------------------
-spec coi_notify(tuple()) -> ok.

coi_notify(CoiNotification) ->
    case isAlarmNotification(CoiNotification) of
	false ->
	    ok;
	true ->
	    gen_server:cast(?SERVER, {notification, CoiNotification})
    end.

%%% ----------------------------------------------------------
%%% @doc Timeout immediately; the initial delay is chosen so
%%% that hopefully the VII service is up when timeout occurs.
%%% @end
%%% ----------------------------------------------------------

init(#state{coreState = active} = State) ->
    {ok, State, ?INITIAL_DELAY};
init(#state{coreState = undefined} = State) ->
    send_update_request(),
    {ok, State}.

%%% ----------------------------------------------------------
handle_call({?MODULE, get_state}, _From, State) ->
    {reply, State, State, ?RETRY_SOON};
handle_call(_Request, _From, State) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 {from, _From},
				 {unrecognized_msg, _Request}] ++
				?STATE_INFO(State)),
    {reply, ok, State, ?RETRY_SOON}.

%%% ----------------------------------------------------------
%%% @doc Handle a notification. A small timeout is set here
%%% so that if there are multiple notifications within a short
%%% time interval only the last one will cause any action.
%%% @end
%%% ----------------------------------------------------------

handle_cast({notification, _CoiNotification}, State) ->
    {noreply, State, ?RETRY_SOON};
handle_cast({?MODULE, update, NewLedState}, State) ->
    %% From the ?SERVER on the active node.
    case updateLed(NewLedState) of
	error ->
	    warning(?MODULE, ?LINE, "failed to set LED state", []),
	    timer:sleep(?RETRY_VII),
	    send_update_request(),
	    {noreply, State};
	{ok, NewLedState} ->
	    {noreply, State#state{ledState = NewLedState}}
    end;
handle_cast({?MODULE, update_request, Node}, State) ->
    send_update(Node, State),
    {noreply, State, ?RETRY_SOON};
handle_cast({?MODULE, info}, State) ->
    sysInitI:info_report(process_info(self()) ++ ?STATE_INFO(State)),
    {noreply, State, ?RETRY_SOON};
handle_cast({?MODULE, warmDone}, State) ->
    NewState = State#state{lastWarmStart=erlang:system_time(milli_seconds)},
    {noreply, NewState, ?RETRY_SOON};
handle_cast(_Request, State) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, _Request}] ++
				?STATE_INFO(State)),
    {noreply, State, ?RETRY_SOON}.

%%% ----------------------------------------------------------
%%% @doc Handle timeouts. All actions by this server are
%%% handled here.
%%% @end
%%% ----------------------------------------------------------

handle_info(timeout,
	    #state{coreState = CoreState} = State) when CoreState /= active ->
    {noreply, State};

handle_info(timeout, #state{retryCountVii = RetryCount} = State)
  when RetryCount > ?MAX_RETRY_VII ->
    sysInitI:error_msg("~w:~w too many VII retries~n", [?MODULE, ?LINE]),
    {stop, too_many_retries_vii, State};

handle_info(timeout, #state{retryCountCoi = RetryCount} = State)
  when RetryCount > ?MAX_RETRY_COI ->
    sysInitI:error_msg("~w:~w too many COI retries~n", [?MODULE, ?LINE]),
    {stop, too_many_retries_coi, State};

handle_info(timeout, #state{isSubscribed = false} = State) ->
    coi:subscribe(?MODULE),
    {noreply, State#state{isSubscribed=true}, ?RETRY_NOW};

handle_info(timeout, #state{alarms = none} = State) ->
    case getAlarms(?LED_CONTROL_EXCLUSION_LIST) of
	{error, Reason} ->
	    NewRetryCount = State#state.retryCountCoi + 1,
	    warning(?MODULE, ?LINE,
		    "failed to get alarm list, attempt: ~w, reason: ~p, retrying",
		    [NewRetryCount, Reason]),
	    {noreply, State#state{retryCountCoi=NewRetryCount}, ?RETRY_COI};
	{ok, Alarms} ->
	    {noreply, State#state{alarms=Alarms,
				  retryCountCoi=0}, ?RETRY_NOW}
    end;

handle_info(timeout, #state{alarms = Alarms,
			    ledState = undefined} = State) ->
    case setLed(Alarms) of
	error ->
	    NewRetryCount = State#state.retryCountVii + 1,
	    % The first call towards VII may take some time so
	    % don't log each and every attempt.
	    if
		NewRetryCount rem 10 =:= 0 ->
		    warning(?MODULE, ?LINE,
			    "failed to set LED state, attempt: ~w; retrying",
			    [NewRetryCount]);
		true ->
		    ok
	    end,
	    {noreply, State#state{retryCountVii=NewRetryCount}, ?RETRY_VII};
	{ok, NewLedState} ->
	    broadcast_update(NewLedState),
	    NewState = State#state{ledState=NewLedState,
				   retryCountVii=0},
	    {noreply, NewState, infinity}
    end;

handle_info(timeout, #state{alarms = Alarms,
			    ledState = LedState} = State) ->
    case getAlarms(?LED_CONTROL_EXCLUSION_LIST) of
	{error, Reason} ->
	    NewRetryCount = State#state.retryCountCoi + 1,
	    warning(?MODULE, ?LINE,
		    "failed to get alarm list, attempt: ~w; reason: ~p, retrying",
		    [NewRetryCount, Reason]),
	    {noreply, State#state{retryCountCoi=NewRetryCount}, ?RETRY_COI};
	{ok, NewAlarms} ->
	    case setLed(Alarms, NewAlarms, LedState) of
		error ->
		    NewRetryCount = State#state.retryCountVii + 1,
		    warning(?MODULE, ?LINE,
			    "failed to set LED state, attempt: ~w; retrying",
			    [NewRetryCount]),
		    {noreply, State#state{retryCountCoi=0,
					  retryCountVii=NewRetryCount}, ?RETRY_VII};
		{ok, NewLedState} ->
		    broadcast_update(NewLedState),
		    NewState = State#state{alarms=NewAlarms,
					   ledState=NewLedState,
					   retryCountCoi=0,
					   retryCountVii=0},
		    {noreply, NewState, infinity}
	    end
    end;
handle_info(_Msg, State) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, _Msg}] ++
				?STATE_INFO(State)),
    {noreply, State, ?RETRY_SOON}.


terminate(_Reason, _State) ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			      {reason, _Reason}] ++ ?STATE_INFO(_State)),
    ok.


code_change(_OldVsn, State, _Extra) ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			      {oldVsn, _OldVsn},
			      {extra, _Extra}] ++ ?STATE_INFO(State)),
    {ok, State}.


%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ----------------------------------------------------------
%%% @doc Returns true if the given notification is alarms
%%% related.
%%% @end
%%% ----------------------------------------------------------
-spec isAlarmNotification(tuple()) -> boolean().

isAlarmNotification({coi_notification_0, CoiEvents}) ->
    isAlarmRelated(coi_event_0, CoiEvents);

isAlarmNotification({coi_notification_1, Elements}) ->
    lists:any(
      fun({events, CoiEvents}) ->
	      isAlarmRelated(coi_event_1, CoiEvents);
	 (_) ->
	      false
      end,
      Elements).


isAlarmRelated(Key, CoiEvents) ->
    Prefix = "ManagedElement=1,SystemFunctions=1,Fm=1,FmAlarm=",
    PrefixLength = length(Prefix),
    PrefixBinList = [list_to_binary(Prefix)],
    lists:any(
      fun({K, EventProperties}) when K =:= Key ->
	      case proplists:get_value(dn, EventProperties) of
		  undefined ->
		      false;
		  DnList ->
		      lists:any(
			fun(Dn) ->
				case binary:longest_common_prefix(
				       [Dn|PrefixBinList]) of
				    PrefixLength ->
					% the prefix matches the DN, trust that the
					% DN is otherwise well-formed
					true;
				    _ ->
					false
				end
			end,
			DnList)
	      end;
	 (_) ->
	      false
      end,
      CoiEvents).


%%% ----------------------------------------------------------
%%% @doc Gets the alarm list. Alarms appearing in the given
%%% exclusion list will be excluded.
%%% @end
%%% ----------------------------------------------------------
-spec getAlarms([{non_neg_integer(), non_neg_integer()}]) ->
	  {ok, ordsets:ordset(#alarmInst{})} | {error, any()}.

getAlarms(ExclList) ->
    getAlarms(ExclList, whereis(?COI_SERVER)).


getAlarms(_ExclList, undefined) ->
    {error, coi_not_started};

getAlarms(ExclList, _CoiServer) ->
    FmInstS = "ManagedElement=1,SystemFunctions=1,Fm=1",
    AttrNames = [<<"source">>, <<"majorType">>, <<"minorType">>],
    case coi:join_new(?MODULE, getAlarms) of
	{error, _}=E ->
	    E;
	{ok, TransId} ->
	    try
		Iterator =
		    coi:getMoIterator(TransId,
				      list_to_binary(FmInstS), <<"FmAlarm">>),
		AlarmInsts =
		    lists:foldl(
		      fun({_, IdB}, Acc) ->
			      Dn = list_to_binary(
				     FmInstS++",FmAlarm="++binary_to_list(IdB)),
			      case coi:getMoAttributes(TransId, Dn, AttrNames) of
				  [{_, Src}, {_, Major}, {_, Minor}] ->
				      case lists:member({Major, Minor}, ExclList) of
					  true ->
					      Acc;
					  _ ->
					      [#alarmInst{id=list_to_integer(
							       binary_to_list(IdB)),
							  src=Src,
							  major=Major,
							  minor=Minor} |
						   Acc]
				      end;
				  Other ->
				      throw({bad_attributes, Dn, Other})
			      end
		      end,
		      [],
		      Iterator),
		Set = ordsets:from_list(AlarmInsts),
		checkDuplicates(Set),
		{ok, Set}
	    catch
		throw:{bad_attributes, Offender, List} when is_list(List) andalso
						      length(List) =:= length(AttrNames) ->
		    {error, {bad_attributes, Offender, lists:zip(AttrNames, List)}};
		ExType:ExData ->
		    {error, {ExType, ExData}}
	    after
		coi:abort_transaction(TransId),
		coi:finish(TransId)
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Log an error message if there are multiple alarm
%%% instances for some {major, minor, source} triple.
%%% @end
%%% ----------------------------------------------------------
-spec checkDuplicates(ordsets:ordset(#alarmInst{})) -> any().

checkDuplicates(Set) ->
    List = ordsets:to_list(Set),
    Triples = [{Major, Minor, Src}
	      || #alarmInst{major=Major, minor=Minor, src=Src} <- List],
    TriplesSet = ordsets:from_list(Triples),
    TriplesSetSize = ordsets:size(TriplesSet),
    if
	TriplesSetSize < length(Triples) ->
	    sysInitI:error_msg(
	      "~w:~w duplicate FmAlarm instances: ~p~n",
	      [?MODULE, ?LINE, List]);
	true ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Sets the LED based on the given alarm list
%%% and returns the state that was set.
%%% @end
%%% ----------------------------------------------------------
-spec setLed(ordsets:ordset(#alarmInst{})) -> {ok, ledState()} | error.

setLed(Alarms) ->
    {LedState, Indication} =
	case ordsets:size(Alarms) of
	    0 ->
		{default, ?CELLO_VII_NODE_FAULT_END};
	    _ ->
		{node, ?CELLO_VII_NODE_FAULT_START}
	end,
    case visualIndRequest(Indication) of
	false ->
	    error;
	true ->
	    {ok, LedState}
    end.

%%% ----------------------------------------------------------
updateLed(undefined) ->
    error;
updateLed(LedState) ->
    Indication =
	case LedState of
	    default ->
		?CELLO_VII_NODE_FAULT_END;
	    node ->
		?CELLO_VII_NODE_FAULT_START
	end,
    case visualIndRequest(Indication) of
	false ->
	    error;
	true ->
	    {ok, LedState}
    end.

%%% ----------------------------------------------------------
%%% @doc Sets the LED based on the given old and new alarm list
%%% and the known LED state. Be lazy when possible.
%%% @end
%%% ----------------------------------------------------------
-spec setLed(ordsets:ordset(#alarmInst{}),
	     ordsets:ordset(#alarmInst{}),
	     ledState()) ->
	  {ok, ledState()} | error.

setLed(_OldAlarms, NewAlarms, undefined) ->
    setLed(NewAlarms);

setLed(OldAlarms, NewAlarms, LedState) ->
    OldAlarmState = ordsets:size(OldAlarms) > 0,
    NewAlarmState = ordsets:size(NewAlarms) > 0,
    if
	NewAlarmState =:= OldAlarmState ->
	    {ok, LedState};
	NewAlarmState andalso not(OldAlarmState) ->
	    case visualIndRequest(?CELLO_VII_NODE_FAULT_START) of
		false ->
		    error;
		true ->
		    {ok, node}
	    end;
	not(NewAlarmState) andalso OldAlarmState ->
	    case visualIndRequest(?CELLO_VII_NODE_FAULT_END) of
		false ->
		    error;
		true ->
		    {ok, default}
	    end
    end.


%%% ----------------------------------------------------------
broadcast_update(LedState) ->
    [rpc:cast(Node, ?MODULE, update, [LedState]) ||
	Node <- clhI:erlang_nodes()].

%%% ----------------------------------------------------------
send_update(Node, #state{ledState = LedState}) ->
    rpc:cast(Node, ?MODULE, update, [LedState]).

%%% ----------------------------------------------------------
send_update_request() ->
    case clhI:erlang_nodes(active) of
	[ActiveNode] ->
	    rpc:cast(ActiveNode, ?MODULE, update_request, [clhI:erlang_node()]);
	Nodes ->
	    sysInitI:error_report([{?MODULE, send_update_request},
				       {no_active_node, Nodes}])
    end.

%%% ----------------------------------------------------------
%%% @doc Try to set LED state as specified. Returns true if
%%% successful.
%%% @end
%%% ----------------------------------------------------------
-spec visualIndRequest(indication()) -> boolean().

visualIndRequest(Indication) ->
    case whereis(?VII_SERVER) of
	undefined ->
	    false;
	_ ->
	    try
		eqs_vii:'CelloVii_visualIndRequest'(Indication),
		true
	    catch
		ExType:ExData ->
		    sysInitI:warning_msg(
		      "~w:~w LED indication failed, ~w, ~p~n",
		      [?MODULE, ?LINE, ExType, ExData]),
		    false
	    end
    end.


warning(Module, Line, Format, Data) ->
    sysInitI:warning_msg("~w:~w "++Format++"~n", [Module, Line|Data]).


%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###

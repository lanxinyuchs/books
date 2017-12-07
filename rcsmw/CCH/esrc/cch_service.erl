%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cch_service.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R10A/R11A/1

%%% @doc ==CCH service==
%%% This module implements Calendar Clock Interface which allows applications
%%% to subscribe to changes in the system calendar clock, and sends out 
%%% notifications to those subscribers
%%% @end

-module(cch_service).
-behaviour(gen_server).
-vsn('/main/R5A/R10A/R11A/1').
-date('2017-08-24').
-author('ekurnik').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2016-01-12 etomist     Created
%%% R5A/2      2016-02-19 ekurnik     Added get_subscribers/0 function
%%%                                   for testing
%%% R5A/3      2016-02-25 ekurnik     Added types and specs
%%% R5A/4      2016-03-01 etomist     Small changes after code review
%%% ----------------------------------------------------------
%%% R10A/1     2017-06-08 edartop     Clock slew algorithm
%%% R10A/2     2017-06-09 estjako     Polling ntpd servers
%%% R10A/3     2017-06-13 estjako     Removed polling
%%% R10A/4     2017-06-19 ekurnik     Polling ntpd servers added, 
%%%                                   removed tinker step check-+
%%% R10A/5     2017-06-20 ekurnik     HV94590 additional fix, 
%%%                                   added is_synced sanity check
%%% R10A/6     2017-06-21 ekurnik     HV94590 additional fix
%%% R10A/7     2017-06-26 ekurnik     Refactored subscriber data storing 
%%% R10A/8     2017-06-26 eivmiha     Added subscribe support for protocol version 2
%%% R10A/9     2017-06-27 edartop     Added check for protocol version
%%% R10A/10    2017-06-27 eivmiha     Added ntp_state_ind handling design
%%% R10A/13    2017-06-29 eivmiha     Added target check for poll_ntpd
%%% R10A/14    2017-06-29 estjako     Exported for test
%%% R10A/16    2017-07-03 ekurnik     Fixed formatting and minor refactor
%%% R10A/17    2017-07-04 ekurnik     Fixed formatting of negative offset
%%% R10A/18    2017-07-06 evadumb     Added timestamp to state, logic to support slew/discrepancy for pv2
%%% ----------------------------------------------------------
%%% R11A/1     2017-08-24 ekurnik     HW22014
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% for test and debug
-export([show_subscribers/0, get_subscribers/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% ----------------------------------------------------------
%%% #2.3   EXPORTED OTHER FUNCTIONS
%%% ----------------------------------------------------------
%% for internal use
-export([calculate_clock_slew/2, start_polling_ntpd/0, send_ntp_state_ind/2]).

%%%===================================================================
%%% Include files
%%%===================================================================
-include("cch_service.hrl").

%%%===================================================================
%%% Macros
%%%===================================================================
-define(SERVER, ?MODULE).

%% Interface constants
-define(CCI_MAX_SUBSCRIBERS, 20).
-define(CCI_MAX_TIME_OFFSET_US, 50000).
-define(CCI_MAX_DELTA, 60 * 60 * 1000000). %% 1h in us

%% Other
-define(IS_SUPPORTED(PV), PV =:= ?CCI_PV1 orelse PV =:= ?CCI_PV2).
-define(DEFAULT_OFFSET, 0).


%%%===================================================================
%%% Records
%%%===================================================================
-record(state, {itcPort,                     %% port of ITC mailbox named CCI_mbox
                monitorRef,                  %% reference for monitoring clock service
                timeOffset,                  %% time offset between system and erlang clock
                ntpOffset = undefined,       %% last read offset from sync source
                cumulativeDelta,             %% total time change during jumps
                subscribers,                 %% applications subscribed to CCH service  
                timerPollReference,          %% timer reference for polling ntpd
                sync,                        %% current state of sync
                ntpOffsetChangedTime}).      %% timestamp when ntpOffset last changed

-record(subscriber, {process_id,             %% pid of subscribing application
                     itc_ref,                %% itc mbox reference
                     pv}).                   %% protocol version

-type process_id() :: integer().

-type subscriber_map() ::
    #{itc:mailbox_id() => subscriber()}.

-type state() :: #state{itcPort :: itc:mailbox(),
                        monitorRef :: reference(),
                        ntpOffset :: integer() | undefined,
                        timeOffset :: integer(),
                        cumulativeDelta :: non_neg_integer(),
                        subscribers :: subscriber_map(),
                        timerPollReference ::  timer:tref() | undefined,
                        sync :: cci_sync(),
                        ntpOffsetChangedTime :: erlang:timestamp() | undefined}.

-type subscriber() :: #subscriber{process_id :: process_id(),
                                  itc_ref :: itc:attach_ref(),
                                  pv :: cci_pv()}.
-type cci_reject_reason() ::
    ?CCI_REJ_SERVICE_UNAVAIL..?CCI_REJ_MAX_SUBSCRIBERS_REACHED.
-type cci_pv() :: 
          ?CCI_PV1 | ?CCI_PV2.
-type cci_sync() ::
            ?NTP_OUT_OF_SYNC | ?NTP_IN_SYNC.
-type cci_time_update_reason() ::
          ?CCI_REASON_INITIAL..?CCI_REASON_SLEW_DISCREPANCY.

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 0, []).

init(_Opts) ->
    erlang:process_flag(trap_exit, true),
    ItcPort = itc:open("CCI_mbox"),
    itc:listen(ItcPort),
    MonitorRef = erlang:monitor(time_offset, clock_service),
    TimerReference = start_polling_ntpd(),
    Sync = ?NTP_OUT_OF_SYNC,
    
    {ok, #state{itcPort = ItcPort,
                monitorRef = MonitorRef,
                timeOffset = erlang:time_offset(micro_seconds),
                cumulativeDelta = 0,
                subscribers = #{},
                timerPollReference = TimerReference,
                sync = Sync,
                ntpOffsetChangedTime = undefined}}.

handle_call({?MODULE, get_state}, _From, State) ->
    {reply, State, State};

handle_call(get_subscribers, _From, 
            #state{subscribers = Subscribers} = State) ->
    {reply, Subscribers, State};

handle_call(Request, _From, State) ->
    sysInitI:warning_msg("cch_service -unknown request received ~p~n",
                         [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(show_subscribers, State) ->
    Fun = fun(MboxId, #subscriber{process_id = ProcessId, pv = PV}, Acc) -> 
            Acc ++ io_lib:format("~10.16x    ~50s       ~B~n", 
                                 [MboxId, "0x", get_process_name(ProcessId), PV])
          end,
    Border = "-------------------------------------------------------------------------------",
    Init = io_lib:format("~10s ~50s ~10s~n~s~n", ["Mailbox", "Process", "PV", Border]),
    Msg = maps:fold(Fun, Init, State#state.subscribers),
    sysInitI:info_msg("~s~s~n", [Msg, Border]),
    {noreply, State};

handle_cast(Msg, State) ->
    sysInitI:warning_msg("cch_service - unknown message received ~p~n", [Msg]),
    {noreply, State}.

handle_info({message, _FromPid,
            {FromMbox, _ToMbox, ?CCI_SUBSCRIBE_REQ, 
             <<ProcessId:4/native-unsigned-integer-unit:8>>}},
            State) ->
    {Status, Reason, NewState} = add_subscriber(FromMbox, ProcessId, State),
    send_subscribe_response(Status, Reason, FromMbox, NewState),
    ProcessInfo = get_process_name(ProcessId),
    case Status of
        ok ->
            send_time_update_ind(unicast, FromMbox, NewState),
            sysInitI:info_msg("cch_service - subscriber ~p [mailbox: ~p] added~n",
                              [ProcessInfo, FromMbox]);
        error ->
            sysInitI:info_msg("cch_service - Unable to add subscriber ~p "
                               "[mailbox: ~p], reason ~p~n",
                               [ProcessInfo, FromMbox, Reason])
    end,
    {noreply, NewState};

handle_info({message, _FromPid, 
            {FromMbox, _ToMbox, ?CCI2_SUBSCRIBE_REQ, 
             <<ProcessId:4/native-unsigned-integer-unit:8,
               CciPv:4/native-unsigned-integer-unit:8>>}},
            State) ->
    {Status, Reason, NewState} = add_subscriber(FromMbox, ProcessId, CciPv, State),
    send_subscribe_response(Status, Reason, FromMbox, NewState),
    ProcessInfo = get_process_name(ProcessId),
    case Status of
        ok ->
            send_time_update_ind(unicast, FromMbox, NewState),
            send_ntp_state_ind(unicast, FromMbox, NewState),
            sysInitI:info_msg("cch_service - subscriber ~p [mailbox: ~p] added, pv: ~p~n",
                              [ProcessInfo, FromMbox, CciPv]);
        error ->
            sysInitI:info_msg("cch_service - Unable to add subscriber ~p "
                               "[mailbox: ~p], reason ~p~n",
                               [ProcessInfo, FromMbox, Reason])
    end,
    {noreply, NewState};

handle_info({message, _FromPid,
            {FromMbox, _ToMbox, ?CCI_UNSUBSCRIBE_REQ, _Data}},
            State) ->
    {Status, Reason, ProcessId, NewState} = remove_subscriber(FromMbox, State),
    case Status of
        error ->
            sysInitI:info_msg("cch_service - unable to remove subscriber ~p, "
                              "reason ~p~n", [FromMbox, Reason]);
        ok ->
            ProcessInfo = get_process_name(ProcessId),
            sysInitI:info_msg("cch_service - subscriber ~p "
                              "[mailbox ~p] removed~n", [ProcessInfo, FromMbox])
    end,
    send_unsubscribe_response(Status, Reason, FromMbox, NewState),
    {noreply, NewState};

handle_info({mailbox_down, _Port, _Ref, MboxId}, State) ->
    sysInitI:info_msg("cch_service - subscriber ~p went down~n", [MboxId]),
    {_, _, _, NewState} = remove_subscriber(MboxId, State),
    {noreply, NewState};

handle_info({'CHANGE', _MonitorReference,
             time_offset, clock_service, _NewTimeOffset}, State) ->
    OldTimeOffset = State#state.timeOffset,
    NewTimeOffset = erlang:time_offset(micro_seconds),
    Delta = NewTimeOffset - OldTimeOffset,
    sysInitI:info_msg("cch_service - monitor sent time change indication, "
                      "old time offset ~p us, new time offset ~p us, "
                      "delta ~p us~n", [OldTimeOffset, NewTimeOffset, Delta]),
    OldDelta = State#state.cumulativeDelta,
    NewState = State#state{timeOffset = NewTimeOffset, 
                           cumulativeDelta = OldDelta + erlang:abs(Delta),
                           ntpOffset = undefined,
                           sync = ?NTP_OUT_OF_SYNC,         %% when step occurs, clear offset,
                           ntpOffsetChangedTime = undefined},
    ?MODULE ! poll_ntpd, %% send poll_ntpd to checck offset and sync change
    {noreply, send_time_update_ind(broadcast, NewState)};

handle_info(poll_ntpd, State) ->
    case target_or_vrcs() of
        true -> timer:cancel(State#state.timerPollReference),
                OffsetVal =
                case is_ntpd_synced() of  %% sanity check, if system is not synced no need to poll for sync_source
                    true ->
                        get_sync_source_state();
                    false->
                        undefined
                end,
                
                send_ntp_state_ind(convert_to_us(to_float(OffsetVal)), State),
                %% Received offset is string in ms, need to convert it to integer and us
                NewState = calculate_clock_slew(convert_to_us(to_float(OffsetVal)), State),
                NewState2 = check_sync_change(OffsetVal, NewState),
                {noreply, NewState2#state{timerPollReference = start_polling_ntpd()}};
        false -> {noreply, State}
    end;

handle_info({_Ref, _Msg}, State) ->
    %% Ignore the late responses from comsaNtpServer
    {noreply, State};

handle_info(Msg, State) ->
    sysInitI:warning_msg("cch_service - unknown message received ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    [itc:send(State#state.itcPort, MboxId, ?CCI_SERVER_DOWN_IND, <<>>)
     || MboxId <- maps:keys(State#state.subscribers)],
    itc:close(State#state.itcPort),
    erlang:demonitor(State#state.monitorRef).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

show_subscribers() ->
    gen_server:cast(?SERVER, show_subscribers).

get_subscribers() ->
    gen_server:call(?SERVER, get_subscribers).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-spec add_subscriber(itc:mailbox_id(), process_id(), state()) ->
    {error, cci_reject_reason(), state()} | {ok, none, state()}.
add_subscriber(MboxId, ProcessId, State) ->
    add_subscriber(MboxId, ProcessId, ?CCI_PV1, State).

-spec add_subscriber(itc:mailbox_id(), process_id(), cci_pv(), state()) ->
    {error, cci_reject_reason(), state()} | {ok, none, state()}.
add_subscriber(MboxId, ProcessId, CciPv, State) when ?IS_SUPPORTED(CciPv)->
    M = State#state.subscribers,
    case check_subscriber_count(count_subscribers(M)) of
        error -> 
            {error, ?CCI_REJ_MAX_SUBSCRIBERS_REACHED, State};
        ok ->
            case maps:is_key(MboxId, M) of
                true ->
                    {error, ?CCI_REJ_ALREADY_SUBSCRIBED, State};
                false ->
                    Ref = itc:attach(State#state.itcPort, MboxId),
                    NewSubscriber = #subscriber{process_id = ProcessId, 
                                                itc_ref = Ref,
                                                pv = CciPv},
                    {ok, none, 
                    State#state{subscribers = M#{MboxId => NewSubscriber}}}
            end
    end;

add_subscriber(_MboxId, _ProcessId, _CciPv, State) ->
    {error, ?CCI_REJ_UNSUPPORTED_PV, State}.

-spec check_subscriber_count(non_neg_integer()) -> error | ok.
check_subscriber_count(Count) when Count > ?CCI_MAX_SUBSCRIBERS ->
    error;

check_subscriber_count(_Count) ->
    ok.

-spec send_subscribe_response(ok, any(), itc:mailbox_id(), state()) -> ok;
                (error, cci_reject_reason(), itc:mailbox_id(), state()) -> ok.
send_subscribe_response(ok, _Reason, MboxId, State) ->
    itc:send(State#state.itcPort, MboxId, ?CCI_SUBSCRIBE_CFM, <<>>);

send_subscribe_response(error, Reason, MboxId, State) ->
    Data = <<Reason:4/native-unsigned-integer-unit:8>>,
    itc:send(State#state.itcPort, MboxId,
             ?CCI_SUBSCRIBE_REJ, iolist_to_binary(Data)).

-spec remove_subscriber(itc:mailbox_id(), state()) ->
        {error, cci_reject_reason(), none, state()} |
        {ok, none, process_id(), state()}.
remove_subscriber(MboxId, State) ->
    M = State#state.subscribers,
    case maps:is_key(MboxId, M) of
        false ->
            {error, ?CCI_REJ_ALREADY_UNSUBSCRIBED, none, State};
        true ->
            #{MboxId := #subscriber{process_id = ProcessId, itc_ref = Ref}} = M,
            itc:detach(State#state.itcPort, Ref),
            {ok, none, 
             ProcessId, State#state{subscribers = maps:remove(MboxId, M)}}
    end.

-spec send_unsubscribe_response(ok, any(), itc:mailbox_id(), state()) -> ok;
                               (error, cci_reject_reason(),
                                itc:mailbox_id(), state()) -> ok.
send_unsubscribe_response(ok, _Reason, MboxId, State) ->
    itc:send(State#state.itcPort, MboxId, ?CCI_UNSUBSCRIBE_CFM, <<>>);

send_unsubscribe_response(error, Reason, MboxId, State) ->
    Data = <<Reason:4/native-unsigned-integer-unit:8>>,
    itc:send(State#state.itcPort, MboxId, 
             ?CCI_UNSUBSCRIBE_REJ, iolist_to_binary(Data)).



send_time_update_ind(broadcast, State) ->
    send_time_update_ind(broadcast, ?CCI_REASON_STEP, State).

-spec send_time_update_ind(unicast, itc:mailbox_id(), state()) -> ok;
                          (broadcast, cci_time_update_reason(), state()) -> state().
send_time_update_ind(unicast, MboxId, State) ->
   Data = <<?CCI_REASON_INITIAL:4/native-unsigned-integer-unit:8,
            0:4/native-unsigned-integer-unit:8>>,
   itc:send(State#state.itcPort, MboxId, 
            ?CCI_TIME_UPDATE_IND, iolist_to_binary(Data));

send_time_update_ind(broadcast, Reason, State) when
        State#state.cumulativeDelta >= ?CCI_MAX_TIME_OFFSET_US ->
    
    Delta = delta_cutoff(State#state.cumulativeDelta),

    sysInitI:info_msg("cch_service - time changed, sending new time indication "
                      "(delta ~p us) to all subscribers (~p total)~n", 
                      [Delta, count_subscribers(State#state.subscribers)]),

    ok = lists:foreach(
            fun(MboxId) ->
                Subscriber = maps:get(MboxId, State#state.subscribers),
                Data = create_time_update_ind_data(Subscriber#subscriber.pv, Delta, Reason),
                itc:send(State#state.itcPort, MboxId, ?CCI_TIME_UPDATE_IND, iolist_to_binary(Data))
            end,
            maps:keys(State#state.subscribers)),

    State#state{cumulativeDelta = 0};

send_time_update_ind(broadcast, _Reason, State) ->
    sysInitI:info_msg("cch_service - time changed, delta: ~p us, too small to "
                      "notify subscribers!~n", [State#state.cumulativeDelta]),
    State.


create_time_update_ind_data(?CCI_PV1, Delta, _Reason) ->
    << ?CCI_REASON_STEP:4/native-unsigned-integer-unit:8,
       Delta:4/native-unsigned-integer-unit:8>>;
create_time_update_ind_data(?CCI_PV2, Delta, Reason) ->
    << Reason:4/native-unsigned-integer-unit:8,
       Delta:4/native-unsigned-integer-unit:8>>.


-spec send_ntp_state_ind(integer() | undefined, state()) -> ok.
send_ntp_state_ind(undefined, _State=#state{sync = ?NTP_OUT_OF_SYNC}) ->
    ok;

send_ntp_state_ind(undefined, State) ->
    Data = <<?NTP_OUT_OF_SYNC:4/native-unsigned-integer-unit:8,
            ?DEFAULT_OFFSET:4/native-signed-integer-unit:8>>,
    send_ntp_state_ind(broadcast, iolist_to_binary(Data), State);

send_ntp_state_ind(OffsetValue, State=#state{sync = ?NTP_OUT_OF_SYNC}) ->
    Offset = convert_to_ms(OffsetValue),
    Data = <<?NTP_IN_SYNC:4/native-unsigned-integer-unit:8,
            Offset:4/native-signed-integer-unit:8>>,
    send_ntp_state_ind(broadcast, iolist_to_binary(Data), State);

send_ntp_state_ind(OffsetValue, State) ->
    Offset = convert_to_ms(OffsetValue),
    case State#state.ntpOffset div 1000 /= Offset of
        false -> ok;
        true ->     Data = <<?NTP_IN_SYNC:4/native-unsigned-integer-unit:8,
                            Offset:4/native-signed-integer-unit:8>>,
                    send_ntp_state_ind(broadcast, iolist_to_binary(Data), State)
    end.
-spec send_ntp_state_ind(unicast | broadcast,  binary() | itc:mailbox_id(), state()) -> ok.
send_ntp_state_ind(broadcast, <<SyncState:4/native-unsigned-integer-unit:8,
                                Offset:4/native-signed-integer-unit:8>> = Data, State) ->
    sysInitI:info_msg("cch_service - ntp state changed, sending new ntp state indication "
                      "(sync state: ~p, offset: ~p ms) to all PV2 subscribers (~p total)~n", 
                      [SyncState, Offset, count_subscribers(?CCI_PV2, State#state.subscribers)]),
    MboxIds =  maps:keys(State#state.subscribers),
    send_ntp_state_message(MboxIds, Data, State);

send_ntp_state_ind(unicast, MboxId, State=#state{sync = ?NTP_OUT_OF_SYNC}) ->
    Data = <<?NTP_OUT_OF_SYNC:4/native-unsigned-integer-unit:8,
            ?DEFAULT_OFFSET:4/native-signed-integer-unit:8>>,
    send_ntp_state_message([MboxId], Data, State);
    

send_ntp_state_ind(unicast, MboxId, State=#state{sync = ?NTP_IN_SYNC}) ->
    Offset = convert_to_ms(State#state.ntpOffset),
    Data = <<?NTP_IN_SYNC:4/native-unsigned-integer-unit:8,
            Offset:4/native-signed-integer-unit:8>>,
    send_ntp_state_message([MboxId], iolist_to_binary(Data), State).

-spec send_ntp_state_message(list(), binary(), state()) -> ok.
send_ntp_state_message([], _Data, _State) ->
    ok;

send_ntp_state_message([MboxId | MboxIds], Data, State) ->
    
    Subscriber = maps:get(MboxId, State#state.subscribers),
    
    case Subscriber#subscriber.pv of
        ?CCI_PV2 -> itc:send(State#state.itcPort, MboxId, ?CCI2_NTP_STATE_IND, Data),
                    send_ntp_state_message(MboxIds, Data, State);
        ?CCI_PV1 -> send_ntp_state_message(MboxIds, Data, State)
    end.

-spec delta_cutoff(non_neg_integer()) -> non_neg_integer().
delta_cutoff(Delta) when Delta > ?CCI_MAX_DELTA ->
       ?CCI_DELTA_TOO_BIG;

delta_cutoff(Delta) ->
       Delta.

-spec get_process_name(process_id()) -> list().
get_process_name(ProcessId) ->
    [_, {_, Name}] = sysUtil:pid_name(ProcessId),
    Name.

-spec calculate_clock_slew(integer(), state()) -> state().
calculate_clock_slew(undefined, S) ->
    S;

calculate_clock_slew(Offset, S=#state{ntpOffset = Offset }) ->
    S;

calculate_clock_slew(Offset, S=#state{ntpOffset = undefined }) ->
    sysInitI:info_msg("cch_service - initial ntpd offset: (~p us)~n", [Offset]),
    S#state{ntpOffset=Offset, ntpOffsetChangedTime=erlang:timestamp()};

calculate_clock_slew(Offset, S=#state{ cumulativeDelta = CD, ntpOffset = OldOffset, ntpOffsetChangedTime = OldTimestamp}) ->
    %% absolute substraction of the old and new offset integer 
    OffsetSub = erlang:abs(OldOffset - Offset),
    Timestamp = erlang:timestamp(),
    %% in ms/s
    MAXSlew =  OffsetSub / micro_to_seconds(timer:now_diff(Timestamp, OldTimestamp)),
    Reason = case MAXSlew >= ?MAX_SLEW_TIME of
                 true -> ?CCI_REASON_SLEW_DISCREPANCY;
                 false -> ?CCI_REASON_SLEW
             end,
    NewState = send_time_update_ind(broadcast, Reason, S#state{cumulativeDelta = OffsetSub + CD}),
    
    NewState#state{ntpOffset = Offset, ntpOffsetChangedTime=Timestamp}.

micro_to_seconds(Time) ->
    Time/1000000.

-spec check_sync_change(integer() | undefined, state()) -> state().
check_sync_change(OffsetValue, State) ->
    NewSync = case OffsetValue of
                  undefined -> ?NTP_OUT_OF_SYNC;
                  _ -> ?NTP_IN_SYNC
              end,
    
    State#state{sync = NewSync}.
    
    
-spec start_polling_ntpd() -> undefined | timer:tref().
start_polling_ntpd() ->
    case target_or_vrcs() of
        false -> 
            undefined; %% no ntpd on SIM
        _-> 
            {ok, Tref} = timer:send_after(?POLL_TIMER, poll_ntpd),
            Tref
    end.

-spec target_or_vrcs() -> true | false.
target_or_vrcs() ->
    case sysEnv:rcs_mode_2() of
    simulated -> false;
    _ -> true
    end.

-spec is_ntpd_synced() -> true | false.
is_ntpd_synced() ->
    %% HW22014
    case call_comsa_ntp_server(is_synced, []) of
        Bool when is_boolean(Bool) ->
            Bool;
        timeout ->
            false
    end.

-spec get_sync_source_state() -> undefined | binary().
get_sync_source_state() ->
    case call_comsa_ntp_server(cci_get_ntpd_sync_source_state, []) of
      no_sync_source -> 
          undefined;
      {sync_source, StateList} ->
          proplists:get_value(?OFFSET_VAR_KEY, StateList, undefined);
      timeout ->
          undefined
    end.

-spec convert_to_us(float()) -> integer();
                   (undefined) -> undefined.
convert_to_us(undefined) ->
  undefined;
convert_to_us(Offset) ->
    erlang:round(Offset*1000).

-spec convert_to_ms(integer()) -> integer();
                   (undefined) -> undefined.
convert_to_ms(undefined) ->
    undefined;
convert_to_ms(Offset) ->
    Offset div 1000.

-spec to_float(binary() | list()) -> float();
              (undefined) -> undefined.
to_float(undefined) ->
    undefined;
to_float(Offset) when is_binary(Offset) ->
    binary_to_float(Offset).

-spec count_subscribers(map() | list()) -> non_neg_integer().
count_subscribers(Subscribers) when is_list(Subscribers)->
    length(Subscribers);
count_subscribers(Subscribers) ->
    maps:size(Subscribers).

-spec count_subscribers(SelectedPV :: cci_pv(), 
                        Subscribers :: map()) -> non_neg_integer().
count_subscribers(SelectedPV, Subscribers) ->
    PVSubscribers = 
    lists:filter(
                fun(#subscriber{pv = PV}) when SelectedPV =:= PV ->
                        true;
                    (_) ->
                         false
                 end, 
                maps:values(Subscribers)),
    count_subscribers(PVSubscribers).

%% comsaNtpServer call wrapper, handling of gen_server:call timeouts
-spec call_comsa_ntp_server(F :: atom(), A :: list()) -> term().
call_comsa_ntp_server(F, A) ->
    try erlang:apply(comsaNtpServer, F, A) of
      Result ->
          Result
    catch
        _:_ ->
            %% this call can fail due to a timeout from ntpd
            timeout
    end.
    

    
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

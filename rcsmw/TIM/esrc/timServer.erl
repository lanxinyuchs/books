%%% ----------------------------------------------------------
%%% %CCaseFile:	timServer.erl %
%%% Author: erarafo
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(timServer).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R4A/R7A/R9A/R10A/1').
-date('2017-07-19').
-author('etomist').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R4A/3      2015-09-04 erarafo     Corrections and trace printouts
%%% R4A/4      2015-09-08 erarafo     Corrected behavior
%%% R4A/5      2015-09-10 erarafo     Handling leap seconds subscriptions
%%% R4A/6      2015-09-28 erarafo     Added functionality
%%% R4A/7      2015-09-30 erarafo     Corrected leap seconds IND
%%% R4A/8      2015-10-08 erarafo     Functionally complete
%%% R4A/10     2015-10-09 erarafo     Functional adjustments
%%% R4A/11     2015-10-14 erarafo     Corrections and refactoring
%%% R4A/12     2015-10-15 erarafo     Undialyzed type fault fixed
%%% R4A/13     2015-10-16 erarafo     Refactoring; some behaviours changed
%%% R4A/14     2015-10-16 erarafo     First version
%%% R4A/15     2015-10-18 erarafo     Error handling
%%% R4A/16     2015-10-19 erarafo     Implementation for re-subscribe fixed
%%% R4A/17     2015-10-20 erarafo     Handling a negative case
%%% R4A/18     2015-10-20 erarafo     Transient TCP connections
%%% R4A/19     2015-10-21 erarafo     Handling of repeated initiateService
%%% R4A/20     2015-10-26 erarafo     Proxy handling simplified
%%% R4A/21     2015-10-27 erarafo     Adapted to OTP18 time API
%%% R4A/22     2015-10-29 erarafo     Responses to client requests
%%% R4A/23     2015-11-03 erarafo     Line length not exceeding 80
%%% R4A/24     2015-11-04 erarafo     Try/catch protection
%%% R4A/25     2015-11-05 erarafo     setMailbox support
%%% R4A/26     2015-11-09 erarafo     setMailbox support, adjusted
%%% R4A/27     2015-11-16 erarafo     setMailbox support, flexibility
%%% R4A/28     2015-11-17 erarafo     Accept unconfigured leap seconds
%%% R4A/29     2015-11-18 erarafo     Exception handling revised
%%% R4A/30     2015-11-29 erarafo     Comments cleaned
%%% R4A/31     2015-11-30 erarafo     Cleanup
%%% R7A/1      2016-09-21 erarafo     Introduce trace functionality
%%% R9A/1      2017-04-03 etomist     HV73602
%%% R10A/1     2017-07-19 etomist     HW12725
%%% ----------------------------------------------------------



% include files in *CAX*/out, run `clearmake' to generate
-include("tim.hrl").
-include("cello_tzii.hrl").
-include("cello_tzii_internal.hrl").
-include("cello_tzii_sig.hrl").

-define(OK, <<?CEC_RESPONSE_OK,0>>).
-define(UNKNOWN, <<?CEC_RESPONSE_UNKNOWN,0>>).


-export([start/0, start/1, start_link/0, start_link/1]).
-export([activate/0]).
-export([cec_setup/1, cec_takeover/1]).

-export([testControl/2]).     % for tests

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([checkDst/0]).     % for debugging purposes

start() ->
    start(start, []).


start(InitArgs) ->
    start(start, InitArgs).


start_link() ->
    start_link([]).


start_link(InitArgs) ->
    start(start_link, InitArgs).


start(StartFun, InitArgs) ->
    apply(gen_server, StartFun, [{local, ?SERVER}, ?MODULE, InitArgs, []]).


activate() ->
    ok = cec:register(?CEC_SIGNATURE, ?MODULE),
    ok.


%%% ----------------------------------------------------------
%%% @doc A new TCP session is being set up. Save the given socket
%%% and return the PID of the gen_server process.
%%% @end
%%% ----------------------------------------------------------

cec_setup(Socket) ->
    gen_server:call(?SERVER, {cec_setup, Socket}).

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

checkDst() ->
    gen_server:cast(?SERVER, getDstStatus).
%%% ----------------------------------------------------------
%%% @doc Special actions, for test only.
%%% @end
%%% ----------------------------------------------------------
-spec testControl(atom(), [any()]) -> any().

testControl(getVersion, []) ->
    case code:which(?MODULE) of
  Path when is_list(Path) ->
      case beam_lib:version(Path) of
    {error, _, _} ->
        'unknown';
    {ok, {_, [Version]}} ->
        Version
      end;
  _ ->
      'unknown'
    end;


testControl(start, []) ->
    start();

testControl(testDstRule, [DstRule, Year]) ->
    try
  {ok, getGregorianDay(DstRule, Year)}
    catch
  throw:Thrown ->
      Thrown
    end;

testControl(Op, Args) ->
    try
  gen_server:call(?SERVER, {testControl, Op, Args})
    catch
  X:Y ->
      {call_failed, {?SERVER, {testControl, Op, Args}, X, Y}}
    end.


-record(state,
   {refTs=0                        :: millis(),
    refDay=0                       :: integer(),
    maxPrewarn=?MAX_PREWARN_MILLIS :: non_neg_integer(),
    simFuture=0                    :: integer(),
    itcPort                        :: port()|undefined,
    timeSettings                   :: #timeSettings{}|undefined,
    clients=gb_trees:empty()       :: gb_trees:tree(),   % spid -> #client{}
    signalRecords=orddict:new()    :: orddict:orddict(),
    nextItime=0                    :: itime(),           % predicted next invoc
    traceLevel=0                   :: integer()
   }).


-record(subscr,
   {clientInfo=0           :: clientInfo(),
    preWarn=0              :: millis()
   }).

-record(client,
   {spidInitial=0          :: uint32(),
    spid=0                 :: uint32(),
    wantedPvs=[]           :: [non_neg_integer()],
    subscrs=orddict:new()  :: orddict:orddict()   % eventType() -> #subscr{}
   }).



%% Represents a point in time when DST changes, as computed from
%% the timesettings MO.

-record(dstChange, {itime=0         :: itime(),
        type=dstStart   :: dstStart|dstEnd}).



-record(signalEvent, {itime         :: itime(),
          chTime        :: itime(),
          evType=dst    :: eventType(),
          dstMode       :: dstMode(),
          client        :: #client{},
          subscr        :: #subscr{}
         }).


%% The purpose of a signalRecord is to represent a signal that has
%% been sent out and should not be resent.
-record(signalRecord, {evType=dst     :: eventType(),
           dstMode        :: dstMode(),
           spid=0         :: non_neg_integer()
          }).


init([]) ->
    gen_server:cast(self(), initialize),
    {ok, initializing}.


handle_call({cec_setup, _Socket},
      _From,
      #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    NextItime = timLib:nowItime(RefTs, SimFuture),
    NewState = State#state{nextItime=NextItime},
    {reply, self(), NewState, ?TIMEOUT_NOW};

handle_call({testControl, setMaxPrewarn=_Op, [Millis]},
      _From,
      #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    NewState =
  State#state{maxPrewarn=Millis,
        nextItime=timLib:nowItime(RefTs, SimFuture)},
    {reply, ok, NewState, ?TIMEOUT_NOW};

handle_call({testControl, restoreMaxPrewarn=_Op, []},
      _From,
      #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    NewState =
  State#state{maxPrewarn=?MAX_PREWARN_MILLIS,
        nextItime=timLib:nowItime(RefTs, SimFuture)},
    {reply, ok, NewState, ?TIMEOUT_NOW};

handle_call({testControl, setSimFuture=_Op, [Millis]},
      _From,
      #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    NewState =
  State#state{simFuture=Millis,
        nextItime=timLib:nowItime(RefTs, SimFuture)},
    {reply, ok, NewState, ?TIMEOUT_NOW};

handle_call({testControl, simTimestep=_Op, [IncrMillis]},
      _From,
      #state{simFuture=Millis}=State) ->
    gen_server:cast(?SERVER, {simTimestep, IncrMillis}),
    NewState = State#state{simFuture=Millis+IncrMillis, nextItime=0},
    {reply, ok, NewState, infinity};

handle_call({testControl, clearSignalRecords=_Op, []},
      _From,
      #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    NewState =
  State#state{signalRecords=orddict:new(),
        nextItime=timLib:nowItime(RefTs, SimFuture)},
    {reply, ok, NewState, ?TIMEOUT_NOW};

handle_call({testControl, setTraceLevel, [Level]}, _From, State)
  when is_integer(Level) ->
    NewState = State#state{traceLevel=Level},
    {reply, ok, NewState, ?TIMEOUT_NOW};

handle_call({testControl, stop, [Reason]}, _From, State) ->
    {stop, Reason, ok, State};

handle_call(Request, From, #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    ?WARNING("unexpected call: ~p, from: ~p", [Request, From]),
    NextItime = timLib:nowItime(RefTs, SimFuture),
    NewState = State#state{nextItime=NextItime},
    {reply, ok, NewState, ?TIMEOUT_NOW}.

handle_cast(getDstStatus, State) ->
    Ts = State#state.timeSettings,
    sysInitI:info_msg("getDstStatus itcPort ~p~n"
                      "clients ~p~n"
                      "timeSettings start~n"
                      "timeSettingsId ~p~n"
                      "daylightSavingTimeEndDate ~p~n"
                      "daylightSavingTimeOffset ~p~n"
                      "daylightSavingTimeStartDate ~p~n"
                      "gpsToUtcLeapSeconds ~p~n"
                      "gpsToUtcLeapSecondsChangeDate ~p~n"
                      "timeOffset ~p~n"
                      "timeSettings end~n"
                      "maxPrewarn ~p~n"
                      "simFuture ~p~n"
                      "refTs ~p~n",
                      [State#state.itcPort,
                       State#state.clients,
                       Ts#timeSettings.timeSettingsId,
                       Ts#timeSettings.daylightSavingTimeEndDate,
                       Ts#timeSettings.daylightSavingTimeOffset,
                       Ts#timeSettings.daylightSavingTimeStartDate,
                       Ts#timeSettings.gpsToUtcLeapSeconds,
                       Ts#timeSettings.gpsToUtcLeapSecondsChangeDate,
                       Ts#timeSettings.timeOffset,
                       State#state.maxPrewarn,
                       State#state.simFuture,
                       State#state.refTs]),

    Itime = timLib:nowItime(State#state.refTs, State#state.simFuture),

    sysInitI:info_msg("Itime ~p~n", [Itime]),

    DstStatus = getDstStatus(State#state.refDay,
                             State#state.timeSettings,
                             State#state.simFuture,
                             Itime),
    DaylightSavingTimeOn = timLib:dstStatusCode(DstStatus),

    sysInitI:info_msg("DstStatus ~p, DST On ~p~n", 
                      [DstStatus, DaylightSavingTimeOn]),
    {noreply, State};

handle_cast({cec_takeover, Socket}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_cast(timestep,
      #state{refTs=RefTs,
       simFuture=SimFuture,
       nextItime=Planned}=State) ->
    % this code is hard to test; it is however intended to behave
    % just like the more easily testable  {simTimestep, IncrMillis}
    % case below

    % time differences are independent of the SimFuture value
    % so no need for the wrapper functions here; TODO: use OTP18
    % clock functions

    Itime = timLib:nowItime(RefTs, SimFuture, Planned, "[timestep]"),
    StepMillis =
  os:system_time(milli_seconds) - erlang:system_time(milli_seconds),
    ?INFO("timestep will be handled, ms: ~w", [StepMillis]),
    try
  handleTimestep(StepMillis, Itime, State)
    catch
  ExType:ExData ->
      logCatch(handleTimestep, ExType, ExData,
         {StepMillis, Itime, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;

handle_cast({simTimestep, StepMillis},
      #state{refTs=RefTs,
       simFuture=SimFuture,
       nextItime=Planned}=State) ->
    % simulated timestep: the value returned by timLib:nowItime will
    % suddenly be larger/smaller according to the given increment.
    Itime = timLib:nowItime(RefTs, SimFuture, Planned, "[sim-timestep]"),
    try
  handleTimestep(StepMillis, Itime, State)
    catch
  ExType:ExData ->
      logCatch(handleSimTimestep, ExType, ExData,
         {StepMillis, Itime, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;

handle_cast(initialize, InitState) ->
    timLib:reportTimeProperties(),

    % Reference points for time related to server start
    {TSM, TSS, _TSU} = os:timestamp(),
    EvenSecondTimestamp = {TSM, TSS, 0},
    {Date, {H, M, S}} =
  calendar:now_to_universal_time(EvenSecondTimestamp),
    SecondInDay = H*3600 + M*60 + S,
    RefTs = (TSM*1000000 + TSS - SecondInDay)*1000,

    RefDay = calendar:date_to_gregorian_days(Date),

    {atomic, [#timeSettings{}=Record]} =
  mnesia:transaction(
    fun() -> mnesia:read(timeSettings, {"1", "1", "1"}) end),

    comsaI:subscribe_timestep(fun timLib:timestepCallback/0),

    case mnesia:subscribe({table, timeSettings, detailed}) of
  {error, Reason} ->
      ?ERROR("subscription failed, ~p", [Reason]),
      {stop, {error, {subscribe_failed, Reason}}, InitState};
  _MnesiaResult ->
      State = #state{itcPort=itc:open(?ITC_PORT_NAME),
         timeSettings=Record,
         refTs=RefTs,
         refDay=RefDay},
      {noreply, State#state{nextItime=0}, ?TIMEOUT_NOW}
    end;

handle_cast(Request, #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    ?WARNING("unexpected cast: ~p, from: ~p", [Request]),
    NextItime = timLib:nowItime(RefTs, SimFuture),
    NewState = State#state{nextItime=NextItime},
    {noreply, NewState, ?TIMEOUT_NOW}.


handle_info({tcp,
       Socket,
       <<?CEC_REQUEST_INIT_SERVICE:4/native-unsigned-integer-unit:8,
         SpidI:4/native-unsigned-integer-unit:8,
         Spid:4/native-unsigned-integer-unit:8,
         PV1:4/native-unsigned-integer-unit:8,
         PV2:4/native-unsigned-integer-unit:8,
         PV3:4/native-unsigned-integer-unit:8>>},
      State) ->
    try
  handleInitService(Socket, SpidI, Spid, PV1, PV2, PV3, State)
    catch
  ExType:ExData ->
      logCatch(handleInitService, ExType, ExData,
         {Socket, Spid, PV1, PV2, PV3, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;

handle_info({tcp,
       Socket,
       <<?CEC_REQUEST_PROCEED:4/native-unsigned-integer-unit:8,
         SpidI:4/native-unsigned-integer-unit:8>>},
      State) ->
    try
  handleProceed(Socket, SpidI, State)
    catch
  ExType:ExData ->
      logCatch(handleProceed,
         ExType, ExData, {Socket, SpidI, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;

handle_info({tcp,
       Socket,
       <<?CEC_REQUEST_TERM_SERVICE:4/native-unsigned-integer-unit:8,
         SpidI:4/native-unsigned-integer-unit:8>>},
      State) ->
    try
  handleTermService(Socket, SpidI, State)
    catch
  ExType:ExData ->
      logCatch(handleTermService, ExType, ExData,
         {Socket, SpidI, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;

handle_info({tcp,
       Socket,
       <<?CEC_REQUEST_SUBSCRIBE_DST:4/native-unsigned-integer-unit:8,
         SpidI:4/native-unsigned-integer-unit:8,
         ClientInfo:4/native-unsigned-integer-unit:8,
         PreWarningTime:4/native-unsigned-integer-unit:8>>},
      State) ->
    try
  handleSubscribeDst(Socket, SpidI, ClientInfo, PreWarningTime, State)
    catch
  ExType:ExData ->
      logCatch(handleSubscribeDst, ExType, ExData,
         {Socket, SpidI, ClientInfo, PreWarningTime, State},
         erlang:get_stacktrace()),
       {noreply, State, ?TIMEOUT_NOW}
    end;

handle_info({tcp,
       Socket,
       <<?CEC_REQUEST_SUBSCRIBE_LEAP_SEC:4/native-unsigned-integer-unit:8,
         SpidI:4/native-unsigned-integer-unit:8,
         ClientInfo:4/native-unsigned-integer-unit:8,
         PreWarningTime:4/native-unsigned-integer-unit:8>>},
      State) ->
    try
  handleSubscribeLeap(Socket, SpidI, ClientInfo, PreWarningTime, State)
    catch
  ExType:ExData ->
      logCatch(handleSubscribeLeap, ExType, ExData,
         {Socket, SpidI, ClientInfo, PreWarningTime, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;

handle_info({tcp, Socket, <<?CEC_REQUEST_SET_MAILBOX:4/native-unsigned-integer-unit:8,
          SpidI:4/native-unsigned-integer-unit:8,
          Spid:4/native-unsigned-integer-unit:8>>},
      State) ->
    try
  handleSetMailbox(Socket, SpidI, Spid, State)
    catch
  ExType:ExData ->
      logCatch(handleSetMailbox, ExType, ExData,
         {Socket, SpidI, Spid, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;

handle_info({tcp_closed, Socket}, State) ->
    ?WARNING("tcp_closed message, socket: ~p", [Socket]),
    {noreply, State, ?TIMEOUT_NOW};

handle_info({mnesia_table_event, _}=Tuple, State) ->
    try
  handleMoUpdate(Tuple, State)
    catch
  ExType:ExData ->
      logCatch(handleMoUpdate, ExType, ExData,
         {Tuple, State},
         erlang:get_stacktrace()),
      {noreply, State, ?TIMEOUT_NOW}
    end;


handle_info(timeout,
      #state{refTs=RefTs,
       simFuture=SimFuture,
       nextItime=Planned,
       traceLevel=TraceLevel}=State) ->
    try
  Itime = timLib:nowItime(RefTs, SimFuture, Planned, "[timeout]"),
  if 
      TraceLevel >= 3 ->
    error_logger:info_msg(
      "[~w] woke up, lateness: ~w~n",
      [Itime, Itime-Planned]);
      true ->
    ok
  end,
  
  {NewItime, NewTimeout, NewPlanned, NewSignalRecords} = sendSignals(State, Itime),
  NewState =
      State#state{signalRecords=NewSignalRecords, nextItime=NewPlanned},
  if 
      TraceLevel >= 3 ->    
    error_logger:info_msg(
      "[~w] go to sleep for ~w ms~n",
      [NewItime, NewTimeout]);
      true ->
    ok
  end,
  {noreply, NewState, NewTimeout}
    catch
  % impossible really to end up here since called functions do not throw
  % exceptions; log the problem as ERROR and time out in some arbitrary time
  % like one hour (alternatively consider exiting the process causing a node
  % restart)
  ExType:ExData ->
      logCatch(timeout, ExType, ExData,
         {State},
         erlang:get_stacktrace()),
      % ad-hoc timeout 1 hour
      {noreply, State, 3600*1000}
    end;


handle_info(Info, #state{refTs=RefTs, simFuture=SimFuture}=State) ->
    ?WARNING("unexpected message: ~p", [Info]),
    NewState = State#state{nextItime=timLib:nowItime(RefTs, SimFuture)},
    {noreply, NewState, ?TIMEOUT_NOW}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


handleInitService(Socket, SpidI, Spid, PV1, PV2, PV3, #state{itcPort=ItcPort,
       clients=Clients,
       nextItime=Planned,
       refTs=RefTs,
       simFuture=SimFuture}=State) ->

    respond(Socket, ?OK),

    SigNo = ?CELLO_TZII_SERVER_UP_IND,
    Data = <<>>,
    itc:send(ItcPort, Spid, SigNo, Data),
    NewClients =
  case gb_trees:lookup(SpidI, Clients) of
      {_, #client{spid=Spid}=Client} ->
    % possible if setMailbox was done before
    NewClient = Client#client{wantedPvs=[PV1, PV2, PV3]},
    gb_trees:update(SpidI, NewClient, Clients);
      none ->
    % setMailbox has not been done yet
    Client = #client{spidInitial=SpidI,
         spid=Spid,
         wantedPvs=[PV1, PV2, PV3]},
    gb_trees:enter(SpidI, Client, Clients)
  end,
    NewState =
  State#state{clients=NewClients,
        nextItime=timLib:nowItime(
        RefTs,
        SimFuture,
        Planned,
        "[init-svc]")},
    {noreply, NewState, ?TIMEOUT_NOW}.


handleProceed(Socket, SpidI, #state{itcPort=ItcPort,
       clients=Clients,
       nextItime=Planned,
       refTs=RefTs,
       simFuture=SimFuture}=State) ->
    case gb_trees:lookup(SpidI, Clients) of
  none ->
      respond(Socket, ?UNKNOWN),
      ?ERROR("unknown spid: ~p", [SpidI]),
      {noreply, State, ?TIMEOUT_NOW};
  {_, #client{spid=Spid, wantedPvs=WantedPvs}} ->
      respond(Socket, ?OK),
      case selectPv(WantedPvs, ?SUPPORTED_PVS) of
    none ->
        SigNo = ?CELLO_TZII_INITIATE_SERVICE_SUS,
        Data =
   <<?SIGNAL_REV:4/native-unsigned-integer-unit:8,
     ?CELLO_TZII_HIGHEST_PV:4/native-unsigned-integer-unit:8>>,
        itc:send(ItcPort, Spid, SigNo, Data);
    SelectedPv ->
        SigNo = ?CELLO_TZII_INITIATE_SERVICE_CFM,
        Data = <<?SIGNAL_REV:4/native-unsigned-integer-unit:8,
           SelectedPv:4/native-unsigned-integer-unit:8>>,
        itc:send(ItcPort, Spid, SigNo, Data)
      end,
      NewState =
    State#state{nextItime=timLib:nowItime(RefTs,
                  SimFuture,
                  Planned,
                  "[proceed]")},
      {noreply, NewState, ?TIMEOUT_NOW}
    end.


handleTermService(Socket, SpidI, #state{itcPort=ItcPort,
       clients=Clients,
       nextItime=Planned,
       refTs=RefTs,
       simFuture=SimFuture}=State) ->
    case gb_trees:lookup(SpidI, Clients) of
  none ->
      respond(Socket, ?UNKNOWN),
      ?WARNING("terminate: unknown spid: ~p", [SpidI]),
      {noreply, State, ?TIMEOUT_NOW};
  {_, #client{spid=Spid}} ->
      respond(Socket, ?OK),
      NewClients = gb_trees:delete(SpidI, Clients),
      SigNo = ?CELLO_TZII_TERMINATE_SERVICE_CFM,
      Data = <<>>,
      itc:send(ItcPort, Spid, SigNo, Data),
      NewState =
    State#state{clients=NewClients,
          nextItime=timLib:nowItime(RefTs,
                  SimFuture,
                  Planned,
                  "[term-svc]")},
      {noreply, NewState, ?TIMEOUT_NOW}
    end.


handleSetMailbox(Socket, SpidI, Spid, #state{clients=Clients, signalRecords=SigRecs}=State) ->
    respond(Socket, ?OK),
    case gb_trees:lookup(SpidI, Clients) of
  none ->
      NewClient = #client{spidInitial=SpidI, spid=Spid},
      NewClients = gb_trees:insert(SpidI, NewClient, Clients),
      {noreply, State#state{clients=NewClients}, ?TIMEOUT_NOW};
  {_, #client{spid=SpidPrevious}=Client} ->
      NewClients =
    gb_trees:update(SpidI, Client#client{spid=Spid}, Clients),

      % update existing signal records to look as if the new spid has
      % been used before the setMailbox call
      NewSigRecs =
    orddict:fold(
      fun(#signalRecord{spid=S}=R, V, Acc) when S =:= SpidPrevious ->
        orddict:store(R#signalRecord{spid=Spid}, V, Acc);
         (R, V, Acc) ->
        orddict:store(R, V, Acc)
      end,
      orddict:new(),
      SigRecs),
      NewState =
    State#state{clients=NewClients, signalRecords=NewSigRecs},
      {noreply, NewState, ?TIMEOUT_NOW}
    end.


handleSubscribeDst(
  Socket,
  SpidI,
  ClientInfo,
  PreWarningTime,
  #state{itcPort=ItcPort,
   clients=Clients,
   timeSettings=TimeSettings,
   maxPrewarn=MaxPrewarn,
   simFuture=SimFuture,
   refTs=RefTs,
   nextItime=Planned}=State) ->
    case gb_trees:lookup(SpidI, Clients) of
  none ->
      respond(Socket, ?UNKNOWN),
      ?ERROR("unknown spid: ~p", [SpidI]),
      {noreply, State, ?TIMEOUT_NOW};
  {_, #client{spid=Spid, subscrs=Subscrs}=Client} ->
      respond(Socket, ?OK),
      if
    PreWarningTime > MaxPrewarn ->
        % pre-warning time exceeds 23 hours, reject
        SigNo = ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ,
        Data =
    <<ClientInfo:4/native-unsigned-integer-unit:8,
      ?CELLO_TZII_PREWARNING_NOK:4/native-unsigned-integer-unit:8>>,
        itc:send(ItcPort, Spid, SigNo, Data),
        {noreply, State, ?TIMEOUT_NOW};
    true ->
        % send a confirm, provide defaults
        SigNo = ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM,
        TimeOffsetToUtc =
      try getTimeOffset(TimeSettings, undefined) of
          undefined ->
        ?CELLO_TZII_UNDEFINED;
          TimeOffsetToUtcMillis ->
        TimeOffsetToUtcMillis div (60*1000)
      catch
          throw:Diag1 ->
        ?WARNING(
            "time offset to UTC misconfigured: ~p",
            [Diag1]),
        ?CELLO_TZII_UNDEFINED
      end,
        DaylightSavingTimeOffset =
      try getDstOffset(TimeSettings, undefined) of
          undefined ->
        ?CELLO_TZII_UNDEFINED;
          DaylightSavingTimeOffsetMillis ->
        DaylightSavingTimeOffsetMillis div (60*1000)
      catch
          throw:Diag2 ->
        ?WARNING("DST offset misconfigured: ~p",
           [Diag2]),
        ?CELLO_TZII_UNDEFINED
      end,
        Itime = timLib:nowItime(RefTs, SimFuture, Planned,
              "[subscr-dst]"),
        DaylightSavingTimeOn =
      timLib:dstStatusCode(
        getDstStatus(
          State#state.refDay,
          TimeSettings,
          SimFuture,
          Itime)),
        Data =
    <<ClientInfo:4/native-unsigned-integer-unit:8,
      TimeOffsetToUtc:4/native-signed-integer-unit:8,
      DaylightSavingTimeOffset:4/native-signed-integer-unit:8,
      DaylightSavingTimeOn:4/native-unsigned-integer-unit:8>>,

        itc:send(ItcPort, Spid, SigNo, Data),

        % update the client
        NewClient =
      Client#client{subscrs=orddict:store(
            dst,
            #subscr{clientInfo=ClientInfo,
              preWarn=PreWarningTime},
            Subscrs)},
        NewClients = gb_trees:enter(SpidI, NewClient, Clients),
        NewState =
      State#state{clients=NewClients,
            nextItime=timLib:nowItime(
            RefTs, SimFuture)},
        {noreply, NewState, ?TIMEOUT_NOW}
      end
    end.


handleSubscribeLeap(
  Socket,
  SpidI,
  ClientInfo,
  PreWarningTime,
  #state{itcPort=ItcPort,
   clients=Clients,
   timeSettings=TimeSettings,
   maxPrewarn=MaxPrewarn,
   refDay=RefDay,
   refTs=RefTs,
   simFuture=SimFuture,
   nextItime=Planned}=State) ->
        case gb_trees:lookup(SpidI, Clients) of
  none ->
      respond(Socket, ?UNKNOWN),
      ?ERROR("unknown spid: ~p", [SpidI]),
      {noreply, State, ?TIMEOUT_NOW};

  {_, #client{spid=Spid, subscrs=Subscrs}=Client} ->
      respond(Socket, ?OK),
      Itime = timLib:nowItime(RefTs, SimFuture, Planned, "[subscr-leap]"),
      LeapSecondsEffective =
    getLeapSecondsEffective(TimeSettings, RefDay, Itime),
      if
    PreWarningTime > MaxPrewarn ->
        % pre-warning time exceeds 23 hours
        SigNo = ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ,
        Data =
    <<ClientInfo:4/native-unsigned-integer-unit:8,
      ?CELLO_TZII_PREWARNING_NOK:4/native-unsigned-integer-unit:8>>,
        itc:send(ItcPort, Spid, SigNo, Data),
        NewState =
      State#state{nextItime=timLib:nowItime(RefTs,
                    SimFuture)},
        {noreply, NewState, ?TIMEOUT_NOW};
    true ->
        % send a confirm
        SigNo = ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM,
        LeapSeconds =
      if
          LeapSecondsEffective =:= ?CELLO_TZII_UNDEFINED ->
        0;
          true ->
        LeapSecondsEffective
      end,
        Data = <<ClientInfo:4/native-unsigned-integer-unit:8,
           LeapSeconds:4/native-signed-integer-unit:8>>,
        itc:send(ItcPort, Spid, SigNo, Data),

        % update the client
        NewClient =
      Client#client{subscrs=orddict:store(
            leapSec,
            #subscr{clientInfo=ClientInfo,
              preWarn=PreWarningTime},
            Subscrs)},
        NewClients = gb_trees:enter(SpidI, NewClient, Clients),
        NewState =
      State#state{clients=NewClients,
            nextItime=timLib:nowItime(RefTs,
                    SimFuture)},
        {noreply, NewState, ?TIMEOUT_NOW}
      end
    end.


handleMoUpdate(
  {mnesia_table_event, Event}=Tuple,
  #state{refDay=RefDay,
   refTs=RefTs,
   simFuture=SimFuture,
   nextItime=Planned}=State) ->
        % happens anytime, so lateness will be just about anything
    Itime = timLib:nowItime(RefTs, SimFuture, Planned, "[update]"),

    % find out what has changed
    case Event of
  {write,
   timeSettings,
   #timeSettings{}=NewTS,
   [#timeSettings{}=OldTS],
   _ActId} ->

      NewDstStatus = getDstStatus(RefDay, NewTS, SimFuture, Itime),
      OldDstStatus = getDstStatus(RefDay, OldTS, SimFuture, Itime),
      DstChange = NewDstStatus =/= OldDstStatus,

      LeapSecOld = getLeapSecondsEffective(OldTS, RefDay, Itime),
      LeapSecNew = getLeapSecondsEffective(NewTS, RefDay, Itime),
      LeapChange = LeapSecNew =/= LeapSecOld,

      handleDisruption(DstChange,
           LeapChange,
           NewDstStatus,
           Itime,
           State#state{timeSettings=NewTS});
  _ ->
      ?ERROR("unexpected mnesia callback: ~p", [Tuple]),
      NewState =
    State#state{nextItime=timLib:nowItime(RefTs, SimFuture)},
      {noreply, NewState, ?TIMEOUT_NOW}
    end.



%%% ----------------------------------------------------------
%%% @doc Handle a time step. A positive StepMillis value means
%%% that the OS UTC clock suddenly returns higher values than just
%%% before. Itime is the current itime value after the step has
%%% occured.
%%% @end
%%% ----------------------------------------------------------
-spec handleTimestep(millis(), itime(), #state{}) ->
    {noreply, #state{}, timeout()}.

handleTimestep(StepMillis,
        Itime,
        #state{refDay=RefDay,
         simFuture=SimFuture,
         timeSettings=TS}=State) ->

    % regardless of positive or negative, is this a DST change?
    NewDstStatus = getDstStatus(RefDay, TS, SimFuture, Itime),
    OldDstStatus = getDstStatus(RefDay, TS, SimFuture, Itime - StepMillis),
    DstChange = NewDstStatus =/= OldDstStatus,

    NewLeapStatus = isPostLeap(RefDay, TS, Itime),
    OldLeapStatus = isPostLeap(RefDay, TS, Itime - StepMillis),
    LeapChange = NewLeapStatus =/= OldLeapStatus,
    handleDisruption(DstChange, LeapChange, NewDstStatus, Itime, State).


%%% ----------------------------------------------------------
%%% @doc Final handling of a disruption: either a MO reconfig
%%% or a time step.
%%% @end
%%% ----------------------------------------------------------
-spec handleDisruption(boolean(), boolean(), dstStatus(), itime(), #state{}) ->
    {noreply, #state{}, timeout()}.

handleDisruption(
  DstChange,
  LeapChange,
  NewDstStatus,
  Itime,
  #state{itcPort=ItcPort,
   clients=Clients,
   timeSettings=TS,
   signalRecords=SigRecs,
   refTs=RefTs,
   refDay=RefDay,
   simFuture=SimFuture}=State) ->

    SigRecs2 =
  if
      DstChange ->
    doUpdate(dst, Clients, ItcPort, TS, NewDstStatus,
       RefDay, Itime),
    removeSignalRecords(dst, SigRecs);
      not(DstChange) ->
    doCancellations(dst,
        ItcPort, Clients, TS, NewDstStatus,
        RefDay, Itime, SimFuture,
        SigRecs)
  end,

    SigRecs3 =
  if
      LeapChange ->
    doUpdate(leapSec, Clients, ItcPort, TS, NewDstStatus,
       RefDay, Itime),
    removeSignalRecords(leapSec, SigRecs2);
      not(LeapChange) ->
    doCancellations(leapSec,
        ItcPort, Clients, TS, NewDstStatus, RefDay,
        Itime, SimFuture,
        SigRecs2)
  end,

    NewState =
  State#state{signalRecords=SigRecs3,
        nextItime=timLib:nowItime(RefTs, SimFuture)},
    {noreply, NewState, ?TIMEOUT_NOW}.


%%% ----------------------------------------------------------
%%% @doc Returns a selected protocol version, or 'none'.
%%% @end
%%% ----------------------------------------------------------
-spec selectPv([protocolVersion()], [protocolVersion()]) ->
    protocolVersion()|none.

selectPv(WantedPvs, SupportedPvs) ->
    lists:foldl(
      fun(WantedPv, none) ->
        case lists:member(WantedPv, SupportedPvs) of
      false ->
          none;
      true ->
          WantedPv
        end;
   (_, Selected) ->
        Selected
      end,
      none,
      WantedPvs).


%%% ----------------------------------------------------------
%%% @doc Handle timeout. Returns a 2-tuple of timeout to be
%%% used next and an updated collection of signal records.
%%% @end
%%% ----------------------------------------------------------
-spec sendSignals(#state{}, itime()) ->
    {itime(), timeout(), itime(), orddict:orddict()}.

sendSignals(#state{signalRecords=SRR,
       itcPort=ItcPort,
       timeSettings=TS,
       clients=Clients,
       refTs=RefTs,
       refDay=RefDay,
       simFuture=SimFuture}=_State,
      Inow) ->
    % discard stale signal records
    FreshSRR =
  orddict:fold(
    fun(_K, {Itime, _ClientInfo}, Acc) when
         Itime < Inow - ?SIGNAL_REC_STALE_LIMIT ->
      Acc;
       (K, V, Acc) ->
      orddict:store(K, V, Acc)
    end,
    orddict:new(),
    SRR),

    % get an ordered set of #signalEvent{}.
    Events = getSignalEvents(Clients, TS, RefDay, Inow, SimFuture),
    {NowOrPastEvents, FutureEvents} = splitEvents(Events, Inow),

    NewSigRecs =
  sendSignalsHelper(
    NowOrPastEvents,
    FreshSRR,
    ItcPort,
    TS,
    RefDay,
    Inow,
    SimFuture),

    NewSRR =
  lists:foldl(
    fun({R, ClientInfo}, Acc) ->
      orddict:store(R, {Inow, ClientInfo}, Acc)
    end,
    FreshSRR,
    NewSigRecs),
    
    % take a new itime value here since some processing
    % time has been consumed
    NewItime = timLib:nowItime(RefTs, SimFuture),
    {NewTimeout, NextItime} =
  case ordsets:size(FutureEvents) of
      0 ->
    {?TIMEOUT_24_H, 0};
      _ ->
    [#signalEvent{itime=Y}|_] = ordsets:to_list(FutureEvents),
    {Y - NewItime, Y}
  end,
    
    LimitedTimeout = min(max(?TIMEOUT_NOW, NewTimeout), ?TIMEOUT_24_H),
    {NewItime, LimitedTimeout, NextItime, NewSRR}.


%%% ----------------------------------------------------------
%%% @doc Gets the collection of events that call for signals
%%% to be sent. The result is ordered by itime.
%%%
%%% This function does not throw exceptions.
%%% @end
%%% ----------------------------------------------------------
-spec getSignalEvents(
  gb_trees:tree(),
  #timeSettings{},
  gregDay(),
  itime(),
  millis()) ->
    ordsets:ordset(#signalEvent{}).

getSignalEvents(Clients, TS, RefDay, Inow, SimFuture) ->
    AllDstChanges = getDstChanges(RefDay, TS, SimFuture),

    % discard DstChange entries that are "in the past"
    DstChanges =
  ordsets:fold(
    fun(#dstChange{itime=I}, Acc) when Inow > I ->
      Acc;
       (C, Acc) ->
      Acc++[C]
    end,
    [],
    AllDstChanges),

    {LeapDateItime, _} =
  try
      getLeapDateItime(RefDay, TS)
  catch
      throw:leap_seconds_change_date_not_configured ->
    {undefined, none};
      throw:Other ->
    ?WARNING(
        "ill-formed gpsToUtcLeapSecondsChangeDate "
        "in TimeSettings: ~p",
                    [Other]),
    {undefined, none}
  end,

    % inspect each client, consider both kinds of subscriptions

    timLib:foldTree(
      fun(_Spid, #client{subscrs=Subscrs}=Client, Acc) ->
        orddict:fold(
    fun(dst, _, A) when DstChanges =:= [] ->
      A;
       (dst, #subscr{preWarn=PreWarningTime}=Su, A) ->
      [#dstChange{itime=DstChangeItime, type=T}|_] =
          DstChanges,
      ordsets:add_element(
        #signalEvent{itime=DstChangeItime -
             PreWarningTime,
               chTime=DstChangeItime,
               evType=dst,
               dstMode=T,
               client=Client,
               subscr=Su}, A);
       (leapSec, #subscr{preWarn=PreWarningTime}=Su, A) ->
      if
          LeapDateItime =:= undefined ->
        A;
          true ->
        ordsets:add_element(
          #signalEvent{itime=LeapDateItime -
               PreWarningTime,
                 chTime=LeapDateItime,
                 evType=leapSec,
                 client=Client,
                 subscr=Su}, A)
      end
    end,
    Acc,
    Subscrs)
      end,
      ordsets:new(),
      Clients).


%%% ----------------------------------------------------------
%%% @doc Splits the given ordset in two: Past-or-now events and
%%% future events.
%%%
%%% This function does not throw exceptions.
%%% @end
%%% ----------------------------------------------------------
-spec splitEvents(ordsets:ordset(#signalEvent{}), itime()) ->
    {ordsets:ordset(#signalEvent{}), ordsets:ordset(#signalEvent{})}.

splitEvents(Events, Inow) ->
    ordsets:fold(
      fun(#signalEvent{itime=Itime}=E, {PON, FUT}) ->
        if
      Inow >= Itime ->
          {ordsets:add_element(E, PON), FUT};
      true ->
          {PON, ordsets:add_element(E, FUT)}
        end
      end,
      {ordsets:new(), ordsets:new()},
      Events).


%%% ----------------------------------------------------------
%%% @doc Send signals according to the given events, except
%%% if it is believed that an unwanted resend would occur.
%%% Returns an updated collection of signal records.
%%%
%%% Special case: The leap seconds update: Just update the TS MO
%%% and timeout zero. TODO, for testability it will be necessary
%%% to have a way to "make this happen anytime".
%%% @end
%%% ----------------------------------------------------------
-spec sendSignalsHelper(
  ordsets:ordset(#signalEvent{}),
  orddict:orddict(),
  port(),
  #timeSettings{},
  gregDay(),
  itime(),
  millis()) ->
    [{#signalRecord{}, clientInfo()}].

sendSignalsHelper(Events, FreshSRR, ItcPort, TS, RefDay, Inow, SimFuture) ->
    NewSignalRecs =
  ordsets:fold(
    fun(#signalEvent{evType=dst,
         dstMode=DstMode,
         client=#client{spid=Spid},
         subscr=#subscr{clientInfo=ClientInfo},
         chTime=ChTime}, AccList) ->
      % DST event
      CauseForChange = ?CELLO_TZII_PREWARNING_START,
      SignalRec = #signalRecord{evType=dst,
              dstMode=DstMode,
              spid=Spid},
      case orddict:is_key(SignalRec, FreshSRR) of
          true ->
        AccList;
          false ->
        DstStatus =
            proplists:get_value(
        getDstStatus(RefDay, TS, SimFuture, Inow),
        [{on, off}, {off, on}]),
        if
            Inow - ChTime > ?PREWARN_OVERDUE_LIMIT ->
          AccList;
            true ->
          case sendDstInd(
           ItcPort,
           Spid,
           ClientInfo,
           TS,
           CauseForChange,
           DstMode,
           DstStatus) of
              false ->
            AccList;
              true ->
            [{SignalRec, ClientInfo}|AccList]
          end
        end
      end;
       (#signalEvent{evType=leapSec,
         client=#client{spid=Spid},
         subscr=#subscr{clientInfo=ClientInfo},
         chTime=ChTime}, AccList) ->
      % LEAP event
      Cause = ?CELLO_TZII_PREWARNING_START,
      SignalRec = #signalRecord{evType=leapSec, spid=Spid},
      case orddict:is_key(SignalRec, FreshSRR) of
          true ->
        % nothing to send
        AccList;
          false ->
        % do not prewarn if the leap has happened "long ago"
        if
            Inow - ChTime > ?PREWARN_OVERDUE_LIMIT ->
          AccList;
            true ->
          sendLeapInd(
            ItcPort,
            Spid,
            ClientInfo,
            TS,
            Cause,
            RefDay,
            Inow),
          [{SignalRec, ClientInfo}|AccList]
        end
      end
    end,
    [],
    Events),
    NewSignalRecs.


%%% ----------------------------------------------------------
%%% @doc Send a DST indication.
%%%
%%% The DstMode parameter is only used if Cause =:= _START.
%%%
%%% The DstStatus parameter is only used if Cause =:= _UPDATE
%%%
%%% This function does not throw exceptions.
%%% @end
%%% ----------------------------------------------------------
-spec sendDstInd(port(),
     spid(),
     clientInfo(),
     #timeSettings{},
     cause(),
     dstMode(),
     dstStatus()) ->
    boolean().

sendDstInd(ItcPort, Spid, ClientInfo, TS,
     CauseForChange, DstMode, DstStatus) ->
    try
  TimeOffsetToUtc =
      case getTimeOffset(TS, undefined) of
    undefined ->
        ?CELLO_TZII_UNDEFINED;
    TimeOffsetMillis ->
        TimeOffsetMillis div (60*1000)
      end,
  DaylightSavingTimeOffset =
      case getDstOffset(TS, undefined) of
    undefined ->
        ?CELLO_TZII_UNDEFINED;
    DstOffsetMillis ->
        DstOffsetMillis div (60*1000)
      end,
  DaylightSavingTimeOn =
      if
    CauseForChange =:= ?CELLO_TZII_PREWARNING_START ->
        if DstMode =:= dstStart -> 1; true -> 0 end;
    CauseForChange =:= ?CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE orelse
        CauseForChange =:= ?CELLO_TZII_PREWARNING_CANCEL ->
        % use the current DST value
        timLib:dstStatusCode(DstStatus)
      end,
  TimeOfChange =
      if
    CauseForChange =/= ?CELLO_TZII_PREWARNING_START ->
        0;
    true ->
        Rule =
      if
          DstMode =:= dstStart ->
        TS#timeSettings.daylightSavingTimeStartDate;
          true ->
        TS#timeSettings.daylightSavingTimeEndDate
      end,
        case getDstTime(Rule) of
      {utc, UtcMillis} ->
          LO = getTimeOffset(TS, zero),
          LstMillis = UtcMillis + LO,
          if
        LstMillis < 0 ->
            (LstMillis + 24*3600*1000) div (60*1000);
        LstMillis >= 24*3600*1000 ->
            (LstMillis - 24*3600*1000) div (60*1000);
        true ->
            LstMillis div (60*1000)
          end;
      {local, 86400000} ->
          % freak case according to NDS 7.12.1
          24*60;
      {local, LocalMillis} ->
          LocalMillis div (60*1000)
        end end,
  SigNo = ?CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND,
  Data = <<ClientInfo:4/native-unsigned-integer-unit:8,
     TimeOffsetToUtc:4/native-signed-integer-unit:8,
     DaylightSavingTimeOffset:4/native-signed-integer-unit:8,
     DaylightSavingTimeOn:4/native-unsigned-integer-unit:8,
     TimeOfChange:4/native-unsigned-integer-unit:8,
     CauseForChange:4/native-unsigned-integer-unit:8>>,
  itc:send(ItcPort, Spid, SigNo, Data),
  true
    catch
  throw:Reason ->
      ?WARNING("incomplete or incorrect TimeSettings: ~p", [Reason]),
      false;
  ExType:ExData ->
      logCatch(sendDstInd, ExType, ExData,
         {ItcPort, Spid, ClientInfo, TS, CauseForChange,
          DstMode, DstStatus}, erlang:get_stacktrace()),
      false
    end.


%%% ----------------------------------------------------------
%%% @doc Send a leap seconds indication.
%%%
%%% This function does not throw exceptions.
%%% @end
%%% ----------------------------------------------------------
-spec sendLeapInd(port(),
      spid(),
      clientInfo(),
      #timeSettings{},
      cause(),
      gregDay(),
      itime()) ->
    ok.

sendLeapInd(ItcPort, Spid, ClientInfo, TS, Cause, RefDay, Itime) ->
    try
  SigNo = ?CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND,
  LeapSeconds = getLeapSeconds(TS),
  if
      LeapSeconds =:= ?CELLO_TZII_UNDEFINED ->
    ?ERROR("gpsToUtcLeapSeconds not configured in TimeSettings",
           []),
    ok;
      true ->
    LS =
        if
      Cause =:= ?CELLO_TZII_PREWARNING_START ->
          % report the post-leap value even though
          % the leap has not yet happened
          {_, Increment} = getLeapDateItime(0, TS),
          LeapSeconds + Increment;
      true ->
          getLeapSecondsEffective(TS, RefDay, Itime)
        end,

    Data = <<ClientInfo:4/native-unsigned-integer-unit:8,
       LS:4/native-signed-integer-unit:8,
       Cause:4/native-unsigned-integer-unit:8>>,
    itc:send(ItcPort, Spid, SigNo, Data),
    ok
  end
    catch
  ExType:ExData ->
      logCatch(
        sendLeapInd, ExType, ExData,
        {ItcPort, Spid, ClientInfo, TS, Cause, RefDay, Itime},
        erlang:get_stacktrace()),
      ok
    end.


%%% ----------------------------------------------------------
%%% @doc Get the effective leap seconds value.
%%% @end
%%% ----------------------------------------------------------
-spec getLeapSecondsEffective(#timeSettings{}, gregDay(), itime()) ->
    leapSeconds()|?CELLO_TZII_UNDEFINED.

getLeapSecondsEffective(TS, RefDay, Itime) ->
    case getLeapSeconds(TS) of
  ?CELLO_TZII_UNDEFINED ->
      ?CELLO_TZII_UNDEFINED;
  LeapSeconds ->
      try getLeapDateItime(RefDay, TS) of
    {LeapItime, Incr} ->
        if
      Itime > LeapItime ->
          LeapSeconds + Incr;
      true ->
          LeapSeconds
        end
      catch
    throw:leap_seconds_change_date_not_configured ->
        LeapSeconds
      end
    end.


%%% ----------------------------------------------------------
%%% @doc Get the leap seconds value as seconds, reading the
%%% gpsToUtcLeapSeconds attribute value. If a value has not been
%%% configured then CELLO_TZII_UNDEFINED is returned.
%%% @end
%%% ----------------------------------------------------------
-spec getLeapSeconds(#timeSettings{}) ->
                        leapSeconds()|?CELLO_TZII_UNDEFINED.

getLeapSeconds(#timeSettings{gpsToUtcLeapSeconds=undefined}) ->
    ?CELLO_TZII_UNDEFINED;

getLeapSeconds(#timeSettings{gpsToUtcLeapSeconds=LS}) ->
    LS.


%%% ----------------------------------------------------------
%%% @doc Returns 'pre' if current itime is before a configured
%%% leap, or 'post' if current itime is after a configured
%%% leap, or 'undefined' if no leap date is configured.
%%% @end
%%% ----------------------------------------------------------
-spec isPostLeap(gregDay(), #timeSettings{}, itime()) ->
    pre|post|undefined.

isPostLeap(RefDay, TS, Itime) ->
    try
  L = getLeapDateItime(RefDay, TS),
  if
      Itime < L ->
    pre;
      true ->
    post
  end
    catch
  throw:leap_seconds_change_date_not_configured ->
      undefined
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the leap seconds change date as an itime value.
%%% The returned value is {Itime, Increment} where Increment
%%% is typically 1 but would be -1 if a leading minus sign is
%%% present in the MO attribute value.
%%%
%%% Throws: leap_seconds_change_date_not_configured,
%%% {bad_leap_seconds_change_date, String}
%%% @end
%%% ----------------------------------------------------------
-spec getLeapDateItime(gregDay(), #timeSettings{}) ->
    {itime(), increment()}.

getLeapDateItime(RefDay, #timeSettings{gpsToUtcLeapSecondsChangeDate=CD}) ->
    if
  CD =:= undefined ->
      throw(leap_seconds_change_date_not_configured);
  true ->
      case re:run(CD, <<"^(-?)([2-9][0-9][0-9][0-9])-"
            "([0-9][0-9])-([0-9][0-9])$">>, []) of
    nomatch ->
        throw({bad_leap_seconds_change_date, CD});
    {match, [{_, _}, {_SP, SL}, {YP, YL}, {MP, ML}, {DP, DL}]} ->
        Increment = if SL =:= 1 -> -1; true -> 1 end,
        Ystr = string:substr(CD, YP+1, YL),
        Mstr = string:substr(CD, MP+1, ML),
        Dstr = string:substr(CD, DP+1, DL),
        LeapDay =
      try
          calendar:date_to_gregorian_days(
            list_to_integer(Ystr),
            list_to_integer(Mstr),
            list_to_integer(Dstr))
      catch
          _:_ ->
        throw({bad_leap_seconds_change_date, CD})
      end,
        Itime =
      (LeapDay - RefDay)*24*3600*1000 +
          (23*3600 + 59*60 + 59)*1000,
        {Itime, Increment}
      end
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the current DST status. Scan all DST change entries
%%% up to the first one that is ahead in time.
%%%
%%% This function does not throw exceptions.
%%% @end
%%% ----------------------------------------------------------
-spec getDstStatus(gregDay(), #timeSettings{}, millis(), itime()) ->
    dstStatus().

getDstStatus(RefDay, TimeSettings, SimFuture, Itime) ->
    Changes = getDstChanges(RefDay, TimeSettings, SimFuture),
    try
  ordsets:fold(
    fun(#dstChange{itime=EventTime, type=dstStart}, undefined) when
         EventTime =< Itime ->
      on;
       (#dstChange{itime=EventTime, type=dstEnd}, undefined) when
         EventTime =< Itime ->
      off;
       (#dstChange{itime=EventTime, type=dstStart}, off) when
         EventTime =< Itime ->
      on;
       (#dstChange{itime=EventTime, type=dstEnd}, on) when
         EventTime =< Itime ->
      off;
       (#dstChange{itime=EventTime, type=Type}, PrevStat) when
         EventTime =< Itime ->
      ?WARNING("inconsistent DST settings: ~p",
         [{Itime, PrevStat, Changes}]),
      if
          Type =:= dstStart ->
        on;
          Type =:= dstEnd ->
        off
      end;
       (#dstChange{itime=EventTime, type=dstStart}, undefined) when
         EventTime > Itime ->
      throw(off);
       (#dstChange{itime=EventTime, type=dstEnd}, undefined) when
         EventTime > Itime ->
      throw(on);
       (#dstChange{itime=EventTime, type=dstStart}, off) when
         EventTime > Itime ->
      throw(off);
       (#dstChange{itime=EventTime, type=dstEnd}, on) when
         EventTime > Itime ->
      throw(on);
       (#dstChange{itime=EventTime, type=Type}, PrevStat) when
         EventTime > Itime ->
      ?WARNING("inconsistent DST settings: ~p",
         [{Itime, PrevStat, Changes}]),
      throw(if
          Type =:= dstStart ->
        off;
          Type =:= dstEnd ->
        on end)
    end,
    undefined,
    Changes)
    catch
  throw: Result ->
      Result
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the schedule of DST change events. The returned
%%% value is an ordset of 4 #dstChange{} records, or an empty
%%% ordset if DST is not configured or incorrectly configured.
%%%
%%% This function does not throw exceptions.
%%% @end
%%% ----------------------------------------------------------

-spec getDstChanges(gregDay(), #timeSettings{}, millis()) ->
    ordsets:ordset(#dstChange{}).

getDstChanges(_RefDay,
        #timeSettings{daylightSavingTimeStartDate=undefined,
          daylightSavingTimeEndDate=undefined},
        _SimFuture) ->
    % concluding that DST is not configured
    ordsets:new();

getDstChanges(_RefDay,
        #timeSettings{daylightSavingTimeStartDate=undefined},
        _SimFuture) ->
    ?WARNING("DST start date not defined", []),
    ordsets:new();

getDstChanges(_RefDay,
        #timeSettings{daylightSavingTimeEndDate=undefined},
        _SimFuture) ->
    ?WARNING("DST end date not defined", []),
    ordsets:new();

getDstChanges(RefDay,
        #timeSettings{daylightSavingTimeStartDate=StartDstRule,
          daylightSavingTimeEndDate=EndDstRule}=TS,
        SimFuture) ->
    case getTimeOffset(TS, undefined) of
  undefined ->
      ordsets:new();
  TimeOffset ->
      {{CurrentYear, _, _}, _} = timLib:universal_time(SimFuture),
      try
    lists:foldl(
      fun({Year, Rule, X}, Acc) ->
        Day = getGregorianDay(Rule, Year),
        {Mode, TimeInDay} = getDstTime(Rule),
              Itime = 
        if
            Mode =:= utc ->
          (Day - RefDay)*24*3600*1000 +
                TimeInDay;
            Mode =:= local ->
          (Day - RefDay)*24*3600*1000 +
                TimeInDay -
                TimeOffset
        end,
        ordsets:add_element(
          #dstChange{type=X, itime=Itime},
          Acc)
      end,
      ordsets:new(),
      [{CurrentYear, StartDstRule, dstStart},
       {CurrentYear, EndDstRule, dstEnd},
       {CurrentYear + 1, StartDstRule, dstStart},
       {CurrentYear + 1, EndDstRule, dstEnd}])
      catch
    throw:Any ->
        ?ERROR("DST incorrectly configured "
           "in TimeSettings: ~p",
           [Any]),
        ordsets:new();
    ExType:ExData ->
        % impossible really
        logCatch(
          getDstChanges, ExType, ExData,
          {RefDay, TS, SimFuture, TimeOffset},
          erlang:get_stacktrace()),
        ordsets:new()
      end
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the gregorian day number from the date rule,
%%% assuming the given year.
%%%
%%% If the date is given as year, month, day where month is
%%% 2 and day is 29 and the year is not a leap year, the
%%% day number is calculated as the previous day number plus 1.
%%%
%%% If parsing fails a {bad_day_rule, _} tuple is thrown.
%%% @end
%%% ----------------------------------------------------------
-spec getGregorianDay(dstRule(), year()) ->
    gregDay().

getGregorianDay(#'DstRule'{month=Month, dayRule=DR}=R, Year) ->
    case re:run(DR, ?RE_DAY_RULE_1) of
  {match, _} ->
      Day = list_to_integer(DR),
      try
    calendar:date_to_gregorian_days(Year, Month, Day)
      catch
    error: _ ->
        IsLeapYear = calendar:is_leap_year(Year),
        if
      Month =:= 2 andalso Day =:= 29 andalso not(IsLeapYear) ->
          1 + calendar:date_to_gregorian_days(
             Year, Month, Day - 1);
      true ->
          throw({bad_day_rule, {Year, R}})
        end
      end;
  nomatch ->
      case re:run(DR, ?RE_DAY_RULE_2) of
    {match, [_, _, {DOWStart, DOWLen}]} ->
        DOWStr = string:to_upper(string:substr(DR, DOWStart + 1, DOWLen)),
        DOW = dayOfTheWeek(Year, R, DOWStr),
        LDM = calendar:last_day_of_the_month(Year, Month),
        DOWLDM = calendar:day_of_the_week(Year, Month, LDM),
        Day = LDM - ((DOWLDM - DOW + 7) rem 7),
        calendar:date_to_gregorian_days(Year, Month, Day);
    nomatch ->
        case re:run(DR, ?RE_DAY_RULE_3) of
          {match, [_, {DayNameStart, DayNameLen}, {DayStart, DayLen}]} ->
            DOWStr = string:to_upper(
                            string:substr(DR, DayNameStart + 1, DayNameLen)),
            DOW = dayOfTheWeek(Year, R, DOWStr),
            DayMinStr = string:strip(string:substr(DR, DayStart + 1, DayLen)),
            DayMin = list_to_integer(DayMinStr),
            DOWMIN = calendar:day_of_the_week(Year, Month, DayMin),
            Day = DayMin + ((DOW - DOWMIN + 7) rem 7),
            try
              % may fail if the right-hand side value of
              % the rule is too large
              calendar:date_to_gregorian_days(Year, Month, Day)
            catch
              _:_ ->
                  throw({bad_day_rule, {Year, R}})
            end;
          _ ->
            throw({bad_day_rule, {Year, R}})
        end
      end
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the day-in-week number. The third parameter
%%% is NOT validated by the MOM.
%%% @end
%%% ----------------------------------------------------------
-spec dayOfTheWeek(non_neg_integer(), dstRule(), string()) ->
    dayInWeek().

dayOfTheWeek(_Y, _R, "SAT") -> 6;
dayOfTheWeek(_Y, _R, "SUN") -> 7;
dayOfTheWeek(_Y, _R, "FRI") -> 5;
dayOfTheWeek(_Y, _R, "THU") -> 4;
dayOfTheWeek(_Y, _R, "WED") -> 3;
dayOfTheWeek(_Y, _R, "TUE") -> 2;
dayOfTheWeek(_Y, _R, "MON") -> 1;
dayOfTheWeek(Year, Rule, _) ->
    throw({bad_day_rule, {Year, Rule}}).


%%% ----------------------------------------------------------
%%% @doc Convert the time field in the given rule to a milliseconds
%%% value. The highest allowed value is the one corresponding to
%%% "24:00".
%%%
%%% The returned value is a tuple {TzType, Milliseconds}, where
%%% TzType is 'utc' or 'local'. This reflects the presence of an
%%% optional appended 'u' for UTC in the attribute value.
%%% NOTE: The 'u' syntax is not supported in the MOM currently.
%%% @end
%%% ----------------------------------------------------------
-spec getDstTime(dstRule()) ->  {tzType(), millis()}.

getDstTime(#'DstRule'{time=HhMm}) ->
    case re:run(HhMm, <<"^[0-9][0-9]:[0-9][0-9]u?$">>, []) of
  nomatch ->
      throw({bad_time, HhMm});
  {match, [{0, Length}|_]} ->
      Hh = list_to_integer(string:substr(HhMm, 1, 2)),
      Mm = list_to_integer(string:substr(HhMm, 4, 2)),
      Millis = (Hh*3600 + Mm*60)*1000,
      if
    Millis > 24*3600*1000 ->
        throw({too_large_time, HhMm});
    Length =:= 6 ->
        {utc, Millis};
    true ->
        {local, Millis}
      end
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the time offset (non-DST difference between
%%% local time and UTC, positive in Eastern countries) as
%%% milliseconds.
%%%
%%% The second argument may be 'undefined' or 'zero' and
%%% specifies what the default should be if the attribute
%%% is not yet configured.
%%%
%%% If no value is configured then 0 is returned.
%%% This behaviour could be modeled by MOM design instead.
%%%
%%% No sanity check of the value is made here. Currently a
%%% value such as +24:00 can be configured in CLI. Checking
%%% could be added in timModel:setMoAttribute* (use e g LMA
%%% as a pattern for how to do it).
%%%
%%% This function does not throw exceptions since the
%%% attribute value is strictly prescribed by the MOM.
%%% ----------------------------------------------------------
-spec getTimeOffset(#timeSettings{}, undefined|zero) -> millis()|undefined.

getTimeOffset(#timeSettings{timeOffset=undefined}, undefined) ->
    undefined;

getTimeOffset(#timeSettings{timeOffset=undefined}, zero) ->
    0;

getTimeOffset(#timeSettings{timeOffset=TimeOffset}, _) ->
    Hh = list_to_integer(string:substr(TimeOffset, 2, 2)),
    Mm = list_to_integer(string:substr(TimeOffset, 5, 2)),
    Sign =  case TimeOffset of [$-|_] -> -1; [$+|_] -> 1 end,
    Sign*(Hh*3600 + Mm*60)*1000.


%%% ----------------------------------------------------------
%%% @doc Gets the DST offset attribute value as milliseconds.
%%% The returned value is non-negative.
%%% @end
%%% ----------------------------------------------------------
-spec getDstOffset(#timeSettings{}, any()) -> millis()|undefined.

getDstOffset(#timeSettings{daylightSavingTimeOffset=undefined}, undefined) ->
    undefined;

getDstOffset(#timeSettings{daylightSavingTimeOffset=DstOffset}, _) ->
    case re:run(DstOffset, <<"^[0-9]:[0-9][0-9]$">>, []) of
  nomatch ->
      throw({bad_dst_offset, DstOffset});
  {match, _} ->
      H = list_to_integer(string:substr(DstOffset, 1, 1)),
      Mm = list_to_integer(string:substr(DstOffset, 3, 2)),
      (H*3600 + Mm*60)*1000
    end.


%%% ----------------------------------------------------------
%%% @doc Remove signal records of the given event type.
%%% @end
%%% ----------------------------------------------------------
-spec removeSignalRecords(eventType(), orddict:orddict()) -> orddict:orddict().

removeSignalRecords(EvType, SigRecs) ->
    orddict:fold(
      fun(#signalRecord{evType=ET}, _V, Acc) when ET =:= EvType ->
        Acc;
   (K, V, Acc) ->
        orddict:store(K, V, Acc)
      end,
      orddict:new(),
      SigRecs).


%%% ----------------------------------------------------------
%%% @doc Scan all DST subscriptions and send INDs to indicate
%%% MO data change. A collection of signal records is returned.
%%% @end
%%% ----------------------------------------------------------
-spec doUpdate(
  eventType(),
  gb_trees:tree(),
  port(),
  #timeSettings{},
  on|off|undefined,
  gregDay(),
  itime()) -> ok.

doUpdate(
  EvType,
  Clients,
  ItcPort,
  NewTS,
  DstStatus,
  RefDay,
  Inow) ->
    timLib:foldTree(
      fun(_Socket, #client{spid=Spid, subscrs=Subscrs}, _) ->
        orddict:fold(
    fun(dst, #subscr{clientInfo=ClientInfo}, _) when
         EvType =:= dst ->
      sendDstInd(
        ItcPort,
        Spid,
        ClientInfo,
        NewTS,
        ?CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE,
        dstStart,  % ignored value
        DstStatus),
      ok;
       (leapSec, #subscr{clientInfo=ClientInfo}, _) when
         EvType =:= leapSec ->
      sendLeapInd(
        ItcPort,
        Spid,
        ClientInfo,
        NewTS,
        ?CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE,
        RefDay,
        Inow),
      ok;
       (_, _, _) ->
      ok
    end,
    ok,
    Subscrs)
      end,
      ok,
      Clients).


%%% ----------------------------------------------------------
%%% @doc Perform cancellations as needed.
%%% @end
%%% ----------------------------------------------------------
-spec doCancellations(
  eventType(),
  port(),
  gb_trees:tree(),
  #timeSettings{},
  dstStatus(),
  gregDay(),
  itime(),
  millis(),
  orddict:orddict()) ->
    orddict:orddict().

doCancellations(EvType,
    ItcPort,
    Clients,
    TS,
    DstStatus,
    RefDay,
    Itime,
    SimFuture,
    SigRecs) ->
    SigEvents = getSignalEvents(Clients, TS, RefDay, Itime, SimFuture),
    {_NowOrPast, Upcoming} = splitEvents(SigEvents, Itime),
    ordsets:fold(
      fun(#signalEvent{evType=ET,
           dstMode=DstMode,
           client=#client{spid=Spid}}, Acc) when ET =:= EvType ->
        R = #signalRecord{evType=EvType,
        dstMode=DstMode,
        spid=Spid},
        case orddict:find(R, Acc) of
      error ->
          Acc;
      {ok, {_, ClientInfo}} ->
          % send a 'cancel'
          if
        EvType =:= dst ->
            sendDstInd(
        ItcPort,
        Spid,
        ClientInfo,
        TS,
        ?CELLO_TZII_PREWARNING_CANCEL,
        dstStart,        % the value is actually ignored
        DstStatus);
        EvType =:= leapSec ->
            sendLeapInd(
        ItcPort,
        Spid,
        ClientInfo,
        TS,
        ?CELLO_TZII_PREWARNING_CANCEL,
        RefDay,
        Itime)
          end,
          orddict:erase(R, Acc)
        end;
   (_, Acc) ->
        Acc
      end,
      SigRecs,
      Upcoming).


%%% ----------------------------------------------------------
%%% @doc Respond to a client request.
%%% @end
%%% ----------------------------------------------------------
-spec respond(port(), binary()) ->  ok.

respond(Socket, Message) ->
    inet:setopts(Socket, [{active, once}]),
    case gen_tcp:send(Socket, Message) of
  {error, Reason} ->
      throw({error, {tcp, Reason}});
  ok->
      ok
    end,
    gen_tcp:close(Socket),
    ok.


%%% ----------------------------------------------------------
%%% @doc Log info that was caught due to an exception.
%%% @end
%%% ----------------------------------------------------------

logCatch(Context, ExType, ExData, Data, Stack) ->
    sysInitI:error_report(
      [{context, Context},
       {exception, {ExType, ExData}},
       {data, Data},
       {stack, Stack}
      ]).

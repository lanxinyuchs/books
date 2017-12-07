%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaEvent.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/4

%%% @doc ==Event sending==
%%% Generic support for netconf notifications from the mnesia domain, which
%%% means model information stored by use of comsaGeneric or similar.
%%%
%%% Also supports AVC notifications from IMM instances.
%%% @end

-module(comsaEvent).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/4').
-date('2017-10-19').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-12-16 etxjotj     Created
%%% R2A/8      2014-01-08 etxpeno     Add wrapper healthCheck()
%%% R2A/10     2014-01-21 erarafo     #state.comTimer format fixed
%%% R2A/11     2014-01-29 erarafo     Pre-release of AVC support
%%% R2A/16     2014-02-10 erarafo     Pre-release of AVC support, improved
%%% R2A/17     2014-02-11 erarafo     Exception handling ensured for AVC
%%% R2A/18     2014-02-11 erarafo     Cleaned up attribute type conversion
%%% R2A/19     2014-02-12 etxarnu     Only call comte:notify if enabled
%%% R2A/20     2014-03-01 etxarnu     Enabled comte:notify again
%%% R2A/21     2014-03-02 etxarnu     Disabled comte:notify again
%%% R2A/22     2014-03-02 etxjotj     make_attribute_list fixed
%%% R2A/23     2014-03-02 etxarnu     Enabled comte:notify again
%%% R2A/24     2014-03-02 etxarnu     Enabled comte:notify again
%%% R2A/27     2014-03-25 erarafo     AVC support, adjustments
%%% R2A/28     2014-03-26 erarafo     Refactoring
%%% R2A/29     2014-03-27 erarafo     Minor correction
%%% R2A/31     2014-03-31 erarafo     Fixed case in faulty string
%%% R2A/32     2014-05-14 etxjotj
%%% R2A/33     2014-05-23 erarafo     Dialyzer fault fixed
%%% R2A/34     2014-05-26 erarafo     HS58831, workaround undone
%%% R2A/35     2014-06-02 etxjotj     Workaround until HS66142 is fixed in safs
%%% R2A/36     2014-07-17 etxberb     TR HS79504: Include all attributes in
%%%                                   notification regardless of read / write
%%%                                   permission.
%%% R2A/37     2014-08-04 etxjotj     Sequence of strings handling
%%% R2A/38     2014-08-07 etxjotj     Dont print error reports on comte errors
%%%                                   until 2014-09-01
%%% R2A/39     2014-08-20 etxjotj     HS88484: Buffer events before COM START
%%% R2A/40     2014-08-20 etxpeno     Corrected EcimPath in handle_safs_notification/1
%%% R2A/41     2014-08-20 erarafo     No code change, purpose is block re-release
%%% R2A/42     2014-08-21 erarafo     Adaptation to current COMTE and COM
%%% R2A/43     2014-08-21 erarafo     Identical to R2A/41 (CNX9012610-R2A322),
%%%                                   for RFA use. After RFS step up to R2A/42
%%%                                   again, which has the correct behavior.
%%% R2A/44     2014-08-25 erarafo     Equal to R2A/42 with some cleanup
%%% R2A/45     2014-09-02 etxjotj     Handle COM shutdown
%%% R2A/46     2014-09-02 etxjotj     Take 2
%%% R2A/47     2014-09-04 etxjotj     HS88904 internal events
%%% R2A/48     2014-09-04 etxjotj     Take 2
%%% R2A/50     2014-09-17 etxarnu     Changed to WARNING when failed to send
%%%                                   event due to no_registered_consumers
%%% R3A/3      2014-11-28 etxberb     Added is_FmAlarm/1.
%%% R3A/4      2015-02-16 etxberb     Added function clauses in
%%%                                   handle_mnesia_table_event/1 to handle
%%%                                   mnesia:clear_table events.
%%% R3A/5      2015-03-11 erarafo     Solving HT52686
%%% R3A/6      2015-03-11 erarafo     Handling of booleans, workaround
%%% R3A/7      2015-03-13 erarafo     Extended set of IMM reserved attribute names
%%% R3A/8      2015-03-16 erarafo     COMTE notification error mapped to warning
%%% R3A/9      2015-03-26 etxjotj     No warning for no_registered_consumers
%%% R3A/10     2015-04-14 erarafo     More details in surprising WARNING report
%%% R3A/11     2015-04-29 erarafo     Ignore AVC handling faults (merge from R4A/1)
%%% R3A/12     2015-05-18 etxpeno     IMM notifications handled using COI (merge from R4A/2)
%%% R3A/14     2015-05-26 etxarnu     Removed undefined attributes in generate_event
%%% R4A/3      2015-05-18 erarafo     Edit on behalf of etxjotj
%%% R4A/4      2015-05-18 erarafo     Mnesia notifications problem fixed
%%% R4A/5      2015-05-21 etxpejn     comsaEvent only started on active core MP
%%% R4A/6      2015-05-27 etxarnu     merged with  R3A/14
%%% R4A/7      2015-06-02 etxarnu     no call to deprecated function in clhI
%%% R4A/8      2015-06-03 erarafo     Solution for "ambiguous class" problem with AVCs
%%% R4A/9      2015-06-04 etxarnu     More places to remove undefined attributes
%%% R4A/10     2015-06-11 erarafo     Fixing a coding fault in the R4A/8 solution
%%% R4A/11     2015-06-16 erarafo     Events buffered in ets table
%%% R4A/12     2015-06-17 erarafo     Cleanup after R4A/11
%%% R4A/13     2015-06-17 erarafo     Spec adjusted
%%% R4A/14     2015-06-22 erarafo     Logging improvements
%%% R4A/18     2015-07-08 etxpeno     remove support for AVCs from structs
%%% R4A/19     2015-08-21 erarafo     Reverted to R4A/14 after fixing TESTMOM
%%% R4A/20     2015-08-25 erarafo     HT96596 solution
%%% R4A/21     2015-08-25 erarafo     slight refactoring
%%% R4A/22     2015-08-27 etxjotj     Changed typing for structs
%%% R4A/23     2015-08-28 etxjotj     More adjustments for struct types
%%% R4A/24     2015-08-28 erarafo     Adjustment for seq of struct
%%% R4A/25     2015-08-29 erarafo     Adjustment for all sequence types
%%% R4A/27     2015-09-06 erarafo     Extra MOM check for struct attributes
%%% R4A/28     2015-09-07 etxjotj     Handle MOM check without MOM available
%%% R4A/29     2015-09-07 erarafo     Handle MOM check without MOM available
%%% R4A/30     2015-09-29 etxjotj     Sending 'unset' events for mnesia domain
%%% R4A/31     2015-10-21 erarafo     Checking COM status in case of restart
%%% R4A/33     2015-11-27 erarafo     Tolerate improper list from COI (workaround)
%%% R4A/34     2015-11-29 erarafo     Handle coi:getMoAttributes/3 -> undefined
%%% R5A/2      2015-11-06 erarafo     Merge from R4A/32: Downgrading an AVC failure to WARNING
%%% R5A/3      2015-11-30 etxpeno     Merge from R4A/34
%%% R5A/4      2016-01-19 erarafo     Support for querying COM status
%%% R5A/5      2016-03-23 erarafo     Alive messages when buffering,
%%%                                   avoid translating struct instance DNs
%%% R5A/6      2016-03-23 erarafo     Dialyzer fault fixed
%%% R5A/7      2016-03-24 erarafo     Fixed type fault that dialyzer missed
%%% R5A/8      2016-04-01 erarafo     HU69578 solution
%%% R5A/9      2016-04-07 erarafo     Removing unused fields in result tuple
%%% R5A/10     2016-04-14 erarafo     Generate AVC from struct cre/del bursts
%%% R6A/1      2016-04-21 erarafo     getAttrNameValues/2 simplified
%%% R6A/2      2016-04-23 erarafo     Whitespace only
%%% R6A/3      2016-04-27 erarafo     Transaction id leak fixed
%%% R6A/4      2016-04-27 uabesvi     HU76779 merged from R5A
%%% R6A/5      2016-05-10 uabesvi     HU81679 dirty AVC
%%% R6A/6      2016-05-24 erarafo     Fixed uncaught throw
%%% R7A/1      2016-09-08 etxpeno     Dialyzer fixes
%%% R8A/1      2016-12-19 etxpeno     TR HV43980 correction
%%% R10A/1     2017-05-19 etxpeno     change com_stopped/0 to be synchronously
%%% R10A/2     2017-06-27 etxjotj     Added interface towards swmServer
%%% R10A/3     2017-07-05 emarnek     HV87092
%%% R11A/1     2017-09-04 etxjotj     Replaced swmBackup with swmI
%%% R11A/2     2017-09-22 etomist     HW31372 (workaround for HW27775)
%%% R11A/3     2017-10-09 etomist     Small fix for HW31372 implementation
%%% R11A/4     2017-10-19 etxpeno     Increase timeout value in com_stopped/0
%%%                                   to 40 seconds
%%% ----------------------------------------------------------

-export([register_subscriptions/1]).
-export([register_subscriptions/2]).
-export([start/0]).
-export([com_started/0]).
-export([com_stopped/0]).
-export([testControl/2]).

%% SAF NTF calls this function
-export([notification_callback/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

%% Call from swmServer
%% This is to because SWM needs to resend AVC message in case OSS misses them
-export([generate_event/5]).

-include("comte_types.hrl").
-include("comte_event.hrl").
-include("comte_ac.hrl").
-include("ComsaEvent.hrl").
-include("ComsaEcimModelAdaptor.hrl").

-include("safs_ais.hrl").
-include("safs_ntf.hrl").

-record('EcimPassword', {cleartext, password}).



%%% Reserved words that cannot occur as attribute names
%%% (from OpenSAF Extensions rel 4.3)

-define(IMM_RESERVED_NAMES,
	[<<"saImmAttrAdminOwnerName">>,
	 <<"saImmAttrClassName">>,
	 <<"saImmAttrImplementerName">>,
	 <<"SaImmOiCcbIdT">>,
	 <<"ccbLast">>]).

%%% Timeout when waiting for COI to come up (milliseconds).
-define(RETRY_COI_TIMEOUT, 200).
-define(RETRY_COI_LIMIT, 120000).

%%% Number of events that can be buffered
%%% when COM is down.
-define(EVENT_BUFFER_SIZE, 999).

%%% Correlation record lifetime (microseconds).
-define(CORRELATION_LIMIT, 5000000).

%%% Log alive-messages during buffering this often
-define(ISALIVE_WHEN_BUFFERING_PERIOD, 60000).

-define(DN_TRANSLATION_FAILURE, dn_translation_failure).
-define(ATTRIBUTES_NAMES_NOT_FOUND, attributes_names_not_found).
-define(MISSING_CCB_INFO, missing_ccb_info).
-define(NO_COI_TRANSACTION, no_coi_transaction).
-define(COI_LOOKUP_FAILED, coi_lookup_failed).
-define(MIM_DETAILS_NOT_FOUND, mim_details_not_found).
-define(MISSING_CLASS_INFO, missing_class_info).
-define(AMBIGUOUS_CLASS, ambiguous_class).

-define(COI_SERVICE, coiServer).
-define(SERVER, ?MODULE).

-type ntfNotification()   :: #safsNtfObjectCreateDeleteNotification{}|
	  #safsNtfAttributeChangeNotification{}.

%%% -compile(export_all).

%%% ----------------------------------------------------------
%%% @doc Register event subscribers for selected classes in a model
%%% This is the raw version, and erroneous input might lead to no avcs being
%%% generated
%%% @end
%%% ----------------------------------------------------------

-spec register_subscriptions(string(),              % Model
			     [{string(), atom()}]   % {Class, Table}
			    ) ->
	  ok | {aborted, term()}.

register_subscriptions(Model, ClassesAndTables) ->
    write_table(Model, ClassesAndTables).

%%% ----------------------------------------------------------
%%% @doc Register event subscribers for all classes in a model
%%% Warning! This function might not be usable in an early installation phase
%%% due to model information not being available, use /2 function instead
%%% @end
%%% ----------------------------------------------------------

register_subscriptions(Model) ->
    %% No sanity check necessary
    case comsaEcimModelAdaptor:get_model_classes(Model) of
	[] ->
	    erlang:error(no_models_in_class, [Model]);
	Classes ->
	    Mapped = [case Class of
			  "Schema" -> sysMSchema;
			  _ -> list_to_atom(small(Class))
		      end||Class<-Classes],
	    write_table(Model, Mapped)
    end.


%%% ----------------------------------------------------------
%%% @doc Callback function used by SAF NTF for avc notifications.
%%% NOTE: Log and alarm notifications are handled in the
%%% comsaNtfSubscriber module.
%%% @end
%%% ----------------------------------------------------------

notification_callback(_SubscriptionId, Notification) ->
    gen_server:cast(?SERVER,
		    {notification_callback, Notification}).



%%% ----------------------------------------------------------
%%% @doc Called by comsaSuper supervisor.
%%% @end
%%% ----------------------------------------------------------

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% ----------------------------------------------------------
%%% @doc Called by comsaServer.
%%% @end
%%% ----------------------------------------------------------

com_started() ->
    gen_server:cast(?SERVER, com_started).


%%% ----------------------------------------------------------
%%% @doc Called by comsaServer.
%%% @end
%%% ----------------------------------------------------------

com_stopped() ->
    gen_server:call(?SERVER, com_stopped, 40000).


%%% ----------------------------------------------------------
%%% @doc For test actions.
%%% @end
%%% ----------------------------------------------------------



testControl(crash, Args) ->
    gen_server:cast(?SERVER, {testControl, crash, Args});

testControl(Op, Args) ->
    gen_server:call(?SERVER, {testControl, Op, Args}).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-type ccbId()  :: non_neg_integer().

-record(ccEntry, {ccbId               :: ccbId(),
		  timestamp           :: integer(),
		  store=orddict:new() :: orddict:orddict()}).

-record(state, {comStatus = stopped            :: started | stopped,
		correlationCache = []          :: [#ccEntry{}],
		etsTable                       :: ets:tid() | 'undefined',
		etsTableNextKey                :: integer() | 'undefined',
		etsTableFull                   :: boolean() | 'undefined',
		etsTableLost                   :: integer() | 'undefined',
		timerRef                       :: timer:tref() | undefined,
		transactions   = #{}           :: map()
	       }).

-record(event,
	{key     :: integer(),
	 data    :: tuple()
	}).

init(_) ->
    case clhI:core_state() of
	active ->
	    %% Let the process start up and take care of the rest later
	    gen_server:cast(self(), initiate),
	    {ok, #state{}};
	undefined ->
	    ignore
    end.


handle_call({testControl, isComRunning, _}, _From, #state{comStatus=ComStatus}=State) ->
    {reply, ComStatus, State};

handle_call(com_stopped, _From, #state{timerRef = Tref} = State) ->
    info_msg("COM is stopped", []),
    cancelTimer(Tref),
    TrefNew = makeTimer(),
    {reply, ok, State#state{comStatus = stopped, timerRef = TrefNew}};

handle_call(_, _, State) ->
    {reply, ok, State}.


handle_cast(initiate, State) ->
    mnesia:subscribe(activity),
    ok = mnesia:wait_for_tables([comsaEvent], 30000),
    [mnesia:subscribe({table, Obj#comsaEvent.table, detailed})
       ||Obj<-ets:tab2list(comsaEvent)],
    mnesia:subscribe({table, comsaEvent, detailed}),
    case waitForCoi(?RETRY_COI_LIMIT) of
	nok ->
	    {stop, coi_not_started, State};
	ok ->
	    EtsTable = ets:new(noname, [ordered_set, {keypos, #event.key}]),
	    % Check if COM is up: During a system restart COM will
	    % not yet be up, but if comsaEvent alone has restarted
	    % then COM is probably up and we need to have the right
	    % perception here.
	    TS1 = erlang:monotonic_time(),
	    ComStatus =
		case comte:healthCheck(25000) of
		    ?ComOk ->
			started;
		    {error, com_not_started} ->
			stopped;
		    {ComErrorCode, ComRecoveryAction} ->
			warning_msg("COM status is: ~p, ~p",
				    [ComErrorCode, ComRecoveryAction]),
			stopped
		end,
	    TS2 = erlang:monotonic_time(),
	    Millis = erlang:convert_time_unit(TS2-TS1, native, milli_seconds),
	    info_msg("COM status is: ~p (health check took ~w ms)",
		     [ComStatus, Millis]),
	    Tref =
		if
		    ComStatus =:= started ->
			undefined;
		    ComStatus =:= stopped ->
			makeTimer()
		end,
	    % info_msg("initiate done", []),
	    {noreply, State#state{comStatus=ComStatus,
				  etsTable=EtsTable,
				  etsTableNextKey=1,
				  etsTableFull=false,
				  etsTableLost=0,
				  timerRef=Tref}}
    end;

handle_cast({notification_callback, Notification},
	    #state{comStatus=ComStatus, correlationCache=CC}=State) when ComStatus =:= started ->
    Now = erlang:monotonic_time(),
    RecentCC =
	lists:filter(
	  fun(#ccEntry{timestamp=Created}) ->
		  AgeMicros = erlang:convert_time_unit(Now-Created, native,
						       micro_seconds),
		  AgeMicros < ?CORRELATION_LIMIT
	  end,
	  CC),
    NewCC = handleNotif(Notification, RecentCC, Now),
    {noreply, State#state{correlationCache=NewCC}};



handle_cast({notification_callback, _Notification}=Tuple,
	    #state{etsTable=Table,
		   etsTableNextKey=Key,
		   etsTableFull=IsFull}=State) ->
    % COM is not running
    if
	IsFull ->
	    Lost = State#state.etsTableLost,
	    {noreply, State#state{etsTableLost=Lost+1}};
	true ->
	    IsFullNow = tableAdd(Tuple, Table, Key),
	    {noreply, State#state{etsTableNextKey=Key+1,
				  etsTableFull=IsFullNow}}
    end;

handle_cast(com_started,
	    #state{etsTable     = Table,
		   etsTableLost = Lost,
		   timerRef     = Tref} = State) ->
    %% info_msg("COM is started", []),
    cancelTimer(Tref),
    tableFlush(Table, Lost),
    {noreply, State#state{comStatus    = started,
			  etsTableFull = false,
			  etsTableLost = 0,
			  timerRef     = undefined}};

handle_cast({testControl, crash, [Reason]}, _State) ->
    erlang:exit(Reason);

handle_cast(_, State) ->
    {noreply, State}.


handle_info({mnesia_activity_event, {complete, Tid} = _Event},
	    #state{comStatus    = ComStatus,
		   transactions = Trans} = State)
  when ComStatus == started ->
    handle_complete(maps:get(Tid, Trans, undefined), Tid, Trans),
    NewTrans = maps:remove(Tid, Trans),
    {noreply, State#state{transactions = NewTrans}};

handle_info({mnesia_activity_event, {complete, _} = _Event},
	    #state{etsTableFull = IsFull} = State)
  when IsFull ->
    {noreply, State};
handle_info({mnesia_activity_event, {complete, _}} = Event,
	    #state{etsTable        = Table,
		   etsTableNextKey = Key} = State) ->
    IsFullNow = tableAdd(Event, Table, Key),
    {noreply, State#state{etsTableNextKey = Key + 1,
			  etsTableFull    = IsFullNow}};


handle_info({mnesia_table_event, {write, comsaEvent, New, _, _}}, State) ->
    mnesia:subscribe({table, New#comsaEvent.table, detailed}),
    {noreply, State};

handle_info({mnesia_table_event, {write, _, _, _, Tid} = Event}, State)
  when State#state.comStatus == started andalso
       element(1, Tid) == dirty ->
    handle_mnesia_table_event(Event),
    {noreply, State};

handle_info({mnesia_table_event, Event}, #state{transactions = Trans} = State)
  when State#state.comStatus == started ->
    %% HU76779
    NewTrans = save_mnesia_table_event(Event, Trans),
    %%handle_mnesia_table_event(Event),
    {noreply, State#state{transactions = NewTrans}};

handle_info({mnesia_table_event, _Event} = Tuple,
	    #state{transactions    = _Trans,
		   etsTable        = Table,
		   etsTableNextKey = Key,
		   etsTableFull    = IsFull} = State) ->
    %% HS88484
    if
	IsFull ->
	    Lost = State#state.etsTableLost,
	    {noreply, State#state{etsTableLost=Lost+1}};
	true ->
	    IsFullNow = tableAdd(Tuple, Table, Key),
	    {noreply, State#state{etsTableNextKey=Key+1,
				  etsTableFull=IsFullNow}}
    end;

handle_info(isBuffering, #state{etsTableNextKey=NextKey, etsTableLost=Lost}=State) ->
    info_msg("buffering AVCs: buffered: ~w, lost: ~w", [NextKey-1, Lost]),
    Tref = makeTimer(),
    {noreply, State#state{timerRef=Tref}};

handle_info(clean, State) ->
    {noreply, State#state{transactions = #{}}};

handle_info(state, State) ->
    io:format("STATE~n~p~n", [State]),
    {noreply, State};


handle_info(_, State) ->
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


terminate(_, #state{timerRef=Tref}) ->
    cancelTimer(Tref),
    ok.


waitForCoi(LeftMillis) ->
    if
	LeftMillis =< 0 ->
	    nok;
	true ->
	    case whereis(?COI_SERVICE) of
		undefined ->
		    % info_msg("COI is not yet available", []),
		    timer:sleep(?RETRY_COI_TIMEOUT),
		    waitForCoi(LeftMillis - ?RETRY_COI_TIMEOUT);
		_ ->
		    % info_msg("COI is available", []),
		    ok
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Adds a table entry. If the table is full a warning
%%% is logged and the returned value is true.
%%% @end
%%% ----------------------------------------------------------
-spec tableAdd(tuple(), ets:tid(), integer()) -> boolean().

tableAdd(Tuple, Table, Key) ->
    TableSize = ets:info(Table, size),
    if
	TableSize >= ?EVENT_BUFFER_SIZE ->
	    warning_msg(
	      "event buffer capacity (~w) exceeded, "
	      "notifications are lost "
	      "until 'event buffer flushed' message is logged",
	      [?EVENT_BUFFER_SIZE]),
	    true;
	true ->
	    ets:insert(Table, #event{key=Key, data=Tuple}),
	    false
    end.


-spec tableFlush(ets:tid(), non_neg_integer()) -> any().

tableFlush(Table, Lost) ->
    case ets:first(Table) of
	'$end_of_table' ->
	    % info_msg("event buffer is empty, no need to flush", []),
	    ok;
	FirstKey ->
	    tableFlushHelper(Table, FirstKey),
	    ets:delete_all_objects(Table),
	    info_msg("event buffer flushed, lost: ~w", [Lost])
    end.


tableFlushHelper(_Table, '$end_of_table') ->
    ok;

tableFlushHelper(Table, Key) ->
    tfh(ets:lookup_element(Table, Key, #event.data)),
    NextKey = ets:next(Table, Key),
    tableFlushHelper(Table, NextKey).

tfh({mnesia_table_event, _} = Tuple) ->
    %% info_msg("table flush, key: ~w, mnesia, ~p", [Key, Tuple]),
    self() ! Tuple;
tfh({mnesia_activity_event, _} = Tuple) ->
    %% info_msg("table flush, key: ~w, mnesia, ~p", [Key, Tuple]),
    self() ! Tuple;
tfh({notification_callback, _} = Tuple) ->
    %% info_msg("table flush, key: ~w, notif, ~p", [Key, Tuple]),
    gen_server:cast(?SERVER, Tuple).




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           write_table(Model, Classes)
%%% Input: Model::string() - Name of the model to which the classes belongs
%%%        Classes::{ClassName::string(), TableName::atom()}
%%% Output: ok | {aborted, Reason}
%%% Exceptions:
%%% Description: This function registers the subscribers
%%% ----------------------------------------------------------
write_table(Model, Classes) ->
    Fun = fun() ->
		  [mnesia:write(#comsaEvent{table=Table,class={Model,Class}}) ||
		      {Class, Table}<-Classes],
		  ok
	  end,
    case mnesia:transaction(Fun) of
	{atomic, ok} -> ok;
	{aborted, Reason} ->
	    {aborted, Reason}
    end.


%%% ----------------------------------------------------------
%%% #           handle_mnesia_table_event(Event)
%%% Input: Event- according to mnesia:subscribe
%%% Output: ok
%%% Exceptions:
%%% Description: This function receives mnesia table events
%%% ----------------------------------------------------------


handle_mnesia_table_event({write, schema, _, _, _}) ->
    %% Generated by e.g. mnesia:clear_table (was previously implemented as
    %% delete / create table and this event behaviour is kept for backwards
    %% compatibility).
    ok;
handle_mnesia_table_event({write, Table, New, [Old], _}) ->
    [Evt] = mnesia:dirty_read({comsaEvent, Table}),
    {Model, Class} = Evt#comsaEvent.class,
    {Attributes, _} =
	comsaEcimModelAdaptor:get_class_types(Model, Class),
    case find_changed_attributes(New, Old, 1, avc, Attributes) of
	[] ->
	    ok;
	Changed ->
	    generate_event(avc, element(2, New), Changed, Model, Class)
    end,
    ok;
handle_mnesia_table_event({write, Table, New, [], _}) ->
    [Evt] = mnesia:dirty_read({comsaEvent, Table}),
    {Model, Class} = Evt#comsaEvent.class,
    {Attributes, _} = comsaEcimModelAdaptor:get_class_types(Model, Class),
    Old = mnesia:table_info(Table, wild_pattern),
    Changed = find_changed_attributes(New, Old, 1, new, Attributes),
    generate_event(new, element(2, New), Changed, Model, Class);
handle_mnesia_table_event({delete, comsaEvent, _, _, _}) ->
    ok;
handle_mnesia_table_event({delete, schema, _, _, _}) ->
    %% Generated by e.g. mnesia:clear_table (was previously implemented as
    %% delete / create table and this event behaviour is kept for backwards
    %% compatibility).
    ok;
handle_mnesia_table_event({delete, Table, Obj, _, _}) ->
    [Evt] = mnesia:dirty_read({comsaEvent, Table}),
    {Model, Class} = Evt#comsaEvent.class,
    generate_event(delete, element(2, Obj), [], Model, Class);

handle_mnesia_table_event(X) ->
    info_msg("unknown event: ~p",[X]),
    ok.

find_changed_attributes(New, Old, N, EventType,
			[{Name, Type, _Prmssn, notification} | Attributes]) ->
    case {element(N+1, New), element(N+1, Old)} of
	{Same, Same} ->
	    find_changed_attributes(New, Old, N+1, EventType, Attributes);
	{undefined, _} ->
	    [{Name, Type, undefined}|
	     find_changed_attributes(New, Old, N+1, EventType, Attributes)];
	{NewValue, _} when Name==nextScheduledTime, Type=='RcsBrM.DateTime'->
	    %% Very special special
	    %% info_msg("Changed: ~p ~p~n~p~n",[Name, Type, NewValue]),
	    [{Name, Type, swmI:format_next_scheduled_time(NewValue)} |
	     find_changed_attributes(New, Old, N+1, EventType, Attributes)];
	{#'EcimPassword'{} = NewValue, _} ->
	    %% Don't expose passwords in cleartext! Make sure it is encrypted
	    %% before we include in notification.
	    [{Name, Type, encrypt_password(NewValue)} |
	     find_changed_attributes(New, Old, N+1, EventType, Attributes)];
	{NewValue, _} ->
	    %% info_msg("Changed: ~p ~p~n~p~n",[Name, Type, NewValue]),
	    [{Name, Type, NewValue} |
	     find_changed_attributes(New, Old, N+1, EventType, Attributes)]
    end;
find_changed_attributes(New, Old, N, EventType, [_|Attributes]) ->
    find_changed_attributes(New, Old, N+1, EventType, Attributes);
find_changed_attributes(_, _, _, _, []) ->
    [].

%%% ----------------------------------------------------------
encrypt_password(#'EcimPassword'{cleartext = true, password = PW}) ->
    #'EcimPassword'{cleartext = undefined,
		    password = comsaGeneric:encrypt_password(PW)};
encrypt_password(EPW) ->
    EPW.

%%% ----------------------------------------------------------
%%% #           generate_avc
%%% Input: Key::tuple() - A key as generated by comsaGeneric:dnrev_to_key/1
%%%        Changed::{Name::atom(), InternalType, AttributeValue}
%%%            - A list of changed attributes, their types and new values
%%%              InternalType format is what's given from comsaEcimModelAdaptor
%%%              AttributeValue format is how the value is stored in mnesia by
%%%              comsaGeneric
%%%        Model::string() - The name of the model
%%%        Class::string() - The name of the class
%%% Output:
%%% Exceptions:
%%% Description: Generates an avc notification for the given changes to an MO
%%% ----------------------------------------------------------

generate_event(Type, Key, Changed, Model, Class) ->
    Dn = comsaEcimModelAdaptor:get_dn(Key, Model, Class),
    ChangedAttributes = updateMeId(make_attribute_list(Dn, Changed)),
    CmEvent = #cm_event_1{dn = Dn,
			  event_type = case Type of
					   new -> ?MoCreated;
					   delete -> ?MoDeleted;
					   avc -> ?AttributeValueChange
				       end,
			  attributes =
			      remove_undefined_attributes(
				ChangedAttributes,[])},
    Notification =
	#cm_notification_1{
	   trans_id = 1, % workaround for HS88903
	   source = ?ResourceOperation,
	   events = [CmEvent]},
    notify(Notification).


%%% ----------------------------------------------------------
%%% @doc Modifies a list of items. For items that match the
%%% pattern {binary(), [any()]} the second element of the
%%% tuple is stripped from 'undefined' atoms.
%%% @end
%%% ----------------------------------------------------------

remove_undefined_attributes([{S,L}|T],Acc) when is_binary(S), is_list(L) ->
    L2 = [X || X <- L,  X /= undefined],
    remove_undefined_attributes(T, [{S,L2}|Acc]);
remove_undefined_attributes([H|T],Acc) ->
    remove_undefined_attributes(T, [H|Acc]);
remove_undefined_attributes([],Acc) ->
    lists:reverse(Acc).


%%% ----------------------------------------------------------
%%% @doc Perform a notification via COI or COM as appropriate.
%%% TODO, a faster check could be made here if it can be trusted
%%% that the format of alarm-related DNs is always "list of binary
%%% RDN tokens".
%%% @end
%%% ----------------------------------------------------------

notify(#cm_notification_1{events=[#cm_event_1{dn=Dn}]}=Notification) ->
    case is_prefix("ManagedElement=1,SystemFunctions=1,Fm=1,FmAlarm=", Dn) of
	false ->
	    notifyViaCoi(Notification),
	    notifyViaCom(Notification);
	true ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc For IMM notifications the Dn argument is a list
%%% containing the 3GPP DN as one single binary. For mnesia
%%% notifications the Dn argument is a list of binary tokens,
%%% in 3GPP order, each token being a RDN.
%%%
%%% The returned value is true if the string given as the
%%% first argument is a prefix of the DN given as the second
%%% argument. Comparison is made character by character. If
%%% the second argument is fragmented then an implied comma
%%% is assumed between each fragment.
%%% @end
%%% ----------------------------------------------------------
-spec is_prefix(string(), [binary(), ...]) ->  boolean().

is_prefix(Prefix, [Fragment|More]) ->
    is_prefix(Prefix, Fragment, 0, More).


is_prefix([C|T], Fragment, K, Fragments) when K =:= byte_size(Fragment) ->
    if
	C =/= $, ->
	    false;
	Fragments =:= [] ->
	    false;
	true ->
	    is_prefix(T, hd(Fragments), 0, tl(Fragments))
    end;

is_prefix([C|T], Fragment, K, Fragments) ->
    X = binary:at(Fragment, K),
    if
	C =/= X ->
	    false;
	true ->
	    is_prefix(T, Fragment, K+1, Fragments)
    end;

is_prefix("", _, _, _) ->
    true.


%%% ----------------------------------------------------------
%%% @doc Sends a notification via COM. An error is logged in
%%% case of failure.
%%%
%%% The 'dn' field of the 'cm_notification_1' record must be
%%% given as a list of binaries. For a DN such as
%%% ManagedElement=1,TestRoot=1,TestClass1=1 the 'dn' may be
%%% given as
%%% ```
%%%    [<<"ManagedElement=1,TestRoot=1,TestClass1=1">>]
%%% '''
%%% or alternatively
%%% ```
%%%    [<<"ManagedElement=1">>, <<"TestRoot=1">>, <<"TestClass1=1">>]
%%% '''
%%% @end
%%% ----------------------------------------------------------

-spec notifyViaCom(#cm_notification_1{}) -> ok.

notifyViaCom(Notification) ->
    case comte:notify(Notification) of
    	ok ->
	    ok;
   	{error, Reason} ->
	    Report = [{?MODULE, notifyViaCom},
		      {line, ?LINE},
		      {mfa, {comte, notify, [Notification]}},
		      {notify_failed, Reason}],
	    case Reason of
		no_registered_consumers ->
		    %% sysInitI:warning_report(Report);
		    %% We get a lot of these during upgrade
		    ok;
		non_empty_attribute_list_expected ->
		    %% this case should be investigated!
		    %% https://eforge.ericsson.se/sf/go/artf507165
		    sysInitI:warning_report(Report);
		{bad_attr_value, {StructAttrNameB, {?STRUCT, []}}} when
		  is_binary(StructAttrNameB) ->
		    %% this case was dicussed by email Nov 2015,
		    %% "Installation on LTE 12 Cell"; the root cause
		    %% really should be found and fixed.
		    sysInitI:warning_report(Report ++ [root_cause_not_understood]);
		_ ->
		    sysInitI:error_report(Report)
	    end
    end.


%%% ----------------------------------------------------------
notifyViaCoi(Notification) ->
    coiEvent:notify(Notification).

%%% The following function is very similar to comsaGeneric:get

make_attribute_list(Dn, [{Name, InternalType, AttributeValue}|Changed]) ->
    [{list_to_binary(atom_to_list(Name)),
      case {InternalType, AttributeValue} of
	  {{struct, _}, undefined} ->
	      undefined;
	  {{struct, StructName}, Value} ->
	      StructFields = comsaEcimModelAdaptor:get_struct_fields(
			       dnrev(Dn), StructName),
	      [pack_struct(Value, StructFields)];
	  {{sequence, _}, undefined} ->
	      undefined;
	  {{sequence, _}, []} ->
	      [];
	  {{sequence, {struct, _}},undefined} ->
	      undefined;
	  {{sequence, {struct, StructName}},Values} ->
	      StructFields = comsaEcimModelAdaptor:get_struct_fields(
			       dnrev(Dn), StructName),
	      [pack_struct(Value, StructFields)||Value<-Values];
	  {{sequence, SubInternalType},Values} ->
	      [T,V] = comsaEcimModelAdaptor:type(SubInternalType, hd(Values)),
	      [{T,V}|
	       [{T, lists:nth(2,comsaEcimModelAdaptor:type(SubInternalType,X))}
		||X<-tl(Values)]];

	  {_, Value} ->
	      case comsaEcimModelAdaptor:type(InternalType, Value) of
		  [ComteType, V] ->
		      [{ComteType, V}];
		  [_] ->
		      [undefined]
	      end
      end}|make_attribute_list(Dn, Changed)];
make_attribute_list(_, []) ->
    [].

%% This is because comsaGeneric is adapted to the ManagedObject interface
%% in which non-sequence attributes are not stored as lists, while the
%% notification interface requres all attribute values to be lists

pack_struct(Value, StructFields) ->
    [StructType, StructValues] =
	comsaGeneric:format_struct(Value, StructFields),
    {StructType, pack_struct_members(StructValues)}.

pack_struct_members([{_,[]}|StructValues]) ->
    %% The member is undefined
    pack_struct_members(StructValues);
pack_struct_members([{_,[_]}|StructValues]) ->
    %% The member is undefined
    pack_struct_members(StructValues);
pack_struct_members([{N,[T,V]}|StructValues]) ->
    [{N,{T,V}}|pack_struct_members(StructValues)];
pack_struct_members([{N,[T|Vs]}|StructValues]) ->
    [{N, [{T,V}||V<-Vs]}|pack_struct_members(StructValues)];
pack_struct_members([]) -> [].







    %% {StructType, [case SV of
    %% 		      {N, []}||SV<-StructValues]}.
    %% %% Comte doesn't support undefined in structs yet
    %% {StructType, [case {N,V} of
    %% 		      _ when is_list(V) ->
    %% 			  {N, V};
    %% 		      _ ->
    %% 			  {N, [V]}
    %% 		  end||{N,V}<-StructValues,
    %% 		       V /= undefined]}.


dnrev(Dn) ->
    lists:foldl(fun(X, A) ->
			[N,V] = string:tokens(binary_to_list(X), "="),
			[list_to_binary(V), list_to_binary(N) | A]
		end, [], Dn).

small([H|String]) when H >= $A, H =< $Z ->
    [H-$A+$a|String].


%%% ----------------------------------------------------------
%%% @doc Handle an NTF notification of create/delete or
%%% value-change type.
%%% @end
%%% ----------------------------------------------------------
-spec handleNotif(ntfNotification(), [#ccEntry{}], integer()) ->
                     [#ccEntry{}].

handleNotif(Notification, CC, Now) ->
    try doHandleNotif(Notification, CC, Now)
    catch
	throw:{?DN_TRANSLATION_FAILURE, Details} ->
%% 	    Stack = erlang:get_stacktrace(),
%% 	    warning_msg("~w, ~p, stack:", [?DN_TRANSLATION_FAILURE, Details]),
%% 	    error_logger:info_report(Stack),
	    warning_msg("~w, ~p", [?DN_TRANSLATION_FAILURE, Details]),
	    CC;
	throw:{?ATTRIBUTES_NAMES_NOT_FOUND, Details} ->
	    warning_msg("~w, ~p", [?ATTRIBUTES_NAMES_NOT_FOUND, Details]),
	    CC;
	throw:{?MISSING_CCB_INFO, Details} ->
	    warning_msg("~w, ~p", [?MISSING_CCB_INFO, Details]),
	    CC;
	throw:{?NO_COI_TRANSACTION, Details} ->
	    warning_msg("~w, ~p", [?NO_COI_TRANSACTION, Details]),
	    CC;
	throw:{?COI_LOOKUP_FAILED, Details} ->
	    warning_msg("~w, ~p", [?COI_LOOKUP_FAILED, Details]),
	    CC;
	throw:{?MIM_DETAILS_NOT_FOUND, Details} ->
	    warning_msg("~w, ~p", [?MIM_DETAILS_NOT_FOUND, Details]),
	    CC;
	throw:{?MISSING_CLASS_INFO, Details} ->
	    warning_msg("~w, ~p", [?MISSING_CLASS_INFO, Details]),
	    CC;
	throw:{?AMBIGUOUS_CLASS, Details} ->
	    error_msg("~w, ~p", [?AMBIGUOUS_CLASS, Details]),
	    CC;

	ExceptionType:ExceptionData ->
	    Stack = erlang:get_stacktrace(),
	    error_msg("unspecific exception, ~p, stack -~n~p", [{ExceptionType, ExceptionData}, Stack]),
	    CC
    end.


-spec doHandleNotif(ntfNotification(), [#ccEntry{}], integer()) ->
	  [#ccEntry{}].

doHandleNotif(Notification, CC, Now)
  when is_record(Notification, safsNtfObjectCreateDeleteNotification) ->
    #safsNtfObjectCreateDeleteNotification
	{notificationHeader=Header, objectAttributes=ObjAttrs} =
	Notification,
    handleAllNotif(Header, CC, ObjAttrs, Now);

doHandleNotif(Notification, CC, Now)
  when is_record(Notification, safsNtfAttributeChangeNotification) ->
    #safsNtfAttributeChangeNotification{
       notificationHeader=Header, changedAttributes=ChAttrs} = Notification,
    #safsNtfNotificationHeader{additionalInfo=_AdditionalInfo}=Header,
    %%info_msg("@@@ ~s ~p: AVC", [ccb(AdditionalInfo, ChAttrs), ImmDn]),
    %%io:format("ChAttrs = ~p~nImmDn = ~p~n",[ChAttrs, ImmDn]),

    handleAllNotif(Header, CC, ChAttrs, Now).


%%% ----------------------------------------------------------
%%% @doc Handle everything except for the "struct" case.
%%% @end
%%% ----------------------------------------------------------
-spec handleAllNotif(#safsNtfNotificationHeader{}, [#ccEntry{}], [any()],
	                    integer()) ->
                        [#ccEntry{}].

handleAllNotif(#safsNtfNotificationHeader{eventType=?SA_NTF_OBJECT_DELETION,
					  notificationObject=NotificationObject,
					  additionalInfo=AdditionalInfo},
	       CC,
	       ObjAttrs,
	       _Now) ->
    EET = ?MoDeleted,

    %%info_msg("+++ ~p: delete", [NotificationObject]),
    EcimDn = getEcimDn(NotificationObject),
    notify(EcimDn, EET, []),
    case getCcbIdInfo(AdditionalInfo, ObjAttrs) of
	{false, _} ->
	    %%info_msg("+++ ~p: delete, no CCB", [NotificationObject]),
	    CC;
	{true, {_, false}} ->
	    %% not last, so nothing to remove
	    %%info_msg("+++ ~p: delete, CCB, not last", [NotificationObject]),
	    CC;
	{true, {CcbId, true}} ->
	    case removeCcEntry(CcbId, CC) of
		{undefined, _} ->
		    %%info_msg("+++ ~p: delete, CCB, last, no previous CC entry", [NotificationObject]),
		    CC;
		{_, RemainderCC} ->
		    %%info_msg("+++ ~p: delete, CCB, last, cache cleaned", [NotificationObject]),
		    RemainderCC
	    end
    end;

handleAllNotif(#safsNtfNotificationHeader{eventType=?SA_NTF_OBJECT_CREATION,
					  notificationObject=NotificationObject,
					  additionalInfo=AdditionalInfo},
	       CC,
	       ObjAttrs,
	       Now) ->
    EET = ?MoCreated,

    %%info_msg("+++ ~s ~p: create", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
    {Defined, {CcbId, IsLast}} = getCcbIdInfo(AdditionalInfo, ObjAttrs),
    if
	not Defined ->
	    %%info_msg("+++ ~s ~p: create, no CCB", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
	    AttrNames = getAttrNames(NotificationObject),
	    {EcimDn, EcimAttributes} = getAttrNameValues3(NotificationObject,
	     						  AdditionalInfo,
	     						  ObjAttrs,
	     						  AttrNames),
	    notify(EcimDn, EET, updateMeId(EcimAttributes)),
	    CC;
	IsLast ->
	    %%info_msg("+++ ~s ~p: create, no more to follow", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
	    EcimDn = getEcimDn(NotificationObject),
	    {EcimClass, Mim} = getEcimClassAndMim(NotificationObject),
	    AttrNames = getAttrNamesByClassAndMim(EcimClass, Mim),
	    EcimAttributes = getAttrNameValues(EcimDn, AttrNames),
	    notify(EcimDn, EET, updateMeId(EcimAttributes)),
	    CC;
	true ->
	    %% defined and not last
	    %%info_msg("+++ ~s ~p: create, more to follow", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
	    EcimDn = getEcimDn(NotificationObject),
	    {EcimClass, Mim} = getEcimClassAndMim(NotificationObject),
	    AttrNames = getAttrNamesByClassAndMim(EcimClass, Mim),
	    EcimAttributes = getAttrNameValues(EcimDn, AttrNames),
	    notify(EcimDn, EET, updateMeId(EcimAttributes)),
	    case removeCcEntry(CcbId, CC) of
		{undefined, _} ->
		    %%info_msg("+++ ~s ~p: create, start ccEntry", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
		    {_, NewStore} = updateAttrValuesStore(EcimDn, EcimAttributes, orddict:new()),
		    [#ccEntry{ccbId=CcbId, store=NewStore, timestamp=Now}|CC];
		{#ccEntry{store=TheStore}, NewCC1} ->
		    %%info_msg("+++ ~s ~p: create, update ccEntry", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
		    {_, NewStore} = updateAttrValuesStore(EcimDn, EcimAttributes, TheStore),
		    [#ccEntry{ccbId=CcbId, store=NewStore, timestamp=Now}|NewCC1]
	    end
    end;


handleAllNotif(#safsNtfNotificationHeader{notificationObject=NotificationObject,
					  additionalInfo=AdditionalInfo},
	       CC,
	       ObjAttrs,
	       Now) ->
    EET = ?AttributeValueChange,

    %%info_msg("+++ ~s ~p: AVC", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
    {Defined, {CcbId, IsLast}} = getCcbIdInfo(AdditionalInfo, ObjAttrs),
    if
	not Defined ->
	    %% just notify
	    %%info_msg("+++ ~s ~p: AVC, no CCB", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
	    {EcimDn, EcimAttrs} = getAttrNameValues2(NotificationObject,
						     AdditionalInfo, ObjAttrs),
	    notify(EcimDn, EET, updateMeId(EcimAttrs)),
	    CC;
	true ->
	    %% the notification has ccbId
	    %%info_msg("+++ ~s ~p: AVC in CCB", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
	    EcimDn = getEcimDn(NotificationObject),
	    AttrNames = getAttrNamesByNotif(AdditionalInfo),
	    EcimAttrs = getAttrNameValues(EcimDn, AttrNames),
	    case removeCcEntry(CcbId, CC) of
		{undefined, _} ->
		    %% no ccEntry yet; just notify
		    %%info_msg("+++ ~s ~p: AVC in CCB, no correlation", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
		    notify(EcimDn, EET, updateMeId(EcimAttrs)),
		    if
			IsLast ->
			    CC;
			true ->
			    {_, NewStore} =
				updateAttrValuesStore(EcimDn, EcimAttrs, orddict:new()),
			    [#ccEntry{ccbId=CcbId, timestamp=Now, store=NewStore}|CC]
		    end;
		{#ccEntry{store=TheStore}, NewCC1} ->
		    %% reduce the attrs

		    %%info_msg("+++ ~s ~p: AVC in CCB, correlation", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
		    {NewAttrs, NewStore} =
			updateAttrValuesStore(EcimDn, EcimAttrs, TheStore),

%%% 				    OrgAttrNames = ordsets:from_list([A||{A, _} <- EcimAttrs]),
%%% 				    NewAttrNames = ordsets:from_list([A||{A, _} <- NewAttrs]),
%%%
%%% 				    DroppedAttrs = ordsets:subtract(OrgAttrNames, NewAttrNames),
%%% 				    case ordsets:size(DroppedAttrs) of
%%% 					0 ->
%%% 					    %info_msg("+++ ~s ~p: AVC in CCB, no reduction", [ccb(AdditionalInfo, ObjAttrs), NotificationObject]),
%%% 					    ok;
%%% 					_N ->
%%% 					    %info_msg("+++ ~s ~p: AVC in CCB, reduction: ~p", [ccb(AdditionalInfo, ObjAttrs), NotificationObject, DroppedAttrs])
%%% 				    end,
		    notify(EcimDn, EET, updateMeId(NewAttrs)),
		    if
			IsLast ->
			    NewCC1;
			true ->
			    CCEntry = #ccEntry{timestamp=Now,
					       ccbId=CcbId,
					       store=NewStore},
			    [CCEntry|NewCC1]
		    end
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Perform a notification, unless it is a value change
%%% with zero attributes.
%%% @end
%%% ----------------------------------------------------------
notify(_EcimDn, ?AttributeValueChange, []) ->
    ok;
notify(EcimDn, EET, EcimAttrs) ->
    Events = [#cm_event_1{dn=[EcimDn],
			  event_type=EET,
			  attributes=EcimAttrs}],
    Notification = #cm_notification_1{trans_id=1,
				      source=?ResourceOperation,
				      events=Events},
    notify(Notification).


%%% ----------------------------------------------------------
%%% @doc Removes the indicated #ccEntry{} from the given list.
%%% A tuple with the removed entry and the reduced list is
%%% returned. If no entry matches then a tuple {undefined, OrigList}
%%% is returned.
%%% @end
%%% ----------------------------------------------------------
-spec removeCcEntry(ccbId(), []) -> {undefined|#ccEntry{}, [#ccEntry{}]}.

removeCcEntry(CcbId, CC) ->
    removeCcEntry(CcbId, CC, CC, []).


removeCcEntry(CcbId, [#ccEntry{ccbId=X}=U|More], _CC, Acc) when X =:= CcbId ->
    {U, lists:reverse(Acc, More)};

removeCcEntry(CcbId, [A|More], CC, Acc) ->
    removeCcEntry(CcbId, More, CC, [A|Acc]);

removeCcEntry(_CcbId, [], CC, _Acc) ->
    {undefined, CC}.


%%% ----------------------------------------------------------
%%% @doc Reduce the list of attribute - value(s) pairs
%%% against the given store. An updated store is returned.
%%% Entries in the resulting list will also be in the store.
%%% @end
%%% ----------------------------------------------------------

updateAttrValuesStore(Dn, AttrValues, Store) ->
    {NewAttrValues, NewStore} =
	lists:foldl(
	  fun({AttrName, Values}=U, {R, S}) ->
		  case updateAttrValuesStore(Dn, AttrName, Values, S) of
		      {true, NewS} ->
			  {R, NewS};
		      {false, NewS} ->
			  {[U|R], NewS}
		  end
	  end,
	  {[], Store},
	  AttrValues),
    {lists:reverse(NewAttrValues), NewStore}.


%%% ----------------------------------------------------------
%%% @doc Matches the AttrName and Values against the store.
%%% The returned value is {Found, NewStore} where Found
%%% is either true or false.
%%% @end
%%% ----------------------------------------------------------
-spec updateAttrValuesStore(any(), any(), [any()], orddict:orddict()) ->
	  {boolean(), orddict:orddict()}.

updateAttrValuesStore(Dn, AttrName, Values, Store) ->
    case orddict:find(Dn, Store) of
	error ->
	    {false,
	     orddict:store(Dn,
			   orddict:store(AttrName, Values, orddict:new()),
			   Store)};
	{_, Attrs} ->
	    case orddict:find(AttrName, Attrs) of
		error ->
		    {false,
		     orddict:store(Dn,
				   orddict:store(AttrName, Values, Attrs),
				   Store)};
		{_, OldValues} when Values =/= OldValues ->
		    {false,
		     orddict:store(Dn,
				   orddict:store(AttrName, Values, Attrs),
				   Store)};
		_ ->
		    {true, Store}
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Converts the given argument to a unified list
%%% of value descriptors consisting of an index, a type code
%%% and the value. Multiple values may occur, in which case
%%% several descriptors will have the same index.
%%% @end
%%% ----------------------------------------------------------
-spec getImmAttrValues(
	[#safsNtfAttribute{}|#safsNtfAttributeChange{}]|
	    undefined) ->
	  [{non_neg_integer(), sa_ntf_value_type(), any()}].

getImmAttrValues(undefined) ->
    getImmAttrValues([]);

getImmAttrValues(A) ->
    lists:filtermap(
      fun(#safsNtfAttributeChange{attributeId=Id,
				  attributeType=Type,
				  newAttributeValue=Value}) ->
	      {true, {Id, Type, Value}};
	 (#safsNtfAttribute{attributeId=Id,
			    attributeType=Type,
			    attributeValue=Value}) ->
	      {true, {Id, Type, Value}}
      end,
      A).


%%% ----------------------------------------------------------
%%% @doc Extracts CCB info. The returned value may be
%%% {false, _} if no info is present, or {true, {CcbId, IsLast}}
%%% otherwise.
%%% @end
%%% ----------------------------------------------------------
-spec getCcbIdInfo([#safsNtfAdditionalInfo{}],
		   [#safsNtfAttribute{}|#safsNtfAttributeChange{}]|undefined) ->
	  {boolean(), {ccbId(), boolean()}}.

getCcbIdInfo(AdditionalInfo, ObjAttrs) ->
    ValueDescriptors = getImmAttrValues(ObjAttrs),
    Result =
	lists:foldl(
	  fun(#safsNtfAdditionalInfo{infoId=K,
				     infoType=sa_ntf_value_string,
				     infoValue= <<"SaImmOiCcbIdT">>},
	      {_A, B}) ->
		  case lists:keyfind(K, 1, ValueDescriptors) of
		      false ->
			  throw({?MISSING_CCB_INFO, {<<"SaImmOiCcbIdT">>, AdditionalInfo, ValueDescriptors}});
		      {_, sa_ntf_value_uint64, #safsNtfValue{uint64Val=CcbId}} ->
			  {CcbId, B}
		  end;
	     (#safsNtfAdditionalInfo{infoId=K,
				     infoType=sa_ntf_value_string,
				     infoValue= <<"ccbLast">>},
	      {A, _B}) ->
		  case lists:keyfind(K, 1, ValueDescriptors) of
		      false ->
			  throw({?MISSING_CCB_INFO, {<<"ccbLast">>, no_value, AdditionalInfo, ValueDescriptors}});
		      {_, sa_ntf_value_uint32, #safsNtfValue{uint32Val=CcbLastN}} ->
			  {A, CcbLastN =:= 1}
		  end;
	     (_, Acc) ->
		  Acc
	  end,
	  {undefined, undefined},
	  AdditionalInfo),
    case Result of
	{undefined, undefined} ->
	    % assume this is a notification referring to a runtime instance
	    {false, {0, false}};
	{_, undefined} ->
	    throw({?MISSING_CCB_INFO, {<<"ccbLast">>, no_name, AdditionalInfo, ValueDescriptors}});
	Result ->
	    {true, Result}
    end.


%%% ----------------------------------------------------------
%%% @doc Converts from IMM to ECIM DN format.
%%% @end
%%% ----------------------------------------------------------
-spec getEcimDn(binary()) -> binary().

getEcimDn(NotificationObject) ->
    case gmfI:imm_to_mim(NotificationObject) of
	{ok, MimDn} ->
	    MimDn;
	{error, Offender} ->
	    throw({?DN_TRANSLATION_FAILURE, {NotificationObject, Offender}})
    end.

%%% ----------------------------------------------------------
%%% @doc Gets the ECIM class name and the MIM name from the
%%% given IMM DN.
%%% @end
%%% ----------------------------------------------------------
-spec getEcimClassAndMim(binary()) -> {string(), string()}.

getEcimClassAndMim(ImmDn) ->
    case gmfTrService:imm_to_mim_details(ImmDn) of
	{error, Reason} ->
	    throw({?MIM_DETAILS_NOT_FOUND, {ImmDn, Reason}});
	{ok, Result} ->
	    Result
    end.

%%% ----------------------------------------------------------
%%% @doc Gets attribute names for the given class. The
%%% "MO created" case uses this function.
%%% @end
%%% ----------------------------------------------------------
-spec getAttrNamesByClassAndMim(string(), string()) ->  [binary()].

getAttrNamesByClassAndMim(EcimClass, Mim) ->
    try coi:getMimClass(EcimClass) of
	[] ->
	    throw({?MISSING_CLASS_INFO, EcimClass});
	[{class, PropList}] ->
	    extractAttributes(PropList);
	List ->
	    Matches =
		lists:foldl(
		  fun({class, PL}=Entry, Acc) ->
			  case proplists:get_value(mimName, PL) of
			      MimNameValue when MimNameValue =:= Mim ->
				  Acc++[Entry];
			      _ ->
				  Acc
			  end
		  end,
		  [],
		  List),
	    % hopefully we have exactly one match, else give up
	    case Matches of
		[] ->
		    throw({?MISSING_CLASS_INFO, {mim_mismatch, EcimClass, List}});
		[{class, PropList}] ->
		    extractAttributes(PropList);
		Other ->
		    throw({?AMBIGUOUS_CLASS, {EcimClass, List, Other}})
	    end
    catch
	ExType:ExData ->
	    % not supposed to be possible
	    throw({?MISSING_CLASS_INFO, {ExType, ExData, EcimClass}})
    end.


%%% ----------------------------------------------------------
%%% @doc Extracts a list of attribute names. The key
%%% attribute is excluded from the result.
%%% @end
%%% ----------------------------------------------------------
-spec extractAttributes([tuple()]) -> [binary()].

extractAttributes(Items) ->
    lists:filtermap(
      fun({attribute, Descr}) ->
	      case proplists:is_defined(key, Descr) of
		  true ->
		      false;
		  false ->
		      case proplists:get_value(name, Descr) of
			  undefined ->
			      false;
			  NameS ->
			      {true, list_to_binary(NameS)}
		      end
	      end;
	 (_) ->
	      false
      end,
      Items).

%%% ----------------------------------------------------------
%%% @doc Gets attribute names for the given IMM DN. The
%%% "MO created" case uses this function.
%%% @end
%%% ----------------------------------------------------------
-spec getAttrNames(binary()) -> [binary()].

getAttrNames(ImmDn) ->
    case gmfI:get_attribute_names(ImmDn) of
	{ok, AttrNames} ->
	    AttrNames;
	{error, Reason} ->
	    throw({?ATTRIBUTES_NAMES_NOT_FOUND, {ImmDn, Reason}})
    end.

%%% ----------------------------------------------------------
%%% @doc Gets attribute names by inspecting a list of
%%% #safsNtfAdditionalInfo{} items. Certain reserved names
%%% are excluded from the list. The resulting list may be
%%% empty.
%%% @end
%%% ----------------------------------------------------------
-spec getAttrNamesByNotif([#safsNtfAdditionalInfo{}]) ->  [binary()].

getAttrNamesByNotif(AdditionalInfo) ->
    lists:filtermap(
      fun(#safsNtfAdditionalInfo{infoType=sa_ntf_value_string,
				 infoValue=StringB}) ->
	      case lists:member(StringB, ?IMM_RESERVED_NAMES) of
		  true ->
		      false;
		  false ->
		      {true, StringB}
	      end
      end,
      AdditionalInfo).


%%% ----------------------------------------------------------
%%% @doc Gets values for the given attribute names. Values are
%%% collected using COI. The format of a value may be a single
%%% {EcimType, EcimValue} item.
%%%
%%% For attributes having no value the 'undefined' marker is
%%% replaced by an empty list.
%%% @end
%%% ----------------------------------------------------------
-spec getAttrNameValues(binary(), [binary()]) ->
	  [{binary(),
	    {any(), any()} |
	    [{any(), any()}] |
	    undefined}].

getAttrNameValues(EcimDn, AttrNames) ->
    case coi:join_new() of
	{error, Reason} ->
	    throw({?NO_COI_TRANSACTION, Reason});
	{ok, TrId} ->
	    try coi:getMoAttributes(TrId, EcimDn, AttrNames) of
		{error, Reason} ->
		    throw({?COI_LOOKUP_FAILED, Reason});
		CoiValues ->
		    safeZip(AttrNames, CoiValues, EcimDn)
	    catch
		ExceptionType:ExceptionData ->
		    throw({?COI_LOOKUP_FAILED,
			   {ExceptionType, ExceptionData}})
	    after
		coi:abort_transaction(TrId),
		coi:finish(TrId)
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Equivalent to lists:zip/2 when L and M are equal-length
%%% lists. In case of mismatch [] is returned and an error is logged.
%%% @end
%%% ----------------------------------------------------------
-spec safeZip(list(), list(), binary()) -> [{any(), any()}].

safeZip(L, M, EcimDn) ->
    try
	safeZipHelper(L, M, [])
    catch
	throw:mismatch ->
	    Stack = erlang:get_stacktrace(),
	    sysInitI:error_report(
	      [mismatch,
	       {dn, EcimDn},
	       {names, L},
	       {values, M},
	       {stack, Stack}]),
	    []
    end.

safeZipHelper([A|T], [B|U], R) ->
    safeZipHelper(T, U, [{A, B}|R]);

safeZipHelper([], [], R) ->
    lists:reverse(R);

safeZipHelper(_, _, _) ->
    throw(mismatch).


-spec makeTimer() -> timer:tref()|undefined.

makeTimer() ->
    case timer:send_after(?ISALIVE_WHEN_BUFFERING_PERIOD, isBuffering) of
	{ok, Tref} ->
	    Tref;
	_->
	    warning_msg("failed to create timer", [])
    end.

-spec cancelTimer(timer:tref()  | undefined) ->
                     {ok, cancel}  | {error, any()}  | ok.

cancelTimer(undefined) ->
    ok;

cancelTimer(Tref) ->
    timer:cancel(Tref).


info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format++"~n", [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format++"~n", [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format++"~n", [?MODULE|Args]).


%% %%% ----------------------------------------------------------
%% %%% @doc For log traces. The returned string indicates the
%% %%% ccbId info of the notification.
%% %%% @end
%% %%% ----------------------------------------------------------
%%
%% ccb(AdditionalInfo, ObjAttrs) ->
%%     case getCcbIdInfo(AdditionalInfo, ObjAttrs) of
%% 	{false, {_, _}} ->
%% 	    "---";                            % no CCB info
%% 	{true, {Id, false}} ->
%% 	    integer_to_list(Id) ++ " ..";     % CCB ID present; not last
%% 	{true, {Id, true}} ->
%% 	    integer_to_list(Id) ++ " //"      % CCB ID present; last
%%     end.



%%% ----------------------------------------------------------
%%% #           save_mnesia_table_event(Event, Transactions)
%%%
%%% Input: Event       - according to mnesia:subscribe
%%%        Transaction - #{Transaction => #{Key => [Event]}}
%%%                      mnesia table events which are not yet
%%%                      sent as AVCs.
%%%                      Key is the length of the LDN.
%%%                      Events are stored in reversed order
%%%                      in what they are received.
%%%
%%% Output: Transaction:
%%%
%%% Exceptions:
%%%
%%% Description: This function receives mnesia table events
%%%              and saves them in Transaction until
%%%              mnesia activity event complete is received
%%%              for this transaction id.
%%% ----------------------------------------------------------
%% HU76779
save_mnesia_table_event({_, _, New, _, Tid} = Event, Transactions)
  when is_tuple(New) andalso
       size(New) > 1 andalso
       is_tuple(element(2, New)) ->
    Size    = size(element(2, New)),
    TidData = smte(Size, Event, maps:get(Tid, Transactions, not_found)),
    Transactions#{Tid => TidData};
save_mnesia_table_event({_, _, _, _, Tid} = Event, Transactions) ->
    TidData = smte(1, Event, maps:get(Tid, Transactions, not_found)),
    Transactions#{Tid => TidData}.


smte(Key, Event, not_found) ->
    #{Key => [Event]};
smte(Key, Event, TidData) ->
    smte_add(maps:get(Key, TidData, not_found), Key, Event, TidData).



smte_add(not_found, Key, Event, TidData) ->
    TidData#{Key => [Event]};
smte_add(KeyData, Key, Event, TidData) ->
    TidData#{Key => [Event | KeyData]}.




%%=======================================================================
%% handle_complete(Found, Tid, Transactions) -> Transactions
%% Found = events for the commpleted transaction
%% Tid   = transaction id
%%
%% Transaction completed.
%%
%% Send the notifications in LDN length order.
%%=======================================================================
%% HU76779
handle_complete(undefined, _, Trans) ->
    Trans;
handle_complete(Found, Tid, Trans) ->
    hc(lists:sort(maps:keys(Found)), Found),
    maps:remove(Tid, Trans).


hc([], _) ->
    ok;
hc([Key | T], Events) ->
    KeyEvents = lists:reverse(maps:get(Key, Events)),
    [handle_mnesia_table_event(Event) || Event <- KeyEvents],
    hc(T, Events).

getAttrNameValues2(NotificationObject, AdditionalInfo, ObjAttrs) ->
    %% Used at modify of readonly attributes
    %% Only use the values and the attributes in the notification
    AttrNamesByNotif = getAttrNamesByNotif2(AdditionalInfo),

    ValueList = [{AttrName, gea(AttrId, ObjAttrs)} ||
		    {AttrName, AttrId} <- AttrNamesByNotif],

    {ok, EcimDn, EcimAttrs} = gmfI:to_coi(NotificationObject, ValueList),
    {EcimDn, EcimAttrs}.

getAttrNameValues3(NotificationObject, AdditionalInfo, ObjAttrs, AttrNames) ->
    %% Used at create of readonly objects
    %% Use the values in the notification, but include all attributes
    AttrNamesByNotif = getAttrNamesByNotif2(AdditionalInfo),

    ValueList = [begin
		     AttrId = proplists:get_value(AttrName, AttrNamesByNotif),
		     {AttrName, gea(AttrId, ObjAttrs)}
		 end || AttrName <- AttrNames],

    {ok, EcimDn, EcimAttrs} = gmfI:to_coi(NotificationObject, ValueList),
    {EcimDn, EcimAttrs}.

getAttrNamesByNotif2(AdditionalInfo) ->
    lists:filtermap(
      fun(#safsNtfAdditionalInfo{infoId    = InfoId,
				 infoType  = sa_ntf_value_string,
				 infoValue = StringB}) ->
	      case lists:member(StringB, ?IMM_RESERVED_NAMES) of
		  true ->
		      false;
		  false ->
		      {true, {StringB, InfoId}}
	      end
      end,
      AdditionalInfo).

gea(undefined, _) ->
    [];
gea(AttrId, [#safsNtfAttributeChange{attributeId       = AttrId,
				     attributeType     = NtfType,
				     newAttributeValue = NtfValue}|T]) ->
    V = gea2(NtfType, NtfValue),
    [V|gea(AttrId, T)];
gea(AttrId, [#safsNtfAttribute{attributeId    = AttrId,
			       attributeType  = NtfType,
			       attributeValue = NtfValue}|T]) ->
    V = gea2(NtfType, NtfValue),
    [V|gea(AttrId, T)];
gea(AttrId, [_|T]) ->
    gea(AttrId, T);
gea(_AttrId, []) ->
    [].

gea2(sa_ntf_value_uint8,     NtfValue) -> NtfValue#safsNtfValue.uint8Val;
gea2(sa_ntf_value_int8,      NtfValue) -> NtfValue#safsNtfValue.int8Val;
gea2(sa_ntf_value_uint16,    NtfValue) -> NtfValue#safsNtfValue.uint16Val;
gea2(sa_ntf_value_int16,     NtfValue) -> NtfValue#safsNtfValue.int16Val;
gea2(sa_ntf_value_uint32,    NtfValue) -> NtfValue#safsNtfValue.uint32Val;
gea2(sa_ntf_value_int32,     NtfValue) -> NtfValue#safsNtfValue.int32Val;
gea2(sa_ntf_value_float,     NtfValue) -> NtfValue#safsNtfValue.floatVal;
gea2(sa_ntf_value_uint64,    NtfValue) -> NtfValue#safsNtfValue.uint64Val;
gea2(sa_ntf_value_int64,     NtfValue) -> NtfValue#safsNtfValue.int64Val;
gea2(sa_ntf_value_double,    NtfValue) -> NtfValue#safsNtfValue.doubleVal;
gea2(sa_ntf_value_string,    NtfValue) -> NtfValue#safsNtfValue.variable;
gea2(sa_ntf_value_ldap_name, NtfValue) -> NtfValue#safsNtfValue.variable;
gea2(sa_ntf_value_binary,    NtfValue) -> NtfValue#safsNtfValue.variable;
gea2(sa_ntf_value_csstructt, NtfValue) -> NtfValue#safsNtfValue.csstruct.

updateMeId(AttributeValues) ->
    Med = comsaI:get_managed_element_data(),
    MeId =
    case lists:keyfind(networkManagedElementId, 1, Med) of
        {networkManagedElementId, Str} when is_list(Str) ->
            list_to_binary(Str);
        _ ->
            <<"1">>
    end,
    F1 =
    fun(Arg) ->
        case Arg of
            {11, X} when X =/= <<>> ->
                <<"ManagedElement=1", Y/binary>> = X,
                {11, <<"ManagedElement=", MeId/binary, Y/binary>>};
            _ ->
                Arg
        end
    end,
    F2 =
    fun ({Type, Value}) ->
        case Value of
            [_X] ->
                {Type, lists:map(F1, Value)};
            {11, X} when X =/= <<>> ->
                <<"ManagedElement=1", Y/binary>> = X,
                {Type, {11, <<"ManagedElement=", MeId/binary, Y/binary>>}};
            _ ->
                {Type, Value}
        end
    end,
    case MeId of
        <<"1">> -> AttributeValues;
        _ -> lists:map(F2, AttributeValues)
    end.

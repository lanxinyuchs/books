%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coiAlarm.erl %
%%% @author etxtory
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R9A/1

%%% @doc == COI - Control system Oam Interface ==
%%% This module contains functions for reading Alarm information and subscribing
%%% to Alarm Events, which supports the coi.erl interface functionality.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coiAlarm).
-vsn('/main/R3A/R4A/R5A/R9A/1').
-date('2017-04-20').
-author(etxberb).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% R3A/    ---------- -------  ------------------------------------------------
%% In the COMSA block:
%% ===================
%%% 1      2014-10-24 etxberb  Created.
%%% 3      2014-11-07 etxberb  Added updateTable/1.
%%% 4      2014-11-10 etxberb  Added struct_additionalInfo/2.
%%% 5      2014-11-14 erarafo  Added tentative subscribe/notify interface.
%%% 7      2014-11-16 erarafo  Minor adjustment.
%%% 8      2014-11-21 erarafo  Variable and function naming adjusted.
%%% 9      2014-11-26 erarafo  Disabled callbacks awaiting HT25861.
%%% 10     2014-11-27 erarafo  Undone all calls to coi functions for now.
%%% 11     2014-11-27 erarafo  Dialyzer complaint fixed too.
%% ===================
%% R3A/1   2015-01-13 etxberb  Moved to the COI block.
%% R3A/2   2015-02-13 etxberb  Changed updateTable/1 from log interface to
%%                             comte_default_replicated_list mnesia table.
%% R3A/3   2015-02-16 etxberb  Added mnesia:clear_table in init/1.
%% R3A/4   2015-02-17 erarafo  Removal of provisional Status LED solution.
%% R3A/5   2015-02-18 etxberb  Removed updateTable/1.
%% R4A/1   2015-06-25 etxberb  Changed from sysEnv:role() to clhI:core_state().
%% R4A/2   2015-09-03 etxberb  TR HU15086: Added utc_diff/1.
%% R4A/3   2015-11-05 etxberb  Correction of the case when the value of an
%%                             additionalInfo instance is empty (two consecutive
%%                             ";" separators in COM:s binary is handled in an
%%                             unwanted way in string:tokens/2).
%%                             Added str_tokens/2 and warning reports.
%% R4A/4   2015-11-06 etxberb  Removed a warning report for non-fault case.
%% R4A/5   2015-11-25 etxberb  TR HU38854 corrected.
%% ===================
%% R5A/1   2016-01-07 etxpeno  TR HU49010 corrected.
%% R5A/2   2016-03-31 etxpeno  update of comteAlarm_decode/1 to match updated
%%                             internal format in COM 6.0 CP2
%% R9A/1   2017-04-20 etxtory  No change in code; problem with mismatch release
%%                             of coiAlarm.beam. Try a change a release again.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
-export([child_specs/0,
	 start_link/0]).

-export([get_state/0, info/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.2 Callbacks
%%% ###---------------------------------------------------------------------###
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).
-export([init_comteAlarm/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%% Test:
-export([comteAlarms_decode/0,
	 comteAlarms_print/0,
	 fmAlarms/0,
	 fmAlarms_print/0]).

%%% ###---------------------------------------------------------------------###
%%% # Extended Interface
%%% ###---------------------------------------------------------------------###

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("coi.hrl").
-include("ComFm.hrl").   % $RCS_TOP/COM/COM_CXA1105460/erlinc/ComFm.hrl

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(PROC_INFO(Pid), sysUtil:pid_info(Pid, {all, [error_handler]})).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).

%%
-define(CHILD_coiAlarm, {?MODULE,
			 {?MODULE, start_link, []},
			 permanent,
			 1000,
			 worker,
			 [?MODULE]}).

-define(COMTE_Repl,     comte_default_replicated_list).
-define(COMTE_AlrmsKey, <<"FmActiveAlarmList2">>).
-define(COI_fmAlarm_tmp, coi_fmAlarm_tmp).

%% MO:s
-define(DN_FmAlarm(Val),
	list_to_binary("ManagedElement=1,SystemFunctions=1,Fm=1,FmAlarm=" ++
			   Val)).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(state, {}).

-record(?COMTE_Repl, {instanceName,
		      buffSize,
		      list}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Return specifications of children processes in the supervisor tree.
%%%
%%% @end
%%% ###=====================================================================###
child_specs() ->
    [?CHILD_coiAlarm].

%%% ###########################################################################
%%% @doc Get the server process state.
%%%
%%% @end
%%% ###=====================================================================###
get_state() ->
    gen_server:call(?MODULE, {?MODULE, get_state}).

%%% ###########################################################################
%%% @doc Print the server process state and information.
%%%
%%% @end
%%% ###=====================================================================###
info() ->
    gen_server:cast(?MODULE, {?MODULE, info}).

%%% ###########################################################################
%%% @doc Start the server process.
%%%
%%% @end
%%% ###=====================================================================###
start_link() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    InitArg = #state{},
    Options = [],
    gen_server:start_link(ServerName, Module, InitArg, Options).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.2 Callbacks
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% Initiate the server process.
%%%
%%% ###=====================================================================###
init(State) ->
    case clhI:core_state() of
	active ->
	    spawn_opt(?MODULE,
		      init_comteAlarm,
		      [],
		      [{min_heap_size, 5000}]),
	    %% The ?COMTE table is not disc_copy, so we need to clear the
	    %% fmAlarm table in order to have them in sync from the start:
	    ClearTableRes = mnesia:clear_table(fmAlarm),
	    mnesia:subscribe({table, fmAlarm, detailed}),
	    sysInitI:info_report([{?MODULE, ?FUNCTION},
				  {'clear fmAlarm table', ClearTableRes} |
				  ?STATE_INFO(State)]),
	    {ok, State};
	_ ->
	    ignore
    end.

%%% ###########################################################################
%%% Handling of the COMTE Alarm Subscriber process.
%%%
%%% ###=====================================================================###
init_comteAlarm() ->
    register(coiAlarm_comte, self()),
    mnesia:subscribe({table, ?COMTE_Repl, detailed}),
    ets:new(?COI_fmAlarm_tmp,
	    [named_table, public, ordered_set, {keypos, 2}]),
    supervise_comteAlarm().

supervise_comteAlarm() ->
    try
	loop_comteAlarm()
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    supervise_comteAlarm()
    end.

loop_comteAlarm() ->
    receive
	{mnesia_table_event, Event} ->
	    mnesia_table_event(flush_comteAlarm(Event)),
	    loop_comteAlarm();
	{?MODULE, comteAlarms_decode, SenderPid} ->
	    ets:delete_all_objects(?COI_fmAlarm_tmp),
	    [#?COMTE_Repl{instanceName = ?COMTE_AlrmsKey,
			  list = Alarms}] =
		ets:lookup(?COMTE_Repl, ?COMTE_AlrmsKey),
	    comteAlarms_decode(Alarms),
	    SenderPid ! {?MODULE, comteAlarms_decode_done},
	    loop_comteAlarm()
    end.

flush_comteAlarm(LatestEvent) ->
    receive
	{mnesia_table_event, {write,
			      ?COMTE_Repl,
			      #?COMTE_Repl{instanceName = ?COMTE_AlrmsKey},
			      _ListOfOldObjects,
			      _ActivityId} = Event} ->
	    flush_comteAlarm(Event)
    after
	0 ->
	    LatestEvent
    end.

%%% ###########################################################################
%%% handle_call
%%%
%%% ###=====================================================================###
handle_call({?MODULE, get_state}, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {from, _From},
			     {unrecognized_msg, _Request} |
			     ?STATE_INFO(State)]),
    {reply, ok, State}.

%%% ###########################################################################
%%% handle_cast
%%%
%%% ###=====================================================================###
handle_cast({?MODULE, info}, State) ->
    error_logger:info_report(?PROC_INFO(self()) ++ ?STATE_INFO(State)),
    {noreply, State};
handle_cast(_Request, State) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {unrecognized_msg, _Request} |
			     ?STATE_INFO(State)]),
    {noreply, State}.

%%% ###########################################################################
%%% handle_info
%%%
%%% ###=====================================================================###
handle_info({mnesia_table_event, Event}, State) ->
    try
	mnesia_table_event(Event)
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    {ErrClass, ErrReason}
    end,
    {noreply, State};
handle_info(_Msg, State) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {unrecognized_msg, _Msg} |
			     ?STATE_INFO(State)]),
    {noreply, State}.

%%% ###########################################################################
%%% code_change
%%%
%%% ###=====================================================================###
code_change(_OldVsn, State, _Extra) ->
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      {oldVsn, _OldVsn},
			      {extra, _Extra} | ?STATE_INFO(State)]),
    {ok, State}.

%%% ###########################################################################
%%% terminate
%%%
%%% ###=====================================================================###
terminate(_Reason, _State) ->
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      {reason, _Reason} | ?STATE_INFO(_State)]),
    ok.

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################

%%% ###########################################################################
%%% comteAlarms_decode
%%%
%%% ###=====================================================================###
comteAlarms_decode() ->
    whereis(coiAlarm_comte) ! {?MODULE, comteAlarms_decode, self()},
    receive
	{?MODULE, comteAlarms_decode_done} ->
	    ets:tab2list(?COI_fmAlarm_tmp)
    end.

%%% ###########################################################################
%%% comteAlarms_print
%%%
%%% ###=====================================================================###
comteAlarms_print() ->
    FmAlarms = comteAlarms_decode(),
    Fields = [table | record_info(fields, fmAlarm)],
    [error_logger:info_report(coi:pairElems(Fields, tuple_to_list(FmAlarm)))
     || FmAlarm <- FmAlarms],
    ok.

%%% ###########################################################################
%%% fmAlarms
%%%
%%% ###=====================================================================###
fmAlarms() ->
    ets:tab2list(fmAlarm).

%%% ###########################################################################
%%% fmAlarms_print
%%%
%%% ###=====================================================================###
fmAlarms_print() ->
    FmAlarms = fmAlarms(),
    Fields = [table | record_info(fields, fmAlarm)],
    [error_logger:info_report(coi:pairElems(Fields, tuple_to_list(FmAlarm)))
     || FmAlarm <- FmAlarms],
    ok.

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% base64_to_string
%%%
%%% ###=====================================================================###
base64_to_string(Base64) ->
    try
	base64:mime_decode_to_string(Base64)
    catch
	_ : _ ->
	    Base64
    end.

%%% ###########################################################################
%%% comteAlarms_decode
%%%
%%%   Since COM does not store alarms in RCS the "proper" way, we need to
%%%   decode the alarms in COM's replicated list and store in the correct
%%%   place (mnesia table fmAlarm). All alarms need to be accessible for the COI
%%%   interface implemented in coi.erl.
%%%
%%%   TODO: If COM would implement the "proper" way to store alarms in RCS,
%%%   this code should be removed.
%%%
%%% ###=====================================================================###
comteAlarms_decode([Alarm | Tail]) ->
    try
	FmAlarm = comteAlarm_decode(Alarm),
	ets:insert(?COI_fmAlarm_tmp, FmAlarm)
    catch
	throw : {ignore, _} ->
	    ok;
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {alarm, Alarm},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}])
    end,
    comteAlarms_decode(Tail);
comteAlarms_decode([]) ->
    ok.

comteAlarm_decode({_, AlarmBin}) ->
    [SourceDn_Base64,
     MajorType,
     MinorType,
     AdditionalText_Base64,
     _AdditionalTextForClear_Base64,
     ActiveSeverity,
     SpecificProblem_Base64,
     SeqNumber,
     EventTime,
     EventType,
     ProbableCause,
     OrigSeqNumber,
     OriginalAdditionalText_Base64,
     OriginalSeverity,
     OriginalEventTime,
     _Unknown1,
     _Unknown2,
     AlarmId | AddInfo] = sysUtil:str_tokens(binary_to_list(AlarmBin),
					     ";"),
    case OrigSeqNumber of
	"0" ->
	    throw({ignore, quarantine_period_started});
	_ ->
	    quarantine_period_is_over
    end,
    SourceDn = base64_to_string(SourceDn_Base64),
    AdditionalText = base64_to_string(AdditionalText_Base64),
    SpecificProblem = base64_to_string(SpecificProblem_Base64),
    OriginalAdditionalText =
	base64_to_string(OriginalAdditionalText_Base64),
    %% 3 Hardcoded instance identities and 1 dynamic:
    %% FmAlarmId = {<ManagedElement=>  "1",
    %%              <SystemFunctions=> "1",
    %%              <Fm=>              "1",
    %%              <FmAlarm=>         AlarmId}
    FmAlarmId = {"1", "1", "1", AlarmId},
    #fmAlarm{fmAlarmId = FmAlarmId,
	     source = SourceDn,
	     lastEventTime = nanosec_to_datetime(EventTime),
	     sequenceNumber = list_to_integer(SeqNumber),
	     activeSeverity = enum_SeverityLevel(ActiveSeverity),
	     additionalText = AdditionalText,
	     majorType = list_to_integer(MajorType),
	     minorType = list_to_integer(MinorType),
	     specificProblem = SpecificProblem,
	     eventType = list_to_integer(EventType),
	     probableCause = list_to_integer(ProbableCause),
	     additionalInfo = struct_additionalInfo(AddInfo),
	     originalEventTime = nanosec_to_datetime(OriginalEventTime),
	     originalSeverity = enum_SeverityLevel(OriginalSeverity),
	     originalAdditionalText = OriginalAdditionalText}.

%%% ###########################################################################
%%% enum_SeverityLevel
%%% Definitions according to MafOamSpiNotificationFm_1.h in COM translated here
%%% to MIM definitions in ComFm.hrl.
%%%
%%% ###=====================================================================###
enum_SeverityLevel("2") ->
    ?SeverityLevel_WARNING;
enum_SeverityLevel("3") ->
    ?SeverityLevel_MINOR;
enum_SeverityLevel("4") ->
    ?SeverityLevel_MAJOR;
enum_SeverityLevel("5") ->
    ?SeverityLevel_CRITICAL;
enum_SeverityLevel(Value) ->
    throw({unknown_value, {enum_SeverityLevel, Value}}).

%%% ###########################################################################
%%% fmAlarm_delete
%%%
%%% ###=====================================================================###
fmAlarm_delete(Key) when Key /= '$end_of_table' ->
    case ets:lookup(?COI_fmAlarm_tmp, Key) of
	[] ->
	    mnesia:dirty_delete(fmAlarm, Key),
	    fmAlarm_delete(ets:next(fmAlarm, Key));
	_ ->
	    fmAlarm_delete(ets:next(fmAlarm, Key))
    end;
fmAlarm_delete('$end_of_table') ->
    ok.

%%% ###########################################################################
%%% fmAlarm_table
%%%
%%% ###=====================================================================###
fmAlarm_table(Comte_Alarms) ->
    comteAlarms_decode(Comte_Alarms),
    fmAlarm_write(ets:first(?COI_fmAlarm_tmp)),
    fmAlarm_delete(ets:first(fmAlarm)).

%%% ###########################################################################
%%% fmAlarm_write
%%%
%%% ###=====================================================================###
fmAlarm_write(Key) when Key /= '$end_of_table' ->
    [FmAlarm] = ets:lookup(?COI_fmAlarm_tmp, Key),
    case ets:lookup(fmAlarm, Key) of
	[FmAlarm] ->
	    fmAlarm_write(ets:next(?COI_fmAlarm_tmp, Key));
	_ ->
	    mnesia:dirty_write(FmAlarm),
	    fmAlarm_write(ets:next(?COI_fmAlarm_tmp, Key))
    end;
fmAlarm_write('$end_of_table') ->
    ok.

%%% ###########################################################################
%%% mnesia_table_event
%%%
%%% ###=====================================================================###
mnesia_table_event({write,
		    ?COMTE_Repl,
		    #?COMTE_Repl{instanceName = ?COMTE_AlrmsKey,
				 list = Alarms},
		    _ListOfOldObjects,
		    _ActivityId}) ->
    ets:delete_all_objects(?COI_fmAlarm_tmp),
    fmAlarm_table(Alarms);
mnesia_table_event({write, TableName, Mo, [], _ActivityId}) ->
    mo_notify(TableName, Mo, ?COI_MoCreated, undefined),
    ok;
mnesia_table_event({write, TableName, Mo, [OldMo], _ActivityId}) ->
    mo_notify(TableName, Mo, ?COI_AttrValueChange, OldMo),
    ok;
mnesia_table_event({delete,
		    TableName,
		    {MoClass, MoKey},
		    [OldMo],
		    _ActivityId}) ->
    mo_notify(TableName, mo_default(MoClass, MoKey), ?COI_MoDeleted, OldMo),
    ok;
mnesia_table_event(_) ->
    ok.

%%% ###########################################################################
%%% mo_attr_names
%%%
%%% ###=====================================================================###
mo_attr_names(fmAlarm) ->
    [_Key | AttrNames] = record_info(fields, fmAlarm),
    [list_to_binary(atom_to_list(Name)) || Name <- AttrNames].

%%% ###########################################################################
%%% mo_attr_values
%%%
%%% ###=====================================================================###
mo_attr_values(#fmAlarm{} = Record) ->
    [_MoClass, _MoKey | AttrValues] = tuple_to_list(Record),
    AttrValues;
mo_attr_values(_) ->
    [].

%%% ###########################################################################
%%% mo_changedAttrs
%%%
%%% ###=====================================================================###
mo_changedAttrs(?COI_MoCreated, _, _, AttrNames) ->
    {true, AttrNames};
mo_changedAttrs(?COI_MoDeleted, _, _, _) ->
    {true, []};
mo_changedAttrs(?COI_AttrValueChange, Values, Values, _) ->
    false;
mo_changedAttrs(?COI_AttrValueChange, NewValues, OldValues, AttrNames) ->
    {true, mo_changedAttrs(NewValues, OldValues, AttrNames)}.

%%% ###=====================================================================###
mo_changedAttrs([Value | NewTail], [Value | OldTail], [_ | NamesTail]) ->
    mo_changedAttrs(NewTail, OldTail, NamesTail);
mo_changedAttrs([_ | NewTail], [_ | OldTail], [ChangedAttrName | NamesTail]) ->
    [ChangedAttrName | mo_changedAttrs(NewTail, OldTail, NamesTail)];
mo_changedAttrs(_, _, _) ->
    [].

%%% ###########################################################################
%%% mo_default
%%%
%%% ###=====================================================================###
mo_default(fmAlarm, Key) ->
    #fmAlarm{fmAlarmId = Key};
mo_default(_, _) ->
    {undefined}.

%%% ###########################################################################
%%% mo_notify
%%%
%%% ###=====================================================================###
mo_notify(fmAlarm = TableName, #fmAlarm{} = Mo, EvtType, OldMo) ->
    {_, _, _, AlarmId} = Mo#fmAlarm.fmAlarmId,
    AttrNames = mo_attr_names(TableName),
    AttrValues = mo_attr_values(Mo),
    OldAttrValues = mo_attr_values(OldMo),
    case mo_changedAttrs(EvtType, AttrValues, OldAttrValues, AttrNames) of
	{true, ChangedAttrs} ->
	    CoiEvent = {coi_event_0, [{dn, [?DN_FmAlarm(AlarmId)]},
				      {event_type, EvtType},
				      {attributes, ChangedAttrs}]},
	    coiEvent:notify({coi_notification_0, [CoiEvent]});
	false ->
	    ok
    end;
mo_notify(_, _, _, _) ->
    ok.

%%% ###########################################################################
%%% nanosec_to_datetime
%%%
%%% ###=====================================================================###
nanosec_to_datetime(TotalNanoSec_Str) when is_list(TotalNanoSec_Str) ->
    nanosec_to_datetime(list_to_integer(TotalNanoSec_Str));
nanosec_to_datetime(TotalNanoSec) when is_integer(TotalNanoSec) ->
    TotalMicroSec = TotalNanoSec div 1000,
    Now_MegaSec = TotalMicroSec div 1000000000000,
    Now_MicroSec = TotalMicroSec rem 1000000,
    Now_Sec = (TotalMicroSec div 1000000) rem 1000000,
    Now = {Now_MegaSec, Now_Sec, Now_MicroSec},
    {{Y, Month, D}, {H, Min, Sec}} = calendar:now_to_local_time(Now),
    MilliSec = Now_MicroSec div 1000,
    Str_Month = integer_to_list(Month),
    Str_D = integer_to_list(D),
    Str_H = integer_to_list(H),
    Str_Min = integer_to_list(Min),
    Str_Sec = integer_to_list(Sec),
    Str_Milli = integer_to_list(MilliSec),
    integer_to_list(Y) ++
	"-" ++
	lists:nthtail(length(Str_Month), "00" ++ Str_Month) ++
	"-" ++
	lists:nthtail(length(Str_D), "00" ++ Str_D) ++
	"T" ++
	lists:nthtail(length(Str_H), "00" ++ Str_H) ++
	":" ++
	lists:nthtail(length(Str_Min), "00" ++ Str_Min) ++
	":" ++
	lists:nthtail(length(Str_Sec), "00" ++ Str_Sec) ++
	"." ++
	lists:nthtail(length(Str_Milli), "000" ++ Str_Milli) ++
	utc_diff(Now).

%%% ###########################################################################
%%% struct_additionalInfo
%%%
%%% ###=====================================================================###
struct_additionalInfo([[$0 | _]])->
    [];
struct_additionalInfo([Size | AddInfo]) ->
    try
	struct_additionalInfo(AddInfo, list_to_integer(Size))
    catch
	ErrClass : ErrReason ->
	    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				     {ErrClass, ErrReason},
				     {size, Size},
				     {addInfo, AddInfo}]),
	    []
    end.

%%% ###=====================================================================###
struct_additionalInfo([Name, Value | Tail], Size) when Size > 1 ->
    [#'AdditionalInformation'{name = base64_to_string(Name),
			      value = base64_to_string(Value)} |
     struct_additionalInfo(Tail, Size - 1)];
struct_additionalInfo([Name, Tail], Size) when Size == 1 ->
    [Value] = string:tokens(Tail, [0]),
    [#'AdditionalInformation'{name = base64_to_string(Name),
			      value = base64_to_string(Value)} |
     struct_additionalInfo(Tail, Size - 1)];
struct_additionalInfo(_, 0) ->
    [];
struct_additionalInfo(Tail, Size) ->
    Error = [{tail, Tail}, {remaining_size, Size}],
    throw({unknown_format, {struct_additionalInfo, Error}}).

%%% ###########################################################################
%%% utc_diff
%%%
%%% ###=====================================================================###
utc_diff(Now) ->
    Local =
	calendar:now_to_local_time(Now),
    Universal =
	calendar:now_to_universal_time(Now),
    Prefix =
	case Local >= Universal of
	    true ->
		"+";
	    false ->
		"-"
	end,
    {_, {Hdiff, Mdiff, _}} = calendar:time_difference(Universal, Local),
    Str_Hdiff = integer_to_list(Hdiff),
    Str_Mdiff = integer_to_list(Mdiff),
    _UtcDiff =
	Prefix ++
	lists:nthtail(length(Str_Hdiff), "00" ++ Str_Hdiff) ++
	":" ++
	lists:nthtail(length(Str_Mdiff), "00" ++ Str_Mdiff).

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###

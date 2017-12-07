%%%-------------------------------------------------------------------
%%% @author Magnus Liden <magnus.liden@ericsson.com>
%%% @doc
%%%     Helper functions
%%% @end
%%% Created :  8 Jul 2013 by Magnus Liden <magnus.liden@ericsson.com>
%%% @private
%%%-------------------------------------------------------------------
-module(comte_lib).

-include("comte_types.hrl").
-include("comte_event.hrl").
-include("comte_ac.hrl").
-include("comte_fm.hrl").
-include("comte_log.hrl").
-include("comte_pm_gp.hrl").

-export([
	 com_type/1,
	 com_type_atom/1,
         error_logger/6,
         error_logger/7,
         log_to_com/6,
         validate_alarm_opts/1,
         validate_notification/1,
         validate_pm_gp/1,
         notify_consumers/2,
         send_alarm/5,
         com_error_code/1,
         tab_create/2,
         tab_clear/1,
         tab_insert/3,
         tab_delete_entry/2,
         tab_lookup/2,
         tab_dump/1
        ]).

com_type(Tag) ->
    case Tag of
	uint8 -> ?UINT8;
	uint16 -> ?UINT16;
	uint32 -> ?UINT32;
	uint64 -> ?UINT64;
	int8 -> ?INT8;
	int16 -> ?INT16;
	int32 -> ?INT32;
	int64 -> ?INT64;
	string -> ?STRING;
	enum -> ?ENUM;
	bool -> ?BOOL;
	struct -> ?STRUCT;
	reference -> ?REFERENCE
    end.

com_type_atom(Type) ->
    case Type of
	?UINT8 -> uint8;
	?UINT16 -> uint16;
	?UINT32 -> uint32;
	?UINT64 -> uint64;
	?INT8 -> int8;
	?INT16 -> int16;
	?INT32 -> int32;
	?INT64 -> int64;
	?STRING -> string;
	?ENUM -> enum;
	?BOOL -> bool;
	?STRUCT -> struct;
	?REFERENCE -> reference
    end.

error_logger(Severity, Module, Line, Req, State, Fmt) ->
    error_logger(Severity, Module, Line, Req, State, Fmt, []).

error_logger(Severity, Module, Line, Req, State, Fmt, Args) ->
    Msg = lists:flatten(io_lib:format(Fmt, Args)),
    Report =
	[{pid,self()},
	 {module,Module},
	 {line,Line},
	 {req,Req},
	 {state,State},
	 {msg,Msg}],
    case Severity of
	info ->
	    comte_error_logger:info_report(Report);
	warning ->
	    comte_error_logger:warning_report(Report);
	error ->
	    comte_error_logger:error_report(Report)
    end.

com_error_code(Code) ->
    case Code of
	ok -> ?ComOk;
	try_again -> ?ComTryAgain;
	not_active -> ?ComNotActive;
	failure -> ?ComFailure;
	not_exist -> ?ComNotExist;
	already_exist -> ?ComAlreadyExist;
	aborted -> ?ComAborted;
	object_locked -> ?ComObjectLocked;
	prepare_failed -> ?ComPrepareFailed;
	commit_failed -> ?ComCommitFailed;
	invalid_argument -> ?ComInvalidArgument;
	validation_failed -> ?ComValidationFailed;
	no_resources -> ?ComNoResources;
	timeout -> ?ComTimeOut;
	_Other -> ?ComFailure %% Default failure
    end.



dn_to_binary(MO) when is_binary(MO) ->
    MO;
dn_to_binary([MO|MOs]) when is_binary(MO) ->
    dn_to_binary(MOs, MO).
%%
dn_to_binary([], Bin) ->
    Bin;
dn_to_binary([MO|MOs], Bin) when is_binary(MO) ->
    dn_to_binary(MOs, <<Bin/binary, ",", MO/binary>>).


tab_create([], _Opts) ->
    ok;
tab_create([TableName | Tables], Opts) ->
    case ets:info(TableName) of
        undefined ->
            TableName = ets:new(TableName, Opts);
        _Created ->
            ok = tab_clear([TableName])
    end,
    tab_create(Tables, Opts).

tab_clear([TableName | Tables]) ->
    true = ets:delete_all_objects(TableName),
    tab_clear(Tables);
tab_clear([]) ->
    ok.

tab_delete_entry(TableName, Entry) ->
    true = ets:delete(TableName, Entry),
    ok.

tab_insert(TableName, Key, Val) ->
    case ets:info(TableName) of
        undefined ->
            {error, {not_created, TableName}};
        _Created ->
            true = ets:insert(TableName, {Key, Val}),
            ok
    end.

tab_lookup(TableName, Key) ->
    case ets:lookup(TableName, Key) of
	[{Key, Cb}] -> {Key, Cb};
        [] -> {error, {not_found, TableName, Key}}
    end.

tab_dump(TableName) ->
    case ets:info(TableName) of
        undefined ->
            {error, {not_created, TableName}};
        _Created ->
            ets:tab2list(TableName)
    end.

log_to_com(File, Func, Line, Severity, Facility, Msg)
  when is_atom(File) orelse is_binary(File),
       is_atom(Func) orelse is_binary(Func),
       is_integer(Line),
       is_atom(Severity) orelse is_integer(Severity),
       is_integer(Facility),
       is_binary(Msg) ->
    try
      SevInt = sev_to_int(Severity),
      check_facility(Facility),
      check_line(Line),
      Args = {File, Func, Line, SevInt, Facility, Msg},
      case comte_bert_server:cast_to_com(undefined, comte_log, Args) of
          ok -> ok;
          Error -> {error, [{bert, Error}, {args, Args}]}
      end
    catch(Reason) ->
            {error, Reason}
    end;

log_to_com(File, Func, Line, Severity, Facility, Msg) ->
    {error, {invalid_input, {File, Func, Line, Severity, Facility, Msg}}}.

sev_to_int(emergency) -> ?LogSeverityEmergency;
sev_to_int(alert) -> ?LogSeverityAlert;
sev_to_int(critical) -> ?LogSeverityCritical;
sev_to_int(error) -> ?LogSeverityError;
sev_to_int(warning) -> ?LogSeverityWarning;
sev_to_int(notice) -> ?LogSeverityNotice;
sev_to_int(info) -> ?LogSeverityInfo;
sev_to_int(Severity)
  when is_atom(Severity) ->
    throw({unknown_severity, Severity});
sev_to_int(Severity)
  when is_integer(Severity), Severity >= ?LogSeverityEmergency ->
    case comte_types:uint16(Severity) of
	true -> Severity;
	false ->
	    throw({unknown_severity, Severity})
    end;
sev_to_int(Severity) ->
    throw({unknown_severity, Severity}).


check_facility(Facility) ->
    case Facility of
	_ when is_integer(Facility), 0 =< Facility, Facility =< 23 -> ok;
	?LogFacilityAlarm -> ok;
	?LogFacilityAlert -> ok;
	_ ->
	    throw({unknown_facility, Facility})
    end.

check_line(Line) ->
    comte_types:uint16(Line) orelse throw({invalid_line, Line}).



%% @private
send_alarm([], _DN, _MajorId, _MinorId, _Opts) ->
    {error, no_registered_consumers};
send_alarm(Consumers, DN, MajorId, MinorId, Opts) ->
    do_send_alarm(Consumers, dn_to_binary(DN), MajorId, MinorId, Opts).

do_send_alarm([], _DN, _MajorId, _MinorId, _Opts) ->
    ok;
do_send_alarm([{ConsumerId, EventType, FilterAddr} | Consumers],
               DN, MajorId, MinorId, Opts) ->
    Args = {ConsumerId, EventType, FilterAddr, [DN, MajorId, MinorId, Opts]},
    case comte_bert_server:cast_to_com(undefined, notify_alarm, Args) of
        ok ->
            do_send_alarm(Consumers, DN, MajorId, MinorId, Opts);
        Error ->
            {error, [{bert, Error}, {args, Args}]}
    end.

notify_consumers([], _Notification) ->
    {error, no_registered_consumers};
notify_consumers(Consumers, Notification) ->
    do_notify_consumers(Consumers, Notification).

do_notify_consumers([], #cm_notification_1{}) ->
    ok;

do_notify_consumers([{ConsumerId, EventType, FilterAddr} | Consumers],
                 #cm_notification_1{trans_id=TId,
                                    source=Source,
                                    events=Events}=Ntf) ->
    Args =  {ConsumerId, EventType, FilterAddr,
             [TId, Source, Events]},
    case comte_bert_server:cast_to_com(undefined,
                                       notify_cm_1,
                                       Args) of
        ok ->
            do_notify_consumers(Consumers, Ntf);
        Error ->
            {error, [{bert, Error}, {args, Args}]}
    end;

do_notify_consumers([], #pm_gp_ready_1{}) ->
    ok;

do_notify_consumers([{ConsumerId, EventType, FilterAddr} | Consumers],
                    #pm_gp_ready_1{gp_id = GpId,
                                   job_id = JobId,
                                   gp_start_time = StartTime,
                                   gp_end_time = StopTime} = Ntf) ->
    Args = {ConsumerId, EventType, FilterAddr,
            {GpId, JobId, StartTime, StopTime}},
    case comte_bert_server:cast_to_com(undefined,
                                       pm_gp_ready_1,
                                       Args) of
        ok ->
            do_notify_consumers(Consumers, Ntf);
        Error ->
            {error, [{bert, Error}, {args, Args}]}
    end.

validate_pm_gp(#pm_gp_ready_1{gp_id = GpId,
                                   job_id = JobId,
                                   gp_start_time = StartTime,
                                   gp_end_time = StopTime}) ->
    case {comte_types:uint64(GpId), 
         comte_types:com_string(JobId), 
         comte_types:uint64(StartTime),
         comte_types:uint64(StopTime)} of
        {true, true, true, true} -> ok;
        _ -> {error, type_error}
    end.

validate_alarm_opts(OptList) ->
    try
        RefKeys = [severity, add_text, add_info],
        _FlatAlarmOpts = validate_alarm_opts(RefKeys,
                                             OptList,
                                             [])
    catch _:Reason ->
            {error, Reason}
    end.

validate_alarm_opts([], [], Acc) ->
    lists:reverse(Acc);
validate_alarm_opts([], UnknownOpts, _Acc) ->
    throw({unknown_alarm_opts, UnknownOpts});
validate_alarm_opts([Key|Keys], OptList, Acc) ->
    case proplists:get_value(Key, OptList) of
        undefined ->
            validate_alarm_opts(Keys, OptList,
                               [[] | Acc]);
        Val ->
            ok = validate_alarm_opt_val(Key, Val),
            UpdOptList = lists:keydelete(Key, 1, OptList),
            validate_alarm_opts(Keys, UpdOptList,
                                [Val | Acc])
    end.

validate_alarm_opt_val(severity, Sev)
  when Sev == ?FmSeverityCleared;
       Sev == ?FmSeverityIndeterminate;
       Sev == ?FmSeverityWarning;
       Sev == ?FmSeverityMinor;
       Sev == ?FmSeverityMajor;
       Sev == ?FmSeverityCritical ->
    ok;
validate_alarm_opt_val(severity, Sev) ->
    throw({invalid_severity, Sev});

validate_alarm_opt_val(add_text, Text)
  when is_binary(Text) -> ok;

validate_alarm_opt_val(add_text, Text) ->
    throw({invalid_additional_text, Text});

validate_alarm_opt_val(add_info, []) ->
    throw({empty_additional_info_list,
           []});
validate_alarm_opt_val(add_info, InfoList)
  when is_list(InfoList) ->
    F = fun({Name, Value})
              when is_binary(Name),
                   is_binary(Value) ->
                false;
           (_Other) ->
                true
        end,
    case lists:filter(F, InfoList) of
        [] -> ok;
        Other -> throw({invalid_additional_info,
                       Other})
    end;

validate_alarm_opt_val(add_info, InfoList) ->
    throw({invalid_additional_info_list,
           InfoList}).

validate_notification(#cm_notification_1{}=N)  ->
    try
        validate_trans_id(N#cm_notification_1.trans_id),
        validate_source(N#cm_notification_1.source),
        Events = prepare_com_events(N#cm_notification_1.events),
        N#cm_notification_1{events=Events}
    catch _:Reason ->
            {error, Reason}
    end.


prepare_com_events([]) ->
    throw(no_events_in_notification);
prepare_com_events(ComEventList) when is_list(ComEventList) ->
    F = fun(#cm_event_1{dn=DN} = Event) ->
                AttrList = Event#cm_event_1.attributes,
                EventType = Event#cm_event_1.event_type,

                validate_event_type(EventType, AttrList),
                UpdAttrs = validate_attr_values(AttrList),
                Event#cm_event_1{dn=dn_to_binary(DN),
                                 attributes=UpdAttrs}
        end,
    lists:map(F, ComEventList).

validate_trans_id(TransId) ->
    case comte_types:transaction_id(TransId) of
	true -> ok;
	false -> throw({out_of_range, trans_id, TransId})
    end.

validate_source(Source) ->
    case lists:member(Source, sources()) of
        true -> ok;
        _ -> throw({bad_source_indicator, Source})
    end.

sources() ->
    [?ResourceOperation, ?ManagementOperation,
     ?SonOperation, ?Unknown].

validate_event_type(?MoDeleted, []) ->
    ok;
validate_event_type(?MoDeleted, Other) ->
    throw({empty_attribute_list_expected, Other});
validate_event_type(?MoCreated, AttrList)
  when is_list(AttrList) ->
    ok;
validate_event_type(?Overflow, []) ->
    ok;
validate_event_type(?Overflow, Other) ->
    throw({empty_attribute_list_expected, Other});
validate_event_type(?AttributeValueChange, []) ->
    throw(non_empty_attribute_list_expected);
validate_event_type(?AttributeValueChange, Attrs)
  when is_list(Attrs) -> ok.


validate_attr_values([]) -> [];
validate_attr_values([NamedAttr | AttrRest]) ->
    comte_types:com_named_attribute(NamedAttr) orelse
	throw({bad_attr_value, NamedAttr}),
    {AttrName, ComValue} = NamedAttr,
    case comte_types:com_value(ComValue) of
	true ->
	    validate_attr_values([{AttrName,[ComValue]} | AttrRest]);
	false ->
	    [NamedAttr |validate_attr_values(AttrRest)]
    end.

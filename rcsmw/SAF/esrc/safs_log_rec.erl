%%--------------------------------------------------------------------
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_log_rec.erl
%%
%% Description: Verification of format expressions and formatting of
%%              log records according to a given format expression.
%%
%%
%%--------------------------------------------------------------------
-module(safs_log_rec).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

%% -include("safs_log_db.hrl").
-include("safs_log.hrl").
-include("safs_ais.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([format/4, 
	 verify_format_expr/2,
	 get_token_value/3]).

%%-compile(export_all).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Test exports
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(Cr, "@Cr").
-define(Ct, "@Ct").
-define(Ch, "@Ch").
-define(Cn, "@Cn").
-define(Cs, "@Cs").
-define(Ca, "@Ca").
-define(Cm, "@Cm").
-define(CM, "@CM").
-define(Cd, "@Cd").
-define(CD, "@CD").
-define(Cy, "@Cy").
-define(CY, "@CY").
-define(Cc, "@Cc").
-define(Cx, "@Cx").
-define(Cb, "@Cb").
-define(Ci, "@Ci").
-define(Sl, "@Sl").
-define(Sv, "@Sv").
-define(Ni, "@Ni").
-define(Nt, "@Nt").
-define(Nh, "@Nh").
-define(Nn, "@Nn").
-define(Ns, "@Ns").
-define(Na, "@Na").
-define(Nm, "@Nm").
-define(NM, "@NM").
-define(Nd, "@Nd").
-define(ND, "@ND").
-define(Ny, "@Ny").
-define(NY, "@NY").
-define(Nc, "@Nc").
-define(Ne, "@Ne").
-define(No, "@No").
-define(Ng, "@Ng").
-define(Cms, "@Cq").
-define(Nms, "@Nq").

-define(COMMON_TOKENS, [?Cr, ?Ct, ?Ch, ?Cn, ?Cs, ?Ca, ?Cm, 
			?CM, ?Cd, ?CD, ?Cy, ?CY, ?Cc, ?Cx, ?Cb, ?Cms]).
-define(APPL_TOKENS, [?Ci]).
-define(SYSTEM_TOKENS, [?Sl, ?Sv]).
-define(ALARM_TOKENS, []).
-define(NOTIF_TOKENS, [?Ni, ?Nt, ?Nh, ?Nn, ?Ns, ?Na, ?Nm, ?NM
		       ?Nd, ?ND, ?Ny, ?NY, ?Nc, ?Ne, ?No, ?Ng, ?Nms]).

-define(ALL_TOKENS, ?COMMON_TOKENS ++ ?APPL_TOKENS ++ ?SYSTEM_TOKENS ++ 
	    ?NOTIF_TOKENS).

-define(FIELD_SIZE_TOKENS, [?Cb, ?Ci, ?Sl, ?Ne, ?No, ?Ng]).

-define(GLOBAL_TOKENS, [?Ca, ?Cx, ?Na]).
 
-define(PRINTABLE_CHAR_B, " ").
-define(PRINTABLE_CHAR_E, "~").

-define(GEN_HDR(LR), (LR#safsLogRecord.logHeader)#safsLogHeader.genericHdr).
-define(NTF_HDR(LR), (LR#safsLogRecord.logHeader)#safsLogHeader.ntfHdr).

-define(NTF_OBJ_NOTIFS_START_HEX, 16#1000).
-define(NTF_OBJ_CREATION_HEX,     16#1001).
-define(NTF_OBJ_DELETION_HEX,     16#1002).

-define(NTF_ATTR_NOTIFS_START_HEX, 16#2000).
-define(NTF_ATTR_ADDED_HEX,        16#2001).
-define(NTF_ATTR_REMOVED_HEX,      16#2002).
-define(NTF_ATTR_CHANGED_HEX,      16#2003).
-define(NTF_ATTR_RESET_HEX,        16#2004).

-define(NTF_STATE_CHANGE_NOTIFS_START_HEX, 16#3000).
-define(NTF_OBJ_STATE_CHANGE_HEX,          16#3001).

-define(NTF_ALARM_NOTIFS_START_HEX,  16#4000).
-define(NTF_ALARM_COMMUNICATION_HEX, 16#4001).
-define(NTF_ALARM_QOS_HEX,           16#4002).
-define(NTF_ALARM_PROCESSING_HEX,    16#4003).
-define(NTF_ALARM_EQUIPMENT_HEX,     16#4004).
-define(NTF_ALARM_ENVIRONMENT_HEX,   16#4005).

-define(NTF_SEC_ALARM_NOTIFS_START_HEX, 16#5000).
-define(NTF_INTEGRITY_VIOLATION_HEX,    16#5001).
-define(NTF_OPERATION_VIOLATION_HEX,    16#5002).
-define(NTF_PHYSICAL_VIOLATION_HEX,     16#5003).
-define(NTF_SEC_SERVICE_VIOLATION_HEX,  16#5004).
-define(NTF_TIME_VIOLATION_HEX,         16#5005).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External Interface Functions
%%======================================================================
%%--------------------------------------------------------------------
%% Function: format/1
%% Description: Formats a Log Record according to a Format Expression.
%%--------------------------------------------------------------------
format(LogRec, LogNo, LogFmt, MaxSize) 
  when is_record(LogRec, safsLogRecord),
       is_integer(LogNo),
       is_list(LogFmt),
       is_integer(MaxSize) ->
    try
	Tokens = tokenize_with_size(LogFmt),
	GTk = get_global_tokens(Tokens),
	NewLogRec = prepare_log_rec(LogRec),
	Data = [format_log_rec(Token, NewLogRec, {LogNo, GTk}) || 
		   Token <- Tokens], 
	Out = append_termination(iolist_to_binary(Data), Tokens, MaxSize),
	{ok, Out}
    catch
	_:E ->
	    ST = erlang:get_stacktrace(),
	    error_logger:error_msg("Failed to format LOG data:\n\t~p\n"
				   "LOG Record:\n\t~p\n"
				   "Format:\n\t~p\n"
				   "Stacktrace:\n\t~p\n", 
				   [E, LogRec, LogFmt, ST]),
	    {error, sa_ais_err_invalid_param}
    end;

format(LogRec, LogNo, LogFmt, MaxSize) 
  when is_binary(LogFmt) ->
    format(LogRec, LogNo, btl(LogFmt), MaxSize).

%%--------------------------------------------------------------------
%% Function: verify_format_expr/1
%% Description: Checks syntax of a Log Record Format Expression.
%%--------------------------------------------------------------------
verify_format_expr(Type, LogFmt) 
  when is_list(Type), 
       is_list(LogFmt) ->
    try
	ok = check_non_printable(LogFmt),
	Tokenized = tokenize(LogFmt),
	ok = check_field_sizes(Tokenized),
	Tokens = strip_literals(Tokenized),
	ok = check_valid(Tokens, Type),
	ok = check_duplicate(Tokens)
    catch
    	_:E ->
    	    error_logger:warning_msg("~p: Invalid Fmt Expr:\n\t ~p\n", 
				     [?MODULE, E]),
    	    {error, sa_ais_err_invalid_param}
    end;

verify_format_expr(Type, LogFmt) 
  when is_binary(Type) -> 
    verify_format_expr(btl(Type), LogFmt);

verify_format_expr(Type, LogFmt) when is_binary(LogFmt) -> 
    verify_format_expr(Type, btl(LogFmt)).


%%--------------------------------------------------------------------
%% Function: get_token_value/3
%% Description: Finds in a log entry (String) the start position and
%%              length where the Token is defined.
%% Returns {ok, Result} || {error, {token_not_found, Token}}
%%--------------------------------------------------------------------
get_token_value(Token, Fmt, String) ->
    Tokens = tokenize_with_size(Fmt),
    gtv(get_positions(Tokens, Token, String), String, Token).

gtv({Start, Length}, String, _) ->
    {ok, string:substr(String, Start, Length)};
gtv(undefined, _, Token) ->
    {error, {token_not_found, Token}}.
	    
%%======================================================================
%% Test Functions
%%======================================================================

%%====================================================================
%% Internal functions
%%====================================================================
prepare_log_rec(LogRec) ->
    TS = get_ts(LogRec#safsLogRecord.logTimeStamp),
    NewLogRec = LogRec#safsLogRecord{logTimeStamp = {TS, nsecs_to_lc(TS)}},
    case NewLogRec#safsLogRecord.logHdrType of
	?LOG_HEADER_NTF ->
	    Hdr = NewLogRec#safsLogRecord.logHeader,
	    NtfHdr = Hdr#safsLogHeader.ntfHdr,
	    ET = NtfHdr#safsLogNtfLogHeader.eventTime,
	    NtfHeader = 
		NtfHdr#safsLogNtfLogHeader{eventTime = {ET, nsecs_to_lc(ET)}},
	    Header = Hdr#safsLogHeader{ntfHdr = NtfHeader},
	    NewLogRec#safsLogRecord{logHeader = Header};
	_ ->
	    NewLogRec
    end.


append_termination(Bin, Tokens, MaxSize) ->
    Cx = lists:member(?Cx, Tokens),
    case size(Bin) of
	Size when Cx, Size > MaxSize - 3 ->
	    BinPart = binary:part(Bin, 0, MaxSize - 3),
	    <<BinPart/binary, $T, $\r, $\n>>;
	Size when not Cx, Size > MaxSize - 2 ->
	    BinPart = binary:part(Bin, 0, MaxSize - 2),
	    <<BinPart/binary, $\r, $\n>>;
	Size when Cx ->
	    Pad = pad_space(MaxSize - Size - 3),
	    <<Bin/binary, $C, Pad/binary, $\r, $\n>>;
	Size ->
	    Pad = pad_space(MaxSize - Size - 2),
	    <<Bin/binary, Pad/binary, $\r, $\n>>
    end.


%%====================================================================
%% Format Expr checks
%%====================================================================
check_non_printable(LogFmt) ->
    case re:run(LogFmt, "[[:^print:]]") of
	nomatch ->
	    ok;
	{match, _} ->
	    throw({"Non printable characters in format expr:", LogFmt})
    end.


check_field_sizes(Tokenized) ->
    SizeTokens = [Token || Token <- add_field_size(Tokenized), is_tuple(Token)],
    check_field_size(SizeTokens).


check_field_size([{?Ci, FS} | T]) ->
    FS rem 2 =:= 0 orelse throw({"Odd field size for @Ci:", FS}),
    check_field_size(T);

check_field_size([{?Ne, FS} | T]) ->
    FS >= 6 orelse throw({"Too small field size for @Ne:", FS}),
    check_field_size(T);
    
check_field_size([_H | T]) ->
    check_field_size(T);
	    
check_field_size([]) ->
    ok.    


check_valid(Tokens, Type) ->
    ValidTokens = get_valid_tokens(Type),
    [lists:member(Token, ValidTokens) orelse check_valid_all(Token, Type)
     || Token <- Tokens],
    ok.


check_valid_all(Token, Type) ->
    case lists:member(Token, ?ALL_TOKENS) of
	 true ->
	    throw({"Invalid token for type", Token, Type});
	_False ->
	    throw({"Invalid token", Token})
    end.


get_valid_tokens(?LOG_SYSTEM) ->
    ?COMMON_TOKENS ++ ?SYSTEM_TOKENS;

get_valid_tokens(Type) when Type =:= ?LOG_ALARM;
			    Type =:= ?LOG_NOTIFY ->
    ?COMMON_TOKENS ++ ?NOTIF_TOKENS;

get_valid_tokens(_App) ->
    ?APPL_TOKENS ++ ?COMMON_TOKENS ++ ?SYSTEM_TOKENS.


check_duplicate([Token | Tokens]) ->
    not lists:member(Token, Tokens) orelse throw({"Duplicated token:", Token}),
    check_duplicate(Tokens);

check_duplicate([]) ->
    ok.

%%====================================================================
%% Format log record
%%====================================================================
format_log_rec(?Cr, _LR, {LNo, _}) ->
    io_lib:format("~10w", [LNo]);

format_log_rec(?Ct, LR, _) ->
    TS = get_ts_sec(LR),
    io_lib:format("0x~16.16.0B", [TS]);

format_log_rec(?Ch, LR, {_, GTk}) ->
    TS = get_ts_lt(LR),
    H = get_hours(hours(TS), GTk),
    io_lib:format("~2.2.0w", [H]);

format_log_rec(?Cn, LR, _) ->
    TS = get_ts_lt(LR),
    io_lib:format("~2.2.0w", [minutes(TS)]);

format_log_rec(?Cs, LR, _) ->
    TS = get_ts_lt(LR),
    io_lib:format("~2.2.0w", [seconds(TS)]);

format_log_rec(?Cms, LR, _) ->
     TS = get_ts_sec(LR),
     io_lib:format("~3.3.0w", [milli_seconds(TS)]);

format_log_rec(?Ca, LR, _) ->
    TS = get_ts_lt(LR),
    get_am_pm(hours(TS));

format_log_rec(?Cm, LR, _) ->
    TS = get_ts_lt(LR),
    io_lib:format("~2.2.0w", [month(TS)]);

format_log_rec(?CM, LR, _) ->
    TS = get_ts_lt(LR),
    month_string(TS);

format_log_rec(?Cd, LR, _) ->
    TS = get_ts_lt(LR),
    io_lib:format("~2.2.0w", [day(TS)]);

format_log_rec(?CD, LR, _) ->
    TS = get_ts_lt(LR),
    day_string(TS);

format_log_rec(?Cy, LR, _) ->
    TS = get_ts_lt(LR),
    io_lib:format("~2.2.0w", [year(TS) rem 100]);
 
format_log_rec(?CY, LR, _) ->
    TS = get_ts_lt(LR),
    io_lib:format("~w", [year(TS)]);
 
format_log_rec(?Cc, LR, _) 
  when LR#safsLogRecord.logHdrType =:= ?LOG_HEADER_GENERIC ->
    NtfClassId = (?GEN_HDR(LR))#safsLogGenericLogHeader.notificationClassId,
    format_ntf_class_id(NtfClassId);

format_log_rec(?Cc, LR, _) ->
    NtfClassId = (?NTF_HDR(LR))#safsLogNtfLogHeader.notificationClassId,
    format_ntf_class_id(NtfClassId);

format_log_rec(?Cx, _LR, _) ->
    <<>>;

format_log_rec(?Cb, LR, _) ->
    make_printable(format_log_buf(LR));

format_log_rec({?Cb, Fsz}, LR, _) ->
    make_printable(format_log_buf(LR, Fsz));

format_log_rec(?Ci, LR, _) ->
    to_hex_string(format_log_buf(LR));

format_log_rec({?Ci, Fsz}, LR, _) ->
    to_hex_string(format_log_buf(LR, Fsz div 2));

format_log_rec(?Sl, LR, _) ->
    make_printable((?GEN_HDR(LR))#safsLogGenericLogHeader.logSvcUsrName);

format_log_rec({?Sl, Fsz}, LR, _) ->
    Name = ltb((?GEN_HDR(LR))#safsLogGenericLogHeader.logSvcUsrName),
    pad_to_size(make_printable(Name), Fsz);

format_log_rec(?Sv, LR, _) ->
    Sev = (?GEN_HDR(LR))#safsLogGenericLogHeader.logSeverity,
    format_log_severity(Sev);

format_log_rec(?Ni, LR, _) ->
    NtfId = (?NTF_HDR(LR))#safsLogNtfLogHeader.notificationId,
    io_lib:format("0x~16.16.0B", [NtfId]);

format_log_rec(?Nt, LR, _) ->
    ET = get_et_sec(LR),
    io_lib:format("0x~16.16.0B", [ET]);

format_log_rec(?Nh, LR, {_, GTk}) ->
    TS = get_et_lt(LR),
    H = get_hours(hours(TS), GTk, ?Na),
    io_lib:format("~2.2.0w", [H]);

format_log_rec(?Nn, LR, _) ->
    TS = get_et_lt(LR),
    io_lib:format("~2.2.0w", [minutes(TS)]);

format_log_rec(?Ns, LR, _) ->
    TS = get_et_lt(LR),
    io_lib:format("~2.2.0w", [seconds(TS)]);

format_log_rec(?Nms, LR, _) ->
     TS = get_et_sec(LR),
     io_lib:format("~3.3.0w", [milli_seconds(TS)]);

format_log_rec(?Na, LR, _) ->
    ET = get_et_lt(LR),
    get_am_pm(hours(ET));

format_log_rec(?Nm, LR, _) ->
    TS = get_et_lt(LR),
    io_lib:format("~2.2.0w", [month(TS)]);

format_log_rec(?NM, LR, _) ->
    TS = get_et_lt(LR),
    month_string(TS);

format_log_rec(?Nd, LR, _) ->
    TS = get_et_lt(LR),
    io_lib:format("~2.2.0w", [day(TS)]);

format_log_rec(?ND, LR, _) ->
    TS = get_et_lt(LR),
    day_string(TS);

format_log_rec(?Ny, LR, _) ->
    TS = get_et_lt(LR),
    io_lib:format("~2.2.0w", [year(TS) rem 100]);
 
format_log_rec(?NY, LR, _) ->
    TS = get_et_lt(LR),
    io_lib:format("~w", [year(TS)]);

format_log_rec(?Nc, LR, _) ->
    NtfClassId = (?NTF_HDR(LR))#safsLogNtfLogHeader.notificationClassId,
    format_ntf_class_id(NtfClassId);

format_log_rec(?Ne, LR, _) ->
    Type = (?NTF_HDR(LR))#safsLogNtfLogHeader.eventType,
    Num = ntf_event_type(Type),
    io_lib:format("0x~.16B", [Num]);

format_log_rec({?Ne, Fsz}, LR, _) ->
    Type = (?NTF_HDR(LR))#safsLogNtfLogHeader.eventType,
    Num = ntf_event_type(Type),
    Pos = itl(Fsz - 2),
    io_lib:format("0x~" ++ Pos ++ ".16.0B", [Num]);

format_log_rec(?No, LR, _) ->
    make_printable((?NTF_HDR(LR))#safsLogNtfLogHeader.notificationObject);

format_log_rec({?No, Fsz}, LR, _) ->
    Name = (?NTF_HDR(LR))#safsLogNtfLogHeader.notificationObject,
    pad_to_size(make_printable(Name), Fsz);

format_log_rec(?Ng, LR, _) ->
    make_printable((?NTF_HDR(LR))#safsLogNtfLogHeader.notifyingObject);

format_log_rec({?Ng, Fsz}, LR, _) ->
    Name = (?NTF_HDR(LR))#safsLogNtfLogHeader.notifyingObject,
    pad_to_size(make_printable(Name), Fsz);

format_log_rec(Str, _LR, _) ->
    Str.


format_ntf_class_id(#safsNtfClassId{vendorId = VId, 
				    majorId = MaId, 
				    minorId = MiId}) ->
    io_lib:format("NCI[0x~8.16.0B,0x~4.16.0B,0x~4.16.0B]", 
		  [VId, MaId, MiId]);

format_ntf_class_id(undefined) ->
    <<>>.


format_log_buf(LR, Size) ->
    Buf = max_size(format_log_buf(LR), Size),
    Pad = pad_space(Size - size(Buf)),
    <<Buf/binary, Pad/binary>>.
    

format_log_buf(#safsLogRecord{logBuffer = Buf}) when Buf =/= undefined ->
    ltb(Buf);

format_log_buf(_) ->
    <<>>.


format_log_severity(?LOG_EMERGENCY) ->
    "EM";    
format_log_severity(?LOG_ALERT) ->
    "AL";
format_log_severity(?LOG_CRITICAL) ->
    "CR";
format_log_severity(?LOG_ERROR) ->
    "ER";
format_log_severity(?LOG_WARNING) ->
    "WA";
format_log_severity(?LOG_NOTICE) ->
    "NO";
format_log_severity(?LOG_INFO) ->
    "IN".
    

ntf_event_type(?NTF_OBJ_NOTIFS_START) ->
    ?NTF_OBJ_NOTIFS_START_HEX;
ntf_event_type(?NTF_OBJ_CREATION) ->
    ?NTF_OBJ_CREATION_HEX;
ntf_event_type(?NTF_OBJ_DELETION) ->
    ?NTF_OBJ_DELETION_HEX;
ntf_event_type(?NTF_ATTR_NOTIFS_START) ->
    ?NTF_ATTR_NOTIFS_START_HEX;
ntf_event_type(?NTF_ATTR_ADDED) ->
    ?NTF_ATTR_ADDED_HEX;
ntf_event_type(?NTF_ATTR_REMOVED) ->
    ?NTF_ATTR_REMOVED_HEX;
ntf_event_type(?NTF_ATTR_CHANGED) ->
    ?NTF_ATTR_CHANGED_HEX;
ntf_event_type(?NTF_ATTR_RESET) ->
    ?NTF_ATTR_RESET_HEX;
ntf_event_type(?NTF_STATE_CHANGE_NOTIFS_START) ->
    ?NTF_STATE_CHANGE_NOTIFS_START_HEX;
ntf_event_type(?NTF_OBJ_STATE_CHANGE) ->
    ?NTF_OBJ_STATE_CHANGE_HEX;
ntf_event_type(?NTF_ALARM_NOTIFS_START) ->
    ?NTF_ALARM_NOTIFS_START_HEX;
ntf_event_type(?NTF_ALARM_COMMUNICATION) ->
    ?NTF_ALARM_COMMUNICATION_HEX;
ntf_event_type(?NTF_ALARM_QOS) ->
    ?NTF_ALARM_QOS_HEX;
ntf_event_type(?NTF_ALARM_PROCESSING) ->
    ?NTF_ALARM_PROCESSING_HEX;
ntf_event_type(?NTF_ALARM_EQUIPMENT) ->
    ?NTF_ALARM_EQUIPMENT_HEX;
ntf_event_type(?NTF_ALARM_ENVIRONMENT) ->
    ?NTF_ALARM_ENVIRONMENT_HEX;
ntf_event_type(?NTF_SEC_ALARM_NOTIFS_START) ->
    ?NTF_SEC_ALARM_NOTIFS_START_HEX;
ntf_event_type(?NTF_INTEGRITY_VIOLATION) ->
    ?NTF_INTEGRITY_VIOLATION_HEX;
ntf_event_type(?NTF_OPERATION_VIOLATION) ->
    ?NTF_OPERATION_VIOLATION_HEX;
ntf_event_type(?NTF_PHYSICAL_VIOLATION) ->
    ?NTF_PHYSICAL_VIOLATION_HEX;
ntf_event_type(?NTF_SEC_SERVICE_VIOLATION) ->
    ?NTF_SEC_SERVICE_VIOLATION_HEX;
ntf_event_type(?NTF_TIME_VIOLATION) ->
    ?NTF_TIME_VIOLATION_HEX.


get_global_tokens(Tokens) ->
    [Token || Token <- Tokens, lists:member(Token, ?GLOBAL_TOKENS)].


tokenize_with_size(Fmt) ->
    add_field_size(tokenize(Fmt)).


tokenize([$@, C1, C2 | T]) ->
    [[$@, C1, C2] | tokenize(T)];

tokenize([$@ | T] = R) when length(T) < 2 ->
    [R];

tokenize(Fmt) when Fmt =/= [] ->
    {L1, L2} = lists:splitwith(fun(Ch) -> Ch =/= $@ end, Fmt),
    [L1 | tokenize(L2)];

tokenize([]) ->
    [].
    

strip_literals(Tokens) ->
    lists:filter(fun([$@ | _]) ->
			 true;
		    (_) ->
			 false
		 end, Tokens).
    

add_field_size([Tk, N | T]) ->
    case lists:member(Tk, ?FIELD_SIZE_TOKENS) of
	true ->
	    add_field_size(Tk, N, T);
	false ->
	    [Tk | add_field_size([N | T])]
    end;

add_field_size(T) ->
    T.


add_field_size(Tk, N, T) ->
    F = fun(Ch) -> 
		($0 =< Ch) and (Ch =< $9)
	end,
    case lists:splitwith(F, N) of
	{N1, N2} when N1 =/= [] ->
	    [{Tk, lti(N1)}, N2 | add_field_size(T)];
	_ ->
	    [Tk | add_field_size([N | T])]
    end.


pad_to_size(Data, MaxSize) ->
    Bin = max_size(ltb(Data), MaxSize),
    Pad = pad_space(MaxSize - size(Bin)),
    <<Bin/binary, Pad/binary>>.


max_size(Bin, MaxSize) when size(Bin) > MaxSize ->
    binary:part(Bin, 0, MaxSize);
max_size(Bin, _MaxSize) ->
    Bin.


pad_space(N) when N > 0 ->
    binary:copy(<<" ">>, N);
pad_space(_N) ->
    <<>>.


make_printable(undefined) ->
    <<>>;

make_printable(Buf) ->
    Data = re:replace([Buf], "[[:^print:]]", "_", [global]),
    iolist_to_binary(Data).


to_hex_string(Bin) ->
    [dec_to_hex(B) || <<B>> <= Bin].
    

dec_to_hex(N) when N > 16#F ->
    integer_to_list(N,16);

dec_to_hex(N) ->
    [$0 | integer_to_list(N,16)].

    
get_ts_lt(#safsLogRecord{logTimeStamp = {_, TS}}) ->
    TS.


get_ts_sec(#safsLogRecord{logTimeStamp = {TS, _}}) ->
    TS.


get_et_lt(LR) ->
    {_, ET} = (?NTF_HDR(LR))#safsLogNtfLogHeader.eventTime,
    ET.


get_et_sec(LR) ->
    {ET, _} = (?NTF_HDR(LR))#safsLogNtfLogHeader.eventTime,
    ET.


get_am_pm(H) when H > 12 -> 
    "pm";

get_am_pm(_H) -> 
    "am".


get_hours(H, GTk) ->
    get_hours(H, GTk, ?Ca).


get_hours(H, GTk, Tk) ->
   case lists:member(Tk, GTk) of
       true when H > 12 ->
	   H - 12; 
       _False ->
	   H
   end.


month_string({{_Y, M, _D}, _}) ->
    httpd_util:month(M).
    

day_string({Date, _}) ->
    D = calendar:day_of_the_week(Date),
    httpd_util:day(D).
    

year({{Y, _M, _D}, _}) ->
    Y.


month({{_Y, M, _D}, _}) ->
    M.


day({{_Y, _M, D}, _}) ->
    D.


get_ts(?SA_TIME_UNKNOWN) ->
    {A,B,C} = os:timestamp(),
    (A* 1000000000000 + B*1000000 + C) * 1000;
get_ts(TS) ->
    TS.

nsecs_to_lc(TS) ->
    MicroSecs = erlang:round(TS/1000),
    calendar:now_to_local_time(msecs_to_now(MicroSecs)).
   
msecs_to_now(MSecs) ->
    Meg = 1000000,
    Secs = MSecs div Meg,
    {Secs div Meg, Secs rem Meg, MSecs rem Meg}.

secs_to_now(Secs) ->
    {Secs div 1000000, Secs rem 1000000, 0}.


hours({_, {H, _M, _S}}) ->
    H;
hours(Secs) when is_integer(Secs) ->
    hours(secs_to_now(Secs));	    
hours({_MeS, _S, _MiS} = TS) ->
    hours(calendar:now_to_local_time(TS)).


minutes({_, {_H, M, _S}}) ->
    M;
minutes(Secs) when is_integer(Secs) ->
    minutes(secs_to_now(Secs));	    
minutes({_MeS, _S, _MiS} = TS) ->
    minutes(calendar:now_to_local_time(TS)).


seconds({_, {_H, _M, S}}) ->
    S;
seconds(Secs) when is_integer(Secs) ->
    seconds(secs_to_now(Secs));	    
seconds({_MeS, _S, _MiS} = TS) ->
    seconds(calendar:now_to_local_time(TS)).
    

milli_seconds(TS) ->
     (TS div 1000000) rem 1000.


lti(L) when is_list(L) ->
    list_to_integer(L).
%% lti(I) when is_integer(I) ->
%%     I.


itl(I) when is_integer(I) ->
    integer_to_list(I).
%% itl(L) when is_list(L) ->
%%     L.


ltb(L) when is_list(L) ->
    list_to_binary(L);
ltb(B) when is_binary(B) ->
    B;
ltb(undefined) ->
    <<>>.


btl(B) when is_binary(B) ->
    binary_to_list(B).
%% btl(L) when is_list(L) ->
%%     L;
%% btl(undefined) ->
%%     [].

%%====================================================================
%% Find the position and length in a log entry for a specific token
%%====================================================================
get_positions(Tokens, Token, String) ->
    get_positions(Tokens, Token, String, 1).

%%---------------------------------------
%% Token not found
%%---------------------------------------
get_positions([], _Token, _String, _Pos) ->
    undefined;
%%---------------------------------------
%% Token found, check the length
%%---------------------------------------
get_positions([Token | T], Token, String, Pos) ->
    case gp_length(token_length(Token), T, string:substr(String, Pos)) of
	Length when is_integer(Length) ->
	    {Pos, Length};
	unknown ->
	    undefined
    end;
%%---------------------------------------
%% Token found, the length was specified
%%---------------------------------------
get_positions([{Token, Length} | _], Token, _String, Pos) ->
    {Pos, Length};
%%---------------------------------------
%% Continue
%%---------------------------------------
get_positions([X|T], Token, String, Pos) ->
    case gp_length(token_length(X), T, string:substr(String, Pos)) of
	Length when is_integer(Length) ->
	    get_positions(T, Token, String, Pos + Length);
	unknown ->
	    undefined
    end.
    
%%    get_positions(T, Token, String, Pos + token_length(X)).
    



%%---------------------------------------
%% Token had a specific length
%%---------------------------------------
gp_length(Length, _Tokens, _String) when is_integer(Length) -> 
    Length;
%%---------------------------------------
%% Undefined length and no separtor char
%% found between this and next token.
%% It is not possible to parse the lenth.
%%---------------------------------------
gp_length(undefined, [[S | _] | _], _String) when S == $@ -> 
    unknown;
%%---------------------------------------
%% Parse the string to find the possition
%% where next token begins.
%%---------------------------------------
gp_length(undefined, [Separator | _], String) -> 
    case string:str(String, Separator) of
	0 -> undefined;
	L -> L - 1
    end.



token_length(?Cr) -> 10;
token_length(?Ct) -> 18;
token_length(?Ch) -> 2;
token_length(?Cn) -> 2;
token_length(?Cs) -> 2;
token_length(?Ca) -> 2;
token_length(?Cm) -> 2;
token_length(?CM) -> 3;
token_length(?Cd) -> 2;
token_length(?CD) -> 3;
token_length(?Cy) -> 2;
token_length(?CY) -> 4;
token_length(?Cc) -> 29;
token_length(?Cx) -> 1;
token_length(?Cb) -> undefined;
token_length(?Ci) -> undefined;
token_length(?Sl) -> undefined;
token_length(?Sv) -> 2;
token_length(?Ni) -> 18;
token_length(?Nt) -> 18;
token_length(?Nh) -> 2;
token_length(?Nn) -> 2;
token_length(?Ns) -> 2;
token_length(?Na) -> 2;
token_length(?Nm) -> 2;
token_length(?NM) -> 3;
token_length(?Nd) -> 3;
token_length(?ND) -> 2;
token_length(?Ny) -> 2;
token_length(?NY) -> 4;
token_length(?Nc) -> 29;
token_length(?Ne) -> undefined;
token_length(?No) -> undefined;
token_length(?Ng) -> undefined;
%% Characters between tokens
token_length(S) when is_list(S) -> length(S).

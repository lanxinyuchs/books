%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logComteCbWrite.erl %
%%% Author:	etxasta
%%% Description: This is a callback module for COMTE to send
%%%              events from COM to the LOG block.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(logComteCbWrite).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R12A/5').
-date('2017-11-20').
-author('uabesvi').
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
%%% -----      ---------  --------    ----------------------
%%% R2A/1      2013-09-05 etxasta     Created
%%% R2A/3      2013-12-12 uabesvi     changed LogM to RcsLogM
%%% R2A/4      2014-07-01 uabesvi     added alarm log updated
%%% R3A/4      2014-11-10 etxberb     Added call to coiAlarm:updateTable/1.
%%% R3A/5      2014-11-19 erarafo     Workaround for HT24951.
%%% R3A/6      2014-11-25 erarafo     Minor adjustment.
%%% R3A/7      2015-02-17 etxberb     Removed call to coiAlarm:updateTable/1.
%%% R3A/8      2015-04-16 etxjotj     Fixed compiler warning
%%% R4A/1      2015-08-20 etxpejn     Added rpc:call for AuditTrailLog
%%% R4A/2      2015-08-28 etxjotj     Bugfix in timestamp calculation
%%% R4A/4      2015-09-23 erarafo     Added a note on how registration takes place
%%% R4A/6      2015-10-12 erarafo     HU25136, splicing of strings added
%%% R4A/7      2015-10-14 erarafo     Stack trace logged in case of failure
%%% R5A/1      2015-11-19 etxarnu     Adapting to COM 6.0 to handle milliseconds 
%%%                                   in time_to_unix_epoch
%%% R5A/2      2016-02-26 uabesvi     Added additional text to alarm log
%%% R12A/2     2017-11-16 uabesvi     Restricted AT to 150 bytes
%%% R12A/5     2017-11-20 uabesvi     Removed AT to 150 bytes
%%%                                   Fixed that AI is filled in properly
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1,
	 logWrite/4]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsLogM.hrl").

-include_lib("xmerl/include/xmerl.hrl").
-include("safs_log.hrl").


%% Defines
-define(AUDIT, 13).  %% Facility number for audit trail
-define(ALARM, 100). %% Facility number for alarm log

-define(ALARM_TAG_LEN,   5). %% length of AI and AT tag
-define(ALARM_DATE_LEN, 35). %% length of AI and AT tag

%% -define(MANDATORY_LEN(__Severity)) ->
%%     ?ALARM_DATE_LEN ++ length(__Severity) ++ 2*?ALARM_TAG_LEN ++ 


%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%% The callback is invoked by ComtE during startup.
%% No configuration is performed.
start(_Args) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc The callback is invoked by COMTE for writing log
%%% events originating from COM. The registration of this
%%% module with COMTE is defined in make_release.escript.
%%% @end
%%% ----------------------------------------------------------
logWrite(EventId, Severity, ?AUDIT, Data) ->
    TS  = os:timestamp(),
    E   = integer_to_list(EventId),
    D   = binary_to_list(Data),
    Msg = "EventId(" ++ E ++ ") " ++ D,
    S   = format_severity(Severity),
    logI:write_log("AuditTrailLog", "-", "-", ?AUDIT, S, TS, Msg),
    ok;
logWrite(EventId, Severity, ?ALARM, Data) ->
    try
	alarm_log(EventId, Severity, binary_to_list(Data)),
	ok
    catch
	ExType:ExData ->
	    Stack = erlang:get_stacktrace(),
	    sysInitI:error_report(
	      [{logWrite, {?MODULE, ?LINE, ExType, ExData}},
	       {eventId, EventId},
	       {severity, Severity},
	       {data, Data},
	       {stack, Stack}]),
	    ok
    end;
logWrite(_EventId, _Severity, _Facility, _Data) ->
    %% Handle other facility events
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%===========================================================================
%% alarm_log(_EventId, _Severity, Data) -> ok.
%%
%% Write the COM alarm into safs alarm log.
%% Data is in internal format, defined by COM.
%%
%%===========================================================================
alarm_log(_EventId, _Severity, Data) ->
    
    Parsed = al_parse(xmerl_scan:string(Data)),
    
    % workaround until HT24951 is fixed
    % TODO, remove when HT24951 is delivered in COM
    % 2015-10-10: COM in the simulator still awaits the fix
    Format =
	case al_format(Parsed) of
	    {ok,
	     {Date,
	      [S1, S2, S3,
	       S4, S5, S6, S7, S8,
	       S9, S10, "ENVIRONMENTALARM"|Tail]}} ->
		{ok,
		 {Date,
		  [S1, S2, S3,
		   S4, S5, S6, S7, S8,
		   S9, S10, "ENVIRONMENTALALARM"|Tail]}};
	    Nomatch ->
		Nomatch
	end,
    % end of workaround
    write_alarm_log(Format).


%%------------------------------------------------------------------
%% al_parse(ParsedData) -> SimpleFormat
%%
%% parse the Data into simple xml format
%%------------------------------------------------------------------
al_parse({#xmlElement{content = C}, []}) ->
    [Simple || {_, _} = Simple <- xmerl_to_simple(C)];
al_parse(_) ->
    [].


%%------------------------------------------------------------------
%% al_format(Parsed) -> {ok, {TimeStamp, Alarm}}
%%
%% Format the alarm data into separate attributes
%%------------------------------------------------------------------
al_format(Parsed) ->
    [TS] = proplists:get_value('LogTimestamp', Parsed),
    [Al] = proplists:get_value('Alarm', Parsed),
    Alarm = al_tokenize(Al, [], []),
    {ok, {TS, Alarm}}.


al_tokenize([], Token, Acc) ->
    lists:reverse([lists:reverse(Token) | Acc]);
al_tokenize([$; | T], Token, Acc) ->
    al_tokenize(T, [], [lists:reverse(Token) | Acc]);
al_tokenize([H | T], Token, Acc) ->
    al_tokenize(T, [H | Token], Acc).



xmerl_to_simple([#xmlElement{name=Name, content=Content}|Tail]) ->
    [{Name, xmerl_to_simple(Content)}|xmerl_to_simple(Tail)];
xmerl_to_simple([#xmlText{value=Val}|[#xmlComment{}|_]=Tail]) ->
    [lists:flatten([Val|xmerl_to_simple(Tail)])];
xmerl_to_simple([#xmlText{value=Val} | Tail]) ->
    case xmerl_to_simple(Tail) of
	[String] ->
	    [lists:flatten(Val)++String];
	Other ->
	    [lists:flatten(Val)|Other]
    end;
xmerl_to_simple([_|Tail]) ->
    xmerl_to_simple(Tail);
xmerl_to_simple([]) ->
    [].


%%------------------------------------------------------------------
%% write_alarm_log({ok, {TimeStamp, Alarm}}) -> ok
%%
%% Alarm: as specified in  1/1553-CAA 901 2587 COM 4.1 shipment 1
%%
%%------------------------------------------------------------------
write_alarm_log({ok,
		 {TimeProduced,
		  [_SeqNo,
		   _EventTime,
		   Source,
		   _MajorType,
		   _MinorType,
		   SpecificProblem,
		   _PropableCause,
		   Severity,
		   AdditionalText,
		   _SeqNumber,
		   _EventType,
		   OrigTimeEventTime,
		   _OrigSeverity,
		   _OrigAdditionalText,
		   OrigSeqNumber,
		   "0"]}}) ->
    wal(TimeProduced,
	Source,
	SpecificProblem,
	AdditionalText,
	"",
	Severity,
	OrigTimeEventTime,
	OrigSeqNumber);
write_alarm_log({ok,
		 {TimeProduced,
		  [_SeqNo,
		   _EventTime,
		   Source,
		   _MajorType,
		   _MinorType,
		   SpecificProblem,
		   _PropableCause,
		   Severity,
		   AdditionalText,
		   _SeqNumber,
		   _EventType,
		   OrigTimeEventTime,
		   _OrigSeverity,
		   _OrigAdditionalText,
		   OrigSeqNumber,
		   AdditionalInfoSize |
		   AdditionalInfoList]}}) ->

    AiList = lists:sublist(AdditionalInfoList, 
			   list_to_integer(AdditionalInfoSize) * 2),
    wal(TimeProduced,
	Source,
	SpecificProblem,
	AdditionalText,
	wal_format_ai(AiList, []),
	Severity,
	OrigTimeEventTime,
	OrigSeqNumber);
write_alarm_log(Unknown) ->
    sysInitI:info_msg("Unknown alarm. ~p:write_alarm_log(~p)~n",
			  [?MODULE, Unknown]),
    ok.


wal_format_ai([], Acc) ->
    lists:join(";", lists:reverse(Acc));
wal_format_ai([K, V | T], Acc) ->
    wal_format_ai(T, [K ++ "=" ++ V | Acc]).
    

wal(TimeProduced, 
    Object, 
    Buffer, 
    AddText, 
    AddInfo, 
    Severity, 
    TimeDetected, 
    AlarmId) ->

    CB = #safsLogCallbacks{saLogFilterSetCallback  = false,
			   saLogStreamOpenCallback = false,
			   saLogWriteLogCallback   = false
			  },

    [SafsVsn | _]   = ?SUPPORTED_LOG_VERSIONS,
    {ok, Handle, _} = safs_log:initialize(CB, SafsVsn),
    {ok, StreamH}   = safs_log:stream_open_2(Handle,
					     ?LOG_ALARM,
					     undefined,
					     undefined,
					     5000),

%% Example of an alarm:
%% '         1 2017-11-07T12:21:29.085Z ManagedElement=1,SystemFunctions=1,Lm=1 "CRITICAL: License Key File Fault :AT:Key file fault in Managed Element :AI:eventId"'
%% 
%% The log entry mau be of length RecSize
%% The AlarmText must be at least of length AtSize
%%
%% The mandatory characters occupies MandatoryBufferLen characters

    {RecSize, AtSize} = logServer:get_alarm_log_data(),

    MandatoryBufferLen = ?ALARM_DATE_LEN +
	length(Object) + 1 +
	length(Severity) + 2 +
	length(Buffer) +
	5 +                      %% length of AT tag
	5 +                      %% length of AI tag
	2 +                      %% double quotes
	2,                       %% cr nl
	
    {AddTextF, AddInfoF} = format_alarm_text(AddText, 
					     AddInfo,
					     RecSize - MandatoryBufferLen, 
					     AtSize),

    LogBuffer = Severity ++ ": " ++ Buffer ++ 
	" :AT:" ++ AddTextF ++  
	" :AI:" ++ AddInfoF,

    Hdr = #safsLogNtfLogHeader{notificationId      = AlarmId,
			       eventType           = ?NTF_ALARM_NOTIFS_START,
			       notifyingObject     = Object,
			       notificationClassId = SafsVsn,
			       eventTime = get_event_time(Severity,
							  TimeDetected,
							  TimeProduced)
			      },

    Header    = #safsLogHeader{ntfHdr = Hdr},

    LogRecord = #safsLogRecord{logTimeStamp = time_to_unix_epoch(TimeProduced),
			       logHdrType   = ?LOG_HEADER_NTF,
			       logHeader    = Header,
			       logBuffer    = LogBuffer
			      },
    safs_log:write_log_async(StreamH, 1, undefined, LogRecord),

    safs_log:finalize(Handle).



get_event_time("CLEARED", _TimeDetected, TimeProduced) ->
    time_to_unix_epoch(TimeProduced);
get_event_time(_, TimeDetected, _TimeProduced) ->
    time_to_unix_epoch(TimeDetected).

%%------------------------------------------------------------------
%% time_to_unix_epoch(Time) -> integer()
%%
%% Transfer the alarm time to unix seconds since 1st Jan 1970
%%------------------------------------------------------------------
time_to_unix_epoch(T) ->
    %% T is in format "2014-06-27T15:02:41+02:00"
    %% or in new format "2014-06-27T15:02:41.012+02:00"
    
   {Local, [Y, Mo, D, H, Mi, S, MS, OffH, OffMi] } =
	case  [list_to_integer(L) || L <- string:tokens(T, "-+T:.")] of
	    [Y1, Mo1, D1, H1, Mi1, S1, MS1, OffH1, OffMi1] -> 
		Local1 = string:sub_string(T, 24, 24),
		{Local1,[Y1, Mo1, D1, H1, Mi1, S1,  MS1, OffH1, OffMi1]};
	    [Y1, Mo1, D1, H1, Mi1, S1, OffH1, OffMi1] -> 
		Local1 = string:sub_string(T, 20, 20),
		{Local1,[Y1, Mo1, D1, H1, Mi1, S1, 0, OffH1, OffMi1]}
	end,
    
	    

    LocalSecs = calendar:datetime_to_gregorian_seconds({{Y,Mo,D},{H,Mi,S}}),
    Offset = calendar:time_to_seconds({OffH, OffMi, 0}),
    UniversalSecs = case Local of
			"+" -> LocalSecs-Offset;
			"-" -> LocalSecs+Offset
		    end,
    Epoch = UniversalSecs -
	calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    %% SaTimeT is in nano seconds
    Epoch * 1000000000 + MS * 1000000.

%%     ST = calendar:time_to_seconds({H, Mi, S}),
%%     SL = calendar:time_to_seconds({OffH, OffMi, 0}),

%%     SUTC = case Local of
%% 	       "+" -> ST - SL;
%% 	       "-" -> ST + SL
%% 	   end,

%%     Time = calendar:seconds_to_time(SUTC),
%%     %% SaTimeT is in nano seconds
%%     unix_seconds_since_epoch({{Y, Mo, D}, Time}) * 1000000000.



%% unix_seconds_since_epoch(DateTime) ->
%%     calendar:datetime_to_gregorian_seconds(DateTime) -
%%     calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).



format_severity(?LogSeverity_EMERGENCY) -> emergency;
format_severity(?LogSeverity_ALERT)     -> alert;
format_severity(?LogSeverity_CRITICAL)  -> critical;
format_severity(?LogSeverity_ERROR)     -> error;
format_severity(?LogSeverity_WARNING)   -> warning;
format_severity(?LogSeverity_NOTICE)    -> notice;
format_severity(?LogSeverity_INFO)      -> info.


%%===========================================================================
%% Fit the alarm text into the remainging size
%% If AI does not fit into the log entry it will truncated in SAFS
%%===========================================================================
%% %% Both AT and AI fits
%% format_alarm_text(AddText, AddInfo, RemainingSize, _AtSizeMin) 
%%   when length(AddText) + length(AddInfo) =< RemainingSize ->
%%     io:format("Both AT and AI fits"),
%%     {AddText, AddInfo};
%% %% AT is shorter than the min AT sixe
%% format_alarm_text(AddText, AddInfo, _RemainingSize, AtSizeMin)
%%   when length(AddText) =< AtSizeMin ->
%%     io:format("AT is shorter than the min AT size~n"),
%%     {AddText, AddInfo};
%% %% AT is longer than the min AT sixe
%% format_alarm_text(AddText, AddInfo, _RemainingSize, AtSizeMin) ->
%%     io:format("AT is longer than the min AT size~n"),
%%     {lists:sublist(AddText, AtSizeMin), AddInfo}.


%% until someone desides how it should be
format_alarm_text(AddText, AddInfo, _RemainingSize, _AtSizeMin) ->
    {AddText, AddInfo}.   
    

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

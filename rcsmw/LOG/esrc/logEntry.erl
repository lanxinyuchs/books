%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logEntry.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(logEntry).
-vsn('/main/R1A/R2A/R3A/R4A/R7A/R8A/R9A/R10A/R11A/2').
-date('2017-10-17').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ----------------------
%%% R1A/1      2012-02-16 etxjotj     Created
%%% R2A/1      2013-06-04 etxasta     Added logStreamServer
%%% R2A/5      2013-12-12 uabesvi     Changed LogM to RcsLogM
%%% R4A/1      2015-09-01 etxjotj     SysInitI printouts
%%% R4A/2      2015-09-17 etxjotj     Log counter not in mnesia
%%% -----      ---------  --------    ------------------------
%%% R8A/1      2016-11-24 uabesvi     Encrypted logs
%%% R9A/1-5    2017-03-27 uabesvi     Added code for compress
%%% R11A/2     2017-10-17 uabesvi     Do awrite_log from a spawned process to 
%%%                                   really make it asynchronous. HW28749.
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([write_log/5,  write_log/7]).
-export([awrite_log/5, awrite_log/7]).
-export([asynch_write/7]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsLogM.hrl").
-include("log.hrl").

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%============================================================================
%% write_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) 
%%
%% synchrenous write
%%============================================================================
write_log(Name, User, Severity, TimeStamp, Msg) ->
    write_log(Name, "-", User, 16, Severity, TimeStamp, Msg).

write_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) 
  when Name == "AlarmLog" ->
    do_wl({ok, Name},
	  false, 
	  Name, 
	  SrcIp, 
	  User, 
	  Facility, 
	  Severity, 
	  TimeStamp, 
	  Msg);
write_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) ->
    do_write_log(logDb:log_data_get_dirty({"1","1","1",Name}),
		 disk_log:info(Name),
		 logDb:log_get_dirty({"1","1","1",Name}),
		 logLib:is_internal(Name),
		 {Name, 
		  SrcIp, 
		  User, 
		  Facility, 
		  Severity, 
		  TimeStamp, 
		  Msg}).



%%--------------------------------------------------------------------
%% No logData exist
%%--------------------------------------------------------------------
do_write_log([], _, _, _, Data) ->
    sysInitI:info_msg("~p:~p write_log call before log exists ~p~n", 
		      [?MODULE, ?LINE, Data]),
    {error, no_such_log};
%%--------------------------------------------------------------------
%% Log is not opened in disk_log 
%% 
%% open it temporarily. This should only happen if a log is created
%% before logServer has been created.
%%--------------------------------------------------------------------
do_write_log(_,
	     {error, no_such_log},
	     LogData,
	     Internal,
	     {Name, SrcIp, User, Facility, Severity, TimeStamp, Msg}) 
  when length(LogData) == 1 orelse Internal ->
    Data = {Name, SrcIp, User, Facility, Severity, TimeStamp, Msg},
    Rec  = ets:lookup(logData, {"1","1","1",Name}),
    Open = wl_open_log(Rec, Name, Data),
    do_wl(Open,
	  do_wl_filter(LogData, Severity), 
	  Name, 
	  SrcIp, 
	  User, 
	  Facility, 
	  Severity, 
	  TimeStamp, 
	  Msg),
    wl_close(Open, Name);
%%--------------------------------------------------------------------
%% Log is open.
%%--------------------------------------------------------------------
do_write_log(_,
	     _,
	     LogData,
	     Internal,
	     {Name, SrcIp, User, Facility, Severity, TimeStamp, Msg}) 
  when length(LogData) == 1 orelse Internal ->
    do_wl({ok, Name},
	  do_wl_filter(LogData, Severity), 
	  Name, 
	  SrcIp, 
	  User, 
	  Facility, 
	  Severity, 
	  TimeStamp, 
	  Msg);
%%--------------------------------------------------------------------
%% No logData exist
%%--------------------------------------------------------------------
do_write_log(_, _, _, _, Data) ->
    sysInitI:info_msg("~p:~p write_log call before log exists ~p~n", 
		      [?MODULE, ?LINE, Data]),
    {error, no_such_log}.




do_wl({ok, _}, true, _Name, _SrcIp, _User, _Facility, _Severity, _TimeS, _Msg) ->
    ok;
do_wl({ok, _}, false, Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) ->
    %% Send log event to stream
    logStreamServer:event(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg),
    
    Entry = format_log_entry(Name, User, Severity, TimeStamp, Msg),
    wl_rc(blog(logLib:is_encrypted(Name),
	       certI:is_encryption_enabled(),
	       Name,
	       Entry));
do_wl(Error, _, _Name, _SrcIp, _User, _Facility, _Severity, _TimeS, _Msg) ->
    Error.


do_wl_filter([Obj], Severity) -> 
    check_filter(Obj, Severity);
do_wl_filter(_, _) ->
    false.


wl_rc(ok) -> 
    ok;
wl_rc({error, no_such_log}) -> 
    invalidParam;
wl_rc({error, {full, _}}) -> 
    comsaI:send_alarm('LogHasReachedFullCapacity', 
		      warning,
		      ?LOG_LDN, 
		      "file is full"),
    noResources;
wl_rc({error, {file_error, _, _}}) -> 
    noResources;
wl_rc({error, _}) -> 
    tryAgain.


wl_open_log([#logData{maxSizeKb        = MaxKb,
		      rotatingSegments = MaxNoFiles}],
	    Name,
	    _) ->
    logLib:open_disk_log(Name, MaxKb, MaxNoFiles);
wl_open_log(_, _, Data) ->
    sysInitI:info_msg("~p:write_log call before log exists ~p~n", 
		      [?MODULE, Data]),
    {error, no_such_log}.
    
wl_close({ok, _}, Name) ->
    disk_log:close(Name);
wl_close(Open, _) ->
    Open.
    





%%============================================================================
%% awrite_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) 
%%
%% asynchrenous write
%%============================================================================
awrite_log(Name, User, Severity, TimeStamp, Msg) ->
    awrite_log(Name, "-", User, 16, Severity, TimeStamp, Msg).

awrite_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) 
  when Name == "AlarmLog" ->
    awrite_log(false, 
	       Name, 
	       SrcIp, 
	       User, 
	       Facility, 
	       Severity, 
	       TimeStamp, 
	       Msg);
awrite_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) ->
    Internal = logLib:is_internal(Name),
    case mnesia:dirty_read({log, {"1","1","1",Name}}) of
	[DataObj] ->
	    DoFilter = check_filter(DataObj, Severity),
	    awrite_log(DoFilter, 
		       Name, 
		       SrcIp, 
		       User, 
		       Facility, 
		       Severity, 
		       TimeStamp, 
		       Msg);
	_ when Internal ->
	    awrite_log(false, 
		       Name, 
		       SrcIp, 
		       User, 
		       Facility, 
		       Severity, 
		       TimeStamp, 
		       Msg);
	_ ->
	    Data = {Name, SrcIp, User, Facility, Severity, TimeStamp, Msg},
	    sysInitI:info_msg("~p:awrite_log call before log exists ~p~n", 
				  [?MODULE, Data])
    end.


%% This severity is filtered.
awrite_log(true, _Name, _SrcIp, _User, _Facility, _Severity, _TimeS, _Msg) ->
    ok;
%% Write the entry
awrite_log(false, Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) ->
    spawn(logEntry, 
	  asynch_write, 
	  [Name, SrcIp, User, Facility, Severity, TimeStamp, Msg]).


asynch_write(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) ->
    %% Send log event to stream
    logStreamServer:event(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg),
    
    Entry = format_log_entry(Name, User, Severity, TimeStamp, Msg),
    write(logLib:is_encrypted(Name),
	  certI:is_encryption_enabled(),
	  Name,
	  Entry),
    ok.


check_filter(#log{severityFilter = undefined}, _) ->
    false;
check_filter(#log{severityFilter = Filters}, Severity) ->
    lists:member(get_severity(Severity), Filters).
    

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

format_log_entry(Name, User, Severity, TimeStamp, Msg) 
  when is_integer(TimeStamp) ->
    %% Generate id
    NewVal = logServer:update_counter(Name),
    Id = NewVal rem 10000000000,
    IdFmt = io_lib:format("~10.10..w ",[Id]),
    
    %% Format timestamp 
    %% (no milliseconds possible because TimeStamp does not contain it)
    Now = {TimeStamp div 1000000, TimeStamp rem 1000000, 0},
    {{Y,M,D}, {H, Mi, S}} = calendar:now_to_datetime(Now),
    fle({{Y,M,D}, {H, Mi, S}}, {false, 0}, IdFmt, User, Severity, Msg);

format_log_entry(Name, User, Severity, {_, _, MS} = TimeStamp, Msg) ->
    %% Generate id
    NewVal = logServer:update_counter(Name),
    Id = NewVal rem 10000000000,
    IdFmt = io_lib:format("~10.10..w ",[Id]),

    UseMS = logLib:use_milli_sec(Name),
    
    %% Format timestamp
    {{Y,M,D}, {H, Mi, S}} = calendar:now_to_datetime(TimeStamp),
    fle({{Y,M,D}, {H, Mi, S}}, {UseMS, MS div 1000}, IdFmt, User, Severity, Msg).


fle({{Y,M,D}, {H, Mi, S}}, {true, MS}, IdFmt, User, Severity, Msg) ->
    TSFmt = io_lib:format("~w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0wZ",
			  [Y,M,D,H,Mi,S,MS]),
    %% Format severity level
    lists:flatten(io_lib:format(IdFmt ++ TSFmt ++ " ~s \"~s ~s\"~n", 
				[User, get_severity_str(Severity), Msg]));
fle({{Y,M,D}, {H, Mi, S}}, _, IdFmt, User, Severity, Msg) ->
    TimeStampFmt = io_lib:format("~w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ",
				 [Y,M,D,H,Mi,S]),
    %% Format severity level
    lists:flatten(io_lib:format(IdFmt ++ TimeStampFmt ++ " ~s \"~s ~s\"~n", 
				[User, get_severity_str(Severity), Msg])).

   

get_severity(Severity) ->
    case Severity of
	Severity when is_list(Severity) -> list_to_integer(Severity);
	emergency -> ?LogSeverity_EMERGENCY;
	alert     -> ?LogSeverity_ALERT;
	critical  -> ?LogSeverity_CRITICAL;
	error     -> ?LogSeverity_ERROR;
	warning   -> ?LogSeverity_WARNING;
	notice    -> ?LogSeverity_NOTICE;
	info      -> ?LogSeverity_INFO
    end.
    
get_severity_str(Severity) ->
    case Severity of
	Severity when is_list(Severity) -> Severity;
	emergency -> "EMERGENCY:";
	alert     -> "ALERT:";
	critical  -> "CRITICAL:";
	error     -> "ERROR:";
	warning   -> "WARNING:";
	notice    -> "NOTICE:";
	info      -> "INFO:"
    end.
    

%%=============================================================================
%% blog(IsEncryptedLog, IsEncryptionOn, LogName, LogEntry) 
%% 
%% IsEncryptedLog: if the log is marked to be encrypted
%% IsEncryptionOn: if the encryption is turned on
%%=============================================================================
blog(true, true, Name, Entry) ->
    blog_rc(disk_log:log(Name, encrypt(Entry)), Name, Entry);
blog(_, _, Name, Entry) ->
    blog_rc(disk_log:blog(Name, list_to_binary(Entry)), Name, Entry).


blog_rc(ok, _, _) ->
    ok;
blog_rc(Error, Name, Entry) ->
    blog_cloud(sysEnv:rcs_mode_2(), Error, Name, Entry),
    Error.

blog_cloud(_, {error, no_such_log}, Name, Entry) ->
    sysInitI:warning_msg("~p: no such log ~p~n~p~n", [?MODULE, Name, Entry]),
    ok;
blog_cloud(vrcs, Error, Name, _) ->
    ErrorStr = lists:flatten(io_lib:format("~p~n", [Error])),
    Msg      = atom_to_list(?MODULE) ++ 
	" disk_log error: " ++ ErrorStr ++
	" log name " ++ Name,
    appmI:restart_node(cold, Msg);
blog_cloud(_, _, _, _) ->
    ok.




%%=============================================================================
%% write(IsEncryptedLog, IsEncryptionOn, LogName, LogEntry) 
%% 
%% IsEncryptedLog: if the log is marked to be encrypted
%% IsEncryptionOn: if the encryption is turned on
%%=============================================================================
write(true, true, Name, Entry) ->
    disk_log:log(Name, encrypt(Entry));
write(_, _, Name, Entry) ->
    disk_log:blog(Name, list_to_binary(Entry)).



encrypt(Data) ->
    encrypt(Data, certI:is_encryption_enabled()).


encrypt(Data, true) ->
    case certI:encrypt_data(Data) of
	{ok, Encrypted} ->
	    Encrypted;
	Error ->
	    sysInitI:error_msg("~p:encrypt error = ~p~n", [?MODULE, Error]),
	    <<>>
    end;
encrypt(Data, false) ->
    Data.



%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


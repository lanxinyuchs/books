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
%% File: safs_log_stream.erl
%%
%% Description:
%%
%% This module implements a process handling a stream of any type.
%%--------------------------------------------------------------------
-module(safs_log_stream).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("safs_log.hrl").
-include("safs_log_db.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([start/4]).
-export([awrite/4]).
-export([close/2]).
-export([call_filter_cb/3]).
-export([change_filter/2]).
-export([filter_modify/2]).
-export([filter_apply/1]).
-export([filter_abort/1]).
-export([is_open/3]).
-export([delete_rt_obj/1]).
-export([stream_reopen_file/2]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([init/2]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(CREATE_TO, 30000).
-define(CLOSE_TO,  30000).
-define(DEFAULT_FILTER, #safsLogSeverityFlags{_ = true}).
-define(MAX_MSG_QUEUE_LENGTH, 1000).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(stream, {disk_log_id,
		 parent,
		 seq_no = 1,
		 file_name,
		 file_path,
		 stream_name,
		 attributes,
		 n_severity = ?DEFAULT_FILTER,
		 severity = ?DEFAULT_FILTER,
		 file_data,
		 oi_handle,
		 version}).


%%======================================================================
%% External Interface Functions
%%======================================================================

%%========================================================================
%% start(StreamName, Handle, CreateAttributes) -> {ok, Pid} | {error, Error}
%%
%% Starts the stream process if FileFormat is ok.
%%========================================================================
%%------------------------------------------------------
%% system, notify or alarm log
%%------------------------------------------------------
start(StreamName, undefined, [], _)
  when StreamName == ?LOG_SYSTEM orelse
       StreamName == ?LOG_NOTIFY orelse
       StreamName == ?LOG_ALARM ->
    [Version | _] = ?SUPPORTED_LOG_VERSIONS,
    start_sys_logs(StreamName, Version);
%%------------------------------------------------------
%% application log
%%------------------------------------------------------
start(StreamName,
      #safsLogFileCreateAttributes_2{logFileName = FileName,
				     logFileFmt  = FileFormat
				    } = CreateAttributes,
      [#safs_log_user{version = Version}],
      OiHandle) ->
    start_spawn(check_format(FileFormat, FileName),
		StreamName,
		CreateAttributes,
		?DEFAULT_FILTER,
		Version,
		OiHandle).



%%------------------------------------------------------
%% fixes for system, notify or alarm logs
%%------------------------------------------------------
start_sys_logs(StreamName, Version) ->
    {CA, Filter} = safs_log_file:get_cfg_params(StreamName),
    {ok, [SC]} = safs_log_db:stream_config_get(StreamName),
    safs_log_db:stream_config_update(SC, {create_attributes, CA}),
    FilePath = CA#safsLogFileCreateAttributes_2.logFilePathName,
    [_, PathPrefix | _] = string:tokens(StreamName, "=,"),
    Path = filename:join(PathPrefix, FilePath),
    CA2  = CA#safsLogFileCreateAttributes_2{logFilePathName = Path},
    SevFilter = choose(Filter =/= undefined, Filter, ?DEFAULT_FILTER),
    start_spawn(StreamName, CA2, SevFilter, Version, undefined).

%%------------------------------------------------------
%% Spawn the stream process
%%------------------------------------------------------
start_spawn({ok, FileFormat}, StreamName, CA, Filter, Version, OiHandle) ->
    CA2  = CA#safsLogFileCreateAttributes_2{logFileFmt = FileFormat},
    start_spawn(StreamName, CA2, Filter, Version, OiHandle);
start_spawn({error, _} = Error, StreamName, _CA, _Filter, _Vsn, _OH) ->
    log(start, {Error, StreamName}),
    {error, ?SA_ERR_INVALID_PARAM}.


start_spawn(StreamName, CA, Filter, Version, OiHandle) ->
    Self = self(),
    spawn(fun() ->
		  init(Self, {StreamName, CA, Filter, Version, OiHandle})
	  end),

    receive
	{stream_init, res, Res} ->
	    Res
    after ?CREATE_TO ->
	    delete_obj_if_app_log(OiHandle, StreamName),
	    error_logger:error_msg("Failed to open log stream:\n\t~p\n"
				   "Stream name: \n\t~p\n",
				   [?SA_ERR_TIMEOUT, StreamName]),
	    {error, ?SA_ERR_TIMEOUT}
    end.


%%========================================================================
%% asynchroneous write
%%========================================================================
awrite(Pid,
       StreamName,
       #safsLogRecord{logHdrType = ?LOG_HEADER_NTF} = Record,
       Data)
  when StreamName == ?LOG_ALARM;
       StreamName == ?LOG_NOTIFY ->
    Pid ! {awrite_log, Record, Data},
    ok;
awrite(Pid,
       StreamName,
       #safsLogRecord{logHdrType = ?LOG_HEADER_GENERIC} = Record,
       Data)
  when StreamName =/= ?LOG_ALARM,
       StreamName =/= ?LOG_NOTIFY ->
    Pid ! {awrite_log, Record, Data},
    ok;
awrite(_, _, _, _) ->
    {error, ?SA_ERR_INVALID_PARAM}.


%%========================================================================
%% close
%%========================================================================
close(Pid, Close) ->
    Pid ! {close, self(), Close},
    receive
	closed -> {ok, deleted}
    after ?CLOSE_TO ->
	    error_logger:error_msg("Failed to close log stream:\n\t~p\n"
				   "Stream name: \n\t~p\n",
				   [?SA_ERR_TIMEOUT, Close]),
	    {error, ?SA_ERR_TIMEOUT}
    end.


%%========================================================================
%% delete_rt_obj
%%========================================================================
delete_rt_obj(Pid) ->
    Pid ! {delete_rt_obj, self()},
    receive
	deleted -> {ok, deleted}
    after 3000 ->
	    {timeout, deleted}
    end.


%%========================================================================
%% stream_reopen_file
%%========================================================================
stream_reopen_file(Pid, Reopen) ->
    Pid ! {stream_reopen_file, self(), Reopen},
    receive
	{reopened, Res} -> Res
    after 3000 ->
	    {timeout, reopened}
    end.


%%========================================================================
%% call_filter_cb
%%========================================================================
call_filter_cb(Pid,
	       [#safs_log_user{callbacks = CB,
			       cb_proxy  = ProxyPid}],
	       Handle) ->
    cf_cb(CB, Pid, ProxyPid, Handle);
call_filter_cb(_Pid, [], _) ->
    ok.


cf_cb(#safsLogCallbacks{saLogFilterSetCallback = Fcb}, _, _, _)
  when Fcb == false;
       Fcb == undefined ->
    ok;
cf_cb(#safsLogCallbacks{saLogFilterSetCallback = Fcb}, Pid, ProxyPid, Handle) ->
    Pid ! {call_filter_cb, Fcb, ProxyPid, Handle}.




%%========================================================================
%% change_filter
%%========================================================================
change_filter(Pid, NewFilters) ->
    Pid ! {change_filter, self(), NewFilters},
    receive
	{change_filter, res, Res} ->
	    Res
    after ?CREATE_TO ->
	    {error, ?SA_ERR_TRY_AGAIN}
    end.


%%========================================================================
%% filter_modify
%%========================================================================
filter_modify(Pid, NewFilters) ->
    Pid ! {filter_modify, self(), NewFilters},
    receive
	{filter_modify, res, Res} ->
	    Res
    after ?CREATE_TO ->
	    {error, ?SA_ERR_TRY_AGAIN}
    end.


%%========================================================================
%% filter_apply
%%========================================================================
filter_apply(Pid) ->
    Pid ! {filter_apply, self()},
    receive
	{filter_apply, res, Res} ->
	    Res
    after ?CREATE_TO ->
	    ok
    end.


%%========================================================================
%% filter_abort
%%========================================================================
filter_abort(Pid) ->
    Pid ! {filter_abort, self()},
    receive
	{filter_abort, res, Res} ->
	    Res
    after ?CREATE_TO ->
	    ok
    end.


%%========================================================================
%% is_open
%%========================================================================
is_open(Pid, FileName, FilePathName) ->
    %% The replies are collected in safs_log_srv:io_wait_res
    Pid ! {is_open, self(), FileName, FilePathName}.


%%======================================================================
%% Internal Interface Functions
%%======================================================================

%%========================================================================
%% init(UserPid, {StreamName, CreateAttributes}) -> {ok, self(), {error, Error}
%%
%% init the stream process
%% open a disk_log
%%========================================================================
init(Pid,
     {StreamName,
      #safsLogFileCreateAttributes_2{logFilePathName = FilePath,
				     logFileName     = FileName} = CA,
      SevFilter,
      Version,
      OiHandle
     }) ->
    FileNameDate = FileName ++ "_" ++ get_time(),
    State = #stream{parent      = Pid,
		    file_name   = FileNameDate,
		    file_path   = FilePath,
		    stream_name = StreamName,
		    attributes  = CA,
		    severity    = SevFilter,
		    oi_handle   = OiHandle,
		    version     = Version},

    init_rc(safs_log_file:open_log(FilePath,
				   FileNameDate,
				   StreamName,
				   CA,
				   Version),
	    State).


init_rc({ok, DiskLogId, FileData, SequenceNo},
	#stream{parent = Pid,
		stream_name = StreamName,
		attributes  = CA,
		severity    = Severity,
		oi_handle   = OiHandle} = State) ->
    create_obj_if_app_log(OiHandle, StreamName, CA, Severity),
    Pid ! {stream_init, res, {ok, self()}},
    loop(State#stream{disk_log_id = DiskLogId,
		      seq_no      = SequenceNo,
		      file_data   = FileData#file_data{disk_log_id =
						       DiskLogId}});
init_rc(Error, #stream{parent = Pid}) ->
    log(init_rc, Error),
    Pid ! {stream_init, res, Error}.


create_obj_if_app_log(undefined, _StreamName, _CA, _Severity) ->
    ok;

create_obj_if_app_log(OiHandle, StreamName, CA, Severity) ->
    ok = safs_log_oi_rt:stream_create(OiHandle, StreamName, CA, Severity).


delete_obj_if_app_log(undefined, _StreamName) ->
    ok;

delete_obj_if_app_log(OiHandle, StreamName) ->
    safs_log_oi_rt:stream_delete(OiHandle, StreamName).

%%========================================================================
%% loop(Loop)
%%========================================================================
loop(#stream{file_path   = FilePath,
	     file_name   = FileName,
	     file_data   = #file_data{name       = FileNameFD,
				      file_names = [FileNameDT | _],
				      cfg_file   = CfgFile} = FileData,
	     stream_name = StreamName,
	     disk_log_id = DiskLogId,
	     seq_no      = SeqNo,
	     attributes  = CA,
	     n_severity  = NFilters,
	     severity    = Filters,
	     oi_handle   = OiHandle,
	     version     = Version} = Loop) ->
    receive
	{awrite_log, Msg, UserData} ->
	    {message_queue_len, MessageQueueLen} =
		erlang:process_info(self(), message_queue_len),
	    if
		MessageQueueLen > ?MAX_MSG_QUEUE_LENGTH ->
		    error_logger:warning_msg("~p: log stream overloaded: ~p\n",
				       [?MODULE, StreamName]),
		    N = mq_flush(0),
		    error_logger:info_msg("~p: log stream: ~p flushed ~p messages\n",
				       [?MODULE, StreamName, N]),
		    loop(Loop);
		true ->
		    p(user, "Stream ~p   LOG ~p~n", [StreamName, Msg]),
		    StreamConf = safs_log_db:stream_config_get(StreamName),
		    Severity   = get_log_severity(StreamName, Msg),
		    Filter     = get_filter(Severity, Filters),
		    {Res, NewFileData} = handle_awrite(Filter,
						       StreamConf,
						       StreamName,
						       Msg,
						       FileData,
						       SeqNo),
		    send_callback(awrite_log, UserData, Res),
		    loop(Loop#stream{seq_no    = get_seq(SeqNo, Filter),
				     file_data = NewFileData})
	    end;
	{close, UserPid, Close} ->
	    safs_log_file:close_log(DiskLogId,
				    StreamName,
				    FilePath,
				    FileNameDT,
				    CfgFile,
				    Close),
	    delete_obj_if_app_log(OiHandle, StreamName),
	    UserPid ! closed,
	    exit(normal);
	{delete_rt_obj, UserPid} ->
	    delete_obj_if_app_log(OiHandle, StreamName),
	    UserPid ! {ok, deleted},
	    loop(Loop);
	{call_filter_cb, true, ProxyPid, Handle} ->
	    safs_log_com:callback_message_async(ProxyPid,
						filter_set,
						{Handle, Filters}),
	    loop(Loop);
	{call_filter_cb, Fun, ProxyPid, Handle} when is_function(Fun) ->
	    spawn(fun() ->
			  Fun({filter_set, {Handle, Filters}, ProxyPid})
		  end),
	    loop(Loop);
	{call_filter_cb, {FcbM, FcbF}, ProxyPid, Handle} ->
	    spawn(fun() ->
			  FcbM:FcbF(ProxyPid, filter_set, {Handle, Filters})
		  end),
	    loop(Loop);
	{change_filter, UserPid, NewFilters}
	  when is_record(NewFilters, safsLogSeverityFlags) ->
	    Res = choose(NewFilters == Filters, {error, ?SA_ERR_NO_OP}, ok),
	    UserPid ! {change_filter, res, Res},
	    loop(Loop#stream{severity = NewFilters});
	{filter_modify, UserPid, NewFilters}
	  when is_record(NewFilters, safsLogSeverityFlags) ->
	    Res = choose(NewFilters == NFilters, {error, ?SA_ERR_NO_OP}, ok),
	    UserPid ! {filter_modify, res, Res},
	    loop(Loop#stream{n_severity = NewFilters});
	{filter_apply, UserPid} ->
	    UserPid ! {filter_apply, res, ok},
	    NewFilters = Loop#stream.n_severity,
	    loop(Loop#stream{severity = NewFilters});
	{filter_abort, UserPid} ->
	    UserPid ! {filter_abort, res, ok},
	    OldFilters = Loop#stream.severity,
	    loop(Loop#stream{n_severity = OldFilters});
	{stream_reopen_file, UserPid, _Data} ->
	    %%FileNameDate = FileName ++ "_" ++ get_time(),
	    {Res, NewFileData, NewFileName} = reopen_file(DiskLogId,
							  StreamName,
							  FilePath,
							  FileNameDT,
							  CfgFile,
							  CA,
							  Version,
							  FileData,
							  FileName),
	    UserPid ! {reopened, Res},
	    loop(Loop#stream{file_name = NewFileName,
			     file_data = NewFileData});
	loop_data ->
	    io:format(user, "===== STREAM loop data ~p =====~n~p~n",
		      [DiskLogId, Loop]),
	    loop(Loop);
	{loop_data, FileNameFD} ->
	    io:format(user, "===== STREAM loop data ~p =====~n~p~n",
		      [DiskLogId, Loop]),
	    io:format(user, "----- Stream file data -----~n~p~n", [FileData]),
	    loop(Loop);
	{loop_data, _FileNameFD} ->
	    %% not this stream
	    loop(Loop);
	{get_seq_no, FileNameFD, UserPid} ->
	    UserPid ! {get_seq_no_res, SeqNo},
	    loop(Loop);
	{get_seq_no, _FileNameFD, _UserPid} ->
	    loop(Loop);
	{is_open, UserPid, ReqFileName, ReqFilePath} ->
	    Res = handle_is_open(ReqFileName, FileNameDT, ReqFilePath, FilePath),
	    UserPid ! {is_open, res, Res},
	    loop(Loop);
	Unknown ->
	    io:format(user, "~p: Stream ~p loop UNKNOWN MSG ~p ~p~n",
		      [?MODULE, self(), StreamName, Unknown]),
	    loop(Loop)
    end.



%%======================================================================
%% mq_flush() -> ok
%%
%% Empty message queue for the process
%%
%%======================================================================
mq_flush(N) ->
    receive
        _ -> mq_flush(N+1)
    after 0 ->
      {ok, N}
end.


%%======================================================================
%% check_format(Format, FileName) -> {ok, Format} | {error, Error}
%%
%%
%%
%%======================================================================
check_format(undefined, ?LOG_SYSTEM) -> {ok, ?LOG_SYSTEM_FORMAT};
check_format(undefined, ?LOG_NOTIFY) -> {ok, ?LOG_NOTIFY_FORMAT};
check_format(undefined, ?LOG_ALARM)  -> {ok, ?LOG_ALARM_FORMAT};
check_format(undefined, _)           -> {ok, ?LOG_APP_FORMAT};
check_format(Format, FileName) ->
    case safs_log_rec:verify_format_expr(FileName, Format) of
	ok    -> {ok, Format};
	Error -> log(check_format, Error), Error
    end.


%%======================================================================
%% get_log_severity(StreamName, LogRecord, Filters) ->
%%    {ok, Severity} | {error, Reason}
%%
%%
%%
%%======================================================================
get_log_severity(Log,
		 #safsLogRecord{
		   logHeader = #safsLogHeader{ntfHdr =
					      #safsLogNtfLogHeader{}}})
  when Log == ?LOG_NOTIFY;
       Log == ?LOG_ALARM ->
    {ok, unfilterable};
get_log_severity(Log, _)
  when Log == ?LOG_NOTIFY;
       Log == ?LOG_ALARM ->
    log(get_log_severity, {log, {Log, invalid_log_header_1}}),
    {error, ?SA_ERR_INVALID_PARAM};
get_log_severity(_,
		 #safsLogRecord{
		   logHeader = #safsLogHeader{genericHdr =
					      #safsLogGenericLogHeader{logSeverity =
								       Severity}}}) ->
    {ok, Severity};
get_log_severity(Log, _) ->
    log(get_log_severity, {log, {Log, invalid_log_header_2}}),
    {error, ?SA_ERR_INVALID_PARAM}.





get_filter({ok, unfilterable}, _) ->
    {ok, true};
get_filter({ok, ?LOG_EMERGENCY},
	    #safsLogSeverityFlags{saLogSevFlagEmergency = Val}) ->
    {ok, Val};
get_filter({ok, ?LOG_ALERT},
	    #safsLogSeverityFlags{saLogSevFlagAlert = Val}) ->
    {ok, Val};
get_filter({ok, ?LOG_CRITICAL},
	    #safsLogSeverityFlags{saLogSevFlagCritical = Val}) ->
    {ok, Val};
get_filter({ok, ?LOG_ERROR},
	    #safsLogSeverityFlags{saLogSevFlagError = Val}) ->
    {ok, Val};
get_filter({ok, ?LOG_WARNING},
	    #safsLogSeverityFlags{saLogSevFlagWarning = Val}) ->
    {ok, Val};
get_filter({ok, ?LOG_NOTICE},
	    #safsLogSeverityFlags{saLogSevFlagNotice = Val}) ->
    {ok, Val};
get_filter({ok, ?LOG_INFO},
	    #safsLogSeverityFlags{saLogSevFlagInfo = Val}) ->
    {ok, Val};
get_filter({error, _} = Error, _) ->
    Error.




%%======================================================================
%% handle_awrite(IsWritable, StreamConf, StreamName, Msg, FileData, SeqNo) ->
%%   {ok, FileData} | {{error, Reason}, FileData}
%%
%% Invoke safs_log_file if the log entry is to be written to the stream.
%%======================================================================
handle_awrite({ok, true}, StreamConf, StreamName, Msg, FileData, SeqNo) ->
    safs_log_file:awrite(StreamConf, StreamName, Msg, FileData, SeqNo);
handle_awrite({ok, false}, _, _, _, FileData, _) ->
    log(handle_awrite, filter_not_set),
    {ok, FileData};
handle_awrite(Error, _, _, _, FileData, _) ->
    log(handle_awrite, Error),
    {Error, FileData}.



%%========================================================================
%% send_callback(CB, UserData, Res) -> _
%%
%%  UserData = {Handle, CbFnc, Invocation, AckFlags}
%%
%% Check if the callback function should be invoked
%%========================================================================
send_callback(awrite_log, {_, undefined, _, _}, _Res) ->
    ok;
send_callback(awrite_log, {_, false, _, _}, _Res) ->
    ok;
send_callback(awrite_log, {CbProxy, Cb, Invocation, ?LOG_ACK_FLAGS}, Res) ->
    CbFnc = choose(Cb == true, {safs_log_com, callback_message_async}, Cb),
    %% CbRes = choose(Res == ok, ?SA_OK, Res),
    log(send_callback, Res),
    gen_server:cast(?SERVER, {awrite_res,
			      CbFnc,
			      CbProxy,
			      {Invocation, Res}});
send_callback(awrite_log, _, _Res) ->
    ok.


%%======================================================================
%% get_seq(SeqNo, Filter) -> Time
%%
%% Update seq no only if not filtered.
%%======================================================================
get_seq(SeqNo, {ok, true}) -> SeqNo + 1;
get_seq(SeqNo, _)          -> SeqNo.

%%======================================================================
%% get_time() -> Time
%%
%% Get current time in a string format
%%======================================================================
get_time() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(os:timestamp()),
    integer_to_list(Y) ++
	i2l(Mo) ++
	i2l(D) ++
	"__" ++
	i2l(H) ++
	i2l(Mi) ++
	i2l(S).


i2l(I) when I < 10 -> "0" ++ integer_to_list(I);
i2l(I)             -> integer_to_list(I).


handle_is_open(FileName, FileName, FilePath, FilePath) ->
    true;
handle_is_open(_, _, _, _) ->
    false.



reopen_file(DiskLogId,
	    StreamName,
	    FilePath,
	    FileNameDT,
	    CfgFile,
	    CA,
	    Version,
	    FileData,
	    FileName) ->
    rf(safs_log_file:reopen_file(DiskLogId,
				 StreamName,
				 FilePath,
				 FileNameDT,
				 CfgFile,
				 CA,
				 Version),
      FileData,
      FileName).

rf({ok, FileData, FileName}, _, _) ->
    {ok, FileData, FileName};
rf(Error, FileData, FileName) ->
    {Error, FileData, FileName}.

%%======================================================================
%% Misc Functions
%%======================================================================
choose(true,  T, _) -> T;
choose(false, _, F) -> F.


log(_Fnc, ok) ->
    ok;
log(Fnc, Error) ->
    p(user, "### ERROR. ~p:~p ~p~n", [?MODULE, Fnc, Error]).


p(_, _, _) ->
    ok.
%% p(A,B,C) ->
%%     ct:pal("### P ~p ~p~n", [B,C]).

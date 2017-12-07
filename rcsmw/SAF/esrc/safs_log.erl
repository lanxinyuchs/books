%%----------------------------------------------------------------------
%%
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
%% File: safs_log.erl
%%
%% Description:
%%    This file implements the erlang API for the LOG service.
%%
%%--------------------------------------------------------------------
-module(safs_log).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_log.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Life-cycle
%%----------------------------------------------------------------------
-export([
         initialize/2,
         finalize/1
        ]).

%%----------------------------------------------------------------------
%% Log Service Operations
%%----------------------------------------------------------------------
-export([
         stream_open_2/5,
	 stream_open_async_2/5,
	 write_log/3,
	 write_log_async/4,
	 stream_close/1,
	 limit_get/2,
	 get_root/0,
	 stream_reopen_file/1
        ]).

%% Extended API
-export([delete_files/3]).

%%----------------------------------------------------------------------
%% Log Service IMM API
%%----------------------------------------------------------------------
-export([change_filter/2]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 trace_groups/0,
	 trace_points_list/0,
	 callbacks_initialize/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SAF_LOG_APP, "safApp=safLogService").
-define(LOG_RT_CLASS, "safLgStr=").
-define(LOG_CFG_CLASS, "safLgStrCfg=").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------



%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% Library Life Cycle
%%--------------------------------------------------------------------

%%======================================================================
%% @doc 
%% This function initializes the Log Service for the invoking process 
%% and registers the various callback functions. 
%% This function must be invoked prior to the invocation of any
%% other Log Service functionality
%% @end
%%======================================================================
-spec initialize(Callbacks, Version) -> Result when
      Callbacks :: sa_log_callbacks(),
      Version :: #safsVersion{},
      Result :: {ok, Handle, SupportedVersion} | 
		{error, atom(), SupportedVersion},
      Handle :: sa_log_handle(),
      SupportedVersion :: #safsVersion{}.
initialize(Callbacks, Version) ->
    try
	verify(callbacks, Callbacks),
	verify(version, Version),
	case validate_version(Version) of
	    {ok, SupportedVersion}  ->
		call({initialize,
		      {i_cb(Callbacks), SupportedVersion, self()}});
	    {error, Error, SupportedVersion} ->
		safs_error(initialize, "version error"),
		{error, Error, SupportedVersion}
	end
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode, #safsVersion{releaseCode=?LOG_RELEASE_CODE,
					    majorVersion=?LOG_MAJOR_VERSION,
					    minorVersion=?LOG_MINOR_VERSION}}
    end.


i_cb(Fun) when is_function(Fun) ->
    #safsLogCallbacks{_ = Fun};
i_cb(CB) ->
    CB.
	    

%%======================================================================
%% @doc 
%% The finalize/1 function closes the association represented by 
%% the Handle parameter between the invoking process and the Log Service. 
%% The process must have invoked initialize/[1,2] before it invokes this 
%% function. A process must invoke this function once for each handle 
%% acquired by invoking initialize.
%% @end
%%======================================================================
-spec finalize(Handle::sa_log_handle()) -> ok | {error, atom()}.
finalize(Handle) ->
    try
	verify(handle, Handle),
	call({finalize, Handle})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.




%%----------------------------------------------------------------------
%% Log Service Operations
%%----------------------------------------------------------------------

%%======================================================================
%% @doc 
%% The stream_open_2() function opens a log stream. If the log stream 
%% is an application log stream and the named application log stream does 
%% not exist, the structure to which CreateAttributes points must be
%% populated and the SA_LOG_STREAM_CREATE flag must be set in the OpenFlags 
%% parameter.
%%
%% For the three well-known log streams, the returned log stream handle 
%% refers to the existing alarm, notification, or system log streams, 
%% which are created when the Log Service is initialized in the cluster. 
%% These log streams persist over the lifetime of the Log Service in the 
%% cluster.
%%
%% An invocation of stream_open_2() is blocking. If the log stream 
%% is successfully opened, a new log stream handle is returned upon 
%% completion. A log stream can be opened multiple times from within 
%% the same process or by different processes.
%% @end
%%======================================================================
-spec stream_open_2(Handle, StreamName, CreateAttributes, 
		    OpenFlags, Timeout) -> 
    Result 
	when
	    Handle :: sa_log_handle(),
    StreamName :: sa_log_stream_name(),
    CreateAttributes :: sa_log_create_attributes(),
    OpenFlags :: sa_log_open_flags(),
    Timeout :: sa_log_timeout(),
    Result :: {ok, sa_log_identifier()} | {error, atom()}.
stream_open_2(Handle,
	      StreamName,
	      CreateAttributes,
	      OpenFlags,
	      Timeout) ->
    try
	verify(handle, Handle),
	verify(create_attributes, to_str(CreateAttributes)),
	verify(open_flags, OpenFlags),
	verify(timeout, Timeout),
        StreamName2 = verify_stream_name(to_str(StreamName)),
	Params = {Handle, 
		  StreamName2, 
		  to_str(CreateAttributes), 
		  OpenFlags,
		  Timeout},
	call({stream_open_2, Params})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.



%%======================================================================
%% @doc 
%% The stream_open_async_2() function opens a log stream. If the log stream 
%% is an application log stream and the named application log stream does not 
%% exist, the structure to which CreateAttributes points must be populated 
%% and the SA_LOG_STREAM_CREATE flag must be set in the OpenFlags parameter.
%%
%% For the three well-known log streams, the returned log stream handle refers 
%% to the existing alarm, notification, or system log streams, which are 
%% created when the Log Service is initialized in the cluster. These log 
%% streams persist over the lifetime of the Log Service in the cluster.
%%
%% A log stream can be opened multiple times from within the same process 
%% or by different processes.
%%
%% Completion of the stream_open_async_2() function is signaled by an 
%% invocation of the associated stream_open_callback function, which must
%% have been supplied when the process invoked the initialize() call. 
%% The process supplies the value of invocation when it invokes the 
%% stream_open_async_2() function, and the Log Service gives that value 
%% of invocation back to the application when it invokes the corresponding 
%% stream_open_callback() function. The invocation parameter is a mechanism
%% that enables the process to determine which call triggered which callback.
%% @end
%%======================================================================
-spec stream_open_async_2(Handle, StreamName, CreateAttributes, OpenFlags, Invocation) -> 
    Result 
	when
	    Handle :: sa_log_handle(),
            StreamName :: sa_log_stream_name(),
            CreateAttributes ::  sa_log_create_attributes(), 
            OpenFlags :: sa_log_open_flags(),
            Invocation ::  sa_log_invocation(),
            Result :: {ok, sa_log_identifier()} | {error, atom()}.
stream_open_async_2(_Handle,
		    _StreamName,
		    _CreateAttributes,
		    _OpenFlags,
		    _Invocation) ->
    {error, ?SA_ERR_LIBRARY}.


%%======================================================================
%% @doc 
%% The API function is used to log a record to a stream specified by 
%% the StreamHandle.
%% An invocation of write_log() is blocking. 
%% @end
%%======================================================================
-spec write_log(StreamHandle, Timeout, LogRecord) -> 
    Result 
	when
	    StreamHandle :: sa_log_stream_handle(),
            Timeout :: sa_log_timeout(),
            LogRecord :: sa_log_log_record(),
            Result :: ok | {error, atom()}.
write_log(_StreamHandle, _Timeout, _LogRecord) ->
    {error, ?SA_ERR_LIBRARY}.




%%======================================================================
%% @doc 
%% The API function is used to log a record to a stream specified by 
%% the StreamHandle.
%% @end
%%======================================================================
-spec write_log_async(StreamHandle, Invocation, AckFlags, LogRecord) -> 
    Result 
	when
	    StreamHandle :: sa_log_stream_handle(),
            Invocation :: sa_log_invocation(),
            AckFlags :: sa_log_ack_flags(),
            LogRecord :: sa_log_log_record(),
            Result :: ok | {error, atom()}.
write_log_async(StreamHandle, Invocation, AckFlags, LogRecord) ->
    try
	verify(stream_handle, to_str(StreamHandle)),
	verify(invocation, Invocation),
	verify(ack_flags, AckFlags),
	verify(log_record, LogRecord),
	Params = {to_str(StreamHandle), Invocation, AckFlags, LogRecord},
	call({write_log_async, Params})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.




%%======================================================================
%% @doc 
%% The invocation of this API function closes the log stream which is 
%% designated by StreamHandle and which was opened by an earlier invocation 
%% of stream_open_2() or stream_open_async_2().
%%
%% After this invocation, the handle StreamHandle is no longer valid.
%% This call frees all resources allocated for this process by the Log Service 
%% on the log stream identified by the handle logStreamHandle.
%% This call cancels all pending callbacks that refer directly or indirectly 
%% to the handle StreamHandle. Note that as the callback invocation is 
%% asynchronous, it is still possible that some callback calls are processed 
%% after this call returns successfully.
%%
%% If the invocation of the saLogStreamClose() function completes successfully,
%% and the log stream is an application log stream, and no other process has 
%% that application log stream open, the Log Service behaves as follows.
%%  - The log stream is deleted.
%%  - The log file associated with that application log stream is closed and 
%%    renamed with a `closetime' that indicates when the last user of the log 
%%    stream designated by logStreamHandle closed the stream 
%%  - The log file configuration file associated with the deleted log stream 
%%    is closed and persists indefinitely.
%% @end
%%======================================================================
-spec stream_close(StreamHandle) -> 
    Result 
	when
	    StreamHandle :: sa_log_stream_handle(),
            Result :: ok | {error, atom()}.
stream_close(StreamHandle) ->
    try
	verify(stream_handle, to_str(StreamHandle)),
	call({stream_close, to_str(StreamHandle)})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.


%%======================================================================
%% @doc 
%% The API function is used to fetch the value of a Log Service Limit. 
%% An invocation of limit_get() is blocking. 
%% @end
%%======================================================================
-spec limit_get(Handle, LimitId) -> 
    Result 
	when
	    Handle :: sa_log_handle(),
            LimitId :: sa_log_limit_id(),
            Result :: {ok, sa_log_limit_value()} | {error, atom()}.
limit_get(Handle, LimitId) ->
    try
	verify(handle, Handle),
	verify(limit_id, LimitId),
	call({limit_get, {Handle, LimitId}})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.


%%======================================================================
%% @doc 
%% This function close a stream file and reopens it
%% without the need of first invoking stream_close and then stream_open_2.
%% 
%% The reason is to be able to restart a halt log wihtout the need of
%% updating StreamHandle.
%% @end
%%======================================================================
-spec stream_reopen_file(StreamHandle) -> 
    Result 
	when
	    StreamHandle :: sa_log_stream_handle(),
            Result :: ok | {error, atom()}.
stream_reopen_file(StreamHandle) ->
    try
	verify(stream_handle, to_str(StreamHandle)),
	call({stream_reopen_file, {to_str(StreamHandle)}})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.

%%======================================================================
%% @doc 
%% The API function is used to delete old log files.
%% The function will search for all log files with FileName in the
%% requested directory FilePathName and delete them. 
%% However, if there exists one stream that is using one of the matched 
%% files the function will not delete any of the files. 
%% In this case the function will return error code BUSY.
%% 
%% An invocation of delete_files() is blocking. 
%% @end
%%======================================================================
-spec delete_files(Handle, FileName, FilePathName) -> 
    Result 
	when
	    Handle :: sa_log_handle(),
	    FileName :: string(),
            FilePathName :: string(),
            Result :: ok | {error, atom()}.
delete_files(_Handle, FileName, FilePathName) ->
    try
	verify(file_name, to_str(FileName)),
	verify(file_path_name, to_str(FilePathName)),
	call({delete_files, {to_str(FileName), to_path(FilePathName)}})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.



%%======================================================================
%% @doc 
%% The API function is used to update the filter settings for a stream.
%% @end
%%======================================================================
-spec change_filter(StreamName, SeverityFlags) -> 
    Result 
	when
	    StreamName :: string(),
            SeverityFlags :: sa_log_severity_flags_record(),
            Result :: ok | {error, atom()}.
change_filter(StreamName, SeverityFlags) ->
    try 
	verify(stream_name, to_str(StreamName)),
	verify(severity_flags, SeverityFlags),
	call({change_filter, to_str(StreamName), SeverityFlags})
    catch
	throw:{?MODULE, ErrorCode, Reason} ->
	    safs_error(verify, Reason),
	    {error, ErrorCode}
    end.

%%======================================================================
%% @doc 
%% The API function is used to fetch the log root directory.
%% @end
%%======================================================================
-spec get_root() -> 
    Result 
	when
            Result :: string() | undefined.
get_root() ->
    safs:get_env(log_root, undefined).

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [life_cycle, service, error].

trace_points_list() ->
    [
     {error,
      [{safs_error, 2}]},
     {life_cycle,
      [{initialize, 2},
       {finalize, 1}]},
     {service,
      [{stream_open_2, 5},
       {stream_open_async_2, 5},
       {write_log, 3},
       {write_log_async, 4},
       {stream_close, 1}
      ]}
    ].

callbacks_initialize(Handle) ->
    call({callbacks_initialize, Handle, self()}).

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description:
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
call(Request) ->
    gen_server:call(?SERVER, Request, infinity).

%%----------------------------------------------------------------------
%% @private
%% verify(Type, Val) -> ok | throw
%%----------------------------------------------------------------------
verify(handle, V) 
  when is_integer(V) -> 
    ok;
verify(handle = T, V) ->
    throw({?MODULE, ?SA_ERR_BAD_HANDLE, {T, V}});
verify(stream_handle, V) 
  when is_integer(V) -> 
    ok;
verify(timeout, V)
  when is_integer(V) -> 
    ok;
verify(open_flags, V)
  when V == undefined;
       V == ?LOG_OPEN_FLAGS ->
    ok;
verify(open_flags = T, V) ->
    throw({?MODULE, ?SA_ERR_BAD_FLAG, {T, V}});
verify(ack_flags, V)
  when V == undefined;
       V == ?LOG_ACK_FLAGS->
    ok;
verify(stream_name, V)
  when is_list(V), V =/= [] -> 
    verify_stream_name(V);
verify(create_attributes, undefined) -> 
    ok;
verify(create_attributes, V) 
  when is_record(V, safsLogFileCreateAttributes_2) -> 
    verify_create_attributes(V);
verify(callbacks, undefined) ->
    ok;
verify(callbacks, V)
  when is_function(V) -> 
    ok;
verify(callbacks, V)
  when is_record(V, safsLogCallbacks) -> 
    verify_cb(V);
verify(version, V)
  when is_record(V, safsVersion) ->
    ok;
verify(log_record, V)
  when is_record(V, safsLogRecord) ->
    ok;
verify(invocation, V)
  when is_integer(V) ->
    ok;
verify(severity_flags, undefined) -> 
    ok;
verify(severity_flags, V) 
  when is_record(V, safsLogSeverityFlags) -> 
    ok;
verify(file_name, V)
  when is_list(V) -> 
    ok;
verify(file_path_name, V)
  when is_list(V) -> 
    ok;
verify(limit_id, ?LIMIT_MAX_APP_STREAMS) ->
    ok;
verify(T, V) ->
    throw({?MODULE, ?SA_ERR_INVALID_PARAM, {T, V}}).



%%----------------------------------------
%% verify create_attributes
%%----------------------------------------
%% Check that mandatory parameters are set
verify_create_attributes(CA) ->
    #safsLogFileCreateAttributes_2{logFileName       = FileName,
				   logFilePathName   = PathName,
				   maxLogFileSize    = MaxFileSize,
				   maxLogRecordSize  = MaxRecSize,
				   haProperty        = HAProp,
				   logFileFullAction = FullAction,
				   maxFilesRotated   = MaxRotated,
				   logFileFmt        = Format} = CA,
    Vals = [FileName, PathName, MaxFileSize, MaxRecSize, HAProp, FullAction,
	    MaxRotated, Format],
    Fields = record_info(fields, safsLogFileCreateAttributes_2),
    lists:foreach(fun verify_ca/1, lists:zip(Fields, Vals)).


verify_ca({logFileName, FileName}) 
  when is_list(FileName), FileName =/= [] ->
    ok;

verify_ca({logFilePathName = Attr, PathName})
  when is_list(PathName), PathName =/= [] ->
    case re:run(PathName, "[.]{2}") of
	nomatch ->
	    ok;
	_Match ->
	    throw({?MODULE, ?SA_ERR_INVALID_PARAM, 
		   {{create_attributes, Attr}, PathName}})
    end;

verify_ca({maxLogFileSize, MaxFileSize}) when MaxFileSize >= 0 ->
    ok;

verify_ca({maxLogRecordSize, MaxRecSize}) when MaxRecSize >= 0 ->
    ok;
    
verify_ca({haProperty, HAProp}) when is_boolean(HAProp) ->
    ok;
    
verify_ca({logFileFullAction, FullAction}) 
  when FullAction =:= ?LOG_WRAP;
       FullAction =:= ?LOG_HALT;
       FullAction =:= ?LOG_ROTATE ->
    ok;
    
verify_ca({maxFilesRotated, MaxRotated}) when MaxRotated >= 0 ->
    ok;
    
verify_ca({logFileFmt, Format}) 
  when is_list(Format) ->
    ok;

verify_ca({A, V}) ->
    throw({?MODULE, ?SA_ERR_INVALID_PARAM, 
	   {{create_attributes, A}, V}}).


%%----------------------------------------
%% verify stream name
%%----------------------------------------
%% Must be a proper DN
verify_stream_name(V) when is_list(V), V =/= [] ->
    case string:tokens(V, ",") of
	[?LOG_RT_CLASS ++ _Name] ->
	    V ++"," ++ ?SAF_LOG_APP;
	[?LOG_CFG_CLASS ++ _Name] ->
	    V ++"," ++ ?SAF_LOG_APP;
	[?LOG_RT_CLASS ++ _Name, ?SAF_LOG_APP] ->
	    V;
	[?LOG_CFG_CLASS ++ _Name, ?SAF_LOG_APP] ->
	    V;
	_ ->
	    throw({?MODULE, ?SA_ERR_INVALID_PARAM, 
		   {stream_name, V}})
    end;
verify_stream_name(V) ->
    throw({?MODULE, ?SA_ERR_INVALID_PARAM, 
		   {stream_name, V}}).

%% Stream name verification should be:
%% verify_stream_name(?LOG_RT_CLASS ++ Name) ->
%%       ok;
%% verify_stream_name(?LOG_CFG_CLASS ++ Name) ->
%%       ok;
%% verify_stream_name(_) ->
%%       {error, ?SA_ERR_INVALID_PARAM}.
%%

%%----------------------------------------
%% verify callbacks
%%----------------------------------------
%% C-interface requires a boolean
verify_cb(#safsLogCallbacks{saLogFilterSetCallback  = Filter,
			    saLogStreamOpenCallback = Open,
			    saLogWriteLogCallback   = Write}) 
  when is_boolean(Filter),
       is_boolean(Open),
       is_boolean(Write) ->
    ok;
%% Erlang interface requires {M,F}, Fun or undefined
verify_cb(#safsLogCallbacks{saLogFilterSetCallback  = Filter,
			    saLogStreamOpenCallback = Open,
			    saLogWriteLogCallback   = Write}) ->
    
    Filter == undefined orelse 
	is_mf(Filter) orelse 
	is_function(Filter) orelse 
	throw({?MODULE, ?SA_ERR_INVALID_PARAM, ?SUPPORTED_LOG_VERSIONS}),
    Open == undefined orelse 
	is_mf(Open) orelse 
	is_function(Open) orelse 
	throw({?MODULE, ?SA_ERR_INVALID_PARAM, ?SUPPORTED_LOG_VERSIONS}),
    Write == undefined orelse 
	is_mf(Write) orelse 
	is_function(Write) orelse 
	throw({?MODULE, ?SA_ERR_INVALID_PARAM, ?SUPPORTED_LOG_VERSIONS}),
    ok.
%% verify_cb(V) ->
%%     throw({?MODULE, ?SA_ERR_INVALID_PARAM, {callbacks, V}}).

is_mf({_,_}) -> true;
is_mf(_)     -> false.
	
	  

%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
validate_version(#safsVersion{releaseCode=?LOG_RELEASE_CODE,
			      majorVersion=?LOG_MAJOR_VERSION} = Version) ->
    {ok, Version#safsVersion{minorVersion=?LOG_MINOR_VERSION}};
validate_version(_Version) ->
    {error, sa_ais_err_version, #safsVersion{releaseCode=?LOG_RELEASE_CODE,
					     majorVersion=?LOG_MAJOR_VERSION,
					     minorVersion=?LOG_MINOR_VERSION}}.


%% If the implementation supports the specified releaseCode and majorVersion,
%% SA_AIS_OK is returned. In this case, the structure pointed to by the 
%% version parameter is set by this function to:
%%  - releaseCode  = required release code
%%  - majorVersion = highest value of the major version that this 
%%                   implementation can support for the required releaseCode
%%  - minorVersion = highest value of the minor version that this 
%%                   implementation can support for the required value of  
%%                   releaseCode and the returned value of majorVersion
% ivs_req([{ReqRC, ReqMajor, _} | _],
% 	#safsVersion{releaseCode  = ReqRC,
% 		     majorVersion = ReqMajor}) ->
%     {true, ivs_get_highest_vsn(ReqRC, ?SUPPORTED_LOG_VERSIONS)};
% ivs_req([_ | T], Version) ->
%     ivs_req(T, Version);
% ivs_req([], Version) ->
%     ivs_lower(?SUPPORTED_LOG_VERSIONS, Version).


%% If the preceding condition cannot be met, SA_AIS_ERR_VERSION is returned,
%% and the structure pointed to by the version parameter is set to:
%% same as above if the requested releaseCode is supported
%%  - releaseCode  = required release code
% ivs_lower([{ReqRC, _, _} | _],
% 	  #safsVersion{releaseCode = ReqRC}) ->
%     {false, ivs_get_highest_vsn(ReqRC, ?SUPPORTED_LOG_VERSIONS)};
% ivs_lower([_ | T], Version) ->
%     ivs_lower(T, Version);
% ivs_lower([], Version) ->
%     ivs_other(?SUPPORTED_LOG_VERSIONS, Version).


%% if the requested releaseCode is not supported:
%% - releaseCode  = the lowest value of the supported release codes that is
%%                  higher than the required releaseCode or
%%                  the highest value of the supported release codes that is
%%                  lower than the required releaseCode
%% - majorVersion = highest value of the major versions that this 
%%                  implementation can support for the returned releaseCode
%% - minorVersion = highest value of the minor versions that this 
%%                  implementation can support for the returned values of 
%%                  releaseCode and majorVersion
% ivs_other([{RC, _, _}, {ReqRC, _, _} | _],
% 	  #safsVersion{releaseCode = ReqRC}) ->
%     {false, ivs_get_highest_vsn(RC, ?SUPPORTED_LOG_VERSIONS)};
%% ivs_other([{ReqRC, _, _}, {RC, _, _} | _],
%% 	  #safsVersion{releaseCode = ReqRC}) 
%%   when ReqRC /= RC ->
%%     {false, ivs_get_highest_vsn(RC, ?SUPPORTED_LOG_VERSIONS)};
% ivs_other([{RC, _, _}], _) ->
%     {false, ivs_get_highest_vsn(RC, ?SUPPORTED_LOG_VERSIONS)};
% ivs_other([_ | T], Version) ->
%     ivs_other(T, Version).




% ivs_get_highest_vsn(RC, [{RC, Major, Minor} | _]) ->
%     #safsVersion{releaseCode  = RC,
% 		 majorVersion = Major,
% 		 minorVersion = Minor};
% ivs_get_highest_vsn(RC, [_ | T]) ->
%     ivs_get_highest_vsn(RC, T).






%%----------------------------------------------------------------------
%% @private
%% to_str(any()) -> string()
%%----------------------------------------------------------------------
to_str(Str) when is_list(Str) ->
    Str;
to_str(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_str(#safsLogFileCreateAttributes_2{logFileName     = FileName,
				      logFilePathName = PathName,
				      logFileFmt      = Format} = CA) ->
    CA#safsLogFileCreateAttributes_2{logFileName     = to_str(FileName),
				     logFilePathName = to_path(PathName),
				     logFileFmt      = to_fmt(Format)};
to_str(Other) ->
    Other.

to_fmt(undefined) ->
    ?LOG_APP_FORMAT;
to_fmt(Fmt) ->
    to_str(Fmt).

to_path(undefined) ->
    ".";
to_path(Other) ->
    to_str(Other).


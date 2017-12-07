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
%% File: safs_log_com.erl
%%
%% Description:
%%    This file implements the interface that handles incoming
%%    LOG requests on the sockets from C. The interface also handles 
%%    callbacks from Erlang to C.
%%
%%--------------------------------------------------------------------
-module(safs_log_com).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%% -include("log.hrl").
-include("safs_internal.hrl").
%% -include("safs_ais.hrl").
-include("safs_log.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         callback_message_async/3,
         close/1,
	 init/2,
         message/2,
	 trace_groups/0,
	 trace_points_list/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 safs_error/2 %% Temp fix to get dialyzer happy
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DUMMY_HANDLE, 0).
-define(DUMMY_LIMIT, #safsLimitValue{uint64Value = 0}).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/2
%% Description:
%%--------------------------------------------------------------------
init(_Proxy, _Cb) ->
    {ok, undefined}.

%%--------------------------------------------------------------------
%% Function: message/2
%% Description:
%%--------------------------------------------------------------------
message(Bytes, State) ->
    case catch log:decode_msg(Bytes, safsLogMessage) of
	#safsLogMessage{} = Msg ->
            {result_msg(Msg), State};
        _Error ->
            error_logger:format("~p~p couldn't decode message:\n\t~p\n",
                                [?MODULE, self(), Bytes]),
            {{error, bad_message}, State}
    end.

%%--------------------------------------------------------------------
%% Function: callback_message_async/4
%% Description:
%%--------------------------------------------------------------------
callback_message_async(Pid, Cb, Arg) ->
    case encode_callback_msg(Cb, Arg) of
        {ok, Bin} ->
            safs_com_inproxy:send(Pid, Bin);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: close/1
%% Description:
%%--------------------------------------------------------------------
close(Proxy) ->
    safs_com_inproxy:stop(Proxy).

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [error].

trace_points_list() ->
    [
     {error,
      [{safs_error, 2}]}
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description:
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Internal functions
%%====================================================================
result_msg(#safsLogMessage{initialize = Initialize}) 
  when Initialize =/= undefined ->
    R = initialize_ret(Initialize),
    log:encode_msg(R);

result_msg(#safsLogMessage{finalize = Finalize}) 
  when Finalize =/= undefined ->
    R = finalize_ret(Finalize),
    log:encode_msg(R);

result_msg(#safsLogMessage{logStreamOpen = Open}) 
  when Open =/= undefined ->
    R = logStreamOpen_ret(Open),
    log:encode_msg(R);

result_msg(#safsLogMessage{logStreamOpenAsync = OpenAs}) 
  when OpenAs =/= undefined ->
    R = logStreamOpenAsync_ret(OpenAs),
    log:encode_msg(R);

result_msg(#safsLogMessage{logWriteLog = Write}) 
  when Write =/= undefined ->
    R = logWriteLog_ret(Write),
    log:encode_msg(R);

result_msg(#safsLogMessage{logWriteLogAsync = WriteAs}) 
  when WriteAs =/= undefined ->
    _R = logWriteLogAsync_ret(WriteAs),
    %% No ack on logWriteLogAsync, c-side does not expect any.
    no_reply;

result_msg(#safsLogMessage{logStreamClose = Close}) 
  when Close =/= undefined ->
    R = logStreamClose_ret(Close),
    log:encode_msg(R);

result_msg(#safsLogMessage{logLimitGet = LGet}) 
  when LGet =/= undefined ->
    R = logLimitGet_ret(LGet),
    log:encode_msg(R);

result_msg(#safsLogMessage{callbacksInitialize = CI}) 
  when CI =/= undefined ->
    R = callbacksInitialize_ret(CI),
    log:encode_msg(R);

result_msg(#safsLogMessage{logDeleteFiles = CI}) 
  when CI =/= undefined ->
    R = logDeleteFiles_ret(CI),
    log:encode_msg(R).

initialize_ret(Initialize) ->
    Version = Initialize#safsLogInitialize.version,
    case safs_log:initialize(Initialize#safsLogInitialize.callbacks,
			     Version) of
	{ok, Handle, SupportedVersion} ->
	    #safsLogInitializeRet{returnVal = ?SA_OK,
				  handle    = Handle,
				  version   = SupportedVersion};
	{error, Error, SupportedVersion} ->
	    #safsLogInitializeRet{returnVal = Error,
				  handle    = ?DUMMY_HANDLE,
				  version   = SupportedVersion}
    end.

finalize_ret(Finalize) ->
    case safs_log:finalize(Finalize#safsLogFinalize.handle) of
	ok ->
	    #safsLogFinalizeRet{returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogFinalizeRet{returnVal = Error}
    end.


logStreamOpen_ret(Open) ->
    Handle = Open#safsLogStreamOpen_2.handle, 
    StreamName = Open#safsLogStreamOpen_2.logStreamName, 
    CreateAttributes = Open#safsLogStreamOpen_2.logFileCreateAttributes, 
    OpenFlags = Open#safsLogStreamOpen_2.logStreamOpenFlags, 
    Timeout = Open#safsLogStreamOpen_2.timeout,
    case safs_log:stream_open_2(Handle, StreamName, CreateAttributes, 
				OpenFlags, Timeout) of
	{ok, LSHandle} ->
	    #safsLogStreamOpen_2Ret{logStreamHandle = LSHandle,
				    returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogStreamOpen_2Ret{logStreamHandle = ?DUMMY_HANDLE,
				    returnVal = Error}
    end.
	    

logStreamOpenAsync_ret(OpenAs) ->
    Handle = OpenAs#safsLogStreamOpenAsync_2.handle, 
    StreamName = OpenAs#safsLogStreamOpenAsync_2.logStreamName, 
    CreateAttributes = OpenAs#safsLogStreamOpenAsync_2.logFileCreateAttributes, 
    OpenFlags = OpenAs#safsLogStreamOpenAsync_2.logStreamOpenFlags, 
    Invocation = OpenAs#safsLogStreamOpenAsync_2.invocation,
    case safs_log:stream_open_async_2(Handle, StreamName, CreateAttributes, 
				      OpenFlags, Invocation) of
	%% ok ->
	%%     #safsLogStreamOpenAsync_2Ret{returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogStreamOpenAsync_2Ret{returnVal = Error}
    end.
	    

logWriteLog_ret(Write) ->
    LSHandle = Write#safsLogWriteLog.logStreamHandle,
    Timeout = Write#safsLogWriteLog.timeout,
    LogRec = Write#safsLogWriteLog.logRecord,
    case safs_log:write_log(LSHandle, Timeout, LogRec) of
	%% ok -> 
	%%     #safsLogWriteLogRet{returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogWriteLogRet{returnVal = Error}
    end.


logWriteLogAsync_ret(WriteAs) ->
    LSHandle = WriteAs#safsLogWriteLogAsync.logStreamHandle,
    Invocation = WriteAs#safsLogWriteLogAsync.invocation,
    AckFlags = WriteAs#safsLogWriteLogAsync.ackFlags,
    LogRec = WriteAs#safsLogWriteLogAsync.logRecord,
    case safs_log:write_log_async(LSHandle, Invocation, AckFlags, LogRec) of
	ok -> 
	    #safsLogWriteLogAsyncRet{returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogWriteLogAsyncRet{returnVal = Error}
    end.


logStreamClose_ret(Close) ->
    LSHandle = Close#safsLogStreamClose.logStreamHandle,
    case safs_log:stream_close(LSHandle) of
	ok ->
	    #safsLogStreamCloseRet{returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogStreamCloseRet{returnVal = Error}
    end.


logLimitGet_ret(LGet) ->
    Handle = LGet#safsLogLimitGet.handle,
    LimitId = LGet#safsLogLimitGet.limitId,
    case safs_log:limit_get(Handle, LimitId) of
	{ok, LimitVal} ->
	    LV = #safsLimitValue{uint64Value = LimitVal},
	    #safsLogLimitGetRet{limitValue = LV,
				returnVal  = ?SA_OK};
	{error, Error} ->
	    #safsLogLimitGetRet{limitValue = ?DUMMY_LIMIT,
				returnVal  = Error}
    end.


callbacksInitialize_ret(CI) ->
    case safs_log:callbacks_initialize(CI#safsLogCallbacksInitialize.handle) of
	ok ->
	    #safsLogCallbacksInitializeRet{returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogCallbacksInitializeRet{returnVal = Error}
    end.

logDeleteFiles_ret(LGet) ->
    Handle       = LGet#safsLogDeleteFiles.handle,
    FileName     = LGet#safsLogDeleteFiles.logFileName,
    FilePathName = LGet#safsLogDeleteFiles.logFilePathName,
    case safs_log:delete_files(Handle, FileName, FilePathName) of
	ok ->
	    #safsLogDeleteFilesRet{returnVal = ?SA_OK};
	{error, Error} ->
	    #safsLogDeleteFilesRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: encode_callback_msg/3
%% Description:
%%--------------------------------------------------------------------
%% @private
encode_callback_msg(Cb, Arg) ->
    try
        Bin = encode_callback(Cb, Arg),
        {ok, Bin}
    catch
        _: Reason ->
            ?ERROR("Failed to encode LOG ~w Callback message. Arg = ~p~n"
                   "Reason: ~p~n"
                   "Stack:~n~p~n",
                   [Cb, Arg, Reason, erlang:get_stacktrace()]),
            {error, ?SA_ERR_INVALID_PARAM}
    end.


encode_callback(stream_open, {Invocation, LSHandle, ok}) ->
    encode_callback(stream_open, {Invocation, LSHandle, ?SA_OK});

encode_callback(stream_open, {Invocation, LSHandle, {error, Error}}) ->
    encode_callback(stream_open, {Invocation, LSHandle, Error});

encode_callback(stream_open, {Invocation, LSHandle, Error}) ->
    Cb = #saLogStreamOpenCallback{invocation = Invocation,
				  logStreamHandle = LSHandle,
				  error = Error},
    CbMsg = #saLogCallbacks{logStreamOpenCallback = Cb},
    encode_callback(CbMsg);
    
encode_callback(write_log, {Invocation, ok}) ->
    encode_callback(write_log, {Invocation, ?SA_OK});

encode_callback(write_log, {Invocation, {error, Error}}) ->
    encode_callback(write_log, {Invocation, Error});

encode_callback(write_log, {Invocation, Error}) ->
    Cb = #saLogWriteLogCallback{invocation = Invocation,
				error = Error},
    CbMsg = #saLogCallbacks{logWriteLogCallback = Cb},
    encode_callback(CbMsg);
    
encode_callback(filter_set, {LSHandle, Severity}) ->
    Cb = #saLogFilterSetCallback{logStreamHandle = LSHandle,
				 logSeverity = Severity},
    CbMsg = #saLogCallbacks{logFilterSetCallback = Cb},
    encode_callback(CbMsg).


encode_callback(CbMsg) ->
    log:encode_msg(CbMsg).


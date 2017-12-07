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
%% File: safs_log_file.erl
%%
%% Description:
%%
%% Log service file handler, i.e. open, close, write log entries.
%% The module uses disk_log but only in halt mode, becase the
%% naming of the files does not correspond to the file naming
%% of disk_log files.
%%
%% In case of rotate stream, the module opens and closes single
%% halt disk_log files and uses internal rotate functionality
%% to get the desired effect.
%%--------------------------------------------------------------------
-module(safs_log_file).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("safs_log.hrl").
-include("safs_log_db.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([awrite/5]).
-export([close_log/6]).
-export([open_log/5]).
-export([reopen_file/7]).

%% Extended API
-export([delete_files/2]).

%% safs internal API
-export([get_cfg_params/1]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(DEFAULT_ROOT, ".").
-define(MAX_SEQUENCE_NO, "9999999999").
-define(NEW_SEQUENCE_NO, 1).

-define(FIXED_LOG_OPTS, [{format, external},
			 {mode,   read_write},
			 {type,   halt}]).


-define(LOG_DEF_CNF, [{alarm_rec_format_expr,  undefined},
		      {notify_rec_format_expr, undefined},
		      {system_rec_format_expr, undefined},
		      {alarm_log_max_file_size,  100000},
		      {notify_log_max_file_size, 100000},
		      {system_log_max_file_size, 100000},
		      {alarm_fixed_log_rec_size,  150},
		      {notify_fixed_log_rec_size, 150},
		      {system_fixed_log_rec_size, 150},
		      {alarm_full_action,  sa_log_file_full_action_rotate},
		      {notify_full_action, sa_log_file_full_action_rotate},
		      {system_full_action, sa_log_file_full_action_rotate},
		      {alarm_max_rotated,  4},
		      {notify_max_rotated, 4},
		      {system_max_rotated, 4}]).

%% File name definitions
%%
%% FileName         - The log file name as given from the application
%% FilePathName     - The log file path as given from the application
%% FileNameDT       - Log file name including date and time part but w.o. ext
%% AbsLogFile       - Absolute Log file name including date and time part + ext
%% CfgFileNameDT    - Cfg file name including date and time part but w.o. ext
%% AbsCfgFileNameDT - Absolute Cfg file name including date and time part
%%                    but w.o. ext
%% AbsDirName       - Absolute directory name, i.e root dir + FilePathName
%% FqFileDT         - Fully qualified file name
%%                    i.e. AbsDirName + FileName + date and time, but w.o. ext

-record(file_info, {file_name_dt,
		    abs_dir_name,
		    fq_file_name_dt
		   }).

%%======================================================================
%% External Interface Functions
%%======================================================================

%%========================================================================
%% open_log(FileNameDT, StreamName, Attrs, Version) ->
%%    {ok, Result, FileData, SequenceNo} | {error, Reason}
%%
%% FileNameDT = string() - file name + date and time without path and extention
%% StreamName = string()
%% Attrs      = #safsLogFileCreateAttributes_2{}
%% Version    =
%%
%% Open a disk_log file for the stream.
%% disk_log is not used in wrap-mode (for the rotate stream logs)
%% but the rotation is handled in this module; this because the
%% naming of the files is not compatible with the disk_log file naming.
%%========================================================================
open_log(FilePathName, FileNameDT, StreamName, Attrs, Version) ->
    #safsLogFileCreateAttributes_2{logFileName     = FileName,
				   maxLogFileSize  = FileMaxSize} = Attrs,
    RootDir       = safs:get_env(log_root, ?DEFAULT_ROOT),
    AbsDirName    = join([RootDir, FilePathName]),
    FqFileNameDT  = join([RootDir, FilePathName, FileNameDT]),
    OpenFilesInfo = get_open_files_info(FileName, AbsDirName),
    FileInfo      = #file_info{abs_dir_name    = AbsDirName,
			       fq_file_name_dt = FqFileNameDT,
			       file_name_dt    = FileNameDT},
    OpenOpts = [{name, StreamName},
		{size, FileMaxSize}] ++ ?FIXED_LOG_OPTS,
    open_log_2(OpenFilesInfo, StreamName, OpenOpts, FileInfo, Attrs, Version).


%%====================================================================
%% open_log_2(OpenFilesInfo, StreamName, OpenOpts, FileInfo, Attrs, Version)
%%
%% OpenFilesInfo = {IsOpenFile, FileName, ClosedFiles, CfgFile}
%% IsOpenFile    = boolean()
%% FileNameDT    = string()
%% CfgFile       = string()   - the cfg file name with path but w.o file ext
%% ClosedFiles   = [string()] - closed files with path and extention
%%====================================================================
%%================================================================
%% Unclosed file found.
%% Restart by using the already existing log files.
%%================================================================
open_log_2({true, _, _, AbsCfgFileNameDT} = OpenFileInfo,
	   StreamName,
	   OpenOpts,
	   FileInfo,
	   Attrs,
	   Version) ->

    open_log_old(find_fmt(AbsCfgFileNameDT),
		 StreamName,
		 OpenOpts,
		 FileInfo,
		 Attrs,
		 Version,
		 OpenFileInfo);
%%================================================================
%% No unclosed file found. Start with new log files.
%% Or an unclosed file was found but the format differes.
%%================================================================
open_log_2({false, _, _, _},
	   StreamName,
	   OpenOpts,
	   #file_info{abs_dir_name    = AbsDirName,
		      file_name_dt    = FileNameDT,
		      fq_file_name_dt = FqFileNameDT},
	   Attrs,
	   Version) ->
    CfgFile  = get_cfg_file(StreamName, Attrs, Version),
    FileData = get_file_data(StreamName,
			     [FileNameDT],
			     join([AbsDirName, FileNameDT]),
			     Attrs),
    open_log_new(filelib:ensure_dir(FqFileNameDT ++ ".log"),
		 file:write_file(FqFileNameDT ++ ".cfg", CfgFile),
		 FileData,
		 OpenOpts ++ [{file, FqFileNameDT ++ ".log"}],
		 FqFileNameDT).


%%------------------------------------------------------------
%% An open log was found and the format is the same.
%% Resuse the old files.
%%------------------------------------------------------------
open_log_old(Fmt,
	     StreamName,
	     OpenOpts,
	     #file_info{abs_dir_name = AbsDirName},
	     #safsLogFileCreateAttributes_2{logFileFmt = Fmt} = Attrs,
	     _Version,
	     {_, FileNameDT, ClosedFiles, AbsCfgFileNameDT}) ->
    FilesNames = [get_file_name(F) || F <- ClosedFiles],
    AllFiles   = [FileNameDT] ++ FilesNames,
    FileData   = get_file_data(StreamName, AllFiles, AbsCfgFileNameDT, Attrs),
    ol_rc(disk_log:open(OpenOpts ++
			[{file, join([AbsDirName, FileNameDT ++ ".log"])}]),
	  FileData,
	  AbsCfgFileNameDT,
	  get_seq_no(AbsDirName, FileNameDT, Fmt));
%%------------------------------------------------------------
%% An open log was found but the format was different.
%% Open new files and leave the old ones as they are.
%%------------------------------------------------------------
open_log_old(_,
	     StreamName,
	     OpenOpts,
	     FileInfo,
	     Attrs,
	     Version,
	     {_, FileNameDT, ClosedFiles, AbsCfgFileNameDT}) ->
    open_log_2({false, FileNameDT, ClosedFiles, AbsCfgFileNameDT},
	       StreamName,
	       OpenOpts,
	       FileInfo,
	       Attrs,
	       Version).


%%------------------------------------------------------
%% If file library is ok and config file was written
%% open a disk log file for the stream.
%%------------------------------------------------------
open_log_new(ok, ok, FileData, OpenOpts, CfgFile) ->
    p(user, "$$$ disk_log:open  ~p  ~n", [OpenOpts]),
    ol_rc(disk_log:open(OpenOpts), FileData, CfgFile, ?NEW_SEQUENCE_NO);
open_log_new({error, _} = Error, _, _, OpenOpts, CfgFile) ->
    log(open_log, {file_dir, OpenOpts, Error}),
    ol_rc({error, ?SA_ERR_INVALID_PARAM}, undefined, CfgFile, ?NEW_SEQUENCE_NO);
open_log_new(_, {error, enospc} = Error, _, _, CfgFile) ->
    log(open_log, {cfg_file, CfgFile, Error}),
    ol_rc({error, ?SA_ERR_NO_MEMORY}, undefined, CfgFile, ?NEW_SEQUENCE_NO);
open_log_new(_, {error, eaccess} = Error, _, _, CfgFile) ->
    log(open_log, {cfg_file, CfgFile, Error}),
    ol_rc({error, ?SA_ERR_NO_RESOURCES}, undefined, CfgFile, ?NEW_SEQUENCE_NO);
open_log_new(_, Error, _, _, CfgFile) ->
    log(open_log, {cfg_file, CfgFile, Error}),
    ol_rc({error, ?SA_ERR_INVALID_PARAM}, undefined, CfgFile, ?NEW_SEQUENCE_NO).



%%================================================================
%% Return the result
%%================================================================
ol_rc({ok, Res}, FileData, _AbsCfgFileNameDT, SequenceNo) ->
    {ok, Res, FileData, SequenceNo};
ol_rc({error, _} = Error, _, AbsCfgFileNameDT, _) ->
    p(user, "$$$ file:delete ~p ~n", [AbsCfgFileNameDT]),
    file:delete(AbsCfgFileNameDT ++ ".cfg"),
    log(ol_rc, {disk_log, Error}),
    Error.

%%========================================================================
%% awrite(StreamConfig, StreamName, LogMsg, FileData, SeqNo) ->
%%     {ok, FileData} | {error, Error}
%%
%%
%%
%%========================================================================
awrite({ok, [#safs_log_stream_config{}] = StreamConf},
       StreamName,
       LogMsg,
       #file_data{rec_format   = RecFormat,
		  max_rec_size = MaxRecSize} = FileData,
       SeqNo) ->
    aw(safs_log_rec:format(LogMsg, SeqNo, RecFormat, MaxRecSize),
       StreamConf,
       StreamName,
       LogMsg,
       FileData,
       SeqNo);
awrite(Error, StreamName, _, FileData, _) ->
    log(awrite, Error),
    aw_rc({error, ?SA_ERR_LIBRARY}, StreamName, FileData).



aw({ok, FormattedMsg}, StreamConf, StreamName, LogMsg, FileData, SeqNo) ->
    p(user, "$$$ disk_log:blog ~p ~p  ~n", [StreamName, FormattedMsg]),
    WriteRes = disk_log:blog(StreamName, FormattedMsg),
    case aw_rc(WriteRes, StreamName, FileData) of
	{retry, NewFileData} ->
	    awrite({ok, StreamConf}, StreamName, LogMsg, NewFileData, SeqNo);
	Result ->
	    Result
    end;
aw(Error, _, StreamName, _, FileData, _) ->
    log(aw, Error),
    aw_rc({error, ?SA_ERR_LIBRARY}, StreamName, FileData).


%%------------------------------------------------------
%% aw_rc(Res, StreamName, FileData) ->
%%     {ok, FileData} | {error, Error}
%%
%% Return the result
%%------------------------------------------------------
aw_rc(ok, _, FileData) ->
    {ok, FileData};
aw_rc({error, {full, _}},
      StreamName,
      #file_data{type = ?LOG_ROTATE} = FileData) ->
    rotate(StreamName, FileData);
aw_rc({error, {full, _}} = Error, StreamName, FileData) ->
    send_alarm(list_to_binary(StreamName)),
    log(aw_rc, Error),
    {{error, ?SA_ERR_NO_RESOURCES}, FileData};
aw_rc({error, no_such_log} = Error, _, FileData) ->
    log(aw_rc, Error),
    {{error, ?SA_ERR_INVALID_PARAM}, FileData};
aw_rc({error, {file_error, _, _}} = Error, _, FileData) ->
    log(aw_rc, Error),
    {{error, ?SA_ERR_NO_RESOURCES}, FileData};
aw_rc({error, Atom} = Error, _, FileData) when is_atom(Atom) ->
    log(aw_rc, Error),
    {Error, FileData};
aw_rc({error, _} = Error, _, FileData) ->
    log(aw_rc, Error),
    {{error, ?SA_ERR_TRY_AGAIN}, FileData}.





%%========================================================================
%% close_log(LogId, StreamName, FilePath, FileName, CfgFileName, Close) ->
%%     {CloseFileName, CloseTime}
%%
%% Close = true  - if logStreamClose was called
%%         false - if the close is caused by an invocation of logFinalize
%%                 without preceding logStreamClose
%%
%% Environment variable log_close defines if logFinalize should implicitely
%% call logStreamClose or not.
%%========================================================================
close_log(LogId, StreamName, FilePath, FileName, CfgFileName, Close) ->
    LogStreamClose = safs:get_env(log_close, false),
    {CloseFileName, CloseTime} = close_log_file(Close,
						LogStreamClose,
						LogId,
						StreamName,
						FilePath,
						FileName),
    close_cfg_file(Close, LogStreamClose, CfgFileName, CloseTime),
    {CloseFileName, CloseTime}.


%%========================================================================
%% reopen_file(LogId, StreamName, FilePath, FileName, CfgFileName) ->
%%     {CloseFileName, CloseTime}
%%
%%
%% close the file and reopen it again.
%% This is a special function (not in the standard) to be able to
%% recreate a halt file that is full (without needed to change the Handle)
%%========================================================================
reopen_file(LogId,
	    StreamName,
	    FilePath,
	    FileNameDT,
	    CfgFileName,
	    #safsLogFileCreateAttributes_2{logFileName = FileName} = Attrs,
	    Version) ->
    Close = true,
    LogStreamClose = true,
    {_CloseFileName, CloseTime} = close_log_file(Close,
						 LogStreamClose,
						 LogId,
						 StreamName,
						 FilePath,
						 FileNameDT),
    close_cfg_file(Close, LogStreamClose, CfgFileName, CloseTime),
    NewFileName = FileName ++ "_" ++ get_time(),
    case open_log(FilePath, NewFileName, StreamName, Attrs, Version) of
	{ok, _, NewFileData, _} ->
	    {ok, NewFileData, NewFileName};
	Error ->
	    Error
    end.


%%============================================================
%% finalize without preceding close
%% If LogStreamClose is false do not rename the file
%% only close the file. If the same stream is reopened
%% later, just continue write to the same file.
%%============================================================
close_log_file(false, false, LogId, _StreamName, _FilePath, _FileName) ->
    disk_log:close(LogId),
    {not_closed, not_closed};
close_log_file(_, _, LogId, StreamName, FilePath, FileName) ->
    p(user, "$$$ disk_log:close ~p ~n", [LogId]),
    disk_log:close(LogId),
    p(user, "### name ~p ~p~n", [?MODULE, FileName]),
    p(user, "### path ~p ~p~n", [?MODULE, FilePath]),

    RootDir   = safs:get_env(log_root, "."),
    CloseTime = get_time(),

    %%------------------------------------------------
    %% close log file
    %%------------------------------------------------
    CloseFileName = FileName ++ "_" ++ CloseTime,
    OldFile = join([RootDir, FilePath, FileName ++ ".log"]),
    NewFile = join([RootDir, FilePath, CloseFileName ++ ".log"]),
    Res = file:rename(OldFile, NewFile),
    p(user, "$$$ file:rename log ~p ~p ~p  ~n", [OldFile, NewFile, Res]),
    p(user, "CLOSED Stream ~p ~p Res = ~p~n",
	      [StreamName, CloseFileName, Res]),
    {CloseFileName, CloseTime}.


%%============================================================
%% finalize without preceding close
%% If LogStreamClose is false do not rename the file.
%%============================================================
close_cfg_file(false, false, _CfgFileName, _CloseTime) ->
    ok;
close_cfg_file(_, _, CfgFileName, CloseTime) ->
    %%------------------------------------------------
    %% close cfg file
    %%------------------------------------------------
    OldCfgFile = CfgFileName ++ ".cfg",
    NewCfgFile = CfgFileName ++ "_" ++ CloseTime ++ ".cfg",
    CfgRes = file:rename(OldCfgFile, NewCfgFile),
    p(user, "$$$ file:rename cfg ~p ~p ~p  ~n",
      [OldCfgFile, NewCfgFile, CfgRes]).


%%========================================================================
%% delete_files(FileName, FilePathName) -> ok
%%
%% This function deletes all files with FileName in the
%% requested directory.
%%========================================================================
delete_files(FileName, FilePathName) ->
    RootDir       = safs:get_env(log_root, ?DEFAULT_ROOT),
    AbsDirName    = join([RootDir, FilePathName]),
    WildCardFiles = filelib:wildcard(join([AbsDirName, FileName ++ "*"])),
    DelFiles = dlf_files([string:tokens(F, "/") || F <- WildCardFiles], []),
    [file:delete(join([AbsDirName, F])) || F <- DelFiles],
    ok.


dlf_files([], Files) ->
    dlf_names([string:tokens(F, "_.") || F <- Files], Files, []);
dlf_files([F| T], Files) ->
    [Name | _] = lists:reverse(F),
    dlf_files(T, [Name | Files]).


dlf_names([], _, Acc) ->
    Acc;
dlf_names([[_, OD, OT, Ext] | T], [F | Files], Acc)
  when (Ext == "log" orelse Ext == "cfg") andalso
        length(OD) == 8 andalso
        length(OT) == 6 ->
    dlf_names(T, Files, [F | Acc]);
dlf_names([[_, OD, OT, CD, CT, Ext] | T], [F | Files], Acc)
  when (Ext == "log" orelse Ext == "cfg") andalso
        length(OD) == 8 andalso
        length(OT) == 6 andalso
        length(CD) == 8 andalso
        length(CT) == 6 ->
    dlf_names(T, Files, [F | Acc]);
dlf_names([_|T], [_|F], Acc) ->
    dlf_names(T, F, Acc).




%%========================================================================
%% get_cfg_params(StreamName) ->
%%     {#safsLogFileCreateAttributes_2{}, #safsLogSeverityFlags{}}
%%
%%
%%========================================================================
get_cfg_params(StreamName) ->
    ImmCfg = safs_log_oi_cfg:get_imm_cfg(StreamName),
    FileName = proplists:get_value(logFileName, ImmCfg),
    PathName = proplists:get_value(logFilePathName, ImmCfg, ?DEFAULT_ROOT),
    MaxFileSize = proplists:get_value(maxLogFileSize, ImmCfg),
    MaxRecSize = proplists:get_value(maxLogRecordSize, ImmCfg),
    FullAction = proplists:get_value(logFileFullAction, ImmCfg),
    MaxRotated = proplists:get_value(maxFilesRotated, ImmCfg),
    Format = proplists:get_value(logFileFmt, ImmCfg, default_fmt(StreamName)),
    Filter = proplists:get_value(severityFilter, ImmCfg),
    HAProp = StreamName /= ?LOG_SYSTEM,
    {#safsLogFileCreateAttributes_2{logFileName       = FileName,
				    logFilePathName   = PathName,
				    maxLogFileSize    = MaxFileSize,
				    maxLogRecordSize  = MaxRecSize,
				    haProperty        = HAProp,
				    logFileFullAction = FullAction,
				    maxFilesRotated   = MaxRotated,
				    logFileFmt        = Format},
     Filter}.

%%======================================================================
%% Internal Interface Functions
%%======================================================================

%%========================================================================
%% get_open_files_info(FileName, AbsDirName) -> Result
%%
%% Result           = {IsOpenFile, FileNameDT, ClosedFiles, AbsCfgFileNameDT}
%% IsOpenFile       = boolean()
%% FileNameDT       = string()
%% AbsCfgFileNameDT = string()
%% ClosedFiles      = [AbsLogFile]
%% AbsLogFile       = string()
%% AbsDirName       = string()
%%
%% Get information about any possible non-closed logs for this stream.
%% In case of an open file is found collect all information about the files,
%% incl the preceding closed files.
%%
%%========================================================================
get_open_files_info(FileName, AbsDirName) ->
    %%-----------------------------------------------------------------
    %% find all log files for the specified stream in the directory
    %% and sort them in decreasing time order.
    %%-----------------------------------------------------------------
    AllFiles = gofi_find_all_files(FileName, AbsDirName),
    FF = [F || F <- AllFiles, string:str(F, ".log") > 0],
    FoundFiles = lists:reverse(lists:sort(FF)),

    %%-----------------------------------------------------------------
    %% find the cfg file for the specified stream
    %%-----------------------------------------------------------------
    AllCfgFiles = lists:sort([F || F <- AllFiles, string:str(F, ".cfg") > 0]),
    AbsCfgFileNameDT = gofi_get_cfg_file_name(lists:reverse(AllCfgFiles)),

    %%-----------------------------------------------------------------
    %% find file info and all closed files associated with the stream
    %%-----------------------------------------------------------------
    {FileInfo, ClosedFiles} = gofi_file_info(FoundFiles, FileName),
    gofi_rc(FileInfo, AbsCfgFileNameDT, ClosedFiles).


gofi_rc({false, FileNameDT, _}, AbsCfgFileNameDT, ClosedFiles) ->
    {false, FileNameDT, ClosedFiles, AbsCfgFileNameDT};
gofi_rc({true, FileNameDT, _}, AbsCfgFileNameDT, ClosedFiles) ->
    {true, FileNameDT, ClosedFiles, AbsCfgFileNameDT}.





%%================================================================
%% calculate a wildcarded path to find all existing log files
%% for the stream.
%%================================================================
gofi_find_all_files(FileName, AbsDirName) ->
    filelib:wildcard(join([AbsDirName, FileName ++ "*"])).



%%================================================================
%% find the cfg file name, if there is any.
%% Take the newest if there are several.
%%================================================================
gofi_get_cfg_file_name([]) ->
    [];
gofi_get_cfg_file_name([CfgFile | _]) ->
    EndPos = string:rstr(CfgFile, ".cfg"),
    string:substr(CfgFile, 1, EndPos - 1).


%%========================================================================
%% gofi_file_info(FoundFiles, FileName) -> {FileInfo, ClosedFiles}
%%
%% FoundFiles  = [AbsFile]
%% FileName    = string()
%% FileInfo    = {IsOpen, FileNameDT, DateTime}
%% IsOpen      = boolean()
%% FileNameDT  = string()
%% DateTime    = [OpenDate, OpenTime] |
%%               [OpenDate, OpenTime, CloseDate, CloseTime]]
%% ClosedFiles = [AbsFile]
%%
%%
%%
%%========================================================================
%%-----------------------------------------------------------------
%% No files found
%%-----------------------------------------------------------------
gofi_file_info([], _) ->
    {{false, undefined, []}, []};
%%-----------------------------------------------------------------
%% Log files for this stream is found,
%% check if the newest file is not closed
%%-----------------------------------------------------------------
gofi_file_info([AbsLogFile | _] = LogFiles, FileName) ->
    gofi_fi(is_log_open(AbsLogFile, FileName), LogFiles, FileName).


gofi_fi({true, _, [Date, Time]} = LogInfo, LogFiles, FileName) ->
    DateTime = Date ++ "__" ++ Time,
    ClosedFiles = gofi_get_closed_files(LogFiles, DateTime, FileName),
    {LogInfo, ClosedFiles};
gofi_fi(LogInfo, _, _) ->
    {LogInfo, []}.


%%================================================================
%% gofi_get_closed_files(LogFiles, DateTime, FileName) -> ClosedFiles
%%
%%================================================================
gofi_get_closed_files(LogFiles, _DateTime, _FileName) ->
    gofi_get_closed_files(LogFiles, _DateTime, _FileName, []).

gofi_get_closed_files([], _DateTime, _FileName, Res) ->
    lists:reverse(Res);
gofi_get_closed_files([AbsLogFile | LogFiles], DateTime, FileName, Res) ->
    case {string:rstr(AbsLogFile, DateTime ++ ".log"),
	  is_log_open(AbsLogFile, FileName)} of
	{0, _} ->
	    %% Not the current file
	    gofi_get_closed_files(LogFiles, DateTime, FileName, Res);
	{_, {true, _, _}} ->
	    %% Log is open, the next log file should have
	    %% DateTime as close time
	    gofi_get_closed_files(LogFiles, DateTime, FileName, Res);
	{_, {false, _, [DO, TO, _, _]}} ->
	    %% This log file is closed having DateTime as close time
	    DoTo = DO ++ "__" ++ TO,
	    gofi_get_closed_files(LogFiles, DoTo, FileName, [AbsLogFile | Res])
    end.



%%========================================================================
%% is_log_open(AbsLogFile, FileName) -> {IsOpen, FileNameDT, DateTime}
%%
%%
%%
%%
%%
%%========================================================================
is_log_open(AbsLogFile, FileName) ->
    FileNameDT = get_file_name(AbsLogFile),
    Time = string:substr(FileNameDT, length(FileName)+1),
    DateTime = string:tokens(Time, "_"),
    choose(length(DateTime) == 2,
	   {true, FileNameDT, DateTime},
	   {false, undefined, DateTime}).


%%========================================================================
%% get_file_name(AbsLogFile) -> FileNameDT
%%
%%
%%========================================================================
get_file_name(AbsLogFile) ->
    EndPos       = string:rstr(AbsLogFile, ".log"),
    AbsLogFileDT = string:substr(AbsLogFile, 1, EndPos-1),
    Tokens       = string:tokens(AbsLogFileDT, "/"),
    [FileNameDT | _] = lists:reverse(Tokens),
    FileNameDT.


default_fmt(?LOG_ALARM) -> ?LOG_ALARM_FORMAT;
default_fmt(?LOG_NOTIFY) -> ?LOG_NOTIFY_FORMAT;
default_fmt(?LOG_SYSTEM) -> ?LOG_SYSTEM_FORMAT;
default_fmt(_) -> ?LOG_APP_FORMAT.

%%======================================================================
%% rotate(StreamName, FileData) -> {retry, FileData} | {error, Reason}
%%
%% Get current time in a string format
%%======================================================================
rotate(StreamName,
       #file_data{disk_log_id   = DiskLogId,
		  max_file_size = FileMaxSize,
		  name          = FileName,
		  path          = FilePath,
		  file_names    = [CurrName | _]} = FileData) ->
    {CloseName, CloseTime} = close_log_file(true, %% forces renaming of the file
					    true,
					    DiskLogId,
					    StreamName,
					    FilePath,
					    CurrName),
    RootDir  = safs:get_env(log_root, "."),
    LongName = FileName ++ "_" ++ CloseTime,
    LogFile  = join([RootDir, FilePath, LongName ++ ".log"]),
    OpenOpts = [{name, StreamName},
		{file, LogFile},
		{size, FileMaxSize}] ++ ?FIXED_LOG_OPTS,
    p(user, "$$$ disk_log:open rotate ~p ~n", [OpenOpts]),
    rotate2(disk_log:open(OpenOpts),
	    r_update_fd(FileData,
			CloseName,
			LongName)).

rotate2({ok, DiskLogId}, FileData) ->
    {retry, FileData#file_data{disk_log_id = DiskLogId}};
rotate2(Error, FileData) ->
    log(aw_r, Error),
    {{error, ?SA_ERR_NO_RESOURCES}, FileData}.


%%-----------------------------------------------------------------
%% The previous file is closed and a new file is opened.
%% The first element of file_names contains the old file name
%% when it was open (i.e. the file name without close time)
%% That name is swapped to the close file name.
%%-----------------------------------------------------------------
%% This is a special case when there is only one rotating file.
%% Delete all existing files (incl CloseName) and create a new
r_update_fd(#file_data{max_rotated = 1,
		       path        = FilePath,
		       file_names  = [_ | Names]} = FileData,
	    CloseName,
	    LogFile) ->
    RootDir     = safs:get_env(log_root, "."),
    RemoveFiles = [join([RootDir, FilePath, DelFile ++ ".log"])
		   || DelFile <- [CloseName | Names]],
    p(user, "$$$ file:delete  ~p~n", [RemoveFiles]),
    [file:delete(RmFile) || RmFile <- RemoveFiles],
    FileData#file_data{file_names = [LogFile]};
%% Less than max number of files
r_update_fd(#file_data{max_rotated = MaxRotated,
		       file_names  = [_ | T] = Names} = FileData,
	    CloseName,
	    LogFile) when length(Names) < MaxRotated ->
    p(user, "$$$ file:delete: no files deleted~n", []),
    FileData#file_data{file_names = [LogFile, CloseName | T]};
%% More than max number of files, delete superfluose files and create a new
r_update_fd(#file_data{max_rotated = MaxRotated,
		       path        = FilePath,
		       file_names  = FileNames} = FileData,
	    CloseName,
	    LogFile) ->
    {[_ | KeepFiles], DelFiles} = lists:split(MaxRotated - 1, FileNames),
    RootDir     = safs:get_env(log_root, "."),
    RemoveFiles = [join([RootDir, FilePath, DelFile ++ ".log"]) || DelFile <- DelFiles],
    p(user, "$$$ file:delete  ~p~n", [RemoveFiles]),
    [file:delete(RmFile) || RmFile <- RemoveFiles],
    FileData#file_data{file_names = [LogFile, CloseName | KeepFiles]}.


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


%%======================================================================
%% get_def_cfg() -> [{Key, Val}]
%%
%% Get the default configuration.
%% If the user has not defined any configuration,
%% internal default values are used.
%%======================================================================

%% get_def_cfg() ->
%%     DefCnfFile = safs:get_env(log_cfg, ""),
%%     p(user, "$$$ file:consult ~p ~n", [DefCnfFile]),
%%     gdc(file:consult(DefCnfFile)).


%% gdc({ok, LogCnf}) -> LogCnf;
%% gdc(_)            -> ?LOG_DEF_CNF.




%%======================================================================
%% get_file_data(StreamName, AllFiles, AbsCfgFileNameDT, Attrs)
%%
%% StreamName = string()
%% AllFiles   = [string()] - [NotClosedFile | ClosedFiles] incl path and
%%                                                         file extention
%%
%% Get the file data for an reopened stream.
%% A stream is reopened when a non-closed log file is found in the dir.
%%======================================================================
get_file_data(_StreamName,
	      AllFiles,
	      AbsCfgFileNameDT,
	      #safsLogFileCreateAttributes_2{logFileName       = FileName,
					     logFilePathName   = FilePath,
					     logFileFullAction = FullAction,
					     maxLogFileSize    = MaxFileSize,
					     maxLogRecordSize  = MaxRecSize,
					     maxFilesRotated   = MaxRotated,
					     logFileFmt        = FileFormat
					    } = _Attrs) ->

    #file_data{file_names    = AllFiles,
	       path          = FilePath,
	       name          = FileName,
	       type          = FullAction,
	       rec_format    = FileFormat,
	       max_file_size = MaxFileSize,
	       max_rec_size  = MaxRecSize,
	       max_rotated   = MaxRotated,
	       cfg_file      = AbsCfgFileNameDT}.

get_cfg_file(_StreamName,
	      #safsLogFileCreateAttributes_2{logFileFullAction = FullAction,
					     maxFilesRotated   = MaxRotated,
					     maxLogFileSize    = MaxFileSize,
					     maxLogRecordSize  = MaxRecSize,
					     logFileFmt        = FileFormat
					    } = _Attrs,
	     #safsVersion{releaseCode  = RC,
			  majorVersion = Major,
			  minorVersion = Minor} = Version) ->

    [Version | _] = ?SUPPORTED_LOG_VERSIONS,
    Vsn = integer_to_list(RC) ++ "." ++
	integer_to_list(Major) ++ "." ++
	integer_to_list(Minor),

    LFA = case FullAction of
	      ?LOG_WRAP   -> "WRAP";
	      ?LOG_HALT   -> "HALT";
	      ?LOG_ROTATE -> "ROTATE " ++ integer_to_list(MaxRotated)
	  end,

    "LOG_SVC_VERSION: "    ++ Vsn ++ "\n" ++
	"FORMAT: "             ++ FileFormat ++ "\n" ++
	"MAX_FILE_SIZE: "      ++ integer_to_list(MaxFileSize) ++ "\n" ++
	"FIXED_LOG_REC_SIZE: " ++ integer_to_list(MaxRecSize) ++ "\n" ++
	"LOG_FULL_ACTION: "    ++ LFA ++ "\n".

%%======================================================================
%% get_seq_no(AbsDirName, FileNameDT, Fmt) -> integer()
%%
%% read the oldest sequence number used and increment the value with one
%% to get the next sequence number to be used.
%% Incase of errors start from 1 again.
%%======================================================================
get_seq_no(AbsDirName, FileNameDT, Fmt) ->
    OpenFile = join([AbsDirName, FileNameDT ++ ".log"]),
    p(user, "$$$ file:open ~p ~n", [OpenFile]),
    IoDev    = file:open(OpenFile, [read]),
    gsn(gsn_last_line(IoDev), Fmt).


gsn(undefined, _Fmt) ->
    1;
gsn(LastLine, Fmt) ->
    gsn_rc(safs_log_rec:get_token_value("@Cr", Fmt, LastLine)).


gsn_rc({ok, ?MAX_SEQUENCE_NO}) ->
    1;
gsn_rc({ok, Result}) ->
    try
	list_to_integer(string:strip(Result)) + 1
    catch error:badarg ->
	    1
    end;
gsn_rc(_) ->
    1.



gsn_last_line({ok, IoDev}) ->
    gsn_ll(file:read_line(IoDev), IoDev, undefined);
gsn_last_line(_Error) ->
    undefined.


gsn_ll(eof, _IoDev, LastLine) ->
    LastLine;
gsn_ll({ok, Line}, IoDev, _) ->
    gsn_ll(file:read_line(IoDev), IoDev, Line);
gsn_ll(_Error, _IoDev, _) ->
    undefined.




find_fmt(AbsCfgFileNameDT) ->
    find_fmt_2(file:read_file(AbsCfgFileNameDT ++ ".cfg")).

find_fmt_2({ok, CfgFile}) ->
    Tokens = string:tokens(binary_to_list(CfgFile), "\n"),
    findf(Tokens);
find_fmt_2(Error) ->
    Error.

findf([]) ->
    {error, format_not_found};
findf(["FORMAT: " ++ Fmt | _]) ->
    Fmt;
findf([_|T]) ->
    findf(T).



%%======================================================================
%% send_alarm() -> ok | {error, Reason}
%%
%% send alarm because a halt file is full.
%%======================================================================
send_alarm(StreamName) ->
    ClassId = #safsNtfClassId{vendorId = ?SA_NTF_VENDOR_ID_SAF,
			      majorId  = 8,
			      minorId  = 0},

    NotObj = <<"safApp=safLogService">>,
    AddTxt = list_to_binary(binary_to_list(StreamName) ++
			    " has reached full capatcity"),
    Head = #safsNtfNotificationHeader{eventType = sa_ntf_object_operation,
				      notificationObject      = NotObj,
				      notificationClassId     = ClassId,
				      correlatedNotifications = [],
				      additionalText          = AddTxt
				     },

    Alarm = #safsNtfAlarmNotification{notificationHeader = Head,
				      probableCause      = sa_ntf_file_error,
				      perceivedSeverity  = sa_ntf_severity_warning},

    CB = #safsNtfCallbacks{saNtfNotificationCallback          = false,
			   saNtfNotificationDiscardedCallback = false},
    {ok, Handle, _} = safs_ntf:initialize(CB),

    safs_ntf:notification_send(Handle, Alarm),
    safs_ntf:finalize(Handle).




%%======================================================================
%% Misc Functions
%%======================================================================

join(List) -> filename:join(List).

choose(true,  T, _) -> T;
choose(false, _, F) -> F.

log(Fnc, Error) ->
    p(user, "### ERROR. ~p:~p ~p~n", [?MODULE, Fnc, Error]).





p(_, _, _) ->
    ok.

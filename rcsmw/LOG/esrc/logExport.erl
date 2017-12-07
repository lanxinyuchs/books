%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logExport.erl %
%%% Author:	etxjotj
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(logExport).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R10A/1').
-date('2017-04-12').
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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-02-23 etxjotj     Created
%%% R2A/5      2013-06-07 etxjotj     Added printout for nxdomain
%%% R2A/9      2013-11-07 uabesvi     transfer availability log
%%% R2A/11     2013-12-12 uabesvi     changed LogM to RcsLogM
%%% R2A/16     2014-09-04 etxlg       set DSCP, TR HS73150
%%% R2A/17     2014-09-22 etxpejn     Added call to sysSftp TR HS89846
%%% R3A/1      2014-10-29 etxpeno     HT16841 add check of URI
%%% R4A/2      2015-07-21 etxjotj     HT93781 Erlang term in progress report
%%% R4A/5      2015-11-12 etxtory     Additional info back; new solution to R4A/4
%%% R5A/1      2016-04-01 uabesvi     
%%% -----      ---------  --------    ------------------------
%%% R8A/1      2016-11-24 uabesvi     Encrypted logs
%%% R8A/3      2016-12-29 uabesvi     Fixed bulk push log (sort error)
%%% R8A/4      2017-01-16 eivmiha     switched ssh_sftp and sysSftp to ftpI
%%% ----------------------------------------------------------
%%% R9A/1      2017-01-31 emarnek     Added support for ftpes
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([export_log/5, export_part/4]).
-export([transfer_avli/3]).
-export([update_progress/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("log.hrl").
-include("RcsLogM.hrl").
-include_lib("kernel/include/file.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

export_log(Name, Url, Password, ActionId, LocalDir) ->
    export_log(Name, Url, Password, ActionId, LocalDir, {"1","1","1",Name}).

export_log(Name, Url, Password, ActionId, LocalDir, LogId) ->
    try do_export_log(Name, Url, Password, ActionId, LocalDir, LogId) of
	ok ->
	    logServer:log_exported(Name),
	    ok
    catch
	throw:incorrect_uri ->
	    ProgressInfo = "Failed to parse the URI",
	    handle_fail(LogId, ProgressInfo);
	throw:empty->
	    ProgressInfo = "No file exported",
	    handle_fail_empty(LogId, ProgressInfo);
	throw:nothing_exported ->
	    ProgressInfo = "The action could not be completed",
	    handle_fail(LogId, ProgressInfo);
	throw:_ ->
	    ProgressInfo = "The action could not be completed",
	    handle_fail(LogId, ProgressInfo);
	Type:Reason ->
	    ProgressInfo = "A software related error occured",
	    sysInitI:error_report([{Type, Reason},
				       erlang:get_stacktrace()]),
	    handle_fail(LogId, ProgressInfo)
    end,
    ok.

export_part(Name, Url, Password, ActionId) ->
    LogId = {"1","1","1",Name},
    try do_export_part(Name, Url, Password, ActionId, LogId) of
	ok -> ok
    catch
	throw:_ ->
	    ProgressInfo = "The action could not be completed",
	    handle_fail(LogId, ProgressInfo);
	Type:Reason ->
	    ProgressInfo = "A software related error occured",
	    sysInitI:error_report([{Type, Reason},
				       erlang:get_stacktrace()]),
	    handle_fail(LogId, ProgressInfo)
    end,
    ok.

transfer_avli(Url, Password, ActionId) ->
    LogId = {"1","1","1"},
    StartTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(LogId, [{actionName, "TransferAvailibilityLog"},
			    {additionalInfoClear, "Exporting AVLI"},
			    {progressInfo, ""},
			    {progressPercentage, 0},
			    {result, ?ActionResultType_NOT_AVAILABLE},
			    {resultInfo, ""},
			    {state, ?ActionStateType_RUNNING},
			    {actionId, ActionId},
			    {timeActionStarted, StartTime}]),

    {ok, {Dir, Name}} = alhI:generate_log(),
    export_log(Name, Url, Password, ActionId, Dir, LogId).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

do_export_log(Name, Url, Passwd, ActionId, LocalDir, LogId) ->
    %% Intiate progress report
    ActionName = choose(size(LogId) == 4,
			"Export",
			"TransferAvailibilityLog"),
    StartTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(LogId, [{actionName, ActionName},
			    {additionalInfoClear, "Exporting "++Name},
			    {progressInfo, ""},
			    {progressPercentage, 0},
			    {result, ?ActionResultType_NOT_AVAILABLE},
			    {resultInfo, ""},
			    {state, ?ActionStateType_RUNNING},
			    {actionId, ActionId},
			    {timeActionStarted, StartTime}]),

    %% Find out where the file should go
    {Proto, User, Host, Port, RemoteDir} = parse_uri(ftpI:parse_uri(Url)),
    put(protocol, Proto),

    %% Get hold of the log files and transfer them
    %% LocalDir = logServer:log_dir(Name),
    case file:list_dir(LocalDir) of
	{ok, []} ->
	    ok;
	{ok, _Files} when size(LogId) == 3 ->
	    %% Get file information objects
	    TempFIOs = collect_file_info(LocalDir, [Name]),
	    FIOs = [FI || {FN, _, _, _} = FI <- TempFIOs, FN == Name],
	    TotalSize = lists:foldl(fun(X, A) -> A + element(3, X) end,
				    0, FIOs),
	    SortedFIOs = lists:keysort(4, FIOs),
	    do_export_log_2(Proto, Name, {User, Host, Port, RemoteDir}, 
			    Passwd,
			    TotalSize, SortedFIOs, LogId);
	{ok, Files} ->
	    %% Get file information objects
	    FIOs = collect_file_info(LocalDir, Files),
	    TotalSize = lists:foldl(fun(X, A) -> A+element(3,X) end,
				    0, FIOs),
	    SortedFIOs = lists:keysort(4, FIOs),
	    do_export_log_2(Proto, Name, {User, Host, Port, RemoteDir}, Passwd,
			    TotalSize, SortedFIOs, LogId)
    end,

    %% Cleanup
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(LogId, [{result, ?ActionResultType_SUCCESS},
			    {resultInfo, ""},
			    {progressPercentage, 100},
			    {state, ?ActionStateType_FINISHED},
			    {timeActionCompleted, CompleteTime}]),
    ok.

do_export_part(Name, Url, Passwd, ActionId, LogId) ->
    %% Intiate progress report
    StartTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(LogId, [{actionName, "Push transfer"},
			    {additionalInfoClear, "Exporting "++Name},
			    {progressInfo, ""},
			    {progressPercentage, 0},
			    {result, ?ActionResultType_NOT_AVAILABLE},
			    {resultInfo, ""},
			    {state, ?ActionStateType_RUNNING},
			    {actionId, ActionId},
			    {timeActionStarted, StartTime}]),

    %% Find out where the file should go
    {Proto, User, Host, Port, RemoteDir} = parse_uri(ftpI:parse_uri(Url)),
    put(protocol, Proto),

    %% Get hold of the log files and transfer them
    LocalDir  = logServer:log_dir(Name),
    [DataObj] = logDb:log_data_get_dirty({"1","1","1",Name}),

    case file:list_dir(LocalDir) of
	{ok, []} ->
	    ok;
	{ok, Files} when is_integer(DataObj#logData.rotatingSegments)->
	    %% Get file information objects
	    FIOs = collect_file_info(LocalDir, Files),
	    %% Get the lasted completed file
	    FIO = dep_sort(Name,
			   lists:reverse(lists:keysort(4, FIOs)),
			   disk_log:info(Name)),
	    TotalSize = element(3, FIO),
	    do_export_log_2(Proto, Name, {User, Host, Port, RemoteDir}, Passwd,
			    TotalSize, [FIO], LogId);
	{ok, Files} when DataObj#logData.rotatingSegments == undefined ->
	    %% Get file information objects
	    FIOs = collect_file_info(LocalDir, Files),
	    %% Get the lasted completed file
	    %% This relies on that there is no garbage in the directory
	    FIO = hd(FIOs),
	    TotalSize = element(3, FIO),
	    do_export_log_2(Proto, Name, {User, Host, Port, RemoteDir}, Passwd,
			    TotalSize, [FIO], LogId)
    end,

    %% Cleanup
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(LogId, [{result, ?ActionResultType_SUCCESS},
			    {resultInfo, ""},
			    {progressPercentage, 100},
			    {state, ?ActionStateType_FINISHED},
			    {timeActionCompleted, CompleteTime}]),
    ok.

parse_uri({ok, {sftp, User, Host, Port, RemoteDir, _Query}}) ->
    {sftp, User, Host, Port, RemoteDir};
parse_uri({ok, {ftpes, User, Host, Port, RemoteDir, _Query}}) ->
    {ftpes, User, Host, Port, RemoteDir};
parse_uri(_) ->
    throw(incorrect_uri).

%% Find the last full file 
dep_sort(Name, Files, Info) ->
    Current  = proplists:get_value(current_file, Info),
    {_, Max} = proplists:get_value(size,         Info),
    Index    = integer_to_list(choose(Current == 1, Max, Current - 1)),
    {value, File, _} = lists:keytake(Name ++ "." ++ Index, 1, Files),
    File.


do_export_log_2(Proto, Name, {User, Host, Port, RemoteDir}, Passwd, TotalSize,
		SortedFIOs, LogId) ->
    %% Figure out file name myLogFile_Createtime_Closetime where date
    %% format is yyyymmdd_hhmmss. But all log files are considered
    %% open, so there will not be a close time
    FirstPath  = element(2, hd(SortedFIOs)),
%%     RemotePath = get_remote_path(Name, FirstPath, RemoteDir, LogId),
%%     update_progress(LogId, [{additionalInfo, "Remote file is " ++ RemotePath}]),

    %% OK, so lets start the transfer
    %% First open the ssh connection and sftp/ftpes channel
    {ok, Pid, CRef} = start_channel(Proto, Name, Host, Port, User, Passwd, LogId),
    put(channelPid, Pid),
    put(connectionRef, CRef),

    do_export_log_3(get_log_type(FirstPath),
            Proto,
		    Name,
		    TotalSize,
		    SortedFIOs,
		    LogId,
		    RemoteDir,
		    Pid,
		    []),

    ftpI:stop_channel(Proto, Pid, CRef).

do_export_log_3(saf_log, _Proto, _Name, _TotSz, [], LogId, _RemDir, _Pid, Progresses) ->
    OrderProgs = lists:reverse(Progresses),
    update_progress(LogId, OrderProgs),
    %% An extra gc to be on the safe side
    garbage_collect(),
    ok;
do_export_log_3(saf_log,
        Proto, 
		Name, 
		TotalSize, 
		[FIO | T], 
		LogId, 
		RemoteDir, 
		Pid, 
		Progresses) ->
    FirstPath  = element(2, FIO),
    RemotePath = get_remote_path(Name, FirstPath, RemoteDir, LogId),
    Progress = do_export_log_4(Name, Proto, TotalSize, [FIO], LogId, Pid, RemotePath),
    do_export_log_3(saf_log,
            Proto, 
		    Name, 
		    TotalSize, 
		    T, 
		    LogId, 
		    RemoteDir, 
		    Pid, 
		    [Progress | Progresses]);
do_export_log_3(_, Proto, Name, TotalSize, SortedFIOs, LogId, RemoteDir, Pid, _Prog) ->
    FirstPath  = element(2, hd(SortedFIOs)),
    RemotePath = get_remote_path(Name, FirstPath, RemoteDir, LogId),
    Progress = do_export_log_4(Name,
                   Proto, 
			       TotalSize, 
			       SortedFIOs, 
			       LogId, 
			       Pid, 
			       RemotePath),
    update_progress(LogId, [Progress]).

do_export_log_4(Name, Proto, TotalSize, SortedFIOs, LogId, Pid, RemotePath) ->
    %% HU35067 
    %% Don't use update_progress with additionalInfo here since it
    %% will build up memory if there are a lot of files.
    %% 
    %% Open the remote file
    {ok, Handle} = open_remote_file(Proto, LogId, Pid, RemotePath),
    put(handle, Handle),

    Res = upload_files(undefined, Pid, Handle, Name, 0, TotalSize, SortedFIOs, LogId),

    %% Cleanup
    ftpI:close(Proto, Pid, Handle),
    case Res of
	ok  -> 
	    {additionalInfo, "Remote file is " ++ RemotePath};
	_ -> 
	    Progress = {additionalInfo, "No files were exported."},
	    update_progress(LogId, [Progress]),
	    throw(nothing_exported)
    end.

get_log_type(FirstPath) ->
    glt(lists:reverse(string:tokens(FirstPath, "."))).

glt(["cfg" | _T]) -> saf_log;
glt(["log" | _T]) -> saf_log;
glt(["gz"  | _T]) -> avli;
glt(_)            -> logm.

get_remote_path(Name, FirstPath, RemoteDir, LogId) ->
    grp(lists:reverse(string:tokens(FirstPath, "./")),
	Name,
	FirstPath,
	RemoteDir,
	LogId).

%% Application SAF log configuration file
grp(["cfg" | _], _RemoteName, FirstPath, RemoteDir, _LogId) ->
    [FileName | _] = lists:reverse(string:tokens(FirstPath, "/")),
    filename:join(RemoteDir, FileName);
%% Application SAF log file
grp(["log" | _], _RemoteName, FirstPath, RemoteDir, _LogId) ->
    [FileName | _] = lists:reverse(string:tokens(FirstPath, "/")),
    filename:join(RemoteDir, FileName);
%% Application AVLI log
grp(["gz", "xml", FileName | _], _Name, _FirstPath, RemoteDir, _LogId) ->
    RemoteName = lists:append([FileName, "_", get_date_time(), ".xml.gz"]),
    filename:join(RemoteDir, RemoteName);
grp(_, Name, FirstPath, RemoteDir, LogId) ->
    RemoteName = get_remote_name(is_encrypted(Name), Name, FirstPath, LogId),
    filename:join(RemoteDir, RemoteName).

get_date_time() ->
    get_date(date()) ++ get_time(time()).

get_date({Y, M, D}) ->
    LY = integer_to_list(Y),
    LM = integer_to_list(M),
    LD = integer_to_list(D),
    LY ++ choose(M > 9, LM, "0" ++ LM) ++ choose(D > 9, LD, "0" ++ LD).

get_time({H, M, S}) ->
    LH = integer_to_list(H),
    LM = integer_to_list(M),
    LS = integer_to_list(S),
    choose(H > 9, LH, "0" ++ LH) ++
	choose(M > 9, LM, "0" ++ LM) ++
	choose(S > 9, LS, "0" ++ LS).

collect_file_info(LocalDir, [File|Files]) ->
    case filename:extension(File) of
	".siz" -> collect_file_info(LocalDir, Files); % OTP disk_log internal
	".idx" -> collect_file_info(LocalDir, Files); % files are not exported
	_ ->
	    Path = filename:join(LocalDir, File),
	    case file:read_file_info(Path) of
		{ok, FI} ->
		    [{File, Path, FI#file_info.size, FI#file_info.mtime}|
		     collect_file_info(LocalDir, Files)];
		{error, Reason} ->
		    sysInitI:error_report(
		      [{mfa, {file, read_file_info, [Path]}},
		       {?MODULE, ?LINE},
	       {error, Reason}]),
		    collect_file_info(LocalDir, Files)
	    end
    end;
collect_file_info(_, []) ->
    [].


%% Non encrypted log
get_remote_name(false, Name, FirstPath, LogId) ->
    Date = grn_open(file:open(FirstPath, [read]),
		    FirstPath,
		    LogId),
    Name ++ "_" ++ Date;
%% Encrypted log. Assumes that it is a disk_log, i.e. ending with .<int>
get_remote_name(true, Name, FirstPath, LogId) ->
    {ok, {File, No}} = grn_split(string:tokens(FirstPath, ".")),
    Date = grn_open_encrypted(wrap_log_reader:open(File, list_to_integer(No)),
			      FirstPath,
			      LogId),
    Name ++ "_" ++ Date.
    

grn_split([Name, Int]) ->
    {ok, {Name, Int}};
grn_split([Name, Int, "tmp"]) ->
    {ok, {Name, Int}};
grn_split(Error) ->
    {error, Error}.

grn_open({ok, Fd}, FirstPath, LogId) ->
    grn_read_line(file:read_line(Fd), Fd, FirstPath, LogId);
grn_open({error, Eopen}, FirstPath, LogId) ->
    Info = file:format_error(Eopen),
    update_progress(LogId, [{resultInfo, Info}]),
    erlang:error(Eopen, [FirstPath]).


grn_open_encrypted({ok, Cont}, FirstPath, LogId) ->
    grn_oe(wrap_log_reader:chunk(Cont, 1), FirstPath, LogId);
grn_open_encrypted({error, Reason}, FirstPath, LogId) ->
    update_progress(LogId, [{resultInfo, pp(Reason)}]),
    erlang:error(Reason, [FirstPath]).

grn_oe({error, Reason}, FirstPath, LogId) ->
    update_progress(LogId, [{resultInfo, pp(Reason)}]),
    erlang:error(Reason, [FirstPath]);
grn_oe({Cont, Data}, _FirstPath, LogId)
  when Data == eof;
       Data == [] ->
    wrap_log_reader:close(Cont),
    Info = "The log is empty",
    update_progress(LogId, [{resultInfo, Info}]),
    throw(empty);
grn_oe({Cont, _, NoBytes}, _FirstPath, LogId) ->
    wrap_log_reader:close(Cont),
    Info = "The log is corrupt. Number of erroneaous bytes = " ++ 
	integer_to_list(NoBytes),
    update_progress(LogId, [{resultInfo, Info}]),
    throw(empty);
grn_oe({Cont, [EncryptedData]}, _FirstPath, _LogId) ->
    wrap_log_reader:close(Cont),
    grn_oe_decrypt(certI:decrypt_data(EncryptedData)).


grn_oe_decrypt({ok, BinData}) ->
    [_, DateStr|_] = string:tokens(binary_to_list(BinData), " "),
    lists:flatten(string:tokens(DateStr, "-T/:")).



grn_read_line({ok, Data}, Fd, _FirstPath, _LogId) ->
    file:close(Fd),
    [_, DateStr | _] = string:tokens(Data, " "),
    lists:flatten(string:tokens(DateStr, "-T/:"));
grn_read_line(eof, Fd, _FirstPath, LogId) ->
    file:close(Fd),
    Info = "The log is empty",
    update_progress(LogId, [{resultInfo, Info}]),
    throw(empty);
grn_read_line({error, Eread}, Fd, FirstPath, LogId) ->
    file:close(Fd),
    Info = file:format_error(Eread),
    update_progress(LogId, [{resultInfo, Info}]),
    erlang:error(Eread, [FirstPath]).

start_channel(Proto, _Name, Host, Port, User, Passwd, LogId) ->
    case ftpI:start_channel(Proto, Host, Port, User, Passwd) of
	{ok, SP, C} -> {ok, SP, C};
	{error, E1} when is_atom(E1) ->
	    Info1 =
		case E1 of
		    etimedout ->
			"Cannot establish a connection to remote server";
            ehost ->
            "Cannot establish a connection to remote server";
		    nxdomain ->
			"Domain name not found";
            no_tls ->
            "TLS parameters are not valid";
		    _ ->
            ftpI:format_error(Proto, E1)
		end,
	    update_progress(LogId, [{resultInfo, Info1}]),
	    throw(error);
	{error, E1} ->
	    %% HT93781 Don't write random erlang terms in progress
	    Msg = lists:flatten(io_lib:format("~p~n",[E1])),
	    update_progress(LogId, [{resultInfo, Msg}]),
	    throw(error)
    end.

open_remote_file(Proto, LogId, Pid, RemotePath) ->
    case ftpI:open(Proto, Pid, RemotePath, [write]) of
	{ok, Handle} -> {ok, Handle};
	{error, Reason} ->
	    Info = case Reason of
		       no_such_file -> "No such file or directory";
               ehost -> "Cannot establish a connection to remote server";
               reconf_error -> "Client is stopped";
               no_proc -> "Client doesn't exists";
		       _ ->
               ftpI:format_error(Proto, Reason)
		       end,
	    update_progress(LogId, [{resultInfo, Info}]),
	    throw(error)
    end.

%%========================================================================
%% upload_files
%%
%% upload avli and logm files
%%========================================================================
upload_files(UploadRes, 
	     Pid,
	     Handle,
	     Name,
	     AccuSize,
	     TotalSize,
	     [{File, Path, Size, _} | FIOs],
	     LogId) ->
    update_progress(LogId, [{progressInfo, "Uploading " ++ File}]),
    CopyDir = sysEnv:tmp_dir() ++ "/log/export/",
    ok      = filelib:ensure_dir(CopyDir),
    TmpPath = copy_file(is_encrypted(Name), Path, CopyDir, LogId, File),
    ExpRes  = export_file(decrypt_file(is_encrypted(Name), Path, TmpPath),
			  TmpPath,
			  File,
			  Pid,
			  Handle,
			  AccuSize,
			  TotalSize,
			  LogId),
    upload_files(choose(ExpRes == ok, ok, UploadRes),
		 Pid,
		 Handle,
		 Name,
		 AccuSize + Size,
		 TotalSize,
		 FIOs,
		 LogId);
upload_files(UploadRes, _, _, _, _, _, [], _) ->
    UploadRes.

export_file(ok,
	    TmpPath,
	    File,
	    Pid,
	    Handle,
	    AccuSize,
	    TotalSize,
	    LogId) ->
    case file:open(TmpPath, [read, raw, binary]) of
	{ok, Fd} ->
	    upload_file(Fd,
			Pid,
			Handle,
			LogId,
			AccuSize,
			TotalSize,
			file:read(Fd, 65536)),
	    file:close(Fd),
	    file:delete(TmpPath),
	    ok;
	{error, Reason} = Error ->
	    %% Push the rest of the log even if this particular part cannot
	    %% be accessed
	    file:delete(TmpPath),
	    Msg = File ++ " " ++ file:format_error(Reason),
	    update_progress(LogId, [{additionalInfo, Msg}]),
	    Error
    end;
export_file({error, ProgInfo} = Error, 
	    _TmpFile,
	    _File,
	    _Pid,
	    _Handle,
	    _AccuSize,
	    _TotalSize,
	    LogId) ->
    update_progress(LogId, ProgInfo),
    Error.



copy_file(false, FromFile, ToPath, LogId, FileName) ->
    ToFile = filename:join(ToPath, FileName),
    case file:copy(FromFile, ToFile) of
	{ok, _} -> 
	    ok;
	{error, OE} ->
	    OeMsg = FileName ++ " copy error " ++ file:format_error(OE),
	    update_progress(LogId, [{additionalInfo, OeMsg}])
    end,
    ToFile;
copy_file(true, _FromFile, ToPath, _LogId, FileName) ->
    filename:join(ToPath, FileName).
    



%%========================================================================
%% decrypt_file(IsDecrypted, FromFile, ToFile, LogId) -> ok | {error, ProgInfo}
%%
%% Decrypt encrypted files to tmp/log/esi/Log
%% 
%% 
%%========================================================================
decrypt_file(true, FromFile, ToFile) ->
    case logLib:decrypt_file(FromFile, ToFile) of
	ok ->
	    ok;
	{error, {Error, Reason}} -> 
	    ErrorStr  = df_error(Error),
	    ReasonStr = io_lib:format("~p", [Reason]),
	    ProgInfo  = [{additionalInfo, ErrorStr ++ ToFile},
			 {resultInfo,     lists:flatten(ReasonStr)}],
	    {error, ProgInfo}
    end;
decrypt_file(false, _FromFile, _ToFile) ->
    ok.


df_error(file_open) ->
    "Decrypting error. Could not open file ";
df_error(file_write) ->
    "Decrypting error. Write error ";
df_error(wrap_log_reader_open) ->
    "Decrypting error. Could not open file ";
df_error(wrap_log_reader_read) ->
    "Decrypting error. Could not open file ";
df_error(decrypt) ->
    "Decrypting error. Could not open file ";
df_error(Error) ->
    "Decrypting error. " ++ lists:flatten(io_lib:format("~p", [Error])) ++ " ".
    


is_encrypted(Name) ->
    certI:is_encryption_enabled() andalso logLib:is_encrypted(Name).

    


upload_file(Fd, Pid, Handle, LogId, AccuSize, TotalSize, {ok, Data})
  when TotalSize < AccuSize + size(Data) ->
    NewTotalSize = AccuSize + size(Data),
    upload_file(Fd, Pid, Handle, LogId, AccuSize, NewTotalSize, {ok, Data});
upload_file(Fd, Pid, Handle, LogId, AccuSize, TotalSize, {ok, Data}) ->
    garbage_collect(),
    Proto = get(protocol),
    NewAccuSize = AccuSize + size(Data),
    ok = ftpI:write(Proto, Pid, Handle, Data, 10000),
    upload_file(Fd,
		Pid,
		Handle,
		LogId,
		NewAccuSize,
		TotalSize,
		file:read(Fd, 65536));
upload_file(_, _, _, _, _, _, eof) ->
    ok;
upload_file(_, _, _, LogId, _, _, {error, Reason}) ->
    update_progress(LogId, [{resultInfo, file:format_error(Reason)}]),
    erlang:error(Reason).

handle_fail(LogId, ProgressInfo) ->
    Proto = get(protocol),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(LogId,
		    [{result, ?ActionResultType_FAILURE},
		     {progressInfo, ProgressInfo},
		     {state, ?ActionStateType_FINISHED},
		     {timeActionCompleted, CompleteTime}]),
    case get(channelPid) of
	Pid when is_pid(Pid) ->
        cleanup(Proto, Pid);
	undefined -> ok
    end,
    case get(connectionRef) of
	CRef when is_pid(CRef) -> ssh:close(CRef);
	undefined -> ok
    end.

handle_fail_empty(LogId, ProgressInfo) ->
    Proto = get(protocol),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(LogId,
		    [{result, ?ActionResultType_SUCCESS},
		     {progressInfo, ProgressInfo},
		     {state, ?ActionStateType_FINISHED},
		     {timeActionCompleted, CompleteTime}]),
    case get(channelPid) of
    Pid when is_pid(Pid) ->
        cleanup(Proto, Pid);
	undefined -> ok
    end,
    case get(connectionRef) of
	CRef when is_pid(CRef) -> ssh:close(CRef);
	undefined -> ok
    end.

cleanup(ftpes, Pid) ->
    ftpI:close(ftpes, Pid, undefined),
    ftpI:stop_channel(ftpes, Pid);
cleanup(sftp, Pid) ->
    case get(handle) of
        undefined -> ok;
        Handle -> ftpI:close(sftp, Pid, Handle)
    end,
    ftpI:stop_channel(sftp, Pid);
cleanup(_, _) ->
    ok.
%%========================================================================
%% update_progress(LogId, ProgressData) -> {atomic, ok}
%%
%% 
%%========================================================================
update_progress({_, _, _, _} = LogId, ProgressData)->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read({log, LogId}),
	      NewProgress = up_get_progress(Obj#log.progressReport,
					    "Export",
					    ProgressData),
	      mnesia:write(Obj#log{progressReport = NewProgress})
      end);
update_progress(LogId, ProgressData)->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read({logM, LogId}),
	      NewProgress = up_get_progress(Obj#logM.progressReport,
					    "TransferAvailibilityLog",
					    ProgressData),
	      mnesia:write(Obj#logM{progressReport = NewProgress})
      end).


up_get_progress(undefined, Action, ProgressData) ->
    Time = comsaI:iso_time(os:timestamp(), extended),
    Data = [{actionName,             Action},
	    {additionalInfo,         [""]},
	    {progressInfo,           ""},
	    {progressPercentage,     0},
	    {result,                 ?ActionResultType_NOT_AVAILABLE},
	    {resultInfo,             ""},
	    {state,                  ?ActionStateType_RUNNING},
	    {actionId,               0},
	    {timeActionStarted,      Time},
	    {timeOfLastStatusUpdate, Time}],
    comsaI:update_progress(Data ++ ProgressData, undefined);
up_get_progress(Progress, _Action, ProgressData) ->
    comsaI:update_progress(ProgressData, Progress).



choose(true,  T, _) -> T;
choose(false, _, F) -> F.


pp(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).


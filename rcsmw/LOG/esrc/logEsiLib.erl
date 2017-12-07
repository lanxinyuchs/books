%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logEsiLib.erl %
%%% Author:	
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(logEsiLib).
-vsn('/main/R10A/R11A/R12A/2').
-date('2017-11-20').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% Rev        Date       Name      What
%%% -----      ---------  --------  ------------------------
%%% R10A/1-2   2017-06-07 uabesvi   Code moved from logEsi
%%% R12A/1     2017-10-26 uabesvi   result in progress fix   
%%% R12A/2     2017-11-20 uabesvi   Removed tmp dir prefix from RU ESI files   
%%% ----------------------------------------------------------
%%%
%%% ---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ---------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


-export([make_esi_dir/0]).
-export([limit_dirs/3]).
-export([get_size_dirs/2]).

-export([get_chunks/5]).
-export([get_free_tmp/0]).
-export([get_free_tmp/1]).
-export([check_disk_space/0]).

-export([pack_and_compress_dirs/4]).
-export([pack_and_compress_ext_dirs/3]).

-export([print_esi_time_warning/0]).
-export([print_top/1]).
-export([print_tmp/1]).
-export([print_chunks/3]).
-export([print_listing/2]).
-export([print_granularity/2]).

-export([count/2]).

-export([update_progress/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("log.hrl").
-include("RcsLogM.hrl").
-include_lib("kernel/include/file.hrl").

-define(MAX_UPLOAD_TIME, 300000).
-define(MAX_WRITE_TIME,   60000).

-define(MIN_TMP_SIZE_MB, 100).

%% Default ESI callback timeout = 10 minutes
-define(DEFAULT_ESI_CB_TIMEOUT,  600).

%% Print a warning in erlang log after this time in seconds
-define(WARNING_ESI_CB_TIME,  150). 

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


make_esi_dir() ->
    EsiTmpDir = filename:join([sysEnv:rcs_root(), "tmp", "log_esi"]),
    Template  = filename:join(EsiTmpDir, "esi.XXXXX"),
    cmd(["mkdir -p ", EsiTmpDir, " && mktemp -d ", Template])--"\n".



%%========================================================================
%% limit_dirs
%%========================================================================
limit_dirs(_LogFd, Granularity, Dirs) when Granularity =:= undefined;
					   Granularity =:= large ->
    Dirs;
limit_dirs(LogFd, Granularity, Dirs) when Granularity =:= small ->
    do_limit_dirs(LogFd, Dirs, _Acc = []);
limit_dirs(LogFd, Granularity, Dirs) when Granularity =:= static ->
    do_limit_dirs(LogFd, Dirs, _Acc = []);
limit_dirs(_LogFd, _Granularity, Dirs) ->
    Dirs.

do_limit_dirs(LogFd, [Dir | T], Acc) ->
    case filename:basename(Dir) of
	"applicationlogs" ->
	    NewDirs = check_subdirs(Dir, LogFd),
	    do_limit_dirs(LogFd, T, lists:append(NewDirs, Acc));
	"dumps" ->
	    NewDir = check_dir([Dir], LogFd, _Acc =  []),
	    do_limit_dirs(LogFd, T, lists:append(NewDir, Acc));
	"rop" ->
	    NewDir = check_dir([Dir], LogFd, _Acc = []),
	    do_limit_dirs(LogFd, T, lists:append(NewDir, Acc));
	_ ->
	    do_limit_dirs(LogFd, T,  [Dir | Acc])
    end;
do_limit_dirs(_LogFd, [], Acc) ->
    Acc.

check_subdirs(Dir, LogFd) ->
    case get_size(Dir) of
	{ok, Size} when Size > 20000 -> %% 20000kB or 20MB
	    io:format(LogFd,
		      "Directory ~p is too large Size=~pkB; "
		      "trying to exclude subdirs~n",
		      [Dir, Size]),
	    SubDirs = get_sub_dirs(Dir),
	    NewSubDirs = check_dir(SubDirs, LogFd, _Acc = []),
	    NewSubDirs;
	_ ->
	    %% Dir is NOT too large; 
	    %% include Dir (and by that all sub-dirs/files)
	    [Dir]
    end.

check_dir([Dir | T], LogFd, Acc) ->
    case get_size(Dir) of
	{ok, Size} when Size > 20000 -> %% 20000kB or 20MB
	    io:format(LogFd,
		      "Directory ~p is too large Size=~pkB. "
		      "Directory is excluded.~n", 
		      [Dir, Size]),
	    check_dir(T, LogFd, Acc);
	_ ->
	    %% Include Dir
	    check_dir(T, LogFd, [Dir | Acc])
    end;
check_dir([], _LogFd, Acc) ->
    Acc.


get_sub_dirs(ParentDir) ->
    case file:list_dir(ParentDir) of
	{ok, SubDirs} ->
	    [filename:join([ParentDir, SubDir]) || SubDir <- SubDirs];
	Other ->
	    warning_msg("Failed when getting sub-directories ~p~n", [Other]),
	    []
    end.



%%========================================================================
%% 
%%========================================================================
get_size_dirs([Dir | T], Acc) ->
    case get_size(Dir) of
	{ok, Size} ->
	    get_size_dirs(T, Acc + Size);
	nok ->
	    get_size_dirs(T, Acc)
    end;
get_size_dirs([], Acc) ->
    Acc.


%%========================================================================
%% chunk handling
%%========================================================================

%% get chunks of 100M files (before compressed)
get_chunks([Dir | T], LogFd, CurrentSize, CurrentDirs, Acc) ->
    %% NOTE: Dir can also be a single file
    ChunkFull = gc_chunk_full(get_size(Dir), CurrentSize),
    {NewDirs, NewCurrentSize, NewCurrentDirs, NewAcc} = 
	get_chunks_dir(ChunkFull,
		       Dir,
		       T,
		       CurrentSize, 
		       CurrentDirs, 
		       Acc,
		       LogFd),
    get_chunks(NewDirs, LogFd, NewCurrentSize, NewCurrentDirs, NewAcc);
get_chunks([], _LogFd, _CurrentSize, _CurrentDirs = [], Acc) ->
    Acc;
get_chunks([], _LogFd, _CurrentSize, CurrentDirs, Acc) ->
    [CurrentDirs | Acc].


%%--------------------------------------------
%% Still not full, try to fill more
%%--------------------------------------------
get_chunks_dir({not_full, NewSize}, 
	       Dir,
	       Dirs, 
	       _CurrentSize, 
	       CurrentDirs, 
	       Acc,
	       _) ->
    {Dirs, NewSize, [Dir | CurrentDirs], Acc};
%%--------------------------------------------
%% Full; start with new chunk
%%--------------------------------------------
get_chunks_dir(full, 
	       Dir,
	       Dirs, 
	       _CurrentSize, 
	       CurrentDirs, 
	       Acc,
	       _) ->
    {[Dir | Dirs], 0, [], [CurrentDirs | Acc]};
%%--------------------------------------------
%% Directory is larger than 100M; use sub-dirs or files
%%--------------------------------------------
get_chunks_dir(too_large, 
	       Dir, 
	       Dirs, 
	       CurrentSize, 
	       CurrentDirs, 
	       Acc, 
	       LogFd) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    %% Insert Files in the loop
	    Fs          = [filename:join([Dir, File]) || File <- Files],
	    UpdatedDirs = lists:append(Dirs, Fs),
	    {UpdatedDirs, CurrentSize, CurrentDirs, Acc};
	{error, enotdir} ->
	    %% File larger than 100M; do not included
	    Info = "File ~p larger than 100M; not included~n",
	    io:format(LogFd, Info, [Dir]),
	    {Dirs, LogFd, CurrentSize, CurrentDirs, Acc};
	{error, Reason} ->
	    Info = "Dir/File ~p not included due to reason: ~p~n",
	    io:format(LogFd, Info, [Dir, Reason]),
	    {Dirs, LogFd, CurrentSize, CurrentDirs, Acc}
    end;
%%--------------------------------------------
%% Directory error, skip the directory
%%--------------------------------------------
get_chunks_dir(nok, _, Dirs, CurrentSize, CurrentDirs, Acc, _) ->
    {Dirs, CurrentSize, CurrentDirs, Acc}.


%%==========================================================
%% check if the directory fits into the current chunk
%%==========================================================
gc_chunk_full({ok, Size}, CurrentSize) 
  when Size < 100000 ->
    NewSize = Size + CurrentSize,
    choose(NewSize < 100000, {not_full, NewSize}, full);
gc_chunk_full({ok, _Size}, _) ->
    too_large;
gc_chunk_full(_, _) ->
    nok.


get_size(Dir) ->
    Res = os:cmd("du -sL " ++ Dir),
    case string:tokens(Res, "\t") of
	[Size | _] ->
	    %% Size is in kB
	    try list_to_integer(Size) of
		S -> {ok, S}
	    catch error:badarg ->
		    filter_unreadable(Res, Dir)
	    end;
	[] ->
	    warning_msg("Directory size did not return:~n~s~p~n",
			["du -sL ", Dir]),
	    nok
    end.


%% {ok, Size} | nok
filter_unreadable(DuStr, Dir) ->
    NlToks = string:tokens(DuStr, "\n"),
    filter_unreadable(NlToks, "Permission denied", Dir).

filter_unreadable([Tok | T], Expect, Dir) ->
    case re:run(Tok, Expect) of
	{match, _} ->
	    warning_msg("Directory size failed for ~p~n~s~n",
			[Dir, Tok]),
	    filter_unreadable(T, Expect, Dir);
	nomatch ->
	    case string:tokens(Tok, "\t") of
		[Size | _] ->
		    try list_to_integer(Size) of
			S -> {ok, S}
		    catch error:badarg ->
			    warning_msg("Directory size failed for ~p~n~s~n",
					[Dir, Tok]),
			    nok
		    end
	    end
    end;
filter_unreadable([], _Expect, _Dir) ->
    %% Should not happen
    nok.





%%========================================================================
%% get_free_tmp() -> integer()
%% 
%% Information regarding free in WR6:
%% tmpfs is using real RAM but not counted in free.
%% This means that free really returns a incorrect value and the
%% real "free" value is as follows:
%% buffers/cache:free - "the used size of /tmp".
%% 
%% The above has been solved in WR8 free and the new column
%% available is giving a correct value (also including tmpfs).
%% See: http://www.linuxatemyram.com/ for more information.
%%========================================================================
get_free_tmp() ->
    get_free_tmp(sysEnv:target()).

get_free_tmp(true) ->
    %% WindRiver 8 version of free
    FreeRes = os:cmd("free -m | grep \"Mem:\" | awk '{print $7}'"),
    [Available] = string:tokens(FreeRes, " \n"),
    AvailMem = list_to_integer(Available),
    
    %% Get "Available" ($4) of /tmp - where esi is stored
    TmpPath = sysEnv:rcs_root() ++ "/tmp/",
    %% -m = MB
    DfRes2 = os:cmd("df -m " ++ TmpPath ++ " | awk '{print $4}'"), 
    [_,TmpAvailSizeStr] = string:tokens(DfRes2, "\n"),
    TmpAvailSize = list_to_integer(TmpAvailSizeStr),
    
    logLib:choose(TmpAvailSize > AvailMem, AvailMem, TmpAvailSize);
get_free_tmp(_) ->
    %% Skip check for sim; ESI is small and will fit.
    %% Sim env does not always have 100M available and the
    %% checks prevent the tests to pass
    1000.

%%========================================================================
%% check_disk_space() -> ok | {nok, Available}
%%========================================================================
check_disk_space() ->
    check_disk_space(sysEnv:rcs_mode_2()).

%%-------------------------------------
%% target
%%-------------------------------------
check_disk_space(target) ->
    DfRes        = os:cmd(["df -km ", sysEnv:rcs_dir()]), %% -m == MegaBytes
    Values       = hd(lists:reverse(string:tokens(DfRes, "\n"))),
    Available    = cds_get_available(Values),
    AvailableInt = list_to_integer(Available),
    %% If 100MB is available; allow generation of ESI
    logLib:choose(AvailableInt >= ?MIN_TMP_SIZE_MB, ok, {nok, Available});
%%-------------------------------------
%% vrcs and simulated
%% 
%% No decision has been made regarding 
%% disk space checks for VRCS jotj 20160516
%% Always assume ok in sim environment,
%% since df command looks different on 
%% different hosts.
%%-------------------------------------
check_disk_space(Mode) 
  when Mode == vrcs; 
       Mode == simulated ->
    ok.

cds_get_available(Values) ->
    [_Filesystem, _Blocks, _Used, Available, _UsePercent, _Mounted] =
	string:tokens(Values, "\t "),
    Available.


%%=======================================================================
%% pack_and_compress_dirs
%%=======================================================================
-spec pack_and_compress_dirs([string()], string(), string(), boolean()) ->
	  string().

pack_and_compress_dirs(Dirs, EsiFile, LogFile, InclErlangDir) ->
    {Encrypt, Uncrypt} = pacd_check_dirs(Dirs, [], []),
    pack_and_compress_dirs(Encrypt, Uncrypt, EsiFile, LogFile, InclErlangDir).

pacd_check_dirs([], Enc, Unc) ->
    {Enc, Unc};
pacd_check_dirs([D | Dirs], Enc, Unc) ->
    [Log | _] = lists:reverse(string:tokens(D, "/")),
    case certI:is_encryption_enabled() andalso logLib:is_encrypted(Log) of
	true  -> pacd_check_dirs(Dirs, [{Log, D} | Enc], Unc);
	false -> pacd_check_dirs(Dirs, Enc, [D | Unc])
    end.

pack_and_compress_dirs(EncDirsIn, UncDirs, EsiFile, LogFile, InclErlangDir) ->
    RootDir  = sysEnv:rcs_root(),
    KeepFrom = keep_from(RootDir),

    {EncDirs, EncCmd} = handle_encrypted(EncDirsIn, KeepFrom, [], []),
    {EDDirs,  EDCmd}  = handle_erlang_dir(InclErlangDir, UncDirs ++ EncDirs),
    {DBDirs,  DBCmd}  = handle_db_dir(EDDirs),
    {LFDirs,  LFCmd}  = include_log_file(LogFile, DBDirs, KeepFrom),
    AvliCmd = handle_avli(alhI:default_log_dir()),

    FilDirs = filter_dirs(LFDirs, KeepFrom),
    StrDirs = [Dir ++ " " || Dir <- FilDirs],
  
    {Cmd, CompEsiFile} = 
    	case lmaI:encrypt_esi_log() of
    	    false ->
    		%% Note: -z option must be before -f option
    		{"cd "
		 ++ RootDir
    		 ++ "; tar --ignore-failed-read  "
    		 ++ EDCmd
		 ++ DBCmd
		 ++ EncCmd
		 ++ AvliCmd
    		 ++ LFCmd
    		 ++ " -cvzf "
    		 ++ EsiFile ++ " "
		 ++ StrDirs, 
		 EsiFile};
    	    true ->
    		PubKey = code:priv_dir(log) ++ "/gpg/public.key",
    		{0, R} = cmdres("gpg --import " ++ PubKey),
    		UserId = get_user_from_key(string:tokens(R, " ")),
		{"cd "
		 ++ RootDir
    		 ++ "; gpg --import " ++ PubKey
    		 ++ "; tar --ignore-failed-read  "
    		 ++ EDCmd
		 ++ DBCmd
		 ++ EncCmd
		 ++ AvliCmd
    		 ++ LFCmd
    		 ++ " -cvzf - "
		 ++ StrDirs
    		 ++ "| gpg "
    		 ++ "--trust-model always --encrypt "
    		 ++ "--output " 
    		 ++ EsiFile ++ ".gpg "
    		 ++ "--recipient " ++ UserId,
		 EsiFile ++ ".gpg"}
    	end,
    
    {Code, Res} = cmdres(lists:flatten(Cmd)),
    case Code of
	0 -> ok;
	_ ->
	    warning_msg("Tar failed with exit ~w~n~s~n~s~n",
			[Code, Cmd, Res])
    end,
    CompEsiFile.

%%=======================================================================
%% pack_and_compress_ext_dirs
%%=======================================================================
-spec pack_and_compress_ext_dirs([string()], string(), string()) ->
	  string().

pack_and_compress_ext_dirs(Dirs, EsiFile, LogFile) ->
    RootDir  = sysEnv:rcs_root(),
    KeepFrom = keep_from(RootDir),

    {LFDirs,  LFCmd}  = include_log_file(LogFile, Dirs, KeepFrom),

    FilDirs = filter_dirs(LFDirs, KeepFrom),
    StrDirs = [Dir ++ " " || Dir <- FilDirs],

    {Cmd, CompEsiFile} = 
    	case lmaI:encrypt_esi_log() of
    	    false ->
    		%% Note: -z option must be before -f option
    		{"cd "
		 ++ RootDir
    		 ++ "; tar --ignore-failed-read  "
    		 ++ LFCmd
    		 ++ " -cvzf "
    		 ++ EsiFile ++ " "
		 ++ StrDirs, 
		 EsiFile};
    	    true ->
    		PubKey = code:priv_dir(log) ++ "/gpg/public.key",
    		{0, R} = cmdres("gpg --import " ++ PubKey),
    		UserId = get_user_from_key(string:tokens(R, " ")),
		{"cd "
		 ++ RootDir
    		 ++ "; gpg --import " ++ PubKey
    		 ++ "; tar --ignore-failed-read  "
    		 ++ LFCmd
    		 ++ " -cvzf - "
		 ++ StrDirs
    		 ++ "| gpg "
    		 ++ "--trust-model always --encrypt "
    		 ++ "--output " 
    		 ++ EsiFile ++ ".gpg "
    		 ++ "--recipient " ++ UserId,
		 EsiFile ++ ".gpg"}
    	end,
    
    {Code, Res} = cmdres(lists:flatten(Cmd)),
    case Code of
	0 -> ok;
	_ ->
	    warning_msg("Tar failed with exit ~w~n~s~n~s~n",
			[Code, Cmd, Res])
    end,
    CompEsiFile.



%%========================================================================
%% update_progress
%%========================================================================
update_progress(ProgressData) ->
    Result = proplists:lookup(result, ProgressData),
    Initial = (Result == {result, ?ActionResultType_NOT_AVAILABLE}),
    mnesia:transaction(
      fun() ->
	      [Obj]       = mnesia:wread({logM, {"1","1","1"}}),
	      NewProgress = up_progress(Initial,
					Obj#logM.progressReport,
					ProgressData),
	      mnesia:write(Obj#logM{progressReport = NewProgress})
      end).

up_progress(_, undefined, ProgressData) -> 
    Time = comsaI:iso_time(os:timestamp(), extended),
    Data = [{actionName,         "transferEsi"},
	    {additionalInfo,     [""]},
	    {progressInfo,       ""},
	    {progressPercentage, 0},
	    {result,             ?ActionResultType_NOT_AVAILABLE},
	    {resultInfo,         ""},
	    {state,              ?ActionStateType_RUNNING},
	    {actionId,           0},
	    {timeActionStarted,  Time}],
    comsaI:update_progress(Data ++ ProgressData, undefined);
%% If a previous ESI has failed do not update result attribute.
up_progress(false, 
	    #'AsyncActionProgress'{result = ?ActionResultType_FAILURE} = Progress,
	    ProgressData) ->
    comsaI:update_progress(proplists:delete(result, ProgressData),
			   Progress);
up_progress(_, Progress, ProgressData) ->
    comsaI:update_progress(ProgressData, Progress).
   

   


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% call to os:cmd
%%========================================================================
cmd(Cmd) ->
    os:cmd(Cmd).

cmdres(Cmd) ->
    CmdR   = Cmd ++ " ; echo -n \"Res=$?\"",
    Res    = os:cmd(CmdR),
    Code   = lists:last(string:tokens(Res, "\n=")),
    Rev    = lists:reverse(Res),
    Result = case string:str(Rev, "\n") of
		 0 -> "";
		 Pos -> lists:reverse(string:sub_string(Rev, Pos))
	     end,
    {list_to_integer(Code), Result}.

   

%%========================================================================
%% keep_from
%%========================================================================
keep_from("/")     -> 2;
keep_from(RootDir) ->  length(RootDir) + 2.


%%========================================================================
%% handle_encrypted
%%
%% Decrypt encrypted files to tmp/log/esi/Log
%% These files will be included in the ESI, but added to rcs/log dir
%% where the original encrypted files are located
%%========================================================================
handle_encrypted([], _, AccSym, AccCmd) ->
    {AccSym, lists:flatten(AccCmd)};
handle_encrypted([{Log, LogDir} | Dirs], KeepFrom, AccSym, AccCmd) ->
    {Sym, Cmd} = he_dir_loop(file:list_dir(LogDir), LogDir, Log, KeepFrom),
    handle_encrypted(Dirs, KeepFrom, [Sym | AccSym], [Cmd | AccCmd]).

%%--------------------------------------------------------------------
%% decrypt all files in a directory
%%--------------------------------------------------------------------
he_dir_loop({ok, AllFiles}, LogDir, Log, KeepFrom) ->
    CopyDir = filename:join([sysEnv:tmp_dir(), "log", "esi"]),
    ToDir   = he_get_to_dir(LogDir),
    Res = [he_file_loop(File, Log, CopyDir, LogDir) || File <- AllFiles],
    Sym = filename:join(CopyDir, Log),
    Cmd = lists:append(["--transform='s,",
			filter_dirs([CopyDir], KeepFrom), "/", Log,
			",", ToDir, ",' "]),
    he_dl_res([R || R <- Res, R /= ok], LogDir),
    {Sym, Cmd};
he_dir_loop(Error, LogDir, _Log, _KeepFrom) ->
    error_msg("Failed to decrypt files in directory: ~p~nError: ~p~n",
	      [LogDir, Error]),
    {[], []}.

he_dl_res([], _) ->
    ok;
he_dl_res(Errors, LogDir) ->
    [error_msg("Failed to decrypt files in directory: ~p~nError: ~p~n",
	       [LogDir, E]) || E <- Errors],
    ok.


he_get_to_dir([$/ | Dir]) -> Dir;
he_get_to_dir(Dir)        -> Dir.

%%--------------------------------------------------------------------
%% decrypt a file
%% disk_log .idx and .siz files are not encrypted
%%--------------------------------------------------------------------
he_file_loop(File, Log, CopyDir, FromDir) ->
    IsDecrypted = 
	filename:extension(File) /= ".idx" andalso
	filename:extension(File) /= ".siz",
    he_fl(IsDecrypted, File, Log, CopyDir, FromDir).


he_fl(true, File, Log, CopyDir, FromDir) ->
    FromFile = filename:join([FromDir, File]),
    ToFile   = filename:join([CopyDir, Log, File]),
    case filelib:ensure_dir(ToFile) of
	ok ->
	    logLib:decrypt_file(FromFile, ToFile);
	{error, Error} ->
	    {error, {encrypt_file_name, Error}}
    end;
he_fl(false, _File, _Log, _CopyDir, _FromDir) ->
    ok.





%%========================================================================
%%  move AVLI logs from RAM to its proper place
%%========================================================================
handle_avli([_ | AvliDir]) ->
    [_ | EsiDir] = choose(sysEnv:vrcs(), sysEnv:vnf_dir(), sysEnv:rcs_dir()),
    "--transform='s," ++ AvliDir ++ "," ++ EsiDir ++ "/alh,' ".
    



%%========================================================================
%% handle_erlang_dir(InclErlangDir, Dirs) -> NewDirs
%% InclErlangDir = true | false
%%
%% /rcs/erlang is symlink and needs special treatment
%% and cannot be included in Dirs. Instead /var/trace/erlang
%% is included in the Dirs and --transform:ed using tar.
%%========================================================================
handle_erlang_dir(true, Dirs) ->
    %% erlang-log is a symlink on target.
    %% erlang-log is an ordinary dir in sim.
    case sysEnv:target() of
	true ->
	    %% "/" is removed in filter_dirs below.
	    SymEd = "/var/trace/erlang",
	    TEDCmd = "--transform='s,var/trace/erlang,rcs/erlang,' ",
	    {[SymEd | Dirs], TEDCmd};
	false ->
	    EDir = filename:join([sysEnv:rcs_dir(), "erlang"]),
	    {[EDir | Dirs], ""}
    end;
handle_erlang_dir(false, Dirs) ->
    {Dirs, ""}.

%%========================================================================
%% handle_db_dir(Dirs) -> NewDirs
%%
%% When cloud environment: Find the mnesia dumps in /tmp/esi_tmp_db.
%%========================================================================
handle_db_dir(Dirs) ->
    case sysEnv:vrcs() of
	    false ->
		{Dirs, ""};
	    true ->
		{["/tmp/esi_tmp_db"|Dirs],
		 "--transform='s,tmp/esi_tmp_db,rcs/db,' "}
	end.

%%========================================================================
%% Include LogFile in Dirs so LogFile is included in ESI-file.
%%========================================================================
include_log_file([], Dirs2, _) ->
	{Dirs2, ""};
include_log_file(LogFile, Dirs2, KeepFrom) ->
    LogFileDir = filename:dirname(LogFile),
    LFDNR      = string:substr(LogFileDir, KeepFrom),
    TLFCmd     = "--transform='s," ++ LFDNR ++ "/,,' ",
    {[LogFile | Dirs2], TLFCmd}.



%%========================================================================
%% Filter dirs
%%========================================================================
filter_dirs(Dirs, KeepFrom) ->
    [string:substr(Dir, KeepFrom) || Dir <- Dirs].



%%========================================================================
%% get_user_from_key
%%========================================================================
get_user_from_key([R | Rest]) ->
    case string:str(R, "@") of
	0 ->
	    get_user_from_key(Rest);  
	_ ->
	    %% The string contains the user
	    %% Remove unwanted caracters
	    strip(strip(strip(R, $<), $\"), $>)
    end.

strip(Str, Char) ->
    string:strip(Str, both, Char).

%%=======================================================================
%% ESI callbacks took long time
%%=======================================================================
print_esi_time_warning() ->
    info_msg("ESI callbacks took more than ~p seconds to execute~n", 
	     [?WARNING_ESI_CB_TIME]).



%%========================================================================
%% Print functions
%%========================================================================
print_top(LogFd) ->
    R = os:cmd("top -b -n 1"),
    io:format(LogFd, "~s", [R]).

print_tmp(LogFd) ->
    TmpDir = filename:join([sysEnv:rcs_root() ++ "/tmp/"]),
    io:format(LogFd, "Listing of all files below ~p~n", [TmpDir]),
    R = os:cmd("ls -lRh " ++ TmpDir),
    io:format(LogFd, "~s~n", [R]).

print_chunks(LogFd, [Chunk | T], No) ->
    io:format(LogFd, "Chunk number ~p contains the following~n", [No]),
    print_listing(LogFd, Chunk),
    io:format(LogFd, "~n", []), 
    print_chunks(LogFd, T, No+1);
print_chunks(_LogFd, [], _No) ->
    ok.

print_listing(LogFd, Dirs) ->
    FixDirs = [Dir ++ " " || Dir <- Dirs],
    io:format(LogFd, "~s~n", [os:cmd("ls -lRhH " ++ FixDirs)]).

%%% ----------------------------------------------------------
%% print_granularity
%%% ----------------------------------------------------------
print_granularity(LogFd, Granularity) 
  when Granularity =:= undefined;
       Granularity =:= large ->
    io:format(LogFd, 
	      "~nGranularity large - all directories included~n~n",
	      []);
print_granularity(LogFd, small) ->
    io:format(LogFd, 
	      "~nGranularity small - "
	      "excluding directories if needed. ~n~n",
	     []);
print_granularity(LogFd, static) ->
    io:format(LogFd,
	      "~nGranularity static:~n"
	      "- Excluding directories if needed~n"
	      "- No ESI callbacks will be executed, which implies that:~n"
	      "  - not all directories may exist~n"
	      "  - some logs may contain data generated for an older ESI ~n~n",
	     []);
print_granularity(LogFd, Granularity) ->
    io:format(LogFd, "~nIncorrect Graunlarity ~p.~n", [Granularity]),
    error_msg("~nIncorrect Granularity ~p~n~n", [Granularity]).



count([_ | T], Count) -> count(T, Count + 1);
count([], Count) -> Count.





choose(true,  T, _) -> T;
choose(false, _, F) -> F.

%%========================================================================
%%% sysInitI handling
%%========================================================================
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: " ++ Format, [?MODULE | Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format, [?MODULE | Args]).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: " ++ Format, [?MODULE | Args]).



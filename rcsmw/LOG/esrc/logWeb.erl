%%==============================================================================
%%  1.    BASIC INFORMATION
%%==============================================================================

%%==============================================================================
%% 1.1   MODULE INFORMATION
%%==============================================================================
%% %CCaseFile:	logWeb.erl %
%% @copyright Ericsson AB 2017
%% @version /main/R9A/R10A/3
%% 
%% @doc == REST API for LOG, version 1 ==
%% Interface module for REST API, specified in log_web.xml.
%%
%% @end
%%==============================================================================

%%==============================================================================
%% 1.2   MODULE DEFINITION
%%==============================================================================
-module(logWeb).
-vsn('/main/R9A/R10A/3').
-date('2017-05-16').

%%==============================================================================
%% # 1.3   LEGAL RIGHTS
%%==============================================================================
%% %CCaseTemplateFile:	module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%
%%==============================================================================

%%==============================================================================
%% 1.4   REVISION LOG
%% -----------------------------------------------------------------------------
%% Rev       Date       Name     What
%% ----      ---------- -------  -------------------------------------------------
%% R9A/1-4   2017-03-28 uabesvi  Created.
%%==============================================================================

%%==============================================================================
%% 2.    INTERNAL DEFINITIONS
%%==============================================================================

%%==============================================================================
%% 2.1   EXPORTED INTERFACE FUNCTIONS
%%==============================================================================
%% 2.1.1 Interface functions
%%==============================================================================

%% REST Query
-export([export_upgrade_logs/4]).
-export([import_upgrade_logs/5]).

-export([export_esi/4]).

-export([cleanup_upgrade_dirs/0]).
-export([cleanup_esi_dirs/0]).

-export([get_time/0]).

%%==============================================================================
%% 2.2   EXPORTED INTERNAL FUNCTIONS
%%==============================================================================

%%==============================================================================
%% 2.3   IMPORT OF DEFINITIONS
%%==============================================================================
-include("log.hrl").

%%==============================================================================
%% 2.4   LOCAL DEFINITION OF MACROS
%%==============================================================================

%%==============================================================================
%% 2.5   LOCAL DEFINITION OF RECORDS
%%==============================================================================

%%==============================================================================
%% 2.6   LOCAL DEFINITION OF TYPES
%%==============================================================================

%%==============================================================================
%% # 3.    CODE
%%==============================================================================

%%==============================================================================
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%==============================================================================

%%==============================================================================
%% @doc REST query for export upgrade logs
%%
%% @end
%%==============================================================================
-spec export_upgrade_logs(Version   :: atom(),
			  SessionID :: pid(),
			  Env       :: list({Property :: any(), 
					     Value    :: any()}),
			  Body      :: string()) ->
    string().

export_upgrade_logs(Version, _SessionID, _Env, _Body) 
  when Version == ?WEB_VERSION_1 ->
    do_generate_log_file(make_upgrade_dir()).


%%==============================================================================
%% @doc REST query for import upgrade logs
%%
%% @end
%%==============================================================================
-spec import_upgrade_logs(Version   :: atom(),
			  WriteRes  :: ok | {error, any()},
			  Data      :: {string(), string()},
			  SessionID :: pid(),
			  Env       :: list({Property :: any(), 
					     Value    :: any()})) ->
    any().

import_upgrade_logs(Version, WriteRes, {Dir, File}, _SessionID, _Env) 
  when Version  == ?WEB_VERSION_1 andalso 
       WriteRes == ok ->
    Tar  = ?UPGR_FILE_PREFIX ++ ".tar",
    Cmd = "cd " ++ Dir ++
	"; pwd; ls" ++
	"; gunzip "  ++ File ++
	"; tar xvf " ++ Tar ++
	"; rm " ++ Tar,
    log("~p import_upgrade_logs  Cmd ~n~p~n", [?MODULE, Cmd]),
    X = cmd(Cmd),
    log("~p import_upgrade_logs  Cmd Res ~n~p~n", [?MODULE, X]),

    alhI:push_from_file(filename:join([Dir, ?AVLI_DIR, ?AVLI_LOG])),
    ok;
import_upgrade_logs(Version, _WriteRes, _, _SessionID, _Env)
  when Version =/= ?WEB_VERSION_1 ->
    ?WEB_ERROR_VERSION;
import_upgrade_logs(_Version, _WriteRes, _, _SessionID, _Env) ->
    ?WEB_ERROR_SAVE.


%%==============================================================================
%% @doc REST query for export esi
%%
%% @end
%%==============================================================================
-spec export_esi(Version   :: atom(),
		 SessionID :: pid(),
		 Env       :: list({Property :: any(), 
				    Value    :: any()}),
		 Body      :: string()) ->
    string().

export_esi(Version, _SessionID, _Env, _Body) 
  when Version == ?WEB_VERSION_1 ->
    {_, _, File} = logEsi:generate_esi(static, complete),
    File;
export_esi(Version, _SessionID, _Env, _Body)
  when Version =/= ?WEB_VERSION_1 ->
    ?WEB_ERROR_VERSION;
export_esi(_Version, _SessionID, _Env, _Body) ->
    ?WEB_ERROR_SAVE.



%%==============================================================================
%% @doc cleanup_upgrade_dirs()
%% 
%% Do hosekeeping of the upgrade directories.
%% 
%% @end
%%==============================================================================
cleanup_upgrade_dirs() ->
    cleanup_dirs(filelib:wildcard(?UPGR_DIR ++ "/*"), ?UPGR_DIRS_MAX).


%%==============================================================================
%% @doc cleanup_esi_dir()
%% 
%% Do hosekeeping of the esi directory
%% 
%% @end
%%==============================================================================
cleanup_esi_dirs() ->
    cleanup_dirs(filelib:wildcard(?ESI_DIR ++ "/*"), ?ESI_DIRS_MAX).

%%========================================================================
%% @doc get_time() -> "YYMMDDTHHMMSS"
%% @end
%%========================================================================
get_time() ->
    get_time(calendar:now_to_local_time(os:timestamp())).

get_time({{Y, M, D}, {H, Mi, S}}) ->
    lists:append([integer_to_list(Y),
		  gt_zero(M),
		  integer_to_list(M),
		  gt_zero(D),
		  integer_to_list(D), "T",
		  gt_zero(H),
		  integer_to_list(H),
		  gt_zero(Mi),
		  integer_to_list(Mi),
		  gt_zero(S),
		  integer_to_list(S)]).

gt_zero(X) when X < 10 -> "0";
gt_zero(_)             -> "".





%%==============================================================================
%% 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%==============================================================================

%%==============================================================================
%% 3.3   CODE FOR INTERNAL FUNCTIONS
%%==============================================================================


%%==============================================================================
%% @doc make_upgrade_dir()
%% 
%% Create the temporary directory where the compressed file containing
%% the upgrade logs is stored. This is called on the FROM VNFC.
%% 
%% @end
%%==============================================================================
make_upgrade_dir() ->
    EsiTmpDir = filename:join([sysEnv:rcs_root(), "tmp", "upgrade_logs"]),
    Template  = filename:join(EsiTmpDir, "upgrade_logs.XXXXX"),
    cmd(["mkdir -p ", EsiTmpDir, " && mktemp -d ", Template])--"\n".


%%==============================================================================
%% @doc cleanup_dirs(Dirs, MaxDirectories)
%% 
%% Remove directories that exceed MaxDirectories,
%% @end
%%==============================================================================
cleanup_dirs([Oldest | T] = Files, MaxDirs)
  when length(Files) >= MaxDirs ->
    cmd("rm -rf " ++ Oldest),
    cleanup_dirs(T, MaxDirs);
cleanup_dirs(_, _MaxDirs) ->
    ok.
    


%%==============================================================================
%% @doc do_generate_log_file(UpgrDir)
%% 
%% Calculate the directory where the upgrade file is temporarily stored.
%% Then pack and compress the included LOGs
%% 
%% @end
%%==============================================================================
do_generate_log_file(UpgrDir) ->
    cmd(["rm -rf ", UpgrDir]),
    Time         = get_time(),
    LogFileName  = "upgrade_files" ++ "." ++ Time ++ ".log",
    LogFile      = filename:join(UpgrDir, LogFileName),
    UpgrFileName = "upgrade.du" ++ "." ++ Time ++ ".tar.gz",
    UpgrFile     = filename:join(UpgrDir, UpgrFileName),
    ok           = filelib:ensure_dir(LogFile),
    {ok, LogFd}  = file:open(LogFile, [write]),
    AvliLogFile  = filename:join([alhI:default_log_dir(), ?AVLI_LOG]),
    _AvliResult  = generate_avli_log(AvliLogFile, LogFd),
    LogDirs      = get_log_dirs(),
    UpgrFile     = pack_and_compress_dirs(LogDirs, UpgrFile, LogFile, LogFd),
    file:sync(LogFd),
    file:close(LogFd),
    UpgrFile.


%%-----------------------------------------------------------------------
%% Find the internal logs that are to be included
%%-----------------------------------------------------------------------
get_log_dirs() ->
    [filename:join([sysEnv:vnf_dir(), "log", Log]) || Log <- ?UPGR_LOGS]. 


%%-----------------------------------------------------------------------
%% generate a file with AVLI log entries
%%-----------------------------------------------------------------------
generate_avli_log(AvliLogFile, LogFd) ->    
    Result = alhI:dump_to_file(AvliLogFile),
    io:format(LogFd, "~n~p generate avli. ~n  Result = ~p~n", [time(), Result]).


%%-----------------------------------------------------------------------
%% pack and comress the included directories
%%-----------------------------------------------------------------------
pack_and_compress_dirs(LogDirs, UpgrFile, LogFile, LogFd) ->
    io:format(LogFd, "~n~p pack_and_compress_dirs ~n"
	      "  LogDirs     = ~p~n" 
	      "  UpgradeFile = ~p~n",
	      [time(), LogDirs, UpgrFile]),
    RootDir  = sysEnv:rcs_root(),
    KeepFrom = keep_from(RootDir),

    {EDDirs,  EDCmd}   = handle_erlang_dir(LogDirs),
    {LFDirs,  LFCmd}   = include_log_file(LogFile, EDDirs, KeepFrom),
    {AvliDir, AvliCmd} = handle_avli(),
    {VarDir,  VarCmd}  = handle_var_log(),

    FilDirs = filter_dirs(LFDirs, KeepFrom),
    StrDirs = [Dir ++ " " || Dir <- FilDirs ++ [AvliDir, VarDir]],
  
    {TarCmd, CompUpgrFile} = pacd(RootDir,
				  EDCmd ++ AvliCmd ++ VarCmd ++ LFCmd,
				  UpgrFile,
				  StrDirs),
    
    io:format(LogFd, "~n~p Create upgrade file command~n  ~p~n",
	      [time(), TarCmd]),
    pacd_res(cmd_res(TarCmd), TarCmd),
    CompUpgrFile.

pacd_res({0, _}, _) ->
    info_msg("Upgrade file successfully created~n", []);
pacd_res({Code, Res}, TarCmd) ->
    warning_msg("ERROR. Tar failed with exit ~w~n~s~n~s~n",
		[Code, Res, TarCmd]).

pacd(RootDir, Cmds, UpgrFile, StrDirs) ->
    %% Note: -z option must be before -f option
    TarCmd = "cd "
	++ RootDir
	++ "; tar --ignore-failed-read "
	++ Cmds
	++ " -cvzf "
	++ UpgrFile ++ " "
	++ StrDirs, 
     {lists:flatten(TarCmd), UpgrFile}.



%%========================================================================
%% handle_erlang_dir(InclErlangDir, Dirs) -> NewDirs
%% InclErlangDir = true | false
%%
%% /rcs/erlang is symlink and needs special treatment
%% and cannot be included in Dirs. Instead /var/trace/erlang
%% is included in the Dirs and --transform:ed using tar.
%%========================================================================
handle_erlang_dir(Dirs) ->
    %% erlang-log is a symlink on target.
    %% erlang-log is an ordinary dir in sim.
    case sysEnv:target() of
	true ->
	    %% "/" is removed in filter_dirs below.
	    SymEd  = "/var/trace/erlang",
	    TEDCmd = "--transform='s,var/trace/erlang,rcs/erlang,' ",
	    {[SymEd | Dirs], TEDCmd};
	false ->
	    EDir = filename:join([sysEnv:rcs_dir(), "erlang"]),
	    {[EDir | Dirs], ""}
    end.

%%========================================================================
%% Include LogFile in Dirs so LogFile is included in UPGR-file.
%%========================================================================
include_log_file([], Dirs2, _) ->
	{Dirs2, ""};
include_log_file(LogFile, Dirs2, KeepFrom) ->
    LogFileDir = filename:dirname(LogFile),
    LFDNR      = string:substr(LogFileDir, KeepFrom),
    TLFCmd     = "--transform='s," ++ LFDNR ++ "/,,' ",
    {[LogFile | Dirs2], TLFCmd}.


%%========================================================================
%%  move AVLI logs from RAM to its proper place
%%========================================================================
handle_avli() ->
    handle_avli(alhI:default_log_dir() -- sysEnv:rcs_root()).
    
handle_avli([$/ | TransDir]) ->
    {TransDir, "--transform='s," ++ TransDir ++ "," ++ ?AVLI_DIR ++ ",' "};
handle_avli(TransDir) ->
    {TransDir, "--transform='s," ++ TransDir ++ "," ++ ?AVLI_DIR ++ ",' "}.
    
%%========================================================================
%%  copy and zip /var/log files
%%========================================================================
handle_var_log() ->
    %% compress /var/log
    TmpVarDir = filename:join([sysEnv:rcs_root(), "tmp", "log", "var"]),
    ok        = filelib:ensure_dir(TmpVarDir ++ "/"),
    Cmd       = "cd " ++
	TmpVarDir ++ 
	"; tar --ignore-failed-read " ++ 
	" -cvzf " ++ ?VAR_LOG ++ " " ++ ?VAR_DIR,
    hvl_res(cmd_res(Cmd),
	    hvl_strip_slash(TmpVarDir -- sysEnv:rcs_root()),
	    Cmd).


hvl_strip_slash([$/ | TransDir]) ->
    TransDir;
hvl_strip_slash(TransDir) ->
    TransDir.


hvl_res({0, _}, TransDir, _Cmd) ->
    info_msg("/var/log compressed to ~p~n", [TransDir]),
    {TransDir, "--transform='s," ++ TransDir ++ "," ++ "var" ++ ",' "};
hvl_res(Error, TransDir, Cmd) ->
    warning_msg("ERROR. /var/log tar failed with exit ~p~n~p~n~p~n",
		[Error, TransDir, Cmd]),
    {"", ""}.
   

%%========================================================================
%% Filter dirs
%%========================================================================
filter_dirs(Dirs, KeepFrom) ->
    [string:substr(Dir, KeepFrom) || Dir <- Dirs].


%%========================================================================
%% keep_from
%%========================================================================
keep_from("/")     -> 2;
keep_from(RootDir) -> length(RootDir) + 2.


%==============================================================================
%% 3.3.2 Help Functions
%%==============================================================================

cmd(Cmd) ->
    os:cmd(Cmd).

cmd_res(Cmd) ->
    CmdR   = Cmd ++ " ; echo -n \"Res=$?\"",
    Res    = os:cmd(CmdR),
    Code   = lists:last(string:tokens(Res, "\n=")),
    Rev    = lists:reverse(Res),
    Result = case string:str(Rev, "\n") of
		 0   -> "";
		 Pos -> lists:reverse(string:sub_string(Rev, Pos))
	     end,
    {list_to_integer(Code), Result}.


info_msg(Format, Args) ->
    sysInitI:info_msg("~w: " ++ Format, [?MODULE | Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format, [?MODULE | Args]).





%%==============================================================================
%% 4     CODE FOR TEMPORARY CORRECTIONS
%%==============================================================================

%% used for debugging
log(_, _) ->
    ok.

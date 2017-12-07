%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logGenerateEsiExt.erl %
%%% Author:	
%%% Description:
%%%
%%% This module generates an ESI file for external boards.
%%%
%%% ----------------------------------------------------------
-module(logGenerateEsiExt).
-vsn('/main/R10A/R11A/R12A/2').
-date('2017-11-08').
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R10A/1-7   2017-06-21 uabesvi     Code moved from logEsi
%%% R11A/1     2017-08-30 uabesvi     RU ESI additions
%%% R11A/4     2017-10-02 uabesvi     Fixes for RU ESI
%%% R11A/5     2017-10-12 uabesvi     Cleanuip and comments, 
%%%                                   fixed ESI directory structure 
%%% R12A/2     2017-11-08 uabesvi     HW37113 ESI during warm restarts
%%% ----------------------------------------------------------
%%%
%%% ---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ---------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------






%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_esi_ext/3]).
-export([generate_esi_ext/4]).

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



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%=======================================================================
%% generate_esi_ext({RuId, Mbox}, Granularity, Mode) ->  
%%      {Node, EsiVersion, EsiFile} | {Node, none}
%% 
%% See also generate_esi_ext/4
%% Generate an ESI file for an exteranal board.
%% 
%%=======================================================================
generate_esi_ext(RuData, Granularity, Mode) ->
    EsiDir = logEsiLib:make_esi_dir(),
    try generate_esi_ext(EsiDir, RuData, Granularity, Mode)
    catch T:E ->
	    cmd(["rm -rf ", EsiDir]),
	    erlang:T(E)
    after
	case get(fd) of
	    Fd when is_pid(Fd) -> file:close(Fd);
	    undefined          -> ok
	end
    end.

%%=======================================================================
%% generate_esi_ext(EsiDir, {RuId, Mbox}, Granularity, Mode) ->  
%%      {Node, EsiVersion, EsiFile} | {Node, none}
%% 
%% EsiDir      = string()  Temp area where the ESI is stored before
%%                         exporting it to the ftp server.
%% RuId        = 
%% Mbox        = integer() Mailbox to CAT
%% Granularity = large | small | static | undefined
%% Node        = atom()
%% EsiVersion  = full | limited | chunk
%% EsiFile     = string()   Compressed ESI file name
%% 
%% Generate an ESI file for an exteranal board.
%% 
%%=======================================================================
generate_esi_ext(EsiDir, {RuId, Mbox}, Granularity, Mode) ->
    TS = comsaI:iso_time(os:timestamp(), basic),
    {LogFileName, EsiFileName} = get_file_names(RuId, TS, sysEnv:vrcs()),
    cmd(["rm -rf ", EsiDir]),
    LogFile     = filename:join(EsiDir, LogFileName),
    EsiFile     = filename:join(EsiDir, EsiFileName),
    ok          = filelib:ensure_dir(LogFile),
    {ok, LogFd} = file:open(LogFile, [write]),
    put(fd, LogFd),
    io:format(LogFd, "Generating Ericsson Support information: ~s~n~n", [TS]),

    logEsiLib:print_granularity(LogFd, Granularity),

    %%=======================================================
    %% Invoke ESI external board callbacks.
    %% Print a warning if the callbacks take very long time 
    %%=======================================================
    {ok, Ref} = timer:apply_after(?WARNING_ESI_CB_TIME * 1000, 
				  logEsiLib, 
				  print_esi_time_warning,
				  []),

    Timeout = ?WARNING_ESI_CB_TIME * 1000, 
    DirInfo = logEsiExtServer:get_ext_board_info(RuId, Mbox, Timeout),
    Res     = generate_esi(DirInfo, LogFd, EsiFile, LogFile, Mode),
    timer:cancel(Ref),
    logEsiExtServer:ext_esi_ready(RuId, Mbox),
    Res.
    


%%=======================================================================
%% get_file_names() -> {LogFileName, EsiFileName}
%% 
%% genereate the ESI file name, and the file name for 
%% log file (where the ESI generation events are logged)
%%=======================================================================
get_file_names(RuId, TS, IsVrcs) when is_integer(RuId) ->
    get_file_names(integer_to_list(RuId), TS, IsVrcs);
get_file_names(RuId, TS, false) ->
    %% Could probably use env HOST here but to be 
    %% on the safe side the old way is kept.
    {"esi.ru_" ++ RuId ++ "." ++ TS ++ ".log",
     "esi.ru_" ++ RuId ++ "." ++ TS ++ ".tar.gz"};
get_file_names(_RuId, TS, true) ->
    Host = case os:getenv("HOST") of
	       false -> "vrcs";
	       ""    -> "vrcs";
	       H     -> H		    
	   end,
    {"esi." ++ Host ++ "." ++ TS ++ ".log",
     "esi." ++ Host ++ "." ++ TS ++ ".tar.gz"}.
   

%%=======================================================================
%% generate_esi() ->  {Node, EsiVersion, EsiFile} | {Node, none}
%% 
%% Node       = atom()
%% EsiVersion = full | limited | chunk
%% EsiFile    = string()   Compressed ESI file name
%% 
%% 
%% 
%% 
%% 
%%=======================================================================
generate_esi({ok, {_RuId, IpAddr, Port, {EsiData, _Remaining}}}, 
	     LogFd, 
	     EsiFile, 
	     LogFile, 
	     Mode) ->
    {Dirs, Size} = lists:foldl(fun fd_get_dirs/2, {[], 0}, EsiData),
    
    OutDir = logEsiLib:make_esi_dir(),
    sysInitI:info_msg("~p: TMP ESI DIR ~p~n", [?MODULE, OutDir]),
    fetch_index_file(IpAddr, Port, OutDir),

%%    Files = test_fd_get_files(OutDir ++ "/index.html", Dirs),

    IndexFile = OutDir ++ "/index.html",
    Files     = fd_get_files(file:read_file(IndexFile), Dirs),

    fetch_ru_files(IpAddr, Port, Files, OutDir),
    cmd(["rm ", OutDir ++ "/index.html"]),
    
    FreeTmpSize = logEsiLib:get_free_tmp(),
    Res = generate_file(Size / (1024 * 1024), 
			FreeTmpSize, 
			[OutDir], 
			LogFd, 
			EsiFile, 
			LogFile, 
			Mode),
    cmd(["rm -rf ", OutDir]),
    Res;
generate_esi(Error, _, _, _, _) ->
    Error.
    

%%-----------------------------------------------------------
%% 
%%-----------------------------------------------------------
fd_get_dirs({Dir, Size}, {Dirs, TotSize}) ->
    {[Dir | Dirs], TotSize + Size}.
    

%%-----------------------------------------------------------
%% Remove files that are not in the directories
%% specified in the board info message.
%%-----------------------------------------------------------
fd_get_files({ok, IndexFile}, Dirs) ->
    List    = string:tokens(binary_to_list(IndexFile), "\n"),
    Tokens  = [string:tokens(Str, "<>") || [$<, $p, $> | _ ] = Str <- List], 
    Files   = [File || [_, _, File | _] <- Tokens],
    fd_gf(Files, Dirs, []);
fd_get_files({error, enoent}, _Dirs) ->
    [].

%% Loop through all directories.
fd_gf([], _Dirs, Acc) ->
    Acc;
fd_gf([F | T], Dirs, Acc) ->
    R = [string:str(F, Dir) || Dir <- Dirs],
    case [Found || Found <- R, Found > 0] of
	[] -> fd_gf(T, Dirs, Acc);
	_  -> fd_gf(T, Dirs, [F | Acc])
    end.
		  
%% test_fd_get_files(_Index, _Dirs) ->
%%     ["/home/uabesvi/tmp/kemi/var/log/auth.log", 
%%      "/home/uabesvi/tmp/kemi/var/log/wtmp",
%%      "/home/uabesvi/tmp/kemi/var/log/syslog",
%%      "/home/uabesvi/tmp/kemi/var/log/erlang.log.1",
%%      "/home/uabesvi/tmp/kemi/var/log/erlang.log.2",
%%      "/home/uabesvi/tmp/kemi/var/log/erlang.log.3",
%%      "/home/uabesvi/tmp/kemi/var/log/erlang.log.4",
%%      "/home/uabesvi/tmp/kemi/var/log/erlang.log.5"
%%     ].


%%-----------------------------------------------------------
%% fetch_index_file() -> binary()
%% 
%% When invoking the WEB server with root directory it will
%% reply with an index.html file. It contains all the dierctories
%% and files in the WEB server directory.
%%-----------------------------------------------------------
fetch_index_file(IpAddr, Port, OutDir) ->
    Cmd = ("wget --proxy off " ++ 
	   "-P " ++ OutDir ++
	   " http://" ++ IpAddr ++ ":" ++ integer_to_list(Port)),
    cmd(Cmd).


%%-----------------------------------------------------------
%% Fetch the RU files one by one.
%%-----------------------------------------------------------
fetch_ru_files(IpAddr, Port, Files, OutDir) ->
    try fruf(IpAddr, Port, Files, OutDir)
    catch T:E ->
	    cmd(["rm -rf ", OutDir]),
	    erlang:T(E)
    after
	case get(fd) of
	    Fd when is_pid(Fd) -> file:close(Fd);
	    undefined          -> ok
	end
    end.


fruf(IpAddr, Port, Files, OutDir) ->
    Cmd = "wget --proxy off -P ",
    URI = " http://" ++ IpAddr ++ ":" ++ integer_to_list(Port),
    [fruf_get_one_file(Cmd, URI, OutDir, File) || File <- Files].


%% Use the same directory structure in ESI as on the RU.	   
fruf_get_one_file(Cmd, URI, OutDir, File) ->
    FileDir  = filename:dirname(File),
    StoreDir = OutDir ++ FileDir,
    filelib:ensure_dir(StoreDir ++ "/"),
    cmd(Cmd ++ StoreDir ++ URI ++ File).

%% For testing
%%    cmd("cp " ++ File ++ " " ++ StoreDir).
    


%%=======================================================================
%% generate_file(EsiSize, FreeSize, AllDirs, LogFd, EsiFile, LogFile, _Mode)) ->
%%    {Node, EsiVersion, EsiFile} | {Node, none}
%% 
%% Node       = atom()
%% EsiVersion = full | limited | chunk
%% EsiFile    = string()   Compressed ESI file name
%% 
%% 
%% Check /tmp and free RAM memory and compared 
%% with size of ESI-files (not compressed)
%% 
%% Information: 
%% For WR6 dus32 cannot allocate all ram and trying using ram less 
%% than 200Mb causes node to reboot. Due to this 250MB was used as
%% a safety margin.
%% WR8 is not having this problem for dus32 so this safety margin 
%% is now removed.
%% 
%%=======================================================================

%%----------------------------------------------------
%% More than 50MB left
%% Mode = any | complete
%%----------------------------------------------------
generate_file(EsiSize, FreeSize, AllDirs, _LogFd, EsiFile, LogFile, _Mode)
  when FreeSize - EsiSize > 50 ->

%%     io:format(LogFd, "All directories are included in the ESI~n", []),
%%     logEsiLib:print_listing(LogFd, AllDirs),
%%     file:close(LogFd),
    CompEsiFile = logEsiLib:pack_and_compress_ext_dirs(AllDirs, 
						       EsiFile, 
						       LogFile),
    {clhI:erlang_node(), ?ESI_VSN_FULL, CompEsiFile};

%%----------------------------------------------------
%% Size at least 150MB to be allowed to do 100MB chunk 
%% (gives 50MB left).
%% Chunk only allowed for Mode = any.
%%----------------------------------------------------
generate_file(_EsiSize, FreeSize, AllDirs, LogFd, EsiFile, LogFile, Mode) 
  when FreeSize >= 150 andalso 
       Mode == any ->
    %% Size at least 150MB to be allowed to do 100MB chunk 
    %% (gives 50MB left).
    %% Chunk only allowed for Mode = any.
    _AllChunks = [FirstChunkDirs | T] =
	logEsiLib:get_chunks(AllDirs, LogFd, 0, [], []),
%%     io:format(LogFd,
%% 	      "ESI is larger than available /tmp disk. "
%% 	      "ESI is chunked in ~p pieces~n~n",
%% 	      [logEsiLib:count(AllChunks, 0)]),
%%     logEsiLib:print_chunks(LogFd, AllChunks, 1),
%%     file:close(LogFd),
    CompEsiFile = logEsiLib:pack_and_compress_ext_dirs(FirstChunkDirs, 
						       EsiFile, 
						       LogFile),
    {clhI:erlang_node(), ?ESI_VSN_CHUNK, CompEsiFile, T};


%%----------------------------------------------------
%% EsiSize does not fit Size (RAM) left.
%%----------------------------------------------------
generate_file(_EsiSize, FreeSize, _, _LogFd, _, _, _Mode) ->
    warning_msg(?LINE, 
		"No ESI since ESI doesn't fit available RAM ~pM~n",
		[FreeSize]),
    {clhI:erlang_node(), none}.




%%========================================================================
%% call to os:cmd
%%========================================================================
cmd(Cmd) ->
    os:cmd(Cmd).



%%% ----------------------------------------------------------
%%% sysInitI handling
%%% ----------------------------------------------------------
warning_msg(Line, Format, Args) ->
    sysInitI:warning_msg("~w:~p " ++ Format, [?MODULE, Line | Args]).



%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logGenerateEsi.erl %
%%% Author:	
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(logGenerateEsi).
-vsn('/main/R10A/R12A/1').
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
%%% R10A/1-2   2017-06-07 uabesvi     Code moved from logEsi
%%% R12A/1     2017-11-08 uabesvi  HW37113 ESI during warm restarts
%%% ----------------------------------------------------------
%%%
%%% ---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ---------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


%% Called by AIC when going back to NL at AI failure.
-export([generate_esi/0]).

%% called from logWeb
-export([generate_esi/2]).

%% Called by AIC for emergency access
-export([generate_esi/3]).

%% Called from logEsi
-export([generate_esi/4]).


%% Called by SWM when before rollback (upgrade or failsafe).
-export([generate_rollback_esi/0]).



%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_cb_fun/2, generate_cb_fun/3]).
-export([handle_fail/1]).

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
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%% Called from aicServer
generate_esi() ->
    generate_esi(_Granularity = undefined, _Mode = complete).

generate_esi(Granularity, Mode) ->
    MpId = clhI:mp_id(),
    generate_esi(MpId, Granularity, Mode).

%% Called from aicGui
generate_esi(MpId, Granularity, Mode) ->
    EsiDir = logEsiLib:make_esi_dir(),
    try generate_esi(logServer:get_warm_restart_state(),
		     EsiDir, 
		     MpId, 
		     Granularity, 
		     Mode)
    catch T:E ->
	    cmd(["rm -rf ", EsiDir]),
	    erlang:T(E)
    after
	case get(fd) of
	    Fd when is_pid(Fd) -> file:close(Fd);
	    undefined          -> ok
	end
    end.

%% Called from logEsi
generate_esi(EsiDir, MpId, Granularity, Mode) ->
    generate_esi(logServer:get_warm_restart_state(),
		 EsiDir, 
		 MpId, 
		 Granularity, 
		 Mode).



generate_rollback_esi() ->
    try do_generate_rollback_esi()
    catch T:E ->
	    erlang:T(E)
    after
	case get(fd) of
	    Fd when is_pid(Fd) -> file:close(Fd);
	    undefined          -> ok
	end
    end.


do_generate_rollback_esi() ->
    TS       = comsaI:iso_time(os:timestamp(), basic),
    Root     = sysEnv:rcs_root(),
    RbEsiDir = filename:join([Root, "rcs", "erlang_disk", "rollback"]),

    LogFileName   = "rollback_esi.log",
    LogFile       = filename:join(RbEsiDir, LogFileName),
    RbEsiFileName = "rollback_esi.tar.gz",
    RbEsiFile     = filename:join(RbEsiDir, RbEsiFileName),
    ok            = filelib:ensure_dir(LogFile),
    {ok, LogFd}   = file:open(LogFile, [write]),
    put(fd, LogFd),
    io:format(LogFd, "Generating Rollback ESI : ~s~n~n", [TS]),
    %%=======================================================
    %% Invoke Rollback ESI callbacks
    %% print a warning if the callbacks take very long time 
    %%=======================================================
    {ok, Ref} = timer:apply_after(?WARNING_ESI_CB_TIME * 1000, 
				  logEsiLib, 
				  print_esi_time_warning,
				  []),
    case do_generate_rollback_esi_cb(LogFd) of
	{ok,Results} ->
	    file:close(LogFd),
	    erase(fd),
	    Paths = [" "++Path ++" " || {_,_,Path} <- lists:flatten(Results)],
	    sysInitI:info_msg("Creating rollback ESI tar file, result  ~p, ~n",
		      [cmd("tar --ignore-failed-read  "
			   ++ " -cvzf "
			   ++ RbEsiFile ++ " " ++ Paths ++ LogFile)]);
	Error ->
	    sysInitI:info_msg("Failed to create rollback ESI tar file, "
			      "result  ~p, ~n",[Error])
    end,
    timer:cancel(Ref),
    ok.

%%=======================================================================
%% generate_esi() ->
%%   {Node, Type, CompEsiFile} | 
%%   {Node, Type, CompEsiFile, Chunks} |
%%   {error, Reason}
%%=======================================================================
generate_esi(true, _EsiDir, MpId, Granularity, _Mode) 
  when Granularity =/= static ->
%%    io:format(LogFd, "No callbacks called due to shortage of disk~n",  []),
    {error, "FAILURE: Warm restart ongoing. MpId: " ++ MpId};
generate_esi(_, EsiDir, MpId, Granularity, Mode) ->
    TS = comsaI:iso_time(os:timestamp(), basic),
    {LogFileName, EsiFileName} = 
	case sysEnv:vrcs() of
	    false ->
		%% Could probably use env HOST here but to be 
		%% on the safe side the old way is kept.
		{"esi.du" ++ integer_to_list(MpId) ++ "." ++ TS ++ ".log",
		 "esi.du" ++ integer_to_list(MpId) ++ "." ++ TS ++ ".tar.gz"};
	    true ->
		Host = case os:getenv("HOST") of
			   false -> "vrcs";
			   ""    -> "vrcs";
			   H     -> H		    
		       end,
		{"esi." ++ Host ++ "." ++ TS ++ ".log",
		 "esi." ++ Host ++ "." ++ TS ++ ".tar.gz"}
	end,
    
    LogFile = filename:join(EsiDir, LogFileName),
    EsiFile = filename:join(EsiDir, EsiFileName),
    cmd(["rm -rf ", EsiDir]),
    ok = filelib:ensure_dir(LogFile),
    {ok, LogFd} = file:open(LogFile, [write]),
    put(fd, LogFd),
    io:format(LogFd, "Generating Ericsson Support information: ~s~n~n", [TS]),

    logEsiLib:print_granularity(LogFd, Granularity),

    %%=======================================================
    %% Invoke ESI callbacks
    %% print a warning if the callbacks take very long time 
    %%=======================================================
    {ok, Ref} = timer:apply_after(?WARNING_ESI_CB_TIME * 1000, 
				  logEsiLib, 
				  print_esi_time_warning,
				  []),
    do_generate_esi_cb(Granularity, 
		       logEsiLib:check_disk_space(),
		       LogFd),
    timer:cancel(Ref),

    %%=======================================================
    %% Limit directories for granularity=small
    %%=======================================================
    [{logEsi, dir, Dirs}] = mnesia:dirty_read({logEsi, dir}),
    AllDirs = logEsiLib:limit_dirs(LogFd, Granularity, Dirs),

    %% Information: 
    %% For WR6 dus32 cannot allocate all ram and trying using ram less 
    %% than 200Mb causes node to reboot. Due to this 250MB was used as
    %% a safety margin.
    %% WR8 is not having this problem for dus32 so this safety margin 
    %% is now removed.
    FreeTmpSize = logEsiLib:get_free_tmp(),
    
    %%=======================================================
    %% Check /tmp and free RAM memory and compared 
    %% with size of ESI-files (not compressed)
    %%=======================================================
    EsiSize = logEsiLib:get_size_dirs(AllDirs, 0) / 1024, %% MB

    case {FreeTmpSize, Mode} of
	{Size, _Mode} when Size - EsiSize > 50 ->
	    %% More than 50MB left
	    %% Mode = any | complete
	    io:format(LogFd, "All directories are included in the ESI~n", []),
            logEsiLib:print_listing(LogFd, AllDirs),
	    file:close(LogFd),
	    CompEsiFile = logEsiLib:pack_and_compress_dirs(AllDirs, 
							   EsiFile, 
							   LogFile, 
							   _InclErlgDir = true),
	    {clhI:erlang_node(), ?ESI_VSN_FULL, CompEsiFile};

	{Size, any} when Size >= 150 ->
	    %% Size at least 150MB to be allowed to do 100MB chunk 
	    %% (gives 50MB left).
	    %% Chunk only allowed for Mode = any.
	    AllChunks = [FirstChunkDirs | T] =
		logEsiLib:get_chunks(AllDirs, LogFd, 0, [], []),
	    io:format(LogFd,
		      "ESI is larger than available /tmp disk. "
		      "ESI is chunked in ~p pieces~n~n",
		      [logEsiLib:count(AllChunks, 0)]),
	    logEsiLib:print_chunks(LogFd, AllChunks, 1),
	    file:close(LogFd),
	    CompEsiFile = logEsiLib:pack_and_compress_dirs(FirstChunkDirs, 
							   EsiFile, 
							   LogFile, 
							   _InclErlDir = true),
	    {clhI:erlang_node(), ?ESI_VSN_CHUNK, CompEsiFile, T};

	{Size, any} when Size =< 50 ->
	    %% Size=0-50
	    %% Both dus32/dus52 with WR8 is unusable when going below ~50MB.
	    warning_msg("No ESI is generated/exported. Not enough RAM ~pM~n",
			[Size]),
	    {clhI:erlang_node(), none};
	
	{Size, any} when Size =< 75 ->
	    %% Size=51-75
	    %% Shortage of RAM. Only list esi-files
	    io:format(LogFd,
		      "There is only ~pM free RAM or /tmp disk.\n\n",
		      [Size]),
	    io:format(LogFd, "No files is included in the ESI.\n", []),
	    logEsiLib:print_top(LogFd),
	    logEsiLib:print_tmp(LogFd),
	    io:format(LogFd, "Listing of all ESI-files.\n", []),
	    logEsiLib:print_listing(LogFd, AllDirs),
	    file:close(LogFd),
	    CompEsiFile = logEsiLib:pack_and_compress_dirs([],
							   EsiFile, 
							   LogFile, 
							   _InclErlDir = false),
	    {clhI:erlang_node(), ?ESI_VSN_LIMITED, CompEsiFile};

	{Size, any} ->
	    %% Size=76-149
	    %% Shortage of RAM; only pack the most important files
	    io:format(LogFd, 
		      "There is only " ++ 
		      integer_to_list(Size) ++
		      "M free RAM or /tmp disk.\n",
		      []),
	    io:format(LogFd,
		      "Only selected files is included in the ESI.\n\n", 
		      []),
	    logEsiLib:print_top(LogFd),
	    logEsiLib:print_tmp(LogFd),
	    io:format(LogFd, "Listing of all ESI-files.\n", []),
	    logEsiLib:print_listing(LogFd, AllDirs),
	    file:close(LogFd),
	    CompEsiFile = logEsiLib:pack_and_compress_dirs([],
							   EsiFile, 
							   LogFile,
							   _InclErlDir = true),
	    {clhI:erlang_node(), ?ESI_VSN_LIMITED, CompEsiFile};

	{Size, complete} ->
	    %% EsiSize does not fit Size (RAM) left.
	    warning_msg("No ESI since ESI doesn't fit available RAM ~pM~n",
			[Size]),
	    {clhI:erlang_node(), none}
	end.


%%=======================================================================
%% Invoke esi callbacks 
%%=======================================================================
do_generate_esi_cb(static, ok, LogFd) ->    
    %% No callbacks are run in static case
    [{logEsi, cb, Callbacks}] = mnesia:dirty_read({logEsi, cb}),
    Modules = [Mod || {Mod, _} <- Callbacks],
    io:format(LogFd,
	      "~n~p generate_esi. "
	      "These callback modules are not invoked "
	      "because granularity is 'static'~n ~p~n~n", 
	      [time(), Modules]);
do_generate_esi_cb(_, ok, LogFd) ->    
    %% Run callbacks
    [{logEsi, cb, Callbacks}] = mnesia:dirty_read({logEsi, cb}),

    io:format(LogFd, 
	      "~n~p generate_esi callback modules = ~p~n", 
	      [time(), Callbacks]),

    AllModules = [M || {M, _} <- Callbacks],
    Modules = [X || X <- AllModules,
		    lists:member({generate_esi, 0},
				 apply(X, module_info, [exports]))],
    % it is trusted that the Callbacks list cannot be empty
    TimeOut = lists:max([T || {_, T} <- Callbacks]),
    Funs = [{?MODULE, 
	     generate_cb_fun,
	     [Module, generate_esi, LogFd]} || Module <- Modules],
    Opts = [{timeout, TimeOut}, {results_order, chrono_reverse}],
    Result = sysUtil:parallel_call(Funs, Opts),
    io:format(LogFd, 
	      "~n~p generate_esi callback results = ~p~n", 
	      [time(), Result]);

%% No callbacks are run and mnesia is not dumped 
%% if the /rcs disk is full or almost full.
do_generate_esi_cb(_, {nok, _RcsSize}, LogFd) ->
    %% No callbacks and no mnesia dump
    io:format(LogFd, "No callbacks called due to shortage of disk~n",  []),
    io:format(LogFd, "No mnesia dump taken due to shortage of disk~n", []).

    

%%=======================================================================
%% Invoke rollback_esi callbacks 
%%=======================================================================
do_generate_rollback_esi_cb(LogFd) ->    
    %% Run callbacks
    [{logEsi, cb, Callbacks}] = mnesia:dirty_read({logEsi, cb}),

    AllModules = [M || {M, _} <- Callbacks],
    Modules = [X || X <- AllModules, 
		    lists:member({generate_rollback_esi,0},
				 apply(X, module_info, [exports]))],
    io:format(LogFd, 
	      "~n~p generate_rollback_esi callback modules = ~p~n", 
	      [time(), Modules]),
    TimeOut = lists:max([T || {_, T} <- Callbacks]),
    Funs    = [{?MODULE, 
		generate_cb_fun, 
		[Module, generate_rollback_esi, LogFd]} || Module <- Modules],
    Opts    = [{timeout, TimeOut}, {results_order, chrono_reverse}],
    Result  = sysUtil:parallel_call(Funs, Opts),
    io:format(LogFd,
	      "~n~p generate_rollback_esi callback results = ~p~n", 
	      [time(), Result]),
    Result.


%%=======================================================================
%% Invoke esi callback fun without logging
%%=======================================================================
generate_cb_fun(Module, Func) ->
    case lists:member({Func, 0}, apply(Module, module_info, [exports])) of
	true ->
	    try apply(Module, Func, []) of
		Result ->
		    Result
	    catch T:E ->
		    ST = erlang:get_stacktrace(),
		    sysInitI:error_report([{T,E}, ST])
	    end;
	false ->
	    ok
    end.

%%=======================================================================
%% Invoke esi callback fun
%%=======================================================================
generate_cb_fun(Module, Func, LogFd) ->
    case lists:member({Func, 0}, apply(Module, module_info, [exports])) of
	true ->
	    io:format(LogFd,
		      "~n~p Calling ~w:~w()~n",
		      [time(), Module, Func]),
	    try apply(Module, Func, []) of
		Result ->
		    io:format(LogFd,
			      "~n~p Result ~w:~w() = ~p~n",
			      [time(), Module, Func, Result]),
		    Result
	    catch T:E ->
		    ST = erlang:get_stacktrace(),
		    io:format(LogFd,
			      "~n~p Error ~w:~w() = ~p~n~p~n", 
			      [time(), Module, Func, {T,E}, ST]),
		    sysInitI:error_report([{T,E}, ST])
	    end;
	false ->
	    io:format(LogFd,
		      "~n~p Callback function not exported ~w:~w()~n",
		      [time(), Module, Func]),
	    ok
    end.
    




%%========================================================================
%% call to os:cmd
%%========================================================================
cmd(Cmd) ->
    os:cmd(Cmd).



%%========================================================================
%% handle_fail
%%========================================================================
handle_fail(ProgressInfo) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    logEsiLib:update_progress([{result, ?ActionResultType_FAILURE},
			       {progressPercentage, 100},
			       {additionalInfo, ProgressInfo},
			       {state, ?ActionStateType_FINISHED},
			       {timeActionCompleted, CompleteTime}]).


%%% ----------------------------------------------------------
%%% sysInitI handling
%%% ----------------------------------------------------------
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format, [?MODULE | Args]).




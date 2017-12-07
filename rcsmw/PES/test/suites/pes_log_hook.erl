%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_log_hook.erl %
%%% Author:	eolaand
%%% 
%%% Description: Start PES Log and logs links to the files for each test case.
%%%
%%% CTH dependency: pes_pei_proxy
%%%
%%% ----------------------------------------------------------
-module(pes_log_hook).
-id('Updated by CCase').
-vsn('/main/R3A/R5A/4').
-date('2016-04-28').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R3A/1    2015-05-19 eolaand     Created
%%% ----------------------------------------------------------

%% CT hook callback functions
-export([
	 id/1,
	 init/2,
	 pre_init_per_suite/3,
	 post_init_per_suite/4,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 post_end_per_suite/4,
	 on_tc_fail/3,
	 terminate/1
	]).


%% -compile([export_all]).

%%%===================================================================
%%% Include files
%%%===================================================================
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Macros and records
%%%===================================================================
-define(PROXY, pes_pei_proxy).
-define(PROXY_SERVER, ?PROXY).
-define(DEFAULT_PROXY_SERVER, ?PROXY).
-define(HOOK_PRIO, 10).
-define(LOG_DIR, pes_log_dir).
-define(LOG_PREFIX, "PESLog").
-define(MAX_SEVERITY, 9).
-define(DEFAULT_LOG_OPTS, [{size, 1000}, {maxNoFiles, 3}, {zip, false}]).
-define(DEFAULT_RCS_DIR, "/rcs").
-define(PES_LOG_NAME, "RcsPmEvents").
-define(DEFAULT_LOG_DIR, "log/" ++ ?PES_LOG_NAME).
%%-define(DEFAULT_TMP_DIR, "/tmp").
%%-define(TARGET_DEFAULT_LOG_DIR, "pes_log").
-define(LIB, pes_test_lib).
-define(DEBUG_MOD, pesDebug).

-record(cth_state, {
	  disabled = false,
	  opts = [], 
	  name = ?MODULE, 
	  proxy,
	  log_dir,
	  priv_dir
	 }).

%%%===================================================================
%%% Support functions
%%%===================================================================

%%%===================================================================
%%% CT Hooks callbacks
%%%===================================================================
%% @private
id(Opts) ->
    proplists:get_value(instance_name, Opts, ?MODULE).


%% @private
init(Id, Opts) ->
    Conf = ?LIB:get_ct_config(?MODULE),
    Disabled = proplists:get_value(disabled, Conf, false),
    ct:pal("~p disabled = ~p", [?MODULE, Disabled]),
    ct:pal("~w:init(~p, ~p)", [?MODULE, Id, Opts]),
    Name = lta(Id),
    Proxy = proplists:get_value(?PROXY_SERVER, Opts, ?DEFAULT_PROXY_SERVER),
    LogDir = proplists:get_value(?LOG_DIR, Opts, default_log_dir(Proxy)),
    CTHState = #cth_state{disabled = Disabled,
			  name = Name, 
			  proxy = Proxy,
			  log_dir = LogDir,
			  opts = Opts},
    {ok, CTHState, ?HOOK_PRIO}.


%% @private
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_suite(_Suite, Ret, CTHState)
  when CTHState#cth_state.disabled ->
    {Ret, CTHState};

pre_init_per_suite(_SuiteName, InitData, CTHState) ->
    PrivDir = ?config(priv_dir, InitData),
    NewCTHState = CTHState#cth_state{priv_dir = PrivDir},
    {InitData, NewCTHState}.


%% @private
post_init_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, CTHState}.


%% @private
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_testcase(_Suite, Ret, CTHState)
  when CTHState#cth_state.disabled ->
    {Ret, CTHState};

pre_init_per_testcase(_TC, Config, CTHState) ->
    stop_log(CTHState),
    delete_pm_log_files(CTHState),
    start_log(CTHState, Config),
    {Config, CTHState}.


%% @private
post_end_per_testcase(_TC, _Config, Return, CTHState) 
  when CTHState#cth_state.disabled ->
    {Return, CTHState};

post_end_per_testcase(_TC, Config, Return, CTHState) ->
    try
	stop_log(CTHState),
	log_pm_log_files(CTHState, Config)
%%	delete_pm_log_files(CTHState)
    catch _:E ->
	    ct:log(lightred,
		   "~p: Failed to clean up and log PES log files~n~p",
		   [?MODULE, E]),
	    ok
    end,
    {Return, CTHState}.


%% @private
post_end_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, CTHState}.


%% @private
on_tc_fail(_TC, _Reason, CTHState) ->
    CTHState.
    

%% @private
terminate(_CTHState) ->
    ok.

%%%===================================================================
%%% Init test environment
%%%===================================================================

%%%===================================================================
%%% lib functions
%%%===================================================================
default_log_dir(Proxy) ->
    case rpc_call(Proxy, sysEnv, rcs_dir, []) of
	[_|_] = RcsDir ->
	    filename:join(RcsDir, ?DEFAULT_LOG_DIR);
	_Other ->
	    ct:log(lightred, "~p: Failed to fetch rcs_dir from CS node: ~p~n", 
		   [?MODULE, _Other]), 
	    %% Default dir only valid on target
	    filename:join(?DEFAULT_RCS_DIR, ?DEFAULT_LOG_DIR)
    end.


%% default_log_dir(Proxy) ->
%%     case rct_safe_rpc_lib:is_target() of
%% 	true ->
%% 	    target_default_log_dir(Proxy);
%% 	_False ->
%% 	    undefined
%%     end.


%% target_default_log_dir(Proxy) ->
%%     case rpc_call(Proxy, sysEnv, tmp_dir, []) of
%% 	[_|_] = TmpDir ->
%% 	    filename:join(TmpDir, ?TARGET_DEFAULT_LOG_DIR);
%% 	_Other ->
%% 	    filename:join(?DEFAULT_TMP_DIR, ?TARGET_DEFAULT_LOG_DIR)
%%     end.
	    

get_file_dir(CTHState, Config) ->
    case catch ?config(priv_dir, Config) of
	FileDir when is_list(FileDir) ->
	    FileDir;
	_ ->
	    CTHState#cth_state.priv_dir
    end.


%% get_log_dir(#cth_state{log_dir = LogDir}, _Config) 
%%   when is_list(LogDir) ->
%%     LogDir;

%% get_log_dir(CTHState, Config) ->
%%     get_file_dir(CTHState, Config).



start_log(CTHState, _Config) ->
    LogDir = CTHState#cth_state.log_dir,
    ok = rpc_call(CTHState, logRamI, write_to_file, [?PES_LOG_NAME]), 
    {ok, Files} = rpc_call(CTHState, file, list_dir, [LogDir]),
    [ok = rpc_call(CTHState, file, delete, [filename:join(LogDir, File)]) ||
	File <- Files],
    ok = rpc_call(CTHState, logRamI, set_options, 
		  [?PES_LOG_NAME, ?DEFAULT_LOG_OPTS]), 
    ok = rpc_call(CTHState, logRamI, set_severity, 
		  [?PES_LOG_NAME, ?MAX_SEVERITY]).



%% start_log(CTHState, Config) ->
%%     LogDir = get_log_dir(CTHState, Config),
%%     rpc_call(CTHState, filelib, ensure_dir, [filename:join(LogDir, "dum")]), 
%%     rpc_call(CTHState, ?DEBUG_MOD, log_to_file, [start, LogDir]),    
%%     rpc_call(CTHState, ?DEBUG_MOD, log_to_file, [filter, {add, pes, [all]}]).


stop_log(CTHState) ->
    ok = rpc_call(CTHState, logRamI, write_to_file, [?PES_LOG_NAME]).
%%    rpc_call(CTHState, ?DEBUG_MOD, log_to_file, [stop]).    
    

log_pm_log_files(#cth_state{log_dir = LogDir} = CTHState, Config) 
  when is_list(LogDir) ->
    Proxy = CTHState#cth_state.proxy,
    case rpc_call(Proxy, file, list_dir, [LogDir]) of
	{ok, AllFiles} ->
	    Files = [File || ?PES_LOG_NAME ++ _ = File <- AllFiles],
	    ct:log("Fetch PES log files in ~s:~n~p~n", 
		   [LogDir, Files]),
	    FileDir = get_file_dir(CTHState, Config),
	    copy_pm_log_files(Proxy, LogDir, Files, FileDir),
	    log_link_pm_log_files(Files, FileDir);
	Error ->
	    ct:log("Can't list PES log files in ~s: ~p~n", [LogDir, Error])
    end;

log_pm_log_files(CTHState, Config) ->
    FileDir = get_file_dir(CTHState, Config),
    case file:list_dir(FileDir) of
	{ok, AllFiles} ->
	    Files = [File || ?LOG_PREFIX ++ _ = File <- AllFiles],
	    log_link_pm_log_files(Files, FileDir);
	Error ->
	    ct:log("Can't list PES log files in ~s: ~p~n", [FileDir, Error])
    end.


copy_pm_log_files(Proxy, LogDir, Files, FileDir) ->
    F = fun(FileName) ->
		case rpc_call(Proxy, file, read_file, 
			      [filename:join(LogDir, FileName)]) of
		    {ok, FileCont} ->
			file:write_file(filename:join(FileDir, FileName), 
					FileCont);
		    Error ->
			ct:log("Can't read PES log files in ~s: ~p~n", 
			       [LogDir, Error])
		end
	end,
    lists:foreach(F, Files).

							
log_link_pm_log_files(Files, FileDir) when is_list(FileDir) ->
    {ok, RelPath} = ?LIB:relative_file_path(FileDir),
    F = fun(FileName, {F, D}) ->
		{"~n<a href=\"~s\">~s</a>" ++ F, 
		 [filename:join(RelPath, FileName), FileName | D]}
	end,
    {Format, OutData} = lists:foldl(F, {[], []}, lists:reverse(Files)),
    ct:log("PES Log files:" ++ Format, OutData);

log_link_pm_log_files(Files, FileDir) ->
    ct:pal("Can't log PES Log files ~p~n, priv_dir is ~p", [Files, FileDir]),
    ok.
    

delete_pm_log_files(#cth_state{log_dir = LogDir, proxy = Proxy}) 
  when is_list(LogDir) ->
    case rpc_call(Proxy, file, list_dir, [LogDir]) of
	{ok, Files} ->
	    ct:log("Deleting PES log files in ~s:~n~p~n", 
		   [LogDir, Files]),
	    [rpc_call(Proxy, file, delete, [filename:join(LogDir, File)]) ||
		File <- Files];
	Error ->
	    ct:log("Can't delete PES log files in ~s: ~p~n", [LogDir, Error]),
	    ok
    end;

delete_pm_log_files(_CTHState) ->
    ok.


rpc_call(#cth_state{proxy = Proxy}, M, F, A) ->
    rpc_call(Proxy, M, F, A);

rpc_call(Proxy, M, F, A) when is_atom(Proxy) ->
    ?PROXY:cs_node_call(Proxy, M, F, A).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.


%% atl(A) when is_atom(A) ->
%%     atom_to_list(A);
%% atl(L) when is_list(L) ->
%%     L.


%%     case ct:get_config(Par) of
%% 	undefined ->
%% 	    ct:log(lightred,
%% 		   "~p ~p Could not read config parameter ~p for "
%% 		   "pes_log_hook, Reason: undefined",
%% 		   [?MODULE, get_config, Par]),
%% 	    throw({?MODULE, {{fail, {undefined, Par}}}});
%% 	Val ->	    
%% 	    Val
%%     end.



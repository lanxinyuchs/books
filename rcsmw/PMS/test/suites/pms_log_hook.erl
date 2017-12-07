%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_log_hook.erl %
%%% Author:	eolaand
%%% 
%%% Description: Start PMS Log and logs links to the files in each test case.
%%%
%%% CTH dependency: pms_pmi_proxy
%%%
%%% ----------------------------------------------------------
-module(pms_log_hook).
-id('Updated by CCase').
-vsn('/main/R3A/R5A/R6A/R11A/1').
-date('2017-10-20').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R3A/1    2015-05-05 eolaand     Created
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

%%%===================================================================
%%% Include files
%%%===================================================================
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Macros and records
%%%===================================================================
-define(PROXY_SERVER, pms_pmi_proxy).
-define(HOOK_PRIO, 10).
-define(LOG_DIR, pms_log_dir).
-define(SEVERITY, severity).
-define(SIZE, size).
-define(MAX_NO_FILES, maxNoFiles).
-define(ZIP, zip).

%% -define(DEFAULT_TMP_DIR, "/tmp").
%% -define(TARGET_DEFAULT_LOG_DIR, "pms_log").
-define(DEFAULT_RCS_DIR, "/rcs").
-define(PMS_LOG_NAME, "RcsPmCounters").
-define(DEFAULT_LOG_DIR, "log/" ++ ?PMS_LOG_NAME).
-define(LIB, pms2_test_lib).
-define(MAX_SEVERITY, 9).
-define(DEFAULT_SIZE, 1000).
-define(DEFAULT_NO_FILES, 3).
-define(DEFAULT_ZIP, false).
-define(DEFAULT_LOG_OPTS, 
	[{size, 1000}, 
	 {maxNoFiles, 3}, 
	 {zip, false}]).

-record(cth_state, {
	  disabled = false,
	  opts = [], 
	  name = ?MODULE, 
	  pms_pmi_proxy,
	  pms_log_dir,
	  severity,
	  size,
	  maxNoFiles,
	  zip, 
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
    Conf = pms2_test_lib:get_ct_config(pms_log_hook, []),
    Disabled = proplists:get_value(disabled, Conf, false),
    ct:pal("~p disabled = ~p", [?MODULE, Disabled]),
    ct:pal("~w:init(~p, ~p)", [?MODULE, Id, Opts]),
    Name = lta(Id),
    Proxy = proplists:get_value(?PROXY_SERVER, Opts, ?PROXY_SERVER),
    LogDir = proplists:get_value(?LOG_DIR, Opts, default_log_dir(Proxy)),
    Sev = proplists:get_value(?SEVERITY, Opts, ?MAX_SEVERITY),
    Size = proplists:get_value(?SIZE, Opts, ?DEFAULT_SIZE),
    NFiles = proplists:get_value(?MAX_NO_FILES, Opts, ?DEFAULT_NO_FILES),
    Zip = proplists:get_value(?ZIP, Opts, ?DEFAULT_ZIP),
    CTHState = #cth_state{disabled = Disabled,
			  name = Name, 
			  pms_pmi_proxy = Proxy,
			  pms_log_dir = LogDir,
			  severity = Sev,
			  maxNoFiles = NFiles,
			  size = Size,
			  zip = Zip,
			  opts = Opts},
    {ok, CTHState, ?HOOK_PRIO}.


%% @private
pre_init_per_suite(_Suite, Ret, #cth_state{disabled = true} = CTHState) ->
    {Ret, CTHState};
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_suite(_SuiteName, InitData, CTHState) ->
    PrivDir = ?config(priv_dir, InitData),
    NewCTHState = CTHState#cth_state{priv_dir = PrivDir},
    {InitData, NewCTHState}.


%% @private
post_init_per_suite(_SuiteName, _Config, Return, CTHState) ->
    {Return, CTHState}.


%% @private
pre_init_per_testcase(_Suite, Ret, #cth_state{disabled = true} = CTHState) ->
    {Ret, CTHState};
pre_init_per_testcase(_TC, {SkipOrFail, _Reason} = Ret, CTHState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CTHState};

pre_init_per_testcase(_TC, Config, CTHState) ->
    stop_pms_log(CTHState),
    delete_pms_log_files(CTHState),
    start_pms_log(CTHState, Config),
    {Config, CTHState}.

%% @private
%% post_end_per_testcase(TC, Config, {SkipOrFail, _Reason} = Ret, CTHState)
%%   when SkipOrFail =:= skip; SkipOrFail =:= fail ->
%%     catch log_te_log(TC, Config, CTHState),
%%     {Ret, CTHState};

post_end_per_testcase(_TC, _Config, Return, #cth_state{disabled = true} = CTHState) ->
    {Return, CTHState};
post_end_per_testcase(_TC, Config, Return, CTHState) ->
    try
	stop_pms_log(CTHState),
	log_pms_log_files(CTHState, Config)
	%% delete_pms_log_files(CTHState)
    catch _:E ->
	    ct:log(lightred,
		   "~p: Failed to clean up and log PMS log files~n~p",
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


%% get_log_dir(#cth_state{pms_log_dir = LogDir}, _Config) 
%%   when is_list(LogDir) ->
%%     LogDir;

%% get_log_dir(CTHState, Config) ->
%%     get_file_dir(CTHState, Config).


start_pms_log(CTHState, _Config) ->
    LogDir = CTHState#cth_state.pms_log_dir,
    ok = rpc_call(CTHState, logRamI, write_to_file, [?PMS_LOG_NAME]), 
    {ok, Files} = rpc_call(CTHState, file, list_dir, [LogDir]),
    [ok = rpc_call(CTHState, file, delete, [filename:join(LogDir, File)]) ||
	File <- Files],
    ok = rpc_call(CTHState, logRamI, set_options, 
		  [?PMS_LOG_NAME, log_options(CTHState)]), 
    ok = rpc_call(CTHState, logRamI, set_severity, 
		  [?PMS_LOG_NAME, CTHState#cth_state.severity]).


log_options(#cth_state{size = Size,
		       maxNoFiles = MaxNoFiles,
		       zip = Zip}) ->
    [{size, Size}, 
     {maxNoFiles, MaxNoFiles}, 
     {zip, Zip}].


stop_pms_log(CTHState) ->
    ok = rpc_call(CTHState, logRamI, write_to_file, [?PMS_LOG_NAME]).
    %% rpc_call(CTHState, pmsDebug, log_to_file, [stop]).    
    

log_pms_log_files(#cth_state{pms_log_dir = LogDir} = CTHState, Config) 
  when is_list(LogDir) ->
    Proxy = CTHState#cth_state.pms_pmi_proxy,
    case rpc_call(Proxy, file, list_dir, [LogDir]) of
	{ok, AllFiles} ->
	    Files = [File || ?PMS_LOG_NAME ++"_" ++ _ = File <- AllFiles],
	    ct:log("Fetch PMS log files in ~s:~n~p~n", 
		   [LogDir, Files]),
	    FileDir = get_file_dir(CTHState, Config),
	    copy_pms_log_files(Proxy, LogDir, Files, FileDir),
	    log_link_pms_log_files(Files, FileDir);
	Error ->
	    ct:log("Can't list PMS log files in ~s: ~p~n", [LogDir, Error])
    end;

log_pms_log_files(CTHState, Config) ->
    FileDir = get_file_dir(CTHState, Config),
    case file:list_dir(FileDir) of
	{ok, AllFiles} ->
	    Files = [File || "PMSLog" ++ _ = File <- AllFiles],
	    log_link_pms_log_files(Files, FileDir);
	Error ->
	    ct:log("Can't list PMS log files in ~s: ~p~n", [FileDir, Error])
    end.


copy_pms_log_files(Proxy, LogDir, Files, FileDir) ->
    F = fun(FileName) ->
		case rpc_call(Proxy, file, read_file, 
			      [filename:join(LogDir, FileName)]) of
		    {ok, FileCont} ->
			file:write_file(filename:join(FileDir, FileName), 
					FileCont);
		    Error ->
			ct:log("Can't read PMS log files in ~s: ~p~n", 
			       [LogDir, Error])
		end
	end,
    lists:foreach(F, Files).

							
log_link_pms_log_files(Files, FileDir) when is_list(FileDir) ->
    {ok, RelPath} = ?LIB:relative_file_path(FileDir),
    F = fun(FileName, {F, D}) ->
		{"~n<a href=\"~s\">~s</a>" ++ F, 
		 [filename:join(RelPath, FileName), FileName | D]}
	end,
    {Format, OutData} = lists:foldl(F, {[], []}, lists:reverse(Files)),
    ct:log("PMS Log files:" ++ Format, OutData);

log_link_pms_log_files(Files, FileDir) ->
    ct:pal("Can't log PMS Log files ~p~n, priv_dir is ~p", [Files, FileDir]),
    ok.
    

delete_pms_log_files(#cth_state{pms_log_dir = LogDir, pms_pmi_proxy = Proxy}) 
  when is_list(LogDir) ->
    case rpc_call(Proxy, file, list_dir, [LogDir]) of
	{ok, Files} ->
	    ct:log("Deleting PMS log files in ~s:~n~p~n", 
		   [LogDir, Files]),
	    [rpc_call(Proxy, file, delete, [filename:join(LogDir, File)]) ||
		File <- Files];
	Error ->
	    ct:log("Can't delete PMS log files in ~s: ~p~n", [LogDir, Error]),
	    ok
    end;

delete_pms_log_files(_CTHState) ->
    ok.


rpc_call(#cth_state{pms_pmi_proxy = Proxy}, M, F, A) ->
    rpc_call(Proxy, M, F, A);

rpc_call(Proxy, M, F, A) when is_atom(Proxy) ->
    pms_pmi_proxy:cs_node_call(Proxy, M, F, A).


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
%% 		   "pms_log_hook, Reason: undefined",
%% 		   [?MODULE, get_config, Par]),
%% 	    throw({?MODULE, {{fail, {undefined, Par}}}});
%% 	Val ->	    
%% 	    Val
%%     end.



%%% ----------------------------------------------------------
%%% %CCaseFile:	oot_log_hook.erl %
%%% Author:	eolaand
%%% 
%%% Description: Monitor changes in OOT Log and T&E log and copies the changes 
%%% to the private directory of tyhe test, as well as linking them from the 
%%% test result webpage.
%%%
%%% CTH dependency: rct_rpc
%%%
%%% ----------------------------------------------------------
-module(oot_log_hook).
-id('Updated by CCase').
-vsn('/main/R11A/9').
-date('2017-09-26').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R11/0    2017-07-14 egabing     Created
%%% R11/4    2017-08-01 eolaand     Fix indentation and some bugs
%%% R11/5    2017-08-22 egabing     Made functionality optional
%%% ----------------------------------------------------------

%% CT hook callback functions
-export([
	 init/2,
 	 pre_init_per_testcase/3,
  	 pre_init_per_suite/3,
	 post_end_per_testcase/4
	]).

%%%===================================================================
%%% Include files
%%%===================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%%===================================================================
%%% Macros and records
%%%===================================================================

-define(HOOK_PRIO, 1000000).

%% Path to OOT log folder from rcs root
-define(OOT_LOG_SOURCE_PATH, "/log/OotLog/"). 
%% Prefix of generated log file REMOVE
-define(OOT_LOG_FILE_PREFIX, "OotLog"). 	  
%% Prefix of generated log file
-define(TE_LOG_FILE_PREFIX, "TE_LOG").		  
%% File extension of generated log files
-define(LOG_FILE_FILE_ENDING, ".txt").		 
%% Timeout of rct_rpc-calls 
-define(RCT_RPC_CALL_TIMEOUT, 5000).		 

-record(cth_state, {
	  ootLogFile,
	  active = false,
	  nodeId = []
	 }).


%%%===================================================================
%%% CT Hooks callbacks
%%%===================================================================

%% @private
init(_Id, Opts) ->
    CTHState = #cth_state{nodeId = proplists:get_value(nodeId, Opts, rpc), 
                          active = is_oot_log_hook_active(Opts)},
    {ok, CTHState, ?HOOK_PRIO}.

%% @private
pre_init_per_suite(_Suite, Config, #cth_state{active = true} = CTHState) ->
    case initiate_logs(CTHState) of
        {ok, NewCTHState} ->
            {Config, NewCTHState};
        {error, Reason} ->
            {{skip, Reason}, CTHState}
    end;
    
pre_init_per_suite(_Suite, Config, CTHState) -> 
    {Config, CTHState}.
    

%% @private
pre_init_per_testcase(_Suite, Config, #cth_state{active = true} = CTHState) ->
    case start_logs(CTHState) of
        ok ->
            {Config, CTHState};
        {error, Reason} ->
            {{skip, Reason}, CTHState}
    end;

pre_init_per_testcase(_Suite, Config, CTHState) -> {Config, CTHState}.

%% @private
post_end_per_testcase(Suite, Config, Return, CTHState) 
  when CTHState#cth_state.active ->
    case end_logs(Suite, Config, CTHState) of
        {ok, NewCTHState} ->
            {Return, NewCTHState};
        {error, Reason} ->
            {{fail, Reason}, CTHState}
    end;
    
post_end_per_testcase(_Suite, _Config, Return, CTHState) -> 
    {Return, CTHState}.

%%%===================================================================
%%% Support functions
%%%===================================================================
initiate_logs(CTHState) ->
    case oot_log_init(CTHState) of
	{error, Reason} ->
	    {error, Reason};
	{ok, NewCTHState} ->
	    initiate_logs2(NewCTHState)
    end.


initiate_logs2(NewCTHState) ->
    case te_log_init(NewCTHState) of
	{error, Reason} ->
	    {error, Reason};
	{ok, NewCTHState2} ->
	    {ok, NewCTHState2}
    end.


%% Initiate OOT log tracking
oot_log_init(#cth_state{nodeId = NodeId} = CTHState) ->
    %% Get RCS root directory
    case rpc_call(NodeId, sysEnv, vnf_dir, []) of
        {ErrTag, Reason} when ErrTag =:= badrpc; ErrTag =:= error ->
            ct:pal("Error getting RCS root directory, ~p", [Reason]),
            {error, "Error getting RCS root directory"};
        Res ->
            RcsRootDir = Res,
	    OotLogFile = RcsRootDir ++ ?OOT_LOG_SOURCE_PATH ++ "OotLog.1",
	    {ok, CTHState#cth_state{ootLogFile = OotLogFile}}
    end.


start_logs(CTHState) ->
    case oot_log_start(CTHState) of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    start_logs2(CTHState)
    end.


%% Start OOT log tracking
oot_log_start(#cth_state{nodeId = NodeId}) ->
    {ok, ModuleList} = oot_module_versions(NodeId),
    case rpc_call(NodeId, disk_log, btruncate, 
		  ["OotLog", list_to_binary(ModuleList)]) of
        {ErrTag, Reason} when ErrTag =:= badrpc; ErrTag =:= error ->
            ct:pal("Error truncating file, ~p", [Reason]),
            {error,"Error truncating file"};
        ok ->
            ok
    end.


start_logs2(CTHState) ->
    case te_log_start(CTHState) of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    ok
    end.


%% Initiate T&E log tracking
te_log_init(#cth_state{nodeId = NodeId} = CTHState) ->
    %% Try to fetch the log. The first time the log is fetched it will print 
    %% some extra information. As this makes the length calculations unreliable
    %% we disregard this first result.
    case rpc_call(NodeId, os, cmd, ["te log read"]) of
	{error, Reason} ->
            ct:pal("Error reading T&E log, ~p", [Reason]),
            {error, "Error clearing T&E log"};
        _->
            {ok, CTHState}
    end.


%% Start T&E log tracking
te_log_start(#cth_state{nodeId = NodeId}) ->
    case rpc_call(NodeId, os, cmd, ["te log clear"]) of
	{error, Reason} ->
	    ct:pal("Error clearing T&E log, ~p", [Reason]),
	    {error, "Error clearing T&E log"};
	_ ->
	    te_enable_debug_level(NodeId)
    end.


end_logs(Suite, Config, CTHState) ->
    %% Get T&E log path
    case te_log_end(Suite, Config, CTHState) of
	{error, Reason} ->
	    {error, Reason};
	{ok, TeLogPath} ->
	    end_logs(Suite, Config, CTHState, TeLogPath)
    end.


end_logs(Suite, Config, CTHState, TeLogPath) ->
    %% Get OOT log path
    case oot_log_end(Suite, Config, CTHState) of
	{error, Reason} ->
	    {error, Reason};
	{ok, OotLogPath} ->
	    %% Write table
	    write_log_table(TeLogPath, OotLogPath),
	    {ok, CTHState}
    end.


write_log_table(TeLogPath, OotLogPath) ->
    format_html("<h2>External logs:</h2>"),
    format_html("<table>"),
    format_html("<tr><td>Log</td><td>Link</td></tr>"),
    format_html("<tr><td>OotLog</td><td><a href=~p>Link</a></td></tr>",
		[OotLogPath]),
    format_html("<tr><td>T&E</td><td><a href=~p>Link</a></td></tr>", 
		[TeLogPath]),
    format_html("</table>").


%% End T&E log tracking
te_log_end(Suite, Config, #cth_state{nodeId = NodeId}) ->
    ct:log("Read TE log from Node~n", []),
    case rpc_call(NodeId, os, cmd, ["te log read"]) of
	{error, Reason} ->
	    ct:pal("Error reading TE log, ~p", [Reason]),
	    te_disable_debug_level(NodeId, unknown),
	    {error, "Error reading  T&E log"};
	Res ->
	    Log = Res,
	    %% Get privdir and calculate the log path in the file system
	    PrivDir = ?config(priv_dir, Config),
	    LogPath = PrivDir ++ ?TE_LOG_FILE_PREFIX ++ "_" 
		++ atom_to_list(Suite) ++ ?LOG_FILE_FILE_ENDING,
	    {ok, LogDestination} = te_print_to_log(LogPath, Log),
	    te_disable_debug_level(NodeId, LogDestination)
    end.

%% Print new Trace and Error log
te_print_to_log(LogPath, Log) ->
    case file:write_file(LogPath, Log) of
	ok ->
	    {ok, LogPath};
	{error, Reason} ->
	    ct:pal("Error writing new T&E log file, ~p", [Reason]),
	    {error, "Error writing new T&E log file"}
    end.

%% Enable debug log level for te
te_enable_debug_level(NodeId) ->
    case rpc_call(NodeId, os, cmd, 
		  ["te enable debug_trace com_ericsson_rcs_cstn"]) of
	{badrpc, Reason} ->
	    ct:pal("Error setting T&E log level, ~p", [Reason]),
	    {error, "Error setting T&E log level"};
        _ ->
            ok
    end.

%% Disable debug log level for te
te_disable_debug_level(NodeId, LogDestination) ->
    case rpc_call(NodeId, os, cmd, 
		  ["te disable debug_trace com_ericsson_rcs_cstn"]) of
        {error, Reason} ->
            ct:pal("Error disabling T&E log level, ~p", [Reason]),
            {error, "Error disabling T&E log level"};
        _ ->
            {ok, LogDestination}
    end.


%% End OOT log tracking
oot_log_end(Suite, Config, CTHState) ->
    NodeId = CTHState#cth_state.nodeId,
    OotLogFile = CTHState#cth_state.ootLogFile,
    timer:sleep(3000),
    ct:log("Read OotLog from Node~n", []),
    case rpc_call(NodeId, file, read_file, [OotLogFile]) of
	{ok, BinContents} ->
            Log = binary:bin_to_list(BinContents),
	    %% Get privdir and calculate the log path in the file system
	    PrivDir = ?config(priv_dir, Config),
	    LogPath = PrivDir ++ ?OOT_LOG_FILE_PREFIX ++ "_" 
		++ atom_to_list(Suite) ++ ?LOG_FILE_FILE_ENDING,
	    %% Print the log to generated log file
	    oot_print_to_log(LogPath, Log),
	    {ok, LogPath};

        {ErrTag, Reason} when ErrTag =:= badrpc; ErrTag =:= error ->
	    ct:pal("Error reading OOT log file, ~p", [Reason]),
	    {error, "Error reading OOT log file"}
    end.

%% Print module versions to log file
oot_module_versions(NodeId) ->
    %% Get versions of oot files from the node
    case rpc_call(NodeId, ootLib, get_versions, [oot]) of
        {ErrTag, Reason} when ErrTag =:= badrpc; ErrTag =:= error ->
	    %% If we cannot get the version list, print a message instead
            ct:pal("Error getting module versions, ~p", [Reason]),
            VersionList = "Unable to get module versions";
        Res ->
            VersionList = Res
    end,

    %% Indent lines where module versions are printed and format output
    IndentedVersionList = 
	lists:foldl(fun(X, Result) ->
			    case X of "\n" -> 
				    Result ++ [X,"\t"]; 
				_ -> Result ++ [X] 
			    end 
		    end, [], VersionList),
    FormattedList = 
	"\n==========================\n" 
	++ IndentedVersionList 
	++ "\n==========================\n",
    {ok, FormattedList}.

%% Print the OOT log to the new log file 
oot_print_to_log(LogPath, Log) ->
    case file:write_file(LogPath, Log) of
	ok ->
	    ok;
    	{error, Reason} ->
	    ct:pal("Error writing new OOT log file, ~p", [Reason]),
	    {error, "Error writing new OOT log file"}
    end.

%% Print HTML to log file webpage
format_html(String) ->
    ct:log(default, 1, String, [], [no_css]).

format_html(String,Args) ->
    ct:log(default, 1, String, Args, [no_css]).


rpc_call(NodeId, M, F, A) ->
    rpc_call(NodeId, M, F, A, ?RCT_RPC_CALL_TIMEOUT).


rpc_call(NodeId, M, F, A, Timeout) ->
    case rct_rpc:call( NodeId, M, F, A, Timeout, noprint) of
        {badrpc, Reason} = Err ->
            ct:pal("rpc:call failed: ~p", [Err]),
	    {error, Reason};
	Res ->
	    Res
    end.
    
    
%% Check if enable_oot_log_hook flag is true (if the test sutie is run with 
%% "-enable_oot_log_hook true"). Otherwise no action should be done
is_oot_log_hook_active(Opts) ->
    case init:get_argument(enable_oot_log_hook) of
	{ok, [[]]} ->
	    true;
	_Other ->
	    proplists:get_value(enabled, Opts, false)
    end.

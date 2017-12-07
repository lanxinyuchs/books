%%==============================================================================
%%  1.    BASIC INFORMATION
%%==============================================================================

%%==============================================================================
%% 1.1   MODULE INFORMATION
%%==============================================================================
%% %CCaseFile:	logWebIv1.erl %
%% @copyright Ericsson AB 2017
%% @version /main/R9A/R10A/R11A/2
%% 
%% @doc == REST API for LOG, version 1 ==
%% Interface module for REST API, specified in log_web.xml.
%%
%% @end
%%==============================================================================

%%==============================================================================
%% 1.2   MODULE DEFINITION
%%==============================================================================
-module(logWebIv1).
-vsn('/main/R9A/R10A/R11A/2').
-date('2017-08-09').

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
%% R9A/1-8   2017-03-28 uabesvi  Created.
%% R10A/1-6  2017-03-28 uabesvi  Create continued
%% R10A/1-6  2017-03-28 uabesvi  Create continued
%% R11A/2    2017-09-20 etxpejn  Added ContentType if missing in deliver
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
-export([export_upgrade_logs/3]).
-export([fetch_upgrade_logs/3]).

-export([export_esi/3]).
-export([fetch_esi/3]).
%%-export([import_esi/3]).

-export([eul_rc/3]).

-include("log.hrl").

%%==============================================================================
%% 2.2   EXPORTED INTERNAL FUNCTIONS
%%==============================================================================

%%==============================================================================
%% 2.3   IMPORT OF DEFINITIONS
%%==============================================================================

%%==============================================================================
%% 2.4   LOCAL DEFINITION OF MACROS
%%==============================================================================
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

-define(REP_INFO(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).

-define(LOG_WARN(__ReportInfo),
	sysInitI:warning_report(?REP_INFO(__ReportInfo))).

-define(LOG_REST_WARNING(__Reason),
	?LOG_WARN([{"REST Query", "Unexpected"},
		   __Reason,
		   {sessionID, SessionID},
		   {env, Env},
		   {body, Body}])).


-define(ERROR_NOT_SUPPORTED, "Request method not supported").

%% JSON

-define(JsonDescr(__Arg), ensure_str(__Arg)).
-define(JsonDescr_BadArg(__Arg),
	"Badly formatted argument: " ++ ensure_str(__Arg)).
-define(JsonDescr_MandArgMissing(__Arg),
	"Mandatory argument missing: " ++ ensure_str(__Arg)).

-define(JsonEndOfHeader, "\r\n\r\n").

-define(JsonResult_Props(__Props),
	jsone:encode({__Props},
		     [{space, 1}, {indent, 4}, {object_key_type, value}])).


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
%% This function is called from VNFM during upgrade of a VNFC.
%% The purpose is to collect the Upgrade Logs from the old VNFC
%% to be stored in the new VNFC to be able to trace what has
%% happened during the upgrade.
%%
%% @end
%%==============================================================================
-spec export_upgrade_logs(SessionID :: pid(),
			  Env       :: list({Property :: any(), 
					     Value :: any()}),
			  Body      :: string()) ->
    any().

export_upgrade_logs(SessionID, Env, Body) ->
    try 
	export_upgrade_logs(env_req(Env), SessionID, Env, Body)
    catch
	T:E ->
	    sysInitI:error_msg("~p:export_upgrade_logs ERROR ~p~n",
			       [?MODULE, {T, E}]),
	    Reply = [{info,   "Export upgrade logs failed"},
		     {result, ensure_bin("FAILURE")}],
	    deliver(SessionID, Env,  ?JsonResult_Props(Reply))    
    end.
			       

export_upgrade_logs("GET", SessionID, Env, Body) ->
    sysInitI:info_msg("~p: export_upgrade_logs~n", [?MODULE]),
    UpgrFile = logWeb:export_upgrade_logs(v1, SessionID, Env, Body),
    eul_rc(file:read_file(UpgrFile), SessionID, Env);
export_upgrade_logs(Request, SessionID, Env, Body) ->
    ?LOG_REST_WARNING(?ERROR_NOT_SUPPORTED),
    Reply = [{info,   "Should be GET but was " ++ Request},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).


eul_rc({ok, Bin}, SessionID, Env) ->
    sysInitI:info_msg("~p: export_upgrade_logs OK   Size ~p~n",
		      [?MODULE, size(Bin)]),
    deliver(SessionID, Env, Bin);


eul_rc({error, Error}, SessionID, Env) ->
    sysInitI:error_msg("~p: export_upgrade_logs Error: ~p ~n", [?MODULE, Error]),
    Reply = [{info,   ensure_str(Error)},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).



%%==============================================================================
%% @doc REST query for fetch upgrade logs
%% 
%% This function is called from VNFM during upgrade of a VNFC.
%% It is called in the new VNFC when the old VNFC is shutdown.
%% The Body contains the Upgrade Logs from the old VNFC.
%% These logs are stored in ?UPGR_FILE_PREFIX directory.
%% ?UPGR_DIRS_MAX number of upgraded logs are stored, the oldest
%% directory is removed when the max number of directories is reached.
%% 
%% NOTE: AVLI log from the old VNFC is compared with the AVLI log
%% in the new VNFC, any entries at the tail of old AVLI log will
%% be written to the new AVLI log, this to be abel to trace the
%% whole upgrade sequence.
%% 
%% NOTE 2: It is assumed that no entries are written to the new AVLI lgo
%% before this function is invoked. 
%% 
%% @end
%%==============================================================================
-spec fetch_upgrade_logs(SessionID :: pid(),
			  Env       :: list({Property :: any(),
					     Value :: any()}),
			  Body      :: string()) ->
    any().

fetch_upgrade_logs(SessionID, Env, Body) ->
    try
	fetch_upgrade_logs(env_req(Env), 
			   SessionID, 
			   Env,
			   Body)
    catch
	T:E ->
	    sysInitI:error_msg("~p:fetch_upgrade_logs ERROR ~p~n",
			       [?MODULE, {T, E}]),
	    Reply = [{info,   "Fetch upgrade logs failed"},
		     {result, ensure_bin("FAILURE")}],
	    deliver(SessionID, Env,  ?JsonResult_Props(Reply))    
    end.

fetch_upgrade_logs("GET", SessionID, Env, _Body) ->
    sysInitI:info_msg("~nfetch_upgrade_logs  Env ~n~p~n", [Env]),
    logWeb:cleanup_upgrade_dirs(),

    %%======================================================
    %% fetch the upgrade log file from VNFM
    %%======================================================
    ful(action(fetch_upgrade_logs, Env), SessionID, Env);
fetch_upgrade_logs(Request, SessionID, Env, Body) ->
    sysInitI:error_msg("~p: fetch_upgrade_logs failed. Request: ~p~n",
		       [?MODULE, Request]),
    ?LOG_REST_WARNING(?ERROR_NOT_SUPPORTED),
    Reply = [{info,   "Should be GET but was " ++ Request},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).



ful({ok, Bin}, SessionID, Env) ->
    log("~nfetch_upgrade_logs  Size ~n~p~n", [size(Bin)]),
    %% remove trailing /n 
    %% (don't know who adds the new line char)
    BodySize = size(Bin) - 1, 
    <<Body:BodySize/binary, _/binary>> = Bin,

    %%======================================================
    %% store the log files
    %%======================================================
    Dir      = ful_get_dir(),
    File     = ful_get_file(Dir),
    WriteRes = ful_write_file(File, Body),
    CopyRes  = logWeb:import_upgrade_logs(v1, 
					  WriteRes,
					  {Dir, File},
					  SessionID, 
					  Env),
    log("~p: fetch_upgrade_logs CopyRes: ~p~n", [?MODULE, CopyRes]),
    Reply = [{info,   ""},
	     {result, ensure_bin("SUCCESS")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply));
ful({error, Error}, SessionID, Env) ->    
    sysInitI:error_msg("~p: fetch upgrade logs Error: ~p ~n", [?MODULE, Error]),
    Reply = [{info,   ensure_str(Error)},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).




ful_write_file(File, Logs) ->
    ok = filelib:ensure_dir(File),
    log("~p: fetch_upgrade_logs File: ~p ~n", [?MODULE, File]),
    file:write_file(File, Logs).

ful_get_dir() ->
    filename:join([?UPGR_DIR, "logs_" ++ logWeb:get_time()]).

ful_get_file(Dir) ->
    log("~p: fetch_upgrade_logs Dir: ~p~n", [?MODULE, Dir]),
    filename:join([Dir, ?UPGR_FILE_PREFIX ++ ".tar.gz"]).


%%==============================================================================
%% @doc REST query for export ESI
%%
%% 
%% 
%% 
%% 
%%
%% @end
%%==============================================================================
-spec export_esi(SessionID :: pid(),
		 Env       :: list({Property :: any(), 
				    Value :: any()}),
		 Body      :: string()) ->
    any().

export_esi(SessionID, Env, Body) ->
    try 
	export_esi(env_req(Env), SessionID, Env, Body)
    catch
	T:E ->
	    sysInitI:error_msg("~p:export_esi ERROR ~p~n",
			       [?MODULE, {T, E}]),
	    Reply = [{info,   "Export ESI logs failed"},
		     {result, ensure_bin("FAILURE")}],
	    deliver(SessionID, Env,  ?JsonResult_Props(Reply))    
    end.

export_esi("GET", SessionID, Env, Body) ->
    sysInitI:info_msg("~p: export esi~n", [?MODULE]),
    EsiFile = logWeb:export_esi(v1, SessionID, Env, Body),
    ee_rc(file:read_file(EsiFile), SessionID, Env);
export_esi(Request, SessionID, Env, Body) ->
    ?LOG_REST_WARNING(?ERROR_NOT_SUPPORTED),
    Reply = [{info,   "Should be GET but was " ++ Request},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).


ee_rc({ok, Bin}, SessionID, Env) ->
    sysInitI:info_msg("~p: export esi OK ~n", [?MODULE]),
    deliver(SessionID, Env, Bin);
ee_rc({error, Error}, SessionID, Env) ->
    sysInitI:error_msg("~p: export esi Error: ~p ~n", [?MODULE, Error]),
    Reply = [{info,   ensure_str(Error)},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).


%%==============================================================================
%% @doc REST query for import esi
%% 
%% 
%% 
%% 
%% 
%% 
%% 
%% @end
%%==============================================================================
-spec fetch_esi(SessionID :: pid(),
		Env       :: list({Property :: any(),
				   Value :: any()}),
		Body      :: string()) ->
    any().

fetch_esi(SessionID, Env, Body) ->
    try
	fetch_esi(env_req(Env), 
		  SessionID, 
		  Env, 
		  Body)
    catch
	T:E ->
	    sysInitI:error_msg("~p:fetch_esi ERROR ~p~n",
			       [?MODULE, {T, E}]),
	    Reply = [{info,   "Fetch ESI failed"},
		     {result, ensure_bin("FAILURE")}],
	    deliver(SessionID, Env,  ?JsonResult_Props(Reply))    
    end.

fetch_esi("GET", SessionID, Env, _Body) ->
    sysInitI:info_msg("~p: fetch_esi ~n", [?MODULE]),
    logWeb:cleanup_esi_dirs(),

    %%======================================================
    %% fetch ESI file from VNFM
    %%======================================================
    fe(action(fetch_esi, Env), SessionID, Env);
fetch_esi(Request, SessionID, Env, Body) ->
    sysInitI:error_msg("~p: fetch_esi failed. Request: ~p~n",
		       [?MODULE, Request]),
    ?LOG_REST_WARNING(?ERROR_NOT_SUPPORTED),
    Reply = [{info,   "Should be GET but was " ++ Request},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).


fe({ok, Bin}, SessionID, Env) ->
    log("~nfetch_esi  Size ~n~p~n", [size(Bin)]),
    %% remove trailing /n 
    %% (don't know who adds the new line char)
    BodySize = size(Bin) - 1, 
    <<Body:BodySize/binary, _/binary>> = Bin,

    %%======================================================
    %% save the ESI file
    %%======================================================
    Dir      = fe_get_dir(),
    File     = fe_get_file(Dir),
    WriteRes = fe_write_file(File, Body),
    sysInitI:info_msg("~p: fetch_esi Result: ~p~n",
		      [?MODULE, WriteRes]),
    Reply = [{info,   ""},
	     {result, ensure_bin("SUCCESS")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply));
fe({error, Error}, SessionID, Env) ->    
    sysInitI:error_msg("~p: fetch_esi Error: ~p ~n", [?MODULE, Error]),
    Reply = [{info,   ensure_str(Error)},
	     {result, ensure_bin("FAILURE")}],
    deliver(SessionID, Env,  ?JsonResult_Props(Reply)).



fe_write_file(File, Logs) ->
    ok = filelib:ensure_dir(File),
    sysInitI:info_msg("~p: fetch_esi File: ~p ~n", [?MODULE, File]),
    file:write_file(File, Logs).

fe_get_dir() ->
    filename:join([?ESI_DIR, "esi_" ++ logWeb:get_time()]).

fe_get_file(Dir) ->
    filename:join([Dir, ?ESI_FILE_PREFIX ++ ".tar.gz"]).




%%==============================================================================
%% 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%==============================================================================



%%==============================================================================
%% 3.3   CODE FOR INTERNAL FUNCTIONS
%%==============================================================================


%%==============================================================================
%% JSON FUNCTIONS
%%==============================================================================

%%==============================================================================
%% env_req(Env)
%%==============================================================================
env_req(Env) ->
    proplists:get_value(request_method, Env, "").


%%==============================================================================
%% deliver(SessionID, Env, Body)
%%==============================================================================
deliver(SessionID, Env, Body) ->
    deliver(sysEnv:rcs_mode_2(), 
	    SessionID, 
	    proplists:get_value(http_content_type, Env),
	    Body).

%% deliver(simulated, _SessionID, _Env, Body) ->
%%     Body;
deliver(_, SessionID, undefined, Body) ->
    sysInitI:warning_msg("~p: ContentType undefined, add application/jsone ~n", [?MODULE]),
    Head = "Content-Type:application/json" ++ ?JsonEndOfHeader,    
    mod_esi:deliver(SessionID, [Head, Body, "\n"]);
deliver(_, SessionID, ContentType, Body) when is_list(ContentType) ->
    Head = "Content-Type: " ++ ContentType ++ ?JsonEndOfHeader,
    mod_esi:deliver(SessionID, [Head, Body, "\n"]).

%%==============================================================================
%% action(Action, Env) -> {ok, Bin}
%%==============================================================================
action(Action, Env) ->
    sysInitI:info_msg("~p action ~n~p ~n", [?MODULE, fetch_upgrade_logs]),
    VnfmIp      = vnfm_ip(),
    JsonUri     = vnfmLcmUri(VnfmIp, 
			     vnfcI:get_vnfm_server(),
			     get_action(Action)),
    Headers     = [],
    ContentType = proplists:get_value(http_content_type, Env),
    Body        = ?JsonResult_Props([{vnf_id, vnf_id()}]),

    Data  = {JsonUri, Headers, ContentType, Body},
    Maps1 = maps:new(),
    Maps2 = maps:put(data,   Data, Maps1),
    Maps3 = maps:put(method, post, Maps2),
    sysInitI:info_msg("~p vnfcHttps:send_receive(~p) ~n", [?MODULE, Maps3]),
    %% Using 'apply' to avoid dialyzer problem in G2.
    apply(vnfcHttps, send_receive, [Maps3]).


vnfmLcmUri(IpAddr, {ok, {_IpAddress, Port, _VnfId}}, Query) ->
    PortInt = integer_to_list(Port),
    "https://" ++ IpAddr ++ ":" ++ PortInt ++ "/eri_lcm_mani/v1/" ++ Query;
vnfmLcmUri(IpAddr, Error, Query) ->
    sysInit:warning_msg("~p: could not get vnfm port. Error ~p~n", [?MODULE, Error]),
    "http://" ++ IpAddr ++ ":" ++ "9998" ++ "/eri_lcm_mani/v1/" ++ Query.


get_action(fetch_upgrade_logs) -> "import_upgrade_logs";
get_action(fetch_esi)          -> "import_esi".
    

%%==============================================================================
%% 3.3.2 Help Functions
%%==============================================================================

ensure_bin(Term) when is_binary(Term) ->
    Term;
ensure_bin(Term) ->
    list_to_binary(ensure_str(Term)).

ensure_str(Term) ->
    catch sysUtil:term_to_string(Term).

vnfm_ip() ->
    %% Using 'apply' to avoid dialyzer problem in G2.
    {ok, {VnfmIp, _Port, _VnfId}} = apply(vnfcI, get_vnfm_server, []),
    VnfmIp.

vnf_id() ->
    %% Using 'apply' to avoid dialyzer problem in G2.
    ensure_bin([_ | _] = apply(vnfcI, vnf_id, [])).



%% used for debugging
log(_, _) ->
    ok.

%%==============================================================================
%% 4     CODE FOR TEMPORARY CORRECTIONS
%%==============================================================================

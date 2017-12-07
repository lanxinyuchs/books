%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logServer.erl %
%%% Author:	etxjotj
%%%
%%% @doc
%%% Server process for LOG handling.
%%%
%%% == Records ==
%%%
%%% ```
%%% #log{}     - Refer to log.hrl
%%%              Contains an entry for each LOG which has an MO associated to it.
%%%
%%% #logData{} - Refer to log.hrl
%%%              Contains an entry for each LOG.
%%% '''
%%% 
%%% == LOG characteristics ==
%%% 
%%% The characteristics are described below: 
%%% 
%%% ```
%%% Erlang         - created from an Erlang module.
%%% Application(1) - Created using an appdata file, aka SAF LOG.
%%%                  Will automatically also be Public and Central.
%%% Application(2) - created directly using the SAF interface.
%%%                  Will automatically also be Internal.
%%% 
%%% System Created - Mandatory LOGs created by the system, e.g. AlarmLog
%%% 
%%% Public         - An MO is associated to the LOG; possible e.g. to export the LOG
%%% Internal       - No MO associated. Not configurable in run time. 
%%%                  Only possible to export via ESI.
%%% 
%%% Central        - A LOG representing a whole node, e.g. VNF.
%%%                  Exists only on the OAM Erlang VM.
%%% Local          - LOG existing on all Erlang VMs.
%%% 
%%% Rotating       - Rotates among a specified number of files; when a file
%%%                  is full the oldest file is deleted.
%%% Halt           - When the LOG is full it is not possible to write to it anymore.
%%% 
%%% Encrypted      - The content of the LOG is encrypted.
%%% 
%%% Compressed     - The LOG files are compressed. 
%%%                  The current open file will not be compressed.
%%%                  This attribute is ignored if the LOG is encrypted.
%%%                  Application (SAF) LOGs can not be compressed. 
%%% '''
%%% The characteristics can be combined in many ways, e.g.
%%% 
%%%   Erlang - Internal - Local - Rotating - Encrypted <br />
%%%   Application(1) - Rotating (- Public - Central)
%%% 
%%% @end
%%% ----------------------------------------------------------
-module(logServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-16').
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
%%% R1A/1      2012-02-06 etxjotj     Created
%%% R1A/19     2012-07-19 etxjotj     Added a code_change printout
%%% R2A/9      2013-09-25 uabesvi     Added admin owner and check event stuff
%%% R2A/26     2013-12-12 uabesvi     changed LogM to RcsLogM
%%% R2A/27     2014-01-28 uabesvi     fixed cs logs
%%% R3A/7      2015-01-14 etxpeno     Support for regular role
%%% R3A/10     2015-02-02 etxjotj     Improved handling of progress reports
%%%                                   and actionId handling
%%% R3A/12     2015-04-02 etxjotj     HT63111 Reset progress report
%%% R3A/13     2015-04-07 etxjotj     Handle 'undefined' progress report
%%% -----      ---------  --------    ------------------------
%%% R4A/1      2015-07-21 etxjotj     Clean disk
%%% R4A/2      2015-08-05 etxtory     Removed info_msg; filling logs
%%% R4A/3      2015-08-24 etxtory     transfer_esi updated
%%% R4A/4      2015-09-01 etxjotj     SysInitI printouts
%%% R4A/5      2015-09-17 etxjotj     Log counter in dets
%%% R4A/6      2015-09-23 etxjotj     30 seconds dets cache
%%% R4A/7      2015-11-27 etxtory     Only allow one export per log at the same time
%%% R5A/1      2016-02-11 etxpejn     Added dets on regular DU
%%% -----      ---------  --------    ------------------------
%%% R8A/1      2016-11-24 uabesvi     Encrypted logs
%%% R9A/1      2017-02-08 uabesvi     Added recordSize to appdata
%%% R9A/2-5    2017-03-27 uabesvi     Added code for compress
%%% R11A/1     2017-09-06 ebabmat     HW19728
%%% R12A/1     2017-11-08 uabesvi     HW37113 ESI during warm restart
%%% ----------------------------------------------------------
%%%
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([create_log/2, delete_log/1, export_log/3]).
-export([update_log/2]).
-export([create_app_log/2, delete_app_log/1]).
-export([transfer_esi/2, transfer_esi/3]).
-export([transfer_avli/2]).
-export([log_dir/1]).
-export([notification_callback/2]).
-export([subscribe_alarm/0]).
-export([log_exported/1]).
-export([clean_disk/1]).
-export([initiate_counters/1, update_counter/1]).
-export([is_action_ongoing/1]).

-export([prep_warm/0]).
-export([warm/0]).
-export([warm_done/0]).
-export([get_warm_restart_state/0]).

-export([get_alarm_log_data/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-include("log.hrl").
-include("RcsLogM.hrl").
-include("LogCounter.hrl").
-include("LogOld.hrl").
%%-include("LogInternal.hrl").
-include("safs_ntf.hrl").

-define(SERVER, ?MODULE).

-define(NtfVersion, #safsVersion{releaseCode = $A,
                                 majorVersion = 1,
                                 minorVersion = 1}).

-define(NtfLogSubscriptionId, 1).
-define(NtfAlarmSubscriptionId, 2).
-define(NtfAvcSubscriptionId, 3).

%%=========================================
%% These are temporary until the hrl
%% files are released

-record(safsLogFileCreateAttributes_2,
        {logFileName,                   % = 1, string
         logFilePathName,               % = 2, string (optional)
         maxLogFileSize,                % = 3, uint64
         maxLogRecordSize,              % = 4, uint32
         haProperty,                    % = 5, bool
         logFileFullAction,             % = 6, {enum,safsLogFileFullAction}
         maxFilesRotated,               % = 7, uint32 (optional)
         logFileFmt                     % = 8, string (optional)
        }).

-record(safsLogCallbacks,
        {saLogFilterSetCallback  = false,
         saLogStreamOpenCallback = false,
         saLogWriteLogCallback   = false
        }).

-define(SAFS_VSN, {safsVersion, $A, 2, 11}).

-define(SA_LOG_ADMIN_CHANGE_FILTER, 1).

-define(LOG_VSN, {safsVersion, $A, 2, 1}).

-define(LOG_WRAP,   sa_log_file_full_action_wrap).
-define(LOG_HALT,   sa_log_file_full_action_halt).
-define(LOG_ROTATE, sa_log_file_full_action_rotate).

-define(SA_ERR_INVALID_PARAM, sa_ais_err_invalid_param).
-define(LOG_OPEN_FLAGS, sa_log_stream_create).

%%=========================================



-define(DEF_SEVERITY_FILTER, []).

-define(ALARM_LDN_ATTR,      <<"safLgStrCfg=saLogAlarm,safApp=safLogService">>).
-define(ALARM_REC_SIZE_ATTR, <<"saLogStreamFixedLogRecordSize">>).
-define(ALARM_DEF_SIZE, 500).
-define(ALARM_AT_SIZE,  150).


-record(state, {ccb_handle,
		admin_owner,
		om_handle,
	        log_handle,
	        alarm_handle,
	        alarm_size   = ?ALARM_DEF_SIZE,
		pids         = [],
	        warm_restart = false}).


%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    gen_server:start_link({local, logServer}, ?MODULE, [], []).


%%=======================================================================
%% create_log(Name, Options)
%% 
%% The log takes a number of options, refer to logI:create_log.
%% The options are stored in a table #logData{}.
%% 
%% If a log is associated with a MO, i.e. option public is set to true
%% a record is created in table #log{}
%% 
%% %% If invoked before logServer is started.
%% ---------------------------------------
%% In this case the invoking process will be the only owner of the disk_log.
%% If the invoking process exits before logServer process is started
%% the disk_log will close the log because there is no owner to the log.
%% 
%% If there is a write request, the logServer module, will in these situations
%% open the log for each request.
%% 
%% If invoked when logServer is started.
%% ---------------------------------------
%% logServer process will be the owner of disk_log.
%% 
%%=======================================================================
create_log(Name, Options) ->
    MaxNoFiles   = proplists:get_value(rotatingSegments, Options),
    MaxSize      = proplists:get_value(maxSize,          Options, 2),
    MaxSizeKb    = proplists:get_value(maxSizeKb,        Options),
    Public       = proplists:get_value(public,           Options, false),
    IsLocal      = proplists:get_bool(local,             Options),
    IsEncrypted  = proplists:get_bool(encrypted,         Options),
    IsCompressed = proplists:get_bool(compressed,        Options),
    MilliSec     = proplists:get_bool(milliSec,          Options),

    UseMaxSize  = choose(MaxSizeKb == undefined,
			 ?GET_LOG_SIZE_BYTES(MaxSize),
			 MaxSizeKb),

    logLib:log_storage_mode_create(Name, IsLocal, IsEncrypted, IsCompressed),
    {ok, _} = do_open_log(whereis(?SERVER), 
			  Public,
			  Name, 
			  UseMaxSize, 
			  MaxNoFiles),

    create_log_data(Name, UseMaxSize, MaxNoFiles, not Public, MilliSec),
    LogDir = log_dir(Name),
    logEsi:register_esi_dir(LogDir),
    ok.


%%-----------------------------------------------------
%% create_log_data(Internal, ServerPid, Name, MaxNoFiles, MaxKb)
%%-----------------------------------------------------
create_log_data(Name, Max, RotSeg, Internal, MilliSec) ->
    ObjLogData = #logData{logId            = {"1", "1", "1", Name},
			  maxSizeKb        = Max,
			  rotatingSegments = RotSeg,
			  appLog           = false,
			  internal         = Internal,
			  milliSec         = MilliSec},
    logDb:log_data_set(ObjLogData).




%%=======================================================================
%% delete_log(Name)
%%=======================================================================
delete_log(Name) ->
    mnesia_dirty_delete(log, {"1","1","1",Name}),
    dets:delete(logCounter, Name),
    disk_log:close([{name, Name}]),
    LogDir = log_dir(Name),
    os:cmd("rm -rf " ++ LogDir),
    logEsi:unregister_esi_dir(LogDir),
    ok.

%%=======================================================================
%% update_log(Name, Options)
%% 
%% For Otions, refer to logI
%%=======================================================================
update_log(Name, Options) ->
    logLib:log_storage_mode_update(Name, Options),
    logLib:log_data_update(Name, Options).


%%=======================================================================
%% export_log(Name, Url, Password)
%%=======================================================================
export_log(Name, Url, Password) ->
    ActionId = new_action_id(),
    ok = gen_server:cast(logServer,
			 {export_log, Name, Url, Password, el_dir(Name),
			  ActionId}),
    {ok, ActionId}.

el_dir(Name) ->
    case ets:lookup(logData, {"1","1","1",Name}) of
	[#logData{appLog = true}] -> app_log_dir(Name);
	_                         -> log_dir(Name)
    end.

%%=======================================================================
%% create_app_log(Name, Options)
%% 
%% Called from logAppData when an appdata file specifying a log is found
%%=======================================================================
create_app_log(Name, Options) ->
    cal(proplists:get_value(rotatingSegments, Options), Name, Options).

cal(RotSeg, Name, Options)
  when (is_integer(RotSeg) andalso
	RotSeg >= 3 andalso
	RotSeg =< 256) orelse
        RotSeg == undefined ->
    MaxNoFiles = proplists:get_value(rotatingSegments, Options),
    MaxSize    = proplists:get_value(maxSize, Options, 2),
    MaxSizeKb  = proplists:get_value(maxSizeKb, Options),
    RecSize    = proplists:get_value(recordSize, Options, ?LOG_REC_SIZE_DEF),
    SevFilter  = proplists:get_value(severityFilter,
				     Options,
				     ?DEF_SEVERITY_FILTER),

    IsLocal      = proplists:get_bool(local,      Options),
    IsEncrypted  = proplists:get_bool(encrypted,  Options),
    IsCompressed = proplists:get_bool(compressed, Options),
    logLib:log_storage_mode_create(Name, IsLocal, IsEncrypted, IsCompressed),

    UseMaxSize = choose(MaxSizeKb == undefined,
			?GET_LOG_SIZE_BYTES(MaxSize),
			MaxSizeKb),

    %% Log counters are added automatically by dirty_update_counter
    ObjData = #logData{logId            = {"1","1","1",Name},
		       maxSizeKb        = UseMaxSize,
		       rotatingSegments = MaxNoFiles,
		       recordSize       = RecSize,
		       appLog           = true},
    logDb:log_data_set(ObjData),
    ObjLog = #log{logId          = {"1","1","1",Name},
		  severityFilter = SevFilter},
    ok = logDb:log_set(ObjLog);
%%    gen_server:call(logServer, {create_log, Name, Options});
cal(_, _, _) ->
    {error, ?SA_ERR_INVALID_PARAM}.

%%=======================================================================
%% delete_app_log(Name)
%%=======================================================================
delete_app_log(Name) ->
    mnesia_dirty_delete(log, {"1","1","1",Name}),
    dets:delete(logCounter, Name),
    disk_log:close([{name, Name}]),
    LogDir = log_dir(Name),
    os:cmd("rm -rf "++LogDir),
    logEsi:unregister_esi_dir(LogDir),
    ok.

%%=======================================================================
%% transfer_esi(Url, Password)
%% transfer_esi(Url, Password, Granularity)
%%=======================================================================
transfer_esi(Url, Password) ->
    transfer_esi(Url, Password, _Granularity = undefined).

transfer_esi(Url, Password, Granularity) ->
    ActionId = new_action_id(),
    ok = gen_server:cast(logServer, 
			 {transfer_esi, Url, Password, Granularity, ActionId}),
    {ok, ActionId}.

%%=======================================================================
%% transfer_avli(Url, Password)
%%=======================================================================
transfer_avli(Url, Password) ->
    ActionId = new_action_id(),
    ok = gen_server:cast(logServer, {transfer_avli, Url, Password, ActionId}),
    {ok, ActionId}.

%%=======================================================================
%% log_exported(Log)
%%=======================================================================
log_exported(Log) ->
    gen_server:cast(logServer, {log_exported, Log}).

%%%===================================================================
%%% safs_ntf callbacks
%%%===================================================================
notification_callback(SubscriptionId, Notification) ->
    gen_server:cast(?SERVER,
		    {notification_callback, SubscriptionId, Notification}).

%%% ----------------------------------------------------------
%%% @doc Clean the disk when necessary
%%% Internal logs are truncated at major level
%%% ----------------------------------------------------------
clean_disk(major) ->
    Match = #logData{internal = true, _ = '_'},
    Objs  = mnesia:dirty_match_object(Match),
    Keys  = [Key || #logData{logId = {_, _, _, Key}} <- Objs],
    [disk_log:truncate(Log) || Log <- Keys],
    clean_disk(minor);
clean_disk(minor) ->
    ok.

%%%===================================================================
%% logCounters.dets contains the index to all RCS log files.
%%
%% The indeces are stored in a dets table to survive a rollback.
%% During an upgrade the new UP may write entries in the logs,
%% those writes will increase the log's index.
%% In case of a roll back the log files are not reverted, thus the
%% index should also survive a rollback. If they were stored in
%% mnesia, then the index would be reverted and the log files 
%% would get several entries with the same indes.
%%%===================================================================
initiate_counters(AllCounters) ->
    File = log_dir("logCounter.dets"),
    filelib:ensure_dir(File),
    open_dets(logCounters, [{file, File}]),
    dets:insert(logCounters, [{Name, Value}||{_,Name,Value}<-AllCounters]),
    dets:close(logCounters).

update_counter(Name) ->
    try dets:update_counter(logCounter, Name, 1) of
	Value ->
	    Value
    catch error:badarg ->
	    File = log_dir("logCounter.dets"),
	    filelib:ensure_dir(File),
	    open_dets(logCounter, [{file, File}]),
	    try dets:update_counter(logCounter, Name, 1) of
		Value ->
		    Value
	    catch error:badarg ->
		    dets:insert(logCounter, {Name, 1}),
		    1
	    end
    end.

%%% ----------------------------------------------------------
%%% Action: exportAvailabilityLog | exportEsi | LogName
%%% true - this action type is ongoing
%%% false - this action type is not onging
%%% ----------------------------------------------------------
is_action_ongoing(Action) ->
    case catch gen_server:call(logServer, {is_action_ongoing, Action}) of
	true -> true;
	false -> false;
	_ -> true
    end.

%%% ----------------------------------------------------------
%%% callbacks for warm restart.
%%% No ESI should be done during warm restart.
%%% Any ongoing ESI should be interrupted.
%%% ----------------------------------------------------------
prep_warm() ->
    gen_server:cast(logServer, prep_warm).

warm() ->
    gen_server:cast(logServer, warm).

warm_done() ->
    gen_server:cast(logServer, warm_done).
    
get_warm_restart_state() ->
    case catch gen_server:call(logServer, get_warm_restart_state) of
	true  -> true;
	false -> false;
	_     -> true
    end.

    
%%% ----------------------------------------------------------
%%% get_alarm_log_data() -> {FixedRecSize, AtSize}
%%% ----------------------------------------------------------
get_alarm_log_data() ->
    case catch gen_server:call(logServer, get_alarm_log_data) of
	{ok, Data} -> Data;
	_          -> {?ALARM_DEF_SIZE, ?ALARM_AT_SIZE}
    end.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%%===================================================================
%%% init logServer process
%%%===================================================================
init(_) ->
    process_flag(trap_exit, true),
    appmI:register_warm_cb(?MODULE),
    case sysEnv:role() of
        active ->

	    %% ActionId handling
	    initiate_action_id(),
	    File = log_dir("logCounter.dets"),
	    filelib:ensure_dir(File),
	    open_dets(logCounter, [{file, File}]),

	    %% Cannot be done in the gen_server process because
	    %% after power cycle the logs will be reapared and
	    %% that may take long time.
	    %% The logs should be ready to accept write requests
	    %% directly when the applications are started.
	    %% If the open is done from the gen_server that is
	    %% not guaranteed.
	    %% Ref to HT34574 Heading: TN Operator Logs are not Initializing
	    %%
	    %% To do: test if open should be done in parallel.
	    %% However, only synchroneous open is implemented in safs as of today.
	    {ok, {Ccb, AOHandle, OmHandle}} = initialize_admin_owner(),
	    {ok, SafHandle, _} = safs_log:initialize(#safsLogCallbacks{}, ?LOG_VSN),
	    mnesia:subscribe({table, log, detailed}),

	    start_logs(SafHandle),

	    %% HT63111 Reset progress report
	    [LogM] = mnesia_dirty_read({logM, {"1","1","1"}}),
	    case LogM#logM.progressReport of
		undefined ->
		    ok;
		Progress when Progress#'AsyncActionProgress'.result==
			      ?ActionResultType_NOT_AVAILABLE ->
		    logEsi:handle_fail(
		      "The action was interrupted by a restart");
		_ ->
		    ok
	    end,

	    {ok, [AlarmSize]} = logLib:get_attr(?ALARM_LDN_ATTR, 
						?ALARM_REC_SIZE_ATTR),

	    {ok, #state{ccb_handle   = Ccb,
			admin_owner  = AOHandle,
			om_handle    = OmHandle,
			log_handle   = SafHandle,
			alarm_handle = subscribe_alarm(),
			alarm_size   = AlarmSize,
			pids = []}};
        regular ->
	    File = log_dir("logCounter.dets"),
	    filelib:ensure_dir(File),
	    open_dets(logCounter, [{file, File}]),
	    {ok, #state{}}
    end.

initiate_action_id() ->
     {atomic, ok} = mnesia:transaction(fun do_initiate_action_id/0).

do_initiate_action_id() ->
    case mnesia:read({logRamVariables, actionId}) of
	[] -> %% Cluster reset
	    {_, X, _} = os:timestamp(),
	    mnesia:write({logRamVariables, actionId, X rem 65536});
	_ ->
	    ok
    end.
new_action_id() ->
    mnesia:dirty_update_counter(logRamVariables, actionId, 1) rem 65536.




%%%===================================================================
%%% handle call
%%%===================================================================
handle_call({open_log, StreamName, MaxKb, MaxNoFiles}, _, State) ->
    open_disk_log(StreamName, MaxKb, MaxNoFiles),
    {reply, ok, State};

handle_call({delete_log, _Name}, _, State) ->
    {reply, ok, State};

handle_call({is_action_ongoing, Action}, _, State) ->
    Res = lists:keyfind(Action, 2, State#state.pids),
    {reply, Res, State};

handle_call(get_warm_restart_state, _, #state{warm_restart = WR} = State) ->
    {reply, WR, State};

handle_call(get_alarm_log_data, _, #state{alarm_size = AS} = State) ->
    {reply, {ok, {AS, ?ALARM_AT_SIZE}}, State};

handle_call(_, _, State) ->
    {reply, ok, State}.




%%%===================================================================
%%% handle cast
%%%===================================================================
handle_cast({notification_callback,
	     ?NtfAlarmSubscriptionId,
	     AlarmNotification},
            State) ->
    handle_alarm_notification(AlarmNotification),
    {noreply, State};
handle_cast({export_log, Name, Url, Pass, Dir, ActionId}, State) ->
    Pid = spawn_link(logExport, export_log, [Name, Url, Pass, ActionId, Dir]),
    Pids = State#state.pids,
    {noreply, State#state{pids = [{Pid, Name} | Pids]}};
%% No ESI if warm restart is ongoing and the ESI cb:s are invoked
%% (no callbacks are invoked if Granularity = static)
handle_cast({transfer_esi, _Url, _Pass, Gran, ActionId}, 
	    #state{warm_restart = WarmRestart} = State)
  when WarmRestart andalso
       Gran =/= static ->
    progress_error(ActionId),
    {noreply, State};
handle_cast({transfer_esi, Url, Pass, Gran, ActionId}, State) ->
    Pid = spawn_link(logEsi, transfer_esi, [Url, Pass, Gran, ActionId]),
    Pids = State#state.pids,
    {noreply, State#state{pids = [{Pid, exportEsi} | Pids]}};
handle_cast({transfer_avli, Url, Pass, ActionId}, State) ->
    Pid = spawn_link(logExport, transfer_avli, [Url, Pass, ActionId]),
    Pids = State#state.pids,
    {noreply, State#state{pids = [{Pid, exportAvailabilityLog} | Pids]}};
handle_cast({log_exported, Log}, State) ->
    handle_log_exported(Log),
    {noreply, State};
handle_cast(prep_warm, State) ->
    {noreply, State#state{warm_restart = true}};
handle_cast(warm, State) ->
    {noreply, State#state{warm_restart = true}};
handle_cast(warm_done, State) ->
    {noreply, State#state{warm_restart = false}};
handle_cast(Request, State) ->
    info_msg("handle_cast ~p ~n", [Request]),
    {noreply, State}.





%%%===================================================================
%%% handle info
%%%===================================================================
handle_info({mnesia_table_event, Event},
	    State) ->
    {ok, NewState} = check_event(Event, State),
    {noreply, NewState};

handle_info(Request = {disk_log, Node, Log, {wrap, NoLostItems}}, State)
  when Node == node() ->
    info_msg("handle_info ~p ~n",[Request]),
    case NoLostItems of
	0 -> ok;
	_ ->
	    warning_msg("Log ~p wrapped and lost ~w items~n",[Log, NoLostItems])
    end,

    Fun = fun() -> handle_push_transfer(Log) end,

    case mnesia:transaction(Fun) of
	{atomic, {push_actions, Actions}} ->
	    [apply(M, F, A) || {M, F, A} <- Actions];
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{mfa, {?MODULE, handle_push_transfer,[Log]}},
	       {aborted, Reason}])
    end,
    compress(logLib:is_compressed(Log), logLib:is_encrypted(Log), Log),
    {noreply, State};
handle_info(Request = {disk_log, Node, Log, full}, State)
  when Node == node() ->
    info_msg("handle_info ~p ~n",[Request]),

    Fun = fun() -> handle_push_transfer(Log) end,

    case mnesia:transaction(Fun) of
	{atomic, {push_actions, Actions}} ->
	    [apply(M, F, A) || {M, F, A} <- Actions],
	    disk_log:truncate(Log);
	{aborted, Reason} ->
	    comsaI:send_alarm('LogHasReachedFullCapacity',
			      warning,
			      ?LOG_LDN,
			      "file is full"),
	    sysInitI:error_report(
	      [{mfa, {?MODULE, handle_push_transfer,[Log]}},
	       {aborted, Reason}])
    end,
    {noreply, State};
handle_info({disk_log, _Node, _Log, {error_status, ok}}, State) ->
    {noreply, State};
handle_info({disk_log, Node, _Log, {error_status, _Status}} = Info, State)
  when Node == node() ->
    info_msg("handle_info ~p ~n",[Info]),
    restart_if_cloud(sysEnv:rcs_mode_2(), Info),
    {noreply, State};

handle_info({'EXIT', Pid, _}, State) ->
    Pids = State#state.pids,
    NewPids = lists:keydelete(Pid, 1, Pids),
    {noreply, State#state{pids = NewPids}};

handle_info(_Request, State) ->
    {noreply, State}.

%%%===================================================================
%%% code change
%%%===================================================================
code_change(OldVsn, State, Extra) ->
    info_msg("code_change(~p, ~p, ~p)~n",[OldVsn, State, Extra]),
    {ok, State}.

%%%===================================================================
%%% terminate
%%%===================================================================
terminate(_, #state{log_handle = LogHandle} = State) ->
    finalize_admin_owner(State),
    safs_log:finalize(LogHandle),
    dets:close(logCounter),
    ok.





%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%%===================================================================
%% start_logs(SafHandle) -> ok
%% 
%% This function is executed when logServer process is started.
%% - open application logs by invoking SAF,
%% - open erlang logs to get ownership of the associated disk_log.
%% 
%%%===================================================================
start_logs(SafHandle) ->
    open_app_logs(sl_get_app_logs(logDb:log_get_all(), []), SafHandle),
    open_erl_logs(sl_get_erl_logs()),
    ok.


sl_get_app_logs([], AppLogs) ->
    AppLogs;
sl_get_app_logs([#log{logId = LogId} = Log | T], AppLogs) ->
    case logDb:log_data_get_dirty(LogId) of
	[#logData{appLog = false}] -> 
	    sl_get_app_logs(T, AppLogs);
	_ -> 
	    sl_get_app_logs(T, [Log | AppLogs])
    end.

sl_get_erl_logs() ->
    Match = #logData{appLog = false, _ = '_'},
    mnesia:dirty_match_object(Match).
    

%%%===================================================================
%%% do_open_log(ServerPid, Public, Name, MaxSize, MaxNoFiles)
%%%===================================================================
%%-----------------------------------------------------------------
%% Public log and the server is not yet started.
%% Open the log from the invoking process
%%-----------------------------------------------------------------
do_open_log(undefined, true, StreamName, MaxKb, MaxNoFiles) ->
    logDb:log_set(#log{logId = {"1","1","1",StreamName}}),
    open_disk_log(StreamName, MaxKb, MaxNoFiles);
%%-----------------------------------------------------------------
%% Internal log and the server is not yet started. 
%% Open the log from the invoking process
%%-----------------------------------------------------------------
do_open_log(undefined, false, StreamName, MaxKb, MaxNoFiles) ->
    open_disk_log(StreamName, MaxKb, MaxNoFiles);
%%-----------------------------------------------------------------
%% Server is started. 
%% Create the log from the server process.
%%-----------------------------------------------------------------
do_open_log(_Pid, _Public,StreamName, MaxKb, MaxNoFiles) ->
    info_msg("Opening log ~p.~n", [StreamName]),
    Res = try_open_log(StreamName, MaxKb, MaxNoFiles),
    {ok, Res}.

try_open_log(StreamName, MaxKb, MaxNoFiles) ->
    case catch gen_server:call(?SERVER, 
            {open_log, StreamName, MaxKb, MaxNoFiles}) of
        {'EXIT', {timeout, _}} ->
            warning_msg("Log open timeout! Trying open from invoking process. LOG: ~p.~n", [StreamName]),
            open_disk_log(StreamName, MaxKb, MaxNoFiles);
        {'EXIT', Reason} ->
            error_msg("Unexpected error opening log: ~p~nReason:~p~n", 
                [StreamName, Reason]),
            {error, Reason};
        Reply ->
            info_msg("Opening of log fom logServer process ~p was sucessfull.~n", [StreamName]),
            Reply
    end.

%%%===================================================================
%%% open_erl_logs([#logData{}])
%%%===================================================================
open_erl_logs(Logs) ->
    [open_disk_log(Name, MaxKb, MaxNoFiles) ||
	#logData{logId            = {_, _, _, Name},
		 maxSizeKb        = MaxKb,
		 rotatingSegments = MaxNoFiles} <- 
	    Logs].
    
%%%===================================================================
%%% open_disk_log(Name, MaxSize, MaxNoFiles)
%%%===================================================================
open_disk_log(Name, MaxKb, MaxNoFiles) ->
    logLib:open_disk_log(Name, MaxKb, MaxNoFiles).


handle_push_transfer(Log) ->
    Keys = mnesia:all_keys(logPushTransfer),
    ThisLogKeys = lists:reverse([Key || Key <- Keys, element(4, Key) == Log]),
    handle_push_transfer(Log, [], ThisLogKeys).

handle_push_transfer(Log, Actions, [Key | Keys]) ->
    [Obj] = mnesia:read({logPushTransfer, Key}),
    #logPushTransfer{uri = Uri, password = Password, transferType = Type} = Obj,
    case Type of
	?TransferType_STREAM ->
	    handle_push_transfer(Log, Actions, Keys);
	?TransferType_BULK ->
	    Decrypted = comsaGeneric:decrypt_password(Password),
	    ActionId  = new_action_id(),
	    Attrs     = [Log, Uri, Decrypted, ActionId],
	    Action    = {logExport, export_part, Attrs},
	    handle_push_transfer(Log, [Action | Actions], Keys)
    end;

handle_push_transfer(_, Actions, []) ->
    {push_actions, Actions}.


log_dir("logCounter.dets" = Name) ->
    filename:join([sysEnv:rcs_dir(), "log", Name]);

log_dir(Name) ->
    logLib:log_dir(Name).

%% alarm log is stored in safs and the log name is not
%% the same as the MO name
app_log_dir("AlarmLog") ->
    filename:join([safs_log:get_root(), "saLogAlarm"]);
app_log_dir(Name) ->
    filename:join([safs_log:get_root(), Name]).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args)->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%===========================================================================
%% check Mnesia events
%% 
%% only write events are handled
%%===========================================================================
check_event({write,
	     log,
	     Log,
	     [Log],
	     _Trans},
	    State) ->
    {ok, State};
check_event({write,
	     log,
	     #log{logId          = {_, _, _, [O | BjectName] = ON},
		  severityFilter = SeverityFilter} = _NewLog,
	     _OldLog,
	     _Trans},
	    #state{ccb_handle = CcbHandle} = State)
  when ON == "SaLogAlarm";
       ON == "SaLogNotification";
       ON == "SaLogSystem" ->

    LowerObject = string:to_lower([O]) ++ BjectName,
    ObjectName  = "safLgStrCfg=" ++ LowerObject ++ ",safApp=safLogService",

    Val      = [#safsImmAttrValue{sauint32 = get_severity(SeverityFilter, 0)}],
    ModAttrs = #safsImmAttrValues_2{attrName = <<"saLogStreamSeverityFilter">>,
				    attrValueType    = sa_imm_attr_sauint32t,
				    attrValuesNumber = 1,
				    attrValues       = Val},

    Attrs = [#safsImmAttrModification_2{modType = sa_imm_attr_values_replace,
					modAttr = ModAttrs}],

    safs_imm_om:ccb_object_modify_2(CcbHandle,
				    list_to_binary(ObjectName),
				    Attrs),

    safs_imm_om:ccb_apply(CcbHandle),
    {ok, State};
check_event({write,
	     log,
	     #log{logId          = {_, _, _, ObjName},
		  severityFilter = SeverityFilter} = _NewLog,
	     _OldLog,
	     _Trans},
	    #state{admin_owner = OwnerHandle} = State) ->
    LowerObject = ObjName,
    ObjectName  = "safLgStr=" ++ LowerObject ++ ",safApp=safLogService",

    safs_imm_om:admin_operation_invoke_2(OwnerHandle,
					 list_to_binary(ObjectName),
					 1,            %% continuation id
					 ?SA_LOG_ADMIN_CHANGE_FILTER,
					 get_severity(SeverityFilter, 0),
					 0),
    {ok, State};
check_event(Event, State) ->
    sysInitI:info_report("~p: ignored mnesia event: ~p ~n",
			     [?MODULE, Event]),
    {ok, State}.





%%===========================================================================
%% SAF handling functions
%%===========================================================================


%%===========================================================================
%% initialize_admin_owner
%%===========================================================================
%%--------------------------------------------
%% initialize
%%--------------------------------------------
initialize_admin_owner() ->
    iao_init(safs_imm_om:initialize(undefined, ?SAFS_VSN)).

%%--------------------------------------------
%% admin_owner_initialize
%%--------------------------------------------
iao_init({ok, OmHandle, _}) ->
    iao_aoi(safs_imm_om:admin_owner_initialize(OmHandle,
					       <<"CsLogM">>,
					       true),
	    OmHandle);
iao_init(Error) ->
    Error.


%%--------------------------------------------
%% ccb_initialize
%%--------------------------------------------
iao_aoi({ok, AOHandle}, OmHandle) ->
    iao_ccb(safs_imm_om:ccb_initialize(AOHandle, 0), AOHandle, OmHandle);
iao_aoi(Error, OmHandle) ->
    safs_imm_om:finalize(OmHandle),
    Error.

%%--------------------------------------------
%% admin_owner_set
%%--------------------------------------------
iao_ccb({ok, Ccb}, AOHandle, OmHandle) ->
    iao_aos(safs_imm_om:admin_owner_set(AOHandle,
					  [<<"safApp=safLogService">>],
 					  sa_imm_subtree),
	    {ok, {Ccb, AOHandle, OmHandle}});
iao_ccb(Error, AOHandle, OmHandle) ->
    safs_imm_om:admin_owner_finalize(AOHandle),
    safs_imm_om:finalize(OmHandle),
    Error.

%%--------------------------------------------
%% Result
%%--------------------------------------------
iao_aos(ok, Result) ->
    Result;
iao_aos(Error, {ok, {_Ccb, AOHandle, OmHandle}}) ->
    sysInitI:error_report(["#### SERVER AOS",
			       Error,
			       ets:tab2list(imm_objects)]),
    safs_imm_om:admin_owner_finalize(AOHandle),
    safs_imm_om:finalize(OmHandle),
    Error.


%%===========================================================================
%% initialize_admin_owner
%%===========================================================================
finalize_admin_owner(#state{ccb_handle  = Ccb,
			    admin_owner = AOHandle,
			    om_handle   = OmHandle}) ->
    {safs_imm_om:ccb_finalize(Ccb),
     safs_imm_om:admin_owner_finalize(AOHandle),
     safs_imm_om:finalize(OmHandle)}.

get_severity(undefined, Acc) ->
    127 bxor Acc;
get_severity([], Acc) ->
    127 bxor Acc;
get_severity([SF | T], Acc) ->
    get_severity(T, (1 bsl SF) + Acc).

%%===========================================================================
%% open_app_logs(AppLogs, SafHandle)
%% 
%% These LOGS are SAF LOGs.
%% RCS will create the LOG in SAF, i.e. RCS will always be in control of
%% the LOG's create attributes. The only attribute the application can
%% configure is the record size.
%%
%% To be able to write to the log the application must also open the log 
%% in SAF. Note, the applications cannot change the create attributes.
%% 
%%===========================================================================
open_app_logs(AppLogs, SafHandle) ->
    [open_app_log(Log#log.logId,
		  Log#log.severityFilter,
		  get_name(Log#log.logId),
		  SafHandle) || Log <- AppLogs].


%% alarm log is already opened in safs
open_app_log({_, _, _, "AlarmLog"}, _Severity, _StreamName, _LogHandle) ->
    ok;
open_app_log({_, _, _, StreamName} = LogId, Severity, StreamName, LogHandle) ->
    {MaxSize, MaxRotate, FullAction, MaxRecordSize} =
	try
	    [Rec] = logDb:log_data_get_dirty(LogId),
	    #logData{maxSizeKb        = MaxSize1,
		     rotatingSegments = MaxNoFiles,
		     recordSize       = MaxRecordSize1} = Rec,
	    FullAct1   = choose(MaxNoFiles == undefined, ?LOG_HALT, ?LOG_ROTATE),
	    MaxRotate1 = choose(MaxNoFiles == undefined, 0, MaxNoFiles),
	    {MaxSize1, MaxRotate1, FullAct1, MaxRecordSize1}

	catch _:_ ->
		{2, 4, ?LOG_WRAP, ?LOG_REC_SIZE_DEF}
	end,

    %%=========================================================
    %% open saf log
    %%=========================================================

    Format      = "@Cr @CY-@Cm-@CdT@Ch:@Cn:@CsZ.@Cq @Sv @Sl \"@Cb\"",
    MaxFileSize = ?GET_LOG_SIZE_BYTES(MaxSize),
    CreateAttributes =
	#safsLogFileCreateAttributes_2{logFileName       = StreamName,
				       logFilePathName   = StreamName,
				       maxLogFileSize    = MaxFileSize,
				       maxLogRecordSize  = MaxRecordSize,
				       haProperty        = false,
				       logFileFullAction = FullAction,
				       maxFilesRotated   = MaxRotate,
				       logFileFmt        = Format
				      },

    LogObj = #log{logId          = {"1","1","1",StreamName},
		  severityFilter = Severity},

    OpenFlags = ?LOG_OPEN_FLAGS,
    Timeout   = 5000,

    StreamNameSaf = "safLgStr=" ++ StreamName ++ ",safApp=safLogService",

    info_msg("Opening SAF log ~p ~n",[StreamNameSaf]),
    hcl_stream(safs_log:stream_open_2(LogHandle,
				      StreamNameSaf,
				      CreateAttributes,
				      OpenFlags,
				      Timeout),
	       FullAction,
	       StreamNameSaf,
	       StreamName,
	       LogObj).

hcl_stream({ok, StreamHandle}, FullAction, StreamNameSaf, StreamName, LogObj) ->
    update_log_alarm(FullAction, StreamNameSaf, StreamName, StreamHandle),

    %%=========================================================
    %% create MO
    %%=========================================================
    %% Log counters are added automatically by dirty_update_counter
    hcl_mo(mnesia_write(LogObj),
	   StreamName,
	   StreamHandle);
hcl_stream(Error, _FullAction, _StreamNameSaf, _StreamName, _LogObj) ->
    Error.

hcl_mo({atomic, ok}, _StreamName, _) ->
    ok;
hcl_mo(Error, _StreamName, StreamHandle) ->
    safs_log:stream_close(StreamHandle),
    Error.

%%===========================================================================
%% subscribe_alarm
%%===========================================================================
subscribe_alarm() ->
    Callbacks = #safsNtfCallbacks{saNtfNotificationCallback = ?MODULE},
    {ok, Handle, _} = safs_ntf:initialize(Callbacks, ?NtfVersion),

    AnFilterHeader = #safsNtfNotificationFilterHeader{_ = []},
    AnFilter =
        #safsNtfAlarmNotificationFilter{notificationFilterHeader =
					AnFilterHeader,
                                        _ = []},
    NotificationTypeFilters =
        #safsNtfNotificationTypeFilters{alarmNotificationFilter = AnFilter},

    ok = safs_ntf:notification_subscribe(Handle,
					 NotificationTypeFilters,
                                         ?NtfAlarmSubscriptionId),
    Handle.

%%===========================================================================
%% handle_log_exported
%%===========================================================================
handle_log_exported(Log) ->
    MatchHead = #logAlarm{alarm         = '$1',
			  stream_handle = '$2',
			  ldn           = '$3',
			  log_name      = Log,
			  _             = '_'},
    hle(mnesia:dirty_select(logAlarm,[{MatchHead, [], [['$_']]}])).

hle([[#logAlarm{alarm         = true,
		stream_handle = StreamHandle} = LogAlarm]]) ->
    hle_rc(safs_log:stream_reopen_file(StreamHandle), LogAlarm);
hle(_) ->
    ok.

hle_rc(ok, #logAlarm{ldn = Ldn} = LA) ->
    NewLA = LA#logAlarm{alarm = false},
    {atomic, ok} = mnesia_write(NewLA),
    comsaI:clear_alarm('LogHasReachedFullCapacity', Ldn, "").

%%===========================================================================
%% handle_alarm_notification
%%===========================================================================
handle_alarm_notification(AlarmNotification) ->
    Header = AlarmNotification#safsNtfAlarmNotification.notificationHeader,
    #safsNtfClassId{vendorId = VendorId,
                    majorId  = MajorId,
                    minorId  = MinorId} =
        Header#safsNtfNotificationHeader.notificationClassId,
    AlarmName = {VendorId, MajorId, MinorId},
    PerceivedSeverity =
        AlarmNotification#safsNtfAlarmNotification.perceivedSeverity,
    NotificationObject = Header#safsNtfNotificationHeader.notificationObject,
    AdditionalText = Header#safsNtfNotificationHeader.additionalText,

    try send_alarm(get_stream_name(binary_to_list(AdditionalText)),
		   AlarmName,
		   PerceivedSeverity,
		   NotificationObject,
                   AdditionalText)
    catch
        _:_ ->
            ok
    end.

send_alarm(_,
	   AlarmName,
	   ?SA_NTF_SEVERITY_CLEARED,
	   <<"safApp=safLogService">> = Dn,
	   AdditionalText) ->
    comsaI:clear_alarm(AlarmName, dn(Dn), toString(AdditionalText));
send_alarm(_, _AlarmName, ?SA_NTF_SEVERITY_CLEARED, _Dn, _AdditionalText) ->
    ok;
send_alarm(StreamName,
	   _AlarmName,
	   Severity,
	   <<"safApp=safLogService">>,
	   AdditionalText) ->
    sa(mnesia_dirty_read(logAlarm, StreamName),
       Severity,
       binary_to_list(AdditionalText));
send_alarm(_, _AlarmName, _Severity, _Dn, _AdditionalText) ->
    ok.

sa([#logAlarm{alarm = false,
	      ldn   = LDN}= LA],
   Severity,
   AdditionalText) ->
    comsaI:send_alarm('LogHasReachedFullCapacity',
		      severity(Severity),
		      LDN,
		      AdditionalText),
    NewLA = LA#logAlarm{alarm = true},
    {atomic, ok} = mnesia_write(NewLA);
sa(_, _, _) ->
    ok.

get_stream_name(AddText) ->
    [SN | _] = string:tokens(AddText, ","),
    SN ++ ",safApp=safLogService".

update_log_alarm(?LOG_HALT, StreamNameSaf, StreamName, StreamHandle) ->
    ula(mnesia_dirty_read(logAlarm, StreamNameSaf),
	StreamNameSaf,
	StreamName,
        StreamHandle);
update_log_alarm(_, _StreamNameSaf, _StreamName, _StreamHandle) ->
    ok.

ula([], StreamNameSaf, StreamName, StreamHandle) ->
    LDN = ?LOG_LDN ++ [list_to_binary("Log=" ++ StreamName)],
    Obj = #logAlarm{sa_name       = StreamNameSaf,
		    stream_handle = StreamHandle,
		    log_name      = StreamName,
		    ldn           = LDN},
    {atomic, ok} = mnesia_write(Obj);
ula(_, _, _, _) ->
    ok.



%%===========================================================================
%% miscelaneous functions
%%===========================================================================


-spec dn(binary()) -> [binary()].
dn(Dn) ->
    binary:split(Dn, [<<$=>>,<<$,>>], [global]).

%% toString(undefined) -> "";
-spec toString(undefined  | binary()) ->  string().
toString(AdditionalText) when is_binary(AdditionalText) ->
    binary_to_list(AdditionalText).

-spec severity(atom()) -> atom().
severity(?SA_NTF_SEVERITY_INDETERMINATE) -> indeterminate;
severity(?SA_NTF_SEVERITY_WARNING)       -> warning;
severity(?SA_NTF_SEVERITY_MINOR)         -> minor;
severity(?SA_NTF_SEVERITY_MAJOR)         -> major;
severity(?SA_NTF_SEVERITY_CRITICAL)      -> critical.

choose(true,  T, _) -> T;
choose(false, _, F) -> F.

open_dets(Name, Args) ->
    case dets:open_file(Name, [{auto_save,30}|Args]) of
	{ok, Name} ->
	    {ok, Name};
	{error, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, open_dets},
	       {mfa, {dets, open_file, [Name, Args]}},
	       {error, Reason}])
    end.


restart_if_cloud(vrcs, Error) ->
    Str = lists:flatten(io_lib:format("~p~n", [Error])),
    appmI:restart_node(cold,
		       atom_to_list(?MODULE) ++ " disk_log error: " ++ Str);
restart_if_cloud(_, _) ->
    ok.


%%=================================================================
%% compress(IsCompressed, IsEncrypted, Log) 
%% 
%% compress only if requested and if not encrypted
%%=================================================================
compress(true, false, Log) ->
    {File, NoofFiles, Index} = compress_info(disk_log:info(Log)),

    Dir    = filename:dirname(File),
    CpFile = filename:join([Dir, lists:append([Log, ".", Index])]),
    GzFile = filename:join([Dir, lists:append([Log, "_", get_time()])]),

    disk_log:sync(Log),
    file:copy(CpFile, GzFile),
    os:cmd("gzip " ++ GzFile),
    
    file:delete(CpFile),
    os:cmd("touch " ++ CpFile),
    cleanup_dir(Log, Dir, file:list_dir(Dir), NoofFiles);
compress(_, _, _) ->
    ok.


compress_info(Info) ->
    File     = proplists:get_value(file,         Info),
    Current  = proplists:get_value(current_file, Info),
    {_, Max} = proplists:get_value(size,         Info),
    Index    = integer_to_list(choose(Current == 1, Max, Current - 1)),
    {File, Max, Index}.
  
    
cleanup_dir(Name, Dir, {ok, AllFiles}, NoofFiles) ->
    Files   = [File || File <- AllFiles, lists:prefix(Name ++ "_", File)],
    Sorted  = lists:reverse(lists:sort(Files)),
    cup_dir(NoofFiles - 1, Sorted, Dir).

cup_dir(Split, Files, _) when length(Files) < Split ->
    ok;
cup_dir(Split, Files, Dir) ->
    {_, Rm} = lists:split(Split, Files),
    [file:delete(filename:join([Dir, File])) || File <- Rm].
    

%%======================================================================
%% get name from LogId
%%======================================================================
get_name({_, _, _, Name}) ->
    Name.


progress_error(ActionId) ->
    %% Intiate progress report
    StartTime = comsaI:iso_time(os:timestamp(), extended),
    Progress  = [{actionName,          "Export ESI"},
		 {additionalInfoClear, ""},
		 {progressPercentage,  0},
		 {result,              ?ActionResultType_FAILURE},
		 {resultInfo,          "Warm restart ongoing."},
		 {state,               ?ActionStateType_FINISHED},
		 {actionId,            ActionId},
		 {timeActionStarted,   StartTime},
		 {timeActionCompleted, StartTime}],
    logEsiLib:update_progress(Progress).



%%======================================================================
%% get_time() -> Time
%%
%% Get current time in a string format
%%======================================================================
get_time() ->
    Time = {_, _, Part} = os:timestamp(),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Time),
    integer_to_list(Y) ++
	i2l(Mo) ++
	i2l(D) ++
	"_" ++
	i2l(H) ++
	i2l(Mi) ++
	i2l(S) ++
	"_" ++
	li2l(Part div 1000).

i2l(I) when I < 10 -> "0" ++ integer_to_list(I);
i2l(I)             -> integer_to_list(I).

li2l(I) when I <   10 -> "00" ++ integer_to_list(I);
li2l(I) when I <  100 -> "0" ++ integer_to_list(I);
li2l(I)               -> integer_to_list(I).



mnesia_write(Obj) ->
    mnesia:transaction(fun() -> mnesia:write(Obj) end).


mnesia_dirty_read(Obj) ->
    mnesia:dirty_read(Obj).

mnesia_dirty_read(Rec, Obj) ->
    mnesia:dirty_read(Rec, Obj).

mnesia_dirty_delete(Rec, Obj) ->
    mnesia:dirty_delete(Rec, Obj).


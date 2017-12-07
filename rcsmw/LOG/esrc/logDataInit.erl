%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logDataInit.erl %
%%% Author:	etxbjca
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(logDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-11-02').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% -----      ---------- --------    ------------------------
%%% R1A/1      2012-01-04 etxbjca     Created
%%% R1A/8      2013-06-04 etxasta     Added logStreamServer
%%% R2A/10     2013-09-04 etxasta     Added AuditTrailLog
%%% R2A/21     2013-12-12 uabesvi     changed LogM to RcsLogM
%%% R2A/31     2014-10-13 etxjotj     Added registrations for log dirs after ug
%%% R2A/32     2014-10-14 etxjotj     Fixed above, moved create table
%%% R2A/33     2014-10-14 etxjotj     Fixed above, moved data init
%%% R3A/1      2014-12-16 etxarnu     Added transform(logPushTransfer) 
%%% R3A/4      2015-01-27 etxjotj     RcsLogM 2.3
%%% R3A/6      2015-02-05 erarafo     SwmInternal log missing from ESI, fixed
%%% R3A/7      2015-04-07 etxjotj     HT63111 Progress reset moved to logServer
%%% ----    ---------- -------  -----------------------------------------------
%%% R4A/1   2015-05-20 etxpejn  Only logs from ?RCS_LOGS should be incl in ESI
%%% R4A/2   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/3   2015-07-21 etxjotj  Disk ownership registration
%%% R4A/4   2015-09-01 etxjotj  SysInitI printouts
%%% R4A/5   2015-09-17 etxjotj  Log counters not in mnesia
%%% R4A/6   2015-09-18 etxtory  Added esi-dir
%%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R6A/1   2016-06-02 etxarnu  Add /var/log to ESI also for vrcs
%%% R6A/3   2016-07-20 etomist  HU96800, add /var/pmd to ESI
%%% ----------------------------------------------------------
%%% R9A/2   2017-03-22 uabesvi  HV63844  no such file or directory
%%% R9A/2   2017-03-27 uabesvi  Added code for compress
%%% ----------------------------------------------------------
%%% R10A/2  2017-05-16 etxjotj  Use sysEnv:rcs_mode_2/0
%%% R10A/4  2017-06-15 uabesvi  Added imported_esi to ESI dirs
%%% ----------------------------------------------------------
%%% R11A/1  2017-08-30 uabesvi  Added child logEsiExtServer
%%% R11A/2  2017-09-04 etxjotj  Replaced swmLib with swmI
%%% ----------------------------------------------------------
%%% R12A/1  2017-10-31 etxarnu  Use sysEnv:rcs_mode_3/0
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0]).
-export([children/0]).
-export([activate/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([add_clh_option/1]).

-include("log.hrl").
-include("RcsLogM.hrl").
-include("LogCounter.hrl").
-include("LogOld.hrl").
%%-include("LogInternal.hrl").
-include("LogExtServer.hrl").

%%=========================================
%% These are temporary until the hrl
%% files are released

-define(SAFS_VSN, {safsVersion, $A, 2, 11}).
%% -define(SAFS_VSN, #safsVersion{releaseCode  = $A,
%% 			       majorVersion = 2,
%% 			       minorVersion = 11}).

-define(LOG_WRAP,   1).
-define(LOG_HALT,   2).
-define(LOG_ROTATE, 3).

%%=========================================

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc Create the Mnesia tables needed for LOG
%%% @end
%%% ----------------------------------------------------------
instPhParallel_init(DbNodes) ->

    IsUpgradeOngoing = swmI:is_upgrade_ongoing(),
    LogM = [{logM,            ?logM_types},
	    {log,             ?log_types},
	    {logPushTransfer, ?logPushTransfer_types}],
    [create_table(Name, DbNodes, Types) || {Name, Types} <- LogM],

    %% Create record where the max size and no of rotating elements
    %% is stored (previously this was stored in #log)
    LogDataFields = record_info(fields, logData),
    {atomic, ok} = 
	clhI:mnesia_create_table(logData, [{type, set},
					   {disc_copies, DbNodes},
					   {attributes, LogDataFields} |
					   add_clh_option(logData)]),

    LogStorageModeFields = record_info(fields, logStorageMode),
    {atomic, ok} = 
	clhI:mnesia_create_table(logStorageMode, 
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, LogStorageModeFields} |
				  add_clh_option(logStorageMode)]),
    
    %% Create alarm record 
    LogAlarmFields = record_info(fields, logAlarm),
    {atomic, ok} = 
	clhI:mnesia_create_table(logAlarm, [{type, set},
					    {disc_copies, DbNodes},
					    {attributes, LogAlarmFields} |
					    add_clh_option(logAlarm)]),
    
    %% Erlang support information
    {atomic, ok} = 
	clhI:mnesia_create_table(logEsi, [{type, set},
					  {disc_copies, DbNodes},
					  {attributes, [key, value]} |
					  add_clh_option(logEsi)]),

    %% RAM log 
    RamLogFields = record_info(fields, logRamLog),
    {atomic, ok} = 
	clhI:mnesia_create_table(logRamLog,
				 [{type, set},
				  {ram_copies, DbNodes},
				  {attributes, RamLogFields} |
				  add_clh_option(logRamLog)]),
    %% Internal variables
    {atomic, ok} = 
	clhI:mnesia_create_table(logRamVariables,
				 [{type, set},
				  {ram_copies, DbNodes},
				  {attributes, [key,value]} |
				  add_clh_option(logRamVariables)]),
    

    mnesia_dirty_write({logEsi, cb, []}),
    mnesia_dirty_write({logEsi, dir, []}),

    case IsUpgradeOngoing of
	true ->
	    transform(logM),
	    transform(logData),
	    swmI:copy_old_table(log),
	    [begin
		 logI:register_esi_dir(logLib:log_dir(LogName)),
		 set_storage_mode_if_empty(LogName)
	     end
	     || LogName <- get_rcs_logs()],
	    transform(logPushTransfer);
	_ ->
	    mnesia_dirty_write(#logM{logMId = {"1","1","1"}}),

	    Logs = 
		[#log{logId = {"1","1","1", PDMO}} || PDMO <- ?PREDEF_MO_LOGS],
	    [logDb:log_set_dirty(Log) || Log <- Logs],
	    LogData = [#logData{logId            = {"1","1","1", PD},
			        maxSizeKb        = 2 * 1024,
				rotatingSegments = 3,
			        appLog           = is_app_log(PD),
			        internal         = is_internal_log(PD)} || 
			  PD <- ?PREDEF_LOGS],
	    [logDb:log_data_set_dirty(LD) || LD <- LogData],
	    [set_storage_mode(PD) || PD <- ?PREDEF_LOGS]
    end,

    [logI:register_esi_dir(logLib:log_dir(PD)) || PD <- ?RCS_LOGS],

    logI:register_esi_dir(?UPGR_DIR),
    logI:register_esi_dir(?ESI_DIR),

    case sysEnv:rcs_mode_3() of
	hostsim ->
	    ok;
	_Mode ->
	    Root = sysEnv:rcs_root(),
	    logI:register_esi_dir(filename:join(Root, "var/log")),
	    logI:register_esi_dir(filename:join(Root, "var/pmd")) %% HU96800
    end,
    
    %% The case is a bit defensive. Could be removed later. 
    case safs_log:get_root() of
	SafLogDir when is_list(SafLogDir) ->
	    logI:register_esi_dir(SafLogDir);
	_Undef ->
	    ok
    end,

    %% External server
    LogExtServerFields = record_info(fields, logExtServer),
    {atomic, ok} = 
	clhI:mnesia_create_table(logExtServer,
				 [{type, set},
				  {ram_copies, DbNodes},
				  {attributes, LogExtServerFields} |
				  add_clh_option(logExtServer)]),

    LogOldFields = record_info(fields, logOld),
    {atomic, ok} = 
	clhI:mnesia_create_table(logOld, [{type, set},
					  {disc_copies, DbNodes},
					  {attributes, LogOldFields} |
					  add_clh_option(logOld)]),
    
    
    case IsUpgradeOngoing of
	true ->
	    %% swmI:copy_old_table(logCounter),
	    case swmI:is_old_table(logCounter) of
		true ->
		    AllCounters = swmI:all_objects(logCounter),
		    logServer:initiate_counters(AllCounters);
		false ->
		    ok
	    end,
	    swmI:copy_old_table(logExtServer),
	    transform(logInternal),
%%	    transform(logOld),
	    transform(logStorageMode),
	    
	    %% NOTE add this line when the base line on NodeC is updated
	    %% swmI:copy_old_table(logAlarm),
	    
	    %% The SwmInternal log must be registered as an ESI
	    %% log after an upgrade
	    try
		LogName = "SwmInternal",
		Match = #logData{logId = {"1","1","1",LogName}, _ = '_'},
		case mnesia:dirty_match_object(Match) of
		    [_] ->
			logI:register_esi_dir(logLib:log_dir(LogName));
		    _ ->
			sysInitI:warning_msg("no SwmInternal log entry~n", [])
		end
	    catch
		exit:Reason ->
		    sysInitI:warning_msg("no SwmInternal log entry, reason: ~p~n",
					 [Reason])
	    end;
	_ ->
	    ok
    end,
    ok.
 


transform(logM) ->
    [LogM] = swmI:all_objects(logM),
    case LogM of
	{logM,Id, _, NC, TC} ->
	    mnesia_dirty_write(
	      #logM{logMId = Id,
		    nodeCredential = NC,
		    trustCategory= TC});
	{logM,Id,_} ->
	    mnesia_dirty_write(#logM{logMId=Id})
    end;

transform(logPushTransfer) ->
    OldTab = swmI:all_objects(logPushTransfer),
    F = fun({logPushTransfer,Id,Uri,Pwd,Type}) -> 
		mnesia:write(
		  #logPushTransfer{logPushTransferId = Id,
				   uri = Uri,
				   password = Pwd,
				   transferType = Type,
				   operationalState = ?OperState_DISABLED,
				   availabilityStatus =
				       ?AvailStatus_OFF_LINE});
	   ( X = #logPushTransfer{} ) ->
		mnesia:write(X)
		    
	end,
    
    {atomic,_} = mnesia:transaction(
		    fun() ->
			    lists:map(F,OldTab)
		    end);

transform(logInternal) ->
    OldTab     = logLib:get_all_objects(logInternal),
    Fun        = fun (Data) -> transform_log_internal(Data) end,
    {atomic,_} = mnesia:transaction(fun() -> lists:map(Fun, OldTab) end);

%% transform(logOld) ->
%%     OldTab     = swmI:all_objects(logOld),
%%     Fun        = fun (Data) -> mnesia:write(transform_log_old(Data)) end,
%%     {atomic,_} = mnesia:transaction(fun() -> lists:map(Fun, OldTab) end);

transform(logData) ->
    OldTab     = logLib:get_all_objects(logData),
    Fun        = fun (Data) -> logDb:log_data_write(transform_log_data(Data)) end,
    {atomic,_} = mnesia:transaction(fun() -> lists:map(Fun, OldTab) end);

transform(logStorageMode) ->
    OldTab     = logLib:get_all_objects(logStorageMode),
    Fun        = fun (Data) -> mnesia:write(transform_log_sm(Data)) end,
    {atomic,_} = mnesia:transaction(fun() -> lists:map(Fun, OldTab) end).



transform_log_internal({logInternal, Name, _MaxSize, _RotatingSegments}) ->
    case logDb:log_data_get({"1","1","1",Name}) of
	{ok, [Obj]} -> 
	    logDb:log_data_set(Obj#logData{internal = true});
	_     -> 
	    ok
    end;
transform_log_internal(Rec) ->
    Rec.


transform_log_sm({logStorageMode,
		  LogId,
		  Local,
		  Encrypted}) ->
    #logStorageMode{logId      = LogId,
		    local      = Local,
		    encrypted  = Encrypted,
		    compressed = false};
transform_log_sm(Rec) ->
    Rec.


transform_log_data({logData, LogId, MaxSize, RotSegm, AppLog}) ->
    #logData{logId            = LogId,
	     maxSize          = undefined,
	     maxSizeKb        = ?GET_LOG_SIZE_BYTES(MaxSize),
	     rotatingSegments = RotSegm,
	     recordSize       = ?LOG_REC_SIZE_DEF,
	     appLog           = AppLog,
	     internal         = false,
	     milliSec         = false};
transform_log_data({logData, LogId, MaxSize, RotSegm, RecSize, AppLog}) ->
    #logData{logId            = LogId,
	     maxSize          = undefined,
	     maxSizeKb        = ?GET_LOG_SIZE_BYTES(MaxSize),
	     rotatingSegments = RotSegm,
	     recordSize       = RecSize,
	     appLog           = AppLog,
	     internal         = false,
	     milliSec         = false};
transform_log_data({logData, 
		    LogId, 
		    MaxSize, 
		    MaxSizeKb, 
		    RotSegm, 
		    RecSize, 
		    AppLog, 
		    Internal}) ->
    #logData{logId            = LogId,
	     maxSize          = MaxSize,
	     maxSizeKb        = MaxSizeKb,
	     rotatingSegments = RotSegm,
	     recordSize       = RecSize,
	     appLog           = AppLog,
	     internal         = Internal,
	     milliSec         = false};
transform_log_data(Rec) ->
    Rec.




set_storage_mode_if_empty(Name) ->
    case logLib:log_storage_mode_exists(Name) of
	true ->
	    ok;
	_False ->
	    set_storage_mode(Name)
    end.


set_storage_mode(Name) ->
    IsEncrypted  = lists:member(Name, ?PREDEF_ENCRYPTED_LOGS),
    IsLocal      = false,
    IsCompressed = false,
    ok = logLib:log_storage_mode_create(Name, 
					IsLocal, 
					IsEncrypted, 
					IsCompressed).

%%% ----------------------------------------------------------
%%% @doc Activate the RcsLogM model
%%% @end
%%% ----------------------------------------------------------
activate() ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Return the permanent process of the log application
%%% @end
%%% ----------------------------------------------------------
children() ->
    {ok, [
	  {logServer, {logServer, start, []},
	   permanent, 1000, worker, [logServer]},
	  {logRamServer, {logRamServer, start, []},
	   permanent, 1000, worker, [logRamServer]},
	  {logExtServer, {logExtServer, start, []},
	   permanent, 1000, worker, [logExtServer]},
	  {logStreamServer, {logStreamServer, start, []},
	   permanent, 1000, worker, [logStreamServer]},
	  {logEsiExtServer, {logEsiExtServer, start, []},
	   permanent, 1000, worker, [logEsiExtServer]}
	 ]}.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc Initiate Mnesia data tables for LOG.
%%% @end
%%% ----------------------------------------------------------
instPhParallel_init_data() ->
    swmI:register_appdata_receiver("log", logAppData),
    sysServer:register_file_owner("log", logI),
    sysServer:register_file_owner("saf_log", logI).
      
%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc Run post initialization functions
%%% This involves enabling netconf AVC for RcsLogM
%%% @end
%%% ----------------------------------------------------------
instPhParallel_post_init() ->
    comsaI:register_subscriptions(
      "RcsLogM", [{"LogM", logM},
		  {"Log", log},
		  {"LogPushTransfer", logPushTransfer}]),
    System = ["ManagedElement", "SystemFunctions"],
    comsaI:register_callback(System ++ ["LogM"], logModel),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
is_app_log("AlarmLog") -> true;
is_app_log(_)          -> false.

is_internal_log(Log) -> lists:member(Log, ?PREDEF_INT_LOGS). 

create_table(Name, DbNodes, Types) ->
    Fields = [Field || {Field, _} <- Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes,  Fields} |
					add_clh_option(Name)]).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].


%% find only internal RCS logs 
%% HV63844
get_rcs_logs() ->
    Match = #logData{appLog = false, _ = '_'},
    Objs  = mnesia:dirty_match_object(Match),
    [Key || #logData{logId = {_, _, _, Key}} <- Objs].


mnesia_dirty_write(Obj) ->
    mnesia:dirty_write(Obj).



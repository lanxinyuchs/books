%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxberb
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/10

%%% @doc ==Backup handling in cloud==
%%% @end

-module(swmvBackup).
-author('etxberb').

%%% ----------------------------------------------------------
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%% Rev     Date       Name     What
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-09-20 etxjotj  Created 
%% R11A/2  2017-10-01 etxberb  Fixed edoc problem.
%% R11A/3  2017-10-18 etxberb  Moved *init_config* functions from swmBackup.erl.
%% R11A/4  2017-10-19 etxberb  Moved handle_restore_backup from swmBackup.erl.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/2  2017-11-15 etxberb  Continuation of SP277.
%% R12A/3  2017-11-15 etxpejn  Added restoreStart trigger
%% R12A/4  2017-11-17 etxberb  Continuation of SP277.
%% R12A/5  2017-11-20 etxberb  Continuation of SP277.
%% R12A/6  2017-11-24 etxberb  Continuation of SP277.
%% R12A/7  2017-11-27 etxberb  SP277: Fault cases.
%% R12A/10 2017-12-06 etxberb  Starting to use swmvUpgrade.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% REST interface
-export([create_backup/1, create_backup/2]).
-export([create_upgrade_backup/2]).
-export([delete_backup/2]).
-export([export_backup/4]).
-export([get_backup_id_by_name/1]).
-export([remove_prepared_init_config/0]).

%% For SYS
-export([init_config_prepare/0]).

%% For swmServer
-export([init_config_activate/0,
	 is_init_config_enabled/0]).

%% For swmBackup
-export([handle_restore_backup/2]).

%%% For swmBackupScheduler
-export([create_backup_common/4]).

%%% For swmBackupModel
-export([cleanup/0]).

%% For COLI & VNFM
-export([init_config_download/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% #2.3   IMPORTED FUNCTIONS
%%% ----------------------------------------------------------

-import(swmBackup, [rollback_restore_file/1,
		    save_progress_report/1,
		    save_restore_file/3]).

-import(swmBackupFile, [compress_file/2,
			do_remove_backup_dir/1,
			download_file/3,
			file_size/3,
			format_product_data/1,
			get_export_prefix/1,
			get_metadata/1,
			get_next_index/0,
			handle_export_backup/4,
			handle_delete_backup/1,
			open_remote_file/5,
			parse_uri/1,
			start_channel/5,
			store_metadata/2]).

-import(swmBackupModel, [action_handler/2,
			 record2props/1,
			 update_progress/2,
			 set_default_progress_report/3,
			 get_backup_by_name/1,
			 default_idle_pr/0,
			 make_dn/1]).


%%% ----------------------------------------------------------
%%% #2.4   DEFINES
%%% ----------------------------------------------------------
-define(timeAllowedToStartNewNode, timer:hms(1, 0, 0) div 1000).   % Seconds

-record(transfer,
	{scheme,
	 user,
	 host,
	 port,
	 remoteFile,
	 bu_key,
	 pwd,
	 fd,
	 pid,
	 handle,
	 rp_key,
	 total_size,
	 path}).


-include("RcsBrM.hrl").
-include("SwmInternal.hrl").
-include("alhI.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Creates a backup.
%%% @end

create_backup(BuName) ->
    create_backup(BuName, swmLib:get_new_action_id(mgr)).

create_backup(BuName, ActionId) ->
    DnRev = [<<"1">>,<<"BrmBackupManager">>,<<"1">>,<<"BrM">>,
	     <<"1">>,<<"SystemFunctions">>,<<"1">>,<<"ManagedElement">>],

    swmLib:mo_lock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID,
                                  ?CREATE_BACKUP_ACTION_CAPABLE_INFO),
    swmBackupModel:set_default_progress_report(mgr, "CREATE", ActionId),
    Name = list_to_binary(BuName),
    T0 = erlang:monotonic_time(),
    action_handler(fun() -> handle_create_backup(Name, DnRev) end, mgr),
    [Obj] = mnesia:dirty_read({brmBackupManager, {"1","1","1","1"}}),
    Result = record2props(Obj#brmBackupManager.progressReport),
    ok = swmLib:unlock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID),

    Tend = erlang:monotonic_time(),
    sysInitI:info_report(
      [{?MODULE, create_backup},
       {buName, BuName},
       {actionId, ActionId},
       {"TOTAL", sysUtil:time_to_string(Tend - T0)}]),
    {ok, Result}.


%%% ----------------------------------------------------------
%%% @doc Creates a backup to be used for upgrade and copies the backup to
%%%   the init_config directory.
%%% @end
create_upgrade_backup(BuName, ActionId) ->
    {ok, ActionResult} = Res = create_backup(BuName, ActionId),
    case proplists:get_value(result, ActionResult) of
	"SUCCESS" ->
	    init_config_copy(BuName);
	_ ->
	    ok
    end,
    Res.

%%% ----------------------------------------------------------
%%% @doc Exports a backup.
%%% @end
export_backup(BuName, Uri, Passwd, ActionId) ->
    BuKey = get_backup_id_by_name(BuName),
    swmLib:mo_lock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID,
                                  ?EXPORT_BACKUP_ACTION_CAPABLE_INFO),
    swmBackupModel:set_default_progress_report(BuKey, "EXPORT", ActionId),
    T0 = erlang:monotonic_time(),
    action_handler(fun() ->
			   handle_export_backup(BuKey, BuKey, Passwd, Uri)
		   end,
		   BuKey),
    [Obj] = mnesia:dirty_read({brmBackup, BuKey}),
    Result = record2props(Obj#brmBackup.progressReport),
    ok = swmLib:unlock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID),

    Tend = erlang:monotonic_time(),
    sysInitI:info_report(
      [{?MODULE, export_backup},
       {buName, BuName},
       {uri, Uri},
       {passwd, Passwd},
       {actionId, ActionId},
       {"TOTAL", sysUtil:time_to_string(Tend - T0)}]),
    {ok, Result}.


%%% ----------------------------------------------------------
%%% @doc Deletes a backup.
%%% @end

delete_backup(BuName, ActionId) ->
    swmLib:mo_lock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID,
                                  ?DELETE_BACKUP_ACTION_CAPABLE_INFO),
    swmBackupModel:set_default_progress_report(mgr, "DELETE", ActionId),
    Name = list_to_binary(BuName),
    T0 = erlang:monotonic_time(),
    action_handler(fun() -> handle_delete_backup(Name) end, mgr),
    [Obj] = mnesia:dirty_read({brmBackupManager, {"1","1","1","1"}}),
    Result = record2props(Obj#brmBackupManager.progressReport),
    ok = swmLib:unlock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID),
    Tend = erlang:monotonic_time(),
    sysInitI:info_report(
      [{?MODULE, delete_backup},
       {buName, BuName},
       {actionId, ActionId},
       {"TOTAL", sysUtil:time_to_string(Tend - T0)}]),
    {ok, Result}.

%% #############################################################################
%% @doc Prepare the Initial Configuration.
%%   Unpack the InitConfig and move the data file to upgrade_init.
%%   Used by sysDbServer.
%%
%% @end
%% ###=======================================================================###
init_config_prepare() ->
    swmvUpgrade:start(#{trigger => node_restart}),
    {ok = Result, RestoreInfo} =
	init_config_prepare(sysDbServer:is_install_complete(),
			    swmvUpgrade:is_prepared(),
			    swmvBuRestore:is_prepared()),
    swmvBuRestore:start(maps:remove(make_release_actions,
				    RestoreInfo#{trigger => node_restart})),
    Result.

init_config_prepare(false, true, false) ->
    ?LOG_INFO(["Upgrade detected"]),
    try
	ok = init_config_copy_shared(),
	ok = init_config_unpack(),
	ok = init_config_enable(),
	?LOG_INFO(["Prepare complete"]),
	{ok, #{}}
    catch
	throw : {inconsistent_software, SwVersions} ->
	    [begin
		 Msg = "Required software: " ++ format_product_data(Sw),
		 swmLib:write_swm_log("InitConfig", error, Msg),
		 ?LOG_INFO([Msg])
	     end
	     || Sw <- SwVersions],
	    ?LOG_INFO(["The applicable software is not installed"]),
	    remove_prepared_init_config(),
	    throw({inconsistent_software, SwVersions});
	throw : {already_exist, UpgrInitFile} ->
	    remove_prepared_init_config(),
	    ?LOG_WARN([{"Already exist", UpgrInitFile},
		       {"Ignoring and removing", swmLib:init_config_dir()}]),
	    {ok, #{}};
	ExcC : ExcR ->
	    Callstack = ?STACKTRACE_E,
	    remove_prepared_init_config(),
	    ?LOG_ERR([{ExcC, ExcR}, {callstack, Callstack}]),
	    erlang:ExcC(ExcR)
    end;
init_config_prepare(_, false, true) ->
    ?LOG_INFO(["Backup Restore detected"]),
    try
	{ok, #{}} = Result = init_config_restore(),
	?LOG_INFO(["Prepare complete"]),
	Result
    catch
	ExcC : ExcR ->
	    Callstack = ?STACKTRACE_E,
	    remove_prepared_init_config(),
	    ?LOG_ERR([{ExcC, ExcR}, {callstack, Callstack}]),
	    erlang:ExcC(ExcR)
    end;
init_config_prepare(I, U, R) ->
    ?LOG_INFO([{install_complete, I},
	       {upgrade, U},
	       {restore, R}]),
    {ok, #{}}.

%% #############################################################################
%% init_config_restore
%%
%% ###=======================================================================###
init_config_restore() ->
    %% The symbolic link from <home>/restore to <swm>/backup/<Index> has been
    %% created by make_release.escript in the SYS block.
    InitRestoreFile = swmLib:init_restore_file(),
    {ok, Bin} = file:read_file(InitRestoreFile),
    RestoreInfo = binary_to_term(Bin),   % Contains #{bu_index := BuIndex}
    ?LOG_INFO([{InitRestoreFile, RestoreInfo}]),
    verify_restore_link(),
    RRF_Init = rollback_restore_file(swmLib:init_config_dir()),
    case filelib:is_file(RRF_Init) of
	true ->
	    RRF_Home = rollback_restore_file(sysEnv:home_dir()),
	    filelib:ensure_dir(RRF_Home),
	    {ok, _} = file:copy(RRF_Init, RRF_Home);
	false ->
	    ok
    end,
    {ok, RestoreInfo}.

%% #############################################################################
%% verify_restore_link
%%
%% ###=======================================================================###
verify_restore_link() ->
    case filelib:is_dir(swmLib:restore_dir()) of
	true ->
	    ok;
	false ->
	    %% The make_release.escript has not been able to create the link.
	    throw("No symbolic link to a restore-backup found.")
    end.

%% #############################################################################
%% @doc Activate the Initial Configuration.
%%
%% @end
%% ###=======================================================================###
init_config_activate() ->
    case is_init_config_enabled() of
	false ->
	    false;
	true ->
	    swmLib:erase_variable(bu_restore),
	    remove_prepared_init_config(),
	    ?LOG_INFO(["Activate complete"]),
	    true
    end.

%% #############################################################################
%% @doc Check if the Initial Configuration is in place.
%%
%% @end
%% ###=======================================================================###
is_init_config_enabled() ->
    filelib:is_file(filename:join(swmLib:init_config_dir(), "enabled")).

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------

cleanup() ->
    ok.

%% #############################################################################
%% @doc Remove the Initial Configuration.
%%
%% @end
%% ###=======================================================================###
-spec remove_prepared_init_config() -> ok.
remove_prepared_init_config() ->
    cmd("rm -rf " ++ swmLib:init_config_dir()),
    cmd(["rm -rf ", swmLib:restore_dir()]),   % Added by SYS:make_release.escript
    ok.

%%% ----------------------------------------------------------
%%% @doc Download a backup, to be used at upgrade of vRCS.
%%%   Used by sysColi and VNFM.
%%% @end
%%% ----------------------------------------------------------
init_config_download(Uri, Pwd) ->
    UriBase = filename:basename(Uri),
    InitMsg = "Downloading init_config: " ++ UriBase,
    ?LOG_INFO([InitMsg]),
    logI:write_log("SwmInternal", ?MODULE_STR, info, InitMsg),
    update_progress(coli, [{additionalInfo, "Starting transport"},
			   {backupFileName, UriBase}]),
    {ok, {Proto, User, Host, Port, RemoteDir, _Query}} = parse_uri(Uri),
    {ok, Pid, _CRef} = start_channel(Proto, Host, Port, User, Pwd),
    TotalSize = file_size(Proto, Pid, RemoteDir), 
    BuFile = swmLib:init_upgrade_file(),
    ok = filelib:ensure_dir(BuFile),
    case file:open(BuFile, [write, raw, binary]) of
	{ok, Fd} ->
	    {ok, Handle} =
		open_remote_file(Proto, coli, Pid, RemoteDir, [read, binary]),
	    Params = 
		#transfer{scheme = Proto,fd = Fd,
			  pid = Pid,
			  handle = Handle,
			  rp_key = coli,
			  total_size = TotalSize,
			  path = BuFile},
	    download_file(Params, 0, ftpI:read(Proto, Pid, Handle, 65536)),
 	    ftpI:close(Proto, Pid, Handle),
	    file:close(Fd),
	    update_progress(coli, [{additionalInfo, "Transport complete."}]),
	    EndMsg = "init_config downloaded: " ++ UriBase,
	    ?LOG_INFO([EndMsg,
		       {"from", filename:dirname(Uri)}]),
	    logI:write_log("SwmInternal", ?MODULE_STR, info, EndMsg),
	    ok;
	{error, Reason} ->
	    Info = file:format_error(Reason),
	    update_progress(coli, [{additionalInfo, Info}]),
	    erlang:error(Reason)	    
    end.

%% #############################################################################
%% handle_restore_backup
%%
%%   Called from swmBackup.erl.
%% ###=======================================================================###
handle_restore_backup(Key, Type) ->
    ?LOG_INFO([{key, Key}, {type, Type}]),
    %% SETUP 
    swmvBuRestore:start(#{trigger  => mo_action,
			  bu_index => element(5, Key)}),
    appmI:inhibit_escalation(),
    Index = element(5, Key),
    DN = "BrmBackup=" ++ Index,
    #{backupName := BuName,
      swVersion  := SwVersion} = Metadata = get_metadata(Index),
    swmLib:write_swm_log(DN, info, "Restore: " ++ BuName),
    info_msg("Restoring backup ~s: ~p~n", [Index, BuName]),
    update_progress(Key, [{additionalInfo, "Restoring " ++ BuName},
			  {progressPercentage, 1}]),
    save_progress_report(Key),
    swmLib:set_ram_variable(restore_backup_active, true),
    case
	{swmInventory:get_current_sw_version(),
	 maps:get(vnfdId, Metadata, false),
	 swmvBuRestore:appCheck_restoreStart(Key)}
	of
	{SwVersion, _, ok} ->
	    %% No change of software.
	    RestoreDir = swmLib:restore_dir(),
	    filelib:ensure_dir(RestoreDir),
	    info_msg("Making symlink ~p -> ~p ~n",
		     [RestoreDir, swmLib:backup_dir(Index)]),
	    ok = file:make_symlink(swmLib:backup_dir(Index), RestoreDir),
	    Msg = "Backup restored. Rebooting...",
	    swmLib:write_swm_log(DN, info, Msg),
	    update_progress(Key, [{additionalInfo, Msg},
				  {progressPercentage, 55}]),
	    save_progress_report(Key),

	    swmLib:set_variable(restore_in_progress, BuName),

	    %% info_msg("#### RESTART OMITTED FOR TEST PURPOSES~n",[]),
	    swmLib:order_restart_node(cold, ?ALH_TAG_DataRestore),
	    self() ! stop_receiving_requests;
	{_, VnfdId, ok} when is_list(VnfdId) ->
	    AddInfo = lists:flatten([?ALH_TAG_RankCold,
				     ?ALH_TAG_Cause(?ALH_TAG_SoftwareRestore)]),
	    alhI:write_node_event(os:timestamp(),
				  ?ALH_TAG_OutOfService,
				  ?ALH_TAG_ShutdownCommand,
				  0,   % EventId
				  ?ALH_TAG_Rcs(AddInfo)),
	    update_progress(Key, [{additionalInfo,
				   "Installing upgrade package"},
				  {progressPercentage, 18}]),
	    save_progress_report(Key),
	    RestoreInfoFile = swmLib:init_restore_file(),
	    filelib:ensure_dir(RestoreInfoFile),
	    MR_Actions = make_release_escript_actions(Index),
	    RestoreInfo = #{bu_index => Index,
			    make_release_actions => MR_Actions},
	    info_msg("Making Init Config file ~p -> ~p ~n",
		     [RestoreInfoFile, Index]),
	    ok = file:write_file(RestoreInfoFile, term_to_binary(RestoreInfo)),
	    save_restore_file(Type, Key, swmLib:init_config_dir()),
	    VnfmData = #{vnfd_id => VnfdId,
			 fallback_timeout => fallback_timeout()},
	    case swmvBuRestore:activate(#{vnfm_data => VnfmData}) of
		ok ->
		    Msg = "Backup restored. Activating new VNF instance...",
		    swmLib:write_swm_log(DN, info, Msg),
		    self() ! stop_receiving_requests;
		undefined ->
		    ?LOG_ERR(["swmvBuRestore:activate failed"]),
		    swmBackup:aborted(Key, #{resultInfo => "Software error"})
	    end;
	{_, false, _} ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    ResultInfo =
		"The backup does not contain information about VNFD-ID",
	    swmLib:write_swm_log(DN, alert, "Restore failed: " ++ ResultInfo),
	    update_progress(Key, [{result, ?ActionResultType_FAILURE},
				  {resultInfo, ResultInfo},
				  {progressPercentage, 100},
				  {state, ?ActionStateType_FINISHED},
				  {timeActionCompleted, CompleteTime}]),
	    swmvBuRestore:stop({ok, ResultInfo});
	{_, _, error} ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    ResultInfo =
		"Application does not allow restore to continue",
	    swmLib:write_swm_log(DN, alert, "Restore failed: " ++ ResultInfo),
	    update_progress(Key, [{result, ?ActionResultType_FAILURE},
				  {resultInfo, ResultInfo},
				  {progressPercentage, 100},
				  {state, ?ActionStateType_FINISHED},
				  {timeActionCompleted, CompleteTime}]),
	    swmvBuRestore:stop({ok, ResultInfo})
    end,
    ok.

%% #############################################################################
%% make_release_escript_actions
%%
%% ###=======================================================================###
make_release_escript_actions(Index) ->
    BackupDir = swmLib:backup_dir(Index),
    RestoreDir = swmLib:restore_dir(),
    LogInfo = lists:flatten(io_lib:format("Making symlink ~s -> ~s ~n",
					  [RestoreDir, BackupDir])),
    [#{mfa => {file, make_symlink, [BackupDir, RestoreDir]},
       expectedMfaResult => ok,
       logInfo => LogInfo}].

%% #############################################################################
%% fallback_timeout
%%
%% ###=======================================================================###
fallback_timeout() ->
    case ets:tab2list(brmRollbackAtRestore) of
	[#brmRollbackAtRestore{timeAllowedBeforeRollback = TABR}]
	when is_integer(TABR) ->
	    TABR;
	[#brmRollbackAtRestore{timeAllowedBeforeRollback = undefined}] ->
	    ?timeAllowedToStartNewNode
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Create a backup
%%% ===Arguments===
%%% Name - The name of the backup
%%% DnRev - The reversed distinguished name to the BrmBackupManager object
%%% ActionId - The sequential action id of this action

handle_create_backup(Name, DnRev)->
    swmLib:write_swm_log("BrmBackupManager", info, 
			 "Creating backup "++binary_to_list(Name)),

    case swmBackupModel:validate_name(Name) of
	ok -> 
	    ok;
	duplicate_name ->
	    throw({fail, "Duplicate name"})
    end,

    case swmBackupModel:housekeeping() of
	ok -> 
	    ok;
	max_reached ->
	    throw({fail, "Housekeeping required"});
	{cleaned, _} ->
	    ok
    end,
    %% HU82011 Prevent create when in failsafe
    case swmFailsafe:get_usage_state() of
	idle -> ok;
	_ ->
	    Msg = "Failsafe is active. Deactivate failsafe before creating a "
		"backup",
	    throw({fail, Msg})
    end,

    %% The timeout is mainly for testing
    Timeout = 
	case swmLib:get_variable(create_backup_cancel_timeout) of
	    T when is_integer(T), T > 300000 ->
		swmLib:write_swm_log("BrmBackupManager", info, 
				     "Create backup timeout is activated"),
		300000;
	    T when is_integer(T), T =< 300000 -> 
		swmLib:write_swm_log("BrmBackupManager", info, 
				     "Create backup timeout is activated"),
		T;
	    _ -> 0
	end,
    receive
	cancel_mgr -> 
	    swmLib:write_swm_log("BrmBackupManager", info, "Create backup cancelled"),
	    throw(cancelled)
    after Timeout -> 
	    swmLib:set_ram_variable(mgr_point_of_no_return, true)
    end,

    BrmBackupManagerKey = comsaGeneric:dnrev_to_key(DnRev), 
    Index = get_next_index(),
    MoRef =  do_create_backup_common(Index, Name, BrmBackupManagerKey,
				     manual, mgr),
    swmLib:unlock_backup(Index),
    swmLib:write_swm_log("BrmBackupManager", info, "Created "++MoRef),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(mgr, [{result, ?ActionResultType_SUCCESS},
			  {resultInfo, MoRef},
			  {progressPercentage, 100},
			  {state, ?ActionStateType_FINISHED},
			  {timeActionCompleted, CompleteTime}]),
    swmLib:erase_ram_variable(mgr_point_of_no_return),
    case get_autoexport(BrmBackupManagerKey) of
	disabled ->
	    ok;
	{enabled, Uri, Password} ->
	    NextId = swmLib:get_new_action_id(mgr),
	    BuKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++[Index]),
	    set_default_progress_report(mgr, "MANUAL_EXPORT", NextId),
	    handle_export_backup(BuKey, mgr, Password, Uri)
    end.

get_autoexport(BrmBackupManagerKey) ->
    case mnesia:dirty_read({brmBackupManager,BrmBackupManagerKey}) of
	[Obj] ->
	    case Obj#brmBackupManager.autoExport of
		?BrmAutoExport_DISABLED -> disabled;
		?BrmAutoExport_ENABLED ->
		    Password = comsaI:decrypt_password(
				 Obj#brmBackupManager.autoExportPassword),
		    {enabled, Obj#brmBackupManager.autoExportUri, Password}
	    end;
	[] ->
	    disabled
    end.
-type backup_key() :: {string(), string(), string(), string(), string()}.
-type key_type() :: mgr | schedule | export | failsafe | undefined | backup_key().

-spec create_backup_common(
	Name::binary(), 
	BrmbackupManagerKey::{string(), string(), string(), string()},
	Type::manual|scheduled|system,
	Progress::key_type()) -> RDN::string().

create_backup_common(Name, BrmBackupManagerKey, Type, Progress) ->
    update_progress(Progress, [{additionalInfo, "Name: "++binary_to_list(Name)}]),
    Index = get_next_index(),

    %% HU30730
    %% Make sure all post processing is done before allowing removal of backups
    %% So let the caller make the unlock call in normal cases

    try do_create_backup_common(Index,Name,BrmBackupManagerKey,Type,Progress)
    catch T:E ->
	    Stacktrace = ?STACKTRACE_E,
	    %% Cleanup after failed create backup
	    %% A lock is in place here already, avoid setting another one
	    error_msg("{~p, ~p}~n~p~n",[T,E,Stacktrace]),
	    do_remove_backup_dir(Index),
	    BrmBackupKey = 
		list_to_tuple(tuple_to_list(BrmBackupManagerKey)++[Index]),
	    {atomic, ok} = 
		mnesia:transaction(
		  fun() -> mnesia:delete({brmBackup, BrmBackupKey}) end),
	    swmLib:unlock_backup(Index),
	    erlang:T(E)
    end.    
	
do_create_backup_common(Index, Name, BrmBackupManagerKey, Type, Progress) ->

    StartTime = comsaI:iso_time(os:timestamp(), extended),
    Memory = lists:sum([mnesia:table_info(T, memory)||
			   T<-mnesia:system_info(tables)]),
    NeededSize = Memory+500000, %% Guestimate for other backup content
    FreeDisk = swmLib:get_free_disk(),
    info_msg("Database size: ~w~n",[NeededSize]),
    info_msg("Free disk:  ~w bytes~n",[FreeDisk]),

    case FreeDisk of
    	FreeDisk when FreeDisk > NeededSize ->
    	    ok;
    	FreeDisk ->
    	    throw({fail, "Create failed due to disk space shortage. "++
    		       integer_to_list(NeededSize div 1024)++" k needed. "++
    		       integer_to_list(FreeDisk div 1024)++" k available"})
    end,

    BuDir = swmLib:backup_dir(Index),
    BuPath = filename:join(BuDir, "mnesia_backup"),
    ok = filelib:ensure_dir(BuPath),
    put(buPath, BuPath),

    update_progress(Progress, [{additionalInfo, "Creating database backup"}]),

    %% Update labelstore, so that the lastCreatedBackup will be updated
    %% if this backup is used for restre
    LsKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++["1"]),
    Fun = fun() -> 
		  %% case Progress of
		  %%     undefined -> ok;
		  %%     _ -> 
			  [LS] = mnesia:read({brmBackupLabelStore, LsKey}),
			  mnesia:write(
			    LS#brmBackupLabelStore{
			      lastCreatedBackup=binary_to_list(Name)}),
			  LS
		  %% end
	  end,
    {atomic, PreviousLS} = mnesia:transaction(Fun),
    swmLib:set_variable(bu_restore, Name),
    try swmLib:mnesia_backup(BuPath) of
	ok -> 
	    {0, _} = swmOs:cmdres(["cd ", BuDir, " ; gzip ", BuPath]);
	{error, 
	 {error, {"Tab copier iteration failed", {file_error, _, enospc}}}} ->
	    mnesia:dirty_write(PreviousLS),
	    throw({fail, "Could not create backup beacuse of disk space shortage"});
	{error, Reason} ->
	    mnesia:dirty_write(PreviousLS),
	    erlang:error(Reason, 
			 [Index, Name, BrmBackupManagerKey, Type, Progress])
    after 
	swmLib:erase_variable(bu_restore)
    end,

    SwVersion = swmInventory:get_current_sw_version(),
    CreationType = case Type of 
		       manual -> ?BrmBackupCreationType_MANUAL;
		       scheduled -> ?BrmBackupCreationType_SCHEDULED
		       %% system -> ?BrmBackupCreationType_SYSTEM_CREATED
		   end,

    HsiEnabled =
	case swmLib:get_variable(hsi_state) of
	    on ->
		true;
	    _ ->
		false
	end,
    Metadata = #{metadata => v3,
		 backupName => binary_to_list(Name),
		 creationTime => StartTime,
		 swVersion => SwVersion,
		 creationType => CreationType,
		 confirmCapable => true,
		 hsiEnabled => HsiEnabled,
		 vnfdId => swmLib:vnfd_id()},
    
    store_metadata(Index, Metadata),


    [cmd(["cd ",sysEnv:home_dir()," ; tar cfz ",BuDir, "/", Dir,".tgz ",
	  "--exclude='releases/*/comte/comea/run/*' ", Dir])||
	Dir <- ["bin", "releases"]],
    
    case swmLib:get_variable(bu_callbacks) of
	undefined -> ok;
	Callbacks ->
	    %% Create application data directory
	    AppDir = cmd("mktmp -d /tmp/backup.XXXXX/appdirs")--"\n",
	    [begin
		 ThisAppDir = filename:join([AppDir, atom_to_list(Module),
					     atom_to_list(Function)]),
		 cmd(["mkdir -p ", ThisAppDir]),
		 try apply(Module, Function, [ThisAppDir])
		 catch T:E ->
			 error_msg("~w:~w(~p) failed on ~p:~p~n",
				   [Module, Function, ThisAppDir, T,E])
		 end
	     end||{Module, Function}<-Callbacks],
	    cmd(["cd ", filename:dirname(AppDir), " ; tar cfz ", BuDir, 
		 "/appdirs.tgz *"]),
	    cmd(["rm -rf ", filename:dirname(AppDir)])
    end,

    case sysEnv:rcs_mode_2() of
	vrcs ->
	    %% Temporarily add cert keys to backup
	    cmd(["cd ",sysEnv:home_dir(),
		 " ; tar cfz ", BuDir, "/cert.tgz /rcs/cert"]);
	_ ->
	    ok
    end,

    %% Add backup in BrM
    BrmBackupKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++[Index]),
    BrmBackup = #brmBackup{brmBackupId = BrmBackupKey,
			   backupName = binary_to_list(Name),
			   creationTime = StartTime,
			   status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
			   swVersion = SwVersion,
			   progressReport = default_idle_pr(),
			   creationType = CreationType},
    mnesia:dirty_write(BrmBackup),


    make_nms_bu_info(BuDir, binary_to_list(Name), StartTime, 
		     BrmBackupManagerKey, BrmBackup),
    case swmLib:get_variable(swm_failsafe_backup_SUITE_create_fail) of
	true ->
	    erlang:error("Simulated failure");
	_ -> 
	    ok
    end,
    swmLib:sync(),
    {_, _, BrM, BMgr} = BrmBackupManagerKey,
    %% HS66152 fix
    make_dn([
	     %% "ManagedElement", ME,
	     %% "SystemFunctions", SF,
	     "BrM",BrM,
	     "BrmBackupManager",BMgr,
	     "BrmBackup",Index]).


make_nms_bu_info(BuDir, Name, StartTime, BrmBackupManagerKey, BrmBackup) ->
    {atomic, [BrmBuMgr]} = 
	mnesia:transaction(
	  fun() ->  mnesia:read({brmBackupManager, BrmBackupManagerKey}) end),
    MeData = comsaI:get_managed_element_data(),
    DnPrefix = proplists:get_value(dnPrefix, MeData),
    MeType = proplists:get_value(managedElementType, MeData),
    NMEI = proplists:get_value(networkManagedElementId, MeData),
    SiteLocation = proplists:get_value(siteLocation, MeData),
    
    %% This information in the managedElement MO does actually come from SWM
    UpData = swmLib:get_current_up_metadata(),
    Release = proplists:get_value(release, UpData),
    MeAttrs = 
	[{id, case NMEI of 
		  undefined -> ["1"];
		  _ -> [NMEI] % HS77223
	      end},
	 {type, [MeType]}]++
	case SiteLocation of
	    undefined -> [];
	    _ -> [{sitelocation, [SiteLocation]}]
	end ++
	case DnPrefix of
	    undefined -> [];
	    _ -> [{dnPrefix, [DnPrefix]}]
	end ++
	[{release, [Release]}],

    SwIM = [{softwareVersion, 
	     [{productRevision, [Pd#'ProductData'.productRevision]},
	      {productNumber, [Pd#'ProductData'.productNumber]},
	      {productName, [Pd#'ProductData'.productName]}], []}
	    ||Pd<-swmInventory:get_current_sw_version()],
    %% [{ProdName, ProdNr, ProdVsn}] = swmModel:get_current_up(),

    CreationType =
	case BrmBackup#brmBackup.creationType of
	    ?BrmBackupCreationType_MANUAL -> "MANUAL";
	    ?BrmBackupCreationType_SCHEDULED -> "SCHEDULED"%;
	    %% ?BrmBackupCreationType_SYSTEM_CREATED -> "SYSTEM_CREATED"    
							 
	end,

    %% HS77223 fix
    %% The value of exportPackageLabelPrefix should be reflected in the
    %% user label element of the backup
    UserLabel = get_export_prefix(MeData),
    %% End of fix
    Path = filename:join(BuDir, "backupinfo.xml"),

    SimpleXML = [{backup, [{name, Name}, {version, "1"}], 
		  [{creationTime, [StartTime]},
		   {userLabel, [UserLabel]},
		   {type, [BrmBuMgr#brmBackupManager.backupType]},
		   {creationType, [CreationType]},
		   {domain, [BrmBuMgr#brmBackupManager.backupDomain]},
		   {managedElement, MeAttrs, []},
		   {softwareInventory, [], SwIM},
		   %% This will be replaced at export
		   {exportTime, [], [StartTime]}
		  ]
		 }
		],
						%    info_msg("NMS Backup info: ~n~p~n",[SimpleXML]),
    Result = xmerl:export_simple(SimpleXML, xmerl_xml, []),
    {ok, Fd} = file:open(Path, [write]),
    io:format(Fd, Result, []),
    file:close(Fd).



%%% ----------------------------------------------------------
%%% @doc Copy a backup, to be used at upgrade of vRCS.
%%% @end
%%% ----------------------------------------------------------
init_config_copy(BuKey) when is_tuple(BuKey) ->
    InitBuFile = swmLib:init_upgrade_file(),
    ok = filelib:ensure_dir(InitBuFile),
    Index = element(5, BuKey),
    sysInitI:info_report(
      [{?MODULE, init_config_copy},
       "Preparing backup for upgrade",
       {index, Index},
       {name, get_backup_name(BuKey)}]),
    BuDir = swmLib:backup_dir(Index),
    ETime = os:timestamp(),
    TmpPath = compress_file(BuDir, ETime),
    {ok, Size} = file:copy(TmpPath, InitBuFile),
    file:delete(TmpPath),
    sysInitI:info_report(
      [{?MODULE, init_config_copy},
       {"Copied to", InitBuFile},
       {size, Size}]),
    ok;
init_config_copy(BuName) when is_list(BuName) ->
    init_config_copy(get_backup_id_by_name(BuName)).

%%% ----------------------------------------------------------
init_config_copy_shared() ->
    LcmBackupFile = swmLib:init_backup_lcm_file(),
    case filelib:is_file(LcmBackupFile) of
	true ->
	    SwmBackupFile = swmLib:init_upgrade_file(),
	    ok = filelib:ensure_dir(SwmBackupFile),
	    {ok, Size} = file:copy(LcmBackupFile, SwmBackupFile),
	    ?LOG_INFO(["Init backup from LCM", LcmBackupFile, {"Size", Size}]),
	    ok;
	false ->
	    ok
    end.

%%% ----------------------------------------------------------
init_config_enable() ->
    ?LOG_INFO(["Preparing for data upgrade..."]),
    UpgrInitFile = swmLib:upgrade_init_file(),
    init_config_enable(filelib:is_file(UpgrInitFile), UpgrInitFile).

init_config_enable(false, UpgrInitFile) ->
    BuDir = swmLib:init_config_dir(),
    [BuDb] = filelib:wildcard(filename:join([BuDir, "mnesia_backup*"])),
    {0, _} = swmOs:cmdres(["cp -f ", BuDb, " ", UpgrInitFile]),
    file:write_file(filename:join(BuDir, "enabled"), <<>>),
    ok;
init_config_enable(true, UpgrInitFile) ->
    throw({already_exist, UpgrInitFile}).

%%% ----------------------------------------------------------
init_config_unpack() ->
    BuFile = swmLib:init_upgrade_file(),
    ?LOG_INFO([{"Unpacking", BuFile}]),
    BuDir = swmLib:init_config_dir(),
    cmd(["cd ", BuDir, " ; unzip ", BuFile]),
    {ok, Bin} = file:read_file(filename:join(BuDir, "export.bup")),
    Cleartext = crypto:block_decrypt(aes_cfb128,
				     <<"1234567890ABCDEF">>,
				     <<"1234567890ABCDEF">>, Bin),
    ok = file:write_file(filename:join(BuDir, "export.tgz"), Cleartext),
    cmd(["cd ", BuDir, " ; tar xfz export.tgz"]),
    file:delete(filename:join(BuDir, "export.bup")),
    file:delete(filename:join(BuDir, "export.tgz")),

    cmd(["cd / ; tar xfz ", BuDir, "/cert.tgz"]),

    swmLib:sync(),

    ?LOG_WARN(["No validation made of UP compatibility."]),

    %% MetadataProps = metadata_props(get_metadata(BuDir)),
    %% ?LOG_INFO(["Unpack complete.",
    %% 	       "------- Backup metadata below: -------"
    %% 	       | MetadataProps]),
    ok.

%%% ----------------------------------------------------------
get_backup_id_by_name(BuName) ->
    case get_backup_by_name(BuName) of
	[#brmBackup{brmBackupId = BuKey}] ->
	    BuKey;
	_ ->
	    undefined
    end.

%%% ----------------------------------------------------------
get_backup_name(BuKey) when is_tuple(BuKey) ->
    case mnesia:dirty_read(brmBackup, BuKey) of
	[#brmBackup{backupName = Name}] ->
	    Name;
	_ ->
	    "undefined"
    end.

%%% ----------------------------------------------------------
%%% @doc Execute a shell command and print the result in the erlang shell

cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.

%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    try
	sysInitI:info_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:info_msg("~w: "++Format, [?MODULE|Args])
    end.

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     try
%% 	sysInitI:warning_msg("~w: "++Format, [?MODULE|Args])
%%     catch
%% 	_ : _ ->
%% 	    %% During startup, before sysInitI has started!
%% 	    error_logger:warning_msg("~w: "++Format, [?MODULE|Args])
%%     end.

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    try
	sysInitI:error_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:error_msg("~w: "++Format, [?MODULE|Args])
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


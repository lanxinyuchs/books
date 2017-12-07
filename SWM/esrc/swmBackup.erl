%49%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmBackup.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/8

%%% @doc ==Backup managmement model==
%%% This module contains all the functionality for creating and using
%%% backups

-module(swmBackup).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/8').

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
%%% -----      -------    --------    ------------------------
%%% OLDER HISTORY AT THE END OF THE FILE
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-08-03 etxberb  Bootfallback enabled:
%%                             * Using --no-mount option.
%%                             * Aligned with Fast Restore.
%% R11A/3  2017-08-07 etxjotj  Reimplementation of cancel backup restore
%% R11A/4  2017-08-09 etxjotj  Bugfix in cancel
%% R11A/5  2017-08-11 etxjotj  Don't install bootfallback while waiting for 
%%                             restore confirm. Wait until after confirm
%% R11A/7  2017-08-16 etxberb  Fast Restore checks BootFallback flag.
%% R11A/8  2017-08-21 etxberb  Added check of is_bootfallback_enabled.
%% R11A/9  2017-09-04 etxberb  HW25300: Inhibit unlock of audit_sw when reboot.
%% R11A/10 2017-09-13 etxjotj  Moved MOM implementation to swmBackupModel
%% R11A/11 2017-09-15 etxjotj  Moved import, export, and delete to swmBackupFile
%% R11A/12 2017-09-15 ecotjos  HW16373
%% R11A/16 2017-10-06 etxjotj  Activated confirm restore
%% R11A/17 2017-10-18 etxberb  Moved *init_config* functions to swmvBackup.erl.
%% R11A/18 2017-10-19 etxberb  Moved handle_restore_backup to swmvBackup.erl.
%% R11A/19 2017-10-20 etxberb  Added swmvBuRestore:start.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/3  2017-11-17 etxberb  Continuation of SP277.
%% R12A/5  2017-11-24 etxberb  SP277: Added aborted/2.
%% R12A/6  2017-11-27 etxberb  SP277: Cancel action.
%% R12A/7  2017-11-28 etxberb  HW46754: Added LOCKOBJ_audit_sw for fast_restore.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0, init_bu_info/0, activate/0]).

-export([aborted/2]).

%%% For comsaEvent

%%% For upgrade/failsafe/fallback list
-export([get_mnesia_file/1]).
-export([delete_system_created_backup/2]).
-export([is_progress_report/0]).
-export([internal_housekeeping/0]). % HS51856
%% -export([delete_all_backups/0]).
-export([restore_backup/1]).
-export([install_bootfallback/0, install_bootfallback/1]).

%%% For scheduled backups, failsafe, esi
-export([create_backup_common/4]).
-export([failsafe_lock/0, failsafe_release/0]).

%%% For backup model
-export([rollback_restore_file/1, cleanup/0]).

%%% For backup file
-export([is_protected_backup/1]).

%% For swmvBackup & swmvBuRestore
-export([backup_object/1,
	 backup_object_file/1,
	 confirm_restore_cont/1,
	 save_progress_report/1,
	 save_restore_file/3]).

%% For ESI
-export([print_inventory/1]).

%% For AIC
-export([make_ai_backup/0, clear_ai_backup/0]).

%%% General purpose
-export([has_dependent_backups/1]).

%%% For test use
-export([scratch_backups/0]).
-export([force_restore/0]).
-export([restore_lab_settings/0]).
-export([update_house_keeping/0]).
-export([test_create_backup/2, test_internal_housekeeping/0]).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

%%% ----------------------------------------------------------
%%% #2.3   IMPORTED FUNCTIONS
%%% ----------------------------------------------------------

-import(swmBackupModel, [set_default_progress_report/3,
			 default_idle_pr/0,
			 default_progress_report/2,
			 get_backup_by_name/1,
			 update_progress/2,
			 make_dn/1,
			 record2props/1, 
			 action_handler/2]).

-import(swmBackupFile, [remove_backup_dir/1,
			do_remove_backup_dir/1,
			get_export_prefix/1, 
			delete_backup_internal/2,
			get_next_index/0,
			get_metadata/1,
			store_metadata/2,
			convert_metadata/1]).

%% For vRAN upgrade, check if this should remain
-import(swmBackupFile, [compress_file/2,
			start_channel/5, 
			file_size/3,
			open_remote_file/5,
			download_file/3,
			format_product_data/1,
			handle_delete_backup/1,
			handle_export_backup/4,
			parse_uri/1]).

%%% For AI backup
-import(swmBackupFile, [do_handle_import_backup/3]).


%%% ----------------------------------------------------------
%%% #2.4   INCLUDES
%%% ----------------------------------------------------------

-include("RcsBrM.hrl").
-include("RcsSwIM.hrl").
-include("SwmInternal.hrl").
-include("comte_types.hrl").
-include("alhI.hrl").
-include_lib("kernel/include/file.hrl").

%%% ----------------------------------------------------------
%%% #2.4   DEFINES
%%% ----------------------------------------------------------


-compile([nowarn_unused_vars]).

%%% #3.    CODE

%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the swmBackup server process
%%% @end

start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).




%%% ----------------------------------------------------------
get_mnesia_file(Arg) when is_list(Arg) ->
    Index = 
	case string:tokens(Arg, "=") of
	    [Ix] -> %% A MO ref would contain the = character
		Ix;
	    Tokens -> %% This was probably a MO ref
		Tokens = string:tokens(Arg, "="),
		lists:last(Tokens)
	end,
    BuDir = swmLib:backup_dir(Index),
    {ok, filename:join(BuDir, "mnesia_backup")}.

%%% ----------------------------------------------------------
%%% @doc Called after complete startup
%%% This function is called by sysApp after a complete startup.
%%% @end
%%% ----------------------------------------------------------

activate() ->
    info_msg("Removing bu_restore flag~n"),
    swmLib:erase_variable(bu_restore),
    gen_server:cast(swmBackup,  restore_ai_bu),
    swmFallbackList:audit_fallback_list(),
    internal_housekeeping(),
    ok.

%%% ----------------------------------------------------------
%%% @doc Used when VNFM aborts the action.
%%% @end
%%% ----------------------------------------------------------
aborted(Key, OptionalInfo) ->
    swmBackup ! {aborted, Key, OptionalInfo},
    ok.

%%% ----------------------------------------------------------
%%% @doc Create the system created brm objects. Then scan the
%%% $RCS_ROOT/rcs/swm/backup dir for any backups
%%% @end

init_bu_info() ->
    case swmI:is_upgrade_ongoing() of
	true ->
	    [swmI:copy_old_table(Tab)||Tab<-[brM, 
%					     brmBackupManager,
					     brmSingleEvent,
					     brmPeriodicEvent,
					     brmBackupHousekeeping,
					     %% brmBackupScheduler,
					     brmCalendarBasedPeriodicEvent,
					     %% brmRollbackAtRestore,
					     brmBackupLabelStore,
					     %% brmFailsafeBackup,
					     swmFallbackList]],
	    update_house_keeping(),
	    case mnesia:dirty_first(brmRollbackAtRestore) of
	    	'$end_of_table' ->
	    	    %% Upgrading from a version where this MO is not present
	    	    %% Don't set a default timeout value
	    	    RAR = #brmRollbackAtRestore
	    		{brmRollbackAtRestoreId = {"1","1","1","1"}},
	    	    mnesia:dirty_write(RAR);
	    	[RAR] -> % Object already exists
		    Transform = 
			case swmLib:read(swmVariables, confirm_restore_db) of
			    [] -> true;
			    _ -> false
			end,
		    OldTime=RAR#brmRollbackAtRestore.timeAllowedBeforeRollback,
		    NewTime = 
			case OldTime of
			    3600 when Transform -> %% Old default, remove it
				undefined;
			    X ->  %% User configured value - keep it
				X
			end,
		    mnesia:dirty_write(RAR#brmRollbackAtRestore
				       {timeAllowedBeforeRollback = NewTime}),
	    	    ok
	    end,

	    [BrmBackupManager] = swmI:all_objects(brmBackupManager),
	    case BrmBackupManager of
		{brmBackupManager, MgrId, Type, Domain, MgrPR} ->
		    NewMgr = 
			#brmBackupManager{brmBackupManagerId = MgrId,
					  backupType = Type,
					  backupDomain = Domain ,
					  progressReport = MgrPR,
					  autoExport = ?BrmAutoExport_DISABLED,
					  autoExportUri = undefined,
					  autoExportPassword = undefined,
					  manualBackupName = undefined},
		    mnesia:dirty_write(NewMgr);
		_ when is_record(BrmBackupManager, brmBackupManager) ->
		    mnesia:dirty_write(BrmBackupManager)
	    end,
	    
	    [BrmBackupScheduler] = swmI:all_objects(brmBackupScheduler),
	    case BrmBackupScheduler of
		{brmBackupScheduler, Id, Max, Name, PR, Recent, Export, Uri, 
		 Passwd, Admin, Next,_} ->
		    NewScheduler = 
			#brmBackupScheduler{
			   brmBackupSchedulerId = Id,
			   maxStoredScheduledBackups = Max,
			   scheduledBackupName = Name,
			   progressReport = PR,
			   mostRecentlyCreatedAutoBackup = Recent,
			   autoExport = case Export of
					    ?BrmAutoExport_ENABLED ->
						Export;
					    _ ->
						?BrmAutoExport_DISABLED
					end,
			   autoExportUri = Uri,
			   autoExportPassword = Passwd,
			   adminState = Admin,
			   nextScheduledTime = Next},
		    mnesia:dirty_write(NewScheduler);
		_ when is_record(BrmBackupScheduler, brmBackupScheduler) ->
		    mnesia:dirty_write(BrmBackupScheduler)
	    end;
	false ->
	    do_init_bu_info()
    end.

do_init_bu_info() ->
    mnesia:dirty_write(#brM{brMId={"1","1","1"}}),

    mnesia:dirty_write(#brmRollbackAtRestore
		       {brmRollbackAtRestoreId = {"1","1","1","1"},
			timeAllowedBeforeRollback = undefined
		       }),
    mnesia:dirty_write(
      #brmBackupManager{brmBackupManagerId = {"1","1","1","1"},
			backupType = "Systemdata",
			backupDomain = "System",
			progressReport = undefined,
			autoExport = ?BrmAutoExport_DISABLED
		       }),


    Max = ?brmBackupScheduler_maxStoredScheduledBackups_default, 
    mnesia:dirty_write(
      #brmBackupScheduler{brmBackupSchedulerId={"1","1","1","1","1"},
			  maxStoredScheduledBackups = Max,
			  scheduledBackupName= "BACKUP",
			  %% HS46984
			  %% progressReport = 
			  %%     swmBackupScheduler:default_progress_report(0),
			  mostRecentlyCreatedAutoBackup ="",
			  adminState = ?BasicAdmState_UNLOCKED,
			  autoExport = ?BrmAutoExport_DISABLED}), % HS62346
    mnesia:dirty_write(
      #brmBackupHousekeeping{
	 brmBackupHousekeepingId={"1","1","1","1","1"},
	 maxStoredManualBackups = 20, %% new default requested by CA
	 autoDelete = ?BrmManualBackupAutoDelete_ENABLED}),

    mnesia:dirty_write(
      #brmBackupLabelStore{
	 brmBackupLabelStoreId={"1","1","1","1","1"}}).        

update_house_keeping() ->
    case mnesia:dirty_read(brmBackupHousekeeping, {"1","1","1","1","1"}) of
	[Obj] ->
	    case Obj#brmBackupHousekeeping.maxStoredManualBackups of
		100 -> 
		    %% Original default value; change
		    mnesia:dirty_write(Obj#brmBackupHousekeeping{
					 maxStoredManualBackups = 20});
		_Other ->
		    %% Operator has changed value or an upgrade
		    %% has already happen; no change
		    ok
	    end;
	Other ->
	    %% Should not happen
	    error_msg("brmBackupHousekeeping not found: ~p~n", [Other])
    end.


init_backup(RestoredBackupName, [BuDir|BuDirs]) ->
    Path = filename:join(BuDir, "metadata"),
    case file:consult(Path) of
	{ok, [BuData]} ->
	    Index = filename:basename(BuDir),
	    init_backup_metadata(RestoredBackupName, Index, 
				 convert_metadata(BuData));
	{ok, []} ->
	    Cmd_ls = "ls -la "++ BuDir,
	    Cmd_cat = "cat "++ Path,
	    Wrn =
		"Corrupt or empty file~n" ++
		"File: ~p~n" ++
		Cmd_ls ++ "~n~s~n" ++
		"File content:~n~s~n",
	    warning_msg(Wrn, [Path, os:cmd(Cmd_ls), os:cmd(Cmd_cat)]),
	    info_msg("Removing backup~n~p~n", [BuDir]),
	    cmd("rm -rf "++BuDir);
	{error, _} ->
	    Cmd_ls = "ls -la "++ BuDir,
	    Cmd_cat = "cat "++ Path,
	    Wrn =
		"Missing file~n" ++
		"File: ~p~n" ++
		Cmd_ls ++ "~n~s~n" ++
		"File content:~n~s~n",
	    warning_msg(Wrn, [Path, os:cmd(Cmd_ls), os:cmd(Cmd_cat)]),
	    info_msg("Removing backup~n~p~n", [BuDir]),
	    cmd("rm -rf "++BuDir)
    end,
    init_backup(RestoredBackupName, BuDirs);
init_backup(_, []) -> 
    {Manual, Scheduled, SysCr} =
	lists:foldl(fun count_backup/2, {0,0,0}, ets:tab2list(brmBackup)),
    Fmt = "No of backups: ~w Manual: ~w Scheduled: ~w System created: ~w",
    BuCountMsg = lists:flatten(io_lib:format(Fmt, [Manual+Scheduled+SysCr, 
						   Manual, Scheduled, SysCr])),
    swmLib:write_swm_log("BrmBackupManager", info, BuCountMsg),
    case swmFailsafe:get_usage_state() of 
	busy ->
	    Name = swmFailsafe:get_failsafe_backup(),
	    case mnesia:transaction(fun() -> get_backup_by_name(Name) end) of
		{atomic, [_]} ->
		    ok;
		{atomic, []} ->
		    erlang:error(failsafe_backup_missing);
		{aborted, Reason} ->
		    erlang:error({aborted, Reason})
	    end;
	_ ->
	    ok
    end.

count_backup(BrmBackup, {Manual, Scheduled, SystemCreated})
  when BrmBackup#brmBackup.creationType == ?BrmBackupCreationType_MANUAL ->
    {Manual+1, Scheduled, SystemCreated};
count_backup(BrmBackup, {Manual, Scheduled, SystemCreated})
  when BrmBackup#brmBackup.creationType == ?BrmBackupCreationType_SCHEDULED ->
    {Manual, Scheduled+1, SystemCreated};
count_backup(BrmBackup, {Manual, Scheduled, SystemCreated})
  when BrmBackup#brmBackup.creationType == 
       ?BrmBackupCreationType_SYSTEM_CREATED ->
    {Manual, Scheduled, SystemCreated+1}.

init_backup_metadata(RestoredBuName, Index, BuData) ->

    Key = {"1","1","1","1",Index},
    case mnesia:dirty_read({brmBackup, Key}) of
	[Bu] ->
	    #{backupName := BackupName,
	      creationTime := CreationTime} = BuData,
	    audit_backup_object(Bu, BackupName, CreationTime);
	[] ->
	    recover_backup_object(Key, RestoredBuName, BuData)
    end.

%%% Description: The db has been wiped clean. Try to recreate backup MOs from
%%%              metadata

%% The following case appear after a cluster restart
%% The backup is being used for restore in this startup.
%% Get the saved progress data from disk.
recover_backup_object(Key, undefined, BuData) ->
    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    Progress =  
	case read_progress_report(Index, erase) of
	    {error, _} ->
		%% error_msg("~s: No progress report: ~p~n",[DN, {error, R}]),
		default_idle_pr();
	    P when P#'AsyncActionProgress'.state == 
		   ?ActionStateType_CANCELLING -> 
		CompleteTime = comsaI:iso_time(os:timestamp(), extended),
		ProgressData = 
		    [{result, ?ActionResultType_FAILURE},
		     {resultInfo, "The action was cancelled by the user"},
		     {progressPercentage, 100},
		     {state, ?ActionStateType_CANCELLED},
		     {timeActionCompleted, CompleteTime}],
		swmLib:write_swm_log(DN, alert, "Restore cancelled"),
		comsaI:update_progress(ProgressData, P);
	    P ->
		CompleteTime = comsaI:iso_time(os:timestamp(), extended),
		ProgressData = 
		    [{result, ?ActionResultType_FAILURE},
		     {resultInfo, "The action was interrupted by a restart"},
		     {progressPercentage, 100},
		     {state, ?ActionStateType_FINISHED},
		     {timeActionCompleted, CompleteTime}],
		swmLib:write_swm_log(DN, alert, "Restore failed"),
		comsaI:update_progress(ProgressData, P)
	end,
    #{backupName := BackupName,
      creationTime := CreationTime,
      swVersion := SwVersion,
      creationType := CreationType} = BuData,
    mnesia:dirty_write(
      #brmBackup{brmBackupId = Key ,
		 backupName = BackupName,
		 creationTime = CreationTime,
		 status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
		 swVersion = SwVersion,
		 creationType = CreationType,
		 progressReport = Progress});

recover_backup_object(Key, RestoreNameB, BuData) ->
    #{backupName := BackupName,
      creationTime := CreationTime,
      swVersion := SwVersion,
      creationType := CreationType} = BuData,
    case binary_to_list(RestoreNameB) of
	RestoreName when BackupName == RestoreName ->
	    %% We are restoring this backup currently
	    %% Fetch saved restore progress
	    %CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    Index = element(5, Key),
	    Progress = case read_progress_report(Index, save) of
			   {error, _} ->
			       default_progress_report("RESTORE",0);
			   P -> P
		       end,
	    confirm_restore_at_startup(RestoreName, Key, BuData, Progress);
	_ ->
	    mnesia:dirty_write(
	      #brmBackup{brmBackupId = Key ,
			 backupName = BackupName,
			 creationTime = CreationTime,
			 status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
			 swVersion = SwVersion,
			 creationType = CreationType,
			 progressReport = default_idle_pr()})
    end.

%%% HANDLE CONFIRM RESTORE
%%% If there is a rollback restore file, then there has been a manual restore
%%% which needs to be confirmed, or the time for restoring has already lapsed.
%%% Update progress information accordingly.
%%% The timer will have been set during the process init phase

confirm_restore_at_startup(_Name, Key, BuData, Progress) ->
    %% DoConfirmRestore = 
    %% 	case swmLib:get_variable(confirm_restore) of
    %% 	    true -> true;
    %% 	    _ -> false
    %% 	end,
    RestoreFile = rollback_restore_file(sysEnv:home_dir()),
    case file:consult(RestoreFile) of
	{error, Reason} ->
	    info_msg("No confirm file. Immediate autoconfirm.~n"),
	    %% If file error, confirm anyway without waiting
	    confirm_restore(Key, BuData, Progress),
	    case Reason of
		enoent -> ok;
		_ -> 
		    error_msg("Error reading ~p = ~p~n",
			      [RestoreFile, 
			       {error, Reason}])
	    end;
	%% {ok, Terms} when DoConfirmRestore ->
	{ok, Terms} ->
	    info_msg("Confirm file found. (recover)~n"),
	    #{backupName := BackupName,
	      creationTime := CreationTime,
	      swVersion := SwVersion,
	      creationType := CreationType} = BuData,
	    Expiry = proplists:get_value(restore_expires, Terms),
	    Timeout = calendar:datetime_to_gregorian_seconds(Expiry)-
		calendar:datetime_to_gregorian_seconds(
		  calendar:universal_time()),
	    [RAR] = ets:tab2list(brmRollbackAtRestore),
	    case Timeout of
		Timeout when Timeout >= 0 ->
		    %% There is time left
		    info_msg("Confirm pending: ~p seconds remaining "
			     "(recover)~n",
			     [Timeout]),
		    NewRAR = RAR#brmRollbackAtRestore{
			       timeRemainingBeforeRollback = Timeout},
		    mnesia:dirty_write(NewRAR),
		    AI = [{additionalInfo, "Waiting for confirmRestore"},
			  {progressPercentage, 90}],
		    NewProgress = comsaI:update_progress(AI, Progress),
		    mnesia:dirty_write(
		      #brmBackup{
			 brmBackupId = Key,
			 backupName = BackupName,
			 creationTime = CreationTime,
			 status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
			 swVersion = SwVersion,
			 creationType = CreationType,
			 progressReport = NewProgress});
		_ ->
		    %% The expiry time has passed. Do nothing
		    ok
	    end
    end.


%%% Description: The backup server has restarted for some reason, but the db
%%%              has survived, only make update
%% The following cases appears after a local restart or a single MP
%% restart in cluster mode, but not after cluster restart
audit_backup_object(Bu, Name, Time) when 
      Bu#brmBackup.backupName == Name,
      Bu#brmBackup.creationTime == Time,
      Bu#brmBackup.progressReport /= undefined ->
    Progress = Bu#brmBackup.progressReport,
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    ProgressData = 
	case Progress#'AsyncActionProgress'.state of
	    ?'ActionStateType_CANCELLING' ->
		[{result, ?ActionResultType_FAILURE},
		 {resultInfo, "The backup service was restarted"},
		 {progressPercentage, 100},
		 {state, ?ActionStateType_CANCELLED},
		 {timeActionCompleted, CompleteTime}
		];
	    ?'ActionStateType_RUNNING' ->
		[{result, ?ActionResultType_FAILURE},
		 {resultInfo, "The backup service was restarted"},
		 {progressPercentage, 100},
		 {state, ?ActionStateType_FINISHED},
		 {timeActionCompleted, CompleteTime}
		];
	    _ ->
		[]
	end,
    NewProgress = comsaI:update_progress(ProgressData, Progress),
    mnesia:dirty_write(Bu#brmBackup{progressReport=NewProgress});
%%% No progress data to care about
audit_backup_object(Bu, Name, Time) when 
      Bu#brmBackup.backupName == Name,
      Bu#brmBackup.creationTime == Time ->
    ok.

set_mgr_progress(BuRestore) ->
    case mnesia:transaction(fun() -> do_set_mgr_progress(BuRestore) end) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    %% Fail to set progress should not crash the system start
	    error_msg("Could not set initial progress because of ~p~n",
		      [{aborted, Reason}])
    end.


do_set_mgr_progress(undefined) ->
    [Mgr] = mnesia:read({brmBackupManager, {"1","1","1","1"}}),
    case Mgr#brmBackupManager.progressReport of
	undefined ->
	    NewProgress = default_idle_pr(),
	    mnesia:write(Mgr#brmBackupManager{progressReport=NewProgress});
	Progress ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    ProgressData = 
		case Progress#'AsyncActionProgress'.state of
		    ?'ActionStateType_CANCELLING' ->
			[{result, ?ActionResultType_FAILURE},
			 {resultInfo, "The backup service was restarted"},
			 {progressPercentage, 100},
			 {state, ?ActionStateType_CANCELLED},
			 {timeActionCompleted, CompleteTime}
			];
		    ?'ActionStateType_RUNNING' ->
			[{result, ?ActionResultType_FAILURE},
			 {resultInfo, "The backup service was restarted"},
			 {progressPercentage, 100},
			 {state, ?ActionStateType_FINISHED},
			 {timeActionCompleted, CompleteTime}
			];
		    _ ->
			[]
		end,
	    NewProgress = comsaI:update_progress(ProgressData, Progress),
	    mnesia:write(Mgr#brmBackupManager{progressReport=NewProgress})
    end;
do_set_mgr_progress(_) ->
    %% HT56916
    %% A restore is ongoing, mgr progress will show states from the create
    %% action, which is not correct here. Reset it entirely
    [Mgr] = mnesia:read({brmBackupManager, {"1","1","1","1"}}),
    NewProgress = default_idle_pr(),
    mnesia:write(Mgr#brmBackupManager{progressReport=NewProgress}).





%%% ----------------------------------------------------------
%%% @doc Outside restore request
%%% Restore request are always run by the swmBackup server to prevent
%%% to restore operations at the same time
%%% @end
%%% ----------------------------------------------------------

-spec restore_backup(Name::string()) -> ok | {error, term()}.

restore_backup(Name) ->
    %% While this code can handle multiple instance with the same 
    %% backup name, according to ECIM that should not happen
    Fun = fun() ->
		  case get_backup_by_name(Name) of
		      [Match] ->
			  Match#brmBackup.brmBackupId;
		      [] ->
			  mnesia:abort({backup_not_found, Name})
		  end
	  end,
    Id = swmLib:get_new_action_id(mgr),

    case mnesia:transaction(Fun) of
	{atomic, Key} ->
	    swmLib:mo_lock_action_capable(?RESTORE_BACKUP_ACTION_CAPABLE_ID,
					  ?RESTORE_BACKUP_ACTION_CAPABLE_INFO),
	    gen_server:cast(swmBackup, {autorestore, Key, Id}),
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%% ----------------------------------------------------------
%%% @doc Internal request to install a fallback UP
%%% @end
%%% ----------------------------------------------------------

-spec install_bootfallback() -> ok | {error, term()}.

install_bootfallback() ->
    BFbUpName = swmFallbackList:get_bootfallback_up(),
    info_msg("BootFallback UP candidate:~n~p~n", [BFbUpName]),
    case sysEnv:rcs_mode_2() of
	target ->
	    install_bootfallback(BFbUpName);
	_ ->
	    ok
    end.

install_bootfallback(undefined) ->
    ok;
install_bootfallback(Name) ->
    install_bootfallback(swmFallbackList:is_bootfallback_enabled(), Name).

install_bootfallback(true, Name) ->
    Fun = fun() ->
  		  case get_backup_by_name(Name) of
  		      [Match] ->
  			  Match#brmBackup.brmBackupId;
  		      [] ->
  			  mnesia:abort({backup_not_found, Name})
  		  end
  	  end,
    case mnesia:transaction(Fun) of
  	{atomic, Key} ->
  	    swmLib:mo_lock_action_capable(
  	      ?INSTALL_BOOTFALLBACK_ACTION_CAPABLE_ID,
  	      ?INSTALL_BOOTFALLBACK_ACTION_CAPABLE_INFO),
  	    gen_server:cast(swmBackup, {install_bootfallback, Key}),
  	    ok;
  	{aborted, Reason} ->
  	    {error, Reason}
    end;
install_bootfallback(IsEnabled, _) ->
    info_msg("BootFallback ~p~n", [IsEnabled]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Outside request to delete a system created backup
%%% @end
%%% ----------------------------------------------------------
delete_system_created_backup(Name, Progress) ->
    %% While this code can handle multiple instance with the same 
    %% backup name, according to ECIM that should not happen
    Fun = fun() ->
		  [begin 
		       FullIndex = tuple_to_list(Bu#brmBackup.brmBackupId),
		       mnesia:delete_object(Bu),
		       lists:last(FullIndex)
		   end||Bu<-get_backup_by_name(Name)]
	  end,

    {atomic, Indices} =  mnesia:transaction(Fun),
    [remove_backup_dir(Index)||Index<-Indices],
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(Progress, [{result, ?ActionResultType_SUCCESS},
			       {resultInfo, "Backup removed"},
			       {progressPercentage, 100},
			       {state, ?ActionStateType_FINISHED},
			       {timeActionCompleted, CompleteTime}]),
    ok.



%%% ----------------------------------------------------------
%%% @doc Initiate internal housekeeping of system created backups
%%% @end
%%% ----------------------------------------------------------

internal_housekeeping() ->
    Time = 
	case swmLib:get_variable(swm_test_housekeeping_delay) of
	    Int when is_integer(Int) ->
		Fmt = "Delayed internal housekeeping changed to ~w "
		    "seconds~n",
		Msg = lists:flatten(io_lib:format(Fmt, [Int div 1000])),
		info_msg(Msg),	      
		DN = "BrmBackupManager",
		swmLib:write_swm_log(DN, warning, Msg),
		Int;
	    _ ->
		info_msg(
		  "Delayed internal housekeeping will be commenced in "
		  "one hour~n"),
		3600000
	end,

    timer:apply_after(
      Time, gen_server, cast, [swmBackup, internal_housekeeping]).

failsafe_lock() ->
    gen_server:call(swmBackup, failsafe_lock, infinity).

failsafe_release()->
    gen_server:cast(swmBackup, failsafe_release).


%% metadata_props(#metadataV2{backupName = BuName,
%% 			   creationTime = CreationTime,
%% 			   swVersion = ProductData}) ->
%%     [{backupName, BuName},
%%      {creationTime, CreationTime}
%%      | productData_props(ProductData)].
%% metadata_props(Metadata) ->
%%     ?LOG_WARN([{unknown, Metadata}]),
%%     [].

%% productData_props(#'ProductData'{productName = ProdName,
%% 				 productNumber = ProdNo,
%% 				 productRevision = ProdRev,
%% 				 productionDate = ProdDate,
%% 				 description = Descr,
%% 				 type = Type}) ->
%%     [{productName, ProdName},
%%      {productNumber, ProdNo},
%%      {productRevision, ProdRev},
%%      {productionDate, ProdDate},
%%      {description, Descr},
%%      {type, Type}];
%% productData_props([ProductData]) ->
%%     productData_props(ProductData);
%% productData_props(ProductData) ->
%%     [{productData, ProductData}].

%%% ----------------------------------------------------------
print_inventory(Path) ->
    {ok, Fd} = file:open(Path, [write]),
    % BU = ets:tab2list(brmBackup),
    io:format(Fd, "~78c~n",[$=]),
    io:format(Fd, "BACKUP INVENTORY~n",[]),
    io:format(Fd, "~-4s ~-25s ~-4s ~-30s\t~s~n",
	      ["Id", "Creation Time", "Type", "BackupName", "Upgrade package"]),
    io:format(Fd, "~78c~n",[$=]),
    Fun = fun(BrmBackup, Accu) ->
		  BackupId = integer_to_list(BrmBackup#brmBackup.brmBackupId),
		  BackupName = BrmBackup#brmBackup.backupName,
		  CreationDate = BrmBackup#brmBackup.creationTime,
		  Type = case BrmBackup#brmBackup.creationType of
			     ?BrmBackupCreationType_MANUAL -> "MAN";
			     ?BrmBackupCreationType_SCHEDULED -> "SCED";
			     ?BrmBackupCreationType_SYSTEM_CREATED -> "SYS"
			 end,
		  [#'ProductData'{productName=UpName,
				  productNumber=UpProdNr,
				  productRevision=UpVsn}] = 
		      BrmBackup#brmBackup.swVersion,
		  UpgradePackage = UpName++" "++UpProdNr++" "++UpVsn,
		  io:format(Fd, "~-4s ~-25s ~-4s ~s\t~s~n",
			    [BackupId, CreationDate, Type, BackupName,  
			     UpgradePackage]),
		  case Accu rem 5 of
		      0 -> io:format(Fd, "~n",[]);
		      _ -> ok
		  end,
		  Accu+1
	  end,
    lists:foldl(Fun, 1, lists:keysort(2, [BrmBackup#brmBackup{brmBackupId = list_to_integer(element(5, BrmBackup#brmBackup.brmBackupId))}||BrmBackup<-ets:tab2list(brmBackup)])),
    io:format(Fd, "~78c~nLock in the mnesia brmBackup table for detailed info",
	      [$=]),
    file:close(Fd).    


make_ai_backup() ->
    ManagerKey = {"1","1","1","1"},
    BuName = list_to_binary(aicI:get_backup_name()),
    try create_backup_common(BuName, ManagerKey, system, undefined) of
        MoRef ->
            Index = lists:last(string:tokens(MoRef, "=")),
            swmLib:unlock_backup(Index),
	    swmFallbackList:add_backup(latest, aicI:get_backup_name())
    catch Type:Error ->
	    {error, {Type, Error}}
    end.


clear_ai_backup() ->
    BuName = aicI:get_backup_name(),
    delete_system_created_backup(BuName, undefined),
    %% HV55838: Audit fallback list after removal of AI backup to cause an
    %% escalation default backup to be made if necessary
    swmFallbackList:audit_fallback_list(),
    ok.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-record(state, {restore=idle,     %% idle | busy
		failsafe=free,    %% free | lock | pending
		confirm=undefined %% timer_ref()
		}).

%%% ----------------------------------------------------------
%%% @doc gen_server callback 
%%% If not created, the $RCS_ROOT/rcs/swm/backup directory is created

init(Args) ->
    %% dbg:tracer(),
    %% dbg:p(self(), c),
    %% dbg:tpl(?MODULE, x),
    gen_server:cast(self(), {initialize, Args}),
    {ok, initializing}.

initialize(_Args) ->
    swmLib:init_action_id(mgr),
    filelib:ensure_dir(swmLib:backup_dir("1")),
    BackupDir = swmLib:backup_dir(),
    BackupDirs = filelib:wildcard(filename:join(BackupDir, "*")),
    mnesia:subscribe({table, swmFallbackList, detailed}),
    BuRestore = swmLib:get_variable(bu_restore),
    case BuRestore of
	undefined ->
	    ok;
	_ ->
	    info_msg("Restore of backup detected: ~p~n",[BuRestore])
    end,
    set_mgr_progress(BuRestore),
    init_backup(BuRestore, BackupDirs),
    State =
	case initiate_confirm_timeout() of
	    no_ongoing_confirm ->
		#state{};
	    {ongoing_confirm, TimerRef} ->
		#state{confirm = TimerRef}
	end,
    swmvBuRestore:restore_initialized(#{bu_restore =>
					sysUtil:term_to_string(BuRestore)}),
    {noreply, State}.

%%% ----------------------------------------------------------
backup_object([#brmBackup{} = Obj]) ->
    Obj;
backup_object([]) ->
    undefined;
backup_object(BuIndexOrName) when is_list(BuIndexOrName) ->
    try list_to_integer(BuIndexOrName) of
	_ ->
	    BuIndex = BuIndexOrName,
	    case backup_object({"1", "1", "1", "1", BuIndex}) of
		#brmBackup{} = Obj ->
		    Obj;
		_ ->
		    backup_object_file(BuIndex)
	    end
    catch
	error : badarg ->
	    BackupName = BuIndexOrName,
	    WP = mnesia:table_info(brmBackup, wild_pattern),
	    Pattern = WP#brmBackup{backupName = BackupName},
	    backup_object(mnesia:dirty_match_object(Pattern))
    end;
backup_object({"1", "1", "1", "1", BuIndex} = Key) when is_list(BuIndex) ->
    backup_object(mnesia:dirty_read(brmBackup, Key));
backup_object(Unrec) ->
    ?LOG_WARN([{"Unrecognized format", Unrec}]),
    undefined.

%%% ----------------------------------------------------------
backup_object_file(BuIndex) ->
    BackupDirs = filelib:wildcard(filename:join(swmLib:backup_dir(), "*")),
    backup_object_file(BackupDirs, BuIndex).

backup_object_file([BuDir | Tail], BuIndex) ->
    case filename:basename(BuDir) of
	BuIndex ->
	    Path = filename:join(BuDir, "metadata"),
	    case file:consult(Path) of
		{ok, [BuData]} ->
		    backup_object_metadata(BuIndex, convert_metadata(BuData));
		_ ->
		    backup_object_file(Tail, BuIndex)
	    end;
	_ ->
	    backup_object_file(Tail, BuIndex)
    end;
backup_object_file([], _) ->
    undefined.

%%% ----------------------------------------------------------
backup_object_metadata(BuIndex, #{backupName   := BackupName,
				  creationTime := CreationTime,
				  swVersion    := SwVersion,
				  creationType := CreationType}) ->
    case read_progress_report(BuIndex, save) of
	{error, _} ->
	    Progress = default_progress_report("RESTORE", 0);
	Progress ->
	    ok
    end,
    #brmBackup{brmBackupId = {"1", "1", "1", "1", BuIndex} ,
	       backupName = BackupName,
	       creationTime = CreationTime,
	       status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
	       swVersion = SwVersion,
	       creationType = CreationType,
	       progressReport = Progress}.

%%% ----------------------------------------------------------
%%% @doc gen_server callback 

handle_call(_, _, State) when State#state.restore == busy ->
    {reply, failure, State};

handle_call({deleteBackup, Name, Id, Tcall}, _From, State) ->
    T0 = ?MonoTime,
    action_handler(fun() -> handle_delete_backup(Name) end, mgr),
    [Obj] = mnesia:dirty_read({brmBackupManager, {"1","1","1","1"}}),
    Result = record2props(Obj#brmBackupManager.progressReport),
    ok = swmLib:unlock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID),
    Tend = ?MonoTime,
    LogInfo =
	[{"Delay => server start", sysUtil:time_to_string(T0 - Tcall)},
	 {"Server execution", sysUtil:time_to_string(Tend - T0)}],
    {reply, {{ok, Result}, LogInfo}, State};
handle_call({createBackup, Name, DnRev, Id, Tcall}, _From, State) ->
    T0 = ?MonoTime,
    action_handler(fun() -> handle_create_backup(Name, DnRev) end, mgr),
    [Obj] = mnesia:dirty_read({brmBackupManager, {"1","1","1","1"}}),
    Result = record2props(Obj#brmBackupManager.progressReport),
    ok = swmLib:unlock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID),
    Tend = ?MonoTime,
    LogInfo =
	[{"Delay => server start", sysUtil:time_to_string(T0 - Tcall)},
	 {"Server execution", sysUtil:time_to_string(Tend - T0)}],
    {reply, {{ok, Result}, LogInfo}, State};

handle_call({export, BuKey = ReportKey, Passwd, Uri, Id, Tcall}, _, State) ->
    T0 = ?MonoTime,
    action_handler(fun() ->
			   handle_export_backup(BuKey, ReportKey, Passwd, Uri)
		   end,
		   ReportKey),
    [Obj] = mnesia:dirty_read({brmBackup, BuKey}),
    Result = record2props(Obj#brmBackup.progressReport),
    ok = swmLib:unlock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID),
    Tend = ?MonoTime,
    LogInfo =
	[{"Delay => server start", sysUtil:time_to_string(T0 - Tcall)},
	 {"Server execution", sysUtil:time_to_string(Tend - T0)}],
    {reply, {{ok, Result}, LogInfo}, State};

handle_call(failsafe_lock, _, State) ->
    case State#state.failsafe of
	lock -> {reply, locked, State};
	free -> {reply, locked, State#state{failsafe = lock}}
    end;

handle_call(Request, _From, State) ->
    error_msg("Unknown call: ~p~n",[Request]),
    {reply, ok, State}.

%%% @doc gen_server callback 
%%% @end

handle_cast({initialize, Args}, initializing) ->
    initialize(Args);
handle_cast(Request, State) when State#state.restore == busy ->
    info_msg("Request ~p supressed while restore is onoing~n",[Request]),
    {noreply, State};

%%% Actions on BrmBackupManager
handle_cast({createBackup, Name, DnRev, Id}, State) ->
    action_handler(fun() -> handle_create_backup(Name, DnRev) end, mgr),
    ok = swmLib:unlock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID),
    {noreply, State};

%% handle_cast({deleteBackup, Name, Id}, State) ->
%%     action_handler(fun() -> handle_delete_backup(Name) end, mgr),
%%     ok = swmLib:unlock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID),
%%     {noreply, State};

%% handle_cast({importBackup, DnRev, Uri, Passwd, Id}, State) ->
%%     action_handler(fun() -> handle_import_backup(DnRev, Uri, Passwd) end,
%% 		   mgr),
%%     ok = swmLib:unlock_action_capable(?IMPORT_BACKUP_ACTION_CAPABLE_ID),
%%     {noreply, State};

%% %%% Actions on BrmBackup
%% handle_cast({export, BuKey, Passwd, Uri, Id}, State) ->
%%     action_handler(fun() -> handle_export_backup(BuKey, BuKey, Passwd, Uri) end,
%% 		   BuKey),
%%     ok = swmLib:unlock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID),
%%     {noreply, State};

handle_cast({install_bootfallback, BuKey}, State) ->
    case State#state.confirm of
	undefined ->
	    T0 = ?MonoTime,
	    Res = handle_install_bootfallback(BuKey),
	    ok = swmLib:unlock_action_capable(?INSTALL_BOOTFALLBACK_ACTION_CAPABLE_ID),
	    case Res of
		ok ->
		    Tend = ?MonoTime,
		    ?LOG_INFO([{"BootFallback TOTAL",
				sysUtil:time_to_string(Tend - T0)}]);
		retry ->
		    timer:sleep(1000),
		    install_bootfallback()
	    end;
	_ ->
	    % No install bootfallback when confirm is ongoing
	    ok
    end,
    {noreply, State};
handle_cast({restore, BuKey, Id}, State) ->
    action_handler(fun() -> handle_restore_backup(BuKey) end, BuKey),
    receive
	stop_receiving_requests ->
	    ok = swmLib:unlock_action_capable(?RESTORE_BACKUP_ACTION_CAPABLE_ID),
	    {noreply, State#state{restore=busy}}
    after 0 ->
	    ok = swmLib:unlock_action_capable(?RESTORE_BACKUP_ACTION_CAPABLE_ID),
	    {noreply, State}
    end;
handle_cast({autorestore, BuKey, Id}, State) ->
    set_default_progress_report(BuKey, "RESTORE", Id),
    action_handler(fun() -> handle_restore_backup(BuKey, auto) end, BuKey),
    receive
	stop_receiving_requests ->
	    ok = swmLib:unlock_action_capable(
		   ?RESTORE_BACKUP_ACTION_CAPABLE_ID),
	    {noreply, State#state{restore=busy}}
    after 0 ->
	    ok = swmLib:unlock_action_capable(
		   ?RESTORE_BACKUP_ACTION_CAPABLE_ID),
	    {noreply, State}
    end;

handle_cast(restore_ai_bu, State) ->
    AiBuFile = swmLib:ai_bu_file(),
    case filelib:is_file(AiBuFile) of
	true ->
	    %% BU from AutoIntegration; restore from this BU
	    info_msg("Backup from AutoIntegration; restoring ~n"),
	    swmLib:write_swm_log(
	      "BrmBackupManager", info, 
	      "Restore backup supplied from autointegration"),
	    try handle_restore_ai_bu(AiBuFile)
	    catch
		Type:Error ->
		    sysInitI:error_report([{?MODULE, handle_store_ai_bu},
					   {Type, Error},
					   erlang:get_stacktrace()]),
		    swmLib:write_swm_log(
		      "BrmBackupManager", info,
		      "Restore AutoIntegration backup failed")
	    end;
	_ ->
	    ok
    end,
    {noreply, State};

handle_cast(internal_housekeeping, State) ->
    case State#state.failsafe of
	lock -> 
	    {noreply, State};
	_ ->
	    do_internal_housekeeping(),
	    {noreply, State}
    end;

handle_cast(failsafe_release, State) ->
    do_internal_housekeeping(),
    {noreply, State#state{failsafe = free}};

handle_cast(confirmRestore, State) ->
    DN = "BrmRollbackAtRestore",
    swmLib:write_swm_log(DN, info, "confirmRestore received"),
    RestoreFile = rollback_restore_file(sysEnv:home_dir()),
    case file:consult(RestoreFile) of
	{ok, Terms} ->
	    Key = proplists:get_value(ongoing_restore, Terms),
	    confirm_restore(Key),    
	    cmd(["rm -rf ", RestoreFile]),
	    timer:cancel(State#state.confirm),
	    {noreply, State#state{confirm = undefined}};
	{error, enoent} ->
	    %% No ongoing confirm. Ignore!
	    {noreply, State};
	{error, Reason} ->
	    swmLib:write_swm_log(
	      DN, alert, "confirmRestore failed on a software error"),
	    error_msg("Failed to read ~p = {error, ~p}~n",[RestoreFile,Reason]),
	    {noreply, State}
    end;

%% handle_cast(create_final_backup, State) ->
%%     handle_create_final_backup(),
%%     {noreply, State};

handle_cast(Request, State) ->
    error_msg("Unknown cast: ~p~n",[Request]),
    {noreply, State}.

%%% @doc gen_server callback 

handle_info({mnesia_table_event, Event}, State) ->
    handle_mnesia_table_event(Event),
    {noreply, State};
handle_info(confirm_timeout, State) ->
    info_msg("Confirm timeout~n",[]),
    %% Check for login. If no login initiate rollback
    DN = "BrmRollbackAtRestore",
    swmLib:write_swm_log(DN, info, "Confirm timeout"),
    try omc_api:is_activity_since_restart() of
    	true ->
    	    swmLib:write_swm_log(DN, info, "Backup autoconfirm initiated"),
    	    RestoreFile = rollback_restore_file(sysEnv:home_dir()),
    	    case file:consult(RestoreFile) of
    		{ok, Terms} ->
    		    Key = proplists:get_value(ongoing_restore, Terms),
		    update_progress(
		      Key, [{additionalInfo, "Confirm timeout. OAM activity detected. Confirm is automatic."}]),
    		    confirm_restore(Key),    
    		    cmd(["rm -rf ", RestoreFile]),
    		    timer:cancel(State#state.confirm),
    		    {noreply, State#state{confirm = undefined}};
    		{error, Reason} ->
    		    swmLib:write_swm_log(
    		      DN, alert, 
    		      "Confirm timeout handling failed on a software error"),
    		    error_msg(
    		      "Failed to read ~p = {error, ~p}~n",[RestoreFile,Reason]),
    		    {noreply, State}
    	    end;
    	false ->
	    Reason = "No contact with managent system. Rolling back",
	    swmLib:write_swm_log(DN, alert, Reason),
	    %% For now we assume the other area is untouched.
	    %% FIXME: Make sure EE is always reainstalled again!!!
	    rollback_at_restore(Reason),
	    {noreply, State}
    catch
	exit:{noproc, _} ->
	    Reason = "No contact with managent system. Rolling back",
    	    swmLib:write_swm_log(DN, alert, Reason),
    	    %% For now we assume the other area is untouched.
    	    rollback_at_restore(Reason),
	    {noreply, State}
    end;    
handle_info({cancel, Key}, #state{restore = busy} = State) ->
    %% No need to do cancel since restore is ongoing
    %% TR HV63703
    info_msg("Cancel when restore is already ongoing Key: ~p~n",[Key]),
    {noreply, State};

handle_info({cancel, Key}, State) ->
    %% This means the operator has cancelled the rollback when in the 
    %% waiting period

    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    [BrmBackup] = mnesia:dirty_read({brmBackup, Key}),
    Progress = BrmBackup#brmBackup.progressReport,
    
    case Progress of
	undefined -> ok; 
	Progress when Progress#'AsyncActionProgress'.state == 
		      ?ActionStateType_CANCELLING ; 
		      %% Legacy case. State Cancelling should now already be
		      %% set in do_action
		      Progress#'AsyncActionProgress'.state == 
		      ?ActionStateType_RUNNING  
		      ->
	
	    Action = Progress#'AsyncActionProgress'.actionName,
	    Info = "Cancelling action " ++ Action,
	    swmLib:write_swm_log(DN, info, Info),
	    update_progress(Key, [{state, ?ActionStateType_CANCELLING}]),
	    case Action of
		"RESTORE" ->
		    save_progress_report(Key),
		    rollback_at_restore("Cancel ordered");
		"EXPORT" -> 
		    %% This should really never occur. All cancels handled
		    %% in handle_export_backup
		    ok
	    end;
	_ ->
	    %% The job is already cancelling or in a stable state
	    ok
    end,
    {noreply, State};
	
handle_info({aborted, Key, OptInfo}, State) ->
    Index = element(5, Key),
    DN = "BrmBackup=" ++ Index,
    case mnesia:dirty_read({brmBackup, Key}) of
	[#brmBackup{progressReport = undefined}] ->
	    ok;
	[#brmBackup{progressReport =
		    #'AsyncActionProgress'{actionName = Action}}] ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    ResultInfo =
		maps:get(resultInfo, OptInfo, "The action was aborted"),
	    ProgressData =
		[{result, ?ActionResultType_FAILURE},
		 {resultInfo, ResultInfo},
		 {progressPercentage, 100},
		 {state, ?ActionStateType_FINISHED},
		 {timeActionCompleted, CompleteTime}],
	    update_progress(Key, ProgressData),
	    AbortedInfo = Action ++ ": " ++ ResultInfo,
	    ?LOG_INFO([{aborted, AbortedInfo}]),
	    swmLib:write_swm_log(DN, info, AbortedInfo)
    end,
    {noreply, State#state{restore = idle}};

handle_info(_Request, State) ->
    {noreply, State}.

%%% @doc gen_server callback 

code_change(OldVsn, State, Extra) ->
    info_msg("code_change(~p, ~p, ~p)~n",[OldVsn, State, Extra]),
    {ok, State}.

%%% @doc gen_server callback 

terminate(_Reason, _State) ->
    ok.

%% #############################################################################
%% rollback_at_restore
%%
%% ###=======================================================================###
rollback_at_restore(Reason) ->
    case swmvBuRestore:is_ongoing() of
	false ->
	    %% G2
	    appmI:inhibit_escalation(),
	    swmOs:rollback_at_restore(),
	    swmLib:order_restart_node(cold, ?ALH_TAG_DataRestore);
	true ->
	    %% 5G, R-VNFM (LCM) takes down the 'to-Node'.
	    swmvBuRestore:cancel(Reason)
    end,
    self() ! stop_receiving_requests.


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
    swmLib:write_swm_log("BrmBackupManager", info, "Creating backup "++Name),

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
	    BuKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++[Index]),
	    NextId = swmLib:get_new_action_id(BuKey),
	    set_default_progress_report(BuKey, "MANUAL_AUTOEXPORT", NextId),
	    action_handler(
	      fun() -> 
		      handle_export_backup(BuKey, BuKey, Password, Uri)
	      end, BuKey)
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
	    %% Cleanup after failed create backup
	    %% A lock is in place here already, avoid setting another one
	    error_msg("{~p, ~p}~n~p~n",[T,E,erlang:get_stacktrace()]),
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
		  case Progress of
		      undefined -> ok;
		      _ -> 
			  [LS] = mnesia:read({brmBackupLabelStore, LsKey}),
			  mnesia:write(
			    LS#brmBackupLabelStore{
			      lastCreatedBackup=binary_to_list(Name)}),
			  LS
		  end
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
		       scheduled -> ?BrmBackupCreationType_SCHEDULED;
		       system -> ?BrmBackupCreationType_SYSTEM_CREATED
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
	    ?BrmBackupCreationType_SCHEDULED -> "SCHEDULED";
	    ?BrmBackupCreationType_SYSTEM_CREATED -> "SYSTEM_CREATED"						    
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
    %%    info_msg("NMS Backup info: ~n~p~n",[SimpleXML]),
    Result = xmerl:export_simple(SimpleXML, xmerl_xml, []),
    {ok, Fd} = file:open(Path, [write]),
    io:format(Fd, Result, []),
    file:close(Fd).





%% delete_all_backups() ->
%%     [delete_backup_internal(BrmBackup#brmBackup.backupName, forced)||
%% 	BrmBackup<-ets:tab2list(brmBackup)],
%%     ok.



%%% ----------------------------------------------------------
%%% @doc Restore an AutoIntegration backup

handle_restore_ai_bu(AiBuFile) ->

    %% Run import routine without progress updates
    Index = get_next_index(),
    DnRev = [<<"1">>,<<"BrmBackupManager">>,<<"1">>,<<"BrM">>,<<"1">>,
	     <<"SystemFunction">>, <<"1">>, <<"ManagedElement">>],
    Fun = fun() -> do_handle_import_backup(DnRev, AiBuFile, Index) end,
    case mnesia:transaction(Fun) of
	{atomic, {_, Metadata}} -> 
	    store_metadata(Index, Metadata),
	    swmLib:unlock_backup(Index),
	    #{backupName := Name} = Metadata,
	    swmLib:write_swm_log("BrmBackup="++Index, info, 
				 "Backup imported: "++Name),
	    ok;
	{aborted, {duplicate_name, Name}} ->
	    do_remove_backup_dir(Index),
	    swmLib:unlock_backup(Index),
	    swmLib:write_swm_log("BrmBackupManager", error, 
				 "Duplicate name: "++Name),
	    throw({duplicate_name, Name});
	{aborted, {inconsistent_software, SwVersions}} ->
	    [begin
		 Msg = "Required software: "++format_product_data(Sw),
		 swmLib:write_swm_log("BrmBackupManager", info, Msg)
	     end||Sw<-SwVersions],
	    do_remove_backup_dir(Index),
	    swmLib:unlock_backup(Index),
	    throw({inconsistent_software, SwVersions});
	{aborted, MnesiaReason} ->
	    do_remove_backup_dir(Index),
	    swmLib:unlock_backup(Index),
	    erlang:error(MnesiaReason, [AiBuFile])
    end,

    %% Run restore routine 
    BuKey = {"1","1","1","1",Index},
    Id = swmLib:get_new_action_id(BuKey),
    set_default_progress_report(BuKey, "RESTORE", Id),
    self()!force,
    ok = handle_restore_backup(BuKey, auto),
    file:delete(AiBuFile).


%%% ----------------------------------------------------------
%%% @doc Restore a backup

handle_install_bootfallback(Key) ->
    swmLib:internal_lock([?LOCKOBJ_audit_sw, ?LOCKOBJ_upgrade],
			 install_bootfallback),
    erlang:yield(),
    try
	handle_install_bootfallback(swmLib:is_internal_lock(
				      ?LOCKOBJ_inst_bootfb),
				    Key)
    catch
	ErrClass : ErrReason ->
	    Stacktrace = erlang:get_stacktrace(),
	    swmLib:internal_lock_remove([?LOCKOBJ_audit_sw, ?LOCKOBJ_upgrade],
					install_bootfallback),
	    ?LOG_ERR(["Install Bootfallback failed",
		      {ErrClass, ErrReason} | Stacktrace]),
	    ok
    end.

handle_install_bootfallback(false, Key) ->
    Index = element(5, Key),
    Metadata = get_metadata(Index),
    #{swVersion := SwVersion,
      backupName := BuName} = Metadata,
    HsiEnabled = case swmLib:map_get_optional(hsiEnabled, Metadata) of
		     {ok, V} -> V;
		     badkey -> undefined
		 end,
    [UP] = swmModel:get_matching_up(SwVersion),
    info_msg("Installing BootFallback...~nIndex = ~s: ~p~n", [Index, BuName]),

    swmOs:homes(mount),
    swmOs:clear(), 

    swmOs:install_up(UP, #{installMode => no_mount,
			   hsiEnabled => HsiEnabled}),

    OtherHomeDir = swmOs:home_dir_other(),
    RestoreDir = swmLib:restore_dir(OtherHomeDir),
    info_msg("Making symlink ~p -> ~p ~n",
	     [RestoreDir, swmLib:backup_dir(Index)]),
    ok = file:make_symlink(swmLib:backup_dir(Index), RestoreDir),


    ArchiveDir = swmServer:get_up_archive_dir(UP),
    swmOs:preload({archive, ArchiveDir}),

    swmLib:make_cxp_list(OtherHomeDir),
    InactiveInstance = integer_to_list(swmOs:get_inactive_instance()),
    swmOs:set_boot_instance(fallback, InactiveInstance),
    swmFallbackList:set_bootfallback_complete(),
    info_msg("BootFallback instance set to ~s~n", [InactiveInstance]),
    swmOs:homes(umount),

    ?LOG_INFO(["Current UP:" | swmLib:get_current_up_metadata()]),
    ?LOG_INFO(["BootFallback UP:" | swmLib:get_bootfallback_up_metadata()]),
    swmLib:internal_lock_remove([?LOCKOBJ_audit_sw], install_bootfallback),
    swmLib:internal_lock_remove([?LOCKOBJ_upgrade], install_bootfallback),
    ok;
handle_install_bootfallback(true, _Key) ->
    swmLib:internal_lock_remove([?LOCKOBJ_audit_sw, ?LOCKOBJ_upgrade],
				install_bootfallback),
    case erase(install_bootfallback_cnt) of
	Cnt when Cnt < 36000 ->
	    put(install_bootfallback_cnt, Cnt + 1),
	    retry;
	undefined ->
	    put(install_bootfallback_cnt, 1),
	    retry;
	_ ->
	    LockInfo = swmLib:internal_lock_who(?LOCKOBJ_inst_bootfb),
	    ?LOG_ERR(["Internally locked more than 1 hour!",
		      {swmLock, LockInfo}]),
	    ok
    end.

handle_restore_backup(Key) ->
    handle_restore_backup(Key, manual, sysEnv:rcs_mode_2()).
    %% handle_restore_backup(Key, auto, sysEnv:rcs_mode_2()).

handle_restore_backup(Key, Type) ->
    handle_restore_backup(Key, Type, sysEnv:rcs_mode_2()).

-spec handle_restore_backup(Key::tuple(), Type::manual|auto,
			    RcsMode::target|simulated|vrcs) -> ok.

handle_restore_backup(Key, Type, vrcs) ->
    swmvBackup:handle_restore_backup(Key, Type);
handle_restore_backup(Key, Type, _) -> 
    
    appmI:inhibit_escalation(), % HU37699
    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    %% Find the corresponding upgrade package to go with this backup
    %% These functions can handle multiple active upgrade packages, but
    %% in reality there will be only one and swmServer will then effectively
    %% use the last one in the list
    Metadata = get_metadata(Index),
    BuName = maps:get(backupName, Metadata),
    SwVersion = maps:get(swVersion, Metadata),
    HsiEnabled = case swmLib:map_get_optional(hsiEnabled, Metadata) of
		     {ok, V} -> {ok, V};
		     badkey -> undefined
		 end,
    swmLib:write_swm_log(DN, info, "Restore: "++BuName),
    info_msg("Restoring backup ~s: ~p~n",[Index, BuName]),
    update_progress(Key, [{additionalInfo, "Restoring "++BuName},
			  {progressPercentage, 1}]),
    save_progress_report(Key),

%%     case swmServer:is_soaking() of
%% 	true ->
	    case is_fast_restore_possible(SwVersion) of
		true ->
%%		    info_msg("Restore while in soaking period. "
		    info_msg("Restore. "
			     "Fast procedure~n"),
		    fast_restore(Key, Type, BuName, SwVersion);
		false ->
%%		    info_msg("Restore while in soaking period. "
		    info_msg("Restore. "
			     "Normal procedure~n"),
		    swmServer:stop_soaking_timer(),
		    swmServer:audit_software_directly(),
		    normal_restore(Key, Type, BuName, SwVersion, HsiEnabled)
%% 	    end;
%% 	false ->
%% 	    info_msg("No soaking. Using normal procedure.~n"),
%% 	    normal_restore(Key, Type, BuName, SwVersion, HsiEnabled)
    end.

normal_restore(Key, Type, BuName, SwVersion, HsiEnabled) ->
    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    swmLib:set_ram_variable(restore_backup_active, true),

    %% Wait for cancel is no longer necessary. It was added because the 
    %% restore operation was very fast, and so there was no time to cancel
    %% But later development, with disk sync to ensure status data is 
    %% properly installed on disk will always make the restore operation
    %% sufficiently slow so that a reasonably fast operatior can issue the
    %% cancelCurrentAction command

    %% We still check for cancel messages only at certain points

    %% CancelMsg = "Use cancel to stop the restore",
    %% wait_for_cancel(Type, Key, DN, CancelMsg, 3),

    check_for_cancel(Type, Key),

    wait_internal_lock(Key),
    swmLib:internal_lock([?LOCKOBJ_audit_sw], bu_restore),
    swmOs:homes(mount),
    swmOs:clear(), 

    case swmInventory:get_current_sw_version() of
	SwVersion -> 
	    %% DataOnly restore. 
	    %% Create copy of /home/sirpa/software on the 2nd area
	    update_progress(
	      Key, [{additionalInfo, "Doing a data only restore"},
		    {progressPercentage, 18}]),
	    SwDir = swmLib:software_dir(),
	    OtherSwDir = swmLib:software_dir_other(),
	    filelib:ensure_dir(OtherSwDir),
	    cmd(["mkdir ",OtherSwDir," ; cp -d ",SwDir,"/* ",OtherSwDir]),
	    AlhTag = ?ALH_TAG_DataRestore;
	_ ->
	    update_progress(Key, [{additionalInfo,
				   "Installing upgrade package"},
				  {progressPercentage, 18}]),
	    %% Assume only one UP exists
	    [UP] = swmModel:get_matching_up(SwVersion),
	    swmOs:install_up(UP, #{hsiEnabled => HsiEnabled}),
	    AlhTag = ?ALH_TAG_SoftwareRestore
    end,

    update_progress(Key,
		    [{additionalInfo, "Preparing restore database"},
		     {progressPercentage, 24}]),
    save_progress_report(Key),

    %% Create the restore directory
    %% When the system reboots, sysInitServer will look in this directory
    %% for a file named "mnesia_backup" and use it as fallback file for 
    %% mnesia. Other application may use whatever files stored in the backup
    %% directory for their own restoration.

    %% Storing the backup on the home directory is consuming to much disk
    %% space. Therefore we symlink to the correct directory instead.

    OtherHomeDir = swmOs:home_dir_other(),
    RestoreDir = swmLib:restore_dir(OtherHomeDir),
    filelib:ensure_dir(RestoreDir),
    info_msg("Making symlink ~p -> ~p ~n",
	     [RestoreDir, swmLib:backup_dir(Index)]),
    ok = file:make_symlink(swmLib:backup_dir(Index), RestoreDir),
    
    swmOs:preload(),

    %% LastChance = "Last chance to cancel. Restore will commence in 30 seconds",
    %% wait_for_cancel(Type, Key, DN, LastChance, 29),

    swmLib:set_ram_variable(restore_point_of_no_return, Key),
    check_for_cancel(Type, Key),

    swmOs:activate(),

    save_restore_file(Type, Key, OtherHomeDir),
    
    swmOs:commit(),
    
    swmOs:homes(umount),
    Msg = "Backup restored. Rebooting...",
    swmLib:write_swm_log(DN, info, Msg),
    update_progress(Key, [{additionalInfo, Msg},
                          {progressPercentage, 55}]),
    save_progress_report(Key),


    swmLib:set_variable(restore_in_progress, BuName), 

    %% info_msg("#### RESTART OMITTED FOR TEST PURPOSES~n",[]),
    swmLib:order_restart_node(cold, AlhTag),
    self()!stop_receiving_requests,      
    ok.

%% If the SwVersion of the backup is the same that is in fallback position
%% if the system is soaking after upgrade, then fast restore can be used to
%% do a data only restore, because the UP will already be in place.

fast_restore(Key, Type, BuName, SwVersion) ->
    info_msg("Fast restore to ~p~n",[BuName]),
    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    wait_internal_lock(Key),
    swmLib:internal_lock([?LOCKOBJ_audit_sw], bu_restore),
    swmLib:set_ram_variable(restore_backup_active, true),

    swmLib:set_ram_variable(restore_point_of_no_return, Key),
    check_for_cancel(Type, Key),
    swmOs:homes(mount),
    AlhTag = ?ALH_TAG_SoftwareRestore,
    update_progress(Key,
		    [{additionalInfo, "Preparing restore database"},
		     {progressPercentage, 24}]),
%    save_progress_report(Key),

    %% Create the restore directory
    %% When the system reboots, sysInitServer will look in this directory
    %% for a file named "mnesia_backup" and use it as fallback file for 
    %% mnesia. Other application may use whatever files stored in the backup
    %% directory for their own restoration.

    %% Storing the backup on the home directory is consuming to much disk
    %% space. Therefore we symlink to the correct directory instead.

    OtherHomeDir = swmOs:home_dir_other(),
    RestoreDir = swmLib:restore_dir(OtherHomeDir),
    filelib:ensure_dir(RestoreDir),
    cmd(["rm -rf ",RestoreDir]),
    info_msg("Making symlink ~p -> ~p ~n",
	     [RestoreDir, swmLib:backup_dir(Index)]),
    ok = file:make_symlink(swmLib:backup_dir(Index), RestoreDir),
    swmOs:commit(),
    swmFallbackList:remove_bootfallback_flag(),   % Restore is taking over a
						% potential bootfallback
						% installation.
    save_restore_file(Type, Key, OtherHomeDir),
    swmOs:homes(umount),
    Msg = "Backup restored. Rebooting...",
    swmLib:write_swm_log(DN, info, Msg),
    update_progress(Key, [{additionalInfo, Msg},
                          {progressPercentage, 55}]),
    save_progress_report(Key),


    swmLib:set_variable(restore_in_progress, BuName), 

    %% info_msg("#### RESTART OMITTED FOR TEST PURPOSES~n",[]),
    swmLib:order_restart_node(cold, AlhTag),
    self()!stop_receiving_requests,      
    ok.


is_fast_restore_possible([SwVersion]) ->
    swmOs:homes(mount),
    try
	true = swmFallbackList:is_bootfallback_complete(),
	SwDirInactive = swmLib:software_dir(swmOs:home_dir_other()),
	Pattern = filename:join(SwDirInactive, "*-up.xml"),
	[MetadataFile_Path] = filelib:wildcard(Pattern),
	{ConfigurationE, _} = xmerl_scan:file(MetadataFile_Path),
	SwVersionE = swmInventory:get_sw_version_pd(ConfigurationE),
	case SwVersionE#swVersion.administrativeData of
	    SwVersion -> 
		true;
	    InactiveSwVer ->
		info_msg("Fast restore is not possible!~n"
			 "Inactive  : ~s ~s~n"
			 "Active: ~s ~s~n",
			 [InactiveSwVer#'ProductData'.productNumber,
			  InactiveSwVer#'ProductData'.productRevision,
			  SwVersion#'ProductData'.productNumber,
			  SwVersion#'ProductData'.productRevision]),
		false
	end
    catch
	_ : _ ->
	    info_msg("Fast restore is not possible!~n"
		     "Inactive  : Not available~n"
		     "Active: ~s ~s~n",
		     [SwVersion#'ProductData'.productNumber,
		      SwVersion#'ProductData'.productRevision]),
	    false
    after
	swmOs:homes(umount)
    end.

%%% ###########################################################################
wait_internal_lock(Key) ->
    info_msg("Checking internal lock of 'bu_restore'...~n", []),
    wait_internal_lock(swmLib:is_internal_lock(?LOCKOBJ_bu_restore), Key).

wait_internal_lock(false, _) ->
    info_msg("No internal lock of 'bu_restore'.~n", []),
    swmLib:internal_lock(?LOCKOBJ_inst_bootfb, bu_restore);
wait_internal_lock(true, undefined) ->
    timer:sleep(100),
    wait_internal_lock(swmLib:is_internal_lock(?LOCKOBJ_bu_restore), undefined);
wait_internal_lock(true, Key) ->
    update_progress(Key, [{additionalInfo,
			   "Waiting for another action to finish"}]),
    wait_internal_lock(true, undefined).

%%% ###########################################################################

check_for_cancel(manual, Key) ->
    receive
	{cancel, Key} ->
	    appmI:cancel_inhibit(), %HU37699
	    throw(cancelled);
	force ->
	    ok
    after 0 -> 
	    ok
    end;   
check_for_cancel(auto, _) ->
    ok.


%% wait_for_cancel(manual, Key, DN, Msg, Percentage) ->
%%     update_progress(Key, [{additionalInfo, Msg},
%% 				  {progressPercentage, Percentage}]),
%%     save_progress_report(Key),
%%     Timeout = case swmLib:get_variable(swm_test_restore_timer) of
%% 		  undefined -> 30000;
%% 		  Timer -> 
%% 		      swmLib:write_swm_log(
%% 			DN, alert, 
%% 			"Cancel timeout has been set to "++
%% 			    integer_to_list(Timer div 1000)++" seconds"),
%% 		      Timer
%% 	      end,
%%     receive 
%% 	{cancel, Key} -> 
%% 	    appmI:cancel_inhibit(), % HU37699
%% 	    throw(cancelled);
%% 	force -> 
%% 	    ok
%%     after Timeout ->
%% 	    ok
%%     end;
%% wait_for_cancel(auto, _, _, _, _) ->
%%     %% Flush msgs
%%     receive 
%% 	{cancel, _} -> ok;
%% 	force -> ok
%%     after 0 -> ok
%%     end.

save_progress_report(BuKey) ->
    [Obj] = mnesia:dirty_read({brmBackup, BuKey}),
    Index = element(5, BuKey),
    Path = filename:join(swmLib:backup_dir(Index), "progress.txt"),
    {ok, Fd} = file:open(Path, [write]),
    save_record_to_file(Fd,
			record_info(fields, 'AsyncActionProgress'),
			tl(tuple_to_list(Obj#brmBackup.progressReport))),
    file:close(Fd),
    swmLib:sync().


save_record_to_file(Fd, [Field|Fields], [Value|Values]) ->
    io:format(Fd, "~p.~n",[{Field, Value}]),
    save_record_to_file(Fd, Fields, Values);    
save_record_to_file(_, [], _) ->
    ok.

is_progress_report() ->
    Pattern = filename:join([swmLib:backup_dir(), "*", "progress.txt"]),
    case filelib:wildcard(Pattern) of
	[] -> false;
	_ -> true
    end.

read_progress_report(Index, Cleanup) when is_atom(Cleanup)->
    Path = filename:join(swmLib:backup_dir(Index), "progress.txt"),
    case file:consult(Path) of
	{ok, ProgressList} ->
	    case Cleanup of
		erase ->
		    file:delete(Path);
		save ->
		    ok
	    end,
	    Values = 
		read_progress_report(record_info(fields, 'AsyncActionProgress'),
				     ProgressList),
	    list_to_tuple(['AsyncActionProgress'|Values]);
	{error, Reason} ->
	    {error, Reason}
    end;

read_progress_report([], _) -> [];
read_progress_report([Field|Fields], ProgressList) -> 
    [proplists:get_value(Field, ProgressList, undefined)|
     read_progress_report(Fields, ProgressList)].

rollback_restore_file(HomeDir) ->
    filename:join([HomeDir, "swm","bu_restore_timeout"]).

save_restore_file(auto, _, _) -> ok;
save_restore_file(manual, BuKey, HomeDir) ->
    Index = element(5, BuKey),
    case get_metadata(Index) of
	#{confirmCapable := true} ->
	    info_msg("Backup ~s is confirm capable~n",[Index]),
	    do_save_restore_file(BuKey, HomeDir);
	_ ->
	    info_msg("Backup ~s is NOT confirm capable~n",[Index]),
	    ok
    end.

do_save_restore_file(BuKey, HomeDir) ->
    [Obj] = mnesia:dirty_read({brmBackup, BuKey}),
    Progress = Obj#brmBackup.progressReport,
    [RAS] = ets:tab2list(brmRollbackAtRestore),
    %% case RAS#brmRollbackAtRestore.timeAllowedBeforeRollback,
    %% 	  swmLib:get_variable(confirm_restore)} of
    case RAS#brmRollbackAtRestore.timeAllowedBeforeRollback of
	%% {Timeout, _} when is_integer(Timeout) ->
	Timeout when is_integer(Timeout) ->
	    EndTime = 
		calendar:gregorian_seconds_to_datetime(
		  calendar:datetime_to_gregorian_seconds(
		    calendar:universal_time())+Timeout),
	    Path = rollback_restore_file(HomeDir),
	    filelib:ensure_dir(Path),
	    {ok, Fd} = file:open(Path, [write]),
	    io:format(Fd, "~p.~n",[{ongoing_restore, BuKey}]), 
	    io:format(Fd, "~w.~n",[{restore_expires, EndTime}]),

	    %% Add a marker for the start of AsyncActionProggress
	    io:format(Fd, "{progress_info, start}.~n",[]),
	    save_record_to_file(
	      Fd, record_info(fields, 'AsyncActionProgress'),
	      tl(tuple_to_list(Progress))),
	    file:close(Fd),
	    info_msg("Restore file saved: ~p~n",[Path]);
	%% {Timeout, ConfirmFlag} ->
	Timeout ->
	    info_msg("Restore file omitted. Timeout ~w~n",
		     [Timeout]),
	    Index = element(5, BuKey),
	    Dn = "BrmBackup="++Index,
	    swmLib:write_swm_log(
	      Dn, info, "No rollback time specified. Autoconfirm")
    end.

confirm_restore(Key) ->
    %% Fail cases are handled in init_backup
    Index = element(5, Key),
    Path = filename:join(swmLib:backup_dir(Index),"metadata"),
    {ok, [BuData]} = file:consult(Path),
    [BrmBackup] = mnesia:dirty_read({brmBackup, Key}),
    Progress = BrmBackup#brmBackup.progressReport,
    confirm_restore(Key, convert_metadata(BuData), Progress).
    
confirm_restore(Key, BuData, Progress) ->
    ContArg = #{key => Key, bu_data => BuData, progress => Progress},
    case swmvBuRestore:is_ongoing() of
	true ->
	    CbFun = fun() -> confirm_restore_cont(ContArg) end,
	    swmvBuRestore:confirm_restore(CbFun);
	false ->
	    confirm_restore_cont(ContArg)
    end.

confirm_restore_cont(#{key := Key, bu_data := BuData, progress := Prgss}) ->
    Progress = read_progress(Key, Prgss),
    #{backupName := Name,
      creationTime := CreationTime,
      swVersion := SwVersion,
      creationType := CreationType} = BuData,
    info_msg("Confirming restore ~p!~n",[Name]),
    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    ProgressData =
	[{result, ?ActionResultType_SUCCESS},
	 {resultInfo, "The backup restore is complete"},
	 {progressPercentage, 100},
	 {state, ?ActionStateType_FINISHED},
	 {timeActionCompleted, CompleteTime}],
    NewProgress = comsaI:update_progress(ProgressData, Progress),
    swmLib:write_swm_log(DN, info, "Backup restored: "++Name),
    [Obj] = ets:tab2list(brmBackupLabelStore),
    mnesia:dirty_write(
      #brmBackup{brmBackupId = Key,
		 backupName = Name,
		 creationTime = CreationTime,
		 status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
		 swVersion = SwVersion,
		 creationType = CreationType,
		 progressReport = NewProgress}),
    
    mnesia:dirty_write(Obj#brmBackupLabelStore{lastRestoredBackup=Name}),
    [RAR] = ets:tab2list(brmRollbackAtRestore),
    NewRAR = RAR#brmRollbackAtRestore{timeRemainingBeforeRollback = undefined},
    mnesia:dirty_write(NewRAR),
    %% HT75846 remove restore dir
    RestoreDir = swmLib:restore_dir(),
    cmd(["rm -rf ",RestoreDir]),
    Path = filename:join(swmLib:backup_dir(Index), "progress.txt"),
    file:delete(Path),
    swmFallbackList:add_backup(latest, Name),
    %% swmFallbackList:audit_bootfallback() calls swmBackup so it must be 
    %% spawned to avoid a deadlock
    proc_lib:spawn(swmFallbackList, audit_bootfallback, []).

read_progress(Key, Prgss) ->
    case mnesia:dirty_read({brmBackup, Key}) of
	[#brmBackup{progressReport = P}] ->
	    P;
	[] ->
	    Prgss
    end.

initiate_confirm_timeout() ->
    %% DoConfirmRestore = 
    %% 	case swmLib:get_variable(confirm_restore) of
    %% 	    true -> true;
    %% 	    _ -> false
    %% 	end,
    RestoreFile = rollback_restore_file(sysEnv:home_dir()),
    case file:consult(RestoreFile) of
	{error, enoent} ->
	    info_msg("Confirm file not found! (init) ~n"),
	    no_ongoing_confirm;
	%% {ok, Terms} when DoConfirmRestore ->
	{ok, Terms}  ->
	    info_msg("Confirm file found! (init)~n"),
	    Expiry = 
		proplists:get_value(restore_expires, Terms),
	    Timeout = calendar:datetime_to_gregorian_seconds(Expiry)-
		calendar:datetime_to_gregorian_seconds(
		  calendar:universal_time()),
	    [RAR] = ets:tab2list(brmRollbackAtRestore),
	    case Timeout of
		Timeout when Timeout >= 0 ->
		    %% There is time left
		    
		    {ok, TimerRef} =
			timer:send_after(Timeout*1000, confirm_timeout),
		    info_msg("Confirm ongoing, starting timer! (~p) ~n",
			     [Timeout]),
		    NewRAR = RAR#brmRollbackAtRestore{
			       timeRemainingBeforeRollback = Timeout},
		    mnesia:dirty_write(NewRAR),
		    BuKey = proplists:get_value(ongoing_restore, Terms),

		    Progress = get_progress_from_restore_file(Terms),
		    AI = [{additionalInfo, "Waiting for confirmRestore"},
			  {progressPercentage, 90}],
		    NewProgress = comsaI:update_progress(AI, Progress),
		    Index = element(5, BuKey),
		    DN = "BrmBackup="++Index,
		    swmLib:write_swm_log(DN, warning, 
					 "Waiting for confirmRestore"),
		    [BrmBackup] = mnesia:dirty_read({brmBackup, BuKey}),
		    mnesia:dirty_write(
		      BrmBackup#brmBackup{progressReport = NewProgress}),
		    {ongoing_confirm, TimerRef};
		_ ->
		    info_msg("Confirm file found, but time expired!~n"),
		    %% After initialization go directly to confirm_timeout
		    swmBackup ! confirm_timeout,
		    no_ongoing_confirm
	    end;
	_ -> 
	    %% Confirm is disable. Don't start a timer.
	    %% Immediate autoconfirm happens in the BrmBackup MO recovery phase
	    no_ongoing_confirm
    end.

%%% ----------------------------------------------------------
get_progress_from_restore_file([{progress_info, start}|ProgressList]) ->	
    Values = 
	read_progress_report(record_info(fields, 'AsyncActionProgress'),
			     ProgressList),
    list_to_tuple(['AsyncActionProgress'|Values]);
get_progress_from_restore_file([_|Terms]) ->		
    get_progress_from_restore_file(Terms).
								   





%% set_creation_type_manual(Index) ->
%%     set_creation_type(Index, ?BrmBackupCreationType_MANUAL).

%% set_creation_type(Index, NewCT) ->
%%     swmLib:lock_backup(Index),
%%     try do_set_creation_type(Index, NewCT) 
%%     after
%% 	swmLib:unlock_backup(Index)
%%     end.

%% do_set_creation_type(Index, NewCT) ->
%%     Md = get_metadata(Index),
%%     Path = filename:join(swmLib:backup_dir(Index), "metadata"),
%%     case filelib:is_file(Path) of
%% 	true ->
%% 	    {ok, Fd} = file:open(Path, [write]),
%% 	    io:format(Fd, "~p.~n", [Md#metadataV2{creationType=NewCT}]),
%% 	    file:close(Fd);
%% 	false ->
%% 	    %% This is because housekeeping sometimes manage to remove the
%% 	    %% backup before this happens
%% 	    ok
%%     end.


%%% ----------------------------------------------------------
%%% @doc Returns true if some backup refers to an upgrade
%%% package identified by the given product data.
%%% @end
%%% ----------------------------------------------------------

-spec has_dependent_backups(#'ProductData'{}) ->  boolean().

has_dependent_backups(
  #'ProductData'{productNumber=Num, productRevision=Rev}) ->
    FirstKey = mnesia:dirty_first(brmBackup),
    has_dependent_backups(Num, Rev, FirstKey).


has_dependent_backups(_Num, _Rev, '$end_of_table') ->
    false;

has_dependent_backups(Num, Rev, BrmBackupKey) ->
    [#brmBackup{swVersion=ProductDatas}] = 
	mnesia:dirty_read(brmBackup, BrmBackupKey),
    case is_one_product_of(Num, Rev, ProductDatas) of
	true ->
	    true;
	_ ->
	    NextKey = mnesia:dirty_next(brmBackup, BrmBackupKey),
	    has_dependent_backups(Num, Rev, NextKey)
    end.

is_one_product_of(Num, Rev, [ProductData|Tail]) ->
    if
	Num =:= ProductData#'ProductData'.productNumber
	andalso Rev =:= ProductData#'ProductData'.productRevision ->
	    true;
	true ->
	    is_one_product_of(Num, Rev, Tail)
    end;

is_one_product_of(_Num, _Rev, []) ->
    false.




%%% ----------------------------------------------------------
%%% @doc Cleans up any open sftp connection or ftpes client

cleanup() ->
    swmLib:set_ram_variable(restore_backup_active, false),
    case get(reboot_ordered) of   % HW25300
	true ->
	    ok;
	_ ->
	    %% End of HW25300
	    swmLib:internal_lock_remove([?LOCKOBJ_inst_bootfb,
					 ?LOCKOBJ_audit_sw],
					bu_restore)
    end,
    cleanup(get(protocol)).

cleanup(sftp) ->
    case get(channelPid) of
        Pid when is_pid(Pid) ->
            case get(handle) of
                undefined ->
                    ok;
                _-> 
                    ftpI:close(sftp, Pid, get(handle))
            end,
            ftpI:stop_channel(sftp, Pid);
        undefined -> ok
    end,
    case get(connectionRef) of
    CRef when is_pid(CRef) -> ssh:close(CRef);
    undefined -> ok
    end;
cleanup(ftpes) ->
    case get(channelPid) of
    Pid when is_pid(Pid) ->
            %% close file if opened
            ftpI:close(ftpes, Pid, []),
            ftpI:stop_channel(ftpes, Pid);
    undefined -> ok
    end;
cleanup(_Other) ->
    ok.

%%% ----------------------------------------------------------
handle_mnesia_table_event({_, swmFallbackList, _, _, _}) ->
    List = swmFallbackList:get_fallback_list(),
    {atomic, ok} = 
	mnesia:transaction(
	  fun() ->
		  [Obj] = mnesia:wread({brmBackupLabelStore, 
					{"1","1","1","1","1"}}),
		  New = Obj#brmBackupLabelStore{restoreEscalationList = List},
		  mnesia:write(New)
	  end),
    internal_housekeeping(),
    ok.

%%% ----------------------------------------------------------
%%% #           internal_housekeeping()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Remove unused system created backups
%%% ----------------------------------------------------------
%% HS30402, HS44344, HT10245


do_internal_housekeeping() ->
    case swmLib:lock_action_capable(
	   ?INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_ID,
	   ?INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_INFO) of
	ok ->
	    WP = mnesia:table_info(brmBackup, wild_pattern),
	    Pattern = WP#brmBackup{creationType =
				       ?BrmBackupCreationType_SYSTEM_CREATED},

	    {atomic, ok} = 
		mnesia:transaction(
		  fun() ->
			  ScBackups = mnesia:match_object(Pattern),
			  [begin
			       BackupName = Backup#brmBackup.backupName,
			       info_msg("Internal housekeeping removes ~p~n",
			       		[BackupName]),
			       delete_backup_internal(BackupName, forced)
			   end||
			      Backup<-ScBackups,
			      not is_protected_backup(
				    Backup#brmBackup.backupName)],
			  ok
		  end),
	    ok = swmLib:unlock_action_capable(
		   ?INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_ID),
	    ok;
	{nok, _} ->
	    ok
    end.

-spec is_protected_backup(BackupName::string()) -> boolean().

is_protected_backup(BackupName) ->
    [LS] = mnesia:read({brmBackupLabelStore, 
			{"1","1","1","1","1"}}),
    Used =
	case (LS#brmBackupLabelStore.restoreEscalationList) of
	    undefined -> [];
	    List -> List
	end++
	case swmLib:get_variable(pending_ug_backup) of
	    undefined ->
		[];
	    BuName ->
		[BuName]
	end++
	case swmFailsafe:get_failsafe_backup() of
	    undefined ->
		[];
	    Failsafe ->
		[Failsafe]
	end++
	[aicI:get_backup_name()],
    info_msg("Protected backups: ~p~n",[Used]),
    lists:member(BackupName, Used).		



%%% ----------------------------------------------------------
%%% LIBRARY
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% @doc Execute a shell command and print the result in the erlang shell

cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.


info_msg(Format) ->
    info_msg(Format, []).
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
warning_msg(Format, Args) ->
    try
	sysInitI:warning_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:warning_msg("~w: "++Format, [?MODULE|Args])
    end.

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


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


scratch_backups() ->
    Keys = mnesia:dirty_all_keys(brmBackup),
    [mnesia:dirty_delete({brmBackup,Key})||Key<-Keys],
    cmd(["rm -rf ", swmLib:backup_dir(), "/*"]),
    [Obj] = mnesia:dirty_read({brmBackupLabelStore, {"1","1","1","1","1"}}),
    mnesia:dirty_write(Obj#brmBackupLabelStore{restoreEscalationList=undefined}).

force_restore() ->
    swmBackup!force.


restore_lab_settings() ->
    restore_lab_settings(5000, 100).

restore_lab_settings(_, N) when N<1 ->
    info_msg("Restore lab settings failed~n");	
restore_lab_settings(Timer, N) ->
    case whereis(aicServer) of
	undefined ->
	    timer:sleep(Timer),
	    restore_lab_settings(Timer, N-1);
	P when is_pid(P) ->
	    do_restore_lab_settings()
    end.

do_restore_lab_settings() ->
    Path = "/rcs/networkloader/siteBasicFile.netconf_loaded_ok",
    NewPath = "/rcs/networkloader/siteBasicFile.netconf",
    file:rename(Path, NewPath),
    mnesia:dirty_write({role,{"1","1","1","1","1","expert"},
			"expert","Lab use only"}),
    mnesia:dirty_write({rule,{"1","1","1","1","1","expert","root"},
			undefined,7,"ManagedElement,*",
			"Access to the entire model"}),

    MeData =  comsaI:get_managed_element_data(),
    MeId = proplists:get_value(networkManagedElementId, MeData),
    case MeId of
	undefined -> gen_server:cast(aicServer, {load_config, [NewPath]});
	"1" -> gen_server:cast(aicServer, {load_config, [NewPath]});
	_ ->
	    [DelMeId, SetMeId] = make_meid_files(MeId),
	    gen_server:cast(
	      aicServer, {load_config, [DelMeId, NewPath, SetMeId]})
    end.

make_meid_files(MeId) ->
    DelMeId = "/tmp/delMeId.netconf",
    SetMeId = "/tmp/setMeId.netconf",

    file:write_file(DelMeId, list_to_binary(get_del_data(MeId))),
    file:write_file(SetMeId, list_to_binary(get_set_data(MeId))),
    [DelMeId, SetMeId].

get_del_data(MeId) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <capabilities>
	<capability>urn:ietf:params:netconf:base:1.0</capability>
	</capabilities>
	</hello>]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"1\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config>
    <target>
      <running/>
    </target>
    <config>
    <ManagedElement xmlns=\"urn:com:ericsson:ecim:ComTop\">
      <managedElementId>"++MeId++"</managedElementId>
      <networkManagedElementId>1</networkManagedElementId>
    </ManagedElement>
    </config>
  </edit-config>
</rpc>
]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"2\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <close-session/>
</rpc>]]>]]>".

get_set_data(MeId) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <capabilities>
    <capability>urn:ietf:params:netconf:base:1.0</capability>
  </capabilities>
</hello>]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"1\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config>
    <target>
      <running/>
    </target>
    <config>
    <ManagedElement xmlns=\"urn:com:ericsson:ecim:ComTop\">
      <managedElementId>1</managedElementId>
      <networkManagedElementId>"++MeId++"</networkManagedElementId>
    </ManagedElement>
    </config>
  </edit-config>
</rpc>
]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"2\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <close-session/>
</rpc>]]>]]>".
	
test_create_backup(Type, Name) ->
    BackupName = list_to_binary(Name),
    BrmBackupManagerKey = {"1","1","1","1"},
    Progress = undefined,
    {group_leader, GroupLeader}=process_info(whereis(swmBackup), group_leader),
    erlang:group_leader(GroupLeader, self()),
    create_backup_common(BackupName, BrmBackupManagerKey, Type, Progress).

test_internal_housekeeping() ->
    {group_leader, GroupLeader}=process_info(whereis(swmBackup), group_leader),
    erlang:group_leader(GroupLeader, self()),
    internal_housekeeping().


%%% ----------------------------------------------------------
%% elems2string(#'AsyncActionProgress'{result = Result, state = State} = Rec) ->
%%     Rec#'AsyncActionProgress'{result = actionResultType(Result),
%% 			      state = actionStateType(State)}.

%%% ----------------------------------------------------------
%% actionResultType(?ActionResultType_SUCCESS) ->
%%     "SUCCESS";
%% actionResultType(?ActionResultType_FAILURE) ->
%%     "FAILURE";
%% actionResultType(?ActionResultType_NOT_AVAILABLE) ->
%%     "NOT_AVAILABLE";
%% actionResultType(Other) ->
%%     sysUtil:term_to_string(Other).

%% %%% ----------------------------------------------------------
%% actionStateType(?ActionStateType_CANCELLING) ->
%%     "CANCELLING";
%% actionStateType(?ActionStateType_RUNNING) ->
%%     "RUNNING";
%% actionStateType(?ActionStateType_FINISHED) ->
%%     "FINISHED";
%% actionStateType(?ActionStateType_CANCELLED) ->
%%     "CANCELLED";
%% actionStateType(Other) ->
%%     sysUtil:term_to_string(Other).

%%%-----------------------------------------------------------
%% protocol_to_text(Proto) ->
%%     atom_to_list(Proto).

%%% ----------------------------------------------------------
%%% #5.    OLD REVISIONS LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-01-10 etxbjca     Created
%%% R1A/4      2012-02-02 etxjotj     Create/delete actions working
%%%                                   Preparation for all other actions
%%% R2A/7      2013-01-28 etxjotj     Fixed selection of bu restore method
%%% R2A/46     2013-10-17 erarafo     Support for new Oam Mgmt SPI in COM
%%% R2A/59     2014-01-28 etxjotj     Prevent system created backups from 
%%%                                   being deleted by the operator
%%% R2A/66     2014-02-19 etxberb     Replaced 'init:reboot' with
%%%                                   appmI:restart_node and added AVLI event
%%%                                   cause codes.
%%% R2A/67     2014-02-21 etxarnu     Added revert/1 (just a stub so far)
%%% R2A/71     2014-02-28 erarafo     Fixed HS38585 mailed by uabguba 2014-02-28
%%% R2A/72     2014-03-03 etxtory     Handling backup from AI
%%% R2A/73     2014-03-03 etxjotj     HS37092, 2038 bug in single event
%%% R2A/74     2014-03-14 etxderb     Fault case, cleanup old export files.
%%% R2A/80     2014-03-18 etxjotj     Cleaned up some code
%%% R2A/81     2014-03-21 etxberb     Save log entries before backup restore
%%% R2A/82     2014-03-24 etxjotj     get_backup_by_name, set_creation_type
%%%                                   Moved revert to swmFallbackList
%%% R2A/83     2014-03-25 etxjotj     Internal housekeeping: HS30402, HS44344
%%% R2A/85     2014-04-03 etxjotj     Copy old backup tables at upgrade
%%% R2A/87     2014-04-11 etxjotj     HS46968 fixed
%%% R2A/88     2014-04-13 etxjotj     HS46984 fixed
%%% R2A/92     2014-05-20 etxjotj     HS62346 fixed
%%% R2A/93     2014-05-28 erarafo     HS62185: UP removal when backup refers it
%%% R2A/94     2014-06-02 etxjotj     HS66152 Only show relative BrmBackup path
%%% R2A/95     2014-06-12 etxjotj     Adaptions for EE split
%%% R2A/96     2014-06-27 etxberb     Changed error to warning in init_backup/2
%%% R2A/97     2014-07-08 etxjotj     HS77223 Standardized file naming 
%%% R2A/98     2014-07-11 etxjotj     Fix in progress reporting
%%% R2A/99     2014-07-21 etxjotj     HS77223 again, backupinfo.xml fix
%%% R2A/100    2014-07-22 etxjotj     No password in backup export handling
%%% R2A/101    2014-07-22 etxjotj     Extended timeout for create backup in test
%%% R2A/102    2014-07-23 etxjotj     HS80846 Cancel should result in Failure
%%%                                   Bugfix for create timeout
%%% R2A/103    2014-07-31 etxjotj     Added restore progress
%%% R2A/104                           More progress
%%% R2A/107    2014-08-05 etxjotj     Concurrency problem in set_creation_type
%%% R2A/108    2014-08-12 etxjotj     Improved concurrency handling for files
%%% R2A/109    2014-09-04 etxjotj     HS73150 Use DSCP value
%%% R2A/111    2014-09-11 etxtory     excluded-file added for tar
%%% R2A/112    2014-09-23 etxpejn     Added call to sysSftp TR HS89846
%%% R2A/113    2014-10-03 etxjotj     HT10245 Delayed internal housekeeping
%%% R2A/114    2014-10-10 etxjotj     HT12440 Block restore during upgrade
%%%                                   Settable delayed internal housekeeping
%%% R2A/115    2014-10-15 etxjotj     COLI set-housekeeping-delay support
%%% R3A/2      2014-11-28 etxberb     Added values/1.
%%% R3A/5      2014-12-12 etxberb     Bug correction.
%%% R3A/6      2015-02-06 etxjotj     Housekeeping when several requests were 
%%%                                   made at the same time
%%% R3A/7      2015-03-17 etxjotj     HT53117 Always fill in progress report
%%%                                   HT56916 Forcibly reset mgr pr at restore
%%% R3A/9      2015-03-26 etxberb     Added restore_backup_active flag.
%%% R3A/10     2015-03-27 etxjotj     Removed legacy action/3
%%%                                   Fixed createBackup with default name
%%% R3A/11     2015-04-07 etxjotj     Added sync during restore for progress
%%% R3A/12     2015-04-08 etxjotj     Removed sleep time after sync
%%% R3A/13     2015-04-16 etxjotj     HT65980 Return error code for make fail
%%% R3A/14     2015-04-24 etxjotj     HT69297 Added log entries for bu status
%%% R3A/15     2015-04-28 etxjotj     Added resultInfo for import backup
%%%                                   Added force restore
%%% R3A/16     2015-04-28 etxjotj     Added info_msg/1
%%% R3A/17     2015-04-29 etxjotj     Backups in ESI support
%%% R3A/18     2015-04-30 etxjotj     HT69297 Protect against interruptions 
%%%                                   during restore
%%% R3A/19     2015-05-04 etxjotj     Removed error printout
%%% R3A/20     2015-05-05 etxjotj     Logging improvments
%%% R3A/21     2015-05-05 etxjotj     HT67309 Use import/restore for AI BU
%%% R3A/22     2015-05-19 etxjotj     HT75846 Remove /h/sirpa/restore dir
%%% R3A/23     2015-05-28 etxjotj     Don't use home for backups handling
%%%                                   Restore lab conditions after loading
%%%                                   a backup from the field
%%% R3A/24     2015-06-05 etxjotj     Don't run restore while auditing sw
%%%                                   Configurable cancel restore timeout
%%% R3A/25     2015-06-18 etxjotj     Remove mnesia symlink if it exists
%%% R4A/1      2015-08-21 etxpejn     Changed logI:write_log to swmLib:write_swm_log
%%% R4A/4      2015-09-09 etxjotj     Library functions for sync and install bu
%%% R4A/5      2015-09-11 etxtory     maxStoredManualBackups=20 
%%% R4A/6      2015-09-14 etxlg       Alternate connection
%%% R4A/8      2015-09-21 etxjotj     Action id fix
%%% R4A/9      2015-09-22 etxjotj     Action id fix 2
%%% R5A/1      2015-10-01 etxjotj     Zipped backups
%%% R4A/10     2015-09-28 etxjotj     Spelling fix
%%% R4A/11     2015-09-29 etxjotj     Label store handling for mnesia on tmp
%%% R4A/12     2015-10-20 etxjotj     Additional progress info created
%%% R4A/13     2015-10-23 etxjotj     Cluster aware backup creation
%%% R4A/14     2015-10-29 etxjotj     HU30730: Lock system backups 
%%% R4A/15     2015-11-04 etxjotj     Don't use lock only for system created bu
%%% R4A/16     2015-11-04 etxjotj     Strange compiler bug fix
%%% R4A/17     2015-11-04 etxjotj     Don't try to housekeep locked backup
%%% R4A/18     2015-11-05 etxjotj     Changed backup lock handling
%%% R4A/19     2015-11-11 etxjotj     Protect AI backup
%%% R5A/2      2015-10-08 etxjotj     ECIM BrM 3.4
%%% R5A/3      2015-11-11 etxjotj     Merge from R4A
%%% R5A/4      2015-11-13 etxjotj     Failsafe deprecated support
%%% R4A/20     2015-11-11 etxjotj     Printout fix in test case
%%% R4A/21     2015-11-20 etxjotj     HU37699 Prevent escalation at restore
%%% R4A/22     2015-11-23 etxjotj     HU37699 is also HU37080
%%% R5A/7      2015-12-07 etxtotj     HU33925 fix
%%% R5A/8      2015-12-07 etxjotj     Added comment
%%% R5A/9      2016-02-04 erarafo     Handling "no such file" case
%%% R5A/11     2016-02-18 etxarnu     Removed dbg: stuff
%%% R5A/12     2016-02-26 etxberb     Fixed edoc problem from backwards merge.
%%% R5A/13     2016-03-11 etomist     Added ActionCapability lock/unlock
%%% R5A/14     2016-03-14 etomist     Added ActionCapability lock/unlock to restore_backup
%%% R5A/15     2016-03-16 etomist     Add lock_fail exception handling to action
%%% R5A/16     2016-03-25 ekurnik     Added ActionCapability lock/unlock to housekeeping
%%% R5A/18     2016-04-01 etxjotj     HU71354 Fix
%%%                                   ESI additions
%%% R4A/23     2016-05-10 etxjojt     HU82011 Prevent backup when in failsafe
%%%
%%% R6A/1      2016-06-16 etomist     HU92050 More descriptive progressInfo
%%% R6A/2      2016-06-30 etxjotj     Changed progress info regarding nw errors
%%% R6A/3      2016-07-05 etxjotj     ECIM BRM 3.5.1
%%% R6A/4      2016-07-05 etxjotj     Removed dependencies to a lot of hrl files
%%% R6A/5      2016-07-06 etxjotj     Bugfix when creating a backup
%%% R6A/6      2016-07-08 etxjotj     Support for removing all backups
%% R6A/7   2016-09-14 etxberb  Added install_bootfallback/0,
%% R6A/8   2016-09-15 etxberb  Removed 'activate' from install_bootfallback.
%% R6A/9   2016-09-16 etxpejn  Added einval as a fault code to start_channel
%% R6A/10  2016-09-19 etxberb  Disabled install_bootfallback temporarily...
%% R6A/11  2016-09-20 etxjotj  Confirm restore
%% R6A/12  2016-09-20 etxjotj  Extra merge handling between /10 and /11
%% R6A/13  2016-09-22 etxjotj  Directory bugfixes in SIM
%% ------  ---------- -------- ------------------------
%% R7A/1   2016-09-30 etxberb  Added init_config_download/2 &
%%                             init_config_prepare/0.
%% R7A/2   2016-09-30 etxjotj  Bugfix when timeAllowedBeforeRollback is undef
%% R7A/3   2016-10-06 etxjotj  Added a vrcs option for restore backup
%% R7A/4   2016-10-07 etxjotj  HV30481: Backed out restore confirm
%% R7A/5   2016-10-07 etxjotj  System created backups deletable
%% R7A/7   2016-10-13 etxjotj  Final backup for restore
%% R7A/8   2016-10-13 etxberb  Added init_config_activate/0 &
%%                             is_init_config_enabled/0.
%% R7A/9   2016-10-13 etxberb  Fixed dialyzer problem.
%% R7A/10  2016-10-13 etxjotj  Final backup for restore completed
%% R7A/11  2016-10-13 etxjotj  Dialyzer fixses
%% R7A/12  2016-10-14 etxjotj  Backed out final backup, add restored backup
%% R7A/13  2016-10-14 etxjotj  New VRCS backup restore
%% R7A/14  2016-10-15 etxjotj  Fixed problem with new ai backup making
%% R7A/15  2016-10-17 etxjotj  Backed out add restored backup
%% R7A/16  2016-10-17 etxberb  Bootfallback installation disabled (WP5541).
%% ------  ---------- -------- ------------------------
%% R8A/1   2016-10-25 etxberb  Bootfallback installation enabled (WP5541).
%% R8A/2   2016-10-25 etxjotj  Add support for AIC in escalation list
%%                             Latest restored in escalation list
%% R8A/3   2016-11-17 edamkon  HV41457 Prevent backup while system is upgrading
%% R8A/6   2016-12-05 etxberb  Moved lock of upgrade to prevent executing
%%                             audit_software in parallel.
%% R8A/7   2016-12-06 erarafo  Fixed compiler warnings.
%% R8A/8   2016-12-15 etxjotj  New metadata.v3
%% R8A/9   2016-12-15 etxjotj  Upgrade support for new metadata
%% R8A/10  2016-12-16 etxberb  Fixed one dialyzer problem.
%% R8A/11  2017-01-12 etxjotj  Added stacktrace in create_backup_common
%% R8A/12  2017-01-16 etxjotj  HV55838 Improved AI backup handling
%% R8A/13  2017-01-17 etxberb  Added 'schema = file' in export_backup.
%% R8A/14  2017-01-17 etxjotj  Enabled coli command for confirm restore
%% R8A/15  2017-01-17 emarnek  Switched ssh_sftp and sysSftp to ftpI
%% R8A/16  2017-01-19 etxberb  HV56969: Introducing mutual lock bu_restore <->
%%                                      audit_software.
%% R8A/17  2017-01-20 etxberb  Moved set_default_progress_report from swmBackup
%%                             process to calling (comsa) process (do_action).
%% ------  ---------- -------- ------------------------
%% R9A/1   2017-01-29 etxberb  Added create_backup/2 & ActionId in create_backup
%% R9A/2   2017-02-02 etxberb  Added export_backup/4 for REST API (swmv1.erl)
%% R9A/3   2017-02-09 edamkon  Handle HTTP encoded URIs; HV58153
%% R9A/5   2017-02-13 etxberb  Added init_config_copy_shared/0.
%% R9A/7   2017-02-20 etxjotj  Fixed problem with confirm timeotu
%% R9A/8   2017-02-21 estjako  Support for ftpes
%% R9A/9-10 2017-02-23 etxberb  TR HV65937, bootfallback unlocks upgrade and
%%                             audit_sw when bootfallback is locked.
%% R9A/11  2017-03-15 etxberb  Disabled install_bootfallback.
%% R9A/12  2017-03-27 etxjotj  Confirm restore activation
%% R9A/13  2017-03-29 etxpejn  HV63703 skip cancel is restore is busy
%% R9A/14  2017-03-29 etxpejn  Added printout
%% R9A/15  2017-03-30 etxjotj  Lock out confirm restore again
%% R9A/17  2017-04-05 etxjotj  Temporarily add cert keys to backup
%% R9A/18  2017-04-06 etxjotj  Temporarily added unpack of cert keys from backup
%% R9A/19  2017-06-16 etxberb  HV96257: Considering HsiEnabled in install_up.
%% ----    ---------- -------  -------------------------------------------------
%% R10A/1  2017-05-03 etxberb  Added delete_backup/2.
%% R10A/2  2017-05-04 etxberb  Added export_backup_sched/4.
%% R10A/3  2017-06-11 etxjotj  HV92792 Soaking after upgrade
%% R10A/4  2017-06-12 etxjotj  Audit software lock on data only restore
%% R10A/5  2017-06-16 etxberb  * Merge from R9A/19.
%%                             * Added create_upgrade_backup/2.
%% R10A/6  2017-07-11 etxjotj  Changed API to swmOs:install_up
%% R10A/7  2017-07-12 etxjotj  Fixed syntax error

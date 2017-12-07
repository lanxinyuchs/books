%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmBackup.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/R5A/16

%%% @doc ==Backup managmement==
%%% This module contains all the functionality for creating and using
%%% backups as well as the agent implementation of the ECIM BrM model

-module(swmBackup).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/16').
-date('2016-03-25').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0, init_bu_info/0, activate/0]).
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 action/4,
	 createMo/5]).

%%% For comsaEvent
-export([format_next_scheduled_time/1]).

%%% For upgrade/failsafe
-export([create_backup/1, get_mnesia_file/1]).
-export([set_default_progress_report/3]).
-export([delete_system_created_backup/2]).
-export([restore_backup/1]).
-export([is_progress_report/0]).
-export([internal_housekeeping/0]). % HS51856

%%% For scheduled backups, failsafe, esi
-export([create_backup_common/4, validate_name/1,
	 update_progress/2, action_handler/2]).
-export([compress_file/2]).
-export([failsafe_lock/0, failsafe_release/0]).

%%% General purpose
-export([get_backup_by_name/1, 
	 set_creation_type/2, 
	 set_creation_type_manual/1,
	 has_dependent_backups/1]).

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

-export([restart_node/2]).

%%% ----------------------------------------------------------
-include("ComTop.hrl").
-include("RcsBrM.hrl").
-include("RcsSwM.hrl").
-include("SwmInternal.hrl").
-include("comte_types.hrl").
-include("alhI.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("ComSysM.hrl").
-include("ComSnmp.hrl").
-include("RcsLdapAuthentication.hrl").
%%% ----------------------------------------------------------
-record(transfer, {fd, pid, handle, rp_key, total_size, path}).
-compile(nowarn_unused_vars).
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
%%% @doc Creates a backup (during upgrade)
%%% @end

create_backup(BuName) ->
    DnRev = [<<"1">>,<<"BrmBackupManager">>,<<"1">>,<<"BrM">>,
	     <<"1">>,<<"SystemFunctions">>,<<"1">>,<<"ManagedElement">>],

    gen_server:call(swmBackup, {createBackup, list_to_binary(BuName), DnRev},
		    300000).


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
    swmLib:erase_variable(bu_restore),
    gen_server:cast(swmBackup,  restore_ai_bu),
    swmFallbackList:audit_fallback_list(),
    internal_housekeeping(),
    ok.

%%% ----------------------------------------------------------
%%% @doc Create the system created brm objects. Then scan the
%%% $RCS_ROOT/rcs/swm/backup dir for any backups
%%% @end

init_bu_info() ->
    case swmI:is_upgrade_ongoing() of
	true ->
	    [swmI:copy_old_table(Tab)||Tab<-[brM, 
					     brmBackupManager,
					     brmSingleEvent,
					     brmPeriodicEvent,
					     brmBackupHousekeeping,
					     %% brmBackupScheduler,
					     brmCalendarBasedPeriodicEvent,
					     brmRollbackAtRestore,
					     brmBackupLabelStore,
					     %% brmFailsafeBackup,
					     swmFallbackList]],
	    update_house_keeping(),
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
			   autoExport = Export,
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
    mnesia:dirty_write(
      #brM{brMId={"1","1","1"}}),

    mnesia:dirty_write(
      #brmBackupManager{brmBackupManagerId = {"1","1","1","1"},
			backupType = "Systemdata",
			backupDomain = "System",
			progressReport = default_idle_pr()
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
%			  schedulerState = ?OperState_ENABLED,
			  adminState = ?BasicAdmState_UNLOCKED}), % HS62346
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

init_backup_metadata(RestoredBuName, Index, BuData) ->
    
    Key = {"1","1","1","1",Index},
    case mnesia:dirty_read({brmBackup, Key}) of
	[Bu] ->
	    audit_backup_object(Bu, BuData#metadataV2.backupName,
				BuData#metadataV2.creationTime);
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
	case read_progress_report(Index) of
	    {error, _} ->
		%% error_msg("~s: No progress report: ~p~n",[DN, {error, R}]),
		default_idle_pr();
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
    mnesia:dirty_write(
      #brmBackup{brmBackupId = Key ,
		 backupName = BuData#metadataV2.backupName,
		 creationTime = BuData#metadataV2.creationTime,
		 status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
		 swVersion = BuData#metadataV2.swVersion,
		 creationType = BuData#metadataV2.creationType,
		 progressReport = Progress});
recover_backup_object(Key, RestoreName, BuData) ->
    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    case binary_to_list(RestoreName) of
	Name when BuData#metadataV2.backupName == Name ->
	    %% We are restoring this backup currently
	    %% Fetch saved restore progress
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    Progress = case read_progress_report(Index) of
			   {error, _} ->
			       %% error_msg("~s: No progress report: ~p~n",
			       %% 		 [DN, {error, R}]),
			       default_progress_report("RESTORE",0);
			   P -> P
		       end,
	    ProgressData =
		[{result, ?ActionResultType_SUCCESS},
		 {resultInfo, "The backup restore is complete"},
		 {progressPercentage, 100},
		 {state, ?ActionStateType_FINISHED},
		 {timeActionCompleted, CompleteTime}],
	    NewProgress = comsaI:update_progress(ProgressData, Progress),
	    swmLib:write_swm_log(DN, info, "Backup restored: "++Name),
	    BuName = BuData#metadataV2.backupName,
	    mnesia:dirty_write(
	      #brmBackup{brmBackupId = Key,
			 backupName = BuName,
			 creationTime = BuData#metadataV2.creationTime,
			 status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
			 swVersion = BuData#metadataV2.swVersion,
			 creationType = BuData#metadataV2.creationType,
			 progressReport = NewProgress}),
	    [Obj] = ets:tab2list(brmBackupLabelStore),
	    mnesia:dirty_write(
	      Obj#brmBackupLabelStore{lastRestoredBackup=BuName});
	_ ->
	    swmLib:write_swm_log(DN, info, "No action ongoing"),
	    mnesia:dirty_write(
	      #brmBackup{brmBackupId = Key ,
			 backupName = BuData#metadataV2.backupName,
			 creationTime = BuData#metadataV2.creationTime,
			 status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
			 swVersion = BuData#metadataV2.swVersion,
			 creationType = BuData#metadataV2.creationType,
			 progressReport = default_idle_pr()})
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


	    


%% @doc Returns true if the specified instance exists.

-spec existsMo([binary()], integer()) -> boolean().

existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%% @doc Returns the number of MO instances of given class
%% directly below the specified parent.

-spec countMoChildren([binary()], binary(), integer()) -> non_neg_integer().

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).


%%% @doc Gets MO attribute values. 
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.

getMoAttributes(AttrNames, DnRev, TxHandle) ->
    [getMoAttribute([AttrName|DnRev], TxHandle)||AttrName<-AttrNames].






%%% ----------------------------------------------------------
%%% @doc Comte callback 

%% getMoAttribute([<<"progressReport">>|DnRev], _) ->
%%     Key = comsaGeneric:dnrev_to_key(DnRev),
%%     Table = table(comsaGeneric:class(DnRev)),
%%     case mnesia:read(Table, Key) of
%% 	[] ->
%% 	    undefined;
%% 	[Obj] ->
%% 	    PR = case Table of
%% 		     brmBackupManager -> 
%% 			 Obj#brmBackupManager.progressReport;
%% 		     brmBackup -> 
%% 			 Obj#brmBackup.progressReport;
%% 		     %% brmExportScheduler ->
%% 		     %% 	 Obj#brmExportScheduler.progressReport;
%% 		     brmBackupScheduler -> 
%% 			 Obj#brmBackupScheduler.progressReport
				  
%% 		 end,
%% 	    comsaGeneric:format_struct(PR, ?AsyncActionProgress_types)
%%     end;

%%% Next scheduled time is stored as a datetime tuple in universal time
%%% so that it is possible to adapt to different time zone settings
getMoAttribute([<<"nextScheduledTime">>|DnRev], _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    Table = table(comsaGeneric:class(DnRev)),
    [Obj] = mnesia:read(Table, Key),
    Value = format_next_scheduled_time(Obj#brmBackupScheduler.nextScheduledTime),
			
    InternalType = proplists:get_value(nextScheduledTime, types(Table)),
    comsaEcimModelAdaptor:type(InternalType, Value);
    
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

%%% @doc Format a date time tuple stored in mnesia to the proper string value
%%% @end

format_next_scheduled_time(NextUT) when is_tuple(NextUT) ->
    NextLT = calendar:universal_time_to_local_time(NextUT),
    format_date(NextLT);
format_next_scheduled_time(undefined) ->
    undefined.


%%% @doc Comte callback 

%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).


%% @doc Sets attributes. The MO instance is specified by the
%% ReversedDn argument. 
%%
%% Attributes that are single-valued are checked for validity.
%%
%% TODO, revise the error handling when comsaGeneric:set/4 is
%% implemented.

	
setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    [check_attribute(AttrName, Value)|| {AttrName, [{_, Value}]} <- Attrs],
    [check_attribute(AttrName, Value)|| {AttrName, {_, Value}} <- Attrs],
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).


%%% @doc Comte callback 

setMoAttribute([Attribute|DnRev], {_Type, Value}=TV, _, _) ->
    check_attribute(Attribute, Value),
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TV).


check_attribute(<<"scheduledTime">>=Attribute, Value) ->
    try swmLib:parse_date(binary_to_list(Value)) of
	{_ , DateTime} ->
	    case valid_datetime(DateTime) of
		true -> 
		    true;
		false -> 
		    Msg = "Invalid date in "++binary_to_list(Attribute),
		    mnesia:abort(list_to_binary(Msg))
	    end
    catch T:E ->
	    sysInitI:info_report(
	      [{mfa,{swmLib, parse_date, [binary_to_list(Value)]}},
	       {T,E}]),
	    Msg = "The date string for "++binary_to_list(Attribute)++
		" cannot be parsed",
	    mnesia:abort(list_to_binary(Msg))
    end;

check_attribute(<<"startTime">>=Attribute, Value) ->
    try swmLib:parse_date(binary_to_list(Value))
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa,{swmLib, parse_date, [binary_to_list(Value)]}},
	       {T,E}]),
	    Msg = "The date string for "++binary_to_list(Attribute)++
		" cannot be parsed",
	    mnesia:abort(list_to_binary(Msg))
    end;

check_attribute(<<"stopTime">>=Attribute, Value) ->
    try swmLib:parse_date(binary_to_list(Value)) of
	{_, {{Year,_,_}, _}} when Year > 2037 ->
	    Msg = "No dates after 2037 are allowed.",
	    mnesia:abort(list_to_binary(Msg));
	_ ->
	    true
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa,{swmLib, parse_date, [binary_to_list(Value)]}},
	       {T,E}]),
	    Msg = "The date string for "++binary_to_list(Attribute)++
		" cannot be parsed",
	    mnesia:abort(list_to_binary(Msg))
    end;

check_attribute(_Attribute, _Value) ->
    ok.





valid_datetime({Date,Time}) ->
    case calendar:valid_date(Date) of
	true ->
	    case Time of
		{H,M,S} when H<24, M < 60, S < 60 -> 
		    %% HS37092 fix
		    if element(1,Date) > 2037 -> false;
		       true -> true
		    end;
		_ -> false
	    end;
	false ->
	    false
    end.




createMo([ClassName | ParentDnRev], _KeyAttrName, KeyValue, InitAttrs, Tid) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			[],  % TODO: the correct way later...
			types(Table)),
    setMoAttributes(InitAttrs, [KeyValue, ClassName | ParentDnRev], Tid).

%%% @doc Comte callback 

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

%%% @doc Comte callback 
deleteMo(DnRev=[_,<<"BrmSingleEvent">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(DnRev=[_,<<"BrmPeriodicEvent">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(DnRev=[_,<<"BrmCalendarBasedPeriodicEvent">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(_, _) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Helper function for comsa usage

table("BrM") -> brM;
table("BrmBackupManager") -> brmBackupManager;
table("BrmBackup") -> brmBackup;
table("BrmBackupScheduler") ->  brmBackupScheduler;
table("BrmCalendarBasedPeriodicEvent") ->  brmCalendarBasedPeriodicEvent;
table("BrmSingleEvent") -> brmSingleEvent;
table("BrmPeriodicEvent") -> brmPeriodicEvent;
table("BrmBackupHousekeeping") ->  brmBackupHousekeeping;
table("BrmRollbackAtRestore") -> brmRollbackAtRestore;
table("BrmBackupLabelStore") -> brmBackupLabelStore.

%%% @doc Helper function for comsa usage

types(brM) -> ?brM_types;
types(brmBackupManager) -> ?brmBackupManager_types;
types(brmBackup) -> ?brmBackup_types;
types(brmBackupScheduler) -> ?brmBackupScheduler_types;
types(brmCalendarBasedPeriodicEvent) -> ?brmCalendarBasedPeriodicEvent_types;
types(brmSingleEvent) -> ?brmSingleEvent_types;
types(brmPeriodicEvent) -> ?brmPeriodicEvent_types;
types(brmBackupHousekeeping) -> ?brmBackupHousekeeping_types;
types(brmRollbackAtRestore) -> ?brmRollbackAtRestore_types;
types(brmBackupLabelStore) -> ?brmBackupLabelStore_types.

%%% ----------------------------------------------------------
%%% @doc Comte callback 

prepare(_DN, User, _Tx) ->
    {ok,User}.

%%% @doc Comte callback 

commit(_DN, User, _Tx) ->
    {ok,User}.

%%% @doc Comte callback 

finish(_DN, _User, _Tx) ->
    ok.

action(Action, DnRev, Params, TransId) ->
    try do_action(Action, DnRev, Params, TransId) of
	ok -> 
	    ?INT32(?actionStarted)
    catch 
	throw:{error, Code} ->
	    %% ?INT32(Code);
	    case Code of
		?nameValidationFailure -> 
		    {error, <<"Name validation failure">>};
		?duplicateName -> 
		    {error, <<"Duplicate name">>};
		?housekeepingRequired -> 
		    {error, <<"Housekeeping required">>};
		?backupNotFound -> 
		    {error, <<"Backup not found">>};
		?functionBusy -> 
		    {error, <<"Function busy">>};
		?missingParameter -> 
		    {error, <<"Missing parameter">>};
		?softwareFault -> 
		    {error, <<"Software fault">>}
	    end;
	throw:{fail, Msg} when is_binary(Msg) -> %HT12440, part 1
	           {error, Msg};
    throw:{fail, Msg} when is_list(Msg) ->
               {error, list_to_binary(Msg)};
    throw:{lock_fail, Msg} when is_binary(Msg) ->
               {error, Msg};
    throw:{lock_fail, Msg} when is_list(Msg) ->
               {error, list_to_binary(Msg)};
	T:E ->
	    sysInitI:error_report(
	      [{mfa, {?MODULE, action, [Action, DnRev, Params, TransId]}},
	       {T,E},
	       erlang:get_stacktrace()]),
%	    ?INT32(?softwareFault)
	    {error, <<"Software fault">>}
    end.
		
%%% Actions on BrmBackupManager

do_action(<<"createBackup">>, DnRev, Params, _) -> 
    %% HT56129 fix
    Default ="MANUAL-"++comsaI:iso_time(os:timestamp(), extended),
    Name = get_value("name", Params, {9, list_to_binary(Default)}),

    case validate_name(Name) of
	ok ->
	    ok;
	duplicate_name ->
	    throw({error, ?duplicateName})
    end,

    case housekeeping() of
	ok ->
	    ok;
	max_reached ->
	    throw({error, ?housekeepingRequired});
	{cleaned, _} ->
	    ok
    end,
    ActionId = swmLib:get_new_action_id(mgr),
    swmLib:mo_lock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID,
                                  ?CREATE_BACKUP_ACTION_CAPABLE_INFO),
    gen_server:cast(swmBackup, {createBackup, Name, DnRev, ActionId});

do_action(<<"deleteBackup">>, _, Params, _) -> 
    Name = get_value("name", Params),
    case get_backup_by_name(binary_to_list(Name)) of
	[] ->
	    throw({error, ?backupNotFound});
	_ ->
	    ActionId = swmLib:get_new_action_id(mgr),
    swmLib:mo_lock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID,
                                  ?DELETE_BACKUP_ACTION_CAPABLE_INFO),
	    gen_server:cast(swmBackup,  {deleteBackup, Name, ActionId})
    end;
do_action(<<"importBackup">>, DnRev, Params, _) -> 
    UBin = get_value("uri", Params),
    PBin = get_value("password", Params, ?STRING(<<"">>)),
    Passwd = binary_to_list(PBin),
    Uri = binary_to_list(UBin),
    ActionId = swmLib:get_new_action_id(mgr),
    swmLib:mo_lock_action_capable(?IMPORT_BACKUP_ACTION_CAPABLE_ID,
                                  ?IMPORT_BACKUP_ACTION_CAPABLE_INFO),
    gen_server:cast(swmBackup, {importBackup, DnRev, Uri, Passwd, ActionId});
    
%%% Actions on BrmBackup

do_action(<<"export">>, DnRev, Params, _) ->
    UBin = get_value("uri", Params),
    PBin = get_value("password", Params, ?STRING(<<"">>)),
    Uri = binary_to_list(UBin),
    Passwd = binary_to_list(PBin),
    Key = comsaGeneric:dnrev_to_key(DnRev), 
    ActionId = swmLib:get_new_action_id(Key),
    swmLib:mo_lock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID,
                                  ?EXPORT_BACKUP_ACTION_CAPABLE_INFO),
    gen_server:cast(swmBackup, {export, Key, Passwd, Uri, ActionId});
do_action(<<"restore">>, DnRev, _, _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev), 
    %% HT12440 fix, part 2
    case swmServer:check_up_states_for_restore() of
	ok ->
	    ok;
	{error, {Up, State}} ->
	    Fmt = "Operation refused because UpgradePackage=~s is in state ~s.",
	    Msg = lists:flatten(io_lib:format(Fmt,[Up, State])),
	    throw({fail, list_to_binary(Msg)})
    end,
    %% Fix ends here
    ActionId = swmLib:get_new_action_id(Key),
    swmLib:mo_lock_action_capable(?RESTORE_BACKUP_ACTION_CAPABLE_ID,
                                  ?RESTORE_BACKUP_ACTION_CAPABLE_INFO),
    gen_server:cast(swmBackup, {restore, Key, ActionId});    
do_action(<<"cancelCurrentAction">>, [_, <<"BrmBackup">>|_], _, _) ->
    swmBackup!cancel,
    ok;
do_action(<<"cancelCurrentAction">>, [_, <<"BrmBackupManager">>|_], _, _) ->
    swmBackup!cancel_mgr,
    ok.



get_value(Key, Params) ->
    case proplists:get_value(list_to_binary(Key), Params) of
	{_, Value} ->
	    Value;
	_ ->
	    throw({error, ?missingParameter})
    end.
get_value(Key, Params, Default) ->
    case proplists:get_value(list_to_binary(Key), Params, Default) of
	{_, Value} ->
	    Value;
	undefined ->
	    element(2, Default);
	_ ->
	    throw({error, ?missingParameter})
    end.

validate_name(NameB) when is_binary(NameB)->
    Name = binary_to_list(NameB),
    validate_name(Name);
validate_name(Name) when is_list(Name)->
    Fun = fun() -> do_validate_name(Name) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{atomic, duplicate_name} ->
	    duplicate_name
    end.

do_validate_name(Name) ->
    Key = mnesia:first(brmBackup),
    do_validate_name(Name, Key).

do_validate_name(_, '$end_of_table') ->
    ok;
do_validate_name(Name, Key) ->
    [BrmBackup] = mnesia:read({brmBackup, Key}),
    case BrmBackup#brmBackup.backupName of
	Name ->
	    duplicate_name;
	_ ->
	    Next = mnesia:next(brmBackup, Key),
	    do_validate_name(Name, Next)
    end.    

housekeeping() ->
    {atomic, Result} = mnesia:transaction(fun() -> do_housekeeping() end),
    case Result of
	{cleaned, Indices} ->
	    [case remove_backup_dir(Index) of
		 ok ->
		     Msg = "Backup "++BackupName++" removed automatically "
			 "due to housekeeping.",
		     swmLib:write_swm_log("BrmBackupManager", info, Msg);
		 {error, backup_busy} ->
		     ok
	     end ||{Index, BackupName}<-Indices],
	    {cleaned, Indices};
	Result ->
	    Result
    end.

do_housekeeping() ->
    [HK] = mnesia:read({brmBackupHousekeeping,{"1","1","1","1","1"}}),
    Type = ?BrmBackupCreationType_MANUAL,
    WP = mnesia:table_info(brmBackup, wild_pattern),
    Pattern = WP#brmBackup{creationType = Type},
    Backups = mnesia:match_object(Pattern),
    
    case {HK#brmBackupHousekeeping.autoDelete, length(Backups)} of
	{?BrmManualBackupAutoDelete_DISABLED, N} 
	  when N>=HK#brmBackupHousekeeping.maxStoredManualBackups ->
	    max_reached;
	{?BrmManualBackupAutoDelete_ENABLED, N}
	  when N>=HK#brmBackupHousekeeping.maxStoredManualBackups ->
	    Remove = N-HK#brmBackupHousekeeping.maxStoredManualBackups+1,
	    Sorted = lists:keysort(#brmBackup.creationTime, Backups),
	    Indices = housekeeping_autodelete(Remove, Sorted),
	    {cleaned, Indices};
	_ ->
	    ok
    end.

housekeeping_autodelete(0, _) ->
    [];
housekeeping_autodelete(_, []) ->
    [];
housekeeping_autodelete(N, [Backup|Sorted]) ->
    Key = Backup#brmBackup.brmBackupId,
    BackupName = Backup#brmBackup.backupName,
    ok = mnesia:delete({brmBackup, Key}),
    [{lists:last(tuple_to_list(Key)), BackupName}|
     housekeeping_autodelete(N-1, Sorted)].

			       






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
       gen_server:cast(swmBackup, {restore, Key, Id}),
        %% Ignore waiting periods
        swmBackup!force,
        ok;
       {aborted, Reason} ->
       {error, Reason}
   end.

%%% ----------------------------------------------------------
%%% @doc Outside requiest to delete a system created backup
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
%%% @doc Find the brmBackup object for backup with name Name
%%% Works both in transactional context and outside
%%% @end
%%% ----------------------------------------------------------
    
get_backup_by_name(BuName) ->
    case mnesia:is_transaction() of
	true ->
	    do_get_backup_by_name(BuName);
	false ->
	    mnesia:sync_dirty(fun() -> do_get_backup_by_name(BuName) end)
    end.
				       
do_get_backup_by_name(BuName) ->
    WP = mnesia:table_info(brmBackup, wild_pattern),
    mnesia:match_object(WP#brmBackup{backupName=BuName}).

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
		DN = "BrmBackupManager=1",
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

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-record(state, {actionId = 1,  %% Not used
		restore=idle, %% idle | busy
		failsafe=free}). %% free | lock | pending

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
    %% HT75846 remove restore dir
    RestoreDir = filename:join(sysEnv:home_dir(), "restore"),
    cmd(["rm -rf ",RestoreDir]),
    %% Fix ends here
    {noreply, #state{}}.

%%% @doc gen_server callback 

handle_call(_, _, State) when State#state.restore == busy ->
    {reply, failure, State};

handle_call({createBackup, Name, DnRev, Id}, _From, State) ->
    set_default_progress_report(mgr, "CREATE", Id),
    action_handler(fun() -> handle_create_backup(Name, DnRev) end, mgr),
    [Obj] = mnesia:dirty_read({brmBackupManager, {"1","1","1","1"}}),
    Progress = Obj#brmBackupManager.progressReport,
    Result = 
	case Progress#'AsyncActionProgress'.result of
	    ?ActionResultType_SUCCESS -> 
		MoRef = Progress#'AsyncActionProgress'.resultInfo,
		{ok, MoRef};
	    ?ActionResultType_FAILURE -> failure;
	    ?ActionResultType_NOT_AVAILABLE -> not_avaialable
	end,
    ok = swmLib:unlock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID),
    {reply, Result, State};

handle_call({export, BuKey, Passwd, Uri, Id}, _, State) ->
    set_default_progress_report(BuKey, "EXPORT", Id),
    action_handler(fun() -> handle_export_backup(BuKey, Passwd, Uri) end,
		   BuKey),
    {reply, Id, State};

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
    set_default_progress_report(mgr, "CREATE", Id),
    action_handler(fun() -> handle_create_backup(Name, DnRev) end, mgr),
    ok = swmLib:unlock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID),
    {noreply, State};

handle_cast({deleteBackup, Name, Id}, State) ->
    set_default_progress_report(mgr, "DELETE", Id),
    action_handler(fun() -> handle_delete_backup(Name) end, mgr),
    ok = swmLib:unlock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID),
    {noreply, State};

handle_cast({importBackup, DnRev, Uri, Passwd, Id}, State) ->
    set_default_progress_report(mgr, "IMPORT", Id),
    action_handler(fun() -> handle_import_backup(DnRev, Uri, Passwd) end,
		   mgr),
    ok = swmLib:unlock_action_capable(?IMPORT_BACKUP_ACTION_CAPABLE_ID),
    {noreply, State};
    
%%% Actions on BrmBackup
handle_cast({export, BuKey, Passwd, Uri, Id}, State) ->
    set_default_progress_report(BuKey, "EXPORT", Id),
    action_handler(fun() -> handle_export_backup(BuKey, Passwd, Uri) end,
		   BuKey),
    ok = swmLib:unlock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID),
    {noreply, State};

handle_cast({restore, BuKey, Id}, State) ->
    set_default_progress_report(BuKey, "RESTORE", Id),
    action_handler(fun() -> handle_restore_backup(BuKey) end, BuKey),
    receive
	stop_receiving_requests ->
        ok = swmLib:unlock_action_capable(?RESTORE_BACKUP_ACTION_CAPABLE_ID),
	    {noreply, State#state{restore=busy}}
    after 0 ->
        ok = swmLib:unlock_action_capable(?RESTORE_BACKUP_ACTION_CAPABLE_ID),
	    {noreply, State}
    end;

handle_cast(restore_ai_bu, State) ->
    AiBuFile = filename:join([sysEnv:rcs_dir(), "networkloader", "backup.zip"]),
    case filelib:is_file(AiBuFile) of
	true ->
	    %% BU from AutoIntegration; restore from this BU
	    info_msg("Backup from AutoIntegration; restoring ~n"),
	    swmLib:write_swm_log("BrmBackupManager", info, 
				 "Restore backup supplied from autointegration"),
	    try handle_restore_ai_bu(AiBuFile)
	    catch
		Type:Error ->
		    sysInitI:error_report([{?MODULE, handle_store_ai_bu},
					       {Type, Error},
					       erlang:get_stacktrace()]),
		    swmLib:write_swm_log("BrmBackupManager", info,
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

handle_cast(Request, State) ->
    error_msg("Unknown cast: ~p~n",[Request]),
    {noreply, State}.

%%% @doc gen_server callback 

handle_info({mnesia_table_event, Event}, State) ->
    handle_mnesia_table_event(Event),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

%%% @doc gen_server callback 

code_change(OldVsn, State, Extra) ->
    info_msg("code_change(~p, ~p, ~p)~n",[OldVsn, State, Extra]),
    {ok, State}.

%%% @doc gen_server callback 

terminate(_Reason, _State) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @ Create a backup
%%% ===Arguments===
%%% Name - The name of the backup
%%% DnRev - The reversed distinguished name to the BrmBackupManager object
%%% ActionId - The sequential action id of this action

handle_create_backup(Name, DnRev)->
    swmLib:write_swm_log("BrmBackupManager", info, "Creating backup "++Name),
  
    case validate_name(Name) of
	ok -> 
	    ok;
	duplicate_name ->
	    throw({fail, "Duplicate name"})
    end,

    case housekeeping() of
	ok -> 
	    ok;
	max_reached ->
	    throw({fail, "Housekeeping required"});
	{cleaned, _} ->
	    ok
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
    after Timeout -> ok
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
			  {timeActionCompleted, CompleteTime}]).


-spec create_backup_common(
	Name::binary(), 
	BrmbackupManagerKey::{string(), string(), string(), string()},
	Type::manual|scheduled|system,
	Progress::key_type()) ->
				  string().

create_backup_common(Name, BrmBackupManagerKey, Type, Progress) ->
    update_progress(Progress, [{additionalInfo, "Name: "++binary_to_list(Name)}]),
    Index = get_next_index(),

    %% HU30730
    %% Make sure all post processing is done before allowing removal of backups
    %% So let the caller make the unlock call in normal cases

    try do_create_backup_common(Index,Name,BrmBackupManagerKey,Type,Progress)
    catch T:E ->
	    %% A lock is in place here already, avoid setting another one
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
    BuMeta = filename:join(BuDir, "metadata"),
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
    Metadata = #metadataV2{backupName = binary_to_list(Name),
			   creationTime = StartTime,
			   swVersion = SwVersion,
			   creationType = CreationType},
    {ok, Fd} = file:open(BuMeta, [write]),
    ok = io:format(Fd, "~p.~n",[Metadata]),
    ok = file:close(Fd),

    [cmd(["cd ",sysEnv:home_dir()," ; tar cfz ",BuDir, "/", Dir,".tgz ",
	  "--exclude='releases/*/comte/comea/run/*' ", Dir])||
	Dir <- ["bin", "releases"]],
    
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

make_dn([Key, Value]) ->
    Key++"="++Value;
make_dn([Key, Value|Tail]) ->
    Key++"="++Value++","++make_dn(Tail).

make_nms_bu_info(BuDir, Name, StartTime, BrmBackupManagerKey, BrmBackup) ->
    {atomic, {[BrmBuMgr], [BrM], [ME], _}} = 
	mnesia:transaction(
	  fun() -> 
		  {mnesia:read({brmBackupManager, BrmBackupManagerKey}),
		   mnesia:read({brM, {"1","1","1"}}),
		   mnesia:read({managedElement, {"1"}}),
		   undefined}
	  end),
    #managedElement{managedElementType = MeType,
		    siteLocation = SiteLocation,
		    dnPrefix = DnPrefix,
		    release = Release,
		    networkManagedElementId = NMEI} = ME,
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
    %% Rely on user input only
    UserLabel = case BrM#brM.exportPackageLabelPrefix of
		    undefined -> "";
		    Label -> Label
		end,
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
%%% @doc Find the next free index for creating a backup dir, and model object
%%%

get_next_index() ->
    BuDir = swmLib:backup_dir(),
    case file:list_dir(BuDir) of
	{ok, Dirs} ->
	    case [D||D<-Dirs, D/=".", D/=".."] of
		[] ->
		    "1";
		IndexDirs ->
		    Max = lists:max([list_to_integer(IxD)||IxD<-IndexDirs]),
		    get_next_index(Max+1)
	    end;
	{error, enoent} ->
	    ok = file:make_dir(BuDir),
	    "1";
	{error, Reason} ->
	    erlang:error(Reason, [])
    end.

get_next_index(IndexN) ->
    Index = integer_to_list(IndexN),
    try swmLib:lock_backup(Index) of
	ok ->
	    Index
    catch error:set_lock_failed ->
	    get_next_index(IndexN+1)
    end.


%%% ----------------------------------------------------------
%%% @doc Delete a backup
%%%

handle_delete_backup(Name) ->
    receive
	cancel_mgr -> throw(cancelled)
    after 5000 -> ok
    end,
    case delete_backup_internal(Name, normal) of
	{ok, Indices} ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    case Indices of
		[] ->
		    update_progress(mgr, [{result, ?ActionResultType_FAILURE},
					  {resultInfo, "Backup not found"},
					  {progressPercentage, 0},
					  {state, ?ActionStateType_FINISHED},
					  {timeActionCompleted, CompleteTime}]);
		_ ->
		    update_progress(mgr, [{result, ?ActionResultType_SUCCESS},
					  {resultInfo, "Backup removed"},
					  {progressPercentage, 100},
					  {state, ?ActionStateType_FINISHED},
					  {timeActionCompleted, CompleteTime}])
	    end;
	{error, system_created} ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    Msg = "Backup "++binary_to_list(Name)++" is system created",
	    update_progress(mgr, [{result, ?ActionResultType_FAILURE},
				  {resultInfo, Msg},
				  {progressPercentage, 0},
				  {state, ?ActionStateType_FINISHED},
				  {timeActionCompleted, CompleteTime}])
    end.


-spec delete_backup_internal(Name::string()|binary(), 
			     Operation::normal|forced) ->
				    {ok, [string()]} | {error, system_created}.


delete_backup_internal(NameB, Operation) when is_binary(NameB) ->
    Name = binary_to_list(NameB),
    delete_backup_internal(Name, Operation);
delete_backup_internal(Name, Operation) ->
    Msg = "Deleting backup "++Name,
    swmLib:write_swm_log("BrmBackupManager", info, Msg),
    %% While this code can handle multiple instance with the same 
    %% backup name, according to ECIM that should not happen
    Fun = fun() ->
		  Matched = get_backup_by_name(Name),
		  [begin 
		       FullIndex = tuple_to_list(Bu#brmBackup.brmBackupId),
		       mnesia:delete_object(Bu),
		       lists:last(FullIndex)
		   end||Bu<-Matched,
			case Bu#brmBackup.creationType of
			    ?BrmBackupCreationType_SYSTEM_CREATED ->
				case Operation of
				    forced ->
					true;
				    normal -> % HU39925
					mnesia:abort(system_created)
				end;
			    _ -> 
				true
			end]
	  end,

    case mnesia:transaction(Fun) of
	{atomic, Indices} -> 
	    [proc_lib:spawn(fun() -> remove_backup_dir(Index) end)||
		Index<-Indices],
	    {ok, Indices};
	{aborted, system_created} ->
	    {error, system_created}
    end.

-spec remove_backup_dir(Index::string()) -> ok | {error, backup_busy}.

remove_backup_dir(Index) ->
    try swmLib:lock_backup(Index) of
	ok ->
	    try do_remove_backup_dir(Index) 
	    after 
		swmLib:unlock_backup(Index)
	    end
    catch error:set_lock_failed ->
	    {error, backup_busy}
    end.

do_remove_backup_dir(Index) ->
    BuDir = swmLib:backup_dir(Index),
    cmd("rm -rf "++BuDir),
    ok.
	

%%% ----------------------------------------------------------
%%% @doc Exports a backup
%%%

handle_export_backup(BuKey, Passwd, Uri) ->
    Index = element(5, BuKey),
    BuDir = swmLib:backup_dir(Index),
    ETime = os:timestamp(),
    Info = "Preparing backup "++Index++" for transport",
    update_progress(BuKey, [{additionalInfoClear, Info}]),

    TmpPath = compress_file(BuDir, ETime),

    %% Upload file
    case Passwd of
	"" -> 
	    update_progress(
	      BuKey, [{additionalInfo, "Starting transport without password"}]);
	_ ->
	    update_progress(BuKey, [{additionalInfo, "Starting transport"}])
    end,
    {ok, {sftp, User, Host, Port, RemoteDir, _Query}} = parse_uri(Uri, BuKey),
    %% Use basic format in order not to introduce any unwanted characters
    %% in the timestamp part of the filename
    BasicET = comsaI:iso_time(ETime, basic),
    RemotePath = filename:join(RemoteDir, make_remote_name(BuKey, BasicET)),
    {ok, Pid, _CRef} = start_channel(Host, Port, User, Passwd, BuKey),
    TotalSize = filelib:file_size(TmpPath),
    case file:open(TmpPath, [read, raw, binary]) of
	{ok, Fd} ->
	    {ok, Handle} = open_remote_file(BuKey, Pid, RemotePath, [write]),
	    Params = 
		#transfer{fd = Fd, pid = Pid, handle = Handle, rp_key = BuKey,
			  total_size = TotalSize, path = TmpPath},
	    upload_file(Params, 0, file:read(Fd, 65536)),
	    file:close(Fd),
	    file:delete(TmpPath);
	{error, Reason} ->
	    file:delete(TmpPath),
	    Info = file:format_error(Reason),
	    update_progress(BuKey, [{additionalInfo, Info}]),
	    erlang:error(Reason)	    
    end,
    Fun = 
	fun() ->
		[BrmBackup] = mnesia:read({brmBackup, BuKey}),
		Name = BrmBackup#brmBackup.backupName,
		LsKey = list_to_tuple(
			  lists:reverse(
			    ["1"|tl(lists:reverse(tuple_to_list(BuKey)))])),
		[LS] = mnesia:read({brmBackupLabelStore, LsKey}),
		mnesia:write(LS#brmBackupLabelStore{lastExportedBackup=Name}),
		Name
	end,
    {atomic, Name} = mnesia:transaction(Fun),
    cleanup(),
    swmLib:write_swm_log("BrmBackup="++Index, info, 
			 "Backup exported: "++Name),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(BuKey, [{additionalInfo, "Transfer complete"},
			    {result, ?ActionResultType_SUCCESS},
			    {resultInfo, RemotePath},
			    {progressPercentage, 100},
			    {state, ?ActionStateType_FINISHED},
			    {timeActionCompleted, CompleteTime}]),
    ok.

-spec compress_file(BuDir::string(), 
		    ETime::{integer(), integer(), integer()}) ->
			   OutPath::string().

compress_file(BuDir, ETime) ->
    %% Cleanup in case of previous sw crash
    Exp_tgz = "export.tgz",
    Exp_bup = "export.bup",
    Exp_zip = "export.zip",
    cleanup_file(filename:join(BuDir, Exp_tgz)), 
    cleanup_file(filename:join(BuDir, Exp_bup)), 
    %% Create bu package
    TmpPath = filename:join(swmLib:swm_dir(), Exp_zip),
    {ok, Files} = file:list_dir(BuDir),
    cmd(["cd ", BuDir, " ; tar cfz ", BuDir, "/"++Exp_tgz]++
	    [[" ", File]||File<-Files--["backupinfo.xml"]]),
    {ok, Bin} = file:read_file(filename:join(BuDir, Exp_tgz)),
    Crypto = crypto:block_encrypt(aes_cfb128,
				  <<"1234567890ABCDEF">>, 
				  <<"1234567890ABCDEF">>, Bin),
    ok = file:write_file(filename:join(BuDir, Exp_bup), Crypto),
    file:delete(filename:join(BuDir, Exp_tgz)),

    %% Adjust export time
    MetaDataPath = filename:join(BuDir, "backupinfo.xml"),
    {ok, BI} = file:read_file(MetaDataPath),
    ExportTime = comsaI:iso_time(ETime, extended),
    NewBI = re:replace(BI, "<exportTime>.*</exportTime>", 
		       "<exportTime>"++ExportTime++"</exportTime>", 
		       [{return, binary}]),
    ok = file:write_file(MetaDataPath, NewBI),

    cmd("cd "++BuDir++" ; zip "++TmpPath++" "++Exp_bup++" backupinfo.xml"),
    file:delete(filename:join(BuDir, Exp_bup)),
    TmpPath.


cleanup_file(File) ->
   case filelib:is_file(File) of
       true ->
	   Result = file:delete(File),
	   info_msg(
	     "Cleaned up rest from interupted backup export: ~p ~p~n", 
	     [File, Result]);
       false ->
	   ok
   end. 
  
%%% ----------------------------------------------------------
%%% @doc Generate the external file name from configuration
%%% In the action uri parameter the string "null" is interpreted as no uri
%%% (at least until COM support optional parameters)

make_remote_name(BuKey, ExportTime) ->
    [Brm] = mnesia:dirty_read({brM, {"1","1","1"}}),
    [BrmBackup] = mnesia:dirty_read({brmBackup, BuKey}),
    ExportUserLabel = case Brm#brM.exportPackageLabelPrefix of
			  undefined -> "_";
			  EUL -> EUL++"_"
		      end,
					   
    Name = BrmBackup#brmBackup.backupName,
    %% Time = lists:append(string:tokens(ExportTime, "-:.")),

    %% Fix for HS77233
    %% File names according to ECIM BrM UCD
    MeData = comsaI:get_managed_element_data(),
    MeId = case proplists:get_value(networkManagedElementId, MeData) of
	       undefined -> "1";
	       Id -> Id
	   end,
    MeType = case proplists:get_value(managedElementType, MeData) of
		 undefined -> "";
		 ET -> ET
	     end,
		 
    noslash(ExportUserLabel++Name)++"_"++MeId++"_"++MeType++"_"++
	ExportTime++".zip".

noslash(Str) ->
    [case X of
	 $/ -> $_;
	 _ -> X
     end||X<-Str].
	      
%%% ----------------------------------------------------------
%%% @doc Open an sftp channel

start_channel(Host, Port, User, Passwd) ->
    start_channel(Host, Port, User, Passwd, undefined).
    
start_channel(Host, Port, User, Passwd, BuKey) ->
    case sysSftp:start_channel_with_alt(Host, Port, User, Passwd) of
	{ok, SP, C} -> 
	    info_msg("~p~n",[ssh:connection_info(
			       C, [client_version, server_version, peer])]),
	    put(channelPid, SP),
	    put(connectionRef, C),
	    {ok, SP, C};
	{error, E1} ->
	    Info1 =
		case E1 of
		    etimedout -> 
			"Cannot establish a connection to remote server";
		    String when is_list(String) ->
			String;
		    _ ->
			lists:flatten(io_lib:format("~p",[E1]))
		end,
	    case BuKey of
		undefined ->
		    update_progress(mgr, [{resultInfo, Info1}]);
		_ ->
		    update_progress(BuKey, [{resultInfo, Info1}])
	    end,
	    throw(E1)
    end.


%%% ----------------------------------------------------------
%%% @doc Upload the backup file and update progress at the same time

open_remote_file(BuKey, ChannelPid, RemotePath, Opts) ->
    %% case verify_path(ChannelPid, filename:dirname(RemotePath)) of
    %% 	ok -> ok;
    %% 	{error, Reason, Dir} ->
    %% 	    Info= lists:flatten(io_lib:format("~p for ~s",[Reason, Dir])),
    %% 	    update_progress(BuKey, [{resultInfo, Info}]),
    %% 	    erlang:error(Reason)
    %% end,

    case ssh_sftp:list_dir(ChannelPid, filename:dirname(RemotePath)) of
	{ok, _} -> ok;
	{error, Reason} ->
	    Info = lists:flatten(
		     io_lib:format("Problem reading directory ~p: ~p",
				   [filename:dirname(RemotePath),
				    Reason])),
	    update_progress(BuKey, [{resultInfo, Info}]),
	    erlang:error(Reason)
    end,

    case ssh_sftp:open(ChannelPid, RemotePath, Opts) of
	{ok, Handle} -> 
	    put(handle, Handle),
	    {ok, Handle};
	{error, Reason2} ->
	    Info2 = case Reason2 of
	    	       no_such_file -> 
	    		   "No such file or directory: "++RemotePath;
	    	       _ ->
	    		   lists:flatten(io_lib:format("~p",[Reason2]))
	    	   end,
	    update_progress(BuKey, [{resultInfo, Info2}]),
	    erlang:error(Reason2)
    end.

%% verify_path(ChannelPid, RemotePath="/") ->
%%     case ssh_sftp:list_dir(ChannelPid, RemotePath) of
%% 	{ok, _} ->
%% 	    ok;
%% 	{error,Reason} ->
%% 	    {error, Reason, RemotePath}
%%     end;
%% verify_path(ChannelPid, RemotePath) ->
%%     case verify_path(ChannelPid, filename:dirname(RemotePath)) of
%% 	ok ->
%% 	    case ssh_sftp:list_dir(ChannelPid, RemotePath) of
%% 		{ok, _} ->
%% 		    ok;
%% 		{error,Reason} ->
%% 		    {error, Reason, RemotePath}
%% 	    end;
%% 	{error, Reason, RemotePath} ->
%% 	    {error, Reason, RemotePath}
%%     end.
		
%% c
%% fault_search_path(_, "/") ->
%%     "/";
%% fault_search_path(ChannelPid, Path) ->
%%     Dir = filename:dirname(Path),
%%     case ssh_sftp:list_dir(ChannelPid, Dir) of
%% 	{ok, _} ->
%% 	    Path;
%% 	{error, Reason} ->
%% 	    info_msg("ssh_sftp:listdir(~p, ~p) = {error, ~p}~n",
%% 		     [ChannelPid, Dir, Reason]),
%% 	    fault_search_path(ChannelPid, Dir)
%%     end.

%%% ----------------------------------------------------------
%%% @doc Upload the backup file and update progress at the same time

upload_file(Params, AccuSize, {ok, Data}) ->
    #transfer{fd = Fd, 
	      pid = Pid, 
	      handle = Handle, 
	      rp_key = BuKey, 
	      total_size = TotalSize} = Params,
    garbage_collect(),
    NewAccuSize = AccuSize + size(Data),
    ok = ssh_sftp:write(Pid, Handle, Data, 10000),
    update_progress(BuKey, [{progressPercentage, AccuSize*100 div TotalSize}]),

    receive
	cancel ->
	    %% Local cleanup, ssh side will be handled in cleanup
	    file:close(Fd),
	    file:delete(Params#transfer.path),
	    throw(cancelled)
    after 0 ->
	    ok
    end,

    upload_file(Params, NewAccuSize, file:read(Fd, 65536));
upload_file(_, _, eof) ->
    ok;
upload_file(Params, _, {error, Reason}) ->
    Info = file:format_error(Reason),
    update_progress(Params#transfer.rp_key, [{additionalInfo, Info}]),
    erlang:error(Reason).

%%% ----------------------------------------------------------
%%% @doc Import a backup
%%%

handle_import_backup(DnRev, Uri, Passwd) ->

    case housekeeping() of
	ok ->
	    ok;
	max_reached ->
	    update_progress(mgr, 
			    [{resultInfo,"Maximum number of backups reached"}]),
	    throw(max_reached);
	{cleaned, _} ->
	    ok
    end,
    swmLib:write_swm_log("BrmBackupManager", info, "Importing backup"),

    %% Download file
    update_progress(mgr, [{additionalInfoClear, "Starting transport"}]),
    update_progress(mgr, [{additionalInfo, "Locating "++Uri}]),
    {ok, {sftp, User, Host, Port, RemoteDir, _Query}} = parse_uri(Uri),
    {ok, Pid, _CRef} = start_channel(Host, Port, User, Passwd),
    TotalSize = ssh_file_size(Pid, RemoteDir),
    TmpPath = filename:join(swmLib:swm_dir(), "import.zip"),
    ok = filelib:ensure_dir(TmpPath),
    case file:open(TmpPath, [write, raw, binary]) of
	{ok, Fd} ->
	    {ok, Handle} = open_remote_file(mgr, Pid, RemoteDir, [read,binary]),
	    Params = 
		#transfer{fd = Fd, pid = Pid, handle = Handle, rp_key = mgr,
			  total_size = TotalSize, path=TmpPath},
	    download_file(Params, 0, ssh_sftp:read(Pid, Handle, 65536)),
 	    ssh_sftp:close(Pid, Handle),
	    file:close(Fd);
	{error, Reason} ->
	    Info = file:format_error(Reason),
	    update_progress(mgr, [{additionalInfo, Info}]),
	    erlang:error(Reason)	    
    end,

    %% Create bu package
    update_progress(mgr, [{additionalInfo, "Transport complete. Unpacking."}]),
    Index = get_next_index(),
    Fun = fun() -> do_handle_import_backup(DnRev, TmpPath, Index) end,
    case mnesia:transaction(Fun) of
	{atomic, {MoRef, Name}} -> 
	    swmLib:unlock_backup(Index),
	    set_creation_type_manual(Index),
	    swmLib:write_swm_log("BrmBackup="++Index, info, 
				 "Backup imported: "++Name),
	    file:delete(TmpPath),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    update_progress(mgr, [{additionalInfo, "Import complete"},
				  {result, ?ActionResultType_SUCCESS},
				  {resultInfo, MoRef},
				  {progressPercentage, 100},
				  {state, ?ActionStateType_FINISHED},
				  {timeActionCompleted, CompleteTime}]),
	    ok;
	{aborted, {duplicate_name, Name}} ->
	    file:delete(TmpPath),
	    swmLib:unlock_backup(Index),
	    update_progress(mgr, [{resultInfo, "Duplicate name "++Name}]),
	    swmLib:write_swm_log("BrmBackupManager", error, 
				 "Duplicate name "++Name),
	    remove_backup_dir(Index),
	    throw({duplicate_name, Name});
	{aborted, {inconsistent_software, SwVersions}} ->
	    file:delete(TmpPath),
	    swmLib:unlock_backup(Index),
	    [begin
		 Msg = "Required software: "++format_product_data(Sw),
		 swmLib:write_swm_log("BrmBackupManager", error, Msg),
		 update_progress(mgr,[{additionalInfo, Msg}])
	     end||Sw<-SwVersions],
	    update_progress(mgr, [{resultInfo, "The applicable software is not installed"}]),
	    remove_backup_dir(Index),
	    throw({inconsistent_software, SwVersions});
	{aborted, MnesiaReason} ->
	    file:delete(TmpPath),
	    do_remove_backup_dir(Index),
	    swmLib:unlock_backup(Index),
	    erlang:error(MnesiaReason)
    end.

format_product_data(Pd) ->
    Pd#'ProductData'.productName++" "++
    Pd#'ProductData'.productNumber++" "++
    Pd#'ProductData'.productRevision.

do_handle_import_backup(DnRev, TmpPath, Index) ->
    BuDir = swmLib:backup_dir(Index),
    cmd(["mkdir -p ", BuDir]),
    cmd(["cd ", BuDir, " ; unzip ", TmpPath]),
    {ok, Bin} = file:read_file(filename:join(BuDir, "export.bup")),
    Cleartext = crypto:block_decrypt(aes_cfb128,
				     <<"1234567890ABCDEF">>,
				     <<"1234567890ABCDEF">>, Bin),
    ok = file:write_file(filename:join(BuDir, "export.tgz"), Cleartext),
    cmd(["cd ", BuDir, " ; tar xfz export.tgz"]),
    file:delete(filename:join(BuDir, "export.bup")),
    file:delete(filename:join(BuDir, "export.tgz")),

    swmLib:sync(),
    Metadata = get_metadata(Index),

    #metadataV2{backupName = Name,
		creationTime = CreationTime,
		swVersion = SwVersion} = Metadata,
 
   case validate_name(Name) of
	ok ->
	    ok;
	duplicate_name ->
	    mnesia:abort({duplicate_name, Name})
    end,

    case swmModel:get_matching_up(SwVersion) of
	nomatch ->
	    mnesia:abort({inconsistent_software, SwVersion});
	_ ->
	    ok
    end,

    %% Add backup in BrM
    BrmBackupManagerKey = comsaGeneric:dnrev_to_key(DnRev), 
    BrmBackupKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++[Index]),

    BrmBackup = #brmBackup{brmBackupId = BrmBackupKey,
			   backupName = Name,
			   creationTime = CreationTime,
			   status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
			   progressReport = default_idle_pr(),
			   swVersion = SwVersion,
			   %% Imported backups are always considered manual
			   creationType = ?BrmBackupCreationType_MANUAL},
    mnesia:write(BrmBackup),

    LsKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++["1"]),
    [LS] = mnesia:read({brmBackupLabelStore, LsKey}),
    mnesia:write(LS#brmBackupLabelStore{lastImportedBackup=Name}),
    {_, _, BrM, BMgr} = BrmBackupManagerKey,
    {make_dn(["BrM",BrM,
	      "BrmBackupManager",BMgr,
	      "BrmBackup",Index]), Name}.

%%% ----------------------------------------------------------
%%% @doc Read the size of the remote file
%%% @end


ssh_file_size(Pid, Path) ->
    case ssh_sftp:read_file_info(Pid, Path, 10000) of
	{error, Reason} ->
	    throw({read_file_info, {Reason, Path}});
	{ok, FIO} ->
	    case FIO#file_info.type of
		regular ->
		    FIO#file_info.size;
		_ ->
		    0
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Download the backup file and update progress at the same time

download_file(Params, AccuSize, {ok, Data}) ->
    #transfer{fd = Fd, 
	      pid = Pid, 
	      handle = Handle, 
	      rp_key = RpKey, 
	      total_size = TotalSize} = Params,
    receive
	cancel_mgr -> 
	    file:close(Fd),
	    file:delete(Params#transfer.path),
	    throw(cancelled)
    after 0 -> ok
    end,
    garbage_collect(),
    NewAccuSize = AccuSize + size(Data),
    ok = file:write(Fd, Data),
    update_progress(RpKey, [{progressPercentage, AccuSize*100 div TotalSize}]),

    download_file(Params, NewAccuSize, ssh_sftp:read(Pid, Handle, 65536));
download_file(_, _, eof) ->
    ok;
download_file(Params, _, {error, Reason}) ->
    Info = case Reason of
	       no_such_file -> "no such file or directory on remote side";
	       _ -> lists:flatten(io_lib:format("~p",[Reason]))
	   end,
    update_progress(Params#transfer.rp_key, [{additionalInfo, Info}]),
    erlang:error(Reason).

%%% ----------------------------------------------------------
%%% @doc Restore an AutoIntegration backup

handle_restore_ai_bu(AiBuFile) ->

    %% Run import routine without progress updates
    Index = get_next_index(),
    DnRev = [<<"1">>,<<"BrmBackupManager">>,<<"1">>,<<"BrM">>,<<"1">>,
	     <<"SystemFunction">>, <<"1">>, <<"ManagedElement">>],
    Fun = fun() -> do_handle_import_backup(DnRev, AiBuFile, Index) end,
    case mnesia:transaction(Fun) of
	{atomic, {_, Name}} -> 
	    do_set_creation_type(Index, ?BrmBackupCreationType_MANUAL),
	    swmLib:unlock_backup(Index),
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
    ok = handle_restore_backup(BuKey),
    file:delete(AiBuFile).
    

%%% ----------------------------------------------------------
%%% @doc Restore a backup

handle_restore_backup(Key) ->
    appmI:inhibit_escalation(), % HU37699
    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    %% Find the corresponding upgrade package to go with this backup
    %% These functions can handle multiple active upgrade packages, but
    %% in reality there will be only one and swmServer will then effectively
    %% use the last one in the list
    Metadata = get_metadata(Index),
    SwVersion = Metadata#metadataV2.swVersion,
    BuName = Metadata#metadataV2.backupName,
    swmLib:write_swm_log(DN, info, "Restore: "++BuName),
    info_msg("Restoring backup ~s: ~p~n",[Index, BuName]),
    update_progress(Key, [{additionalInfo, "Restoring "++BuName},
			  {progressPercentage, 1}]),
    save_progress_report(Key),
    swmLib:set_ram_variable(restore_backup_active, true),
    case swmInventory:get_current_sw_version() of
	SwVersion ->
	    restore_data_only(Key);
	_ ->
	    UPs = swmModel:get_matching_up(SwVersion),
	    restore_with_software(Key, UPs)
    end.

restore_data_only(Key) ->
    update_progress(Key, [{additionalInfo, "Backup restore (data only)"},
			  {progressPercentage, 2}]),
    save_progress_report(Key),

    Index = element(5, Key),
    DN = "BrmBackup="++Index,
    BuPath = filename:join(swmLib:backup_dir(Index), "mnesia_backup.gz"),
    LastChance = "Last chance to cancel. Restore will commence in 30 seconds",
    update_progress(Key, [{additionalInfo, LastChance},
			  {progressPercentage, 3}]),
    save_progress_report(Key),
    Timeout = case swmLib:get_variable(swm_test_restore_timer) of
		  undefined -> 30000;
		  Timer -> 
		      swmLib:write_swm_log(
			DN, alert, 
			"Cancel timeout has been set to "++
			    integer_to_list(Timer div 1000)++" seconds"),
		      Timer
	      end,
    receive 
	cancel -> 
	    appmI:cancel_inhibit(), % HU37699
	    throw(cancelled);
	force -> 
	    info_msg("Force restore. Ignoring waiting period~n"),
	    ok
    after Timeout ->
	    ok
    end,

    swmLib:install_fallback(BuPath), 

    Index = element(5, Key),
    swmLib:write_swm_log(DN, info, "Backup restored. Rebooting..."),
    update_progress(Key, [{additionalInfo, "Backup restored. Rebooting..."},
			  {progressPercentage, 45}]),
    save_progress_report(Key),
    timer:apply_after(1000,
    		      ?MODULE,
    		      restart_node,
    		      [cold, ?ALH_TAG_DataRestore]),
    self()!stop_receiving_requests,
    ok.
 
save_progress_report(BuKey) ->
    [Obj] = mnesia:dirty_read({brmBackup, BuKey}),
    Index = element(5, BuKey),
    Path = filename:join(swmLib:backup_dir(Index), "progress.txt"),
    {ok, Fd} = file:open(Path, [write]),
    do_save_progress_report(Fd, record_info(fields, 'AsyncActionProgress'),
			    tl(tuple_to_list(Obj#brmBackup.progressReport))),
    file:close(Fd),
    swmLib:sync().


do_save_progress_report(Fd, [Field|Fields], [Value|Values]) ->
    io:format(Fd, "~p.~n",[{Field, Value}]),
    do_save_progress_report(Fd, Fields, Values);    
do_save_progress_report(_, [], _) ->
    ok.

is_progress_report() ->
    Pattern = filename:join([swmLib:backup_dir(), "*", "progress.txt"]),
    case filelib:wildcard(Pattern) of
	[] -> false;
	_ -> true
    end.

read_progress_report(Index) ->
    Path = filename:join(swmLib:backup_dir(Index), "progress.txt"),
    case file:consult(Path) of
	{ok, ProgressList} ->
	    file:delete(Path),
	    Values = 
		read_progress_report(record_info(fields, 'AsyncActionProgress'),
				     ProgressList),
	    list_to_tuple(['AsyncActionProgress'|Values]);
	{error, Reason} ->
	    {error, Reason}
    end.

read_progress_report([], _) -> [];
read_progress_report([Field|Fields], ProgressList) -> 
    [proplists:get_value(Field, ProgressList, undefined)|
     read_progress_report(Fields, ProgressList)].
    
restore_with_software(Key, [UP]) ->
    update_progress(Key, [{additionalInfo, "Backup restore (with software)"},
			  {progressPercentage, 2}]),
    save_progress_report(Key),

    Index = element(5, Key),
    DN = "BrmBackup="++Index,

    update_progress(Key, [{additionalInfo, "Use cancel to stop the restore"},
			  {progressPercentage, 3}]),
    save_progress_report(Key),
    Timeout = case swmLib:get_variable(swm_test_restore_timer) of
		  undefined -> 30000;
		  Timer -> 
		      swmLib:write_swm_log(
			DN, alert, 
			"Cancel timeout has been set to "++
			    integer_to_list(Timer div 1000)++" seconds"),
		      Timer
	      end,
    receive 
	cancel -> 
	    appmI:cancel_inhibit(), % HU37699
	    throw(cancelled)
    after Timeout ->
	    ok
    end,

    update_progress(Key, [{additionalInfo, "Installing upgrade package"},
			  {progressPercentage, 18}]),
    save_progress_report(Key),
    swmOs:homes(mount),
    swmOs:clear(), 

    case swmLib:get_ram_variable(audit_software) of
	true ->
	    throw({fail, "The software manager is busy auditing software. "
		   "Try again later."});
	_ ->
	    swmOs:install_up(UP)
    end,

    update_progress(Key, [{additionalInfo, "Preparing restore database"},
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
    RestoreDir = filename:join(OtherHomeDir, "restore"),
    info_msg("Making symlink ~p -> ~p ~n",
	     [RestoreDir, swmLib:backup_dir(Index)]),
    ok = file:make_symlink(swmLib:backup_dir(Index), RestoreDir),

    %% cmd(["mkdir -p ",RestoreDir]),
    %% cmd(["cp ", swmLib:backup_dir(Index), "/* ",RestoreDir]),
    update_progress(Key, [{additionalInfo, "Loading OS"},
			  {progressPercentage, 25}]),

    save_progress_report(Key),
    receive 
	cancel -> 
	    appmI:cancel_inhibit(), % HU37699
	    throw(cancelled)
    after 0 ->
	    ok
    end,

    swmOs:preload(),
    
    LastChance = "Last chance to cancel. Restore will commence in 30 seconds",
    update_progress(Key, [{additionalInfo, LastChance},
			  {progressPercentage, 29}]),
    save_progress_report(Key),
    %% Last chance
    receive 
	cancel -> 
	    appmI:cancel_inhibit(), % HU37699
	    throw(cancelled)
    after Timeout ->
	    ok
    end,

    update_progress(Key, [{additionalInfo, "Activating"},
			  {progressPercentage, 47}]),
    save_progress_report(Key),
    swmOs:activate(),
    update_progress(Key, [{additionalInfo, "Switching over"},
			  {progressPercentage, 52}]),
    save_progress_report(Key),
    swmOs:commit(),
    swmOs:homes(umount),
    Msg = "Backup restored. Rebooting...",
    swmLib:write_swm_log(DN, info, Msg),
    update_progress(Key, [{additionalInfo, Msg}]),
    save_progress_report(Key),
    timer:apply_after(1000,
    		      ?MODULE,
    		      restart_node,
    		      [cold, ?ALH_TAG_SoftwareRestore]),
    self()!stop_receiving_requests,      
    ok.

%%% ----------------------------------------------------------
%%% #           get_metadata(Index)
%%% Input: Index:string()
%%% Output: #metadataV2{}
%%% Exceptions:
%%% Description: This function is supposed to return a compatible
%%%              metadata version, even if the metadata record changes
%%%              I.e. if a metadataV1 is found in the file and the latest
%%%              version is metadataV2 this function should convert to V2
%%% ----------------------------------------------------------

get_metadata(Index) ->
    Path = filename:join(swmLib:backup_dir(Index), "metadata"),
    {ok, [Md]} = file:consult(Path),
    convert_metadata(Md).

convert_metadata(Md) when is_record(Md, metadataV2) ->
    Md;
convert_metadata(Md) when is_record(Md, metadataV1) ->
    #metadataV1{backupName = Name,
		creationTime = Time,
		software = Software, 
		creationType = CType} = Md,
    #metadataV2{backupName = Name,
		creationTime=Time,
		swVersion = 
		    [#'ProductData'{productName=element(1,S),
				    productNumber=element(2,S),
				    productRevision=element(3,S),
				    description="",
				    type=""
				   }
		     ||S<-Software],
		creationType=CType}.

set_creation_type_manual(Index) ->
    set_creation_type(Index, ?BrmBackupCreationType_MANUAL).

set_creation_type(Index, NewCT) ->
    swmLib:lock_backup(Index),
    try do_set_creation_type(Index, NewCT) 
    after
	swmLib:unlock_backup(Index)
    end.

do_set_creation_type(Index, NewCT) ->
    Md = get_metadata(Index),
    Path = filename:join(swmLib:backup_dir(Index), "metadata"),
    case filelib:is_file(Path) of
	true ->
	    {ok, Fd} = file:open(Path, [write]),
	    io:format(Fd, "~p.~n", [Md#metadataV2{creationType=NewCT}]),
	    file:close(Fd);
	false ->
	    %% This is because housekeeping sometimes manage to remove the
	    %% backup before this happens
	    ok
    end.


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
%%% @doc Runs an asynchronous ECIM action
%%% The Fun must set a default progress report

action_handler(Fun, ReportKey) ->
    
    try Fun() 
    catch
	throw:cancelled ->
	    handle_cancelled(ReportKey);
	throw:{read_file_info, {no_such_file, File}} ->
	    error_msg("no such file on SFTP server: ~s~n", [File]),
	    ProgressInfo = "The action could not be completed",
	    handle_fail(ReportKey, ProgressInfo);
	throw:{read_file_info, {Reason, File}} ->
	    error_msg("~p: ~s~n", [Reason, File]),
	    ProgressInfo = "The action could not be completed",
	    handle_fail(ReportKey, ProgressInfo);
	throw:{fail, Message} -> % Operator action needed
	    handle_fail(ReportKey, Message);
	throw:Throw ->
	    warning_msg("throw ~p~n",[Throw]),
	    ProgressInfo = "The action could not be completed",
	    handle_fail(ReportKey, ProgressInfo);
	Type:Reason ->
	    ProgressInfo = "A software related error occured",
	    sysInitI:error_report([{Type, Reason}, 
				   erlang:get_stacktrace()]),
	    handle_fail(ReportKey,ProgressInfo)
    end.

%%% ----------------------------------------------------------
%%% @doc Handles the reception of an exception

handle_fail(ReportKey, ProgressInfo) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(ReportKey,
		    [{result, ?ActionResultType_FAILURE},
		     {additionalInfo, ProgressInfo},
		     {state, ?ActionStateType_FINISHED},
		     {timeActionCompleted, CompleteTime}]),
    cleanup(),
    {failed, ProgressInfo}.

%%% ----------------------------------------------------------
%%% @doc Handles the reception of a cancellation

handle_cancelled(ReportKey) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(ReportKey, 
		    [{result, ?ActionResultType_FAILURE}, %HS80846
		     {state, ?ActionStateType_CANCELLED},
		     {progressPercentage, 100},
		     {timeActionCompleted, CompleteTime}]),
    cleanup().

%%% ----------------------------------------------------------
%%% @doc Cleans up any open sftp connection

cleanup() ->
    swmLib:set_ram_variable(restore_backup_active, false),
    case get(channelPid) of
	Pid when is_pid(Pid) ->
	    case get(handle) of
		undefined ->
		    ok;
		Handle ->
		    ssh_sftp:close(Pid, Handle)
	    end,
	    ssh_sftp:stop_channel(Pid);
	undefined -> ok
    end,
    case get(connectionRef) of
	CRef when is_pid(CRef) -> ssh:close(CRef);
	undefined -> ok
    end.

%%% ----------------------------------------------------------
restart_node(Rank, AvliCause) ->
    swmLib:activate_upgrWindow_tables(),
    appmI:restart_node(Rank, AvliCause).

%%% ----------------------------------------------------------
%%% @doc A generic function for updating the AsyncActionProgress struct
%%%
%%% This function updates the AsyncActionProgress struct of various classes
%%% depending on the Key attribute

-type backup_key() :: {string(), string(), string(), string(), string()}.
-type key_type() :: mgr | schedule | export | failsafe | undefined | backup_key().
-type pdata() :: string() | integer().
-type progress_data() :: [{Field::atom(), StrValue::pdata()}].

-spec update_progress(Key::key_type(), ProgressData::progress_data()) -> {atomic, ok} | {aborted, Reason::any()}.

update_progress(undefined, _) ->
    ok;
update_progress(mgr, ProgressData) ->
    mnesia:transaction(
      fun() ->
    	      [Obj] = mnesia:read({brmBackupManager, {"1","1","1","1"}}),
    	      Old = case Obj#brmBackupManager.progressReport of
    			undefined ->
    			    warning_msg("No default progress report!~n"),
    			    default_progress_report("brmBackupManager", 0);
    			PR -> PR
    		    end,
    	      Progress =  comsaI:update_progress(ProgressData, Old),
    	      mnesia:write(Obj#brmBackupManager{progressReport=Progress})
      end);
update_progress(schedule, ProgressData) ->
    mnesia:transaction(
      fun() ->
    	      [Obj] = mnesia:read({brmBackupScheduler, {"1","1","1","1","1"}}),
    	      Old = case Obj#brmBackupScheduler.progressReport of
    			undefined ->
    			    warning_msg("No default progress report!~n"),
    			    default_progress_report("brmBackupScheduler", 0);
    			PR -> PR
    		    end,
    	      Progress =  comsaI:update_progress(ProgressData, Old),
    	      mnesia:write(Obj#brmBackupScheduler{progressReport=Progress})
      end);
update_progress(failsafe, ProgressData) ->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read({brmFailsafeBackup, {"1","1","1","1","1"}}),
	      Old = case Obj#brmFailsafeBackup.progressReport of
			undefined ->
			    warning_msg("No default progress report!~n"),
			    default_progress_report("brmFailsafeBackup", 0);
			PR -> PR
		    end,
	      Progress = comsaI:update_progress(ProgressData, Old),
	      mnesia:write(Obj#brmFailsafeBackup{progressReport = Progress,
						 progress = Progress})
      end);
update_progress(Key, ProgressData)->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read({brmBackup, Key}),
	      Old = case Obj#brmBackup.progressReport of
			undefined ->
			    warning_msg("No default progress report!~n"),
			    default_progress_report("brmBackup", 0);
			PR -> PR
		    end,
	      Progress =  comsaI:update_progress(ProgressData, Old),
	      mnesia:write(Obj#brmBackup{progressReport=Progress})
      end).

set_default_progress_report(mgr, Action, Id) ->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:wread({brmBackupManager, {"1", "1","1","1"}}),
	      Progress = default_progress_report(Action, Id),
	      mnesia:write(Obj#brmBackupManager{progressReport=Progress}),
	      BuKeys = mnesia:all_keys(brmBackup),
	      IdlePr = default_idle_pr(),
	      [begin
		   [BuObj] = mnesia:read({brmBackup, Key}),
		   mnesia:write(BuObj#brmBackup{progressReport=IdlePr})
	       end||Key<-BuKeys]
      end);
set_default_progress_report(failsafe, Action, Id) ->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:wread({brmFailsafeBackup, {"1","1","1","1","1"}}),
	      Progress = default_progress_report(Action, Id),
	      mnesia:write(Obj#brmFailsafeBackup{progressReport=Progress})
      end);
set_default_progress_report(BuKey, Action, Id) ->
    mnesia:transaction(
      fun() ->
	      [Mgr] = mnesia:read({brmBackupManager, {"1", "1","1","1"}}),
	      IdlePr = default_idle_pr(),
	      mnesia:write(Mgr#brmBackupManager{progressReport=IdlePr}),
	      BuKeys = mnesia:all_keys(brmBackup),
	      [begin
		   [BuObj] = mnesia:read({brmBackup, Key}),
		   mnesia:write(BuObj#brmBackup{progressReport=IdlePr})
	       end||Key<-BuKeys, Key /= BuKey],
	      [Obj] = mnesia:wread({brmBackup, BuKey}),
	      Progress = default_progress_report(Action, Id),
	      mnesia:write(Obj#brmBackup{progressReport=Progress})
      end).
    

default_progress_report(Action, Id) ->
    #'AsyncActionProgress'
	{actionName=Action,
	 additionalInfo=["Action started"],
	 progressInfo = "Action started",
	 progressPercentage=0,
	 result=?ActionResultType_NOT_AVAILABLE,
	 resultInfo="",
	 state=?ActionStateType_RUNNING,
	 actionId=Id,
	 timeActionStarted = comsaI:iso_time(os:timestamp(), extended),
	 timeActionCompleted= "",
	 timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)}.

default_idle_pr() ->
    #'AsyncActionProgress'
	{actionName="",
	 additionalInfo=[],
	 progressInfo = "No action ongoing",
	 progressPercentage=100,
	 result=?ActionResultType_NOT_AVAILABLE,
	 resultInfo="",
	 state=?ActionStateType_FINISHED,
	 actionId=0,
	 timeActionStarted = "",
	 timeActionCompleted= "",
	 timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)}.
    


%%% ----------------------------------------------------------
%%% @doc Parse an http uri or return an exception

parse_uri(Uri) ->
    parse_uri(Uri, mgr).

parse_uri(Uri, ReportKey) ->
    case http_uri:parse(Uri) of
	{ok, Parsed} -> {ok, Parsed};
	{error, no_scheme} -> 
	    update_progress(
	      ReportKey,
	      [{resultInfo, "The uri has no scheme or it is unknown"}]),
	    throw({no_scheme, Uri});
	{error, {malformed_url, Scheme, AbsURI}} -> 
	    update_progress(
	      ReportKey,
	      [{resultInfo, "Malformed uri: "++AbsURI}]),
	    throw({malformed_url, Scheme, AbsURI});
	{error, {no_default_port, Scheme, AbsURI}} ->
	    update_progress(
	      ReportKey,
	      [{resultInfo, "The system has no default port for "++
		    atom_to_list(Scheme)}]),
	    throw({no_default_port, Scheme, AbsURI})
    end.

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
    case swmLib:lock_action_capable(?INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_ID,
                                    ?INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_INFO) of
    ok ->
        WP = mnesia:table_info(brmBackup, wild_pattern),
        Pattern = WP#brmBackup{creationType=?BrmBackupCreationType_SYSTEM_CREATED},
    
        {atomic, ok} = 
    	mnesia:transaction(
    	  fun() ->
    		  ScBackups = mnesia:match_object(Pattern),
    		  [delete_backup_internal(Backup#brmBackup.backupName,forced)||
    		      Backup<-ScBackups,
    		      not is_protected_backup(Backup#brmBackup.backupName)],
    		  ok
    	  end),
        ok = swmLib:unlock_action_capable(?INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_ID),
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
%%% @doc Format a datetime to a string

format_date({{Y,M,D},{H,Mi,S}}) ->
    lists:append([integer_to_list(Y), "-", padzero(M), "-", padzero(D),"T",
		  padzero(H), ":", padzero(Mi), ":", padzero(S)]).
    
%%% ----------------------------------------------------------
%%% @doc Add a zero to a single digit number and returna two digit string.

padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.

%%% ----------------------------------------------------------
%%% @doc Execute a shell command and print the result in the erlang shell

cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.


info_msg(Format) ->
   info_msg(Format, []).
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format) ->
   warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

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

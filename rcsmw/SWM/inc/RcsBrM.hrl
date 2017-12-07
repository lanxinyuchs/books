%%% --------------------------------------------------------
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
%%% --------------------------------------------------------

-hrl_id({"RcsBrM","2.3.0","/main/R1A/R2A/R3A/R5A/R6A/R8A/R9A/R11A/2"}).


%% -------------- CLASS BrM -------------------------

%% Description:
%% ECIM Backup and Restore Management top-level class.

-record(brM, {brMId,
              exportPackageLabelPrefix}).

-define(brM_types,
        [{brMId, string},
         {exportPackageLabelPrefix, string}]).

-define(BrM_restricted, [brMId]).


%% -------------- CLASS BrmBackupManager -------------------------

%% Description:
%% Serves as container for Backup instances of a particular backupType and backupDomain.

-record(brmBackupManager, {brmBackupManagerId,
                           backupType,
                           backupDomain,
                           progressReport,
                           autoExport,
                           autoExportUri,
                           autoExportPassword,
                           manualBackupName}).

-define(brmBackupManager_types,
        [{brmBackupManagerId, string},
         {backupType, string},
         {backupDomain, string},
         {progressReport, {struct,'AsyncActionProgress'}},
         {autoExport, 'RcsBrM.BrmAutoExport'},
         {autoExportUri, string},
         {autoExportPassword, {struct,'EcimPassword'}},
         {manualBackupName, 'RcsBrM.EcimBackupNameString'}]).

-define(brmBackupManager_autoExport_default, 'DISABLED').
-define(BrmBackupManager_restricted, [brmBackupManagerId]).


%% -------------- CLASS BrmBackup -------------------------

%% Description:
%% Represents one backup of the type and domain specified by the BrmBackupManager in which it is contained.

-record(brmBackup, {brmBackupId,
                    backupName,
                    creationTime,
                    status,
                    progressReport,
                    creationType,
                    swVersion}).

-define(brmBackup_types,
        [{brmBackupId, string},
         {backupName, string},
         {creationTime, 'RcsBrM.DateTime'},
         {status, 'RcsBrM.BrmBackupStatus'},
         {progressReport, {struct,'AsyncActionProgress'}},
         {creationType, 'RcsBrM.BrmBackupCreationType'},
         {swVersion, {sequence,{struct,'ProductData'}}}]).

-define(BrmBackup_restricted, [brmBackupId]).


%% -------------- CLASS BrmSingleEvent -------------------------

%% Description:
%% A single scheduled backup event.

-record(brmSingleEvent, {brmSingleEventId,
                         scheduledTime}).

-define(brmSingleEvent_types,
        [{brmSingleEventId, string},
         {scheduledTime, 'RcsBrM.DateTime'}]).

-define(BrmSingleEvent_restricted, [brmSingleEventId]).


%% -------------- CLASS BrmPeriodicEvent -------------------------

%% Description:
%% Periodic backup event.

-record(brmPeriodicEvent, {brmPeriodicEventId,
                           months,
                           weeks,
                           days,
                           hours,
                           minutes,
                           startTime,
                           stopTime}).

-define(brmPeriodicEvent_types,
        [{brmPeriodicEventId, string},
         {months, uint16},
         {weeks, uint16},
         {days, uint16},
         {hours, uint16},
         {minutes, uint16},
         {startTime, 'RcsBrM.DateTime'},
         {stopTime, 'RcsBrM.DateTime'}]).

-define(brmPeriodicEvent_months_default, 0).
-define(brmPeriodicEvent_weeks_default, 0).
-define(brmPeriodicEvent_days_default, 0).
-define(brmPeriodicEvent_minutes_default, 0).
-define(BrmPeriodicEvent_restricted, [brmPeriodicEventId]).


%% -------------- CLASS BrmBackupHousekeeping -------------------------

%% Description:
%% Provides optional support for automated housekeeping of manually created backups. 

-record(brmBackupHousekeeping, {brmBackupHousekeepingId,
                                maxStoredManualBackups,
                                autoDelete}).

-define(brmBackupHousekeeping_types,
        [{brmBackupHousekeepingId, string},
         {maxStoredManualBackups, 'RcsBrM.RcsBrM_BrmBackupHousekeeping_maxStoredManualBackups'},
         {autoDelete, 'RcsBrM.BrmManualBackupAutoDelete'}]).

-define(brmBackupHousekeeping_maxStoredManualBackups_default, 30).
-define(brmBackupHousekeeping_autoDelete_default, 'ENABLED').
-define(BrmBackupHousekeeping_restricted, [brmBackupHousekeepingId]).


%% -------------- CLASS BrmBackupScheduler -------------------------

%% Description:
%% Provides optional support for scheduling backup creation.

-record(brmBackupScheduler, {brmBackupSchedulerId,
                             maxStoredScheduledBackups,
                             scheduledBackupName,
                             progressReport,
                             mostRecentlyCreatedAutoBackup,
                             autoExport,
                             autoExportUri,
                             autoExportPassword,
                             adminState,
                             nextScheduledTime}).

-define(brmBackupScheduler_types,
        [{brmBackupSchedulerId, string},
         {maxStoredScheduledBackups, 'RcsBrM.RcsBrM_BrmBackupScheduler_maxStoredScheduledBackups'},
         {scheduledBackupName, string},
         {progressReport, {struct,'AsyncActionProgress'}},
         {mostRecentlyCreatedAutoBackup, string},
         {autoExport, 'RcsBrM.BrmAutoExport'},
         {autoExportUri, string},
         {autoExportPassword, {struct,'EcimPassword'}},
         {adminState, 'RcsBrM.BasicAdmState'},
         {nextScheduledTime, 'RcsBrM.DateTime'}]).

-define(brmBackupScheduler_maxStoredScheduledBackups_default, 5).
-define(brmBackupScheduler_scheduledBackupName_default, "BACKUP").
-define(brmBackupScheduler_autoExport_default, 'DISABLED').
-define(brmBackupScheduler_adminState_default, 'UNLOCKED').
-define(BrmBackupScheduler_restricted, [brmBackupSchedulerId]).


%% -------------- CLASS BrmCalendarBasedPeriodicEvent -------------------------

%% Description:
%% Periodic backup event using calendar-based interval.

-record(brmCalendarBasedPeriodicEvent, {brmCalendarBasedPeriodicEventId,
                                        month,
                                        dayOfWeek,
                                        dayOfMonth,
                                        time,
                                        dayOfWeekOccurrence,
                                        startTime,
                                        stopTime}).

-define(brmCalendarBasedPeriodicEvent_types,
        [{brmCalendarBasedPeriodicEventId, string},
         {month, 'RcsBrM.Month'},
         {dayOfWeek, 'RcsBrM.DayOfWeek'},
         {dayOfMonth, 'RcsBrM.DayOfMonth'},
         {time, 'RcsBrM.Time'},
         {dayOfWeekOccurrence, 'RcsBrM.DayOfWeekOccurrence'},
         {startTime, 'RcsBrM.DateTime'},
         {stopTime, 'RcsBrM.DateTime'}]).

-define(brmCalendarBasedPeriodicEvent_month_default, 0).
-define(brmCalendarBasedPeriodicEvent_dayOfWeek_default, 'ALL').
-define(brmCalendarBasedPeriodicEvent_dayOfMonth_default, 0).
-define(brmCalendarBasedPeriodicEvent_dayOfWeekOccurrence_default, 'ALL').
-define(BrmCalendarBasedPeriodicEvent_restricted, [brmCalendarBasedPeriodicEventId]).


%% -------------- CLASS BrmRollbackAtRestore -------------------------

%% Description:
%% Provides optional support for confirmation of a restore action or automatic rollback after an unconfirmed restore operation was executed.

-record(brmRollbackAtRestore, {brmRollbackAtRestoreId,
                               timeAllowedBeforeRollback,
                               timeRemainingBeforeRollback}).

-define(brmRollbackAtRestore_types,
        [{brmRollbackAtRestoreId, string},
         {timeAllowedBeforeRollback, uint16},
         {timeRemainingBeforeRollback, uint16}]).

-define(BrmRollbackAtRestore_restricted, [brmRollbackAtRestoreId]).


%% -------------- CLASS BrmBackupLabelStore -------------------------

%% Description:
%% Provides optional support for labeling of certain backups.

-record(brmBackupLabelStore, {brmBackupLabelStoreId,
                              lastRestoredBackup,
                              lastCreatedBackup,
                              lastImportedBackup,
                              lastExportedBackup,
                              restoreEscalationList}).

-define(brmBackupLabelStore_types,
        [{brmBackupLabelStoreId, string},
         {lastRestoredBackup, string},
         {lastCreatedBackup, string},
         {lastImportedBackup, string},
         {lastExportedBackup, string},
         {restoreEscalationList, {sequence,string}}]).

-define(BrmBackupLabelStore_restricted, [brmBackupLabelStoreId]).


%% -------------- CLASS BrmFailsafeBackup -------------------------

%% Description:
%% MO for activating and deactivating failsafe backups.

-record(brmFailsafeBackup, {brmFailsafeBackupId,
                            backupName,
                            timeRemaining,
                            usageState,
                            progressReport,
                            timeoutLength,
                            backup,
                            progress}).

-define(brmFailsafeBackup_types,
        [{brmFailsafeBackupId, string},
         {backupName, string},
         {timeRemaining, uint64},
         {usageState, 'RcsBrM.UsageState'},
         {progressReport, {struct,'AsyncActionProgress'}},
         {timeoutLength, 'RcsBrM.RcsBrM_BrmFailsafeBackup_timeoutLength'},
         {backup, string},
         {progress, {struct,'AsyncActionProgress'}}]).

-define(brmFailsafeBackup_timeoutLength_default, 1200).
-define(BrmFailsafeBackup_restricted, [brmFailsafeBackupId]).


%% ------------------ ENUM BrmBackupStatus ----------------------
-ifndef('BrmBackupStatus').
-define('BrmBackupStatus', 1).

-define(BrmBackupStatus_BRM_BACKUP_COMPLETE, 1).
-define(BrmBackupStatus_BRM_BACKUP_INCOMPLETE, 2).
-define(BrmBackupStatus_BRM_BACKUP_CORRUPTED, 3).

-endif. % BrmBackupStatus

%% ------------------ ENUM BasicAdmState ----------------------
-ifndef('BasicAdmState').
-define('BasicAdmState', 1).

-define(BasicAdmState_LOCKED, 0).
-define(BasicAdmState_UNLOCKED, 1).

-endif. % BasicAdmState

%% ------------------ ENUM BrmManualBackupAutoDelete ----------------------
-ifndef('BrmManualBackupAutoDelete').
-define('BrmManualBackupAutoDelete', 1).

-define(BrmManualBackupAutoDelete_ENABLED, 1).
-define(BrmManualBackupAutoDelete_DISABLED, 2).

-endif. % BrmManualBackupAutoDelete

%% ------------------ ENUM ActionResultType ----------------------
-ifndef('ActionResultType').
-define('ActionResultType', 1).

-define(ActionResultType_SUCCESS, 1).
-define(ActionResultType_FAILURE, 2).
-define(ActionResultType_NOT_AVAILABLE, 3).

-endif. % ActionResultType

%% ------------------ ENUM BrmBackupCreationType ----------------------
-ifndef('BrmBackupCreationType').
-define('BrmBackupCreationType', 1).

-define(BrmBackupCreationType_MANUAL, 1).
-define(BrmBackupCreationType_SCHEDULED, 2).
-define(BrmBackupCreationType_SYSTEM_CREATED, 3).

-endif. % BrmBackupCreationType

%% ------------------ ENUM BrmDeactivatePostAction ----------------------
-ifndef('BrmDeactivatePostAction').
-define('BrmDeactivatePostAction', 1).

-define(BrmDeactivatePostAction_NOP, 1).
-define(BrmDeactivatePostAction_CLEAR_LIST, 2).
-define(BrmDeactivatePostAction_ADD_BACKUP_TO_LIST, 3).

-endif. % BrmDeactivatePostAction

%% ------------------ ENUM BrmAutoExport ----------------------
-ifndef('BrmAutoExport').
-define('BrmAutoExport', 1).

-define(BrmAutoExport_DISABLED, 2).
-define(BrmAutoExport_ENABLED, 1).

-endif. % BrmAutoExport

%% ------------------ ENUM UsageState ----------------------
-ifndef('UsageState').
-define('UsageState', 1).

-define(UsageState_IDLE, 0).
-define(UsageState_ACTIVE, 1).
-define(UsageState_BUSY, 2).

-endif. % UsageState

%% ------------------ ENUM DayOfWeek ----------------------
-ifndef('DayOfWeek').
-define('DayOfWeek', 1).

-define(DayOfWeek_SUNDAY, 7).
-define(DayOfWeek_MONDAY, 1).
-define(DayOfWeek_TUESDAY, 2).
-define(DayOfWeek_WEDNESDAY, 3).
-define(DayOfWeek_THURSDAY, 4).
-define(DayOfWeek_FRIDAY, 5).
-define(DayOfWeek_SATURDAY, 6).
-define(DayOfWeek_ALL, 0).

-endif. % DayOfWeek

%% ------------------ ENUM DayOfWeekOccurrence ----------------------
-ifndef('DayOfWeekOccurrence').
-define('DayOfWeekOccurrence', 1).

-define(DayOfWeekOccurrence_FIRST, 1).
-define(DayOfWeekOccurrence_SECOND, 2).
-define(DayOfWeekOccurrence_THIRD, 3).
-define(DayOfWeekOccurrence_FOURTH, 4).
-define(DayOfWeekOccurrence_LAST, 5).
-define(DayOfWeekOccurrence_ALL, 0).

-endif. % DayOfWeekOccurrence

%% ------------------ ENUM ActionStateType ----------------------
-ifndef('ActionStateType').
-define('ActionStateType', 1).

-define(ActionStateType_CANCELLING, 1).
-define(ActionStateType_RUNNING, 2).
-define(ActionStateType_FINISHED, 3).
-define(ActionStateType_CANCELLED, 4).

-endif. % ActionStateType

%% ------------------ STRUCT ProductData ----------------------
-ifndef(_PRODUCT_DATA).
-define(_PRODUCT_DATA, 1).

-record('ProductData', {productName,
                        productNumber,
                        productRevision,
                        productionDate,
                        description,
                        type}).

-define('ProductData_types',
        [{productName, string},
         {productNumber, string},
         {productRevision, string},
         {productionDate, 'RcsBrM.DateTime'},
         {description, string},
         {type, string}]).


-endif. % _PRODUCT_DATA


%% ------------------ STRUCT EcimPassword ----------------------
-ifndef(_ECIM_PASSWORD).
-define(_ECIM_PASSWORD, 1).

-record('EcimPassword', {cleartext,
                         password}).

-define('EcimPassword_types',
        [{cleartext, 'RcsBrM.EcimEmpty'},
         {password, string}]).


-endif. % _ECIM_PASSWORD


%% ------------------ STRUCT AsyncActionProgress ----------------------
-ifndef(_ASYNC_ACTION_PROGRESS).
-define(_ASYNC_ACTION_PROGRESS, 1).

-record('AsyncActionProgress', {actionName,
                                additionalInfo,
                                progressInfo,
                                progressPercentage,
                                result,
                                resultInfo,
                                state,
                                actionId,
                                timeActionStarted,
                                timeActionCompleted,
                                timeOfLastStatusUpdate}).

-define('AsyncActionProgress_types',
        [{actionName, string},
         {additionalInfo, {sequence,string}},
         {progressInfo, string},
         {progressPercentage, uint8},
         {result, 'RcsBrM.ActionResultType'},
         {resultInfo, string},
         {state, 'RcsBrM.ActionStateType'},
         {actionId, 'RcsBrM.ActionInvocationResult'},
         {timeActionStarted, 'RcsBrM.DateTime'},
         {timeActionCompleted, 'RcsBrM.DateTime'},
         {timeOfLastStatusUpdate, 'RcsBrM.DateTime'}]).


-endif. % _ASYNC_ACTION_PROGRESS


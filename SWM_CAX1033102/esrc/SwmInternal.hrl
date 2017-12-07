%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	SwmInternal.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R2A/R3A/R4A/R5A/2').
-hrl_date('2016-03-25').
-hrl_author('ekurnik').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R1A/1      2012-02-15 etxjotj     Created
%%% R2A/3      2013-10-17 erarafo     Added COMTE types
%%% R2A/4      2013-12-18 etxjotj     Moved COMTE types
%%% R2A/5      2014-01-22 etxjotj     Added backup fault codes
%%% R3A/1      2014-10-28 erarafo     Support for CXP ordering
%%% R3A/2      2014-10-29 erarafo     Handle missing 'os' marker
%%% R4A/1      2015-09-24 etxjotj     Added exemption marking
%%% R5A/1      2016-03-11 etomist     Added ActionCapability IDs and infos
%%% R5A/2      2016-03-21 ekurnik     Added more ActionCapability IDs and infos
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: Backup fault codes to be matched with CPI
%%% ----------------------------------------------------------

-define(actionStarted, 0).
-define(nameValidationFailure, 1).
-define(duplicateName, 2).
-define(housekeepingRequired, 3).
-define(backupNotFound, 4).
-define(functionBusy, 97).
-define(missingParameter, 98).
-define(softwareFault, 99).

%% action ID used for lock/unlock actionCapable
-define(CREATE_UP_ACTION_CAPABLE_ID, swmCreateUpActionCapableId).
-define(REMOVE_UP_ACTION_CAPABLE_ID, swmRemoveUpActionCapableId).
-define(PREPARE_UP_ACTION_CAPABLE_ID, swmPrepareUpActionCapableId).
-define(VERIFY_UP_ACTION_CAPABLE_ID, swmVerifyUpActionCapableId).
-define(ACTIVATE_UP_ACTION_CAPABLE_ID, swmActivateUpActionCapableId).
-define(CONFIRM_UP_ACTION_CAPABLE_ID, swmConfirmUpActionCapableId).
-define(CREATE_BACKUP_ACTION_CAPABLE_ID, swmCreateBackupActionCapableId).
-define(DELETE_BACKUP_ACTION_CAPABLE_ID, swmDeleteBackupActionCapableId).
-define(IMPORT_BACKUP_ACTION_CAPABLE_ID, swmImportBackupActionCapableId).
-define(EXPORT_BACKUP_ACTION_CAPABLE_ID, swmExportBackupActionCapableId).
-define(RESTORE_BACKUP_ACTION_CAPABLE_ID, swmRestoreBackupActionCapableId).
-define(BACKUP_SCHEDULER_ACTION_CAPABLE_ID, swmBackupSchedulerActionCapableId).
-define(SWM_FAILSAFE_ACTION_CAPABLE_ID, swmFailsafeActionCapableId). 
-define(INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_ID, 
        swmInternalHouseheepingActionCapableId).

%% This string defines actionCapableInfo when action is performed
-define (CREATE_UP_ACTION_CAPABLE_INFO,
         "The system is performing SwM::createUpgradePackage.").
-define (REMOVE_UP_ACTION_CAPABLE_INFO,
         "The system is performing SwM::removeUpgradePackage.").
-define (PREPARE_UP_ACTION_CAPABLE_INFO,
         "The system is performing UpgradePackage::prepare.").
-define (VERIFY_UP_ACTION_CAPABLE_INFO,
         "The system is performing UpgradePackage::verify.").
-define (ACTIVATE_UP_ACTION_CAPABLE_INFO,
         "The system is performing UpgradePackage::activate.").
-define (CONFIRM_UP_ACTION_CAPABLE_INFO,
         "The system is performing UpgradePackage::confirm.").
-define(CREATE_BACKUP_ACTION_CAPABLE_INFO,
        "The system is executing BackupManager::createBackup.").
-define(DELETE_BACKUP_ACTION_CAPABLE_INFO,
        "The system is executing BackupManager::deleteBackup.").
-define(IMPORT_BACKUP_ACTION_CAPABLE_INFO,
        "The system is executing BackupManager::importBackup.").
-define(EXPORT_BACKUP_ACTION_CAPABLE_INFO,
        "The system is executing Backup::export.").
-define(RESTORE_BACKUP_ACTION_CAPABLE_INFO,
        "The system is executing Backup::restore.").
-define(MAKE_SCHED_BACKUP_ACTION_CAPABLE_INFO, 
        "The system is in process of creating scheduled backup.").
-define(EXPORT_SCHED_BACKUP_ACTION_CAPABLE_INFO, 
        "The system is in process of exporting scheduled backup.").
-define(ACTIVATE_FAILSAFE_ACTION_CAPABLE_INFO, 
        "The system is executing a BrmFailsafeBackup::activate.").
-define(DEACTIVATE_FAILSAFE_ACTION_CAPABLE_INFO, 
        "The system is executing a BrmFailsafeBackup::deactivate.").
-define(INTERNAL_HOUSEKEEPING_ACTION_CAPABLE_INFO, 
        "The system is executing an internal housekeeping.").


%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------
%-define(CONSTANT_NAME,               Value).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           swmVariables
%%% Description: Record for internal disc_copy table
%%% ----------------------------------------------------------
-record(swmVariables, {key, value}).

%%% ----------------------------------------------------------
%%% #           swmRamVariables
%%% Description: Record for internal ram_copy table
%%% ----------------------------------------------------------
-record(swmRamVariables, {key, value}).

%%% ----------------------------------------------------------
%%% #           swmFallbackList
%%% Description: Table for internal maintenance of the swmFallbackList
%%% ----------------------------------------------------------

-record(swmFallbackList, {key,          % atom(),
			  name,         % string(),
			  creationType, % integer(), (enum)
			  date}).       % datetime() in UTC

%%% ----------------------------------------------------------
%%% #           swmBackup metadata format version 1
%%% Description: Determines the contents of the internal metadata file
%%% ----------------------------------------------------------
-record(metadataV1, {backupName, creationTime, software, creationType}).

%%% ----------------------------------------------------------
%%% #           swmBackup metadata format version 2
%%% Description: Determines the contents of the internal metadata file
%%% ----------------------------------------------------------

-record(metadataV2, {backupName, creationTime, swVersion, creationType}).


%%% ----------------------------------------------------------
%%% Description: A backlog of info tuples about CXPs to be mounted
%%% ----------------------------------------------------------

-record(mountBacklog,
	{isOsMounted=false   :: boolean(),
	 deferred=[]         :: [tuple()],
	 accSize=0           :: non_neg_integer(),
	 nofCxpsLeft=9999    :: non_neg_integer()
	 }).

%%% ----------------------------------------------------------
%%% Description: Monitoring exemptions
%%% ----------------------------------------------------------

-record(swmDbMonitorExemptions, {table, field}).

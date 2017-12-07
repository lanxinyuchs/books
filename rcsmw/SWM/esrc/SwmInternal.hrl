%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	SwmInternal.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R11A/1').
-hrl_author('etxberb').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R5A/3      2016-05-11 etxjotj     HU82011 Prevent create when in failsafe
%%% R6A/1      2016-08-18 etxpejn     Added ActionCapability for sign cert
%%% R6A/2      2016-09-14 etxberb     Added INSTALL_BOOTFALLBACK_ACTION_CAPABLE_
%%%                                   ID/INFO and swmLock.
%%% R8A/1      2016-10-26 etxpejn     Added AUDIT_SW_ACTION_CAPABLE_
%%% R8A/2      2016-11-17 edamkon     HV41457 Prevent backup while
%%%                                   system is upgrading
%%% R8A/3      2016-12-09 etxpejn     Corrected spelling
%%% R8A/5      2016-12-16 etxberb     Added swmBoardSW.
%%% R8A/6      2016-12-29 etxberb     Changed swmBoardSW to swmBoardOth.
%%% R8A/7      2017-01-19 etxberb     Added LOCKOBJ_bu_restore.
%% ----    ---------- -------  ------------------------------------------------
%%% R9A/2      2017-05-15 etxberb     Added bootInstance in #mountBacklog{}.
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-04-27 etxberb  Added swmAppdataReg.
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: Backup fault codes to be matched with CPI
%%%
%%% Note! The use of error codes is expected to be outdated in ECIM
%%% ----------------------------------------------------------

-define(actionStarted, 0).
-define(nameValidationFailure, 1).
-define(duplicateName, 2).
-define(housekeepingRequired, 3).
-define(backupNotFound, 4).
-define(backupNotAllowedDuringUpgrade, 95). %HV41457
-define(failsafeBusy, 96). % HU82011 Prevent create when in failsafe
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
-define(SIGNING_CERTIFICATE_ACTION_CAPABLE_ID, 
	swmSigningCertificateActionCapableId).
-define(INSTALL_BOOTFALLBACK_ACTION_CAPABLE_ID,
	swmInstallBootfallbackActionCapableId).
-define(AUDIT_SW_ACTION_CAPABLE_ID, swmAuditSwActionCapableId).


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
-define(SIGNING_CERTIFICATE_ACTION_CAPABLE_INFO,
	"The system is in the software signing certificate revocation grace period, which expires ").
-define(INSTALL_BOOTFALLBACK_ACTION_CAPABLE_INFO, 
        "The system is installing a boot-fallback UpgradePackage.").
-define (AUDIT_SW_ACTION_CAPABLE_INFO, "The system is performing software audit.").

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_ERR_ALL(__ReportInfo),
	error_logger:error_report(?RepInfo(__ReportInfo))).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO_ALL(__ReportInfo),
	error_logger:info_report(?RepInfo(__ReportInfo))).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(MODULE_STR, atom_to_list(?MODULE)).
-define(MonoTime, erlang:monotonic_time()).
-define(PROC_INFO(__Pid), sysUtil:pid_info(__Pid, {all, [error_handler]})).
-define(STATE_INFO(__Record),
	sysUtil:record_format(record_info(fields, state), __Record)).
-define(STACKTRACE_C,   % Current stacktrace
	element(2, process_info(self(), current_stacktrace))).
-define(STACKTRACE_E,   % Stacktrace at Exception
	erlang:get_stacktrace()).

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
%%% #           swmAppdataReg
%%% Description: Table for internal handling of appdata registrations
%%% ----------------------------------------------------------

-record(swmAppdataReg, {swp,                      % string(),
			is_registered = true}).   % boolean()

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

%%% Starting from R8A, backup metadata is handled as maps, thus no new record
%%% versions should be added

%%% ----------------------------------------------------------
%%% Description: A backlog of info tuples about CXPs to be mounted
%%% ----------------------------------------------------------

-record(mountBacklog,
	{isOsMounted=false   :: boolean(),
	 deferred=[]         :: [tuple()],
	 accSize=0           :: non_neg_integer(),
	 nofCxpsLeft=9999    :: non_neg_integer(),
	 bootInstance = swmOs:get_inactive_instance() :: 0|1|2|not_supported
	 }).

%%% ----------------------------------------------------------
%%% Description: Monitoring exemptions
%%% ----------------------------------------------------------

-record(swmDbMonitorExemptions, {table, field}).

%%% ----------------------------------------------------------
%%% Description: Internal locks
%%% ----------------------------------------------------------
-define(LOCKOBJ_audit_sw, audit_software).
-define(LOCKOBJ_inst_bootfb, install_bootfallback).
-define(LOCKOBJ_upgrade, upgrade).
-define(LOCKOBJ_bu_restore, bu_restore).

-record(swmLock, {lockobj, lockowner}).

%%% ----------------------------------------------------------
%%% Description: Boards of hwcategory="OTHER" reported by CAT via LMHI
%%% ----------------------------------------------------------
-record(swmBoardOth,
	{boardType,
	 sw = [],
	 sw_state}).

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

-hrl_id({"RcsLogM","2.4.2","/main/R1A/R2A/R3A/R4A/R9A/R10A/R11A/0"}).


%% -------------- CLASS LogM -------------------------

%% Description:
%% The LogM class contains various parameters concerning the log management service.

-record(logM, {logMId,
               progressReport,
               nodeCredential,
               trustCategory}).

-define(logM_types,
        [{logMId, string},
         {progressReport, {struct,'AsyncActionProgress'}},
         {nodeCredential, moRef},
         {trustCategory, moRef}]).

-define(LogM_restricted, [logMId]).


%% -------------- CLASS Log -------------------------

%% Description:
%% The Log class contains parameters for a specific log in the system.

-record(log, {logId,
              progressReport,
              severityFilter}).

-define(log_types,
        [{logId, string},
         {progressReport, {struct,'AsyncActionProgress'}},
         {severityFilter, {sequence,'RcsLogM.LogSeverity'}}]).

-define(Log_restricted, [logId]).


%% -------------- CLASS LogPushTransfer -------------------------

%% Description:
%% If this MO is created, the log management service pushes out log data automatically.
%% The system transfers a specific log whenever it reaches a maximum level or as a continuous stream. A rotating log transfers a file whenever a new file is rotated in. 
%% If there is no instance of this class for a specific log, the log service stops writing to a non-wrapping log when it reaches its maximum size. A wrap log wraps as usual when reaches its maximum size. Log data transfer for wrap files occurs before the log is wrapped.

-record(logPushTransfer, {logPushTransferId,
                          uri,
                          password,
                          transferType,
                          operationalState,
                          availabilityStatus}).

-define(logPushTransfer_types,
        [{logPushTransferId, string},
         {uri, string},
         {password, {struct,'EcimPassword'}},
         {transferType, 'RcsLogM.TransferType'},
         {operationalState, 'RcsLogM.OperState'},
         {availabilityStatus, {sequence,'RcsLogM.AvailStatus'}}]).

-define(logPushTransfer_transferType_default, 'BULK').
-define(LogPushTransfer_restricted, [logPushTransferId]).


%% ------------------ ENUM ActionStateType ----------------------
-ifndef('ActionStateType').
-define('ActionStateType', 1).

-define(ActionStateType_CANCELLING, 1).
-define(ActionStateType_RUNNING, 2).
-define(ActionStateType_FINISHED, 3).
-define(ActionStateType_CANCELLED, 4).

-endif. % ActionStateType

%% ------------------ ENUM AvailStatus ----------------------
-ifndef('AvailStatus').
-define('AvailStatus', 1).

-define(AvailStatus_IN_TEST, 0).
-define(AvailStatus_FAILED, 1).
-define(AvailStatus_POWER_OFF, 2).
-define(AvailStatus_OFF_LINE, 3).
-define(AvailStatus_OFF_DUTY, 4).
-define(AvailStatus_DEPENDENCY, 5).
-define(AvailStatus_DEGRADED, 6).
-define(AvailStatus_NOT_INSTALLED, 7).
-define(AvailStatus_LOG_FULL, 8).
-define(AvailStatus_DEPENDENCY_LOCKED, 9).
-define(AvailStatus_DEPENDENCY_FAILED, 10).
-define(AvailStatus_DEPENDENCY_SHUTTINGDOWN, 11).

-endif. % AvailStatus

%% ------------------ ENUM TransferType ----------------------
-ifndef('TransferType').
-define('TransferType', 1).

-define(TransferType_STREAM, 1).
-define(TransferType_BULK, 2).

-endif. % TransferType

%% ------------------ ENUM ActionResultType ----------------------
-ifndef('ActionResultType').
-define('ActionResultType', 1).

-define(ActionResultType_SUCCESS, 1).
-define(ActionResultType_FAILURE, 2).
-define(ActionResultType_NOT_AVAILABLE, 3).

-endif. % ActionResultType

%% ------------------ ENUM OperState ----------------------
-ifndef('OperState').
-define('OperState', 1).

-define(OperState_DISABLED, 0).
-define(OperState_ENABLED, 1).

-endif. % OperState

%% ------------------ ENUM LogSeverity ----------------------
-ifndef('LogSeverity').
-define('LogSeverity', 1).

-define(LogSeverity_EMERGENCY, 0).
-define(LogSeverity_ALERT, 1).
-define(LogSeverity_CRITICAL, 2).
-define(LogSeverity_ERROR, 3).
-define(LogSeverity_WARNING, 4).
-define(LogSeverity_NOTICE, 5).
-define(LogSeverity_INFO, 6).

-endif. % LogSeverity

%% ------------------ STRUCT EcimPassword ----------------------
-ifndef(_ECIM_PASSWORD).
-define(_ECIM_PASSWORD, 1).

-record('EcimPassword', {cleartext,
                         password}).

-define('EcimPassword_types',
        [{cleartext, 'RcsLogM.EcimEmpty'},
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
         {result, 'RcsLogM.ActionResultType'},
         {resultInfo, string},
         {state, 'RcsLogM.ActionStateType'},
         {actionId, 'RcsLogM.ActionInvocationResult'},
         {timeActionStarted, 'RcsLogM.DateTime'},
         {timeActionCompleted, 'RcsLogM.DateTime'},
         {timeOfLastStatusUpdate, 'RcsLogM.DateTime'}]).


-endif. % _ASYNC_ACTION_PROGRESS


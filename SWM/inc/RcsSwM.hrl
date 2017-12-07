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

-hrl_id({"RcsSwM","3.3.0","/main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/2"}).


%% -------------- CLASS SwM -------------------------

%% Description:
%% The root structural element of SW management activities.

-record(swM, {swMId,
              reportProgress,
              fallbackTimer,
              timeRemainingBeforeFallback,
              localFileStorePath,
              userLabel,
              timeoutFallbackCapability,
              actionCapable,
              actionCapableInfo,
              defaultUri,
              defaultPassword}).

-define(swM_types,
        [{swMId, string},
         {reportProgress, {struct,'AsyncActionProgress'}},
         {fallbackTimer, int16},
         {timeRemainingBeforeFallback, int16},
         {localFileStorePath, string},
         {userLabel, string},
         {timeoutFallbackCapability, 'RcsSwM.SwMTimeoutFallbackCapability'},
         {actionCapable, 'RcsSwM.ActionCapabilityState'},
         {actionCapableInfo, string},
         {defaultUri, string},
         {defaultPassword, {struct,'EcimPassword'}}]).

-define(swM_fallbackTimer_default, 1800).
-define(SwM_restricted, [swMId]).


%% -------------- CLASS UpgradePackage -------------------------

%% Description:
%% Controls the change process of the software configuration of a ME.

-record(upgradePackage, {upgradePackageId,
                         state,
                         reportProgress,
                         ignoreBreakPoints,
                         userLabel,
                         administrativeData,
                         activationStep,
                         created,
                         uri,
                         creatorActionId,
                         password}).

-define(upgradePackage_types,
        [{upgradePackageId, string},
         {state, 'RcsSwM.UpgradePackageState'},
         {reportProgress, {struct,'AsyncActionProgressWithSteps'}},
         {ignoreBreakPoints, boolean},
         {userLabel, string},
         {administrativeData, {sequence,{struct,'ProductData'}}},
         {activationStep, {sequence,{struct,'ActivationStep'}}},
         {created, 'RcsSwM.DateTime'},
         {uri, string},
         {creatorActionId, uint16},
         {password, {struct,'EcimPassword'}}]).

-define(upgradePackage_ignoreBreakPoints_default, true).
-define(UpgradePackage_restricted, [upgradePackageId]).


%% ------------------ ENUM ActionCapabilityState ----------------------
-ifndef('ActionCapabilityState').
-define('ActionCapabilityState', 1).

-define(ActionCapabilityState_CAPABLE, 1).
-define(ActionCapabilityState_WAIT, 2).

-endif. % ActionCapabilityState

%% ------------------ ENUM ActionStateType ----------------------
-ifndef('ActionStateType').
-define('ActionStateType', 1).

-define(ActionStateType_CANCELLING, 1).
-define(ActionStateType_RUNNING, 2).
-define(ActionStateType_FINISHED, 3).
-define(ActionStateType_CANCELLED, 4).

-endif. % ActionStateType

%% ------------------ ENUM SwMTimeoutFallbackCapability ----------------------
-ifndef('SwMTimeoutFallbackCapability').
-define('SwMTimeoutFallbackCapability', 1).

-define(SwMTimeoutFallbackCapability_SUPPORTED, 1).
-define(SwMTimeoutFallbackCapability_NOT_SUPPORTED, 2).

-endif. % SwMTimeoutFallbackCapability

%% ------------------ ENUM ActionResultType ----------------------
-ifndef('ActionResultType').
-define('ActionResultType', 1).

-define(ActionResultType_SUCCESS, 1).
-define(ActionResultType_FAILURE, 2).
-define(ActionResultType_NOT_AVAILABLE, 3).

-endif. % ActionResultType

%% ------------------ ENUM UpgradePackageState ----------------------
-ifndef('UpgradePackageState').
-define('UpgradePackageState', 1).

-define(UpgradePackageState_INITIALIZED, 1).
-define(UpgradePackageState_PREPARE_IN_PROGRESS, 2).
-define(UpgradePackageState_PREPARE_COMPLETED, 3).
-define(UpgradePackageState_ACTIVATION_IN_PROGRESS, 4).
-define(UpgradePackageState_ACTIVATION_STEP_COMPLETED, 5).
-define(UpgradePackageState_WAITING_FOR_COMMIT, 6).
-define(UpgradePackageState_COMMIT_COMPLETED, 7).
-define(UpgradePackageState_DEACTIVATION_IN_PROGRESS, 8).

-endif. % UpgradePackageState

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
         {productionDate, 'RcsSwM.DateTime'},
         {description, string},
         {type, string}]).


-endif. % _PRODUCT_DATA


%% ------------------ STRUCT EcimPassword ----------------------
-ifndef(_ECIM_PASSWORD).
-define(_ECIM_PASSWORD, 1).

-record('EcimPassword', {cleartext,
                         password}).

-define('EcimPassword_types',
        [{cleartext, 'RcsSwM.EcimEmpty'},
         {password, string}]).


-endif. % _ECIM_PASSWORD


%% ------------------ STRUCT AsyncActionProgressWithSteps ----------------------
-ifndef(_ASYNC_ACTION_PROGRESS_WITH_STEPS).
-define(_ASYNC_ACTION_PROGRESS_WITH_STEPS, 1).

-record('AsyncActionProgressWithSteps', {actionName,
                                         additionalInfo,
                                         progressInfo,
                                         progressPercentage,
                                         result,
                                         resultInfo,
                                         state,
                                         actionId,
                                         timeActionStarted,
                                         timeActionCompleted,
                                         timeOfLastStatusUpdate,
                                         step,
                                         stepProgressPercentage}).

-define('AsyncActionProgressWithSteps_types',
        [{actionName, string},
         {additionalInfo, {sequence,string}},
         {progressInfo, string},
         {progressPercentage, uint8},
         {result, 'RcsSwM.ActionResultType'},
         {resultInfo, string},
         {state, 'RcsSwM.ActionStateType'},
         {actionId, uint16},
         {timeActionStarted, 'RcsSwM.DateTime'},
         {timeActionCompleted, 'RcsSwM.DateTime'},
         {timeOfLastStatusUpdate, 'RcsSwM.DateTime'},
         {step, int16},
         {stepProgressPercentage, int8}]).


-endif. % _ASYNC_ACTION_PROGRESS_WITH_STEPS


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
         {result, 'RcsSwM.ActionResultType'},
         {resultInfo, string},
         {state, 'RcsSwM.ActionStateType'},
         {actionId, 'RcsSwM.ActionInvocationResult'},
         {timeActionStarted, 'RcsSwM.DateTime'},
         {timeActionCompleted, 'RcsSwM.DateTime'},
         {timeOfLastStatusUpdate, 'RcsSwM.DateTime'}]).


-endif. % _ASYNC_ACTION_PROGRESS


%% ------------------ STRUCT ActivationStep ----------------------
-ifndef(_ACTIVATION_STEP).
-define(_ACTIVATION_STEP, 1).

-record('ActivationStep', {serialNumber,
                           name,
                           description}).

-define('ActivationStep_types',
        [{serialNumber, int16},
         {name, string},
         {description, string}]).


-endif. % _ACTIVATION_STEP


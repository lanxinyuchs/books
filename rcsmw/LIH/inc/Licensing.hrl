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

-hrl_id({"Licensing","1.0.0","/main/R1A/R2A/R3A/1"}).


%% -------------- CLASS Licensing -------------------------

%% Description:
%% Licensing
%% This MO represents the Licensing (Software Keys) functions.
%% This MO is created automatically and cannot be deleted.
%% The RDN of this MO is Licensing=1.
%%       

-record(licensing, {'LicensingId',
                    userLabel,
                    emergencyStateInfo,
                    licenseFileUrl,
                    fingerprint,
                    lastLicensingPiChange,
                    licenseFileUrlIpv6}).

-define(licensing_types,
        [{'LicensingId', string},
         {userLabel, string},
         {emergencyStateInfo, {struct,'EmergencyInfo'}},
         {licenseFileUrl, string},
         {fingerprint, string},
         {lastLicensingPiChange, string},
         {licenseFileUrlIpv6, string}]).

-define(licensing_userLabel_default, "").
-define(licensing_licenseFileUrl_default, "").
-define(licensing_fingerprint_default, "").
-define(licensing_licenseFileUrlIpv6_default, "").
-define(Licensing_restricted, ['LicensingId']).


%% -------------- CLASS CapacityFeature -------------------------

%% Description:
%%       

-record(capacityFeature, {'CapacityId',
                          capacityUnit,
                          currentCapacityLimit,
                          keyId,
                          licensedCapacity,
                          gracePeriod,
                          capacityType}).

-define(capacityFeature_types,
        [{'CapacityId', string},
         {capacityUnit, string},
         {currentCapacityLimit, {struct,'CapacityData'}},
         {keyId, string},
         {licensedCapacity, {struct,'CapacityData'}},
         {gracePeriod, {struct,'GracePeriod'}},
         {capacityType, 'Licensing.CapacityType'}]).

-define(capacityFeature_capacityType_default, 'CAPACITY_LICENSE').
-define(CapacityFeature_restricted, ['CapacityId']).


%% -------------- CLASS OptionalFeature -------------------------

%% Description:
%%       

-record(optionalFeature, {'FeatureId',
                          featureState,
                          licenseState,
                          serviceState,
                          keyId,
                          featureInstanceRef}).

-define(optionalFeature_types,
        [{'FeatureId', string},
         {featureState, 'Licensing.FeatureActivationState'},
         {licenseState, 'Licensing.LicenseState'},
         {serviceState, 'Licensing.ServiceState'},
         {keyId, string},
         {featureInstanceRef, string}]).

-define(optionalFeature_featureState_default, 'DEACTIVATED').
-define(OptionalFeature_restricted, ['FeatureId']).


%% ------------------ ENUM EmergencyStatus ----------------------
-ifndef('EmergencyStatus').
-define('EmergencyStatus', 1).

-define(EmergencyStatus_NEVER_USED, 0).
-define(EmergencyStatus_ACTIVE, 1).
-define(EmergencyStatus_USE_DEGRADED, 2).
-define(EmergencyStatus_ACTIVE_AGAIN, 3).
-define(EmergencyStatus_USE_DISABLED, 4).

-endif. % EmergencyStatus

%% ------------------ ENUM FeatureActivationState ----------------------
-ifndef('FeatureActivationState').
-define('FeatureActivationState', 1).

-define(FeatureActivationState_DEACTIVATED, 0).
-define(FeatureActivationState_ACTIVATED, 1).

-endif. % FeatureActivationState

%% ------------------ ENUM LicenseState ----------------------
-ifndef('LicenseState').
-define('LicenseState', 1).

-define(LicenseState_DISABLED, 0).
-define(LicenseState_ENABLED, 1).

-endif. % LicenseState

%% ------------------ ENUM ServiceState ----------------------
-ifndef('ServiceState').
-define('ServiceState', 1).

-define(ServiceState_INOPERABLE, 0).
-define(ServiceState_OPERABLE, 1).

-endif. % ServiceState

%% ------------------ ENUM CapacityType ----------------------
-ifndef('CapacityType').
-define('CapacityType', 1).

-define(CapacityType_CAPACITY_LICENSE, 0).
-define(CapacityType_HWAC, 1).

-endif. % CapacityType

%% ------------------ ENUM LmGracePeriodState ----------------------
-ifndef('LmGracePeriodState').
-define('LmGracePeriodState', 1).

-define(LmGracePeriodState_INACTIVE, 0).
-define(LmGracePeriodState_ACTIVATED, 1).
-define(LmGracePeriodState_ACTIVATED_EXPIRING, 2).
-define(LmGracePeriodState_EXPIRED, 3).

-endif. % LmGracePeriodState

%% ------------------ STRUCT EmergencyInfo ----------------------
-ifndef(_EMERGENCY_INFO).
-define(_EMERGENCY_INFO, 1).

-record('EmergencyInfo', {state,
                          time}).

-define('EmergencyInfo_types',
        [{state, 'Licensing.EmergencyStatus'},
         {time, int32}]).


-endif. % _EMERGENCY_INFO


%% ------------------ STRUCT CapacityData ----------------------
-ifndef(_CAPACITY_DATA).
-define(_CAPACITY_DATA, 1).

-record('CapacityData', {value,
                         noLimit}).

-define('CapacityData_types',
        [{value, uint32},
         {noLimit, boolean}]).


-endif. % _CAPACITY_DATA


%% ------------------ STRUCT GracePeriod ----------------------
-ifndef(_GRACE_PERIOD).
-define(_GRACE_PERIOD, 1).

-record('GracePeriod', {isGracePeriodControlled,
                        gracePeriodState,
                        gracePeriodTimeLeft}).

-define('GracePeriod_types',
        [{isGracePeriodControlled, boolean},
         {gracePeriodState, 'Licensing.LmGracePeriodState'},
         {gracePeriodTimeLeft, int32}]).


-endif. % _GRACE_PERIOD


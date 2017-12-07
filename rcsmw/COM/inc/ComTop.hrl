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

-hrl_id({"ComTop","10.21.0","/main/R2A/R3A/R5A/R6A/R9A/R11A/1"}).


%% -------------- CLASS ManagedElement -------------------------

%% Description:
%% The top-level class in the Common Information Model is Managed Element root Managed Object Class.

-record(managedElement, {managedElementId,
                         siteLocation,
                         userLabel,
                         productIdentity,
                         localDateTime,
                         timeZone,
                         dateTimeOffset,
                         dnPrefix,
                         managedElementType,
                         release,
                         networkManagedElementId}).

-define(managedElement_types,
        [{managedElementId, string},
         {siteLocation, string},
         {userLabel, string},
         {productIdentity, {sequence,{struct,'ProductIdentity'}}},
         {localDateTime, 'ComTop.DateTimeWithoutOffset'},
         {timeZone, string},
         {dateTimeOffset, 'ComTop.DifferenceFromUTC'},
         {dnPrefix, string},
         {managedElementType, string},
         {release, string},
         {networkManagedElementId, string}]).

-define(ManagedElement_restricted, [managedElementId]).


%% -------------- CLASS SystemFunctions -------------------------

%% Description:
%% This model has a purpose to group the management of the system functions of the Managed Element.

-record(systemFunctions, {systemFunctionsId,
                          dummy}).

-define(systemFunctions_types,
        [{systemFunctionsId, string},
         {dummy, atom}]).

-define(SystemFunctions_restricted, [systemFunctionsId]).


%% -------------- CLASS Transport -------------------------

%% Description:
%% This is a container for common transport functions used within the Managed Element. 

-record(transport, {transportId,
                    dummy}).

-define(transport_types,
        [{transportId, string},
         {dummy, atom}]).

-define(Transport_restricted, [transportId]).


%% -------------- CLASS Legacy -------------------------

%% Description:
%% Introduced to support deprecated types

-record(legacy, {legacyId,
                 admState,
                 basicAdmState,
                 dateTime,
                 ipDNSAddress,
                 operState,
                 schemaFormat}).

-define(legacy_types,
        [{legacyId, string},
         {admState, 'ComTop.AdmState'},
         {basicAdmState, 'ComTop.BasicAdmState'},
         {dateTime, 'ComTop.DateTime'},
         {ipDNSAddress, 'ComTop.IpDNSAddress'},
         {operState, 'ComTop.OperState'},
         {schemaFormat, 'ComTop.SchemaFormat'}]).

-define(Legacy_restricted, [legacyId]).


%% ------------------ ENUM BasicAdmState ----------------------
-ifndef('BasicAdmState').
-define('BasicAdmState', 1).

-define(BasicAdmState_LOCKED, 0).
-define(BasicAdmState_UNLOCKED, 1).

-endif. % BasicAdmState

%% ------------------ ENUM AdmState ----------------------
-ifndef('AdmState').
-define('AdmState', 1).

-define(AdmState_LOCKED, 0).
-define(AdmState_UNLOCKED, 1).
-define(AdmState_SHUTTINGDOWN, 2).

-endif. % AdmState

%% ------------------ ENUM SchemaFormat ----------------------
-ifndef('SchemaFormat').
-define('SchemaFormat', 1).

-define(SchemaFormat_MP_DTD, 0).

-endif. % SchemaFormat

%% ------------------ ENUM OperState ----------------------
-ifndef('OperState').
-define('OperState', 1).

-define(OperState_DISABLED, 0).
-define(OperState_ENABLED, 1).

-endif. % OperState

%% ------------------ STRUCT ProductIdentity ----------------------
-ifndef(_PRODUCT_IDENTITY).
-define(_PRODUCT_IDENTITY, 1).

-record('ProductIdentity', {productNumber,
                            productRevision,
                            productDesignation}).

-define('ProductIdentity_types',
        [{productNumber, string},
         {productRevision, string},
         {productDesignation, string}]).


-endif. % _PRODUCT_IDENTITY


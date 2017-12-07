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

-hrl_id({"RcsLocalAuthorization","1.1.0","/main/R6A/R9A/1"}).


%% -------------- CLASS Role -------------------------

%% Description:
%% Authorization roles that may be assigned to users.

-record(role, {roleId,
               roleName,
               userLabel}).

-define(role_types,
        [{roleId, string},
         {roleName, string},
         {userLabel, string}]).

-define(Role_restricted, [roleId]).


%% -------------- CLASS Rule -------------------------

%% Description:
%% The rules that define the access control to objects.

-record(rule, {ruleId,
               ruleName,
               permission,
               ruleData,
               userLabel}).

-define(rule_types,
        [{ruleId, string},
         {ruleName, string},
         {permission, 'RcsLocalAuthorization.PermissionType'},
         {ruleData, 'RcsLocalAuthorization.RuleDataType'},
         {userLabel, string}]).

-define(Rule_restricted, [ruleId]).


%% -------------- CLASS LocalAuthorizationMethod -------------------------

%% Description:
%% The Root MOC of the Local Authorization method.

-record(localAuthorizationMethod, {localAuthorizationMethodId,
                                   administrativeState,
                                   userLabel}).

-define(localAuthorizationMethod_types,
        [{localAuthorizationMethodId, string},
         {administrativeState, 'RcsLocalAuthorization.BasicAdmState'},
         {userLabel, string}]).

-define(LocalAuthorizationMethod_restricted, [localAuthorizationMethodId]).


%% -------------- CLASS CustomRole -------------------------

%% Description:
%% Authorization roles defined by the MS that may be assigned to users.

-record(customRole, {customRoleId,
                     roleName,
                     userLabel,
                     rules}).

-define(customRole_types,
        [{customRoleId, string},
         {roleName, string},
         {userLabel, string},
         {rules, {sequence,moRef}}]).

-define(CustomRole_restricted,
        [customRoleId,
         roleName]).


%% -------------- CLASS CustomRule -------------------------

%% Description:
%% The rules created by the MS that define the access control to objects.

-record(customRule, {customRuleId,
                     ruleName,
                     permission,
                     ruleData,
                     reservedByRoles,
                     userLabel}).

-define(customRule_types,
        [{customRuleId, string},
         {ruleName, string},
         {permission, 'RcsLocalAuthorization.PermissionType'},
         {ruleData, 'RcsLocalAuthorization.RuleDataType'},
         {reservedByRoles, {sequence,moRef}},
         {userLabel, string}]).

-define(CustomRule_restricted, [customRuleId]).


%% ------------------ ENUM PermissionType ----------------------
-ifndef('PermissionType').
-define('PermissionType', 1).

-define(PermissionType_R, 4).
-define(PermissionType_RW, 6).
-define(PermissionType_X, 1).
-define(PermissionType_RX, 5).
-define(PermissionType_RWX, 7).
-define(PermissionType_NO_ACCESS, 0).

-endif. % PermissionType

%% ------------------ ENUM BasicAdmState ----------------------
-ifndef('BasicAdmState').
-define('BasicAdmState', 1).

-define(BasicAdmState_LOCKED, 0).
-define(BasicAdmState_UNLOCKED, 1).

-endif. % BasicAdmState

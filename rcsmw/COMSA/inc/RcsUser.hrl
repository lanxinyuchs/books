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

-hrl_id({"RcsUser","5.0.1","/main/R2A/R3A/R4A/2"}).


%% -------------- CLASS UserIdentity -------------------------

%% Description:
%% The UserIdentity class provides means for giving the true user name.
%% This class is used for recording the true user name, in case a generic user name has been used at login. 
%% This typically happens when the operator access the ME through a MS, and the user's credentials are only checked when logging in to the MS.
%% This class also provides a mechanism for shutting down all other sessions than maintenance user sessions.

-record(userIdentity, {userIdentityId,
                       userName}).

-define(userIdentity_types,
        [{userIdentityId, string},
         {userName, string}]).

-define(UserIdentity_restricted, [userIdentityId]).


%% -------------- CLASS MaintenanceUser -------------------------

%% Description:
%% This class provides means for offline authentication.
%% A maintenance user can login without LDAP authentication and is assigned a role through which the entire model can be accessed.
%% Only maintenance users can add MaintenanceUser MOs. In order to do so there may not be any normal users logged in.
%% The closeSession action of the MaintenanceUserSecurity MOC can be used to close any other open sessions.

-record(maintenanceUser, {maintenanceUserId,
                          subjectName,
                          userName,
                          password}).

-define(maintenanceUser_types,
        [{maintenanceUserId, string},
         {subjectName, string},
         {userName, string},
         {password, {struct,'EcimPassword'}}]).

-define(MaintenanceUser_restricted, [maintenanceUserId]).


%% -------------- CLASS MaintenanceUserSecurity -------------------------

%% Description:
%% This MO provides an action for shutting down all other sessions than maintenance user sessions.
%% This is necessary in order to configure attributes in this MO as well as adding or removing MaintenanceUser MOs.

-record(maintenanceUserSecurity, {maintenanceUserSecurityId,
                                  dummy}).

-define(maintenanceUserSecurity_types,
        [{maintenanceUserSecurityId, string},
         {dummy, atom}]).

-define(MaintenanceUserSecurity_restricted, [maintenanceUserSecurityId]).


%% ------------------ STRUCT EcimPassword ----------------------
-ifndef(_ECIM_PASSWORD).
-define(_ECIM_PASSWORD, 1).

-record('EcimPassword', {cleartext,
                         password}).

-define('EcimPassword_types',
        [{cleartext, 'RcsUser.EcimEmpty'},
         {password, string}]).


-endif. % _ECIM_PASSWORD


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

-hrl_id({"RcsLdapAuthentication","3.1.1","/main/R3A/R4A/R6A/R9A/R10A/1"}).


%% -------------- CLASS Filter -------------------------

%% Description:
%% Indicates the filter used to locate the user's authorization profile in the LDAP directory. 

-record(filter, {filterId,
                 filter,
                 type,
                 userLabel}).

-define(filter_types,
        [{filterId, string},
         {filter, string},
         {type, string},
         {userLabel, string}]).

-define(Filter_restricted, [filterId]).


%% -------------- CLASS Ldap -------------------------

%% Description:
%% Contains the configurable information on primary and secondary LDAP directories.  

-record(ldap, {ldapId,
               ldapIpAddress,
               fallbackLdapIpAddress,
               serverPort,
               bindDn,
               bindPassword,
               baseDn,
               useReferrals,
               profileFilter,
               nodeCredential,
               trustCategory,
               useTls,
               tlsMode,
               userLabel}).

-define(ldap_types,
        [{ldapId, string},
         {ldapIpAddress, 'RcsLdapAuthentication.IpDNSAddress'},
         {fallbackLdapIpAddress, 'RcsLdapAuthentication.IpDNSAddress'},
         {serverPort, uint16},
         {bindDn, 'RcsLdapAuthentication.LdapDistinguishedName'},
         {bindPassword, {struct,'EcimPassword'}},
         {baseDn, 'RcsLdapAuthentication.LdapDistinguishedName'},
         {useReferrals, boolean},
         {profileFilter, 'RcsLdapAuthentication.ProfileFilter'},
         {nodeCredential, moRef},
         {trustCategory, moRef},
         {useTls, boolean},
         {tlsMode, 'RcsLdapAuthentication.TlsMode'},
         {userLabel, string}]).

-define(ldap_useReferrals_default, false).
-define(ldap_tlsMode_default, 'STARTTLS').
-define(Ldap_restricted, [ldapId]).


%% -------------- CLASS LdapAuthenticationMethod -------------------------

%% Description:
%% Root MOC of the LDAP Authentication Method.

-record(ldapAuthenticationMethod, {ldapAuthenticationMethodId,
                                   administrativeState,
                                   userLabel}).

-define(ldapAuthenticationMethod_types,
        [{ldapAuthenticationMethodId, string},
         {administrativeState, 'RcsLdapAuthentication.BasicAdmState'},
         {userLabel, string}]).

-define(LdapAuthenticationMethod_restricted, [ldapAuthenticationMethodId]).


%% -------------- CLASS EricssonFilter -------------------------

%% Description:
%% Provides configuration of features supported by the Ericsson LDAP schema. 

-record(ericssonFilter, {ericssonFilterId,
                         roleAliasesBaseDn,
                         targetBasedAccessControl,
                         version}).

-define(ericssonFilter_types,
        [{ericssonFilterId, string},
         {roleAliasesBaseDn, 'RcsLdapAuthentication.LdapDistinguishedName'},
         {targetBasedAccessControl, 'RcsLdapAuthentication.BasicAdmState'},
         {version, 'RcsLdapAuthentication.EricssonFilterVersion'}]).

-define(ericssonFilter_targetBasedAccessControl_default, 'LOCKED').
-define(ericssonFilter_version_default, 2).
-define(EricssonFilter_restricted, [ericssonFilterId]).


%% ------------------ ENUM EricssonFilterVersion ----------------------
-ifndef('EricssonFilterVersion').
-define('EricssonFilterVersion', 1).

-define(EricssonFilterVersion_1, 1).
-define(EricssonFilterVersion_2, 2).

-endif. % EricssonFilterVersion

%% ------------------ ENUM TlsMode ----------------------
-ifndef('TlsMode').
-define('TlsMode', 1).

-define(TlsMode_STARTTLS, 0).
-define(TlsMode_LDAPS, 1).

-endif. % TlsMode

%% ------------------ ENUM BasicAdmState ----------------------
-ifndef('BasicAdmState').
-define('BasicAdmState', 1).

-define(BasicAdmState_LOCKED, 0).
-define(BasicAdmState_UNLOCKED, 1).

-endif. % BasicAdmState

%% ------------------ ENUM ProfileFilter ----------------------
-ifndef('ProfileFilter').
-define('ProfileFilter', 1).

-define(ProfileFilter_POSIX_GROUPS, 0).
-define(ProfileFilter_ERICSSON_FILTER, 1).
-define(ProfileFilter_FLEXIBLE, 2).

-endif. % ProfileFilter

%% ------------------ STRUCT EcimPassword ----------------------
-ifndef(_ECIM_PASSWORD).
-define(_ECIM_PASSWORD, 1).

-record('EcimPassword', {cleartext,
                         password}).

-define('EcimPassword_types',
        [{cleartext, 'RcsLdapAuthentication.EcimEmpty'},
         {password, string}]).


-endif. % _ECIM_PASSWORD


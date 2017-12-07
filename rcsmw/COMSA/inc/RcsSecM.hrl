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

-hrl_id({"RcsSecM","11.2.0","/main/R6A/3"}).


%% -------------- CLASS SecM -------------------------

%% Description:
%% Security Management root MOC.

-record(secM, {secMId,
               dummy}).

-define(secM_types,
        [{secMId, string},
         {dummy, atom}]).

-define(SecM_restricted, [secMId]).


%% -------------- CLASS AuthenticationOrder -------------------------

%% Description:
%% MO Class used to view the order of authentication methods.

-record(authenticationOrder, {authenticationOrderId,
                              authenticationMethodOrder,
                              userLabel}).

-define(authenticationOrder_types,
        [{authenticationOrderId, string},
         {authenticationMethodOrder, {sequence,{struct,'MethodOrder'}}},
         {userLabel, string}]).

-define(AuthenticationOrder_restricted, [authenticationOrderId]).


%% -------------- CLASS AuthorizationOrder -------------------------

%% Description:
%% MO Class used to view the order of authorization methods.

-record(authorizationOrder, {authorizationOrderId,
                             authorizationMethodOrder,
                             userLabel}).

-define(authorizationOrder_types,
        [{authorizationOrderId, string},
         {authorizationMethodOrder, {sequence,{struct,'MethodOrder'}}},
         {userLabel, string}]).

-define(AuthorizationOrder_restricted, [authorizationOrderId]).


%% -------------- CLASS UserManagement -------------------------

%% Description:
%% User Management MOC. 

-record(userManagement, {userManagementId,
                         legalNotice,
                         loginFailureDelay,
                         targetType,
                         userLabel}).

-define(userManagement_types,
        [{userManagementId, string},
         {legalNotice, string},
         {loginFailureDelay, 'RcsSecM.RcsSecM_UserManagement_loginFailureDelay'},
         {targetType, {sequence,string}},
         {userLabel, string}]).

-define(userManagement_legalNotice_default, "IF YOU ARE NOT AN AUTHORIZED USER, PLEASE EXIT IMMEDIATELY").
-define(userManagement_loginFailureDelay_default, 5).
-define(UserManagement_restricted, [userManagementId]).


%% -------------- CLASS Tls -------------------------

%% Description:
%% Configures system-wide properties of Transport Layer Security (TLS).

-record(tls, {tlsId,
              enabledCiphers,
              cipherFilter,
              supportedCiphers}).

-define(tls_types,
        [{tlsId, string},
         {enabledCiphers, {sequence,{struct,'Cipher'}}},
         {cipherFilter, 'RcsSecM.CipherList'},
         {supportedCiphers, {sequence,{struct,'Cipher'}}}]).

-define(tls_cipherFilter_default, "DEFAULT").
-define(Tls_restricted, [tlsId]).


%% -------------- CLASS Ssh -------------------------

%% Description:
%% Configures system-wide properties of Secure Shell Transport Layer Protocol (SSH).

-record(ssh, {sshId,
              selectedCiphers,
              supportedCiphers,
              selectedKeyExchanges,
              supportedKeyExchanges,
              selectedMacs,
              supportedMacs}).

-define(ssh_types,
        [{sshId, string},
         {selectedCiphers, {sequence,'RcsSecM.SshAlgorithm'}},
         {supportedCiphers, {sequence,'RcsSecM.SshAlgorithm'}},
         {selectedKeyExchanges, {sequence,'RcsSecM.SshAlgorithm'}},
         {supportedKeyExchanges, {sequence,'RcsSecM.SshAlgorithm'}},
         {selectedMacs, {sequence,'RcsSecM.SshAlgorithm'}},
         {supportedMacs, {sequence,'RcsSecM.SshAlgorithm'}}]).

-define(Ssh_restricted, [sshId]).


%% ------------------ STRUCT MethodOrder ----------------------
-ifndef(_METHOD_ORDER).
-define(_METHOD_ORDER, 1).

-record('MethodOrder', {orderNumber,
                        methodReference,
                        userLabel}).

-define('MethodOrder_types',
        [{orderNumber, int16},
         {methodReference, moRef},
         {userLabel, string}]).


-endif. % _METHOD_ORDER


%% ------------------ STRUCT Cipher ----------------------
-ifndef(_CIPHER).
-define(_CIPHER, 1).

-record('Cipher', {protocolVersion,
                   keyExchange,
                   authentication,
                   encryption,
                   mac,
                   export,
                   name}).

-define('Cipher_types',
        [{protocolVersion, string},
         {keyExchange, string},
         {authentication, string},
         {encryption, string},
         {mac, string},
         {export, string},
         {name, string}]).


-endif. % _CIPHER


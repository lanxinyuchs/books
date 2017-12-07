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

-hrl_id({"RcsSysM","1.0.3","/main/R5A/R10A/R11A/R12A/0"}).


%% -------------- CLASS SysM -------------------------

%% Description:
%% This is the System Management MO. 

-record(sysM, {sysMId,
               userLabel}).

-define(sysM_types,
        [{sysMId, string},
         {userLabel, string}]).

-define(SysM_restricted, [sysMId]).


%% -------------- CLASS NtpServer -------------------------

%% Description:
%% The NTP server configuration data.

-record(ntpServer, {ntpServerId,
                    userLabel,
                    serverAddress,
                    administrativeState}).

-define(ntpServer_types,
        [{ntpServerId, string},
         {userLabel, string},
         {serverAddress, 'RcsSysM.IpDNSAddress'},
         {administrativeState, 'RcsSysM.BasicAdmState'}]).

-define(NtpServer_restricted, [ntpServerId]).


%% -------------- CLASS SysMSchema -------------------------

%% Description:
%% Represents a MIM model fragment whose corresponding functionality is operational in the ME.

-record(sysMSchema, {schemaId,
                     identifier,
                     baseModelIdentifier,
                     version,
                     baseModelVersion,
                     selectedModelOptions}).

-define(sysMSchema_types,
        [{schemaId, string},
         {identifier, string},
         {baseModelIdentifier, string},
         {version, string},
         {baseModelVersion, string},
         {selectedModelOptions, {sequence,string}}]).

-define(SysMSchema_restricted, [schemaId]).


%% -------------- CLASS NetconfTls -------------------------

%% Description:
%% Represents the Netconf configuration management service over Transport Layer Security (TLS).

-record(netconfTls, {netconfTlsId,
                     nodeCredential,
                     trustCategory,
                     administrativeState,
                     port}).

-define(netconfTls_types,
        [{netconfTlsId, string},
         {nodeCredential, moRef},
         {trustCategory, moRef},
         {administrativeState, 'RcsSysM.BasicAdmState'},
         {port, uint16}]).

-define(NetconfTls_restricted, [netconfTlsId]).


%% -------------- CLASS NetconfSsh -------------------------

%% Description:
%% Represents the Netconf configuration management service over Secure Shell.

-record(netconfSsh, {netconfSshId,
                     administrativeState,
                     port}).

-define(netconfSsh_types,
        [{netconfSshId, string},
         {administrativeState, 'RcsSysM.BasicAdmState'},
         {port, uint16}]).

-define(netconfSsh_port_default, 830).
-define(NetconfSsh_restricted, [netconfSshId]).


%% -------------- CLASS CliSsh -------------------------

%% Description:
%% Represents the CLI configuration management service over Secure Shell.

-record(cliSsh, {cliSshId,
                 administrativeState,
                 port}).

-define(cliSsh_types,
        [{cliSshId, string},
         {administrativeState, 'RcsSysM.BasicAdmState'},
         {port, uint16}]).

-define(CliSsh_restricted, [cliSshId]).


%% -------------- CLASS CliTls -------------------------

%% Description:
%% Represents the CLI configuration management service over Transport Layer Security (TLS).

-record(cliTls, {cliTlsId,
                 nodeCredential,
                 trustCategory,
                 administrativeState,
                 port}).

-define(cliTls_types,
        [{cliTlsId, string},
         {nodeCredential, moRef},
         {trustCategory, moRef},
         {administrativeState, 'RcsSysM.BasicAdmState'},
         {port, uint16}]).

-define(CliTls_restricted, [cliTlsId]).


%% -------------- CLASS OamAccessPoint -------------------------

%% Description:
%% An OaM Access Point instance for the ME.

-record(oamAccessPoint, {oamAccessPointId,
                         accessPoint,
                         dscp,
                         ipv4address,
                         netconfPort,
                         sshPort}).

-define(oamAccessPoint_types,
        [{oamAccessPointId, string},
         {accessPoint, moRef},
         {dscp, 'RcsSysM.RcsSysM_OamAccessPoint_dscp'},
         {ipv4address, moRef},
         {netconfPort, uint16},
         {sshPort, uint16}]).

-define(oamAccessPoint_dscp_default, 0).
-define(OamAccessPoint_restricted, [oamAccessPointId]).


%% -------------- CLASS OamTrafficClass -------------------------

%% Description:
%% Defines the traffic class and priority for IP packets.

-record(oamTrafficClass, {oamTrafficClassId,
                          name,
                          dscp}).

-define(oamTrafficClass_types,
        [{oamTrafficClassId, string},
         {name, string},
         {dscp, 'RcsSysM.RcsSysM_OamTrafficClass_dscp'}]).

-define(OamTrafficClass_restricted, [oamTrafficClassId]).


%% ------------------ ENUM BasicAdmState ----------------------
-ifndef('BasicAdmState').
-define('BasicAdmState', 1).

-define(BasicAdmState_LOCKED, 0).
-define(BasicAdmState_UNLOCKED, 1).

-endif. % BasicAdmState

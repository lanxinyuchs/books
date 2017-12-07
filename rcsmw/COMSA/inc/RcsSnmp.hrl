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

-hrl_id({"RcsSnmp","10.12.0","/main/R5A/R7A/R11A/1"}).


%% -------------- CLASS Snmp -------------------------

%% Description:
%% Configuration of the SNMP protocol, for example IP addresses and ports.

-record(snmp, {snmpId,
               operationalState,
               administrativeState,
               agentAddress,
               nodeCredential,
               agentAddressDtls,
               trustCategory,
               enableSourceCheckForV1V2C,
               port,
               portDtls}).

-define(snmp_types,
        [{snmpId, string},
         {operationalState, 'RcsSnmp.OperState'},
         {administrativeState, 'RcsSnmp.BasicAdmState'},
         {agentAddress, {sequence,{struct,'HostAndPort'}}},
         {nodeCredential, moRef},
         {agentAddressDtls, {sequence,{struct,'HostAndPort'}}},
         {trustCategory, moRef},
         {enableSourceCheckForV1V2C, boolean},
         {port, uint16},
         {portDtls, uint16}]).

-define(snmp_administrativeState_default, 'UNLOCKED').
-define(snmp_enableSourceCheckForV1V2C_default, false).
-define(snmp_port_default, 161).
-define(snmp_portDtls_default, 10161).
-define(Snmp_restricted, [snmpId]).


%% -------------- CLASS SnmpTargetV1 -------------------------

%% Description:
%% An SNMP target defines a trap receiver and gives access privileges.
%% This class, defines V1 protocol attributes.

-record(snmpTargetV1, {snmpTargetV1Id,
                       community,
                       administrativeState,
                       address,
                       port,
                       isMibWritable,
                       operationalState,
                       networkPrefixLength}).

-define(snmpTargetV1_types,
        [{snmpTargetV1Id, string},
         {community, 'RcsSnmp.SnmpCommunity'},
         {administrativeState, 'RcsSnmp.BasicAdmState'},
         {address, 'RcsSnmp.IPAddress'},
         {port, 'RcsSnmp.RcsSnmp_SnmpTargetV1_port'},
         {isMibWritable, boolean},
         {operationalState, 'RcsSnmp.OperState'},
         {networkPrefixLength, 'RcsSnmp.RcsSnmp_SnmpTargetV1_networkPrefixLength'}]).

-define(snmpTargetV1_administrativeState_default, 'UNLOCKED').
-define(snmpTargetV1_port_default, 162).
-define(snmpTargetV1_isMibWritable_default, true).
-define(SnmpTargetV1_restricted, [snmpTargetV1Id]).


%% -------------- CLASS SnmpTargetV2C -------------------------

%% Description:
%% SNMP target defines a trap receiver and gives access privileges.
%% This class, defines V2C protocol attributes.

-record(snmpTargetV2C, {snmpTargetV2CId,
                        community,
                        informRetryCount,
                        address,
                        port,
                        informTimeout,
                        transportMethod,
                        isMibWritable,
                        operationalState,
                        administrativeState,
                        networkPrefixLength}).

-define(snmpTargetV2C_types,
        [{snmpTargetV2CId, string},
         {community, 'RcsSnmp.SnmpCommunity'},
         {informRetryCount, uint32},
         {address, 'RcsSnmp.IPAddress'},
         {port, 'RcsSnmp.RcsSnmp_SnmpTargetV2C_port'},
         {informTimeout, 'RcsSnmp.Timeout'},
         {transportMethod, 'RcsSnmp.NotifyType'},
         {isMibWritable, boolean},
         {operationalState, 'RcsSnmp.OperState'},
         {administrativeState, 'RcsSnmp.BasicAdmState'},
         {networkPrefixLength, 'RcsSnmp.RcsSnmp_SnmpTargetV2C_networkPrefixLength'}]).

-define(snmpTargetV2C_informRetryCount_default, 1).
-define(snmpTargetV2C_port_default, 162).
-define(snmpTargetV2C_informTimeout_default, 300).
-define(snmpTargetV2C_transportMethod_default, 'TRAP').
-define(snmpTargetV2C_isMibWritable_default, true).
-define(snmpTargetV2C_administrativeState_default, 'UNLOCKED').
-define(SnmpTargetV2C_restricted, [snmpTargetV2CId]).


%% -------------- CLASS SnmpTargetV3 -------------------------

%% Description:
%% SNMP target defines a trap receiver and gives access privileges.
%% This class, defines V3 USM protocol attributes.

-record(snmpTargetV3, {snmpTargetV3Id,
                       user,
                       authProtocol,
                       privProtocol,
                       informRetryCount,
                       administrativeState,
                       address,
                       port,
                       informTimeout,
                       transportMethod,
                       snmpSecurityLevel,
                       privKey,
                       authKey,
                       isMibWritable,
                       operationalState}).

-define(snmpTargetV3_types,
        [{snmpTargetV3Id, string},
         {user, 'RcsSnmp.SnmpSecurityName'},
         {authProtocol, 'RcsSnmp.AuthProtocol'},
         {privProtocol, 'RcsSnmp.PrivProtocol'},
         {informRetryCount, uint32},
         {administrativeState, 'RcsSnmp.BasicAdmState'},
         {address, 'RcsSnmp.IPAddress'},
         {port, 'RcsSnmp.RcsSnmp_SnmpTargetV3_port'},
         {informTimeout, 'RcsSnmp.Timeout'},
         {transportMethod, 'RcsSnmp.NotifyType'},
         {snmpSecurityLevel, 'RcsSnmp.SecurityLevel'},
         {privKey, {struct,'EcimPassword'}},
         {authKey, {struct,'EcimPassword'}},
         {isMibWritable, boolean},
         {operationalState, 'RcsSnmp.OperState'}]).

-define(snmpTargetV3_authProtocol_default, 'MD5').
-define(snmpTargetV3_privProtocol_default, 'DES').
-define(snmpTargetV3_informRetryCount_default, 1).
-define(snmpTargetV3_administrativeState_default, 'UNLOCKED').
-define(snmpTargetV3_port_default, 162).
-define(snmpTargetV3_informTimeout_default, 300).
-define(snmpTargetV3_transportMethod_default, 'TRAP').
-define(snmpTargetV3_snmpSecurityLevel_default, 'AUTH_PRIV').
-define(snmpTargetV3_isMibWritable_default, true).
-define(SnmpTargetV3_restricted, [snmpTargetV3Id]).


%% -------------- CLASS SnmpViewV1 -------------------------

%% Description:
%% SNMP view gives users access to SNMP MIBs.
%% This class explicitly defines a view for SNMP V1 users. Users are identified by community name.
%% Communities that are not configured in any view, are given by default access to the following SNMP MIBs:
%% - Ericsson Alarm MIB
%% - SNMP-FRAMEWORK-MIB
%% - MIB-2
%% Default status denies access to other SNMP MIBs. Create a new view configuration if default behaviour is insufficient.

-record(snmpViewV1, {snmpViewV1Id,
                     community,
                     readOids,
                     writeOids}).

-define(snmpViewV1_types,
        [{snmpViewV1Id, string},
         {community, {sequence,string}},
         {readOids, {sequence,'RcsSnmp.SnmpOid'}},
         {writeOids, {sequence,'RcsSnmp.SnmpOid'}}]).

-define(SnmpViewV1_restricted, [snmpViewV1Id]).


%% -------------- CLASS SnmpViewV2C -------------------------

%% Description:
%% SNMP view gives users access to SNMP MIBs.
%% This class explicitly defines a view for SNMP V2C users. Users are identified by community name.
%% Communities that are not configured in any view, are given by default access to the following SNMP MIBs:
%% - Ericsson Alarm MIB
%% - SNMP-FRAMEWORK-MIB
%% - MIB-2
%% Default status denies access to other SNMP MIBs. Create a new view configuration if default behaviour is insufficient.

-record(snmpViewV2C, {snmpViewV2CId,
                      community,
                      readOids,
                      writeOids}).

-define(snmpViewV2C_types,
        [{snmpViewV2CId, string},
         {community, {sequence,string}},
         {readOids, {sequence,'RcsSnmp.SnmpOid'}},
         {writeOids, {sequence,'RcsSnmp.SnmpOid'}}]).

-define(SnmpViewV2C_restricted, [snmpViewV2CId]).


%% -------------- CLASS SnmpViewV3 -------------------------

%% Description:
%% SNMP view gives users access to SNMP MIBs.
%% This class explicitly defines a view for SNMP V3 users. Users are identified by community name.
%% Communities that are not configured in any view, are given by default access to the following SNMP MIBs:
%% - Ericsson Alarm MIB
%% - SNMP-FRAMEWORK-MIB
%% - MIB-2
%% Default status denies access to other SNMP MIBs. Create a new view configuration if default behaviour is insufficient.

-record(snmpViewV3, {snmpViewV3Id,
                     user,
                     readOids,
                     writeOids}).

-define(snmpViewV3_types,
        [{snmpViewV3Id, string},
         {user, {sequence,string}},
         {readOids, {sequence,'RcsSnmp.SnmpOid'}},
         {writeOids, {sequence,'RcsSnmp.SnmpOid'}}]).

-define(SnmpViewV3_restricted, [snmpViewV3Id]).


%% -------------- CLASS SnmpTargetV3Dtls -------------------------

%% Description:
%% SNMP target defines a trap receiver and gives access privileges.
%% This class defines attributes needed for SNMP over DTLS. Attributes nodeCredential and trustCategory defined in the Snmp MO class/instance are used. DTLS is used as transport protocol for incoming requests, outgoing responses and SNMP notifications.

-record(snmpTargetV3Dtls, {snmpTargetV3DtlsId,
                           informRetryCount,
                           administrativeState,
                           address,
                           port,
                           informTimeout,
                           transportMethod,
                           isMibWritable,
                           operationalState,
                           user}).

-define(snmpTargetV3Dtls_types,
        [{snmpTargetV3DtlsId, string},
         {informRetryCount, uint32},
         {administrativeState, 'RcsSnmp.BasicAdmState'},
         {address, 'RcsSnmp.IPAddress'},
         {port, 'RcsSnmp.RcsSnmp_SnmpTargetV3Dtls_port'},
         {informTimeout, 'RcsSnmp.Timeout'},
         {transportMethod, 'RcsSnmp.NotifyType'},
         {isMibWritable, boolean},
         {operationalState, 'RcsSnmp.OperState'},
         {user, 'RcsSnmp.SnmpSecurityName'}]).

-define(snmpTargetV3Dtls_informRetryCount_default, 1).
-define(snmpTargetV3Dtls_administrativeState_default, 'UNLOCKED').
-define(snmpTargetV3Dtls_port_default, 10162).
-define(snmpTargetV3Dtls_informTimeout_default, 300).
-define(snmpTargetV3Dtls_transportMethod_default, 'TRAP').
-define(snmpTargetV3Dtls_isMibWritable_default, true).
-define(SnmpTargetV3Dtls_restricted, [snmpTargetV3DtlsId]).


%% ------------------ ENUM AuthProtocol ----------------------
-ifndef('AuthProtocol').
-define('AuthProtocol', 1).

-define(AuthProtocol_NONE, 0).
-define(AuthProtocol_MD5, 1).
-define(AuthProtocol_SHA1, 2).

-endif. % AuthProtocol

%% ------------------ ENUM SecurityLevel ----------------------
-ifndef('SecurityLevel').
-define('SecurityLevel', 1).

-define(SecurityLevel_NO_AUTH_NO_PRIV, 1).
-define(SecurityLevel_AUTH_NO_PRIV, 2).
-define(SecurityLevel_AUTH_PRIV, 3).

-endif. % SecurityLevel

%% ------------------ ENUM BasicAdmState ----------------------
-ifndef('BasicAdmState').
-define('BasicAdmState', 1).

-define(BasicAdmState_LOCKED, 0).
-define(BasicAdmState_UNLOCKED, 1).

-endif. % BasicAdmState

%% ------------------ ENUM NotifyType ----------------------
-ifndef('NotifyType').
-define('NotifyType', 1).

-define(NotifyType_TRAP, 1).
-define(NotifyType_INFORM, 2).

-endif. % NotifyType

%% ------------------ ENUM OperState ----------------------
-ifndef('OperState').
-define('OperState', 1).

-define(OperState_DISABLED, 0).
-define(OperState_ENABLED, 1).

-endif. % OperState

%% ------------------ ENUM PrivProtocol ----------------------
-ifndef('PrivProtocol').
-define('PrivProtocol', 1).

-define(PrivProtocol_NONE, 0).
-define(PrivProtocol_DES, 1).
-define(PrivProtocol_AES128, 2).

-endif. % PrivProtocol

%% ------------------ STRUCT HostAndPort ----------------------
-ifndef(_HOST_AND_PORT).
-define(_HOST_AND_PORT, 1).

-record('HostAndPort', {host,
                        port}).

-define('HostAndPort_types',
        [{host, 'RcsSnmp.IPAddress'},
         {port, 'RcsSnmp.RcsSnmp_HostAndPort_port'}]).


-endif. % _HOST_AND_PORT


%% ------------------ STRUCT EcimPassword ----------------------
-ifndef(_ECIM_PASSWORD).
-define(_ECIM_PASSWORD, 1).

-record('EcimPassword', {cleartext,
                         password}).

-define('EcimPassword_types',
        [{cleartext, 'RcsSnmp.EcimEmpty'},
         {password, string}]).


-endif. % _ECIM_PASSWORD


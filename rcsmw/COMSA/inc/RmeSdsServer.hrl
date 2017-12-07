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

-hrl_id({"RmeSdsServer","1.0.0","/main/R12A/1"}).


%% -------------- CLASS ServiceDiscoveryServer -------------------------

%% Description:
%% Configuration data the GSDS cluster members.
%% This MO must be created for the vSD to be operational.

-record(serviceDiscoveryServer, {serviceDiscoveryServerId,
                                 localAddress,
                                 cluster,
                                 gsdsStatus,
                                 members,
                                 trustCategory,
                                 nodeCredential}).

-define(serviceDiscoveryServer_types,
        [{serviceDiscoveryServerId, string},
         {localAddress, moRef},
         {cluster, {struct,'SdCluster'}},
         {gsdsStatus, 'RmeSdsServer.GsdsStatus'},
         {members, {sequence,{struct,'MemberStatus'}}},
         {trustCategory, moRef},
         {nodeCredential, moRef}]).

-define(ServiceDiscoveryServer_restricted, [serviceDiscoveryServerId]).


%% ------------------ ENUM OperState ----------------------
-ifndef('OperState').
-define('OperState', 1).

-define(OperState_DISABLED, 0).
-define(OperState_ENABLED, 1).

-endif. % OperState

%% ------------------ ENUM GsdsStatus ----------------------
-ifndef('GsdsStatus').
-define('GsdsStatus', 1).

-define(GsdsStatus_OK, 0).
-define(GsdsStatus_ERROR, 1).
-define(GsdsStatus_BUILDING, 2).
-define(GsdsStatus_DEGRADED, 3).

-endif. % GsdsStatus

%% ------------------ STRUCT SdCluster ----------------------
-ifndef(_SD_CLUSTER).
-define(_SD_CLUSTER, 1).

-record('SdCluster', {host,
                      port,
                      serviceArea}).

-define('SdCluster_types',
        [{host, 'RmeSdsServer.IpDNSAddress'},
         {port, uint32},
         {serviceArea, 'RmeSdsServer.RmeSdsServer_SdCluster_serviceArea'}]).


-endif. % _SD_CLUSTER


%% ------------------ STRUCT MemberStatus ----------------------
-ifndef(_MEMBER_STATUS).
-define(_MEMBER_STATUS, 1).

-record('MemberStatus', {address,
                         status}).

-define('MemberStatus_types',
        [{address, 'RmeSdsServer.IpDNSAddress'},
         {status, 'RmeSdsServer.OperState'}]).


-endif. % _MEMBER_STATUS


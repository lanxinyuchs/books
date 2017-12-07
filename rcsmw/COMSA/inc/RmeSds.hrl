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

-hrl_id({"RmeSds","2.0.1","/main/R8A/R9A/R12A/2"}).


%% -------------- CLASS ServiceDiscovery -------------------------

%% Description:
%% Configuration data for service discovery. 
%% This MO must be created for service discovery to be active in the node.

-record(serviceDiscovery, {serviceDiscoveryId,
                           gsdsAddress,
                           localAddress,
                           trustCategory,
                           nodeCredential,
                           datacenter}).

-define(serviceDiscovery_types,
        [{serviceDiscoveryId, string},
         {gsdsAddress, {struct,'HostAndPort'}},
         {localAddress, moRef},
         {trustCategory, moRef},
         {nodeCredential, moRef},
         {datacenter, string}]).

-define(serviceDiscovery_datacenter_default, "vrcs").
-define(ServiceDiscovery_restricted, [serviceDiscoveryId]).


%% ------------------ STRUCT HostAndPort ----------------------
-ifndef(_HOST_AND_PORT).
-define(_HOST_AND_PORT, 1).

-record('HostAndPort', {host,
                        port}).

-define('HostAndPort_types',
        [{host, 'RmeSds.IpDNSAddress'},
         {port, uint32}]).


-endif. % _HOST_AND_PORT


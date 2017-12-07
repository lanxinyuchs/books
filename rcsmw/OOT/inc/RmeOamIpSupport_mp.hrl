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

-hrl_id({"RmeOamIpSupport","1.0.0","/main/R9A/3"}).


%% -------------- CLASS OamIpSupport -------------------------

%% Description:
%% This MO class contains an attribute set by a management system to acknowledge successful setting of the IP address used for OaM access to a node.
%% The IP address is received by a management system via an SNMP trap eriChangeAddressEvent.

-record(oamIpSupport, {oamIpSupportId,
                       ipAddressChangeStatus}).

-define(oamIpSupport_types,
        [{oamIpSupportId, string},
         {ipAddressChangeStatus, int32}]).

-define(OamIpSupport_restricted, [oamIpSupportId]).


%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile: vnfcI.erl %
%%% Author:     etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(vnfcI).
-author(etxaldu).
-vsn('').
-date('2016-09-26').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% %CCaseCopyrightEnd%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%%            160926     etxaldu     Created
%%%            170203     etxberb     Added vnf_id/0.
%%%            170217     etxasta     Added vnfc_id/0.
%%%            171020     emariad     Added update_test_license
%%%            171024     etxberb     Added vnfd_id/0.
%%%            171114     etxaldu     Added is_heartbeat_rcvd/0.
%%%            171205     etxaldu     Change is_heartbeat_rcvd() so
%%%                                   that it reflects that the VNF
%%%                                   has received a heartbeat and
%%%                                   replied with 200 OK at least
%%%                                   once since the last vnfcs
%%%                                   process restart.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS 
%%% ----------------------------------------------------------
-export([restart_ind/1]).
-export([get_vnfm_server/0]).
-export([stop_heartbeat/0]).
-export([start_heartbeat/0]).
-export([send_heal/1]).
-export([send_heal/2]).
-export([is_standalone/0]).
-export([vnf_id/0]).
-export([vnfc_id/0]).
-export([vnfd_id/0]).
-export([update_test_license/1]).
-export([is_heartbeat_rcvd/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([debug/0]).


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc Restart completed
%%% Send indication to VNFM with restart type
%%% ===Arguments===
%%% RestartType - ?RESTART_USR|?RESTART_APP|?RESTART_OSMW
%%% @end
-spec restart_ind(RestartType :: atom()) -> ok |  {error,Reason::any()}.
restart_ind(RestartType) ->
    vnfcs:safe_call(restart_ind, [RestartType]).

%%% ----------------------------------------------------------
%%% @doc get_vnfm_server()
%%% Get the vnfm server data and VnfId
%%% Input: -
%%% Output: {IpAddress, Port, VnfId} | {error, Reason}
%%%      IpAddress: string()  e.g. "147.214.13.196"
%%%      Port:      integer()
%%%      VnfId:     string()  e.g. "92ba5b8e-8a74-11e6-ad11-fa163e13b2f0"
%%% @end
%%% ----------------------------------------------------------g
get_vnfm_server() ->
    vnfcs:get_vnfm_server().

%%% ----------------------------------------------------------
%%% @doc stop_heartbeat()
%%% Stop replying to the VNFM heartbeats.
%%% Called when the heal request fails, heartbeat replies
%%% resume after the next restart.
%%% Input: -
%%% Output: -
%%% @end
%%% ----------------------------------------------------------
stop_heartbeat() ->
    vnfcs:safe_call(stop_heartbeat, []).

%%% ----------------------------------------------------------
%%% @doc start_heartbeat()
%%% Start replying to the VNFM heartbeats.
%%% Called when ok to reply to heartbeats (e.g. database ready)
%%% Input: -
%%% Output: -
%%% @end
%%% ----------------------------------------------------------
start_heartbeat() ->
    vnfcs:safe_call(start_heartbeat, []).

%%% ----------------------------------------------------------
%%% @doc send_heal(Initiator)
%%% Send a heal request to the VNFM.
%%% Input: Initiator - atom() (calling ?MODULE)
%%% Output: ok | {error, Reason}
%%% @end
%%% ----------------------------------------------------------
send_heal(Initiator) ->
    vnfcc:send_heal(Initiator).

send_heal(Initiator,Reason) ->
    vnfcc:send_heal(Initiator,Reason).


%%% ----------------------------------------------------------
%%% @doc is_standalone()
%%% Check in the VNF is standalone (for legacy tests).
%%% (no VNF_ID env is present for standalone)
%%% Input: 
%%% Output: true|false
%%% @end
%%% ----------------------------------------------------------
is_standalone() ->
    vnfcs:is_standalone().

%% #############################################################################
%% @doc Get the value of VNF_ID.
%%
%% @end
%% ###=======================================================================###
-spec vnf_id() ->
    string() | false.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
vnf_id() ->
    os:getenv("VNF_ID").


%% #############################################################################
%% @doc Get the value of VNFC_ID.
%%
%% @end
%% ###=======================================================================###
-spec vnfc_id() ->
    string() | false.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
vnfc_id() ->
    os:getenv("VNFC_ID").

%% #############################################################################
%% @doc Get the value of VNFD_ID.
%%
%% @end
%% ###=======================================================================###
-spec vnfd_id() ->
    string() | false.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
vnfd_id() ->
    os:getenv("VNFD_ID").

%%% ----------------------------------------------------------
%%% @doc update_test_license(Loaded)
%%% Update LKF test license has been loaded.
%%% Input: Loaded - boolean( true|false) (
%%% Output: ok | {error, Reason}
%%% @end
%%% ----------------------------------------------------------
update_test_license(Loaded) ->
    vnfcSec:update_test_license(Loaded).


%%% ----------------------------------------------------------
%% @doc Check if the VNF has at anytime received a heartbeat and
%%      replied with 200 OK since the last vnfcs process restart.
%% @end
%%% ----------------------------------------------------------
-spec is_heartbeat_rcvd() ->
    boolean().
is_heartbeat_rcvd() ->
    vnfcs:is_heartbeat_rcvd().

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #       dbg_status()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Debug function show the vnfcs status
%%% ----------------------------------------------------------
debug() ->
    vnfcs:dbg_status(),
    vnfcc:dbg_status().
  

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_linx_handler.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_linx_handler).
-vsn('/main/R1A/R2A/1').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2013 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([parse_linx/1]).

-export([send_lici_init_cfm/2,
	 send_lici_init_sus/2,
	 send_lici_feature_subscr_cfm/2,
	 send_lici_feature_subscr_rej/2,
	 send_lici_feature_change_ind/2,
	 send_lici_capacity_subscr_cfm/2,
	 send_lici_capacity_subscr_rej/2,
	 send_lici_capacity_change_ind/2,
	 send_lici_status_subscr_cfm/2,
	 send_lici_status_subscr_rej/2,
	 send_lici_status_change_ind/2,
	 send_lici_is_lkf_installed_rsp/2
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici_sig.hrl").
-include("lih_lici.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

parse_linx({?OSA_LICI_INITIATE_SERVICE_REQ,
	    <<SignalRevision:4/native-unsigned-integer-unit:8,
	      PvFirstWanted:4/native-unsigned-integer-unit:8,
	      PvSecondWanted:4/native-unsigned-integer-unit:8,
	      PvThirdWanted:4/native-unsigned-integer-unit:8>>}) ->
    {lici_initiate_service_req, SignalRevision,
     PvFirstWanted, PvSecondWanted, PvThirdWanted};
parse_linx({?OSA_LICI_TERMINATE_SERVICE_REQ, <<>>}) ->
    lici_terminate_service_req;
parse_linx({?OSA_LICI_FEATURE_SUBSCRIBE_REQ, <<RawFeatureId:24/binary>>}) ->
    {lici_feature_subscribe_req, binary_to_list(RawFeatureId)};
parse_linx({?OSA_LICI_CAPACITY_SUBSCRIBE_REQ, <<RawCapacityId:24/binary>>}) ->
    {lici_capacity_subscribe_req, binary_to_list(RawCapacityId)};
parse_linx({?OSA_LICI_STATUS_SUBSCRIBE_REQ, <<>>}) ->
    lici_status_subscribe_req;
parse_linx({?OSA_LICI_IS_LKF_INSTALLED_REQ,
	    <<_SignalRevision:4/native-unsigned-integer-unit:8>>}) ->
    lici_is_lkf_installed_req;
parse_linx({?OSA_LICI_CLIENT_DOWN_IND, <<>>}) ->
    lici_client_down_ind;
parse_linx({_SigNo, _Data}) ->
    unknown_signal.

send_lici_init_cfm({Port, Spid}, {SignalRevision, SelectedPV}) ->
    Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
	     SelectedPV:4/native-unsigned-integer-unit:8>>,
    itc:send(Port, Spid, ?CELLO_LICI_INITIATE_SERVICE_CFM, Data).

send_lici_init_sus({Port, Spid}, SignalRevision) ->
    Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
	     ?CELLO_LICI_PV3:4/native-unsigned-integer-unit:8>>,
    itc:send(Port, Spid, ?CELLO_LICI_INITIATE_SERVICE_SUS, Data).

send_lici_feature_subscr_cfm({Port, Spid},
			     {FeatureId, FeatureStatus, ChangeReason}) ->
    Data =
	iolist_to_binary([lih_lib:pad(FeatureId),
			  <<FeatureStatus:4/native-unsigned-integer-unit:8,
			    ChangeReason:4/native-unsigned-integer-unit:8>>]),
    itc:send(Port, Spid, ?CELLO_LICI_FEATURE_SUBSCRIBE_CFM, Data).

send_lici_feature_subscr_rej({Port, Spid}, {FeatureId, RejectReason}) ->
    Data =
	iolist_to_binary([lih_lib:pad(FeatureId),
			  <<RejectReason:2/native-unsigned-integer-unit:8>>]),
    itc:send(Port, Spid, ?CELLO_LICI_FEATURE_SUBSCRIBE_REJ, Data).

send_lici_feature_change_ind({Port, Spid},
			     {FeatureId, FeatureStatus, ChangeReason}) ->
    Data =
	iolist_to_binary([lih_lib:pad(FeatureId),
			  <<FeatureStatus:4/native-unsigned-integer-unit:8,
			    ChangeReason:4/native-unsigned-integer-unit:8>>]),
    itc:send(Port, Spid, ?CELLO_LICI_FEATURE_CHANGE_IND, Data).

send_lici_capacity_subscr_cfm({Port, Spid},
			      {CapacityId, {NoLimit1, Value1},
			       {NoLimit2, Value2}, ChangeReason}) ->
    Data =
	iolist_to_binary([lih_lib:pad(CapacityId),
			  <<NoLimit1:4/native-unsigned-integer-unit:8,
			    Value1:4/native-unsigned-integer-unit:8,
			    NoLimit2:4/native-unsigned-integer-unit:8,
			    Value2:4/native-unsigned-integer-unit:8,
			    ChangeReason:4/native-unsigned-integer-unit:8>>]),
    itc:send(Port, Spid, ?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM, Data).

send_lici_capacity_subscr_rej({Port, Spid},
			      {CapacityId, RejectReason}) ->
    Data =
	iolist_to_binary([lih_lib:pad(CapacityId),
			  <<RejectReason:2/native-unsigned-integer-unit:8>>]),
    itc:send(Port, Spid, ?CELLO_LICI_CAPACITY_SUBSCRIBE_REJ, Data).

send_lici_capacity_change_ind({Port, Spid},
			      {CapacityId, {NoLimit1, Value1},
			       {NoLimit2, Value2}, ChangeReason}) ->
    Data =
	iolist_to_binary([lih_lib:pad(CapacityId),
			  <<NoLimit1:4/native-unsigned-integer-unit:8,
			    Value1:4/native-unsigned-integer-unit:8,
			    NoLimit2:4/native-unsigned-integer-unit:8,
			    Value2:4/native-unsigned-integer-unit:8,
			    ChangeReason:4/native-unsigned-integer-unit:8>>]),
    itc:send(Port, Spid, ?CELLO_LICI_CAPACITY_CHANGE_IND, Data).

send_lici_status_subscr_cfm({Port, Spid},
			    {LicMgrStatus, EmergencyCounter}) ->
    Data = <<LicMgrStatus:4/native-unsigned-integer-unit:8,
	     EmergencyCounter:4/native-unsigned-integer-unit:8>>,
    itc:send(Port, Spid, ?CELLO_LICI_STATUS_SUBSCRIBE_CFM, Data).

send_lici_status_subscr_rej({Port, Spid}, RejectReason) ->
    Data = <<RejectReason:2/native-unsigned-integer-unit:8>>,
    itc:send(Port, Spid, ?CELLO_LICI_STATUS_SUBSCRIBE_REJ, Data).

send_lici_status_change_ind({Port, Spid},
			    {LicMgrStatus, EmergencyCounter}) ->
    Data = <<LicMgrStatus:4/native-unsigned-integer-unit:8,
	     EmergencyCounter:4/native-unsigned-integer-unit:8>>,
    itc:send(Port, Spid, ?CELLO_LICI_STATUS_CHANGE_IND, Data).

send_lici_is_lkf_installed_rsp({Port, Spid}, {SignalRev, LFKResult}) ->
    Data = <<SignalRev:4/native-unsigned-integer-unit:8,
	     LFKResult:4/native-unsigned-integer-unit:8>>,
    itc:send(Port, Spid, ?CELLO_LICI_IS_LKF_INSTALLED_RSP, Data).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

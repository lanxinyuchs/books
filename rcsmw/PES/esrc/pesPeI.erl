%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesPeI.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2014
%%% @version /main/R3A/8
%%%
%%% @doc == PEI - Performance Management Event Interface ==
%%% This module implements the user API of the RBS CS Performance Event 
%%% Service.
%%% @end
%%% ----------------------------------------------------------
-module(pesPeI).
-vsn('/main/R3A/8').
-date('2014-12-01').
-author('uabesvi').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% R3A/1      2014-11-10 eolaand     Created
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API PMI
-export([peiInitialize/4,
	 peiInitialize/3,
	 peiInitialize/2,
	 peiInitialize/1,
	 peiFinalize/1]).

%% Default Callbacks
-export([peiEventJobCallback/8,
	 peiMEAttrUpdateCallback/3]).

%%-compile(export_all).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("pes.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(DEFAULT_CB_DATA, [{peiEventJobCallback, true}]).

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type event_map_id() :: string() | undefined.
%%% Identifier for the appData file containing definitions and aliases
%%% for the Event Types handled by this session.
-type callback_module() :: atom().
%%% The `CallbackModule' is a module implementing the callback function
%%% `peiEventJobCallback/3'.<br/>
%%% The default implementation of this function in `pesPeI' can be used 
%%% as a template for application specific implementations.<br/> 
%%% An application may also opt to use the default implementations directly 
%%% by specifying `pesPeI' as `CallbackModule'.<br/>
%%% See {@link peiEventJobCallback/3}.
-type event_type_alias()::integer().
%%% An integer used as a shorthand alias for a PM Event Type.
-type cb_func():: peiEventJobCallback | peiMEAttrUpdateCallback.
-type cb_data()::[{CbFunc::cb_func(), Bool::boolean()}].
-type file_control()::{ReportingPeriod::integer(), 
		       CompressionType::integer()}.
-type stream_control()::{CompressionType::integer(),
			 DestinationIpAddress::string(),
			 DestinationPort::integer()}.
-type filter_id()::{FilterName::string(), 
		    FilterValue::string()}.
%%% @end
%%%
%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc 
%%% Initialize a PM Event application instance.
%%% 
%%% The `AppPid' is the pid of an erlang process that must be kept alive 
%%% during the whole lifecycle from peiInitialize to peiFinalize.
%%% This pid is used by PES to identify the application instance.
%%% It is also used as the first parameter in all callback functions.
%%%        
%%% @end
%%% ----------------------------------------------------------
-spec peiInitialize(EventMapId::event_map_id(), 
		    CbData::cb_data(),
		    CallbackModule::callback_module(), 
		    AppPid::pid()) -> {ok, Handle::pid()} | 
				      {error, Reason::string()}.
peiInitialize(EventMapId, CbData, CallbackModule, AppPid) 
  when (is_list(EventMapId) orelse EventMapId == undefined) andalso
       is_list(CbData) andalso
       is_atom(CallbackModule) andalso
       is_pid(AppPid) -> 
    pesAppRegistry:initialize_pei(EventMapId, CbData, CallbackModule, AppPid).

%%% @equiv peiInitialize(EventMapId, CbData, CallbackModule, self())
-spec peiInitialize(EventMapId::event_map_id(), 
		    CbData::cb_data(), 
		    CallbackModule::callback_module()) -> 
    {ok, Handle::pid()} | {error, Reason::string()}.
peiInitialize(EventMapId, CbData, CallbackModule) -> 
    peiInitialize(EventMapId, CbData, CallbackModule, self()).

%%% @equiv peiInitialize(EventMapId, CbData, pmsPei)
-spec peiInitialize(EventMapId::event_map_id(), CbData::cb_data()) -> 
    {ok, Handle::pid()} | {error, Reason::string()}.
peiInitialize(EventMapId, CbData) -> 
    peiInitialize(EventMapId, CbData, ?MODULE).


%%% @equiv peiInitialize(EventMapId, [{peiEventJobCallback, true}])
-spec peiInitialize(EventMapId::event_map_id()) -> 
    {ok, Handle::pid()} | {error, Reason::string()}.
peiInitialize(EventMapId) -> 
    peiInitialize(EventMapId, ?DEFAULT_CB_DATA).


%%% ----------------------------------------------------------
%%% @doc 
%%% Finalize a PM Event application instance. The `Handle' is the same 
%%% that was returned from {@link peiInitialize/4}. 
%%% @end
%%% ----------------------------------------------------------
-spec peiFinalize(Handle::pid()) -> ok.
peiFinalize(Handle) when is_pid(Handle) ->
    pesAppRegistry:finalize_pei(Handle).

%%% ----------------------------------------------------------
%%% @doc 
%%% Default implementation of peiEventJobCallback. Sends an asynchronous 
%%% message to the AppPid provided in {@link peiInitialize/4}.
%%% @end
%%% ----------------------------------------------------------
-spec peiEventJobCallback(AppPid::pid(),
			  ProducerId::string(),
			  JobId::string(),
			  ReqJobState::integer(),
			  Types::[event_type_alias()],
			  FilterIds::[filter_id()],
			  FileCtrl::file_control(),
			  StreamCtrl::stream_control()) -> ok.
peiEventJobCallback(AppPid, ProducerId, JobId, ReqJobState, Types, FilterIds, 
		    FileCtrl, StreamCtrl) -> 
    AppPid ! {peiEventJob, {ProducerId, JobId, ReqJobState, Types, FilterIds, 
			    FileCtrl, StreamCtrl}},
    ok.

%%% ----------------------------------------------------------
%%% @doc 
%%% Default implementation of peiMEAttrUpdateCallback. Sends an asynchronous 
%%% message to the AppPid provided in {@link peiInitialize/4}.
%%% @end
%%% ----------------------------------------------------------
-spec peiMEAttrUpdateCallback(AppPid::pid(), 
			      UserlLabel::string() | undefined, 
			      LogicalName::string() | undefined) -> ok.
peiMEAttrUpdateCallback(AppPid, UserlLabel, LogicalName) ->
    AppPid ! {peiMEAttrUpdate, {UserlLabel, LogicalName}},
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

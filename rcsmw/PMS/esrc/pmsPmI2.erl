%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsPmI2.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R5A/R6A/1
%%%
%%% @doc == PMI - Performance Management Interface ==
%%% This module implements the user API of the RBS CS Performance Management 
%%% Service.
%%% @end
%%% ----------------------------------------------------------
-module(pmsPmI2).
-vsn('/main/R3A/R5A/R6A/1').
-date('2016-06-13').
-author('eolaand').
-shaid('d72ab94947b83b0675b19cea84fbe5442686de2a').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R3A/1      2014-09-04 uabesvi     Copied from pmsPmI
%%% R5A/1      2016-02-10 eolaand     Merge pmsAppJob2 into pmsAppJob
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API PMI
-export([pmi2Initialize/4,
	 pmi2Initialize/3,
	 pmi2Initialize/2,
	 pmi2Initialize/1,
	 pmi2CounterMap/2,
	 pmi2Finalize/1,
	 pmi2DataShowCounters/5,
	 pmi2DataRop/5]).

%% Default Callbacks
-export([pmi2SubscribeRopCallback/3,
	 pmi2ReportRopCallback/4,
	 pmi2ReportShowCountersCallback/5]).


%%-compile(export_all).
-include("pms.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type callback_module() :: atom().
%%% The `CallbackModule' is a module implementing the callback functions
%%% `pmi2SubscribeRopCallback/3', `pmi2ReportRopCallback/4' and 
%%% `pmi2ReportShowCountersCallback/4' .<br/>
%%% The default implementations of these functions in `pmsPmI2' can be used 
%%% as a template for application specific implementations.<br/> 
%%% An application may also opt to use the default implementations directly 
%%% by specifying `pmsPmI2' as `CallbackModule'.<br/>
%%% See {@link pmi2SubscribeRopCallback/3}, {@link pmi2ReportRopCallback/4} and
%%% {@link pmi2ReportShowCountersCallback/4}.
-type pm_group_alias()::integer().
-type measurement_type_alias()::integer().
-type mo_instance_alias()::integer().
-type pmi_callback() :: {pmi2SubscribeRopCallback, Bool::boolean()} |
			{pmi2ReportRopCallback, Bool::boolean()} |
			{pmi2ReportShowCountersCallback, Bool::boolean()}.
%% Default value for an unspecified callback is false
-type cb_data() :: [pmi_callback()].
-type mt_data() :: [{MeasurementTypeId::binary(), MeasurementTypeIdAlias::integer()}].
-type cm_data() :: [{GroupId::binary(), PmGroupIdAlias::integer(), MtData::mt_data()}].
-type subscribe_spec()::{PmGroupAlias::pm_group_alias(), 
			 [MeasurementTypeAlias::measurement_type_alias()]}.
-type show_counters_spec()::{PmGroupAlias::pm_group_alias(), 
			     [MeasurementTypeAlias::measurement_type_alias()],
			     [MoInstanceAlias::mo_instance_alias()]}.
-type meas_values()::[{MeasurementTypeAlias::measurement_type_alias(), 
		       [CounterValue::integer()]}].
-type value_bundle()::{PMGroupAlias::pm_group_alias(), MeasValues::meas_values()}. 
-type seconds() :: integer().
%%% An integer connecting a request to the session client with the
%%% reply from the session client.
-type report_id()::integer().
%%% This parameter specifies the deadline for measurement reporting related to 
%%% the current granularity period in seconds since Jan 1 1970 00:00:00.
-type max_report_time() :: seconds().
%%% This parameter specifies the timeout in seconds for reporting the result of
%%% the show counters command.
-type granularity_period() :: seconds().
%%% The granularity period is the time in seconds between the 
%%% initiation of two successive gatherings of measurement data.
%%% @end
%%%
%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc 
%%% Initialize a PM application instance.
%%% 
%%% The `AppPid' is the pid of an erlang process that must be kept alive 
%%% during the whole lifecycle from pmi2Initialize to pmi2Finalize.
%%% This pid is used by PMS to identify the application instance.
%%% It is also used as the first parameter in all callback functions.
%%%        
%%% @end
%%% ----------------------------------------------------------
-spec pmi2Initialize(CbData::cb_data(), 
		     CounterMap::string() | undefined,
		     CallbackModule::callback_module(), 
		     AppPid::pid()) -> {ok, Handle::pid()} | 
					   {error, Reason::string()}.
pmi2Initialize(CbData, CounterMap, CallbackModule, AppPid) 
  when is_list(CbData) andalso
       (is_list(CounterMap) orelse 
	CounterMap == undefined) andalso
       is_atom(CallbackModule) andalso
       is_pid(AppPid) -> 
    pmsAppRegistry:initialize_pmi2(CbData, CounterMap, CallbackModule, AppPid).

%%% @equiv pmi2Initialize(CbData, CounterMap, CallbackModule, self())
-spec pmi2Initialize(CbData::cb_data(),
		     CounterMap::string() | undefined,
                     CallbackModule::callback_module()) -> 
    {ok, Handle::pid()} | {error, Reason::string()}.
pmi2Initialize(CbData, CounterMap, CallbackModule) -> 
    pmi2Initialize(CbData, CounterMap, CallbackModule, self()).

%%% @equiv pmi2Initialize(CbData, CounterMap, pmsPmI2)
-spec pmi2Initialize(CbData::cb_data(), 
		     CounterMap::string() | undefined) -> 
    {ok, Handle::pid()} | {error, Reason::string()}.
pmi2Initialize(CbData, CounterMap) -> 
    pmi2Initialize(CbData, CounterMap, ?MODULE).

%%% @equiv pmi2Initialize(CbData, undefined)
-spec pmi2Initialize(CbData::cb_data()) -> 
    {ok, Handle::pid()} | {error, Reason::string()}.
pmi2Initialize(CbData) -> 
    pmi2Initialize(CbData, undefined).

%%% ----------------------------------------------------------
%%% @doc 
%%% Initialize Counters that this session handles.
%%% 
%%% 'Handle' is the handle received from pmi2Initialize.       
%%%        
%%% CounterMaps contains the PmGroups (and their aliases), and
%%% also the MeasurementTypes (and their aliases) that the
%%% session is responsible for.
%%% The aliases will be used in the further interaction in
%%% the session.
%%%
%%% The session client may send several counter map messages,
%%% thus adding new PmGroups/MeasurementTypes.
%%% @end
%%% ----------------------------------------------------------
-spec pmi2CounterMap(Handle::pid(), CounterMaps::cm_data()) -> 
    ok | {error, Reason::string()}.
pmi2CounterMap(Handle, CounterMaps) -> 
    BinCounterMaps = [{to_binary(G), GA, 
		       [{to_binary(MT), MTA} || {MT, MTA} <- MTs]} ||
			 {G, GA, MTs} <- CounterMaps],
    ok = pmsAppJob:pmi2_counter_map(Handle, BinCounterMaps).


%%% ----------------------------------------------------------
%%% @doc Finalize a PM application instance. The `Handle' is the same 
%%%      that was returned from {@link pmi2Initialize/3}. 
%%% @end
%%% ----------------------------------------------------------
-spec pmi2Finalize(Handle::pid()) -> ok.
pmi2Finalize(Handle) when is_pid(Handle) ->
    pmsAppRegistry:finalize_pmi2(Handle).

%%% ----------------------------------------------------------
%%% @doc Send PM data to PMS.
%%% @end
%%% ----------------------------------------------------------
-spec pmi2DataRop(Handle::pid(), 
		  GranularityPeriod::granularity_period(), 
		  ReportId::report_id(), 
		  ValueBundle::[value_bundle()],
		  FinalFragment::boolean()) -> ok.
pmi2DataRop(Handle, GranularityPeriod, ReportId, ValueBundle, FinalFragment)  
  when is_pid(Handle),
       is_integer(GranularityPeriod),
       is_integer(ReportId),
       is_list(ValueBundle),
       is_boolean(FinalFragment) ->

    pmsAppJob:pmi2_data(Handle, 
			 GranularityPeriod, 
			 ReportId, 
			 ValueBundle,
			 FinalFragment).

%%% ----------------------------------------------------------
%%% @doc Send Show Counters data to PMS.
%%% @end
%%% ----------------------------------------------------------
-spec pmi2DataShowCounters(Handle::pid(), 
			   ReportId::report_id(),
			   Result::integer(),
			   ErrorStr::string(),
			   MeasValues::[meas_values()]) -> ok.
pmi2DataShowCounters(Handle, ReportId, Result, ErrorStr, MeasValues) 
  when is_pid(Handle), is_integer(ReportId), is_list(MeasValues) ->
    pmsAppJob:pmi2_data_show_counters(Handle, 
				      ReportId, 
				      Result, 
				      ErrorStr,
				      MeasValues).

%%% ----------------------------------------------------------
%%% @doc Default implementation of pmi2SubscribeCallback. Sends an asynchronous 
%%%      message to the AppPid provided in {@link pmi2Initialize/3}.
%%% @end
%%% ----------------------------------------------------------
-spec pmi2SubscribeRopCallback(AppPid::pid(),
			       GranularityPeriod::granularity_period(), 
			       SubscribeSpecs::[subscribe_spec()]) -> ok.
pmi2SubscribeRopCallback(AppPid, GranularityPeriod, SubscribeSpecs) -> 
    AppPid ! {pmi2SubscribeRop, {GranularityPeriod, SubscribeSpecs}},
    ok.


%%% ----------------------------------------------------------
%%% @doc Default implementation of pmi2ReportRopCallback. Sends an asynchronous 
%%%      message to the AppPid provided in {@link pmi2Initialize/3}.
%%% @end
%%% ----------------------------------------------------------
-spec pmi2ReportRopCallback(AppPid::pid(), 
			    GranularityPeriod::granularity_period(), 
			    ReportId::integer(), 
			    Timeout::max_report_time()) -> ok.
pmi2ReportRopCallback(AppPid, GranularityPeriod, ReportId, Timeout) ->    
    AppPid ! {pmi2ReportRop, {GranularityPeriod, ReportId, Timeout}},
    ok.


%%% ----------------------------------------------------------
%%% @doc Default implementation of pmi2ReportShowCountersCallback. 
%%%      Sends an asynchronous message to the AppPid provided in 
%%%      {@link pmi2Initialize/3}.
%%% @end
%%% ----------------------------------------------------------
-spec pmi2ReportShowCountersCallback(AppPid::pid(),
				     ReportId::report_id(), 
				     MeasObjLDNAlias::integer(), 
				     ShowCountersSpecs::[show_counters_spec()],
				     Timeout::max_report_time()) -> ok.
pmi2ReportShowCountersCallback(AppPid, 
			       ReportId, 
			       MeasObjLDNAlias, 
			       ShowCountersSpecs,
			       Timeout) ->
    AppPid ! {pmi2ReportShowCounters, {ReportId, 
				       MeasObjLDNAlias, 
				       ShowCountersSpecs,
				       Timeout}},
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
to_binary(Term) ->
    pmsLib:to_binary(Term).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

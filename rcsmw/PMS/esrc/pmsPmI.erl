%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsPmI.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R5A/R6A/1
%%%
%%% @doc == PMI - Performance Management Interface ==
%%% This module implements the user API of the RBS CS Performance Management 
%%% Service.
%%% @end
%%% ----------------------------------------------------------
-module(pmsPmI).
-vsn('/main/R2A/R3A/R5A/R6A/1').
-date('2016-06-13').
-author('eolaand').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/1      2013-02-05 eolaand     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API PMI
-export([pmiInitialize/3,
	 pmiInitialize/2,
	 pmiInitialize/1,
	 pmiFinalize/1,
	 pmiDataShowCounters/5,
	 pmiData/5]).

%% Default Callbacks
-export([pmiSubscribeCallback/3,
	 pmiReportCallback/4,
	 pmiReportShowCountersCallback/4]).

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
-type measurement_type()::string().
-type callback_module() :: atom().
%%% The `CallbackModule' is a module implementing the callback functions
%%% `pmiSubscribeCallback/3', `pmiReportCallback/4' and 
%%% `pmiReportShowCountersCallback/4' .<br/>
%%% The default implementations of these functions in `pmsPmI' can be used 
%%% as a template for application specific implementations.<br/> 
%%% An application may also opt to use the default implementations directly 
%%% by specifying `pmsPmI' as `CallbackModule'.<br/>
%%% See {@link pmiSubscribeCallback/3}, {@link pmiReportCallback/4} and
%%% {@link pmiReportShowCountersCallback/4}.
-type pm_group()::string().
-type reg_data() :: [PMGroup::pm_group()] | 
		    [{TopLDN::string(), [PMGroup::pm_group()]}].
-type counter_spec()::{PMgroup::pm_group(), 
		       [MeasurementType::measurement_type()]}.
-type meas_values()::[{MeasurementType::measurement_type(), 
			 [CounterValue::integer()]}].
-type value_bundle()::[{PMGroup::pm_group(), MeasValues::meas_values()}]. 
-type seconds() :: integer().
-type request_id()::integer().
-type meas_timestamp() :: seconds().
%%% The measurement timestamp identifies the current granularity period.
-type deadline() :: seconds().
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
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc 
%%% Initialize a PM application instance.
%%% 
%%% The `AppPid' is the pid of an erlang process that must be kept alive 
%%% during the whole lifecycle from pmiInitialize to pmifinalize.
%%% This pid is used by PMS to identify the application instance.
%%% It is also used as the first parameter in all callback functions.
%%%        
%%% @end
%%% ----------------------------------------------------------
-spec pmiInitialize(RegData::reg_data(), CallbackModule::callback_module(), 
		    AppPid::pid()) -> {ok, Handle::pid()} | 
				      {error, Reason::string()}.
pmiInitialize(RegData, CallbackModule, AppPid) 
  when is_list(RegData),
       is_atom(CallbackModule), 
       is_pid(AppPid) -> 
    pmsAppRegistry:initialize(RegData, CallbackModule, AppPid).

%%% @equiv pmiInitialize(RegData, CallbackModule, self())
-spec pmiInitialize(RegData::reg_data(), CallbackModule::callback_module()) -> 
    {ok, Handle::pid()} | 
	{error, Reason::string()}.
pmiInitialize(RegData, CallbackModule) -> 
    pmiInitialize(RegData, CallbackModule, self()).

%%% @equiv pmiInitialize(RegData, pmsPmI)
-spec pmiInitialize(RegData::reg_data()) -> 
    {ok, Handle::pid()} | 
	{error, Reason::string()}.
pmiInitialize(RegData) -> 
    pmiInitialize(RegData, ?MODULE).


%%% ----------------------------------------------------------
%%% @doc Finalize a PM application instance. The `Handle' is the same 
%%%      that was returned from {@link pmiInitialize/3}. 
%%% @end
%%% ----------------------------------------------------------
-spec pmiFinalize(Handle::pid()) -> ok.
pmiFinalize(Handle) when is_pid(Handle) ->
    pmsAppRegistry:finalize(Handle).

%%% ----------------------------------------------------------
%%% @doc Send PM data to PMS.
%%% @end
%%% ----------------------------------------------------------
-spec pmiData(Handle::pid(), GranularityPeriod::granularity_period(), 
	      MeasTimestamp::meas_timestamp(), MeasObjLDN::string(), 
	      ValueBundle::value_bundle()) -> ok.
pmiData(Handle, GranularityPeriod, MeasTimestamp, MeasObjLDN, 
	ValueBundle) when is_pid(Handle),
			  is_integer(GranularityPeriod),
			  is_integer(MeasTimestamp),
			  is_list(MeasObjLDN),
			  is_list(ValueBundle) ->
    case verify_value_bundle(ValueBundle) of
	NewVB when is_list(NewVB) ->
	    pmsAppJob:pmi_data(Handle, GranularityPeriod, MeasTimestamp, 
			       to_binary(MeasObjLDN), NewVB);
	Error ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {" ERROR: illegal pmData received ~p~n", [ValueBundle]}),
	    Error
    end.


%%% ----------------------------------------------------------
%%% @doc Send Show Counters data to PMS.
%%% @end
%%% ----------------------------------------------------------
-spec pmiDataShowCounters(Handle::pid(), 
			  ReqId::request_id(),
			  Result::integer(),
			  ErrorStr::string(),
			  MeasValues::[meas_values()]) -> ok.
pmiDataShowCounters(Handle, ReqId, Result, ErrorStr, MeasValues) 
  when is_pid(Handle), is_integer(ReqId), is_list(MeasValues) ->
    case verify_meas_values(MeasValues) of
	NewVals when is_list(NewVals) ->
	    pmsAppJob:pmi_data_show_counters(Handle, 
					     ReqId, 
					     Result, 
					     ErrorStr,
					     NewVals);
	Error ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {" ERROR: illegal pmData received ~p~n", [MeasValues]}),
	    Error
    end.


%%% ----------------------------------------------------------
%%% @doc Default implementation of pmiSubscribeCallback. Sends an asynchronous 
%%%      message to the AppPid provided in {@link pmiInitialize/3}.
%%% @end
%%% ----------------------------------------------------------
-spec pmiSubscribeCallback(AppPid::pid(),
			   GranularityPeriod::granularity_period(), 
			   CounterSpecs::[counter_spec()]) -> ok.
pmiSubscribeCallback(AppPid, GranularityPeriod, CounterSpecs) -> 
    NewSpec = [{pmsLib:to_list(Group), [pmsLib:to_list(MT) || MT <- MTs]} ||
		  {Group, MTs} <- CounterSpecs],
    AppPid ! {pmiSubscribe, {GranularityPeriod, NewSpec}},
    ok.


%%% ----------------------------------------------------------
%%% @doc Default implementation of pmiReportCallback. Sends an asynchronous 
%%%      message to the AppPid provided in {@link pmiInitialize/3}.
%%% @end
%%% ----------------------------------------------------------
-spec pmiReportCallback(AppPid::pid(), GranularityPeriod::granularity_period(), 
			MeasTimestamp::meas_timestamp(), 
			DeadLine::deadline()) -> ok.
pmiReportCallback(AppPid, GranularityPeriod, MeasTimestamp, DeadLine) ->    
    AppPid ! {pmiReport, {GranularityPeriod, MeasTimestamp, DeadLine}},
    ok.


%%% ----------------------------------------------------------
%%% @doc Default implementation of pmiReportShowCountersCallback. 
%%%      Sends an asynchronous message to the AppPid provided in 
%%%      {@link pmiInitialize/3}.
%%% @end
%%% ----------------------------------------------------------
-spec pmiReportShowCountersCallback(AppPid::pid(),
				    ReqId::request_id(), 
				    MeasObjLDN::string(), 
				    Timeout::max_report_time()) -> ok.
pmiReportShowCountersCallback(AppPid, ReqId, MeasObjLDN, Timeout) ->    
    AppPid ! {pmiReportShowCounters, {ReqId, to_list(MeasObjLDN), Timeout}},
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
verify_meas_values(MeasValues) ->
    try
	[{to_binary(verify_integer_list(MT)), 
	  verify_integer_list(Vals)} || 
	    {MT, Vals} <- MeasValues]
    catch
	_:_ ->
	    {error, invalid_parameter}
    end.
	    

verify_value_bundle(ValueBundle) ->
    try
	[{to_binary(verify_integer_list(G)), 
	  [{to_binary(verify_integer_list(MT)), 
	    verify_integer_list(Vals)} || 
	      {MT, Vals} <- MTVs]}
	 || {G, MTVs} <- ValueBundle]
    catch
	_:_ ->
	    {error, invalid_parameter}
    end.
	    

verify_integer_list(IL) when IL =/= [] ->		
    [true = is_integer(N) || N <- IL],
    IL.


to_binary(Term) ->
    pmsLib:to_binary(Term).


to_list(Term) ->
    pmsLib:to_list(Term).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

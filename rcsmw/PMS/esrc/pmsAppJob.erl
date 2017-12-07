%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsAppJob.erl %
%%% @private
%%% Description:
%%%
%%% pmsAppJob initializes the measurement data collection 
%%% from the applications by sending the report message.
%%% There will be one report message request for each granularity period. 
%%% 
%%% To send the messages pmsAppJob uses a pmsSession process.
%%% A pmsSession process is started when an application attaches.
%%% The pmsSession process contacts pmsAppRegistry and informs which
%%% counters it handles. pmsAppRegistry spawns a pmsAppJob process
%%% and stores the counter data and the 2 pids in a table.
%%% 
%%% The pmsJob searches that table to find all applications handling
%%% the requested counters to send the subscribe messages.
%%% 
%%% pmsAppJob will calculate a union of all counters in all subsrcibe 
%%% requests and send a new subscribe request to the application as soon as
%%% there are any changes. 
%%% ----------------------------------------------------------
-module(pmsAppJob).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/3').
-date('2017-11-30').
-author('eolaand').
-shaid('73cf5c3e1fdd44096c461abc9335a370cb53b302').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% -----------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% -----------------------------------------------------------------------
%%% Rev        Date          Name        What
%%% -----      ----------    --------    ----------------------------------
%%% R1A/1      2013-01-18    uabesvi     Created
%%% R1A/4      2013-02-01    uabesvi     added subsribe and report
%%% R5A/3      2016-02-01    uabesvi     HU52245 time accuracy
%%% R5A/5      2016-02-10    eolaand     Merge pmsAppJob2 into pmsAppJob
%%% R9A/2      2017-02-06    uabesvi     Added CEC proc to log
%%% R9A/3      2017-03-21    eolaand     Change abstraction level of PMI/PMI2
%%%                                      to limit knowledge to pmsAppJob also
%%%                                      for showCounters.
%%% -----------------------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([pmi_start/3]).
-export([pmi2_start/5]).
-export([stop/1]).
-export([pmi_data/5]).
-export([pmi_data_show_counters/5]).
-export([pmi2_counter_map/2]).
-export([pmi2_data/5]).
-export([pmi2_data_show_counters/5]).
-export([subscribe/5]).
-export([unsubscribe/2]).
-export([show_counters/3]).
-export([show_counters/4]).

-export([cec_proc_name/2]).

%% test functions
-export([get_proc_info/1]).
-export([get_aliases/2]).
-export([gld/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% -export([init/7]).

%% -compile(export_all).

-include("RcsPm.hrl").
-include("pms.hrl").


-define(START_TO, 10000).
-define(SEND_TO, 5000).
-define(ET, pmsLib:epoch_time()).

-define(GP_SYNCED, gp_synced_app_job).
-define(ROP_DATA_TO, rop_data_to).
-define(MAX_REQ_ID, 16#FFFFFFFF).

-define(SKEY, #subscriber.key).

-define(DEFAULT_BL_TO, 10).
-define(LONG_BL_TO, 30).

-define(NO_REQ, {undefined, undefined}).

-define(DELAY_TIME, 5000).
-define(MIN_DELAY_SLICE, 10).
-define(MAX_DELAY_SLICE, 100).
-define(MIN_REM_TIME, 25000).

%%========================================================================
%% loop record
%% 
%% Subscribers = [#subscriber, 
%% 		   {key, 
%% 		    meas_tab,  
%% 		    cspec_add_prev,
%% 		    cspec_add_curr,
%% 		    cspec_mod_curr,
%% 		    comp_gp_prev, 
%% 		    comp_gp_curr, 
%% 		    gp_ff_sent,
%% 		    last_gp}]
%%
%%   key          = {JobPid, GP},
%%   meas_tab     = Tab - pmsJob ets table containing subscribed PmGroups and 
%%                        measurementTypes
%%   cspec_add_prev = [{PmGroup, [MT]}], - Added during previous GP
%%   cspec_add_curr = [{PmGroup, [MT]}], - Added during current GP
%%   cspec_mod_curr = [{add, [{PmGroup, [MT]}]}, - Changes during current GP
%%                     {rm, [{PmGroup, [MT]}]}],
%%   comp_gp_prev   = bool() - true if the measurement data for the previous GP 
%%                             is to be written to ROP
%%   comp_gp_curr   = bool() - true if the measurement data for the current GP 
%%                             is to be written to ROP
%%   gp_ff_sent     = bool() - true if the final fragment for the current GP 
%%                             has been sent
%%   last_gp        = bool() - True if 
%%
%% The following picture explains how CompleteGP works: 
%%   The numbers are wall clock time for GP = 15 min and 
%%   maximum reporting time is 4 minutes. 
%%
%%   - at 00, 15, 30, and 45 report rop request is sent to the application
%%     these occations are indicated with the GP_SYNCED message
%%   - 04, 19, 34, and 49 is the latest the application may reply  
%%     these occations are indicated with the ROP_DATA_TO message
%%
%% The measurement data is to be written to ROP only if the counters have
%% been active the whole GP, i.e. 00-15, 15-30, 30-45, or 45-00
%%
%%        00   04       15   19       30   34       45   49       00
%%    ------------------------------------------------------------------------>
%%
%%  - when pmsJob invokes subscribe/3 both CurrentGP and PreviousGP 
%%    are set to false
%%  - at 00/15/30/45 for currently active jobs:
%%      - the value of CurrentGP is copied to PreviousGP 
%%      - CurrentGP is set to true
%%  - if a pmsJob is stopped the currentGP is set to false
%%  - if a stopped pmsJob is set to active, the flags are not changed
%%  - when an application returns measurement data and final flag (FF) is false
%%    - if PreviousGP = true   the data is sent to pmsJob with FF not set
%%    - if PreviousGP = false  the data is ignored
%%  - when an application returns measurement data with FF set:
%%    - if PreviousGP = true   the data is sent to pmsJob with FF set
%%    - if PreviousGP = false  an empty measurement data is sent 
%%                             to pmsJob with FF set to true
%%  - if an application has not replied with FF before ROP_DATA_TO, 
%%    an empty measurement data is sent to pmsJob with FF set
%% 
%%========================================================================
-record(loop, {protocol,
	       pm_groups    = [],  %% [PmGroupId]
	       callbacks,
	       cb_mod,
	       session_pid,
	       subscribers  = [],  %% see description above
	       measurements = [],  %% [{GP, [{Grp, MeasType}]}]
	       gp_timers    = [],  %% [{GP, TimerRef}]
	       gp_end_time  = [],
	       req_id       = 0,
	       sc_reqs      = [],
	       counter_map  = true, %% true if accept counter map calls
	       alias_tab,
	       proc_type    = ?PROC_TYPE_UNDEFINED,
	       cec_proc_name
	      }).

-record(subscriber, {key,
		     job_id,
		     meas_tab,
		     cspec_add_prev = [],
		     cspec_add_curr = [],
		     cspec_mod_curr = [],
		     comp_gp_prev   = false, 
		     comp_gp_curr   = false, 
		     gp_ff_sent     = false,
		     last_gp        = false}).


%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% pmi_start(PmGroups, PmiCb, SessionPid) -> {ok, Pid} | {error, Reason}
%%
%% Spawn a new AppJob process as a result of a pmiImitialize from the 
%% application.
%%
%% The AppJob will then... 
%%
%%========================================================================
pmi_start(PmGroups, PmiCb, SessionPid) ->
    start(pmi, {[], PmiCb, PmGroups, undefined, SessionPid}).


%%========================================================================
%% pmi_data(AppJobPid, GP, TimeSpec, MeasObjLDN, ValueBundle) -> ok.
%%
%%========================================================================
pmi_data(AppJobPid, GP, TimeSpec, MeasObjLDN, ValueBundle) ->
    VB = [{Group, [{MeasObjLDN, MeasVals}]} || 
	     {Group, MeasVals} <- ValueBundle],
    MeasData = {GP, TimeSpec, VB, VB =:= []},
    send_data(AppJobPid, MeasData).


%%========================================================================
%% pmi_data_show_counters(AppJobPid, ReqId, Result, MeasValues) -> ok.
%%
%%========================================================================
pmi_data_show_counters(AppJobPid, ReqId, Result, ErrorStr, MeasValues) ->
    AppJobPid ! {data_sc, {ReqId, Result, ErrorStr, MeasValues}},
    ok.


%%========================================================================
%% pmi2_start(Callbacks, PmiCb, PmGroups, CounterMap, SessionPid) -> 
%%    {ok, Pid} | {error, Reason}
%%
%% Spawn a new AppJob process as a result of a pmi2Imitialize from the 
%% application.
%%
%% The AppJob will then... 
%%
%%========================================================================
pmi2_start(Callbacks, PmiCb, PmGroups, CounterMap, SessionPid) ->
    start(pmi2, {Callbacks, PmiCb, PmGroups, CounterMap, SessionPid}).
 

%%========================================================================
%% pmi2_counter_map(Pid, CounterMaps) -> ok
%%
%% pmsJob sends a subscribe request. If the request changes the union
%% of all counters in all subscribe requests then a new subscribe message
%% is sent to the application. Note, that the union is counted per 
%% granularity period.
%%========================================================================
pmi2_counter_map(AppJobPid, CounterMaps) ->
    AppJobPid ! {counter_map, CounterMaps},
    ok.
    

%%========================================================================
%% pmi2_data(AppJobPid, GP, TimeSpec, MeasObjLDN, ValueBundle) -> ok.
%%
%%========================================================================
pmi2_data(AppJobPid, GP, ReportId, ValueBundle, FinalFragment) ->
    MeasData = {GP, ReportId, ValueBundle, FinalFragment},
    send_data(AppJobPid, MeasData).


%%========================================================================
%% pmi2_data_show_counters(AppJobPid, ReqId, Result, MeasValues) -> ok.
%%
%%========================================================================
pmi2_data_show_counters(AppJobPid, ReqId, Result, ErrorStr, MeasValues) ->
    AppJobPid ! {data_sc, {ReqId, Result, ErrorStr, MeasValues}},
    ok.


%%========================================================================
%% subscribe(Pid, GP, Modifications, MeasTab) -> ok
%%
%% pmsJob sends a subscribe request. If the request changes the union
%% of all counters in all subscribe requests then a new subscribe message
%% is sent to the application. Note, that the union is counted per 
%% granularity period.
%%========================================================================
subscribe(AppJobPid, JobId, GP, Modifications, MeasTab) ->
    AppJobPid ! {subscribe, JobId, self(), GP, Modifications, MeasTab},
    ok.

%%========================================================================
%% unsubscribe(Pid, GP) -> ok
%%
%% pmsJob sends a request to stop subscribing to any previously subscribed
%% counters.
%% If the request changes the union of all counters in all subscribe requests 
%% then a new subscribe message
%% is sent to the application. Note, that the union is counted per 
%% granularity period.
%%========================================================================
unsubscribe(AppJobPid, GP) ->
    AppJobPid ! {unsubscribe, self(), GP},
    ok.

%%========================================================================
%% show_counters(Pid, LDN, MaxReportTime) -> ok
%%
%% COM sends a show counters request. 
%%========================================================================
show_counters(AppJobPid, LDN, MaxResponseTime) ->
    AppJobPid ! {show_counters, self(), LDN, [], MaxResponseTime},
    ok.
    
%%========================================================================
%% show_counters(Pid, LDN, Counters, MaxReportTime) -> ok
%%
%% COM sends a show counters request. 
%%========================================================================
show_counters(AppJobPid, LDN, Counters, MaxResponseTime) ->
    AppJobPid ! {show_counters, self(), LDN, Counters, MaxResponseTime},
    ok.

%%========================================================================
%% cec_proc_name(Pid, CecProcId) ->
%%
%% If PMI2 pmsSession2 will inform pmAppJob about the CEC process name
%% Only used for logging
%%========================================================================
cec_proc_name(AppJobPid, CecProcName) ->
    AppJobPid ! {cec_proc_name, CecProcName},
    ok.
    
%%========================================================================
%% start(Callbacks, PmiCb, PmGroups, CounterMap, SessionPid) -> 
%%    {ok, Pid} | {error, Reason}
%%
%% Spawn a new AppJob process.
%%
%%========================================================================
start(Protocol, Params) ->
    Self = self(),
    spawn(fun() -> init(Self, Protocol, Params) end),

    receive
	{pm_app_job_init_res, Pid} ->
	    {ok, Pid}
    after ?START_TO ->
	    {error, {timeout, {?MODULE, start}}}
    end.


stop(Pid) ->
    Pid ! stop,
    ok.

 
%%========================================================================
%% get_proc_info(Pid) -> ok
%%========================================================================
get_proc_info(Pid) ->
    Pid ! {get_proc_info, self()},
    receive
	{proc_info, Res} ->
	    Res
    after ?SEND_TO ->
	    undefined
    end.
 
%%========================================================================
%% send_data(AppJobPid, GP, TimeSpec, MeasObjLDN, ValueBundle) -> ok.
%%
%%========================================================================
send_data(AppJobPid, MeasData) ->
    MRef = erlang:monitor(process, AppJobPid),
    AppJobPid ! {data, self(), MeasData},
    receive
	{ok, AppJobPid} ->
	    erlang:demonitor(MRef, [flush]),
	    ok;
	{'DOWN', MRef, process, AppJobPid, Reason} ->
	    ?LOG_RAM(?SEV_WARNING,
		     {"pmsAppJob ~p died unexpectedly, reason: ~p~n",
		      [AppJobPid, Reason]}),
	    ok
    end.


%%========================================================================
%% Debug functions
%%========================================================================
gld(Pid, UserPid) ->
    Pid ! {get_loop_data, UserPid}.


get_aliases(Pid, UserPid) ->
    Pid ! {get_aliases, UserPid}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% Init function for the Job process
%%========================================================================
init(UserPid, Protocol, {Callbacks, PmiCb, PmGroups, CounterMap, SessionPid}) ->
    UserPid ! {pm_app_job_init_res, self()},
    ?LOG_RAM(?SEV_1,
	     ?LFUN({"Started.~n"
		    "  Session = ~p~n"
		    "  Groups  = ~p~n"
		    "  CB      = ~p~n", 
		    [SessionPid, pmsLib:log_trunc_list(PmGroups), PmiCb]})),

    FlexGroups = [G || G <- PmGroups, pmsLib:is_flex_group(G)],
    {ProcType, FilteredGroups} = get_proc_type(FlexGroups, PmGroups),

    AliasTab = new_alias_tab(Protocol),
    add_aliases(ProcType, AliasTab, CounterMap),

    loop(#loop{protocol    = Protocol,
	       pm_groups   = lists:usort(FilteredGroups),
	       callbacks   = {PmiCb, Callbacks},
	       cb_mod      = PmiCb,
	       session_pid = SessionPid,
	       counter_map = CounterMap == undefined,
	       alias_tab   = AliasTab,
	       proc_type   = ProcType}).


new_alias_tab(pmi2) ->
    ets:new(aliases, [protected, set]);

new_alias_tab(_) ->
    undefined.



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% loop(JobId, TimeRef) 
%%
%% Loop until the session is deleted.
%%========================================================================
loop(#loop{protocol      = Prot,
	   pm_groups     = PmGroups,
	   session_pid   = SessionPid,
	   counter_map   = CmFlag,
	   subscribers   = Subscribers,
	   measurements  = Measurements,
	   gp_timers     = GpTimers,
	   alias_tab     = AliasTab,
	   proc_type     = ProcType,
	   cec_proc_name = CecProcName} = Loop) ->

    SD = [{K, {CGP, CGC, GPFF, LGP}} || 
	     #subscriber{key = K, 
			 comp_gp_prev = CGP, 
			 comp_gp_curr = CGC, 
			 gp_ff_sent = GPFF,
			 last_gp = LGP} 
		 <- Subscribers],
    ?LOG_RAM(?SEV_5, {"LOOP ~n"
		      "  AppName     = ~p~n"
		      "  Subscribers = ~p~n"
		      "  ProcType    = ~p~n",
		      [CecProcName, SD, ProcType]}),
    log_loop(Loop),
    receive
	%%-------------------------------------------------------------
	%% Receive and store report from the application.
	%%-------------------------------------------------------------
	{data, Session, {GPSec, ReportId, ValueBundle, FinalFragment}} ->
	    VB = decode_value_bundle_rop(Prot, ValueBundle, AliasTab),
	    ?LOG_RAM([{?SEV_1, 
		       ?LFUN({" <=== PM DATA (~p)~n"
			      "  AppName  = ~p~n"
			      "  Subscr   = ~p~n"
			      "  GP       = ~p~n"
			      "  ReportId = ~p~n"
			      "  Data     = ~p~n"
			      "  NoVals   = ~p~n"
			      "  NoLDNs   = ~p~n"
			      "  NoGrps   = ~p~n"
			      "  Final    = ~p~n", 
			      [CecProcName,
			       Prot,
			       self(), 
			       GPSec, 
			       ReportId, 
			       pmsLib:log_trunc_bundle(VB),
			       num_of_data_vals(ValueBundle),
			       num_of_data_ldns(ValueBundle),
			       num_of_data_grps(ValueBundle),
			       FinalFragment]}), ?DEFAULT_BL_TO},
		      {?SEV_5, 
		       ?LFUN({" <=== PM DATA (~p)~n"
			      "  AppName  = ~p~n"
			      "  Subscr   = ~p~n"
			      "  GP       = ~p~n"
			      "  ReportId = ~p~n"
			      "  Data     = ~p~n"
			      "  NoVals   = ~p~n"
			      "  NoLDNs   = ~p~n"
			      "  NoGrps   = ~p~n"
			      "  Final    = ~p~n", 
			      [CecProcName,
			       Prot,
			       self(), 
			       GPSec, 
			       ReportId, 
			       pmsLib:log_trunc_bundle(VB),
			       num_of_data_vals(ValueBundle),
			       num_of_data_ldns(ValueBundle),
			       num_of_data_grps(ValueBundle),
			       FinalFragment]})
			    }]),
	    GP = pmsLib:get_gp_enum(GPSec),
	    Data = {GP, ReportId, VB, FinalFragment},
	    log_msg(data, Data),
	    GPEndTime = proplists:get_value(GP, Loop#loop.gp_end_time),
	    NewSubscribers = send_to_subscribers(GPEndTime, 
						 Subscribers, 
						 Data,
						 CecProcName),
	    Session ! {ok, self()},
 	    loop(Loop#loop{subscribers = NewSubscribers});

	%%-------------------------------------------------------------
	%% Subscribe request
	%%-------------------------------------------------------------
	{subscribe, JobId, JobPid, GP, CSpecMods, MeasTab} ->
 	    ?LOG_RAM([{?SEV_5,
		       {"Subscribe ~n"
			"  AppName     = ~p~n"
			"  Protocol    = ~p~n"
			"  JobId       = ~p~n"
			"  JobPid      = ~p~n"
			"  GP          = ~p~n"
			"  Updates     = ~p~n", 
			[CecProcName, Prot, JobId, JobPid, GP, CSpecMods]}}]),
	    log_msg(subscribe, {GP, CSpecMods, JobPid}),
	    NewSubscribers = handle_subscribe(JobId,
					      JobPid, 
					      GP, 
					      CSpecMods, 
					      MeasTab,
					      Subscribers),
	    NewGPMeas = get_gp_measurements(NewSubscribers, GP, 
					    PmGroups),
	    OldGPMeas = proplists:get_value(GP, Measurements, []),
	    NewMeasurements = send_subscribe(NewGPMeas,
					     OldGPMeas,
					     GP,
					     Loop),
	    NewGpTimers = init_gp_sync(proplists:get_value(GP, GpTimers), 
				       GP, 
				       NewMeasurements,
				       GpTimers),
	    loop(Loop#loop{subscribers  = NewSubscribers,
			   measurements = NewMeasurements,
			   gp_timers    = NewGpTimers});

	%%-------------------------------------------------------------
	%% Unsubscribe request
	%%-------------------------------------------------------------
	{unsubscribe, JobPid, GP} ->
 	    ?LOG_RAM([{?SEV_5,
		       {"Unsubscribe ~n"
			"  JobPid      = ~p~n"
			"  GP          = ~p~n", 
			[JobPid, 
			 GP]}}]),
	    log_msg(unsubscribe, {JobPid, GP}),
	    OldGPMeas = proplists:get_value(GP, Measurements, []),
	    NewSubscribers = handle_unsubscribe(JobPid, GP, Subscribers),
	    NewGPMeas = get_gp_measurements(NewSubscribers, GP, PmGroups),
	    NewMeasurements = send_subscribe(NewGPMeas,
					     OldGPMeas,
					     GP,
					     Loop),
	    NewGpTimers = init_gp_sync(proplists:get_value(GP, GpTimers), 
				       GP, 
				       NewMeasurements,
				       GpTimers),
	    loop(Loop#loop{subscribers  = NewSubscribers,
			   measurements = NewMeasurements,
			   gp_timers    = NewGpTimers});

	%%-------------------------------------------------------------
	%% Counter map
	%%-------------------------------------------------------------
	{counter_map, CounterMaps} when CmFlag ->
	    ?LOG_RAM([%% {?SEV_1, " <=== COUNTER_MAP ~n"},
		      {?SEV_1,
		       ?LFUN({" <=== COUNTER_MAP (~p) ~n"
			      "  AppName = ~p~n"
			      "  Maps    = ~p~n",
			      [Prot, 
			       CecProcName,
			       pmsLib:log_trunc_cm(CounterMaps)]})}]),
	    CmGrps      = [G || {G, _, _} <- CounterMaps], 
	    FlexGrps    = [G || G <- CmGrps, pmsLib:is_flex_group(G)],
	    NewProcType = 
		update_proc_type(ProcType, FlexGrps, CmGrps -- FlexGrps),
	    add_aliases(NewProcType, AliasTab, CounterMaps),
	    NewGrps = lists:usort(ets:select(AliasTab, [{{'$1', '$2'}, 
							 [{is_integer, '$1'}], 
							 ['$2']}])),
	    Callbacks = Loop#loop.callbacks,
	    pmsAppRegistry:counter_map_pmi2(self(),
					    Callbacks,
					    NewGrps  -- PmGroups,
					    PmGroups -- NewGrps,
					    SessionPid),
	    
	    loop(Loop#loop{pm_groups = NewGrps,
			   proc_type = NewProcType});
	{counter_map, CounterMaps} ->
	    ?LOG_RAM([%% {?SEV_1, "<=== COUNTER_MAP --- IGNORED~n"},
		      {?SEV_1,
		       {"<=== COUNTER_MAP (~p) --- IGNORED~n"
			"CounterMapId received in initialize msg." 
			"  AppJob = ~p~n"
			"  Maps   = ~p~n",
			[Prot, self(), CounterMaps]}}]),
	    loop(Loop);
	%%-------------------------------------------------------------
	%% Granularity period tick is synchronized to the wall 
	%% clock time. Resync the granularity tick timer and send
	%% a report request to the application if synched.
	%%-------------------------------------------------------------
	{?GP_SYNCED, GP, {MeSec, Sec, MiSec} = NextSyncTime} ->
	    Now = os:timestamp(), 
	    NST = erlang:round(MeSec * 1000000 + Sec + MiSec/1000000),
	    log_msg(gp_synced, {GP, Now, NextSyncTime}),
	    MeasTS = meas_timestamp(GP),
	    case handle_gp_sync(GP, Now, NextSyncTime, Loop) of
		%% Fine tuning towards the wall clock 
		{true = FineTune, _} ->
		    ?LOG_RAM(?SEV_5, 
			     {"GP SYNC AppJob ~p Fine tune~n", 
			      [CecProcName]}),
		    NewGpTimers = resync(FineTune, GpTimers, GP, Now),
		    loop(Loop#loop{gp_timers = NewGpTimers});
		%% Report rop data request was sent to the application
		{false = FineTune, SyncLoop} ->
		    ?LOG_RAM(?SEV_1,
			     {"GP SYNC AppJob ~p~n"
			      "  Next sync time = ~p (~s)~n"
			      "  ReqId          = ~p (~s)~n", 
			      [CecProcName,
			       NST, 
			       pmsLib:rtime(NST),
			       MeasTS,
			       pmsLib:rtime(MeasTS)]}),
		    NewGpTimers   = resync(FineTune, GpTimers, GP, Now),
		    loop(SyncLoop#loop{gp_timers = NewGpTimers})
	    end;

	%%-------------------------------------------------------------
	%% Timer when the applications should have replied
	%% with the rop data.
	%% Inform all pmsJobs that the time has expired.
	%%-------------------------------------------------------------
	{?ROP_DATA_TO, GP, _Time} ->
	    handle_rop_data_to(GP, Subscribers),
	    GPTs = lists:keystore(GP, 1, Loop#loop.gp_end_time, 
				  {GP, undefined}),
	    NewSubscribers = update_subscribers_gp_reported(GP, Subscribers),
	    loop(Loop#loop{subscribers = NewSubscribers,
			   gp_end_time = GPTs});

	%%-------------------------------------------------------------
	%% Show counters from COM
	%%-------------------------------------------------------------
	{show_counters, COMPid, _LDN, [], _MaxRespTime} when Prot =:= pmi2 ->
	    ?LOG_RAM(?SEV_1,
		     {"Show counters ~n"
		      "  Error = ~p (no counters)~n", [?SC_NO_COUNTERS]}),
	    show_counters_response(COMPid, error, ?SC_NO_COUNTERS),
 	    loop(Loop);
	{show_counters, COMPid, LDN, ScSpec, MaxRespTime} ->
	    ?LOG_RAM(?SEV_5, 
		     ?LFUN({"Show counters ~n"
			    "  Spec = ~p ~n",
			    [pmsLib:log_trunc_sc_counters(ScSpec)]})),
	    ReqId = get_next_req_id(Loop#loop.req_id),
	    NewReqs = handle_show_counters(ReqId, 
					   COMPid, 
					   LDN,
					   ScSpec,
					   MaxRespTime, 
					   Loop),
	    loop(Loop#loop{req_id = ReqId, sc_reqs = NewReqs});

	%%-------------------------------------------------------------
	%% Show counters response
	%%-------------------------------------------------------------
	{data_sc, {ReqId, Result, ErrorStr, ValueBundle}} ->
	    Reqs = Loop#loop.sc_reqs,
	    {COMPid, ReqSC} = proplists:get_value(ReqId, Reqs, ?NO_REQ),
	    VB = decode_value_bundle(Prot, ValueBundle, AliasTab, ReqSC),
	    ?LOG_RAM([%% {?SEV_1, " <=== DATA SHOW COUNTERS ~n"},
		      {?SEV_1,
		       ?LFUN({" <=== DATA SHOW COUNTERS (~p)~n"
			      "  Subscr   = ~p~n"
			      "  ReqId    = ~p~n"
			      "  Result   = ~p~n"
			      "  ErrorStr = ~p~n"
			      "  Data     = ~p~n"
			      "  NoVals   = ~p~n",
			      [Prot,
			       self(), 
			       ReqId, 
			       Result,
			       ErrorStr,
			       log_trunc_sc_vals(Prot, VB, ValueBundle),
			       num_of_sc_data_vals(ValueBundle)]}), 
		       ?LONG_BL_TO},
		      {?SEV_5,
		       ?LFUN({" <=== DATA SHOW COUNTERS (~p)~n"
			      "  Subscr   = ~p~n"
			      "  ReqId    = ~p~n"
			      "  Result   = ~p~n"
			      "  ErrorStr = ~p~n"
			      "  Data     = ~p~n"
			      "  NoVals   = ~p~n",
			      [Prot,
			       self(), 
			       ReqId, 
			       Result,
			       ErrorStr,
			       log_trunc_sc_vals(Prot, VB, ValueBundle),
			       num_of_sc_data_vals(ValueBundle)]})}]),
	    log_msg(show_counters, {ReqId, VB}),
	    show_counters_response(COMPid, ok, {Result, ErrorStr, VB}),
	    loop(Loop#loop{sc_reqs = lists:keydelete(ReqId, 1, Reqs)});

	%%-------------------------------------------------------------
	%% Show counters timeout
	%%-------------------------------------------------------------
	{timeout, _Ref, {show_counters, ReqId}} ->
	    log_msg(show_counters_timeout, ReqId),
	    Reqs = Loop#loop.sc_reqs,
	    show_counters_timeout(ReqId, Reqs),
	    loop(Loop#loop{sc_reqs = lists:keydelete(ReqId, 1, Reqs)});

	%%-------------------------------------------------------------
	%% pmsJob terminated, remove from subscribers
	%%-------------------------------------------------------------
	{'DOWN', _MRef, process, JobPid, Reason} ->
	    ?LOG_RAM(?SEV_1,
		     {"pmsJob ~p died, reason: ~p~nRemove from Subscribers~n",
		      [JobPid, Reason]}),
	    NewSubscribers = [Sub || #subscriber{key = {Pid, _GP}} = Sub 
					 <- Subscribers, Pid =/= JobPid],
	    loop(Loop#loop{subscribers = NewSubscribers});

	%%-------------------------------------------------------------
	%% Stop from pmsAppRegistry. Kill the process.
	%%-------------------------------------------------------------
	stop ->
	    handle_app_exit(Subscribers, CecProcName),
	    ?LOG_RAM(?SEV_1, {"stopped Session ~p~n", [SessionPid]}),
	    exit(normal);

	%%-------------------------------------------------------------
	%% CEC process id
	%%-------------------------------------------------------------
	{cec_proc_name, NewCecProcName} ->
	    ?LOG_RAM(?SEV_1, {"CEC process name ~p~n", [NewCecProcName]}),
	    loop(Loop#loop{cec_proc_name = NewCecProcName});

	%%-------------------------------------------------------------
	%% Read loop data
	%%-------------------------------------------------------------
	{get_loop_data, UserPid} ->
	    UserPid ! {loop_data, format_loop(Loop)},
	    loop(Loop);
	%%-------------------------------------------------------------
	%% Read process info
	%%-------------------------------------------------------------
	{get_proc_info, UserPid} ->
	    Res = {app_job, 
		   [{loop_size, erts_debug:flat_size(Loop)},
		    {proc_info, pmsLib:get_proc_info(self())},
		    {ets_tables, [{alias_tab, ets:info(AliasTab, memory)}]}]},
	    UserPid ! {proc_info, Res},
	    loop(Loop);
	%%-------------------------------------------------------------
	%% Read alias data
	%%-------------------------------------------------------------
	{get_aliases, UserPid} ->
	    AliasesData = AliasTab =/= undefined andalso
		ets:tab2list(AliasTab),
	    UserPid ! {aliases_data, AliasesData},
	    loop(Loop);
	%%-------------------------------------------------------------
	%% Unknown message
	%%-------------------------------------------------------------
	UnknownMsg ->
	    ?LOG_RAM(?SEV_5, {"Received unknown msg ~p~n", [UnknownMsg]}),
	    loop(Loop)
    end.


%%========================================================================
%% handle_app_exit(Subscribers, CecProcName) -> ok
%% 
%% Inform all pmsJobs that the app has exited
%%========================================================================
handle_app_exit(Subscribers, CecProcName) ->
    lists:foreach(fun(Subscriber) ->
			  CSpecDelta = get_cspec_delta(Subscriber),
			  {Pid, GP} = Subscriber#subscriber.key,
			  GpEndTime = pmsLib:gp_end_time(GP),
			  MeasData = {GP, GpEndTime, [], true},
			  ?LOG_RAM(?SEV_1,
				   {"APP EXIT ~n"
				    "  AppName = ~p~n"
				    "  GP      = ~p~n"
				    "  EndTime = ~p~n"
				    "  JobPid  = ~p~n", 
				    [CecProcName, GP, GpEndTime, Pid]}),
			  pmsJob:meas_data(Pid, self(), MeasData, CSpecDelta, 
					   false)
		  end, Subscribers).

%%========================================================================
%% handle_subscribe(JobPid, GP, CounterSpec, MeasTab, Subscribers) -> Res
%% 
%% Send subscribe request to the application.
%% Do not send anything if the new request does not modify
%% the previously sent CounterSpec.
%%========================================================================
handle_subscribe(JobId, JobPid, GP, CSpecMods, MeasTab, Subscribers) ->
    Subscription = lists:keyfind({JobPid, GP}, ?SKEY, Subscribers),
    get_subscribers(Subscription, 
		    JobId,
		    JobPid, 
		    GP,
		    CSpecMods, 
		    MeasTab,
		    Subscribers).

%%========================================================================
%% handle_unsubscribe(JobPid, GP, Subscribers) -> Res
%% 
%% Send empty subscribe request to the application if last pmJob.
%% Do not send anything if the new request does not modify
%% the previously sent CounterSpec.
%%========================================================================
handle_unsubscribe(JobPid, GP, Subscribers) ->
    case lists:keytake({JobPid, GP}, ?SKEY, Subscribers) of
	false ->
	    Subscribers;
	{value, Subscriber, RSubscribers} 
	  when Subscriber#subscriber.gp_ff_sent =:= true ->
	    {JobPid, _} = Subscriber#subscriber.key,
	    GpEndTime = pmsLib:gp_end_time(GP),
	    FFData = {GP, GpEndTime, [], true},
	    pmsJob:meas_data(JobPid, self(), FFData, undefined, false),
	    ?LOG_RAM(?SEV_1,
		     {"PmJob stopped or deleted.~n"
		      "Send empty meas_data and remove Job from Subscribers~n"
		      "  JobPid  = ~p~n", 
		      [JobPid]}),
	    RSubscribers;
	{value, Subscriber, RSubscribers} ->
	    ?LOG_RAM(?SEV_1,
		     {"PmJob stopped or deleted, wait for Final Fragment~n"
		      "  JobPid  = ~p~n", 
		      [JobPid]}),
	    [Subscriber#subscriber{last_gp = true, 
				   comp_gp_curr = false} | RSubscribers]
    end.
    
%%-------------------------------------------------------
%% get_subscribers(DefinedInOldSubsc, JobPid, GP, CounterSpec, Subscribers)
%% 
%% Create the new subscribers
%%-------------------------------------------------------
%% Job added, add it to the subscribers
get_subscribers(false, JobId, JobPid, GP, _CSpecMods, MeasTab, Subscribers) ->
    Subscriber = #subscriber{key      = {JobPid, GP}, 
			     job_id   = JobId,
			     meas_tab = MeasTab},
    erlang:monitor(process, JobPid),
    [Subscriber | Subscribers];

%% Job changed
get_subscribers(Subscriber, _JobId, _JP, _GP, CSpec, _MeasTab, Subscribers)
  when not Subscriber#subscriber.last_gp ->
    UpdatedSubscriber = update_subscriber_cspecs(CSpec, Subscriber),
    Key = Subscriber#subscriber.key,
    lists:keystore(Key, ?SKEY, Subscribers, UpdatedSubscriber);

% Job is activated from stopped state 
get_subscribers(Subscriber, _JobId, _JP, _GP, _CSpec, _MeasTab, Subscribers) ->
    Key = Subscriber#subscriber.key,
    NewSubscriber = Subscriber#subscriber{comp_gp_curr = false,
					  last_gp = false},
    lists:keystore(Key, ?SKEY, Subscribers, NewSubscriber).
    

update_subscriber_cspecs(CSpecMods, Subscriber) ->
    NewAdd = proplists:get_value(add, CSpecMods, []), 
    CurrAdd = merge_cspecs(NewAdd, Subscriber#subscriber.cspec_add_curr),
    CurrMods = get_curr_mods(CSpecMods, Subscriber),
    Subscriber#subscriber{cspec_add_curr = CurrAdd,
			  cspec_mod_curr = CurrMods,
			  last_gp = false}.


get_curr_mods(_CSpecMods, Subscriber) 
  when Subscriber#subscriber.gp_ff_sent ->
    [];

get_curr_mods(CSpecMods, Subscriber) ->
    CSpecMods ++ Subscriber#subscriber.cspec_mod_curr.


%%========================================================================
%% update_subscribers_gp_reported(GP, CompleteGp, Msg) -> CompleteGp
%% 
%% 
%%========================================================================
update_subscribers_gp_reported(CurGP, Subscribers) ->
    lists:map(fun(#subscriber{key = {_, GP}} = Sub) 
		    when GP =:= CurGP, 
			 not Sub#subscriber.gp_ff_sent ->
		      Sub#subscriber{gp_ff_sent = true,
				     cspec_add_prev = [],
				     cspec_mod_curr = []};
		 (Sub) ->
		      Sub
	      end, [Subscriber || Subscriber <- Subscribers, 
				  not Subscriber#subscriber.last_gp]).
			 
  
%%========================================================================
%% send_to_subscribers(GPEndTime, Subscribers, Data, CompleteGp, CecProcName)
%% 
%% Send pmi data to subscribers
%%
%% Subscribers = {{JobPid, GP}, CounterSpec}
%% Counterspec = [{PMGroup, [MeasurementType]}]
%% Data = {GP, TimeSpec, ValueBundle, FinalFrag}
%% ValueBundle = [{PMGroup, [{MOLDN, [{MeasurementType, [Value]}]}]}]
%%========================================================================
send_to_subscribers(GPEndTime,
		    Subscribers,
		    {GP, TimeSpec, _VB, FF} = Data,
		    CecProcName) 
  when GPEndTime =:= TimeSpec ->
    GPSubsc = [Subscriber || #subscriber{key = {_, SGP}} = Subscriber 
				 <- Subscribers, SGP =:= GP],
    sts(GPSubsc, Data, FF, CecProcName),
    if 
	FF ->
	    update_subscribers_gp_reported(GP, Subscribers);
	true ->
	    Subscribers
    end;

send_to_subscribers(_GPEndTime, 
		    Subscribers, 
		    {GP, TimeSpec, VB, _FF}, 
		    CecProcName) ->
    RecTime = ?ET,
    ?LOG_RAM(?SEV_WARNING,
	     ?LFUN({"Discarding PM DATA.  Received after deadline. ~n"
		    "  AppName  = ~p~n"
		    "  Subscr   = ~p~n"
		    "  GP       = ~p~n"
		    "  Received = ~p~n"
		    "  ReportId = ~p~n"
		    "  Data     = ~p~n"
		    "  Final    = ~p~n", 
		    [CecProcName,
		     Subscribers, 
		     GP, 
		     RecTime,
		     TimeSpec, 
		     pmsLib:log_trunc_bundle(VB),
		     _FF]}),
	     pmsLib:get_report_offset(GP)),
    Subscribers.


sts(GPSubsc, {GP, TimeSpec, _VB, FF} = Data, FF, CecProcName) ->
    F = fun(Sub) when Sub#subscriber.comp_gp_prev =:= true,
		      Sub#subscriber.gp_ff_sent =:= false -> 
		send_to_subscriber(Sub, Data, CecProcName); 
	   (#subscriber{key = {Pid, _}} = Sub) 
	      when FF =:= true, 
		   Sub#subscriber.gp_ff_sent =:= false ->
		?LOG_RAM(?SEV_1,
			 {"Measurement values discarded due to "
			  "incomplete GP. FF = true.~n "
			  "~p Send empty meas data to ~p~n",
			  [CecProcName, Pid]},
			 pmsLib:get_report_offset(GP)),
		send_to_subscriber(Sub, {GP, TimeSpec, [], FF}, CecProcName); 
	   (#subscriber{key = {Pid, _}, gp_ff_sent = false}) ->
		?LOG_RAM(?SEV_1,
			 {"~p Measurement values discarded due to "
			  "incomplete GP. ~p~n", [CecProcName, Pid]},
			 pmsLib:get_report_offset(GP));
	   (#subscriber{key = {Pid, _}}) ->
		?LOG_RAM(?SEV_WARNING, 
			 {"~p Measurement values received after "
			  "final fragment. Discarded. ~p~n", 
			  [CecProcName, Pid]},
			 pmsLib:get_report_offset(GP))
	end,
    lists:foreach(F, GPSubsc).


send_to_subscriber(Subscriber, {GP, TimeSpec, ValueBundle, FF}, CecProcName) ->
    MTab = Subscriber#subscriber.meas_tab,
    ?LOG_RAM(?SEV_2, {"Send measurement values to pmJobs ~n", []}),
    Added = get_added_since_prev(Subscriber),
    Removed = get_removed_since_prev(Subscriber),
    Fun = fun(GroupVals) ->
		  filter_meas_values(GroupVals, Added, Removed, MTab)
	  end,
    %% TS1 = os:timestamp(),
    Delay = get_filter_delay(FF, GP),
    VB = pmsLib:pmapr_delay(Fun, ValueBundle, Delay, ?MIN_DELAY_SLICE, 
			    ?MAX_DELAY_SLICE),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time filter_meas_values: ~p ms~n", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    case sort_meas_data(lists:append(VB)) of
	MeasData when MeasData =/= []; FF ->
	    {JobPid, _} = Subscriber#subscriber.key,
	    ?LOG_RAM(?SEV_1, {"~p Measurement values sent to ~p ~n", 
			      [CecProcName, JobPid]}),
	    CSpecDelta = get_cspec_delta(FF, Subscriber),
	    pmsJob:meas_data(JobPid, self(), {GP, TimeSpec, MeasData, FF}, 
			     CSpecDelta);
	_ ->
	    ok
    end.


get_filter_delay(FF, GP) when FF ->
    GPEnd = pmsLib:gp_end_time(GP),
    Delay = pmsLib:get_curr_delay_time(GP, GPEnd, ?DELAY_TIME, ?MIN_REM_TIME),
    ?LOG_RAM(?SEV_1, {"Delay time filter_meas_values: ~p ms~n", [Delay]}),
    Delay;

get_filter_delay(_FF, _GP) ->
    0.
    

get_cspec_delta(true, Subsc) ->
    get_cspec_delta(Subsc);

get_cspec_delta(_False, _Subsc) ->
    {[], []}.


get_cspec_delta(Subsc) ->
    {Subsc#subscriber.cspec_add_prev, Subsc#subscriber.cspec_mod_curr}.


filter_meas_values({PMGroup, LDNMTVs}, Added, Removed, MTab) 
  when LDNMTVs =/= [] ->
    GAdd = proplists:get_value(PMGroup, Added, []),
    GRm = proplists:get_value(PMGroup, Removed, []),
    IsFlex = pmsLib:is_flex_group(PMGroup),
    Fun = fun(MOLDNVals) ->
		  filter_meas_ldn_vals(MOLDNVals, GAdd, GRm, MTab, PMGroup, 
				       IsFlex)
	  end,
    %% TS1 = os:timestamp(),
    GV = pmsLib:pmapr_delay(Fun, LDNMTVs, 0),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time filter_meas_ldn_vals: ~p ms~n", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    case lists:append(GV) of
	GroupVals when GroupVals =/= [] ->
	    [{PMGroup, GroupVals}];
	_ ->
	    []
    end;

filter_meas_values({_PMGroup, _LDNMTVs}, _Added, _Removed, _MTab) ->
    [].


filter_meas_ldn_vals({MOLDN, LDNVals}, Added, Removed, MTab, PMGroup, IsFlex) ->
    case [MTV || MTV = {MT, _V} <- LDNVals, 
		 check_mt_subscribed(IsFlex, MT, Added, Removed, MTab, 
				     PMGroup)] of
	[] ->
	    [];
	FLDNVals ->
	    [{MOLDN, FLDNVals}]
    end.


get_added_since_prev(#subscriber{cspec_add_prev = Added, 
				 cspec_mod_curr = Mods}) ->
    get_mod_since_prev(add, Added, Mods).


get_removed_since_prev(#subscriber{cspec_mod_curr = Mods}) ->
    get_mod_since_prev(rm, [], Mods).


get_mod_since_prev(Op, StartAcc, Mods) ->
    F = fun({AddOrRm, Spec}, Acc) when AddOrRm =:= Op ->
		merge_cspecs(Spec, Acc);
	   ({_, Spec}, Acc) ->
		[{G, MT -- proplists:get_value(G, Spec, [])} || 
		    {G, MT} <- Acc]
	end,
    lists:foldl(F, StartAcc, lists:reverse(Mods)).
    

merge_cspecs([], CSpec2) ->
    CSpec2;

merge_cspecs(CSpec1, []) ->
    CSpec1;

merge_cspecs(CSpec1, CSpec2) ->
    TmpCSpec = [{G, lists:umerge(MT, proplists:get_value(G, CSpec2, []))} ||
		   {G, MT} <- CSpec1],
    lists:ukeymerge(1, TmpCSpec, CSpec2).


check_mt_subscribed(false, MT, Added, Removed, MTab, PMGroup) ->
    check_normal_mt_subscribed(MT, Added, Removed, MTab, PMGroup);

check_mt_subscribed(_IsFlex, MT, Added, Removed, MTab, PMGroup) ->
    check_flex_mt_subscribed(MT, Added, Removed, MTab, PMGroup).


check_normal_mt_subscribed(MT, [], [], MTab, PMGroup) ->
    is_mt_subscribed(MTab, PMGroup, MT);

check_normal_mt_subscribed(MT, Added, Removed, MTab, PMGroup) ->
    case is_mt_subscribed(MTab, PMGroup, MT) of
	true ->
	    not lists:member(MT, Added);
	_False ->
	    lists:member(MT, Removed)
    end.


check_flex_mt_subscribed({BaseMTId, _}, Added, Removed, MTab, PMGroup) ->
    check_normal_mt_subscribed(BaseMTId, Added, Removed, MTab, PMGroup);

check_flex_mt_subscribed(_BaseMTId, _Added, _Removed, _MTab, _PMGroup) ->
    false.


is_mt_subscribed(MTab, PMGroup, MT) ->
    try
         [] =/= ets:lookup(MTab, {PMGroup, MT})
    catch 
    	_:_ ->
    	    false
    end.	  
    %% case ets:next(MTab, {PMGroup, MT}) of
    %% 	'$end_of_table' ->
    %% 	    [] =/= ets:lookup(MTab, {PMGroup, MT});
    %% 	Key ->
    %% 	    {PMGroup, MT} =:= ets:prev(MTab, Key)
    %% end.


sort_meas_data(VB) ->
    %% Each MeasObjLDN may only occur once per PmGroup. 
    %% Each MeasurementType may only occur once for each MeasObjLDN.
    %% This is an optimization in order to reduce time for aggregation.
    [{Group, parallel_sort(LDNMVals)} || {Group, LDNMVals} <- VB].


parallel_sort(KeyVals) ->
    Fun = fun({Key, Vals}) ->
		  {Key, lists:ukeysort(1, Vals)}
	  end,
    %% TS1 = os:timestamp(),
    SortVals = pmsLib:pmapr_delay(Fun, KeyVals, 0),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time parallel_sort: ~p ms~n", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    SortVals.
		    

%%-------------------------------------------------------
%% handle_show_counters(ReqId, COMPid, MeasObjLDN, ScSpec, MaxRespTime, Loop)
%% 
%% Handle show counters request from COM
%%
%% ReqId      = integer()
%% MeasObjLDN = string()
%%-------------------------------------------------------
handle_show_counters(ReqId, COMPid, LDN, ScSpec, MaxRespTime, Loop) 
  when Loop#loop.protocol =:= pmi2 ->
    case gmfI:check_string(to_list(LDN)) of
	{ok, LDNAlias} ->
	    ?LOG_RAM(?SEV_5, {"Using LDN alias ~p~n", [LDNAlias]}),
	    ScSpecAlias = get_sc_aliases(ScSpec, Loop#loop.alias_tab, []),
	    send_show_counters(ReqId, COMPid, LDNAlias, ScSpecAlias, 
			       MaxRespTime, Loop);
	_Error ->
	    ?LOG_RAM(?SEV_ERROR, {"No alias found for LDN ~p~n", [LDN]}),
	    show_counters_response(COMPid, error, ?SC_NO_COUNTERS),
	    Loop#loop.sc_reqs
    end;

handle_show_counters(ReqId, COMPid, LDN, ScSpec, MaxRespTime, Loop) ->
    send_show_counters(ReqId, COMPid, LDN, ScSpec, MaxRespTime, Loop).


%%-------------------------------------------------------
%% send_show_counters(ReqId, MeasObjLDN, ScSpec, MaxResponseTime, Loop)
%% 
%% Send show counters request to application
%%
%% ReqId      = integer()
%% MeasObjLDN = string()
%%-------------------------------------------------------
send_show_counters(ReqId, COMPid, MeasObjLDN, ScSpec, Timeout, Loop) 
  when ScSpec =/= []; Loop#loop.protocol =:= pmi ->
    Pid = Loop#loop.session_pid,
    PmiCb = Loop#loop.cb_mod,
    Prot = Loop#loop.protocol,
    ?LOG_RAM([{?SEV_1," ===> REPORT SHOW COUNTERS~n"},  
	      {?SEV_5, 
	       ?LFUN({" ===> REPORT SHOW COUNTERS (~p) ~n"
		      "  Session    = ~p~n"
		      "  ReqId      = ~p~n"
		      "  MeasObjLDN = ~p~n"
		      "  ScSpec     = ~w~n", 
		      [Prot, Pid, ReqId, MeasObjLDN, 
		       pmsLib:log_trunc_sc_spec(ScSpec)]})}]),
    log_msg(send_show_counters, {ReqId, MeasObjLDN}),
    try
	send_sc(Prot, PmiCb, Pid, ReqId, MeasObjLDN, ScSpec, Timeout)
    catch _:R ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"Failed to call show counters: ~p~n", [R]}),
	    ok
    end,
    erlang:start_timer(1000 * (Timeout + 1),
		       self(), 
		       {show_counters, ReqId}),
    if
	Loop#loop.protocol =:= pmi2 ->
	    [{ReqId, {COMPid, undefined}} | Loop#loop.sc_reqs];
	true ->
	    [{ReqId, {COMPid, {MeasObjLDN, ScSpec}}} | Loop#loop.sc_reqs]
    end;

send_show_counters(_ReqId, COMPid, MeasObjLDN, _ScSpec, _Timeout, Loop) ->
    ?LOG_RAM(?SEV_ERROR, {"No counter aliases found.~nLDN ~p~n", [MeasObjLDN]}),
    show_counters_response(COMPid, error, ?SC_NO_COUNTERS),
    Loop#loop.sc_reqs.


send_sc(pmi2, PmiCb, Pid, ReqId, MeasObjLDN, ScSpec, Timeout) ->
    PmiCb:?REPORT_SC_CB(Pid, ReqId, MeasObjLDN, ScSpec, Timeout);

send_sc(_Prot, PmiCb, Pid, ReqId, MeasObjLDN, _ScSpec, Timeout) ->
    PmiCb:pmiReportShowCountersCallback(Pid, ReqId, MeasObjLDN, Timeout).

%%-------------------------------------------------------
%% show_counters_response(ReqId, Result, MeasValues, Reqs)
%% 
%% Send show counters data to COM
%%
%% ReqId      = integer()
%% MeasValues = [{MeasurementType, [Value]}]
%%-------------------------------------------------------
show_counters_response(Pid, Tag, Result) when is_pid(Pid) ->
    Pid ! {show_counters_res, {Tag, Result}};

show_counters_response(Pid, Tag, Result) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Not expected response.~n"
	      "  Pid    = ~p~n"
	      "  Tag    = ~p~n"
	      "  Result = ~p~n", 
	      [Pid, Tag, Result]}),
    ok.

%%-------------------------------------------------------
%% show_counters_timeout(ReqId, Reqs)
%% 
%% Send show counters timeout
%%
%% ReqId      = integer()
%% Reqs       = [{ReqId, COMPid}]
%%-------------------------------------------------------
show_counters_timeout(ReqId, Reqs) ->
    case proplists:get_value(ReqId, Reqs) of
	{Pid, _} ->
	    ?LOG_RAM(?SEV_WARNING,
		     {"Timeout.~n"
		      "  ReqId    = ~p~n"
		      "  Requests = ~p~n", 
		      [ReqId, Reqs]}),
	    show_counters_response(Pid, error, timeout); % ?SC_NO_COUNTERS ?
	_Undefined -> 
	    response_received
    end.


%%-------------------------------------------------------
%% sort counter/show counters spec, just to make easy to check if
%% new specs are equal to old specs
%%-------------------------------------------------------
%% sort_spec([], Acc) ->
%%     lists:sort(Acc);
%% sort_spec([{Grp, MTs}  | T], Acc) ->
%%     sort_spec(T, [{Grp, lists:sort(MTs)} | Acc]).


%%-------------------------------------------------------
%% log_trunc_sc_vals(Prot, ValueBundle) -> list()
%%
%%-------------------------------------------------------
log_trunc_sc_vals(pmi2, VB, _VB) ->    
    pmsLib:log_trunc_bundle(VB);

log_trunc_sc_vals(pmi, VB, VB) ->    
    pmsLib:log_trunc_pmi_sc_vals(VB);

log_trunc_sc_vals(_PMI, VB, _VB) ->    
    pmsLib:log_trunc_bundle(VB).
	      
%%========================================================================
%% send_subscribe(NewMeasurements, OldMeasurements, GP, Loop) -> NewMeas
%% 
%% Send subscribe request to the application.
%% Do not send anything if the new request does not modify
%% the previously sent CounterSpec.
%%========================================================================
send_subscribe(Measurements, Measurements, _, #loop{measurements = OldMeas}) ->
    OldMeas;
send_subscribe([], undefined, _, #loop{measurements = OldMeas}) ->
    OldMeas;
send_subscribe(Measurements,
	       _,
	       GP, 
	       #loop{protocol      = Prot,
		     cb_mod        = PmiCb,
		     session_pid   = SessionPid,
		     measurements  = OldMeas,
		     alias_tab     = AliasTab,
		     cec_proc_name = CecProcName}) ->
    CSpec = get_counter_spec(Measurements, AliasTab),
    ?LOG_RAM(?SEV_1,
	     ?LFUN({" ===> SUBSCRIBE (~p)~n"
		    "  Session = ~p~n"
		    "  AppName = ~p~n"
		    "  GP      = ~p~n"
		    "  Meas    = ~p~n" 
		    "  NoMTs   = ~p~n" 
		    "  NoGrps  = ~p~n", 
		    [Prot,
		     CecProcName,
		     SessionPid, 
		     GP, 
		     log_trunc_ma(CSpec), 
		     num_of_mts(CSpec),
		     num_of_grps(CSpec)]})),
    SubscribeFunc = subscribe_func(Prot),
    PmiCb:SubscribeFunc(SessionPid, 
			pmsLib:get_interval(GP), %% change to seconds
			CSpec),
    MeasDel = proplists:delete(GP, OldMeas),
    ss_rc(Measurements, MeasDel, GP).


ss_rc([], MeasDel, _) ->
    MeasDel;
ss_rc(Measurements, MeasDel, GP) ->
    [{GP, Measurements} | MeasDel].


get_counter_spec(Measurements, AliasTab) when AliasTab =/= undefined ->
    get_measurement_aliases(Measurements, AliasTab);
get_counter_spec(Measurements, _AliasTab) ->
    Measurements.


subscribe_func(pmi2) ->
    ?SUBSCRIBE_CB;
subscribe_func(_PMI) ->
    pmiSubscribeCallback.

%%========================================================================
%% Update GpTimers with the new Timer reference
%%========================================================================
update_gp(GP, NewRef, GpTimers) ->
    [{GP, NewRef} | proplists:delete(GP, GpTimers)].


%%========================================================================
%% Init tick if new GP. 
%% Remove the tick if no measurement is ongoing.
%%========================================================================
init_gp_sync(undefined, _, [], GpTimers) -> 
    GpTimers;
init_gp_sync(undefined, GP, _, GpTimers) -> 
    {Ref, _, _} = pmsLib:gp_sync_preset(?GP_SYNCED, 
					GP, 
					get_minus_offset(GP)),
    igs(GP, Ref, GpTimers);
init_gp_sync(TimerRef, GP, [], GpTimers) -> 
    pmsLib:cancel_granularity_tick(TimerRef),
    proplists:delete(GP, GpTimers);
init_gp_sync(_, _, _, GpTimers) -> 
    GpTimers.


igs(GP, Ref, GpTimers) ->
    choose(lists:member(GP, GpTimers), GpTimers, [{GP, Ref} | GpTimers]).


%%========================================================================
%% handle_gp_sync(GP, Now, NextSyncTime, Loop) -> {FineTune, Loop}
%% 
%% Start a new timer tick for GP, if not already started.
%% Send report request if new GP is started.
%%========================================================================
handle_gp_sync(GP, Now, NextSyncTime, #loop{gp_timers = GpTimers} = Loop)
  when Now >= NextSyncTime ->
    hgs(proplists:get_value(GP, GpTimers), GP, Loop);
handle_gp_sync(GP, 
	       {NowM, NowS, NowMS}, 
	       {NstM, NstS, _NstMS}, 
	       #loop{gp_timers = GpTimers} = Loop) ->
    %% If diff between now and expected is less than 300 ms sleep the 
    %% difference + 50 ms, otherwise send a new sync message
    case (NstM*1000000 + NstS) - (NowM*1000000 + NowS) of
	1 when NowMS > 700000 ->
	    Sleep = (1000000 - NowMS + 50000) div 1000,
	    ?LOG_RAM(?SEV_5, {"Resync start. Sleeping = ~p ms~n", [Sleep]}),
	    timer:sleep(Sleep),
	    hgs(proplists:get_value(GP, GpTimers), GP, Loop);
	_ ->
	    {true, Loop}
    end.


%% Do not send report request if GP is not in the GpTimers list
hgs(undefined, _, Loop) ->
    {true, Loop};
hgs(_,
    GP, 
    #loop{protocol      = Prot,
	  cb_mod        = PmiCb,
	  session_pid   = SesPid,
	  subscribers   = Subscribers,
	  measurements  = Meas,
	  cec_proc_name = CecProcName} = Loop) ->
    MeasTs = meas_timestamp(GP),
    Subscr = 
	send_report_request(Prot, 
			    PmiCb, 
			    SesPid, 
			    GP, 
			    MeasTs, 
			    Meas, 
			    Subscribers,
			    CecProcName),
    GPTs   = lists:keystore(GP, 1, Loop#loop.gp_end_time, {GP, MeasTs}),
    {false, 
     Loop#loop{gp_end_time = GPTs,
	       subscribers = Subscr}}.


%%========================================================================
%% send_report_request(SessionPid, GP, GpTimers, Measurements, Subs) -> Subs.
%% 
%% send a report request to the application only if there is 
%% a measurement ongoing
%%========================================================================
send_report_request(_, _, _, _, _, [] = _Measurements, [] = Subscribers, _) ->
    Subscribers;
send_report_request(Prot, 
		    PmiCb, 
		    SessionPid, 
		    GP, 
		    MeasTs, 
		    _Measurements, 
		    Subscribers,
		    CecProcName) ->
    JobIds = [JobId || #subscriber{job_id = JobId} <- Subscribers],
    ?LOG_RAM([{?SEV_1, 
	       {" ===> REPORT ROP ~n"
		"  AppName = ~p~n"
		"  ReqId   = ~p (~s)~n"
		"  Jobs    = ~p~n",
		[CecProcName,
		 MeasTs,
		 pmsLib:rtime(MeasTs), 
		 JobIds]}},
	      {?SEV_5, 
	       {" ===> REPORT ROP (~p)~n"
		"  AppName  = ~p~n"
		"  Session  = ~p~n"
		"  Jobs     = ~p~n"
		"  GP       = ~p~n"
		"  ReqId    = ~p~n"
		"  MaxTime  = ~p~n", 
		[CecProcName,
		 Prot, 
		 SessionPid,
		 JobIds,
		 pmsLib:get_interval(GP), 
		 MeasTs, 
		 pmsLib:get_report_offset(GP)]}}]),
    log_msg(send_report, {GP, MeasTs}),
    DeadLine = get_report_deadline(Prot, GP, MeasTs),
    CbFunc = report_cb_func(Prot),
    PmiCb:CbFunc(SessionPid,
			 pmsLib:get_interval(GP), %% change to seconds
			 MeasTs,
			 DeadLine),
    F = fun(#subscriber{key = {_P, SGP},
			cspec_add_curr = AddCurr,
			comp_gp_curr = CGPCurr} = Subscriber) 
	      when SGP =:= GP ->
		Subscriber#subscriber{cspec_add_prev = AddCurr,
				      cspec_add_curr = [],
				      cspec_mod_curr = [],
				      comp_gp_curr = true,
				      comp_gp_prev = CGPCurr,
				      gp_ff_sent = false};
	   (Subscriber) ->
		Subscriber
	end,
    lists:map(F, Subscribers).


get_report_deadline(pmi2, GP, _MeasTs) ->
    pmsLib:get_report_offset(GP);
get_report_deadline(_Prot, GP, MeasTs) ->
    MeasTs + pmsLib:get_report_offset(GP).


report_cb_func(pmi2) ->
    ?REPORT_ROP_CB;   
report_cb_func(_) ->
    pmiReportCallback.

%%========================================================================
%% resync(GpTimers) -> GpTimers
%%========================================================================
resync(FineTune, GpTimers, GP, Now) ->
    resync(proplists:get_value(GP, GpTimers), FineTune, GpTimers, GP, Now).


%% GP not found
resync(undefined, _, GpTimers, GP, _) ->
    log_msg(resync, {stopped, GP}),
    ?LOG_RAM(?SEV_ERROR, 
	     {"GP SYNC ~n"
	      "  Reason   = GP not found in GpTimers ~n"
	      "  GpTimers = ~p ~n"
	      "  GP       = ~p ~n", 
	      [GpTimers, GP]}),
    GpTimers;
%% Fine tune, do not start the ROP_DATA_TO, only new fine tuned sync
resync(_, true, GpTimers, GP, Now) ->
    {NewRef, {MeSec, Sec, MiSec}, _} = 
	pmsLib:gp_tick(?GP_SYNCED, GP, 0, 0, Now),
    NextSyncTime = erlang:round(MeSec * 1000000 + Sec + MiSec/1000000),
    ?LOG_RAM(?SEV_5, 
	     {"Resync start. Fine tune. NextSyncTime = ~p ~n", 
	      [NextSyncTime]}),
    update_gp(GP, NewRef, GpTimers);
%% Report rop data requested, start new sync and start also ROP_DATA_TO
resync(_, false, GpTimers, GP, _) ->
    ?LOG_RAM(?SEV_1, {"Resyncing.~n", []}),
    {NewRef, NextSyncTime, _} = pmsLib:gp_sync_preset(?GP_SYNCED, 
						      GP, 
						      get_minus_offset(GP)),
    timer:send_after(pmsLib:get_rop_data_to(GP) * 1000,
		     self(),
		     {?ROP_DATA_TO, GP, NextSyncTime}),
    update_gp(GP, NewRef, GpTimers).


%%========================================================================
%% meas_timestamp(GP) -> MeasTimestamp
%% 
%% Get end time of the latest GP.
%%========================================================================
meas_timestamp(GP) ->
    pmsLib:gp_end_time(GP).

%%========================================================================
%% handle_rop_data_to(GP, Subscribers) -> ok
%% 
%% Inform all pmsJobs that the rop data timer has expired
%%========================================================================
handle_rop_data_to(GP, Subscribers) ->
    GpEndTime = pmsLib:gp_end_time(GP),
    FFData = {GP, GpEndTime, [], true},
    lists:foreach(fun(#subscriber{key = {Pid, SGP}, 
				  gp_ff_sent = false
				  %% comp_gp_prev = true} = Subsc) 
				  %% comp_gp_curr = CGPC,
				  %% last_gp = LGP
				 } = Subsc) 
			when SGP =:= GP ->
			  ?LOG_RAM(?SEV_1,
				   {"REPORT ROP DATA TO ~n"
				    "  GP      = ~p~n"
				    "  EndTime = ~p~n"
				    "  JobPid = ~p~n", 
				    [GP, GpEndTime, Pid]}),
			  CSpecDelta = get_cspec_delta(true, Subsc),
			  pmsJob:meas_data(Pid, self(), FFData, CSpecDelta);
		     (_) ->
			  ok
		  end, Subscribers).

%%========================================================================
%% add_aliases(ProcType, AliasTab, CounterMaps) -> ok
%% 
%% 
%%========================================================================
add_aliases(?PROC_TYPE_COMMON, AliasTab, CounterMaps) ->
    add_aliases_common(AliasTab, CounterMaps);  
add_aliases(?PROC_TYPE_FLEX, AliasTab, CounterMaps) ->
    add_aliases_common(AliasTab, format_cm(CounterMaps, []));  
add_aliases(?PROC_TYPE_UNDEFINED, _AliasTab, _CounterMaps) ->
    ok.

format_cm([], Acc) ->
    Acc;
format_cm([{Grp, GrpAlias, MTs} | T], Acc) ->
    format_cm(T, [{Grp, GrpAlias, format_cm_mt(MTs, Grp, [])} | Acc]).

format_cm_mt([], _, Acc) ->
    Acc;
format_cm_mt([{MT, MtAlias} | T], Grp, Acc) ->
    case base_flex_name_get(Grp, MT) of
	{ok, MT} ->
	    %% Base flex counter
	    format_cm_mt(T, Grp, [{MT, MtAlias} | Acc]);
	{ok, BaseMT} ->
	    %% Sub flex counter
	    format_cm_mt(T, Grp, [{{BaseMT, MT}, MtAlias} | Acc]);
	_ ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"Flex MeasurementType not found. "
		      "Could not be added to alias table. ~n"
		      "  PmGroup         = ~p~n"
		      "  MeasurementType = ~p~n",
		      [Grp, MT]},
		     ?DEFAULT_BL_TO),
	    format_cm_mt(T, Grp, Acc)
    end.


base_flex_name_get(Grp, MT) ->
    case pmsDb:base_flex_name_get(to_list(Grp), to_list(MT)) of
	{ok, LMT} ->
	    {ok, to_binary(LMT)};
	E ->
	    E
    end.


add_aliases_common(undefined, _) ->
    ok;
add_aliases_common(_, undefined) ->
    ok;
add_aliases_common(AliasTab, CounterMaps) ->
    [add_aliases_grp(ets:lookup(AliasTab, GidAlias), Grp, AliasTab) ||
	{_Gid, GidAlias, _MTs} = Grp <- CounterMaps],
    ok.


add_aliases_grp([], Grp, AliasTab) ->
    add_aliases_new_grp(Grp, AliasTab);
add_aliases_grp([{GidAlias, Gid}], {Gid, GidAlias, MTs}, AliasTab) ->
    [add_alias_mt(ets:lookup(AliasTab, {GidAlias, MTAlias}), 
		  GidAlias, MTid, MTAlias, AliasTab) || 
	{MTid, MTAlias} <- lists:ukeysort(1, MTs)];
add_aliases_grp([{_, _OldGid}], {_Gid, GidAlias, _MTs} = Grp, AliasTab) ->
    ets:select_delete(AliasTab, [{{{GidAlias, '_'}, '_'}, [], [true]}]),
    ets:delete(AliasTab, GidAlias),
    add_aliases_new_grp(Grp, AliasTab).


add_aliases_new_grp({Gid, _GidAlias, _MTs} = Grp, AliasTab) ->
    Res = ets:select(AliasTab, [{{'$1', Gid}, [{is_integer, '$1'}], ['$1']}]),
    add_aliases_new_grp_alias(Res, Grp, AliasTab).


add_aliases_new_grp_alias([], Grp, AliasTab) ->
    add_aliases_new_group_mts(Grp, AliasTab);
add_aliases_new_grp_alias([OldGidAlias], Grp, AliasTab) ->
    ets:select_delete(AliasTab, [{{{OldGidAlias, '_'}, '_'}, [], [true]}]),
    ets:delete(AliasTab, OldGidAlias),
    add_aliases_new_group_mts(Grp, AliasTab).


add_aliases_new_group_mts({Gid, GidAlias, MTs}, AliasTab) ->
    ets:insert(AliasTab, {GidAlias, Gid}),
    [ets:insert(AliasTab, {{GidAlias, MTAlias}, MTid}) || 
	{MTid, MTAlias} <- lists:ukeysort(1, lists:reverse(MTs))].


add_alias_mt([], GidAlias, MTid, MTAlias, AliasTab) ->
    ets:select_delete(AliasTab, [{{{GidAlias, '_'}, MTid}, [], [true]}]),
    ets:insert(AliasTab, {{GidAlias, MTAlias}, MTid});
add_alias_mt([{{_GA, _MTA}, MTid}], _GidAlias, MTid, _MTAlias, _AliasTab) ->
    ok;
add_alias_mt([{Key, _OldMTid}], GidAlias, MTid, MTAlias, AliasTab) ->
    ets:delete(AliasTab, Key),
    ets:select_delete(AliasTab, [{{{GidAlias, '_'}, MTid}, [], [true]}]),
    ets:insert(AliasTab, {{GidAlias, MTAlias}, MTid}).


%%========================================================================
%% decode_value_bundle(AliasTab, ValueBundle) -> ok
%% 
%% 
%%========================================================================
decode_value_bundle_rop(pmi, ValueBundle, _AliasTab) ->
    ValueBundle;

decode_value_bundle_rop(Prot, ValueBundle, AliasTab) ->
    decode_value_bundle(Prot, ValueBundle, AliasTab, undefined).


decode_value_bundle(pmi, ValueBundle, _AliasTab, {LDN, Spec}) ->
    [{to_binary(Group), [{to_binary(LDN), group_sc_vals(MTs, ValueBundle)}]} || 
	{Group, MTs} <-  Spec];

decode_value_bundle(pmi, ValueBundle, _AliasTab, _) ->
    ValueBundle;

decode_value_bundle(_Prot, ValueBundle, AliasTab, _) ->
    Self = self(),
    F = fun(GroupVals) ->
		decode_group_aliases(GroupVals, AliasTab, Self)
	end,
    %% TS1 = os:timestamp(),
    Decoded = decode_aliases(F, ValueBundle),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time decode_aliases: ~p ms~n", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    Decoded.


group_sc_vals(MTs, VB) ->
    F = fun(MT, Acc) ->
		MTBin = to_binary(MT),
		case proplists:get_value(MTBin, VB) of
		    Vals when is_list(Vals) ->
			[{MTBin, Vals} | Acc];
		    _ ->
			Acc
		end
	end,
    lists:foldr(F, [], MTs). 
    
%%--------------------------------------------------------
%% Decode the PM Group aliases
%%--------------------------------------------------------
decode_group_aliases({GroupAlias, LDNValues}, AliasTab, Parent) ->
    case ets:lookup(AliasTab, GroupAlias) of
	[{_, GrpId}] ->
	    F = fun(LDNVals) ->
			decode_ldn_aliases(GroupAlias, LDNVals, AliasTab, 
					   Parent)
		end,
	    [{GrpId, decode_aliases(F, LDNValues)}];
	_ ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_ERROR,
			   {"PmGroupAlias not found ~n"
			    "  PmGroupAlias = ~p~n",
			    [GroupAlias]},
			   ?DEFAULT_BL_TO),
	    []
    end.
    
%%--------------------------------------------------------
%% Decode the MO LDN aliases
%%--------------------------------------------------------
decode_ldn_aliases(GroupAlias, {LDNAlias, MTValues}, AliasTab, Parent) ->
    case gmfI:check_integer(LDNAlias) of
	{ok, LDN} ->
	    %% F = fun(MTVals) ->
	    %% 		decode_mt_aliases(GroupAlias, MTVals, AliasTab, Parent)
	    %% 	end,
	    %% [{to_binary(LDN), decode_aliases(F, MTValues)}];
	    DecMTV = [decode_mt_aliases(GroupAlias, MTVals, AliasTab, Parent) 
	    	      || MTVals <- MTValues],
	    [{to_binary(LDN), lists:append(DecMTV)}];
	_Error ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_ERROR, 
			   {"LDN alias not found ~n"
			    "  LDNAlias = ~p~n",
			    [LDNAlias]},
			   ?DEFAULT_BL_TO),
	    []
    end.

%%--------------------------------------------------------
%% Decode the Measurement Type aliases
%%--------------------------------------------------------
decode_mt_aliases(GrpAlias, {MTAlias, Values}, AliasTab, Parent) ->
    case ets:lookup(AliasTab, Alias = {GrpAlias, MTAlias}) of
	[{_, MTId}] ->
	    [{MTId, Values}];
	Other ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_ERROR, 
			   {"MeasurementTypeAlias ~p not found ~n"
			    "  Found = ~p~n",
			    [Alias, Other]},
			    ?DEFAULT_BL_TO),
	    []
    end.
    
%%--------------------------------------------------------
%% Parallel alias decode function
%%--------------------------------------------------------
decode_aliases(DecodeFun, Aliases) ->
    Fun = fun(Alias) ->
		  DecodeFun(Alias)
	  end,
    %% TS1 = os:timestamp(),
    Decoded = pmsLib:pmapr_delay(Fun, Aliases, 0),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time ~p: ~p~n", 
    %% 		      [Fun, timer:now_diff(TS2, TS1)]}),
    lists:append(Decoded).    


%%========================================================================
%% get_gp_measurements(Subscribers, GP, PmGroups)
%% 
%% Collect and sort the combined measurements for a GP from all subscribers
%%========================================================================
get_gp_measurements(Subscribers, GP, PmGroups) ->
    GPMTabs = [{MTab, JPid} || #subscriber{key = {JPid, SGP}, 
					   meas_tab = MTab,
					   last_gp = LGP} 
				   <- Subscribers, SGP =:= GP, 
			       MTab =/= undefined, not LGP],
    lists:append([get_group_mts(PmGroup, GPMTabs) || PmGroup <- PmGroups]).


get_group_mts(PmGroup, GPMTabs) ->
    case lists:usort(lists:append([group_mts_from_tab(PmGroup, Tab, JPid) || 
				      {Tab, JPid} <- GPMTabs])) of
	[] ->
	    [];
	MTs ->
	    [{PmGroup, MTs}]
    end.


group_mts_from_tab(PmGroup, Tab, JP) ->
    try
	ets:select(Tab, [{{{'$1', '$2'}, '$3'}, [{'==', '$1', PmGroup}], 
			  ['$2']}])
    catch _:_ ->
	    ?LOG_RAM(?SEV_1, 
		     {"MeasTab ~p deleted. PmJob ~p probably terminated.~n"
		      "  Assume no MTs for ~p~n",
		      [Tab, JP, PmGroup]},
		     ?DEFAULT_BL_TO),
	    []
    end.


%%========================================================================
%% get_measurement_aliases(Measured, AliasTab) -> 
%% 
%%========================================================================
get_measurement_aliases(Measured, AliasTab) ->
    TmpTab = ets:new(tmp_tab, []),
    ets:foldl(fun({{_GA, _MTA}, {_, _}}, _) ->
		      ok;
		 ({{GA, MTA}, MT}, _) ->
		      ets:insert(TmpTab, {{GA, MT}, MTA});
		 ({GA, G}, _) ->
		      ets:insert(TmpTab, {G, GA})
	      end, ok, AliasTab),
    Aliases = get_aliases(Measured, TmpTab, []),
    ets:delete(TmpTab),
    Aliases.


get_aliases([], _, Acc) ->
    lists:reverse(Acc);

get_aliases([{Grp, Mts} | T], AliasTab, Acc) ->
    case ssat_grp_aliases(get_alias(Grp, AliasTab), Grp, Mts, AliasTab) of
	{ok, {GrpAlias, MtAliases}} ->
	    get_aliases(T, AliasTab, [{GrpAlias, MtAliases} | Acc]);
	_Error ->
	    get_aliases(T, AliasTab, Acc)
    end.


ssat_grp_aliases({ok, GrpAlias}, Grp, Mts, AliasTab) ->
    case ssat_mt_aliases(Mts, GrpAlias, AliasTab) of
	{ok, MtAliases} ->
	    {ok, {GrpAlias, MtAliases}};
	Error ->
	    ?LOG_RAM(?SEV_ERROR,
		     {"No MT aliases found for Group ~p~n", [Grp]},
		     ?DEFAULT_BL_TO),
	    Error
    end;

ssat_grp_aliases(Error, _Grp, _Mts, _AliasTab) ->
    Error.


ssat_mt_aliases(Mts, GrpAlias, AliasTab) ->
    Aliases = [get_alias({GrpAlias, Mt}, AliasTab) || Mt <- Mts],
    case [A || {ok, A} <- Aliases] of
	[]  -> 
	    {error, no_mt_aliases};
	MtA -> 
	    {ok, MtA}
    end.


get_alias(Key, AliasTab) ->
    case ets:lookup(AliasTab, Key) of
	[{_Key, Val}] ->
	    {ok, Val};
	_  ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"No alias found for ~p~n", [Key]},
		     ?DEFAULT_BL_TO),
	    {error, {no_alias_found, Key}}
    end.

%%========================================================================
%% get_sc_aliases(Measured, AliasTab, []) -> 
%% 
%%========================================================================
get_sc_aliases([], _, Acc) ->
    lists:reverse(Acc);

get_sc_aliases([{Grp, Mts} | T], AliasTab, Acc) ->
    GroupAliasRes = get_grp_sc_alias(to_binary(Grp), AliasTab),
    case ssat_grp_sc_aliases(GroupAliasRes, Grp, Mts, AliasTab) of
	{ok, {GrpAlias, MtAliases}} ->
	    get_sc_aliases(T, AliasTab, [{GrpAlias, MtAliases} | Acc]);
	_Error ->
	    get_sc_aliases(T, AliasTab, Acc)
    end.


ssat_grp_sc_aliases({ok, GrpAlias}, Grp, Mts, AliasTab) ->
    case ssat_mt_sc_aliases(Mts, GrpAlias, AliasTab) of
	{ok, MtAliases} ->
	    {ok, {GrpAlias, MtAliases}};
	Error ->
	    ?LOG_RAM(?SEV_ERROR,
		     {"No MT aliases found for Group ~p~n", [Grp]},
		     ?DEFAULT_BL_TO),
	    Error
    end;

ssat_grp_sc_aliases(Error, _Grp, _Mts, _AliasTab) ->
    Error.


ssat_mt_sc_aliases(Mts, GrpAlias, AliasTab) ->
    Aliases = [get_mt_sc_alias(to_binary(Mt), GrpAlias, AliasTab) || Mt <- Mts],
    case [A || {ok, {_, A}} <- Aliases] of
	[]  -> 
	    {error, no_mt_aliases};
	MtA -> 
	    {ok, MtA}
    end.


get_grp_sc_alias(Value, AliasTab) ->
    Match = ets:select(AliasTab, [{{'$1', '$2'}, 
				   [{'==', '$2', Value}], 
				   ['$1']}]),
    get_sc_alias_rc(Match, Value).


get_mt_sc_alias(Value, GrpAlias, AliasTab) ->
    Match = ets:select(AliasTab, [{{{'$1', '$2'}, '$3'},
				   [{'==', '$1', GrpAlias},
				    {'==', '$3', {const, Value}}], 
				   [{{'$1', '$2'}}]}]),
    get_sc_alias_rc(Match, Value).

    
get_sc_alias_rc([Key], _) ->
    {ok, Key};

get_sc_alias_rc(_, Value) -> 
    ?LOG_RAM(?SEV_WARNING, 
	     {"No alias found for ~p~n", [Value]},
	     ?DEFAULT_BL_TO),
    {error, {no_alias_found, Value}}.

%%===========================================================================
%% Misc functions
%%===========================================================================
log_msg(_, _) -> ok.
log_loop(_)   -> ok.

choose(true,  T, _) -> T;
choose(false, _, F) -> F.

get_next_req_id(N) when N < ?MAX_REQ_ID ->
    N + 1;
get_next_req_id(_N) ->
    1.

to_binary(Term) ->
    pmsLib:to_binary(Term).


to_list(Term) ->
    pmsLib:to_list(Term).

%%========================================================================
%% get_minus_offset(GP) -> Interval
%%
%% Count a time offset that is used when counting the next gp sync time.
%% This offset is used to be sure that any drifting clock does not
%% cause that we miss the 00 wall-clock.
%% The clock can drift 1% in the worst case.
%%========================================================================
get_minus_offset(?TimePeriod_TEN_SECONDS)    -> 0;
get_minus_offset(?TimePeriod_THIRTY_SECONDS) -> 0;
get_minus_offset(?TimePeriod_ONE_MIN)        -> 0;
get_minus_offset(?TimePeriod_FIVE_MIN)       -> -30;
get_minus_offset(?TimePeriod_FIFTEEN_MIN)    -> -90;
get_minus_offset(?TimePeriod_THIRTY_MIN)     -> -180;
get_minus_offset(?TimePeriod_ONE_HOUR)       -> -500;
get_minus_offset(?TimePeriod_TWELVE_HOUR)    -> -6000;
get_minus_offset(?TimePeriod_ONE_DAY)        -> -12000.



%%===========================================================================
%% 
%%===========================================================================
get_proc_type(FlexGroups, CommonGroups) ->
    Res = gpt(FlexGroups, CommonGroups),
    ?LOG_RAM(?SEV_5, {"Process type = ~p~n", [Res]}),
    Res.


%% No groups, wait for counter maps message
gpt([], []) ->
    {?PROC_TYPE_UNDEFINED, []};
%% All groups are flex groups 
gpt(FlexGroups, FlexGroups) ->
    {?PROC_TYPE_FLEX, FlexGroups};
%% All groups are non-flex groups
gpt([], Groups) ->
    {?PROC_TYPE_COMMON, Groups};
%% Mix of flex and non-flex groups
gpt(FlexGroups, Groups) ->
    ?LOG_RAM(?SEV_ERROR, 
	     {"Mixture of flex and non-flex PmGroups~n"
	      "Removing all flex PmGroups ~p~n",
	      [FlexGroups]}),
	    sysInitI:error_report([{module, ?MODULE},
				   {function, initialize},
				   {error, {"Mix of flex and non-flex PmGroups, "
					    "removing all flex PmGroups", 
					    FlexGroups}}]),
    {?PROC_TYPE_COMMON, Groups}.

%%===========================================================================
%% 
%%===========================================================================
update_proc_type(Type, FlexGrps, CommonGrps) ->
    Res = upt(Type, FlexGrps, CommonGrps),
    ?LOG_RAM(?SEV_5, {"Updated process type = ~p~n", [Res]}),
    Res.

upt(?PROC_TYPE_UNDEFINED, [], []) ->
    ?PROC_TYPE_UNDEFINED;
upt(?PROC_TYPE_UNDEFINED, [], _) ->
    ?PROC_TYPE_COMMON;
upt(?PROC_TYPE_UNDEFINED, _, []) ->
    ?PROC_TYPE_FLEX;
upt(?PROC_TYPE_COMMON, [], _) ->
    ?PROC_TYPE_COMMON;
upt(?PROC_TYPE_FLEX, FlexGrps, []) when length(FlexGrps) > 0 ->
    ?PROC_TYPE_FLEX;
upt(Type, FlexGrps, CommonGrps) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Process Type~n"
	      "  Previous Type  = ~p~n"
	      "  New FlexGroups = ~p~n"
	      "  Common Groups  = ~p~n",
	      [Type, FlexGrps, CommonGrps]}),
    Type.


num_of_mts(MeasAliases) ->
    length(lists:append([MTs || {_G, MTs} <- MeasAliases])).   


num_of_grps(MeasAliases) ->
    length(MeasAliases).   


num_of_sc_data_vals([{_G1, [{_LDN1, [{_MT, _Vals} | _]} | _]} | _] = PMData) ->
    LDNVals = lists:append([LDNs || {_G, LDNs} <- PMData]),
    length(lists:append([Vals || {_LDN, Vals} <- LDNVals]));

num_of_sc_data_vals(PMData) ->
    length(PMData).

num_of_data_vals(PMData) ->
    LDNVals = lists:append([LDNs || {_G, LDNs} <- PMData]),
    length(lists:append([Vals || {_LDN, Vals} <- LDNVals])).

num_of_data_ldns(PMData) ->
    length(lists:append([LDNs || {_G, LDNs} <- PMData])).   


num_of_data_grps(PMData) ->
    length(PMData).   

%%===========================================================================
%% log truncate functions
%%===========================================================================
log_trunc_ma(MeasAliases) ->
    ltma(MeasAliases, 0, pmsDb:pms_env_get(log_max), []).

ltma([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltma(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltma([], _, _, Acc) ->
    lists:reverse(Acc);
ltma([{Grp, MTs} | T], N, Max, Acc) ->
    ltma(T, N + 1, Max, [{Grp, pmsLib:log_trunc_list(MTs)} | Acc]).

				  

format_loop(Loop) ->
    F       = record_info(fields, loop),
    [_ | L] = tuple_to_list(Loop), 
    lists:zip(F,L).

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



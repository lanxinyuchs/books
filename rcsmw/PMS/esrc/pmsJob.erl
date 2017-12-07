%%% ------------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ------------------------------------------------------------------------
%%% %CCaseFile:	pmsJob.erl %
%%% @private
%%% 
%%% Description:
%%%
%%% pmsJob process is started, from pmsJobGroup, when a PM Job instance 
%%% is created. pmsJob contains a collection of counters to be measured.
%%% 
%%% pmsJob sends a subscribe message, when it is in state active,
%%% and an unsubscribe when it switches from active to passive state
%%% to all application instances handling the measured counters.
%%%
%%% To find all application instances pmsJob reads #pmsAppRegistry.
%%% That table contains PIDs to all pmsAppJob processes and what counter
%%% each of them is handling.
%%% 
%%% The (un)subrscibe requests are not sent directly to the application
%%% but to the pmsAppJob process. There can be several PM Jobs active 
%%% simultaneous measuring the same counter(s). The pmsAppJob process 
%%% builds a union of all counters to be measured and sends the union 
%%% in the subscribe message to the application instance.
%%% 
%%% pmsAppJob is responsible to send the report request at granularity
%%% periods to the application. It also receives the measured data
%%% which is stored in #pmsMeasData record. pmsJob will read the
%%% received data from that record at GP + GP/8 and builds a measInfo
%%% in xml-format to be used by pmsJobGroup when building the rop file 
%%% at GP + GP/4 (the ROP file should be ready at GP + GP/3).
%%% ------------------------------------------------------------------------
-module(pmsJob).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R12A/1').
-date('2017-11-30').
-author('eolaand').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    --------------------------------------
%%% R1A/1      2013-01-18 uabesvi     Created
%%% R2A/10     2013-03-18 uabesvi     Cleaned up
%%% R5A/1      2015-11-11 uabesvi     Created r5 branch
%%% R6A/3      2016-05-16 eolaand     Introduce worker process per PmGroup
%%% R6A/8      2016-06-13 eolaand     Change PmGroup and MT Id to binary
%%% R6A/10     2016-08-31 eolaand     Report current state to pmsJobGroup
%%% ------------------------------------------------------------------------
%%%
%%% ------------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ------------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ------------------------------------------------------------------------

-export([create/1]).
-export([update/3]).
-export([takeover/4]).
-export([delete/2]).
-export([terminate/2]).

-export([test_update/3]).

%% Not used: it is not allowed to update a MR as today  
%%-export([mr_update/1]).
%%-export([mr_delete/2]).

-export([meas_data/4, meas_data/5]).

%%-export([get_current_state/1]).
-export([get_loop_data/1]).
-export([get_proc_info/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/2]).

%% -compile(export_all).

-include("RcsPm.hrl").
-include("pms.hrl").

-record(loop, {job_id, 
	       job_state,           %% current job state
	       req_state,           %% requested job state
	       meas_tab,            %% ets table with measured types.
	       mi_slave_tab,        %% ets table with measInfo slave processes.
	       gp         = 0,      %% granularity period   
	       rp         = 0,      %% reporting period
	       cspec_gp_delta = [], %% Added and removed MRs since prev GP
	       job_group,           %% {ParentPid, MonitorRef}
	       app_pids   = [],     %% list of apps expected to send meas info
	       gp_end,              %% last gp end time stamp
	       deleted = false,
	       n_exp_meas_data = 0,
	       n_pm_groups = 0
	      }).

-define(SEND_TO,   5000).
-define(CREATE_TO, 5000).

-define(MEAS_INFO_TICK, meas_info_tick).
-define(GP_SYNCED,      gp_synced_job).
-define(ET, pmsLib:epoch_time()).

-define(MI_ID(PmGroup), "PM=1,PmGroup=" ++ PmGroup).
-define(ME, "ManagedElement").

-define(DEFAULT_BL_TO, 10).
-define(DELAY_TIME, 5000).
-define(MIN_DELAY_SLICE, 10).
-define(MAX_DELAY_SLICE, 200).
-define(MIN_REM_TIME, 15000).


%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% create(PmJob) -> {ok, Pid}
%%
%% Create a new PM Job process.
%%========================================================================
create(PmJob) ->
    Self = self(),
    {ok, spawn(fun() -> init(Self, PmJob) end)}.


%%========================================================================
%% update(Pid, PmJob, Old) -> ok | {error, Reason}
%%
%% Update a PM Job.
%%========================================================================
update(Pid, PmJob, Old) ->
    send(Pid, update, {PmJob, Old}).

%%========================================================================
%% test_update(Pid, PmJobId) -> ok | {error, Reason}
%%
%% Update a PM Job.
%%========================================================================
test_update(Pid, PmJob, Old) ->
    send(Pid, update, {PmJob, Old}).
%%    send(Pid, test_update, {PmJob, Old}).

%%========================================================================
%% takeover(Pid, OldJobGroup, NewJobGroup, TakeoverPid) -> ok | {error, Reason}
%%
%% Update a PM Job.
%%========================================================================
takeover(Pid, OldJobGroup, NewJobGroup, TakeoverPid) ->
    Pid ! {takeover, self(), OldJobGroup, NewJobGroup, TakeoverPid},
    receive
	{takeover_res, Res} ->
	    Res
    after 60000 ->
	    {error, {takeover, timeout}}
    end.

%%========================================================================
%% delete(Pid, PmJob) -> ok
%%
%% A PM Job is deleted.
%%========================================================================
delete(Pid, PmJob) ->
    send(Pid, delete, PmJob).

%%========================================================================
%% terminate(Pid, PmJob) -> ok
%%
%% A PM Job is terminate.
%%========================================================================
terminate(Pid, PmJob) ->
    send(Pid, terminate, PmJob).

%%========================================================================
%% get_loop_data(Pid) -> ok
%%========================================================================
get_loop_data(Pid) ->
    Pid ! {get_loop_data, self()},
    receive
	{loop_data, Res} ->
	    Res
    after ?SEND_TO ->
	    undefined
    end.
    
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

%%-------------------------------------------------------------
%% Send the message to the process and wait for reply
%%-------------------------------------------------------------
send(Pid, Msg, Data) ->
    send2(process_info(Pid), Pid, Msg, Data).

send2(undefined, Pid, _, _) ->
    {error, {no_process, {?MODULE, Pid}}};    
send2(_, Pid, Msg, Data) ->
    Pid ! {Msg, self(), Data},
    ok.
    

%%========================================================================
%% meas_data(Pid, UserPid, Data, CSpec) -> ok
%%
%% Recieve measurement data
%%========================================================================
meas_data(JobPid, UserPid, Data, CSpec) ->
    meas_data(JobPid, UserPid, Data, CSpec, true).

meas_data(JobPid, UserPid, Data, CSpec, Log) ->
    MRef = erlang:monitor(process, JobPid),
    JobPid ! {meas_data, UserPid, Data, CSpec},
    receive
	{ok, JobPid} ->
	    erlang:demonitor(MRef, [flush]),
	    ok;
	{'DOWN', MRef, process, JobPid, Reason} 
	  when Log andalso Reason =/= normal ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"pmsJob ~p died unexpectedly, reason: ~p~n",
		      [JobPid, Reason]}),
	    ok;
	{'DOWN', MRef, process, JobPid, _Reason} ->
	    ok
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% init(Pid, PmJob) -> loop/1
%% 
%% Init function for the Job process
%% 
%% Start supervising if application instances (de)attach by subscribing
%% to #pmsAppRegistry.
%% 
%% Send subscribe request to attached applications, if PM Job is active.
%% In that case also start the measInfo tick.
%%========================================================================
init(Pid, #pmJob{pmJobId           = JobId, 
		 requestedJobState = InJobState,
		 granularityPeriod = GP,
		 reportingPeriod   = RP} = PmJob) ->
    put(name, {?MODULE, JobId}), %% used by pms_cli to print process data
    log_msg(init, JobId),
    ReqJobState = req_state(InJobState),
    LogStr = io_lib:format("Started.~n"
			   "  Job = ~n~s~n",
			   [pp(PmJob)]),
    ?LOG_RAM(?SEV_1, {LogStr, []}), 
    MRef = erlang:monitor(process, Pid),
    pmsDb:mnesia_subscribe({table, pmsAppRegistry, detailed}),
    Measured = get_measurements(JobId),
    MTab = ets:new(measured, [protected, ordered_set]),
    %% MTab = ets:new(measured, []),
    measured_to_tab(Measured, MTab), 
    MITab = ets:new(meas_info_slaves, []),
    start_meas_info_slaves(Measured, MITab, JobId, RP, GP, MTab),
    update_current_state_and_def_vals(pmsDb:pm_job_get(JobId),
				      ?JobState_STOPPED),
    JobState = send_subscribe(JobId, 
			      ReqJobState, 
			      Measured,
			      [], 
			      MTab,
			      GP),
    pmsJobGroup:job_op_state(Pid, JobId, JobState, self()),
    AppJobs = get_app_jobs(MTab),
    NAppJobs = length(AppJobs),
    Loop = #loop{job_id           = JobId, 
		 job_state        = JobState,
		 req_state        = ReqJobState,
		 meas_tab         = MTab,
		 mi_slave_tab     = MITab,
		 gp               = GP,
		 rp               = RP,
		 job_group        = {Pid, MRef},
		 n_exp_meas_data  = NAppJobs,
		 n_pm_groups      = length(Measured)
		},
    ?LOG_RAM(?SEV_5,
	     ?LFUN({"Measured after init.~n"
		    "  Requested = ~p~n",
		    [log_trunc_grp(Measured)]})),
    loop(Loop).


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% loop(JobId, TimeRef) 
%%
%% Job loop. Loop until the PM Job is deleted.
%% 
%% Until then:
%%  - check if PM Job attributes are updated
%%  - check if measurement readers are updated
%%  - genereate measInfo results if the job is active
%%  
%%========================================================================
loop(#loop{job_id       = JobId, 
	   meas_tab     = MeasTab,
	   mi_slave_tab = MITab,
	   gp           = GP,
	   rp           = RP,
	   job_group    = {JobGrpPid, JobGrpRef},
	   deleted      = Deleted,
	   app_pids     = AppPids,
	   gp_end       = GPEnd
	  } = Loop) ->

    ?LOG_RAM(?SEV_5,
	     {"JOB LOOP ~n"
	      "  JobId   = ~p~n"
	      "  GP      = ~p~n"
	      "  Deleted = ~p~n"
	      "  AppPids = ~p~n"
	      "  GpEnd   = ~p~n",
	      [JobId, GP, Deleted, AppPids, GPEnd]}),
    log_loop(Loop),

    receive
	%%----------------------------------------------------------
	%% meas data 
	%%----------------------------------------------------------
	{meas_data, _From, {RGP, RGPEnd, MeasVals, FF}, _CSpec} = Msg ->
	    ?LOG_RAM([{?SEV_1, 
		       {" meas_data ~n"
			"  JobId    = ~p~n",
			[JobId]}},
		      {?SEV_5, 
		       ?LFUN({" meas_data ~n"
			      "  JobId    = ~p~n"
			      "  RGP      = ~p~n"
			      "  RGPEnd   = ~p~n"
			      "  FF       = ~p~n"
			      "  MeasVals = ~p~n",
			      [JobId, RGP, RGPEnd, FF, 
			       log_trunc_mv(MeasVals)]})}
		     ]),
	    log_msg(Msg, {GP, GPEnd}),
	    Res = handle_meas_data(Msg, Loop),
	    %% send_reply(From, {ok, self()}),
	    case Res of
		terminate -> 
		    ?LOG_RAM(?SEV_1,
			     {"PmJob deleted after meas_data~n"
			      "  JobId = ~p~n"
			      "  GP    = ~p~n", 
			      [JobId, GP]}),
		    clean_up_and_exit(normal, Loop);
		NewLoop -> 
		    loop(NewLoop)
	    end;
	
	%%-------------------------------------------------------------
	%% PM Job update
	%%-------------------------------------------------------------
	{update = Msg, 
	 _UserPid, 
	 {#pmJob{requestedJobState = UReqState} = NewJob, [OldJob]}} ->
	    log_msg(Msg, {NewJob, OldJob}),
	    NewReqState = req_state(UReqState),
	    LogStr = io_lib:format("Job update:~n"
				   "  NewJob = ~n~s~n  OldJob = ~n~s~n",
				   [pp(NewJob), pp(OldJob)]),
	    ?LOG_RAM(?SEV_1, {LogStr, []}), 
	    LoopSync = gp_sync_job_update(NewJob, OldJob, Loop),
	    NewMeasured  = get_measurements(JobId),
	    Measured = measured_from_tab(MeasTab),
	    measured_to_tab(NewMeasured, Measured, MeasTab),
	    update_meas_info_slaves(NewMeasured, Measured, MITab, JobId, RP, 
				    GP, MeasTab),
	    NewJobState = send_subscribe(JobId,
					 NewReqState, 
					 NewMeasured,
					 Measured,
					 MeasTab,
					 GP),
	    report_op_state_change(NewJobState, Loop),
	    NAppJobs = length(get_app_jobs(MeasTab)),
	    NewCSpecDelta = 
		send_meas_info_if_last_app(NAppJobs, JobId, GP, GPEnd, Loop),
	    NExpected = get_updated_n_expected(Loop),
	    NPmGroups = length(NewMeasured), 
	    loop(LoopSync#loop{req_state = NewReqState,
			       job_state = NewJobState,
			       cspec_gp_delta = NewCSpecDelta,
			       n_exp_meas_data = NExpected,
			       n_pm_groups = NPmGroups});

	%%-------------------------------------------------------------
	%% PM job takeover
	%%-------------------------------------------------------------
	{test_update = Msg, NewJob, _OldJob} ->
	    log_msg(Msg, NewJob),
	    loop(Loop);

	%%-------------------------------------------------------------
	%% PM job takeover
	%%-------------------------------------------------------------
	{takeover = Msg, UserPid, OldJobGroup, NewJobGroup, TakeoverPid} ->
	    log_msg(Msg, TakeoverPid),
	    ?LOG_RAM(?SEV_1,
		     {"Job takeover:~n"
		      "  New JobGroup = ~p ~p~n"
		      "  Old JobGroup = ~p ~p~n",
		      [TakeoverPid, NewJobGroup, JobGrpPid, OldJobGroup]}), 
	    erlang:demonitor(JobGrpRef, [flush, info]),
	    MRef = erlang:monitor(process, TakeoverPid),
	    OpState = Loop#loop.job_state,
	    pmsJobGroup:over_taken(TakeoverPid, JobId, GP, OpState, self()),
	    UserPid ! {takeover_res, ok},
	    loop(Loop#loop{job_group = {TakeoverPid, MRef}});

	%%-------------------------------------------------------------
	%% Delete or terminate PmJob
	%%-------------------------------------------------------------
	{delete = Msg, _UserPid, PmJob} ->
	    log_msg(Msg, PmJob),
	    NewLoop = Loop#loop{job_state = ?JobState_STOPPED,
				deleted   = true},
	    DelRes = handle_delete(AppPids, 
				   pmsLib:gp_end_time(GP) == GPEnd,
				   JobId,
				   MeasTab,
				   GP),
	    case DelRes of
		terminate -> 
		    ?LOG_RAM(?SEV_1,
			     {"PmJob deleted. (delete)~n"
			      "  JobId = ~p~n"
			      "  GP    = ~p~n", 
			      [JobId, GP]}),
		    clean_up_and_exit(normal, Loop);
		pending -> 
		    ?LOG_RAM(?SEV_1,
			     {"PmJob delete pending. ~n"
			      "  JobId = ~p~n"
			      "  GP    = ~p~n", 
			      [JobId, GP]}),
		    loop(NewLoop#loop{deleted = true})
	    end;

	%%-------------------------------------------------------------
	%% Terminate PmJob
	%%-------------------------------------------------------------
	{terminate = Msg, _UserPid, PmJob} ->
	    log_msg(Msg, PmJob),
	    erlang:demonitor(JobGrpRef, [flush, info]),
	    Measured = measured_from_tab(MeasTab),
	    send_subscribe(JobId, ?JobState_STOPPED, Measured, Measured, 
			   MeasTab, GP),
	    ?LOG_RAM(?SEV_1, {"Terminated.~n"
			      "  JobId = ~p~n"
			      "  GP    = ~p~n", 
			      [JobId, GP]}),
	    clean_up_and_exit(terminated, Loop);

	%%-------------------------------------------------------------
	%% Mnesia table events, only subscribing to #pmsAppRegistry
	%%-------------------------------------------------------------
	{mnesia_table_event, Event} ->
	    ?LOG_RAM(?SEV_5, 
		     {"Mnesia ~p event, JobId = ~p~n",
		      [element(1, Event), JobId]},
		     pmsLib:get_report_offset(GP)),
	    NewLoop = check_event(Event, Loop),
	    report_op_state_change(NewLoop#loop.job_state, Loop),
	    loop(NewLoop);

	%%-------------------------------------------------------------
	%% PmsJobGroup died, let this process also die
	%%-------------------------------------------------------------
	{'DOWN', JobGrpRef, process, JobGrpPid, Reason} ->
	    Msg = {JobId, JobGrpRef, process, JobGrpPid, Reason, Loop},
	    ?LOG_RAM(?SEV_WARNING, 
		     {"DOWN message from JobGroup ~p~n", [Msg]}),
	    clean_up_and_exit(job_group_down_msg, Loop);

	%%-------------------------------------------------------------
	%% PmsJobMeasInfo died, remove from tab and recover at next GP
	%%-------------------------------------------------------------
	{'DOWN', Ref, process, SlavePid, Reason} ->
	    case ets:lookup(MITab, SlavePid) of
		[] ->
		    ok;
		_ ->
		    ets:delete(MITab, SlavePid),
		    Msg = {JobId, Ref, process, SlavePid, Reason, Loop},
		    ?LOG_RAM(?SEV_ERROR, 
			     {"DOWN message from JobMeasInfo slave ~p~n", 
			      [Msg]})
	    end,
	    loop(Loop);

	%%-------------------------------------------------------------
	%% Read loop data
	%%-------------------------------------------------------------
	{get_loop_data, UserPid} ->
	    UserPid ! {loop_data, format_loop(Loop)},
	    io:format("### ~p MeasTab  ~n~p~n", 
		      [self(), ets:tab2list(MeasTab)]),
	    loop(Loop);

	%%-------------------------------------------------------------
	%% Read process info
	%%-------------------------------------------------------------
	{get_proc_info, UserPid} ->
	    {_, _, _, Jid} = JobId,
	    Res = {Jid, 
		   [{loop_size, erts_debug:flat_size(Loop)},
		    {proc_info, pmsLib:get_proc_info(self())},
		    {ets_tables, [{meas_tab, ets:info(MeasTab, memory)},
				  {mi_slave_tab, ets:info(MITab, memory)}]}]},
	    UserPid ! {proc_info, Res},
	    loop(Loop);

	%%-------------------------------------------------------------
	%% Unknown messages
	%%-------------------------------------------------------------
	X ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"~p Received unknown message ~p~n", [jobId, X]}),
	    log_msg(X, unknown_message),
	    loop(Loop)
    end.

%%========================================================================
%% send_reply(Pid, Res) ->
%%     ok
%%========================================================================
send_reply(undefined, _Res) ->
    ok;

send_reply(Pid, Res) ->
    Pid ! Res.

%%========================================================================
%% start_meas_info_slaves(Measured, MITab, JobId, RP, GP, MTab) ->
%%     ok
%%========================================================================
start_meas_info_slaves(Measured, MITab, JobId, RP, GP, MeasTab) ->
    F = fun(PmGroup) ->
		start_meas_info_slave(MITab, JobId, PmGroup, RP, GP, MeasTab)
	end,
    lists:foreach(F, [Group || {Group, _MTs} <- Measured]).
    

start_meas_info_slave(MITab, JobId, PmGroup, RP, GP, MeasTab) ->
    {ok, Pid} = pmsJobMeasInfo:start(JobId, PmGroup, RP, GP, MeasTab),
    erlang:monitor(process, Pid),
    ets:insert(MITab, {PmGroup, Pid}).

%%========================================================================
%% restart_meas_info_slaves(PmGroups, MITab, JobId, RP, GP, MTab) ->
%%     ok
%%========================================================================
restart_meas_info_slaves(PmGroups, MITab, JobId, RP, GP, MeasTab) ->
    stop_meas_info_slaves(MITab),
    ets:delete_all_objects(MITab),
    F = fun(PmGroup) ->
		start_meas_info_slave(MITab, JobId, PmGroup, RP, GP, MeasTab)
	end,
    lists:foreach(F, PmGroups).

%%========================================================================
%% stop_meas_info_slaves(MITab) ->
%%     ok
%%========================================================================
stop_meas_info_slaves(MITab) ->
    F = fun({_PmGroup, Pid}, _Acc) ->
	    stop_meas_info_slave(Pid)
	end,
    ets:foldl(F, ok, MITab).
    

stop_meas_info_slave(PmGroup, MITab) ->
    case ets:lookup(MITab, PmGroup) of
	[{_, Pid}] ->
	    stop_meas_info_slave(Pid);
	_ ->
	    ok
    end,
    ets:delete(MITab, PmGroup).   
    

stop_meas_info_slave(Pid) ->
    pmsJobMeasInfo:stop(Pid).

%%========================================================================
%% update_meas_info_slaves(Measured, MITab, JobId, RP, GP, MTab) ->
%%     ok
%%========================================================================
update_meas_info_slaves(Measured, Measured, _MITab, _JobId, _RP, _GP, _MTab) ->
    ok;

update_meas_info_slaves(NewMeasured, Measured, MITab, JobId, RP, GP, MTab) ->
    NewGroups = groups_from_measured(NewMeasured),
    OldGroups = groups_from_measured(Measured),
    FRm = fun(PmGroup) ->
		  stop_meas_info_slave(PmGroup, MITab)
	  end,
    lists:foreach(FRm, OldGroups -- NewGroups),
    FAdd = fun(PmGroup) ->
		   start_meas_info_slave(MITab, JobId, PmGroup, RP, GP, MTab)
	   end,
    lists:foreach(FAdd, NewGroups -- OldGroups).
    

%%========================================================================
%% report_op_state_change(NewOpState, Loop) ->
%%     ok
%%========================================================================
report_op_state_change(NewJobState, #loop{job_state = JobState,
					  job_group = {JobGrp, _Ref},
					  job_id = JobId}) 
  when NewJobState =/= JobState ->
    pmsJobGroup:job_op_state(JobGrp, JobId, NewJobState, self());

report_op_state_change(_NewJobState, _Loop) ->
    ok.

%%========================================================================
%% handle_delete(AppPids, SameGP, JobId, MeasTab, GP) ->
%%     terminate | pending
%%========================================================================
%%----------------------------------------------------------
%% If AppPids is empty noone will send meas data anymore.
%% 
%% If the current and previous gp_end timestamps are equal then
%% all appJobs have sent their data for the current GP.
%% The current GP is the last and no ROP data is collected for 
%% the last (incomplete) RP.
%%----------------------------------------------------------
handle_delete(AppPids, SameGP, JobId, MeasTab, GP) 
  when AppPids == [] orelse SameGP ->
    Measured = measured_from_tab(MeasTab),
    send_subscribe(JobId, ?JobState_STOPPED, Measured, Measured, MeasTab, GP),
    ?LOG_RAM(?SEV_1, {"Deleted.~n"
		      "  JobId = ~p~n"
		      "  GP    = ~p~n", 
		      [JobId, GP]}),
    terminate;
%%----------------------------------------------------------
%% Wait for the measurement data for the previous GP 
%% (which is the last complete GP).
%%----------------------------------------------------------
handle_delete(_, _, JobId, MeasTab, GP) ->
    Measured = measured_from_tab(MeasTab),
    send_subscribe(JobId, ?JobState_STOPPED, Measured, Measured, MeasTab, GP),
    ?LOG_RAM(?SEV_1, 
	     {"Pending delete.~n"
	      "  JobId = ~p~n"
	      "  GP    = ~p~n", 
	      [JobId, GP]}),
    pending.


%%========================================================================
%% handle_meas_data(MeasData, Loop) -> Loop | terminate
%% 
%% Measurement data receive from pmsAppJob[2]
%%========================================================================
%%----------------------------------------------------------
%% FF and no AppPids
%% 
%% The first app has started before the GP. 
%% Calculate the AppPid list (for the current GP)
%% 
%% If there are other apps starting for the same GP
%% they will be discarded in the clause below.
%%----------------------------------------------------------
handle_meas_data({meas_data, From, {RGP, RGPEnd, MeasVals, true = FF}, CSpec},
		 #loop{job_group = {JobGrpPid, _},
		       meas_tab = MeasTab,
		       deleted  = Deleted,
		       app_pids = []} = Loop) ->
    Discarded = pmsLib:choose(MeasVals == [], 
			      "Empty meas vals sent to job group~n",
			      "Meas data discarded~n"),
    ?LOG_RAM(?SEV_1,
	     ?LFUN({"Measurement data when AppPids == [] and FF == true. ~n" 
		    ++ Discarded ++ 
		    "  AppJob = ~p~n"
		    "  Data   = ~p~n"
		    "  CSpec  = ~p~n", 
		    [From, {RGP, RGPEnd, log_trunc_mv(MeasVals), FF}, CSpec]})),
    MeasVals == [] andalso 
	pmsJobGroup:meas_info(JobGrpPid, self(), RGP, RGPEnd, <<>>),
    send_reply(From, {ok, self()}),
    NewAppPids = get_app_jobs(MeasTab),
    case Deleted of
	true ->
	    terminate;
	_ ->
	    Loop#loop{cspec_gp_delta = [], 
		      app_pids  = NewAppPids,
		      gp_end    = RGPEnd}
    end;

%%----------------------------------------------------------
%% FF and the GpEnd received is the same
%% as the GpEnd in the loop
%% 
%% Another app has started before the GP.
%% 
%% No need to send the meas data to job group
%%----------------------------------------------------------
handle_meas_data({meas_data, From, {RGP, GPEnd, MeasVals, true = FF}, CSpec},
		 #loop{gp_end   = GPEnd,
		       app_pids = AppPids} = Loop) ->
    ?LOG_RAM(?SEV_1,
	     ?LFUN({"Measurement data when yet another app has started.~n"
		    "  AppPids = ~p~n"
		    "  AppJob  = ~p~n"
		    "  Data    = ~p~n"
		    "  CSpec   = ~p~n", 
		    [AppPids, From, {RGP, GPEnd, log_trunc_mv(MeasVals), FF}, 
		     CSpec]})),
    send_reply(From, {ok, self()}),
    Loop;

%%----------------------------------------------------------
%% not FF
%% 
%% Save the data in meas_data
%%----------------------------------------------------------
handle_meas_data({meas_data, From, {RGP, RGPEnd, MeasVals, false = FF}, CSpec},
		 #loop{app_pids   = AppPids} = Loop) ->
    ?LOG_RAM([{?SEV_1, 
	       {"Measurement data. FF not set.~n"
		"  AppPids = ~p~n"
		"  AppJob  = ~p~n",
		[AppPids, From]}},
	      {?SEV_5,
	       ?LFUN({"Measurement data. FF not set.~n"
		      "  AppPids = ~p~n"
		      "  AppJob  = ~p~n"
		      "  Data    = ~p~n"
		      "  CSpec   = ~p~n", 
		      [AppPids,
		       From, 
		       {RGP, RGPEnd, log_trunc_mv(MeasVals), FF}, 
		       CSpec]})}]),
    NewCSpecDelta = acc_meas_vals(RGPEnd, MeasVals, FF, CSpec, Loop),
    send_reply(From, {ok, self()}),
    Loop#loop{cspec_gp_delta = NewCSpecDelta};

%%-----------------------------
%% FF last app
%% 
%% waiting to be deleted
%%-----------------------------
handle_meas_data({meas_data, From, {RGP, RGPEnd, MeasVals, FF}, CSpec},
		 #loop{job_id   = JobId, 
		       deleted  = true,
		       app_pids = [From] = AppPids
		      } = Loop) ->
    ?LOG_RAM([{?SEV_1, 
	       {"Measurement data: FF and deleted~n"
		"  AppPids = ~p~n"
		"  AppJob  = ~p~n",
	        [AppPids, From]}},
	      {?SEV_5, 
	       ?LFUN({"Measurement data: FF and deleted~n"
		      "  AppPids = ~p~n"
		      "  AppJob  = ~p~n"
		      "  Data    = ~p~n"
		      "  CSpec   = ~p~n", 
		      [AppPids, From, 
		       {RGP, RGPEnd, log_trunc_mv(MeasVals), FF}, 
		       CSpec]})}]),
    NewCSpecDelta = acc_meas_vals(RGPEnd, MeasVals, FF, CSpec, Loop),
    send_reply(From, {ok, self()}),
    send_meas_info(JobId, 
		   RGP, 
		   RGPEnd, 
		   Loop#loop{cspec_gp_delta = NewCSpecDelta}),
    terminate;

%%-----------------------------
%% FF last app 
%% 
%% send the meas data to job group
%% recalculate the AppPid list
%%-----------------------------
handle_meas_data({meas_data, From, {RGP, RGPEnd, MeasVals, FF}, CSpec},
		 #loop{job_id   = JobId, 
		       meas_tab = MeasTab,
		       app_pids = [From] = AppPids
		      } = Loop) ->
    ?LOG_RAM([{?SEV_1, 
	       {"Measurement data: FF and last app~n"
		"  AppPids = ~p~n"
		"  AppJob  = ~p~n",
		[AppPids, From]}},
	      {?SEV_5,
	       ?LFUN({"Measurement data: FF and last app~n"
		      "  AppPids = ~p~n"
		      "  AppJob  = ~p~n"
		      "  Data    = ~p~n"
		      "  CSpec   = ~p~n", 
		      [AppPids, From, 
		       {RGP, RGPEnd, log_trunc_mv(MeasVals), FF}, 
		       CSpec]})}]),
    NAppPids = get_app_jobs(MeasTab),
    NewCSpecDelta = acc_meas_vals(RGPEnd, MeasVals, FF, CSpec, Loop),
    send_reply(From, {ok, self()}),
    send_meas_info(JobId, 
		   RGP, 
		   RGPEnd, 
		   Loop#loop{cspec_gp_delta = NewCSpecDelta}),
    Loop#loop{cspec_gp_delta = [],
	      gp_end    = RGPEnd,
	      app_pids  = NAppPids};

%%-----------------------------
%% FF
%%  
%% not last app, or unknown app
%%-----------------------------
handle_meas_data({meas_data, From, {RGP, RGPEnd, MeasVals, FF}, CSpec},
		 #loop{app_pids = AppPids} = Loop) ->
    
    case lists:member(From, AppPids) of
	%%-----------------------------
	%% expected app, but not last
	%%-----------------------------
	true ->
	    ?LOG_RAM([{?SEV_1, 
		       {"Measurement data: FF set.~n"
			"  AppPids = ~p~n"
			"  AppJob  = ~p~n",
			[AppPids, From]}},
		      {?SEV_5,
		       ?LFUN({"Measurement data: FF set.~n"
			      "  AppPids = ~p~n"
			      "  AppJob  = ~p~n"
			      "  Data    = ~p~n"
			      "  CSpec   = ~p~n", 
			      [AppPids, From, 
			       {RGP, RGPEnd, log_trunc_mv(MeasVals), FF}, 
			       CSpec]})}
		     ]),
	    NewCSpecDelta = acc_meas_vals(RGPEnd, MeasVals, FF, CSpec, Loop),
	    send_reply(From, {ok, self()}),
	    Loop#loop{cspec_gp_delta = NewCSpecDelta,
		      app_pids  = lists:delete(From, AppPids)};
	%%-----------------------------
	%% not expected app,ignore
	%%-----------------------------
	_ ->
	    ?LOG_RAM([{?SEV_1, 
		       {"Measurement data: FF set. "
			"From not expected app.~n"
			"  AppPids = ~p~n"
			"  AppJob  = ~p~n",
			[AppPids, From]}},
		      {?SEV_5,
		       ?LFUN({"Measurement data: FF set. "
			      "From not expected app.~n"
			      "  AppPids = ~p~n"
			      "  AppJob  = ~p~n"
			      "  Data    = ~p~n"
			      "  CSpec   = ~p~n", 
			      [AppPids, From, 
			       {RGP, RGPEnd, log_trunc_mv(MeasVals), FF}, 
			       CSpec]})}
		     ]),
	    send_reply(From, {ok, self()}),
	    Loop
    end.



%%========================================================================
%% acc_meas_vals(RGPEnd, MeasVals, CSpecDelta, Loop) -> NewCSpecDelta.
%% 
%% Send received measurement data to slave processes for accumulation.
%% Update CSpecDelta with updates during GP from pmsAppJob.
%%========================================================================
acc_meas_vals(RGPEnd, MeasVals, FF, CSpecDelta, Loop) ->
    MITab = Loop#loop.mi_slave_tab,
    Slaves = acc_to_mi_slaves(Loop#loop.gp, RGPEnd, MeasVals, FF, MITab),
    wait_slaves_ready(Slaves, MITab),
    case {CSpecDelta, Loop#loop.cspec_gp_delta} of
	{undefined, []} ->    
	    {[], []};
	{_, []} ->    
	    CSpecDelta;
	{undefined, AccCSpecDelta} ->    
	    AccCSpecDelta;
	{_, AccCSpecDelta} ->
	    acc_cspec_delta(CSpecDelta, AccCSpecDelta)
    end.


acc_to_mi_slaves(GP, RGPEnd, MeasVals, FF, MITab) ->
    Sleep = get_acc_delay(FF, MeasVals, GP, RGPEnd),
    AccFun = fun({Group, GMeasVals}, GPids) ->
		     timer:sleep(Sleep),
		     case ets:lookup(MITab, Group) of
			 [{_, Pid}] ->     
			     Pid ! {meas_values, self(), RGPEnd, GMeasVals},
			     [{Pid, Group} | GPids];
			 _ ->
			     GPids
		     end
	     end,
    lists:foldl(AccFun, [], MeasVals).
		     

get_acc_delay(FF, MeasVals, GP, RGPEnd) when FF -> 
    NGroups = length(MeasVals),
    Sleep = get_meas_info_delay(NGroups, GP, RGPEnd, ?MAX_DELAY_SLICE div 4), 
    ?LOG_RAM(?SEV_1, 
	     {"Sleep time between sending measVals to slaves = ~p ms~n"
	      "Number of PmGroups = ~p~n", 
	      [Sleep, NGroups]}),
    Sleep;

get_acc_delay(_FF, _MeasVals, _GP, _RGPEnd) -> 
    0.


wait_slaves_ready([{SPid, Group} | SPids] = Slaves, MITab) ->
    receive
	{SPid, ok} ->
	    wait_slaves_ready(SPids, MITab);
	{'DOWN', _Ref, process, SPid, Reason} ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {"~s measInfo slave ~p terminated unexpectedly: ~p~n", 
		      [Group, SPid, Reason]}),
	    ets:delete(MITab, SPid),
	    flush_slave_result(SPid),
	    wait_slaves_ready(SPids, MITab)
    after 60000 ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {"Failed to receive result from measInfo slave "
		      "after 60 seconds: ~p~n", [Slaves]}),
	    stop_meas_info_slave(SPid)
    end;

wait_slaves_ready([], _MITab) ->
    ok.


flush_slave_result(SPid) ->
    receive
	{SPid, ok} ->
	    ok
    after 0 ->
	ok
    end.

%%========================================================================
%% acc_cspec_delta(CSpecDelta, AccCSpecDelta) -> NewCSpecDelta.
%% 
%% Accumulate received modifications of measured.
%%========================================================================
acc_cspec_delta({Added, Mod} = Delta, {AccAdded, AccMod} = AccDelta) 
  when Delta =/= AccDelta, 
       Delta =/= {[], []} ->
    F = fun({Group, MTs}, Acc) ->
		case proplists:get_value(Group, Acc, []) of
		    MTs ->
			Acc;
		    AccMTs ->
			NewMTs = lists:umerge(MTs, AccMTs),
			lists:keystore(Group, 1, Acc, {Group, NewMTs})
		end
	end,
    NewAdded = lists:keysort(1, lists:foldl(F, AccAdded, Added)),
    NewMod = AccMod ++ Mod,
    {NewAdded, NewMod};

acc_cspec_delta(_Delta, {_AccAdded, _AccMod} = AccDelta) ->
    AccDelta.

%%========================================================================
%% check_event(Event, Loop) -> Loop
%% 
%%========================================================================
check_event({write, 
	     pmsAppRegistry, 
	     #pmsAppRegistry{pm_group = PmGroup,
			     job_pid  = JobPid,
			     pmi_cb   = PmiCb}, 
	     _Old, 
	     _Transaction},
	    #loop{job_id    = JobId, 
		  req_state = ?JobState_ACTIVE,
		  meas_tab  = MeasTab,
		  gp        = GP
		 } = Loop) ->
    JId = element(4, JobId),
    SubscFlag = get_cb(?SUBSCRIBE_CB, PmiCb), 
    case group_mts_from_tab(PmGroup, MeasTab) of
	[_|_] when SubscFlag =:= true ->
	    update_current_state_and_def_vals(pmsDb:pm_job_get(JobId),
					      ?JobState_ACTIVE),
	    pmsAppJob:subscribe(JobPid, JId, GP, [], MeasTab),
	    NAppJobs = length(get_app_jobs(MeasTab)),
	    Loop#loop{job_state = ?JobState_ACTIVE, 
		      n_exp_meas_data = NAppJobs};
	_ ->
	    Loop
    end;
check_event({write, pmsAppRegistry, _PmJob, _Old, _Transaction},
	    #loop{req_state = ?JobState_STOPPED} = Loop) ->
    Loop;
check_event({delete,
	     pmsAppRegistry,
	     #pmsAppRegistry{pm_group = PmGroup,
			     pmi_cb   = PmiCb},
             _Old,
	     _Transaction},
	    #loop{job_id   = JobId,
		  meas_tab = MeasTab,
		  app_pids = AppPids,
		  gp       = GP,
		  gp_end   = GPEnd} = Loop) ->
    case get_cb(?SUBSCRIBE_CB, PmiCb) of
	true ->
	    NewAppPids = get_app_jobs(MeasTab),
	    NAppJobs = length(NewAppPids),
	    NewCSpecDelta = 
		send_meas_info_if_last_app(NAppJobs, JobId, GP, GPEnd, Loop),
	    NextState = get_next_state(NAppJobs),
	    update_current_state_and_def_vals(pmsDb:pm_job_get(JobId),
					      NextState),
	    log_msg(delete_app_reg, PmGroup),
	    Loop#loop{job_state       = NextState, 
		      app_pids        = AppPids -- (AppPids -- NewAppPids),
		      cspec_gp_delta  = NewCSpecDelta,
		      n_exp_meas_data = NAppJobs};
	false ->
	    Loop
    end;
check_event(Event, #loop{gp        = GP,
			 job_state = State} = Loop) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"UNKNOWN MNESIA EVENT ~p ~p~n", [State, Event]},
	     pmsLib:get_report_offset(GP)),
    Loop.


get_next_state(NAppJobs) when NAppJobs > 0 ->
    ?JobState_ACTIVE;
get_next_state(_) ->
    ?JobState_STOPPED.
    

get_updated_n_expected(#loop{meas_tab = Tab, n_exp_meas_data = N}) 
  when N =:= 0; N =:= undefined ->
    length(get_app_jobs(Tab));

get_updated_n_expected(#loop{n_exp_meas_data = N}) -> 
    N.
    
%%========================================================================
%% send_meas_info_if_last_app() -> ok.
%% 
%%========================================================================
send_meas_info_if_last_app(NAppJobs, JobId, GP, GPEnd, Loop) 
  when NAppJobs =:= 0, Loop#loop.cspec_gp_delta =/= [] ->
    NextGPEnd = GPEnd + pmsLib:get_interval(GP),
    send_meas_info(JobId, GP, NextGPEnd, Loop),
    [];

send_meas_info_if_last_app(_NApps, _JobId, _GP, _GPEnd, Loop) ->
    Loop#loop.cspec_gp_delta.

%%========================================================================
%% send_meas_info() -> ok.
%% 
%%========================================================================
send_meas_info(JobId, GP, GPEnd, Loop) ->
    case catch build_meas_info(JobId, GPEnd, Loop) of
	{ok, {GPEnd, MI}} ->
	    {JobGrpPid, _} = Loop#loop.job_group,
	    pmsJobGroup:meas_info(JobGrpPid, self(), GP, GPEnd, MI);
	Error ->
	    ?LOG_RAM(?SEV_ERROR, {"measInfo: ~p~n", [Error]})
    end,
    MITab = Loop#loop.mi_slave_tab,
    TabSize = ets:info(MITab, size),
    if
	Loop#loop.n_pm_groups =:= TabSize ->
	    ok;
	true ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"Mismatch between MeasInfo slaves and number of PmGroups:"
		      " ~p vs ~p~nRestart MeasInfo slaves", 
		      [Loop#loop.n_pm_groups, TabSize]}),
	    JobId = Loop#loop.job_id,
	    RP = Loop#loop.rp,
	    MeasTab = Loop#loop.meas_tab,
	    PmGroups = groups_from_tab(MeasTab),
	    restart_meas_info_slaves(PmGroups, MITab, JobId, RP, GP, MeasTab)
    end.


%%========================================================================
%% build_meas_info() -> ok.
%% 
%%========================================================================
build_meas_info(JobId, GPEnd, Loop) ->
    CSpecDelta = Loop#loop.cspec_gp_delta,
    ?LOG_RAM(?SEV_5, 
	     {"Building measInfo~n"
	      "  MeasuredDelta = ~p~n", 
	      [CSpecDelta]}),
    MeasTab = Loop#loop.meas_tab,
    MITab = Loop#loop.mi_slave_tab,
    JobState = Loop#loop.job_state,
    MEData = comsaI:get_managed_element_data(),
    NeMEId = proplists:get_value(networkManagedElementId, MEData),
    revert_meas_tab(JobState, CSpecDelta, MeasTab),
    GP = Loop#loop.gp,
    TS1 = os:timestamp(),
    Slaves = order_create_meas_info(NeMEId, MITab, GP, GPEnd),
    XML = get_meas_info_xml(Slaves, MITab),
    restore_meas_tab(JobState, CSpecDelta, MeasTab, JobId),
    TS2 = os:timestamp(),
    ?LOG_RAM(?SEV_1, {"Execution time build_meas_info_data: ~p ms~n", 
		      [timer:now_diff(TS2, TS1)/1000]}),
    {ok, {GPEnd, iolist_to_binary(XML)}}.


order_create_meas_info(NeMEId, MITab, GP, GPEnd) ->
    N = ets:info(MITab, size),
    Sleep = get_meas_info_delay(N, GP, GPEnd, ?MAX_DELAY_SLICE),
    ?LOG_RAM(?SEV_1, 
	     {"Sleep time between ordering slaves to create measInfo = ~p ms~n"
	      "Number of slaves = ~p~n", 
	      [Sleep, N]}),
    F = fun({_, Pid} = GPid, GPids) ->
		timer:sleep(Sleep),
		pmsJobMeasInfo:create_meas_info(Pid, NeMEId),
		[GPid | GPids]
	end,
    lists:keysort(1, ets:foldl(F, [], MITab)).


get_meas_info_delay(N, _GP, _GPEnd, _Max) 
  when N =< 1 ->
    0;

get_meas_info_delay(N, GP, GPEnd, Max) ->
    Delay = pmsLib:get_curr_delay_time(GP, GPEnd, ?DELAY_TIME, ?MIN_REM_TIME), 
    pmsLib:get_delay_slice(N, Delay, ?MIN_DELAY_SLICE, Max).
    

get_meas_info_xml(Slaves, MITab) ->
    [get_meas_info_from_slave(Pid, MITab) || {_, Pid} <- Slaves].


get_meas_info_from_slave(Pid, MITab) ->
    case pmsJobMeasInfo:get_meas_info(Pid) of
	{ok, Data} -> 
	    Data;
	{error, _Reason} ->
	    ets:delete(MITab, Pid),
	    []
    end.

%%========================================================================
%% measured_to_tab(Measured, Tab) -> ok.
%% 
%% This function stores all active measurementTypes for this job in an ets 
%% table.
%%========================================================================
measured_to_tab(Measured, Measured, _Tab) ->
    ok;

measured_to_tab(NewMeasured, _Measured, Tab) ->
    measured_to_tab(NewMeasured, Tab).


measured_to_tab([], _Tab) ->
    %% Do not empty the table when job is stopped.
    %% This is to prevent measData from being invalidly discarded. 
    ok;

measured_to_tab(Measured, Tab) ->
    ets:delete_all_objects(Tab),
    lists:foreach(fun({G, MTs}) ->
			  lists:foldl(fun(MTId, N) ->
					      Key = {G, MTId},
					      ets:insert(Tab, {Key, N}),
					      N + 1
				      end, 1, MTs)
		  end, Measured).


measured_from_tab(Tab) ->
    F = fun({{G, MTId}, _N}, [{G, MTs} | T]) ->
    		[{G, [MTId | MTs]} | T];
    	   ({{G, MTId}, _N}, Acc) ->
    		[{G, [MTId]} | Acc];
    	   (_, Acc) ->
    		Acc
    	end,
    ets:foldr(F, [], Tab).


revert_meas_tab(JobState, {Added, Mod}, Tab) 
  when JobState =/= ?JobState_STOPPED andalso 
       (Added =/= [] orelse Mod =/= []) ->
    RmFun = fun({G, MTs}) ->
		    [ets:insert(Tab, {{G, MT}, 0}) || MT <- MTs],
		    MTs =:= [] orelse update_gmt_seq(G, Tab)
	    end,
    AddFun = fun({G, MTs}) ->
		     [ets:delete(Tab, {G, MT}) || MT <- MTs],
		     MTs =:= [] orelse update_gmt_seq(G, Tab)
	     end,
    ModFun = fun({add, Spec}) ->
		     lists:foreach(AddFun, Spec);
		({rm, Spec}) ->
		     lists:foreach(RmFun, Spec)
	     end,
    lists:foreach(ModFun, Mod),
    lists:foreach(AddFun, Added);
    
revert_meas_tab(_MeasDelta, _Tab, _JobId) ->
    ok.    
		     

restore_meas_tab(JobState, {Added, Mod}, Tab, JobId) 
  when JobState =/= ?JobState_STOPPED andalso 
       (Added =/= [] orelse Mod =/= []) ->
    Measured = get_measurements(JobId),
    measured_to_tab(Measured, Tab);

restore_meas_tab(_JobState, _Delta, _Tab, _JobId) ->
    ok.


update_gmt_seq(Group, Tab) ->
    NewMTs = group_mts_from_tab(Group, Tab),
    lists:foldl(fun({_, MT}, N) ->
			ets:insert(Tab, {{Group, MT}, N}),
			N + 1
		end, 1, NewMTs).


groups_from_tab(Tab) ->
    lists:usort(ets:select(Tab, [{{{'$1', '$2'}, '$3'}, [], ['$1']}])).


group_mts_from_tab(Group, Tab) ->
    ets:select(Tab, [{{{'$1', '$2'}, '$3'}, [{'==', '$1', Group}], 
		      [{{'$3','$2'}}]}]).


groups_from_measured(Measured) ->
    [Group || {Group, _MTs} <- Measured].


%%========================================================================
%% gp_sync_job_update(NewPmJob, OldPmJob, Loop) -> Loop.
%% 
%% This function is called when the PM Job is updated.
%% 
%% NOTE: the case when the GP is changed (both new and old state = active)
%%       is not a valid case in this release because pmsJobGroup will 
%%       first stoppe the old pmsJob and then start a new pmsJob with
%%       the updated GP.
%%========================================================================
%%---------------------------------------------------------------
%% stopped -> active
%%
%% start wall clock syncronizing
%%---------------------------------------------------------------
gp_sync_job_update(#pmJob{pmJobId           = JobId,
			  requestedJobState = ?JobState_ACTIVE,
			  granularityPeriod = GP,
			  reportingPeriod   = RP}, 
		   #pmJob{pmJobId           = JobId,
			  currentJobState   = ?JobState_STOPPED},
		   Loop) ->
    PrintState = 'RcsPm':'RcsPm.JobState'(?JobState_ACTIVE),
    ?LOG_RAM(?SEV_1, 
	     {"~p currentJobState changed to ~p~n", [JobId, PrintState]}),
    Loop#loop{gp = GP,
	      rp = RP,
	      job_state = ?JobState_ACTIVE};

%%---------------------------------------------------------------
%% active -> stopped
%%
%% stop meas info tick
%%---------------------------------------------------------------
gp_sync_job_update(#pmJob{pmJobId           = JobId,
			  requestedJobState = ?JobState_STOPPED} = Job, 
		   #pmJob{pmJobId         = JobId,
			  currentJobState = ?JobState_ACTIVE},
		   #loop{job_id = JobId} = Loop) ->
    NextJobState = ?JobState_STOPPED,
    pmsDb:pm_job_set(Job#pmJob{currentJobState = NextJobState}),
    PrintState = 'RcsPm':'RcsPm.JobState'(NextJobState),
    ?LOG_RAM(?SEV_1, 
	     {"~p currentJobState changed to ~p~n", [JobId, PrintState]}),
    Loop#loop{job_state = NextJobState};

%%---------------------------------------------------------------
%% Other cases
%%---------------------------------------------------------------
gp_sync_job_update(#pmJob{reportingPeriod = RP},
		   _,
		   Loop) ->
    Loop#loop{rp = RP}.

%%========================================================================
%% get_measurements(JobId) -> Measured.
%% 
%%========================================================================
get_measurements(JobId) ->
    {ok, MRs} = pmsDb:measurement_reader_match(JobId),
    GrMTRefs = [gm_get_mt_ref(MS) || 
		   #measurementReader{measurementSpecification = MS} <- MRs],
    {Groups, MTRefs} = lists:unzip(GrMTRefs),
    {ok, MatchMTs}   = pmsDb:measurement_types_match(MTRefs),
    {_Ref, MTs}      = lists:unzip(MatchMTs),
    GMTs = pmsLib:key_collect(lists:zip(Groups, MTs)),
    get_measured(GMTs).


get_measured(GMTs) ->
    [{to_binary(Group), lists:usort(lists:flatmap(fun get_mt_data/1, MT))} 
     || {Group, MT} <- GMTs].


gm_get_mt_ref(#'MeasurementSpecification'{groupRef = GR}) 
  when is_binary(GR) ->
    [[_, ME], [_, SF], [_, PM], [_, Grp]] = unpack_dn(GR),
    {Grp, {ME, SF, PM, Grp, '_'}};

gm_get_mt_ref(#'MeasurementSpecification'{measurementTypeRef = MTR}) ->
    [[_, ME], [_, SF], [_, PM], [_, Grp], [_, MT]] = unpack_dn(MTR),
    {Grp, {ME, SF, PM, Grp, MT}}.


unpack_dn(Dn) ->
    DnL    = binary_to_list(Dn),
    Tokens = string:tokens(DnL, ","),
    [string:tokens(T, "=") || T <- Tokens].

%%========================================================================
%% send_subscribe(JobId, RequestedState, NewMeasure, OldMeasured, GP) -> 
%%     {Measured, JobState}
%% 
%% send subscribe to pmsAppJob if measurement was updated.
%% Remove all measurements if the job is stopped.
%%========================================================================
send_subscribe(JobId, JobState, Measured, OldMeasured, _MeasTab, GP) 
  when JobState =:= ?JobState_STOPPED; Measured =:= [] ->
    Pids = ss(OldMeasured),
    ?LOG_RAM([{?SEV_1, 
	       {"Sending unsubscribe~n"
		"  AppPids = ~p~n"
		"  AppJob  = ~p~n",
		[Pids, JobId]}}]),
    [pmsAppJob:unsubscribe(JobPid, GP) || JobPid <- Pids],
    ?JobState_STOPPED;

send_subscribe(JobId, _, NewMeasured, Measured, MeasTab, GP) ->
    Pids = ss(NewMeasured ++ Measured),
    MeasDiff = get_meas_diff(NewMeasured, Measured),
    ss_send(Pids, JobId, NewMeasured, MeasDiff, MeasTab, GP).


ss_send([], _, _Measured, _, _, _) ->
    ?JobState_STOPPED;

ss_send(Pids, JobId, _Measured, MeasDiff, MeasTab, GP) ->
    JId = element(4, JobId),
    update_current_state_and_def_vals(pmsDb:pm_job_get(JobId),
				      ?JobState_ACTIVE),
    ?LOG_RAM([{?SEV_1, 
	       {"Sending subscribe~n"
	      "  AppPids = ~p~n"
	      "  AppJob  = ~p~n",
		 [Pids, JobId]}},
	      {?SEV_5, 
	       {"Sending subscribe~n"
		"  AppPids     = ~p~n"
		"  AppJob      = ~p~n"
		"  GP          = ~p~n"
		"  CSpecMods   = ~p~n", 
		[Pids, JobId, GP, MeasDiff]}}]),
    [pmsAppJob:subscribe(JobPid, JId, GP, MeasDiff, MeasTab) || JobPid <- Pids],
    ?JobState_ACTIVE.


get_app_jobs(MeasTab) ->
    lists:usort(ss_get_pids(groups_from_tab(MeasTab))).


ss(Measured) ->
    PmGroups = [PmGroup || {PmGroup, [_|_]} <- lists:ukeysort(1, Measured)],
    lists:usort(ss_get_pids(PmGroups)).


ss_get_pids(PmGroups) ->
    %% Find all records with any of the requested PmGroups
    case pmsDb:app_reg_get(PmGroups) of
	{ok, []} -> 
	    [];
	{ok, Recs} ->
	    [JobPid || #pmsAppRegistry{job_pid = JobPid} = Rec <- Recs, 
		       get_cb(?SUBSCRIBE_CB, Rec#pmsAppRegistry.pmi_cb)]
    end.


%%===========================================================================
%% Misc functions
%%===========================================================================
%%===========================================================================
%% get_meas_diff(Measured, Measured)
%% 
%% Get added and removed MTs
%% 
%%===========================================================================
get_meas_diff(Measured, Measured) ->
    [];

get_meas_diff(_NewMeasured, []) ->
    [];

get_meas_diff(NewCSpec, OldCSpec) ->
    Added = get_meas_added(NewCSpec, OldCSpec), 
    case get_meas_removed(NewCSpec, OldCSpec) of
	[] when Added =:= [] ->
	    [];
	[] ->
	    [{add, Added}];
	Removed when Added =:= [] ->
	    [{rm, Removed}];
	Removed ->
	    [{rm, Removed}, {add, Added}]
    end.


get_meas_added([], _Prev) ->
    [];
get_meas_added(Curr, []) ->
    Curr;
get_meas_added(Same, Same) ->
    [];
get_meas_added(Curr, Prev) ->
    diff_cspecs(Prev, Curr).
    

get_meas_removed(_Curr, []) ->
    [];
get_meas_removed([], Prev) ->
    Prev;
get_meas_removed(Same, Same) ->
    [];
get_meas_removed(Curr, Prev) ->
    diff_cspecs(Curr, Prev).


diff_cspecs(CSpec1, CSpec2) ->
    [{G, DMT} || {G, DMT} <- [{G, MT -- proplists:get_value(G, CSpec1, [])} || 
				 {G, MT} <- CSpec2 -- CSpec1], DMT =/= []].
    
%%===========================================================================
%% update_current_state_and_def_vals(Job, NewState) ->
%% 
%% No need to update if same state.
%% 
%%===========================================================================
update_current_state_and_def_vals({ok, [#pmJob{currentJobState = NewState}]},
				  NewState) ->
    ok;
update_current_state_and_def_vals({ok, [#pmJob{jobControl = JC,
					       pmJobId    = JobId} = Job]},
				  NewState) ->
    PrintState = 'RcsPm':'RcsPm.JobState'(NewState),
    ?LOG_RAM(?SEV_1, 
	     {"~p currentJobState changed to ~p (~p)~n", 
	      [JobId, NewState, PrintState]}),
    JobCtrl = pmsLib:choose(JC == undefined, ?JobControl_FULL, JC),
    pmsDb:pm_job_set(Job#pmJob{currentJobState = NewState,
			       jobControl      = JobCtrl}),
    pmsServer:check_alarm();
update_current_state_and_def_vals(_, _) ->
    ok.


clean_up_and_exit(Reason, #loop{meas_tab = MeasTab,
				mi_slave_tab = MITab,
				job_id = JobId}) ->
    stop_meas_info_slaves(MITab),
    ets:delete(MITab),
    ets:delete(MeasTab),
    check_alarm_on_exit(pmsDb:pm_job_get(JobId)),
    exit(Reason).
    

check_alarm_on_exit({ok, [Job]}) ->
    pmsDb:pm_job_set(Job#pmJob{currentJobState = ?JobState_STOPPED}),
    pmsServer:check_alarm();
check_alarm_on_exit({ok, []}) ->
    pmsServer:check_alarm().


log_msg(_, _) -> ok.


log_loop(_)   -> ok.


get_mt_data(Recs) ->
    [to_binary(Id) || 
	#measurementType{measurementTypeId = {_, _, _, _, Id},
			 measurementStatus = MeasStatus} 
	    <- Recs, MeasStatus =/= ?MeasurementStatus_OBSOLETE].


format_loop(#loop{mi_slave_tab = MITab} = Loop) ->
    F       = record_info(fields, loop),
    [_ | L] = tuple_to_list(Loop), 
    List    = lists:zip(F,L),
    [{mi_slave_tab, ets:tab2list(MITab)} | proplists:delete(mi_slave_tab, List)].
    

get_cb(Cb, {_Mod, Cbs}) -> 
    proplists:get_value(Cb, Cbs, false);

get_cb(_Cb, Mod) when is_atom(Mod) ->
    true.


req_state(ReqState) when is_integer(ReqState) ->
    ReqState;

req_state(_) ->
    ?JobState_ACTIVE.


%% to_binaries(Terms) ->
%%     [to_binary(Term) || Term <- Terms].


to_binary(Term) ->
    pmsLib:to_binary(Term).

%%===========================================================================
%% log truncate functions
%%===========================================================================
log_trunc_grp(Grps) ->
    ltg(Grps, 0, pmsDb:pms_env_get(log_max), []).

ltg([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltg(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltg([], _, _, Acc) ->
    lists:reverse(Acc);
ltg([{Grp, MT} | T], N, Max, Acc) when is_binary(Grp) ->
    Res = {Grp, ltg(MT, 0, Max, [])}, 
    ltg(T, N + 1, Max, [Res | Acc]);
ltg([MtId | T], N, Max, Acc) when is_binary(MtId) ->
    ltg(T, N + 1, Max, [MtId | Acc]).
%% ltg([{_SeqNo, MtId} | T], N, Max, Acc) when is_binary(MtId) ->
%%     ltg(T, N + 1, Max, [MtId | Acc]).

	   
log_trunc_mv(Bundles) ->
    ltb_grp(Bundles, 0, pmsDb:pms_env_get(log_max), []).

ltb_grp([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltb_grp(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltb_grp([], _, _, Acc) ->
    lists:reverse(Acc);
ltb_grp([{Grp, Ldns} | T], N, Max, Acc) ->
    Res = ltb_ldn(Ldns, 0, Max, []),
    ltb_grp(T, N + 1, Max, [{Grp, Res} | Acc]).

ltb_ldn([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltb_ldn(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltb_ldn([], _, _, Acc) ->
    lists:reverse(Acc);
ltb_ldn([{Ldn, Vals} | T], N, Max, Acc) ->
    ltb_ldn(T, N + 1, Max, [{Ldn, pmsLib:log_trunc_list(Vals)} | Acc]).

%%========================================================================
%% pp(Rec) -> ok.
%% 
%%========================================================================
pp(Rec) when is_record(Rec, pmJob) ->
    [_ | Vals] = tuple_to_list(Rec),
    F = record_info(fields, pmJob),
    L = pp_max_length(F, 0),
    lists:flatten([io_lib:format("    ~-*s = ~p~n", [L, K, V]) || 
	{K, V} <- lists:zip(F, Vals)]).


pp_max_length([], L) ->
    L;
pp_max_length([H|T], L) ->
    case length(atom_to_list(H)) of
	GR when GR > L -> pp_max_length(T, GR);
	_              -> pp_max_length(T, L)
    end.

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

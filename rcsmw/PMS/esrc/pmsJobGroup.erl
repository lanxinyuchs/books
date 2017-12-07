%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsJobGroup.erl %
%%% @private 	
%%% 	
%%% @doc
%%% Description:
%%% 
%%% pmsJobGroup is a process that creates ROP-files.
%%% 
%%% It controls all PM Jobs with the same group tag in PM Job object.
%%% There is a default pmsJobGroup, aka common jobGroup, for those 
%%% PM Jobs which have jobGroup not set, i.e. jobGroup == undefined..
%%% 
%%% pmsJobGroup will create a process for each PM Job it controls.
%%% 
%%% The ME->NodeSupport->PmSupport MO has an attribute, ropFileHandling,
%%% which can be set to single or multi rop file.
%%%
%%% In case of single only one ROP file should be generated;
%%% all non common jobGroup processes should send their measurement data 
%%% to the common job.
%%% 
%%% In case of multi each jobGroup process will generate their own ROP file.
%%% Note: when the value is updated it should not affect the current RP,
%%% but take effect on the next RP.
%%% 
%%% At a certain time after a RP pmsJobGroup assumes that all 
%%% measurement data has been received and the process starts
%%% to generate the ROP file.
%%% 
%%% Note: The PM Jobs controlled by a pmsJobGroup may
%%% have different GP but they must have the same RP.
%%% 
%%% @end 
%%% ----------------------------------------------------------
-module(pmsJobGroup).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R11A/R12A/2').
-date('2017-11-06').
-author('eolaand').
-shaid('a2bfbe43458cccf1a6b269e42a473d97228a796a').
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      2013-01-23 uabesvi     Created
%%% R5A/1      2015-10-08 etxjotj     Preliminary OTP R18 adaption
%%% R5A/12     2015-12-22 uabesvi     PmSupport
%%% R5A/17     2016-01-11 uabesvi     single/multi rop file handling
%%% R5A/37     2016-03-29 uabesvi     rop file name, unique id
%%% R6A/4      2016-08-31 eolaand     Keep track of PmJob state and PmGroup
%%%                                   state in order to speed up ROP file
%%%                                   generation
%%% R6A/7      2016-09-19 eolaand     Fix TR HV26514, make sure pm_groups
%%%                                   is updated correctly
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% pmsJobGroup functions called from pmsServer
-export([create/2]).
-export([terminate/2]).

%% pmsJob update functions called from pmsServer
-export([job_create/3]).
-export([job_update/3]).
-export([job_delete/3]).
-export([job_takeover/4]).

%% Called from other pmsJobGroup processes
-export([pm_group_update/5]).
-export([pm_group_meas_info/5]).
-export([pm_group_exit/4]).

%% Called from pmsJob processes
-export([meas_info/5]).
-export([over_taken/5]).
-export([job_op_state/4]).

%% Test functions, called from pmsDebug
-export([get_loop_data/2]).
-export([get_proc_info/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/3]).



-include("RcsPm.hrl").
-include("RmePmSupport.hrl").

-include("pms.hrl").


-define(SEND_TO,   5000).
-define(CREATE_TO, 5000).

-define(ROP_TICK,  rop_tick).
-define(GP_SYNCED, gp_synced_job_grp).
-define(ET, pmsLib:epoch_time()).

-define(ROP_TYPE_A, "A").
-define(ROP_TYPE_C, "C").
-define(VSN, "32.435 V10.0").
-define(VENDOR, "Ericsson AB").

%%========================================================================
%% #loop
%% 
%% group_id : string() | undefined
%%   name of the job group. The common job group has group_id = undefined
%% 
%% rp : integer() 
%%   reporting period
%% 
%% pm_jobs : [{JobId, {JobPid, MRef, JobGP, CurrJobState}}]  
%%   references to the PmJobs that reports meas data to this process
%% 
%% pm_groups : [{JobGroupId, {JobGroupPid, MRef}}]
%%   references to the PmJobGroups that reports meas data to this process
%% 
%% pm_groups_add : [{JobGroupId, {JobGroupPid, MRef, Time}}]
%%   references to the PmJobGroups that are to start reporting meas data
%%   at Time =< CurrentTime
%% 
%% pm_groups_rem : [{JobGroupId, {JobGroupPid, MRef, Time}}]
%%   references to the PmJobGroups that are to stop reporting meas data
%%   at Time =< CurrentTime
%% 
%% mi_pids : [{pid(), N}]
%%   references to the PmJobs that reports meas data to this process
%% 
%% jg_pids : [pid()]
%%   references to the PmJobGroups that reports meas data to this process
%% 
%% to_pids : [pmJobId]
%%   references to the PmJobs that are to taken over from another 
%%   PmsJobGroup process
%% 
%% timer_ref : integer() | undefined
%%   if set indicates that the RP TICK is running
%% 
%% rop_tab : integer()
%%   reference to an ETS table where the meas data is stored for 
%%   ongoing RP
%% 
%% rop_begin_time : integer()
%%   Start time of the current RP. Set to 0 when the ROP-file is built.
%%   Initiated at each ROP Tick.
%% 
%% mi_2_common : integer() | common | multi
%%   Indicates if the meas info should be sent to the common job group
%%   common    - this is the common job group
%%   multi     - multi ROP files; do not send to common
%%   single    - single ROP file: 
%%               the integer indicates the ROP time stamp 
%%               when to start to send to the common job group
%% 
%% file_method : {Method, Time} 
%%   Method = ?FileHandlingMethod_SINGLE_ROP_FILE |
%%            ?FileHandlingMethod_MULTIPLE_ROP_FILES
%%   Time   = integer()
%%     Indicates the last change of the file handling method
%%     and when it occured 
%% 
%% sw_vsn : string()
%%   The running SW version. Used in the ROP files 
%% 
%% server : {ServerPid, MRef}
%%   Reference to the PMS server process
%% 
%% deleted = boolean()
%%   Indicates if the job group is to be deleted
%% 
%% n_exp_meas_info : integer()
%%   Indicates how many PmJobs should report meas data in this RP
%% 
%% n_exp_job_group : integer()
%%   Indicates how many PmJobGroups should report meas data in this RP
%% 
%%========================================================================

-record(loop, {group_id, 
	       rp,
	       pm_jobs          = [],
	       pm_groups        = [],
	       pm_groups_add    = [],
	       pm_groups_rem    = [],
	       mi_pids          = [],
	       jg_pids          = [],
	       to_jobs          = [],
	       timer_ref,
	       rop_tab,
	       rop_begin_time,
	       mi_2_common,     
	       file_method,
	       rep_file_method,
	       sw_vsn,
	       server,
	       deleted          = false,
	       n_exp_meas_info  = 0,
	       n_exp_job_group  = 0
	      }).


%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%========================================================================
%% create(JobGroupId) -> Pid
%%
%% Spawn a new Job Group process.
%%========================================================================
create(JobGroupId, Job) ->
    Self = self(),
    spawn(fun() -> init(Self, JobGroupId, Job) end),

    receive
	{pm_group_id_res, JobGroupId, Pid} ->
	    {ok, Pid}
    after ?CREATE_TO ->
	    {error, {timeout, {?MODULE, create}}}
    end.


%%========================================================================
%% job_update(Pid, PmJob, OldPmJob) -> ok
%%
%% A PM Job is updated.
%%========================================================================
job_update(Pid, PmJob, OldJob) ->
    send(Pid, job_update, {PmJob, OldJob}).

%%========================================================================
%% job_create(Event) -> ok
%%
%% A new PM Job is created.
%%========================================================================
job_create(Pid, PmJob, Old) ->
    send(Pid, job_create, {PmJob, Old}).

%%========================================================================
%% job_update(Event) -> ok
%%
%% A PM Job is updated.
%%========================================================================
%% job_update(Pid, PmJob, Old) ->
%%     send(Pid, job_update, {PmJob, Old}).

%%========================================================================
%% job_delete(Pid, PmJob, Reason) -> ok
%% Reason = delete | move
%%
%% A PM Job is deleted.
%%========================================================================
job_delete(Pid, PmJob, Reason) ->
    send(Pid, job_delete, {PmJob, Reason}).

%%========================================================================
%% job_takeover(Pid, PmJob, NewJobGrpPid, NewJobGroup) -> ok
%% Reason = delete | move
%%
%% A PM Job is deleted.
%%========================================================================
job_takeover(Pid, PmJob, TakeoverPid, NewJobGroup) ->
    send(Pid, job_takeover, {PmJob, TakeoverPid, NewJobGroup}).

%%========================================================================
%% meas_info(Pid, UserPid, GP, GPEnd, MI) -> ok
%%
%% Measurement data from PmJob
%%========================================================================
meas_info(JobGroupPid, UserPid, GP, GPEnd, MI) ->
    JobGroupPid ! {meas_info, UserPid, GP, GPEnd, MI}.

%%========================================================================
%% over_taken(JobGroupPid, JobId, GP, JobPid)  -> ok
%%
%% Measurement data from PmJob
%%========================================================================
over_taken(JobGroupPid, JobId, GP, OpState, JobPid) ->
    JobGroupPid ! {over_taken, JobId, GP, OpState, JobPid}.

%%========================================================================
%% job_op_state(JobGroupPid, JobId, OpState, JobPid)  -> ok
%%
%% Op state update from PmJob
%%========================================================================
job_op_state(JobGroupPid, JobId, OpState, JobPid) ->
    JobGroupPid ! {job_op_state, JobId, OpState, JobPid}.

%%========================================================================
%% pm_group_update(Pid, UserPid, FileState; StartTime) -> ok
%%
%% Non common jobGroup pmsJobGroup process informs
%% the commen jobGroup process that its rop file handling state
%% is updated
%%========================================================================
pm_group_update(JobGroup, UserPid, JobGrpId, FileState, StartTime) ->
    Pid = whereis(JobGroup),
    pgu(Pid, JobGroup, UserPid, JobGrpId, FileState, StartTime, 10).

%% Send to the common pmsJobGroup
pgu(Pid, _JobGroup, UserPid, JobGrpId, FileState, StartTime, _N) 
  when is_pid(Pid) ->
    Pid ! {pm_group_update, UserPid, JobGrpId, FileState, StartTime};

%% Sometimes the named job group process starts
%% before the common job group process
pgu(_, JobGroup, UserPid, JobGrpId, FileState, StartTime, N) 
  when N > 0 ->
    timer:sleep(100),
    Pid = whereis(JobGroup),
    pgu(Pid, JobGroup, UserPid, JobGrpId, FileState, StartTime, N - 1);

pgu(_, JobGroup, _UserPid, JobGrpId, FileState, _StartTime, _N) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Failed to notify common JobGroup about state change."
	      " JobGroup not up after 10 attempts."
	      "~nJobGroup = ~p"
	      "~nJobGroupId = ~p"
	      "~nFileState = ~p~n", 
	      [JobGroup, JobGrpId, FileState]}),
    sysInitI:warning_msg("Failed to notify common JobGroup about state change."
			 " JobGroup not up after 10 attempts."
			 "~nJobGroup = ~p"
			 "~nJobGroupId = ~p"
			 "~nFileState = ~p~n", 
			 [JobGroup, JobGrpId, FileState]),
    ok.

%%========================================================================
%% pm_group_meas_info(Common, JobGroupId, UserPid, RPEnd, MI) -> ok
%%
%% Non common jobGroup pmsJobGroup process sends meas info to
%% the commen jobGroup process.
%%========================================================================
pm_group_meas_info(Common, JobGroupId, UserPid, RPEnd, MI) ->
    Common ! {pm_group_meas_info, JobGroupId, UserPid, RPEnd, MI}.

%%========================================================================
%% pm_group_exit(Common, JobGroupId, UserPid, Reason) -> ok
%%
%% Non common jobGroup pmsJobGroup process informs
%% the commen jobGroup process that it will exit
%%========================================================================
pm_group_exit(Common, JobGroupId, UserPid, Reason) ->
    case whereis(Common) of
	undefined ->
	    ?LOG_RAM(?SEV_1,
		     {"pm_group_exit. Common JobGroup does not exist ~p~n",
		      [JobGroupId]});
	_ ->
	    Common ! {pm_group_exit, JobGroupId, UserPid, Reason}
    end.


%%========================================================================
%% terminate(Pid, JobGroupId) -> ok
%%
%% Pms Server is terminated. Terminate also this process, but do not
%% delete any table entries.
%%========================================================================
terminate(Pid, JobGroupId) ->
    ?LOG_RAM(?SEV_1,
	     {"PMS JOB GROUP terminated ~p  ~p~n", [Pid, JobGroupId]}),
    send(Pid, terminate, JobGroupId).


%%========================================================================
%% Send the message to the process and wait for reply
%%========================================================================
send(Pid, Msg, Data) ->
    send2(process_info(Pid), Pid, Msg, Data).

%% Trying to delete a job group process that doesn't exist
%% It has propably already died because of last job was deleted.
send2(undefined, _Pid, job_delete, _) ->
    ok;
%% The requested process does not exist
send2(undefined, Pid, _, _) ->
    {error, {no_process, {?MODULE, Pid}}};    
%% Normal case
send2(_, Pid, Msg, Data) ->
    Pid ! {Msg, self(), Data},
    ok.
    

%%========================================================================
%% get_loop_data(Pid) -> ok
%%========================================================================
get_loop_data(JG, RP) ->
    get_name(JG, RP) ! {get_loop_data, self()},
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


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% Init function for the Job process
%%========================================================================
init(UserPid, JobGroupId, {Action, JobId, RP}) ->
    UserPid ! {pm_group_id_res, JobGroupId, self()},
    put(name, {?MODULE, JobGroupId}), %% used by pmsDebug to print process data
    MRef    = erlang:monitor(process, UserPid),
    Tab     = ets:new(rop_tab, [bag]),
    SWData  = swmI:get_current_up_metadata(),
    ProdNo  = proplists:get_value(productNumber, SWData),
    ProdRev = proplists:get_value(productRevision, SWData),
    PmJobs  = choose(Action == create,   [{JobId, undefined}], []),
    ToJobs  = choose(Action == takeover, [JobId], []),
    pmsLib:gp_tick(?GP_SYNCED, RP, pmsLib:get_rop_offset(RP)),
    pmsDb:mnesia_subscribe({table, pmSupport, detailed}),
    register(get_name(JobGroupId, RP), self()),
    ?LOG_RAM(?SEV_1, {"Started.~n  jobGroup = ~p~n", [JobGroupId]}),
    RopBegin         = rop_begin_time(RP),
    {ok, FileMethod} = _FM = pmsDb:pm_support_get(),
    FMToReport = get_fm_to_report(is_job_active(JobId), FileMethod),
    RepFm = update_common(JobGroupId, FMToReport, RP, 0),
    loop(#loop{group_id        = JobGroupId, 
	       rp              = RP,
	       pm_jobs         = PmJobs,
	       to_jobs         = ToJobs,
	       rop_tab         = Tab, 
	       rop_begin_time  = RopBegin,
	       mi_2_common     = i_m2c(JobGroupId, FileMethod), 
	       file_method     = {FileMethod, RopBegin},
	       rep_file_method = RepFm,
	       sw_vsn          = ProdNo ++ " " ++ ProdRev,
	       server          = {UserPid, MRef},
	       timer_ref       = RP}). 


get_fm_to_report(true, FileMethod) ->
    FileMethod;
get_fm_to_report(_False, _FM) ->
    ?FileHandlingMethod_MULTIPLE_ROP_FILES.


is_job_active(JobId) ->
    case pmsDb:pm_job_dirty_get(JobId) of
	%% {ok, [#pmJob{currentJobState = ?JobState_ACTIVE}]} ->
	{ok, [#pmJob{requestedJobState = ?JobState_ACTIVE}]} ->
	    true;
	_ -> 
	    false
    end.


i_m2c(undefined, _) ->
    common;
i_m2c(_, FileFormat) ->
    format_ff(FileFormat).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% loop(#loop{}) 
%%
%% Job loop. Loop until the job is deleted.
%%========================================================================
%% Non common job group process will die if no pmsJobs are attached to it.
%% The common job group should never die 
loop(#loop{group_id        = JobGroupId,
	   rp              = RP,
	   pm_jobs         = [],
	   to_jobs         = [],
	   n_exp_meas_info = 0
	   %%deleted  = false
	  }) 
  when JobGroupId /= undefined ->
    ?LOG_RAM(?SEV_1, 
	     {"Killed jobGroup process due to no jobs~n  jobGroup = ~p~n",
	      [JobGroupId]}),
    pmsJobGroup:pm_group_exit(get_name(?COMMON_JG, RP), 
			      JobGroupId,
			      self(), 
			      no_jobs),
    exit(no_jobs);
loop(#loop{group_id        = JobGroupId,
	   pm_jobs         = PmJobs,
	   pm_groups       = PmGroups,
	   pm_groups_add   = PmGroupsAdd,
	   pm_groups_rem   = PmGroupsRem,
	   mi_2_common     = MiToCommon,
	   file_method     = FileMethod, 
	   rep_file_method = RepFm,
	   rp              = RP,
	   timer_ref       = TimerRef,
	   server          = {SrvPid, SrvRef},
	   rop_tab         = RopTab,
	   deleted         = Deleted
	  } = Loop) ->
    ?LOG_RAM(?SEV_5, {"JOB GROUP LOOP ~p~n"
		      "~p~n~n", 
		      [pmsLib:epoch_time(), format_loop(Loop)]}),
    log_loop(Loop),
    receive
	%%-------------------------------------------------------------
	%% Receive measurement info from pmsJob 
	%%-------------------------------------------------------------
	{meas_info, _, _, _, _} = Msg ->
	    loop(handle_meas_info(Msg, Loop));

	%%-------------------------------------------------------------
	%% Receive measurement data from another pmsJobGroup process
	%% (Should only happen on common job group process).
	%%-------------------------------------------------------------
	{pm_group_meas_info, _JobgrpId, _Pid, _RPEnd, _MI} = Msg ->
	    loop(handle_pm_group_meas_info(Msg, Loop));

	%%-------------------------------------------------------------
	%% PmJob is created
	%%-------------------------------------------------------------
 	{job_create = Msg, UserPid, {#pmJob{pmJobId = JobId} = PmJob, []}} ->
	    ?LOG_RAM(?SEV_1,
		     {"PmJob created.~n"
		      "  jobGroupId = ~p~n"
		      "  jobId      = ~p~n",
		      [JobGroupId, JobId]}),
 	    log_msg(Msg, {UserPid, PmJob, []}),
	    {_Res, NewLoop} = pm_job_new(PmJob, Loop),
 	    NewExp = get_n_expected(NewLoop),
	    %% pmsServer must be informed that the job is created to 
	    %% prevent that the job group was just killed.
	    %% (Because it did not have any jobs). 
	    %% Refer to ?ROP_TICK message.
	    pmsServer:job_create_res(ok, self(), JobId),
	    loop(NewLoop#loop{n_exp_meas_info = NewExp,
			      deleted         = false});

	%%-------------------------------------------------------------
	%% PmJob is updated
	%%-------------------------------------------------------------
	{job_update = Msg, UserPid, {#pmJob{pmJobId = JobId} = PmJob, OldJob}} ->
	    log_msg(Msg, {UserPid, PmJob}),
	    Exists = proplists:get_value(JobId, PmJobs, undefined),
	    loop(pm_job_update(Exists, PmJob, OldJob, Loop));

	%%-------------------------------------------------------------
	%% PmJob is deleted
	%%-------------------------------------------------------------
	{job_delete = Msg,
	 _UserPid,
	 {#pmJob{pmJobId = JobId} = PmJob, Reason}} ->
	    ?LOG_RAM(?SEV_1,
		     {"PmJob delete.~n"
		      "  jobGroupId = ~p~n"
		      "  jobId      = ~p~n"
		      "  Reason     = ~p~n",
		      [JobGroupId, JobId, Reason]}),
 	    log_msg(Msg, {PmJob, Reason}),
	    NewLoop = pm_job_delete_move(PmJob, Loop, Reason),
	    #loop{pm_jobs = _DelPmJobs} = NewLoop,
	    loop(NewLoop);

	%%-------------------------------------------------------------
	%% Takeover PmJob (request from pmsServer)
	%%-------------------------------------------------------------
	{job_takeover = Msg, _UserPid, {PmJob, TakeoverPid, NewJobGroup}} ->
	    ?LOG_RAM(?SEV_1,
		     {"Takeover request from pmsServer.~n"
		      "  JobGroupId   = ~p~n"
		      "  JobId        = ~p~n"
		      "  Takeover Pid = ~p~n"
		      "  NewJobGroup  = ~p~n",
		      [JobGroupId, PmJob, TakeoverPid, NewJobGroup]}),
 	    log_msg(Msg, {PmJob, TakeoverPid}),
	    NewLoop = pm_job_takeover(PmJob, 
				      TakeoverPid, 
				      NewJobGroup,
				      Loop),
	    #loop{pm_jobs = _DelPmJobs} = NewLoop,
	    loop(NewLoop);

	%%-------------------------------------------------------------
	%% Over taken PmJob (sent from pmsJob when it starts
	%% to send meas data to this pmsJobGroup
	%%-------------------------------------------------------------
	{over_taken = Msg, ToPmJob, ToGP, ToOpState, ToJobPid} ->
 	    log_msg(Msg, {ToPmJob, ToGP, ToOpState, ToJobPid}),
	    ?LOG_RAM(?SEV_1, 
		     {"Taken over job.~n"
		      "  jobGroupId = ~p~n"
		      "  jobId      = ~p~n"
		      "  jobOpState = ~p~n"
		      "  jobPid     = ~p~n",
		      [JobGroupId, ToPmJob, ToOpState, ToJobPid]}),
	    loop(over_taken_job(ToPmJob, ToJobPid, ToGP, ToOpState, Loop));

	%%-------------------------------------------------------------
	%% PmJob OpState update (sent from pmsJob when it changes
	%% operational state.
	%%-------------------------------------------------------------
	{job_op_state = Msg, JobId, JobOpState, JobPid} ->
 	    log_msg(Msg, {JobId, JobOpState, JobPid}),
	    ?LOG_RAM(?SEV_1, 
		     {"PmJob op state update.~n"
		      "  jobGroupId = ~p~n"
		      "  jobId      = ~p~n"
		      "  jobPid     = ~p~n"
		      "  jobOpState = ~p~n",
		      [JobGroupId, JobId, JobPid, JobOpState]}),
	    loop(job_op_state_update(JobId, JobOpState, JobPid, Loop));

	%%-------------------------------------------------------------
	%% PmsGroup process state update
	%%-------------------------------------------------------------
	{pm_group_update, UserPid, UserId, FileState, Time} ->
	    ?LOG_RAM(?SEV_1, 
		     {"PmsGroup process state update~n"
		      "  JobGroupPid = ~p~n"
		      "  JobGroupId  = ~p~n"
		      "  FileState   = ~p (~p)~n"
		      "  Time        = ~p~n",
		      [UserPid, 
		       UserId, 
		       FileState,
		       format_ff(FileState),
		       Time]}),
	    IsRunning = proplists:is_defined(UserId, PmGroups),
	    {PGRun, PGAdd, PGRem} = handle_pm_group_update(FileState,
							   Time,
							   rop_begin_time(RP),
							   UserId,
							   UserPid,
							   IsRunning,
							   PmGroups,
							   PmGroupsAdd,
							   PmGroupsRem),
	    loop(Loop#loop{pm_groups     = PGRun,
			   pm_groups_add = PGAdd,
			   pm_groups_rem = PGRem});

	%%-------------------------------------------------------------
	%% Another pms job group process exits
	%%-------------------------------------------------------------
	{pm_group_exit, DownJobGrpId, _Pid, Reason} = Msg ->
	    ?LOG_RAM(?SEV_1,
		     {"JobGroup process exited ~n"
		      "  JobGroup = ~p~n" 
		      "  Reason   = ~p~n", 
		      [DownJobGrpId, Reason]}),
	    loop(handle_pm_group_exit(Msg, Loop));

	%%-------------------------------------------------------------
	%% Initialize the ROP report tick
	%%-------------------------------------------------------------
	{?GP_SYNCED, RP, NextSyncTime} ->
	    ?LOG_RAM(?SEV_1,
		     {"Sync received. JobGroup = ~p~n"
		      "  RP = ~p (~p sec)~n", 
		      [JobGroupId, RP, pmsLib:get_interval(RP)]}),
	    {SecOff, MilOff} = get_offset(RP, os:timestamp(), NextSyncTime),
	    NextTo = pmsLib:gp_tick(?ROP_TICK, RP, SecOff, MilOff),
	    Time   = rop_begin_time(RP),
	    log_msg(gp_synced, 
		    {{RP, erlang:timestamp(), NextSyncTime}, NextTo, Time}),
	    ets:delete_all_objects(RopTab),
	    MiPids = get_mi_pids(PmJobs, RP),
	    NewRepFm = job_state_update_common(MiToCommon,
					       MiPids,
					       JobGroupId, 
					       RP, 
					       Time,
					       RepFm),
	    {Run, Add, Rem} = get_job_groups(Loop),
	    loop(Loop#loop{rop_begin_time  = Time, 
			   mi_pids         = get_mi_pids(PmJobs, RP),
			   jg_pids         = get_jg_pids(PmGroups),
			   pm_groups       = Run,
			   pm_groups_add   = Add,
			   pm_groups_rem   = Rem,
			   rep_file_method = NewRepFm,
			   n_exp_meas_info = get_n_expected(Loop), 
			   n_exp_job_group = length(Run)});

	%%-------------------------------------------------------------
	%% Start to create the ROP file
	%%-------------------------------------------------------------
	{?ROP_TICK, _RP, _NextSyncTime} 
	when Deleted          andalso 
	     PmJobs     == [] andalso  
	     PmGroups   == [] andalso
	     JobGroupId /= undefined ->
	    ?LOG_RAM(?SEV_1,
		     {"Exited due to no jobs no pmGorups and delete = true.~n"
		      "  JobGroupId = ~p~n", [JobGroupId]}),
	    pmsJobGroup:pm_group_exit(get_name(?COMMON_JG, RP), 
				      JobGroupId,
				      self(), 
				      no_jobs),
	    exit(normal);
	{?ROP_TICK, RP, NextSyncTime} 
	when RP == TimerRef andalso not Deleted ->
	    Time = rop_begin_time(RP),
	    ?LOG_RAM(?SEV_1, 
		     {"ROP tick received. (~p)~n"
		      "  Id = ~p~n"
		      "  RP = ~p (~p sec)~n",
		      [Time, JobGroupId, RP, pmsLib:get_interval(RP)]}),
	    NextTo = resync(Loop, RP, NextSyncTime),
	    LogMsg = {{RP, erlang:timestamp(), NextSyncTime}, NextTo, Time}, 
	    build_rop_file(Loop, Time, RP, JobGroupId),
	    log_msg(rop_tick, LogMsg),
	    {Run, Add, Rem} = get_job_groups(Loop),
	    RopBegin        = rop_begin_time(RP),
	    UpdateDelay     = pmsLib:get_interval(RP),
	    MiPids = get_mi_pids(PmJobs, RP),
 	    NewMiToCommon   = get_send_mi_2_common(FileMethod, 
						   RP, 
						   RopBegin,
						   UpdateDelay,
						   JobGroupId,
						   MiToCommon,
						   MiPids),
	    NewRepFm = job_state_update_common(NewMiToCommon,
					       MiPids,
					       JobGroupId, 
					       RP, 
					       Time,
					       RepFm),
	    loop(Loop#loop{rop_begin_time  = Time, 
			   mi_pids         = MiPids,
			   jg_pids         = get_jg_pids(PmGroups),
			   pm_groups       = Run,
			   pm_groups_add   = Add,
			   pm_groups_rem   = Rem,
 			   mi_2_common     = NewMiToCommon,
			   rep_file_method = NewRepFm,
			   n_exp_meas_info = get_n_expected(Loop), 
			   n_exp_job_group = length(Run)});
	{?ROP_TICK, RP, _NextSyncTime} when RP == TimerRef ->
	    Time = rop_begin_time(RP),
	    ?LOG_RAM(?SEV_1,
		     {"ROP tick received, exiting. (~p)~n"
		      "  Id = ~p~n"
		      "  RP = ~p~n", 
		      [Time, JobGroupId, RP]}),
	    build_rop_file(Loop, Time, RP, JobGroupId),
	    log_msg(last_rop_tick, {RP, erlang:timestamp(), Time}),
	    pmsServer:job_group_exit(JobGroupId, self(), RP, no_jobs),
	    pmsJobGroup:pm_group_exit(get_name(?COMMON_JG, RP), 
				      JobGroupId,
				      self(),
				      normal),
	    ?LOG_RAM(?SEV_1,
		     {"Terminating (rop tick).~n"
		      "  Id = ~p~n"
		      "  RP = ~p~n",
		      [JobGroupId, RP]}),
	    exit(normal);
	{?ROP_TICK, RP, NextSyncTime} ->
	    Time = rop_begin_time(RP),
	    ?LOG_RAM(?SEV_1,
		     {"ROP tick received but ignored. ~p~n"
		      "  Id = ~p~n"
		      "  RP = ~p~n", 
		      [Time, JobGroupId, RP]}),
	    log_msg(rop_tick, {{RP, TimerRef}, NextSyncTime}),
	    loop(Loop);

	%%-------------------------------------------------------------
	%% Mnesia events
	%%-------------------------------------------------------------
	{mnesia_table_event, Event} when JobGroupId == ?COMMON_JG ->
	    ?LOG_RAM(?SEV_5, 
		     {"mnesia_table_event COMMON JG ~n  ~p~n", [Event]}),
	    loop(Loop);
	{mnesia_table_event, Event} ->
	    ?LOG_RAM(?SEV_1, 
		     {"mnesia_table_event~n"
		      "  JobGroupId = ~p~n"
		      "  Event      = ~p~n", [JobGroupId, Event]}),
	    {NewFileMethod, NewRepFm} = handle_event(Event, 
						     FileMethod, 
						     JobGroupId, 
						     RP,
						     RepFm),
	    loop(Loop#loop{file_method = NewFileMethod, 
			   rep_file_method = NewRepFm});

	%%-------------------------------------------------------------
	%% Server died, let this process also die
	%%-------------------------------------------------------------
	{'DOWN', SrvRef, process, SrvPid, Reason} ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"DOWN message from Server~n"
		      "  JobGroupId = ~p~n"
		      "  Message    = ~p~n", 
		      [JobGroupId, {SrvRef, process, SrvPid, Reason}]}),
	    exit(server_down_msg);

	%%-------------------------------------------------------------
	%% Job process dies
	%%-------------------------------------------------------------
	{'DOWN', MRef, process, Pid, Reason} ->
	    case get_down_reason(Pid, Loop) of
		not_found -> 
		    loop(Loop);
		{process_exited, Pids} -> 
		    ?LOG_RAM(?SEV_WARNING, 
			     {"DOWN message. EXITING~n"
			      "  JobGroupId = ~p~n"
			      "  Message    = ~p~n"
			      "  Pids       = ~p~n", 
			      [JobGroupId, 
			       {MRef, process, Pid, Reason},
			       Pids]}),
		    exit(Reason)
	    end;

	%%-------------------------------------------------------------
	%% Terminate this JobGroup process
	%%-------------------------------------------------------------
	{terminate = Msg, UserPid, Gid} ->
 	    log_msg(Msg, UserPid),
	    erlang:demonitor(SrvRef, [flush, info]),
	    ?LOG_RAM(?SEV_1,
		     {"Terminated.~n"
		      "  JobGroupId = ~p~n"
		      "  UserPid    = ~p~n"
		      "  GId        = ~p~n",
		      [JobGroupId, UserPid, Gid]}),
	    [terminate_job(JobId, JobPid, JobRef) || 
		{JobId, {JobPid, JobRef, _GP}} <- PmJobs],
	    pmsJobGroup:pm_group_exit(get_name(?COMMON_JG, RP), 
				      JobGroupId,
				      self(), 
				      no_jobs),
	    exit(normal);

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
	    Res = {JobGroupId, 
		   [{loop_size, erts_debug:flat_size(Loop)},
		    {proc_info, pmsLib:get_proc_info(self())},
		    {ets_tables, [{rop_tab, get_roptab_size(RopTab)}]}]},
	    UserPid ! {proc_info, Res},
	    loop(Loop);

	%%-------------------------------------------------------------
	%% Unknown messages
	%%-------------------------------------------------------------
	X ->
	    ?LOG_RAM(?SEV_5, 
		     {"UNKNOWN MSG. Id = ~p~n  ~p~n", [JobGroupId, X]}),
	    loop(Loop)
    end.

	
%%========================================================================
%% handle_meas_info(MeasInfo, Loop) -> Loop
%% 
%% Measurement info receive from pmsJob
%%========================================================================
%%-------------------------------------------------------------
%% meas data received
%%-------------------------------------------------------------
handle_meas_info({meas_info, From, GP, GPEnd, Data}, 
		 #loop{group_id        = JobGroupId,
		       mi_pids         = MiPids,
		       jg_pids         = JgPids,
		       rop_tab         = RopTab,
		       rop_begin_time  = RopBeginTime,
		       n_exp_meas_info = MiExp,
		       n_exp_job_group = JgExp
		      } = Loop)
  when is_integer(MiExp), MiExp > 0, GPEnd > RopBeginTime, RopBeginTime > 0 ->
    SD    = size(Data),
    Data5 = binary:bin_to_list(Data, 0, choose(SD > 400, 400, SD)),
    LogStr1 = io_lib:format("Received Meas Data. Id = ~p ~n"
			   "  Job   = ~p~n"
			   "  GP    = ~p~n"
			   "  GPEnd = ~p (~s)~n",
			   [JobGroupId, From, GP, GPEnd, pmsLib:rtime(GPEnd)]),
    LogStr5 = LogStr1 ++ "  Data  =\n" ++ Data5 ++ "...",
    ?LOG_RAM([{?SEV_1, {LogStr1, []}}, {?SEV_5, {LogStr5, []}}]), 
    log_msg(meas_info, {GP, GPEnd}),
    hmi_store_meas_info(RopTab, {meas_info, GP, GPEnd, Data}),
    NextExp = MiExp - 1,
    NextMip = get_next_mi_pids(From, MiPids),
    IsBuilt = build_rop_if_last_meas_info(NextExp, 
					  NextMip, 
					  JgExp, 
					  JgPids, 
					  Loop),
    Loop#loop{rop_begin_time  = choose(IsBuilt, 0, RopBeginTime),
	      n_exp_meas_info = NextExp,
	      mi_pids         = NextMip};


%%-------------------------------------------------------------
%% Discard measurement info from pmsJob because: 
%% - illegal timeref or
%% - all jobs have already reported meas info 
%%-------------------------------------------------------------
handle_meas_info({meas_info, From, GP, GPEnd, _Data}, 
		 #loop{group_id        = JobGroupId,
		       rop_begin_time  = RopBeginTime,
		       n_exp_meas_info = NExp} = Loop) ->
    ?LOG_RAM(?SEV_1,
	     {"Discard Meas Data. ~n"
	      "  Id        = ~p~n"
	      "  Job       = ~p~n"
	      "  GP        = ~p~n"
	      "  GPEnd     = ~p~n"
	      "  RPBegin   = ~p~n"
	      "  NExpected = ~p~n", 
	      [JobGroupId, From, GP, GPEnd, RopBeginTime, NExp]}),
    log_msg(meas_info, {GP, GPEnd}),
    Loop.


%% Do not store in the ets table if no meas data
hmi_store_meas_info(_, {meas_info, _, _, <<>>}) ->
    ok;
hmi_store_meas_info(RopTab, MeasInfo) ->
    ets:insert(RopTab, MeasInfo).
    

get_next_mi_pids(Pid, MiPids) ->
    case proplists:get_value(Pid, MiPids) of
	undefined ->    
	    MiPids;
	N when N > 1 ->
	    lists:keyreplace(Pid, 1, MiPids, {Pid, N - 1});
	_ ->
	    lists:keydelete(Pid, 1, MiPids)
    end.

%%========================================================================
%% handle_pm_group_meas_info(MeasInfo, Loop) -> Loop
%% 
%% Measurement info receive from another (non common) pmsJobGroup
%%========================================================================
handle_pm_group_meas_info({pm_group_meas_info, JobGroupId, From, RPEnd, MI},
			  #loop{mi_pids         = MiPids,
				jg_pids         = JgPids,
				rop_begin_time  = RopBeginTime,
				n_exp_meas_info = MiExp,
				n_exp_job_group = JgExp,
			        rop_tab         = RopTab} = Loop) 
  when is_integer(JgExp), JgExp > 0, RPEnd > RopBeginTime ->

    LogMsg1 = {"PmGroup measurement data~n"
	       "  JobGroupId  = ~p~n"
	       "  JobGroupPid = ~p~n"
	       "  RP End      = ~p~n"
	       "  RPBegin     = ~p~n",
	       [JobGroupId, From, RPEnd, RopBeginTime]},
    LogMsg5 = ?LFUN({"PmGroup measurement data~n"
		     "  JobGroupId  = ~p~n"
		     "  JobGroupPid = ~p~n"
		     "  RP End      = ~p~n"
		     "  RPBegin     = ~p~n"
		     "  Data        = ~p~n",
		     [JobGroupId, From, RPEnd, RopBeginTime, MI]}),
    ?LOG_RAM([{?SEV_1, LogMsg1}, {?SEV_5, LogMsg5}]),

    [hmi_store_meas_info(RopTab, {meas_info, GP, RPEnd, Data}) ||
	{GP, Data} <- MI],
    NextExp = JgExp - 1,
    NextJgp = lists:delete(From, JgPids),
    IsBuilt = build_rop_if_last_meas_info(MiExp, 
					  MiPids, 
					  NextExp, 
					  NextJgp, 
					  Loop),
    Loop#loop{rop_begin_time  = choose(IsBuilt, 0, RopBeginTime),
	      n_exp_job_group = NextExp,
	      jg_pids         = NextJgp};
%% The message arrived too late to be included in the ROP file
handle_pm_group_meas_info({pm_group_meas_info, JobGroupId, From, RPEnd, _MI},
			  #loop{rop_begin_time  = RopBeginTime,
				n_exp_job_group = NExp} = Loop) -> 
    ?LOG_RAM(?SEV_1, 
	     {"Discard PmsGroup measurement data~n"
	      "  JobGroupId   = ~p~n"
	      "  JobGroupPid  = ~p~n"
	      "  RP End       = ~p~n"
	      "  RPBegin      = ~p~n"
	      "  NoofExpected = ~p~n",
	      [JobGroupId, From, RPEnd, RopBeginTime, NExp]}),
    log_msg(pm_group_meas_info, {JobGroupId, From, RPEnd}),
    Loop.


%%========================================================================
%% get_mi_pids(PmJobs) -> [JobPid]
%% 
%% Get all pmsJob pids from which a meas info is expected for this RP
%%========================================================================
get_mi_pids(PmJobs, RP) ->
    get_mi_pids(PmJobs, RP, []).

get_mi_pids([], _RP, Acc) ->
    Acc;
get_mi_pids([{_JobId, {JobPid, _MRef, JobGP, JobOpState}} | T], RP, Acc) 
  when JobOpState =:= active ->
    N = pmsLib:get_interval(RP) div pmsLib:get_interval(JobGP),
    get_mi_pids(T, RP, [{JobPid, N} | Acc]);
get_mi_pids([{_JobId, {_JobPid, _MRef, _JobGP, _JobOpState}} | T], RP, Acc) ->
    get_mi_pids(T, RP, Acc).

    %% case pmsDb:pm_job_dirty_get(JobId) of
    %% 	{ok, [#pmJob{requestedJobState = ?JobState_ACTIVE}]} ->
    %% 	    get_mi_pids(T, [JobPid | Acc]);
    %% 	_ -> 
    %% 	    get_mi_pids(T, Acc)
    %% end.
    
    
%%========================================================================
%% get_jg_pids(PmJobs) -> [JobPid]
%% 
%% Get all pmsJob pids from which a meas info is expected for this RP
%%========================================================================
get_jg_pids(PmGroups) ->
    [Pid || {_JobGrpId, {Pid, _MRef}} <- PmGroups].



%%========================================================================
%% get_n_expected(Loop) -> integer()
%% 
%% Get number of expected pmJobs that should report meas data
%%========================================================================
get_n_expected(Loop) ->
    get_n_expected2(Loop).

get_n_expected2(Loop) ->
    %% Should probably check OpState here
    F = fun({_Id, {_Pid, _Ref, GP, OpState}}, Sum) when OpState =:= active ->
		Sum + pmsLib:get_interval(Loop#loop.rp) div 
		    pmsLib:get_interval(GP);
	   (_, Sum) ->
		Sum
	end,
    lists:foldl(F, 0, Loop#loop.pm_jobs).



%%========================================================================
%% pm_job_new(PmJob, Loop) -> Loop
%% 
%% A new PM Job is created.
%%========================================================================
pm_job_new(#pmJob{pmJobId = JobId} = PmJob, #loop{pm_jobs = PmJobs} = Loop) ->
    pjn(pmsJob:create(PmJob),
	PmJob,
	pjn_pm_jobs(proplists:get_value(JobId, PmJobs), JobId, Loop)).

pjn_pm_jobs(undefined, JobId, #loop{pm_jobs = PmJobs} = Loop) ->
    Loop#loop{pm_jobs = proplists:delete(JobId, PmJobs)};
pjn_pm_jobs(PmJobData, JobId, #loop{pm_jobs = PmJobs} = Loop) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Job create when already created~n"
	      "  JobId   = ~p~n"
	      "  JobData = ~p~n",
	      [JobId, PmJobData]}),
    Loop#loop{pm_jobs = proplists:delete(JobId, PmJobs)}.



pjn({ok, JobPid}, 
    #pmJob{pmJobId           = JobId,
	   reportingPeriod   = JobRP,
	   granularityPeriod = JobGP,
	   requestedJobState = _ReqState,
	   currentJobState   = CurrState},
    #loop{pm_jobs = PmJobs} = Loop) ->
    ?LOG_RAM(?SEV_5, 
	     {"Job process started.~n"
	      "  JobPid = ~p~n",
	      [JobPid]}),
    MRef = erlang:monitor(process, JobPid),
    OpState = job_op_state(CurrState),
    NewPmJobs = [{JobId, {JobPid, MRef, JobGP, OpState}} | PmJobs],
    {ok, Loop#loop{rp      = JobRP,
		   pm_jobs = NewPmJobs}}.

%%========================================================================
%% pm_job_update(PmsJobPid, PmJob, OldPmJob, Loop) -> Loop
%% 
%% A PM Job is created or updated
%%========================================================================
pm_job_update(undefined, 
	      #pmJob{pmJobId           = JobId,
		     granularityPeriod = JobGP,
		     requestedJobState = _ReqState,
		     currentJobState   = CurrState} = NewJob,
	      _OldJob,
	      #loop{pm_jobs = PmJobs} = Loop) ->
    {ok, JobPid} = pmsJob:create(NewJob),
    ?LOG_RAM(?SEV_5, 
	     {"Job process started.~n"
	      "  JobPid = ~p~n",
	      [JobPid]}),
    MRef = erlang:monitor(process, JobPid),
    PJs  = proplists:delete(JobId, PmJobs),
    IOpState = job_op_state(CurrState),
    Loop#loop{pm_jobs = [{JobId, {JobPid, MRef, JobGP, IOpState}} | PJs]};

pm_job_update({JobPid, _MRef, _JobGP, _OpState}, NewJob, OldJob, Loop) ->
    pmsJob:update(JobPid, NewJob, OldJob),
    ?LOG_RAM(?SEV_5, 
	     {"Job process updated.~n"
	      "  JobPid = ~p~n",
	      [JobPid]}),
    Loop.


job_op_state(?JobState_ACTIVE) ->
    active;
job_op_state(_) ->
    stopped.

%%========================================================================
%% pm_job_delete_move(PmJob, Loop, Reason) -> Loop
%% 
%% A PM Job is deleted or moved.
%%========================================================================
pm_job_delete_move(#pmJob{pmJobId = JobId} = PmJob,
		   #loop{group_id  = JobGroupId,
			 rp        = RP,
			 pm_jobs   = PmJobs,
			 pm_groups = PmGroups} = Loop,
		   Reason) ->
    pjd_del_mr(Reason, JobId),
    pjd(proplists:get_value(JobId, PmJobs), PmJob),
    NewPmJobs = proplists:delete(JobId, PmJobs),
    case NewPmJobs of
	[] when PmGroups   == [] andalso
		JobGroupId /= undefined -> 
	    %% default jobGroup should never be deleted
	    ?LOG_RAM(?SEV_1,
		     {"Pending delete.~n"
		      "  JobGroup = ~p~n"
		      "  RP       = ~p~n", 
		      [JobGroupId, RP]}),
	    Loop#loop{pm_jobs = NewPmJobs,
		      deleted = true};
	_  -> 
	    Loop#loop{pm_jobs = NewPmJobs}
    end.

pjd({Pid, MRef, _GP, _OpState}, PmJob) ->
    erlang:demonitor(MRef),
    pmsJob:delete(Pid, PmJob).


pjd_del_mr(delete, JobId) ->
    {ok, Recs} = pmsDb:measurement_reader_match(JobId),
    [pjd_dmr(Rec) || Rec <- Recs],
    ok;
pjd_del_mr(move, _JobId) ->
    ok.

pjd_dmr(#measurementReader{measurementReaderId = MrId} = Rec) ->
    ?LOG_RAM(?SEV_5, {"deleteMo ~p ~p~n", ["measurementReader", MrId]}),
    pmsDb:measurement_reader_delete(Rec).
   


pm_job_takeover(JobId, 
		TakeoverPid,
		NewJobGroup,
		#loop{group_id = OldJobGroup,
		      pm_jobs  = PmJobs} = Loop) ->
    {JobPid, MRef, _GP, _OpState} = proplists:get_value(JobId, PmJobs),
    ok = pmsJob:takeover(JobPid, OldJobGroup, NewJobGroup, TakeoverPid),
    erlang:demonitor(MRef),
    Loop#loop{pm_jobs = proplists:delete(JobId, PmJobs)}.


over_taken_job(ToJobId,
	       ToJobPid,
	       JobGP,
	       ToOpState,
	       #loop{pm_jobs   = PmJobs,
		     mi_pids   = MiPids,
		     to_jobs   = ToJobs,
		     n_exp_meas_info = NExp,
		     rp        = RP,
		     timer_ref = _TimerRef} = Loop) ->
    MRef = erlang:monitor(process, ToJobPid),
    N = pmsLib:get_interval(RP) div pmsLib:get_interval(JobGP),
    NewMiPids = lists:keystore(ToJobPid, 1, MiPids, {ToJobPid, N}),
    OpState = job_op_state(ToOpState),
    Loop#loop{pm_jobs         = [{ToJobId, {ToJobPid, MRef, JobGP, OpState}} 
				 | PmJobs],
	      mi_pids         = NewMiPids,
	      to_jobs         = lists:delete(ToJobId, ToJobs),
	      n_exp_meas_info = NExp + N
	      }.


job_op_state_update(JobId, OpState, JobPid, Loop) ->
    PmJobs = Loop#loop.pm_jobs,
    UpdOpState = job_op_state(OpState),
    case lists:keyfind(JobId, 1, PmJobs) of
	{JobId, {JobPid, MRef, JobGP, OldOpState}} 
	  when OldOpState =/= UpdOpState ->
	    NewPmJobs = 
		lists:keyreplace(JobId, 1, PmJobs,
				 {JobId, {JobPid, MRef, JobGP, UpdOpState}}),
	    job_mi_pids_update(UpdOpState, JobPid, JobGP, 
			       Loop#loop{pm_jobs = NewPmJobs});
	_False ->
	    Loop
    end.


job_mi_pids_update(active, JobPid, JobGP, Loop) ->
    MiPids = Loop#loop.mi_pids,
    NExp = Loop#loop.n_exp_meas_info,
    N = pmsLib:get_interval(Loop#loop.rp) div pmsLib:get_interval(JobGP),
    NewMiPids = lists:keystore(JobPid, 1, MiPids, {JobPid, N}),
    NewLoop = update_common_job_group(Loop#loop{mi_pids = NewMiPids}),
    case lists:keyfind(JobPid, 1, MiPids) of
	{JobPid, _OldN} ->
	    NewLoop;
	_ ->
	    NewLoop#loop{n_exp_meas_info = NExp + N}
    end;

job_mi_pids_update(_OpState, JobPid, _JobGP, Loop) ->
    MiPids = Loop#loop.mi_pids,
    NExp = Loop#loop.n_exp_meas_info,
    NewMiPids = lists:keydelete(JobPid, 1, MiPids),
    NewLoop = update_common_job_group(Loop#loop{mi_pids = NewMiPids}),
    case lists:keyfind(JobPid, 1, MiPids) of
	{JobPid, N} ->
	    NewLoop#loop{n_exp_meas_info = NExp - N};	   
	_ ->
	    NewLoop
    end.


update_common_job_group(#loop{group_id = JobGroupId,
			      mi_pids = MiPids,
			      rep_file_method = RepFM,
			      rp = RP,
			      file_method = {FileMethod, _RopBegin}} = Loop) ->
    if
	MiPids =:= [], 
	RepFM =:= single ->
	    Time = rop_begin_time(RP),
	    NewState = ?FileHandlingMethod_MULTIPLE_ROP_FILES,
	    NewRepFM = update_common(JobGroupId, NewState, RP, Time),
	    Loop#loop{rep_file_method = NewRepFM};
	MiPids =/= [], 
	RepFM =:= multi, 
	FileMethod =:= ?FileHandlingMethod_SINGLE_ROP_FILE ->
	    Time = rop_begin_time(RP) + pmsLib:get_interval(RP),
	    NewRepFM = update_common(JobGroupId, FileMethod, RP, Time),
	    Loop#loop{rep_file_method = NewRepFM};
	true ->
	    Loop
    end.

%%========================================================================
%% build_rop_if_last_meas_info(NExp, Loop) -> true | false
%% 
%%========================================================================
build_rop_if_last_meas_info(0, _, 0, _, Loop)  ->
    RP = Loop#loop.rp,
    ET = rop_begin_time(RP),
    build_rop_file(Loop, ET, RP, Loop#loop.group_id);

build_rop_if_last_meas_info(MiExp, _, JgExp, _, #loop{group_id = JgId}) ->
    ?LOG_RAM(?SEV_5, 
	     {"Still missing meas data from ~p jobs and "
	      "~p job groups ~p.~n", [MiExp, JgExp, JgId]}),
    false.


%%========================================================================
%% build_rop_file(Loop, GPEnd, RP) -> ok.
%% 
%%========================================================================
build_rop_file(#loop{rop_begin_time = 0}, _ET, _RP, JobGroupId) ->
    ?LOG_RAM(?SEV_5, 
	     {"Build rop file false JobGroupId ~p~n", [JobGroupId]}),
    false;
build_rop_file(#loop{rop_begin_time = BT,
		     mi_2_common    = MiToCommon,
		     file_method    = {_, FmTime},
		     sw_vsn         = SwVsn,
		     rop_tab        = RopTab},
	       ET, 
	       RP, 
	       JobGroupId) ->
    case brf_get_meas_data(RopTab) of
	[] ->
	    ?LOG_RAM(?SEV_1, 
		     {"No rop file created, no meas data received.~n"
		      "  JobGroup = ~p~n",
		      [JobGroupId]}),
	    true;
	MeasData ->
	    ets:delete_all_objects(RopTab),
	    FMD = [{GP, MI} || {_, GP, GPE, MI} <- MeasData, 
			       GPE - pmsLib:get_interval(GP) >= BT, GPE =< ET],
	    ?LOG_RAM(?SEV_5,
		     {"Rop file create.~n  start of period: ~p ~n  ~p~n", 
		      [BT, JobGroupId]}),
	    
	    MEData = comsaI:get_managed_element_data(),
	    do_build_rop_file(FMD, 
			      MiToCommon, 
			      BT, 
			      ET, 
			      FmTime + pmsLib:get_interval(RP),
			      RP, 
			      JobGroupId, 
			      MEData, 
			      SwVsn)
    end.


do_build_rop_file([], _BT, _, _ET, _FM, _RP, JobGroupId, _MEData, _SwVsn) ->
    ?LOG_RAM(?SEV_1, {"Rop file create. No valid MeasData received.~n"
		      "  JobGroup = ~p~n",
		      [JobGroupId]}),
    true;
do_build_rop_file(MeasData, 
		  single = MiToCommon, 
		  BTime, 
		  ETime, 
		  FmTime,
		  RP, 
		  JobGroupId, 
		  _MEData, 
		  _SwVsn) ->
    ?LOG_RAM(?SEV_1, 
	     {"PmsGroup measurement data sent to common job group. ~p~n"
	      "  JobGroupId = ~p~n"
	      "  Mi2Common  = ~p~n"
	      "  FM Time    = ~p (~s)~n"
	      "  Begin Time = ~p (~s)~n"
	      "  End Time   = ~p (~s)~n",
	      [rop_begin_time(RP), JobGroupId, MiToCommon, 
	       FmTime, pmsLib:rtime(FmTime),
	       BTime, pmsLib:rtime(BTime), 
	       ETime, pmsLib:rtime(ETime)]}),
    pmsJobGroup:pm_group_meas_info(get_name(?COMMON_JG, RP), 
				   JobGroupId, 
				   self(), 
				   ETime, 
				   MeasData),
    true;
do_build_rop_file(MeasData, 
		  MiToCommon, 
		  BTime, 
		  ETime, 
		  FmTime,
		  RP, 
		  JobGroupId, 
		  MEData, 
		  SwVsn) ->
    ?LOG_RAM(?SEV_5, 
	     {"PmsGroup start to build rop file. ~p~n"
	      "  JobGroupId = ~p~n"
	      "  Mi2Common  = ~p~n"
	      "  FM Time    = ~p (~s)~n"
	      "  Begin Time = ~p (~s)~n"
	      "  End Time   = ~p (~s)~n",
	      [rop_begin_time(RP), JobGroupId, MiToCommon, 
	       FmTime, pmsLib:rtime(FmTime),
	       BTime, pmsLib:rtime(BTime), 
	       ETime, pmsLib:rtime(ETime)]}),
    MeasInfo  = iolist_to_binary([MI || {_GP, MI} <- MeasData]),
    LocalDN   = proplists:get_value(siteLocation, MEData),
    UserLabel = proplists:get_value(userLabel, MEData),
    ME        = pmsRopXml:managedElement(LocalDN, UserLabel, SwVsn),
    MD        = pmsRopXml:measData(ME, MeasInfo),
    DNPrefix  = proplists:get_value(dnPrefix, MEData),
    ElemType  = proplists:get_value(managedElementType, MEData),
    Sender    = pmsRopXml:fileSender(LocalDN, ElemType),
    BeginTime = pmsRopXml:measCollecBegin(BTime),
    Header    = pmsRopXml:fileHeader(?VSN, ?VENDOR, DNPrefix, Sender, BeginTime),
    EndTime   = pmsRopXml:measCollecEnd(ETime),
    Footer    = pmsRopXml:fileFooter(EndTime),
    File      = pmsRopXml:measCollecFile(Header, MD, Footer),
    Type      = get_rop_type(RP, MeasData),
    NeMEId    = proplists:get_value(networkManagedElementId, MEData),
    UniqueId  = get_unique_id(NeMEId, JobGroupId),
    FileName  = binary_to_list(pmsRopXml:measResultFileName(Type, 
							    BTime, 
							    ETime, 
							    undefined, 
							    UniqueId)) ++ ".xml",
    pmsDb:rop_file_store(FileName, iolist_to_binary([File])),
    ?LOG_RAM(?SEV_1, 
	     {"Rop file created~n"
	      "  FileName = ~p~n" 
	      "  JobGroup = ~p~n", 
	      [FileName, JobGroupId]}),
    true.

get_unique_id(undefined, undefined) ->
    undefined;
get_unique_id(NeMEId, undefined) ->
    NeMEId;
get_unique_id(undefined, JobGroupId) ->
    JobGroupId;
get_unique_id(NeMEId, JobGroupId) ->
    NeMEId ++ "_" ++ JobGroupId.


brf_get_meas_data(Tab) ->
    ets:tab2list(Tab).


job_state_update_common(NewMiToCommon, _MiPids, _JobGroupId, _RP, _Time, RepFm) 
  when RepFm =:= common; NewMiToCommon =:= RepFm ->
    RepFm;

job_state_update_common(NewMiToCommon, MiPids, JobGroupId, RP, Time, RepFm) ->
    case NewMiToCommon of
	single when RepFm =:= multi, MiPids =/= [] ->
	    NewState = ?FileHandlingMethod_SINGLE_ROP_FILE,
	    update_common(JobGroupId, NewState, RP, Time);
	multi when RepFm =:= single ->
	    NewState = ?FileHandlingMethod_MULTIPLE_ROP_FILES,
	    update_common(JobGroupId, NewState, RP, Time);
	_ ->
	    RepFm
    end.


handle_event({write, pmSupport, #pmSupport{ropFileHandling = StateNew}, _, _},
	     FileStateNew,
	     JobGroupId,
	     RP,
	     RepFm) ->
    BeginTime = rop_begin_time(RP), 
    NewRepFm = he_update_common({StateNew, BeginTime}, 
				FileStateNew, 
				JobGroupId, 
				RP,
				BeginTime + pmsLib:get_interval(RP),
				RepFm),
    
    {{StateNew, rop_begin_time(RP)}, NewRepFm}.


he_update_common({SameState, _}, {SameState, _}, _, _, _, RepFm) ->
    RepFm;
he_update_common({NewState, _}, _, JobGroupId, RP, Time, _RepFm) ->
    update_common(JobGroupId, NewState, RP, Time).


update_common(?COMMON_JG, _State, _RP, _Time) ->
    common;
update_common(JobGroupId, State, RP, Time) ->
    ?LOG_RAM(?SEV_1, 
	     {"Sending pm_group_update to common jobGroup.~n"
	      "  JobGroupId = ~p~n"
	      "  FileState  = ~p (~p)~n"
	      "  Time       = ~p (~s)~n",
	      [JobGroupId, State, format_ff(State), Time, pmsLib:rtime(Time)]}),
    pmsJobGroup:pm_group_update(get_name(?COMMON_JG, RP), 
				self(), 
				JobGroupId, 
				State,
			        Time),
    format_ff(State).
    
%% %%===========================================================================
%% %% Misc functions
%% %%===========================================================================
log_msg(_, _) -> ok.


log_loop(_)   -> ok.


rop_begin_time(RP) ->
    %% The end of the previous GP is the start of the current ROP. 
    pmsLib:gp_end_time(RP).
 

get_rop_type(RP, MeasData) ->
    case is_multi_gp_rop(RP, MeasData) of
	true ->
	    ?ROP_TYPE_C;
	_ ->
	    ?ROP_TYPE_A
    end.


is_multi_gp_rop(RP, MeasData) ->
    F = fun({GP, _D}) when GP =:= RP ->
		false;
	   (_) ->
		true
	end,
    lists:any(F, MeasData).
		

resync(#loop{timer_ref = RP}, RP, NextSyncTime) ->
    {SecOff, MilOff} = get_offset(RP, os:timestamp(), NextSyncTime),
    pmsLib:gp_tick(?ROP_TICK, RP, SecOff, MilOff);
resync(_, _, _) ->
    not_resynced.

get_offset(RP, Now, NextSyncTime) when Now >= NextSyncTime ->
    {pmsLib:get_rop_offset(RP), 0};
get_offset(RP, Now, NextSyncTime) ->
    Diff = timer:now_diff(NextSyncTime, Now) div 1000,
    DiffSec = Diff div 1000,
    DiffMil = Diff rem 1000,
    {pmsLib:get_rop_offset(RP) + DiffSec, DiffMil}.


get_name(Id, RP) when is_atom(Id) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ 
		 atom_to_list(Id) ++ "_" ++ 
		 integer_to_list(RP));
get_name(Id, RP) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ 
		 Id ++ "_" ++ 
		 integer_to_list(RP)).



%%================================================================
%% get_send_mi_2_common({FileHandling, Time}, RP, 
%%                       BeginTime, JobGoupId, OldState) ->
%%       StartToSendAtTime | common | multi 
%%
%%   FileHandling = single | multi
%%
%%   StartToSendAtTime = integer()
%%       Indicates when to start to send meas data to the common
%%       pmsJobGroup process
%% 
%% Set mi_2_common attribute. Indicates if and when to start
%% to send meas data to the common pmsJobGroup.
%%================================================================
get_send_mi_2_common(_, _, _, _, undefined, _, _) ->
    ?LOG_RAM(?SEV_5, {"get_send_mi_2_common  ~p~n", [common]}),
    common;


get_send_mi_2_common({FileFormat, Time} = FM, 
		     RP, 
		     RopBegin,
		     UpdateDelay, 
		     JobGroupId, 
		     Old,
		     MiPids) ->
    IsUpdate = Time < RopBegin,
    RBT      = rop_begin_time(RP),
    New      = get_new_mi2_common(MiPids, IsUpdate, Old, FileFormat),
    ?LOG_RAM(?SEV_1, 
	     {"PmsGroup get_send_mi_2_common. ~p~n"
	      "  JobGroupId = ~p~n"
	      "  Rop Begin  = ~p (~p)~n"
	      "  FM         = ~p~n"
	      "  Delay      = ~p~n"
	      "  BTime      = ~p (~p)~n"
	      "  Old        = ~p~n"
	      "  New        = ~p~n"
	      "  MiPids     = ~p~n",
	      [choose(IsUpdate, "UPDATE", "KEEP"),
	       JobGroupId, 
	       RBT,
	       pmsLib:rtime(RBT),	       
	       FM, 
	       UpdateDelay, 
	       RopBegin,
	       pmsLib:rtime(RopBegin),
	       Old,
	       New,
	       MiPids]}),
    New.


get_new_mi2_common(MiPids, IsUpdate, Old, FileFormat) ->
    case MiPids of
	[] ->
	    multi;
	_ when IsUpdate ->
	    format_ff(FileFormat);
	_ ->
	    Old
    end.

%%================================================================
%% get_job_groups(Loop) -> {Run, Add, Rem}
%%  
%% Check if any job group process should be added or deleted
%% from the currently running list.
%% This is only valid for the common job group.
%%================================================================
get_job_groups(#loop{group_id      = undefined,
		     rp            = RP,
		     pm_groups     = PmGroupsRun,
		     pm_groups_add = PmGroupsAdd,
		     pm_groups_rem = PmGroupsRem}) ->
    RopBegin = rop_begin_time(RP),
    Add = gjg_del_add(PmGroupsAdd, RopBegin),
    Rem = gjg_del_rem(PmGroupsRem, RopBegin),
    Run = gjg_run(PmGroupsRun, Rem, Add),
    
    ?LOG_RAM(?SEV_5, 
	     {"get_job_groups  ~n"
	      " JobGroup = ~p~n" 
	      " RopBegin = ~p (~s)~n" 
	      " RunBef   = ~p~n" 
	      " AddBef   = ~p~n"
	      " RemBef   = ~p~n" 
	      " Run      = ~p~n"
	      " Add      = ~p~n"
	      " Rem      = ~p~n", 
	      [undefined, RopBegin, pmsLib:rtime(RopBegin),
	       PmGroupsRun, PmGroupsAdd, PmGroupsRem,
	       Run, Add, Rem]}),
    gjg_demonitor(Rem, PmGroupsRun),
    {Run, PmGroupsAdd -- Add, PmGroupsRem -- Rem};
get_job_groups(#loop{pm_groups     = Run,
		     pm_groups_add = Add,
		     pm_groups_rem = Rem}) ->
    {Run, Add, Rem}.


gjg_del_add(PmGroups, RopBegin) ->
    [Entry || {_, {_, _, Time}} = Entry <- PmGroups, Time =< RopBegin].

gjg_del_rem(PmGroups, RopBegin) ->
    [Entry || {_, Time} = Entry <- PmGroups, Time =< RopBegin].


gjg_run(PmGroupsRun, Rem, Add) -> 
    AddToRun = [{AddId, {Ref, Pid}} || {AddId, {Ref, Pid, _Time}} <- Add],
    lists:foldl(fun({RmId, _Ref}, Acc) ->
			proplists:delete(RmId, Acc)
		end, PmGroupsRun, Rem) ++ AddToRun. 


gjg_demonitor([], _PmGroupsRun) ->
    ok;
gjg_demonitor([{Key, _} | T], Running) ->
    gv_demonitor(proplists:get_value(Key, Running, undefined)),
    gjg_demonitor(T, Running).


%%================================================================
%% handle_pm_group_update(...) -> {Run, Add, Rem}
%%       
%% A non common job group informs the common job group process
%% that the file handling state is updated.
%%
%% This will be valid from the next RP. The current RP should not
%% be affected. The status change must therefor be temporarily
%% stored in a variable.
%%================================================================
%% Add the JobGroup to Running because the add time is less than current time
handle_pm_group_update(?FileHandlingMethod_SINGLE_ROP_FILE,  
		       Time,
		       BeginTime,
		       JobGroupId,
		       JobGroupPid,
		       _IsRunning,
		       PmGroupsRun,
		       PmGroupsAdd,
		       PmGroupsRem) when Time =< BeginTime ->
    ?LOG_RAM(?SEV_1, {"~p handle_pm_group_update Time =< BeginTime ~n"
		      "  Time      = ~p~n"
		      "  BeginTime = ~p~n", 
		      [JobGroupId, Time, BeginTime]}),
    Run = jg_del(JobGroupId, PmGroupsRun),
    
    Ref = erlang:monitor(process, JobGroupPid),
    {[{JobGroupId, {JobGroupPid, Ref}} | Run], 
     jg_del(JobGroupId, PmGroupsAdd),
     jg_del(JobGroupId, PmGroupsRem)};
%% Add the JobGroup to Add or Rem depending if the state is changed
%% to single or multi ROP file handling.
handle_pm_group_update(State,  
		       Time,
		       BeginTime,
		       JobGroupId,
		       JobGroupPid,
		       IsRunning,
		       PmGroupsRun,
		       PmGroupsAdd,
		       PmGroupsRem) ->
    ?LOG_RAM(?SEV_1, 
	     {"handle_pm_group_update ~n"
	      "  FileState  = ~p (~p)~n"
	      "  JobGroupId = ~p~n"
	      "  Time       = ~p~n"
	      "  BeginTime  = ~p~n", 
	      [State, format_ff(State), JobGroupId, Time, BeginTime]}),
    {Add, Rem} = jg_add(State, 
			Time,
			JobGroupId, 
			JobGroupPid, 
			IsRunning,
			PmGroupsAdd, 
			PmGroupsRem),
    {PmGroupsRun, Add, Rem}.




jg_add(?FileHandlingMethod_SINGLE_ROP_FILE, Time, Gid, Pid, _, Add, Rem) ->
    Ref = erlang:monitor(process, Pid),
    {[{Gid, {Pid, Ref, Time}} | jg_del(Gid, Add)], Rem};
jg_add(?FileHandlingMethod_MULTIPLE_ROP_FILES, Time, Gid, _, true, Add, Rem) ->
    {Add, [{Gid, Time} | jg_del(Gid, Rem)]};
%% do not add the job group to rem if it is not running
jg_add(?FileHandlingMethod_MULTIPLE_ROP_FILES, _, _, _, false, Add, Rem) ->
    {Add, Rem}.
    
jg_del(Key, PropList) ->
    gv_demonitor(proplists:get_value(Key, PropList, undefined)),
    proplists:delete(Key, PropList).

gv_demonitor({_, MRef}) ->
    erlang:demonitor(MRef);
gv_demonitor({_, MRef, _}) ->
    erlang:demonitor(MRef);
gv_demonitor(_Other) ->
    ok.
    


%%================================================================
%% handle_pm_group_exit(...) -> {Run, Add, Rem}
%% 
%% A pmsJobGroup informs the common job group that it will exit.
%% Remove all references to it.
%%================================================================
%% Add the JobGroup to Running because the add time is less than current time
handle_pm_group_exit({pm_group_exit, DownJobGrpId, _Pid, _Reason},
		     #loop{pm_groups       = Run,
			   pm_groups_add   = PmGroupsAdd,
			   pm_groups_rem   = PmGroupsRem} = Loop) ->
    Loop#loop{pm_groups       = jg_del(DownJobGrpId, Run),
	      pm_groups_add   = jg_del(DownJobGrpId, PmGroupsAdd),
	      pm_groups_rem   = jg_del(DownJobGrpId, PmGroupsRem)}.



%%================================================================
%% get_down_reason(Pid, Loop) -> not_found | Reason
%% 
%% Check if the DOWN is comming from a known Pid
%%================================================================
get_down_reason(Pid,
		#loop{pm_jobs       = PmJobs,
		      pm_groups     = Run,
		      pm_groups_add = Add,
		      pm_groups_rem = Rem}) ->

    PmGrps = lists:append([Run, Add, Rem]),

    Exit = [{pm_job, Id}       || {Id, {P, _, _}} <- PmJobs, P == Pid] ++
           [{pm_job_group, Id} || {Id, {P, _}}    <- PmGrps, P == Pid],
    
    case Exit of
	[Reason | _] -> {process_exited, Reason};
	[]           -> not_found
    end.
		    


%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

terminate_job(JobId, Pid, Ref) ->
    erlang:demonitor(Ref, [flush, info]),
    pmsJob:terminate(Pid, JobId).


get_roptab_size(RopTab) ->
    List = ets:tab2list(RopTab),
    Fun = fun({_, _, _, Bin}, Len) -> size(Bin) + Len end,
    %% Count the leght of all binaries 
    %% plus length of the tuples 
    %% plus length of the ets table 
    lists:foldl(Fun, 0, List) + 
	(length(List) * 12) + 
	ets:info(RopTab, memory).
		  
    

format_loop(Loop) ->
    F       = record_info(fields, loop),
    [_ | L] = tuple_to_list(Loop), 
    lists:zip(F,L).

format_ff(?FileHandlingMethod_SINGLE_ROP_FILE)    -> single;
format_ff(?FileHandlingMethod_MULTIPLE_ROP_FILES) -> multi;
format_ff(_)                                      -> illegal_value.


choose(true,  T, _) -> T;
choose(false, _, F) -> F.

    

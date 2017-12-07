%%% ------------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ------------------------------------------------------------------------
%%% %CCaseFile:	pesJob.erl %
%%% @private
%%% 
%%% Description:
%%%
%%% pesJob process is started, from pesJobGroup, when a PM Job instance 
%%% is created. pesJob contains a collection of counters to be measured.
%%% 
%%% pesJob sends a subscribe message, when it is in state active,
%%% and an unsubscribe when it switches from active to passive state
%%% to all application instances handling the measured counters.
%%%
%%% To find all application instances pesJob reads #pesAppRegistry.
%%% That table contains PIDs to all pesAppJob processes and what counter
%%% each of them is handling.
%%% 
%%% The (un)subrscibe requests are not sent directly to the application
%%% but to the pesAppJob process. There can be several PM Jobs active 
%%% simultaneous measuring the same counter(s). The pesAppJob process 
%%% builds a union of all counters to be measured and sends the union 
%%% in the subscribe message to the application instance.
%%% 
%%% pesAppJob is responsible to send the report request at granularity
%%% periods to the application. It also receives the measured data
%%% which is stored in #pesMeasData record. pesJob will read the
%%% received data from that record at GP + GP/8 and builds a measInfo
%%% in xml-format to be used by pesJobGroup when building the rop file 
%%% at GP + GP/4 (the ROP file should be ready at GP + GP/3).
%%% ------------------------------------------------------------------------
-module(pesJob).
-vsn('/main/R3A/R4A/R5A/3').
-date('2016-04-05').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    --------------------------------------
%%% R1A/1      2013-01-18 uabesvi     Created
%%% R2A/10     2013-03-18 uabesvi     Cleaned up
%%% R4A/1      2015-11-25 uabesvi     HU38028
%%% ------------------------------------------------------------------------
%%%
%%% ------------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ------------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ------------------------------------------------------------------------

-export([create/1]).
-export([update/3]).
-export([delete/2]).
-export([terminate/2]).

-export([get_current_state/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/2]).

-include("pes.hrl").
-include("RcsPMEventM.hrl").

-record(loop, {job_id, 
	       job_state,
	       req_state,
	       types,
	       rp        = 0,
	       producer,      %% {ParentPid, MonitorRef}
	       job_rec
	      }).

-define(SEND_TO,   5000).
-define(CREATE_TO, 5000).


%% -define(MT_KEY(G, T), {"1", "1", "1", itl(G), T}).
-define(MI_ID(PmGroup), "PM=1,PmGroup=" ++ PmGroup).
-define(ME, "ManagedElement").

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% create(PmJob) -> {ok, {Pid, MonitorRef}} | {error, Reason}
%%
%% Create a new PM Job process.
%%========================================================================
create(PmJob) ->
    Self = self(),
    spawn(fun() -> init(Self, PmJob) end),

    receive
	{event_job_init, res, Pid} ->
	    {ok, Pid}
    after ?CREATE_TO ->
	    {error, {timeout, {?MODULE, create}}}
    end.



%%========================================================================
%% update(Pid, PmJob, Old) -> ok | {error, Reason}
%%
%% Update a PM Job.
%%========================================================================
update(Pid, PmJob, Old) ->
    send(Pid, update, {PmJob, Old}).

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
%% 
%%
%% 
%%========================================================================
get_current_state(Pid) ->
    Pid ! {get_current_state, self()},
    receive
	{get_current_state, res, Res} ->
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
    receive
	{Msg, res, Res} ->
	    Res
    after ?SEND_TO ->
	    {error, {timeout, {?MODULE, Msg}}}
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
%% to #pesAppRegistry.
%% 
%% Send subscribe request to attached applications, if PM Job is active.
%% In that case also start the measInfo tick.
%%========================================================================
init(Pid,
     #eventJob{eventJobId        = JobId, 
	       requestedJobState = ReqJobState,
	       currentJobState   = CurrJobState,
	       eventGroupRef     = EventGrpRef,
	       eventTypeRef      = EventTypeRef,
	       reportingPeriod   = RP} = EventJob) ->
    put(name, {?MODULE, JobId}), %% used by pes_cli to print process data
    log_msg(init, JobId),
    MRef = erlang:monitor(process, Pid),
    pesDb:mnesia_subscribe({table, pesAppRegPid, detailed}),
    Types = get_types(EventGrpRef, EventTypeRef),
    CurrState = choose(ReqJobState == ?SessionState_ACTIVE,
		       ?SessionState_ACTIVATING,
		       ?SessionState_STOPPED),
    Loop = #loop{job_id    = JobId, 
		 job_state = CurrState,
		 req_state = ReqJobState,
		 types     = Types,
		 rp        = RP,
		 producer  = {Pid, MRef},
		 job_rec   = EventJob
		},
    update_current_state(JobId, CurrState),
    ReqJobState == ?SessionState_ACTIVE andalso
	send_job_request(ReqJobState, JobId, Types, EventJob),
    Pid ! {event_job_init, res, self()},
    ?LOG_RAM(?SEV_1, 
	     {"Started ~n"
	      "  EventJobId   = ~p~n"
	      "  ReqJobState  = ~p~n"
	      "  CurrJobState = ~p~n", 
	      [JobId, ReqJobState, CurrJobState]}),
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
loop(#loop{job_id    = JobId, 
	   producer  = {ProdPid, ProdRef},
	   job_state = JobState,
	   types     = Types,
	   job_rec   = _EventJob
	  } = Loop) ->
    log_loop(Loop),
    receive
	%%-------------------------------------------------------------
	%% PM Job update
	%%-------------------------------------------------------------
	{update = Msg, 
	 UserPid, 
	 {#eventJob{requestedJobState = NewReqState,
		    eventGroupRef     = EventGrpRef,
		    eventTypeRef      = EventTypeRef} = NewJob,
	  [OldJob]}} ->
	    log_msg(Msg, {NewJob, OldJob}),
	    LogStr = io_lib:format("Job update:~n"
				   "  NewJob = ~n~s~n  OldJob = ~n~s~n",
				   [pp(NewJob), pp(OldJob)]),
	    ?LOG_RAM(?SEV_1, {LogStr, []}), 
	    LoopSync = update_state(NewJob, OldJob, Loop),
	    NewTypes = get_types(EventGrpRef, EventTypeRef),
	    NewState = send_job_request(NewReqState, JobId, NewTypes, NewJob),
	    JobState =:= LoopSync#loop.job_state 
		orelse 
		pesDb:event_job_set(NewJob#eventJob{currentJobState = 
						    NewState}),
	    UserPid ! {Msg, res, ok},
	    loop(LoopSync#loop{types     = NewTypes,
			       req_state = NewReqState,
			       job_state = NewReqState});	
	%%-------------------------------------------------------------
	%% Delete or terminate PmJob
	%%-------------------------------------------------------------
	{delete = Msg, UserPid, DelEventJob} ->
	    log_msg(Msg, DelEventJob),
	    send_job_request(delete, JobId, Types, DelEventJob),
	    UserPid ! {Msg, res, ok},
	    ?LOG_RAM(?SEV_1, {"Deleted ~n  Job = ~p~n", [JobId]}),
	    exit(deleted);
	%%-------------------------------------------------------------
	%% Terminate PmJob
	%%-------------------------------------------------------------
	{terminate = Msg, UserPid, TermEventJob} ->
	    log_msg(Msg, TermEventJob),
	    erlang:demonitor(ProdRef, [flush, info]),
	    send_job_request(delete, JobId, Types, TermEventJob),
	    UserPid ! {Msg, res, ok},
	    ?LOG_RAM(?SEV_1, {"Terminated ~n  Job = ~p~n", [JobId]}),
	    exit(terminated);
	%%-------------------------------------------------------------
	%% Mnesia table events, only subscribing to #pesAppRegistry
	%%-------------------------------------------------------------
	{mnesia_table_event, Event} ->
	    NewState = check_event(Event, Loop),
	    ok = update_current_state(JobId, NewState),
	    loop(Loop#loop{job_state = NewState});
	%%-------------------------------------------------------------
	%% Get current job state
	%%-------------------------------------------------------------
	{get_current_state, Pid} ->
	    Pid ! {get_current_state, res, JobState},
	    loop(Loop);	    
	%%-------------------------------------------------------------
	%% PesJobGroup died, let this process also die
	%%-------------------------------------------------------------
	{'DOWN', ProdRef, process, ProdPid, Reason} ->
	    ?LOG_RAM(?SEV_WARNING,
		     {"DOWN message from JobGroup ~p~n", 
		      [{ProdRef, process, ProdPid, Reason, Loop}]}),
	    exit(producer_down_msg);
	%%-------------------------------------------------------------
	%% Read loop data
	%%-------------------------------------------------------------
	{get_loop_data, UserPid} ->
	    UserPid ! {loop_data, format_loop(Loop)},
	    loop(Loop);
	%%-------------------------------------------------------------
	%% Unknown messages
	%%-------------------------------------------------------------
	X ->
	    ?LOG_RAM(?SEV_WARNING,
		     {"PmJob Received unknown message ~p~n", [X]}),
	    log_msg(X, unknown_message),
	    loop(Loop)
    end.

%%========================================================================
%% check_event(Event, Loop) -> ok.
%% 
%%========================================================================
check_event({write, 
	     pesAppRegPid, 
	     #pesAppRegPid{job_pid = JobPid}, 
	     _Old, 
	     _Transaction},
	    #loop{req_state = ?SessionState_ACTIVE,
		  job_rec   = EventJob,
		  types     = Types}) ->
    pesAppJob:job_request(JobPid, Types, EventJob),
    ?SessionState_ACTIVE;
check_event({write, pesAppRegPid, _PmJob, _Old, _Transaction},
	    #loop{req_state = ?SessionState_STOPPED}) ->
    ?SessionState_STOPPED;
check_event({delete,
	     pesAppRegPid,
	     #pesAppRegPid{event_types = Types},
             _Old,
	     _Transaction},
	    #loop{req_state = ?SessionState_ACTIVE}) ->
    log_msg(check_event, {delete, Types}),
    ?SessionState_ACTIVATING;
check_event({delete,
	     pesAppRegPid,
	     #pesAppRegPid{event_types = Types},
             _Old,
	     _Transaction},
	    #loop{req_state = ?SessionState_STOPPED}) ->
    log_msg(check_event, {delete, Types}),
    ?SessionState_STOPPED;
check_event(Event, #loop{job_state = State}) ->
    ?LOG_RAM(?SEV_WARNING,
	     {"UNKNOWN MNESIA EVENT ~p ~p~n", [State, Event]}),
    State.
    


%%========================================================================
%% update_state(NewPmJob, OldPmJob, Loop) -> Loop.
%% 
%% This function is called when the PM Job is updated.
%% 
%%========================================================================
%%---------------------------------------------------------------
%% stopped -> active
%%
%%---------------------------------------------------------------
update_state(#eventJob{eventJobId        = JobId,
		       requestedJobState = ?SessionState_ACTIVE}, 
	     #eventJob{eventJobId      = JobId,
		       currentJobState = ?SessionState_STOPPED},
	     Loop) ->
    Loop#loop{job_state = ?SessionState_ACTIVE};
%%---------------------------------------------------------------
%% active -> stopped
%%
%%---------------------------------------------------------------
update_state(#eventJob{eventJobId        = JobId,
		       requestedJobState = ?SessionState_STOPPED}, 
	     #eventJob{eventJobId      = JobId,
		       currentJobState = ?SessionState_ACTIVE},
	     #loop{job_id = JobId} = Loop) ->
    Loop#loop{job_state = ?SessionState_STOPPED};
%%---------------------------------------------------------------
%% Other cases
%%---------------------------------------------------------------
update_state(_, _, Loop) ->
    Loop.



%%========================================================================
%% send_job_request(State, EventJob) -> 
%% 
%% send job request to pesAppJob if job is active.
%%========================================================================
send_job_request(delete, JobId, Types, EventJob) ->
    Pids = lists:usort(sjr(Types, [])),
    [pesAppJob:job_request(JobPid, [], EventJob) || JobPid <- Pids],
    update_current_state(JobId, ?SessionState_STOPPED),
    ?SessionState_STOPPED;
send_job_request(?SessionState_STOPPED, JobId, Types, EventJob) ->
    Pids = lists:usort(sjr(Types, [])),
    [pesAppJob:job_request(JobPid, Types, EventJob) || JobPid <- Pids],
    update_current_state(JobId, ?SessionState_STOPPED),
    ?SessionState_STOPPED;
send_job_request(?SessionState_ACTIVE, JobId, Types, EventJob) ->
    Pids = lists:usort(sjr(Types, [])),
    [pesAppJob:job_request(JobPid, Types, EventJob) || JobPid <- Pids],
    case length(Pids) of
	0 ->
	    update_current_state(JobId, ?SessionState_ACTIVATING),
	    ?SessionState_ACTIVATING;
	_ -> 
	    update_current_state(JobId, ?SessionState_ACTIVE),
	    ?SessionState_ACTIVE
    end.


sjr([], Acc) ->
    lists:append(Acc);
sjr([Type | T], Acc) ->
    sjr(T, [sjr_get_pids(Type) | Acc]).


sjr_get_pids(Type) ->
    case pesDb:app_reg_get([Type]) of
	{ok, []} -> 
	    [];
	%% Find all records with any of the requested PmGroups
	{ok, Recs} ->
	    [JobPid || #pesAppRegType{job_pid = JobPid} <- Recs]
    end.


    


%%========================================================================
%% get_types(EventGrpRefs, EventTypeRefs)
%% 
%% 
%%========================================================================
get_types(EventGrpRefs, EventTypeRefs) ->
    EGR = choose(EventGrpRefs  == undefined, [], EventGrpRefs),
    ETR = choose(EventTypeRefs == undefined, [], EventTypeRefs),

    %% Split the LDNs to Event Group identities
    FunMatch  = fun(Class) -> [_, Val] = string:tokens(Class, "="), Val end,
    GrpTokens = [string:tokens(b2l(Ref), ",") || Ref <- EGR],
    GrpIds    = [l2t([FunMatch(T) || T <- GrpTok]) || GrpTok <- GrpTokens],

    %% Match the Event Types below the Event Groups
    GTMatch = [pesDb:event_type_match(G) || G <- GrpIds],
    GT      = f([G || {ok, G} <- GTMatch]),

    %% Select the type ids of all Event Group References
    Grps    = [T || #eventType{eventTypeId = {_, _, _, _, _, T}} <- GT],
    
    %% Select the type ids of the Event Type References
    FunType = fun([T | _]) -> T end,
    Types   = [FunType(r(string:tokens(b2l(Ref), ",="))) || Ref <- ETR],

    %% remove all possible duplicates
    lists:usort(Grps ++ Types).
    
						   
    



%%===========================================================================
%% Misc functions
%%===========================================================================

pp(Rec) when is_record(Rec, eventJob) ->
    [_ | Vals] = tuple_to_list(Rec),
    F = record_info(fields, eventJob),
    L = max_length(F, 0),
    lists:flatten([io_lib:format("    ~-*s = ~p~n", [L, K, V]) || 
	{K, V} <- lists:zip(F, Vals)]).



max_length([], L) ->
    L;
max_length([H|T], L) ->
    case length(atom_to_list(H)) of
	GR when GR > L -> max_length(T, GR);
	_              -> max_length(T, L)
    end.




update_current_state(JobId, NewState) ->
    case pesDb:event_job_get(JobId) of
	{ok, [Job]} ->
	    pesDb:event_job_set(Job#eventJob{currentJobState = NewState});
	{ok, []} ->
	    ok
    end.

log_msg(_, _) -> ok.
log_loop(_)   -> ok.


choose(true,  T, _) -> T;
choose(false, _, F) -> F.



%% remove all Groups without MT
%% filter_measured(_Measured) ->
%%     [].
%%    [M || {_, MT} = M <- Measured, length(MT) > 0].


%% ttl(T) when is_tuple(T) ->
%%     tuple_to_list(T);
%% ttl(L) when is_list(L) ->
%%     L.

%% ltt(L) when is_list(L) ->
%%     list_to_tuple(L);
%% ltt(T) when is_tuple(T) ->
%%     T.


format_loop(Loop) ->
    F       = record_info(fields, loop),
    [_ | L] = tuple_to_list(Loop), 
    lists:zip(F,L).
    

r(L) -> lists:reverse(L).
f(L) -> lists:flatten(L).

b2l(Bin) -> binary_to_list(Bin).
l2t(Bin) -> list_to_tuple(Bin).


%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



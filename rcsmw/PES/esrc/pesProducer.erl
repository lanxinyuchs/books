%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesProducer.erl %
%%% @private 	
%%% 	
%%% Description:
%%% 
%%% 
%%% 
%%% 
%%% 
%%% ----------------------------------------------------------
-module(pesProducer).
-vsn('/main/R3A/R5A/2').
-date('2016-04-28').
-author('uabesvi').
-shaid('a2bfbe43458cccf1a6b269e42a473d97228a796a').
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2014-11-07 uabesvi     Created
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([create/1]).
-export([delete/1]).
-export([terminate/2]).

-export([job_update/3]).
-export([job_delete/3]).

-export([get_current_state/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/2]).


-include("pes.hrl").
-include("RcsPMEventM.hrl").


-define(SEND_TO,   5000).
-define(CREATE_TO, 5000).

-define(ET, pesLib:epoch_time()).

-record(loop, {producer_id,  
	       event_jobs = [], %% [{JobId, {JobPid, MRef}}]
	       server
	      }).


%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%========================================================================
%% create(ProducerId, NewPmJob, OldPmJob) -> {ok, Pid} | {error, Error}
%%
%% Spawn a new Job Group process.
%%========================================================================
create(ProducerId) ->
    Self = self(),
    spawn(fun() -> init(Self, ProducerId) end),

    receive
	{pm_producer_id_res, Res, ProducerId} ->
	    Res
    after ?CREATE_TO ->
	    {error, {timeout, {?MODULE, create}}}
    end.

%%========================================================================
%% delete(Pid, PmJob) -> ok
%%
%% A PM Job is deleted.
%%========================================================================
delete(Pid) ->
    send(Pid, delete, []).

%%========================================================================
%% delete(Pid, PmJob) -> ok
%%
%% Pes Server is terminated. Terminate also this process, but do not
%% delete any table entries.
%%========================================================================
terminate(Pid, GrpId) ->
    send(Pid, terminate, GrpId).

%%========================================================================
%% job_update(Event) -> ok
%%
%% A PM Job is updated.
%%========================================================================
job_update(Pid, PmJob, Old) ->
    send(Pid, job_update, {PmJob, Old}).

%%========================================================================
%% job_delete(Pid, PmJob, Reason) -> ok
%% Reason = delete | move
%%
%% A PM Job is deleted.
%%========================================================================
job_delete(Pid, PmJob, Reason) ->
    send(Pid, job_delete, {PmJob, Reason}).


%%-------------------------------------------------------------
%% Send the message to the process and wait for reply
%%-------------------------------------------------------------
send(ProdId, Msg, Data) ->
    send2(whereis(?PRODUCER_NAME(ProdId)), ProdId, Msg, Data).

send2(undefined, ProdId, _, _) ->
    {error, {no_process, {?MODULE, ProdId}}};    
send2(Pid, _, Msg, Data) ->
    Pid ! {Msg, self(), Data},
    ok.

%%     receive
%% 	{Msg, res, Res} ->
%% 	    Res
%%     after ?SEND_TO ->
%% 	    {error, {timeout, {?MODULE, Msg}}}
%%     end.
    

%%========================================================================
%% 
%%
%% 
%%========================================================================
get_current_state(ProducerPid, JobId) ->
    ProducerPid ! {get_current_state, self(), JobId},
    receive
	{get_current_state, res, Res} ->
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
init(Pid, ProdId) ->
    init2(kill_old(whereis(?PRODUCER_NAME(ProdId))), Pid, ProdId).

init2(ok, Pid, ProdId) ->
    register(?PRODUCER_NAME(ProdId), self()),
    MRef = erlang:monitor(process, Pid),
    put(name, {?MODULE, ProdId}), %% used by pes_cli to print process data
    Pid ! {pm_producer_id_res, {ok, self()}, ProdId},
    ?LOG_RAM(?SEV_1, {"Started event producer = ~p~n", [ProdId]}),
    loop(#loop{producer_id    = ProdId, 
	       server         = {Pid, MRef}});
init2(Error, Pid, ProdId) ->
    Pid ! {pm_producer_id_res, Error, ProdId}.


kill_old(undefined) ->
    ok;
kill_old(Pid) ->
    Pid ! {terminate, self(), restarted},
    receive
	{terminate, res, Res} ->
	    Res
    after ?SEND_TO ->
	    {error, timeout}
    end.
    

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% loop(#loop{}) 
%%
%% Job loop. Loop until the job is deleted.
%%========================================================================
loop(#loop{producer_id  = ProducerId,
	   event_jobs   = PmJobs,
	   server       = {SrvPid, SrvRef}} = Loop) ->
    log_loop(Loop),
    receive
	%%-------------------------------------------------------------
	%% Delete this JobGroup process
	%%-------------------------------------------------------------
	{delete, UserPid, _} ->
 	    log_msg(delete, UserPid),
	    ?LOG_RAM(?SEV_1, "Deleted~n"),
	    exit(deleted);
	%%-------------------------------------------------------------
	%% Terminate this JobGroup process
	%%-------------------------------------------------------------
	{terminate = Msg, UserPid, _} ->
 	    log_msg(Msg, UserPid),
	    erlang:demonitor(SrvRef, [flush, info]),
	    UserPid ! {Msg, res, ok},
	    ?LOG_RAM(?SEV_1, {"Terminated pm_grp ~p ~p ~n", [ProducerId, Loop]}),
	    [terminate_job(JobId, JobPid, JobRef) || 
		{JobId, {JobPid, JobRef}} <- PmJobs],
	    exit(terminated);
	%%-------------------------------------------------------------
	%% New PmJob created
	%%-------------------------------------------------------------
 	{job_update = Msg, UserPid, {PmJob, []}} ->
	    ?LOG_RAM(?SEV_1, {"PmJob created  ~p~n", [element(2, PmJob)]}),
 	    log_msg(Msg, {UserPid, PmJob, []}),
	    {_Res, NewLoop} = pm_job_new(PmJob, Loop),
 	    loop(NewLoop);
	%%-------------------------------------------------------------
	%% Updated PmJob
	%%-------------------------------------------------------------
	{job_update = Msg, UserPid, {PmJob, Old}} ->
	    ?LOG_RAM(?SEV_1, {"PmJob updated  ~p~n", [element(2, PmJob)]}),
	    log_msg(Msg, {UserPid, PmJob, Old}),
	    {_Res, NewLoop} = pm_job_update(PmJob, Old, Loop),
	    loop(NewLoop);
	%%-------------------------------------------------------------
	%% Delete PmJob
	%%-------------------------------------------------------------
	{job_delete = Msg, _UserPid, {EventJob, Reason}} ->
	    ?LOG_RAM(?SEV_1, {"PmJob deleted  ~p~n", [element(2, EventJob)]}),
 	    log_msg(Msg, {EventJob, Reason}),
	    NewLoop = pm_job_delete_move(EventJob, Loop, Reason),
	    loop(NewLoop);
	%%-------------------------------------------------------------
	%% Get current job state
	%%-------------------------------------------------------------
	{get_current_state, Pid, JobId} ->
	    Res = handle_get_current_state(JobId, PmJobs),
	    Pid ! {get_current_state, res, Res},
	    loop(Loop);	    
	%%-------------------------------------------------------------
	%% Server died, let this process also die
	%%-------------------------------------------------------------
	{'DOWN', SrvRef, process, SrvPid, Reason} ->
	    ?LOG_RAM(?SEV_WARNING,
		     {"DOWN message from Server ~p~n", 
		      [{SrvRef, process, SrvPid, Reason}]}),
	    exit(server_down_msg);
	%%-------------------------------------------------------------
	%% Job process dies
	%%-------------------------------------------------------------
	{'DOWN', MRef, process, Pid, Reason} ->
	    ?LOG_RAM(?SEV_WARNING,
		     {"DOWN message ~p~n", 
		      [{MRef, process, Pid, Reason}]}),
	    loop(Loop);
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
		     {"++++ PmJobGroup LOOP UNKNOWN MSG ~p~n", [X]}),
	    loop(Loop)
    end.

	

%%========================================================================
%% pm_job_new(PmJob, Loop) -> Loop
%% 
%% A new PM Job is created.
%%========================================================================
pm_job_new(PmJob, Loop) ->
    pjn(pesJob:create(PmJob), PmJob, Loop).


pjn({ok, JobPid}, 
    #eventJob{eventJobId = JobId},
    #loop{event_jobs = EventJobs} = Loop) ->
    MRef = erlang:monitor(process, JobPid),
    {ok, Loop#loop{event_jobs = [{JobId, {JobPid, MRef}} | EventJobs]}};
pjn(Error, _PmJob, Loop) ->
    {Error, Loop}.


%%========================================================================
%% pm_job_update(PmJob, OldPmJob, Loop) -> Loop
%% 
%% An event Job is updated.
%%========================================================================
pm_job_update(#eventJob{eventJobId = JobId} = PmJob, 
	      [#eventJob{}] = Old, 
	      #loop{event_jobs = EventJobs} = Loop) ->

    {pju(proplists:get_value(JobId, EventJobs), PmJob, Old),
     Loop}.


%%-------------------------------------------------------------
%% Updated PM Job. GP not changed. 
%%-------------------------------------------------------------
pju({Pid, _}, PmJob, Old) ->
    pesJob:update(Pid, PmJob, Old).

   


%%========================================================================
%% pm_job_delete_move(PmJob, Loop, Reason) -> Loop
%% 
%% A PM Job is deleted or moved.
%%========================================================================
pm_job_delete_move(#eventJob{eventJobId = JobId} = PmJob,
		   #loop{event_jobs = EventJobs} = Loop,
		   _Reason) ->
    pjd(proplists:get_value(JobId, EventJobs), PmJob),
    NewEventJobs = proplists:delete(JobId, EventJobs),
    Loop#loop{event_jobs = NewEventJobs}.

pjd({Pid, MRef}, PmJob) ->
    erlang:demonitor(MRef),
    pesJob:delete(Pid, PmJob).





%% %%===========================================================================
%% %% Misc functions
%% %%===========================================================================
log_msg(_, _) -> ok.


log_loop(_)   -> ok.




handle_get_current_state(JobId, PmJobs) ->
    hgcs(proplists:get_value(JobId, PmJobs)).

hgcs({Pid, _}) ->
    pesJob:get_current_state(Pid);
hgcs(_X) ->
    undefined.



%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

terminate_job(JobId, Pid, Ref) ->
    erlang:demonitor(Ref, [flush, info]),
    pesJob:terminate(Pid, JobId).


format_loop(Loop) ->
    F       = record_info(fields, loop),
    [_ | L] = tuple_to_list(Loop), 
    lists:zip(F,L).
    

%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesAppJob.erl %
%%% @private
%%% Description:
%%%
%%% pesAppJob initializes the measurement data collection 
%%% from the applications by sending the report message.
%%% There will be one report message request for each granularity period. 
%%% 
%%% To send the messages pesAppJob uses a pesSession process.
%%% A pesSession process is started when an application attaches.
%%% The pesSession process contacts pesAppRegistry and informs which
%%% counters it handles. pesAppRegistry spawns a pesAppJob process
%%% and stores the counter data and the 2 pids in a table.
%%% 
%%% The pesJob searches that table to find all applications handling
%%% the requested counters to send the subscribe messages.
%%% 
%%% pesAppJob will calculate a union of all counters in all subsrcibe 
%%% requests and send a new subscribe request to the application as soon as
%%% there are any changes. 
%%% ----------------------------------------------------------
-module(pesAppJob).
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-04-05').
-author('uabesvi').
-shaid('73cf5c3e1fdd44096c461abc9335a370cb53b302').
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
%%% -----------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% -----------------------------------------------------------------------
%%% Rev        Date          Name        What
%%% -----      ----------    --------    ----------------------------------
%%% R1A/1      2013-01-18    uabesvi     Created
%%% R1A/4      2013-02-01    uabesvi     added subsribe and report
%%% R4A/1      2015-09-08    uabesvi     error_logger -> sysInitI
%%% -----------------------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start/4]).
-export([stop/1]).
-export([job_request/3]).
-export([job_update/2]).
-export([gld/2]).
-export([me_attr_update/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/5]).

%%-compile(export_all).

-include("pes.hrl").
-include("RcsPMEventM.hrl").


-define(START_TO,    10000).
-define(ET, pesLib:epoch_time()).


-record(loop, {types        = [],  %% [{eventTypeId, Alias}]
	       pei_cb_mod,
	       pei_event_job_cb,
	       pei_me_update_cb,
	       session_pid,
	       last_job}).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% start(PmGroups, Callbacks, PeiCb, SessionPid) -> {ok, Pid} | {error, Reason}
%%
%% Spawn a new Job process, init the connection to the application
%% by creating an start job object in IMM.
%%
%% The application should then send a register message with its
%% cb module. 
%%
%% The Job will then send the counter information to the application,
%% including what counters are to be monitored. Also the granularity
%% period is sent.
%%
%% It is the responsibility of the application to start to send the
%% counters by calling 'counters'-function.
%%========================================================================
start(Types, Callbacks, PeiCb, SessionPid) ->
    Self = self(),
    spawn(fun() -> init(Self, Types, Callbacks, PeiCb, SessionPid) end),

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
%% subscribe(Pid, GP, CounterSpec) -> ok
%%
%% pesJob sends a subscribe request. If the request changes the union
%% of all counters in all subscribe requests then a new subscribe message
%% is sent to the application. Note, that the union is counted per 
%% granularity period.
%%========================================================================
job_request(AppJobPid, Types, EventJob) ->
    AppJobPid ! {job_request, self(), Types, EventJob},
    ok.
    

%%========================================================================
%% subscribe(Pid, GP, CounterSpec) -> ok
%%
%% pesJob sends a subscribe request. If the request changes the union
%% of all counters in all subscribe requests then a new subscribe message
%% is sent to the application. Note, that the union is counted per 
%% granularity period.
%%========================================================================
job_update(AppJobPid, Types) ->
    AppJobPid ! {job_update, self(), Types},
    ok.
    
%%========================================================================
%% me_attr_update(AppJob, MEData) -> ok
%%
%% Notify application about ME attribute update
%%========================================================================
me_attr_update(AppJobPid, MEData) ->
    AppJobPid ! {me_attr_update, MEData}.
    

%%========================================================================
%% Debug functions
%%========================================================================
gld(Pid, UserPid) ->
    Pid ! {get_loop_data, UserPid}.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% Init function for the Job process
%%========================================================================
init(UserPid, Types, Callbacks, PeiCb, SessionPid) ->
    UserPid ! {pm_app_job_init_res, self()},
    ?LOG_RAM(?SEV_1,
	     {"Started~n"
	      "  Session = ~p~n"
	      "  Types   = ~p~n"
	      "  CB      = ~p~n", 
	      [SessionPid, Types, PeiCb]}),
    EvJobCb = proplists:get_value(peiEventJobCallback, Callbacks, false),
    MEUpdCb = proplists:get_value(peiMEAttrUpdateCallback, Callbacks, false),
    send_initial_me_attr(MEUpdCb, PeiCb, SessionPid),
    loop(#loop{types       = Types,
	       pei_cb_mod  = PeiCb,
	       pei_event_job_cb = EvJobCb,
	       pei_me_update_cb = MEUpdCb,
	       session_pid = SessionPid}).


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% loop(JobId, TimeRef) 
%%
%% Loop until the session is deleted.
%%========================================================================
loop(#loop{session_pid  = SesPid} = Loop) ->

    log_loop(Loop),
    receive
	%%-------------------------------------------------------------
	%% New job created
	%%-------------------------------------------------------------
	{job_request, _EventJobPid, JobTypes, EventJob} ->
	    %% filter the non valid pmGroups from the counter spec,
	    %% i.e. counters not handled by the application
	    %% 	    CS = [Spec || 
	    %% 		     {Grp, _} = Spec <- 
	    %% 			 CounterSpec, 
	    %% 		     lists:member(Grp, PmGroups)],
 	    log_msg(job_request, {JobTypes, EventJob}),
	    NewLoop = send_job_request(JobTypes,
				       EventJob,
				       Loop), 
 	    loop(NewLoop);

	%%-------------------------------------------------------------
	%% Job updated
	%%-------------------------------------------------------------
	{job_update, _EventJobPid, JobTypes} ->
	    %% filter the non valid pmGroups from the counter spec,
	    %% i.e. counters not handled by the application
	    %% 	    CS = [Spec || 
	    %% 		     {Grp, _} = Spec <- 
	    %% 			 CounterSpec, 
	    %% 		     lists:member(Grp, PmGroups)],
 	    log_msg(job_update, {JobTypes}),
	    NewLoop = update_job_request(JobTypes, Loop), 
 	    loop(NewLoop);

	%%-------------------------------------------------------------
	%% ME Attribute Update
	%%-------------------------------------------------------------
	{me_attr_update, MEData} ->
	    MEUpdCb = Loop#loop.pei_me_update_cb,
	    PeiCb = Loop#loop.pei_cb_mod,
	    ?LOG_RAM(?SEV_1, {"Got ME Attr Update ~p~n", [MEData]}),
 	    log_msg(me_attr_update, MEData),
	    send_me_attr_update(MEUpdCb, PeiCb, MEData, SesPid),
 	    loop(Loop);

	%%-------------------------------------------------------------
	%% Stop from pesAppRegistry. Kill the process.
	%%-------------------------------------------------------------
	stop ->
	    ?LOG_RAM(?SEV_1, {"Stopped~n  Session = ~p~n", [SesPid]}),
	    exit(normal);

	%%-------------------------------------------------------------
	%% Read loop data
	%%-------------------------------------------------------------
	{get_loop_data, UserPid} ->
	    UserPid ! {loop_data, format_loop(Loop)},
	    loop(Loop);
	%%-------------------------------------------------------------
	%% Unknown message
	%%-------------------------------------------------------------
	UnknownMsg ->
	    ?LOG_RAM(?SEV_WARNING, {"Received unknown msg: ~p~n", [UnknownMsg]}),
	    loop(Loop)
    end.



%%========================================================================
%% send_subscribe_request(JobPid, GP, CounterSpec, Loop) -> Res
%% 
%% Send subscribe request to the application.
%% Do not send anything if the new request does not modify
%% the previously sent CounterSpec.
%%========================================================================
%% Delete job
send_job_request(_JobTypes, _Job, Loop)
  when Loop#loop.pei_event_job_cb =:= false ->
    Loop;

send_job_request(JobTypes, 
		 #eventJob{eventJobId = {_, _, _, ProdId, JobId},
			   eventFilter                = Filters,
			   requestedJobState          = ReqJobState,
			   fileOutputEnabled          = FileEnabled,
			   streamOutputEnabled        = StreamEnabled,
			   reportingPeriod            = RP,
			   fileCompressionType        = FileCompr,
			   streamCompressionType      = StreamCompr,
			   streamDestinationIpAddress = IpAddr,
			   streamDestinationPort      = Port
			  },
		 #loop{types       = Types,
		       pei_cb_mod  = PeiCb,
		       session_pid = SesPid,
		       last_job    = LastJob
		      } = Loop) ->

    TypesF  = type_aliases(JobTypes, Types, []),
    FilterF = filters(JobTypes, Filters, []),
    FileF   = fileCtrl(JobTypes, FileEnabled, RP, FileCompr),
    StreamF = streamCtrl(JobTypes, StreamEnabled, StreamCompr, IpAddr, Port),
    sjr(LastJob, 
	{SesPid, ProdId, JobId, ReqJobState, TypesF, FilterF, FileF, StreamF},
        PeiCb),
    Loop#loop{last_job = {SesPid, ProdId, JobId, ReqJobState, 
			  TypesF, FilterF, FileF, StreamF}}.



%% Check if callback has to be sent.
%% Do not send if the previously sent cb contained
%% the same info as this one
sjr(LastJob, LastJob, _) ->
    ok;
sjr(_LastJob, 
    {SesPid, ProdId, JobId, ReqJobState, TypesF, FilterF, FileF, StreamF},
    PeiCb) ->
    ?LOG_RAM(?SEV_1, 
	     {" ===> EVENT JOB ~n"
	      "  Session  = ~p~n"
	      "  Producer = ~p~n"
	      "  Job      = ~p~n"
	      "  ReqState = ~p~n"
	      "  Types    = ~p~n"
	      "  Filters  = ~p~n"
	      "  File     = ~p~n"
	      "  Stream   = ~p~n",
	      [SesPid, ProdId, JobId, ReqJobState, 
	       TypesF, FilterF, FileF, StreamF]}),

    PeiCb:peiEventJobCallback(SesPid, 
			      ProdId, 
			      JobId,
			      ReqJobState,
			      TypesF, 
			      FilterF, 
			      FileF, 
			      StreamF).
    




update_job_request(JobTypes, 
		   #loop{pei_cb_mod  = PeiCb,
			 session_pid = SesPid,
			 last_job    = {SesPid, ProdId, JobId, ReqJobState, 
				       Types, Filter, File, Stream} 
			               = LastJob
			} = Loop) ->
    

    TypesF  = type_aliases(JobTypes, Types, []),
    sjr(LastJob, 
	{SesPid, ProdId, JobId, ReqJobState, TypesF, Filter, File, Stream},
        PeiCb),
    Loop#loop{last_job = {SesPid, ProdId, JobId, ReqJobState, 
			  TypesF, Filter, File, Stream}}.









filters([], _, _) ->
    [];
filters(_, undefined, _) ->
    [];
filters(_, Filters, Acc) ->
    filters(Filters, Acc).

filters([], Acc) ->
    Acc;
filters([#'EventFilter'{filterName = Name, filterValue = Value} | T], Acc) ->
    filters(T, [{Name, Value} | Acc]).


fileCtrl([], _, _, _) ->
    undefined;
fileCtrl(_, true, RP, FileCompr) ->
    {pesLib:get_interval(RP), FileCompr};
fileCtrl(_, _, _, _) ->
    undefined.

streamCtrl([], _, _, _, _) ->
    undefined;
streamCtrl(_, true, StreamCompr, IpAddr, Port) ->
    {StreamCompr, IpAddr, Port};
streamCtrl(_, _, _, _, _) ->
    undefined.

    
type_aliases([], _Types, Acc) ->
    lists:sort(Acc);
type_aliases([H | T], Types, Acc) ->
    case proplists:get_value(H, Types) of
	Alias when is_integer(Alias) ->
	    type_aliases(T, Types, [Alias | Acc]);
	_ ->
	    ?LOG_RAM(?SEV_WARNING, {"No alias found for Event Type = ~p~n", [H]}),
	    sysInitI:warning_msg("No alias found for Event Type = ~p~n", [H]), 
	    type_aliases(T, Types, Acc)
    end.
    


send_initial_me_attr(true, PeiCb, SessionPid) ->
    MEData = pesLib:get_me_data(),
    do_send_me_attr_update(PeiCb, MEData, SessionPid);

send_initial_me_attr(_, _PeiCb, _SessionPid) ->
    ok.


send_me_attr_update(true, PeiCb, MEData, SessionPid) ->
    do_send_me_attr_update(PeiCb, MEData, SessionPid);

send_me_attr_update(_, _PeiCb, _MEData, _SessionPid) ->
    ok.


do_send_me_attr_update(PeiCb, {UserLabel, NeMEId}, SessionPid) ->
    ?LOG_RAM(?SEV_1, 
	     {" ===> ME ATTR UPDATE~n"
	      "  Session                 = ~p~n"
	      "  UserLabel               = ~p~n"
	      "  NetworkManagedElementId = ~p~n",
	      [SessionPid, UserLabel, NeMEId]}),
    PeiCb:peiMEAttrUpdateCallback(SessionPid, UserLabel, NeMEId).


%%===========================================================================
%% Misc functions
%%===========================================================================
log_msg(_, _) -> ok.
log_loop(_)   -> ok.



format_loop(Loop) ->
    F       = record_info(fields, loop),
    [_ | L] = tuple_to_list(Loop), 
    lists:zip(F,L).
    
%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



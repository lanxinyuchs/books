%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesServer.erl %
%%% 
%%% Description:     
%%%
%%% pesServer is a gen_server process and is the heart of PES process 
%%% handling of pm jobs. 
%%% It subscribes to mnesia events to check when event jobs are
%%% created/changed/deleted. It starts pesProducer processes
%%% when needed. The pesProducer will in its turn start a pesJob
%%% processes when needed.
%%% 
%%% ----------------------------------------------------------
-module(pesServer).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/R6A/R8A/3').
-date('2017-01-09').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R3A/1      2014-11-06 uabesvi     Created
%%% R3A/13     2015-01-20 erarafo     Separation of PMS and PES SFTP services
%%% R4A/3      2015-09-08 uabesvi     error_logger -> sysInitI
%%% R5A/1      2016-02-29 eolaand     Moved SFTP registration to pesDataInit
%%% R8A/1      2016-16-11 etomist     HV40718
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([get_current_state/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2]).

-export([restartEventJobs/0]). %%HV23638

-include("pes.hrl").
-include("RcsPMEventM.hrl").


-record(state, 
	{prod_ids    = [], %% [{ProdId, ProdPid}]
	 actions     = [], %% {TransId, ActionId, Actions}
	 rem_actions = [], %% {TransId, ActionId, RemainingActions}
	 me_data           %% {UserLabel, NetworkManagedElementId}
	}).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% start() -> ok.
%% 
%%========================================================================
start() ->
    Server  = {local, ?MODULE},
    Module  = ?MODULE,
    Args    = [],
    Options = [],
    gen_server:start_link(Server, Module, Args, Options).

%%========================================================================
%% get_current_state(JobId) -> ok.
%% 
%%========================================================================
get_current_state(JobId) ->
    gen_server:call(?MODULE, {get_current_state, JobId}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% init(_) -> {ok, State}
%%========================================================================
init(_) ->
    Options = [{maxNoFiles, 10}, 
	       {size,       500},
	       {header,     {pmsDebug, get_versions, [pes, ?PES_MODS]}},
	       {zip,        true},
	       {local,      true}],
    logRamI:create_log(?PM_EVENT_LOG, Options),
    init_active(sysEnv:role()).


init_active(active) ->
    process_flag(trap_exit, true), %% to receive terminate at shutdown
    put(name, {?MODULE, ?MODULE}), %% used by pes_cli to print process data
    pesDb:mnesia_subscribe({table, managedElement, detailed}),
    pesDb:mnesia_subscribe({table, eventJob,       detailed}),
    pesDb:mnesia_subscribe({table, eventProducer,  detailed}),
    %% sysFi:register_sftp_dir(get_output_dirs()),


    ?LOG_RAM(?SEV_1, "Server process started~n"),
    {ok, init_check(#state{me_data = pesLib:get_me_data()})};
    
init_active(_Regular) ->
    ignore.


%%-------------------------------------------------------------
%% Fetch output directories to register in sysFi
%%-------------------------------------------------------------
%% get_output_dirs() ->
%%     ProdTmp = mnesia:dirty_match_object(#pesEnv{key = {'_', app_tmp_dir}, 
%% 						value = '_'}),
%%     RegDirs = 
%% 	lists:flatmap(fun(#pesEnv{key = {ProdId, _}, value = TmpDir}) ->
%% 			      {ok, FPC} = pesDb:file_pull_cap_get(ProdId),
%% 			      [{TmpDir, 
%% 				Rec#filePullCapabilities.outputDirectory, 
%% 				ram, undefined} 
%% 			       || Rec <- FPC]
%% 		      end, ProdTmp),
%%     lists:usort(RegDirs).


%%-------------------------------------------------------------
%% Check if there is already created jobs
%%-------------------------------------------------------------
init_check(State) ->
    Prods = ic_producers(ets:tab2list(eventProducer), []),
    ic_jobs(ets:tab2list(eventJob), State#state{prod_ids = Prods}).

ic_jobs([], State) ->
    State;
ic_jobs([Job | T], State) -> 
    {ok, NewState} = create_job(Job, [], State),
    ic_jobs(T, NewState).


ic_producers([], Acc) ->
    Acc;
ic_producers([#eventProducer{eventProducerId = Id} = H | T], Acc) -> 
    {ok, Pid} = create_producer(H),
    ic_producers(T, [{Id, Pid} | Acc]).


%%========================================================================
%% handle_call(Msg, From, State) -> {reply, Res, State}
%%========================================================================
handle_call({prepare_transaction, TransId, ActionId, Objects}, 
	    _From, 
	    #state{actions = Actions} = State) ->
    case prepare_transaction(lists:keymember(TransId, 1, Actions),
			     TransId, 
			     ActionId, 
			     Objects, 
			     State) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({abort_transaction, TransId},
	    _From,
	    #state{rem_actions = RemAct,
		   actions     = Actions} = State) ->
    RemRemAct  = lists:keydelete(TransId, 1, RemAct),
    RemActions = lists:keydelete(TransId, 1, Actions),
    {reply, ok, State#state{rem_actions = RemRemAct,
			    actions     = RemActions}};
handle_call(get_loop_data, _From, State) ->
    {reply, format_state(State), State};
handle_call({get_current_state, JobId}, 
	    _From, 
	    #state{prod_ids = Gids} = State) ->
    CS = get_current_state(JobId, Gids),
    {reply, CS, State};
handle_call(_Msg, _From, State) ->
    ?LOG_RAM(?SEV_WARNING, {"PES SERVER: call ~p~n", [_Msg]}),
    {reply, ok, State}.


%%========================================================================
%% handle_cast(Msg, State) -> {noreply, State}
%%========================================================================
handle_cast({test_terminate, Reason}, State) ->
    ?LOG_RAM(?SEV_1, {"cast ~p~n", [{test_terminate, Reason}]}),
    {stop, Reason, State};
handle_cast(_Msg, State) ->
    ?LOG_RAM(?SEV_WARNING, {"PES SERVER: cast ~p~n", [_Msg]}),
    {noreply, State}.

%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
handle_info({mnesia_table_event, Event},
	    State) ->
    case check_event(Event, State) of
	{ok, NewState} ->
	    {noreply, NewState};
	Error ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {"ERROR MNESIA EVENT ~n~p~n~p~n", [Error, Event]}),
	    {noreply, State}
    end;
handle_info({'DOWN', MRef, process, Pid, normal}, State) ->
    {noreply, handle_monitor(MRef, Pid, State)};
handle_info({'DOWN', MRef, process, Pid, Reason}, State) ->
    sysInitI:info_msg("EventProducer ~p terminated unexpectedly: ~p", 
		      [Pid, Reason]),
    {noreply, handle_monitor(MRef, Pid, State)};
handle_info(_Info, State) ->
    ?LOG_RAM(?SEV_WARNING, {"info ~p~n", [_Info]}),
    {noreply, State}.

%%========================================================================
%% code_change(OldVsn, State, Extra) -> {ok, State}
%%========================================================================
code_change(OldVsn, State, _Extra) ->
    ?LOG_RAM(?SEV_1, {"code change ~p~n", [OldVsn]}),
    {ok, State}.

%%========================================================================
%% terminate(Reason, State) -> Reason
%%========================================================================
terminate(Reason, #state{prod_ids = Gids} = State) ->
    ?LOG_RAM(?SEV_1, {"terminated ~p ~p~n", [Reason, State]}),
    [terminate_job_grp(Pid, Gid, Ref) || {{Gid, _}, {Pid, Ref, _}} <- Gids],
    Reason.


terminate_job_grp(Pid, Gid, Ref) ->
    erlang:demonitor(Ref, [flush, info]),
    pesProducer:terminate(Pid, Gid).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% check_event(Event, State) -> {ok, State} | {error, Reason}
%% 
%%========================================================================
check_event(Event, State) ->
    ce_rc(ce(Event, State)).


ce_rc({ok, State}) ->
    {ok, trans_end(State)};
ce_rc(Error) ->
    Error.



ce({write, eventJob, New, Old, Aid}, State) ->
    {ok, ce_write(Aid, New, Old, State)};
ce({delete, eventJob, EventJobId, Old, {dirty, _}}, State) ->
    %% This is a probably a dirty delete from a test suite.
    ?LOG_RAM(?SEV_WARNING, 
	     {"Unexpected delete of ~p ~p~n", [EventJobId, Old]}),
    sysInitI:info_msg("Unexpected dirty delete of EventJob: ~p",
		      [EventJobId]),
    {ok, _NewState} = delete_job(Old, delete, State);
ce({delete, eventJob, EventJobId, Old, Aid}, State) ->
    {ok, ce_delete(Aid, EventJobId, Old, State)};
ce({write, managedElement, _New, _Old, _Aid}, State) ->
    {ok, me_notify(State)};
ce(Event, _State) ->
    ?LOG_RAM(?SEV_WARNING, {"UNKNOWN MNESIA EVENT ~p~n", [Event]}),
    {error, {unknown_event, Event}}.


ce_write(Aid,
	 New, 
	 Old,
	 #state{rem_actions = RemAct,
		actions     = Actions} = State) ->
    cew(lists:keytake(Aid, 2, RemAct),
	lists:keytake(Aid, 2, Actions),
	Aid,
	New, 
	Old,
	State).



cew({value, {Tid, _, TidRemAct}, RemRemAct},
    {value, {Tid, _, TidActions}, RemActions},
    Aid,
    New, 
    Old,
    State) ->
    NewTidRemAct  = TidRemAct -- [{created, New}],
    NewTidActions = [{created, New, Old} | TidActions], 
    State#state{rem_actions = [{Tid, Aid, NewTidRemAct}  | RemRemAct],
		actions     = [{Tid, Aid, NewTidActions} | RemActions]};
%% This clause is for the eventJob current state updates from pesJob
cew(_, _, _, _, _, State) ->
    State.


ce_delete(Aid,
	  {eventJob, EventJobId}, 
	  Old,
	  #state{rem_actions = RemAct,
		 actions     = Actions} = State) ->
    {value, {Tid, _, TidRemAct},  RemRemAct}  = lists:keytake(Aid, 2, RemAct),
    {value, {Tid,  _, TidActions}, RemActions} = lists:keytake(Aid, 2, Actions),
 
    NewTidRemAct  = TidRemAct -- [{deleted, EventJobId}],
    NewTidActions = [{deleted, Old} | TidActions], 

    State#state{rem_actions = [{Tid, Aid, NewTidRemAct}  | RemRemAct],
		actions     = [{Tid, Aid, NewTidActions} | RemActions]}.


%%========================================================================
%% trans_end
%% 
%% All transaction actions done.
%% Create and delete all objects included in the transaction
%% 
%%========================================================================
trans_end(#state{rem_actions = RemAct} = State) ->
    te([Aid || {_, Aid, []} <- RemAct], State).


te([],
   State) ->
    State;
te([Aid],
   #state{rem_actions = RemAct,
	  actions     = Actions} = State) ->
    {value, {_, _, []},         RemRemAct}  = lists:keytake(Aid, 2, RemAct),
    {value, {_, _, TidActions}, RemActions} = lists:keytake(Aid, 2, Actions),

    update_pes_job_groups(TidActions, 
			  State#state{rem_actions = RemRemAct,
				      actions     = RemActions}).

update_pes_job_groups([], State) ->
    State;
update_pes_job_groups([{created, Job, Old} | T], State)
  when is_record(Job, eventJob) ->
    {ok, NewState} = create_job(Job, Old, State),
    update_pes_job_groups(T, NewState);
update_pes_job_groups([{deleted, Old} | T], State) ->
    {ok, NewState} = delete_job(Old, delete, State),
    update_pes_job_groups(T, NewState).


%%========================================================================
%% create_job(EventJob, Old, State) -> {ok, State} | {error, Reason}
%% 
%% New eventJob instance created.
%% If no EventJob has previously had the same GroupId create a new
%% pesJobGroup process.
%% 
%% 
%% 
%%========================================================================
create_job(#eventJob{eventJobId = {_, _, _, Prod, _},
		     jobControl = JobCtrl} = EventJob,
	   Old,
	   State) ->
    JC = choose(JobCtrl == undefined, ?JobControl_FULL, JobCtrl),
    EJ = EventJob#eventJob{jobControl = JC},
    pesDb:event_job_set(EJ),
    pesProducer:job_update(Prod, EJ, Old), 
    {ok, State}.


%%========================================================================
%% delete_job([EventJob], Reason, State) -> {ok, State} | {error, Reason}
%% 
%% eventJob instance deleted.
%% 
%%========================================================================
delete_job([], _, State) ->
    {ok, State};
delete_job([#eventJob{eventJobId = {_, _, _, Prod, _}} = EventJob | Rem],
	   Reason,
	   State) ->
    pesProducer:job_delete(Prod, EventJob, Reason), 
    delete_job(Rem, Reason, State).



%%========================================================================
%% create_producer(Producer) -> ok | {error, Reason}
%% 
%% New eventProducer instance created.
%%========================================================================
create_producer(#eventProducer{eventProducerId = {_, _, _, Eid}}) ->
    case whereis(?PRODUCER_NAME(Eid)) of
	undefined -> pesProducer:create(Eid);
	Pid       -> {ok, Pid}
    end.



%%========================================================================
%% prepare_transaction
%% 
%%========================================================================
prepare_transaction(true, 
		    TransId, 
		    ActionId, 
		    Objects,
		    #state{rem_actions = RemAct,
			   actions     = Actions} = State) ->
    RmRemAct  = lists:keydelete(TransId, 1, RemAct),
    RmActions = lists:keydelete(TransId, 1, Actions),
    prepare_transaction(false, 
			TransId, 
			ActionId, 
			Objects,
			State#state{rem_actions = RmRemAct,
				    actions     = RmActions});
prepare_transaction(false,
		    TransId, 
		    ActionId, 
		    Objects,
		    #state{rem_actions = RemAct,
			   actions     = Actions} = State) ->


    FilterObjects = [Object || {Action, Obj} = Object <- Objects,
			       Action == deleted orelse 
				   is_record(Obj, eventJob)],
    
    case check_prep_trans(FilterObjects, State) of
	{ok, NewState} ->
	    NewRemActions = [{TransId, ActionId, FilterObjects} | RemAct],
	    NewActions    = [{TransId, ActionId, []} | Actions],
	    {ok, 
	     NewState#state{rem_actions = NewRemActions,
			    actions     = NewActions}};
	Error ->
	    Error
    end.





%%========================================================================
%% check_prep_trans([Object], State) -> {ok, State} | {error, Reason}
%% 
%% New transaction is ongoing.
%% Check that PES can accept it.
%% 
%%========================================================================
check_prep_trans(Objects, State) ->
    CreJobs = [C || {created, C} <- Objects],
    DelJobs = [D || {deleted, D} <- Objects],

    cpt(cpt_deleted(DelJobs), Objects, State, CreJobs, DelJobs).


cpt(ok, Objects, State, CreJobs, DelJobs) ->
    cpt_rc([cpt_update(Objects),
	    cpr_rp(pesDb:pes_env_get(reporting_period), CreJobs),
	    cpt_jobs(CreJobs, DelJobs, get_currently_active_jobs()),
	    cpt_file(CreJobs),
	    cpt_stream(CreJobs)],
	   State);
cpt(Error, _, _, _, _) ->
    Error.



%%----------------------------------------------------------------------
%% check_prep_trans result
%%----------------------------------------------------------------------
cpt_rc(Results, State) ->
    case [E || {error, _} = E <- Results] of 
	[]          -> {ok, State};
	[Error | _] -> Error
    end.



cpt_deleted([]) ->
    ok;
cpt_deleted([Job | T]) ->
    case cpt_del(pesDb:event_job_dirty_get(Job)) of
	ok    -> cpt_deleted(T);
	Error -> Error
    end.


cpt_del({ok, [#eventJob{currentJobState = ?SessionState_ACTIVE}]}) ->
    {error, "Not allowed to delete job in state active"};
cpt_del({ok, [#eventJob{jobControl = JobCtrl}]}) 
    when JobCtrl == ?JobControl_FULL;
	 JobCtrl == undefined ->
    ok;
cpt_del(_) ->
    {error, "Not allowed to delete a pre-defined EventJob"}.




cpt_update([]) ->
    ok;
cpt_update([{created, #eventJob{eventJobId = {ME, SF, PM, Pr, J} = JobId}
	     = Rec} | T]) ->
    Error = "{" ++
	ME ++ "," ++
	SF ++ "," ++
	PM ++ "," ++
	Pr ++ "," ++
	J  ++ "}:  ",
    case cpt_update_job(Rec, mnesia:dirty_read(eventJob, JobId)) of
	ok        -> cpt_update(T);
	{no, Txt} -> {error, list_to_binary(Error ++ Txt)}
    end;
cpt_update([{deleted, _} | T]) ->
    cpt_update(T).


%%--------------------------------------------------------------
%% New job
%%--------------------------------------------------------------
cpt_update_job(_, []) ->
    ok;
%%--------------------------------------------------------------
%% Not allowed to update a job if requested and current states
%% are not stopped
%%--------------------------------------------------------------
cpt_update_job(#eventJob{requestedJobState = ReqState,
			 currentJobState   = CurrState}, 
	       [#eventJob{}])
  when ReqState  /= ?SessionState_STOPPED andalso 
       CurrState /= ?SessionState_STOPPED  ->
    {no, "Not allowed to update an event job that is not in state stopped"};
%%--------------------------------------------------------------
%% Not allowed to update job control
%%--------------------------------------------------------------
cpt_update_job(#eventJob{jobControl = NewJobCtrl}, 
	       [#eventJob{jobControl = OldJobCtrl}])
  when NewJobCtrl /= OldJobCtrl ->
    {no, "Not allowed to update jobControl for an EventJob"};
%%--------------------------------------------------------------
%% The same record, it is propably reloading the configuration
%%--------------------------------------------------------------
cpt_update_job(Old, [Old]) ->
    ok;
%%--------------------------------------------------------------
%% Not allowed to modify a view only predefined job
%%--------------------------------------------------------------
cpt_update_job(_, [#eventJob{jobControl = ?JobControl_VIEWONLY}]) ->
    {no, "Not allowed to update a view only predefined EventJob"};
%%--------------------------------------------------------------
%% start stop predefined job, allow only state change
%%--------------------------------------------------------------
cpt_update_job(New, [#eventJob{jobControl = ?JobControl_STARTSTOP} = Old]) ->
    Res = New#eventJob{requestedJobState = na,
		       currentJobState   = na} /=
	Old#eventJob{requestedJobState = na,
		     currentJobState   = na},
    case Res of
	true  -> ok;
	false -> {no, "Not allowed to update a start stop predefined EventJob"}
    end;
%%--------------------------------------------------------------
%% Operator created job
%%--------------------------------------------------------------
cpt_update_job(_, [_]) ->
    ok.


cpr_rp(legacy, 
       [#eventJob{reportingPeriod = ?TimePeriod_FIFTEEN_MIN, fileOutputEnabled = true} | T]) ->
    cpr_rp(legacy, T);
cpr_rp(legacy, 
       [#eventJob{fileOutputEnabled = false} | T]) ->
    cpr_rp(legacy, T);
cpr_rp(legacy, [#eventJob{} | _]) ->
    {error, <<"Invalid reporting period.">>};
cpr_rp(legacy, [_ | T]) ->
    cpr_rp(legacy, T);
cpr_rp(_, _) ->
    ok.
  


%%----------------------------------------------------------------------
%% Count number of jobs
%%----------------------------------------------------------------------
    

cpt_jobs(CreJobs, DelJobs, ActJobs) ->
    AllJobs = 
	[{Prod, 1} || #eventJob{eventJobId = {_, _, _, Prod, _},
			        requestedJobState = ?JobState_ACTIVE} 
			  <- lists:append(CreJobs, ActJobs)] ++
	[{Prod, -1} || #eventJob{eventJobId = {_, _, _, Prod, _}} 
			   <- DelJobs],
    
    Noof = cpt_jobs_noof(proplists:get_keys(AllJobs), AllJobs, []),
    cpt_check(Noof).
    

cpt_jobs_noof([], _AllJobs, Acc) ->
    Acc;
cpt_jobs_noof([P | T], AllJobs, Acc) ->
    Prod = {P, lists:sum(proplists:append_values(P, AllJobs))},
    cpt_jobs_noof(T, AllJobs, [Prod | Acc]).
   




cpt_check([]) ->
    ok;
cpt_check([{Prod, Noof} | T]) -> 
    case Noof > cpt_get_max_jobs(Prod) of
	true ->
	    Txt = "Max number of jobs already started for Producer = " ++ Prod,
	    {error, list_to_binary(Txt)};
	false ->
	    cpt_check(T)
    end.

cpt_get_max_jobs(Prod) ->
    case pesDb:event_cap_get(Prod) of
	{ok, [#eventCapabilities{maxNoOfJobs = Max}]} ->
	    Max;
	_ ->
	    undefined
    end.



cpt_file([]) ->
    ok;
cpt_file([#eventJob{fileOutputEnabled = true,
		    reportingPeriod   = undefined} | _]) ->
    {error, "No ReportingPeriod defined for file output."};
cpt_file([_|T]) ->
    cpt_file(T).





cpt_stream([]) ->
    ok;
cpt_stream([#eventJob{streamOutputEnabled        = true,
		      streamDestinationIpAddress = undefined} | _]) ->
    {error, "No Destination IpAddress defined for stream output."};
cpt_stream([#eventJob{streamOutputEnabled   = true,
		      streamDestinationPort = undefined} | _]) ->
    {error, "No Destination Port defined for stream output."};
cpt_stream([_|T]) ->
    cpt_stream(T).


%%----------------------------------------------------------------------
%% Send any changed ME data to all pesAppJobs
%%----------------------------------------------------------------------
me_notify(#state{me_data = MEData} = State) ->
    case pesLib:get_me_data() of
	MEData ->
	    State;
	NewMEData ->
	    me_notify_app_job(NewMEData, State)
    end.
	    

me_notify_app_job(NewMEData, State) ->
    {ok, AppJobs} = pesDb:app_reg_get_app_jobs(),
    lists:foreach(fun(AppJob) ->
			  pesAppJob:me_attr_update(AppJob, NewMEData)
		  end, AppJobs),
    State#state{me_data = NewMEData}.


%%========================================================================
%% 
%% 
%%========================================================================
handle_monitor(MRef, Pid, #state{prod_ids = GroupIds} = State) ->
    case [Key || {Key, {P, MR}} <- GroupIds, P == Pid, MR == MRef] of
	[Delete] ->
	    %%io:format("PES SERVER: JOB GROUP ~p DOWN~n", [Delete]),
	    State#state{prod_ids = proplists:delete(Delete, GroupIds)};
	[] ->
	    sysInitI:info_msg("PES SERVER: UNKNOWN JOB GROUP ~p ~p DOWN~n",
			      [MRef, Pid]),
	    State
    end.
		  

get_currently_active_jobs() -> 
    ets:foldl(fun gcaj/2, [], eventJob).

gcaj(#eventJob{currentJobState = ?JobState_ACTIVE} = Job, Acc) -> [Job | Acc];
gcaj(_, Acc) -> Acc.



%%========================================================================
%% 
%% 
%%========================================================================
get_current_state(JobId, Gids) ->
    gcs([JgPid || {_, {JgPid, _, JobIds}} <- Gids,
		  lists:member(JobId, JobIds)],
	JobId).

gcs([JobGrpPid], JobId) ->
    pesProducer:get_current_state(JobGrpPid, JobId);
gcs(_, _) ->
    undefined.



%%===========================================================================
%% Misc functions
%%===========================================================================
%% log_msg(_, _) -> ok.


format_state(State) ->
    F       = record_info(fields, state),
    [_ | L] = tuple_to_list(State), 
    lists:zip(F, L).
    
choose(true,  T, _) -> T;
choose(false, _, F) -> F.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
restartEventJobs() -> %%HV23638
    [restartEventJob(Job) || Job <- ets:tab2list(eventJob)],
    ok.

restartEventJob(#eventJob{eventJobId = {_, _, _, Prod, _},
              requestedJobState = ?JobState_ACTIVE,
              jobControl = ?JobControl_FULL} = EventJob) ->
    EJ = EventJob#eventJob{requestedJobState = ?JobState_STOPPED},
    pesDb:event_job_set(EJ),
    pesProducer:job_update(Prod, EJ, [EventJob]),
    timer:sleep(250),
    pesDb:event_job_set(EventJob),
    pesProducer:job_update(Prod, EventJob, [EJ]),
    ok;

restartEventJob(_) ->
    ok.

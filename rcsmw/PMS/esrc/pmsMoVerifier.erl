%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsMoVerifier.erl %
%%% 
%%% @doc
%%% Description:     
%%%
%%% Verifies the MOs in a COM transaction. 
%%% 
%%% @end 
%%% ----------------------------------------------------------
-module(pmsMoVerifier).
-id('Updated by CCase').
-vsn('/main/R8A/4').
-date('2016-12-15').
-author('eolaand').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R8A/1      2016-11-23 uabesvi     Created
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([verify/2]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("pms.hrl").
-include("RcsPm.hrl").


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% verify(ActionId, Objects) ->
%%   {ok, NumberOfActivePmJobs} | {error, Reason}
%% 
%% This function is invoked from pmsComteI to verify the COM transaction 
%% 
%%========================================================================
verify(ActionId, Objects) ->
    ?LOG_RAM(?SEV_5, 
	     ?LFUN({"Verify objects~n"
		    "  TransactionId = ~p~n"
		    "  Objects = ~n    ~p~n", 
		    [ActionId, [{What, element(1, Rec), element(2, Rec)} ||
				   {What, Rec} <- Objects]]})),
	     
    check_prep_trans(Objects).



%%========================================================================
%% check_prep_trans([Object], State) -> {ok, State} | {error, Reason}
%% 
%% New transaction is ongoing.
%% Check that PMS can accept it.
%% 
%%========================================================================
check_prep_trans(Objects) ->
    CreJobs = [C || {created, #pmJob{} = C} <- Objects],
    Active  = [C || #pmJob{requestedJobState = ?JobState_ACTIVE} = C <- CreJobs],
    DelJobs = [D || {deleted, #pmJob{} = D} <- Objects],
    {ok, MaxJobs} = pmsDb:meas_capabilities_get(maxNoOfJobs),
    cpt_rc(cpt_allowed(Objects),
	   cpt_rp(pmsDb:pms_env_get(reporting_period), CreJobs),
	   cpt_jobs(get_active_jobs(), 
		    get_active_new_jobs(Active),
		    length(DelJobs),
		    MaxJobs)).

%%----------------------------------------------------------------------
%% check_prep_trans result
%%----------------------------------------------------------------------
cpt_rc(ok, ok, {ok, ActiveJobs, JobDelta}) ->
    log_msg(prep_trans, {ActiveJobs, JobDelta}),
    {ok, ActiveJobs + JobDelta}; 
cpt_rc({error, _} = Error, _, _) ->
    Error;
cpt_rc(_, {error, _} = Error, _) ->
    Error;
cpt_rc(_, _, {error, _} = Error) ->
    Error.


%%----------------------------------------------------------------------
%% Check if allowed to update
%%----------------------------------------------------------------------    
cpt_allowed([]) ->
    ok;
cpt_allowed([{created, #pmJob{pmJobId = {ME, SF, PM, J} = JobId} = Rec} | T]) ->
    Error = "Not allowed to update an existsing PmJob {" ++
	ME ++ ", " ++ SF ++ ", " ++ PM ++ ", " ++ J  ++ "} ",
    case cpt_allowed_job(Rec, mnesia:dirty_read(pmJob, JobId)) of
	true            -> cpt_allowed(T);
	{error, Reason} -> {error, list_to_binary(Error ++  Reason)}
    end;
cpt_allowed([{created, 
	      #measurementReader{
		measurementReaderId = {ME, SF, PM, J, MR},
		measurementSpecification = #'MeasurementSpecification'{
		  groupRef           = undefined,
		  measurementTypeRef = undefined}}} | _]) ->
    Error = "MeasurementSpecification: either groupRef or measurementTypeRef"
	" must be specified \n"
	"  MeasurementReader = {" ++
	ME ++ ", " ++ SF ++ ", " ++ PM ++ ", " ++ J ++  ", " ++ MR ++ "}",
    {error, list_to_binary(Error)};
cpt_allowed([{created, 
	      #measurementReader{measurementReaderId = 
				 {ME, SF, PM, J, MR} = MrId} = New} | T]) ->
    Error = "Not allowed to update an existsing MeasurementReader {" ++
	ME ++ ", " ++ SF ++ ", " ++ PM ++ ", " ++ J ++  ", " ++ MR ++ "}",
    case mnesia:dirty_read(measurementReader, MrId) of
	[]    -> cpt_allowed(T);
	[New] -> cpt_allowed(T); %% this is a MR for a pre-defined
	_     -> {error, list_to_binary(Error)}
    end;
cpt_allowed([{deleted, _} | T]) ->
    cpt_allowed(T).



cpt_allowed_job(_, []) ->
    true;
%% Its allowed to update the requested state and jobGroup
cpt_allowed_job(#pmJob{pmJobId           = Jid,
		       requestedJobState = _,
		       reportingPeriod   = RP,
		       jobType           = Type,
		       jobPriority       = Prio,
		       granularityPeriod = GP,
		       currentJobState   = _,
		       jobControl        = Ctrl,
		       compressionType   = Comp,
		       jobGroup          = _
		      },
		[#pmJob{pmJobId           = Jid,
			requestedJobState = _,
			reportingPeriod   = RP,
			jobType           = Type,
			jobPriority       = Prio,
			granularityPeriod = GP,
			currentJobState   = _,
			jobControl        = Ctrl,
			compressionType   = Comp,
			jobGroup          = _
		       }
		]) ->
    true;
cpt_allowed_job(#pmJob{reportingPeriod   = OldRP,
		       jobType           = OldType,
		       jobPriority       = OldPrio,
		       granularityPeriod = OldGP,
		       jobControl        = OldCtrl,
		       compressionType   = OldComp
		      },
		[#pmJob{reportingPeriod   = NewRP,
			jobType           = NewType,
			jobPriority       = NewPrio,
			granularityPeriod = NewGP,
			jobControl        = NewCtrl,
			compressionType   = NewComp
		       }
		]) ->
    Illegal = [{OldRP   == NewRP,   "reportingPeriod"},
	       {OldType == NewType, "jobType"},
	       {OldPrio == NewPrio, "jobPriority"},
	       {OldGP   == NewGP,   "granularityPeriod"},
	       {OldCtrl == NewCtrl, "jobControl"},
	       {OldComp == NewComp, "compressionType"}],

    [Error | _] = [Text || {false, Text} <- Illegal],
    {error, "because it is not allowed to update " ++ Error}.
    
    
    



cpt_rp(legacy, 
       [#pmJob{reportingPeriod   = ?TimePeriod_FIFTEEN_MIN,
	       granularityPeriod = ?TimePeriod_FIFTEEN_MIN} | T]) ->
    cpt_rp(legacy, T);
cpt_rp(legacy, 
       [#pmJob{granularityPeriod = ?TimePeriod_FIFTEEN_MIN} | _]) ->
    {error, <<"Invalid reporting period.">>};
cpt_rp(legacy, [#pmJob{} | _]) ->
    {error, <<"Invalid granularity period.">>};
cpt_rp(legacy, [_ | T]) ->
    cpt_rp(legacy, T);
cpt_rp(_, _) ->
    ok.
  


%%----------------------------------------------------------------------
%% Count number of jobs
%%----------------------------------------------------------------------
    
cpt_jobs(Active, CreSize, DelSize, Max) 
  when Active > Max andalso 
       CreSize > DelSize ->
    {error, <<"Max number of jobs already started">>};
cpt_jobs(Active, CreSize, DelSize, _Max) ->
    {ok, Active, CreSize - DelSize}.







%%===========================================================================
%% Misc functions
%%===========================================================================
%%========================================================================
%% get_active_jobs() -> integer()
%% 
%%========================================================================
get_active_jobs() -> 
    ets:foldl(fun(#pmJob{currentJobState = ?JobState_ACTIVE}, Acc) -> Acc + 1;
		 (_,                                          Acc) -> Acc 
	      end,
	      0,
	      pmJob).		      

%%========================================================================
%% get_active_new_jobs(Jobs) -> integer()
%% 
%%========================================================================
get_active_new_jobs(Jobs) -> 
    lists:foldl(fun(#pmJob{currentJobState = ?JobState_ACTIVE}, Acc) -> Acc + 1;
		   (_,                                          Acc) -> Acc 
		end,
		0,
		Jobs).



%%===========================================================================
%% Misc functions
%%===========================================================================


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
 
log_msg(_, _) -> ok.


%%p(_,_) -> ok.

%%p(S,A) -> io:format(S,A).


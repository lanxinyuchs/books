%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsShowCountersLib.erl %
%%% 
%%% Description:     
%%%
%%% 
%%% 
%%% 
%%% 
%%% 
%%% 
%%% ----------------------------------------------------------
-module(pmsShowCountersLib).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/R6A/R8A/R9A/2').
-date('2017-03-17').
-shaid('60c493b7dad1de126485a475696a34cc668b7531').
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
%%% R1A/1      2014-04-07 uabesvi     Created
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([show_counter_names/1]).

-export([get_pmi_vsn/1]).
-export([get_job_ids/1]).

-export([filter_counters/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("pms.hrl").
-include("RcsPm.hrl").


%% show counter timeout for waiting for result from the application 
-define(SC_TO, 5).


%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

	 
%%========================================================================
%% show_counter_names(Ldn) -> [Counter].
%% 
%% Find all counter names for auto completion.
%% 
%% First, if no application has registered an app ldn for the requested 
%% instance return error no counters.
%% 
%% 
%% 
%% 
%%========================================================================
show_counter_names(Ldn) ->
    Classes  = pmsDb:pms_mo_class_get(get_mo_class(Ldn)),
    ?LOG_RAM(?SEV_5, {"Found classes~n  ~p~n", [Classes]}),
    Counters = lists:append([Cs || #pmsScMoClasses{counters = Cs} <- Classes]),
    scn_rc([C || C <- Counters]).

scn_rc([]) ->
    {error, not_exist};
scn_rc(Counters) ->
    [list_to_binary(C) || C <- Counters].





get_mo_class(Ldn) ->
    DN     = string:tokens(Ldn, ","),
    Tokens = [{K,V} || [K,V] <- [string:tokens(LL, "=") || LL <- DN]],
    [{MoClass, _} | _] = lists:reverse(Tokens),
    MoClass.



%%========================================================================
%% get_pmi_vsn(Ldn) -> {Vsn, VsnName}
%% 
%% Find which version of PMI the application uses.
%% 
%%========================================================================
get_pmi_vsn(Ldn) ->
    AllLdns = get_all_ldns(lists:reverse(string:tokens(Ldn, ",")), []),
    {Vsn, Pmi} = case cal(AllLdns, mnesia:dirty_all_keys(pmsScAppMoLdns)) of
		     {ok, _} -> {1, "PMI"};
		     _       -> {2, "PMI2"}
		 end,
    ?LOG_RAM(?SEV_5, {"Using interface ~p~n", [Pmi]}),
    Vsn.


%%========================================================================
%% get_job_ids(Ldn) -> [JobId].
%% 
%% Ldn    - binary()
%% JobId  - binary()
%% 
%% Find PM Jobs associated with the LDN
%% 
%%========================================================================
get_job_ids(Ldn) ->
    MoClass   = get_mo_class(Ldn),
    AllJobIds = pmsDb:pm_job_all_keys(),
    Res = [check_job(pmsDb:measurement_reader_dirty_match(JobId), 
		     JobId,
		     MoClass)
	   || JobId <- AllJobIds],
    [list_to_binary(JobId) || {ok, {_, _, _, JobId}} <- Res].




%%========================================================================
%% filter_counters(LDN, RequestedCounters, Options) -> Counters.
%% 
%% There are three filtering of counters.
%% 
%% 1) LDN
%% The LDN pinpoints the MO class for which the counter values are
%% requested. That class is matched agains the MoClass for all PmGroups.
%% The matching PmGroups, and their MeasurementReaders, are the only
%% valid counters to be further filetered.
%% 
%% 2) RequestedCounters
%% The operator can narrow the search by defining the wanted counters
%% in RequestedCounters.
%% 
%% 3) JobId
%% Options may contain a job_id option. The JobId is used to filter
%% the remaining counters from filter 2.
%% The JobId has MeasurementReaders (MR) attached to it. The MRs 
%% contains references to either a PmGroup or to a MeasurementType.
%% This information is used to do the third filtering.
%%========================================================================
filter_counters(Ldn, ReqCounters, Options) ->
    %%------------------------------------------------
    %% 1) Find all counters associated wiht the LDN
    %%------------------------------------------------
    [_, MoClass | _] = lists:reverse(string:tokens(Ldn, "=,")),
    fc_grps(fc_get_grps(pmsDb:pm_group_find_mo_class(MoClass)),
	    ReqCounters, 
	    Options).
    %% fc_grps(fc_get_grps(pmsDb:pm_group_find_obj_ldn(Ldn)),
    %% 	    ReqCounters, 
    %% 	    Options).

fc_grps({ok, []}, _, _) ->
    {error, ?SC_NO_COUNTERS};
fc_grps({ok, _} = Grps, ReqCounters, Options) ->
    ?LOG_RAM(?SEV_5, {"Found Groups = ~p~n", [Grps]}),
    Counters = fc_get_counters(Grps),
    ?LOG_RAM(?SEV_5, 
	     ?LFUN({"Found Counters = ~p~n", 
		    [pmsLib:log_trunc_sc_counters(Counters)]})),
    %%------------------------------------------------
    %% 2) Filter RequestedCounters
    %%------------------------------------------------
    FilterReqCounters = fc_filter_req_counters(Counters, ReqCounters, []),
    ?LOG_RAM(?SEV_5, 
	     ?LFUN({"Filtered counters (Requested) = ~p~n", 
		    [pmsLib:log_trunc_sc_counters(FilterReqCounters)]})),
    %%------------------------------------------------
    %% 3) Filter JobId
    %%------------------------------------------------
    JobCounters = fc_job_counters(proplists:get_value(job_id, Options)),
    ?LOG_RAM(?SEV_5, 
	     ?LFUN({"Filtered counters (JobId) = ~p~n", 
		    [pmsLib:log_trunc_sc_counters(JobCounters)]})),
    FilteredCounters = fc_filter_job_counters(FilterReqCounters,
					      JobCounters, 
					      []),
    ?LOG_RAM(?SEV_5, 
	     ?LFUN({"Filtered counters = ~p~n", 
		    [pmsLib:log_trunc_sc_counters(FilteredCounters)]})),
    FilteredCounters.


fc_get_grps({ok, Recs}) ->
    {ok, [Grp || #pmGroup{pmGroupId = {_, _, _, Grp}} <- Recs]};
fc_get_grps(_Error) ->
    {error, ?SC_NO_COUNTERS}.



fc_get_counters({ok, Grps}) ->
    fc_get_counters(Grps, []).

fc_get_counters([], Acc) ->
    Acc;
fc_get_counters([PmGroup | T], Acc) ->
    {ok, MTs} = pmsDb:measurement_type_match({"1", "1", "1", PmGroup}),
    MT = [Mid || #measurementType{measurementTypeId = {_,_,_,_,Mid}} <- MTs],
    fc_get_counters(T, [{PmGroup, MT} | Acc]).


fc_filter_req_counters(All, [], _) ->
    All;
fc_filter_req_counters([], _, Acc) ->
    Acc;
fc_filter_req_counters([{Grp, Counters} | T], ReqCounters, Acc) ->
    RemCounters = [C || C <- Counters, lists:member(C, ReqCounters)],
    case RemCounters of
	[] -> fc_filter_req_counters(T, ReqCounters, Acc);
	RC -> fc_filter_req_counters(T, ReqCounters, [{Grp, RC} | Acc])
    end.


fc_job_counters(undefined) ->
    undefined;
fc_job_counters(Job) ->
    JobName   = binary_to_list(Job),
    JobId     = {"1", "1", "1", JobName},
    {ok, MRs} = pmsDb:measurement_reader_dirty_match(JobId),
    fc_get_job_counters(MRs, []).
    

fc_filter_job_counters(Counters, undefined, []) ->
    Counters;
fc_filter_job_counters(_, [], []) ->
    [];
fc_filter_job_counters([], _, Acc) ->
    ?LOG_RAM(?SEV_5, {"Final Counters = ~p~n", [Acc]}),
    lists:flatten(Acc);
fc_filter_job_counters([{ReqGrp, ReqGrpMTs} | T], JobMRMTs, Acc) ->
    Res = fc_fjc(proplists:get_value(ReqGrp, JobMRMTs), ReqGrpMTs, JobMRMTs),
    fc_filter_job_counters(T, JobMRMTs, [{ReqGrp, Res} | Acc]).
    

fc_fjc(undefined, ReqGrpMTs, JobMRMTs) ->
    JobNoGrpMTs = [NoGrpMT || NoGrpMT <- JobMRMTs, not is_tuple(NoGrpMT)],
    fc_fjc(JobNoGrpMTs, ReqGrpMTs, JobMRMTs);
fc_fjc(JobMRGroupMTs, ReqGrpMTs, _JobMRMTs) ->
    [MT || MT <- ReqGrpMTs, lists:member(MT, JobMRGroupMTs)].


fc_get_job_counters([], Acc) ->
    lists:usort(lists:append(Acc));
fc_get_job_counters([#measurementReader{measurementSpecification = MS} | T],
		 Acc) ->
    fc_get_job_counters(T, [fc_gjc(MS) | Acc]).


fc_gjc(#'MeasurementSpecification'{groupRef           = undefined,
				   measurementTypeRef = MtRef}) ->   
    {_, _, _, _, MtId} = get_id(string:tokens(binary_to_list(MtRef), "=,"),
				[]),
    [MtId];
fc_gjc(#'MeasurementSpecification'{groupRef = GroupRef}) -> 
    GrpId = get_id(string:tokens(binary_to_list(GroupRef), "=,"), []),
    {_, _, _, Grp} = GrpId,
    {ok, MTs} = pmsDb:measurement_type_dirty_match(GrpId),
    Cnts = [MT || #measurementType{measurementTypeId = {_, _, _, _, MT}}
		      <- MTs],
    [{Grp, Cnts}].




%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% check_job(MRs, JobId, MoClass) -> {ok, JobId} | {error, not_associated}
%% 
%% Return JobId if the MRs contains an association to MoClass.
%% 
%%========================================================================
check_job(MRs, JobId, MoClass) ->
    MoClasses = get_mo_classes(MRs, []),
    cj(lists:member(MoClass, MoClasses), JobId).

cj(true, JobId) ->
    {ok, JobId};
cj(false, _) ->
    {error, not_associated}.

%%--------------------------------------------------------------
%% Find the PmGroup and return the MoClasses for the associated
%% PmGroups.
%%--------------------------------------------------------------
get_mo_classes([], Acc) ->
    Acc;
get_mo_classes({ok, [#measurementReader{measurementSpecification = MS} | T]},
	       Acc) ->
    get_mo_classes(T, [gmc(MS) | Acc]).


gmc(#'MeasurementSpecification'{groupRef           = undefined,
			        measurementTypeRef = MtRef}) ->   
    {ME, SF, PM, Grp, _} = get_id(string:tokens(binary_to_list(MtRef), "=,"),
				  []),
    gmc_get_mo_class({ME, SF, PM, Grp});
gmc(#'MeasurementSpecification'{groupRef = GroupRef}) -> 
    GrpId = get_id(string:tokens(binary_to_list(GroupRef), "=,"), []),
    gmc_get_mo_class(GrpId).
    

gmc_get_mo_class(GrpId) ->
    {ok, [#pmGroup{moClass = #'ManagedObjectClass'{moClassName = MoClass}}]} =
	pmsDb:pm_group_get(GrpId),
    MoClass.

    




%%========================================================================
%% Transform a LDN to MO identity
%%========================================================================
get_id([], Acc) ->
    list_to_tuple(lists:reverse(Acc));
get_id([_, Id | T], Acc) ->
    get_id(T, [Id | Acc]).




%%========================================================================
%% Create a list of all sub LDNs from an LDN
%%========================================================================
get_all_ldns([], Acc) ->
    lists:reverse(Acc);
get_all_ldns([H | T] = Ldn, Acc) ->
    [Class | _] = string:tokens(H, "="),
    get_all_ldns(T, 
		 [string:join(lists:reverse([Class | T]), ","),
		  string:join(lists:reverse(Ldn), ",") | Acc]). 




%%========================================================================
%% Check if the LDN is found in the top ldn table
%%========================================================================
cal([], _Keys) ->
    {error, ?SC_NO_COUNTERS};
cal([Ldn | T], Keys) ->
    Matches = [string:str(Ldn, K) || K <- Keys],
    case {Matches, length([1 || 1 <- Matches])} of
	{[], _} -> cal(T, Keys);
	{_,  0} -> cal(T, Keys);
	_       -> {ok, Ldn}
    end.
	    





%%========================================================================
%% Misc functions
%%========================================================================



%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


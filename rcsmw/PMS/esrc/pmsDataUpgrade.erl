%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsDataUpgrade.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pmsDataUpgrade).
-vsn('/main/R3A/R4A/R5A/3').
-date('2016-04-04').
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2014-11-11 uabesvi     created
%%% R5A/1      2015-11-10 uabesvi     ecim 2.4
%%% R5A/2      2016-02-03 uabesvi     HU52672
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([upgrade/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("pms.hrl").
-include("RcsPm.hrl").



%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

upgrade(false) ->
    ok;
upgrade(true) ->
    %% These are not changed in any previous release
    %% do not copy
    %% - pm (will be defined in init_data)
    %% - pmMeasurementCapabilities (will be defined in init_data)
    %% - pmsAppRegistry (will be recreated when the apps sends initialize)
    %% - pmGroup and measurementType (will be created when appdata is received)
    Tabs = [pm,                       
	    pmThresholdMonitoring,
	    pmsAppsInfo
	   ],
    [copy_old_table(T) || T <- Tabs],

    pm_job(swmI:all_objects(pmJob)),
    measurement_reader(swmI:all_objects(measurementReader)),
    ok.

copy_old_table(T) ->
    try
	swmI:copy_old_table(T)
    catch
	_:_ ->
	    ok
    end.
	    
%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% pmJob() -> ok.
%% 
%% Upgrade pmJob records from the old format to the new format
%%========================================================================
pm_job([]) ->
    ok;
pm_job([#pmJob{jobControl = ?JobControl_FULL} = Job | T]) ->
    %% ECIM PM 2.2
    RefRec = #pmJob{jobControl      = ?JobControl_FULL,
		    compressionType = ?CompressionTypes_GZIP,
		    jobGroup        = ?DEFAULT_JOB_GROUP},
    [NewJob] = transform([Job], RefRec),
    pmsDb:pm_job_set(NewJob),
    pm_job(T);
pm_job([#pmJob{pmJobId           = Jid,
	       requestedJobState = ReqJobState} | T]) ->
    pj(pmsDb:pm_job_get(Jid), ReqJobState, Jid),
    pm_job(T).


%% Predefined job exists in both old and new release
%% Get the requested state from the old release
pj({ok, [NewJob]}, OldReqJobState, _Jid) ->
    pmsDb:pm_job_set(NewJob#pmJob{requestedJobState = OldReqJobState});
%% Deleted predefiend job
pj(_, _, Jid) ->
    sysInitI:info_msg("Predefined Job ~p is removed in the upgrade", [Jid]),
    ok.



%%========================================================================
%% measurementReader() -> ok.
%% 
%% Upgrade measurement reader records from the old format to the new format
%%========================================================================
%% ECIM PM 2.2
measurement_reader([]) ->
    ok;
measurement_reader([MR | T]) ->
    {ME, SF, PM, PmId, _} = Mid = element(2, MR),
    mr(pmsDb:pm_job_get({ME, SF, PM, PmId}), MR, Mid),
    measurement_reader(T).


%% Predefined job, do not copy the old MRs, 
%% the new appdata file has already created all of the MRs for the new UP
mr({ok, [#pmJob{jobControl = ?JobControl_STARTSTOP}]}, _MR, Mid) ->
    sysInitI:info_msg("Predefined measurement reader deleted = ~p~n", [Mid]),
    ok;
%% Operator created job, or predefined job that is new or exists in both UPs
mr({ok, [#pmJob{}]}, MR, _Mid) ->
    RefRec  = #measurementReader{},
    [NewMr] = transform([MR], RefRec),
    pmsDb:measurement_reader_set(NewMr);
%% This is a predefined job that has been removed
mr(_, _, Mid) ->
    sysInitI:info_msg("Predefined job delted, "
		      "delete also measurement reader = ~p~n", [Mid]),
    ok.



%%========================================================================
%% transform(OldRecs, ReferenceRec) -> ok.
%% 
%% Generic transformation of old record format to new.
%% 
%% NOTE: works only if the new fields in the new record
%%       are appended to the end of the record.
%%========================================================================
%% transform([], _RefRec) ->
%%     [];
transform([OldRec | _] = OldRecs, RefRec) ->
    [transform_rec(OR, size(OldRec), RefRec) || OR <- OldRecs].
    
    

transform_rec(Old, OldSize, New) ->
    OldL = tuple_to_list(Old),
    {_, NewAttrs} = lists:split(OldSize, tuple_to_list(New)),
    list_to_tuple(OldL ++ NewAttrs).

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


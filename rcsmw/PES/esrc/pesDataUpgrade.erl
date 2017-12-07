%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesDataUpgrade.erl %
%%% Author:	uabesvi
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pesDataUpgrade).
-vsn('/main/R3A/R4A/R5A/R6A/1').
-date('2016-09-06').
-author('etomist').
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
%%% R3A/1      2014-12-17 uabesvi     created
%%% R4A/1      2015-09-08 uabesvi     error_logger -> sysInitI
%%% R5A/1      2016-01-26 uabesvi     fixes for predefined jobs
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


-include("pes.hrl").
-include("RcsPMEventM.hrl").



%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

upgrade(false) ->
    ok;
upgrade(true) ->
    %% All but event jobs are created from the appdata files
    try
	Remove = pm_appdata_jobs(swmI:all_objects(pesEnv)),
	pm_event_job(swmI:all_objects(eventJob), Remove),
	ok
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
%% pm_appdata_jobs() -> [EventJobId].
%% 
%% Find the predefined jobs that existed in the old UP but 
%% not in the new UP
%%========================================================================
pm_appdata_jobs([]) ->
    [];
pm_appdata_jobs(Env) ->
    Old = paj(lists:keyfind(appdata_created, 2, Env)),
    New = pesDb:pes_env_get(appdata_created),
    ?LOG_RAM(?SEV_1, 
	     {"upgrade. These predefined jobs are not "
	      "included in the new UP ~p~n",
	      [Old -- New]}),
    Old -- New.

paj(false) ->
    [];
paj({pesEnv, appdata_created, Old}) ->
    Old.

%%========================================================================
%% pm_event_job() -> ok.
%% 
%% Upgrade pmJob records from the old format to the new format
%%========================================================================
pm_event_job([], _) ->
    ok;
pm_event_job([#eventJob{eventJobId = Jid} = Old | T], Remove) ->
    {ok, New} = pesDb:event_job_get(Jid),
    pej_predef(lists:member(Jid, Remove), Old, New),
    pm_event_job(T, Remove).


%% Removed predefind job
pej_predef(true, #eventJob{eventJobId = Jid}, _) ->
    sysInitI:info_msg("Predefined Job ~p is removed in the upgrade", [Jid]),
    ?LOG_RAM(?SEV_1, {"upgrade. Removed old predefined job ~p~n", [Jid]}),
    ok;
%% Operator created event job
pej_predef(false, #eventJob{eventJobId = Jid} = Job, []) ->
    ?LOG_RAM(?SEV_1, {"upgrade. Old job recreated ~p~n", [Jid]}),
    pesDb:event_job_set(Job);
%% Predefined job 
pej_predef(false,
	   #eventJob{eventJobId             = Jid,
		     requestedJobState          = ReqState,
		     eventFilter                = Filter,
		     streamDestinationIpAddress = IpAddr,
		     streamDestinationPort      = Port,
		     streamOutputEnabled        = Stream,
		     fileOutputEnabled          = File,
             eventGroupRef              = EventGroupRef,
             eventTypeRef               = EventTypeRef,
             fileCompressionType        = FileCompressionType,
             streamCompressionType      = StreamCompressionType
		     }, 
	   [New]) ->
    ?LOG_RAM(?SEV_1, {"upgrade. Predef job created ~p~n", [Jid]}),
    pesDb:event_job_set(New#eventJob{requestedJobState          = ReqState,
				     eventFilter                = Filter,
				     streamDestinationIpAddress = IpAddr,
				     streamDestinationPort      = Port,
				     streamOutputEnabled        = Stream,
				     fileOutputEnabled          = File,
                     eventGroupRef              = EventGroupRef,
                     eventTypeRef               = EventTypeRef,
                     fileCompressionType        = FileCompressionType,
                     streamCompressionType      = StreamCompressionType
				    }).


%%========================================================================
%% transform(OldRecs, ReferenceRec) -> ok.
%% 
%% Generic transformation of old record format to new.
%% 
%% NOTE: works only if the new fields in the new record
%%       are appended to the end of the record.
%% %%========================================================================
%% transform([], _RefRec) ->
%%     [];
%% transform([OldRec | _] = OldRecs, RefRec) ->
%%     [transform_rec(OR, size(OldRec), RefRec) || OR <- OldRecs].
    

%% transform_rec(Old, OldSize, New) ->
%%     OldL = tuple_to_list(Old),
%%     {_, NewAttrs} = lists:split(OldSize, tuple_to_list(New)),
%%     list_to_tuple(OldL ++ NewAttrs).



%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


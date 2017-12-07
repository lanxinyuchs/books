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
%%% ----------------------------------------------------------


-define(T, pesLib:get_time()).

%%==================================================
%% EventProducer
%%==================================================
-define(PRODUCER_NAME(Name), list_to_atom("event_producer_" ++ Name)).


%%==================================================
%% EventJob
%%==================================================
-define(APP_ACTIVE,  active).
-define(APP_STOPPED, stopped).



%%==================================================
%% logging
%%==================================================

-define(PM_EVENT_LOG, "RcsPmEvents").


-define(SEV_ERROR,   error).
-define(SEV_WARNING, warning).
-define(SEV_1, 1).
-define(SEV_2, 2).
-define(SEV_3, 3).
-define(SEV_4, 4).
-define(SEV_5, 5).
-define(SEV_6, 6).
-define(SEV_7, 7).
-define(SEV_8, 8).
-define(SEV_9, 9).


-define(LOG_RAM(__MultiMsg),
	begin
	    logRamI:write_log(?PM_EVENT_LOG,
			      {?MODULE, ?LINE, self()}, 
			      __MultiMsg)
	end).

-define(LOG_RAM(__Sev, __Msg),
	begin
	    logRamI:write_log(?PM_EVENT_LOG,
			      {?MODULE, ?LINE, self()}, 
			      {__Sev, __Msg})
	end).

-define(LOG_RAM(__Sev, __Msg, __BL),
	begin
	    logRamI:write_log(?PM_EVENT_LOG,
			      {?MODULE, ?LINE, self()}, 
			      {__Sev, __Msg, __BL})
	end).


 

-define(ERR_UNKNOWN_MAP_ID, "Unknown EventMapId").

%%==================================================
%% record definitions
%%==================================================

-record(pesEnv, {key, value}).


%% values = {GroupAlias, eventGroupId} 
-record(pesGroupAliases, {eventMapId, values}). 

%% values = {TypeAlias, eventTypeId} 
-record(pesTypeAliases, {eventMapId, values}). 


-record(pesAppRegType, {event_type,   
			job_pid
		       }             
       ).

-record(pesAppRegPid, {job_pid,
		       event_types,
		       callbacks,
		       session_pid
		      }             
       ).


%%--------------------------------------------------
%% list of all PES modules (used for debugging)
%%--------------------------------------------------

-define(PES_MODS, [
		   pesAppData,
		   pesAppJob,
		   pesAppRegistry,
		   pesDataInit,
		   pesDataUpgrade,
		   pesDb,
		   pesDebug,
		   pesEnv,
		   pesI,
		   pesJob,
		   pesLib,
		   pesLog,
		   pesModel,
		   pesPeI,
		   pesProducer,
		   pesServer,
		   pesSession,
		   pesSshSftpdFile
		  ]).

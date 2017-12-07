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
%%% ----------------------------------------------------------

-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').



-define(PM_SUPPORT(RFH), #pmSupport{pmSupportId     = {"1", "1", "1"},
				    ropFileHandling = RFH}).

-define(COMMON_JG, undefined).

%%==================================================
%% PMI2 Callbacks
%%==================================================
-define(SUBSCRIBE_CB, pmi2SubscribeRopCallback).
-define(REPORT_ROP_CB, pmi2ReportRopCallback).
-define(REPORT_SC_CB, pmi2ReportShowCountersCallback).

%%==================================================
%% PmJob
%%==================================================
-define(APP_ACTIVE,  active).
-define(APP_STOPPED, stopped).

%%==================================================
%% PmGroup
%%==================================================
-define(PM_GROUP_ID(PmGroup),  {"1", "1", "1", PmGroup}).

%%==================================================
%% PmJob
%%==================================================
-define(PM_JOB_ID(PmJob),  {"1", "1", "1", PmJob}).

%%==================================================
%% JobGroup
%%==================================================
-define(DEFAULT_JOB_GROUP,  default_job_group).

%%==================================================
%% show counters
%%==================================================
-define(SC_OK, 0).
-define(SC_NO_COUNTERS,    1).
-define(SC_INTERNAL_ERROR, 2).

%%==================================================
%% logging
%% 
%% To create a new tag you have to do the following:
%% 
%% pms.hrl
%%  - add two new defines below for the tag
%%  - add the new tag to LOG_LEVELS macro
%% 
%% pmsLog.erl
%%  - add new define for the tag
%%  - add the tag to LEVELS macro
%%  - add new function (named after the tag) 
%%==================================================
-define(PM_LOG,       "RcsPmCounters").


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

-define(LFUN(__FMT),
	fun() ->
		__FMT
	end).

-define(LOG_RAM(__MultiMsg),
	begin
	    logRamI:write_log(?PM_LOG,
			      {?MODULE, ?LINE, self()}, 
			      __MultiMsg)
	end).

-define(LOG_RAM(__Sev, __Msg),
	begin
	    logRamI:write_log(?PM_LOG,
			      {?MODULE, ?LINE, self()}, 
			      {__Sev, __Msg})
	end).

-define(LOG_RAM(__Sev, __Msg, __BL),
	begin
	    logRamI:write_log(?PM_LOG,
			      {?MODULE, ?LINE, self()}, 
			      {__Sev, __Msg, __BL})
	end).



 
-define(LOG_RAM_SLAVE(__Pid, __MultiMsg),
	begin
	    logRamI:write_log(?PM_LOG,
			      {?MODULE, ?LINE, __Pid}, 
			      __MultiMsg)
	end).

-define(LOG_RAM_SLAVE(__Pid, __Sev, __Msg),
	begin
	    logRamI:write_log(?PM_LOG,
			      {?MODULE, ?LINE, __Pid}, 
			      {__Sev, __Msg})
	end).

-define(LOG_RAM_SLAVE(__Pid, __Sev, __Msg, __BL),
	begin
	    logRamI:write_log(?PM_LOG,
			      {?MODULE, ?LINE, __Pid}, 
			      {__Sev, __Msg, __BL})
	end).

  

%%==================================================
%% LOG_MAX is used to truncate long variables
%% when logging
%%==================================================
-define(LOG_MAX, 2).


%%==================================================
%% Error strings
%%==================================================
-define(ERR_UNKNOWN_MAP_ID, "Unknown CounterMapId").
-define(ERR_INCONSISTENT_CALLBACKS, "Inconsistent Callback Flags").

%%==================================================
%% Default rop dir
%%==================================================
-define(DEFAULT_ROP_DIR, "rop").
-define(ROP_DIR_SIZE, 30000).


%%==================================================
%% FLEX counters
%% 
%%==================================================
-define(FLEX_PREFIX, "PmFlex").

-define(PROC_TYPE_UNDEFINED, pt_undefined).
-define(PROC_TYPE_FLEX,      pt_flex).
-define(PROC_TYPE_COMMON,    pt_common).


%%==================================================
%% record definitions
%%==================================================

%%--------------------------------------------------
%% Stores information about the applications
%%--------------------------------------------------
-record(pmsAppsInfo, {key,                   %% {CxpProdId, CxpVersion}
		      pg_ids = [],           %% [pmGroupId]
		      state  = ?APP_STOPPED, %% active | stopped
		      callback}             
       ).


%%--------------------------------------------------
%% Used to find the sessions handling specific PmGroups
%%--------------------------------------------------
-record(pmsAppRegistry, {pm_group,   
		         job_pid,
		         session_pid,
			 pmi_cb        %% CallbackMod | 
                                       %% {CallbackMod, CallbackFuncFlags}
			}             
       ).



%%--------------------------------------------------
%% Used for debugging issues from pmsEnv
%%--------------------------------------------------
-record(pmsEnv, {key, value}).


%%--------------------------------------------------
%% Used for show-counters.
%% 
%% This table is populated when the appdata files
%% for PmGroups are parsed in pmsAppData.
%% 
%% It is used to find the counter names for the 
%% MO instance for wich the show-counters is requested.
%%--------------------------------------------------
-record(pmsScMoClasses, {mo_class,   
			 pm_group,
			 counters
			}             
       ).


%%--------------------------------------------------
%% Used for show counters.
%% Used only in PMI and only for pmiInititialize_2
%% 
%% The application informs PMS about the Top LDN.
%% All show-counters requests where the requested
%% LDN has Top LDN as a prefix should be forwarded
%% to that session.
%%--------------------------------------------------
-record(pmsScAppMoLdns, {ldn,   
			 app_job_pid,
			 pm_groups
			}             
       ).


%%--------------------------------------------------
%% Stores the aliases for PmGroups and MeasurementTypes
%% 
%% Populated when parsing the alias appdata file.
%% 
%% values = {GroupAlias, GroupId} | {{GroupAlias, MtAlias}, {GroupId, MrId}}
%%--------------------------------------------------
-record(pmsCounterAliases, {counterMapId, values}). 


%%--------------------------------------------------
%% list of all PMS modules (used for debugging)
%%--------------------------------------------------
-define(PMS_MODS, [
		   pmsAppData,
		   pmsAppJob,
		   pmsAppRegistry,
		   pmsComteI,
		   pmsDataInit,
		   pmsDataUpgrade,
		   pmsDb,
		   pmsDebug,
		   pmsI,
		   pmsJob,
		   pmsJobGroup,
		   pmsJobMeasInfo,
		   pmsLib,
		   pmsMoVerifier,
		   pmsPmI,
		   pmsPmI2,
		   pmsRopXml,
		   pmsServer,
		   pmsSession,
		   pmsSession2,
		   pmsSftpdEnv,
		   pmsShowCounters,
		   pmsShowCountersI,
		   pmsShowCountersLib,
		   pmsSshSftpdFile
		  ]).

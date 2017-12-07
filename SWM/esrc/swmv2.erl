%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmv2.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R10A/R11A/R12A/3

%% @doc == REST API for SWM, version 2 ==
%% Interface module for REST API, specified in swmWeb.xml.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmv2).
-vsn('/main/R10A/R11A/R12A/3').
-date('2017-12-06').
-author(etxberb).

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:	module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%

%% ###=======================================================================###
%% # 1.4   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R10A/1  2017-06-21 etxberb  Created.
%% R11A/1  2017-10-01 etxberb  SP531: Added calls to alhI.
%% R11A/3  2017-10-12 etxberb  Added upgrade_clone_complete/3.
%% R11A/4  2017-10-17 etxpejn  Added calls to upgrade appl in start_traffic
%%                             and stop_traffic
%% ----    ---------- -------  -------------------------------------------------
%% R12A/2  2017-12-01 etxberb  Refactoring.
%% R12A/3  2017-12-06 etxberb  Refactoring.
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    INTERNAL DEFINITIONS
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 2.1.1 Interface functions
%% ###-----------------------------------------------------------------------###
%% Basic

%% REST Query
-export([create_upgrade_backup/3,
	 is_supported/3,
	 start_traffic/3,
	 stop_traffic/3,
	 upgrade_clone_complete/3]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###
-include("SwmREST.hrl").
-include_lib("kernel/include/file.hrl").

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 3.    CODE
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% @doc REST query for creating a backup.
%%
%% @end
%% ###=======================================================================###
-spec create_upgrade_backup(SessionID :: any(),
			    Env       :: list({Property :: any(),
					       Value :: any()}),
			    Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
create_upgrade_backup(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(create_upgrade_backup).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
create_upgrade_backup("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    ActionId = swmLib:get_new_action_id(mgr),
    BackupName = swmREST:maps_get(?JsonAttrName_BackupName, Attrs),
    swmvUpgrade:create_upgrade_backup(Attrs#{env => Env,
					     action_id => ActionId,
					     backup_name => BackupName}),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([{action_id, ActionId}]));
create_upgrade_backup(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to determine if the REST API Version is supported.
%%
%% @end
%% ###=======================================================================###
-spec is_supported(SessionID :: any(),
		   Env       :: list({Property :: any(), Value :: any()}),
		   Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_supported(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(is_supported).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_supported("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
is_supported(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to start traffic.
%%
%% @end
%% ###=======================================================================###
-spec start_traffic(SessionID :: any(),
		    Env       :: list({Property :: any(), Value :: any()}),
		    Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_traffic(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(start_traffic).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_traffic("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    StopTime = swmREST:maps_get_optional(?JsonAttrName_StopTime, Attrs),
    ok = alhI:start_traffic(StopTime),

    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} = swmREST:send_cs_trigger(CbModules, start_traffic, "", ok),
    case {{CSAnswer, CSInfo}, swmREST:send_appl_trigger("trafficStart")} of
    	{{ok, ""}, {ok, ""}} ->
	    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
    	{{ok, _Msg1}, {ok, _Msg2}} ->
	    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
    	{{_Ans1, Msg1}, {_Ans2, Msg2}} ->
    	    %% One or both of the the answeres are error
	    swmREST:deliver_error(SessionID, Env,
    				  500,
    				  Msg1 ++ Msg2)
    end;
start_traffic(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to stop traffic.
%%
%% @end
%% ###=======================================================================###
-spec stop_traffic(SessionID :: any(),
		   Env       :: list({Property :: any(), Value :: any()}),
		   Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop_traffic(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(stop_traffic).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop_traffic("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    Res = comsaServDiscServer:stop_gRpc(),   % TODO: Temporary action until
						% permanent solution in place.
    ?LOG_INFO([{stop_gRpc, Res}]),

    TimeOfAction = swmREST:maps_get_optional(?JsonAttrName_TimeOfAction, Attrs),
    Cause = swmREST:maps_get_optional(?JsonAttrName_Cause, Attrs),
    StopTime = calendar:local_time(),
    ReturnValues = alhI:stop_traffic(TimeOfAction, Cause, StopTime),
    
    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} = swmREST:send_cs_trigger(CbModules, stop_traffic, "", ok),
    case {{CSAnswer, CSInfo}, swmREST:send_appl_trigger("trafficStop")} of
    	{{ok, ""}, {ok, ""}} ->
	    swmREST:deliver(SessionID, Env, ?JsonResult_Props(ReturnValues));
    	{{ok, _Msg1}, {ok, _Msg2}} ->
	    swmREST:deliver(SessionID, Env, ?JsonResult_Props(ReturnValues));
    	{{_Ans1, Msg1}, {_Ans2, Msg2}} ->
    	    %% One or both of the the answeres are error
	    swmREST:deliver_error(SessionID, Env,
    				  500,
    				  Msg1 ++ Msg2)
    end;
stop_traffic(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to inform that the disk is now cloned to the to-state node.
%%
%% @end
%% ###=======================================================================###
-spec upgrade_clone_complete(SessionID :: any(),
			     Env       :: list({Property :: any(),
						Value :: any()}),
			     Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
upgrade_clone_complete(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(upgrade_clone_complete).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
upgrade_clone_complete("POST", SessionID, Env, Body) ->
    ?LOG_INFO([{sessionID, SessionID},
	       {env, Env},
	       {body, Body}]),
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    swmvBackup:remove_prepared_init_config(),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
upgrade_clone_complete(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Help Functions
%% ###-----------------------------------------------------------------------###

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###

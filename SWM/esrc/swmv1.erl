%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmv1.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R9A/R10A/R11A/R12A/4

%% @doc == REST API for SWM, version 1 ==
%% Interface module for REST API, specified in swmWeb.xml.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmv1).
-vsn('/main/R9A/R10A/R11A/R12A/4').
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
%% R9A/1   2017-01-26 etxberb  Created.
%% R9A/2   2017-01-29 etxberb  Implemented create_backup partly.
%% R9A/3   2017-02-02 etxberb  Implemented create_backup and export_backup.
%% R9A/4   2017-02-02 etxberb  Added confirm/3 & monitor_upgrade.
%% R9A/5   2017-02-06 etxberb  Completed confirm/3.
%% R9A/8   2017-02-10 etxberb  Completed monitor_upgrade/1.
%% R9A/9   2017-02-13 etxberb  Removed port 9998 from calls to SVNFM.
%% R9A/11  2017-03-01 etxberb  VNFM IP address always from vnfcI.
%% R9A/13  2017-03-13 etxberb  Added ?FILE_upgrade_ongoing &
%%                             monitor_upgrade_failure/1.
%% R9A/14  2017-04-13 etxberb  Added call to comsaServDiscServer:stop_gRpc.
%% ----    ---------- -------  -------------------------------------------------
%% R10A/1  2017-05-03 etxberb  Added delete_backup/3.
%% R10A/2  2017-05-08 etxberb  Added comsaI:stop_configuration.
%% R10A/3  2017-05-08 etxberb  Added start_configuration/3.
%% R10A/4  2017-06-21 etxberb  Moved common code to swmREST.hrl & swmREST.erl.
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-10-01 etxberb  SP531: Added calls to alhI.
%% R11A/3  2017-10-17 etxpejn  Added calls to upgrade appl in start_configuration,
%%                             start_traffic, stop_configuration & stop_traffic
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/2  2017-11-08 etxpejn  Corr HW41191 send IP trap in start_configuration
%% R12A/3  2017-12-01 etxberb  Refactoring.
%% R12A/4  2017-12-06 etxberb  Refactoring.
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
-export([delete_backup/3,
	 cancel/3,
	 confirm/3,
	 create_backup/3,
	 export_backup/3,
	 start_configuration/3,
	 start_traffic/3,
	 stop_configuration/3,
	 stop_traffic/3]).

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
%% @doc REST query for deleting a backup.
%%
%% @end
%% ###=======================================================================###
-spec delete_backup(SessionID :: any(),
		    Env       :: list({Property :: any(), Value :: any()}),
		    Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
delete_backup(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(delete_backup).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
delete_backup("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    BackupName = swmREST:maps_get(?JsonAttrName_BackupName, Attrs),
    ActionId = swmLib:get_new_action_id(mgr),
    true =
	{error, timeout} /=
	swmvUpgrade:delete_backup(Attrs#{action_id => ActionId,
					 backup_name => BackupName}),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
delete_backup(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query for cancellation.
%%
%% @end
%% ###=======================================================================###
-spec cancel(SessionID :: any(),
	     Env       :: list({Property :: any(), Value :: any()}),
	     Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cancel(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(cancel).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cancel("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    ActionId = swmREST:maps_get(?JsonAttrName_ActionId, Attrs),
    swmvUpgrade:cancel(Attrs#{action_id => ActionId}),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
cancel(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query for upgrade confirm.
%%
%% @end
%% ###=======================================================================###
-spec confirm(SessionID :: any(),
	      Env       :: list({Property :: any(), Value :: any()}),
	      Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
confirm(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(confirm).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
confirm("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    swmvUpgrade:confirm(Attrs),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
confirm(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query for creating a backup.
%%
%% @end
%% ###=======================================================================###
-spec create_backup(SessionID :: any(),
		    Env       :: list({Property :: any(), Value :: any()}),
		    Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
create_backup(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(create_backup).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
create_backup("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    ActionId = swmLib:get_new_action_id(mgr),
    BackupName = swmREST:maps_get(?JsonAttrName_BackupName, Attrs),
    swmvUpgrade:create_backup(Attrs#{env => Env,
				     action_id => ActionId,
				     backup_name => BackupName}),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([{action_id, ActionId}]));
create_backup(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query for exporting a backup.
%%
%% @end
%% ###=======================================================================###
-spec export_backup(SessionID :: any(),
		    Env       :: list({Property :: any(), Value :: any()}),
		    Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export_backup(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(export_backup).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export_backup("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    BackupName = swmREST:maps_get(?JsonAttrName_BackupName, Attrs),
    Uri = swmREST:maps_get(?JsonAttrName_Url, Attrs),
    Pwd = swmREST:maps_get_optional(?JsonAttrName_Pwd, Attrs),
    BuKey = swmvBackup:get_backup_id_by_name(BackupName),
    ActionId = swmLib:get_new_action_id(BuKey),
    swmvUpgrade:create_backup(Attrs#{env => Env,
				     action_id => ActionId,
				     backup_name => BackupName,
				     uri => Uri,
				     pwd => Pwd}),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([{action_id, ActionId}]));
export_backup(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to start (enable) configuration. The upgrade action has been
%%   aborted.
%%
%% @end
%% ###=======================================================================###
-spec start_configuration(SessionID :: any(),
			  Env       :: list({Property :: any(), Value ::any()}),
			  Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_configuration(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(start_configuration).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_configuration("POST", SessionID, Env, _Body) ->
    ?LOG_RestQuery([]),
    swmvUpgrade:stop({ok, start_configuration}),
    
    %% HW41191 No restart of VNF will happen due to failed upgrade
    %% send trap to ENM to be sure that the VNF IP is corrected
    aicI:send_change_ip_address_trap(),

    comsaI:start_configuration(),

    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} =
	swmREST:send_cs_trigger(CbModules, start_configuration, "", ok),
    case {{CSAnswer, CSInfo}, swmREST:send_appl_trigger("configurationStart")} of
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
start_configuration(_, SessionID, Env, Body) ->
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
start_traffic("POST", SessionID, Env, _Body) ->
    ?LOG_RestQuery([]),
   
    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} =
	swmREST:send_cs_trigger(CbModules, start_traffic, "", ok),
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
%% @doc REST query to stop (disable) configuration. This is the starting point
%%   of an upgrade action.
%%
%% @end
%% ###=======================================================================###
-spec stop_configuration(SessionID :: any(),
			 Env       :: list({Property :: any(), Value :: any()}),
			 Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop_configuration(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(stop_configuration).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop_configuration("POST", SessionID, Env, _Body) ->
    ?LOG_RestQuery([]),
    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} =
	swmREST:send_cs_trigger(CbModules, stop_configuration, "", ok),
    case {{CSAnswer, CSInfo}, swmREST:send_appl_trigger("configurationStop")} of
	{{ok, ""}, {ok, ""}} ->
	    swmvUpgrade:start(#{trigger => vnfm_action}),
	    comsaI:stop_configuration("Software upgrade in progress."),
	    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
	{{ok, _Msg1}, {ok, _Msg2}} ->
	    swmvUpgrade:start(#{trigger => vnfm_action}),
	    comsaI:stop_configuration("Software upgrade in progress."),
	    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
	{{_Ans1, Msg1}, {_Ans2, Msg2}} ->
	    %% One or both of the the answeres are error
	    swmREST:deliver_error(SessionID, Env,
				  500,
				  Msg1 ++ Msg2)
    end;
stop_configuration(_, SessionID, Env, Body) ->
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
stop_traffic("POST", SessionID, Env, _Body) ->
    ?LOG_RestQuery([]),
    Res = comsaServDiscServer:stop_gRpc(),   % TODO: Temporary action until
						% permanent solution in place.
    ?LOG_INFO([{stop_gRpc, Res}]),
     
    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} =
	swmREST:send_cs_trigger(CbModules, stop_traffic, "", ok),
    case {{CSAnswer, CSInfo}, swmREST:send_appl_trigger("trafficStop")} of
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
stop_traffic(_, SessionID, Env, Body) ->
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

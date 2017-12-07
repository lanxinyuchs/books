%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	SwmREST.hrl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R10A/R11A/R12A/5

%% @doc == REST API for SWM ==
%% Common definitions for REST API, specified in swmWeb.xml.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-hrl_vsn('/main/R10A/R11A/R12A/5').
-hrl_date('2017-12-06').
-hrl_author('etxberb').

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:	module.hrl %
%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
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
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-10-01 etxberb  SP531: Added avl attributes.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-11-15 etxberb  Continuation of SP277.
%% R12A/2  2017-11-17 etxberb  Continuation of SP277.
%% R12A/3  2017-11-20 etxberb  Continuation of SP277.
%% R12A/4  2017-11-24 etxberb  Continuation of SP277.
%% R12A/5  2017-12-06 etxberb  Refactoring.
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    DEFINITIONS
%% ###-----------------------------------------------------------------------###
-include("SwmInternal.hrl").

%% ###=======================================================================###
%% # 2.1   DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-define(TIME_DeleteBackup, timer:hms(0, 5, 0)).

%% JSON
-define(JsonAttrName_ActionId, <<"action_id">>).
-define(JsonAttrName_Info, <<"info">>).   % For ProgrRep additionalInfo
-define(JsonAttrName_AvlEvent, <<"avl_event">>).
-define(JsonAttrName_BackupName, <<"backup_name">>).
-define(JsonAttrName_Cause, <<"cause">>).
-define(JsonAttrName_OperationResult, <<"operation_result">>).
-define(JsonAttrName_Pwd, <<"password">>).
-define(JsonAttrName_Result, <<"result">>).
-define(JsonAttrName_ResultInfo, <<"result_info">>).   % For ProgrRep resultInfo
-define(JsonAttrName_State, <<"state">>).
-define(JsonAttrName_StopTime, <<"stop_time">>).
-define(JsonAttrName_UserMessage, <<"userMessage">>).
-define(JsonAttrName_TimeOfAction, <<"time_of_action">>).
-define(JsonAttrName_Url, <<"url">>).

-define(AttrName_ActionId, action_id).
-define(AttrName_Info, info).
-define(AttrName_AvlEvent, avl_event).
-define(AttrName_BackupName, backup_name).
-define(AttrName_Cause, cause).
-define(AttrName_OperationResult, operation_result).
-define(AttrName_Pwd, password).
-define(AttrName_Result, result).
-define(AttrName_ResultInfo, result_info).
-define(AttrName_State, state).
-define(AttrName_StopTime, stop_time).
-define(AttrName_UserMessage, userMessage).
-define(AttrName_TimeOfAction, time_of_action).
-define(AttrName_Url, url).

-define(JsonDescr(__Arg), swmREST:ensureStr(__Arg)).
-define(JsonDescr_BadArg(__Arg),
	"Badly formatted argument: " ++ swmREST:ensureStr(__Arg)).
-define(JsonDescr_MandArgMissing(__Arg),
	"Mandatory argument missing: " ++ swmREST:ensureStr(__Arg)).
-define(JsonDescr_ReqMethNotSupported, "Request method not supported").
-define(JsonDescr_SwErr, "Software error").

-define(JsonEndOfHeader, "\r\n\r\n").

-define(JsonResult_Err(__Code, __Descr),
	jsone:encode([{error, [{code, __Code},
			       {type,
				swmREST:ensureBin(swmREST:ensureStr(__Code) ++
					  " " ++
					  httpd_util:reason_phrase(__Code))},
			       {description, swmREST:ensureBin(__Descr)}]}],
		     [{space, 1}, {indent, 4}, {object_key_type, value}])).
-define(JsonResult_Props(__Props),
	jsone:encode({__Props},
		     [{space, 1}, {indent, 4}, {object_key_type, value}])).

-define(VnfmLcmV1, "v1/").
-define(VnfmLcmV2, "v2/").
-define(VnfmLcmUri(__IpAddr, __V, __Query),
    "https://" ++ __IpAddr ++ ":4443/eri_lcm_mani/" ++ __V ++ __Query).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% gen_statem
-define(StateM_NAME, ?MODULE).
-define(CURRENT_STATE, ?FUNCTION).

-define(EVT(__Evt, __EvtData, __InData),
	__Data = swmREST:stateM_evt_data(?CURRENT_STATE,
					 __Evt,
					 __EvtData,
					 __InData),
	try
	    ?LOG_INFO([{evt, __Evt},
		       {evtData, __EvtData},
		       {data, __Data}]),
	    ?MODULE:__Evt(__EvtData, __Data)
	catch
	    throw : __ThrowR ->
		__ThrowST = ?STACKTRACE_E,
		swmREST:stateM_evt_exc(#{excClass      => throw,
					 excReason     => __ThrowR,
					 evt           => __Evt,
					 evtData       => __EvtData,
					 data          => __Data,
					 stateM_module => ?MODULE,
					 stacktrace    => __ThrowST});
	    __EC : __ER ->   % ExceptionClass : ExceptionReason
		__ST = ?STACKTRACE_E,
		__ExcData = #{excClass      => __EC,
			      excReason     => __ER,
			      evt           => __Evt,
			      evtData       => __EvtData,
			      data          => __Data,
			      stateM_module => ?MODULE},
		?LOG_ERR([__ExcData, "--- Stacktrace ---" | __ST]),
		swmREST:stateM_evt_exc(__ExcData)
	end).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% General interface actions
-define(LOG_RestQuery(__InfoList), ?LOG_INFO(["REST Query" | __InfoList])).
-define(LOG_Unexpected(__Reason),
	?LOG_WARN([{"REST Query", "Unexpected"},
		   __Reason,
		   {sessionID, SessionID},
		   {env, Env},
		   {body, Body}])).

-define(TRY_REST_QUERY(__Function),
	try
	    __Function(swmREST:env_req(Env), SessionID, Env, Body)
	catch
	    throw : {fail, Reason} ->
		JsonDescr = ?JsonDescr(Reason),
		?LOG_WARN(JsonDescr),
		swmREST:deliver(SessionID, Env, ?JsonResult_Err(500,JsonDescr));
	    throw : {uc_error, JsonDescr} ->
		?LOG_Unexpected(JsonDescr),
		swmREST:deliver(SessionID, Env, ?JsonResult_Err(400,JsonDescr));
	    __ErrClass : __ErrReason ->
		__Stacktrace = ?STACKTRACE_E,
		?LOG_ERR([{sessionID, SessionID},
			  {env, Env},
			  {body, Body},
			  {__ErrClass, __ErrReason},
			  "--- Stacktrace ---"
			  | __Stacktrace]),
		swmREST:deliver(SessionID,
				Env,
				?JsonResult_Err(500, ?JsonDescr_SwErr))
	end).

%% ###=======================================================================###
%% # 2.2   DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.3   DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###


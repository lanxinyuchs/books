%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmREST.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R10A/R11A/R12A/10

%% @doc == REST API for SWM, common ==
%% Common module for all versions of REST API, specified in swmWeb.xml.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmREST).
-vsn('/main/R10A/R11A/R12A/10').
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
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-10-01 etxberb  Added maps_to_propslist/1.
%% R11A/2  2017-10-17 etxpejn  Added deliver_error/4, send_appl_trigger/1,
%%                             and send_cs_trigger/4
%% R11A/3  2017-10-20 etxberb  Added msg2vnfm_swRestore/1, vnfd_id/0.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/2  2017-10-30 etxpejn  Temp removed check for brmBackup in
%%                             actionResult_Json
%% R12A/3  2017-11-01 etxpejn  Temp added mo_function fnk to handle call with
%%                             UP Key
%% R12A/5  2017-11-17 etxberb  Continuation of SP277.
%% R12A/6  2017-11-20 etxberb  Continuation of SP277.
%% R12A/7  2017-11-24 etxberb  Continuation of SP277.
%% R12A/8  2017-11-28 etxberb  SP277: Cancel bug fix.
%% R12A/9  2017-12-01 etxberb  Refactoring.
%% R12A/10 2017-12-06 etxberb  Refactoring.
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

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% General Support Functions
-export([actionResult_Json/1,
	 decode/1,
	 deliver/3,
	 deliver_error/4,
	 ensureBin/1,
	 ensureStr/1,
	 env_req/1,
	 maps_get/2,
	 maps_get_optional/2,
	 maps_to_propslist/1,
	 msg2vnfm_send/1,
	 msg2vnfm_send/3,
	 msg2vnfm_send/5,
	 send_appl_trigger/1,
	 send_cs_trigger/4,
	 stateM_evt_data/4,
	 stateM_evt_exc/1,
	 vnfm_ip/0,
	 vnf_id/0,
	 vnfd_id/0]).

%% MO
-export([mo_is_finished/1,
	 mo_is_finished/2,
	 mo_format/1,
	 mo_format/2]).

%% Messages to the VNFM / LCM
-export([msg2vnfm_backup_status/1,
	 msg2vnfm_getRestoreStatus/1,
	 msg2vnfm_swRestore_cancel/1,
	 msg2vnfm_swRestore_confirm/1,
	 msg2vnfm_swRestore_confirm_force/1,
	 msg2vnfm_swRestore_postCheckStatus/1,
	 msg2vnfm_swRestore_start/1,
	 msg2vnfm_swRestore_status/1,
	 msg2vnfm_upgrade_status/1]).

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###
-include("SwmREST.hrl").
-include("RcsSwM.hrl").
-include("RcsBrM.hrl").
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

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.2.1 General Support Functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% actionResult_Json
%%
%% ###=======================================================================###
actionResult_Json(#{timeActionCompleted := TActionCompl,
		    result := StatusCode,
		    resultInfo := StatusInfo} = Result) ->
    [{vnf_time, ensureBin(TActionCompl)},
     {status, [{status_code, ensureBin(StatusCode)},
	       {info, ensureBin(StatusInfo)}]}
     | actionResult_Json_opts(Result)];
actionResult_Json(#{action_result := ActionResult} = Result) ->
    TActionCompl = proplists:get_value(timeActionCompleted, ActionResult, ""),
    StatusCode = proplists:get_value(result, ActionResult, ""),
    StatusInfo = proplists:get_value(resultInfo, ActionResult, ""),
    actionResult_Json(Result#{timeActionCompleted => TActionCompl,
			      result => StatusCode,
			      resultInfo => StatusInfo});
actionResult_Json(ActionResult) when is_list(ActionResult) ->
    actionResult_Json(#{action_result => ActionResult}).

%% ###=======================================================================###
actionResult_Json_opts(#{} = Result) ->
    Keys =
	maps:keys(Result) --
	[action_result, timeActionCompleted, result, resultInfo],
    [{Key, ensureBin(maps:get(Key, Result))} || Key <- Keys].

%% #############################################################################
%% decode
%%
%% ###=======================================================================###
decode([]) ->
    [];
decode(Body) when is_list(Body) ->
    decode(erlang:list_to_binary(Body));
decode(Body) ->
    jsone:decode(Body).

%% #############################################################################
%% deliver
%%
%% ###=======================================================================###
%%deliver(SessionID, Body) ->
%%    mod_esi:deliver(SessionID, [Body, "\n"]).

%% #############################################################################
deliver(SessionID, Env, Body) ->
    Head =
	"Content-Type: " ++
	proplists:get_value(http_content_type, Env) ++   % "application/json"
	?JsonEndOfHeader,
    mod_esi:deliver(SessionID, [Head, Body, "\n"]).

deliver_error(SessionID, Env, Status, Body) ->
    Request = proplists:get_value(script_name, Env),
    sysInitI:warning_report([{?MODULE, deliver_error}, {request, Request}, {error, Body}]),

    mod_esi:deliver(SessionID, ["status:"++integer_to_list(Status)++" Error"++?JsonEndOfHeader]),
    mod_esi:deliver(SessionID, Body).     

%% #############################################################################
%% ensureBin
%%
%% ###=======================================================================###
ensureBin(Term) when is_binary(Term) ->
    Term;
ensureBin(Term) ->
    list_to_binary(ensureStr(Term)).

%% #############################################################################
%% ensureStr
%%
%% ###=======================================================================###
ensureStr(Term) ->
    catch sysUtil:term_to_string(Term).

%% #############################################################################
%% env_req
%%
%% ###=======================================================================###
env_req(Env) ->
    proplists:get_value(request_method, Env, "").

%% #############################################################################
%% format_stringsList
%%
%% ###=======================================================================###
format_stringsList([E | _] = SL) when is_list(E) orelse
				      is_binary(E) ->
    SL;
format_stringsList([E | _] = String) when is_integer(E) ->
    [String];
format_stringsList([]) ->
    [].

%% #############################################################################
%% format_value
%%
%% ###=======================================================================###
format_value(Map) when is_map(Map) ->
    maps_to_propslist(Map);
format_value(Val) ->
    ensureStr(Val).

%% #############################################################################
%% maps_get
%%
%% ###=======================================================================###
maps_get(Key, [])->
    throw({uc_error, ?JsonDescr_MandArgMissing(Key)});
maps_get(Key, Map)->
    try
	Value = maps:get(Key, Map),
	ensureStr(Value)
    catch
	error : {badmap, BadMap} ->
	    throw({uc_error, ?JsonDescr_BadArg(BadMap)});
	error : {badkey, _} ->
	    throw({uc_error, ?JsonDescr_MandArgMissing(Key)})
    end.

%% #############################################################################
%% maps_get_optional
%%
%% ###=======================================================================###
maps_get_optional(Key, Map) ->
    format_value(maps:get(Key, Map, [])).

%% #############################################################################
%% maps_to_propslist
%%
%% ###=======================================================================###
maps_to_propslist(Map) when is_map(Map) ->
    [{list_to_atom(ensureStr(Key)), maps_to_propslist(Val)}
      || {Key, Val} <- maps:to_list(Map)];
maps_to_propslist(Value) ->
    ensureStr(Value).

%% #############################################################################
%% msg2vnfm_send
%%
%% ###=======================================================================###
msg2vnfm_send(Request, JsonUri, BodyProps) ->
    msg2vnfm_send(Request, JsonUri, [], "application/json", BodyProps).

%% ###=======================================================================###
msg2vnfm_send(Request, JsonUri, Headers, ContentType, BodyProps) ->
    Body = ?JsonResult_Props(BodyProps),
    msg2vnfm_send(Request#{data => {JsonUri, Headers, ContentType, Body}}).

%% ###=======================================================================###
msg2vnfm_send(Req) ->
    Request = maps:merge(#{verbose_send => false,
			   verbose_receive => false},
			 Req),
    ?LOG_INFO(["======= Request =======" | maps:to_list(Request)]),
    try vnfcHttps:send_receive(Request) of
	#{body := Body} = Result ->
	    ?LOG_INFO(["======= Result ======="
		       | maps:to_list(Result#{body => decode_body(Body)})]),
	    Result;
	Result ->
	    ?LOG_INFO(["======= Result =======", Result]),
	    Result
    catch
	error : undef ->
	    false
    end.

%% #############################################################################
%% send_appl_trigger
%%
%% ###=======================================================================###
send_appl_trigger(AppmIAction) ->
    case appmI:call_upi(AppmIAction) of
	{ok, []} ->
	    swmLib:write_swm_log("SwM",
				 info,
				 "All participants ok " ++ AppmIAction),
	    {ok, ""};
	{ok, Responses} ->
	    make_appl_response_list(Responses, "", ok);
	{error, Reasons} ->
	    swmLib:write_swm_log("SwM", error, "Participants not ok"),
	    {error, Reasons}	
    end.

make_appl_response_list([], List, TotResult) ->
    {TotResult, List};
make_appl_response_list([{{LmName, ProdNr}, Result} | Rest], List, TotResult) ->
    Msg = LmName++"("++ProdNr++"): "++Result--"\n",
    case Result of
	"ERROR: "++_ ->
	    swmLib:write_swm_log("SwM", error, Msg),
	    make_appl_response_list(Rest,
				    List ++ [list_to_binary(Msg++" ")],
				    error);
	"CAUTION: "++_ ->
	    swmLib:write_swm_log("SwM", warning, Msg),
	    make_appl_response_list(Rest,
				    List ++ [list_to_binary(Msg++" ")],
				    TotResult);
	"OK"++_ ->
	    swmLib:write_swm_log("SwM", info, Msg),
	    make_appl_response_list(Rest,
				    List ++ [list_to_binary(Msg++" ")],
				    TotResult);
	_Else ->
	    swmLib:write_swm_log("SwM", warning, Msg),
	    ?LOG_WARN(["Unknown result",
		       {"upgrade trigger", Msg}]),
	    make_appl_response_list(Rest,
				    List ++ [list_to_binary(Msg)],
				    TotResult)
    end.

%% #############################################################################
%% send_cs_trigger
%%
%% ###=======================================================================###
send_cs_trigger([], _Action, InfoList, Answer) ->
    {Answer, InfoList};
send_cs_trigger([CbModule| RestCbModules], Action, InfoList, Answer) ->
    case swmServer:has_callback(CbModule, Action) of
	true ->
	    sysInitI:info_msg("~w: Calling ~w:~w()~n",
			      [?MODULE, CbModule, Action]),
	    try apply(CbModule, Action, []) of
		ok ->
		    send_cs_trigger(RestCbModules, Action, InfoList, Answer);
		{ok, Msg} ->
		    swmLib:write_swm_log("SwM", warning, Msg),
		    send_cs_trigger(RestCbModules, Action, InfoList 
				    ++ [list_to_binary(Msg)], Answer);
		{error, Msg} ->
		    swmLib:write_swm_log("SwM", error, Msg),
		    send_cs_trigger(RestCbModules, Action, InfoList 
				    ++ [list_to_binary(Msg)], error)
	    catch _T:_E ->
		    Msg = "Action "++atom_to_list(Action)++
			" failed on a software error",
		    swmLib:write_swm_log("SwM", error, Msg),
		    ?LOG_WARN(["Unknown result",
			       {"upgrade trigger", Msg}]),
		    send_cs_trigger(RestCbModules, Action, InfoList 
				    ++ [list_to_binary(Msg)], error)
	    end;
	false ->
	    send_cs_trigger(RestCbModules, Action, InfoList, Answer)
    end.

%% #############################################################################
%% stateM_evt_data
%%
%% ###=======================================================================###
stateM_evt_data(CurrState, Evt, EvtData, Data) ->
    try
	stateM_evt_data(Evt, EvtData, Data#{state => CurrState})
    catch
	EC : ER ->
	    ST = ?STACKTRACE_E,
	    ?LOG_ERR([{currState, CurrState},
		      {evt, Evt},
		      {vtData, EvtData},
		      {data, Data},
		      {EC, ER},
		      "--- Stacktrace ---"
		      | ST]),
	    Data#{state => CurrState}
    end.

%% ###=======================================================================###
stateM_evt_data(Evt, _EvtData, #{state := State} = Data) ->
    LogData = {State, Evt},
    case maps:get(evtLogFirst, Data, []) of
	EvtLogFirst when length(EvtLogFirst) < 3 ->
	    Data#{evtLogFirst => EvtLogFirst ++ [LogData]};
	_ ->
	    case maps:get(evtLogLatest, Data, []) of
		EvtLogLatest when length(EvtLogLatest) < 8 ->
		    Data#{evtLogLatest => EvtLogLatest ++ [LogData]};
		['...', _Drop | EvtLogLatestTail] ->
		    Data#{evtLogLatest =>
			  ['...' | EvtLogLatestTail] ++ [LogData]};
		[_Drop | EvtLogLatestTail] ->
		    Data#{evtLogLatest =>
			  ['...' | EvtLogLatestTail] ++ [LogData]}
	    end
    end.

%% #############################################################################
%% stateM_evt_exc
%%
%% ###=======================================================================###
stateM_evt_exc(#{excClass := throw,
		 stateM_module := M} = ExcData) ->
    try
	M:stateM_evt_exc(ExcData)
    catch
	error : undef ->
	    keep_state_and_data
    end;
stateM_evt_exc(#{excClass := EC,
		 excReason := ER,
		 stateM_module := M} = ExcData) ->
    try
	M:stateM_evt_exc(ExcData)
    catch
	error : undef ->
	    ok
    end,
    erlang:EC(ER).

%% #############################################################################
%% vnfm_ip
%%
%% ###=======================================================================###
vnfm_ip() ->
    swmLib:vnfm_ip().

%% #############################################################################
%% vnf_id
%%
%% ###=======================================================================###
vnf_id() ->
    ensureBin([_ | _] = swmLib:vnf_id()).

%% #############################################################################
%% vnfd_id
%%
%% ###=======================================================================###
vnfd_id() ->
    ensureBin([_ | _] = swmLib:vnfd_id()).

%% ###-----------------------------------------------------------------------###
%% # 3.2.2 Managed Object
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% mo_format
%%
%% ###=======================================================================###
mo_format({"1", "1", "1", _UP} = Key) ->
    mo_format(upgradePackage, Key);
mo_format(Obj) ->
    mo_format(element(1, Obj), Obj).

%% ###=======================================================================###
mo_format(upgradePackage, #upgradePackage{reportProgress = RepPro} = Rec) ->
    RecInfo = record_info(fields, upgradePackage),
    PrettyRec = elems2string(Rec),
    [_ | PrettyRecElems] = tuple_to_list(PrettyRec),
    case mo_format(reportProgress, RepPro) of
	[] ->
	    RepProFormat = undefined;
	RepProFormat ->
	    ok
    end,
    lists:keyreplace(reportProgress,
		     1,
		     sysUtil:pairElems(RecInfo, PrettyRecElems),
		     {reportProgress, RepProFormat});
mo_format(brmBackup, #brmBackup{progressReport = RepPro} = Rec) ->
    RecInfo = record_info(fields, brmBackup),
    PrettyRec = elems2string(Rec),
    [_ | PrettyRecElems] = tuple_to_list(PrettyRec),
    case mo_format(progressReport, RepPro) of
	[] ->
	    RepProFormat = undefined;
	RepProFormat ->
	    ok
    end,
    lists:keyreplace(progressReport,
		     1,
		     sysUtil:pairElems(RecInfo, PrettyRecElems),
		     {progressReport, RepProFormat});
mo_format(reportProgress, #upgradePackage{reportProgress = Rec}) ->
    mo_format(reportProgress, Rec);
mo_format(progressReport, #brmBackup{progressReport = Rec}) ->
    mo_format(progressReport, Rec);
mo_format(_, #'AsyncActionProgressWithSteps'{} = Rec) ->
    RecInfo = record_info(fields, 'AsyncActionProgressWithSteps'),
    PrettyRec = elems2string(Rec),
    [_ | PrettyRecElems] = tuple_to_list(PrettyRec),
    sysUtil:pairElems(RecInfo, PrettyRecElems);
mo_format(_, #'AsyncActionProgress'{} = Rec) ->
    RecInfo = record_info(fields, 'AsyncActionProgress'),
    PrettyRec = elems2string(Rec),
    [_ | PrettyRecElems] = tuple_to_list(PrettyRec),
    sysUtil:pairElems(RecInfo, PrettyRecElems);
mo_format(_, undefined) ->
    [];
mo_format(Parameter, Key) ->
    case mnesia:dirty_read({upgradePackage, Key}) of
	[Obj] ->
	    mo_format(Parameter, Obj);
	_ ->
	    case mnesia:dirty_read({brmBackup, Key}) of
		[Obj] ->
		    mo_format(Parameter, Obj);
		_ ->
		    Stacktrace = ?STACKTRACE_C,
		    sysInitI:warning_report([{"Unrecognized object", Key},
					     {parameter, Parameter}
					     | Stacktrace]),
		    []
	    end
    end.

%% #############################################################################
%% mo_is_finished
%%
%% ###=======================================================================###
mo_is_finished(#'AsyncActionProgress'{state = ?ActionStateType_FINISHED}) ->
    true;
mo_is_finished(#'AsyncActionProgressWithSteps'{state =
					       ?ActionStateType_FINISHED}) ->
    true;
mo_is_finished(#'AsyncActionProgress'{}) ->
    false;
mo_is_finished(#'AsyncActionProgressWithSteps'{}) ->
    false;
mo_is_finished(Key) ->
    case mnesia:dirty_read({upgradePackage, Key}) of
	[Obj] ->
	    mo_is_finished(Key, Obj);
	_ ->
	    sysInitI:warning_report([{"Not found", Key}]),
	    false
    end.

%% ###=======================================================================###
mo_is_finished(Key, #upgradePackage{upgradePackageId = Key,
				    reportProgress = RepProgress}) ->
    mo_is_finished(RepProgress);
mo_is_finished(_, _) ->
    false.

%% ###-----------------------------------------------------------------------###
%% # 3.2.3 Messages to VNFM / LCM
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% msg2vnfm_backup_status
%%
%% ###=======================================================================###
msg2vnfm_backup_status(#{env := Env,
			 action_id := ActionId,
			 action_result := ActionResult} = UsrMap) ->
    ?LOG_INFO(ActionResult),
    Headers = [],
    ContentType = proplists:get_value(http_content_type, Env),
    Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV1, "backup_status"),
		      Headers,
		      ContentType,
		      [{vnf_id, vnf_id()},
		       {action_id, ActionId}
		       | actionResult_Json(ActionResult)]),
    maps:merge(UsrMap, Httpc).

%% #############################################################################
%% msg2vnfm_getRestoreStatus
%%
%% ###=======================================================================###
msg2vnfm_getRestoreStatus(UsrMap) ->
    #{status_code := StatusCode, body := RetBody} = Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV2, "restore_status_info"),
		      [{vnf_id, vnf_id()}]),
    case StatusCode of
	200 ->
	    RetMap = decode(binary_to_list(RetBody)),
	    ?LOG_INFO([RetMap]),
	    State      = maps_get_optional(?JsonAttrName_State, RetMap),
	    OperResult = maps_get_optional(?JsonAttrName_OperationResult,RetMap),
	    Result = proplists:get_value(?AttrName_Result, OperResult),
	    Info   = proplists:get_value(?AttrName_Info, OperResult, []),
	    maps:merge(UsrMap, Httpc#{state => State,
				      result => Result,
				      info => Info});
	404 ->   % "Object Not Found"
	    ?LOG_ERR(["VnfId unknown", {httpc, Httpc}]),
	    maps:merge(UsrMap, Httpc);
	_ ->
	    ?LOG_ERR([{httpc, Httpc}]),
	    maps:merge(UsrMap, Httpc)
    end.

%% #############################################################################
%% msg2vnfm_swRestore_cancel
%%
%% ###=======================================================================###
msg2vnfm_swRestore_cancel(#{info := Info} = UsrMap) ->
    Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV2, "restore_cancel"),
		      [{vnf_id, vnf_id()},
		       {info, strings_array(format_stringsList(Info))}]),
    maps:merge(UsrMap, Httpc).

%% #############################################################################
%% msg2vnfm_swRestore_confirm
%%
%% ###=======================================================================###
msg2vnfm_swRestore_confirm(UsrMap) ->
    Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV2, "restore_confirm"),
		      [{vnf_id, vnf_id()}]),
    maps:merge(UsrMap, Httpc).

%% #############################################################################
%% msg2vnfm_swRestore_confirm_force
%%
%% ###=======================================================================###
msg2vnfm_swRestore_confirm_force(UsrMap) ->
    Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV2,"restore_confirm_force"),
		      [{vnf_id, vnf_id()}]),
    maps:merge(UsrMap, Httpc).

%% #############################################################################
%% msg2vnfm_swRestore_postCheckStatus
%%
%% ###=======================================================================###
msg2vnfm_swRestore_postCheckStatus(#{result := Result, info := Info} = UsrMap) ->
    Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV2, "post_check_status"),
		      [{vnf_id, vnf_id()},
		       {result, ensureBin(Result)},
		       {info, strings_array(format_stringsList(Info))}]),
    maps:merge(UsrMap, Httpc).

%% #############################################################################
%% msg2vnfm_swRestore_start
%%
%% ###=======================================================================###
msg2vnfm_swRestore_start(#{vnfd_id := VnfdId, fallback_timeout := FT} = UsrMap)->
    #{body := RetBody} = Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV2, "restore_start"),
		      [{vnf_id, vnf_id()},
		       {vnfd_id, ensureBin(VnfdId)},
		       {fallback_timeout, FT}]),
    RetBodyAttrs = swmREST:decode(RetBody),
    UserMessage =
	swmREST:maps_get_optional(?JsonAttrName_UserMessage, RetBodyAttrs),
    maps:merge(UsrMap, Httpc#{?AttrName_UserMessage => UserMessage}).

%% #############################################################################
%% msg2vnfm_swRestore_status
%%
%% ###=======================================================================###
msg2vnfm_swRestore_status(#{brm_obj := BrmObj} = UsrMap) ->
    BrmResult = restore_state2result(BrmObj),
    ?LOG_INFO(BrmResult),
    Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV2, "restore_status"),
		      [{vnf_id, vnf_id()} | actionResult_Json(BrmResult)]),
    maps:merge(UsrMap, Httpc).

%% #############################################################################
%% msg2vnfm_upgrade_status
%%
%% ###=======================================================================###
msg2vnfm_upgrade_status(#{action_result := ActionResult} = UsrMap) ->
    ?LOG_INFO(ActionResult),
    Httpc =
	msg2vnfm_send(#{method => post, return => httpc},
		      ?VnfmLcmUri(vnfm_ip(), ?VnfmLcmV1, "upgrade_status"),
		      [{vnf_id, vnf_id()}
		       | actionResult_Json(ActionResult)]),
    maps:merge(UsrMap, Httpc).

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Help Functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% actionResultType
%%
%% ###=======================================================================###
actionResultType(?ActionResultType_SUCCESS) ->
    "SUCCESS";
actionResultType(?ActionResultType_FAILURE) ->
    "FAILURE";
actionResultType(?ActionResultType_NOT_AVAILABLE) ->
    "NOT_AVAILABLE";
actionResultType(Other) ->
    sysUtil:term_to_string(Other).

%% #############################################################################
%% actionStateType
%%
%% ###=======================================================================###
actionStateType(?ActionStateType_CANCELLING) ->
    "CANCELLING";
actionStateType(?ActionStateType_RUNNING) ->
    "RUNNING";
actionStateType(?ActionStateType_FINISHED) ->
    "FINISHED";
actionStateType(?ActionStateType_CANCELLED) ->
    "CANCELLED";
actionStateType(Other) ->
    sysUtil:term_to_string(Other).

%% #############################################################################
%% decode_body
%%
%% ###=======================================================================###
decode_body(Body) ->
    try
	maps:from_list(maps_to_propslist(decode(Body)))
    catch
	_ : _ ->
	    Body
    end.

%% #############################################################################
%% elems2string
%%
%% ###=======================================================================###
elems2string(#upgradePackage{state = State} = Rec) ->
    Rec#upgradePackage{state = stateSwM2str(State)};
elems2string(#brmBackup{status = Status} = Rec) ->
    Rec#brmBackup{status = stateBrM2str(Status)};
elems2string(#'AsyncActionProgress'{result = Result,
				    state = State} = Rec) ->
    Rec#'AsyncActionProgress'{result = actionResultType(Result),
			      state = actionStateType(State)};
elems2string(#'AsyncActionProgressWithSteps'{result = Result,
					     state = State} = Rec) ->
    Rec#'AsyncActionProgressWithSteps'{result = actionResultType(Result),
				       state = actionStateType(State)}.

%% #############################################################################
%% restore_state2result
%%
%% ###=======================================================================###
restore_state2result(BrmObj) ->
    ProgRep = mo_format(progressReport, BrmObj),
    ExpectedState = actionStateType(?ActionStateType_RUNNING),
    case proplists:get_value(state, ProgRep, "") of
	ExpectedState ->
	    Result = {result, ExpectedState},
	    lists:keyreplace(result, 1, ProgRep, Result);
	_ ->
	    FailResult = {result, actionResultType(?ActionResultType_FAILURE)},
	    lists:keyreplace(result, 1, ProgRep, FailResult)
    end.

%% #############################################################################
%% stateBrM2str
%%
%% ###=======================================================================###
stateBrM2str(?BrmBackupStatus_BRM_BACKUP_COMPLETE) -> "COMPLETE";
stateBrM2str(?BrmBackupStatus_BRM_BACKUP_INCOMPLETE) -> "INCOMPLETE";
stateBrM2str(?BrmBackupStatus_BRM_BACKUP_CORRUPTED) -> "CORRUPTED";
stateBrM2str(Other) ->
    sysUtil:term_to_string(Other).

%% #############################################################################
%% stateSwM2str
%%
%% ###=======================================================================###
stateSwM2str(?UpgradePackageState_INITIALIZED) -> "INITIALIZED";
stateSwM2str(?UpgradePackageState_PREPARE_IN_PROGRESS) -> "PREPARE_IN_PROGRESS";
stateSwM2str(?UpgradePackageState_PREPARE_COMPLETED) -> "PREPARE_COMPLETED";
stateSwM2str(?UpgradePackageState_ACTIVATION_IN_PROGRESS) ->
    "ACTIVATION_IN_PROGRESS";
stateSwM2str(?UpgradePackageState_ACTIVATION_STEP_COMPLETED) ->
    "ACTIVATION_STEP_COMPLETED";
stateSwM2str(?UpgradePackageState_WAITING_FOR_COMMIT) -> "WAITING_FOR_COMMIT";
stateSwM2str(?UpgradePackageState_COMMIT_COMPLETED) -> "COMMIT_COMPLETED";
stateSwM2str(?UpgradePackageState_DEACTIVATION_IN_PROGRESS) ->
    "DEACTIVATION_IN_PROGRESS";
stateSwM2str(Other) ->
    sysUtil:term_to_string(Other).

%% #############################################################################
%% strings_array
%%
%% ###=======================================================================###
strings_array([I | Tail]) when is_binary(I) ->
    [I | strings_array(Tail)];
strings_array([I | Tail]) ->
    [list_to_binary(sysUtil:term_to_string(I)) | strings_array(Tail)];
strings_array([]) ->
    [].

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###

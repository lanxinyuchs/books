%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2012. All Rights Reserved.
%% 
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%% 
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_oi_com.erl
%% 
%% Description:
%%    This file the interface that decodes incoming IMM OI requests.
%%
%%--------------------------------------------------------------------
-module(safs_imm_oi_com).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("oi.hrl").
-include("safs_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 message/2,
	 init/2,
	 callback_message_async/5,
	 callback_message_sync/5,
	 close/1,
	 trace_groups/0,
	 trace_points_list/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Test exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(CB_TIMEOUT, 5000).
-define(SA_MAX_NAME_LENGTH, 256).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/2
%% Description:
%%--------------------------------------------------------------------
init(_Proxy, _Cb) ->
    {ok, undefined}.

%%--------------------------------------------------------------------
%% Function: message/2
%% Description:
%%--------------------------------------------------------------------
message(Bytes, State) ->
    case catch oi:decode_msg(Bytes, safsImmOiMessage) of
	#safsImmOiMessage{} = Msg ->
	    {result_msg(Msg), State};
	_Error ->
	    error_logger:format("~p~p couldn't decode message:\n\t~p\n", 
				[?MODULE, self(), Bytes]),
	    {{error, bad_message}, State}
    end. 

%%--------------------------------------------------------------------
%% Function: callback_message_async/5
%% Description:
%%--------------------------------------------------------------------
callback_message_async(Pid, CbMsgNumber, Cb, Handle, Arg) ->
    case encode_callback_msg(CbMsgNumber, Cb, Handle, Arg) of
	{ok, Bin} ->
	    safs_com_inproxy:send(Pid, Bin);
	Error ->
	    Error
    end.
	    
%%--------------------------------------------------------------------
%% Function: callback_message_sync/5
%% Description:
%%--------------------------------------------------------------------
callback_message_sync(Pid, CbMsgNumber, Cb, Handle, CcbId) ->
    case encode_callback_msg(CbMsgNumber, Cb, Handle, CcbId) of
	{ok, Bin} ->
	    Self = self(),
	    ReplyFun = 
		fun(Res) ->
			case decode_callback_reply(Res, CbMsgNumber) of
			    not_my_reply -> 
				not_my_reply;
			    Reply ->
				Self ! {callback_reply, Cb, Handle, CbMsgNumber, Reply}
			end
		end,
	    TimeOut = safs_imm_oi:get_cb_timeout(Cb),
	    case safs_com_inproxy:send_receive(Pid, Bin, ReplyFun) of
		{error, sa_ais_err_bad_handle} ->
		    {error, sa_ais_err_bad_handle};
		ok ->
		    case TimeOut of
			infinity ->
			    {ok, undefined};
			_ ->
			    %% Returns {ok, TRef} |{error, Reason}
			    timer:send_after(TimeOut, {callback_timeout, Cb, CbMsgNumber, Handle}) 
		    end
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: close/1
%% Description:
%%--------------------------------------------------------------------
close(Proxy) ->
    safs_com_inproxy:stop(Proxy).

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [error, message, encoding].

trace_points_list() ->
    [
     {error, 
      [{safs_error, 2}]},
     {message,
      [{message,2},
       {callback_message_async,4},
       {callback_message_sync,4},
       {result_msg,1}]},
     {encoding,
      [{encode_callback_msg,3},
       {decode_callback_reply,1}]}
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description: 
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: result_msg/1
%% Description:
%%--------------------------------------------------------------------
result_msg(#safsImmOiMessage{initialize = Initialize}) 
  when Initialize =/= undefined ->    
    % io:format("OI Initialize message:\n~p\n\n", [Initialize]), 
    Callbacks = Initialize#safsImmOiInitialize.callbacks,  
    RequestedVersion = Initialize#safsImmOiInitialize.version,
    R = initialize_ret(Callbacks, RequestedVersion),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{callbacksInitialize = CbInit}) 
  when CbInit =/= undefined ->    
    % io:format("OI Callback Initialize message:\n~p\n\n", [CbInit]), 
    Handle = CbInit#safsImmOiCallbacksInitialize.handle,
    R = callbacks_initialize_ret(Handle),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{finalize = Finalize}) 
  when Finalize =/= undefined ->
    % io:format("OI Finalize message:\n~p\n\n", [Finalize]), 
    Handle = Finalize#safsImmOiFinalize.handle,
    R = finalize_ret(Handle),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{implementerSet = ImplSet}) 
  when ImplSet =/= undefined ->
    % io:format("OI Implementer Set message:\n~p\n\n", [ImplSet]), 
    Handle = ImplSet#safsImmOiImplementerSet.handle,
    ImplName = ImplSet#safsImmOiImplementerSet.implementerName,
    R = implementer_set_ret(Handle, ImplName),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{implementerClear = ImplClear}) 
  when ImplClear =/= undefined ->
    % io:format("OI Implementer Clear message:\n~p\n\n", [ImplClear]), 
    Handle = ImplClear#safsImmOiImplementerClear.handle,
    R = implementer_clear_ret(Handle),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{classImplementerSet = ClassImplSet}) 
  when ClassImplSet =/= undefined ->
    % io:format("OI Class Implementer Set message:\n~p\n\n", [ClassImplSet]), 
    Handle = ClassImplSet#safsImmOiClassImplementerSet.handle,
    ClassName = ClassImplSet#safsImmOiClassImplementerSet.className,
    R = class_implementer_set_ret(Handle, ClassName),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{classImplementerRelease = ClassImplRel}) 
  when ClassImplRel =/= undefined ->
    % io:format("OI Class Implementer Release message:\n~p\n\n", [ClassImplRel]), 
    Handle = ClassImplRel#safsImmOiClassImplementerRelease.handle,
    ClassName = ClassImplRel#safsImmOiClassImplementerRelease.className,
    R = class_implementer_release_ret(Handle, ClassName),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{objectImplementerSet = ObjImplSet}) 
  when ObjImplSet =/= undefined ->
    % io:format("OI Object Implementer Set message:\n~p\n\n", [ObjImplSet]),
    Handle = ObjImplSet#safsImmOiObjectImplementerSet.handle,
    ObjectName = ObjImplSet#safsImmOiObjectImplementerSet.objectName,
    Scope = ObjImplSet#safsImmOiObjectImplementerSet.scope,
    R = object_implementer_set_ret(Handle, ObjectName, Scope),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{objectImplementerRelease = ObjImplRel}) 
  when ObjImplRel =/= undefined ->
    % io:format("OI Object Implementer Release message:\n~p\n\n", 
    % 	      [ObjImplRel]), 
    Handle = ObjImplRel#safsImmOiObjectImplementerRelease.handle,
    ObjectName = ObjImplRel#safsImmOiObjectImplementerRelease.objectName,
    Scope = ObjImplRel#safsImmOiObjectImplementerRelease.scope,
    R = object_implementer_release_ret(Handle, ObjectName, Scope),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{rtObjectCreate_2 = RtObjCreate}) 
  when RtObjCreate =/= undefined ->
    Handle = RtObjCreate#safsImmOiRtObjectCreate_2.handle,
    ClassName = RtObjCreate#safsImmOiRtObjectCreate_2.className,
    ParentName = RtObjCreate#safsImmOiRtObjectCreate_2.parentName,
    AttrValues = RtObjCreate#safsImmOiRtObjectCreate_2.attrValues,
    R = rt_object_create_2_ret(Handle, ClassName, ParentName, AttrValues),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{rtObjectDelete = RtObjDelete}) 
  when RtObjDelete =/= undefined ->
    Handle = RtObjDelete#safsImmOiRtObjectDelete.handle,
    ObjectName = RtObjDelete#safsImmOiRtObjectDelete.objectName,
    R = rt_object_delete_ret(Handle, ObjectName),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{rtObjectUpdate_2 = RtObjUpdate}) 
  when RtObjUpdate =/= undefined ->
    Handle = RtObjUpdate#safsImmOiRtObjectUpdate_2.handle,
    ObjectName = RtObjUpdate#safsImmOiRtObjectUpdate_2.objectName,
    AttrMods = RtObjUpdate#safsImmOiRtObjectUpdate_2.attrMods,
    R = rt_object_update_2_ret(Handle, ObjectName, AttrMods),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{adminOperationResult = AdmOpResult}) 
  when AdmOpResult =/= undefined ->
    Handle = AdmOpResult#safsImmOiAdminOperationResult.handle,
    Invocation = AdmOpResult#safsImmOiAdminOperationResult.invocation,
    Result = AdmOpResult#safsImmOiAdminOperationResult.result,
    R = admin_operation_result_ret(Handle, Invocation, Result),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{adminOperationResult_o2 = AdmOpResultO2}) 
  when AdmOpResultO2 =/= undefined ->
    Handle = AdmOpResultO2#safsImmOiAdminOperationResult_o2.handle,
    Invocation = AdmOpResultO2#safsImmOiAdminOperationResult_o2.invocation,
    Result = AdmOpResultO2#safsImmOiAdminOperationResult_o2.result,
    ReturnParams = AdmOpResultO2#safsImmOiAdminOperationResult_o2.returnParams,
    R = admin_operation_result_o2_ret(Handle, Invocation, Result, ReturnParams),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);
result_msg(#safsImmOiMessage{augmentCcbInitialize = AugmentCcb}) 
  when AugmentCcb =/= undefined ->
    Handle = AugmentCcb#safsImmOiAugmentCcbInitialize.handle,
    CcbId = AugmentCcb#safsImmOiAugmentCcbInitialize.ccbId,
    R = augment_ccb_initialize_ret(Handle, CcbId),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);
result_msg(#safsImmOiMessage{ccbSetErrorString = CcbSetErrorString}) 
  when CcbSetErrorString =/= undefined ->
    Handle = CcbSetErrorString#safsImmOiCcbSetErrorString.handle,
    CcbId = CcbSetErrorString#safsImmOiCcbSetErrorString.ccbId,
    ErrorString = CcbSetErrorString#safsImmOiCcbSetErrorString.errorString,
    R = ccb_set_error_string_ret(Handle, CcbId, ErrorString),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(#safsImmOiMessage{rtObjectDelayedNcUpdate_s2 = RtObjectUpdateDelayedNcValues}) 
  when RtObjectUpdateDelayedNcValues =/= undefined ->
    Handle = RtObjectUpdateDelayedNcValues#safsImmOiRtObjectUpdateDelayedNcValues_s2.handle,
    ObjectName = RtObjectUpdateDelayedNcValues#safsImmOiRtObjectUpdateDelayedNcValues_s2.objectName,
    AttrNames = RtObjectUpdateDelayedNcValues#safsImmOiRtObjectUpdateDelayedNcValues_s2.attributeNames,
    R = rt_object_update_delayed_nc_values_s2_ret(Handle, ObjectName, AttrNames),
    % io:format("Result message:\n~p\n\n", [R]),
    oi:encode_msg(R);

result_msg(Message) ->
    safs_error(result_msg, {?MODULE, "Received invalid message", Message}),
    {error, invalid_message}.
    

%%--------------------------------------------------------------------
%% Function: initialize_ret/2
%% Description:
%%--------------------------------------------------------------------
initialize_ret(Callbacks, RequestedVersion) ->
    case safs_imm_oi:initialize(Callbacks, RequestedVersion) of
	{ok, Handle, SupportedVersion}  ->
	    #safsImmOiInitializeRet{returnVal = sa_ais_ok,
				    handle = Handle,
				    version = SupportedVersion
				   };
	{error, Error} ->
	    #safsImmOiInitializeRet{returnVal = Error,
				    handle = 0, 
				    version = RequestedVersion
				   }
	end.

%%--------------------------------------------------------------------
%% Function: callbacks_initialize_ret/1
%% Description:
%%--------------------------------------------------------------------
callbacks_initialize_ret(Handle) ->
    case safs_imm_oi:callbacks_initialize(Handle) of
	ok ->
	    #safsImmOiCallbacksInitializeRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiCallbacksInitializeRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: finalize_ret/1
%% Description:
%%--------------------------------------------------------------------
finalize_ret(Handle) ->
    case safs_imm_oi:finalize(Handle) of
	ok ->
	    #safsImmOiFinalizeRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiFinalizeRet{returnVal = Error}
    end.


%%--------------------------------------------------------------------
%% Function: implementer_set_ret/2
%% Description:
%%--------------------------------------------------------------------
implementer_set_ret(Handle, ImplName) ->
    case safs_imm_oi:implementer_set(Handle, ImplName) of
	ok ->
	    #safsImmOiImplementerSetRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiImplementerSetRet{returnVal = Error}
    end.
    

%%--------------------------------------------------------------------
%% Function: implementer_clear_ret/1
%% Description:
%%--------------------------------------------------------------------
implementer_clear_ret(Handle) ->
    case safs_imm_oi:implementer_clear(Handle) of
	ok ->
	    #safsImmOiImplementerClearRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiImplementerClearRet{returnVal = Error}
    end.


%%--------------------------------------------------------------------
%% Function: class_implementer_set_ret/2
%% Description:
%%--------------------------------------------------------------------
class_implementer_set_ret(Handle, ClassName) ->
    case safs_imm_oi:class_implementer_set(Handle, ClassName) of
	ok ->
	    #safsImmOiClassImplementerSetRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiClassImplementerSetRet{returnVal = Error}
    end.
    

%%--------------------------------------------------------------------
%% Function: class_implementer_release_ret/2
%% Description:
%%--------------------------------------------------------------------
class_implementer_release_ret(Handle, ClassName) ->
    case safs_imm_oi:class_implementer_release(Handle, ClassName) of
	ok ->
	    #safsImmOiClassImplementerReleaseRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiClassImplementerReleaseRet{returnVal = Error}
    end.


%%--------------------------------------------------------------------
%% Function: object_implementer_set_ret/3
%% Description:
%%--------------------------------------------------------------------
object_implementer_set_ret(Handle, ObjectName, Scope) ->
    case safs_imm_oi:object_implementer_set(Handle, ObjectName, Scope) of
	ok ->
	    #safsImmOiObjectImplementerSetRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiObjectImplementerSetRet{returnVal = Error}
    end.
    

%%--------------------------------------------------------------------
%% Function: object_implementer_release_ret/3
%% Description:
%%--------------------------------------------------------------------
%% @private
object_implementer_release_ret(Handle, ObjectName, Scope) ->
    case safs_imm_oi:object_implementer_release(Handle, ObjectName, Scope) of
	ok ->
	    #safsImmOiObjectImplementerReleaseRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiObjectImplementerReleaseRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: rt_object_create_2_ret/4
%% Description:
%%--------------------------------------------------------------------
%% @private
rt_object_create_2_ret(Handle, ClassName, ParentName, AttrValues) ->
    case safs_imm_oi:rt_object_create_2(Handle,  ClassName, ParentName, AttrValues) of
	ok ->
	    #safsImmOiRtObjectCreate2Ret{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiRtObjectCreate2Ret{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: rt_object_delete_ret/2
%% Description:
%%--------------------------------------------------------------------
%% @private
rt_object_delete_ret(Handle, ObjectName) ->
    case safs_imm_oi:rt_object_delete(Handle,  ObjectName) of
	ok ->    
	    #safsImmOiRtObjectDeleteRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiRtObjectDeleteRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: rt_object_update_2_ret/3
%% Description:
%%--------------------------------------------------------------------
%% @private
rt_object_update_2_ret(Handle, ObjectName, AttrMods) ->
    case safs_imm_oi:rt_object_update_2(Handle,  ObjectName, AttrMods) of
	ok ->    
	    #safsImmOiRtObjectUpdate2Ret{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiRtObjectUpdate2Ret{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: admin_operation_result_ret/3
%% Description:
%%--------------------------------------------------------------------
%% @private
admin_operation_result_ret(Handle, Invocation, Result) ->
    case safs_imm_oi:admin_operation_result(Handle,  Invocation, Result) of
	ok ->    
	    #safsImmOiAdminOperationResultRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiAdminOperationResultRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: admin_operation_result_o2_ret/3
%% Description:
%%--------------------------------------------------------------------
%% @private
admin_operation_result_o2_ret(Handle, Invocation, Result, ReturnParams) ->
    case safs_imm_oi:admin_operation_result_o2(Handle,  Invocation, Result, ReturnParams) of
	ok ->    
	    #safsImmOiAdminOperationResultRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiAdminOperationResultRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% @private
augment_ccb_initialize_ret(Handle, CcbId) ->
    case safs_imm_oi:augment_ccb_initialize(Handle, CcbId) of
	{ok, CcbHandle, AoHandle} ->    
	    #safsImmOiAugmentCcbInitializeRet{returnVal = sa_ais_ok,
					     ccbHandle = CcbHandle,
					     ownerHandle = AoHandle};
	{error, Error} ->
	    #safsImmOiAugmentCcbInitializeRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% @private
ccb_set_error_string_ret(Handle, CcbId, ErrorString) ->
    case safs_imm_oi:ccb_set_error_string(Handle, CcbId, ErrorString) of
	ok ->    
	    #safsImmOiCcbSetErrorStringRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiCcbSetErrorStringRet{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: rt_object_delayed_nc_values_s2_ret/3
%% Description:
%%--------------------------------------------------------------------
%% @private
rt_object_update_delayed_nc_values_s2_ret(Handle, ObjectName, AttrNames) ->
    case safs_imm_oi:rt_object_update_delayed_nc_values(Handle,  ObjectName, AttrNames) of
	ok ->    
	    #safsImmOiRtObjectUpdateDelayedNcValuesS2Ret{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsImmOiRtObjectUpdateDelayedNcValuesS2Ret{returnVal = Error}
    end.

%%--------------------------------------------------------------------
%% Function: encode_callback_msg/3
%% Description:
%%--------------------------------------------------------------------
%% @private
encode_callback_msg(CbMsgNumber, Cb, Handle, Arg) ->
    try
	Bin = encode_callback(Cb, CbMsgNumber, Handle, Arg),
	{ok, Bin}
    catch
	_: Reason ->
	    ?ERROR("Failed to encode OI ~w Callback message. Arg = ~p~n"
		   "Reason: ~p~n" 
		   "Stack:~n~p~n",
		   [Cb, Arg, Reason, erlang:get_stacktrace()]),
	    {error, sa_ais_err_invalid_param}
    end.

encode_callback(ccb_object_create_2, CbMsgNumber, 
		    Handle, {CcbId, Class, Parent, Attrs}) ->
    ConvAttrs = [convert_attr(Attr) || Attr <- Attrs],
    Cb = 
	case Parent of 
	    undefined -> 
		#saImmOiCcbObjectCreateCallback_2{handle = Handle,
						  ccbId = CcbId,
						  className = to_bin(Class),
						  parentName = <<"">>,
						  attr = ConvAttrs};
	    _ ->
		assert_saname(Parent),
		#saImmOiCcbObjectCreateCallback_2{handle = Handle,
						  ccbId = CcbId,
						  className = to_bin(Class),
						  parentName = to_bin(Parent),
						  attr = ConvAttrs}
	end,
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, ccbObjectCreateCallback_2 = Cb},
    encode_callback(CbMsg);

encode_callback(ccb_object_delete, CbMsgNumber, Handle, {CcbId, ObjName}) ->
    assert_saname(ObjName),
    Cb = #saImmOiCcbObjectDeleteCallback{handle = Handle,
					 ccbId = CcbId,
					 objectName = to_bin(ObjName)},
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, ccbObjectDeleteCallback = Cb},
    encode_callback(CbMsg);
    
encode_callback(ccb_object_modify_2, CbMsgNumber, Handle, {CcbId, ObjName, AttrMods}) ->
    assert_saname(ObjName),
    ConvAttrMods = [convert_attr_mod(AttrMod) || AttrMod <- AttrMods],
    Cb = 
	#saImmOiCcbObjectModifyCallback_2{handle = Handle,
					  ccbId = CcbId,
					  objectName = to_bin(ObjName),
					  attrMods = ConvAttrMods},
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, ccbObjectModifyCallback_2 = Cb},
    encode_callback(CbMsg);
    
encode_callback(ccb_completed, CbMsgNumber, Handle, CcbId) ->
    Cb = #saImmOiCcbCompletedCallback{handle = Handle, ccbId = CcbId},
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, ccbCompletedCallback = Cb},
    encode_callback(CbMsg);
    
encode_callback(ccb_apply, CbMsgNumber, Handle, CcbId) ->
    Cb = #saImmOiCcbApplyCallback{handle = Handle, ccbId = CcbId},
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, ccbApplyCallback = Cb},
    encode_callback(CbMsg);

encode_callback(ccb_abort, CbMsgNumber, Handle, CcbId) ->
    Cb = #saImmOiCcbAbortCallback{handle = Handle, ccbId = CcbId},
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, ccbAbortCallback = Cb},
    encode_callback(CbMsg);

encode_callback(rt_attr_update, CbMsgNumber, Handle, {ObjectName, AttributeNames}) ->
    Cb = #saImmOiRtAttrUpdateCallback{handle = Handle, 
				      objectName = ObjectName,
				      attributeNames = AttributeNames},
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, rtAttrUpdateCallback = Cb},
    encode_callback(CbMsg);
encode_callback(admin_operation_2, CbMsgNumber, Handle, {Invocation, ObjectName, OperationId, Params}) ->
    Cb = #saImmOiAdminOperationCallback_2{handle = Handle, 
					  invocation = Invocation,
					  objectName = ObjectName,
					  operationId = OperationId,
					  params = Params},
    CbMsg = #'SaImmOiCallbacks'{callbackMessageNumber = CbMsgNumber, adminOperationCallback_2 = Cb},
    encode_callback(CbMsg).


encode_callback(CbMsg) ->
    oi:encode_msg(CbMsg).

%%--------------------------------------------------------------------
%% Function: convert_attr_mod/1
%% Description:
%%--------------------------------------------------------------------
convert_attr_mod({Type, Attr}) when Type =:= add; 
				    Type =:= sa_imm_attr_values_add ->
    #safsImmAttrModification_2{modType = sa_imm_attr_values_add,
				  modAttr = convert_attr(Attr)};
    
convert_attr_mod({Type, Attr}) when Type =:= delete; 
				    Type =:= sa_imm_attr_values_delete ->
    #safsImmAttrModification_2{modType = sa_imm_attr_values_delete,
				  modAttr = convert_attr(Attr)};
    
convert_attr_mod({Type, Attr}) when Type =:= replace; 
				    Type =:= sa_imm_attr_values_replace ->
    #safsImmAttrModification_2{modType = sa_imm_attr_values_replace,
				  modAttr = convert_attr(Attr)};
convert_attr_mod(#safsImmAttrModification_2{} = AttrMod) ->
    AttrMod.

%%--------------------------------------------------------------------
%% Function: convert_attr/1
%% Description:
%%--------------------------------------------------------------------
convert_attr({AttrName, int32, Ints}) ->
    Vals = [#safsImmAttrValue{saint32 = Int} || Int <- Ints],
    set_attr_values(AttrName, sa_imm_attr_saint32t, Vals);
    
convert_attr({AttrName, uint32, Ints}) ->
    Vals = [#safsImmAttrValue{sauint32 = Int} || Int <- Ints],
    set_attr_values(AttrName, sa_imm_attr_sauint32t, Vals);
    
convert_attr({AttrName, int64, Ints}) ->
    Vals = [#safsImmAttrValue{saint64 = Int} || Int <- Ints],
    set_attr_values(AttrName, sa_imm_attr_saint64t, Vals);
    
convert_attr({AttrName, uint64, Ints}) ->
    Vals = [#safsImmAttrValue{sauint64 = Int} || Int <- Ints],
    set_attr_values(AttrName, sa_imm_attr_sauint64t, Vals);
    
convert_attr({AttrName, time, Times}) ->
    Vals = [#safsImmAttrValue{satime = Time} || Time <- Times],
    set_attr_values(AttrName, sa_imm_attr_satimet, Vals);
    
convert_attr({AttrName, name, Names}) ->
    Vals = [#safsImmAttrValue{saname = to_bin(Name)} || Name <- Names],
    set_attr_values(AttrName, sa_imm_attr_sanamet, Vals);
    
convert_attr({AttrName, float, Floats}) ->
    Vals = [#safsImmAttrValue{safloat = Float} || Float <- Floats],
    set_attr_values(AttrName, sa_imm_attr_safloatt, Vals);
    
convert_attr({AttrName, double, Doubles}) ->
    Vals = [#safsImmAttrValue{sadouble = Double} || Double <- Doubles],
    set_attr_values(AttrName, sa_imm_attr_sadoublet, Vals);
    
convert_attr({AttrName, string, Strings}) ->
    Vals = 
	[#safsImmAttrValue{sastring = to_bin(String)} || String <- Strings],
    set_attr_values(AttrName, sa_imm_attr_sastringt, Vals);
    
convert_attr({AttrName, any, Anys}) ->
    Vals = [#safsImmAttrValue{saany = to_bin(Any)} || Any <- Anys],
    set_attr_values(AttrName, sa_imm_attr_saanyt, Vals);
convert_attr(#safsImmAttrValues_2{} = Attr) ->  
    Attr.

%%--------------------------------------------------------------------
%% Function: set_attr_values/3
%% Description:
%%--------------------------------------------------------------------
set_attr_values(AttrName, Type, Vals) ->
    #safsImmAttrValues_2{attrValueType = Type,
			 attrName = to_bin(AttrName),
			 attrValuesNumber = length(Vals), 
			 attrValues = Vals}.

%%--------------------------------------------------------------------
%% Function: decode_callback_reply/1
%% Description:
%%--------------------------------------------------------------------
decode_callback_reply({error, _} = Error, _) ->
    Error;
decode_callback_reply({ok, Bin}, CbMsgNumber) ->
    case catch oi:decode_msg(Bin, safsImmOiCallbacksRet) of
	#safsImmOiCallbacksRet{returnVal = sa_ais_ok, callbackMessageNumber = CbMsgNumber} ->
	    ok;
	#safsImmOiCallbacksRet{returnVal = RetVal, callbackMessageNumber = CbMsgNumber} ->
	    {error, RetVal};
	#safsImmOiCallbacksRet{} ->
	    %%error_logger:info_msg("Message not expected:\n ~p\n", [_Msg]),
	    not_my_reply;
	_Error ->
	    %%error_logger:info_msg("Message could not be decoded:\n~p\n", [_Error]),
	    not_my_reply
    end.

%%--------------------------------------------------------------------
%% Function: to_bin/1
%% Description:
%%--------------------------------------------------------------------
to_bin(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
to_bin(L) when is_list(L) ->
    list_to_binary(L);
to_bin(B) ->
    B.

%%--------------------------------------------------------------------
%% Function: assert_saname/1
%% Description:
%%--------------------------------------------------------------------
assert_saname(SaName) when is_binary(SaName) ->
    byte_size(SaName) < ?SA_MAX_NAME_LENGTH orelse throw(invalid_length_saname);

assert_saname(SaName) when is_list(SaName) ->
    length(SaName) < ?SA_MAX_NAME_LENGTH orelse throw(invalid_length_saname).

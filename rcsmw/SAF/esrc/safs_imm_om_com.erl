%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2012-2015. All Rights Reserved.
%% 
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%% 
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_om_com.erl
%%
%% Description:
%%    This file the interface that handles incomming IMM OM requests
%%    on the sockets from C.
%%
%%--------------------------------------------------------------------
-module(safs_imm_om_com).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("om.hrl").
-include("safs_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 message/2,
	 init/2,
	 callback_message_async/4,
	 callback_message_sync/4,
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
%% Macros
%%--------------------------------------------------------------------
-define(CB_TIMEOUT, 5000).
-define(SA_MAX_NAME_LENGTH, 256).


-define(SA_IMM_CCB_REGISTERED_OI,  16#00000001).
-define(SA_IMM_CCB_ALLOW_NULL_OI,  16#0000000000000100).
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(om_proxy_state, {external_pid}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/2
%% Description:
%%--------------------------------------------------------------------
init(_Proxy, _Cb) ->
    {ok, #om_proxy_state{}}.

%%--------------------------------------------------------------------
%% Function: message/2
%% Description:
%%--------------------------------------------------------------------
message(Bytes, State) ->
    case catch om:decode_msg(Bytes, safsImmOmMessage) of
	#safsImmOmMessage{} = Msg ->
	    handle_msg(Msg, State);
	_Error ->
	    error_logger:format("~p~p couldn't decode message:\n\t~p\n\tfrom:~p\n", 
				[?MODULE, self(), Bytes, 
				 State#om_proxy_state.external_pid]),
	    {{error, bad_message}, State}
    end. 

%%--------------------------------------------------------------------
%% Function: callback_message_async/4
%% Description:
%%--------------------------------------------------------------------
callback_message_async(Pid, Cb, Handle, Arg) ->
    case encode_callback_msg(Cb, Handle, Arg) of
	{ok, Bin} ->
	    safs_com_inproxy:send(Pid, Bin);
	Error ->
	    Error
    end.
	    
%%--------------------------------------------------------------------
%% Function: callback_message_sync/4
%% Description:
%%--------------------------------------------------------------------
callback_message_sync(Pid, Cb, Handle, Arg) ->
    case encode_callback_msg(Cb, Handle, Arg) of
	{ok, Bin} ->
	    Self = self(),
	    ReplyFun = 
		fun(Res) ->
			Reply = decode_callback_reply(Res),
			Self ! {callback_reply, Cb, Handle, Reply}
		end,
	    safs_com_inproxy:send_receive(Pid, Bin, ReplyFun);
	Error ->
	    safs_error(callback_message_sync, {"encode_callback_msg error:", Error}),
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
trace_groups() -> [error].

trace_points_list() ->
    [
     {error, 
      [{safs_error, 2}]}     
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
%% Life Cycle
handle_msg(#safsImmOmMessage{initialize = Initialize}, State) when Initialize =/= undefined ->
    Pid = Initialize#safsImmOmInitialize.callerPid,
    Callbacks = Initialize#safsImmOmInitialize.callbacks,
    RequestedVersion = Initialize#safsImmOmInitialize.version,
    R = case safs_imm_om:initialize(Callbacks,  RequestedVersion) of
	    {ok, Handle, SupportedVersion}  ->
		safs_imm_om:set_external_pid(Handle, Pid),
		#safsImmOmInitializeRet{returnVal = sa_ais_ok,
					handle = Handle,
					version = SupportedVersion
				       };
	    {error, Error} ->
		#safsImmOmInitializeRet{returnVal = Error,
					handle = 0,
					version = RequestedVersion
				       }
	end,
    {om:encode_msg(R), State#om_proxy_state{external_pid=Pid}};

handle_msg(#safsImmOmMessage{finalize = Finalize}, State) when Finalize =/= undefined ->
    Handle = Finalize#safsImmOmFinalize.handle,
    R = case safs_imm_om:finalize(Handle) of
	    ok ->
		#safsImmOmFinalizeRet{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmOmFinalizeRet{returnVal = Error}
	end,
    {om:encode_msg(R), State};

%%--------------------------------------------------------------------
%% Object Class Management
handle_msg(#safsImmOmMessage{classCreate_2 = ClassCreate}, State) when ClassCreate =/= undefined ->
    Handle = ClassCreate#safsImmOmClassCreate_2.handle,
    ClassName = ClassCreate#safsImmOmClassCreate_2.className,
    ClassCategory = ClassCreate#safsImmOmClassCreate_2.classCategory,
    AttrDefs = ClassCreate#safsImmOmClassCreate_2.attrDefinitions,
    R = case safs_imm_om:class_create_2(Handle, ClassName, ClassCategory, AttrDefs) of
	    ok ->
		#safsImmOmClassCreate2Ret{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmOmClassCreate2Ret{returnVal = Error}
	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{classDescriptionGet_2 = ClassGet}, State) when ClassGet =/= undefined ->
    Handle = ClassGet#safsImmOmClassDescriptionGet_2.handle,
    ClassName = ClassGet#safsImmOmClassDescriptionGet_2.className,
    R = case safs_imm_om:class_description_get_2(Handle, ClassName) of
	    {ok, Category, AttrDefs} ->
		#safsImmOmClassDescriptionGet2Ret{returnVal = sa_ais_ok,
						  classCategory = Category,
						  attrDefinitions = AttrDefs};
	    {error, Error} ->
		#safsImmOmClassDescriptionGet2Ret{returnVal = Error,
						  attrDefinitions = []}
	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{classDelete = ClassDelete}, State) when ClassDelete =/= undefined ->
    Handle = ClassDelete#safsImmOmClassDelete.handle,
    ClassName = ClassDelete#safsImmOmClassDelete.className,
    R = case safs_imm_om:class_delete(Handle, ClassName) of
	    ok ->
		#safsImmOmClassDeleteRet{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmOmClassDeleteRet{returnVal = Error}
	end,
    {om:encode_msg(R), State};

%%--------------------------------------------------------------------
%% Admin Owner
handle_msg(#safsImmOmMessage{adminOwnerInitialize = AOInitialize}, State) when AOInitialize =/= undefined ->
    Handle = AOInitialize#safsImmOmAdminOwnerInitialize.handle,
    AOName = AOInitialize#safsImmOmAdminOwnerInitialize.adminOwnerName,
    ReleaseOwnership = AOInitialize#safsImmOmAdminOwnerInitialize.releaseOwnershipOnFinalize,
    R = case safs_imm_om:admin_owner_initialize(Handle, AOName, ReleaseOwnership) of
	    {ok, OwnerHandle} ->
		#safsImmOmAdminOwnerInitializeRet{returnVal = sa_ais_ok,
						  handle = OwnerHandle
						 };
	    {error, Error} ->
		#safsImmOmAdminOwnerInitializeRet{returnVal = Error,
						  handle = 0}
	end,
    {om:encode_msg(R), State};
handle_msg(#safsImmOmMessage{adminOwnerFinalize = AOFinalize}, State) when AOFinalize =/= undefined ->
    OwnerHandle = AOFinalize#safsImmOmAdminOwnerFinalize.ownerHandle,
    R = case safs_imm_om:admin_owner_finalize(OwnerHandle) of
	    ok ->
		#safsImmOmAdminOwnerFinalizeRet{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmOmAdminOwnerFinalizeRet{returnVal = Error}
	end,
    {om:encode_msg(R), State};
handle_msg(#safsImmOmMessage{adminOwnerSet = AOSet}, State) when AOSet =/= undefined ->
    OwnerHandle = AOSet#safsImmOmAdminOwnerSet.ownerHandle,
    ObjectNames = AOSet#safsImmOmAdminOwnerSet.objectNames,
    Scope = AOSet#safsImmOmAdminOwnerSet.scope,
    R = case safs_imm_om:admin_owner_set(OwnerHandle, ObjectNames, Scope) of
	    ok ->
		#safsImmOmAdminOwnerSetRet{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmOmAdminOwnerSetRet{returnVal = Error}
	end,
    {om:encode_msg(R), State};
handle_msg(#safsImmOmMessage{adminOwnerRelease = AORelease}, State) when AORelease =/= undefined ->
    OwnerHandle = AORelease#safsImmOmAdminOwnerRelease.ownerHandle,
    ObjectNames = AORelease#safsImmOmAdminOwnerRelease.objectNames,
    Scope = AORelease#safsImmOmAdminOwnerRelease.scope,
    R = case safs_imm_om:admin_owner_release(OwnerHandle, ObjectNames, Scope) of
	    ok ->
		#safsImmOmAdminOwnerReleaseRet{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmOmAdminOwnerReleaseRet{returnVal = Error}
	end,
    {om:encode_msg(R), State};
handle_msg(#safsImmOmMessage{adminOwnerClear = AORelease}, State) when AORelease =/= undefined ->
    Handle = AORelease#safsImmOmAdminOwnerClear.handle,
    ObjectNames = AORelease#safsImmOmAdminOwnerClear.objectNames,
    Scope = AORelease#safsImmOmAdminOwnerClear.scope,
    R = case safs_imm_om:admin_owner_clear(Handle, ObjectNames, Scope) of
	    ok ->
		#safsImmOmAdminOwnerClearRet{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmOmAdminOwnerClearRet{returnVal = Error}
	end,
    {om:encode_msg(R), State};

%%--------------------------------------------------------------------
%% Search functions
handle_msg(#safsImmOmMessage{searchInitialize_2 = SearchInitialize}, State) when SearchInitialize =/= undefined ->
    Handle = SearchInitialize#safsImmOmSearchInitialize_2.handle,
    RootName = SearchInitialize#safsImmOmSearchInitialize_2.rootName,
    Scope = SearchInitialize#safsImmOmSearchInitialize_2.scope,
    SearchOptions = SearchInitialize#safsImmOmSearchInitialize_2.searchOptions,
    SearchParams = SearchInitialize#safsImmOmSearchInitialize_2.searchParam,
    AttributeNames = SearchInitialize#safsImmOmSearchInitialize_2.attributeNames,
    
    R = case safs_imm_om:search_initialize_2(Handle, RootName, Scope, SearchOptions, 
    					     SearchParams, AttributeNames) of
    	    {ok, SearchHandle} ->
    		#safsImmOmSearchInitialize2Ret{returnVal = sa_ais_ok,
					       handle = SearchHandle
					      };
    	    {error, Error} ->
    		#safsImmOmSearchInitialize2Ret{returnVal = Error, 
					       handle = 0
					      }
    	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{searchClassInitialize_s2 = SearchClassInitialize}, State) when SearchClassInitialize =/= undefined ->
    Handle = SearchClassInitialize#safsImmOmSearchClassInitialize_s2.handle,
    RootName = SearchClassInitialize#safsImmOmSearchClassInitialize_s2.rootName,
    Scope = SearchClassInitialize#safsImmOmSearchClassInitialize_s2.scope,
    SearchOptions = SearchClassInitialize#safsImmOmSearchClassInitialize_s2.searchOptions,
    ClassNames = SearchClassInitialize#safsImmOmSearchClassInitialize_s2.classNames,
    AttributeNames = SearchClassInitialize#safsImmOmSearchClassInitialize_s2.attributeNames,
    
    R = case safs_imm_om:search_class_initialize_s2(Handle, RootName, Scope, SearchOptions, 
						    ClassNames, AttributeNames) of
    	    {ok, SearchHandle} ->
    		#safsImmOmSearchClassInitializeS2Ret{returnVal = sa_ais_ok,
						     handle = SearchHandle
						    };
    	    {error, Error} ->
    		#safsImmOmSearchClassInitializeS2Ret{returnVal = Error, 
						     handle = 0
						    }
    	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{searchNext_2 = SearchNext}, State) when SearchNext =/= undefined ->
    Handle = SearchNext#safsImmOmSearchNext_2.searchHandle,
    
    R = case safs_imm_om:c_search_next_2(Handle) of
    	    {ok, ObjectName, Attributes} ->
    		#safsImmOmSearchNext2Ret{returnVal = sa_ais_ok,
					 objectName = ObjectName,
					 attributes = Attributes
					};
    	    {error, Error} ->
    		#safsImmOmSearchNext2Ret{returnVal = Error, 
					 attributes = []}
    	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{searchNextN_s2 = SearchNextN}, State) when SearchNextN =/= undefined ->
    Handle = SearchNextN#safsImmOmSearchNextN_s2.searchHandle,
    N = SearchNextN#safsImmOmSearchNextN_s2.numberOfObjects,

    R = case safs_imm_om:c_search_next_n_s2(Handle, N) of
    	    {ok, ObjectList} ->		
    		#safsImmOmSearchNextNS2Ret{returnVal = sa_ais_ok,
					   searchObjects = ObjectList};
    	    {error, Error} ->
    		#safsImmOmSearchNextNS2Ret{returnVal = Error, 
					   searchObjects = []}
    	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{searchFinalize = SearchFinalize}, State) when SearchFinalize =/= undefined ->
    SearchHandle = SearchFinalize#safsImmOmSearchFinalize.searchHandle,
    R = case safs_imm_om:search_finalize(SearchHandle) of
   	    ok ->
   		#safsImmOmSearchFinalizeRet{returnVal = sa_ais_ok};
   	    {error, Error} ->
   		#safsImmOmSearchFinalizeRet{returnVal = Error}
   	end,
    {om:encode_msg(R), State};

%%--------------------------------------------------------------------
%% Accessor functions
handle_msg(#safsImmOmMessage{accessorInitialize = AccessorInitialize}, State) when AccessorInitialize =/= undefined ->
    Handle = AccessorInitialize#safsImmOmAccessorInitialize.handle,
    
    R = case safs_imm_om:accessor_initialize(Handle) of
    	    {ok, AccessorHandle} ->
    		#safsImmOmAccessorInitializeRet{returnVal = sa_ais_ok,
						handle = AccessorHandle
					       };
    	    {error, Error} ->
    		#safsImmOmAccessorInitializeRet{returnVal = Error,
						handle = 0
					       }
    	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{accessorGet_2 = AccessorGet}, State) when AccessorGet =/= undefined ->

    Handle = AccessorGet#safsImmOmAccessorGet_2.accessorHandle,
    ObjectName = AccessorGet#safsImmOmAccessorGet_2.objectName,
    AttributeNames = AccessorGet#safsImmOmAccessorGet_2.attributeNames,
   
    R = case safs_imm_om:c_accessor_get_2(Handle, ObjectName, AttributeNames) of
    	    {ok, Attributes} ->
    		#safsImmOmAccessorGet2Ret{returnVal = sa_ais_ok,
					  attributes = Attributes
					 };
    	    {error, Error} ->
    		#safsImmOmAccessorGet2Ret{returnVal = Error, 
					  attributes = []
					 }
    	end,
    {om:encode_msg(R), State};


handle_msg(#safsImmOmMessage{accessorFinalize = AccessorFinalize}, State) when AccessorFinalize =/= undefined ->
    AccessorHandle = AccessorFinalize#safsImmOmAccessorFinalize.accessorHandle,
    R = case safs_imm_om:accessor_finalize(AccessorHandle) of
   	    ok ->
   		#safsImmOmAccessorFinalizeRet{returnVal = sa_ais_ok};
   	    {error, Error} ->
   		#safsImmOmAccessorFinalizeRet{returnVal = Error}
   	end,
    {om:encode_msg(R), State};


%%--------------------------------------------------------------------
%% CCB functions
handle_msg(#safsImmOmMessage{ccbInitialize = CcbInitialize}, State) when CcbInitialize =/= undefined ->

    OwnerHandle = CcbInitialize#safsImmOmCcbInitialize.ownerHandle,
    CcbFlags = CcbInitialize#safsImmOmCcbInitialize.ccbFlags,

     R = 
	case ccb_flags_to_atoms(CcbFlags, []) of
	    {error, Error1} ->
		#safsImmOmCcbInitializeRet{returnVal = Error1};
	    CcbFlags2 ->
		case safs_imm_om:ccb_initialize(OwnerHandle, CcbFlags2) of
		    {error, Error2} ->
			#safsImmOmCcbInitializeRet{returnVal = Error2};
		    {ok, CcbHandle} ->
			#safsImmOmCcbInitializeRet{returnVal = sa_ais_ok,
						   handle = CcbHandle
						  }
		end
	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{ccbObjectCreate_2 = CcbOC}, State) when CcbOC =/= undefined ->
    CcbHandle = CcbOC#safsImmOmCcbObjectCreate_2.handle,
    ClassName = CcbOC#safsImmOmCcbObjectCreate_2.className,
    ParentName = CcbOC#safsImmOmCcbObjectCreate_2.parentName,
    AttrValues = CcbOC#safsImmOmCcbObjectCreate_2.attrValues,
    
    case safs_imm_om:ccb_object_create_2(CcbHandle, ClassName, ParentName, AttrValues) of
	ok ->
	    {om:encode_msg(#safsImmOmCcbObjectCreate2Ret{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmCcbObjectCreate2Ret{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{ccbObjectDelete = CcbDelete}, State) when CcbDelete =/= undefined ->
    CcbHandle = CcbDelete#safsImmOmCcbObjectDelete.handle,
    ObjectName = CcbDelete#safsImmOmCcbObjectDelete.objectName,

    case safs_imm_om:ccb_object_delete(CcbHandle, ObjectName) of
	ok ->
	    {om:encode_msg(#safsImmOmCcbObjectDeleteRet{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmCcbObjectDeleteRet{returnVal = ErrorCode}),
	     State}
    end;

handle_msg(#safsImmOmMessage{ccbObjectModify_2 = CcbMod}, State) when CcbMod =/= undefined ->
    CcbHandle = CcbMod#safsImmOmCcbObjectModify_2.handle,
    ObjectName = CcbMod#safsImmOmCcbObjectModify_2.objectName,
    AttrMods = CcbMod#safsImmOmCcbObjectModify_2.attrMods,

    case safs_imm_om:ccb_object_modify_2(CcbHandle, ObjectName, AttrMods) of
	ok ->
	    {om:encode_msg(#safsImmOmCcbObjectModify2Ret{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmCcbObjectModify2Ret{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{ccbApply = CcbApply}, State) when CcbApply =/= undefined ->
    CcbHandle = CcbApply#safsImmOmCcbApply.handle,

    case safs_imm_om:ccb_apply(CcbHandle) of
	ok ->
	    {om:encode_msg(#safsImmOmCcbApplyRet{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmCcbApplyRet{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{ccbValidate = CcbValidate}, State) when CcbValidate =/= undefined ->
    CcbHandle = CcbValidate#safsImmOmCcbValidate.handle,

    case safs_imm_om:ccb_validate(CcbHandle) of
	ok ->
	    {om:encode_msg(#safsImmOmCcbValidateRet{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmCcbValidateRet{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{ccbAbort = CcbAbort}, State) when CcbAbort =/= undefined ->
    CcbHandle = CcbAbort#safsImmOmCcbAbort.handle,

    case safs_imm_om:ccb_abort(CcbHandle) of
	ok ->
	    {om:encode_msg(#safsImmOmCcbAbortRet{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmCcbAbortRet{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{ccbFinalize = CcbFinalize}, State) when CcbFinalize =/= undefined ->
    CcbHandle = CcbFinalize#safsImmOmCcbFinalize.handle, 

    case safs_imm_om:ccb_finalize(CcbHandle) of
	ok ->
	    {om:encode_msg(#safsImmOmCcbFinalizeRet{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmCcbFinalizeRet{returnVal = ErrorCode}), 
	     State}
    end;


handle_msg(#safsImmOmMessage{adminOperationInvoke_2 = AdminOpInvoke2}, State) when AdminOpInvoke2 =/= undefined ->
    OwnerHandle = AdminOpInvoke2#safsImmOmAdminOperationInvoke_2.ownerHandle, 
    ObjectName = AdminOpInvoke2#safsImmOmAdminOperationInvoke_2.objectName, 
    ContinuationId = AdminOpInvoke2#safsImmOmAdminOperationInvoke_2.continuationId, 
    OperationId = AdminOpInvoke2#safsImmOmAdminOperationInvoke_2.operationId, 
    Params = AdminOpInvoke2#safsImmOmAdminOperationInvoke_2.params, 
    Timeout = AdminOpInvoke2#safsImmOmAdminOperationInvoke_2.timeout, 

    case safs_imm_om:admin_operation_invoke_2(OwnerHandle, ObjectName, ContinuationId,
					      OperationId, Params, Timeout) of
	{ok, OpRetVal}  ->
	    {om:encode_msg(#safsImmOmAdminOperationInvoke2Ret{returnVal = sa_ais_ok, 
							      operationReturnValue = OpRetVal}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmAdminOperationInvoke2Ret{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{adminOperationInvokeAsync_2 = AdminOpInvokeAsync2}, State) when AdminOpInvokeAsync2 =/= undefined ->
    OwnerHandle = AdminOpInvokeAsync2#safsImmOmAdminOperationInvokeAsync_2.ownerHandle, 
    UserInvocation = AdminOpInvokeAsync2#safsImmOmAdminOperationInvokeAsync_2.invocation, 
    ObjectName = AdminOpInvokeAsync2#safsImmOmAdminOperationInvokeAsync_2.objectName, 
    ContinuationId = AdminOpInvokeAsync2#safsImmOmAdminOperationInvokeAsync_2.continuationId, 
    OperationId = AdminOpInvokeAsync2#safsImmOmAdminOperationInvokeAsync_2.operationId, 
    Params = AdminOpInvokeAsync2#safsImmOmAdminOperationInvokeAsync_2.params, 

    case safs_imm_om:c_admin_operation_invoke_async_2(OwnerHandle, UserInvocation, 
						    ObjectName, ContinuationId,
						    OperationId, Params) of
	ok  ->
	    {om:encode_msg(#safsImmOmAdminOperationInvokeAsync2Ret{returnVal = sa_ais_ok}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmAdminOperationInvokeAsync2Ret{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{callbacksInitialize = CallbacksInit}, State)  when CallbacksInit =/= undefined ->

    Handle = CallbacksInit#safsImmOmCallbacksInitialize.handle,
    case safs_imm_om:callbacks_initialize(Handle) of
	ok ->
	    {om:encode_msg(#safsImmOmCallbacksInitializeRet{returnVal = sa_ais_ok}), 
	     State};
	{error, Error} ->
	    {om:encode_msg(#safsImmOmCallbacksInitializeRet{returnVal = Error}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{initialize_o2 = Initialize}, State) when Initialize =/= undefined ->
    Pid = Initialize#safsImmOmInitialize_o2.callerPid,
    Callbacks = Initialize#safsImmOmInitialize_o2.callbacks,
    RequestedVersion = Initialize#safsImmOmInitialize_o2.version,
    R = case safs_imm_om:initialize_o2(Callbacks,  RequestedVersion) of
	    {ok, Handle, SupportedVersion}  ->
		safs_imm_om:set_external_pid(Handle, Pid),
		#safsImmOmInitializeRet{returnVal = sa_ais_ok,
					handle = Handle,
					version = SupportedVersion
					   };
	    {error, Error} ->
		#safsImmOmInitializeRet{returnVal = Error,
					handle = 0,
					version = RequestedVersion
				       }
	end,
    {om:encode_msg(R), State#om_proxy_state{external_pid=Pid}};

handle_msg(#safsImmOmMessage{adminOperationInvoke_o2 = AdminOpInvokeO2}, State) when AdminOpInvokeO2 =/= undefined ->
    OwnerHandle = AdminOpInvokeO2#safsImmOmAdminOperationInvoke_o2.ownerHandle, 
    ObjectName = AdminOpInvokeO2#safsImmOmAdminOperationInvoke_o2.objectName, 
    ContinuationId = AdminOpInvokeO2#safsImmOmAdminOperationInvoke_o2.continuationId, 
    OperationId = AdminOpInvokeO2#safsImmOmAdminOperationInvoke_o2.operationId, 
    Params = AdminOpInvokeO2#safsImmOmAdminOperationInvoke_o2.params, 
    Timeout = AdminOpInvokeO2#safsImmOmAdminOperationInvoke_o2.timeout, 

    case safs_imm_om:c_admin_operation_invoke_o2(OwnerHandle, ObjectName, ContinuationId,
					      OperationId, Params, Timeout) of
	{ok, OpRetVal, ReturnParams}  ->
	    {om:encode_msg(#safsImmOmAdminOperationInvokeO2Ret{returnVal = sa_ais_ok, 
							       operationReturnValue = OpRetVal,
							       returnParams = ReturnParams}), 
	     State};
	{error, ErrorCode} ->
	    {om:encode_msg(#safsImmOmAdminOperationInvokeO2Ret{returnVal = ErrorCode}), 
	     State}
    end;

handle_msg(#safsImmOmMessage{ccbObjectRead = CcbRead}, State) when CcbRead =/= undefined ->
    CcbId = CcbRead#safsImmOmCcbObjectRead.handle,
    ObjectName = CcbRead#safsImmOmCcbObjectRead.objectName,
    AttributeNames = CcbRead#safsImmOmCcbObjectRead.attributeNames,

    R = case safs_imm_om:c_ccb_object_read(CcbId, ObjectName, AttributeNames) of
	    {ok, Attributes} ->
		#safsImmOmCcbObjectReadRet{returnVal = sa_ais_ok,
					   attributes = Attributes
					  };
	    {error, ErrorCode} ->
		#safsImmOmCcbObjectReadRet{returnVal = ErrorCode,
					   attributes = []
					  }
	end,
    {om:encode_msg(R), State};

handle_msg(#safsImmOmMessage{ccbGetErrorStrings = CcbGetErrorStrings}, State) when CcbGetErrorStrings =/= undefined ->
    Ccb = CcbGetErrorStrings#safsImmOmCcbGetErrorStrings.handle,

    R = case safs_imm_om:ccb_get_error_strings(Ccb) of
	    {ok, ErrorStrings} ->
		#safsImmOmCcbGetErrorStringsRet{returnVal = sa_ais_ok,
						errorStrings = ErrorStrings
					       };
	    {error, ErrorCode} ->
		#safsImmOmCcbGetErrorStringsRet{returnVal = ErrorCode,
						errorStrings = []
					       }
	end,
    {om:encode_msg(R), State};


handle_msg(Message, State) ->
    error_logger:format("~p~p received invalid message:\n\t~p\n", 
			[?MODULE, self(), Message]),
    {{error, invalid_message}, State}.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: encode_callback_msg/3
%% Description:
%%--------------------------------------------------------------------
encode_callback_msg(Cb, Handle, Arg) ->
    try
	Bin = encode_callback(Cb, Handle, Arg),
	{ok, Bin}
    catch
	_: Reason ->
	    ?ERROR("Failed to encode OM ~w Callback message. Arg = ~p~n"
		   "Reason: ~p~n" 
		   "Stack:~n~p~n",
		   [Cb, Arg, Reason, erlang:get_stacktrace()]),
	    {error, sa_ais_err_invalid_param}
    end.


encode_callback(admin_operation_invoke_callback, Handle,  
		{Invocation, OperationReturnValue, Error}) ->
    
    Cb = 
	#saImmOmAdminOperationInvokeCallback{handle = Handle,
					     invocation = Invocation,
					     operationReturnValue = convert_ok_to_sa_ais_ok(OperationReturnValue),
					     error = convert_ok_to_sa_ais_ok(Error)},
    CbMsg = #'SaImmCallbacks'{adminOperationInvokeCallback = Cb},
    encode_callback(CbMsg);
encode_callback(admin_operation_invoke_callback_o2, Handle, 
		{Invocation, OperationReturnValue, Error, ReturnParams}) ->
    
    Cb = 
	#saImmOmAdminOperationInvokeCallback_o2{handle = Handle,
						invocation = Invocation,
						operationReturnValue = 
						   convert_ok_to_sa_ais_ok(OperationReturnValue),
						error = convert_ok_to_sa_ais_ok(Error),
						returnParams = ReturnParams},
    
    CbMsg = #'SaImmCallbacks'{adminOperationInvokeCallback_o2 = Cb},
    encode_callback(CbMsg).

encode_callback(CbMsg) ->
    om:encode_msg(CbMsg).


%%--------------------------------------------------------------------
%% Function: decode_callback_reply/1
%% Description:
%%--------------------------------------------------------------------
decode_callback_reply({error, _} = Error) ->
    Error;
decode_callback_reply({ok, Bin}) ->
    case catch om:decode_msg(Bin, safsImmOmCallbacksRet) of
	#safsImmOmCallbacksRet{returnVal = sa_ais_ok} ->
	    ok;
	#safsImmOmCallbacksRet{returnVal = RetVal} ->
	    {error, RetVal};
	Error ->
	    error_logger:error_msg("Couldn't decode callback reply: ~p\n",
				   [Error]),
	    {error, sa_ais_err_bad_operation}
    end.

%%--------------------------------------------------------------------
%% Function: convert_ok_to_sa_ais_ok/1
%% Description:
%%--------------------------------------------------------------------
convert_ok_to_sa_ais_ok(ok) ->
    sa_ais_ok;
convert_ok_to_sa_ais_ok(E) ->
    E.


%%--------------------------------------------------------------------
%% Function: ccb_flags_to_atoms/2
%% Description:
%%--------------------------------------------------------------------
ccb_flags_to_atoms(Flags, Acc) when Flags == 0 ->
    Acc;
ccb_flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_CCB_REGISTERED_OI  =:= ?SA_IMM_CCB_REGISTERED_OI ->
    ccb_flags_to_atoms(Flags bxor ?SA_IMM_CCB_REGISTERED_OI, [ccb_registered_oi |Acc]);
ccb_flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_CCB_ALLOW_NULL_OI  =:= ?SA_IMM_CCB_ALLOW_NULL_OI ->
    ccb_flags_to_atoms(Flags bxor ?SA_IMM_CCB_ALLOW_NULL_OI, [ccb_allow_null_oi |Acc]);
ccb_flags_to_atoms(_Flags, _Acc) ->
    {error, sa_ais_err_invalid_param}.


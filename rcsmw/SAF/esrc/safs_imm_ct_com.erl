%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
%% 
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%% 
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_ct_com.erl
%%
%% Description:
%%    This file the interface that handles incomming IMM Class Transfer requests
%%    on the sockets from C.
%%
%%--------------------------------------------------------------------
-module(safs_imm_ct_com).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_imm_ct_p.hrl").
-include("safs_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 message/2,
	 init/2,
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
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(srv_proxy_state, {proxy_pid, external_pid, cb, cb_state}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/2
%% Description:
%%--------------------------------------------------------------------
init(Proxy, CbModule) ->
    case apply(CbModule, init, []) of
	{ok, CbState} ->
	    {ok, #srv_proxy_state{proxy_pid=Proxy, cb=CbModule, cb_state=CbState}};
	{error, Error} ->
	    safs_error(init, {"IMM CT Callback init error:", Error}),
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%% Function: message/2
%% Description:
%%--------------------------------------------------------------------
message(Bytes, State) ->
    case catch safs_imm_ct_p:decode_msg(Bytes, safsImmCtMessage) of
	#safsImmCtMessage{} = Msg ->
	    handle_msg(Msg, State);
	_Error ->
	    if
		State#srv_proxy_state.external_pid =/= undefined ->
		    error_logger:format("~p~p couldn't decode message:\n\t~p\n\tfrom:~p\n", 
					[?MODULE, self(), Bytes, 
					 State#srv_proxy_state.external_pid]),
		    {{error, bad_message}, State};
		true ->
		    error_logger:format("~p~p couldn't decode first message:\n\t~p\n", 
					[?MODULE, self(), Bytes]),
		    {{error, bad_message}, State}
	    end
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
trace_groups() -> [message, error].

trace_points_list() ->
    [
     {message,
      [{handle_msg, 2}]},
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
handle_msg(#safsImmCtMessage{initialize = Initialize}, State) when Initialize =/= undefined ->
    ?INFO("Initialize\n", []),
    Pid = Initialize#safsImmCtInitialize.callerPid,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message, 
			  [{initialize, Pid}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
	    {ok, Handle}  ->
		#safsImmCtInitializeRet{returnVal = sa_ais_ok,
					handle = Handle
				       };
	    {error, Error} ->
		#safsImmCtInitializeRet{returnVal = Error,
					handle = 0
				       }
	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{external_pid=Pid,
							cb_state=NewState}};
handle_msg(#safsImmCtMessage{initialize_2 = Initialize}, State) when Initialize =/= undefined ->
    ?INFO("Initialize_2\n", []),
    Pid = Initialize#safsImmCtInitialize_2.callerPid,
    DelayedStart = Initialize#safsImmCtInitialize_2.delayedStart,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message, 
			  [{initialize_2, Pid, DelayedStart}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
	    {ok, Handle}  ->
		#safsImmCtInitializeRet{returnVal = sa_ais_ok,
					handle = Handle
				       };
	    {error, Error} ->
		#safsImmCtInitializeRet{returnVal = Error,
					handle = 0
				       }
	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{external_pid=Pid,
							cb_state=NewState}};

handle_msg(#safsImmCtMessage{finalize = Finalize}, State) when Finalize =/= undefined ->
    ?INFO("Finalize\n", []),
    Handle = Finalize#safsImmCtFinalize.handle,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			      handle_message, 
			      [{finalize, Handle}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
	    ok ->
		#safsImmCtFinalizeRet{returnVal = sa_ais_ok};
	    {error, Error} ->
		#safsImmCtFinalizeRet{returnVal = Error}
	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{readSchemaVersion = ReadSchemaVersion}, State) when ReadSchemaVersion =/= undefined ->
    ?INFO("Read Schema Version\n", []),
    Handle = ReadSchemaVersion#safsImmCtReadSchemaVersion.handle,
    SchemaName = ReadSchemaVersion#safsImmCtReadSchemaVersion.schemaName,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			      handle_message, 
			      [{read_schema_version, Handle, SchemaName}, 
			       State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
     	    {ok, OldSchemaVersion, NewSchemaVersion} ->
		#safsImmCtReadSchemaVersionRet{returnVal = sa_ais_ok,
					       oldVersion = OldSchemaVersion,
					       newVersion = NewSchemaVersion};
     	    {error, Error} ->
     		#safsImmCtReadSchemaVersionRet{returnVal = Error}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{failUpgrade = FailUpgrade}, State) when FailUpgrade =/= undefined ->
    ?INFO("Fail Upgrade\n", []),
    Handle = FailUpgrade#safsImmCtFailUpgrade.handle,
    Message = FailUpgrade#safsImmCtFailUpgrade.message,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message,
			  [{fail_upgrade, Handle, Message}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
     	    ok ->
     		#safsImmCtFailUpgradeRet{returnVal = sa_ais_ok};
     	    {error, Error} ->
     		#safsImmCtFailUpgradeRet{returnVal = Error}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{readInstances = ReadInstances}, State) when ReadInstances =/= undefined ->
    ?INFO("Read Instances\n", []),
    Handle = ReadInstances#safsImmCtReadInstances.handle,
    ClassNames = ReadInstances#safsImmCtReadInstances.classNames,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message,
			  [{read_instances, Handle, ClassNames}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
     	    {ok, InstanceGroups} ->
     		#safsImmCtReadInstancesRet{returnVal = sa_ais_ok,
					   instanceGroups = InstanceGroups};
     	    {error, Error} ->
     		#safsImmCtReadInstancesRet{returnVal = Error,
					   instanceGroups = []}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{readInstances_2 = ReadInstances}, State) when ReadInstances =/= undefined ->
    ?INFO("Read Instances 2\n", []),
    Handle = ReadInstances#safsImmCtReadInstances_2.handle,
    ClassNames = ReadInstances#safsImmCtReadInstances_2.classNames,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			      handle_message,
			      [{read_instances_2, Handle, ClassNames}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
     	    {ok, InstanceGroups} ->
     		#safsImmCtReadInstancesRet{returnVal = sa_ais_ok,
					   instanceGroups = InstanceGroups};
     	    {error, Error} ->
     		#safsImmCtReadInstancesRet{returnVal = Error,
					   instanceGroups = []}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{writeInstances = WriteInstances}, State) when WriteInstances =/= undefined ->
    ?INFO("Write Instances\n", []),
    Handle = WriteInstances#safsImmCtWriteInstances.handle,
    InstanceGroups = WriteInstances#safsImmCtWriteInstances.instanceGroups,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message,
			  [{write_instances, Handle, InstanceGroups}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
	    ok ->
     		#safsImmCtWriteInstancesRet{returnVal = sa_ais_ok};
     	    {error, Error} ->
     		#safsImmCtWriteInstancesRet{returnVal = Error}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{copyInstances = CopyInstances}, State) when CopyInstances =/= undefined ->
    ?INFO("Copy Instances\n", []),
    Handle = CopyInstances#safsImmCtCopyInstances.handle,
    ClassNames = CopyInstances#safsImmCtCopyInstances.classNames,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message,
			  [{copy_instances, Handle, ClassNames}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
     	    ok ->
     		#safsImmCtCopyInstancesRet{returnVal = sa_ais_ok};
     	    {error, Error} ->
     		#safsImmCtCopyInstancesRet{returnVal = Error}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{waitForClasses = WaitForClasses}, State) when WaitForClasses =/= undefined ->
    ?INFO("Wait For Classes\n", []),
    Handle = WaitForClasses#safsImmCtWaitForClasses.handle,
    ClassNames = WaitForClasses#safsImmCtWaitForClasses.classNames,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message, 
			  [{wait_for_classes, Handle, ClassNames}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
     	    ok ->
     		#safsImmCtWaitForClassesRet{returnVal = sa_ais_ok};
     	    {error, Error} ->
     		#safsImmCtWaitForClassesRet{returnVal = Error}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{writeRtInstances = WriteRtInstances}, State) when WriteRtInstances =/= undefined ->
    ?INFO("Write Rt Instances\n", []),
    Handle = WriteRtInstances#safsImmCtWriteRtInstances.handle,
    ImplementerName = WriteRtInstances#safsImmCtWriteRtInstances.implementerName,
    InstanceGroups = WriteRtInstances#safsImmCtWriteRtInstances.instanceGroups,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message,
			  [{write_rt_instances, Handle, ImplementerName, InstanceGroups}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
	    ok ->
     		#safsImmCtWriteRtInstancesRet{returnVal = sa_ais_ok};
     	    {error, Error} ->
     		#safsImmCtWriteRtInstancesRet{returnVal = Error}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(#safsImmCtMessage{copyRtInstances = CopyRtInstances}, State) when CopyRtInstances =/= undefined ->
    ?INFO("Copy Rt Instances\n", []),
    Handle = CopyRtInstances#safsImmCtCopyRtInstances.handle,
    ImplementerName = CopyRtInstances#safsImmCtCopyRtInstances.implementerName,
    ClassNames = CopyRtInstances#safsImmCtCopyRtInstances.classNames,
    {CbRet, NewState} = apply(State#srv_proxy_state.cb, 
			  handle_message,
			  [{copy_rt_instances, Handle, ImplementerName, ClassNames}, State#srv_proxy_state.cb_state]),
    R = 
	case CbRet of
     	    ok ->
     		#safsImmCtCopyRtInstancesRet{returnVal = sa_ais_ok};
     	    {error, Error} ->
     		#safsImmCtCopyRtInstancesRet{returnVal = Error}
     	end,
    {safs_imm_ct_p:encode_msg(R), State#srv_proxy_state{cb_state=NewState}};

handle_msg(Message, State) ->
    error_logger:format("~p~p received invalid message:\n\t~p\nfrom:~p\n", 
			[?MODULE, self(), Message, State#srv_proxy_state.external_pid]),
    {{error, invalid_message}, State}.



%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	rct_safe_imm_om_rpc.erl %
%% @author eolaand
%% @copyright Ericsson AB 2013-2015
%% @doc 
%% This module provides an RPC API towards safe_imm_om executing on
%% a remote node. The safe_imm_om API is part of the OTP Safe application.
%%
%% The module is implemented as a ct_hook in order to facilitate its use in 
%% Common Test suites. Before each test run this ct_hook starts 
%% the Safe application on the remote test node. The Safe application connects
%% towards an RCS node, either on target or in a simulated environment.
%%
%% The ct_hook sets up a connection towards the remote node using the default 
%% value for node name, unless the parameter sname is used, and a hardcoded 
%% cookie `testnode'. A server with a registered name is started in order to 
%% handle the communication towards the remote test node.  
%%
%% The API functions in this module corresponds to the functions in 
%% the module safe_imm_om in OTP Safe application.
%% The API functions have the same names and parameters, with one exception;
%% the {@link initialize/1} function in this module do not accept a module
%% name as Callback function. The atom undefined may be used if no callbacks
%% are needed. All API functions also has a corresponding higher arity function
%% that takes the registered name of the server as first parameter. 
%% See the OTP Safe documentation for more information about the API functions.
%%
%% The API in this module can also be used together with the `rct_safe_rpc' 
%% hook.
%% If several of the Safe services IMM, LOG or NTF is to be used in a test 
%% suite, it is required to use the `rct_safe_rpc' hook instead of 
%% `rct_safe_imm_om_rpc'. See {@link rct_safe_rpc} for more information.
%%
%% The `rct_safe_imm_om_rpc' hook is specified in the `suite/0' function of the 
%% test suite as described below:
%%
%% ```suite() -> 
%%        [{ct_hooks, [{safe_imm_om_rpc, Opts}]}].
%%
%%    Opts = [Opt]
%%    Opt = {du_no, No} | 
%%          {sim_sname, Sname} | 
%%          {instance_name, InstanceName} | 
%%          {safe_debug_level, SafeDebugLevel} | 
%%          {finalize_on_fail, FoFFlag}'''
%%
%%  `No = integer()' <br/>
%%    - Used to indicate testnode (DU) number when testing towards a specific 
%%      node in a cluster.<br/>
%%      Default is 1
%%
%%  `Sname = atom()' <br/>
%%    - Used to provide a different sim node name than the default 
%%      when running tests towards a simulator.<br/>
%%      Default is `$USER' <br/>
%%
%%  `InstanceName = atom()' <br/>
%%    - Used to provide an identity of the CTH instance. Also used as the 
%%      registered name of the server handling the connection towards the test 
%%      node. The naming is useful when 
%%      running multinode tests. See example below.<br/>
%%      Default is `?MODULE' <br/>
%%
%%  `SafeDebugLevel = integer()' <br/>
%%    - Set debug level of safe application. 0, 1 or 2 can be used. <br/>
%%      Default is undefined <br/>
%%
%%  `FoFFlag = boolean()' <br/>
%%    - Determines if all active OM handles should be automatically finalized
%%      at test case failure. <br/>
%%      Default is `false'<br/>
%%
%% In a multinode scenario several instances may be specified using different
%% instance names as in this example.
%%
%% ```suite() -> 
%%     [{ct_hooks, [{safe_imm_om_rpc, [{instance_name, safe_om_1},
%%                                     {du_no, 1}]},
%%                  {safe_imm_om_rpc, [{instance_name, safe_om_2}
%%                                     {du_no, 2}]}].'''
%%
%% @end
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-11-13 eolaand     Created
%%% R4A/1      2015-06-08 eolaand     Updates for cluster.
%%% ----------------------------------------------------------
-module(rct_safe_imm_om_rpc).

%% OM API
-export([
	 initialize/1,
	 initialize/2,
	 finalize/1,
	 finalize/2,

         class_create_2/4,
         class_create_2/5,
         %% class_description_get_2/2,
         %% class_description_get_2/3,
         %% class_delete/2,
         %% class_delete/3,

         %% search_initialize_2/6,
         %% search_initialize_2/7,
         %% search_next_2/1,
         %% search_next_2/2,
         %% search_finalize/1,
         %% search_finalize/2,

         accessor_initialize/1,
         accessor_initialize/2,
         accessor_get_2/3,
         accessor_get_2/4,
         accessor_finalize/1,
         accessor_finalize/2,

         admin_owner_initialize/3,
         admin_owner_initialize/4,
         admin_owner_set/3,
         admin_owner_set/4,
         admin_owner_release/3,
         admin_owner_release/4,
         admin_owner_clear/3,
         admin_owner_clear/4,
         admin_owner_finalize/1,
         admin_owner_finalize/2,

	 %% ccb_initialize/2,
	 %% ccb_initialize/3,
	 %% ccb_object_create_2/4,
	 %% ccb_object_create_2/5,
	 %% ccb_object_delete/2,
	 %% ccb_object_delete/3,
	 %% ccb_object_modify_2/3,
	 %% ccb_object_modify_2/4,
	 %% ccb_apply/1,
	 %% ccb_apply/2,
	 %% ccb_finalize/1,
	 %% ccb_finalize/2,
         %% ccb_get_error_strings/1,
         %% ccb_get_error_strings/2,

	 admin_operation_invoke_2/6,
	 admin_operation_invoke_2/7
	 %% admin_operation_invoke_async_2/6,
	 %% admin_operation_invoke_async_2/7,
	 %% admin_operation_continue/3,
	 %% admin_operation_continue/4,
	 %% admin_operation_continue_async/4,
	 %% admin_operation_continue_async/5,
	 %% admin_operation_continuation_clear/3,
	 %% admin_operation_continuation_clear/4
	]).

%% Support API
-export([get_testnode/0, 
	 get_testnode/1,
	 get_node_status/0,
	 get_node_status/1,
	 subscribe_testnode/0,
	 subscribe_testnode/1,
	 unsubscribe_testnode/0,
	 unsubscribe_testnode/1
	 ]).

%% Server API
-export([start/0, 
	 start/1, 
	 stop/0, 
	 stop/1]).

%% ct_hooks callbacks
-export([id/1, 
	 init/2, 
	 pre_init_per_suite/3, 
	 pre_init_per_testcase/3, 
	 on_tc_fail/3,
	 terminate/1]).

%% Debug
-export([dump/0, 
	 dump/1]).

%-compile(export_all).

-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_imm.hrl").
-include("rct_safe_rpc.hrl").

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================
%%======================================================================
%% 4.3 Library Life Cycle
%%======================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initializes an OM handle.<br/>
%% Callbacks is a fun/1 that returns either ok or {error, safe_ais_error()}.
%% <br/>Otherwise this function is equivalent to 
%% <a href="safe_imm_om.html#initialize-1">
%% <tt>safe_imm_om:initialize(Callbacks)</tt></a>.
%%
%% @spec initialize(Callbacks) -> Result
%%
%% Callbacks = fun() | safe_imm_callbacks() | undefined
%% Result = {ok, OmHandle, SupportedVersion} | {error, safe_ais_error()}
%% OmHandle = safe_imm_handle()
%% SupportedVersion = safe_version()
%%           
%% @end
%%--------------------------------------------------------------------
initialize(Callbacks) 
  when is_function(Callbacks); 
       is_record(Callbacks, safe_imm_callbacks);
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    om_call({initialize, [get_cb_fun(Callbacks, ?SERVER)]}).

%% @private
initialize(Server, Callbacks)
  when is_function(Callbacks);
       is_record(Callbacks, safe_imm_callbacks);
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    om_call(Server, {initialize, [get_cb_fun(Callbacks, Server)]}).


%% @spec finalize(OmHandle) -> Result
%% @equiv safe_imm_om:finalize(OmHandle)
finalize(OmHandle) ->
    om_call({finalize, [OmHandle]}).

%% @private
finalize(Server, OmHandle) ->
    om_call(Server, {finalize, [OmHandle]}).

%%======================================================================
%% 4.4 Object Class Management
%%======================================================================

%% @spec class_create_2(OmHandle, ClassName, ClassCategory, AttrDefs) -> Result
%% @equiv safe_imm_om:class_create_2(OmHandle, ClassName, ClassCategory, 
%%                                   AttrDefs)
class_create_2(_OmHandle, _ClassName, _ClassCategory, _AttrDefs) ->
    {error, not_implemented_yet}.
    
%% @private
class_create_2(_Server, _OmHandle, _ClassName, _ClassCategory, _AttrDefs) ->
    {error, not_implemented_yet}.
    
%%======================================================================
%% 4.6 Object access
%%======================================================================
%% @spec accessor_initialize(OmHandle) -> Result
%% @equiv safe_imm_om:accessor_initialize(OmHandle)
accessor_initialize(OmHandle) ->
    om_call({accessor_initialize, [OmHandle]}).

%% @private
accessor_initialize(Server, OmHandle) ->
    om_call(Server, {accessor_initialize, [OmHandle]}).


%% @spec accessor_get_2(AccHandle, ObjectName, AttrNames) -> Result
%% @equiv safe_imm_om:accessor_get_2(OmHandle, ObjectName, AttrNames)
accessor_get_2(AccHandle, ObjectName, AttrNames) ->
    om_call({accessor_get_2, [AccHandle, ObjectName, AttrNames]}).

%% @private
accessor_get_2(Server, AccHandle, ObjectName, AttrNames) ->
    om_call(Server, {accessor_get_2, [AccHandle, ObjectName, AttrNames]}).


%% @spec accessor_finalize(AccHandle) -> Result
%% @equiv safe_imm_om:accessor_finalize(AccHandle)
accessor_finalize(AccHandle) -> 
    om_call({accessor_finalize, [AccHandle]}).

%% @private
accessor_finalize(Server, AccHandle) -> 
    om_call(Server, {accessor_finalize, [AccHandle]}).
    
%%======================================================================
%% 4.7 Object administration ownership
%%======================================================================
%% @spec admin_owner_initialize(OmHandle, AdminOwnerName, 
%%                              ReleaseOwnershipOnFinalize) -> Result
%% @equiv safe_imm_om:admin_owner_initialize(OmHandle, AdminOwnerName, 
%%                              ReleaseOwnershipOnFinalize)
admin_owner_initialize(OmHandle, AdminOwnerName, ReleaseOwnershipOnFinalize) ->
    om_call({admin_owner_initialize, [OmHandle, AdminOwnerName, 
				      ReleaseOwnershipOnFinalize]}).
%% @private
admin_owner_initialize(Server, OmHandle, AdminOwnerName, 
		       ReleaseOwnershipOnFinalize) ->
    om_call(Server, {admin_owner_initialize, [OmHandle, AdminOwnerName, 
					      ReleaseOwnershipOnFinalize]}).

%% @spec admin_owner_set(AdminHandle, ObjectNames, Scope) -> Result
%% @equiv safe_imm_om:admin_owner_set(AdminHandle, ObjectNames, Scope)
admin_owner_set(AdminHandle, ObjectNames, Scope) ->
    om_call({admin_owner_set, [AdminHandle, ObjectNames, Scope]}).

%% @private
admin_owner_set(Server, AdminHandle, ObjectNames, Scope) ->
    om_call(Server, {admin_owner_set, [AdminHandle, ObjectNames, Scope]}).


%% @spec admin_owner_release(AdminHandle, ObjectNames, Scope) -> Result
%% @equiv safe_imm_om:admin_owner_release(AdminHandle, ObjectNames, Scope)
admin_owner_release(AdminHandle, ObjectNames, Scope) ->
    om_call({admin_owner_release, [AdminHandle, ObjectNames, Scope]}).

%% @private
admin_owner_release(Server, AdminHandle, ObjectNames, Scope) ->
    om_call(Server, {admin_owner_release, [AdminHandle, ObjectNames, Scope]}).


%% @spec admin_owner_finalize(AdminHandle) -> Result
%% @equiv safe_imm_om:admin_owner_finalize(AdminHandle)
admin_owner_finalize(AdminHandle) ->
    om_call({admin_owner_finalize, [AdminHandle]}).

%% @private
admin_owner_finalize(Server, AdminHandle) ->
    om_call(Server, {admin_owner_finalize, [AdminHandle]}).


%% @spec admin_owner_clear(OmHandle, ObjectNames, Scope) -> Result
%% @equiv safe_imm_om:admin_owner_clear(OmHandle, ObjectNames, Scope)
admin_owner_clear(OmHandle, ObjectNames, Scope) ->
    om_call({admin_owner_clear, [OmHandle, ObjectNames, Scope]}).

%% @private
admin_owner_clear(Server, OmHandle, ObjectNames, Scope) ->
    om_call(Server, {admin_owner_clear, [OmHandle, ObjectNames, Scope]}).

%%======================================================================
%% 4.8 Configuration changes
%%======================================================================

%%======================================================================
%% 4.9 Administrative operations invocation
%%======================================================================
%% @spec admin_operation_invoke_2(AdminHandle, ObjectName, ContinuationId, 
%%                                OperationId, Params, Timeout) -> Result
%% @equiv safe_imm_om:admin_operation_invoke_2(AdminHandle, ObjectName, 
%%                                             ContinuationId, OperationId,
%%                                             Params, Timeout)
admin_operation_invoke_2(AdminHandle, ObjectName, ContinuationId, OperationId,
			 Params, Timeout) ->
    om_call({admin_operation_invoke_2, [AdminHandle, ObjectName, 
					ContinuationId, OperationId,
					Params, Timeout]}).

%% @private
admin_operation_invoke_2(Server, AdminHandle, ObjectName, ContinuationId, 
			 OperationId, Params, Timeout) ->
    om_call(Server, {admin_operation_invoke_2, [AdminHandle, ObjectName, 
						ContinuationId, OperationId,
						Params, Timeout]}).

%%%===================================================================
%%% Support functions
%%%===================================================================
%% @private
get_testnode() ->
    get_testnode(?SERVER).

%% @private
get_testnode(Server) ->
    rct_safe_rpc:get_testnode(Server).

%% @private
get_node_status() ->
    get_node_status(?SERVER).

%% @private
get_node_status(Server) ->
    rct_safe_rpc:get_node_status(Server).

%% @private
subscribe_testnode() ->
    subscribe_testnode(?SERVER).

%% @private
subscribe_testnode(Server) ->
    rct_safe_rpc:subscribe_testnode(Server).

%% @private
unsubscribe_testnode() ->
    unsubscribe_testnode(?SERVER).

%% @private
unsubscribe_testnode(Server) ->
    rct_safe_rpc:unsubscribe_testnode(Server).

%%%===================================================================
%%% CT Hooks callbacks
%%%===================================================================
%% @private
id(Opts) ->
    proplists:get_value(instance_name, Opts, ?SERVER).


%% @private
init(Id, Opts) ->
    rct_safe_rpc:init(Id, modify_opts(Opts)).


%% @private
pre_init_per_suite(Suite, Config, State) ->
    rct_safe_rpc:pre_init_per_suite(Suite, Config, State).


%% @private
pre_init_per_testcase(TC, Config, State) ->
    rct_safe_rpc:pre_init_per_testcase(TC, Config, State).
	    

%% @private
on_tc_fail(TC, Reason, State) ->
    rct_safe_rpc:on_tc_fail(TC, Reason, State).


%% @private
terminate(State) ->
    rct_safe_rpc:terminate(State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start([]).

%% @private
start(Opts) ->
    NewOpts = modify_opts(Opts),
    rct_safe_rpc:start(NewOpts).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stop the server.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    stop(?SERVER).


%% @private
stop(Server) ->
    rct_safe_rpc:stop(Server).

%%%===================================================================
%%% Debug functions
%%%===================================================================
%% @private
dump() ->
    dump(?SERVER).


%% @private
dump(Server) ->
    rct_safe_rpc:dump(Server).

%%%===================================================================
%%% Internal functions
%%%===================================================================
modify_opts(Opts) ->
    Server = lta(proplists:get_value(instance_name, Opts, ?SERVER)),
    SafeDebugLev = proplists:get_value(safe_debug_level, Opts),
    SafeServices = {safe_services, [{imm, SafeDebugLev}]}, 
    NewOpts = lists:keystore(instance_name, 1, Opts, {instance_name, Server}),
    [SafeServices | NewOpts].


om_call(Arg) ->
    om_call(?SERVER, Arg).


om_call(Server, {Func, Arg}) ->
    gen_server:call(Server, {safe_call, {safe_imm_om, Func, Arg}}, 20000).


get_cb_fun(Cbs, Srv) when is_atom(Srv), Srv =/= undefined ->
    get_cb_fun(Cbs, whereis(Srv));

get_cb_fun(Cbs, Srv) 
  when is_record(Cbs, safe_imm_callbacks) ->
    AdmOpCb = Cbs#safe_imm_callbacks.admin_operation_invoke,
    Cbs#safe_imm_callbacks{admin_operation_invoke = get_cb_fun(AdmOpCb, Srv)};

get_cb_fun(Cbs, Srv) when is_pid(Srv) ->
    rct_safe_rpc_lib:get_cb_fun(Cbs, Srv, ?SAFE_CB_TAG, ?SAFE_CB_RES_TAG).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.



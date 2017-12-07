%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	rct_safe_imm_oi_rpc.erl %
%% @author etxpeno
%% @copyright Ericsson AB 2012-2016
%% @doc
%% This module provides an RPC API towards safe_imm_oi executing on
%% a remote node. The safe_imm_oi API is part of the OTP Safe application.
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
%% the module safe_imm_oi in OTP Safe application.
%% The API functions have the same names and parameters, with one exception;
%% the {@link initialize_2/1} function in this module do not accept a module
%% name as Callback function. The atom undefined may be used if no callbacks
%% are needed. All API functions also has a corresponding higher arity function
%% that takes the registered name of the server as first parameter.
%% See the OTP Safe documentation for more information about the API functions.
%%
%% The API in this module can also be used together with the `rct_safe_rpc' hook.
%% If several of the Safe services IMM, LOG or NTF is to be used in a test
%% suite, it is required to use the `rct_safe_rpc' hook instead of
%% `rct_safe_imm_oi_rpc'. See {@link rct_safe_rpc} for more information.
%%
%% The `rct_safe_imm_oi_rpc' hook is specified in the `suite/0' function of the
%% test suite as described below:
%%
%% ```suite() ->
%%        [{ct_hooks, [{safe_imm_oi_rpc, Opts}]}].
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
%%    - Determines if all active OI handles should be automatically finalized
%%      at test case failure. <br/>
%%      Default is `false'<br/>
%%
%% In a multinode scenario several instances may be specified using different
%% instance names as in this example.
%%
%% ```suite() ->
%%     [{ct_hooks, [{safe_imm_oi_rpc, [{instance_name, safe_oi_1},
%%                                     {du_no, 1}]},
%%                  {safe_imm_oi_rpc, [{instance_name, safe_oi_2}
%%                                     {du_no, 2}]}].'''
%%
%% @end
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R2A/1      2012-10-23 eolaand     Created
%%% R2A/2      2013-04-12 etxivri     Updated for target and to be used for
%%%                                   measure response time.
%%% R2A/3      2013-04-12 etxivri     Updated previous what.
%%% R2A/4      2013-05-02 etxivri     Changed path to safe to 1.2.8
%%% R2A/5      2013-05-15 etxivri     Updates to subscribe the staus of the
%%%                                   testnode.
%%%                                   Needed when node goes down and up.
%%%                                   Added also option SafeDebugLevel
%%% R2A/6      2013-05-30 etxivri     Removed code:add_path safe due to safe
%%%                                   now exist in DUMMY cxp and path sets when
%%%                                   testnode starts.
%%% R2A/7      2013-05-30 etxivri     Updates in case the testnode is down when
%%%                                   hook start.
%%% R2A/9      2013-08-13 eolaand     Complete rewrite to adapt to a common
%%%                                   generic format for all SAFE hooks.
%%% R2A/11     2013-08-19 eolaand     Use common gen_server implementation
%%%                                   in safe_rpc instead of specific version.
%%% R2A/12     2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/14     2014-06-10 etxberb     Added ccb_set_error_string/3 &
%%%                                   ccb_set_error_string/4.
%%% R4A/1      2015-06-08 eolaand     Updates for cluster.
%%% R5A/1      2016-02-18 etxpeno     add support for admin_operation_result_o2
%%% ----------------------------------------------------------
-module(rct_safe_imm_oi_rpc).

%% OI API
-export([
	 initialize_2/1,
	 initialize_2/2,
         augmentccb_initialize/2,
         augmentccb_initialize/3,
         augmentccb_initialize/4,
	 finalize/1,
	 finalize/2,

	 ccb_set_error_string/3,
	 ccb_set_error_string/4,
	 implementer_set/2,
	 implementer_set/3,
	 implementer_clear/1,
	 implementer_clear/2,
	 class_implementer_set/2,
	 class_implementer_set/3,
	 class_implementer_release/2,
	 class_implementer_release/3,
	 object_implementer_set/3,
	 object_implementer_set/4,
	 object_implementer_release/3,
	 object_implementer_release/4,

	 rt_object_create_2/4,
	 rt_object_create_2/5,
	 rt_object_delete/2,
	 rt_object_delete/3,
	 rt_object_update_2/3,
	 rt_object_update_2/4,

	 admin_operation_result/3,
	 admin_operation_result/4,

	 admin_operation_result_o2/4,
	 admin_operation_result_o2/5
	]).

%% Support API
-export([get_testnode/0,
	 get_testnode/1,
	 get_node_status/0,
	 get_node_status/1,
	 subscribe_testnode/0, %% Used when node restarts from TC.
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

%%--------------------------------------------------------------------
%% @doc
%% Initializes an OI handle.<br/>
%% Callbacks is a fun/1 that returns either ok or {error, safe_ais_error()}.
%% <br/>Otherwise this function is equivalent to
%% <a href="safe_imm_oi.html#initialize_2-1">
%% <tt>safe_imm_oi:initialize_2(Callbacks)</tt></a>.
%%
%% @spec initialize_2(Callbacks) -> Result
%%
%% Callbacks = fun() | safe_imm_oi_callbacks_2() | undefined
%% Result = {ok, OiHandle, SupportedVersion} | {error, safe_ais_error()}
%% OiHandle = safe_imm_oi_handle()
%% SupportedVersion = safe_version()
%%
%% @end
%%--------------------------------------------------------------------
initialize_2(Callbacks)
  when is_function(Callbacks);
       is_record(Callbacks, safe_imm_oi_callbacks_2);
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    oi_call({initialize_2, [get_cb_fun(Callbacks, ?SERVER)]}).

%% @private
initialize_2(Server, Callbacks)
  when is_function(Callbacks);
       is_record(Callbacks, safe_imm_oi_callbacks_2);
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    oi_call(Server, {initialize_2, [get_cb_fun(Callbacks, Server)]}).


%% @spec augmentccb_initialize(OiHandle, CcbId) -> Result
%% @equiv safe_imm_oi:augmentccb_initialize(OiHandle,CcbId)
augmentccb_initialize(_OiHandle, _CcbId) ->
    {error, not_implemented_yet}.

%% @spec augmentccb_initialize(OmHandle, OiHandle, CcbId) -> Result
%% @equiv safe_imm_oi:augmentccb_initialize(OmHandle,OiHandle,CcbId)
augmentccb_initialize(OmHandle, _OiHandle, _CcbId)
  when is_record(OmHandle, imm_om_handle)->
    {error, not_implemented_yet};

augmentccb_initialize(_Server, _OiHandle, _CcbId) ->
    {error, not_implemented_yet}.


%% @private
augmentccb_initialize(_Server, _OmHandle, _OiHandle, _CcbId) ->
    {error, not_implemented_yet}.


%% @spec finalize(OiHandle) -> Result
%% @equiv safe_imm_oi:finalize(OiHandle)
finalize(OiHandle) ->
    oi_call({finalize, [OiHandle]}).

%% @private
finalize(Server, OiHandle) ->
    oi_call(Server, {finalize, [OiHandle]}).


%% @spec ccb_set_error_string(OiHandle, CcbId, ErrorString) -> Result
%% @equiv safe_imm_oi:ccb_set_error_string(OiHandle, CcbId, ErrorString)
ccb_set_error_string(OiHandle, CcbId, ErrorString)
  when is_integer(CcbId) ;
       is_list(ErrorString) ->
    oi_call({ccb_set_error_string, [OiHandle, CcbId, ErrorString]}).

%% @private
ccb_set_error_string(Server, OiHandle, CcbId, ErrorString)
  when is_integer(CcbId) ;
       is_list(ErrorString) ->
    oi_call(Server, {ccb_set_error_string, [OiHandle, CcbId, ErrorString]}).


%% @spec implementer_set(OiHandle, ImplementerName) -> Result
%% @equiv safe_imm_oi:implementer_set(OiHandle, ImplementerName)
implementer_set(OiHandle, ImplementerName)
  when is_list(ImplementerName) ->
    oi_call({implementer_set, [OiHandle, ImplementerName]}).

%% @private
implementer_set(Server, OiHandle, ImplementerName)
  when is_list(ImplementerName) ->
    oi_call(Server, {implementer_set, [OiHandle, ImplementerName]}).


%% @spec implementer_clear(OiHandle) -> Result
%% @equiv safe_imm_oi:implementer_clear(OiHandle)
implementer_clear(OiHandle) ->
    oi_call({implementer_clear, [OiHandle]}).

%% @private
implementer_clear(Server, OiHandle) ->
    oi_call(Server, {implementer_clear, [OiHandle]}).


%% @spec class_implementer_set(OiHandle, ClassName) -> Result
%% @equiv safe_imm_oi:class_implementer_set(OiHandle, ClassName)
class_implementer_set(OiHandle, ClassName)
  when is_list(ClassName) ->
    oi_call({class_implementer_set, [OiHandle, ClassName]}).

%% @private
class_implementer_set(Server, OiHandle, ClassName)
  when is_list(ClassName) ->
    oi_call(Server, {class_implementer_set, [OiHandle, ClassName]}).


%% @spec class_implementer_release(OiHandle, ClassName) -> Result
%% @equiv safe_imm_oi:class_implementer_release(OiHandle, ClassName)
class_implementer_release(OiHandle, ClassName)
  when is_list(ClassName) ->
    oi_call({class_implementer_release, [OiHandle, ClassName]}).

%% @private
class_implementer_release(Server, OiHandle, ClassName)
  when is_list(ClassName) ->
    oi_call(Server, {class_implementer_release, [OiHandle, ClassName]}).


%% @spec object_implementer_set(OiHandle, ObjectName, Scope) -> Result
%% @equiv safe_imm_oi:object_implementer_set(OiHandle, ObjectName, Scope)
object_implementer_set(OiHandle, ObjectName, Scope)
  when is_list(ObjectName),
       is_integer(Scope), Scope >= 0 ->
    oi_call({object_implementer_set, [OiHandle, ObjectName, Scope]}).

%% @private
object_implementer_set(Server, OiHandle, ObjectName, Scope)
  when is_list(ObjectName),
       is_integer(Scope), Scope >= 0 ->
    oi_call(Server, {object_implementer_set, [OiHandle, ObjectName, Scope]}).


%% @spec object_implementer_release(OiHandle, ObjectName, Scope) -> Result
%% @equiv safe_imm_oi:object_implementer_release(OiHandle, ObjectName, Scope)
object_implementer_release(OiHandle, ObjectName, Scope)
  when is_list(ObjectName),
       is_integer(Scope), Scope >= 0 ->
    oi_call({object_implementer_release, [OiHandle, ObjectName, Scope]}).

%% @private
object_implementer_release(Server, OiHandle, ObjectName, Scope)
  when is_list(ObjectName),
       is_integer(Scope), Scope >= 0 ->
    oi_call(Server,
	    {object_implementer_release, [OiHandle, ObjectName, Scope]}).


%% @spec rt_object_create_2(OiHandle, ClassName, ParentName, AttrValues) ->
%%           Result
%% @equiv safe_imm_oi:rt_object_create_2(OiHandle, ClassName, ParentName,
%%                                       AttrValues)
rt_object_create_2(OiHandle, ClassName, ParentName, AttrValues)
  when is_list(ClassName), is_list(AttrValues) ->
    oi_call({rt_object_create_2,
	     [OiHandle, ClassName, ParentName, AttrValues]}).

%% @private
rt_object_create_2(Server, OiHandle, ClassName, ParentName, AttrValues)
  when is_list(ClassName), is_list(AttrValues) ->
    oi_call(Server, {rt_object_create_2,
		     [OiHandle, ClassName, ParentName, AttrValues]}).


%% @spec rt_object_delete(OiHandle, ObjectName) -> Result
%% @equiv safe_imm_oi:rt_object_delete(OiHandle, ObjectName)
rt_object_delete(OiHandle, ObjectName)
  when is_list(ObjectName) ->
    oi_call({rt_object_delete, [OiHandle, ObjectName]}).

%% @private
rt_object_delete(Server, OiHandle, ObjectName)
  when is_list(ObjectName) ->
    oi_call(Server, {rt_object_delete, [OiHandle, ObjectName]}).


%% @spec rt_object_update_2(OiHandle, ObjectName, AttrMods) -> Result
%% @equiv safe_imm_oi:rt_object_update_2(OiHandle, ObjectName, AttrMods)
rt_object_update_2(OiHandle, ObjectName, AttrMods)
  when is_list(ObjectName), is_list(AttrMods) ->
    oi_call({rt_object_update_2, [OiHandle, ObjectName, AttrMods]}).

%% @private
rt_object_update_2(Server, OiHandle, ObjectName, AttrMods)
  when is_list(ObjectName), is_list(AttrMods) ->
    oi_call(Server, {rt_object_update_2, [OiHandle, ObjectName, AttrMods]}).


%% @spec admin_operation_result(OiHandle, Invocation, ResultOfOperation) ->
%%           Result
%% @equiv safe_imm_oi:admin_operation_result(OiHandle, Invocation,
%%                                           ResultOfOperation)
admin_operation_result(_OiHandle, Invocation, ResultOfOperation)
  when is_integer(Invocation), Invocation >= 0,
       is_integer(ResultOfOperation), ResultOfOperation >= 0 ->
    {error, not_implemented_yet}.

%% @private
admin_operation_result(_Server, _OiHandle, Invocation, Result)
  when is_integer(Invocation), Invocation >= 0,
       is_integer(Result), Result >= 0 ->
    {error, not_implemented_yet}.

%% @spec admin_operation_result_o2(OiHandle, Invocation, ResultOfOperation, ResultParams) ->
%%           Result
%% @equiv safe_imm_oi:admin_operation_result_02(OiHandle, Invocation,
%%                                              ResultOfOperation, ResultParams)
admin_operation_result_o2(OiHandle, Invocation, ResultOfOperation, ResultParams) ->
    oi_call({admin_operation_result_o2, [OiHandle, Invocation, ResultOfOperation,
					 ResultParams]}).

%% @private
admin_operation_result_o2(Server, OiHandle, Invocation, ResultOfOperation,
			  ResultParams) ->
    oi_call(Server,
	    {admin_operation_result_o2, [OiHandle, Invocation, ResultOfOperation,
					 ResultParams]}).

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


oi_call(Arg) ->
    oi_call(?SERVER, Arg).


oi_call(Server, {Func, Arg}) ->
    gen_server:call(Server, {safe_call, {safe_imm_oi, Func, Arg}}, 20000).

get_cb_fun(Cbs, Srv) when is_atom(Srv), Srv =/= undefined ->
    get_cb_fun(Cbs, whereis(Srv));

get_cb_fun(Cbs, Srv) when is_record(Cbs, safe_imm_oi_callbacks_2) ->
    RtAUCb = Cbs#safe_imm_oi_callbacks_2.rt_attr_update,
    CcbOCCb = Cbs#safe_imm_oi_callbacks_2.ccb_object_create,
    CcbODCb = Cbs#safe_imm_oi_callbacks_2.ccb_object_delete,
    CcbOMCb = Cbs#safe_imm_oi_callbacks_2.ccb_object_modify,
    CcbCCb = Cbs#safe_imm_oi_callbacks_2.ccb_completed,
    CcbApCb = Cbs#safe_imm_oi_callbacks_2.ccb_apply,
    CcbAbCb = Cbs#safe_imm_oi_callbacks_2.ccb_abort,
    CcbAOpCb = Cbs#safe_imm_oi_callbacks_2.admin_operation,

    Cbs#safe_imm_oi_callbacks_2{rt_attr_update = get_cb_fun(RtAUCb, Srv),
			      ccb_object_create = get_cb_fun(CcbOCCb, Srv),
			      ccb_object_delete = get_cb_fun(CcbODCb, Srv),
			      ccb_object_modify = get_cb_fun(CcbOMCb, Srv),
			      ccb_completed = get_cb_fun(CcbCCb, Srv),
			      ccb_apply = get_cb_fun(CcbApCb, Srv),
			      ccb_abort = get_cb_fun(CcbAbCb, Srv),
			      admin_operation = get_cb_fun(CcbAOpCb, Srv)};

get_cb_fun(Cbs, Srv) when is_pid(Srv) ->
    rct_safe_rpc_lib:get_cb_fun(Cbs, Srv, ?SAFE_CB_TAG, ?SAFE_CB_RES_TAG).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.

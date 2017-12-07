%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	rct_safe_ntf_rpc.erl %
%% @author eolaand
%% @copyright Ericsson AB 2013-2015
%% @doc 
%% This module provides an RPC API towards safe_ntf executing on
%% a remote node. The safe_ntf API is part of the OTP Safe application.
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
%% the module safe_ntf in OTP Safe application.
%% The API functions have the same names and parameters, with one exception;
%% the {@link initialize/1} function in this module do not accept a module name
%% as Callback function. The atom undefined may be used if no callbacks are 
%% needed. All API functions also has a corresponding higher arity function
%% that takes the registered name of the server as first parameter. 
%% See the OTP Safe documentation for more information about the API functions.
%%
%% The API in this module can also be used together with the `rct_safe_rpc' hook.
%% If several of the Safe services IMM, LOG or NTF is to be used in a test 
%% suite, it is required to use the `rct_safe_rpc' hook instead of 
%% `rct_safe_ntf_rpc'. See {@link rct_safe_rpc} for more information.
%%
%% The `rct_safe_ntf_rpc' hook is specified in the `suite/0' function of the 
%% test suite as described below:
%%
%% ```suite() -> 
%%        [{ct_hooks, [{rct_safe_ntf_rpc, Opts}]}].
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
%%      node. The naming is useful when running multinode tests. 
%%      See example below.<br/>
%%      Default is `?MODULE' <br/>
%%
%%  `SafeDebugLevel = integer()' <br/>
%%    - Set debug level of Safe application. 0, 1 or 2 can be used. <br/>
%%      Default is undefined <br/>
%%
%%  `FoFFlag = boolean()' <br/>
%%    - Determines if all active NTF handles should be automatically finalized
%%      at test case failure. <br/>
%%      Default is `false'<br/>
%%
%% In a multinode scenario several instances may be specified using different
%% instance names as in this example.
%%
%% ```suite() -> 
%%        [{ct_hooks, [{rct_safe_ntf_rpc, [{instance_name, safe_ntf_1},
%%                                     {du_no, 1}]},
%%                     {rct_safe_ntf_rpc, [{instance_name, safe_ntf_2}
%%                                     {du_no, 2}]}].'''
%%
%% @end
%%% ----------------------------------------------------------
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
%%% R2A/2      2013-04-25 etxivri     Created
%%% R2A/3      2013-04-30 etxivri     Changed path to safe to 1.2.8
%%% R2A/4      2013-05-30 etxivri     Removed code:add_path safe due to safe 
%%%                                   now exist in DUMMY cxp and path sets when
%%%                                   testnode starts.
%%% R2A/5      2013-06-04 etxivri     Updates in case the testnode is down when
%%%                                   hook start.
%%% R2A/7      2013-08-13 eolaand     Complete rewrite in order to conform to
%%%                                   the generic structure of safe_rpc.
%%% R2A/8      2013-08-19 eolaand     Use common gen_server implementation
%%%                                   in safe_rpc instead of specific version.
%%% R2A/10      2013-09-10 etxivri    Updates due to renamed saf-rpc hooks.
%%% R4A/1      2015-06-08 eolaand     Updates for cluster.
%%%                                   
%%% ----------------------------------------------------------
-module(rct_safe_ntf_rpc).

%% NTF API
-export([initialize/1,
	 initialize/2,
	 initialize/3,
	 notification_send/2,
	 notification_send/3,
	 finalize/1,
	 finalize/2
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
-include_lib("safe/include/safe_ntf.hrl").
-include("rct_safe_rpc.hrl").

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% NTF API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes an NTF handle.<br/>
%% Callbacks is a fun/1 that returns either ok or {error, safe_ais_error()}.
%% <br/>Otherwise this function is equivalent to 
%% <a href="safe_ntf.html#initialize-1">
%% <tt>safe_ntf:initialize(Callbacks)</tt></a>.
%%
%% @spec initialize(Callbacks) -> Result
%%
%% Callbacks = fun() | undefined
%% Result = {ok, NtfHandle, SupportedVersion} | {error, safe_ais_error()}
%% NtfHandle = safe_ntf_handle()
%% SupportedVersion = safe_version()
%%           
%% @end
%%--------------------------------------------------------------------
initialize(Callbacks) 
  when is_function(Callbacks); 
       is_record(Callbacks, safe_ntf_callbacks);
       Callbacks =:= undefined ->
    ntf_call({initialize, [get_cb_fun(Callbacks, ?SERVER)]}).

%%--------------------------------------------------------------------
%% @doc
%% Initializes an NTF handle.<br/>
%% Callbacks is a fun/1 that returns either ok or {error, safe_ais_error()}.
%% <br/>Otherwise this function is equivalent to 
%% <a href="safe_ntf.html#initialize-2">
%% <tt>safe_ntf:initialize(Callbacks, Version)</tt></a>.
%%
%% @spec initialize(Callbacks, Version) -> Result
%%
%% Callbacks = fun() | undefined
%% Version = safe_version()
%% Result = {ok, NtfHandle, SupportedVersion} | {error, safe_ais_error()}
%% NtfHandle = safe_ntf_handle()
%% SupportedVersion = safe_version()
%%           
%% @end
%%--------------------------------------------------------------------
initialize(Callbacks, Version) 
  when is_function(Callbacks); 
       is_record(Callbacks, safe_ntf_callbacks);
       Callbacks =:= undefined ->
    ntf_call({initialize, [get_cb_fun(Callbacks, ?SERVER), Version]});

initialize(Server, Callbacks) 
  when is_function(Callbacks); 
       is_record(Callbacks, safe_ntf_callbacks);
       Callbacks =:= undefined ->
    ntf_call(Server, {initialize, [get_cb_fun(Callbacks, Server)]}).

%% @private
initialize(Server, Callbacks, Version) 
  when is_function(Callbacks); 
       is_record(Callbacks, safe_ntf_callbacks);
       Callbacks =:= undefined ->
    ntf_call(Server, {initialize, [get_cb_fun(Callbacks, Server), Version]}).


%% @spec finalize(Handle) -> Result
%% @equiv safe_ntf:finalize(Handle)
finalize(Handle) ->
    ntf_call({finalize, [Handle]}).

%% @private
finalize(Server, Handle) ->
    ntf_call(Server, {finalize, [Handle]}).

%% @spec notification_send(Handle,Notification) -> Result
%% @equiv safe_ntf:notification_send(Handle,Notification)
notification_send(Handle,Notification) ->
    ntf_call({notification_send, [Handle,Notification]}).

%% @private
notification_send(Server, Handle,Notification) ->
    ntf_call(Server, {notification_send, [Handle,Notification]}).

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
    proplists:get_value(instance_name, Opts, ?MODULE).


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
    SafeServices = {safe_services, [{ntf, SafeDebugLev}]}, 
    NewOpts = lists:keystore(instance_name, 1, Opts, {instance_name, Server}),
    [SafeServices | NewOpts].


ntf_call(Arg) ->
    ntf_call(?SERVER, Arg).


ntf_call(Server, {Func, Arg}) ->
    gen_server:call(Server, {safe_call, {safe_ntf, Func, Arg}}, 20000).


get_cb_fun(Cbs, Srv) when is_atom(Srv), Srv =/= undefined ->
    get_cb_fun(Cbs, whereis(Srv));

get_cb_fun(Cbs, Srv) 
  when is_record(Cbs, safe_ntf_callbacks) ->
    NtfCb = Cbs#safe_ntf_callbacks.notification_callback,
    NtfDiscCb = Cbs#safe_ntf_callbacks.notification_discarded_callback,
    Cbs#safe_ntf_callbacks{notification_callback = get_cb_fun(NtfCb, Srv),
			   notification_discarded_callback = 
			       get_cb_fun(NtfDiscCb, Srv)};

get_cb_fun(Cbs, Srv) when is_pid(Srv) ->
    rct_safe_rpc_lib:get_cb_fun(Cbs, Srv, ?SAFE_CB_TAG, ?SAFE_CB_RES_TAG).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.



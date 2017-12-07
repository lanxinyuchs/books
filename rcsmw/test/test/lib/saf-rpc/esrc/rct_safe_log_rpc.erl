%%% #0.    BASIC INFORMATION
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
%%% R2A/1      2013-06-27 eolaand     Created
%%% R2A/10     2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R4A/1      2015-06-08 eolaand     Updates for cluster.
%%% ----------------------------------------------------------
%% @author eolaand
%% @copyright Ericsson AB 2013-2015
%% @doc 
%% This module provides an RPC API towards safe_log executing on
%% a remote node. The safe_log API is part of the OTP Safe application.
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
%% the module safe_log in OTP Safe application.
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
%% `rct_safe_log_rpc'. See {@link rct_safe_rpc} for more information.
%%
%% The `rct_safe_log_rpc' hook is specified in the `suite/0' function of the 
%% test suite as described below:
%%
%% ```suite() -> 
%%        [{ct_hooks, [{rct_safe_log_rpc, Opts}]}].
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
%%    - Determines if all active LOG handles should be automatically finalized
%%      at test case failure. <br/>
%%      Default is `false'<br/>
%%
%% In a multinode scenario several instances may be specified using different
%% instance names as in this example.
%%
%% ```suite() -> 
%%        [{ct_hooks, [{rct_safe_log_rpc, [{instance_name, safe_log_1},
%%                                     {du_no, 1}]},
%%                     {rct_safe_log_rpc, [{instance_name, safe_log_2}
%%                                     {du_no, 2}]}].'''
%%
%% @end

-module(rct_safe_log_rpc).

%% LOG API
-export([
	 initialize/1,
	 initialize/2,
	 initialize/3,
	 finalize/1,
	 finalize/2,

	 stream_open_2/4,
	 stream_open_2/5,
	 stream_open_2/6,
	 stream_open_async_2/4,
	 stream_open_async_2/5,
	 stream_open_async_2/6,
	 write_log/3,
	 write_log/4,
	 write_log_async/4,
	 write_log_async/5,
	 stream_close/1,
	 stream_close/2,

	 limit_get/2,
	 limit_get/3
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
-include_lib("safe/include/safe_log.hrl").
-include("rct_safe_rpc.hrl").

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% LOG API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes a LOG handle.<br/>
%% Callbacks is a fun/1 that returns either ok or {error, safe_ais_error()}.
%% <br/>Otherwise this function is equivalent to 
%% <a href="safe_log.html#initialize-1">
%% <tt>safe_log:initialize(Callbacks)</tt></a>.
%%
%% @spec initialize(Callbacks) -> Result
%%
%% Callbacks = fun() | undefined
%% Result = {ok, Handle, SupportedVersion} | {error, safe_ais_error()}
%% Handle = safe_log_handle()
%% SupportedVersion = safe_version()
%%           
%% @end
%%--------------------------------------------------------------------
initialize(Callbacks) 
  when is_function(Callbacks); 
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    log_call({initialize, [get_cb_fun(Callbacks, ?SERVER)]}).

%%--------------------------------------------------------------------
%% @doc
%% Initializes a LOG handle.<br/>
%% Callbacks is a fun/1 that returns either ok or {error, safe_ais_error()}.
%% <br/>Otherwise this function is equivalent to 
%% <a href="safe_log.html#initialize-2">
%% <tt>safe_log:initialize(Callbacks, Version)</tt></a>.
%%
%% @spec initialize(Callbacks, Version) -> Result
%%
%% Callbacks = fun() | undefined
%% Version = safe_version()
%% Result = {ok, Handle, SupportedVersion} | {error, safe_ais_error()}
%% Handle = safe_log_handle()
%%           
%% @end
%%--------------------------------------------------------------------
initialize(Callbacks, Version) 
  when is_function(Callbacks); 
       is_record(Callbacks, safe_log_callbacks);
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    log_call({initialize, [get_cb_fun(Callbacks, ?SERVER), Version]});

initialize(Server, Callbacks)
  when is_function(Callbacks); 
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    log_call(Server, {initialize, [get_cb_fun(Callbacks, Server)]}).

%% @private
initialize(Server, Callbacks, Version) 
  when is_function(Callbacks); 
       is_record(Callbacks, safe_log_callbacks);
       Callbacks =:= undefined;
       Callbacks =:= ok ->
    log_call(Server, {initialize, [get_cb_fun(Callbacks, Server), Version]}).

%% @spec finalize(Handle) -> Result
%% @equiv safe_log:finalize(Handle)
finalize(Handle) ->
    log_call({finalize, [Handle]}).

%% @private
finalize(Server, Handle) ->
    log_call(Server, {finalize, [Handle]}).


%% @spec stream_open_2(Handle, StreamName, StreamOpenFlags, Timeout) -> Result
%% @equiv safe_log:stream_open_2(Handle, StreamName, StreamOpenFlags, Timeout)
stream_open_2(Handle, StreamName, StreamOpenFlags, Timeout) ->
    log_call({stream_open_2, [Handle, StreamName, StreamOpenFlags, Timeout]}).

%% @spec stream_open_2(Handle, StreamName, StreamOpenFlags, Timeout, 
%%                     FileCreateAttributes) -> Result
%% @equiv safe_log:stream_open_2(Handle, StreamName, StreamOpenFlags, Timeout, 
%%                               FileCreateAttributes)
stream_open_2(Handle, StreamName, StreamOpenFlags, Timeout, 
	      FileCreateAttributes)
  when not is_atom(Handle), is_list(StreamName) ->
    log_call({stream_open_2, [Handle, StreamName, StreamOpenFlags, Timeout, 
			  FileCreateAttributes]});

stream_open_2(Server, Handle, StreamName, StreamOpenFlags, Timeout) ->
    log_call(Server, {stream_open_2, [Handle, StreamName, StreamOpenFlags, 
				  Timeout]}).

%% @private
stream_open_2(Server, Handle, StreamName, StreamOpenFlags, Timeout, 
	      FileCreateAttributes) ->
    log_call(Server, {stream_open_2, [Handle, StreamName, StreamOpenFlags, Timeout,
				  FileCreateAttributes]}).


%% @private
stream_open_async_2(_Handle, _StreamName, _StreamOpenFlags, _Invocation) ->
    {error, not_implemented_yet}.


%% @private
stream_open_async_2(_Server, _Handle, _StreamName, _StreamOpenFlags, 
		    _Invocation) 
  when is_atom(_Server) ->
    {error, not_implemented_yet};

stream_open_async_2(_Handle, _StreamName, _StreamOpenFlags, _Invocation, 
		    _FileCreateAttributes) ->
    {error, not_implemented_yet}.


%% @private
stream_open_async_2(_Server, _Handle, _StreamName, _StreamOpenFlags, 
		    _Invocation, _FileCreateAttributes) ->
    {error, not_implemented_yet}.


%% @private
write_log(_StreamHandle, _Timeout, _LogRecord) ->
    {error, not_implemented_yet}.

%% @private
write_log(_Server, _StreamHandle, _Timeout, _LogRecord) ->
    {error, not_implemented_yet}.


%% @spec write_log_async(StreamHandle, Invocation, LogAckFlags, LogRecord) -> 
%%           Result
%% @equiv safe_log:write_log_async(StreamHandle, Invocation, LogAckFlags, 
%%                                 LogRecord)
write_log_async(StreamHandle, Invocation, LogAckFlags, LogRecord) ->
    log_call({write_log_async, [StreamHandle, Invocation, LogAckFlags, LogRecord]}).

%% @private
write_log_async(Server, StreamHandle, Invocation, LogAckFlags, LogRecord) ->
    log_call(Server, {write_log_async, 
		  [StreamHandle, Invocation, LogAckFlags, LogRecord]}).


%% @spec stream_close(StreamHandle) -> Result
%% @equiv safe_log:stream_close(StreamHandle)
stream_close(StreamHandle) ->
    log_call({stream_close, [StreamHandle]}).

%% @private
stream_close(Server, StreamHandle) ->
    log_call(Server, {stream_close, [StreamHandle]}).


%% @spec limit_get(Handle, LimitId) -> Result
%% @equiv safe_log:limit_get(Handle, LimitId)
limit_get(Handle, LimitId) ->
    log_call({limit_get, [Handle, LimitId]}).


%% @private
limit_get(Server, Handle, LimitId) ->
    log_call(Server, {limit_get, [Handle, LimitId]}).


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
    SafeServices = {safe_services, [{log, SafeDebugLev}]}, 
    NewOpts = lists:keystore(instance_name, 1, Opts, {instance_name, Server}),
    [SafeServices | NewOpts].


log_call(Arg) ->
    log_call(?SERVER, Arg).


log_call(Server, {Func, Arg}) ->
    gen_server:call(Server, {safe_call, {safe_log, Func, Arg}}, 20000).


get_cb_fun(Callbacks, Srv) when is_atom(Srv), Srv =/= undefined ->
    get_cb_fun(Callbacks, whereis(Srv));

get_cb_fun(Callbacks, Srv) when is_record(Callbacks, safe_log_callbacks) ->
    FSCb = get_cb_fun(Callbacks#safe_log_callbacks.filter_set_callback, Srv),
    SOCb = get_cb_fun(Callbacks#safe_log_callbacks.stream_open_callback, Srv),
    WLCb = get_cb_fun(Callbacks#safe_log_callbacks.write_log_callback, Srv),
    Callbacks#safe_log_callbacks{filter_set_callback = FSCb,
				 stream_open_callback = SOCb,
				 write_log_callback = WLCb};

get_cb_fun(Callbacks, Srv) when is_pid(Srv) ->
    rct_safe_rpc_lib:get_cb_fun(Callbacks, Srv, ?SAFE_CB_TAG, ?SAFE_CB_RES_TAG).


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.



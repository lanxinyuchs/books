%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_pei_proxy_session.erl %
%%% Author:	eolaand
%%% 
%%% Description: Module handling the PEI session for pes_pei_proxy.
%%%
%%% Modules used: 
%%%
%%% ----------------------------------------------------------
-module(pes_pei_proxy_session).
-id('Updated by CCase').
-vsn('/main/R3A/9').
-date('2015-05-12').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R3A/1    2014-11-28 eolaand     Created
%%% ----------------------------------------------------------
%% Server API
-export([start/4, 
	 stop/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% Debug
-export([dump/1]).

%-compile([export_all]).

-define(SERVER, ?MODULE).
-define(TEST_INFO(Format, Items), pes_test_lib:ct_log(Format, Items)).
-define(TEST_DEBUG(Format, Items), pes_test_lib:ct_log(Format, Items)).
-define(CHILD_PREFIX, "ift_client_").

-define(TIMEOUT, 10000).

-define(PES_PEI_PROXY, pes_pei_proxy).
-define(ERL_PEI, pesPeI).
-define(CB_FUNCS, [peiEventJobCallback, peiMEAttrUpdateCallback]).

%% must match definition in master.h of ift_app
-define(PEI, 20).

-define(PEI_INITIALIZE, 10000).
%% PEI fcns. Must match IFT_APP codes
-define(IFT_PEI_INITIALIZE, 1).
-define(IFT_PEI_SETUP_CALLBACK, 2).
-define(IFT_PEI_EVENT_JOB, 3).
-define(IFT_PEI_ME_ATTR_UPDATE, 4).
-define(IFT_PEI_FINALIZE, 5).
-define(IFT_PEI_EMUL_DEATH, 6).

-define(ERR_UNKNOWN_MAP_ID, "Unknown EventMapId").

-define(PEI_OK, 1).
-define(PEI_UNKNOWN_EVENT_MAP_ID, 3).
-define(PEI_BAD_PARAMETER, 4).
-define(PEI_INTERNAL_ERROR, 5).


-record(state, {session,
		parent,
		app_pid,
		handle,
		callbacks,
		cs_node,
		remote_node,
		erl_api = false,
		trace_child = false
	       }).

%%%===================================================================
%%% PEI functions
%%%===================================================================
%%%===================================================================
%%% Support functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% Starts the server
%%
%%--------------------------------------------------------------------
start(Session, RNode, CSNode, Opts) ->
    ct:pal("start pes_pei_proxy_session (~p).~nRNode = ~p, CSNode = ~p~n"
	   "Opts = ~p", [Session, RNode, CSNode, Opts]),
    %% Default options are already set in proxy.
    UseErlAPI = proplists:get_value(erl_api, Opts),
    Trace = (not UseErlAPI) andalso proplists:get_value(trace_child, Opts),
    State = #state{session = Session, 
		   parent = self(),
		   remote_node = RNode,
		   cs_node = CSNode, 
		   erl_api = UseErlAPI,
		   trace_child = Trace},
    Res = gen_server:start({local, Session}, ?MODULE, State, []),
    ct:pal("pes_pei_proxy_session (~p) started: ~p", [Session, Res]),
    Res.

%%--------------------------------------------------------------------
%% @private
%% Stop the server.
%%
%%--------------------------------------------------------------------
stop(Server) ->
    gen_server:call(Server, stop).
    

%%%===================================================================
%%% Debug functions
%%%===================================================================
%% @private
dump(Server) ->
    gen_server:call(Server, dump).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(State) ->
    erlang:monitor_node(State#state.remote_node, true),
    erlang:monitor(process, State#state.parent),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% Handling call messages
%%
%%--------------------------------------------------------------------
handle_call({pei_call, {Func, Args}}, _From, State)
  when Func =:= peiFinalize ->
    ct:pal("[~p(~p)] peiFinalize received from test app", 
	   [?MODULE, State#state.session]),
    Reply = call_pei(Func, Args, State),
    {stop, normal, Reply, State};   

handle_call({pei_call, {Func, Args}}, _From, State) ->
    ct:pal("[~p(~p)] ~p received from test app", 
	   [?MODULE, State#state.session, Func]),
    Reply = call_pei(Func, Args, State),
    {reply, Reply, State};

handle_call({emulAppDeath, _CloseSocket}, _From, State) ->
    {reply, {error, not_implemented_yet}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(dump, _From, State) ->
    {reply, {ok, State}, State};

handle_call(Call, _From, State) ->
    {reply, {error, {invalid_call, Call}}, State}.

%%--------------------------------------------------------------------
%% Handling cast messages
%%
%%--------------------------------------------------------------------
handle_cast({{Func, [EventMap, Callbacks, CbMod, AppPid]}, From}, 
	    State)
  when Func =:= peiInitialize ->
    case handle_pei_initialize(State, Func, [EventMap, Callbacks]) of
	{ok, Handle} ->
	    ct:pal("peiInitialize Handle ~p, self ~p", [Handle, self()]),
	    Cbs = to_callbacks(Callbacks, CbMod),
	    gen_server:reply(From, {ok, self()}),
	    erlang:monitor(process, AppPid),
	    {noreply, State#state{handle = Handle, callbacks = Cbs,
				  app_pid = AppPid}};
	Error ->
	    gen_server:reply(From, to_init_error(Error)),
	    {stop, to_stop_reason(Error), State}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info({nodedown, _Node}, State) ->
    ct:pal("[~p] Node ~p is down!", [?MODULE, _Node]),
    {stop, normal, State};   

handle_info({signal, {Callback, Arg}} = _Msg, State) ->
    %% ct:pal("[~p] Got msg from IFT app: ~p", [?MODULE, Msg]),
    exec_callback(State, Callback, Arg),
    {noreply, State};

handle_info({Callback, Arg}, State) when is_atom(Callback), is_tuple(Arg) -> 
    exec_callback(State, Callback, to_list(Arg)),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    call_pei(peiFinalize, [], State),
    {stop, normal, State};   

handle_info(_Info, State) ->
    ct:pal("~p(~p): Received unexpected Info: ~p", 
	   [?MODULE, State#state.session, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ct:pal("pes_pei_proxy_session (~p) terminating: ~p", 
	   [State#state.session, Reason]),
    case Reason of
	normal ->
	    ok;
	_Other ->
	    call_pei(peiFinalize, [], State)
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% Convert process state when code is changed
%%
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_pei_initialize(State, peiInitialize = Func, [EventMap, Callbacks]) 
  when State#state.erl_api =:= true ->
    pei_rpc_call(Func, [EventMap, Callbacks, ?ERL_PEI, self()], State);

handle_pei_initialize(State, Func, [EventMap, Callbacks]) ->
    case start_pei_ift_client(State) of
	ok ->
	    ?TEST_INFO("~w: PEI IFT client session started ~n", [?MODULE]),
	    start_session_trace(State),
	    UserData = atl(State#state.session) ++ "_user_data",
	    CbFlags = to_cb_flags(Callbacks),
	    InitMsg = {EventMap, CbFlags, UserData},
	    send_initialize(State, Func, InitMsg);
	Error ->
	    Error
    end.


call_pei(_Func, _Arg, State) when State#state.handle =:= undefined ->
    ok;

call_pei(Func, Arg, State) 
  when State#state.erl_api ->
    pei_rpc_call(Func, [State#state.handle | Arg], State);

call_pei(Func, Arg, State) ->
    call_pei_proxy(Func, Arg, State).
    

exec_callback(State, Callback, Arg) when is_list(Arg) ->
    CbFunc = get_cb_func(Callback), 
    Module = get_cb_mod(State#state.callbacks, CbFunc),
    exec_callback(State, Module, CbFunc, Arg); 

exec_callback(State, Callback, Arg) ->
    [_ | NewArg] = to_list(Arg),
    exec_callback(State, Callback, NewArg).


exec_callback(State, Module, CbFunc, Arg) 
  when Module =/= false, Module =/= undefined ->
    AppPid = State#state.app_pid,
    NewArg = [AppPid | Arg],
    ct:log("~p exec callback: {~p, ~p, ~p}", 
	   [?MODULE, Module, CbFunc, NewArg]), 
    spawn(fun() ->
		  apply(Module, CbFunc, NewArg)
	  end);

exec_callback(_State, _Module, _CbFunc, _Arg) ->
    ok.


get_cb_func(peiEventJob) ->
    peiEventJobCallback;
get_cb_func(peiMEAttrUpdate) ->
    peiMEAttrUpdateCallback.


get_cb_mod(Mod, _Func) when is_atom(Mod) ->
    Mod;
get_cb_mod(Callbacks, Func) when is_list(Callbacks) ->
    proplists:get_value(Func, Callbacks).

%%%===================================================================
%%% RPC call
%%%===================================================================
pei_rpc_call(Function, Args, State) ->
    pei_rpc_call(?ERL_PEI, Function, Args, State).


pei_rpc_call(Module, Function, Args, State) ->
    ct:pal("~p pei_rpc_call   ~p~n", 
	   [?MODULE, {Module, Function, Args, State#state.cs_node}]),
    rpc:call(State#state.cs_node, Module, Function, Args, 20000).

%%%===================================================================
%%% rct_proxy functions
%%%===================================================================
start_pei_ift_client(State) when State#state.erl_api =:= false ->
    Session = State#state.session,
    IFTNode = State#state.remote_node,
    {start, IFTNode} ! {self(), Session, ?PEI},
    case wait_ift_reply(?TIMEOUT) of
	{ok, client_started} ->
	    ok;
	_Other ->
	    ct:pal("Got unexpected reply from IFT: ~p", [_Other]),
	    {error, failed_to_start_client}
    end;

start_pei_ift_client(_State) ->
    ok.


stop_pei_ift_client(IFTNode, Session) -> 
    {stop, IFTNode} ! {self(), Session},
    case wait_ift_reply(?TIMEOUT) of
	{ok, client_stopped} ->
	    ok;
	_Other ->
	    {error, failed_to_stop_client}
    end.


send_initialize(State, Func, InitMsg) ->
    FuncId = func_id(Func),
    case call_pei_proxy(FuncId, InitMsg, State) of
	{ok, "handle" ++ _ = Handle} -> 
	    ?TEST_INFO("~w: PEI IFT session: ~p, Handle = ~p~n", 
		       [?MODULE, Func, Handle]),
	    send_setup_cb(State, Handle);
	{_OkOrError, Other} ->
	    ?TEST_INFO("~w: PEI IFT session: ~p failed ~p~n", 
		       [?MODULE, Func, Other]),
	    {error, Other}
    end.
    

send_setup_cb(State, Handle) ->
    FuncId = ?IFT_PEI_SETUP_CALLBACK,
    case call_pei_proxy(FuncId, {Handle}, State) of
	{ok, _Socket} ->
	    ?TEST_INFO("~w: PEI IFT session: setup cb socket ~p~n", 
		       [?MODULE, _Socket]),
	    {ok, Handle};
	Error ->
	    ?TEST_INFO("~w: PEI IFT session: setup cb failed ~p~n", 
		       [?MODULE, Error]),
	    Error
    end.


wait_ift_reply(Timeout) ->
    receive
	{ok, _} = Msg ->
	    Msg;
	{ok} ->
	    ok;
	{error, _} = E ->
	    E;
	{error, E1, E2} ->
	    {error, {E1, E2}}
    after Timeout ->
	    {error, timeout}
    end.


call_pei_proxy(FuncId, Arg, State) when is_integer(FuncId), is_tuple(Arg) ->
    Session = State#state.session,
    IFTNode = State#state.remote_node,
    {send, IFTNode} ! {self(), Session, FuncId, Arg},
    ct:pal("Send to proxy:~n~p ! ~p", 
	   [{send, IFTNode}, {self(), Session, FuncId, Arg}]),
    Res = wait_ift_reply(?TIMEOUT),
    if FuncId =:= ?IFT_PEI_FINALIZE ->
	    stop_pei_ift_client(IFTNode, Session);
       true ->
	    Res
    end;

call_pei_proxy(Func, Arg, State) ->
    FuncId = func_id(Func),
    NewArg = to_tuple([State#state.handle | Arg]),
    call_pei_proxy(FuncId, NewArg, State).


start_session_trace(State) 
  when State#state.trace_child =:= true, 
       State#state.erl_api =:= false ->
    timer:sleep(3000),
    Session = State#state.session,    
    ?PES_PEI_PROXY:start_trace(?CHILD_PREFIX ++ atl(Session), 
			       State#state.cs_node);

start_session_trace(_) ->
    ok.
    

func_id(peiInitialize) ->
    ?IFT_PEI_INITIALIZE;
func_id(peiFinalize) ->
    ?IFT_PEI_FINALIZE.


%%%===================================================================
%%% lib functions
%%%===================================================================
to_init_error({error, {_, ?PEI_UNKNOWN_EVENT_MAP_ID}}) ->
    {error, ?ERR_UNKNOWN_MAP_ID};

to_init_error(Error) ->
    Error.


to_stop_reason({error, ?ERR_UNKNOWN_MAP_ID}) ->
    normal;
to_stop_reason({error, {_, ?PEI_INITIALIZE + ?PEI_UNKNOWN_EVENT_MAP_ID}}) ->
    normal;
to_stop_reason(Error) ->
    Error.    


%% lta(L) when is_list(L) ->
%%     list_to_atom(L);
%% lta(A) when is_atom(A) ->
%%     A.

atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) when is_list(L) ->
    L.


to_list(T) when is_tuple(T) ->
    tuple_to_list(T);
to_list(L) when is_list(L) ->
    L;
to_list(I) when is_integer(I) ->
    integer_to_list(I).


to_tuple(L) when is_list(L) ->
    list_to_tuple(L);
to_tuple(T) when is_tuple(T) ->
    T.


to_cb_flags(Callbacks) ->
    to_tuple([Bool || {_Func, Bool} <- to_callbacks(Callbacks)]).
    

to_callbacks(Callbacks) ->
    [{Func, proplists:get_value(Func, Callbacks, false)} || 
	Func <- ?CB_FUNCS].


to_callbacks(Callbacks, CbMod) ->
    [{Func, to_cb_mod(Bool, CbMod)} || 
	{Func, Bool} <- to_callbacks(Callbacks)].


to_cb_mod(true, CbMod) ->
    CbMod;
to_cb_mod(_False, _cbMod) ->
    false.
    


%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_pmi_proxy_session.erl %
%%% Author:	eolaand
%%% 
%%% Description: Module handling the PM session for pms_pmi_proxy.
%%%
%%% Modules used: rct_proxy, pms_c_util
%%%
%%% ----------------------------------------------------------
-module(pms_pmi_proxy_session).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R11A/1').
-date('2017-10-20').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R3A/1    2014-08-28 eolaand     Created
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

-define(SERVER, ?MODULE).

-define(CT_PAL(Format, Items), pms_test_lib:ct_pal(Format, Items)).
-define(CT_LOG(Format, Items), pms_test_lib:ct_log(Format, Items)).
%% -define(TEST_INFO(Format, Items), pms_test_lib:ct_log(Format, Items)).
%% -define(TEST_DEBUG(Format, Items), pms_test_lib:ct_log(Format, Items)).


-define(CHILD_PREFIX, "ift_client_").

-define(TIMEOUT, 10000).

-define(PMI_VSN_1, pmi).
-define(PMI_VSN_2, pmi2).

-define(PMS_PMI_PROXY, pms_pmi_proxy).
-define(ERL_PMI, pmsPmI).
-define(ERL_PMI2, pmsPmI2).

-define(PMI_INIT, pmiInitialize).
-define(PMI_FIN, pmiFinalize).
-define(PMI_SUBSCRIBE_CB, pmiSubscribeCallback).
-define(PMI_REPORT_CB, pmiReportCallback).
-define(PMI_REPORT_SC_CB, pmiReportShowCountersCallback).

-define(PMI2_INIT, pmi2Initialize).
-define(PMI2_FIN, pmi2Finalize).
-define(PMI2_SUBSCRIBE_ROP_CB, pmi2SubscribeRopCallback).
-define(PMI2_REPORT_ROP_CB, pmi2ReportRopCallback).
-define(PMI2_REPORT_SC_CB, pmi2ReportShowCountersCallback).

%% must match definition in master.h of ift_app
-define(PMI, 7).
-define(PMI2, 18).

%% PMI fcns. Must match IFT_APP codes
-define(PMI_INITIALIZE, 1).
-define(PMI_FINALIZE, 2).
-define(PMI_DATA, 3).
-define(PMI_SETUP_CALLBACK, 4).
-define(PMI_EMUL_DEATH, 5).
-define(PMI_INITIALIZE_2, 6).
-define(PMI_DATA_SHOW_COUNTERS, 7).

%% PMI2 fcns. Must match IFT_APP codes
-define(IFT_PMI2_INITIALIZE, 10).
-define(IFT_PMI2_COUNTER_MAP, 11).
-define(IFT_PMI2_SETUP_CALLBACK, 12).
-define(IFT_PMI2_DATA_ROP, 13).
-define(IFT_PMI2_DATA_SHOW_COUNTERS, 14).
-define(IFT_PMI2_FINALIZE, 15).
-define(IFT_PMI2_EMUL_DEATH, 16).

%% must match codes in the C interface
%% -define(SUBSCRIBE, 1).
%% -define(REPORT, 2).

-define(PMI2_INITIALIZE, 10000).

-define(PMI2_OK, 1).
-define(PMI2_UNKNOWN_HANDLE, 2).
-define(PMI2_UNKNOWN_COUNTER_MAP_ID, 3).
-define(PMI2_BAD_PARAMETER, 4).
-define(PMI2_INTERNAL_ERROR, 5).
-define(PMI2_INTERNAL_HASHMAP_FAILURE, 11).
-define(PMI2_INTERNAL_OUT_OF_MEMORY, 12).
-define(PMI2_INTERNAL_ALLOCATION_FAILURE, 13).
-define(PMI2_INTERNAL_VERSION_MISMATCH, 14).
-define(PMI2_INTERNAL_BAD_ARITY, 15).
-define(PMI2_INTERNAL_UNKNOWN_FUNCTION, 16).
-define(PMI2_INTERNAL_DECODE_FAILURE, 17).
-define(PMI2_INTERNAL_ENCODE_FAILURE, 18).
-define(PMI2_INTERNAL_CEC_OPEN_FAILURE, 19).
-define(PMI2_INTERNAL_CEC_SEND_FAILURE, 20).
-define(PMI2_INTERNAL_CEC_RECEIVE_FAILURE, 21).
-define(PMI2_INTERNAL_CEC_CLOSE_FAILURE, 22).
-define(PMI2_INTERNAL_MAX_HANDLES_EXCEEDED, 23).
-define(PMI2_INTERNAL_HASHMAP_VALUE_OVERWRITE, 24).
-define(PMI2_INTERNAL_HASHMAP_OUT_OF_MEMORY, 25).
-define(PMI2_INTERNAL_HASHMAP_INTERFACE_ERROR, 26).
-define(PMI2_INTERNAL_HASHMAP_OVERWRITE, 27).
-define(PMI2_INTERNAL_LIMITS_INCONSISTENCY, 28).


-record(state, {session,
		parent,
		pmi_vsn = ?PMI2,
		app_pid,
		handle,
		callbacks,
		cs_node,
		remote_node,
		erl_api = false,
		trace_child = false
	       }).

%%%===================================================================
%%% PMI functions
%%%===================================================================
%%%===================================================================
%%% Support functions
%%%===================================================================

%% Make the C node behave as a dead application would.
%% Only the specifed application is affected. If CloseSocket
%% is true the socket is closed from the application side,
%% otherwise it is left unattended.

%% emulAppDeath(AppPid, CloseSocket) ->
%%     ?SERVER ! {signal, {emulAppDeath, AppPid, CloseSocket}},	
%%     receive
%% 	{response, emulAppDeath} ->
%% 	    ok
%%     end.


%%--------------------------------------------------------------------
%% @private
%% Starts the server
%%
%%--------------------------------------------------------------------
start(Session, RNode, CSNode, Opts) ->
    ?CT_PAL("start pms_pmi_proxy_session (~p).~nRNode = ~p, CSNode = ~p~n"
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
    ?CT_PAL("pms_pmi_proxy_session (~p) started: ~p", [Session, Res]),
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
handle_call({pmi_call, {Func, Args}}, _From, State)
  when Func =:= pmiFinalize; Func =:= pmi2Finalize ->
    ?CT_PAL("[~p] pmiFinalize received", [?MODULE]),
    Reply = call_pmi(Func, Args, State),
    {stop, normal, Reply, State};   

handle_call({pmi_call, {Func, Args}}, _From, State) ->
    ?CT_PAL("[~p] ~p received", [?MODULE, Func]),
    Reply = call_pmi(Func, Args, State),
    {reply, Reply, State};

handle_call({pmi2EmulAppDeath, _CloseSocket}, _From, State) ->
    {reply, {error, not_implemented_yet}, State};

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
handle_cast({{Func, [CbData, CounterMap, CbMod, AppPid]}, From}, 
	    State)
  when Func =:= pmi2Initialize ->
    Callbacks = to_erl_callbacks(CbData),
    ?CT_PAL("pmi2Initialize ~p ~p", [Callbacks, CbData]),
    case handle_pmi_initialize(State, Func, [Callbacks, CounterMap]) of
	{ok, Handle} ->
	    ?CT_PAL("pmi2Initialize Handle ~p, self ~p", [Handle, self()]),
	    Cbs = to_callbacks(Callbacks, CbMod),
	    gen_server:reply(From, {ok, self()}),
	    erlang:monitor(process, AppPid),
	    {noreply, State#state{handle = Handle, callbacks = Cbs,
				  app_pid = AppPid}};
	Error ->
	    gen_server:reply(From, Error),
	    {stop, to_stop_reason(Error), State}
    end;

handle_cast({{Func, [RegData, CbMod, AppPid]}, From}, 
	    State)
  when Func =:= pmiInitialize ->
    NewState = State#state{pmi_vsn = ?PMI},
    case handle_pmi_initialize(NewState, Func, [RegData]) of
	{ok, Handle} ->
	    gen_server:reply(From, {ok, self()}),
	    erlang:monitor(process, AppPid),
	    {noreply, NewState#state{handle = Handle, callbacks = CbMod,
				     app_pid = AppPid}};
	Error ->
	    gen_server:reply(From, Error),
	    {stop, Error, NewState}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info({nodedown, _Node}, State) ->
    ?CT_PAL("[~p] Node ~p is down!", [?MODULE, _Node]),
    {stop, normal, State};   

handle_info({signal, {Callback, Arg}}, State) ->
    exec_callback(State, Callback, Arg),
    {noreply, State};

handle_info({Callback, Arg}, State) when is_atom(Callback), is_tuple(Arg) -> 
    exec_callback(State, Callback, to_list(Arg)),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    case State#state.pmi_vsn of
	?PMI2 ->
	    call_pmi(pmi2Finalize, [], State);
	_PMI ->
	    call_pmi(pmiFinalize, [], State)
    end,
    {stop, normal, State};   

handle_info(_Info, State) ->
    ?CT_PAL("~p(~p): Received unexpected Info: ~p", 
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
    ?CT_PAL("pms_pmi_proxy_session (~p) terminating: ~p", 
	   [State#state.session, Reason]),
    case Reason of
	normal ->
	    ok;
	_Other when State#state.pmi_vsn =:= ?PMI2 ->
	    call_pmi(pmi2Finalize, [], State);
	_Other ->
	    call_pmi(pmiFinalize, [], State)
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
handle_pmi_initialize(State, pmiInitialize = Func, [RegData]) 
  when State#state.erl_api =:= true ->
    pmi_rpc_call(Func, [RegData, ?ERL_PMI, self()], State);

handle_pmi_initialize(State, pmi2Initialize = Func, [Callbacks, CounterMap]) 
  when State#state.erl_api =:= true ->
    pmi_rpc_call(Func, [Callbacks, CounterMap, ?ERL_PMI2, self()], State);

handle_pmi_initialize(State, Func, InitArg) ->
    case start_pms_client(State) of
	ok ->
	    ?CT_PAL("~w: pmi session started ~n", [?MODULE]),
	    start_session_trace(State),
	    CecHost = "",
	    CecPort = "0",
	    InitMsg = initialize_msg(Func, CecHost, CecPort, InitArg, State),
	    send_initialize(State, Func, InitMsg);
	Error ->
	    Error
    end.


call_pmi(_Func, _Arg, State) when State#state.handle =:= undefined ->
    ?CT_PAL("#### PROXY SESSION call_pmi Handle ~p~n", [undefined]),
    ok;

call_pmi(Func, Arg, State) 
  when State#state.erl_api ->
    ?CT_PAL("#### PROXY SESSION call_pmi State ~p~n", [erl_api]),
    pmi_rpc_call(Func, [State#state.handle | Arg], State);

call_pmi(pmiData = Func, [G, T, L, B], State) ->
    ?CT_PAL("#### PROXY SESSION call_pmi Bundle ~p~n", [B]),
    Arg = [G, T, L, pms_c_util:tuplifyBundles(B)],
    call_pmi_proxy(Func, Arg, State);
    
call_pmi(pmiDataShowCounters = Func, [RId, Res, E, B], State) ->
    Arg = [RId, Res, E, pms_c_util:tuplifyMTs(B)],
    call_pmi_proxy(Func, Arg, State);

call_pmi(Func, Arg, State) ->
    ?CT_PAL("#### PROXY SESSION call_pmi Func ~p~n", [Func]),
    call_pmi_proxy(Func, Arg, State).
    

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
    ?CT_LOG("~p exec callback: {~p, ~p, ~p}", 
	   [?MODULE, Module, CbFunc, NewArg]), 
    spawn(fun() ->
		  apply(Module, CbFunc, NewArg)
	  end),
    %% there is a small chance that two cb received within milliseconds
    %% can be executed in the opposite order. To prevent this sleep
    %% a short while after each cb call.
    timer:sleep(50);

exec_callback(_State, _Module, _CbFunc, _Arg) ->
    ok.


get_cb_func(subscribe) ->
    ?PMI_SUBSCRIBE_CB;
get_cb_func(report) ->
    ?PMI_REPORT_CB;
get_cb_func(reportShowCounters) ->
    ?PMI_REPORT_SC_CB;
get_cb_func(showCounters) ->
    ?PMI_REPORT_SC_CB;
get_cb_func(pmiSubscribe) ->
    ?PMI_SUBSCRIBE_CB;
get_cb_func(pmiReport) ->
    ?PMI_REPORT_CB;
get_cb_func(pmiReportShowCounters) ->
    ?PMI_REPORT_SC_CB;

get_cb_func(pmi2SubscribeRop) ->
    ?PMI2_SUBSCRIBE_ROP_CB;
get_cb_func(pmi2ReportRop) ->
    ?PMI2_REPORT_ROP_CB;
get_cb_func(pmi2ReportShowCounters) ->
    ?PMI2_REPORT_SC_CB.


get_cb_mod(Mod, _Func) when is_atom(Mod) ->
    Mod;
get_cb_mod(Callbacks, Func) when is_list(Callbacks) ->
    proplists:get_value(Func, Callbacks, ?PMS_PMI_PROXY).

%%%===================================================================
%%% RPC call
%%%===================================================================
pmi_rpc_call(Function, Args, State) ->
    %% TBD: Get module from function.
    Module = func_to_pmi_mod(Function),
    ?CT_PAL("#### PROXY SESSION pmi_rpc_call Module ~p~n", [Module]),
    pmi_rpc_call(Module, Function, Args, State).


pmi_rpc_call(Module, Function, Args, State) ->
    ?CT_PAL("~p pmi_rpc_call   ~p~n", 
	   [?MODULE, {Module, Function, Args, State#state.cs_node}]),
    rpc:call(State#state.cs_node, Module, Function, Args, 20000).

%%%===================================================================
%%% rct_proxy functions
%%%===================================================================
start_pms_client(State) when State#state.erl_api =:= false ->
    Session = State#state.session,
    IFTNode = State#state.remote_node,
    Vsn = State#state.pmi_vsn,
    {start, IFTNode} ! {self(), Session, Vsn},
    case wait_ift_reply(?TIMEOUT) of
	{ok, client_started} ->
	    ok;
	_Other ->
	    ?CT_PAL("Got unexpected reply from IFT: ~p", [_Other]),
	    {error, failed_to_start_client}
    end;

start_pms_client(_State) ->
    ok.


stop_pms_client(IFTNode, Session) -> 
    {stop, IFTNode} ! {self(), Session},
    case wait_ift_reply(?TIMEOUT) of
	{ok, client_stopped} ->
	    ok;
	_Other ->
	    {error, failed_to_stop_client}
    end.


initialize_msg(pmi2Initialize, _Host, _Port, [Callbacks, CounterMap], State) ->
    UserData = "pmi_proxy_" ++ atl(State#state.session),
    {CounterMap, to_ift_callbacks(Callbacks), UserData};

initialize_msg(_Func, CecHost, CecPort, [[{TopMoLDN, RegData}]], _State) ->
    {CecHost, CecPort, TopMoLDN, RegData};

initialize_msg(_Func, CecHost, CecPort, [RegData], _State) ->
    {CecHost, CecPort, RegData}.
    

send_initialize(State, Func, InitMsg) ->
    FuncId = func_id(Func, InitMsg),
    case call_pmi_proxy(FuncId, InitMsg, State) of
	{ok, Handle} -> 
	    ?CT_PAL("~w: pmi session: ~p, Handle = ~p~n", 
		       [?MODULE, Func, Handle]),
	    send_setup_cb(State, Handle);
	Error ->
	    ?CT_PAL("~w: pmi session: ~p failed ~p~n", 
		       [?MODULE, Func, Error]),
	    Error
    end.
    

send_setup_cb(State, Handle) ->
    FuncId = setup_cb_id(State),
    case call_pmi_proxy(FuncId, {Handle}, State) of
	{ok, _Socket} ->
	    ?CT_PAL("~w: pmi session: setup cb socket ~p~n", 
		       [?MODULE, _Socket]),
	    {ok, Handle};
	Error ->
	    ?CT_PAL("~w: pmi session: setup cb failed ~p~n", 
		       [?MODULE, Error]),
	    Error
    end.


setup_cb_id(State) when State#state.pmi_vsn =:= ?PMI ->
    ?PMI_SETUP_CALLBACK;

setup_cb_id(_State) ->
    ?IFT_PMI2_SETUP_CALLBACK.


wait_ift_reply(Timeout) ->
    receive
	{ok, _} = Msg ->
	    Msg;
	{ok} ->
	    ok;
	{error, _} = E ->
	    E
    after Timeout ->
	    {error, timeout}
    end.


call_pmi_proxy(FuncId, Arg, State) when is_integer(FuncId), is_tuple(Arg) ->
    Session = State#state.session,
    IFTNode = State#state.remote_node,
    {send, IFTNode} ! {self(), Session, FuncId, Arg},
    ?CT_PAL("Send to proxy:~n~p ! ~p", 
	   [{send, IFTNode}, {self(), Session, FuncId, Arg}]),
    Res = wait_ift_reply(?TIMEOUT),
    if FuncId =:= ?PMI_FINALIZE;
       FuncId =:= ?IFT_PMI2_FINALIZE ->
	    stop_pms_client(IFTNode, Session);
       true ->
	    Res
    end;

call_pmi_proxy(Func, Arg, State) ->
    FuncId = func_id(Func),
    NewArg = to_tuple([State#state.handle | Arg]),
    call_pmi_proxy(FuncId, NewArg, State).


start_session_trace(State) 
  when State#state.trace_child =:= true, 
       State#state.erl_api =:= false ->
    timer:sleep(3000),
    Session = State#state.session,    
    ?PMS_PMI_PROXY:start_trace(?CHILD_PREFIX ++ atl(Session), 
			      State#state.cs_node);

start_session_trace(_) ->
    ok.
    

func_id(?PMI_INIT, {_, _, _, _}) ->
    ?PMI_INITIALIZE_2;
func_id(Func, _) ->
    func_id(Func).


func_id(?PMI_INIT) ->
    ?PMI_INITIALIZE;
func_id(?PMI_FIN) ->
    ?PMI_FINALIZE;
func_id(pmiData) ->
    ?PMI_DATA;
func_id(pmiDataShowCounters) ->
    ?PMI_DATA_SHOW_COUNTERS;
func_id(?PMI2_INIT) ->
    ?IFT_PMI2_INITIALIZE;
func_id(pmi2CounterMap) ->
    ?IFT_PMI2_COUNTER_MAP;
func_id(?PMI2_FIN) ->
    ?IFT_PMI2_FINALIZE;
func_id(pmi2DataRop) ->
    ?IFT_PMI2_DATA_ROP;
func_id(pmi2DataShowCounters) ->
    ?IFT_PMI2_DATA_SHOW_COUNTERS.


%%%===================================================================
%%% lib functions
%%%===================================================================
%% lta(L) when is_list(L) ->
%%     list_to_atom(L);
%% lta(A) when is_atom(A) ->
%%     A.


to_stop_reason({error, {_, ?PMI2_INITIALIZE + ?PMI2_UNKNOWN_COUNTER_MAP_ID}}) ->
    normal;
to_stop_reason(Error) ->
    Error.    

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


to_ift_callbacks(Callbacks) when is_list(Callbacks) ->
    SubscrCb = proplists:get_value(?PMI2_SUBSCRIBE_ROP_CB, Callbacks, false),
    RepRopCb = proplists:get_value(?PMI2_REPORT_ROP_CB, Callbacks, false),
    RepSCCb = 
	proplists:get_value(?PMI2_REPORT_SC_CB, Callbacks, false),
    {SubscrCb, RepRopCb, RepSCCb};

to_ift_callbacks(Callbacks) when is_tuple(Callbacks) ->
    Callbacks.


to_erl_callbacks(Callbacks) when is_list(Callbacks) ->
    to_erl_callbacks(to_ift_callbacks(Callbacks));

to_erl_callbacks({SubF, RepRF, RepSCF}) ->
    [{?PMI2_SUBSCRIBE_ROP_CB, SubF},
     {?PMI2_REPORT_ROP_CB, RepRF},
     {?PMI2_REPORT_SC_CB, RepSCF}].


to_callbacks(Callbacks, CbMod) when is_list(Callbacks) ->
    to_callbacks(to_ift_callbacks(Callbacks), CbMod);

to_callbacks({SubF, RepRF, RepSCF}, CbMod) ->
    [{?PMI2_SUBSCRIBE_ROP_CB, to_cb_mod(SubF, CbMod)},
     {?PMI2_REPORT_ROP_CB, to_cb_mod(RepRF, CbMod)},
     {?PMI2_REPORT_SC_CB, to_cb_mod(RepSCF, CbMod)}].


to_cb_mod(true, CbMod) ->
    CbMod;
to_cb_mod(_False, _cbMod) ->
    false.


func_to_pmi_mod(Func) when Func =:= pmi2Initialize;
			   Func =:= pmi2CounterMap;
			   Func =:= pmi2Finalize;
			   Func =:= pmi2DataShowCounters;
			   Func =:= pmi2DataRop ->
    ?ERL_PMI2;

func_to_pmi_mod(_Func) ->
    ?ERL_PMI.

    


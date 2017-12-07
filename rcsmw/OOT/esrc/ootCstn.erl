%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootCstn.erl %
%%% @author eolaand
%%%
%%% @doc ==Server that terminates the CSTN protocol ==
%%% Performs mapping of Router-DN to network namespace
%%%
%%% =========
%%% State
%%% =========
%%%
%%% At server start, State=undefined
%%% State=connected is intermediate - not seen in the wild
%%%
%%% State      Event                Next_state
%%% -----      -----                ----------
%%% undefined =cec_setup     =>      pending
%%% pending   =connect_cec   =>      pending
%%% pending   =connect_cec   =>      connected => resolv1
%%% resolv1   =NS1 data      =>      resolv2
%%% resolv2   =NS2 data      =>      done
%%% get_ns    =NS  data      =>      done
%%% any       =warm          =>      warm
%%% warm      =cec_setup     =>      pending
%%% restart   =NS? data      =>      connected => resolv1
%%% resolv?   =new_dn        =>      restart
%%% undefined =new_dn        =>      undefined
%%% done      =new_dn        =>      connected => resolv1
%%% done      =get_ns_and_ip =>      connected => get_ns
%%% Other     =new_dn        =>      Other
%%% 
%%% 
%%% 
%%% =========
%%% Sequences
%%% =========
%%% 
%%% Initialize
%%% ----------
%%%
%%% ootCstn asks for the OAM AP name space and IP address
%%% for both for the ordinary and, if it exists, the alternative. 
%%% 
%%% ootServer              ootCstn                       TN
%%% 
%%%                           &lt;------- initialize() -------
%%% 
%%%                           ----- resolve(DN_ord) ------&gt;
%%%                           &lt;---- reply(DN, NS, Addr) ---
%%% 
%%% 
%%%                           ----- resolve(DN_alt) -------&gt;
%%%                           &lt;---- reply(DN, NS, Addr) ----
%%% 
%%%   &lt;--- cstn_complete -----
%%% 
%%% 
%%% Updated OAM AP DN
%%% -----------------
%%%
%%% The OAM access point DN is chaged
%%%
%%% ootServer              ootCstn                       TN
%%% 
%%%   ----- new_dn() --------&gt;
%%% 
%%%                           ----- resolve(DN_ord) ------&gt;
%%%                           &lt;---- reply(DN, NS, Addr) ---
%%% 
%%% 
%%%                           ----- resolve(DN_alt) -------&gt;
%%%                           &lt;---- reply(DN, NS, Addr) ----
%%% 
%%%   &lt;--- cstn_complete -----
%%% 
%%% 
%%% Update
%%% ------
%%%
%%% Namespace or IP address is updated
%%%
%%% ootServer              ootCstn                       TN
%%% 
%%%                           &lt;-- update(DN, NS, Addr) ---
%%% 
%%% 
%%%   &lt;--- cstn_update -----
%%% 
%%% 
%%% Warm restart
%%% ------------
%%%
%%% ootServer              ootCstn                       TN
%%% 
%%%   ------- warm() --------&gt;
%%% 
%%%                     wait for initializze from TN
%%% 
%%% 
%%% 
%%% get ns and IP address
%%% ---------------------
%%%
%%% An erlang application asks for the name space and IP address
%%% for a given DN.
%%% 
%%% If cec_state is undefined, {error, oot_cstn_not_started} is returned
%%% If cec_state is not done,  {error, oot_cstn_try_again} is returned
%%% 
%%% 
%%% ootServer                  ootCstn                       TN
%%% 
%%%   -- get_ns_and_ip(DN) -------&gt;
%%% 
%%%                                ----- resolve(DN_ord) ------&gt;
%%%                                &lt;---- reply(DN, NS, Addr) ---
%%% 
%%%   &lt;---- reply(DN, NS, Addr) ---
%%% 
%%% 
%%% 
%%% subscribe to updates
%%% --------------------
%%%
%%% An application subscribes for name space and IP address
%%% updates for a specified DN
%%%
%%% ootServer                          ootCstn                       TN
%%% 
%%%   -- register_ip_change_cb(Mod, DN) --&gt;
%%% 
%%%                                        &lt;---- update(DN, NS, Addr) ---
%%% 
%%%   &lt;--- Mod:ip_changed(DN, NS, Addr) ---
%%% 
%%% 
%%% 
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% -----      -------    --------    ------------------------
%%% R          2015-10-13 etxlg     Created
%%% R4A/2      2015-10-16 etxlg     Wait for ootServer after upgrade
%%% R4A/3      2015-10-19 etxlg     Accept that OamAP is undefined
%%% R4A/4      2015-10-20 etxlg     Removed deadlock...
%%% R4A/5      2015-10-29 etxlg     Fixed warm restart
%%% R4A/6      2015-11-02 etxlg     Fix for TN slow starting
%%% R5A/1      2015-12-04 eolaand   Changed name of a ootServer fcn
%%% R4A/8      2015-12-15 erarafo   Tracing cec_state changes
%%% R4A/9      2015-12-15 erarafo   Corrected revision history
%%% R5A/2      2015-12-15 erarafo   Merge from R4A/9
%%% R5A/3      2016-01-26 etxlg     Handle unsetting the OamAP
%%% R6A/1      2016-05-10 etxpeno   correction of error case in cec_setup/1
%%% R9A/1      2017-01-11 uabesvi   cleanup of the code
%%% R9A/2-3    2017-01-22 uabesvi   new cstn interface added
%%% R9A/4-6    2017-01-25 uabesvi   new functions 
%%%                                 get_ns_and_ip register_ip_change_cb
%%% R9A/7      2017-01-25 uabesvi   added OotLog
%%% R10A/1     2017-05-15 ecaiyan   HV86530
%%% R10A/2     2017-05-09 ecaiyan   added support for initilize3 & 
%%%                                 unregister_ip_change_cb
%%% R10A/9     2017-07-11 egabing   Removed cstn version 1
%%% R11A/1     2017-07-27 egabing   Added cstn version atom macros 
%%%                                 (instead of integers)
%%% R11A/2     2017-07-28 eolaand   Don't try to send undefined to TN
%%% R11A/3     2017-07-29 eolaand   Don't send DN to TN if unchanged 
%%% R11A/5     2017-08-01 eolaand   Notify ootServer even if alt OAP unchanged
%%% R11A/12    2017-09-13 eolaand   Fix bug with conflicting unsubscribe
%%% R11A/13    2017-09-19 eolaand   Remove ERROR log when discarding timeout
%%% ----------------------------------------------------------

-module(ootCstn).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/R6A/R8A/R9A/R10A/R11A/14').
-date('2017-09-19').
-author('eolaand').

%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%% callback from CEC
-export([cec_setup/1]).

%% ootServer calls
-export([new_dn/0]).	%% when any DN changes
-export([warm/0]).	%% warm starts

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([print_state/0]).	%% debug

%% get name space and ip addres for an MO ref
-export([get_ns_and_ip/1]).
-export([unregister_ip_change_cb/3]).
-export([register_ip_change_cb/3]).

-include("oot.hrl").

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SERVER, ?MODULE).
-define(SERVER_CALL_TIMEOUT, 30000).
-define(CEC_CALL_TIMEOUT, 60000).
-define(CEC_CONNECT_PRINT, 300).
-define(NS_FILE, "/home/sirpa/oam_net_namespace").

-define(NAME_SPACE_1, 2).
-define(INITIALIZE_2, 3).
-define(NAME_SPACE_2, 4).
-define(UPDATE,       5).
-define(INITIALIZE_3, 6).
-define(OamDNtoNsNameAndIp, 8).
-define(Unsubscribe, 9).

-define(CSTN_VSN_2, cstn_vsn_2).
-define(CSTN_VSN_3, cstn_vsn_3).

%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------

%% version:   ?CSTN_VSN_2 or ?CSTN_VSN_3 depending of version initialize message
%% cec_state: undefined | warm | pending | connected |
%%            resolv1 | resolv2 | get_ns | restart | done
%% update_cb: [{LDN, [CbModule]}]
%% msg_queue: incomming messages while not in state done
-record(state, {version,
		socket,
		cec_state,
		ldn1,
		ns1 = <<>>,
		addr1,
		ldn2,
		ns2 = <<>>,
		addr2,
		update_cb = [],
		get_ns_info,
		new_dn_pending
	       }).

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type error()::{error, Reason::string()}.


%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%% @doc
%%% Starts the Cstn server.
%%% @end
-spec start_link() -> {ok, pid()} | error().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% @private
cec_setup(Sock) ->
    ?LOG_INFO("cec_setup, socket: ~p", [Sock]),
    case whereis(?SERVER) of
	undefined ->
	    ?LOG_WARNING("Got cec_setup/1 but server not running."),
	    throw(ootCstn_not_running);
	_ ->
	    Pid = gen_server:call(?SERVER, 
				  {cec_setup, Sock}, 
				  ?CEC_CALL_TIMEOUT),
	    ?LOG_INFO("cec_setup, result: ~p", [Pid]),
	    Pid
    end.

new_dn() ->
    gen_server:cast(?SERVER, new_dn).

warm() ->
    gen_server:cast(?SERVER, warm).

%% get name space and ip addres for an MO ref
get_ns_and_ip(MoRef) when is_list(MoRef) ->
    get_ns_and_ip(l2b(MoRef));

get_ns_and_ip(MoRef) when is_binary(MoRef) ->
    try
	gen_server:call(?SERVER,
			{get_ns_and_ip, MoRef}, 
			?SERVER_CALL_TIMEOUT * 2)
    catch
	_:{timeout, _} ->
	    {error, reply_timeout};
	_:_ ->
	    {error, oot_cstn_not_started}
    end.


register_ip_change_cb(MoRef, CbModule, ExtraArgs) ->
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    gen_server:cast(?SERVER,
			    {register_ip_change_cb, {l2b(MoRef), 
						     CbModule, 
						     ExtraArgs}});
	_ ->
	    {error, oot_cstn_not_started}
    end.
	    

unregister_ip_change_cb(MoRef, CbModule, ExtraArgs) ->
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    gen_server:cast(?SERVER,
			    {unregister_ip_change_cb, {l2b(MoRef),
						       CbModule, 
						       ExtraArgs}});
	_ ->
	    {error, oot_cstn_not_started}
    end.


print_state() ->
    gen_server:cast(?SERVER, print_state).
    

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
init([]) ->
    trace_cec_state(init, no_state, undefined),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%%=================================================
%% CEC calls back, tn-oam-agent is calling
%%=================================================
handle_call({cec_setup, Sock}, _From, State) ->
    case State#state.socket of
	OldSock when is_port(OldSock) ->
	    ?LOG_INFO("Reopening CEC connection."),
	    catch gen_tcp:close(OldSock);
	_ ->
	    ok
    end,
    NewCecState = pending,
    trace_cec_state(cec_setup, State#state.cec_state, NewCecState),
    {reply, self(), State#state{cec_state = NewCecState, socket = Sock}};

%%=================================================
%% get_oap_namespace
%%
%% called from ootI
%%=================================================
handle_call(get_oap_namespace, _From, State) -> 
    Reply = reply_ns(State, State#state.ns1),
    ?LOG_INFO("Return from get_oap_namespace: ~p", [Reply]),
    {reply, Reply, State};

%%=================================================
%% get_oap_alt_namespace
%% 
%% called from ootI
%%=================================================
handle_call(get_oap_alt_namespace, _From, State) ->
    Reply = reply_ns(State, State#state.ns2),
    ?LOG_INFO("Return from get_oap_alt_namespace: ~p", [Reply]),
    {reply, Reply, State};

%%=================================================
%% get_ns_and_ip
%% 
%% ask TN for ns and ip if nothing is ongoing
%% if server is busy, put the request in the queue
%%=================================================
%% state done
%% execute the request
handle_call({get_ns_and_ip, MoRef}, From, State)
  when State#state.cec_state =:= done ->
    Msg = {get_ns_and_ip_to, MoRef, From},
    erlang:send_after(?SERVER_CALL_TIMEOUT, self(), Msg),
    NewState = do_get_ns_and_ip(MoRef, From, State),
    {noreply, NewState};

handle_call({get_ns_and_ip, _MoRef}, _From, State)
  when State#state.cec_state =:= undefined ->
    {reply, {error, oot_cstn_not_started}, State};

handle_call({get_ns_and_ip, _MoRef}, _From, State) -> 
    {reply, {error, oot_cstn_try_again}, State};

%%=================================================
%% stop
%%=================================================
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

%%=================================================
%% unknown msg
%%=================================================
handle_call(Call, _From, State) ->
    ?LOG_ERROR("unknown call: ~p from ~p", [Call, _From]),
    Reply = {error, {unknown_call, Call}},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%%=================================================
%% new_dn
%%=================================================
%% "normal case"
handle_cast(new_dn, #state{cec_state = done} = State) ->
    ?LOG_INFO("Got 'new_dn' in state ~p", [State#state.cec_state]),
    NewCecState = connected,
    trace_cec_state(new_dn, State#state.cec_state, NewCecState),
    {noreply, ask_for_name_space(State#state{cec_state = NewCecState})};

%% need to cancel/block as resolution is in progress
handle_cast(new_dn, #state{cec_state = Resolv} = State)
  when Resolv =:= resolv1;
       Resolv =:= resolv2 ->
    ?LOG_INFO("Got 'new_dn' in state ~p", [Resolv]),
    NewCecState = restart,
    trace_cec_state(new_dn, State#state.cec_state, NewCecState),
    {noreply, State#state{cec_state = NewCecState}};

%% other events will get us out of here
handle_cast(new_dn, #state{cec_state = get_ns} = State) ->
    ?LOG_INFO("Got 'new_dn' in cec_state: get_ns, got new_dn from ootServer,"
	      " pending for later ", []),
    {noreply, State#state{new_dn_pending = true}};

%% other events will get us out of here
handle_cast(new_dn, #state{cec_state = Other} = State) ->
    ?LOG_INFO("Got 'new_dn' in cec_state: ~p", [Other]),
    {noreply, State};

%%=================================================
%% warm
%%
%% need to cancel/block if resolution is in progress
%% after this we wait for new cec_setup caused by TN restarting
%%=================================================
handle_cast(warm, State) ->
    ?LOG_INFO("Got 'warm' in state ~p", [State#state.cec_state]),
    NewCecState = warm,
    trace_cec_state(warm, State#state.cec_state, NewCecState),
    {noreply, State#state{cec_state   = NewCecState,
			  ldn1        = undefined,
			  ldn2        = undefined,
			  addr1       = undefined,
			  addr2       = undefined,
			  ns1         = <<>>,
			  ns2         = <<>>,
			  get_ns_info = undefined}};

%%=================================================
%% register_ip_change_cb
%%=================================================
handle_cast({register_ip_change_cb, {MoRef, CbModule, ExtraArgs}}, State) ->
    {noreply, 
     do_register_ip_change_cb(MoRef, CbModule, ExtraArgs, State)};

%%=================================================
%% unregister_ip_change_cb
%%=================================================
handle_cast({unregister_ip_change_cb, {MoRef, CbModule, ExtraArgs}}, State) ->
    NewState = do_unregister_ip_change_cb(MoRef, CbModule, ExtraArgs, State),
    {noreply, NewState};

%%=================================================
%% execute_ip_change_cb, only for testing
%%=================================================
handle_cast({execute_ip_change_cb, {Ldn, NS, IpAddr}}, 
	    #state{cec_state = done} = State) ->
    notify_callback(Ldn, NS, IpAddr, State),
    {noreply, State};

%%=================================================
%% print_state
%%=================================================
handle_cast(print_state, State) ->
    do_print_state(State),
    {noreply, State};

%%=================================================
%% Unknown message
%%=================================================
handle_cast(Msg, State) ->
    ?LOG_INFO("unknown cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%%=================================================
%% cec connect
%%=================================================
handle_info({connect_cec, N}, #state{cec_state = pending} = State) ->
    NewState = handle_cec_connect(N, State),
    {noreply, NewState};
%% cec connect in wrong state
handle_info(connect_cec, State) ->
    ?LOG_WARNING("connect_cec in wrong state: ~p", [State#state.cec_state]),
    {noreply, State};

%%=================================================
%% tcp message
%% 
%% a single NULL means NS not in use, 
%% results in {oap_namespace, <<>>}
%%=================================================
handle_info({tcp, 
	     Sock, 
	     <<Msg:1/native-unsigned-integer-unit:32, Data/binary>>} = Event, 
	    #state{socket = Sock} = State) ->
    NewState = handle_app_msg(Msg, Data, State),
    inet:setopts(Sock,[{active, once}]),
    trace_cec_state(Event, State#state.cec_state, NewState#state.cec_state),
    log_state(NewState),
    {noreply, NewState};

%%=================================================
%% tcp_closed
%% 
%% don't do anything much, expect a new cec_setup
%%=================================================
handle_info({tcp_closed, Sock}, #state{socket = Sock} = State) ->
    ?LOG_WARNING("CEC connection closed, expecting reconnect"),
    catch gen_tcp:close(Sock),
    trace_cec_state(tcp_closed, State#state.cec_state, undefined),
    {noreply, State#state{socket      = undefined, 
			  cec_state   = undefined,
			  version     = undefined,
			  ldn1        = undefined,
			  ldn2        = undefined,
			  addr1       = undefined,
			  addr2       = undefined,
			  ns1         = <<>>,
			  ns2         = <<>>,
			  get_ns_info = undefined}};

%%=================================================
%% get_ns_and_ip_to
%%=================================================
handle_info({get_ns_and_ip_to, MoRef, From}, 
	    #state{get_ns_info = {MoRef, From}} = State) ->
    ?LOG_ERROR("get_ns_and_ip_to send reply ~p", [{MoRef, From}]),
    gen_server:reply(From, {error, timeout}),
    NewState = handle_ns_timeout(State),
    {noreply, NewState#state{cec_state = done, get_ns_info = undefined}};

handle_info({get_ns_and_ip_to, _MoRef, _From}, State) ->
    %% Already replied, ignore this message
    {noreply, State};

%%=================================================
%% Unkown msg
%%=================================================
handle_info(Info, State) ->
    ?LOG_INFO("unknown info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    trace_cec_state(terminate, State#state.cec_state, no_state),
    %% see if closing the socket enables recovery after crash - it doesn't
    catch gen_tcp:shutdown(State#state.socket, read),
    catch gen_tcp:send(State#state.socket, ["non_existing", 0]), %% append null
    catch gen_tcp:close(State#state.socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%%===================================================================
%%% handle CEC connect
%%%===================================================================
handle_cec_connect(N, State) ->
    case get_dn(resolv1) of
	%% OK
	DnWorks when is_binary(DnWorks) ->
	    trace_cec_state(connect_cec, State#state.cec_state, connected),
	    ask_for_name_space(State#state{cec_state = connected});
	%% server running but no OamAP set
	undefined ->
	    trace_cec_state(connect_cec, State#state.cec_state, connected),
	    ask_for_name_space(State#state{cec_state = connected});
	_NotReady when N < ?CEC_CONNECT_PRINT ->
	    erlang:send_after(1000, self(), {connect_cec, N + 1}),
	    State;
	NotReady ->
	    ?LOG_INFO("ootServer returned: ~p, "
		      "maybe upgrading - retrying every 1000 ms",
		      [NotReady]),
	    erlang:send_after(1000, self(), {connect_cec, 0}),
	    State
    end.


%%========================================================================
%% handle_app_msg(Msg, Data, State) -> State
%% 
%% Check the message and take actions.
%%========================================================================
handle_app_msg(?INITIALIZE_2, _Data, State) ->
    ?LOG_INFO("CSTN initialize version 2"),
    NewState = handle_cec_connect(?CEC_CONNECT_PRINT, State),
    NewState#state{version = ?CSTN_VSN_2};

handle_app_msg(?INITIALIZE_3, _Data, State) ->
    ?LOG_INFO("CSTN initialize version 3"),
    NewState = handle_cec_connect(?CEC_CONNECT_PRINT, State),
    NewState#state{version = ?CSTN_VSN_3};

handle_app_msg(Msg, Data, State) 
  when Msg == ?NAME_SPACE_1;
       Msg == ?NAME_SPACE_2->
    NewState = handle_namespace(decode_ns_addr(Msg, Data), State),
    case NewState#state.new_dn_pending of
	true when NewState#state.cec_state == done ->
	    NewCecState = connected,
	    trace_cec_state(new_dn, NewState#state.cec_state, NewCecState),
	    ask_for_name_space(NewState#state{cec_state = NewCecState, 
					      new_dn_pending = false});
	_->
	    NewState
    end;

handle_app_msg(?UPDATE = Msg, Data, State) ->
    handle_update(decode_ns_addr(Msg, Data), State);

handle_app_msg(Msg, Data, State) ->
    ?LOG_WARNING("CSTN received unknown message ~p, discarded. "
		 "Data = ~p", [Msg, Data]),
    State.

%%%===================================================================
%%% handle name space and IP address received from TN
%%%===================================================================
%%-------------------------------------
%% warm
%%-------------------------------------
handle_namespace(_Data, #state{cec_state = warm} = State) ->
    ?LOG_WARNING("CSTN response during 'warm', discarding"),
    State;

%%-------------------------------------
%% restart
%%-------------------------------------
handle_namespace(_Data, #state{cec_state = restart} = State) ->
    ?LOG_WARNING("CSTN response during protocol 'restart', discarding"),
    ask_for_name_space(State#state{cec_state = connected, 
				   ldn1 = undefined, 
				   ldn2 = undefined});

%%-------------------------------------
%% resolv1 and resolv2
%%-------------------------------------
handle_namespace({_, NS, IpAddr, _}, #state{cec_state = CecState} = State)
	when CecState =:= resolv1; CecState =:= resolv2 ->
    is_ns_error(NS),
    ?LOG_INFO("~nNameSpace = ~p~n"
	      "IpAddress = ~p~n"
	      "CEC State = ~p",
	      [NS, IpAddr, CecState]),
    case CecState of
	resolv1 ->
	    update_ns_file(NS),
	    ask_for_name_space(State#state{ns1   = NS,
					   addr1 = IpAddr});
	resolv2 ->
	    NewState = State#state{cec_state = done,
				   ns2       = NS,
				   addr2     = IpAddr},
	    notify_ootserver(NewState)
    end;

%%-------------------------------------
%% get_ns
%%-------------------------------------
handle_namespace({Ldn, NS, IpAddr, _}, 
		 #state{cec_state   = get_ns,
		        get_ns_info = {Ldn, From}} = State) ->
    ?LOG_INFO("~nNameSpace = ~p~n"
	      "IpAddress = ~p~n"
	      "CEC State = ~p",
	      [NS, IpAddr, get_ns]),
    gen_server:reply(From, {ok, 
			    NS, 
			    choose(IpAddr == undefined, "", IpAddr)}),
    State#state{cec_state   = done,
	        get_ns_info = undefined};

%%-------------------------------------
%% unknown
%%-------------------------------------
handle_namespace(_Data, #state{cec_state = Unexpected} = State) ->
    ?LOG_WARNING("CSTN response unexpected '~p', "
		 "restarting resolution", [Unexpected]),
    ask_for_name_space(State#state{cec_state = connected}).


handle_ns_timeout(State) ->
    case State#state.new_dn_pending of
	true ->
	    ?LOG_INFO("Pending new_dn from ootServer, process appmServer "
		      "msg first", []),
	    NewCecState = connected,
	    trace_cec_state(new_dn, State#state.cec_state, NewCecState),
	    ask_for_name_space(State#state{cec_state = NewCecState, 
					   new_dn_pending = false});
	_->
	    State
    end.

%%%===================================================================
%%% handle name space and IP address updates from TN
%%%===================================================================
%%-------------------------------------
%% warm
%%-------------------------------------
handle_update(_Data, #state{cec_state = warm} = State) ->
    ?LOG_WARNING("CSTN update message during 'warm', discarding"),
    State;

%%-------------------------------------
%% ordinary case
%%-------------------------------------
handle_update({Ldn, NS, Addr, _}, 
	      #state{version   = _Vsn,
		     ldn1      = Ldn,
		     cec_state = CecState} = State) ->
    ?LOG_INFO("CSTN update ordinary ~n"
	      "  LDN       = ~p~n"
	      "  NameSpace = ~p~n"
	      "  IpAddress = ~p~n"
	      "  CEC State = ~p",
	      [Ldn, NS, Addr, CecState]),
    update_ns_file(NS),
    Opts = [{?CNF_OAP_NAMESPACE, NS},
	    [{?CNF_ACC_POINT_ADDR, ootLib:to_bin(Addr)}]],
    notify_callback(Ldn, NS, Addr, State),
    ootServer:cstn_update(lists:flatten(Opts)),
    State#state{addr1 = Addr,
		ns1   = NS};
%%-------------------------------------
%% alternative case
%%-------------------------------------
handle_update({Ldn, NS, Addr, _}, 
	      #state{version   = _Vsn,
		     ldn2      = Ldn,
		     cec_state = CecState} = State) ->
    ?LOG_INFO("CSTN update alternative~n"
	      "  Alt LDN       = ~p~n"
	      "  Alt NameSpace = ~p~n"
	      "  Alt IpAddress = ~p~n"
	      "  CEC State     = ~p",
	      [Ldn, NS, Addr, CecState]),
    Opts = [{?CNF_OAP_ALT_NAMESPACE, NS},
	    [{?CNF_ACC_POINT_ADDR_ALT, ootLib:to_bin(Addr)}]],
    notify_callback(Ldn, NS, Addr, State),
    ootServer:cstn_update(lists:flatten(Opts)),
    State#state{addr2 = Addr,
		ns2   = NS};
%%-------------------------------------
%% callback case
%%-------------------------------------
handle_update({Ldn, NS, IpAddr, _}, State) ->
    ?LOG_INFO("CSTN update callback ~n"
	      "LDN       = ~p~n"
	      "NameSpace = ~p~n"
	      "IpAddress = ~p~n"
	      "CEC State = ~p",
	      [Ldn, NS, IpAddr, get_ns]),
    notify_callback(Ldn, NS, IpAddr, State).


%%===================================================================
%% do_get_ns_and_ip(MoRef, From, State) -> State
%%===================================================================
do_get_ns_and_ip(MoRef, From, State) ->
    ask_tn(?OamDNtoNsNameAndIp, MoRef, State),
    State#state{cec_state   = get_ns,
	        get_ns_info = {MoRef, From}}.

%%===================================================================
%% do_unregister_ip_change_cb(MoRef, CbMod, State) -> State
%%===================================================================
do_unregister_ip_change_cb(MoRef, CbMod, ExtraArgs, State) ->
    ?LOG_INFO("Unregister callback mod ~p for ~p", [CbMod, MoRef]),
    Ucb = State#state.update_cb,
    Val = proplists:get_value(MoRef, Ucb, []),
    NewVal = delete_cb_mod(CbMod, ExtraArgs, Val),
    UcbRm = proplists:delete(MoRef, Ucb),
    case NewVal of
	[] when State#state.cec_state =:= done,
		State#state.ldn1 =/= MoRef,
		State#state.ldn2 =/= MoRef ->
	    ?LOG_INFO("Last subscriber, unsubscribe in TN", []),
	    ask_tn(?Unsubscribe, MoRef, State),
	    State#state{update_cb = UcbRm};
	[] ->
	    ?LOG_INFO("Last subscriber", []),
	    State#state{update_cb = UcbRm};
	_ ->
	    State#state{update_cb = UcbRm ++ [{MoRef, NewVal}]}
    end.


delete_cb_mod(DelCbMod, [], CbMods) ->
    [{CbMod, EA} || {CbMod, EA} <- CbMods, CbMod =/= DelCbMod];

delete_cb_mod(CbMod, ExtraArgs, CbMods) ->
    lists:delete({CbMod, ExtraArgs}, CbMods).
    
%%===================================================================
%% do_register_ip_change_cb
%%===================================================================
do_register_ip_change_cb(MoRef, CbMod, ExtraArgs, State) ->
    ?LOG_INFO("Register callback mod ~p for ~p", [CbMod, MoRef]),
    Ucb = State#state.update_cb,
    Val = proplists:get_value(MoRef, Ucb, []),
    UcbRm = proplists:delete(MoRef, Ucb),
    NewUcb = UcbRm ++ [{MoRef, lists:usort(Val ++ [{CbMod, ExtraArgs}])}],
    State#state{update_cb = NewUcb}.

%%===================================================================
%% ask for name space
%%===================================================================
ask_for_name_space(#state{cec_state = connected, ldn1 = LDN1} = State) ->
    DN = get_dn(resolv1),
    ?LOG_INFO("ask_for_name_space in state: connected~nLDN1 = ~p~nDN1 = ~p", 
	      [LDN1, DN]),
    IsSubscribed = [] =/= proplists:get_value(LDN1, State#state.update_cb, []),
    case DN of
	LDN1 when DN =/= undefined ->
	    ask_for_name_space(State#state{cec_state = resolv1});
	DN when LDN1 =:= <<>>; 
		LDN1 =:= undefined;
		State#state.version =/= ?CSTN_VSN_3 ->
	    ask_ns(resolv1, DN, State);
	DN when IsSubscribed ->
	    ask_ns(resolv1, DN, State);
	DN ->
	    ask_tn(?Unsubscribe, LDN1, State),
	    ask_ns(resolv1, DN, State)
    end;

ask_for_name_space(#state{cec_state = resolv1, ldn2 = LDN2} = State) ->
    DN = get_dn(resolv2),
    ?LOG_INFO("ask_for_name_space in state: resolv1~nLDN2 = ~p~nDN2 = ~p", 
	      [LDN2, DN]),
    IsSubscribed = [] =/= proplists:get_value(LDN2, State#state.update_cb, []),
    case DN of
	LDN2 when DN =/= undefined ->
	    notify_ootserver(State#state{cec_state = done});
	DN when LDN2 =:= <<>>; 
		LDN2 =:= undefined;
		State#state.version =/= ?CSTN_VSN_3 ->
	    ask_ns(resolv2, DN, State);
	DN when IsSubscribed ->
	    ask_ns(resolv2, DN, State);
	DN ->
	    ask_tn(?Unsubscribe, LDN2, State),
	    ask_ns(resolv2, DN, State)
    end;

ask_for_name_space(#state{cec_state = Other} = State) ->
    ?LOG_WARNING("ask_for_name_space in state: ~p", [Other]),
    State.


ask_ns(resolv1, undefined, State) ->
    %% OamAP not set, there can be no namespace, 
    %% remove the COMEA control-file
    file:delete(?NS_FILE),
    ask_for_name_space(State#state{ns1       = <<>>,
				   ldn1      = undefined,
				   addr1     = undefined,
				   cec_state = resolv1});
ask_ns(resolv2, undefined, State) ->
    NewState = State#state{ns2       = <<>>,
			   ldn2      = undefined,
			   addr2     = undefined,
			   cec_state = done},
    notify_ootserver(NewState);
ask_ns(WhichNs, Dn, State) ->
    ?LOG_INFO("Send DN to TN for NS: ~p", [WhichNs]),
    ask_tn(?OamDNtoNsNameAndIp, Dn, State),
    case WhichNs of
	resolv1 ->
	    State#state{cec_state = resolv1,
		        ldn1      = Dn};
	resolv2 ->
	    State#state{cec_state = resolv2,
		        ldn2      = Dn}
    end.


ask_tn(Msg, Dn, #state{socket = Socket}) 
  when Socket == undefined ->
    ?LOG_WARNING("No socket, can't send ~p to TN for DN: ~p", [Msg, Dn]),
    ok;

ask_tn(Msg, Dn, #state{socket = Socket}) 
  when Msg == ?OamDNtoNsNameAndIp ->
    ?LOG_INFO("Request IP and NS from TN for DN: ~p", [Dn]),
    gen_tcp:send(Socket, [Msg, Dn, 0]);

ask_tn(Msg, Dn, #state{version = VSN, socket = Socket}) 
  when Msg == ?Unsubscribe, VSN == ?CSTN_VSN_3 ->
    ?LOG_INFO("Send Unsubscribe to TN for DN: ~p", [Dn]),
    %% append null
    gen_tcp:send(Socket, [Msg, Dn, 0]);

ask_tn(Msg, _Dn, #state{version = VSN}) ->
    ?LOG_ERROR("unknown msg: ~p, unexpected version: ~p", [Msg, VSN]),
    {error, {unknown_msg, Msg}}.    

%%%===================================================================
%%% notify oot server
%%%===================================================================
notify_ootserver(#state{version   = _Vsn,
			ldn1      = Ldn1,
			ldn2      = Ldn2,
			ns1       = Ns1,
			ns2       = Ns2,
			addr1     = Addr1,
			addr2     = Addr2,
			cec_state = CecState} = State) ->
    ?LOG_INFO("CSTN complete~n"
	      "  LDN           = ~p~n"
	      "  NameSpace     = ~p~n"
	      "  IpAddress     = ~p~n"
	      "  Alt LDN       = ~p~n"
	      "  Alt NameSpace = ~p~n"
	      "  Alt IpAddress = ~p~n"
	      "  CEC State     = ~p",
	      [Ldn1, Ns1, Addr1, Ldn2, Ns2, Addr2, CecState]),

    AddrOrig = [{?CNF_ACC_POINT_ADDR, ootLib:to_bin(Addr1)}],
    AddrAlt  = [{?CNF_ACC_POINT_ADDR_ALT, ootLib:to_bin(Addr2)}],
    
    Opts = [{?CNF_OAP_NAMESPACE,     Ns1},
	    {?CNF_OAP_ALT_NAMESPACE, Ns2},
	    AddrOrig,
	    AddrAlt],
    ootServer:cstn_complete(lists:flatten(Opts)),
    State.


%%%===================================================================
%%% notify subscribers
%%%===================================================================
notify_callback(Ldn, NS, IpAddr, #state{update_cb = Ucb} = State) ->
    ncb(proplists:get_value(l2b(Ldn), Ucb, []), Ldn, NS, IpAddr),
    State.

ncb(CbData, Ldn, NS, IpAddr) ->
    [try
	 ?LOG_INFO("Notify subscriber ~p:~nLdn = ~p~nNS = ~p~nIpAddr = ~p", 
		   [CbData, Ldn, NS, IpAddr]),
	 apply(CbMod, ip_changed, [b2l(Ldn), b2l(NS), b2l(IpAddr)] 
	       ++ ExtraArgs) 
     catch _:_E ->
	     ?LOG_ERROR("Failed to notify subscriber ~p: ~p", [CbData, _E]),
	     ok
     end || {CbMod, ExtraArgs} <- CbData].

%%%===================================================================
%%% get_dn
%%%===================================================================
%% get_dn(resolv1) ->
%%     <<"ManEl=1,SF=1,Oam=2">>;
%% get_dn(resolv2) ->
%%     <<"ManEl=1,SF=1,Oam=3">>.


get_dn(resolv1) ->
    ?LOG_INFO("Fetch AccessPoint DN from ootServer in state resolv1", []),
    ootServer:get_access_point_dn();
get_dn(resolv2) ->
    ?LOG_INFO("Fetch Alt AccessPoint DN from ootServer in state resolv2", []),
    ootServer:get_access_point_alt_dn().

%%%===================================================================
%%% reply name space
%%%===================================================================
reply_ns(#state{cec_state = done}, Ns) ->
    {ok, Ns};
reply_ns(#state{cec_state = warm}, _) ->
    {ok, <<>>};
reply_ns(_, _) ->
    {error, resolution_pending}.


%%===================================================================
%%% update name space file
%% 
%% write the OamNamespace to file for use by the script that runs NetSNMP
%% no namespace means that there should be no file
%% no support for LMT namespaces, don't do this since we don't have any
%% OamIP during warmrestart.
%%===================================================================
%% OamAccessPoint but no Ns
update_ns_file(<<>>) -> 
    file:delete(?NS_FILE);

%% OamAccessPoint with Ns
update_ns_file(Ns) ->
    case file:read_file(?NS_FILE) of
	%% file is there and already correct
	{ok, Ns} ->
	    ok;
	%% file exists but have different namespace written
	{ok, _} ->
	    write_new_ns(Ns);
	%% assume the file is missing and try to write it
	{error, _} ->
	    write_new_ns(Ns)
    end.

write_new_ns(Ns) ->
    case file:write_file(?NS_FILE, Ns) of
	ok ->
	    ok;
	{error, What} ->
	    ?LOG_ERROR("Failed to write configuration file for "
		       "Oam-network-namespace [~p]", [What])
    end.



%%===================================================================
%% decode functions
%%===================================================================
decode_ns_addr(?NAME_SPACE_1, Data) ->
    {Ldn, R2} = decode_string(Data),
    {NS,  R3} = decode_string(R2),
    {l2b(Ldn), l2b(NS), undefined, R3};
decode_ns_addr(_, Data) ->
    {Ldn, R2} = decode_string(Data),
    {NS,  R3} = decode_string(R2),
    {IpA, R4} = decode_string(R3),
    {l2b(Ldn), l2b(NS), l2b(IpA), R4}.



decode_string(Data) ->
    {Len, Rem1} = decode_32(Data),
    <<Str:Len/binary, Rem2/binary>> = Rem1, 
    {string:strip(binary_to_list(Str), both, 0), rm_null(Rem2)}.

decode_32(<<Int:1/native-unsigned-integer-unit:32, Rest/binary>>) ->
    {Int, Rest}.


rm_null(<<>>) ->
    <<>>;
rm_null(<<0, Bin/binary>>) ->
    rm_null(Bin);
rm_null(Bin) ->
    Bin.

%%%===================================================================
%% is_ns_error(NS) -> boolean()
%% 
%% corresponds to DN-NS resosolution failure - 
%% maybe we should retry now
%% a bit more code is needed HERE FIXME
%%%===================================================================
is_ns_error(<<>>) ->
    ?LOG_WARNING("DN-NS resolution failed - empty NS <<>> returned from TN"),
    true;
is_ns_error(_) ->
    false.


%%%===================================================================
%%% binary <-> list transform funtions
%%%===================================================================
l2b(List) ->
    ootLib:to_bin(List).


b2l(Bin) ->
    ootLib:to_list(Bin).


%%%===================================================================
%%% print cec state
%%%===================================================================
trace_cec_state(Event, CecState, NewCecState) ->
    ?LOG_INFO("~p. cec_state: ~p -> ~p",
	      [Event, CecState, NewCecState]).

log_state(#state{version     = Vsn,
		 socket      = Soc,
		 cec_state   = Cec,
		 ldn1        = Ldn1,
		 ns1         = Ns1,
		 addr1       = Addr1,
		 ldn2        = Ldn2,
		 ns2         = Ns2,
		 addr2       = Addr2,
		 update_cb   = Ucb,
		 get_ns_info = NsInfo
		}) ->
    
    ?LOG_INFO("~n version   = ~p~n"
	      " socket    = ~p~n"
	      " cec_state = ~p~n"
	      " ldn1      = ~p~n"
	      " ns1       = ~p~n"
	      " addr1     = ~p~n"
	      " ldn2      = ~p~n"
	      " ns2       = ~p~n"
	      " addr2     = ~p~n"
	      " cb        = ~p~n"
	      " ns_info   = ~p",
	      [Vsn, Soc, Cec, Ldn1, Ns1, Addr1, 
	       Ldn2, Ns2, Addr2, Ucb, NsInfo]).

do_print_state(#state{version     = Vsn,
		      socket      = Soc,
		      cec_state   = Cec,
		      ldn1        = Ldn1,
		      ns1         = Ns1,
		      addr1       = Addr1,
		      ldn2        = Ldn2,
		      ns2         = Ns2,
		      addr2       = Addr2,
		      update_cb   = Ucb,
		      get_ns_info = NsInfo
		     }) ->
    
    io:format("=======================================~n"
	      "version   = ~p~n"
	      "socket    = ~p~n"
	      "cec_state = ~p~n"
	      "ldn1      = ~p~n"
	      "ns1       = ~p~n"
	      "addr1     = ~p~n"
	      "ldn2      = ~p~n"
	      "ns2       = ~p~n"
	      "addr2     = ~p~n"
	      "cb        = ~p~n"
	      "ns_info   = ~p~n"
	      "=======================================~n",
	      [Vsn, Soc, Cec, Ldn1, Ns1, Addr1, 
	       Ldn2, Ns2, Addr2, Ucb, NsInfo]).



%%%===================================================================
%%% misc functions
%%%===================================================================
choose(true,  T, _) -> T;
choose(false, _, F) -> F.


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

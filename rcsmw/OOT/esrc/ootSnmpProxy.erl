%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootSnmpProxy.erl %
%%% Author:	etxlg
%%% Description: proxy between COM and NetSNMP (snmpd) when namespacing on
%%%		 OaM is used (the normal case)
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(ootSnmpProxy).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/R6A/1').
-date('2016-06-17').
-author('etomist').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R3A/1      2015-02-02 etxlg       Created
%%% R3A/2      2015-02-03 etxlg       undef in callback-fun
%%% R3A/3      2015-02-03 etxlg       receive agent_update
%%% R3A/4      2015-02-06 etxlg       bug fix
%%% R3A/5      2015-03-12 etxlg       Workaround for oot_not_started
%%% R4A/1      2015-09-24 eolaand     Change error_logger to sysInitI 
%%% R6A/1      2016-06-17 etomist     HU85518, clearing of buffered 
%%%                                   SNMP messages when changing sockets
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%called from COMSA  (through ootI) when the agentlistener port changes
-export([agent_update/1]).

%for debug
-export([stop/0]).


%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-export([config_update/1]). %fully qualified mod:fun() given in fun()
%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SERVER, ?MODULE).
-define(LOOPBACK, {127,0,0,1}).


%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------
-record(st, {state = false,
	     agent_ip,		%string("1.2.3.4")
	     agent_port,	%integer().
	     oam_ns_opt_list,   %[{netns,<<"/var/run/netns/fib_3">>}]
	     ns,		%ns in oot format, i.e. <<"fib_3">>
	     proxy_sock_com,
	     proxy_sock_snmp,
	     cached_com_port,
	     timer,
	     dummy}).


%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------

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
%%% Starts the SNMP proxy server
%%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    %case sysEnv:target() of
%	true ->
%	    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%	false ->
%	    ignore
%    end.

agent_update(Prop_list) ->
    gen_server:cast(?SERVER, {agent_update, Prop_list}).

stop() ->
    gen_server:cast(?SERVER, stop).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR UNEXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% @private
%called by fun registered in ootServer
config_update(Prop_list) ->
    case proplists:get_value(oap_namespace, Prop_list) of
	undefined -> 
	    ok;
	New_ns ->
	    gen_server:cast(?SERVER, {config_update, New_ns})
    end.
%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%% ----------------------------------------------------------
%%% #           init(Args)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
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
    self() ! async_init, %release and do all work outside of init
    {ok, #st{state = init}}.

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

handle_call(Call, _From, S) ->
    Reply = {error, {unknown_call, Call}},
    {reply, Reply, S}.

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
handle_cast(stop, S) ->
    warn_msg("Stopping by command"),
    {stop, normal, S};
handle_cast({config_update, Ns}, #st{ns = Ns} = S) ->
    %no change
    {noreply, S};
handle_cast({config_update, Ns}, #st{} = S) ->
    {noreply, run_async_init(S, {ok, Ns})};
handle_cast({agent_update, Prop_list}, S) ->
    New_host = proplists:get_value(host, Prop_list, S#st.agent_ip),
    New_port = proplists:get_value(port, Prop_list, S#st.agent_port),
    case New_port =:= S#st.agent_port andalso
	 inet:parse_address(New_host) =:= inet:parse_address(S#st.agent_ip) of
	true -> %nothing changed
	    {noreply, S};
	false ->
	    {noreply, run_async_init(S)}
    end;
    %warn_msg("agent_update: Not yet implemented: ~p", [Prop_list]),
handle_cast(_Msg, S) ->
    {noreply, S}.

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
handle_info({timeout, What}, S) ->
    New_s = S#st{timer = undefined},
    case What of
	async_init ->
	    handle_info(async_init, New_s);
	_ -> %just restart everything
	    {noreply, run_async_init(S)}
    end;
handle_info(async_init, #st{state = init} = S) ->
    %do the stuff that could have been in init (this may speed things up)
    %register the fun with module to allow module reloading
    case ootI:register_cfg_upd_cb(fun ?MODULE:config_update/1) of
	ok ->
	    {noreply, run_async_init(S)};
	Err -> %workaround for oot_not_started (which seems unlikely but...)
	    sysInitI:info_msg(
	      "~p: OOT-server not ready: ~p - retry in one second~n",
	      [?MODULE, Err]),
	    erlang:send_after(1000, self(), async_init),
	    {noreply, S}
    end;
handle_info(Udp, #st{state = running} = S) when element(1, Udp) =:= udp ->
    {noreply, handle_udp(Udp, S)};
handle_info(_Info, #st{state = running} = S) ->
    sysInitI:info_msg("~p: running: handle_info: ~p~n", [?MODULE, _Info]),
    {noreply, S};
handle_info(_Info, S) ->
    sysInitI:info_msg("~p: handle_info: ~p~n", [?MODULE, _Info]),
    {noreply, S}.

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
terminate(_Reason, _S) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
handle_udp({udp, Port, ?LOOPBACK, From_port, Data},
	   #st{proxy_sock_com = Port} = S) ->
    %it is from COM send it to snmpd
    %case gen_udp:send(S#st.proxy_sock_snmp,OAMPIPHERE
    case gen_udp:send(S#st.proxy_sock_snmp, ?LOOPBACK,
		      S#st.agent_port, Data) of
	ok ->
	    S#st{cached_com_port = From_port};
	{error, What} ->
	    ratelimit_print(S, ["failed send to snmpd", What])
     end;

handle_udp({udp, Port, ?LOOPBACK, _FromPort, Data},
	   #st{proxy_sock_snmp = Port} = S) ->
    %it is from snmpd send it to COM
    case S#st.cached_com_port of
	undefined ->
	    ratelimit_print(S, ["no port to respond back to COM"]),
	    S;
	To_port ->
	     case gen_udp:send(S#st.proxy_sock_com, ?LOOPBACK,
			       To_port, Data) of
		ok ->
		    S;
		{error, What} ->
		    ratelimit_print(S, ["failed to send to COM", What])
	    end
    end;

handle_udp({udp, Port, _, _, _}, S) ->
    sysInitI:info_msg("unhandled UDP message on port ~p~n", [Port]),
    S.

%this strange wrapping is to make it possible setting the NS through
%config_update - it may break stuff since later omc_api is used
run_async_init(Old_state) ->
    run_async_init(Old_state, ootI:get_oap_namespace()).

run_async_init(Old_state, Oot_ns) ->
    S = cancel_any_timer(Old_state),
    case Oot_ns of
	{ok, <<>>} -> % ME isn't namespaced (or not yet)
	    stop_proxy(S);
	{ok, Ns}->    % normal case
	    Props = comsaI:get_snmp_agent_params(),
	    Agent_ip = proplists:get_value(host, Props),
	    Agent_port = proplists:get_value(port, Props),
	    case {Agent_ip, Agent_port} of
		{undefined, _} ->
		    stop_proxy(S);
		{_, undefined} ->
		    stop_proxy(S);
		{Agent_ip, Agent_port} ->
		    New_s = #st{agent_ip = Agent_ip,
			    agent_port = Agent_port,
			    oam_ns_opt_list = omc_api:get_oam_ns_opt_list(),
			    ns = Ns},
		    run_proxy(New_s)
	    end;
	{error, _} -> %maybe namespace resolution pending, retry later
	    run_timer(S#st{state = init}, async_init)
    end.
    
run_proxy(S) ->
    case inet:parse_address(S#st.agent_ip) of
	{ok, {0,0,0,0}} -> ok;
	_ ->
	    warn_msg("Unless the agent address is set to 0.0.0.0, SNMP is "
		     "unlikely to work, current agent address: ~p",
		     [S#st.agent_ip])
    end,
    case open_com_sock(S) of
	{ok, New_s} ->
	    case open_snmp_sock(New_s) of
		{ok, Last_s} ->
		    Last_s#st{state = running};  %good case
		{error, Last_s} ->
		    Last_s
	    end;
	{error, New_s} ->
	    New_s
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% Implemented as a fix for TR HU85518 - A socket was closed before 
%% all of its messages were handled. This function should be called 
%% before closing the socket. 
%%
%% @spec handle_leftover_msgs(Sock, State) -> State
%% @end
%%--------------------------------------------------------------------
handle_leftover_msgs(Sock, State) ->
    receive
        {udp, Sock, _, _, _} = Msg ->
            NewState = handle_udp(Msg, State),
            handle_leftover_msgs(Sock, NewState)
    after 
        0 ->
            State
    end.

open_com_sock(State) ->
    S = handle_leftover_msgs(State#st.proxy_sock_com, State),
    maybe_close(S#st.proxy_sock_com),
    Common_opts = [binary, inet, {active, true}, {reuseaddr, true},
		  {ip, ?LOOPBACK}],
    case gen_udp:open(S#st.agent_port, Common_opts) of
	{ok, Sock} ->
	    {ok, S#st{proxy_sock_com = Sock}};
	{error, Why} ->
	    warn_msg("Failed to open proxy_sock_com: ~p - will retry", [Why]),
	    {error, run_timer(S#st{proxy_sock_com = undefined}, com_sock)}
    end.

open_snmp_sock(State) ->
    S = handle_leftover_msgs(State#st.proxy_sock_snmp, State),
    maybe_close(S#st.proxy_sock_snmp),
    Common_opts = [binary, inet, {active, true}, {reuseaddr, true},
		  {ip, ?LOOPBACK}],
    case gen_udp:open(0, Common_opts ++ S#st.oam_ns_opt_list) of
	{ok, Sock} ->
	    {ok, S#st{proxy_sock_snmp = Sock}};
	{error, Why} ->
	    warn_msg("Failed to open proxy_sock_snmp: ~p - will retry", [Why]),
	    {error, run_timer(S#st{proxy_sock_snmp = undefined}, snmp_sock)}
    end.

stop_proxy(S) ->
    maybe_close(S#st.proxy_sock_com),
    maybe_close(S#st.proxy_sock_snmp),
    #st{state = inactive}.
	 

%make sure we do not run more than one timer at a time
run_timer(S, What) ->
    run_timer(S, What, 1000).
run_timer(#st{timer = undefined} = S, What, Timeout) ->
    Tref = erlang:send_after(Timeout, self(), {timeout, What}),
    S#st{timer = Tref}. %;
%dialyzer correctly deduces that the following clause will never match
%however, I'm fairly sure that it will match _eventually_, i.e. after this
%module has been fixed a bit.
%run_timer(S, What, _) ->
%    warn_msg("Tried to run timer: ~p, but something is already running",
%	     [What]),
%    S.

cancel_any_timer(#st{timer = undefined} = S) ->
    S;
cancel_any_timer(#st{timer = Tmr} = S ) ->
    case erlang:cancel_timer(Tmr) of
	false -> %maybe already fired flush mbox
	    receive
		{timeout, _} -> ok
	    after
		0 -> ok
	    end;
	_ ->
	    ok
    end,
    S#st{timer = undefined}.

maybe_close(undefined) ->
    ok;
maybe_close(Sock) ->
    catch gen_udp:close(Sock).

ratelimit_print(S, What) ->
    warn_msg("NO ratelimit yet - ~p", [What]),
    S.

warn_msg(Format) ->
    warn_msg(Format, []).
warn_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format ++ "~n", [?MODULE | Args]).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%dbg(Format) ->
%    dbg(Format, []).
%dbg(Format, Args) ->
%    io:format("~w: dbg: " ++ Format ++ "~n", 
%	[?MODULE | Args]).

%%% #---------------------------------------------------------

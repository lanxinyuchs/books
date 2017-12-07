%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaNtpProxy.erl %
%%% Author:	etxlg
%%% Description: proxy for NTP (ntpd) when namespacing on
%%%		 OaM is used (the normal case)
%%%
%%%
%%% ----------------------------------------------------------
-module(comsaNtpProxy).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R5A/1').
-date('2015-11-09').
-author('uabesvi').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% R5A/1      2015-11-09 uabesvi     Modified to NTP (copied from ootSnmpProxy)
%%%                                   Not ready, not fully tested and 
%%%                                   not activated
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%for debug
-export([stop/0]).


%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-export([config_update/1]). %% fully qualified mod:fun() given in fun()
-export([print_state/0]).

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SERVER, ?MODULE).
-define(LOOPBACK, {127,0,0,1}).

-define(OP_CODE_TRAP, 7).
-define(OP_CODE_RSTATUS, 1).


-define(NTP_DEFAULT_PORT, 123).
-define(NTP_GET_NEW_PORT, 0).

-define(COMMON_SOCKET_OPTS, [binary,
			     inet,
			     {active, true},
			     {reuseaddr, true},
			     {ip, ?LOOPBACK}]).

%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------
-record(st, {state = false,
	     oam_ns_opt_list,   %% [{netns,<<"/var/run/netns/fib_3">>}]
	     ns,		%% ns in oot format, i.e. <<"fib_3">>
	     proxy_sock_core,
	     proxy_sock_regular,
	     core_port,         %% integer(), proxy port towards core ntpd
	     cached_reg_port,
	     cached_reg_addr,
	     timer,
	     verbosity     = 3,
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
%% @doc
%% Starts the NTP proxy server
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

print_state() ->
    gen_server:cast(?SERVER, print_state).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR UNEXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%% @private
%% called by fun registered in ootServer
config_update(PropList) ->
    case proplists:get_value(oap_namespace, PropList) of
	undefined -> 
	    ok;
	NewNs ->
	    gen_server:cast(?SERVER, {config_update, NewNs})
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
    self() ! async_init, %% release and do all work outside of init 
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
    %% no change
    {noreply, S};
handle_cast({config_update, Ns}, #st{} = S) ->
    {noreply, run_async_init(S, {ok, Ns})};
handle_cast(print_state, S) ->
    ps(S),
    {noreply, S};
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
%%==========================================================================
%% async_init
%% 
%% do the stuff that could have been in init (this may speed things up)
%% register the fun with module to allow module reloading
%%==========================================================================
handle_info(async_init, #st{state = init} = S) ->
    case ootI:register_cfg_upd_cb(fun ?MODULE:config_update/1) of
	ok ->
	    NS = run_async_init(S),
	    ps(NS),
	    {noreply, NS};
%%	    {noreply, run_async_init(S)};
	Err -> %% workaround for oot_not_started (which seems unlikely but...)
	    sysInitI:info_msg(
		"~p: OOT-server not ready: ~p - retry in one second~n",
		[?MODULE, Err]),
	    erlang:send_after(1000, self(), async_init),
	    {noreply, S}
    end;

handle_info({timeout, async_init}, S) ->
    NewS = S#st{timer = undefined},
    handle_info(async_init, NewS);
%% just restart everything
handle_info({timeout, _What}, S) ->
    {noreply, run_async_init(S)};


handle_info(Udp, #st{state = running} = S) when element(1, Udp) =:= udp ->
    {noreply, handle_udp(Udp, S)};
handle_info(_Info, #st{state = running} = S) ->
    sysInitI:info_msg("~p: running: handle_info: ~p~n", [?MODULE, _Info]),
    {noreply, S};
handle_info(_Info, S) ->
    sysInitI:info_msg("~p: Unknown handle_info: ~p~n", [?MODULE, _Info]),
    {noreply, S}.


ps(#st{state              = S,
       oam_ns_opt_list    = Opt, 
       ns                 = NS,		
       proxy_sock_core    = PsCore,
       proxy_sock_regular = PsReg,
       core_port          = CoreP,       
       cached_reg_port    = RegP,
       cached_reg_addr    = RegA,
       timer              = Timer,
       verbosity          = V,
       dummy              = D}) ->
    io:format("~n~n#### PROXY STATE ~n"
	      "state              = ~p~n"
	      "oam_ns_opt_list    = ~p~n"
	      "ns                 = ~p~n"
	      "proxy_sock_core    = ~p~n"
	      "proxy_sock_regular = ~p~n"
	      "core_port          = ~p~n"       
	      "cached_reg_port    = ~p~n"
	      "cached_reg_addr    = ~p~n"
	      "timer              = ~p~n"
	      "verbosity          = ~p~n"
	      "dummy              = ~p~n",
	      [S, Opt, NS, PsCore, PsReg, CoreP, RegP, RegA, Timer, V, D]).


    
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

%%==========================================================================
%% handle_udp() 
%%==========================================================================
%% it is from Regular node. Send it to core
handle_udp({udp, SocketReg, LB, FromPort, Data},
	   #st{proxy_sock_regular = SocketReg,
	       proxy_sock_core    = SocketCore,
	       core_port          = _CorePort} = S) ->
    hu(regular, Data, S),
    p("~n~nPROXY  ======= REGULAR -> CORE =============~n"
      "Sock: ~p  FromPort ~p  LB ~p~n",
      [SocketReg, FromPort, LB]),
%%     conditional_report(S, 1, "PROXY  Forwarding Msg to: ~p", [S#st.proxy_sock_core]),
    case send_socket(SocketCore, ?LOOPBACK, ?NTP_DEFAULT_PORT, Data) of
	ok            -> to(), S#st{cached_reg_port = FromPort,
				    cached_reg_addr = LB};
	{error, What} -> ratelimit_print(S, ["failed send to core", What])
    end;

%% it is from core send it to regular
handle_udp({udp, SocketCore, ?LOOPBACK, FromPort, Data},
	   #st{proxy_sock_regular = SocketReg,
	       proxy_sock_core    = SocketCore,
	       cached_reg_port    = ToPort,
	       cached_reg_addr    = ToAddr} = S) ->
    hu(core, Data, S),
    ps(S),
    p("~n~nPROXY  ======= CORE -> REGULAR =============~n"
      "Sock: ~p  FromPort ~p~n",
      [SocketCore, FromPort]),
%%     conditional_report(S, 1, "PROXY  Forwarding Msg to: ~p", [S#st.proxy_sock_regular]),
    case send_socket(SocketReg, ToAddr, ToPort, Data) of
	ok            -> to(), S;
	{error, What} -> ratelimit_print(S, ["failed to send to regular", What])
    end.


to() ->
    timer:sleep(50).



hu(_,_,_) ->
    ok.

%% hu(From,
%%    <<_:2, 4:3, 6:3, 1:1, 0:1, _M:1, ?OP_CODE_RSTATUS:5, _/binary>>,
%%    _S) ->
%%     p("PROXY  From ~p: Got a rstatus packet~n)", [From]);
%% hu(From,
%%    <<_:2, 4:3, 6:3, 1:1, 0:1, _M:1, ?OP_CODE_TRAP:5, _/binary>>,
%%    _S) ->
%%     p("PROXY  From ~p: Got a trap packet~n", [From]);
%% hu(From,
%%    <<_:2, 4:3, 6:3, 1:1, 0:1, _M:1, Msg:5, _/binary>>,
%%    _S) ->
%%     p("PROXY  From ~p :Got a Msg: ~p~n", [From, Msg]);
%% hu(From, Msg, _S) ->
%%     p("PROXY  From ~p :Got a UDP Msg: ~p~n", [From, Msg]).
    





%%==========================================================================
%% run_async_init(OldState) -> NewState
%% 
%% this strange wrapping is to make it possible setting the NS through
%% config_update - it may break stuff since later omc_api is used
%%==========================================================================
run_async_init(OldState) ->
    run_async_init(cancel_any_timer(OldState),
		   ootI:get_oap_namespace()).

%% ME isn't namespaced (or not yet)
run_async_init(S, {ok, <<>>}) ->
    sysInitI:info_msg("~p: run_async_init: ~p~n", [?MODULE, no_name_space]),    
    stop_proxy(S);
%% normal case
run_async_init(S, {ok, Ns}) ->
    sysInitI:info_msg("~p: run_async_init Name space: ~p~n", [?MODULE, Ns]),    
    rai_core_sock(Ns, S);
%% maybe namespace resolution pending, retry later
run_async_init(S, {error, _}) ->
    run_timer(S#st{state = init}, async_init).


rai_core_sock(Ns, _S) ->
    NewS = #st{oam_ns_opt_list = omc_api:get_oam_ns_opt_list(),
	       ns              = Ns},
    rai_reg_sock(open_core_sock(NewS)).

rai_reg_sock({ok, S}) ->
    case open_regular_sock(S) of
	{ok, NewS} ->
	    NewS#st{state = running};  %% good case
	{error, NewS} ->
	    NewS
    end;
rai_reg_sock({error, S}) ->
    S.




open_core_sock(S) ->
    maybe_close(S#st.proxy_sock_core),
    case open_socket(?NTP_GET_NEW_PORT, S#st.oam_ns_opt_list) of
	{{ok, Sock}, {ok, Port}} = X ->
	    p("### PROXY open CORE socket   Port ~p  Opt ~p~n    Res ~p~n",
	      [?NTP_GET_NEW_PORT, S#st.oam_ns_opt_list, X]),
	    {ok, S#st{proxy_sock_core = Sock,
		      core_port       = Port}};
	{{ok, Sock}, {error, Why}} ->
	    warn_msg("Failed to get port_core: ~p - will retry", [Why]),
	    maybe_close(Sock),
	    {error, run_timer(S#st{proxy_sock_core = undefined}, core_sock)};
	{error, Why} ->
	    warn_msg("Failed to open proxy_sock_core: ~p - will retry", [Why]),
	    {error, run_timer(S#st{proxy_sock_core = undefined}, core_sock)}
    end.


open_regular_sock(S) ->
    maybe_close(S#st.proxy_sock_regular),
    case open_socket(?NTP_DEFAULT_PORT, []) of
	{{ok, Sock}, _} = X ->
	    p("### PROXY open REGULAR socket   Port ~p  Opt ~p~n    Res ~p~n",
	      [?NTP_DEFAULT_PORT, [], X]),
	    {ok, S#st{proxy_sock_regular = Sock}};
	{error, Why} ->
	    warn_msg("Failed to open proxy_sock_regular: ~p - will retry", [Why]),
	    {error, run_timer(S#st{proxy_sock_regular = undefined}, regular_sock)}
    end.


open_socket(Port, Opts) ->
    os(gen_udp:open(Port, Opts)).

os({ok, Sock} = S) ->
    {S, inet:port(Sock)};
os({error, _} = E) ->
    E.

send_socket(Socket, Addr, Port, Data) ->
    p("### PROXY send Socket ~p  Addr  ~p  Port ~p~nData ~p~n", [Socket, Addr, Port, Data]),
    gen_udp:send(Socket, Addr, Port, Data).
%%    gen_udp:send(Socket, ?LOOPBACK, ?NTP_DEFAULT_PORT, Data).


stop_proxy(S) ->
    maybe_close(S#st.proxy_sock_core),
    maybe_close(S#st.proxy_sock_regular),
    #st{state = inactive}.
	 


maybe_close(undefined) ->
    ok;
maybe_close(Sock) ->
    catch gen_udp:close(Sock).


%%==========================================================================
%% Timer functions
%%==========================================================================

%% make sure we do not run more than one timer at a time
run_timer(S, What) ->
    run_timer(S, What, 1000).
run_timer(#st{timer = undefined} = S, What, Timeout) ->
    Tref = erlang:send_after(Timeout, self(), {timeout, What}),
    S#st{timer = Tref}. %;
%% dialyzer correctly deduces that the following clause will never match
%% however, I'm fairly sure that it will match _eventually_, i.e. after this
%% module has been fixed a bit.
%% run_timer(S, What, _) ->
%%    warn_msg("Tried to run timer: ~p, but something is already running",
%%	     [What]),
%%    S.

cancel_any_timer(#st{timer = undefined} = S) ->
    S;
cancel_any_timer(#st{timer = Tmr} = S ) ->
    case erlang:cancel_timer(Tmr) of
	false -> %% maybe already fired flush mbox
	    receive
		{timeout, _} -> ok
	    after
		0 -> ok
	    end;
	_ ->
	    ok
    end,
    S#st{timer = undefined}.


%%==========================================================================
%% misc functions
%%==========================================================================
ratelimit_print(S, What) ->
    warn_msg("NO ratelimit yet - ~p", [What]),
    S.

warn_msg(Format) ->
    warn_msg(Format, []).
warn_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format ++ "~n", 
	[?MODULE | Args]).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%% dbg(Format) ->
%%    dbg(Format, []).
%% dbg(Format, Args) ->
%%    io:format("~w: dbg: " ++ Format ++ "~n", 
%%	[?MODULE | Args]).

%%% #---------------------------------------------------------

%% conditional_report(Vlvl, Lvl, _, _) when Lvl > Vlvl ->
%%     ok;
%% conditional_report(_, _, Format, Args) ->
%%     io:format("~n*** ~s ***~n", [comsaNtpUtil:mk_ds()]),
%%     report(Format, Args).

%% report(Format, Params) ->
%%     io:format("~s" ++ Format ++ "~n",
%% 	["NTP event: " | Params]).


p(S, A) ->
    io:format(S,A).

%% p(_,_) ->
%%     ok.

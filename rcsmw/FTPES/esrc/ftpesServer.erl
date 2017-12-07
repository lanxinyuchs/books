%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ftpesServer.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/R10A/R12A/6
%%%
%%% ----------------------------------------------------------

-module(ftpesServer).
-behaviour(gen_server).
-vsn('/main/R8A/R9A/R10A/R12A/6').
-date('2017-12-06').
-author('enekdav').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/5    2016-11-23   eivmiha    Specs for ged_bind_address()
%%% R8A/6    2016-11-23   estjako    Client fun
%%% R8A/9    2016-11-28   ekurnik    Redesign according to US 4.1 Ip interfaces
%%% R8A/10   2016-11-29   ekurnik    Fixed conversion of alt_oam address
%%% R8A/11   2016-11-28   estjako    Cipher suite change nofity function
%%% R8A/12   2016-11-30   emarnek    Reuse_sessions in setup_tls is now false
%%% R8A/13   2016-12-02   emarnek    Checking ip interface priorities for client added
%%% R8A/14   2016-12-02   estjako    Config update for client
%%% R8A/16   2016-12-06   emarnek    Checking IP family for client added
%%% R8A/17   2016-12-06   estjako    Added client_args fun; handle_call changed
%%% R8A/20   2016-12-06   ekurnik    Fixed server OAM reconfig behaviour
%%% R8A/21   2016-12-07   ekurnik    Fixed dialyzer warnings
%%% R8A/22   2016-12-07   estjako    Change in mnesia_table_event
%%% R8A/28   2016-12-13   eivmiha    Added stop_clients function, config update changes
%%% R8A/29   2016-12-13   ekurnik    Added support for running FTPES server on SIM
%%% R8A/30   2016-12-15   estjako    Changes in start_client
%%% R8A/32   2016-12-19   enekdav    General logging redesign
%%% R8A/33   2016-12-16   ekurnik    Verify_fun getter temp solution
%%% R8A/35   2016-12-19   ekurnik    Verify_fun full solution
%%% R8A/40   2016-12-22   ekurnik    Small changes in logging
%%% R8A/41   2016-12-23   ekurnik    Added 'active' flag to state
%%% R8A/42   2017-01-03   ekurnik    NC & TC are now passed from state, 
%%%                                  not read from DB again
%%% R9A/1    2017-01-24   ekurnik    handle_recv_chunk added, changed client definition
%%% R9A/5    2017-02-10   ekurnik    Redesign - moved client parts to other modules
%%% R9A/6    2017-02-16   estjako    Changed ftpes_cipher_notify fun
%%% R9A/7    2017-03-29   ekurnik    cert_event timing issue fix
%%% R9A/9    2017-04-04   ekurnik    Graceful close on terminate
%%% R10A/1   2017-04-27   ekurnik    Added OOT ready check when starting
%%% R12A/1   2017-11-24   eivmiha    Added idle timer
%%% R12A/2   2017-11-27   eivmiha    Idle timer fix
%%% R12A/3   2017-11-23   emirbos    Added control connection port mnesia table event
%%% R12A/4   2017-11-23   enekdav    Changed NC and TCat mnesia table event for server
%%% R12A/5   2017-11-29   emirbos    Small update for control connection port mnesia table event
%%% R12A/4   2017-12-06   enekdav    Added min and max data ports to record, mnesia event 
%%%                                  and listener arguments
%%%--------------------------------------------------------------------


%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1, start_link/1, init/1, stop/0, start_ftpesd_server/0,
         stop_ftpesd_server/0, change_notify/1, cert_event/1, ftpes_cipher_notify/0,
         get_ftpes_port_conf/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port, %% default is 9921
                serv_options, %% list of parameters, defined later in code
                ftpes_serv_list = [], %% list of ftpes servers
                active = false, %% true when activate is called from init module
                idle_timer,
                minDataPort,
                maxDataPort
               }
       ).

%% Represents FTPES server instance
-record (ftpes_serv, {type, %% IP interface which server binds to
                      bind_address,
                      namespace,
                      listener_pid = undefined}).

%% Common options for all server instances
-record (common_serv_options, {node_credential,
                               trust_category,
                               administrative_state}).

-include("RcsFileTPM.hrl").
-include("ftpesd.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Config], []).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

init([Config]) ->
    process_flag(trap_exit, true),
    
    NewState = #state{port = proplists:get_value(port, Config), 
                      idle_timer = proplists:get_value(idle_timer, Config),
                      minDataPort = proplists:get_value(minDataPort, Config),
                      maxDataPort = proplists:get_value(maxDataPort, Config)},
    {ok, NewState}.

stop() ->
    gen_server:cast(?MODULE, shutdown).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES server
%%% ----------------------------------------------------------
-spec start_ftpesd_server() ->
          ok.
%%% ###=====================================================================###
start_ftpesd_server() ->
    gen_server:cast(?MODULE, start_ftpesd_server).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Stops a FTPES server
%%% ----------------------------------------------------------
-spec stop_ftpesd_server() ->
          ok.
%%% ###=====================================================================###
stop_ftpesd_server() ->
    gen_server:call(?MODULE, stop_ftpesd_server).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Getting and processing Changes from OOT
%%% ----------------------------------------------------------
-spec change_notify(Changes :: list()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
change_notify(Changes) ->
    gen_server:cast(?MODULE, {change_notify, Changes}).

%% FIXME: Temporary solution for CERT timing issue
cert_event(MoRef) ->
    timer:apply_after(?CERT_EVENT_OFFSET, gen_server, cast, [?MODULE, {cert_event, MoRef}]).

ftpes_cipher_notify() ->
    case whereis(?MODULE) of
        undefined -> 
            ok;
        _->
            gen_server:cast(?MODULE, ftpes_cipher_notify)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
handle_call(_Msg, _From, #state{active = false} = State)->
    sysInitI:warning_msg("FTPES server is not active!"),
    {reply, {error, not_active}, State};

handle_call(stop_ftpesd_server, _From, State)->
    sysInitI:info_msg("~p: Stopping FTPES server.~n", [?MODULE]),
    NewState = do_stop_ftpesd_server(State),
    {reply, ok, NewState#state{active = false}};

handle_call(Msg, _From, State)->
    sysInitI:warning_msg("~p: Unexpected call: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_cast(start_ftpesd_server, #state{active = false} = State) ->
    {_, NewState} = do_start_ftpesd_server(State),
    {noreply, NewState};

handle_cast(start_ftpesd_server, #state{active = true} = State) ->
    %% already started
    {noreply, State};

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, #state{active = false} = State)->
    sysInitI:warning_msg("FTPES server is not active!"),
    {noreply, State};


handle_cast({change_notify, Changes}, #state{ftpes_serv_list = OldServerConfig} = State) ->
    %% Read new ip config
    NewServerConfig = get_ftpes_servers_config(Changes, OldServerConfig),
    
    case compare_ip_configs(NewServerConfig, OldServerConfig) of
        [] ->
            %%no change
            sysInitI:info_msg("~p: config_update received: ~p~nNo change~n", [?MODULE, Changes]),
            {noreply, State};
        Diff ->
            %% reconfig only servers which changed
            sysInitI:info_msg("~p: config_update received: ~p~nFollowing interfaces changed: ~p~n", [?MODULE, Changes, Diff]),
            reconfig_server(Diff, State, ip)
    end;

handle_cast({cert_event, MoRef}, State) ->
    sysInitI:info_msg("~p: cert_event received - MO changed: ~p~n", [?MODULE, MoRef]),
    reconfig_server(State, mo_changed);


handle_cast(ftpes_cipher_notify, State) ->
    sysInitI:info_msg("~p: cipher_notify received~n", [?MODULE]),
    reconfig_server(State, mo_changed);

handle_cast(Msg, State)->
    sysInitI:warning_msg("~p: Unexpected cast: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(_Msg, #state{active = false} = State)->
    sysInitI:warning_msg("FTPES server is not active!"),
    {noreply, State};

%% no change
handle_info({mnesia_table_event, {write, ftpServer, New, [New], _ }}, State) ->
    sysInitI:info_msg("~p: ftpServer mnesia event received - no change~n", [?MODULE]),
    {noreply, State};

handle_info({mnesia_table_event, {write, ftpServer, #ftpServer{idleTimer = NewIdleTimer},_, _ }}, State) ->
    sysInitI:info_msg("~p: ftpServer mnesia event received.~nidleTimer: ~p~n", [?MODULE, NewIdleTimer]),
    reconfig_server(State#state{idle_timer = NewIdleTimer}, idle_timer); 


%% no change
handle_info({mnesia_table_event, {write, ftpTlsServer, New, [New], _ }}, State) ->
    sysInitI:info_msg("~p: ftpTlsServer mnesia event received - no change~n", [?MODULE]),
    {noreply, State};

handle_info({mnesia_table_event, {write, ftpTlsServer, New, [Old], _ }}, State) ->
    CommonParams = State#state.serv_options,
    %% Check if node credential and trust category have been changed
    check_nc_tc_changes({New#ftpTlsServer.nodeCredential, New#ftpTlsServer.trustCategory},
                        {Old#ftpTlsServer.nodeCredential, Old#ftpTlsServer.trustCategory}),
    NewControlConnectionPort = New#ftpTlsServer.port, 
    NewCommonParams = CommonParams#common_serv_options
           {node_credential = New#ftpTlsServer.nodeCredential,
            trust_category = New#ftpTlsServer.trustCategory,
            administrative_state = New#ftpTlsServer.administrativeState},
    NewState = State#state{serv_options = NewCommonParams, port = NewControlConnectionPort,
                           minDataPort = New#ftpTlsServer.minDataPort, maxDataPort = New#ftpTlsServer.maxDataPort},
    sysInitI:info_msg("~p: ftpTlsServer mnesia event received.~nnodeCredential: ~p~ntrustCategory: ~p~nadministrativeState: ~p~n"
                      "port: ~p~nminDataPort: ~p~nmaxDataPort: ~p~n",
                      [?MODULE, New#ftpTlsServer.nodeCredential, New#ftpTlsServer.trustCategory,
                       New#ftpTlsServer.administrativeState, New#ftpTlsServer.port,
                       New#ftpTlsServer.minDataPort, New#ftpTlsServer.maxDataPort]),
    reconfig_server(NewState, new_params);

handle_info({'EXIT', FromPid, Reason}, State) ->
    sysInitI:warning_msg("~p: Unexpected exit signal received from: ~p~nReason: ~p~n", [?MODULE, FromPid,Reason]),
    {noreply, State};

handle_info(Message, State) ->
    sysInitI:warning_msg("~p: Unexpected message received: ~p~n", [?MODULE, Message]),
    {noreply, State}.

terminate(_Reason, State) ->
    do_stop_ftpesd_server(State),
    ok.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

check_nc_tc_changes(New, New) ->
    ok;

check_nc_tc_changes(New, _Old) ->
    {NewNc, NewTc} = New,
    ftpesLib:unsubscribe_mo(?MODULE, NewNc, NewTc),
    ftpesLib:subscribe_mo(?MODULE, NewNc, NewTc).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This should be called only once when starting ftpesServer
%%% ----------------------------------------------------------

initialize_state(State) ->
    mnesia:subscribe({table, ftpTlsServer, detailed}),
    mnesia:subscribe({table, ftpServer, detailed}),
    
    {NodeCredential, TrustCategory} = ftpesLib:get_server_cert(),
    ftpesLib:subscribe_mo(?MODULE, NodeCredential, TrustCategory),
    AdminState = ftpesLib:get_server_admin_state(),
    
    NewState = #state{port = State#state.port,
                      ftpes_serv_list = get_ftpes_servers_config(),
                      serv_options = #common_serv_options 
                        {node_credential = NodeCredential,
                         trust_category = TrustCategory,
                         administrative_state = AdminState
                        },
                      idle_timer = State#state.idle_timer,
                      minDataPort = State#state.minDataPort,
                      maxDataPort = State#state.maxDataPort
                     },
    
    ok = ootI:register_cfg_upd_cb(fun ?MODULE:change_notify/1),
    
    ftpesd_sup:start_link(),
    NewState#state{active = true}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to start FTPES listener on all IP interfaces
%%% ----------------------------------------------------------
-spec do_start_listeners(State :: #state{}) -> #state{}.
do_start_listeners(State) ->
    NewFtpesServList = [start_listener(Serv, State) || Serv <- State#state.ftpes_serv_list],
    State#state{ftpes_serv_list = NewFtpesServList}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to start single listener on specified IP interface
%%% ----------------------------------------------------------
-spec start_listener(Serv :: #ftpes_serv{}, State :: #state{}) -> #ftpes_serv{}.
%% No address case - ftpesd cannot be started
start_listener(#ftpes_serv{type = _Type, listener_pid = ListenerPid} = Serv, _State) when ListenerPid =/= undefined ->
    ?LOG("Unable to start listener on ~p interface - already started (this is a problem)~n", [_Type]),
    Serv;

%% If IP interface is enabled, start listener
start_listener(#ftpes_serv{type = Type, bind_address = Address, namespace = Ns} = Serv, State) when ?IP_IF_AVAIL(Type, Address, Ns)->
    do_start_listener(Serv, State);

start_listener(#ftpes_serv{type = _Type, bind_address = _Address, namespace = _Ns} = Serv, _State) ->
    ?LOG("Unable to start listener on ~p interface - Address: ~p; Namespace: ~p~n", [_Type, _Address, _Ns]),
    Serv.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to start single listener on specified IP interface
%%% ----------------------------------------------------------
-spec do_start_listener(Serv :: #ftpes_serv{}, State :: #state{}) -> #ftpes_serv{}.
do_start_listener(#ftpes_serv{type = Type} = Serv, State) ->
    case listener_args(Serv, State) of
        [] ->
            ?LOG("Unable to start listener on ~p interface - getting options failed~n", [Type]),
            Serv;
        ListenerArgs ->
            ?LOG("Starting listener on ~p interface~n", [Type]),
            {ok, ListenerPid} = ftpesd_sup:start_child(Type, ListenerArgs),
            Serv#ftpes_serv{listener_pid = ListenerPid}
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checking changes from CommonOptions
%%%      Need to check if new parameters are enabled
%%%      and taking appropriate action: Starting FTPES server,
%%%      restarting FTPES listener and stopping FTPES server
%%% ----------------------------------------------------------
-spec reconfig_server(State :: #state{},
                      Reason :: atom()) ->
          {noreply, State :: #state{}}.
%%% ###=====================================================================###
reconfig_server(State, Reason) ->
    reconfig_server(State#state.ftpes_serv_list, State, Reason).
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checking changes from CommonOptions
%%%      Need to check if new parameters are enabled
%%%      and taking appropriate action: Starting FTPES server,
%%%      restarting FTPES listener and stopping FTPES server
%%% ----------------------------------------------------------
-spec reconfig_server(FtpesServList :: [#ftpes_serv{}],
                      State :: #state{},
                      Reason :: atom()) ->
          {noreply, State :: #state{}}.
%%% ###=====================================================================###
reconfig_server(FtpesServList, State, Reason) ->
    ?LOG("FTPES server reconfiguration started - reason: ~p~n", [Reason]),
    
    NewFtpesServList =
        case is_tls_server_enabled(State#state.serv_options) of
            ok ->
                %% If TLS is available, reconfiguration is performed on passed FTPES servers
                reconfig_listeners(FtpesServList, State, Reason);
            _ ->
                %% If TLS is down, all server should be stopped
                shutdown_listeners(State)
        end,
    
    {noreply, update_listener_state(NewFtpesServList, State)}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to reconfig listeners
%%% ----------------------------------------------------------
-spec reconfig_listeners(FtpesServList :: [#ftpes_serv{}],
                         State :: #state{},
                         Reason :: atom()) ->
          [#ftpes_serv{}].
%%% ###=====================================================================###
reconfig_listeners(FtpesServList, State, Reason) ->
    [reconfig_listener(Serv, Reason, State) || Serv <- FtpesServList].

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to reconfig single listener with new parameters
%%% ----------------------------------------------------------
-spec reconfig_listener(Serv :: #ftpes_serv{}, 
                        Reason :: atom(), 
                        State :: #state{}) ->
          #ftpes_serv{}.
%%% ###=====================================================================###
%%case listener not started, try to start it
reconfig_listener(#ftpes_serv{listener_pid = undefined} = Serv, _Reason, State) ->
    start_listener(Serv, State);

%% If IP interface is enabled, reconfig listener
reconfig_listener(#ftpes_serv{type = Type, bind_address = Address, namespace = Ns} = Serv, Reason, State) 
  when ?IP_IF_AVAIL(Type, Address, Ns)->
    do_reconfig_listener(Serv, Reason, State);

%% If IP interface is disabled, shutdown listener
reconfig_listener(Serv, _Reason, State) ->
    shutdown_listener(Serv, State).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to reconfig single listener with new parameters
%%% ----------------------------------------------------------
-spec do_reconfig_listener(Serv :: #ftpes_serv{}, 
                           Reason :: atom(), 
                           State :: #state{}) ->
          #ftpes_serv{}.
%%% ###=====================================================================###
do_reconfig_listener(#ftpes_serv{type = _Type, listener_pid = ListenerPid} = Serv, Reason, State) ->
    
    case listener_args(Serv, State) of
        [] ->
            ?LOG("Unable to reconfigure listener on ~p interface - getting options failed~n", [_Type]),
            shutdown_listener(Serv, State);
        ListenerArgs ->
            ?LOG("Reconfigurating listener on ~p interface - Reason: ~p~n", [_Type, Reason]),
            ftpesd_listener:config_update(ListenerPid, ListenerArgs ++ [{type, Reason}]),
            Serv
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to shutdown listeners
%%% ----------------------------------------------------------
-spec shutdown_listeners(State :: #state{}) ->
          [#ftpes_serv{}].
%%% ###=====================================================================###
shutdown_listeners(State) ->
    shutdown_listeners(State#state.ftpes_serv_list, State).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to shutdown listeners
%%% ----------------------------------------------------------
-spec shutdown_listeners(FtpesServList :: [#ftpes_serv{}],
                         State :: #state{}) ->
          [#ftpes_serv{}].
%%% ###=====================================================================###
shutdown_listeners(FtpesServList, State) ->
    [shutdown_listener(Serv, State) || Serv <- FtpesServList].

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to shutdown single listener on specified IP interface
%%% ----------------------------------------------------------
-spec shutdown_listener(Serv :: #ftpes_serv{},
                        State :: #state{}) ->
          #ftpes_serv{}.
%%% ###=====================================================================###
shutdown_listener(#ftpes_serv{type = _Type, listener_pid = undefined} = Serv, _State) ->
    ?LOG("Listener on ~p interface not running~n", [_Type]),
    Serv;

shutdown_listener(#ftpes_serv{type = _Type, listener_pid = ListenerPid} = Serv, _State) ->
    ?LOG("Stopping listener on ~p interface~n", [_Type]),
    ftpesd_sup:stop_child(ListenerPid),
    Serv#ftpes_serv{listener_pid = undefined}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get common listener parameters
%%% ----------------------------------------------------------
-spec listener_args(Serv :: #ftpes_serv{}, State :: #state{}) ->
          Params :: [{atom(), any()}].
%%% ###=====================================================================###
listener_args(#ftpes_serv{type = Type}, #state{port = Port, serv_options = ServCommonOpts, idle_timer = IdleTimer,
                                               minDataPort = MinDataPort, maxDataPort = MaxDataPort}) ->
    NC = ServCommonOpts#common_serv_options.node_credential,
    TC = ServCommonOpts#common_serv_options.trust_category,
    case ftpesLib:setup_tls(NC, TC) of
        {ok, TlsOptions} ->
            [{port, Port}, {tls_options, TlsOptions}, {ip_options, ftpesLib:get_ip_options(Type)},
             {trusted_category, TC}, {idle_timer, IdleTimer}, {minDataPort, MinDataPort}, {maxDataPort, MaxDataPort}];
        _ ->
            []
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Initial start of ftpes server
%%% ----------------------------------------------------------
-spec do_start_ftpesd_server(State :: #state{}) -> {ok | nok, #state{}}.
do_start_ftpesd_server(State) ->
    %% check if OOT is ready
    case ootI:get_lmt_ipv4() of
        {error, oot_not_started} ->
            %% retry
            sysInitI:info_msg("~p: Initial starting failed.~nOOT is no ready - retry in 5 seconds~n", [?MODULE]),
            timer:apply_after(5000, ?MODULE, start_ftpesd_server, []),
            {retry, State};
        _ ->
            NewState = initialize_state(State),
            case is_tls_server_enabled(NewState#state.serv_options) of
                ok ->
                    sysInitI:info_msg("~p: Starting FTPES server.~n", [?MODULE]),
                    NewState2 = do_start_listeners(NewState),
                    {ok, NewState2};
                {nok, Parameter, Value}-> 
                    sysInitI:info_msg("~p: Initial starting failed.~n[~p = ~p]~n", [?MODULE, Parameter, Value]),
                    {nok, NewState}
            end
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Shutdown all listeners and unsubscribes from all events
%%% ----------------------------------------------------------
-spec do_stop_ftpesd_server(State :: #state{}) ->
          #state{}.
%%% ###=====================================================================###
do_stop_ftpesd_server(State) ->
    NewFtpesServList = shutdown_listeners(State),
    certI:unsubscribe(?MODULE),
    State#state{ftpes_serv_list = NewFtpesServList}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Updates state with new listener configurations
%%% ----------------------------------------------------------
-spec update_listener_state(FtpesServList :: [#ftpes_serv{}], State :: #state{}) ->
          #state{}.
%%% ###=====================================================================###
update_listener_state([#ftpes_serv{type = Type} = Serv | FtpesServList], State) ->
    NewFtpesServList = 
        lists:map(fun
                     (#ftpes_serv{type = OldType}) when OldType =:= Type -> Serv;
                     (OldServ) -> OldServ 
                  end,
                  State#state.ftpes_serv_list),
    update_listener_state(FtpesServList, State#state{ftpes_serv_list = NewFtpesServList});

update_listener_state([], State) ->
    State.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Compares 2 server configs and returns difference
%%% ----------------------------------------------------------

compare_ip_configs(NewServerConfig, OldServerConfig) ->
    %% take new ip config and existing listener_pid
    [NewServ#ftpes_serv{listener_pid = OldServ#ftpes_serv.listener_pid} 
     || NewServ <- NewServerConfig, OldServ <- OldServerConfig,
        NewServ#ftpes_serv.type =:= OldServ#ftpes_serv.type,
        NewServ#ftpes_serv.bind_address =/= OldServ#ftpes_serv.bind_address
        orelse 
        NewServ#ftpes_serv.namespace =/= OldServ#ftpes_serv.namespace
].


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checking if parameters from CommonOptions are valid
%%%      If they are valid, FTPES server can be started
%%% ----------------------------------------------------------
-spec is_tls_server_enabled(CommonOptions :: #common_serv_options{}) ->
          ok | {nok, Attribute :: atom(), Value :: any()}.
%%% ###=====================================================================###
is_tls_server_enabled(#common_serv_options{node_credential = Nc, 
                                           trust_category = Tc, 
                                           administrative_state = As} = _CommonOptions) ->
    ?LOG("Checking server options: ~p~n", [_CommonOptions]),
    
    if         
        As == ?BasicAdmState_LOCKED -> {nok, administrative_state, 'LOCKED'};
        Nc == undefined -> {nok, node_credential, undefined};
        Tc == undefined -> {nok, trust_category, undefined};
        true -> ok
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns a list of initial FTPES server configurations
%%% ----------------------------------------------------------
-spec get_ftpes_servers_config() -> [#ftpes_serv{}].
get_ftpes_servers_config() ->
    [get_ftpes_server_config(Type) || Type <- ftpesLib:get_ip_interfaces()].

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns a list of updated FTPES server configurations
%%% ----------------------------------------------------------
-spec get_ftpes_servers_config(Config :: [property()], 
                               ServConfigList:: [#ftpes_serv{}]) -> [#ftpes_serv{}].
get_ftpes_servers_config(ConfigUpdate, OldServerConfig) ->
    [get_ftpes_server_config(Type, ConfigUpdate, OldServerConfig) || Type <- ftpesLib:get_ip_interfaces()].


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns initial FTPES server IP configuration
%%% ----------------------------------------------------------
-spec get_ftpes_server_config(Type :: ip_interface()) -> #ftpes_serv{}.
get_ftpes_server_config(Type) ->
    #ftpes_serv{type = Type,
                bind_address = ftpesLib:get_ip_address(Type),
                namespace = ftpesLib:get_namespace(Type)}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns updated FTPES server IP configuration
%%% ----------------------------------------------------------
-spec get_ftpes_server_config(Type :: ip_interface(),
                              Config :: [property()], 
                              ServConfigList:: [#ftpes_serv{}]) -> #ftpes_serv{}.
get_ftpes_server_config(Type, Config, ServConfigList) ->
    {IpAddress, Ns} = get_ip_address_ns(Type, Config, ServConfigList),
    #ftpes_serv{type = Type,
                bind_address = IpAddress,
                namespace = Ns}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns address of specified IP interface from config update or old config
%%% ----------------------------------------------------------
%% 
%% -spec get_ip_address(Type :: ip_interface(), Config :: [#ftpes_serv{}]) -> 
%%                         tuple() | string() | undefined;
%%                     (Type :: ip_interface(), Config :: [property()]) -> 
%%                         string() | no_change.
%% if list is ftpes_serv record list, it will always return result before empty
get_ip_address(Type, [#ftpes_serv{type = Type, bind_address = IpAddress} | _]) ->
    IpAddress;
get_ip_address(Type, [#ftpes_serv{type = _OtherType} | Rest]) ->
    get_ip_address(Type, Rest);

%% if list is proplist of changes
get_ip_address(TargetIf, Config) when ?IS_TARGET_IF(TargetIf) ->
    case proplists:get_value(ftpesLib:get_address_tag(TargetIf), 
                             Config, no_change) of
        [] ->
            undefined;
        Other ->
            Other
    end;
get_ip_address(SimIf, _Config) when ?IS_SIM_IF(SimIf) ->
    %% not possible to get SIM updates
    no_change.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns address and NS of specified IP interface from config update or old config
%%% ----------------------------------------------------------
-spec get_ip_address_ns(Type :: ip_interface(), 
                        Config :: [property()], 
                        ServConfigList:: [#ftpes_serv{}]) -> 
          {tuple() | string() | undefined, binary()}.
get_ip_address_ns(Type, Config, ServConfigList) ->
    %% first check changes
    case get_ip_address(Type, Config) of
        %% if no change use existing and check NS
        no_change ->
            {get_ip_address(Type, ServConfigList), get_namespace(Type, Config, ServConfigList)};
        %% if change in address, check for NS change or else NS resolution
        NewAddress ->
            {NewAddress,
             case get_namespace(Type, Config) of
                 no_change ->
                     ftpesLib:get_namespace(Type);
                 NewNs ->
                     NewNs
             end}
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns namespace of specified IP interface from config update or old config
%%% ----------------------------------------------------------
%% 
%% -spec get_namespace(Type :: ip_interface(), Config :: [#ftpes_serv{}]) -> 
%%                         binary();
%%                    (Type :: ip_interface(), Config :: [property()]) -> 
%%                         binary() | no_change.
%% if list is ftpes_serv record list, it will always return result before empty
get_namespace(Type, [#ftpes_serv{type = Type, namespace = Ns} | _]) ->
    Ns;
get_namespace(Type, [#ftpes_serv{type = _OtherType} | Rest]) ->
    get_namespace(Type, Rest);

%% if list is proplist of changes
get_namespace(lmt, _Config) ->
    %% not possible to get LMT updates currently
    no_change;
get_namespace(oam, Config) ->
    proplists:get_value(oap_namespace, Config, no_change);
get_namespace(alt_oam, Config) ->
    proplists:get_value(oap_alt_namespace, Config, no_change);
get_namespace(sim_ipv4, _Config) ->
    %% not possible to get SIM updates
    no_change;
get_namespace(sim_ipv6, _Config) ->
    %% not possible to get SIM updates
    no_change.


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns namespace of specified IP interface from config update or old config
%%% ----------------------------------------------------------
-spec get_namespace(Type :: ip_interface(), 
                    Config :: [property()], 
                    ServConfigList:: [#ftpes_serv{}]) -> 
          binary().
get_namespace(Type, Config, ServConfigList) ->
    %% first check changes
    case get_namespace(Type, Config) of
        %% if no change use existing
        no_change ->
            get_namespace(Type, ServConfigList);
        NewNs ->
            NewNs
    end.

-spec get_ftpes_port_conf() -> [tuple()].
get_ftpes_port_conf() ->
    [{server_control_port, ?DEFAULT_SERVER_CONTROL_CONNECTION_PORT}].


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


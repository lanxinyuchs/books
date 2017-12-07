%%%-------------------------------------------------------------------
%%% @private
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @end
%%% Created :  7 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_bert_server).

%% API
-export([start_link/0, cast_to_com/3]).

%% gen_server callbacks
-export([init/1, handle_connection/2, handle_info/2,
	 terminate/2]).

%% Types
-type reason() :: normal | shutdown | term().
-record(state, { gen_tcpd :: pid() }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    {ok,Port} = comte:get_env(comte_port),
    {ok,Ip} = comte:get_env(comte_ip),
    DefSockOpts = [{reuseaddr,true},
                   binary,
                   {packet,4},
                   {active,once}],

    SockOpts =
        extra_sock_opts([{fun parse_address/1, Ip}],
                        DefSockOpts),

       comte_gen_tcpd:start_link(
      ?MODULE, [], tcp, Port,
      [{socket_options, SockOpts}]).

extra_sock_opts([{Fun, Arg} | Opts], Acc) ->
    case Fun(Arg) of
        {ok, Opt} ->
            extra_sock_opts(Opts, [Opt|Acc]);
        false ->
            extra_sock_opts(Opts, Acc)
    end;
extra_sock_opts([], Acc) ->
    Acc.

parse_address("localhost") ->
    {ok, {ip, {127,0,0,1}}};
parse_address(Addr) when is_tuple(Addr),
                         size(Addr) == 4;
                         size(Addr) == 6 ->
    {ok, {ip, Addr}};
parse_address(AddrStr) ->
    case inet:parse_address(AddrStr) of
        {ok, Ip} ->
            {ok, {ip, Ip}};
        {error, _Reason} ->
            false
    end.

cast_to_com(M,F,A) ->
    ComTimeout = comte:get_env(comte_com_timeout, 5000),
    cast_to_com(M, F, A, ComTimeout).

cast_to_com(M, F, A, ComTimeout) ->
    {ok,ComPort} = comte:get_env(com_port),
    {ok,Host} = comte:get_env(com_ip),

    %%
    %% The combination of {linger,{true,0}} and gen_tcp:close(S)
    %% will make this side of the connection skip the TIME_WAIT
    %% state which otherwise will strike on the closing side.
    %% The connection will be abruptly reset instead of a full
    %% TCP close handshake.
    %%
    %% Since we have an application level ack due to this being an
    %% RPC - we receive an answer, there is no actual problem with just
    %% killing the connection after the answer has been received.
    %%
    %% The alternative would be to keep and manage a connection
    %% dedicated for downlink RPC, as well as one other for uplink.
    %%
    ConnectOpts = [binary,{packet,4},
                   {active,false},{linger,{true,0}}],
    case gen_tcp:connect(Host, ComPort, ConnectOpts) of
	{ok,S} ->
	    BertBin = comte_bert:encode({cast,M,F,A}),
	    ok = gen_tcp:send(S, BertBin),
	    case gen_tcp:recv(S, 0, ComTimeout) of
		{ok,Reply} ->
		    gen_tcp:close(S),
		    comte_bert:decode(Reply);
		RecvError ->
		    gen_tcp:close(S),
		    comte_error_logger:warning_report(
		      [{pid,self()},
		       {registered_name,?MODULE},
		       {function,{cast_to_com,M,F,A}},
		       {msg,"gen_tcp:recv/3 error"},
		       RecvError]),
		    RecvError
	    end;
	ConnectError ->
	    comte_error_logger:warning_report(
	      [{pid,self()},
	       {registered_name,?MODULE},
	       {function,{cast_to_com,M,F,A}},
	       {msg,"gen_tcp:connect/3 error"},
	       ConnectError]),
	    ConnectError
    end.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
-spec init(Args :: []) -> {ok, State :: #state{}}.
init([]) ->

    %% Create callback table
    comte_registrar:init(),

    %% Default callback handlers
    Callbacks =
        [{access_mgmt, get_cb_env(access_mgmt,
                                  {comte_default_access_mgmt, []})},
         {replicated_list, get_cb_env(replicated_list,
                                      {comte_default_replicated_list, []})},
         {transaction_server, get_cb_env(transaction_server,
                                         {comte_default_transaction_server, []})},
         {crypto, get_cb_env(crypto)},
         {log_write, get_cb_env(log_write)},
         {cm_event_producer, get_cb_env(cm_event_producer,
                                        {comte_default_alarm_event_producer, []})},
         {pm_event_producer, get_cb_env(pm_event_producer,
                                        {comte_default_alarm_event_producer, []})},
         {alarm_producer, get_cb_env(alarm_producer,
                                     {comte_default_alarm_event_producer, []})},
         {oi_register, get_cb_env(oi_register,
                                  {comte_default_oi_register, []})},
         {pm, get_cb_env(pm, {comte_default_pm_handler, []})},
         {pm_gp, get_cb_env(pm_gp)}
        ],

    ok = register_init_callbacks(Callbacks),

    process_flag(trap_exit, true),
    {ok, #state{ gen_tcpd = self() }}.

register_init_callbacks([]) ->
    ok;
register_init_callbacks([{_Key, undefined} | Callbacks]) ->
    register_init_callbacks(Callbacks);
register_init_callbacks([{Key, {Mod, StartArgs}} | Callbacks]) ->
    ok = Mod:start(StartArgs),
    ok = comte:register_callback(Key, Mod),
    register_init_callbacks(Callbacks).


get_cb_env(Key) ->
    get_cb_env(Key, undefined).

get_cb_env(Key, Default) ->
    case comte:get_env(Key, Default) of
        Default ->
            Default;
        Callback when is_atom(Callback) ->
            {Callback, []};
        {Callback, StartArgs} when is_atom(Callback) ->
            {Callback, StartArgs}
    end.



handle_connection(Socket, State) ->
    unlink(State#state.gen_tcpd),
    erlang:monitor(process, State#state.gen_tcpd),
    read_data(Socket, State),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #state{}) ->
			 {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate.
%%--------------------------------------------------------------------
-spec terminate(Reason :: reason(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

read_data(Socket, State) ->
    comte_gen_tcpd:setopts(Socket, [{active,once}]),
    receive
	{'DOWN',_, process,Pid,Reason} when Pid == State#state.gen_tcpd ->
	    exit(Reason);
	{tcp,_Socket,Data} ->
            SAReply =
		try handle_rpc(comte_bert:decode(Data))
		catch E:R ->
			comte_error_logger:warning_report(
			  [{exception,E,R},
			   {stacktrace,erlang:get_stacktrace()},
			   {pid,self()},
			   {registered_name, ?MODULE},
			   {function,{read_data,Data}},
			   {msg,"handle_rpc error"}]),
			{reply,{error,comte_lib:com_error_code(failure)}}
		end,
            comte_gen_tcpd:send(Socket, comte_bert:encode(SAReply)),
            read_data(Socket, State);
	{tcp_closed,_Socket} ->
	    tcp_closed
    end.

handle_rpc({call,M,F,A}) ->
    {reply,apply(M,F,A)};
handle_rpc({cast,M,F,A}) ->
    spawn(fun() -> apply(M,F,A) end),
    {noreply}.

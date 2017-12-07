%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysDhcpd.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R5A/4
%%% @doc == DHCP daemon ==
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(sysDhcpd).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/4').
-date('2016-02-05').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% Rev     Date     Name      What
%%% -----   -------  --------  ------------------------
%%% R4A/1   150417   etxpeno   First try
%%% R4A/2   150504   etxpeno   Handle unrelated DHCPREQUEST
%%% R4A/3   15060x   etxlg     Changed something...
%%% ----------------------------------------------------------
%%% R5A/1   151117   etxpeno   move dead code
%%% R5A/2   151204   etxarnu   added reopen_if
%%% R5A/3   160122   etxpejn   HU51959: More robust handling of gen_udp:open
%%% R5A/4   160205   etxpeno   handle incorrect DHCP messages
%%% ----------------------------------------------------------

%% API
-export([start_link/0]).

-export([add_client/2, remove_client/1]).
-export([reopen_if/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DHCP_SUBNET_MASK, {255,255,255,0}).
-define(DHCP_BROADCAST_ADDRESS, {192,168,3,255}).
-define(DHCP_ROUTER, {192,168,3,1}).
-define(DHCP_CONTENT_FILE, <<"contents.txt">>).
-define(DHCP_OFFER_TIMEOUT, timer:minutes(1)).

-define(DHCP_SERVER_PORT, 67).
-define(DHCP_CLIENT_PORT, 68).

-define(BOOTREQUEST, 1).
-define(BOOTREPLY, 2).

%%% Magic cookie
-define(DHCP_OPTIONS_COOKIE, [99, 130, 83, 99]).

%%% Hardware type (htype) field
-define(HTYPE_ETHER, 1). %% Ethernet 10Mbps

%%% DHCP Option codes
-define(DHCP_OPTION_SUBNET_MASK,             1).
-define(DHCP_OPTION_ROUTERS,                 3).
-define(DHCP_OPTION_BROADCAST_ADDRESS,       28).
-define(DHCP_OPTION_DHCP_REQUESTED_ADDRESS,  50).
-define(DHCP_OPTION_DHCP_MESSAGE_TYPE,       53).
-define(DHCP_OPTION_DHCP_SERVER_IDENTIFIER,  54).
-define(DHCP_OPTION_PARAMETER_REQUEST_LIST,  55).
-define(DHCP_OPTION_DHCP_MESSAGE,            56).
-define(DHCP_OPTION_DHCP_MAX_MESSAGE_SIZE,   57).
-define(DHCP_OPTION_VENDOR_CLASS_IDENTIFIER, 60).
-define(DHCP_OPTION_DHCP_CLIENT_IDENTIFIER,  61).
-define(DHCP_OPTION_END,                    255).

%%% DHCP Message types
-define(DHCP_MSG_DISCOVER, 1).
-define(DHCP_MSG_OFFER,    2).
-define(DHCP_MSG_REQUEST,  3).
-define(DHCP_MSG_DECLINE,  4).
-define(DHCP_MSG_ACK,      5).
-define(DHCP_MSG_NAK,      6).
-define(DHCP_MSG_RELEASE,  7).
-define(DHCP_MSG_INFORM,   8).

-define(DHCP_STATUS_AVAILABLE, available).
-define(DHCP_STATUS_OFFERED,   offered).
-define(DHCP_STATUS_ALLOCATED, allocated).
-define(DHCP_STATUS_DECLINED,  declined).

-define(SO_BINDTODEVICE, 25).
-define(SOL_SOCKET, 1).

-record(state,
	{
	  socket,
	  clients = orddict:new()
	}).

-record(dhcp,
	{
	  op, %% Message op code
	  htype = ?HTYPE_ETHER, %% Hardware address type
	  hlen = 6, %% Hardware address length
	  hops = 0, %% Number of relay agent hops from client
	  xid = 0, %% Transaction ID
	  secs = 0, %% Seconds since client started looking
	  flags = 0, %% Flag bits
	  ciaddr = {0,0,0,0}, %% Client IP address (if already in use)
	  yiaddr = {0,0,0,0}, %% Client IP address
	  siaddr = {0,0,0,0}, %% IP address of next server to talk to
	  giaddr = {0,0,0,0}, %% DHCP relay agent IP address
	  chaddr = {0,0,0,0,0,0}, %% Client hardware address
	  sname = <<>>, %% Server name
	  file = <<>>, %% Boot filename
	  options = [] %% Optional parameters
	}).

-record(client,
	{
	  ip,
	  status,
	  timer
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_client(ClientId, AllocatedIp) ->
    sysInitI:info_report("~p:add_client(~p, ~p) called~n",
			 [?MODULE, ClientId, AllocatedIp]),
    try
	gen_server:call(?SERVER, {add_client, ClientId, AllocatedIp})
    catch
	exit:{noproc, {gen_server, call,
		       [?SERVER, {add_client, ClientId, AllocatedIp}]}} ->
	    noproc
    end.
remove_client(ClientId) ->
    sysInitI:info_report("~p:remove_client(~p) called~n",
			 [?MODULE, ClientId]),
    try
	gen_server:call(?SERVER, {remove_client, ClientId})
    catch
	exit:{noproc, {gen_server, call,
		       [?SERVER, {remove_client, ClientId}]}} ->
	    noproc
    end.

reopen_if() ->
    sysInitI:info_report("~p:reopen_if() called~n",
			 [?MODULE]),
    try
	gen_server:call(?SERVER, reopen_if)
    catch
	exit:{noproc, {gen_server, call,
		       [?SERVER, reopen_if]}} ->
	    noproc
    end.

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
    OwnIp = clhI:ip_address(),
    case os:cmd("ip address show |grep " ++ to_str(OwnIp)) of
	[] ->
	    ignore; % Own IP address not configured
	_ ->
	    case open_if(1)  of
		{ok, Socket} ->
		    sysInitI:info_msg("~p: Starting DHCP server~n", [?MODULE]),
		    Clients = orddict:new(),
		    {ok, #state{socket  = Socket,
				clients = Clients}};
		{error, Reason} ->
		    sysInitI:error_msg("~p: Cannot open udp port ~p~n",
				       [?MODULE, ?DHCP_SERVER_PORT]),
		    {stop, Reason}
	    end
    end.

open_if(NoOfTries) ->
    OwnIf = os:getenv("OWN_CLUSTER_IF"), % set in rcs_start
    Ifname = iolist_to_binary([OwnIf, 0]),
    Options = [binary, {broadcast, true},
	       {raw, ?SOL_SOCKET, ?SO_BINDTODEVICE, Ifname},
	       {reuseaddr, true}],
    open_if(NoOfTries, Options).

open_if(NoOfTries, Options) ->
    case gen_udp:open(?DHCP_SERVER_PORT, Options) of
	{error, eaddrinuse} when NoOfTries > 1 ->
	    %% Address is still in use, try open once more.
	    timer:sleep(100),
	    open_if(NoOfTries - 1, Options);
	OkOrOtherError ->
	    OkOrOtherError
    end.

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
handle_call({add_client, ClientId, AvailableIp}, _From, State) ->
    case orddict:find(ClientId, State#state.clients) of
	{ok, _} ->
	    Reply = {error, <<"Client is already registered">>},
	    {reply, Reply, State};
	error ->
	    Client = #client{ip     = AvailableIp,
			     status = ?DHCP_STATUS_AVAILABLE},
	    NewClients = orddict:store(ClientId, Client, State#state.clients),
	    Reply = ok,
	    {reply, Reply, State#state{clients = NewClients}}
    end;
handle_call({remove_client, ClientId}, _From, State) ->
    case orddict:find(ClientId, State#state.clients) of
	{ok, _} ->
	    NewClients = orddict:erase(ClientId, State#state.clients),
	    {reply, ok, State#state{clients = NewClients}};
	error ->
	    {reply, not_found, State}
    end;
handle_call(reopen_if, _From, State) ->
    gen_udp:close(State#state.socket),
    case open_if(3)  of
	{ok, Socket} ->
	    sysInitI:info_msg("~p: Restarting DHCP server~n", [?MODULE]),
	    {reply, ok,State#state{socket = Socket}};
	{error, Reason} ->
	    sysInitI:error_msg("~p: Cannot open udp port ~p~n",
			       [?MODULE, ?DHCP_SERVER_PORT]),
	    {stop, Reason, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_cast(_Msg, State) ->
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
handle_info({udp, Socket, _IP, _Port, Packet}, State) ->
    case decode(Packet) of
	undefined ->
	    sysInitI:warning_msg("~p: Could not decode DHCP message~n"
				 "Message: ~p~n", [?MODULE]),
	    {noreply, State};
	Dhcp ->
	    NewState =
		case option(?DHCP_OPTION_DHCP_MESSAGE_TYPE, Dhcp) of
		    {value, MsgType} ->
			handle_dhcp(MsgType, Dhcp, Socket, State);
		    undefined ->
			State
		end,
	    {noreply, NewState}
    end;
handle_info({expired, ClientId}, State) ->
    Clients = State#state.clients,
    case orddict:find(ClientId, Clients) of
	error ->
	    {noreply, State};
	{ok, Client} ->
	    NewClient = Client#client{status = ?DHCP_STATUS_AVAILABLE,
				      timer  = undefined},
	    NewClients = orddict:store(ClientId, NewClient, Clients),
	    NewState = State#state{clients = NewClients},
	    {noreply, NewState}
    end;
handle_info(_Info, State) ->
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
    gen_udp:close(State#state.socket),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_str({A,B,C,D}) ->
    io_lib:format("~p.~p.~p.~p",[A,B,C,D]).

get_client(ClientId, Clients) ->
    case orddict:find(ClientId, Clients) of
	error ->
	    undefined;
	{ok, Client} ->
	    Client
    end.

option(Option, D) ->
    case lists:keyfind(Option, 1, D#dhcp.options) of
	{Option, Value} ->
	    {value, Value};
	false ->
	    undefined
    end.

handle_dhcp(?DHCP_MSG_DISCOVER, Dhcp, Socket, State) ->
    ClientId = get_client_id(Dhcp),
    Clients = State#state.clients,
    Client = get_client(ClientId, Clients),

    case reserve_ip(Client) of
	{ok, IP, Options} ->
	    send_offer(Socket, Dhcp, IP, Options),
	    cancel_timer(Client#client.timer),
	    Timer = erlang:send_after(?DHCP_OFFER_TIMEOUT, ?SERVER,
				      {expired, ClientId}),
	    NewClient = Client#client{status = ?DHCP_STATUS_OFFERED,
				      timer  = Timer},
	    NewClients = orddict:store(ClientId, NewClient, Clients),
	    State#state{clients = NewClients};
	error ->
	    State
    end;
handle_dhcp(?DHCP_MSG_REQUEST, Dhcp, Socket, State) ->
    ClientId = get_client_id(Dhcp),
    Clients = State#state.clients,
    Client = get_client(ClientId, Clients),
    DhcpServerIdentifier = clhI:ip_address(),

    case client_state(Dhcp) of
	{selecting, DhcpServerIdentifier} ->
	    case allocate_ip(Client) of
		{ok, IP, Options} ->
		    cancel_timer(Client#client.timer),
		    NewClient = Client#client{status = ?DHCP_STATUS_ALLOCATED,
					      timer  = undefined},
		    NewClients = orddict:store(ClientId, NewClient, Clients),
		    NewState = State#state{clients = NewClients},
		    send_ack(Socket, Dhcp, IP, Options),
		    NewState;
		error ->
		    State
	    end;
	{selecting, _} ->
	    %% Do nothing
	    State;
	{init_reboot, ReqIP} ->
	    case verify_ip(ReqIP, Client) of
		{ok, IP, Options} ->
		    cancel_timer(Client#client.timer),
		    NewClient = Client#client{status = ?DHCP_STATUS_ALLOCATED,
					      timer  = undefined},
		    NewClients = orddict:store(ClientId, NewClient, Clients),
		    NewState = State#state{clients = NewClients},
		    send_ack(Socket, Dhcp, IP, Options),
		    NewState;
		{error, Reason} ->
		    send_nak(Socket, Dhcp, Reason),
		    State;
		no_client ->
		    State
	    end;
	{_, ReqIP} ->
	    case extend_ip(ReqIP, Client) of
		{ok, IP, Options} ->
		    send_ack(Socket, Dhcp, IP, Options);
		{error, Reason} ->
		    send_nak(Socket, Dhcp, Reason);
		no_client ->
		    ok
	    end,
	    State
    end;
handle_dhcp(?DHCP_MSG_DECLINE, Dhcp, _Socket, State) ->
    IP = get_requested_ip(Dhcp),
    ClientId = get_client_id(Dhcp),
    Clients = State#state.clients,

    case get_client(ClientId, Clients) of
	#client{ip     = IP,
		status = ?DHCP_STATUS_ALLOCATED} = Client ->
	    cancel_timer(Client#client.timer),
	    NewClient = Client#client{status = ?DHCP_STATUS_DECLINED,
				      timer  = undefined},
	    NewClients = orddict:store(ClientId, NewClient, Clients),
	    State#state{clients = NewClients};
	_ ->
	    State
    end;
handle_dhcp(?DHCP_MSG_RELEASE, Dhcp, _Socket, State) ->
    IP = Dhcp#dhcp.ciaddr,
    ClientId = get_client_id(Dhcp),
    Clients = State#state.clients,

    case get_client(ClientId, Clients) of
	#client{ip     = IP,
		status = ?DHCP_STATUS_ALLOCATED} = Client->
	    cancel_timer(Client#client.timer),
	    NewClient = Client#client{status = ?DHCP_STATUS_AVAILABLE,
				      timer  = undefined},
	    NewClients = orddict:store(ClientId, NewClient, Clients),
	    State#state{clients = NewClients};
	_ ->
	    State
    end;
handle_dhcp(?DHCP_MSG_INFORM, Dhcp, Socket, State) ->
    IP = Dhcp#dhcp.ciaddr,
    Options = get_options(),
    send_ack(Socket, Dhcp, IP, Options),
    State;
handle_dhcp(_MsgType, _Dhcp, _Socket, State) ->
    State.

reserve_ip(#client{ip     = ClientIP,
		   status = ?DHCP_STATUS_AVAILABLE}) ->
    Options = get_options(),
    {ok, ClientIP, Options};
reserve_ip(_Client) ->
    error.

allocate_ip(#client{ip     = ClientIP,
		    status = ?DHCP_STATUS_OFFERED}) ->
    Options = get_options(),
    {ok, ClientIP, Options};
allocate_ip(_Client) ->
    error.

verify_ip(ReqIP, #client{ip = ReqIP}) ->
    Options = get_options(),
    {ok, ReqIP, Options};
verify_ip(_, undefined) ->
    no_client;
verify_ip(_, #client{}) ->
    {error, "Wrong IP"}.

extend_ip(ReqIP, #client{ip     = ReqIP,
			 status = ?DHCP_STATUS_ALLOCATED}) ->
    Options = get_options(),
    {ok, ReqIP, Options};
extend_ip(_, undefined) ->
    no_client;
extend_ip(ReqIP, #client{ip = ReqIP}) ->
    {error, <<"IP not allocated">>};
extend_ip(_, #client{}) ->
    {error, <<"Wrong IP">>}.

client_state(Dhcp) ->
    case option(?DHCP_OPTION_DHCP_SERVER_IDENTIFIER, Dhcp) of
	{value, ServerId} ->
	    {selecting, ServerId};
	undefined ->
	    case option(?DHCP_OPTION_DHCP_REQUESTED_ADDRESS, Dhcp) of
		{value, RequestedIP} ->
		    {init_reboot, RequestedIP};
		undefined ->
		    case is_broadcast(Dhcp) of
			false ->
			    {renewing, Dhcp#dhcp.ciaddr};
			true ->
			    {rebinding, Dhcp#dhcp.ciaddr}
		    end
	    end
    end.

get_client_id(Dhcp) ->
    case option(?DHCP_OPTION_DHCP_CLIENT_IDENTIFIER, Dhcp) of
	{value, ClientId} ->
	    ClientId;
	undefined ->
	    undefined
    end.

get_requested_ip(Dhcp) ->
    case option(?DHCP_OPTION_DHCP_REQUESTED_ADDRESS, Dhcp) of
	{value, IP} ->
	    IP;
	undefined ->
	    {0,0,0,0}
    end.

get_options() ->
    [{?DHCP_OPTION_SUBNET_MASK, ?DHCP_SUBNET_MASK},
     {?DHCP_OPTION_BROADCAST_ADDRESS, ?DHCP_BROADCAST_ADDRESS},
     {?DHCP_OPTION_ROUTERS, [?DHCP_ROUTER]}].

decode(<<Op:8, Htype:8, Hlen:8, Hops:8, Xid:32, Secs:16, Flags:16,
	 Ciaddr:4/binary, Yiaddr:4/binary, Siaddr:4/binary, Giaddr:4/binary,
	 Chaddr:16/binary, Sname:64/binary, File:128/binary,
	 Options/binary>>) ->
    try
	OptsList = binary_to_options(Options),

	#dhcp{op      = Op,
	      htype   = Htype,
	      hlen    = Hlen,
	      hops    = Hops,
	      xid     = Xid,
	      secs    = Secs,
	      flags   = Flags,
	      ciaddr  = decode_binary(ip, Ciaddr),
	      yiaddr  = decode_binary(ip, Yiaddr),
	      siaddr  = decode_binary(ip, Siaddr),
	      giaddr  = decode_binary(ip, Giaddr),
	      chaddr  = decode_binary(eth, Chaddr),
	      sname   = Sname,
	      file    = File,
	      options = OptsList}
    catch throw:invalid_option ->
	    undefined
    end;
decode(_) ->
    undefined.

binary_to_options(<<99, 130, 83, 99, Opts/binary>>) ->
    binary_to_options(Opts, []);
binary_to_options(_) ->
    %% return empty list if the MAGIC is not there
    [].

binary_to_options(<<?DHCP_OPTION_END, _/binary>>, Acc) ->
    Acc;
binary_to_options(<<Tag, Data/binary>>, Acc) ->
    Type = type(Tag),
    {Rest, Value} = value(Type, Data),
    binary_to_options(Rest, [{Tag, Value} | Acc]);
binary_to_options(_, _) ->
    throw(invalid_option).

value(byte, <<1, Byte, Rest/binary>>) ->
    {Rest, Byte};
value(short, <<2, Short:16, Rest/binary>>) ->
    {Rest, Short};
%% value(integer, <<4, Integer:32, Rest/binary>>) ->
%%     {Rest, Integer};
value(string, <<N, String:N/binary, Rest/binary>>) ->
    {Rest, String};
value(ip, <<4, A, B, C, D, Rest/binary>>) ->
    {Rest, {A, B, C, D}};
value(iplist, <<N, Binary:N/binary, Rest/binary>>) ->
    {Rest, binary_to_iplist(Binary)};
value(unknown, <<N, Binary:N/binary, Rest/binary>>) ->
    {Rest, Binary};
value(_, _) ->
    throw(invalid_option).

decode_binary(ip, <<A, B, C, D>>) ->
    {A, B, C, D};
decode_binary(eth, <<A, B, C, D, E, F, _/binary>>) ->
    {A, B, C, D, E, F}.

binary_to_iplist(<<A, B, C, D, T/binary>>) ->
    [{A, B, C, D} | binary_to_iplist(T)];
binary_to_iplist(<<>>) ->
    [].

type(?DHCP_OPTION_SUBNET_MASK)             -> ip;
type(?DHCP_OPTION_ROUTERS)                 -> iplist;
type(?DHCP_OPTION_BROADCAST_ADDRESS)       -> ip;
type(?DHCP_OPTION_DHCP_REQUESTED_ADDRESS)  -> ip;
type(?DHCP_OPTION_DHCP_MESSAGE_TYPE)       -> byte;
type(?DHCP_OPTION_DHCP_SERVER_IDENTIFIER)  -> ip;
type(?DHCP_OPTION_PARAMETER_REQUEST_LIST)  -> string;
type(?DHCP_OPTION_DHCP_MESSAGE)            -> string;
type(?DHCP_OPTION_DHCP_MAX_MESSAGE_SIZE)   -> short;
type(?DHCP_OPTION_VENDOR_CLASS_IDENTIFIER) -> string;
type(?DHCP_OPTION_DHCP_CLIENT_IDENTIFIER)  -> string;
type(_)                                    -> unknown.

get_client_ip(#dhcp{giaddr = {0, 0, 0, 0},
		    ciaddr = {0, 0, 0, 0}} = Dhcp) ->
    case is_broadcast(Dhcp) of
	true -> {255, 255, 255, 255};
	false -> Dhcp#dhcp.yiaddr
    end;
get_client_ip(#dhcp{giaddr = {0, 0, 0, 0},
		    ciaddr = CiAddr}) ->
    CiAddr;
get_client_ip(#dhcp{giaddr = GiAddr}) ->
    GiAddr.

get_udp_port(#dhcp{giaddr = {0, 0, 0, 0}}) -> ?DHCP_CLIENT_PORT;
get_udp_port(#dhcp{}) -> ?DHCP_SERVER_PORT.

is_broadcast(Dhcp) ->
    (Dhcp#dhcp.flags bsr 15) == 1.

send_offer(Socket, Dhcp, IP, Options) ->
    DhcpServerIdentifier = clhI:ip_address(),
    Offer = Dhcp#dhcp{op = ?BOOTREPLY,
		      hops = 0,
		      secs = 0,
		      ciaddr = {0, 0, 0, 0},
		      yiaddr = IP,
		      siaddr = clhI:ip_address(),
		      file = ?DHCP_CONTENT_FILE,
		      options = [{?DHCP_OPTION_DHCP_MESSAGE_TYPE,
				  ?DHCP_MSG_OFFER},
				 {?DHCP_OPTION_DHCP_SERVER_IDENTIFIER,
				  DhcpServerIdentifier} |
				 Options]},

    ClientIP = get_client_ip(Offer),
    UdpPort = get_udp_port(Offer),
    sysInitI:info_msg("~p: Sending DHCPOFFER~n"
		      "IP: ~p~n"
		      "Port: ~p~n"
		      "Offer: ~p~n", [?MODULE, ClientIP, UdpPort, Offer]),
    gen_udp:send(Socket, ClientIP, UdpPort, encode(Offer)).

send_ack(Socket, Dhcp, IP, Options) ->
    DhcpServerIdentifier = clhI:ip_address(),
    DHCPAck = Dhcp#dhcp{op = ?BOOTREPLY,
			hops = 0,
			secs = 0,
			yiaddr = IP,
			siaddr = clhI:ip_address(),
			file = ?DHCP_CONTENT_FILE,
			options = [{?DHCP_OPTION_DHCP_MESSAGE_TYPE,
				    ?DHCP_MSG_ACK},
				   {?DHCP_OPTION_DHCP_SERVER_IDENTIFIER,
				    DhcpServerIdentifier} |
				   Options]},
    ClientIP = get_client_ip(DHCPAck),
    UdpPort = get_udp_port(DHCPAck),
    sysInitI:info_msg("~p: Sending DHCPACK~n"
		      "IP: ~p~n"
		      "Port: ~p~n"
		      "DHCPAck: ~p~n",
		      [?MODULE, ClientIP, UdpPort, DHCPAck]),
    gen_udp:send(Socket, ClientIP, UdpPort, encode(DHCPAck)).

send_nak(Socket, Dhcp, Reason) ->
    DhcpServerIdentifier = clhI:ip_address(),
    DHCPNak = Dhcp#dhcp{
		op = ?BOOTREPLY,
		hops = 0,
		secs = 0,
		ciaddr = {0, 0, 0, 0},
		yiaddr = {0, 0, 0, 0},
		siaddr = {0, 0, 0, 0},
		flags = Dhcp#dhcp.flags bor 16#8000, %% set broadcast bit
		options = [{?DHCP_OPTION_DHCP_MESSAGE_TYPE, ?DHCP_MSG_NAK},
			   {?DHCP_OPTION_DHCP_SERVER_IDENTIFIER,
			    DhcpServerIdentifier},
			   {?DHCP_OPTION_DHCP_MESSAGE, Reason}]},
    ClientIP = get_client_ip(Dhcp),
    UdpPort = get_udp_port(Dhcp),
    sysInitI:info_msg("~p: Sending DHCPNAK~n"
		      "IP: ~p~n"
		      "Port: ~p~n"
		      "DHCPNak: ~p~n",
		      [?MODULE, ClientIP, UdpPort, DHCPNak]),
    gen_udp:send(Socket, ClientIP, UdpPort, encode(DHCPNak)).

encode(Dhcp) ->
    Op = Dhcp#dhcp.op,
    Htype = Dhcp#dhcp.htype,
    Hlen = Dhcp#dhcp.hlen,
    Hops = Dhcp#dhcp.hops,
    Xid = Dhcp#dhcp.xid,
    Secs = Dhcp#dhcp.secs,
    Flags = Dhcp#dhcp.flags,
    Ciaddr = encode_binary(ip, Dhcp#dhcp.ciaddr),
    Yiaddr = encode_binary(ip, Dhcp#dhcp.yiaddr),
    Siaddr = encode_binary(ip, Dhcp#dhcp.siaddr),
    Giaddr = encode_binary(ip, Dhcp#dhcp.giaddr),
    Chaddr = encode_binary(eth, Dhcp#dhcp.chaddr),
    Sname = encode_binary(64, Dhcp#dhcp.sname),
    File = encode_binary(128, Dhcp#dhcp.file),
    Opts = encode_binary(options, Dhcp#dhcp.options),
    <<Op:8, Htype:8, Hlen:8, Hops:8, Xid:32, Secs:16, Flags:16,
      Ciaddr/binary, Yiaddr/binary, Siaddr/binary, Giaddr/binary,
      Chaddr/binary, Sname/binary, File/binary, Opts/binary>>.

encode_binary(ip, {A, B, C, D}) ->
    <<A, B, C, D>>;
encode_binary(eth, {A, B, C, D, E, F}) ->
    <<A, B, C, D, E, F, 0:10/integer-unit:8>>;
encode_binary(Size, B) when is_integer(Size),
			    is_binary(B) ->
    Len = byte_size(B),
    <<B/binary, 0:(Size - Len)/integer-unit:8>>;
encode_binary(options, Options) ->
    L = [<<(option_to_binary(Tag, Val))/binary>> || {Tag, Val} <- Options],
    list_to_binary(?DHCP_OPTIONS_COOKIE ++ L ++ [?DHCP_OPTION_END]).

option_to_binary(Tag, Val) ->
    case type(Tag) of
	byte ->
	    <<Tag, 1, Val>>;
	short ->
	    <<Tag, 2, Val:16/big>>;
	%% integer ->
	%%     <<Tag, 4, Val:32/big>>;
	string ->
	    <<Tag, (size(Val)), Val/binary>>;
	ip ->
	    <<Tag, 4, (encode_binary(ip, Val))/binary>>;
	iplist ->
	    B = list_to_binary([encode_binary(ip, IP) || IP <- Val]),
	    <<Tag, (size(B)), B/binary>>
    end.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> timer:cancel(Ref).

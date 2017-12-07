%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logStreamSendServer.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R9A/R10A/R11A/6
%%%
%%% @doc ==Syslog send server module for LOG==

-module(logStreamSendServer).
-vsn('/main/R3A/R4A/R5A/R6A/R9A/R10A/R11A/6').
-date('2017-10-23').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% Rev       Date        Name        What
%%% -----     ----------  --------    ------------------------
%%% R3A/1     2015-01-07  etxasta     Created
%%% R6A/1     2016-07-07  etomist     HU93672, disabled TLS session caching
%%% R6A/2     2016-08-12  ekurnik     HV17184, fixed formatting of IETF
%%%                                   messages over TLS
%%% R6A/3     2016-08-17  ekurnik     Reverting HV17184
%%% R6A/4     2016-08-18  ekurnik     HV18070, added support for 
%%%                                   IPv6 addresses in syslog URI
%%% R6A/5     2016-09-14  emariad     CSUC feature, cipher configuration
%%% R10A/1-6  2017-06-21  uabesvi     Create UDP socket at start of process
%%% R11A/1-2  2017-08-29  uabesvi     Comments
%%% R11A/5   2017-10-10  etomist     HW34290
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% Called from supervisor
-export([start/5]).

%%% Called from logStreamServer
-export([make_child/5, make_process_name/1]).

%%% Action handling
-export([send/2]).
-export([try_send/2]).
-export([disconnect/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).


-include_lib("public_key/include/public_key.hrl").
-include("RcsLogM.hrl").

-define(RAM_SIZE, 300).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Send message to syslog server
%%% @end
%%% ----------------------------------------------------------
send(Index, Msg) ->
    Name = make_process_name(Index),
    gen_server:cast(Name, {send, Msg}).

%%% ----------------------------------------------------------
%%% @doc Try to send queued messages to syslog server
%%% @end
%%% ----------------------------------------------------------
try_send(Pid, Msg) ->
    try 
	gen_server:call(Pid, {send, Msg})
    catch exit:{timeout, _} ->
	    {error, timeout}
    end.

%%% ----------------------------------------------------------
%%% @doc Disconnect tls connection to syslog server
%%% @end
%%% ----------------------------------------------------------
disconnect(Index) ->
    Name = make_process_name(Index),
    gen_server:call(Name, disconnect).


%%% ----------------------------------------------------------
%%% @doc Starts the logStreamSendServer server process
%%% @end
%%% ----------------------------------------------------------
start(Index, Type, Ip, Port, CN) ->
    ServerName = {local, make_process_name(Index)},
    Module = ?MODULE,
    Args = [Index, Type, Ip, Port, CN],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Makes child specs for a specific log push transfer id
%%% @end
%%% ----------------------------------------------------------
make_child(Index, Type, Ip, Port, CN) ->
    Id = make_process_name(Index),
    StartFunc = {logStreamSendServer, start, [Index, Type, Ip, Port, CN]},
    {Id, StartFunc, permanent, 3000, worker, [?MODULE]}.

%%% ----------------------------------------------------------
%%% @doc Generates a process name from the nodeCertficateId value
%%% @end
%%% ----------------------------------------------------------
make_process_name(Index) ->
    list_to_atom("logStreamSendServer."++Index).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
-record(state, {name,
		index,
		ram_pid,
		actionId = 1,
	        socket_udp}).

init(Args) ->
    [Index, Type, Ip, Port, CN] = Args,
    process_flag(trap_exit, true),
    put(index,    Index),
    put(type,     Type),
    put(ip,       Ip),
    put(port,     Port),
    put(cn,       CN),     %% Current Name
    put(tls_send, false),
    ssl:start(),
    start_oot_subscribe(),
    Name    = make_process_name(Index),
    RamName = list_to_atom("logStreamSendServer." ++ Index ++ ".Ram"),

    Self = self(),
    RamServerName = {local, RamName},
    RamModule = logStreamRam,
    RamArgs   = [RamName, Self, ?RAM_SIZE],
    Options   = [],
    init_2(gen_server:start_link(RamServerName, RamModule, RamArgs, Options),
	   #state{name       = Name,
		  index      = Index,
		  socket_udp = init_udp_socket()}).

init_2({ok, RamPid}, State) ->
%%     io:format("####### RamPid = ~p~n", [RamPid]),
%%     logStreamRam:write_udp(RamPid, ".....23 dafdafda ~n"),
%%     logStreamRam:write_udp(RamPid, ".....24 dafdafda ~n"),
%%     logStreamRam:write_udp(RamPid, ".....25 dafdafda ~n"),
%%     logStreamRam:write_tls(RamPid, ".....26 dafdafda ~n"),
    {ok, State#state{ram_pid = RamPid}};
init_2(_, State) ->
    {ok, State}.



start_oot_subscribe() ->
    Pid = self(),
    Fun = fun(Not) -> gen_server:cast(Pid, {oam_notify, Not}) end,
    sos(ootI:register_cfg_upd_cb(Fun)).

sos(ok) ->
    info_msg(?LINE, "Subscribing to OOT callbacks.~n", []),
    ok;
sos(_Error) ->
    timer:send_after(200, start_oot_subscribe).

init_udp_socket() ->
    Opts = [binary, {active, true}] ++ ootI:get_all_oam_opt_list(),
    case gen_udp:open(0, Opts) of
	{ok, UdpSocket} ->
	    info_msg(?LINE, "Created UDP socket.~n", []),
	    UdpSocket;
	{error, _Reason} ->
	    timer:send_after(200, init_udp_socket),
	    undefined
    end.
   

handle_call({try_send, Msg}, 
	    _From, 
	    #state{socket_udp = UdpSocket,
		   ram_pid    = RamPid} = State) ->
    {Res, NewUdpSocket} = handle_send(get(type), UdpSocket, RamPid, Msg),
    {noreply, Res, State#state{socket_udp = NewUdpSocket}};
%%    {noreply, error, State#state{socket_udp = NewUdpSocket}};
handle_call(disconnect, _From, State) ->
    {reply, close_tls(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({send, Msg}, #state{socket_udp = UdpSocket,
			        ram_pid    = RamPid} = State) ->
    {_, NewUdpSocket} = handle_send(get(type), UdpSocket, RamPid, Msg),
    {noreply, State#state{socket_udp = NewUdpSocket}};
handle_cast({oam_notify, Msg}, #state{socket_udp = UdpSocket} = State) ->
    info_msg(?LINE, "oam_notify. Restarted UDP socket  ~p~n", [Msg]),
    close_socket(UdpSocket),
    {noreply, State#state{socket_udp = init_udp_socket()}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(start_oot_subscribe, State) ->
    start_oot_subscribe(),
    {noreply, State};
handle_info(init_udp_socket, State) ->
    {noreply, State#state{socket_udp = init_udp_socket()}};
handle_info({ssl_closed, Socket}, State) ->
    info_msg(?LINE, "ssl_closed: ~p~n", [Socket]),
    close_tls(),
    {noreply, State};
handle_info({user_from_cert, Result}, State) ->
    %%info_msg(?LINE, "user_from_cert: ~p~n", [Result]),
    handle_user_from_cert(Result),
    {noreply, State};
handle_info(tls_timeout, State) ->
    put(tls_timeout_ref, undefined),
    case get(tls_send) of
        false ->
            %%info_msg(?LINE, "tls connection timeout, close~n", []),
            close_tls();
        true ->
            %%info_msg(?LINE, "tls connection timeout, still used~n", []),
            new_timer()
    end,
    {noreply, State};
handle_info(_Request, State) ->
    %%info_msg(?LINE, "handle_info: ~p~n", [Request]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #state{socket_udp = Socket} = State) ->
    info_msg(?LINE, "terminate(~p, ~p)~n",[Reason, State]),
    close_socket(Socket),
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           handle_send(Socket, Msg)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
handle_send(syslog, UdpSocket, RamPid, Msg) ->
    %% HV18070, generic getaddr
    handle_send_udp(ootI:getaddr(get(ip)), UdpSocket, RamPid, Msg);
handle_send(syslogs, UdpSocket, RamPid, Msg) ->
    %% HV18070, generic getaddr
    handle_send_tls(get(tls_socket), RamPid, Msg),
    {ok, UdpSocket}.


%%===================================================================
%% Send syslog events in plaintext using UDP
%%===================================================================
handle_send_udp({ok, Ip}, Socket, _RamPid, Msg) ->
    case gen_udp:send(Socket, Ip, get(port), Msg) of
	ok ->
	    update_conn_state(ok),
	    {ok, Socket};
	%% Dirty patch until subscribe OAM updates is working
	{error, eafnosupport} ->
	    close_socket(Socket),
	    hs_udp(init_udp_socket(), Ip, Msg);
	{error, R} ->
	    update_conn_state(error),
	    error_msg("~n--> send_stream ~p: error, ~p~n", [Msg, R]),
	    {error, Socket}
    end;
handle_send_udp({error, Reason}, Socket, _RamPid, _Msg) ->
    info_msg(?LINE, "resolving ip: ~p failed: ~p~n", [get(ip), Reason]),
    {error, Socket}.


hs_udp(undefined, _Ip, Msg) ->
    error_msg("~n--> send_stream ~p: error, ~p~n", [Msg, reopen_socket]),
    {error, undefined};
hs_udp(Socket, Ip, Msg) ->
    case gen_udp:send(Socket, Ip, get(port), Msg) of
	ok ->
	    update_conn_state(ok),
	    {ok, Socket};
	{error, R} ->
	    update_conn_state(error),
	    error_msg("~n--> send_stream ~p: error, ~p~n", [Msg, R]),
	    {error, Socket}
    end.



%%===================================================================
%% Send syslog event via tls instead of udp
%% Keep the tls connection open for 10 minutes if nothing more is sent,
%% then close the connection. Meaning that the connection should be
%% re-used, when a new event arrives.
%% Ip   = get(ip),
%% Port = get(port),
%% CN   = get(cn),
%%===================================================================
handle_send_tls(undefined, _RamPid, Msg) ->
    %%info_msg(?LINE, "handle_send/4 --> "
    %% "Type: syslogs, Ip: ~p, Port: ~p Msg: ~p~n",
    %%    [Ip, Port, Msg]),
    case setup_tls() of
	{ok, _Socket} ->
	    put(message, Msg);
	{error, Reason} ->
	    update_conn_state(error),
	    {error, Reason}
    end;
handle_send_tls(Socket, _RamPid, Msg) ->
    case ssl:send(Socket, format_msg(Msg)) of
	ok ->
	    update_conn_state(ok),
	    put(tls_send, true),
	    %%info_msg(?LINE, "Sent to reused socket~n", []),
	    ok;
	{error, _Reason} ->
	    %%info_msg(?LINE, "Sent to reused socket, error ~p~n", [Reason]),
	    update_conn_state(error),
	    put(tls_socket, undefined),
	    ssl:close(Socket),
	    case setup_tls() of
		{ok, _NewSocket} ->
		    put(message, Msg);
		{error, Reason1} ->
		    {error, Reason1}
	    end
    end.



format_msg(Msg) ->
    L1   = lists:append(integer_to_list(length(Msg)), " "),
    L2   = lists:append(L1, Msg),
    list_to_binary(L2).


setup_tls() ->
    [Obj]  = mnesia:dirty_read(logM, {"1", "1", "1"}),
    NcDn   = Obj#logM.nodeCredential,
    TcatDn = Obj#logM.trustCategory,
    setup_tls_1(certI:get_cert(NcDn), TcatDn).

setup_tls_1({ok, Cert, {_Type, _Key} = Kt}, TcatDn) ->
    setup_tls_2({Cert, Kt}, certI:get_cert(TcatDn), TcatDn);
setup_tls_1({error, Reason}, _) ->
    info_msg(?LINE, "Node Credential missing, ~p~n", [Reason]),
    {error, Reason}.

setup_tls_2({[NcCert|NcChain], Kt}, {ok, TcCerts}, TcatDn) ->
    info_msg(?LINE, "Got TCAT~n", []),
    VerifyFun = certI:mk_verify_fun(TcatDn, self()),
    setup_tls_3(VerifyFun, {NcCert, Kt}, TcCerts++NcChain, TcatDn);
setup_tls_2(_, {error, Reason}, _) ->
    info_msg(?LINE, "Trusted Certificates missing, ~p~n", [Reason]),
    {error, Reason}.

setup_tls_3({error, Reason}, _, _, _) ->
    info_msg(?LINE, "Verify fun faulty, ~p~n", [Reason]);
setup_tls_3({ok, VerifyFun, PartialFun}, {NcCert, Kt}, TcCerts, _TcatDn) ->
    Options = [{cert,               NcCert},
               {key,                Kt},
               {cacerts,            TcCerts},
               {secure_renegotiate, true},
               {depth,              10},
               VerifyFun,
               PartialFun,
               {reuse_sessions,     false}, %%HU93672, set to false
               {ciphers,            comsaI:get_tls_cipher_suites()},
               {versions,           [tlsv1, 'tlsv1.2']},
               {server_name_indication, disable} %% HW34290, MITM protection disabled!
              ] ++ ootI:get_all_oam_opt_list(),
        %% HV18070, generic getaddr
        case ootI:getaddr(get(ip)) of
            {ok, Ip} ->
                case ssl:connect(Ip, get(port), Options, infinity) of
                    {error, Reason} ->
                        %%info_msg(?LINE, "TLS connect failed, ~p~n", [Reason]),
                        {error, Reason};
                    {ok, Socket} ->
                        %%info_msg(?LINE, "TLS connect ok, ~p~n", [Socket]),
                        put(tls_socket, Socket),
                        {ok, Socket}
                end;
            {error, Reason} ->
                info_msg(?LINE, 
                    "resolving ip: ~p failed: ~p~n", 
                    [get(ip), Reason]),
                    {error, Reason}
        end.

handle_user_from_cert({ok, User, SubjectNameString, crl_ok}) ->
    case get(cn) of
        {no_cn, undefined} ->
            info_msg(?LINE, 
		     "No CurrentName defined~n"
		     "  User = ~p~n"
		     "  Name = ~p~n",
		     [User, SubjectNameString]),
            case get(tls_socket) of
                undefined ->
                    ok; % Failed
                _Socket ->
                    send_saved_msg()
            end;
        User ->
            info_msg(?LINE, "CurrentName ~p matched~n", [User]),
            case get(tls_socket) of
                undefined ->
                    ok; % Failed
                _Socket ->
                    send_saved_msg()
            end;
        OtherUser ->
            info_msg(?LINE,
		     "Wanted user: ~p, dosen't matching peer user: ~p~n ",
		     [User, OtherUser]),
            %% Maybe sec log
            case get(tls_socket) of
                undefined ->
                    ok;
                Socket ->
                    put(tls_socket, undefined),
                    ssl:close(Socket)
            end
    end.


send_saved_msg() ->
    case get(message) of
        undefined ->
            ok;
        Msg ->
            put(message, undefined),
            case get(tls_socket) of
                undefined ->
                    ok;
                Socket ->
                    case ssl:send(Socket, format_msg(Msg)) of
                        ok ->
                            %%info_msg(?LINE, "Sent to new socket~n", []),
                            update_conn_state(ok),
                            new_timer(),
                            ok;
                        {error, Reason} ->
%%                             info_msg(?LINE,
%% 				     "Sent to new socket, err ~p~n",
%% 				     [Reason]),
                            update_conn_state(error),
                            put(tls_socket, undefined),
                            {error, Reason}
                    end
            end
    end.


new_timer() ->
    case get(tls_timeout_ref) of
        undefined ->
            ok;
        OldTRef ->
            erlang:cancel_timer(OldTRef)
    end,
    TRef = erlang:send_after(600000, self(), tls_timeout),
    put(tls_timeout_ref, TRef),
    put(tls_send, false).


close_tls() ->
    case get(tls_timeout_ref) of
        undefined ->
            ok;
        OldTRef ->
            erlang:cancel_timer(OldTRef),
            put(tls_timeout_ref, undefined)
    end,
    case get(tls_socket) of
        undefined ->
            put(tls_send, false),
            ok;
        Socket ->
            put(tls_socket, undefined),
            put(tls_send, false),
            ssl:close(Socket),
            Socket
    end.

%% State: ok|error
update_conn_state(Status) ->
    case get(send_status) of
        Status ->
            ok;
        _ ->
            put(send_status, Status),
            logStreamServer:update_state({get(index), get(type), Status})
    end.

close_socket(undefined) ->
    ok;
close_socket(UdpSocket) ->
    gen_udp:close(UdpSocket).


%%% ----------------------------------------------------------
%%% INFO, WARNING and ERROR MESSAGE HANDLING
%%% ----------------------------------------------------------
info_msg(Line, Format, Args) ->
   sysInitI:info_msg("~w:~p " ++ Format, [?MODULE, Line | Args]).

%warning_msg(Format) ->
%    warning_msg(Format, []).

%warning_msg(Format, Args) ->
%    error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).



%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

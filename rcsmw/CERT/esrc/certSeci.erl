%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certSeci.erl %
%%% @author ebabmat
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R10A/4

%%% @doc ==Distinguished name translation service==
%%% This module implements the SECI interface between CS and TN
%%% used by IPSec
%%% end

-module(certSeci).
-behaviour(gen_server).
-vsn('/main/R3A/R4A/R5A/R6A/R7A/R10A/4').
-date('2017-07-06').
-author('ebabmat').

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
%%% Rev     Date       Name    What
%%% -----   ---------- ------- -------------------------------
%%% R3A/1   2014-10-31 etxasta Created
%%% R4A/7   2015-08-20 etxpejn Added rpc:call for SecurityLog
%%% R4A/9   2015-09-25 etxpejn Moved rpc:call to logI:write_log
%%% R6A/3   2016-06-16 ehsake  Moved der_to_pem to certLib, 
%%%                            use new get_cert/2.
%%% R7A/1   2016-09-27 etxasta Added SECI_GET_VCERT,
%%%                            SECI_VERIFY_PEER_VC,
%%%                            SECI_ENCODE and SECI_DECODE
%%% R7A/5   2016-09-29 ehsake  Fixes for DU2DU authentication
%%% R10A/1  2017-05-12 emarnek HV85974
%%% R10A/2  2017-06-28 ebabmat HV97126
%%% R10A/3  2017-06-30 ebabmat reverting HV97126
%%% R10A/4  2017-07-06 ebabmat real fix for HV97126
%%% ----------------------------------------------------------

-export([start/0]).


-export([activate/0]).

-export([cec_setup/1]).

-export([event/2]).

-export([verify_peer_result/2]).

-export([warm/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("RcsCertM.hrl").
-include_lib("public_key.hrl").

%% Refer to csrc/cert_seci.h
-define(SECI_OK,                        0).
-define(SECI_NOT_FOUND,                 1).
-define(SECI_ERROR,                     2).
-define(SECI_INVALID_PARAMETER_RESULTP, 3).
-define(SECI_SEND_ERROR,                4).
-define(SECI_RECEIVE_ERROR,             5).
-define(SECI_DELETE_FAILED_WRONG_ID,    6).
-define(SECI_UNINITIALIZED_SUB,         7).
-define(SECI_VERIFY_PEER_VALID,         8).
-define(SECI_VERIFY_PEER_NOT_VALID,     9).
-define(SECI_VERIFY_PEER_UNKNOWN,      10).

-define(SECI_GET_CERT,        1).
-define(SECI_READ,            2).
-define(SECI_WRITE,           3).
-define(SECI_DELETE,          4).
-define(SECI_LOG,             5).
-define(SECI_SUB_INIT,        6).
-define(SECI_ADD_SUB,         7).
-define(SECI_DEL_SUB,         8).
-define(SECI_GET_SUB_EVENT,   9).
%% 10 is reserved for SECI_GET_VC, only exist in c-code
-define(SECI_GET_NC,         11).
-define(SECI_GET_TCAT,       12).
-define(SECI_VERIFY_PEER,    13).
-define(SECI_GET_VCERT,      14).
-define(SECI_VERIFY_PEER_VC, 15).
-define(SECI_ENCODE,         16).
-define(SECI_DECODE,         17).

-define(SECI_SECURITY_LOG,    0).
-define(SECI_AUDIT_TRAIL_LOG, 1).

-define(SECI_SEVERITY_EMERGANCY, 0).
-define(SECI_SEVERITY_ALERT,     1).
-define(SECI_SEVERITY_CRITICAL,  2).
-define(SECI_SEVERITY_ERROR,     3).
-define(SECI_SEVERITY_WARNING,   4).
-define(SECI_SEVERITY_NOTICE,    5).
-define(SECI_SEVERITY_INFO,      6).

-define(LogSeverity_EMERGENCY, 0).
-define(LogSeverity_ALERT, 1).
-define(LogSeverity_CRITICAL, 2).
-define(LogSeverity_ERROR, 3).
-define(LogSeverity_WARNING, 4).
-define(LogSeverity_NOTICE, 5).
-define(LogSeverity_INFO, 6).

-define(SECI_CRL_CHECK_ENABLED,  0).
-define(SECI_CRL_CHECK_DISABLED, 1).

-record(state, {
	cfg_upd_cb = [],
	cec_state,	%undefined | pending | connected | done
        sockets,
        socket
    }).


%%===========================================================================
%% gen_server start functions
%%===========================================================================
start() ->
    ok = cec:register(<<"SECI">>, certSeci),
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    appmI:register_warm_cb(?MODULE),
    gen_server:start_link(ServerName, Module, Args, Options).

init(_Opts) ->
    %%    erlang:process_flag(trap_exit, true),
    {ok, #state{sockets = []}}.

activate() ->
    gen_server:cast(?MODULE, activate).

cec_setup(Socket) ->
    try
	gen_server:call(?MODULE, {cec_setup, Socket}, 30000)
    catch exit:{timeout, _} = Reason ->
            sysInitI:error_report([{mfa, {?MODULE, cec_setup, []}},
				       {caught, {exit, Reason}},
				       {socket, Socket}
				      ]),
	    throw(Reason)
    end.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Reason, S) ->
    [gen_tcp:close(Socket) || Socket <- S#state.sockets],
    ok.

event(Id, MoRef) ->
    gen_server:cast(?MODULE, {event, Id, MoRef}),
    ok.

verify_peer_result(Result, Socket) ->
    gen_server:cast(?MODULE, {verify_peer_result, Result, Socket}),
    ok.

warm() ->
    gen_server:cast(?MODULE, cleanup_sockets).

%%===========================================================================
%% gen_server message functions
%%===========================================================================
%%---------------------------------------------------------------------
%% call
%%---------------------------------------------------------------------
handle_call({cec_setup, Socket}, _From, #state{sockets = Sockets} = S) ->
    %% HV85974 Clean all closed sockets from list
    UpdatedSockets = lists:filter(fun(N) -> case erlang:port_info(N) of
                                                undefined -> false;
                                                _ -> true
                                            end end, Sockets),
    info_msg("handle_call cec_setup, ~p~n",
        [[{socket, Socket},{sockets, [Socket | UpdatedSockets]}]]),
    {reply, self(), S#state{sockets = [Socket | UpdatedSockets]}};
handle_call({register_cfg_upd_cb, Fun, Pid}, _From, State) ->
    CBFuns = State#state.cfg_upd_cb,
    lists:keymember(Pid, 1, CBFuns) orelse erlang:monitor(process, Pid),
    NewCBFuns = lists:keystore(Pid, 1, CBFuns, {Pid, Fun}),
    {reply, ok, State#state{cfg_upd_cb = NewCBFuns}};
handle_call(Command, _From, S) ->
    {reply, Command, S}.

%%---------------------------------------------------------------------
%% cast
%%---------------------------------------------------------------------
handle_cast(stop, S) ->
    {stop, normal, S};
handle_cast(activate, S) ->
    {noreply, S};
handle_cast({event, Id, MoRef}, S) ->
    handle_event(Id, MoRef),
    {noreply, S};
handle_cast({verify_peer_result, Result, Socket}, S) ->
    handle_verify_peer_result(Result, Socket),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_cast(cleanup_sockets, S) ->
    [gen_tcp:close(Socket) || Socket <- S#state.sockets],
    SocketsCleared = lists:filter(fun(N) -> case erlang:port_info(N) of 
                                              undefined -> false;
                                              _ -> true
                                            end
                                  end, S#state.sockets),
    info_msg("Cleared sockets because of a warm restart! Sockets left: ~p~n", [SocketsCleared]),
    {noreply, S#state{sockets = SocketsCleared}};
handle_cast(_Msg, S) ->
    {noreply, S}.


%%---------------------------------------------------------------------
%% info
%%---------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Config update subscriber is down. Remove subscription.
    NewCBFuns = lists:keydelete(Pid, 1, State#state.cfg_upd_cb),
    sysInitI:warning_msg("~p: Subscriber ~p is DOWN ~p~n", 
			     [?MODULE, Pid, _Reason]), 
    {noreply, State#state{cfg_upd_cb = NewCBFuns}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_GET_CERT:4/native-unsigned-integer-unit:8,
	       _Size:4/native-unsigned-integer-unit:8, Dn/binary>>},
            S) ->
    DnString = string:strip(binary_to_list(Dn), right, 0), 
    info_msg("GET_CERT --> ~s~n", [DnString]),
    Res =
    case certI:get_cert(DnString) of
        {ok, [NodeCert|_Chain], {_,PrivKey}} ->
            NL = byte_size(NodeCert),
            KL = byte_size(PrivKey),
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<(NL):4/native-integer-unit:8>>, NodeCert,
                <<(KL):4/native-integer-unit:8>>, PrivKey,
                <<0:4/native-integer-unit:8>>];
        {ok, TcatList} ->
            List1 =
            lists:filtermap(
                fun(TC) ->
                        L = byte_size(TC),
                        {true, [<<(L):4/native-integer-unit:8>>, TC]}
                end, TcatList),
            case List1 of
                [] ->
                    [<<?SECI_OK:4/native-integer-unit:8>>,
                        <<0:4/native-integer-unit:8>>];
                _ ->
                    List2 =
                    lists:append([<<?SECI_OK:4/native-integer-unit:8>>],
                        lists:flatten(List1)),
                    lists:append(List2, [<<0:4/native-integer-unit:8>>])
            end;
        {ok, Cert, Key} ->
            NL = byte_size(Cert),
            KL = byte_size(Key),
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<(NL):4/native-integer-unit:8>>, Cert,
                <<(KL):4/native-integer-unit:8>>, Key,
                <<0:4/native-integer-unit:8>>];
        {error, not_found} ->
            info_msg("certI:get_cert -> ~p~n", [{error, not_found}]),
            [<<?SECI_NOT_FOUND:4/native-integer-unit:8>>];
        {error, Reason} ->
            info_msg("certI:get_cert -> ~p~n", [{error, Reason}]),
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_GET_VCERT:4/native-unsigned-integer-unit:8,_/binary>>},
           S) ->
    info_msg("GET_VCERT~n", []),
    Res =
    case certSecStore:get_vc() of
        {ok, Cert, _} ->
            NL = byte_size(Cert),
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<(NL):4/native-integer-unit:8>>, Cert,
                <<0:4/native-integer-unit:8>>];
        {error, not_found} ->
            info_msg("certI:get_cert -> ~p~n", [{error, not_found}]),
            [<<?SECI_NOT_FOUND:4/native-integer-unit:8>>];
        {error, Reason} ->
            info_msg("certI:get_cert -> ~p~n", [{error, Reason}]),
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_GET_NC:4/native-unsigned-integer-unit:8,
	       _Size:4/native-unsigned-integer-unit:8, Dn/binary>>},
            S) ->
    DnString = string:strip(binary_to_list(Dn), right, 0), 
    info_msg("GET_NC --> ~s~n", [DnString]),
    Res =
    case certI:get_cert(DnString,pem) of
        {ok,NodeCert,PrivKey} ->
            NL = byte_size(NodeCert),
            KL = byte_size(PrivKey),
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<(NL):4/native-integer-unit:8>>, NodeCert,
                <<(KL):4/native-integer-unit:8>>, PrivKey,
                <<0:4/native-integer-unit:8>>];
        {error, not_found} ->
            info_msg("certI:get_cert -> ~p~n", [{error, not_found}]),
            [<<?SECI_NOT_FOUND:4/native-integer-unit:8>>];
        {error, Reason} ->
            info_msg("certI:get_cert -> ~p~n", [{error, Reason}]),
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_GET_TCAT:4/native-unsigned-integer-unit:8,
	       _Size:4/native-unsigned-integer-unit:8, Dn/binary>>},
            S) ->
    DnString = string:strip(binary_to_list(Dn), right, 0), 
    info_msg("GET_TCAT --> ~s~n", [DnString]),
    Res =
    case certI:get_tcat_and_crlcheck(DnString) of
        {ok, TcatList, CrlCheck} ->
            List1 =
            lists:filtermap(
                fun(TC) ->
                        Entity = public_key:der_decode('Certificate', TC),
                        PemEntry =
                        public_key:pem_entry_encode('Certificate', Entity),
                        PemBin = public_key:pem_encode([PemEntry]),
                        L = byte_size(PemBin),
                        {true, [<<(L):4/native-integer-unit:8>>, PemBin]}
                end, TcatList),
            Crl =
            case CrlCheck of
                ?FeatureState_ACTIVATED ->
                    ?SECI_CRL_CHECK_ENABLED;
                ?FeatureState_DEACTIVATED ->
                    ?SECI_CRL_CHECK_DISABLED
            end,
            case List1 of
                [] ->
                    [<<?SECI_OK:4/native-integer-unit:8>>,
                        <<(Crl):4/native-integer-unit:8>>,
                        <<0:4/native-integer-unit:8>>];
                _ ->
                    List2 =
                    lists:append([<<?SECI_OK:4/native-integer-unit:8>>,
                        <<(Crl):4/native-integer-unit:8>>],
                    lists:flatten(List1)),
                    lists:append(List2, [<<0:4/native-integer-unit:8>>])
            end;
        {error, not_found} ->
            info_msg("certI:get_cert -> ~p~n", [{error, not_found}]),
            [<<?SECI_NOT_FOUND:4/native-integer-unit:8>>];
        {error, Reason} ->
            info_msg("certI:get_cert -> ~p~n", [{error, Reason}]),
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_READ:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, Id:Size1/binary,
               _Size2:4/native-unsigned-integer-unit:8, Index/binary>>},
            S) ->
    IdString = string:strip(binary_to_list(Id), right, 0), 
    IndexString = string:strip(binary_to_list(Index), right, 0), 
    info_msg("READ, Id: ~p, Index: ~p~n", [IdString, IndexString]),
    Res =
    case certSecStore:read(IdString, IndexString) of
        {ok, Bin} ->
            Len = byte_size(Bin),
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<(Len):4/native-integer-unit:8>>, Bin];
        {error, not_found} ->
            [<<?SECI_NOT_FOUND:4/native-integer-unit:8>>];
        _ ->
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_WRITE:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, Id:Size1/binary,
               Size2:4/native-unsigned-integer-unit:8, Index:Size2/binary,
               _Size3:4/native-unsigned-integer-unit:8, DataBin/binary>>},
            S) ->
    IdString = string:strip(binary_to_list(Id), right, 0), 
    IndexString = string:strip(binary_to_list(Index), right, 0), 
    info_msg("WRITE, Id: ~p, Index: ~p~n", [IdString, IndexString]),
    certSecStore:write(IdString, IndexString, DataBin),
    gen_tcp:send(Socket, [<<?SECI_OK:4/native-integer-unit:8>>]),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_DELETE:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, Id:Size1/binary,
               _Size2:4/native-unsigned-integer-unit:8, Index/binary>>},
            S) ->
    IdString = string:strip(binary_to_list(Id), right, 0), 
    IndexString = string:strip(binary_to_list(Index), right, 0), 
    info_msg("DELETE, Id: ~p, Index: ~p~n", [IdString, IndexString]),
    certSecStore:delete(IdString, IndexString),
    gen_tcp:send(Socket, [<<?SECI_OK:4/native-integer-unit:8>>]),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_LOG:4/native-unsigned-integer-unit:8,
	       Type:4/native-signed-integer-unit:8,
               Facility:4/native-signed-integer-unit:8,
	       Severity:4/native-signed-integer-unit:8,
               _Size2:4/native-unsigned-integer-unit:8, Message/binary>>},
            S) ->
    MessageString = string:strip(binary_to_list(Message), right, 0), 
    info_msg("LOG, Type:~p, Facility: ~p, Severity: ~p, Msg: ~p~n",
        [Type, Facility, Severity, MessageString]),
    LogType =
    case Type of
        ?SECI_SECURITY_LOG ->
            "SecurityLog";
        ?SECI_AUDIT_TRAIL_LOG ->
            "AuditTrailLog"
    end,
    List = comsaI:get_managed_element_data(),
    MeId =
    case lists:keyfind(networkManagedElementId, 1, List) of
        {networkManagedElementId, undefined} ->
            "ManagedElement=1";
        {networkManagedElementId, ME} ->
            "ManagedElement=" ++ ME
    end,
    Sve =
    case Severity of
        ?LogSeverity_EMERGENCY -> emergency;
        ?LogSeverity_ALERT     -> alert;
        ?LogSeverity_CRITICAL  -> critical;
        ?LogSeverity_ERROR     -> error;
        ?LogSeverity_WARNING   -> warning;
        ?LogSeverity_NOTICE    -> notice;
        ?LogSeverity_INFO      -> info
    end,
   logI:write_log(LogType, MeId, 
		  Facility, Sve, os:timestamp(), MessageString),
    Res = [<<?SECI_OK:4/native-integer-unit:8>>],
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_SUB_INIT:4/native-unsigned-integer-unit:8,
	       _:4/native-unsigned-integer-unit:8, Id/binary>>},
            S) ->
    IdString = string:strip(binary_to_list(Id), right, 0), 
    info_msg("Subscription initialization, id: ~p~n", [IdString]),
    put({sub_socket, IdString}, Socket),
    Res = [<<?SECI_OK:4/native-integer-unit:8>>],
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, true}, {nodelay, true}]),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_ADD_SUB:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, Id:Size1/binary,
               _Size2:4/native-unsigned-integer-unit:8, Dn/binary>>},
            S) ->
    IdString = string:strip(binary_to_list(Id), right, 0), 
    DnString = string:strip(binary_to_list(Dn), right, 0), 
    info_msg("Add subscription, Id: ~p, Dn: ~p~n", [IdString, DnString]),
    Res =
    case certSub:subscribe(list_to_binary(DnString), {clib, IdString}) of
        ok ->
            [<<?SECI_OK:4/native-integer-unit:8>>];
        _ ->
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    %% Just for test
    case IdString of
        "seci_test" ->
            certSub:trig(nc,{"1","1","1","1","1"});
        _ ->
            ok
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_DEL_SUB:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, Id:Size1/binary,
               _Size2:4/native-unsigned-integer-unit:8, Dn/binary>>},
            S) ->
    IdString = string:strip(binary_to_list(Id), right, 0), 
    DnString = string:strip(binary_to_list(Dn), right, 0), 
    info_msg("Delete subscription, Id: ~p, Dn: ~p~n", [IdString, DnString]),
    Res =
    case certSub:unsubscribe(list_to_binary(DnString), {clib, IdString}) of
        ok ->
            [<<?SECI_OK:4/native-integer-unit:8>>];
        _ ->
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_GET_SUB_EVENT:4/native-unsigned-integer-unit:8,
	       _Size:4/native-unsigned-integer-unit:8, Id/binary>>},
            S) ->
    IdString = string:strip(binary_to_list(Id), right, 0), 
    info_msg("Get subscription event --> Id: ~p~n", [IdString]),
    List =
    lists:filtermap(
        fun(Dn) ->
                L = byte_size(Dn),
                {true, [<<(L):4/native-integer-unit:8>>, Dn]}
        end, get_event(IdString)),
    Res =
    case List of
        [] ->
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<0:4/native-integer-unit:8>>];
        _ ->
            List1 =
            lists:append([<<?SECI_OK:4/native-integer-unit:8>>],
                lists:flatten(List)),
            lists:append(List1, [<<0:4/native-integer-unit:8>>])
    end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, true}, {nodelay, true}]),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_VERIFY_PEER:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, TcatDn:Size1/binary,
	       Size2:4/native-unsigned-integer-unit:8, PeerPem:Size2/binary,
               _/binary>>},
            S) ->
    TcatDnString = string:strip(binary_to_list(TcatDn), right, 0), 
    info_msg("VERIFY_PEER --> Pem binary: ~p~nTcat: ~s",
        [PeerPem, TcatDnString]),
    PeerDerList =
    case catch public_key:pem_decode(PeerPem) of
        {'EXIT', _} ->
            info_msg("Failed to decode pem binary", []),
            [];
        [] -> % Might be der format
            info_msg("Failed to decode pem binary, might be faulty format",[]),
            [];
        Pems -> % List of pem
            lists:filtermap(
                fun({'Certificate',DerCert,_}) ->
                        {true, DerCert};
                    (_) ->
                        false
                end, Pems)
    end,
    proc_lib:spawn_link(certI, verify_peer,
        [PeerDerList, TcatDnString, server, Socket]),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_VERIFY_PEER_VC:4/native-unsigned-integer-unit:8,
	       Size:4/native-unsigned-integer-unit:8, PeerVcPem:Size/binary,
               _/binary>>},
            S) ->
    info_msg("VERIFY_PEER_VC --> Pem binary: ~p~n", [PeerVcPem]),
    proc_lib:spawn_link(certVerify, verify_peer_vc, [PeerVcPem, Socket]),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_ENCODE:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, PeerVcPem:Size1/binary,
	       Size2:4/native-unsigned-integer-unit:8, Data:Size2/binary,
               _/binary>>},
            S) ->
    info_msg("ENCODE, PeerCert: ~p, Data: ~p~n", [PeerVcPem, Data]),
    Res =
    case certLib:encode_w_vc(PeerVcPem, Data) of
        {ok, Bin} ->
            Len = byte_size(Bin),
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<(Len):4/native-integer-unit:8>>, Bin];
        {error, _} ->
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    info_msg("send encoded data ~p ~n",[Res]),
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SECI_DECODE:4/native-unsigned-integer-unit:8,
	       Size1:4/native-unsigned-integer-unit:8, PeerVcPem:Size1/binary,
	       Size2:4/native-unsigned-integer-unit:8, Data:Size2/binary,
               _/binary>>},
            S) ->
    info_msg("DECODE, PeerCert: ~p, Data: ~p~n", [PeerVcPem, Data]),
    Res =
    case certLib:decode_w_vc(PeerVcPem, Data) of
        {ok, Bin} ->
            Len = byte_size(Bin),
            [<<?SECI_OK:4/native-integer-unit:8>>,
                <<(Len):4/native-integer-unit:8>>, Bin];
        {error, _} ->
            [<<?SECI_ERROR:4/native-integer-unit:8>>]
    end,
    info_msg("send decoded data ~p ~n",[Res]),
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S#state{sockets = lists:delete(Socket, S#state.sockets)}};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = S) ->
    %don't do anything much, expect a new cec_setup
    sysInitI:warning_msg("~p: CEC connection closed, expecting reconnect~n",
			     [?MODULE]),
    gen_tcp:close(Socket),
    {noreply, S#state{
            socket    = undefined,
            sockets   = lists:delete(Socket, S#state.sockets),
            cec_state = undefined}};
handle_info(_Info, S) ->
    %%info_msg("handle_info(~p, ~p) called ~n", [Info, S]),
    {noreply, S}.



handle_event(Id, MoRef) ->
    info_msg("Event, Id: ~p, MoRef: ~p~n", [Id, MoRef]),
    case get({event, Id}) of
        undefined ->
            put({event, Id}, [MoRef]);
        List ->
            case lists:member(MoRef, List) of
                true ->
                    ok;
                false ->
                    List1 = lists:append(List, [MoRef]),
                    put({event, Id}, List1)
            end
    end,
    %% Trig event towards c-lib
    case get({sub_socket, Id}) of
        undefined ->
            ok;
        Socket ->
            info_msg("Send event to c-lib on socket: ~p~n", [Socket]),
            gen_tcp:send(Socket, [<<?SECI_OK:4/native-integer-unit:8>>]),
            inet:setopts(Socket, [{active, true}, {nodelay, true}])
    end.

get_event(Id) ->
    case get({event, Id}) of
        undefined ->
            [];
        List ->
            put({event, Id}, []),
            List
    end.

handle_verify_peer_result({ok, _}, Socket) ->
    info_msg("verify_peer -> VALID~n", []),
    Res = [<<?SECI_VERIFY_PEER_VALID:4/native-integer-unit:8>>],
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]);
handle_verify_peer_result({error, {bad_cert, {revoked, Reason}}}, Socket) ->
    info_msg("verify_peer -> NOT_VALID: ~p~n",
        [{bad_cert, {revoked, Reason}}]),
    Res = [<<?SECI_VERIFY_PEER_NOT_VALID:4/native-integer-unit:8>>],
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]);
handle_verify_peer_result({error, {bad_cert, revocation_status_undetermined}},
    Socket) ->
    info_msg("verify_peer -> UNKNOWN: ~p~n",
        [{bad_cert, revocation_status_undetermined}]),
    Res = [<<?SECI_VERIFY_PEER_UNKNOWN:4/native-integer-unit:8>>],
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]);
handle_verify_peer_result({error, Reason}, Socket) ->
    info_msg("verify_peer -> NOT_VALID: ~p~n", [Reason]),
    Res = [<<?SECI_VERIFY_PEER_NOT_VALID:4/native-integer-unit:8>>],
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]).


%%===============================================================
%% Misc functions
%%===============================================================



%%% ----------------------------------------------------------
%%% INFO, WARNING and ERROR MESSAGE HANDLING
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).


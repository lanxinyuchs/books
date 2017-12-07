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
%%% %CCaseFile:	ftpesLib.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R11A/R12A/4
%%%
%%% ----------------------------------------------------------

-module(ftpesLib).
-vsn('/main/R9A/R11A/R12A/4').
-date('2017-12-06').
-author('enekdav').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R9A/1    2017-02-08   ekurnik    Created
%%% R9A/2    2017-03-08   ekurnik    Trace added
%%% R9A/3    2017-03-27   ekurnik    Support for VRCS added
%%% R11A/1-2 2017-10-11   etomist    OTP20 - disabling MITM protection
%%% R12A/1   2017-11-24   eivmiha    Added idle timer
%%% R12A/2   2017-11-28   emirbos    Added reading of ftpTlsServer info
%%% R12A/3   2017-12-06   enekdav    Changed ftpTlsServer info function
%%% ----------------------------------------------------------

-export([setup_tls/2,
         get_ip_interfaces/0,
         subscribe_mo/3,
         unsubscribe_mo/3,
         check_available_interface/0,
         check_available_interface/1,
         check_ip_address/1,
         get_cert/0,
         get_server_cert/0,
         get_server_admin_state/0,
         get_ip_address/1,
         get_namespace/1,
         get_ip_options/1,
         get_address_tag/1,
         get_idle_timer/0,
         get_ftp_tls_server_info/0
        ]).

-include("RcsFileTPM.hrl").
-include("ftpesd.hrl").

-type admin_state() :: ?BasicAdmState_LOCKED | ?BasicAdmState_UNLOCKED.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns TLS options. Verify_fun is included but will not send the
%%% message. If process needs to get the message the TCAT DN is also returned,
%%% so process is responsible for getting its own verify_fun
%%% ----------------------------------------------------------
-spec setup_tls(NC :: binary(), TC :: binary()) ->
          {ok, [property()], term()} | {error, reason()}.
setup_tls(NC, _TC) when NC =:= undefined  ->
    {error, nc_undefined};

setup_tls(_NC, TC) when TC =:= undefined  ->
    {error, tc_undefined};

setup_tls(NC, TC) ->
    setup_tls_1(certI:get_cert(NC), TC).

setup_tls_1({ok, Cert, {_Type, _Key} = Kt}, TcatDn) ->
    setup_tls_2({Cert, Kt}, certI:get_cert(TcatDn), TcatDn);

setup_tls_1({error, Reason}, _) ->
    sysInitI:warning_msg("~p: Node Credential missing, ~p~n", [?MODULE, Reason]),
    {error, Reason}.     

setup_tls_2({[NcCert|NcChain], Kt}, {ok, TcCerts}, TcatDn) ->
    VerifyFun = certI:mk_verify_fun(TcatDn, undefined),
    setup_tls_3(VerifyFun, {NcCert, Kt}, TcCerts++NcChain, TcatDn);

setup_tls_2(_, {error, Reason}, _) ->
    sysInitI:warning_msg("~p: Trusted Certificates missing, ~p~n", [?MODULE, Reason]),
    {error, Reason}.

setup_tls_3({error, Reason}, _, _, _) ->
    sysInitI:warning_msg("~p: Verify fun faulty, ~p~n", [?MODULE, Reason]), 
    {error, Reason};

setup_tls_3({ok, VerifyFun, PartialFun}, {NcCert, Kt}, TcCerts, _TcatDn) ->
    ?LOG("NcCert: [~p]~nKey: [~p]~nTcCerts: [~p]~nTcatDn: [~p]~n", [NcCert, Kt, TcCerts, _TcatDn]),
    Options = [
               {cert, NcCert},
               {key, Kt},
               {cacerts, TcCerts},
               {secure_renegotiate, true},
               {depth, 10},
               {verify, verify_peer},
               {fail_if_no_peer_cert, true},
               VerifyFun,
               PartialFun,
               {reuse_sessions, false},
               {ciphers, comsaI:get_tls_cipher_suites()},
               {versions, [tlsv1, 'tlsv1.2']},
               {server_name_indication, disable}
              ], 
    {ok, Options}.

%% subscribe_mo/3
%% ====================================================================
%% @doc Subscribe to nodeCredential and TrustCategory references

-spec subscribe_mo(Pid :: pid() | atom(), 
                   NodeCredential :: term(), 
                   TrustCategory :: term()) -> atom().
%% ====================================================================
subscribe_mo(Pid, NodeCredential, TrustCategory) ->
    case NodeCredential of
        undefined -> ok;
        _ -> certI:subscribe(NodeCredential, Pid)
    end,
    case TrustCategory of
        undefined -> ok;
        _ -> certI:subscribe(TrustCategory, Pid)
    end.

%% unsubscribe_mo/3
%% ====================================================================
%% @doc Unsubscribe from nodeCredential and TrustCategory references

-spec unsubscribe_mo(Pid :: pid() | atom(), 
                     NodeCredential :: term(), 
                     TrustCategory :: term()) -> atom().
%% ====================================================================
unsubscribe_mo(Pid, NodeCredential, TrustCategory) ->
    case {TrustCategory, NodeCredential} of
        {undefined, undefined} -> ok;
        {_, _}-> certI:unsubscribe(Pid)
    end.

%% check_available_interface/0
%% ====================================================================
%% @doc Check which interface is available by priority in list ?IP_INTERFACES
%%      and check which IP family BindAddress belongs

-spec check_available_interface() -> atom().
%% ====================================================================
check_available_interface() ->
    check_available_interface(get_ip_interfaces()).

%% check_available_interface/1
%% ====================================================================
%% @doc Check which interface is available by priority in list ?IP_INTERFACES
%%      and check which IP family BindAddress belongs

-spec check_available_interface(Interfaces :: [ip_interface()]) -> atom().
%% ====================================================================
check_available_interface([]) ->
    inet;
check_available_interface([Interface|OtherInterfaces]) ->
    BindAddress = get_ip_address(Interface),
    Namespace = get_namespace(Interface),
    case ?IP_IF_AVAIL(Interface, BindAddress, Namespace) of
        false -> check_available_interface(OtherInterfaces);
        _ -> IP_family = check_ip_address(BindAddress),
             IP_family
    end.

check_ip_address(IPAddress) when is_list(IPAddress)->
    {ok, ParsedAddress} = inet:parse_address(IPAddress),
    check_ip_address(ParsedAddress);
check_ip_address({_,_,_,_,_,_,_,_})->
    inet6;
check_ip_address(_) ->
    inet.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns FtpTls nodeCredential and trustCategory
%%% ----------------------------------------------------------
-spec get_cert() -> {term(), term()}.
get_cert() -> 
    [FtpTls] = mnesia:dirty_read(ftpTls, {"1", "1", "1", "1", "1"}), 
    {FtpTls#ftpTls.nodeCredential, FtpTls#ftpTls.trustCategory}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns FtpTlsServer nodeCredential and trustCategory
%%% ----------------------------------------------------------
-spec get_server_cert() -> {term(), term()}.
get_server_cert() -> 
    [FtpTlsServer] = mnesia:dirty_read(ftpTlsServer, {"1", "1", "1", "1", "1", "1"}),
    {FtpTlsServer#ftpTlsServer.nodeCredential, FtpTlsServer#ftpTlsServer.trustCategory}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns FtpTlsServer administrative state
%%% ----------------------------------------------------------
-spec get_server_admin_state() -> admin_state().
get_server_admin_state() ->
    [FtpTlsServer] = mnesia:dirty_read(ftpTlsServer, {"1", "1", "1", "1", "1", "1"}),
    FtpTlsServer#ftpTlsServer.administrativeState.


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns interfaces for target or for sim
%%% ----------------------------------------------------------
-spec get_ip_interfaces() -> [ip_interface()].
get_ip_interfaces() ->
    case sysEnv:rcs_mode_2() of
        simulated ->
            ?SIM_IP_INTERFACES;
        Mode when Mode =:= target orelse
                  Mode =:= vrcs ->
            ?IP_INTERFACES
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns address of specified IP interface
%%% ----------------------------------------------------------
-spec get_ip_address(Type :: ip_interface()) -> tuple() | string() | undefined.
get_ip_address(lmt) ->
    ootI:get_lmt_ipv4();
get_ip_address(oam) ->
    case ootI:get_oap_ip_addr() of
        [] ->
            undefined;
        IpAddress ->
            IpAddress
    end;
get_ip_address(alt_oam) ->
    case ootI:get_oap_ip_addr_alt() of
        [] ->
            undefined;
        IpAddress ->
            IpAddress
    end;
get_ip_address(sim_ipv4) ->
    {127,0,0,1}; %% localhost
get_ip_address(sim_ipv6) ->
    {0,0,0,0,0,0,0,1}. %% localhost


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns namespace of specified IP interface 
%%% ----------------------------------------------------------
-spec get_namespace(Type :: ip_interface()) -> binary().
get_namespace(lmt) ->
    sysInitI:get_lmt_ns();
get_namespace(oam) ->
    case ootI:get_oap_namespace() of
        {ok, Ns} ->
            Ns;
        {error, _} ->
            %% resolution pending
            <<>>
    end;

get_namespace(alt_oam) ->
    case ootI:get_oap_alt_namespace() of
        {ok, Ns} ->
            Ns;
        {error, _} ->
            %% resolution pending
            <<>>
    end;
get_namespace(sim_ipv4) ->
    <<>>;
get_namespace(sim_ipv6) ->
    <<>>.


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns IP options based on interface
%%% ----------------------------------------------------------
-spec get_ip_options(Type :: ip_interface()) -> [property()].
get_ip_options(lmt) ->
    ootI:get_lmt_ns_opt_list() ++ 
        [{ip, get_ip_address(lmt)}];
get_ip_options(oam) ->
    ootI:get_all_oam_opt_list();
get_ip_options(alt_oam) ->
    omc_api:ns_to_opt_list(get_namespace(alt_oam)) ++ 
        ootI:get_dscp_opt_list() ++
        ootI:get_oap_ip_opt_list(get_ip_address(alt_oam));
get_ip_options(SimIf) ->
    [{ip, get_ip_address(SimIf)}].

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns address "tag" in update based on interface
%%% ----------------------------------------------------------
-spec get_address_tag(Type :: ip_interface()) -> atom().
get_address_tag(lmt) ->
    lmt_ipv4;
get_address_tag(oam) ->
    access_point_address;
get_address_tag(alt_oam) ->
    access_point_address_alt.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns FtpServer idle timer
%%% ----------------------------------------------------------
-spec get_idle_timer() -> number().
get_idle_timer() ->
    [FtpServer] = mnesia:dirty_read(ftpServer, {"1", "1", "1", "1", "1"}),
    FtpServer#ftpServer.idleTimer.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns FtpTlsServer info
%%% ----------------------------------------------------------
get_ftp_tls_server_info() ->
    [FtpTlsServerInfo] = mnesia:dirty_read(ftpTlsServer, {"1", "1", "1", "1", "1", "1"}),
    FtpTlsServerInfo.

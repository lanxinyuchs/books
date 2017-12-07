%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_tls_ecoli_proxy.erl %
%%% @author emariad
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/R6A/1

%%% @doc ==Header==
%%% Process instance that works like a proxy towards ECOLI
%%% The module at the other end is mostly an ssh_channel (adapted)
%%% @end

%%% This is not in any way elegant - quick hack to get COLI working on TLS
%%% for the maintenance user. The this module, ecoli_transport_adaptor, and
%%% ecoli_ssh_channel should be merged and splitted in some clever way to 
%%% avoid duplication of code and differences in how COLI works depending on
%%% the transport. (e.g. what is done at terminate() in ssh isn't done here)
%%% /lg
-module(omc_tls_ecoli_proxy).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/1').
-date('2016-09-05').
-author('emariad').

-include("omc.hrl").
-define(ECOLI_CB, ecoli_transport_adaptor).

-record(st, {	type,	%expected to alway be 'coli'
		parent,
		socket,
		peer_ip,
		user,
		roles = [],
		%is_change_user_cap = false,
		%ch_st, % #user_scan{}
		cb_state,
		timeout}).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/1      2014-07-08   etxlg     copied from omc_tls_instance
%%% R2A/2      2014-07-09   etxlg     add expert role to maintenance user
%%% R2A/3      2014-09-01   etxlg     spelling
%%% R2A/4      2014-09-11   etxlg     idle timeout. More fixing needed -
%%%				      wait until we can merge proxy/transport
%%% R3A/1      2015-02-24   etxlg     verify_fun is a list
%%% R3A/2      2015-03-02   etxlg     new cert API for maintenance user
%%% R3A/3      2015-03-09   etxlg     Type changed due to namespacing
%%% R3A/4      2015-03-10   etxlg     Trap-exit needed when running ecoli
%%% R3A/5      2015-05-19   etxlg     call ?ECOI_CB:terminate
%%% R4A/1      2015-07-14   etxlg     RCS-COLI Role fudged in ECOLI
%%% R4A/2      2015-08-13   etxtory   dialyze fix
%%% R5A/1      2016-08-19   ehsake    Remove sec log, close socket, HV18193
%%% R6A/1      2016-08-30   emariad   CSUC feature, cipher configuration

%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/5]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
start({coli, _} = Type, Tls_gen_srv, Listen_socket, Tcat, Options) ->
    {ok, Sock} = gen_tcp:accept(Listen_socket),
    {ok, {Address, _Peer_port}} = inet:peername(Sock),
    Vfun = omc_lib:fetch_verify_fun("COLI TLS", [{trustCategory, Tcat}],
        {client, self()}),
    ok = omc_tls_server:event(accept, {Type, Address}),
    case ssl:ssl_accept(Sock,  Vfun ++ Options, 5000) of
	{ok, Ssl_sock} ->
	    receive
		%new "correct" api towards CERT
		{user_from_cert, {ok, User, Subject_name, Crl_status}} ->
		    check_super_oam(Type, Tls_gen_srv, Ssl_sock, Address, User,
				  Subject_name, Crl_status)
	    after
		2000 ->
		    Msg = "Timeout verifying client-cert",
		    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg),
		    ok = omc_tls_server:event(auth_error, {Type, Address,
			Msg}),
		    exit(normal)
	    end;
	{error, timeout} ->
	    Msg = "TLS handshake timed out",
            omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg),
	    ok = omc_tls_server:event(auth_error, {Type, Address,
			Msg}),
	    exit(normal);
	{error, Error} ->
	    Msg = io_lib:format("TLS accept error: ~p ",[Error]),
	    ok = omc_tls_server:event(auth_error, {Type, Address,
			Msg}),
	    gen_tcp:close(Sock),
	    exit(normal)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
tls_loop(#st{socket = Socket, timeout = Timeout} = S) ->
    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl, Socket, Data} ->
		%HERE FIXME handle the timeout
	    case ?ECOLI_CB:handle_ssh_msg({ssh_cm, nil,
					      {data, nil, nil, Data}},
					  S#st.cb_state) of
	    {ok, New_cb_state, New_timeout} ->
		tls_loop(S#st{cb_state = New_cb_state, timeout = New_timeout});
	    {stop, _ ,_} ->
		exit(normal)
	    end;
	{ssl_closed, Socket} ->
	    ?ECOLI_CB:terminate(ssl_closed, S#st.cb_state),
	    exit(normal);
	{ssl_error, Socket, Reason} ->
	    ?ECOLI_CB:terminate(ssl_error, S#st.cb_state),
	    exit(Reason);
	{coli_shell, {data, Data}} ->
	    ok = ssl:send(Socket, Data),
	    tls_loop(S);
    {close_session, Message} ->
         case Message of
        [] ->
            ok;
        Message ->
            ok = ssl:send(Socket, [Message, "\r\n"])
        end,
        exit(normal);
	Wup ->
		%HERE FIXME  + could maybe also return {stop,,,}
	    {ok, New_cb_state, New_timeout} =
		?ECOLI_CB:handle_msg(Wup, S#st.cb_state),
	    tls_loop(S#st{cb_state = New_cb_state, timeout = New_timeout})
    after
	Timeout ->
	    ?ECOLI_CB:terminate(timeout, S#st.cb_state),
	    exit(normal)
    end.

check_super_oam(Type, Tls_gen_srv, Ssl_sock, Peer_ip, User,
				  Subject_name, Crl_status) ->
    case comsaI:is_super_oam_user(Subject_name) of
        true ->
            Roles = [?SUPER_ROLE], %ecoli_lib will add "expert" to this
            Msg = "Maintenance user login: " ++ User ++ ", Roles: " ++
                omc_lib:roles_to_string(Roles),
            omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
            more_auth(Type, Tls_gen_srv, Ssl_sock, Peer_ip, User, Roles);
        false ->
	    continue_auth(Type, Tls_gen_srv, Ssl_sock, Peer_ip, User,
			  Crl_status)
    end.

continue_auth(Type, Tls_gen_srv, Ssl_sock, Peer_ip, User, crl_ok) ->
    case omc_ldap_server:ldap_lookup(User) of
	{true, Roles} ->
            Msg = "LDAP: lookup for user: " ++ User ++ ", Roles: " ++
		omc_lib:roles_to_string(Roles),
            omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
	    more_auth(Type, Tls_gen_srv, Ssl_sock, Peer_ip, User, Roles);
	{false, _Reason} ->
	    Msg = "LDAP: lookup failure for user: " ++ User,
            omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
            ok = omc_tls_server:event(auth_error, {Type, Peer_ip, Msg}),
            exit(normal)
    end;
continue_auth(Type, _Tls_gen_srv, _Ssl_sock, Peer_ip, User, _Crl_bad_reason) ->
    Msg = "TLS: Certificate faild CRL check for user: " ++ User,
    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
    ok = omc_tls_server:event(auth_error, {Type, Peer_ip, Msg}),
    exit(normal).

more_auth(Type, Tls_gen_srv, Ssl_sock, Peer_ip, User, Roles) ->
    ok = omc_server:store_authorization(User, Roles),
    omc_server:store_session(User, element(1, Type), tls, Roles),
    ok = omc_tls_server:event(login, {Type, Peer_ip, User}),
    {ok, Cb_state, Timeout} = 
	?ECOLI_CB:init(self(), omc_lib:ip_to_string(Peer_ip), User, Roles),
    process_flag(trap_exit, true),
    tls_loop(#st{type = Type,
		 parent = Tls_gen_srv,
		 socket = Ssl_sock,
		 peer_ip = Peer_ip,
		 user = User,
		 roles = Roles, 
		 cb_state = Cb_state,
		 timeout = Timeout}).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

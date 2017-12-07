%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_tls_instance.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R10A/2

%%% @doc ==Header==
%%% @end

-module(omc_tls_instance).
% -behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R10A/2').
-date('2017-05-05').
-author('etxarnu').

-include("omc.hrl").

-record(st, {	type,
		parent,
		socket,
		peer_ip,
		user,
		roles = [],
		is_change_user_cap = false,
		ch_st, % #user_scan{}
		port}).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-01-20   etxlg     Created
%%% R2A/2      2014-01-20   etxlg     Tried to silence dialyzer
%%% R2A/3      2014-01-22   etxlg     Continued dev
%%% R2A/4      2014-01-26   etxlg     ms_useridentity
%%% R2A/5      2014-03-28   etxlg     Connect to sysM, Cert, Ldap
%%% R2A/6      2014-06-04   etxlg     less tuple with more arity
%%% R2A/7-8    2014-06-16   etxlg     super oam user
%%% R2A/9      2014-06-17   etxlg     close_session
%%% R2A/10     2014-09-01   etxlg     not simulated when running on target,
%%%				      spelling
%%% R3A/1      2015-01-15   etxlg     maintenance user works with bad CRL
%%% R3A/2      2015-02-24   etxlg     v_fun is a list [verify_fun, partial_fun]
%%% R3A/3      2015-03-09   etxlg     Changed as namespaces are introduce
%%% R5A/1      2016-08-19   ehsake    Remove sec log, close socket, HV18193
%%% R6A/1      2016-08-30   uabesvi   vRC should be treated as target and not simulated
%%% R10A/1-2   2017-05-05   etxarnu   Handle ssl:send errors (to avoid filling 
%%%                                   erlang logs as in HV76473)
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/5]).
-export([close_session/2]).

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
%Type is now a tuple, e.g. {cli, lmt}, {cli, any}, {cli, oam}
start(Type, Tls_gen_srv, Listen_socket, Tcat, Options) ->
    {ok, Sock} = gen_tcp:accept(Listen_socket),
    {ok, {Address, _Peer_port}} = inet:peername(Sock),
    Vfun = omc_lib:fetch_verify_fun("NETCONF TLS", [{trustCategory, Tcat}],
        {client, self()}),
    ok = omc_tls_server:event(accept, {Type, Address}),
    case ssl:ssl_accept(Sock,  Vfun ++ Options, 5000) of
	{ok, Ssl_sock} ->
	    receive
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
%%            omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg),
	    ok = omc_tls_server:event(auth_error, {Type, Address,
			Msg}),
	    gen_tcp:close(Sock),
	    exit(normal)
    end.

close_session(Pid, Message) when is_pid(Pid) ->
    Pid ! {close_session, Message}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
tls_loop(#st{type = Type, socket = Socket, port = Port} = S) ->
    ssl:setopts(Socket, [{active, once}]),
    receive
	{flushed_data_to_user, Data} ->
	    case ssl:send(Socket, Data) of
		ok ->
		    tls_loop(S);
		Err ->
		    sysInitI:warning_msg(
		      "~p:~p: ssl:send #1 failed with: ~p~n",
		      [?MODULE, tls_loop, Err]),
		    exit(normal)
	    end;

		   
	{ssl, Socket, Data} when S#st.is_change_user_cap ->
	    if
		element(1, S#st.type) =:= cli ->
		    true = port_command(Port, Data);
		true ->
		    true
	    end,
	    {User_scan, Is_user_cap, New_port, User, Roles} =
		omc_ms_user_change:ms_user_change(element(1, Type), "TLS",
						S#st.ch_st,
						Data,
						Port,
						S#st.peer_ip,
						S#st.user,
						S#st.roles),
	    if
		User =/= S#st.user ->
                    ok = omc_tls_server:event(relogin, {Type, User});
		true ->
		    true
	    end,
	    tls_loop(S#st{ch_st = User_scan,
			is_change_user_cap = Is_user_cap,
			port = New_port,
			user = User,
			roles = Roles});
	{ssl, Socket, Data} ->
	    true = port_command(Port, Data),
	    tls_loop(S);
	{ssl_closed, Socket} ->
	    %io:format("~p: TLS socket closed~n", [?MODULE]),
	    exit(normal);
	{ssl_error, Socket, Reason} ->
	    %io:format("~p: Error on sslsocket: ~p~n", [?MODULE, Reason]),
	    exit(Reason);
	{Port, {data, Data}} when S#st.is_change_user_cap,
				  element(1, S#st.type) =:= cli ->
	    case ssl:send(Socket, Data) of
		ok ->
		    New_ch_st = S#st.ch_st#user_scan{last_from = Data},
		    tls_loop(S#st{ch_st = New_ch_st});
		Err ->
		    sysInitI:warning_msg(
		      "~p:~p: ssl:send #2 failed with: ~p~n",
		      [?MODULE, tls_loop, Err]),
		    exit(normal)
	    end;
	{Port, {data, Data}} ->
	    case ssl:send(Socket, Data) of
		ok ->
		    tls_loop(S);
		Err ->
		    sysInitI:warning_msg(
		      "~p:~p: ssl:send #3 failed with: ~p~n",
		      [?MODULE, tls_loop, Err]),
		    exit(normal)
	    end;
	{Port, {exit_status, 0}} ->
	    exit(normal);
	{Port, {exit_status, Rc}} ->
	    sysInitI:warning_msg(
		"~p: ~p: Error exit from port program: ~p~n",
		[?MODULE, Type, Rc]),
	    exit(normal);
	{close_session, []} ->
	    exit(normal);
	{close_session, Message} ->
	    ssl:send(Socket, [Message, "\r\n"]), %don't care for ssl:send result
	    exit(normal);
	_Wup ->
	    tls_loop(S)
    end.

check_super_oam(Type, Tls_gen_srv, Ssl_sock, Peer_ip, User,
				  Subject_name, Crl_status) ->
    case comsaI:is_super_oam_user(Subject_name) of
        true ->
            Roles = [?SUPER_ROLE],
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
    {Change_user_cap, Change_state} =
	omc_ms_user_change:ms_user_change_capable(element(1, Type), Roles),
    simulator_message(Type, Ssl_sock),
    Port =
	if
	    Change_user_cap andalso element(1, Type) =:= netconf ->
		undefined;
	    true ->
		omc_lib:run_portprog(User, element(1, Type))
	end,
    tls_loop(#st{type = Type,
		parent = Tls_gen_srv,
		socket = Ssl_sock,
		peer_ip = Peer_ip,
		user = User,
		roles = Roles,
		is_change_user_cap = Change_user_cap,
		ch_st = Change_state,
		port = Port}).

simulator_message({netconf, _}, _) -> ok;
simulator_message({cli, _}, Sock) ->
    case sysEnv:rcs_mode_2() of
	simulated ->
	    ssl:send(Sock, << "\r\nYou are accessing a simulated RBS\r\n\n" >>);
	_ ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

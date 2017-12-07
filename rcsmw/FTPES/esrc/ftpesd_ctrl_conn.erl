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
%%% %CCaseFile:	ftpesd_ctrl_conn.erl %
%%% @author eivmiha
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/R12A/2
%%%
%%% ----------------------------------------------------------
-module(ftpesd_ctrl_conn).
-vsn('/main/R8A/R9A/R12A/2').
-date('2017-11-27').
-author('eivmiha').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%% -----      -------    --------    ------------------------
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/4    2016-11-23   eivmiha    Changed dataport opening, acceptors closing
%%% R8A/5    2016-11-23   emarnek    Start_ssl handling added
%%% R8A/6    2016-11-24   eivmiha    code editing
%%% R8A/10   2016-12-09   eivmiha    get_ipv 
%%% R8A/11   2016-12-16   ekurnik    Verify_fun getter temp solution
%%% R8A/12   2016-12-19   emarnek    Added security log
%%% R8A/14   2016-12-19   ekurnik    Added fetching verify_fun
%%% R8A/16   2017-01-09   emarnek    Changes because of intercept
%%% R8A/17   2017-01-10   eivmiha    Changed PORT command return codes
%%% R8A/18   2017-01-10   ekurnik    Added absolute path support
%%% R9A/1    2017-01-24   ekurnik    Fixed warnings
%%% R9A/3    2017-02-27   ekurnik    Added timeout for idle connection
%%% R9A/4    2017-03-08   ekurnik    Fixed ABOR command demonitor
%%% R9A/6    2017-03-29   ekurnik    Authentication sequence refactoring
%%% R12A/1   2017-11-24   eivmiha    Added idle timer
%%% R12A/2   2017-11-27   eivmiha    Idle timer fix
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/4,
         start_accepting/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, 
         handle_call/3,
         handle_cast/2,
         terminate/2, 
         code_change/3, 
         handle_info/2]).


-include("ftpesd.hrl").
-behaviour(gen_server).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start_link(LCtrlSock, LDataSock, Args, Pid) ->
    gen_server:start_link(?MODULE, [LCtrlSock, LDataSock, Args, Pid], []).

start_accepting() ->
    self() ! start_accepting.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init([LCtrlSock, LDataSock, Args, Pid]) ->   
    start_accepting(),
    {ok, [LCtrlSock, LDataSock, Args, Pid]}.


handle_call(stop, _From, State)->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State)->
    {reply, nok, State, get_timeout(State)}.

handle_cast(_Msg, State)->
    {noreply, State, get_timeout(State)}.

%%% @doc Start accepting on listen socket and send the Pid to listener process 
handle_info(start_accepting, [LSock, DSock, Args, Pid]) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            ok = ftpesd_listener:event(Pid, accept, {self()}),
            inet:setopts(Sock, [{packet, line}]),
            ftpesd_util:send_reply(Sock, 220, "Hello"),
            ConnDataState = construct_conn_data(Args, Sock, DSock, Pid),
            {noreply, ConnDataState, ConnDataState#ctrl_conn_data.idle_timer};
        {error, Res} -> 
            {stop, {shutdown, {Res, LSock}}, [LSock, DSock, Args, Pid]}
    end;

handle_info({tcp, Socket, Data}, #ctrl_conn_data{auth_phase=none, control_socket = Socket, idle_timer = IdleTimer} = State) ->
    {Command, Msg} = ftpesd_util:packet_to_tokens(Data),
    NewState = process_message(Socket, Command, Msg, State),
    {noreply, NewState, IdleTimer};

handle_info({tcp, Socket, _Data}, State) -> 
    close_socket(Socket),
    {stop, {shutdown, unexpected_tcp}, State};

handle_info({ssl, Socket, Data}, #ctrl_conn_data{control_socket = Socket} = State ) ->
    {Command, Msg} = ftpesd_util:packet_to_tokens(Data),
    ?LOG("[~p-Recv]: ~p - ~p~n", [Socket, Command, Msg]),
    NewState = process_message(Socket, Command, Msg, State),
    {noreply, NewState, get_timeout(NewState)};

%%% @doc Phase after AUTH command is started and SSL/TLS server-side handshake is 
%%% performed.
handle_info(start_ssl, #ctrl_conn_data{auth_phase=started} = State) ->
    Socket = State#ctrl_conn_data.control_socket,
    {ok, {Address, Peer_port}} = inet:peername(Socket),
    
    TlsOptions = State#ctrl_conn_data.tls_options,
    TcatDn = State#ctrl_conn_data.trusted_category,
    IdleTimer = State#ctrl_conn_data.idle_timer,
    case ftpesd_util:get_verify_fun(TcatDn, TlsOptions) of
        {ok, NewTlsOptions} ->
            case ssl:ssl_accept(Socket, NewTlsOptions) of
                {ok, SSLSocket} -> 
                    receive
                        {user_from_cert, {ok, User, _Subject_name, _Crl_status}} ->
                            omc_lib:add_session_info(self(), "TLS", "FTP", User, ftpesd_util:ip_to_string(Address)),
                            Count = omc_lib:add_session(tls_ftp),
                            Msg = io_lib:format("TLS: User: ~p, ftpes session ~p started, session count ~p",[User, self(), Count]),
                            ftpesd_util:sec_log(ftpesd_util:ip_to_string(Address), Msg),
                            %% Old params
                            OldUserArgs = State#ctrl_conn_data.user_data_params,
                            %% New params
                            NewUserArgs = OldUserArgs#user_data{peer_address = Address, peer_port = Peer_port, peer_user = User},
                            NewState = State#ctrl_conn_data{control_socket = SSLSocket, user_data_params = NewUserArgs, 
                                                            auth_phase = next_auth_state(started)},
                             {noreply, NewState, IdleTimer}
                    after 2000 ->
                        Msg = "Timeout verifying client-cert",
                        ftpesd_util:sec_log(ftpesd_util:ip_to_string(Address), Msg),
                        {stop, {shutdown, verify_timeout}, State}
                    end;
                {error, timeout} ->
                    Msg = "TLS: ftpes handshake timed out",
                    ftpesd_util:sec_log(ftpesd_util:ip_to_string(Address), Msg),
                    {stop, {shutdown, handshake_timeout}, State};
                {error, Error} ->
                    Msg = io_lib:format("TLS: ftpes accept error: ~p ",[Error]),
                    ftpesd_util:sec_log(ftpesd_util:ip_to_string(Address), Msg),
                    {stop, {shutdown, Error}, State}
            end;
        {error, Reason} ->
            Msg = io_lib:format("TLS: faulty verify_fun: ~p ",[Reason]),
            ftpesd_util:sec_log(ftpesd_util:ip_to_string(Address), Msg),
            {stop, {shutdown, Reason}, State}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    ?LOG("TCP closed!~n"),
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    sysInitI:error_msg("~p: TCP error ~p~n", [?MODULE, Reason]),
    {stop, Reason, State};
handle_info({ssl_closed, _Socket}, State) ->
    ?LOG("SSL closed!~n"),
    {stop, normal, State};
handle_info({ssl_error, _Socket, Reason}, State) ->
    sysInitI:error_msg("~p: SSL error ~p~n", [?MODULE, Reason]),
    {stop, Reason, State};

%% Data process has exited, clear the state and send message if it was unexpected
handle_info({'DOWN', MonitorRef, process, _, Reason}, 
            #ctrl_conn_data{data_proc_ref = MonitorRef} = State) ->
    handle_data_process_exit(State#ctrl_conn_data.control_socket, Reason),
    {noreply, State#ctrl_conn_data{data_pid = none, data_proc_ref = none}, State#ctrl_conn_data.idle_timer};


handle_info(timeout, State) ->
    sysInitI:info_msg("~p: FTPES session idle for ~p seconds - shutting down~n", 
                      [?MODULE, State#ctrl_conn_data.idle_timer/1000]),
    {stop, {shutdown, session_timeout}, State};

handle_info(_Message, State) ->
    ?LOG("Unexpected message received: ~p~n", [_Message]),
    {noreply, State, get_timeout(State)}.

%% If SSL session was established, log the session end
terminate(_Reason, #ctrl_conn_data{control_socket = {sslsocket, _, _}} = State) ->
    Address = State#ctrl_conn_data.user_data_params#user_data.peer_address,
    User = State#ctrl_conn_data.user_data_params#user_data.peer_user,
    omc_lib:remove_session_info(self()),
    Count = omc_lib:remove_session(tls_ftp),
    Msg = io_lib:format("TLS: User: ~p, ftpes session ~p ended, session count ~p",[User, self(), Count]),
    ftpesd_util:sec_log(ftpesd_util:ip_to_string(Address), Msg),
    ok;

%% SSL wasn't established, no need to log
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle incoming FTP commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% #    handle_command/3
%%% @doc 
-spec handle_command(Command :: bitstring(), Message :: [bitstring()],
                     Args :: #ctrl_conn_data{}) -> {reply(), argschange()}.
%%% ----------------------------------------------------------
handle_command(<<"NOOP">>, [], _) ->
    mk_rep(200, "NOOP command successful");

handle_command(<<"ABOR">>, _, #ctrl_conn_data{data_pid = DataPid, data_proc_ref = Ref} = Args) ->
    abort_data_proc(DataPid, Ref),
    mk_rep(226, "Closed listen socket", Args#ctrl_conn_data{data_pid = none, data_proc_ref = none});

handle_command(<<"QUIT">>, [], Args) ->
    _User = Args#ctrl_conn_data.user_data_params#user_data.username,
    ?LOG("Connection close, user: ~p~n", [_User]),
    mk_rep(221, "Goodbye.");

handle_command(<<"USER">>, _, #ctrl_conn_data{authed = true}) ->
    mk_rep(503, "You are already logged in");
handle_command(<<"USER">>, ParamsBin, #ctrl_conn_data{auth_phase = AuthPhase,
                                                      user_data_params = OldUserArgs} = Args) ->
    User = ftpesd_util:binlist_to_string(ParamsBin),
    ?LOG("User: ~p~n", [User]),

    %% New args
    NewUserArgs = OldUserArgs#user_data{ username = User},
    NewArgs = Args#ctrl_conn_data{auth_phase = next_auth_state({AuthPhase, false}, "USER"),
                                  authed = true, user_data_params = NewUserArgs},
    mk_rep(232, "User logging in", NewArgs);

handle_command(<<"TYPE">>, ParamsBin, Args) ->
    Params  = [ binary_to_list(E)  || E <- ParamsBin],
    ParamsF = [ string:to_upper(E) || E <- Params],
    case ftpesd_util:check_repr_type(ParamsF) of
        true ->
            NewArgs = Args#ctrl_conn_data{ repr_type = ParamsF },
            mk_rep(200, "TYPE set to " ++ hd(ParamsF), NewArgs);
        false ->
            mk_rep(500, "'TYPE "++string:join(Params," ")++"' not understood")
    end;

handle_command(<<"SIZE">>, ParamsBin, Args) ->
    FileName = ftpesd_util:binlist_to_string(ParamsBin),
    RelPath  = ftpesd_util:concat_paths(ftpesd_util:get_current_dir(FileName, Args), FileName),
    FullPath = ftpesd_util:get_full_path(RelPath, Args),
    case filelib:is_regular(FullPath) of
        true  -> mk_rep(213, integer_to_list(filelib:file_size(FullPath)));
        false -> mk_rep(550, FileName ++ ": not a regular file")
    end;

handle_command(<<"RETR">>, ParamsBin, Args) ->
    FileName = ftpesd_util:binlist_to_string(ParamsBin),
    ftpesd_data_conn:send_msg(retr, FileName, Args);


handle_command(<<"STOR">>, ParamsBin, Args) ->
    FileName = ftpesd_util:binlist_to_string(ParamsBin),
    ftpesd_data_conn:send_msg(stor, {FileName, write}, Args);

handle_command(<<"APPE">>, ParamsBin, Args) ->
    FileName = ftpesd_util:binlist_to_string(ParamsBin),
    ftpesd_data_conn:send_msg(stor, {FileName, append}, Args);

handle_command(<<"CWD">>, ParamsBin, Args) ->
    NewDir  = ftpesd_util:binlist_to_string(ParamsBin),
    CurDir  = ftpesd_util:get_current_dir(NewDir, Args),
    BaseDir = Args#ctrl_conn_data.chrootdir,
    NewPath = ftpesd_dir:canonicalize_path(ftpesd_util:concat_paths(CurDir,NewDir)),
    case filelib:is_dir(BaseDir++NewPath) of
        true ->
            case ftpesd_util:check_dir_in_chroot(BaseDir, BaseDir++NewPath) of
                true ->
                    NewArgs = Args#ctrl_conn_data{ curr_path = NewPath },
                    mk_rep(250, "CWD command successful.", NewArgs);
                false ->
                    mk_rep(550, NewDir ++ ": No such file or directory")
            end;
        false ->
            mk_rep(550, NewDir ++ ": No such file or directory")
    end;

handle_command(<<"PWD">>, [], Args) ->
    RspStr = "\""++Args#ctrl_conn_data.curr_path++"\" is the current directory",
    mk_rep(257, RspStr);

handle_command(<<"STRU">>, [Type], _) ->
    case ftpesd_util:bin_to_upper(Type) of
        <<"F">> -> mk_rep(200, "Structure set to F");
        _       -> mk_rep(504, "Unsupported structure type")
    end;

handle_command(<<"MODE">>, [Type], _) ->
    case ftpesd_util:bin_to_upper(Type) of
        <<"S">> -> mk_rep(200, "Mode set to Stream");
        _ -> mk_rep(504, "Unsupported mode")
    end;

handle_command(<<"PASV">>, [], Args) ->
    DSock = Args#ctrl_conn_data.data_socket,
    TlsOptions = Args#ctrl_conn_data.tls_options,
    ftpesd_data_conn:reinit_data_conn(Args),
    NewArgs=Args#ctrl_conn_data{inet = inet},
    Ip = Args#ctrl_conn_data.ip,
    case ftpesd_data_conn:start_passive_mode(inet, TlsOptions, DSock) of
        {ok, {PasvPid, Ref, Port}} ->
            AddrStr = ftpesd_util:format_address(Ip, Port),
            RespStr = "Entering Passive Mode (" ++ AddrStr ++ ").",
            NewArgs2 = NewArgs#ctrl_conn_data{ data_pid = PasvPid, data_proc_ref = Ref, type = passive },
            mk_rep(227, RespStr, NewArgs2);
        {error, _} ->
            mk_rep(500, "PASV command failed (1)")
    end;

handle_command(<<"PORT">>, [BinArg1], Args) ->
    Params       = binary_to_list(BinArg1),
    IpPortParams = string:tokens(Params, ","),
    case ftpesd_util:list2portip(IpPortParams) of
        {ok, {Addr, Port}} ->
            case check_addr_port(Addr, Port) of % funkcija za verifikaciju Addr i Port
                ok ->
                    NewArgs = Args#ctrl_conn_data{type = active, port = Port, address = Addr, inet = inet},
                    mk_rep(200, "PORT received on server side", NewArgs);
                nok ->
                    mk_rep(501, "PORT command failed, wrong IP or Port")
            end;
        {error, _} -> mk_rep(501, "PORT command failed, wrong format") 
    end;

handle_command(<<"EPSV">>, [], Args) ->
    start_passive_mode_EPASV(Args);

handle_command(<<"EPSV">>, [BinArg1], Args) ->
    Params = binary_to_list(BinArg1),
    Ipv = get_ipv(Args),
    case Params of
        "1" when Ipv == inet ->
            start_passive_mode_EPASV(Args);
        "2" when Ipv == inet6 -> 
            start_passive_mode_EPASV(Args);
        "ALL" -> 
            NewArgs = Args#ctrl_conn_data{ auth_phase = epsv_spec_phase },
            start_passive_mode_EPASV( NewArgs);
        _ -> mk_rep(522, "Network protocol not supported")
    
    end;

%% format : EPRT<space><d><net-prt><d><net-addr><d><tcp-port><d>
handle_command(<<"EPRT">>, [BinArg1], Args) ->
    Params = binary_to_list(BinArg1),
    IpPortParams = string:tokens(Params, "|"),
    case ftpesd_util:eprtlist2portip(IpPortParams) of
        {ok, {Addr, Port, Ipv}} ->
            case port_in_range(Port) of
                ok-> ?LOG("Address, port: ~p~p~n", [Addr, Port]),
                     NewArgs = Args#ctrl_conn_data{type = active, port = Port, address = Addr, inet = Ipv},
                     mk_rep(200, "EPRT received on server side", NewArgs);
                _-> mk_rep(501, "EPRT command failed, port number out of range") 
            end;
        {error, _} -> mk_rep(501, "EPRT command failed, wrong format") 
    end;

handle_command(<<"LIST">>, ParamsBin, Args) ->
    DirToList = ftpesd_util:binlist_to_string(ParamsBin),
    AbsPath   = Args#ctrl_conn_data.chrootdir,
    UserArgs  = Args#ctrl_conn_data.user_data_params,
    RelPath   = ftpesd_util:get_current_dir(DirToList, Args),
    _NewPath = ftpesd_util:concat_paths(RelPath, DirToList),
    {ok, Files} =  ftpesd_dir:long_list_dir(AbsPath, RelPath, DirToList, UserArgs),
    ?LOG("List, NewPath ~p~n", [_NewPath]),
    ftpesd_data_conn:send_msg(list, Files, Args);

handle_command(<<"NLST">>, ParamsBin, Args) ->
    DirToList = ftpesd_util:binlist_to_string(ParamsBin),
    AbsPath   = Args#ctrl_conn_data.chrootdir,
    UserArgs  = Args#ctrl_conn_data.user_data_params,
    RelPath   = ftpesd_util:get_current_dir(DirToList, Args),
    _NewPath = ftpesd_util:concat_paths(RelPath, DirToList),
    {ok, Files} =  ftpesd_dir:short_list_dir(AbsPath, RelPath, DirToList, UserArgs),
    ?LOG("NLIST, NewPath ~p~n", [_NewPath]),
    ftpesd_data_conn:send_msg(list, Files, Args);

handle_command(<<"REIN">>, [], Args) ->
    % Old params
    OldUserArgs = Args#ctrl_conn_data.user_data_params,
    %% New params
    NewUserArgs = OldUserArgs#user_data{ username = none},
    NewArgs = Args#ctrl_conn_data{ authed = false, auth_phase = next_auth_state({valid_seq, true}, "REIN"), 
                                   user_data_params = NewUserArgs},
    mk_rep(220, "Service ready for new user", NewArgs);

handle_command(<<"MKD">>, ParamsBin, Args) ->
    Dir      = ftpesd_util:binlist_to_string(ParamsBin),
    UserArgs  = Args#ctrl_conn_data.user_data_params,
    RelPath  = ftpesd_util:concat_paths(ftpesd_util:get_current_dir(Dir, Args), Dir),
    FullPath = ftpesd_util:get_full_path(RelPath, Args),
    case ftpesd_dir:make_dir(FullPath, UserArgs) of
        ok	      -> mk_rep(257, "\"" ++RelPath++ "\" directory created");
        {error, eexist} -> mk_rep(550, "Folder already exists");
        {error, eacces} -> mk_rep(550, "MKD command failed, no access");
        {error, _}      -> mk_rep(550, "MKD command failed")
    end;

handle_command(<<"RMD">>, ParamsBin, Args) ->
    Dir      = ftpesd_util:binlist_to_string(ParamsBin),
    UserArgs  = Args#ctrl_conn_data.user_data_params,
    RelPath  = ftpesd_util:concat_paths(ftpesd_util:get_current_dir(Dir, Args), Dir),
    FullPath = ftpesd_util:get_full_path(RelPath, Args),
    case ftpesd_dir:del_dir(FullPath, UserArgs) of
        ok	 -> mk_rep(250, "Folder deleted");
        {error, eacces} -> mk_rep(550, "RMD command failed, no access");
        {error, _} -> mk_rep(550, "RMD command failed")
    end;

handle_command(<<"DELE">>, ParamsBin, Args) ->
    Dir      = ftpesd_util:binlist_to_string(ParamsBin),
    UserArgs  = Args#ctrl_conn_data.user_data_params,
    RelPath  = ftpesd_util:concat_paths(ftpesd_util:get_current_dir(Dir, Args), Dir),
    FullPath = ftpesd_util:get_full_path(RelPath, Args),
    case ftpesd_dir:delete(FullPath, UserArgs) of
        ok	 -> mk_rep(250, "File deleted");
        {error, eacces} -> mk_rep(550, "DELE command failed, no access");
        {error, _} -> mk_rep(550, "DELE command failed")
    end;

handle_command(<<"RNFR">>, ParamsBin, Args) ->
    FromName = ftpesd_util:binlist_to_string(ParamsBin),
    RelPath  = ftpesd_util:concat_paths(ftpesd_util:get_current_dir(FromName, Args), FromName),
    FullPath = ftpesd_util:get_full_path(RelPath, Args),
    NewArgs  = Args#ctrl_conn_data{ rename_from = FullPath },
    mk_rep(350, "Requested file action pending further information.", NewArgs);

handle_command(<<"RNTO">>, ParamsBin, Args) ->
    ToName = ftpesd_util:binlist_to_string(ParamsBin),
    RelPath  = ftpesd_util:concat_paths(ftpesd_util:get_current_dir(ToName, Args), ToName),
    ToPath = ftpesd_util:get_full_path(RelPath, Args),
    case Args#ctrl_conn_data.rename_from of
        none ->
            mk_rep(503, "RNTO command failed, RNFR required before");
        FromPath ->
            NewArgs = Args#ctrl_conn_data{ rename_from = none },
            UserArgs  = Args#ctrl_conn_data.user_data_params,
            case ftpesd_dir:rename(FromPath, ToPath, UserArgs) of
                ok -> mk_rep(250, "RNTO ok", NewArgs);
                {error, eacces} -> mk_rep(550, "RNTO command failed, no access");
                _  -> mk_rep(550, "RNTO command failed", NewArgs)
            end
    end;

%%----EXTENSIONS----%%

handle_command(<<"FEAT">>, [], _) ->
    mk_rep(211, {feat, "Features:\r\n" ++ ftpesd_util:concat_extensions() ++ "211 End"});

handle_command(<<"OPTS">>, [], _) ->
    mk_rep(501, "Syntax error!");

handle_command(<<"OPTS">>, [Command], _) ->
    StringCommand = string:to_upper(binary_to_list(Command)),
    case lists:keyfind(StringCommand,1, ftpesd_util:opts_enabled()) of
        {_Command, _Params} -> mk_rep(500, "Opts ok, but not implemented!");
        false -> mk_rep(501, "Option not understood!")
    end;

handle_command(<<"OPTS">>, [Command, Params], _) ->
    StringCommand = string:to_upper(binary_to_list(Command)),
    case lists:keyfind(StringCommand,1, ftpesd_util:opts_enabled()) of
        {_Command, ExistingParams} ->
            ParamsList = re:split(binary_to_list(Params),"(;)",[{return,list},group, trim]),
            case lists:all(fun(T)-> lists:member(string:join(T,""), ExistingParams) end, ParamsList) of
                true -> mk_rep(500, "Opts ok, but not implemented!");
                false -> mk_rep(501, "Option not understood!")
            end;
        false -> mk_rep(501, "Option not understood!")
    end;

handle_command(<<"">>, _, _) ->
    mk_rep(500, "Invalid command");


%%----SECURITY EXTENSIONS----%%

handle_command(<<"AUTH">>, [SecurType], Args) ->
    StringSecurType = string:to_upper(binary_to_list(SecurType)),
    case lists:member(StringSecurType, ftpesd_util:supported_security_protocols()) of
        true ->
            NewArgs = Args#ctrl_conn_data{auth_phase = next_auth_state(none)},
            self() ! start_ssl,
            mk_rep(234, "Security mechanism supported!", NewArgs);
        _ -> 
            mk_rep(504, "Security mechanism not understood!")
    end;

handle_command(<<"PBSZ">>, [<<"0">>], #ctrl_conn_data{auth_phase = negotiation_done, authed = Authed} = Args) ->
    NewArgs = Args#ctrl_conn_data{auth_phase = next_auth_state({negotiation_done, Authed}, "PBSZ")},
    mk_rep(200, "Protected buffer size set to 0", NewArgs);

handle_command(<<"PBSZ">>, _, #ctrl_conn_data{auth_phase = negotiation_done})->
    mk_rep(501, "Unsupported protected buffer size!");

handle_command(<<"PBSZ">>, _, _Args) ->
    mk_rep(503, "Security data exchange not done!");

handle_command(<<"PROT">>, [ProtLevel], #ctrl_conn_data{auth_phase = pbsz_done, authed = Authed} = Args) ->
    StringProtLevel = string:to_upper(binary_to_list(ProtLevel)),
    case lists:member(StringProtLevel, ftpesd_util:protection_levels()) of
        true ->
            case StringProtLevel of
                "P" ->
                    NewArgs = Args#ctrl_conn_data{auth_phase = next_auth_state({pbsz_done, Authed}, "PROT")},
                    mk_rep(200, "Protection level set to private", NewArgs);
                _ ->
                    mk_rep(534, "Protection level rejected!")
            end;
        _ ->
            mk_rep(504, "Protection level not understood!")
    end;

handle_command(<<"PROT">>, _, _Args) ->
    mk_rep(503, "Security data exchange not done!");

handle_command(Command, _, _) ->
    case lists:member(Command, ftpesd_util:implemented_commands()) of
        true  -> mk_rep(501, "Invalid number of arguments");
        false -> mk_rep(502, binary_to_list(Command) ++ " not implemented")
    end.

handle_data_process_exit(_Sock, normal) ->
    ok; %% if process exited normally, reply is already sent
handle_data_process_exit(Sock, _Other) ->
    handle_reply(Sock, ?RESP(426, "Connection closed; transfer aborted")).

construct_conn_data(Args, Sock, DSock, Pid) ->
    case lists:member(inet6, Args) of
        true -> 
            IPV = inet6;
        _-> IPV = inet
    end,
    RootDir = proplists:get_value(chrootDir, Args, ?DEFAULT_ROOT_DIR),
    TlsOptions = proplists:get_value(tls_options, Args),
    IpAddress = proplists:get_value(ip, Args),
    IpOptions = proplists:get_value(ip_options, Args),
    TcatDn = proplists:get_value(trusted_category, Args),
    BaseDir = re:replace(RootDir, "\/$", "", [{return, list}]), %% trim / at end,
    IdleTimer = get_idle_timer_value(Args),
    
    Data    = #ctrl_conn_data{ control_socket = Sock, data_socket = DSock, chrootdir = BaseDir, inet = IPV, tls_options = TlsOptions,
                               ip = IpAddress, ip_options = IpOptions, listener_pid = Pid, trusted_category = TcatDn, idle_timer = IdleTimer},
    lists:foldl(fun fold_args/2, Data, Args).

fold_args(_, Data) ->
    Data.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checks if given command meets current authentication phase.
%% If it does, then command is executed.  
%%% ----------------------------------------------------------
-spec process_message(Sock :: ftp_socket(), Command :: bitstring(), Message :: [bitstring()],
                      Args :: #ctrl_conn_data{}) -> #ctrl_conn_data{}.
%%% ###=====================================================================###
process_message(Sock, Command, Msg, State) ->
    case ftpesd_util:check_auth(Command, State) of
        ok ->
            {Reply, MaybeNewArgs} = handle_command(Command, Msg, State),
            handle_reply(Sock, Reply),
            case MaybeNewArgs of
                {newargs, NewArgs} -> NewArgs;
                sameargs       -> State
            end;
        bad ->
            handle_reply(Sock, ?RESP(530, "Not logged in")),
            State;
        nok ->
            handle_reply(Sock, ?RESP(503,  "Bad sequence of commands")),
            State
    end.

%% comment: doc

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Constructs response (special clause for FEAT command)
%%% ----------------------------------------------------------
-spec mk_rep(Code :: integer(), Message :: string() | {feat, string()}) -> tuple().
%%% ###=====================================================================###
mk_rep(Code, Message) ->
    {?RESP(Code, Message), sameargs}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Constructs response with new arguments (special clause for FEAT command) 
%%% ----------------------------------------------------------
-spec mk_rep(Code :: integer(), Message :: string() | {feat, string()}, 
             NewArgs :: #ctrl_conn_data{}) -> tuple().
%%% ###=====================================================================###
mk_rep(Code, Message, NewArgs) ->
    {?RESP(Code, Message), {newargs, NewArgs}}.

handle_reply(_, noreply) ->
    ok;
handle_reply(Sock, {reply, Code, Message}) ->
    ftpesd_util:send_reply(Sock, Code, Message).

close_socket(Sock) when is_port(Sock)->
    gen_tcp:close(Sock);
close_socket(Sock) when is_tuple(Sock) ->
    ssl:close(Sock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc
%%% 
%%% ----------------------------------------------------------
-spec check_addr_port(Addr :: tuple(), Port :: number()) -> ok | nok.
%%% ###=====================================================================###
check_addr_port(Addr, Port) ->
    A = element(1, Addr),
    B = element(2, Addr),
    C = element(3, Addr),
    D = element(4, Addr),
    List = [A, B, C, D],
    Acc = number_in_range(List),
    case Acc of
        4 -> 
            case port_in_range(Port) of
                ok ->
                    ?LOG("Valid IP address and Port: ~p:~p~n", [Addr, Port]), 
                    ok;
                _-> 
                    ?LOG("Valid IP address, invalid Port ~p~n", [Port]),
                    nok
            end;
        _-> 
            ?LOG("Invalid IP address: ~p~n", [Addr]),
            nok
    end.

%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc
%%% Verifies if given address is valid. 
%%% 
%%% ----------------------------------------------------------
-spec number_in_range(List :: list()) -> number().
%%% ###=====================================================================###
number_in_range(List) ->
    number_in_range(0, List).

%%% ###=====================================================================###
-spec number_in_range(Acc :: number(), List :: list()) -> number().
%%% ###=====================================================================###
number_in_range(Acc, [H|T]) when H >=0 ->
    if H=< 255 ->
           number_in_range(Acc+1, T);
       true -> 
           number_in_range(Acc, T)
    end;
number_in_range(Acc, []) ->
    Acc.

%%% ----------------------------------------------------------
%%% @doc
%%% Verifies if given port is valid. 
%%% 
%%% ----------------------------------------------------------
-spec port_in_range(Port :: number()) -> ok | nok.
%%% ###=====================================================================###
port_in_range(Port) when Port >= 1024 ->
    if Port =< 65535 ->
           ?LOG("Port: ~p~n", [Port]),
           ok;
       true -> nok
    end;

port_in_range(Port) when Port <1024 ->
    nok.

%%% ----------------------------------------------------------
%%% @doc
%%% Starts passive mode.
%%% 
%%% ----------------------------------------------------------
-spec start_passive_mode_EPASV(Args :: #ctrl_conn_data{}) -> tuple().
%%% ###=====================================================================###
start_passive_mode_EPASV(Args) ->
    ftpesd_data_conn:reinit_data_conn(Args),
    Ipv = Args#ctrl_conn_data.inet,
    TlsOptions = Args#ctrl_conn_data.tls_options,
    DSock = Args#ctrl_conn_data.data_socket,
    case ftpesd_data_conn:start_passive_mode(Ipv, TlsOptions, DSock) of
        {ok, {PasvPid, Ref, Port}} ->
            RspStr = "Entering Extended Passive Mode (|||"
                         ++ integer_to_list(Port) ++ "|)",
            NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid, data_proc_ref = Ref, type = passive},
            mk_rep(229, RspStr, NewArgs);
        {error, _} ->
            mk_rep(500, "EPSV command failed (1)")
    end.

%%% ----------------------------------------------------------
%%% #    get_ipv/1
%%% @doc 
-spec get_ipv(Args :: #ctrl_conn_data{}) -> inet | inet6.
%%% ----------------------------------------------------------
get_ipv(Args) ->
    Socket= Args#ctrl_conn_data.control_socket,
    case ssl:sockname(Socket) of
        {ok, {{_, _, _, _, _, _, _, _} = _IP, _}} -> inet6;
        {ok, {{_, _, _, _} = _IP, _}} -> inet
    end.

%% if there is no data connection, default timeout value is used
%% if the data transfer is ongoing, there is no timeout on control connection
-spec get_timeout(State :: #ctrl_conn_data{}) ->
          non_neg_integer() | infinity.
get_timeout(#ctrl_conn_data{data_proc_ref = none, idle_timer = IdleTimer}) ->
    IdleTimer;

get_timeout(_) ->
    infinity.

get_idle_timer_value(Args)->
    case proplists:get_value(idle_timer, Args) of
        undefined -> infinity;
        Integer -> Integer * 1000
    end.

abort_data_proc(none, _) ->
    ok;
abort_data_proc(DataPid, Ref) ->
    demonitor_data_proc(Ref),
    exit(DataPid, normal).

demonitor_data_proc(none) ->
    ok;
demonitor_data_proc(Ref) ->
    erlang:demonitor(Ref).

%% none --> started --> negotiation_done --> pbsz_done --> prot_done --> valid_seq
%% current state is a {auth_phase, authed}, second argument is a command
next_auth_state({_AuthPhase, _Authed} = CurrentState) ->
    next_auth_state(CurrentState, undefined);
next_auth_state(AuthPhase) ->
    next_auth_state({AuthPhase, false}).

next_auth_state({none, false}, _) ->
    started;
next_auth_state({started, false}, _) ->
    negotiation_done;
next_auth_state({negotiation_done, false}, "USER") ->
    negotiation_done; %% only authed changes
next_auth_state({negotiation_done, _}, "PBSZ") ->
    pbsz_done; 
next_auth_state({pbsz_done, false}, "PROT") ->
    prot_done; %% authed is false, waiting for user command
next_auth_state({pbsz_done, true}, "PROT") ->
    valid_seq; %% authed is already true, move to valid_seq
next_auth_state({prot_done, false}, "USER") ->
    valid_seq;
next_auth_state({valid_seq, true}, "REIN") ->
    prot_done; %% waiting for USER command
next_auth_state({AuthPhase, Authed}, Command) ->
    sysInitI:warning_msg("~p: Unexpected command sequence: {~p, ~p}: ~p", 
                         [?MODULE,AuthPhase,Authed,Command]),
    AuthPhase.

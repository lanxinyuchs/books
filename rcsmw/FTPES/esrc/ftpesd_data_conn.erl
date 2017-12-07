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
%%% %CCaseFile:	ftpesd_data_conn.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/6
%%%
%%% ----------------------------------------------------------
-module(ftpesd_data_conn).
-vsn('/main/R8A/R9A/6').
-date('2017-03-13').
-author('ekurnik').
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
%%% R8A/4    2016-11-24   eivmiha    editing code
%%% R8A/7    2016-12-19   ekurnik    Changed 'certificate' to 'tls_options'
%%% R8A/8    2017-01-09   emarnek    Changes because of intercept
%%% R8A/9    2017-01-10   ekurnik    Added absolute path support
%%% R9A/1    2017-01-23   ekurnik    Added linger option to data socket
%%% R9A/2    2017-01-24   ekurnik    Fixed warnings
%%% R9A/3    2017-01-27   ekurnik    Temporary fix of directory listing timing
%%%                                  issue which causes client to sometimes crash
%%% R9A/4    2017-02-10              Removed temporary fix
%%% R9A/4    2017-03-03   ekurnik    Added process monitoring of data_conn
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_passive_mode/3, 
         start_active_mode/5, 
         send_msg/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([pasv_accept/2, 
         actv_accept/1,
         reinit_data_conn/1]).

-include("ftpesd.hrl").
-define(ONE_CHUNK, 32*1024).
-type inet() :: inet | inet6.

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #    start_active_mode/5
%%% @doc Function for connecting to a given port and performs the SSL/TLS server-side
%%% handshake, spawns a process with SSL socket.
-spec start_active_mode(Ipv :: inet() , Addr :: inet:ip_address(), Port :: integer(),
                        TlsOptions :: tuple(), IpOptions :: list() ) -> {ok, pid()} | error_reason().
%%% ----------------------------------------------------------
start_active_mode(Ipv, Addr, Port, TlsOptions, IpOptions) ->
    Args = [binary, {packet, 0}, {active, false}, {linger, {true, ?LINGER_TIMEOUT}}]
               ++ ipv_argl(Ipv) ++ IpOptions,
    case gen_tcp:connect(Addr, Port, Args, infinity) of
        {ok, Socket} ->
            case  ssl:ssl_accept(Socket, TlsOptions) of
                {ok, SSLSocket} ->
                    Pid = spawn(?MODULE, actv_accept, [SSLSocket]),
                    Ref = erlang:monitor(process, Pid), %% monitor process
                    ?LOG("[~p]: Active mode started, port: ~p~n", [Pid, Port]),
                    {ok, {Pid, Ref}};
                Error -> Error
            end;
        Error -> Error
    end.

%%% ----------------------------------------------------------
%%% #   start_passive_mode/3
%% @doc Function that starts passive mode and spawns the accept process with listen 
%%% socket.
%%%
-spec start_passive_mode(Ipv :: inet(), TlsOptions :: tuple(), DSock :: ftp_socket()) -> {ok, {pid(), ftp_socket()}}
                                                                                             | error_reason().
%%% ----------------------------------------------------------
start_passive_mode(_Ipv, TlsOptions, DSock) ->
    Pid = spawn(?MODULE, pasv_accept, [DSock, TlsOptions]),
    Ref = erlang:monitor(process, Pid), %% monitor process
    case inet:port(DSock) of
        {ok, Port} ->
            ?LOG("[~p]: Passive mode started, port: ~p~n", [Pid, Port]),
            {ok, {Pid, Ref, Port}};
        Error -> Error
    end.



%%% ----------------------------------------------------------
%%% #    send_msg/3
%%% @doc If data type is passive, the function send a reply to the client wheater a data
%%% connection is established or not. If it is active, we connect on a given Port.
%%%
-spec send_msg(MsgType :: atom(), Msg :: string() | {string(), atom()} , Args :: #ctrl_conn_data{}) ->
          {reply(), argschange()}.               
%%% ----------------------------------------------------------          
send_msg(MsgType, Msg, Args) ->
    case Args#ctrl_conn_data.type of
        passive ->
            case Args#ctrl_conn_data.data_pid of
                none ->
                    send_ctrl_reply(Args, 500, "Data connection not established.");
                PasvPid ->
                    PasvPid ! accept,
                    send_ctrl_reply(Args, 150, "Opening BINARY mode data connection"),
                    PasvPid ! {MsgType, Msg, Args}
            end,
            {noreply, sameargs};
        active ->
            send_ctrl_reply(Args, 150, "Opening BINARY mode data connection"),
            ftpesd_data_conn:reinit_data_conn(Args),
            Addr = Args#ctrl_conn_data.address,
            Port = Args#ctrl_conn_data.port,
            Ipv= Args#ctrl_conn_data.inet,
            IpOptions = Args#ctrl_conn_data.ip_options,
            TlsOptions = Args#ctrl_conn_data.tls_options,
            case ftpesd_data_conn:start_active_mode(Ipv, Addr, Port, TlsOptions, IpOptions) of
                {ok, {ActvPid, Ref}} ->
                    NewArgs = Args#ctrl_conn_data{data_pid = ActvPid, data_proc_ref = Ref}, 
                    ActvPid ! {MsgType, Msg, NewArgs},
                    {noreply, {newargs, NewArgs}};
                {error, _Error} ->
                    send_ctrl_reply(Args, 500, "Data connection not established."),
                    {noreply, sameargs}
            end
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

actv_accept(Sock) ->
    ?LOG("ACTV accept start~n"),
    data_conn_main(Sock).

pasv_accept(LSock, TlsOptions) ->
    receive 
        accept ->
            ?LOG("PASV accept start~n"),
            {ok, Sock} = gen_tcp:accept(LSock),
            {ok, SSLSocket} = ssl:ssl_accept(Sock, TlsOptions),
            data_conn_main(SSLSocket);
        _ ->
            exit(unexpected_msg)
    after ?SERVER_SESSION_TIMEOUT ->
        exit(timeout)
    end.

reinit_data_conn(Args) ->
    case Args#ctrl_conn_data.data_pid of
        none    -> true;
        LastPid -> exit(LastPid, kill)
    end.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

ipv_argl(inet) -> [];
ipv_argl(inet6) -> [inet6].

%%% ----------------------------------------------------------
%%% #     data_conn_main/1
%%% @doc Function used for handling data commands list, retr, stor
-spec data_conn_main(DataSock :: ftp_socket()) -> ok | {error, error_reason()}.
%%% ---------------------------------------------------------- 
data_conn_main(DataSock) ->
    receive
        {list, FileNames, Args} ->
            TempMsg = [FN ++ "\r\n"|| FN <- FileNames],
            FormattedMsg = lists:flatten(TempMsg),
            ok = ftpesd_util:send_message(DataSock, FormattedMsg),
            transfer_complete(Args);
        {retr, FileName, Args} ->
            AbsPath = Args#ctrl_conn_data.chrootdir,
            UserArgs = Args#ctrl_conn_data.user_data_params,
            RelPath = ftpesd_util:get_current_dir(FileName, Args),
            FPath   = ftpesd_dir:canonicalize_path(AbsPath ++ ftpesd_util:concat_paths(RelPath,FileName)),
            case ftpesd_dir:open(FPath, [binary, read], UserArgs) of
                {ok, Id} ->
                    read_chunks("", Id, FileName, RelPath, DataSock, Args);
                _Error -> 
                    RespStr = "Requested action not taken. File unavailable, "
                              "not found, not accessible",
                    send_ctrl_reply(Args, 550, RespStr)
            end;
        {stor, {FileName, Mode}, Args} ->
            AbsPath = Args#ctrl_conn_data.chrootdir,
            RelPath = ftpesd_util:get_current_dir(FileName, Args),
            FPath   = ftpesd_dir:canonicalize_path(AbsPath ++ ftpesd_util:concat_paths(RelPath,FileName)),
            Repr    = Args#ctrl_conn_data.repr_type,
            case receive_and_store(DataSock, FPath, Mode, Repr, Args) of
                ok ->
                    transfer_complete(Args);
                {error, _} ->
                    RespStr = "Requested action not taken. File unavailable, "
                              "not found, not accessible",
                    send_ctrl_reply(Args, 550, RespStr)
            end
    end,
    graceful_close(DataSock),
    ?LOG("Data conn main loop end~n").

read_chunks(Data, FPath, FileName, RelPath, DataSock, Args) ->
    UserArgs  = Args#ctrl_conn_data.user_data_params,
    case ftpesd_dir:read(FPath, ?ONE_CHUNK, UserArgs) of
        {ok, Bin} ->
            NewData = Data ++ Bin,
            read_chunks(NewData, FPath, FileName, RelPath, DataSock, Args);
        eof ->
            BinT = ftpesd_util:transformto(Data, Args#ctrl_conn_data.repr_type),
            ok = ftpesd_util:send_message(DataSock, BinT),
            transfer_complete(Args);
        {error, eacces} ->
            RespStr = "Requested action not taken. File not accessible",
            send_ctrl_reply(Args, 550, RespStr);
        {error, _} ->
            RespStr = "Requested action not taken. File unavailable, "
                      "not found, not accessible",
            send_ctrl_reply(Args, 550, RespStr)
    end.

%% Receive binaries and store them in a file
receive_and_store(DataSock, FPath, Mode, ReprType, Args) ->
    UserArgs  = Args#ctrl_conn_data.user_data_params,
    case ftpesd_dir:open(FPath, [binary, Mode], UserArgs) of
        {ok, Id} ->
            Receive = receive_and_write_chunks(DataSock, Id, ReprType, UserArgs),
            Close   = ftpesd_dir:close(Id, UserArgs), 
            case {Receive, Close} of
                {ok, ok} -> ok;
                _	-> {error, receive_fail}
            end;
        Error -> Error
    end.

%%% ----------------------------------------------------------
%%% #   receive_and_write_chunks/4
%%% @doc  Function that receives a packet from a socket and writes it to io_device(). 
%%% Lenght is 0 so all available bytes are returned.
%%%
-spec receive_and_write_chunks(DataSock :: ftp_socket(), DevId :: io:device(), 
                               ReprType :: string(), UserArgs :: #user_data{} ) -> ok | {error, error_reason()}.
%%% ----------------------------------------------------------
receive_and_write_chunks(DataSock, DevId, ReprType, UserArgs) ->
    case ssl:recv(DataSock, 0) of
        {ok, Data} ->
            DataTransform = ftpesd_util:transformfrom(Data, ReprType),
            case ftpesd_dir:write(DevId, DataTransform, UserArgs) of
                ok -> receive_and_write_chunks(DataSock, DevId, ReprType, UserArgs);
                {error, eacces} -> {error, eacces}
            end;
        {error, closed} -> ok;
        {error, Reason} -> 
            {error, Reason}
    end.

transfer_complete(Args) ->
    send_ctrl_reply(Args, 226, "Transfer complete").

send_ctrl_reply(Args, Command, Msg) ->
    case Args#ctrl_conn_data.control_socket of
        none -> 
            ?LOG("Error: control connection lookup failed~n");
        Sock -> ftpesd_util:send_reply(Sock, Command, Msg)
    end.


graceful_close(DataSocket) ->
    %% finished writing
    ssl:shutdown(DataSocket, write),
    
    %% close socket
    ssl:close(DataSocket).


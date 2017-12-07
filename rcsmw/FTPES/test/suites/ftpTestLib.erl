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
%%% %CCaseFile:	ftpTestLib.erl %
%%% Author: 
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module (ftpTestLib).

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
%%% R8A/3    2016-11-23   emarnek    Setup_tls made from smaller functions
%%% R8A/4    2016-11-24   emarnek    Cleaned comments, added missing specs/docs
%%% R8A/7    2016-11-29   ekurnik    Redesigned setup_tls
%%% R8A/8    2016-11-30   estjako    Added get and set fun for cipher suite
%%% R8A/11   2016-12-06   ekurnik    Added support functions for OAM tests
%%% R8A/12   2016-12-09   eivmiha    Edited get_client_ip 
%%% R8A/16   2016-12-12   ekurnik    Added Ipv6 support
%%% R8A/19   2016-12-13   ekurnik    Added SIM support
%%% R8A/21   2016-12-20   ekurnik    Added Security log supporting functions
%%% R8A/22   2016-12-21   eivmiha    added start_server
%%% R8A/26   2016-12-23   ekurnik    Minor fix in start_server
%%% R8A/27   2017-01-04   ekurnik    Added support for register_sftp_dir
%%% R8A/28   2017-01-10   enekdav    Added NLST, RNFR and RNTO commands
%%% R8A/30   2017-01-10   ekurnik    Fixed minor issue with register_sftp_dir
%%%--------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include("ftpTestLib.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%% helper funs
-export([flush_messages/0,
         expected_timeout/0,
         get_client_ip/2
          ]).

%% FTP interface commands
-export([open_ftp_control_connection/3,
         tcp_connect/2, 
         tcp_connect/3,
         ssl_connect/3,
         ssl_connect_host/3,
         close_ftp_connection/1,
         ftp_user/3,
         ftp_ls/2, 
         ftp_ls/3,
         ftp_nls/2,
         ftp_nls/3,
         ftp_retr/3,
         ftp_retr_close/3,
         ftp_stor/3,
         ftp_rename/3,
         ftp_rnfr/2,
         ftp_rnto/2,
         ftp_stor_close/3,
         ftp_appe/3,
         ftp_appe_close/3]).

%% FTP over TCP generic commands
-export([send_FTP_command_over_TCP/3,
        recv_FTP_response_over_TCP/1,
        recv_FTP_response_over_TCP/2,
        send_recv_ftp_command/3,
        send_recv_ftp_command_sequence/2,
        send_specific_command_over_TCP/4]).

%% FTP over TCP specific commands
-export([send_user_command/2, 
         send_pass_command/2, 
         send_acct_command/2,
         send_port_command/3,
         send_port_command/4,
         send_pasv_command/1,
         send_pasv_command/2,
         send_list_command/1,
         send_list_command/2,
         send_nlst_command/1,
         send_nlst_command/2,
         send_retr_command/2,
         send_rnfr_command/2,
         send_rnto_command/2,
         send_stor_command/2]).

%% Simple common FTP commands
-export([send_quit/1,
         ftp_delete/2,
         ftp_rmdir/2,
         ftp_mkdir/2,
         ftp_size/2,
         ftp_cd/2,
         ftp_type/2, 
         ftp_pwd/1]).

-export([authentication_sequence/2, setup_control_connection/1]).
-export([open_ftp_data_connection/5]).
-export([get_node_ip/2, 
         get_node_ip/3,
         copy_files_to_node/3, 
         start_server/0,
         start_ftpes_sup/2,
         start_ftpes_server/1, 
         stop_ftpes_sup/1, 
         stop_ftpes_server/1,
         activate/1,
         initialize_tables/1]).

% TLS netconf commands
-export([% Creating, installing and deleting of all TLS parameters
         initialize_nc_tc/1,
         clean_nc_tc/1,
         setup_tls/1,
         get_all_node_credentials/1,
         get_all_trusted_certificates/1,
         create_trusted_certificate/1,
         % Creating, installing and deleting of one ore more TLS parameters
         prepare_files/1,
         create_install_NC_MO_credentials/1,
         create_NC_MO/1,
         install_credentials/1,
         create_trust_category/1,
         install_activate_trusted_certificate/1,
         set_trust_category/2,
         set_nc_tc/3,
         set_nc/2,
         set_tc/2,
         set_as/1,
         enable_ftpes_tls/1,
         disable_ftpes_tls/0,
         remove_node_credential/1,
         remove_trust_category/1,
         remove_trusted_certificates/1,
         exists_ftp_tls/0,
         exists_ftp_tls_server/0
        ]).

%% OAM netconf commands
-export([oam_exists/1,
         oam_exists/2,
         get_oam_ip/2,
         get_oam_ip/3,
         get_oam_ref/1,
         get_oam_ref/2,
         edit_oam_ip/3,
         edit_oam_ip/4,
         edit_oam_ref/2,
         edit_oam_ref/3,
         delete_oam_ref/1,
         delete_oam_ref/2,
         parse_address/1
         ]).

%% Cipher netconf commands
-export([
         set_tls_cipher_suites/1,
         get_tls_cipher_suites/0]).

%% config getters
-export([get_sftp_server_address/1,
         get_sftp_server_username/0,
         get_sftp_server_password/0,
         get_ftpes_test_server_address/1,
         get_ftpes_test_server_username/0,
         get_ftpes_test_server_password/0,
         get_oam_addr_ref/2,
         get_oam_ip_protocol/0]).

-export([is_target/0, 
         exec_command/2, 
         register_sftp_dir/4, 
         create_rop_file/2,
         delete_rop_file/1]).

-type ftp_socket() :: port() | ssl:sslsocket(). %% TCP or SSL+TCP socket
-type ftp_connection_type() :: tcp | ftp_client.
-type ftp_handler() :: ftp_socket() | pid().
-type ftp_response() :: string().
-type ftp_request() :: string().
-type ftp_send_command() :: fun().
-type ftp_data_conn_mode() :: active | passive.
-type error_reason() :: any().
-type ftp_data_connection() :: #ftpDataConnection{}.
-type ip_protocol() :: ipv4 | ipv4_ext | ipv6.
-type ip_address () :: tuple().
-type config_list() :: list().
-type rcp_handler() :: atom().

-define(is_socket(Socket), is_tuple(Socket);is_port(Socket)).

%% helper flush fun 
flush_messages() ->
    receive
        _ -> 
            flush_messages()
    after 0 ->
        ok
    end.

%% helper timeout function for invalid & wrong IP address
expected_timeout() ->
       receive 
           {error, timeout} -> ok
           
        after 15000 -> error
end. 

%% helper function for ipfamily option 
 ipv_argl(ipv4) -> [inet];
 ipv_argl(ipv4_ext) -> [inet];
 ipv_argl(ipv6) -> [inet6].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TCP functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tcp_connect/2
%% ====================================================================
%% @doc Wrapper for tcp_connect/3
%% Same as:
%%      tcp_connect(Host, Port, []).

-spec tcp_connect(Host :: inet:ip_address(), 
                  Port :: inet:port_number()) -> 
                    {ok, port()} | {error, error_reason()}.
%% ====================================================================
tcp_connect(Host, Port) ->
    tcp_connect(Host, Port, []).

%% tcp_connect/3
%% ====================================================================
%% @doc Wrapper for gen_tcp:connect/4

-spec tcp_connect(Host :: inet:ip_address(), 
                  Port :: inet:port_number(), 
                  Args :: inet:connect_option()) -> 
                    {ok, port()} | {error, error_reason()}.
%% ====================================================================
tcp_connect(Host, Port, Args) ->
    {ok, _Socket} = gen_tcp:connect(Host, Port, Args, 10000).

%% ssl_connect_host/3
%% ====================================================================
%% @doc Wrapper for ssl:connect/3 with TCP socket

-spec ssl_connect(Socket :: inet:port(),  
                       Options :: inet:connect_option(),
                       Timeout :: non_neg_integer()) -> 
                       {ok, ssl:sslsocket()} | {error, error_reason()}.
%% ====================================================================
ssl_connect(Socket, SslOptions, Timeout) ->
    {ok, _SSLSocket} = ssl:connect(Socket, SslOptions , Timeout).

%% ssl_connect_host/3
%% ====================================================================
%% @doc Wrapper for ssl:connect/3

-spec ssl_connect_host(Host :: inet:ip_address(), 
                       Port :: inet:port_number(), 
                       Options :: inet:connect_option()) -> 
                       {ok, ssl:sslsocket()} | {error, error_reason()}.
%% ====================================================================
 ssl_connect_host(Host, Port, Options) ->
    {ok, SSLSocket} = ssl:connect(Host, Port, Options, ?DEFAULT_TIMEOUT),
    {ok, SSLSocket}.

%% tcp_send/2
%% ====================================================================
%% @doc Wrapper for gen_tcp:send/2 and ssl:send/2

-spec tcp_send(Socket :: ftp_socket(), Msg :: iodata()) -> ok | {error, error_reason()}.
%% ====================================================================
tcp_send(Socket, Msg) when is_list(Msg) ->
    tcp_send(Socket, list_to_binary(Msg));

tcp_send(Socket, Msg) when is_port(Socket), is_binary(Msg) ->
    gen_tcp:send(Socket, Msg);

tcp_send(Socket, Msg) when is_tuple(Socket), is_binary(Msg) ->
    ssl:send(Socket, Msg).

%% tcp_recv/1
%% ====================================================================
%% @doc Wrapper for tcp_recv/2
%% Same as:
%%      tcp_recv(Socket, ?DEFAULT_TIMEOUT).

-spec tcp_recv(Socket :: ftp_socket()) -> list() | {error, error_reason()}.
%% ====================================================================
tcp_recv(Socket)  when ?is_socket(Socket)->
    tcp_recv(Socket, ?DEFAULT_TIMEOUT).

%% tcp_recv/2
%% ====================================================================
%% @doc Wrapper for receiving TCP and SSL messages

-spec tcp_recv(Socket :: ftp_socket(), Timeout :: non_neg_integer()) -> 
          list() | {error, error_reason()}.
%% ====================================================================
tcp_recv(Socket, Timeout) when is_tuple(Socket), is_integer(Timeout)->
    receive
        {ssl, Socket, Data} when is_binary(Data) ->
            binary_to_list(Data);
        {ssl, Socket, Data} ->
            Data;
        {ssl_closed, Socket} ->
            {error, ssl_closed};
        {ssl_error, Socket, Reason} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end;

tcp_recv(Socket, Timeout) when is_port(Socket), is_integer(Timeout)->
    receive
        {tcp, Socket, Data} when is_binary(Data) ->
            binary_to_list(Data);
        {tcp, Socket, Data} ->
            Data;
        {tcp_closed, Socket} ->
            {error, tcp_closed};
        {tcp_error, Socket, Reason} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end.

%% tcp_close/1
%% ====================================================================
%% @doc Wrapper for gen_tcp:close/1 and ssl:close/1

-spec tcp_close(Socket :: ftp_socket()) -> ok.
%% ====================================================================
tcp_close(Socket) when is_port(Socket) ->
    gen_tcp:close(Socket);

tcp_close(Socket) when is_tuple(Socket) ->
    ssl:close(Socket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FTP over TCP socket commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% send_FTP_command_over_TCP/3
%% ====================================================================
%% @doc Function which sends FTP command with arguments over TCP socket

-spec send_FTP_command_over_TCP(Socket :: ftp_socket(), 
                                Command :: iodata(), 
                                Args :: list()) ->
          ok | {error, error_reason()}.
%% ====================================================================
send_FTP_command_over_TCP(Socket, Command, Args) when ?is_socket(Socket) ->
    Msg = create_FTP_command(Command, Args),
    tcp_send(Socket, Msg).

%% recv_FTP_response_over_TCP/1
%% ====================================================================
%% @doc Wrapper for recv_FTP_response_over_TCP/2
%% Same as:
%%   recv_FTP_response_over_TCP(Socket, "").

-spec recv_FTP_response_over_TCP(Socket :: ftp_socket()) -> 
          {ok, ftp_response()} | {nok, ftp_response()} | {error, error_reason()}.
%% ====================================================================
recv_FTP_response_over_TCP(Socket) when ?is_socket(Socket)->
    recv_FTP_response_over_TCP(Socket, "").


%% recv_FTP_response_over_TCP/2
%% ====================================================================
%% @doc Function which receives response from FTP server and 
%% matches it with the expected response

-spec recv_FTP_response_over_TCP(Socket :: port(), ExpectedResponse :: string()) -> 
          {ok, ftp_response()} | {nok, ftp_response()} | {error, error_reason()}.
%% ====================================================================
recv_FTP_response_over_TCP(Socket, ExpectedResponse) when ?is_socket(Socket) -> 
    case tcp_recv(Socket) of
    Response when is_list(Response) ->
        ct:log("Received response message: ~p", [Response]),
        case check_ftp_response(Response, ExpectedResponse) of
        ok ->
            ct:log("Received response matches the expected: ~p", [Response]),
            {ok, Response};
        nok ->
            ct:log("Received response does not match the expected: ~p =/= ~p", 
                   [Response, ExpectedResponse]),
            {nok, Response}
        end;
    {error, Reason} ->
        ct:log(lightred, "Failed to receive message: ~p", [Reason]),
        {error, Reason}
    end.


%% send_user_command/2
%% ====================================================================
%% @doc Function which sends USER command over TCP

-spec send_user_command(Handler :: ftp_handler(), Username :: iodata()) ->
    ok | {error, error_reason()}.
%% ====================================================================
send_user_command(Handler, Username) when ?is_socket(Handler)-> 
    send_FTP_command_over_TCP(Handler, "USER", [Username]).

%% send_pass_command/2
%% ====================================================================
%% @doc Function which sends PASS command over TCP

-spec send_pass_command (Handler :: ftp_handler(), Password :: iodata()) ->
    ok | {error, error_reason()}.
%% ====================================================================
send_pass_command (Handler, Password) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "PASS", [Password]).

%% send_acct_command/2
%% ====================================================================
%% @doc Function which sends ACCT command over TCP

-spec send_acct_command (Handler :: ftp_handler(), Account :: iodata()) ->
    ok | {error, error_reason()}.
%% ====================================================================
send_acct_command (Handler, Account) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "ACCT", [Account]).

%% send_port_command/3
%% ====================================================================
%% @doc Function which sends PORT command over TCP

-spec send_port_command(Handler :: ftp_handler(), 
                        Host :: inet:ip_address(), 
                        Port :: inet:port_number()) ->
                            ok | {error, error_reason()}.
%% ====================================================================
send_port_command(Handler, Address, Port) when ?is_socket(Handler)->
    send_port_command(Handler, Address, Port, ipv4).


%% send_port_command/4
%% ====================================================================
%% @doc Function which sends PORT or EPRT command over TCP

-spec send_port_command(Handler :: ftp_handler(), 
                        Host :: inet:ip_address(), 
                        Port :: inet:port_number(),
                        Ip_protocol :: ip_protocol()) ->
                            ok | {error, error_reason()}.
%% ====================================================================
send_port_command(Handler, {_, _, _, _} = IpAddress, Port, ipv4) when ?is_socket(Handler)->
    HostS = string:join(lists:map(fun(N) -> integer_to_list(N) end, tuple_to_list(IpAddress)), ","),
    PortS = string:join([integer_to_list(Port div 256), integer_to_list(Port rem 256)], ","),
    Arg = HostS ++ "," ++ PortS,
    send_FTP_command_over_TCP(Handler, "PORT", [Arg]);

send_port_command(Handler, {_, _, _, _} = IpAddress, Port, ipv4_ext) when ?is_socket(Handler)->
    HostS = string:join(lists:map(fun(N) -> integer_to_list(N) end, tuple_to_list(IpAddress)), "."),
    PortS = integer_to_list(Port),
    Arg = "|1|" ++ HostS ++ "|" ++ PortS ++ "|",
    send_FTP_command_over_TCP(Handler, "EPRT", [Arg]);

send_port_command(Handler, {_, _, _, _, _, _, _, _} = IpAddress, Port, ipv6) when ?is_socket(Handler)->
    HostS = inet:ntoa(IpAddress),
%%     HostS = string:join(lists:map(fun(N) -> integer_to_list(N) end, tuple_to_list(IpAddress)), ":"),
    PortS = integer_to_list(Port),
    Arg = "|2|" ++ HostS ++ "|" ++ PortS ++ "|",
    send_FTP_command_over_TCP(Handler, "EPRT", [Arg]).

%% send_pasv_command/1
%% ====================================================================
%% @doc Function which sends PASV command over TCP

-spec send_pasv_command(Handler :: ftp_handler()) ->
                            ok | {error, error_reason()}.
%% ===================================================================
send_pasv_command(Handler) when ?is_socket(Handler)->
    send_pasv_command(Handler, ipv4).

%% send_pasv_command/2
%% ====================================================================
%% @doc Function which sends PASV or EPSV command over TCP

-spec send_pasv_command(Handler :: ftp_handler(),
                        Ip_protocol :: ip_protocol()) ->
                            ok | {error, error_reason()}.
%% ===================================================================
send_pasv_command(Handler, ipv4) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "PASV", []);

send_pasv_command(Handler, ipv4_ext) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "EPSV", ["1"]);

send_pasv_command(Handler, ipv6) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "EPSV", ["2"]).

%% get_expected_pasv_response_code/1
%% ====================================================================
%% @doc Returns expected positive response for PASV/EPASV command

-spec get_expected_pasv_response_code(Ip_protocol :: ip_protocol()) ->
                                        list().
%% ===================================================================
get_expected_pasv_response_code(ipv4) ->
    ?FTP_RESPONSE_COMMAND_SUCCESSFUL_PASV;
get_expected_pasv_response_code(ipv4_ext) ->
    ?FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV;
get_expected_pasv_response_code(ipv6) ->
    ?FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV;
get_expected_pasv_response_code(_) ->
    throw(unsupported_ip_proto).

%% send_list_command/1
%% ====================================================================
%% @doc Function which sends LIST command over TCP

-spec send_list_command(Handler :: ftp_handler()) -> ok | {error, error_reason()}.
%% ====================================================================
send_list_command(Handler) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "LIST", []).

%% send_list_command/2
%% ====================================================================
%% @doc Function which sends LIST command with directory parameter over TCP

-spec send_list_command(Handler :: ftp_handler(), Directory :: iodata()) -> 
          ok | {error, error_reason()}.
%% ====================================================================
send_list_command(Handler, Directory) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "LIST", [Directory]).

%% send_nlst_command/1
%% ====================================================================
%% @doc Function which sends NLST command over TCP

-spec send_nlst_command(Handler :: ftp_handler()) -> ok | {error, error_reason()}.
%% ====================================================================
send_nlst_command(Handler) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "NLST", []).

%% send_nlst_command/2
%% ====================================================================
%% @doc Function which sends NLST command with directory parameter over TCP

-spec send_nlst_command(Handler :: ftp_handler(), Directory :: iodata()) -> 
          ok | {error, error_reason()}.
%% ====================================================================
send_nlst_command(Handler, Directory) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "NLST", [Directory]).

%% send_recv_ftp_command/3
%% ====================================================================
%% @doc Function which executes an FTP send_command and verifies the response.
%% It returns a response or an error with reason.

-spec send_recv_ftp_command(Handler :: ftp_handler(), 
                            SendCommandFun :: ftp_send_command(), 
                            ExpectedResponse :: iodata()) ->
                                {ok, ftp_response()} | {error, error_reason()}.
%% ====================================================================
send_recv_ftp_command(Handler, SendCommandFun, ExpectedResponse)  when ?is_socket(Handler)->
    case SendCommandFun() of
    ok ->
        case recv_FTP_response_over_TCP(Handler, ExpectedResponse) of
        {ok, Response} ->
            {ok, Response};
        {nok, Response} ->
            {error, "Response does not match the expected: " ++ Response};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
        end;
    {error, Reason} ->
        {error, Reason};
    Other ->
        {error, Other}
    end.

%% send_recv_ftp_command_sequence/2
%% ====================================================================
%% @doc Function which executes a command sequence of FTP send_command functions.
%% Returns a list of responses or error with reason

-spec send_recv_ftp_command_sequence(Handler :: ftp_handler(), 
                            SendCommandFunSeq :: [{ftp_send_command(), string()}]) ->
                                {ok, [ftp_response()]} | {error, error_reason()}.
%% ====================================================================
send_recv_ftp_command_sequence(Handler, SendCommandFunSeq) when ?is_socket(Handler) -> 
    ResultList = [send_recv_ftp_command(Handler, SendCommandFun, ExpectedResponse) 
                    || {SendCommandFun, ExpectedResponse} <- SendCommandFunSeq],
    case lists:keyfind(error, 1, ResultList) of
    {error, Reason} ->
        {error, Reason};
    false ->
        ResponseList = [FtpResponse || {ok, FtpResponse} <- ResultList],
        {ok, ResponseList}
    end.


%% send_specific_command_over_TCP/4
%% ====================================================================
%% @doc Function which executes a specific single command over TCP

-spec send_specific_command_over_TCP(Handler :: ftp_handler(), 
                                    FTPCommand :: string(),
                                    Arguments :: [string()],
                                    ResponseCode :: string()) ->
                                {ok, [ftp_response()]} | {error, error_reason()}.
%% ====================================================================
send_specific_command_over_TCP(Handler, FTPCommand, Arguments, ResponseCode) when ?is_socket(Handler)->
    send_recv_ftp_command(Handler,
                          fun() -> ftpTestLib:send_FTP_command_over_TCP(
                                     Handler,
                                     FTPCommand,
                                     Arguments)
                          end,
                          ResponseCode).
    

%% ====================================================================

%% open_ftp_data_connection/5
%% ====================================================================
%% @doc Function which acts as a data receiver in FTP, should be started
%% in a separate process. Returns received data or error with reason
%% to the specified CallbackPid.

-spec open_ftp_data_connection(Mode :: ftp_data_conn_mode(),
                               Host :: inet:ip_address(),
                               Port :: inet:port_number(),
                               Args :: list(),
                               CallbackPid :: pid()) -> ok.
%% ====================================================================
open_ftp_data_connection(active, _Host, Port, Args, CallbackPid) ->
   Ipv = element(1, Args),
   Certificate = element(3, Args),
   SockArgs = [binary, {packet, 0}, {active, true}, {reuseaddr, true} ]++ ipv_argl(Ipv),
   Response =  
   case gen_tcp:listen(Port, SockArgs) of
   {ok, LSock} -> 
      case gen_tcp:accept(LSock, ?DEFAULT_TIMEOUT) of
        {ok, Sock} ->
            ct:log("Connection request accepted on a listening socket."),
            {ok, SSLSocket} = ssl:connect(Sock, Certificate, ?DEFAULT_TIMEOUT),
            tcp_close(LSock),
            ct:log("Connected socket upgraded to an SSL socket."),
            ftp_data_connection_loop(SSLSocket, CallbackPid);
        {error, Reason} ->
            tcp_close(LSock),
            ct:log("TCP not accepted: ~p",[Reason]),
            {error, Reason}
       end;
    {error, Reason} ->
        ct:log("Listen failed: ~p", [Reason]),
        {error, Reason}
    end,
    CallbackPid ! Response,
    ok;

open_ftp_data_connection(passive, Host, Port, Certificate, CallbackPid) ->
    Response =
    case ssl_connect_host(Host, Port, Certificate) of
    {ok, DataSocket} ->  %sslsocket
        ftp_data_connection_loop(DataSocket, CallbackPid);
    {error, Reason} -> 
        {error, Reason}
    end,
    CallbackPid ! Response,
    ok;

open_ftp_data_connection(Mode, _, _, _, CallbackPid) ->
    ct:log(lightred, "Unsupported data connection mode: ~p", [Mode]),
    CallbackPid ! {error, io_lib:format("Unsupported data connection mode: ~p", [Mode])},
    ok.

%% ftp_data_connection_loop/2
%% ====================================================================
%% @doc Function which is used by ftp_data_connection process.
%% It receives data and sends it to callback process.

-spec ftp_data_connection_loop(Socket :: port(), CallbackPid :: pid()) ->
    any().
%% ====================================================================
ftp_data_connection_loop(Socket, CallbackPid) when is_port(Socket) ->
    receive
        {tcp, Socket, Data} ->
            CallbackPid ! Data,
            ct:log("Retrieving data ~p",[Data]),
            ftp_data_connection_loop(Socket, CallbackPid);
        {tcp_closed, Socket} ->
            ct:log("TCP closed!"),
            tcp_close(Socket),
            tcp_closed;
        {tcp_error, Socket, Reason} ->
               tcp_close(Socket),
            {error, {tcp_error, Reason}};
      
        {msg, Data} ->
            gen_tcp:send(Socket, Data),
            ct:log("Storing data ~p",[Data]),
            ftp_data_connection_loop(Socket, CallbackPid);
        stop ->
            tcp_close(Socket)
    end;

ftp_data_connection_loop(Socket, CallbackPid) when is_tuple(Socket)->
    receive
        {ssl, Socket, Data} ->
            CallbackPid ! Data,
            ct:log("Retrieving data ~p",[Data]),
            ftp_data_connection_loop(Socket, CallbackPid);
        {ssl_closed, Socket} ->
            ct:log("SSL socket closed!"),
               tcp_close(Socket),
            ssl_closed;
        {ssl_error, Socket, Reason} ->
            {error, {ssl_error, Reason}};
        {msg, Data} ->
            ssl:send(Socket, Data),
            ct:log("Storing data ~p",[Data]),
            ftp_data_connection_loop(Socket, CallbackPid);
        stop ->
            tcp_close(Socket),
               tcp_close(Socket)
    end.

%% execute_ftp_data_command/5
%% ====================================================================
%% @doc Function which executes ftp data command. It does initial commands 
%% (e.g PORT) and then executes specific data command. It then expects
%% transfer stared and transfer finished messages on control port.

-spec execute_ftp_data_command(Handler :: ftp_handler(),
                                FtpDataSendCommand :: ftp_send_command(), 
                                DataConnectionParams :: ftp_data_connection(),
                                Command :: command) -> 
           {ok, string()} | {error, error_reason()}.
%% ====================================================================
execute_ftp_data_command(Handler, FtpDataSendCommand, #ftpDataConnection{mode = active, ip_protocol = Ip_proto,
                                                      port = Port, data_dir = DataDir, certificate = Certificate}, Command) when ?is_socket(Handler)->
    CallbackPid = self(),
    
    %% starting a data connection process
    DataConnectionPid = spawn(fun() -> open_ftp_data_connection(active, undefined, Port, {Ip_proto, DataDir, Certificate}, CallbackPid) end),
    timer:sleep(100),
    %% sending PORT command
    {ok, _} = send_recv_ftp_command(Handler, fun() -> send_port_command(Handler, get_client_ip(Handler, Ip_proto), Port, Ip_proto) end,  ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    %% sending data command 
    {ok, _} = send_recv_ftp_command(Handler, fun() -> FtpDataSendCommand() end, ?FTP_RESPONSE_OPENING_DATA_CONNECTION), 
  
    data_command_worker(Handler, Command, DataConnectionPid);
 
%% execute ftp_data_command pasive
execute_ftp_data_command(Handler, FtpDataSendCommand, #ftpDataConnection{mode = passive, 
                         host = Host, ip_protocol = Ip_proto, data_dir = _DataDir, certificate = Certificate}, Command) when ?is_socket(Handler)->
    CallbackPid = self(),
    {IpAddress, Port} =
    case get_pasv_data_host_port(Handler, Ip_proto) of
    {_, _} = HostPort ->
        HostPort;
    Port1 when is_integer(Port1) ->
        {Host, Port1};
    Invalid->
         throw(Invalid)
    end,
    DataConnectionPid = spawn(fun() -> open_ftp_data_connection(passive, IpAddress, Port, Certificate, CallbackPid) end), 
    timer:sleep(100),
    {ok, _} = send_recv_ftp_command(Handler, fun() -> FtpDataSendCommand() end, ?FTP_RESPONSE_OPENING_DATA_CONNECTION), 
    data_command_worker(Handler, Command, DataConnectionPid).
    
%% get_pasv_data_host_port/2
%% ====================================================================
%% @doc Function that is used as first step in obtaining host address and port.

-spec get_pasv_data_host_port(Handler :: ftp_handler(), Ip_proto :: ip_protocol()) -> 
          {ip_address(), non_neg_integer()}.
%% ====================================================================
get_pasv_data_host_port(Handler, Ip_proto) when ?is_socket(Handler) ->
    {ok, Msg} = send_recv_ftp_command(Handler, fun() -> send_pasv_command(Handler, Ip_proto) end,  get_expected_pasv_response_code(Ip_proto)),
    extract_data_host_port(Msg, Ip_proto).

%% extract_data_host_port/2
%% ====================================================================
%% @doc Function which is used by get_pasv_data_host_port.
%% It parses received data and returns tuple with IP address and port number.

-spec extract_data_host_port(Msg :: iodata(), Ip_proto :: ip_protocol()) -> 
          {ip_address(), non_neg_integer()}.
%% ====================================================================
extract_data_host_port(Msg, ipv4) ->
    
    Begin = string:rstr(Msg,"("),
    End = string:rstr(Msg,")"),
    Content = string:substr(Msg ,Begin + 1, End - Begin - 1), 
    
    List=lists:map(fun(X) -> {Int, _} = string:to_integer(X), 
                    Int end, 
          string:tokens(Content, ",")),

    HostA = lists:nth(1, List),
    HostB = lists:nth(2, List),
    HostC = lists:nth(3, List),
    HostD = lists:nth(4, List),
    Port1 = lists:nth(5, List),
    Port2 = lists:nth(6, List),
    Port = Port1 * 256 + Port2,
    
    {{HostA, HostB, HostC, HostD}, Port};

%% Format: |||PORT_NUM|
extract_data_host_port(Msg, Ip_proto) when Ip_proto =:= ipv4_ext; Ip_proto =:= ipv6 ->
    Response = string:substr(Msg ,string:str(Msg, "|"), string:rstr(Msg, "|")),
    [Port | _] = string:tokens(Response, "|"),
    list_to_integer(Port);

extract_data_host_port(_, _) ->
    throw(not_supported).
    
%% execute_ftp_data_command/6
%% ====================================================================
%% @doc Unlike execute_ftp_data_command/5, this function uses 
%% data_command_worker/5 for simulating case of closed connection when 
%% retrieving file

-spec execute_ftp_data_command(Handler :: ftp_handler(),
                                FtpDataSendCommand :: ftp_send_command(), 
                                DataConnectionParams :: ftp_data_connection(),
                                Command :: command,
                                closed) -> 
           {ok, string()} | {error, error_reason()}.
%% ====================================================================
execute_ftp_data_command(Handler, FtpDataSendCommand, #ftpDataConnection{mode = active, ip_protocol = Ip_proto,
                                                      port = Port, data_dir = DataDir, certificate = Certificate}, Command, closed) when ?is_socket(Handler)->
    CallbackPid = self(),
    %% starting a data connection process
    DataConnectionPid = spawn(fun() -> open_ftp_data_connection(active, undefined, Port, {Ip_proto, DataDir, Certificate}, CallbackPid) end),
    timer:sleep(100),
    %% sending PORT command
    {ok, _} = send_recv_ftp_command(Handler, fun() -> send_port_command(Handler, get_client_ip(Handler, Ip_proto), Port, Ip_proto) end,  ?FTP_RESPONSE_COMMAND_SUCCESSFUL), 
    %% sending data command 
    ok = FtpDataSendCommand(),
    data_command_worker(Handler, Command, DataConnectionPid, closed);

%% execute ftp_data_command passive
execute_ftp_data_command(Handler, FtpDataSendCommand, #ftpDataConnection{mode = passive, 
                         host = Host, ip_protocol = Ip_proto, data_dir = _DataDir, certificate = Certificate}, Command, closed) when ?is_socket(Handler)->
    CallbackPid = self(),
    {IpAddress, Port} =
    case get_pasv_data_host_port(Handler, Ip_proto) of
    {_, _} = HostPort ->
        HostPort;
    Port1 when is_integer(Port1) ->
        {Host, Port1};
    Invalid->
         throw(Invalid)
    end,
 
    DataConnectionPid = spawn(fun() -> open_ftp_data_connection(passive, IpAddress, Port, Certificate, CallbackPid) end), 
    timer:sleep(100),
    {ok, _} = send_recv_ftp_command(Handler, fun() -> FtpDataSendCommand() end, ?FTP_RESPONSE_OPENING_DATA_CONNECTION), 
    data_command_worker(Handler, Command, DataConnectionPid, closed).

%% data_command_worker/3
%% ====================================================================
%% @doc Function is used for storing or appending data to a file.

-spec data_command_worker(Handler :: ftp_handler(),
                          Command :: command,
                          DataConnectionPid :: pid()) -> 
           {ok, string()} | {error, error_reason()}.
%% ====================================================================
data_command_worker(Handler,  Command, DataConnectionPid) ->
    case Command of
        stor ->
            DataConnectionPid ! {msg, "Test content for store! Hura55555\r\n"},
            DataConnectionPid ! stop;
        appe ->
            DataConnectionPid ! {msg, "Test content for append!\r\n"},
            DataConnectionPid ! stop;
        _ ->
            ok
    end,
    
    {ok, _} = recv_FTP_response_over_TCP(Handler, ?FTP_RESPONSE_TRANSFER_COMPLETED),
    
    receiving_looper([]).

%% data_command_worker/4
%% ====================================================================
%% @doc This function differs from data_command_worker/3 because
%% it closes tcp connection while transfering data to a file

-spec data_command_worker(Handler :: ftp_handler(),
                          Command :: command,
                          DataConnectionPid :: pid(),
                          closed) -> 
           {ok, string()} | {error, error_reason()}.
%% ====================================================================
data_command_worker(Handler,  Command, _DataConnectionPid, closed) ->
    case Command of
        stor ->
%%             DataConnectionPid ! {msg, "Test content for store! Hura\r\n"},
            tcp_close(Handler);
        appe ->
%%             DataConnectionPid ! {msg, "Test content for append! Hura\r\n"},
            tcp_close(Handler);
        _ ->
            tcp_close(Handler)
    end.

%% receiving_looper/1
%% ====================================================================
%% @doc Function which receives messages until it gets error, tcp_closed or
%% ftp_closed message 

-spec receiving_looper(List :: list()) -> 
          {ok, list()} | {ok, timeout}.
%% =================================================================
receiving_looper(List) ->
    receive
        {error, Reason} ->
            ct:fail("Error: ~p", [Reason]);
        tcp_closed ->
            {ok, List};
        ssl_closed ->
            {ok, List};
        Data ->
            ct:log("Received data: ~p", [Data]),
            receiving_looper(List ++ Data)
    after 5000 ->
        {ok, timeout}
    end.

%% send_retr_command/2
%% ====================================================================
%% @doc Function which sends RETR command with directory parameter over TCP

-spec send_retr_command(Handler :: ftp_handler(), Directory :: iodata()) -> 
          ok | {error, error_reason()}.
%% ====================================================================
send_retr_command(Handler, Directory) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "RETR", [Directory]).

%% send_rnfr_command/2
%% ====================================================================
%% @doc Function which sends RNFR command with directory parameter over TCP

-spec send_rnfr_command(Handler :: ftp_handler(), Directory :: iodata()) -> 
          ok | {error, error_reason()}.
%% ====================================================================
send_rnfr_command(Handler, Directory) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "RNFR", [Directory]).

%% send_rnto_command/2
%% ====================================================================
%% @doc Function which sends RNTO command with directory parameter over TCP

-spec send_rnto_command(Handler :: ftp_handler(), Directory :: iodata()) -> 
          ok | {error, error_reason()}.
%% ====================================================================
send_rnto_command(Handler, Directory) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "RNTO", [Directory]).


%% send_stor_command/2
%% ====================================================================
%% @doc Function which sends STOR command with directory parameter over TCP

-spec send_stor_command(Handler :: ftp_handler(), FileName :: iodata()) -> 
          ok | {error, error_reason()}.
%% ====================================================================
send_stor_command(Handler, FileName) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "STOR", [FileName]).


%% send_appe_command/2
%% ====================================================================
%% @doc Function which sends APPE command with directory parameter over TCP

-spec send_appe_command(Handler :: ftp_handler(), FileName :: iodata()) -> 
          ok | {error, error_reason()}.
%% ====================================================================
send_appe_command(Handler, FileName) when ?is_socket(Handler)->
    send_FTP_command_over_TCP(Handler, "APPE", [FileName]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple common FTP commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% send_quit/1
%% ====================================================================
%% @doc Sends QUIT ftp command. 

-spec send_quit(Handler :: ftp_connection_type()) -> ok.
%% ====================================================================
send_quit(Handler) when ?is_socket(Handler)->
    Response = ftpTestLib:send_recv_ftp_command(
                 Handler,
                 fun() -> ftpTestLib:send_FTP_command_over_TCP(
                            Handler, 
                            "QUIT", 
                            [])
                 end,
                 ?FTP_RESPONSE_SERVICE_CLOSING_CONTROL_CONNECTION),
    tcp_close(Handler),
    Response;

send_quit(Handler) when is_pid(Handler) ->
    case ftp:close(Handler) of
        ok -> ct:log("Successful close"),
            {ok, "Successful quit"};
        Error -> {error, Error}
    end.

%% ftp_delete/2
%% ====================================================================
%% @doc Sends DELE ftp command. 

-spec ftp_delete(Handler :: ftp_connection_type(), FileName :: string()) -> ok.
%% ====================================================================

ftp_delete(Handler, FileName) when ?is_socket(Handler)->
    ftpTestLib:send_recv_ftp_command(
      Handler,
      fun() -> ftpTestLib:send_FTP_command_over_TCP(
                 Handler,
                 "DELE",
                 [FileName])
      end,
      ?FTP_DIRECTORY_COMMAND_SUCCESSFUL);

ftp_delete(Handler, FileName) when is_pid(Handler) ->
    case ftp:delete(Handler, FileName) of
        ok -> ct:log("Successful delete"),
            {ok, "Successful delete"};
        Error -> {error, Error}
    end.

%% ftp_rename/3
%% ====================================================================
%% @doc Sends sequence of RNFR and RNTO commands. 

-spec ftp_rename(Handler :: ftp_connection_type(),
                 FileNameFrom :: string(),
                 FileNameTo :: string()) -> ok.
%% ====================================================================

ftp_rename(Handler, FileNameFrom, FileNameTo) when ?is_socket(Handler)->
    {error, _Error} = ftp_rnfr(Handler, FileNameFrom),
    {ok, _} = ftp_rnto(Handler, FileNameTo);

ftp_rename(Handler, FileNameFrom, FileNameTo) when is_pid(Handler) ->
    case ftp:rename(Handler, FileNameFrom, FileNameTo) of
        ok -> ct:log("Successful rename"),
            {ok, "Successful rename"};
        Error -> {error, Error}
    end.

%% ftp_rnfr/2
%% ====================================================================
%% @doc Sends RNFR ftp command. 

-spec ftp_rnfr(Handler :: ftp_connection_type(), FileName :: string()) -> ok.
%% ====================================================================

ftp_rnfr(Handler, FileName) when ?is_socket(Handler)->
    ftpTestLib:send_recv_ftp_command(
      Handler,
      fun() -> ftpTestLib:send_FTP_command_over_TCP(
                 Handler,
                 "RNFR",
                 [FileName])
      end,
      ?FTP_RESPONSE_OPENING_DATA_CONNECTION).

%% ftp_rnto/2
%% ====================================================================
%% @doc Sends RNTO ftp command. 

-spec ftp_rnto(Handler :: ftp_connection_type(), FileName :: string()) -> ok.
%% ====================================================================

ftp_rnto(Handler, FileName) when ?is_socket(Handler)->
    ftpTestLib:send_recv_ftp_command(
      Handler,
      fun() -> ftpTestLib:send_FTP_command_over_TCP(
                 Handler,
                 "RNTO",
                 [FileName])
      end,
      ?FTP_DIRECTORY_COMMAND_SUCCESSFUL).

%% ftp_rmdir/2
%% ====================================================================
%% @doc Sends RMD ftp command. 

-spec ftp_rmdir(Handler :: ftp_connection_type(), Dir :: string()) -> ok.
%% ====================================================================

ftp_rmdir(Handler, Dir) when ?is_socket(Handler) ->
    ftpTestLib:send_recv_ftp_command(
      Handler,
      fun() -> ftpTestLib:send_FTP_command_over_TCP(
                 Handler,
                 "RMD",
                 [Dir])
      end,
      ?FTP_DIRECTORY_COMMAND_SUCCESSFUL);

ftp_rmdir(Handler, Dir) when is_pid(Handler) ->
    case ftp:rmdir(Handler, Dir) of
        ok -> ct:log("Successful delete folder"),
            {ok, "Successful delete"};
        Error -> {error, Error}
    end.

%% ftp_mkdir/2
%% ====================================================================
%% @doc Sends MKD ftp command. 

-spec ftp_mkdir(Handler :: ftp_connection_type(), Dir :: string()) -> ok.
%% ====================================================================

ftp_mkdir(Handler, Dir) when ?is_socket(Handler)->
    ftpTestLib:send_recv_ftp_command(
      Handler,
      fun() -> ftpTestLib:send_FTP_command_over_TCP(
                 Handler,
                 "MKD",
                 [Dir])
      end,
      ?FTP_CREATING_DIRECTORY_SUCCESSFUL);

ftp_mkdir(Handler, Dir) when is_pid(Handler) ->
    case ftp:mkdir(Handler, Dir) of
        ok -> ct:log("Successful make folder"),
            {ok, "Successful make"};
        Error -> {error, Error}
    end.

%% ftp_size/2
%% ====================================================================
%% @doc Sends SIZE ftp command. 

-spec ftp_size(Handler :: ftp_connection_type(), Dir :: string()) -> ok.
%% ====================================================================

ftp_size(Handler, Dir) when ?is_socket(Handler)->
    ftpTestLib:send_recv_ftp_command(
      Handler,
      fun() -> ftpTestLib:send_FTP_command_over_TCP(
                 Handler,
                 "SIZE",
                 [Dir])
      end,
      ?FTP_SIZE).

%% ftp_cd/2
%% ====================================================================
%% @doc Sends CWD ftp command. 

-spec ftp_cd(Handler :: ftp_connection_type(), Dir :: string()) -> ok.
%% ====================================================================

ftp_cd(Handler, Dir) when ?is_socket(Handler)->
    case ftpTestLib:send_recv_ftp_command(
      Handler,
      fun() -> ftpTestLib:send_FTP_command_over_TCP(
                 Handler,
                 "CWD",
                 [Dir])
      end,
      ?FTP_DIRECTORY_COMMAND_SUCCESSFUL) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end;

ftp_cd(Handler, Dir) when is_pid(Handler) ->
    case ftp:cd(Handler, Dir) of
        ok -> ct:log("Successful CWD"),
            ok;
        Error -> {error, Error}
    end.

%% ftp_type/2
%% ====================================================================
%% @doc Sends TYPE ftp command. 

-spec ftp_type(Handler :: ftp_connection_type(), Type :: string()) -> ok.
%% ====================================================================
ftp_type(Handler, Type) when ?is_socket(Handler)->
    Response = ftpTestLib:send_recv_ftp_command(
                 Handler,
                 fun() -> ftpTestLib:send_FTP_command_over_TCP(
                            Handler, 
                            "TYPE", 
                            [Type])
                 end,
                 ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    tcp_close(Handler),
    Response;

ftp_type(Handler, Type) when is_pid(Handler) ->
    case ftp:type(Handler, Type) of
        ok -> ct:log("Successful TYPE change"),
            {ok, "Successful TYPE change"};
        Error -> {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FTP connection functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ====================================================================
%% @doc Function which opens an FTP control connection towards FTP server.
%% It accepts tcp and ftp_client as parameter

-spec open_ftp_control_connection(
        ConnectionType :: ftp_connection_type(),
        Host :: inet:ip_address(),
        Args :: list()) ->  {ok, ftp_handler()} | {error, error_reason()}.
%% ====================================================================
open_ftp_control_connection(tcp, Host, Args) ->
    
    %% Returns server port and sets client port
    {ServerPort, NewArgs} =
    case lists:keyfind(port, 1, Args) of
    {port, Port} ->
        {Port, lists:keydelete(port, 1, Args)};
    _ ->
        {?DEFAULT_SERVER_CONTROL_CONNECTION_PORT, Args}
    end,
    case tcp_connect(Host, ServerPort, [{active, true}, {packet, 0} | NewArgs]) of 
    {ok, Socket} ->
       
        case recv_FTP_response_over_TCP(Socket, ?FTP_RESPONSE_HELLO) of
        {ok, _Response} ->
            ct:log("Connected to FTP server ~p:~p via TCP socket.~n" 
                    "Connection parameters: ~p", [Host, ServerPort, NewArgs]),
            {ok, Socket};
        {nok, Response} ->
            ct:log(lightred, "Connected to FTP server ~p:~p via TCP socket but wrong response received: ~p.~n"
                              "Connection parameters: ~p~n", [Host, ServerPort, Response, NewArgs]),
            tcp_close( Socket),
            {error, "Received a wrong welcome response: " ++ Response};
        {error, Reason} ->
            ct:log(lightred, "Connected to FTP server ~p:~p via TCP socket but received no response: ~p.~n"
                              "Connection parameters: ~p~n", [Host, ServerPort, Reason, NewArgs]),
            tcp_close( Socket),
            {error, Reason}
        end;
    {error, Reason} ->
        ct:log(lightred, "Failed to connect to FTP server ~p:~p via TCP socket.~n"
                              "Connection parameters: ~p~nReason: ~p", [Host, ServerPort, NewArgs, Reason]),
        {error, Reason}
    end;

open_ftp_control_connection(ftp_client, Host, Args) ->
    {ok, _Dir} = file:get_cwd(),
    case ftp:open(Host, Args) of %% ++ [{debug, debug}]) of
    {ok, Pid} ->
        ct:log("Connected to FTP server ~p via FTP client.~n" 
                    "Connection parameters: ~p", [Host, Args]),
        {ok, Pid};
    {error, Reason} ->
        ct:log(lightred, "Failed to connect to FTP server ~p via FTP client.~n"
                              "Connection parameters: ~p~nReason: ~p", [Host, Args, Reason]),
        {error, Reason};
    Other ->
        Other
    end;

open_ftp_control_connection(ConnType, _Host, _Args) ->
    ct:log(lightred, "Unsupported connection type: ~p", [ConnType]),
    {error, io_lib:format("Unsupported connection type: ~p", [ConnType])}.

%% close_ftp_connection/1
%% ====================================================================
%% @doc Closes FTP connection. Wrapper for ftp:close/1 and tcp_close/1.

-spec close_ftp_connection(Handler :: ftp_connection_type()) -> ok.
%% ====================================================================
close_ftp_connection(Handler) when ?is_socket(Handler)->
    tcp_close(Handler);

close_ftp_connection(Handler) when is_pid(Handler) ->
    ftp:close(Handler).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface FTP commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ftp_user/3
%% ====================================================================
%% @doc Sends a sequence of USER and PASS commands to FTP server

-spec ftp_user(Handler :: ftp_handler(), 
               Username :: iodata(),
               Password :: iodata()) ->
    ok | {error, error_reason()}.
%% ====================================================================
ftp_user(Handler, Username, _Password) when ?is_socket(Handler) ->
    case send_recv_ftp_command(Handler,
                          fun() -> send_user_command(Handler, Username) end,
                          ?FTP_USER_OK) of
        {ok, _Resp} ->
            ok;
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
     end;
        

ftp_user(Handler, Username, Password) when is_pid(Handler) ->
    ftp:user(Handler, Username, Password).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface FTP data commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ftp_ls/2
%% ====================================================================
%% @doc Sends a LIST command to FTP server. The tcp handler uses 
%% execute_ftp_data_command function

-spec ftp_ls(Handler :: ftp_handler(), DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_ls(Handler, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_list_command(Handler) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, list);

%% Data connection parameteres are ignored in ftp_client calls
%% because the ftp_client keeps the state of data connection
ftp_ls(Handler, _DataConnectionParams) when is_pid(Handler)->
    ftp:ls(Handler).

%% ftp_ls/3
%% ====================================================================
%% @doc Sends a LIST command with directory argument to FTP server. 
%% The tcp handler uses execute_ftp_data_command function

-spec ftp_ls(Handler :: ftp_handler(), Directory :: string() ,
             DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_ls(Handler, Directory, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_list_command(Handler, Directory) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, list);

%% Data connection parameteres are ignored in ftp_client calls
%% because the ftp_client keeps the state of data connection
ftp_ls(Handler, Directory, _DataConnectionParams) when is_pid(Handler)->
     ftp:ls(Handler, Directory).

%% ftp_nls/2
%% ====================================================================
%% @doc Sends a NLST command to FTP server. The tcp handler uses 
%% execute_ftp_data_command function

-spec ftp_nls(Handler :: ftp_handler(), DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_nls(Handler, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_nlst_command(Handler) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, nlst);

%% Data connection parameteres are ignored in ftp_client calls
%% because the ftp_client keeps the state of data connection
ftp_nls(Handler, _DataConnectionParams) when is_pid(Handler)->
    ftp:nlist(Handler).

%% ftp_nls/3
%% ====================================================================
%% @doc Sends a NLST command with directory argument to FTP server. 
%% The tcp handler uses execute_ftp_data_command function

-spec ftp_nls(Handler :: ftp_handler(), Directory :: string() ,
             DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_nls(Handler, Directory, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_nlst_command(Handler, Directory) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, nlst);

%% Data connection parameteres are ignored in ftp_client calls
%% because the ftp_client keeps the state of data connection
ftp_nls(Handler, Directory, _DataConnectionParams) when is_pid(Handler)->
     ftp:nlist(Handler, Directory).

%% ftp_retr/3
%% ====================================================================
%% @doc Sends a RETR command with directory argument to FTP server. 
%% The tcp handler uses execute_ftp_data_command function

-spec ftp_retr(Handler :: ftp_handler(), Directory :: string() ,
             DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_retr(Handler, Directory, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_retr_command(Handler, Directory) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, retr);

%% Data connection parameteres are ignored in ftp_client calls
%% because the ftp_client keeps the state of data connection
ftp_retr(Handler, Directory, _DataConnectionParams) when is_pid(Handler)->
    case ftp:recv_bin(Handler, Directory) of
        {ok, Bin} -> 
            ct:log("Successfull retrieve"),
            {ok, Bin};
        Error -> {error, Error}
    end.

ftp_retr_close(Handler, Directory, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_retr_command(Handler, Directory) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, retr, close).


%% ftp_stor/3
%% ====================================================================
%% @doc Sends a STOR command with filename argument to FTP server. 

-spec ftp_stor(Handler :: ftp_handler(), FileName :: string() ,
             DataConnectionParams :: ftp_data_connection())->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================

ftp_stor(Handler, Directory, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler) ->
    FtpDataSendCommand = fun() -> send_stor_command(Handler, Directory) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, stor);

%% Data connection parameteres are ignored in ftp_client calls
%% because the ftp_client keeps the state of data connection
ftp_stor(Handler, FileName, _DataConnectionParams) when is_pid(Handler)->
    case ftp:send(Handler, FileName) of
        ok ->
            ct:log("Successful store"), {ok, "Successfull store"};
        Error -> {error, Error}
    end.

%% ftp_stor_close/3
%% ====================================================================
%% @doc Sends a STOR command with filename argument to FTP server. 

-spec ftp_stor_close(Handler :: ftp_handler(), FileName :: string() ,
             DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_stor_close(Handler, FileName, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_stor_command(Handler, FileName) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, stor, closed);

ftp_stor_close(Handler, _FileName, #ftpDataConnection{} = _DataConnectionParams) when is_pid(Handler)->
    case ftp:close(Handler) of
        ok -> {closed, "client closed connection"};
        Error -> {error, Error}
    end.


%% ftp_appe/3
%% ====================================================================
%% @doc Sends a APPE command with directory argument to FTP server. 
%% The tcp handler uses execute_ftp_data_command function

-spec ftp_appe(Handler :: ftp_handler(), Directory :: string() ,
             DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_appe(Handler, FileName, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_appe_command(Handler, FileName) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, appe);

%% Data connection parameteres are ignored in ftp_client calls
%% because the ftp_client keeps the state of data connection
ftp_appe(Handler, FileName, _DataConnectionParams) when is_pid(Handler)->
    case ftp:append(Handler, FileName) of
        ok -> ct:log("Successful append"),
            {ok, "Successfull append"};
        Error -> {error, Error}
    end.

%% ftp_appe_close/3
%% ====================================================================
%% @doc Sends a APPE command with directory argument to FTP server. 
%% The tcp handler uses execute_ftp_data_command function

-spec ftp_appe_close(Handler :: ftp_handler(), Directory :: string() ,
             DataConnectionParams :: ftp_data_connection()) ->
    {ok, ftp_response()} |{error, error_reason()}.
%% ====================================================================
ftp_appe_close(Handler, Directory, #ftpDataConnection{} = DataConnectionParams) when ?is_socket(Handler)->
    FtpDataSendCommand = fun() -> send_appe_command(Handler, Directory) end,
    execute_ftp_data_command(Handler, FtpDataSendCommand, DataConnectionParams, appe, closed);

ftp_appe_close(Handler, _Directory, #ftpDataConnection{} = _DataConnectionParams) when is_pid(Handler)->
    case ftp:close(Handler) of
        ok -> {closed, "client closed connection"};
        Error -> {error, Error}
    end.

%% ftp_pwd/1
%% ====================================================================
%% @doc Sends a PWD command with directory argument to FTP server. 
%% The tcp handler uses execute_ftp_data_command function
-spec ftp_pwd(Handler :: ftp_handler()) ->
    {ok, ftp_response()} |{error, error_reason()}.
ftp_pwd(Handler) when ?is_socket(Handler) ->
    send_specific_command_over_TCP(Handler, "PWD", [], ?FTP_CREATING_DIRECTORY_SUCCESSFUL);

ftp_pwd(Handler) when is_pid(Handler) ->
    case ftp:pwd(Handler) of
        {ok, Dir} -> 
            ct:log("Successful pwd"),
            {ok, Dir};
        Error -> {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% create_FTP_command/2
%% ====================================================================
%% @doc Wrapper for create_FTP_command/3

-spec create_FTP_command(FtpCommand :: iodata(), Args :: list()) ->
    ftp_request().
%% ====================================================================
create_FTP_command(FtpCommand, Args) ->
    create_FTP_command(FtpCommand, Args, FtpCommand).

%% create_FTP_command/3
%% ====================================================================
%% @doc Function which creates FTP command

-spec create_FTP_command(FtpCommand :: iodata(), Args :: list(), Result :: string()) ->
    ftp_request().
%% ====================================================================
create_FTP_command(_FtpCommand, [], Result) ->
    Result ++ "\r\n";

create_FTP_command(FtpCommand, [Arg | Args], Result) ->
    create_FTP_command(FtpCommand, Args, Result ++ " " ++ Arg).

%% check_ftp_response/2
%% ====================================================================
%% @doc Function which checks if the response contains expected response

-spec check_ftp_response(Response :: ftp_response(), ExpectedResponse :: string()) ->
          ok | nok.
%% ====================================================================
check_ftp_response(Response, ExpectedResponse) ->
    case string:str(Response, ExpectedResponse) of
    0 ->
        nok;
    _Index ->
        ok
    end.

%% authentication_sequence/2
%% ====================================================================
%% @doc 

-spec authentication_sequence(Handler :: ftp_handler(), Config :: config_list()) ->
          {ftp_socket(), config_list()}.
%% ====================================================================
authentication_sequence(Handler, Config) when is_port(Handler)-> 
    {_, Certificate} = lists:keyfind(certificate, 1, Config),
    {ok, _} = send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpTestLib:ssl_connect(Handler, Certificate, ?DEFAULT_TIMEOUT),
    NewConfig = [{socket,SSLSocket}|Config],
    {ok, _} = send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    {ok, _} = send_specific_command_over_TCP(SSLSocket, "PROT", ["P"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    {SSLSocket, NewConfig};

authentication_sequence(Handler, Config) when is_pid(Handler) ->
    {Handler, Config}.

%% get_client_ip/2
%% ====================================================================
%% @doc Function which returns clients IP address.

-spec get_client_ip(Handler :: ftp_handler(), Ip_protocol :: ip_protocol()) -> ip_address().
%% ====================================================================
get_client_ip(Handler, ipv6) ->
    {ok, {{_, _, _, _, _, _, _, _} = IP, _}} = ssl:sockname(Handler),
    IP;

get_client_ip(_Handler, _OtherIp) ->
    {ok, Name} = inet:gethostname(),
    {ok, Ip} =
    case inet_res:gethostbyname(Name) of
        {ok, HostInfo} -> {ok, hd(HostInfo#hostent.h_addr_list)};
        {error, _}     -> inet:getaddr(Name)
    end,
    Ip.

%% setup_control_connection/1
%% ====================================================================
%% @doc Function which connects, logins and changes working directory

-spec setup_control_connection(Config :: [tuple()]) -> [tuple()].
%% ====================================================================
setup_control_connection(Config) ->
    flush_messages(),
    Connection_mode = ?config(connection_mode,Config),
    IpAddress = ?config(host, Config),
    Args = ?config(args, Config),
    {ok, Handler} = open_ftp_control_connection(Connection_mode, IpAddress, Args),
    NewConfig =[{socket,Handler}|Config],

    User = ?config(user, Config),
    {SSLHandler, NewConfig2} = ftpTestLib:authentication_sequence(Handler, NewConfig),
    ok = ftpTestLib:ftp_user(SSLHandler, User, ""),
    
    ok = ftpTestLib:ftp_cd(SSLHandler, ?FTPES_TEST_DIR),
    NewConfig2.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Node and server starting functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_node_name() ->
    [{_N, Node} | _] = ct:get_config(test_nodes),
    Node.

get_ip_config_name(lmt, IpProto) when ?INET(IpProto) ->
    ssh_lmt_ipv4;
get_ip_config_name(lmt, IpProto) when ?INET6(IpProto) ->
    ssh_lmt_ipv6;
get_ip_config_name(oam, IpProto) when ?INET(IpProto) ->
    ssh_TN_A_ipv4;
get_ip_config_name(oam, IpProto) when ?INET6(IpProto) ->
    ssh_TN_A_ipv6;
get_ip_config_name(alt_oam, IpProto) when ?INET(IpProto) ->
    ssh_TN_A_ipv4_alt;
get_ip_config_name(alt_oam, IpProto) when ?INET6(IpProto) ->
    ssh_TN_A_ipv6_alt.

%% get_node_ip/2
%% ==================================================================== 
%% @doc Function which gets ipv4 or ipv6 address
 
-spec get_node_ip(RpcHandler :: atom(),
                  IpProtocol :: atom()) -> ip_address().
 %% ====================================================================
get_node_ip(RpcHandler, IpProtocol) ->
    get_node_ip(RpcHandler, lmt, IpProtocol).

%% get node ip from config
get_node_ip(config, IpInterface, IpProtocol) ->
    ConfigList = ct:get_config(get_node_name()),
    Address = proplists:get_value(ssh, proplists:get_value(get_ip_config_name(IpInterface, IpProtocol), ConfigList)),
    case inet:parse_address(Address) of
        {ok, IpAddrTuple} ->
            IpAddrTuple;
        _ ->
            einval
    end;

get_node_ip(RpcHandler, IpInterface, IpProtocol) ->
    case is_target() of
        true ->
            get_node_ip(target, RpcHandler, IpInterface, IpProtocol);
        false ->
            get_node_ip(sim, RpcHandler, IpInterface, IpProtocol)
    end.

%% get localhost address
get_node_ip(sim, _RpcHandler, _IpInterface, IpProto) when ?INET(IpProto) ->
    {127,0,0,1};
get_node_ip(sim, _RpcHandler, _IpInterface, IpProto) when ?INET6(IpProto) ->
    {0,0,0,0,0,0,0,1};

%% get node ip from node
get_node_ip(target, RpcHandler, lmt, IpProto) when ?INET(IpProto) ->
    rct_rpc:call(RpcHandler, ootI, get_lmt_ipv4, [], 10000);
get_node_ip(target, _RpcHandler, lmt, IpProto) when ?INET6(IpProto) ->
    einval;
get_node_ip(target, RpcHandler, oam, IpProto) when ?INET(IpProto) ->
    Host = rct_rpc:call(RpcHandler, ootI, get_oap_ip_addr, [], 10000),
    case inet:parse_address(Host) of
    {ok, {_,_,_,_} = IpAddrTuple} ->
        IpAddrTuple;
    _ ->
        ct:log("OAM IP address is IPv6..."),
        einval
    end;
get_node_ip(target, RpcHandler, oam, IpProto) when ?INET6(IpProto) ->
    Host = rct_rpc:call(RpcHandler, ootI, get_oap_ip_addr, [], 10000),
    case inet:parse_address(Host) of
    {ok, {_,_,_,_,_,_,_,_} = IpAddrTuple} ->
        IpAddrTuple;
    _ ->
        ct:log("OAM IP address is IPv4... "),
        einval
    end;
get_node_ip(target, RpcHandler, alt_oam, IpProto) when ?INET(IpProto) ->
    Host = rct_rpc:call(RpcHandler, ootI, get_oap_ip_addr_alt, [], 10000),
    case inet:parse_address(Host) of
    {ok, {_,_,_,_} = IpAddrTuple} ->
        IpAddrTuple;
    _ ->
        ct:log("Alt OAM IP address is IPv6..."),
        einval
    end;
get_node_ip(target, RpcHandler, alt_oam, IpProto) when ?INET6(IpProto) ->
    Host = rct_rpc:call(RpcHandler, ootI, get_oap_ip_addr_alt, [], 10000),
    case inet:parse_address(Host) of
    {ok, {_,_,_,_,_,_,_,_} = IpAddrTuple} ->
        IpAddrTuple;
    _ ->
        ct:log("Alt OAM IP address is IPv4..."),
        einval
    end.

% copy_files_to_node/3
%% ====================================================================
%% @doc 

%% ====================================================================
copy_files_to_node(_ScpHandler, [], _Dir) ->
    ok;

copy_files_to_node(ScpHandler, [File | Files], Dir) ->
    {ok, _} = rct_scp:to_target(ScpHandler, File, Dir, 5),
    copy_files_to_node(ScpHandler, Files, Dir).

% start_server/0
%% ====================================================================
%% @doc 

%% ====================================================================
start_server() ->
    case rct_rpc:call(rpc, erlang, whereis, [ftpesServer], 10000) of
        undefined -> initialize_tables(rpc),
                     start_ftpes_sup(rpc, [{port, ?DEFAULT_SERVER_CONTROL_CONNECTION_PORT}]),
                     activate(rpc),
                     no;
        _Pid ->  activate(rpc),
                 yes
    end.

    
%% start_ftpes_sup/2
%% ====================================================================
%% @doc 

-spec start_ftpes_sup(RpcHandler :: rcp_handler(), Opts :: list()) -> {ok,ftp_handler()} | ignore | {error, string()}. %%???
%% ====================================================================
start_ftpes_sup(RpcHandler, Opts) ->
    {ok, SupPid} = rct_rpc:call(RpcHandler, ftpesServer, start, [Opts], 10000),
    SupPid.

%% start_ftpes_server/1
%% ====================================================================
%% @doc 

-spec start_ftpes_server(RpcHandler :: rcp_handler()) -> ftp_handler().
%% ====================================================================
start_ftpes_server(RpcHandler) ->
    rct_rpc:call(RpcHandler, ftpesServer, start_ftpesd_server, [], 10000).

%% stop_ftpes_sup/1
%% ====================================================================
%% @doc 

-spec stop_ftpes_sup(RpcHandler :: rcp_handler()) -> ok.
%% ====================================================================
stop_ftpes_sup(RpcHandler) ->
    rct_rpc:call(RpcHandler, ftpesServer, stop, [], 10000).

%% stop_ftpes_server/1
%% ====================================================================
%% @doc 

-spec stop_ftpes_server(RpcHandler :: rcp_handler()) -> ok.
%% ====================================================================
stop_ftpes_server(RpcHandler) ->
    rct_rpc:call(RpcHandler, ftpesServer, stop_ftpesd_server, [], 10000).

%% activate/1
%% ====================================================================
%% @doc 

-spec activate(RpcHandler :: rcp_handler()) -> ok.
%% ====================================================================
activate(RpcHandler) ->
    rct_rpc:call(RpcHandler, ftpesDataInit, activate, [], 10000).

%% initialize_tables/1
%% ====================================================================
%% @doc 

-spec initialize_tables(RpcHandler :: rcp_handler()) -> ok.
%% ====================================================================
initialize_tables(RpcHandler) ->
    rct_rpc:call(RpcHandler, ftpesDataInit, instPhParallel_init_data, [], 10000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TLS commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% initialize_nc_tc/1
%% ====================================================================
%% @doc Function which initialize all parameters needed for setup_tls

-spec initialize_nc_tc(Config :: config_list()) -> config_list().
%% ====================================================================
initialize_nc_tc(Config) ->
    FirstNcId = "ftpes_test_login",
    SecondNcId = "TLS_login",
    FirstTcId = "ftpes_test_login_tc",
    SecondTcId = "TLS_login_tc",
    Files = ["tls_nodecert_pkcs12", "user-ca.crt"],
    Priv_dir =
    filename:join(
        lists:droplast(
        filename:split(
            ?config(priv_dir, Config)))),
    FileNc = filename:join([Priv_dir, "tls_nodecert_pkcs12"]),
    FileTc = filename:join([Priv_dir, "user-ca.crt"]),
    IpProto = get_oam_ip_protocol(),
    SftpUser = get_sftp_server_username(),
    SftpPassword = get_sftp_server_password(),
    SftpAddress = get_sftp_server_address(IpProto),
    %Uri = "sftp://" ++ Uname ++ "@" ++ Shost ++ File,
    
    UriNc = "sftp://" ++ SftpUser ++ "@" ++ SftpAddress ++ FileNc,
    UriTc = "sftp://" ++ SftpUser ++ "@" ++ SftpAddress ++ FileTc,

    NcCredPwd = "idiocy",
    NcFingerPrint = "17:8b:19:ef:57:e1:12:62:67:33:f5:bd:bd:8c:28:8e:bd:4b:c2:ce",
    TcFingerPrint = "DB:59:94:FB:BE:E6:1B:83:D4:77:88:BF:F8:27:9B:B9:BC:A0:5D:23",
    %% Try to clean nc and tc just if something left behind before
    clean_nc_tc([{firstNC, FirstNcId}, {secondNC, SecondNcId}, {firstTC, FirstTcId}, {secondTC, SecondTcId} | Config]),
    setup_tls([{nodeCredentialPwd, NcCredPwd}, {nodeCredentialFPrint, NcFingerPrint},
               {trustCategoryPwd, SftpPassword}, {trustCategoryFPrint, TcFingerPrint},
               {nodeCredentialIDList, [FirstNcId, SecondNcId]},{trustCategoryList, [FirstTcId, SecondTcId]},
               {files, Files}, {uriNc, UriNc}, {uriTc, UriTc} | Config]),
    Result = get_all_node_credentials("1"),
    io:format("NC: ~p",[Result]),
    Result2 = get_all_trusted_certificates("1"),
    io:format("TC: ~p",[Result2]),
    [{firstNC, FirstNcId}, {secondNC, SecondNcId}, {firstTC, FirstTcId}, {secondTC, SecondTcId} | Config].

%% setup_tls/1
%% ====================================================================
%% @doc Function which setups all needed TLS options

-spec setup_tls(Config :: config_list()) -> ok.
%% ====================================================================
setup_tls(Config) ->
    prepare_files(Config),
    
    %% create NCs
    [create_install_NC_MO_credentials([{nodeCredentialID, Id} | Config]) || Id <- ?config(nodeCredentialIDList, Config)],

    %% create FTPES trusted certificate
    create_trusted_certificate(Config),
    
    %% create trustCategories
    [create_trust_category([{trustCategory, Id} | Config]) || Id <- ?config(trustCategoryList, Config)].


%% clean_nc_tc/1
%% ====================================================================
%% @doc Function which deletes all created TLS options

-spec clean_nc_tc(Config :: config_list()) -> ok.
%% ====================================================================
clean_nc_tc(Config) ->
    FirstNcId = ?config(firstNC, Config),
    SecondNcId = ?config(secondNC, Config),
    FirstTcId = ?config(firstTC, Config),
    SecondTcId = ?config(secondTC, Config),
    remove_node_credential([{nodeCredentialID, FirstNcId} | Config]),
    remove_node_credential([{nodeCredentialID, SecondNcId} | Config]),
    remove_trust_category([{trustCategory, FirstTcId} | Config]),
    remove_trust_category([{trustCategory, SecondTcId} | Config]),
    remove_trusted_certificates(Config),
    Result = get_all_node_credentials("1"),
    io:format("NC: ~p",[Result]),
    Result2 = get_all_trusted_certificates("1"),
    io:format("TC: ~p",[Result2]),
    ok.

%% prepare_files/1
%% ====================================================================
%% @doc  Copy the files that the node may need to fetch using sftp
%% tls_nodecert_pkcs12: used as nodecredential in the TLS server
%% user-ca.crt: CA, (trusted-certificate) used to validate the login-cert.
%% Those files are given in Config

-spec prepare_files(Config :: config_list()) -> ok.
%% ====================================================================
prepare_files(Config) ->
    Files = ?config(files, Config),
    Priv_dir =
    filename:join(
        lists:droplast(
        filename:split(
            ?config(priv_dir, Config)))),
    Data_dir = ?config(data_dir, Config),
    ct:pal("prepare_files: ~p, ~p, ~p", [Files, Priv_dir, Data_dir]),
    [begin
        Fpriv = filename:join([Priv_dir, F]),
        Fdata = filename:join([Data_dir, F]),
        case {file:read_file(Fpriv), file:read_file(Fdata)} of
            {{ok, Same}, {ok, Same}} -> ok;
            {_, {ok, Bin_file}} ->
                ok = file:write_file(Fpriv, Bin_file)
        end
     end || F <- Files].


%% create_install_NC_MO_credentials/1
%% ====================================================================
%% @doc Function which creates and installs NC MO credentials

-spec create_install_NC_MO_credentials(Config :: config_list()) -> ok.
%% ====================================================================
create_install_NC_MO_credentials(Config) ->
    create_NC_MO(Config),
    install_credentials(Config).


%% create_NC_MO/1
%% ====================================================================
%% @doc Install PKCS#12 as node credential from uri via https

-spec create_NC_MO(Config :: config_list()) -> ok.
%% ====================================================================
create_NC_MO(Config) ->
    rand:seed(exs64),
    MeId = "1",
    NodeCredentialId = ?config(nodeCredentialID, Config),
    KeyInfo = "RSA_2048",
    ct:pal("Key type ~s selected.~n",[KeyInfo]),
    SubjectName = "esekilvxen519.rnd.ki.sw.ericsson.se",
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
             {'SystemFunctions', [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                        [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                           [{certMId,[],["1"]},
                   {'NodeCredential', [{nodeCredentialId, [NodeCredentialId]},
                                       {userLabel, ["Created by ftpesServer"]},
                                       {subjectName, [SubjectName]},
                                       {keyInfo, [KeyInfo]}]}]}]}]}]},
     ct:pal("Create Node Credential MO~n",[]),
     case set_mo(Edit) of
     ok -> ok;
     _ ->
         ct:pal("Node Credential MO couldn't be created"),
         ok
         %ct:fail(Error1)
     end.

%% install_credentials/1
%% ====================================================================
%% @doc Install NC

-spec install_credentials(Config :: config_list()) -> ok.
%% ====================================================================
install_credentials(Config) ->
    MeId = "1",
    NodeCredentialId = ?config(nodeCredentialID, Config),
    Uri = ?config(uriNc, Config),
    CredPwd = ?config(nodeCredentialPwd, Config),
    FingerPrint = ?config(nodeCredentialFPrint, Config),
    Install = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                                 [{managedElementId,[],[MeId]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                           [{secMId,[],["1"]},
                    {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                              [{certMId,[],["1"]},
                      {'NodeCredential', [{nodeCredentialId, [NodeCredentialId]},
                        {installCredentialFromUri, [{uri, [Uri]},
                                                    {uriPassword, ["dustest"]},
                                                    {credentialPassword, [CredPwd]},
                                                    {fingerprint, [FingerPrint]}]}]}]}]}]}]},
    ct:pal("Action installCredentialFromUri 1 PKCS#12~n",[]),
    case action_mo(Install) of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("installCredentialFromUri 1 action failed ~p ~n",[Error]),
        ok
    end.

%% create_trust_category/1
%% ====================================================================
%% @doc Create trusted certificates and reserve them by trust category

-spec create_trust_category(Config :: config_list()) -> ok.
%% ====================================================================
create_trust_category(Config) ->
    MoRefs = get_trusted_certificates(),
    set_trust_category(Config, MoRefs).

create_trusted_certificate(Config) ->
    {ok, TcId} = install_trusted_certificate(Config),
    ok = activate_trusted_certificate(TcId).

get_trusted_certificates() ->
    [make_tc_moref("1", TC)||TC<-get_all_trusted_certificates("1")].
    
install_trusted_certificate(Config) ->
    MeId = "1",
    ProgressFilter = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
                       {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                         {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                                  [{secMId,[],["1"]},
                           {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                                     [{certMId,[],["1"]},
                             {reportProgress, []}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),
    Uri = ?config(uriTc, Config),
    Password = ?config(trustCategoryPwd, Config),
    Fingerprint = ?config(trustCategoryFPrint, Config),

    Action = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
               {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                 {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                          [{secMId,[],["1"]},
                   {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                             [{certMId,[],["1"]},
                     {installTrustedCertFromUri, [], [{uri, [Uri]},
                                                      {uriPassword, [Password]},
                                                      {fingerprint, [Fingerprint]}]}]}]}]}]},
    ActionResult = action_mo(Action),
    case ActionResult of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("installTrustedCertFromUri action failed: ~p~n",[Error])
        %ct:fail(Error)
    end,
    
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
    ["SUCCESS"] ->
        ResultInfo = get_result_info(ProgressFilter),
        ct:pal("Reading ~p~n",[ResultInfo]),
        TcKey = lists:last(string:tokens(ResultInfo, ",=")),
        {ok, TcKey};
    Other ->
        {error, Other}
    end.
        
      
activate_trusted_certificate(TcKey) ->
    
    Edit = {'ManagedElement', [?COMTOP], [{managedElementId,[],["1"]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                           [{secMId,[],["1"]},
                    {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                              [{certMId,[],["1"]},
                      {'TrustedCertificate', [{trustedCertificateId, [TcKey]},
                                              {managedState,[],["ENABLED"]}]}]}]}]}]},
    set_mo(Edit).


%% set_trust_category/2
%% ====================================================================
%% @doc Create trusted certificates

-spec set_trust_category(Config :: config_list(),
                         MoRefs :: list()) -> ok.
%% ====================================================================
set_trust_category(Config, MoRefs) ->
    case tl(MoRefs) of
        [] -> Element = MoRefs;
        _ -> Element = tl(MoRefs)
    end,
    TrustCategory = ?config(trustCategory, Config),
    MeId = "1",
    Edit = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
             {'SystemFunctions', [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                        [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                           [{certMId,[],["1"]},
                   {'TrustCategory', [], [{trustCategoryId, [], [TrustCategory]},
                                          {userLabel, ["Created by ftpesServer"]}|
                                           [{trustedCertificates, [Element]}]]}]}]}]}]},
    EditRes = set_mo(Edit),
    case EditRes of
    ok -> ok;
    _ ->
        ct:pal("Trust category couldn't be set"),
        ok
    end.

%% remove_node_credential/1
%% ====================================================================
%% @doc Removes all node credentials unless there is a specific nodeCredentialId
%% given as Config

-spec remove_node_credential(Config :: config_list()) -> ok.
%% ====================================================================
remove_node_credential(Config) ->
    MeId = "1",
    NcIds =  case proplists:get_value(nodeCredentialID, Config) of
         undefined ->
             [begin
              {ok, I} = extract_element(nodeCredentialID, [NC]),
              I
              end||NC<-get_all_node_credentials(MeId)];
         NcId ->
             [NcId]
         end,
    
    DeleteOps = [{'NodeCredential', [{'xc:operation', "delete"}], [{nodeCredentialId, [], [Id]}]}||
            Id<-NcIds],
           
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
             {'SystemFunctions', [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                        [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                           [{certMId,[],["1"]}|DeleteOps]}]}]}]},

    ct:pal("~p~n",[Edit]),
        
    
    EditRes = set_mo(Edit),
    case EditRes of
    ok -> ok;
    _ ->
        ct:pal("NodeCredential couldn't be deleted, it doesnt exists"),
        ok
        %ct:fail(EditRes)
    end.

%% remove_trust_category/1
%% ====================================================================
%% @doc Remove a specific trust category

-spec remove_trust_category(Config :: config_list()) -> ok.
%% ====================================================================
remove_trust_category(Config) ->
    TrustCategory = ?config(trustCategory, Config),
    MeId = "1",
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
             {'SystemFunctions', [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                        [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                           [{certMId,[],["1"]},
                   {'TrustCategory', [{'xc:operation', "delete"}],
                                     [{trustCategoryId, [], [TrustCategory]}]}]}]}]}]},
    EditRes = set_mo(Edit),
    case EditRes of
    ok -> ok;
    _ ->
        ct:pal("TrustCategory couldn't be deleted, it doesnt exists"),
        ok
        %ct:fail(EditRes)
    end.

%% remove_trusted_certificates/1
%% ====================================================================
%% @doc Remove all trusted certificates

-spec remove_trusted_certificates(Config :: config_list()) -> ok.
%% ====================================================================
remove_trusted_certificates(Config) ->
    MeId = "1",
    ProgressFilter = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
                       {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                         {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                                  [{secMId,[],["1"]},
                           {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                                     [{certMId,[],["1"]},
                             {reportProgress, []}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    AllTCs = get_all_trusted_certificates(MeId),
    remove_one_by_one(AllTCs, ActionId, ProgressFilter, Config).

remove_one_by_one([], _, _, _) -> ok;

remove_one_by_one(AllTCs, ActionId, ProgressFilter, Config) ->
    MeId = "1",
    MoRef = make_tc_moref(MeId, hd(AllTCs)),
    Action = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
               {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                 {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                          [{secMId,[],["1"]},
                   {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                             [{certMId,[],["1"]},
                     {removeTrustedCert, [], [{trustedCert, [MoRef]}]}]}]}]}]},
    ActionResult = action_mo(Action),
    case ActionResult of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("removeTrustedCert action failed: ~p~n",[Error])
        %ct:fail(Error)
    end,
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
    ["SUCCESS"] ->
        ok;
    Result ->
        ct:pal("removeTrustedCert: ~s~n",[Result])
        %ct:fail(Result)
    end,
    ProgressFilterNew = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
                          {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                                     [{secMId,[],["1"]},
                              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                                        [{certMId,[],["1"]},
                                         {reportProgress, []}]}]}]}]},
   ActionIdNew = get_action_id(ProgressFilter),
   remove_one_by_one(tl(AllTCs), ActionIdNew, ProgressFilterNew, Config).

%%%--------------------------------------------------------------------
%%% Description: Make a Trusted certificate object reference from netconf data
%%%--------------------------------------------------------------------

make_tc_moref(MeId, TC) ->
    {ok, {trustedCertificateId, _, [Id]}} = 
    extract_element(trustedCertificateId, [TC]),
    "ManagedElement="++MeId++",SystemFunctions=1,SecM=1,CertM=1,"
    "TrustedCertificate="++Id.

%%%--------------------------------------------------------------------
%%% Description: Read out all TrustedCertificiate MO instances
%%%--------------------------------------------------------------------

get_all_trusted_certificates(MeId) ->
    Get = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
            {'SystemFunctions', [{systemFunctionsId,[],["1"]},
              {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                       [{secMId,[],["1"]},
                {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                          [{certMId,[],["1"]}]}]}]}]},
    {ok, Result} = get_mo(Get),
    {ok, {_, _, Contents}} = extract_element('CertM', Result),
   [Element||Element<-Contents, 
         element(1, Element) == 'TrustedCertificate'].


%%%--------------------------------------------------------------------
%%% @doc Install and activate trusted certificate using SFTP
%%% @end
%%%--------------------------------------------------------------------
install_activate_trusted_certificate(Config) ->
    MeId = "1",
    ProgressFilter = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
                       {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                         {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                                  [{secMId,[],["1"]},
                           {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                                     [{certMId,[],["1"]},
                             {reportProgress, []}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),
    Uri = ?config(uriTc, Config),
    Password = ?config(trustCategoryPwd, Config),
    Fingerprint = ?config(trustCategoryFPrint, Config),

    Action = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
               {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                 {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                          [{secMId,[],["1"]},
                   {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                             [{certMId,[],["1"]},
                     {installTrustedCertFromUri, [], [{uri, [Uri]},
                                                      {uriPassword, [Password]},
                                                      {fingerprint, [Fingerprint]}]}]}]}]}]},
    ActionResult = action_mo(Action),
    case ActionResult of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("installTrustedCertFromUri action failed: ~p~n",[Error])
        %ct:fail(Error)
    end,
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
    ["SUCCESS"] ->
        ResultInfo = get_result_info(ProgressFilter),
        ct:pal("Reading ~p~n",[ResultInfo]),
        TcKey = lists:last(string:tokens(ResultInfo, ",=")),
        timer:sleep(1000),
        Get = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                           [{secMId,[],["1"]},
                    {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                              [{certMId,[],["1"]},
                      {'TrustedCertificate', [{trustedCertificateId, [TcKey]},
                                              {managedState,[],["ENABLED"]}]}]}]}]}]},
        %% At this stage of development we do not care of the returned
        %% value from netconf, only that we actually succeds with the 
        %% get operation. jotj 2013-08-12
        %{ok, _} = netconf(get, [nc1, Get]),
        set_mo(Get),
        {ok, ResultInfo};
    Result ->
        ct:pal("installTrustedCertFromUri: ~s~n",[Result]),
        ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% Description: Read out all NodeCredential MO instances
%%%--------------------------------------------------------------------

get_all_node_credentials(MeId) ->
    Get = {'ManagedElement', [?COMTOP], [{managedElementId,[],[MeId]},
            {'SystemFunctions', [{systemFunctionsId,[],["1"]},
              {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                       [{secMId,[],["1"]},
                {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                          [{certMId,[],["1"]}]}]}]}]},
    {ok, Result} = get_mo(Get),
    {ok, {_, _, Contents}} = extract_element('CertM', Result),
    [Element||Element<-Contents, 
         element(1, Element) == 'NodeCredential'].

%%%--------------------------------------------------------------------
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(ProgressFilter) ->
    get_progress_report_member(actionId, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(ProgressFilter) ->
    get_progress_report_member(resultInfo, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, ProgressFilter) ->
    {ok, A} = get_mo(ProgressFilter),
    ct:pal("ProgressFilter result: ~p~n", [A]),
    case extract_element(reportProgress, A) of
    {ok, {reportProgress, L, _}}  ->
        case lists:keyfind(unset,1, L) of
        {unset, "true"} ->
            undefined;
        _ ->
            extract_member(Member, A)
        end;
    _ ->
        extract_member(Member, A)
    end.
extract_member(Member, A) ->
    {ok, {Member, _, [Value]}} = 
    extract_element(Member, A),
    Value.
    
%%%--------------------------------------------------------------------
%%% Description: This function and the associated check_progress_elements
%%%              loops over a netconf get for the progress information until
%%%              the state is FINISHED, otherwise it prints the status and
%%%              progress percentage
%%%--------------------------------------------------------------------

wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(1000),
    case ct_netconfc:open(nc1, []) of
    {ok, _} ->
        Get = ct_netconfc:get(nc1, ProgressFilter), 
        ct_netconfc:close_session(nc1),
        case Get of
        {ok, Report} ->
            case extract_element(actionId, Report) of
            {ok, {actionId, _, [OldActionId]}} ->
                ct:pal("Waiting for updated progress ~n~p~n",[Report]),
                wait_for_progress(Attribute, OldActionId, 
                          ProgressFilter);
            _ ->
                case check_progress_elements(Attribute, Report) of
                loop ->
                    wait_for_progress(Attribute, OldActionId, 
                              ProgressFilter);
                {ok, Result} ->
                    Result
                end
            end;
        {error, Error} ->
            ct:pal("Netconf get error:~n~p",[Error]),
            wait_for_progress(Attribute, OldActionId, ProgressFilter)
        end;
    {error, Reason} ->
        ct:pal("Netconf open error:~n~p",[Reason]),
        wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    {ok, State} = extract_element(state, [Report]),
    case State of
    {state, _, ["FINISHED"]} ->
        format_progress_report(Report),
        ct:pal("~s",[format_progress_report(Report)]),
        {ok, {result, _, Result}} = extract_element(result, [Report]),
        {ok, Result};
    {state, _, [Current]} ->
        {ok, {progressPercentage, _, [Percent]}} =
        extract_element(progressPercentage, [Report]),
        {ok, {progressInfo, _, [Info]}} = 
        extract_element(progressInfo, [Report]),
        ct:pal("State: ~s ~s % ready~n~s",[Current, Percent, Info]),
        loop
    end.

%%%--------------------------------------------------------------------
%%% Description: Format a AsyncActionStruct for printing
%%%--------------------------------------------------------------------


format_progress_report({reportProgress, _, Members}) ->
    [io_lib:format("reportProgress:~n",[])|format_progress_report(Members)];
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) -> 
    ct:pal("Unknown format: ~p~n",[X]),
    ct:fail(unknown_progress_report_format).

%% set_nc_tc/3
%% ====================================================================
%% @doc Sets or deletes our NC and TC

-spec set_nc_tc(Nc_name :: string(),
                Tc_name :: string(),
                Delete :: list()) -> ok | error.
%% ====================================================================
set_nc_tc(Nc_name, Tc_name, Delete) ->
    case Delete of
        [] -> Nc_dn = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
                      "NodeCredential=" ++ Nc_name],
              Tc_dn = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
                      "TrustCategory=" ++ Tc_name];
        __ -> Nc_dn = [],
              Tc_dn = []
    end,
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],["1"]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'FileTPM', [], [{fileTPMId,[],["1"]},
                      {'FtpTls', [], [{ftpTlsId,[],["1"]},
                                      {nodeCredential, Delete, Nc_dn},
                                      {trustCategory, Delete, Tc_dn}]}]}]}]}]},

    case set_mo(Edit) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.


%% set_nc/2
%% ====================================================================
%% @doc Sets or deletes our NC

-spec set_nc(Nc_name :: string(),
             Delete :: list()) -> ok | error.
%% ====================================================================
set_nc(Nc_name, Delete) ->
    case Delete of
        [] -> Nc_dn = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
                      "NodeCredential=" ++ Nc_name];
        __ -> Nc_dn = []
    end,
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],["1"]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'FileTPM', [], [{fileTPMId,[],["1"]},
                      {'FtpTls', [], [{ftpTlsId,[],["1"]},
                                      {nodeCredential, Delete, Nc_dn}]}]}]}]}]},

    case set_mo(Edit) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

%% set_tc/2
%% ====================================================================
%% @doc Sets or deletes our TC

-spec set_tc(Tc_name :: string(),
             Delete :: list()) -> ok | error.
%% ====================================================================
set_tc(Tc_name, Delete) ->
    case Delete of
        [] -> Tc_dn = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
                      "TrustCategory=" ++ Tc_name];
        __ -> Tc_dn = []
    end,
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],["1"]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'FileTPM', [], [{fileTPMId,[],["1"]},
                      {'FtpTls', [], [{ftpTlsId,[],["1"]},
                                      {trustCategory, Delete, Tc_dn}]}]}]}]}]},

    case set_mo(Edit) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

%% set_as/1
%% ====================================================================
%% @doc Sets AS to UNLOCKED or LOCKED

-spec set_as(As_state :: string()) -> ok | error.
%% ====================================================================
set_as(As_state) ->
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],["1"]},
             {'SystemFunctions', [{systemFunctionsId,[],["1"]},
               {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                        [{sysMId,[],["1"]},
                 {'FileTPM', [], [{fileTPMId,[],["1"]},
                   {'FtpTls', [], [{ftpTlsId,[],["1"]},
                     {'FtpTlsServer', [], [{ftpTlsServerId,[],["1"]},
                                           {administrativeState, [], [As_state]}]}]}]}]}]}]},
    
    case set_mo(Edit) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.
    
enable_ftpes_tls(Config) ->
    FirstNcId = ?config(firstNC, Config),
    FirstTcId = ?config(firstTC, Config),
    ftpTestLib:set_nc_tc(FirstNcId, FirstTcId, []),
    ftpTestLib:set_as("UNLOCKED").

disable_ftpes_tls() ->
    set_nc_tc([], [], ?OPER_DELETE_ATTR).

-spec exists_ftp_tls() -> ok | nok.
exists_ftp_tls() ->
    case get_ftp_tls() of
    {error, _} ->
        nok;
    Result ->
        case extract_element('FtpTls', Result) of
        {ok, {_, _, _Contents}} ->
            ct:log("FtpTls Mo exists"),
            ok;
        _Other ->
            ct:log("FtpTls mo doesn't exist"),
            nok
        end
    end.

-spec exists_ftp_tls_server() -> ok | nok.
exists_ftp_tls_server() ->
    case get_ftp_tls_server() of
    {error, _} ->
        nok;
    Result ->
        case extract_element('FtpTlsServer', Result) of
        {ok, {_, _, _Contents}} ->
            ct:log("FtpTlsServer Mo exists"),
            ok;
        _Other ->
            ct:log("FtpTlsServer Mo doesn't exist"),
            nok
        end
    end.

-spec get_ftp_tls() -> tuple() | {error, term()}.
get_ftp_tls() ->
    Get = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],["1"]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'FileTPM', [], [{fileTPMId,[],["1"]},
                      {'FtpTls', [], 
                            [{ftpTlsId,[],["1"]}]}]}]}]}]},
    
    case get_mo(Get) of
    {ok, Result} ->
        Result;
    Other ->
        Other
    end.

-spec get_ftp_tls_server() -> tuple() | {error, term()}.
get_ftp_tls_server() ->
    Get = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],["1"]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'FileTPM', [], [{fileTPMId,[],["1"]},
                      {'FtpTls', [], [{ftpTlsId,[],["1"]},
                        {'FtpTlsServer', [], 
                            [{ftpTlsServerId, [], ["1"]}]}]}]}]}]}]},
    
    case get_mo(Get) of
    {ok, Result} ->
        Result;
    Other ->
        Other
    end.

%% set_tls_cipher_suites/1
%% ====================================================================
%% @doc Set cipherFilter attribute in Tls Mo.
%% ====================================================================
set_tls_cipher_suites(Cipher) ->
    MeId = "1",
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
             {'SystemFunctions', [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                        [{secMId,[],["1"]},
                 {'Tls', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                           [{tlsId,[],["1"]},
                            {cipherFilter, [Cipher]}]}]}]}]},
    
    case set_mo(Edit) of
        ok -> 
            ct:log("cipherFilter successful set"), ok;
        Error1 ->
            ct:fail(Error1)
    end.


%% get_tls_cipher_suites/1
%% ====================================================================
%% @doc Get all enabled ciphers from Tls Mo.
%% ====================================================================
get_tls_cipher_suites() ->
   Get = {'ManagedElement', [?COMTOP], [{managedElementId,[],["1"]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                           [{secMId,[],["1"]},
                      {'Tls', [{xmlns,"urn:com:ericsson:ecim:RcsTls"}],
                              [{tlsId,[],["1"]},
                              {enabledCiphers, []}]}]}]}]},
    {ok, Conf} = get_mo(Get),
    
    Conf.


%% oam_exists/2
%% ====================================================================
%% @doc Checks if OAM accessPoint address is set
%% ====================================================================
oam_exists(IpProto) ->
       case {oam_exists(oam, IpProto), oam_exists(alt_oam, IpProto)} of
        {ok, ok} ->
            ok;
         _ ->
             nok
       end.

oam_exists(Type, IpProto) -> 
    OamRef = get_oam_addr_ref(oam, IpProto),
    AltOamRef = get_oam_addr_ref(alt_oam, IpProto),
    try case get_oam_ref(nc2, Type) of
              OamRef ->
                 ok;
              AltOamRef ->
                 ok;
             _ ->
                 nok
           end
       catch _:_ ->
             nok
       end.

%% get_oam_ip/3
%% ====================================================================
%% @doc Fetches IP address from AddressIPv4 of type oam or alt_oam
%% ====================================================================
get_oam_ip(Type, IpProto) ->
    get_oam_ip(nc2, Type, IpProto).

get_oam_ip(NC, Type, IpProto) ->

    Get = {'ManagedElement', [?COMTOP], [{managedElementId, [], ["1"]},
              {'Transport', [], [{transportId, [], ["1"]},
                {'Router', [], [{routerId, [], ["OAM" ++ get_oam_suffix(Type)]},
                  {ip_interface_class(IpProto), [], [{ip_interface_class_id(IpProto), [], 
                                                      ["TN_A_OAM_IP" ++ get_oam_suffix(Type)]},
                     {ip_address_class(IpProto), [], [{ip_address_class_id(IpProto), [], 
                                                       ["TN_A_OAM_IP" ++ get_oam_suffix(Type)]}]}]}]}]}]},

    {ok, Conf} = get_mo(NC, Get),
    {ok, {address, _, [Address]}} = extract_element(address, Conf),

    Address.
    
%% get_oam_ref/2
%% ====================================================================
%% @doc Fetches AddressIPv4 reference which OamAccessPoint points to
%% ====================================================================
get_oam_ref(Type)->
    get_oam_ref(nc2, Type).
get_oam_ref(NC, Type)->
    Get = {'ManagedElement', [?COMTOP],
             [{managedElementId,[],["1"]},
              {'SystemFunctions',
               [{systemFunctionsId,[],["1"]},
                {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSySM"}],
                 [{sysMId,[],["1"]},
                  {'OamAccessPoint', [],
                   [{oamAccessPointId,[],[get_oam_id(Type)]}]}]}]}]},

    
    {ok, Conf} = get_mo(NC, Get),
    {ok, {accessPoint, _, [AccessPoint]}} = extract_element(accessPoint, Conf),
    
    AccessPoint.

%% edit_oam_ip/4
%% ====================================================================
%% @doc Edit AddressIPv4 / AddressIPv6 address field
%% ====================================================================
edit_oam_ip(Address, Type, IpProto) ->
    edit_oam_ip(nc2, Address, Type, IpProto).
edit_oam_ip(NC, Address, Type, IpProto) ->
      Edit = {'ManagedElement', [?COMTOP], [{managedElementId, [], ["1"]},
          {'Transport', [], [{transportId, [], ["1"]},
            {'Router', [], [{routerId, [], ["OAM" ++ get_oam_suffix(Type)]},
              {ip_interface_class(IpProto), [], [{ip_interface_class_id(IpProto), [], 
                                                  ["TN_A_OAM_IP" ++ get_oam_suffix(Type)]},
                 {ip_address_class(IpProto), [], [{ip_address_class_id(IpProto), [], 
                                                   ["TN_A_OAM_IP" ++ get_oam_suffix(Type)]},
                                     {address, [], [Address]}]}]}]}]}]},
              
    
    case set_mo(NC, Edit) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

%% edit_oam_ref/3
%% ====================================================================
%% @doc Edit OamAccessPoint accessPoint field
%% ====================================================================
edit_oam_ref(Ref, Type) ->
    edit_oam_ref(nc2, Ref, Type).
edit_oam_ref(NC, Ref, Type) ->
     Edit = {'ManagedElement',
             [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
             [{managedElementId,[],["1"]},
              {'SystemFunctions',
               [{systemFunctionsId,[],["1"]},
                {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSySM"}],
                 [{sysMId,[],["1"]},
                  {'OamAccessPoint', [],
                   [{oamAccessPointId,[],[get_oam_id(Type)]},
                   {accessPoint, [], [Ref]}]}]}]}]},

    case set_mo(NC, Edit) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.    

%% delete_oam_ref/2
%% ====================================================================
%% @doc Removes OamAccessPoint accessPoint field
%% ====================================================================
delete_oam_ref(Type) ->
    delete_oam_ref(nc2, Type).
delete_oam_ref(NC, Type) ->
    
    Edit = {'ManagedElement', [?COMTOP],
      [{managedElementId, [], ["1"]},
       {'SystemFunctions', [], 
        [{systemFunctionsId, [], ["1"]},
         {'SysM', [],
          [{sysMId, [], ["1"]},
           {'OamAccessPoint', [],
            [{oamAccessPointId, [], [get_oam_id(Type)]},
             {accessPoint, ?DELETE, []}]
           }]
         }]
       }]
     },
     case set_mo(NC, Edit) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.
    
%% convert "10.67.225.X/24" to {10,67,225,X}
parse_address(Address) ->
    Length = string:len(Address), 
    %% remote subnet (e.g /24)
    AddressString = string:substr(Address, 1, Length - 3),
    {ok, AddressTuple} = inet:parse_address(AddressString),
    AddressTuple.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NETCONF get/set/action functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_mo(Get :: tuple()) -> {ok, tuple()} | {error, term()}.
get_mo(Get) ->
    get_mo(nc1, Get).
get_mo(NC, Get) ->
    netconf(NC, get, [NC, Get]).

-spec set_mo(Edit :: tuple()) -> ok | {error, term()}.
set_mo(Edit) ->
    set_mo(nc1, Edit).
set_mo(NC, Edit) ->
        case netconf(NC, edit_config, [NC, running, Edit]) of
            ok ->
                timer:sleep(1000),
                ok;
            {error, Reason} ->
                {error, Reason};
            Other ->
                {error, Other}
        end.

-spec action_mo(Action :: tuple()) -> ok | {error, term()}.
action_mo(Action) ->
    action_mo(nc1, Action).
action_mo(NC, Action) ->
        case  netconf(NC, action, [NC, Action]) of
            {ok, Result} ->
                timer:sleep(2000),
                {ok, Result};
            {error, Reason} ->
                {error, Reason};
            Other ->
                {error, Other}
        end.

%% netconf/3
%% ====================================================================
%% @doc Wrapper for netconf command

-spec netconf(NC :: atom(), F :: string(), A :: list()) -> string().
%% ====================================================================
netconf(NC, F, A) ->
    Result = ct_netconfc:open(NC, []),
    case Result of 
        {ok, _} ->  Res = apply(ct_netconfc, F, A),
                    Status = case Res of
                                {error, _} ->
                                    Res;
                                _ ->
                                    ct_netconfc:close_session(NC)
                             end,
    %% Result of negative edit requests are seen first when closing session.
                    case Status of
                        ok ->
                             Res;
                        _ ->
                             ct_netconfc:close_session(NC),
                             Status
                    end;
        Other -> ct:pal("netconf command failed, reason ~p", [Other])
    end.

%% extract_element/2
%% ====================================================================
%% @doc From a general short xml syntax, extract a certain xml
%%              element

-spec extract_element(Element :: atom(), List :: list()) -> {ok, tuple()}.
%% ====================================================================
extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

get_oam_suffix(alt_oam) ->
   "_ALT";
get_oam_suffix(oam) ->
    "".

get_oam_id(alt_oam) ->
    "Alternative";
get_oam_id(oam) ->
    "1".

get_oam_ip_protocol() ->
    case is_target() of
        true ->
            case get_node_ip(rpc, oam, ipv6) of
            einval ->
                inet;
            _ ->
                inet6
            end;
        false ->
            inet %% use ipv4 for sim
    end.

get_sftp_server_address(IpProto) when ?INET(IpProto) ->
    proplists:get_value(host, ct:get_config(sftp_server, []), "10.68.101.150");
get_sftp_server_address(IpProto) when ?INET6(IpProto) ->
    proplists:get_value(host, ct:get_config(sftp_server_ipv6, []), "[2001:1b70:6282:b280::150]").

get_sftp_server_username() ->
    proplists:get_value(username, ct:get_config(sftp_server, []), "dustest").

get_sftp_server_password() ->
    proplists:get_value(password, ct:get_config(sftp_server, []), "dustest").

get_ftpes_test_server_address(IpProto) when ?INET(IpProto) ->
    "10.68.101.131";
get_ftpes_test_server_address(IpProto) when ?INET6(IpProto) ->
    "2001:1b70:6282:b280::131".

get_ftpes_test_server_username() ->
    "labuser".

get_ftpes_test_server_password() ->
    "labuser".

get_oam_addr_ref(oam, IpProto) when ?INET(IpProto) ->
    ?OAM_ADDR_REF;
get_oam_addr_ref(oam, IpProto) when ?INET6(IpProto) ->
    ?OAM_ADDR6_REF;
get_oam_addr_ref(alt_oam, IpProto) when ?INET(IpProto) ->
    ?ALT_OAM_ADDR_REF;
get_oam_addr_ref(alt_oam, IpProto) when ?INET6(IpProto) ->
    ?ALT_OAM_ADDR6_REF.

ip_interface_class(IpProto) when ?INET(IpProto) ->
    'InterfaceIPv4';
ip_interface_class(IpProto) when ?INET6(IpProto) ->
    'InterfaceIPv6'.

ip_interface_class_id(IpProto) when ?INET(IpProto) ->
    interfaceIPv4Id;
ip_interface_class_id(IpProto) when ?INET6(IpProto) ->
    'interfaceIPv6Id'.

ip_address_class(IpProto) when ?INET(IpProto) ->
    'AddressIPv4';
ip_address_class(IpProto) when ?INET6(IpProto) ->
    'AddressIPv6'.

ip_address_class_id(IpProto) when ?INET(IpProto) ->
    addressIPv4Id;
ip_address_class_id(IpProto) when ?INET6(IpProto) ->
    'addressIPv6Id'.

%% exec_command/2
%% ====================================================================
%% @doc Executes linux command on node if running on target or 
%%      on HUB if running on SIM

-spec exec_command(SSH :: atom(), Command :: string) -> string().
%% ====================================================================
exec_command(SSH, Command) ->
    exec_command(is_target(), SSH, Command).

exec_command(true, SSH, Command) ->
    %% connect to node with root
    rct_ssh:connect(SSH),
    
    {ok, Result} = rct_ssh:exec(SSH, Command, 5000, "(.*?)", [global, {capture, all, list}]),
    ct:log("Command executed: ~p~nResult: ~p~n", [Command, Result]),
    
    rct_ssh:disconnect(SSH),
    
    Result;

exec_command(false, _SSH, Command) ->
    %% if SIM test, execute OS command
    Result = os:cmd(Command),
    ct:log("Command executed: ~p~nResult: ~p~n", [Command, Result]),
    
    Result.

%% is_sftp_dir_registered/1
%% ====================================================================
%% @doc Checks if given directory is already registered (to avoid extra work)

-spec is_sftp_dir_registered(Dir :: string()) ->
          true | false.
%% ====================================================================
is_sftp_dir_registered(Dir) ->
    
    RegDirs = rct_rpc:call(rpc, sysFi, get_sftp_reg_dirs, [], 10000),
    lists:keymember(Dir, 1, RegDirs).

%% register_sftp_dir/4
%% ====================================================================
%% @doc Registeres given directory as SFTP/FTPES directory

-spec register_sftp_dir(SSH :: atom(), Dir :: string(),
                        Size :: integer(), FileHandler :: atom()) ->
          ok.
%% ====================================================================
register_sftp_dir(SSH, Dir, Size, FileHandler) ->
    register_sftp_dir(is_sftp_dir_registered(Dir), SSH, Dir, Size, FileHandler).

register_sftp_dir(false, SSH, Dir, Size, FileHandler) when
  is_list(Dir), is_integer(Size), is_atom(FileHandler) ->
    SftpDir = ?RCS_DIR ++ "/sftp",
    
    %% Set permissions
    case is_target() of
    true ->
        exec_command(SSH, "chmod -R 777 " ++ SftpDir);
    false ->
        ok
    end,

    %% create directory
    exec_command(SSH, "mkdir " ++ SftpDir ++ "/" ++ Dir),
    
    %% register directory
    case rct_rpc:call(rpc, sysFi, register_sftp_dir, [{undefined, Dir, disc, Size, FileHandler}], 10000) of
    ok ->
        ct:log("Successfully registered directory [~p] in sysFi~n", [Dir]);
    {error, Reason} ->
        ct:log("Failed to register directory [~p] in sysFi~nReason: ~p~n", [Dir, Reason])
    end,
    
    %% set permissions on newly created directories
    case is_target() of
    true ->
        exec_command(SSH, "chmod -R 777 " ++ SftpDir ++ "/" ++ Dir);
    false ->
        ok
    end,
    
    ok;

register_sftp_dir(true, _SSH, Dir, _Size, _FileHandler) ->
    %% already registered
    ct:log("Directory ~p already registered~n", [Dir]),
    ok.

%% create_rop_file/2
%% ====================================================================
%% @doc Creates a dummy ROP file in /rop directory

-spec create_rop_file(Name :: string(), Data :: iodata()) ->
          ok.
%% ====================================================================
create_rop_file(Name, Data) ->
    
    ct:log("Creating ROP file [~p] with content: [~p]~n", [Name ++ ".gz", Data]),
    rct_rpc:call(rpc, pmsDb, rop_file_store, [Name, Data], 10000),
    
    ok.

%% delete_rop_file/1
%% ====================================================================
%% @doc Deletes a dummy ROP file in /rop directory

-spec delete_rop_file(Name :: string()) ->
          ok.
%% ====================================================================
delete_rop_file(Name) ->
    
    ct:log("Deleting ROP file [~p]~n", [Name ++ ".gz"]),
    rct_rpc:call(rpc, pmsDb, rop_file_delete, [Name ++ ".gz"], 10000),
    
    ok.

is_target() ->
    case os:getenv("SIM_OR_TARGET") of
    "target" ->
        true;
    _Sim ->
        false 
    end.

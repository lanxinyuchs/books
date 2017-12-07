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
%%% %CCaseFile:	ftpes_server_suite.erl %
%%% Author: 
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(ftpes_server_suite).

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
%%% R8A/4    2016-11-24   eivmiha    Code editing
%%% R8A/5    2016-12-09   eivmiha    Added tcp_client_ipv6 groups, edited init_per_group for ipv6 groups
%%% R8A/11   2016-12-14   enekdav    Added code coverage
%%% R8A/16   2017-01-04   ekurnik    Added support for using designated FTPES dir
%%% R8A/17   2017-01-10   eivmiha    Added TC-US1-11, 2.4-2b, c, d, e
%%% R8A/18   2017-01-10   enekdav    Added SIZE, NLST, RNFR and RNTO tests
%%% R8A/21   2017-01-17   ekurnik    Switched to ftpes_test_lib in RCT
%%% R9A/1    2017-03-29   ekurnik    Added tests for alternative authentication sequence
%%%--------------------------------------------------------------------

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0,
         all/0
       ]).

-export([
         successful_login/1,
         login_after_login/1,
         unsuccessful_login_sequence_according_to_use_case_12_1/1,
         unsuccessful_login_sequence_according_to_use_case_12_3/1,
         noop_success/1,
         invalid_command/1,
         empty_string_as_command/1,
         successful_quit/1,
         successful_login_quit/1,
         unsuccessful_login_sequence_login_without_security/1,
         re_login/1,
         re_login_rein/1,
         execute_FTP_command_after_unsuccessful_login/1,
         execute_FTP_command_without_login/1,
         setting_port_information_for_data_connection/1,
         setting_invalid_IP_address_in_PORT_command/1,
         setting_wrong_port_number_in_PORT_command/1,
         setting_wrong_IP_address_in_PORT_command/1,
         setting_invalid_port_number_in_PORT_command/1,
         setting_invalid_parameter_format_in_PORT_command/1,
         successful_store/1,
         successful_store_to_existing_file/1,
         unsuccessful_store_invalid_data_port/1,
         unsuccessful_store_client_closes_connection/1,
         successful_rename_of_file/1,
         unsuccessful_rename_of_file_no_rnfr_command/1,
         unsuccessful_rename_of_file_renaming_non_existing_file/1,
         successful_append_to_a_file/1,
         unsuccessful_append_to_a_file/1,
         successful_retrieve_of_a_file/1,
         unsuccessful_retrieve_of_a_file/1,
         unsuccessful_retrieve_invalid_data_port/1,
         unsuccessful_retrieve_client_closes_connection/1,
         setting_valid_data_structure/1,
         setting_invalid_data_structure/1,
         setting_valid_transmission_mode/1,
         setting_invalid_transmission_mode/1,
         setting_valid_file_representation_type/1,
         setting_invalid_file_representation_type/1,
         successful_listing_of_current_directory/1,
         successful_nlisting_of_current_directory/1,
         successful_listing_of_specified_directory/1,
         unsuccessful_listing_files/1,
         successful_size_of_a_file/1,
         unsuccessful_size_of_a_file/1,
         successful_changing_directory/1,
         unsuccessful_changing_directory/1,
         successful_create_folder/1,
         unsuccessful_create_folder/1,
         successful_delete_folder/1,
         unsuccessful_delete_folder/1,
         successful_delete_file/1,
         unsuccessful_delete_file/1,
         successful_printing_working_directory/1,
         successful_listing_features/1,
         unsuccessful_listing_features/1,
         successful_changing_options/1,
         unsuccessful_changing_options/1,
         unsuccessful_changing_options_no_args/1,
         unsuccessful_changing_options_wrong_command/1,
         unsuccessful_changing_options_wrong_command_with_args/1,
         unsuccessful_changing_options_no_command_and_args/1,
         valid_sequence_of_security_handshake/1,
         valid_sequence_of_security_handshake_w_user/1,
         invalid_sequence_of_security_handshake/1,
         successful_authorization/1,
         unknown_authorization_parameter/1,
         successful_protection_buffer_setting/1,
         unsuccessful_protection_buffer_setting_wrong_sequence/1,
         invalid_protection_buffer_parameter/1,
         successful_protection_level_setting/1,
         unsuccessful_protection_level_setting_unsupported_level/1,
         unsuccessful_protection_level_setting_invalid_level/1,
         unsuccessful_protection_level_setting_wrong_sequence/1,
         successful_EPSV_command_for_current_protocol/1,
         successful_EPSV_command_for_IPv4/1,
         successful_EPSV_command_for_IPv6/1,
         successful_EPSV_ALL_command/1,
         unsuccessful_EPSV_command_for_non_existent_protocol/1,
         unuccessful_EPSV_command_for_current_protocol_due_to_client_reject/1
         ]).
-export([check_create_folder/1,
         check_create_file/1,
         check_delete_file/1,
         check_delete_folder/1]).

-include_lib("common_test/include/ct.hrl").
-include("$RCT_TOP/test/lib/lib/esrc/ftpes_test_lib.hrl").
-include_lib("kernel/include/file.hrl").

-define (IS_LOGIN_TEST(TestCase), TestCase =:= successful_login orelse
                         TestCase =:= unsuccessful_login_sequence_according_to_use_case_12_1 orelse
                         TestCase =:= unsuccessful_login_sequence_according_to_use_case_12_3 orelse
                         TestCase =:= successful_quit orelse
                         TestCase =:= successful_login_quit orelse
                         TestCase =:= unsuccessful_login_sequence_login_without_security orelse
                         TestCase =:= re_login orelse
                         TestCase =:= re_login_rein orelse
                         TestCase =:= execute_FTP_command_after_unsuccessful_login orelse
                         TestCase =:= execute_FTP_command_without_login orelse
                         TestCase =:= valid_sequence_of_security_handshake orelse
                         TestCase =:= valid_sequence_of_security_handshake_w_user orelse
                         TestCase =:= invalid_sequence_of_security_handshake orelse
                         TestCase =:= successful_authorization orelse
                         TestCase =:= unknown_authorization_parameter orelse
                         TestCase =:= successful_protection_buffer_setting orelse
                         TestCase =:= unsuccessful_protection_buffer_setting_wrong_sequence orelse
                         TestCase =:= invalid_protection_buffer_parameter orelse
                         TestCase =:= successful_protection_level_setting orelse
                         TestCase =:= unsuccessful_protection_level_setting_unsupported_level orelse
                         TestCase =:= unsuccessful_protection_level_setting_invalid_level orelse
                         TestCase =:= unsuccessful_protection_level_setting_wrong_sequence).

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------

-spec suite() -> [tuple()].

suite() ->
   [{timetrap, {minutes, 10}},
    {ct_hooks, [{rct_rpc, rpc},
                {rct_netconf,{nc1, html}},
                {rct_scp, scp1},
                {rct_ssh,{ssh,[manual_connect]}},
                {rct_htmllink,[]},
                {rct_logging, {all,
                                [{erlang,
                                  {["ERROR REPORT","CRASH REPORT"],
                                   ["exception exit: unexpected_exit",
                                    "\\*\\* unexpected_exit"]}
                                }]}},
        {rct_core,[]},
        {cover_hook,[{du1, username}]}
           ]}].

groups() ->
    TCPGroup = tcp(),
    FTPGroup = ftp(),
    TCPPassiveGroup= tcp_pasv(),
    FTPPassiveGroup= ftp_pasv(),
    TCPIPv4Ext = tcp_ipv4_ext(),
    TCPIPv4ExtPassive = tcp_ipv4_ext_passive() ,
    FTPIPv4Ext = ftp_ipv4_ext(),
    FTPIPv4ExtPassive = ftp_ipv4_ext_passive() ,
    TCPIpv6 =  tcp_ipv6(),
    TCPIpv6Passive =tcp_ipv6_passive(),
    FtpClientIpv6 = ftp_client_ipv6(),
    FtpClientIpv6Passive = ftp_client_ipv6_passive(),
    [
     {tcp_group, [], TCPGroup},
     {ftp_client_group, [], FTPGroup},
     {tcp_passive_group, [], TCPPassiveGroup},
     {ftp_client_passive_group, [], FTPPassiveGroup},
     {tcp_ipv4_ext_group, [], TCPIPv4Ext},
     {tcp_ipv4_ext_passive_group, [], TCPIPv4ExtPassive},
     {ftp_client_ipv4_ext_group, [], FTPIPv4Ext},
     {ftp_client_ipv4_ext_passive_group, [], FTPIPv4ExtPassive},
     {tcp_ipv6_group, [], TCPIpv6},
     {tcp_ipv6_passive_group, [], TCPIpv6Passive},
     {ftp_client_ipv6_group, [],FtpClientIpv6 },
     {ftp_client_ipv6_passive_group, [], FtpClientIpv6Passive}
    ].

all() ->
     [
     {group, tcp_group},
     {group, ftp_client_group},
     {group, tcp_passive_group},
     {group, ftp_client_passive_group},
     {group, tcp_ipv4_ext_group},
     {group, tcp_ipv4_ext_passive_group},
     {group, ftp_client_ipv4_ext_group},
     {group, ftp_client_ipv4_ext_passive_group},
     {group, tcp_ipv6_group},
     {group, tcp_ipv6_passive_group},
     {group, ftp_client_ipv6_group},
     {group, ftp_client_ipv6_passive_group}
     ].

tcp() ->
    [
     successful_login,
     unsuccessful_login_sequence_according_to_use_case_12_1,
     unsuccessful_login_sequence_according_to_use_case_12_3,
     noop_success,
     invalid_command,
     empty_string_as_command,
     successful_quit,
     successful_login_quit,
     unsuccessful_login_sequence_login_without_security,
     re_login,
     re_login_rein,
     execute_FTP_command_after_unsuccessful_login,
     execute_FTP_command_without_login,
     setting_port_information_for_data_connection,
     setting_invalid_parameter_format_in_PORT_command,
     setting_invalid_IP_address_in_PORT_command,
     setting_wrong_IP_address_in_PORT_command,
     setting_invalid_port_number_in_PORT_command,
     setting_wrong_port_number_in_PORT_command,
     setting_valid_data_structure,
     setting_invalid_data_structure,
     setting_valid_transmission_mode,
     setting_invalid_transmission_mode,
     setting_valid_file_representation_type,
     setting_invalid_file_representation_type,
     successful_store,
     successful_store_to_existing_file,
     unsuccessful_store_client_closes_connection,
     unsuccessful_store_invalid_data_port,
     successful_rename_of_file,
     unsuccessful_rename_of_file_no_rnfr_command,
     unsuccessful_rename_of_file_renaming_non_existing_file,
     successful_append_to_a_file,
     unsuccessful_append_to_a_file,
     successful_retrieve_of_a_file ,
     unsuccessful_retrieve_of_a_file,
     unsuccessful_retrieve_invalid_data_port,
     unsuccessful_retrieve_client_closes_connection,
     successful_create_folder,
     unsuccessful_create_folder,
     successful_changing_directory,
     unsuccessful_changing_directory,
     successful_printing_working_directory,
     successful_listing_of_current_directory,
     successful_nlisting_of_current_directory,
     successful_listing_of_specified_directory,
     unsuccessful_listing_files,
     successful_size_of_a_file,
     unsuccessful_size_of_a_file,
     successful_delete_folder,
     unsuccessful_delete_folder,
     successful_delete_file,
     unsuccessful_delete_file,
     successful_listing_features,
     unsuccessful_listing_features,
     successful_changing_options,
     unsuccessful_changing_options,
     unsuccessful_changing_options_no_args,
     unsuccessful_changing_options_wrong_command,
     unsuccessful_changing_options_wrong_command_with_args,
     unsuccessful_changing_options_no_command_and_args,
     valid_sequence_of_security_handshake,
     valid_sequence_of_security_handshake_w_user,
     invalid_sequence_of_security_handshake,
     successful_authorization,
     unknown_authorization_parameter,
     successful_protection_buffer_setting,
     unsuccessful_protection_buffer_setting_wrong_sequence,
     invalid_protection_buffer_parameter,
     successful_protection_level_setting,
     unsuccessful_protection_level_setting_unsupported_level,
     unsuccessful_protection_level_setting_invalid_level,
     unsuccessful_protection_level_setting_wrong_sequence
     ].

ftp() ->
    [
     successful_login,
     %login_after_login,
     successful_quit,
     successful_login_quit,
     re_login,
     setting_valid_file_representation_type,
     setting_invalid_file_representation_type,
     successful_store,
     successful_store_to_existing_file,
     unsuccessful_store_client_closes_connection,
     successful_rename_of_file,
     successful_append_to_a_file,
     unsuccessful_append_to_a_file,
     successful_retrieve_of_a_file , 
     unsuccessful_retrieve_of_a_file,
     unsuccessful_retrieve_client_closes_connection,
     successful_create_folder,
     unsuccessful_create_folder,
     successful_changing_directory,
     unsuccessful_changing_directory,
     successful_printing_working_directory,
     successful_listing_of_current_directory,
     successful_nlisting_of_current_directory,
     unsuccessful_listing_files,
     successful_delete_folder,
     unsuccessful_delete_folder,
     successful_delete_file,
     unsuccessful_delete_file
    ].

tcp_pasv() ->
    [
     successful_store,
     successful_store_to_existing_file,
     unsuccessful_store_client_closes_connection,
     unsuccessful_rename_of_file_no_rnfr_command,
     unsuccessful_rename_of_file_renaming_non_existing_file,
     successful_retrieve_of_a_file,
     successful_append_to_a_file,
     unsuccessful_retrieve_of_a_file,
     successful_listing_of_current_directory,
     successful_nlisting_of_current_directory,
     unsuccessful_listing_files
    ].

ftp_pasv()  -> 
    [
     successful_store,
     successful_store_to_existing_file,
     unsuccessful_store_client_closes_connection,
     successful_append_to_a_file,
     successful_retrieve_of_a_file ,
     unsuccessful_retrieve_of_a_file,
     successful_listing_of_current_directory,
     successful_nlisting_of_current_directory,
     unsuccessful_listing_files,
     successful_create_folder,
     unsuccessful_create_folder,
     successful_changing_directory,
     unsuccessful_changing_directory,
     successful_delete_folder,
     unsuccessful_delete_folder,
     successful_delete_file,
     unsuccessful_delete_file
    ].

tcp_ipv4_ext() -> 
    [
     setting_port_information_for_data_connection,
     setting_invalid_parameter_format_in_PORT_command,
     setting_invalid_IP_address_in_PORT_command,
     setting_wrong_IP_address_in_PORT_command,
     setting_invalid_port_number_in_PORT_command,
     setting_wrong_port_number_in_PORT_command,
     successful_store,
     successful_append_to_a_file,
     successful_retrieve_of_a_file
     ].

tcp_ipv4_ext_passive() -> 
    [
     successful_store,
     successful_retrieve_of_a_file,
     successful_EPSV_command_for_current_protocol,
     successful_EPSV_command_for_IPv4,
     successful_EPSV_ALL_command,
     unsuccessful_EPSV_command_for_non_existent_protocol,
     unuccessful_EPSV_command_for_current_protocol_due_to_client_reject
     ].

ftp_ipv4_ext() -> 
    [
     successful_store,
     successful_store_to_existing_file,
     unsuccessful_store_client_closes_connection,
     successful_append_to_a_file,
     successful_retrieve_of_a_file ,
     unsuccessful_retrieve_of_a_file
     ].

ftp_ipv4_ext_passive() -> 
    [
     successful_store,
     successful_store_to_existing_file,
     unsuccessful_store_client_closes_connection,
     successful_append_to_a_file,
     successful_retrieve_of_a_file ,
     unsuccessful_retrieve_of_a_file
     ].

tcp_ipv6() -> 
    [
     setting_port_information_for_data_connection,
     setting_invalid_IP_address_in_PORT_command,
     setting_wrong_IP_address_in_PORT_command,
     setting_invalid_port_number_in_PORT_command,
     setting_wrong_port_number_in_PORT_command,
     successful_store,
     successful_append_to_a_file,
     successful_retrieve_of_a_file
     ].

tcp_ipv6_passive() -> 
    [
     successful_store,
     successful_retrieve_of_a_file,
     successful_EPSV_command_for_current_protocol,
     successful_EPSV_ALL_command,
     unsuccessful_EPSV_command_for_non_existent_protocol,
     unuccessful_EPSV_command_for_current_protocol_due_to_client_reject
     ].

ftp_client_ipv6() -> 
    [
     successful_store,
     successful_append_to_a_file,
     successful_retrieve_of_a_file
     ].

ftp_client_ipv6_passive() -> 
    [
     successful_store,
     successful_append_to_a_file,
     successful_retrieve_of_a_file
     ].


%% @hidden
init_per_suite(Config) ->
    User = "stef",
    DataDir = ?config(data_dir, Config),
    ok = ftpes_test_lib:register_sftp_dir(ssh, ?FTPES_TEST_DIR, 1024, ssh_sftpd_file),
    Started = ftpes_test_lib:start_server(),
    
    ssl:start(),
    UpdatedConfig = ftpes_test_lib:initialize_nc_tc(Config),
    ftpes_test_lib:enable_ftpes_tls([{firstNC, "ftpes_test_login"}, {firstTC, "ftpes_test_login_tc"} |Config]),
    timer:sleep(2000),
    [{data_port,25600}, {port, ?DEFAULT_SERVER_CONTROL_CONNECTION_PORT}, {user, User}, {started, Started},
     {certificate, [{certfile, DataDir ++ "user_expert.crt"}, {keyfile, DataDir ++ "user_expert.key"}]}|UpdatedConfig].

%% @hidden
end_per_suite(Config) ->
    ftpes_test_lib:disable_ftpes_tls(),
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_ftpes_server(rpc),
              ftpes_test_lib:stop_ftpes_sup(rpc);
        yes -> ok
    end,
    ftpes_test_lib:clean_nc_tc(Config).

%% @hidden
init_per_group(tcp_group, Config) ->
    Connection_mode = tcp,
    Port = ?config(port, Config), 
    Type = active,
    IpProtocol = ipv4,
    Host = ftpes_test_lib:get_node_ip(rpc, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "Tcp"},
                            {ip_protocol, IpProtocol}, {host, Host}, {args, [{port, Port}]} |Config]
    end;

%% @hidden
init_per_group(ftp_client_group, Config) ->
    Connection_mode = ftp_client,
    Type = active,
    IpProtocol = ipv4,
    Port = ?config(port, Config),
    FilePrefix = "FtpClient",
    Certificate = ?config(certificate, Config),
    Host = ftpes_test_lib:get_node_ip(rpc, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, FilePrefix}, 
                            {ip_protocol, IpProtocol}, {host, Host},{args,[{port, Port},{mode, Type}, {reuseaddr, true},
                            {tls, Certificate}]} |Config]
    end;

init_per_group(tcp_passive_group, Config) ->
    Connection_mode = tcp,
    Port = ?config(port, Config),
    Type = passive,
    IpProtocol = ipv4,
    Host = ftpes_test_lib:get_node_ip(rpc, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "Tcp"},
                            {ip_protocol, IpProtocol}, {host, Host}, {args, [{port, Port}]} |Config]
    end;

%% @hidden
init_per_group(ftp_client_passive_group, Config) ->
    Connection_mode = ftp_client,
    Type = passive,
    Port = ?config(port, Config),
    IpProtocol = ipv4,
    FilePrefix = "FtpClient",
    Certificate = ?config(certificate, Config),
    Host = ftpes_test_lib:get_node_ip(rpc, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, FilePrefix}, 
                           {ip_protocol, IpProtocol}, {host, Host},{args,[{port, Port},{mode, Type}, {reuseaddr, true},
                           {tls, Certificate}]} |Config]
    end;

%%@hidden
init_per_group(tcp_ipv4_ext_group, Config) ->
    Connection_mode = tcp,
    Type = active,
    Port = ?config(port, Config),
    IpProtocol = ipv4_ext,
    Host = ftpes_test_lib:get_node_ip(rpc, ipv4),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "TcpIpv4Ext"},
                            {ip_protocol, IpProtocol}, {host, Host}, {args, [{port, Port}]} |Config]
    end;
    
%%@hidden
init_per_group(tcp_ipv4_ext_passive_group, Config) ->
    Connection_mode = tcp,
    Type = passive,
    Port = ?config(port, Config),
    IpProtocol = ipv4_ext,
    Host = ftpes_test_lib:get_node_ip(rpc, ipv4),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "TcpIpv4Ext"},
                            {ip_protocol, IpProtocol}, {host, Host}, {args, [{port, Port}]} |Config]
    end;

%%@hidden
init_per_group(ftp_client_ipv4_ext_group, Config) ->
    Connection_mode = ftp_client,
    Type = active,
    Port = ?config(port, Config),
    IpProtocol = ipv4_ext,
    Certificate = ?config(certificate, Config),
    Host = ftpes_test_lib:get_node_ip(rpc, ipv4),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "FtpIpv4Ext"}, 
                           {ip_protocol, IpProtocol}, {host, Host},{args,[{port, Port},{mode, Type}, {reuseaddr, true},
                           {ftp_extension, true}, {tls, Certificate}]} |Config]
    end;
    
%%@hidden
init_per_group(ftp_client_ipv4_ext_passive_group, Config) ->
    Connection_mode = ftp_client,
    Type = passive,
    Port = ?config(port, Config),
    IpProtocol = ipv4_ext,
    Certificate = ?config(certificate, Config),
    Host = ftpes_test_lib:get_node_ip(rpc, ipv4),
    if (Host == einval) -> {skip, "This group expects ipv4 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "FtpIpv4Ext"}, 
                           {ip_protocol, IpProtocol}, {host, Host},{args,[{port, Port},{mode, Type}, {reuseaddr, true},
                           {ftp_extension, true}, {tls, Certificate}]} |Config]
    end;

%%@hidden
init_per_group(tcp_ipv6_group, Config) ->
    Connection_mode = tcp,
    Type = active,
    Port = ?config(port, Config),
    InetMode = inet6,
    IpProtocol = ipv6,
    Host = ftpes_test_lib:get_node_ip(rpc, oam, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv6 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "TcpIpv6"},
                            {inet_mode, InetMode}, {ip_protocol, IpProtocol}, {host, Host}, {args, [{port, Port}]}|Config]
    end;

%%@hidden
init_per_group(tcp_ipv6_passive_group, Config) ->
    Connection_mode = tcp,
    Type = passive,
    Port = ?config(port, Config),
    InetMode = inet6,
    IpProtocol = ipv6,
    Host = ftpes_test_lib:get_node_ip(rpc, oam, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv6 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, "TcpIpv6Passive"},
                            {inet_mode, InetMode}, {ip_protocol, IpProtocol}, {host, Host}, {args, [{port, Port}]}|Config]
    end;

%% @hidden
init_per_group(ftp_client_ipv6_group, Config) ->
    Connection_mode = ftp_client,
    Type = active,
    Port = ?config(port, Config),
    FilePrefix = "FtpClientIpv6",
    InetMode = inet6,
    IpProtocol = ipv6,
    Certificate = ?config(certificate, Config),
    Host = ftpes_test_lib:get_node_ip(rpc, oam, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv6 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, FilePrefix}, 
                            {inet_mode, InetMode}, {ip_protocol, IpProtocol}, {host, Host},
                            {args, [{port, Port}, {ipfamily, inet6}, {mode, Type}, {reuseaddr, true},{ftp_extension, true},
                            {tls, Certificate}]} |Config]
    end;
    
%% @hidden
init_per_group(ftp_client_ipv6_passive_group, Config) ->
    Connection_mode = ftp_client,
    Type = passive,
    Port = ?config(port, Config),
    InetMode = inet6,
    IpProtocol = ipv6,
    FilePrefix = "FtpClientIpv6",
    Certificate = ?config(certificate, Config),
    Host = ftpes_test_lib:get_node_ip(rpc, oam, IpProtocol),
    if (Host == einval) -> {skip, "This group expects ipv6 address"};
                   true -> [{connection_mode,Connection_mode}, {type, Type}, {file_prefix, FilePrefix}, 
                            {inet_mode, InetMode}, {ip_protocol, IpProtocol}, {host, Host},
                            {args, [{port, Port}, {ipfamily, inet6}, {mode, Type}, {reuseaddr, true},{ftp_extension, true},
                            {tls, Certificate}]} |Config]
    end.

%% @hidden
end_per_group(_GroupName, Config) ->
    Config.

%% if test is about login, don't login in preconditions, just establish connection
init_per_testcase(TestCase, Config) when ?IS_LOGIN_TEST(TestCase)->
    set_control_connection(Config);

%% @hidden 
init_per_testcase(successful_store, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_store_to_existing_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(unsuccessful_store_client_closes_connection,Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_rename_of_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(unsuccessful_rename_of_file_no_rnfr_command, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(unsuccessful_rename_of_file_renaming_non_existing_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_listing_of_current_directory, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    create_file_on_server(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_nlisting_of_current_directory, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    create_file_on_server(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_delete_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    create_file_on_server(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_size_of_a_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_delete_folder, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_folder(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_create_folder, Config) ->
    ftpes_test_lib:setup_control_connection(Config);
    
%% @hidden 
init_per_testcase(unsuccessful_create_folder, Config) -> % folder already exists
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_folder(NewConfig),
    NewConfig;

%% @hidden 
init_per_testcase(successful_changing_directory, Config) -> % folder already exists
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_folder(NewConfig),
    NewConfig;


init_per_testcase(successful_append_to_a_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig;

init_per_testcase(unsuccessful_append_to_a_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    check_create_file(NewConfig),
    NewConfig; 

init_per_testcase( successful_retrieve_of_a_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    create_file_on_server(NewConfig),
    NewConfig; 

init_per_testcase( unsuccessful_retrieve_of_a_file, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    create_file_on_server(NewConfig),
    NewConfig; 

init_per_testcase(unsuccessful_retrieve_invalid_data_port, Config) ->
    NewConfig = ftpes_test_lib:setup_control_connection(Config),
    create_file_on_server(NewConfig),
    NewConfig;

%% @hidden
init_per_testcase(_TestCase, Config) -> 
    ftpes_test_lib:setup_control_connection(Config).

%% @hidden
end_per_testcase(successful_store, Config) ->
     check_delete_file(Config),
     close_control_connection(Config);

%% @hidden
end_per_testcase(successful_store_to_existing_file, Config) ->
     check_delete_file(Config),
     close_control_connection(Config);

%% @hidden
end_per_testcase(unsuccessful_store_client_closes_connection, Config) ->
     check_delete_file(Config),
     close_control_connection(Config);

%% @hidden
end_per_testcase(successful_rename_of_file, Config) ->
     close_control_connection(Config);

%% @hidden
end_per_testcase(unsuccessful_rename_of_file_no_rnfr_command, Config) ->
     check_delete_file(Config),
     close_control_connection(Config);

%% @hidden
end_per_testcase(unsuccessful_rename_of_file_renaming_non_existing_file, Config) ->
     check_delete_file(Config),
     close_control_connection(Config);

%% @hidden
end_per_testcase(successful_listing_of_current_directory, Config) ->
     close_control_connection(Config);

%% @hidden
end_per_testcase(successful_nlisting_of_current_directory, Config) ->
     close_control_connection(Config);

%% @hidden
end_per_testcase(successful_size_of_a_file, Config) ->
     check_delete_file(Config),
     close_control_connection(Config);

%% @hidden
end_per_testcase(successful_create_folder, Config) ->
     check_delete_folder(Config),
     close_control_connection(Config);

%% @hidden
end_per_testcase(unsuccessful_create_folder, Config) ->
     check_delete_folder(Config),
     close_control_connection(Config);

%% @hidden 
end_per_testcase(successful_delete_file, Config) ->
    check_delete_file(Config),
    close_control_connection(Config);

%% @hidden 
end_per_testcase(successful_changing_directory, Config) ->
    Handler = ?config(socket, Config),
    ftpes_test_lib:ftp_cd(Handler, "/" ++ ?FTPES_TEST_DIR),
    check_delete_folder(Config),
    close_control_connection(Config);

%% @hidden 
end_per_testcase(successful_append_to_a_file, Config) ->
    check_delete_file(Config),
    close_control_connection(Config);

%% @hidden 
end_per_testcase(unsuccessful_append_to_a_file, Config) ->
    check_delete_file(Config),
    close_control_connection(Config);

 %% @hidden 
end_per_testcase(successful_retrieve_of_a_file, Config) ->
    check_delete_file(Config),
    close_control_connection(Config);

 %% @hidden 
end_per_testcase(unsuccessful_retrieve_of_a_file, Config) ->
    check_delete_file(Config),
    close_control_connection(Config);
 %% @hidden 
end_per_testcase(unsuccessful_retrieve_invalid_data_port,Config) ->
    check_delete_file(Config),
    close_control_connection(Config);

%% @hidden 
end_per_testcase(_TestCase, Config) ->
   close_control_connection(Config).

check_create_folder(Config) ->
    Handler = ?config(socket, Config),
    case ftpes_test_lib:ftp_mkdir(Handler, ?TEST_DIR_NAME) of
       {ok, _msg} -> 
           ct:log("Folder for data test doesn't exist, now created");
        _ ->
             ct:log("Folder for data test already exists")
    end.

check_delete_folder(Config) ->
    Handler = ?config(socket,  Config),
    case ftpes_test_lib:ftp_rmdir(Handler, ?TEST_DIR_NAME) of
        {ok, _msg} -> 
             ct:log("Delete folder after data test");
        Smt ->  %delete folder
           ct:log("Folder deleted after data test ~p", [Smt])
    end.

%for store tests    
check_create_file(Config) ->
  FilePrefix = ?config(file_prefix, Config),
  case file:open(FilePrefix ++ ?TEST_FILE_NAME , read) of
        {ok, IoDevice}-> %file exists
            ct:log("File for data test already exists"),
            file:close(IoDevice);
      _ ->  %file doesn't exist
            ct:log("Create file for data test"),
            file:write_file(FilePrefix ++ ?TEST_FILE_NAME, "Test content"),
            {ok, FileInfo}= file:read_file_info(FilePrefix ++ ?TEST_FILE_NAME),
            NewFileInfo = FileInfo#file_info{access= none},
            file:write_file_info(FilePrefix ++ ?TEST_FILE_NAME, NewFileInfo)
  end.


create_file_on_server(Config) ->
     FilePrefix = ?config(file_prefix,Config),
     Type = ?config(type, Config),
     Port = ?config(data_port, Config),
     IpProtocol = ?config(ip_protocol, Config),
     DataDir = ?config(data_dir, Config),
     Host = ?config(host, Config),
     check_create_file(Config),
     Handler = ?config(socket, Config),
     Certificate = ?config(certificate, Config),
     ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME ,#ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}).

check_delete_file(Config) ->
     Handler = ?config(socket, Config),
     FilePrefix = ?config(file_prefix, Config),
     case ftpes_test_lib:ftp_delete(Handler, FilePrefix ++ ?TEST_FILE_NAME ) of
         {ok, _Msg} -> 
             ct:log("Delete file after data test");
        {error, Reason} -> 
              ct:log("File after data test already deleted, Reason ~p", [Reason])
     end.

close_control_connection(Config) ->
    Socket = ?config(socket, Config),
    ok = ftpes_test_lib:close_ftp_connection(Socket).

set_control_connection(Config) ->
    ftpes_test_lib:flush_messages(),
    Connection_mode = ?config(connection_mode,Config),
    IpAddress = ?config(host, Config),
    Args = ?config(args, Config),
    {ok, Pid} = ftpes_test_lib:open_ftp_control_connection(Connection_mode, IpAddress, Args),
    NewConf =[{socket,Pid}|Config],
    NewConf.

%% @hidden TC-US2.1-1, TC-US1-1, TC-US3-4
%% Successful login sequence - according to use case 12.2
%% TC-US1-1 Obsolete, replaced by TC-US3-4
successful_login(Config) ->
     Handler = ?config(socket, Config),
     User = ?config(user, Config),
     
     {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
     ok = ftpes_test_lib:ftp_user(SSLSocket, User, ""),
     NewConfig.

%% @hidden
%% Login after successful login sequence
login_after_login(Config) ->
     Handler = ?config(socket, Config),
     User = ?config(user, Config),
     
     {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
     ok = ftpes_test_lib:ftp_user(SSLSocket, User, ""),
     {error, _} = ftpes_test_lib:ftp_user(SSLSocket, User, ""),
     NewConfig.

%% @hidden TC-US3-4a
%% Unsuccessful login sequence - according to use case 12.1
unsuccessful_login_sequence_according_to_use_case_12_1(Config) ->
     Handler = ?config(socket, Config),
     
     {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
     ok= ftpes_test_lib:ftp_user(SSLSocket, "stef", ""),
     {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PASS", [], ?FTP_SECURITY_DATA_EXCHANGE_NOT_DONE),
     NewConfig.

%% @hidden TC-US3-4b
%% Unsuccessful login sequence - according to use case 12.3
unsuccessful_login_sequence_according_to_use_case_12_3(Config) ->
    Handler = ?config(socket, Config),
    
    {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
    ok= ftpes_test_lib:ftp_user(SSLSocket, "stef", ""),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "CCC", [], ?FTP_RESPONSE_COMMAND_NOT_IMPLEMENTED),
    NewConfig.


%% @hidden TC-US1-9
%% Successful NOOP operation
noop_success(Config) ->
    Handler = ?config(socket, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "NOOP", [], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-US1-10
%% Sending invalid commands
invalid_command(Config) ->
    Handler = ?config(socket, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "WHATEVER", [], ?FTP_RESPONSE_COMMAND_NOT_IMPLEMENTED),
    Config.

%% @hidden TC-US1-11
empty_string_as_command(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "", [], ?FTP_RESPONSE_UNSUCCESSFUL),
    Config.

%% @hidden TC-US1-2
%% Successful quit
successful_quit(Config) ->
    Handler = ?config(socket, Config),
    
    {ok, _} = ftpes_test_lib:send_quit(Handler),
    Config.

%% @hidden TC-US1-2a
%% Successful login and quit
successful_login_quit(Config) ->
    Handler = ?config(socket, Config),
    
    {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
    ok = ftpes_test_lib:ftp_user(SSLSocket, "stef", ""),
    Handler2 = ?config(socket, NewConfig),
    {ok, _} = ftpes_test_lib:send_quit(Handler2),
    NewConfig.

%% @hidden TC-US3-4c
%% Unsuccessful login sequence - login without security
unsuccessful_login_sequence_login_without_security(Config) ->
    Handler = ?config(socket, Config),
    
    {error, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "USER", ["stef"], ?FTP_SECURITY_DATA_EXCHANGE_NOT_DONE),
    Config.
     
%% @hidden TC-US2.1-1d, TC-US3-5b
%%  Re-login
re_login(Config) ->
    Handler = ?config(socket, Config), 

    {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
    ok = ftpes_test_lib:ftp_user(SSLSocket, "stef", ""),
    {error, _} = ftpes_test_lib:ftp_user(SSLSocket, "stef", ""),
    NewConfig.

re_login_rein(Config) ->
    Handler = ?config(socket, Config), 

    {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
    ok = ftpes_test_lib:ftp_user(SSLSocket, "stef", ""),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "REIN", [], ?FTP_RESPONSE_HELLO), %% 220, same as hello msg
    {error, _} = ftpes_test_lib:ftp_pwd(SSLSocket),
    ok = ftpes_test_lib:ftp_user(SSLSocket, "stef", ""),
    {ok, _} = ftpes_test_lib:ftp_pwd(SSLSocket),
    NewConfig.

%% @hidden  TC-US2.1-1c, TC-US3-5a
%% Execute FTP command after unsuccessful login
execute_FTP_command_after_unsuccessful_login(Config) ->
    Handler = ?config(socket, Config), 
    
    {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PASS", ["stef"], ?FTP_RESPONSE_NOT_LOGGED_IN),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PWD", [], ?FTP_RESPONSE_NOT_LOGGED_IN),
    NewConfig.

%% @hidden TC-US2.1-1b, TC-US3-5
%% Execute FTP command witout login
execute_FTP_command_without_login(Config) ->
    Handler = ?config(socket, Config),
    
    {SSLSocket, NewConfig} = ftpes_test_lib:authentication_sequence(Handler, Config),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PWD", [], ?FTP_RESPONSE_NOT_LOGGED_IN),
    NewConfig.
    
%% @hidden TC-US1-3, TC-US2.5-1, TC-US2.5-2,
%% Setting port information for data connection
setting_port_information_for_data_connection(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    Host = ?config(host, Config),
    DataDir = ?config(data_dir, Config),
    IpProtocol = ?config(ip_protocol, Config),
    Certificate = ?config(certificate, Config),
    
    _Pid= spawn(fun()-> ftpes_test_lib:open_ftp_data_connection(Type, Host, Port, {IpProtocol, DataDir, Certificate} ,self()) end),
    timer:sleep(100),
    io:format("Handler: ~p", [Handler]),
    io:format("Client ip: ~p", [ftpes_test_lib:get_client_ip(Handler, IpProtocol)]),
    io:format("Port: ~p", [Port]),
    io:format("IpProtocol: ~p", [IpProtocol]),
    {ok, _}= ftpes_test_lib:send_recv_ftp_command(Handler, fun() -> ftpes_test_lib:send_port_command(Handler, ftpes_test_lib:get_client_ip(Handler, IpProtocol), Port, IpProtocol) end, 
                                              ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-US1-3a, , TC-US2.5-1b
%% Setting invalid parameter format in PORT command
setting_invalid_parameter_format_in_PORT_command(Config) ->
    Handler = ?config(socket,Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "PORT", ["127,0,0,0,45,35,66,77"], ?INVALID_NUMBER_OF_ARGUMENTS),
    
    Config.

%% @hidden TC-US1-3b, TC-US2.5-1c
%% Setting invalid IP address in PORT command
setting_invalid_IP_address_in_PORT_command(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    Host = ?config(host, Config),
    IpProtocol = ?config(ip_protocol, Config),
    InvalidHost = case IpProtocol of
                    ipv4 -> {327,0,0,1};
                    ipv4_ext -> {327,0,0,1};
                    ipv6 -> {192,168,1,100,100,100,100,70025}
                  end,                    
    DataDir = ?config(data_dir, Config),
    Certificate = ?config(certificate, Config),

    _Pid =  spawn(fun() -> ftpes_test_lib:open_ftp_data_connection(Type, Host, Port, {IpProtocol, DataDir, Certificate}, self()) end),
    timer:sleep(100),
    {ok, _} = ftpes_test_lib:send_recv_ftp_command(Handler, fun() -> ftpes_test_lib:send_port_command(Handler, InvalidHost, Port, IpProtocol) end,
                                                        ?INVALID_NUMBER_OF_ARGUMENTS),
    ftpes_test_lib:expected_timeout(),
    Config.

%% @hidden TC-US1-3c, TC-US2.5-1d
%% Setting wrong IP address in PORT command
setting_wrong_IP_address_in_PORT_command(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    Host = ?config(host, Config),
    IpProtocol = ?config(ip_protocol, Config),
    WrongHost = case IpProtocol of
                    ipv4 ->  {200,0,0,1};
                    ipv4_ext -> {200,0,0,1};
                    ipv6 -> {12,0,0,0,0,0,0,1}
                end,
    DataDir = ?config(data_dir, Config),
    Certificate = ?config(certificate, Config),
    
    _Pid = spawn(fun() -> ftpes_test_lib:open_ftp_data_connection(Type, Host, Port, {IpProtocol, DataDir, Certificate}, self()) end),
    timer:sleep(100),
    {ok, _} = ftpes_test_lib:send_recv_ftp_command(Handler, fun() -> ftpes_test_lib:send_port_command(Handler, WrongHost, Port, IpProtocol) end,
                                               ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    ftpes_test_lib:expected_timeout(),
    Config.

%% @hidden TC-US1-3d, TC-US2.5-1e
%% Setting invalid port number in PORT command
setting_invalid_port_number_in_PORT_command(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    Host = ?config(host, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    InvalidPort = 59876954325,
    Certificate = ?config(certificate, Config),
    
    _Pid = spawn(fun() -> ftpes_test_lib:open_ftp_data_connection(Type, Host, Port, {IpProtocol, DataDir, Certificate}, self()) end),
    timer:sleep(100),
    {ok, _} = ftpes_test_lib:send_recv_ftp_command(Handler, fun() -> ftpes_test_lib:send_port_command(Handler, ftpes_test_lib:get_client_ip(Handler, IpProtocol), InvalidPort, IpProtocol) end,
                                               ?INVALID_NUMBER_OF_ARGUMENTS),
    ftpes_test_lib:expected_timeout(),
    Config.

%% @hidden TC-US1-3e, TC-US2.5-1f
%% Setting wrong port number in PORT command
setting_wrong_port_number_in_PORT_command(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    Host = ?config(host, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Certificate = ?config(certificate, Config),
    
    _Pid =  spawn(fun() -> ftpes_test_lib:open_ftp_data_connection(Type, Host, Port, {IpProtocol, DataDir, Certificate}, self()) end),
    timer:sleep(100),
    {ok, _} = ftpes_test_lib:send_recv_ftp_command(Handler, fun() -> ftpes_test_lib:send_port_command(Handler, ftpes_test_lib:get_client_ip(Handler, IpProtocol), 5555, IpProtocol) end,
                                               ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    ftpes_test_lib:expected_timeout(),
    Config.

%% @hidden TC-US1-5
%% Setting valid data structure
setting_valid_data_structure(Config)->
    Handler = ?config(socket,  Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "STRU", ["F"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-US1-5a
%% Setting invalid data structure
setting_invalid_data_structure(Config)->
     Handler = ?config(socket,  Config),
     
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "STRU", ["R"], ?UNSUPPORTED_STRUCTURE_TYPE),
    Config.

%% @hidden TC-US1-6
%% Setting valid transmission mode
setting_valid_transmission_mode(Config)->
    Handler = ?config(socket,  Config), 
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "MODE", ["S"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-US1-6a
%% Setting invalid transmission mode
setting_invalid_transmission_mode(Config)->
    Handler = ?config(socket,  Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "MODE", ["X"], ?UNSUPPORTED_STRUCTURE_TYPE),
    Config.

%% @hidden TC-US1-4
%% Setting valid file representation type
setting_valid_file_representation_type(Config) ->
    Handler = ?config(socket,  Config),
    Connection_mode = ?config(connection_mode,Config),
    
    case Connection_mode of
        tcp ->
            {ok, _} = ftpes_test_lib:ftp_type(Handler, "A");
        ftp_client ->
            {ok, _} = ftpes_test_lib:ftp_type(Handler, ascii)
    end,
    Config.

%% @hidden TC-US1-4a
%% Setting invalid file representation type
setting_invalid_file_representation_type(Config) ->
    Handler = ?config(socket,  Config),

    {error, Reason} = ftpes_test_lib:ftp_type(Handler, "WL"),
    ct:log("Reason: ~p", [Reason]),
    Config.

%% @hidden TC-US1-8, TC-US2.2-3
%% Successful store
successful_store(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    Host = ?config(host, Config),
    DataDir = ?config(data_dir, Config),
    Certificate = ?config(certificate, Config),
    {ok, _} = ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME ,#ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    Config.

%% @hidden TC-US1-8a, TC-US2.2-3a
%% Successful store to existing file
successful_store_to_existing_file(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    %% First we store to file, then read its content, then we store again to same file, then we read its content again
    %% Those 2 contents must be same
    {ok, _} = ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, DataFirst} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, _} = ftpes_test_lib:ftp_stor(Handler,  FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, 
                                                               host = Host, port = Port,  ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, DataSecond} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),

    case (DataFirst =:= DataSecond) of
        true -> ct:log("Successful store to existing file"),
                 ok;
        false -> ct:fail("Testcase unsuccessful")
    end,
    Config.


%% @hidden TC-US1-8b
%% Unuccessful store - invalid data port
unsuccessful_store_invalid_data_port(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    try ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host=Host, port=563254, 
                                                                                       ip_protocol = IpProtocol,  data_dir = DataDir, certificate = Certificate}) of
        {ok, _} ->
            ct:fail("STOR was expected to fail due to invalid data port");
        {nok, Reason} ->
            ct:log("STOR was expected to fail, reason: ~p", [Reason]);
        {error, Reason} ->
            ct:log("STOR failed: ~p", [Reason])
    catch 
        error:Reason ->
            ct:log("STOR failed because of unknown error ~p", [Reason])
    end,
    Config.

%% @hidden TC-US1-8c, TC-US2.2-3c
%% Unuccessful store - client closes connection
unsuccessful_store_client_closes_connection(Config) ->
    Handler = ?config(socket, Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    FilePrefix = ?config(file_prefix, Config),
    Certificate = ?config(certificate, Config),
    try ftpes_test_lib:ftp_stor_close(Handler, FilePrefix ++ ?TEST_FILE_NAME ,#ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}) of
        {ok, _} ->
            ct:fail("STOR was expected to fail due to closed connection");
        _ ->
            ct:log("STOR was expected to fail")
    catch 
        error:Reason ->
            ct:log("STOR failed because of unknown error ~p", [Reason])
    end,
    Config.

%% Successful rename of file
successful_rename_of_file(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    %% First we store to file, then read its content, then we rename it, then we read its content again
    %% Those 2 contents must be same
    {ok, _} = ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, DataFirst} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, _} = ftpes_test_lib:ftp_rename(Handler, FilePrefix ++ ?TEST_FILE_NAME, FilePrefix ++ ?TEST_FILE_NAME ++ "2"),
    {ok, DataSecond} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME ++ "2", #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, _} = ftpes_test_lib:ftp_delete(Handler, FilePrefix ++ ?TEST_FILE_NAME ++ "2"),
    case (DataFirst =:= DataSecond) of
        true -> ct:log("Successful rename of file"),
                 ok;
        false -> ct:fail("Testcase unsuccessful")
    end,
    Config.

%% Unuccessful rename of file no rnfr command
unsuccessful_rename_of_file_no_rnfr_command(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    %% First we store to file, then read its content, then we try to rename it without RNFR command
    {ok, _} = ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, _DataFirst} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {error, Reason} = ftpes_test_lib:ftp_rnto(Handler, FilePrefix ++ ?TEST_FILE_NAME ++ "2"),
    ct:log("Rename was expected to fail, reason: ~p", [Reason]),
    Config.

%% Unsuccessful rename of file renaming non existing file
unsuccessful_rename_of_file_renaming_non_existing_file(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    %% First we store to file, then read its content, then we rename non existing file
    {ok, _} = ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, _DataFirst} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {error, _} = ftpes_test_lib:ftp_rnfr(Handler, FilePrefix ++ ?TEST_FILE_NAME ++ "2"),
    {error, Reason} = ftpes_test_lib:ftp_rnto(Handler, FilePrefix ++ ?TEST_FILE_NAME ++ "2"),
    ct:log("Rename was expected to fail, reason: ~p", [Reason]),
    Config.

%% @hidden TC-US1-7
%% Successful retrieve of a file
successful_retrieve_of_a_file(Config) -> 
    Handler = ?config(socket, Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    {ok, _} = ftpes_test_lib:ftp_appe(Handler,  FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, 
                                                               host = Host, port = Port,  ip_protocol = IpProtocol, data_dir = DataDir , certificate = Certificate}),   
    {ok, Data} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME , #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    ct:log("File content after storing: ~p", [Data]),
    Config.



%% @hidden TC-US1-7b, TC-US2.2-2b
%% Unuccessful retrieve - file does not exist
unsuccessful_retrieve_of_a_file(Config) ->
    Handler = ?config(socket, Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    try ftpes_test_lib:ftp_retr(Handler, "probaa.txt", #ftpDataConnection{mode=Type, host=Host, port=Port,
                                                                      ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}) of
        {ok, _} ->
            ct:fail("RETR was expected to fail due to non existing file name");
        {nok, Reason} ->
            ct:log("RETR was expected to fail, reason: ~p", [Reason]);
        {error, Reason} ->
            ct:log("RETR failed, reason: ~p", [Reason])
    catch 
        error:Reason ->
            ct:log("RETR failed because of unknown error, reason: ~p", [Reason])
    end,
    Config.

%% @hidden TC-US1-7c, TC-US2.2-2c
%% Unuccessful retrieve - invalid data port
unsuccessful_retrieve_invalid_data_port(Config) -> 
    Handler = ?config(socket,Config),
    Type = ?config(type, Config),
    FilePrefix = ?config(file_prefix, Config),
    InvalidPort = 59876954325,
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    try ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, host=Host, port= InvalidPort,
                                                                                       ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}) of
        {ok, _} ->
            ct:log("RETR was expected to fail due to invalid data port");
        {nok, Reason} ->
            ct:log("RETR was expected to fail, reason: ~p", [Reason]);
        {error, Reason} ->
            ct:log("RETR failed, reason: ~p", [Reason])
    catch 
        error:Reason ->
            ct:log("RETR failed because of unknown error, reason: ~p", [Reason])
    end,
    Config.

%% @hidden TC-US1-7d, TC-US2.2-2d
%% Unuccessful retrieve - client closes connection
unsuccessful_retrieve_client_closes_connection(Config) -> 
    Handler = ?config(socket, Config),
    Type = ?config(type, Config),
    FilePrefix = ?config(file_prefix, Config),
    Port = ?config(data_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    try ftpes_test_lib:ftp_retr_close(Handler, FilePrefix ++ ?TEST_FILE_NAME, 
                        #ftpDataConnection{mode=Type, host=Host, port= Port,  ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}) of
        {ok, _} ->
            ct:log("RETR was expected to fail due to non existing file name");
        {nok, Reason} ->
            ct:log("RETR was expected to fail, reason: ~p", [Reason]);
        {error, Reason} ->
            ct:log("RETR failed, reason: ~p", [Reason])
    catch 
        error:Reason ->
            ct:log("RETR failed because of unknown error, reason: ~p", [Reason])
    end,
    Config.

successful_append_to_a_file(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    {ok, _} = ftpes_test_lib:ftp_appe(Handler,  FilePrefix ++ ?TEST_FILE_NAME, #ftpDataConnection{mode=Type, 
                                                               host = Host, port = Port,  ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),   
    {ok, Data} = ftpes_test_lib:ftp_retr(Handler, FilePrefix ++ ?TEST_FILE_NAME , #ftpDataConnection{mode=Type, host= Host, 
                                                            port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    ct:log("File content after appending: ~p", [Data]),
    Config.


unsuccessful_append_to_a_file(Config) ->
    Handler = ?config(socket, Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    DataDir = ?config(data_dir, Config),
    FilePrefix = ?config(file_prefix, Config),
    Certificate = ?config(certificate, Config),
    
    try ftpes_test_lib:ftp_appe_close(Handler, FilePrefix ++ ?TEST_FILE_NAME ,#ftpDataConnection{mode=Type,
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}) of
        {ok, _} ->
            ct:fail("APPE was expected to fail due to closed connection");
        _ ->
            ct:log("APPE was expected to fail")
    catch 
        error:Reason ->
            ct:log("APPE failed because of unknown error, reason: ~p", [Reason])
    end,
    Config.


%% @hidden TC-US2.1-3
%% Successful directory change
successful_changing_directory(Config)->
    %% Also listing all files in a changed directory
    Handler = ?config(socket, Config),

    ok = ftpes_test_lib:ftp_cd(Handler, ?TEST_DIR_NAME),
    Config.

%% @hidden TC-US2.1-3a
%% Unsuccessful directory change
unsuccessful_changing_directory(Config)->
    Handler = ?config(socket,  Config),
    
    {error, Reason} = ftpes_test_lib:ftp_cd(Handler, ?TEST_DIR_NAME ++ "_not_existing"),
    ct:log("Reason: ~p", [Reason]),%%      unsuccessful_retrieve_file_busy,
    Config.

%% @hidden TC-US2.1-2, TC-US2.2-4
%% Successful listing of current directory
successful_listing_of_current_directory(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    FilePrefix = ?config(file_prefix, Config),
    
    {ok, Listing} = ftpes_test_lib:ftp_ls(Handler, "", #ftpDataConnection{mode=Type, host=Host, port=Port, data_dir = DataDir, certificate = Certificate}),
    ct:log("Listing: ~p", [Listing]),
    {ok, _} = ftpes_test_lib:ftp_delete(Handler, FilePrefix ++ ?TEST_FILE_NAME),
    Config.

%% @hidden
%% Successful nlisting of current directory
successful_nlisting_of_current_directory(Config) ->
    Handler = ?config(socket, (Config)),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    FilePrefix = ?config(file_prefix, Config),
    
    {ok, Listing} = ftpes_test_lib:ftp_nls(Handler, "", #ftpDataConnection{mode=Type, host=Host, port=Port, data_dir = DataDir, certificate = Certificate}),
    ct:log("Listing: ~p", [Listing]),
    {ok, _} = ftpes_test_lib:ftp_delete(Handler, FilePrefix ++ ?TEST_FILE_NAME),
    Config.

%% @hidden TC-US2.1-2a, TC-US2.2-4a
%% Successful listing of specified directory
successful_listing_of_specified_directory(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    {ok, Listing} = ftpes_test_lib:ftp_ls(Handler, "/", #ftpDataConnection{mode=Type, host= Host, port=Port, data_dir = DataDir, certificate = Certificate}),
    ct:log("Listing: ~p", [Listing]),
    {ok, NListing} = ftpes_test_lib:ftp_nls(Handler, "/", #ftpDataConnection{mode=Type, host= Host, port=Port, data_dir = DataDir, certificate = Certificate}),
    ct:log("NListing: ~p", [NListing]),
    Config.

%% @hidden TC-US2.1-2b, TC-US2.2-4b
%% Unsuccessful listing of files
unsuccessful_listing_files(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(host, Config),
    Certificate = ?config(certificate, Config),
    
    {ok, Listing} = ftpes_test_lib:ftp_ls(Handler, "blabla", #ftpDataConnection{mode=Type, host=Host, port=Port, data_dir = DataDir, certificate = Certificate}),
    ct:log("Listing: ~p", [Listing]), 
    Config.

%% @hidden
successful_size_of_a_file(Config) ->
    Handler = ?config(socket,  Config),
    Type = ?config(type, Config),
    Port = ?config(data_port, Config),
    FilePrefix = ?config(file_prefix, Config),
    IpProtocol = ?config(ip_protocol, Config),
    Host = ?config(host, Config),
    DataDir = ?config(data_dir, Config),
    Certificate = ?config(certificate, Config),
    {ok, _} = ftpes_test_lib:ftp_stor(Handler, FilePrefix ++ ?TEST_FILE_NAME ,#ftpDataConnection{mode=Type, host = Host, 
                                     port=Port, ip_protocol = IpProtocol, data_dir = DataDir, certificate = Certificate}),
    {ok, _Listing} = ftpes_test_lib:ftp_nls(Handler, "", #ftpDataConnection{mode=Type, host=Host, port=Port, data_dir = DataDir, certificate = Certificate}),
    {ok, _} = ftpes_test_lib:ftp_size(Handler, FilePrefix ++ ?TEST_FILE_NAME),
    Config.

%% @hidden
unsuccessful_size_of_a_file(Config) ->
    Handler = ?config(socket, Config),
    FilePrefix = ?config(file_prefix, Config),
    {error, Reason} = ftpes_test_lib:ftp_size(Handler, FilePrefix ++ ?TEST_FILE_NAME),
    ct:log("Reason: ~p", [Reason]),
    Config.

%% @hidden TC-US2.1-4
%% Successful folder creation
successful_create_folder(Config)->
    Handler = ?config(socket, Config),
    {ok, _} = ftpes_test_lib:ftp_mkdir(Handler, ?TEST_DIR_NAME),
    Config.

%% @hidden TC-US2.1-4a
%% Unsuccessful folder creation
unsuccessful_create_folder(Config)->
    Handler = ?config(socket, Config),
    {error, Reason} = ftpes_test_lib:ftp_mkdir(Handler, ?TEST_DIR_NAME),
    ct:log("Reason: ~p", [Reason]),
    Config.

%% @hidden TC-US2.1-6
%% Successful folder deletion
successful_delete_folder(Config)->
    %% It sleeps so we can see created folder for 5 seconds
    timer:sleep(500),
    Handler = ?config(socket, Config),

    {ok, _} = ftpes_test_lib:ftp_rmdir(Handler, ?TEST_DIR_NAME),
    Config.

%% @hidden TC-US2.1-6a
%% Unsuccessful folder deletion
unsuccessful_delete_folder(Config)->
    Handler = ?config(socket, Config),

    {error, Reason} = ftpes_test_lib:ftp_rmdir(Handler, ?TEST_DIR_NAME ++ "_not_existing"),
    ct:log("Reason: ~p", [Reason]),
    Config.

%% @hidden TC-US2.1-5
%% Successful file deletion
successful_delete_file(Config)->
    Handler = ?config(socket,  Config),
    FilePrefix = ?config(file_prefix, Config),
    Listing = ftp:ls(Handler ,FilePrefix ++ ?TEST_FILE_NAME ),
    io:format("Listing : ~p", [Listing]),
    {ok, _} = ftpes_test_lib:ftp_delete(Handler,  FilePrefix ++ ?TEST_FILE_NAME),
    Config.

%% @hidden TC-US2.1-5a
%% Unsuccessful file deletion
unsuccessful_delete_file(Config)->
    Handler = ?config(socket, Config),

    {error, Reason} = ftpes_test_lib:ftp_delete(Handler,  ?TEST_FILE_NAME ++ "_not_existing"),
    ct:log("Reason: ~p", [Reason]),
    Config.

%% @hidden TC-US2.1-7
%% Successful print of current directory
successful_printing_working_directory(Config) ->
    Handler = ?config(socket,  Config),

%%     {ok, _} = ftpes_test_lib:ftp_cd(Handler, "/" ++ ?TEST_DIR_NAME),
    {ok, _} = ftpes_test_lib:ftp_pwd(Handler),
    Config.

%% @hidden TC-2.4-1
%% Successful feature listing
successful_listing_features(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "FEAT", [], ?FTP_FEAT_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-2.4-1a
%% Unsuccessful feature listing
unsuccessful_listing_features(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "FEAT", ["something"], ?INVALID_NUMBER_OF_ARGUMENTS),
    Config.

%% @hidden TC-2.4-2
%% Successful option changing
successful_changing_options(Config)->
    %% Successful test, but no extra features exists for detailed testing (test runs dummy command bla)
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "OPTS", ["bla", "x;y;z;"], ?FTP_RESPONSE_UNSUCCESSFUL),
    Config.

%% @hidden TC-2.4-2a
%% Unsuccessful option changing
unsuccessful_changing_options(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "OPTS", ["bla", "x"], ?INVALID_NUMBER_OF_ARGUMENTS),
    Config.

%% @hidden TC-2.4-2b
%% Unsuccessful option changing no arguments
unsuccessful_changing_options_no_args(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "OPTS", ["bla"], ?FTP_RESPONSE_UNSUCCESSFUL),
    Config.

%% @hidden TC-2.4-2c
%% Unsuccessful option changing wrong command
unsuccessful_changing_options_wrong_command(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "OPTS", ["wrong"], ?INVALID_NUMBER_OF_ARGUMENTS),
    Config.

%% @hidden TC-2.4-2d
%% Unsuccessful option changing wrong command with arguments
unsuccessful_changing_options_wrong_command_with_args(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "OPTS", ["bljak", "x"], ?INVALID_NUMBER_OF_ARGUMENTS),
    Config.

%% @hidden TC-2.4-2e
%% Unsuccessful option changing no command and arguments
unsuccessful_changing_options_no_command_and_args(Config)->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "OPTS", [], ?INVALID_NUMBER_OF_ARGUMENTS),
    Config.

%% @hidden TC-2.3-4a, TC-US3-6a
%% 
valid_sequence_of_security_handshake(Config)->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PROT", ["P"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

valid_sequence_of_security_handshake_w_user(Config)->
    Handler = ?config(socket, Config),
    User = ?config(user, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    ok = ftpes_test_lib:ftp_user(SSLSocket, User, ""),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PROT", ["P"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-2.3-4b, TC-US3-6b
invalid_sequence_of_security_handshake(Config)->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PROT", ["P"], ?FTP_SECURITY_DATA_EXCHANGE_NOT_DONE),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-US2.3-1, TC-US3-1
%% Successful authorization
successful_authorization(Config) ->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, _SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    Config.

%% @hidden TC-US2.3-1a, TC-US3-1a
%% Unknown authorization parameter
unknown_authorization_parameter(Config)->
    Handler = ?config(socket, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["WHATEVER"], ?UNSUPPORTED_STRUCTURE_TYPE),
    Config.


%% @hidden TC-US2.3-2, TC-US3-2
%% Successful protection buffer setting
successful_protection_buffer_setting(Config) ->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.
    
%% @hidden TC-US2.3-2a, TC-US3-2a
%% Unsuccessful protection buffer setting - wrong sequence
unsuccessful_protection_buffer_setting_wrong_sequence(Config) ->
    Handler = ?config(socket, Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "PBSZ", ["0"], ?FTP_RESPONSE_NOT_LOGGED_IN),
    Config.    

%% @hidden TC-US2.3-2b, TC-US3-3b
%% Invalid protection buffer parameter
invalid_protection_buffer_parameter(Config)->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["10"], ?INVALID_NUMBER_OF_ARGUMENTS),
    Config.

%% @hidden TC-US2.3-3, TC-US3-3
%% Successful protection level setting
successful_protection_level_setting(Config) ->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PROT", ["P"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    Config.

%% @hidden TC-US2.3-3a, TC-US3-3a
%% Unsuccessful protection level setting - unsupported level
unsuccessful_protection_level_setting_unsupported_level(Config) ->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PROT", ["S"], ?UNSUPPORTED_STRUCTURE_TYPE),
    Config.


%% @hidden TC-US2.3-3b, TC-US3-3b
%% Unsuccessful protection level setting - invalid level
unsuccessful_protection_level_setting_invalid_level(Config) ->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PBSZ", ["0"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PROT", ["BLAK"], ?UNSUPPORTED_STRUCTURE_TYPE),
    Config.

%% @hidden TC-US2.3-3c, TC-US3-3c
%% Unsuccessful protection level setting - wrong sequence
unsuccessful_protection_level_setting_wrong_sequence(Config) ->
    Handler = ?config(socket, Config),
    {_, Certificate} = lists:keyfind(certificate, 1, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "AUTH", ["TLS"], ?FTP_AUTHENTICATION_MODE_SUCCESSFUL),
    {ok, SSLSocket}= ftpes_test_lib:ssl_connect(Handler, Certificate, infinity),
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(SSLSocket, "PROT", ["P"], ?FTP_SECURITY_DATA_EXCHANGE_NOT_DONE),
    Config.

%% @hidden TC-US2.5-2
%% Successful EPSV command for current protocol
successful_EPSV_command_for_current_protocol(Config) ->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "EPSV", [], ?FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV),
    Config.

%%@hidden TC-US2.5-2a
%% Successful EPSV command for IPv4
successful_EPSV_command_for_IPv4(Config) ->
     Handler = ?config(socket,  Config),

     {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "EPSV", ["1"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV),
     Config.

%%@hidden TC-US2.5-2b
%% Successful EPSV command for IPv6
successful_EPSV_command_for_IPv6(Config) ->
     Handler = ?config(socket,  Config),

     {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "EPSV", ["2"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV),
     Config.

%%@hidden TC-US2.5-2c
%% Successful EPSV ALL command
successful_EPSV_ALL_command(Config) ->
    Handler = ?config(socket,  Config),
    Port = ?config(data_port, Config),
    Host = ?config(host, Config),
    IpProtocol = ?config(ip_protocol, Config),
    
    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "EPSV", ["ALL"], ?FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV),
    {ok, _}= ftpes_test_lib:send_recv_ftp_command(Handler, fun() -> ftpes_test_lib:send_port_command(Handler, Host,  Port, IpProtocol) end, 
                                              ?FTP_SECURITY_DATA_EXCHANGE_NOT_DONE),
    Config.

%%@hidden TC-US2.5-2e
%% Unsuccessful EPSV command for non-existent protocol
unsuccessful_EPSV_command_for_non_existent_protocol(Config) ->
    Handler = ?config(socket,  Config),

    {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "EPSV", ["MyFancyProtocol"], ?NETWORK_PROTOCOL_NOT_SUPPORTED),
    Config.

%%@hidden TC-US2.5-2d
%% Unuccessful EPSV command for current protocol due to client reject
unuccessful_EPSV_command_for_current_protocol_due_to_client_reject(Config) ->
   Handler = ?config(socket,  Config),
   
   {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "EPSV", [], ?FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV),
   {ok, _} = ftpes_test_lib:send_specific_command_over_TCP(Handler, "ABOR", [], ?FTP_RESPONSE_TRANSFER_COMPLETED),
   Config.

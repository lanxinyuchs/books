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
%%% %CCaseFile:	ftpes_basic_suite.erl %
%%% Author: 
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(ftpes_basic_suite).

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
%%% R8A/4    2016-11-23   eivmiha    logging fault fix
%%% R8A/14   2016-11-30   ekurnik    Added US 4.3 TCs
%%% R8A/15   2016-11-30   emarnek    Added US 4.2 TCs and AS tests
%%% R8A/18   2016-12-01   emarnek    Small changed, added process_info
%%% R8A/21   2016-12-06   ekurnik    Added OAM server tests
%%% R8A/22   2016-12-07   ekurnik    Added warm restart test
%%% R8A/23   2016-12-07   estjako    Added TC-US9.1-2 &3
%%% R8A/24   2016-12-08   emarnek    Added TC-US9.1-1
%%% R8A/25   2016-12-08   ekurnik    Added 2 additional test for OAM server
%%% R8A/29   2016-12-12   ekurnik    Support for ipv6
%%% R8A/35   2016-12-13   eivmiha    Added TC-US9.3
%%% R8A/37   2016-12-13   ekurnik    Added support for SIM
%%% R8A/38   2016-12-14   emarnek    Added TC-US9.2
%%% R8A/39   2016-12-14   enekdav    Added code coverage
%%% R8A/41   2016-12-15   estjako    Added TC-US9.4-1a,b & TC-US9.4-13
%%% R8A/48   2016-12-19   estjako    Added TC-US9.4-1c, 2,2a, 3, 3a, 4, 4a, 5, 5a, 6, 6a, 
%%%                                   7, 7a, 8, 8a, 15
%%% R8A/50   2016-12-20   ekurnik    Added support functions for security log test
%%% R8A/51   2016-12-20   emarnek    Security log tests done
%%% R8A/52   2016-12-19   eivmiha    Added TC-US9.4-9, 9a, 10, 10a, 11, 11a, 12, 12a, 13, 
%%%                                   13a, 14, 14a
%%% R8A/62   2017-01-04   ekurnik    Added support for using designated FTPES dir
%%% R8A/64   2017-01-05   estjako    Added TC-US9.4-1c, 1d, 1e, 1f, 1g, 1h, 17
%%% R8A/67   2017-01-09   eivmiha    Added TC-US5-1, 2, 3, 4, 5, 6, 7, 8
%%% R8A/79   2017-01-13   ekurnik    Fixed TC-US5-3b
%%% R8A/80   2017-01-17   ekurnik    Switched to ftpes_test_lib in RCT

%%% R9A/8    2017-02-16   estjako    Added TC-9.4-17 & 18
%%% R9A/9    2017-02-16   eivmiha    Added TC-9.1a-d, f-i, k-n, 9.4-7c, 8c, 14c, 15b, 11.1-1
%%% R9A/10   2017-02-16   emarnek    Added aread
%%% R9A/14   2017-02-20   ekurnik    Added binary mode tests
%%% R9A/17   2017-03-09   estjako    Commented out some testcases
%%% R9A/21   2017-03-14   estjako    Commented test cases return back
%%% R9A/24   2017-03-27   emarnek    Added sleep in 2 tests
%%% R9A/25   2017-03-30   estjako    Changed create folder name
%%% R9A/27   2017-04-04   emarnek    Removed duplicate opening of start_channel in some functions
%%% R9A/30   2017-04-11   ekurnik    Removed cover_hook since it causes problems when running cover_spec
%%% R9A/31   2017-04-12   ekurnik    Removed hardcoded path to .hrl
%%% R10A/1   2017-05-31   estifil    Security log test issue resolved
%%% R12A/1   2017-11-10   ekurnik    Minor fix
%%% R12A/2   2017-11-22   enatdok    Added 3 control port TCs
%%% R12A/6   2017-11-28   evadumb    Added idle timer tests
%%% R12A/7   2017-11-28   enekdav    Changed nc_tc_group and nc_tc_client_group TC's
%% ====================================================================
%% API functions
%% ====================================================================
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         groups/0,
         all/0
       ]).

-export([start_server_without_oam/1,
         oam_configured/1,
         oam_config_change/1,
         oam_down/1,
         oam_config_change_wrong_address/1,
         alt_oam_configured/1,
         alt_oam_config_change/1,
         alt_oam_config_change_wrong_address/1,
         alt_oam_down/1,
         warm_restart_oam/1,
         check_ftp_tls_mo_exists/1,
         set_nc_tc/1,
         node_credential_update/1,
         trust_category_update/1,
         nc_tc_no_update/1,
         node_credential_no_install/1,
         trust_category_set_wrong_value/1,
         remove_nc/1,
         remove_tc/1,
         remove_nc_and_tc/1,
         administrative_state_unlocked/1,
         administrative_state_locked/1,
         enabled_cipher_suites/1,
         successful_cipher_suite_changed/1,
         unsuccessful_cipher_suite_changed/1,
         connect_using_trusted_cert/1,
         connect_using_non_trusted_cert/1,
         connect_without_cert/1,
         successful_login_entry/1,
         unsuccessful_login_entry/1,
         unsuccessful_login_entry_wrong_cert/1,
         duration_of_session/1
        ]).

-export([
         start_client_all_interfaces_up/1,
         start_client_oam_down/1,
         start_client_oam_alt_oam_down/1,
         start_client_all_interfaces_down/1,
         start_client_all_interfaces_up_unsuccessful/1,
         start_client_oam_down_unsuccessful/1,
         start_client_oam_alt_oam_down_unsuccessful/1,
         oam_config_change_client/1,
         alt_oam_config_change_client/1,
         lmt_config_change_client/1,
         oam_down_client/1,
         alt_oam_down_client/1,
         lmt_down_client/1,
         node_credential_no_install_client/1,
         trust_category_set_wrong_value_client/1,
         successful_cipher_suite_changed_client/1,
         set_nc_tc_client/1,
         node_credential_update_client/1,
         trust_category_update_client/1,
         nc_tc_no_update_client/1,
         remove_nc_client/1,
         remove_tc_client/1
        ]).

-export([
         start_channel_with_hostname_options_and_port/1,
         start_channel_with_hostname_and_options/1,
         start_channel_unsuccessful/1,
         start_channel_empty_options/1,
         start_channel_empty_options_wrong_host/1,
         start_channel_port_empty_options/1,
         start_channel_wrong_port_empty_options/1,
         start_channel_no_user_in_options/1,
         start_channel_port_no_user_in_options/1,  
         stop_channel/1,
         open_file_in_read_mode/1,
         open_file_in_read_mode_binary/1,
         open_file_in_read_mode_unsuccessful/1,
         open_file_in_write_mode/1,
         close_file_read_mode/1,
         close_file_read_mode_unsuccessful/1,
         close_file_write_mode/1,
         close_file_write_mode_unsuccessful/1,
         open_read_file/1,
         open_read_file_binary/1,
         open_read_file_timeout/1,
         open_read_file_unsuccessful/1,
         open_read_file_timeout_unsuccessful/1,
         aread/1,
         rename_file/1, 
         rename_file_unsuccessful/1,
         make_directory/1, 
         make_directory_unsuccessful/1,
         delete_directory/1,
         delete_directory_unsuccessful/1,
         open_write_file/1,
         open_write_file_timeout/1,
         open_write_file_unsuccessful/1,
         delete_file/1,
         delete_file_timeout/1,
         delete_file_unsuccessful/1,
         list_directory/1,
         list_directory_timeout/1,
         list_directory_unsuccessful/1,
         list_directory_timeout_unsuccessful/1,
         read_file/1,
         read_file_timeout/1,
         read_file_unsuccessful/1,
         read_file_timeout_unsuccessful/1,
         read_file_cipher_notify/1,
         write_file/1,
         write_file_timeout/1,
         write_file_timeout_unsuccessful/1,
         put_file/1,
         put_file_unsuccessful/1,
         get_file_size/1,
         get_file_size_unsuccessful/1,
         is_directory_test/1,
         is_directory_test_unsuccessful/1,
         is_file_test/1
         ]).

-export([start_channel_with_hostname_port_username_password/1,
         start_channel_with_alt_oam_only/1,
         start_channel_with_alt_oam/1,
         start_channel_with_hostname_port_username_password_with_opts/1,
         start_channel_with_alt_oam_only_with_opts/1,
         start_channel_with_alt_oam_with_opts/1,
         stop_channel_and_close_connection_reference/1]).

-export([
         intercept_open_read/1,
         intercept_open_write/1, 
         intercept_delete_file/1,
         intercept_delete_directory/1,
         intercept_list_directory/1,
         intercept_print_current_working_directory/1,
         intercept_make_directory/1,
         intercept_rename_file/1
        ]).
         

-export([open_read_rop_PMS_intercept/1,
         open_read_non_rop_PMS_intercept/1,
         open_write_rop_PMS_intercept/1,
         delete_rop_PMS_intercept/1,
         delete_rop_PMS_intercept_unsuccessful/1,
         get_session_info/1]).

-export([multiple_clients/1,
         max_number_of_sessions/1,
         client_timeout/1,
         remote_client_crash/1,
         read_write_large_file/1,
         read_write_large_file_from_server/1,
         read_write_upgrade/1,
         start_new_session_bucket_full/1,
         start_new_session_bucket_empty_max_sessions/1,
         max_sessions_bucket_not_empty/1]).

-export([control_conn_port_startup/1,
         control_conn_port_reconf/1,
         control_conn_port_used/1,
         idle_timer_startup/1,
         idle_timer_reconf/1,
         idle_timer_null/1]).


%% ====================================================================
%% Internal functions
%% ====================================================================


-include_lib("common_test/include/ct.hrl").
-include("$RCT_TOP/test/lib/lib/esrc/ftpes_test_lib.hrl").
-include_lib("kernel/include/file.hrl").

-spec suite() -> [tuple()].

-define(NC_CONFIG, case ftpes_test_lib:is_target() of
                        true ->
                            [{1, nc1, oam_auto}, {1, nc2, ssh_lmt_ipv4}];
                        false ->
                            [nc1]
                    end).
-define(DefaultTimeout, 600).

suite() ->
   [{timetrap, {minutes, 10}},
    {ct_hooks, [{rct_rpc, rpc},
                {rct_scp, scp1},
                {rct_netconf, ?NC_CONFIG},
                {rct_ssh,{ssh,[manual_connect]}},
                {rct_htmllink,[]},
                {rct_logging, {all,
                                [{erlang,
                                  {["ERROR REPORT","CRASH REPORT"],
                                   ["exception exit: unexpected_exit",
                                    "Fatal error: unknown ca",
                                    "Fatal error: handshake failure",
                                    "\\*\\* unexpected_exit",
                                    "ftpesClientHandler: Force stopped client",
                                    "\\*\\* unexpected_exit",
                                    "appmServer:will_restart:{\"ricm\""]}
                                }]}},
        {rct_core,[]}
           ]}].

groups() ->
    NcTcGroup = nc_tc(),
    OAMGroup = oam(),
    OAMClientGroup = oam_client(),
    NcTcClient = nc_tc_client(),
    CipherGroup = cipher(),
    CertConnGroup = cert_conn(),
    FtpIFtpesGroup = ftpI_ftpes(),
    OamFtpIFtpesGroup = oam_ftpI_ftpes(),
    FtpISftpGroup = ftpI_sftp(),
    OamFtpISftpGroup = oam_ftpI_sftp(),
    SecurityLogGroup = security_log(),
    AllWithoutOam = all_without_oam(),
    AccessControl = access_control(),
    PMSAccessControl = pms_access_control(),
    Robust = robustness(),
    FileTpm = file_tpm(),
    Group1 = group1(),
    Group2 = group2(),
    [
     {nc_tc_group, [], NcTcGroup},
     {oam_group, [], OAMGroup},
     {oam_client_group, [], OAMClientGroup},
     {nc_tc_client_group, [], NcTcClient},
     {cipher_group, [], CipherGroup},
     {cert_conn_group, [], CertConnGroup},
     {ftpI_ftpes_group, [], FtpIFtpesGroup},
     {oam_ftpI_ftpes_group, [], OamFtpIFtpesGroup},
     {ftpI_sftp_group, [], FtpISftpGroup},
     {oam_ftpI_sftp_group, [], OamFtpISftpGroup},
     {security_log_group, [], SecurityLogGroup},
     {access_control_group, [], AccessControl},
     {pms_access_control_group, [], PMSAccessControl},
     {robustness_group, [], Robust},
     {file_tpm_group, [], FileTpm},
     
     %% special group containing groups which don't require OAM
     {all_without_oam_group, [], AllWithoutOam},
     {group1, [], Group1},
     {group2, [], Group2}
    ].

all() ->
     [
      {group, nc_tc_group},
      {group, oam_group},
      {group, oam_client_group},
      {group, nc_tc_client_group},
      {group, cipher_group},
      {group, cert_conn_group},
      {group, ftpI_ftpes_group},
      {group, oam_ftpI_ftpes_group},
      {group, ftpI_sftp_group},
      {group, oam_ftpI_sftp_group},
      {group, security_log_group},
      {group, access_control_group},
      {group, pms_access_control_group}
     ].

all_without_oam() ->
    [
     {group, group1},
     {group, group2}
    ].

group1() ->
    [
     {group, nc_tc_group},
     {group, nc_tc_client_group},
     {group, cipher_group},
     {group, cert_conn_group},
     {group, security_log_group},
     {group, access_control_group},
     {group, pms_access_control_group}
    ].

group2() ->
    [
     {group, ftpI_ftpes_group},
     {group, ftpI_sftp_group}
    ].

nc_tc() ->
    [
     check_ftp_tls_mo_exists,
     set_nc_tc,
     node_credential_update,
     trust_category_update,
     nc_tc_no_update,
     node_credential_no_install,
     trust_category_set_wrong_value,
     remove_nc,
     remove_tc,
     remove_nc_and_tc,
     administrative_state_unlocked,
     administrative_state_locked
    ].

cert_conn() ->
    [
     connect_using_trusted_cert,
     connect_using_non_trusted_cert,
     connect_without_cert
    ].

security_log() ->
    [
    successful_login_entry,
    unsuccessful_login_entry,
    unsuccessful_login_entry_wrong_cert,
    duration_of_session
    ].
    
oam() ->
    [
     start_server_without_oam,
     oam_configured,
     oam_config_change,
     oam_down,
     oam_config_change_wrong_address,
     alt_oam_configured,
     alt_oam_config_change,
     alt_oam_down,
%%      alt_oam_config_change_wrong_address, %% commented out because unstable
     warm_restart_oam
    ].

oam_client() ->
    [
     start_client_all_interfaces_up,
     start_client_oam_down,
     start_client_oam_alt_oam_down,
     start_client_all_interfaces_down,
     start_client_all_interfaces_up_unsuccessful,
     start_client_oam_down_unsuccessful,
     start_client_oam_alt_oam_down_unsuccessful,
     oam_config_change_client,
     alt_oam_config_change_client,
     lmt_config_change_client,
     oam_down_client,
     alt_oam_down_client,
     lmt_down_client
    ].

nc_tc_client() ->
    [
     node_credential_no_install_client,
     trust_category_set_wrong_value_client, 
     successful_cipher_suite_changed_client,
     set_nc_tc_client,
     node_credential_update_client,
     trust_category_update_client,
     nc_tc_no_update_client,
     remove_nc_client,
     remove_tc_client
    ].

cipher() ->
    [
     enabled_cipher_suites,
     successful_cipher_suite_changed
    ].

ftpI_ftpes() -> [
                 start_channel_with_hostname_options_and_port,
                 start_channel_with_hostname_and_options,
                 start_channel_with_hostname_port_username_password,
                 start_channel_with_hostname_port_username_password_with_opts,
                 start_channel_empty_options,
                 start_channel_empty_options_wrong_host,
                 start_channel_port_empty_options,
                 start_channel_wrong_port_empty_options,
                 start_channel_no_user_in_options,
                 start_channel_port_no_user_in_options,
                 stop_channel,
                 stop_channel_and_close_connection_reference,
                 open_file_in_read_mode,
                 open_file_in_read_mode_binary,
                 open_file_in_read_mode_unsuccessful,
                 open_file_in_write_mode,
                 close_file_read_mode,
                 close_file_read_mode_unsuccessful,
                 close_file_write_mode,
                 close_file_write_mode_unsuccessful,
                 open_read_file,
                 open_read_file_binary,
                 open_read_file_timeout,
                 open_read_file_unsuccessful,
                 open_read_file_timeout_unsuccessful,
                 aread,
                 open_write_file,
                 open_write_file_timeout,
                 open_write_file_unsuccessful,
                 rename_file,
                 rename_file_unsuccessful,
                 delete_file,
                 delete_file_timeout,
                 delete_file_unsuccessful,
                 make_directory,
                 make_directory_unsuccessful,
                 delete_directory,
                 delete_directory_unsuccessful,
                 list_directory,
                 list_directory_timeout,
                 list_directory_unsuccessful,
                 list_directory_timeout_unsuccessful,
                 read_file,
                 read_file_timeout,
                 read_file_unsuccessful,
                 read_file_timeout_unsuccessful,
                 read_file_cipher_notify,
                 write_file,
                 write_file_timeout,
                 write_file_timeout_unsuccessful,
                 put_file,
                 put_file_unsuccessful,
                 get_file_size,
                 get_file_size_unsuccessful,
                 is_directory_test,
                 is_directory_test_unsuccessful,
                 is_file_test
                 
                 ].

oam_ftpI_ftpes() -> [
                     start_channel_with_alt_oam_only,
                     start_channel_with_alt_oam,
                     start_channel_with_alt_oam_only_with_opts,
                     start_channel_with_alt_oam_with_opts
                     ].

ftpI_sftp() -> [
                 start_channel_with_hostname_options_and_port,
                 start_channel_with_hostname_and_options,
                 start_channel_with_hostname_port_username_password,
                 start_channel_with_hostname_port_username_password_with_opts,
                 start_channel_unsuccessful,
                 stop_channel,
                 stop_channel_and_close_connection_reference,
                 open_file_in_read_mode,
                 open_file_in_read_mode_binary,
                 open_file_in_read_mode_unsuccessful,
                 open_file_in_write_mode,
                 close_file_read_mode,
                 close_file_read_mode_unsuccessful,
                 close_file_write_mode,
                 close_file_write_mode_unsuccessful,
                 open_read_file,
                 open_read_file_binary,
                 open_read_file_timeout,
                 open_read_file_unsuccessful,
                 open_write_file,
                 open_write_file_timeout,
                 open_write_file_unsuccessful,
                 aread,
                 delete_file,
                 delete_file_timeout,
                 delete_file_unsuccessful,
                 list_directory,
                 list_directory_timeout,
                 list_directory_unsuccessful,
                 read_file,
                 read_file_timeout,
                 read_file_unsuccessful,
                 write_file,
                 write_file_timeout,
                 put_file,
                 put_file_unsuccessful
                
                ].

oam_ftpI_sftp() -> [
                     start_channel_with_alt_oam_only,
                     start_channel_with_alt_oam,
                     start_channel_with_alt_oam_only_with_opts,
                     start_channel_with_alt_oam_with_opts
                     ].

access_control() ->[
                     intercept_open_read,
                     intercept_open_write, 
                     intercept_delete_file,
                     intercept_delete_directory,
                     intercept_list_directory,
                     intercept_print_current_working_directory,
                     intercept_make_directory,
                     intercept_rename_file
                    ].

pms_access_control() ->[
     open_read_rop_PMS_intercept,
     open_read_non_rop_PMS_intercept,
     open_write_rop_PMS_intercept,
     delete_rop_PMS_intercept,
%%      delete_rop_PMS_intercept_unsuccessful, 
     get_session_info
     ].

robustness() ->[
     multiple_clients,
     max_number_of_sessions,
     client_timeout,
     remote_client_crash,
     read_write_large_file,
     read_write_large_file_from_server,
     read_write_upgrade,
     start_new_session_bucket_full,
     start_new_session_bucket_empty_max_sessions,
     max_sessions_bucket_not_empty
     ].

file_tpm() ->[
              control_conn_port_startup,
              control_conn_port_reconf,
              control_conn_port_used,
              idle_timer_startup,
              idle_timer_reconf,
              idle_timer_null
              ].
    

%% Setting test parameters and initializing ftpTls DB + initializing NC and TC + starting ftpesServer
init_per_suite(Config) ->
    CCPort = ?DEFAULT_SERVER_CONTROL_CONNECTION_PORT,
    DataDir = ?config(data_dir, Config),
    ok = ftpes_test_lib:register_sftp_dir(ssh, ?FTPES_TEST_DIR, 1024, ssh_sftpd_file),
    Host = ftpes_test_lib:get_node_ip(rpc, ipv4),
    Certificate = [{certfile, DataDir ++ "user_expert.crt"}, {keyfile, DataDir ++ "user_expert.key"}],
    File = "myFile.txt",
    NewFile = "newFile.txt",
%% 	BigFile = "test.txt",
    Folder = "FTPES" ++ ftpes_test_lib:unique_name() ++ "_" ++ atom_to_list(ftpes_test_lib:get_node_name()),
    TestFolder = "test_folder",
    User = "stef",
    Type = passive,
    IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    Namespace = <<"fib_2">>,
    ClientHost = ftpes_test_lib:get_ftpes_test_server_address(IpProtocol),
    ClientUser = ftpes_test_lib:get_ftpes_test_server_username(),
    ClientPass = ftpes_test_lib:get_ftpes_test_server_password(),
    ClientPort = 21,
    Connection_mode = ftp_client,
    UpdatedConfig = ftpes_test_lib:initialize_nc_tc(Config),
    Started = ftpes_test_lib:start_server(),
    [{port, CCPort}, {user, User}, {host, Host}, {file_name, File}, {folder, Folder},{new_file_name, NewFile}, %{big_file, BigFile},
    {namespace, Namespace},{started, Started}, {args,[{port, CCPort},{mode, Type}, {reuseaddr, true}, 
    {tls, Certificate}]}, {connection_mode,Connection_mode}, {type, Type}, {ip_protocol, IpProtocol}, {certificate, Certificate},
    {client_host, ClientHost}, {client_user, ClientUser}, {client_pass, ClientPass}, {client_port, ClientPort},{test_folder_name, TestFolder}| UpdatedConfig].

% @hidden
%% Cleaning NC and TC MOs + stopping ftpesServer + missing clean ftpTls
end_per_suite(Config) ->
    ftpes_test_lib:disable_ftpes_tls(),
    case ?config(started, Config) of
        no -> ftpes_test_lib:stop_ftpes_server(rpc),
              ftpes_test_lib:stop_ftpes_sup(rpc);
        yes -> ok
    end,
    ftpes_test_lib:clean_nc_tc(Config).


% @hidden
init_per_group(oam_group, Config) ->
    IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    case ftpes_test_lib:oam_exists(IpProtocol) of 
        nok -> {skip, "No OAM is configured"};
        _-> ftpes_test_lib:enable_ftpes_tls(Config),
            lists:keyreplace(ip_protocol, 1, Config, {ip_protocol, IpProtocol})
    end;

% @hidden
init_per_group(oam_client_group, Config) ->
    IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    case ftpes_test_lib:oam_exists(IpProtocol) of 
        nok -> {skip, "No OAM is configured"};
        _-> ftpes_test_lib:enable_ftpes_tls(Config),
            lists:keyreplace(ip_protocol, 1, Config, {ip_protocol, IpProtocol})
    end;

% @hidden
init_per_group(nc_tc_client_group, Config) ->
    ftpes_test_lib:disable_ftpes_server_tls(),
    ftpes_test_lib:enable_ftpes_client_tls(Config),
    Config;

% @hidden
init_per_group(ftpI_ftpes_group, Config) ->
      ftpes_test_lib:enable_ftpes_tls(Config),
      Folder = ?config(folder, Config),
      Port = ?config(client_port, Config),
      {ok, Pid, undefined} = start_channel(Port, ftpes, Config),
      Folder = ?config(folder, Config),
      FilePath = Folder ++ "/",
      ok = rct_rpc:call(rpc, ftpesI, make_dir, [Pid, Folder], 10000),
      ok =rct_rpc:call(rpc, ftpI, stop_channel, [ftpes, Pid], 10000),
      [{server, ftpes}, {client_port, Port}, {file_path, FilePath}|Config];

% @hidden
init_per_group(oam_ftpI_ftpes_group, Config) ->
    IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    case ftpes_test_lib:oam_exists(IpProtocol) of 
        nok -> {skip, "No OAM is configured"};
        _-> ftpes_test_lib:enable_ftpes_tls(Config),
            NewConfig = lists:keyreplace(ip_protocol, 1, Config, {ip_protocol, IpProtocol}),
            [{server, ftpes} | NewConfig]
    end;


% @hidden
init_per_group(ftpI_sftp_group, Config) ->
     IpProto = ?config(ip_protocol, Config),
     Host = ftpes_test_lib:get_sftp_server_address(IpProto),
     Host1 = string:strip(Host, both, $[),
     SftpHost = string:strip(Host1, both, $]),
     SftpUser = ftpes_test_lib:get_sftp_server_username(),
     SftpPass = ftpes_test_lib:get_sftp_server_password(),
     Folder = ?config(folder, Config),
     FilePath = Folder ++ "/",
     Port = 22,
     NewConfig = [{client_host, SftpHost}, {client_user, SftpUser}, {client_pass, SftpPass} |Config],
     {ok, Pid, _ChannelRef}= start_channel(Port, sftp, NewConfig),
     ok = rct_rpc:call(rpc,ssh_sftp, make_dir, [Pid, Folder], 100000),
     ok =rct_rpc:call(rpc, ftpI, stop_channel, [sftp, Pid], 10000),
     [{server, sftp}, {client_port, Port}, {file_path, FilePath}|NewConfig];

% @hidden
init_per_group(oam_ftpI_sftp_group, Config) ->
    IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    Port = 22,
    case ftpes_test_lib:oam_exists(IpProtocol) of 
        nok -> {skip, "No OAM is configured"};
        _-> NewConfig = lists:keyreplace(ip_protocol, 1, Config, {ip_protocol, IpProtocol}),
            [{server, sftp}, {client_port, Port} | NewConfig]
    end;

% @hidden
init_per_group(access_control_group, Config) ->
    Folder = ?config(folder, Config),
    ok = rct_rpc:call(rpc, file, make_dir, [?RCS_DIR ++ "/sftp/" ++ Folder], 100000),
    Config;

% @hidden
init_per_group(robustness_group, Config) ->
    ftpes_test_lib:enable_ftpes_tls(Config),
    Folder = ?config(folder, Config),
    Port = ?config(client_port, Config),
    {ok, Pid, undefined} = start_channel(Port, ftpes, Config),
    Folder = ?config(folder, Config),
    N = 20, %% Max number of sessions
    case ftpes_test_lib:get_node_ip(rpc, ipv4) of
        einval -> Host = ftpes_test_lib:get_node_ip(rpc, oam, ipv6),
                  IpFamily = inet6;
        _ -> Host =  ?config(host, Config),
             IpFamily = inet
    end,
    FilePath = Folder ++ "/",
    ok = rct_rpc:call(rpc, ftpesI, make_dir, [Pid, Folder], 10000),
    ok = rct_rpc:call(rpc, ftpI, stop_channel, [ftpes, Pid], 10000),
    [{server, ftpes}, {client_port, Port}, {file_path, FilePath}, {max_sessions, N}, {host, Host}, {ip_family, IpFamily}|Config];

% @hidden
init_per_group(group1, Config) ->
    %% supress output from this group
    error_logger:tty(false),
    Config;

% @hidden
init_per_group(_Group, Config) ->
    Config.

% @hidden
end_per_group(ftpesITest, Config) ->
    Config;

% @hidden
end_per_group(oam_group, Config) ->
    ct:log("Set OAM references back, just for security"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),

    ftpes_test_lib:disable_ftpes_tls(),
    Config;

% @hidden
end_per_group(oam_client_group, Config) ->
    ct:log("Set OAM references back, just for security"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    
    ftpes_test_lib:disable_ftpes_tls(),
    Config;

% @hidden
end_per_group(ftpI_ftpes_group, Config) ->
    ftpes_test_lib:enable_ftpes_tls(Config),
    Port = ?config(client_port, Config),
    {ok, Pid, _ConRef} = start_channel(Port, ftpes, Config),
    {ok, Ls} = rct_rpc:call(rpc, ftpI, list_dir, [ftpes, Pid, "/"], 10000 ),
    ct:log("Listing ~p", [Ls]),
    rpc_delete_folder(Pid, Config),
    rct_rpc:call(rpc, ftpI, stop_channel, [ftpes, Pid], 10000),
    Config;

% @hidden
end_per_group(oam_ftpI_ftpes_group, Config) ->
    ct:log("Set OAM references back, just for security"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),

    ftpes_test_lib:disable_ftpes_tls(),
    Config;

% @hidden
end_per_group(ftpI_sftp_group, Config) ->
    Port = ?config(client_port, Config),
    Folder = ?config(folder, Config),
    {ok, Pid, _ConRef} = start_channel(Port, sftp, Config),
    rct_rpc:call(rpc, ssh_sftp, del_dir, [Pid, Folder], 10000),
    rct_rpc:call(rpc, ftpI, stop_channel, [sftp, Pid], 10000),
    Config;

% @hidden
end_per_group(oam_ftpI_sftp_group, Config) ->
    ct:log("Set OAM references back, just for security"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    Config;

% @hidden
end_per_group(access_control_group, Config) ->
    Folder = ?config(folder, Config),
    ok = rct_rpc:call(rpc, file, del_dir, [?RCS_DIR ++ "/sftp/" ++ Folder], 100000),
    Config;

% @hidden
end_per_group(robustness_group, Config) ->
    Port = ?config(client_port, Config),
    {ok, Pid, _ConRef} = start_channel(Port, ftpes, Config),
    {ok, Ls} = rct_rpc:call(rpc, ftpI, list_dir, [ftpes, Pid, "/"], 10000),
    ct:log("Listing ~p", [Ls]),
    rpc_delete_folder(Pid, Config),
    rct_rpc:call(rpc, ftpI, stop_channel, [ftpes, Pid], 10000),
    ftpes_test_lib:disable_ftpes_tls();

% @hidden
end_per_group(group1, Config) ->
    error_logger:tty(true),
    Config;

% @hidden
end_per_group(_GroupName, Config) ->
    Config.

%------------ oam_group -----------------------------------------------------------------
% @hidden
init_per_testcase(start_server_without_oam, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing references from OAM and Alt OAM"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              ok = ftpes_test_lib:delete_oam_ref(oam),
              Config
    end;

% @hidden
init_per_testcase(oam_configured, Config) -> 
    Config;

% @hidden
init_per_testcase(oam_config_change, Config) ->
    
    ct:log("Removing Alt OAM reference, to be used to switch OAM"),
    ok = ftpes_test_lib:delete_oam_ref(alt_oam),
    Config;

% @hidden
init_per_testcase(oam_down, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing ALT OAM reference, since OAM cannot be deleted if ALT OAM is configured"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              Config
    end;

% @hidden
init_per_testcase(oam_config_change_wrong_address, _Config) -> 
    {skip, "By setting wrong address netconf becomes unavailable"};

% @hidden
init_per_testcase(alt_oam_configured, Config) -> 
    Config;

% @hidden
init_per_testcase(alt_oam_config_change, _Config) ->
    {skip, "Faulty test, unknown reason"};

% @hidden
init_per_testcase(alt_oam_down, Config) ->
    Config;

% @hidden
init_per_testcase(alt_oam_config_change_wrong_address, Config) -> 
    Config;

% @hidden
init_per_testcase(warm_restart_oam, Config) ->
    Config;

%-------------oam_client_group ----------------------------------------------------------
% @hidden
init_per_testcase(start_client_all_interfaces_up, Config) -> 
    Config;

% @hidden
init_per_testcase(start_client_all_interfaces_up_unsuccessful, Config) -> 
    Config;

% @hidden
init_per_testcase(oam_down_client, Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ -> Config
    end;

% @hidden
init_per_testcase(oam_config_change_client, Config) ->
    
    ct:log("Removing Alt OAM reference, to be used to switch OAM"),
    ok = ftpes_test_lib:delete_oam_ref(alt_oam),
    
    Config;

% @hidden
init_per_testcase(start_client_oam_down, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing ALT OAM reference, since OAM cannot be deleted if ALT OAM is configured"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              Config
    end;

% @hidden
init_per_testcase(start_client_oam_down_unsuccessful, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing ALT OAM reference, since OAM cannot be deleted if ALT OAM is configured"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              Config
    end;

% @hidden
init_per_testcase(start_client_oam_alt_oam_down_unsuccessful, Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing references from OAM and Alt OAM"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              ok = ftpes_test_lib:delete_oam_ref(oam),
              Config
    end;

% @hidden
init_per_testcase(alt_oam_down_client, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing references from OAM and Alt OAM"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              ok = ftpes_test_lib:delete_oam_ref(oam),
              Config
    end;

% @hidden
init_per_testcase(start_client_oam_alt_oam_down, Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing references from OAM and Alt OAM"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              ok = ftpes_test_lib:delete_oam_ref(oam),
              Config
    end;

% @hidden
init_per_testcase(lmt_down_client, Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing references from OAM and Alt OAM"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              ok = ftpes_test_lib:delete_oam_ref(oam),
              Config
    end;

% @hidden
init_per_testcase(start_client_all_interfaces_down, Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    case IpProtocol of
        inet6 -> {skip, "Removing references from OAM and Alt OAM forbidden because of IPv6"};
        _ ->  ct:log("Removing references from OAM and Alt OAM"),
              ok = ftpes_test_lib:delete_oam_ref(alt_oam),
              ok = ftpes_test_lib:delete_oam_ref(oam),
              Config
    end;

init_per_testcase(alt_oam_config_change_client, _Config) ->
    {skip, "Faulty test, unknown reason"};


%---------------------nc_tc_group--------------------------------------------------------
% @hidden
init_per_testcase(set_nc_tc, Config) ->
    %% remove NC and TC
    ftpes_test_lib:set_nc_tc_server(undefined, undefined),
    Config;

% @hidden
init_per_testcase(node_credential_no_install, Config) ->
    ct:log("Remove nc ref from FtpTls"),
    %% Remove NC ref from FtpTls
    ftpes_test_lib:set_nc_tc_server(undefined, []),
    %% Remove Node credential MO
    ct:log("Remove Node Credential MO"),
	
    FirstNcId = ?config(firstNC, Config),
    ftpes_test_lib:remove_node_credential([{nodeCredentialID, FirstNcId} | Config]),
    Config;

% @hidden
init_per_testcase(administrative_state_unlocked, Config) ->
    ftpes_test_lib:disable_ftpes_client_tls(),
    ftpes_test_lib:enable_ftpes_server_tls(Config),
    %% Set AS to LOCKED so we can test if server is started after setting AS to UNLOCKED
    As_state = "LOCKED",
    ct:log("Setting administrative state"),
    ftpes_test_lib:set_as(As_state),

    Config;

% @hidden
init_per_testcase(trust_category_set_wrong_value, Config) ->
    Config;

% @hidden
init_per_testcase(trust_category_set_wrong_value_client, Config) ->
    Config;

%--------------------------nc_tc_client_group--------------------------------------------

% @hidden
init_per_testcase(node_credential_no_install_client, Config) ->
	ct:log("Remove nc ref from FtpTls"),
    %% Remove NC ref from FtpTls
    ftpes_test_lib:set_nc_tc_client(undefined, []),
    %% Remove Node credential MO

    ct:log("Remove Node Credential MO"),
    FirstNcId = ?config(firstNC, Config),
    ftpes_test_lib:remove_node_credential([{nodeCredentialID, FirstNcId} | Config]),
    Config;

%------------------------cipher----------------------------------------------------------
% @hidden
init_per_testcase(unsuccessful_cipher_suite_changed, Config) -> 
    case ?config(started, Config) of
        no -> Config;
        yes -> {skip, "Server already started"}
    end; 
%---------------------ftpI_ftpes_group ftpI_sftp_group ----------------------------------

% @hidden
init_per_testcase(start_channel_with_hostname_and_options, Config) ->
      Config;

% @hidden
init_per_testcase(start_channel_with_hostname_options_and_port, Config)->
      Config;

% @hidden
init_per_testcase(start_channel_empty_options, Config)->
      Config;

% @hidden
init_per_testcase(start_channel_empty_options_wrong_host, Config)->
      Config;

% @hidden
init_per_testcase(start_channel_port_empty_options, Config)->
      Config;

% @hidden
init_per_testcase(start_channel_wrong_port_empty_options, Config)->
      Config;

% @hidden
init_per_testcase(start_channel_no_user_in_options, Config)->
      Config;

% @hidden
init_per_testcase(start_channel_port_no_user_in_options, Config)->
      Config;

% @hidden
init_per_testcase(start_channel_with_hostname_port_username_password, Config) ->
      Config;

% @hidden
init_per_testcase(start_channel_with_hostname_port_username_password_with_opts, Config) ->
      Config;

% @hidden
init_per_testcase(start_channel_unsuccessful, Config) ->
      Config;

% @hidden
init_per_testcase(stop_channel, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     [{client_pid, Pid} | Config];

% @hidden
init_per_testcase(open_file_in_read_mode, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

init_per_testcase(open_file_in_read_mode_binary, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(open_file_in_write_mode, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(open_read_file, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(open_read_file_binary, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(open_read_file_timeout, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(open_read_file_timeout_unsuccessful, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     NewConfig;

% @hidden
init_per_testcase(aread, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(close_file_read_mode, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig),
     open_file_read_mode(NewConfig);

% @hidden
init_per_testcase(close_file_write_mode, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig),
     open_file_write_mode(NewConfig);

% @hidden
init_per_testcase(rename_file, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(delete_file, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(delete_file_timeout, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(delete_file_timeout_unsuccessful, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(delete_directory, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     make_directory(NewConfig);

% @hidden
init_per_testcase(read_file, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(read_file_timeout, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(read_file_timeout_unsuccessful, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     NewConfig;

% @hidden
init_per_testcase(read_file_cipher_notify, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     NewConfig;

% @hidden
init_per_testcase(get_file_size, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

% @hidden
init_per_testcase(is_file_test, Config) ->
     Port = ?config(client_port, Config),
     Server = ?config(server, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     write_file(NewConfig);

init_per_testcase(is_directory_test, Config) ->
     Server = ?config(server, Config),
     Port = ?config(client_port, Config),
     {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
     NewConfig = [{client_pid, Pid}|Config],
     make_directory(NewConfig);

init_per_testcase(TestCase, Config) when TestCase =:= idle_timer_startup orelse
                                         TestCase =:= idle_timer_reconf orelse
                                         TestCase =:= idle_timer_null -> 
    ftpes_test_lib:enable_ftpes_tls(Config),
    ?DefaultTimeout = ftpes_test_lib:get_idle_timer(), %servers timeout
    ok = ftpes_test_lib:set_idle_timer(60),
    Config;

% @hidden
%% Setting NC and TC to FtpTls MO + setting up connection
init_per_testcase(_TestCase, Config) ->
    [{_Name,Group}] = ?config(tc_group_properties, Config),
    Port = ?config(client_port, Config),
    case Group of
        nc_tc_group -> 
            ftpes_test_lib:disable_ftpes_client_tls(),
            ftpes_test_lib:enable_ftpes_server_tls(Config),
            case set_control_connection(Config) of
                {error, _Reason} ->
                     ct:fail("Unable to setup control connection");
                {ok, NewConfig} ->
                     ct:pal("Ensuring that server will be properly started before test ~n",[]),
                     NewConfig
            end;
        cipher_group -> 
            ftpes_test_lib:enable_ftpes_tls(Config),
            case set_control_connection(Config) of
                {error, _Reason} ->
                     ct:fail("Unable to setup control connection");
                {ok, NewConfig} ->
                     ct:pal("Ensuring that server will be properly started before test ~n",[]),
                     NewConfig
            end;
        cert_conn_group ->
            ftpes_test_lib:enable_ftpes_tls(Config),
            %% Don't connect here
            Config;
        oam_client_group -> 
            ftpes_test_lib:enable_ftpes_tls(Config),
            Config;
        nc_tc_client_group -> 
            ftpes_test_lib:disable_ftpes_server_tls(),
            ftpes_test_lib:enable_ftpes_client_tls(Config),
            Config;
        ftpI_ftpes_group ->
            ftpes_test_lib:enable_ftpes_tls(Config),
             {ok, Pid, undefined} = start_channel(Port, ftpes, Config),
             [{client_pid, Pid}|Config];
        ftpI_sftp_group ->
            {ok, Pid, _ConRef} = start_channel(Port, sftp, Config),
            [{client_pid, Pid}|Config];
        access_control_group ->
            ftpes_test_lib:enable_ftpes_tls(Config),
            case set_control_connection(Config) of
               {error, _Reason} ->
                    ct:fail("Unable to setup control connection");
               {ok, NewConfig} ->
                    Folder = ?config(folder, NewConfig),
                    ct:pal("Change directory to ~p",[Folder]),
                    
                    ok = ftpes_test_lib:ftp_cd(?config(socket, NewConfig), ".."),
                    
                    {ok, Dir} = ftp:pwd(?config(socket, NewConfig)),
                    ct:log("Working Directory: ~p", [Dir]),
                    
                    {ok, Listing2} = ftp:ls(?config(socket, NewConfig)),
                    ct:log("Listing: ~p", [Listing2]),
                    
                    ok = ftpes_test_lib:ftp_cd(?config(socket, NewConfig), Folder),
                    NewConfig
               end;
        pms_access_control_group ->
            ftpes_test_lib:enable_ftpes_tls(Config),
            case set_control_connection(Config) of
                {error, _Reason} ->
                     ct:fail("Unable to setup control connection");
                {ok, NewConfig} ->
                     ct:pal("Change directory to /rop~n",[]),
                     ok = ftpes_test_lib:ftp_cd(?config(socket, NewConfig), "/rop"),
                     NewConfig
            end;
        robustness_group -> 
            Config;
        file_tpm_group -> 
            ftpes_test_lib:enable_ftpes_tls(Config),
            Config;
        _OtherGroup -> 
            Config
    end.

%-----------------oam_group--------------------------------------------------------------
% @hidden
end_per_testcase(start_server_without_oam, Config) -> 
    
    ct:log("Set OAM references back"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    
    Config;

% @hidden
end_per_testcase(oam_configured, Config) -> 
    Config;

% @hidden
end_per_testcase(oam_config_change, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    ct:log("Set alt oam reference back"),
    set_alt_oam_ref(IpProtocol),
    
    Config;

% @hidden
end_per_testcase(oam_down, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    ct:log("Set alt oam reference back"),
    set_alt_oam_ref(IpProtocol),
    Config;

% @hidden
end_per_testcase(oam_config_change_wrong_address, Config) -> 
    Config;

% @hidden
end_per_testcase(alt_oam_configured, Config) -> 
    Config;

% @hidden
end_per_testcase(alt_oam_config_change, Config) ->
    Config;

% @hidden
end_per_testcase(alt_oam_config_change_wrong_address, Config) -> 
    Config;

% @hidden
end_per_testcase(alt_oam_down, Config) ->
    Config;

% @hidden
end_per_testcase(warm_restart_oam, Config) ->
    Config;

%---------------oam_client_group---------------------------------------------------------
% @hidden
end_per_testcase(start_client_all_interfaces_up, Config) -> 
    Config;

% @hidden
end_per_testcase(start_client_all_interfaces_up_unsuccessful, Config) -> 
    Config;

% @hidden
end_per_testcase(oam_down_client, Config) -> 
    Config;

% @hidden
end_per_testcase(oam_config_change_client, Config) ->
    ct:log("Set alt oam reference back"),
    IpProtocol = ?config(ip_protocol, Config),
    set_alt_oam_ref(IpProtocol),
    
    Config;

% @hidden
end_per_testcase(start_client_oam_down, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    ct:log("Set alt oam reference back"),
    set_alt_oam_ref(IpProtocol),

    Config;

% @hidden
end_per_testcase(start_client_oam_down_unsuccessful, Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    ct:log("Set alt oam reference back"),
    set_alt_oam_ref(IpProtocol),

    Config;

% @hidden
end_per_testcase(alt_oam_down_client, Config) ->

    ct:log("Set OAM references back"),
   IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),

    Config;

% @hidden
end_per_testcase(start_client_oam_alt_oam_down_unsuccessful, Config) -> 
    ct:log("Set OAM references back"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    Config;

% @hidden
end_per_testcase(start_client_oam_alt_oam_down, Config) -> 
    ct:log("Set OAM references back"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    Config;

% @hidden
end_per_testcase(lmt_down_client, Config) -> 
    ct:log("Set OAM references back"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    Config;

% @hidden
end_per_testcase(start_client_all_interfaces_down, Config) -> 
    ct:log("Set OAM references back"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    Config;

% @hidden
end_per_testcase(alt_oam_config_change_client, Config) ->
    Config;

% @hidden
end_per_testcase(lmt_config_change_client, Config) ->
    Config;

%----------------------nc_tc_group-------------------------------------------------------
% @hidden
end_per_testcase(set_nc_tc, Config) ->
    %% no need to close the connection since it is already closed
    Config;

% @hidden
end_per_testcase(node_credential_no_install, Config) ->
    Config;

% @hidden
end_per_testcase(trust_category_set_wrong_value, Config) ->
    FirstTcId = ?config(firstTC, Config),
    SecondTcId = ?config(secondTC, Config),
    MoRefGood = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2"],
    ftpes_test_lib:set_trust_category([{trustCategory, FirstTcId} | Config], MoRefGood),
    ftpes_test_lib:set_trust_category([{trustCategory, SecondTcId} | Config], MoRefGood),
    ftpes_test_lib:disable_ftpes_tls(),
    ct:pal("Deleting NC and TC ~n",[]),
    Config;

%--------------------nc_tc_client_group--------------------------------------------------
% @hidden
end_per_testcase(node_credential_no_install_client, Config) ->
    Config;

% @hidden
end_per_testcase(trust_category_set_wrong_value_client, Config) ->
    FirstTcId = ?config(firstTC, Config),
    SecondTcId = ?config(secondTC, Config),
    MoRefGood = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2"],
    ftpes_test_lib:set_trust_category([{trustCategory, FirstTcId} | Config], MoRefGood),
    ftpes_test_lib:set_trust_category([{trustCategory, SecondTcId} | Config], MoRefGood),
    ftpes_test_lib:disable_ftpes_tls(),
    ct:pal("Deleting NC and TC ~n",[]),
    Config;

% @hidden
end_per_testcase(administrative_state_locked, Config) ->
    %% set AS to UNLOCKED
    As_state = "UNLOCKED",
    ftpes_test_lib:set_as(As_state),
    
    %% Removing NC and TC from FtpTls MO
    ct:pal("Deleting NC and TC ~n",[]),
    ftpes_test_lib:disable_ftpes_tls(),
    Config;

%--------------ftpI_ftpes_group ftpI_sftp_group------------------------------------------

% @hidden
end_per_testcase(start_channel_with_hostname_and_options, Config) ->
      Config;

% @hidden
end_per_testcase(start_channel_with_hostname_options_and_port, Config)->
      Config;

% @hidden
end_per_testcase(start_channel_empty_options, Config)->
      Config;

% @hidden
end_per_testcase(start_channel_empty_options_wrong_host, Config)->
      Config;

% @hidden
end_per_testcase(start_channel_port_empty_options, Config)->
      Config;

% @hidden
end_per_testcase(start_channel_wrong_port_empty_options, Config)->
      Config;

% @hidden
end_per_testcase(start_channel_no_user_in_options, Config)->
      Config;

% @hidden
end_per_testcase(start_channel_port_no_user_in_options, Config)->
      Config;

% @hidden
end_per_testcase(start_channel_with_hostname_port_username_password, Config) ->
      Config;

% @hidden
end_per_testcase(start_channel_with_alt_oam, Config) ->
      Config;

% @hidden
end_per_testcase(start_channel_with_alt_oam_only, Config) ->
      Config;

% @hidden
end_per_testcase(start_channel_with_hostname_port_username_password_with_opts, Config) ->
      Config;

% @hidden
end_per_testcase(start_channel_with_alt_oam_with_opts, Config) ->
      Config;

% @hidden
end_per_testcase(start_channel_with_alt_oam_only_with_opts, Config) ->
      Config;

% @hidden
end_per_testcase(start_channel_unsuccessful, Config) ->
      Config;

% @hidden
end_per_testcase(stop_channel, Config) ->
    Config;

% @hidden
end_per_testcase(stop_channel_and_close_connection_reference, Config) ->
    Config;

% @hidden
end_per_testcase(open_file_in_read_mode, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);

end_per_testcase(open_file_in_read_mode_binary, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);


% @hidden
end_per_testcase(open_file_in_write_mode, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(open_read_file, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(open_read_file_binary, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(open_read_file_timeout, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(open_read_file_timeout_unsuccessful, Config) ->
    Server = ?config(server, Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(aread, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(close_file_read_mode, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(close_file_write_mode, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(rename_file, Config) ->
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    File = ?config(new_file_name, Config),
    Pid = ?config(client_pid, Config),
    ok = rct_rpc:call(rpc, ftpI, delete, [Server, Pid, FilePath ++ File], 10000),
    
    stop_channel(Server, Config),
    Config;

% @hidden
end_per_testcase(make_directory, Config)->
    Server = ?config(server, Config),
    delete_directory(Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(make_directory_unsuccessful, Config)->
    Server = ?config(server, Config),
    delete_directory(Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(read_file, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(read_file_timeout, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(read_file_timeout_unsuccessful, Config) ->
    Server = ?config(server, Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(read_file_cipher_notify, Config) ->
    Server = ?config(server, Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(write_file, Config) ->
    delete_file(Config),
    stop_channel(ftpes, Config);

% @hidden
end_per_testcase(write_file_timeout, Config) ->
    delete_file(Config),
    stop_channel(ftpes, Config);

% @hidden
end_per_testcase(put_file, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(unsuccessful_put_file, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(get_file_size, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);

% @hidden
end_per_testcase(is_file_test, Config) ->
    Server = ?config(server, Config),
    delete_file(Config),
    stop_channel(Server, Config);

end_per_testcase(is_directory_test, Config) ->
     Server = ?config(server, Config),
    delete_directory(Config),
    stop_channel(Server, Config);

end_per_testcase(TestCase, _Config) when TestCase =:= idle_timer_startup orelse
                                        TestCase =:= idle_timer_reconf orelse
                                        TestCase =:= idle_timer_null ->
    ok = ftpes_test_lib:set_idle_timer(?DefaultTimeout),
    ?DefaultTimeout = ftpes_test_lib:get_idle_timer(),
    ok;

% @hidden
%% Removing NC and TC from FtpTls MO + closing connection
end_per_testcase(_TestCase, Config) ->
    Handler = ?config(socket, Config),
    case Handler of 
        undefined -> ProcessInfo = Handler;
        _ -> ProcessInfo = erlang:process_info(Handler)
    end,
    [{_Name,Group}] = ?config(tc_group_properties, Config),
    NewConfig =
    case {Group, ProcessInfo} of
        {nc_tc_group, undefined} -> Config;
        {nc_tc_group, _} -> ok = ftpes_test_lib:close_ftp_connection(Handler),
                            Config;
        {cert_conn_group, _} -> Config;
        {ftpesITest, _} -> stop_client(Config),
                           Config;
        {nc_tc_client_group, _} -> Config;
        {ftpI_ftpes_group, _} ->
            stop_channel(ftpes, Config);
        {ftpI_sftp_group, _} -> stop_channel(sftp, Config);
        {security_log_group, _} -> Config;
        {access_control_group, _} -> ok = ftpes_test_lib:ftp_cd(?config(socket, Config), ".."),
                                     ok = ftpes_test_lib:close_ftp_connection(?config(socket, Config));
        {pms_access_control_group, _} ->
            ok = ftpes_test_lib:close_ftp_connection(Handler),
            Config;
        {robustness_group, _} -> Config;
        {file_tpm_group, _} -> 
            Config;
        _ ->     ok = ftpes_test_lib:close_ftp_connection(Handler),
                 Config
    end,
    NewConfig.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  US 4.1 OAM server tests
%%  US 4.2 CERT block events server tests
%%  US 4.3 NC and TC handling server tests
%%  US 4.4 Security & general logging tests
%%  US 5 Access control
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OAM server tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TC-US4.1-1
%% Start FTP server on target node without OAM
start_server_without_oam(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that oam references don't exist"),
    nok = ftpes_test_lib:oam_exists(IpProtocol),
    
    ct:log("Try to connect to oam and alt_oam interface - expect to fail"),
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    {error, _} = set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),
    {error, _} = set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),
    
    ct:log("Try to connect to lmt - expect to succeed"),
    LmtAddress = ftpes_test_lib:get_node_ip(config, lmt, ipv4),
    {ok, NewConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, LmtAddress})),
    
    ct:log("Close the connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    
    NewConfig.
    
%% TC-US4.1-2
%% OAM configured 
oam_configured(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),

    ct:log("Try to connect to OAM"),
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    {ok, NewConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),

    ct:log("Close the connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    
    NewConfig.

%% TC-US4.1-2a
%% OAM config change
oam_config_change(Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),    
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),

    ct:log("Try to connect to OAM"),
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    {ok, NewConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),
   
    ct:log("Change OAM reference to ALT OAM address reference"),
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(alt_oam, IpProtocol), oam),
    timer:sleep(1000),
    ct:log("Check that the connection is terminated"),
    {error, _} = list_folders(NewConfig),
    
    ct:log("Check that new connection cannot be established to OAM address"),
    {error, _} = set_control_connection(lists:keyreplace(host, 1, NewConfig, {host, OamAddress})),

    ct:log("Check that new connection can be established to Alt OAM address (now referenced by OAM)"),
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    NewConfig2 = lists:keydelete(socket, 1, NewConfig),
    {ok, NewConfig3} =  set_control_connection(lists:keyreplace(host, 1, NewConfig2, {host, AltOamAddress})),

    ct:log("Close the connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig3)),
   
    ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    timer:sleep(1000),
    NewConfig3.

%% TC-US4.1-2b
%% OAM down
oam_down(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    %% TODO: how to deactivate OAM without deactivating ALT OAM?
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),

    ct:log("Try to connect to OAM"),
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    {ok, NewConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),

    ct:log("Try to connect to LMT"),
    LmtAddress = ftpes_test_lib:get_node_ip(config, lmt, ipv4),
    {ok, LmtConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, LmtAddress})),

    ct:log("Remove OAM reference"),
    ok = ftpes_test_lib:delete_oam_ref(oam),

    ct:log("Check that the connection  to OAM is terminated"),
    {error, _} = list_folders(NewConfig),
    
    ct:log("Check that new connection cannot be established to OAM"),
    {error, _} = set_control_connection(lists:keyreplace(host, 1, NewConfig, {host, OamAddress})),

    ct:log("Check that LMT connection is not affected"),
    {ok, Listing} = list_folders(LmtConfig),
    ct:log("Listing: ~p", [Listing]),

    ct:log("Close LMT connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,LmtConfig)),

    ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    timer:sleep(1000),
    NewConfig.

%% TC-US4.1-2c
%% OAM config change - wrong address
oam_config_change_wrong_address(Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    OamRefAddress = ftpes_test_lib:get_oam_ip(oam, IpProtocol),

    ct:log("Try to connect to OAM"),
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    {ok, OamConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),
    
    ct:log("Change OAM address to invalid value - cannot bind"),
    ok = ftpes_test_lib:edit_oam_ip(create_wrong_address(OamAddress) ++ get_address_prefix(IpProtocol), oam, IpProtocol),
    timer:sleep(1000),
    ct:log("Check that the connection is terminated"),
    {error, _} = list_folders(OamConfig),

    ct:log("Check that new connection cannot be established to new OAM address"),
    {ok, OamAddress2} = inet:parse_address(create_wrong_address(OamAddress)),
    {error, _} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress2})),
    
    ct:log("Change OAM address back"),
    ok = ftpes_test_lib:edit_oam_ip(OamRefAddress, oam, IpProtocol),
    timer:sleep(1000),
    ct:log("Check that the connection can be established once again"),
    {ok, OamConfig2} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),

    ct:log("Close the connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,OamConfig2)),

    Config.

%% TC-US4.1-3
%% Alt_OAM configured 
alt_oam_configured(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Try to connect to ALT OAM"),
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    {ok, NewConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),

    ct:log("Close the connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    
    NewConfig.

%% TC-US4.1-3a
%% Alt_OAM config change
alt_oam_config_change(Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Try to connect to ALT OAM"),
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    {ok, AltOamConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),
    
    ct:log("Change ALT OAM address (hardcoding address since it seems it is not used)"),
    AltOamRefAddress = ftpes_test_lib:get_oam_ip(alt_oam),
    ok = ftpes_test_lib:edit_oam_ip(create_new_valid_address(AltOamAddress) ++ get_address_prefix(IpProtocol), alt_oam, IpProtocol),
    timer:sleep(1000),
    ct:log("Check that the connection is terminated"),
    {error, _} = list_folders(AltOamConfig),

    ct:log("Check that new connection can be established to new ALT OAM address"),
    {ok,AltOamAddress2} = inet:parse_address(create_new_valid_address(AltOamAddress)),
    {ok, AltOamConfig2} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress2})),

    ct:log("Close the connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,AltOamConfig2)),
    
    ct:log("Change ALT OAM address back"),
    ok = ftpes_test_lib:edit_oam_ip(AltOamRefAddress, alt_oam, IpProtocol),
    timer:sleep(1000),
    Config.

%% TC-US4.1-3b
%% Alt_OAM down
alt_oam_down(Config) ->
    IpProtocol = ?config(ip_protocol, Config), 
    ct:log("Check that ALT OAM is configured"),

    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Try to connect to ALT OAM"),
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    {ok, NewConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),

    ct:log("Try to connect to LMT and OAM also"),
    LmtAddress = ftpes_test_lib:get_node_ip(config, lmt, ipv4),
    {ok, LmtConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, LmtAddress})),

    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    {ok, OamConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),

    ct:log("Remove ALT OAM reference"),
    ok = ftpes_test_lib:delete_oam_ref(alt_oam),
    timer:sleep(1000),
    ct:log("Check that the connection  to ALT OAM is terminated"),
    {error, _} = list_folders(NewConfig),
    
    ct:log("Check that new connection cannot be established to ALT OAM"),
    {error, _} = set_control_connection(lists:keyreplace(host, 1, NewConfig, {host, AltOamAddress})),

    ct:log("Check that LMT and OAM connections are not affected"),
    {ok, Listing} = list_folders(LmtConfig),
    ct:log("Listing: ~p", [Listing]),

    {ok, Listing2} = list_folders(OamConfig),
    ct:log("Listing2: ~p", [Listing2]),

    ct:log("Close LMT and OAM connections"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,LmtConfig)),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,OamConfig)),
    
    ct:log("Change ALT OAM reference back to ALT OAM address"),
    set_alt_oam_ref(IpProtocol),
    timer:sleep(1000),
    NewConfig.

%% TC-US4.1-3c
%% Alt_OAM config change - wrong address
alt_oam_config_change_wrong_address(Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),
    AltOamRefAddress = ftpes_test_lib:get_oam_ip(alt_oam, IpProtocol),

    ct:log("Try to connect to ALT OAM"),
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    {ok, AltOamConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),

    ct:log("Change ALT OAM address to invalid value - cannot bind"),
    ok = ftpes_test_lib:edit_oam_ip(create_wrong_address(AltOamAddress) ++ get_address_prefix(IpProtocol), alt_oam, IpProtocol),
    timer:sleep(1000),
    ct:log("Check that the connection is terminated"),
    {error, _} = list_folders(AltOamConfig),

    ct:log("Check that new connection cannot be established to new ALT OAM address"),
    {ok, AltOamAddress2} = inet:parse_address(create_wrong_address(AltOamAddress)),
    {error, _} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress2})),
    
    ct:log("Change ALT OAM address back"),
    ok = ftpes_test_lib:edit_oam_ip(AltOamRefAddress, alt_oam, IpProtocol),
    timer:sleep(1000),
    ct:log("Check that the connection can be established once again"),
    {ok, AltOamConfig2} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),

    ct:log("Close the connection"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,AltOamConfig2)),

    Config.

%% TC-US4.1-4
%% Warm restart
warm_restart_oam(Config) ->
    
    ct:log("Check that both OAM and ALT OAM are configured"),
    IpProtocol = ?config(ip_protocol, Config),
    ok = ftpes_test_lib:oam_exists(IpProtocol),
    
    ct:log("Try to connect to LMT, OAM and ALT OAM"),
    LmtAddress = ftpes_test_lib:get_node_ip(config, lmt, ipv4),
    {ok, LmtConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, LmtAddress})),

    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    {ok, OamConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),
    
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    {ok, AltOamConfig} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),
    
    ct:log("Trigger warm restart"),
    kill_app(rpc, "ricm"), %% poor ricm
    timer:sleep(5000),
    
    ct:log("Check that OAM and ALT OAM connections are down and LMT is unaffected"),
    {error, _} = list_folders(OamConfig),
    {error, _} = list_folders(AltOamConfig),
    
    {ok, Listing} = list_folders(LmtConfig),
     ct:log("Listing: ~p", [Listing]),
    
    ct:log("Wait for TN to come back up"),
    timer:sleep(90000),
    
    ct:log("Check that OAM and ALT OAM are back up"),
    {ok, OamConfig2} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, OamAddress})),
    {ok, AltOamConfig2} =  set_control_connection(lists:keyreplace(host, 1, Config, {host, AltOamAddress})),
    
    ct:log("Close all connections"),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,LmtConfig)),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,OamConfig2)),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,AltOamConfig2)),
    
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CERT block events server tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TC-US4.2-1
% NodeCredential MO install
% @hidden
node_credential_no_install(Config) ->
    %% Parameters for reinstalling NC MO
    Priv_dir =
    filename:join(
        lists:droplast(
        filename:split(
            ?config(priv_dir, Config)))),
    FileNc = filename:join([Priv_dir, "tls_nodecert_pkcs12"]),
    FirstNcId = ?config(firstNC, Config),
    FirstTcId = ?config(firstTC, Config),
    IpProto = ftpes_test_lib:get_oam_ip_protocol(),
    SftpUser = ftpes_test_lib:get_sftp_server_username(),
    SftpAddress = ftpes_test_lib:get_sftp_server_address(IpProto),
    %Uri = "sftp://" ++ Uname ++ "@" ++ Shost ++ File,
    UriNc = "sftp://" ++ SftpUser ++ "@" ++ SftpAddress ++ FileNc,
    NcCredPwd = "idiocy",
    NcFingerPrint = "17:8b:19:ef:57:e1:12:62:67:33:f5:bd:bd:8c:28:8e:bd:4b:c2:ce",
    
    ct:log("Creating nc MO"),
    ftpes_test_lib:create_NC_MO([{nodeCredentialID, FirstNcId} | Config]),
    timer:sleep(100),
    
    ct:log("Trying to connect to server"),
    %% Trying to connect to server
    ftpes_test_lib:set_nc_tc(FirstNcId, FirstTcId, []),
    
    %% Check if server is not started
    {error, _} = set_control_connection(Config),
    
    ct:log("Installing nc MO"),
    %% Installing nc MO
    ftpes_test_lib:install_credentials([{nodeCredentialPwd, NcCredPwd}, {nodeCredentialFPrint, NcFingerPrint},
                                    {nodeCredentialID, FirstNcId}, {uriNc, UriNc} | Config]),
    
    ct:log("Trying to connect to server"),
    %% Trying to connect to server
    ftpes_test_lib:set_nc_tc(FirstNcId, FirstTcId, []),

    %% Check if server is started for real
    NewConfig = try_to_connect_server(Config, 10),

    {ok, Result} = list_folders(NewConfig),
    ct:log("LISTING ---- ~p", [Result]),
    
    ct:log("Close connection"),
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    Config.

try_to_connect_server(_,0) -> ct:fail("Couldn't connect!");
try_to_connect_server(Config, Try) ->
    timer:sleep(2000),
    case set_control_connection(Config) of
        {ok, NewConfig} -> NewConfig;
        {error, _Reason} -> try_to_connect_server(Config, Try - 1)
    end.

% TC-US4.2-2
% TrustedCategory MO set trustedCertificate
% @hidden
trust_category_set_wrong_value(Config) ->
    FirstTcId = ?config(firstTC, Config),
    SecondTcId = ?config(secondTC, Config),
    ct:log("Set trusted categories to point to wrong tc"),
    %% Set trusted categories to point on wrong trusted certificate, system created
    MoRef = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=1"],
    ftpes_test_lib:set_trust_category([{trustCategory, FirstTcId} | Config], MoRef),
    ftpes_test_lib:set_trust_category([{trustCategory, SecondTcId} | Config], MoRef),
    Result = ftpes_test_lib:get_all_node_credentials("1"),
    ct:log("NC after setting: ~p",[Result]),
    Result2 = ftpes_test_lib:get_all_trusted_certificates("1"),
    ct:log("TC after setting: ~p",[Result2]),
   
    %% Trying to connect to server
    FirstNcId = ?config(firstNC, Config),
    ftpes_test_lib:set_nc_tc(FirstNcId, FirstTcId, []),
     ct:log("Trying to connect to server"),
    %% Check that connection cannot be established
    {error, _} = set_control_connection(Config),

    ct:log("Set trusted categories to point on right tc"),
    % Set trusted categories to point on right trusted certificate
    MoRefGood = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2"],
    ftpes_test_lib:set_trust_category([{trustCategory, FirstTcId} | Config], MoRefGood),
    ftpes_test_lib:set_trust_category([{trustCategory, SecondTcId} | Config], MoRefGood),
    Result3 = ftpes_test_lib:get_all_node_credentials("1"),
    ct:log("NC after real setting: ~p",[Result3]),
    Result4 = ftpes_test_lib:get_all_trusted_certificates("1"),
    ct:log("TC after real setting: ~p",[Result4]),
    timer:sleep(5000),
    ct:log("Trying to connect to server"),
    %% Check that connection can be established
    {ok, NewConfig} = set_control_connection(Config),

    {ok, Result22} = list_folders(NewConfig),
    ct:log("LISTING ---- ~p", [Result22]),
    
    ct:log("Close connection"),
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cipher suite tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enabled_cipher_suites(Config) ->
    
    CipherList=rct_rpc:call(rpc, comsaI, get_tls_cipher_suites, [], 10000),
    ct:log("Cipher list : ~p", [CipherList]),
    Config.

% TC-US4.2-3
successful_cipher_suite_changed(Config) ->  
    Cipher1 = "ALL:-aRSA:-CBC:SHA256",
    ct:log("Changing cipher suite, filer ~p", [Cipher1]),
    ok = ftpes_test_lib:set_tls_cipher_suites(Cipher1),
    CipherList1=rct_rpc:call(rpc, comsaI, get_tls_cipher_suites, [], 10000),
    ct:log("Cipher list : ~p", [CipherList1]),
    
    %% Change cipher suite back
    Cipher2 = "ALL",
    ct:log("Changing cipher suite, filer ~p", [Cipher2]),
    ok = ftpes_test_lib:set_tls_cipher_suites(Cipher2),
    CipherList2 = rct_rpc:call(rpc, comsaI, get_tls_cipher_suites, [], 10000),
    ct:log("Cipher list : ~p", [CipherList2]),
    Config.

unsuccessful_cipher_suite_changed(Config) ->
    ct:log("Stopping ftpesServer"),
    ok = rct_rpc:call(rpc, ftpesServer, stop, [], 10000),
    Cipher ="ALL:!kRSA",
    Cipher2 = "ALL",
    ct:log("Change cipher suite"),
    ok = ftpes_test_lib:set_tls_cipher_suites(Cipher),
    ok = ftpes_test_lib:set_tls_cipher_suites(Cipher2),
    ct:log("Start ftpesServer"),
    ftpes_test_lib:start_ftpes_sup(rpc,Config),
    ftpes_test_lib:activate(rpc),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NC and TC handling server tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TC-US4.3-1
% Create FtpTls and FtpTlsServer MOs
check_ftp_tls_mo_exists(Config) ->
    ct:log("Check if FtpTls and FtpTlsServer Mo exists"),
    %% Check that ftpTls MOs are created
    {ok, ok} = {ftpes_test_lib:exists_ftp_tls(), ftpes_test_lib:exists_ftp_tls_server()},
    
    Config.

% TC-US4.3-2
% FtpTls MO set nodeCredential and trustCategory
set_nc_tc(Config) ->
    
    %% Check that the server is not started
    ct:log("Checking if server is not started"),
    {error, _} = set_control_connection(Config),
    
    %% Set NC and TC 
    FirstNcId = ?config(firstNC, Config),
    FirstTcId = ?config(firstTC, Config),
    ct:log("Setting nc and tc"),
    ftpes_test_lib:set_nc_tc_server(FirstNcId, FirstTcId),
    
    ct:log("Checking if server is started"),
    %% Check that the server is started
    {ok, NewConfig} = set_control_connection(Config),
    
    ct:log("Close connection"),
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    
    NewConfig.

% TC-US4.3-2a
% FtpTls MO update nodeCredential 
% @hidden
node_credential_update(Config) ->
    Nc_name = ?config(secondNC, Config),
    ct:log("Setting nc"),
    ftpes_test_lib:set_nc_tc_server(Nc_name, []),

    {ok, _Result} = list_folders(Config),
    Config.

% TC-US4.3-2b
% FtpTls MO update trustCategory 
% @hidden
trust_category_update(Config) ->
    Tc_name = ?config(secondTC, Config),
    ct:log("Setting tc"),
    ftpes_test_lib:set_nc_tc_server([], Tc_name),

    {ok, _Result} = list_folders(Config),
    Config.

% TC-US4.3-2c
% FtpTls MO update nodeCredential and trustCategory with the same value 
% @hidden
nc_tc_no_update(Config) ->
    Nc_name = ?config(firstNC, Config),
    Tc_name = ?config(firstTC, Config),
    ct:log("Set same values for nc and tc as before"),
    ftpes_test_lib:set_nc_tc_server(Nc_name, Tc_name),

    {ok, _} = list_folders(Config),
    Config.

% TC-US4.3-2d
% FtpTls MO remove nodeCredential 
remove_nc(Config) ->
    ct:log("Remove nc ref from FtpTls"),
    %% Remove NC ref from FtpTls
    ftpes_test_lib:set_nc_tc_server(undefined, []),
    
    %% Check that the connection is lost
    {error, _} = list_folders(Config),
    ct:log("Check that new connection cannot be established"),
    %% Check that the new connection cannot be established
    {error, _} = set_control_connection(Config),
    
    %% Set NC back
    ct:log("Setting nc"),
    Nc_name = ?config(firstNC, Config),
    ftpes_test_lib:set_nc_tc_server(Nc_name, []),
    
    ct:log("Re-establish connection"),
    %% Re-establish connection
    {ok, NewConfig} = set_control_connection(Config),
    
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    NewConfig.

% TC-US4.3-2e
% FtpTls MO remove trustCategory
remove_tc(Config) ->
    ct:log("Remove tc ref from FtpTls"),
    %% Remove TC ref from FtpTls
    ftpes_test_lib:set_nc_tc_server([], undefined),
    
    %% Check that the connection is lost
    {error, _} = list_folders(Config),
    
    ct:log("Check that new connection cannot be established"),
    %% Check that the new connection cannot be established
    {error, _} = set_control_connection(Config),
    
    ct:log("Setting tc"),
    %% Set TC back
    Tc_name = ?config(firstTC, Config),
    ftpes_test_lib:set_nc_tc_server([], Tc_name),
    
    ct:log("Re-establish connection"),
    %% Re-establish connection
    {ok, NewConfig} = set_control_connection(Config),
    
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    NewConfig.

% @hidden
remove_nc_and_tc(Config) ->
    ct:log("Remove nc and tc ref from FtpTls"),
    %% Remove NC and TC ref from FtpTls
    ftpes_test_lib:set_nc_tc_server(undefined, undefined),

    %% Check that the connection is lost
    {error, _} = list_folders(Config),
    
    ct:log("Check that new connection cannot be established"),
    %% Check that the new connection cannot be established
    {error, _} = set_control_connection(Config),
    
    ct:log("Setting nc and tc"),
    %% Set NC and TC back
    Nc_name = ?config(firstNC, Config),
    Tc_name = ?config(firstTC, Config),
    ftpes_test_lib:set_nc_tc_server(Nc_name, Tc_name),
    
    ct:log("Re-establish connection"),
    %% Re-establish connection
    {ok, NewConfig} = set_control_connection(Config),
    
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    NewConfig.

% TC-US4.3-3
% FtpTlsServer MO set UNLOCKED
% @hidden
administrative_state_unlocked(Config) ->
    %% Check that the new connection cannot be established
    %% Since AS is set to LOCKED in init_per_testcase
    ct:log("Trying to connect to server"),
    {error, _} = set_control_connection(Config),
    
    %% Set AS to UNLOCKED
    As_state = "UNLOCKED",
    ct:log("Setting administrative state"),
    ftpes_test_lib:set_as(As_state),
    
    %% sleep a bit
    timer:sleep(500),
    
    ct:log("Trying to connect to server"),
     %% Re-establish connection
    {ok, NewConfig} = set_control_connection(Config),

    {ok, Result} = list_folders(NewConfig),
    ct:log("LISTING ---- ~p", [Result]),
    
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    NewConfig.

% TC-US4.3-3a
% FtpTlsServer MO set LOCKED
% @hidden
administrative_state_locked(Config) ->
    %% Set AS to LOCKED
    As_state = "LOCKED",
    ct:log("Setting administrative state"),
    ftpes_test_lib:set_as(As_state),
    {error, _} = list_folders(Config),
    Config.

% TC-US4.3-4
% Connect to ftpesServer using trusted certificate
% @hidden
connect_using_trusted_cert(Config) ->
    %% Connect using trusted certificate
    ct:log("Trusted certificate: ~p~n", [?config(tls, ?config(args, Config))]),
    {ok, NewConfig} = set_control_connection(Config),
    
    %% Close connection
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    NewConfig.

% TC-US4.3-4a
% Connect to ftpesServer using non-trusted certificate
% @hidden
connect_using_non_trusted_cert(Config) ->
    %% Switch out certificates from config
    DataDir = ?config(data_dir, Config),
    NonTrustedCert = [{certfile, DataDir ++ "alt_user_expert.crt"}, {keyfile, DataDir ++ "alt_user_expert.key"}],
    
    ConnectionArgs = ?config(args, Config),
    NonTrustedConnectionArgs = lists:keyreplace(tls, 1, ConnectionArgs, {tls, NonTrustedCert}),
    NonTrustedConfig = lists:keyreplace(args, 1, Config, {args, NonTrustedConnectionArgs}),
    
    %% Connect using non-trusted certificate
    ct:log("Non-trusted certificate: ~p~n", [?config(tls, ?config(args, NonTrustedConfig))]),
    {error, ?ERR_UNKNOWN_CA }= set_control_connection(NonTrustedConfig),
    
    ok.

% TC-US4.3-4b
% Connect to ftpesServer without certificate
% @hidden
connect_without_cert(Config) ->
    %% Remove certificates from config
    ConnectionArgs = ?config(args, Config),
    NoTlsConnectionArgs = lists:keyreplace(tls, 1, ConnectionArgs, {tls, []}),
    NoTlsConfig = lists:keyreplace(args, 1, Config, {args, NoTlsConnectionArgs}),
    
    %% Connect using non-trusted certificate
    {error, ?ERR_HANDSHAKE_FAIL} = set_control_connection(NoTlsConfig),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Security & general logging tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TC-US4.4-1
% Connect to ftpesServer using valid username and check Security Log
% @hidden
successful_login_entry(Config) ->
    %% check number of patterns you're testing
    Pattern = "ftpes session <0.[0-9]+.[0-9]+> started",
    NoOfOccurancesBefore = get_security_log_count(Pattern),
    
    %% Perform action (e.g connect-disconnect)
    {ok, NewConfig} = set_control_connection(Config),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    
    %% Check Security Log
    ok = check_log_count(NoOfOccurancesBefore, Pattern, 10, sec_log),
    Config.

% TC-US4.4-1a
% Connect to ftpesServer using invalid username and check Security Log
% @hidden
unsuccessful_login_entry(Config) ->
    %% check number of patterns you're testing
    Pattern = "ftpes accept error: {tls_alert,\"handshake failure\"} ",
    NoOfOccurancesBefore = get_security_log_count(Pattern),
    
    %% Remove certificates from config
    ConnectionArgs = ?config(args, Config),
    NoTlsConnectionArgs = lists:keyreplace(tls, 1, ConnectionArgs, {tls, []}),
    NoTlsConfig = lists:keyreplace(args, 1, Config, {args, NoTlsConnectionArgs}),
    
    %% Connect using non-trusted certificate
    {error, ?ERR_HANDSHAKE_FAIL} = set_control_connection(NoTlsConfig),
    
    %% Check Security Log
    ok = check_log_count(NoOfOccurancesBefore, Pattern, 10, sec_log),
    Config.

% TC-US4.4-2
% Connect to ftpesServer using non-trusted certificate and check Security Log
% @hidden
unsuccessful_login_entry_wrong_cert(Config) ->
    %% check number of patterns you're testing
    Pattern = "ftpes accept error: {tls_alert,\"unknown ca\"} ",
    NoOfOccurancesBefore = get_security_log_count(Pattern),
    
    %% Switch out certificates from config
    DataDir = ?config(data_dir, Config),
    NonTrustedCert = [{certfile, DataDir ++ "alt_user_expert.crt"}, {keyfile, DataDir ++ "alt_user_expert.key"}],
    
    ConnectionArgs = ?config(args, Config),
    NonTrustedConnectionArgs = lists:keyreplace(tls, 1, ConnectionArgs, {tls, NonTrustedCert}),
    NonTrustedConfig = lists:keyreplace(args, 1, Config, {args, NonTrustedConnectionArgs}),
    
    %% Connect using non-trusted certificate
    ct:log("Non-trusted certificate: ~p~n", [?config(tls, ?config(args, NonTrustedConfig))]),
    {error, ?ERR_UNKNOWN_CA }= set_control_connection(NonTrustedConfig),
    
    %% Check Security Log
    ok = check_log_count(NoOfOccurancesBefore, Pattern, 10, sec_log),
    Config.

% TC-US4.4-3
% Start FtpesServer, then disconnect and check Security Log
% @hidden
duration_of_session(Config) ->
    %% check number of patterns you're testing
    Pattern = "ftpes session <0.[0-9]+.[0-9]+> ended",
    NoOfOccurancesBefore = get_security_log_count(Pattern),
    
    %% Perform action (e.g connect-disconnect)
    {ok, NewConfig} = set_control_connection(Config),
    ok = ftpes_test_lib:close_ftp_connection(?config(socket,NewConfig)),
    
    %% Check Security Log
    ok = check_log_count(NoOfOccurancesBefore, Pattern, 10, sec_log),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access control tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TC-US5-1
% @hidden
intercept_open_read(Config) ->
    File = ?config(file_name, Config),
    Folder = ?config(folder, Config),
    Content = <<"Test content">>,
    
    ct:log("Writing file content"),
    ok = rct_rpc:call(rpc, file, write_file, [?RCS_DIR ++ "/sftp/" ++  Folder ++ "/" ++ File, Content], 100000),
    
    ct:log("Reading file content"),
    {ok, Bin} = ftp:recv_bin(?config(socket, Config), File),
    
    ct:log("File content: ~p", [Bin]),
    
    ct:log("Deleting file"),
    ok = ftp:delete(?config(socket, Config), File),
    Config.

% TC-US5-3
% @hidden
intercept_open_write(Config) ->
    File = ?config(file_name, Config),
    Content = <<"Test content">>,
    
    ct:log("Writing file content"),
    {error, Reason} = ftp:send_bin(?config(socket, Config), Content, File),
    
    ct:log("Writing file failed: ~p", [Reason]),

    Config.

% TC-US5-4
% @hidden
intercept_delete_file(Config) ->
    File = ?config(file_name, Config),
    Folder = ?config(folder, Config),
    Content = <<"Test content">>,
    
    ct:log("Writing file content"),
    ok = rct_rpc:call(rpc, file, write_file, [?RCS_DIR ++ "/sftp/" ++  Folder ++ "/" ++ File, Content], 100000),
    
    ct:log("Deleting file"),
    ok = ftp:delete(?config(socket, Config), File),
    Config.

% TC-US5-5
% @hidden
intercept_delete_directory(Config) ->
    Directory = "newDirectory",
    
    ct:log("Creating directory"),
    ok = rct_rpc:call(rpc, file, make_dir, [?RCS_DIR ++ "/sftp/" ++ Directory], 100000),

    ct:log("Deleting directory"),
    {error, Reason} = ftp:rmdir(?config(socket, Config), Directory),
    
    ct:log("Deleting directory failed: ~p", [Reason]),

    ok = rct_rpc:call(rpc, file, del_dir, [?RCS_DIR ++ "/sftp/" ++ Directory], 100000),
    Config.

% TC-US5-6
% @hidden
intercept_list_directory(Config) ->
    ct:log("Listing directory content"),
    {ok, Listing} = ftp:ls(?config(socket, Config)),
    
    ct:log("Listing: ~p", [Listing]),
    Config.

% TC-US5-7
% @hidden
intercept_print_current_working_directory(Config) ->
    ct:log("Printing working directory"),
    
    {ok, Dir} = ftp:pwd(?config(socket, Config)),
    ct:log("Working Directory: ~p", [Dir]),
    Config.

% TC-US5-8
% @hidden
intercept_make_directory(Config) ->
    Directory = "NewDirectory",
    
    ct:log("Creating new directory"),
    {error, Reason} = ftp:mkdir(?config(socket, Config), Directory),
    
    ct:log("Creating directory failed: ~p", [Reason]),
    Config.

% TC-US5-9
% @hidden
intercept_rename_file(Config) ->
    File = ?config(file_name, Config),
    Folder = ?config(folder, Config),
    NewFile = "newFile.txt",
    Content = <<"Test content">>,
    
    ct:log("Writing file content"),
    ok = rct_rpc:call(rpc, file, write_file, [?RCS_DIR ++ "/sftp/" ++  Folder ++ "/" ++ File, Content], 100000),
    
    ct:log("Renaming file"),
    {error, Reason} = ftp:rename(?config(socket, Config), File, NewFile),
    
    ct:log("Renaming failed: ~p", [Reason]),
    
    ct:log("Deleting file"),
    ok = ftp:delete(?config(socket, Config), File),
    Config.

% TC-US5-1a
% PMS intercept open read ROP file
% @hidden
open_read_rop_PMS_intercept(Config) ->
    
    ct:log("Create a dummy ROP file"),
    Filename = "dummy_rop",
    Content = <<"This is content of dummy ROP file\n\r">>,
    
    ftpes_test_lib:create_rop_file(Filename, Content),
    
    ct:log("Try to fetch ROP file"),
    {ok, GZip} = ftp:recv_bin(?config(socket, Config), Filename ++ ".gz"),
    Content = zlib:gunzip(GZip),
    
    ct:log("Delete dummy ROP file"),
    ftpes_test_lib:delete_rop_file(Filename),
    
    Config.
    
% TC-US5-1b
% PMS intercept open read non-ROP file
% @hidden
open_read_non_rop_PMS_intercept(Config) ->
    
    ct:log("Create a dummy ROP file"),
    Filename = "dummy_rop",
    Content = <<"This is content of dummy ROP file\n\r">>,
    
    ftpes_test_lib:create_rop_file(Filename, Content),
    
    ct:log("Try to fetch ROP directory - expected to fail"),
    {error, epath} = ftp:recv_bin(?config(socket, Config), "/rop"),
    
    ct:log("Delete dummy ROP file"),
    ftpes_test_lib:delete_rop_file(Filename),

    Config.

% TC-US5-2a
% PMS intercept open write ROP file
% @hidden
open_write_rop_PMS_intercept(Config) ->
    
    ct:log("Try to write ROP file in /rop directory"),
    Filename = "dummy_rop",
    Content = <<"This is content of dummy ROP file\n\r">>,
    
    {error, epath} = ftp:send_bin(?config(socket, Config), Content, Filename ++ ".gz"),

    Config.

% TC-US5-3a
% PMS intercept delete file
% @hidden
delete_rop_PMS_intercept(Config) ->
    
    ct:log("Create a dummy ROP file"),
    Filename = "dummy_rop",
    Content = <<"This is content of dummy ROP file\n\r">>,
    
    ftpes_test_lib:create_rop_file(Filename, Content),
    
    ct:log("Try to fetch ROP file"),
    {ok, GZip} = ftp:recv_bin(?config(socket, Config), Filename ++ ".gz"),
    Content = zlib:gunzip(GZip),
    
    ct:log("Try to delete ROP file"),
    ok = ftp:delete(?config(socket, Config), Filename ++ ".gz"),
    
    Config.

% TC-US5-3b
% PMS intercept delete file unsuccessful
% @hidden
delete_rop_PMS_intercept_unsuccessful(Config) ->
        
    ct:log("Create a dummy ROP file"),
    Filename = "dummy_rop",
    Content = <<"This is content of dummy ROP file\n\r">>,
    
    ftpes_test_lib:create_rop_file(Filename, Content),
    
    ct:log("Try to delete ROP file before reading it"),
    timer:sleep(5000),
    rct_rpc:call(rpc, pmsDb, rop_file_info, [Filename ++ ".gz"], 10000),
    {error, epath} = ftp:delete(?config(socket, Config), Filename ++ ".gz"),
    
    ct:log("Delete dummy ROP file"),
    ftpes_test_lib:delete_rop_file(Filename),
    
    Config.

get_session_info(Config) ->
    User = "steph",
    Default = "stef",
    UserData = #user_data{peer_user = User},
    Proplist = rct_rpc:call(rpc, ftpesI, get_ftpes_session_info, [UserData], 10000),
    {user, User} = lists:keyfind(user, 1, Proplist), 
    User = rct_rpc:call(rpc, ftpesI, get_ftpes_session_info, [UserData, user], 10000),
    undefined = rct_rpc:call(rpc, ftpesI, get_ftpes_session_info, [UserData, peer_address], 10000),
    User = rct_rpc:call(rpc, ftpesI, get_ftpes_session_info, [UserData, user, Default], 10000),
    Default = rct_rpc:call(rpc, ftpesI, get_ftpes_session_info, [UserData, peer_port, Default], 10000),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  US 9.1 IP IF client tests
%%  US 9.2 CERT block events client tests
%%  US 9.3 NC and TC handling client tests
%%  US 9.4 ftpI tests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IP IF client tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TC-US9.1-1 Start FTP client on target node with OAM
start_client_all_interfaces_up(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Connect to FTPES client"),
    {ok, Pid} = start_client(Config),
%%     rpc_write_file([{client_pid, Pid} | Config]),
%%     read_data_from_file([{client_pid, Pid} | Config]),
    
    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.

%% TC-US9.1-1a Start FTP client on target node with OAM down
start_client_oam_down(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    %% TODO: how to deactivate OAM without deactivating ALT OAM?
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),

    ct:log("Remove OAM reference"),
    ok = ftpes_test_lib:delete_oam_ref(oam),
    
    ct:log("Check that OAM is down"),
    nok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
%%     ct:log("Check that ALT OAM is configured"),
%%     ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Connect to FTPES client"),
    {ok, Pid} = start_client(Config),
%%     read_data_from_file([{client_pid, Pid} | Config]),
    
    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config,
    
    ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    
    Config.

%% TC-US9.1-1b Start FTP client on target node with both OAM and Alt_OAM down
start_client_oam_alt_oam_down(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that both OAM and Alt_OAM are down"),
    nok = ftpes_test_lib:oam_exists(IpProtocol),

    ct:log("Connect to FTPES client"),
    {ok, Pid} = start_client(Config),
%%     read_data_from_file([{client_pid, Pid} | Config]),
    
    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.

%% TC-US9.1-1a, b, c, d
start_client_all_interfaces_up_unsuccessful(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Connect to FTPES client"),
    start_client_unsuccessful(Config),

    Config.

%% TC-US9.1-1f, g, h, i
start_client_oam_down_unsuccessful(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    %% TODO: how to deactivate OAM without deactivating ALT OAM?
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),

    ct:log("Remove OAM reference"),
    ok = ftpes_test_lib:delete_oam_ref(oam),
    
    ct:log("Check that OAM is down"),
    nok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
%%     ct:log("Check that ALT OAM is configured"),
%%     ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Connect to FTPES client"),
    start_client_unsuccessful(Config),
%%     read_data_from_file([{client_pid, Pid} | Config]),
    
    ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    
    Config.

%% TC-US9.1-1k, l, m, n
start_client_oam_alt_oam_down_unsuccessful(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that both OAM and Alt_OAM are down"),
    nok = ftpes_test_lib:oam_exists(IpProtocol),

    ct:log("Connect to FTPES client"),
    start_client_unsuccessful(Config),
%%     read_data_from_file([{client_pid, Pid} | Config]),
    
    Config.

%% TC-US9.1-1c Start FTP client on target node with all IP interfaces down
start_client_all_interfaces_down(Config) ->
%%     ClientHost = ?config(client_host, Config),
%%     ClientPort = ?config(client_port, Config),
%%     ClientUser = ?config(client_user, Config),
%%     ClientPass = ?config(client_pass, Config), 
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that both OAM and Alt_OAM are down"),
    nok = ftpes_test_lib:oam_exists(IpProtocol),
    
    %%emarnek: What will we do with LMT?
    Config.

% @hidden TC-US9.1-2
oam_config_change_client(Config) -> 
    IpProtocol = ?config(ip_protocol, Config),
%%     File = ?config(file_name, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),

    ct:log("Try to connect to OAM"),
    {ok, _Pid} = start_client(Config),

    ct:log("Change OAM reference to ALT OAM address reference"),
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(alt_oam, IpProtocol), oam),
    
%%     ct:log("Check that new connection cannot be established to OAM address"),
%%     {error, _} = rct_rpc:call(rpc, ftpesI, rename, [Pid, File, "document.txt"], 10000),
    
    ct:log("Try to connect to ALT_OAM"),
    {ok, Pid2} = start_client(Config),

    ct:log("Close the connection"),
    stop_client([{client_pid, Pid2} | Config]),

    ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    Config.

% @hidden TC-US9.1-2a 
alt_oam_config_change_client(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
%%     File = ?config(file_name, Config),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Try to connect to OAM"),
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    {ok, _Pid} = start_client(Config),
 
    ct:log("Change ALT OAM address (hardcoding address since it seems it is not used)"),
    AltOamRefAddress = ftpes_test_lib:get_oam_ip(alt_oam),
    ok = ftpes_test_lib:edit_oam_ip(create_new_valid_address(AltOamAddress) ++ get_address_prefix(IpProtocol), alt_oam, IpProtocol),

%%     ct:log("Check that the connection is terminated"),
%%    {error, _} = rct_rpc:call(rpc, ftpesI, rename, [Pid, File, "document.txt"], 10000),
    
    ct:log("Check that new connection can be established to new ALT OAM address"),
    {ok, Pid2} = start_client(Config),

    ct:log("Close the connection"),
    stop_client([{client_pid, Pid2} | Config]),

    ct:log("Change ALT OAM address back"),
    ok = ftpes_test_lib:edit_oam_ip(AltOamRefAddress, alt_oam, IpProtocol),
    
    Config.
                                  
% @hidden  TC-US9.1-2b ????????????????????                            
lmt_config_change_client(Config) ->
  Config.

%% TC-US9.1-3
oam_down_client(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),

    ct:log("Start FTPES client on OAM interface"),
    {ok, _Pid} = start_client(Config),
    
    %% We can't remove OAM without Alt_OAM
    ct:log("Removing references from OAM and Alt OAM"),
    ok = ftpes_test_lib:delete_oam_ref(alt_oam),
    ok = ftpes_test_lib:delete_oam_ref(oam),
%%     
%%     ct:log("Check that FTPES client is stopped"),
%%     Info = process_info(Pid), % should be undefined
    
    ct:log("Restart FTPES client on LMT interface"),
    {ok, NewPid} = start_client(Config),
%%     read_data_from_file([{client_pid, NewPid} | Config]),
    
    ct:log("Set OAM references back"),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),
    
    ct:log("Close FTPES client"),
    stop_client([{client_pid, NewPid} | Config]),
    
    Config.

%% TC-US9.1-3a
alt_oam_down_client(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    %% We can't remove OAM without Alt_OAM so this test is kinda doing nothing, it just starts client on LMT interface
    ct:log("Check that both OAM and Alt_OAM are down"),
    nok = ftpes_test_lib:oam_exists(IpProtocol),

    {ok, Pid} = start_client(Config),

    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.

%% TC-US9.1-3b
lmt_down_client(Config) ->
    IpProtocol = ?config(ip_protocol, Config),
    
    %% Again, how to make that LMT is brought down???
    ct:log("Check that both OAM and Alt_OAM are down"),
    nok = ftpes_test_lib:oam_exists(IpProtocol),

    {ok, Pid} = start_client(Config),
    
    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CERT block events client tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TC-US9.2-1
% NodeCredential MO install client
% @hidden
node_credential_no_install_client(Config) ->
    %% Parameters for reinstalling NC MO
    Priv_dir =
    filename:join(
        lists:droplast(
        filename:split(
            ?config(priv_dir, Config)))),
    FileNc = filename:join([Priv_dir, "tls_nodecert_pkcs12"]),
    FirstNcId = ?config(firstNC, Config),
    FirstTcId = ?config(firstTC, Config),
    IpProto = ftpes_test_lib:get_oam_ip_protocol(),
    SftpUser = ftpes_test_lib:get_sftp_server_username(),
    SftpAddress = ftpes_test_lib:get_sftp_server_address(IpProto),
    %Uri = "sftp://" ++ Uname ++ "@" ++ Shost ++ File,
    UriNc = "sftp://" ++ SftpUser ++ "@" ++ SftpAddress ++ FileNc,
    NcCredPwd = "idiocy",
    NcFingerPrint = "17:8b:19:ef:57:e1:12:62:67:33:f5:bd:bd:8c:28:8e:bd:4b:c2:ce",
    
    %% Creating nc MO
    ct:log("Creating nc MO"),
    ftpes_test_lib:create_NC_MO([{nodeCredentialID, FirstNcId} | Config]),
    timer:sleep(100),
    
    %% Trying to connect to client
    ftpes_test_lib:set_nc_tc_client(FirstNcId, FirstTcId),
    ct:log("Try to connect to client"),
    {error, _Reason} = start_client(Config),
    
    %% Installing nc MO
    ct:log("Installing nc MO"),
    ftpes_test_lib:install_credentials([{nodeCredentialPwd, NcCredPwd}, {nodeCredentialFPrint, NcFingerPrint},
                                    {nodeCredentialID, FirstNcId}, {uriNc, UriNc} | Config]),
    
     %% Trying to connect to client
    ftpes_test_lib:set_nc_tc_client(FirstNcId, FirstTcId),
    ct:log("Try to connect to client"),
    Pid = try_to_connect_client(Config, 10),
    
    %% Close FTPES client
    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.

try_to_connect_client(_,0) -> ct:fail("Couldn't connect!");
try_to_connect_client(Config, Try) ->
    timer:sleep(2000),
    case start_client(Config) of
        {ok, Pid} -> Pid;
        {error, _Reason} -> try_to_connect_client(Config, Try - 1)
    end.

% TC-US9.2-2
% TrustedCategory MO set trustedCertificate client
% @hidden
trust_category_set_wrong_value_client(Config) ->
    FirstTcId = ?config(firstTC, Config),
    SecondTcId = ?config(secondTC, Config),
    ct:log("Set trusted categories to point to wrong tc"),
    %% Set trusted categories to point on wrong trusted certificate, system created
    MoRef = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=1"],
    ftpes_test_lib:set_trust_category([{trustCategory, FirstTcId} | Config], MoRef),
    ftpes_test_lib:set_trust_category([{trustCategory, SecondTcId} | Config], MoRef),
    Result = ftpes_test_lib:get_all_node_credentials("1"),
    ct:log("NC after setting: ~p",[Result]),
    Result2 = ftpes_test_lib:get_all_trusted_certificates("1"),
    ct:log("TC after setting: ~p",[Result2]),
    
    %% Trying to connect to client
    FirstNcId = ?config(firstNC, Config),
    ftpes_test_lib:set_nc_tc_client(FirstNcId, FirstTcId),
    ct:log("Try to connect to client"),
    {error, _Reason} = start_client(Config),

    % Set trusted categories to point on right trusted certificate
    ct:log("Set trusted categories to point on right tc"),
    MoRefGood = ["ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=2"],
    ftpes_test_lib:set_trust_category([{trustCategory, FirstTcId} | Config], MoRefGood),
    ftpes_test_lib:set_trust_category([{trustCategory, SecondTcId} | Config], MoRefGood),
    Result3 = ftpes_test_lib:get_all_node_credentials("1"),
    ct:log("NC after real setting: ~p",[Result3]),
    Result4 = ftpes_test_lib:get_all_trusted_certificates("1"),
    ct:log("TC after real setting: ~p",[Result4]),
    timer:sleep(5000),
    %% Trying to connect to client
    ct:log("Try to connect to client"),
    {ok, Pid} = start_client(Config),
    
    %% Close FTPES client
    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.

% TC-US9.2-3
successful_cipher_suite_changed_client(Config) ->
    %% Trying to connect to client
    ct:log("Try to connect to client"),
    {ok, Pid} = start_client(Config),
    
    CipherList = rct_rpc:call(rpc, comsaI, get_tls_cipher_suites, [], 10000),
    ct:log("Cipher list : ~p", [CipherList]),
    %% Change cipher suite
    Cipher = "ALL:-aRSA:-CBC:SHA256",
    ct:log("Changing cipher suite, filer ~p", [Cipher]),
    ok = ftpes_test_lib:set_tls_cipher_suites(Cipher),
    timer:sleep(1000),
    CipherList2 = rct_rpc:call(rpc, comsaI, get_tls_cipher_suites, [], 10000),
    case lists:sort(CipherList) =:= lists:sort(CipherList2) of
        false -> ct:log("Change cipher suite successful");
            _ -> ct:fail("Change cipher suite successful"),
                 Config
    end,
    %% Check that client needs to be restarted
    undefined = rct_rpc:call(rpc, erlang, process_info, [Pid], 10000),
    
    %% Change cipher suite back
    CipherBack = "ALL",
    ct:log("Changing cipher suite, filer ~p", [CipherBack]),
    ok = ftpes_test_lib:set_tls_cipher_suites(CipherBack),
    timer:sleep(1000),
    CipherList3 = rct_rpc:call(rpc, comsaI, get_tls_cipher_suites, [], 10000),
    case lists:sort(CipherList) =:= lists:sort(CipherList3) of
        true -> ct:log("Change cipher suite successful");
           _ -> ct:fail("Change cipher suite successful"),
                 Config
    end,
    Config.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NC and TC handling client tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @hidden TC-US9.3-1
set_nc_tc_client(Config) ->
    %% Set NC and TC 
    FirstNcId = ?config(firstNC, Config),
    FirstTcId = ?config(firstTC, Config),
    ct:log("Setting nc and tc"),
    ftpes_test_lib:set_nc_tc_client(FirstNcId, FirstTcId),
    %% Check that the client can be started
    {ok, Pid} = start_client(Config),

    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.
    
% @hidden TC-US9.3-1a
%% FtpTls MO update nodeCredential 
node_credential_update_client(Config) ->
    Nc_name = ?config(secondNC, Config),
    
    {ok, Pid} = start_client(Config),
    
    ct:log("Setting nc"),
    ftpes_test_lib:set_nc_tc_client(Nc_name, []),
    
    %% Checking if client is stopped
    undefined = rct_rpc:call(rpc, erlang, process_info, [Pid], 10000),
    
    %% Check that the client can be restarted
    {ok, Pid2} = start_client(Config),

    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid2} | Config]),
    Config.

% @hidden TC-US9.3-1b
%% FtpTls MO update trustCategory 
trust_category_update_client(Config) ->
    Tc_name = ?config(secondTC, Config),
    
    {ok, Pid} = start_client(Config),
    
    ct:log("Setting tc"),
    ftpes_test_lib:set_nc_tc_client([], Tc_name),
    
    %% Checking if client is stopped
    undefined = rct_rpc:call(rpc, erlang, process_info, [Pid], 10000),
    
    %% Check that the client can be restarted
    {ok, Pid2} = start_client(Config),

    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid2} | Config]),
    Config.

% @hidden TC-US9.3-1c
%% FtpTls MO update nodeCredential and trustCategory with the same value 
nc_tc_no_update_client(Config) ->
    Nc_name = ?config(firstNC, Config),
    Tc_name = ?config(firstTC, Config),
    
    {ok, Pid} = start_client(Config),
                           
    ct:log("Set same values for nc and tc as before"),
    ftpes_test_lib:set_nc_tc_client(Nc_name, Tc_name),

    %% Checking if client is still running
    case rct_rpc:call(rpc, erlang, process_info, [Pid], 10000) of
        undefined -> ct:fail("Client is stopped");
        _Other -> ok
    end,

    ct:log("Close FTPES client"),
    stop_client([{client_pid, Pid} | Config]),
    Config.


% @hidden TC-US9.3-1d
%% FtpTls MO remove nodeCredential
remove_nc_client(Config) ->
    {ok, Pid} = start_client(Config),
    
    ct:log("Remove nc ref from FtpTls"),
    %% Remove NC ref from FtpTls
    ftpes_test_lib:set_nc_tc_client(undefined, []),
    
    %% Checking if client is stopped
    undefined = rct_rpc:call(rpc, erlang, process_info, [Pid], 10000),
    
    %%checking that client cannot be started
    {error, _Reason} = start_client(Config),
    
    %% Set NC back
    ct:log("Setting nc"),
    Nc_name = ?config(firstNC, Config),
    ftpes_test_lib:set_nc_tc_client(Nc_name, []),
    
    Config.

% @hidden TC-US9.3-1e
%% FtpTls MO remove trustCategory
remove_tc_client(Config) ->
    {ok, Pid} = start_client(Config),
    
    ct:log("Remove tc ref from FtpTls"),
    %% Remove NC ref from FtpTls
    ftpes_test_lib:set_nc_tc_client([], undefined),
    
    %% Checking if client is stopped
    undefined = rct_rpc:call(rpc, erlang, process_info, [Pid], 10000),
    
    %%checking that client cannot be started
    {error, _Reason} = start_client(Config),
    
    %% Set NC back
    ct:log("Setting tc"),
    Tc_name = ?config(firstTC, Config),
    ftpes_test_lib:set_nc_tc_client([], Tc_name),
    
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% US 9.4 ftpesI tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @hidden TC-US9.4-1
start_channel_with_hostname_and_options(Config) ->
    Server = ?config(server, Config),
    ct:log("Start ~p channel", [Server]),
    {ok, Pid, _ChanRef} = start_channel(undefined, Server, Config),
    stop_channel(Server, [{client_pid, Pid}| Config] ),
    Config.

% @hidden TC-US9.4-1a
start_channel_with_hostname_options_and_port(Config) ->
    Server = ?config(server, Config),
    Port = ?config(client_port, Config),
    {ok, Pid, _ConRef} = start_channel(Port, Server, Config),
    ok=stop_channel(Server, [{client_pid, Pid}|Config]),
    Config.

% @hidden TC-US9.4-1b
start_channel_unsuccessful(Config) ->
    Server = ?config(server, Config),
    Port = 45,
    {error, _Reason} = start_channel(Port, Server, Config),
    Config.

% @hidden TC-US9.4-1c
start_channel_with_hostname_port_username_password(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    User = ?config(client_user, Config),
    Pass = ?config(client_pass, Config),
    Port = ?config(client_port, Config),
    ct:log("Start ~p client", [Server]),
    {ok, Pid, _ConRef} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host, Port, User, Pass], 10000),
    ok=stop_channel(Server, [{client_pid, Pid}|Config]),
    Config.

start_channel_empty_options(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    ct:log("Start ~p client", [Server]),
    {error, euser} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host,[]], 10000),
    Config.

start_channel_empty_options_wrong_host(Config) ->
    Server = ?config(server, Config),
    Host = "256.68.1.256",
    ct:log("Start ~p client", [Server]),
    {error, euser} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host,[]], 10000),
    Config.

start_channel_port_empty_options(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    Port = ?config(client_port, Config),
    ct:log("Start ~p client", [Server]),
    {error, euser} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host, Port, []], 10000),
    Config.

start_channel_wrong_port_empty_options(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    Port = 24,
    ct:log("Start ~p client", [Server]),
    {error, _Reason} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host, Port, []], 10000),
    Config.

start_channel_no_user_in_options(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    ct:log("Start ~p client", [Server]),
    {error, euser} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host,[{connect_timeout, 30000}]], 10000),
    Config.

start_channel_port_no_user_in_options(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    Port = ?config(client_port, Config),
    ct:log("Start ~p client", [Server]),
    {error, euser} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host, Port, [{connect_timeout, 30000}]], 10000),
    Config.

% @hidden TC-US9.4-1d
start_channel_with_hostname_port_username_password_with_opts(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    User = ?config(client_user, Config),
    Pass = ?config(client_pass, Config),
    Port = ?config(client_port, Config),
    ct:log("Start ~p client", [Server]),
    Opts = [{connect_timeout, 30000}],
    {ok, Pid, _ConRef} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host, Port, User, Pass, Opts], 10000),
    ok=stop_channel(Server, [{client_pid, Pid}|Config]),
    Config.

% @hidden TC-US9.4-1e
start_channel_with_alt_oam_only(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    User = ?config(client_user, Config),
    Pass = ?config(client_pass, Config),
    Port = ?config(client_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),
    
    ct:log("Start ~p client", [Server]),
    {ok, Pid, _ConRef} = rct_rpc:call(rpc, ftpI, start_channel_only_alt, [Server, Host, Port, User, Pass], 10000),
    ok=stop_channel(Server, [{client_pid, Pid}|Config]),
    Config.

% @hidden TC-US9.4-1f
start_channel_with_alt_oam_only_with_opts(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    User = ?config(client_user, Config),
    Pass = ?config(client_pass, Config),
    Port = ?config(client_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    Opts = [{timeout, 300000},{connect_timeout, 10000}], %from aicGui.erl
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),
    
    ct:log("Start ~p client", [Server]),
    {ok, Pid, _ConRef} = rct_rpc:call(rpc, ftpI, start_channel_only_alt, [Server, Host, Port, User, Pass, Opts], 10000),
    ok=stop_channel(Server, [{client_pid, Pid}|Config]),
    Config.

% @hidden TC-US9.4-1g
start_channel_with_alt_oam(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    User = ?config(client_user, Config),
    Pass = ?config(client_pass, Config),
    Port = ?config(client_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    
    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),
    
    ct:log("Start ~p client", [Server]),
    {ok, Pid, _ConRef} = rct_rpc:call(rpc, ftpI, start_channel_with_alt, [Server, Host, Port, User, Pass], 10000),
    ok=stop_channel(Server, [{client_pid, Pid}|Config]),
    Config.

% @hidden TC-US9.4-1h
start_channel_with_alt_oam_with_opts(Config) ->
    Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    User = ?config(client_user, Config),
    Pass = ?config(client_pass, Config),
    Port = ?config(client_port, Config),
    IpProtocol = ?config(ip_protocol, Config),
    Opts = [{timeout, 300000},{connect_timeout, 10000}], %from aicGui.erl

    ct:log("Check that OAM is configured"),
    ok = ftpes_test_lib:oam_exists(oam, IpProtocol),
    
    ct:log("Check that ALT OAM is configured"),
    ok = ftpes_test_lib:oam_exists(alt_oam, IpProtocol),
    
    ct:log("Start ~p client", [Server]),
    {ok, Pid, _ConRef} = rct_rpc:call(rpc, ftpI, start_channel_with_alt, [Server, Host, Port, User, Pass, Opts], 10000),
    ok=stop_channel(Server, [{client_pid, Pid}|Config]),
    Config.
% @hidden TC-US9.4-2b
stop_channel_and_close_connection_reference(Config) ->
     Server = ?config(server, Config),
    Host = ?config(client_host, Config),
    User = ?config(client_user, Config),
    Pass = ?config(client_pass, Config),
    Port = ?config(client_port, Config),
    ct:log("Start ~p client", [Server]),
    {ok, Pid, ConRef} = rct_rpc:call(rpc, ftpI, start_channel, [Server, Host, Port, User, Pass], 10000),
     ok = rct_rpc:call(rpc, ftpI, stop_channel, [Server, Pid, ConRef], 10000), 
     Config.

% @hidden TC-US9.4-2
stop_channel(Config) ->
    Server = ?config(server, Config),
    ok = stop_channel(Server, Config),
 Config.

% @hidden TC-US9.4-3
open_file_in_read_mode(Config) ->
   Pid = ?config(client_pid, Config),
   File = ?config(file_name, Config),
   FilePath = ?config(file_path, Config),
   Server = ?config(server, Config),
   {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [read]], 10000),
   ct:log("File opened for reading, handle: ~p", [Handle]),
   ct:log("Close file after reading"),
   rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
   [{file_handle, Handle}|Config].

% @hidden TC-US9.4-3b
open_file_in_read_mode_binary(Config) ->
   Pid = ?config(client_pid, Config),
   File = ?config(file_name, Config),
   FilePath = ?config(file_path, Config),
   Server = ?config(server, Config),
   {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [read, binary]], 10000),
   ct:log("File opened for reading, handle: ~p", [Handle]),
   ct:log("Close file after reading"),
   rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
   [{file_handle, Handle}|Config].


% @hidden TC-US9.4-3a
open_file_in_read_mode_unsuccessful(Config) ->
   Pid = ?config(client_pid, Config),
   Server = ?config(server, Config),
   FilePath = ?config(file_path, Config),
   try rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ "nonExistingFile.txt", [read]], 10000) of
       {error, Reason} -> 
        ct:log("File opened for reading unsuccessful, ~p", [Reason]);
       {ok, _Handle} ->
           ct:log("Was expected to fail, ~p", [Server])
   catch
        error:Reason ->
            ct:log("Unknown error ~p", [Reason])
   end,
   Config.


% @hidden TC-US9.4-4
open_file_in_write_mode(Config) ->
   Pid = ?config(client_pid, Config),
   File = ?config(file_name, Config),
   FilePath = ?config(file_path, Config),
   Server = ?config(server, Config),
   {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [write]], 10000),
   ct:log("File opened for writing, handle: ~p", [Handle]),
   ct:log("Close file after reading"),
   rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
   Config.


% @hidden TC-US9.4-5
close_file_read_mode(Config) ->
    Pid = ?config(client_pid, Config),
    Handle = ?config(file_handle, Config), 
    Server = ?config(server, Config),
    ct:log("Close file read mode"),
    ok =rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
    Config.

% @hidden TC-US9.4-5a
close_file_read_mode_unsuccessful(Config) ->
    Pid = ?config(client_pid, Config),
    Handle = ?config(file_handle, Config), 
    Server = ?config(server, Config),
    ct:log("Unsuccessful close read-file"),
    {error, _Reason} = rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
    Config.
 

% @hidden TC-US9.4-6
close_file_write_mode(Config) ->
    Pid = ?config(client_pid, Config),
    Handle = ?config(file_handle, Config), 
    Server = ?config(server, Config),
    ct:log("Close file write mode"),
    ok =rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
    Config.

% @hidden TC-US9.4-6a
close_file_write_mode_unsuccessful(Config) ->
    Pid = ?config(client_pid, Config),
    Handle = ?config(file_handle, Config), 
    Server = ?config(server, Config),
    ct:log("Unsuccessful close write-file"),
    {error, _Reason} = rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
    Config.

% @hidden TC-US9.4-7
open_read_file(Config) ->
	%BigFile = ?config(big_file, Config),
%% 	DataDir = ?config(data_dir, Config),
%% 	{ok, Iodata} = file:read_file(DataDir ++ BigFile),
    Iodata = <<"0123456789ABCDEFGHIJKLMNOPRSTUVZ">>,
	Size = erlang:byte_size(Iodata),
    Server = ?config(server, Config),
    Len = 500,
    Pid =?config(client_pid, Config),
    File = ?config(file_name, Config),
    Folder = ?config(folder, Config),
    FilePath = Folder ++ "/",
    ct:log("Open file for reading"),
    {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [read]], 10000, no_print),
    Acc = read(Server, Pid, Handle, Len, 0),
	case Size == Acc of
		true -> ct:log("Successful reading file");
		_-> ct:log("Unsuccessful reading file")
	end,
    Config.

% @hidden TC-US9.4-7d
open_read_file_binary(Config) ->
    %BigFile = ?config(big_file, Config),
%%  DataDir = ?config(data_dir, Config),
%%  {ok, Iodata} = file:read_file(DataDir ++ BigFile),
    Iodata = <<"0123456789ABCDEFGHIJKLMNOPRSTUVZ">>,
    Size = erlang:byte_size(Iodata),
    Server = ?config(server, Config),
    Len = 500,
    Pid =?config(client_pid, Config),
    File = ?config(file_name, Config),
    Folder = ?config(folder, Config),
    FilePath = Folder ++ "/",
    ct:log("Open file for reading"),
    {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [read, binary]], 10000, no_print),
    Acc = read(Server, Pid, Handle, Len, 0),
    case Size == Acc of
        true -> ct:log("Successful reading file");
        _-> ct:log("Unsuccessful reading file")
    end,
    Config.

read(Server, Pid, Handle, Len, Acc) ->
    case  rct_rpc:call(rpc, ftpI, read, [Server, Pid, Handle, Len], 10000, no_print) of
        {ok, Data} ->
			     case is_binary(Data) of
                     true -> Length = erlang:byte_size(Data);
					 _-> Length = erlang:length(Data)
				 end,
				 NewAcc = Acc + Length,
				 read(Server, Pid, Handle, Len, NewAcc);

        eof -> ct:log("End of file"),
               rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
			   Acc;
       {error, Reason} -> ct:log("Reason ~p", [Reason]),
                          rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000) 
    end.
     

% @hidden TC-US9.4-7
open_read_file_timeout(Config) ->
    Server = ?config(server, Config),
    Len = 10,
    Pid =?config(client_pid, Config),
    File = ?config(file_name, Config),
    Folder = ?config(folder, Config),
    FilePath = Folder ++ "/",
    Timeout = 3000,
    ct:log("Open file for reading"),
    {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [read]], 10000),
    case  rct_rpc:call(rpc, ftpI, read, [Server, Pid, Handle, Len, Timeout], 10000, no_print) of
        {ok, Data} ->
                 ct:log("Reading data: ~p", [Data]),
                 rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000);
        eof -> ct:log("End of file");
       {error, Reason} -> ct:log("Reason ~p", [Reason]),
                          rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000) 
    end,
     
    Config.
    
% @hidden TC-US9.4-7a
open_read_file_unsuccessful(Config) ->
    Server = ?config(server, Config),
    Len = 100,
    Pid =?config(client_pid, Config),
    {error,Reason} = rct_rpc:call(rpc, ftpI, read, [Server, Pid, undefined, Len], 10000, no_print),
    ct:log("Unsuccessful reading data: ~p", [Reason]),
    Config.

% @hidden TC-US9.4-7c
open_read_file_timeout_unsuccessful(Config) ->
    Server = ?config(server, Config),
    Len = 1000000,
    Pid =?config(client_pid, Config),
    Timeout = 1,
    ct:log("Open file for reading"),
    {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, "/home/labuser/big.txt", [read]], 10000),
    {error, Reason} = rct_rpc:call(rpc, ftpI, read, [Server, Pid, Handle, Len, Timeout], 10000, no_print),
    ct:log("Reason ~p", [Reason]),
    {error, no_proc} = rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
     
    Config.

aread(Config) ->
    IoData = <<"0123456789ABCDEFGHIJKLMNOPRSTUVZ">>,
    Server = ?config(server, Config),
    Len = 5,
    N = 10, %Number of times aread will be started
    Pid =?config(client_pid, Config),
    File = ?config(file_name, Config),
    Folder = ?config(folder, Config),
    FilePath = Folder ++ "/",
    ct:log("Open file for reading"),
    {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [read, binary]], 10000, no_print),
    Result = rct_rpc:call(rpc, ftpesClientHandler, test_aread, [Server, Pid, Handle, Len, N], 10000, no_print),
    rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
    ct:log("Sequence of replies: ~p",[Result]),
    check_results(Result, <<>>, IoData).

check_results([], Data, Data) -> 
    ct:log("Successful aread"),
    ok;
check_results([eof|T], Data, Data) ->
    check_results(T, Data, Data);
check_results([{ok, Acc}|T], Data, IoData) when T =/= [] ->
    check_results(T, <<Data/binary, Acc/binary>>, IoData).

% @hidden TC-US9.4-8
open_write_file(Config) -> 
     Server = ?config(server, Config),
     File = ?config(file_name, Config),
     Data = <<"12345675271851848485253690134824518152">>,
     Folder = ?config(folder, Config),
     FilePath = Folder ++ "/",
     Pid =?config(client_pid, Config),
     ct:log("Open file for writing"),
     {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [write]], 10000),
     ct:log("Write data: ~p", [Data]),
     ok = rct_rpc:call(rpc, ftpI, write, [Server, Pid, Handle, Data ], 10000),
     ct:log("Close file after writing"),
     ok = rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
     Config.
     
% @hidden TC-US9.4-8
open_write_file_timeout(Config) -> 
     Server = ?config(server, Config),
     File = ?config(file_name, Config),
     Data = <<"12345675271851848485253690134824518152">>,
     Folder = ?config(folder, Config),
     FilePath = Folder ++ "/",
     Pid =?config(client_pid, Config),
     Timeout = 3000,
     ct:log("Open file for writing"),
     {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [write]], 10000),
     ct:log("Write data: ~p", [Data]),
     ok = rct_rpc:call(rpc, ftpI, write, [Server, Pid, Handle, Data, Timeout], 10000),
     ct:log("Close file after writing"),
     ok = rct_rpc:call(rpc, ftpI, close, [Server, Pid, Handle], 10000),
     Config.
     
% @hidden TC-US9.4-8a
open_write_file_unsuccessful(Config) ->
     Server = ?config(server, Config),
    Data = <<"12345675271851848485253690134824518152">>,
    Pid =?config(client_pid, Config),
     {error, Reason}= rct_rpc:call(rpc, ftpI, write, [Server, Pid, undefined, Data ], 10000),
     ct:log("Unsuccessful writing : ~p", [Reason]),
     Config.

% @hidden TC-US9.4-9
rename_file(Config) ->
    ct:log("Renaming file"),
    Pid = ?config(client_pid, Config),
    File = ?config(file_name, Config),
    NewFile = ?config(new_file_name, Config),
    FilePath = ?config(file_path, Config),
    
    ok = rct_rpc:call(rpc, ftpesI,rename, [Pid, FilePath ++ File, FilePath ++ NewFile], 10000),
    
    Config.

% @hidden TC-US9.4-9a
rename_file_unsuccessful(Config) ->
    ct:log("Renaming file"),
    Pid = ?config(client_pid, Config),
    File = "nonexisting.txt",
    NewFile = ?config(new_file_name, Config),
    FilePath = ?config(file_path, Config),
    
    {error, _Reason} = rct_rpc:call(rpc, ftpesI,rename, [ Pid, FilePath ++ File, FilePath ++ NewFile], 10000),

    ct:log("Failed to rename file"),
    
    Config.

% @hidden TC-US9.4-10
delete_file(Config) ->
    File = ?config(file_name, Config),
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    ok = rct_rpc:call(rpc, ftpI, delete, [Server, Pid, FilePath ++ File], 10000),
    
    Config.

% @hidden TC-US9.4-10
delete_file_timeout(Config) ->
    File = ?config(file_name, Config),
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    Timeout = 4000,
    ok = rct_rpc:call(rpc, ftpI, delete, [Server, Pid, FilePath ++ File, Timeout], 10000),
    
    Config.


% @hidden TC-US9.4-10a
delete_file_unsuccessful(Config) ->
    ct:log("Deleting file"),
    Pid = ?config(client_pid, Config),
    File = "nonexisting.txt",
    Server = ?config(server, Config),
    {error, Reason} = rct_rpc:call(rpc, ftpI,delete, [Server, Pid, File], 10000),
    ct:log("Failed to delete file: ~p", [Reason]),
    
    Config.

% @hidden TC-US9.4-11
make_directory(Config) ->
    ct:log("Creating directory"),
    Pid = ?config(client_pid, Config),
    Folder = ?config(test_folder_name, Config),
    FilePath = ?config(file_path, Config),

    ok = rct_rpc:call(rpc, ftpesI,make_dir, [Pid, FilePath ++ Folder], 10000),
    
    Config.

% @hidden TC-US9.4-11a
make_directory_unsuccessful(Config) ->
    ct:log("Creating directory"),
    Pid = ?config(client_pid, Config),
    Folder = ?config(test_folder_name, Config),
    FilePath = ?config(file_path, Config),

    ok = rct_rpc:call(rpc, ftpesI,make_dir, [Pid, FilePath ++ Folder], 10000),
    
    ct:log("Creating same directory"),
    
    {error, _Reason} = rct_rpc:call(rpc, ftpesI,make_dir, [Pid, FilePath ++ Folder], 10000),
    
    ct:log("Failed to create directory"),
    
    Config.

% @hidden TC-US9.4-12
delete_directory(Config) ->
    ct:log("Deleting directory"),
    Pid = ?config(client_pid, Config),
    Folder = ?config(test_folder_name, Config),
    FilePath = ?config(file_path, Config),

    ok = rct_rpc:call(rpc, ftpesI,del_dir, [Pid, FilePath ++ Folder], 10000),

    Config.

% @hidden TC-US9.4-12a
delete_directory_unsuccessful(Config) ->
    ct:log("Deleting directory"),
    Pid = ?config(client_pid, Config),
    Folder = "nonexisting_folder",
    FilePath = ?config(file_path, Config),

    {error, _Reason} = rct_rpc:call(rpc, ftpesI,del_dir, [Pid, FilePath ++ Folder], 10000),
    ct:log("Failed to delete directory"),
    
    Config.

% @hidden TC-US9.4-13
list_directory(Config) ->
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    {ok, Listing} = rct_rpc:call(rpc, ftpI, list_dir, [Server, Pid, FilePath], 10000),
    ct:pal("Listing : ~p", [Listing]),
    
    Config.

% @hidden TC-US9.4-13
list_directory_timeout(Config) ->
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    Timeout = 4000,
    {ok, Listing} = rct_rpc:call(rpc, ftpI, list_dir, [Server, Pid, FilePath, Timeout], 10000),
    ct:pal("Listing : ~p", [Listing]),
    
    Config.

% @hidden TC-US9.4-13a
list_directory_unsuccessful(Config) ->
    Pid = ?config(client_pid, Config),
    Folder = "nonexisting",
    Server = ?config(server, Config),
    [{_, Group}] = ?config(tc_group_properties, Config),
    
    case Group of
        ftpI_ftpes_group -> 
            {ok, _List} = rct_rpc:call(rpc, ftpI, list_dir, [Server, Pid, Folder], 10000);
        ftpI_sftp_group -> 
            {error, _Reason} = rct_rpc:call(rpc, ftpI, list_dir, [Server, Pid, Folder], 10000)
    end,
    ct:log("Listing failed"),
    
    Config.

% @hidden TC-US9.4-13d
list_directory_timeout_unsuccessful(Config) ->
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    Timeout = 1,
    {error, Reason} = rct_rpc:call(rpc, ftpI, list_dir, [Server, Pid, FilePath, Timeout], 10000),
    ct:pal("Reason : ~p", [Reason]),
    
    Config.

% @hidden TC-US9.4-14
read_file(Config) ->
    ct:log("Reading file"),
    Pid = ?config(client_pid, Config),
    File = ?config(file_name, Config),
    FilePath = ?config(file_path, Config),
    Server = ?config(server, Config),
    {ok, Content} = rct_rpc:call(rpc, ftpI,read_file, [Server, Pid, FilePath ++ File], 10000),
    ct:log("File content: ~p", [Content]),
    
    Config.

% @hidden TC-US9.4-14
read_file_timeout(Config) ->
    ct:log("Reading file"),
    Pid = ?config(client_pid, Config),
    File = ?config(file_name, Config),
    FilePath = ?config(file_path, Config),
    Server = ?config(server, Config),
    Timeout = 4500,
    {ok, Content} = rct_rpc:call(rpc, ftpI,read_file, [Server, Pid, FilePath ++ File, Timeout], 10000),
    ct:log("File content: ~p", [Content]),
    
    Config.

% @hidden TC-US9.4-14a
read_file_unsuccessful(Config) ->
    ct:log("Reading file"),
    Pid = ?config(client_pid, Config),
    File = "nonexisting_file.txt",
    FilePath = ?config(file_path, Config),
    Server = ?config(server, Config),
    {error, Reason} = rct_rpc:call(rpc, ftpI,read_file, [Server, Pid, FilePath ++ File], 10000),
    ct:log("Reading file failed: ~p", [Reason]),
    
    Config.

% @hidden TC-US9.4-14c
read_file_timeout_unsuccessful(Config) ->
    ct:log("Reading file"),
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    Timeout = 1,
    {error, Reason} = rct_rpc:call(rpc, ftpI,read_file, [Server, Pid, "/home/labuser/big.txt", Timeout], 10000),
    ct:log("Reason: ~p", [Reason]),
    
    Config.

% @hidden TC-US9.4-14
read_file_cipher_notify(Config) ->
    ct:log("Reading file"),
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    Timeout = 4500,
    _Pid = spawn(fun()-> {error, reconf} = rct_rpc:call(rpc, ftpI,read_file, [Server, Pid, "/home/labuser/big.txt", Timeout], 10000) end),
    timer:sleep(100),
    ok = rct_rpc:call(rpc, ftpesClientHandler ,ftpes_cipher_notify, [], 10000),
    
    Config.


% @hidden TC-US9.4-15
write_file(Config) ->
%% 	BigFile = ?config(big_file, Config),
%% 	DataDir=?config(data_dir, Config),
%% 	{ok, Iodata} = file:read_file(DataDir ++ BigFile),
    Iodata = <<"0123456789ABCDEFGHIJKLMNOPRSTUVZ">>,
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    File = ?config(file_name, Config),
    ok = rct_rpc:call(rpc, ftpI, write_file, [Server, Pid, FilePath ++ File, Iodata], 10000, no_print),
    Config.

% @hidden TC-US9.4-15
write_file_timeout(Config) ->
    Iodata = <<1234>>,
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    File = ?config(file_name, Config),
    Timeout = 4000,
    ok = rct_rpc:call(rpc, ftpI, write_file, [Server, Pid, FilePath ++ File, Iodata, Timeout], 10000),
    Config.

% @hidden TC-US9.4-15b
write_file_timeout_unsuccessful(Config) ->
    Iodata = <<1234>>,
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    File = ?config(file_name, Config),
    Timeout = 1,
    {error, Reason} = rct_rpc:call(rpc, ftpI, write_file, [Server, Pid, FilePath ++ File, Iodata, Timeout], 10000),
    ct:log("Reason ~p",[Reason]),
    Config.

% @hidden TC-US9.4-16
put_file(Config) ->
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = ?config(file_path, Config),
    File = ?config(file_name, Config),
    Content = <<"Test content">>,
    
    ct:log("Writing file content"),
    ok = rct_rpc:call(rpc, file, write_file, [?RCS_DIR ++ "/sftp/" ++ File, Content], 100000),
    
    ok = rct_rpc:call(rpc, ftpI, put_file, [Server, Pid, ?RCS_DIR ++ "/sftp/" ++  File, FilePath ++ File], 10000),
    
    ct:log("Deleting file"),
    ok = rct_rpc:call(rpc, file, delete, [?RCS_DIR ++ "/sftp/" ++ File], 100000),
    Config.

% @hidden TC-US9.4-16
put_file_unsuccessful(Config) ->
    Pid = ?config(client_pid, Config),
    Server = ?config(server, Config),
    FilePath = "wrongfolder/",
    File = ?config(file_name, Config),
    Content = <<"Test content">>,
    
    ct:log("Writing file content"),
    ok = rct_rpc:call(rpc, file, write_file, [?RCS_DIR ++ "/sftp/" ++ File, Content], 100000),
    
    {error, _Module, _Reason} = rct_rpc:call(rpc, ftpI, put_file, [Server, Pid, ?RCS_DIR ++ "/sftp/" ++  File, FilePath ++ File], 10000),
    
    ct:log("Deleting file"),
    ok = rct_rpc:call(rpc, file, delete, [?RCS_DIR ++ "/sftp/" ++ File], 100000),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% US 11.2 Performance and robustness tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @hidden TC-US11.2-1
multiple_clients(Config) ->
    N = 10, % number of clients
    Server = ?config(server, Config),
    Files = ["myFile.txt" ++ integer_to_list(X) || X <- lists:seq(1,N)],
    FilePath = ?config(file_path, Config),
    List_of_pids = [{ok, _} = start_client(Config) || _ <- lists:seq(1,N)],
    List_of_pid_files = pid_files(List_of_pids, Files, []),
    Self = self(),
    ct:log("List of ~p clients and their files: ~p", [N, List_of_pid_files]),
    Start = rct_rpc:call(rpc, os, timestamp, [], 10000),
    [spawn(fun() -> Result = rct_rpc:call(rpc, ftpI, put_file, [Server, Pid, "/bin/testagt", FilePath ++ File], 10000), 
                    Self!Result 
                    end) || {Pid, File} <- List_of_pid_files], %% send 10 put files for 10 clients in parallel
    Responses = [receive 
                    Whatever ->
                        Whatever
                    after 60000 ->
                        nok
                    end || _ <- List_of_pid_files], %% wait for answers
    End = rct_rpc:call(rpc, os, timestamp, [], 10000),
    Diff = timer:now_diff(End, Start), %% in microseconds
    ct:log("Time passed: ~p seconds",[Diff / 1000000]),
    [ok = Response || Response <- Responses], %% check if all responses are ok
    [ok = rct_rpc:call(rpc, ftpI, delete, [Server, Pid, FilePath ++ File], 10000, no_print) || {Pid, File} <- List_of_pid_files],
    [ok = rct_rpc:call(rpc, ftpesI, stop_client, [Pid], 10000) || {_, Pid} <- List_of_pids],
    Config.

pid_files([], [], List) -> List;
pid_files([{ok, Pid}|T1], [File|T2], List) ->
    pid_files(T1, T2, List ++ [{Pid, File}]).

% @hidden TC-US11.2-4
max_number_of_sessions(Config) ->
    N = ?config(max_sessions, Config),
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    Sessions = [{ok, _} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]) || _ <- lists:seq(1,N)],
    ct:log("Starting ~p sessions~n",[N]),
    process_flag(trap_exit, true),
    {error,eclosed} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}, {timeout, 10000}]),
    receive
        {'EXIT',timeout} -> ok
        after 5000 -> ok
    end,
    process_flag(trap_exit, false),
    [ok = ftp:close(Pid) || {_, Pid} <- Sessions],
    Config.

client_timeout(Config) ->
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    {ok, Pid} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}, {timeout, 100000}]),
    process_flag(trap_exit, true),
    timer:sleep(65000),
    receive
        {'EXIT',timeout} -> ok
        after 5000 -> ok
    end,
    Result = ftp:user(Pid, "mate", ""),
    ct:log("Result = ~p",[Result]),
    process_flag(trap_exit, false),
    ftp:close(Pid),
    Config.

% @hidden TC-US11.2-5
remote_client_crash(Config) ->
    N = ?config(max_sessions, Config),
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    Sessions = [{ok, _} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]) || _ <- lists:seq(1,N)],
    %% Crash all the sessions
    process_flag(trap_exit, true),
    [exit(Pid, kill) || {_, Pid} <- Sessions],
    [receive
        {'EXIT',timeout} -> ok
     after 5000 -> ok
     end|| _ <- lists:seq(1,N)], %% wait for answers
    %% [ok = ftp:close(Pid) || {_, Pid} <- Sessions],
    process_flag(trap_exit, false),
    Config.

% @hidden TC-US11.2-3
read_write_large_file(Config) ->
    RemoteFile = "big.txt",
    LocalFile = "/rcs/sftp/ftpes_test/",
    NewRemoteFile =  "up_test/",
    N = 10, % number of clients
    Files = ["myFile.txt" ++ integer_to_list(X) || X <- lists:seq(1,N)],
    Server = ?config(server, Config),
    List_of_pids = [{ok, _} = start_client(Config) || _ <- lists:seq(1,N)],
    List_of_pid_files = pid_files(List_of_pids, Files, []),
    Self = self(),
    ct:log("List of ~p clients and their files: ~p", [N, List_of_pid_files]),
    %% read
    Start = rct_rpc:call(rpc, os, timestamp, [], 10000),
    [spawn(fun() -> Result = rct_rpc:call(rpc, ftpI, recv_file, [Pid, RemoteFile, LocalFile ++ File], 300000),
                    Self!Result 
                    end) || {Pid, File} <- List_of_pid_files], %% send 10 read files for 10 clients in parallel
    Responses = [receive 
                    Whatever ->
                        Whatever
                    after 60000 ->
                        nok
                    end || _ <- List_of_pid_files], %% wait for answers
    End = rct_rpc:call(rpc, os, timestamp, [], 10000),
    [ok = Response || Response <- Responses], %% check if all responses are ok
    Diff = timer:now_diff(End, Start), %% in microseconds
    ct:log("Time passed for reading large file with ~p clients: ~p seconds",[N, Diff / 1000000]),
    %% write
    Start2 = rct_rpc:call(rpc, os, timestamp, [], 10000),
    [spawn(fun() -> Result = rct_rpc:call(rpc, ftpI, put_file, [Server, Pid, LocalFile ++ File, NewRemoteFile ++ File], 10000), 
                    Self!Result 
                    end) || {Pid, File} <- List_of_pid_files], %% send 10 put files for 10 clients in parallel
    Responses2 = [receive 
                    Whatever ->
                        Whatever
                    after 60000 ->
                        nok
                    end || _ <- List_of_pid_files], %% wait for answers
    End2 = rct_rpc:call(rpc, os, timestamp, [], 10000),
    [ok = Response || Response <- Responses2], %% check if all responses are ok
    Diff2 = timer:now_diff(End2, Start2), %% in microseconds
    ct:log("Time passed for writing large file with ~p clients: ~p seconds",[N, Diff2 / 1000000]),
    [ok = rct_rpc:call(rpc, file, delete, [LocalFile ++ File], 10000) || {_, File} <- List_of_pid_files],
    [ok = rct_rpc:call(rpc, ftpI, delete, [Server, Pid, NewRemoteFile ++ File], 10000) || {Pid, File} <- List_of_pid_files],
    [ok = rct_rpc:call(rpc, ftpesI, stop_client, [Pid], 10000) || {_, Pid} <- List_of_pids],
    Config.

% @hidden TC-US11.2-6
read_write_large_file_from_server(Config) ->
    N = 15, % number of sessions
    User = "mate",
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    ct:log("Starting ~p sessions",[N]),
    Self = self(),
    [spawn(fun() -> {ok, Pid} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]),
                    ok = ftp:user(Pid, User, ""),
                    ok = ftp:cd(Pid, ?FTPES_TEST_DIR),
                    %% Store N files
                    Z = integer_to_binary(X),
                    ok = ftp:send_bin(Pid, <<<<?TEST_FILE_CONTENT>>/binary, Z/binary>>, ?TEST_FILE_NAME ++ integer_to_list(X)),
                    %% Receive N files
                    Content = ftp:recv_bin(Pid, ?TEST_FILE_NAME ++ integer_to_list(X)),
                    %% Delete N files
                    ftp:delete(Pid, ?TEST_FILE_NAME ++ integer_to_list(X)),
                    ftp:close(Pid),
                    Self!Content end) || X <- lists:seq(1,N)],
    Responses = [receive 
                    Whatever ->
                        Whatever
                    after 20000 ->
                        nok
                    end || _ <- lists:seq(1,N)],
    ct:log("Responses ~p",[Responses]),
    ok = check_responses(Responses),
    Config.

check_responses([]) -> ok;
check_responses([H|T]) ->
    case H of
        nok -> nok;
        _ -> check_responses(T)
    end.

read_write_upgrade(Config) ->
    RemoteFile = "up_test/CXP9024418_6-R4A221.zip",
    LocalFile = "/rcs/sftp/ftpes_test/big_up",
    NewRemoteFile =  "up_test/big_up",
    Server = ?config(server, Config),
    {ok, Pid} = start_client(Config),
    ct:log("Pid: ~p",[Pid]),
    %% read
    Start = rct_rpc:call(rpc, os, timestamp, [], 10000),
    ok = rct_rpc:call(rpc, ftpI, recv_file, [Pid, RemoteFile, LocalFile], 300000),
    End = rct_rpc:call(rpc, os, timestamp, [], 10000),
    Diff = timer:now_diff(End, Start), %% in microseconds
    ct:log("Time passed for reading upgrade file: ~p seconds",[Diff / 1000000]),
    Size1 = rct_rpc:call(rpc, filelib, file_size, [LocalFile], 10000),
    Size2 = rct_rpc:call(rpc, ftpesI, size, [Pid, RemoteFile], 10000),
    true = Size1 =:= Size2,
    %% write
    Start2 = rct_rpc:call(rpc, os, timestamp, [], 10000),
    ok = rct_rpc:call(rpc, ftpI, put_file, [Server, Pid, LocalFile, NewRemoteFile], 300000),
    End2 = rct_rpc:call(rpc, os, timestamp, [], 10000),
    Diff2 = timer:now_diff(End2, Start2), %% in microseconds
    ct:log("Time passed for writing upgrade file: ~p seconds",[Diff2 / 1000000]),
    Size3 = rct_rpc:call(rpc, ftpesI, size, [Pid, NewRemoteFile], 10000),
    true = Size2 =:= Size3,
    ok = rct_rpc:call(rpc, file, delete, [LocalFile], 10000),
    ok = rct_rpc:call(rpc, ftpI, delete, [Server, Pid, NewRemoteFile], 10000),
    rct_rpc:call(rpc, ftpesI, stop_client, [Pid], 10000),
    Config.

% @hidden TC-US11.2-7
start_new_session_bucket_full(Config) ->
    N = 15, % number of sessions
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    %% Starting enough sessions to fill bucket
    Sessions = [{ok, _} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]) || _ <- lists:seq(1,N)],
    ct:log("Starting ~p sessions~n",[N]),
    %% Try to start another session
    Start = rct_rpc:call(rpc, os, timestamp, [], 10000),
    {ok, CPid} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]),
    End = rct_rpc:call(rpc, os, timestamp, [], 10000),
    Diff = timer:now_diff(End, Start), %% in microseconds
    ct:log("Time passed: ~p seconds",[Diff / 1000000]),
    [ok = ftp:close(Pid) || {_, Pid} <- Sessions],
    ftp:close(CPid),
    %% Check that last session didn't started immediately
    true = (Diff / 1000000) > 1,
    Config.

% @hidden TC-US11.2-7a
start_new_session_bucket_empty_max_sessions(Config) ->
    %% check number of patterns you're testing
    Pattern = "FTPES: High connection rate - rate limit in effect",
    NoOfOccurancesBefore = get_security_log_count(Pattern),
    
    N = ?config(max_sessions, Config),
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    %% Starting enough sessions to fill bucket
    Sessions = [{ok, _} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]) || _ <- lists:seq(1,N)],
    ct:log("Starting ~p sessions~n",[N]),
    timer:sleep(50000),
    [ok = ftp:close(Pid) || {_, Pid} <- Sessions],
    
    %% Check Security Log
    ok = check_log_count(NoOfOccurancesBefore, Pattern, 10, bucket_empty),
    Config.

% @hidden TC-US11.2-7b
max_sessions_bucket_not_empty(Config) ->
    %% check number of patterns you're testing
    Pattern = "FTPES: High connection rate - rate limit in effect",
    NoOfOccurancesBefore = get_security_log_count(Pattern),
    
    N = ?config(max_sessions, Config),
    N1 = round(N / 2),
    N2 = N - N1,
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    ct:log("Starting ~p sessions~n",[N1]),
    %% Starting N1 sessions
    Sessions = [{ok, _} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]) || _ <- lists:seq(1,N1)],
    timer:sleep(60000),
    ct:log("Starting ~p sessions~n",[N2]),
    %% Starting N2 sessions
    Sessions2 = [{ok, _} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]) || _ <- lists:seq(1,N2)],
    [ok = ftp:close(Pid) || {_, Pid} <- Sessions],
    [ok = ftp:close(Pid) || {_, Pid} <- Sessions2],
    timer:sleep(50000),
    
    %% Check Security Log
    ok = check_log_count(NoOfOccurancesBefore, Pattern, 10, bucket_not_empty),
    Config.




get_file_size(Config) ->
    Pid = ?config(client_pid, Config),
    FilePath = ?config(file_path, Config),
    File = ?config(file_name, Config),
    Size = rct_rpc:call(rpc, ftpesI, size, [Pid, FilePath ++ File], 10000),
    ct:log("Size is ~p", [Size]),
    Config.

get_file_size_unsuccessful(Config) ->
    Pid = ?config(client_pid, Config),
    FilePath = ?config(file_path, Config),
    {error, Reason} = rct_rpc:call(rpc, ftpesI, size, [Pid, FilePath++ "nofile"], 10000),
    ct:log("Error, ~p", [Reason]),
    Config.

is_file_test(Config) ->
    Pid = ?config(client_pid, Config),
    FilePath = ?config(file_path, Config),
    File = ?config(file_name, Config),
    case  rct_rpc:call(rpc, ftpesI, is_directory, [Pid, FilePath ++ File], 10000) of
        file ->
            ct:log("Is file!");
        _ ->
            ct:fail("Is not a file...")
    end,
    Config.

is_directory_test(Config) ->
    Pid = ?config(client_pid, Config),
    FilePath = ?config(file_path, Config),
    Folder = ?config(test_folder_name, Config),
    case  rct_rpc:call(rpc, ftpesI, is_directory, [Pid, FilePath ++ Folder], 10000) of
        directory ->
            ct:log("Is directory!");
        _ ->
            ct:fail("Is not a directory...")
    end,
    Config.

is_directory_test_unsuccessful(Config) ->
    Pid = ?config(client_pid, Config),
    FilePath = ?config(file_path, Config),
    case  rct_rpc:call(rpc, ftpesI, is_directory, [Pid, FilePath ++ "wrongFolder"], 10000) of
        directory ->
            ct:fail("Is directory!");
        {error, epath}->
            ct:log("Is not a directory...")
    end,
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% US TC3 Control connection tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FileTPM_2.0_FTPES_TC3
% Control connection port on startup
control_conn_port_startup(Config) ->
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    Default_port = ?config(port, Config),

    case ftpes_test_lib:is_target() of
        true ->
            9921 = Default_port;
        false ->
            ok
    end,

    Default_port = ftpes_test_lib:get_port(),

    {ok, Pid} = ftp:open(Host, [{port, Default_port}, {ipfamily, IpFamily}, {tls, Certificate}]),

    ok = ftp:user(Pid, "mate", ""),

    ftp:close(Pid).

% FileTPM_2.0_FTPES_TC3a
%Control connection port on reconfiguration
control_conn_port_reconf(Config) ->
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    Default_port = ?config(port, Config),
    New_port = Default_port+1,
    
    Default_port = ftpes_test_lib:get_port(),
    
    {ok, Pid} = ftp:open(Host, [{port, Default_port}, {ipfamily, IpFamily}, {tls, Certificate}]),

    ok = ftpes_test_lib:set_port(New_port),
    
    {error, ehost} = ftp:open(Host, [{port, Default_port}, {ipfamily, IpFamily}, {tls, Certificate}]),

    {ok, Pid3} = ftp:open(Host, [{port, New_port}, {ipfamily, IpFamily}, {tls, Certificate}]),

    %check that previously established session is not impacted
    ok = ftp:user(Pid, "mate", ""),

    %postconditions

    ok = ftpes_test_lib:set_port(Default_port),
    ftp:close(Pid),
    ftp:close(Pid3).

% FileTPM_2.0_FTPES_TC3b
%Server cannot be started - port in use
control_conn_port_used(Config) ->
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    Default_port = ?config(port, Config),

    {ok, [{port, ListPort}]} = rct_mo_handler_lib:get_mo({netconf,nc1}, "ManagedElement=1,SystemFunctions=1,SysM=1,NetconfSsh=1", port),
    Port = list_to_integer(ListPort),

    ok = ftpes_test_lib:set_port(Port),
    
    {error, econn} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}]),

    %postconditions
    
    ok = ftpes_test_lib:set_port(Default_port),
    {ok, Pid} = ftp:open(Host, [{port, Default_port}, {ipfamily, IpFamily}, {tls, Certificate}]),
    ftp:close(Pid),
    ok.


idle_timer_startup(Config) ->
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = inet,
    %timer + 1 min to test servers timeout
    ct:pal("Testing servers timeout"),
    {ok, Pid} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}, {timeout, 120000}]),   %clients timeout

    process_flag(trap_exit, true),
    % wait for timer to expire to test servers timeout
    timer:sleep(61000),

    % confirm that conn is closed
    {error, econn} = ftp:user(Pid, "mate", ""),
    process_flag(trap_exit, false),
    ftp:close(Pid),
    ct:pal("Servers timeout successful"),
    ok.

% @hidden TC-US?.4a
idle_timer_reconf(Config) ->
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    ct:pal("Testing servers timeout"),
    {ok, Pid} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}, {timeout, 120000}]),    %clients timeout, ms?

    %set timer to less for 2nd conn
    IdleTimer = 30, %seconds
    ct:pal("Setting new idle timer"),
    ok = ftpes_test_lib:set_idle_timer(IdleTimer),
    %establish another connection
    {ok, Pid2} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}, {timeout, 120000}]), %clients timeout
    
    process_flag(trap_exit, true),
    %wait for idle_timer to expire for 2nd conn
    %check that timer hasn't expired for 1st one
    timer:sleep(IdleTimer*1100),
    
    %check that 1st conn is open, and 2nd closed
    ok = ftp:user(Pid, "mate", ""),
    {error, econn} = ftp:user(Pid2, "mate", ""),
    process_flag(trap_exit, false),
    ftp:close(Pid),
    ftp:close(Pid2),
    ct:pal("Servers timeout successful"),
    ok.

% @hidden TC-US?.4b
idle_timer_null(Config) ->
    Port = ?config(port, Config),
    Certificate = ?config(certificate, Config),
    Host = ?config(host, Config),
    IpFamily = ?config(ip_family, Config),
    ok = ftpes_test_lib:set_idle_timer(undefined),
    ct:pal("Testing servers NULL timeout"),
    {ok, Pid} = ftp:open(Host, [{port, Port}, {ipfamily, IpFamily}, {tls, Certificate}, {timeout, 120000}]),    %clients timeout, ms?

    process_flag(trap_exit, true),
    %wait for default value and more
    timer:sleep(61000),
    
    %timeout shouldn't have occured
    ok = ftp:user(Pid, "mate", ""),
    process_flag(trap_exit, false),
    ftp:close(Pid),
    ct:pal("NULL timeout never occured"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_channel(Port,Server, Config) ->
   Host = ?config(client_host, Config),
   User = ?config(client_user, Config),
   Pass = ?config(client_pass, Config),
   Inet = ?config(ip_protocol, Config),
   ct:log("Start ~p client", [Server]),
   case Port of
           undefined -> 
               Args = [Server, Host, [{user, User}, {password, Pass}, {inet, Inet}, {silently_accept_hosts, true}]];
           _-> 
               Args = [Server, Host, Port, [{user, User}, {password, Pass}, {inet, Inet}, {silently_accept_hosts, true}]]
   end,
   rct_rpc:call(rpc, ftpI, start_channel, Args, 10000).

start_client(Config) ->
    ClientHost = ?config(client_host, Config),
    ClientPort = ?config(client_port, Config),
    ClientUser = ?config(client_user, Config),
    ClientPass = ?config(client_pass, Config),
    
    rct_rpc:call(rpc, ftpesI, start_client, [ClientHost, ClientPort, {ClientUser, ClientPass}], 10000).

start_client_unsuccessful(Config) ->
    ClientHost = ?config(client_host, Config),
    ClientPort = ?config(client_port, Config),
    ClientUser = ?config(client_user, Config),
    ClientPass = ?config(client_pass, Config),
    
    ct:log("Trying to start client with wrong host"),
    {error, ehost} = rct_rpc:call(rpc, ftpesI, start_client, ["10.68.101.0", ClientPort, {ClientUser, ClientPass}], 10000),

    ct:log("Trying to start client with wrong port"),
    {badrpc, timeout} = rct_rpc:call(rpc, ftpesI, start_client, [ClientHost, 22, {ClientUser, ClientPass}], 10000),

    ct:log("Trying to start client with wrong username"),
    {error, euser} = rct_rpc:call(rpc, ftpesI, start_client, [ClientHost, ClientPort, {"wronguser", ClientPass}], 10000),

    ct:log("Trying to start client with wrong password"),
    {error, euser} = rct_rpc:call(rpc, ftpesI, start_client, [ClientHost, ClientPort, {ClientUser, "wrongpass"}], 10000),

    Config.

stop_client(Config) ->
    Pid = ?config(client_pid, Config),
    ok =rct_rpc:call(rpc, ftpesI, stop_client, [Pid], 10000).

stop_channel(Server, Config) ->
    Pid = ?config(client_pid, Config),
    case rct_rpc:call(rpc, ftpI, stop_channel, [Server, Pid], 10000) of
        ok ->  ct:log("Stop ~p client", [Server]), ok;
        _-> 
            ct:log("Unsuccessful stopping ~p client", [Server]), nok
    end.

open_file_write_mode(Config) -> 
     File = ?config(file_name, Config),
     Server = ?config(server, Config),    
     Pid = ?config(client_pid, Config),
     FilePath = ?config(file_path, Config),
     ct:log("Open file for writing"),
     {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [write]], 10000),
     [{file_handle, Handle} | Config].
     
open_file_read_mode(Config) ->  
     File = ?config(file_name, Config),
     Server = ?config(server, Config), 
     Pid = ?config(client_pid, Config),
     FilePath = ?config(file_path, Config),
     ct:log("Open file for reading"),
     {ok, Handle} = rct_rpc:call(rpc, ftpI, open, [Server, Pid, FilePath ++ File, [read]], 10000),
     [{file_handle, Handle} | Config].    

rpc_delete_folder(Pid, Config) ->
    Folder = ?config(folder, Config),
    case rct_rpc:call(rpc, ftpesI, del_dir, [Pid,  Folder], 10000) of
        ok ->
            ct:pal("Folder exists; delete folder");
        _-> 
            ct:pal("Folder doesn't exist")
    end.

kill_app(RpcHandler, AppName) ->
    Apps = rct_rpc:call(RpcHandler, appmServer, get_apps, [], 10000),
    {AppName, Pid} = lists:keyfind(AppName, 1, Apps),
    rct_rpc:call(RpcHandler, os, cmd, ["kill -9 " ++ Pid], 10000),
    Pid.

create_wrong_address({A,B,_,D}) ->
    inet:ntoa({A,B,200,D});
create_wrong_address({A,B,C,_,E,F,G,H}) ->
    inet:ntoa({A,B,C,200,E,F,G,H}).

create_new_valid_address({A,B,C,_}) ->
    inet:ntoa({A,B,C,4});
create_new_valid_address({A,B,C,D,E,F,G,_}) ->
    inet:ntoa({A,B,C,D,E,F,G,4}).

get_address_prefix(IpProto) when ?INET(IpProto) ->
    "/24";
get_address_prefix(IpProto) when ?INET6(IpProto) ->
    "/64".
set_oam_ref(IpProto) ->
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(oam, IpProto), oam).
    
set_alt_oam_ref(IpProto) ->
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(alt_oam, IpProto), alt_oam).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% move to testLib


list_folders(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    Handler = ?config(socket,Config),
    DataDir = ?config(data_dir, Config),
    Type = ?config(type, Config),
    Certificate = ?config(certificate, Config),
    
    ftpes_test_lib:ftp_ls(Handler, "", #ftpDataConnection{mode=Type, host=Host, port=Port, data_dir = DataDir, certificate = Certificate}).

%% Count number of occurances of Pattern in SecurityLog.x
get_security_log_count(Pattern) ->
    Cmd = "cat " ++ ?RCS_DIR ++ "/log/SecurityLog/SecurityLog.* | grep -i -E \'" ++ Pattern ++ "\' -c",
    Res = ftpes_test_lib:exec_command(ssh, Cmd),
    NumberOfLines = string:substr(Res, 1, string:len(Res) - 1),

    list_to_integer(NumberOfLines).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_control_connection(Config) ->
    ftpes_test_lib:flush_messages(),
    Connection_mode = ?config(connection_mode,Config),
    IpAddress = ?config(host, Config),
    Args = ?config(args, Config),
    NewArgs = case IpAddress of
                 {_,_,_,_,_,_,_,_} -> [{ipfamily, inet6} | Args];
                                _  -> Args
              end,
    case ftpes_test_lib:open_ftp_control_connection(Connection_mode, IpAddress, NewArgs) of
        {ok, Pid} -> 
             ok = ftpes_test_lib:ftp_user(Pid, "dustest", ""),
             ok = ftpes_test_lib:ftp_cd(Pid, ?FTPES_TEST_DIR),
             {ok, [{socket,Pid}|Config]};
        {error, Reason} -> ct:log("Failed to set connection"),
                           {error, Reason};
        Other -> {error, Other}
    end.

check_log_count(_, _, 0, Version) when Version =:=  bucket_not_empty ->
    ok;
check_log_count(_, _, 0, _) ->
    ct:fail("Log haven't updated in 10 seconds!");
check_log_count(NoOfOccurancesBefore, Pattern, Number, Version) when Version =:= sec_log ->
    timer:sleep(1000),
    NoOfOccurancesAfter = get_security_log_count(Pattern),
    case (NoOfOccurancesAfter - NoOfOccurancesBefore) of
        1 -> ok;
        0 -> check_log_count(NoOfOccurancesBefore, Pattern, Number -1, Version);
        _ -> ct:fail("Log is not correctly updated!")
    end;
check_log_count(NoOfOccurancesBefore, Pattern, Number, Version) when Version =:= bucket_empty ->
    timer:sleep(1000),
    NoOfOccurancesAfter = get_security_log_count(Pattern),
    case (NoOfOccurancesAfter - NoOfOccurancesBefore) of
        0 -> check_log_count(NoOfOccurancesBefore, Pattern, Number -1, Version);
        _ -> ok
    end;
check_log_count(NoOfOccurancesBefore, Pattern, Number, Version) when Version =:= bucket_not_empty ->
    timer:sleep(1000),
    NoOfOccurancesAfter = get_security_log_count(Pattern),
    case (NoOfOccurancesAfter - NoOfOccurancesBefore) of
        0 -> check_log_count(NoOfOccurancesBefore, Pattern, Number -1, Version);
        _ -> ct:fail("Log is not correctly updated!")
    end.

%%% %CCaseFile:	comsa_snmpv3_over_dtls_SUITE.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2017
%%% @version /main/R10A/R11A/R12A/11

%%% @doc ==
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
%%% R10A/0     2017-07-05 estifil     First version
%%% R10A/3     2017-07-14 enekdav     Added snmp_config_group and snmp_update_group
%%% R10A/4     2017-07-14 evadumb     Added create_delete_directory_and_cert_group
%%% R10A/6     2017-07-20 eivmiha     Added com_snmp_event_handler_group
%%% R10A/8     2017-07-21 enekdav     Finished snmp_config_group and snmp_update_group
%%% R11A/2     2017-08-01 evadumb     Fixed test cases for new design
%%% R11A/5     2017-08-02 ekurnik     Refactored TCs + robust part 1
%%% R11A/6     2017-08-04 ekurnik     Added clear_certm in pre/post suite
%%% R11A/7     2017-08-04 estjako     Added TC-US1-IIIc test and config_2_group 
%%% R11A/8     2017-08-10 edartop     Added agent_address_dtls_group
%%% R11A/9     2017-08-11 evadumb     Fixed snmp_update_group
%%% R11A/10    2017-08-15 estjako     Added tests for dtlsUser
%%% R11A/11    2017-08-16 ekurnik     Added dtls enabled check
%%% R11A/12    2017-08-18 ekurnik     Test changes for DER fingerprint 
%%% R11A/13    2017-08-18 edartop     Added TC-US1-1c, 1d, 1e tests
%%% R11A/14    2017-08-18 ekurnik     Minor fix
%%% R11A/15    2017-08-21 eivmiha     Fixed tests for dtlsUser
%%% R11A/16    2017-08-21 estjako     Small fix
%%% R11A/17    2017-08-21 enatdok     Added TC-US1l-1n tests
%%% R11A/18    2017-08-22 edartop     Added TC-US1o test
%%% R11A/20    2017-08-22 ekurnik     Created sim_group + .crt fix
%%% R11A/21    2017-08-23 eivmiha     Added TC-US1-2 tests
%%% R11A/22    2017-08-23 edartop     Added SIM check for TC-US1o
%%% R11A/23    2017-08-24 ekurnik     Minor test fixes due to design changes
%%% R11A/24    2017-08-25 estjako     Changed TC-US1-3 tests
%%% R11A/26    2017-08-25 emirbos     small fix
%%% R11A/28    2017-08-30 estjako     Changed check_log
%%% R11A/29    2017-08-31 estjako     Changes in init_per_suite and TC-US1-4
%%% R11A/30    2017-08-31 estjako     Finished TC-US1-4
%%% R11A/31    2017-08-31 eivmiha     Added TC-US2, fixed issues in TC-US1-2 tests
%%% R11A/32    2017-09-01 evadumb     Fixed update_snmp_config_same_nc_and_tc test
%%% R11A/33    2017-09-04 eivmiha     Added check_config_data_after_node_restart TC
%%% R11A/34    2017-09-06 estjako     Changes on TC-US1-3
%%% R11A/36    2017-09-07 enekdav     Changes on TC-US1-2c
%%% R11A/37    2017-09-08 enekdav     Added snmp_update_group to all_group
%%% R11A/38    2017-09-25 ekurnik     Trap receiver IP address changed
%%% R11A/39    2017-09-25 eivmiha     Fixed update_snmpTargetV3Dtls_new_various_parameters test
%%% R11A/40    2017-09-26 eivmiha     Added timer:sleep() to subscribe_unsubscribe_nc test
%%% R11A/41    2017-09-28 eivmiha     Removed update_snmpTargetV3Dtls_new_various_parameters
%%% R11A/42    2017-10-17 eivmiha     Added autointegration_group
%%% R11A/43    2017-10-17 eivmiha     Fixed hello_message
%%% R11A/44    2017-10-18 estjako     Small fix in if
%%% R11A/45    2017-10-23 evadumb     Removed old implementation
%%% R12A/1     2017-10-25 eivmiha     Added fault_management_v3_group and fault_management_group
%%% R12A/2     2017-10-31 estjako     Added tests for US6
%%% R12A/3     2017-11-02 enekdav     Added agent_sends_alert and alarm_actions_inform_manager test cases
%%% R12A/4     2017-11-02 evadumb     Added set_heartbeat_interval test
%%% R12A/5     2017-11-07 enekdav     Fixed agent_sends_alert and alarm_actions_inform_manager test cases on SIM
%%% R12A/8     2017-11-09 enekdav     Fixed send_alarm_list_rebuilt
%%% R12A/9     2017-11-16 evadumb     Fixed heartbeat tests, added snmp_get, snmp_set
%%% R12A/10    2017-11-17 edartop     Added TC-US5-4a
%%% R12A/11    2017-11-23 enekdav     Fixed hello message

-module(comsa_snmpv3_over_dtls_SUITE).
-id('Updated by CCase').
-vsn('/main/R10A/R11A/R12A/11').
-date('2017-11-23').
-author('enekdav').

-export([
         config_snmp_with_tls/1,
         config_snmp_with_tls_disabled_tc/1,
         config_snmp_with_tls_uninstalled_nc/1,
         config_snmp_with_tls_without_agentAddressDtls/1,
         config_snmp_with_tls_without_tc/1,
         config_snmp_with_tls_without_nc/1,
         lock_unlock_snmp_mo/1
        ]).

-export([
         update_snmp_config_after_new_nc_install/1,
         update_snmp_config_after_tc_updated/1,
         update_snmp_config_after_nc_deleted/1,
         update_snmp_config_after_tc_deleted/1,
         update_snmp_config_after_nc_and_tc_deleted/1,
         update_snmp_config_same_nc_and_tc/1,
         update_snmp_config_with_agentAddressDtls/1,
         update_snmp_config_with_multiple_agentAddressDtls/1,
         update_snmp_config_after_agentAddressDtls_is_deleted/1
        ]).

-export([
		configure_snmpTargetV3Dtls/1,
		configure_multiple_snmpTargetV3Dtls_targets/1,
		delete_snmpTargetV3Dtls/1,
		lock_unlock_snmpTargetV3Dtls/1,
		update_snmpTargetV3Dtls_new_user/1,
		update_snmpTargetV3Dtls_new_isMibWritable/1,
		update_snmpTargetV3Dtls_new_various_parameters/1
        ]).

-export([
         suite/0,
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
         send_trap_to_manager_with_trusted_certificate/1,
         send_inform_to_manager_with_trusted_certificate/1,
         send_trap_inform_to_manager_with_non_trusted_certificate/1,
         send_snmp_command_from_manager_to_node/1,
         send_snmp_command_from_manager_to_node_no_tls_params/1,
         send_snmp_command_from_manager_to_node_no_agentAddressDtls/1
        ]).


-export([
         configure_tls_cipher_suites/1,
         configure_unsupported_tls_cipher_suites/1,
		 check_config_data_after_node_restart/1
        ]).

-export([
         hello_message/1,
         hello_message_dtls/1,
         hello_message_non_trusted_certificate/1
        ]).

-export([
         set_heartbeat_interval/1,
         receive_heartbeat/1,
         alarm_actions_inform_manager/1,
         agent_sends_alert/1,
         send_alarm_list_rebuilt/1,
         get_active_alarm_list/1
         ]).

-export([changed_ipAddr_trap_restart/1,
		 changed_ipAddr_trap_ack/1,
         changed_ipAddr_trap_no_ack/1]).

-record(trapVariable, {oid, %% variable OID
					   type, %% variable type
					   value %% variable value
					  }
		).

-record(trapLogMessage, {date_time, 	%% datetime() type: {{Year, Month, Day}, {Hour, Minute, Second}}
						 hostname,      %% e.g "localhost" 
						 protocol,  	%% udp or dtlsudp
						 from,  		%% {ip_address, port}
						 to,   			%% {ip_address, port}
						 content = []   %% List of different trap variables
						}
		).

-define(SNMP_CONFIG,   "etc/snmpd.conf").
-define(COMEA,   "bin/comea").
-define(SNMP_TARGET_V3_ID, "10.67.31.150").
-define(SNMP_TARGET_V3_PORT, 10162).
-define(SNMPD_CONF_PATH, rct_rpc:call(rpc, sysEnv, releases_vsn_dir, [], 10000) ++ ?SNMP_CONF).
-define(DEFAULT_AGENT_ADDRESS_PORT, 6161).
-define(DEFAULT_AGENT_ADDRESS_DTLS_PORT, 10161).
-define(TLS_DN, "SystemFunctions=1,SecM=1,Tls=1").
-define(FM_DN, "SystemFunctions=1,Fm=1").
-define(HELLO, "SNMPv2-SMI::enterprises.193.183.5.4.0.1").
-define(TRAP, "SNMPv2-MIB::coldStart").
-define(HEARTBEAT, ".1.3.6.1.4.1.193.183.4.1.5.1.0").
-define(HBOid, "SNMPv2-SMI::enterprises.193.183.4.2.0.20").
-define(SnmpEngineBoots, "1.3.6.1.6.3.10.2.1.2.0").

-define(V3USER, "testTwo").
-define(V3PASSWORD, "OvoNijeOsamZnakova").
-define(ALARM_LIST_REBUILD, "193.183.4.2.0.30").
-define(ALARM_ALERT_TABLE_URL, ".1.3.6.1.4.1.193.183.4.1.3.4.0").

-define(SNMP_TARGET_V3_DTLS(TestCase), TestCase =:= configure_snmpTargetV3Dtls orelse
		                               TestCase =:= lock_unlock_snmpTargetV3Dtls orelse
		                               TestCase =:= update_snmpTargetV3Dtls_new_user orelse
		                               TestCase =:= update_snmpTargetV3Dtls_new_isMibWritable orelse
		                               TestCase =:= update_snmpTargetV3Dtls_new_various_parameters).

%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].

%% @hidden
-spec suite() -> config().

-define(NC_REGEX, "localCert .*").
-define(TRAPSSES_REGEX, "trapsess .*").
-define(SNMP_CONF, "/comte/comea/etc/snmpd.conf").
-define(SNMP_DN, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1").
-define(SNMP_TARGET_DN(TargetType, Id), ?SNMP_DN ++ "," ++ type_to_mo(TargetType) ++ "=" ++ Id).
-define(SNMP_GET_RESPONSE, "SNMP-FRAMEWORK-MIB::snmpEngineBoots.0 = INTEGER: 1\n").
-define(SNMP_GET_ALARM_LIST_RESPONSE, "SNMPv2-SMI::enterprises.193.183.4.1.3.4.0 = \"\"\n").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("$RCT_TOP/test/lib/lib/esrc/ftpes_test_lib.hrl").

-define(CLI, c1).
-define(TNODE, rpc).
-define(RPC_CALL_TIMEOUT, 10000).

%% -record(snmpTargetV3Dtls, {snmpTargetV3DtlsId,
%%                            informRetryCount,
%%                            administrativeState,
%%                            address,
%%                            port,
%%                            informTimeout,
%%                            transportMethod,
%%                            isMibWritable,
%%                            operationalState,
%%                            user}).

-define(NC_USER, case ftpes_test_lib:is_target() of
                        true ->
                            [{1, nc1, oam_auto}, {1, nc2, ssh_lmt_ipv4}];
                        false ->
                            [nc1]
                    end).
%% -define(NC_USER, nc1).

suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks,
      [{rct_htmllink,[]},
       {rct_rpc, rpc},
       {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"], ["*Unexpected COM*"]}}]}},
       {rct_netconf, ?NC_USER},
       {cth_conn_log, []},
       {rct_core,[]},
%%        {rct_cli, {?CLI,[manual_connect]}},
       {rct_cli, ?CLI},
       {cover_hook,[{du1, username}]},
       {rct_ssh,{ssh,[manual_connect]}}
      ]}
    ].

%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    Handler = {netconf, nc1},
    %% Cleanup CertM just in case
    rct_certm_lib:clear_certm_config(Handler),
    Path = rct_rpc:call(rpc, sysEnv, releases_vsn_dir, [], 10000) ++ ?SNMP_CONF,
    NodeCredName = "ncred_1",
    TrustCatName = "tcat_1",
    NodeCredFingerprint = "e4:7c:12:92:a2:1f:48:bb:43:75:1a:c9:eb:a2:ce:95:7a:1a:89:35",
    TrustCatFingerprint = "FA:7E:10:69:8C:11:4D:3B:50:AE:F3:C1:53:F7:85:53:46:57:BC:40",
    CertPassword = "nodeagent",
    UriPassword = "labuser",
    CertUri = "sftp://labuser@10.68.101.131/home/labuser/snmp_dtls_certificates/nodeagent.p12",
    TrustCatUri = "sftp://labuser@10.68.101.131/home/labuser/snmp_dtls_certificates/mngr.example.com.crt",
    
    User = "root",
    Pass = "rootroot",
    Cookie = erlang:get_cookie(),
    RemoteName = "steph",
    RemoteIp = "10.67.31.150",
    RemotePort =10162,
    RemoteConnectPort = 22, %ssh port
    TheirHostname = "'10.67.31.150'",
    Node = RemoteName ++"@" ++ "rootroot",
    
    Name = "traptest",
    Type1 = "rw",
    Type2 = "ro",
    Protocol = "dtlsudp",
    TransportType = 2,
    TransportTypeTrap = 1,
    Retries = 1,
    Timeout = 3,
	IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    [{ncName, NodeCredName}, {tcName, TrustCatName}, {ncFingerprint, NodeCredFingerprint},
     {tcFingerprint, TrustCatFingerprint}, {ncPassword, CertPassword}, {uriPassword, UriPassword}, {ncUri, CertUri}, 
     {tcUri, TrustCatUri}, {handler, Handler}, {path, Path}, 
     {user, User}, {pass, Pass}, {cookie, Cookie},{remote_name, RemoteName}, {node, Node}, 
     {remote_ip, RemoteIp}, {remote_port, RemotePort}, {remote_connect_port, RemoteConnectPort}, 
     {their_hostname, TheirHostname}, {name, Name}, {type1, Type1}, {type2, Type2}, {protocol, Protocol},
     {trans_type, TransportType}, {trans_type_trap, TransportTypeTrap}, {retries, Retries}, {timeout, Timeout},
	  {ip_protocol, IpProtocol} | Config].


%% @hidden
-spec end_per_suite(config()) -> any().

end_per_suite(Config) ->
    %% Cleanup CertM just in case
    rct_certm_lib:clear_certm_config(?config(handler, Config)),
    ok.


%% @hidden
-spec init_per_group(atom(), config()) -> config().

init_per_group(snmp_update_group, Config) ->
    installNC(Config),
    installTC(Config),
    Config = set_snmp_config(Config),
    ok = set_agent_address(?DEFAULT_AGENT_ADDRESS_PORT, Config),
    ok = set_agent_address_dtls(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, Config),
    timer:sleep(5000),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_PORT, true),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    {ok, InstalledTCs} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), ?config(tcName, Config)),
    TrustedCertIds = lists:reverse(InstalledTCs),
    [{trusted_certificate, TrustedCertIds} | Config];

init_per_group(GroupName, Config) when  GroupName =:= config_2_group->
    installNC(Config),
    Config1 = installTC(Config),
    Config2 = set_snmp_config(Config1),
    ok = set_agent_address(?DEFAULT_AGENT_ADDRESS_PORT, Config2),
    ok = set_agent_address_dtls(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, Config2),
    timer:sleep(5000),
    
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_PORT, true),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),

    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config2)),
    ConRef = start_connection_to_node( ?config(user, Config2),
                                       ?config(pass, Config2), 
                                       ?config(remote_ip, Config2),
                                       ?config(remote_connect_port, Config2)),
    [{connection_reference, ConRef} | Config2];

init_per_group(configuration_of_tls_cipher_suites_group, Config) ->
	installNC(Config),
    installTC(Config),
    Config = set_snmp_config(Config),
    ok = set_agent_address(?DEFAULT_AGENT_ADDRESS_PORT, Config),
    ok = set_agent_address_dtls(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, Config),
    timer:sleep(5000),

	check_snmp_running(?DEFAULT_AGENT_ADDRESS_PORT, true),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
	
	V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, "labuser", undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
	timer:sleep(5000),
	{ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
	
    {ok, InstalledTCs} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), ?config(tcName, Config)),
    TrustedCertIds = lists:reverse(InstalledTCs),
    [{trusted_certificate, TrustedCertIds} | Config];

init_per_group(fault_management_group, Config) ->
    installNC(Config),
    Config1 = installTC(Config),
    Config2 = set_snmp_config(Config1),
    ok = set_agent_address(6161, Config2),
    ok = set_agent_address_dtls(10161, Config2),

    V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, ?SNMP_TARGET_V3_ID, undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
    timer:sleep(6000),
    
    ok = rct_mo_handler_lib:set_mo(?config(handler, Config2), ?SNMP_TARGET_DN(v3dtls, "1"), 
                                [{informRetryCount, integer_to_list(?config(retries, Config2))},
                                 {informTimeout, integer_to_list(?config(timeout, Config2))},
                                 {isMibWritable, atom_to_list(true)},
                                 {transportMethod, "INFORM"}
                                 ]),
    
    common_init_per_group(v3dtls, Config2);

init_per_group(fault_management_v3_group, Config) ->
    V3Config = rct_snmp_lib:create_snmp_target_configuration(v3, "10.67.31.150", 162, ?V3USER, {?V3PASSWORD, ?V3PASSWORD}),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3, "1", [{transportMethod, "INFORM"} | V3Config]),
    timer:sleep(6000),

    common_init_per_group(v3, Config);

init_per_group(aic_ip_change_v3_trap_group, Config) -> 
    IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    NewConfig = case IpProtocol of
		inet ->
	        installNC(Config),
            installTC(Config),
            Config;
		inet6 ->
	       SftpUser = ftpes_test_lib:get_ftpes_test_server_username(),
           SftpAddress = ftpes_test_lib:get_ftpes_test_server_address(IpProtocol),
           UriNc = "sftp://" ++ SftpUser ++ "@" ++ "["++ SftpAddress ++"]" ++ "/home/labuser/snmp_dtls_certificates/nodeagent.p12",
		   UriTc = "sftp://" ++ SftpUser ++ "@" ++ "["++ SftpAddress ++"]" ++ "/home/labuser/snmp_dtls_certificates/mngr.example.com.crt",
		   installNC([{ncUri, UriNc}| Config]),
           installTC([{tcUri, UriTc}| Config]),
		   [{ncUri, UriNc}, {tcUri, UriTc}| Config]
	end,

    Config1 = set_snmp_config(NewConfig),
    ok = set_agent_address(?DEFAULT_AGENT_ADDRESS_PORT, Config1),
    timer:sleep(5000),

    V3Config = rct_snmp_lib:create_snmp_target_configuration(v3, "10.67.31.150", 162, "testTwo", {"OvoNijeOsamZnakova", "OvoNijeOsamZnakova"}),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3, "1", [{transportMethod, "INFORM"} | V3Config]),
	
	ConRef = start_connection_to_node( ?config(user, Config1),
                                       ?config(pass, Config1), 
                                       ?config(remote_ip, Config1),
                                       ?config(remote_connect_port, Config1)),

	Config2 = [{ip_protocol, IpProtocol}, {connection_reference, ConRef} | Config1],
    case ftpes_test_lib:oam_exists(IpProtocol) of 
        nok -> {skip, "No OAM is configured"};
        _-> 
            lists:keyreplace(ip_protocol, 1, Config2, {ip_protocol, IpProtocol})
    end;
	

init_per_group(aic_ip_change_trap_group, Config) -> 
	
    Config = set_snmp_config(Config),
    ok = set_agent_address(?DEFAULT_AGENT_ADDRESS_PORT, Config),
    ok = set_agent_address_dtls(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, Config),
    timer:sleep(5000),
    
    check_snmp_dtls_enabled(true),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_PORT, true),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
	V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, ?SNMP_TARGET_V3_ID, undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
	
    IpProtocol = ftpes_test_lib:get_oam_ip_protocol(),
    case ftpes_test_lib:oam_exists(IpProtocol) of 
        nok -> {skip, "No OAM is configured"};
        _-> 
            lists:keyreplace(ip_protocol, 1, Config, {ip_protocol, IpProtocol})
    end;

init_per_group(_GroupName, Config) ->
    Config.

common_init_per_group(Dtls, Config) ->
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    {ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), Dtls, "1"),
    Dn = rct_snmp_lib:get_snmp_target_dn(Dtls, "1"),
    {ok,[{isMibWritable,"true"}]} = rct_mo_handler_lib:get_mo(?config(handler, Config), Dn, [isMibWritable]),

    ConRef = start_connection_to_node(Config),
    timer:sleep(6000),
    IpProtocol = ?config(ip_protocol, Config),
    NodeIp = case is_target() of
                true -> inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, lmt, IpProtocol));
                false -> inet_parse:ntoa(ftpes_test_lib:get_client_ip(?config(handler, Config), ipv4))
             end,
    timer:sleep(10000),

    [{connection_reference, ConRef}, {nodeIp, NodeIp}, {dtls, Dtls} | Config].


%% @hidden
-spec end_per_group(atom(), config()) -> any().

end_per_group(GroupName, Config) when GroupName =:= config_2_group orelse
                                      GroupName =:= snmp_update_group ->
    Config = unset_snmp_config(Config),
    Config = removeNC(Config),
    Config = removeTC(Config),
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, []),
    {ok, [{agentAddressDtls, []}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
    ok;

end_per_group(configuration_of_tls_cipher_suites_group, Config) ->
    Config = unset_snmp_config(Config),
    Config = removeNC(Config),
    Config = removeTC(Config),
    
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, []),
    {ok, [{agentAddressDtls, []}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
	ok = rct_snmp_lib:delete_snmp_target(?config(handler, Config), v3dtls, "1");

end_per_group(fault_management_group, Config) ->   
    Config = unset_snmp_config(Config),
    Config = removeNC(Config),
    Config = removeTC(Config),
    
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, []),
    {ok, [{agentAddressDtls, []}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
    ok = rct_snmp_lib:delete_snmp_target(?config(handler, Config), v3dtls, "1");

end_per_group(fault_management_v3_group, Config) ->
    ok = rct_snmp_lib:delete_snmp_target(?config(handler, Config), v3, "1");

end_per_group(aic_ip_change_trap_group, Config) ->
	Config = unset_snmp_config(Config),
    Config = removeNC(Config),
    Config = removeTC(Config),
    
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, []),
    {ok, [{agentAddressDtls, []}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
	ok = rct_snmp_lib:delete_snmp_target(?config(handler, Config), v3dtls, "1"),
	
	ct:log("Set OAM references back, just for security"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol),

    ok;

end_per_group(aic_ip_change_v3_trap_group, Config) ->
    timer:sleep(5000),
    ok = rct_snmp_lib:delete_snmp_target(?config(handler, Config), v3, "1"),
	ct:log("Set OAM references back, just for security"),
    IpProtocol = ?config(ip_protocol, Config),
    set_oam_ref(IpProtocol),
    set_alt_oam_ref(IpProtocol);

end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
-spec init_per_testcase(atom(), config()) -> config().

init_per_testcase(TestCase, Config) when ?SNMP_TARGET_V3_DTLS(TestCase) ->
	V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, "labuser", undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
	Config;

init_per_testcase(configure_multiple_snmpTargetV3Dtls_targets, Config) ->
	V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, "labuser", undefined),
	V3DtlsConfig2 = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, "labuser2", undefined),
	V3DtlsConfig3 = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, "labuser3", undefined),
	
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "2", V3DtlsConfig2),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "3", V3DtlsConfig3),
	Config;

init_per_testcase(hello_message, Config) ->
    V3Config = rct_snmp_lib:create_snmp_target_configuration(v3, "10.67.31.150", 162, "testTwo", {"OvoNijeOsamZnakova", "OvoNijeOsamZnakova"}),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3, "1", [{transportMethod, "INFORM"} | V3Config]),
    ConRef = start_connection_to_node( ?config(user, Config),
                               ?config(pass, Config), 
                               ?config(remote_ip, Config),
                               ?config(remote_connect_port, Config)),
    timer:sleep(5000),
    [{connection_reference, ConRef} | Config];

init_per_testcase(TestCase, Config) when TestCase =:= hello_message_dtls
                                    orelse TestCase =:= hello_message_non_trusted_certificate ->
    installNC(Config),
    Config1 = installTC(Config),
    Config2 = set_snmp_config(Config1),
    ok = set_agent_address(6161, Config2),
    ok = set_agent_address_dtls(10161, Config2),
    timer:sleep(5000),
    
    %check_snmp_dtls_enabled(true),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config2)),
    ConRef = start_connection_to_node( ?config(user, Config2),
                                       ?config(pass, Config2), 
                                       ?config(remote_ip, Config2),
                                       ?config(remote_connect_port, Config2)),
    
    case TestCase of
        hello_message_non_trusted_certificate ->
            TcId = ?config(trust_cert_id, Config),
            {ok, _CertsBefore} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), "tcat_1"),
            ok = rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), TcId, disabled),
            timer:sleep(20000);
        _ -> ok
    end,

    V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, ?SNMP_TARGET_V3_ID, undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
    ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?SNMP_TARGET_DN(v3dtls, "1"), 
                                [{informRetryCount, integer_to_list(?config(retries, Config))},
                                 {informTimeout, integer_to_list(?config(timeout, Config))},
                                 {isMibWritable, atom_to_list(true)},
                                 {transportMethod, "INFORM"}
                                 ]),
    
    [{connection_reference, ConRef} | Config2];

init_per_testcase(TestCase, Config) when TestCase =:= configure_unsupported_tls_cipher_suites 
  									orelse TestCase =:= configure_tls_cipher_suites->
    ConRef = start_connection_to_node( ?config(user, Config),
                                       ?config(pass, Config), 
                                       ?config(remote_ip, Config),
                                       ?config(remote_connect_port, Config)),
	[{connection_reference, ConRef} | Config];

init_per_testcase(TestCase, Config) when TestCase =:= changed_ipAddr_trap_restart 
                                    orelse TestCase =:= changed_ipAddr_trap_ack
                                    orelse TestCase =:= changed_ipAddr_trap_no_ack->
	ct:log("Removing Alt OAM reference, to be used to switch OAM"),
    ok = ftpes_test_lib:delete_oam_ref(alt_oam),
	Config;

init_per_testcase(send_alarm_list_rebuilt, Config) ->
    reset_restart_list(),
    Config;

init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
-spec end_per_testcase(atom(), config()) -> any().

end_per_testcase(config_snmp_with_tls_disabled_tc, Config)->
    {ok, TrustedCertIds} =  rct_certm_lib:get_all_trusted_certificates(?config(handler, Config)),
    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), Tc, enabled) || Tc <- TrustedCertIds],
    timer:sleep(5000),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= update_snmp_config_after_new_nc_install orelse
                                        TestCase =:= config_snmp_with_tls_uninstalled_nc ->
    NCDn = rct_certm_lib:get_node_credential_dn(?config(ncName, Config)),
    ok = rct_snmp_lib:set_snmp_node_credential(?config(handler, Config), NCDn),
    ok = rct_certm_lib:delete_node_credential(?config(handler, Config), "ncred_2"),
    timer:sleep(5000),
    ok;

end_per_testcase(TestCase, Config) when ?SNMP_TARGET_V3_DTLS(TestCase) ->
	delete_snmptargetv3dtls_mo(?config(handler, Config), ["1"]);

end_per_testcase(configure_multiple_snmpTargetV3Dtls_targets, Config) ->
	delete_snmptargetv3dtls_mo(?config(handler, Config), ["1", "2", "3"]);

end_per_testcase(TestCase, Config) when TestCase =:= send_trap_to_manager_with_trusted_certificate orelse
                                        TestCase =:= send_inform_to_manager_with_trusted_certificate orelse
                                        TestCase =:= send_trap_inform_to_manager_with_non_trusted_certificate ->
    ct:log("Delete dtls user"),
    delete_snmptargetv3dtls_mo(?config(handler, Config), ["1"]),
    ok;

end_per_testcase(hello_message, Config) ->   
    ok = rct_snmp_lib:delete_snmp_target(?config(handler, Config), v3, "1");

end_per_testcase(TestCase, Config) when TestCase =:= hello_message_dtls
                                   orelse TestCase =:= hello_message_non_trusted_certificate->
    if TestCase =:= hello_message_non_trusted_certificate ->
        ok = rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), ?config(trust_cert_id, Config), enabled);
	true -> ok
    end,
    
    Config = unset_snmp_config(Config),
    Config = removeNC(Config),
    Config = removeTC(Config),
    
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, []),
    {ok, [{agentAddressDtls, []}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
    ok = rct_snmp_lib:delete_snmp_target(?config(handler, Config), v3dtls, "1");

end_per_testcase(TestCase, Config) when TestCase =:= set_heartbeat_interval orelse
                                        TestCase =:= receive_heartbeat ->
    "SNMPv2-SMI::enterprises.193.183.4.1.5.1.0 = Gauge32: 60\n" =
        snmp_set(v3, [?V3USER, "MD5", "DES", ?V3PASSWORD, ?V3PASSWORD, "authPriv", ?config(nodeIp, Config), "6161",
                       ?HEARTBEAT, "u", "60"], ?config(connection_reference, Config)),
    timer:sleep(10000),
    "SNMPv2-SMI::enterprises.193.183.4.1.5.1.0 = Gauge32: 60\n" =
        snmp_get(v3, [?V3USER, "MD5", "DES", ?V3PASSWORD, ?V3PASSWORD, "authPriv", ?config(nodeIp, Config), "6161",
                                       ?HEARTBEAT], ?config(connection_reference, Config)),
    {ok,[{heartbeatInterval, "60"}]}  = rct_mo_handler_lib:get_mo(?config(handler, Config), ?FM_DN, [heartbeatInterval]),
    ok;

end_per_testcase(send_alarm_list_rebuilt, _Config) ->
    reset_restart_list();

end_per_testcase(TestCase, Config) when TestCase =:= changed_ipAddr_trap_restart 
                                    orelse TestCase =:= changed_ipAddr_trap_ack
                                    orelse TestCase =:= changed_ipAddr_trap_no_ack->
	IpProtocol = ?config(ip_protocol, Config),
    ct:log("Set alt oam reference back"),
    set_alt_oam_ref(IpProtocol),
	ok;

end_per_testcase(_TestCase, _Config) ->
    ok.


groups() ->
    AllGroup = all(),
    SNMPUpdateGroup = snmp_update_group(),
    Config2Group = config_2_group(),
	ConfigurationOfTlsCipherSuites = configuration_of_tls_cipher_suites_group(),
    AutointegrationGroup = autointegration_group(),
    AutointegrationDtlsGroup = autointegration_dtls_group(),
    FaultManagementGroup = fault_management_group(),
	AicIpChangeTrapGroup = aic_ip_change_trap_group(),
    [
     {default__group, [], AllGroup},
     {snmp_update_group, [], SNMPUpdateGroup},
     {config_2_group, [], Config2Group},
	 {configuration_of_tls_cipher_suites_group, [], ConfigurationOfTlsCipherSuites},
     {autointegration_group, [], AutointegrationGroup},
     {autointegration_dtls_group, [], AutointegrationDtlsGroup},
     {fault_management_group, [], FaultManagementGroup},
     {fault_management_v3_group, [], FaultManagementGroup},
	 {aic_ip_change_trap_group, [], AicIpChangeTrapGroup},
	 {aic_ip_change_v3_trap_group, [], AicIpChangeTrapGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []},
     {sbc__upgrade__all__1__group, [], []},
     {sdc__cover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},
     {sdc__qual__all__1__group, [], []}
    ].

all() ->
     [
      
      {group, snmp_update_group},
      {group, fault_management_v3_group},
      {group, config_2_group},
      {group, configuration_of_tls_cipher_suites_group}
     ].

snmp_update_group() ->
    [
     config_snmp_with_tls,
     config_snmp_with_tls_disabled_tc,
     config_snmp_with_tls_uninstalled_nc,
     config_snmp_with_tls_without_agentAddressDtls,
     config_snmp_with_tls_without_tc,
     config_snmp_with_tls_without_nc,
     update_snmp_config_after_new_nc_install,
     update_snmp_config_after_tc_updated,
     update_snmp_config_after_nc_deleted,
     update_snmp_config_after_tc_deleted,
     update_snmp_config_after_nc_and_tc_deleted,
     update_snmp_config_same_nc_and_tc,
     %%update_snmp_config_with_agentAddressDtls,    %maybe obsolete
     update_snmp_config_with_multiple_agentAddressDtls,
     update_snmp_config_after_agentAddressDtls_is_deleted,
     lock_unlock_snmp_mo,
     configure_snmpTargetV3Dtls,
     configure_multiple_snmpTargetV3Dtls_targets,
     delete_snmpTargetV3Dtls,
     lock_unlock_snmpTargetV3Dtls,
     update_snmpTargetV3Dtls_new_user,
     update_snmpTargetV3Dtls_new_isMibWritable,
     update_snmpTargetV3Dtls_new_various_parameters
     ].

config_2_group() ->
    [
     send_trap_to_manager_with_trusted_certificate,
     send_inform_to_manager_with_trusted_certificate,
     send_trap_inform_to_manager_with_non_trusted_certificate,
     send_snmp_command_from_manager_to_node,
     send_snmp_command_from_manager_to_node_no_tls_params,
     send_snmp_command_from_manager_to_node_no_agentAddressDtls
    ].

configuration_of_tls_cipher_suites_group() ->
	[
	 configure_tls_cipher_suites,
	 configure_unsupported_tls_cipher_suites%,
	 %% check_config_data_after_node_restart           %%needs update
	].

autointegration_group() ->
    [
     hello_message
    ].

autointegration_dtls_group() ->
    [
     hello_message_dtls,
     hello_message_non_trusted_certificate
    ].

fault_management_group() ->
    [
     set_heartbeat_interval,
     receive_heartbeat,
     alarm_actions_inform_manager,
     agent_sends_alert,
     send_alarm_list_rebuilt,
     get_active_alarm_list
    ].

aic_ip_change_trap_group() ->
	[changed_ipAddr_trap_restart,
     changed_ipAddr_trap_ack,
     changed_ipAddr_trap_no_ack
    ].

%%--------------------------------------------------------------------
%% TEST CASES - configuration of SNMP with TLS parameters
%%--------------------------------------------------------------------

%% TC-US1-1     Configure SNMP with TLS parameters
%% Configure Snmp MO with nodeCredential and trustCategory references. Check that Snmp MO is enabled.
%% Configure SnmpTargetV3Dtls with username which corresponds to peer certificate's subjectAltName
%% and IP address which corresponds to address of MgmtSys. Check that SnmpTargetV3Dtls is enabled.
config_snmp_with_tls(Config) ->
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("Snmp MO successfully configured"),
    ok.


%% TC-US1-1a    Configure SNMP with TLS with disabled TrustedCertificate
%% Configure Snmp MO with nodeCredential and trustCategory references. TrustCategory has disabled
%% TrustedCertificate. Check that Snmp MO is disabled. Check that TrustedCertificate is not written
%% to directory nor written to configuration.
config_snmp_with_tls_disabled_tc(Config) ->
    ct:pal("Setting snmp MO with disabled trusted certificates"),
    {ok, TrustedCertIds} =  rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), "tcat_1"),
    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), Tc, disabled) || Tc <- TrustedCertIds],
    timer:sleep(5000),

    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    ct:pal("Snmp MO with disabled trusted certificates is disabled"),

    ok.

%% TC-US1-1b    Configure SNMP with TLS with uninstalled NodeCredential
%% Configure Snmp MO with nodeCredential and trustCategory references. NodeCredential is not installed.
%% Check that Snmp MO is disabled. Check that NodeCredential is not written to directory nor written to configuration.
config_snmp_with_tls_uninstalled_nc(Config) ->
    ct:pal("Uninstalling node certificate"),
    %new uninstalled nc
    ok = rct_certm_lib:create_node_credential(?config(handler, Config), "ncred_2"),
    NC2Dn = rct_certm_lib:get_node_credential_dn("ncred_2"),
    ok = rct_snmp_lib:set_snmp_node_credential(?config(handler, Config), NC2Dn),
    {ok, NC2Dn} = rct_snmp_lib:get_snmp_node_credential(?config(handler, Config)),
    timer:sleep(5000),
    
    %check that nc1 is uninstalled
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),

    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("DTLS with uninstalled node certificate is disabled"),

    ok.

%% TC-US1-1c    Configure SNMP with TLS without agentAddressDtls
%% Configure Snmp MO with nodeCredential and trustCategory references without agentAddressDtls.
%% Check that Snmp MO is disabled.
config_snmp_with_tls_without_agentAddressDtls(Config) ->
    %% get the initial agentAddressDtls
	{ok, [{agentAddressDtls, [{Host, Port}]}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
    ct:pal("Configure SNMP with TLS without agentAddressDtls",[]),
    
    %% delete agentAddressDtls
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, []),
    timer:sleep(3000),

    %% check if MO is disabled
    check_snmp_running(list_to_integer(Port), false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("DTLS without agentAddressDtls is disabled"),
    
    %% set the initial agentAddressDtls
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, [{Host, Port}]),
    timer:sleep(3000),
    ok.


%% TC-US1-1d    Configure SNMP with TLS without nodeCredential
%% Configure Snmp MO with trustCategory reference and agentAddressDtls without nodeCredential.
%% Check that Snmp MO is disabled.
config_snmp_with_tls_without_nc(Config) ->
    % get the initial nodeCredential
    {ok, NCDn} = rct_snmp_lib:get_snmp_node_credential(?config(handler, Config)),
    ct:pal("Configure SNMP with TLS without nodeCredential",[]),

    % delete nodeCredential
    ok = rct_snmp_lib:set_snmp_node_credential(?config(handler, Config), undefined),
    timer:sleep(3000),
    
    %% check if MO is disabled
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("DTLS without nodeCredential is disabled"),
    
    % set the initial nodeCredential
    ok = rct_snmp_lib:set_snmp_node_credential(?config(handler, Config), NCDn),
    timer:sleep(3000),
    ok.

%% TC-US1-1e    Configure SNMP with TLS without trustCategory
%% Configure Snmp MO with nodeCredential reference and agentAddressDtls without trustCategory.
%% Check that Snmp MO is disabled.
config_snmp_with_tls_without_tc(Config) ->
    % get the initial trustCategory
    {ok, TcatDn} = rct_snmp_lib:get_snmp_trust_category(?config(handler, Config)),
    ct:pal("Configure SNMP with TLS without trustCategory",[]),

    % delete trustCategory
    ok = rct_snmp_lib:set_snmp_trust_category(?config(handler, Config), undefined),
    timer:sleep(3000),
    
    %% check if MO is disabled
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("DTLS without trustCategory is disabled"),
    
    % set the initial trustCategory
    ok = rct_snmp_lib:set_snmp_trust_category(?config(handler, Config), TcatDn),
    timer:sleep(3000),
    ok.

%% TC-US1-1f     Update SNMP configuration after new NC install
%% Install new NodeCredential on NC which is set on Snmp MO. Check that snmpd.conf is updated with new NC.
%% Check that node certificate is stored in cert_dir. 
update_snmp_config_after_new_nc_install(Config) ->
    ct:pal("Installing new node credential"),
    ok = rct_certm_lib:create_node_credential(?config(handler, Config), "ncred_2"),
    NC2Dn = rct_certm_lib:get_node_credential_dn("ncred_2"),
    ok = rct_snmp_lib:set_snmp_node_credential(?config(handler, Config), NC2Dn),
    timer:sleep(5000),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    
    ok = rct_certm_lib:install_node_credential_from_uri(?config(handler, Config), "ncred_2",
                                                        "sftp://labuser@10.68.101.131/home/labuser/FTP_TLS_SETUP/tls_nodecert_pkcs12",
                                                        ?config(uriPassword, Config), 
                                                        "idiocy",
                                                        "17:8b:19:ef:57:e1:12:62:67:33:f5:bd:bd:8c:28:8e:bd:4b:c2:ce"),
    {ok, NC2Dn} = rct_snmp_lib:get_snmp_node_credential(?config(handler, Config)),
    timer:sleep(5000),

    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),

    ct:pal("New node certificate successfully installed"),
    ok.


%% TC-US1-1g    Update SNMP configuration after TC is updated
%% Add new trustedCertificate to trustCategory which is set on Snmp MO. Check that snmpd.conf is updated
%% with new TC. Check that trusted certificates are stored in cert_dir. 
update_snmp_config_after_tc_updated(Config) ->
    {ok, CertsBefore} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), "tcat_1"),

    ct:pal("Installing new trusted certificate"),
    {ok, TcId} = rct_certm_lib:install_trusted_certificate_from_uri(?config(handler, Config), 
                                         "sftp://labuser@10.68.101.131/home/labuser/FTP_TLS_SETUP/user-ca.crt", 
                                         "labuser", "DB:59:94:FB:BE:E6:1B:83:D4:77:88:BF:F8:27:9B:B9:BC:A0:5D:23"),
    ok = rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), TcId, enabled),
    ok = rct_certm_lib:set_trust_category_trusted_certs(?config(handler, Config), "tcat_1", [TcId|CertsBefore]),
    timer:sleep(5000),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),

    ct:pal("Successfully installed trusted certificate"),
    %remove it
    ok = rct_certm_lib:set_trust_category_trusted_certs(?config(handler, Config), "tcat_1", CertsBefore),
    ok = rct_certm_lib:delete_trusted_certificate(?config(handler, Config), TcId),

    ok.


%% TC-US1-1h    Update SNMP configuration after NC is deleted
%% Remove previously set nodeCredential from Snmp MO. Check that the existing node certificate is deleted
%% from snmpd.conf. Check that node certificate is deleted from cert_dir.
update_snmp_config_after_nc_deleted(Config) ->
    ct:pal("Removing node credential"),
    Config = unset_snmp_config(Config),
    Config = removeNC(Config),
    timer:sleep(5000),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("Node certificate successfully removed"),

    %recovering node
    installNC(Config),

    Config = set_snmp_config(Config),
    ok.


%% TC-US1-1i    Update SNMP configuration after TC is deleted
%% Remove previously set trustCategory on Snmp MO. Check that the existing trusted certificate is deleted
%% from snmpd.conf. Check that trusted certificate is deleted from cert_dir.
update_snmp_config_after_tc_deleted(Config) ->
    ct:pal("Removing trust category"),
    Config = unset_snmp_config(Config),
    Config = removeTC(Config),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    {error, _} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), ?config(tcName, Config)),
    ct:pal("Successfully removed trust category"),

    %recovering tcat
    Config = removeNC(Config),
    installNC(Config),
    installTC(Config),

    Config = set_snmp_config(Config),
    ok.


%% TC-US1-1j    Update SNMP configuration after NC and TC are deleted
%% Remove previously set nodeCredential and trustCategory on Snmp MO. Check that the existing node certificate
%% and trusted certificate are deleted from snmpd.conf. Check that node certificate and trusted certificate
%% are deleted from cert_dir.
update_snmp_config_after_nc_and_tc_deleted(Config) ->
    ct:pal("Removing node credential and trust category"),
    Config = unset_snmp_config(Config),
    Config = removeNC(Config),
    Config = removeTC(Config),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    {error, _} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), ?config(tcName, Config)),
    ct:pal("Successfully removed node credential and trust category"),

    %recovering node and tcat
    installNC(Config),
    installTC(Config),
    Config = set_snmp_config(Config),

    ok.


%% TC-US1-1k    Update SNMP configuration with same value for NC and TC
%% Update trustCategory and nodeCredential on Snmp MO with the same value. Check that no reconfiguration is done
%% and Snmp MO is enabled.
update_snmp_config_same_nc_and_tc(Config) ->
    ct:pal("Setting node credential and trust category with same values"),
    Config = set_snmp_config(Config),

    ct:pal("No configuration has been done"),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("Administrative state stays enabled"),
    ok.

%% TC-US1-1l    Configure Snmp MO with agentAddressDtls.
%% Check that agentaddress for dtls is changed in snmpd.conf.
update_snmp_config_with_agentAddressDtls(_Config) ->
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    ok.


%% TC-US1-1m    Configure Snmp MO with multiple agentAddressDtls.
%% Check that all agentaddresses for dtls are stored in snmpd.conf.
update_snmp_config_with_multiple_agentAddressDtls(Config) ->
    Handler = ?config(handler, Config),
    {ok, [{agentAddressDtls, [{Host, Port}]}]} = rct_snmp_lib:get_snmp_agent_address(Handler, dtls),
    ok = rct_snmp_lib:set_snmp_agent_address(Handler, dtls, [{Host, 10617}, {Host, 10618}, {Host, 10619}]),
    timer:sleep(5000),
    
    check_snmp_running(10617, true),
    check_snmp_running(10618, true),
    check_snmp_running(10619, true),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    
    %return to the starting state
    ok = rct_snmp_lib:set_snmp_agent_address(Handler, dtls, [{Host, Port}]),
    ok.


%% TC-US1-1n    Delete agentAddressDtls from Snmp MO.
%% Check that all agentaddresses for dtls are deleted from snmpd.conf.
update_snmp_config_after_agentAddressDtls_is_deleted(Config) ->
    Handler = ?config(handler, Config),
    {ok, [{agentAddressDtls, [{Host, Port}]}]} = rct_snmp_lib:get_snmp_agent_address(Handler, dtls),
    ok = rct_snmp_lib:set_snmp_agent_address(Handler, dtls, []),
    timer:sleep(5000),
    check_snmp_running(list_to_integer(Port), false),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    %return to the starting state
    ok = rct_snmp_lib:set_snmp_agent_address(Handler, dtls, [{Host, Port}]),
    ct:pal("Successfuly deleted agentAddressDtls from Snmp MO"),
    ok.

%% TC-US1-1o    Lock/unlock Snmp MO
%% Lock Snmp MO. Check that operationalState is DISABLED. Update Snmp config with TLS parameters.
%% Check that changes are made in snmpd.conf but snmpd is not started. Unlock Snmp MO.
%% Check that snmpd is started with TLS configuration and operational state is ENABLED
lock_unlock_snmp_mo(Config) ->
    ct:pal("Lock/unlock Snmp MO",[]),
    ok = rct_snmp_lib:set_snmp_administrative_state(?config(handler, Config), locked),
    timer:sleep(10000),
        
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_PORT, false),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, false),
    {ok, disabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("DTLS with locked Snmp MO is disabled"),
    
    {ok, _NCDn} = rct_snmp_lib:get_snmp_node_credential(?config(handler, Config)),
    {ok, _TcatDn} = rct_snmp_lib:get_snmp_trust_category(?config(handler, Config)),
    {ok, [{agentAddressDtls, [{_Host, _Port}]}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
        
    ok = rct_snmp_lib:set_snmp_administrative_state(?config(handler, Config), unlocked),
    timer:sleep(10000),

    check_snmp_running(?DEFAULT_AGENT_ADDRESS_PORT, true),
    check_snmp_running(?DEFAULT_AGENT_ADDRESS_DTLS_PORT, true),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ct:pal("DTLS with unlocked Snmp MO is enabled"),
 
    ok.

%% TC-US1-2 Configure Snmp MO with nodeCredential and trustCategory references 
%% and agentAddressDtls. Check that Snmp MO is enabled. Create SnmpTargetV3Dtls. 
%% Check that it is ENABLED. Check that it is written to snmpd.conf
configure_snmpTargetV3Dtls(Config)-> 
    timer:sleep(6000),
	ct:pal("V3Dtls targets:~p~n", [rct_snmp_lib:get_snmp_targets(?config(handler, Config), v3dtls)]),
    
	%%check target enabled
    {ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
	{ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
     ok.

%% TC-US1-2a Configure Snmp MO with nodeCredential and trustCategory references 
%% and agentAddressDtls. Check that Snmp MO is enabled. Create multiple SnmpTargetV3Dtls MOs. 
%% Check that they are ENABLED. Check that it is written to snmpd.conf
configure_multiple_snmpTargetV3Dtls_targets(Config)->
	{ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
	{ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "2"),
    {ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "3"),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ok.

%% TC-US1-2b Delete SnmpTargetV3Dtls MO from the node. Check that it is deleted from snmpd.conf
delete_snmpTargetV3Dtls(Config)->
    V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, "labuser", undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
    timer:sleep(5000),
    ok = delete_snmptargetv3dtls_mo(?config(handler, Config), ["1"]),
    timer:sleep(5000),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ok.

%% TC-US1-2c Lock SnmpTargetV3Dtls MO. Check that it is deleted from snmpd.conf. Check that 
%% all changes on MO are ignored (not written to conf) and MO is DISABLED Unlock it again 
%% and check that all changes are written to snmpd.conf and MO is ENABLED
lock_unlock_snmpTargetV3Dtls(Config)->
	ok = rct_snmp_lib:set_snmp_target_administrative_state(?config(handler, Config), v3dtls, "1", locked),
    timer:sleep(5000),
    {ok, locked} = rct_snmp_lib:get_snmp_target_administrative_state(?config(handler, Config), v3dtls, "1"),
	{ok, disabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
	
	ok = rct_snmp_lib:set_snmp_target_administrative_state(?config(handler, Config), v3dtls, "1", unlocked),
    timer:sleep(5000),
	{ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ok.

%% TC-US1-2d Update SnmpTargetV3Dtls MO with new user. Check that it is changed in snmpd.conf
update_snmpTargetV3Dtls_new_user(Config) ->
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{user, "labuser2"}]),
    timer:sleep(6000),
    {ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ok.

%% TC-US1-2e Update SnmpTargetV3Dtls MO with new boolean isMibWritable. 
%% Check that it is changed in snmpd.conf
update_snmpTargetV3Dtls_new_isMibWritable(Config) ->
    ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{isMibWritable, "false"}]),
    timer:sleep(6000),
    {ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
	ok.


%% TC-US1-2f Update SnmpTargetV3Dtls MO with various new parameters for trapses:
%% - address (IPV4/6)
%% - port
%% - transportMethod (INFORM/TRAP)
%% - timeout 
%% - retryCount. Check that it they are all changed in snmpd.conf
update_snmpTargetV3Dtls_new_various_parameters(Config) ->
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{address, "10.68.101.132"}]),
    timer:sleep(6000),
	
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{port, "10163"}]),
    timer:sleep(6000),
	
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{transportMethod, "INFORM"}]),
    timer:sleep(6000),
	
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{informTimeout, "100"}]),
    timer:sleep(6000),

	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{informRetryCount, "2"}]),
    timer:sleep(10000),
    
    ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), 
                                   [{address, "10.67.31.150"}, {port, "10162"}, {transportMethod, "TRAP"}, 
                                    {informTimeout, "300"}, {informRetryCount, "1"}]),

    timer:sleep(10000),
    {ok, enabled} = rct_snmp_lib:get_snmp_target_operational_state(?config(handler, Config), v3dtls, "1"),
    {ok, enabled} = rct_snmp_lib:get_snmp_operational_state(?config(handler, Config)),
    ok.


%%--------------------------------------------------------------------
%% TEST CASES - com SNMP Dtls event handler tests
%%--------------------------------------------------------------------
% TC-US1-3
% Snmp MO and SnmpTargetV3Dtls are enabled. 
% Check that the manager receives the heartbeat TRAP message.
send_trap_to_manager_with_trusted_certificate(Config) ->
    A = os:timestamp(),
    ok = timer:sleep(5000),
    V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, ?SNMP_TARGET_V3_ID, undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
    ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?SNMP_TARGET_DN(v3dtls, "1"), 
                                [{informRetryCount, integer_to_list(?config(retries, Config))},
                                 {informTimeout, integer_to_list(?config(timeout, Config))},
                                 {isMibWritable, atom_to_list(true)},
                                 {transportMethod, "TRAP"}
                                 ]),
    timer:sleep(5000),
    B = os:timestamp(),
    {ok, _} = check_log( {A, B}, "/var/log/traps.log", ?TRAP, Config),
    
    timer:sleep(10000),
    ok.

%TC-US1-3a
% Snmp MO and SnmpTargetV3Dtls are enabled. 
% Check that the manager receives the heartbeat INFORM message.
send_inform_to_manager_with_trusted_certificate(Config) ->
    A = os:timestamp(),
    timer:sleep(5000),
    V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, ?SNMP_TARGET_V3_ID, undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
     
    rct_mo_handler_lib:set_mo(?config(handler, Config), ?SNMP_TARGET_DN(v3dtls, "1"), 
                                [{informRetryCount, integer_to_list(?config(retries, Config))},
                                 {informTimeout, integer_to_list(?config(timeout, Config))},
                                 {isMibWritable, atom_to_list(true)},
                                 {transportMethod, "INFORM"}
                                 ]),

    timer:sleep(20000),
    B = os:timestamp(),
    {ok, _} = check_log( {A, B}, "/var/log/traps.log", ?TRAP, Config),

    timer:sleep(10000),
    ok.

%% TC-US1-3b
%% Snmp and SnmpTargetV3Dtls are enabled. Manager has certificate which is not trusted. 
%% Check that the connection is rejected.
send_trap_inform_to_manager_with_non_trusted_certificate(Config) ->
    TcId = ?config(trust_cert_id, Config),
    {ok, _CertsBefore} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), "tcat_1"),
    ok = rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), TcId, disabled),
  
    timer:sleep(20000),
    A = os:timestamp(),
    timer:sleep(5000),
    V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, ?SNMP_TARGET_V3_ID, undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
     
    rct_mo_handler_lib:set_mo(?config(handler, Config), ?SNMP_TARGET_DN(v3dtls, "1"), 
                                [{informRetryCount, integer_to_list(?config(retries, Config))},
                                 {informTimeout, integer_to_list(?config(timeout, Config))},
                                 {isMibWritable, atom_to_list(true)},
                                 {transportMethod, "TRAP"}
                                 ]),
 
    B = os:timestamp(),
    error = check_log( {A, B}, "/var/log/traps.log", ?TRAP, Config),
    
    ok = rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), ?config(trust_cert_id, Config), enabled),
    timer:sleep(5000),
    ok.

%% TC-US1-4
%% Configure Snmp MO with NC and TC, and SnmpTargetVrDtls. Send SNMP GET sysContact.0 
%% from manager. Check that response is received successfully.
send_snmp_command_from_manager_to_node(Config) ->
    V3DtlsConfig = rct_snmp_lib:create_snmp_target_configuration(v3dtls, ?SNMP_TARGET_V3_ID, ?SNMP_TARGET_V3_PORT, ?SNMP_TARGET_V3_ID, undefined),
    ok = rct_snmp_lib:create_snmp_target(?config(handler, Config), v3dtls, "1", V3DtlsConfig),
     
    rct_mo_handler_lib:set_mo(?config(handler, Config), ?SNMP_TARGET_DN(v3dtls, "1"), 
                                [{informRetryCount, integer_to_list(?config(retries, Config))},
                                 {informTimeout, integer_to_list(?config(timeout, Config))},
                                 {isMibWritable, atom_to_list(true)},
                                 {transportMethod, "INFORM"}
                                 ]),
    ok = rct_mo_handler_lib:set_mo(?config(handler, Config), rct_snmp_lib:get_snmp_target_dn(v3dtls, "1"), [{informTimeout, "100"}]),
    timer:sleep(20000),
    case rct_rpc:call(rpc, ootI, get_lmt_ipv4, [], 10000) of
        [] -> Host = rct_rpc:call(rpc, ootI, get_oap_ip_addr, [], 10000);
        Host -> Host
    end,
    IpAddr = inet:ntoa(Host),
    Resp = snmp_get(v3dtls, [IpAddr, "10616", ?SnmpEngineBoots], ?config(connection_reference, Config)), 
    ok = check_response(Resp, ?SNMP_GET_RESPONSE),
    
    ct:log("Delete dtlsUser"),
    ok = delete_snmptargetv3dtls_mo(?config(handler, Config), ["1"]),
    timer:sleep(5000),
    ok.

%% TC-US1-4a
%% Configure Snmp MO without NC and TC, and SnmpTargetV3Dtls. 
%% Send SNMP GET sysContact.0 from manager. Check that message is not received
send_snmp_command_from_manager_to_node_no_tls_params(Config) -> 
    Config = removeNC(Config),
    Config = removeTC(Config),
    timer:sleep(5000),
    case rct_rpc:call(rpc, ootI, get_lmt_ipv4, [], 10000) of
        [] -> Host = rct_rpc:call(rpc, ootI, get_oap_ip_addr, [], 10000);
        Host -> Host
    end,
    IpAddr = inet:ntoa(Host),
    Resp = snmp_get(v3dtls, [IpAddr, "10616", ?SnmpEngineBoots], ?config(connection_reference, Config)),

    error = check_response(Resp, ?SNMP_GET_RESPONSE),
    installNC(Config),
    installTC(Config),
    timer:sleep(5000),
    ok.

%% TC-US1-4c
%% Configure Snmp MO with NC and TC, without agentAddressDtls, and SnmpTargetV3Dtls. Send SNMP GET s
%% ysContact.0 from manager. Check that message is not received.
send_snmp_command_from_manager_to_node_no_agentAddressDtls(Config) ->
    case rct_rpc:call(rpc, ootI, get_lmt_ipv4, [], 10000) of
        [] -> Host = rct_rpc:call(rpc, ootI, get_oap_ip_addr, [], 10000);
        Host -> Host
    end,
    IpAddr = inet:ntoa(Host),
    Resp = snmp_get(v3dtls, [IpAddr, "10616", ?SnmpEngineBoots], ?config(connection_reference, Config)),

    error = check_response(Resp, ?SNMP_GET_RESPONSE),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES - Configuration of TLS cipher suites
%%--------------------------------------------------------------------
%% TC-US2-1
%% Change cipher suite in TLS MO. Check that the new cipher suites are stored in snmpd.conf.
configure_tls_cipher_suites(Config) ->
	CipherFilter = "RSA",

	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?TLS_DN, [{cipherFilter, "DEFAULT"}]),
	{ok, [{cipherFilter, CipherFilterDefault}]} = rct_mo_handler_lib:get_mo(?config(handler, Config), ?TLS_DN, [cipherFilter]),
	
	Timestamp1 = os:timestamp(),
	
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?TLS_DN, [{cipherFilter, CipherFilter}]),
    timer:sleep(10000),
	
    Timestamp2 = os:timestamp(),
    {ok, _} = check_log({Timestamp1, Timestamp2}, "/var/log/traps.log", ?TRAP, Config),
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?TLS_DN, [{cipherFilter, CipherFilterDefault}]).

%% TC-US2-1a
%% Change cipher suite in TLS MO. Check that the new cipher suites are stored in snmpd.conf. 
%% Check that agent and manager cannot communicate because they have different cipher suites.
configure_unsupported_tls_cipher_suites(Config) ->
	CipherFilter = "DSS",
	{ok, [{cipherFilter, CipherFilterDefault}]} = rct_mo_handler_lib:get_mo(?config(handler, Config), ?TLS_DN, [cipherFilter]),
	
	Timestamp1 = os:timestamp(),
	
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?TLS_DN, [{cipherFilter, CipherFilter}]),   
	timer:sleep(10000),
	
    Timestamp2 = os:timestamp(),
    error = check_log({Timestamp1, Timestamp2}, "/var/log/traps.log", ?TRAP, Config),
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?TLS_DN, [{cipherFilter, CipherFilterDefault}]).

check_config_data_after_node_restart(Config) ->
	CipherFilter = "RSA",
	%% {ok, [{agentAddressDtls, [{Host, Port}]}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), dtls),
	
	ok = rct_mo_handler_lib:set_mo(?config(handler, Config), ?TLS_DN, [{cipherFilter, CipherFilter}]),
	timer:sleep(5000),
	
	case is_target() of
        false -> ct:pal("Skipping restart on sim"),
                 ok;
        true -> 
            rct_rpc:call(rpc, appmI, restart_node, [cold, "Checking config after restart"], 10000),
            timer:sleep(60000),
            waitForNodeUp()
    end.

	

%%--------------------------------------------------------------------
%% TEST CASES - Autointegration - "hello" message over DTLS
%%--------------------------------------------------------------------
hello_message(Config) ->
    Time = get_start_time(Config),
    continue = rct_rpc:call(rpc, aicSnmp, do_send_trap, [], 10000),
    timer:sleep(5000),
    IpProtocol = ?config(ip_protocol, Config),
    NodeIp = case is_target() of
                true -> inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, lmt, IpProtocol));
                false -> inet_parse:ntoa(ftpes_test_lib:get_client_ip(?config(handler, Config), ipv4))
             end,
    [Trap | _] = check_def_log(NodeIp, Time, ?HELLO, Config),
    [?HELLO] = get_traps_value([Trap], "SNMPv2-MIB::snmpTrapOID.0", 1),
    ok.

hello_message_dtls(Config) ->
    Time = get_start_time(Config),
    continue = rct_rpc:call(rpc, aicSnmp, do_send_trap, [], 10000),
    timer:sleep(5000),
    IpProtocol = ?config(ip_protocol, Config),
    NodeIp = case is_target() of
                true -> inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, lmt, IpProtocol));
                false -> inet_parse:ntoa(ftpes_test_lib:get_client_ip(?config(handler, Config), ipv4))
             end,
    [Trap | _] = check_def_log(NodeIp, Time, ?HELLO, Config),
    [?HELLO] = get_traps_value([Trap], "SNMPv2-MIB::snmpTrapOID.0", 1),
    ok.

hello_message_non_trusted_certificate(Config) ->
    Time = get_start_time(Config),
    continue = rct_rpc:call(rpc, aicSnmp, do_send_trap, [], 10000),
    timer:sleep(5000),
    IpProtocol = ?config(ip_protocol, Config),
    NodeIp = case is_target() of
                true -> inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, lmt, IpProtocol));
                false -> inet_parse:ntoa(ftpes_test_lib:get_client_ip(?config(handler, Config), ipv4))
             end,
    [] = check_def_log(NodeIp, Time, ?HELLO, Config),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES - Fault Management - SNMP over DTLS
%%--------------------------------------------------------------------
%% TC-US5-1
%% Snmp MO and SnmpTargetV3Dtls are enabled. Manager sets the wanted HB interval via SNMPSET. 
%% Check that the new interval is set.
set_heartbeat_interval(Config) ->
    {ok,[{heartbeatInterval, "60"}]}  = rct_mo_handler_lib:get_mo(?config(handler, Config), ?FM_DN, [heartbeatInterval]),
    HBInterval = "40",
    Response = "SNMPv2-SMI::enterprises.193.183.4.1.5.1.0 = Gauge32: " ++ HBInterval ++ "\n",
    ct:pal("Setting heartbeat interval to 40"),

    case ?config(dtls, Config) of
        v3 ->
            Response = snmp_set(?config(dtls, Config), [?V3USER, "MD5", "DES", ?V3PASSWORD, ?V3PASSWORD, "authPriv", ?config(nodeIp, Config), "6161",
                                       ?HEARTBEAT, "u", HBInterval], ?config(connection_reference, Config)),
            timer:sleep(10000),
            Response = snmp_get(?config(dtls, Config), [?V3USER, "MD5", "DES", ?V3PASSWORD, ?V3PASSWORD, "authPriv", ?config(nodeIp, Config), "6161",
                                       ?HEARTBEAT], ?config(connection_reference, Config));
        v3dtls ->
            Response = snmp_set(?config(dtls, Config), [?config(nodeIp, Config), "10616", ?HEARTBEAT, "u", HBInterval], ?config(connection_reference, Config)),
            timer:sleep(10000),
            Response = snmp_get(?config(dtls, Config), [?config(nodeIp, Config), "10616", ?HEARTBEAT], ?config(connection_reference, Config))
    end,

    {ok,[{heartbeatInterval, HBInterval}]}  = rct_mo_handler_lib:get_mo(?config(handler, Config), ?FM_DN, [heartbeatInterval]),
    
    ct:pal("Checking if new HB is working properly"),
    Timestamp = get_start_time(Config),
    From = {date(), time()},
    WaitingTime = 90,
    Added = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + WaitingTime,
    To = calendar:gregorian_seconds_to_datetime(Added),
    timer:sleep(WaitingTime*1000),

    Traps = filter_traps(?config(nodeIp, Config), Timestamp, From, To, ?HBOid, Config), 
    Timings = get_time_from_recordlog(Traps, []),
    heartbeat_interval_check(Timings, HBInterval),

    ct:pal("New heartbeat interval successfully set"),
    ok.


%% TC-US5-1a
%% Snmp MO and SnmpTargetV3Dtls are enabled. Check that the manager receives a HB message.
receive_heartbeat(Config) ->
    {ok,[{heartbeatInterval, HBInterval}]}  = rct_mo_handler_lib:get_mo(?config(handler, Config), ?FM_DN, [heartbeatInterval]),
    Timestamp = get_start_time(Config),
    From = {date(), time()},
    WaitingTime = 90,
    Added = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + WaitingTime,
    To = calendar:gregorian_seconds_to_datetime(Added),
    ct:pal("Checking if heartbeat interval is set and working properly"),
    timer:sleep(WaitingTime*1000),

    Traps = filter_traps(?config(nodeIp, Config), Timestamp, From, To, ?HBOid, Config), 
    Timings = get_time_from_recordlog(Traps, []),
    heartbeat_interval_check(Timings, HBInterval),

    ct:pal("New heartbeat interval successfully set"),
    ok.

%% TC-US5-2
%% Snmp MO and SnmpTargetV3Dtls are enabled. Raise an alarm. Check that the manager is informed.
alarm_actions_inform_manager(Config) ->
    TimeA = get_start_time(Config),
    ok = rct_rpc:call(rpc, comsaI, send_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', 
                           major, [<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass=1">>], 
                           "major"],20000),
    ct:pal("Alarm raised"),
    timer:sleep(30000), %% Alarm takes about 20 sec before active and visible in log.
    [A1, A2 | _] = check_def_log(?config(nodeIp, Config), TimeA, "\"major\"", Config),
    TrapsA = [A1, A2],
    ["\"major\""] = get_traps_value(TrapsA, "SNMPv2-SMI::enterprises.193.183.4.1.2.1.0", 2), % 2 traps, for some reasson duplicated traps in log
    
    TimeB = get_start_time(Config),
    ok = rct_rpc:call(rpc, comsaI, send_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', 
                           critical, [<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass=1">>], 
                           "critical"],20000),
    ct:pal("Alarm critical"),
    timer:sleep(30000), %% Alarm takes about 20 sec before active and visible in log.
    [B1, B2 | _] = check_def_log(?config(nodeIp, Config), TimeB, "\"critical\"", Config),
    TrapsB = [B1, B2],
    ["\"critical\""] = get_traps_value(TrapsB, "SNMPv2-SMI::enterprises.193.183.4.1.2.1.0", 2), % 2 traps, for some reasson duplicated traps in log

    TimeC = get_start_time(Config),
    ok = rct_rpc:call(rpc, comsaI, clear_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', 
                           [<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass=1">>], 
                           "clear"],20000),
    ct:pal("Alarm ceased"),
    timer:sleep(30000), %% Alarm takes about 20 sec before active and visible in log.
    [C1, C2 | _] = check_def_log(?config(nodeIp, Config), TimeC, "\"critical\"", Config),
    TrapsC = [C1, C2],
    ["\"critical\""] = get_traps_value(TrapsC, "SNMPv2-SMI::enterprises.193.183.4.1.2.1.0", 2), % 2 traps, for some reasson duplicated traps in log
    ok.

%% TC-US5-3
%% Snmp MO and SnmpTargetV3Dtls are enabled. An alert notification is generated and sent to the 
%% manager. Check that the manager received the alert.
agent_sends_alert(Config) ->
    Time = get_start_time(Config),
    ok = rct_rpc:call(rpc, comsaI, send_alarm,['mock-alert-65520', 
                           major, [<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass=1">>], 
                           "alert"],20000),
    ct:pal("Alert is sent"),
    timer:sleep(30000),
    [A1, A2 | _] = check_def_log(?config(nodeIp, Config), Time, "\"alert\"", Config),
    Traps = [A1, A2],
    ["\"alert\""] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.4.1.2.1.0", 2), % 2 traps, for some reasson duplicated traps in log

    ok.

%% TC-US5-4
%% Snmp MO and SnmpTargetV3Dtls are enabled. Node is restared by the operator. Node sends alarm 
%% list rebuild notification to manager,
send_alarm_list_rebuilt(Config) ->
    Time = get_start_time(Config),
    Com = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000),
    [ComPid|_] = string:tokens(Com, "\n "),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ ComPid], 2000),
    _NewComPid = wait_for_com_to_restart(ComPid),
    timer:sleep(60000),
    [A1, A2 | _] = check_def_log(?config(nodeIp, Config), Time, "SNMPv2-SMI::enterprises.193.183.4.2.0.30", Config),
    Traps = [A1, A2],
    ["SNMPv2-SMI::enterprises.193.183.4.2.0.30"] = get_traps_value(Traps, "SNMPv2-MIB::snmpTrapOID.0", 2), % 2 traps, for some reasson duplicated traps in log
    ok.

%% TC-US5-4a
%% Snmp MO and SnmpTargetV3Dtls are enabled. Manager gets active alarm list via snmp. 
%% Value must be empty string
get_active_alarm_list(Config) ->
    ct:pal("Get active alarm list",[]),
    Resp = snmp_get(?config(dtls, Config), [?V3USER, "MD5", "DES", ?V3PASSWORD, ?V3PASSWORD, "authPriv", ?config(nodeIp, Config), "6161",
                                       ?ALARM_ALERT_TABLE_URL], ?config(connection_reference, Config)),
    ct:pal("Resp:~p", [Resp]),
    ok = check_response(Resp, ?SNMP_GET_ALARM_LIST_RESPONSE),
    
    ok.


%%--------------------------------------------------------------------
%% TEST CASES - for testing the sending of the IP change trap
%%--------------------------------------------------------------------
changed_ipAddr_trap_restart(Config) ->
	timer:sleep(60000),
	Time = get_start_time(Config),
    IpProtocol = ?config(ip_protocol, Config),
 
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    ct:log("OAM address is ~p", [OamAddress]),
   
    ct:log("Change OAM reference to ALT OAM address reference"),
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(alt_oam, IpProtocol), oam),
    timer:sleep(1000),
	
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    ct:log("Alt OAM address is ~p", [AltOamAddress]),
	%% restart (note that the namespace simulator replaces IFT after restart)
    %% not acknowledging the trap
    ct:log("Not acknowledging trap", []),
    cold_restart(),

    %% after restart, must connect
%%     ok = cli_connect(),
	
	timer:sleep(60000),
    NodeIp = inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, alt_oam, ?config(ip_protocol, Config))), 
	Traps = check_def_log(NodeIp, Time, "SNMPv2-SMI::enterprises.193.183.6.2.0.1", Config),
	[EriChangeIPAddressAckFdn] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.6", 2), % 2 traps, for some reasson duplicated traps in log
    [EriChangeIPAddressAckAttributeName] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.7", 2),
    [EriChangeIPAddressAckAttributeValue] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.8", 2), 
	ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName, EriChangeIPAddressAckAttributeValue, Config),

    ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    timer:sleep(1000),
    Config.

changed_ipAddr_trap_ack(Config) ->
    timer:sleep(60000),
	Time = get_start_time(Config),
	IpProtocol = ?config(ip_protocol, Config),
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    ct:log("OAM address is ~p", [OamAddress]),
   
    ct:log("Change OAM reference to ALT OAM address reference"),
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(alt_oam, IpProtocol), oam),
    timer:sleep(1000),
	
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    ct:log("Alt OAM address is ~p", [AltOamAddress]),
	Ip = inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, oam, ?config(ip_protocol, Config))), 
	Traps = check_def_log(Ip, Time, "SNMPv2-SMI::enterprises.193.183.6.2.0.1", Config),
	[EriChangeIPAddressAckFdn] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.6", 2), % 2 traps, for some reasson duplicated traps in log
    [EriChangeIPAddressAckAttributeName] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.7", 2),
    [EriChangeIPAddressAckAttributeValue] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.8", 2), 
	
%% 	ok = cli_connect(),
	ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName, EriChangeIPAddressAckAttributeValue, Config),
	Time1 = get_start_time(Config),
    timer:sleep(5000),
	
    %%check that no more traps are sent after acknowledgment
    Ip1 = inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, oam, ?config(ip_protocol, Config))),
    [] = check_def_log(Ip1, Time1, "SNMPv2-SMI::enterprises.193.183.6.2.0.1", Config),
	
	ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    timer:sleep(1000),
    Config.

changed_ipAddr_trap_no_ack(Config) ->
   timer:sleep(60000),
	Time = get_start_time(Config),
	IpProtocol = ?config(ip_protocol, Config),
    OamAddress = ftpes_test_lib:get_node_ip(config, oam, IpProtocol),
    ct:log("OAM address is ~p", [OamAddress]),
   
    ct:log("Change OAM reference to ALT OAM address reference"),
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(alt_oam, IpProtocol), oam),
    timer:sleep(1000),
	
    AltOamAddress = ftpes_test_lib:get_node_ip(config, alt_oam, IpProtocol),
    ct:log("Alt OAM address is ~p", [AltOamAddress]),
    %% not acknowledging the trap
    ct:log("Not acknowledging trap", []),

    %%check that  trap is being resent
    timer:sleep(60000),
    Ip = inet_parse:ntoa(ftpes_test_lib:get_node_ip(config, oam, ?config(ip_protocol, Config))),
    Traps = check_def_log(Ip, Time, "SNMPv2-SMI::enterprises.193.183.6.2.0.1", Config),

%%     ok = cli_connect(),

    [EriChangeIPAddressAckFdn] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.6", 2), % 2 traps, for some reasson duplicated traps in log
    [EriChangeIPAddressAckAttributeName] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.7", 2),
    [EriChangeIPAddressAckAttributeValue] = get_traps_value(Traps, "SNMPv2-SMI::enterprises.193.183.6.1.8", 2), 
	ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName, EriChangeIPAddressAckAttributeValue, Config),

   	ct:log("Change OAM reference back to OAM address"),
    set_oam_ref(IpProtocol),
    timer:sleep(1000),
	Config.


%%--------------------------------------------------------------------------------------
%% HELPER FUNCTIONS

start_connection_to_node(Config) ->
    ConRef = start_connection_to_node( ?config(user, Config),
                                       ?config(pass, Config), 
                                       ?config(remote_ip, Config),
                                       ?config(remote_connect_port, Config)),
    ConRef.

start_connection_to_node(User, Pass, RemoteIp, RemotePort) ->
%%    ssh:start(),
   {ok, ConRef} = ssh:connect(RemoteIp, RemotePort, [{user, User}, {password, Pass}, {silently_accept_hosts, true}]),
   ConRef.

delete_snmptargetv3dtls_mo(_Handler, []) ->
	ok;
delete_snmptargetv3dtls_mo(Handler, [HeadID|TailIDs])->
	ok = rct_snmp_lib:delete_snmp_target(Handler, v3dtls, HeadID),
 	delete_snmptargetv3dtls_mo(Handler, TailIDs).

%% generateUser(Name, Type, Host, Port, TransportType, Retries, Timeout, AdmState) ->
%% 	#snmpTargetV3Dtls{snmpTargetV3DtlsId = Name,
%% 					  informRetryCount = Retries,
%%                       administrativeState = AdmState,
%%                       address = Host,
%%                       port = Port,
%%                       informTimeout = Timeout,
%%                       transportMethod = TransportType,
%%                       isMibWritable = Type,
%%                       operationalState = 'RcsSnmp.BasicOperState',
%%                       user = Name}.

installNC(Config) ->
    ok = rct_certm_lib:create_node_credential(?config(handler, Config), ?config(ncName, Config)),

    ok = rct_certm_lib:install_node_credential_from_uri(?config(handler, Config), ?config(ncName, Config),
                                                        ?config(ncUri, Config), ?config(uriPassword, Config), 
                                                        ?config(ncPassword, Config), ?config(ncFingerprint, Config)),
    Config.

installTC(Config) ->
    {ok, TcId} = rct_certm_lib:install_trusted_certificate_from_uri(?config(handler, Config), ?config(tcUri, Config),
                                                                     ?config(uriPassword, Config), ?config(tcFingerprint, Config)),

    {ok, TrustedCertIds} =  rct_certm_lib:get_all_trusted_certificates(?config(handler, Config)),
    
    [ rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), Tc, enabled) || Tc <- TrustedCertIds],
    
    ok = rct_certm_lib:create_trust_category(?config(handler, Config), ?config(tcName, Config), TrustedCertIds),

    [{trust_cert_id, TcId} | Config].

removeNC(Config) ->
    rct_certm_lib:delete_node_credential(?config(handler, Config), ?config(ncName, Config)),
    
    Config.

removeTC(Config) ->
    rct_certm_lib:delete_trust_category(?config(handler, Config), ?config(tcName, Config)),
    
    {ok, TrustCerts} =  rct_certm_lib:get_all_trusted_certificates(?config(handler, Config)),
    [ rct_certm_lib:delete_trusted_certificate(?config(handler, Config), Tc) || Tc <- TrustCerts],
    
    Config.

set_snmp_config(Config) ->
    NCDn = rct_certm_lib:get_node_credential_dn(?config(ncName, Config)),
    TcatDn = rct_certm_lib:get_trust_category_dn(?config(tcName, Config)),
    ok = rct_snmp_lib:set_snmp_node_credential(?config(handler, Config), NCDn),
    ok = rct_snmp_lib:set_snmp_trust_category(?config(handler, Config), TcatDn),
    {ok, NCDn} = rct_snmp_lib:get_snmp_node_credential(?config(handler, Config)),
    {ok, TcatDn} = rct_snmp_lib:get_snmp_trust_category(?config(handler, Config)),
    timer:sleep(5000),
    Config.

unset_snmp_config(Config) ->
    ok = rct_snmp_lib:set_snmp_node_credential(?config(handler, Config), undefined),
    ok = rct_snmp_lib:set_snmp_trust_category(?config(handler, Config), undefined),
    Config.

set_agent_address(Port, Config) ->
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), udp, [{"0.0.0.0", Port}]).

set_agent_address_dtls(Port, Config) ->
    {ok, [{agentAddress, [{Host, _Port}]}]} = rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), udp),
    set_agent_address_dtls(Host, Port, Config).

set_agent_address_dtls(Host, Port, Config) ->
    ok = rct_snmp_lib:set_snmp_agent_address(?config(handler, Config), dtls, [{Host, Port}]),
    ct:pal("agentAddress all: ~p~n", [rct_snmp_lib:get_snmp_agent_address(?config(handler, Config), all)]),
    ok.

reset_restart_list() ->
    ct:pal("Reseting restart list"),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 30000),
    rct_rpc:call(rpc, alhI, reset_log, [], 10000),
    timer:sleep(10000).


    %########### SNMP COMMANDS ####################
% Port - snmpd 161 for udp and tcp, 10161 dtlsudp, tlstcp
% Port snmptrapd 10162

% Function for SNMPGET 
% RemoteIp is Ip from agent
% Test on localhost

%% Example (heartbeatI):  snmpset -u user -a MD5 -x DES -A 12345678 -X 12345678 -l authPriv 10.67.225.26:6161 .1.3.6.1.4.1.193.183.4.1.5.1.0 u 75
snmp_set_v3([User, AuthProtocol, PrivProtocol, AuthKey, PrivKey, SecLevel, RemoteIp, Port, Oid, Type, Value]) ->
    "/usr/local/bin/snmpset -v 3 --defSecurityModel=usm " ++ 
    "-l " ++ SecLevel ++ " " ++
    "-u " ++ User ++ " " ++"-a " ++ AuthProtocol ++ " " ++
    "-x " ++ PrivProtocol ++ " " ++
    "-A " ++ AuthKey ++ " " ++
    "-X " ++ PrivKey ++ " " ++
    RemoteIp ++ ":" ++ Port ++ " " ++
    Oid ++ " "++
    Type ++ " " ++
    Value.

%% Example (heartbeatI):  snmpget -u user -a MD5 -x DES -A 12345678 -X 12345678 -l authPriv 10.67.225.26:6161 .1.3.6.1.4.1.193.183.4.1.5.1.0
snmp_get_v3([User, AuthProtocol, PrivProtocol, AuthKey, PrivKey, SecLevel, RemoteIp, Port, Oid]) ->
    "/usr/local/bin/snmpget -v 3 --defSecurityModel=usm " ++ 
    "-l " ++ SecLevel ++ " " ++
    "-u " ++ User ++ " " ++
    "-a " ++ AuthProtocol ++ " " ++
    "-x " ++ PrivProtocol ++ " " ++
    "-A " ++ AuthKey ++ " " ++
    "-X " ++ PrivKey ++ " " ++
    RemoteIp ++ ":" ++ Port ++ " " ++
    Oid.

snmp_set_v3dtls([RemoteIp, Port, Oid, Type, Value]) ->
    "/usr/local/bin/snmpset -v 3 " ++ 
    "-T our_identity=4D:34:BE:BC:74:F7:6D:B2:E8:49:5B:CA:E7:7E:8E:AC:93:36:42:55" ++ " " ++
    "-T their_identity=3E:BC:1A:11:DE:6C:2B:1A:9D:A8:C0:D3:22:6E:A8:04:72:C1:2C:27" ++ " " ++  
    "dtlsudp" ++ ":" ++
    RemoteIp ++ ":" ++ Port ++ " " ++
    Oid ++ " " ++
    Type ++ " " ++
    Value.

% Example: ./snmpget -v 3 -T their_hostname=steph dtlsudp:10.67.31.150:10161 sysContact.0
snmp_get_v3dtls([RemoteIp, Port, Oid]) ->
    "/usr/local/bin/snmpget -v 3 " ++ 
    "-T our_identity=4D:34:BE:BC:74:F7:6D:B2:E8:49:5B:CA:E7:7E:8E:AC:93:36:42:55" ++ " " ++
    "-T their_identity=3E:BC:1A:11:DE:6C:2B:1A:9D:A8:C0:D3:22:6E:A8:04:72:C1:2C:27" ++ " " ++  
    "dtlsudp" ++ ":" ++ 
    RemoteIp ++ ":" ++ 
    Port ++ " " ++ Oid.    %oid

snmp_set(v3, Arguments, ConRef) ->
    Cmd = snmp_set_v3(Arguments),
    execute_cmd(Cmd, ConRef);
snmp_set(v3dtls, Arguments, ConRef) ->
    Cmd = snmp_set_v3dtls(Arguments),
    execute_cmd(Cmd, ConRef).

snmp_get(v3, Arguments, ConRef) ->
    Cmd = snmp_get_v3(Arguments),
    execute_cmd(Cmd, ConRef);
snmp_get(v3dtls, Arguments, ConRef) ->
    Cmd = snmp_get_v3dtls(Arguments),
    execute_cmd(Cmd, ConRef).

execute_cmd(Cmd, ConRef) ->
    {ok, ChannelId} = ssh_connection:session_channel(ConRef, infinity),
    success = ssh_connection:exec(ConRef, ChannelId, Cmd, 10000),
    receive_ssh_message().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_log({A, B}, LogFile, StringMatch, Config) ->
   check_log({A, B}, LogFile, StringMatch, Config, "2", 6).

check_log({_A, _B}, _LogFile, _StringMatch, _Config, _N ,0) ->
    error; 
check_log({A, B}, LogFile, StringMatch, Config, N, Acc) ->
    {_ADate, ATime} = calendar:now_to_local_time(A),
    {_BDate, BTime} = calendar:now_to_local_time(B),
    Cmd = "tail -n " ++ N ++ " " ++ LogFile,
    ConRef = ?config(connection_reference, Config),
    {ok, ChannelId} = ssh_connection:session_channel(ConRef, infinity),
    success = ssh_connection:exec(ConRef, ChannelId, Cmd, 10000),
    Msg = receive_ssh_message(),
    ct:log("Msg is ~p", [Msg]),
    ok = ssh_connection:close(ConRef, ChannelId),
    SplitResp = string:tokens(Msg, " "),
    Time = lists:nth(2, SplitResp),
    Timelist= string:tokens(Time, ":"),
    case Timelist of
         ["error"] ->
              ct:log("Could not send snmp command to manager"),
              error;
         _->
            TimeTuple = list_to_tuple([list_to_integer(X) || X <- Timelist]),
              case ((TimeTuple >= ATime) and (TimeTuple =< BTime)) of 
                   true -> case re:run(Msg, StringMatch, [global]) of
                                {match, _} ->
                                    {ok, TimeTuple}; 
                                nomatch -> 
                                    ct:log("Failed to find a match"), % try last 4 lines if HB message arrived
                                    check_log({A, B}, LogFile, StringMatch, Config, integer_to_list(list_to_integer(N) + 2), Acc -1)    
                            end;
                   false -> ct:log("Failed to find a match"), 
                            error
             end
    end.

get_time_from_recordlog([], Acc) ->
    Acc;
get_time_from_recordlog([Log | Rest], Acc) ->
    {trapLogMessage,{Date,Time},_,_,_,_,_} = Log,
    get_time_from_recordlog(Rest, [{Date,Time} | Acc]).

receive_ssh_message() ->
    receive 
        {_, _, {data, _, _, Msg}} ->
              binary_to_list(Msg);
         _Other ->
              receive_ssh_message()
    after 
        10000 -> error
    end.

check_response(Msg, Expected) when Msg =:= Expected ->
    ct:log("Snmpget response ~p \n", [Msg]);
check_response(_, _) ->
    ct:log("Could not send snmp get "),
    error.

check_snmp_running(Port, ExpectedResult) ->
    case is_target() of
        true ->
            ExpectedResult = is_snmp_running(Port);
        false ->
            ct:log("No support for DTLS on SIM env~n")
    end.

%% Chech if snmp process is started
is_snmp_running(Port) ->
    ok = rct_ssh:connect(ssh),
    Result = 
    case rct_ssh:exec(ssh, "netstat -nlp | grep " ++ erlang:integer_to_list(Port), 5000, "(.*?:" ++ erlang:integer_to_list(Port) ++ ")", [global, {capture, all, list}]) of
        {ok,[]} ->
            % this should never be matched because of reference("0.0.0.0:Port") in filter options
            false;
        {ok, _List} ->
            % if list is matched with "0.0.0.0:Port"
            true;
        {error,[]} ->
            % if list is not matched with "0.0.0.0:Port"
            false
    end,
    rct_ssh:disconnect(ssh),
    Result.
check_snmp_dtls_enabled(Expected) ->
    Expected = rct_rpc:call(rpc, comSnmpDtlsConfig, is_snmp_dtls_enabled, [], 10000).

% Check if target or SIM
is_target() ->
    case os:getenv("SIM_OR_TARGET") of
    "sim" ->
        false;
    _Target -> %% can be target or cloudish
        true 
    end.

wait_for_com_to_restart(ComPid)->
    wait_for_com_to_restart(ComPid, 60000).

wait_for_com_to_restart(_ComPid, Timeout) when Timeout < 3000 ->
    ct:fail("New COM process not started up within max timeout after restart.");

wait_for_com_to_restart(ComPid, Timeout)->
    NewComPid = wait_for_com_to_start(),
    %% ct:pal("NewComPid: ~p",[NewComPid]),
    case ComPid == NewComPid of
        true ->
        timer:sleep(2000),
        wait_for_com_to_restart(ComPid, Timeout-2000);
        false ->
        timer:sleep(2000),
        NewComPid
    end.

wait_for_com_to_start() ->
    wait_for_com_to_start(90000).

wait_for_com_to_start(Timeout) when Timeout < 6000 ->
    ct:fail("COM not started within max timeout.");

wait_for_com_to_start(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 1000)  of
    [] ->
        timer:sleep(5000),
        wait_for_com_to_start(Timeout - 5000);
    {badrpc,_} -> %{badrpc, timeout | nodedown}
        timer:sleep(5000),
        wait_for_com_to_start(Timeout - 5000);
    Com ->
        %% remove \n from the received list.
        [ComPid|_] = string:tokens(Com, "\n "),
        ComPid
    end.

waitForNodeUp() ->
    waitForNodeUp(600000).

waitForNodeUp(Timeout) when Timeout < 0 ->
    ct:fail("The node is still down");

waitForNodeUp(Timeout) ->
    timer:sleep(5000),
    case rct_rpc:call(rpc, appmServer, get_apps, [], 10000) of
        {error, _Reason} ->
            waitForNodeUp(Timeout - 5000);
        {badrpc, _Reason} ->
            waitForNodeUp(Timeout - 5000);
        _ ->
            ct:pal("The node is up!")
    end.


type_to_mo(all) ->
    "SnmpTarget";
type_to_mo(v1) ->
    "SnmpTargetV1";
type_to_mo(v2c) ->
    "SnmpTargetV2C";
type_to_mo(v3) ->
    "SnmpTargetV3";
type_to_mo(v3dtls) ->
    "SnmpTargetV3Dtls".


%%%%------------------------------------
check_def_log(NodeIp, DateTime, StringMatch, Config) ->
	FormatDateTime = convert_time(DateTime), %from tuple to string {{2017,11,8},{15,16,20}} -> "2017-11-08 15:16:20"
	Cmd = "grep -a -A 100 '" ++ FormatDateTime ++ "' " ++ "/var/log/traps.log",
	ConRef = ?config(connection_reference, Config),
    {ok, ChannelId} = ssh_connection:session_channel(ConRef, infinity),
    success = ssh_connection:exec(ConRef, ChannelId, Cmd, 10000),
	Msg = receive_ssh_message(),
    ok = ssh_connection:close(ConRef, ChannelId),
    List = test(Msg),
	
	lists:filter(fun(X) -> case {X#trapLogMessage.from, X#trapLogMessage.date_time} of
								             {{NodeIp,_}, _}-> 
								            	 Content = X#trapLogMessage.content,
						                         IsMember = [Z || Y <- Content,  
														     Z = case Y#trapVariable.value of
										                             StringMatch -> true;
								                                      _-> false
						                                        end],  
								                 case IsMember of
									                  [true] -> true;
									                 _-> false
								                 end;
											_-> false
						         	  end end, List).


get_traps_value(Traps, Oid, N) ->
	case lists:flatlength(Traps) of
		N -> get_trap_value(lists:nth(N, Traps), Oid);
		_-> error
	end.

get_trap_value(Trap, Oid) ->
	Content = Trap#trapLogMessage.content,
	[ Y#trapVariable.value || Y <- Content, Y#trapVariable.oid =:= Oid ].

filter_traps(NodeIp, Timestamp, FromDate, ToDate, Oid, Config) ->
    Traps = check_def_log(NodeIp, Timestamp, Oid, Config),

    lists:filter(fun(X) -> case X#trapLogMessage.date_time >= FromDate of
                               true when X#trapLogMessage.date_time =<ToDate ->
                                   true;
                               _ -> false
                           end
                 end, Traps).

heartbeat_interval_check([], _HBInterval) ->
    ok;
heartbeat_interval_check([_Timing1, _Timing1], _HBInterval) ->
    ok;
heartbeat_interval_check([{_Date1, _Time1}, {_Date1, Time1}, {Date2, Time2} | Rest], HBInterval) ->
    HB = list_to_integer(HBInterval),
    Difference= calendar:time_to_seconds(Time1) - calendar:time_to_seconds(Time2),
    if
        Difference > HB * 1.05 ->
           ct:fail("Second heartbeat received too late: ~p s", [Difference]);
        Difference < HB * 0.95 ->
           ct:fail("Second heartbeat received too soon: ~p s", [Difference]);
        true ->
            heartbeat_interval_check([{Date2, Time2} | Rest], HBInterval)
    end.

%% @hidden
%% cli_connect() ->
%%     %% 10 min
%%     cli_connect(600000).
%% 
%% %% @hidden
%% cli_connect(Timeout) when Timeout < 0 ->
%%     ct:fail("Could not setup a COMCLI connection within max timeout.");
%% cli_connect(Timeout) ->
%%     case rct_cli:connect(?CLI) of
%% 	ok ->
%% 	    ok;
%% 	{error, _} ->
%% 	    timer:sleep(5000),
%%             cli_connect(Timeout-5000)
%%     end.


rpc_call(M, F, A) ->
    rpc_call(?TNODE, M, F, A).


rpc_call(TNode, M, F, A) ->
    rpc_call(TNode, M, F, A, ?RPC_CALL_TIMEOUT).


rpc_call(TNode, M, F, A, Timeout) ->
    rct_rpc:call(TNode, M, F, A, Timeout).

ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn,
			    _EriChangeIPAddressAckAttributeName,
			    EriChangeIPAddressAckAttributeValue, Config) ->
      Res2 = rct_mo_handler_lib:set_mo(?config(handler, Config), re:replace(EriChangeIPAddressAckFdn, "[^A-Za-z1-9=,]", "", [global, {return, list}]),
									    [{ipAddressChangeStatus, EriChangeIPAddressAckAttributeValue}]),
      ct:pal("Set MO result: ~p~n", [Res2]),
     
      timer:sleep(5000),
	  ok.

set_oam_ref(IpProto) ->
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(oam, IpProto), oam).
    
set_alt_oam_ref(IpProto) ->
    ok = ftpes_test_lib:edit_oam_ref(ftpes_test_lib:get_oam_addr_ref(alt_oam, IpProto), alt_oam).

%% @hidden
cold_restart() ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ct:pal("Cold restart", []),
    rpc_call(swmI, force_auto_backup, []),
    rpc_call(appmI, restart_node, [cold, "ManualRestart"]),
    wait_for_node_state(ErlNode, down),
    wait_for_node_state(ErlNode, up),
    ok.

wait_for_node_state(Node, ExpState) ->
    wait_for_node_state(Node, ExpState, 600000). %% 10 min

wait_for_node_state(Node, ExpState, Timeout) when Timeout < 0 ->
    ct:pal("ErlNode state not expected : ~p, ~p", [Node, ExpState]),
    ct:fail("Node state not expected within max timeout.");

wait_for_node_state(Node, down, Timeout) ->
    case net_adm:ping(Node) of
        pang ->
	    net_kernel:disconnect(Node),
	    ct:pal("Node is down"),
	    timer:sleep(5000);
	pong ->
	    net_kernel:disconnect(Node),
            timer:sleep(5000),
            wait_for_node_state(Node, down, Timeout-5000)
    end;
wait_for_node_state(Node, up, Timeout) ->
    case net_adm:ping(Node) of
        pang ->
            net_kernel:disconnect(Node),
            timer:sleep(5000),
            wait_for_node_state(Node, up, Timeout-5000);
        pong ->
            net_kernel:disconnect(Node),
            ct:pal("Node is up"),
            timer:sleep(60000)
    end.

%-----------------------------------------------------------------
% Check log
%-----------------------------------------------------------------
test(RawLogMessages) ->
	SeparatedTrapMessages = seaparate_trap_messages(RawLogMessages),
	lists:map(fun(TrapMessage) ->
					TrapMessageRecord = parse_trap_message(TrapMessage),
					format_trap_record(TrapMessageRecord),
					TrapMessageRecord
				  end,
				  SeparatedTrapMessages).
		

seaparate_trap_messages(TrapMessages) ->
	SeparatedTrapMessages = string:tokens(TrapMessages, [$\n]),
	join_head_body(SeparatedTrapMessages, []).

join_head_body([], Acc) ->
	Acc;
join_head_body([Head, Body | Rest], Acc) ->
	join_head_body(Rest, Acc ++ [Head ++ "\n" ++ Body]).
	
%% Trap message structure:
%% YYYY-MM-DD HH:mm:ss Hostname [Protocol: [NodeIpAddress]:Port->[TrapReceiver]:SnmpPort]:\n
%% OID1 = Type1: Value1\tOID2 = Type2: Value2\t....OIDN = TypeN: ValueN\n

parse_trap_message(Message) ->
	[Header, Body] = string:tokens(Message, [$\n]),
	{ok, DateTimePattern} = re:compile("([0-9]+)-([0-9]+)-([0-9]+)\s([0-9]+):([0-9]+):([0-9]+)"),
	{ok, AddressPattern} = re:compile("([A-Z]+):\\s\\[(.*?)\\]:([0-9]+)->\\[(.*?)\\]:([0-9]+)"),
	{ok, FullPattern} = re:compile("([0-9]+-[0-9]+-[0-9]+\\s[0-9]+:[0-9]+:[0-9]+)\\s(.*?)\\s\\[(.*)\\]:"),
	
	{match, [_, DateTime, HostName, IpAddrInfo]} = re:run(Header, FullPattern, [{capture, all, list}]),
	{match, [_, Year, Month, Day, Hour, Minute, Second]} = re:run(DateTime, DateTimePattern, [{capture, all, list}]),
	
	TrapLogMsg =
			  #trapLogMessage{date_time = {{list_to_integer(Year),
											list_to_integer(Month),
											list_to_integer(Day)}, 
										   {list_to_integer(Hour),
										    list_to_integer(Minute),
											list_to_integer(Second)}},
							  hostname = HostName},
	TrapLogMsg2 =		
	case re:run(IpAddrInfo, AddressPattern, [{capture, all, list}]) of
	{match, [_, Protocol, SourceIp, SourcePort, DestIp, DestPort]} ->
		TrapLogMsg#trapLogMessage{protocol = string_to_proto(Protocol),
								  from = {SourceIp, list_to_integer(SourcePort)},
								  to = {DestIp, list_to_integer(DestPort)}};
	_ ->
		TrapLogMsg
	end,
	
	%%TODO
	Entries = parse_trap_entries(Body),
	
	TrapLogMsg2#trapLogMessage{content = Entries}.
	
parse_trap_entries(MessageBody) ->
	Entries = string:tokens(MessageBody, [$\t]), %% tab separated values
	{ok, EntryPattern} = re:compile("(.*?)=((.*?): )?(.*)"),
	lists:map(fun(Entry) ->
				{match, [_, OID, _, Type, Value]} = re:run(Entry, EntryPattern, [{capture, all, list}]),
				#trapVariable{oid = string:strip(OID), type = string:strip(Type), value = string:strip(Value)}
			  end,
			  Entries). 
	
string_to_proto("UDP") ->
	udp;
string_to_proto("DTLSUDP") ->
	dtlsudp;
string_to_proto(Other) ->
	{unknown, list_to_atom(Other)}.
	
%% get_oid_value(Oid, #trapLogMessage{content = Entries}) ->
%% 	lists:filter(fun(#trapVariable{oid = OID}) ->
%% 					Oid =:= OID
%% 				 end,
%% 				 Entries).
						
	
format_trap_record(#trapLogMessage{date_time = DateTime,
								   hostname = Hostname,
								   protocol = Protocol,
								   from = From,
								   to = To,
								   content = Entries}) ->
    log("Trap received: ~p~n", [DateTime]),
	log("Sender hostname: ~p~n", [Hostname]),
	log("Transport protocol: ~p~n", [Protocol]),
	log("Sender IP address and port: ~p~n", [From]),
	log("Receiver IP address and port: ~p~n", [To]),
	lists:foreach(fun(#trapVariable{oid = OID, type = Type, value = Value}) ->
					log("Log entry:[OID: ~p, Type: ~p, Value: ~p]~n", [OID, Type, Value])
				  end,
				  Entries).
				  
log(Format, Args) ->
	ct:log(Format, Args).  

check_zero(Hour) ->
   case Hour < 10 of
		true ->  "0"++integer_to_list(Hour);
		_-> integer_to_list(Hour)
	end.

convert_time(Time) ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = Time,
    DateTime = integer_to_list(Year) ++ "-" ++ check_zero(Month) ++ "-" ++ check_zero(Day) ++ " " ++ check_zero(Hour) ++ ":" ++
		       check_zero(Min) ++ ":" ++  check_zero(Sec),
	DateTime.
	
get_start_time(Config) ->
	Cmd = "tail -n 2 /var/log/traps.log",
	ConRef = ?config(connection_reference, Config),
    {ok, ChannelId} = ssh_connection:session_channel(ConRef, infinity),
    success = ssh_connection:exec(ConRef, ChannelId, Cmd, 10000),
    Msg = receive_ssh_message(),
	[Start] = test(Msg),
	Time = Start#trapLogMessage.date_time, 
	Time.


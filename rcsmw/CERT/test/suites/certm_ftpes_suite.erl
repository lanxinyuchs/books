%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certm_ftpes_suite.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/6
%%% 
%%% @doc == Test Suite for testing Certificate management operations via FTPES==
%%% Use case numbering refers to the ECIM Certificate Management Use
%%% Case Description 10/155 56-FAE 151 01 Uen Rev A 2013-06-19
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.  
%%% <br/><br/> rct_netconf is used in ct_hooks: see <a
%%% href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end

-module(certm_ftpes_suite).
-vsn('/main/R9A/6').
-author('ekurnik').

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
%%% -----      ---------  --------    ------------------------
%%% R9A/1   2017-02-03    ekurnik    Created
%%% R9A/2   2017-02-16    ekurnik    Minor fixes
%%% R9A/3   2017-02-22    estjako    Commented out some tests & small changes in tests
%%% R9A/4   2017-03-01    estjako    New group added
%%% R9A/6   2017-03-14    ekurnik    Changed ct:pal to ct:log
%%% ----------------------------------------------------------
%%% 

-include_lib("common_test/include/ct.hrl").

-export([suite/0,
     init_per_suite/1,
     end_per_suite/1,
     init_per_group/2,
     end_per_group/2,
     init_per_testcase/2,
     end_per_testcase/2,
     all/0,
     groups/0]).


-export([
     install_trusted_certificate/1,
     install_trusted_certificate_wrong_uri/1,
     install_trusted_certificate_wrong_password/1,
     install_trusted_certificate_cancel/1,
     
     install_trusted_der_certificate/1,
     install_trusted_der_certificate_wrong_uri/1,
     install_trusted_der_certificate_wrong_password/1,
     install_trusted_der_certificate_cancel/1,
     
     csr_initial_offline_enrollment/1,
     csr_initial_offline_enrollment_wrong_uri/1,
     csr_initial_offline_enrollment_wrong_password/1,
     csr_initial_offline_enrollment_cancel/1,
     
     csr_initial_offline_enrollment_file/1,
     csr_initial_offline_enrollment_file_wrong_uri/1,
     csr_initial_offline_enrollment_file_wrong_password/1,
     csr_initial_offline_enrollment_file_cancel/1,
     
     install_csr_credential_from_uri/1,
     install_csr_credential_from_uri_wrong_password/1,
     install_csr_credential_from_wrong_uri/1,
     install_csr_credential_from_uri_cancel/1,
     
     install_pkcs12_from_uri/1,
     install_pkcs12_from_uri_wrong_uri/1,
     install_pkcs12_from_uri_wrong_password/1,
     install_pkcs12_from_uri_cancel/1
    ]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, install_trusted_cert_group},
     {group, csr_offline_enrollment_group},
     {group, install_credential_group}].

sim() ->
    [{group, install_trusted_cert_group},
     {group, install_credential_group}].

install_trusted_cert() ->
    [install_trusted_certificate,
     install_trusted_certificate_wrong_uri,
     install_trusted_certificate_wrong_password,
     install_trusted_certificate_cancel,
     
     install_trusted_der_certificate
%%      install_trusted_der_certificate_wrong_uri,
%%      install_trusted_der_certificate_wrong_password,
%%      install_trusted_der_certificate_cancel
    ].

csr_offline_enrollment() ->
    [csr_initial_offline_enrollment,
     csr_initial_offline_enrollment_wrong_uri,
     csr_initial_offline_enrollment_wrong_password,
     csr_initial_offline_enrollment_cancel,
     
     csr_initial_offline_enrollment_file,
     csr_initial_offline_enrollment_file_wrong_uri
%%      csr_initial_offline_enrollment_file_wrong_password,
%%      csr_initial_offline_enrollment_file_cancel
    ].

install_credential() ->
    [install_csr_credential_from_uri,
     install_csr_credential_from_uri_wrong_password,
     install_csr_credential_from_wrong_uri,
     install_csr_credential_from_uri_cancel,
     
     install_pkcs12_from_uri
%%      install_pkcs12_from_uri_wrong_uri,
%%      install_pkcs12_from_uri_wrong_password,
%%      install_pkcs12_from_uri_cancel
    ].


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
         {rct_rpc, rpc_1},
         {rct_netconf,{nc1, html}},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging,
                     {all,
                         [{erlang,
                                 {["ERROR REPORT", "CRASH REPORT"], 
                                  ["mfa: {ftpI,write_file,",
                                   "error: {error,efnamena}",
                                   "certServer: throw faulty_uri",
                                   "certServer: throw ftp_timeout"]}}]}},
          ftpes_test_lib:ftpes_hook(true)
    ]}].

%% @hidden
init_per_suite(Config) ->

    %% This is because due to errors in COM there is no proper way to
    %% read the set managedElementId if it has changed from "1"
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
    case proplists:get_value(networkManagedElementId, MeData) of
        undefined ->
        "1";
        NMEI -> NMEI
    end,
    
    rct_ftpes_client:open(),
    ftpes_test_lib:create_test_folder("CERT"),
    {ok, Folder} = rct_ftpes_client:pwd(),
    
    %% enable ftp over tls on the node
    ftpes_test_lib:start_server(rpc_1),
    NewConfig = ftpes_test_lib:initialize_nc_tc(Config),
    ftpes_test_lib:enable_ftpes_tls(NewConfig),
    
    FilePath = Folder ++ "/",
    rct_ftpes_client:close(),

    DataDir = string:strip(proplists:get_value(data_dir, Config), right, $/),
    CertmDataDir = filename:join(filename:dirname(DataDir), "certm_SUITE_data/"),
    
    [{meId, MeId}, {ftp_protocol, ftpes}, {file_path, FilePath}, {certm_data_dir, CertmDataDir} | NewConfig].
%% @hidden
end_per_suite(Config) ->
    rct_ftpes_client:open(),
    rct_ftpes_client:rmdir(?config(file_path, Config)),
    ftpes_test_lib:disable_ftpes_tls(),
    ftpes_test_lib:clean_nc_tc(Config),
    ok.

init_per_group(install_trusted_cert_group, Config) ->
    rct_ftpes_client:open(),
    copy_file("test_tc.pem", Config),
    copy_file("etxasta.der", Config),
    rct_ftpes_client:close(),
    Config;

init_per_group(install_credential_group, Config) ->
    rct_ftpes_client:open(),
    copy_file("client_syslog.p12", Config),
    copy_file("signed_cert.pem", Config),
    rct_ftpes_client:close(),
    Config;

init_per_group(_Group, Config) ->
    Config.

end_per_group(install_trusted_cert_group, Config) ->
    rct_ftpes_client:open(),
    delete_file("test_tc.pem", Config),
    delete_file("etxasta.der", Config),
    rct_ftpes_client:close(),
    Config;

end_per_group(csr_offline_enrollment_group, Config) ->
    rct_ftpes_client:open(),
    delete_file("test.csr", Config),
    delete_file("csr_enrollment_ftpes.csr", Config),
    rct_ftpes_client:close(),
    Config;

end_per_group(install_credential_group, Config) ->
    rct_ftpes_client:open(),
    delete_file("client_syslog.p12", Config),
    delete_file("signed_cert.pem", Config),
    rct_ftpes_client:close(),
    Config;
    
end_per_group(_Group, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TC, Config) ->
    init_per_testcase_ftp(Config).

end_per_testcase(_TestCase, _Config) ->
    ok.

%% generic init_per_tc for sftp/ftpes TCs
init_per_testcase_ftp(Config) ->
    FtpProtocol = ?config(ftp_protocol, Config),
    [{host, Host},{username, Username},{password, Password}] = 
        ftpes_test_lib:get_ftp_config(FtpProtocol),
    Uri = atom_to_list(FtpProtocol) ++ "://"++Username++"@"++Host,
    [{uri, Uri}, {password, Password} | Config].

%% @hidden
groups() ->
    AllGroup = all(),
    InstallTc = install_trusted_cert(),
    CsrOfflineEnroll = csr_offline_enrollment(),
    InstallCred = install_credential(),
    Sim = sim(),
    [
     {default_group, [], AllGroup},
     {install_trusted_cert_group, [], InstallTc},
     {csr_offline_enrollment_group, [], CsrOfflineEnroll},
     {install_credential_group, [], InstallCred},
     {sim_group, [], Sim}
    ].
%%%--------------------------------------------------------------------
%%% TEST CASES
%%%--------------------------------------------------------------------


%%%--------------------------------------------------------------------
%%% @doc Tests for offline enrollment with file and direcory parameters
%%% @end
%%%--------------------------------------------------------------------
csr_initial_offline_enrollment_file_cancel(Config) ->
    FilePath = ?config(file_path, Config),
    NewConfig = lists:keyreplace(file_path, 1, Config, 
                 {file_path, filename:join(FilePath, "test.csr")}),
    csr_initial_offline_enrollment_cancel(NewConfig).

csr_initial_offline_enrollment_file_wrong_password(Config) ->
    FilePath = ?config(file_path, Config),
    NewConfig = lists:keyreplace(file_path, 1, Config, 
                 {file_path, filename:join(FilePath, "test.csr")}),
    csr_initial_offline_enrollment_wrong_password(NewConfig).

csr_initial_offline_enrollment_file_wrong_uri(Config) ->
    NewConfig = lists:keyreplace(file_path, 1, Config, {file_path, "/not/a/dir/test.csr"}),
    csr_initial_offline_enrollment("FAILURE", "FINISHED", NewConfig).

csr_initial_offline_enrollment_file(Config) ->
    FilePath = ?config(file_path, Config),
    NewConfig = lists:keyreplace(file_path, 1, Config, 
                 {file_path, filename:join(FilePath, "test.csr")}),
    csr_initial_offline_enrollment(NewConfig).

csr_initial_offline_enrollment_cancel(Config) ->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ ":9921"}), %% set wrong port
    csr_initial_offline_enrollment("SUCCESS", "CANCELLED", [{cancel, true} | NewConfig]).

csr_initial_offline_enrollment_wrong_password(Config) ->
    NewConfig = lists:keyreplace(password, 1, Config, {password, "notapassword"}),
    NewConfig2 = [{cancel, true}| NewConfig],
    csr_initial_offline_enrollment("NOT_AVAILABLE", "CANCELLED", NewConfig2).

csr_initial_offline_enrollment_wrong_uri(Config) ->
    NewConfig = lists:keyreplace(file_path, 1, Config, {file_path, "/not/a/dir/"}),
    csr_initial_offline_enrollment("FAILURE", "FINISHED", NewConfig).

csr_initial_offline_enrollment(Config) ->
    csr_initial_offline_enrollment("SUCCESS", "FINISHED", Config).

csr_initial_offline_enrollment(ExpectedResult, ExpectedState, Config) ->
    Uri = ?config(uri, Config),
    MeId = ?config(meId, Config),
    FilePath = ?config(file_path, Config),
    Password = ?config(password, Config),
    Cancel = proplists:get_value(cancel, Config, false),
    NcId = "enrollment_ftpes",

    create_node_credential(NcId, Config),
    ProgressFilter = get_enrollmentProgress_filter(NcId, MeId),
    ActionId = get_action_id(ProgressFilter),
    ct:pal("OldActionId = ~p~n",[ActionId]),
    
    Action = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"},
           {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
          [{secMId,[],["1"]},
           {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
            [{certMId,[],["1"]},
             {'NodeCredential', 
              [{nodeCredentialId, [NcId]},
               {startOfflineCsrEnrollment, 
            [{uri, [Uri ++ FilePath]},
             {uriPassword, [Password]}]}]}]}]}]}]},
    
    ct:pal("Action startOfflineCsrEnrollment~n",[]),
    case netconf(action, [nc1, Action]) of
    {ok, _} ->
        ok;
    Error2 ->
        ct:pal("startOfflineCsrEnrollment action failed: ~p~n",
           [Error2]),
        ct:fail(Error2)
    end,

    if Cancel ->
           timer:sleep(20000),
           nc_cancel(NcId, MeId);
       true ->
           ok
    end,
    
    enrollmentProgress(ExpectedResult, ExpectedState, ActionId, MeId, NcId),
    
    remove_node_credential(NcId, Config),

    ok.


%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------

install_csr_credential_from_uri_cancel(Config) ->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ ":9921"}), %% set wrong port
    install_csr_credential_from_uri("SUCCESS", "CANCELLED", [{cancel, true} | NewConfig]).

install_csr_credential_from_uri_wrong_password(Config) ->
    NewConfig = lists:keyreplace(password, 1, Config, {password, "notapassword"}),
    NewConfig2 = [{cancel, true}| NewConfig],
    install_csr_credential_from_uri("NOT_AVAILABLE", "CANCELLED", NewConfig2).

install_csr_credential_from_wrong_uri(Config) ->
    NewConfig = lists:keyreplace(file_path, 1, Config, {file_path, "/not/a/dir/"}),
    install_csr_credential_from_uri("FAILURE", "FINISHED", NewConfig).

install_csr_credential_from_uri(Config) ->
    install_csr_credential_from_uri("SUCCESS", "FINISHED", Config).

install_csr_credential_from_uri(ExpectedResult, ExpectedState, Config) ->
    ct:pal("Action installCredentialFromUri~n",[]),
    Uri = ?config(uri, Config),
    MeId = ?config(meId, Config),
    FilePath = ?config(file_path, Config),
    UriPwd = ?config(password, Config),
    NcId = "csr_from_uri_ftpes",
    Cancel = proplists:get_value(cancel, Config, false),
    
    create_node_credential(NcId, Config),
    ProgressFilter = get_enrollmentProgress_filter(NcId, MeId),
    ActionId = get_action_id(ProgressFilter),
    ct:pal("OldActionId = ~p~n",[ActionId]),

    File = "signed_cert.pem",
    CredPwd     = "true", %% FIXME should be NULL
    FingerPrint =
    "7E:17:89:C7:9C:AD:C2:BC:04:68:AE:70:55:B3:21:71:68:9E:08:BF",
    
    FullUri = Uri ++ filename:join(FilePath, File),
    ct:pal("Uri: ~p~n", [FullUri]),
    Install = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"},
           {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
          [{secMId,[],["1"]},
           {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
            [{certMId,[],["1"]},
             {'NodeCredential', 
              [{nodeCredentialId, [NcId]},
               {installCredentialFromUri, 
            [{uri, [FullUri]},
                 {uriPassword, [UriPwd]},
                 {credentialPassword, [CredPwd]},
                 {fingerprint, [FingerPrint]}]}]}]}]}]}]},
    
    ct:pal("Action installCredentialFromUri~n",[]),
    case  netconf(action, [nc1, Install]) of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("installCredentialFromUri action failed: ~p~n",
           [Error]),
        ct:fail(Error)
    end,
    
    if Cancel ->
           timer:sleep(20000),
           nc_cancel(NcId, MeId);
       true ->
           ok
    end,
    
    enrollmentProgress(ExpectedResult, ExpectedState, ActionId, MeId, NcId),
    
    remove_node_credential(NcId, Config),
    ok.


%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
install_pkcs12_from_uri_cancel(Config) ->
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ ":9921"}), %% set wrong port
    install_pkcs12_from_uri("SUCCESS", "CANCELLED", [{cancel, true} |NewConfig]).

install_pkcs12_from_uri_wrong_password(Config) ->
    NewConfig = lists:keyreplace(password, 1, Config, {password, "notapassword"}),
    NewConfig2 = [{cancel, true}| NewConfig],
    install_pkcs12_from_uri("NOT_AVAILABLE", "CANCELLED", NewConfig2).

install_pkcs12_from_uri_wrong_uri(Config)->
    NewConfig = lists:keyreplace(file_path, 1, Config, {file_path, "/not/a/dir/"}),
    install_pkcs12_from_uri("FAILURE", "FINISHED", NewConfig).

install_pkcs12_from_uri(Config) ->
    install_pkcs12_from_uri("SUCCESS", "FINISHED", Config).

install_pkcs12_from_uri(ExpectedResult, ExpectedState, Config) ->
    Uri = ?config(uri, Config),
    MeId = ?config(meId, Config),
    FilePath = ?config(file_path, Config),
    UriPwd = ?config(password, Config),
    NcId = "pkcs12_ftpes",
    Cancel = proplists:get_value(cancel, Config, false),
    
    create_node_credential(NcId, Config),
    ProgressFilter = get_enrollmentProgress_filter(NcId, MeId),
    ActionId = get_action_id(ProgressFilter),
    ct:pal("OldActionId = ~p~n",[ActionId]),
    
    ct:pal("Action installCredentialFromUri~n",[]),

    CredPwd     = "test",
    FingerPrint = % test_cert_ext.pkcs12
    "34:57:11:ed:ae:8e:19:22:4d:d0:e5:8e:83:57:1f:73:f6:97:e0:c6",
    File = "client_syslog.p12",
    FullUri = Uri ++ filename:join(FilePath, File),
    ct:pal("Uri: ~p~n", [FullUri]),
    Install = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"},
           {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
          [{secMId,[],["1"]},
           {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
            [{certMId,[],["1"]},
             {'NodeCredential', 
              [{nodeCredentialId, [NcId]},
               {installCredentialFromUri, 
            [{uri, [FullUri]},
                 {uriPassword, [UriPwd]},
                 {credentialPassword, [CredPwd]},
                 {fingerprint, [FingerPrint]}]}]}]}]}]}]},
    
    ct:pal("Action installCredentialFromUri PKCS#12~n",[]),
    case  netconf(action, [nc1, Install]) of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("installCredentialFromUri action failed: ~p~n",
           [Error]),
        ct:fail(Error)
    end,
    
    if Cancel ->
           timer:sleep(20000),
           nc_cancel(NcId, MeId);
       true ->
           ok
    end,

    enrollmentProgress(ExpectedResult, ExpectedState, ActionId, MeId, NcId),
    remove_node_credential(NcId, Config),
    ok.


%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
install_trusted_certificate(Config) ->
    Fingerprint = get_fingerprint("test_tc.pem", Config),
    NewConfig = [{fp, Fingerprint} | Config],
    install_trusted_certificate("SUCCESS", "FINISHED", NewConfig).

install_trusted_certificate_cancel(Config) ->
    Fingerprint = get_fingerprint("test_tc.pem", Config),
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ ":9921"}), %% set wrong port
    NewConfig2 = [{fp, Fingerprint}, {cancel, true} | NewConfig],
    install_trusted_certificate("NOT_AVAILABLE", "CANCELLED", NewConfig2).

install_trusted_certificate_wrong_password(Config) ->
    Fingerprint = get_fingerprint("test_tc.pem", Config),
    NewConfig = lists:keyreplace(password, 1, Config, {password, "notapassword"}),
    NewConfig2 = [{fp, Fingerprint}, {cancel, true}| NewConfig],
%% we cancel after 20 seconds due to long execution time without cancel
    install_trusted_certificate("NOT_AVAILABLE", "CANCELL",NewConfig2).

install_trusted_certificate_wrong_uri(Config) ->
    Fingerprint = get_fingerprint("test_tc.pem", Config),
    NewConfig = lists:keyreplace(file_path, 1, Config, {file_path, "/not/a/dir"}),
    NewConfig2 = [{fp, Fingerprint}| NewConfig],
    install_trusted_certificate("FAILURE", "FINISHED",NewConfig2).

%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
install_trusted_der_certificate(Config) ->
    Fingerprint = get_der_fingerprint("etxasta.der", Config),
    NewConfig = [{fp, Fingerprint}, {file, "etxasta.der"} | Config],
    install_trusted_certificate("SUCCESS", "FINISHED",NewConfig).

install_trusted_der_certificate_cancel(Config) ->
    Fingerprint = get_der_fingerprint("etxasta.der", Config),
    Uri = ?config(uri, Config),
    NewConfig = lists:keyreplace(uri, 1, Config, {uri, Uri ++ ":9921"}), %% set wrong port
    NewConfig2 = [{fp, Fingerprint}, {file, "etxasta.der"}, {cancel, true} | NewConfig],
    install_trusted_certificate("NOT_AVAILABLE", "CANCELLED", NewConfig2).

install_trusted_der_certificate_wrong_password(Config) ->
    Fingerprint = get_der_fingerprint("etxasta.der", Config),
    NewConfig = lists:keyreplace(password, 1, Config, {password, "notapassword"}),
    NewConfig2 = [{fp, Fingerprint}, {file, "etxasta.der"}, {cancel, true}  | NewConfig], 
    %% we cancel after 20 seconds due to long execution time without cancel
    install_trusted_certificate("NOT_AVAILABLE", "CANCELLED",NewConfig2).

install_trusted_der_certificate_wrong_uri(Config) ->
    Fingerprint = get_der_fingerprint("etxasta.der", Config),
    NewConfig = lists:keyreplace(file_path, 1, Config, {file_path, "/not/a/dir"}),
    NewConfig2 = [{fp, Fingerprint}, {file, "etxasta.der"} | NewConfig],
    install_trusted_certificate("FAILURE", "FINISHED",NewConfig2).

install_trusted_certificate(ExpectedResult, ExpectedState, Config) ->
    Uri = ?config(uri, Config),
    MeId = ?config(meId, Config),
    FilePath = ?config(file_path, Config),
    Password = ?config(password, Config),
    File = proplists:get_value(file, Config, "test_tc.pem"),
    Fingerprint = ?config(fp, Config),
    Cancel = proplists:get_value(cancel, Config, false),

    FullUri = Uri ++ filename:join(FilePath, File),
    ProgressFilter = get_reportProgress_filter(MeId),
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),
 
    Action = 
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
         [{secMId,[],["1"]},
          {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
           [{certMId,[],["1"]},
        {installTrustedCertFromUri, [], 
         [{uri, [FullUri]},
          {uriPassword, [Password]},
          {fingerprint, [Fingerprint]}]}]}]}]}]},
    ActionResult = netconf(action, [nc1, Action]),
    case ActionResult of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("installTrustedCertFromUri action failed: ~p~n",[Error]),
        ct:fail(Error)
    end,

    if Cancel ->
           timer:sleep(20000),
           certm_cancel(MeId);
       true ->
           ok
    end,
    report_progress(ExpectedResult, ExpectedState, ActionId, MeId),
    
    case ExpectedResult of
        "SUCCESS" ->
            TcId = integer_to_list(length(get_all_trusted_certificates(MeId))),
            remove_trusted_certificate(TcId, Config);
        _ ->
            ok
    end,
    ok.

    

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

create_node_credential(NcId, Config) ->
    MeId = proplists:get_value(meId, Config),
    KeyInfo = case rand:uniform(4)-1 of
          0 -> "RSA_1024";
          1 -> "RSA_2048";
          2 -> "RSA_3072";
          3 -> "RSA_4096";
          4 -> "ECDSA_160";
          5 -> "ECDSA_224";
          6 -> "ECDSA_256";
          7 -> "ECDSA_384";
          8 -> "ECDSA_512"
          end,
    ct:pal("Key type ~s selected.~n",[KeyInfo]),
    SubjectName = "C=SE,O=Ericsson,CN=certm_SUITE.ericsson.com",
    SubjectAltName = "123456.ericsson.com",
    Edit = {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[MeId]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
        [{secMId,[],["1"]},
         {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
          [{certMId,[],["1"]},
           {'NodeCredential', 
            [{nodeCredentialId, [NcId]},
             {userLabel, ["Created by certm_ftpes_SUITE"]},
             {subjectName, [SubjectName]},
             {subjectAltName, [SubjectAltName]},
             {keyInfo, [KeyInfo]}]}]}]}]}]},
    
    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
    ok -> ok;
    Error1 ->
        ct:fail(Error1)
    end.
    
remove_node_credential(NcId, Config) ->

    MeId = ?config(meId, Config),
    DeleteOps = [{'NodeCredential', [{'xc:operation', "delete"}], 
                  [{nodeCredentialId, [], [NcId]}]}],
           
    Edit = {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}
        ],
        [{managedElementId,[],[MeId]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
        [{secMId,[],["1"]},
         {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
          [{certMId,[],["1"]}|
           DeleteOps]}]}]}]},

    ct:log("~p~n",[Edit]),
        
    
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
    ok -> ok;
    _ ->
        ct:fail(EditRes)
    end.

remove_trusted_certificate(TcId, Config) ->
    MeId = proplists:get_value(meId, Config),
    ProgressFilter = 
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
         [{secMId,[],["1"]},
          {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
           [{certMId,[],["1"]},
        {reportProgress, []}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),
    
    MoRef = make_tc_moref(MeId, TcId),

    Action = 
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
         [{secMId,[],["1"]},
          {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
           [{certMId,[],["1"]},
        {removeTrustedCert, [], 
         [{trustedCert, [MoRef]}]}]}]}]}]},
    ActionResult = netconf(action, [nc1, Action]),
    case ActionResult of
    {ok, _} ->
        ok;
    Error ->
        ct:pal("removeTrustedCert action failed: ~p~n",[Error]),
        ct:fail(Error)
    end,
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
    {"SUCCESS", "FINISHED"} ->
        ok;
    Result ->
        ct:pal("removeTrustedCert: ~p~n",[Result]),
        ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------

cmd(Cmd) ->
    ct:log("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.

%%%--------------------------------------------------------------------
%%% Description: Make a Trusted certificate object reference from netconf data
%%%--------------------------------------------------------------------

make_tc_moref(MeId, TcId) ->
    "ManagedElement="++MeId++",SystemFunctions=1,SecM=1,CertM=1,"
    "TrustedCertificate="++TcId.

%%%--------------------------------------------------------------------
%%% Description: Read out all TrustedCertficiate MO instances
%%%--------------------------------------------------------------------

get_all_trusted_certificates(MeId) ->
    Get = {'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]},
        {'SystemFunctions',
         [{systemFunctionsId,[],["1"]},
          {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
           [{secMId,[],["1"]},
        {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
         [{certMId,[],["1"]}]}]}]}]},
    {ok, Result} = netconf(get, [nc1, Get]),
    {ok, {_, _, Contents}} = extract_element('CertM', Result),
    [Element||Element<-Contents, 
         element(1, Element) == 'TrustedCertificate'].

%%%--------------------------------------------------------------------
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(ProgressFilter) -> 
   get_action_id(reportProgress, ProgressFilter).

get_action_id(Progress, ProgressFilter) ->
    case get_progress_report_member(actionId, Progress, ProgressFilter) of
        undefined ->
            "0";
        Result ->
            Result
    end.


%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(ProgressFilter) ->
      get_result_info(reportProgress, ProgressFilter).

get_result_info(Progress, ProgressFilter) ->
    case get_progress_report_member(resultInfo, Progress, ProgressFilter) of
        undefined ->
            "";
        Result ->
            Result
    end.

%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, ProgressAttr, ProgressFilter) ->
    try
        {ok, A} = netconf(get, [nc1, ProgressFilter]),
        ct:log("ProgressFilter result: ~p~n", [A]),
        case extract_element(ProgressAttr, A) of
        {ok, {ProgressAttr, L, _}}  ->
            case lists:keyfind(unset,1, L) of
            {unset, "true"} ->
                undefined;
            _ ->
                extract_member(Member, A)
            end;
        _ ->
            extract_member(Member, A)
        end
    catch _:_ ->
        undefined
    end.
extract_member(Member, A) ->
    {ok, {Member, _, [Value]}} = 
    extract_element(Member, A),
    Value.
    
%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    
    Status = case Res of
    {error, _} ->
        Res;
    _ ->
        ct_netconfc:close_session(nc1)
    end,
    
    %% Result of negative edit requests are seen first when closing session.
    case Status of
    ok ->
        Res;
    _ ->
        Status
    end.


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
                ct:log("Waiting for updated progress ~n~p~n",[Report]),
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
            ct:log("Netconf get error:~n~p",[Error]),
            wait_for_progress(Attribute, OldActionId, ProgressFilter)
        end;
    {error, Reason} ->
        ct:log("Netconf open error:~n~p",[Reason]),
        wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    {ok, State} = extract_element(state, [Report]),
    case State of
    {state, _, [Progress]} when Progress =:= "FINISHED"
                         orelse Progress =:= "CANCELLED" ->
        format_progress_report(Report),
        ct:log("~s",[format_progress_report(Report)]),
        {ok, {result, _, [Result]}} = extract_element(result, [Report]),
        {ok, {Result, Progress}};
    {state, _, [Current]} ->
        {ok, {progressPercentage, _, [Percent]}} =
        extract_element(progressPercentage, [Report]),
        {ok, {progressInfo, _, [Info]}} = 
        extract_element(progressInfo, [Report]),
        ct:log("State: ~s ~s % ready~n~s",[Current, Percent, Info]),
        loop
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
report_progress(ExpectedResult, ExpectedState, ActionId, MeId)->
    
      ProgressFilter =  get_reportProgress_filter(MeId),

     case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
        {ExpectedResult, ExpectedState} ->
            case {get_result_info(ProgressFilter), ExpectedResult} of
                {ResultInfo, "SUCCESS"} when ResultInfo =/= ""->
                    ct:log("Reading ~p~n",[ResultInfo]),
                    TcKey = lists:last(string:tokens(ResultInfo, ",=")),
                    Get = {'ManagedElement',
                       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                       [{managedElementId,[],[MeId]},
                        {'SystemFunctions',
                         [{systemFunctionsId,[],["1"]},
                          {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                           [{secMId,[],["1"]},
                        {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                         [{certMId,[],["1"]},
                          {'TrustedCertificate', 
                           [{trustedCertificateId, [TcKey]}]}]}]}]}]},
                    %% At this stage of development we do not care of the returned
                    %% value from netconf, only that we actually succeds with the 
                    %% get operation. jotj 2013-08-12
                    {ok, _} = netconf(get, [nc1, Get]),
                    {ok, ResultInfo};
                {_, _} ->
                    {ok, undefined}
            end;
        Result ->
            ct:log("installTrustedCertFromUri: ~p~n",[Result])
        end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enrollmentProgress(ExpectedResult, ExpectedState, ActionId, MeId, NcId) -> 
    ProgressFilter = get_enrollmentProgress_filter(NcId, MeId),

    case wait_for_progress(enrollmentProgress, ActionId, ProgressFilter) of
    {ExpectedResult, ExpectedState} ->
        case {get_result_info(enrollmentProgress, ProgressFilter), ExpectedResult} of
            {ResultInfo, "SUCCESS"} when ResultInfo =/= "" ->
                ct:log("Reading ~p~n",[ResultInfo]),
                Get = {'ManagedElement',
                   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                   [{managedElementId,[],[MeId]},
                    {'SystemFunctions',
                     [{systemFunctionsId,[],["1"]},
                      {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                       [{secMId,[],["1"]},
                    {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                     [{certMId,[],["1"]},
                      {'NodeCredential', 
                       [{nodeCredentialId, [NcId]}]}]}]}]}]},
                %% At this stage of development we do not care of the returned
                %% value from netconf, only that we actually succeds with the 
                %% get operation. jotj 2013-08-12
                {ok, _} = netconf(get, [nc1, Get]),
                {ok, ResultInfo};
            {_, _} ->
                {ok, undefined}
        end;
    Result ->
        ct:log("NodeCredential action failed: ~p~n",[Result])
    end.

get_reportProgress_filter(MeId) ->
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
         [{secMId,[],["1"]},
          {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
           [{certMId,[],["1"]},
        {reportProgress, []}]}]}]}]}.

get_enrollmentProgress_filter(NcId, MeId) ->
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
         [{secMId,[],["1"]},
          {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
           [{certMId,[],["1"]},
            {'NodeCredential', 
              [{nodeCredentialId, [NcId]},
               {enrollmentProgress, []}]}]}]}]}]}.

certm_cancel(MeId) ->
    Conf =
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"},
       {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}
     ],
     [{managedElementId,[],[MeId]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
         [{secMId,[],["1"]},
          {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
           [{certMId, [], ["1"]},
        {cancel, [], []}]}]}]}]},


    ActionRes = netconf(action, [nc1, Conf]),
    case ActionRes of
    {ok, _} -> ok;
    _ ->
        ct:fail(ActionRes)
    end.

nc_cancel(NcId, MeId) ->
     Conf =
    {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}
        ],
        [{managedElementId,[],[MeId]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
        [{secMId,[],["1"]},
         {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
          [{certMId,[],["1"]},
                   {'NodeCredential',
                    [{nodeCredentialId, [NcId]},
                        {cancelEnrollment, [], []}]}]}]}]}]},

    ct:log("~p~n",[Conf]),
        

    ActionRes = netconf(action, [nc1, Conf]),
    case ActionRes of
    {ok, _} -> ok;
    _ ->
        ct:fail(ActionRes)
    end.


%%%--------------------------------------------------------------------
%%% Description: Format a AsyncActionStruct for printing
%%%--------------------------------------------------------------------


format_progress_report({reportProgress, _, Members}) ->
    [io_lib:format("reportProgress:~n",[])|format_progress_report(Members)];
format_progress_report({enrollmentProgress, _, Members}) ->
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
        


%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

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


copy_file(DefaultFile, Config) ->
    CertmDataDir = ?config(certm_data_dir, Config),
    FilePath = proplists:get_value(file_path, Config),
    File = proplists:get_value(file, Config, DefaultFile),
    Path =  filename:join(CertmDataDir, File),
    FtpesPath = filename:join(FilePath, File),
    {ok, Bin} = file:read_file(Path),
    ok = rct_ftpes_client:write_file(FtpesPath, Bin).

delete_file(DefaultFile, Config) ->
    FilePath = proplists:get_value(file_path, Config),
    File = proplists:get_value(file, Config, DefaultFile),
    FtpesPath = filename:join(FilePath, File),
    rct_ftpes_client:delete_file(FtpesPath).

get_fingerprint(DefaultFile, Config) ->
    DataDir = proplists:get_value(certm_data_dir, Config),
    File = proplists:get_value(file, Config, DefaultFile),
    Path =  filename:join(DataDir, File),
    [_, FP] = 
    string:tokens(
      cmd(["openssl x509 -fingerprint -noout -in ", Path]), "="),
    string:strip(FP, both, $\n).

get_der_fingerprint(DefaultFile, Config) ->
    DataDir = proplists:get_value(certm_data_dir, Config),
    File = proplists:get_value(file, Config, DefaultFile),
    Path =  filename:join(DataDir, File),
    [_, FP] = 
    string:tokens(
      cmd(["openssl x509 -fingerprint -noout -inform DER -in ", Path]), "="),
    string:strip(FP, both, $\n).
    


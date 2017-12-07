%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certSecCredu_suite.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/2
%%% 
%%% @doc == Test Suite for testing SnmpV3 over DTLS
%%% @end

-module(certSecCredu_suite).
-vsn('/main/R11A/R12A/2').
-date('2017-10-25').
-author('enekdav').

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
%%% R11A/1   2017-09-12   evadumb    Created
%%% R11A/2   2017-09-13   eivmiha    TC-US1-19
%%% R11A/3   2017-09-15   eivmiha    Fixed TC-US1-19
%%% R11A/4   2017-09-19   evadumb    Added initialize and finalize test cases
%%% R11A/5   2017-09-22   enekdav    Fixed initialize and finalize test cases
%%% R11A/6   2017-10-02   eivmiha    Added qualification_group
%%% R11A/7   2017-10-02   emirbos    Added NC and TCAT subscribe and unsubscribe
%%%                                  test cases
%%% R11A/8   2017-10-02   emirbos    Minor fixes
%%% R11A/9   2017-10-03   emirbos    Added TC sec_credu_tcat_subscribe_with_invalid_tcat_id
%%% R11A/10  2017-10-05   emirbos    Added TC sec_credu_nc_subscribe_with_invalid_nc_id
%%% R11A/11  2017-10-06   evadumb    Added sec_credu_trust_category_get_group, sec_credu_tcat_cert_get_group,
%%%                                  sec_credu_trust_category_free_group
%%% R11A/12  2017-10-09   edartop    Added sec_credu_nc group
%%% R11A/13  2017-10-10   enekdav    Added selection object and dispatch group, refactored test suite
%%% R11A/14  2017-10-11   enekdav    Fixed dispatch_tcat test case, joined 4 groups in one to reduce duration of tests
%%% R11A/15  2017-10-11   emirbos    Added nc_get_key cases to sec_credu_nc group
%%% R11A/16  2017-10-17   enekdav    Fixed dispatch and get_tcat_cert test cases
%%% R11A/17  2017-10-19   enekdav    Added status string test case
%%% R11A/18  2017-10-23   evadumb    Added com_restart_group
%%% R12A/1   2017-10-24   enekdav    Added SDC group
%%% R12A/2   2017-10-25   enekdav    Removed error message from logs
%%% ----------------------------------------------------------
%%% 

-include_lib("common_test/include/ct.hrl").


-export([
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         groups/0,
         all/0,
         final_block/1
        ]).

-export([
         sec_credu_initialize/1,
         sec_credu_initialize_with_nc_tc_null/1,
         sec_credu_initialize_with_version_num_null/1
        ]).

-export([
         sec_credu_finalize/1,
         sec_credu_finalize_with_empty_handle/1,
         sec_credu_finalize_with_invalid_id/1
        ]).

-export([
         sec_credu_sel_object_get/1,
         sec_credu_sel_object_with_invalid_id/1,
         sec_credu_sel_object_with_empty_handle/1
        ]).

-export([
         sec_credu_nc_subscribe_successful/1,
         sec_credu_nc_subscribe_successful_multiple/1,
         sec_credu_nc_subscribe_with_empty_handle/1,
         sec_credu_nc_subscribe_with_invalid_nc_id/1,
         sec_credu_nc_subscribe_with_invalid_subscriber_id/1,
         sec_credu_uninstalled_nc_subscribe_successful/1
        ]).

-export([
         sec_credu_nc_unsubscribe_successful/1,
         sec_credu_nc_unsubscribe_with_empty_handle/1,
         sec_credu_nc_unsubscribe_with_invalid_subscriber_id/1,
         sec_credu_nc_unsubscribe_with_invalid_subscription_id/1
        ]).

-export([
         sec_credu_tcat_get_info/1,
         sec_credu_tcat_get_info_invalid_id/1,
         sec_credu_tcat_get_info_invalid_subid/1,
         sec_credu_tcat_get_info_disabled_tc/1,

         sec_credu_tcat_get_dir_name/1,
         sec_credu_tcat_get_dir_name_null/1,
         sec_credu_tcat_get_nr_of_certs/1,
         sec_credu_tcat_get_nr_of_certs_null/1
        ]).

-export([
         sec_credu_tcat_subscribe_successful/1,
         sec_credu_tcat_subscribe_successful_multiple/1,
         sec_credu_tcat_subscribe_with_empty_handle/1,
         sec_credu_tcat_subscribe_with_invalid_tcat_id/1,
         sec_credu_tcat_subscribe_with_invalid_subscriber_id/1,
         sec_credu_disabled_tcat_subscribe_successful/1
        ]).

-export([
         sec_credu_tcat_unsubscribe_successful/1,
         sec_credu_tcat_unsubscribe_with_empty_handle/1,
         sec_credu_tcat_unsubscribe_with_invalid_subscriber_id/1,
         sec_credu_tcat_unsubscribe_with_invalid_subscription_id/1
        ]).

-export([
         sec_credu_get_tcat_cert_info/1,
         sec_credu_get_tcat_cert_info_null/1,
         sec_credu_get_tcat_cert_info_wrong_format/1,
         sec_credu_get_tcat_cert_info_wrong_index/1,
         sec_credu_get_tcat_cert_filename/1,
         sec_credu_get_tcat_cert_filename_wrong_format/1,
         sec_credu_get_tcat_cert_filename_wrong_index/1
        ]).

-export([
         sec_credu_free_memory_tcat/1,
         sec_credu_free_memory_tcat_null_handle/1
        ]).

-export([
         sec_credu_get_nc_cert/1,
         sec_credu_get_nc_cert_invalid_id/1,
         sec_credu_get_nc_cert_invalid_subscribe_id/1,
         sec_credu_get_nc_cert_uninstalled_nc/1,
         sec_credu_get_nc_empty_handle/1,
         sec_credu_get_nc_wrong_format/1,
         sec_credu_get_nc_key/1,
         sec_credu_get_nc_key_invalid_id/1,
         sec_credu_get_nc_key_invalid_subscribe_id/1,
         sec_credu_get_nc_key_uninstalled_nc/1,
         sec_credu_get_nc_key_empty_handle/1,
         sec_credu_get_nc_key_wrong_format/1
        ]).

-export([
         sec_credu_dispatch_one_nc/1,
         sec_credu_dispatch_one_tcat/1,
         sec_credu_dispatch_one_nc_and_tcat/1,
         sec_credu_dispatch_all/1,
         sec_credu_dispatch_blocking/1,
         sec_credu_dispatch_with_empty_handle/1,
         sec_credu_dispatch_with_invalid_fd/1,
         sec_credu_dispatch_with_invalid_flag/1
        ]).

-export([
         sec_credu_status_string/1
        ]).

-export([
         certSecCredu_process_running_after_node_restart/1,
         com_restart_while_initialize/1,
         com_restart_while_subd_to_nc/1,
         com_restart_while_subd_to_tcat/1,
         com_restart_while_subd_nc_tcat/1
        ]).
%% DEFINE

-type config() :: [{atom(), term()}].

-define (NC_USER, nc1).
-define (CSC, 26).       %certSecCredu
-define (MODULE_NAME,  certSecCredu).
-define (RC, 65).        %release code, "A" = 65
-define (MAJ_VER, 1).    %major version
-define (MIN_VER, 1).    %minor version
-define (ID, 1).         %unit32_t
-define (PEM, 2).
-define (FILEPATH, 1).
-define (DISPATCH_ONE, 1).
-define (DISPATCH_ALL, 2).
-define (DISPATCH_BLOCKING, 3).
-define (INVALID_FLAG, 4).

-define (SUCCESS, 0).
-define (HANDLE_NULL, 1).
-define (VERSION_NULL, 2).
-define (PARAMETERS_NULL, 3).
-define (INVALID_FD, -1).
-define (INVALID_ID, 4).
-define (INVALID_NC_ID, 5).
-define (INVALID_TCAT_ID, 6).
-define (INVALID_SUB_ID, 7).

-define (ROOT_DIR,     rct_rpc:call(rpc, sysEnv, tmp_dir, [], 10000)).
-define (SEC_CREDU_DIR,     ?ROOT_DIR ++ "/sec_credu_api/").
-define (CACERTS_DIR,  ?SEC_CREDU_DIR ++ "cacerts/").
-define (CACERTSTCAT1_DIR,  ?CACERTS_DIR ++ "tcattcat1/").
-define (CERTS_DIR,  ?SEC_CREDU_DIR ++ "certs/").
-define (PRIVATE_DIR,  ?SEC_CREDU_DIR ++ "private/").
-define (REG_PROCESS_NAME,  <<"SEC_CREDU_API">>).

-define (REQ_STATUS_STRING, 0).
-define (REQ_INITIALIZE, 1).
-define (REQ_FINALIZE, 2).
-define (REQ_SELECTION_OBJECT_GET, 3).
-define (REQ_DISPATCH, 4).
-define (REQ_NC_SUBSCRIBE, 5).
-define (REQ_NC_UNSUBSCRIBE, 6).
-define (REQ_NC_CERT_GET, 7).
-define (REQ_NC_KEY_GET, 8).
-define (REQ_TCAT_SUBSCRIBE, 9).
-define (REQ_TCAT_UNSUBSCRIBE, 10).
-define (REQ_TCAT_GET, 11).
-define (REQ_FREE_TCAT, 12).
-define (REQ_TCAT_DIR, 13).
-define (REQ_TCAT_COUNT, 14).
-define (REQ_TCAT_CERT, 15).

-define (SEC_CREDU_OK, 1).
-define (SEC_CREDU_ERR_UNSPECIFIED, 2).
-define (SEC_CREDU_ERR_SERVICE_UNAVAILABLE, 3).
-define (SEC_CREDU_ERROR_INVALID_PARAMETER, 4).
-define (SEC_CREDU_ERROR_UNSUPPORTED_FORMAT, 5).

-define (TC_CERT_EXT, ".pem").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
%% @hidden
-spec suite() -> config().

suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks,
      [{rct_htmllink,[]},
       {rct_rpc, rpc},
       {rct_logging, {all, [{erlang, {["ERROR REPORT", "CRASH REPORT"], [
                                                                         "\"ift_app\" died with Exit code 137",
                                                                         "Program ID [0-9]+ has crashed",
                                                                         "Program ID [0-9]+ has terminated",
                                                                         "has failed to start after 3 attempts within 300 seconds"
                                                                        ]}}]}},
       {rct_netconf, ?NC_USER},
       {cth_conn_log, []},
       {rct_core,[]},
       {cover_hook,[{du1, username}]},
       {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]}
      ]}
    ].


%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    Handler = {netconf, ?NC_USER},
    %% Cleanup CertM just in case
    rct_certm_lib:clear_certm_config(Handler),
    NodeCredName = "nc1",
    NodeCred2Name = "nc2",
    UriPassword = "labuser",
    CertUri = "sftp://labuser@10.68.101.131/home/labuser/snmp_dtls_certificates/nodeagent.p12",
    CertPassword = "nodeagent",
    NodeCredFingerprint = "e4:7c:12:92:a2:1f:48:bb:43:75:1a:c9:eb:a2:ce:95:7a:1a:89:35",
    TrustCatName = "tcat1",
    TrustCertFingerprint = "FA:7E:10:69:8C:11:4D:3B:50:AE:F3:C1:53:F7:85:53:46:57:BC:40",
    TrustCertUri = "sftp://labuser@10.68.101.131/home/labuser/snmp_dtls_certificates/mngr.example.com.crt",
    
    {ok, client_started} = rct_proxy:start_proxy(node1, sec_credu1, ?CSC),
    
    [{handler, Handler}, {ncName, NodeCredName}, {nc2Name, NodeCred2Name}, {ncUri, CertUri}, {uriPassword, UriPassword}, {ncPassword, CertPassword}, 
    {ncFingerprint, NodeCredFingerprint}, {tcName, TrustCatName}, {tcFingerprint, TrustCertFingerprint}, {tcUri, TrustCertUri} | Config].

%% @hidden
-spec end_per_suite(config()) -> any().

end_per_suite(Config) ->
    %% Cleanup CertM just in case
    rct_certm_lib:clear_certm_config(?config(handler, Config)),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, sec_credu1, 10000),
    ok.


%% @hidden
-spec init_per_group(atom(), config()) -> config().

init_per_group(sec_credu_nc_tcat_subscription_group, Config) ->
    init_per_group_common(Config);

init_per_group(sec_credu_trust_category_get_group, Config) ->
    NewConfig = init_per_group_common(Config),
    {save_config,[{tcatSubscriptionId, SubscriptionID}]} = sec_credu_tcat_subscribe_successful(NewConfig),
    [{tcatSubscriptionId, SubscriptionID} | NewConfig];

init_per_group(sec_credu_tcat_cert_get_group, Config) ->
    NewConfig = init_per_group_common(Config),
    {save_config,[{tcatSubscriptionId, SubscriptionID}]} = sec_credu_tcat_subscribe_successful(NewConfig),
    NewConfig2 = [{tcatSubscriptionId, SubscriptionID} | NewConfig],
    {save_config,[{tcat_info, TcatInfo}]} = sec_credu_tcat_get_info(NewConfig2),
    [{tcat_info, TcatInfo} | NewConfig2];

init_per_group(sec_credu_selection_object_group, Config) ->
    {save_config,[{handleAddress, HandleAddress}, {subscription, Subscription}]} = sec_credu_initialize(Config),
    [{handleAddress, HandleAddress}, {subscription, Subscription} | Config];

init_per_group(sec_credu_dispatch_group, Config) ->
    {save_config,[{handleAddress, HandleAddress}, {subscription, Subscription}]} = sec_credu_initialize(Config),
    NewConfig = [{handleAddress, HandleAddress}, {subscription, Subscription} | Config],
    {save_config,[{fd, Fd}]} = sec_credu_sel_object_get(NewConfig),
    [{fd, Fd} | NewConfig];

init_per_group(sec_credu_trust_category_free_group, Config) ->
    {save_config,[{handleAddress, HandleAddress}, {subscription, Subscription}]} = sec_credu_initialize(Config),
    [{handleAddress, HandleAddress}, {subscription, Subscription} | Config];

init_per_group(sec_credu_nc_group, Config) ->
    NewConfig = init_per_group_common(Config),
    {save_config,[{ncSubscriptionId, SubscriptionID}]} = sec_credu_nc_subscribe_successful(NewConfig),
    [{ncSubscriptionId, SubscriptionID} | NewConfig];

init_per_group(_Group, Config) ->
    Config.

init_per_group_common(Config) ->
    installNC(Config),
    timer:sleep(1000),
    installTC(Config),
    timer:sleep(1000),
    {ok, InstalledTCs} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), ?config(tcName, Config)),
    TrustedCertIds = lists:reverse(InstalledTCs),

    {save_config,[{handleAddress, HandleAddress}, {subscription, Subscription}]} = sec_credu_initialize(Config),
    [{trusted_certificate, TrustedCertIds}, {handleAddress, HandleAddress}, {subscription, Subscription} | Config].

%% @hidden
-spec end_per_group(atom(), config()) -> any().

end_per_group(sec_credu_nc_tcat_subscription_group, Config) ->
    end_per_group_common(Config);

end_per_group(sec_credu_selection_object_group, Config) ->
    sec_credu_finalize(Config),
    ok;

end_per_group(sec_credu_trust_category_get_group, Config) ->
    sec_credu_tcat_unsubscribe_successful(Config),
    end_per_group_common(Config);

end_per_group(sec_credu_tcat_cert_get_group, Config) ->
    sec_credu_tcat_unsubscribe_successful(Config),
    end_per_group_common(Config);

end_per_group(sec_credu_dispatch_group, Config) ->
    sec_credu_finalize(Config),
    ok;

end_per_group(sec_credu_trust_category_free_group, Config) ->
    sec_credu_finalize(Config),
    ok;

end_per_group(sec_credu_nc_group, Config) ->
    sec_credu_nc_unsubscribe_successful(Config),
    end_per_group_common(Config);

end_per_group(_Group, _Config) ->
    ok.

end_per_group_common(Config) ->
    sec_credu_finalize(Config),
    Config = removeTC(Config),
    Config = removeNC(Config),
    ok.


%% @hidden
-spec init_per_testcase(atom(), config()) -> config().

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_finalize orelse
                                         TestCase =:= sec_credu_finalize_with_empty_handle orelse
                                         TestCase =:= sec_credu_finalize_with_invalid_id ->
    
    {save_config,[{handleAddress, HandleAddress}, {subscription, Subscription}]} = sec_credu_initialize(Config),
    [{handleAddress, HandleAddress}, {subscription, Subscription} | Config];

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_uninstalled_nc_subscribe_successful ->
    installEmptyNC(Config);

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_nc_unsubscribe_successful orelse
                                         TestCase =:= sec_credu_nc_unsubscribe_with_empty_handle orelse
                                         TestCase =:= sec_credu_nc_unsubscribe_with_invalid_subscriber_id orelse
                                         TestCase =:= sec_credu_nc_unsubscribe_with_invalid_subscription_id ->
  
    {save_config,[{ncSubscriptionId, SubscriptionID}]} = sec_credu_nc_subscribe_successful(Config),
    [{ncSubscriptionId, SubscriptionID} | Config];

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_tcat_unsubscribe_successful orelse
                                         TestCase =:= sec_credu_tcat_unsubscribe_with_empty_handle orelse
                                         TestCase =:= sec_credu_tcat_unsubscribe_with_invalid_subscriber_id orelse
                                         TestCase =:= sec_credu_tcat_unsubscribe_with_invalid_subscription_id ->
  
    {save_config,[{tcatSubscriptionId, SubscriptionID}]} = sec_credu_tcat_subscribe_successful(Config),
    [{tcatSubscriptionId, SubscriptionID} | Config];

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_free_memory_tcat orelse 
                                         TestCase =:= sec_credu_free_memory_tcat_null_handle ->
    installNC(Config),
    installTC(Config),
    {ok, InstalledTCs} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), ?config(tcName, Config)),
    TrustedCertIds = lists:reverse(InstalledTCs),
    NewConfig = [{trusted_certificate, TrustedCertIds} | Config],
    {save_config,[{tcatSubscriptionId, SubscriptionID}]} = sec_credu_tcat_subscribe_successful(NewConfig),
    NewConfig2 = [{tcatSubscriptionId, SubscriptionID} | NewConfig],
    {save_config,[{tcat_info, TcatInfo}]} = sec_credu_tcat_get_info(NewConfig2),
    [{tcat_info, TcatInfo} | NewConfig2];

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_dispatch_one_nc orelse
                                         TestCase =:= sec_credu_dispatch_with_empty_handle orelse
                                         TestCase =:= sec_credu_dispatch_with_invalid_fd orelse
                                         TestCase =:= sec_credu_dispatch_with_invalid_flag ->
    {ok, ?SEC_CREDU_OK, 0, 0} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),
    installEmptyNC(Config),
    timer:sleep(1000),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, ?config(handleAddress,Config), {3, "nc2"}}),
    NewConfig = [{ncSubscriptionId, SubscriptionID} | Config],
    ok = rct_certm_lib:install_node_credential_from_uri(?config(handler, NewConfig), ?config(nc2Name, NewConfig),
                                                        ?config(ncUri, NewConfig), ?config(uriPassword, NewConfig), 
                                                        ?config(ncPassword, NewConfig), ?config(ncFingerprint, NewConfig)),
    NewConfig;

init_per_testcase(sec_credu_dispatch_one_tcat, Config) ->
    {ok, ?SEC_CREDU_OK, 0, 0} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),
    installNC(Config),
    timer:sleep(1000),
    installTC(Config),
    timer:sleep(1000),
    {ok, InstalledTCs} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), ?config(tcName, Config)),
    TrustedCertIds = lists:reverse(InstalledTCs),
    NewConfig = [{trusted_certificate, TrustedCertIds} | Config],
    {save_config,[{tcatSubscriptionId, SubscriptionID}]} = sec_credu_tcat_subscribe_successful(NewConfig),
    NewConfig2 = [{tcatSubscriptionId, SubscriptionID} | NewConfig],
    ct:pal("Svi ID-evi ~p~n", [TrustedCertIds]),
    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, NewConfig2), Tc, disabled) || Tc <- ?config(trusted_certificate, NewConfig2)],
    timer:sleep(1000),
    NewConfig2;

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_get_nc_cert_uninstalled_nc orelse
                                         TestCase =:= sec_credu_get_nc_key_uninstalled_nc ->
    installEmptyNC(Config),
    timer:sleep(1000),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, ?config(handleAddress,Config), {3, "nc2"}}),
    [{ncSubscriptionId2, SubscriptionID} | Config];

init_per_testcase(TestCase, Config) when TestCase =:= sec_credu_dispatch_one_nc_and_tcat orelse
                                         TestCase =:= sec_credu_dispatch_blocking orelse
                                         TestCase =:= sec_credu_dispatch_all ->
    {ok, ?SEC_CREDU_OK, 0, 0} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),
    installEmptyNC(Config),
    timer:sleep(1000),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, ?config(handleAddress,Config), {3, "nc2"}}),
    NewConfig = [{ncSubscriptionId, SubscriptionID} | Config],
    ok = rct_certm_lib:install_node_credential_from_uri(?config(handler, NewConfig), ?config(nc2Name, NewConfig),
                                                        ?config(ncUri, NewConfig), ?config(uriPassword, NewConfig), 
                                                        ?config(ncPassword, NewConfig), ?config(ncFingerprint, NewConfig)),
    timer:sleep(1000),
    installTC(NewConfig),
    timer:sleep(1000),
    {ok, InstalledTCs} = rct_certm_lib:get_trust_category_trusted_certs(?config(handler, NewConfig), ?config(tcName, NewConfig)),
    TrustedCertIds = lists:reverse(InstalledTCs),
    NewConfig2 = [{trusted_certificate, TrustedCertIds} | NewConfig],
    {save_config,[{tcatSubscriptionId, SubscriptionID2}]} = sec_credu_tcat_subscribe_successful(NewConfig2),
    NewConfig3 = [{tcatSubscriptionId, SubscriptionID2} | NewConfig2],
    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, NewConfig3), Tc, disabled) || Tc <- ?config(trusted_certificate, NewConfig3)],
    timer:sleep(1000),
    NewConfig3;

init_per_testcase(TestCase, Config) when TestCase =:= com_restart_while_initialize orelse
                                         TestCase =:= com_restart_while_subd_to_nc ->
    {save_config,[{handleAddress, HandleAddress}, {subscription, Subscription}]} = sec_credu_initialize(Config),
    {ok, 1, 80} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_SELECTION_OBJECT_GET, {?SUCCESS, HandleAddress}),
    [{handleAddress, HandleAddress}, {subscription, Subscription} | Config];
    
init_per_testcase(TestCase, Config) when TestCase =:= com_restart_while_subd_to_tcat orelse
                                         TestCase =:= com_restart_while_subd_nc_tcat ->
    {save_config,[{handleAddress, HandleAddress}, {subscription, Subscription}]} = sec_credu_initialize(Config),
    installNC(Config),
    installTC(Config),
    timer:sleep(10000),
    {ok, 1, 80} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_SELECTION_OBJECT_GET, {?SUCCESS, HandleAddress}),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?SUCCESS, HandleAddress, {5, "tcat1"}}),
    timer:sleep(2000),
    [{handleAddress, HandleAddress}, {subscription, Subscription}, {tcatSubscriptionId, SubscriptionID} | Config];

init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
-spec end_per_testcase(atom(), config()) -> any().

end_per_testcase(sec_credu_initialize, Config) ->
    sec_credu_finalize([{handleAddress, ?config(handleAddress, ?config(save_config, Config))} | Config]),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_finalize_with_empty_handle orelse
                                        TestCase =:= sec_credu_finalize_with_invalid_id ->
    sec_credu_finalize(Config),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_nc_subscribe_successful orelse
                                        TestCase =:= sec_credu_nc_subscribe_successful_multiple ->
    sec_credu_nc_unsubscribe_successful([{ncSubscriptionId, ?config(ncSubscriptionId, ?config(save_config, Config))} | Config]),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_nc_unsubscribe_with_empty_handle orelse
                                        TestCase =:= sec_credu_nc_unsubscribe_with_invalid_subscription_id orelse
                                        TestCase =:= sec_credu_nc_unsubscribe_with_invalid_subscriber_id ->
    sec_credu_nc_unsubscribe_successful(Config),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_uninstalled_nc_subscribe_successful ->
    removeEmptyNC(Config),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_tcat_subscribe_successful orelse
                                        TestCase =:= sec_credu_tcat_subscribe_successful_multiple ->
    sec_credu_tcat_unsubscribe_successful([{tcatSubscriptionId, ?config(tcatSubscriptionId, ?config(save_config, Config))} | Config]),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_tcat_unsubscribe_with_empty_handle orelse
                                        TestCase =:= sec_credu_tcat_unsubscribe_with_invalid_subscription_id orelse
                                        TestCase =:= sec_credu_tcat_unsubscribe_with_invalid_subscriber_id ->
    sec_credu_tcat_unsubscribe_successful(Config),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_free_memory_tcat orelse 
                                        TestCase =:= sec_credu_free_memory_tcat_null_handle ->
    sec_credu_tcat_unsubscribe_successful(Config),
    removeTC(Config),
    removeNC(Config),
    ok;

end_per_testcase(sec_credu_dispatch_one_tcat, Config) ->
    sec_credu_tcat_unsubscribe_successful(Config),
    removeTC(Config),
    removeNC(Config),
    rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ALL}),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_dispatch_one_nc orelse
                                        TestCase =:= sec_credu_dispatch_with_empty_handle orelse
                                        TestCase =:= sec_credu_dispatch_with_invalid_fd orelse
                                        TestCase =:= sec_credu_dispatch_with_invalid_flag ->
    sec_credu_nc_unsubscribe_successful(Config),
    removeEmptyNC(Config),
    rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ALL}),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_get_nc_cert_uninstalled_nc orelse
                                        TestCase =:= sec_credu_get_nc_key_uninstalled_nc ->
    removeEmptyNC(Config),
    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_UNSUBSCRIBE, {?SUCCESS, ?config(handleAddress,Config), ?config(ncSubscriptionId2,Config)}),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= sec_credu_dispatch_one_nc_and_tcat orelse
                                        TestCase =:= sec_credu_dispatch_blocking orelse
                                        TestCase =:= sec_credu_dispatch_all ->
    sec_credu_tcat_unsubscribe_successful(Config),
    sec_credu_nc_unsubscribe_successful(Config),
    removeTC(Config),
    removeEmptyNC(Config),
    rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ALL}),
    ok;

end_per_testcase(TestCase, Config) when TestCase =:= com_restart_while_subd_to_tcat orelse
                                        TestCase =:= com_restart_while_subd_nc_tcat ->
    removeNC(Config),
    removeTC(Config),
    ok;

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    SDCGroup = sdc_group(),
    QualificationGroup = qualification_group(),
    SecCreduInitializeGroup = sec_credu_initialize_group(),
    SecCreduFinalizeGroup = sec_credu_finalize_group(),
    SecCreduSelectionObjectGroup = sec_credu_selection_object_group(),
    SecCreduNCTcatSubscriptionGroup = sec_credu_nc_tcat_subscription_group(),
    SecCreduTrustCategoryGetGroup = sec_credu_trust_category_get_group(),
    SecCreduTcatTrustCertGetGroup = sec_credu_tcat_cert_get_group(),
    SecCreduTrustCategoryFreeGroup = sec_credu_trust_category_free_group(),
    SecCreduNCGroup = sec_credu_nc_group(),
    SecCreduDispatchGroup = sec_credu_dispatch_group(),
    ComRestartGroup = com_restart_group(),
    [
     {default_group, [], AllGroup},
     {sdc_group, [], SDCGroup},
     {qualification_group, [], QualificationGroup},
     {sec_credu_initialize_group, [], SecCreduInitializeGroup},
     {sec_credu_finalize_group, [], SecCreduFinalizeGroup},
     {sec_credu_selection_object_group, [], SecCreduSelectionObjectGroup},
     {sec_credu_nc_tcat_subscription_group, [], SecCreduNCTcatSubscriptionGroup},
     {sec_credu_trust_category_get_group, [], SecCreduTrustCategoryGetGroup},
     {sec_credu_tcat_cert_get_group, [], SecCreduTcatTrustCertGetGroup},
     {sec_credu_trust_category_free_group, [], SecCreduTrustCategoryFreeGroup},
     {sec_credu_nc_group, [], SecCreduNCGroup},
     {sec_credu_dispatch_group, [], SecCreduDispatchGroup},
     {com_restart_group, [], ComRestartGroup}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [].

sdc_group() ->
    [
     {group, sec_credu_initialize_group},
     {group, sec_credu_finalize_group},
     {group, sec_credu_nc_tcat_subscription_group},
     {group, sec_credu_nc_group},
     {group, sec_credu_trust_category_get_group},
     {group, sec_credu_tcat_cert_get_group},
     {group, sec_credu_trust_category_free_group},
     {group, sec_credu_selection_object_group},
     {group, sec_credu_dispatch_group}
    ].

qualification_group() ->
    [
     {group, sec_credu_initialize_group},
     {group, sec_credu_finalize_group},
     {group, sec_credu_nc_tcat_subscription_group},
     {group, sec_credu_nc_group},
     {group, sec_credu_trust_category_get_group},
     {group, sec_credu_tcat_cert_get_group},
     {group, sec_credu_trust_category_free_group},
     {group, sec_credu_selection_object_group},
     {group, sec_credu_dispatch_group}
    ].
    
sec_credu_initialize_group() ->
    [
     sec_credu_initialize,
     sec_credu_initialize_with_nc_tc_null,
     sec_credu_initialize_with_version_num_null,
     sec_credu_status_string
    ].

sec_credu_finalize_group() ->
    [
     sec_credu_finalize,
     sec_credu_finalize_with_empty_handle,
     sec_credu_finalize_with_invalid_id
    ].

sec_credu_selection_object_group() ->
    [
     sec_credu_sel_object_get,
     sec_credu_sel_object_with_invalid_id,
     sec_credu_sel_object_with_empty_handle
    ].

sec_credu_nc_tcat_subscription_group() ->
    [
     sec_credu_nc_subscribe_successful,
     sec_credu_nc_subscribe_successful_multiple,
     sec_credu_nc_subscribe_with_empty_handle,
     sec_credu_nc_subscribe_with_invalid_nc_id,
     sec_credu_nc_subscribe_with_invalid_subscriber_id,
     sec_credu_uninstalled_nc_subscribe_successful,
     sec_credu_nc_unsubscribe_successful,
     sec_credu_nc_unsubscribe_with_empty_handle,
     sec_credu_nc_unsubscribe_with_invalid_subscriber_id,
     sec_credu_nc_unsubscribe_with_invalid_subscription_id,
     sec_credu_tcat_subscribe_successful,
     sec_credu_tcat_subscribe_successful_multiple,
     sec_credu_tcat_subscribe_with_empty_handle,
     sec_credu_tcat_subscribe_with_invalid_tcat_id,
     sec_credu_tcat_subscribe_with_invalid_subscriber_id,
     sec_credu_disabled_tcat_subscribe_successful,
     sec_credu_tcat_unsubscribe_successful,
     sec_credu_tcat_unsubscribe_with_empty_handle,
     sec_credu_tcat_unsubscribe_with_invalid_subscriber_id,
     sec_credu_tcat_unsubscribe_with_invalid_subscription_id
    ].

sec_credu_trust_category_get_group() ->
    [
     sec_credu_tcat_get_info,
     sec_credu_tcat_get_info_invalid_id,
     sec_credu_tcat_get_info_invalid_subid,
     sec_credu_tcat_get_info_disabled_tc,
     sec_credu_tcat_get_dir_name,
     sec_credu_tcat_get_dir_name_null,
     sec_credu_tcat_get_nr_of_certs,
     sec_credu_tcat_get_nr_of_certs_null
    ].

sec_credu_tcat_cert_get_group() ->
    [
     sec_credu_get_tcat_cert_info,
     sec_credu_get_tcat_cert_info_null,
     sec_credu_get_tcat_cert_info_wrong_format,
     sec_credu_get_tcat_cert_info_wrong_index,
     sec_credu_get_tcat_cert_filename,
     sec_credu_get_tcat_cert_filename_wrong_format,
     sec_credu_get_tcat_cert_filename_wrong_index
    ].

sec_credu_nc_group() ->
    [
     sec_credu_get_nc_cert,
     sec_credu_get_nc_cert_invalid_id,
     sec_credu_get_nc_cert_invalid_subscribe_id,
     sec_credu_get_nc_cert_uninstalled_nc,
     sec_credu_get_nc_empty_handle,
     sec_credu_get_nc_wrong_format,
     sec_credu_get_nc_key,
     sec_credu_get_nc_key_invalid_id,
     sec_credu_get_nc_key_invalid_subscribe_id,
     sec_credu_get_nc_key_uninstalled_nc,
     sec_credu_get_nc_key_empty_handle,
     sec_credu_get_nc_key_wrong_format
    ].

sec_credu_dispatch_group() ->
    [
     sec_credu_dispatch_one_nc,
     sec_credu_dispatch_one_tcat,
     sec_credu_dispatch_one_nc_and_tcat,
     sec_credu_dispatch_all,
     sec_credu_dispatch_blocking,
     sec_credu_dispatch_with_empty_handle,
     sec_credu_dispatch_with_invalid_fd,
     sec_credu_dispatch_with_invalid_flag
    ].

 sec_credu_trust_category_free_group() ->
     [
      sec_credu_free_memory_tcat,
      sec_credu_free_memory_tcat_null_handle
     ].

com_restart_group() ->
    [
     com_restart_while_initialize,
     com_restart_while_subd_to_nc,
     com_restart_while_subd_to_tcat,
     com_restart_while_subd_nc_tcat
    ].

%%%--------------------------------------------------------------------
%%% TEST CASES
%%%--------------------------------------------------------------------

%% TC-US1-5    SecCredu initialize successful
%% Client calls initialize with a NC callback, TC callback and a version number. The client
%% expects SEC_CREDU_OK signal. Check that the handle is set to the value that represents
%% this particular API initialization. Check that the version fields are set to the actual
%% version selected.

sec_credu_initialize(_Config) ->
    ct:pal("Sending request 'initialize' with NC and TC callback and a version number~n"),
    {ok, ?SEC_CREDU_OK, HandleAddress, Subscription, {?RC, ?MAJ_VER, ?MIN_VER}} =
        rct_proxy:send_proxy(node1, sec_credu1, ?REQ_INITIALIZE, {?SUCCESS, ?RC, ?MAJ_VER, ?MIN_VER}),        %0 for pass, 1 for fail handle, 2 for fail ver

    ok = is_handle_valid(HandleAddress),
    ct:pal("certSecCredu initialize was successful~n"),
    ct:pal(?LOW_IMPORTANCE, "HandleAddress: ~p ", [HandleAddress]),
    ExportConfig = [{handleAddress, HandleAddress}, {subscription, Subscription}],
    {save_config, ExportConfig}.

%% TC-US1-5a    SecCredu initialize with NC and TC set to NULL
%% Client calls initialize with NULL parameters. Check that a specific SEC_CREDU_ERROR
%% is obtained.

sec_credu_initialize_with_nc_tc_null(_Config) ->
    ct:pal("Sending request 'initialize' with NC and TC as NULL~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_INITIALIZE, {?PARAMETERS_NULL, ?RC, ?MAJ_VER, ?MIN_VER}),        %0 for pass, 2 for fail ver, 3 for fail params
    ct:log("certSecCredu initialize with NC and TC NULL returned: sec credu invalid parameter~n"),

    ok.

%% TC-US1-5b    SecCredu initialize with version number set to NULL
%% Client calls initialize with no version number. Check that a specific SEC_CREDU_ERROR
%% is obtained.

sec_credu_initialize_with_version_num_null(_Config) ->
    ct:pal("Sending request 'initialize' with version number as NULL~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_INITIALIZE, {?VERSION_NULL, ?RC, 0, 0}),        %0 for pass, 2 for fail ver, 3 for fail params, version "A"00
    ct:log("certSecCredu initialize with version NULL returned: sec credu invalid parameter~n"),

    ok.

%% TC-US-5c    Status strings
%% Client calls various status requests. The client expects decoded status string requests.

sec_credu_status_string(_Config) ->
    ct:pal("Sending various requests 'status string'"),
    
    {ok, "Operation succeeded"} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_STATUS_STRING, {?SEC_CREDU_OK}),
    {ok, "Operation failed: reason unspecified"} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_STATUS_STRING, {?SEC_CREDU_ERR_UNSPECIFIED}),
    {ok, "Operation failed: service is unavailable, try again"} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_STATUS_STRING, {?SEC_CREDU_ERR_SERVICE_UNAVAILABLE}),
    {ok, "Operation failed: an invalid parameter was given"} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_STATUS_STRING, {?SEC_CREDU_ERROR_INVALID_PARAMETER}),
    {ok, "Operation failed: unsupported format was given"} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_STATUS_STRING, {?SEC_CREDU_ERROR_UNSUPPORTED_FORMAT}),
    {ok, []} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_STATUS_STRING, {6}),

    ct:pal("certSecCredu status string was successful~n"),
    ok.

%% TC-US1-6    SecCredu finalize successful
%% Client calls finalization with a valid handle. Client expects SEC_CREDU_OK signal.
%% Check that the user is removed and selection object socket closed as well as the
%% communication socket.

sec_credu_finalize(Config) ->
    ct:pal("Sending request 'finalize'~n"),
    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?SUCCESS, ?config(handleAddress, Config)}),        %0 for pass, 1 for null handle, 3 for wrong id
    ct:pal("certSecCredu finalize was successful~n"),

    ok.

%% TC-US1-6a    SecCredu finalize unsuccessful with handle set to NULL
%% Client calls finalization with NULL handle. Check that a SEC_CREDU_ERROR_INVALID_PARAMETER
%% is obtained.

sec_credu_finalize_with_empty_handle(Config) ->
    ct:pal("Sending request 'finalize' with handle as NULL~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?HANDLE_NULL, ?config(handleAddress, Config)}),        %0 for pass, 1 for null handle, 4 for wrong id
    ct:log("certSecCredu finalize with handle NULL returned: sec credu invalid parameter~n"),

    ok.

%% TC-US1-6b    SecCredu finalize unsuccessful with invalid ID
%% Client calls finalization with an invalid ID. Check that a SEC_CREDU_ERROR is obtained.

sec_credu_finalize_with_invalid_id(Config) ->
    ct:pal("Sending request 'finalize' with invalid ID~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?INVALID_ID, ?config(handleAddress, Config)}),        %0 for pass, 1 for null handle, 4 for wrong id
    ct:log("certSecCredu finalize with invalid ID returned: sec credu invalid parameter~n"),

    ok.

%% TC-US1-7    SecCredu get selection object successful
%% Client calls selectionobject_get with valid handle and ID. Check that the selection object
%% (socket) is stored by certSecCredu and sec_credu_api as file descriptor (FD). The client
%% expects SEC_CREDU_OK signal.

sec_credu_sel_object_get(Config) ->
    ct:pal("Sending request 'selection object get' with valid handle and ID~n"),
    {ok, ?SEC_CREDU_OK, Fd} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_SELECTION_OBJECT_GET, {?SUCCESS, ?config(handleAddress, Config)}),        %0 for pass, 1 for null handle, 4 for wrong id
    ct:pal("certSecCredu selection object get was successful~n"),
    ExportConfig = [{fd, Fd}],
    {save_config, ExportConfig}.

%% TC-US1-7a    SecCredu get selection object with invalid id
%% Client calls selectionobject_get with invalid ID. Check that a SEC_CREDU_ERROR is obtained.

sec_credu_sel_object_with_invalid_id(Config) ->
    ct:pal("Sending request 'selection object get' with invalid id~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_SELECTION_OBJECT_GET, {?INVALID_ID, ?config(handleAddress, Config)}),        %0 for pass, 1 for null handle, 4 for wrong id
    ct:log("certSecCredu selection object with invalid ID returned: sec credu invalid parameter~n"),
    ok.

%% TC-US1-7b    SecCredu get selection object with handle set to NULL
%% Client calls selectionobject_get with NULL handle. Check that a
%% SEC_CREDU_ERROR_INVALID_PARAMETER  is obtained.

sec_credu_sel_object_with_empty_handle(Config) ->
    ct:pal("Sending request 'selection object get' with valid handle and FD~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_SELECTION_OBJECT_GET, {?HANDLE_NULL, ?config(handleAddress, Config)}),        %0 for pass, 1 for null handle, 4 for wrong id
    ct:log("certSecCredu selection object with handle NULL returned: sec credu invalid parameter~n"),
    ok.

%% TC-US1-14, 15    Get NodeCredential Certificate
%% Client is initalized. Client Sbscribes to NC. Client gets information about  NodeCredential certificate.
%% Check that client received SEC_CREDU_OK. Data is stored in Data.

sec_credu_get_nc_cert(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Certificate"),
    {ok, NC, _PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, ?SEC_CREDU_OK, {Data}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?PEM}),
    {ok, ?SEC_CREDU_OK, {Data1}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?FILEPATH}),
    
    {ok, [FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [_FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    
    Data = binary_to_list(NC),
    Data1 = ?CERTS_DIR ++FileNc,
    
    ok.

%% TC-US1-14a   Get NodeCredential Certificate - invalid ID
%% Client is initalized. Client Sbscribes to NC. Client gets information about NodeCredential
%% certificate with invalid ID. Check that client received SEC_CREDU_INVALID_PARAMETER .

sec_credu_get_nc_cert_invalid_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Certificate - invalid ID"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?INVALID_ID, HandleAddress, SubscriptionID, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?INVALID_ID, HandleAddress, SubscriptionID, ?FILEPATH}),

    ok.

%% TC-US1-14b   Get NodeCredential Certificate - invalid SubscribeID
%% Client is initalized. Client Sbscribes to NC. Client gets information about  NodeCredential
%% certificate with invalid SubscribeID. Check that client received SEC_CREDU_ERROR_UNSPECIFIED.

sec_credu_get_nc_cert_invalid_subscribe_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Certificate - invalid SubscribeID"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?SUCCESS, HandleAddress, SubscriptionID+1, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?SUCCESS, HandleAddress, SubscriptionID+1, ?FILEPATH}),

    ok.

%% TC-US1-14c   Get NodeCredential Certificate - uninstalled NC
%% Client is initalized. Client Subscribes to NC.  NC is uninstalled. Client gets information
%% about NC. Check that client received SEC_CREDU_ERROR_UNSPECIFIED.

sec_credu_get_nc_cert_uninstalled_nc(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId2, Config),
    
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?FILEPATH}),
    
    ok.

%% TC-US1-14d   Get NodeCredential Certificate - NULL as Handle
%% Client is initalized. Client Subscribes to NC. Client gets information about  NodeCredential
%% certificate with NULL as data Check that client received SEC_CREDU_INVALID_PARAMETE.

sec_credu_get_nc_empty_handle(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
        
    ct:pal("Get NodeCredential Certificate - NULL as Handle"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?HANDLE_NULL, HandleAddress, SubscriptionID, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?HANDLE_NULL, HandleAddress, SubscriptionID, ?FILEPATH}),
    
    ok.

%% TC-US1-14e   Get NodeCredential Certificate - wrong format
%% Client is initalized. Client Subscribes to NC. Client gets information about
%% NodeCredential certificate using wrong format. Client recieves SEC_CREDU_INVALID_PARAMETER_ERROR.

sec_credu_get_nc_wrong_format(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Certificate - wrong format"),
    {error, ?SEC_CREDU_ERROR_UNSUPPORTED_FORMAT} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_CERT_GET, {?SUCCESS, HandleAddress, SubscriptionID, 56}),
   
    ok.

%% TC-US1-16, 17    Get NodeCredential Key
%% Client is initalized. Client Subscribes to NC. Client gets information about NodeCredential Key.
%% Check that client received SEC_CREDU_OK. Data is stored in Data.

sec_credu_get_nc_key(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Key"),
    {ok, _NC, PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, ?SEC_CREDU_OK, {Data}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?PEM}),
    {ok, ?SEC_CREDU_OK, {Data1}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?FILEPATH}),
    
    {ok, [_FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    
    Data = binary_to_list(PKey),
    Data1 = ?PRIVATE_DIR ++FilePkey,
    
    ok.

%% TC-US1-16a   Get NodeCredential Key - invalid ID
%% Client is initalized. Client Sbscribes to NC. Client gets information about NodeCredential
%% Key with invalid ID. Check that client received SEC_CREDU_INVALID_PARAMETER .

sec_credu_get_nc_key_invalid_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Key - invalid ID"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?INVALID_ID, HandleAddress, SubscriptionID, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?INVALID_ID, HandleAddress, SubscriptionID, ?FILEPATH}),

    ok.

%% TC-US1-16b   Get NodeCredential Key - invalid SubscribeID
%% Client is initalized. Client Sbscribes to NC. Client gets information about  NodeCredential
%% Key with invalid SubscribeID. Check that client received SEC_CREDU_ERROR_UNSPECIFIED.

sec_credu_get_nc_key_invalid_subscribe_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Key - invalid SubscribeID"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?SUCCESS, HandleAddress, SubscriptionID+1, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?SUCCESS, HandleAddress, SubscriptionID+1, ?FILEPATH}),

    ok.

%% TC-US1-16c   Get NodeCredential Key - uninstalled NC
%% Client is initalized. Client Subscribes to NC.  NC is uninstalled. Client gets information
%% about NC. Check that client received SEC_CREDU_ERROR_UNSPECIFIED.

sec_credu_get_nc_key_uninstalled_nc(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId2, Config),
    
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?SUCCESS, HandleAddress, SubscriptionID, ?FILEPATH}),
    
    ok.

%% TC-US1-16d   Get NodeCredential Key - NULL as Handle
%% Client is initalized. Client Subscribes to NC. Client gets information about  NodeCredential
%% Key with NULL as data Check that client received SEC_CREDU_INVALID_PARAMETE.

sec_credu_get_nc_key_empty_handle(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
        
    ct:pal("Get NodeCredential Key - NULL as Handle"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?HANDLE_NULL, HandleAddress, SubscriptionID, ?PEM}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?HANDLE_NULL, HandleAddress, SubscriptionID, ?FILEPATH}),
    
    ok.

%% TC-US1-16e   Get NodeCredential Key - wrong format
%% Client is initalized. Client Subscribes to NC. Client gets information about
%% NodeCredential Key using wrong format. Client recieves SEC_CREDU_INVALID_PARAMETER_ERROR.

sec_credu_get_nc_key_wrong_format(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    ct:pal("Handle: ~p", [HandleAddress]),
    
    ct:pal("Get NodeCredential Key - wrong format"),
    {error, ?SEC_CREDU_ERROR_UNSUPPORTED_FORMAT} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_KEY_GET, {?SUCCESS, HandleAddress, SubscriptionID, 56}),
   
    ok.

%% TC-US1-18
sec_credu_nc_subscribe_successful(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    
    ct:pal("subscribe to NC~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, HandleAddress, {3, "nc1"}}),
    ct:pal("subscription to NC was successful!~n"),

    %check that NC and Privatekey are stored in directory
    {ok, NC, PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, [FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    ok = check_content_in_file(FileNc, NC, ?CERTS_DIR),
    ok = check_content_in_file(FilePkey, PKey, ?PRIVATE_DIR),
    ct:pal(?LOW_IMPORTANCE, "HandleAddress: ~p, SubscriptionID: ~p, ", [HandleAddress, SubscriptionID]),

    ExportConfig = [{ncSubscriptionId, SubscriptionID}],
    {save_config, ExportConfig}.

%% TC-US1-18a multiple clients
sec_credu_nc_subscribe_successful_multiple(Config) ->
    HandleAddress = ?config(handleAddress, Config),

    ct:pal("1st client subscribes to NC~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, HandleAddress, {3, "nc1"}}),
    ct:pal("subscription to NC for first client was successful!~n"),

    {ok, ?SEC_CREDU_OK, HandleAddress1, _ID, {?RC, ?MAJ_VER, ?MIN_VER}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_INITIALIZE, {?SUCCESS, ?RC, ?MAJ_VER, ?MIN_VER}),
    ct:pal("2nd client subscribes to NC~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID1}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, HandleAddress1, {3, "nc1"}}),
    ct:pal("subscription to NC for second client was successful!~n"),
    
    %check that NC and Privatekey are stored in directory
    {ok, NC, PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, [FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    ok = check_content_in_file(FileNc, NC, ?CERTS_DIR),
    ok = check_content_in_file(FilePkey, PKey, ?PRIVATE_DIR),

    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_UNSUBSCRIBE, {?SUCCESS, HandleAddress1, SubscriptionID1}),
    ct:pal("unsubscribe from NC for second client!~n"),

    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?SUCCESS, HandleAddress1}), 
    ct:pal("finalize second client!~n"),

    %check that NC and Privatekey are still stored in directory
    {ok, NC, PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, [FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    ok = check_content_in_file(FileNc, NC, ?CERTS_DIR),
    ok = check_content_in_file(FilePkey, PKey, ?PRIVATE_DIR),
    
    ExportConfig = [{ncSubscriptionId, SubscriptionID}],
    {save_config, ExportConfig}.

%% TC-US1-18b subscription unsuccessful, empty handle
sec_credu_nc_subscribe_with_empty_handle(_Config) ->

    ct:pal("subscribe to NC~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?HANDLE_NULL, 0, {3, "nc1"}}),
    ct:pal("subscription to NC was unsuccessful, handle is empty!~n"),
   
    %check that NC and Privatekey are not stored in directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    
    ok.

%% TC-US1-18c subscription unsuccessful, invalid node credential id
sec_credu_nc_subscribe_with_invalid_nc_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),

    ct:pal("subscribe to NC~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, HandleAddress, {3, "nc2"}}),
    ct:pal("subscription to NC was unsuccessful, invalid node credential id!~n"),

    %check that NC and Privatekey are not stored in directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
        
    ok.

%% TC-US1-18d subscription unsuccessful, invalid subscriber id
sec_credu_nc_subscribe_with_invalid_subscriber_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    ct:pal("subscribe to NC~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?INVALID_ID, HandleAddress, {3, "nc1"}}),
    ct:pal("subscription to NC was unsuccessful, invalid subscriber ID!~n"),

    %check that NC and Privatekey are not stored in directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
        
    ok.

%% TC-US1-18e, subscribe to uninstalled NC, unsubscribe from uninstalled NC
sec_credu_uninstalled_nc_subscribe_successful(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    
    ct:pal("subscribe to NC~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, HandleAddress, {3, "nc2"}}),
    ct:pal("subscription to NC was successful!~n"),

    %check that NC and Privatekey are not stored in directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),

    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_UNSUBSCRIBE, {?SUCCESS, HandleAddress, SubscriptionID}),
    
     %check that NC and Privatekey are not stored in directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    
    ok.

%% TC-US1-19 unsubscription successful, 
sec_credu_nc_unsubscribe_successful(Config) ->

    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),

    ct:pal("unsubscribe from NC~n"),

    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_UNSUBSCRIBE, {?SUCCESS, HandleAddress, SubscriptionID}),

    %check that NC and Privatekey are deleted from directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    ct:pal("unsubscription from NC was successful!~n"),
    
    ok.

%% TC-US1-19a unsubscription unsuccessful, empty handle
sec_credu_nc_unsubscribe_with_empty_handle(Config) ->
    SubscriptionID = ?config(ncSubscriptionId, Config),
    
    ct:pal("unsubscribe from NC~n"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_UNSUBSCRIBE, {?HANDLE_NULL, 0, SubscriptionID}),
    
    %check that NC and Privatekey are still in the directory
    {ok, NC, PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, [FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    ok = check_content_in_file(FileNc, NC, ?CERTS_DIR),
    ok = check_content_in_file(FilePkey, PKey, ?PRIVATE_DIR),
    
    ct:pal("unsubscription from NC was not successful!~n"),
    ok.

%% TC-US1-19b unsubscription unsuccessful, invalid subscription id
sec_credu_nc_unsubscribe_with_invalid_subscription_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    
    ct:pal("unsubscribe from NC~n"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_UNSUBSCRIBE, {?SUCCESS, HandleAddress, SubscriptionID + 1}),
    
    %check that NC and Privatekey are still in the directory
    {ok, NC, PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, [FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    ok = check_content_in_file(FileNc, NC, ?CERTS_DIR),
    ok = check_content_in_file(FilePkey, PKey, ?PRIVATE_DIR),
    
    ct:pal("unsubscription from NC was not successful!~n"),
    ok.

%% TC-US1-19c unsubscription unsuccessful, invalid subscriber id
sec_credu_nc_unsubscribe_with_invalid_subscriber_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(ncSubscriptionId, Config),
    
    ct:pal("unsubscribe from NC~n"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_UNSUBSCRIBE, {?INVALID_ID, HandleAddress, SubscriptionID}),
    
    %check that NC and Privatekey are still in the directory
    {ok, NC, PKey} =  rct_rpc:call(rpc, certSecCredu, get_cert, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=nc1">>], 10000),
    {ok, [FileNc]} =  rct_rpc:call(rpc, file, list_dir_all, [?CERTS_DIR], 10000),
    {ok, [FilePkey]} =  rct_rpc:call(rpc, file, list_dir_all, [?PRIVATE_DIR], 10000),
    ok = check_content_in_file(FileNc, NC, ?CERTS_DIR),
    ok = check_content_in_file(FilePkey, PKey, ?PRIVATE_DIR),
    
    ct:pal("unsubscription from NC was not successful!~n"),
    ok.

%% TC-US1-20
sec_credu_tcat_subscribe_successful(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    
    ct:pal("subscribe to TCat~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?SUCCESS, HandleAddress, {5, "tcat1"}}),
    ct:pal("subscription to TCat was successful!~n"),

    %check that Trusted Certificates are stored in directory
    {ok, List} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    {ok, TrustCertandMoRefsList} =  rct_rpc:call(rpc, certSecCredu, get_tcat_certs_and_MoRefs, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=tcat1">>], 10000),
    {_TrustedCertificateMoRefs, TrustedCertificates} = lists:unzip(TrustCertandMoRefsList),
    ok = check_pem_files_for_certs(lists:reverse(lists:sort(List)), 2, lists:sort(TrustedCertificates), ?CACERTSTCAT1_DIR),
    ct:pal(?LOW_IMPORTANCE, "HandleAddress: ~p, SubscriptionID: ~p, ", [HandleAddress, SubscriptionID]),

    ExportConfig = [{tcatSubscriptionId, SubscriptionID}],
    {save_config, ExportConfig}.

%% TC-US1-20a multiple clients
sec_credu_tcat_subscribe_successful_multiple(Config) ->
    HandleAddress = ?config(handleAddress, Config),

    ct:pal("1st client subscribes to TCat~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?SUCCESS, HandleAddress, {5, "tcat1"}}),
    ct:pal("subscription to TCat for first client was successful!~n"),

    {ok, ?SEC_CREDU_OK, HandleAddress1, _ID, {?RC, ?MAJ_VER, ?MIN_VER}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_INITIALIZE, {?SUCCESS, ?RC, ?MAJ_VER, ?MIN_VER}),
    ct:pal("2nd client subscribes to TCat~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID1}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?SUCCESS, HandleAddress1, {5, "tcat1"}}),
    ct:pal("subscription to TCat for second client was successful!~n"),
    ct:pal(?LOW_IMPORTANCE, "HandleAddress: ~p, SubscriptionID: ~p, ", [HandleAddress, SubscriptionID]),
    ct:pal(?LOW_IMPORTANCE, "HandleAddress1: ~p, SubscriptionID1: ~p, ", [HandleAddress1, SubscriptionID1]),
    
    %check that Trusted Certificates are stored in directory
    {ok, List} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    {ok, TrustCertandMoRefsList} =  rct_rpc:call(rpc, certSecCredu, get_tcat_certs_and_MoRefs, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=tcat1">>], 10000),
    {_TrustedCertificateMoRefs, TrustedCertificates} = lists:unzip(TrustCertandMoRefsList),
    ok = check_pem_files_for_certs(lists:reverse(lists:sort(List)), 2, lists:sort(TrustedCertificates), ?CACERTSTCAT1_DIR),
                                                                      
    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_UNSUBSCRIBE, {?SUCCESS, HandleAddress1, SubscriptionID1}),
    
    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?SUCCESS, HandleAddress1}), 

    %check that Trusted Certificates are still stored in directory
    {ok, List} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    {ok, TrustCertandMoRefsList} =  rct_rpc:call(rpc, certSecCredu, get_tcat_certs_and_MoRefs, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=tcat1">>], 10000),
    {_TrustedCertificateMoRefs, TrustedCertificates} = lists:unzip(TrustCertandMoRefsList),
    ok = check_pem_files_for_certs(lists:reverse(lists:sort(List)), 2, lists:sort(TrustedCertificates), ?CACERTSTCAT1_DIR),
    
    ExportConfig = [{tcatSubscriptionId, SubscriptionID}],
    {save_config, ExportConfig}.

%% TC-US1-20b subscription unsuccessful, empty handle
sec_credu_tcat_subscribe_with_empty_handle(_Config) ->

    ct:pal("subscribe to Tcat~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?HANDLE_NULL, 0, {5, "tcat1"}}),
    ct:pal("subscription to TCat was unsuccessful, handle is empty!"),
   
    %check that Trusted certificates are not stored in Tcat directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTS_DIR], 10000),
    ok.

%% TC-US1-20c subscription unsuccessful, invalid tcat id
sec_credu_tcat_subscribe_with_invalid_tcat_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),

    ct:pal("subscribe to TCat~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?SUCCESS, HandleAddress, {5, "tcat2"}}),
    ct:pal("subscription to TCat was unsuccessful, invalid trust category id!"),

    %check that Trusted certificates are not stored in Tcat directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTS_DIR], 10000),
    ok.

%% TC-US1-20d subscription unsuccessful, invalid subscriber id
sec_credu_tcat_subscribe_with_invalid_subscriber_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    ct:pal("subscribe to TCat~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?INVALID_ID, HandleAddress, {5, "tcat1"}}),
    ct:pal("subscription to TCat was unsuccessful, invalid subscriber ID!"),

    %check that Trusted certificates are not stored in Tcat directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTS_DIR], 10000),
    ok.

%% TC-US1-20e, subscribe to disabled TCat, unsubscribe from disabled TCat
sec_credu_disabled_tcat_subscribe_successful(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    
    {ok, TrustedCertIds} =  rct_certm_lib:get_trust_category_trusted_certs(?config(handler, Config), "tcat1"),
    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), Tc, disabled) || Tc <- TrustedCertIds],
    
    ct:pal("subscribe to TCat~n"),
    {ok, ?SEC_CREDU_OK, {SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_SUBSCRIBE, {?SUCCESS, HandleAddress, {5, "tcat1"}}),
    ct:pal("subscription to TCat was successful!"),

    %check that Trusted certificates do not exist in TCat directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTS_DIR], 10000),
    ct:pal("Trusted certificates do not exist in TCat directory!"),

    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_UNSUBSCRIBE, {?SUCCESS, HandleAddress, SubscriptionID}),
    
    %check that Trusted certificates do not exist in TCat directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTS_DIR], 10000),
    
    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), Tc, enabled) || Tc <- TrustedCertIds],
    ok.

%% TC-US1-21 unsubscription successful, 
sec_credu_tcat_unsubscribe_successful(Config) ->

    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(tcatSubscriptionId, Config),

    ct:pal("unsubscribe from TCat~n"),

    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_UNSUBSCRIBE, {?SUCCESS, HandleAddress, SubscriptionID}),

    %check that Trusted certificates are deleted from TCat directory
    {ok, []} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTS_DIR], 10000),
    ct:pal("unsubscription from TCat was successful!"),
    ok.

%% TC-US1-21a unsubscription unsuccessful, empty handle
sec_credu_tcat_unsubscribe_with_empty_handle(Config) ->
    SubscriptionID = ?config(tcatSubscriptionId, Config),

    ct:pal("unsubscribe from TCat~n"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_UNSUBSCRIBE, {?HANDLE_NULL, 0, SubscriptionID}),
    %check that Trusted certificates are still stored in Tcat directory
    {ok, List} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    {ok, TrustCertandMoRefsList} =  rct_rpc:call(rpc, certSecCredu, get_tcat_certs_and_MoRefs, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=tcat1">>], 10000),
    {_TrustedCertificateMoRefs, TrustedCertificates} = lists:unzip(TrustCertandMoRefsList),
    ok = check_pem_files_for_certs(lists:reverse(lists:sort(List)), 2, lists:sort(TrustedCertificates), ?CACERTSTCAT1_DIR),
    
    ct:pal("unsubscription from TCat was not successful!"),
    ok.

%% TC-US1-21b unsubscription unsuccessful, invalid subscription id
sec_credu_tcat_unsubscribe_with_invalid_subscription_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(tcatSubscriptionId, Config),
    ct:pal("unsubscribe from TCat~n"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_UNSUBSCRIBE, {?SUCCESS, HandleAddress, SubscriptionID + 1}),
    
    %check that Trusted certificates are still stored in Tcat directory
    {ok, List} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    {ok, TrustCertandMoRefsList} =  rct_rpc:call(rpc, certSecCredu, get_tcat_certs_and_MoRefs, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=tcat1">>], 10000),
    {_TrustedCertificateMoRefs, TrustedCertificates} = lists:unzip(TrustCertandMoRefsList),
    ok = check_pem_files_for_certs(lists:reverse(lists:sort(List)), 2, lists:sort(TrustedCertificates), ?CACERTSTCAT1_DIR),
    
    ct:pal("unsubscription from TCat was not successful!"),
    ok.

%% TC-US1-21c unsubscription unsuccessful, invalid subscriber id
sec_credu_tcat_unsubscribe_with_invalid_subscriber_id(Config) ->
    HandleAddress = ?config(handleAddress, Config),
    SubscriptionID = ?config(tcatSubscriptionId, Config),
    
    ct:pal("unsubscribe from NC~n"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_UNSUBSCRIBE, {?INVALID_ID, HandleAddress, SubscriptionID}),
    
    %check that Trusted certificates are still stored in Tcat directory
    {ok, List} =  rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    {ok, TrustCertandMoRefsList} =  rct_rpc:call(rpc, certSecCredu, get_tcat_certs_and_MoRefs, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=tcat1">>], 10000),
    {_TrustedCertificateMoRefs, TrustedCertificates} = lists:unzip(TrustCertandMoRefsList),
    ok = check_pem_files_for_certs(lists:reverse(lists:sort(List)), 2, lists:sort(TrustedCertificates), ?CACERTSTCAT1_DIR),

    ct:pal("unsubscription from TCat was not successful!"),
    
    ok.


%% TC-US1-8    Get TrustCategory information successful
%% Client is initalized. Client Subscribes to TC. Client gets information about TrustCategory.
%% Check that client received SEC_CREDU_OK signal. Data is stored in Category.

sec_credu_tcat_get_info(Config) ->
    ct:pal("Getting info about trust category: "),

    {ok, ?SEC_CREDU_OK, {TcatInfo}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?SUCCESS, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)}),
    ct:pal("Tcat info: ~p", [TcatInfo]),
    ExportConfig = [{tcat_info, TcatInfo}],
    {save_config, ExportConfig}.

%% TC-US1-8a   Get TrustCategory information - invalid ID
%% Client is initalized. Client Subscribes to TC.  Client gets information about TrustCategory with
%% invalid ID. Check that client received SEC_CREDU_INVALID_PARAMETER signal.

sec_credu_tcat_get_info_invalid_id(Config) ->
    ct:pal("Getting info about trust category with invalid id:"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?INVALID_ID, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)}),
    ct:pal("certSecCredu get tcat info with invalid ID returned: sec credu invalid parameter"),
    ok.

%% TC-US1-8b    Get TrustCategory information - invalid SubscribeID
%% Client is initalized. Client Subscribes to TC.  Client gets information about TrustCategory
%% with invalid SubscribeID. Check that client received SEC_CREDU_INVALID_PARAMETER signal.

sec_credu_tcat_get_info_invalid_subid(Config) ->
    ct:pal("Getting info about trust category with invalid subscription id:"),

    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?INVALID_SUB_ID, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)+1}),
    ct:pal("certSecCredu get tcat info with invalid SUB_ID returned: sec credu invalid parameter"),
    ok.

%% TC-US1-8c    Client gets info about trust category with disabled trusted certificate
%% Snmp MO and SnmpTargetV3Dtls are enabled. Client subscribes to TC. TrustedCertificate
%% is disabled. Client gets information about TrustCategory. Check that client received
%% SEC_CREDU_ERR_UNSPECIFIED signal.

sec_credu_tcat_get_info_disabled_tc(Config) ->
    {ok, TrustedCertIds} =  rct_certm_lib:get_all_trusted_certificates(?config(handler, Config)),
    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), Tc, disabled) || Tc <- TrustedCertIds],
    timer:sleep(5000),
    
    ct:pal("Getting info about trust category with disabled trust certificates:"),

    {error, ?SEC_CREDU_ERR_UNSPECIFIED} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?SUCCESS, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)}),
    ct:pal("certSecCredu get tcat info with invalid ID returned: sec credu unspecified"),

    [rct_certm_lib:set_trusted_certificate_managed_state(?config(handler, Config), Tc, enabled) || Tc <- TrustedCertIds],
    timer:sleep(5000),
    ok.


%% TC-US1-9    Get TrustCategory directory name
%% Client gets TrustCategory directory name. Check that client received SEC_CREDU_OK signal
%% and that data is stored in Category

sec_credu_tcat_get_dir_name(Config) ->
    ct:pal("Getting trust category directory name: "),
    
    {ok, ?SEC_CREDU_OK, {TcatInfo}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?SUCCESS, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)}),
    {ok, ?SEC_CREDU_OK, {DirName}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_DIR, {?SUCCESS, TcatInfo}),
    
    ct:pal("Dirname: ~p", [DirName]),
    ok.

%% TC-US1-9a    Get TrustCategory directory name - NULL as Category
%% Client gets TrustCategory directory name with Category as NULL. Check that client received
%% SEC_CREDU_INVALID_PARAMETER signal.

sec_credu_tcat_get_dir_name_null(Config) ->
    ct:pal("Getting trust category directory name with NULL as category: "),
    
    {ok, ?SEC_CREDU_OK, {TcatInfo}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?SUCCESS, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_DIR, {?HANDLE_NULL, TcatInfo}),
    ct:pal("certSecCredu get tcat dirname with NULL category returned: sec credu invalid parameter"),
    ok.

%% TC-US1-10    Get number of TrustCategory certificates
%% Client gets number of TrustCategory certificates. Check that client received SEC_CREDU_OK
%% signal and that data is stored in Category

sec_credu_tcat_get_nr_of_certs(Config) ->
    ct:pal("Getting number of certificates in trust category :"),
    
    {ok, ?SEC_CREDU_OK, {TcatInfo}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?SUCCESS, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)}),
    %expecting count number 2
    {ok, ?SEC_CREDU_OK, {2}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_COUNT, {?SUCCESS, TcatInfo}),
    {ok, [_Tcert1, _Tcert2]} =  rct_certm_lib:get_all_trusted_certificates(?config(handler, Config)),
    ct:pal("Count number: ~p", [2]),
    ok.

%% TC-US1-10a    Get number of TrustCategory certificates - NULL as Category
%% Client gets number of TrustCategory certificates. Check that client received
%% SEC_CREDU_INVALID_PARAMETER signal.

sec_credu_tcat_get_nr_of_certs_null(Config) ->
    ct:pal("Getting number of certificates in NULL trust category :"),
    
    {ok, ?SEC_CREDU_OK, {TcatInfo}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_GET, {?SUCCESS, ?config(handleAddress, Config), ?config(tcatSubscriptionId, Config)}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_COUNT, {?HANDLE_NULL, TcatInfo}),
    ct:pal("certSecCredu get tcat number of certs with NULL category returned: sec credu invalid parameter"),
    ok.

%% TC-US1-11    Get trusted certificate info
%% Client gets trusted certificate info. Check that client received SEC_CREDU_OK signal
%% and that info is stored in Category

sec_credu_get_tcat_cert_info(Config) ->
    ct:pal("Getting trust certificate info:"),

    {ok, TrustedCertIds} =  rct_certm_lib:get_all_trusted_certificates(?config(handler, Config)),
    [TrustCertId1, TrustCertId2] = TrustedCertIds,   % id = 1, 2
    CertIndex1= 1,
    CertIndex2= 2,
    {ok, AllTCs} = rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    [TC1, TC2] = lists:sort(AllTCs),
    {ok, [{_Dn1, Pem11}, {_Dn2, Pem22}]} =  rct_rpc:call(rpc, certSecCredu, get_tcat_certs_and_MoRefs, [<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=tcat1">>], 10000),
    Pem1 = binary_to_list(Pem11),
    Pem2 = binary_to_list(Pem22),
    
    % {TrustCertId, TcertInfo}
     {ok, ?SEC_CREDU_OK, {TrustCertId1, TC1}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?FILEPATH, CertIndex1}),
     {ok, ?SEC_CREDU_OK, {TrustCertId1, Pem1}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?PEM, CertIndex1}),
     {ok, ?SEC_CREDU_OK, {TrustCertId2, TC2}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?FILEPATH, CertIndex2}),
     {ok, ?SEC_CREDU_OK, {TrustCertId2, Pem2}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?PEM, CertIndex2}),
    
    ct:pal("Successfuly got trust category certificates info"),

    ok.

%% TC-US1-11a    Get trusted certificate info - NULL as category
%% Client is initalized. Client Subscribes to TC. Client gets trusted certificate info
%% with NULL as Category. Check that client received SEC_CREDU_INVALID_PARAMETER.

sec_credu_get_tcat_cert_info_null(Config) ->
    ct:pal("Getting trust certificate info with NULL category:~n"),
    
    CertIndex1= 1,
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?HANDLE_NULL, ?config(tcat_info, Config), ?FILEPATH, CertIndex1}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?HANDLE_NULL, ?config(tcat_info, Config), ?PEM, CertIndex1}),
    ct:pal("certSecCredu get tcat cert with NULL as category returned message: sec credu invalid parameter"),
    ok.

%% TC-US1-11b    Get trusted certificate info - wrong format
%% Client gets trusted certificate info using wrong format. Client receives
%% SEC_CREDU_INVALID_PARAMETER signal.

sec_credu_get_tcat_cert_info_wrong_format(Config) ->
    ct:pal("Getting trust certificate info with wrong format:"),
    
    CertIndex1= 1,
    WrongFormat = 3,
    {error, ?SEC_CREDU_ERROR_UNSUPPORTED_FORMAT} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), WrongFormat, CertIndex1}),
    ct:pal("certSecCredu get tcat cert with wrong format returned message: sec credu unsupported format"),
    ok.

%% TC-US1-11c    Get trusted certificate info - wrong Index
%% Client gets trusted certificate info with wrong indeks. Check that client received
%% SEC_CREDU_INVALID_PARAMETER.

sec_credu_get_tcat_cert_info_wrong_index(Config) ->
    ct:pal("Getting trust certificate info with wrong index:"),
    
    WrongIndex = 5,
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?FILEPATH, WrongIndex}),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?PEM, WrongIndex}),
    ct:pal("certSecCredu get tcat cert with wrong index returned message: sec credu invalid parameter"),
    ok.

%% TC-US1-12    Get trusted certificate filename
%% Client gets trusted certificate filename. Check that client received SEC_CREDU_OK signal
%% and that info is stored in Category.

sec_credu_get_tcat_cert_filename(Config) ->
    ct:pal("Getting trust certificate filename:"),
    
    CertIndex1= 1,
    TrustCertId1 = "1",
    {ok, AllTCs} = rct_rpc:call(rpc, file, list_dir_all, [?CACERTSTCAT1_DIR], 10000),
    [TC1, _TC2] = lists:sort(AllTCs),
    {ok, ?SEC_CREDU_OK, {TrustCertId1, TC1}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?FILEPATH, CertIndex1}),
    ct:pal("Successfuly got tcat cert filename: ~p", [TC1]),
    ok.

%% TC-US1-12a    Get trusted certificate filename - wrong format
%% Client gets trusted certificate filename with wrong format. Check that client received
%% SEC_CREDU_ERROR_UNSUPPORTED_FORMAT.

sec_credu_get_tcat_cert_filename_wrong_format(Config) ->
    ct:pal("Getting trust certificate filename with wrong format:"),
    
    CertIndex1= 1,
    WrongFormat = 3,
    {error, ?SEC_CREDU_ERROR_UNSUPPORTED_FORMAT} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), WrongFormat, CertIndex1}),
    ct:pal("certSecCredu get tcat cert with wrong format returned message: sec credu unsupported format"),
    ok.

%% TC-US1-12b    Get trusted certificate filename - wrong index
%% Client gets trusted cerfiticate filename with wrong index. Check that client received
%% SEC_CREDU_ERROR_INVALID_PARAMETER.

sec_credu_get_tcat_cert_filename_wrong_index(Config) ->
    ct:pal("Getting trust certificate filename with wrong index:"),
    
    WrongIndex = 5,
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_TCAT_CERT, {?SUCCESS, ?config(tcat_info, Config), ?FILEPATH, WrongIndex}),
    ct:pal("certSecCredu get tcat cert with wrong index returned message: sec credu invalid parameter"),
    ok.

%% TC-US1-13    Free memory from TrustCategory data
%% Client is initalized. Client Sbscribes to TC. Client frees memory from TrustCategory
%% data and recieves Sec_CREDU_OK. Check that memory is empty.

sec_credu_free_memory_tcat(Config) ->
    ct:pal("Freeing memory from trust category data: ~n"),
    
    {ok, ?SEC_CREDU_OK} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FREE_TCAT, {?SUCCESS, ?config(tcat_info, Config), ?config(tcat_info, Config)}),
    ct:pal("Successfuly freed memory from tcat data"),
    ok.

%% TC-US1-13a   Free memory from TrustCategory data - NULL as Handle
%% Client is initalized. Client Sbscribes to TC. Client frees memory from TrustCategory
%% data with Handle set as NULL. Check that client received SEC_CREDU_INVALID_PARAMETER
%% and that memory isn't empty.
sec_credu_free_memory_tcat_null_handle(Config) ->
    ct:pal("Freeing memory from trust category data with null handle: ~n"),
    
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FREE_TCAT, {?HANDLE_NULL, ?config(tcat_info, Config), ?config(tcat_info, Config)}),
    ct:pal("certSecCredu freeing memory from tcat data with NULL handle returned message: sec credu invalid parameter"),
    ok.

%% TC-US1-18    Client makes dispatch request for NC
%% Client is initialized and subscribed to NC. Dispatch for NC is received and NC_callback is started.
%% Check if one message is received (SEC_CREDU_NC_EVENT) and SEC_CREDU_OK signal.
    
sec_credu_dispatch_one_nc(Config) ->
    ct:pal("Sending request 'dispatch' with valid handle, FD and flag~n"),
    {ok, ?SEC_CREDU_OK, 1, 0} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch was successful~n"),
    ok.

%% TC-US1-18a    Client makes dispatch request for TC
%% Client is initialized and subscribed to TC. Dispatch for TC is received and TC_callback is started.
%% Check if two messages are received (SEC_CREDU_TC_EVENT) and SEC_CREDU_OK signal.

sec_credu_dispatch_one_tcat(Config) ->
    ct:pal("Sending request 'dispatch' with valid handle, FD and flag~n"),
    {ok, ?SEC_CREDU_OK, 0, 1} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    {ok, ?SEC_CREDU_OK, 0, 1} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch was successful~n"),
    ok.

%% TC-US1-18b   Client makes dispatch request for NC and TC
%% Client is initialized and subscribed to NC and TC. Separate dispatch for NC and TC is received and both NC_callback and
%% TC_callback are started. Check if one message (SEC_CREDU_NC_EVENT for first and SEC_CREDU_TC_EVENT for second request)
%% and SEC_CREDU_OK signal are received after each dispatch request.

sec_credu_dispatch_one_nc_and_tcat(Config) ->
    ct:pal("Sending request 'dispatch' for NC with valid handle, FD and flag~n"),
    {ok, ?SEC_CREDU_OK, 1, 0} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch for NC was successful~n"),
    
    ct:pal("Sending request 'dispatch' for TCat with valid handle, FD and flag~n"),
    {ok, ?SEC_CREDU_OK, 0, 1} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    {ok, ?SEC_CREDU_OK, 0, 1} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),
    ct:pal("certSecCredu dispatch for TCat was successful~n"),
    
    ok.

%% TC-US1-18c   Client makes dispatch all request 
%% Client is initialized and subscribed to NC and TC. Dispatch all request is received and both NC_callback and
%% TC_callback are started. Check if both messages (SEC_CREDU_NC_EVENT and SEC_CREDU_TC_EVENT) are received and
%% one SEC_CREDU_OK signal.

sec_credu_dispatch_all(Config) ->
    ct:pal("Sending request 'dispatch all' with valid handle, FD and flag~n"),
    {ok, ?SEC_CREDU_OK, 1, 1} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ALL}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch all was successful~n"),
    ok.

%% TC-US1-18d   Client makes dispatch blocking request 
%% Client is initialized and subscribed to NC. Dispatch for NC and TC is received and NC_callback is started. Check 
%% if all messages are received (SEC_CREDU_NC_EVENT and SEC_CREDU_TC_EVENT) and SEC_CREDU_OK signal until
%% SEC_CREDU_FINALIZE_EVENT is received.
    
sec_credu_dispatch_blocking(Config) ->
    ct:pal("Spawning process which will send finalize event after 5 seconds"),
    spawn(?MODULE, final_block, [Config]),
    ct:pal("Sending request 'dispatch blocking' with valid handle, FD and flag~n"),
    {ok, ?SEC_CREDU_OK, 1, 1} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_BLOCKING}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch blocking was successful~n"),
    ok.

final_block(Config) ->
    timer:sleep(5000),
    ok = rct_rpc:call(rpc, certSecCredu, test_finalize, [?config(subscription, Config)], 10000).

%% TC-US1-18e   Client makes dispatch request for NC with handle set to NULL
%% Client is initialized and subscribed to NC. Dispatch for NC is received with an empty handle and NC_callback
%% is not started. Check that one message is not received (SEC_CREDU_NC_EVENT).

sec_credu_dispatch_with_empty_handle(Config) ->
    ct:pal("Sending request 'dispatch' with handle NULL~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?HANDLE_NULL, ?config(handleAddress, Config), ?config(fd, Config), ?DISPATCH_ONE}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch with handle NULL returned: sec credu invalid parameter~n"),
    ok.

%% TC-US1-18f   Client makes dispatch request for NC with FD set to wrong value
%% Client is initialized and subscribed to NC. Dispatch for NC is received with FD set to wrong value and NC_callback
%% is not started. Check that one message is not received (SEC_CREDU_NC_EVENT).

sec_credu_dispatch_with_invalid_fd(Config) ->
    ct:pal("Sending request 'dispatch' with wrong FD"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?INVALID_FD, ?DISPATCH_ONE}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch with wrong FD returned: sec credu invalid parameter~n"),
    ok.

%% TC-US1-18h   Client makes dispatch request for NC with flag set to wrong value
%% Client is initialized and subscribed to NC. Dispatch for NC is received with flag set to wrong value and NC_callback
%% is not started. Check that one message is not received (SEC_CREDU_NC_EVENT).

sec_credu_dispatch_with_invalid_flag(Config) ->
    ct:pal("Sending request 'dispatch' with wrong flag~n"),
    {error, ?SEC_CREDU_ERROR_INVALID_PARAMETER} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_DISPATCH, {?SUCCESS, ?config(handleAddress, Config), ?config(fd, Config), ?INVALID_FLAG}),        %0 for pass, 1 for null handle, -1 is wrong fd, 4 is wrong flag
    ct:pal("certSecCredu dispatch with wrong flag returned: sec credu invalid parameter~n"),
    ok.

%% TC-US1-19 certSecCredu module is started.
%% Check that process is running and that it is registered. Check that process is running and it is registered
%% after node restart.

certSecCredu_process_running_after_node_restart(_Config) ->
    check_if_process_is_running(),
    check_directory_structure(),
    check_if_process_is_registered(),
    
    rct_rpc:call(rpc, appmI, restart_node, [cold, "Checking config after restart"], 10000),
    timer:sleep(6000),
    waitForNodeUp(),
    timer:sleep(6000),
    
    check_if_process_is_running(),
    check_directory_structure(),
    check_if_process_is_registered().

%% TC-US1-20 COM restart while initialized
%% COM calls sec_credu_initialize. COM is restarted. Check that finalization is done correctly and COM is no
%% longer in subscriber list.

com_restart_while_initialize(Config) ->
    ct:pal("Restarting client after initialize"),

    rct_rpc:call(rpc, appmI, restart_node, [warm, "ManualRestart"], 10000), 
    timer:sleep(30000),
    {ok, client_started} = rct_proxy:start_proxy(node1, sec_credu1, ?CSC),
    timer:sleep(10000),
    {error, ?SEC_CREDU_ERR_SERVICE_UNAVAILABLE} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?SUCCESS, ?config(handleAddress, Config)}),

    %check that there are no more subscribers
    [] = rct_rpc:call(rpc, certSecCredu, get_subscribers_list, [], 10000),
    ct:pal("Subscriber successfuly removed."),
    ok.

com_restart_while_subd_to_nc(Config) ->
    installNC(Config),
    timer:sleep(10000),
    {ok, ?SEC_CREDU_OK, {_SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, ?config(handleAddress, Config), {3, "nc1"}}),
    timer:sleep(2000),

    ct:pal("Restarting client while subscribed to node credential"),
    rct_rpc:call(rpc, appmI, restart_node, [warm, "ManualRestart"], 10000), 
    timer:sleep(30000),
    {ok, client_started} = rct_proxy:start_proxy(node1, sec_credu1, ?CSC),
    timer:sleep(10000),
    {error, ?SEC_CREDU_ERR_SERVICE_UNAVAILABLE} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?SUCCESS, ?config(handleAddress, Config)}),

    %check that there are no more subscribers and subscriptions
    [] = rct_rpc:call(rpc, certSecCredu, get_subscribers_list, [], 10000),
    ct:pal("Subscriber successfuly removed."),
    removeNC(Config),
    ok.

com_restart_while_subd_to_tcat(Config) ->
    ct:pal("Restarting client while subscribed to trust category"),
    rct_rpc:call(rpc, appmI, restart_node, [warm, "ManualRestart"], 10000), 
    timer:sleep(30000),
    {ok, client_started} = rct_proxy:start_proxy(node1, sec_credu1, ?CSC),
    timer:sleep(10000),
    {error, ?SEC_CREDU_ERR_SERVICE_UNAVAILABLE} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?SUCCESS, ?config(handleAddress, Config)}),

    %check that there are no more subscribers and subscriptions
    [] = rct_rpc:call(rpc, certSecCredu, get_subscribers_list, [], 10000),
    ct:pal("Subscriber successfuly removed."),
    ok.

com_restart_while_subd_nc_tcat(Config) ->
    {ok, ?SEC_CREDU_OK, {_SubscriptionID}} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_NC_SUBSCRIBE, {?SUCCESS, ?config(handleAddress, Config), {3, "nc1"}}),
    timer:sleep(2000),

    ct:pal("Restarting client while subscribed to node credential and trust category"),
    rct_rpc:call(rpc, appmI, restart_node, [warm, "ManualRestart"], 10000), 
    timer:sleep(30000),
    {ok, client_started} = rct_proxy:start_proxy(node1, sec_credu1, ?CSC),
    timer:sleep(10000),
    {error, ?SEC_CREDU_ERR_SERVICE_UNAVAILABLE} = rct_proxy:send_proxy(node1, sec_credu1, ?REQ_FINALIZE, {?SUCCESS, ?config(handleAddress, Config)}),

    %check that there are no more subscribers and subscriptions
    [] = rct_rpc:call(rpc, certSecCredu, get_subscribers_list, [], 10000),
    ct:pal("Subscriber successfuly removed."),
    ok.






%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%--------------------------------------------------------------------
check_if_process_is_running() ->
    case rct_rpc:call(rpc, erlang, whereis, [certSecCredu], 10000) of
        undefined -> ct:fail("certSecCredu is not running");
        _-> ok
    end.

check_directory_structure() ->
    true =  rct_rpc:call(rpc, filelib, is_dir, [?SEC_CREDU_DIR], 10000),
    true =  rct_rpc:call(rpc, filelib, is_dir, [?CACERTS_DIR], 10000),
    true =  rct_rpc:call(rpc, filelib, is_dir, [?CERTS_DIR], 10000),
    true =  rct_rpc:call(rpc, filelib, is_dir, [?PRIVATE_DIR], 10000).

check_if_process_is_registered()->
    RegisteredProcesses = rct_rpc:call(rpc, cec_db, get_info, [], 10000),
    check_if_proccess_in_db(RegisteredProcesses).

check_if_proccess_in_db([]) ->
    ct:fail("Process is not registered!");
check_if_proccess_in_db([RegisteredProcess|RegisteredProcesses]) ->
    case RegisteredProcess of
        {?REG_PROCESS_NAME, ?MODULE_NAME, _State} -> check_if_proccess_in_db(RegisteredProcesses);
        true -> ok
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


is_handle_valid(0) ->
    nok;
is_handle_valid(_NotNull) ->
    ok.

installNC(Config) ->
    ok = rct_certm_lib:create_node_credential(?config(handler, Config), ?config(ncName, Config)),

    ok = rct_certm_lib:install_node_credential_from_uri(?config(handler, Config), ?config(ncName, Config),
                                                        ?config(ncUri, Config), ?config(uriPassword, Config), 
                                                        ?config(ncPassword, Config), ?config(ncFingerprint, Config)),
    Config.

installEmptyNC(Config) ->
    ok = rct_certm_lib:create_node_credential(?config(handler, Config), ?config(nc2Name, Config)),

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

removeEmptyNC(Config) ->
    rct_certm_lib:delete_node_credential(?config(handler, Config), ?config(nc2Name, Config)),
    
    Config.

removeTC(Config) ->
    rct_certm_lib:delete_trust_category(?config(handler, Config), ?config(tcName, Config)),
    
    {ok, TrustCerts} =  rct_certm_lib:get_all_trusted_certificates(?config(handler, Config)),
    [ rct_certm_lib:delete_trusted_certificate(?config(handler, Config), Tc) || Tc <- TrustCerts],
    
    Config.

check_content_in_file(File, Content, Path) ->
    BinaryContent = case erlang:is_list(Content) of
                        true ->
                            rct_rpc:call(rpc, erlang, list_to_binary, [Content], 10000);
                        false ->
                            Content
                    end,
    {ok, BinaryContent} = rct_rpc:call(rpc, file ,read_file, [Path ++ File], 10000),
    ok.

check_pem_files_for_certs([Head|Tail], ListSize, [Content|Rest], Path) ->
    Head = "tc" ++ erlang:integer_to_list(ListSize) ++ ?TC_CERT_EXT,
    ok = check_content_in_file(Head, Content, Path),
    check_pem_files_for_certs(Tail, ListSize-1, Rest, Path);
check_pem_files_for_certs([], 0, [], _Path) ->
    ok.


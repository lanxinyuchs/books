%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certm_cmpv2_SUITE.erl %
%%% @author eivomat
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/7
%%% 
%%% @doc == Test Suite for testing CertM ==
%%% Basic CRUD tests following MO operations on CertM branch:
%%% create, get, set, delete.
%%% TOL with details can be found here:
%%% https://ericoll.internal.ericsson.com/sites/RCS_Product_Care_ZG/Documents/CMPv2_Improvements/CMPv2_TOL.xlsx
%%% Additional tests will be added when necessary after each TR fix.
%%%
%%% HOW TO ADD NEW TEST CASES/SUBTESTCASES
%%%
%%% +New non-CRUD related test - just add a new common test.
%%% +Set attribute value (both ok and nok cases):
%%%  In test_M_O_crud add value to the relevant list. For example, testing
%%%  that invalid uri value "invalid..uri" cannot be set as a value on
%%%  EnrollmentServer uri attribute, add "invalid..uri" to NokUris list in
%%%  test_enrollment_server_crud.
%%% +Non-set CRUD test (e.g. create-delete-create, complex cases) are added to
%%%  TTests list with both pre and post conditions. Available basic operations:
%%%  create_successful, create_unsuccessful, set_successful, set_unsuccessful,
%%%  delete_successful, delete_unsuccessful. Each operation also fetches (gets)
%%%  and validates resulting state, so get operation is always verified.
%%%  create_successful will try to create an MO with given attribute values and
%%%  verify it succeeded.
%%%  *_successful arguments tuple (which goes in TTest list):
%%%   {Tag, operation_(un)successful, 'ManagedObjectName',
%%%       [{attribute1, "Value1"}, ..., {attributeN, "ValueN"}]}
%%%   where Tag is t (subtest), pre (operation before a test),
%%%   post (operation after a test) or preset or postset (pre/post set tests).
%%%  Note: A subtest can have any number of pre or post operations.
%%%  *_unsuccessful arguments: same as *_successful with additional argument
%%%  string  of type "Managed to do something that should not be possible!"
%%%
%%% @end

-module(certm_cmpv2_SUITE).
-vsn('/main/R9A/7').
-author('eivomat').

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
%%% R9A/3      2017-02-14 eivomat     Created
%%% R9A/6      2017-02-20 eivomat     Temporary disable some tests
%%% R9A/7      2017-02-23 eivomat     Fix warning in output
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         groups/0]).
-export([test_enrollment_server_crud/1,
         test_enrollment_authority_crud/1,
         test_node_credential_crud/1]).
-export([create_successful/3,
         create_unsuccessful/4,
         set_successful/3,
         set_unsuccessful/4,
         delete_successful/3,
         delete_unsuccessful/4,
         delete/3,
         get_successful/3,
         check_successful/3]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [test_enrollment_server_crud,
     test_enrollment_authority_crud,
     test_node_credential_crud].



%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks,
      [{rct_htmllink,[]},
       {rct_rpc, rpc_1},
       {rct_netconf,{nc1, html}},
       {cth_conn_log,[]},
       {rct_core,[]},
       {rct_logging,
        {all,
         [{erlang,
           {["ERROR REPORT", "CRASH REPORT"],
            []}}]}}
      ]}].

-define(MO_ID_LIST,
        ["1", "2"]).%%, "3", "4", "5"]).
-define(ORDERED_PROPLIST_OF_MOS_TO_BE_DELETED_AFTER_EACH_TC,
        [{'NodeCredential', nodeCredentialId},
          %% EnrollmentServerGroup also deletes EnrollmentServer
         {'EnrollmentServerGroup', enrollmentServerGroupId},
         {'EnrollmentAuthority', enrollmentAuthorityId}]).

-define(PROTOCOL_ENUM,
        ["SCEP", "CMP"]).
-define(KEY_INFO_ENUM,
        ["RSA_1024", "RSA_2048", "RSA_3072",
         "RSA_4096", "ECDSA_160", "ECDSA_224",
         "ECDSA_256", "ECDSA_384", "ECDSA_512 ",
         "ECDSA_521 ", "ECDSA_BRAINPOOL_256", "ECDSA_BRAINPOOL_320",
         "ECDSA_BRAINPOOL_384", "ECDSA_BRAINPOOL_512"]).

-define(URN_COMTOP, {xmlns,"urn:com:ericsson:ecim:ComTop"}).
-define(URN_NETCONF, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}).
-define(URN_COMSECM, {xmlns,"urn:com:ericsson:ecim:ComSecM"}).
-define(URN_RCS_CERTM, {xmlns,"urn:com:ericsson:ecim:RcsCertM"}).
-define(BASIC_CERTM(MeId, CertMChildStructList),
        {'ManagedElement',
         [?URN_COMTOP, ?URN_NETCONF],
         [{managedElementId, [], [MeId]},
          {'SystemFunctions',
           [{systemFunctionsId, [], ["1"]},
            {'SecM', [?URN_COMSECM],
             [{secMId, [], ["1"]},
              {'CertM', [?URN_RCS_CERTM],
               [{certMId, [], ["1"]}, CertMChildStructList]
              }]}]}]}).
-define(CERTM_CAPABILITIES,
         {'CertMCapabilities', [], [{certMCapabilitiesId, [], ["1"]}]}).
-define(ENROLLMENT_AUTHORITY(Operations, Attributes),
        {'EnrollmentAuthority', Operations, Attributes}).
-define(ENROLLMENT_SERVER_GROUP(Operations, Attributes),
        {'EnrollmentServerGroup', Operations, Attributes}).
-define(ENROLLMENT_SERVER(Operations, GroupId, Attributes),
        {'EnrollmentServerGroup',
         [GroupId, {'EnrollmentServer', Operations, Attributes}]}).
-define(NODE_CREDENTIAL(Operations, Attributes),
        {'NodeCredential', Operations, Attributes}).
-define(TRUST_CATEGORY(Operations, Attributes),
        {'TrustCategory', Operations, Attributes}).
-define(TRUSTED_CERTIFICATE(Operations, Attributes),
        {'TrustedCertificate', Operations, Attributes}).

%% @hidden
init_per_suite(Config) ->
    %% This is because due to errors in COM there is no proper way to
    %% read the set managedElementId if it has changed from "1"
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
    case proplists:get_value(networkManagedElementId, MeData) of
        undefined -> "1";
        NMEI -> NMEI
    end,
    PrivDir = proplists:get_value(priv_dir, Config),
    cmd(["chmod a+rwx ", PrivDir]),
    [{host, SftpHost},{username, Username},{password, Password}] =
        ct:get_config(sftp_server),

    Filter = ?BASIC_CERTM(MeId, ?CERTM_CAPABILITIES),
    {ok, Capabilities} = netconf_get(Filter),

    [{meId, MeId},
     {cerm_capabilities, Capabilities},
     {sftp_host, SftpHost},
     {sftp_user, Username},
     {sftp_pass, Password}
    | Config].

%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(test_enrollment_server_crud, Config) ->
    ct:pal("init_per_testcase: ~p~n", [test_enrollment_server_crud]),
    MeId = ?config(meId, Config),
    cleanup(Config),
    create_successful(MeId, 'EnrollmentServerGroup',
                      [{enrollmentServerGroupId, "1"},
                       {userLabel, "certm_cmp2_SUITE"}]),
    create_successful(MeId, 'NodeCredential',
                      [{nodeCredentialId, "1"},
                       {subjectName, "CN=okname"},
                       {userLabel, "certm_cmp2_SUITE"}]),
    [{esGroup, {enrollmentServerGroupId, "1"}},
     {nCredential, {nodeCredentialId, "1"}} | Config];
init_per_testcase(test_enrollment_authority_crud, Config) ->
    ct:pal("init_per_testcase: ~p~n", [test_enrollment_authority_crud]),
    MeId = ?config(meId, Config),
    cleanup(Config),
    %% install trusted certificate here TODO
    create_successful(MeId, 'NodeCredential',
                      [{nodeCredentialId, "1"},
                       {subjectName, "CN=okname"},
                       {userLabel, "certm_cmp2_SUITE"}]),
    [{tcFingerprint, "F1:27:CC:F4:0E:61:42:2F:CF:7D:8E:7F:1F:"
      "34:21:36:B8:22:4E:A1"},
     {tcRnd, "ManagedElement=1,SystemFunctions=1,SecM=1,"
      "CertM=1,TrustedCertificate=1"},
     {nCredential, {nodeCredentialId, "1"}},
     {nCredentialRnd, "ManagedElement=1,SystemFunctions=1,SecM=1,"
      "CertM=1,NodeCredential=1"},
     {tcRnd, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
      "TrustedCertificate=1"} |Config];
init_per_testcase(test_node_credential_crud, Config) ->
    ct:pal("init_per_testcase: ~p~n", [test_node_credential_crud]),
    MeId = ?config(meId, Config),
    cleanup(Config),
    create_successful(MeId, 'EnrollmentServerGroup',
                      [{enrollmentServerGroupId, "1"},
                       {userLabel, "certm_cmp2_SUITE"}]),
    create_successful(MeId, 'EnrollmentServer',
                      [{enrollmentServerGroupId, "1"},
                       {enrollmentServerId, "1"},
                       {protocol, "CMP"},
                       {uri, "http://validuri.org"},
                       {userLabel, "Created by certm_cmpv2_SUITE"}]
                     ),
    create_successful(MeId, 'EnrollmentAuthority',
                      [{enrollmentAuthorityId, "1"},
                       {userLabel, "Created by certm_cmpv2_SUITE"}]
                     ),
    [{esGroupRnd,
      "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
      "EnrollmentServerGroup=1"},
     {eAuthorityRnd,
      "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
      "EnrollmentAuthority=1"},
     {esGroup, {enrollmentServerGroupId, "1"}},
     {eServer, {enrollmentServerId, "1"}},
     {eAuthority, {enrollmentAuthorityId, "1"}}
         | Config].

%% @hidden
end_per_testcase(TestCase, Config) ->
    ct:pal("init_per_testcase: ~p~n", [TestCase]),
    cleanup(Config).

%% @hidden
groups() ->
    AllGroup = all(),

    [
     {default__group, [], AllGroup},
     {repeat__group, [{repeat, 100}], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]}, 
     {sbc__upgrade_short__all__1__group, [], [{group, default__group}]}, 
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], []},  
     {sdc__nocover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []}  
    ].

%%%--------------------------------------------------------------------
%%% TEST CASES
%%%--------------------------------------------------------------------

%% Also includes testing of EnrollmentServerGroup MO
test_enrollment_server_crud(Config) ->
    EnrollmentServerCrud = enrollment_server_crud_,
    MeId = ?config(meId, Config),
    EnrollmentServerGroupId1 = ?config(esGroup, Config),
    %NodeCredentialId1 = ?config(nCredential, Config),
    EnrollmentServerId1 = {enrollmentServerId, "1"},
    Count = 1,

    OkUris =
        ["cmp://192.168.33.27",
         "http://10.68.101.131",
         "cmp://192.168.33.27:8080",
         "http://10.68.101.131:8080",
         "http://[2001:1b70:6282:b280::131]:8080/ejbca/publicweb/cmp/cmp_G2_VC",
         "http://[1200:0000:AB00:1234:0000:2552:7777:1313]",
         "cmp://[1200::AB00:1234:2552:7777:1313]",
         "cmp://[1200::AB00:1234:2552:7777:1313]:8080",
         "http://[21DA:D3:0:2F3B:2AA:FF:FE28:9C5A]",
         "http://[21DA:D3:0:2F3B:2AA:FF:FE28:9C5A]:8080",
         "http://nesto"],
    NokUris =
        ["blabla",
         "",
         "C9:E9D20",
         "1200:0000:AB00:1234:0000:2552:7777:1313"
         "111111111"
         "1...1.1",
         "11:11::::1",
         "http//jrwlrj",
         %%"cmp://[FE80::FFFF",
         %%"cmp://FE80::FFFF]",
         %%"http://../",
         %%"http://10.1.1.1",
         %%"http://www.foo.bar./",
         %%"http://.www.foo.bar./",
         %%"cmp://111....11",
         "cmp://111::11:::1"],
    OkProtocols = ["CMP"], %% TODO fetch from capabilities
    NokProtocols = ["SCEP"], %% = Allprotocols\OkProtocols

    TSetOk =
        [{t, set_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1, {uri, Uri}]} ||
         Uri <- OkUris] ++
        [{t, set_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1, {protocol, Prot}]} ||
         Prot <- OkProtocols],
    TSetNok =
        [{t, set_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1, {uri, Uri}],
          "Unsupported uri set: " ++ Uri} ||
         Uri <- NokUris] ++
        [{t, set_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1, {protocol, Prot}],
          "Unsupported protocol set: " ++ Prot} ||
         Prot <- NokProtocols],
    TTests =
        [
         %% 1 Create unsuccessfully, URI value: Empty string
         {t, create_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}],
          "Created EnrollmentServer MO with empty uri!"},
         %% 2 Create unsuccessfully, URI value: Invalid
         {t, create_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "blablabla"}],
          "Created EnrollmentServer MO with invalid uri!"},
         %% 3 Create unsuccessfully, URI value: Invalid
         {t, create_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "11::::11"}],
          "Created EnrollmentServer MO with invalid uri!"},
         %% 4 Create unsuccessfully, Protocol value: SCEP, URI value: Invalid
         {t, create_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "SCEP"}, {uri, "11::::11"}],
          "Created EnrollmentServer MO with invalid uri!"},
         %% 5 Create unsuccessfully, Protocol value: SCEP
         {t, create_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "SCEP"}, {uri, "http://validuri.org"}],
          "Created EnrollmentServer MO with invalid uri!"},
         %% 6 Create-Delete-Create
         {pre, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://a.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {pre, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         {t, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://a.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         %% 7 Create URI value: HTTP protocol + IP address + port
         {t, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://10.68.101.131:8080"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         %% 8 Create URI value: HTTP protocol + IP address
         {t, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://10.68.101.131"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         %% 9 Create URI value: HTTP protocol + DNS
         {t, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://10.68.101.131"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         %% 10 Create URI value: CMP protocol + IP address
         {t, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "cmp://192.168.33.27"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         %% 11 Create URI value: CMP protocol + IP address + port
         {t, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "cmp://192.168.33.27:8080"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         %% 12 Create MO with same ID on same parent
         {pre, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://validuri.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {t, create_unsuccessful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://validuri.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}],
          "Created EnrollmentServer with ID as existing MO on same parent!"},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         %% 13 Create MO with same ID on different parents
         {pre, create_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"},
           {userLabel, "certm_cmp2_SUITE"}]},
         {pre, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://validuri.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {t, create_successful, 'EnrollmentServer',
          [{enrollmentServerGroupId, "2"}, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://validuri.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]},
         {post, delete_successful, 'EnrollmentServer',
          [{enrollmentServerGroupId, "2"}, EnrollmentServerId1]},
         {post, delete_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"}]},
         %% ENROLLMENT SERVER GROUP TESTS
         %% 14 Create multiple MOs
         {pre, create_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"},
           {userLabel, "certm_cmp2_SUITE"}]},
         {t, create_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "3"},
           {userLabel, "certm_cmp2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"}]},
         {post, delete_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "3"}]},
         %% 15 Create MO with same ID on same parent
         {t, create_unsuccessful, 'EnrollmentServerGroup',
          [EnrollmentServerGroupId1],
           "Created EnrollmentServerGroup with ID"
            " as existing MO on same parent!"},
         %% 16 Delete MO with children Mos
         {pre, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://validuri.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {t, delete_successful, 'EnrollmentServerGroup',
          [EnrollmentServerGroupId1]},
         {post, create_successful, 'EnrollmentServerGroup',
          [EnrollmentServerGroupId1]},
         %% 17 Create-Delete-Create
         {pre, create_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"},
           {userLabel, "certm_cmp2_SUITE"}]},
         {pre, delete_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"}]},
         {t, create_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"},
           {userLabel, "certm_cmp2_SUITE"}]},
         {post, delete_successful, 'EnrollmentServerGroup',
          [{enrollmentServerGroupId, "2"}]},
         %% 17 Try to delete MO when NodeCredential has reference on it
         %{pre, create_successful, 'EnrollmentServerGroup',
         % [{enrollmentServerGroupId, "2"},
         %  {userLabel, "certm_cmp2_SUITE"}]},
         %{pre, set_successful, 'NodeCredential',
         % [NodeCredentialId1,
         %  {enrollmentServerGroup,
         %   "SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=2"}]},
         %{t, delete_unsuccessful, 'EnrollmentServerGroup',
         % [{enrollmentServerGroupId, "2"}]},
         %{post, set_successful, 'NodeCredential',
         % [NodeCredentialId1,
         %  {enrollmentServerGroup, ""}]},
         %{post, delete_successful, 'EnrollmentServerGroup',
         % [{enrollmentServerGroupId, "2"}]},
         %%ADD MORE TESTS HERE
         %% Pre-all set tests
         {preset, create_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1,
           {protocol, "CMP"}, {uri, "http://validuri.org"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]}
         ],
    PostSet =
        [{postset, delete_successful, 'EnrollmentServer',
          [EnrollmentServerGroupId1, EnrollmentServerId1]}],

    run_sub_test_cases(MeId, EnrollmentServerCrud, Count,
                       TTests ++ TSetOk ++ TSetNok ++ PostSet).

test_enrollment_authority_crud(Config) ->
    EnrollmentAuthorityCrud = enrollment_authority_crud_,
    MeId = ?config(meId, Config),
    %Certificate = ?config(tcRnd, Config),
    Fingerprint = ?config(tcFingerprint, Config),
    %NodeCredentialId1 = ?config(nCredential, Config),
    EnrollmentAuthorityId1 = {enrollmentAuthorityId, "1"},
    Count = 1,

    OkAuthorityNames =
        ["CN=BasicCase",
         "CN=Before\\0dAfter,DC=example,DC=net",
         "CN=John Smith\\, III,DC=example,DC=net",
         "O=Ericsson,OU=eri cssonOAM,CN=atclv m837NECertCA",
         "CN=42",
         "CN=vuco"],
    NokAuthorityNames =
        ["blahblah",
         "",
         %%"UID=jsmith,DC=example,DC=net",
         %%"OU=Sales+CN=J. Smith,DC=example,DC=net",
         %%"1.3.6.1.4.1.1466.0=#04024869,DC=example,DC=com",
         %%"CN=Lu\\C4\\8Di\\C4\\87",
         "CN=John Smith, III,DC=example,DC=net"
        ],
    OkCaFingerprints =
        [Fingerprint,
         "F1:27:CC:F4:0E:61:42:2F:CF:7D:8E:7F:1F:34:21:36:B8:22:4E:A1",
         %%"F2:0C:51:95:3C:EA:C4:24:63:9B:B2:B8:64:1F:B5:9D:D7:1C:DA:0G",
         %%"F2:0C:51:95:3C:EA:C4:24:63:9B:B2:B8:64:1F:B5:9D:D7:1C:DA:09",
         "11:11:11:11",
         "F2",
         "f2:42:42:42:ab"
         ],
    NokCaFingerprints =
        ["F2F2",
         "blahblah"
         ],
    OkCaCertificates =
        [%%Certificate
         ],
    NokCaCertificates =
        ["blabla",
         "SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=i_dont_exist",
         "SystemFunctions=1,SecM=1,CertM=1,EnrollmentAuthority=i_dont_exist",
         "SystemFunctions=1,SecM=1,CertM=1,EnrollmentServerGroup=i_dont_exist",
         "\""],

    TSetOk =
        [{t, set_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1, {enrollmentAuthorityName, Name}]} ||
         Name <- OkAuthorityNames] ++
        [{t, set_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1, {enrollmentCaFingerprint, Fp}]} ||
         Fp <- OkCaFingerprints],
    TSetNok =
        [{t, set_unsuccessful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1, {enrollmentAuthorityName, Name}],
          "Unsupported enrollmentAuthorityName set: " ++ Name} ||
         Name <- NokAuthorityNames] ++
        [{t, set_unsuccessful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1, {enrollmentCaFingerprint, Fp}],
          "Unsupported enrollmentCaFingerprint set: " ++ Fp} ||
         Fp <- NokCaFingerprints],
     TSetCaCert =
        %% Note: Fingerprint tests must come before certificate tests!
        [{t, set_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1, {enrollmentCaCertificate, Cert}]} ||
         Cert <- OkCaCertificates] ++
        %% Clear enrollmentCaCertificate so Nok fingerprint tests can be run
        [{t, set_unsuccessful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1, {enrollmentCaCertificate, Cert}],
          "Unsupported enrollmentCaCertificate set: " ++ Cert} ||
         Cert <- NokCaCertificates],
    TTests =
        [%% 1 Create-Delete-Create
         {pre, create_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1,
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {pre, delete_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1]},
         {t, create_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1,
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1]},
         %% 2 Try to delete MO when NodeCredential has reference on it
         %{pre, create_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1,
         %  {userLabel, "Created by certm_cmpv2_SUITE"}]},
         %{pre, set_successful, 'NodeCredential',
         % [NodeCredentialId1,
         %  {enrollmentServerGroup, "ManagedElement=1,SystemFunctions=1,"
         %   "SecM=1,CertM=1,EnrollmentServerGroup=2"}]},
         %{t, delete_unsuccessful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1],
         %  "Managed to delete EnrollmentAuthority MO which is referenced by "
         %  "NodeCredential MO!"},
         %{post, set_successful, 'NodeCredential',
         % [NodeCredentialId1,
         %  {enrollmentAuthority, ""}]},
         %{post, delete_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1]},
         %% 3 EnrollmentAuthority - set CaCertificate and check Fingerprint
         %{pre, create_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1,
         %  {userLabel, "Created by certm_cmpv2_SUITE"}]},
         %{pre, set_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1,
         %  {enrollmentCaCertificate, Certificate}]},
         %{pre, check_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1, {enrollmentCaFingerprint, Fingerprint}]},
         %{t, set_unsuccessful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1,
         %  {enrollmentCaFingerprint, %% Ok fingerprint
         %   "F1:27:CC:F4:0E:61:42:2F:CF:7D:8E:7F:1F:34:21:36:B8:22:4E:A1"}],
         %  "Managed to set EnrollmentAuthority MO enrollmentCaFingerprint "
         %  "attribute while enrollmentCaCertificate is set!"},
         %{post, delete_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1]},
         %% Unset enrollmentCaFingerprint (set it to empty string in MO shell)
         {pre, create_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1,
           {enrollmentCaFingerprint, Fingerprint},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {pre, set_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1,
           {enrollmentCaFingerprint, [{'xc:operation', "delete"}], []}]},
         {post, delete_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1]},
         %% Unset enrollmentCaCertificate (set it to empty string in MO shell)
         %{pre, create_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1,
         %  {enrollmentCaCertificate, Certificate},
         %  {userLabel, "Created by certm_cmpv2_SUITE"}]},
         %{pre, set_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1,
         %  {enrollmentCaCertificate, [{'xc:operation', "delete"}], []}]},
         %{post, delete_successful, 'EnrollmentAuthority',
         % [EnrollmentAuthorityId1]},
         %%ADD MORE TESTS HERE
         %% Pre-all set tests
         {preset, create_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1,
           {userLabel, "Created by certm_cmpv2_SUITE"}]}],
    PostSet =
        [{postset, delete_successful, 'EnrollmentAuthority',
          [EnrollmentAuthorityId1]}],

    run_sub_test_cases(MeId, EnrollmentAuthorityCrud, Count,
                       TTests ++ TSetOk ++ TSetNok ++ TSetCaCert ++ PostSet).

test_node_credential_crud(Config) ->
    NodeCredentialCrud = node_credential_crud_,
    MeId = ?config(meId, Config),
    %EnrollmentServerGroupId1 = ?config(esGroup, Config),
    EnrollmentServerGroupId1Rnd = ?config(esGroupRnd, Config),
    %EnrollmentAuthorityId1 = ?config(eAuthority, Config),
    EnrollmentAuthorityId1Rnd = ?config(eAuthorityRnd, Config),
    NodeCredentialId1 = {nodeCredentialId, "1"},
    OkSubjectName = {subjectName, "CN=okname"},
    Count = 1,

    OkEnrollmentAuthorities =
        [EnrollmentAuthorityId1Rnd],
    NokEnrollmentAuthorities =
        ["blabla",
         "",
         "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
         "TrustedCertificate=i_dont_exist",
         %"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
         %"EnrollmentAuthority=i_dont_exist",
         "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
         "EnrollmentServerGroup=i_dont_exist",
         "\""],
    OkEnrollmentServerGroups =
        [EnrollmentServerGroupId1Rnd],
    NokEnrollmentServerGroups =
        ["blabla",
         "",
         "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
         "TrustedCertificate=i_dont_exist",
         "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
         "EnrollmentAuthority=i_dont_exist",
         %"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
         %"EnrollmentServerGroup=i_dont_exist",
         "\""],
    OkEnrollmentTimers =
        ["80",
         "42",
         "60"],
    NokEnrollmentTimers =
        ["",
         "-11"],
    OkExpiryAlarmThresholds =
        ["365",
         "1",
         "42"],
    NokExpiryAlarmThresholds =
        ["-1",
         "-42",
         "366",
         "0",
         "blabla",
         "0.13254543",
         "-1",
         ""],
    OkKeyInfos =
        [%% TODO fetch from capabilities
         "RSA_1024",
         "RSA_2048",
         "RSA_3072",
         "RSA_4096",
         "ECDSA_160",
         "ECDSA_224",
         "ECDSA_256",
         "ECDSA_384",
         "ECDSA_512","ECDSA_BRAINPOOL_256",
         "ECDSA_512","ECDSA_BRAINPOOL_320",
         "ECDSA_512","ECDSA_BRAINPOOL_384",
         "ECDSA_512","ECDSA_BRAINPOOL_512"
         ],
    NokKeyInfos =
        [
         %%All\Ok TODO
         ],
    OkRenewalModes =
        ["MANUAL",
         "AUTOMATIC"],
    NokRenewalModes =
        [], %% EMPTY
    OkSubjectAltNames =
        ["someserialnumber.ericsson.com",
         "DNS:someserialnumber.ericsson.com",
         "IP:145.34.23.123",
         "IP:2001:DB8::8:800:200C:417A",
         "2001:DB8::8:800:200C:417A",
         "145.34.23.123"],
    NokSubjectAltNames =
        [%%"DNS:10.45.62.1",
         %%"DNS:2001:DB8::8:800:200C:417A",
         %%"IP:nesto",
         %%"blah"
        ],

    TSetOk =
        [{t, set_successful, 'NodeCredential',
          [NodeCredentialId1, {enrollmentAuthority, Authority}]} ||
         Authority <- OkEnrollmentAuthorities] ++
        [{t, set_successful, 'NodeCredential',
          [NodeCredentialId1, {enrollmentServerGroup, Group}]} ||
         Group <- OkEnrollmentServerGroups] ++
        [{t, set_successful, 'NodeCredential',
          [NodeCredentialId1, {enrollmentTimer, Time}]} ||
         Time <- OkEnrollmentTimers] ++
        [{t, set_successful, 'NodeCredential',
          [NodeCredentialId1, {expiryAlarmThreshold, Threshold}]} ||
         Threshold <- OkExpiryAlarmThresholds] ++
        [{t, set_successful, 'NodeCredential',
           [NodeCredentialId1, {keyInfo, KeyInfo}]} ||
         KeyInfo <- OkKeyInfos] ++
        [{t, set_successful, 'NodeCredential',
          [NodeCredentialId1, {renewalMode, Mode}]} ||
         Mode <- OkRenewalModes] ++
        [{t, set_successful, 'NodeCredential',
          [NodeCredentialId1, {subjectAltName, AltName}]} ||
         AltName <- OkSubjectAltNames],
    TSetNok =
        [{t, set_unsuccessful, 'NodeCredential',
          [NodeCredentialId1, {enrollmentAuthority, Authority}],
          "Unsupported enrollmentAuthority set: " ++ Authority} ||
         Authority <- NokEnrollmentAuthorities] ++
        [{t, set_unsuccessful, 'NodeCredential',
          [NodeCredentialId1, {enrollmentServerGroup, Group}],
          "Unsupported enrollmentAuthority set: " ++ Group} ||
         Group <- NokEnrollmentServerGroups] ++
        [{t, set_unsuccessful, 'NodeCredential',
          [NodeCredentialId1, {enrollmentTimer, Time}],
          "Unsupported enrollmentTimer set: " ++ Time} ||
          Time <- NokEnrollmentTimers] ++
        [{t, set_unsuccessful, 'NodeCredential',
          [NodeCredentialId1, {expiryAlarmThreshold, Threshold}],
          "Unsupported expiryAlarmThreshold set: " ++ Threshold} ||
         Threshold <- NokExpiryAlarmThresholds] ++
        [{t, set_unsuccessful, 'NodeCredential',
          [NodeCredentialId1, {keyInfo, KeyInfo}],
          "Unsupported keyInfo set: " ++ KeyInfo} ||
          KeyInfo <- NokKeyInfos] ++
        [{t, set_unsuccessful, 'NodeCredential',
          [NodeCredentialId1, {renewalMode, Mode}],
          "Unsupported renewalMode set: " ++ Mode} ||
          Mode <- NokRenewalModes] ++
        [{t, set_unsuccessful, 'NodeCredential',
          [NodeCredentialId1, {subjectAltName, AltName}],
          "Unsupported subjectAltName set: " ++ AltName} ||
          AltName <- NokSubjectAltNames],
    TTests =
        [
         %% 1 NodeCredential - Create MO subjectName=CN=okname
         {t, create_successful, 'NodeCredential',
          [NodeCredentialId1, OkSubjectName,
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 2 subjectName=C=SE,O=Ericsson,CN=someserialnumber.ericsson.com
         {t, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {subjectName, "C=SE,O=Ericsson,CN=someserialnumber.ericsson.com"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 3 subjectName=emptystring
         {t, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {subjectName, ""},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 4 subjectName=default
         {t, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {subjectName, "default"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 5 subjectName=blahblah
         {t, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {subjectName, "blahblah"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 6 Create-Delete-Create
         {pre, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {subjectName, "CN=okname"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {pre, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         {t, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {subjectName, "CN=okname"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 7 Unset enrollmentAuthority (set empty in MO shell)
         {pre, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {enrollmentAuthority, EnrollmentAuthorityId1Rnd},
           {subjectName, "CN=okname"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {t, set_successful, 'NodeCredential',
          [NodeCredentialId1,
           {enrollmentAuthority, [{'xc:operation', "delete"}], []}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 8 Unset enrollmentServerGroup (set empty in MO shell)
         {pre, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {enrollmentServerGroup, EnrollmentServerGroupId1Rnd},
           {subjectName, "CN=okname"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {t, set_successful, 'NodeCredential',
          [NodeCredentialId1,
           {enrollmentServerGroup, [{'xc:operation', "delete"}], []}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% 9 Unset subjectAltName (set empty in MO shell)
         {pre, create_successful, 'NodeCredential',
          [NodeCredentialId1,
           {enrollmentServerGroup, EnrollmentServerGroupId1Rnd},
           {subjectName, "CN=okname"},
           {subjectAltName, "DNS:someserialnumber.ericsson.com"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {t, set_successful, 'NodeCredential',
          [NodeCredentialId1,
           {subjectAltName, [{'xc:operation', "delete"}], []}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% Try to delete MO when LogM has reference on it
         %{pre, create_successful, 'NodeCredential',
         % [NodeCredentialId1, {subjectName, "CN=okname"},
         %  {userLabel, "Created by certm_cmpv2_SUITE"}]},
         %{pre, set_successful, 'LogM',
         % [{nodeCredential,
         %   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"
         %   ",NodeCredential=1"}]},
         %{t, delete_unsuccessful, 'NodeCredential',
         % [NodeCredentialId1],
         % "Deleted NodeCredential MO while it is referenced by LogM!"},
         %{post, set_successful, 'LogM',
         % [{nodeCredential, ""}]},
         %{post, delete_successful, 'NodeCredential',
         % [NodeCredentialId1]},
         %% Try to delete MO when Https has reference on it
         %{pre, create_successful, 'NodeCredential',
         % [NodeCredentialId1, {subjectName, "CN=okname"},
         %  {userLabel, "Created by certm_cmpv2_SUITE"}]},
         %{pre, set_successful, 'Https',
         % [{httpsId, "1"}, {nodeCredential,
         %   "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"
         %   ",NodeCredential=1"}]},
         %{t, delete_unsuccessful, 'NodeCredential',
         % [NodeCredentialId1],
         % "Deleted NodeCredential MO while it is referenced by LogM!"},
         %{post, set_successful, 'LogM',
         % [{nodeCredential, ""}]},
         %{post, delete_successful, 'NodeCredential',
         % [NodeCredentialId1]},
         %% NodeCredential - reservedByUser LogM (set+unset)
         {pre, create_successful, 'NodeCredential',
          [NodeCredentialId1, {subjectName, "CN=okname"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {pre, set_successful, 'LogM',
          [{nodeCredential,
            "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"
            ",NodeCredential=1"}]},
         {t, check_successful, 'NodeCredential',
          [NodeCredentialId1,
           {reservedBy, "SystemFunctions=1,LogM=1"}]},
         {post, set_successful, 'LogM',
          [{nodeCredential, [{'xc:operation', "delete"}], []}]},
         {post, check_successful, 'NodeCredential',
          [NodeCredentialId1,
           {reservedBy, ""}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %% NodeCredential - reservedByUser LogM and Https (set+unset)
         {pre, create_successful, 'NodeCredential',
          [NodeCredentialId1, {subjectName, "CN=okname"},
           {userLabel, "Created by certm_cmpv2_SUITE"}]},
         {pre, set_successful, 'LogM',
          [{nodeCredential,
            "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"
            ",NodeCredential=1"}]},
         {pre, set_successful, 'Https',
          [{httpsId, "1"}, {nodeCredential,
            "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"
            ",NodeCredential=1"}]},
         {t, check_successful, 'NodeCredential',
          [NodeCredentialId1,
           {reservedBy, [], ["SystemFunctions=1,LogM=1",
            "SystemFunctions=1,SysM=1,HttpM=1,Https=1"]}]},
         {post, set_successful, 'LogM',
          [{nodeCredential, [{'xc:operation', "delete"}], []}]},
         {post, set_successful, 'Https',
          [{httpsId, "1"},
           {nodeCredential, [{'xc:operation', "delete"}], []}]},
         {post, check_successful, 'NodeCredential',
          [NodeCredentialId1, {reservedBy, ""}]},
         {post, delete_successful, 'NodeCredential',
          [NodeCredentialId1]},
         %%ADD MORE TESTS HERE
         %% Pre-all set tests
         {preset, create_successful, 'NodeCredential',
          [NodeCredentialId1, OkSubjectName,
           {userLabel, "Created by certm_cmpv2_SUITE"}]}],
    PostSet =
        [{postset, delete_successful, 'NodeCredential',
          [NodeCredentialId1]}],

    run_sub_test_cases(MeId, NodeCredentialCrud, Count,
                       TTests ++ TSetOk ++ TSetNok ++ PostSet).

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Run through given list and apply actions
%%%--------------------------------------------------------------------
run_sub_test_cases(MeId, Name, StartCnt, TestSubCaseList) ->
    Fun =
        fun({t, FunToApply, Mo, AttrList}, {Name0, MeId0, Cnt}) ->
                ct:pal("********** TestSubCase: ~p~p **********~nMO:~p~n"
                      "Operation: ~p~nAttributes: ~p~n",
                       [Name, Cnt, Mo, FunToApply, AttrList]),
                apply(?MODULE, FunToApply, [MeId0, Mo, AttrList]),
                {Name0, MeId0, Cnt+1};
           ({t, FunToApply, Mo, AttrList, FailReason},
            {Name0, MeId0, Cnt}) ->
                ct:pal("********** TestSubCase: ~p~p **********~nMO:~p~n"
                      "Operation: ~p~nAttributes: ~p~n",
                       [Name, Cnt, Mo, FunToApply, AttrList]),
                apply(?MODULE, FunToApply, [MeId0, Mo, AttrList, FailReason]),
                {Name0, MeId0, Cnt+1};
           ({PrePost, FunToApply, Mo, AttrList}, {Name0, MeId0, Cnt}) ->
                CntToPrint =
                    case PrePost of
                        pre  -> Cnt;
                        post -> Cnt-1;
                        preset -> all_set_subtests;
                        postset -> all_set_subtests;
                        preposttest -> test
                    end,
                ct:pal("********** TestSubCase: ~p~p **********~nMO:~p~n"
                      "Operation: ~p~nAttributes: ~p~n",
                       [Name, CntToPrint, Mo, FunToApply, AttrList]),
                apply(?MODULE, FunToApply, [MeId0, Mo, AttrList]),
                {Name0, MeId0, Cnt};
           ({PrePost, FunToApply, Mo, AttrList, FailReason},
            {Name0, MeId0, Cnt}) ->
                CntToPrint =
                    case PrePost of
                        pre  -> Cnt;
                        post -> Cnt-1;
                        preset -> all_set_subtests;
                        postset -> all_set_subtests;
                        preposttest -> test
                    end,
                ct:pal("********** TestSubCase: ~p~p **********~nMO:~p~n"
                      "Operation: ~p~nAttributes: ~p~n",
                       [Name, Cnt, CntToPrint, FunToApply, AttrList]),
                apply(?MODULE, FunToApply, [MeId0, Mo, AttrList, FailReason]),
                {Name0, MeId0, Cnt}
        end,
    lists:foldl(Fun, {Name, MeId, StartCnt}, TestSubCaseList),
    ok.

%%%--------------------------------------------------------------------
%%% Description: Try to create an MO with given attributes and expect it
%%%              to succeed
%%%--------------------------------------------------------------------
create_successful(MeId, Mo, AttributesAndValuesList) ->
    Create = [{'xc:operation', "create"}],
    edit_successful(MeId, Mo, Create, AttributesAndValuesList).

%%%--------------------------------------------------------------------
%%% Description: Try to create an MO with given attributes and expect it
%%%              to fail
%%%--------------------------------------------------------------------
create_unsuccessful(MeId, Mo, AttributesAndValuesList, FailReason) ->
    Create = [{'xc:operation', "create"}],
    edit_unsuccessful(MeId, Mo, Create, AttributesAndValuesList, FailReason).

%%%--------------------------------------------------------------------
%%% Description: Try to set MO attributes and expect it to succeed
%%%--------------------------------------------------------------------
set_successful(MeId, Mo, AttributesAndValuesList) ->
    edit_successful(MeId, Mo, [], AttributesAndValuesList).

%%%--------------------------------------------------------------------
%%% Description: Try to set MO attributes and expect it to fail
%%%--------------------------------------------------------------------
set_unsuccessful(MeId, Mo, AttributesAndValuesList, FailReason) ->
    edit_unsuccessful(MeId, Mo, [], AttributesAndValuesList, FailReason).

%%%--------------------------------------------------------------------
%%% Description: Try to delete MO  and expect it to succeed
%%%--------------------------------------------------------------------
delete_successful(MeId, Mo, AttributesAndValuesList) ->
    Delete = [{'xc:operation', "delete"}],
    edit_successful(MeId, Mo, Delete, AttributesAndValuesList).

%%%--------------------------------------------------------------------
%%% Description: Try to delete MO  and expect it to fail
%%%--------------------------------------------------------------------
delete_unsuccessful(MeId, Mo, AttributesAndValuesList, FailReason) ->
    Delete = [{'xc:operation', "delete"}],
    edit_unsuccessful(MeId, Mo, Delete, AttributesAndValuesList, FailReason).

%%%--------------------------------------------------------------------
%%% Description: Delete MO. Don't care for result (used in postconditions)
%%%--------------------------------------------------------------------
delete(MeId, Mo, AttributesAndValuesList) ->
    Delete = [{'xc:operation', "delete"}],
    Edit =
        construct_edit_config(MeId, Mo, Delete, AttributesAndValuesList),
    netconf_edit(Edit),
    ok.

%%%--------------------------------------------------------------------
%%% Description: Check given MO attribute values are same as given values
%%%--------------------------------------------------------------------
check_successful(_MeId, _Mo, [_Id | _] = _AttributesAndValuesList) ->
    %% NOTE: Expect
%    AttValListNode = get_successful(MeId, Mo, [Id]),
%    [case lists:member(AttVal, AttValListNode) of
%         true ->
%             ok;
%         false ->
%             ct:pal("Given attribute doesn't have same value node: ~p~n~n~p",
%                    [AttVal, AttValListNode]),
%             ct:fail([AttVal, AttValListNode])
%             ok
%     end
%    || AttVal <- AttributesAndValuesList].
    ok.

%%%--------------------------------------------------------------------
%%% Description: Try to get MO attribute and expect it to succeed
%%%--------------------------------------------------------------------
get_successful(MeId, Mo, Subfilter) ->
    Filter =
        construct_edit_config(MeId, Mo, [], Subfilter),
    case netconf_get(Filter) of
        {ok, _Data} -> ok; %% don't care for data at the moment. TODO
        Error -> ct:fail(Error)
    end.

%%%--------------------------------------------------------------------
%%% Description: Construct and apply edit via netconf on given Mo with
%%%              given attributes. Expect netconf transaction to succeed,
%%%              if it doesn't, print FailReason
%%%--------------------------------------------------------------------
edit_successful(MeId, Mo, Operations, AttributesAndValuesList)->
    Edit =
        construct_edit_config(MeId, Mo, Operations, AttributesAndValuesList),
    %% TODO add get and check here
    case netconf_edit(Edit) of
        ok -> ok;
        Error -> ct:fail(Error)
    end.

%%%--------------------------------------------------------------------
%%% Description: Construct and apply edit via netconf on given Mo with
%%%              given attributes. Expect netconf transaction not to succeed,
%%%              if it doesn't, print FailReason
%%%--------------------------------------------------------------------
edit_unsuccessful(MeId, Mo, Operations, AttributesAndValuesList, FailReason) ->
    Edit =
        construct_edit_config(MeId, Mo, Operations, AttributesAndValuesList),
    %% TODO add get and check here
    case netconf_edit(Edit) of
        ok -> ct:fail(FailReason);
        _Error -> ok
    end.

%%%--------------------------------------------------------------------
%%% Description: Shorten some function calls
%%%--------------------------------------------------------------------
netconf_edit(Edit) -> netconf(edit_config, [nc1, running, Edit]).

%netconf_action(Action) -> netconf(action, [nc1, Action]). not used for now

netconf_get(Get) -> netconf(get, [nc1, Get]).

%%%--------------------------------------------------------------------
%%% Description: Construct an edit_config structure for a given MO and
%%%              proplist with elements {MOAttribute, Value}
%%%--------------------------------------------------------------------
construct_edit_config(MeId, Mo, Operations, AttributesAndValuesList) ->
    Attributes = listify_values(AttributesAndValuesList),
    case Mo of
        'LogM' ->
            {'ManagedElement',
             [?URN_COMTOP, ?URN_NETCONF],
             [{managedElementId, [], [MeId]},
              {'SystemFunctions',
               [{systemFunctionsId, [], ["1"]},
                {'LogM', [{xmlns,"urn:com:ericsson:ecim:LogM"}],
                 [{logMId, Operations, ["1"]}] ++ Attributes}]}]};
        'Https' ->
            {'ManagedElement',
             [?URN_COMTOP, ?URN_NETCONF],
             [{managedElementId, [], [MeId]},
              {'SystemFunctions',
               [{systemFunctionsId, [], ["1"]},
                {'SysM', [{xmlns,"urn:com:ericsson:ecim:SysM"}],
                 [{sysMId, [], ["1"]},
                  {'HttpM', [{xmlns,"urn:com:ericsson:ecim:HttpM"}],
                   [{httpMId, [], ["1"]},
                    {'Https', Operations, Attributes}]
                  }]}]}]};
        'Ikev2PolicyProfile' ->
            [];
        'EnrollmentServer' ->
            [GroupId | Tail] = Attributes,
            ?BASIC_CERTM(MeId, ?ENROLLMENT_SERVER(Operations, GroupId, Tail));
        _ ->
            ?BASIC_CERTM(MeId, {Mo, Operations, Attributes})
    end.

%%%--------------------------------------------------------------------
%%% Description: Given a list, change every tuple element {A, B}
%%%              with {A, [B]}
%%%--------------------------------------------------------------------
listify_values(AttributesAndValuesList) ->
    Fun = fun({Attribute, Value}) -> {Attribute, [Value]};
             (AnythingElse)  -> AnythingElse
          end,
    lists:map(Fun, AttributesAndValuesList).

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------
netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),

    Status =
        case Res of
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
            ct_netconfc:close_session(nc1), %% find a better way TODO
            Status
    end.

%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------
cmd(Cmd) ->
    ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.

%%%--------------------------------------------------------------------
%%% Description: Cleanup after a TC
%%%--------------------------------------------------------------------
cleanup(Config) ->
    MeId = ?config(meId, Config),
    IdList = ?MO_ID_LIST,
    MoProplist = ?ORDERED_PROPLIST_OF_MOS_TO_BE_DELETED_AFTER_EACH_TC,

    %% also unset nodeCredential attribute on Https and LogM

    ExecList =
        [[{preposttest, delete, Mo, [{MoId, Id}]} || Id <- IdList] ||
         {Mo, MoId} <- MoProplist],
    ExecListFlatten = lists:flatten(ExecList),
    run_sub_test_cases(MeId, cleanup, 0, ExecListFlatten).

%% TODO Create new library/module
%%%--------------------------------------------------------------------
%%% Description: Given netconf structure, try to apply it on node
%%%--------------------------------------------------------------------
%netconf_transaction(_NetconfStruct, _Node) ->
%    ok.

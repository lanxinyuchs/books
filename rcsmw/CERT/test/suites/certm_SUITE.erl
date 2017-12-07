%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certm_SUITE.erl %
%%% @author eivirad
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R12A/1
%%% 
%%% @doc == Test Suite for testing Certificate management operations==
%%% Use case numbering refers to the ECIM Certificate Management Use
%%% Case Description 10/155 56-FAE 151 01 Uen Rev A 2013-06-19
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.  
%%% <br/><br/> rct_netconf is used in ct_hooks: see <a
%%% href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end

-module(certm_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R12A/1').
-author('eivirad').

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
%%% R2A/1      2013-08-12 etxjotj     Created
%%% R2A/6      2013-11-21 etxasta     Updated
%%% R3A/9      2015-02-28 etxkols     Preparation for 2 labs
%%% R4A/4      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R5A/1      2016-04-25 ehsake      Added negative_invalid_uri TC
%%% R6A/1      2016-09-05 emariad     Random deprectated, changed to rand.
%%% R6A/2      2016-09-15 emariad     Changed back to rand for 17A.
%%% R7A/1      2016-09-15 emariad     Removed all random:seed. Changed random to rand.
%%% R9A/3      2017-03-20 eivomat     Update test after fix for HV70387
%%% R12A/1     2017-11-08 eivirad     Update test after fix for HW39506
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
         groups/0,
         syslogs/1]).
-export([read_capabilities/1,
	 update_revocation_information/1,    % UC 3.1.2
	 %% UC 3.1.3 is deprecated
	 configure_enrollment_authority1/1,   % UC 3.1.4
	 configure_enrollment_authority2/1,   % UC 3.1.4
	 %% UC 3.1.5 is deprecated
	 configure_server_group/1,           % UC 3.1.6
	 %% UC 3.1.7 is deprecated
	 configure_enrollment_server/1,      % UC 3.1.8
	 configure_enrollment_server2/1,     % UC 3.1.8
	 remove_enrollment_server/1,         % UC 3.1.9
	 remove_enrollment_server2/1,        % UC 3.1.9
	 remove_enrollment_authority/1,      % UC 3.1.10
	 remove_server_group/1,              % UC 3.1.11
	 csr_initial_offline_enrollment/1,   % UC 3.2.2 (part1)
	 csr_initial_offline_enrollment_file/1,   % UC 3.2.2 (part1)
         install_csr_credential_from_uri/1,  % UC 3.2.2 (part2)
         csr_renewal_offline_enrollment/1,   % UC 3.2.2 (part1)
         install_pkcs12_from_sftp_uri/1,     % UC 3.2.4
         install_pkcs12_from_https_uri1/1,    % UC 3.2.4
         install_pkcs12_from_https_uri2/1,    % UC 3.2.4
         start_online_enrollment/1,          % UC 3.2.6
         renewal_online_enrollment/1,        % UC 3.2.6
         install_trusted_certificate_sftp/1, % UC 3.2.10
         install_trusted_der_certificate_sftp/1, % UC 3.2.10
         install_trusted_certificate_http/1, % UC 3.2.10
	 create_trust_category/1,            % UC 3.2.11
	 validate_trusted_certificate/1,     % UC 3.2.12
	 invalidate_trusted_certificate/1,   % UC 3.2.12
         cancel_node_credential_enrollment/1,% UC 3.2.14
	 remove_node_credential/1,           % UC 3.2.16
	 remove_trust_category/1,            % UC 3.2.17
	 remove_trusted_certificate/1,       % UC 3.2.18
	 negative_invalid_uri/1,
	 x/1
	]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [read_capabilities,
     update_revocation_information,
     install_trusted_certificate_sftp,
     install_trusted_der_certificate_sftp,
     install_trusted_certificate_http,
     validate_trusted_certificate,
     invalidate_trusted_certificate,
     create_trust_category,
     remove_trust_category,
     remove_trusted_certificate,
     csr_initial_offline_enrollment,
     csr_initial_offline_enrollment_file,
     install_csr_credential_from_uri,
     csr_renewal_offline_enrollment,
     install_csr_credential_from_uri,
     install_pkcs12_from_sftp_uri,
     install_pkcs12_from_https_uri1,
     install_pkcs12_from_https_uri2,
     %cancel_node_credential_enrollment,
     configure_enrollment_authority1,
     remove_enrollment_authority,
     configure_enrollment_authority2,
     configure_server_group,
     configure_enrollment_server,
     %start_online_enrollment,
     %renewal_online_enrollment,
     negative_invalid_uri,
     remove_node_credential,
     remove_enrollment_server,
     remove_server_group,
     remove_enrollment_authority].

syslogs(Config) ->
    install_pkcs12_from_https_uri1(Config),
    create_trust_category(Config).

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
                                 {["ERROR REPORT", "CRASH REPORT"], []}}]}}
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

    PrivDir = proplists:get_value(priv_dir, Config),
    cmd(["chmod a+rwx ", PrivDir]),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    [{meId, MeId},
     {sftp_host, SftpHost},
     {sftp_user, Username},
     {sftp_pass, Password}
     %% {sftp_host, "10.68.200.11"},
     %% {sftp_user, "mauve"},
     %% {sftp_pass, "dilbert"}
     | Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
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

%%%--------------------------------------------------------------------
%%% @doc Read the CertMCapabilities MO
%%% @end
%%%--------------------------------------------------------------------

read_capabilities(Config) ->
    MeId = proplists:get_value(meId, Config),
    Filter = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
	     [{secMId,[],["1"]},
	      {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
	       [{certMId,[],["1"]},
		{'CertMCapabilities', [],
		 [{certMCapabilitiesId, [], ["1"]}]}]}]}]}]},
    {ok, Result} = netconf(get, [nc1, Filter]),
    %% At this moment in development, these values are not important
    %% We just want to make sure they are there. But later on it might
    %% be valueable to see that the correct values are returned. jotj 20130812
    {ok, _} = extract_element(enrollmentSupport, Result),
    {ok, _} = extract_element(fingerprintSupport, Result),
    ok.

%%%--------------------------------------------------------------------
%%% @doc UC 3.1.2 Update Revocation information
%%% @end
%%%--------------------------------------------------------------------
update_revocation_information(Config) ->
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
    ActionId = get_action_id(reportProgress, ProgressFilter),
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
		{downloadCrl, [], []}]}]}]}]},
    ActionResult = netconf(action, [nc1, Action]),
    case ActionResult of
	{ok, _} ->
	    ok;
	Error ->
	    ct:pal("downloadCrl action failed: ~p~n",[Error]),
	    ct:fail(Error)
    end,
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    ok;
	Result ->
	    ct:pal("downloadCrl: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.1.4 Configure Enrollment Authority with CaCert
%%% @end
%%%--------------------------------------------------------------------
configure_enrollment_authority1(Config) ->
    MeId = proplists:get_value(meId, Config),
    %{ok, MoRef} = install_trusted_certificate_sftp(
    %[{file, "cacert.pem"}|Config]),
    {ok, MoRef} = install_trusted_certificate_sftp(
                    [{file, "etxasta.pem"}|Config]),

    Edit = {'ManagedElement',
            [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
            [{managedElementId,[],[MeId]},
             {'SystemFunctions',
              [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                  [{certMId,[],["1"]},
                   {'EnrollmentAuthority', [],
                    [{enrollmentAuthorityId, ["certm_SUITE"]},
                     {authorityType, ["CERTIFICATION_AUTHORITY"]},
                     {userLabel, ["Created by certm_SUITE"]},
                     {enrollmentAuthorityName,
                      ["C=SE,O=Ericsson,CN=etxasta"]},
                     {enrollmentCaCertificate, [MoRef]}
                    ]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
        ok -> ok;
        _ ->
            ct:fail(EditRes)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.1.4 Configure Enrollment Authority with Fingerprint
%%% @end
%%%--------------------------------------------------------------------
configure_enrollment_authority2(Config) ->
    MeId = proplists:get_value(meId, Config),

    DataDir = proplists:get_value(data_dir, Config),
    File = proplists:get_value(file, Config, "etxasta.pem"),
    Path =  filename:join(DataDir, File),
    [_, FP] = 
        string:tokens(
          cmd(["openssl x509 -fingerprint -noout -in ", Path]), "="),
    Fingerprint = string:strip(FP, both, $\n),
    Edit = {'ManagedElement',
            [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
            [{managedElementId,[],[MeId]},
             {'SystemFunctions',
              [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                  [{certMId,[],["1"]},
                   {'EnrollmentAuthority', [],
                    [{enrollmentAuthorityId, ["certm_SUITE"]},
                     {authorityType, ["CERTIFICATION_AUTHORITY"]},
                     {userLabel, ["Created by certm_SUITE"]},
                     {enrollmentAuthorityName,
                      ["C=SE,O=Ericsson,CN=etxasta"]},
                     {enrollmentCaFingerprint, [Fingerprint]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
        ok -> ok;
        _ ->
            ct:fail(EditRes)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.1.6 Configure enrollment server group
%%% @end
%%%--------------------------------------------------------------------
configure_server_group(Config) ->
    MeId = proplists:get_value(meId, Config),
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'EnrollmentServerGroup',
		    [{enrollmentServerGroupId, ["certm_SUITE"]},
		     {userLabel, ["Created by certm_SUITE"]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.1.8 Configure enrollment server 
%%% @end
%%%--------------------------------------------------------------------
configure_enrollment_server(Config) ->
    [{host, Cmpv2Host}] = ct:get_config(cmpv2_server),
    MeId = proplists:get_value(meId, Config),
    MoRef = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
	"EnrollmentAuthority=certm_SUITE",
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'EnrollmentServerGroup',
		    [{enrollmentServerGroupId, ["certm_SUITE"]},
		     {'EnrollmentServer',
		      [{enrollmentServerId, ["certm_SUITE"]},
		       {enrollmentAuthority, [MoRef]},
		       {protocol, ["CMP"]},
		       {uri, ["http://"++Cmpv2Host++":8180/ejbca/publicweb/cmp/3gpp"]},
		       %% {uri, ["http://10.68.200.14:8180/ejbca/publicweb/cmp/3gpp"]},
                       %{uri, ["http://10.1.2.3:8180/ejbca/publicweb/cmp/3gpp"]},
		       {userLabel, ["Created by certm_SUITE"]}]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.

configure_enrollment_server2(Config) ->
    [{host, Cmpv2Host}] = ct:get_config(cmpv2_server),
    MeId = proplists:get_value(meId, Config),
    MoRef = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
	"EnrollmentAuthority=certm_SUITE",
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'EnrollmentServerGroup',
		    [{enrollmentServerGroupId, ["certm_SUITE"]},
		     {'EnrollmentServer',
		      [{enrollmentServerId, ["certm_SUITE2"]},
		       {enrollmentAuthority, [MoRef]},
		       {protocol, ["CMP"]},
		       {uri, ["http://"++Cmpv2Host++":8180/ejbca/publicweb/cmp/3gpp"]},
		       %% {uri, ["http://10.68.200.14:8180/ejbca/publicweb/cmp/3gpp"]},
		       {userLabel, ["Created by certm_SUITE2"]}]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.


%%%--------------------------------------------------------------------
%%% @doc UC 3.1.9 Remove enrollment server
%%% @end
%%%--------------------------------------------------------------------
remove_enrollment_server(Config) ->
    MeId = proplists:get_value(meId, Config),
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
		   {'EnrollmentServerGroup',
		    [{enrollmentServerGroupId, ["certm_SUITE"]},
		     {'EnrollmentServer', [{'xc:operation', "delete"}],
		      [{enrollmentServerId, ["certm_SUITE"]}]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.

remove_enrollment_server2(Config) ->
    MeId = proplists:get_value(meId, Config),
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
		   {'EnrollmentServerGroup',
		    [{enrollmentServerGroupId, ["certm_SUITE"]},
		     {'EnrollmentServer', [{'xc:operation', "delete"}],
		      [{enrollmentServerId, ["certm_SUITE2"]}]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.


%%%--------------------------------------------------------------------
%%% @doc UC 3.1.10 Remove Enrollment Authority
%%% @end
%%%--------------------------------------------------------------------
remove_enrollment_authority(Config) ->
    MeId = proplists:get_value(meId, Config),
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
		  [{certMId,[],["1"]},
		   {'EnrollmentAuthority', [{'xc:operation', "delete"}],
		    [{enrollmentAuthorityId, [], ["certm_SUITE"]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.1.11 Remove enrollment server group
%%% @end
%%%--------------------------------------------------------------------
remove_server_group(Config) ->
    MeId = proplists:get_value(meId, Config),
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
		   {'EnrollmentServerGroup',[{'xc:operation', "delete"}],
		    [{enrollmentServerGroupId, ["certm_SUITE"]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.2 Start CSR based offline initial enrollment (part1)
%%%      No file name in path, just directory
%%% @end
%%%--------------------------------------------------------------------
csr_initial_offline_enrollment(Config) ->
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
		    [{nodeCredentialId, ["certm_SUITE_offline"]},
		     {userLabel, ["Created by certm_SUITE"]},
		     {subjectName, [SubjectName]},
		     {subjectAltName, [SubjectAltName]},
		     {keyInfo, [KeyInfo]}]}]}]}]}]},
    
    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,

    PrivDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", PrivDir]),
    Uri = sftp_uri(Config, PrivDir),
    Password = proplists:get_value(sftp_pass, Config),

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
		      [{nodeCredentialId, ["certm_SUITE_offline"]},
		       {startOfflineCsrEnrollment, 
			[{uri, [Uri]},
			 {uriPassword, [Password]}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_offline"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action startOfflineCsrEnrollment~n",[]),
    case netconf(action, [nc1, Action]) of
	{ok, _} ->
	    ok;
	Error2 ->
	    ct:pal("startOfflineCsrEnrollment action failed: ~p~n",
		   [Error2]),
	    ct:fail(Error2)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("startOfflineCsrEnrollment: ~s~n",[Result]).

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.2 Start CSR based offline initial enrollment (part1)
%%%      Filename in path
%%% @end
%%%--------------------------------------------------------------------
csr_initial_offline_enrollment_file(Config) ->
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
		    [{nodeCredentialId, ["certm_SUITE_offline_file"]},
		     {userLabel, ["Created by certm_SUITE"]},
		     {subjectName, [SubjectName]},
		     {keyInfo, [KeyInfo]}]}]}]}]}]},
    
    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,

    PrivDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", PrivDir]),
    Uri = sftp_uri(Config, PrivDir) ++ "test.csr",
    Password = proplists:get_value(sftp_pass, Config),

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
		      [{nodeCredentialId, ["certm_SUITE_offline_file"]},
		       {startOfflineCsrEnrollment, 
			[{uri, [Uri]},
			 {uriPassword, [Password]}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_offline_file"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action startOfflineCsrEnrollment~n",[]),
    case netconf(action, [nc1, Action]) of
	{ok, _} ->
	    ok;
	Error2 ->
	    ct:pal("startOfflineCsrEnrollment action failed: ~p~n",
		   [Error2]),
	    ct:fail(Error2)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("startOfflineCsrEnrollment: ~s~n",[Result]).

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.2 Start CSR based offline renewal enrollment (part1)
%%% @end
%%%--------------------------------------------------------------------
csr_renewal_offline_enrollment(Config) ->
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
		    [{nodeCredentialId, ["certm_SUITE_offline"]},
		     {userLabel, ["Created by certm_SUITE"]},
		     {keyInfo, [KeyInfo]}]}]}]}]}]},
    
    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,

    PrivDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", PrivDir]),
    Uri = sftp_uri(Config, PrivDir),
    Password = proplists:get_value(sftp_pass, Config),

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
		      [{nodeCredentialId, ["certm_SUITE_offline"]},
		       {startOfflineCsrEnrollment, 
			[{uri, [Uri]},
			 {uriPassword, [Password]}]}]}]}]}]}]},

    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_offline"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),
    ct:pal("Action startOfflineCsrEnrollment~n",[]),
    case netconf(action, [nc1, Action]) of
	{ok, _} ->
	    ok;
	Error2 ->
	    ct:pal("startOfflineCsrEnrollment action failed: ~p~n",
		   [Error2]),
	    ct:fail(Error2)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("startOfflineCsrEnrollment: ~s~n",[Result]).

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.2 Start CSR based install or renewal of credential
%%%      from uri (part2)
%%% @end
%%%--------------------------------------------------------------------
install_csr_credential_from_uri(Config) ->
    ct:pal("Action installCredentialFromUri~n",[]),
    MeId    = proplists:get_value(meId, Config),
    Uri     =
    sftp_uri(Config, "/backup/sftp/etxasta/test_cert/signed_cert.pem"),
    UriPwd      = proplists:get_value(sftp_pass, Config),
    CredPwd     = "true", %% FIXME should be NULL
    FingerPrint =
    "7E:17:89:C7:9C:AD:C2:BC:04:68:AE:70:55:B3:21:71:68:9E:08:BF",
    
    ct:pal("Uri: ~p~n", [Uri]),
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
		      [{nodeCredentialId, ["certm_SUITE_offline"]},
		       {installCredentialFromUri, 
			[{uri, [Uri]},
                         {uriPassword, [UriPwd]},
                         {credentialPassword, [CredPwd]},
                         {fingerprint, [FingerPrint]}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_offline"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action installCredentialFromUri~n",[]),
    case  netconf(action, [nc1, Install]) of
	{ok, _} ->
	    ok;
	Error ->
	    ct:pal("installCredentialFromUri action failed: ~p~n",
		   [Error]),
	    ct:fail(Error)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("installCredentialFromUri: ~s~n",[Result]).

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.4 Install PKCS#12 as node credential from uri via sftp
%%% @end
%%%--------------------------------------------------------------------
install_pkcs12_from_sftp_uri(Config) ->
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
    SubjectName = "C=SE,O=Ericsson,CN=certm_SUITE_pkcs12.ericsson.com",
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
		    [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_sftp"]},
		     {userLabel, ["Created by certm_SUITE"]},
		     {subjectName, [SubjectName]},
		     {keyInfo, [KeyInfo]}]}]}]}]}]},

    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,
    
    ct:pal("Action installCredentialFromUri~n",[]),
    MeId    = proplists:get_value(meId, Config),
    %Uri     = "sftp://10.1.2.3/olle.txt",
    Uri =
    sftp_uri(Config, "/backup/sftp/etxasta/test_cert/client_syslog.p12"),
    UriPwd      = proplists:get_value(sftp_pass, Config),
    CredPwd     = "test",
    FingerPrint = % test_cert_ext.pkcs12
    "34:57:11:ed:ae:8e:19:22:4d:d0:e5:8e:83:57:1f:73:f6:97:e0:c6",
    ct:pal("Uri: ~p~n", [Uri]),
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
		      [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_sftp"]},
		       {installCredentialFromUri, 
			[{uri, [Uri]},
                         {uriPassword, [UriPwd]},
                         {credentialPassword, [CredPwd]},
                         {fingerprint, [FingerPrint]}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_sftp"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action installCredentialFromUri PKCS#12~n",[]),
    case  netconf(action, [nc1, Install]) of
	{ok, _} ->
	    ok;
	Error ->
	    ct:pal("installCredentialFromUri action failed: ~p~n",
		   [Error]),
	    ct:fail(Error)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("installCredentialFromUri: ~s~n",[Result]).
 
%%%--------------------------------------------------------------------
%%% @doc UC 3.2.4 Install PKCS#12 as node credential from uri via https
%%% @end
%%%--------------------------------------------------------------------
install_pkcs12_from_https_uri1(Config) ->
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
    SubjectName = "C=SE,O=Ericsson,CN=certm_SUITE_pkcs12.ericsson.com",
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
		    [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_http"]},
		     {userLabel, ["Created by certm_SUITE"]},
		     {subjectName, [SubjectName]},
		     {keyInfo, [KeyInfo]}]}]}]}]}]},
    
    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,
    
    ct:pal("Action installCredentialFromUri 1~n",[]),
    MeId    = proplists:get_value(meId, Config),
    [{host, HttpIp}] = ct:get_config(http_server),
    Uri = "http://"++HttpIp++"/test/client_syslog.p12",
    CredPwd = "test",
    FingerPrint = "34:57:11:ed:ae:8e:19:22:4d:d0:e5:8e:83:57:1f:73:f6:97:e0:c6",
    ct:pal("Uri: ~p~n", [Uri]),
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
		      [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_http"]},
		       {installCredentialFromUri, 
			[{uri, [Uri]},
                         {credentialPassword, [CredPwd]},
                         {fingerprint, [FingerPrint]}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_http"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action installCredentialFromUri 1 PKCS#12~n",[]),
    case  netconf(action, [nc1, Install]) of
	{ok, _} ->
	    ok;
	Error ->
	    ct:pal("installCredentialFromUri 1 action failed: ~p~n",
		   [Error]),
	    ct:fail(Error)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("installCredentialFromUri: ~s~n",[Result]).
 
%%%--------------------------------------------------------------------
%%% @doc UC 3.2.4 Install PKCS#12 as node credential from uri via https
%%%      with no fingerprint
%%% @end
%%%--------------------------------------------------------------------
install_pkcs12_from_https_uri2(Config) ->
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
    SubjectName = "C=SE,O=Ericsson,CN=certm_SUITE_pkcs12.ericsson.com",
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
		    [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_http2"]},
		     {userLabel, ["Created by certm_SUITE"]},
		     {subjectName, [SubjectName]},
		     {keyInfo, [KeyInfo]}]}]}]}]}]},
    
    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,
    
    ct:pal("Action installCredentialFromUri 2~n",[]),
    MeId    = proplists:get_value(meId, Config),
    [{host, HttpIp}] = ct:get_config(http_server),
    Uri = "http://"++HttpIp++"/test/client_syslog.p12",
    CredPwd = "test",
    ct:pal("Uri: ~p~n", [Uri]),
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
		      [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_http2"]},
		       {installCredentialFromUri, 
			[{uri, [Uri]},
                         {credentialPassword, [CredPwd]},
                         {fingerprint, ["NULL"]}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_pkcs12_offline_http2"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action installCredentialFromUri 2 PKCS#12~n",[]),
    case  netconf(action, [nc1, Install]) of
	{ok, _} ->
	    ok;
	Error ->
	    ct:pal("installCredentialFromUri 2 action failed: ~p~n",
		   [Error]),
	    ct:fail(Error)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("installCredentialFromUri: ~s~n",[Result]).

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.6 Start online initial enrollment
%%% @end
%%%--------------------------------------------------------------------
start_online_enrollment(Config) ->
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
    SubjectName = "C=SE,O=Ericsson,CN=etxasta1",
    DN_enroll_auth = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1," ++
    "EnrollmentAuthority=certm_SUITE",
    DN_enroll_serv = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1," ++
    "EnrollmentServerGroup=certm_SUITE",

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
		    [{nodeCredentialId, ["certm_SUITE_online"]},
		     {userLabel, ["Created by certm_SUITE"]},
		     {subjectName, [SubjectName]},
                     {enrollmentTimer, [integer_to_list(5)]},
                     {keyInfo, [KeyInfo]},
                     {enrollmentServerGroup, [DN_enroll_serv]},
                     {renewalMode, ["AUTOMATIC"]},
                     {enrollmentAuthority, [DN_enroll_auth]},
                     {expiryAlarmThreshold, [integer_to_list(5)]}]}]}]}]}]},
    
    ct:pal("Create Node Credential MO~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,

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
		      [{nodeCredentialId, ["certm_SUITE_online"]},
		       {startOnlineEnrollment, 
			[{challengePassword, ["NULL"]}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_online"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action startOnlineEnrollment (initial)~n",[]),
    case netconf(action, [nc1, Action]) of
	{ok, _} ->
	    ok;
	Error2 ->
            ct:pal("startOnlineEnrollment action failed (initial): ~p~n",
		   [Error2]),
	    ct:fail(Error2)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("startOnlineEnrollment: ~s~n",[Result]).

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.6 Start online renewal enrollment
%%% @end
%%%--------------------------------------------------------------------
renewal_online_enrollment(Config) ->
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
		    [{nodeCredentialId, ["certm_SUITE_online"]},
                     {keyInfo, [KeyInfo]}]}]}]}]}]},
    
    ct:pal("Change KeyInfo~n",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
	ok -> ok;
	Error1 ->
	    ct:fail(Error1)
    end,

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
		      [{nodeCredentialId, ["certm_SUITE_online"]},
		       {startOnlineEnrollment, 
			[{challengePassword, ['NULL']}]}]}]}]}]}]},
    ProgressFilter =
               {'ManagedElement',
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
                        [{nodeCredentialId, ["certm_SUITE_online"]},
                         {enrollmentProgress, []}]}]}]}]}]},
    ActionId = get_action_id(enrollmentProgress, ProgressFilter),
    ct:pal("Action startOnlineEnrollment (renewal)~n",[]),
    case netconf(action, [nc1, Action]) of
	{ok, _} ->
	    ok;
	Error2 ->
            ct:pal("startOnlineEnrollment action failed (renewal): ~p~n",
		   [Error2]),
	    ct:fail(Error2)
    end,
    %% wait here since we do not want to start new action before completing this one
    Result = wait_for_progress(enrollmentProgress, ActionId, ProgressFilter),
    ct:pal("startOnlineEnrollment: ~s~n",[Result]).

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.10 Install trusted certificate using SFTP
%%% @end
%%%--------------------------------------------------------------------
install_trusted_certificate_sftp(Config) ->
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
    ActionId = get_action_id(reportProgress, ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    File = proplists:get_value(file, Config, "test_tc.pem"),
    Path =  filename:join(DataDir, File),
    SftpPath = filename:join(PrivDir, File),
    {ok, _} = file:copy(Path, SftpPath),
    [_, FP] = 
	string:tokens(
	  cmd(["openssl x509 -fingerprint -noout -in ", Path]), "="),
    Fingerprint = string:strip(FP, both, $\n),
    Uri = sftp_uri(Config, SftpPath),
    %Uri = "sftp://10.1.2.3/olle.txt",
    Password = proplists:get_value(sftp_pass, Config),

 
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
		 [{uri, [Uri]},
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
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
	["SUCCESS"] ->
            ResultInfo = get_result_info(reportProgress, ProgressFilter),
	    ct:pal("Reading ~p~n",[ResultInfo]),
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
	Result ->
	    ct:pal("installTrustedCertFromUri: ~s~n",[Result]),
	    ct:fail(Result)
    end.

sftp_uri(Config, Path) ->
    Host = proplists:get_value(sftp_host, Config),
    User = proplists:get_value(sftp_user, Config),
    Uri = "sftp://"++User++"@"++Host++Path,
    ct:pal("SFTP URI: ~p~n", [Uri]),
    Uri.

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.10 Install trusted certificate in DER format using SFTP
%%% @end
%%%--------------------------------------------------------------------
install_trusted_der_certificate_sftp(Config) ->
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
    ActionId = get_action_id(reportProgress, ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    File = proplists:get_value(file, Config, "etxasta.der"),
    Path =  filename:join(DataDir, File),
    SftpPath = filename:join(PrivDir, File),
    {ok, _} = file:copy(Path, SftpPath),
    [_, FP] = 
	string:tokens(
	  cmd(["openssl x509 -fingerprint -noout -inform DER -in ", Path]), "="),
    Fingerprint = string:strip(FP, both, $\n),
    Uri = sftp_uri(Config, SftpPath),
    Password = proplists:get_value(sftp_pass, Config),
    
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
		 [{uri, [Uri]},
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
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
	["SUCCESS"] ->
            ResultInfo = get_result_info(reportProgress, ProgressFilter),
	    ct:pal("Reading ~p~n",[ResultInfo]),
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
	Result ->
	    ct:pal("installTrustedCertFromUri: ~s~n",[Result]),
	    ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.10 Install trusted certificate using HTTP/HTTPS
%%% @end
%%%--------------------------------------------------------------------
install_trusted_certificate_http(Config) ->
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
    ActionId = get_action_id(reportProgress, ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    DataDir = proplists:get_value(data_dir, Config),
    File = proplists:get_value(file, Config, "test_tc.pem"),
    Path =  filename:join(DataDir, File),
    [_, FP] = 
	string:tokens(
	  cmd(["openssl x509 -fingerprint -noout -in ", Path]), "="),
    Fingerprint = string:strip(FP, both, $\n),
    [{host, HttpIp}] = ct:get_config(http_server),
    Uri = "http://"++HttpIp++"/test/test_tc.pem",
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
		 [{uri, [Uri]},
		  {fingerprint, [Fingerprint]}]}]}]}]}]},
    ActionResult = netconf(action, [nc1, Action]),
    case ActionResult of
	{ok, _} ->
	    ok;
	Error ->
	    ct:pal("installTrustedCertFromUri action failed: ~p~n",[Error]),
	    ct:fail(Error)
    end,
    case wait_for_progress(reportProgress, ActionId, ProgressFilter) of
	["SUCCESS"] ->
            ResultInfo = get_result_info(reportProgress, ProgressFilter),
	    ct:pal("Reading ~p~n",[ResultInfo]),
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
	Result ->
	    ct:pal("installTrustedCertFromUri: ~s~n",[Result]),
	    ct:fail(Result)
    end.


%%%--------------------------------------------------------------------
%%% @doc UC 3.2.11 Create/configure a trust category
%%% @end
%%%--------------------------------------------------------------------

create_trust_category(Config) ->
    %% First, check if there is a trusted certificate
    MeId = proplists:get_value(meId, Config),
    AllTCs = 
	case get_all_trusted_certificates(MeId) of
	    [] ->
		install_trusted_certificate_sftp(Config),
		get_all_trusted_certificates(MeId);
	    List ->
		List
	end,
    MoRefs = [make_tc_moref(MeId, TC)||TC<-AllTCs],
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'TrustCategory', [],
		    [{trustCategoryId, [], ["certm_SUITE"]},
		     {userLabel, ["Created by certm_SUITE"]}|
		     [{trustedCertificates, [MoRef]}||MoRef<-MoRefs]]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end,
    ok.

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.12 (Invalidate/)validate a trusted certificate
%%% This UC is split in half
%%% @end
%%%--------------------------------------------------------------------
validate_trusted_certificate(Config) ->
    MeId = proplists:get_value(meId, Config),
    AllTCs = 
	case get_all_trusted_certificates(MeId) of
	    [] ->
		install_trusted_certificate_sftp(Config),
		get_all_trusted_certificates(MeId);
	    List ->
		List
	end,
    {ok, {trustedCertificateId, _, [Id]}} = 
	extract_element(trustedCertificateId, [hd(AllTCs)]),
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'TrustedCertificate',
		    [{trustedCertificateId, [Id]},
		     {managedState, ["ENABLED"]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end,
    ok.

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.12 Invalidate(/validate) a trusted certificate
%%% This UC is split in half
%%% @end
%%%--------------------------------------------------------------------
invalidate_trusted_certificate(Config) ->
    MeId = proplists:get_value(meId, Config),
    AllTCs = 
	case get_all_trusted_certificates(MeId) of
	    [] ->
		install_trusted_certificate_sftp(Config),
		get_all_trusted_certificates(MeId);
	    List ->
		List
	end,
    {ok, {trustedCertificateId, _, [Id]}} = 
	extract_element(trustedCertificateId, [hd(AllTCs)]),
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'TrustedCertificate',
		    [{trustedCertificateId, [Id]},
		     {managedState, ["DISABLED"]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end,
    ok.


%%%--------------------------------------------------------------------
%%% @doc UC 3.2.14 Cancel a node credential enrollment
%%% Cancel an ongoing online node credential enrollment
%%% @end
%%%--------------------------------------------------------------------
cancel_node_credential_enrollment(Config) ->
    MeId = proplists:get_value(meId, Config),
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
                    [{nodeCredentialId, ["certm_SUITE_offline"]},
                        {cancelEnrollment, [], []}]}]}]}]}]},

    ct:pal("~p~n",[Conf]),
		

    EditRes = netconf(edit_config, [nc1, running, Conf]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.


%%%--------------------------------------------------------------------
%%% @doc UC 3.2.16 Remove a node credential
%%% Removes all node credentials unless there is a specific nodeCredentialId
%%% given as Config
%%% @end
%%%--------------------------------------------------------------------
remove_node_credential(Config) ->
    
    ct:pal("remove_node_credential. waiting 5 minutes ~n",[]),

    timer:sleep(5*60000),

    MeId = proplists:get_value(meId, Config),
    NcIds =  case proplists:get_value(nodeCredentialId, Config) of
		 undefined ->
		     [begin
			  {ok, I} = extract_element(nodeCredentialId, [NC]),
			  I
		      end||NC<-get_all_node_credentials(MeId)];
		 NcId ->
		     [NcId]
	     end,
    
    DeleteOps = [{'NodeCredential', [{'xc:operation', "delete"}], [Id]}||
		    Id<-NcIds],
	       
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

    ct:pal("~p~n",[Edit]),
		
    
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.


%%%--------------------------------------------------------------------
%%% @doc UC 3.2.17 Remove a trust category
%%% @end
%%%--------------------------------------------------------------------

remove_trust_category(Config) ->
    MeId = proplists:get_value(meId, Config),
    %% Maybe ensure that there is a trust category, later
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
		  [{certMId,[],["1"]},
		   {'TrustCategory', [{'xc:operation', "delete"}],
		    [{trustCategoryId, [], ["certm_SUITE"]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> ok;
	_ ->
	    ct:fail(EditRes)
    end.

%%%--------------------------------------------------------------------
%%% @doc UC 3.2.18 Remove a trusted certificate
%%% @end
%%%--------------------------------------------------------------------
remove_trusted_certificate(Config) ->
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
    ActionId = get_action_id(reportProgress, ProgressFilter),
    ct:pal("ActionId = ~p~n",[ActionId]),

    AllTCs = get_all_trusted_certificates(MeId),
    MoRef = make_tc_moref(MeId, hd(AllTCs)),

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
        ["SUCCESS"] ->
            ok;
        Result ->
            ct:pal("removeTrustedCert: ~s~n",[Result]),
            ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% @doc Enrollment with invalid URI
%%% @end
%%%--------------------------------------------------------------------
negative_invalid_uri(Config) ->
    
    ct:pal("negative_invalid_uri ~n",[]),
    
     MeId = proplists:get_value(meId, Config),
    
    
    ct:pal("create enrollment server with empty URI~n",[]),
    EmptyUri = "",
    
        Edit2 = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'EnrollmentServerGroup',
		    [{enrollmentServerGroupId, ["certm_SUITE"]},
		     {'EnrollmentServer',
		      [{enrollmentServerId, ["certm_SUITE_neg"]},
		       {protocol, ["CMP"]},
		       {uri, [EmptyUri]}, 
		       {userLabel, ["Created by certm_SUITE"]}]}]}]}]}]}]},
    EditRes2 = netconf(edit_config, [nc1, running, Edit2]),
   case EditRes2 of
	ok -> 
	    ct:fail("Should not be possible to configure empty URI");
	_ ->
	    ok
    end,
    
    ct:pal("create enrollment server with faulty URI~n",[]),
    FaultyUri = "http://2001:1b70:82a1:3:1::c05:26772/cmp",
    
   
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {'SystemFunctions',
	      [{systemFunctionsId,[],["1"]},
	       {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		[{secMId,[],["1"]},
		 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		  [{certMId,[],["1"]},
		   {'EnrollmentServerGroup',
		    [{enrollmentServerGroupId, ["certm_SUITE"]},
		     {'EnrollmentServer',
		      [{enrollmentServerId, ["certm_SUITE_neg"]},
		       {protocol, ["CMP"]},
		       {uri, [FaultyUri]}, 
		       {userLabel, ["Created by certm_SUITE"]}]}]}]}]}]}]},
    EditRes = netconf(edit_config, [nc1, running, Edit]),
    case EditRes of
	ok -> 
	    ct:fail("Should not be possible to configure faulty URI");
	_ ->
	    ok
    end.



%%%--------------------------------------------------------------------
%%% @doc Intentionally empty TC. To be removed later on.
%%% @end
%%%--------------------------------------------------------------------

x(_) ->
    ok.

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: Make a Trusted certificate object reference from netconf data
%%%--------------------------------------------------------------------

make_tc_moref(MeId, TC) ->
    {ok, {trustedCertificateId, _, [Id]}} = 
	extract_element(trustedCertificateId, [TC]),
    "ManagedElement="++MeId++",SystemFunctions=1,SecM=1,CertM=1,"
	"TrustedCertificate="++Id.

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
%%% Description: Read out all NodeCredential MO instances
%%%--------------------------------------------------------------------

get_all_node_credentials(MeId) ->
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
		 element(1, Element) == 'NodeCredential'].

%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------

cmd(Cmd) ->
    ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.
%%%--------------------------------------------------------------------
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(Type, ProgressFilter) ->
    get_progress_report_member(actionId, Type, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(Type, ProgressFilter) ->
    get_progress_report_member(resultInfo, Type, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, Type, ProgressFilter) ->
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
    ct:pal("ProgressFilter result: ~p~n", [A]),
    case extract_element(Type, A) of
        {ok, {Type, L, _}}  ->
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
format_progress_report({enrollmentProgress, _, Members}) ->
    [io_lib:format("enrollmentProgress:~n",[])|format_progress_report(Members)];
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




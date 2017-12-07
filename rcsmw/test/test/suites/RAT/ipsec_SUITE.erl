%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ipsec_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R7A/R8A/R9A/R11A/1
%%% 
%%% @doc == TestSuite for testing OAM access over IPSec.==
%%% <br/><br/>
%%% @end

-module(ipsec_SUITE).
-vsn('/main/R5A/R6A/R7A/R8A/R9A/R11A/1').
-author('etxivri').

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
%%% Rev        Date        Name        What
%%% -----      ---------   --------    ------------------------
%%% R5A/1      2016-04-05  etxnnor     Created
%%% R5A/2      2016-04-07  etxnnor     Updated
%%% R5A/3      2016-04-07  etxnnor     Updated
%%% R6A/1      2016-08-11  etxivri     Make more robust.
%%% R6A/2      2016-08-11  etxivri     Make more robust.
%%% R6A/3      2016-09-20  etxnnor     Added several checks if tunnel is up after config.
%%% R6A/4      2016-09-21  etxnnor     Added check if ok/error after commit config.
%%% R7A/1      2016-11-02  etxivri     Add a check to why Inner interface commit sometimes fails due to re.
%%% R7A/2      2016-11-02  etxivri     Forgot to remove a break. 
%%% R7B/1      2017-01-25  erarube     Adapted to handle TN_C for DUS33, DUS53, MACRO 6303, MICRO 6502.
%%% R7B/2      2017-01-26  erarube     Fix BOARDTYPE unused when TN_PORT = "TN_A".
%%% R8A/3      2017-03-01  etxivri     A try to make it more robust.
%%% R9A/1      2017-04-03  etxkols     Fixed rct_netconf hook.
%%% R9A/2      2017-04-03  etxkols     Fixed rct_netconf hook.
%%% R11A/2     2017-09-06  etxivri     skip delete backup in end_per_tc.
%%% ----------------------------------------------------------
%%% 

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 test_oam_access/1]).

-define(BU_NAME, "before_ipsec_config").
-define(BRM_PROGRESS_STATE_TIMEOUT, 45000).
-define(BRM_LDN, "ManagedElement=1,SystemFunctions=1,BrM=1,BrmBackupManager=1").
-define(TRUST_FINGER, "E2:78:A9:D5:81:6C:4A:CB:10:BB:0E:99:B3:BE:16:1D:3E:AE:68:04").
-define(NC_FINGER, "3f:3c:48:3e:16:fa:37:34:3f:b8:dc:54:14:41:f4:21:d7:80:3d:25").
-define(NODE_CERT, "/backup/sftp/etxnnor/cert/rbs_node_generic.pkcs12").
-define(NC_PASS, "1Ericsson").
-define(RE_OPTION, [global, {capture, all, list}]).

suite() -> 
    %%% TN_C used for DUS33, DUS53, MACRO 6303, MICRO 6502, no need to change here. 
    case check_if_vc_board()  of
        "yes"->  [{ct_hooks, [{rct_cli,{cli,[manual_connect]}},
			      {rct_cli,[{1, cli_ipsec, ssh_TN_A_ipv4_ipsec, [manual_connect]}]},
			      {rct_cli,[{1, cli_alt, ssh_TN_A_ipv4_alt, [manual_connect]}]},
			      {rct_coli,[{1, coli_ipsec, ssh_TN_A_ipv4_ipsec, [manual_connect]}]},
			      {rct_netconf, {[{1, nc_ipsec, ssh_TN_A_ipv4_ipsec}], man_auth}}]}];
	_->      [{ct_hooks, [{rct_logging,[]},
			      {rct_core,[]},
			      {rct_cli,{cli,[manual_connect]}},
			      {rct_coli,[{1, coli_ipsec, ssh_TN_A_ipv4_ipsec, [manual_connect]}]},
			      {rct_netconf, {[{1, nc_ipsec, ssh_TN_A_ipv4_ipsec}], expert_auth}},
			      {rct_cli,[{1, cli_ipsec, ssh_TN_A_ipv4_ipsec, [manual_connect]}]},
			      {rct_cli,[{1, cli_alt, ssh_TN_A_ipv4_alt, [manual_connect]}]}]}]
    end.

init_per_suite(Config) ->
    Config.
end_per_suite(_Config) ->
    ok.
init_per_testcase(_TestCase, Config) ->
    case check_if_vc_board()  of
        "yes"-> rct_cli:connect(cli, "SysAdminTest","SysAdminTest"),
		rct_cli:connect(cli_alt, "SysAdminTest","SysAdminTest");
    	_->     rct_cli:connect(cli),
		rct_cli:connect(cli_alt)
    end,
    BackupId = create_delete_backup(cli, "createBackup"),
    NewConfig = [Config, {backup, lists:flatten(BackupId)}],
    lists:flatten(NewConfig).
end_per_testcase(_TestCase, Config) ->
    rct_cli:disconnect(cli),
    BackupId = proplists:get_value(backup, Config),
    ok = restore_backup(cli_alt, BackupId),
    ok = check_progress(["result=SUCCESS", "state=FINISHED"], 
    			?BRM_LDN ++ " ," ++ BackupId, 
    			100, cli_alt), 
    
    %% _ = create_delete_backup(cli, "deleteBackup"),
    rct_cli:disconnect(cli),
    rct_cli:disconnect(cli_ipsec),
    rct_cli:disconnect(cli_alt).

all() -> 
    [test_oam_access].

test_oam_access(Config) ->
    ok = configure_certs(),
    timer:sleep(5000),
    ok = activate_license(),
    timer:sleep(5000),
    ok = configure_ikev2_profile(),
    timer:sleep(5000),
    ok = configure_ipsec_proposal_profile(),
    timer:sleep(5000),
    ok = configure_vlan_routers_and_ipsec_tunnel(),
    timer:sleep(5000),
    case check_tunnel(30000) of
	ok -> ct:log("Tunnel is up");
	_ -> ct:fail("IpSec Tunnel is down after configuration")
    end,
    ok = configure_oam_access_over_ipsec(),

    timer:sleep(30000),

    ct:pal("### Verify CLI access."),
    case check_if_vc_board()  of
        "yes"-> rct_cli:connect(cli_ipsec, "SysAdminTest","SysAdminTest");
    	_->     rct_cli:connect(cli_ipsec)
    end,
    {ok, OamAccess} = rct_cli:send(cli_ipsec, "show all ManagedElement=1,SystemFunctions=1,SysM=1,OamAccessPoint=1"),
    case re:run(OamAccess, "accessPoint=\"ManagedElement=1,Transport=1,Router=OAM_IPSEC,InterfaceIPv4=INNER,AddressIPv4=1\"") of
        {match, _} -> ct:log("Login over IpSec OamAccess successful");
        nomatch -> ct:fail("Not possible to login over IpSec OamAccess")
    end,

    ct:pal("### Verify COLI access."),
    case check_if_vc_board()  of
        "yes"-> rct_coli:connect(coli_ipsec, "SysAdminTest","SysAdminTest");
    	_->     rct_coli:connect(coli_ipsec)
    end,
    {ok,_} = rct_coli:send(coli_ipsec,"help coli", "Coli cmd shell usage"),    
    ok = rct_coli:disconnect(coli_ipsec),

    ct:pal("### Verify netconf access."),
    F = fun() -> {ok,_} = netconf_open(nc_ipsec,[]),
		 {ok,_} = ct_netconfc:get(nc_ipsec,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],[{managedElementId,[],["1"]}]}),
    		 ok = ct_netconfc:close_session(nc_ipsec)
    	end,    
    try F()
    catch
    	_:Reason ->
    	    ct_netconfc:close_session(nc_ipsec),
    	    ct:fail(Reason)
    end,
    Config.


%%%% Helper functions
activate_license() ->
    ct:pal("### Activate license for Ipsec."),
    rct_cli:send(cli,"configure"),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,SystemFunctions=1,Lm=1,FeatureState=CXC4040004"),
    {ok, _} = rct_cli:send(cli, "featureState=ACTIVATED"),
    {ok, _} = rct_cli:send(cli, "commit",{".*FeatureState.*",?RE_OPTION}),
    ok.

configure_ikev2_profile() ->
    ct:pal("### Configure IKEv2 profile."),
    rct_cli:send(cli,"configure"),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,Ikev2PolicyProfile=1"),
    {ok, _} = rct_cli:send(cli, "credential=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=RBS_NODE_GENERIC\""),
    {ok, _} = rct_cli:send(cli, "trustCategory=\"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1\""),
    {ok, _} = rct_cli:send(cli, "dpdTime=60"),
    {ok, _} = rct_cli:send(cli, "ikeDscp=48"),
    {ok, _} = rct_cli:send(cli, "ikeSaLifetime=3600"),
    {ok, _} = rct_cli:send(cli, "ikev2Proposal"),
    {ok, _} = rct_cli:send(cli, "diffieHellmanGroup=MODP_2048_GROUP_14 encryptionAlgorithm=ENCR_AES_CBC_128 integrityAlgorithm=AUTH_HMAC_SHA1_96 pseudoRandomFunction=PRF_HMAC_SHA1"),
    {ok, _} = rct_cli:send(cli, "commit", {".*Ikev2PolicyProfile.*",?RE_OPTION}),
    ok.

configure_ipsec_proposal_profile() ->
    ct:pal("### Configure IPSEC proposal profile."),
    rct_cli:send(cli,"configure"),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,IpsecProposalProfile=1",print),
    {ok, _} = rct_cli:send(cli, "childSaLifetime"),
    {ok, _} = rct_cli:send(cli, "dataLimit=20000"),
    {ok, _} = rct_cli:send(cli, "timeLimit=1440"),
    {ok, _} = rct_cli:send(cli, "commit", {".*IpsecProposalProfile.*", ?RE_OPTION}),
    ok.

configure_vlan_routers_and_ipsec_tunnel() ->
    Hw = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    [{ssh, Inner_ip}, {ssh_outer, Outer_ip}, {netmask_outer, Outer_netmask}, {vlan, Vlan}, {gateway_outer, SecGw_ip}] = ct:get_config({list_to_atom(Hw), ssh_TN_A_ipv4_ipsec}),
        %%% DUSx3 Boards use TN_C
    case ct:get_config({Hwa,board_type}) of
    	BOARDTYPE when BOARDTYPE == "dus5301";		%dus53 6630
    		       BOARDTYPE == "dus3301";		%dus33 6620
		       BOARDTYPE == "dus6502";		%micro
		       BOARDTYPE == "dus6303" ->	%macro
    		TN_PORT = "TN_C";
	_BOARDTYPE  ->
		TN_PORT = "TN_A"     
    end,
    ct:pal("### Configure tunnel VLAN ID ~p.", [Vlan]),
    rct_cli:send(cli,"configure"),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,VlanPort=" ++ TN_PORT ++ "_OAM_" ++ integer_to_list(Vlan)),
    {ok, _} = rct_cli:send(cli, "encapsulation=ManagedElement=1,Transport=1,EthernetPort=" ++ TN_PORT),
    {ok, _} = rct_cli:send(cli, "vlanId=" ++ integer_to_list(Vlan)),
    ct:pal("### Configure Outer interface."),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,Router=IPSEC"),
    {ok, _} = rct_cli:send(cli, "InterfaceIPv4=OUTER"),
    {ok, _} = rct_cli:send(cli, "encapsulation=ManagedElement=1,Transport=1,VlanPort=" ++ TN_PORT ++ "_OAM_" ++ integer_to_list(Vlan)),
    {ok, _} = rct_cli:send(cli, "AddressIPv4=1"),
    {ok, _} = rct_cli:send(cli, "address=" ++ Outer_ip ++ "/" ++ Outer_netmask),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,Router=IPSEC"),
    {ok, _} = rct_cli:send(cli, "PeerIPv4=SecureGateway-1"),
    {ok, _} = rct_cli:send(cli, "address=" ++ SecGw_ip),
    {ok, _} = rct_cli:send(cli, "commit -s", {".*SecureGateway-1.*",?RE_OPTION}),
    timer:sleep(5000),
    ct:pal("### Configure IpSec tunnel."),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,Router=OAM_IPSEC"),
    {ok, _} = rct_cli:send(cli, "IpsecTunnel=OAM"),
    {ok, _} = rct_cli:send(cli, "localAddress=ManagedElement=1,Transport=1,Router=IPSEC,InterfaceIPv4=OUTER,AddressIPv4=1"),
    {ok, _} = rct_cli:send(cli, "remoteAddress=ManagedElement=1,Transport=1,Router=IPSEC,PeerIPv4=SecureGateway-1"),
    {ok, _} = rct_cli:send(cli, "Ikev2Session=1"),
    {ok, _} = rct_cli:send(cli, "ikev2PolicyProfile=ManagedElement=1,Transport=1,Ikev2PolicyProfile=1"),
    {ok, _} = rct_cli:send(cli, "localIdentityOrigin=SUBJECT_ALT_NAME"),
    {ok, _} = rct_cli:send(cli, "up"),
    {ok, _} = rct_cli:send(cli, "IpsecPolicy=1"),
    {ok, _} = rct_cli:send(cli, "ipsecProposalProfile=ManagedElement=1,Transport=1,IpsecProposalProfile=1"),
    {ok, _} = rct_cli:send(cli, "commit -s", {".*IpsecPolicy.*",?RE_OPTION}),
    timer:sleep(5000),
    ct:pal("### Configure Inner interface."),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,Router=OAM_IPSEC"),
    {ok, _} = rct_cli:send(cli, "InterfaceIPv4=INNER"),
    {ok, _} = rct_cli:send(cli, "encapsulation=ManagedElement=1,Transport=1,Router=OAM_IPSEC,IpsecTunnel=OAM"),
    {ok, _} = rct_cli:send(cli, "AddressIPv4=1"),
    {ok, _} = rct_cli:send(cli, "address=" ++ Inner_ip ++ "/32"),
    %% {ok, _} = rct_cli:send(cli, "commit -s", {".*AddressIPv4.*",?RE_OPTION}),
    commit(cli, "commit -s", {".*AddressIPv4.*",?RE_OPTION}),
    timer:sleep(5000),

    ct:pal("### Configure Default route to use tunnel."),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,Transport=1,Router=OAM_IPSEC"),
    {ok, _} = rct_cli:send(cli, "RouteTableIPv4Static=1"),
    {ok, _} = rct_cli:send(cli, "Dst=Default"),
    {ok, _} = rct_cli:send(cli, "dst=0.0.0.0/0"),
    {ok, _} = rct_cli:send(cli, "NextHop=IpSecTunnel"),
    {ok, _} = rct_cli:send(cli, "reference=ManagedElement=1,Transport=1,Router=OAM_IPSEC,InterfaceIPv4=INNER"),
    {ok, _} = rct_cli:send(cli, "commit", {".*IpSecTunnel.*",?RE_OPTION}),
    ok.


commit(Cli, CommitCmd, {SearchStr, ReOpt}) ->
    {ok, Res} = rct_cli:send(Cli, CommitCmd),
    ct:log("# Res: ~p", [Res]),
    case re:run(Res, "ERROR:") of
    	nomatch -> %% Commit is ok
	    ct:log("# Search for expected search str : ~p.", [SearchStr]);
    	{match, _} ->
    	    ct:fail("# Commit NOK, TC will fail")
    end,
    case re:run(Res, SearchStr, ReOpt) of
	{match, Res2} ->
	    ct:log("# Search str matched OK. ~n Res2 : ~p", [Res2]);
	 nomatch ->
	    ct:fail("# Don't match expected search str, NOK, TC will fail")
    end,
    ct:log("# Commit OK #", []).
    

configure_certs() ->
    ct:pal("### Configure Node Credential."),
    rct_cli:send(cli,"configure"),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=RBS_NODE_GENERIC"),
    {ok, _} = rct_cli:send(cli, "subjectName=\"C=SE,ST=Stockholm,L=Kista,O=Labbet,CN=RBS_NODE_GENERIC\""),
    {ok, _} = rct_cli:send(cli, "commit -s", {".*NodeCredential.*",?RE_OPTION}),
    ct:pal("### Install Trusted Cert."),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"),
    {ok, _} = rct_cli:send(cli, "installTrustedCertFromUri --uri \"\" --uriPassword \"\" --fingerprint " ++ ?TRUST_FINGER),
    {ok, _} = rct_cli:send(cli, "commit -s", {".*CertM.*",?RE_OPTION}),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1"),
    {ok, _} = rct_cli:send(cli, "trustedCertificates=ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=1"),
    {ok, _} = rct_cli:send(cli, "commit -s", {".*TrustCategory.*",?RE_OPTION}),
    ct:pal("### Install Node  Cert."),
    [{host, Sftp_Ip}, {username, User}, {password, Pass}] = ct:get_config({sftp_server}),
    Uri = "sftp://" ++ User ++ "@" ++ Sftp_Ip ++ ?NODE_CERT,
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=RBS_NODE_GENERIC"),
    {ok, _} = rct_cli:send(cli, "installCredentialFromUri --uri " ++ Uri ++ " --uriPassword " ++ Pass ++ " --fingerprint " ++ ?NC_FINGER ++ " --credentialPassword " ++ ?NC_PASS),
    {ok, _} = rct_cli:send(cli, "commit", {".*NodeCredential.*",?RE_OPTION}),
    ok.

configure_oam_access_over_ipsec() ->
    ct:pal("### Configure oam access point over IpSec."),
    rct_cli:send(cli,"configure"),
    {ok, _} = rct_cli:send(cli, "ManagedElement=1,SystemFunctions=1,SysM=1,OamAccessPoint=1"),
    {ok, _} = rct_cli:send(cli, "accessPoint=ManagedElement=1,Transport=1,Router=OAM_IPSEC,InterfaceIPv4=INNER,AddressIPv4=1"),
    {ok, _} = rct_cli:send(cli, "commit", {".*OamAccessPoint.*",?RE_OPTION}),
    ok.

check_if_vc_board() ->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

create_delete_backup(Cli, Operation) ->
    ct:pal("### " ++ Operation ++ " performed."),
    {ok,_} = rct_cli:send(Cli, ?BRM_LDN),
    {ok,_} = rct_cli:send(Cli, Operation ++ " " ++ ?BU_NAME),
    ok = check_progress(["result=SUCCESS", "state=FINISHED"], 
                                                  ?BRM_LDN, 
                                                  100, Cli), 
    {ok, BackupManagerData} = rct_cli:send(Cli,"show " ++ ?BRM_LDN),
    {match, BackupId} = re:run(BackupManagerData, "(BrmBackup\=\\d+)", [global, {capture, [1], list}]),
    [lists:last(lists:append(BackupId))].

restore_backup(Cli, BackupId) ->
    ct:pal("### Restore original configuration. ~p", [BackupId]),
    {ok,_} = rct_cli:send(Cli, ?BRM_LDN ++ "," ++ BackupId),
    {ok,_} = rct_cli:send(Cli, "restore"),
    ok.

check_progress([], _Mo, _NoOfTries, _CliSession) ->
    ok;

check_progress([WantedString | RestWantedList], Mo, NoOfTries, CliSession) ->
    RecievedData = rct_cli:send(CliSession, "show verbose "++Mo, print), 
    case RecievedData of
        {ok, Data} ->
            case check_data(WantedString, Data) of
                ok ->
                    check_progress(RestWantedList, Mo, NoOfTries, CliSession);
                nok ->
                    case NoOfTries of
                        0 ->
                            nok;
                        _Else ->
                            ct:pal("Did not find ~p, trying again in 5 sec", [WantedString]),
                            timer:sleep(5000),
                            check_progress([WantedString] ++ RestWantedList, 
                                                                     Mo, NoOfTries -1, CliSession)
                    end
            end;
        {error,ssh_session_not_alive} ->
            ok = rct_cli:disconnect(CliSession),
            ct:pal("Reboot, Sleep 120 sec before check progress report again"),
            timer:sleep(120000),
	    
	    case check_if_vc_board()  of
		"yes" ->
		    case rct_cli_connect(cli, "SysAdminTest","SysAdminTest", 30) of
			ok ->
			    check_progress([WantedString] ++ RestWantedList, 
								     Mo, NoOfTries -1, cli);
			nok ->
			    nok
		    end;	    
		_ ->
		    case rct_cli_connect(cli, "expert","expert", 30) of
			ok ->
			    check_progress([WantedString] ++ RestWantedList, 
								     Mo, NoOfTries -1, cli);
			nok ->
			    nok
		    end
	    end	    
    end.

rct_cli_connect(CliSession, User, Pass, Tries)->
    Return =
        case rct_cli:connect(CliSession, User, Pass) of
            ok ->  
                ok;
            _ ->
                case Tries of
                    0 ->
                        nok;
                    _Else ->
                        ct:pal("rct_cli connect after reboot trying again in 5 sec"),
                        timer:sleep(5000),
                        rct_cli_connect(CliSession, User, Pass, Tries-1)
                end                 
        end,
    Return.

netconf_open(Session, InPar)->
    Param = [{timeout,10000}|InPar],
    case check_if_vc_board()  of
	"yes" ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param]);
	_ -> ct_netconfc:open(Session,Param)
    end.

check_data(WantedString, RecievedData) ->
    Options = [global, %% check all rows
               {capture, all, binary}],
    case re:run(RecievedData, WantedString, Options) of
        {match, [[_Value]]} ->
            ok;
        nomatch ->
            nok
    end.

check_tunnel(0) ->
    ct:pal("### ERROR: Timeout waiting for tunnel to come up!"),
    nok;
check_tunnel(Timer) ->
    {ok, Tunnel} = rct_cli:send(cli, "show ManagedElement=1,Transport=1,Router=OAM_IPSEC,IpsecTunnel=OAM"),
    case re:run(Tunnel, "operationalState=ENABLED") of
        {match, _} -> ok;
        nomatch -> timer:sleep(1000),
		   ct:pal("### Checking state. Timer = ~p ms ~nTunnel is not up yet, trying again...", [Timer]),
		   check_tunnel(Timer - 1000)
    end.

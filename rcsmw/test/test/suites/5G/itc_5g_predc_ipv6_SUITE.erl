%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	itc_5g_predc_ipv6_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/1
%%% 
%%% @doc == TestSuite for ITC communication between RCF and BPU.==
%%% <br/><br/>
%%% @end

-module(itc_5g_predc_ipv6_SUITE).
-vsn('/main/R8A/1').
-author('etxivri').

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
%%% Rev        Date        Name        What
%%% -----      ---------   --------    ------------------------
%%% R8A/1      2017-02-06  etxivri     Created for IPv6.
%%% ----------------------------------------------------------
%%% 

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 groups/0,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 test_setup_itc/1,
	 test_locate_bpf/1,
	 test_basic_itc_msg/1,
	 test_large_itc_msg/1,
	 test_multiple_itc_msg/1,
	 test_multiple_large_itc_msg/1,
	 test_monitor/1,
	 test_subscribe_publish/1,
	 test_reboot_reconnect/1,
	 scp_itc/1,
	 configure_tn/1,
	 break/1,
	 clean/1]).

-define(TN_NAMESPACE, "ip netns exec fib_2").
%% Soon will only VRCS_64 be supported.
-define(ITC_TOOL_FROM_PATH_X86_64, "/proj/rcs/misc/itc_ns_test/build/x86/itc_64").
-define(ITC_TOOL_FROM_PATH_X86, "/proj/rcs/misc/itc_ns_test/build/x86/itc").
-define(ITC_TOOL_FROM_PATH_ARM, "/proj/rcs/misc/itc_ns_test/build/arm/itc").
-define(ITC_TOOL_TO_PATH_X86, "/usr/bin/").
-define(ITC_TOOL_TO_PATH_ARM, "/home/sirpa/dev_patches/").
-define(PS_TCPLNH, "ps -ef | grep tcplnh | grep -v grep").
-define(NS_INFO, "/bin/ns-info").
-define(INIT_MBOX_CMD, " init mbox_1 ").

%% %% this is needed until add_stp works
%% %% check settings in 
%% %%  https://lte-wiki.rnd.ki.sw.ericsson.se/mediawiki/index.php/5G/VRAN/Cloud/KI20_CEE/Redhat_production_environment
%% %% redhat007-rcs-ci, uses 2001:1b70:8299:100::/64 subnet ->  Gateway 2001:1b70:8299:100::1 used in nexthop
%% -define(TN_NS_GATEWAY, "2001:1b70:8299::2").  
%% -define(SubNet, "64").
%% -define(FronthaulNet, "2001:1b70").

%% redhat007-rcs -> Sanbox, uses 2001:1b70:6282:b0e0::/64 subnet ->  Gateway 2001:1b70:6282:1 used in nexthop
-define(TN_NS_GATEWAY, "2001:1b70:6282:b0e0::2").  
-define(SubNet, "64").
-define(FronthaulNet, "2001:1b70").

%% -define(STP, "rcf_2_etxivri_dus5075").




suite() -> 
    [{ct_hooks, [{rct_cli, {cli_rcf, []}},
		 {rct_logging,[]},
		 {rct_scp, [{1, scp_rcf}]},
		 {rct_scp, [{2, scp_bpu}]},
		 {rct_rs232,[{2, console_bpu}]},
		 {rct_rpc, [{2, rpc_bpu}]},
		 {rct_ssh,[{1, ssh_rcf, [{ip_type, ssh_lmt_ipv4}]}]},
		 {rct_ssh,[{2, ssh_bpu, [{ip_type, ssh_lmt_ipv4}]}]}]}].
%%               {rct_ssh,[{1, ssh_rcf, [{ip_type, ssh_lmt_ipv6}]}]},
%% 		 {rct_ssh,[{2, ssh_bpu, [{ip_type, ssh_lmt_ipv6}]}]}]}].

init_per_suite(Config) ->
    %% %% %% Soon will only VRCS_64 be supported.
    %% %% TestType = ct:get_config({jenkins_config, test_type}),
    %% TestType = "GIT_VRCS",
    %% ct:pal("## TestType : ~p", [TestType] ),
    %% case TestType of 
    %% 	"GIT_VRCS" ->
    %% 	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86,
    %% 					?ITC_TOOL_TO_PATH_X86, 10);
    %% 	"GIT_BRCS" ->
    %% 	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86,
    %% 					?ITC_TOOL_TO_PATH_X86, 10);
    %% 	"GIT_VRCS64" -> 
    %% 	    %% cp itc_64 to itc then this SUITE not need more updates.
    %% 	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86_64,
    %% 					?ITC_TOOL_TO_PATH_X86 ++ "itc", 10); 
    %% 	_Other -> 
    %% 	    %% cp itc_64 to itc then this SUITE not need more updates.
    %% 	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86_64,
    %% 					?ITC_TOOL_TO_PATH_X86 ++ "itc", 10)  
    %% end,
    %% Ls_Itc = ct_ssh:exec(ssh_rcf, "ls -l "++?ITC_TOOL_TO_PATH_X86++"itc", 30000),
    %% ct:pal("ls -l on itc : ~p ", [Ls_Itc]),
    %% {ok,_} =  rct_scp:to_target(scp_bpu, ?ITC_TOOL_FROM_PATH_ARM,
    %% 				?ITC_TOOL_TO_PATH_ARM, 10),


    %% redhat007-rcs, check settings in 
    %% https://lte-wiki.rnd.ki.sw.ericsson.se/mediawiki/index.php/5G/VRAN/Cloud/KI20_CEE/Redhat_production_environment
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    [{ssh, Rcf_Tn_Ip}, _, {netmask, Rcf_Tn_Mask}, {gateway, Gateway}] = ct:get_config({list_to_atom(Hw), ssh_TN_A_ipv6}),
    ct:pal("Hw : ~p", [Hw]),
    ct:pal("Rcf_Tn_Ip : ~p", [Rcf_Tn_Ip]),
    ct:pal("Rcf_Tn_Mask : ~p", [Rcf_Tn_Mask]),
    ct:pal("Gateway : ~p", [Gateway]),
    {A_Rcf_Tn_Ip, A_Rcf_Tn_Mask, A_Gateway} =
	case Rcf_Tn_Ip of
	    [] ->
		%% redhat007-rcs Sandbox or rcs-ci
		B_Rcf_Tn_Ip = get_pran_fronthaul_addr(),
		ct:pal("### B_Rcf_Tn_Ip: ~p", [B_Rcf_Tn_Ip]),
		B_Rcf_Tn_Mask = ?SubNet,
		ct:pal("### B_Rcf_Tn_Mask: ~p", [B_Rcf_Tn_Mask]),
		B_Gateway = ?TN_NS_GATEWAY,
		ct:pal("### B_Gateway: ~p", [B_Gateway]),
		{B_Rcf_Tn_Ip, B_Rcf_Tn_Mask, B_Gateway};
	    _ConfExst -> %% Kalle fixed so config for fronthaul esxist in stp config.
		ct:pal("### Fronthaul config exist in stp.cfg to be used.", []),
		{Rcf_Tn_Ip, Rcf_Tn_Mask, Gateway}
	end,

    [%% {ip_type, IpType}
     {fronthaul_ip, A_Rcf_Tn_Ip},
     {fronthaul_subnet, A_Rcf_Tn_Mask},
     {fronthaul_gateway, A_Gateway} | Config].
    %% Config.




scp_itc(Config) ->
    ct:pal("Config : ~p", [Config]),
    %% %% Soon will only VRCS_64 be supported.
    TestType = ct:get_config({jenkins_config, test_type}),
    %% TestType = "GIT_VRCS",
    %% TestType = "GIT_VRCS64",
    ct:pal("## TestType : ~p", [TestType] ),
    case TestType of 
    	"GIT_VRCS" ->
    	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86,
    					?ITC_TOOL_TO_PATH_X86, 10);
    	"GIT_BRCS" ->
    	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86,
    					?ITC_TOOL_TO_PATH_X86, 10);
    	"GIT_VRCS64" -> 
    	    %% cp itc_64 to itc then this SUITE not need more updates.
    	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86_64,
    					?ITC_TOOL_TO_PATH_X86 ++ "itc", 10); 
    	_Other -> 
    	    %% cp itc_64 to itc then this SUITE not need more updates.
    	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86_64,
    					?ITC_TOOL_TO_PATH_X86 ++ "itc", 10)  
    end,
    Ls_Itc = ct_ssh:exec(ssh_rcf, "ls -l "++?ITC_TOOL_TO_PATH_X86++"itc", 30000),
    ct:pal("ls -l on itc : ~p ", [Ls_Itc]),
    {ok,_} =  rct_scp:to_target(scp_bpu, ?ITC_TOOL_FROM_PATH_ARM,
    				?ITC_TOOL_TO_PATH_ARM, 10),
    ok.

get_pran_fronthaul_addr() ->
    Hw = atom_to_list(ct:get_config({test_nodes,1})), %% to get vrcf config
    Cat = os:cmd("cat /proj/rcs-tmp/stps/"++Hw++"/config/vrcs.txt"),
    ct:pal("Cat : ~p", [Cat]),
    FixCat = fix_str(Cat, "\n ,"),
    ct:pal("FixCat : ~p", [FixCat]),
    Fronthaul = dropwhile(FixCat, "pran_fronthaul"),
    ct:pal("Fronthaul : ~p", [Fronthaul]),
    ct:pal("search for ip adress  for fronthaul that start with : ~p ", [?FronthaulNet]),
    [IPAddr | _] = dropwhile_re_match(Fronthaul, ?FronthaulNet),
    ct:pal("Fronthaul IPAddr : ~p", [IPAddr]),
    IPAddr.

dropwhile(List, MatchStr) ->
    RestList = lists:dropwhile(fun(X) ->
				   case X of
				       MatchStr -> %% stop and return rest of list.
					   false;
				       _ ->
					   true
				   end
			   end, List),
    ct:pal("RestList : ~n~p", [RestList]),
    RestList.

dropwhile_re_match(List, MatchStr) ->
    RestList = lists:dropwhile(fun(X) ->
				   case re:run(X, MatchStr) of
				       {match, _} -> %% stop and return rest of list.
					   false;
				       nomatch ->
					   true
				   end
			   end, List),
    ct:pal("RestList_re_match : ~n~p", [RestList]),
    RestList.

end_per_suite(_Config) ->
    %% ok = clean_up(ssh_rcf),
    %% ok = clean_up(ssh_bpu),
    %% ok = disconnect_ssh().
    ok. 

init_per_group(_GroupName, Config) ->
    Config.
end_per_group(_GroupName, Config) ->
    ct:log("## end_per_group: ~n~p.  \n", [Config]),
    GrouResult = proplists:get_value(tc_group_result, Config),
    case proplists:get_value(failed, GrouResult) of
    	[] -> %% No TC failed.
	    ok;
	[Failed_TC]  ->
	    ct:pal("Failed TestCase : ~p .  \n", [Failed_TC]),
	    clean(Config)
    end.

init_per_testcase(TestCase, Config) ->
    ct:pal("# Start TC : ~p #", [TestCase]),
    Config.
end_per_testcase(TestCase, Config) ->
    ct:pal("# End TC : ~p #", [TestCase]),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n", [Reason]),
	    clean(Config)
    end.

all() -> 
    [scp_itc,
     configure_tn,
     test_setup_itc,
     test_locate_bpf,
     test_basic_itc_msg,
     test_large_itc_msg,
     test_multiple_itc_msg,
     test_multiple_large_itc_msg,
     test_monitor,
     test_subscribe_publish,
     test_reboot_reconnect,
     clean].


groups() ->
    [{group_all, [sequence],[scp_itc,
			     configure_tn,
			     test_setup_itc,
			     test_locate_bpf,
			     test_basic_itc_msg,
			     test_large_itc_msg,
			     test_multiple_itc_msg,
			     test_multiple_large_itc_msg,
			     test_monitor,
			     test_subscribe_publish,
			     test_reboot_reconnect,
			     clean
			    ]}
    ].

break(_Config) ->
    test_server:break("AA").

test_setup_itc(_Config) ->
    %% ok = configure_tn(),
    ct:pal("Test case 1: Open TCP port on RCF & BPU, connect RCF -> BPU. Tool used is tcplnh."),
    ok = connect_ssh(),
    ok = setup_itc(rcf, _Config),
    ok = setup_itc(bpu, _Config),
    Rcf = atom_to_list(ct:get_config({test_nodes,1})),
    %% [{ssh, Rcf_ip}, _, _ ,_] =  ct:get_config({list_to_atom(Rcf), ssh_TN_A_ipv6}),
    Rcf_ip = proplists:get_value(fronthaul_ip, _Config),
    ct:pal("## Rcf: ~p. Rcf_ip : ~p ##", [Rcf, Rcf_ip]),

    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    ct:pal("## Bpu: ~p. Bpu_ip : ~p ##", [Bpu, Bpu_ip]),
    connect_itc(ssh_bpu, Rcf_ip),
    ct:pal("### Sleep 5 seconds so all processes appears"),
    timer:sleep(5000),
    ok = verify_itc_conn(Bpu_ip).

test_locate_bpf(_Config) ->
    ct:pal("Test case 2: Init mbox_1 on both RCF and BPU, locate mbox_1 on BPU from RCF. Tool used is itc test app."),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    ok = init_mbox(rcf),
    ok = init_mbox(bpu),
    case ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " locate " ++ Bpu_ip ++ "/mbox_1", 30000) of
	{ok, Locate_reply} -> case re:run(Locate_reply, Bpu_ip ++ "/mbox_1 = 0x\\w+") of
				  {match, [_]} -> ct:pal("### Locate successful, locate reply: ~p", [Locate_reply]);
				  nomatch -> ct:fail("Process not found, locate reply: ~p", [Locate_reply])
			      end;
	{timeout,[]} -> ct:fail("Timeout when trying to locate BPF");
	Other -> ct:fail("Unknown error when trying to locate BPF: ~p", [Other])
    end,
    ok.

test_basic_itc_msg(_Config) ->
    ct:pal("Test case 3: Send one normal message RCF -> BPU. Tool used is itc test app."),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    case send_msg(Bpu_ip) of
	{ok, Result} -> ct:pal("Result: ~p", [Result]),
			Time = verify_send_receive(Result),
			ct:pal("Send/receive successful, time to send/receive: ~p", [Time]);
	{error, Reason} -> ct:fail("Reply: ~p", [Reason])
    end,
    ok.

test_large_itc_msg(_Config) ->
    ct:pal("Test case 4: Send one large message RCF -> BPU. Tool used is itc test app."),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    case send_msg("65535", 1, Bpu_ip) of
	{ok, Result} -> ct:pal("Result: ~p", [Result]),
			Time = verify_send_receive(Result),
			ct:pal("Send/receive successful, time to send/receive: ~p", [Time]);
	{error, Reason} -> ct:fail("Reply: ~p", [Reason])
    end,
    ok.

test_multiple_itc_msg(_Config) ->
    ct:pal("Test case 5: Send multiple 100 Bytes messages RCF -> BPU. Tool used is itc test app."),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    case send_msg("100", "100", Bpu_ip) of
	{ok, Result} -> ct:pal("Result: ~p", [Result]),
			Time = verify_send_receive(Result),
			ct:pal("Send/receive successful, time to send/receive: ~p", [Time]);
	{error, Reason} -> ct:fail("Reply: ~p", [Reason])
    end,
    ok.

test_multiple_large_itc_msg(_Config) ->
    ct:pal("Test case 6: Send multiple 50000 Bytes messages RCF -> BPU. Tool used is itc test app."),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    case send_msg("50000", "50", Bpu_ip) of
	{ok, Result} -> ct:pal("Result: ~p", [Result]),
			Time = verify_send_receive(Result),
			ct:pal("Send/receive successful, time to send/receive: ~p", [Time]);
	{error, Reason} -> ct:fail("Reply: ~p", [Reason])
    end,
    timer:sleep(60000),
    ok.

test_monitor(_Config) ->
    ct:pal("Test case 7: Monitor BPU from RCF, kill proc on BPU.  Tool used is itc test app."),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    case ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " monitor mbox_1 " ++ Bpu_ip ++ "/mbox_1", 5000) of
	{ok, Monitor_reply} -> case re:run(Monitor_reply, "mbox_1 is now monitoring " ++ Bpu_ip ++ "/mbox_1") of
				   {match, [_]} -> ct:pal("### Monitor BPF successful: ~p", [Monitor_reply]);
				   nomatch -> ct:fail("Monitor BPF failed.")
			       end;
	{timeout,[]} -> ct:fail("Timeout when trying to monitor BPF");
	Other1 -> ct:fail("Unknown error when trying to monitor BPF: ~p", [Other1])
    end,
    
    ct:pal("### Check monitor before killed process"),
    case ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " monitor_check " ++ Bpu_ip ++ "/mbox_1", 30000) of
	{ok, Mon_check_reply1} -> case re:run(Mon_check_reply1, Bpu_ip ++ "/mbox_1 not found") of
				  {match, [_]} -> ct:fail("Monitored process not found: ~p", [Mon_check_reply1]);
				  nomatch -> ct:pal("### Monitored process found which is ok.")
				 end;
	Other2 -> ct:fail("Unknown error when trying to check monitored process: ~p", [Other2])
    end,

    ct:pal("### Killing mbox on BPU"),
    {ok, Mbox} = ct_ssh:exec(ssh_bpu, "um list | grep mbox_1", 5000),
    case re:run(Mbox, "\\s+\\d+\\s+\\w+\\s+(\\d+)", [{capture,[1],list}]) of
	{match, [Pid]} -> ct:pal("### Pid to mbox_1 found: ~p", [Pid]),
			  {ok, _} = ct_ssh:exec(ssh_bpu, "kill " ++ Pid, 5000);
	nomatch -> ct:fail("Cannot find pid to mbox_1.")
    end,
    
    ct:pal("### Check monitor after killed process"),
    case ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " monitor_check " ++ Bpu_ip ++ "/mbox_1", 30000) of
	{ok, Mon_check_reply3} -> case re:run(Mon_check_reply3, Bpu_ip ++ "/mbox_1 not found") of
				  {match, [_]} -> ct:pal("### Monitored process not found: ~p", [Mon_check_reply3]);
				  nomatch -> ct:fail("Monitored process found but should have not")
				 end;
	Other3 -> ct:fail("Unknown error when trying to check monitored process: ~p", [Other3])
    end,

    ok.

test_subscribe_publish(_Config) ->
    ct:pal("Test case 8: Subscribe on services on one node, publish on the other. Tool used is itc test app and ns-info."),
    ok = init_mbox(bpu),
    ct:pal("### Subscribe on BPU SERVICE_A on RCF"),
    {ok, _} = ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " ns_subscribe mbox_1 SERVICE_A", 5000),
    ct:pal("### Publish BPU SERVICE_A"),
    {ok, _} = ct_ssh:exec(ssh_bpu, ?ITC_TOOL_TO_PATH_ARM ++ "itc" ++ " ns_publish mbox_1 SERVICE_A", 5000),
    {ok, Service_info} = ct_ssh:exec(ssh_rcf, ?NS_INFO ++ " srv SERVICE_A", 5000),
    case re:run(lists:flatten(Service_info), "SERVICE_A") of
	{match, _} -> ct:pal("### RCF notified on BPU SERVICE_A.");
	nomatch -> ct:fail("Failed to find published service.")
    end.

test_reboot_reconnect(_Config) ->
    ct:pal("Test case 9: Verify reconnect after BPU reboot"),
    ok = clean_up(ssh_rcf),
    ok = clean_up(ssh_bpu),
    ok = disconnect_ssh(),
    ok = connect_ssh(),
    ok = setup_itc(rcf, _Config),
    ok = setup_itc(bpu, _Config),
    Rcf = atom_to_list(ct:get_config({test_nodes,1})),
    %% [{ssh, Rcf_ip}, _, _ ,_] =  ct:get_config({list_to_atom(Rcf), ssh_TN_A_ipv6}),
    Rcf_ip = proplists:get_value(fronthaul_ip, _Config),
    ct:pal("## Rcf: ~p. Rcf_ip : ~p ##", [Rcf, Rcf_ip]),

    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
    ct:pal("## Bpu: ~p.  Bpu_ip: ~p ##", [Bpu, Bpu_ip]),

    connect_itc(ssh_bpu, Rcf_ip),
    ok = verify_itc_conn(Bpu_ip),
    ok = disconnect_ssh(),
    ok = reboot_bpu(),
    timer:sleep(15000),
    ok = connect_ssh(),
    ok = wait_for_tn_after_reboot(120000, ssh_bpu),
    ok = setup_itc(bpu, _Config),
    connect_itc(ssh_bpu, Rcf_ip),
    ct:pal("### Sleep 5 seconds so all processes appears"),
    timer:sleep(5000),
    ok = verify_itc_conn(Bpu_ip).


%%% Helper functions below
%% configure_tn() ->
configure_tn(_Config) ->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    %% [{ssh, Rcf_Tn_Ip}, _, {netmask, Rcf_Tn_Mask}, {gateway, Gateway}] = ct:get_config({list_to_atom(Hw), ssh_TN_A_ipv6}),
    Rcf_Tn_Ip =  proplists:get_value(fronthaul_ip, _Config),
    Rcf_Tn_Mask = proplists:get_value(fronthaul_subnet, _Config),
    Gateway = proplists:get_value(fronthaul_gateway, _Config),

    ct:log("Hw : ~p", [Hw]),
    ct:log("Rcf_Tn_Ip : ~p", [Rcf_Tn_Ip]),
    ct:log("Rcf_Tn_Mask : ~p", [Rcf_Tn_Mask]),
    ct:log("Gateway : ~p", [Gateway]),
    ct:pal("### Configure Transport network"),
    rct_cli:send(cli_rcf,"configure"),

    {ok, A} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A"),
    {ok, B} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A,administrativeState=UNLOCKED"),
    {ok, C} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A,index=1"),

    {ok, D} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP"),
    {ok, E} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,InterfaceIPv6=UP"),
    {ok, F} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,InterfaceIPv6=UP,encapsulation=ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A"),
    {ok, G} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,InterfaceIPv6=UP,AddressIPv6=1"),
    {ok, H} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,InterfaceIPv6=UP,AddressIPv6=1,address=" ++ Rcf_Tn_Ip ++ "/" ++ Rcf_Tn_Mask),

    {ok, I} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,RouteTableIPv6Static=1"),
    {ok, J} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,RouteTableIPv6Static=1,Dst=default"),
    {ok, K} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,RouteTableIPv6Static=1,Dst=default,dst=::/0"),
    {ok, L} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,RouteTableIPv6Static=1,Dst=default,NextHop=1"),
    {ok, M} = rct_cli:send(cli_rcf, "ManagedElement=1,Transport=1,Router=UP,RouteTableIPv6Static=1,Dst=default,NextHop=1,address=" ++ Gateway),

    {ok, W} = rct_cli:send(cli_rcf, "validate"),
    rct_cli:send(cli_rcf,"commit"),

    CheckList = [A,B,C,  
		 D,E,F,G,H,
		 I,J,K,L,M, 
		 W],
    lists:foreach(fun(ConfRes) ->
			  case re:run(ConfRes, "ERROR") of
			      {match,_} ->
				  ct:pal("## TN configure failed: ~n~p", [ConfRes]),
				  ct:fail("## TN configure failed: no need to continue!!");
			      nomatch ->
				  ok
			  end
		  end, CheckList),
    ok.


init_mbox(Node) ->
    ct:pal("### Start mbox_1 on ~p", [Node]),
    case Node of
	rcf -> Ssh_handle = ssh_rcf,
	       Itc_tool = ?ITC_TOOL_TO_PATH_X86 ++ "itc" ;
	bpu -> Ssh_handle = ssh_bpu,
	       Itc_tool = ?ITC_TOOL_TO_PATH_ARM ++ "itc"
    end,
    case ct_ssh:exec(Ssh_handle, Itc_tool ++ " init mbox_1 " ++ "& \n", 10000) of
	{ok, []} -> ct:fail("Init mbox failed, reason: ~p", [[]]);
	{ok, Init_reply} -> ct:pal("### Reply from init command: ~p", [Init_reply]);
	{timeout,[]} -> {ok, _} = ct_ssh:exec(Ssh_handle, "\n", 5000);
	Other -> ct:fail("### Unknown error when trying to create mbox on ~p: ~p", [Node, Other])
    end,
    ok.

send_msg(Bpu_ip) ->
    send_msg(0, 1, Bpu_ip).

send_msg(Size, Nr_of_signals, Bpu_ip) ->
    case Nr_of_signals of
	1 -> case Size of
		 0 -> case ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " echo mbox_1 " ++ Bpu_ip ++ "/mbox_1", 30000) of
			  {ok, Msg_test_res} -> {ok, Msg_test_res};
			  Reason -> {error, Reason}
		      end;
		 _ -> case ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " echo -s " ++ Size ++ " mbox_1 " ++ Bpu_ip ++ "/mbox_1", 30000) of
			  {ok, Msg_test_res} -> {ok, Msg_test_res};
			  Reason -> {error, Reason}
		      end
	     end;
	_ -> case ct_ssh:exec(ssh_rcf, ?ITC_TOOL_TO_PATH_X86 ++ "itc" ++ " echo -s " ++ Size ++ " -n " ++ Nr_of_signals ++ " mbox_1 " ++ Bpu_ip ++ "/mbox_1", 30000) of
		 {ok, Msg_test_res} -> {ok, Msg_test_res};
		 Reason -> {error, Reason}
	     end
    end.

verify_send_receive(Input) ->
    case re:run(Input, "(\\d+ us)", [{capture,[1],list}]) of
	{match, [Time]} -> Time;
	nomatch -> ct:fail("Send/receive message failed.")
    end.    

connect_ssh() ->
    ct:pal("### Connecting to RCF"),
    ok = rct_ssh:connect(ssh_rcf),
    ct:pal("### Connecting to BPU"),
    ok = rct_ssh:connect(ssh_bpu),
    ok.

disconnect_ssh() ->
    ok = rct_ssh:disconnect(ssh_rcf),
    ok = rct_ssh:disconnect(ssh_bpu).
    
%% setup_itc(Node) ->
setup_itc(Node, _Config) ->
    ct:pal("# Start setup_itc #"),
    case Node of
	rcf ->     Rcf = atom_to_list(ct:get_config({test_nodes,1})),
		   %% [{ssh, Rcf_ip}, _, _ ,_] =  ct:get_config({list_to_atom(Rcf), ssh_TN_A_ipv6}),
		   Rcf_ip = proplists:get_value(fronthaul_ip, _Config),
		   ct:pal("Rcf: ~p. Rcf_ip : ~p", [Rcf, Rcf_ip]),
		   case ct_ssh:exec(ssh_rcf, ?TN_NAMESPACE ++ " /usr/bin/tcplnh -addr " ++ Rcf_ip ++ "  &", 5000) of
		       {timeout,[]} -> {ok, _} = ct_ssh:exec(ssh_rcf, "\n", 5000);
		       Other1 -> ct:fail("### Unknown error when trying to initialize tcp_link on RCF: ~p", [Other1])
		   end;
	bpu ->     Bpu = atom_to_list(ct:get_config({test_nodes,2})),
		   [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_TN_A_ipv6}),
		   ct:pal("Bpu: ~p. Bpu_ip : ~p", [Bpu, Bpu_ip]),
		   case ct_ssh:exec(ssh_bpu, ?TN_NAMESPACE ++ " /usr/bin/tcplnh -addr " ++ Bpu_ip ++ " &", 5000) of
		       {timeout,[]} -> {ok, _} = ct_ssh:exec(ssh_bpu, "\n", 5000);
		       Other2 -> ct:fail("### Unknown error when trying to initialize tcp_link on BPU: ~p", [Other2])
		   end
    end,
    ct:pal("# End setup_itc #"),
    ok.

connect_itc(FromNode, ToNode) ->
    {ok, _} = ct_ssh:exec(FromNode, ?TN_NAMESPACE ++ " /usr/bin/tcplnh -connect " ++ ToNode ++ " &", 5000).

verify_itc_conn(Bpu_ip) ->
    {ok, Res} = ct_ssh:exec(ssh_rcf, "um list | grep " ++Bpu_ip, 15000),
    FixRes=fix_str(Res, "\n"),
    ct:log("Filtered um list: ~p", [FixRes]),
    ct:pal("### Verify RCF processes seen from BPU."),
    ok = check_processes(Res, Bpu_ip, "\/tcp_lh"),
    ok = check_processes(Res, Bpu_ip, "\/ns_main"),
    ok = check_processes(Res, Bpu_ip, "\/glms"),
    ok = verify_ns(Bpu_ip),
    ok.

verify_ns(Bpu_ip) ->
    {ok, Res} = ct_ssh:exec(ssh_rcf, "ns-info list ns", 15000),
    ct:pal("ns-info: ~p", [Res]),
    ct:pal("### Verify ns-info"),
    ok = check_processes(Res, Bpu_ip, "\/ns_main"),
    ok.

check_processes(Res, Bpu_ip, Proc) ->
    case re:run(lists:flatten(Res), Bpu_ip ++ Proc, []) of
	{match, _} -> ct:pal("RESULT: ~p process found ok!", [Bpu_ip ++ Proc]);
	nomatch -> ct:fail("Hunt for processes failed.")
    end,
    ok.

reboot_bpu() ->
    ct:pal("### Rebooting BPU."),
    ok = rct_rs232:login(console_bpu),
    %% {ok, ErlNode} = rct_rpc:get_erlnode(rpc_bpu),
    %% net_kernel:disconnect(ErlNode),
    ct_telnet:send(console_bpu, "reboot"), 
    {ok,_} = ct_telnet:expect(console_bpu, "Restarting system",
                              [{timeout,180000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console_bpu, "login:", 
                              [{timeout,60000}, no_prompt_check]),
    ok.

wait_for_tn_after_reboot(0, _) ->
    ct:pal("### ERROR: Timeout waiting for TN to come up after reboot!"),
    nok;
wait_for_tn_after_reboot(Timer, Node) ->
    {ok, Namespace_list} = ct_ssh:exec(Node, "ip netns list", 5000),
    case Namespace_list of
	[] -> timer:sleep(1000),
	      ct:pal("### Checking namespace. Timer = ~p ms ~nNamespace not up yet, trying again...", [Timer]),
	      wait_for_tn_after_reboot(Timer - 1000, Node);
	_ -> ok
    end.

clean_up(Node) ->
    {ok, PsRes} = ct_ssh:exec(Node, ?PS_TCPLNH, 5000),
    {match, [Pid]} = re:run(PsRes, "root\\s+(\\d+)", [{capture,[1],list}]),
    ct:pal("### Killing pid ~p", [Pid]),
    {ok, _} = ct_ssh:exec(Node, "kill " ++Pid, 5000),
    ok.

clean(_Config) ->
    ok = clean_up(ssh_rcf),
    ok = clean_up(ssh_bpu),
    ok = disconnect_ssh().

fix_str(Str, FixStr) ->
    string:tokens(Str, FixStr).

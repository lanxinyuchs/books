%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	itc_5g_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R6A/R8A/2
%%% 
%%% @doc == TestSuite for ITC communication between RCF and BPU.==
%%% <br/><br/>
%%% @end

-module(itc_5g_SUITE).
-vsn('/main/R6A/R8A/2').
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
%%% R6A/1      2016-05-17  etxnnor     Created
%%% R6A/2      2016-05-26  etxnnor     Updated
%%% R6A/3      2016-06-20  etxnnor     Added test case for BPU restart and reconnect
%%% R6A/4      2016-06-30  etxnnor     Fix needed due to different output from "um list"
%%% R8A/1      2017-01-13  etxivri     Update for VRCS_64
%%% R8A/2      2017-01-20  etxivri     Update scp to cp correct itc if BRCS is used.
%%% ----------------------------------------------------------
%%% 

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
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
	 test_reboot_reconnect/1]).

%% Soon will only VRCS_64 be supported.
-define(ITC_TOOL_FROM_PATH_X86_64, "/proj/rcs/misc/itc_ns_test/build/x86/itc_64").
-define(ITC_TOOL_FROM_PATH_X86, "/proj/rcs/misc/itc_ns_test/build/x86/itc").
-define(ITC_TOOL_FROM_PATH_ARM, "/proj/rcs/misc/itc_ns_test/build/arm/itc").
-define(ITC_TOOL_TO_PATH_X86, "/usr/bin/").
-define(ITC_TOOL_TO_PATH_ARM, "/home/sirpa/dev_patches/").
-define(PS_TCPLNH, "ps -ef | grep tcplnh | grep -v grep").
-define(NS_INFO, "/bin/ns-info").
-define(INIT_MBOX_CMD, " init mbox_1 ").

suite() -> 
    [{ct_hooks, [{rct_logging,[]},
		 {rct_scp, [{1, scp_rcf}]},
		 {rct_scp, [{2, scp_bpu}]},
		 {rct_rs232,[{2, console_bpu}]},
		 {rct_rpc, [{2, rpc_bpu}]},
		 {rct_ssh,[{1, ssh_rcf, [{ip_type, ssh_lmt_ipv4}]}]},
		 {rct_ssh,[{2, ssh_bpu, [{ip_type, ssh_lmt_ipv4}]}]}]}].

init_per_suite(Config) ->
    %% %% Soon will only VRCS_64 be supported.
    TestType = ct:get_config({jenkins_config, test_type}),
    %% TestType = "GIT_VRCS",
    ct:pal("## TestType : ~p", [TestType] ),
    case TestType of 
	"GIT_VRCS" ->
	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86,
					?ITC_TOOL_TO_PATH_X86, 10);
	"GIT_BRCS" ->
	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86,
					?ITC_TOOL_TO_PATH_X86, 10);
	"GIT_VRCS64" -> 
	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86_64,
					?ITC_TOOL_TO_PATH_X86 ++ "itc", 10); %% cp itc_64 to itc then this SUITE not need more updates.
	_Other -> 
	    {ok,_} =  rct_scp:to_target(scp_rcf, ?ITC_TOOL_FROM_PATH_X86_64,
					?ITC_TOOL_TO_PATH_X86 ++ "itc", 10)  %% cp itc_64 to itc then this SUITE not need more updates.
    end,
    Ls_Itc = ct_ssh:exec(ssh_rcf, "ls -l "++?ITC_TOOL_TO_PATH_X86++"itc", 30000),
    ct:pal("ls -l on itc : ~p ", [Ls_Itc]),
    {ok,_} =  rct_scp:to_target(scp_bpu, ?ITC_TOOL_FROM_PATH_ARM,
    				?ITC_TOOL_TO_PATH_ARM, 10),
    Config.
end_per_suite(_Config) ->
    ok = clean_up(ssh_rcf),
    ok = clean_up(ssh_bpu),
    ok = disconnect_ssh().

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> 
    [test_setup_itc,
     test_locate_bpf,
     test_basic_itc_msg,
     test_large_itc_msg,
     test_multiple_itc_msg,
     test_multiple_large_itc_msg,
     test_monitor,
     test_subscribe_publish,
     test_reboot_reconnect].

test_setup_itc(_Config) ->
    ct:pal("Test case 1: Open TCP port on RCF & BPU, connect RCF -> BPU. Tool used is tcplnh."),
    ok = connect_ssh(),
    ok = setup_itc(rcf),
    ok = setup_itc(bpu),
    Rcf = atom_to_list(ct:get_config({test_nodes,1})),
    [{ssh, Rcf_ip}, _, _ ,_] =  ct:get_config({list_to_atom(Rcf), ssh_lmt_ipv4}),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
    connect_itc(ssh_rcf, Bpu_ip),
    ok = verify_itc_conn(Rcf_ip).

test_locate_bpf(_Config) ->
    ct:pal("Test case 2: Init mbox_1 on both RCF and BPU, locate mbox_1 on BPU from RCF. Tool used is itc test app."),
    Bpu = atom_to_list(ct:get_config({test_nodes,2})),
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
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
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
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
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
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
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
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
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
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
    [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
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
    ok = setup_itc(rcf),
    ok = setup_itc(bpu),
    Rcf = atom_to_list(ct:get_config({test_nodes,1})),
    [{ssh, Rcf_ip}, _, _ ,_] =  ct:get_config({list_to_atom(Rcf), ssh_lmt_ipv4}),
    connect_itc(ssh_bpu, Rcf_ip),
    ok = verify_itc_conn(Rcf_ip),
    ok = disconnect_ssh(),
    ok = reboot_bpu(),
    timer:sleep(15000),
    ok = connect_ssh(),
    ok = setup_itc(bpu),
    connect_itc(ssh_bpu, Rcf_ip),
    ok = verify_itc_conn(Rcf_ip).


%%% Helper functions below
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
    
setup_itc(Node) ->
    case Node of
	rcf ->     Rcf = atom_to_list(ct:get_config({test_nodes,1})),
		   [{ssh, Rcf_ip}, _, _ ,_] =  ct:get_config({list_to_atom(Rcf), ssh_lmt_ipv4}),
		   case ct_ssh:exec(ssh_rcf, "/usr/bin/tcplnh -addr " ++ Rcf_ip ++ "  &", 5000) of
		       {timeout,[]} -> {ok, _} = ct_ssh:exec(ssh_rcf, "\n", 5000);
		       Other1 -> ct:fail("### Unknown error when trying to initialize tcp_link on RCF: ~p", [Other1])
		   end;
	bpu ->     Bpu = atom_to_list(ct:get_config({test_nodes,2})),
		   [{ssh, Bpu_ip}, _, _, _] =  ct:get_config({list_to_atom(Bpu), ssh_lmt_ipv4}),
		   case ct_ssh:exec(ssh_bpu, "/usr/bin/tcplnh -addr " ++ Bpu_ip ++ " &", 5000) of
		       {timeout,[]} -> {ok, _} = ct_ssh:exec(ssh_bpu, "\n", 5000);
		       Other2 -> ct:fail("### Unknown error when trying to initialize tcp_link on BPU: ~p", [Other2])
		   end
    end,

    ok.

connect_itc(FromNode, ToNode) ->
    {ok, _} = ct_ssh:exec(FromNode, "/usr/bin/tcplnh -connect " ++ ToNode ++ " &", 5000).

verify_itc_conn(Rcf_ip) ->
    {ok, Res} = ct_ssh:exec(ssh_bpu, "um list | grep " ++Rcf_ip, 15000),
    ct:pal("Filtered um list: ~p", [Res]),
    ct:pal("### Verify RCF processes seen from BPU."),
    ok = check_processes(Res, Rcf_ip, "\/tcp_lh"),
    ok = check_processes(Res, Rcf_ip, "\/ns_main"),
    ok = check_processes(Res, Rcf_ip, "\/glms"),
    ok.

check_processes(Res, Rcf_ip, Proc) ->
    case re:run(lists:flatten(Res), Rcf_ip ++ Proc, []) of
	{match, _} -> ct:pal("RESULT: ~p process found ok!", [Rcf_ip ++ Proc]);
	nomatch -> ct:fail("Hunt for RCF processes failed.")
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

clean_up(Node) ->
    {ok, PsRes} = ct_ssh:exec(Node, ?PS_TCPLNH, 5000),
    {match, [Pid]} = re:run(PsRes, "root\\s+(\\d+)", [{capture,[1],list}]),
    ct:pal("### Killing pid ~p", [Pid]),
    {ok, _} = ct_ssh:exec(Node, "kill " ++Pid, 5000),
    ok.


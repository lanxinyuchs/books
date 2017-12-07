%% coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	vnfm_enrollment_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R12A/5
-module(vnfm_enrollment_SUITE).
-id('Updated by CCase').
-vsn('/main/R12A/5').
-date('2017-11-15'). 
-author('etxivri').
%%% ----------------------------------------------------------
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
%%% R12A/1      2017-03-07 etxivri     Created
%%% R12A/4      2017-03-14 etxivri     Update check port 443 to use rct_vnfm lib
%%% R12A/5      2017-11-15 etxivri     -Set vnfm to down in hook due to soon rpc will not work 
%%%                                    on vnfm and the logging will fail.
%%%                                    -Moved some good stuff to rct_vnfm hook.
%%%                                    -Some cleanup.
%%%
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
 	 
	 wait_for_cli_is_up/1,
 	 instantiate_sitebasic_lkf/1,
   	 enrollment_needed_cert/1,
   	 check_port443/1,

	 get_vnfs_from_old_port/1,
   	 break/1
    	]).

%% @hidden
suite() -> 
    TestNodes = ct:get_config(test_nodes),
    ct:pal("TestNodes: ~p",[TestNodes]),
    NrOfTestnodes = length(TestNodes),
    ct:pal("NrOfTestnodes: ~p",[NrOfTestnodes]),
    
    %%%%%%%%%%%%
    %% fix hooks needed for nr of testnodes
    %%%%%%%%%%%%
    
    %% rct_node_state
    VnfNodeState = [{N,vnf,down} || N <- lists:seq(2, NrOfTestnodes)] ,
    ct:log("VnfNodeState: ~p",[VnfNodeState]),
    RctNodeStateHook = 
     	{rct_node_state, [{1,vnfm,down}]++VnfNodeState},
    ct:log("RctNodeState: ~p",[RctNodeStateHook]),

    [{ct_hooks,[{rct_htmllink,[]},
		RctNodeStateHook,
   		%% {rct_node_state, [{1,vnfm,up}]},
   		{rct_logging, [
  			       {1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []}
    			      ]},
    		{rct_cli, [{1, cli1ipv4, ssh_lmt_ipv4, [manual_connect]},
    			   {1, cli1ipv6, ssh_lmt_ipv6, [manual_connect]}
    			  ]},
   		{rct_coli,[{1, coli1ipv4, ssh_lmt_ipv4, [manual_connect]},
   			   {1, coli1ipv6, ssh_lmt_ipv6, [manual_connect]}
   			  ]},
    		{rct_netconf,[{1, nc1ipv4, ssh_lmt_ipv4},
   			      {1, nc2ipv6, ssh_lmt_ipv6}
   			     ]},
    		{rct_rpc,[rpc1
    			 ]},
    		{rct_http,[http1
    			  ]},
    		{rct_vnfm,[vnfm1]},
    		{cth_conn_log, []},
    		{rct_core,[[]
    			  ]}
	       ]}].
   

%% @hidden
init_per_suite(Config) -> 
    [{1,TestNode}|_T] = ct:get_config(test_nodes),
    ct:pal("TestNode: ~p",[TestNode]),
    [{testNode, TestNode} | Config].

%% @hidden
end_per_suite(_Config) -> ok.
%% @hidden
init_per_testcase(_TestCase, Config) -> Config.
%% @hidden
end_per_testcase(_TestCase, _Config) -> ok.
%% @hidden
all() -> [
	  wait_for_cli_is_up,
	  instantiate_sitebasic_lkf,
	  enrollment_needed_cert,
	  check_port443
	 
	 ].


break(_Config) ->
    test_server:break("AA").


get_vnfs_from_old_port(_Config) ->
    A=rct_vnfm:get_vnfs(vnfm1),
    ct:pal("# A: ~p",[A]).


%%%--------------------------------------------------------------------
%%% @doc
%%%   instantiate_sitebasic_lkf from an json file
%%% @end
%%%--------------------------------------------------------------------
wait_for_cli_is_up(_Config) ->
    CliCmd = "show ManagedElement=1,NodeSupport=1,AutoProvisioning=1",
    MatchStr = "AutoProvisioning",
    ok = rct_vnfm:wait_for_cli(cli1ipv4, 180000, CliCmd, MatchStr).



%%%--------------------------------------------------------------------
%%% @doc
%%%   instantiate_sitebasic_lkf from an json file
%%% @end
%%%--------------------------------------------------------------------
instantiate_sitebasic_lkf(Config) ->
    TestNode = proplists:get_value(testNode, Config),
    ct:pal("# TestNode: ~p",[TestNode]),
    [{ssh, SshLmtIp}, _, _ ,_] =  ct:get_config({TestNode, ssh_lmt_ipv4}),
    %% [{ssh, SshLmtIp}, _, _ ,_] =  ct:get_config({TestNode, ssh_lmt_ipv6}),
    ct:pal("# SshLmtIp: ~p",[SshLmtIp]),
    
    %% Get jobs
    PreGetJobs = get_jobs(SshLmtIp),
    ct:pal("# Pre Get jobs : ~p",[PreGetJobs]),
    sleep5(),
    
    %% Create new job
    Create1 = create_job(SshLmtIp),
    ct:pal("# Create new Job: ~p",[Create1]),
    sleep5(),
    
    %% Instantiate sitebasic and lkf
    Instantiate = 
   	"curl -k -i --noproxy '*' -g -X PUT -H \"Content-Type:application/json\" -H \"Expect:\" https://"++SshLmtIp++":4000/ranvnfm/eri-pkimgmt-v1/jobs/1/instantiate -d @/proj/rcs-tmp/etxivri/vnfm_json_files/rcs_vnfm_nodeConfig.json",
    B = send_os_cmd(Instantiate),
    ct:pal("# Instantiate: ~p",[B]),
    %% test_server:break("C"),
    ct:pal("Sleep 60 sec to ensure instantiate is done.~nIt will fails if someone is logged in using cli."),
    timer:sleep(60000),
    case wait_for_site_config_complete() of
   	ok -> 
   	    ok;
   	nok -> 
   	    ct:pal("# SITE_CONFIG_COMPLETE not exist. Try the instantiate once more"), 
   	    send_os_cmd(Instantiate)
    end,
    wait_for_site_config_complete(),
    sleep5(),
    ok = check_testlicense_exist_via_cli(),
    
    %% Get jobs
    AfterGetJobs = get_jobs(SshLmtIp),
    ct:pal("# After Get jobs: ~p",[AfterGetJobs]),
    sleep5(),
    
    %% Clean up
    ct:pal("# Delete Job Id 1"),
    delete_job_id(SshLmtIp, "1"),
    get_jobs(SshLmtIp),
    sleep5(),
   
    ok.
   
   
%%%--------------------------------------------------------------------
%%% @doc
%%%   enrollment_needed_cert
%%%   This is needed to get pot 443 open
%%% @end
%%%--------------------------------------------------------------------
enrollment_needed_cert(Config) ->
    TestNode = proplists:get_value(testNode, Config),
    ct:pal("# TestNode: ~p",[TestNode]),
    [{ssh, SshLmtIp}, _, _ ,_] =  ct:get_config({TestNode, ssh_lmt_ipv4}),
    %% [{ssh, SshLmtIp}, _, _ ,_] =  ct:get_config({TestNode, ssh_lmt_ipv6}),
    ct:pal("# SshLmtIp: ~p",[SshLmtIp]),
    
    %% Get jobs
    PreGetJobs = get_jobs(SshLmtIp),
    ct:pal("# Pre Get jobs : ~p",[PreGetJobs]),
    sleep5(),
    
    %% Create new job
    Create1 = create_job(SshLmtIp),
    ct:pal("# Create new Job: ~p",[Create1]),
    sleep5(),
    
    %%%%%%%
    %% Enroll Oam, This will open port 443 on oam net
    %%%%%%%
    Enroll_Oam = 
	"curl --noproxy '*' -k -i -X PUT -H \"Content-Type:application/json\" -H \"Expect:\" https://"++SshLmtIp++":4000/ranvnfm/eri-pkimgmt-v1/jobs/1/enroll -d @/proj/rcs-tmp/etxivri/vnfm_json_files/enroll_oam_p12.json",
    B = send_os_cmd(Enroll_Oam),
    ct:pal("# Enroll_Oam: ~p",[B]),
    %% test_server:break("C"),
    timer:sleep(30000),
    
    %% %%%%%%%
    %% %%% här går det att komma åt port 443
    %% %%%%%%%
    
    %% %% Create new job
    %% Create2 = create_job(SshLmtIp),
    %% ct:pal("# Create new: ~p",[Create2]),
    %% sleep5(),
    %% GetJobs = get_jobs(SshLmtIp),
    %% ct:pal("# Get jobs : ~p",[GetJobs]),
    %% sleep5(),
   
    %% %%%%%%%
    %% %% Enroll Infra, This will open port 4001 on Infra net
    %% %% After this is enrolled then port 4000 on infra is closed.
    %% %% We don't need this Infra net for our test.
    %% %%%%%%%
    %% Enroll_Infra = 
    %% 	"curl --noproxy '*' -k -i -X PUT -H \"Content-Type:application/json\" -H \"Expect:\" https://"++SshLmtIp++":4000/ranvnfm/eri-pkimgmt-v1/jobs/2/enroll -d @/proj/rcs-tmp/etxivri/vnfm_json_files/enroll_infra_oauth_p12.json",
    %% C = send_os_cmd(Enroll_Infra),
    %% ct:pal("# Enroll_Infra: ~p",[C]),
    %% %% test_server:break("E"),
    %% timer:sleep(30000),
    
    %% %% Get jobs
    %% AfterGetJobs = get_jobs(SshLmtIp),
    %% ct:pal("#After Get jobs: ~p",[AfterGetJobs]),
    %% sleep5(),

    ok.

   
%%%--------------------------------------------------------------------
%%% @doc
%%%   check_port443
%%% @end
%%%--------------------------------------------------------------------
check_port443(_Config) ->
    {ok, A} = rct_vnfm:get_vnfs_443(vnfm1),
    ct:pal("# A: ~p",[A]).

    %% TestNode = proplists:get_value(testNode, Config),
    %% ct:pal("# TestNode: ~p",[TestNode]),
    %% [{ssh, SshLmtIp}, _, _ ,_] =  ct:get_config({TestNode, ssh_lmt_ipv4}),
    %% %% [{ssh, SshLmtIp}, _, _ ,_] =  ct:get_config({TestNode, ssh_lmt_ipv6}),
    %% ct:pal("# SshLmtIp: ~p",[SshLmtIp]),
    
    %% %% %% This cmd will not work now.
    %% %% curl -v http://10.209.49.14:443/lcm/v0/vnf_instances
    %% CheckCmd = "curl --noproxy '*' -k -i -X GET -H \"Content-Type:application/json\" https://"++SshLmtIp++":443/lcm/v0/vnf_instances",
    

    
    %% %% CheckCmd = "curl --noproxy '*' -i -X GET https://"++SshLmtIp++":443/openmano/tenants -H 'Accept: application/json' --insecure",
    %% Check = send_os_cmd(CheckCmd),
    %% ct:pal("# Check port 443 : ~n~p", [Check]),
    
    %% case re:run(Check, "HTTP/1.1 200 OK") of
    %% 	{match,_} -> 
    %% 	    ct:pal("rcvd, 200 OK from port 443"),
    %% 	    ok;
    %% 	nomatch -> 
    %% 	    ct:fail("Check port 443 failed due to 200 OK not rcvd !!!")
    %% end,
    %% ok.


%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
get_jobs(Ip) ->
    ct:log("Get Jobs"),
    GetJobs = "curl --noproxy '*' -k -i -X GET -H \"Content-Type:application/json\" https://"++Ip++":4000/ranvnfm/eri-pkimgmt-v1/jobs",
    Answ = send_os_cmd(GetJobs),
    ct:log("# Answ from  get jobs: ~n~p",[Answ]),
    Answ.


create_job(Ip) ->
    ct:log("Create Job"),
    CreateJob = "curl -k -i --noproxy '*' -g -X POST -H \"Content-Type:application/json\" https://"++Ip++":4000/ranvnfm/eri-pkimgmt-v1/jobs",
    Answ = send_os_cmd(CreateJob),
    Reply = case re:run(Answ, "201 Created") of
		{match, _} ->
		    ct:log("# Job is created"),
		    Answ;
		nomatch ->
		    ct:log("# Job was not created, sleep and try again!"),
		    timer:sleep(30000),
		    NewAnsw = create_job(Ip),
		    NewAnsw
	    end,
    ct:log("# Answ from create job: ~n~p",[Reply]),
    Answ.
   

delete_job_id(Ip, Job_Id) ->
    ct:log("Delete Job_id: ~p", [Job_Id]),
    DeleteJobId = "curl -k -i --noproxy '*' -g -X DELETE -H \"Content-Type:application/json\" https://"++Ip++":4000/ranvnfm/eri-pkimgmt-v1/jobs/"++Job_Id,
    Answ = send_os_cmd(DeleteJobId),
    ct:log("# Answ from delete job_id: ~n~p",[Answ]),
    Answ.

   
send_os_cmd(Cmd) ->
    ct:log("Send os cmd : ~n~p", [Cmd]),
    Answ = os:cmd(Cmd),
    ct:log("Answ: ~p",[Answ]),
    Answ.


wait_for_site_config_complete() ->
    CliCmd = "show ManagedElement=1,NodeSupport=1,AutoProvisioning=1",
    MatchStr = "SITE_CONFIG_COMPLETE",
    ok =  rct_vnfm:wait_for_cli(cli1ipv4, 180000, CliCmd, MatchStr).


sleep5() ->
    ct:log("sleep 5 sec"),
    timer:sleep(5000).

check_testlicense_exist_via_cli() ->
    CliCmd1 = "show ManagedElement=1,SystemFunctions=1,Lm=1",
    MatchStr1 = "CXC4012117",
    ok =  rct_vnfm:wait_for_cli(cli1ipv4, 60000, CliCmd1, MatchStr1),
    
    CliCmd2 = "show ManagedElement=1,SystemFunctions=1,Lm=1,FeatureState="++MatchStr1,
    MatchStr2 = "licenseState=ENABLED",
    ok =  rct_vnfm:wait_for_cli(cli1ipv4, 60000, CliCmd2, MatchStr2).



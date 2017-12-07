%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	create_vnf_from_zip_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R11A/R12A/4
-module(create_vnf_from_zip_SUITE).
-id('Updated by CCase').
-vsn('/main/R9A/R10A/R11A/R12A/4').
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
%%% R9A/1      2017-03-07 etxivri     Created
%%% R9A/2      2017-03-07 etxivri     make it more robust.
%%% R9A/3      2017-03-29 etxivri     More update when create vnf.
%%%                                   Add a check to be used after create vnf.
%%% R9A/4      2017-03-29 etxivri     Update to handle only one vnf can be created,
%%%                                   due to hook coordination.
%%%                                   Right now this vnf shall be nr 2 in stp config files.
%%% R9A/5      2017-03-31 etxivri     Update to look for instance name in jenkins config.
%%% R9A/6      2017-03-31 etxivri     Update search str in jenkins config for Href.
%%% R9A/7      2017-03-31 etxivri     Update when from up shall be used in upgrade.
%%% R9A/8      2017-04-04 etxivri     Update create_from_vnf_for_ug to use new
%%%                                   jenkins config.
%%% R9A/9      2017-04-05 etxivri     Update in delete vnf.
%%% R9A/10     2017-04-11 etxivri     Increase timeout.
%%% R10A/1     2017-05-02 etxivri     Update due to behaviour in create datacenter is changed 
%%%                                   with new vnfm.
%%% R10A/2     2017-05-03 etxivri     Update due to new behaviour when create datacenter.
%%% R10A/3     2017-05-18 etxivri     Update for vPP and vSD
%%% R10A/4     2017-05-22 etxivri     Update due to changed behaviour.
%%%                                   Update HORROR filter  due to error printout on vPP.
%%% R10A/5     2017-05-29 etxivri     Update error filter.
%%% R10A/6     2017-06-01 etxivri     Minor bugfix.
%%% R11A/2     2017-09-13 etxivri     Add check for cli ipv6 to check for node is up.
%%% R11A/3     2017-09-27 etxivri     Update for vPP2
%%% R11A/4     2017-09-27 etxivri     Minor update for vPP2
%%% R12A/3     2017-11-06 etxivri     -Update hooks.
%%%                                   -Update for second vnf.
%%%                                   -Add get_datacenter_name.
%%%                                   -Update instatiate due to new behaviour.
%%%                                   -Update delete_vnf_zip due to new behaviour.
%%% R12A/4     2017-11-15 etxivri     -Set vnfm to down in hook due to soon rpc will not work 
%%%                                    on vnfm and the logging will fail.
%%%                                   -Add instantiate with sitebasic and lkf.
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
	 
	 %% Create vnf from zip pkg
	 create_datacenter/1,
	 create_vnf_using_zip/1, %% create first VNF
	 create_vnf_using_zip_ai/1, %% create first VNF
	 check_cli_coli_nc/1,
	 create_second_vnf_using_zip/1, %% create second VNF
	 check_cli_coli_nc_after_vnf_2/1,

	 create_from_vnf_for_ug/1, %% Create a from vnf used in upgrade  
	 create_from_vnf_for_ug_ai/1, 

	 delete_vnf_zip/1,
	 delete_datacenter/1,
	 get_datacenter_name/1,
	 get_vnf_packages/1,
	 create_second_vnf_using_existing_zip/1,
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


    %% rct_logging
    RctLogging = [{N, list_to_atom("log"++integer_to_list(N)), all, 
		   [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []}  || N <- lists:seq(1, NrOfTestnodes)],
    ct:log("RctLogging: ~p",[RctLogging]),
    RctLoggingHook = {rct_logging, RctLogging},
    ct:log("RctLoggingHook: ~p",[RctLoggingHook]),

    %% rct_cli
    CliIPv4 = [{N,list_to_atom("cli"++integer_to_list(N)++"ipv4"),ssh_lmt_ipv4, [manual_connect]}
	       || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("CliIPv4: ~p",[CliIPv4]),
    CliIPv6=[{N,list_to_atom("cli"++integer_to_list(N)++"ipv6"),ssh_lmt_ipv6, [manual_connect]} 
	     || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("CliIPv6: ~p",[CliIPv6]),
    CliHooks = {rct_cli,CliIPv4++CliIPv6},
    ct:log("CliHooks: ~p",[CliHooks]),

    %% rct_coli
    ColiIPv4 = [{N,list_to_atom("coli"++integer_to_list(N)++"ipv4"),ssh_lmt_ipv4, [manual_connect]}
	       || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("ColiIPv4: ~p",[ColiIPv4]),
    ColiIPv6=[{N,list_to_atom("coli"++integer_to_list(N)++"ipv6"),ssh_lmt_ipv6, [manual_connect]} 
	     || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("ColiIPv6: ~p",[ColiIPv6]),
    ColiHooks = {rct_coli,ColiIPv4++ColiIPv6},
    ct:log("ColiHooks: ~p",[ColiHooks]),

    %% rct_netconf
    NcIPv4 = [{N,list_to_atom("nc"++integer_to_list(N)++"ipv4"), ssh_lmt_ipv4} || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("NcIPv4: ~p",[NcIPv4]),
    NcIPv6 = [{N,list_to_atom("nc"++integer_to_list(N)++"ipv6"), ssh_lmt_ipv6} || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("NcIPv6: ~p",[NcIPv6]),
    NetconfHooks = {rct_netconf,NcIPv4++NcIPv6},
    ct:log("NetconfHooks: ~p",[NetconfHooks]),

    %% rct_rpc
    Rpc = [list_to_atom("rpc"++integer_to_list(N)) || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("Rpc: ~p",[Rpc]),
    RpcHooks = {rct_rpc,Rpc},
    ct:log("RpcHooks: ~p",[RpcHooks]),

    %% rct_http
    Http = [list_to_atom("http"++integer_to_list(N)) || N <- lists:seq(1,NrOfTestnodes)],
    ct:log("Http: ~p",[Http]),
    HttpHooks = {rct_http,Http},
    ct:log("HttpHooks: ~p",[HttpHooks]),

    [{ct_hooks,[{rct_htmllink,[]},
		RctNodeStateHook,		
		RctLoggingHook,
		CliHooks,
		ColiHooks,
		NetconfHooks,
		RpcHooks,
		HttpHooks,
		{rct_vnfm,[vnfm1]},
		{cth_conn_log, []},
		{rct_core,[[],
			   []
			   %% []
			  ]}
		]}].

%% @hidden
init_per_suite(Config) -> Config.
%% @hidden
end_per_suite(_Config) -> ok.
%% @hidden
init_per_testcase(_TestCase, Config) -> Config.
%% @hidden
end_per_testcase(_TestCase, _Config) -> ok.
%% @hidden
all() -> [].


break(_Config) ->
    test_server:break("AA").


%%--------------------------------------------------------------------
%% @doc
%% Create Datacenter, using REST API
%% @end
%%--------------------------------------------------------------------
create_datacenter(_Config) ->
    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = 
	rct_vnfm:get_tenants(vnfm1),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("INSERT DATACENTER"),
    %% {ok, #{<<"datacenter">> := #{<<"uuid">> := Datacenter_uuid}}} = 
    %% 	rct_vnfm:insert_datacenter(vnfm1,"rcs","openstack"),
    %% ct:pal("datacenter uuid ~p",[Datacenter_uuid]),

    %% {ok, Answer} = 
    {ok, #{<<"datacenter">> := Answer}} =
	rct_vnfm:insert_datacenter(vnfm1,"rcs", "openstack"),
    ct:log("## Answer : ~p",[Answer]),
    {ok, Datacenter_uuid} = maps:find(<<"uuid">>, Answer),
    ct:pal("Datacenter_uuid: ~p",[Datacenter_uuid]),

    ct:pal("ASSOCIATE DATACENTER"),
    {ok, _} = rct_vnfm:associate_datacenter(vnfm1,Datacenter_uuid),

    ct:pal("GET DATACENTERS"),
    {ok, A} = rct_vnfm:get_datacenters(vnfm1,Tenant_uuid),
    ct :pal("Get DataCenters : ~p ", [A]),
    ct :pal("uuid : ~p ", [Tenant_uuid]),
    ct:pal("datacenter uuid ~p",[Datacenter_uuid]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Get Datacenter, using REST API
%% @end
%%--------------------------------------------------------------------
get_datacenter_name(_Config) ->
    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = 
	rct_vnfm:get_tenants(vnfm1),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("GET DATACENTERS"),
    {ok, A} = rct_vnfm:get_datacenters(vnfm1,Tenant_uuid),
    ct :pal("# DataCenters : ~p ", [A]),
    
    [B] = maps:get(<<"datacenters">>, A),
    ct :pal("# datacenter data : ~p ", [B]),

    C = maps:get(<<"name">>, B),
    ct :pal("# datacenter name : ~p ", [C]),
    C.

%%--------------------------------------------------------------------
%% @doc
%% Delete VNF when VNF has been created from zip pkg, using VNFM REST API
%% @end
%%--------------------------------------------------------------------
delete_datacenter(_Config) ->
    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = rct_vnfm:get_tenants(vnfm1),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("GET DATACENTERS"),
    {ok, #{<<"datacenters">> := [#{<<"uuid">> := Datacenter_uuid}]}} = rct_vnfm:get_datacenters(vnfm1,Tenant_uuid),
    
    ct:pal("DELETE DATACENTER"),
    {ok, _} = rct_vnfm:delete_datacenter(vnfm1,Datacenter_uuid).



%%%--------------------------------------------------------------------
%%% @doc
%%%   Create first VNF using a zip from url, using REST API 
%%%   Note! Href must be an url to zip
%%%
%%%   Input argument when run test.       
%%%   urlzip   - url to the zip file.
%%%   instname - name of instance
%%% exampel how to run test:
%%%   -suite create_vnf_from_zip_SUITE -case create_vnf_x -urlzip url -instname etxivri_vnf1
%%% @end
%%%--------------------------------------------------------------------
create_vnf_using_zip(Config) -> %% This will create first VNF
    VnfType = get_vnf_type(), 
    ct:pal("## VnfType : ~p", [VnfType]),
    TestNodeNr = 2,
    create_vnf_using_zip(Config, VnfType, TestNodeNr),
    rct_node_state:set_state_up(TestNodeNr).

create_vnf_using_zip_ai(Config) -> %% This will create first VNF
    VnfType = get_vnf_type(), 
    ct:pal("## VnfType : ~p", [VnfType]),
    TestNodeNr = 2,
    create_vnf_using_zip(Config, VnfType, TestNodeNr, ai),
    rct_node_state:set_state_up(TestNodeNr).

create_second_vnf_using_zip(Config) ->
    VnfType = get_vnf_type(), 
    ct:pal("## VnfType : ~p", [VnfType]),
    TestNodeNr = 3,
    create_vnf_using_zip(Config, VnfType, TestNodeNr),
    rct_node_state:set_state_up(2), %% get logs from first VNF
    rct_node_state:set_state_up(TestNodeNr).

create_second_vnf_using_existing_zip(Config) ->
    VnfType = get_vnf_type(), 
    ct:pal("## VnfType : ~p", [VnfType]),
    TestNodeNr = 3,
    VnfInstName = get_vnf_instance_name(TestNodeNr),
    create_vnf_using_existing_zip(Config, vnfm1, VnfInstName, TestNodeNr),
    rct_node_state:set_state_up(2), %% get logs from first VNF
    rct_node_state:set_state_up(TestNodeNr).

create_vnf_using_zip(Config, VnfType, TestNodeNr) ->
    create_vnf_using_zip(Config, VnfType, TestNodeNr, no_ai).
create_vnf_using_zip(Config, VnfType, TestNodeNr, AIflag) ->
    ct:pal("### Create VNF , that will be testnode nr : ~p ###", [TestNodeNr]),
    ct:pal("# VnfType: ~p ", [VnfType]),
    Container = case VnfType of 
		    vnf -> %% Old TC could use this when create vRC
			container_vnf_2;
		    vrc ->
			container_vrc_2;
		    vpp ->
			container_vpp_2;
		    vpp2 ->
			container_vpp2_2;
		    vsd ->
			container_vsd_2;
		    undefined ->
			undef
		end,
    
    %% Href = case ct:get_config({jenkins_config, container_vnf_2}) of
    Href = case ct:get_config({jenkins_config, Container}) of
	       undefined ->
		   get_ct_arg(urlzip);
	       HREF ->
		   HREF
	   end,	          
    ct:pal("## Href : ~p", [Href]),

    %% TestNodeNr = 2,
    VnfInstName = get_vnf_instance_name(TestNodeNr),
    %% create_vnf(Config, vnfm1, TestNodeNr, Href, VnfInstName).
    create_vnf_x(Config, vnfm1, TestNodeNr, Href, VnfInstName, AIflag).

create_vnf_x(Config, VNFM, TestNodeNr, Href, VnfInstName, AIflag) ->
    ct:pal("TestNodeNr: ~p",[TestNodeNr]),
    create_vnf_using_zip(Config, VNFM, TestNodeNr, Href, VnfInstName, AIflag),
    Rpc=list_to_atom("rpc"++integer_to_list(TestNodeNr)),
    IfConfigList = rct_rpc:xcall(Rpc, os, cmd,["ifconfig"],10000),
    IfConfig = fix_str(IfConfigList),
    ct:log("IfConfig: ~n~p", [IfConfig]),
    timer:sleep(30000),
    Cli_IPv4 = list_to_atom("cli"++integer_to_list(TestNodeNr)++"ipv4"),
    Cli_IPv6 = list_to_atom("cli"++integer_to_list(TestNodeNr)++"ipv6"),
    ok = wait_for_cli(Cli_IPv4, 600000),
    ok = wait_for_cli(Cli_IPv6, 600000),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%%   Create a from VNF that shall be used in a upgrade. 
%%%   Shall be used in upgrade tests in jenkins.
%%% @end
%%%--------------------------------------------------------------------
create_from_vnf_for_ug(Config) ->
    VnfType = get_vnf_type(), 
    ct:pal("## VnfType : ~p", [VnfType]),
    create_from_vnf_for_ug(Config, VnfType, no_ai).

create_from_vnf_for_ug_ai(Config) ->
    VnfType = get_vnf_type(), 
    ct:pal("## VnfType : ~p", [VnfType]),
    create_from_vnf_for_ug(Config, VnfType, ai).

create_from_vnf_for_ug(Config, VnfType, AIflag) ->
    %% Href = case ct:get_config({jenkins_config, upgrade_from_container}) of
    %% Note! container_upgrade_vnf_2 in jenkins config shall be used as from UP.
    ct:pal("## VnfType: ~p ", [VnfType]),
    FromContainer = case VnfType of 
			vrc ->
			    container_upgrade_vrc_1;
			vpp ->
			    container_upgrade_vpp_1;
			vpp2 ->
			    container_upgrade_vpp2_1;
			vsd ->
			    container_upgrade_vsd_1;
			undefined ->
			    undef
		    end,
    %% Href = case ct:get_config({jenkins_config, container_upgrade_vnf_2}) of
    Href = case ct:get_config({jenkins_config, FromContainer}) of
	       undefined ->
		   get_ct_arg(urlzip);
	       HREF ->
		   HREF
	   end,	          
    ct:pal("## Href : ~p", [Href]),
    TestNodeNr = 2,
    VnfInstName = get_vnf_instance_name(TestNodeNr),
    create_from_vnf(Config, vnfm1, TestNodeNr, Href, VnfInstName, AIflag).


get_vnf_instance_name(TestNodeNr) ->
    VnfInstName = case ct:get_config({jenkins_config, vnf_instance_name}) of
		      undefined ->
			  case get_ct_arg(instname) of 
			      undefined ->
				  atom_to_list(ct:get_config({test_nodes, TestNodeNr}));
			      InstName ->
				  ct:pal("InstName: ~p", [InstName]),
				  InstName
			  end;
		      Inst_Name ->
			  Inst_Name
		  end,
    ct:pal("VnfInstName: ~p", [VnfInstName]),
    VnfInstName.

%% create_from_vnf(Config, VNFM, TestNodeNr, Href, VnfInstName) ->
create_from_vnf(Config, VNFM, TestNodeNr, Href, VnfInstName, AIflag) ->
    create_vnf_using_zip(Config, VNFM, TestNodeNr, Href, VnfInstName, AIflag),
    IfConfigList = rct_rpc:xcall(rpc2, os, cmd,["ifconfig"],10000),
    IfConfig = fix_str(IfConfigList),
    ct:log("IfConfig: ~n~p", [IfConfig]),
    timer:sleep(30000),
    ok = wait_for_cli(cli2ipv4, 600000),
    ok = wait_for_cli(cli2ipv6, 600000),
    rct_node_state:set_state_up(TestNodeNr),
    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%%  check_cli_coli_nc <br/>
%%% @end
%%%--------------------------------------------------------------------
check_cli_coli_nc(_Config) ->
    check_ipv4(),
    check_ipv6().

check_ipv4() ->
    ct:pal("################# Check CLI, COLI, NETCONF IPv4 on first vnf ##"),
    ct:pal("## Check cli using ipv4 ##"),
    ok = wait_for_cli(cli2ipv4, 300000),
    ct:pal("## Check coli using ipv4 ##"),
    ok = wait_for_coli(coli2ipv4, 300000),
    ct:pal("## Check Netconf using ipv4 ##"),
    ok = wait_for_netconf(nc2ipv4, 300000).

check_ipv6() ->
    ct:pal("################# Check CLI, COLI, NETCONF IPv6 on first vnf ##"),
    ct:pal("## Check cli using ipv6 ##"),
    ok = wait_for_cli(cli2ipv6, 300000),
    ct:pal("## Check coli using ipv6 ##"),
    ok = wait_for_coli(coli2ipv6, 300000),
    ct:pal("## Check Netconf using ipv6 ##"),
    ok = wait_for_netconf(nc2ipv6, 300000).


%%%--------------------------------------------------------------------
%%% @doc
%%%   check_cli_coli_nc_after_vnf_2
%%% @end
%%%--------------------------------------------------------------------
check_cli_coli_nc_after_vnf_2(_Config) ->
    TestNode_Nr = "3",
    check_cli_coli_nc_after_vnf_x(_Config, TestNode_Nr).


check_cli_coli_nc_after_vnf_x(_Config, TestNode_Nr) ->
    ct:pal("TestNode_Nr: ~p", [TestNode_Nr]),
    
    ct:pal("################# Check CLI ##"),
    ct:pal("## Check cli using ipv4 ##"),
    CliIpv4 = "cli"++TestNode_Nr++"ipv4",
    ok = wait_for_cli(list_to_atom(CliIpv4), 60000),
    ct:pal("## Check cli using ipv6 ##"),
    CliIpv6 = "cli"++TestNode_Nr++"ipv6",
    ok = wait_for_cli(list_to_atom(CliIpv6), 60000),

    ct:pal("################# Check COLI ##"),
    ct:pal("## Check coli using ipv4 ##"),
    ColiIpv4 = "coli"++TestNode_Nr++"ipv4",
    ok = wait_for_coli(list_to_atom(ColiIpv4), 60000),
    ct:pal("## Check coli using ipv6 ##"),
    ColiIpv6 = "coli"++TestNode_Nr++"ipv6",
    ok = wait_for_coli(list_to_atom(ColiIpv6), 60000),
    
    ct:pal("################# Check NETCONF ##"),
    ct:pal("## Check Netconf using ipv4 ##"),
    NcIpv4 = "nc"++TestNode_Nr++"ipv4",
    ok = wait_for_netconf(list_to_atom(NcIpv4), 60000),
    ct:pal("## Check Netconf using ipv6 ##"),
    NcIpv6 = "nc"++TestNode_Nr++"ipv6",
    ok = wait_for_netconf(list_to_atom(NcIpv6), 60000).


%%%--------------------------------------------------------------------
get_vnf_packages(_Config) ->
    ct:pal("### Get vnfPackages before create to package for upgrade.", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages}} = 
    	rct_vnfm:get_all_vnf_packages(vnfm1),
    ct:pal("### VnfPackages  : ~p", [VnfPackages]).

create_vnf_using_existing_zip(Config, Vnfm, VnfInstName, TestNodeNr) ->

    ct:pal("### Get vnfPackages before create to package for upgrade.", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages}} = 
    	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### VnfPackages  : ~p", [VnfPackages]),

    [VnfPackagesMap] = VnfPackages,
   
    Vnfd_Id = maps:get(<<"vnfdId">>, VnfPackagesMap),
    ct:pal("### Vnfd_Id : ~p", [Vnfd_Id]),

    ct:pal("###  VnfInstName: ~p", [VnfInstName]),

    ct:pal("CREATE VNF ID"),
    {ok, #{<<"vnfInstanceId">> := VnfInstanceId}} = 
    	%% rct_vnfm:create_vnf_zip(Vnfm,Vnfd_Id, VnfName),
    	rct_vnfm:create_vnf_zip(Vnfm,Vnfd_Id, VnfInstName),
    ct:pal("vnfInstanceId ~p",[VnfInstanceId]),
    
    timer:sleep(10000),
    ct:pal("GET VNFS"),
    {ok, VNFS} = rct_vnfm:get_vnfs(Vnfm),
    ct:pal("VNFS ~p",[VNFS]),

    ct:pal("INSTANTIATE VNF"),
    %% {ok, #{<<"vnfLcOpId">> := VnfLcOpId}} = 
    %% 	rct_vnfm:instantiate_vnf_zip(Vnfm,VnfInstanceId),
    {ok, Header, _Body} = 
	rct_vnfm:instantiate_vnf_zip(Vnfm,VnfInstanceId),
    ct:log("HeaderPropList:~p", [Header]),
    BinaryVnfLcOpId = rct_vnfm:get_vnf_lcm_op_occs_binary(Header),

    timer:sleep(5000), 
    ok = rct_vnfm:wait_for_lcop_success(Vnfm, BinaryVnfLcOpId, 600000),

    ct:pal("SLEEP 30 SEC"),    
    timer:sleep(30000),
 
    ct:pal("GET VNFS"),
    {ok, _} = rct_vnfm:get_vnfs(Vnfm),

    ct:pal("GET VNF IP ADDRESSES"),
    {ok, IPs} = rct_vnfm:get_vnf_ips(Vnfm,VnfInstanceId),
    ct:pal("IPs ~p",[IPs]),

    timer:sleep(120000),

    rct_node_state:update_ip_addresses(TestNodeNr, IPs, Config),
    %% rct_node_state:set_state_up(TestNodeNr),

    ok.



%%%--------------------------------------------------------------------
%%% @doc
%%%  TestNodeNr  - integer
%%%  Href        - url, string
%%%  VnfInstName - string
%%% @end
%%%--------------------------------------------------------------------  
create_vnf_using_zip(Config, Vnfm, TestNodeNr, Href, VnfInstName, AIflag) ->
    ct:pal("## Href : ~p", [Href]),

    ct:pal("### Get vnfPackages before create to package for upgrade.", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages}} = 
    	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### VnfPackages  : ~p", [VnfPackages]),

    {ok,#{<<"jobId">> := Job_Id,
    	   <<"vnfPackageId">> := VnfPackageId}} =
    	rct_vnfm:onboarding_vnf_package(Config, Vnfm, Href),
    timer:sleep(5000),
    ok = wait_for_pkg_status_success(Vnfm, Job_Id, 600000),
    ct:pal("### Job_Id  : ~p", [Job_Id]),
    ct:pal("### VnfPackageId  : ~p", [VnfPackageId]),

    Vnfd_Id = get_vnfd_id_(Vnfm, VnfPackageId),
    ct:pal("### Vnfd_Id : ~p", [Vnfd_Id]),

    ct:pal("###  VnfInstName: ~p", [VnfInstName]),

    ct:pal("CREATE VNF ID"),
    {ok, #{<<"vnfInstanceId">> := VnfInstanceId}} = 
    	%% rct_vnfm:create_vnf_zip(Vnfm,Vnfd_Id, VnfName),
    	rct_vnfm:create_vnf_zip(Vnfm,Vnfd_Id, VnfInstName),
    ct:pal("vnfInstanceId ~p",[VnfInstanceId]),
    
    timer:sleep(10000),
    ct:pal("GET VNFS"),
    {ok, VNFS} = rct_vnfm:get_vnfs(Vnfm),
    ct:pal("VNFS ~p",[VNFS]),

    ct:pal("INSTANTIATE VNF"),
    %% {ok, Header, _Body} = 
    %% 	rct_vnfm:instantiate_vnf_zip(Vnfm,VnfInstanceId),
    %% ct:log("HeaderPropList:~p", [Header]),
    %% BinaryVnfLcOpId = rct_vnfm:get_vnf_lcm_op_occs_binary(Header),

    ct:pal("AI_flag: ~p", [AIflag]),
    {ok, Header, _Body} = 
	case AIflag of
	    ai ->
		rct_vnfm:ai_instantiate_vnf_zip(Vnfm,VnfInstanceId);
	    _NoAI ->
		rct_vnfm:instantiate_vnf_zip(Vnfm,VnfInstanceId)
	end,

    ct:log("HeaderPropList:~p", [Header]),
    BinaryVnfLcOpId = rct_vnfm:get_vnf_lcm_op_occs_binary(Header),
    timer:sleep(5000), 
    ok = rct_vnfm:wait_for_lcop_success(Vnfm, BinaryVnfLcOpId, 600000),

    ct:pal("SLEEP 30 SEC"),    
    timer:sleep(30000),
 
    ct:pal("GET VNFS"),
    {ok, _} = rct_vnfm:get_vnfs(Vnfm),

    ct:pal("GET VNF IP ADDRESSES"),
    {ok, IPs} = rct_vnfm:get_vnf_ips(Vnfm,VnfInstanceId),
    ct:pal("IPs ~p",[IPs]),

    rct_node_state:update_ip_addresses(TestNodeNr, IPs, Config),
    
    case AIflag of
	ai ->
	    wait_for_site_config_complete(),
	    check_testlicense_exist_via_cli();
	_Other ->
	    timer:sleep(120000)
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Delete all VNF when VNF has been created from zip pkg, using VNFM REST API
%% @end
%%--------------------------------------------------------------------
delete_vnf_zip(_Config) ->
    ct:pal("GET VNFS"),
    {ok,AllVnfInstance}  = rct_vnfm:get_vnfs(vnfm1),
    ct:log("AllVnfInstance ~p",[AllVnfInstance]),
    {ok, #{<<"vnfs">> := VnfMapList}} = rct_vnfm:get_vnfs(vnfm1),
    ct:pal("VnfMapList ~p",[VnfMapList]),
    
    lists:foreach(fun(Map) ->
    			  VnfInstanceId = maps:get(<<"instanceId">>, Map),
    			  ct:pal("vnfInstanceId ~p",[VnfInstanceId]),
    			  ct:pal("TERMINATE VNF"),
    			  {ok, Header, _Body} = 
			      rct_vnfm:terminate_vnf(vnfm1,VnfInstanceId),
			  ct:log("HeaderPropList:~p", [Header]),
			  BinaryVnfLcOpId = rct_vnfm:get_vnf_lcm_op_occs_binary(Header),
			  
			  %% Location = proplists:get_value("location", Header),
			  %% ct:log("Location: ~p", [Location]),
			  %% VnfLcOpId = lists:last(string:tokens(Location,"/")),
			  %% ct:log("vnfLcOpId:~p", [VnfLcOpId]),
			  %% BinaryVnfLcOpId = list_to_binary(VnfLcOpId),
			  %% ct:log("BinaryVnfLcOpId:~p", [BinaryVnfLcOpId]),

			  %% %% {ok, #{<<"vnfLcOpId">> := VnfLcOpId}}
			  %% %%     = rct_vnfm:terminate_vnf(vnfm1,VnfInstanceId),
			  ok = rct_vnfm:wait_for_lcop_success(vnfm1, BinaryVnfLcOpId, 300000),

			  timer:sleep(5000),
    			  ct:pal("GET VNFS"),
    			  {ok, _} = rct_vnfm:get_vnfs(vnfm1),

    			  timer:sleep(5000),
			  ct:pal("DELETE VNF INSTANCE"),
			  ok = delete_vnf(vnfm1, VnfInstanceId, 120000),
    			  
    			  timer:sleep(5000),
			  ct:pal("GET VNFS"),
    			  {ok, _} = rct_vnfm:get_vnfs(vnfm1)
    		  end, VnfMapList),

    ct:pal("## GET VNFS ##"),
    {ok, VnfAfterDel} = rct_vnfm:get_vnfs(vnfm1),
    ct:pal("VnfAfterDel ~p", [VnfAfterDel]),	  

    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = rct_vnfm:get_tenants(vnfm1),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("GET VNFDS"),
    {ok, AllVnfds}  = rct_vnfm:get_vnfds(vnfm1,Tenant_uuid),
    ct:log("AllVnfds ~p",[AllVnfds]),
    {ok, #{<<"vnfds">> := VnfdMapList}} = rct_vnfm:get_vnfds(vnfm1,Tenant_uuid),
    ct:pal("VnfdMapList ~p",[VnfdMapList]),
    
    lists:foreach(fun(VnfdMap) ->
			  Vnfd_id = maps:get(<<"uuid">>, VnfdMap),
			  ct:pal("Vnfd_id ~p",[Vnfd_id]),

			  ct:pal("DELETE VNFD"),
			  {ok, _} = rct_vnfm:delete_vnfd(vnfm1,Tenant_uuid,Vnfd_id),
			  timer:sleep(30000)

		  end, VnfdMapList),


    ct:pal("GET VNFDS"),
    {ok, VnfdsAfterDel}  = rct_vnfm:get_vnfds(vnfm1,Tenant_uuid),
    ct:pal("## AllVnfdsAfterDel ~p",[VnfdsAfterDel]),

    %% rct_node_state:set_state_down(2).
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Internal functions %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% get_vnfd_id_
%% @end
%%--------------------------------------------------------------------
get_vnfd_id_(Vnfm, VnfPackageId) ->
    ct:pal("### Get vnfdId", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages}} = 
	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### VnfPackages  : ~p", [VnfPackages]),

    ct:pal("### Get package Map. ~n Match VnfPackageId with  : ~p ###", [VnfPackageId]),
    PkgMap = get_map_with_vnfd_pkg_id(VnfPackages, VnfPackageId),
    ct:pal("### PkgMap  : ~p", [PkgMap]),

    Vnfd_Id = maps:get(<<"vnfdId">>, PkgMap),
    ct:pal("### Vnfd_Id  : ~p", [Vnfd_Id]),
    Vnfd_Id.

%%--------------------------------------------------------------------
%% @doc
%% get_map_with_vnfd_pkg_id
%% @end
%%--------------------------------------------------------------------
get_map_with_vnfd_pkg_id([], VnfPackageId) ->
    ct:pal("# # No map match for vnfdId with  : ~p : ~p", [VnfPackageId]),
    [];
get_map_with_vnfd_pkg_id([H | Tail], VnfPackageId) ->
    C = maps:get(<<"vnfPackageId">>, H),
    ct:pal("C: ~p , from map : ~p ", [C, H]),
    %% case binary_to_list(maps:get(<<"vnfdId">>, H)) of
    case maps:get(<<"vnfPackageId">>, H) of
	VnfPackageId ->
	    ct:pal("vnfPackageId: ~p, match in map : ~p", [VnfPackageId, H]),
	    H;
	_Nomatch ->
	    get_map_with_vnfd_pkg_id(Tail, VnfPackageId)
    end.

%%--------------------------------------------------------------------
%% @doc
%% wait_for_pkg_status_success
%% @end
%%--------------------------------------------------------------------
wait_for_pkg_status_success(_Vnfm, _Job_Id, Timeout) when Timeout < 0 ->
    ct:pal("## Job status is not success within expected time."),
    nok;
wait_for_pkg_status_success(Vnfm, Job_Id, Timeout) ->
    JobStatus = rct_vnfm:get_vnfp_job_status(vnfm1, Job_Id),
    ct:log("### Job Status  : ~p", [JobStatus]),
    {ok, #{<<"status">> :=  Status}} = JobStatus,
    case binary_to_list(Status) of
	"success" ->
	    ct:pal("Job Status is success. OK."),
	    ok;
	"failed" ->
	    ct:pal("Job Status is : ~p. Not expected.", [binary_to_list(Status)]),
	    nok;
	_Other ->
	    ct:log("Job Status is : ~p. Not expected. sleep and try again.", [binary_to_list(Status)]),
	    timer:sleep(10000),
	    wait_for_pkg_status_success(Vnfm, Job_Id, Timeout-10000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% delete_vnf
%% @end
%%--------------------------------------------------------------------
delete_vnf(_Vnfm, _VnfInstanceId, Timeout) when Timeout < 0 ->
    ct:pal("## Delete VNF not ok within expected time."),
    nok;
delete_vnf(Vnfm, VnfInstanceId, Timeout) ->
    case rct_vnfm:delete_vnf(vnfm1,VnfInstanceId) of
	{ok, Ans} ->
	    ct:pal("# Delete vnf ok. Answer from delete VNF instance : ~n~p", [Ans]),
	    ok;
	 {Ans2, Ans3} ->
	    ct:log("# Delete vnf is not ok. ~nrcvd reply : ~p. Sleep and try again"
		   "Answer from delete VNF instance : ~n~p", [Ans2, Ans3]),
	    timer:sleep(10000),
	    delete_vnf(Vnfm, VnfInstanceId, Timeout-10000)
    end.


%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

	    
%% get argument from rct_run
get_ct_arg(Arg) ->
    case init:get_argument(Arg) of
        {ok,[[Reply]]} -> 
	    Reply;
        error -> 
	    undefined
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
fix_str(List) ->
    fix_str(List, "\n").
fix_str(List, Token) ->
    FixedStr = string:tokens(List, Token),
    ct:log("Fixed str: ~n~p", [FixedStr]),
    FixedStr.


%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
wait_for_cli(_Cli, Timeout) when Timeout < 0 ->
    ct:fail("wait for cli. Fail due to wait timeout!");
wait_for_cli(Cli, Timeout) ->
    ct:log("Cli : ~p", [Cli]),
    case rct_cli:connect(Cli) of
	ok ->
	    {ok,A} = rct_cli:send(Cli,"show ManagedElement=1,SystemFunctions=1,SwInventory=1 "),
	    ct:log("# AAA: ~p", [A]),
	    ok = rct_cli:disconnect(Cli),
	    ok;
	_Other ->
	    ct:log("# cli not up : ~p .~n sleep and try again.",[_Other]),
	    timer:sleep(5000),
	    wait_for_cli(Cli, Timeout-5000)
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
wait_for_netconf(_Nc, Timeout) when Timeout < 0 ->
    ct:fail("wait for netconf. Fail due to wait timeout!");
wait_for_netconf(Nc, Timeout) ->
    ct:log("Nc : ~p", [Nc]),
    case ct_netconfc:open(Nc, []) of
	{ok, _} ->
	    {ok,A} = 
		ct_netconfc:get(Nc,
				{'ManagedElement',
				 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				 [{managedElementId,[],["1"]}]}),
	    ct:log("# BBB: ~p", [A]),
	    ok = ct_netconfc:close_session(Nc),
	    ok;
	_Other ->
	    ct:log("# Netconf not up : ~p .~n sleep and try again.",[_Other]),
	    timer:sleep(5000),
	    wait_for_netconf(Nc, Timeout-5000)
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
wait_for_coli(_Coli, Timeout) when Timeout < 0 ->
    ct:fail("wait for coli. Fail due to wait timeout!");
wait_for_coli(Coli, Timeout) ->
    ct:log("Coli : ~p", [Coli]),
    case rct_coli:connect(Coli) of
	ok ->
	    {ok,A} = rct_coli:send(Coli, "help coli", "Coli cmd shell usage"),
	    ct:log("# CCC: ~p", [A]),
	    ok = rct_coli:disconnect(Coli),
	    ok;
	_Other ->
	    ct:log("# Coli not up : ~p .~n sleep and try again.",[_Other]),
	    timer:sleep(5000),
	    wait_for_coli(Coli, Timeout-5000)
    end.


%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
get_vnf_type() ->
    VnfType = case ct:get_config({jenkins_config, label_vnf_1}) of
		  undefined ->
		      case ct:get_config({jenkins_config, label_vrc_1}) of
			  undefined ->
			      case ct:get_config({jenkins_config, label_vpp_1}) of
				  undefined ->
				      case ct:get_config({jenkins_config, label_vpp2_1}) of
					  undefined ->
					      case ct:get_config({jenkins_config, label_vsd_1}) of
						  undefined ->
						      ct:log("# undefined #"),
						      undefined;
						  "rcf" ->
						      ct:log("# vSD #"),
						      vsd
					      end;
					  "rcf" ->
					      ct:log("# vPP2 # "),
					      vpp2
				      end;
				  "rcf" ->
				      ct:log("# vPP # "),
				      vpp
			      end;
			  "rcf" ->
			      ct:log("# vRC #"),
			      vrc
		      end;
		  "rcf"-> %% old test could use ths this when create vRC
		      ct:log("# VNF #"),
		      vnf
	      end,
    ct:pal("### VnfType : ~p", [VnfType]),
    VnfType.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
wait_for_site_config_complete() ->
    CliCmd = "show ManagedElement=1,NodeSupport=1,AutoProvisioning=1",
    MatchStr = "SITE_CONFIG_COMPLETE",
    ok =  rct_vnfm:wait_for_cli(cli1ipv4, 300000, CliCmd, MatchStr).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
check_testlicense_exist_via_cli() ->
    CliCmd1 = "show ManagedElement=1,SystemFunctions=1,Lm=1",
    MatchStr1 = "CXC4012117",
    ok =  rct_vnfm:wait_for_cli(cli1ipv4, 60000, CliCmd1, MatchStr1),
    
    CliCmd2 = "show ManagedElement=1,SystemFunctions=1,Lm=1,FeatureState="++MatchStr1,
    MatchStr2 = "licenseState=ENABLED",
    ok =  rct_vnfm:wait_for_cli(cli1ipv4, 60000, CliCmd2, MatchStr2).

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	create_vnf_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/1
-module(create_vnf_SUITE).
-id('Updated by CCase').
-vsn('/main/R9A/1').
-date('2017-03-01'). 
-author('etxkols').
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
%%% R9A/1      2017-03-01 etxkols     Created
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
	 create_vnf/1,
	 delete_vnf/1]).

%% @hidden
suite() -> 
    [{ct_hooks,[{rct_node_state, [{1,vnfm,up},{2,vnf,down},{3,vnf,down}]},
		{rct_logging, [{1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all]},
			       {2, log2, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []}]},
		{rct_cli, [{cli1, [manual_connect]},{cli2, [manual_connect]}]},
		{rct_coli,[{coli1,[manual_connect]},{coli2,[manual_connect]}]},
		{rct_rpc,[rpc1,rpc2]},
		{rct_netconf,[nc1,nc2]},
		{rct_http,[http1,http2]},
		{rct_vnfm,[vnfm1]},
		{rct_core,[[],[]]}
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
all() -> [create_vnf].
%all() -> [delete_vnf].
%all() -> [create_vnf,delete_vnf].

%%--------------------------------------------------------------------
%% @doc
%% Create VNF using VNFM REST API
%% @end
%%--------------------------------------------------------------------
create_vnf(Config) ->
    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = 
	rct_vnfm:get_tenants(vnfm1),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("INSERT DATACENTER"),
    {ok, #{<<"datacenter">> := #{<<"uuid">> := Datacenter_uuid}}} = 
	rct_vnfm:insert_datacenter(vnfm1,"rcs","openstack"),
    ct:pal("datacenter uuid ~p",[Datacenter_uuid]),

    ct:pal("ASSOCIATE DATACENTER"),
    {ok, _} = rct_vnfm:associate_datacenter(vnfm1,Datacenter_uuid),

    ct:pal("GET DATACENTERS"),
    {ok, _} = rct_vnfm:get_datacenters(vnfm1,Tenant_uuid),
 
    ct:pal("INSERT VNFD"),
    {ok, #{<<"id">> := Vnfd_id}} = 
	rct_vnfm:insert_vnfd(vnfm1,2,Tenant_uuid),
    ct:pal("vnfd id ~p",[Vnfd_id]),

    ct:pal("GET VNFDS"),
    {ok, _} = rct_vnfm:get_vnfds(vnfm1,Tenant_uuid),

    ct:pal("CREATE VNF"),
    {ok, #{<<"vnfInstanceId">> := VnfInstanceId}} = 
	rct_vnfm:create_vnf(vnfm1,Vnfd_id),
    ct:pal("vnfInstanceId ~p",[VnfInstanceId]),
    
    ct:pal("GET VNFS"),
    {ok, _} = rct_vnfm:get_vnfs(vnfm1),
    
    ct:pal("INSTANTIATE VNF"),
    {ok, #{<<"vnfLcOpId">> := _VnfLcOpId}} = 
	rct_vnfm:instantiate_vnf(vnfm1,VnfInstanceId),

    ct:pal("SLEEP 5 SEC (STUPID, BUT SEEMS NECESSARY AT THE MOMENT)"),    
    timer:sleep(5000),
    
    ct:pal("GET VNFS"),
    {ok, _} = rct_vnfm:get_vnfs(vnfm1),

    ct:pal("SLEEP 30 SEC"),    
    timer:sleep(30000),
 
    ct:pal("GET VNFS"),
    {ok, _} = rct_vnfm:get_vnfs(vnfm1),

    ct:pal("GET VNF IP ADDRESSES"),
    {ok, IPs} = rct_vnfm:get_vnf_ips(vnfm1,VnfInstanceId),
    ct:pal("IPs ~p",[IPs]),

    timer:sleep(120000),

    rct_node_state:update_ip_addresses(2, IPs, Config),
    rct_node_state:set_state_up(2),

    rct_rpc:xcall(rpc2,os,cmd,["ifconfig eth0"],10000),
    ok = rct_cli:connect(cli2),
    {ok,_} = rct_cli:send(cli2,"show ManagedElement=1"),
    ok = rct_cli:disconnect(cli2).

%%--------------------------------------------------------------------
%% @doc
%% Delete VNF using VNFM REST API
%% @end
%%--------------------------------------------------------------------
delete_vnf(_Config) ->
    ct:pal("GET VNFS"),
    {ok, #{<<"vnfs">> := [#{<<"instanceId">> := VnfInstanceId}]}} = rct_vnfm:get_vnfs(vnfm1),
    ct:pal("vnfInstanceId ~p",[VnfInstanceId]),

    ct:pal("TERMINATE VNF"),
    {ok, _} = rct_vnfm:terminate_vnf(vnfm1,VnfInstanceId),

    ct:pal("GET VNFS"),
    {ok, _} = rct_vnfm:get_vnfs(vnfm1),

    ct:pal("SLEEP 30 SEC"),    
    timer:sleep(30000),

    ct:pal("GET VNFS"),
    {ok, _} = rct_vnfm:get_vnfs(vnfm1),

    ct:pal("DELETE VNF"),
    {ok, _} = rct_vnfm:delete_vnf(vnfm1,VnfInstanceId),

    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = rct_vnfm:get_tenants(vnfm1),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("GET VNFDS"),
    {ok, #{<<"vnfds">> := [#{<<"uuid">> := Vnfd_id}]}} = rct_vnfm:get_vnfds(vnfm1,Tenant_uuid),

    ct:pal("DELETE VNFD"),
    {ok, _} = rct_vnfm:delete_vnfd(vnfm1,Tenant_uuid,Vnfd_id),

    ct:pal("GET DATACENTERS"),
    {ok, #{<<"datacenters">> := [#{<<"uuid">> := Datacenter_uuid}]}} = rct_vnfm:get_datacenters(vnfm1,Tenant_uuid),

    ct:pal("DELETE DATACENTER"),
    {ok, _} = rct_vnfm:delete_datacenter(vnfm1,Datacenter_uuid),

    rct_node_state:set_state_down(2).




	    


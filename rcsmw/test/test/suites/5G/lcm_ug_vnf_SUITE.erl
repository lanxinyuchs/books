%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lcm_ug_vnf_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/3
%%%
%%% @doc == VNFM-VNF upgrade VNF. ==
%%% <br/><br/>
%%% @end
%%%

-module(lcm_ug_vnf_SUITE).
-vsn('/main/R9A/3').

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
%%% R9A/1      2017-02-21 etxivri     Created
%%% R9A/2      2017-02-21 etxivri     Add logging for second and new upgraded vnf.
%%% R9A/3      2017-03-02 etxivri     Removed get_all to be used in logging hook.
%%% ----------------------------------------------------------

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
	 groups/0,
	 break/1,
	 create_vnf/1,
	 delete_vnf/1,
	 check_vnf1_cli_nc_coli/1,

	 %% %% Upgrade
	 upgrade_vnf/1,
	 ug_prepare_cancel_delete/1
	]).


-define(CliTxtFile, "/home/etxivri/Kista/5G/vnfm_vnf/check_cli_swmI.txt").

suite() ->  
    [{ct_hooks,[
		{rct_node_state, [{1,vnfm,up},{2,vnf,down}, {3,vnf,down}]},
		{rct_logging, [%% {1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all]},
			       {1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []},
			       {2, log2, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []},
			       {3, log3, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []}
			      ]},
		{rct_cli, [{cli1, [manual_connect]}, {cli2, [manual_connect]}, {cli3, [manual_connect]}] },
		{rct_coli,[{coli1,[manual_connect]}, {coli2,[manual_connect]}, {coli3,[manual_connect]}] },
		{rct_rpc,[rpc1, rpc2, rpc3]},
		{rct_netconf,[nc1, nc2, nc3]},
		{rct_http,[http1, http2, http3]},
		{rct_vnfm,[vnfm1]},
		{rct_core,[[],[]]}
		]}].
%% @hidden
init_per_suite(Config) -> 
    Config.

%% @hidden
end_per_suite(_Config) -> 
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) -> 
    Config.
%% @hidden
end_per_testcase(TestCase, Config) -> 
    ct:pal("# End TC : ~p #", [TestCase]),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end.

init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> [
	  ].

groups() ->
    [
    ].


break(_Config) ->
    test_server:break("AA").

%%%--------------------------------------------------------------------
%%% @doc
%%%  upgrade_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
upgrade_vnf(_Config) ->
    ct:pal("# Get needed data for create upgrade job Id "),

    FromInstanceId = get_from_instance_id(vnfm1),
    %% %% {FromInstanceId, ToVnfPkgId, ToVnfDescId} =
    %% %% 	get_needed_data_for_ug_create(vnfm1),
    
    ToVnfd_Id = create_vnf_to_pkg(vnfm1, 3),
    %% ToVnfd_Id = <<"ivri-99">>,
    ToVnfPkgId = get_to_pkg_id_for_ug_create(vnfm1, ToVnfd_Id),

    %% %%%%%% Create upgrade job id %%%%%%%%%%%%%%%%
    ct:pal("# Create job id operation for upgrade "),
    Ug_Job_Id = ug_create_job_id_op(vnfm1, FromInstanceId, ToVnfPkgId, ToVnfd_Id),

    ct:pal("# Get job status operation for an ongoing upgrade job "),
    get_job_status(vnfm1, Ug_Job_Id ),


    %% %%%%%% Prepare %%%%%%%%%%%%%%%%
    ct:pal("# Prepare operation for upgrade "),
    ug_prepare(vnfm1, Ug_Job_Id),


    %% %%%%%% Verify %%%%%%%%%%%%%%%%
    ct:pal("# Verify operation for upgrade "),
    ug_verify(vnfm1, Ug_Job_Id),


    %% %%%%%% Activate %%%%%%%%%%%%%%%%
    ct:pal("# Activate operation for upgrade "),
    ug_activate(vnfm1, Ug_Job_Id),


    %% %%%%%% Confirm  %%%%%%%%%%%%%%%%
    ug_confirm(vnfm1, Ug_Job_Id),
    
    get_job_status(vnfm1, Ug_Job_Id),

    %% %%%%% Check after Upgrade %%%%%%%
    ct:pal("####  Check after Upgrade #### "),
    check_ug_result_ok(vnfm1, Ug_Job_Id),

    %% %%%%% Set Node State Up after upgrade %%%%%%%
    ct:pal("Set node state up after upgrade."),
    set_node_state_up(3),

    %% %%%%% Get new vnf IP addresses %%%%%%%
    IpTupleList = get_vnf_ip_addresses(vnfm1),

    %% %%%%% Update IP addresses in stp config %%%%%%%
    ct:pal("update_ip_addresses for new vnf after upgrade"),
    rct_node_state:update_ip_addresses(3, IpTupleList, _Config),

    %% %%%%% Some checks after upgrade %%%%%%%
    ct:pal("Check for node up and ready for cli, netconf, coli"), 
    ok = wait_for_cli(cli3, 180000),
    ok = wait_for_coli(coli3, 180000),
    ok = wait_for_netconf(nc3, 300000), %% Currently not woring after upgrade due to no port is given.

    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_prepare_cancel_delete <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_prepare_cancel_delete(_Config) ->
    ct:pal("# Get needed data for create upgrade job Id "),
    %% {FromInstanceId, ToVnfPkgId, ToVnfDescId} =
    %% 	get_needed_data_for_ug_create(vnfm1),
    FromInstanceId = get_from_instance_id(vnfm1),
    ToVnfd_Id = create_vnf_to_pkg(vnfm1, 3),

    ToVnfPkgId = get_to_pkg_id_for_ug_create(vnfm1, ToVnfd_Id),
    
    ct:pal("# Create job id operation for upgrade "),
    Ug_Job_Id = ug_create_job_id_op(vnfm1, FromInstanceId, ToVnfPkgId, ToVnfd_Id),
    ct:pal("# Ug_Job_Id : ~p", [Ug_Job_Id]),
    ct:pal("# Prepare operation for upgrade "),
    ug_prepare_no_state_check(vnfm1, Ug_Job_Id),

    get_job_status(vnfm1, Ug_Job_Id ),

    ct:pal("# Cancel operation for upgrade "),
    ug_cancel(vnfm1, Ug_Job_Id),

    ug_delete_job_id(vnfm1, Ug_Job_Id),

    %% delete vnf package
    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = rct_vnfm:get_tenants(vnfm1),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("DELETE VNFD"),
    {ok, _} = rct_vnfm:delete_vnfd(vnfm1,Tenant_uuid, ToVnfd_Id),

    timer:sleep(5000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Upgrade flows
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_create_job_id_op <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_create_job_id_op(Vnfm,FromInstanceId, ToVnfPkgId, ToVnfDescId) ->
    ct:pal("# Create job id operation for upgrade "),
    {ok, #{<<"job_id">> :=  Ug_Job_Id}} =
    	rct_vnfm:ug_create_job_id_operation(Vnfm, FromInstanceId, ToVnfPkgId, ToVnfDescId),
    ct:pal("### Upgrade Job Id : ~p", [Ug_Job_Id]),
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"INITIALIZED">>, 30000),
    Ug_Job_Id.

%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_prepare <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_prepare(Vnfm, Ug_Job_Id) ->
    ct:pal("# Prepare operation for upgrade "),
    Prepare = rct_vnfm:ug_prepare(Vnfm, Ug_Job_Id),
    ct:pal("### Ans from prepare   : ~p", [Prepare]),
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"PREPARE_COMPLETED">>, 30000),
    Prepare.


ug_prepare_no_state_check(Vnfm, Ug_Job_Id) ->
    ct:pal("# Prepare operation for upgrade "),
    Prepare = rct_vnfm:ug_prepare(Vnfm, Ug_Job_Id),
    ct:pal("### Ans from prepare   : ~p", [Prepare]),
    Prepare.
%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_verify <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_verify(Vnfm, Ug_Job_Id) ->
    ct:pal("# Verify operation for upgrade "),
    Verify = rct_vnfm:ug_verify(Vnfm, Ug_Job_Id),
    ct:pal("### Ans from verify   : ~p", [Verify]),
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"PREPARE_COMPLETED">>, 60000),
    Verify.

%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_activate <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_activate(Vnfm, Ug_Job_Id) ->
    ct:pal("# Activate operation for upgrade "),
    Activate = rct_vnfm:ug_activate(Vnfm, Ug_Job_Id),
    ct:pal("### Ans from activate   : ~p", [Activate]),
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"WAITING_FOR_CONFIRM">>, 300000), 
    Activate.

%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_confirm <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_confirm(Vnfm, Ug_Job_Id) ->
    ct:pal("# Confirm operation for upgrade "),
    Comfirm = rct_vnfm:ug_confirm(Vnfm, Ug_Job_Id),
    ct:pal("### Ans from confirm   : ~p", [Comfirm]),
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"CONFIRM_COMPLETED">>, 60000),
    Comfirm.

%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_cancel <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_cancel(Vnfm, Ug_Job_Id) ->
    ct:pal("# Cancel operation for upgrade "),
    Cancel = rct_vnfm:ug_cancel(Vnfm, Ug_Job_Id),
    ct:pal("### Ans from cancel   : ~p", [Cancel]),
    wait_for_exp_ug_state(vnfm1, Ug_Job_Id, <<"INITIALIZED">>, 60000), %% ?
    Cancel.

%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_delete_job_id <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_delete_job_id(Vnfm, Ug_Job_Id) ->
    ct:pal("# Delete job id  "),
    Delete =
    	rct_vnfm:ug_delete_job_id(Vnfm, Ug_Job_Id),
    ct:pal("### Delete : ~p", [Delete]),
    Delete.

%%%--------------------------------------------------------------------
%%% @doc
%%%  get_job_status <br/>
%%% @end
%%%--------------------------------------------------------------------
get_job_status(Vnfm,Ug_Job_Id ) ->
    ct:pal("# Get job status operation for an ongoing upgrade job "),
    {ok, UgJobStatus} = rct_vnfm:ug_get_job_status(Vnfm, Ug_Job_Id),
    ct:pal("### UgJobStatus  : ~p", [UgJobStatus]),
    UgJobStatus.


%%%--------------------------------------------------------------------
%%% @doc
%%%  set_node_state_up <br/>
%%% @end
%%%--------------------------------------------------------------------
set_node_state_up(NodeNr) ->
    rct_node_state:set_state_up(NodeNr),
    ct:pal("wait_for_node_state up"),
    wait_for_node_state(3, up, 60000).

%%%--------------------------------------------------------------------
%%% @doc
%%%  get_vnf_ip_addresses <br/>
%%% @end
%%%--------------------------------------------------------------------
get_vnf_ip_addresses(Vnfm) ->
    ct:pal("GET VNF ID"),
    {ok, #{<<"vnfs">> := [VnfMapData] }} = rct_vnfm:get_vnfs(Vnfm),
    ct:pal("# # Vnf data : ~p", [VnfMapData]),
    
    VnfInstanceId = maps:get(<<"instanceId">>, VnfMapData),
    ct:pal("# # Vnf instanceId : ~p", [VnfInstanceId]),

    IpTupleList = wait_for_vnf_ips(Vnfm, VnfInstanceId, 300000),
    ct:pal("# # IpTupleList : ~p", [IpTupleList]),
    IpTupleList.

%% -----------------------------
%% -----------------------------

%% wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState) ->
%%     wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState, 300000). %% 5min
wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState, Timeout) when Timeout < 0 ->
    ct:pal("# Not rcvd ext state within time.  ~nState : ~p , Vnfm : ~p , job_id : ~p", [ExpState, Vnfm, Ug_Job_Id]),
    ct:fail("## Expected State not rcvd within timeout. ##");
wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState, Timeout) ->
    ct:pal("# Wait for exp state : ~p , Vnfm : ~p , job_id : ~p", [ExpState, Vnfm, Ug_Job_Id]),
    {ok, UgStatusMap} = rct_vnfm:ug_get_job_status(Vnfm, Ug_Job_Id),
    ct:pal("### UgStatusMap  : ~p", [UgStatusMap]),
    #{<<"state">> :=  State} = UgStatusMap,
    case State == ExpState of
	true ->
	    ct:pal("### Rcvd State : ~p , OK it is same as expecte State : ~p ", [State, ExpState]),
	    ok;
	_Other ->
	    ct:pal("### Rcvd State : ~p , NOT OK it is not same as expecte State : ~p . ~nSleep and try again.", [State, ExpState]),
	    timer:sleep(10000),
	    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState, Timeout- 10000)
    end.

get_from_instance_id(Vnfm) ->
    ct:pal("GET VNF ID"),
    {ok, #{<<"vnfs">> := [VnfMapData] }} = rct_vnfm:get_vnfs(Vnfm),
    ct:pal("### Vnf data : ~p", [VnfMapData]),
    
    FromInstanceId = maps:get(<<"instanceId">>, VnfMapData),
    ct:pal("### From Vnf instanceId : ~p", [FromInstanceId]),
    FromInstanceId.


create_vnf_to_pkg(Vnfm, TestNodeNr) ->
    ct:pal("Prepare the vnf upgradepackage on vnfm for VNF: ~p", [TestNodeNr]),

    ct:pal("GET TENANTS"),
    {ok, #{<<"tenant">> := #{<<"uuid">> := Tenant_uuid}}} = 
    	rct_vnfm:get_tenants(Vnfm),
    ct:pal("tenant uuid~p",[Tenant_uuid]),

    ct:pal("GET DATACENTERS"),
    {ok, _} = rct_vnfm:get_datacenters(vnfm1,Tenant_uuid),

    ct:pal("### Get vnfPackages before create to package for upgrade.", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages}} = 
	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### VnfPackages  : ~p", [VnfPackages]),

    ct:pal("INSERT VNFD , to create vnf to package"),
    {ok, #{<<"id">> := Vnfd_id}} = 
    	rct_vnfm:insert_vnfd(Vnfm, TestNodeNr,Tenant_uuid),
    ct:pal("vnfd id ~p",[Vnfd_id]),

    timer:sleep(5000),
    ct:pal("### Get vnfPackages after create to package for upgrade ", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages2}} = 
	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### VnfPackages  : ~p", [VnfPackages2]),
    Vnfd_id.


get_to_pkg_id_for_ug_create(Vnfm, ToVnfd_Id) ->
    ct:pal("### Get vnfPackages", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages}} = 
	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### VnfPackages  : ~p", [VnfPackages]),

    ct:pal("########## Get To package Map. ~n Match vnfdId with  : ~p #################", [ToVnfd_Id]),
    ToPkgMap = get_map_with_to_vnfd_pkg_id(VnfPackages, ToVnfd_Id),
    ct:pal("### ToPkgMap  : ~p", [ToPkgMap]),

    ToVnfPkgId = maps:get(<<"vnfPackageId">>, ToPkgMap),
    ct:pal("### ToVnfPkgId  : ~p", [ToVnfPkgId]),

    ToVnfPkgId.


get_map_with_to_vnfd_pkg_id([], ToVnfd_Id) ->
    ct:pal("# # No map match for vnfdId with  : ~p : ~p", [ToVnfd_Id]),
    [];
get_map_with_to_vnfd_pkg_id([H | Tail], ToVnfd_Id) ->
    C = maps:get(<<"vnfdId">>, H),
    ct:pal("C: ~p , from map : ~p ", [C, H]),
    %% case binary_to_list(maps:get(<<"vnfdId">>, H)) of
    case maps:get(<<"vnfdId">>, H) of
	ToVnfd_Id ->
	    ct:pal("vnfdId: ~p, match in map : ~p", [ToVnfd_Id, H]),
	    H;
	_Nomatch ->
	    get_map_with_to_vnfd_pkg_id(Tail, ToVnfd_Id)
    end.


check_ug_result_ok(Vnfm, Ug_Job_Id) ->
    #{<<"operation_results">> :=  OperationResMaps} = 
	get_job_status(Vnfm, Ug_Job_Id ),
    ct:pal("# # OperationResMaps  : ~p", [OperationResMaps]),

    PrepareMap = check_maps_for_operation_success(OperationResMaps, "PREPARE"),
    <<"SUCCESS">> = maps:get(<<"result">>, PrepareMap),
    VerifyMap = check_maps_for_operation_success(OperationResMaps, "VERIFY"),
    <<"SUCCESS">> = maps:get(<<"result">>, VerifyMap),
    ActivateMap = check_maps_for_operation_success(OperationResMaps, "ACTIVATE"),
    <<"SUCCESS">> = maps:get(<<"result">>, ActivateMap),
    ConfirmMap = check_maps_for_operation_success(OperationResMaps, "CONFIRM"),
    <<"SUCCESS">> = maps:get(<<"result">>, ConfirmMap),
    ok.


check_maps_for_operation_success([], Operation) ->
    ct:pal("# # No map match for operation : ~p", [Operation]),
    [];
check_maps_for_operation_success([H | Tail], Operation) ->
    B = maps:get(<<"operation">>, H),
    ct:log("B: ~p ", [B]),
    case binary_to_list(maps:get(<<"operation">>, H)) of
	Operation ->
	    ct:pal("Operation: ~p, match in map : ~p", [Operation, H]),
	    H;
	_Nomatch ->
	    check_maps_for_operation_success(Tail, Operation)
    end.


%%%--------------------------------------------------------------------
%%% @doc
%%% create_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
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


    %% %% Here the vnf package is created , openmano vnfp-list. No VNF inst is created.
    ct:pal("INSERT VNFD"),
    {ok, #{<<"id">> := Vnfd_id}} = 
	rct_vnfm:insert_vnfd(vnfm1,2,Tenant_uuid),
    ct:pal("vnfd id ~p",[Vnfd_id]),

    %% %% No VNF inst is created.
    ct:pal("GET VNFDS"),
    {ok, _} = rct_vnfm:get_vnfds(vnfm1,Tenant_uuid),

    %% %% Here the VNF is created. vnfm vnf-list. 
    ct:pal("CREATE VNF"),
    {ok, #{<<"vnfInstanceId">> := VnfInstanceId}} = 
	rct_vnfm:create_vnf(vnfm1,Vnfd_id),
    ct:pal("vnfInstanceId ~p",[VnfInstanceId]),

    timer:sleep(10000),
    ct:pal("GET VNFS"), %% VRCS
    %% {ok, _} = rct_vnfm:get_vnfs(vnfm1),
    ok = wait_for_vnfs(vnfm1, 30000),

    ct:pal("INSTANTIATE VNF"),
    {ok, #{<<"vnfLcOpId">> := _VnfLcOpId}} =
	rct_vnfm:instantiate_vnf(vnfm1,VnfInstanceId),

    timer:sleep(10000),
    ct:pal("GET VNFS"),
    %% {ok, _} = rct_vnfm:get_vnfs(vnfm1),
    ok = wait_for_vnfs(vnfm1, 30000),

    timer:sleep(30000),
    ct:pal("GET VNF IP ADDRESSES"),
    IPs = wait_for_vnf_ips(vnfm1,VnfInstanceId, 30000),
    ct:pal("IPs ~p",[IPs]),

    ct:pal("SLEEP 10 SEC"),    
    timer:sleep(10000),
    
    ct:pal("update_ip_addresses"),
    rct_node_state:update_ip_addresses(2, IPs, Config),
    ct:pal("set_state_up"),
    rct_node_state:set_state_up(2),
    ct:pal("wait_for_node_state up"),
    wait_for_node_state(2, up, 60000),

    ct:pal("Check for node up and ready for cli"), 
    ok = wait_for_cli(cli2, 180000),
    ok = wait_for_netconf(nc2, 180000),
    ok = wait_for_coli(coli2, 180000),

    %% ct:pal("ifconfig eth0"), 
    %% IfConfig = rct_rpc:xcall(rpc2,os,cmd,["ifconfig eth0"],10000),
    ct:pal("ifconfig"), 
    IfConfig = rct_rpc:xcall(rpc2,os,cmd,["ifconfig "],10000),
    If_Config = fix_str(IfConfig),
    ct:pal("# If_Config : ~n~p", [If_Config]),
    
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% delete_vnf<br/>
%%% @end
%%%--------------------------------------------------------------------
delete_vnf(_Config) ->
    ct:pal("GET VNFS"),
    {ok, #{<<"vnfs">> := [#{<<"instanceId">> := VnfInstanceId}]}} = rct_vnfm:get_vnfs(vnfm1),
    ct:pal("vnfInstanceId ~p", [VnfInstanceId]),

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
	

%%%--------------------------------------------------------------------
%%% @doc
%%% check_vnf_cli_nc_coli <br/>
%%% @end
%%%--------------------------------------------------------------------
check_vnf1_cli_nc_coli(Config) ->
    check_vnf_cli_nc_coli(cli2, nc2, coli2, Config).

check_vnf_cli_nc_coli(Cli, Nc, Coli, _Config) ->
    ok = wait_for_cli(Cli, 180000),
    ok = wait_for_netconf(Nc, 180000),
    ok = wait_for_coli(Coli, 180000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
wait_for_vnfs(_Vnfm, Timeout) when Timeout < 0 ->
    ct:fail("wait_for_vnfs. Fail due to wait timeout!");
wait_for_vnfs(Vnfm, Timeout) ->
    ct:log("Vnfm : ~p", [Vnfm]),
    case rct_vnfm:get_vnfs(Vnfm) of
	{ok, _Answ} ->
	    ct:pal("# get_vnfs rcvd : ~p ", [_Answ]),
	    ok;
	_Other ->
	    ct:log("# no VNFS recvd : ~p .~n sleep and try again.",[_Other]),
	    timer:sleep(5000),
	    wait_for_vnfs(Vnfm, Timeout-5000)
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
wait_for_vnf_ips(_Vnfm, _VnfInstanceId, Timeout) when Timeout < 0 ->
    ct:fail("wait_for_vnf_ips. Fail due to wait timeout!");
wait_for_vnf_ips(Vnfm,VnfInstanceId, Timeout) ->
    ct:log("Vnfm : ~p", [Vnfm]),
    case rct_vnfm:get_vnf_ips(Vnfm, VnfInstanceId) of
	{ok, IPs} ->
	    ct:pal("# IPs ~p",[IPs]),
	    IPs;
	_Other ->
	    ct:log("# no IPs recvd : ~p .~n sleep and try again.",[_Other]),
	    timer:sleep(5000),
	    wait_for_vnf_ips(Vnfm, VnfInstanceId, Timeout-5000)
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
wait_for_node_state(_NodeNr, _ExpState, Timeout) when Timeout < 0 ->
    ct:fail("wait_for_node_state. Fail due to wait timeout!");
wait_for_node_state(NodeNr, ExpState, Timeout) ->
    ct:log("ExpState : ~p", [ExpState]),
    case rct_node_state:get_state(NodeNr) of
	ExpState ->
	    ct:log("Node : ~p , ExpState : ~p", [NodeNr, ExpState]),
	    ok;
	_Other ->
	    ct:log("# Node : ~p  : ~p .~n sleep and try again.",[NodeNr]),
	    timer:sleep(5000),
	    wait_for_node_state(NodeNr, ExpState, Timeout-5000)
    end.

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
    ct:log("Nc : ~p", [Coli]),
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


%% %%%--------------------------------------------------------------------
%% %%% @doc
%% %%% @end
%% %%%--------------------------------------------------------------------
%% %% rpc_os_cmd(NodeRpc, Cmd) ->
%% %%     rpc_os_cmd(NodeRpc, Cmd, 10000).
%% rpc_os_cmd(NodeRpc, Cmd, Timeout) ->
%%     ct:pal("# rpc : ~p, cmd : ~p",[NodeRpc, Cmd]),
%%     Reply = rct_rpc:xcall(NodeRpc, os, cmd, [Cmd], Timeout),
%%     ct:log("Reply : ~p", [Reply]),
%%     FixedReply = fix_str(Reply),
%%     ct:log("FixedReply : ~p", [FixedReply]),
%%     FixedReply.

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

%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lcm_ug_vnf_from_zip_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R11A/R12A/2
%%%
%%% @doc == VNFM-VNF upgrade VNF using a zip packets. ==
%%% <br/><br/>
%%% @end
%%%

-module(lcm_ug_vnf_from_zip_SUITE).
-vsn('/main/R9A/R10A/R11A/R12A/2').

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
%%% R9A/1      2017-03-23 etxivri     Created
%%% R9A/2      2017-04-03 etxivri     Update to get zip packet that shall be used as to UP,
%%%                                   Href from jenkins config
%%% R9A/3      2017-04-06 etxivri     Update to get better logs.
%%%                                   Devide upgrade into specific upgrade flow.
%%% R9A/4      2017-04-07 etxivri     Increased timeout when wait for ug result.
%%% R9A/5      2017-04-07 etxivri     Minor update.
%%% R9A/6      2017-04-11 etxivri     Minor update in printouts.
%%% R10A/1     2017-05-11 etxivri     Update due to new behaviour. Shutown on from vnf after
%%%                                   activate is done.
%%% R10A/2     2017-05-11 etxivri     add fallback timeout when send confirm.
%%% R10A/3     2017-05-17 etxivri     Removed usage of fallbacktimer in confirm 
%%% R10A/4     2017-05-18 etxivri     Update for vPP and vSD
%%% R10A/5     2017-05-22 etxivri     Update HORROR filter  due to printout on vPP.
%%% R10A/6     2017-05-29 etxivri     Update HORROR filter .
%%% R10A/7     2017-06-27 etxivri     Update cancel tc to. 
%%%                                   Increased som timeouts.
%%% R10A/9     2017-07-06 etxivri     Update to cancel durin activate is ongoing.
%%% R11A/2     2017-08-30 etxivri     fixed compile faults.
%%% R11A/3     2017-09-13 etxivri     increased timouts.
%%% R11A/4     2017-09-27 etxivri     Update for vPP2
%%% R11A/5     2017-09-28 etxivri     More update for vPP2
%%% R12A/1     2017-11-15 etxivri     -Set vnfm to down in hook due to soon rpc will not work 
%%%                                    on vnfm and the logging will fail.
%%% R12A/2     2017-11-28 etxivri     Update in error log filter due to 
%%%                                   esi@vrcs is started up by lrat when generate esi. 
%%%                                   lrat need to fix this. 
%%%                                   
%%% ----------------------------------------------------------
-include_lib("common_test/include/ct.hrl").

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
	 %% check_vnf1_cli_nc_coli/1,

	 %% %% Upgrade, step by step
	 create_to_pkg/1,
	 prepare_vnf/1,
	 verify_vnf/1,
	 activate_vnf/1,
	 confirm_vnf/1,

	 ug_confirm/2,
	 ug_confirm/3,

	 set_node_2_state_down/1,
	 check_cli_coli_nc_node3/1,

	 %% vRC All upgrade steps in one TC., use 2 stps
	 upgrade_vnf/1,
	 %% ug_prepare_cancel_delete/1,
	 ug_activate_cancel_delete/1,

	 check_cli_coli_nc/1,
	 check_vnf_cli_ipv4_ipv6/1,
	 check_vnf_nc_ipv4_ipv6/1,
	 check_vnf_coli_ipv4_ipv6/1
	]).


suite() ->  
    [{ct_hooks,[{rct_htmllink,[]},
		{rct_node_state, [{1,vnfm,down},{2,vnf,up},{3,vnf,down}]}, %% vnf2 already up.
		{rct_logging, [
			       {1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
							["initial supervision timer expired"]}}], []},
			       {2, log2, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []},
			       {3, log3, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
							["xmerl_scan,file",
							 "Node esi@vrcs not responding"]}}], []}
			      ]},
		{rct_cli, [{1, cli1, ssh_lmt_ipv4, [manual_connect]},
			   {2, cli2ipv4, ssh_lmt_ipv4, [manual_connect]},
			   {2, cli2ipv6, ssh_lmt_ipv6, [manual_connect]},
			   {3, cli3ipv4, ssh_lmt_ipv4, [manual_connect]},
			   {3, cli3ipv6, ssh_lmt_ipv6, [manual_connect]}
			  ]},
		{rct_coli,[{1, coli1, ssh_lmt_ipv4, [manual_connect]},
			   {2, coli2ipv4, ssh_lmt_ipv4, [manual_connect]},
			   {2, coli2ipv6, ssh_lmt_ipv6, [manual_connect]},
			   {3, coli3ipv4, ssh_lmt_ipv4, [manual_connect]},
			   {3, coli3ipv6, ssh_lmt_ipv6, [manual_connect]}
			  ]},

		{rct_rpc,[rpc1,rpc2,rpc3]},
		{rct_netconf,[{1, nc1, oam_auto},
			      {2, nc2ipv4, ssh_lmt_ipv4},
			      {2, nc2ipv6, ssh_lmt_ipv6},
			      {3, nc3ipv4, ssh_lmt_ipv4},
			      {3, nc3ipv6, ssh_lmt_ipv6}
			     ]},
		%% {rct_http,[http1,http2]},
		{rct_vnfm,[vnfm1]},
		{cth_conn_log, []},
		{rct_core,[[], [], []]}
		]}].

%% used in check after upgrade_vnf
-define(cli2ipv4, cli2ipv4).
-define(cli2ipv6, cli2ipv6).
-define(nc2ipv4, nc2ipv4).
-define(nc2ipv6, nc2ipv6).
-define(coli2ipv4, coli2ipv4).
-define(coli2ipv6, coli2ipv6).

%% used in check after step by step upgrade
-define(cli3ipv4, cli3ipv4).
-define(cli3ipv6, cli3ipv6).
-define(nc3ipv4, nc3ipv4).
-define(nc3ipv6, nc3ipv6).
-define(coli3ipv4, coli3ipv4).
-define(coli3ipv6, coli3ipv6).

%% @hidden
init_per_suite(Config) -> 
    Config.

%% @hidden
end_per_suite(_Config) -> 
    ok.
init_per_testcase(_TestCase, Config) -> 
    Config.
%% @hidden
end_per_testcase(activate_vnf, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.", [Reason]),
	    ct:pal("Set node state up on new instance after activate."),
	    set_node_state_up(3), %% to get all logs.
	    ct:pal("Update ip address on node 3 config to get logs ."),
	    {_, ToVnfd_Id, _} = get_ug_job_id_from_config(Config),
	    update_ip_addr(vnfm1, 3, ToVnfd_Id, Config)
    end;
end_per_testcase(confirm_vnf, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
      	    ct:pal("Testcase failed due to: ~p.", [Reason]),
	    ct:pal("Set node state up on new instance after activate."),
	    set_node_2_state_down(Config) %% Old VNF deletes at confim
    end;
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
all() -> [check_vnf_cli_ipv4_ipv6,
	  check_vnf_nc_ipv4_ipv6,
	  check_vnf_coli_ipv4_ipv6,
	  upgrade_vnf,
	  check_vnf_cli_ipv4_ipv6,
	  check_vnf_nc_ipv4_ipv6,
	  check_vnf_coli_ipv4_ipv6
	  ].

groups() ->
    [{ug_step_by_step,[],[create_to_pkg,
			  prepare_vnf,
			  verify_vnf,
			  activate_vnf,
			  confirm_vnf,
			  check_cli_coli_nc_node3
			 ]}
    ].


break(_Config) -> 
    test_server:break("AA").


%%%--------------------------------------------------------------------
%%% @doc
%%%   create_to_pkg <br/>
%%% @end
%%%--------------------------------------------------------------------
create_to_pkg(Config) ->
    VnfType = get_vnf_type(), 
    ct:pal("## VnfType : ~p", [VnfType]),
    create_to_pkg(Config, VnfType).


create_to_pkg(Config, VnfType)->
    ct:pal(" Config: ~p ", [Config]),
    ct:pal("# Get needed data for create upgrade job Id "),
    ct:pal("# VnfType: ~p ", [VnfType]),
    FromInstanceId = get_from_instance_id(vnfm1),
    ToVnfd_Id = create_vnf_to_pkg_from_zip(Config, vnfm1, VnfType),
    ToVnfPkgId = get_to_pkg_id_for_ug_create(vnfm1, ToVnfd_Id),

    ct:pal("# Create job id operation for upgrade "),
    Ug_Job_Id = ug_create_job_id_op(vnfm1, FromInstanceId, ToVnfPkgId, ToVnfd_Id),
    ct:pal("Ug_Job_Id : ~p", [Ug_Job_Id]),
    
    ct:pal("# Get job status operation for an ongoing upgrade job "),
    get_job_status(vnfm1, Ug_Job_Id ),
    update_config_for_next_tc(Ug_Job_Id, ToVnfd_Id, ToVnfPkgId).

%%%--------------------------------------------------------------------
%%% @doc
%%%   prepare_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
prepare_vnf(Config) ->    
    ct:pal(" Config: ~p ", [Config]),
    {Ug_Job_Id, ToVnfd_Id, ToVnfPkgId} = get_ug_job_id_from_config(Config),

    ct:pal("# Prepare operation for upgrade "),
    ug_prepare(vnfm1, Ug_Job_Id),
    update_config_for_next_tc(Ug_Job_Id, ToVnfd_Id, ToVnfPkgId).

%%%--------------------------------------------------------------------
%%% @doc
%%%  verify_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
verify_vnf(Config) ->
    ct:pal(" Config: ~p ", [Config]),
    {Ug_Job_Id, ToVnfd_Id, ToVnfPkgId} = get_ug_job_id_from_config(Config),

    ct:pal("# Verify operation for upgrade "),
    ug_verify(vnfm1, Ug_Job_Id),
    update_config_for_next_tc(Ug_Job_Id, ToVnfd_Id, ToVnfPkgId).

%%%--------------------------------------------------------------------
%%% @doc
%%%   activate_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
activate_vnf(Config) ->
    ct:pal(" Config: ~p ", [Config]),
    {Ug_Job_Id, ToVnfd_Id, ToVnfPkgId} = get_ug_job_id_from_config(Config),

    ct:pal("# Activate operation for upgrade "),
    ug_activate(vnfm1, Ug_Job_Id),

    ct:pal("Set node state up on new instance after activate."),
    set_node_state_up(3), %% to get all logs.
    set_node_2_state_down(Config),

    timer:sleep(30000),
    ct:pal("Update ip address on node 3 config to get logs ."),
    update_ip_addr(vnfm1, 3, ToVnfd_Id, Config),
    update_config_for_next_tc(Ug_Job_Id, ToVnfd_Id, ToVnfPkgId).

%%%--------------------------------------------------------------------
%%% @doc
%%%   confirm_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
confirm_vnf(Config) ->
    ct:pal(" Config: ~p ", [Config]),
    {Ug_Job_Id, _ToVnfd_Id, _ToVnfPkgId} = get_ug_job_id_from_config(Config),

    %% FbTimeout = 1800,
    %% ug_confirm(vnfm1, Ug_Job_Id, FbTimeout),
    ug_confirm(vnfm1, Ug_Job_Id),
    get_job_status(vnfm1, Ug_Job_Id),

    ct:pal("####  Check after Upgrade #### "),
    check_ug_result_ok(vnfm1, Ug_Job_Id),
    
    %% set_node_2_state_down(Config),
    timer:sleep(30000),

    %% Delete ug_job_id on vnmf.
    ug_delete_job_id(vnfm1, Ug_Job_Id),

    ok.


get_ug_job_id_from_config(Config) ->
    {_tc_job, SavedConfList} = proplists:get_value(saved_config, Config),
    Ug_Job_Id = proplists:get_value(ug_job_id, SavedConfList),
    ToVnfd_Id = proplists:get_value(to_vnfd_id, SavedConfList),
    ToVnfPkgId = proplists:get_value(to_vnf_pkg_id, SavedConfList),
    ct:pal("Ug_Job_Id : ~p", [Ug_Job_Id]),
    ct:pal("ToVnfd_Id : ~p", [ToVnfd_Id]),
    ct:pal("ToVnfPkgId : ~p", [ToVnfPkgId]),
    {Ug_Job_Id, ToVnfd_Id, ToVnfPkgId}.
    

update_config_for_next_tc(Ug_Job_Id, ToVnfd_Id, ToVnfPkgId) ->
    NewConfig = [{ug_job_id,Ug_Job_Id},
		 {to_vnfd_id, ToVnfd_Id},
		 {to_vnf_pkg_id, ToVnfPkgId}
		],
    ct:pal("Add to NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.


update_ip_addr(Vnfm, NodeNr, ToVnfd_Id, _Config) ->
    %% %%%%% Get new vnf IP addresses %%%%%%%
    IpTupleList = get_vnf_ip_addresses_from_latest_vnf(Vnfm, ToVnfd_Id, _Config),

    %% %%%%% Update IP addresses in stp config %%%%%%%
    ct:pal("update_ip_addresses for new vnf after upgrade"),
    rct_node_state:update_ip_addresses(NodeNr, IpTupleList, _Config).


set_node_2_state_down(_Config) ->
    ct:pal("Set node state 2 to down due to upgrade success and new instance created."),
    set_node_state_down(2). %% After upgrade node 2 shall not be used.


%%%--------------------------------------------------------------------
%%% @doc
%%%  upgrade_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
upgrade_vnf(Config) ->
    ct:pal("# Get needed data for create upgrade job Id "),

    FromInstanceId = get_from_instance_id(vnfm1),
    %% %% {FromInstanceId, ToVnfPkgId, ToVnfDescId} =
    %% %% 	get_needed_data_for_ug_create(vnfm1),
    
    ToVnfd_Id = create_vnf_to_pkg_from_zip(Config, vnfm1),
    %% ToVnfd_Id = <<"CXP2010078_1A-R2A719">>,
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
    set_node_state_up(2),

    %% %%%%% Get new vnf IP addresses %%%%%%%
    IpTupleList = get_vnf_ip_addresses(vnfm1),

    %% %%%%% Update IP addresses in stp config %%%%%%%
    ct:pal("update_ip_addresses for new vnf after upgrade"),
    rct_node_state:update_ip_addresses(2, IpTupleList, Config),

    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_ug_activate_cancel_delete <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_activate_cancel_delete(Config) ->
    ct:pal("# Get needed data for create upgrade job Id "),
    %% {FromInstanceId, ToVnfPkgId, ToVnfDescId} =
    %% 	get_needed_data_for_ug_create(vnfm1),
    FromInstanceId = get_from_instance_id(vnfm1),
    ToVnfd_Id = create_vnf_to_pkg_from_zip(Config, vnfm1),

    ToVnfPkgId = get_to_pkg_id_for_ug_create(vnfm1, ToVnfd_Id),
    
    ct:pal("# Create job id operation for upgrade "),
    Ug_Job_Id = ug_create_job_id_op(vnfm1, FromInstanceId, ToVnfPkgId, ToVnfd_Id),
    ct:pal("# Ug_Job_Id : ~p", [Ug_Job_Id]),
    %% ct:pal("# Prepare operation for upgrade "),
    %% ug_prepare_no_state_check(vnfm1, Ug_Job_Id),

    %% get_job_status(vnfm1, Ug_Job_Id ),

    %% %%%%%% Prepare %%%%%%%%%%%%%%%%
    ct:pal("# Prepare operation for upgrade "),
    ug_prepare(vnfm1, Ug_Job_Id),


    %% %%%%%% Verify %%%%%%%%%%%%%%%%
    ct:pal("# Verify operation for upgrade "),
    ug_verify(vnfm1, Ug_Job_Id),

    %% %% ug_activate(vnfm1, Ug_Job_Id),

    %% %%%%%% trig Activate %%%%%%%%%%%%%%%%
    ct:pal("# Activate operation for upgrade "),
    Activate = rct_vnfm:ug_activate(vnfm1, Ug_Job_Id),
    ct:pal("### Ans from activate   : ~p", [Activate]),

    ct:pal("sleep 10 sec"),
    timer:sleep(10000),
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


    %% %% %%%%% Set Node State Up after cancel activate %%%%%%%
    %% ct:pal("Set node state up after cancel activate."),
    %% set_node_state_up(2),
    
    timer:sleep(5000).


%%%--------------------------------------------------------------------
%%% @doc
%%%  check_after_create_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
check_cli_coli_nc(_Config) ->
    check_cli_coli_nc(?cli2ipv4, ?cli2ipv6, ?coli2ipv4, ?coli2ipv6, ?nc2ipv4, ?nc2ipv6).
check_cli_coli_nc_node3(_Config) ->
    check_cli_coli_nc(?cli3ipv4, ?cli3ipv6, ?coli3ipv4, ?coli3ipv6, ?nc3ipv4, ?nc3ipv6).

    
check_cli_coli_nc(CliIpV4, CliIpV6, ColiIpV4, ColiIpV6, NcIpV4, NcIpV6) ->
    ct:pal("## Check CLI ##"),
    ct:pal("## Check cli using ipv4 ##"),
    ok = wait_for_cli(CliIpV4, 60000),
    ct:pal("## Check cli using ipv6 ##"),
    ok = wait_for_cli(CliIpV6, 60000),

    ct:pal("## Check COLI ##"),
    ct:pal("## Check coli using ipv4 ##"),
    ok = wait_for_coli(ColiIpV4, 60000),
    ct:pal("## Check coli using ipv6 ##"),
    ok = wait_for_coli(ColiIpV6, 60000),
    
    ct:pal("## Check NETCONF ##"),
    ct:pal("## Check Netconf using ipv4 ##"),
    ok = wait_for_netconf(NcIpV4, 60000),
    ct:pal("## Check Netconf using ipv6 ##"),
    ok = wait_for_netconf(NcIpV6, 60000).


check_vnf_cli_ipv4_ipv6(_Config) ->
    ct:pal("## Check cli using ipv4 ##"),
    ok = wait_for_cli(?cli2ipv4, 60000),
    ct:pal("## Check cli using ipv6 ##"),
    ok = wait_for_cli(?cli2ipv6, 60000).

check_vnf_nc_ipv4_ipv6(_Config) ->
    ct:pal("## Check Netconf using ipv4 ##"),
    ok = wait_for_netconf(?nc2ipv4, 60000),
    ct:pal("## Check Netconf using ipv6 ##"),
    ok = wait_for_netconf(?nc2ipv6, 60000).

check_vnf_coli_ipv4_ipv6(_Config) ->
    ct:pal("## Check coli using ipv4 ##"),
    ok = wait_for_coli(?coli2ipv4, 60000),
    ct:pal("## Check coli using ipv6 ##"),
    ok = wait_for_coli(?coli2ipv6, 60000).


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
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"INITIALIZED">>, 300000),
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
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"PREPARE_COMPLETED">>, 600000),
    Prepare.


%% ug_prepare_no_state_check(Vnfm, Ug_Job_Id) ->
%%     ct:pal("# Prepare operation for upgrade "),
%%     Prepare = rct_vnfm:ug_prepare(Vnfm, Ug_Job_Id),
%%     ct:pal("### Ans from prepare   : ~p", [Prepare]),
%%     Prepare.

%%%--------------------------------------------------------------------
%%% @doc
%%%  ug_verify <br/>
%%% @end
%%%--------------------------------------------------------------------
ug_verify(Vnfm, Ug_Job_Id) ->
    ct:pal("# Verify operation for upgrade "),
    Verify = rct_vnfm:ug_verify(Vnfm, Ug_Job_Id),
    ct:pal("### Ans from verify   : ~p", [Verify]),
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"PREPARE_COMPLETED">>, 600000),
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
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"WAITING_FOR_CONFIRM">>, 1200000), 
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
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"CONFIRM_COMPLETED">>, 1200000),
    Comfirm.


ug_confirm(Vnfm, Ug_Job_Id, FbTimeout) ->
    ct:pal("# Confirm operation for upgrade "),
    Comfirm = rct_vnfm:ug_confirm(Vnfm, Ug_Job_Id, FbTimeout),
    ct:pal("### Ans from confirm   : ~p", [Comfirm]),
    wait_for_exp_ug_state(Vnfm, Ug_Job_Id, <<"CONFIRM_COMPLETED">>, 600000),
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
    %% wait_for_exp_ug_state(vnfm1, Ug_Job_Id, <<"INITIALIZED">>, 600000), 
    wait_for_exp_ug_state(vnfm1, Ug_Job_Id, <<"PREPARE_COMPLETED">>, 600000),
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
    %% wait_for_node_state(3, up, 60000).
    wait_for_node_state(NodeNr, up, 60000).

%%%--------------------------------------------------------------------
%%% @doc
%%%  set_node_state_up <br/>
%%% @end
%%%--------------------------------------------------------------------
set_node_state_down(NodeNr) ->
    rct_node_state:set_state_down(NodeNr),
    ct:pal("wait_for_node_state down"),
    %% wait_for_node_state(3, up, 60000).
    wait_for_node_state(NodeNr, down, 60000).

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

get_vnf_ip_addresses_from_latest_vnf(Vnfm, ToVnfd_Id, _Config) ->
    ct:pal("GET VNF ID from latest upgraded vnf"),
    {ok, #{<<"vnfs">> := VnfMaps }} = rct_vnfm:get_vnfs(Vnfm),
    ct:pal("# # Vnfs maps data : ~p", [VnfMaps]),

    %% ToVnfd_Id = <<"CXP2010078_1A-R2A864">>,
    %% %% ToVnfd_Id is binary
    VnfMapData = get_map_with_to_vnfd_pkg_id(VnfMaps, ToVnfd_Id),
    ct:pal("# Wanted to vnf Map  : ~p", [VnfMapData]),

    VnfInstanceId = maps:get(<<"instanceId">>, VnfMapData),
    ct:pal("# # Vnf instanceId : ~p", [VnfInstanceId]),

    IpTupleList = wait_for_vnf_ips(Vnfm, VnfInstanceId, 300000),
    ct:pal("# # IpTupleList : ~p", [IpTupleList]),
    IpTupleList.


%%%--------------------------------------------------------------------
%%% @doc
%%%   wait_for_exp_ug_state <br/>
%%% @end
%%%--------------------------------------------------------------------
%% wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState) ->
%%     wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState, 300000). %% 5min
wait_for_exp_ug_state(Vnfm, Ug_Job_Id, ExpState, Timeout) when Timeout < 0 ->
    ct:pal("# Not rcvd exp state within time.  ~nExpected State : ~p , Vnfm : ~p , job_id : ~p", [ExpState, Vnfm, Ug_Job_Id]),
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

%%%--------------------------------------------------------------------
%%% @doc
%%%   get_from_instance_id <br/>
%%% @end
%%%--------------------------------------------------------------------
get_from_instance_id(Vnfm) ->
    ct:pal("GET VNF ID"),
    {ok, #{<<"vnfs">> := [VnfMapData] }} = rct_vnfm:get_vnfs(Vnfm),
    ct:pal("### Vnf data : ~p", [VnfMapData]),
    
    FromInstanceId = maps:get(<<"instanceId">>, VnfMapData),
    ct:pal("### From Vnf instanceId : ~p", [FromInstanceId]),
    FromInstanceId.

%%%--------------------------------------------------------------------
%%% @doc
%%%   create_vnf_to_pkg_from_zip <br/>
%%% @end
%%%--------------------------------------------------------------------
create_vnf_to_pkg_from_zip(Config, Vnfm) ->
    create_vnf_to_pkg_from_zip(Config, Vnfm, vrc).

create_vnf_to_pkg_from_zip(Config, Vnfm, VnfType) ->
    ct:log("##### Note this will upgrade to container_abc_2 in jenkins config #######"),
    ct:pal("## VnfType: ~p ", [VnfType]),
    ToContainer =  case VnfType of 
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
    Href = case ct:get_config({jenkins_config, ToContainer}) of
	       undefined ->
		   get_ct_arg(urlzip);
	       HREF ->
		   HREF
	   end,	          
    ct:pal("## Href : ~p", [Href]),

    ct:pal("### Get vnfPackages before create to package for upgrade.", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages}} = 
    	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### VnfPackages  : ~p", [VnfPackages]),

    {ok, #{<<"jobId">> := Job_Id,
    	   <<"vnfPackageId">> := VnfPackageId}} =
    	rct_vnfm:onboarding_vnf_package(Config, Vnfm, Href),
    timer:sleep(5000),
    ok = wait_for_pkg_status_success(Vnfm, Job_Id, 600000),
    ct:pal("### Job_Id  : ~p", [Job_Id]),
    ct:pal("### VnfPackageId  : ~p", [VnfPackageId]),

    Vnfd_Id = get_vnfd_id(Vnfm, VnfPackageId),
    ct:pal("### Vnfd_Id : ~p", [Vnfd_Id]),

    %% ct:pal("TestNodeNr: ~p", [TestNodeNr]),
    %% test_server:break("B"),
    
    timer:sleep(5000),
    ct:pal("### Get vnfPackages after create to package for upgrade ", []),
    {ok, #{<<"vnfPackages">> :=  VnfPackages2}} = 
    	rct_vnfm:get_all_vnf_packages(Vnfm),
    ct:pal("### All VnfPackages  : ~p", [VnfPackages2]),
    
    Vnfd_Id.

%%%--------------------------------------------------------------------
%%% @doc
%%%    get_to_pkg_id_for_ug_create <br/>
%%% @end
%%%--------------------------------------------------------------------
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

%%%--------------------------------------------------------------------
%%% @doc
%%%     get_map_with_to_vnfd_pkg_id <br/>
%%% @end
%%%--------------------------------------------------------------------
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
	    ct:log("Exp ToVnfd_Id : ~p, not match rcvd map : ~p", [ToVnfd_Id, C]),
	    get_map_with_to_vnfd_pkg_id(Tail, ToVnfd_Id)
    end.

%% %% ToPkg_Id is a string
%% get_instance_map_with_to_up_pkgid([], ToPkg_Id) ->
%%     ct:pal("# # No map match for vnfPackageId with  : ~p : ~p", [ToPkg_Id]),
%%     [];
%% get_instance_map_with_to_up_pkgid([H | Tail], ToPkg_Id) ->
%%     C = maps:get(<<"vnfPackageId">>, H),
%%     ct:pal("C: ~p , from map : ~p ", [C, H]),
%%     %% case binary_to_list(maps:get(<<"vnfdId">>, H)) of
%%     case maps:get(<<"vnfPackageId">>, H) of
%% 	ToVnfd_Id ->
%% 	    ct:pal("vnfPackageId: ~p, match in map : ~p", [ToVnfd_Id, H]),
%% 	    H;
%% 	_Nomatch ->
%% 	    get_instance_map_with_to_up_pkgid(Tail, ToPkg_Id)
%%     end.

%%%--------------------------------------------------------------------
%%% @doc
%%%     check_ug_result_ok <br/>
%%% @end
%%%--------------------------------------------------------------------
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%--------------------------------------------------------------------
%% %%% @doc
%% %%% @end
%% %%%--------------------------------------------------------------------
%% wait_for_vnfs(_Vnfm, Timeout) when Timeout < 0 ->
%%     ct:fail("wait_for_vnfs. Fail due to wait timeout!");
%% wait_for_vnfs(Vnfm, Timeout) ->
%%     ct:log("Vnfm : ~p", [Vnfm]),
%%     case rct_vnfm:get_vnfs(Vnfm) of
%% 	{ok, _Answ} ->
%% 	    ct:pal("# get_vnfs rcvd : ~p ", [_Answ]),
%% 	    ok;
%% 	_Other ->
%% 	    ct:log("# no VNFS recvd : ~p .~n sleep and try again.",[_Other]),
%% 	    timer:sleep(5000),
%% 	    wait_for_vnfs(Vnfm, Timeout-5000)
%%     end.

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
%% fix_str(List) ->
%%     fix_str(List, "\n").
%% fix_str(List, Token) ->
%%     FixedStr = string:tokens(List, Token),
%%     ct:log("Fixed str: ~n~p", [FixedStr]),
%%     FixedStr.


%%--------------------------------------------------------------------
%% @doc
%% get_vnfd_id
%% @end
%%--------------------------------------------------------------------
get_vnfd_id(Vnfm, VnfPackageId) ->
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
get_vnf_type() ->
    VnfType = case ct:get_config({jenkins_config, label_vrc_1}) of
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
				      ct:log("# vPP2 #"),
				      vpp2
			      end;
			  "rcf" ->
			      ct:log("# vPP #"),
			      vpp
		      end;
		  "rcf" ->
		      ct:log("# vRC #"),
		      vrc
	      end,	          
    ct:pal("### VnfType : ~p", [VnfType]),
    VnfType.

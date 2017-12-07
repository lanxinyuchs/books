
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:  itc_5g_predc_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R6A/R8A/R9A/4
%%% 
%%% @doc == TestSuite for ITC communication between RCF and BPU.==
%%% <br/><br/>
%%% @end

-module(config_cell_cloud_stp_2BPU_SUITE).
-vsn('/main/R6A/R8A/R9A/4').
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
%%% Rev	 Date	     Name	 What
%%% -----	 ---------   --------	 ------------------------
%%% R9A/1	 2017-03-29  eransbn	 Created
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
	 configure_cell_cloud_stp_2bpu_ipv6/1,
	 configure_cell_cloud_stp_2bpu_ipv4/1
	]).

-define(SD_PORT, "8301").


suite() -> 
    [{ct_hooks, [{rct_node_state, [{1,vnfm,up},{2,vnf,up},{3,vnf,up},{4,du,up},{5,du,up}]},%%vnfm,vrc, sd, bpu1, bpu2
		 {rct_cli, [{cli1_vnfm, [manual_connect]},{cli1_vrc, [manual_connect]},
		  	    {cli1_sd, [manual_connect]},{cli1_bpu, [manual_connect]},{cli2_bpu, [manual_connect]}]},
		 {rct_coli,[ {coli1_vnfm, [manual_connect]}, {coli1_vrc, [manual_connect]}, 
			     {coli1_sd, [manual_connect]}, {coli1_dus, [manual_connect]},{coli2_dus, [manual_connect]}]},
		 {rct_logging, [{1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all]},
				{2, log2, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all]},
				{3, log3, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all]},
				{4, log4, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all]},
				{5, log5, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], [get_all]}]},
		 {rct_netconf, [nc_vnfm1, nc_vrcs1, nc_sd1, nc_bpu1, nc_bpu2]}

		]}].

init_per_suite(Config) ->
    %% %% Get vrcs config it it not exist in stp.cfg.
    HW_SD = atom_to_list(ct:get_config({test_nodes,3})),
    HW_VRCS1 = atom_to_list(ct:get_config({test_nodes,2})),
    HW_BPU1 = atom_to_list(ct:get_config({test_nodes,4})),
    HW_BPU2 = atom_to_list(ct:get_config({test_nodes,5})),
    ct:pal("Hw SD rcf: ~p", [HW_SD]),
    ct:pal("Hw VRCS rcf: ~p", [HW_VRCS1]),
    ct:pal("Hw BPU1: ~p", [HW_BPU1]),
    ct:pal("Hw BPU2: ~p", [HW_BPU2]),
   
    [{hw_sd,HW_SD},{hw_vrcs1,HW_VRCS1},
     {hw_bpu1,HW_BPU1}, {hw_bpu2,HW_BPU2}| Config]. 

end_per_suite(_Config) ->    
    ok. 

init_per_group(_GroupName, Config) ->
    Config.
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestCase, Config)  ->
    ct:pal("# Start TC : ~p #", [TestCase]),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

all() -> 
    [ configure_R2_ipv6,
      clean].


groups() ->
    [{group_all, [sequence],[
			     configure_tn_ip6

			    ]}
    ].

%% break(_Config) ->
%%     test_server:break("AA").

configure_cell_cloud_stp_2bpu_ipv6(Config)->
    IpVersion = "6",
    Hw_bpu1 = proplists:get_value(hw_bpu1, Config),
    Hw_bpu2 = proplists:get_value(hw_bpu2, Config),
    Hw_vrc1 = proplists:get_value(hw_vrcs1, Config),
    Hw_sd = proplists:get_value(hw_sd, Config),


    %% Configure bpu 1
    ok = config_cloud_stp_lib:set_managedElementID(cli1_bpu, Hw_bpu1),
    ok = config_cloud_stp_lib:create_mo_sectorCarrier(cli1_bpu,Hw_bpu1 ),
    ok = config_cloud_stp_lib:configure_service_discovery(cli1_bpu, IpVersion, Hw_bpu1, Hw_sd, ?SD_PORT, Config), 
    ok = config_cloud_stp_lib:configure_bpfunction(cli1_bpu, IpVersion, Hw_bpu1),
    %% Configure bpu 2
    ok = config_cloud_stp_lib:set_managedElementID(cli2_bpu, Hw_bpu2),
    ok = config_cloud_stp_lib:create_mo_sectorCarrier(cli2_bpu, Hw_bpu2),
    ok = config_cloud_stp_lib:configure_service_discovery(cli2_bpu, IpVersion, Hw_bpu2, Hw_sd, ?SD_PORT, Config), 
    ok = config_cloud_stp_lib:configure_bpfunction(cli2_bpu, IpVersion, Hw_bpu2),

    %%Configure VRCS
    ok = config_cloud_stp_lib:configure_tn(Hw_vrc1, cli1_vrc, IpVersion, Config),
    config_cloud_stp_lib:create_mo_sctpProfile(nc_vrcs1, "1"),
    config_cloud_stp_lib:create_mo_sctpEndpoint(nc_vrcs1, cli1_vrc, IpVersion, Hw_vrc1),
    config_cloud_stp_lib:configure_service_discovery(cli1_vrc, IpVersion ,"1", Hw_sd, ?SD_PORT, Config),
    config_cloud_stp_lib:create_mo_enbFunction(nc_vrcs1, cli1_vrc, IpVersion, "1"),

    %%Configure VRCS bpu 1
    config_cloud_stp_lib:create_mo_EUtranCellFDD(nc_vrcs1, cli1_bpu, Hw_bpu1, "1"),
    %%Configure VRCS bpu 2
    config_cloud_stp_lib:create_mo_EUtranCellFDD(nc_vrcs1, cli2_bpu, Hw_bpu2, "2"),
 
    %%Deblock cell 1(bpu1) and 2 (bpu2)
    config_cloud_stp_lib:set_adminstate_cell(cli1_vrc,"1","UNLOCKED"),
    config_cloud_stp_lib:set_adminstate_cell(cli1_vrc,"2","UNLOCKED"),
    %%Check XCM connection
    %% check_ip_registrate_sd(coli1_vrc, SdIP)

    ok.

configure_cell_cloud_stp_2bpu_ipv4(Config)->
    IpVersion = "4",
    Hw_bpu1 = proplists:get_value(hw_bpu1, Config),
    Hw_bpu2 = proplists:get_value(hw_bpu2, Config),
    Hw_vrc1 = proplists:get_value(hw_vrcs1, Config),
    Hw_sd = proplists:get_value(hw_sd, Config),

    %% Configure bpu 1
    ok = config_cloud_stp_lib:set_managedElementID(cli1_bpu, Hw_bpu1),
    ok = config_cloud_stp_lib:create_mo_sectorCarrier(cli1_bpu,Hw_bpu1 ),
    ok = config_cloud_stp_lib:configure_service_discovery(cli1_bpu, IpVersion, Hw_bpu1, Hw_sd, ?SD_PORT, Config), 
    ok = config_cloud_stp_lib:configure_bpfunction(cli1_bpu, IpVersion, Hw_bpu1),
    %% Configure bpu 2
    ok = config_cloud_stp_lib:set_managedElementID(cli2_bpu, Hw_bpu2),
    ok = config_cloud_stp_lib:create_mo_sectorCarrier(cli2_bpu, Hw_bpu2),
    ok = config_cloud_stp_lib:configure_service_discovery(cli2_bpu, IpVersion, Hw_bpu2, Hw_sd, ?SD_PORT, Config), 
    ok = config_cloud_stp_lib:configure_bpfunction(cli2_bpu, IpVersion, Hw_bpu2),

    %%Configure VRCS
    ok = config_cloud_stp_lib:configure_tn(Hw_vrc1, cli1_vrc, IpVersion, Config),
    config_cloud_stp_lib:create_mo_sctpProfile(nc_vrcs1, "1"),
    config_cloud_stp_lib:create_mo_sctpEndpoint(nc_vrcs1, cli1_vrc, IpVersion, Hw_vrc1),
    config_cloud_stp_lib:configure_service_discovery(cli1_vrc, IpVersion ,"1", Hw_sd, ?SD_PORT, Config),
    config_cloud_stp_lib:create_mo_enbFunction(nc_vrcs1, cli1_vrc, IpVersion, "1"),

    %%Configure VRCS bpu 1
    config_cloud_stp_lib:create_mo_EUtranCellFDD(nc_vrcs1, cli1_bpu, Hw_bpu1, "1"),
    %%Configure VRCS bpu 2
    config_cloud_stp_lib:create_mo_EUtranCellFDD(nc_vrcs1, cli2_bpu, Hw_bpu2, "2"),

    %%Deblock cell 1(bpu1) and 2 (bpu2)
    config_cloud_stp_lib:set_adminstate_cell(cli1_vrc,"1","UNLOCKED"),
    config_cloud_stp_lib:set_adminstate_cell(cli1_vrc,"2","UNLOCKED"),

    %%Check XCM connection
    %% check_ip_registrate_sd(coli1_vrc, SdIP)

    ok.

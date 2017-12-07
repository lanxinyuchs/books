%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_test_lib.erl %
%%% @author eransbn
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/3
%%%
%%% == support lib when testing upgrade mechanism. ==
%%% <br/>
%%%
%%%
%%%

-module(config_cloud_stp_lib).
%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2013-12-13 etxivri     Created

%%% ----------------------------------------------------------

% -compile([export_all]).

-export([
	 configure_bpfunction/3,
	 set_managedElementID/2,
	 create_mo_sectorCarrier/2,
	 configure_service_discovery/6,
	 configure_tn/4,
	 create_mo_sctpProfile/2,
	 create_mo_sctpEndpoint/4,
	 create_mo_EUtranCellFDD/4,
	 create_mo_enbFunction/4,
	 create_mo_TermPointToMme/1,
	 set_adminstate_cell/3
    ]).

%%-define(Sleep, 1000).

%%% ===========================================================================
%%%
%%%  configure_bpfunction -> ok
%%%
%%% ===========================================================================
configure_bpfunction(CLI, IpVersion, HwId)->
    ct:log("Create mo bpfunction on ~p",[HwId]),
    IpAddressLdn = get_ip_address_LDN(CLI, IpVersion),

    ok = rct_cli:connect(CLI),
    {ok, A} =  rct_cli:send(CLI,"configure"),
    {ok, B} =   rct_cli:send(CLI, "ManagedElement="++HwId++",BpFunction=1"),
    {ok, C} =   rct_cli:send(CLI, "ManagedElement="++HwId++",BpFunction=1,"
			     "midhaulIpAddressRef=ManagedElement="++HwId++","++ IpAddressLdn),
    rct_cli:send(CLI,"commit"),
    ok = rct_cli:disconnect(CLI),
    CheckList = [A,B,C],
    lists:foreach(fun(ConfRes) ->
			  case re:run(ConfRes, "ERROR") of
			      {match,_} ->
				  ct:pal("## Sector config failed: ~n~p", [ConfRes]),
				  ct:fail("## Sector config failed: no need to continue!!");
			      nomatch ->
				  ok
			  end
		  end, CheckList).


%%% ===========================================================================
%%%
%%%  create_mo_sectorCarrie -> ok
%%%
%%% ===========================================================================
create_mo_sectorCarrier(CLI,HwId) ->
    ct:log("Create mo sectorCarrier on ~p",[HwId]),
    ok = rct_cli:connect(CLI),
    {ok, A} =  rct_cli:send(CLI,"configure"),
    {ok, B} =   rct_cli:send(CLI, "ManagedElement="++HwId++",SectorFunction=1"),
    {ok, C} =   rct_cli:send(CLI, "ManagedElement="++HwId++",SectorFunction=1,SectorCarrier=1"),
    {ok, D} =   rct_cli:send(CLI, "ManagedElement="++HwId++",SectorFunction=1,SectorCarrier=1,"
			     "sectorFunctionRef=ManagedElement="++HwId++",NodeSupport=1,SectorEquipmentFunction=1"),

    rct_cli:send(CLI,"commit"),
    ok = rct_cli:disconnect(CLI),

    CheckList = [A,B,C,D],
    lists:foreach(fun(ConfRes) ->
			  case re:run(ConfRes, "ERROR") of
			      {match,_} ->
				  ct:pal("## Sector config failed: ~n~p", [ConfRes]),
				  ct:fail("## Sector config failed: no need to continue!!");
			      nomatch ->
				  ok
			  end
		  end, CheckList).
%%% ===========================================================================
%%%
%%%  create_mo_sctpProfile -> ok
%%%
%%% ===========================================================================
create_mo_sctpProfile(Nc1, HW)->
    ct:log("Create mo sctpProfile"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],[HW]},
			 {'Transport',
			  [{xmlns,"urn:com:ericsson:ecim:Transport"}],
			  [{transportId,[],["1"]},
			   {'SctpProfile',
			    [{xmlns,"urn:com:ericsson:ecim:SctpProfile"}],
			    [{sctpProfileId,[],["1"]},
			     {heartbeatInterval,[],["30000"]}
			    ]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
%%% ===========================================================================
%%%
%%%  create_mo_sctpEndpoin -> ok
%%%
%%% ===========================================================================
create_mo_sctpEndpoint(Nc1, CLI, IpVersion, HW)->
    ct:log("Create mo sctpEndpoint"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),

		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],[HW]},
			 {'Transport',
			  [{xmlns,"urn:com:ericsson:ecim:Transport"}],
			  [{transportId,[],["1"]},    
			   {'SctpEndpoint',
			    [{xmlns,"urn:com:ericsson:ecim:SctpEndpoint"}],
			    [{sctpEndpointId,[],["1"]},
			     {sctpProfile,[],["ManagedElement=1,Transport=1,SctpProfile=1"]},
			     {portNumber,[],["36422"]},
			     %%  {localIpAddress,[],["ManagedElement=1,Transport=1,Router=1,InterfaceIPv"++IpVersion ++"=1,AddressIPv"++IpVersion ++"=1"]}  %%TODO:read in 
			     {localIpAddress,[],["ManagedElement=1,"++ get_ip_address_LDN(CLI, IpVersion)]}
			    ]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
%%% ===========================================================================
%%%
%%%  create_mo_enbFunction -> ok
%%%
%%% ===========================================================================
create_mo_enbFunction(Nc1, CLI, IpVersion, HW)->
    ct:log("Create mo enbFunction"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],[HW]},
			 {'ENodeBFunction',
			  [{xmlns,"urn:com:ericsson:ecim:ENodeBFunction"}],
			  [{eNodeBFunctionId,[],["1"]},
			   {eNBId,[],["1"]},
			   {eNodeBPlmnId,
			    [{struct,"PlmnIdentity"}],
			    [{mcc,[],["262"]},{mnc,[],["80"]},{mncLength,[],["2"]}]},
			   {midhaulIpAddressRef,[],
			    %% ["ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1"]},	
			    ["ManagedElement=1," ++ get_ip_address_LDN(CLI, IpVersion) ]},	     
			   {sctpRef,[],["ManagedElement=1,Transport=1,SctpEndpoint=1"]}
			  ]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
%%% ===========================================================================
%%%
%%%  create_mo_EUtranCellFDD -> ok
%%%
%%% ===========================================================================
create_mo_EUtranCellFDD(Nc1, CLI_BPU, Hw_BPU, CellId)->
    ct:log("Create mo EUtranCellFDD for HW ~p",[Hw_BPU]),

    SectorCarrierLdnBpu0 = get_sectorCarrier_LDN(CLI_BPU),
    SectorCarrierLdnBpu = "ManagedElement="++Hw_BPU++"," ++SectorCarrierLdnBpu0,
    ct:pal("SectorCarrierLdnBpu ~p",[SectorCarrierLdnBpu]),
    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),

		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],["1"]},
			 {'ENodeBFunction',
			  [{xmlns,"urn:com:ericsson:ecim:ENodeBFunction"}],
			  [{eNodeBFunctionId,[],["1"]},
			   {'EUtranCellFDD',
			    [{xmlns,"urn:com:ericsson:ecim:EUtranCellFDD"}],
			    [{eUtranCellFDDId,[],[CellId]},
			     {earfcndl,[],["1300"]},
			     {earfcnul,[],["19300"]},
			     {cellId,[],[CellId]},
			     {cellSimulated,[],["true"]},
			     {physicalLayerCellIdGroup,[],["67"]},
			     {physicalLayerSubCellId,[],["0"]},
			     {tac,[],["30742"]},
			     {sectorCarrierRemoteRef,[],
			      [SectorCarrierLdnBpu]},
			     {frameStartOffset,
			      [{struct,"FrameStartOffset"}],
			      [{subFrameOffset,[],["0"]}]},
			     {dlChannelBandwidth,[],["20000"]},
			     {ulChannelBandwidth,[],["20000"]},
			     {changeNotification,
			      [{struct,"ChangeNotificationSIBs"}],[]},
			     {siPeriodicity,
			      [{struct,"PeriodicitySI"}],[]},
			     {systemInformationBlock3,
			      [{struct,"SIB3"}],[]},
			     {userLabel,[],["Cell_1"]}

			    ]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
create_mo_TermPointToMme(Nc1)->
    ct:log("Create mo TermPointToMme"),

    F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
		 ok =
		     ct_netconfc:edit_config(
		       Nc1,running,
		       {'ManagedElement',
			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			[{managedElementId,[],["1"]},
			 {'ENodeBFunction',
			  [{xmlns,"urn:com:ericsson:ecim:ENodeBFunction"}],
			  [{eNodeBFunctionId,[],["1"]},
			   {'TermPointToMme',[],
			    [{termPointToMmeId,[],["1"]},
			     {ipAddress1,[],["10.45.132.3"]}
			    ]}]}]}),
		 ok = ct_netconfc:close_session(Nc1, 10000)
	end,
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(Nc1, 10000),
	    ct:fail(Reason)
    end.
%%% ===========================================================================
%%%
%%%  set_managedElementID -> ok
%%%
%%% ===========================================================================
set_managedElementID(CLI, Hw)->
    ct:log("Set ManagedElement=~p",[Hw]),
    ok = rct_cli:connect(CLI),
    {ok, A} =  rct_cli:send(CLI,"configure"),
    {ok, B} =   rct_cli:send(CLI, "ManagedElement=1,networkManagedElementId="++Hw),
    rct_cli:send(CLI,"commit"),
    ok = rct_cli:disconnect(CLI),

    CheckList = [A,B],
    lists:foreach(fun(ConfRes) ->
			  case re:run(ConfRes, "ERROR") of
			      {match,_} ->
				  ct:pal("##  failed: ~n~p", [ConfRes]),
				  ct:fail("## failed: no need to continue!!");
			      nomatch ->
				  ok
			  end
		  end, CheckList).


%%% ===========================================================================
%%%
%%%   configure_service_discovery -> ok
%%%
%%% ===========================================================================
configure_service_discovery(CLI, IpVersion, HwId, HW_SD, SD_port, _Config)->
    ct:log("Create mo serviceDiscovery"),
    case IpVersion of
	"4" -> 
	    [{ssh, Ip_SD},_,_,_] = ct:get_config({list_to_atom(HW_SD), ssh_lmt_ipv4});
	"6" ->
	    [{ssh, Ip_SD},_,_,_] = ct:get_config({list_to_atom(HW_SD), ssh_lmt_ipv6})

    end,
    AddressLdn = get_ip_address_LDN(CLI, IpVersion),    

    ok = rct_cli:connect(CLI),
    {ok, A} =  rct_cli:send(CLI,"configure"),
    {ok, B} =   rct_cli:send(CLI, "ManagedElement="++HwId++",NodeSupport=1,ServiceDiscovery=1"),
    {ok, C} =   rct_cli:send(CLI, "ManagedElement="++HwId++",NodeSupport=1,ServiceDiscovery=1,gsdsAddress host=" ++Ip_SD ),
    {ok, D} =  rct_cli:send(CLI, "ManagedElement="++HwId++",NodeSupport=1,ServiceDiscovery=1,gsdsAddress port=" ++SD_port ),
    {ok, E} =   rct_cli:send(CLI, "ManagedElement="++HwId++",NodeSupport=1,ServiceDiscovery=1,localAddress=" ++"\"" ++"ManagedElement="++HwId ++"," ++  AddressLdn  ++"\"" ),

    rct_cli:send(CLI,"commit"),


    ok = rct_cli:disconnect(CLI),
    CheckList = [A,B,C,  
		 D,E],
    lists:foreach(fun(ConfRes) ->
			  case re:run(ConfRes, "ERROR") of
			      {match,_} ->
				  ct:pal("## ServiceDiscovery configure failed: ~n~p", [ConfRes]),
				  ct:fail("## ServiceDiscovery configure failed: no need to continue!!");
			      nomatch ->
				  ok
			  end
		  end, CheckList).
%%% ===========================================================================
%%%
%%%   configure_tn -> ok
%%%
%%% ===========================================================================
configure_tn(HW, CLI, IpVersion, _Config) ->
    ct:log("Configure TN"),

    case IpVersion of

	"4" ->
	    [{ssh, Rcf_Tn_Ip}, _, {netmask, Rcf_Tn_Mask}, {gateway, Gateway}] =
		ct:get_config({list_to_atom(HW),om_ran_ipv4});
	"6" -> 
	       [{ssh, Rcf_Tn_Ip}, _, {netmask, Rcf_Tn_Mask}, {gateway, Gateway}] = 
			ct:get_config({list_to_atom(HW),om_ran_ipv6})
    end,

    ct:log("Hw : ~p", [HW]),
    ct:log("Rcf_Tn_Ip : ~p", [Rcf_Tn_Ip]),
    ct:log("Rcf_Tn_Mask : ~p", [Rcf_Tn_Mask]),
    ct:log("Gateway : ~p", [Gateway]),
    ct:pal("### Configure Transport network"),
    ok = rct_cli:connect(CLI),
    rct_cli:send(CLI,"configure"),
    {ok, A} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A"),
    {ok, B} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A,administrativeState=UNLOCKED"),
    %% {ok, _} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A,admOperatingMode=1G_FULL"),
    {ok, C} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A,index=1"),

    {ok, D} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1"),
    {ok, E} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,InterfaceIPv" ++IpVersion ++  "=1"),
    {ok, F} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,InterfaceIPv"++IpVersion ++  "=1,encapsulation=ManagedElement=1,Transport=1,VirtualEthernetPort=TN_A"),
    {ok, G} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,InterfaceIPv"++IpVersion++"=1,AddressIPv"++IpVersion ++  "=1"),
    {ok, H} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,InterfaceIPv"++IpVersion++"=1,AddressIPv"++IpVersion ++  "=1,address=" ++ Rcf_Tn_Ip ++ "/" ++ Rcf_Tn_Mask),

    {ok, I} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv" ++IpVersion++ "Static=1"),
    {ok, J} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv"++IpVersion++"Static=1,Dst=default"),
    case IpVersion of
	"4" ->   {ok, K} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv"++IpVersion ++"Static=1,Dst=default,dst=0.0.0.0/0"),
		 {ok, L} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv"++IpVersion++"Static=1,Dst=default,NextHop=1"),
		 {ok, M} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv"++IpVersion++"Static=1,Dst=default,NextHop=1,address=" ++ Gateway);
	"6" ->
	    {ok, K} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv"++IpVersion ++"Static=1,Dst=default,dst=::/0"),
	    {ok, L} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv"++IpVersion++"Static=1,Dst=default,NextHop=1"),
	    {ok, M} = rct_cli:send(CLI, "ManagedElement=1,Transport=1,Router=1,RouteTableIPv"++IpVersion++"Static=1,Dst=default,NextHop=1,address=" ++ Gateway)
    end,
    {ok, W} = rct_cli:send(CLI, "validate"),
    rct_cli:send(CLI,"commit"),
    ok = rct_cli:disconnect(CLI),

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


get_ip_address_LDN(CLI, IpVersion)->
    ok = rct_cli:connect(CLI),
    Reply =
	rct_cli:send(CLI, "show-mib -v", {"Transport=(.*),Router=(.*),InterfaceIPv"++IpVersion++"=(.*),AddressIPv"++IpVersion++"=(.*)", 
					  [global, {capture,first, list}]}, noprint, 30000), 
    Matched =  case Reply of
		   {ok, {_Reply, [[Matched1],_]}} -> Matched1;
		   {ok, {_Reply, [[Matched2] ]}} -> Matched2;
		   {error, Error}->
		       ct:fail(Error)
	       end,
    AddressLdn = lists:droplast(Matched),
    ok = rct_cli:disconnect(CLI),
    AddressLdn.  

get_sectorCarrier_LDN(CLI)->
    ok = rct_cli:connect(CLI),
    Reply =
	rct_cli:send(CLI, "show-mib -v", {"SectorFunction=(.*),SectorCarrier=(.*)", 
					  [global, {capture,first, list}]}, noprint, 30000), 
    Matched =  case Reply of
		   {ok, {_Reply, [[Matched1],_]}} -> Matched1;
		   {ok, {_Reply, [[Matched2] ]}} -> Matched2;
		   {error, Error}->
		       ct:fail(Error)
	       end,
    AddressLdn = lists:droplast(Matched),
    ok = rct_cli:disconnect(CLI),
    ct:log("SectorCarrier LDN ~p",[AddressLdn]),
    AddressLdn.
set_adminstate_cell(CLI, CellId, Value)->
    ct:log("Set administrativeState to ~p on cellid: ~p",[Value,CellId]),
    ok = rct_cli:connect(CLI),
    rct_cli:send(CLI,"configure"),
    {ok, A} = rct_cli:send(CLI, "ManagedElement=1,ENodeBFunction=1,EUtranCellFDD="++CellId++",administrativeState="++Value),
    rct_cli:send(CLI,"commit"),
    ok = rct_cli:disconnect(CLI),

    CheckList = [A],
    lists:foreach(fun(ConfRes) ->
			  case re:run(ConfRes, "ERROR") of
			      {match,_} ->
				  ct:pal("##cell configure failed: ~n~p", [ConfRes]),
				  ct:fail("##cell configure failed: no need to continue!!");
			      nomatch ->
				  ok
			  end
		  end, CheckList),

    ok.



%% connect_ssh() ->
%%     ct:pal("### Connecting to RCF"),
%%     ok = rct_ssh:connect(ssh_rcf),
%%     ct:pal("### Connecting to BPU"),
%%     ok = rct_ssh:connect(ssh_bpu),
%%     ok.

%% disconnect_ssh() ->
%%     ok = rct_ssh:disconnect(ssh_rcf),
%%     ok = rct_ssh:disconnect(ssh_bpu).

%% setup_itc(Node) ->



%% reboot_bpu() ->
%%     ct:pal("### Rebooting BPU."),
%%     ok = rct_rs232:login(console_bpu),
%%     %% {ok, ErlNode} = rct_rpc:get_erlnode(rpc_bpu),
%%     %% net_kernel:disconnect(ErlNode),
%%     ct_telnet:send(console_bpu, "reboot"), 
%%     {ok,_} = ct_telnet:expect(console_bpu, "Restarting system",
%% 			       [{timeout,180000}, no_prompt_check]),
%%     {ok,_} = ct_telnet:expect(console_bpu, "login:", 
%% 			       [{timeout,60000}, no_prompt_check]),
%%     ok.

%% wait_for_tn_after_reboot(0, _) ->
%%     ct:pal("### ERROR: Timeout waiting for TN to come up after reboot!"),
%%     nok;
%% wait_for_tn_after_reboot(Timer, Node) ->
%%     {ok, Namespace_list} = ct_ssh:exec(Node, "ip netns list", 5000),
%%     case Namespace_list of
%%  [] -> timer:sleep(1000),
%% 	ct:pal("### Checking namespace. Timer = ~p ms ~nNamespace not up yet, trying again...", [Timer]),
%% 	wait_for_tn_after_reboot(Timer - 1000, Node);
%%  _ -> ok
%%     end.


%% create_mo_sectorFunction(Nc1)->
%%     F = fun() -> {ok,_} = ct_netconfc:open(Nc1, [{port, 2022}]),
%% 		 ok =
%% 		     ct_netconfc:edit_config(
%% 		       Nc1,running,
%% 		       {'ManagedElement',
%% 			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 			[{managedElementId,[],["1"]},
%% 			 {'SectorFunction',
%% 			  [{xmlns,"urn:com:ericsson:ecim:SectorFunction"}],
%% 			  [{sectorFunctionId,[],["1"]}

%% 			    ]}]}),
%% 		ok = ct_netconfc:close_session(Nc1, 10000)
%% 	end,
%%     try F()
%%     catch
%% 	_:Reason ->
%% 	    ct_netconfc:close_session(Nc1, 10000),
%% 	    ct:fail(Reason)
%%     end.
%% create_mo_SectorCarrier(Nc1)->
%%     F = fun() -> {ok,_} = ct_netconfc:open(Nc1,[]),
%% 		 ok =
%% 		     ct_netconfc:edit_config(
%% 		       Nc1,running,
%% 		       {'ManagedElement',
%% 			[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 			[{managedElementId,[],["1"]},
%% 			 {'SectorFunction',
%% 			  [{xmlns,"urn:com:ericsson:ecim:SectorFunction"}],
%% 			  [{sectorFunctionId,[],["1"]},
%% 			   {'SectorCarrier',
%% 			    [{xmlns,"urn:com:ericsson:ecim:SectorCarrier"}],
%% 			    [{sectorCarrierId,[],["1"]},
%% 			     {sectorFunctionRef,[],
%% 			      ["NodeSupport=1,SectorEquipmentFunction=1"]}		 
%% 			    ]}]}]}),
%% 		 ok = ct_netconfc:close_session(Nc1, 10000)
%% 	end,
%%     try F()
%%     catch
%% 	_:Reason ->
%% 	    ct_netconfc:close_session(Nc1, 10000),
%% 	    ct:fail(Reason)
%%     end.


%% get_DlFreq_from_band(Fqband)->
%%     BandFreqList = [{"1","10700"},{"2","9800"},{"3","1338"},{"4","1638"},
%% 		    {"5","4408"},{"6","4400"},{"7","2400"},{"8","3013"},
%% 		    {"9","9312"},{"10","3250"},{"11","3750"},{"12","3873"},
%% 		    {"13","4030"},{"14","4130"},{"19","738"},{"20","4575"},
%% 		    {"21","887"},{"22","4850"},{"25","5263"},{"26","5838"}],
%%     case proplists:get_value(Fqband,BandFreqList, undefined) of
%% 	undefined -> ct:fail("Radio Band ~p not defined",[Fqband]);
%% 	UarfcnDl -> UarfcnDl
%%     end.

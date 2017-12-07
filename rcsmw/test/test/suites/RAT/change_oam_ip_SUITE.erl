%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	change_oam_ip_SUITE.erl %
%%% @author etrucvu
%%% @version /main/R11A/4

%%% 
%%% @doc == Test Suite 
%%% This Test Suite needs a full UP with TN, running on target
%%% <br/><br/>
%%% rct_netconf, snmp are used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end

-module(change_oam_ip_SUITE).
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
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
%%% -----      ----------  --------    -----------------------
%%% Rx         2017-08-11  etrucvu     Created
%%% Rx	       2017-08-31  uabhten etrucvu 	Two TCs works
%%% R11A/2     2017-09-05  etrucvu     Fixed after code review
%%% R11A/3     2017-09-05  etrucvu     Change to muexpert
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0
	]).

-export([check_changed_ipv4/1, check_changed_ipv6/1]).

%%% ----------------------------------------------------------
%%% #2.2   DEFINED MACROS
%%% ----------------------------------------------------------

-define(ME_ID, "1").
-define(FRU, "1").
-define(ROUTER, "OAM").
-define(ROUTER_ALT, "OAM_ALT").
-define(TN_PORT, "TN_A").
-define(TN_MASK, "24").
-define(TN_MASK_ALT, "24").
-define(TN_GW, "10.67.226.1").
-define(TN_GW_ALT, "10.67.233.1").
-define(TN_IPV6_NET, "2001:1b70:6281:f100").
-define(TN_IPV6_GW, ?TN_IPV6_NET ++ "::" ++ "2").
-define(TN_IPV6_MASK, "64").

-define(FEATURE_IPV6, "CXC4040006").
-define(LDAP_IPV6, "2001:1b70:6282:b280::150").
-define(LDAP_IPV4, "10.68.101.150").

-define(NETCONF_DELETE, [{'xmlns:nc', "urn:ietf:params:xml:ns:netconf:base:1.0"},
			 {'nc:operation', "delete"}]).
-define(NETCNF, nc1).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_netconf, rct_snmpmgr, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks,
      [{rct_rpc, rpc1},
       %%{rct_netconf,{?NETCNF, expert_auth}},	   
	   {rct_netconf, [{1, ?NETCNF, ssh_lmt_ipv4,[{user,"muexpert"},{password,"muexpert"}], html}]},
       {rct_snmpmgr, snmp1},
       {rct_core, []},
       {cth_conn_log,[]},
       {rct_logging,
              {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]
                              }}]}}]
     }].

%% @hidden
init_per_suite(InConfig) ->
    ct:pal("Preparing ct-loopdata (Config)"),
    Config = prepare_config(InConfig), % add things from stp.cfg
    ct:log("InConfig: ~p~nPrepared_config:~p~n", [InConfig, Config]),

    case get_required_node_config(Config) of
	{ok, UpdatedConfig} ->
	    UpdatedConfig;
	{error, Reason} ->
	    {skip, Reason}
    end.

%% @hidden
end_per_suite(_Config) ->	
    ok.

%% @hidden
init_per_testcase(_TC, Config) ->
    Config.

%% @hidden
end_per_testcase(_TC, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [check_changed_ipv4, check_changed_ipv6].

%%% #---------------------------------------------------------
%%% #3   CODE FOR TESTCASES
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.1   TESTCASES
%%% #---------------------------------------------------------
%%=============================================================
%% @doc 
%% TC for changing node address to new IPv4 
%% @end
%%=============================================================
check_changed_ipv4(Config) ->
    %% remove OAM_ALT configuration, VlanPort=OAM_ALT if any
    ok = rm_oam_alt_if_any(Config),

    %% Create a new VlanPort MO associated with the new IP address; 
    ok = create_vlanport_alt(Config),

    %% Create new NextHop MO for routing to this IP, under Router=OAM
    RouterOam = ?config(router, Config),
    ok = create_alt_routing_table_under_router(Config,RouterOam),	

    %% Trigger the test
    ct:pal("Use the new IP address + new VlanPort to modify the node's "
	   "configuration \n"),
    ok = modify_addressIpv4(Config),

    ct:pal("Wait for the CstnUpdate(new_IP) trap to be sent."
	   " Assert that the trap contains the new IP \n"),
    Timeout = 180,
    IpAddress = ?config(tn_ip_alt, Config),
    ok = wait_for_eriChangeIPAddressEvent(Timeout, IpAddress),	

    %% Restore the node's configuration to the original state 
    OldVlanPortLdn = ?config(orig_vlan_port_ldn, Config),
    OldIpAddress = ?config(orig_ipv4_address_val, Config),

    ok = modify_addressIpv4(Config, OldVlanPortLdn, OldIpAddress),
    Timeout = 180,
    ok = wait_for_eriChangeIPAddressEvent(Timeout, OldIpAddress),

    ok = rm_vlanport_and_nexthop_alt(Config),

    %% If deleted before, recreate Router=OAM_ALT, NextHop and OAP for it
    case ?config(orig_is_vlanport_alt_configured,Config) of
	true ->	
	    ok = create_oam_alt(Config);
	false ->
	    ok
    end.

%%=============================================================
%% @doc 
%% TC for changing node address from IPv4 to IPv6 
%% @end
%%=============================================================
check_changed_ipv6(Config) ->

    Ipv6Address = configure_ipv6_transport(Config),

    ct:pal("Set OamAccessPoint=1::accessPoint to AddressIpv6 MO and wait for"
	   " eriChangeIPAddressEvent"),
    ok = point_oap_to_addressIpv6(Config),

    Timeout = 180,	
    ok = wait_for_eriChangeIPAddressEvent(Timeout, Ipv6Address),		

    ct:pal("Restore the node's configuration to the original state \n"),
    ok = point_oap_to_addressIpv4(Config),		

    %% remove the AddressIpv6 MO
    ok = rm_ipv6_address(Config).


%%% #---------------------------------------------------------
%%% #3.2   INTERNAL METHODS
%%% #---------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Add needed parameters to Config -proplist,
%% the majority of environment hardcoding is here.
%% @spec prepare_config(Config) -> New_config
%% @end
%%--------------------------------------------------------------------
prepare_config(Config) ->
    Dut 		= ?config(1, ct:get_config(test_nodes)),
    DutProps 		=  ct:get_config(Dut),
    ConfigAttribute 	= list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv4"),
    ConfigAttributeAlt 	= list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv4_alt"),    
    TnIp 		= ?config(ssh, ?config(ConfigAttribute, DutProps)),
    TnVlanId 		= ?config(vlan, ?config(ConfigAttribute, DutProps)),
    TnIpv6 		= get_ipv6_from_config(DutProps),	
    TnVlanPortId 	= ?TN_PORT ++"_OAM_" ++ integer_to_list(TnVlanId),
    TnIpAlt 		= ?config(ssh, ?config(ConfigAttributeAlt, DutProps)),
    TnVlanIdAlt 	= ?config(vlan, ?config(ConfigAttributeAlt, DutProps)),
    TnVlanPortAltId 	= ?TN_PORT ++"_OAM_" ++ integer_to_list(TnVlanIdAlt),
    [
     {me_id, ?ME_ID},				% 1
     {fru, ?FRU},
     {router, ?ROUTER}, 			% OAM
     {router_alt, ?ROUTER_ALT}, 		% OAM_ALT
     {tn_port, ?TN_PORT},			% TN_A
     {tn_ip, TnIp},				% ex: 10.67.226.136
     {tn_vlan, TnVlanId},			% 2402
     {tn_vlan_port, TnVlanPortId},		% TN_A_OAM_2402 or TN_A_OAM_IP
     {tn_gw, ?TN_GW},				% 10.67.233.1
     {tn_mask, ?TN_MASK},			% 24
     {tn_ip_alt, TnIpAlt},			% ex: 10.67.233.136
     {tn_vlan_alt, TnVlanIdAlt},		% 2415
     {tn_vlan_port_alt, TnVlanPortAltId},       % TN_A_OAM_2415 | TN_A_OAM_IP_ALT
     {tn_gw_alt, ?TN_GW_ALT},			% 10.67.233.1
     {tn_mask_alt, ?TN_MASK_ALT},		% 24
     {tn_ipv6_net, ?TN_IPV6_NET},
     {tn_ipv6, TnIpv6},
     {tn_ipv6_gw, ?TN_IPV6_GW},
     {tn_ipv6_mask, ?TN_IPV6_MASK}
    ] ++ Config.


%%------------------------------------
get_required_node_config(Config) ->
    case is_target() of
	false -> {error, "Simulated environment detected. Expecting target failed."};
	true -> get_required_target_config(Config)
    end.

get_required_target_config(Config) ->
    case is_oap_configured_with_ipv4(Config) of
	true ->	   
	    process_precondtion_vlan_port(Config);
	_ ->
	    {error,"Missing OamAccessPoint configuration"}
    end.

is_target() ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    true;
	_Sim ->
	    false 
    end.

%%
get_ipv6_from_config(DutProps) ->
    ConfigAttributeIpv6Key = list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv6"),
    SshIpv6Value = ?config(ConfigAttributeIpv6Key, DutProps),

    case SshIpv6Value  of
	undefined ->
	    ct:log("No config '~p' - TN IPv6 created from Macro and link address",
		   [ConfigAttributeIpv6Key]),
	    undefined;
	SshIpv6 -> 
	    ct:log("Found '~p' from Config, value: ~p",
		   [ConfigAttributeIpv6Key, SshIpv6]),
	    get_ssh(SshIpv6)			
    end.

%%
get_ssh(SshIpv6) ->
    case ?config(ssh, SshIpv6) of
	Nopsky when Nopsky =:= ""; Nopsky =:= undefined ->
	    ct:log("No config 'ssh' in '~p'", [SshIpv6]),
	    undefined;
	TnIpv6FromConfig ->
	    TnIpv6FromConfig
    end.

%%========================================================================
%% is_oap_configured_with_ipv4(Config) -> ok
%% 
%% @doc 
%% Check if OamAccessPoint=1 is configured
%% @end
%%========================================================================
is_oap_configured_with_ipv4(Config) ->
    is_oap_configured_with_ipv4(Config, "1", "").
is_oap_configured_with_ipv4(Config, OapId, OapType) ->
    MeId = ?config(me_id, Config),
    Get =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSySM"}],
	     [{sysMId,[],["1"]},
	      {'OamAccessPoint', [],
	       [{oamAccessPointId,[],[OapId]}]}]}]}]},
    {ok, Conf} = netconf(get, [?NETCNF, Get]),
    %%ct:pal("OamAP conf: ~p", Conf),
    Names = ['ManagedElement','SystemFunctions','SysM','OamAccessPoint'],
    [OapAttributes] = struct(Conf, Names),
    %%ct:pal("OapAttributes: ~p", [OapAttributes]),
    case lists:keyfind(accessPoint, 1, OapAttributes) of
	{accessPoint, [], [OapAccessPoint]} ->
	    ct:pal("The ~s OamAccessPoint points to here: ~p",
		   [OapType, OapAccessPoint]),
	    %% investigate OAP to see which IP version it points to 
	    is_oap_to_ipv4(OapAccessPoint);
	false ->
	    false
    end.

is_oap_to_ipv4(OapAccessPoint) ->
    case string:str(OapAccessPoint, "AddressIPv4") of
	0 -> 
	    ct:log("The OamAccessPoint doesn't point to AddressIPv4 MO."),
	    false;
	_ -> 
	    ct:log("The OamAccessPoint point to AddressIPv4 MO."),
	    true
    end.

%%------------------------------------------------------------------------------
process_precondtion_vlan_port(Config) ->
    [EthernetPortId, InterfaceIpv4Id, OldVlanPortLdn, VlanPortIds, AddressIpv4Id, 
     OrigAddressIpv4] = get_restore_values(Config),

    case length(VlanPortIds) of
	0 ->
	    {error, "There is no VlanPort MO configured."};	    
	1 -> % Router=OAM is configured but Router=OAM_ALT is NOT configured
	    ct:log("VlanPort=~p configured",[VlanPortIds]),
	    %% Update the config values with existing values from the node
	    [VlanPortOam] = VlanPortIds,
	    ModifiedConfig = lists:keyreplace(tn_vlan_port, 1, Config,
					      {tn_vlan_port, VlanPortOam}),
		ModifiedConfig2 = lists:keyreplace(tn_port, 1, ModifiedConfig,
						  {tn_port, EthernetPortId}),
	    UpdatedConfig = [
			     {orig_interface_ipv4_id, InterfaceIpv4Id},
			     {orig_vlan_port_ldn, OldVlanPortLdn},					
			     {orig_address_ipv4_id, AddressIpv4Id},
			     {orig_ipv4_address_val, OrigAddressIpv4},
			     {orig_is_vlanport_alt_configured, false}
			     | ModifiedConfig2],
	    {ok, UpdatedConfig};
	_ -> % Both Router=OAM and Router=OAM_ALT is configured
	    ct:log("VlanPort(s)=~p configured",
		   [VlanPortIds]),

	    %% Only process 2 first elements in the VlanPortIds list
	    [VlanPortOam, VlanPortOamAlt] = sort_vlan_port_ids(VlanPortIds),
	    ct:log("Oam VlanPort: ~p, Oam Alt VlanPort: ~p",
		   [VlanPortOam, VlanPortOamAlt]),			

	    %% Update the config values with existing values from the node
	    ModConfig = lists:keyreplace(tn_vlan_port, 1, Config, 
					 {tn_vlan_port, VlanPortOam}),
	    ModConfig2 = lists:keyreplace(tn_vlan_port_alt, 1, ModConfig, 
					  {tn_vlan_port_alt, VlanPortOamAlt}),
		ModConfig3 = lists:keyreplace(tn_port, 1, ModConfig2,
						  {tn_port, EthernetPortId}),
	    UpdatedConfig = [
			     {orig_interface_ipv4_id, InterfaceIpv4Id},
			     {orig_vlan_port_ldn, OldVlanPortLdn},					
			     {orig_address_ipv4_id, AddressIpv4Id},
			     {orig_ipv4_address_val, OrigAddressIpv4},
			     {orig_is_vlanport_alt_configured, true}
			     | ModConfig3],
	    {ok, UpdatedConfig}
    end.

%% Take in a list of at least 2 VlanPort Ids. Hope to return VlanPort=OAM
%% as first element; VlanPort=OAM_ALT as second element
sort_vlan_port_ids(VlanPortIds) ->
    [FirstElement, SecondElement | _] = VlanPortIds,
    case (string:str(FirstElement, "ALT") =:= 0)
	and (string:str(FirstElement, "Alt") =:= 0)
	and (string:str(FirstElement, "alt") =:= 0) of
	true ->
	    [FirstElement, SecondElement];
	false ->
	    [SecondElement, FirstElement]
    end.

%%========================================================================
%% get_restore_values(Config) -> [VlanPortLdn, OrigAddressIpv4]
%%
%% @doc 
%% Remember the existing IP values of the node before the TC. 
%% Used for restoring the node to the state before the TC.
%% @end
%%========================================================================
get_restore_values(Config) ->
    ct:log("Remember old AddressIPv4::address, encapsulated VlanPort LDN,"
	   " InterfaceIpv4 ID and AddressIpv4 Id."),
    MeId = ?config(me_id, Config),
    Router = ?config(router, Config), % ex: 'OAM'
	%% Transport=1,EthernetPort=TN_C

    Get =
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'Router',[],
	     [{routerId,[],[Router]},
	      {'InterfaceIPv4',
	       [{interfaceIPv4Id,[],[]},
		{encapsulation,[],[]}, 
		{'AddressIPv4',[],
		 [{addressIPv4Id,[],[]},
		  {address,[],[]}
		 ]}]}]},
		{'EthernetPort',[],
		 [{ethernetPortId,[],[]}]},
	    {'VlanPort',[], 
	     [{vlanPortId,[],[]},
	      {vlanId,[],[]}]
	    }]}]},
    
    {ok, OldConf} = netconf(get, [?NETCNF, Get]),
	ct:pal("Got restore values: ~p", [OldConf]),

	%% Get EthernetPort ID
	SearchEthernetPortId = ['ManagedElement','Transport', 'EthernetPort', 
				ethernetPortId],
    [EthernetPortId] = struct(OldConf, SearchEthernetPortId),

    %% Get the InterfaceIpv4Id 
    SearchInterfaceIpv4Id = ['ManagedElement','Transport', 'Router',
			     'InterfaceIPv4', interfaceIPv4Id],
    [InterfaceIpv4Id] = struct(OldConf, SearchInterfaceIpv4Id),
   
    %% Get the encapsulated VlanPort
    SearchEcapVlanPortLdn = ['ManagedElement','Transport', 'Router',
			     'InterfaceIPv4', encapsulation],
    [VlanPortLdn] = struct(OldConf, SearchEcapVlanPortLdn),

    %% Get the AddressIpv4Id
    SearchAddressIpv4Id = ['ManagedElement','Transport', 'Router',
			   'InterfaceIPv4', 'AddressIPv4', addressIPv4Id],
    [AddressIpv4Id] = struct(OldConf, SearchAddressIpv4Id),

    %% Get the original IP address from AddressIpv4
    SearchAddressIPv4 = ['ManagedElement','Transport', 'Router',
			 'InterfaceIPv4', 'AddressIPv4', address],
    [OrigAddressIpv4] = struct(OldConf, SearchAddressIPv4),

    %%- Get the OAM VlanPort Id and OAM_ALT VlanPort ID
    SearchVlanPortId = ['ManagedElement','Transport', 'VlanPort', vlanPortId],
    [VlanPortIds] = get_value(OldConf, SearchVlanPortId),

    ct:pal("EthernetPortId=~p, InterfaceIpv4Id=~p, encapsulation: ~p, 
		VlanPort=~p \n AddressIPv4Id=~p, address=~p",
	   [EthernetPortId, InterfaceIpv4Id, VlanPortLdn,VlanPortIds, AddressIpv4Id, 
	    OrigAddressIpv4]),

    [EthernetPortId, InterfaceIpv4Id, VlanPortLdn, VlanPortIds, AddressIpv4Id, 
	OrigAddressIpv4].

%%========================================================================
%% rm_oam_alt_if_any(Config) -> ok
%% 
%% @doc 
%% Delete OamAccessPoint=Alternative:accessPoint and remove TN alt configuration
%% @end
%%========================================================================
rm_oam_alt_if_any(Config) ->
    ok = rm_oap_alt_if_any(Config),
    ok = rm_router_oam_alt_if_any(Config),
    ok = rm_vlanport_alt_if_any(Config).

rm_oap_alt_if_any(Config) ->
    ct:pal("Remove OamAccessPoint=Alternative::accessPoint if any..."),
    MeId = ?config(me_id, Config),
    Edit =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSySM"}],
	     [{sysMId,[],["1"]},
	      {'OamAccessPoint', [],
	       [{oamAccessPointId,[] ,["Alternative"]},
		{accessPoint, ?NETCONF_DELETE,[]}
	       ]}]}]}]},

    ok = netconf(edit_config, [?NETCNF, running, Edit]).

%%---------------- Router=OAM_ALT --------------------
rm_router_oam_alt_if_any(Config) ->
    ct:pal("Remove Router=OAM_ALT if any..."),
    case is_router_oam_alt_configured(Config) of
	false -> ok;
	true ->
	    MeId = ?config(me_id, Config),
	    RouterAltId = ?config(router_alt, Config),

	    Edit =
		{'ManagedElement',
		 [{managedElementId,[],[MeId]},
		  {'Transport',[],
		   [{transportId,[],["1"]},
		    {'Router', ?NETCONF_DELETE,
		     [{routerId,[],[RouterAltId]}]
		    }]}]},

	    ok = netconf(edit_config, [?NETCNF, running, Edit])
    end.

is_router_oam_alt_configured(Config) ->
    MeId = ?config(me_id, Config),
    RouterAltId = ?config(router_alt, Config),
    Get =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'Router', [],
	     [{routerId,[],[]}]}]}]},

    {ok, Conf} = netconf(get, [?NETCNF, Get]),
    %%ct:log("RESULT Conf: ~p", [Conf]),
    SearchRouterOamAlt = ['ManagedElement','Transport', 'Router', routerId],
    is_exist(Conf, SearchRouterOamAlt, RouterAltId).

%%---------------- VlanPort Alt --------------------
rm_vlanport_alt_if_any(Config) ->
    ct:pal("Remove VlanPort for OAM ALT if any..."),
    case ?config(orig_is_vlanport_alt_configured, Config) of
	false -> ok;
	true ->
	    MeId = ?config(me_id, Config),
	    VlanPortIdAlt = ?config(tn_vlan_port_alt, Config),

	    Edit =
		{'ManagedElement',
		 [{managedElementId,[],[MeId]},
		  {'Transport',[],
		   [{transportId,[],["1"]},
		    {'VlanPort', ?NETCONF_DELETE,
		     [{vlanPortId,[],[VlanPortIdAlt]}]
		    }]}]},

	    ok = netconf(edit_config, [?NETCNF, running, Edit])
    end.

%%========================================================================
%% create_oam_alt(Config) -> ok
%% 
%% @doc 
%% Create TN alt configuration and OamAccessPoint=Alternative::accessPoint to it
%% @end
%%========================================================================
create_oam_alt(Config) ->
    ok = create_vlanport_alt(Config),
    ok = create_address_ipv4_alt(Config),
    RouterOamAlt = ?config(router_alt, Config),
    ok = create_alt_routing_table_under_router(Config, RouterOamAlt),
    ok = configure_oap_alt(Config).

%%----------------------------------------------------------------------
%% Create Router=OAM_ALT,InterfaceIpv4=xx
%% and Router=OAM_ALT,InterfaceIpv4=xx,AddressIpv4=xx
%%----------------------------------------------------------------------
create_address_ipv4_alt(Config) ->
    ct:pal("Create Router=OAM_ALT,InterfaceIpv4=xx and"
	   " Router=OAM_ALT,InterfaceIpv4=xx,AddressIpv4=xx"),
    MeId = ?config(me_id, Config),	% MeId
    Router = ?config(router_alt, Config),
    InterfaceIpv4Id = ?config(tn_vlan_port_alt, Config),
    AddressIpv4Id = ?config(tn_vlan_port_alt, Config),
    VlanPortId = ?config(tn_vlan_port_alt, Config),
    VlanPortAltLdn = "ManagedElement=" ++ MeId ++ 
	",Transport=1,VlanPort=" ++ VlanPortId,
    AddressAlt = ?config(tn_ip_alt, Config) ++ "/" ++ ?config(tn_mask, Config),

    Edit =
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'Router',
	     [{routerId,[],[Router]},
	      {'InterfaceIPv4',
	       [{interfaceIPv4Id,[],[InterfaceIpv4Id]},
		{encapsulation,[], [VlanPortAltLdn]},
		%%			{mtu,[],["9000"]},
		{'AddressIPv4',[],
		 [{addressIPv4Id,[],[AddressIpv4Id]},
		  {address,[],[AddressAlt]}]}]}
	     ]}]}]},

    ok = netconf(edit_config, [?NETCNF, running, Edit]).

%%----------------------------------------------------------------------
%% Configure OamAccessPoint=Alternative to point to 
%% Router=OAM_ALT,InterfaceIpv4=xx,AddressIpv4=xx
%%----------------------------------------------------------------------
configure_oap_alt(Config) ->
    ct:pal("Configure OamAccessPoint=Alternative::accessPoint "),
    MeId = ?config(me_id, Config),

    Router = ?config(router_alt, Config),
    InterfaceIpv4Id = ?config(tn_vlan_port_alt, Config),
    AddressIpv4Id = ?config(tn_vlan_port_alt, Config),
    AddressIpv4Ldn = "ManagedElement=" ++ MeId ++ ",Transport=1,Router=" ++ 
	Router ++ ",InterfaceIPv4=" ++ InterfaceIpv4Id ++ ",AddressIPv4="
	++ AddressIpv4Id,

    Edit =
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSySM"}],
	     [{sysMId,[],["1"]},
	      {'OamAccessPoint', [],
	       [{oamAccessPointId,[] ,["Alternative"]},
		{accessPoint, [],[AddressIpv4Ldn]}
	       ]}]}]}]},

    ok = netconf(edit_config, [?NETCNF, running, Edit]).

%%========================================================================
%% create_vlanport_alt(Config) -> ok
%%
%% @doc 
%% Create VlanPort for alternative IP address
%% @end
%%========================================================================	
create_vlanport_alt(Config) ->
    ct:pal("Create VlanPort alt "),
    MeId = ?config(me_id, Config),	% MeId
    TnPort = ?config(tn_port, Config), % TnPort=TN_A | TN_C

    %%--------------------------------------------------
    %% Create a new VlanPort MO, corresponding to alt IP address
    %%--------------------------------------------------
    TnVlanPortAltId = ?config(tn_vlan_port_alt, Config),
    VlanId_alt = ?config(tn_vlan_alt, Config),
    EthernetPortLdn = "ManagedElement=" ++ MeId ++ ",Transport=1,EthernetPort="
	++ TnPort,

    Edit =
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'VlanPort',
	     [{vlanPortId,[],[TnVlanPortAltId]}, % create new VlanPort=alt
	      {vlanId,[],[integer_to_list(VlanId_alt)]},
	      {encapsulation,[],
	       [EthernetPortLdn]},
	      {isTagged,[],["true"]}]}
	   ]}]},
    ok = netconf(edit_config, [?NETCNF, running, Edit]).

%%========================================================================
%% create_alt_routing_table_under_router(Config, RouterName) -> ok
%%
%% @doc 
%% Create Router and Nexthop for alternative IP address
%% @end
%%========================================================================	
create_alt_routing_table_under_router(Config, RouterName) ->
    ct:pal("Create NextHop for the alt IP address under Router=~p", 
	   [RouterName]),
    MeId = ?config(me_id, Config),	% MeId
    GwAlt = ?config(tn_gw_alt, Config),

    Edit =
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'Router',
	     [{routerId,[],[RouterName]},
	      {'RouteTableIPv4Static',
	       [{routeTableIPv4StaticId,[],["1"]},
		{'Dst',[],
		 [{dst,[],["0.0.0.0/0"]},
		  {dstId,[],["default"]},
		  {'NextHop',[],
		   [{address,[],[GwAlt]}, % add router for the alt IP
		    {adminDistance,[],["2"]},
		    {nextHopId,[], [GwAlt]}]}]}]}]}]}]},
    ok = netconf(edit_config, [?NETCNF, running, Edit]),
    ct:log("Router=~s  Gateway: ~p", [RouterName, GwAlt]),
    ok.

%%========================================================================
%% rm_vlanport_and_nexthop_alt(Config) -> ok
%%
%% @doc 
%% Remove VlanPort for alternative IP address. Assume that this VlanPort is
%% not reserved by any MO.
%% @end
%%========================================================================	
rm_vlanport_and_nexthop_alt(Config) ->
    ct:pal("Remove VlanPort and NextHop for the alt IP address"),
    MeId = ?config(me_id, Config),
    Router = ?config(router, Config),

    %%--------------------------------------------------
    %% Remove the alt VlanPort MO
    %%--------------------------------------------------
    TnVlanPortAltId = ?config(tn_vlan_port_alt, Config),

    %%--------------------------------------------------
    %% Remove NextHop (router) for the alt IP
    %%--------------------------------------------------
    GwAlt = ?config(tn_gw_alt, Config),

    Edit =
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'VlanPort', ?NETCONF_DELETE,
	     [{vlanPortId,[],[TnVlanPortAltId]}		
	     ]},
	    {'Router',
	     [{routerId,[],[Router]},
	      {'RouteTableIPv4Static',
	       [{routeTableIPv4StaticId,[],["1"]},
		{'Dst',[],
		 [{dst,[],["0.0.0.0/0"]},
		  {dstId,[],["default"]},
		  {'NextHop', ?NETCONF_DELETE,
		   [{nextHopId,[],[GwAlt]}
		   ]}]}]}]}]}]},

    ok = netconf(edit_config, [?NETCNF, running, Edit]),
    ct:log("Under Router=~s: removed VlanPort=~p, NextHop(Gateway): ~p",
	   [Router, TnVlanPortAltId, GwAlt]),
    ok.

%%========================================================================
%% modify_addressIpv4(Config) -> ok
%%
%% @doc 
%% Use the alt IP to modify the existing AddressIPv4
%% @end
%%========================================================================
modify_addressIpv4(Config) ->
    ct:pal("Modify AddressIPv4::address using the alternative IP address"),
    MeId = ?config(me_id, Config),
    %%--------------------------------------------------
    %% Change AddressIPv4::address to alternative OAM IP
    %%--------------------------------------------------
    Mask = ?config(tn_mask, Config),
    NewIpv4 = ?config(tn_ip_alt, Config),
    NewAddress = NewIpv4 ++ "/" ++ Mask,

    TnVlanPortAltId = ?config(tn_vlan_port_alt, Config),
    NewVlanPortLdn = "ManagedElement=" ++ MeId ++ ",Transport=1,VlanPort=" 
	++ TnVlanPortAltId,

    modify_addressIpv4(Config, NewVlanPortLdn, NewAddress).

modify_addressIpv4(Config, NewVlanPortLdn, NewAddress) ->
    ct:pal("Modify AddressIPv4::address= ~p, InterfaceIPv4::encapsulation= ~s",
	   [NewAddress, NewVlanPortLdn]),
    MeId = ?config(me_id, Config),
    Router = ?config(router, Config),
    TnInterfaceIpv4Id = ?config(orig_interface_ipv4_id, Config),
    TnAddressIpv4Id = ?config(orig_address_ipv4_id, Config),

    Edit =
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'Router',
	     [{routerId,[],[Router]},
	      {'InterfaceIPv4',
	       [{interfaceIPv4Id,[],[TnInterfaceIpv4Id]},
		%% Point the OAM interface to the new vlan port
		{encapsulation,[], [NewVlanPortLdn]},
		{'AddressIPv4',[],
		 [{addressIPv4Id,[],[TnAddressIpv4Id]},
		  {address,[],[NewAddress]}]}]} % Change address
	     ]}]}]},
    ok = netconf(edit_config, [?NETCNF, running, Edit]),
    ct:log("Router=~s, InterfaceIPv4=~s",
           [Router, TnInterfaceIpv4Id]),
    ct:log("IPv4/Mask: ~p", [NewAddress]),
    ok.

%%========================================================================
%% wait_for_eriChangeIPAddressEvent(Timeout, IpAddress)
%% 
%% @doc 
%% Wait for the SNMP trap to be sent; assert that the content of the trap 
%% contains the expected new IP
%% @end
%%========================================================================
%% @hidden
wait_for_eriChangeIPAddressEvent(Timeout, IpAddress) ->
    Traps = [[{type, eriChangeIPAddressEvent},
	      {eriChangeIPAddressNewNodeOamIpAddress, IpAddress}]],
    Opts = [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
			    [{type,eriAlarmAlarmListRebuilt}],
			    [{type,eriAlarmCleared}],
			    [{type,eriAlarmCritical}],
			    [{type,eriAlarmMinor}],
			    [{type,eriAlarmMajor}],
			    [{type,eriAlarmWarning}],
			    [{type,nsNotifyShutdown}]]},
	    return_received_traps],
    ct:pal("Setup waiting for SNMP traps~n"
	   "type: eriChangeIPAddressEvent~n"
	   "eriChangeIPAddressNewNodeOamIpAddress: ~s~n"
	   "Timeout: ~p", [IpAddress, Timeout]),
    ok = rct_snmpmgr:wait_for_traps(Traps, Opts, Timeout).

%%========================================================================
%% point_oap_to_addressIpv6(Config) ->
%% 
%% @doc 
%% point OamAccessPoint=1 to AddressIPv6 MO
%% @end
%%========================================================================
point_oap_to_addressIpv6(Config) ->
    MeId = ?config(me_id, Config),
    Router = ?config(router, Config),
    InterfaceIpv6Id = ?config(tn_vlan_port, Config),
    AddressIpv6Id = ?config(tn_vlan_port, Config),
    AddressIpv6Ldn = "ManagedElement=" ++ MeId ++ ",Transport=1,Router="
	++Router ++ ",InterfaceIPv6=" ++ InterfaceIpv6Id ++ 
	",AddressIPv6=" ++ AddressIpv6Id,
    LdapIpv6 = ?LDAP_IPV6,
    ok = point_oap_to_addressIpvx(Config, AddressIpv6Ldn, LdapIpv6).

point_oap_to_addressIpv4(Config) ->
    MeId = ?config(me_id, Config),
    Router = ?config(router, Config),
    InterfaceIpv4Id = ?config(orig_interface_ipv4_id, Config),
    AddressIpv4Id = ?config(orig_address_ipv4_id, Config),
    AddressIpv4Ldn = "ManagedElement=" ++ MeId ++ ",Transport=1,Router=" ++ 
	Router ++ ",InterfaceIPv4=" ++ InterfaceIpv4Id ++ ",AddressIPv4="
	++ AddressIpv4Id,
    LdapIpv4 = ?LDAP_IPV4,

    ok = point_oap_to_addressIpvx(Config, AddressIpv4Ldn, LdapIpv4).

point_oap_to_addressIpvx(Config, AddressIpvXLdn, LdapIpvX) ->
    ct:pal("About to set OamAccessPoint=1::accessPoint to ~s, LDAP server: ~p",
	   [AddressIpvXLdn, LdapIpvX]),
    MeId = ?config(me_id, Config),
    Edit=
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'SystemFunctions',[],
	   [{systemFunctionsId,[],["1"]},
	    {'SysM',
	     [{sysMId,[],["1"]},
	      {'OamAccessPoint',[],
	       [{oamAccessPointId,[],["1"]},
		{accessPoint,[],
		 [AddressIpvXLdn]}]}]},
	    {'SecM',
	     [{secMId,[],["1"]},
	      {'UserManagement',[],
	       [{userManagementId,[],["1"]},
		{'LdapAuthenticationMethod',
		 [{ldapAuthenticationMethodId,[],["1"]},
		  {'Ldap',[],
		   [{ldapId,[],["1"]},
		    {ldapIpAddress,[],[LdapIpvX]}]}]}
	       ]}]}
	   ]}]},

    ok = netconf(edit_config, [?NETCNF, running, Edit]),
    ct:pal("OamAccessPoint=1::accessPoint=~s  Ldap=1::ldapIpAddress=~p",
	   [AddressIpvXLdn, LdapIpvX]),
    ok.

%%========================================================================
%% configure_ipv6_transport(Config) -> Ipv6Addr
%% 
%% @doc 
%% Configure IPv6 for transport, configure AddressIPv6 MO
%% @end
%%========================================================================
configure_ipv6_transport(Config) ->
    ok = activate_ipv6_licence(Config),
    Ipv6Addr = get_ipv6(Config),
    ok = add_ipv6_address(Config, Ipv6Addr),
    Ipv6Addr.

%%========================================================================
%% activate_ipv6_licence(Config) 
%% 
%% @doc 
%% Activate the IPv6 license
%% @end
%%========================================================================
activate_ipv6_licence(Config) ->
    ct:pal("Activating IPv6-license"),
    MeId = ?config(me_id, Config),
    Edit =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[MeId]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	    {'Lm',
	     [{lmId, [], ["1"]},
	      {'FeatureState',
	       [{featureStateId, [], [?FEATURE_IPV6]},
		{featureState, [], ["ACTIVATED"]}]}]}]}]},
    ok = netconf(edit_config, [?NETCNF, running, Edit]),
    ct:pal("IPv6-license ACTIVATED"),
    ok.

%%========================================================================
%% add_ipv6_address(Config, Ipv6Addr) 
%% 
%% @doc 
%% Configure AddressIPv6 MO with routing table and default router for it
%% @end
%%========================================================================
add_ipv6_address(Config, Ipv6Addr) ->
    ct:pal("Configuring Transport with IPv6"),
    MeId = ?config(me_id, Config),
    Router = ?config(router, Config),
    TnVlanPortId = ?config(tn_vlan_port, Config),
    VlanPortLdnOrig = ?config(orig_vlan_port_ldn, Config),
    Gw = ?config(tn_ipv6_gw, Config),
    Mask = ?config(tn_ipv6_mask, Config),
    Address = Ipv6Addr ++ "/" ++ Mask,
    Edit =
        {'ManagedElement',
         [{managedElementId,[],[MeId]},
          {'Transport',[],
           [{transportId,[],["1"]},
            {'Router',
             [{routerId,[],[Router]},
              {'InterfaceIPv6',
               [{interfaceIPv6Id,[],[TnVlanPortId]},
                {encapsulation,[], [VlanPortLdnOrig]},
                {mtu,[],["9000"]},
                {'AddressIPv6',[],
                 [{addressIPv6Id,[],[TnVlanPortId]},
                  {address,[],[Address]}]}]},
              {'RouteTableIPv6Static',
               [{routeTableIPv6StaticId,[],["1"]},
                {'Dst',[],
                 [{dst,[],["::/0"]},
                  {dstId,[],["default"]},
                  {'NextHop',[],
                   [{address,[],[Gw]},
                    {nextHopId,[], [Gw]}]}]}]}]}]}]},
    ok = netconf(edit_config, [?NETCNF, running, Edit]),
    ct:log("Router=~s created InterfaceIPv6=~s",
           [Router, TnVlanPortId]),
    ct:log("IPv6/Mask: ~p, Gateway: ~p", [Address, Gw]),
    ok.

%%========================================================================
%% rm_ipv6_address(Config) 
%% 
%% @doc 
%% Remove AddressIPv6 MO with routing table and default router for it
%% @end
%%========================================================================
rm_ipv6_address(Config) ->
    ct:pal("Removing Transport with IPv6"),
    MeId = ?config(me_id, Config),
    Router = ?config(router, Config),
    TnVlanPortId = ?config(tn_vlan_port, Config),
    Gw = ?config(tn_ipv6_gw, Config),
    Edit =
        {'ManagedElement',
         [{managedElementId,[],[MeId]},
          {'Transport',[],
           [{transportId,[],["1"]},
            {'Router',
             [{routerId,[],[Router]},
              {'InterfaceIPv6', ?NETCONF_DELETE,
               [{interfaceIPv6Id, [],[TnVlanPortId]}]},
              {'RouteTableIPv6Static', ?NETCONF_DELETE,
               [{routeTableIPv6StaticId, [],["1"]}
	       ]}]}]}]},
    ok = netconf(edit_config, [?NETCNF, running, Edit]),
    ct:pal("Under Router=~s: deleted InterfaceIPv6=~s, deleted Gateway: ~p",
           [Router, TnVlanPortId, Gw]),
    ok.

%%------------------------------------------------------------------------------
%% Get the IPv6 for the STP. If not exist, create one.
%%------------------------------------------------------------------------------	
get_ipv6(Config) ->
    case ?config(tn_ipv6, Config) of
	undefined ->
	    mk_ipv6(Config);
	IPv6 ->
	    IPv6
    end.

mk_ipv6(Config) ->
    MeId = ?config(me_id, Config),
    TnPort = ?config(tn_port, Config),
    TnIpv6Net = ?config(tn_ipv6_net, Config),
    Get =
	{'ManagedElement',
	 [{managedElementId,[],[MeId]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'EthernetPort',
	     [{ethernetPortId,[],[TnPort]}]}]}]},

    {ok, Conf} = netconf(get, [?NETCNF, Get]),

    MacString = get_mac_string(Conf),
    %%get rid of any starting zeros, i.e. :0123: -> :123:
    [A,B,C,D,E,F] = [integer_to_list(list_to_integer(I, 16), 16) ||
			I <- string:tokens(MacString, ":")],
    IPv6 = TnIpv6Net ++ "::" ++ A ++ B ++ ":" ++ C ++ D ++ ":" ++ E ++ F,
    ct:log("IPv6 created from net: ~p and MAC: ~p -> ~p",
	   [TnIpv6Net, MacString, IPv6]),
    IPv6.

get_mac_string(Conf) ->
    Names = ['ManagedElement','Transport', 'EthernetPort', 'macAddress'],
    case struct(Conf, Names) of
	[[Mac]] ->
	    Mac;
	_ ->
	    ct:fail("Couldn't locate MAC in this data: ~p", [Conf])
    end.

%% ===========================================================================
%% Description: Execute netconf configuration toward the node.<br/>
%% netconf(F, A)  -> Res
%% ===========================================================================
netconf(F, A) ->
    %%{ok, _} = ct_netconfc:open(nc1, [{user, "expert"}, {password, "expert"}]),
    %%{ok, _} = ct_netconfc:open(nc1, []),
    open_trans(600000),
    Res = apply(ct_netconfc, F, A),
    case Res of
        {error, _} ->
            catch ct_netconfc:close_session(?NETCNF),
            Res;
        _ ->
            ok = ct_netconfc:close_session(?NETCNF),
	    Res
    end.

%% @hidden
open_trans(Timeout) when Timeout < 0 ->
    ct:fail("Could not setup a NETCONF connection within max timeout.");
open_trans(Timeout) ->
    case ct_netconfc:open(?NETCNF, [{user, "muexpert"}, {password, "muexpert"}]) of
    %% case ct_netconfc:open(?NETCNF, []) of
	{ok, _} = R ->
	    R;
	_ ->
	    timer:sleep(5000),
            open_trans(Timeout-5000)
    end.

%%-------------------------------------------------------------
%% matching for a given Name and Value
is_exist([{Name, _, [Value]} | _], [Name], Value) ->
    true;
is_exist( [ {Name, _, _} | Neighbor], [Name], Value) ->
    is_exist(Neighbor, [Name], Value);
is_exist( [], _ , _) ->
    false;
%% descend downwards into the structure, following Names
is_exist([ {Name, _, Next1} , {Name, _, Next2} | _], [Name | Names], Value) ->
    is_exist(Next1, Names, Value) orelse is_exist(Next2, Names, Value);
is_exist([ {Name, _, Next} | _], [Name | Names], Value) ->
    is_exist(Next, Names, Value);
is_exist([ {_,_,_} | T], Names, Value) ->
    is_exist(T, Names, Value).

%%-------------------------------------------------------------
%% matching for a given Name and Value
get_value([{LastName, _, Value} | Neighbor], [LastName]) ->
    [Value | get_value(Neighbor, [LastName]) ];
get_value( [], _ ) ->
    [];
%% descend into the subtree	
get_value([{Name, _, Next1} , {Name, _, Next2} | _] , [Name | Names]) ->
    [get_value(Next1, Names) ++ get_value(Next2, Names)];
get_value([{Name,_, Children} | _], [Name | Names]) ->
    get_value(Children, Names);
get_value([{_,_,_} | Neighbor] , Names) ->
    get_value(Neighbor, Names).

%% -------------------------------------------------------------
%% descend into the structure following list(Names), when only one Name
%% remains collect all matching and return them
struct([{Name, _, Next} | T], [Name]) ->
    [Next | struct(T, [Name])];
struct([{_, _, _} | T], [Name]) ->
    struct(T, [Name]);
struct([], _) -> [];
struct([{Name, _, Next} | _], [Name | Names]) ->
    struct(Next, Names);
struct([{_, _, _} | T], Names) ->
    struct(T, Names).

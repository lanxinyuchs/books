%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_oap_SUITE.erl %
%%% @author etxlg
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R5A/2
%%% 
%%% @doc == Test Suite 
%%% This Test Suite needs an UP with TN
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end


-module(omc_oap_SUITE).
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
%%% Rev        Date        Name        What
%%% -----      ----------  --------    -----------------------
%%% Rx         2015-09-02  etxlg       Created
%%% R4A/2      2015-10-20  etxlg       Now using VLAN
%%% R4A/3      2015-11-02  etxlg       Also OamAP=Alternative
%%% R4A/4      2015-11-03  etxlg       And... export some functions
%%%					activate TN-licens for multiple routers
%%% R4A/5      2015-11-25  etxlg       IPv6, works but not done...
%%% R4A/6      2015-11-28  etxlg       Removed compiler warnings
%%% R4A/7      2015-12-08  etxlg       Names changed to fit basic-install
%%% R4A/8      2015-12-15  etxlg       New IPv6 networks in lab
%%% R5A/1      2016-01-14  etxlg       New IPv6 default GW in lab
%%% R5A/2      2016-03-07  etxlg       Add MpClusterHandling
%%% ----------------------------------------------------------
%%% 

%%-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0
	 %groups/0
	]).

%testcases for development and debug
-export([configure_oap/1, print_stp_dut_config/1]).

%this will create the Alternative OamAccessPoint and its Transport config
-export([configure_alt_oap/1]).
%this will add an IPv6 address to TN
-export([configure_ipv6_transport/1]).
%point/repoint OamAccessPoint=1  to IPv4/IPv6
-export([oam_ap_to_ipv4/1, oam_ap_to_ipv6/1]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_rpc, rpc1},
		 {rct_netconf,{nc1, expert_auth}},
                 {cth_conn_log,[]},
                 {rct_logging, {all,
			[{erlang,{["ERROR REPORT","CRASH REPORT"],
				  ["SSL: certify: ssl_handshake.erl:"]}}]}}
    		]
     }].


%% @hidden
init_per_suite(In_config) ->
    os:getenv("SIM_OR_TARGET") =:= "sim" andalso 
	    ct:fail("This suite does NOT work in simulated environment"),
%%    crypto:start(),
%%    ssh:start(),
%%    ssl:start(),

    ct:pal("Preparing ct-loopdata (Config)"),
    Config = prepare_config(In_config), %add; things from stp.cfg
    ct:pal("In_config: ~p~nPrepared_config:~p~n", [In_config, Config]),
    Config.

%% @hidden
end_per_suite(_Config) ->
	%duh
    ok.

%% @hidden
init_per_testcase(print_stp_dut_config, Config) ->
    Config;
init_per_testcase(_Tc, Config) ->
    %matching all testcases except those listed above
    %Config is already prepared in init_per_suite/1
    ct:pal("Checking/Configuring the site for OamAccessPoint"),
    add_oap_to_node(Config),
    ct:pal("OamAccessPoint configured - ready to run testcase"),
    Config.

end_per_testcase(_Tc, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [].

%groups() ->
%    AllGroup = all(),
%    [
%     {default__group, [], AllGroup},
%     {sbc__qual__all__1__group, [], []},
%     {sbc__def__all__1__group, [], []},  
%     {sbc__upgrade__all__1__group, [], []},  
%     {sdc__cover__sim__1__group, [], []},
%     {sdc__def__all__1__group, [], []},  
%     {sdc__qual__all__1__group, [], [{group, all_working_1}]},
%     {all_working, [], [{group, all_working_1}]},  
%     {all_working_1, [sequence], [connect_cli,
%				connect_netconf,
%				connect_rcscoli,
%				disable_tls_misconnect,
%				connect_max_cli_sessions]}].
%%--------------------------------------------------------------------
%% @doc
%% This is not actually a testcase.
%% Using netconf to just add all config
%% the testcase is actually empty, all work done in init_per_suite(..).
%%
%% @spec configure_oap(_Config) -> void()
%% @end
%%--------------------------------------------------------------------
configure_oap(_Config) ->
    %nothing here, all is done in init_per_suite/1
    % ct:pal("Node configured with OAP").
    ok.

configure_alt_oap(Config) ->
    ok = add_alt_oap_to_node(Config).

configure_ipv6_transport(Config) ->
    ok = activate_ipv6_licence(Config),
    IPv6_addr = get_ipv6(Config),
    ok = add_ipv6_address(Config, IPv6_addr),
    duh.

oam_ap_to_ipv4(Config) ->
    ok = create_oap(Config, inet).

%IPv6 address (configure_ipv6_transport/1) must be complete
oam_ap_to_ipv6(Config) ->
    ok = create_oap(Config, inet6).

print_stp_dut_config(_Config) ->
    Dut = ?config(1, ct:get_config(test_nodes)),
    Dut_props =  ct:get_config(Dut),
    ct:pal("DUT: ~p~nDut_props: ~p~n", [Dut, Dut_props]).



%%--------------------------------------------------------------------
%% @doc
%% Add needed parameters to Config -proplist,
%% the majority of environment hardcoding is here.
%% @spec prepare_config(Config) -> New_config
%% @end
%%--------------------------------------------------------------------
%% we need
%% FieldReplaceableUnit: -> 1
%% TnPort: -> TN_A
%% IPaddress: from stp.cfg via ssh_TN_A_ipv4-tuple
%% Netmask: -> 24
%% Default route: -> 10.67.226.2

-define(ME_ID, "1").
-define(FRU, "1").
-define(ROUTER, "OAM").
-define(ROUTER_ALT, "OAM_ALT").
-define(TN_PORT, "TN_A").
%Kalle will add these two to stp.cfg
-define(TN_MASK, "24").
-define(TN_MASK_ALT, "24").
-define(TN_GW, "10.67.226.1").
-define(TN_GW_ALT, "10.67.233.1").
%%% IPv6 net: 10.67.226.0/24       2001:1b70:8292:226::/64  2402
%%-define(TN_IPV6_NET, "2001:1b70:8292:226").
-define(TN_IPV6_NET, "2001:1b70:6281:f100").
%% not working any more %-define(TN_IPV6_GW, ?TN_IPV6_NET ++ "::" ++ "1").
-define(TN_IPV6_GW, ?TN_IPV6_NET ++ "::" ++ "2"). %works 20160114
-define(TN_IPV6_MASK, "64").
prepare_config(Config) ->
    Dut = ?config(1, ct:get_config(test_nodes)),
    Dut_props =  ct:get_config(Dut),
    Config_attribute = list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv4"),
    Config_attribute_alt = list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv4_alt"),
    Config_attribute_ipv6 = list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv6"),
    Tn_ip = ?config(ssh, ?config(Config_attribute, Dut_props)),
    Tn_vlan = ?config(vlan, ?config(Config_attribute, Dut_props)),
    Tn_ipv6 = 
	case ?config(Config_attribute_ipv6, Dut_props) of
	    undefined ->
		ct:pal("No config '~s' - TN IPv6 created from Macro and link address",
		       [Config_attribute_ipv6]),
		undefined;
	    Ssh_ipv6 ->
		case ?config(ssh, Ssh_ipv6) of
		    Nopsky when Nopsky =:= ""; Nopsky =:= undefined ->
		ct:pal("No config '~s' in '~s' - TN IPv6 created from Macro and link address",
		       ["ssh", Config_attribute_ipv6]),
			undefined;
		    Tn_ipv6_from_config ->
			Tn_ipv6_from_config
		end
	end,
    Tn_vlan_port = ?TN_PORT ++"_OAM_" ++ integer_to_list(Tn_vlan),
    Tn_ip_alt = ?config(ssh, ?config(Config_attribute_alt, Dut_props)),
    Tn_vlan_alt = ?config(vlan, ?config(Config_attribute_alt, Dut_props)),
    Tn_vlan_port_alt = ?TN_PORT ++"_OAM_" ++ integer_to_list(Tn_vlan_alt),
    [
	{me_id, ?ME_ID},
	{fru, ?FRU},
	{router, ?ROUTER},
	{router_alt, ?ROUTER_ALT},
	{tn_port, ?TN_PORT},
	{tn_ip, Tn_ip},
	{tn_vlan, Tn_vlan},
	{tn_vlan_port, Tn_vlan_port},
	{tn_gw, ?TN_GW},
	{tn_mask, ?TN_MASK},
	{tn_ip_alt, Tn_ip_alt},
	{tn_vlan_alt, Tn_vlan_alt},
	{tn_vlan_port_alt, Tn_vlan_port_alt},
	{tn_gw_alt, ?TN_GW_ALT},
	{tn_mask_alt, ?TN_MASK_ALT},
	{tn_ipv6_net, ?TN_IPV6_NET},
	{tn_ipv6, Tn_ipv6},
	{tn_ipv6_gw, ?TN_IPV6_GW},
	{tn_ipv6_mask, ?TN_IPV6_MASK}
    ] ++ Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_oap_to_node(Config) ->
    add_oap_to_node(is_oap_configured(Config), Config).
add_oap_to_node(true, _) ->
    ct:pal("ME is already configured OamAccessPoint=1");
add_oap_to_node(false, Config) ->
    create_fru(is_fru_created(Config), Config),
    create_transport(is_transport_created(Config), Config),
    create_oap(Config, inet).

add_alt_oap_to_node(Config) ->
    add_alt_oap_to_node(is_alt_oap_configured(Config), Config).
add_alt_oap_to_node(true, _) ->
    ct:pal("ME is already configured with OamAccessPoint=Alternative");
add_alt_oap_to_node(false, Config) ->
    activate_vr_license(is_vr_license_activated(Config), Config),
    create_fru(is_fru_created(Config), Config),
    create_alt_transport(is_alt_transport_created(Config), Config),
    create_alt_oap(Config, inet).

-define(FEATURE_VR, "CXC4011823").
-define(FEATURE_IPV6, "CXC4040006").
activate_vr_license(true, _) ->
    ct:pal("ME already have multiple-router-license activated"),
    ok;
activate_vr_license(false, Config) ->
    ct:pal("Activating multiple-router-license"),
    Me_id = ?config(me_id, Config),
    Edit =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{lmId, [], ["1"]},
		{'FeatureState',
		 [{featureStateId, [], [?FEATURE_VR]},
		  {featureState, [], ["ACTIVATED"]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("Multiple-router-license ACTIVATED"),
    ok.

is_vr_license_activated(Config) ->
    Me_id = ?config(me_id, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{lmId, [], ["1"]},
		{'FeatureState',
		 [{featureStateId, [], [?FEATURE_VR]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','SystemFunctions', 'Lm', 'FeatureState',
	     featureState],
    case struct(Conf, Names) of
	[] ->
	    ct:fail("Featurestate not found: ~p", [Conf]);
	[["ACTIVATED"]] ->
	    true;
	[["DEACTIVATED"]] ->
	    false;
	Other ->
	    ct:fail("Unexpected, Conf: ~p~nStruct: ~p", [Conf, Other])
    end.

activate_ipv6_licence(Config) ->
    ct:pal("Activating IPv6-license"),
    Me_id = ?config(me_id, Config),
    Edit =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{lmId, [], ["1"]},
		{'FeatureState',
		 [{featureStateId, [], [?FEATURE_IPV6]},
		  {featureState, [], ["ACTIVATED"]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("IPv6-license ACTIVATED"),
    ok.


create_oap(Config, Inet) ->
    Tn_vlan_port = ?config(tn_vlan_port, Config),
    Router = ?config(router, Config),
    create_oap(Config, Router, Tn_vlan_port, "1", "", Inet).

%%need to add create_transport for this before setting it to IPv6
create_alt_oap(Config, Inet) ->
    Tn_vlan_port_alt = ?config(tn_vlan_port_alt, Config),
    Router = ?config(router_alt, Config),
    create_oap(Config, Router, Tn_vlan_port_alt, "Alternative", "Alternative", Inet).

create_oap(Config, Router, Tn_vlan_port, Oam_AP_id, Oap_type, Inet) ->
    Me_id = ?config(me_id, Config),
    Point =
	case Inet of
	    inet ->
		"ManagedElement=" ++ Me_id ++ ",Transport=1,Router=" ++
		 Router ++ ",InterfaceIPv4=" ++ Tn_vlan_port ++ 
		 ",AddressIPv4=" ++ Tn_vlan_port;
	    inet6 ->
		"ManagedElement=" ++ Me_id ++ ",Transport=1,Router=" ++
		 Router ++ ",InterfaceIPv6=" ++ Tn_vlan_port ++ 
		 ",AddressIPv6=" ++ Tn_vlan_port
	end,
    Edit = 
	{'ManagedElement',
                [{managedElementId,[],[Me_id]},
                 {'SystemFunctions',[],
                     [{systemFunctionsId,[],["1"]},
                      {'SysM',
                          [{sysMId,[],["1"]},
                           {'OamAccessPoint',[],
                               [{oamAccessPointId,[],[Oam_AP_id]},
                                {accessPoint,[],
                                    [Point]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("~s ~p OamAccessPoint created, pointing to: ~p", [Oap_type, Inet, Point]).


%%%FRU stuff is the same for both OamAP=1 and OamAP=Alternative
create_fru(true, _) ->
    ct:pal("FieldReplaceableUnit is already created");
create_fru(false, Config) ->
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Fru_id = ?config(fru, Config),
    PcoreRef = "ManagedElement=" ++ Me_id ++
	       ",Equipment=1,FieldReplaceableUnit=" ++ Fru_id,
    %the MpClusterHandling i just added - no check if it exists or not
    Edit =
        {'ManagedElement',
         [{managedElementId,[],[Me_id]},
          {'Equipment',
           [{equipmentId,[],["1"]},
	    {'FieldReplaceableUnit',
	     [{fieldReplaceableUnitId,[],[Fru_id]},
	      {administrativeState,[],["UNLOCKED"]},
	      {'TnPort',
	       [{tnPortId,[],[Tn_port]}]}]}]},
          {'NodeSupport',
           [{nodeSupportId,[],["1"]},
	    {'MpClusterHandling',
	     [{mpClusterHandlingId,[],["1"]},
	      {primaryCoreRef, [],[PcoreRef]}]}]}
	 ]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("FieldReplaceableUnit=1 with TnPort=~p - created", [Tn_port]).

is_fru_created(Config) ->
true andalso
begin
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'Equipment',
           [{equipmentId,[],["1"]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','Equipment', 'FieldReplaceableUnit', 'TnPort'],
    case struct(Conf, Names) of
	[] -> false;
	[Equipment] ->
	    case lists:member({tnPortId,[],[Tn_port]}, Equipment) of
		true ->
		    true;
		_ ->
		    ct:pal("Equipment conf: ~p", Conf),
		    ct:pal("Equipment attrs: ~p", [Equipment]),
		    false
	    end
    end
end.

create_transport(Done, Config) ->
    Ip = ?config(tn_ip, Config),
    Vlan = ?config(tn_vlan, Config),
    Tn_vlan_port = ?config(tn_vlan_port, Config),
    Mask = ?config(tn_mask, Config),
    Gw = ?config(tn_gw, Config),
    Router = ?config(router, Config),
    create_transport(Done, Config, Ip, Vlan, Tn_vlan_port, Mask, Gw,
		     Router, "").
create_alt_transport(Done, Config) ->
    Ip = ?config(tn_ip_alt, Config),
    Vlan = ?config(tn_vlan_alt, Config),
    Tn_vlan_port = ?config(tn_vlan_port_alt, Config),
    Mask = ?config(tn_mask_alt, Config),
    Gw = ?config(tn_gw_alt, Config),
    Router = ?config(router_alt, Config),
    Fudged_done = case Done of between -> false; _ -> Done end,
    create_transport(Fudged_done, Config, Ip, Vlan, Tn_vlan_port, Mask, Gw,
		     Router, "Alternative").

create_transport(true, _, _, _, _, _, _, _, Oap_type) ->
    ct:pal("~sTransport is already created", [Oap_type]);
create_transport(between, _,_,  _, _, _, _, _, Oap_type) ->
    ct:pal("~sTransport is partly configured - manual intervention required",
	   [Oap_type]),
    exit(duh);
create_transport(false, Config, Ip, Vlan, Tn_vlan_port, Mask, Gw, Router,
		 Oap_type) ->
    ct:pal("Configuring Transport"),
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Fru_id = ?config(fru, Config),
    Encap_eth = "ManagedElement=" ++ Me_id ++
		    ",Equipment=1,FieldReplaceableUnit=" ++
		    Fru_id ++ ",TnPort=" ++ Tn_port,
    Encap_vlan = "ManagedElement=" ++ Me_id ++ ",Transport=1,EthernetPort=" ++
		 Tn_port,
    Encap_int = "ManagedElement=" ++ Me_id ++ ",Transport=1,VlanPort=" ++
		    Tn_vlan_port,
    Address = Ip ++ "/" ++ Mask,
    Edit =
        {'ManagedElement',
         [{managedElementId,[],[Me_id]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'EthernetPort',
	     [{ethernetPortId,[],[Tn_port]},
	      {encapsulation,[], [Encap_eth]},
	      {administrativeState,[],["UNLOCKED"]}]},
	    {'VlanPort',
	     [{vlanPortId,[],[Tn_vlan_port]},
	     {vlanId,[],[integer_to_list(Vlan)]},
	     {encapsulation,[],
		[Encap_vlan]},
	     {isTagged,[],["true"]}]},
	    {'Router',
	     [{routerId,[],[Router]},
	      {'InterfaceIPv4',
	       [{interfaceIPv4Id,[],[Tn_vlan_port]},
		{encapsulation,[], [Encap_int]},
		{mtu,[],["9000"]},
		{'AddressIPv4',[],
		 [{addressIPv4Id,[],[Tn_vlan_port]},
		  {address,[],[Address]}]}]},
	      {'RouteTableIPv4Static',
	       [{routeTableIPv4StaticId,[],["1"]},
		{'Dst',[],
		 [{dst,[],["0.0.0.0/0"]},
		  {dstId,[],["default"]},
		  {'NextHop',[],
		   [{address,[],[Gw]},
		    {nextHopId,[], [Gw]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("~sTransport=1 with TnPort=~p Vlan=~p - created",
	   [Oap_type, Tn_port, Vlan]),
    ct:pal("~sIP/Mask: ~p/~p, Gateway: ~p", [Oap_type, Ip, Mask, Gw]).

is_transport_created(Config) ->
    Router = ?config(router, Config),
    is_transport_created(Config, Router, ?config(tn_vlan_port, Config), "").
is_alt_transport_created(Config) ->
    Router = ?config(router_alt, Config),
    is_transport_created(Config, Router, ?config(tn_vlan_port_alt, Config),
			 "Alternative").
is_transport_created(Config, Router, Tn_vlan_port, Oap_type) ->
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'Transport',
           [{transportId,[],["1"]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    %ct:pal("Transport conf: ~p", Conf),
    Names1 = ['ManagedElement','Transport', 'Router'],
    Names2 = ['ManagedElement','Transport', 'EthernetPort'],
    Names3 = ['ManagedElement','Transport', 'VlanPort'],
    Routers = struct(Conf, Names1),
    Ethers = struct(Conf, Names2),
    Vlans = struct(Conf, Names3),
    Router_ids = [lists:keyfind(routerId, 1, R) || R <- Routers],
    Ether_ids = [lists:keyfind(ethernetPortId, 1, E) || E <- Ethers],
    Vlan_ids = [lists:keyfind(vlanPortId, 1, V) || V <- Vlans],
    REV = {lists:keyfind([Router], 3, Router_ids),
	  lists:keyfind([Tn_port], 3, Ether_ids),
	  lists:keyfind([Tn_vlan_port], 3, Vlan_ids)},
    ct:pal("debug REV: ~p ~n", [REV]),
    case REV of
	{false, false, false} -> %nothing configured go on
	    false;
	REV ->
	    case lists:member(false, tuple_to_list(REV)) of
		true ->
		    {R, E, V} = REV,
		    ct:pal("~s Transport conf: ~p~n"
			   "~s Transport Router: ~p~n"
			   "~s Transport Ether: ~p~n"
			   "~s Transport Vlan: ~p~n",
			   [Oap_type, Conf, Oap_type, Routers,
			    Oap_type, Ethers, Oap_type, Vlans]),
		    R =:= false andalso
			ct:pal("~s Transport has no Router - "
				"erase/fix this manually.~n", [Oap_type]),
		    E =:= false andalso
			ct:pal("~s Transport has no Etherport - "
				"erase/fix this manually.~n", [Oap_type]),
		    V =:= false andalso
			ct:pal("~s Transport has no Vlan - "
				"erase/fix this manually.~n", [Oap_type]),
		    between;
		false ->
		    %configured ok-ish
		    %we skip checking the address and such
		    true
	    end
    end.

is_oap_configured(Config) ->
    is_oap_configured(Config, "1", "").
is_alt_oap_configured(Config) ->
    is_oap_configured(Config, "Alternative", "Alternative").
is_oap_configured(Config, Oam_AP_id, Oap_type) ->
true andalso
begin
    Me_id = ?config(me_id, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSySM"}],
             [{sysMId,[],["1"]},
              {'OamAccessPoint', [],
               [{oamAccessPointId,[],[Oam_AP_id]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    %ct:pal("OamAP conf: ~p", Conf),
    Names = ['ManagedElement','SystemFunctions','SysM','OamAccessPoint'],
    [Oap_attributes] = struct(Conf, Names),
    %ct:pal("Oap_attributes: ~p", [Oap_attributes]),
    case lists:keyfind(accessPoint, 1, Oap_attributes) of
	{accessPoint, [], [Oap_is_here]} ->
	    ct:pal("The ~s OamAccessPoint points to here: ~p",
		   [Oap_type, Oap_is_here]),
	    true;
	false ->
	    false
    end
end.

add_ipv6_address(Config, IPv6_addr) ->
    ct:pal("Configuring Transport"),
    Me_id = ?config(me_id, Config),
    Router = ?config(router, Config),
    Tn_vlan_port = ?config(tn_vlan_port, Config),
    Gw = ?config(tn_ipv6_gw, Config),
    Mask = ?config(tn_ipv6_mask, Config),
    Encap_int = "ManagedElement=" ++ Me_id ++ ",Transport=1,VlanPort=" ++
                    Tn_vlan_port,
    Address = IPv6_addr ++ "/" ++ Mask,
    Edit =
        {'ManagedElement',
         [{managedElementId,[],[Me_id]},
          {'Transport',[],
           [{transportId,[],["1"]},
            {'Router',
             [{routerId,[],[Router]},
              {'InterfaceIPv6',
               [{interfaceIPv6Id,[],[Tn_vlan_port]},
                {encapsulation,[], [Encap_int]},
                {mtu,[],["9000"]},
                {'AddressIPv6',[],
                 [{addressIPv6Id,[],[Tn_vlan_port]},
                  {address,[],[Address]}]}]},
              {'RouteTableIPv6Static',
               [{routeTableIPv6StaticId,[],["1"]},
                {'Dst',[],
                 [{dst,[],["::/0"]},
                  {dstId,[],["default"]},
                  {'NextHop',[],
                   [{address,[],[Gw]},
                    {nextHopId,[], [Gw]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("Router=~s created InterfaceIPv6=~s",
           [Router, Tn_vlan_port]),
    ct:pal("IPv6/Mask: ~p, Gateway: ~p", [Address, Gw]),
    ok.

netconf(F, A) ->
    %%{ok, _} = ct_netconfc:open(nc1, [{user, "expert"}, {password, "expert"}]),
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    case Res of
        {error, _} ->
            catch ct_netconfc:close_session(nc1),
            Res;
        _ ->
            ok = ct_netconfc:close_session(nc1)
    end,
    Res.

get_ipv6(Config) ->
    case ?config(tn_ipv6, Config) of
	undefined ->
	    mk_ipv6(Config);
	IPv6 ->
	    IPv6
    end.

mk_ipv6(Config) ->
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Tn_ipv6_net = ?config(tn_ipv6_net, Config),
    Get =
	{'ManagedElement',
         [{managedElementId,[],[Me_id]},
          {'Transport',[],
           [{transportId,[],["1"]},
            {'EthernetPort',
             [{ethernetPortId,[],[Tn_port]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','Transport', 'EthernetPort', 'macAddress'],
    Mac_string = 
	case struct(Conf, Names) of
	    [[Mac]] ->
		Mac;
	    _ ->
		ct:fail("Couldn't locate MAC in this data: ~p", [Conf])
	end,
    %%get rid of any starting zeros, i.e. :0123: -> :123:
    [A,B,C,D,E,F] = [integer_to_list(list_to_integer(I, 16), 16) ||
		     I <- string:tokens(Mac_string, ":")],
    IPv6 = Tn_ipv6_net ++ "::" ++ A ++ B ++ ":" ++ C ++ D ++ ":" ++ E ++ F,
    ct:pal("IPv6 created from net: ~p and MAC: ~p -> ~p",
	   [Tn_ipv6_net, Mac_string, IPv6]),
    IPv6.

	
%descend into the structure following list(Names), when only one Name
%remains collect all matching and return them
struct([{Name, _, Next} | T], [Name]) ->
    [Next | struct(T, [Name])];
struct([{_, _, _} | T], [Name]) ->
    struct(T, [Name]);
struct([], _) -> [];
struct([{Name, _, Next} | _], [Name | Names]) ->
    struct(Next, Names);
struct([{_, _, _} | T], Names) ->
    struct(T, Names).

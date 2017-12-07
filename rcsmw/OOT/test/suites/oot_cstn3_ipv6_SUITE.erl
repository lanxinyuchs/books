%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oot_cstn3_ipv6_SUITE.erl %
%%% @author eolaand
%%% @version /main/R11A/3
%%%
%%% @doc ==Test Suite for IPv6 over CSTN vsn3 interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%% @end
%%%
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
%%% Rev        Date       Name        What
%%% R11A/1     2017-09-20 eolaand     Created
%%% ----------------------------------------------------------

-module(oot_cstn3_ipv6_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0,
	 all/0]).

-export([set_and_delete_access_point/1,
	 set_and_delete_alt_access_point/1,
	 ip_change_cb_one_ldn/1,
	 ip_change_cb_two_ldn/1,
	 change_access_point/1,
	 change_alt_access_point/1,
	 update_access_point_ip/1,
 	 update_alt_access_point_ip/1,
 	 subscribe_access_point_dn/1,
	 subscribe_alt_access_point_dn/1]).

-include_lib("common_test/include/ct.hrl").
-include("test_cstn.hrl").
-include("oot_test.hrl").

%% -define(TN_MASK, "/24").
%% -define(TN_MASK_ALT, "/24").
%% -define(TN_GW, "10.67.226.1").
%% -define(TN_GW_ALT, "10.67.233.1").
%% -define(TN_FH_1, "10.67.241.1").
%% -define(TN_FH_2, "10.67.242.1").
%% -define(TN_FH_3, "10.67.243.1").
%% -define(TN_FH_4, "10.67.244.1").

%% -define(TN_IPV6_MASK, "/64").
%% -define(TN_IPV6_MASK_ALT, "/64").
-define(TN_IPV6_NET, "2001:1b70:6281:f100").
-define(TN_IPV6_GW, ?TN_IPV6_NET ++ "::" ++ "2").
-define(TN_IPV6_GW_ALT, ?TN_IPV6_NET ++ "::" ++ "4").
-define(TN_IPV6_GW_NEW, ?TN_IPV6_NET ++ "::" ++ "8").
-define(TN_FH_1, "2001:1b70:6281:f100::138").
-define(TN_FH_2, "2001:1b70:6282:b200::92").
-define(TN_FH_3, "2001:1b70:8210:8b00::68").
-define(TN_FH_4, "2001:1b70:8210:b280::150").

%% 2001:1b70:6281:f100::138
%% "2001:1b70:6282:b200::92"
%% "2001:1b70:8210:8b00::68/64"
%% 10.56.23.203

-define(LIB, oot_test_lib).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 5}},
     {ct_hooks, [{rct_rpc, ?TNODE},
		 %% {rct_snmpmgr, snmp1},
                 {rct_htmllink, []},
                 {rct_netconf, ?NETCNF},
                 {rct_logging,
		  {rabbe_logs, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]
					}}]}},
                 {rct_proxy, [{1, ?IFT_NODE, ssh_lmt_ipv4, du1, username}]},
                 {rct_core, []},
		 {rct_cli, {?CLI,[manual_connect]}},
                 {oot_log_hook, [{nodeId, ?TNODE}, {enabled, false}]}
                 %% {oot_log_hook, [{nodeId, ?TNODE}, {enabled, true}]}
                ]}].

%% @hidden
init_per_suite(Config) ->
    LDN = get_accessPoint(),
    LDNAlt = get_alt_accessPoint(),
    %% ok = create_ipv6_instances(),
    ok = stop_ns_simulator(),
    start_proxy_slave(),
    RestoreValues = {restore_values, 
		     [{access_point_ldn, LDN}, 
		      {access_point_alt_ldn, LDNAlt}]},
    [RestoreValues | Config].


%% @hidden
end_per_suite(Config) ->
    stop_proxy_slave(),
    ok = warm_restart(),
    RestoreVals = ?config(restore_values, Config),
    ok = restore_oap(RestoreVals),
    wait_netconf_up().



%% @hidden
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    flush_proxy(),
    case get_alt_accessPoint() of
	undefined ->
	    ok;
	AltLdn ->
	    ct:pal("Alternative accessPoint = ~p~n", [AltLdn]),
	    ok = delete_alt_accessPoint(),
	    {ok, {cstnDoUnsubscribe, _}} = receive_proxy()
    end,
    case get_accessPoint() of
	undefined ->
	    ok;
	Ldn ->
	    ct:pal("Primary accessPoint = ~p~n", [Ldn]),
	    ok = delete_accessPoint(),
	    {ok, {cstnDoUnsubscribe, _}} = receive_proxy()
    end,
    log_tc_start(TestCase),
    Config.

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    LDNs = [?LDN_IPV6_1, ?LDN_IPV6_2, ?LDN_IPV6_3],
    [unregister_ip_change_cb(LdnAccPoint) || LdnAccPoint <- LDNs],
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},
     {sdc__qual__all__1__group, [], []}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [set_and_delete_access_point,
     set_and_delete_alt_access_point,
     ip_change_cb_one_ldn,
     ip_change_cb_two_ldn,
     change_access_point,
     change_alt_access_point,
     update_access_point_ip,
     update_alt_access_point_ip,
     subscribe_access_point_dn,
     subscribe_alt_access_point_dn
    ].


%%--------------------------------------------------------------------
%% TEST CASE FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Set accessPoint and then delete. Verify all corresponding notifications.
%% @end
%%--------------------------------------------------------------------
set_and_delete_access_point(_Config) ->
    LdnAccPoint = ?LDN_IPV6_1,
    %% IP = get_new_ipv6_addr(),
    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,
    NS = ?NS,

    %% Redirect OOT notifications to test case only
    ok = test_register_cfg_upd_cb(),

    %% Set IFT response 
    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{access_point_address, IP}],
    ok = receive_config_update_item(Expected1),
    
    %% Reset AP
    ok = delete_accessPoint(),

    Expected2 = [{access_point_address,[]}],
    ok = receive_config_update_item(Expected2),

    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy().


%%--------------------------------------------------------------------
%% @doc
%% Set Alternative accessPoint and then delete. 
%% Verify all corresponding notifications.
%% @end
%%--------------------------------------------------------------------
set_and_delete_alt_access_point(_Config) ->
    LdnAccPoint = ?LDN_IPV6_1,
    LdnAltAccPoint = ?LDN_IPV6_2,

    %% IP = get_new_ipv6_addr(),
    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,
    IPAlt = ?TN_IPV6_GW_ALT,
    IPAddrAlt = IPAlt ++ ?TN_IPV6_MASK_ALT,

    NS = ?NS,
    NSBin = ltb(?NS),

    NSAlt = ?ALT_NS,
    NSAltBin = ltb(?ALT_NS),

    %% Redirect notifications to test case only
    ok = test_register_cfg_upd_cb(),

    %% Set IFT response 
    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    %% Verify IFT response 
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{oap_namespace, NSBin},
		 {access_point_address, IP}],
    ok = receive_config_update_item(Expected1),
    
    ok = send_proxy(?SetOamDNtoNsNameRsp, 
			 {LdnAltAccPoint, NSAlt, IPAddrAlt}),

    ok = set_alt_accessPoint(LdnAltAccPoint),

    {ok, {cstnDnToTn, {LdnAltAccPoint, {NSAlt, IPAddrAlt}}}} = 
	receive_proxy(),

    Expected2 = [{oap_alt_namespace, NSAltBin},
		 {access_point_address_alt, IPAlt}],
    ok = receive_config_update_item(Expected2),

    ok = delete_alt_accessPoint(),
    
    Expected3 = [{oap_alt_namespace, <<>>},
		 {access_point_address_alt, []}],
    ok = receive_config_update_item(Expected3),
    
    {ok, {cstnDoUnsubscribe, {LdnAltAccPoint}}} = receive_proxy(),
    
    ok = delete_accessPoint(),
    
    Expected4 = [{oap_namespace, <<>>},
		 {access_point_address, []}],
    ok = receive_config_update_item(Expected4),
    
    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Register a callback for a MoRef and verify that notifications about  
%% updates are sent from OOT
%% @end
%%--------------------------------------------------------------------
ip_change_cb_one_ldn(_Config) ->    
    %% Register a callback for an MoRef and set IFT response
    LdnV4 = ?LDN_IPV6_3,
    IPAddr = ?TN_FH_1 ++ ?TN_IPV6_MASK,
    IPUpdate = ?TN_FH_2 ++ ?TN_IPV6_MASK,
    NSFH = ?FH_NS,

    %% Subscribe to IP updates in OOT
    ok = register_ip_change_cb(LdnV4),

    %% Set IFT response 
    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnV4, NSFH, IPAddr}),

    %% Run get_ns_and_ip to emulate subscription in TN
    {ok, NSFH, IPAddr} = get_ns_and_ip(LdnV4),
    
    %% Check with IFT that a lookup was done
    {ok, {cstnDnToTn, {LdnV4, {NSFH, IPAddr}}}} = receive_proxy(),

    %% Update IP Address    
    ok = send_proxy(?CsTnUpdate, {LdnV4, NSFH, IPUpdate}),

    %% Check that we get a notification
    ok = receive_ip_changed({LdnV4, NSFH, IPUpdate}),

    %% Update back to orig IP Address    
    ok = send_proxy(?CsTnUpdate, {LdnV4, NSFH, IPAddr}),

    %% Check that we get a notification
    ok = receive_ip_changed({LdnV4, NSFH, IPAddr}),

    %% Unregister the callback
    ok = unregister_ip_change_cb(LdnV4),

    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnV4}}} = receive_proxy().


%%--------------------------------------------------------------------
%% @doc
%% Register callbacks for two MoRef and verify that notifications about  
%% updates are sent from OOT
%% @end
%%--------------------------------------------------------------------
ip_change_cb_two_ldn(_Config) ->    
    %% Register a callback for an MoRef and set IFT response
    Ldn1 = ?LDN_IPV6_2,
    Ldn2 = ?LDN_IPV6_3,

    IPAddr1 = ?TN_FH_1 ++ ?TN_IPV6_MASK,
    IPUpdate1 = ?TN_FH_2 ++ ?TN_IPV6_MASK,
    IPAddr2 = ?TN_FH_3 ++ ?TN_IPV6_MASK,
    IPUpdate2 = ?TN_FH_4 ++ ?TN_IPV6_MASK,

    NSFH = ?FH_NS,

    %% Subscribe to IP updates in OOT for the first LDN
    ok = register_ip_change_cb(Ldn1),
    ok = send_proxy(?SetOamDNtoNsNameRsp, {Ldn1, NSFH, IPAddr1}),

    %% Run get_ns_and_ip to emulate subscription in TN
    {ok, NSFH, IPAddr1} = get_ns_and_ip(Ldn1),
    
    %% Check with IFT that a lookup was done
    {ok, {cstnDnToTn, {Ldn1, {NSFH, IPAddr1}}}} = receive_proxy(),

    %% Subscribe to IP updates in OOT for the second LDN
    ok = register_ip_change_cb(Ldn2),
    ok = send_proxy(?SetOamDNtoNsNameRsp, {Ldn2, NSFH, IPAddr2}),

    %% Run get_ns_and_ip to emulate subscription in TN
    {ok, NSFH, IPAddr2} = get_ns_and_ip(Ldn2),
    
    %% Check with IFT that a lookup was done
    {ok, {cstnDnToTn, {Ldn2, {NSFH, IPAddr2}}}} = receive_proxy(),

    %% Update IP Address of the first LDN    
    ok = send_proxy(?CsTnUpdate, {Ldn1, NSFH, IPUpdate1}),

    %% Check that we get a notification
    ok = receive_ip_changed({Ldn1, NSFH, IPUpdate1}),

    %% Update IP Address of the second LDN    
    ok = send_proxy(?CsTnUpdate, {Ldn2, NSFH, IPUpdate2}),

    %% Check that we get a notification
    ok = receive_ip_changed({Ldn2, NSFH, IPUpdate2}),

    %% Update first LDN back to orig IP Address    
    ok = send_proxy(?CsTnUpdate, {Ldn1, NSFH, IPAddr1}),

    %% Check that we get a notification
    ok = receive_ip_changed({Ldn1, NSFH, IPAddr1}),

    %% Update second LDN back to orig IP Address    
    ok = send_proxy(?CsTnUpdate, {Ldn2, NSFH, IPAddr2}),

    %% Check that we get a notification
    ok = receive_ip_changed({Ldn2, NSFH, IPAddr2}),

    %% Unregister the first callback
    ok = unregister_ip_change_cb(Ldn1),

    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {Ldn1}}} = receive_proxy(),

    %% Unregister the second callback
    ok = unregister_ip_change_cb(Ldn2),

    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {Ldn2}}} = receive_proxy().


%%--------------------------------------------------------------------
%% @doc
%% Set AP, and then change it
%% Check that an unsubscribe is done for the first one. <br/>
%% @end
%%--------------------------------------------------------------------
change_access_point(_Config) ->    
    LdnAccPoint = ?LDN_IPV6_1,
    LdnAccPointNew = ?LDN_IPV6_2,

    %% IP = get_new_ipv6_addr(),
    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,
    IPNew = ?TN_IPV6_GW_NEW,
    IPAddrNew = IPNew ++ ?TN_IPV6_MASK,

    NS = ?NS,
    NSBin = ltb(NS),

    %% Redirect OOT notifications to test case only
    ok = test_register_cfg_upd_cb(),

    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{access_point_address, IP},
		 {oap_namespace, NSBin}],
    ok = receive_config_update_item(Expected1),

    %% Change AP, this should trigger an unsubscribe
    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPointNew, NS, IPAddrNew}),

    ok = set_accessPoint(LdnAccPointNew),

    %% Check that an unsubscribe were done.
    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy(),

    {ok, {cstnDnToTn, {LdnAccPointNew, {NS, IPAddrNew}}}} = receive_proxy(),

    Expected2 = [{access_point_address, IPNew}],
    ok = receive_config_update_item(Expected2),

    %% Reset AP
    ok = delete_accessPoint(),

    Expected3 = [{access_point_address,[]},
		 {oap_namespace, <<>>}],
    ok = receive_config_update_item(Expected3),

    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAccPointNew}}} = receive_proxy().


%%--------------------------------------------------------------------
%% @doc
%% Set alternative AP, and then change it
%% Check that an unsubscribe is done for the first one. <br/>
%% @end
%%--------------------------------------------------------------------
change_alt_access_point(_Config) ->    
    LdnAccPoint = ?LDN_IPV6_1,
    LdnAltAccPoint = ?LDN_IPV6_2,
    LdnAltAccPointNew = ?LDN_IPV6_3,

    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,
    IPAlt = ?TN_IPV6_GW_ALT,
    IPAddrAlt = IPAlt ++ ?TN_IPV6_MASK_ALT,
    
    IPAltNew = ?TN_IPV6_GW_NEW,
    IPAddrAltNew = IPAltNew ++ ?TN_IPV6_MASK,

    NS = ?NS,
    NSBin = ltb(NS),
    NSAlt = ?ALT_NS,
    NSAltBin = ltb(NSAlt),

    %% Redirect notifications to test case only
    ok = test_register_cfg_upd_cb(),

    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{oap_namespace, NSBin},
		 {access_point_address, IP}],
    ok = receive_config_update_item(Expected1),
    
    ok = send_proxy(?SetOamDNtoNsNameRsp, 
			 {LdnAltAccPoint, NSAlt, IPAddrAlt}),

    ok = set_alt_accessPoint(LdnAltAccPoint),

    {ok, {cstnDnToTn, {LdnAltAccPoint, {NSAlt, IPAddrAlt}}}} = 
	receive_proxy(),

    Expected2 = [{oap_alt_namespace, NSAltBin},
		 {access_point_address_alt, IPAlt}],
    ok = receive_config_update_item(Expected2),

    %% Change AP, this should trigger an unsubscribe
    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAltAccPointNew, NSAlt, 
					   IPAddrAltNew}),

    ok = set_alt_accessPoint(LdnAltAccPointNew),

    %% Check that an unsubscribe were done.
    {ok, {cstnDoUnsubscribe, {LdnAltAccPoint}}} = receive_proxy(),

    {ok, {cstnDnToTn, {LdnAltAccPointNew, {NSAlt, IPAddrAltNew}}}} = 
	receive_proxy(),

    Expected3 = [{access_point_address_alt, IPAltNew}],
    ok = receive_config_update_item(Expected3),

    ok = delete_alt_accessPoint(),
    
    Expected4 = [{oap_alt_namespace, <<>>},
		 {access_point_address_alt, []}],
    ok = receive_config_update_item(Expected4),
    
    {ok, {cstnDoUnsubscribe, {LdnAltAccPointNew}}} = receive_proxy(),
    
    ok = delete_accessPoint(),
    
    Expected5 = [{oap_namespace, <<>>},
		 {access_point_address, []}],
    ok = receive_config_update_item(Expected5),
    
    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Set primary AP, and register a subscriber to OOT. Change IP of AP
%% and check that we receive notification <br/>
%% @end
%%--------------------------------------------------------------------
update_access_point_ip(_Config) ->    
    LdnAccPoint = ?LDN_IPV6_1,

    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,
    IPNew = ?TN_IPV6_GW_NEW,
    IPAddrNew = IPNew ++ ?TN_IPV6_MASK,

    NS = ?NS,
    NSBin = ltb(NS),

    %% Redirect OOT notifications to test case only
    ok = test_register_cfg_upd_cb(),

    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{access_point_address, IP},
		 {oap_namespace, NSBin}],
    ok = receive_config_update_item(Expected1),

    %% Update IP
    ct:pal("Update accessPoint IP from ~p to ~p~n", [IPAddr, IPAddrNew]),
    ok = send_proxy(?CsTnUpdate, {LdnAccPoint, NS, IPAddrNew}),

    %% Check that we receive notification    
    Expected2 = [{access_point_address, IPNew}],
    ok = receive_config_update(Expected2),

    %% Reset AP
    ok = delete_accessPoint(),

    Expected3 = [{access_point_address,[]},
		 {oap_namespace,<<>>}],
    ok = receive_config_update_item(Expected3),

    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy().

%%--------------------------------------------------------------------
%% @doc
%% Set primary and alt AP, and register a subscriber to OOT. Change IP 
%% of alt AP and check that we receive notification <br/>
%% @end
%%--------------------------------------------------------------------
update_alt_access_point_ip(_Config) ->    
    LdnAccPoint = ?LDN_IPV6_1,
    LdnAltAccPoint = ?LDN_IPV6_2,

    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,
    IPAlt = ?TN_IPV6_GW_ALT,
    IPAddrAlt = IPAlt ++ ?TN_IPV6_MASK_ALT,
    IPNew = ?TN_IPV6_GW_NEW,
    IPAddrNew = IPNew ++ ?TN_IPV6_MASK_ALT,

    NS = ?NS,
    NSBin = ltb(NS),
    NSAlt = ?ALT_NS,
    NSAltBin = ltb(NSAlt),

    %% Redirect notifications to test case only
    ok = test_register_cfg_upd_cb(),

    %% Set IFT CSTN response
    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{oap_namespace, NSBin},
		 {access_point_address, IP}],
    ok = receive_config_update_item(Expected1),
    
    ok = send_proxy(?SetOamDNtoNsNameRsp, 
		    {LdnAltAccPoint, NSAlt, IPAddrAlt}),
    
    ok = set_alt_accessPoint(LdnAltAccPoint),
    
    {ok, {cstnDnToTn, {LdnAltAccPoint, {NSAlt, IPAddrAlt}}}} = receive_proxy(),

    Expected2 = [{oap_alt_namespace, NSAltBin},
		 {access_point_address_alt, IPAlt}],
    ok = receive_config_update_item(Expected2),

    %% Update OAM IP
    ct:pal("Update accessPoint IP from ~p to ~p~n", [IPAddr, IPAddrNew]),
    ok = send_proxy(?CsTnUpdate, {LdnAccPoint, NS, IPAddrNew}),


    %% Check that we receive notification    
    Expected3 = [{access_point_address, IPNew}],
    ok = receive_config_update(Expected3),


    %% Revert OAM IP to previous 
    ct:pal("Update accessPoint IP from ~p to ~p~n", [IPAddrNew, IPAddr]),
    ok = send_proxy(?CsTnUpdate, {LdnAccPoint, NS, IPAddr}),


    %% Check that we receive notification    
    Expected4 = [{access_point_address, IP}],
    ok = receive_config_update(Expected4),


    %% Update OAM Alt IP
    ct:pal("Update Alternative accessPoint IP from ~p to ~p~n", 
	   [IPAddrAlt, IPAddrNew]),
    ok = send_proxy(?CsTnUpdate, {LdnAltAccPoint, NSAlt, IPAddrNew}),


    %% Check that we receive notification    
    Expected5 = [{access_point_address_alt, IPNew}],
    ok = receive_config_update(Expected5),


    %% Revert OAM IP to previous 
    ct:pal("Update Alternative accessPoint IP from ~p to ~p~n", 
	   [IPAddrNew, IPAddrAlt]),
    ok = send_proxy(?CsTnUpdate, {LdnAltAccPoint, NSAlt, IPAddrAlt}),


    %% Check that we receive notification    
    Expected6 = [{access_point_address_alt, IPAlt}],
    ok = receive_config_update(Expected6),

    %% Delete Alternative accessPoint
    ok = delete_alt_accessPoint(),
    
    Expected7 = [{oap_alt_namespace, <<>>},
		 {access_point_address_alt, []}],
    ok = receive_config_update_item(Expected7),
    
    {ok, {cstnDoUnsubscribe, {LdnAltAccPoint}}} = receive_proxy(),
    
    %% Delete accessPoint
    ok = delete_accessPoint(),
    
    Expected8 = [{oap_namespace, <<>>},
		 {access_point_address, []}],
    ok = receive_config_update_item(Expected8),
    
    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to the same Ldn that is used as accessPoint and change IP of that.
%% Check that we receive both config and ip change notifications. <br/>
%% Unregister the subscription and check that no unsubscribe is sent.
%% @end
%%--------------------------------------------------------------------
subscribe_access_point_dn(_Config) ->    
    LdnAccPoint = ?LDN_IPV6_1,
    %% IP = get_new_ipv6_addr(),
    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,

    IPNew = ?TN_IPV6_GW_NEW,
    IPAddrNew = IPNew ++ ?TN_IPV6_MASK,

    NS = ?NS,
    NSBin = ltb(NS),

    %% Redirect notifications to test case only
    ok = test_register_cfg_upd_cb(),

    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{oap_namespace, NSBin},
		 {access_point_address, IP}],
    ok = receive_config_update_item(Expected1),
    
    %% Subscribe   
    register_ip_change_cb(LdnAccPoint),

    %% Update OAM IP
    ct:pal("Update accessPoint IP from ~p to ~p~n", [IPAddr, IPAddrNew]),
    ok = send_proxy(?CsTnUpdate, {LdnAccPoint, NS, IPAddrNew}),


    %% Check that we receive IP change notification    
    ok = receive_ip_changed({LdnAccPoint, NS, IPAddrNew}),

    %% Check that we receive config update notification    
    Expected2 = [{access_point_address, IPNew}],
    ok = receive_config_update(Expected2),

    %% Revert OAM IP to previous 
    ct:pal("Update accessPoint IP from ~p to ~p~n", [IPAddrNew, IPAddr]),
    ok = send_proxy(?CsTnUpdate, {LdnAccPoint, NS, IPAddr}),

    %% Check that we receive IP change notification    
    ok = receive_ip_changed({LdnAccPoint, NS, IPAddr}),

    %% Check that we receive notification    
    Expected3 = [{access_point_address, IP}],
    ok = receive_config_update(Expected3),

    unregister_ip_change_cb(LdnAccPoint),

    %% Check that we don't receive notification    
    ct:log("Check that no unsubscribe notification is received~n", []), 
    {error, timeout} = receive_proxy(3000),

    %% Subscribe again   
    register_ip_change_cb(LdnAccPoint),

    %% Reset AP
    ok = delete_accessPoint(),

    Expected4 = [{oap_namespace, <<>>},
		 {access_point_address, []}],
    ok = receive_config_update_item(Expected4),
    
    %% Check that we don't receive notification    
    ct:log("Check that no unsubscribe notification is received~n", []), 
    {error, timeout} = receive_proxy(3000),

    unregister_ip_change_cb(LdnAccPoint),

    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy().

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to the same Ldn that is used as accessPoint and change IP of that.
%% Check that we receive both config and ip change notifications. <br/>
%% Unregister the subscription and check that no unsubscribe is sent.
%% @end
%%--------------------------------------------------------------------
subscribe_alt_access_point_dn(_Config) ->    
    LdnAccPoint = ?LDN_IPV6_1,
    LdnAltAccPoint = ?LDN_IPV6_2,

    IP = ?TN_IPV6_GW,
    IPAddr = IP ++ ?TN_IPV6_MASK,
    IPAlt = ?TN_IPV6_GW_ALT,
    IPAddrAlt = IPAlt ++ ?TN_IPV6_MASK_ALT,
    IPNew = ?TN_IPV6_GW_NEW,
    IPAddrNew = IPNew ++ ?TN_IPV6_MASK_ALT,

    NS = ?NS,
    NSBin = ltb(NS),
    NSAlt = ?ALT_NS,
    NSAltBin = ltb(NSAlt),

    %% Redirect notifications to test case only
    ok = test_register_cfg_upd_cb(),

    ok = send_proxy(?SetOamDNtoNsNameRsp, {LdnAccPoint, NS, IPAddr}),

    ok = set_accessPoint(LdnAccPoint),
    {ok, {cstnDnToTn, {LdnAccPoint, {NS, IPAddr}}}} = receive_proxy(),

    Expected1 = [{oap_namespace, NSBin},
		 {access_point_address, IP}],
    ok = receive_config_update_item(Expected1),
    
    ok = send_proxy(?SetOamDNtoNsNameRsp, 
			 {LdnAltAccPoint, NSAlt, IPAddrAlt}),

    ok = set_alt_accessPoint(LdnAltAccPoint),

    {ok, {cstnDnToTn, {LdnAltAccPoint, {NSAlt, IPAddrAlt}}}} = 
	receive_proxy(),

    Expected2 = [{oap_alt_namespace, NSAltBin},
		 {access_point_address_alt, IPAlt}],
    ok = receive_config_update_item(Expected2),

    %% Subscribe   
    register_ip_change_cb(LdnAltAccPoint),

    %% Update OAM Alt IP
    ct:pal("Update Alternative accessPoint IP from ~p to ~p~n", 
	   [IPAddrAlt, IPAddrNew]),
    ok = send_proxy(?CsTnUpdate, {LdnAltAccPoint, NSAlt, IPAddrNew}),

    %% Check that we receive IP change notification    
    ok = receive_ip_changed({LdnAltAccPoint, NSAlt, IPAddrNew}),

    %% Check that we receive notification    
    Expected3 = [{access_point_address_alt, IPNew}],
    ok = receive_config_update(Expected3),

    %% Revert OAM Alt IP to previous 
    ct:pal("Update Alternative accessPoint IP from ~p to ~p~n", 
	   [IPAddrNew, IPAddrAlt]),
    ok = send_proxy(?CsTnUpdate, {LdnAltAccPoint, NSAlt, IPAddrAlt}),

    %% Check that we receive IP change notification    
    ok = receive_ip_changed({LdnAltAccPoint, NSAlt, IPAddrAlt}),

    %% Check that we receive notification    
    Expected4 = [{access_point_address_alt, IPAlt}],
    ok = receive_config_update(Expected4),

    unregister_ip_change_cb(LdnAltAccPoint),

    %% Check that we don't receive notification    
    ct:log("Check that no unsubscribe notification is received~n", []), 
    {error, timeout} = receive_proxy(3000),

    %% Subscribe again
    register_ip_change_cb(LdnAltAccPoint),

    %% Delete Alternative accessPoint
    ok = delete_alt_accessPoint(),
    
    Expected5 = [{oap_alt_namespace, <<>>},
		 {access_point_address_alt, []}],
    ok = receive_config_update_item(Expected5),
    
    %% Check that we don't receive notification    
    ct:log("Check that no unsubscribe notification is received~n", []), 
    {error, timeout} = receive_proxy(3000),

    unregister_ip_change_cb(LdnAltAccPoint),

    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAltAccPoint}}} = receive_proxy(),
    
    %% Delete accessPoint
    ok = delete_accessPoint(),
    
    Expected6 = [{oap_namespace, <<>>},
		 {access_point_address, []}],
    ok = receive_config_update_item(Expected6),
    
    %% Check that an unsubscribe is done.
    {ok, {cstnDoUnsubscribe, {LdnAccPoint}}} = receive_proxy(),
    ok.


%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Configure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
stop_ns_simulator() ->
    ?LIB:stop_ns_simulator().


%% start_ns_simulator() ->
%%     ?LIB:start_ns_simulator().


warm_restart() ->
    ?LIB:warm_restart().


wait_netconf_up() ->
    ?LIB:wait_netconf_up().


%% create_ipv6_instances() ->
%%     ?LIB:create_ipv6_instances().


%% get_new_ipv6_addr() ->
%%     ?LIB:get_new_ipv6_addr().


restore_oap(RestoreVals) ->
    OrigLDN = proplists:get_value(access_point_ldn, RestoreVals),
    OrigAltLDN = proplists:get_value(access_point_alt_ldn, RestoreVals),
    ?LIB:restore_access_point(OrigLDN, OrigAltLDN).


set_accessPoint(AccessPoint) ->
    ?LIB:set_accessPoint(AccessPoint).


get_accessPoint() ->
    ?LIB:get_accessPoint().


delete_accessPoint() ->
    ?LIB:delete_accessPoint().


set_alt_accessPoint(AccessPoint) ->
    ?LIB:set_alt_accessPoint(AccessPoint).


get_alt_accessPoint() ->
    ?LIB:get_alt_accessPoint().


delete_alt_accessPoint() ->
    ?LIB:delete_alt_accessPoint().


get_ns_and_ip(MoRef) ->
    ?LIB:get_ns_and_ip(MoRef).


register_ip_change_cb(MoRef) ->
    ?LIB:register_ip_change_cb(MoRef).
    

unregister_ip_change_cb(MoRef) ->
    ?LIB:unregister_ip_change_cb(MoRef).
    

receive_ip_changed(Expected) ->
    ?LIB:receive_ip_changed(Expected).


test_register_cfg_upd_cb() ->
    ?LIB:test_register_cfg_upd_cb().


receive_config_update(Expected) ->
    ?LIB:receive_config_update(Expected).


receive_config_update_item(Expected) ->
    ?LIB:receive_config_update_item(Expected).


start_proxy_slave() ->
    ?LIB:start_proxy_slave().


send_proxy(Msg, Data) ->
    {Res, _} = ?LIB:send_proxy(Msg, Data),
    Res.


receive_proxy() ->
    ?LIB:receive_proxy().


receive_proxy(Timeout) ->
    ?LIB:receive_proxy(Timeout).
	

flush_proxy() ->
    ?LIB:flush_proxy().


stop_proxy_slave() ->
    ?LIB:stop_proxy_slave().
	

%% rpc_call(M, F, A) ->
%%     ?LIB:rpc_call(M, F, A).

log_tc_start(TC) ->
    ?LIB:log_tc_start(TC).
    

ltb(L) ->    
    ?LIB:ltb(L).

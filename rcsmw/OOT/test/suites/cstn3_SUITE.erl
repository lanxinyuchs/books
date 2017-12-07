%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cstn3_SUITE.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2017
%%% @version /main/R10A/R11A/8
%%%
%%% @doc ==Basic Test Suite for the CSTN interface on SUT.==
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
%%% R10A/1     2017-06-22 ecaiyan     Created
%%% R11A/1     2017-07-29 eolaand     Add TC:s for unsubscribe
%%% ----------------------------------------------------------

-module(cstn3_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0,
	 all/0]).

-export([changed_ipAddr_trap_ack/1,
	 changed_ipAddr_trap_no_ack/1,
	 changed_ipAddr_trap_restart/1,
	 unsubscribe_register_and_unregister_ip_change_cb/1,
	 unsubscribe_change_access_point/1,
	 unsubscribe_change_alt_access_point/1,
	 update_primary_access_point/1,
 	 update_alt_access_point/1,
 	 update_subscribed_access_point/1]).

-include_lib("common_test/include/ct.hrl").
-include("test_cstn.hrl").

-define(TNODE, rpc).
-define(NETCNF, nc1).
-define(CLI, c1).
-define(IFT_NODE, node1).
-define(IFT_NAME, cstn1).
-define(COMTOP, {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").
-define(DELETE, [{'xmlns:nc', ?NC_NAMESPACE}, {'nc:operation', "delete"}]).
-define(SLEEP_AFTER_EDIT, timer:seconds(10)).
-define(RPC_CALL_TIMEOUT, 10000).
-define(IFT_NOTIFICATION_TIMEOUT, 10000).
-define(OOT_NOTIFICATION_TIMEOUT, 10000).
-define(CSTN_NOTIFICATION_TIMEOUT, 10000).

-define(LDN_IPV4_1, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1").
-define(LDN_IPV4_2, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=2,AddressIPv4=1").
-define(LDN_IPV4_3, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=3,AddressIPv4=1").

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
    [{timetrap, {minutes, 300}},
     {ct_hooks, [{rct_rpc, ?TNODE},
		 {rct_snmpmgr, snmp1},
                 {rct_htmllink, []},
                 {rct_netconf, ?NETCNF},
                 {rct_logging,
		  {rabbe_logs, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]
					}}]}},
                 {rct_proxy, [{1, node1, ssh_lmt_ipv4, du1, username}]},
                 {rct_core, []},
		 {rct_cli, {?CLI,[manual_connect]}},
                 {oot_log_hook, [{nodeId, ?TNODE}]}
                ]}].

%% @hidden
init_per_suite(Config) ->
    ok = configure_snmp(),
    ok = create_ip_instances(),

    Config.

%% @hidden
end_per_suite(_Config) ->
    ok = deconfigure_snmp(),
    ok = rct_proxy:exit_master(node1).

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    ok = stop_ns_simulator(),
    {ok, client_started} = rct_proxy:start_proxy(?IFT_NODE, ?IFT_NAME, ?CSTN),
    Config.

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok = warm_restart(),
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
    [changed_ipAddr_trap_restart,
     changed_ipAddr_trap_ack,
     changed_ipAddr_trap_no_ack,
     unsubscribe_register_and_unregister_ip_change_cb,
     unsubscribe_change_access_point,
     unsubscribe_change_alt_access_point,
     update_primary_access_point,
     update_alt_access_point,
     update_subscribed_access_point
    ].


%%--------------------------------------------------------------------
%% TEST CASE FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Call initialize3. <br/>
%% When expected trap is received, no acknowledgment sent.<br/>
%% Restart node. Wait for resending of  trap. <br/>
%% @end
%%--------------------------------------------------------------------
changed_ipAddr_trap_restart(_Config) ->

    LdnV4 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4),

    %% test init3 and trap
    IPAdd = get_new_ipv4_addr(),
    {ok, _Number} = send_proxy(?SetOamDNtoNsNameRsp, {LdnV4, "", IPAdd}),

    {ok, _Handle} = send_proxy(?CsTnInitialize3,{}),
    {ok,{cstnDnToTn,{LdnV4, _}}} = receive_proxy(),

    Timeout = 180,
    ok = wait_for_eriChangeIPAddressEvent(Timeout),
    {ok, _EriChangeIPAddressAckFdn, _EriChangeIPAddressAckAttributeName,
     _EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),

    %% not acknowledging the trap
    ct:pal("Not acknowledging trap", []),

    %% restart (note that the namespace simulator replaces IFT after restart)
    cold_restart(),

    %% after restart, must connect
    ok = cli_connect(),

    %%check that  trap is being resent after restart
    Timeout2 = 480,
    ok = wait_for_eriChangeIPAddressEvent(Timeout2),
    {ok, EriChangeIPAddressAckFdn2, EriChangeIPAddressAckAttributeName2,
     EriChangeIPAddressAckAttributeValue2} = check_eriChangeIPAddressEvent(),

    ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn2,
				     EriChangeIPAddressAckAttributeName2,
				     EriChangeIPAddressAckAttributeValue2),

    ok = delete_accessPoint().

%%--------------------------------------------------------------------
%% @doc
%% Call initialize3. Check expected trap is received. <br/>
%% @end
%%--------------------------------------------------------------------
changed_ipAddr_trap_ack(_Config) ->

    LdnV4 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4),

    %% test init3 and trap
    IPAdd = get_new_ipv4_addr(),
    {ok, _Number} = send_proxy(?SetOamDNtoNsNameRsp, {LdnV4, "", IPAdd}),

    {ok, _Handle} = send_proxy(?CsTnInitialize3, {}),
    {ok, {cstnDnToTn, {LdnV4, _}}} = receive_proxy(),

    Timeout = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout),
    {ok, EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName,
     EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),
    ok = cli_connect(),

    ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn,
				     EriChangeIPAddressAckAttributeName,
				     EriChangeIPAddressAckAttributeValue),

    %%check that no more traps are sent after acknowledgment
    Timeout2 = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout2),
    error = check_eriChangeIPAddressEvent(),

    %%test update
    %%{ok, _Result} = send_proxy(?CsTnUpdate, {LdnV4, "", IPAdd}),

    ok = delete_accessPoint(),
    {ok, {cstnDoUnsubscribe, {LdnV4}}} = receive_proxy(),
    ok = rct_cli:disconnect(?CLI).

%%--------------------------------------------------------------------
%% @doc
%% Call initialize3. <br/>
%% When expected trap is received, no acknowledgment sent.<br/>
%% Wait for resending of  trap. <br/>
%% @end
%%--------------------------------------------------------------------
changed_ipAddr_trap_no_ack(_Config) ->

    LdnV4 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4),

    %% test init3 and trap
    IPAdd = get_new_ipv4_addr(),
    {ok, _Number} = send_proxy(?SetOamDNtoNsNameRsp, {LdnV4, "", IPAdd}),

    {ok, _Handle} = send_proxy(?CsTnInitialize3, {}),
    {ok, {cstnDnToTn, {LdnV4, _}}} = receive_proxy(),

    Timeout = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout),
    {ok, EriChangeIPAddressAckFdn, 
     EriChangeIPAddressAckAttributeName,
     EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),

    %% not acknowledging the trap
    ct:pal("Not acknowledging trap", []),

    %%check that  trap is being resent
    Timeout2 = 300,
    ok = wait_for_eriChangeIPAddressEvent(Timeout2),
    {ok, EriChangeIPAddressAckFdn, 
     EriChangeIPAddressAckAttributeName,
     EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),

    ok = cli_connect(),

    ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn,
				     EriChangeIPAddressAckAttributeName,
				     EriChangeIPAddressAckAttributeValue),

    ok = delete_accessPoint(),
    {ok,{cstnDoUnsubscribe,{LdnV4}}} = receive_proxy(),
    ok = rct_cli:disconnect(?CLI).


%%--------------------------------------------------------------------
%% @doc
%% Register a callback for a mo, and remove it. 
%% Check that an unsubscribe is done. <br/>
%% @end
%%--------------------------------------------------------------------
unsubscribe_register_and_unregister_ip_change_cb(_Config) ->    
    %% Initalize CSTN
    {ok, _Handle} = send_proxy(?CsTnInitialize3, {}),
    _ = receive_proxy(),

    %% Wait a second to make sure CSTN is initialized
    timer:sleep(1000), 

    %% Register a callback for an MO and set IFT response
    LdnV4 = ?LDN_IPV4_1,
    IPAdd = get_new_ipv4_addr(),
    rpc_call(ootI, register_ip_change_cb, [LdnV4, ootI, [self()]]),
    {ok, _Number} = send_proxy(?SetOamDNtoNsNameRsp, {LdnV4, "", IPAdd}),

    %% Run get_ns_and_ip to subscribe
    IP_bin = list_to_binary(IPAdd),
    {ok, _NS, IP_bin}  = rpc_call(ootI, get_ns_and_ip, [LdnV4]),

    %% Update IP Address    
    {ok, _Res} = send_proxy(?CsTnUpdate, {LdnV4, "", "127.0.0.1"}),

    %% Check with IFT that a lookup were done
    {ok,{cstnDnToTn,{LdnV4, _}}} = receive_proxy(),

    %% Check that we get a notification
    ok = receive_ip_changed({LdnV4, "", "127.0.0.1"}),

    %% Unregister the callback
    rpc_call(ootI, unregister_ip_change_cb, [LdnV4, ootI]),

    %% Check that an unsubscribe were done.
    {ok, {cstnDoUnsubscribe, {LdnV4}}} = receive_proxy(),

    %% Reset AP
    ok = delete_accessPoint(),
    ok = rct_cli:disconnect(?CLI).


%%--------------------------------------------------------------------
%% @doc
%% Set AP, and then change it
%% Check that an unsubscribe is done for the first one. <br/>
%% @end
%%--------------------------------------------------------------------
unsubscribe_change_access_point(_Config) ->    

    %% Set AP
    LdnV4_1 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4_1),

    %% Initalize CSTN
    {ok,_Handle} = send_proxy(?CsTnInitialize3, {}),
    {ok, {cstnDnToTn, {LdnV4_1, _}} = Recv1} = receive_proxy(),
    ct:pal("Received: ~p", [Recv1]),

    %% Wait a second to make sure CSTN is initialized
    timer:sleep(1000), 

    %% Change AP, this should trigger an unsubscribe
    LdnV4_2 = ?LDN_IPV4_2,
    ok = set_accessPoint(LdnV4_2),

    %% Check that an unsubscribe were done.
    {ok, {cstnDoUnsubscribe, {LdnV4_1}}} = receive_proxy(),

    %% Check that NS and IP for the new DN is requested
    {ok, {cstnDnToTn, {LdnV4_2, _}} = Recv2} = receive_proxy(),
    ct:pal("Received: ~p", [Recv2]),

    %% Reset AP
    ok = delete_accessPoint(),

    %% Check that an unsubscribe were done.
    {ok, {cstnDoUnsubscribe, {LdnV4_2}}} = receive_proxy(),

    ok = rct_cli:disconnect(?CLI).


%%--------------------------------------------------------------------
%% @doc
%% Set alternative AP, and then change it
%% Check that an unsubscribe is done for the first one. <br/>
%% @end
%%--------------------------------------------------------------------
unsubscribe_change_alt_access_point(_Config) ->    

    %% Set AP
    LdnV4_1 = ?LDN_IPV4_1,
    LdnV4_2 = ?LDN_IPV4_2,
    %% LdnV4_3 = ?LDN_IPV4_3,

    %% Initalize CSTN
    ct:pal("Init cstn"),
    {ok, _Handle} = send_proxy(?CsTnInitialize3,{}),
    timer:sleep(3000),

    IPAdd = get_new_ipv4_addr(),
    ct:pal("IP: ~p", [IPAdd]),

    ct:pal("Set IP of primary AP"),
    {ok, _Number} = send_proxy(?SetOamDNtoNsNameRsp, {"TEST A", "", IPAdd}),
    %% timer:sleep(5000),

    ct:pal("Set primary AP"),
    ok = set_accessPoint(LdnV4_1),
    ct:pal("Expect to receive lookup of primary"),

    %% Check that NS and IP for the primary DN is requested
    {ok, {cstnDnToTn, {LdnV4_1, _}} = Recv1} = receive_proxy(),
    ct:pal("Received: ~p", [Recv1]),

    ct:pal("Set IP of alt AP"),
    {ok, _Number} = 
	send_proxy(?SetOamDNtoNsNameRsp, {"TEST B", "", "127.0.0.1"}),

    ct:pal("Set alternative AP"),
    ok = set_alt_accessPoint(LdnV4_2),
    ct:pal("Expect to receive lookup of alternative"),

    %% Check that NS and IP for the primary DN is requested
    {ok, {cstnDnToTn, {LdnV4_2, _}} = Recv2} = receive_proxy(),
    ct:pal("Received: ~p", [Recv2]),

    %% ct:pal("Expect to receive lookup of primary and alt"),
    %% ct:pal("Received2: ~p", [rct_proxy:receive_proxy(1000)]), % We receive a notification about a DN lookup
    %% ct:pal("Received3: ~p", [rct_proxy:receive_proxy(1000)]), % We receive a notification about a DN lookup
    %% ct:pal("Received4: ~p", [rct_proxy:receive_proxy(1000)]), 
    %% ct:pal("Received5: ~p", [rct_proxy:receive_proxy(1000)]), 


    %% Wait a second to make sure IFT_App is started
						%ct:pal("Sleep 1 sec"),
						%timer:sleep(1000), 


    %% Change alt AP, this should trigger an unsubscribe
						%ct:pal("Change alternative AP"),
						%ok = set_alt_accessPoint(LdnV4_2),
						%ct:pal("Received6: ~p", [rct_proxy:receive_proxy(1000)]), % We receive a notification about a DN lookup
						%ct:pal("Received7: ~p", [rct_proxy:receive_proxy(10000)]), 


    %% Change AP, this should trigger an unsubscribe
						%ct:pal("Change primary AP"),
						%ok = set_accessPoint(LdnV4_3),

						%ct:pal("Received8: ~p", [rct_proxy:receive_proxy(1000)]), % We receive a notification about a DN lookup
						%ct:pal("Received9: ~p", [rct_proxy:receive_proxy(1000)]), 


    %% Check that an unsubscribe were done.
						%{ok, {cstnDoUnsubscribe, {LdnV4_1}}} = receive_proxy(),


    %% Reset AP
    ct:pal("Reset"),
    ok = delete_alt_accessPoint(),
    {ok, {cstnDoUnsubscribe, {LdnV4_2}}} = receive_proxy(),
    %% ct:pal("Received10: ~p", [rct_proxy:receive_proxy(1000)]), 
    %% ct:pal("Received11: ~p", [rct_proxy:receive_proxy(1000)]), 
    timer:sleep(20000),

    ok = delete_accessPoint(),
    {ok, {cstnDoUnsubscribe, {LdnV4_1}}} = receive_proxy(),
    %% ct:pal("Received12: ~p", [rct_proxy:receive_proxy(1000)]), 
    %% ct:pal("Received13: ~p", [rct_proxy:receive_proxy(1000)]), 
    timer:sleep(20000),

    ok = rct_cli:disconnect(?CLI).


%%--------------------------------------------------------------------
%% @doc
%% Set primary and alt AP, and register a subscriber to OOT. Change IP 
%% of alt AP and check that we recive notification <br/>
%% @end
%%--------------------------------------------------------------------
update_alt_access_point(_Config) ->    

    %% Set AP
    LdnV4_1 = ?LDN_IPV4_1,
    LdnV4_2 = ?LDN_IPV4_2,
    ok = set_accessPoint(LdnV4_1),
    ok = set_alt_accessPoint(LdnV4_2),

    %% Initalize CSTN
    {ok,_Handle} = send_proxy(?CsTnInitialize3, {}),
    {ok, {cstnDnToTn, {LdnV4_1, _}}} = receive_proxy(),
    {ok, {cstnDnToTn, {LdnV4_2, _}}} = receive_proxy(),

    %% Wait a second to make sure CSTN is initialized
    timer:sleep(1000), 


    %% Subscribe   
    Fun = rpc_call(ootI, get_cfg_upd_cb_fun, [self()]),
    rpc_call(ootServer, test_register_cfg_upd_cb, [Fun, self()]),



    %% Update IP
    {ok,_Handle2} = send_proxy(?CsTnUpdate, {LdnV4_2, "", "127.0.0.1"}),

    %% Check that we recive notification    
    ok = receive
	     {oot_config_update, [{access_point_address_alt,"127.0.0.1"}, 
				  {ipv4_address_alt,"127.0.0.1"}]} -> 
		 ok
	 after
	     10000 -> error
	 end,


    %% Reset AP
    ok = delete_alt_accessPoint(),
    ok = receive
	     {oot_config_update,[{ipv4_address_alt,[]},{access_point_address_alt,[]}]} -> ok
	 after
	     10000 -> error
	 end,
    ok = receive
	     {oot_config_update,[{oap_namespace,<<>>},{oap_alt_namespace,<<>>}]} -> ok
	 after
	     10000 -> error
	 end,

    {ok, {cstnDoUnsubscribe, {LdnV4_2}}} = receive_proxy(),


    ok = delete_accessPoint(),

    ok = receive
	     {oot_config_update,[{ipv4_address,[]}]} -> 
		 ok;
	     {oot_config_update,[{oap_namespace,<<>>},
				 {oap_alt_namespace,<<>>}]} -> 
		 ok;
	     {oot_config_update,Update1} -> 
		 ct:pal("Received unexpected config_update: ~p~n",[Update1]),
		 error
	 after
	     10000 -> error
	 end,
    %% ok = receive
    %% 	     {oot_config_update,[{ipv4_address,[]}]} -> 
    %% 		 ok;
    %% 	     {oot_config_update,[{oap_namespace,<<>>},
    %% 				 {oap_alt_namespace,<<>>}]} -> 
    %% 		 ok;
    %% 	     {oot_config_update,Update2} -> 
    %% 		 ct:pal("Received unexpected config_update: ~p~n",[Update2]),
    %% 		 error
    %% 	 after
    %% 	     10000 -> error
    %% 	 end,
    %% ok = receive
    %% 	     {oot_config_update,[{oap_namespace,<<>>},{oap_alt_namespace,<<>>}]} -> ok
    %% 	 after
    %% 	     10000 -> error
    %% 	 end,

    {ok, {cstnDoUnsubscribe, {LdnV4_1}}} = receive_proxy(),

    ok = rct_cli:disconnect(?CLI).

%%--------------------------------------------------------------------
%% @doc
%% Set primary AP, and register a subscriber to OOT. Change IP of AP
%% and check that we recive notification <br/>
%% @end
%%--------------------------------------------------------------------
update_primary_access_point(_Config) ->    
    %% Set AP
    LdnV4_1 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4_1),

    %% Initalize CSTN
    {ok,_Handle} = send_proxy(?CsTnInitialize3, {}),
    {ok, {cstnDnToTn, {LdnV4_1, _}}} = receive_proxy(),

    %% Wait a second to make sure CSTN is initialized
    timer:sleep(1000), 


    %% Subscribe   
    Fun = rpc_call(ootI, get_cfg_upd_cb_fun, [self()]),
    rpc_call(ootServer, test_register_cfg_upd_cb, [Fun, self()]),


    %% Update IP
    {ok,_Handle2} = send_proxy(?CsTnUpdate, {LdnV4_1, "", "127.0.0.1"}),


    %% Check that we recive notification    
    Expected1 = [{access_point_address,"127.0.0.1"}, 
		 {ipv4_address,"127.0.0.1"}],
    ok = receive_config_update(Expected1),
    %% ok = receive
    %% 	     {oot_config_update, } -> ok
    %% 	 after
    %% 	     10000 -> error
    %% 	 end,


    %% Reset AP
    ok = delete_accessPoint(),
    Expected2 = [{ipv4_address,[]},{access_point_address,[]}],
    ok = receive_config_update(Expected2),
    %% ok = receive
    %% 	     {oot_config_update,} -> ok 
    %% 	 after
    %% 	     10000 -> error
    %% 	 end,
    Expected3 = [{oap_namespace,<<>>},{oap_alt_namespace,<<>>}],
    ok = receive_config_update(Expected3),
    %% ok = receive
    %% 	     {oot_config_update,} -> ok
    %% 	 after
    %% 	     10000 -> error
    %% 	 end,

    {ok, {cstnDoUnsubscribe, {LdnV4_1}}} = receive_proxy(),

    ok = rct_cli:disconnect(?CLI).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to an Ldn and chage IP of that. Check that we recive an
%% notification and trap <br/>
%% @end
%%--------------------------------------------------------------------
update_subscribed_access_point(_Config) ->    

    %% Set AP
    LdnV4_1 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4_1),

    %% Initalize CSTN
    {ok,_Handle} = send_proxy(?CsTnInitialize3, {}),
    {ok, {cstnDnToTn, {LdnV4_1, _}}} = receive_proxy(),

    %% Wait a second to make sure CSTN is initialized
    timer:sleep(1000), 


    %% Subscribe   
    rpc_call(ootI, register_ip_change_cb, [LdnV4_1, ootI, [self()]]),
    rpc_call(ootI, unregister_ip_change_cb, [LdnV4_1, ootI]),

    %% Check that we don't recive notification    
    ct:log("Check that no unsubscribe notification is received~n", []), 
    {error, timeout} = receive_proxy(3000),


    %% Update IP
    {ok, _} = send_proxy(?CsTnUpdate, {LdnV4_1, "", "127.0.0.1"}),


    %% Check trap
    Timeout = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout),
    {ok, EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName,
     EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),
    ok = cli_connect(),

    ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn,
				     EriChangeIPAddressAckAttributeName,
				     EriChangeIPAddressAckAttributeValue),


    %% Reset AP
    ok = delete_accessPoint(),

    %% Check that an unsubscribe were done.
    {ok, {cstnDoUnsubscribe, {LdnV4_1}}} = receive_proxy(),

    ok = rct_cli:disconnect(?CLI).


%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Configure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
configure_snmp() ->
    {{mgr_ip, MgrIp},
     {mgr_port, MgrPort},
     {agent_ip, AgentIp},
     {agent_port, AgentPort}} = rct_snmpmgr:get_mgr_data(snmp1),

    ct:pal("Configure SNMP\n mgr_ip: ~p,\n mgr_port: ~p,\n agent_ip: ~p,\n agent_port: ~p",
	   [MgrIp, MgrPort, AgentIp, AgentPort]),

    SimOrTarget = os:getenv("SIM_OR_TARGET"),

    try
	{ok,_} = open_trans(),
	Request = configure_snmp_req(SimOrTarget,
				     {MgrIp, MgrPort, AgentIp, AgentPort}),
	ct_netconfc:edit_config(?NETCNF,running,Request),
	ok = close_trans()
    catch
        _:Reason ->
            close_trans(),
            ct:fail(Reason)
    end,

    timer:sleep(10000), % Seems necessary before start sending traps

    ok.

%% @hidden
configure_snmp_req("sim", {MgrIp, MgrPort, _AgentIp, _AgentPort}) ->
    {'ManagedElement',[?COMTOP],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	{'SysM',[{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	 [{sysMId,[],["1"]},
	  {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	   [{snmpId,[],["1"]},
	    {administrativeState,[],["UNLOCKED"]},
	    {'SnmpTargetV2C',
	     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	     [{snmpTargetV2CId,[],["1"]},
	      {address,[],[MgrIp]},
	      {community,[],["public"]},
	      {transportMethod,[],["TRAP"]},
	      {port,[],[MgrPort]}]}]}]}]}]};
configure_snmp_req("target", {MgrIp, MgrPort, AgentIp, AgentPort}) ->
    {'ManagedElement',[?COMTOP],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	{'SysM',[{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	 [{sysMId,[],["1"]},
	  {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	   [{snmpId,[],["1"]},
	    {administrativeState,[],["UNLOCKED"]},
	    {agentAddress,
	     [{struct,"HostAndPort"}],
	     [{host,[],[AgentIp]},
	      {port,[],[AgentPort]}]},
	    {'SnmpTargetV2C',
	     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	     [{snmpTargetV2CId,[],["1"]},
	      {address,[],[MgrIp]},
	      {informRetryCount,[],["3"]},
	      {transportMethod,[],["INFORM"]},
	      {community,[],["public"]},
	      {port,[],[MgrPort]}]}]}]}]}]}.

%% @hidden
deconfigure_snmp() ->
    SimOrTarget = os:getenv("SIM_OR_TARGET"),

    ct:pal("Deconfigure SNMP", []),
    try
	{ok,_} = open_trans(),
	Request = deconfigure_snmp_req(SimOrTarget),
	ct_netconfc:edit_config(?NETCNF,running,Request),
	ok = close_trans()
    catch
        _:Reason ->
            close_trans(),
            ct:fail(Reason)
    end,

    ok.

%% @hidden
deconfigure_snmp_req("sim") ->
    {'ManagedElement',[?COMTOP],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	{'SysM',[{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	 [{sysMId,[],["1"]},
	  {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	   [{snmpId,[],["1"]},
	    {'SnmpTargetV2C',
	     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
	      {'xmlns:nc',?NC_NAMESPACE},
	      {'nc:operation',"delete"}],[{snmpTargetV2CId,[],["1"]}]}]
	  }]}]}]};
deconfigure_snmp_req("target") ->
    {'ManagedElement',[?COMTOP],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	{'SysM',[{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	 [{sysMId,[],["1"]},
	  {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	   [{snmpId,[],["1"]},
	    {agentAddress,
	     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
	      {'xmlns:nc',?NC_NAMESPACE},
	      {'nc:operation',"delete"}],[]},
	    {'SnmpTargetV2C',
	     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
	      {'xmlns:nc',?NC_NAMESPACE},
	      {'nc:operation',"delete"}],[{snmpTargetV2CId,[],["1"]}]}]
	  }]}]}]}.

%% @hidden
stop_ns_simulator() ->
    ct:pal("Stopping namespace_simulator", []),
    rpc_call(appmServer, stop_lm, ["namespace_simulator"]),
    ok.

%% @hidden
warm_restart() ->
    ct:pal("Warm restart", []),
    rpc_call(appmI, restart_node, [warm, "ManualRestart"]),
    ok = timer:sleep(20000),
    ok.

%% @hidden
cold_restart() ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ct:pal("Cold restart", []),
    rpc_call(appmI, restart_node, [cold, "ManualRestart"]),
    wait_for_node_state(ErlNode, down),
    wait_for_node_state(ErlNode, up),
    ok.

%% @hidden
create_ip_instances() ->
    IPv4 = get_new_ipv4_addr(),
    {ok,_} = open_trans(),
    create_ipv4_instance(IPv4),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

%% @hidden
get_new_ipv4_addr() ->
    case is_target() of
        true ->
            {ok, IP} = rct_cli_coli_common:get_ip(?CLI),
            IP;
        false ->
	    {ok, Host} = inet:gethostname(),
	    {ok, IP} = inet:getaddr(Host, inet),
	    inet:ntoa(IP)
    end.

%% @hidden
is_target() ->
    "target" == os:getenv("SIM_OR_TARGET").

%% @hidden
open_trans() ->
    %% 10 min
    open_trans(600000).

%% @hidden
open_trans(Timeout) when Timeout < 0 ->
    ct:fail("Could not setup a NETCONF connection within max timeout.");
open_trans(Timeout) ->
    case ct_netconfc:open(?NETCNF, []) of
	{ok, _} = R ->
	    R;
	_ ->
	    timer:sleep(5000),
            open_trans(Timeout-5000)
    end.


%% @hidden
close_trans() -> ct_netconfc:close_session(?NETCNF).

%% @hidden
create_ipv4_instance(Address) ->
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'Transport', [],
                                    [{transportId, [], ["1"]},
                                     {'Router', [],
                                      [{routerId, [], ["1"]},
                                       {'InterfaceIPv4', [],
                                        [{interfaceIPv4Id, [], ["1"]},
                                         {'AddressIPv4', [],
                                          [{addressIPv4Id, [], ["1"]},
                                           {address, [], [Address ++ "/32"]}]}]
                                       }]
                                     }]}]
                                 }).

%% @hidden
set_accessPoint(AccessPoint) ->
    ct:pal("Set accesspoint to ~p", [AccessPoint]),
    {ok,_} = open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF, running,
				 {'ManagedElement', [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions', [],
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint',
					[],
					[{oamAccessPointId, [], ["1"]},
					 {accessPoint, [], [AccessPoint]}]
				       }]
				     }]}]
				 }),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

%% @hidden
delete_accessPoint() ->
    ct:pal("Delete accesspoint", []),
    {ok,_} = open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF, running,
				 {'ManagedElement', [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions', [],
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint',
					[],
					[{oamAccessPointId, [], ["1"]},
					 {accessPoint, ?DELETE, []}]
				       }]
				     }]}]
				 }),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

%% @hidden
set_alt_accessPoint(AccessPoint) ->
    ct:pal("Set alt accesspoint to ~p", [AccessPoint]),
    {ok,_} = open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF, running,
				 {'ManagedElement', [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions', [],
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint',
					[],
					[{oamAccessPointId, [], ["Alternative"]},
					 {accessPoint, [], [AccessPoint]}]
				       }]
				     }]}]
				 }),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

%% @hidden
delete_alt_accessPoint() ->
    ct:pal("Delete alt accesspoint", []),
    {ok,_} = open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF, running,
				 {'ManagedElement', [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions', [],
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint',
					[],
					[{oamAccessPointId, [], ["Alternative"]},
					 {accessPoint, ?DELETE, []}]
				       }]
				     }]}]
				 }),
    ok = close_trans(),
    ok = timer:sleep(?SLEEP_AFTER_EDIT).

%% @hidden
wait_for_eriChangeIPAddressEvent(Timeout) ->
    Traps = [[{type, eriChangeIPAddressEvent}]],
    Opts = [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
			    [{type,eriAlarmAlarmListRebuilt}],
			    [{type,eriAlarmCleared}],
			    [{type,eriAlarmCritical}],
			    [{type,eriAlarmMinor}],
			    [{type,eriAlarmMajor}],
			    [{type,eriAlarmWarning}],
			    [{type,nsNotifyShutdown}]]},
	    return_received_traps],
    ct:pal("Setup waiting for SNMP traps Timeout:~p", [Timeout]),
    ok = rct_snmpmgr:wait_for_traps(Traps, Opts, Timeout).

%% @hidden
check_eriChangeIPAddressEvent() ->
    ct:pal("Waiting for SNMP traps", []),
    case rct_snmpmgr:check_traps() of
	{ok, TrapList} ->
	    ct:pal("Received SNMP traps: ~p", [TrapList]),
	    search_for_eriChangeIPAddressEvent(TrapList);
	_ ->
	    ct:pal("Received no wanted SNMP traps", []),
	    error
    end.

%% @hidden
search_for_eriChangeIPAddressEvent([]) ->
    error;
search_for_eriChangeIPAddressEvent([H|T]) ->
    case lists:keyfind(type, 1, H) of
	{type, eriChangeIPAddressEvent} ->
	    EriChangeIPAddressAckFdn =
		search_for_eriChangeIPAddressEvent(eriChangeIPAddressAckFdn, H),
	    EriChangeIPAddressAckAttributeName =
		search_for_eriChangeIPAddressEvent(eriChangeIPAddressAckAttributeName, H),
	    EriChangeIPAddressAckAttributeValue =
		search_for_eriChangeIPAddressEvent(eriChangeIPAddressAckAttributeValue, H),
	    {ok, EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName,
	     EriChangeIPAddressAckAttributeValue};
	_ ->
	    search_for_eriChangeIPAddressEvent(T)
    end.

%% @hidden
search_for_eriChangeIPAddressEvent(Key, TupleList) ->
    {Key, Result} = lists:keyfind(Key, 1, TupleList),
    Result.

%% @hidden
ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn,
			    EriChangeIPAddressAckAttributeName,
			    EriChangeIPAddressAckAttributeValue) ->
    ct:pal("Trying to acknowledge eriChangeIPAddressEvent\n"
	   "EriChangeIPAddressAckFdn: ~s\n"
	   "EriChangeIPAddressAckAttributeName: ~s\n"
	   "EriChangeIPAddressAckAttributeValue: ~p",
	   [EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName,
	    EriChangeIPAddressAckAttributeValue]),
    {ok, _} = rct_cli:send(?CLI, "configure"),
    {ok, _} = rct_cli:send(?CLI, EriChangeIPAddressAckFdn),
    {ok, _} = 
	rct_cli:send(?CLI, EriChangeIPAddressAckAttributeName++"="++
			 integer_to_list(EriChangeIPAddressAckAttributeValue)),
    {ok, _} = rct_cli:send(?CLI, "commit"),
    ok.

%%% ===========================================================================
%%% @doc
%%% Description: Wait for expected node state.<br/>
%%% Node = ErlNode <br/>
%%% State = atom(), down | up
%%% @spec wait_for_node_state(Node, ExpState) -> ok
%%% @end
%%% ===========================================================================
wait_for_node_state(Node, ExpState) ->
    wait_for_node_state(Node, ExpState, 600000). %% 10 min

wait_for_node_state(Node, ExpState, Timeout) when Timeout < 0 ->
    ct:pal("ErlNode state not expected : ~p, ~p", [Node, ExpState]),
    ct:fail("Node state not expected within max timeout.");

wait_for_node_state(Node, down, Timeout) ->
    case net_adm:ping(Node) of
        pang ->
	    net_kernel:disconnect(Node),
	    ct:pal("Node is down"),
	    timer:sleep(5000);
	pong ->
	    net_kernel:disconnect(Node),
            timer:sleep(5000),
            wait_for_node_state(Node, down, Timeout-5000)
    end;
wait_for_node_state(Node, up, Timeout) ->
    case net_adm:ping(Node) of
        pang ->
            net_kernel:disconnect(Node),
            timer:sleep(5000),
            wait_for_node_state(Node, up, Timeout-5000);
        pong ->
            net_kernel:disconnect(Node),
            ct:pal("Node is up"),
            timer:sleep(5000)
    end.


receive_ip_changed(Expected) ->
    receive_ip_changed(Expected, 10000).


receive_ip_changed(Expected, Timeout) ->
    receive
	{ip_changed, Expected} -> 
	    ct:log("Received expected ip_changed: ~p~n", [Expected]),
	    ok
    after
	Timeout -> 
	    ct:pal("Failed to receive expected ip_changed: ~p~n", [Expected]),
	    {error, timeout}
    end.


receive_config_update(Expected) ->
    receive_config_update(Expected, ?OOT_NOTIFICATION_TIMEOUT).


receive_config_update(Expected, Timeout) ->
    receive
	{oot_config_update, Expected} -> 
	    ct:log("Received expected oot_config_update: ~p~n", [Expected]),
	    ok 
    after
	Timeout -> 
	    ct:log("Failed to receive expected oot_config_update: ~p~n", 
		   [Expected]),
	    {error, timeout}
    end.


%% receive_config_update_item(Item) ->
%%     receive_config_update_item(Item, ?CSTN_NOTIFICATION_TIMEOUT).


%% receive_config_update_item({_Attr, _Val} = Item, Timeout) ->
%%     receive
%% 	{oot_config_update, Config} -> 
%% 	    ct:log("Received oot_config_update: ~p~n", [Config]),
%% 	    check_config_item(Item, Config)
%%     after
%% 	Timeout -> 
%% 	    {error, timeout}
%%     end.


%% check_config_item(Item, Config) ->
%%     case lists:member(Item, Config) of
%% 	true ->
%% 	    ct:log("Matched item in oot_config_update: ~p~n", [Item]),
%% 	    ok;
%% 	_False ->
%% 	    {error, item_not_found}
%%     end.
    

send_proxy(Msg, Data) ->
    ct:log("send_proxy: Msg = ~p, Data = ~p~n", [Msg, Data]),
    Res = rct_proxy:send_proxy(?IFT_NODE, ?IFT_NAME, Msg, Data, 5000, false),
    ct:log("send_proxy Result: ~p~n", [Res]),
    Res.


receive_proxy() ->
    receive_proxy(?IFT_NOTIFICATION_TIMEOUT).


receive_proxy(Timeout) ->
    Res = rct_proxy:receive_proxy(Timeout),
    ct:log("receive_proxy: ~p~n", [Res]),
    Res.    


%% @hidden
cli_connect() ->
    %% 10 min
    cli_connect(600000).

%% @hidden
cli_connect(Timeout) when Timeout < 0 ->
    ct:fail("Could not setup a COMCLI connection within max timeout.");
cli_connect(Timeout) ->
    case rct_cli:connect(?CLI) of
	ok ->
	    ok;
	{error, _} ->
	    timer:sleep(5000),
            cli_connect(Timeout-5000)
    end.


rpc_call(M, F, A) ->
    rpc_call(?TNODE, M, F, A).


rpc_call(TNode, M, F, A) ->
    rpc_call(TNode, M, F, A, ?RPC_CALL_TIMEOUT).


rpc_call(TNode, M, F, A, Timeout) ->
    rct_rpc:call(TNode, M, F, A, Timeout).


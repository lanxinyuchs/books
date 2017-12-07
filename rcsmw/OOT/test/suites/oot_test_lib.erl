%%% ----------------------------------------------------------
%%% %CCaseFile:	oot_test_lib.erl %
%%% Author:	eolaand
%%% 
%%% Description: Common support functions for OOT tests
%%%
%%% ----------------------------------------------------------
-module(oot_test_lib).
-id('Updated by CCase').
-vsn('/main/R11A/9').
-date('2017-10-04').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R11/1    2017-08-29 eolaand     Created
%%% ----------------------------------------------------------

%% API
-export([configure_snmp/0,
	 deconfigure_snmp/0]).

-export([warm_restart/0,
	 warm_notify/0,
	 warm_done_notify/0,
	 cold_restart/0]).

-export([wait_for_node_state/2,
	 wait_for_node_state/3]).

-export([stop_ns_simulator/0,
	 start_ns_simulator/0]).

-export([cli_connect/0]).

-export([wait_netconf_up/0]).

-export([get_new_ipv4_addr/0,
	 create_ipv4_instances/0,
	 create_ipv4_instance/1,
	 create_ipv4_instance/2,
	 get_new_ipv6_addr/0,
	 create_ipv6_instances/0,
	 create_ipv6_instance/1,
	 create_ipv6_instance/2]).

-export([set_access_point/2,
	 get_access_point/1,
	 delete_access_point/1,
	 set_accessPoint/1,
	 get_accessPoint/0,
	 delete_accessPoint/0,
	 set_alt_accessPoint/1,
	 get_alt_accessPoint/0,
	 delete_alt_accessPoint/0,
	 restore_access_point/2]).

-export([wait_for_eriChangeIPAddressEvent/1,
	 check_eriChangeIPAddressEvent/0,
	 ack_eriChangeIPAddressEvent/1]).

-export([get_oap_namespace/0, 
	 get_oap_alt_namespace/0]).

-export([get_ns_and_ip/1,
	 register_ip_change_cb/1,
	 unregister_ip_change_cb/1,
	 receive_ip_changed/1,
	 receive_ip_changed/2]).

-export([test_register_cfg_upd_cb/0,
	 register_cfg_upd_cb/0,
	 receive_config_update/1,
	 receive_config_update/2,
	 receive_config_update_item/1,
	 receive_config_update_item/2]).

-export([start_proxy_slave/0,
	 stop_proxy_slave/0,
	 send_proxy/2,
	 receive_proxy/0,
	 receive_proxy/1,
	 flush_proxy/0]).

-export([is_target/0]).

-export([rpc_call/3]).

-export([log_tc_start/1]).

-export([ltb/1,
	 btl/1]).

%%%===================================================================
%%% Include files
%%%===================================================================
-include("test_cstn.hrl").
-include("oot_test.hrl").

%%%===================================================================
%%% Macros and records
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Configure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
configure_snmp() ->
    {{mgr_ip, MgrIp},
     {mgr_port, MgrPort},
     {agent_ip, AgentIp},
     {agent_port, AgentPort}} = rct_snmpmgr:get_mgr_data(?SNMP),

    ct:pal("Configure SNMP\n mgr_ip: ~p\n mgr_port: ~p\n agent_ip: ~p\n"
	   " agent_port: ~p", [MgrIp, MgrPort, AgentIp, AgentPort]),

    SimOrTarget = os:getenv("SIM_OR_TARGET"),

    try
	{ok,_} = open_trans(),
	Request = configure_snmp_req(SimOrTarget,
				     {MgrIp, MgrPort, AgentIp, AgentPort}),
	ct_netconfc:edit_config(?NETCNF, running, Request),
	ok = close_trans()
    catch
        _:Reason ->
            close_trans(),
            ct:fail(Reason)
    end,

    timer:sleep(10000), % Seems necessary before start sending traps

    ok.


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


stop_ns_simulator() ->
    ct:pal("Stopping namespace_simulator", []),
    rpc_call(appmServer, stop_lm, ["namespace_simulator"]),
    ok.


start_ns_simulator() ->
    ct:pal("Starting namespace_simulator", []),
    rpc_call(appmServer, start_lm, ["namespace_simulator"]),
    ok.


warm_restart() ->
    ct:pal("Warm restart", []),
    rpc_call(appmI, restart_node, [warm, "ManualRestart"]),
    ok = timer:sleep(20000),
    ok.


warm_notify() ->
    rpc_call(ootServer, warm, []).


warm_done_notify() ->
    rpc_call(ootServer, warm_done, []).


cold_restart() ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ct:pal("Cold restart", []),
    rpc_call(appmI, restart_node, [cold, "ManualRestart"]),
    wait_for_node_state(ErlNode, down),
    wait_for_node_state(ErlNode, up),
    ok.


create_ipv4_instances() ->
    IPv4 = get_new_ipv4_addr(),
    {ok,_} = open_trans(),
    create_ipv4_instance("1", IPv4 ++ ?TN_MASK),
    ok = close_trans().


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


create_ipv6_instances() ->
    IPv6 = get_new_ipv6_addr(),
    {ok,_} = open_trans(),
    create_ipv6_instance("1", IPv6 ++ ?TN_IPV6_MASK),
    ok = close_trans().


get_new_ipv6_addr() ->
    {ok, {IPv6Addr, _IPv4Addr}} = rpc_call(ootLib, get_ip_addresses, []),
    IPv6Addr.


is_target() ->
    "target" == os:getenv("SIM_OR_TARGET").


open_trans() ->
    %% 10 min
    open_trans(600000).


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


close_trans() -> 
    ct_netconfc:close_session(?NETCNF).


create_ipv4_instance(Address) ->
    create_ipv4_instance("1", Address).


create_ipv4_instance(InterfaceIPv4Id, Address) ->
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'Transport', [],
                                    [{transportId, [], ["1"]},
                                     {'Router', [],
                                      [{routerId, [], ["1"]},
                                       {'InterfaceIPv4', [],
                                        [{interfaceIPv4Id, [], 
					  [InterfaceIPv4Id]},
                                         {'AddressIPv4', [],
                                          [{addressIPv4Id, [], ["1"]},
                                           {address, [], [Address]}]}]
                                       }]
                                     }]}]
                                 }).


create_ipv6_instance(Address) ->
    create_ipv6_instance("1", Address).


create_ipv6_instance(InterfaceIPv6Id, Address) ->
    ok = ct_netconfc:edit_config(?NETCNF, running,
                                 {'ManagedElement', [?COMTOP],
                                  [{managedElementId, [], ["1"]},
                                   {'Transport', [],
                                    [{transportId, [], ["1"]},
                                     {'Router', [],
                                      [{routerId, [], ["2"]},
                                       {'InterfaceIPv6', [],
                                        [{interfaceIPv6Id, [], 
					  [InterfaceIPv6Id]},
                                         {'AddressIPv6', [],
                                          [{addressIPv6Id, [], ["1"]},
                                           {address, [], [Address]}]}]
                                       }]
                                     }]}]
                                 }).


set_access_point(?OAP_ID, AccessPoint) ->
    set_accessPoint(AccessPoint);

set_access_point(?ALT_OAP_ID, AccessPoint) ->
    set_alt_accessPoint(AccessPoint).


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
    ok = close_trans().


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
					[{oamAccessPointId, [], 
					  ["Alternative"]},
					 {accessPoint, [], [AccessPoint]}]
				       }]
				     }]}]
				 }),
    ok = close_trans().


get_access_point(?OAP_ID) ->
    get_accessPoint();

get_access_point(?ALT_OAP_ID) ->
    get_alt_accessPoint().


get_accessPoint() ->
    {ok,_} = open_trans(),
    Val = get_oap_attr_val(accessPoint, ?OAP_ID),
    ok = close_trans(),
    Val.


get_alt_accessPoint() ->
    {ok,_} = open_trans(),
    Val = get_oap_attr_val(accessPoint, ?ALT_OAP_ID),
    ok = close_trans(),
    Val.
	

delete_access_point(?OAP_ID) ->
    delete_accessPoint();

delete_access_point(?ALT_OAP_ID) ->
    delete_alt_accessPoint().


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
    ok = close_trans().


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
					[{oamAccessPointId, [], 
					  ["Alternative"]},
					 {accessPoint, ?DELETE, []}]
				       }]
				     }]}]
				 }),
    ok = close_trans().


restore_access_point(OrigMoRef, OrigMoRefAlt) ->
    F = fun({MoRef, Id}) ->
		restore_oap(MoRef, Id)
	end,
    lists:foreach(F, [{OrigMoRef, ?OAP_ID}, {OrigMoRefAlt, ?ALT_OAP_ID}]). 


restore_oap(undefined, Id) ->
    ct:pal("Restore OamAccessPoint=~p to original value: ~p~n", 
	   [Id, undefined]),
    delete_access_point(Id);

restore_oap(OrigMoRef, Id) ->
    case get_access_point(Id) of
	OrigMoRef ->
	    ok;
	_MoRef ->
	    ct:pal("Restore OamAccessPoint=~p to original value: ~p~n", 
		   [Id, OrigMoRef]),
	    set_access_point(Id, OrigMoRef)
    end.


wait_netconf_up() ->
    wait_netconf_up(60).


wait_netconf_up(N) when N > 0 ->
    case open_trans() of
	{ok, _} ->
	    ct:pal("Verified that netconf is up again after the tests", []), 
	    timer:sleep(3000),
	    close_trans();    
	_Error ->
	    ct:log("Failed to open netconf session: ~p", [_Error]),
	    wait_netconf_up(N - 1)
    end;

wait_netconf_up(_N) ->
    {error, netconf_not_up}.


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


check_eriChangeIPAddressEvent() ->
    ct:pal("Waiting for SNMP traps", []),
    case rct_snmpmgr:check_traps() of
	{ok, TrapList} ->
	    ct:pal("Received SNMP traps: ~p", [TrapList]),
	    get_trap_ack_data(TrapList);
	_ ->
	    ct:pal("Received no wanted SNMP traps", []),
	    {error, no_trap_received}
    end.


ack_eriChangeIPAddressEvent([EriChangeIPAddressAckFdn,
			     EriChangeIPAddressAckAttributeName,
			     EriChangeIPAddressAckAttributeValue]) ->
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
    {error, "Node state not expected within max timeout"};

wait_for_node_state(Node, down, Timeout) ->
    case net_adm:ping(Node) of
        pang ->
	    net_kernel:disconnect(Node),
	    ct:pal("Node is down"),
	    ok;
	pong ->
            timer:sleep(3000),
            wait_for_node_state(Node, down, Timeout-3000)
    end;

wait_for_node_state(Node, up, Timeout) ->
    case net_adm:ping(Node) of
        pang ->
            timer:sleep(3000),
            wait_for_node_state(Node, up, Timeout-3000);
        pong ->
            ct:pal("Node is up"),
	    ok
    end.


get_oap_namespace() ->
    rpc_call(ootI, get_oap_namespace, []).


get_oap_alt_namespace() ->
    rpc_call(ootI, get_oap_alt_namespace, []).


get_ns_and_ip(MoRef) ->
    case rpc_call(ootI, get_ns_and_ip, [MoRef]) of
	{ok, NS, IP} -> 
	    {ok, btl(NS), btl(IP)};
	Error ->
	    Error
    end.


register_ip_change_cb(MoRef) ->
    rpc_call(ootI, register_ip_change_cb, [MoRef, ootI, [self()]]).
 

unregister_ip_change_cb(MoRef) ->
    rpc_call(ootI, unregister_ip_change_cb, [MoRef, ootI]).
   

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


register_cfg_upd_cb() ->
    Fun = rpc_call(ootI, get_cfg_upd_cb_fun, [self()]),
    rpc_call(ootI, register_cfg_upd_cb, [Fun, self()]).


test_register_cfg_upd_cb() ->
    Fun = rpc_call(ootI, get_cfg_upd_cb_fun, [self()]),
    rpc_call(ootServer, test_register_cfg_upd_cb, [Fun, self()]).


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


receive_config_update_item(Item) ->
    receive_config_update_item(Item, ?CSTN_NOTIFICATION_TIMEOUT).


receive_config_update_item({_Attr, _Val} = Item, Timeout) ->
    receive_config_update_item([Item], Timeout);

receive_config_update_item(Items, Timeout) ->
    receive
	{oot_config_update, Config} -> 
	    ct:log("Received oot_config_update: ~p~n", [Config]),
	    check_config_item(Items, Config)
    after
	Timeout -> 
	    {error, timeout}
    end.


start_proxy_slave() ->
    Pid = self(),
    spawn(fun() ->
		  register(?PROXY_SLAVE, self()),
		  ct:pal("start_proxy slave", []),
		  Res = {ok, client_started} = 
		      rct_proxy:start_proxy(?IFT_NODE, ?IFT_NAME, ?CSTN),
		  ct:pal("start_proxy result: ~p~n", [Res]),
		  {ok, _Handle} = send_proxy(?CsTnInitialize3, {}),
		  ct:pal("CsTnInitialize3 result: {ok, ~p}~n", [_Handle]),
		  %% %% Wait a second to make sure CSTN is initialized
		  timer:sleep(1000),
		  Pid ! proxy_started,
		  proxy_slave_loop()
	  end),
    receive
	proxy_started ->
	    ok
    end.


proxy_slave_loop() ->
    receive
	{receive_proxy, Pid, Timeout} ->
	    Pid ! {receive_proxy, Pid, receive_rct_proxy(Timeout)},
	    proxy_slave_loop();
	{flush_proxy, Pid} ->
	    do_flush_proxy(),
	    Pid ! {flush_proxy, Pid, ok},
	    proxy_slave_loop();
	{stop_proxy, Pid} ->
	    Pid ! {stop_proxy, Pid, 
		   rct_proxy:stop_proxy(?IFT_NODE, ?IFT_NAME, ?CSTN)}
    end.


send_proxy(Msg, Data) ->
    ct:log("send_proxy: Msg = ~p, Data = ~p~n", [Msg, Data]),
    Res = rct_proxy:send_proxy(?IFT_NODE, ?IFT_NAME, Msg, Data, 5000, false),
    ct:log("send_proxy Result: ~p~n", [Res]),
    Res.


receive_proxy() ->
    receive_proxy(?IFT_NOTIFICATION_TIMEOUT).


receive_proxy(Timeout) ->
    Self = self(),
    ?PROXY_SLAVE ! {receive_proxy, Self, Timeout},
    receive
	{receive_proxy, Self, Result} ->
	    Result
    after Timeout + 1000 ->
	    {error, timeout}
    end.
	

flush_proxy() ->
    ct:log("Flush proxy~n", []),
    Self = self(),
    ?PROXY_SLAVE ! {flush_proxy, Self},
    receive
	{flush_proxy, Self, Result} ->
	    Result
    after 5000 ->
	    {error, timeout}
    end.
    

do_flush_proxy() ->
    case rct_proxy:receive_proxy(1000) of
	{error, timeout} ->
	    ok;
	Msg ->
	    ct:log("Flush proxy notification: ~p~n", [Msg]),
	    do_flush_proxy()
    end.


receive_rct_proxy(Timeout) ->
    Res = rct_proxy:selective_receive_proxy(Timeout),
    ct:log("receive_proxy: ~p~n", [Res]),
    Res.    


stop_proxy_slave() ->
    Self = self(),
    ?PROXY_SLAVE ! {stop_proxy, Self},
    receive
	{stop_proxy, Self, Result} ->
	    ct:log("Stop proxy result: ~p~n", [Result]),
	    ok
    after 10000 ->
	    ct:log("Failed to stop proxy: ~p~nKill slave process", [timeout]),
	    exit(whereis(?PROXY_SLAVE), kill),
	    {ok, slave_killed}
    end.
	

cli_connect() ->
    %% 10 min
    cli_connect(600000).


rpc_call(M, F, A) ->
    rpc_call(?TNODE, M, F, A).


rpc_call(TNode, M, F, A) ->
    rpc_call(TNode, M, F, A, ?RPC_CALL_TIMEOUT).


rpc_call(TNode, M, F, A, Timeout) ->
    rct_rpc:call(TNode, M, F, A, Timeout, noprint).


%%%===================================================================
%%% Internal functions
%%%===================================================================
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


check_config_item(Items, Config) ->
    F = fun(Item) ->
		case lists:member(Item, Config) of
		    true ->
			ct:log("Matched item in oot_config_update: ~p~n", 
			       [Item]),
			true;
		    false ->
			ct:log("Failed to match item in oot_config_update: "
			       "~p~n", [Item]),
			false
		end
	end,
    case lists:all(F, Items) of
	true ->
	    ok;
	false ->
	    {error, item_not_found} 
    end.

    

get_trap_ack_data([]) ->
    {error, not_found};

get_trap_ack_data([H|T]) ->
    case lists:keyfind(type, 1, H) of
	{type, eriChangeIPAddressEvent} ->
	    AckAttrs = [eriChangeIPAddressAckFdn, 
			eriChangeIPAddressAckAttributeName,
			eriChangeIPAddressAckAttributeValue],
	    {ok, [Val || {Key, Val} <- H, lists:member(Key, AckAttrs)]};
	_ ->
	    get_trap_ack_data(T)
    end.


get_oap_attr_val(Attr, OapId) ->
    {'OamAccessPoint', _, AttrVals} = get_oap_attr_vals(OapId),
    case lists:keyfind(Attr, 1, AttrVals) of
	{_Attr, _, [Val]} ->    
	    Val;
	_False ->
	    undefined
    end.


get_oap_attr_vals(OapId) ->
    {ok,[{'ManagedElement', _, [{managedElementId,[],["1"]},
				{'SystemFunctions',[],
				 [{systemFunctionsId,[],["1"]},
				  {'SysM', _, [{sysMId,[],["1"]},
					       {'OamAccessPoint',
						_,
						_OamAccessPointAttr} 
					       = Oap]}]}]}]} =
        ct_netconfc:get_config(?NETCNF, 
			       running,
			       {'ManagedElement',
				[?COMTOP],
				[{managedElementId, [], ["1"]},
				 {'SystemFunctions',
				  [], 
				  [{systemFunctionsId, [], ["1"]},
				   {'SysM',
				    [],
				    [{sysMId, [], ["1"]},
				     {'OamAccessPoint', [], 
				      [{oamAccessPointId, [],[OapId]}]}]
				   }]
				 }]
			       }),
    Oap.


log_tc_start(TC) ->
    ct:print("~n=================== TestCase =======================~n~p~n"
	     "====================================================", [TC]).


ltb(L) ->
    list_to_binary(L).


btl(B) ->
    binary_to_list(B).

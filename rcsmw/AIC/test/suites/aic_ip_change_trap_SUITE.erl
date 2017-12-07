%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aic_ip_change_trap_SUITE.erl %
%%% @author eolaand
%%% @version /main/R11A/2
%%%
%%% @doc ==Test Suite for test of sending the IP change trap==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%% @end

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
%%% R11A/1     2017-10-02 eolaand     Copied from OOT to AIC
%%% ----------------------------------------------------------
-module(aic_ip_change_trap_SUITE).

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
	changed_ipAddr_trap_restart/1]).

-include_lib("common_test/include/ct.hrl").

-define(TNODE, rpc).
-define(NETCNF, nc1).
-define(CLI, c1).
-define(COMTOP, {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").
-define(DELETE, [{'xmlns:nc', ?NC_NAMESPACE}, {'nc:operation', "delete"}]).
-define(SLEEP_AFTER_EDIT, timer:seconds(10)).
-define(RPC_CALL_TIMEOUT, 10000).

-define(LDN_IPV4_1,
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1").

%% must match definition in master.h of ift_app
-define(CSTN, 25).
-define(CsTnInitialize2, 2).
-define(SetOamDNtoNsNameRsp, 5).

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
		 {rct_cli, {?CLI,[manual_connect]}}
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
    {ok, client_started} = rct_proxy:start_proxy(node1, cstn1, ?CSTN),
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
     changed_ipAddr_trap_no_ack
    ].


%%--------------------------------------------------------------------
%% @doc
%% Call initialize2. <br/>
%% When expected trap is received, no acknowledgment sent.<br/>
%% Restart node. Wait for resending of  trap. <br/>
%% @end
%%--------------------------------------------------------------------
changed_ipAddr_trap_restart(_Config) ->

    LdnV4 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4),

    %% test init2 and trap
    IPAdd = get_new_ipv4_addr(),
    {ok,_Number} = rct_proxy:send_proxy(node1, cstn1, ?SetOamDNtoNsNameRsp,
					{LdnV4, "", IPAdd}),

    {ok,_Handle} = rct_proxy:send_proxy(node1, cstn1, ?CsTnInitialize2,{}),

    Timeout = 180,
    ok = wait_for_eriChangeIPAddressEvent(Timeout, IPAdd),
    {ok, _EriChangeIPAddressAckFdn, _EriChangeIPAddressAckAttributeName,
     _EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),

    %% not acknowledging the trap
    ct:pal("Not acknowledging trap", []),

    %% restart (note that the namespace simulator replaces IFT after restart)
    cold_restart(),

    %% after restart, must connect
    ok = cli_connect(),

    %%check that  trap is being resent after restart
    Timeout2 = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout2, IPAdd),
    {ok, EriChangeIPAddressAckFdn2, EriChangeIPAddressAckAttributeName2,
     EriChangeIPAddressAckAttributeValue2} = check_eriChangeIPAddressEvent(),

    ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn2,
				     EriChangeIPAddressAckAttributeName2,
				     EriChangeIPAddressAckAttributeValue2),

    ok = delete_accessPoint(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Call initialize2. Check expected trap is received. <br/>
%% @end
%%--------------------------------------------------------------------
changed_ipAddr_trap_ack(_Config) ->

    LdnV4 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4),

    %% test init2 and trap
    IPAdd = get_new_ipv4_addr(),
    {ok,_Number} = rct_proxy:send_proxy(node1, cstn1, ?SetOamDNtoNsNameRsp,
					{LdnV4, "", IPAdd}),

    {ok,_Handle} = rct_proxy:send_proxy(node1, cstn1, ?CsTnInitialize2,{}),

    Timeout = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout, IPAdd),
    {ok, EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName,
     EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),
    ok = cli_connect(),

    ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn,
				     EriChangeIPAddressAckAttributeName,
				     EriChangeIPAddressAckAttributeValue),


    %%check that no more traps are sent after acknowledgment
    Timeout2 = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout2, IPAdd),
    error = check_eriChangeIPAddressEvent(),


    %%test update
    %%{ok,_Result}   = rct_proxy:send_proxy(node1, cstn1, ?CsTnUpdate,{LdnV4, "", IPAdd}),

    ok = delete_accessPoint(),
    ok = rct_cli:disconnect(?CLI),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Call initialize2. <br/>
%% When expected trap is received, no acknowledgment sent.<br/>
%% Wait for resending of  trap. <br/>
%% @end
%%--------------------------------------------------------------------
changed_ipAddr_trap_no_ack(_Config) ->

    LdnV4 = ?LDN_IPV4_1,
    ok = set_accessPoint(LdnV4),

    %% test init2 and trap
    IPAdd = get_new_ipv4_addr(),
    {ok,_Number} = rct_proxy:send_proxy(node1, cstn1, ?SetOamDNtoNsNameRsp,
					{LdnV4, "", IPAdd}),

    {ok,_Handle} = rct_proxy:send_proxy(node1, cstn1, ?CsTnInitialize2,{}),

    Timeout = 120,
    ok = wait_for_eriChangeIPAddressEvent(Timeout, IPAdd),
    {ok, EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName,
     EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),

    %% not acknowledging the trap
    ct:pal("Not acknowledging trap", []),

    %%check that  trap is being resent
    Timeout2 = 300,
    ok = wait_for_eriChangeIPAddressEvent(Timeout2, IPAdd),
    {ok, EriChangeIPAddressAckFdn, EriChangeIPAddressAckAttributeName,
     EriChangeIPAddressAckAttributeValue} = check_eriChangeIPAddressEvent(),


    ok = cli_connect(),


    ok = ack_eriChangeIPAddressEvent(EriChangeIPAddressAckFdn,
				     EriChangeIPAddressAckAttributeName,
				     EriChangeIPAddressAckAttributeValue),

    ok = delete_accessPoint(),
    ok = rct_cli:disconnect(?CLI),

    ok.

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

    ct:pal("Configure SNMP\n mgr_ip: ~p,\n mgr_port: ~p,\n agent_ip: ~p,"
	   "\n agent_port: ~p", [MgrIp, MgrPort, AgentIp, AgentPort]),

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
    rpc_call(swmI, force_auto_backup, []),
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

%% @hidden
check_eriChangeIPAddressEvent() ->
    ct:pal("Waiting for SNMP traps", []),
    case rct_snmpmgr:check_traps() of
	{ok, TrapList} ->
	    ct:print("Received SNMP traps", []),
	    ct:log("Received SNMP traps: ~p", [TrapList]),
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
    {ok, _} = rct_cli:send(?CLI, EriChangeIPAddressAckAttributeName++"="++integer_to_list(EriChangeIPAddressAckAttributeValue)),
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

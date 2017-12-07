%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsa_alarm_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/2
%%%
%%% @doc == Test Suite for test if COMSA alarm handling ==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% @end

-module(comsa_alarm_SUITE).
-vsn('/main/R11A/R12A/2').

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
%%% R11A/1     2017-09-26 elarrun     Created
%%% R12A/2     2017-11-27 etxpeno     delete attributes nodeCredential
%%%                                   and trustCategory in conf_snmpmgr
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0,
	 conf_snmpmgr/1,
	 send_def_CI_trap_1/1,
         send_def_CI_trap_2/1,
         send_pri_CI_trap/1]
).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_rpc, rct_netconf, cth_conn_log, rct_logging, rct_tlib
%% @end
%%--------------------------------------------------------------------
suite() ->
    CpuMemory = case ct:get_config({jenkins_config,installed_type}) of
                    no   -> 300;
                    lrat -> 1050;
                    wrat -> 1050;
                    grat -> 1050;
                    rat  -> 1050;
                    tcu  -> 1000;
                    _ ->
                        ct:pal("NO {jenkins_config,installed_type} using node_type"),
                        case ct:get_config(node_type) of
			    undefined ->
				case os:getenv("SIM_OR_TARGET") of
				    "sim" -> 300;
				    _ ->
					400
				end;
			    node_ci_dc_dus -> 1150; % Remove when node CI changes curl call
			    node_ci_dc_rbs -> 1150;
			    node_ci_hc_rbs -> 1150;
			    node_ci_dc_tcu03 -> 400;
			    node_ci_hc_tcu03 -> 400
			end
		end,
    case check_if_vc_board()  of
	"yes" -> [{ct_hooks, [{rct_htmllink,[]},
			      {rct_consserv,cs1},
			      {cth_conn_log,[]},
			      {rct_ssh,{ssh,[manual_connect]}},
			      {rct_coli, {coli, [manual_connect]}},
			      {rct_netconf, {nc1,man_auth}},
			      {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}}
			     ]}];
	_->
	    Serial = case os:getenv("SIM_OR_TARGET") of
			 "cloudish" -> [{cth_conn_log,[]}];
			 _ -> [{rct_rs232,console}, {cth_conn_log,[]}]
		     end,
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_power,node},
			 {rct_snmpmgr,snmp1},
			 {rct_rpc, rpc},
			 {rct_netconf, nc1},
			 {rct_cli, {cli, [manual_connect]}},
			 {rct_ssh,{ssh,[manual_connect]}},
			 {rct_coli, {coli, [manual_connect]}},
			 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
			 {rct_tlib,{kalle,[{cpumemory, CpuMemory},{cpuload,100}]}},
			 {rct_core,[]}] ++ Serial}]
    end.


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
end_per_testcase(_TestCase, _Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------

all() ->
    case check_if_vc_board() of
	"yes" -> [];
	_->    [conf_snmpmgr, send_def_CI_trap_1, send_def_CI_trap_2, send_pri_CI_trap]
    end.

%%--------------------------------------------------------------------
%% @doc
%% Groups.<br/><br/>
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup = all(),
    [{default__group, [], AllGroup}].


%%--------------------------------------------------------------------
%% @doc
%% Configure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
conf_snmpmgr(_Config) ->
    {{mgr_ip,MgrIp},
     {mgr_port,MgrPort},
     {agent_ip,AgentIp},
     {agent_port,AgentPort}} = rct_snmpmgr:get_mgr_data(snmp1),
    Platform = os:getenv("SIM_OR_TARGET"),
    PduType = if Platform =:= "sim" -> "TRAP"; true -> "INFORM" end,
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 ok = case Platform of
			  "sim" ->
			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
								   [{xmlns,"urn:com:ericsson:ecim:ComTop"},
								    {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
								   [{managedElementId,[],["1"]},
								    {'SystemFunctions',
								     [{systemFunctionsId,[],["1"]},
								      {'SysM',
								       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
								       [{sysMId,[],["1"]},
									{'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									 [{snmpId,[],["1"]},
									  {administrativeState,[],["UNLOCKED"]},
									  {nodeCredential, [{'xc:operation', "delete"}], []},
									  {trustCategory, [{'xc:operation', "delete"}], []},
									  {'SnmpTargetV2C',
									   [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									   [{snmpTargetV2CId,[],["1"]},
									    {address,[],[MgrIp]},
									    {community,[],["public"]},
									    {transportMethod,[],[PduType]},
									    {port,[],[MgrPort]}]}]}]}]}]});
			  TARG_OR_CLOUD when TARG_OR_CLOUD == "target";
					     TARG_OR_CLOUD == "cloudish" ->
			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
								   [{xmlns,"urn:com:ericsson:ecim:ComTop"},
								    {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
								   [{managedElementId,[],["1"]},
								    {'SystemFunctions',
								     [{systemFunctionsId,[],["1"]},
								      {'SysM',
								       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
								       [{sysMId,[],["1"]},
									{'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									 [{snmpId,[],["1"]},
									  {administrativeState,[],["UNLOCKED"]},
									  {agentAddress,
									   [{struct,"HostAndPort"}],
									   [{host,[],[AgentIp]},
									    {port,[],[AgentPort]}]},
									  {nodeCredential, [{'xc:operation', "delete"}], []},
									  {trustCategory, [{'xc:operation', "delete"}], []},
									  {'SnmpTargetV2C',
									   [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									   [{snmpTargetV2CId,[],["1"]},
									    {address,[],[MgrIp]},
									    {informRetryCount,[],["6"]},
									    {transportMethod,[],[PduType]},
									    {community,[],["public"]},
									    {port,[],[MgrPort]}]}]}]}]}]})
		      end,
                 ok = ct_netconfc:close_session(nc1)
        end,
    try F()
    catch
        _:Reason ->
            ct_netconfc:close_session(nc1),
           ct:fail(Reason)
    end,
    ct:pal("SNMP ~ss will be sent to: ~s:~s", [PduType, MgrIp, MgrPort]),
    timer:sleep(1000). % Seems necessary before start sending traps


%%--------------------------------------------------------------------
%% @doc
%% Send alarm trap and clear alarm trap.<br/><br/>
%% @end
%%--------------------------------------------------------------------
-define(SpecificProblem, "Resource Monitor Coffee Beans Container Low Level").
-define(AdditionalText, "Coffee beans refill required").
-define(PreferableGevalia, "Preferable=Gevalia").
-define(BinPreferableGevalia, {<<"Preferable">>, <<"Gevalia">>}).

send_def_CI_trap_1(_Config) ->
    LDN = "ManagedElement=1,TestRoot=1,TestClass1=1",
    BinLDN = list_to_binary(LDN),
    SystemUUID = rct_rpc:call(rpc, sysEnv, get_systemUUID,[],20000),
    ComputeName = rct_rpc:call(rpc, sysEnv, get_computeName,[],20000),
    ExpAddInfo = lists:concat(['CI={"C":[{"I":"', SystemUUID,'","n":"', ComputeName,'"}]}']),

    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmMajor},
				      {eriAlarmActiveManagedObject, LDN},
				      {eriAlarmActiveSpecificProblem, ?SpecificProblem},
				      {eriAlarmNObjAdditionalText, ?AdditionalText},
                                      {eriAlarmNObjAdditionalInfo, ExpAddInfo}
                                    ]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),

    ok = rct_rpc:call(rpc, comsaI, send_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel',
                                               major,
                                               [BinLDN],
                                               ?AdditionalText
                                              ],
                                              20000),
    ok = rct_snmpmgr:check_traps(),
    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
				      {eriAlarmActiveManagedObject, LDN},
				      {eriAlarmActiveSpecificProblem, ?SpecificProblem},
				      {eriAlarmNObjAdditionalText, ?AdditionalText}]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),
    ok = rct_rpc:call(rpc, comsaI, clear_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', [BinLDN]], 20000),
    ok = rct_snmpmgr:check_traps().


%%--------------------------------------------------------------------
%% @doc
%% Send and clear correlation info alarm trap.<br/><br/>
%% @end
%%--------------------------------------------------------------------
send_def_CI_trap_2(_Config) ->
    LDN = "ManagedElement=1,TestRoot=1,TestClass1=2",
    BinLDN = list_to_binary(LDN),
    SystemUUID = rct_rpc:call(rpc, sysEnv, get_systemUUID,[],20000),
    ComputeName = rct_rpc:call(rpc, sysEnv, get_computeName,[],20000),
    CI = lists:concat(['CI={"C":[{"I":"', SystemUUID,'","n":"', ComputeName,'"}]}']),
    ExpAddInfo = lists:concat([CI, ";", ?PreferableGevalia, ";"]),

    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmMajor},
				      {eriAlarmActiveManagedObject, LDN},
				      {eriAlarmActiveSpecificProblem, ?SpecificProblem},
				      {eriAlarmNObjAdditionalText, ?AdditionalText},
                                      {eriAlarmNObjAdditionalInfo, ExpAddInfo}
                                    ]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),

    ok = rct_rpc:call(rpc, comsaI, send_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel',
                                               major,
                                               [BinLDN],
                                               ?AdditionalText,
                                               [?BinPreferableGevalia],
                                               {}
                                              ],
                                              20000),
    ok = rct_snmpmgr:check_traps(),
    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
				      {eriAlarmActiveManagedObject, LDN},
				      {eriAlarmActiveSpecificProblem, ?SpecificProblem},
				      {eriAlarmNObjAdditionalText, ?AdditionalText}]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),
    ok = rct_rpc:call(rpc, comsaI, clear_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', [BinLDN]], 20000),
    ok = rct_snmpmgr:check_traps().


%%--------------------------------------------------------------------
%% @doc
%% Send and clear correlation info alarm trap.<br/><br/>
%% @end
%%--------------------------------------------------------------------
send_pri_CI_trap(_Config) ->
    LDN = "ManagedElement=1,TestRoot=1,TestClass1=3",
    BinLDN = list_to_binary(LDN),
    SystemUUID = rct_rpc:call(rpc, sysEnv, get_systemUUID,[],20000),
    ComputeName = rct_rpc:call(rpc, sysEnv, get_computeName,[],20000),
    CorrelationUUID = binary_to_list(rct_rpc:call(rpc, sysOei, get_correlation_uuid,[],20000)),
    CI = lists:concat(['CI={"C":[{"I":"', SystemUUID,'","n":"', ComputeName,'"}],"P":"', CorrelationUUID, '"}']),
    ExpAddInfo = lists:concat([CI, ";", ?PreferableGevalia, ";"]),

    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmMajor},
				      {eriAlarmActiveManagedObject, LDN},
				      {eriAlarmActiveSpecificProblem, ?SpecificProblem},
				      {eriAlarmNObjAdditionalText, ?AdditionalText},
                                      {eriAlarmNObjAdditionalInfo, ExpAddInfo}
                                    ]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),

    ok = rct_rpc:call(rpc, comsaI, send_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel',
                                               major,
                                               [BinLDN],
                                               ?AdditionalText,
                                               [?BinPreferableGevalia],
                                               {primary, CorrelationUUID}
                                              ],
                                              20000),
    ok = rct_snmpmgr:check_traps(),
    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
				      {eriAlarmActiveManagedObject, LDN},
				      {eriAlarmActiveSpecificProblem, ?SpecificProblem},
				      {eriAlarmNObjAdditionalText, ?AdditionalText}]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),
    ok = rct_rpc:call(rpc, comsaI, clear_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', [BinLDN]],20000),
    ok = rct_snmpmgr:check_traps().

%%%  INTERNAL FUNCTIONS

netconf_open(Session, InPar)->
    Param = [{timeout,10000}|InPar],
    case check_if_vc_board()  of
	"yes" ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param]);
	_ -> ct_netconfc:open(Session,Param)
    end.
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

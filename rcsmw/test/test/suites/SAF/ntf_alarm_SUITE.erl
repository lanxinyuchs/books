%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	ntf_alarm_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/1
%%%
%%% @doc == Generate and cease Alarms using saf ntf, Check ecpected traps is recieved==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%%```
%%% These alarms is used.
%%% 100 = lihLKFFaultAlarm
%%% 101 = lihLKFMissingAlarm
%%% 102 = lihEmergencyAlarm
%%% 103 = lihEmergencyExpiredAlarm
%%% 104 = lihIntegrationAlarm
%%% 105 = lihIntegrationExpiredAlarm
%%% 106 = lihProductionAlarm
%%% 107 = lihProductionExpiredAlarm'''
%%% @end

-module(ntf_alarm_SUITE).
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
%%% R2A/1      2013-07-05 etxivri     Created
%%% R2A/4      2013-07-09 etxivri     Increased time between alarms gnerates and ceased.
%%%                                   Moved TCs to a new suite in /ROB
%%% R2A/5      2013-07-09 etxivri     Changed so we check trap after each specific alarm.
%%%                                   Removed unused code.
%%% R2A/6      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/7      2014-03-18 etxivri     Update allowed traps.
%%% R2A/9      2014-06-10 etxivri     Update allowed traps.
%%% R2A/10     2014-07-08 etxivri     Update ManagedObjec path in traps.
%%% R2A/12     2014-08-25 erarafo     Adjusted due to COMSA changes.
%%% ----------------------------------------------------------
%%% R3A/1      2014-08-26 erarafo     Equal to R2A/12; branching unintentional
%%% R4A/1      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_ntf.hrl").

-include("ntf_alarm_allowed_traps.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 config_snmpmgr/1,
	 deconfig_snmpmgr/1,
	 gen_cease_8_alarms_check/1
	]).

-define(NrOfNetConfUsers, 1).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of CliHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],

    %% ct:pal("CliHooks: ~p",[CliHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_safe_ntf_rpc,[{safe_debug_level, 0}, {finalize_on_fail, true}]},
		 {rct_scp, [{1, node1}]},
		 {rct_core,[]},
		 {rct_snmpmgr,snmp1},
		 %% {rct_safe_ntf_rpc,[{targ_du_no, 1},
		 %% 		   {instance_name, ?INSTANCE_NAME},
		 %% 		    {safe_debug_level, 2},
		 %% 		    {finalize_on_fail, true}]},
		 %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} | CliHooks
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} | NetconfHooks
		]}].

%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added rbs units", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),

	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[B]),
   	    ct:pal("### BeamPid: ~p",[D]),

	    ct_netconfc:open(nc1,[]),
	    FM = ct_netconfc:get(nc1,{'ManagedElement',
				      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				      [{managedElementId,[],["1"]},
				       {'SystemFunctions',[{systemFunctionsId,[],["1"]},
							   {'Fm',
							    [{xmlns,"urn:com:ericsson:ecim:ComFm"}],
							    [{fmId,[],["1"]}]}]}
				      ]}),
	    ct:pal("~p", [FM]),

	    %%%%%%%%%%%%%%
	    %% Clean up!
	    %%%%%%%%%%%%%%
	    deconfig_snmpmgr_no_check(),
	    ct_netconfc:close(nc1),


            %%%%
	    %% Cease alarm.
            %%%%
	    rct_rpc:call(rpc, lih_fm, cancel_LKF_fault_alarm, [], 10000, noprint),
	    rct_rpc:call(rpc, lih_fm, cancel_LKF_missing_alarm, [], 10000, noprint),
	    rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_alarm, [], 10000, noprint),
	    rct_rpc:call(rpc, lih_fm, cancel_emergency_unlock_expired_alarm, [], 10000, noprint),
	    rct_rpc:call(rpc, lih_fm, cancel_integration_unlock_alarm, [], 10000, noprint),
	    rct_rpc:call(rpc, lih_fm, cancel_integration_unlock_expired_alarm, [], 10000, noprint),
	    rct_rpc:call(rpc, lih_fm, cancel_production_unlock_alarm, [], 10000, noprint),
	    rct_rpc:call(rpc, lih_fm, cancel_production_unlock_expired_alarm, [], 10000, noprint)
    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() ->
    [config_snmpmgr,
     gen_cease_8_alarms_check,
     deconfig_snmpmgr].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].


%%--------------------------------------------------------------------
%% @doc
%%  Generate 8 alarms, cease the alarms. <br/>
%%  After each alarm,check expected traps i recieved. <br/>
%% - Generate 8 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% - Cease 8 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% @spec gen_cease_8_alarms_check(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
gen_cease_8_alarms_check(_Config) ->
    ct:pal("¤¤¤ generate 8 alarms, cease the alarms. After each alarm, check expected traps i recieved.", []),
    %% test_server:break("break"),

    %%%%
    %% Get Beam Pid.
    %%%%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% Get testnode Beam Pids.
    %%%%
    %% get_testnode_beam_pid(),
    {ok, TestNode} = rct_safe_ntf_rpc:get_testnode(rct_safe_ntf_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 10000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    %%%%
    %% Initialize NTF Handle
    %%%%
    ct:pal("Initialize NTF Handle", []),
    {ok, Handle, _} = rct_safe_ntf_rpc:initialize(undefined),
    %% ct:pal("### Handle: ~p",[Handle]),

    try
        %%%%
	%% Build a alarm nr list
        %%%%
	AlarmNrList = lists:seq(100,107),
	%% ct:pal("### AlarmNrList: ~p",[AlarmNrList]),

        %%%%
	%% Generate alarm.
        %%%%
	ct:pal("Generate alarms.",[]),
	HeaderList = generate_alarms(Handle, AlarmNrList),

        %%%%
	%% Cease alarm.
        %%%%
	ct:pal("Cease alarms.",[]),
	cease_alarms(Handle, HeaderList, AlarmNrList),
	timer:sleep(1000),

        %%%%
	%% Finalize NTF Handle
        %%%%
	ct:pal("Finalize NTF Handle ~p", [Handle]),
	ok = rct_safe_ntf_rpc:finalize(Handle)

    catch
	_:Reason -> %% Case TC fail, then delete NTF handle
	    ct:pal("Finalize NTF Handle ~p", [Handle]),
	    ok = rct_safe_ntf_rpc:finalize(Handle),

	    ct:fail(Reason)
    end,

    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
generate_alarms(Handle, AlarmNrList) ->
    HeaderList =
	lists:map(
	  fun(AlarmNr) ->
		  %%%%
		  %% Set expected recieved traps when generate alarms
                                  %%%%
				  TrapList = build_exp_trap_list(eriAlarmCritical, [AlarmNr]),
				  ct:pal("### Expected TrapList: ~p",[TrapList]),
		  ok = rct_snmpmgr:wait_for_traps(TrapList, [{allowed_traps, ?ALLOWED_TRAPS}], 20),
				  %%%%
				  %% Generate Alarm
                                  %%%%
    				  Header = #safe_ntf_notification_header{event_type = ?SAFE_NTF_TYPE_ALARM,
    									 notification_object =
							     "testClass2Id=1,TESTMOMtestRootId=1",
    									 notification_class_id =
    									     #safe_ntf_class_id{vendor_id = 193,
    												major_id = 101,
    												minor_id = AlarmNr},
    									 event_time = ?SAFE_TIME_UNKNOWN},

    				  Notification = #safe_ntf_alarm_notification{notification_header = Header,
    									      probable_cause = ?SAFE_NTF_UNSPECIFIED_REASON,
    									      perceived_severity = ?SAFE_NTF_SEVERITY_CRITICAL},

    				  {ok, _Id } = rct_safe_ntf_rpc:notification_send(Handle, Notification),

                                  %%%%
				  %% Check trap is recieved.
                                  %%%%
				  ok = rct_snmpmgr:check_traps(),

    				  Header
    			  end, AlarmNrList),
    HeaderList.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cease_alarms(Handle, HeaderList, AlarmNrList) ->
    Header_AlarmNr_List = lists:zip(HeaderList, AlarmNrList),
    lists:foreach(
      fun({Header, AlarmNr}) ->
	      %%%%
	      %% Set expected recieved traps when cease the alarms
			  %%%%
			  ClearTrapList = build_exp_trap_list(eriAlarmCleared, [AlarmNr]),
			  ct:pal("### Expected ClearTrapList: ~p",[ClearTrapList]),
	      ok = rct_snmpmgr:wait_for_traps(ClearTrapList, [{allowed_traps, ?ALLOWED_TRAPS}], 20),

                          %%%%
			  %% Generate Alarm
                          %%%%
    			  NotificationClear = #safe_ntf_alarm_notification{notification_header = Header,
    									   probable_cause = ?SAFE_NTF_UNSPECIFIED_REASON,
    									   perceived_severity = ?SAFE_NTF_SEVERITY_CLEARED},
    			  {ok, _Id2 } =  rct_safe_ntf_rpc:notification_send(Handle, NotificationClear),
			  %%%%
			  %% Check trap is recieved.
                          %%%%
			  ok = rct_snmpmgr:check_traps()
		  end, Header_AlarmNr_List),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
build_exp_trap_list(Cause, AlarmNrList)->
    %% 100 = lihLKFFaultAlarm
    %% 101 = lihLKFMissingAlarm
    %% 102 = lihEmergencyAlarm
    %% 103 = lihEmergencyExpiredAlarm
    %% 104 = lihIntegrationAlarm
    %% 105 = lihIntegrationExpiredAlarm
    %% 106 = lihProductionAlarm
    %% 107 = lihProductionExpiredAlarm

    AlarmList = lists:map(fun(AlarmNr) ->
				  case AlarmNr of
				      100 -> Alarm = "lihLKFFaultAlarm";
				      101 -> Alarm = "lihLKFMissingAlarm";
				      102 -> Alarm = "lihEmergencyAlarm";
				      103 -> Alarm = "lihEmergencyExpiredAlarm";
				      104 -> Alarm = "lihIntegrationAlarm";
				      105 -> Alarm = "lihIntegrationExpiredAlarm";
				      106 -> Alarm = "lihProductionAlarm";
				      107 -> Alarm = "lihProductionExpiredAlarm";

				      _ ->
					  Alarm = "dummy",
					  ct:pal("Alarm does not exist! ~p", [AlarmNr]),
					  ct:fail("Alarm does not exist!")
				  end,

				  [{type, Cause},
				   {eriAlarmActiveSpecificProblem, Alarm},
				   {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass2=1"}
				  ]
			  end, AlarmNrList),
    AlarmList.





%%--------------------------------------------------------------------
%% @doc
%% Configure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
config_snmpmgr(_Config) ->
    {{mgr_ip,MgrIp},
     {mgr_port,MgrPort},
     {agent_ip,AgentIp},
     {agent_port,AgentPort}} = rct_snmpmgr:get_mgr_data(snmp1),

    ct:pal(" mgr_ip: ~p, \n mgr_port: ~p, \n agent_ip: ~p, \n agent_port: ~p,", [MgrIp, MgrPort, AgentIp, AgentPort]),

    F = fun() -> {ok,_} = ct_netconfc:open(nc1,[]),
		 ok = case os:getenv("SIM_OR_TARGET") of
			  "sim" ->
			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
								   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
								   [{managedElementId,[],["1"]},
								    {'SystemFunctions',
								     [{systemFunctionsId,[],["1"]},
								      {'SysM',
								       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
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
									    {port,[],[MgrPort]}]}]}]}]}]});
			  "target" ->
			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
								   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
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
									  {'SnmpTargetV2C',
									   [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									   [{snmpTargetV2CId,[],["1"]},
									    {address,[],[MgrIp]},
									    {informRetryCount,[],["3"]},
									    {transportMethod,[],["INFORM"]},
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
    timer:sleep(5000). % Seems necessary before start sending traps

%%--------------------------------------------------------------------
%% @doc
%% DeConfigure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
deconfig_snmpmgr(_Config) ->
    {{mgr_ip,MgrIp},
     {mgr_port,MgrPort},
     {agent_ip,AgentIp},
     {agent_port,AgentPort}} = rct_snmpmgr:get_mgr_data(snmp1),

    ct:pal(" mgr_ip: ~p, \n mgr_port: ~p, \n agent_ip: ~p, \n agent_port: ~p,", [MgrIp, MgrPort, AgentIp, AgentPort]),

    F = fun() -> {ok,_} = ct_netconfc:open(nc1,[]),
		 ok = case os:getenv("SIM_OR_TARGET") of
			  "sim" ->
		      	      ok = ct_netconfc:edit_config(nc1,running,{'ManagedElement',
									[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
									[{managedElementId,[],["1"]},
									 {'SystemFunctions',
									  [{systemFunctionsId,[],["1"]},
									   {'SysM',
									    [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
									    [{sysMId,[],["1"]},
									     {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									      [{snmpId,[],["1"]},
									       {'SnmpTargetV2C',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
												 {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
												 {'nc:operation',"delete"}],[{snmpTargetV2CId,[],["1"]}]}]
									     }]}]}]});
			  "target" ->
		      	      ok = ct_netconfc:edit_config(nc1,running,{'ManagedElement',
									[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
									[{managedElementId,[],["1"]},
									 {'SystemFunctions',
									  [{systemFunctionsId,[],["1"]},
									   {'SysM',
									    [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
									    [{sysMId,[],["1"]},
									     {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									      [{snmpId,[],["1"]},
									       {agentAddress, [{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
									       		       {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
									       		       {'nc:operation',"delete"}],[]},
									       {'SnmpTargetV2C',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
												 {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
												 {'nc:operation',"delete"}],[{snmpTargetV2CId,[],["1"]}]}]
									     }]}]}]})


			      %% Set administrativeState to LOCKED
			      %% ct_netconfc:edit_config(nc1,running,{'ManagedElement',
			      %% 					   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			      %% 					   [{managedElementId,[],["1"]},
			      %% 					    {'SystemFunctions',
			      %% 					     [{systemFunctionsId,[],["1"]},
			      %% 					      {'SysM',
			      %% 					       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
			      %% 					       [{sysMId,[],["1"]},
			      %% 						{'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
			      %% 						 [{snmpId,[],["1"]},
			      %% 						  {administrativeState,[],["LOCKED"]}
			      %% 						  ]}]}]}]})

		      end,
                 ok = ct_netconfc:close_session(nc1)
        end,
    try F()
    catch
        _:Reason ->
            ct_netconfc:close_session(nc1),
           ct:fail(Reason)
    end,
    timer:sleep(1000).

%%--------------------------------------------------------------------
%% @doc
%% Used in cleanup if TC fail, DeConfigure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
deconfig_snmpmgr_no_check() ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ct_netconfc:edit_config(nc1,running,{'ManagedElement',
						 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						 [{managedElementId,[],["1"]},
						  {'SystemFunctions',
						   [{systemFunctionsId,[],["1"]},
						    {'SysM',
						     [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
						     [{sysMId,[],["1"]},
						      {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
						       [{snmpId,[],["1"]},
							{'SnmpTargetV2C',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
									  {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
									  {'nc:operation',"delete"}],[{snmpTargetV2CId,[],["1"]}]}]
						      }]}]}]});
	"target" ->
	    ct_netconfc:edit_config(nc1,running,{'ManagedElement',
						 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						 [{managedElementId,[],["1"]},
						  {'SystemFunctions',
						   [{systemFunctionsId,[],["1"]},
							 {'SysM',
							  [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
							  [{sysMId,[],["1"]},
							   {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
							    [{snmpId,[],["1"]},
							     {agentAddress, [{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
									     {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
									     {'nc:operation',"delete"}],[]},
							     {'SnmpTargetV2C',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
									       {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
									       {'nc:operation',"delete"}],[{snmpTargetV2CId,[],["1"]}]}]
							   }]}]}]})

    end,

    ct_netconfc:close_session(nc1),

    timer:sleep(1000).

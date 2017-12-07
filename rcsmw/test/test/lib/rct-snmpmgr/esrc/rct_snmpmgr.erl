%% -----------------------------------------------------------------------------
%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	rct_snmpmgr.erl %
%% @author etxkols
%% @copyright Ericsson AB 2013-2017
%% @version /main/R2A/R3A/R4A/R5A/R10A/3
%% @doc == Module that starts a SNMP manager for receiving and matching traps ==
%%
%%
%%   This module is intended to be used from a Common Test suite,
%%   where the module is called as a ct_hook at the beginning of
%%   each test suite.<br/>
%%
%%   The ct_hook can be constructed in several ways depending on if
%%   tests are carried out towards one or several SUT(s).<br/>
%%
%% Hook formats:
%% ```{rct_snmpmgr, {[{N, Name, Sname, IPType}], Opts}'''
%%
%% There are two short formats for testing towards one node:
%% ```{rct_snmpmgr, Name}         expands to {rct_snmpmgr, {[{1, Name, username, oam_auto}]}, []}
%%    {rct_snmpmgr, {Name, Opts}} expands to {rct_snmpmgr, {[{1, Name, username, oam_auto}]}, Opts}'''
%%
%% Argument description:
%% ```N        = integer()                      Default is 1. Used to match card in stp.cfg file when running on target.
%%                                              Not used in simulated environment.
%%    Name     = atom()                         Used as identifier
%%    Sname    = username | atom()              Default is username. Used in simulated environment to retrieve SNMP manager port
%%                                              by doing rpc:call.
%%                                              If Sname = username, called erlang node is os:getenv("USER")@hostname,
%%                                              otherwise Sname@hostname. This must be correlated with how rcsssim is started.
%%                                              Not used in target environment.
%%    IPType   =                                Used in target env to specify which IP address SNMP traps will originate from (agent).
%%               oam_auto |                     Agent IP address will be automatically selected depending on precence of -oamap_ipv4 or -oamap_ipv6 flags to rct_run.sh or
%%                                              `{jenkins_config,[{oamap_ipv4, []}]}', `{jenkins_config,[{oamap_ipv6, []}]}' config parameters.
%%               ssh_lmt_ipv4 |                 LMT IPv4 is used, read from stp.cfg file
%%               ssh_TN_A_ipv4 |                TNA IPv4 is used, read from stp.cfg file
%%               ssh_TN_A_ipv4_alt|             Alternateive TNA IPv4 is used, read from stp.cfg file
%%               ssh_lmt_ipv6 |                 LMT IPv6 is used, read from stp.cfg file
%%               ssh_TN_A_ipv6 |                TNA IPv6 is used, read from stp.cfg file
%%               ssh_TN_A_ipv6_alt|             Alternateive TNA IPv6 is used, read from stp.cfg file
%%                                              Not used in simulated environment
%%    Opts     = [Opt]                          Options.
%%    Opt      = {port, Port} | {nport, N} |    Default is {nport, 1}. Defines listening port for SNMP manager.
%%                                              {port, Port} sets a fixed Port number for SNMP manager.
%%                                              {nport, N} calculates a unique portnumber for SNMP manager, (a must, when several
%%                                              users use the same VDI/TS. N is the board which is used to calculate port number.
%%                                              In simulated environment a rpc:call is made to board N, to collect port number.
%%                                              In target environment, the port number is calculated by adding last octet of board
%%                                              LMT IP address to 27000.
%%               format_output |                When format_output is given, logging of traps is made in a oneline format, otherwise
%%                                              a [{Key, Value}] format is printed. The [{Key, Value}] format is the format in which
%%                                              traps are matched, see wait_for_traps/3.
%%               {extra_agents, [IPv4]}         Specify additional agents that may send TRAPs and INFORMs to the SNMP manager. This
%%                                              may be useful for test purposes. TRAPs and INFORMs not coming from the System Under
%%                                              Test will be silently ignored unless specified here.
%%    IPv4     = tuple(integer())               A 4-tuple specifying an ipv4 address.
%%    Port     = integer()                      '''
%%
%% If `rcstprep.sh' script (wrapped by `rcs_install.sh' script) has been run with `-oamap_ipv4' or `-oamap_ipv6' flag, OaM Accesspoint has been configured with either ipv4 or ipv6 address during installation.
%%
%% If `IPType=oam_auto' (default) is set in the testsuite, the SNNP managers agent IP address will be automatically selected depending on precense of `-oamap_ipv4' or `-oamap_ipv6' as arguments to `rct_run.sh' or `{jenkins_config,[{oamap_ipv4, []}]}' or `{jenkins_config,[{oamap_ipv6, []}]}' as config parameters, Examples:
%%
%% ```rct_run.sh -stp dus5000                  SNMP manager will listen for traps from ssh_lmt_ipv4
%%    rct_run.sh -stp dus5000 -oamap_ipv4      SNMP manager will listen for traps from ssh_TN_A_ipv4
%%    rct_run.sh -stp dus5000 -oamap_ipv6      SNMP manager will listen for traps from ssh_TN_A_ipv6'''
%%
%% `-oamap_ipv4' and `-oamap_ipv6' has precedence over `{jenkins_config,[{oamap_ipv4, []}]}' and `{jenkins_config,[{oamap_ipv6, []}]}', i.e. the config parameters will only be checked if the arguments are not given to `rct_run.sh'.
%%
%% == Examples ==
%%
%% ```
%% suite() ->
%%     [{ct_hooks, [{rct_snmpmgr, agent1}]}].
%%
%% mytest(Config) ->
%%     ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCritical},
%%				        {managedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%%				       300).'''
%% Below is an example testsuite that uses netconf to configure trapreceiver on board and sends and waits for 2 traps.
%% ```
%% suite() ->
%%     [{ct_hooks, [{rct_snmpmgr,snmp1},
%%                  {rct_netconf, nc1},
%%                  {cth_conn_log,[]},
%% 		    {rct_rpc, rpc1}]}].
%%
%% config_snmp(_Config) ->
%%     [{mgr_ip,MgrIp},
%%      {mgr_port,MgrPort},
%%      {agent_ip,AgentIp},
%%      {agent_port,AgentPort}] = rct_snmpmgr:get_mgr_data(snmp1),
%%     F = fun() -> {ok,_} = ct_netconfc:open(nc1,[]),
%% 		        ok = case os:getenv("SIM_OR_TARGET") of
%% 			  "sim" ->
%% 			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
%% 								   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 								   [{managedElementId,[],["1"]},
%% 								    {'SystemFunctions',
%% 								     [{systemFunctionsId,[],["1"]},
%% 								      {'SysM',
%% 								       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
%% 								       [{sysMId,[],["1"]},
%% 									{'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
%% 									 [{snmpId,[],["1"]},
%% 									  {administrativeState,[],["UNLOCKED"]},
%% 									  {'SnmpTargetV2C',
%% 									   [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
%% 									   [{snmpTargetV2CId,[],["1"]},
%% 									    {address,[],[MgrIp]},
%% 									    {community,[],["public"]},
%%									    {transportMethod,[],["TRAP"]},
%% 									    {port,[],[MgrPort]}]}]}]}]}]});
%% 			  "target" ->
%% 			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
%% 								   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 								   [{managedElementId,[],["1"]},
%% 								    {'SystemFunctions',
%% 								     [{systemFunctionsId,[],["1"]},
%% 								      {'SysM',
%% 								       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
%% 								       [{sysMId,[],["1"]},
%% 									{'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
%% 									 [{snmpId,[],["1"]},
%% 									  {administrativeState,[],["UNLOCKED"]},
%% 									  {agentAddress,
%% 									   [{struct,"HostAndPort"}],
%% 									   [{host,[],[AgentIp]},
%% 									    {port,[],[AgentPort]}]},
%% 									  {'SnmpTargetV2C',
%% 									   [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
%% 									   [{snmpTargetV2CId,[],["1"]},
%% 									    {address,[],[MgrIp]},
%%									    {informRetryCount,[],["3"]},
%%									    {transportMethod,[],["INFORM"]},
%% 									    {community,[],["public"]},
%% 									    {port,[],[MgrPort]}]}]}]}]}]})
%% 		      end,
%%                ok = ct_netconfc:close_session(nc1)
%%         end,
%%     try F()
%%     catch
%%         _:Reason ->
%%             ct_netconfc:close_session(nc1),
%%             ct:fail(Reason)
%%     end,
%%     timer:sleep(1000). % Seems necessary before start sending traps.
%%
%% send_traps(_Config) ->
%%     ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCritical},
%% 				      {eriAlarmActiveSpecificProblem,"lihLKFFaultAlarm"},
%% 				      {eriAlarmActiveManagedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%% 				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]}],
%% 				    10),
%%     ok = rct_rpc:call(rpc1,lih_fm, request_LKF_fault_alarm,[],1000),
%%     ok = rct_snmpmgr:check_traps(),
%%     ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
%% 				      {eriAlarmActiveSpecificProblem,"lihLKFFaultAlarm"},
%% 				      {eriAlarmActiveManagedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%% 				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]}],
%% 				    10),
%%     ok = rct_rpc:call(rpc1,lih_fm, cancel_LKF_fault_alarm,[],1000),
%%     ok = rct_snmpmgr:check_traps().'''
%% @end

-module(rct_snmpmgr).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R10A/3').
-date('2017-10-25').
-author('etxkols').
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/1      2013-05-17 etxkols     Created
%%% R2A/2      2013-05-31 etxkols     edoc fixes
%%% R2A/3      2013-06-10 etxkols     fixes
%%% R2A/4      2014-03-26 etxkols     Faulty init/2 return value
%%% R2A/6      2014-08-28 erarafo     Edoc adjusted
%%% R2A/7      2014-09-03 erarafo     Sanity check of traps
%%% R2A/8      2014-10-03 erarafo     Added option for additional agents
%%% R2A/9      2014-10-03 erarafo     Duplicate addresses elimination
%%% R3A/1      2014-10-03 erarafo     Merge from R2
%%% R2A/10     2014-10-04 erarafo     Preserve address order too
%%% R3A/2      2014-10-04 erarafo     Merge from R2
%%% R3A/3      2015-10-05 erarafo     Embarrassing bug fixed
%%% R3A/4      2015-10-15 etxkols     Take care of 10.86.147.0 net
%%% R3A/5      2015-04-27 etxkols     Fix for 2 labs
%%% R3A/6      2015-04-28 etxkols     Fix for Ki30 lab
%%% R3A/7      2015-05-07 etxkols     New IP addresses in Ki10
%%% R4A/1      2015-11-26 etxkols     Handling of OamAccessPoint
%%% R4A/2      2015-12-01 etxkols     oam_auto default IPType, will select OAMAP or LMT
%%% R5A/1      2016-02-03 etxkols     ipv6
%%% R5A/2      2016-02-04 etxkols     Temporarily backing out OAMAP for sec board until Jenkins is synced.
%%% R5A/3      2016-02-09 etxkols     New flags
%%% R5A/4      2016-02-10 etxkols     Documentation
%%% R5A/5      2016-02-10 etxkols     Documentation
%%% R5A/6      2016-02-12 etxkols     using rct_oamap_ipv:mgmnt_iptype/1
%%% R5A/7      2016-03_03 etxkols     ipvt fix
%%% R5A/8      2016-03_03 etxkols     ipv6 again
%%% R5A/9      2016-03_03 etxkols     ipv6 again
%%% R5A/10     2016-03_03 etxkols     A better ipv6 solution
%%% R5A/11     2016-03_21 etxkols     New IP net in lab and cloud
%%% R5A/12     2016-04_01 etxkols     Cloud fix
%%% R5A/13     2016-04_19 etxkols     CommonTest no longer supports io:format for html tags
%%% R5A/14     2016-05_13 etxkols     New network in cloud
%%% R5A/15     2016-06_21 etxkols     GIT migration
%%% R5A/16     2016-08_19 etxkols     Removal of references to CC
%%% R5A/17     2017-03_10 etxkols     redhat007
%%% R10A/1     2017-05_09 etxkols     New return_received_traps option to wait_for_traps/3 
%%% R10A/2     2017-08_30 etxkols     Adaption to youlab 
%%% R10A/3     2017-10_25 etxkols     Adaption to youlab 
%%% ----------------------------------------------------------
-export([init/2,
	 pre_init_per_suite/3,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 terminate/1]).

-export([get_mgr_data/1,
	 get_mgr_conf/1,
	 wait_for_traps/2,
	 wait_for_traps/3,
	 check_traps/0]).

-export([get_attrs_trap_receiver_create/1,
	 get_attrs_trap_receiver_delete/0]).

-define(AGENT_PORT, 6161).
-define(MIB_DIR,{"RCT_TOP","test/lib/rct-snmpmgr/mibs"}).       % Directory with mibs

-define(TIMEOUT_TIME_TICKS, "300").
-define(INFORM_RETRY, "8").

-define(ATTRS_LDN_PATH(SnmpAttrs),
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'SysM',
	     [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	     [{sysMId,[],["1"]},
	      {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	       [{snmpId,[],["1"]} | SnmpAttrs]}]}]}]}).

-define(ATTRS_CREATE(MgrIp, MgrPort, AgentAddr, SnmpTgt),
	?ATTRS_LDN_PATH([{administrativeState,[],["UNLOCKED"]}] ++
		   AgentAddr ++
		   [{'SnmpTargetV2C',
		     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
		     [{snmpTargetV2CId,[],["1"]},
		      {address,[],[MgrIp]}] ++
			 SnmpTgt ++
			 [{community,[],["public"]},
			  {port,[],[MgrPort]}]}])).

-define(ATTRS_CREATE_SIM(MgrIp, MgrPort),
	?ATTRS_CREATE(MgrIp, MgrPort, [], [{transportMethod,[],["TRAP"]}])).

-define(ATTRS_CREATE_TGT(MgrIp, MgrPort, AgentIp, AgentPort),
	?ATTRS_CREATE(MgrIp, MgrPort,[{agentAddress,
				       [{struct,"HostAndPort"}],
				       [{host,[],[AgentIp]},
					{port,[],[AgentPort]}]}],
		      [{informTimeout,[],[?TIMEOUT_TIME_TICKS]},
		       {informRetryCount,[],[?INFORM_RETRY]},
		       {transportMethod,[],["INFORM"]}])).

-define(ATTRS_DELETE,
	[{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
	 {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
	 {'nc:operation',"delete"}]).

-define(ATTRS_DELETE(AgentAddr),
	?ATTRS_LDN_PATH(AgentAddr ++
			    [{'SnmpTargetV2C',
			      ?ATTRS_DELETE,
			      [{snmpTargetV2CId,[],["1"]}]}])).

-define(ATTRS_DELETE_SIM, ?ATTRS_DELETE([])).

-define(ATTRS_DELETE_TGT,
	?ATTRS_DELETE([{agentAddress, ?ATTRS_DELETE, []}])).


%%===========================================================================
%% @spec get_mgr_data(Node) -> {{mgr_ip,MgrIp},{mgr_port,MgrPort},{agent_ip,AgentIp},{agent_port,AgentPort}} | {error,Reason}
%% Node      = atom()
%% MgrIp     = string()
%% MgrPort   = string()
%% AgentIp   = string()
%% AgentPort = string()
%% Reason    = term()
%% @doc Get manager data necessary to configure board via netconf or CLI.<br/><br/>
%% Example:
%% ```
%% rct_snmpmgr:get_mgr_data(snmp1) ->
%%    {{mgr_ip,"147.214.13.193"},
%%     {mgr_port,"34652"},
%%     {agent_ip,"147.214.13.193"}},
%%     {agent_port,"6161"}}'''
%% @end
%%===========================================================================
get_mgr_data(Node) ->
    list_to_tuple(get_mgr_conf(Node)).


get_mgr_conf(Node) ->
    [{mgr_ip,{MIP1,MIP2,MIP3,MIP4}},
     {mgr_port,MgrPort},
     {agent_ip,{AIP1,AIP2,AIP3,AIP4}},
     {agent_port,AgentPort}] = ct:get_config(make_name_module(Node)),
    MgrIpStr = integer_to_list(MIP1)++"."++integer_to_list(MIP2)++"."
	++integer_to_list(MIP3)++"."++integer_to_list(MIP4),
    MgrPortStr = integer_to_list(MgrPort),
    AgentIpStr = integer_to_list(AIP1)++"."++integer_to_list(AIP2)++"."
	++integer_to_list(AIP3)++"."++integer_to_list(AIP4),
    AgentPortStr = integer_to_list(AgentPort),
    [{mgr_ip,MgrIpStr},
     {mgr_port,MgrPortStr},
     {agent_ip,AgentIpStr},
     {agent_port,AgentPortStr}].

%%===========================================================================
%% @spec get_attrs_trap_receiver_create(MgrName) -> Attrs
%% Mgrname = atom()
%% Attrs   = tuple()
%% @doc Get default netconf attributes for creating a trap receiver.
%% @end
%%===========================================================================
get_attrs_trap_receiver_create(MgrName) when is_atom(MgrName) ->
    SNMPConf = get_mgr_conf(MgrName),
    get_attrs_trap_receiver_create(SNMPConf);

get_attrs_trap_receiver_create(SNMPConf) when is_list(SNMPConf) ->
    MgrIp = proplists:get_value(mgr_ip, SNMPConf),
    MgrPort = proplists:get_value(mgr_port, SNMPConf),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ?ATTRS_CREATE_SIM(MgrIp, MgrPort);
	_Target ->
	    AgentIp = proplists:get_value(agent_ip, SNMPConf),
	    AgentPort = proplists:get_value(agent_port, SNMPConf),
	    ?ATTRS_CREATE_TGT(MgrIp, MgrPort, AgentIp, AgentPort)
    end;

get_attrs_trap_receiver_create(SNMPConf) when is_tuple(SNMPConf) ->
    get_attrs_trap_receiver_create(tuple_to_list(SNMPConf)).

%%===========================================================================
%% @spec get_attrs_trap_receiver_delete() -> Attrs
%% Attrs   = tuple()
%% @doc Get default netconf attributes for creating a trap receiver.
%% @end
%%===========================================================================
get_attrs_trap_receiver_delete() ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ?ATTRS_DELETE_SIM;
	_Target ->
	    ?ATTRS_DELETE_TGT
    end.

%%===========================================================================
%% @spec wait_for_traps(Traps, Timeout) -> ok | {error,Reason}
%% @doc Same as wait_for_traps(Traps, [], Timeout).
%% @end
%%===========================================================================
wait_for_traps(Traps, Timeout) ->
    wait_for_traps(Traps, [], Timeout).

%%===========================================================================
%% @spec wait_for_traps(Traps, Opts, Timeout) -> ok | {ok, AllRecTrapData} | {error,Reason}
%% Traps          = [Trap | {Node, Trap}]
%% Opts           = [Opt]
%% Opt            = wait | any_order | other_alarms_allowed | other_alerts_allowed | {allowed_traps, Traps} | return_received_traps
%% Trap           = [{Key, Value}]
%% Key            = atom()
%% Value          = string() | atom() | integer()
%% Node           = atom()
%% Timeout        = integer()
%% AllRecTrapData = string()
%% Reason         = term()
%%
%% @doc Waits for traps in max Timeout seconds.<br/><br/>
%% If wait is in Otps, function hangs until wanted traps are received or until timeout.<br/>
%% If wait is NOT in Opts, function returns immediately and result is polled with check_traps/0.<br/>
%% Alarms and Alerts are received in a Key, Value format, ex:
%% ```
%% [{type,eriAlarmCritical},
%%  {agentAddr,"10.86.148.133"},
%%  {managedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"},
%%  {lastEventTime,"1970-1-1 3:41:22"},
%%  {sequenceNumber,7},
%%  {majorType,193},
%%  {minorType,6619236},
%%  {specificProblem,"lihLKFFaultAlarm"},
%%  {eventType,processingErrorAlarm},
%%  {probableCause,999},
%%  {additionalInfo,"FIXME"},
%%  {moreadditionalInfo,false},
%%  {resourceId,false}]'''
%% Any {Key, Value} above can be used for matching traps, ex:
%% ```
%% mytest(Config) ->
%%     ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCritical},
%%			                 {managedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}],
%%				        [{type,eriAlarmCleared},
%%				         {managedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%%				       60).'''
%% Example above will wait for 2 traps in exact order (NO other traps are accepted). Opts may change this behaviour.<br/>
%% ```
%% any_order              By default the traps must arrive in the given order. If any_order is specified
%%                        then the expected traps may arrive in any order.
%% {allowed_traps, Traps} Traps that are allowed without causing the function to return {error, Reason}.
%%                        This option takes precedence over other_alarms_allowed and other_alerts_allowed.
%% other_alarms_allowed   All other alarm traps are allowed.
%% other_alerts_allowed   All other alert traps are allowed.
%% return_received_traps  Will return the matched traps in {ok, AllRecTrapData} so testcase can match it
%% wait                   By default the function returns ok promptly and rct_snmpmgr:check_traps() can
%%                        be used to retrieve the result. If wait is specified then the function hangs
%%                        until the expected traps are received. '''
%% Examples:
%% ```
%% mytest(Config) ->
%%    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCritical},
%%				       {specificProblem,"lihLKFFaultAlarm"},
%%				       {managedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%%				      [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]}],
%%				     10),
%%    ok = rct_rpc:call(rpc1,lih_fm, request_LKF_fault_alarm,[],1000),
%%    ok = rct_snmpmgr:check_traps(),
%%    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
%%				        {specificProblem,"lihLKFFaultAlarm"},
%%				        {managedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%%				      [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]},
%%    				        return_received_traps],
%%				     10),
%%    ok = rct_rpc:call(rpc1,lih_fm, cancel_LKF_fault_alarm,[],1000),
%%    {ok, Traps} = rct_snmpmgr:check_traps().
%%
%% mytest(Config) ->
%%     timer:apply_after(10000, rct_rpc, call, [rpc1,lih_fm, request_LKF_fault_alarm,[],1000]),
%%     ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCritical},
%% 				         {eriAlarmActiveSpecificProblem,"lihLKFFaultAlarm"},
%% 				         {eriAlarmActiveManagedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%% 				       [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]},
%% 				        wait],
%% 				       60),
%%     timer:apply_after(2000, rct_rpc, call, [rpc1,lih_fm, cancel_LKF_fault_alarm,[],1000]),
%%     {ok, Traps} = rct_snmpmgr:wait_for_traps([[{eriAlarmActiveSpecificProblem,"lihLKFFaultAlarm"},
%% 				                  {type,eriAlarmCleared},
%% 				                  {eriAlarmActiveManagedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%% 				                [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]},
%%    			         	         return_received_traps,
%% 				                 wait],
%% 				                60).'''
wait_for_traps(Traps, Opts, Timeout) ->
    [check_trap_well_formed(Trap, expected_traps) || Trap <- Traps],
    case proplists:get_value(allowed_traps, Opts) of
	undefined ->
	    ok;
	AllowedTraps ->
	    [check_trap_well_formed(Trap, allowed_traps) || Trap <- AllowedTraps]
    end,
    rcs_snmpmgr:wait_for_traps(add_agentAddr(Traps, []), Opts, Timeout).

%%% ----------------------------------------------------------
%%% @doc Returns true if the given trap is well-formed. A trap
%%% is expected to be represented by a property list with unique
%%% keys.
%%% @end
%%% ----------------------------------------------------------
check_trap_well_formed(Trap, Context) ->
    if
	is_list(Trap) ->
	    Keys = proplists:get_keys(Trap),
	    if
		length(Keys) < length(Trap) ->
		    ct:fail("context: ~p, keys not unique in trap:~n"
			    "~p",
			    [Context, Trap]);
		true ->
		    ok
	    end;
	true ->
	    ct:fail("context: ~p, ill-formed trap (not a proplist):~n"
		    "~p",
		    [Context, Trap])
    end.


%%===========================================================================
%% @spec check_traps() -> ok | {error,Reason}
%% Reason  = term()
%%
%% @doc Checks that traps according to wait_for_traps/3 are received according to input data<br/>
check_traps() ->
    rcs_snmpmgr:check_traps().

%%===========================================================================
%% @hidden
%% add_agentAddr([{Node, Trap}|Traps], R) -> [string()] | {error, term()}
%% If multi Agents, Nodes are replaced with their IP addresses and inserted as
%% {agentAddr, "IP address"} in search patten for traps.
%% This is to identify that trap comes from correct Node.
%%===========================================================================
add_agentAddr([], R) ->
    R;
add_agentAddr([{Node, Trap}|Traps], R) ->
    case ct:get_config({make_name_module(Node),agent_ip}) of
	{IP1, IP2, IP3, IP4} ->
	    AgentAddr = integer_to_list(IP1) ++ "." ++
		        integer_to_list(IP2) ++ "." ++
		        integer_to_list(IP3) ++ "." ++
		        integer_to_list(IP4),
	    add_agentAddr(Traps, R ++ [[{agentAddr, AgentAddr}] ++ Trap]);
	Other ->
	    {error, Other}
    end;
add_agentAddr([Trap|Traps], R) ->
    add_agentAddr(Traps, R ++ [Trap]).

%%===========================================================================
%% @hidden
%% init function for ct_hook
%%===========================================================================
init(_Id, Opts) ->
    {ok,Opts}.

%%===========================================================================
%% @hidden
%% Figures out Manager IP address and port, and Agents IP addresses and Ports
%% Start snmp manager
%%===========================================================================
pre_init_per_suite(Suite,Config,Name) when is_atom(Name) ->
    pre_init_per_suite(Suite,Config,{[{1, Name, username, oam_auto}], []});
pre_init_per_suite(Suite,Config,{Name, Opts}) when is_atom(Name) ->
    pre_init_per_suite(Suite,Config,{[{1, Name, username, oam_auto}], Opts});
pre_init_per_suite(Suite,Config,{CthState, Opts}) ->
    CthState2 = [{N,Name,Username,rct_oamap_ipv:mgmnt_iptype(IPType)}||{N,Name,Username,IPType}<-CthState],
    pre_init_per_suite2(Suite,Config,{CthState2, Opts}).

pre_init_per_suite2(_Suite,Config,{CthState, Opts}) ->
    Opts2 = case proplists:get_value(port,Opts) of
		undefined ->
		    Opts ++ [{nport, 1}];
		_ ->
		    Opts
	    end,
    case get_local_ips() of
	{MGRipv4,MGRipv6} ->
	    case get_mgrport(CthState, Opts2) of
		{ok, MGRPort} ->
		    case do_pre_init_per_suite(CthState, MGRipv4, MGRPort, []) of
			{ok, AgentIps} ->
			    ExtendedIps = addAddresses(
					    AgentIps,
					    proplists:get_value(extra_agents, Opts2, [])),
			    case MGRipv6 of
				[] ->
				    ct:log("Starting SNMP Manager with:~nIPv4 manager address ~s, port ~p~nAgent addresses ~p, port ~p",
					   [inet:ntoa(MGRipv4), MGRPort, 
					    [inet:ntoa(ExtendedIp)||ExtendedIp<-ExtendedIps],?AGENT_PORT]);
				_ ->
				    ct:log("Starting SNMP Manager with:~nIPv4 manager address ~s, port ~p~nIPv6 manager address ~s, port ~p~nAgent addresses ~p, port ~p",
					   [inet:ntoa(MGRipv4), MGRPort, 
					    inet:ntoa(MGRipv6), MGRPort, 
					    [inet:ntoa(ExtendedIp)||ExtendedIp<-ExtendedIps],?AGENT_PORT])
			    end,
			    FormatOutput =  proplists:get_value(format_output,Opts2,false),			    
			    {RCT_TOP, MIB_PATH} = ?MIB_DIR,
			    case os:getenv(RCT_TOP) of
				false ->
				    {{fail, "Could not resolve environment variable $RCT_TOP"},CthState};
				Base ->
				    MIB_DIR = filename:join(Base,MIB_PATH),
				    ct:pal("MIB_DIR ~p~n",[MIB_DIR]),
				    case rcs_snmpmgr:start([{addresses, ExtendedIps},
							    {port, MGRPort},
							    {snmpdir, ?config(priv_dir,Config)},
							    {mibdir, MIB_DIR},
							    {format_output, FormatOutput}]) of
					Reply when Reply == {ok, xmgr_started};
						   Reply == {ok, already_started} ->
					    
					    {Config, CthState};
					Reply ->
					    ct:log(lightred,
						   "~p ~p Could not start SNMP manager with addresses ~p port ~p, Reason: ~p",
						   [?MODULE,do_pre_init_per_suite, ExtendedIps, MGRPort, Reply]),
					    {{fail, "Could not start SNMP manager"},CthState}
				    end
			    end;
			error ->
			    {{fail, "Faulty configuration when starting SNMP manager"},CthState}
		    end;
		error ->
		    {{fail, "Could not find snmpmgr local UDP port"},CthState}
	    end;
	error ->
	    {{fail, "Could not get snmpmgr local IP address"},CthState}
    end.

%% If `rcstprep.sh' script (wrapped by `rcs_install.sh' script) has been run with `-oamap_ipv4' or `-oamap_ipv6' flag, OaM Accesspoint has been configured with either ipv4 or ipv6 address during installation.
%%
%% If `IPType=oam_auto' (default) is set in the testsuite, the SNNP managers agent IP address will be automatically selected depending on precense of `-oamap_ipv4' or `-oamap_ipv6' as arguments to `rct_run.sh' or `{jenkins_config,[{oamap_ipv4, []}]}' or `{jenkins_config,[{oamap_ipv6, []}]}' as config parameters, Examples:
%%
%% ```rct_run.sh -stp dus5000                  SNMP manager will listen for traps from ssh_lmt_ipv4
%%    rct_run.sh -stp dus5000 -oamap_ipv4      SNMP manager will listen for traps from ssh_TN_A_ipv4
%%    rct_run.sh -stp dus5000 -oamap_ipv6      SNMP manager will listen for traps from ssh_TN_A_ipv6'''
%%
%% `-oamap_ipv4' and `-oamap_ipv4' has precedence over `{jenkins_config,[{oamap_ipv4, []}]}' and `{jenkins_config,[{oamap_ipv6, []}]}', i.e. the config parameters will only be checked if the arguments are not given to `rct_run.sh'.
%% check_iptype(_N,oam_auto) ->
%%     case init:get_argument(oamap_ipv4) of
%% 	{ok,[[]]} ->
%% 	    ssh_TN_A_ipv4;
%% 	_ ->
%% 	    case init:get_argument(oamap_ipv6) of
%% 		{ok,[[]]} ->
%% 		    ssh_TN_A_ipv6;
%% 		_ ->
%% 		    case ct:get_config({jenkins_config,oamap_ipv4}) of
%% 			[] ->
%% 			    ssh_TN_A_ipv4;
%% 			_ ->
%% 			    case ct:get_config({jenkins_config,oamap_ipv6}) of
%% 				[] ->
%% 				    ssh_TN_A_ipv6;
%% 				_ ->
%% 				    ssh_lmt_ipv4
%% 			    end
%% 		    end
%% 	    end
%%     end;
%% check_iptype(_N,IPType) ->
%%     IPType.

%%===========================================================================
%% @hidden
%% pre_init_per_testcase(TC,Config, CthStates).
%% Starts logging traps to file
%%===========================================================================
pre_init_per_testcase(TC,Config, CthStates) ->
    LogFile = filename:join(?config(priv_dir,Config), atom_to_list(TC) ++ "_snmpmgr.log"),
    case file:open(LogFile, [write]) of
        {ok, Handle} ->
	    rcs_snmpmgr:start_logging(Handle),
	    put(rct_snmpmgr_logfile, LogFile), % timetrap may destroy Config variable
	    {Config, CthStates};
        {error, Reason} ->
	    ct:log(lightred,
		   "~p ~p Could not open ~s for SNMP trap logging, Reason: ~p",
		   [?MODULE,pre_init_per_testcase,LogFile, Reason]),
	    {{fail, could_not_open_snmp_logfile}, [LogFile]}
    end.

%%===========================================================================
%% @hidden
%% post_end_per_testcase(TC,Config,Return,CthStates).
%% Stops logging to file and prints link to logfile in html log
%%===========================================================================
post_end_per_testcase(_TC,_Config,Return,CthStates) ->
    rcs_snmpmgr:stop_logging(),
    LogFile = get(rct_snmpmgr_logfile),
    ct:log(default, 1, "<a href=\"~s\">~s</a>", [LogFile, "SNMP manager log"], [no_css]),
    %% io:format("<a href=\"~s\">~s</a>", [LogFile, "SNMP manager log"]),
    {Return, CthStates}.

%%===========================================================================
%% @hidden
%% terminate(States).
%% Stops SNMP manager
%%===========================================================================
terminate(_States) ->
    rcs_snmpmgr:stop(),
    ok.

%%===========================================================
%% do_pre_init_per_suite(CthStates, MGRipv4, MGRPort, AgentIPs) ->
%% {ok, [{IP1,IP2,IP3,IP4}]} | error
%% makes CT aliases for manager and agents data
%%===========================================================
do_pre_init_per_suite([], _, _, AgentIPs) ->
    {ok, AgentIPs};
do_pre_init_per_suite([{N, Name, _Sname, IPType}|T], MGRipv4, MGRPort, AgentIPs) ->
    case get_agent_ip(N, Name, IPType, MGRipv4) of
	{ok, AgentIP} ->
	    case rct_multi_node_cfg:require(make_name_module(Name),[{mgr_ip,MGRipv4},
								    {mgr_port,MGRPort},
								    {agent_ip, AgentIP},
								    {agent_port, ?AGENT_PORT}] ) of
		ok ->
		    do_pre_init_per_suite(T, MGRipv4, MGRPort, AgentIPs ++ [AgentIP]);
		{error, Reason} ->
		    ct:log(lightred,
			   "~p: ~p ~p Could not initiate config parameter ~p, Reason: ~p",
			   [Name, ?MODULE,do_pre_init_per_suite,[{mgr_ip,MGRipv4},{mgr_port,MGRPort}, {agent_ip, AgentIP}, {agent_port, ?AGENT_PORT}], Reason]),
		    error
	    end;
	error ->
	    error
    end.

%%===========================================================
%% get_agent_ip(N, Name, IPType, MGRipv4) -> {ok, [{IP1,IP2,IP3,IP4}]} | error
%% Find out a IP address that the manager will listen for
%% traps from.
%% For sim, managers IP address is used
%% For target, the agents IP address i used
%%===========================================================
get_agent_ip(N, Name, IPType, MGRipv4) ->
     case os:getenv("SIM_OR_TARGET") of
	 "sim" ->
             {ok, MGRipv4};
	 TARG_OR_CLOUD when TARG_OR_CLOUD == "target";
			    TARG_OR_CLOUD == "cloudish" ->
	     case rct_multi_node_cfg:get_config(N,{IPType,ssh}) of
		 undefined ->
		     ct:log(lightred,
			    "~p: ~p ~p Could not read config parameter ~p",
			    [Name, ?MODULE,get_agent_ip,{N,IPType,ssh}]),
		     error;
		 IPStr ->
		     inet:parse_address(IPStr)
              end
     end.

%%===========================================================
%% get_mgrport(CthState, Opts) -> {ok, Port} | error
%% Returns mgr port to which agents should send their traps
%% If {port,Port} is given in Opts, Port is used.
%% If {nport,N} is given in Opts, N is used to get port
%%    For sim, rpc:call is made to node 1 to get port number
%%    For target, 27000 + (148 - Net) * 256 + Oct, where 10.86.Net.Oct calculates the port
%%===========================================================
get_mgrport(CthState, Opts) ->
    case proplists:get_value(nport, Opts) of
	undefined ->
	    case proplists:get_value(port, Opts) of
		undefined ->
		    ct:log(lightred,
			   "~p ~p Could not start SNMP manager with addresses ~p port ~p",
			   [?MODULE, get_mgrport, Opts]),
		    error;
		Port ->
		    {ok, Port}
	    end;
	N ->
	    case lists:keysearch(N, 1, CthState) of
		{value, {N, Name, Sname, IPType}} ->
		    case os:getenv("SIM_OR_TARGET") of
			TARG_OR_CLOUD when TARG_OR_CLOUD == "target";
					   TARG_OR_CLOUD == "cloudish" ->
			    case rct_multi_node_cfg:get_config(N,{IPType,ssh}) of
				undefined ->
				    ct:log(lightred,
					   "~p: ~p ~p Could not read config parameter ~p",
					   [Name, ?MODULE,get_mgrport,{N,IPType,ssh}]),
				    error;
				IPStr ->
				    %% Quick fix to support 3 labs
				    case inet:parse_address(IPStr) of					    
					{ok,{10,67,224,Oct}} ->                   {ok, 27256 + Oct};                % Ki10 EE can be removed when MW running on 225
					{ok,{10,67,225,Oct}} ->                   {ok, 27000 + Oct};                % Ki10 MW1
					{ok,{10,67,234,Oct}} ->                   {ok, 27000 + 256 + Oct};          % Ki10 MW2
					{ok,{10,67,226,Oct}} ->                   {ok, 27000 + Oct};                % Ki10 MW1 OamAP
					{ok,{10,67,235,Oct}} ->                   {ok, 27000 + 256 + Oct};          % Ki10 MW2 OamAP
					{ok,{10,67,233,Oct}} ->                   {ok, 27000 + Oct};                % Ki10 MW1 OamAP ALT 
					{ok,{10,67,236,Oct}} ->                   {ok, 27000 + 256 + Oct};          % Ki10 MW2 OamAP ALT 
					{ok,{10,68,102,Oct}} ->                   {ok, 27000 + 512 + Oct};          % Ki10 Cloud Mirantis
					{ok,{10,68,98,Oct}}  ->                   {ok, 27000 + 512 + Oct};          % Ki10 Cloud redhat007-rcs-ci 
					{ok,{10,68,97,Oct}}  ->                   {ok, 27000 + 768 + Oct};          % Ki10 Cloud redhat007-rcs
					{ok,{8193,7024,25217,61696,_,_,_,Hex}} -> {ok, 27000 + to_dec(Hex)};        % Ki10 MW1 OamAP 					    
					{ok,{8193,7024,25217,62592,_,_,_,Hex}} -> {ok, 27000 + to_dec(Hex)};        % Ki10 MW1 OamAP ALT  
					{ok,{8193,7024,25217,62848,_,_,_,Hex}} -> {ok, 27000 + 256 + to_dec(Hex)};  % Ki10 MW2 OamAP					    
					{ok,{8193,7024,33426,1536, _,_,_,Hex}} -> {ok, 27000 + 256 + to_dec(Hex)};  % Ki10 MW2 OamAP ALT
					{ok,{8193,7024,25218,45312,_,_,_,Hex}} -> {ok, 27000 + 512 + to_dec(Hex)};  % Ki10 Cloud redhat007-rcs-ci
					{ok,{8193,7024,25218,45248,_,_,_,Hex}} -> {ok, 27000 + 768 + to_dec(Hex)};  % Ki10 Cloud redhat007-rcs
					{ok,{10,67,140,Oct}} ->                   {ok, 27000 + Oct};                % youlab EE
					{ok,{10,67,141,Oct}} ->                   {ok, 27000 + Oct};                % youlab MW1
					{ok,{10,67,142,Oct}} ->                   {ok, 27256 + Oct};                % youlab MW2				    
					{ok,{10,67,162,Oct}} ->                   {ok, 27000 + Oct};                % youlab MW1 OamAP					    
					{ok,{10,67,169,Oct}} ->                   {ok, 27256 + Oct};                % youlab MW2 OamAP
					{ok,{10,67,170,Oct}} ->                   {ok, 27000 + Oct};                % youlab MW1 OamAP ALT 
					{ok,{10,67,171,Oct}} ->                   {ok, 27256 + Oct};                % youlab MW2 OamAP ALT 
					{ok,{10,209,48,Oct}} ->                   {ok, 27512 + Oct};                % youlab Cloud redhat017_rcs-ci /24 
					{ok,{10,209,49,Oct}} ->                   {ok, 27768 + Oct};                % youlab Cloud redhat017_XFT /24 
					{ok,{10,209,51,Oct}} ->                   {ok, 28024 + Oct};                % youlab Cloud redhat017_sim-ci /24 
					{ok,{10,209,50,Oct}} ->                   {ok, 28280 + Oct};                % youlab Cloud redhat017_ee-ci /25  
					{ok,{8193,7024,33426,53504,_,_,_,Hex}} -> {ok, 27000 + Hex};                % youlab MW1 OamAP					    
					{ok,{8193,7024,33426,54400,_,_,_,Hex}} -> {ok, 27256 + Hex};                % youlab MW2 OamAP					    
					{ok,{8193,7024,33426,54528,_,_,_,Hex}} -> {ok, 27000 + Hex};                % youlab MW1 OamAP ALT				    
					{ok,{8193,7024,33426,54656,_,_,_,Hex}} -> {ok, 27256 + Hex};                % youlab MW2 OamAP ALT					    
					{ok,{8193,7024,33281,3072, _,_,_,Hex}} -> {ok, 27512 + Hex};                % youlab Cloud redhat017_rcs-ci					    
					{ok,{8193,7024,33281,3136, _,_,_,Hex}} -> {ok, 27768 + Hex};                % youlab Cloud redhat017_XFT					    
					{ok,{8193,7024,33281,3264, _,_,_,Hex}} -> {ok, 28024 + Hex};                % youlab Cloud redhat017_sim-ci			    
					{ok,{8193,7024,33281,3200, _,_,_,Hex}} -> {ok, 28280 + Hex}                 % youlab Cloud redhat017_ee-ci					    
				    end
				    %% {ok, {_,_,Net,Oct}} = ipstr_to_tuple(IPStr),
				    %% {ok, 27000 + (148 - Net) * 256 + Oct}
			    end;
			"sim" ->
			    User = get_sim_sname_du(os:getenv("USER")),
			    {ok, HostName} = inet:gethostname(),
			    ErlNode = list_to_atom(User ++ "@" ++ HostName),
			    case rpc:call(ErlNode, sysEnv, get_port_conf, [snmpMgrRem]) of
				Port when is_integer(Port) ->
				    {ok,Port};
				Other ->
				    ct:log(lightred,
					   "~p: ~p ~p Could not get sim port "
					   "for SNMP manager , Reason: ~p"
					   "ErlNode: ~p"
					   "Sname: ~p~n",
					   [Name, ?MODULE, get_mgrport, Other, ErlNode, Sname]),
				    error
			    end
		    end;
		false ->
		    ct:log(lightred,
			   "~p ~p Could not find instance ~p in config",
			   [?MODULE,get_mgrport,N]),
		    error
	    end
    end.

to_dec(Hex) ->
    list_to_integer(integer_to_list(Hex, 16)).

%%===========================================================
%% get_local_ip() -> {ok, {IP1,IP2,IP3,IP4}} | error
%% Figures out local ip address of this terminal server
%%===========================================================
%% get_local_ip() ->
%%     {ok, Hostname} = inet:gethostname(),
%%     case inet:gethostbyname(Hostname) of
%% 	{ok,{_,_,_,_,_,[IPv4]}} ->
%% 	    IPv4;
%% 	    %% {ok,IFs} = inet:getifaddrs(),
%% 	    %% [Items] = [Items||{"eth0",Items}<-IFs],
%% 	    %% case [IP||{addr,IP={IP1,_,_,_,_,_,_,_}}<-Items,IP1 =/= 16#fe80] of
%% 	    %% 	[IPv6] ->
%% 	    %% 	    {{ipv4,IPv4},{ipv6,IPv6}};
%% 	    %% 	[] ->
%% 	    %% 	    {{ipv4,IPv4},{ipv6,[]}}
%% 	    %% end;
%% 	Other ->
%% 	    ct:log(lightred,
%% 		   "~p ~p Could not get snmpmgr IP address, Reason ~p",
%% 		   [?MODULE,do_pre_init_per_suite, Other]),
%% 	    error
%%     end.

%% get_local_ips() ->
%%     {ok,IFs} = inet:getifaddrs(),
%%     case [Items||{"eth0",Items}<-IFs] of
%%         [Items] ->
%%             case [IPv4||{addr,IPv4={_,_,_,_}}<-Items] of
%%                 [IPv4] ->
%%                     case [IPv6||{addr,IPv6={IP1,_,_,_,_,_,_,_}}<-Items,IP1 == 16#2001] of
%%                         [_,IPv6] ->
%%                             {IPv4,IPv6};
%%                         [IPv6] ->
%%                             {IPv4,IPv6};
%%                         [] ->
%%                             {IPv4,[]}
%%                     end;
%%                 _ -> error
%%             end;
%%         _ ->
%%             error
%%     end.

get_local_ips() ->
    {ok,IFs} = inet:getifaddrs(),
    case [Items||{"eth0",Items}<-IFs] of
	[Items] ->
	    case [IPv4||{addr,IPv4={_,_,_,_}}<-Items] of
		[{_,_,_,Last}=IPv4] ->
		    Hex = list_to_integer(integer_to_list(Last),16),
		    case [IPv6||{addr,IPv6={IP1,_,_,_,_,_,_,IP8}}<-Items,IP1 == 16#2001, IP8 == Hex] of
			[IPv6] ->
			    {IPv4,IPv6};
			[] ->
			    {IPv4,[]}
		    end;
		_ -> error
	    end;
	_ ->
	    error
    end.

%%===========================================================
%% Make a unique alias by appending module name to alias
%%===========================================================
make_name_module(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(?MODULE)).


%%===========================================================
%% Returns an extended list of addresses. The addresses given
%% by the 2nd argument are appended to the list of addresses
%% given as the first argument, except for addresses that
%% would be duplicates.
%%===========================================================

-type ipV4Address() :: {non_neg_integer(),
			non_neg_integer(),
			non_neg_integer(),
			non_neg_integer()}.

-spec addAddresses([ipV4Address()], [ipV4Address()]) ->
	  [ipV4Address()].

addAddresses(Addrs, ExtraAddrs) ->
    addAddresses(Addrs, ExtraAddrs, Addrs).


addAddresses(Addrs, [Addr|More], Acc) ->
    case lists:member(Addr, Addrs) of
	true ->
	    addAddresses(Addrs, More, Acc);
	false ->
	    addAddresses(Addrs, More, Acc++[Addr])
    end;

addAddresses(_Addrs, [], Acc) ->
    Acc.

get_sim_sname_du(User) ->
    case init:get_argument(mp) of
	{ok, [[DU]]} ->
	    DU ++ "_" ++ User;
	_Other ->
	    User
    end.

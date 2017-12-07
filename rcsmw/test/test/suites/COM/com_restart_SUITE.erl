%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	com_restart_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R8A/R11A/2
-module(com_restart_SUITE).
%%% ----------------------------------------------------------
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
%%% R2A/1      2013-08-12 etxpejn     Created
%%% R2A/2      2013-08-27 etxpejn     Updates
%%% R2A/3      2013-09-04 etxpejn     Added group com_test
%%% R2A/4      2013-09-05 etxpejn     Removed COLI check and added check_interfaces
%%%                                   and netconf to kill_pid_with_dump
%%% R2A/6      2013-09-16 etxivri     Added reboot as a prcondition in com_kill_pids TC.
%%% R2A/7      2013-10-18 etxivri     Added a check to make sure node restarts.
%%% R2A/8      2013-11-19 etxivri     Update 
%%%                                   TC com_kill_pid_with_dump_restart_node  
%%%                                   due to the restarts kill -6 shall be 
%%%                                   within 5min to trigger node restart.
%%%                                   Also added delete snmp config.
%%% R2A/9      2014-01-28 etxivri     Added allowed trap, 
%%%                                   "License Management, Key File Fault".
%%%                                   Replaced reboot with reset restart list 
%%%                                   in Appm, as precondition.
%%% R2A/10     2014-02-06 etxivri     Update to filter out a new ERROR REPORT.
%%% R2A/11     2014-02-26 etxivri     Changed order in all to ensure kill com 6times
%%%                                   within 5min results in node restart.
%%%                                   Also added one allowed_trap.
%%% R2A/12     2014-02-28 etxivri     Added a allowed trap when clear alarm.
%%% R2A/12     2014-03-19 etxivri     Filter out an new ERROR. Update allowed traps.
%%% R2A/14     2014-03-27 etxivri     Update error log filter
%%% R2A/15     2014-04-24 etxivri     Update allowed trap and increased time before send traps. 
%%% R2A/16     2014-04-25 etxivri     Update due to COM max_restarts changed to 3.
%%% R2A/17     2014-05-09 etxivri     Update in allowed traps.
%%% R2A/18     2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R2A/19     2014-07-01 etxivri     Changed chek of restart string.
%%%                                   Increase timeout before check new com pid
%%% R2A/20     2014-08-26 etxivri     Changed use of alarm.
%%% R2A/21     2014-09-17 etxivri     Update in allowed traps.
%%% R3A/1      2014-12-11 etxivri     Update due to beam core generates when node retarts.
%%% R3A/2      2014-12-18 etxivri     Update cli check.
%%% R3A/3      2015-01-30 etxivri     Update allowed traps.
%%% R3A/4      2015-01-30 etxivri     A try to make it more robust.
%%% R4A/1      2015-07-07 etxivri     Update allowed traps.
%%% R4A/2      2015-09-24 etxivri     Update check str for node to restart.
%%% R4A/3      2015-09-30 etxivri     Add cth_conn_log hook.
%%% R4A/4      2015-10-30 etxivri     Update to make it more robust.
%%% R5A/1      2016-05-13 etxnnor     Update to make it more robust.
%%% R5A/2      2016-05-13 etxnnor     Deliverytool didn't discover my changes, trying again
%%% R8A/1      2016-12-14 etxivri     Update to make it more robust.
%%% R8A/2      2016-12-29 etxivri     Update to make it more robust.
%%% R8A/2      2016-12-29 etxivri     Ignore erl err if event failed due to com goes down.
%%% R11A/1     2017-10-16 etxarnu     Removed export_all
%%% R11A/2     2017-10-17 etxivri     Fixed compile error
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%-compile(export_all).

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0,
	 deconfig_snmpmgr/1,
	 com_kill_pids/1,
	 com_kill_pid_with_dump/1,
	 com_kill_pid_with_dump_restart_node/1
	]).

%% A new test should be added that changes the 5 times in 5 minutes values of COMTE
%% in runtime, should look like something like this: 
%% rct_rpc:call(rpc, application, set_env, [comte, max_restarts_time_limit, 420], 500, noprint)

-define(Number_of_restarts, 4).
-define(IGNORELIST , ["Unexpected COM\\[",
		     "** Generic server comsaEvent terminating",
		     %%"mfa: {comte,notify",
		     "gmfDataInit,prep_stop",
		     "application_master: shutdown_error"]).
%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_snmpmgr, rct_rpc, rct_netconf,  
%% rct_cli, rct_logging and rct_core.
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {cth_conn_log, []},
                 {rct_power,node},
		 {rct_snmpmgr,snmp1},
		 {rct_rpc, rpc},
		 {rct_netconf, nc1},
		 {rct_cli, {cli, [manual_connect]}},
		 %% "application_master: " should be removed when fix from GMF is recieved.
		 {rct_logging, {all, [{erlang,{["ERROR REPORT"],[?IGNORELIST]}}]}},
		 {rct_core,[]}
		]}].


%% @hidden
init_per_suite(Config) -> 
    Config.
%% @hidden
end_per_suite(_Config) ->
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    ok.

%% @hidden
init_per_testcase(com_kill_pid_with_dump, Config) ->
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    {skip,"At the moment it is not possible to collect dumps in the simulated enviroment"};
	"target" ->
	    Config
    end;
init_per_testcase(com_kill_pid_with_dump_restart_node, Config) ->
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    {skip,"At the moment it is not possible to collect dumps in the simulated enviroment"};
	"target" ->
	    Config
    end;
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------

all() ->
    [{group, com_test}].

groups()->
    [{com_test, [sequence],
      [
       com_kill_pids,
       com_kill_pid_with_dump_restart_node,
       com_kill_pid_with_dump,
       %% com_kill_pid_with_dump_restart_node,
       deconfig_snmpmgr
      ]}].


ignore() ->
    Local_Ignorelist = ["notify_failed"],
    %% rct_logging:change_logging_parameters_in_this_tc({filter, [{all, {["ERROR REPORT"], ?IGNORELIST ++ Local_Ignorelist }}]}).
    rct_logging:change_logging_parameters_in_this_tc({filter, [{erlang, {["ERROR REPORT"], ?IGNORELIST ++ Local_Ignorelist }}]}).

%%--------------------------------------------------------------------
%% @doc 
%% Kill linux COM process, verify that it is restarted and verify netconf, CLI and SNMP<br/><br/>
%% @end
%%--------------------------------------------------------------------
com_kill_pids(_) ->
    ignore(),
    rct_core:coredump_test(),
    %% Precondition. Reset restart list in APPM.
    ct:pal("### Clear appmServer reset_restart_list, as a precondition.!!",[]),
    ok = rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    kill_pids(?Number_of_restarts, no_dumps).

kill_pids(0, _Dumps) ->
    ok;
kill_pids(NumberOfRestarts, Dumps) ->
    ct:pal("Killing COM pid but no dumps should be generated. "
	   "No of times the PID should be restarted: ~p", [NumberOfRestarts]),
    Beam = rct_rpc:call(rpc, os, getpid, [], 5000, noprint),
    ComPid = find_com_pid(),
    %% kill_com_pid(ComPid, Dumps),
    %% Update due to beam core when node restarts.
    case NumberOfRestarts of
	1 ->
	    NewDumps = dumps;
	_R ->
	    NewDumps = Dumps
    end,
    kill_com_pid(ComPid, NewDumps),

    case NumberOfRestarts of
	1 ->
	    %% 
	    %% After killing the COM process this time the hole node will restart instead of just the 
	    %% restarting the COM process and COMTE server.
	    %%
	    ct:pal("Here the node shall restart"),
	    case os:getenv("SIM_OR_TARGET") of
		"sim" ->
		    %% Check that the BEAM process is new as a sign that the node has restarted
		    ok = check_beam_pid(Beam);
		"target" ->
		    check_for_node_restart(),
		    wait_for_login_prompt()
		    %% ok = rct_rs232:login(console)
	    end,
	    ok = wait_for_appl_started();
	_Else ->
	    do_nada
    end,
    ComPid2 = find_new_com_pid(),
    case ComPid2 of
	ComPid -> 
	    ct:fail("Linux com process not restarted");
	_ -> 
	    ok
    end,
    try wait_for_netconf_started()
    catch
    	_:Reason ->
    	    ct_netconfc:close_session(nc1),
    	    ct:fail(Reason)
    end,
    check_interfaces(),
    kill_pids(NumberOfRestarts - 1, Dumps).


check_beam_pid(Beam) ->
    check_beam_pid(120000, Beam).

check_beam_pid(Timeout, _Beam) when Timeout < 500 ->
    ct:fail("No new BEAM PID within max timeout after restart.");
check_beam_pid(Timeout, Beam) ->
    NewBeam = rct_rpc:call(rpc, os, getpid, [], 5000, noprint),
    case Beam of
	NewBeam ->
	    timer:sleep(250),
	    check_beam_pid(Timeout - 250, Beam);
	_Else ->
	    ok
    end.


%%--------------------------------------------------------------------
%% @doc 
%% Kill linux COM process, verify that it is restarted, a dump is created and netconf, CLI and SNMP.<br/><br/>
%% @end
%%--------------------------------------------------------------------
com_kill_pid_with_dump(_) ->
    ignore(),
    rct_core:coredump_test(),
    kill_pid_with_dump(1, dumps).

kill_pid_with_dump(0, _Dumps) ->
    ok;
kill_pid_with_dump(NumberOfRestarts, Dumps) ->
    ct:pal("Killing COM pid expecting dumps"),
    ComPid = find_com_pid(),
    kill_com_pid(ComPid, Dumps),

    timer:sleep(20000),
    ComPid2 = find_new_com_pid(),
    case ComPid2 of
	ComPid -> 
	    ct:fail("Linux com process not restarted");
	_ -> 
	    ok
    end,
 
    try wait_for_netconf_started()
    catch
    	_:Reason ->
    	    ct_netconfc:close_session(nc1),
    	    ct:fail(Reason)
    end,
    check_interfaces(),
    kill_pids(NumberOfRestarts - 1, Dumps).

%%--------------------------------------------------------------------
%% @doc 
%% Kill linux COM process, verify that both the node and the COM process is restarted, a dump is created and that netconf,
%% CLI and SNMP workes after the restart.<br/><br/>
%% @end
%%--------------------------------------------------------------------
com_kill_pid_with_dump_restart_node(_) ->
    ct:pal("sleep 2 min to make tc more robust"),
    timer:sleep(120000), %% To make it more robust.
    ignore(),
    rct_core:coredump_test(),
    kill_pid_with_dump_and_restart().

kill_pid_with_dump_and_restart() ->
    ct:pal("Killing COM pid expecting dumps and node restart"),
    %% com_kill_pid_with_dump2(4, dumps),
    com_kill_pid_with_dump2(3, dumps),

    ComPid = find_com_pid(),
    kill_com_pid(ComPid, dumps),
    %% 
    %% After killing the COM process this time the hole node will restart instead of just the 
    %% restarting the COM process and COMTE server.
    %%
    ct:pal("Here the node shall restart"),
    wait_for_login_prompt(),
    ok = wait_for_appl_started(),
    ok = rct_rs232:login(console),
    
    try wait_for_netconf_started()
    catch
    	_:Reason ->
    	    ct_netconfc:close_session(nc1),
    	    ct:fail(Reason)
    end,
    
    ComPid2 = find_new_com_pid(),
    case ComPid2 of
	ComPid -> 
	    ct:fail("Linux com process not restarted");
	_ -> 
	    ok
    end,
    ok = check_interfaces().


com_kill_pid_with_dump2(0, _Dumps) ->
    ok;
com_kill_pid_with_dump2(NumberOfRestarts, Dumps) ->
    ct:pal("Nr of remaining kill before node shall restart: ~p.", 
	   [NumberOfRestarts]),
    ct:pal("Killing COM pid expecting dumps"),
    ComPid = find_com_pid(),
    kill_com_pid(ComPid, Dumps),

    timer:sleep(20000),
    ComPid2 = find_new_com_pid(),
    case ComPid2 of
	ComPid -> 
	    ct:fail("Linux com process not restarted");
	_ -> 
	    ok
    end,
 
    try wait_for_netconf_started()
    catch
    	_:Reason ->
    	    ct_netconfc:close_session(nc1),
    	    ct:fail(Reason)
    end,
    check_interfaces(),
    com_kill_pid_with_dump2(NumberOfRestarts - 1, Dumps).


%%% Internal functions
%%--------------------------------------------------------------------
%% @doc 
%% Return the COM process on the node<br/>
%% @end
%%--------------------------------------------------------------------
find_com_pid() ->
    ComPid = 
	case os:getenv("SIM_OR_TARGET") of
	    "sim" ->
		Pid = string:strip(os:cmd("pgrep -f com/bin/com -u $USER")),
		ct:pal("Cmd: \"pgrep -f com/bin/com -u $USER\"~nResult: ~p",[Pid]), 
		Pid;
	    "target" ->
		ok = rct_rs232:login(console),
		{ok, [_, Pid, _]} = ct_telnet:cmd(console,"pgrep -f com/bin/com"),
		Pid
	end,
    {match,_} = re:run(ComPid,"^[0-9]+$",[]),
    ComPid.

%%--------------------------------------------------------------------
%% @doc 
%% Kill the COM process, either with or without dump generated.<br/>
%% @end
%%--------------------------------------------------------------------
kill_com_pid(Pid, dumps) ->
    ct:pal("Cmd: \"kill -6 " ++ Pid ++ "\""),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    os:cmd("kill -6 " ++ Pid);
	"target" ->
	    ct_telnet:cmd(console,"kill -6 " ++ Pid)
    end;
kill_com_pid(Pid, no_dumps) ->
    ct:pal("Cmd: \"kill -9 " ++ Pid ++ "\""),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    os:cmd("kill -9 " ++ Pid);
	"target" ->
	    ct_telnet:cmd(console,"kill -9 " ++ Pid)
    end.

%% ===========================================================================
%% @doc
%% Check that node restats. <br/>
%% Wait for a specific string arrives in the consol printouts. <br/>
%% @spec check_for_node_restart() -> ok
%% @end
%% ===========================================================================
check_for_node_restart() ->
    ct:pal("### Check that node restarts.", []),
    %% Check that node has restarted.
    BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    RestartStr = case BoardType of
		     BoardType when BoardType == "dus4101";
				    BoardType == "duw4101" ->
			 "2:nd Stage Boot Loader";
		     BoardType -> % tcu03, dus5201, dus3201
			 "Ericsson Version:"
		 end,
    ok = check_node_is_starting(RestartStr).

check_node_is_starting(RestartStr)->
    ct:log("RestartStr: ~p", [RestartStr]),
    case ct_telnet:expect(console, RestartStr, [{timeout,120000}, no_prompt_check]) of
	{ok, _} ->
	    ok;
	Res ->
	    ct:pal("### Res: ~w", [Res]),
	    ct:fail("Node has not restarted within expected time!.")
    end.

%% ===========================================================================
%% @doc
%% Check for login prompt. <br/>
%% Wait for login prompt arrives in the consol printouts. <br/>
%% @spec wait_for_login_prompt() -> ok
%% @end
%% ===========================================================================
wait_for_login_prompt() ->
    wait_for_login_prompt(100000).

wait_for_login_prompt(Timeout) when Timeout < 500 ->
    ct:fail("No login prompt within max timeout after restart.");
wait_for_login_prompt(Timeout) ->
    case ct_telnet:expect(console, "login:", [{timeout,250}, no_prompt_check]) of
	{ok, _} -> 
	    ok;
	_  ->
	    wait_for_login_prompt(Timeout - 250)
    end.

%% ===========================================================================
%% @doc
%% Check for COM started. <br/>
%% Wait for COM process to be started. <br/>
%% @spec find_new_com_pid() -> Data
%% @end
%% ===========================================================================
find_new_com_pid() ->
    ok = rct_rs232:login(console),
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    find_com_pid(360000, "target");
	"sim" ->
	    find_com_pid(150000, "sim")
    end.

find_com_pid(Timeout, _Env) when Timeout < 200 ->
    ct:fail("COM not started within max timeout after restart.");
find_com_pid(Timeout, "sim") ->
    Pid = string:strip(os:cmd("pgrep -f com/bin/com -u $USER")),
    case Pid of
	[] ->
	   timer:sleep(200),
	    find_com_pid(Timeout - 200,"sim");
	_Else ->
	    Pid
    end;
find_com_pid(Timeout, "target") ->
    case ct_telnet:cmd(console,"pgrep -f com/bin/com") of
   	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(5000),
	    find_com_pid(Timeout - 5000, "target");
	[] ->	    
	    timer:sleep(5000),
	    find_com_pid(Timeout - 5000, "target");
	{ok,[_,_,"du1 login: "]} ->
	    timer:sleep(5000),
	    find_com_pid(Timeout - 5000, "target");
	{ok, [_, Pid, _]} ->
	    {match,_} = re:run(Pid,"^[0-9]+$",[]),
	    Pid;
	_Else ->
	    timer:sleep(5000),
	    find_com_pid(Timeout - 5000, "target")
    end.

%% ===========================================================================
%% @doc
%% Check for Application to be started. <br/>
%% Wait for test_app is started. <br/>
%% @spec wait_for_appl_started() -> ok
%% @end
%% ===========================================================================
wait_for_appl_started() ->
    wait_for_appl_started(120000).

wait_for_appl_started(Timeout) when Timeout < 300 ->
    ct:fail("No Appl started within max timeout after restart.");
wait_for_appl_started(Timeout) ->
    case rct_rpc:call(rpc, appmServer, get_apps, [], 300, noprint) of
    	{badrpc, _} ->
	    timer:sleep(300),
	    wait_for_appl_started(Timeout - 300);
	[] -> 
	    timer:sleep(300),
	    wait_for_appl_started(Timeout - 300);
	AppProplist ->
	    ct:pal("Apps: ~p ",[AppProplist]),
	    ok
    end.

%% ===========================================================================
%% @doc
%% Check for Netconf to be started. <br/>
%% Wait for ct_netconfc:get_config... returns  ok. <br/>
%% @spec wait_for_netconf_started() -> ok
%% @end
%% ===========================================================================
wait_for_netconf_started() ->
    wait_for_netconf_started(60000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");
wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    {ok,_} = ct_netconfc:get_config(nc1,running,{'ManagedElement',
							 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							 [{managedElementId,[],["1"]}]}),
	    ok = ct_netconfc:close_session(nc1);
	_  ->
	    timer:sleep(150),
	    wait_for_netconf_started(Timeout - 150)
    end.

%% ===========================================================================
%% @doc
%% Check for CLI and SNMP to be started. <br/>
%% @spec check_interfaces() -> ok
%% @end
%% ===========================================================================
check_interfaces() ->
    ct:pal("Start check_interfaces"),
    ok = rct_cli:connect(cli),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,SwM=1"),
    ok = rct_cli:disconnect(cli),
    snmp().

%% check_interfaces2() ->
%%     ct:pal("Start check_interfaces"),
%%     ok = rct_cli:connect(cli),
%%     {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,SwM=1"),
%%     ok = rct_cli:disconnect(cli).

%%--------------------------------------------------------------------
%% @doc 
%% Configure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
snmp() ->
    {{mgr_ip,MgrIp}, 
     {mgr_port,MgrPort}, 
     {agent_ip,AgentIp}, 
     {agent_port,AgentPort}} = rct_snmpmgr:get_mgr_data(snmp1),
    F = fun() -> 
		{ok,_} = ct_netconfc:open(nc1,[]),
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
    timer:sleep(10000), % Seems necessary before start sending traps          
    
    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmMajor},
				      {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass1=1"},
				      {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
				      {eriAlarmNObjAdditionalText,"Coffee beans refill required"}]],
				    [{allowed_traps,[
						     [{type,eriAlarmHeartBeatNotif}], 
						     [{type,eriAlarmCritical}, {eriAlarmActiveSpecificProblem,
										"License Key File Fault"},
						      {eriAlarmActiveManagedObject,
						       "ManagedElement=1,SystemFunctions=1,Lm=1"}],
						     [{type,eriAlarmMinor}, {eriAlarmActiveSpecificProblem,
									     "All NTP Servers Unreachable"},
						      {eriAlarmActiveManagedObject,
						       "ManagedElement=1,SystemFunctions=1,SysM=1"}],
						     [{type,eriAlarmCleared}, {eriAlarmActiveSpecificProblem,
									       "All NTP Servers Unreachable"},
						      {eriAlarmActiveManagedObject,
						       "ManagedElement=1,SystemFunctions=1,SysM=1"}],
						     [{type,nsNotifyShutdown}],
						     %% "Calendar Clock All NTP Servers Unavailable"
						     [{eriAlarmActiveMinorType,9175058}],
						     %% Calendar Clock NTP Server Unavailable
						     [{eriAlarmActiveMinorType,9175059}],
						     [{type,eriAlarmAlarmListRebuilt}]
						    ]}],
				    180),

    ok = rct_rpc:call(rpc, comsaI, send_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', 
					       major, [<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass1=1">>], 
					       "Coffee beans refill required"],20000),


    timer:sleep(5000), %% Alarm takes two sec before acktive.
    ok = rct_snmpmgr:check_traps(),

    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
				      {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass1=1"},
				      {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
				      {eriAlarmNObjAdditionalText,"Coffee beans refill required"}]],
				    [{allowed_traps,[
						     [{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmCleared}, {eriAlarmActiveSpecificProblem,
									       "All NTP Servers Unreachable"},
						      {eriAlarmActiveManagedObject,
						       "ManagedElement=1,SystemFunctions=1,SysM=1"}],
						     [{type,eriAlarmCritical}, {eriAlarmActiveSpecificProblem,
										"License Key File Fault"},
						      {eriAlarmActiveManagedObject,
						       "ManagedElement=1,SystemFunctions=1,Lm=1"}],
						     [{type,eriAlarmMinor}, {eriAlarmActiveSpecificProblem,
									     "All NTP Servers Unreachable"},
						      {eriAlarmActiveManagedObject,
						       "ManagedElement=1,SystemFunctions=1,SysM=1"}],
						     [{type,nsNotifyShutdown}],
						     %% "Calendar Clock All NTP Servers Unavailable"
						     [{eriAlarmActiveMinorType,9175058}],
						     %% Calendar Clock NTP Server Unavailable
						     [{eriAlarmActiveMinorType,9175059}],
						     [{type,eriAlarmAlarmListRebuilt}]
						    ]}],
				    180),

    ok = rct_rpc:call(rpc, comsaI, clear_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', 
						[<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass1=1">>]],20000),

    ok = rct_snmpmgr:check_traps().


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
		      	      ok = 
				  ct_netconfc:
				  edit_config(nc1,running,
					      {'ManagedElement',
					       [{xmlns,"urn:com:ericsson:ecim:"
						 "ComTop"}],
					       [{managedElementId,[],["1"]},
						{'SystemFunctions',
						 [{systemFunctionsId,[],["1"]},
						  {'SysM',
						   [{xmlns,"urn:com:ericsson:"
						     "ecim:ComSysM"}],
						   [{sysMId,[],["1"]},
						    {'Snmp',
						     [{xmlns,"urn:com:ericsson:"
						       "ecim:ComSnmp"}],
						     [{snmpId,[],["1"]},
						      {'SnmpTargetV2C',
						       [{xmlns,"urn:com:"
							 "ericsson:ecim:"
							 "ComSnmp"},
							{'xmlns:nc',"urn:ietf:"
							 "params:xml:ns:"
							 "netconf:base:1.0"},
							{'nc:operation',
							 "delete"}],
						       [{snmpTargetV2CId,[],
							 ["1"]}]}]
						    }]}]}]});
			  "target" ->       
		      	      ok = 
				  ct_netconfc:
				  edit_config(nc1,running,
					      {'ManagedElement',
					       [{xmlns,"urn:com:ericsson:ecim:"
						 "ComTop"}],
					       [{managedElementId,[],["1"]},
						{'SystemFunctions',
						 [{systemFunctionsId,[],["1"]},
						  {'SysM',
						   [{xmlns,"urn:com:ericsson:"
						     "ecim:ComSysM"}],
						   [{sysMId,[],["1"]},
						    {'Snmp',
						     [{xmlns,"urn:com:ericsson:"
						       "ecim:ComSnmp"}],
						     [{snmpId,[],["1"]},
						      {agentAddress, 
						       [{xmlns,
							 "urn:com:ericsson:"
							 "ecim:ComSnmp"},
							{'xmlns:nc',
							 "urn:ietf:params:xml:"
							 "ns:netconf:base:1.0"},
							{'nc:operation',
							 "delete"}],[]},
						      {'SnmpTargetV2C',
						       [{xmlns,
							 "urn:com:ericsson:"
							 "ecim:ComSnmp"},
							{'xmlns:nc',
							 "urn:ietf:params:"
							 "xml:ns:netconf:"
							 "base:1.0"},
							{'nc:operation',
							 "delete"}],
						       [{snmpTargetV2CId,[],
							 ["1"]}]}]
						    }]}]}]})
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

%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	rob_ntf_alarm_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/2
%%%
%%% @doc == Robustness Generate and cease Alarms using saf ntf, Check ecpected traps is recieved==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%% @end

-module(rob_ntf_alarm_SUITE).
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
%%% R2A/1      2013-07-09 etxivri     Created
%%% R2A/3      2013-07-10 etxivri     Allowed traps to be recieved in any_order.
%%% R2A/4      2013-08-13 etxkols     Changed raw to no_prompt_check in
%%%                                   ct_telnet:expect
%%% R2A/5      2013-08-28 etxivri     Added new TC. stop/start com via comte,
%%%                                   check active alarm still exist.
%%%                                   - Kill COM when acive alarm exist,
%%%                                     Could not be runed for yet, due to
%%%                                     due to active alarm list is cleared.
%%% R2A/6      2013-08-28 etxivri     Corrected a fault in reboot.
%%% R2A/7      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/8      2013-09-10 etxivri     Fixed kill_com TC and added it to all.
%%% R2A/9      2013-09-19 etxivri     Minor fix, removed in
%%%                                   deconfig_snmpmgr_no_check() in cleanup.
%%% R2A/9      2013-11-18 etxivri     Upadates due to changed behaviour of Alarm
%%%                                   handling. Alarm need to be generated or
%%%                                   ceased 2sec. If alarm been togling 3times
%%%                                   within 60sec, then Alarm will be active
%%%                                   as togled alarm.
%%%                                   (can be ceased after 180sec)
%%% R2A/11      2013-11-18 etxivri   Forgott to remove break in prev.
%%% R2A/12      2013-11-18 etxivri   Correct nr of alarams in a TC.
%%% R2A/12      2013-11-19 etxivri   Remowed check_active_alarmlist due to
%%%                                  Alarm could exist several sec after cease.
%%%                                  Then no value to use this check.
%%% R2A/14      2013-11-25 etxivri   Update to handle, if active alarms exist
%%%                                  when TC starts.
%%% R2A/15      2014-02--26 etxivri   Addet allowed_traps.
%%% R2A/15      2014-03-19 etxivri   Update allowed_traps
%%% R2A/17      2014-05-09 etxivri   Update allowed_traps
%%% R2A/18      2014-06-04 etxivri    Changed check for "du1 login" prompt to
%%%                                   "login:"
%%% R2A/20      2014-06-04 etxivri   Update alarm check of expected alarms.
%%% R2A/21      2014-08-27 etxivri   Update to use another alarm.
%%%                                  Use one alarm from two MOs classes.
%%%                                  removed a tc.
%%% R2A/22      2014-09-03 etxivri   Removed deconfig snmpmgr if tc fails.
%%% R2A/23      2014-09-03 etxivri   Update due to differents behaviour between
%%%                                  boards using wr6 or wr5.
%%% R2A/24      2014-09-11 etxivri   Update allowed_traps.
%%% R2A/25      2014-09-17 etxivri   No need to conf snmp, due to it is done at
%%%                                  installation.
%%% R2A/27      2014-10-15 etxivri   Update due to new behaviour.
%%% R3A/1       2014-11-04 etxivri   Try to avoid ERROR when testnode goes down.
%%% R3A/2       2014-11-04 etxivri   Minor update.
%%% R3A/3       2015-01-30 etxivri   Update allowed traps.
%%% R3A/4       2015-01-30 erarafo   Adapted to alarm minorType change.
%%% R4A/1       2015-07-16 etxivri   Update to be more robust.
%%% R4A/2       2016-02-19 etxkols   Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_ntf.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 gen_cease_alarms_check/1,
	 gen_cease_alarms_10loop_check/1,
	 gen_alarms_reboot_check/1,
	 gen_alarms_kill_com_cease/1,
	 gen_not_existing_alarm/1
	]).

-define(NrOfCliUsers, 1). % Only one is needed to get active alarms.
-define(NrOfNetConfUsers, 1).
-define(TIME, 3000).  %% Time for Alarm to be generated or ceased.
-define(NrOfAlarms, "2").

-define(CLI_SessionNameList,
	[list_to_atom(
	   "cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(ClassNrList, ["1", "2"]).

-define(GenTrapList, [[{type,eriAlarmMajor},
		       {eriAlarmActiveManagedObject,
			"ManagedElement=1,TestRoot=1,TestClass1=1"},
		       {eriAlarmActiveSpecificProblem,
			"Resource Monitor Coffee Beans Container Low Level"},
		       {eriAlarmNObjAdditionalText,
			"Coffee beans refill required"}],
		      [{type,eriAlarmMajor},
		       {eriAlarmActiveManagedObject,
			"ManagedElement=1,TestRoot=1,TestClass2=1"},
		       {eriAlarmActiveSpecificProblem,
			"Resource Monitor Coffee Beans Container Low Level"},
		       {eriAlarmNObjAdditionalText,
			"Coffee beans refill required"}]
		     ]).

-define(ClearTrapList, [[{type,eriAlarmCleared},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,TestRoot=1,TestClass1=1"},
			 {eriAlarmActiveSpecificProblem,
			  "Resource Monitor Coffee Beans Container Low Level"},
			 {eriAlarmNObjAdditionalText,
			  "Coffee beans refill required"}],
			[{type,eriAlarmCleared},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,TestRoot=1,TestClass2=1"},
			 {eriAlarmActiveSpecificProblem,
			  "Resource Monitor Coffee Beans Container Low Level"},
			 {eriAlarmNObjAdditionalText,
			  "Coffee beans refill required"}]
		       ]).

-define(AllowedTraps,[any_order,
		      {allowed_traps,
		       [
			[{type,eriAlarmCritical},
			 {eriAlarmActiveSpecificProblem,
			  "License Key File Fault"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,Lm=1"}],
			[{type,eriAlarmMinor},
			 {eriAlarmActiveSpecificProblem,
			  "All NTP Servers Unreachable"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,SysM=1"}],
			[{type,eriAlarmCleared},
			 {eriAlarmActiveSpecificProblem,
			  "All NTP Servers Unreachable"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,SysM=1"}],
			%% "Calendar Clock All NTP Servers Unavailable"
			[{eriAlarmActiveMinorType,9175058}],
			%% Calendar Clock NTP Server Unavailable
			[{eriAlarmActiveMinorType,9175059}],

			%% [{type,eriAlarmMinor},
			%%  {eriAlarmActiveSpecificProblem,
			%%   "Calendar Clock All NTP Servers Unavailable"},
			%%  {eriAlarmActiveManagedObject,
			%%   "ManagedElement=1,SystemFunctions=1,SysM=1"}],
			%% [{type,eriAlarmCleared},
			%%  {eriAlarmActiveSpecificProblem,
			%%   "Calendar Clock NTP Server Unavailable"},
			%%  {eriAlarmActiveManagedObject,
			%%   "ManagedElement=1,SystemFunctions=1,SysM=1,NtpServer=1"}],
			[{type,eriAlarmHeartBeatNotif}],
			[{type,nsNotifyShutdown}]
		       ]
		      }] ).

%% eriAlarmHeartBeatNotif not included
-define(AllowedTraps2,[any_order,
		       {allowed_traps,
		       [
			[{type,eriAlarmCritical},
			 {eriAlarmActiveSpecificProblem,
			  "License Key File Fault"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,Lm=1"}],
			[{type,eriAlarmMinor},
			 {eriAlarmActiveSpecificProblem,
			  "All NTP Servers Unreachable"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,SysM=1"}],
			[{type,eriAlarmCleared},
			 {eriAlarmActiveSpecificProblem,
			  "All NTP Servers Unreachable"},
			 {eriAlarmActiveManagedObject,
			  "ManagedElement=1,SystemFunctions=1,SysM=1"}],
			%% "Calendar Clock All NTP Servers Unavailable"
			[{eriAlarmActiveMinorType,9175058}],
			%% Calendar Clock NTP Server Unavailable
			[{eriAlarmActiveMinorType,9175059}]
		       ]
		      }] ).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, <br/>
%% to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of CliHooks touples with differents Names.
    NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    CliHooks =[{rct_cli, {list_to_atom("cli"++integer_to_list(N)),
			  [manual_connect]}} || N <- lists:seq(1,
							       ?NrOfCliUsers)] ,

    %% ct:pal("CliHooks: ~p",[CliHooks]),
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 {rct_safe_ntf_rpc,[{safe_debug_level, 0},
				    {finalize_on_fail, true}]},
		 {rct_scp, [{1, node1}]},
		 {rct_core,[]},
		 {rct_snmpmgr,snmp1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       ["Unexpected COM\\[",
						"application_master: "
						"shutdown_error"]}}]}} |
		 lists:append(NetconfHooks, CliHooks)
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
init_per_testcase(gen_alarms_reboot_check, Config) ->
    Config;
init_per_testcase(_TestCase, Config) ->
    Handle = initialize_ntf(),
    ct:pal("### Handle: ~p",[Handle]),
    %% Config.
    [{handle, Handle}| Config].

%% @hidden
%% this tc will create Handle and finalize in TC otherwise end_per_testcase will be a warning.
end_per_testcase(gen_alarms_reboot_check, Config) ->
    check_result(Config),
    ok;
end_per_testcase(_TestCase, Config) ->
    %%%%
    %% Finalize NTF Handle
    %%%%
    Handle = proplists:get_value(handle, Config),
    ct:pal("### Handle: ~p",[Handle]),
    ct:pal("Finalize NTF Handle ~p", [Handle]),
    ok = rct_safe_ntf_rpc:finalize(Handle),
    check_result(Config),
    ok.

%% @hidden
check_result(Config) ->
    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up.", [Reason]),
	    %% test_server:break("break"),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000, noprint),
	    B = rct_rpc:call(rpc, os, cmd, ["pgrep cli"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),

	    ct:pal("### Com: ~p",[A]),
	    ct:pal("### Cli: ~p",[B]),
   	    ct:pal("### BeamPid: ~p",[D]),

	    ct_netconfc:open(nc1,[]),
	    FM =
		ct_netconfc:get(nc1,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'SystemFunctions',
				       [{systemFunctionsId,
					 [],["1"]},{'Fm',
						    [{xmlns,"urn:com:ericsson:"
						      "ecim:ComFm"}],
						    [{fmId,[],["1"]}]}]}
				     ]}),
	    ct:pal("~p", [FM]),

	    %%%%%%%%%%%%%%
	    %% Clean up!
	    %%%%%%%%%%%%%%
	    ct_netconfc:close(nc1),

            %%%%
	    %% Cease alarm.
            %%%%
	    %% TestClass1
	    rct_rpc:call(rpc, comsaI, clear_alarm,
			 ['ResourceMonitorCoffeeBeansContainerLowLevel',
			  [<<"ManagedElement=1">>,
			   <<"TestRoot=1">>,
			   <<"TestClass1=1">>]],
			 20000),
	    %% TestClass2
	    rct_rpc:call(rpc, comsaI, clear_alarm,
			 ['ResourceMonitorCoffeeBeansContainerLowLevel',
			  [<<"ManagedElement=1">>,
			   <<"TestRoot=1">>,
			   <<"TestClass2=1">>]],
			 20000)

    end,

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() ->
    [gen_cease_alarms_10loop_check,
     gen_alarms_kill_com_cease,
     gen_alarms_reboot_check].

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
%%  Generate 2 alarms, cease the alarms. Check expected traps i recieved. <br/>
%% - Generate 2 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% - Cease 2 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% @spec gen_cease_alarms_check(Config) -> ok
%% @end
%%--------------------------------------------------------------------
gen_cease_alarms_check(Config) ->
    ct:pal("¤¤¤ generate ~p alarms, cease the alarms. "
	   "Check expected traps is recieved.", [?NrOfAlarms]),

    Handle = proplists:get_value(handle, Config),

    gen_cease_alarm_wait_for_traps(Handle),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% generate 2 alarms, cease the alarms. Check expected traps i recieved. <br/>
%% Repeat 10 times. <br/>
%% - Generate 2 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% - Cease 2 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% - Repeat this 10 times. <br/>
%% @spec gen_cease_alarms_10loop_check(Config) -> ok
%% @end
%%--------------------------------------------------------------------
gen_cease_alarms_10loop_check(Config) ->
    ct:pal("¤¤¤ generate 2 alarms, cease the alarms. "
	   "Check traps is recieved. Repeat 10 times.", []),

    timer:sleep(60000),

    Handle = proplists:get_value(handle, Config),

    NrList = lists:seq(1, 10),
    %% NrList = lists:seq(1, 2),
    lists:foreach(fun(Nr) ->
			  ct:pal("### Loop Nr: ~p",[Nr]),
			  gen_cease_alarm_wait_for_traps(Handle),
			  timer:sleep(30000) %% Wait to avoid toggling alarm

		  end, NrList),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Reboot, generate 2 alarms, cease the alarms. <br/>
%% Check expected traps i recieved.<br/>
%% - Generate 2 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% - reboot. <br/>
%% - Check expected traps is received.<br/>
%% @spec gen_alarms_reboot_check(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
gen_alarms_reboot_check(_Config) ->
    ct:pal("¤¤¤ Generate 2 alarms, reboot. "
	   "Check expected traps i recieved.", []),

    Handle = initialize_ntf(),
    ct:pal("### Handle: ~p",[Handle]),

    try

        %%%%
	%% Set expextected recieved traps when generate alarms
        %%%%
	ct:pal("### Expected TrapList: ~p",[?GenTrapList]),
	set_wait_for_traps(?GenTrapList),

        %%%%
	%% Generate alarm.
        %%%%
	ct:pal("Generate alarms.",[]),
	_HeaderList = generate_alarms(Handle, ?ClassNrList),
	timer:sleep(?TIME),

        %%%%
	%% Check trap is recieved.
        %%%%
	ok = rct_snmpmgr:check_traps(),

        %%%%
	%% Set expextected recieved traps.
        %%%%
	ct:pal("### Expected Trap:",[]),
	ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmAlarmListRebuilt}],
					 [{type,eriAlarmHeartBeatNotif}]],
					?AllowedTraps2, 240)
    catch
	_:Reason -> %% Case TC fail, then delete NTF handle
	    ct:pal("Finalize NTF Handle ~p", [Handle]),
	    ok = rct_safe_ntf_rpc:finalize(Handle),

	    ct:fail(Reason)
    end,

    %%%%
    %% Reboot node
    %%%%
    reboot_node(),

    %%%%
    %% Check trap is recieved.
    %%%%
    ok = rct_snmpmgr:check_traps(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Generate 2 alarms, kill COM, cease the alarms. <br/>
%% Check expected traps i recieved.<br/>
%% - Generate 2 Alarms. <br/>
%% - Check expected traps is received.<br/>
%% - kill -9 COM. <br/>
%% - Check that Alarm traps is not received again.<br/>
%% - Cease the alarms. <br/>
%% - Check expected traps is received.<br/>
%% @spec gen_alarms_kill_com_cease(Config) -> ok
%% @end
%%--------------------------------------------------------------------
gen_alarms_kill_com_cease(Config) ->
    ct:pal("¤¤¤ Generate 2 alarms, kill COM, cease the alarms. "
	   "Check expected traps i recieved.", []),

    Handle = proplists:get_value(handle, Config),

    Com = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000),
    [ComPid|_] = string:tokens(Com, "\n "),
    BoardType =
	proplists:get_value(board_type,
			    ct:get_config(
			      ct:get_config({test_nodes,1}))),

    %%%%
    %% Set expextected recieved traps when generate alarms
    %%%%
    ct:pal("### Expected TrapList: ~p",[?GenTrapList]),
    set_wait_for_traps(?GenTrapList),

    %%%%
    %% Generate alarm.
    %%%%
    ct:pal("Generate alarms.",[]),
    HeaderList = generate_alarms(Handle, ?ClassNrList),
    timer:sleep(?TIME),

    %%%%
    %% Check trap is recieved.
    %%%%
    ok = rct_snmpmgr:check_traps(),

    %%%%
    %% Set expextected recieved traps.
    %%%%
    case BoardType of
	"tcu03" -> %% wr6
	    ct:pal("Check that only nsNotifyShutdown and eriAlarmHeartBeatNotif"
		   " trap is recieved.",[]),
	    ok = rct_snmpmgr:wait_for_traps([ [{type,nsNotifyShutdown}],
					      [{type,eriAlarmAlarmListRebuilt}],
					      [{type,eriAlarmHeartBeatNotif}] ],
					    ?AllowedTraps2, 120);
	_OtherBoards ->
	    ct:pal("Check that only eriAlarmHeartBeatNotif"
		   " trap is recieved.",[]),
	    ok = rct_snmpmgr:wait_for_traps([ [{type,eriAlarmAlarmListRebuilt}],
					      [{type,eriAlarmHeartBeatNotif}] ],
					    ?AllowedTraps, 120)
    end,

    ct:pal("ComPid: ~p .",[ComPid]),
    %%%%
    %% Kill COM
    %%%%
    ct:pal("kill com.",[]),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ ComPid], 2000),
    NewComPid = wait_for_com_to_restart(ComPid),
    ct:pal("NewComPid: ~p",[NewComPid]),

    ok = rct_snmpmgr:check_traps(),

    timer:sleep(?TIME),

    %%%%
    %% Set expextected recieved traps when generate alarms
    %%%%
    ct:pal("wait for ceased traps",[]),
    set_wait_for_traps(?ClearTrapList),

    %%%%
    %% Cease alarm.
    %%%%
    ct:pal("After kill COM, Cease alarms.",[]),
    cease_alarms(Handle, HeaderList),
    timer:sleep(?TIME),

    %%%%
    %% Check trap is recieved.
    %%%%
    ok = rct_snmpmgr:check_traps(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Generate a non existing alarm. <br/>
%% Check that no unexpected traps i recieved.<br/>
%% - Generate 1 non existing alarm. use major_id = 101. <br/>
%% - Check no unexpected traps is received.<br/>
%% @spec gen_not_existing_alarm(Config) -> ok
%% @end
%%--------------------------------------------------------------------
gen_not_existing_alarm(Config) ->
    Handle = proplists:get_value(handle, Config),

    %%%%
    %% Check that no unexpected traps is generated.
    %%%%
    ct:pal("Check that only eriAlarmHeartBeatNotif trap is recieved.",[]),
    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmHeartBeatNotif}]],
				    ?AllowedTraps, 90),

    %%%%
    %% Generate alarm.
    %%%%
    ct:pal("Generate a not existing alarm.",[]),
    Header =
	#safe_ntf_notification_header{
	   event_type = ?SAFE_NTF_TYPE_ALARM,
	   notification_object =
	       "testClass1Id=1,TESTMOMtestRootId=1",
	   notification_class_id =
	       #safe_ntf_class_id{vendor_id = 193,
				  major_id = 101,  %% Faulty for this alarm
				  minor_id = 65519}, %% new coffe alarm
	   event_time = ?SAFE_TIME_UNKNOWN},

    Notification =
	#safe_ntf_alarm_notification{
	   notification_header = Header,
	   probable_cause = ?SAFE_NTF_UNSPECIFIED_REASON,
	   perceived_severity = ?SAFE_NTF_SEVERITY_MAJOR},

    {ok, _Id } = rct_safe_ntf_rpc:notification_send(Handle,
						    Notification),

    timer:sleep(?TIME),

    ok = rct_snmpmgr:check_traps(),

    timer:sleep(?TIME),

    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
initialize_ntf() ->
    %%%%
    %% Get Beam Pid.
    %%%%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    ct:pal("BeamPid: ~p .",[BeamPid]),

    %%%%
    %% Get testnode Beam Pids.
    %%%%
    {ok, TestNode} = rct_safe_ntf_rpc:get_testnode(rct_safe_ntf_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 10000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    %%%%
    %% Initialize NTF Handle
    %%%%
    ct:pal("Initialize NTF Handle", []),
    {ok, Handle, _} = rct_safe_ntf_rpc:initialize(undefined),
    %% ct:pal("### Handle: ~p",[Handle]),
    Handle.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
gen_cease_alarm_wait_for_traps(Handle) ->
        %%%%
	%% Set expextected recieved traps when generate alarms
        %%%%
	ct:pal("### Expected TrapList: ~p",[?GenTrapList]),

        set_wait_for_traps(?GenTrapList),

        %%%%
	%% Generate alarm.
        %%%%
	ct:pal("Generate alarms.",[]),
	HeaderList = generate_alarms(Handle, ?ClassNrList),
	timer:sleep(?TIME),
        %%%%
	%% Check trap is recieved.
        %%%%
	ok = rct_snmpmgr:check_traps(),

        %%%%
	%% Set expextected recieved traps when cease alarms
        %%%%
	ct:pal("### Expected ClearTrapList: ~p",[?ClearTrapList]),

        set_wait_for_traps(?ClearTrapList),

        %%%%
	%% Cease alarm.
        %%%%
	ct:pal("Cease alarms.",[]),
	cease_alarms(Handle, HeaderList),
	timer:sleep(?TIME),

        %%%%
	%% Check trap is recieved.
        %%%%
	ok = rct_snmpmgr:check_traps(),

    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
generate_alarms(Handle, ClassNrList) ->
    generate_alarms(Handle, ClassNrList, 0).
generate_alarms(Handle, ClassNrList, Sleep) ->
    HeaderList=
	lists:map(
	  fun(ClassNr) ->
		  Header =
		      #safe_ntf_notification_header
		      {event_type =
			   ?SAFE_NTF_TYPE_ALARM,
		       notification_object =
			   "testClass"++ClassNr++"Id=1,TESTMOMtestRootId=1",
		       notification_class_id =
			   #safe_ntf_class_id{vendor_id = 193,
					      major_id = 140,
					      minor_id = 65519}, %% new coffe alarm
		       event_time = ?SAFE_TIME_UNKNOWN},

		  Notification =
		      #safe_ntf_alarm_notification
		      {notification_header = Header,
		       probable_cause = ?SAFE_NTF_UNSPECIFIED_REASON,
		       perceived_severity = ?SAFE_NTF_SEVERITY_MAJOR},

		  {ok, _Id } =
		      rct_safe_ntf_rpc:notification_send(Handle,
							 Notification),
		  timer:sleep(Sleep),
		  Header
	  end, ClassNrList),
    HeaderList.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cease_alarms(Handle, HeaderList) ->
cease_alarms(Handle, HeaderList, 0).
cease_alarms(Handle, HeaderList, Sleep) ->
    lists:foreach(
      fun(Header) ->
	      NotificationClear =
		  #safe_ntf_alarm_notification
		  {notification_header = Header,
		   probable_cause = ?SAFE_NTF_UNSPECIFIED_REASON,
		   perceived_severity = ?SAFE_NTF_SEVERITY_CLEARED},
	      {ok, _Id2 } =
		  rct_safe_ntf_rpc:notification_send(Handle,
						     NotificationClear),
			  timer:sleep(Sleep)
		  end, HeaderList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% reboot the node.  <br/>
%% @spec reboot_node() -> ok
%% @end
%%--------------------------------------------------------------------
reboot_node() ->
    %%%%
    %% reboot
    %%%%
    {ok, TestNode} = rct_safe_ntf_rpc:get_testnode(rct_safe_ntf_rpc),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rs232:login(console),
    %% net_kernel:disconnect(TestNode),
    %% net_kernel:disconnect(ErlNode),

    Nodes = nodes(),
    ct:pal("# Nodes: ~p",[Nodes]),

    ct:pal("### reboot!",[]),
    ok = ct_telnet:send(console, "reboot"),
    net_kernel:disconnect(TestNode),
    net_kernel:disconnect(ErlNode),
    timer:sleep(5000),

    {ok,_} = ct_telnet:expect(console, "login:", [{timeout,60000},
						     no_prompt_check]),
    net_kernel:connect(ErlNode),

    %%%%
    %% Sleep to be sure that com is up and ready to be used.
    %%%%
    %% timer:sleep(35000).
    wait_for_com_to_be_ready().

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
wait_for_com_to_be_ready() ->
    wait_for_com_to_be_ready(120000).

wait_for_com_to_be_ready(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not ready to be used, within max timeout: 60sec.");

wait_for_com_to_be_ready(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    A = rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 10000,
			     noprint),
	    ct:pal("### Com: ~p",[A]),

	    ct_netconfc:close_session(nc1);
	_  ->
	    timer:sleep(500),
	    wait_for_com_to_be_ready(Timeout - 500)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% wait_for_com_to_restart(ComPid)
%%--------------------------------------------------------------------
wait_for_com_to_restart(ComPid)->
    wait_for_com_to_restart(ComPid, 60000).

wait_for_com_to_restart(_ComPid, Timeout) when Timeout < 3000 ->
    ct:fail("New COM process not started up within max timeout after restart.");

wait_for_com_to_restart(ComPid, Timeout)->
    NewComPid = wait_for_com_to_start(),
    %% ct:pal("NewComPid: ~p",[NewComPid]),
    case ComPid == NewComPid of
    	true ->
	    timer:sleep(2000),
	    wait_for_com_to_restart(ComPid, Timeout-2000);
    	false ->
	    timer:sleep(2000),
	    NewComPid
    end.

%%--------------------------------------------------------------------
%% @hidden
%% wait_for_com_to_start()
%%--------------------------------------------------------------------
wait_for_com_to_start() ->
    wait_for_com_to_start(90000).

wait_for_com_to_start(Timeout) when Timeout < 6000 ->
    ct:fail("COM not started within max timeout.");

wait_for_com_to_start(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 1000)  of
	[] ->
	    timer:sleep(5000),
	    wait_for_com_to_start(Timeout - 5000);
	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(5000),
	    wait_for_com_to_start(Timeout - 5000);
	Com ->
	    %% remove \n from the received list.
	    [ComPid|_] = string:tokens(Com, "\n "),
	    ComPid
    end.

%%--------------------------------------------------------------------
%% @hidden
%% set_wait_for_traps()
%%--------------------------------------------------------------------
set_wait_for_traps(TrapList) ->
    ok = rct_snmpmgr:wait_for_traps(TrapList,
				    ?AllowedTraps,
				    60).

%% set_wait_for_traps(TrapList) ->
%%     ok = rct_snmpmgr:wait_for_traps(TrapList,
%% 				    [any_order,
%% 				     {allowed_traps,
%% 				      [
%% 				       [{type,eriAlarmHeartBeatNotif}],
%% 					   [{type,eriAlarmCritical},
%% 					    {eriAlarmActiveSpecificProblem,
%% 					     "License Key File Fault"},
%% 					    {eriAlarmActiveManagedObject,
%% 					     "ManagedElement=1,SystemFunctions=1,Lm=1"}],
%% 				       [{type,eriAlarmMinor},
%% 					{eriAlarmActiveSpecificProblem,
%% 					 "All NTP Servers Unreachable"},
%% 					{eriAlarmActiveManagedObject,
%% 					 "ManagedElement=1,SystemFunctions=1,SysM=1"}],
%% 				       [{type,eriAlarmCleared},
%% 					{eriAlarmActiveSpecificProblem,
%% 					 "All NTP Servers Unreachable"},
%% 					{eriAlarmActiveManagedObject,
%% 					 "ManagedElement=1,SystemFunctions=1,SysM=1"}],
%% 				       [{type,eriAlarmHeartBeatNotif}]
%% 				      ]}],
%% 				    60).

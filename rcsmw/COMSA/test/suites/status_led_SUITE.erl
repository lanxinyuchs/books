%%% %CCaseFile:	status_led_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R6A/R11A/2

%%% @doc ==Tests of Status LED==
%%% This test suite exercises the "Status LED" functionality
%%% implemented in WP 3722. The suite can be runs against target and
%%% simulator nodes. The rct_rpc proxy is used, which may make this
%%% suite unusable in a fully secure context.
%%%
%%% Version R3A/7 took 160 s to run on target (152 s on the simulator).
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% -----      -------    --------    ------------------------
%%% R3A/1      2014-11-17 erarafo     First version
%%% R3A/3      2014-11-18 erarafo     Fully functional version
%%% R3A/4      2014-11-19 erarafo     Refactoring
%%% R3A/5      2014-11-21 erarafo     More testcases; robustness
%%% R3A/6      2015-01-30 erarafo     Tweaked for provisional solution
%%% R3A/7      2015-02-02 erarafo     Added updates and alerts
%%% R3A/8      2015-02-16 erarafo     Test case added.
%%% R3A/9      2015-02-19 erarafo     Delays to avoid "toggling"
%%% R3A/10     2015-02-28 etxkols     Preparation for 2 labs
%%% R3A/11     2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-10-15 etxpejn     Add group for cluster sim
%%% R6A/1      2016-05-17 erarafo     Adjusted COM transient filter time
%%% R6A/2      2016-05-17 erarafo     Adjusted recognition of LKF alarm
%%% ----------------------------------------------------------


-module(status_led_SUITE).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R6A/R11A/2').
-date('2017-10-19').

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0]).

-export([test_red_path/1,
	 test_black_path/1,
	 test_blue_path/1,
	 test_green_path/1,
	 test_alarm_raise_clear/1,
	 test_alarm_raise_two_clear/1,
	 test_rpc/1,
	 test_cli/1,
	 test_reset_OutOfOrder/1,

	 do_open_door_clear/1

	 ]).

-include_lib("common_test/include/ct.hrl").

%% identifiers used by RCT hooks
-define(DU, du1).
-define(NODE, node1).
-define(C_VII, vii1).
-define(C_NTFI, ntfi1).
-define(NC, nc1).
-define(RCS, rcs1).
-define(CLI, cli1).

%% must match definitions in IFT
-define(NTFI, 11).
-define(NTFI_INITIALIZE, 1).
-define(NTFI_FINALIZE, 2).
-define(NTFI_ALARM, 3).

%% must match definition in master.h of ift_app
-define(VII, 4).

%% must match definitions in test_vii.c of ift_app
-define(Vii_visualIndGet,     2).

%% must match values in cello_vii.h
-define(CELLO_VII_FAILED, -1).
-define(CELLO_VII_SUCCESS, 0).

-define(CELLO_VII_FAULT,                     -1).
-define(CELLO_VII_NO_FAULT,                   0).
-define(CELLO_VII_LOADTEST_START,             1).
-define(CELLO_VII_LOADTEST_END,               2).
-define(CELLO_VII_NO_POWER,                   3).
-define(CELLO_VII_POWER,                      4).
-define(CELLO_VII_BOOTTEST_START,             5).
-define(CELLO_VII_BOOTTEST_END,               6).
-define(CELLO_VII_MISSING_RESOURCE_START,     7).
-define(CELLO_VII_MISSING_RESOURCE_END,       8).
-define(CELLO_VII_BOARD_LOCKED,               9).
-define(CELLO_VII_BOARD_UNLOCKED,            10).
-define(CELLO_VII_BOARD_BLOCKED,             11).
-define(CELLO_VII_BOARD_UNBLOCKED,           12).
-define(CELLO_VII_DISC_SYNC_START,           13).
-define(CELLO_VII_DISC_SYNC_STOP,            14).
-define(CELLO_VII_BOARD_BUSY_START,          15).
-define(CELLO_VII_BOARD_BUSY_STOP,           16).
-define(CELLO_VII_SHUTDOWN_START,            17).
-define(CELLO_VII_SHUTDOWN_END,              18).
-define(CELLO_VII_BACKUP_START,              19).
-define(CELLO_VII_BACKUP_END,                20).
-define(CELLO_VII_MEDIUM_BUTTON_PRESS_START, 21).
-define(CELLO_VII_MEDIUM_BUTTON_PRESS_END,   22).
-define(CELLO_VII_SHORT_BUTTON_PRESS_START,  23).
-define(CELLO_VII_SHORT_BUTTON_PRESS_END,    24).
-define(CELLO_VII_ALARM_SUPPRESS_START,      25).
-define(CELLO_VII_ALARM_SUPPRESS_END,        26).
-define(CELLO_VII_NODE_FAULT_START,          27).
-define(CELLO_VII_NODE_FAULT_END,            28).
-define(CELLO_VII_REMOTE_UNIT_FAULT_START,   29).
-define(CELLO_VII_REMOTE_UNIT_FAULT_END,     30).

-define(CELLO_VII_LED_OPERATIONAL, 1).
-define(CELLO_VII_LED_FAULT,       2).
-define(CELLO_VII_LED_STATUS,      3).
-define(CELLO_VII_LED_MAINTENANCE, 4).

-define(CELLO_VII_LED_OFF,              1).
-define(CELLO_VII_LED_ON,               2).
-define(CELLO_VII_LED_SLOW_BLINK,       3).
-define(CELLO_VII_LED_FAST_BLINK,       4).
-define(CELLO_VII_LED_DOUBLE_FLASH_OFF, 5).
-define(CELLO_VII_LED_DOUBLE_FLASH_ON,  6).

%% must match SaNtfSeverityT in saNtf.h
-define(SA_NTF_SEVERITY_CLEARED, 0).
-define(SA_NTF_SEVERITY_INDETERMINATE, 1).
-define(SA_NTF_SEVERITY_WARNING, 2).
-define(SA_NTF_SEVERITY_MINOR, 3).
-define(SA_NTF_SEVERITY_MAJOR, 4).
-define(SA_NTF_SEVERITY_CRITICAL, 5).

%% COM delay of alarm status changes
-define(COM_TRANSIENT_FILTER_TIME, 4000).

%% ad-hoc delay to ensure that clearing the
%% LKF alarm by rpc call results in the
%% alarm list being updated
-define(LKF_ALARM_CHANGE_DELAY, 4000).


%% maximum time until an alarm list change causes
%% a LED status change
%% -define(LED_LATENCY, 5000).

%% Assume 1000 ms is enough since no polling is involved
%% in the provisional solution
-define(LED_LATENCY, 1000).

%% Send alerts with this many milliseconds in between.
%% Set a huge value (3600000 or so) to disable.
-define(ALERTS_PERIOD, 1700).

%% Definitions for alarm used in this suite; the
%% alarms are defined in
%% IFT_CAX1033316/appdata/ift_app_alarm.xml
-define(ADD_INFO_NONE, 0).
-define(MO1_IMM, "testClass1Id=1,TESTMOMtestRootId=1").
-define(MO_EQUIPMENT, "equipmentId=1").
-define(MAJOR, 140).
-define(ALARM_0, 65504).
-define(ALARM_1, 65505).
-define(ALERT_65520, 65520).
-define(ENC_DOOR_OPEN, 45).


%% Testcases that will be run with a background of
%% alerts, which should be ignored.
-define(TC_W_ALERTS(TC),
	TC =:= test_red_path orelse
	    TC =:= test_black_path orelse
	    TC =:= test_blue_path orelse
	    TC =:= test_green_path).


%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

%% @hidden
suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks,
      [{rct_htmllink,[]},
       {rct_logging, {all, [{erlang, {["ERROR REPORT", "CRASH REPORT"], []}}]}},
       {rct_proxy,[{1, ?NODE, ssh_lmt_ipv4, ?DU, username}]},
       {rct_netconf, ?NC},
       {rct_rpc, ?RCS},
       {rct_cli, {?CLI, [manual_connect]}},
       {rct_core,[]}
      ]}
    ].


%%% ----------------------------------------------------------
%%% @doc Per-suite initialization.
%%% @end
%%% ----------------------------------------------------------
init_per_suite(Config) ->

    % Prepare for using the VII interface.
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?C_VII, ?VII),

    % Check if the LKF alarm is raised.
    HasLkfAlarm = hasLkfAlarm(getAlarms()),
    ct:pal("node has the LKF alarm: ~p", [HasLkfAlarm]),

    % Try to cease the LKF alarm temporarily if needed. In case of
    % failure install the LKF as a last resort. For some strange
    % reason this cannot be done just yet (badarg in rct_cli, bug?).
    {RestoreLkfAlarm, InstallLkfNeeded} =
	if
	    HasLkfAlarm ->
		case alterLkfAlarm(cease) of
		    {badrpc, _}=E ->
			ct:pal("failed to remove LKF alarm temporarily", [E]),
			{false, true};
		    Result ->
			ct:pal("removed LKF alarm temporarily: ~p", [Result]),
			timer:sleep(?COM_TRANSIENT_FILTER_TIME + ?LKF_ALARM_CHANGE_DELAY),
			NetcRes = getAlarms(),
			HasLkfAlarmAfterCease = hasLkfAlarm(NetcRes),
			if
			    HasLkfAlarmAfterCease ->
				% unclear status, stop here but try to restore
				% the alarm status first
				alterLkfAlarm(raise),
				ct:fail("the LKF alarm was still present after attempt to remove: ~p",
					[NetcRes]);
			    true ->
				ct:pal("the LKF alarm has been ceased temporarily"),
				{true, false}
			end
		end;
	    not HasLkfAlarm ->
		{false, false}
	end,

    %% Initialize for NTF actions.
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?C_NTFI, ?NTFI),

    %% Get an NTF handle.
    {ok, HighBits, LowBits} =
	rct_proxy:send_proxy(?NODE, ?C_NTFI, ?NTFI_INITIALIZE, {}),
    NtfHandle = (HighBits bsl 32) + LowBits,
    if
	HighBits =/= 0 ->
	    ct:fail("handle not reasonable: ~p", [NtfHandle]);
	true ->
	    ct:pal("NTF session initialized, handle: ~p", [NtfHandle])
    end,

    suiteDictCreate(installLkfNeeded, InstallLkfNeeded, Config) ++
	[{restoreLkfAlarm, RestoreLkfAlarm}] ++
	[{ntfHandle, NtfHandle}].


%%% ----------------------------------------------------------
%%% @doc Restores any alarms that were cleared in
%%% init_per_suite/1.
%%% @end
%%% ----------------------------------------------------------
end_per_suite(Config) ->

    NtfHandle = ?config(ntfHandle, Config),
    {ok} = rct_proxy:send_proxy(?NODE, ?C_NTFI, ?NTFI_FINALIZE, {NtfHandle}),
    ct:pal("NTF session finalized, handle: ~p", [NtfHandle]),

    {ok, client_stopped} = rct_proxy:stop_proxy(?NODE, ?C_NTFI),

    RestoreLkfAlarm = (?config(restoreLkfAlarm, Config) =:= true),
    if
	RestoreLkfAlarm ->
	    alterLkfAlarm(raise),
	    timer:sleep(?COM_TRANSIENT_FILTER_TIME + ?LKF_ALARM_CHANGE_DELAY),
	    apply(
	      fun(NetcRes) ->
		      HasLkfAlarmAfterRaise = hasLkfAlarm(NetcRes),
		      if
			  not HasLkfAlarmAfterRaise ->
			      ct:fail("the LKF alarm was not re-raised", [NetcRes]);
			  true ->
			      ct:pal("the LKF alarm has been re-raised")
		      end
	      end,
	      [getAlarms()]);
	true ->
	    ok
    end,

    {ok, client_stopped} = rct_proxy:stop_proxy(?NODE, ?C_VII),

    suiteDictDelete(Config),
    ok.


%% @hidden
init_per_group(_GroupName, Config) ->
    Config.


%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.


%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("=== << ~w >> ======================", [TestCase]),

    case suiteDictGet(installLkfNeeded, Config) of
	{ok, true} ->
	    ct:pal("try to install LKF"),
	    installLkf(),
	    suiteDictSet(installLkfNeeded, false, Config);
	{ok, false} ->
	    ct:pal("no need to install LKF"),
	    ok
    end,

    if
	?TC_W_ALERTS(TestCase) ->
	    Pid = spawn(fun() -> sendAlerts(Config) end),
	    suiteDictSet(alertSender, Pid, Config);
	true -> ok
    end,

    Config.




%% @hidden
end_per_testcase(TestCase, Config) ->
    if
	?TC_W_ALERTS(TestCase) ->
	    case suiteDictGet(alertSender, Config) of
		{ok, Pid} ->
		    Pid ! stop;
		_ ->
		    ok
	    end;
	true ->
	    ok
    end,
    ok.


%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], [{group, default__group}]},
     {sbc__def__all__1__group, [], []},
     {sbc__upgrade__all__1__group, [], []},
     {sdc__cover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},
     {sdc__qual__all__1__group, [], []},
     %% This suite can be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].


%% @hidden
all() ->
    [test_red_path,
     test_black_path,
     test_blue_path,
     test_green_path].


%%% ----------------------------------------------------------
%%% TEST CASES
%%% ----------------------------------------------------------

test_cli(_Config) ->
    R1 = rct_cli:connect(?CLI),
    R2 = rct_cli:send(?CLI, "ManagedElement=1"),
    R3 = rct_cli:disconnect(?CLI),
    ct:pal("CLI: ~p", [{R1, R2, R3}]),
    ok.


test_rpc(Config) ->
    ct:pal("SIM_OR_TARGET: ~s", [os:getenv("SIM_OR_TARGET")]),
    ct:pal("erlnode: ~p", [rct_rpc:get_erlnode(?RCS)]),
    ct:pal("appmServer:get_apps(): ~p",
	   [rct_rpc:call(?RCS, appmServer, get_apps, [], infinity)]),

    ct:pal("TC Config is: ~p", [Config]),

    ok.


test_reset_OutOfOrder(Config) ->
    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED).


%%% ----------------------------------------------------------
%%% @doc Raise alarm, open door, cease alarm, close door.
%%% This case includes both raise and update of the alarm
%%% that causes the status LED to be lit.
%%% @end
%%% ----------------------------------------------------------

test_red_path(Config) ->
    ok = verifyStatusLed(off),

    pauseBetweenTestCases(Config),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_WARNING),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of order"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_MAJOR),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of order (severity increased)"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_WARNING),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of order, open door"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(off),
    %ct:break("expect: open door"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(off),

    testCaseReady(Config),

    ok.





%%% ----------------------------------------------------------
%%% @doc Raise alarm, open door, close door, cease alarm.
%%% @end
%%% ----------------------------------------------------------

test_black_path(Config) ->
    ok = verifyStatusLed(off),

    pauseBetweenTestCases(Config),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of order"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_WARNING),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of order, open door"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of order"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(off),

    testCaseReady(Config),

    ok.

%%% ----------------------------------------------------------
%%% @doc Open door, raise alarm, raise another alarm, clear one
%%% alarm, clear the other alarm, close door.
%%%
%%% This case includes both raise and update of the "door"
%%% alarm.
%%% @end
%%% ----------------------------------------------------------

test_blue_path(Config) ->
    ok = verifyStatusLed(off),

    pauseBetweenTestCases(Config),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_WARNING),
    ok = verifyStatusLed(off),
    %ct:break("expect: open door"),

    ok = alarmAction(Config, ?ALARM_1, ?SA_NTF_SEVERITY_CRITICAL),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: open door, out of memory"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: open door, out of memory, out of order"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_MAJOR),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: open door, out of memory, out of order"),

    ok = alarmAction(Config, ?ALARM_1, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: open door, out of order"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(off),
    %ct:break("expect: open door"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(off),

    testCaseReady(Config),

    ok.


%%% ----------------------------------------------------------
%%% @doc Raise alarm, raise another alarm, open door,
%%% clear one alarm, clear the other alarm, close door.
%%% @end
%%% ----------------------------------------------------------

test_green_path(Config) ->
    ok = verifyStatusLed(off),

    pauseBetweenTestCases(Config),

    ok = alarmAction(Config, ?ALARM_1, ?SA_NTF_SEVERITY_CRITICAL),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of memory"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of memory, out of order"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_WARNING),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of memory, out of order, open door"),

    ok = alarmAction(Config, ?ALARM_1, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(steady_on),
    %ct:break("expect: out of order, open door"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(off),
    %ct:break("expect: open door"),

    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_CLEARED),
    ok = verifyStatusLed(off),

    testCaseReady(Config),

    ok.


%%% ----------------------------------------------------------
%%% @doc Raise an alarm, update and clear it. For interactive
%%% exercises. The status LED is not checked.
%%% @end
%%% ----------------------------------------------------------

test_alarm_raise_clear(Config) ->

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_WARNING),
    ct:break("alarm created, severity: warning"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL),
    ct:break("alarm updated, severity: critical"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED),
    ct:break("alarm cleared"),

    ok.


%%% ----------------------------------------------------------
%%% @doc Raise two alarms, one at a time, and clear them one by
%%% one. For interactive testing. The status LED is not checked.
%%% @end
%%% ----------------------------------------------------------

test_alarm_raise_two_clear(Config) ->

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_WARNING),
    ct:break("alarm 65504 created, severity: warning"),

    ok = alarmAction(Config, ?ALARM_1, ?SA_NTF_SEVERITY_MAJOR),
    ct:break("alarm 65505 created, severity: major"),

    ok = alarmAction(Config, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED),
    ct:break("alarm 65504 cleared"),

    ok = alarmAction(Config, ?ALARM_1, ?SA_NTF_SEVERITY_CLEARED),
    ct:break("alarm 65505 cleared, test case ends"),

    ok.


%%% ----------------------------------------------------------
%%% @doc Clear the "open door" alarm.
%%% @end
%%% ----------------------------------------------------------

do_open_door_clear(Config) ->
    ok = alarmAction(Config, ?ENC_DOOR_OPEN, ?SA_NTF_SEVERITY_CLEARED),
    ok.



%%% ----------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Store a timestamp.
%%% @end
%%% ----------------------------------------------------------

testCaseReady(Config) ->
    suiteDictSet(readyTimestamp, erlang:monotonic_time(), Config).

%%% ----------------------------------------------------------
%%% @doc If there is no stored timestamp: Set one and recurse.
%%% Otherwise hang until we are 60 s beyond the timestamp.
%%% This ensures that toggling is not triggered.
%%% @end
%%% ----------------------------------------------------------

pauseBetweenTestCases(Config) ->
    case suiteDictGet(readyTimestamp, Config) of
	undefined ->
	    testCaseReady(Config),
	    pauseBetweenTestCases(Config);
	{ok, T1} ->
	    T2 = erlang:monotonic_time(),
	    PausedSecs = erlang:convert_time_unit(T2-T1,native,seconds),
	    if
		PausedSecs < 60 ->
		    timer:sleep(2000),
		    pauseBetweenTestCases(Config);
		true -> ok
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc This function is needed only until
%%% lmaGlms:test_function_for_alarm/1 becomas available.
%%% @end
%%% ----------------------------------------------------------

installLkf() ->
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    rct_cli:connect(?CLI),
    rct_cli:send(?CLI, "ManagedElement=1,SystemFunctions=1,Lm=1", ">"),
    rct_cli:send(?CLI, "configure", ">"),
    rct_cli:send(?CLI, "fingerprint=tn_lab4", ">"),
    rct_cli:send(?CLI, "commit", ">"),
    rct_cli:send(?CLI, "KeyFileManagement=1", ">"),
    rct_cli:send(?CLI, "installKeyFile "
		 "sftp://"++Username++"@"++SftpHost++"/proj/rcs-tmp/tn_lab4_141008_082950.xml "++Password++">"),
		 %% "sftp://mauve@10.68.200.11/proj/rcs-tmp/tn_lab4_141008_082950.xml "
		 %% "dilbert",
		 %% ">"),
    rct_cli:disconnect(?CLI),

    timer:sleep(?COM_TRANSIENT_FILTER_TIME + 1000),
    NetcRes = getAlarms(),
    HasLkfAlarmAfterInstallLkf = hasLkfAlarm(NetcRes),
    if
	HasLkfAlarmAfterInstallLkf ->
	    ct:fail("the LKF alarm is present after LKF was installed: ~p",
		    [NetcRes]);
	true ->
	    ct:pal("the LKF alarm has been ceased after LKF was installed")
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the set of FmAlarm instances, or an "error"
%%% structure if the set is empty.
%%% @end
%%% ----------------------------------------------------------

getAlarms() ->
    FmAlarm = 'FmAlarm',
    Fm = {'Fm', [FmAlarm]},
    SystemFunctions = {'SystemFunctions', [Fm]},
    Filter = {'ManagedElement', [SystemFunctions]},
    NetconfResponse =
	doNetconf(
	  fun(F) ->
		  ct_netconfc:get(?NC, F)
	  end,
	  [Filter]),
    NetconfResponse.


%%% ----------------------------------------------------------
%%% @doc Executes a function within a NETCONF session.
%%% @end
%%% ----------------------------------------------------------

-spec doNetconf(fun(), list()) -> any().

doNetconf(Fun, Args) ->
    try
	{ok,_} = ct_netconfc:open(?NC, []),
	Result = apply(Fun, Args),
	ok = ct_netconfc:close_session(?NC),
	Result
    catch
	ExType:ExData ->
	    ct_netconfc:close_session(?NC),
	    ct:fail("netconf operation failed: ~p", [{ExType, ExData}])
    end.


%%% ----------------------------------------------------------
%%% @doc Returns true if the given NETCONF chunk contains a
%%% License Key File alarm instance.
%%% @end
%%% ----------------------------------------------------------

-spec hasLkfAlarm(any()) -> boolean().

hasLkfAlarm(
  {ok,
   [{'ManagedElement',
     [{xmlns, _NS1}],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',[],
       [{systemFunctionsId,[],["1"]},
	{'Fm',
	 [{xmlns, _NS2}],
	 Alarms}]}]}]}) ->
    lists:any(
      fun({'FmAlarm', [], ContentRaw}) ->
	      Content = lists:sort(lists:filter(fun({additionalInfo, _, _})->false; (_)->true end, ContentRaw)),
	      case Content of
		  [{activeSeverity,[],["CRITICAL"]},
		   {additionalText,[],["Key file fault in Managed Element"]},
		   {eventType,[],["PROCESSINGERRORALARM"]},
		   {fmAlarmId,[],[_RdnValue]},
		   {lastEventTime,[],[_LastEventTime]},
		   {majorType,[],["193"]},
		   {minorType,[],["9175046"]},
		   {originalAdditionalText,[],
		    ["Key file fault in Managed Element"]},
		   {originalEventTime,[],[_OrigEventTime]},
		   {originalSeverity,[],["CRITICAL"]},
		   {probableCause,[],["307"]},
		   {sequenceNumber,[],[_SeqNo]},
		   {source,[],["ManagedElement=1,SystemFunctions=1,Lm=1"]},
		   {specificProblem,[],["License Key File Fault"]}] ->
		      true;
		  _ ->
		      ct:pal("not a LKF alarm -~n~p", [Content]),
		      false
	      end
      end,
      proplists:delete(fmId, Alarms));

hasLkfAlarm({error, ContentRaw}=R) ->
    Content = lists:sort(ContentRaw),
    case Content of
	[{'error-message',
	  _Flags,
	  ["MO: ManagedElement,SystemFunctions,Fm,FmAlarm is not available (no instance)."]},
	 {'error-severity',
	  [{xmlns, _XmlNsUrl}],
	  ["error"]},
	 {'error-tag',
	  [{xmlns, _XmlNsUrl}],
	  ["data-missing"]},
	 {'error-type',
	  [{xmlns, _XmlNsUrl}],
	  ["application"]}
	] ->
	    false;
	_ ->
	    ct:fail("unexpected NETCONF response", [R])
    end;

hasLkfAlarm(Other) ->
    ct:fail("unexpected NETCONF response: ~p", [Other]).


%%% ----------------------------------------------------------
%%% @doc Produce an alarm or alert notification.
%%% @end
%%% ----------------------------------------------------------

alarmAction(Config, Minor, Severity) ->
    Handle =
	case ?config(ntfHandle, Config) of
	    undefined ->
		ct:fail("no NTF handle", []);
	    Value ->
		Value
	end,
    {MoInstance, AddText} = alarmInfo(Minor),
    case rct_proxy:send_proxy(
	   ?NODE,
	   ?C_NTFI,
	   ?NTFI_ALARM,
	   {Handle,
	    ?MAJOR,
	    Minor,
	    MoInstance,
	    Severity,
	    AddText,
	    ?ADD_INFO_NONE}) of
	{ok, NotificationId} ->
	    ct:pal("alarm/alert action: ~p",
		   [{Minor, Severity, MoInstance, NotificationId}]),
	    ok;
	Other ->
	    Other
    end.

alarmInfo(?ALARM_0) ->
    {?MO1_IMM, "NO_TEXT"};

alarmInfo(?ALARM_1) ->
    {?MO1_IMM, "NO_TEXT"};

alarmInfo(?ALERT_65520) ->
    {?MO1_IMM, "NO_TEXT"};

alarmInfo(?ENC_DOOR_OPEN) ->
    {?MO_EQUIPMENT, "NO_TEXT"}.


%%% ----------------------------------------------------------
%%% @doc Verifies the state of the status LED.
%%% @end
%%% ----------------------------------------------------------

verifyStatusLed(ExpectedStatus) ->
    timer:sleep(?COM_TRANSIENT_FILTER_TIME + ?LED_LATENCY + 1000),
    {ok, Code} =
	rct_proxy:send_proxy(?NODE, ?C_VII, ?Vii_visualIndGet, {indicatorCode(status)}),
    ActualStatus = behaviorName(Code),
        Symbol =
	case ActualStatus of
	    off -> $\s;
	    steady_on -> $*;
	    _ -> $?
	end,
    ct:print("~c[1;33;40m  ~c  ~c[0m", [27, Symbol, 27]),
    ct:pal("verify status led, expected: ~w, actual: ~w",
	   [ExpectedStatus, ActualStatus]),

    case ActualStatus of
	ExpectedStatus ->
	    ok;
	Other ->
	    {unexpected_status, Other}
    end.


indicatorCode(operational) ->
    ?CELLO_VII_LED_OPERATIONAL;
indicatorCode(fault) ->
    ?CELLO_VII_LED_FAULT;
indicatorCode(status) ->
    ?CELLO_VII_LED_STATUS;
indicatorCode(maintenance) ->
    ?CELLO_VII_LED_MAINTENANCE.


behaviorName(?CELLO_VII_LED_OFF) ->
    off;
behaviorName(?CELLO_VII_LED_ON) ->
    steady_on;
behaviorName(?CELLO_VII_LED_SLOW_BLINK) ->
    slow_blink;
behaviorName(?CELLO_VII_LED_FAST_BLINK) ->
    fast_blink;
behaviorName(?CELLO_VII_LED_DOUBLE_FLASH_OFF) ->
    double_flash_off;
behaviorName(?CELLO_VII_LED_DOUBLE_FLASH_ON) ->
    double_flash_on;
behaviorName(Other) ->
    ct:fail("cannot translate VII code: ~p", [Other]).


-spec suiteDictGet(any(), list()) -> {ok, any()} | undefined | no_process.

suiteDictGet(Key, Config) ->
    case ?config(suiteDict, Config) of
	undefined ->
	    no_process;
	Pid ->
	    Pid ! {get, Key, self()},
	    receive
		error ->
		    undefined;
		Other ->
		    Other
	    end
    end.


-spec suiteDictSet(any(), any(), list()) -> ok.

suiteDictSet(Key, Value, Config) ->
    case ?config(suiteDict, Config) of
	undefined ->
	    ct:fail("no suiteDict process; key: ~p, value: ~p",
		    [Key, Value]);
	Pid ->
	    Pid ! {set, Key, Value},
	    ok
    end.


-spec suiteDictCreate(any(), any(), list()) -> list().

suiteDictCreate(Key, Value, Config) ->
    NewConfig = suiteDictCreate(Config),
    suiteDictSet(Key, Value, NewConfig),
    NewConfig.


-spec suiteDictDelete(list()) -> list().

suiteDictDelete(Config) ->
    case ?config(suiteDict, Config) of
	undefined ->
	    Config;
	Pid ->
	    Pid ! stop,
	    proplists:delete(suiteDict, Config)
    end.


-spec suiteDictCreate(list()) -> list().

suiteDictCreate(Config) ->
    Pid = spawn(fun() -> suiteDictLoop(orddict:new()) end),
    [{suiteDict, Pid}|Config].


-spec suiteDictLoop(orddict:orddict()) -> ok.

suiteDictLoop(Dict) ->
    receive
	{get, Key, From} ->
	    From ! orddict:find(Key, Dict),
	    suiteDictLoop(Dict);
	{set, Key, Value} ->
	    NewDict = orddict:store(Key, Value, Dict),
	    suiteDictLoop(NewDict);
	stop ->
	    ok
    end.


-spec alterLkfAlarm(cease|raise) -> any().

alterLkfAlarm(Action) ->
    rct_rpc:call(?RCS, lmaGlms,
			 test_function_for_alarm, [Action], infinity).

%%% ----------------------------------------------------------
%%% @doc Keep sending alerts periodically. Alerts should not
%%% affect the status LED.
%%% @end
%%% ----------------------------------------------------------
sendAlerts(Config) ->
    receive
	stop ->
	    ok
    after ?ALERTS_PERIOD ->
	alarmAction(Config, ?ALERT_65520, ?SA_NTF_SEVERITY_MAJOR),
	sendAlerts(Config)
    end.

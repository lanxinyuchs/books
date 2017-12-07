%%% %CCaseFile:	ntf_alarm_via_ift_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/4

%%% @doc ==Tests of alarms and alerts raised via the IFT CXC==
%%%
%%% It appears that unreliable behaviour in COM can be triggered
%%% by untypical use of the configSnmpMgr() and deconfigSnmpMgr()
%%% functions. As long as the sequence of tests starts with config
%%% and ends with deconfig then behaviour seems to be OK.

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/1      2013-11-05 erarafo     Work in progress.
%%% R2A/2      2013-11-07 erarafo     First version.
%%% R2A/3      2013-11-18 erarafo     Tests of transient and toggling
%%% R2A/5      2013-11-25 erarafo     Added case for exercising alarms example
%%% R2A/6      2013-11-27 erarafo     Sync to changes in metadata
%%% R2A/9      2013-12-04 erarafo     Adapted to changed passing of handle
%%% R2A/10     2013-12-10 erarafo     Candidate for running in CI
%%% R2A/11     2013-12-11 erarafo     Reworked candidate for running in CI
%%% R2A/12     2014-08-21 erarafo     Revised after changes in RBS CS
%%% R2A/13     2014-08-25 erarafo     Allowing more "noise" traps
%%% R2A/14     2014-08-29 erarafo     Selectable DN style for notificationObject
%%% R2A/15     2014-09-03 erarafo     Ill-formed trap corrected
%%% R2A/16     2014-09-18 erarafo     On-board trap receiver configured in COM
%%% R2A/17     2014-09-19 erarafo     Retrieving on-board traps
%%% R2A/18     2014-09-23 erarafo     Adaptations for simulator support
%%% R2A/19     2014-09-23 erarafo     Improved printouts
%%% R2A/20     2014-09-24 erarafo     Adapted to changes in snmptrapd wrapper
%%% R3A/1      2014-09-25 erarafo     Work towards Additional Info
%%% R3A/2      2014-09-25 erarafo     Merge from R2A/21: Order of checking traps
%%% R3A/3      2014-09-26 erarafo     Merge from R2A/22: Fixed compile error
%%% R3A/4      2014-09-26 erarafo     Testing all Additional Info data types
%%% R3A/5      2014-09-28 erarafo     Verifying that the RCT/OTP SNMP manager is ok
%%% R3A/6      2014-09-30 erarafo     Verifying Additional Info item values
%%% R3A/7      2014-09-30 erarafo     Removed offending console printout
%%% R3A/8      2014-10-01 erarafo     Expect big-endian data in ARRAY item
%%% R3A/9      2014-10-01 erarafo     Adjusted console log output
%%% R3A/10     2014-10-02 erarafo     Adjusted to script changes in FAKE
%%% R3A/11     2014-10-03 erarafo     Specifying additional SNMP agent
%%% R3A/12     2014-10-04 erarafo     Netstat snapshot of the SNMP manager port
%%% R3A/13     2014-10-05 erarafo     Refactoring, improved logging
%%% R3A/14     2014-10-07 erarafo     Dialyzer faults fixed
%%% R3A/15     2014-10-07 erarafo     COM configuration moved to init_per_suite
%%%                                   Added code for checking on-board traps
%%%                                   (this code is not yet put to use)
%%%                                   Shortened the "transient" time a bit
%%% R3A/16     2014-10-08 erarafo     Refactored config/deconfig of COM
%%% R3A/17     2014-10-09 erarafo     Refactoring in progress
%%% R3A/18     2014-10-10 erarafo     Refactoring completed
%%% R3A/19     2014-10-13 erarafo     Order of SNMP managers
%%% R3A/20     2014-10-13 erarafo     Deferring ct:fail() to get max info
%%% R3A/21     2014-10-15 erarafo     Keeping host:port info in suite Config
%%% R3A/22     2014-10-23 erarafo     Robustness in analyzing Net-SNMP results
%%% R3A/23     2014-10-23 erarafo     Increased logging
%%% R3A/24     2014-10-29 erarafo     Second Opinion adjustments
%%% R3A/25     2014-11-16 erarafo     Disabled net-snmp for tcu03
%%% R3A/26     2014-11-25 erarafo     Test case for demo purposes
%%% R3A/27     2014-11-26 erarafo     Activating Net-SNMP with TCU03
%%% R3A/28     2014-11-27 erarafo     Using the EthernetLinkFailure alarm
%%% R3A/29     2014-12-16 erarafo     Skip on-board manager if not started
%%% R3A/30     2014-12-18 erarafo     Unprecise timing problem addressed
%%% R3A/31     2014-12-19 erarafo     "Unused" warnings fixed
%%% R3A/32     2014-12-19 erarafo     Alarm minorType changed
%%% R3A/33     2015-02-20 erarafo     Added a long-playing test case (HT23830)
%%% R3A/34     2015-03-23 erarafo     WP2979 additions
%%% R3A/35     2015-03-28 erarafo     Fixing test case interaction
%%% R4A/1      2015-05-13 erarafo     Use NETCONF for alarms validation
%%% R4A/3      2015-05-15 erarafo     Survive if trap receiver not started
%%% R4A/4      2015-05-15 erarafo     Fix a log print that upsets local CI
%%% R4A/5      2015-05-17 erarafo     Handle "license key alarm NOT present"
%%% R4A/6      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/7      2015-09-23 erarafo     Added test_raise_clear_with_add_info, HU17686
%%% R4A/8      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp()
%%% R4A/9      2015-10-16 erarafo     Just to get mail about failures
%%% R4A/10     2015-11-03 erarafo     Suite version included in HTML report
%%% R4A/11     2015-11-03 erarafo     Fixed typo
%%% R4A/12     2015-11-10 erarafo     Test of alarms handling at warm restart
%%% R4A/13     2015-11-12 erarafo     Warm restart, added some delay
%%% R5A/1      2016-01-19 erarafo     Support for fetching ift_app application log,
%%%                                     timing of test_netconf_raise_clear,
%%%                                     fixed end of test_warm_restart,
%%%                                     testcase for hu51667
%%% R6A/1      2016-06-21 erarafo     Tests for JIRA-RCS-18 added
%%% R6A/2      2016-06-30 erarafo     Reverted SNMP_VERIFICATION to disabled
%%% R6A/3      2016-07-15 etxivri     Update Ign alarm.
%%% R6A/3      2016-07-15 etxivri     More update of Ign alarm.
%%% R6A/5      2016-08-11 erarafo     Obfuscate strings ERROR and WARNING in output
%%% R6A/6      2016-08-26 etxivri     Increased time in COM_TRANSIENT_PLUS macro.
%%% R6A/7      2016-09-06 etxkols     Cloud
%%% R7A/1      2016-09-06 etxivri     Changed random to rand due to random is
%%%                                   depricated in OTP19..
%%% R7A/2      2016-10-12 etxkols     NTP alarm in VRCS (cloud).
%%% R8A/1      2016-12-06 erarafo     TC for configuredSeverity, work in progress
%%% R8A/2      2016-12-14 erarafo     TC for configuredSeverity, work in progress
%%% R8A/3      2016-12-19 uabhten     TC for configuredSeverity
%%% R8A/4      2017-01-11 uabhten     Removed unused variable 'Result' in doConfiguredSeverity
%%% R9A/1-2    2017-02-24 etxberb     Added "Lost Connection to File Server" in
%%%                                   the Ignore alarms list.
%%% R11/1      2017-08-29 elarrun     Added test for SP539 Alarm correlation, test mapping
%%%                                   from NTF infoId to name
%%% R11A/5     2017-10-20 etxkols     Changing ct:pal to ct:log
%%% R12A/3     2017-11-09 etxpeno     Add test_single_rat_restart/1

-module(ntf_alarm_via_ift_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/4').
-date('2017-11-14').

-define(SNMP_VERIFICATION, (os:timestamp() =:= {0, 0, 0})).     % disabled
%-define(SNMP_VERIFICATION, (os:timestamp() =/= {0, 0, 0})).    % enabled

-define(IF(A, B), if A -> B; true -> ok end).

% Define which format to use in notificationObject DNs towards the
% NTF API. This applies to alarms created by the IFT application only.
% The alarms_example program has hard-coded IMM style regardless of
% what is defined here.
-define(DN_STYLE_IMM, true).
%-define(DN_STYLE_3GPP, true).


% Specifies a number of milliseconds that the test suite must wait
% after adding SNMP targets in COM. Failure to do so may cause
% SNMP PDUs to arrive too late.
-define(COM_CONFIG_WAIT, 15000).


% Transients shorter than this many milliseconds are ignored by COM.
-define(COM_TRANSIENT_TIME, 4000).


% A time period that is clearly greater than the COM transient filter time.
-define(COM_TRANSIENT_PLUS, ?COM_TRANSIENT_TIME + 2500).


% Port number hardcoded in trapReceiver.sh, which is part of
% the FAKE CXC. This port number is also reserved in
% SYS/make_release.conf.
-define(ONBOARD_MGR_PORT, "11001").

-define(TRAPRECEIVER_LOG, "trapReceiver-log.txt").


-define(ADD_INFO_NONE, 0).
-define(ADD_INFO_STRING,    16#0002).
-define(ADD_INFO_CORR_INFO, 16#0040).
-define(ADD_INFO_ALARM_ID,  16#0080).
-define(ADD_INFO_UUID,      16#0100).
-define(ADD_INFO_DN3,       16#0200).
-define(ADD_INFO_INFOID0,   16#0400).

-define(ADD_INFO_DYN_STRING, 16#0800).


% The values below, for tests of Additional Info, must match
% hard-coded values in test_ntfi.c

-define(TESTVALUE_STRING_1,    "please get us some ladders real soon").
-define(TESTVALUE_CORR_INFO_1,     "{\"C\":\"191f0558-94bd-49d1-9851-ac99ceac95c2\",\"n\": \"RadioNode\"},\"P\": \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"}").
-define(TESTVALUE_ALARM_ID_1,      "4711").
-define(TESTVALUE_UUID_1,          "191f0558-94bd-49d1-9851-ac99ceac95c2").
-define(TESTVALUE_DN3_1,           "SubNetwork=LteNetwork,ManagedElement=11300_RadioNode12,ENodeBFunction=1").
-define(TESTVALUE_INFOID0_NAME_1,  "ThereIs").
-define(TESTVALUE_INFOID0_VALUE_1, "EqualsInTheString").
-define(TESTVALUE_INFOID0_2,       "ThereIsNoEqualsInTheString").
-define(TESTVALUE_INFOID0_NAME_3,  "ThereCanBe").
-define(TESTVALUE_INFOID0_VALUE_3, "Multiple=Equals=In=The=String").

-include("ntf_alarm_allowed_traps.hrl").


-export([test_config/1,
	 test_warm_restart/1,
	 test_netconf_raise_clear/1,
	 test_netconf_raise_clear_configuredSeverity/1,

	 %% test of Single Rat restart
	 test_single_rat_restart/1,

	 test_composite/1,
	 test_quick/1,
	 test_alarm_example_alarms/1,
	 test_alarm_example_alerts/1,
	 test_raise_clear/1,
	 test_demo/1,
	 test_raise_clear_quickly/1,
	 test_raise_clear_raise_clear/1,
	 test_raise_clear_raise_clear_quickly/1,
	 test_toggling/1,
	 test_add_info_weird_string/1,
	 % test_toggling_critical_warning/1,
	 % test_toggling_critical_cleared_warning_cleared/1,
	 test_raise_clear_with_add_info/1,
	 test_alerts/1,

	 % some low-level tests created during development
	 test_count_lines/1,
	 test_get_lines/1,
	 test_get_snmptrapd_log/1,
	 test_expand_string/1,

	 % possible error behaviour reported by Robert Wingren
	 test_toggling_ewinrob/1,

         test_raise_clear_long/1,
	 test_raise/1,
	 test_clear/1,
	 test_raise_FOSS_critical/1,
	 test_raise_FOSS_indeterminate/1,
	 test_clear_FOSS/1,
	 test_raise_coffee_minor/1,
	 test_raise_coffee_indeterminate/1,
	 test_clear_coffee/1,
	 test_raise_need_reboot_indeterminate/1,
	 test_clear_need_reboot/1,
	 test_wp2979/1,
	 test_hu51667/1,
	 test_JIRA_RCS_18_AA/1,
	 test_JIRA_RCS_18_AB/1,
	 test_JIRA_RCS_18_AD/1,
	 test_JIRA_RCS_18_BA/1,
	 test_JIRA_RCS_18_BB/1,
	 test_JIRA_RCS_18_BD/1,
	 test_JIRA_RCS_18_CA/1,
	 test_JIRA_RCS_18_CB/1,
	 test_JIRA_RCS_18_CC/1,
	 test_JIRA_RCS_18_CD/1
	]).

-export([
	 suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0
	]).


%% consider dropping
-export([waitForWarmRestart/1]).

%% just to avoid "unused" warnings
-export([netSnmpPrepare/2,
	 netSnmpVerify/3]).

-include_lib("common_test/include/ct.hrl").

% Use values above 100 for stretching times (makes test cases go slower).
-define(STRETCH, 100).

% A number of milliseconds well below 2000.
% Not subject to stretching.
-define(TRANSIENT, 1100).


-define(INFORM_TIMEOUT, "100").
-define(INFORM_RETRY, "5").


-define(SNMP_MGR_NAME, snmp1).
-define(NETCONF_SESSION, nc1).


%% Must match SaNtfSeverityT in saNtf.h
-define(SA_NTF_SEVERITY_CLEARED, 0).
-define(SA_NTF_SEVERITY_INDETERMINATE, 1).
-define(SA_NTF_SEVERITY_WARNING, 2).
-define(SA_NTF_SEVERITY_MINOR, 3).
-define(SA_NTF_SEVERITY_MAJOR, 4).
-define(SA_NTF_SEVERITY_CRITICAL, 5).

% Must match definitions in IFT
-define(NTFI, 11).
-define(NTFI_INITIALIZE, 1).
-define(NTFI_FINALIZE, 2).
-define(NTFI_ALARM, 3).
-define(NTFI_ALARM_RAISE_CLEAR, 4).
-define(NTFI_ALARM_CLEAR_RAISE, 5).
-define(NTFI_ALARM_RAISE, 6).
-define(NTFI_ALARM_CLEAR, 7).

-define(NTFI_START_ALARMS_EXAMPLE, 91).
-define(NTFI_STOP_ALARMS_EXAMPLE, 92).

-define(NTFI_COUNT_LINES, 101).
-define(NTFI_GET_TAIL_LINES, 102).

-define(NTFI_EXPAND_STRING, 901).

% Must match IFT metadata in ift_app_alarm.xml
-define(MAJOR, 140).
-define(ALARM_0, 65504).
-define(ALARM_1, 65505).
-define(ALARM_2, 65506).
-define(ALARM_3, 65507).

-define(ALARM_EDO, 45).  % Enclosure Door Open
-define(ALARM_ELF, 21).  % Ethernet Link Failure
-define(ALARM_FOSS, 3).  % FallbackOperationStartingSoon
-define(ALARM_COCOA, 65518).
-define(ALARM_COFFEE, 65519).
-define(ALARM_REBOOT, 65510).

-define(ALERT_0, 65520).
-define(ALERT_1, 65521).
-define(ALERT_2, 65522).
-define(ALERT_3, 65523).

-ifdef(DN_STYLE_IMM).
-define(MO1_IMM, "testClass1Id=1,TESTMOMtestRootId=1").
-define(MO2_IMM, "testClass2Id=1,TESTMOMtestRootId=1").
-define(EQUIPMENT_IMM, "equipmentId=1").
-define(TRANSPORT_IMM, "transportId=1").
-endif.

-ifdef(DN_STYLE_3GPP).
-define(MO1_IMM, "ManagedElement=1,TestRoot=1,TestClass1=1").
-define(MO2_IMM, "ManagedElement=1,TestRoot=1,TestClass2=1").
-define(EQUIPMENT_IMM, "ManagedElement=1,Equipment=1").
-define(TRANSPORT_IMM, "ManagedElement=1,Transport=1").
-endif.


-define(NrOfNetConfUsers, 1).
-define(DU, du1).
-define(NODE, node1).
-define(CHILD, child1).
-define(PROXY_TIMEOUT_XL, 25000).


-record(snmpTarget,
	{id                        :: string(),
	 type                      :: string(),   % "RCT/OTP" | "NET-SNMP"
         rdnValue                  :: string(),
         ip                        :: string(),
	 port                      :: string(),
	 mode="TRAP"               :: string(),   % "TRAP" | "INFORM"
	 timeoutTicks="ignored"    :: string(),
	 maxRetries="ignored"      :: string(),
	 prepare                   :: fun(),
	 verify                    :: fun()
	}).

-record(scenario,
	{steps                     :: list()
	}).


%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

suite() ->
    NetconfHooks =
	[{rct_netconf, list_to_atom("nc"++integer_to_list(N))}
	 || N <- lists:seq(1,?NrOfNetConfUsers)],

    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_proxy,[{1, ?NODE, ssh_lmt_ipv4, ?DU, username}]},
		 {rct_rpc, rpc1},
		 {rct_core,[]},
		 {rct_snmpmgr, {?SNMP_MGR_NAME, [{extra_agents, externalIpAddresses()}]}},
                 {rct_logging, {rabbe_logs, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
		] ++ NetconfHooks
     }
    ].



%%% ----------------------------------------------------------
%%% @doc Define SNMP managers for the suite and configure
%%% COM to send traps to them.
%%% @end
%%% ----------------------------------------------------------

init_per_suite(Config) ->

%%     InitDelay = 60,
%%     ct:pal("*** A looong delay (~w s) follows, to ensure that COM is 'warm'.~n"
%% 	   "*** If you are testing interactively you may want to tweak this~n"
%% 	   "*** in the init_per_suite/1 function.", [InitDelay]),
%%     delayFixed(InitDelay*1000, ms),

    iftStart(),
    {ok, BT} = rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_EXPAND_STRING, {"$BT"}),
    ct:pal("board type: ~s", [BT]),
    OnBoardPort = getOnBoardPort(),
    ct:pal("OnBoardPort: ~s", [OnBoardPort]),
    iftStop(),

    {{mgr_ip, MgrIp}, {mgr_port, MgrPort}, _, _} =
	rct_snmpmgr:get_mgr_data(?SNMP_MGR_NAME),

    ct:pal("RCT/OTP SNMP Manager listening at: ~s:~s", [MgrIp, MgrPort]),

    Mode =
	case os:getenv("SIM_OR_TARGET") of
	    "sim" ->
		"TRAP";
	    "target" ->
		"INFORM";
	    "cloudish" ->
		"INFORM"
	end,

    Targets =
	lists:append(
	  [if
	       OnBoardPort =:= "unknown" ->
		   %% has been seen to happen occasionally on
		   %% dus32, not clear why, inspect the dumped
		   %% log from which the port would have been
		   %% collected if successful. TODO !!!!
		   [];
	       true ->
		   %% The Net-SNMP manager setup is just not
		   %% robust enough; perhaps try to launch it
		   %% just for the duration of the test suite
		   %% instead (wrap it as a dynamic program
		   %% perhaps).
		   []
%% 		   [#snmpTarget{id="onboard1",
%% 				type="NET-SNMP",
%% 				mode=Mode,
%% 				ip="127.0.0.1",
%% 				port=OnBoardPort,
%% 				rdnValue="2",
%% 				timeoutTicks=?INFORM_TIMEOUT,
%% 				maxRetries=?INFORM_RETRY,
%% 				prepare=fun netSnmpPrepare/2,
%% 				verify=fun netSnmpVerify/3
%% 			       }]
	   end
	  ,
	   case ?SNMP_VERIFICATION of
	       true ->
		   [#snmpTarget{id="rct1",
				type="RCT/OTP",
				mode=Mode,
				ip=MgrIp,
				port=MgrPort,
				rdnValue="1",
				timeoutTicks=?INFORM_TIMEOUT,
				maxRetries=?INFORM_RETRY,
				prepare=fun rctPrepare/2,
				verify=fun rctVerify/3
			       }];
	       _ ->
		   []
	   end
	  ]),

    configSnmpTargets(Targets),

    %% enable this break if demoing the SNMP target configuration
    %% is wanted
    %%
    %% ct:break("SNMP target configuration ready"),

    [{suiteVersion, getVersion(?MODULE)},    % look in HTML results, bottom of 'init_per_suite'
     {onboardTrapReceiverPort, OnBoardPort},
     {snmpTargets, Targets},
     {togglePrevent, 0}
    ] ++ Config.


%% @hidden

end_per_suite(Config) ->
    SnmpTargets = ?config(snmpTargets, Config),
    deconfigSnmpTargets(SnmpTargets),
    ok.


%% @hidden

init_per_group(GroupName, Config) ->
    if
	GroupName =:= group_JIRA_RCS_18 ->
	    [{togglePrevent, 50000}|Config];
	true ->
	    Config
    end.


%% @hidden

end_per_group(_GroupName, _Config) ->
    ok.


%% @hidden

init_per_testcase(Testcase, Config) ->
    banner(Testcase),
    iftStart(),
    Config.


%% @hidden

end_per_testcase(Testcase, _Config) ->
    if
	Testcase =:= test_warm_restart ->
	    ok;
	true ->
	    iftStop()
    end,
    ok.


%% @hidden

groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},
     {sdc__qual__all__1__group, [], []},

     {group_JIRA_RCS_18, [],
      [test_JIRA_RCS_18_AA,
       test_JIRA_RCS_18_AB,
       test_JIRA_RCS_18_AD,
       test_JIRA_RCS_18_BA,
       test_JIRA_RCS_18_BB,
       test_JIRA_RCS_18_BD,
       test_JIRA_RCS_18_CA,
       test_JIRA_RCS_18_CB,
       test_JIRA_RCS_18_CC,
       test_JIRA_RCS_18_CD
      ]}
    ].


%% @hidden

all() ->
    Mode = os:getenv("SIM_OR_TARGET"),
    all(Mode).

all("cloudish") ->
    %% Don't test single rat restart in cloud
    [test_warm_restart,
     test_netconf_raise_clear,
     test_netconf_raise_clear_configuredSeverity,
     test_raise_clear_with_add_info];
all(Mode) when Mode == "sim";
	       Mode == "target" ->
    [test_warm_restart,
     test_netconf_raise_clear,
     test_netconf_raise_clear_configuredSeverity,
     test_raise_clear_with_add_info,
     test_single_rat_restart

%%     ,
%%      test_alerts,
%%      test_wp2979,
%%      test_composite
    ].


%%% ----------------------------------------------------------
%%% @doc Default set of cases to be run in CI. Alarms are chosen so
%%% as to not trigger the toggling filter unintendedly.
%%% @end
%%% ----------------------------------------------------------

test_composite(Config) ->
    ok = doCase(test_get_snmptrapd_log, Config),
    do_verify_snmp_mgr(Config),
    ok = doCase(test_raise_clear, Config),                        % ALARM_0
    ok = doCase(test_alarm_example_alarms, Config),               % separate alarms
    ok = doCase(test_alarm_example_alerts, Config),               % separate alarms
    ok = doCase(test_raise_clear_quickly, Config),                % ALARM_2
    ok = doCase(test_raise_clear_raise_clear, Config),            % ALARM_1
    ok = doCase(test_raise_clear_raise_clear_quickly, Config),    % ALARM_3
    delay(15, s),                                                 % safety measure
    ok = doCase(test_toggling, Config),                           % ALARM_0 again
    ok.


%%% ----------------------------------------------------------
%%% @doc Execute a testcase after printing its banner.
%%% @end
%%% ----------------------------------------------------------

-spec doCase(atom(), list()) -> ok.

doCase(Function, Config) ->
    banner(Function),
    apply(?MODULE, Function, [Config]).


%%% ----------------------------------------------------------
%%% @doc A smaller sequence of tests, sometimes useful in debugging.
%%% @end
%%% ----------------------------------------------------------

test_quick(Config) ->
    ok = test_raise_clear(Config),                     % ALARM_0
    ok = test_alarm_example_alarms(Config),            % separate alarm
    ok = test_raise_clear_raise_clear(Config),         % ALARM_1
    delay(5, s),                                       % safety measure
    ok.


%%% ----------------------------------------------------------
%%% @doc Useful for an interactive demo. Consider enabling the
%%% break in init_per_suite/1 too.
%%% @end
%%% ----------------------------------------------------------

test_demo(Config) ->
    DnImm = ?EQUIPMENT_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmWarning},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {alarm, DnImm, ?ALARM_EDO, ?SA_NTF_SEVERITY_WARNING, "NO_TEXT", ?ADD_INFO_NONE},
	 {break, "alarm was raised"},
	 %{pause, ms, 4000},  % see above
	 {alarm, DnImm, ?ALARM_EDO, ?SA_NTF_SEVERITY_CLEARED, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Executes the Alarm Service IWD example program. This testcase
%%% requests the IFT application to start the example program as a
%%% dynamic program.
%%%
%%% Dedicated alarm types are used.
%%%
%%% The timing of the dynamically started program cannot be controlled
%%% by the test case. The 12000 ms pause is longer than the time needed
%%% by the program to step through its scenario.
%%% @end
%%% ----------------------------------------------------------

test_alarm_example_alarms(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[
	 [{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveMajorType, 193},
	  {eriAlarmActiveMinorType, 9240559},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Coffee beans refill required"}],

	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmActiveMajorType, 193},
	  {eriAlarmActiveMinorType, 9240559},
	  {eriAlarmNObjAdditionalText, "Coffee beans refill required"}]
	],
    Allowed = ?ALLOWED_TRAPS ++ [[{type, eriAlarmWarnAlert}]],
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {alarmExample, start},
	 {pause, ms, 12000},  % see above
	 {alarmExample, stop},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Same as the previous test except that it verifies the
%%% alert.
%%% @end
%%% ----------------------------------------------------------

test_alarm_example_alerts(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmWarnAlert},
	  {eriAlarmAlertSpecificProblem,"Integrity Monitor Excessive Vibrations"},
	  {eriAlarmAlertMajorType,193},
	  {eriAlarmAlertMinorType,9240564}]],
    Allowed =
	[
	 [{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveMajorType, 193},
	  {eriAlarmActiveMinorType, 9240559},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Coffee beans refill required"}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmActiveMajorType, 193},
	  {eriAlarmActiveMinorType, 9240559},
	  {eriAlarmNObjAdditionalText, "Coffee beans refill required"}]
	] ++ ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {alarmExample, start},
	 {pause, ms, 12000},  % see above
	 {alarmExample, stop},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Send one alarm and clear it, after waiting for a
%%% sufficient amount of time so that it passes the transient
%%% filter in COM.
%%%
%%% The 4000 ms pause is sufficently long so that the
%%% transient filter in COM will not suppress the alarm.
%%% @end
%%% ----------------------------------------------------------

test_raise_clear(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raiseClear, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?COM_TRANSIENT_PLUS},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Raise an application alarm, then perform a warm
%%% restart, then verify that the alarm has been cleared.
%%%
%%% NOTE: The name of this function is hardcoded in
%%% end_per_testcase/2.
%%% @end
%%% ----------------------------------------------------------
test_warm_restart(_Config) ->
    DnImm = ?MO1_IMM,
    Exp = [[{source, "ManagedElement=1,TestRoot=1,TestClass1=1"},
	    {activeSeverity, "CRITICAL"},
	    {additionalText, "Out of order."}
	    ]],
    %% Ign = [[{source, "ManagedElement=1,SystemFunctions=1,Lm=1"},
    %% 	    {specificProblem, "License Key File Fault"}]],
    %% Ign = [[{source, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1"},
    %% 	    {specificProblem, "Emergency Unlock Reset Key Required"}]],
    Ign = [[{source, "ManagedElement=1,SystemFunctions=1,Lm=1"},
    	    {specificProblem, "License Key File Fault"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1"},
    	    {specificProblem, "Emergency Unlock Reset Key Required"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,SysM=1"},
    	    {specificProblem, "Calendar Clock All NTP Servers Unavailable"}],
	   [{specificProblem, "Lost Connection to File Server"}]],

    Steps =
	[{ntfi, initialize},

	 {raise, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT"},

	 {pause, ms, ?COM_TRANSIENT_PLUS},
	 {validateAlarms, Exp, Ign},

	 {warmRestart},
	 {waitForCom, 30000},

	 {pause, ms, 5000},  % enough for COM to be ready?
	 {validateAlarms, [], Ign}
	],
    execute(#scenario{steps=Steps}),
    ok.

test_single_rat_restart(_Config) ->
    DnImm = ?MO1_IMM,
    Exp = [[{source, "ManagedElement=1,TestRoot=1,TestClass1=1"},
	    {activeSeverity, "CRITICAL"},
	    {additionalText, "Out of memory."}
	   ]],
    Ign = [[{source, "ManagedElement=1,SystemFunctions=1,Lm=1"},
    	    {specificProblem, "License Key File Fault"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1"},
    	    {specificProblem, "Emergency Unlock Reset Key Required"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,SysM=1"},
    	    {specificProblem, "Calendar Clock All NTP Servers Unavailable"}],
	   [{specificProblem, "Lost Connection to File Server"}]],

    PG = "mppg1",
    Steps = [{ntfi, initialize},

	     {raise, DnImm, ?ALARM_1, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", PG},

	     {pause, ms, ?COM_TRANSIENT_PLUS},
	     {validateAlarms, Exp, Ign},

	     {pgRestart, PG},

	     {pause, ms, 4000},
	     {validateAlarms, [], Ign},

	     {ntfi, finalize}],
    execute(#scenario{steps=Steps}),
    ok.

test_netconf_raise_clear(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}]],
    Allowed = ?ALLOWED_TRAPS,

    Exp = [[{source, "ManagedElement=1,TestRoot=1,TestClass1=1"},
	    {activeSeverity, "CRITICAL"},
	    {additionalText, "Out of order."}
	    ]],
    Ign = [[{source, "ManagedElement=1,SystemFunctions=1,Lm=1"},
    	    {specificProblem, "License Key File Fault"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1"},
    	    {specificProblem, "Emergency Unlock Reset Key Required"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,SysM=1"},
    	    {specificProblem, "Calendar Clock All NTP Servers Unavailable"}],
	   [{specificProblem, "Lost Connection to File Server"}]],

    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},

	 {raise, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT"},
	 {pause, ms, ?COM_TRANSIENT_PLUS},
	 {validateAlarms, Exp, Ign},

	 {clear, DnImm, ?ALARM_0, "NO_TEXT"},
	 {pause, ms, ?COM_TRANSIENT_PLUS},
	 {validateAlarms, [], Ign},

	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


test_netconf_raise_clear_configuredSeverity(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65505"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65505"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}]],
    Allowed = ?ALLOWED_TRAPS,

    Exp = [[{source, "ManagedElement=1,TestRoot=1,TestClass1=1"},
	    {activeSeverity, "MINOR"},
	    {additionalText, "Out of memory."}
	    ]],
    Ign = [[{source, "ManagedElement=1,SystemFunctions=1,Lm=1"},
    	    {specificProblem, "License Key File Fault"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1"},
    	    {specificProblem, "Emergency Unlock Reset Key Required"}],
	   [{source, "ManagedElement=1,SystemFunctions=1,SysM=1"},
    	    {specificProblem, "Calendar Clock All NTP Servers Unavailable"}],
	   [{specificProblem, "Lost Connection to File Server"}]],

    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},

	  % change configuredSeverity
	 {configuredSeverity, "mock-alarm-65505", "MINOR"},

	 {raise, DnImm, ?ALARM_1, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT"},
	 {pause, ms, ?COM_TRANSIENT_PLUS},
	 {validateAlarms, Exp, Ign},

	 {clear, DnImm, ?ALARM_1, "NO_TEXT"},
	 {pause, ms, ?COM_TRANSIENT_PLUS},
	 {validateAlarms, [], Ign},

	 % change configuredSeverity  back to empty
	 {configuredSeverity, "mock-alarm-65505", empty},

	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Send one alarm and clear it. The clear is sent quickly after
%%% the raise so no traps are expected.
%%% @end
%%% ----------------------------------------------------------

test_raise_clear_quickly(Config) ->
    DnImm = ?MO1_IMM,
    SnmpTargets = ?config(snmpTargets, Config),
    Expected = [],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raiseClear, DnImm, ?ALARM_2, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?TRANSIENT},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Test that a clear followed by a raise
%%% is not filtered if the raise comes after a
%%% pause greater than the clear transient time.
%%%
%%% The pauses in this testcase (4000 ms) are
%%% sufficently long compared to the COM transient
%%% filter time (2000 ms).
%%% @end
%%% ----------------------------------------------------------

test_raise_clear_raise_clear(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65505"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of memory."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65505"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of memory."}],
	 [{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65505"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of memory."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65505"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of memory."}]
	],

        Allowed = ?ALLOWED_TRAPS,

        Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raiseClear, DnImm, ?ALARM_1, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", 4000},
	 {pause, ms, 4000},
	 {raiseClear, DnImm, ?ALARM_1, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", 4000},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
        execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Test that a clear followed by a raise
%%% is filtered if the raise comes quickly enough.
%%%
%%% The other pauses in this testcase (5000 ms) are
%%% sufficently long compared to the COM transient
%%% filter time (2000 ms).
%%% @end
%%% ----------------------------------------------------------

test_raise_clear_raise_clear_quickly(Config) ->

    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65507"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of luck."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65507"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of luck."}]
	],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {alarm, DnImm, ?ALARM_3, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 5000},
	 {clearRaise, DnImm, ?ALARM_3, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?TRANSIENT},
	 {pause, ms, 5000},
	 {alarm, DnImm, ?ALARM_3, ?SA_NTF_SEVERITY_CLEARED, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 5000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],

    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Provoke the "toggling" filter in COM. The test will take
%%% 3+ minutes since COM waits for 180 s before letting a "clear"
%%% notification go out over the NBI.
%%%
%%% The 4000 ms pauses are longer than the transient filter time
%%% in COM. The 190000 ms pause is longer than the recover-from
%%% toggling time in COM.
%%% @end
%%% ----------------------------------------------------------

test_toggling(Config) ->
    DnImm = ?MO1_IMM,
    DnMim = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    RaiseCommon =
	[{eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	 {eriAlarmActiveManagedObject, DnMim}],
    Raise =
	[{type, eriAlarmCritical},
	 {eriAlarmNObjAdditionalText, "Out of order."}
	     | RaiseCommon],
    Toggling =
	[{type, eriAlarmCritical},
	 {eriAlarmNObjAdditionalText, "Out of order. The alarm is currently toggling."}
	     | RaiseCommon],
    Clear =
	[{type, eriAlarmCleared},
	 {eriAlarmNObjAdditionalText, "Out of order."}
	     | RaiseCommon],
    Expected = [Raise, Clear, Raise, Clear, Toggling, Clear],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 4000},  % see above
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 4000},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 4000},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 4000},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 4000},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED, "NO_TEXT", ?ADD_INFO_NONE},
	 {pause, ms, 190000},  % see above
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


test_config(Config) ->
    ct:print("config: ~p", [Config]).


%%% ----------------------------------------------------------
%%% @doc This TC provokes a crash in the Erlang log. See HU17686
%%% for details. Retry when the TR is fixed.
%%% @end
%%% ----------------------------------------------------------
test_add_info_weird_string(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}]],
    Allowed = ?ALLOWED_TRAPS,
    WeirdString = "<Empty container>",
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", ?ADD_INFO_DYN_STRING, 4711, WeirdString},
	 {pause, ms, 4000},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED, "NO_TEXT", ?ADD_INFO_DYN_STRING, 4711, WeirdString},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.



%% COM does not perceive this scenario as toggling. The severity
%% is going back and forth between the levels WARNING and CRITICAL,
%% without going to CLEARED in between. This test is not included in
%% all/0. - TO BE REVISED

%% test_toggling_critical_warning(_Config) ->
%%     DnImm = ?MO1_IMM,
%%     DnMim = pairlistFlatten(?MO1_COM),
%%     Minor = ?ALARM_0,
%%
%%     %configSnmpMgr(true, Config),
%%     iftStart(),
%%
%%     RaiseCommon =
%% 	[{type, eriAlarmCritical},
%% 	 {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
%% 	 {eriAlarmActiveManagedObject, DnMim}],
%%     Raise =
%% 	[{eriAlarmNObjAdditionalText, "replace unit"}
%% 	     | RaiseCommon],
%%     %%     Toggling =
%%     %% 	[{eriAlarmNObjAdditionalText, "replace unit The alarm is currently toggling."}
%%     %% 	| RaiseCommon],
%%
%%     Clear =
%% 	[{type, eriAlarmCleared},
%% 	 {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
%% 	 {eriAlarmActiveManagedObject, DnMim}],
%%
%%     Warning =
%% 	[{type, eriAlarmWarning},
%% 	 {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
%% 	 {eriAlarmActiveManagedObject, DnMim}],
%%
%%     TrapsExpected = [Raise, Warning, Raise, Warning, Raise, Warning, Raise, Clear],
%%
%%     Options = [{allowed_traps, ?ALLOWED_TRAPS}],
%%     rct_snmpmgr:wait_for_traps(TrapsExpected, Options, 180 + stretching(30, s)),
%%
%%     Handle = initialize(), delay(2, s),
%%
%%     SkipLines = getLinesCount(),
%%
%%     raise(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     warning(Handle, DnImm, Minor, ""), delay(2500, ms),
%%
%%     raise(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     warning(Handle, DnImm, Minor, ""), delay(2500, ms),
%%
%%     raise(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     warning(Handle, DnImm, Minor, ""), delay(2500, ms),
%%
%%     raise(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     clear(Handle, DnImm, Minor, ""),
%%
%%     ok = ?CHECK_TRAPS(SkipLines),
%%
%%     finalize(Handle),
%%
%%     iftStop(),
%%     %configSnmpMgr(false, []).
%%
%%     ok.


%% COM behaves in a complex fashion here, subject to
%% discussion. This test is not included in all/0.
%% TO BE REVISED.

%% test_toggling_critical_cleared_warning_cleared(_Config) ->
%%     DnImm = ?MO1_IMM,
%%     DnMim = pairlistFlatten(?MO1_COM),
%%     Minor = ?ALARM_0,
%%
%%     Common =
%% 	[{eriAlarmActiveSpecificProblem, "mock-alarm-100"},
%% 	 {eriAlarmActiveManagedObject, DnMim}],
%%     Raise =
%% 	[{type, eriAlarmCritical},
%% 	 {eriAlarmNObjAdditionalText, "replace unit"}
%% 	     | Common],
%%     Warning =
%% 	[{type, eriAlarmWarning},
%% 	 {eriAlarmNObjAdditionalText, "replace unit"}
%% 	     |Common],
%%     TogglingR =
%% 	[{type, eriAlarmCritical},
%% 	 {eriAlarmNObjAdditionalText, "replace unit The alarm is currently toggling."}
%% 	     | Common],
%%     TogglingW =
%% 	[{type, eriAlarmWarning},
%% 	 {eriAlarmNObjAdditionalText, "replace unit The alarm is currently toggling."}
%% 	     | Common],
%%     Clear =
%% 	[{type, eriAlarmCleared},
%% 	 {eriAlarmNObjAdditionalText, "replace unit"},
%% 	 {eriAlarmActiveSpecificProblem, "mock-alarm-100"},
%% 	 {eriAlarmActiveManagedObject, DnMim}],
%%
%%     TrapsExpected = [Raise, Clear, Warning, Clear, TogglingR, TogglingW, Clear],
%%     Options = [{allowed_traps, ?ALLOWED_TRAPS}],
%%     rct_snmpmgr:wait_for_traps(TrapsExpected, Options, 180 + stretching(30, s)),
%%
%%     Handle = initialize(), delay(2, s),
%%
%%     SkipLines = getLinesCount(),
%%
%%     raise(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     clear(Handle, DnImm, Minor, ""), delay(2500, ms),
%%
%%     warning(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     clear(Handle, DnImm, Minor, ""), delay(2500, ms),
%%
%%     raise(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     clear(Handle, DnImm, Minor, ""), delay(2500, ms),
%%
%%     warning(Handle, DnImm, Minor, ""), delay(2500, ms),
%%     clear(Handle, DnImm, Minor, ""),
%%
%%     ok = ?CHECK_TRAPS(SkipLines),
%%
%%     finalize(Handle),
%%
%%     ok.


%%% ----------------------------------------------------------
%%% @doc Test of using additional info for primitive
%%% numeric data types.
%%%
%%% The 4000 ms pauses are chosen to be longer than the
%%% transient filter time in COM (2000 ms).
%%% @end
%%% ----------------------------------------------------------

test_raise_clear_with_add_info(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}]],
    Allowed = ?ALLOWED_TRAPS,

    AddInfo = ?ADD_INFO_STRING bor
              ?ADD_INFO_CORR_INFO bor
              ?ADD_INFO_ALARM_ID bor
              ?ADD_INFO_UUID bor
              ?ADD_INFO_DN3 bor
              ?ADD_INFO_INFOID0,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", AddInfo},
	 {pause, ms, 4000},  % see above
	 {verifyAddInfo},
	 {alarm, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CLEARED, "NO_TEXT", AddInfo},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed},
	 {reportAddInfo}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Test that alerts cause SNMP traps, and that
%%% transient and toggling filtering is not applied in COM.
%%%
%%% The 8000 ms pauses are probalby overkill here. Alerts
%%% should pass through the system with very little delay
%%% since no transient filter is applied by COM.
%%% @end
%%% ----------------------------------------------------------

test_alerts(Config) ->
    DnImm = ?MO2_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmWarnAlert},
	  {eriAlarmAlertSpecificProblem, "mock-alert-65520"},
	  {eriAlarmAlertManagedObject, MimDn}]
	,
	 [{type, eriAlarmWarnAlert},
	  {eriAlarmAlertSpecificProblem, "mock-alert-65521"},
	  {eriAlarmAlertManagedObject, MimDn}]
	,
	 [{type, eriAlarmWarnAlert},
	  {eriAlarmAlertSpecificProblem, "mock-alert-65522"},
	  {eriAlarmAlertManagedObject, MimDn}]
	,
	 [{type, eriAlarmWarnAlert},
	  {eriAlarmAlertSpecificProblem, "mock-alert-65523"},
	  {eriAlarmAlertManagedObject, MimDn}]
	],

    _U=[{type,eriAlarmWarnAlert},
 {agentAddr,"147.214.13.189"},
 {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass2=1"},
 {eriAlarmActiveEventTime,"2014-10-10 19:18:11"},
 {eriAlarmActiveLastSequenceNo,46},
 {eriAlarmActiveMajorType,193},
 {eriAlarmActiveMinorType,9240560},
 {eriAlarmActiveSpecificProblem,"mock-alert-65520"},
 {eriAlarmActiveEventType,other},
 {eriAlarmActiveProbableCause,45},
 {eriAlarmNObjAdditionalText,"Attention!"},
 {eriAlarmNObjMoreAdditionalText,false},
 {eriAlarmNObjResourceId,false}]
,

    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {alert, ?ALERT_0, DnImm, ?SA_NTF_SEVERITY_WARNING},
	 {pause, ms, 8000},  % see above
	 {alert, ?ALERT_1, DnImm, ?SA_NTF_SEVERITY_WARNING},
	 {pause, ms, 8000},
	 {alert, ?ALERT_2, DnImm, ?SA_NTF_SEVERITY_WARNING},
	 {pause, ms, 8000},
	 {alert, ?ALERT_3, DnImm, ?SA_NTF_SEVERITY_WARNING},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


multiplyList(N, L) ->
    lists:foldl(fun(_K, A) -> L++A end, [], lists:seq(1, N)).


test_raise_clear_long(Config) ->

    Episodes = 50,
    Period = 33,
    EpisodeLength = 10,

    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	multiplyList(
	  Episodes,
	  [[{type, eriAlarmCritical},
	    {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	    {eriAlarmActiveManagedObject, MimDn},
	    {eriAlarmNObjAdditionalText, "Out of order."}],
	   [{type, eriAlarmCleared},
	    {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	    {eriAlarmActiveManagedObject, MimDn},
	    {eriAlarmNObjAdditionalText, "Out of order."}]]),
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize}
	]++multiplyList(
	  Episodes,
	  [{raiseClear, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", EpisodeLength*1000},
	   {pause, ms, (Period-EpisodeLength)*1000}
	  ])++
	    [{ntfi, finalize},
	     {verify, SnmpTargets, Expected, Allowed}],
    execute(#scenario{steps=Steps}),
    ok.





%%% ----------------------------------------------------------
%%% @doc Test the lines count operation. Expect to see the number
%%% of lines in /etc/group.
%%% @end
%%% ----------------------------------------------------------

test_count_lines(_Config) ->
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_COUNT_LINES, {"/etc", "group"})]).


%%% ----------------------------------------------------------
%%% @doc Test the get lines operation. Expect to see the lines in
%%% /etc/group, except for 5 skipped initial lines.
%%% @end
%%% ----------------------------------------------------------

test_get_lines(_Config) ->
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_GET_TAIL_LINES, {"/etc", "group", 5})]).


%%% ----------------------------------------------------------
%%% @doc Display the content of the on-board trap receiver log.
%%% Some filtering is applied.
%%% @end
%%% ----------------------------------------------------------

test_get_snmptrapd_log(_Config) ->
    Lines = getLines(0, 100),
    LinesFiltered =
	lists:filter(
	  fun("trap from: "++_) ->
		  false;
	     (_) ->
		  true
	  end,
	  Lines),
    ct:pal("on-board trap receiver log contains (filtered) -~n~p",
	   [LinesFiltered]),
    ok.


%%% ----------------------------------------------------------
%%% @doc Test of environment variable expansion in an RBS CS
%%% application (IFT, to be specific).
%%% @end
%%% ----------------------------------------------------------

test_expand_string(_Config) ->
    {ok, ""} = rct_proxy:send_proxy(
		 ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
		 {""}),
    {ok, "$"} = rct_proxy:send_proxy(
		  ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
		  {"$"}),
    {ok, "$,"} = rct_proxy:send_proxy(
		   ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
		   {"$,"}),
    {ok, "$$"} = rct_proxy:send_proxy(
		   ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
		   {"$$"}),
    {ok, ",,$"} = rct_proxy:send_proxy(
		    ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
		    {",,$"}),
    {ok, "/aa"} = rct_proxy:send_proxy(
		    ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
		    {"$NOSUCHNAME_IN_ENV/aa"}),
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
	      {"APP_TMP: $APP_TMP"})]),
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
	      {"BT: $BT"})]),
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
	      {"CXP_REV: $CXP_REV, CXP_PATH: $CXP_PATH"})]),
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
	      {"CXC_NAME: $CXC_NAME, CXC_NO: $CXC_NO, CXC_REV: $CXC_REV"})]),
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
	      {"LD_LIBRARY_PATH: $LD_LIBRARY_PATH"})]),
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
	      {"LOG_DIR: $LOG_DIR"})]),
    ct:pal("~p",
	   [rct_proxy:send_proxy(
	      ?NODE, ?CHILD, ?NTFI_EXPAND_STRING,
	      {"RESTART_TYPE: $RESTART_TYPE"})]).


test_toggling_ewinrob(ConfigIn) ->

    % verified
    %Config = [{addText, default}|ConfigIn],

    %verified
    %Config = [{addText, nonDefault}|ConfigIn],

    %expected to fail with as-is COM: causes the
    %alarm to be sent out repeatedly with an appended
    %'currently toggling' warning.
    Config = [{addText, nonDefaultOnRaise}|ConfigIn],

    DnImm = ?TRANSPORT_IMM,
    DnMim = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),

    AdditionalTextNtfRaise =
	case ?config(addText, Config) of
	    default ->
		"NO_TEXT";
	    nonDefault ->
		"check cable please";
	    nonDefaultOnRaise ->
		"check cable please"
	end,

    AdditionalTextNtfClear =
	case ?config(addText, Config) of
	    default ->
		"NO_TEXT";
	    nonDefault ->
		"check cable please";
	    nonDefaultOnRaise ->
		"NO_TEXT"
	end,

    AdditionalTextComRaise =
	case ?config(addText, Config) of
	    default ->
		"";
	    nonDefault ->
		"check cable please";
	    nonDefaultOnRaise ->
		"check cable please"
	end,

    AdditionalTextComClear =
	case ?config(addText, Config) of
	    default ->
		"";
	    nonDefault ->
		"check cable please";
	    nonDefaultOnRaise ->
		""
	end,

    RaiseCommon =
	[{eriAlarmActiveSpecificProblem, "Ethernet Link Failure"},
	 {eriAlarmActiveManagedObject, DnMim}],
    Raise =
	[{type, eriAlarmMinor},
	 {eriAlarmNObjAdditionalText, AdditionalTextComRaise}
	     | RaiseCommon],
    Clear =
	[{type, eriAlarmCleared},
	 {eriAlarmNObjAdditionalText, AdditionalTextComClear}
	     | RaiseCommon],
    Toggling =
	[{type, eriAlarmMinor},
	 {eriAlarmNObjAdditionalText, AdditionalTextComRaise++" The alarm is currently toggling."}
	     | RaiseCommon],

    Expected = [Raise, Clear, Raise, Clear, Toggling, Clear],

    Allowed = ?ALLOWED_TRAPS,



    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},

	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_MINOR, AdditionalTextNtfRaise, ?ADD_INFO_DYN_STRING, 0, "eventId=1"},
	 {pause, ms, 3500},
	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_CLEARED, AdditionalTextNtfClear, ?ADD_INFO_NONE},
	 {pause, ms, 11000},

	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_MINOR, AdditionalTextNtfRaise, ?ADD_INFO_DYN_STRING, 0, "eventId=2"},
	 {pause, ms, 3500},
	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_CLEARED, AdditionalTextNtfClear, ?ADD_INFO_NONE},
	 {pause, ms, 11000},

	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_MINOR, AdditionalTextNtfRaise, ?ADD_INFO_DYN_STRING, 0, "eventId=3"},
	 {pause, ms, 3500},
	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_CLEARED, AdditionalTextNtfClear, ?ADD_INFO_NONE},
	 {pause, ms, 11000},

	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_MINOR, AdditionalTextNtfRaise, ?ADD_INFO_DYN_STRING, 0, "eventId=4"},
	 {pause, ms, 3500},
	 {alarm, DnImm, ?ALARM_ELF, ?SA_NTF_SEVERITY_CLEARED, AdditionalTextNtfClear, ?ADD_INFO_NONE},
	 {pause, ms, 185000},

	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Raise the "Out of order" alarm, severity CRITICAL.
%%% @end
%%% ----------------------------------------------------------
test_raise(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_0, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Clear the "Out of order" alarm.
%%% @end
%%% ----------------------------------------------------------
test_clear(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "mock-alarm-65504"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText, "Out of order."}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {clear, DnImm, ?ALARM_0, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Raise the Fallback alarm, severity CRITICAL. The
%%% default severity of this alarm is WARNING.
%%% @end
%%% ----------------------------------------------------------
test_raise_FOSS_critical(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "A Fallback Operation will soon be started"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_FOSS, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Raise the Fallback alarm, severity INDETERMINATE. The
%%% default severity of this alarm is WARNING, so expect to see
%%% this.
%%% @end
%%% ----------------------------------------------------------
test_raise_FOSS_indeterminate(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmWarning},
	  {eriAlarmActiveSpecificProblem, "A Fallback Operation will soon be started"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_FOSS, ?SA_NTF_SEVERITY_INDETERMINATE, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Clears the Fallback alarm.
%%% @end
%%% ----------------------------------------------------------
test_clear_FOSS(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "A Fallback Operation will soon be started"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {clear, DnImm, ?ALARM_FOSS, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Raises the Coffe Bean alarm with perceivedSeverity 'minor'.
%%% @end
%%% ----------------------------------------------------------
test_raise_coffee_minor(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMinor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MINOR, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Raises the Coffe Bean alarm with perceivedSeverity 'indeterminate'.
%%% Since no default severity is defined for this alarm type the
%%% severity of the alarm sent out shall be WARNING by default.
%%% @end
%%% ----------------------------------------------------------
test_raise_coffee_indeterminate(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmWarning},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_INDETERMINATE, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Clears the Coffe Beans alarm.
%%% @end
%%% ----------------------------------------------------------

test_clear_coffee(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Raises the NeedRebootJustInCase alarm with severity
%%% INDETERMINATE. Since this alarm has default severity
%%% MAJOR we expect to see this.
%%% @end
%%% ----------------------------------------------------------
test_raise_need_reboot_indeterminate(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Need A Quick Reboot Just In Case Something Is Wrong"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_REBOOT, ?SA_NTF_SEVERITY_INDETERMINATE, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Clears the NeedRebootJustInCase alarm.
%%% @end
%%% ----------------------------------------------------------
test_clear_need_reboot(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Need A Quick Reboot Just In Case Something Is Wrong"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {clear, DnImm, ?ALARM_REBOOT, "NO_TEXT"},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


%%% ----------------------------------------------------------
%%% @doc Tests of the WP2979 functionality.
%%% @end
%%% ----------------------------------------------------------
test_wp2979(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMinor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Cocoa Powder Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Cocoa Powder Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],

	 [{type, eriAlarmWarning},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Cocoa Powder Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Cocoa Powder Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],

	 [{type, eriAlarmCritical},
	  {eriAlarmActiveSpecificProblem, "A Fallback Operation will soon be started"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "A Fallback Operation will soon be started"},
	  {eriAlarmActiveManagedObject, MimDn}],

	 [{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Need A Quick Reboot Just In Case Something Is Wrong"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Need A Quick Reboot Just In Case Something Is Wrong"},
	  {eriAlarmActiveManagedObject, MimDn}]
	],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raiseClear, DnImm, ?ALARM_COCOA, ?SA_NTF_SEVERITY_MINOR, "NO_TEXT", 4000},
	 {pause, ms, 4000},

	 {raiseClear, DnImm, ?ALARM_COCOA, ?SA_NTF_SEVERITY_INDETERMINATE, "NO_TEXT", 4000},
	 {pause, ms, 4000},

	 {raiseClear, DnImm, ?ALARM_FOSS, ?SA_NTF_SEVERITY_CRITICAL, "NO_TEXT", 4000},
	 {pause, ms, 4000},

	 {raiseClear, DnImm, ?ALARM_REBOOT, ?SA_NTF_SEVERITY_INDETERMINATE, "NO_TEXT", 4000},
	 {pause, ms, 4000},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.


test_hu51667(Config) ->
    AlarmType = ?ALARM_REBOOT,
    AddTextPredef = "Rebooting sometimes mysteriously helps.",
    AddTextOther1 = "plz reboot",
    NoText = "NO_TEXT",
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMinor}, {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmMinor}, {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared}, {eriAlarmActiveManagedObject, MimDn}]],
    Allowed =
	?ALLOWED_TRAPS ++
	    [],
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},

	 % raise the alarm
	 {pause, ms, 1000},
	 {alarm, DnImm, AlarmType, ?SA_NTF_SEVERITY_MAJOR, AddTextPredef, ?ADD_INFO_NONE},

	 % flicker once
	 {pause, ms, 5000},
	 {alarm, DnImm, AlarmType, ?SA_NTF_SEVERITY_CLEARED, NoText, ?ADD_INFO_NONE},
	 {pause, ms, 5},
	 {alarm, DnImm, AlarmType, ?SA_NTF_SEVERITY_MAJOR, NoText, ?ADD_INFO_NONE},

	 % flicker twice ... must not trigger toggling filter so long pause needed;
	 % in this case the change of additional text causes COM to think of it
	 % as an alarm "update".
	 {pause, ms, 57000},
	 {alarm, DnImm, AlarmType, ?SA_NTF_SEVERITY_CLEARED, AddTextPredef, ?ADD_INFO_NONE},
	 {pause, ms, 5},
	 {alarm, DnImm, AlarmType, ?SA_NTF_SEVERITY_MAJOR, AddTextOther1, ?ADD_INFO_NONE},

	 % clear
	 {pause, ms, 1000},
	 {alarm, DnImm, AlarmType, ?SA_NTF_SEVERITY_CLEARED, NoText, ?ADD_INFO_NONE},

	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],

    execute(#scenario{steps=Steps}).


%%% ----------------------------------------------------------
%%% @doc Tests of cases where two 'raise' occurs in sequence
%%% with different additionalText. The tests may be run as the group
%%% group_JIRA_RCS_18. Because of the need for delays to avoid
%%% triggering the toggling filter the group takes some 10 minutes
%%% to execute.
%%% @end
%%% ----------------------------------------------------------
test_JIRA_RCS_18_AA(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "NO_TEXT"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "NO_TEXT"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_AB(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "NO_TEXT"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "Coffee beans refill required"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_AD(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type,eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText,"fill up"}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "NO_TEXT"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_BA(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "Coffee beans refill required"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "NO_TEXT"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_BB(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "Coffee beans refill required"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "Coffee beans refill required"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_BD(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type,eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText,"fill up"}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "Coffee beans refill required"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_CA(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type,eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText,"Coffee beans refill required"}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "NO_TEXT"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_CB(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type,eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText,"Coffee beans refill required"}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "Coffee beans refill required"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_CC(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    timer:sleep(?config(togglePrevent, Config)),
    ok.

test_JIRA_RCS_18_CD(Config) ->
    DnImm = ?MO1_IMM,
    MimDn = immToMim(DnImm),
    SnmpTargets = ?config(snmpTargets, Config),
    Expected =
	[[{type, eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}],
	 [{type,eriAlarmMajor},
	  {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn},
	  {eriAlarmNObjAdditionalText,"fill up ASAP"}],
	 [{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, "Resource Monitor Coffee Beans Container Low Level"},
	  {eriAlarmActiveManagedObject, MimDn}]],
    Allowed = ?ALLOWED_TRAPS,
    Steps =
	[{prepare, SnmpTargets, Expected, Allowed},
	 {ntfi, initialize},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up"},
	 {pause, ms, 6000},
	 {raise, DnImm, ?ALARM_COFFEE, ?SA_NTF_SEVERITY_MAJOR, "fill up ASAP"},
	 {pause, ms, 6000},
	 {clear, DnImm, ?ALARM_COFFEE, "NO_TEXT"},
	 {ntfi, finalize},
	 {verify, SnmpTargets, Expected, Allowed}
	],
    execute(#scenario{steps=Steps}),
    ok.




%%% ----------------------------------------------------------
%%% @doc Executes an alarm scenario.
%%% @end
%%% ----------------------------------------------------------

execute(#scenario{steps=Steps}) ->
    lists:foldl(fun execute/2, [], Steps),
    ok.

execute({ntfi, initialize}, State) ->
    Handle = initialize(),
    statePut(ntfiHandle, Handle, State);

execute({ntfi, finalize}, State) ->
    Handle = stateGet(ntfiHandle, State),
    finalize(Handle),
    stateDelete(ntfiHandle, State);

execute({alarm, DnImm, Minor, Severity, AddText, AddInfo}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, Severity, AddText, AddInfo},
    {ok, _NotifId} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM, Notif),
    State;

execute({alarm, DnImm, Minor, Severity, AddText, AddInfo, DynStrId, DynStrValue}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, Severity, AddText, AddInfo,
	     DynStrId, DynStrValue},
    {ok, _NotifId} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM, Notif),
    State;

execute({raiseClear, DnImm, Minor, Severity, AddText, SeparationMillis}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, Severity, AddText, SeparationMillis},
    {ok, {NotifId1, NotifId2}} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM_RAISE_CLEAR, Notif, ?PROXY_TIMEOUT_XL),
    ct:pal("raiseClear, notification ids: ~s, ~s", [NotifId1, NotifId2]),
    State;

execute({clearRaise, DnImm, Minor, Severity, AddText, SeparationMillis}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, Severity, AddText, SeparationMillis},
    {ok, {NotifId1, NotifId2}} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM_CLEAR_RAISE, Notif, ?PROXY_TIMEOUT_XL),
    ct:pal("raiseClear, notification ids: ~s, ~s", [NotifId1, NotifId2]),
    State;

execute({raise, DnImm, Minor, Severity, AddText}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, Severity, AddText, 0},
    {ok, {NotifId1, _NotifId2}} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM_RAISE, Notif, ?PROXY_TIMEOUT_XL),
    ct:pal("raise, notification id: ~s", [NotifId1]),
    State;

execute({raise, DnImm, Minor, Severity, AddText, PG}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, Severity, AddText, 0, PG},
    {ok, {NotifId1, _NotifId2}} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM_RAISE, Notif, ?PROXY_TIMEOUT_XL),
    ct:pal("raise, notification id: ~s", [NotifId1]),
    State;

execute({clear, DnImm, Minor, AddText}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, ?NTFI_ALARM_CLEAR, AddText, 0},
    {ok, {NotifId1, _NotifId2}} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM_CLEAR, Notif, ?PROXY_TIMEOUT_XL),
    ct:pal("clear, notification id: ~s", [NotifId1]),
    State;

execute({alert, Minor, DnImm, Severity}, State) ->
    Handle = stateGet(ntfiHandle, State),
    Notif = {Handle, ?MAJOR, Minor, DnImm, Severity, "NO_TEXT", ?ADD_INFO_NONE},
    {ok, _NotifId} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_ALARM, Notif),
    State;

execute({alarmExample, start}, State) ->
    PgmId =
	case rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_START_ALARMS_EXAMPLE, {}) of
	    {error, Code} ->
		ct:fail("error, code: ~p", [Code]);
	    {ok, Id} ->
		ct:pal("started the example program, pgm id: ~p", [Id]),
		Id
	end,
    State++[{examplePgmId, PgmId}];

execute({alarmExample, stop}, State) ->
    % kind of cheating, the program id is taken from
    % a static memory item in IFT, TODO: handle the id here
    case rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_STOP_ALARMS_EXAMPLE, {}) of
	{error, Code} ->
	    ct:fail("error, code: ~p", [Code]);
	{ok} ->
	    ct:pal("stopped the example program", [])
    end,
    stateDelete(examplePgmId, State);

execute({prepare, SnmpTargets, Expected, Allowed}, State) ->
    ExistingMgrData =
	stateGet(mgrData, State, dictEmpty()),
    PairsById =
	lists:foldl(
	  fun(#snmpTarget{id=Id, prepare=Fun}, Acc) ->
		  Pairs = apply(Fun, [Expected, Allowed]),
		  Acc++[{Id, Pairs}]
	  end,
	  [],
	  SnmpTargets),
    NewMgrData =
	lists:foldl(
	  fun({Id, Pairs}, Dict) -> dictExtend(Id, Pairs, Dict) end,
	  ExistingMgrData,
	  PairsById),
    statePut(mgrData, NewMgrData, State);

execute({verify, SnmpTargets, Expected, Allowed}, State) ->
    Dict = stateGet(mgrData, State, dictEmpty()),
    Reports =
       [begin
             case Mode of %% TRAP == simulator, skip check
         	 "TRAP" ->
                     {Id, skip};
                  _ ->
                     Data = dictGetPairs(Id, Dict),
                     Report = apply(Fun, [Expected, Allowed, Data]),
                     {Id, Report}
	     end
	 end
	 ||#snmpTarget{verify=Fun, id=Id, mode=Mode} <- SnmpTargets],
    Success =
	lists:foldl(
	  fun({Id, Report}, Acc) ->
		  case Report of
		      ok ->
			  ct:pal("successful verification, manager: ~s",
				 [Id]),
			  Acc;
		      skip ->
			  ct:pal("verification skipped, manager: ~s",
				 [Id]),
			  Acc;
		      Other ->
			  ct:pal("verification failed, manager: ~s, what: ~p",
				 [Id, Other]),
			  false
		  end
	  end,
	  true,
	  Reports),
    if
	Success ->
	    ct:pal("successful verification by all managers", []);
	true ->
	    ct:fail("unsuccessful verification", [])
    end,
    State;

execute({pause, ms, Millis}, State) ->
    delay(Millis, ms),
    State;

execute({pauseFixed, ms, Millis}, State) ->
    delayFixed(Millis, ms),
    State;

execute({verifyAddInfo}, State) ->
    FailureDescrs = verifyAddInfo(),
    statePut(failureDescrs, FailureDescrs, State);

execute({reportAddInfo}, State) ->
    case stateGet(failureDescrs, State) of
	undefined ->
	    ct:fail("testcase internal error", []);
	[] ->
	    ok;
	FailureDescrs ->
	    ct:fail("failed to verify some AdditionalInfo items -~n~p",
		    [FailureDescrs])
    end,
    State;

execute({validateAlarms, AlarmPatterns, Ignore}, State) ->
    validateAlarms(AlarmPatterns, Ignore),
    State;

execute({break, Message}, State) ->
    ct:break(Message),
    State;

execute({warmRestart}, State) ->
    IsWarmRestartEnabled =
	rct_rpc:call(rpc1, appmServer, is_warm_restart_enabled, [], 4000),
    if
	not(IsWarmRestartEnabled) ->
	    rct_rpc:call(rpc1, appmServer, enable_warm_restart, [none], 4000);
	true ->
	    ok
    end,

    rct_rpc:call(rpc1, appmI, restart_node, [warm, "ManualRestart"], 4000),
    % waitForWarmRestart(100),

    if
	not(IsWarmRestartEnabled) ->
	    rct_rpc:call(rpc1, appmServer, disable_warm_restart, [none], 4000);
	true ->
	    ok
    end,
    State;

execute({pgRestart, Pg}, State) ->
    IsPgRestartEnabled =
	rct_rpc:call(rpc1, appmServer, is_pg_restart_enabled, [], 4000),

    case IsPgRestartEnabled of
	true ->
	    ok;
	false ->
	    rct_rpc:call(rpc1, appmServer, enable_pg_restart, [none], 4000)
    end,

    rct_rpc:call(rpc1, appmServer, restart_pg, [Pg], 4000),

    case IsPgRestartEnabled of
	true ->
	    ok;
	false ->
	    rct_rpc:call(rpc1, appmServer, disable_pg_restart, [none], 4000)
    end,

    State;

execute({waitForCom, Timeout}, State) ->
    doWaitForCom(Timeout, false),
    State;

execute({configuredSeverity, AlarmTypeId, Severity}, State) ->
    doConfiguredSeverity(AlarmTypeId, Severity),
    State;

execute(Other, _State) ->
    ct:fail("unknown scenario step: ~p", [Other]).


doWaitForCom(Timeout, _HasBeenDown) when Timeout < 0 ->
    ct:fail("timed out waiting for COM", []);

doWaitForCom(Timeout, HasBeenDown) ->
    RpcResult = rct_rpc:call(rpc1, comsaEvent, testControl, [isComRunning, []], 1500),
    ct:print(
      "waiting for COM to come up; has been down: ~w, new RPC result: ~p",
      [HasBeenDown, RpcResult]),
    case RpcResult of
	{badrpc, Reason} ->
	    ct:pal("bad RPC: ~p", [Reason]),
	    timer:sleep(500),
	    doWaitForCom(Timeout - 500, true);
	stopped ->
	    timer:sleep(500),
	    doWaitForCom(Timeout - 500, true);
	started ->
	    if
		not HasBeenDown ->
		    timer:sleep(500),
		    doWaitForCom(Timeout - 500, false);
		HasBeenDown ->
		    ct:print("COM is up at last", []),
		    started
	    end;
	Other ->
	    ct:print("unexpected RPC result: ~p", [Other]),
	    timer:sleep(500),
	    doWaitForCom(Timeout - 500, true)
    end.


stateGet(Key, State) ->
    proplists:get_value(Key, State).


stateGet(Key, State, Default) ->
    proplists:get_value(Key, State, Default).


statePut(Key, Value, State) ->
    case proplists:lookup(Key, State) of
	none ->
	    [{Key, Value}|State];
	_ ->
	    [{Key, Value}|stateDelete(Key, State)]
    end.


stateDelete(Key, State) ->
    proplists:delete(Key, State).

dictGetPairs(Id, Dict) ->
    case orddict:find(Id, Dict) of
	error ->
	    [];
	{ok, Pairs} ->
	    Pairs
    end.

dictExtend(Id, Pairs, Dict) ->
    case orddict:find(Id, Dict) of
	error ->
	    orddict:store(Id, Pairs, Dict);
	{ok, Existing} ->
	    orddict:store(Id, Existing++Pairs, Dict)
    end.

dictEmpty() ->
    orddict:new().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Preparing for capture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec rctPrepare([trapPattern()], [trapPattern()]) -> [{atom(), any()}].

rctPrepare(Expected, Allowed) ->
    Options = [{allowed_traps, Allowed}],
    rct_snmpmgr:wait_for_traps(Expected, Options, stretching(30, s)),
    [].


-spec netSnmpPrepare([trapPattern()], [trapPattern()]) -> [{atom(), any()}].

netSnmpPrepare(_Expected, _Allowed) ->
    Count = getLinesCount(),
    [{linesCount, Count}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verifying captured traps
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec rctVerify([trapPattern()], [trapPattern()], [{atom(), any()}]) ->
	  ok | {failure, any()}.

rctVerify(_Expected, _Allowed, _Data) ->
    case rct_snmpmgr:check_traps() of
	ok ->
	    ct:pal("expected traps were received by RCT/OTP trap receiver",
		   []),
	    ok;
	Nok ->
	    ct:pal("expected traps NOT received, reason: ~p",
		   [Nok]),
	    {failure, {traps_not_received, Nok}}
    end.


%%% ----------------------------------------------------------
%%% @doc Verify traps captured by the on-board snmptrapd
%%% instance.
%%% @end
%%% ----------------------------------------------------------

-spec netSnmpVerify([trapPattern()], [trapPattern()], [{atom(), any()}]) ->
	  ok | {failure, any()}.

netSnmpVerify(Expected, Allowed, Data) ->
    SkipLines = proplists:get_value(linesCount, Data, 0),
    Lines = getLines(SkipLines),
    ReceivedTrapsRaw = [string:tokens(Line, "\t")||Line <- Lines],
    ReceivedTrapsBySeqno =
	orddict:from_list(
	  [{trapOrderingKey(Trap), Trap}
	  ||Trap <- ReceivedTrapsRaw]),
    ReceivedTraps =
	[Trap ||{_, Trap} <- orddict:to_list(ReceivedTrapsBySeqno)],
    try
	checkTraps(Expected, ReceivedTraps, Allowed)
    catch
	throw:ExData ->
	    {failure, ExData};
	ExType:ExData ->
	    {failure, {internal_error, {ExType, ExData}}}
    end.


%%% ----------------------------------------------------------
%%% @doc Check that the expected sequence of traps
%%% (specified by sparse patterns) and the actual traps
%%% are in agreement, otherwise fail. This function can
%%% be used to analyze traps captured by the on-board
%%% trap receiver. Currently not in use.
%%%
%%% Only value types handled (yet) are atom and string.
%%% @end
%%% ----------------------------------------------------------

-type trapPattern() :: [{atom(), atom() | string()}].
-type trap() :: [string()].

-type alarm() :: {atom, string(), [{atom(), any(), any()}]}.
-type pattern() :: [{atom(), string()}].

-spec checkTraps([trapPattern()], [trap()], [trapPattern()]) -> ok.

checkTraps([E|Em], [A|Am], Allowed) ->
    case match(E,A) of
	true ->
	    ct:log("Net-SNMP manager, expected trap:~n~p", [A]),
	    checkTraps(Em, Am, Allowed);
	_ ->
	    case someMatch(A, Allowed) of
		true ->
		    checkTraps([E|Em], Am, Allowed);
		_ ->
		    ct:pal("Net-SNMP manager, unexpected trap:~n~p", [A]),
		    throw(unexpected_trap)
	    end
    end;

checkTraps([], [A|Am], Allowed) ->
    case someMatch(A, Allowed) of
	true ->
	    checkTraps([], Am, Allowed);
	_ ->
	    ct:pal("Net-SNMP manager, unexpected trap:~n~p", [A]),
	    throw(unexpected_trap)
    end;

checkTraps([_|_]=E, [], _Allowed) ->
    ct:pal("Net-SNMP manager, traps not received -~n~p", [E]),
    throw(traps_not_received);

checkTraps([], [], _Allowed) ->
    ct:pal("Net-SNMP manager, expected traps received", []),
    ok.


%%% ----------------------------------------------------------
%%% @doc Returns true is some of the given patterns
%%% match the given trap.
%%% @end
%%% ----------------------------------------------------------

-spec someMatch(trap(), [trapPattern()]) -> boolean().

someMatch(A, [U|Um]) ->
    case match(U,A) of
	true ->
	    true;
	_ ->
	    someMatch(A, Um)
    end;

someMatch(_A, []) ->
    false.


%%% ----------------------------------------------------------
%%% @doc Returns true if the given pattern matches
%%% the given trap.
%%% @end
%%% ----------------------------------------------------------

match([{Key, Value}|More], Trap) ->
    matchHelper(Key, Value, Trap) andalso match(More, Trap);

match([],_Trap) ->
    true.


matchHelper(Key, Value, [Tel|More]) ->
    case trapElementLookup(Key, Tel) of
	undefined ->
	    matchHelper(Key, Value, More);
	{ok, Captured} ->
	    ValueS =
		if
		    Key =:= type ->
			atom_to_list(Value);
		    true ->
			Value
		end,
	    if
		ValueS =:= Captured ->
		    true;
		true ->
		    matchHelper(Key, Value, More)
	    end
    end;

matchHelper(_Key, _Value, []) ->
    false.


%%% ----------------------------------------------------------
%%% @doc Returns a key by which traps can be ordered and
%%% duplicates killed. Alerts are ordered before alarms.
%%% "Heartbeats" are weird as they have dual sequence numbers.
%%% @end
%%% ----------------------------------------------------------

-spec trapOrderingKey([string()]) -> {string(), integer(), integer()}.

trapOrderingKey(Trap) ->
    case trapLookup(type, Trap) of
	undefined ->
	    {"9_UNKNOWN", 0, 0};
	{ok, Type} ->
	    Class = alarmOrAlert(Type, Trap),
	    case Class of
		"1_ALERT" ->
		    case trapLookup(eriAlarmAlertLastSequenceNo, Trap) of
			undefined ->
			    {"7_UNKNOWN_ALERT", 0, 0};
			{ok, SeqNoAlert} ->
			    {Class, 0, SeqNoAlert}
		    end;
		"2_ALARM" ->
		    case trapLookup(eriAlarmActiveLastSequenceNo, Trap) of
			undefined ->
			    {"8_UNKNOWN_ALARM", 0, 0};
			{ok, SeqNoAlarm} ->
			    {Class, SeqNoAlarm, 0}
		    end;
		"3_HEARTBEAT" ->
		    SeqNoAlarm =
			case trapLookup(eriAlarmActiveLastSequenceNo, Trap) of
			    undefined ->
				0;
			    {ok, SeqNo1} ->
				SeqNo1
			end,
		    SeqNoAlert =
			case trapLookup(eriAlarmActiveLastSequenceNo, Trap) of
			    undefined ->
				0;
			    {ok, SeqNo2} ->
				SeqNo2
			end,
		    {Class, SeqNoAlarm, SeqNoAlert};
		_Other ->
		    {Class, 0, 0}
	    end
    end.


-spec alarmOrAlert(string(), any()) -> string().

alarmOrAlert("eriAlarmHeartBeatNotif", _Trap) -> "3_HEARTBEAT";

alarmOrAlert("eriAlarmIndeterminate", _Trap) -> "2_ALARM";
alarmOrAlert("eriAlarmWarning", _Trap) ->       "2_ALARM";
alarmOrAlert("eriAlarmMinor", _Trap) ->         "2_ALARM";
alarmOrAlert("eriAlarmMajor", _Trap) ->         "2_ALARM";
alarmOrAlert("eriAlarmCritical", _Trap) ->      "2_ALARM";
alarmOrAlert("eriAlarmCleared", _Trap) ->       "2_ALARM";

alarmOrAlert("eriAlarmIndAlert", _Trap) ->      "1_ALERT";
alarmOrAlert("eriAlarmWarnAlert", _Trap) ->     "1_ALERT";
alarmOrAlert("eriAlarmMinorAlert", _Trap) ->    "1_ALERT";
alarmOrAlert("eriAlarmMajorAlert", _Trap) ->    "1_ALERT";
alarmOrAlert("eriAlarmCriticalAlert", _Trap) -> "1_ALERT";

alarmOrAlert(Type, Trap) ->
    ct:pal("WARNING: unexpected notification, type: ~p, notification: ~p",
	   [Type, Trap]),
    "9_OTHER".


%%% ----------------------------------------------------------
%%% @doc Returns a string or integer value for the given key.
%%% Only certain SNMP data types are supported.
%%% @end
%%% ----------------------------------------------------------

-spec trapLookup(atom(), [string()]) -> {ok, string()|integer()} | undefined.

trapLookup(Key, [Tel|More]) ->
    case trapElementLookup(Key, Tel) of
	undefined ->
	    trapLookup(Key, More);
	{ok, _}=Result ->
	    Result
    end;

trapLookup(_Key, []) ->
    undefined.


%%% ----------------------------------------------------------
%%% @doc Returns a string or integer value for the given key.
%%% Only certain SNMP data types are supported; check the source
%%% below.
%%% @end
%%% ----------------------------------------------------------

-spec trapElementLookup(atom(), string()) -> {ok, string()|integer()} | undefined.

trapElementLookup(type, Tel) ->
    % io:format("trying ~s -~n", [Tel]),
    RE = "SNMPv2-SMI::snmpModules.1.1.4.1.0 = OID: ERICSSON-ALARM-MIB::(.*)",
    case re:run(Tel, RE, [anchored]) of
	{match, [{_, _}, {P, _L}]} ->
	    Captured = string:substr(Tel, P+1),
	    {ok, Captured};
	_ ->
	    undefined
    end;

trapElementLookup(Key, Tel) ->
    RE = lists:flatten(io_lib:format("ERICSSON-ALARM-MIB::~w.0 = STRING: (.*)", [Key])),
    case re:run(Tel, RE, [anchored]) of
	{match, [{_, _}, {P, _L}]} ->
	    Captured = string:substr(Tel, P+1),
	    {ok, Captured};
	_ ->
	    RE2 = lists:flatten(io_lib:format("ERICSSON-ALARM-MIB::~w.0 = Gauge32: (.*)", [Key])),
	    case re:run(Tel, RE2, [anchored]) of
		{match, [{_, _}, {P, _L}]} ->
		    Captured = list_to_integer(string:substr(Tel, P+1)),
		    {ok, Captured};
		_ ->
		    undefined
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Alarms validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validateAlarms(AlarmPatterns, IgnorePatterns) ->
    FmAlarm = 'FmAlarm',
    Fm = {'Fm', [FmAlarm]},
    SystemFunctions = {'SystemFunctions', [Fm]},
    Filter = {'ManagedElement', [SystemFunctions]},
    NetconfResponse =
	doNetconf(
	  fun(F) ->
		  ct_netconfc:get(?NETCONF_SESSION, F)
	  end,
	  [Filter]),

    case NetconfResponse of
	{ok, [Tree]} ->
	    Instances = filterFlatten(Tree),
	    ct:pal("alarm instances present -~n~p", [detox(Instances)]),
	    Inst2 = allPatternsMustMatch(AlarmPatterns, Instances),
	    Inst3 = ignoreAlarms(IgnorePatterns, Inst2),
	    if
		Inst3 =/= [] ->
		    ct:fail("unexpected alarms present -~n~p",
			    [detox(Inst3)]);
		true ->
		    ok
	    end;
	{error, ErrorItems} ->
	    % no FmAlarm instance is treated as an error, check if this
	    % is what we actually have
	    SortedErrorItems = lists:sort(ErrorItems),
	    case SortedErrorItems of
		[
		 {'error-message',
		  [{xmlns, _}, {'xml:lang',"en"}],
		  ["MO: ManagedElement,SystemFunctions,Fm,FmAlarm is not available (no instance)."]},
		 {'error-severity',
		  [{xmlns, _}],
		  ["error"]},
		 {'error-tag',
		  [{xmlns, _}],
		  ["data-missing"]},
		 {'error-type',
		  [{xmlns, _}],
		  ["application"]}] ->
		    Instances = [],
		    ct:pal("alarm instances present -~n~p", [detox(Instances)]),
		    Inst2 = allPatternsMustMatch(AlarmPatterns, Instances),
		    Inst3 = ignoreAlarms(IgnorePatterns, Inst2),
		    if
			Inst3 =/= [] ->
			    ct:fail("unexpected alarms present -~n~p",
				    [detox(Inst3)]);
			true ->
			    ok
		    end;
		Other ->
		    ct:fail("unexpected NETCONF result: ~p", [Other])
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Suppress a string that would be interpreted as an
%%% error in local CI.
%%% @end
%%% ----------------------------------------------------------

detox(Instances) ->
    [{RdnName, RdnValue,
      lists:map(
	fun({eventType,[],["PROCESSINGERRORALARM"]}) ->
		{eventType,[],["PROCESSINGERR@RALARM"]};
	   ({originalSeverity,[],["WARNING"]}) ->
		{originalSeverity,[],["W@RNING"]};
	   ({activeSeverity,[],["WARNING"]}) ->
		{activeSeverity,[],["W-RNING"]};
	   (Other) ->
		Other
	end,
	TupleList)}
     ||{RdnName, RdnValue, TupleList} <- Instances].


%%% ----------------------------------------------------------
%%% @doc Returns a reduced list of alarms where those that
%%% match the given patterns are removed. Every pattern
%%% is required to match a single alarm instance.
%%% @end
%%% ----------------------------------------------------------
-spec allPatternsMustMatch([pattern()], [alarm()]) -> [alarm()].

allPatternsMustMatch(Patterns, Alarms) ->
    lists:foldl(
      fun(Pattern, AlarmsLeft) ->
	      matchSomeAlarm(Pattern, AlarmsLeft, mandatory)
      end,
      Alarms,
      Patterns).


%%% ----------------------------------------------------------
%%% @doc Returns a reduced list where alarms that match some
%%% of the given patterns are removed.
%%% @end
%%% ----------------------------------------------------------
-spec ignoreAlarms([pattern()], [alarm()]) ->  [alarm()].

ignoreAlarms(Patterns, Alarms) ->
    lists:foldl(
      fun(Pattern, Decc) ->
	      matchSomeAlarm(Pattern, Decc, optional)
      end,
      Alarms,
      Patterns).


%%% ----------------------------------------------------------
%%% @doc Returns a reduced list if the given alarm pattern matches
%%% one of the given alarm instances.
%%% For Mode =:= mandatory the pattern must match exactly one
%%% alarm instance.
%%% For Mode =:= optional the number of matches is not significant.
%%% @end
%%% ----------------------------------------------------------
-spec matchSomeAlarm(pattern(), [alarm()], mandatory|optional) -> [alarm()].

matchSomeAlarm(Pattern, Alarms, Mode) ->
    {Result, FinalCount} =
	lists:foldl(
	  fun(Alarm, {Acc, Count}) ->
		  case matchAlarm(Pattern, Alarm) of
		      true ->
			  {Acc, Count+1};
		      false ->
			  {Acc++[Alarm], Count}
		  end
	  end,
	  {[], 0},
	  Alarms),
    if
	Mode =:= optional ->
	    Result;
	FinalCount =:= 0 ->
	    ct:fail("no alarm matches pattern: ~p -~nalarms: ~p",
		    [Pattern, Alarms]);
	FinalCount > 1 ->
	    ct:fail("pattern ~p matches multiple alarms -~n~p",
		    [Pattern, Alarms]);
	true ->
	    Result
    end.


%%% ----------------------------------------------------------
%%% @doc Returns true if the given alarm pattern matches the
%%% given alarm instance.
%%% @end
%%% ----------------------------------------------------------

-spec matchAlarm(pattern(), alarm()) -> boolean().

matchAlarm(Pattern, Alarm) ->
    case Alarm of
	{'FmAlarm', _, PP} ->
	    PPReduced =
		[{Key, Value}||{Key, _, [Value]} <- PP],
	    lists:all(
	      fun(X) ->
		      lists:member(X, PPReduced)
	      end,
	      Pattern);
	_ ->
	    false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Additional Info validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec verifyAddInfo() -> [string()].

verifyAddInfo() ->
    FmAlarm = 'FmAlarm',
    Fm = {'Fm', [FmAlarm]},
    SystemFunctions = {'SystemFunctions', [Fm]},
    Filter = {'ManagedElement', [SystemFunctions]},
    NetconfResponse =
	doNetconf(
	  fun(F) ->
		  ct_netconfc:get(?NETCONF_SESSION, F)
	  end,
	  [Filter]),
    verifyAddInfo(NetconfResponse).


%%% ----------------------------------------------------------
%%% @doc Verify additional info. The hard-coded values here
%%% must match the hard-coded values in IFT_CAX*/csrc/test_ntfi.c
%%% and also the hard-coded choice of alarms in the
%%% test_raise_clear_with_add_info testcase. A list of failure
%%% descriptors are returned.
%%% @end
%%% ----------------------------------------------------------

verifyAddInfo({ok, [Tree]}) ->
    Instances = flatten(Tree),
    [{_, _, Attrs}] = getAlarmInstances("mock-alarm-65504", Instances),
    ct:log("########## Attrs: ~p~n", [Attrs]),
    Failures =
	lists:append(
	  [validateAddInfo("110",  ?TESTVALUE_STRING_1, Attrs),

	   validateAddInfo("CI",   ?TESTVALUE_CORR_INFO_1, Attrs),
	   validateAddInfo("alarmId",  ?TESTVALUE_ALARM_ID_1, Attrs),
	   validateAddInfo("UUID", ?TESTVALUE_UUID_1, Attrs),
	   validateAddInfo("DN3",  ?TESTVALUE_DN3_1, Attrs),

	   validateAddInfo(?TESTVALUE_INFOID0_NAME_1, ?TESTVALUE_INFOID0_VALUE_1, Attrs),
	   validateAddInfo("0",                       ?TESTVALUE_INFOID0_2, Attrs),
	   validateAddInfo(?TESTVALUE_INFOID0_NAME_3, ?TESTVALUE_INFOID0_VALUE_3, Attrs)
	  ]),
		[lists:flatten(
		   io_lib:format("name: ~s, expected value: ~s, actual: ~p", [N, E, A]))
		   ||{N, E, A} <- Failures];

verifyAddInfo({ok, Other}) ->
    ct:fail("unexpected NETCONF response: c", [Other]);

verifyAddInfo({error, Reason}) ->
    ct:fail("NETCONF failure: ~p", [Reason]).


%%% ----------------------------------------------------------
%%% @doc Verify that the given Name/Value pair is found
%%% in the given list of attributes. Returns [] if
%%% verified, or [{Name, ExpectedValue, ActualValue}] or
%%% [{Name, ExpectedValue, "NO_VALUE_AVAILABLE"}]
%%% @end
%%% ----------------------------------------------------------

%% Name with expected value not found
validateAddInfo(Name, ExpValue, [], []) ->
    [{Name, ExpValue, ["NO_VALUE_AVAILABLE"]}];

%% Name found but not with the expected value
validateAddInfo(Name, ExpValue, [], ActValues) ->
    [{Name, ExpValue, ActValues}];

%% Name and value matches, return [] as in success
validateAddInfo(Name,
                ExpValue,
                [{additionalInfo,
                  [{struct, "AdditionalInformation"}],
                  [{name, [], [Name]}, {value, [], [ExpValue]}]} | _MoreAttrs],
                _ActValues) ->
    [];

%% Name matches but not the value, add value to list of actual values and keep looking
validateAddInfo(Name,
                ExpValue,
                [{additionalInfo,
                  [{struct, "AdditionalInformation"}],
                  [{name, [], [Name]}, {value, [], [ActValue]}]} | MoreAttrs],
                ActValues) ->
    validateAddInfo(Name, ExpValue, MoreAttrs, [ActValue | ActValues]);

%% Try next attr
validateAddInfo(Name, ExpValue, [_|MoreAttrs], ActValues) ->
    validateAddInfo(Name, ExpValue, MoreAttrs, ActValues).

%% For convenience
validateAddInfo(Name, ExpValue, [_|MoreAttrs]) ->
    validateAddInfo(Name, ExpValue, MoreAttrs, []).


doConfiguredSeverity(AlarmTypeId, Severity) when Severity =:= "CRITICAL" orelse
						     Severity =:= "MAJOR" orelse
						     Severity =:= "MINOR" orelse
						     Severity =:= "WARNING" orelse
						     Severity =:= empty ->
    {Operation, Value} =
	if
	    Severity =:= empty ->
		{[{'xmlns:nc', "urn:ietf:params:xml:ns:netconf:base:1.0"},
		  {'nc:operation', "delete"}],
		 []};
	    true ->
		{[],
		 [Severity]}
	end,

	doNetconf(
	  fun(T, Oper, V) ->
		  ct_netconfc:edit_config(
		    nc1,
		    running,
		    {'ManagedElement',
		     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		     [{managedElementId,[],["1"]},
		      {'SystemFunctions',
		       [{systemFunctionsId,[],["1"]},
			{'Fm',
			 [{xmlns,"urn:com:ericsson:ecim:ComFm"}],
			 [{fmId,[],["1"]},
			  {'FmAlarmModel',[{xmlns,"urn:com:ericsson:ecim:ComFmAlarmModel"}],
			   [{fmAlarmModelId,[],["1"]},
			    {'FmAlarmType',
			     [{xmlns,"urn:com:ericsson:ecim:ComFmAlarmType"}],
			     [{fmAlarmTypeId,[],[T]},
			      {configuredSeverity,Oper,V}
			     ]}]}]}]}]}),

		  ok
	  end,
	  [AlarmTypeId, Operation, Value]);

doConfiguredSeverity(_AlarmTypeId, _Severity) ->
    ct:fail("doConfiguredSeverity: unknown AlarmTypeId or configuredSeverity. ", []).


%%% ----------------------------------------------------------
%%% @doc Select the FmAlarm instances that match the given
%%% specific problem (typically just one instance).
%%% @end
%%% ----------------------------------------------------------

getAlarmInstances(SpecificProblem, Instances) ->
    lists:append(
      [case lists:member({specificProblem, [], [SpecificProblem]}, Attrs) of
	   true ->
	       [Inst];
	   false ->
	       []
       end
       ||{'FmAlarm', _, Attrs}=Inst <- Instances]).


-spec filterFlatten(any()) -> list().

filterFlatten(Tree) ->
    List = flatten(Tree),
    lists:keydelete(
      'ManagedElement', 1,
      lists:keydelete(
	'SystemFunctions', 1,
	lists:keydelete(
	  'Fm', 1, List))).




%%% ----------------------------------------------------------
%%% @doc Converts a given tree in simple_xml() format to a flat
%%% list of pseudo-instances, each having the format
%%% {MoClassA, RdnValueS, AttrList} where each Attr in the list
%%% has the format {AttrNameA, Options, Values}.
%%% For a univalued attribute the options list is empty and the
%%% Values list is of length 1. For a struct-valued attribute
%%% the options list is [{struct, StructNameS}] and the values
%%% list is [{StructMemberNameA, [], [Value]}].
%%% @end
%%% ----------------------------------------------------------

flatten({Tag, _Attrs, Content}) ->
    [{Tag, getRdnValue(Content), getAttrs(Content)} | flattenContent(Content)];

flatten({Tag, Content}) ->
    [{Tag, getRdnValue(Content), getAttrs(Content)} | flattenContent(Content)];

flatten(Tag) when is_atom(Tag) ->
    [{Tag, "rdnUnknown", []}].


flattenContent(Content) ->
    Children = getChildren(Content),
    lists:append([flatten(Child)|| Child <- Children]).


getChildren(Content) ->
    lists:filter(fun({X, _, _}) ->
			 [C|_] = atom_to_list(X),
			 C >= $A andalso C =< $Z end,
		 Content).


getAttrs(Content) ->
    tl(lists:filter(fun({X, _, _}) ->
			    [C|_] = atom_to_list(X),
			    C < $A orelse C > $Z end,
		    Content)).


getRdnValue([{_, _, [RdnS]}|_]) ->
    RdnS;

getRdnValue(_) ->
    "rdnNotFound".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RCT/OTP SNMP manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% @doc Verify that the RCT/OTP SNMP manager is operational.
%%% This test always succeeds; mild warnings are printed to
%%% the console.
%%% @end
%%% ----------------------------------------------------------

do_verify_snmp_mgr(Config) ->
    Targets = ?config(snmpTargets, Config),
    case lists:append(
	   [[T]
	   ||#snmpTarget{type=Type}=T <- Targets, Type =:= "RCT/OTP"]) of
	[] ->
	    ok;
	[_, _|_] ->
	    ct:fail("cannot handle multiple RCT/OTP SNMP managers (yet)");
	[#snmpTarget{ip=MgrIp, port=MgrPort}] ->
	    Text = ?ADD_TEXT_ANYBODY_THERE,
	    %% {A1,A2,A3} = os:timestamp(),
	    %% random:seed(A1, A2, A3),
	    %% RandomDigits = random:uniform(999999999),
	    RandomDigits = rand:uniform(999999999),
	    TrapsExpected =
		[[{type, eriAlarmIndAlert},
		  {eriAlarmNObjAdditionalText, Text},
		  {eriAlarmNObjResourceId, RandomDigits}]],
	    Options = [other_alarms_allowed, other_alerts_allowed],
	    rct_snmpmgr:wait_for_traps(TrapsExpected, Options, stretching(10, s)),

	    NetstatCommand = "netstat --udp --numeric --listening",
	    NetstatCommandResult = string:tokens(os:cmd(NetstatCommand), "\n"),
	    Pattern =
		lists:flatten(
		  io_lib:format("udp[ ]+[0-9]+[ ]+[0-9]+[ ]+[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+:~s[ ]", [MgrPort])),
	    NetstatCommandResultFiltered =
		lists:filter(
		  fun(L) -> re:run(L, Pattern, [anchored]) =/= nomatch end,
		  NetstatCommandResult),
	    case NetstatCommandResultFiltered of
		[] ->
		    ct:pal("unexpected: nothing listens to port: ~w", [MgrPort]);
		[NetstatEntry] ->
		    ct:pal("expected single netstat entry: ~s", [NetstatEntry]);
		Multiple ->
		    ct:pal("multiple netstat entries, please investigate -~n~p", [Multiple])
	    end,

	    Command =
		lists:flatten(
		  io_lib:format(
		    "env SNMP_PERSISTENT_FILE=/dev/null snmpinform "
		    "-v 2c "
		    "-c public "
		    "-M ~s/test/lib/rct-snmpmgr/mibs:/usr/share/snmp/mibs "
		    "-m ERICSSON-TOP-MIB:ERICSSON-ALARM-MIB "
		    "~s:~s "
		    "'' "
		    "ERICSSON-ALARM-MIB::eriAlarmIndAlert "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertManagedObject.0 s qwerty "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertMajorType.0 u 0 "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertMinorType.0 u 0 "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertSpecificProblem.0 s qwerty "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertLastSequenceNo.0 u 0 "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertEventType.0 i 0 "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertEventTime.0 x 07DE091B000000002B0200 "
		    "ERICSSON-ALARM-MIB::eriAlarmAlertProbableCause.0 i 0 "
		    "ERICSSON-ALARM-MIB::eriAlarmNObjAdditionalText.0 s '~s' "
		    "ERICSSON-ALARM-MIB::eriAlarmNObjMoreAdditionalText.0 i 0 "
		    "ERICSSON-ALARM-MIB::eriAlarmNObjResourceId.0 i ~w ",
		    [os:getenv("RCT_TOP"), MgrIp, MgrPort, Text, RandomDigits])),

	    ct:pal("executing: ~p", [Command]),
	    Result = os:cmd(Command),

	    ?IF(Result =/= [],
		ct:pal("unexpected command response: ~p", [string:tokens(Result, "\n")])),

	    case rct_snmpmgr:check_traps() of
		ok ->
		    ct:pal("the SNMP manager receives INFORM PDUs correctly", []);
		{error, Reason} ->
		    ct:pal("please investigate: the SNMP manager has problems: ~P", [Reason, 77]);
		Other ->
		    ct:pal("WARNING: unspecific problem with SNMP: ~P", [Other, 77])
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NETCONF operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% @doc Configure the given SNMP targets in COM.
%%% @end
%%% ----------------------------------------------------------

-spec configSnmpTargets([#snmpTarget{}]) -> ok.

configSnmpTargets(SnmpTargets) ->
    doNetconf(
      fun(Targets) ->
	      ct_netconfc:edit_config(
		?NETCONF_SESSION,
		running,
		createSnmpTargetsReq(Targets)),
	      [
	       if
		   Mode =:= "TRAP" ->
		       ct:pal("configured target /~s/: ~s:~s, mode: ~s",
			      [RdnValue, Ip, Port, Mode]);
		   true ->
		       ct:pal("configured target /~s/: ~s:~s, mode: ~s, "
			      "timeout: ~s ticks, retries: ~s",
			      [RdnValue, Ip, Port, Mode, Ticks, Retries])
	       end

	       ||#snmpTarget{rdnValue=RdnValue,
			     ip=Ip,
			     port=Port,
			     mode=Mode,
			     timeoutTicks=Ticks,
			     maxRetries=Retries} <- Targets]
      end,
      [SnmpTargets]),

    delay(?COM_CONFIG_WAIT, ms),
    ok.


%%% ----------------------------------------------------------
%%% @doc Deconfigure the given SNMP targets.
%%% @end
%%% ----------------------------------------------------------

deconfigSnmpTargets(SnmpTargets) ->
    doNetconf(
      fun(Targets) ->
	      RdnValues = [RdnValue||#snmpTarget{rdnValue=RdnValue} <- Targets],
	      ct_netconfc:edit_config(
		?NETCONF_SESSION,
		running,
		deleteSnmpTargetReq(RdnValues)),
	      ct:pal("deconfigured targets: ~p", [RdnValues])
      end,
      [SnmpTargets]).


createSnmpTargetsReq(SnmpTargets) ->
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	{'SysM',
	 [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	 [{sysMId,[],["1"]},
	  {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	   [{snmpId,[],["1"]},
	    {administrativeState,[],["UNLOCKED"]}
		|
		[snmpTargetInst(Mode, RdnValue, Ip, Port, TimeoutTicks, MaxRetries)
		   || #snmpTarget{
				  rdnValue=RdnValue,
				  ip=Ip,
				  port=Port,
				  mode=Mode,
				  timeoutTicks=TimeoutTicks,
				  maxRetries=MaxRetries
				 } <- SnmpTargets]
	   ]}]}]}]}.


snmpTargetInst("INFORM", RdnValue, MgrIp, MgrPort, Timeout, MaxRetries) ->
    {'SnmpTargetV2C',
     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
     [{snmpTargetV2CId,[],[RdnValue]},
      {address,[],[MgrIp]},
      {informTimeout,[],[Timeout]},
      {informRetryCount,[],[MaxRetries]},
      {transportMethod,[],["INFORM"]},
      {community,[],["public"]},
      {port,[],[MgrPort]}]};

snmpTargetInst("TRAP", RdnValue, MgrIp, MgrPort, _Timeout, _MaxRetries) ->
    {'SnmpTargetV2C',
     [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
     [{snmpTargetV2CId,[],[RdnValue]},
      {address,[],[MgrIp]},
      {community,[],["public"]},
      {transportMethod,[],["TRAP"]},
      {port,[],[MgrPort]}]}.


%%% ----------------------------------------------------------
%%% @doc Deletes the instances specified by the given
%%% RDN values.
%%% @end
%%% ----------------------------------------------------------

-spec deleteSnmpTargetReq([string()]) ->  {atom(), list(), list()}.

deleteSnmpTargetReq(RdnValues) ->
    Insts = [{'SnmpTargetV2C',
	      [{xmlns,"urn:com:ericsson:ecim:ComSnmp"},
	       {'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
	       {'nc:operation',"delete"}],[{snmpTargetV2CId,[],[RdnValue]}]}
	     || RdnValue <- RdnValues],
    {'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
	{'SysM',
	 [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
	 [{sysMId,[],["1"]},
	  {'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
	   [{snmpId,[],["1"]}
		|Insts]}]}]}]}.


%%% ----------------------------------------------------------
%%% @doc Executes a function within a NETCONF session.
%%% @end
%%% ----------------------------------------------------------

-spec doNetconf(fun(), list()) -> any().

doNetconf(Fun, Args) ->
    try
	{ok,_} = ct_netconfc:open(?NETCONF_SESSION, []),
	Result = apply(Fun, Args),
	ok = ct_netconfc:close_session(?NETCONF_SESSION),
	Result
    catch
	ExType:ExData ->
	    ct_netconfc:close_session(?NETCONF_SESSION),
	    ct:fail("netconf operation failed: ~p", [{ExType, ExData}])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NTFI session
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% @doc Receive the handle encoded as 2 32-bit unsigned integers.
%%% We hope that the high bits are all zero, or fail the test case.
%%% @end
%%% ----------------------------------------------------------

initialize() ->
    {ok, HighBits, LowBits} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_INITIALIZE, {}),
    Handle = (HighBits bsl 32) + LowBits,
    if
	HighBits =/= 0 ->
	    ct:fail("handle not reasonable: ~p", [Handle]);
	true ->
	    ct:print("NTF session initialized, handle: ~p", [Handle]),
	    Handle
    end.


finalize(Handle) ->
    {ok} =
	rct_proxy:send_proxy(?NODE, ?CHILD, ?NTFI_FINALIZE, {Handle}),
    ct:print("NTF session finalized, handle: ~p", [Handle]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUT access
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% @doc Get the on-board port number. This may be something
%%% other than 11001 on the simulator, so we need to analyze
%%% the snmptrapd log. We trust that the first 100 lines
%%% have the answer.
%%%
%%% It is assumed that the IFT proxy is started.
%%%
%%% In case the port cannot be deduced then the string
%%% "unknown" is returned.
%%% @end
%%% ----------------------------------------------------------

-spec getOnBoardPort() -> string().

getOnBoardPort() ->
    LinesLimit = 100,
    Lines = getLines(0, LinesLimit),
    HasFailure =
	lists:any(
	  fun(Line) ->
		  case re:run(Line, "TESTCASE MESSAGE: failed") of
		      {match, _} ->
			  true;
		      _ ->
			  false
		  end
	  end,
	  Lines),
    case HasFailure of
	true ->
	    ct:pal("failed to deduce actual port for on-board trap receiver,~n"
		   "collected lines: ~p",
		   [Lines]),
	    "unknown";
	_ ->
	    Port =
		lists:foldl(
		  fun(Line, Acc) ->
			  case re:run(Line, "TESTCASE MESSAGE: success, using port: ([0-9]+)") of
			      nomatch ->
				  Acc;
			      {match, [_, {Pos, Len}]} ->
				  string:substr(Line, Pos+1, Len)
			  end
		  end,
		  "unknown",
		  Lines),
	    if
		Port =:= "unknown" ->
		    ct:pal("unsuccessful search for port for on-board trap receiver,~n"
			   "collected lines: ~p",
			   [Lines]),
		    "unknown";
		true ->
		    Port
	    end
    end.


-spec getLinesCount() -> non_neg_integer().

getLinesCount() ->
    {ok, Result} =
	rct_proxy:send_proxy(
	  ?NODE, ?CHILD, ?NTFI_COUNT_LINES, {"$LOG_DIR", ?TRAPRECEIVER_LOG}),
    Result.


-spec getLines(non_neg_integer()) -> [string()].

getLines(SkipLines) ->
    {ok, Result} =
	rct_proxy:send_proxy(
	  ?NODE, ?CHILD, ?NTFI_GET_TAIL_LINES, {"$LOG_DIR", ?TRAPRECEIVER_LOG, SkipLines}),
    Result.


-spec getLines(non_neg_integer(), non_neg_integer()) -> [string()].

getLines(SkipLines, MaxLines) ->
    case rct_proxy:send_proxy(
	   ?NODE, ?CHILD, ?NTFI_GET_TAIL_LINES,
	   {"$LOG_DIR", ?TRAPRECEIVER_LOG, SkipLines, MaxLines}) of
	{ok, Result} ->
	    Result;
	{error, _} ->
	    ct:pal("failed to read the trap receiver log", []),
	    []
    end.


iftStart() ->
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD, ?NTFI).


iftStop() ->
    {ok, client_stopped} = rct_proxy:stop_proxy(?NODE, ?CHILD).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% @doc Returns all IPv4 address for this host other
%%% than the loopback address.
%%% @end
%%% ----------------------------------------------------------

-spec externalIpAddresses() ->
	  [{integer(), integer(), integer(), integer()}].

externalIpAddresses() ->
    {ok, Ii} = inet:getifaddrs(),
    externalIpAddressesHelper(Ii, []).

externalIpAddressesHelper([{_, Tt} | Tail], Acc) ->
    Flags = proplists:get_value(flags, Tt, []),
    case lists:member(loopback, Flags) of
	true ->
	    externalIpAddressesHelper(Tail, Acc);
	_ ->
	    case proplists:lookup(addr, Tt) of
		none ->
		    externalIpAddressesHelper(Tail, Acc);
		{_, {_, _, _, _, _, _, _, _}} ->
		    externalIpAddressesHelper(Tail, Acc);
		{_, {A, B, C, D}} ->
		    externalIpAddressesHelper(Tail, [{A, B, C, D}|Acc])
	    end
    end;

externalIpAddressesHelper([], Acc) ->
    lists:reverse(Acc).


banner(TestCase) when is_atom(TestCase) ->
    ct:pal("=====<< ~w >>======================", [TestCase]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DN conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% @doc Translate IMM to MIM style. This function has limited
%%% hard-coded knowledge of MOM names such as TESTMOM. Actually
%%% the CS CMSI interface could be used instead; TODO
%%% @end
%%% ----------------------------------------------------------

immToMim(Dn) ->
    RawMimRdns = lists:reverse(string:tokens(Dn, ",")),
    lists:foldl(
      fun(X, A) -> if A =:= [] -> X; true ->  A++","++X end end,
      "ManagedElement=1",
      [
       begin
	   [RawRdnName, RdnValue] = string:tokens(RawMimRdn, "="),
	   RdnName =
	       case re:run(RawRdnName, "TESTMOM(.)(.*)Id$", [anchored]) of
		   {match, [{_, _}, {P, _}, {Q, Qlen}]} ->
		       string:to_upper(string:substr(RawRdnName, P+1, 1))++
			   string:substr(RawRdnName, Q+1, Qlen);
		   _ ->
		       case re:run(RawRdnName, "(.)(.*)Id$", [anchored]) of
			   {match, [{_, _}, {P, _}, {Q, Qlen}]} ->
			       string:to_upper(string:substr(RawRdnName, P+1, 1))++
				   string:substr(RawRdnName, Q+1, Qlen);
			   _ ->
			       ct:fail("DN translation failure for: ~s", [Dn])
		       end
	       end,
	   RdnName++"="++RdnValue
       end
       || RawMimRdn <- RawMimRdns]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testcase timing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------
%%% @doc Sleep for the given number of seconds, or
%%% milliseconds. The STRETCH macro affects the
%%% actual amount of sleep.
%%% @end
%%% ----------------------------------------------------------

delay(N, s) ->
    Millis = (N * 1000 * ?STRETCH) div 100,
    timer:sleep(Millis);

delay(N, ms) ->
    Millis = (N * ?STRETCH) div 100,
    timer:sleep(Millis).


%%% ----------------------------------------------------------
%%% @doc Sleep for the given number of milliseconds.
%%% @end
%%% ----------------------------------------------------------
delayFixed(N, ms) ->
    timer:sleep(N).


%%% ----------------------------------------------------------
%%% @doc A number of seconds, subject to stretching up and
%%% down. Since an integer value is returned the stretching
%%% becomes very coarse for small values of N.
%%% @end
%%% ----------------------------------------------------------

stretching(N, s) ->
    (N * ?STRETCH) div 100.




%%% ----------------------------------------------------------
%%% @doc Returns the version of the given module. The
%%% expected result, if the -vsn() directive is present,
%%% is an atom.
%%% @end
%%% ----------------------------------------------------------
-spec getVersion(module()) -> atom() | integer().

getVersion(Module) ->
    case code:which(Module) of
	Path when is_list(Path) ->
	    case beam_lib:version(Path) of
		{error, _, _} ->
		    'unknown';
		{ok, {_, [Version]}} ->
		    Version
	    end;
	_ ->
	    'unknown'
    end.


%%% ----------------------------------------------------------
%%% @doc Wait for the node to complete a warm restart.
%%% Not used, CONSIDER REMOVING
%%% @end
%%% ----------------------------------------------------------
waitForWarmRestart(Count) when Count =:= 0 ->
    ct:fail("waitForRestart: too many retries", []);

waitForWarmRestart(Count) ->
    timer:sleep(1000),
    try rct_rpc:call(rpc1, comsaLedControl, millisSinceWarmRestart, [], 1000) of
	ok ->
	    ct:print("waitForRestart: unexpected RPC execution path; will retry", []),
	    waitForWarmRestart(Count - 1);
	{badrpc, _}=X ->
	    ct:print("waitForRestart: bad RPC; will retry: ~p", [X]),
	    waitForWarmRestart(Count - 1);
	Millis when is_integer(Millis) andalso Millis < 10000 ->
	    ok;
	_Other ->
	    waitForWarmRestart(Count - 1)
    catch
	X:Y ->
	    ct:print("waitForRestart: caught: ~p", [{X, Y}]),
	    waitForWarmRestart(Count - 1)
    end.

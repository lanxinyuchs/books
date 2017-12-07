%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_memory_saf_ntf_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/3
%%%
%%% @doc == Measure memory when generate/cease Alarms using saf ntf==
%%% This Test Suite can be used on target enviroment.<br/>
%%%
%%%
%%% @end

-module(measure_memory_saf_ntf_SUITE).
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
%%% R2A/2      2013-04-24 etxivri     Created
%%% R2A/3      2013-05-20 etxivri     Updates of no_check when when generate and cease alarms.
%%%                                   reboot in a own internal function.
%%%                                   Added backgroundload
%%% R2A/4      2013-05-27 etxivri     Update check that alarm is not active, cli_check_alarm_is_inactive.
%%% R2A/5      2013-06-04 etxivri     Correted LOG_DIR
%%% R2A/6      2013-06-24 etxivri     Change in delete snmp config.
%%% R2A/7      2013-08-07 etxivri     Simplified check of snmp config created.
%%%                                   Updates due to one snmpd pid exist after installation.
%%% R2A/8      2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/10     2013-08-13 etxivri     Added disusage printed out in ct log.
%%% R2A/11     2013-09-06 etxivri     Corrected a fault in reboot.
%%% R2A/11     2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/13     2013-09-11 etxivri     Minor update.
%%% R2A/14     2013-10-01 etxivri     Changed cli command end to top, due to changes in com bd8.
%%% R2A/14     2013-11-07 etxivri     Updates to make cpulimit works on ARM.
%%% R2A/16     2013-11-18 etxivri     Upadates due to changed behaviour of Alarm
%%%                                   handling. Alarm need to be generated or
%%%                                   ceased 2sec. If alarm been togling 3times
%%%                                   within 60sec, then Alarm will be active
%%%                                   as togled alarm.
%%%                                   (can be ceased after 180sec)
%%% R2A/17     2013-11-25 etxivri     Update to handle, if active alarms exist
%%%                                   when TC starts.
%%% R2A/18     2013-11-29 etxivri     Added HW type on File name,
%%%                                   to separate the measured files.
%%% R2A/19     2014-01-16 etxkols     Support for dus5201.
%%% R2A/20     2014-02-25 etxkols     Update to take care if alarm that is
%%%                                   not checked is ceased
%%%                                   during test period, during test period.
%%% R2A/21     2014-02-28 etxkivri    Some minor updates.
%%% R2A/22     2014-03-06 etxkivri    Update check of snmpd pid
%%%                                   Update to handle write data to file when
%%%                                   other than Branch R2A is used.
%%% R2A/22     2014-03-06 etxkivri    Minor update.
%%% R2A/23     2014-03-31 etxkivri    Update due to snmp is configured at inst.
%%% R2A/25     2014-06-04 etxivri     Changed check for "du1 login" prompt to
%%%                                   "login:"
%%% R2A/27     2014-08-22 etxivri     Update to get disc use on arm
%%% R2A/28     2014-08-29 etxivri     Update due to changed alarm handling.
%%% R2A/29     2014-09-03 etxivri     Minor update.
%%% R2A/30     2014-09-16 etxivri     Update when use df on dus52
%%% R2A/31     2014-09-17 etxivri     Update due to snmp is automatic created at install.
%%% R3A/1      2014-12-11 etxivri     Minor update when disconnect after reboot.
%%% R3A/2      2014-12-19 etxivri     Remove unnecessary reboot. And cleanup.
%%% R3A/3      2014-12-19 erarafo     Adapted to alarm minorType change
%%% R4A/1      2015-10-06 etxmlar     Allow 2 CPU:s to have lower background load
%%%                                   than expected.
%%% R4A/2      2015-10-06 etxmlar     cleanup.
%%% R4A/3      2016-02-12 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
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
	 meas_mem_1_hour_saf_ntf_alarm/1,
	 meas_mem_1_hour_saf_ntf_alarm_backgroundload/1,
	 meas_mem_10_hour_saf_ntf_alarm/1,
	 meas_mem_10_hour_saf_ntf_alarm_backgroundload/1,
	 meas_mem_48_hour_saf_ntf_alarm/1,
	 meas_mem_48_hour_saf_ntf_alarm_backgroundload/1,
	 meas_mem_150_hour_saf_ntf_alarm/1,
	 meas_mem_150_hour_saf_ntf_alarm_backgroundload/1
	]).

-define(NrOfCliUsers, 1). % Only one is needed to get active alarms.

%% Note at install on target is created, this result in that 5 targets will be used.
%% -define(NrSnmpTarget, 4). % Used in create snmp.
%% -define(NrSnmpTarget, 1). % Used in create snmp

-define(CLI_SessionNameList, [list_to_atom("cli"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfCliUsers)]).

-define(SnmpTarget_NameList, [list_to_atom("targ"++integer_to_list(N)) || N <-  lists:seq(1,?NrSnmpTarget)]).

-define(ALL_ALARM_NTF_MEASURED_FILE_1, "all_alarm_ntf_measured_memory_file_1.txt").
-define(ALL_ALARM_NTF_MEASURED_FILE_2, "all_alarm_ntf_measured_memory_file_2.txt").

-define(LOG_DIR, "/proj/rcs/measurements/meas_mem_ntf_alarm/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").

-define(CPULIMIT_FROM_PATH, "/proj/rcs/misc/cpulimit").
-define(CPULIMIT_FROM_PATH_ARM, " /proj/rcs/misc/cpulimit_arm/cpulimit").
-define(CPULIMIT_TO_PATH, "/home/sirpa/dev_patches/").

-define(TIME, 3000).  %% Time for Alarm to be generated or ceased.

%% -define(ClassNrList, ["1"]).
-define(ClassNrList, ["1", "2"]). %% Two MOs will generate coffe alarm
-define(CoffeAlarm, 9240559).     %% minorType=9240559

%% Two alarm on 5 snmp targets results in 10 traps
-define(GenTrapList, [[{type,eriAlarmMajor},
		       {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass1=1"},
		       {eriAlarmActiveMinorType,?CoffeAlarm}],
		      [{type,eriAlarmMajor},
		       {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass2=1"},
		       {eriAlarmActiveMinorType,?CoffeAlarm}]
		     ]).

-define(ClearTrapList, [[{type,eriAlarmCleared},
			 {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass1=1"},
			 {eriAlarmActiveMinorType,?CoffeAlarm}],
			[{type,eriAlarmCleared},
			 {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass2=1"},
			 {eriAlarmActiveMinorType,?CoffeAlarm}]
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
			[{type,eriAlarmHeartBeatNotif}]
		       ]
		      }] ).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    %% build up a list of CliHooks touples with differents Names.
    %% NetconfHooks = [{rct_netconf, list_to_atom("nc"++integer_to_list(N))} || N <- lists:seq(1,?NrOfNetConfUsers)],
    CliHooks = [{rct_cli, {list_to_atom("cli"++integer_to_list(N)), [manual_connect]}} || N <- lists:seq(1,?NrOfCliUsers)] ,

    %% ct:pal("CliHooks: ~p",[CliHooks]),
    [{timetrap, {hours, 200}}, % 200 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 {rct_snmpmgr,snmp1},
		 {rct_consserv,cs1},
                 {rct_rs232,console},
		 %% {rct_core,[]},
		 {rct_tlib, identifier}, %% some miscellaneous functions
		 %% {rct_safe_ntf_rpc,[{safe_debug_level, 0}, {finalize_on_fail, true}]},
		 {rct_safe_rpc,[{safe_services,
				 [{ntf, 0}]}, %% debuglevel 0
				{instance_name, rct_safe_ntf_rpc},
				{finalize_on_fail, true}]},
		 {rct_scp, [{1, node1}]},
		 {rct_core,[]},
		 %% {rct_netconf,{[{1, nc1, username, ssh_lmt_ipv4}], html}},

		 %% {rct_safe_ntf_rpc,[{targ_du_no, 1},
		 %% 		   {instance_name, ?INSTANCE_NAME},
		 %% 		    {safe_debug_level, 2},
		 %% 		    {finalize_on_fail, true}]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}} | CliHooks
		 %% lists:append(NetconfHooks, CliHooks)
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
    %% reboot_node(),
    wait_for_testnode_up(),
    %%%%
    %% Get testnode Beam Pids.
    %%%%
    {ok, TestNode} = rct_safe_ntf_rpc:get_testnode(rct_safe_ntf_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 5000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    %%%%
    %% Initialize NTF Handle
    %%%%
    ct:pal("Initialize NTF Handle", []),
    {ok, Handle, _} = rct_safe_ntf_rpc:initialize(undefined),
    ct:pal("### Handle: ~p",[Handle]),

    %% Config.
    [{handle, Handle}| Config].

%% @hidden
end_per_testcase(_TestCase, Config) ->
    %%%
    %% Finalize NTF Handle
    %%%%
    Handle = proplists:get_value(handle, Config),
    ct:pal("### Handle: ~p",[Handle]),
    ct:pal("Finalize NTF Handle ~p", [Handle]),
    ok = rct_safe_ntf_rpc:finalize(Handle),

    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up.", [Reason]),

	    A = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000, noprint),
	    D = rct_rpc:call(rpc, os, getpid, [], 10000),

	    ct:pal("### Com: ~p",[A]),
   	    ct:pal("### BeamPid: ~p",[D]),

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

    %%%%
    %% Clean up cpulimit, don't care if it has been used or not.!
    %% When start cpulimit with -z flag, then Only need to delete target process "cat".
    %%%%
    %% rct_rpc:call(rpc, os, cmd, ["pkill cpulimit"], 10000, noprint),
    %% timer:sleep(2000),
    rct_rpc:call(rpc, os, cmd, ["pkill cat"], 10000, noprint),
    rct_rpc:call(rpc, os, cmd, ["rm "++?CPULIMIT_TO_PATH++"cpulimit"], 10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),
    [] = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),

    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() ->
    [].

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
%% calls do_meas_check_memory_alarm with numbers of Hours that will perform the actual test.
%% Check active alarm and mem size will be done. <br/>
%% @spec meas_mem_1_hour_saf_ntf_alarm(Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_1_hour_saf_ntf_alarm(Config) ->
    Handle = proplists:get_value(handle, Config),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(1, LogPath, check, Handle).

%% With backgroundload.
meas_mem_1_hour_saf_ntf_alarm_backgroundload(Config) ->
    Handle = proplists:get_value(handle, Config),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(1, LogPath, check, Handle).

%%--------------------------------------------------------------------
%% @doc
%% calls do_meas_check_memory_alarm with numbers of Hours that will perform the actual test.
%% Check active alarm and mem size will be done. <br/>
%% @spec meas_mem_10_hour_saf_ntf_alarm(Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_10_hour_saf_ntf_alarm(Config) ->
    Handle = proplists:get_value(handle, Config),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(10, LogPath, check, Handle).

%% With backgroundload.
meas_mem_10_hour_saf_ntf_alarm_backgroundload(Config) ->
    Handle = proplists:get_value(handle, Config),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(10, LogPath, check, Handle).

%%--------------------------------------------------------------------
%% @doc
%% calls do_meas_check_memory_alarm with numbers of Hours that will perform the actual test.
%% No check of active alarms and mem size will be done. <br/>
%% @spec meas_mem_48_hour_saf_ntf_alarm(Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_48_hour_saf_ntf_alarm(Config) ->
    %% reboot_node(),
    Handle = proplists:get_value(handle, Config),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(48, LogPath, no_check, Handle).

%% With backgroundload.
meas_mem_48_hour_saf_ntf_alarm_backgroundload(Config) ->
    Handle = proplists:get_value(handle, Config),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(48, LogPath, no_check, Handle).
%%--------------------------------------------------------------------
%% @doc
%% calls do_meas_check_memory_alarm with numbers of Hours that will perform the actual test.
%% No check of active alarms and mem size will be done. <br/>
%% @spec meas_mem_150_hour_saf_ntf_alarm(_onfig) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_150_hour_saf_ntf_alarm(Config) ->
    Handle = proplists:get_value(handle, Config),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(150, LogPath, no_check, Handle).

%% With backgroundload.
meas_mem_150_hour_saf_ntf_alarm_backgroundload(Config) ->
    Handle = proplists:get_value(handle, Config),
    generate_cpu_load("50"),
    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath), % else permission denied.
    do_meas_mem_saf_ntf_alarm(150, LogPath, no_check, Handle).

%%--------------------------------------------------------------------
%% @doc
%% Check memory size after several generate / cease alarms. <br/>
%% This TC log measured memory size to file.<br/>
%% - reboot node. <br/>
%% %% - Open cli connections and config snmp. <br/>
%% - Check used memory for tot_bc, com, beam, snmpd. This will be the starting point!<br/>
%%   ps -eo fname,vsz,rss | egrep '(beam)|(com)' <br/>
%% - Do Generate  and cease alarms. <br/>
%% - Check used memory for tot_bc, com, beam, snmpd. This will be the end point!<br/>
%%   ps -eo fname,vsz,rss | egrep '(beam)|(com)' <br/>
%%   ps -eo fname,rss | egrep '(snmpd)' <br/>
%% %% - Close cli connections and delete config snmp. <br/>
%% @spec do_meas_mem_saf_ntf_alarm(Hours, LogPath, CheckFlag, Handle) -> ok
%% @end
%%--------------------------------------------------------------------
do_meas_mem_saf_ntf_alarm(Hours, LogPath, CheckFlag, Handle) ->
    ct:pal("### Check memory, after generate / cease Alarm via NTF. ~p hours!", [Hours]),

    [{_, NodeName}] = ct:get_config(test_nodes),
    BoardType =
	proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:pal("BoardType: ~p .",[BoardType]),

    %%%%
    %% Get sw version, using cli
    %%%%
    [H|_] = ?CLI_SessionNameList,
    CXS_label = rct_tlib:get_sw_version(H),
    ct:pal("~p", [CXS_label]),
    Seconds = Hours*60*60,

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
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 5000),
    ct:pal("TestNodeBeamPid: ~p .",[TestNodeBeamPid]),

    SnmpdPidA = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 
					   10000, noprint), "\n "),
    SnmpdPidB = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 
					   10000, noprint), "\n "),
    case SnmpdPidA of
    	[] ->
    	    SnmpdPid = SnmpdPidB;
	SnmpdPidB ->
	    SnmpdPid = SnmpdPidB; %% Same Pid, no change
	_ ->
    	    SnmpdPid = lists:delete(lists:flatten(SnmpdPidA), SnmpdPidB) %% Two pids exist! Use latest..
    end,
    ct:log("A: ~p",[SnmpdPidA]),
    ct:log("B: ~p",[SnmpdPidB]),
    ct:log("C: ~p",[SnmpdPid]),
    Branch = rct_tlib:get_branch(),
    case Branch of
	"R2A" ->
	    ct:pal("#LogPath_1# ~p", [LogPath++?ALL_ALARM_NTF_MEASURED_FILE_1]),
	    ct:pal("#LogPath_2# ~p", [LogPath++?ALL_ALARM_NTF_MEASURED_FILE_2]);
	_ ->
	    ct:pal("#~s_LogPath_1# ~p", [Branch, LogPath++?ALL_ALARM_NTF_MEASURED_FILE_1]),
	    ct:pal("#~s_LogPath_2# ~p", [Branch, LogPath++?ALL_ALARM_NTF_MEASURED_FILE_2])
    end,

    %%%%
    %%get start memory size
    %%%%
    [Start_Tot_bc,
     Start_Com_rss,
     Start_Beam_rss,
     Start_Snmpd_rss,
     Start_TestBeam_rss] = get_used_mem_size([tot_bc,
					      com,
					      beam,
					      snmpd,
					      test_beam],
					     BeamPid,
					     TestNodeBeamPid,
					     SnmpdPid),

    %%%%
    %% Generate / Cease Alarm.
    %%%%
    StartTime = os:timestamp(),

    {End_Tot_bc,
     End_Com_rss,
     End_Beam_rss,
     End_Snmpd_rss,
     End_TestBeam_rss,
     Nr} = loop_gen_cease_alarm_check(StartTime,
				      Handle,
				      Seconds,
				      %% 120,
				      Start_Tot_bc,
				      Start_Com_rss,
				      Start_Beam_rss,
				      Start_Snmpd_rss,
				      Start_TestBeam_rss,
				      LogPath,
				      BeamPid,
				      TestNodeBeamPid,
				      SnmpdPid,
				      BoardType,
				      CheckFlag),

    FileNameStr = "meas_check_memory_alarm_generate_cease_"++
	integer_to_list(Hours) ++ "_hour",
    FileName = rct_tlib:get_filename(FileNameStr),
    rct_tlib:writeDataToFile(?LOG_DIR, FileName,
			     "~p;~w;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~s;~s;~p;~w ~n",
			     [httpd_util:rfc1123_date(),
			      CXS_label,
			      Nr,
			      Start_Tot_bc,
			      End_Tot_bc,
			      list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
			      Start_Com_rss,
			      End_Com_rss,
			      list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
			      Start_Beam_rss,
			      End_Beam_rss,
			      list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss),
			      Start_Snmpd_rss,
			      End_Snmpd_rss,
			      list_to_integer(End_Snmpd_rss) - list_to_integer(Start_Snmpd_rss),
			      NodeName
			     ]),
    %%%%
    %% Print top
    %%%%
    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
    TOP = string:tokens(Top, "\n"),
    ct:pal("Nr: ~p, ~p",[Nr, TOP]),

    ct:pal("##LogDir1: ~p ",[?LOG_DIR++FileName]),

    ct:pal(" Start : Tot_bc: ~s,  com: ~s,  beam: ~s,  snmpd: ~s,  test_beam: ~s ~n", [Start_Tot_bc,
										       Start_Com_rss,
										       Start_Beam_rss,
										       Start_Snmpd_rss,
										       Start_TestBeam_rss
										      ]),

    ct:pal("End  : Tot_bc: ~s,  com: ~s,  beam: ~s,  snmpd: ~s,  test_beam: ~s ~n", [End_Tot_bc,
										     End_Com_rss,
										     End_Beam_rss,
										     End_Snmpd_rss,
										     End_TestBeam_rss
										    ]),

    %%%%
    %% check that snmpd has not restarted.
    %%%%
    SnmpdPid2 = string:tokens(rct_rpc:call(rpc, os, cmd, ["pgrep snmpd"], 10000, noprint), "\n "),
    SnmpdPidB=SnmpdPid2,

    ct:pal("All measured memory is loged at:",[]),
    ct:pal("LogPath: ~p, ~n"
	   "filename 1:  ~p ~n"
	   "filename 2:  ~p ~n",[LogPath,
				 ?ALL_ALARM_NTF_MEASURED_FILE_1,
				 ?ALL_ALARM_NTF_MEASURED_FILE_2]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Loop alarm operations during a time intervall. <br/>
%% There is a check of expecting active alarm exist and memsize. <br/>
%% Chek memory size. <br/>
%% @end
%%--------------------------------------------------------------------
loop_gen_cease_alarm_check(StartTime, Handle, Duration, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss, Start_TestBeam_rss, LogPath, BeamPid, TestBeamPid, SnmpdPid, BoardType, CheckFlag) ->
    loop_gen_cease_alarm_check(StartTime, Handle, Duration, 0, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss, Start_TestBeam_rss, LogPath, BeamPid, TestBeamPid, SnmpdPid, BoardType, CheckFlag).

loop_gen_cease_alarm_check(StartTime, Handle, Duration, Nr, Start_Tot_bc, Start_Com_rss, Start_Beam_rss, Start_Snmpd_rss, Start_TestBeam_rss, LogPath, BeamPid, TestBeamPid, SnmpdPid, BoardType, CheckFlag) ->
    ct:pal("¤¤¤ generate / cease alarm, nr: ~p", [Nr]),
    %% test_server:break("break"),

    case CheckFlag of
	check ->
            %%%%
	    %% Set expextected recieved traps when generate alarms
            %%%%
	    rct_snmpmgr:wait_for_traps(?GenTrapList,
				       ?AllowedTraps,
				       300);
	no_check ->
	    ok
    end,

    %%%%
    %% Generate alarm.
    %%%%
    ct:pal("Generate alarms.",[]),
    HeaderList = generate_alarms(Handle, ?ClassNrList),
    %% timer:sleep(?TIME),

    case CheckFlag of
	check ->
            %%%%
	    %% Check alarm traps is rcvd.
            %%%%
	    ok = rct_snmpmgr:check_traps(),

	    %%%%
	    %% Set expextected recieved traps when cease alarms
            %%%%
	    rct_snmpmgr:wait_for_traps(?ClearTrapList,
				       ?AllowedTraps,
				       300);
	no_check ->
	    ok
    end,

    %%%%
    %% Cease alarm.
    %%%%
    ct:pal("Cease alarms.",[]),
    cease_alarms(Handle, HeaderList),
    %% timer:sleep(?TIME),

    case CheckFlag of
	check ->
            %%%%
	    %% Check clear traps is rcvd.
            %%%%
	    ok = rct_snmpmgr:check_traps();
	no_check ->
	    ok
    end,

    %% case Nr rem 10 of
    case Nr rem 1000 of
	0 ->
	    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),
	    TOP = string:tokens(Top, "\n"),
	    ct:log("Nr: ~p, ~p",[Nr, TOP]);
	_ ->
	    ok
    end,

    %% case Nr rem 10 of
    case Nr rem 500 of
	0 ->
	    [End_Tot_bc,
	     End_Com_rss,
	     End_Beam_rss,
	     End_Snmpd_rss,
	     End_TestBeam_rss] = get_used_mem_size([tot_bc,
						    com,
						    beam,
						    snmpd,
						    test_beam],
						   BeamPid,
						   TestBeamPid,
						   SnmpdPid),

	    TimeStamp = os:timestamp(),
	    PassedTime =
		trunc(timer:now_diff(TimeStamp, StartTime) / 1000 / 1000),
	    ct:log("~p; Nr: ~p; Time: ~p; "
		   "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; "
		   "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
		   "Start_beam: ~s; End_beam: ~s; diff_beam: ~p; "
		   "Start_snmpd: ~s; End_snmpd: ~s; diff_snmpd: ~p; "
		   "Start_test_beam: ~s; End_test_beam: ~s; "
		   "diff_test_beam: ~p ~n",
		   [httpd_util:rfc1123_date(),
		    Nr,
		    PassedTime,
		    Start_Tot_bc,
		    End_Tot_bc,
		    list_to_integer(End_Tot_bc) -
			list_to_integer(Start_Tot_bc),
		    Start_Com_rss,
		    End_Com_rss,
		    list_to_integer(End_Com_rss) -
			list_to_integer(Start_Com_rss),
		    Start_Beam_rss,
		    End_Beam_rss,
		    list_to_integer(End_Beam_rss) -
			list_to_integer(Start_Beam_rss),
		    Start_Snmpd_rss,
		    End_Snmpd_rss,
		    list_to_integer(End_Snmpd_rss) -
			list_to_integer(Start_Snmpd_rss),
		    Start_TestBeam_rss,
		    End_TestBeam_rss,
		    list_to_integer(End_TestBeam_rss) -
			list_to_integer(Start_TestBeam_rss)
		   ]),

	    %% Logs will be stored in test log path.
	    rct_tlib:writeDataToFile(
	      LogPath, ?ALL_ALARM_NTF_MEASURED_FILE_1, "~p; Nr: ~p; "
	      "Start_tot_bc: ~s; End_tot_bc: ~s; diff_tot_bc: ~p ; "
	      "Start_com: ~s; End_com: ~s; diff_com: ~p ; "
	      "Start_beam: ~s; End_beam: ~s; diff_beam: ~p ; "
	      "Start_snmpd: ~s; End_snmpd: ~s; diff_snmpd: ~p ~n",
	      [httpd_util:rfc1123_date(),
	       Nr,
	       Start_Tot_bc,
	       End_Tot_bc,
	       list_to_integer(End_Tot_bc) - list_to_integer(Start_Tot_bc),
	       Start_Com_rss,
	       End_Com_rss,
	       list_to_integer(End_Com_rss) - list_to_integer(Start_Com_rss),
	       Start_Beam_rss,
	       End_Beam_rss,
	       list_to_integer(End_Beam_rss) - list_to_integer(Start_Beam_rss),
	       Start_Snmpd_rss,
	       End_Snmpd_rss,
	       list_to_integer(End_Snmpd_rss) - list_to_integer(Start_Snmpd_rss)
	      ]),

	    %% UsedDisk = get_used_disc(),
	    UsedDisk = get_use_disc_procentage(BoardType),

	    rct_tlib:writeDataToFile(
	      LogPath, ?ALL_ALARM_NTF_MEASURED_FILE_2, "~p; Nr: ~p; "
	      "Start_TestBeam: ~s; End_TestBeam: ~s; diff_TestBeam: ~p; "
	      "UsedDisc: ~p ~n",
	      [httpd_util:rfc1123_date(),
	       Nr,
	       Start_TestBeam_rss,
	       End_TestBeam_rss,
	       list_to_integer(End_TestBeam_rss) -
		   list_to_integer(Start_TestBeam_rss),
	       UsedDisk]),
	    %% [Start_snmpd_rss, End_snmpd_rss]),
	    evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss),
	    evaluate_snmpd_mem_size(Start_Snmpd_rss, End_Snmpd_rss);

	_ ->
	    ok
    end,

    %% Wait to avoid toggling alarm
    timer:sleep(30000),

    %% ct:pal("### Start_snmpd_rss: ~p , End_snmpd_rss: ~p ",
    %% %% [Start_snmpd_rss, End_snmpd_rss]),
    %% evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss),
    %% evaluate_snmpd_mem_size(Start_Snmpd_rss, End_Snmpd_rss),

    CheckTime = os:timestamp(),
    TimeDiff = trunc(timer:now_diff(CheckTime, StartTime) / 1000 / 1000),
    %% ct:pal("TimeDiff: ~p", [TimeDiff]),
    case TimeDiff > Duration of
	true ->
	    [End_Tot_bc1,
	     End_Com_rss1,
	     End_Beam_rss1,
	     End_Snmpd_rss1,
	     End_TestBeam_rss1] = get_used_mem_size([tot_bc,
						     com,
						     beam,
						     snmpd,
						     test_beam],
						    BeamPid,
						    TestBeamPid,
						    SnmpdPid),
	    {End_Tot_bc1, End_Com_rss1, End_Beam_rss1, End_Snmpd_rss1,
	     End_TestBeam_rss1, Nr};
	    %% {End_Tot_bc, End_Com_rss, End_Beam_rss, End_Snmpd_rss,
	    %%  End_TestBeam_rss, Nr};
	false ->
	   loop_gen_cease_alarm_check(StartTime, Handle, Duration, Nr+1,
				      Start_Tot_bc, Start_Com_rss,
				      Start_Beam_rss, Start_Snmpd_rss,
				      Start_TestBeam_rss, LogPath, BeamPid,
				      TestBeamPid, SnmpdPid, BoardType, CheckFlag)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Evaluate memory size from start to end.<br/>
%% @end
%%--------------------------------------------------------------------
evaluate_mem_size(Start_Com_rss, End_Com_rss, Start_Beam_rss, End_Beam_rss) ->

    Com_max_accepted_limit = list_to_integer(Start_Com_rss) + 8000,
    %% ct:pal("Com_max_accepted_limit: ~p", [Com_max_accepted_limit]),

    Beam_max_accepted_limit = list_to_integer(Start_Beam_rss) + 15000,
    %% ct:pal("Beam_max_accepted_limit: ~p", [Beam_max_accepted_limit]),

    case list_to_integer(End_Com_rss) of
    	Val when Val > Com_max_accepted_limit ->
	    ct:pal("Start_Com_rss: ~p", [Start_Com_rss]),
	    ct:pal("End_Com_rss: ~p", [End_Com_rss]),
	    ct:pal("End_Com_Size: ~s > ~p, Com_max_accepted_limit", [End_Com_rss, Com_max_accepted_limit]),
    	    ct:fail("Com memory usage is larger than expected!");
    	_Val ->
	    %% ct:pal("End com size: ~p", [Val]),
	    ok
    end,

    case list_to_integer(End_Beam_rss) of
    	Value when Value > Beam_max_accepted_limit ->
	    ct:pal("Start_Beam_rss: ~p", [Start_Beam_rss]),
	    ct:pal("End_Beam_rss: ~p", [End_Beam_rss]),
	    ct:pal("End beam size: ~s > ~p, Beam_max_accepted_limit", [End_Beam_rss, Beam_max_accepted_limit]),
    	    ct:fail("Beam memory usage is larger than expected!");
    	_Value ->
	    %% ct:pal("End beam size: ~p", [Value]),
    	    ok
    end,

    ok.

evaluate_snmpd_mem_size(Start_snmpd_rss, End_snmpd_rss) ->
    %% ct:pal("Start_snmpd_rss: ~p", [Start_snmpd_rss]),
    %% ct:pal("End_snmpd_rss: ~p", [End_snmpd_rss]),

    Snmpd_max_accepted_limit = list_to_integer(Start_snmpd_rss) + 55000,
    %% ct:pal("Snmpd_max_accepted_limit: ~p", [Snmpd_max_accepted_limit]),

    case list_to_integer(End_snmpd_rss) of
    	Value2 when Value2 > Snmpd_max_accepted_limit ->
	    ct:pal("Start_snmpd_rss: ~p", [Start_snmpd_rss]),
	    ct:pal("End_snmpd_rss: ~p", [End_snmpd_rss]),
	    ct:pal("End snmpd size: ~s > ~p, Snmpd_max_accepted_limit", [End_snmpd_rss, Snmpd_max_accepted_limit]),
    	    ct:fail("snmpd memory usage is larger than expected!");
    	_Value2 ->
	    %% ct:pal("End beam size: ~p", [Value]),
    	    ok
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% get_used_mem_size. <br/>
%% @end
%%--------------------------------------------------------------------
%% get_used_mem_size(WantedSizes, BeamPid) ->
%%     get_used_mem_size(WantedSizes, BeamPid, "0").

get_used_mem_size(WantedSizes, BeamPid, TestNodeBeamPid, SnmpdPid) ->
    MemSizes = lists:map(fun(X) ->
				 case X of
				     tot_bc ->
					 Free = rct_rpc:call(rpc, os, cmd, ["free"], 10000, noprint),
					 {match,[Tot_bc]} = re:run(Free,"buffers/cache: *([0-9]+).*",[{capture,[1],list}]),
					 Tot_bc;
				     com ->
					 Com_Rss = rct_rpc:call(rpc, os, cmd, ["ps -eo fname,rss | egrep '(com)'"], 10000, noprint),
					 ["com", Com_rss] = string:tokens(Com_Rss," \n"),
					 Com_rss;
				     beam ->
					 Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++BeamPid], 10000, noprint),
					 ["COMMAND","RSS","beam.smp", Beam_rss] = string:tokens(Beam_Rss_Data," \n"),
					 Beam_rss;
				     test_beam ->
					 Test_Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++TestNodeBeamPid], 10000, noprint),
					 ["COMMAND","RSS","beam.smp", Test_Beam_rss] = string:tokens(Test_Beam_Rss_Data," \n"),
					 Test_Beam_rss;
				     snmpd ->
					 Snmpd_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p"++SnmpdPid], 10000, noprint),
					 ["COMMAND","RSS","snmpd", Snmpd_rss] = string:tokens(Snmpd_Rss_Data," \n"),
					 Snmpd_rss
				 end
			 end, WantedSizes),
    MemSizes.


%% ===========================================================================
%% @doc
%% Generate cpu load on all 4 cpus<br/>
%% - 100% CPU load. <br/>
%% - Use cpulimit to controll wanted load on CPUs. <br/>
%% @spec generate_cpu_load(LoadProcent) -> ok
%% @end
%% ===========================================================================
generate_cpu_load(LoadProcent) ->
    ct:pal("Generate cpu load",[]),

    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),
    case BoardType of
	BoardType when BoardType == "dus4101";
		       BoardType == "duw4101" ->
	    CPULIMIT_FROM_PATH = ?CPULIMIT_FROM_PATH,
	    NrOfCores = 4;
	"tcu03" ->
	    CPULIMIT_FROM_PATH = ?CPULIMIT_FROM_PATH_ARM,
	    NrOfCores = 8;
	_ -> % dus5201, dus3201
	    CPULIMIT_FROM_PATH = ?CPULIMIT_FROM_PATH_ARM,
	    NrOfCores = 12
    end,
    {ok,_} =  rct_scp:to_target(node1, CPULIMIT_FROM_PATH, ?CPULIMIT_TO_PATH, 10),

    %%%%
    %% cpulimit program will controll wanted load on CPUs.
    %%%%
    lists:foreach(fun(_C)->
    			  [] = rct_rpc:call(rpc, os, cmd, [?CPULIMIT_TO_PATH++"cpulimit -z -i -l "++LoadProcent++" cat /dev/zero > /dev/null &"],
					    10000, noprint)
    		  end, lists:seq(1,NrOfCores)),

    CatCmdsPids = rct_rpc:call(rpc, os, cmd, ["pgrep cat"], 10000, noprint),
    %% Build a List with Pids, remove \n from the received list.
    CatPidList = string:tokens(CatCmdsPids, "\n"),
    ct:pal("CatPidList:~p", [CatPidList]),
    case BoardType of
	BoardType when BoardType == "dus4101";
		       BoardType == "duw4101" ->
	    4 = length(CatPidList);
	"tcu03" ->
	    8 = length(CatPidList);
	_ -> % dus5201, dus3201
	    12 = length(CatPidList)
    end,

    %% CpuLimitPids = rct_rpc:call(rpc, os, cmd, ["pgrep cpulimit"], 10000, noprint),
    %% %% Build a List with Pids, remove \n from the received list.
    %% CpuLimitPidList = string:tokens(CpuLimitPids, "\n"),
    %% ct:pal("CpuLimitPidList:~p", [CpuLimitPidList]),
    %% %% Check nr of expecting pids is started.
    %% 4 = length(CpuLimitPidList),

    %% Check CPU load.
    {ok, CpuLoad} = rct_tlib:cpuload(identifier,100),
    lists:foreach(fun({_, Load})->
    			  case Load > 30 of
    			      true ->
    				  ok;
    			      false ->
    				  case check_nr_of_cpu(CpuLoad) of
				      ok ->
					  ok;
				      nok ->
					  ct:pal("Fail:~p", [CpuLoad]),
					  ct:fail(" Background load is lower "
						  "than expected! ")
				  end
    			  end
    		  end, CpuLoad),
    ok.


%% ===========================================================================
%% get_used_disc() ->
%%     DF = rct_rpc:call(rpc, os, cmd, ["df"], 10000, noprint),
%%     Df = string:tokens(DF, " \n"),
%%     ct:log("### Df: ~p", [Df]),
%%     DiskNr = string:str(Df, ["/disk"]),
%%     UsedDisk = lists:nth(DiskNr-1, Df),
%%     UsedDisk.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
get_use_disc_procentage(BoardType) ->
    DF =  rct_rpc:call(rpc, os, cmd, ["df"], 10000,
		       noprint),
    Df = string:tokens(DF, " \n"),
    ct:log("### Df: ~p", [Df]),
    DiskNr = case BoardType of  %% Get element nr in list.
		 BoardType when BoardType == "dus4101";
				%% BoardType == "dus5201"; %% Remove this when RCSEE is used
				BoardType == "duw4101" ->
		     string:str(Df, ["/disk"]);
		 _ ->
		     string:str(Df, ["/home/sirpa"])  % ARM, mnesia i written here.
	     end,
    %% ct:log("### DiskNr: ~p", [DiskNr]),
    UsedDisk = lists:nth(DiskNr-1, Df),
    %% ct:log("### UsedDisk: ~p", [UsedDisk]),
    UsedDisk.



%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
generate_alarms(Handle, ClassNrList) ->
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

		  timer:sleep(?TIME),
		  {ok, _Id } =
		      rct_safe_ntf_rpc:notification_send(Handle,
							 Notification),
		      Header
	  end, ClassNrList),
    HeaderList.



%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
cease_alarms(Handle, HeaderList) ->
    lists:foreach(
      fun(Header) ->
	      NotificationClear =
		  #safe_ntf_alarm_notification
		  {notification_header = Header,
		   probable_cause = ?SAFE_NTF_UNSPECIFIED_REASON,
		   perceived_severity = ?SAFE_NTF_SEVERITY_CLEARED},

	      timer:sleep(?TIME),
	      {ok, _Id2 } =
		  rct_safe_ntf_rpc:notification_send(Handle,
						     NotificationClear)
      end, HeaderList),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
wait_for_testnode_up() ->
    wait_for_testnode_up(120000).
wait_for_testnode_up(Timeout) when Timeout < 0 ->
    ct:fail("TC will fail due to Testnode is not up within 2 min.");
wait_for_testnode_up(Timeout) ->
    case rct_safe_ntf_rpc:get_testnode(rct_safe_ntf_rpc) of
	{ok, _TestNode} ->
	    ct:pal("Testnode is up");
	 _Other ->
	    ct:pal("Testnode is not up, sleep and try again."),
	    timer:sleep(10000),
	    wait_for_testnode_up(Timeout-10000)
    end.

%%-------------------------------------------------------------------- 
%% @hidden
%%-------------------------------------------------------------------- 
check_nr_of_cpu(CpuLoadList)->

    NrOfCpu  = length(
		 lists:filter(fun({_, Load})->
				      case Load =< 20 of   
					  true -> 
					      true;  
					  false -> 
					      false
				      end 
			      end, CpuLoadList)
		),

    case NrOfCpu =< 2 of
	true ->
	    ct:log("~p CPU have background load lower than expected!", [NrOfCpu]),
	    ok;
	false ->
	    nok
    end.
